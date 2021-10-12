import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_tables
import mm_lib
import mm_diags
import mm_genpcl

import* mm_pcl

const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

const dodotchains=1

const maxnestedloops	= 50

const maxparams=100

const maxswitchrange=500
const maxcases=maxswitchrange

const maxcasedepth=20
[maxcasedepth]unit casestmt
[maxcasedepth]int caseelse
int casedepth

ref[]int sw_labeltable			!set from do-switch
ref[]int sw_valuetable
int sw_lower
int sw_ncases					!1..n for serial switch; 0 for simple
byte sw_defaultseen				!starts at 0, set to 1 when default: seen
int sw_defaultlabel
int sw_breaklabel

int maxreg=0

global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)

global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a,b
	symbol d
	ref[]int32 pmult

	if p=nil then return fi
	mlineno:=p.pos

	a:=p.a
	b:=p.b

	switch p.tag
	when j_const         then do_const(p)
	when j_null          then
	when j_name          then do_name(p)
	when j_block,j_stmtblock then
				         do_block(p)
	when j_callproc      then do_callproc(p,a,b,0)
	when j_return        then do_return(p,a)
	when j_returnmult    then do_returnmult(p,a)
	when j_assign        then do_assign(p,a,b)
	when j_to            then do_to(p,a,b)
	when j_if            then do_if(p,a,b,p.c,0)
	when j_longif        then do_longif(p,a,b,0)
	when j_forup         then do_for(p,a,b,p.c,0)
	when j_fordown       then do_for(p,a,b,p.c,1)
	when j_forall        then do_forall(p,a,b,p.c,0)
	when j_forallrev     then do_forall(p,a,b,p.c,1)
	when j_while         then do_while(p,a,b,p.c)
	when j_repeat        then do_repeat(p,a,b)
	when j_goto          then do_goto(a)
	when j_labeldef      then do_labeldef(p)
	when j_restart       then do_exit(p,1)
	when j_redo          then do_exit(p,2)
	when j_next          then do_exit(p,3)
	when j_exit          then do_exit(p,4)
	when j_do            then do_do(p,a,b)
	when j_case          then do_case(p,a,b,p.c,0,0)
	when j_docase        then do_case(p,a,b,p.c,1,0)
	when j_switch        then do_switch(p,a,b,p.c,0,0)
	when j_doswitch      then do_switch(p,a,b,p.c,1,0)
	when j_recase        then do_recase(p,a)
	when j_swap          then do_swap(p,a,b)
	when j_select        then do_select(p,a,b,p.c,0)
	when j_print,j_println then
		do_print(p,a,b)
	when j_fprint,j_fprintln, j_cprint, j_cprintln then
		do_print(p,a,b)
	when j_read	        then do_read(p,a)
	when j_readln        then do_readln(a)
	when j_stop          then do_stop(p,a)
	when j_eval          then
		evalunit(a)
		pcl_gen(keval)
	when j_andl          then do_andl(p,a,b)
	when j_orl           then do_orl(p,a,b)

	when j_makerange     then PCL_GENCOMMENT("MAKERANGE")
	when j_callfn        then do_callproc(p,a,b,1)

	when j_cmp           then do_setcc(p,a,b)
	when j_cmpchain      then do_setccchain(p,a)

	when j_bin           then do_bin(p,a,b)
	when j_index         then do_index(p,a,b)
	when j_slice         then do_slice(a,b)
	when j_makeslice     then
		evalunit(b)
		evalunit(a)
		pcl_gen(kmakeslice)
		setmode(tu128)

	when j_dotindex      then do_dotindex(p,a,b)
	when j_dotslice      then do_dotslice(p,a,b)
	when j_dot           then do_dot(p)
	when j_ptr           then do_ptr(p,a)
	when j_addrof        then evalref(a,b)
	when j_addroffirst   then evalref(a)
	when j_convert       then do_convert(p,a)
	when j_typepun       then do_typepun(p,a)
	when j_shorten       then do_shorten(p,a)
	when j_typeconst     then do_typeconst(p)

	when j_unary         then do_unary(p,a)

	when j_notl          then do_notl(p,a)
	when j_istruel       then do_istruel(p,a)

	when j_incr          then
		if p.pclop in [kincr, kdecr] then
			do_incr(p,a)
		else
			do_incrload(p,a)
		fi
!
	when j_binto         then do_binto(p,a,b)
!
	when j_unaryto       then do_unaryto(p,a)
!
	when j_syscall then
		do_syscall(p,a)

	when j_assem         then
		pcl_gen(kassem,pcl_genassem(p))
		setmode_u(p)

	when j_cvlineno      then
		pcl_gen(kpush,pcl_genint(p.lineno iand 16777215))

	when j_empty         then do_empty(p,a)

	else
		PRINTLN "UNSUPPORTED TAG: ",JTAGNAMES[P.TAG],
				MLINENO IAND 16777215, SOURCEFILENAMES[MLINENO>>24]
		pcl_gencomment("Unimplemented:")
		pcl_gencomment(jtagnames[p.tag])
		return
	endswitch

	if p.mode<>tvoid and not p.resultflag then
		case p.tag
		when j_assign, j_callproc, j_syscall then

		else
			pcl_gen(kpopstack)
			setmode_u(p)
		esac
	fi
end

proc evalref(unit p, q=nil)=
	unit a,b,c
	a:=p.a
	b:=p.b
	c:=p.c
	mlineno:=p.pos

	switch p.tag
	when j_name then
		genpushmemaddr_d(p.def)
		if q then					!addrof may have optional byte offset
			genpushint(q.value)
			pcl_gen(kaddrefoff)
			pcl_setscale(1)
!			pccurrxx.scale:=1
			setmode(tu8)
		fi
	when j_index then
		do_indexref(a,b)

	when j_dot then
		do_dotref(p)

	when j_ptr then
		evalunit(p.a)

	else
		case p.tag
		when j_if then
			do_if(p,a,b,c,1)
		when j_longif then
			do_longif(p,a,b,1)
!		when j_select then
!			do_select(p,a,b,c,1)
!		when j_switch then
!			do_switch(p,a,b,c,0,1)
!		when j_case then
!			do_case(p,a,b,c,0,1)
		else
			PRINTUNIT(P)
			gerror("evalref")
		esac
	end switch
end

proc evallv(unit p)=
	evalref(p)
end

global proc evalunitx(unit p, int isref) =
!call either evalunit (isref=0) or evalref(isref=1)
	if isref then
		evalref(p)
	else
		evalunit(p)
	fi
end

global proc evalblock(unit p) =
	evalunit(p)
end

proc evalarray(unit p)=
	if ttbasetype[p.mode]=tslice then
		evalunit(p)
		pcl_gen(ksliceptr)
		setmode(tu64)
	elsif p.mode=trefchar then
		evalunit(p)
	else
		evalref(p)
	fi

end

proc do_block(unit p)=
	unit a:=p.a

	while a do
		evalunit(a)
		a:=a.nextunit
	od
end

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q,r,s
	int lab2,i

	q:=p.a
	r:=p.b

	switch p.tag
	when j_andl then
		case opc
		when kjumpf then
			genjumpcond(kjumpf,q,lab)
			genjumpcond(kjumpf,r,lab)
		when kjumpt then
			lab2:=createfwdlabel()
			genjumpcond(kjumpf,q,lab2)
			genjumpcond(kjumpt,r,lab)
			definefwdlabel(lab2)
		esac

	when j_orl then
		case opc
		when kjumpf then
			lab2:=createfwdlabel()
			genjumpcond(kjumpt,q,lab2)
			genjumpcond(kjumpf,r,lab)
			definefwdlabel(lab2)
		when kjumpt then
			genjumpcond(kjumpt,q,lab)
			genjumpcond(kjumpt,r,lab)
		esac

	when j_notl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt,q,lab)
		when kjumpt then
			genjumpcond(kjumpf,q,lab)
		esac

	when j_istruel then
		evalunit(q)

		pcl_gen((opc=kjumpt|kjumptrue|kjumpfalse),pcl_genlabel(lab))
		setmode_u(q)

	when j_block then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc,q,lab)

	when j_cmp then

		gcomparejump(opc,p.pclop,q,r,lab)

	when j_inrange then
		evalunit(q)
		evalunit(r.a)
		evalunit(r.b)
		pcl_gen((opc=kjumpf|kjumpnotinrange|kjumpinrange),pcl_genlabel(lab))
		setmode_u(q)

	when j_inset then
		s:=r.a
		if s=nil then
			gerror("empty set")
		fi

		if opc=kjumpf then
			lab2:=createfwdlabel()
			evalunit(q)

			while s do
				evalunit(s)
				s:=s.nextunit
				if s then
					pcl_gen(ksetjumpeq,pcl_genlabel(lab2))
				else
					pcl_gen(ksetjumpne,pcl_genlabel(lab))
				fi
				setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s do
				evalunit(s)
				s:=s.nextunit
				pcl_gen((s|ksetjumpeq|ksetjumpeqx),pcl_genlabel(lab))
				setmode_u(q)
			od
		fi

	when j_cmpchain then
		r:=q.nextunit
		i:=1
		if opc=kjumpf then
			while r do
				evalunit(q)
				evalunit(r)
				pcl_gen(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq),pcl_genlabel(lab))
				setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(q)
				evalunit(r)
				if r.nextunit then
					pcl_gen(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq),pcl_genlabel(lab2))
				else
					pcl_gen(condtopclop(p.cmpgenop[i],kjumpeq),pcl_genlabel(lab))
				fi
				setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else			!other, single expression
		evalunit(p)
		if p.mode not in [ti64,tu64] then gerror("jumptrue/not i64") fi

		pcl_gen((opc=kjumpt|kjumptrue|kjumpfalse),pcl_genlabel(lab))
		setmode(ti64)
	endswitch
end

proc gcomparejump(int jumpopc,int cond,unit lhs,rhs,int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int opc

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	evalunit(lhs)
	evalunit(rhs)

	pcl_gen(condtopclop(cond,kjumpeq),pcl_genlabel(lab))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pcl_gen(kjump,pcl_genlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #",mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] then
		if ttsize[mode]<16 then
			genpushint(p.value,mode)
		else
			pcl_gen(kpush, pcl_genint128(p.value128,mode))
		fi
	elsif ttisreal[mode] then
		if ttsize[mode]=4 then
			genpushreal32(p.xvalue)
		else
			genpushreal(p.xvalue)
		fi

	elsif ttisref[mode] then
		if p.isastring then
			genpushstring(p.svalue)
		else
			genpushint(p.value)
		fi
	else
		gerror("do_const")
	fi
	setmode(mode)
end

!proc do_null(unit p,a,b) =
!	unimpl("do_null")
!end

proc do_name(unit p)=
	symbol d

	d:=p.def
	case d.nameid
	when procid,dllprocid then
		genpushmemaddr_d(d)
	when labelid then
		if d.index=0 then
!CPL "NAME",LABELNO
			d.index:=++labelno
		fi
		pcl_gen(kjump, pcl_genlabel(d.index))
		p.resultflag:=0
		p.mode:=tvoid
!
	when fieldid then
		genpushint(d.offset,ti64)


	else
		genpushmem_d(d)
!		setmode(getmemmode(p))
		pccurr.mode:=getpclmode(getmemmode_m(p))
		pccurr.size:=ttsize[p.mode]
	esac
end

proc do_stop(unit p,a) =
	if a then
		evalunit(a)
	else
		pcl_gen(kpush,pcl_genint(0))
	fi
	pcl_gen(kstop)
end

proc do_andl(unit p,a,b) =
	int labfalse, labend

	pcl_gen(kstartmult)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf,a,labfalse)
	genjumpcond(kjumpf,b,labfalse)

	genpushint(1)
	pcl_gen(kresetmult)

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pcl_gen(kendmult)

	definefwdlabel(labend)
end

proc do_orl(unit p,a,b) =
	int labtrue, labfalse, labend

	pcl_gen(kstartmult)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt,a,labtrue)
	genjumpcond(kjumpf,b,labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	pcl_gen(kresetmult)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pcl_gen(kendmult)

	definefwdlabel(labend)
end

proc do_notl(unit p,a) =
	evalunit(a)
	pcl_gen(p.pclop)
	setmode(ti64)
end

proc do_istruel(unit p,a) =
	evalunit(a)
	if islogical(a) then
		return
	fi
	pcl_gen(p.pclop)
	setmode(ti64)
end

proc do_typepun(unit p, a) =
	evalunit(a)
	setmode_u(a)
	if a.mode=p.mode then return fi
	pcl_gen(ktypepun)
	setmode(p.convmode)
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

global function islogical(unit p)int=			!ISLOGICAL
!return 1 if p is known to have a logical value
	case p.tag
	when j_istruel,j_notl,j_andl,j_orl,j_xorl then
		return 1
	esac
	return 0
end

proc do_assign(unit p,a,b) =
!fstore=1 when result is needed
	unit c
	symbol d
	int offset

!deal with list constructs on either side
!	if a.tag=j_makelist and b.tag=j_makelist then
!		do_multassign_lr(a,b)
!		return
!	elsif a.tag=j_makelist then
!		do_multassign_l(a,b)
!		return
!	elsif b.tag=j_makelist then
!		do_multassign_r(a,b)
!		return
!	fi

!Simple assignment, but look at block sizes
	if a.tag<>j_makelist and b.tag=j_makelist then
		if not p.resultflag then
			do_assignblock(p,a,b)		!(avoids pushing/popping block data)
			return
		fi
	fi

	if a.tag=j_makelist then
		if p.resultflag then gerror("multass/store") fi
		do_multassign(a,b)
		return
	elsif b.tag=j_callfn and ttbasetype[b.mode]=ttuple then
		do_multassign(a,b)
		return
	fi

	case a.tag
	when j_index then

		do_storeindex(p,a.a,a.b,b)
		return
	when j_slice then
GERROR("ASS/SLICE")

	when j_dot then
		do_storedot(a,a.b,b)
		return
	esac

	switch a.tag
	when j_name then
		evalunit(b)
		pcl_gen((p.resultflag|kstore|kpop), genmem_u(a))
	when j_ptr then
		evalunit(b)
		evalref(a)

		if pcl_getopcode()=kaddrefoff then 
			pcl_setopcode((p.resultflag|kstoreptroff|kpopptroff))
		else
			pcl_gen((p.resultflag|kstoreptr|kpopptr))
		fi
		setmode(getmemmode_m(a))

	when j_dotindex then
		evalunit(b)
		evalref(a.a)
		evalunit(a.b)
		pcl_gen((p.resultflag|kstoredotindex|kpopdotindex))
		setmode_u(a.a)
		return
	when j_dotslice then
		evalunit(b)
		evalref(a.a)
		evalunit(a.b.a)
		evalunit(a.b.b)
		pcl_gen((p.resultflag|kstoredotslice|kpopdotslice))
		setmode_u(a.a)
		return
	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	end switch

	setmode_u(a)

end

proc do_bin(unit p,a,b) =
	int offset

	evalunit(a)

	if pcl_getopcode()=kaddrefoff and
			p.pclop in [kaddrefoff, ksubrefoff] and
		ttisref[a.mode] and ttisinteger[b.mode] and b.tag=j_const then
		offset:=ttsize[tttarget[a.mode]]*b.value
		if p.pclop=kaddrefoff then
			pcl_addoffset(offset)
		else
			pcl_addoffset(-offset)
		fi
		return
	fi

	evalunit(b)

	pcl_gen(p.pclop)
	setmode_u(p)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi

	if p.pclop=ksubref and ttisref[a.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi
end

proc do_setcc(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	pcl_gen(condtopclop(p.pclop,kseteq))
	setmode_u(a)
end

proc do_setccchain(unit p,q) =
	int lab1,lab2,i
	unit r

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	i:=1

	pcl_gen(kstartmult)

	while r do
		evalunit(q)
		evalunit(r)
		pcl_gen(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq),pcl_genlabel(lab1))
		setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	od

	genpushint(1)
	pcl_gen(kresetmult)
	pcl_gen(kjump, pcl_genlabel(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	pcl_gen(kendmult)
	definefwdlabel(lab2)
end

proc do_binto(unit p,a,b)=
	evallv(a)
	evalunit(b)

	pcl_gen(p.pclop)
	setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi

end

proc do_unary(unit p,a) =
	evalunit(a)

	pcl_gen(p.pclop)
	setmode_u(p)
	if p.pclop=kupb and ttbasetype[a.mode]=tslice then
		pcl_setxy(ttlower[a.mode],0)
	fi

end

proc do_unaryto(unit p,a)=
	evallv(a)

	pcl_gen(p.pclop)
	setmode_u(a)
end

proc do_ptr(unit p,a)=

	evalunit(a)

!	if ttbasetype[p.mode] in [trecord,tarray] then
!		return
!	fi

	if pcl_getopcode()=kaddrefoff then 
		pcl_setopcode(kpushptroff)
	else
		pcl_gen(kpushptr)
	fi
	setmode(getmemmode_m(p))
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.index=0 then
		d.index:=++labelno
	fi
	print @&.str,d.name,,"::"
	pcl_gencomment(&.str)
	pcl_gen(klabel,pcl_genlabel(d.index))
end

proc do_goto(unit a)=
	symbol d

	case a.tag
	when j_name then
		d:=a.def
		if d.index=0 then
			d.index:=++labelno
		fi
		pcl_gen(kjump, pcl_genlabel(d.index))

	else
		gerror("goto ptr?")
	esac
end

proc do_do(unit p,a,b) =
	int lab_abc,lab_d

	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_abc, lab_d)

	evalblock(a)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_to(unit p,a,b) =
	unit cvar
	int lab_a,lab_b,lab_c,lab_d,count

	cvar:=p.c

	lab_a:=definelabel()
	a.mode:=ti64

	evalunit(a)
	pcl_gen(kpop,genmem_u(cvar))
	setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_a,lab_b,lab_c,lab_d)

!check for count being nonzero
	if a.tag<>j_const then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		pcl_gen(kjumple,pcl_genlabel(lab_d))
		setmode(ti64)

	else
		count:=a.value
		if count<=0 then
			genjumpl(lab_d)
		fi
	fi

	definefwdlabel(lab_b)
	evalblock(b)			!main body

	definefwdlabel(lab_c)

	pcl_gen(kto,pcl_genlabel(lab_b))
	pcl_gen(kopnd,genmem_u(cvar))

	definefwdlabel(lab_d)
	--loopindex
end

proc do_while(unit p,pcond,pbody,pincr) =
	int lab_b,lab_c,lab_d,lab_incr

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pincr then
		lab_incr:=createfwdlabel()
	else
		lab_incr:=lab_c
	fi

	stacklooplabels(lab_c, lab_b, lab_c, lab_d)

	genjumpl(lab_incr)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalblock(pbody)

	definefwdlabel(lab_c)

	if pincr then
		evalblock(pincr)
		definefwdlabel(lab_incr)
	fi

	docond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p,a,b) =
	int lab_ab, lab_c, lab_d

	lab_ab:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_ab, lab_ab, lab_c, lab_d)

	evalblock(a)

	definefwdlabel(lab_c)

	unless b.tag=j_const and b.value=0 then
		docond(kjumpf,b,lab_ab)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_exit(unit p,int k) =
	int n,index

	index:=p.index
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k,index)
	if n=0 then
		gerror("Bad exit/loop index",p)
	else
		genjumpl(n)
	fi
end

proc do_if(unit p,a,b,c, int isref) =
	int lab1,lab2,ismult

	ismult:=p.mode<>tvoid

	if ismult then pcl_gen(kstartmult) fi

	lab1:=createfwdlabel()

	docond(kjumpf,a,lab1)

	evalunitx(b,isref)
	if ismult then pcl_gen(kresetmult) fi

	if c then
		lab2:=createfwdlabel()			!label past else part
		genjumpl(lab2)
		definefwdlabel(lab1)
		evalunitx(c,isref)
		if ismult then pcl_gen(kendmult) fi
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_longif(unit p,a,b, int isref) =
	int labend,i,lab2,ismult
	unit pcond

	labend:=createfwdlabel()
	ismult:=p.mode<>tvoid

	pcond:=a
	i:=0
	if ismult then pcl_gen(kstartmult) fi

	while pcond do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf,pcond.a,lab2)

		evalunitx(pcond.b,isref)
		if ismult then pcl_gen(kresetmult) fi

		if pcond.nextunit or b then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
		pcond:=pcond.nextunit
	od

	if b then
		evalunitx(b,isref)
		if ismult then pcl_gen(kendmult) fi
	fi
	definefwdlabel(labend)
end

proc do_return(unit p,a) =
	if a then
		evalunit(a)

		if ttbasetype[p.mode] in [tslice, ti128, tu128] then
			pcl_genx(ksetretmult,2)

		else
			pcl_gen(ksetret)
		fi
		setmode_u(a)
	fi
	genjumpl(retindex)
end

proc do_returnmult(unit p,a) =
	[maxparams]unit params
	unit q
	int nparams

	q:=a
	nparams:=0
	while q do
		if nparams>=maxparams then gerror("Mult?") fi
		params[++nparams]:=q
		q:=q.nextunit
	od

	for i:=nparams downto 1 do
		evalunit(params[i])
	od

!need individual setret codes (not sure about the order)
	pcl_genx(ksetretmult, nparams)

	genjumpl(retindex)
	p.resultflag:=1
end

proc do_callproc(unit p,a,b,int isfn) =
	[maxparams]unit paramlist
	int nparams,nmult,ffi,isptr,nslots,nvariadics, blockret, nret
	symbol d
	symbol dtemp
	ref[]int32 pmult
	unit q

	isptr:=0
	case a.tag
	when j_name then
		d:=a.def

	when j_ptr then
		d:=ttnamedef[a.mode]
		isptr:=1
	else
		gerror("call/not ptr")
	esac

	nparams:=0
	nslots:=0
	nvariadics:=0
	blockret:=0
	ffi:=0

	if d.fflang in [clangff,windowsff] then
		ffi:=1
	fi

	q:=b
	while q do
		++nslots
		if nparams>=maxparams then gerror("maxparams") fi
		paramlist[++nparams]:=q
		if ffi and d.varparams and nparams>=d.varparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		fi
		if ttbasetype[q.mode] in [tslice, ti128, tu128] then
			++nslots
		fi
		q:=q.nextunit
	od

	pcl_gen(ksetargs)
	pcl_setnargs(nslots)
	pcl_setnvariadics(nvariadics)

	for i:=nparams downto 1 do
		evalunit(paramlist[i])
	od

	if not isptr then
		pcl_gen((isfn|kcallfn|kcallproc), genmemaddr_d(d))
	else
		evalunit(a.a)
		pcl_gen((isfn|kcallfnptr|kcallprocptr))
	fi
	if isfn then
		setmode(getmemmode_m(p))
	fi

	if d.nretvalues>1 then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			pcl_gent(ktype, getpclmode(pmult[i]))
		od
	fi
end

proc do_print(unit p,a,b) =
	unit q,r,fmt
	int m, fn, needprintend

	if a then
		needprintend:=1
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi
		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			pcl_gen_sysproc(sysfn_print_startfile,a)
		when tc8 then
			pcl_gen_sysproc(sysfn_print_startstr,a)
		when tref then
			pcl_gen_sysproc(sysfn_print_startptr,a)
		else
			gerror("@dev?")
		esac
	else
		needprintend:=1
		pcl_gen_sysproc(sysfn_print_startcon)
	fi

	q:=b

	case p.tag
	when j_fprint,j_fprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		fi
		pcl_gen_sysproc(sysfn_print_setfmt,q)
		q:=p.c
	esac

	while q do
		case q.tag
		when j_fmtitem then
			fmt:=q.b
			r:=q.a
			m:=r.mode
		when j_nogap then
			pcl_gen_sysproc(sysfn_print_nogap)
			q:=q.nextunit
			next
		when j_space then
			pcl_gen_sysproc(sysfn_print_space)
			q:=q.nextunit
			next
		else
			fmt:=nil
			r:=q
			m:=q.mode
		esac

		switch ttbasetype[m]
		when ti64 then
			fn:=sysfn_print_i64
			if not fmt then fn:=sysfn_print_i64_nf fi
		when tu64 then
			fn:=sysfn_print_u64
		when tr32 then
			fn:=sysfn_print_r32
		when tr64 then
			fn:=sysfn_print_r64
		when ti128 then
			fn:=sysfn_print_i128
		when tu128 then
			fn:=sysfn_print_u128
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sysfn_print_str
				if not fmt then fn:=sysfn_print_str_nf fi
			else
				fn:=sysfn_print_ptr
				if not fmt then fn:=sysfn_print_ptr_nf fi
			fi
		when tarray then
			GERROR("PRINTARRAY")
			q:=q.nextunit
		when trecord then
			GERROR("PRINTRECORD")
		when tslice then
			if tttarget[m]=tc8 then
				fn:=sysfn_print_strsl
			else
				gerror("PRINTSLICE")
			fi

		when tc64 then
			fn:=sysfn_print_c8

		else
			gerror_s("PRINT/T=#",strmode(m))
		end switch

		case fn
		when sysfn_print_i64_nf, sysfn_print_str_nf, sysfn_print_ptr_nf then
			pcl_gen_sysproc(fn, r)
		else
			pcl_gen_sysproc(fn, r, (fmt|fmt|pzero))
		esac

		q:=q.nextunit
	od

	case p.tag
	when j_println,j_fprintln then
		pcl_gen_sysproc(sysfn_print_newline)
	esac
	if needprintend then
		pcl_gen_sysproc(sysfn_print_end)
	fi
end

proc do_incr(unit p,a) =
	evallv(a)
	pcl_gen(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pcl_setincr(1)

	if ttisref[m] then
		pcl_setincr(ttsize[tttarget[m]])
	fi
end

proc do_incrload(unit p,a) =
	evallv(a)
	pcl_gen(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc do_for(unit p,pindex,pfrom, pbody, int down) =
	unit pto, pstep, pelse, px, plimit, ptoinit
	int lab_a,lab_b,lab_c,lab_d,lab_e
	int a,b,stepx

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit
	ptoinit:=pindex.nextunit

	if pto.tag=j_ptr then
		px:=pto.a
		symbol d
		if px.tag=j_name and (d:=px.def).nameid=paramid and
			 d.parammode=out_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	fi

	lab_a:=definelabel()
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_a, lab_b, lab_c, lab_d)

!now start generating code
	evalunit(pfrom)
	pcl_gen(kpop,genmem_u(pindex))
	setmode_u(pindex)

	if ptoinit then			!using temp for limit
		ptoinit.resultflag:=0
		evalunit(ptoinit)
	fi

	if pfrom.tag=j_const and pto.tag=j_const then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pcl_gen(kjump, pcl_genlabel(lab_e))
		fi
	else
		if pfrom.tag=j_const then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pcl_gen((down|kjumpgt|kjumplt),pcl_genlabel(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			pcl_gen((down|kjumplt|kjumpgt),pcl_genlabel(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
		if pstep.tag<>j_const then
			gerror("for/step non-const not ready")
		fi
		stepx:=pstep.value
		if stepx<=0 then
			gerror("Bad for-step")
		fi
		pcl_genx((down|kfordown|kforup),stepx, pcl_genlabel(lab_b))
		setmode_u(pindex)
	else
		pcl_genx((down|kfordown|kforup),1, pcl_genlabel(lab_b))
		setmode_u(pindex)
	fi

	pcl_gen(kopnd, genmem_u(pindex))
	if pto.tag=j_const then
		pcl_gen(kopnd, pcl_genint(pto.value))
	else
		pcl_gen(kopnd, genmem_u(pto))
	fi

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p,pindex,plist, pbody, int down) =
	unit plocal, pfrom, pto, pelse, px, plimit, passign
	int lab_a,lab_b,lab_c,lab_d,lab_e
	int a,b,stepx

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit
	pelse:=pbody.nextunit

	lab_a:=definelabel()
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi


	stacklooplabels(lab_a, lab_b, lab_c, lab_d)

!now start generating code

	evalunit(pfrom)
	pcl_gen(kpop, genmem_u(pindex))

	setmode_u(pindex)

	if pfrom.tag=j_const and pto.tag=j_const then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pcl_gen(kjump, pcl_genlabel(lab_e))
		fi
	else
		if pfrom.tag=j_const then				!reverse condition; compare mem:imm
			evalunit(pfrom)
			evalunit(pto)
			pcl_gen((down|kjumpgt|kjumplt),pcl_genlabel(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			pcl_gen((down|kjumplt|kjumpgt),pcl_genlabel(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	pcl_genx((down|kfordown|kforup),1, pcl_genlabel(lab_b))
	setmode_u(pindex)

	pcl_gen(kopnd, genmem_u(pindex))
	if pto.tag=j_const then
		pcl_gen(kopnd, pcl_genint(pto.value))
	else
		pcl_gen(kopnd, genmem_u(pto))
	fi

!RETURN
	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_swap(unit p,a,b) =
	evallv(a)
	evallv(b)
	pcl_gen(kswap)
	setmode_u(a)
end

proc do_convert(unit p,a) =
	int opc

	case p.tag
	when j_makelist, j_makeset then
	else
		case p.pclop
		when ksoftconv then
			gerror("CONV/SOFTCONV")
		when kerror then
			gerror("CONV/ERROR")
		else
			evalunit(a)
			pcl_gen(p.pclop)
		esac
		setmode_u(p)
		pcl_setoldtype(getpclmode(p.convmode))
	esac
end

proc do_dot(unit pdot) =
	int offset
	unit a,pname

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)

	if offset then
		genpushint(offset)
		pcl_gen(kpushptroff)
	else
		pcl_gen(kpushptr)
	fi
	pcl_setscale(1)

	setmode(getmemmode_m(pdot))
end

global function checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions,
!or -1 when offsets cannot be combined
	int offset,axmode

	case p.tag
	when j_dot then
		offset:=checkdotchain(p.a,pname)
		return p.offset+offset

	else							!anything else, is the start expression
		pname:=p
		return 0
	esac
	return 0
end

proc do_dotref(unit pdot) =
	int imode:=createrefmode(nil,pdot.mode,0)
	int offset
	unit a,pname


	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)

	if offset then
		genpushint(offset)
		pcl_gen(kaddrefoff)
		pcl_setscale(1)
!		pccurrxx.scale:=1
	fi
	setmode(imode)
end

proc do_storedot(unit pdot,pfield, rhs) =
	int offset
	unit a,pname

	evalunit(rhs)
	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)
	genpushint(offset)

	pcl_gen((pdot.resultflag|kstoreptroff|kpopptroff))
	pcl_setscale(1)
	setmode_u(pdot)
end

proc do_index(unit p,parray,pindex) =
	int addoffset,scale
	if ttbasetype[p.mode] in [tarray,tblock] then
		do_indexref(parray,pindex)
		return
	fi
	addoffset:=getindexoffset(pindex)

	evalarray(parray)
	evalunit(pindex)
	pcl_gen(kpushptroff)
	setmode(getmemmode_m(p))

	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale + addoffset*scale)
end

proc do_storeindex(unit p,parray,pindex,rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(pindex)

	evalunit(rhs)
	evalarray(parray)
	evalunit(pindex)

	pcl_gen((p.resultflag|kstoreptroff|kpopptroff))
	setmode_u(p.a)

	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

proc do_indexref(unit parray,pindex) =
	int addoffset,scale
	addoffset:=getindexoffset(pindex)

	evalarray(parray)
	evalunit(pindex)

	pcl_gen(kaddrefoff)
!
	setmode(tttarget[parray.mode])
	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

function getindexoffset(unit &pindex)int offset=
!convert index like [i+3] to [i], returning the +3 etc as a separate offset
	int addoffset:=0

	if pindex.tag=j_bin and pindex.pclop in [kadd, ksub] then
		if pindex.b.tag=j_const then		!incorporate const offset into lwb adjustment
			addoffset:=(pindex.pclop=kadd|pindex.b.value|-pindex.b.value)
			pindex:=pindex.a
		fi
	fi
	return addoffset
end

proc do_switch(unit p,pindex,pwhenthen,pelse, int loopsw,isref) =
	const maxlabels = 1000
	int minlab,maxlab,n,iscomplex,i
	int lab_a,lab_b,lab_d, labjump, elselab, labstmt,ax,bx,ismult
	[0..maxlabels]ref pclrec labels
	unit w,wt

	ismult:=p.mode<>tvoid and not loopsw

	minlab:=1000000
	maxlab:=-1000000		!highest index seen

	n:=0				!no. different values
	iscomplex:=0			!whether complex switch

	wt:=pwhenthen
	while wt do
		w:=wt.a
		while w do		!for each when expression
			case w.tag
			when j_makerange then
				ax:=w.a.value
				bx:=w.b.value
	dorange::
				for i:=ax to bx do
					minlab := min(i,minlab)
					maxlab := max(i,maxlab)
				od
			when j_const then		!assume int
				ax:=bx:=w.value
				goto dorange
			else
				gerror_s("Switch when2: not const: #",strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	n:=maxlab-minlab+1
	if n>maxlabels then
		gerror("Switch too big")
	fi

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a,lab_a,lab_a,lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then pcl_gen(kstartmult) fi

	evalunit(pindex)
	pcl_genxy(kswitch, minlab, maxlab,pcl_genlabel(labjump))
	pcl_gen(kopnd,pcl_genlabel(elselab))

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		pcl_gen(kswitchlabel,pcl_genlabel(elselab))
		labels[i]:=pccurr
	od
	pcl_gen(kendswitch)

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when j_makerange then
				ax:=w.a.value
				bx:=w.b.value
			when j_const then
					ax:=bx:=int(w.value)
			esac
			for i:=ax to bx do
				labels[i].labelno:=labstmt
			od
			w:=w.nextunit
		od

		evalunitx(wt.b,isref)
		if ismult then pcl_gen(kresetmult) fi
		genjumpl((loopsw|lab_a|lab_d))
		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse,isref)
		if ismult then pcl_gen(kendmult) fi
	fi

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi
end

proc do_select(unit p,a,b,c, int isref) =
	const maxlabels=256
	[maxlabels]ref pclrec labels
	int labend,labjump,n,i,elselab,labstmt,ismult
	unit q

	ismult:=p.mode<>tvoid and p.resultflag

	q:=b
	n:=0
	while q do
		if n>=maxlabels then gerror("selectx: too many labels") fi
		++n
		q:=q.nextunit
	od

	labend:=createfwdlabel()
	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then pcl_gen(kstartmult) fi
	evalunit(a)

	pcl_genxy(kswitch, 1, n, pcl_genlabel(labjump))
	pcl_gen(kopnd, pcl_genlabel(elselab))


	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		pcl_gen(kswitchlabel,pcl_genlabel(elselab))
		labels[i]:=pccurr
	od
	pcl_gen(kendswitch)

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q,isref)
		if ismult then pcl_gen(kresetmult) fi
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c,isref)
	if ismult then pcl_gen(kendmult) fi

	definefwdlabel(labend)
end

proc do_case(unit p,pindex,pwhenthen,pelse, int loopsw,isref) =
	const maxcase=256
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, opc, ismult

	int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, labelse
	unit w,wt

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	ismult:=p.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc,lab_abc,lab_abc,lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	if ismult then pcl_gen(kstartmult) fi
	evalunit(pindex)

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	ncases:=0
	wt:=pwhenthen
	while wt do
		w:=wt.a
		if ncases>=maxcase then
			gerror("too many cases")
		fi
		labtable[++ncases]:=createfwdlabel()
		unittable[ncases]:=wt.b

		while w do
			evalunit(w)
			pcl_gen(kcasejumpeq, pcl_genlabel(w.whenlabel:=labtable[ncases]))
			setmode_u(w)
			w:=w.nextunit
		od

		wt:=wt.nextunit
	od

	pcl_gen(kpopstack)
	setmode_u(pindex)

	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		evalunitx(unittable[i],isref)
		if ismult then pcl_gen(kresetmult) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse,isref)
		if ismult then pcl_gen(kendmult) fi
	fi

	if loopsw then
		genjumpl(lab_abc)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi

	--casedepth
end

proc do_dotindex(unit p,a,b) =
	evalunit(a)
	evalunit(b)

	pcl_gen(kdotindex)
	setmode(ti64)
end

proc do_dotslice(unit p,a,b) =
	evalunit(a)
	evalunit(b.a)
	evalunit(b.b)

	pcl_gen(kdotslice)
	setmode(ti64)
end

proc do_read(unit p,a) =
	int m

	m:=p.mode

	if a=nil then
		a:=pzero
	fi

	if ttisinteger[m] then
		pcl_gen_sysfn(sysfn_read_i64,a)
	elsif ttisreal[m] and ttsize[m]=8 then
		pcl_gen_sysfn(sysfn_read_r64,a)
	elsif m=trefchar then
		pcl_gen_sysfn(sysfn_read_str,a)
	else
CPL =STRMODE(M)
		GERROR("CAN'T READ THIS ITEM")
	fi
	setmode_u(p)
end

proc do_readln(unit a) =
	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			pcl_gen_sysproc(sysfn_read_fileline, a)
		when tu8,tc8 then
			pcl_gen_sysproc(sysfn_read_strline, a)
		else
			gerror("rd@dev?")
		esac
	else
		pcl_gen_sysproc(sysfn_read_conline)
	fi
end

proc docond(int opc,unit p,int lab)=
	genjumpcond(opc,p,lab)
end

proc do_syscall(unit p,a)=
	case p.fnindex
	when sysfn_getnprocs then
		pcl_gen(kgetnprocs)
	when sysfn_getprocname then
		evalunit(a)
		pcl_gen(kgetprocname)
	when sysfn_getprocaddr then
		evalunit(a)
		pcl_gen(kgetprocaddr)
	esac
	setmode(ti64)
end

proc do_slice(unit a,b, int doref=0) =
!generate separate code for (ptr, length) parts

	IF DOREF THEN GERROR("DOSLICE/REF?") fi

	if b=nil then

		if a.tag=j_const then			!assume string
			genpushint(strlen(a.svalue))
		else
			genpushint(ttlength[a.mode])
		fi
		evalarray(a)

	else
!worth checking for const bounds? Both must be const to make it worthwhile
		if b.a.tag=b.b.tag=j_const then
			genpushint(b.b.value-b.a.value+1)
		else
			evalunit(b.b)
			evalunit(b.a)
			pcl_gen(ksub)
			setmode(ti64)
			genpushint(1)
			pcl_gen(kadd)
		fi
		setmode(ti64)

		do_indexref(a,b.a)
	fi

	pcl_gen(kmakeslice)
	setmode(tu128)
end

proc do_assignblock(unit p,a,b) =
!fstore=1 when result is needed
!method used is::
! load ref to lhs
! load ref to rhs
! do block xfer, not using the stack

	if b.tag=j_makelist then
		if ttbasetype[a.mode]=tarray then
			do_assignarray(a,b)
		else
			do_assignrecord(a,b)
		fi
	else
		GERROR("ASSIGN BLOCK")
	fi
end

proc do_assignarray(unit a,b)=
	unit passign, pindex, pconst,q
	int index

	if ttbasetype[tttarget[a.mode]]=tc8 then
		gerror("Assignment not suitable for []char type")
	fi

	pconst:=createconstunit(1,ti64)
	pindex:=createunit2(j_index,a,pconst)
	passign:=createunit2(j_assign,pindex, b.a)
	passign.mode:=pindex.mode:=tttarget[a.mode]

	index:=ttlower[a.mode]
	q:=b.a

	while q do
		pconst.value:=index
		pconst.resultflag:=1
		passign.b:=q
		evalunit(passign)

		++index
		q:=q.nextunit
	od

end

proc do_assignrecord(unit a,b)=
	unit passign, pdot, pfield,q
	int m,fieldtype
	symbol d,e

	pfield:=createunit0(j_name)
	pdot:=createunit2(j_dot,a,pfield)
	passign:=createunit2(j_assign,pdot, b.a)
	passign.mode:=pdot.mode:=tttarget[a.mode]

	m:=a.mode
	d:=ttnamedef[m]
	e:=d.deflist
	q:=b.a
	while e do
		if e.nameid=fieldid and e.mode<>tbitfield then
			fieldtype:=e.mode
			pfield.def:=e
			passign.mode:=pfield.mode:=pdot.mode:=fieldtype
			passign.b:=q
			pdot.offset:=e.offset
			evalunit(passign)
			q:=q.nextunit
		fi
		e:=e.nextdef
	od
end

proc pushrhs(unit a)=
	if a=nil then return fi
	pushrhs(a.nextunit)
	evalunit(a)
end

proc do_multassign(unit a,b)=
	unit p
	int nlhs,nrhs
	symbol d

	nlhs:=a.length

	if b.tag=j_callfn then
		evalunit(b)
		if b.a.tag<>j_name then
			gerror("multassign from fn: not simple fn")
		fi
		d:=b.a.def
		nrhs:=d.nretvalues

		if a.tag<>j_makelist then		!mult-ret fn assigned to scalar
			if a.nextunit then GERROR("MULTASS?") fi
			nlhs:=1
		else
			a:=a.a					!point to elements of makelist
		fi

	else
		nrhs:=b.length
		pushrhs(b.a)			!push rhs elements in right-to-left order
		a:=a.a					!point to elements of makelist

	fi

	repeat
		switch a.tag
		when j_name then
			pcl_gen(kpop,genmem_u(a))
		when j_index, j_slice,j_dot then
			evalref(a)
			pcl_gen(kpopptr,pcl_genint(0))
		when j_ptr then
			evalunit(a.a)
			pcl_gen(kpopptr,pcl_genint(0))
		when j_if, j_longif, j_case, j_switch, j_select then
			evalref(a)
			pcl_gen(kpopptr,pcl_genint(0))
		when j_dotindex then
			evalref(a.a)
			evalunit(a.b)
			pcl_gen(kpopdotindex)
		else
			cpl jtagnames[a.tag]
			gerror("Bad mult assign element")
		end switch

		setmode_u(a)

		a:=a.nextunit
	until a=nil

	d:=getprocretmodes(b)

	for i:=nlhs+1 to nrhs do
		pcl_gen(kpopstack)
		setmode(ttmult[d.mode,i])
	od
end

proc do_recase(unit p,a)=
	unit q,wt,w
	int destlab,casevalue

	if casedepth=0 then
		gerror("recase outside case stmt")
	fi

	if a then
		casevalue:=a.value
	else				!a=null means goto else
		genjumpl(caseelse[casedepth])
	fi

	q:=casestmt[casedepth]

	destlab:=0

	wt:=q.b
	while wt do
		w:=wt.a
		while w do
			if w.tag=j_const and ttisinteger[w.mode] and w.value=casevalue then
				destlab:=w.whenlabel
				exit all
			fi
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	if destlab=0 then
		genjumpl(caseelse[casedepth])
	else
		genjumpl(destlab)
	fi
end

proc do_empty(unit p,a)=
	evallv(a)

	pcl_gen(kclear)

	setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value,ti64)
end

function condtopclop(int cond, baseop)int=
!turn keq etc into kjumpeq etc
!baseop is kjumpeq, kseteq, kselecteq
	return baseop+(cond-keq)
end
