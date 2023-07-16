const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

const dodotchains=1
!const dodotchains=0

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
	unit a,b,c
	symbol d
	ref[]int32 pmult

	if p=nil then return fi
	mlineno:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	switch p.tag
	when jconst         then do_const(p)
	when jnull          then
	when jname          then do_name(p)
	when jblock then
		do_block(p)
	when jcallproc, jcallfn then
		do_callproc(p,a,b)
	when jreturn        then do_return(p,a)
	when jreturnmult    then do_returnmult(p,a)
	when jassign        then do_assign(p,a,b)
	when jassignms      then do_assignms(a,b)
	when jassignmm      then do_assignmm(a,b)
	when jassignmdrem   then do_assignmdrem(a,b)
	when jto            then do_to(p,a,b)
	when jif            then do_if(p,a,b,c,0)
	when jforup         then do_for(p,a,b,c,0)
	when jfordown       then do_for(p,a,b,c,1)
	when jforall        then do_forall(p,a,b,c,0)
	when jforallrev     then do_forall(p,a,b,c,1)
	when jwhile         then do_while(p,a,b,c)
	when jrepeat        then do_repeat(p,a,b)
	when jgoto          then do_goto(a)
	when jlabeldef      then do_labeldef(p)
	when jredo          then do_exit(p,1)
	when jnext          then do_exit(p,2)
	when jexit          then do_exit(p,3)
	when jdo            then do_do(p,a,b)
	when jcase          then do_case(p,a,b,c,0,0)
	when jdocase        then do_case(p,a,b,c,1,0)
	when jswitch        then do_switch(p,a,b,c,0,0)
	when jdoswitch      then do_switch(p,a,b,c,1,0, 0)
	when jdoswitchu      then do_switch(p,a,b,c,1,0, 1)
	when jrecase        then do_recase(p,a)
	when jswap          then do_swap(p,a,b)
	when jselect        then do_select(p,a,b,c,0)
	when jprint,jprintln then
		do_print(p,a,b)
	when jfprint,jfprintln then
		do_print(p,a,b)
	when jread	        then do_read(p,a)
	when jreadln        then do_readln(a)
	when jstop          then do_stop(p,a)
	when jeval          then
		evalunit(a)
		genpc(keval)
	when jandl          then do_andl(p,a,b)
	when jorl           then do_orl(p,a,b)

	when jmakerange     then GENPC_COMMENT("MAKERANGE")

	when jcmp           then do_setcc(p,a,b)
	when jcmpchain      then do_setccchain(p,a)

	when jbin           then do_bin(p,a,b)
	when jindex         then do_index(p,a,b)
	when jslice         then do_slice(p,a,b)
	when jmakeslice     then
		evalunit(a)
		evalunit(b)
		genpc((p.resultflag|kstoresliced|kstoreslice), genmem_d(newblocktemp(p.mode)))
		setmode(tslice)

	when jdotindex      then do_dotindex(p,a,b)
	when jdotslice      then do_dotslice(p,a,b)
	when jdot           then do_dot(p)
	when jptr           then do_ptr(p,a)
	when jaddrof        then evalref(a,b)

	when jaddroffirst   then evalref(a)
!	when jaddrvar       then do_addrvar(p,a)
	when jconvert       then do_convert(p,a)
	when jtypepun       then do_typepun(p,a)
	when jshorten       then do_shorten(p,a)
	when jtypeconst     then do_typeconst(p)

	when junary         then do_unary(p,a)

	when jnotl          then do_notl(p,a)
	when jistruel       then do_istruel(p,a)

	when jincr          then
		if p.pclop in [kincr, kdecr] then
			do_incr(p,a)
		else
			do_incrload(p,a)
		fi
!
	when jbinto         then do_binto(p,a,b)
!
	when junaryto       then do_unaryto(p,a)
!
	when jsyscall then
		do_syscall(p,a)

	when jassem         then
		genpc(kassem,genpc_assem(p))
		setmode_u(p)

	when jcvlineno      then
		genpc(kload,genpc_int(getlineno(p.pos)))

	when jempty         then do_empty(p,a)
	when jcopy          then do_copy(p,a)

else
		GERROR_S("UNSUPPORTED TAG ",JTAGNAMES[P.TAG])
		return
	end switch

	if p.mode<>tvoid and not p.resultflag then
		case p.tag
		when jassign, jcallproc, jsyscall then

		else
			genpc(kunload)
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

!CPL "EVALREF",JTAGNAMES[P.TAG]

	switch p.tag
	when jname then
		genpushmemaddr_d(p.def)
!CPL "EVALREF",P,Q
		if q then					!addrof may have optional byte offset
			genpushint(q.value)
			genpc(kaddrefx)
			pcl_setscale(1)
			setmode(tu8)
		fi
	when jindex then
		do_indexref(a,b)

	when jdot then
		do_dotref(p)

	when jptr then
		evalunit(p.a)

	else
		case p.tag
		when jif then
			do_if(p,a,b,c,1)
!		when jselect then
!			do_select(p,a,b,c,1)
!		when jswitch then
!			do_switch(p,a,b,c,0,1)
!		when jcase then
!			do_case(p,a,b,c,0,1)
		elsif ttisblock[p.mode] then
			evalunit(p)

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
	case ttbasetype[p.mode]
	when tslice then
		evalunit(p)
		genpc(ksliceptr)
		setmode(tu64)
	elsif p.mode=trefchar then
		evalunit(p)
	else
		evalref(p)
	esac

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
	when jandl then
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

	when jorl then
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

	when jnotl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt,q,lab)
		when kjumpt then
			genjumpcond(kjumpf,q,lab)
		esac

	when jistruel then
		evalunit(q)

		genpc((opc=kjumpt|kjumptrue|kjumpfalse),genpc_label(lab))
		setmode_u(q)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc,q,lab)

	when jcmp then

		gcomparejump(opc,p.pclop,q,r,lab)

	when jinrange then
		evalunit(q)

		if opc=kjumpt then
			lab2:=createfwdlabel()
			evalunit(r.a)
			genpc(kjumplt, genpc_label(lab2))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			genpc(kjumple, genpc_label(lab))
			setmode_u(q)
			definefwdlabel(lab2)
		else
			evalunit(r.a)
			genpc(kjumplt, genpc_label(lab))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			genpc(kjumpgt, genpc_label(lab))
			setmode_u(q)
		fi

	when jinset then
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
					genpc(kjumpeq, genpc_label(lab2))
					pccurr.popone:=1
				else
					genpc(kjumpne, genpc_label(lab))
				fi
				setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s, s:=s.nextunit do
				evalunit(s)
				genpc(kjumpeq, genpc_label(lab))
				setmode_u(q)
				if s.nextunit then pccurr.popone:=1 fi
			od
		fi

	when jcmpchain then
		r:=q.nextunit
		i:=1
		evalunit(q)
		if opc=kjumpf then
			while r do
				evalunit(r)
				if r.nextunit then
					genpc(kswapopnds)

					genpc(condtopclop(reversecond_order(reversecond(p.cmpgenop[i])),kjumpeq), genpc_label(lab))

					pccurr.popone:=1
				else
					genpc(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq), genpc_label(lab))
				fi

				setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(r)
				if r.nextunit then
					genpc(kswapopnds)
					genpc(condtopclop(reversecond_order(reversecond(p.cmpgenop[i])),kjumpeq), genpc_label(lab2))
					pccurr.popone:=1
				else
					genpc(condtopclop(p.cmpgenop[i],kjumpeq), genpc_label(lab))
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
		if p.mode not in [ti64,tu64,tbool64] then gerror_s("jumptrue/not i64:",strmode(p.mode)) fi

		genpc((opc=kjumpt|kjumptrue|kjumpfalse),genpc_label(lab))
		setmode(ti64)
	end switch
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

	genpc(condtopclop(cond,kjumpeq),genpc_label(lab))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	genpc(kjump,genpc_label(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #",mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] or mode=tbool then
		genpushint(p.value)
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

proc do_name(unit p)=
	symbol d

	d:=p.def
	case d.nameid
	when procid,dllprocid then
		genpushmemaddr_d(d)
	when labelid then
		if d.index=0 then
			d.index:=++mlabelno
		fi
		if p.resultflag then		!get label address
			genpc(kloadlabel, genpc_label(d.index))
		else
			genpc(kjump, genpc_label(d.index))
		fi
		p.resultflag:=0
		p.mode:=tvoid
!
	when fieldid then
		genpushint(d.offset)


	else
		genpushmem_d(d)
		setmode(getmemmode_m(p))
	esac
end

proc do_stop(unit p,a) =
	if a then
		evalunit(a)
	else
		genpc(kload,genpc_int(0))
	fi
	genpc(kstop)
end

proc do_andl(unit p,a,b) =
	int labfalse, labend

	genpc(kstartmx)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf,a,labfalse)
	genjumpcond(kjumpf,b,labfalse)

	genpushint(1)
	genpc(kresetmx)

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	genpc(kendmx)

	definefwdlabel(labend)
end

proc do_orl(unit p,a,b) =
	int labtrue, labfalse, labend

	genpc(kstartmx)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt,a,labtrue)
	genjumpcond(kjumpf,b,labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	genpc(kresetmx)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	genpc(kendmx)

	definefwdlabel(labend)
end

proc do_notl(unit p,a) =
	evalunit(a)
	genpc(p.pclop)
	setmode(ti64)
end

proc do_istruel(unit p,a) =
	evalunit(a)
	if a.mode=tbool then
		return
	fi
	genpc(p.pclop)
	setmode_u(a)
end

proc do_typepun(unit p, a) =
	evalunit(a)
	setmode_u(a)
	if a.mode=p.mode then return fi
	genpc(ktypepun)
	setmode(p.convmode)
	pccurr.oldmode:=ttbasetype[a.mode]
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

proc do_assign(unit p,a,b) =
!fstore=1 when result is needed
	unit c
	symbol d
	int offset

if a.tag=jname and not a.def.used then
	RETURN
FI


	if b.tag=jmakelist then
		if not p.resultflag then
			do_assignblock(p,a,b)		!(avoids pushing/popping block data)
			return
		fi
	fi

	if b.tag=jmakeslice and a.tag=jname then
		evalunit(b.a)
		evalunit(b.b)
		genpc((p.resultflag|kstoresliced|kstoreslice), genmem_u(a))
		return
	fi

	case a.tag
	when jindex then
		do_storeindex(p,a.a,a.b,b)
		return
	when jslice then
GERROR("ASS/SLICE")

	when jdot then
		do_storedot(a,a.b,b)
		return
	esac

	evalunit(b)
	if p.resultflag then
		genpc(kdupl)
	fi

	switch a.tag
	when jname then
		genpc(kstore, genmem_u(a))
	when jptr then
		evalref(a)

		if pccurr.opcode=kaddrefx then 
			pccurr.opcode:=kistorex
		else
			genpc(kistore)
		fi
		setmode(getmemmode_m(a))

	when jdotindex then
		evalref(a.a)
		evalunit(a.b)
		genpc(kstorebit)
		setmode_u(a.a)
		return
	when jdotslice then
		evalref(a.a)
		evalunit(a.b.a)
		evalunit(a.b.b)
		genpc(kstorebf)
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

	if pccurr.opcode=kaddrefx and
			p.pclop in [kaddrefx, ksubrefx] and
		ttisref[a.mode] and ttisinteger[b.mode] and b.tag=jconst then
		offset:=ttsize[tttarget[a.mode]]*b.value
		if p.pclop=kaddrefx then
			pcl_addoffset(offset)
		else
			pcl_addoffset(-offset)
		fi
		return
	fi

	evalunit(b)

	genpc(p.pclop)
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
	genpc(condtopclop(p.pclop,kseteq))
	setmode_u(a)
end

proc do_setccchain(unit p,q) =
	int lab1,lab2,i,cond
	unit r

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	i:=1

	genpc(kstartmx)

	evalunit(q)
	while r do
		evalunit(r)
		cond:=reversecond(p.cmpgenop[i])
		if r.nextunit then
			genpc(kswapopnds)
			cond:=reversecond_order(cond)
		fi

!		genpc_cond(kjumpeq, cond, genlabel(lab1))
		genpc(condtopclop(cond,kjumpeq), genpc_label(lab1))
		if r.nextunit then pccurr.popone:=1 fi

		setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	od

	genpushint(1)
	genpc(kresetmx)
	genpc(kjump, genpc_label(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	genpc(kendmx)
	definefwdlabel(lab2)
end

proc do_binto(unit p,a,b)=
	evallv(a)
	evalunit(b)

	genpc(p.pclop)
	setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi

end

proc do_unary(unit p,a) =
	evalunit(a)

	genpc(p.pclop)
	setmode_u(p)
	if p.pclop in [klen,kupb] and ttbasetype[a.mode]=tslice then
		if p.pclop=kupb then
			pcl_setxy(ttlower[a.mode],0)
		fi

		setmode(tslice)
	fi

end

proc do_unaryto(unit p,a)=
	evallv(a)

	genpc(p.pclop)
	setmode_u(a)
end

proc do_ptr(unit p,a)=
	pcl pprev

	evalunit(a)

	case pccurr.opcode
	when kaddrefx then 
		pccurr.opcode:=kiloadx
	when ksubrefx then
		pprev:=pccurr-1
		if pprev.opcode=kload and pprev.opndtype=int_opnd then
			pprev.value:=-pprev.value
			pccurr.opcode:=kiloadx
		fi
	else
		genpc(kiload)
	esac
	setmode(getmemmode_m(p))
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.index=0 then
		d.index:=++mlabelno
	fi
	print @&.str,d.name,,"::"
	genpc_comment(&.str)
	genpc(klabel,genpc_label(d.index))
end

proc do_goto(unit a)=
	symbol d

	if a.tag=jname and a.def.nameid=labelid then
		d:=a.def
		if d.index=0 then
			d.index:=++mlabelno
		fi
		genpc(kjump, genpc_label(d.index))
	else
		evalunit(a)
		genpc(kjumpptr)
	fi
end

proc do_do(unit p,a,b) =
	int lab_abc,lab_d

	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_d)

	evalblock(a)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_to(unit p,a,b) =
	unit cvar
	int lab_b,lab_c,lab_d,count

	cvar:=p.c

	a.mode:=ti64

	evalunit(a)
	genpc(kstore,genmem_u(cvar))
	setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b,lab_c,lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		genpc(kjumple,genpc_label(lab_d))
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

	genpc(kto,genpc_label(lab_b))
	genpc(kopnd,genmem_u(cvar))

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

	stacklooplabels(lab_b, lab_c, lab_d)

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

	stacklooplabels(lab_ab, lab_c, lab_d)

	evalblock(a)

	definefwdlabel(lab_c)

	unless b.tag=jconst and b.value=0 then
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

proc do_if(unit p, pcond, plist, pelse, int isref) =
	int labend,i,lab2,ismult

	labend:=createfwdlabel()
	ismult:=p.mode<>tvoid

	i:=0
	if ismult then genpc(kstartmx) fi

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf,pcond,lab2)

		evalunitx(plist,isref)
		if ismult then genpc(kresetmx) fi

		if pcond.nextunit or pelse then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
	od

	if pelse then
		evalunitx(pelse,isref)
		if ismult then genpc(kendmx) fi
	fi
	definefwdlabel(labend)
end

proc do_return(unit p,a) =
	if a then
		evalunit(a)

		genpc(ksetret)
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
	genpc_x(ksetretmult, nparams)

	genjumpl(retindex)
	p.resultflag:=1
end

proc do_callproc(unit p,a,b) =
	[maxparams]unit paramlist
	int nparams,nmult,isptr,nvariadics, blockret, nret, size, isfn
	int oldstackdepth
	symbol d,dblock
	symbol dtemp
	ref[]int32 pmult
	unit q

	isptr:=0
	isfn:=p.tag=jcallfn

	case a.tag
	when jname then
		d:=a.def

	when jptr then
		d:=ttnamedef[a.mode]
		isptr:=1
	else
		gerror("call/not ptr")
	esac

	nparams:=0
	nvariadics:=0
	blockret:=0

	if ttisblock[p.mode] then
		blockret:=1
		nparams:=1
		paramlist[1]:=nil			!will be extra blocktemp
	fi

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparams then gerror("maxparams") fi
		paramlist[++nparams]:=q

		if d.varparams and nparams>=d.varparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		fi
	od

	genpc(ksetcall)
	pccurr.nargs:=nparams

	for i:=nparams downto 1 do			!downto 
		q:=paramlist[i]
		if q then
			evalunit(q)
			if nvariadics and i>=nvariadics and pccurr.pmode=tr32 then
				genpc(kfwiden)
				pccurr.pmode:=tr64
				pccurr.oldmode:=tr32
			fi

!			if i>4 then
				genpc(ksetarg)
				setmode_u(q)
				pccurr.x:=i
!			fi
		else								!temp block
			dblock:=newblocktemp(p.mode)
			dblock.used:=1
			genpc(kload, genmem_d(dblock))
			setmode(p.mode)
		fi
	od

	if not isptr then
		genpc((isfn|kcallf|kcallp), genmemaddr_d(d))
	else
		evalunit(a.a)
		genpc((isfn|kicallf|kicallp))
	fi

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
		setmode(getmemmode_m(p))
	fi

	if d.nretvalues>1 and isfn then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			genpc(ktype)
!			genpc(kopnd, genint(0))
			setmode(pmult[i])
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
			genpc_sysproc(sf_print_startfile,a)
		when tc8 then
			genpc_sysproc(sf_print_startstr,a)
		when tref then
			genpc_sysproc(sf_print_startptr,a)
		else
			gerror("@dev?")
		esac
	else
		needprintend:=1
		genpc_sysproc(sf_print_startcon)
	fi

	q:=b

	case p.tag
	when jfprint,jfprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		fi
		genpc_sysproc(sf_print_setfmt,q)
		q:=p.c
	esac

	while q do
		case q.tag
		when jfmtitem then
			fmt:=q.b
			r:=q.a
			m:=r.mode
		when jnogap then
			genpc_sysproc(sf_print_nogap)
			q:=q.nextunit
			next
		when jspace then
			genpc_sysproc(sf_print_space)
			q:=q.nextunit
			next
		else
			fmt:=nil
			r:=q
			m:=q.mode
		esac

		switch ttbasetype[m]
		when ti64 then
			fn:=sf_print_i64
			if not fmt then fn:=sf_print_i64_nf fi
		when tu64 then
			fn:=sf_print_u64
		when tr32 then
			fn:=sf_print_r32
		when tr64 then
			fn:=sf_print_r64
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sf_print_str
				if not fmt then fn:=sf_print_str_nf fi
			else
				fn:=sf_print_ptr
				if not fmt then fn:=sf_print_ptr_nf fi
			fi
		when tbool then
			fn:=sf_print_bool
		when tarray then
			GERROR("PRINTARRAY")
			q:=q.nextunit
		when trecord then
			GERROR("PRINTRECORD")
		when tslice then
			if tttarget[m]=tc8 then
				fn:=sf_print_strsl
			else
				gerror("PRINTSLICE")
			fi

		when tc64 then
			fn:=sf_print_c8

		else
			PRINTLN STRMODE(M), STRMODE(TTBASETYPE[M])
			gerror_s("PRINT/T=#",strmode(m))
		end switch

		case fn
		when sf_print_i64_nf, sf_print_str_nf, sf_print_ptr_nf then
			genpc_sysproc(fn, r)
		else
			genpc_sysproc(fn, r, (fmt|fmt|pzero))
		esac

		q:=q.nextunit
	od

	case p.tag
	when jprintln,jfprintln then
		genpc_sysproc(sf_print_newline)
	esac
	if needprintend then
		genpc_sysproc(sf_print_end)
	fi
end

proc do_incr(unit p,a) =
	evallv(a)
	genpc(p.pclop)
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
	genpc(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc do_for(unit p,pindex,pfrom, pbody, int down) =
!Structure:
!	Forup/to
!		pindex -> [ptoinit]
!		pfrom -> pto -> [pstep]
!		pbody -> [pelse]
!When pto is complex, then pto refers to an AV variable, and ptoinit contains code
!to evaluate the complex pto, and assign it to that AV

	unit pto, pstep, pelse, px, plimit, ptoinit, ptemp
	int lab_b,lab_c,lab_d,lab_e
	int a,b,stepx

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit
	ptoinit:=pindex.nextunit

	case pto.tag
	when jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.parammode=out_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	when jconst, jname then
	else
		if pto.mode=ti64 then
			ptemp:=createname(newblocktemp(ti64))
			ptemp.mode:=ti64
			ptemp.resultflag:=1
			evalunit(pto)
			genpc(kstore, genmem_u(ptemp))
			setmode(ti64)
			pto:=ptemp
		else
			gerror("Complex TO")
		fi
	esac

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code
	evalunit(pfrom)
	genpc(kstore,genmem_u(pindex))
	setmode_u(pindex)

	if ptoinit then			!using temp for limit
		ptoinit.resultflag:=0
		evalunit(ptoinit)
	fi

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			genpc(kjump, genpc_label(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			genpc((down|kjumpgt|kjumplt),genpc_label(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			genpc((down|kjumplt|kjumpgt),genpc_label(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
		if pstep.tag<>jconst then
			gerror("for/step non-const not ready")
		fi
		stepx:=pstep.value
		if stepx<=0 then
			gerror("Bad for-step")
		fi
		genpc_x((down|kfordown|kforup),stepx, genpc_label(lab_b))
		setmode_u(pindex)
	else
		genpc_x((down|kfordown|kforup),1, genpc_label(lab_b))
		setmode_u(pindex)
	fi

	genpc(kopnd, genmem_u(pindex))
	case pto.tag
	when jconst then
		genpc(kopnd, genpc_int(pto.value))
	when jname then
		genpc(kopnd, genmem_u(pto))
	esac

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p,pindex,plist, pbody, int down) =
!Structure:
!	forall
!		pindex -> plocal -> pfrom -> pto
!		plist -> passign
!		pbody -> [pelse]

	unit plocal, pfrom, pto, pelse, px, plimit, passign
	int lab_b,lab_c,lab_d,lab_e
	int a,b,stepx

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit
	pelse:=pbody.nextunit

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_b, lab_c, lab_d)

!now start generating code

	evalunit(pfrom)
	genpc(kstore, genmem_u(pindex))

	setmode_u(pindex)

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			genpc(kjump, genpc_label(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pfrom)
			evalunit(pto)
			genpc((down|kjumpgt|kjumplt),genpc_label(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			genpc((down|kjumplt|kjumpgt),genpc_label(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	genpc_x((down|kfordown|kforup),1, genpc_label(lab_b))
	setmode_u(pindex)

	genpc(kopnd, genmem_u(pindex))
	case pto.tag
	when jconst then
		genpc(kopnd, genpc_int(pto.value))
	when jname then
		genpc(kopnd, genmem_u(pto))
	else
		gerror("forall/to: not const or name")
	esac

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
	genpc(kswap)
	setmode_u(a)
end

proc do_convert(unit p,a) =
	int opc,n,oldmode
	unit q

	oldmode:=getpclmode(p.convmode)

	case p.pclop
	when ksoftconv then
		evalunit(a)
		return
	when kerror then
		gerror("CONV/ERROR")
	else
		evalunit(a)
		genpc(p.pclop)
	esac
	setmode_u(p)

	pcl_setoldtype(oldmode)
end

global function checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions,
!or -1 when offsets cannot be combined
	int offset,axmode

	case p.tag
	when jdot then
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
		genpc(kaddrefx)
		pcl_setscale(1)
	fi
	setmode(imode)
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
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kiloadx
			pccurr.extra+:=offset
			finish
		else
			genpushint(offset)
			genpc(kiloadx)
		fi
	else
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kiloadx
			finish
		else
			genpc(kiload)
		fi
	fi
	pcl_setscale(1)
finish::
	setmode(getmemmode_m(pdot))
end

proc do_storedot(unit pdot,pfield, rhs) =
	int offset
	unit a,pname

	evalunit(rhs)
	if pdot.resultflag then
		genpc(kdupl)
	fi

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
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kistorex
			pccurr.extra+:=offset
			finish
		else
			genpushint(offset)
			genpc(kistorex)
		fi
	else
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kistorex
			finish
		else
			genpc(kistore)
		fi
	fi
	pcl_setscale(1)
finish::
	setmode_u(pdot)
end

proc do_index(unit p,parray,pindex) =
	int addoffset,scale,offset

	if ttisblock[p.mode] then
		do_indexref(parray,pindex)
		return
	fi

	addoffset:=getindexoffset(parray,pindex)

	evalarray(parray)
	scale:=ttsize[tttarget[parray.mode]]
	offset:=-ttlower[parray.mode]*scale + addoffset*scale

	evalunit(pindex)
	genpc(kiloadx)

	pcl_setscale(scale)
	pcl_setoffset(offset)
finish::
	setmode(getmemmode_m(p))
end

proc do_storeindex(unit p,parray,pindex,rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(parray,pindex)

	evalunit(rhs)
	if p.resultflag then
		genpc(kdupl)
	fi

	evalarray(parray)
	evalunit(pindex)

	genpc(kistorex)
	setmode_u(p.a)

	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

proc do_indexref(unit parray,pindex) =
	int addoffset,scale
!cpl "DOINDEXREF"
	addoffset:=getindexoffset(parray,pindex)

	evalarray(parray)
	evalunit(pindex)

	genpc(kaddrefx)

	setmode(tttarget[parray.mode])
	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

function getindexoffset(unit parray, &pindex)int offset=
!convert index like [i+3] to [i], returning the +3 etc as a separate offset
	int addoffset:=0

	if pindex.tag=jbin and pindex.pclop in [kadd, ksub] then
		if pindex.b.tag=jconst then		!incorporate const offset into lwb adjustment
			addoffset:=(pindex.pclop=kadd|pindex.b.value|-pindex.b.value)
			pindex:=pindex.a
		fi
	fi
	return addoffset
end

proc do_switch(unit p,pindex,pwhenthen,pelse, int loopsw,isref, isfast=0) =
	const maxlabels = 1000
	int minlab,maxlab,n,iscomplex,i
	int lab_a,lab_b,lab_d, labjump, elselab, labstmt,ax,bx,ismult
	[0..maxlabels]ref pclrec labels
	unit w,wt

	ismult:=p.mode<>tvoid and not loopsw
	if not loopsw then isfast:=0 fi

	minlab:=1000000
	maxlab:=-1000000		!highest index seen

	n:=0				!no. different values
	iscomplex:=0			!whether complex switch

	wt:=pwhenthen
	while wt do
		w:=wt.a
		while w do		!for each when expression
			case w.tag
			when jmakerange then
				ax:=w.a.value
				bx:=w.b.value
	dorange::
				for i:=ax to bx do
					minlab := min(i,minlab)
					maxlab := max(i,maxlab)
				od
			when jconst then		!assume int
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
		stacklooplabels(lab_a,lab_a,lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then genpc(kstartmx) fi

	evalunit(pindex)
	genpc_xy((isfast|kswitchu|kswitch), minlab, maxlab,genpc_label(labjump))
	genpc(kopnd,genpc_label(elselab))

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		genpc(kswitchlabel,genpc_label(elselab))
		labels[i]:=pccurr
	od
	genpc(kendswitch)

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				ax:=w.a.value
				bx:=w.b.value
			when jconst then
					ax:=bx:=int(w.value)
			esac
			for i:=ax to bx do
				labels[i].labelno:=labstmt
			od
			w:=w.nextunit
		od

		evalunitx(wt.b,isref)
		if ismult then genpc(kresetmx) fi
		if isfast then
			evalunit(pindex)
			genpc_xy(kswitchu, minlab, maxlab,genpc_label(labjump))
			genpc(kopnd,genpc_label(elselab))
		else
			genjumpl((loopsw|lab_a|lab_d))
		fi
		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse,isref)
		if ismult then genpc(kendmx) fi
	fi

	if loopsw then
		if isfast then
			evalunit(pindex)
			genpc_xy(kswitchu, minlab, maxlab,genpc_label(labjump))
			genpc(kopnd,genpc_label(elselab))
		else
			genjumpl(lab_a)
		fi

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

	if ismult then genpc(kstartmx) fi
	evalunit(a)

	genpc_xy(kswitch, 1, n, genpc_label(labjump))
	genpc(kopnd, genpc_label(elselab))


	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		genpc(kswitchlabel,genpc_label(elselab))
		labels[i]:=pccurr
	od
	genpc(kendswitch)

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q,isref)
		if ismult then genpc(kresetmx) fi
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c,isref)
	if ismult then genpc(kendmx) fi

	definefwdlabel(labend)
end

proc do_case(unit p,pindex,pwhenthen,pelse, int loopsw,isref) =
	const maxcase=500
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, opc, ismult

	int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, labelse
	unit w,wt

	loopsw:=p.tag=jdocase

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	ismult:=p.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc,lab_abc,lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	if ismult then genpc(kstartmx) fi

	ncases:=0
	if pwhenthen=nil then
		if ismult then gerror("case") fi
		goto skip
	fi

	evalunit(pindex)

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	wt:=pwhenthen

	while wt do
		w:=wt.a
		if ncases>=maxcase then
			gerror("too many cases")
		fi
		labtable[++ncases]:=createfwdlabel()
		unittable[ncases]:=wt.b

		while w do
!			if w.nextunit or wt.nextunit then genpc(kdouble) fi
			evalunit(w)
			genpc(kjumpeq, genpc_label(w.whenlabel:=labtable[ncases]))
			if w.nextunit or wt.nextunit then pccurr.popone:=1 fi
			setmode_u(w)
			w:=w.nextunit
		od

		wt:=wt.nextunit
	od

skip::
	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		evalunitx(unittable[i],isref)
		if ismult then genpc(kresetmx) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse,isref)
		if ismult then genpc(kendmx) fi
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

	genpc(kloadbit)
	setmode(ti64)
end

proc do_dotslice(unit p,a,b) =
	evalunit(a)
	evalunit(b.a)
	evalunit(b.b)

	genpc(kloadbf)
	setmode(ti64)
end

proc do_read(unit p,a) =
	int m

	m:=p.mode

	if a=nil then
		a:=pzero
	fi

	if ttisinteger[m] then
		genpc_sysfn(sf_read_i64,a)
	elsif ttisreal[m] and ttsize[m]=8 then
		genpc_sysfn(sf_read_r64,a)
	elsif m=trefchar then
		genpc_sysfn(sf_read_str,a)
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
			genpc_sysproc(sf_read_fileline, a)
		when tu8,tc8 then
			genpc_sysproc(sf_read_strline, a)
		else
			gerror("rd@dev?")
		esac
	else
		genpc_sysproc(sf_read_conline)
	fi
end

proc docond(int opc,unit p,int lab)=
	genjumpcond(opc,p,lab)
end

proc do_syscall(unit p,a)=
	case p.fnindex
	when sf_getnprocs then
		genpc(kgetnprocs)
	when sf_getprocname then
		evalunit(a)
		genpc(kgetprocname)
	when sf_getprocaddr then
		evalunit(a)
		genpc(kgetprocaddr)

	else
		GENPC_COMMENT("SYSCALL/GENERIC")
	esac
	setmode(ti64)
end

proc do_slice(unit p,a,b, int doref=0) =
!generate separate code for (ptr, length) parts

	IF DOREF THEN GERROR("DOSLICE/REF?") fi

	if b=nil then

		evalarray(a)
		if a.tag=jconst then			!assume string
			genpushint(strlen(a.svalue))
		else
			genpushint(ttlength[a.mode])
		fi

	else
!worth checking for const bounds? Both must be const to make it worthwhile
		do_indexref(a,b.a)
		if b.a.tag=b.b.tag=jconst then
			genpushint(b.b.value-b.a.value+1)
		else
			evalunit(b.b)
			evalunit(b.a)
			genpc(ksub)
			setmode(ti64)
			genpushint(1)
			genpc(kadd)
		fi
		setmode(ti64)

	fi

!GENPC_COMMENT("DOSLICE")
!CPL("DOSLICE")
!CPL "DOSLICE",=P.RESULTFLAG
	genpc((p.resultflag|kstoresliced|kstoreslice), genmem_d(newblocktemp(p.mode)))
	setmode(tslice)
end

proc do_assignblock(unit p,a,b) =
!fstore=1 when result is needed
!method used is::
! load ref to lhs
! load ref to rhs
! do block xfer, not using the stack

	if b.tag=jmakelist then
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
	pindex:=createunit2(jindex,a,pconst)
	passign:=createunit2(jassign,pindex, b.a)
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

	pfield:=createunit0(jname)
	pdot:=createunit2(jdot,a,pfield)
	passign:=createunit2(jassign,pdot, b.a)
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

proc do_assignms(unit a,b)=
	unit p
	int nlhs,nrhs
	symbol d

	nlhs:=a.length

	case b.tag
	when jcallfn then
		evalunit(b)
		if b.a.tag<>jname then
			gerror("multassign from fn: not simple fn")
		fi
		d:=b.a.def
		nrhs:=d.nretvalues

		a:=a.a					!point to elements of makelist
	elsif ttbasetype[b.mode]=tslice then
GERROR("DECONSTR SLICE NOT READY")
	else
		gerror("(a,b):=x; var only")
	esac

	poptomult(a)

	if nrhs>nlhs then
		d:=getprocretmodes(b)

		for i:=nlhs+1 to nrhs do
			genpc(kunload)
			setmode(ttmult[d.mode,i])
		od
	fi
end

proc do_assignmm(unit a,b)=
!(a,b,c):=(x,y,z)
	pushrhs(b.a)			!push rhs elements in right-to-left order
	genpc(kloadall)
	poptomult(a.a)
end

proc do_assignmdrem(unit a,b)=
!(a,b):=x divrem y
	evalunit(b)
	poptomult(a.a)
end

proc poptomult(unit a)=
!a is a linked list; assign n popped values to each element in turn 
	repeat
		switch a.tag
		when jname then
			genpc(kstore,genmem_u(a))
		when jindex, jslice,jdot then
			evalref(a)
			genpc(kistore,genpc_int(0))
		when jptr then
			evalunit(a.a)
			genpc(kistore,genpc_int(0))
		when jif, jcase, jswitch, jselect then
			evalref(a)
			genpc(kistore,genpc_int(0))
		when jdotindex then
			evalref(a.a)
			evalunit(a.b)
			genpc(kstorebit)
		else
			cpl jtagnames[a.tag]
			gerror("Bad mult assign element")
		end switch

		setmode_u(a)

		a:=a.nextunit
	until a=nil
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
			if w.tag=jconst and ttisinteger[w.mode] and w.value=casevalue then
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
	genpc(kclear)
	setmode_u(a)
end

proc do_copy(unit p,a)=
	evalunit(a)

	genpc(kcopyblock, genmem_d(newblocktemp(a.mode)))
	setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value)
end

function condtopclop(int cond, baseop)int=
!turn keq etc into kjumpeq etc
!baseop is kjumpeq, kseteq, kselecteq
	return baseop+(cond-keq)
end
