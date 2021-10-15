!M Compiler - x64 Target Code Generator 2
!import main
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib

import* cc_pcl

import cc_genpcl

const tintptr=tullong
const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

GLOBAL INT NADDTO
GLOBAL INT NADDTOX

macro evalexpr(p) = dx_expr(p)
macro evaladdr(p) = dx_exprref(p)
macro evalptr(p) = dx_expr(p)

[maxnestedloops]int continuestack		!labels for continue/break
[maxnestedloops]int breakstack
int loopindex							!current level of nested loop/switch blocks

const maxparams=200

const maxswitchrange=500
const maxcases=maxswitchrange
const maxswitchdepth=20

ref[]int sw_labeltable			!set from do-switch
ref[]int sw_valuetable
int sw_lower
int sw_ncases					!1..n for serial switch; 0 for simple
byte sw_defaultseen				!starts at 0, set to 1 when default: seen
int sw_defaultlabel
int sw_breaklabel

int maxreg=0

global proc do_stmt(unit p) =
	int oldclineno,value,i
	unit a,b
	symbol d
	[256]char str

	if p=nil then
		return
	fi

	oldclineno:=clineno
	clineno:=p.fileno<<24+p.lineno

	a:=p.a
	b:=p.b

	switch p.tag
	when j_block then
		while a, a:=a.nextunit do
			do_stmt(a)
		od

	when j_decl then
		do_decl(p.def)

	when j_callfn then
!CPL "CALLFN/STMT"
		dx_call(p,a,b)
		if pcl_getopcode() in [kcallfn, kcallfnptr] then
			pcl_gen(kpopstack)
		fi


	when j_return then
		do_return(a)

	when j_assign then
		dx_assign(a,b)

	when j_if then
		do_if(a,b,p.c)

	when j_for then
		do_for(a,b)

	when j_while then
		do_while(a,b)
!
	when j_dowhile then
		do_dowhile(a,b)
!
	when j_goto then
		do_goto(p.def)

	when j_labelstmt then
		do_labeldef(p.def)
		do_stmt(a)
!
	when j_casestmt then

		fprint @str,"case",p.index,,":"

		pcl_gencomment(pcm_copyheapstring(str))
		if sw_ncases=0 then
			pcl_gen(klabel,pcl_genlabel(sw_labeltable[p.value-sw_lower+1]))
		else
			value:=p.value
			for i:=1 to sw_ncases do
				if sw_valuetable[i]=value then
					pcl_gen(klabel,pcl_genlabel(sw_labeltable[i]))
					exit
				fi
			else
				gerror("case: serial switch not found")
			od
		fi
		do_stmt(a)

	when j_defaultstmt then
		sw_defaultseen:=1
		pcl_gencomment("default:")
		pcl_gen(klabel,pcl_genlabel(sw_defaultlabel))
		do_stmt(a)

	when j_breaksw then
		genjumpl(sw_breaklabel)

	when j_break then
		genjumpl(breakstack[loopindex])

	when j_continue then
		genjumpl(continuestack[loopindex])

	when j_switch then
		do_switch(p,a,b)
!
	when j_addto then
		dx_binto(kaddto,a,b,0)

	when j_subto then
		dx_binto(ksubto,a,b,0)

	when j_multo then
		dx_binto(kmulto, a,b,0)
!
	when j_divto then
		if gettypecat(a)='R' then
			dx_binto(kdivto,a,b,0)
		else
			dx_binto(kidivto,a,b,0)
		fi

	when j_remto then
		dx_binto(kiremto,a,b,0)

	when j_iandto then
		dx_binto(kiand,a,b,0)

	when j_iorto then
		dx_binto(kior,a,b,0)

	when j_ixorto then
		dx_binto(kixor,a,b,0)

	when j_shlto then
		dx_binto(kshl,a,b,0)

	when j_shrto then
		dx_binto(kshr,a,b,0)

	when j_preincr, j_postincr then
		do_incr(a,kincr)

	when j_predecr, j_postdecr then
		do_incr(a,kdecr)

!when j_null then
!	gerror("stmt/null")
!
	when j_exprlist then
		do_exprlist(a)

	else
!assume standalone expression (assign/call/addto/incr done above)

!CPL "LONE",=FSHOWNAMES
		if p.tag<>j_const or not fshownames then
			loneexpr(p)
		fi

	endswitch

!clineno:=oldclineno
end

proc dx_expr(unit p, int reg=1) =
	int oldclineno,value,i,m
	unit a,b
	[256]char str
	symbol d

	if p=nil then
		return
	fi

	oldclineno:=clineno
	clineno:=p.fileno<<24+p.lineno

	a:=p.a
	b:=p.b
	m:=p.mode

	switch p.tag
	when j_const then
		dx_const(p)

	when j_name then
		dx_name(p)

!	when j_widenmem then
!		dx_widen(a,m)

	when j_funcname then
		pcl_gen(kpush,genmemaddr_u(p))

	when j_assign then
		dx_assign(a,b)
!
	when j_andl,j_orl then
		dx_andorl(p)		!use non-short circuit versions for now

	when j_notl then
		dx_unary(knotl,a)

	when j_istruel then
		dx_unary(kistruel,a)

	when j_exprlist then
		dx_exprlist(a)

	when j_callfn then
		dx_call(p,a,b)

	when j_ifx then
		dx_ifx(a,b,p.c)

	when j_eq then dx_bin(keq,a,b)
	when j_ne then dx_bin(keq,a,b)
	when j_lt then dx_bin(klt,a,b)
	when j_le then dx_bin(kle,a,b)
	when j_ge then dx_bin(kge,a,b)
	when j_gt then dx_bin(kgt,a,b)
!
	when j_add then
		if ttisref[a.mode] and ttsize[b.mode]<=4 then
			b.mode:=tintptr
		fi
		dx_bin(kadd,a,b)

	when j_sub then
		dx_bin(ksub,a,b)

	when j_mul then
		dx_bin(kmul,a,b)
!
	when j_div then
		if gettypecat(a)='R' then
			dx_bin(kdiv,a,b)
		else
			dx_bin(kidiv,a,b)
		fi

	when j_rem then
		dx_bin(kirem,a,b)
!
	when j_iand then
		dx_bin(kiand,a,b)

	when j_ior then
		dx_bin(kior,a,b)

	when j_ixor then
		dx_bin(kixor,a,b)

	when j_shl then
		dx_bin(kshl,a,b)

	when j_shr then
		dx_bin(kshr,a,b)

	when j_ptr then
		evalexpr(a)
		pcl_gen(kpushptr)
		setmode_u(a)

!when j_ptroffset then
!	tx:=dx_ptroffset(p,a,b,p.c, 0, reg)

	when  j_addptr then
		dx_unary(kaddrefoff,a)

	when  j_subptr then
		if gettypecat(b)='I' then
			dx_unary(ksubrefoff,a)
		else
			dx_unary(ksubref, a)
		fi

	when j_convert then
		if m=tvoid then
			evalexpr(a)
		else
			dx_convert(p,a)
		fi

!	when j_scale then
!		dx_scale(p,a,b,reg)

	when j_neg then
		dx_unary(kneg,a)
!		evalunit(a)
!		pcl_gen(kneg)
!		setmode_u(a)

	when j_inot then
		dx_unary(kinot,a)
!		evalunit(a)
!		pcl_gen(kinot)
!		setmode_u(a)
!
	when j_preincr then dx_prepostincrx(p,a,kincrload)
	when j_predecr then dx_prepostincrx(p,a,kdecrload)

	when j_postincr then dx_prepostincrx(p,a,kloadincr)
	when j_postdecr then dx_prepostincrx(p,a,kloaddecr)

	when j_addto then
		dx_binto(kadd,a,b,1)

	when j_subto then
		dx_binto(ksub,a,b,1)

	when j_multo then
		dx_binto(kmul,a,b,1)

	when j_divto then
		if gettypecat(b)='R' then
			dx_binto(kdiv,a,b,1)
		else
			dx_binto(kidiv,a,b,1)
		fi

	when j_remto then
		dx_binto(kirem,a,b,1)

	when j_iandto then
		dx_binto(kiand,a,b,1)

	when j_iorto then
		dx_binto(kior,a,b,1)

	when j_ixorto then
		dx_binto(kixor,a,b,1)

	when j_shlto then
		dx_binto(kshl,a,b,1)

	when j_shrto then
		dx_binto(kshr,a,b,1)
!
	when j_sqrt then
		dx_unary(ksqrt,a)

	when j_addrof then
		evaladdr(a)

	when j_dot then
		dx_dot(p,a,b)

	else
		gerror_s("DX-EXPR: can't do tag: %s",jtagnames[p.tag])
	endswitch

	if reg=0 then
!	PCL_GENCOMMENT("POP VALUE")
		pcl_gen(kpopstack)
	fi


	clineno:=oldclineno
end

proc dx_exprref(unit p) =
	int oldclineno,value,i,m
	unit a,b
	[256]char str
	symbol d

	if p=nil then
		return
	fi

	oldclineno:=clineno
	clineno:=p.fileno<<24+p.lineno

	a:=p.a
	b:=p.b
	m:=p.mode

	switch p.tag
	when j_name then
GERROR("EXPRREF/NAME")
!		dx_name(p,reg,am)

!	when j_ptr then

!	when j_dot then
	else
		gerror_s("DX-EXPRREF: can't do tag: %s",jtagnames[p.tag])
	endswitch

	clineno:=oldclineno
end

proc loneexpr(unit p)=
	if p and p.tag<>j_null then
		dx_expr(p,0)
	fi
end

proc dx_assign(unit a,b,int reg=0) =
	dx_expr(b)

	case a.tag
	when j_name then
		pcl_gen((reg|kstore|kpop), genmem_d(a.def))
		setmode(a.def.mode)
!	when j_ptr then
!	when j_dot then
	else
		gerror("ASSIGN")
	esac
end

proc getlvalueopnd (unit a)=
	evalexpr(a)
end

proc dx_const(unit p)=
	int t

CPL =STRMODE(P.MODE)

	if (t:=ttbasetype[p.mode])>=tfirstint and t<=tlastint then
		pcl_gen(kpush, pcl_genint(p.value,getpclmode(p.mode)))

	elsif t=tfloat then
		pcl_gen(kpush, pcl_genreal32(p.xvalue))
	elsif t in [tdouble,tldouble] then
		pcl_gen(kpush, pcl_genreal(p.xvalue))

	elsif t=tref then
		if p.isstrconst then
			pcl_gen(kpush, pcl_genstring(p.svalue))

		elsif p.iswstrconst then
			gerror("push wstring")
		else
			pcl_gen(kpush, pcl_genint(p.value,tpu64))
		fi
	else
		gerror_s("dxconst %s",Strmode(p.mode))
	fi
end

proc do_labeldef(symbol d)=
!	pcl_gen(klabel,pcl_genlabel(d.index),genmemaddr_d(d))
	pcl_gen(klabelname,genmemaddr_d(d))
end

proc do_goto(symbol d)=
	if d.index=0 then
		gerror_s("Label not defined: %s",d.name)
	fi
	pcl_gen(kjump,pcl_genlabel(d.index))
end

proc dx_bin(int opc, unit a,b)=
	evalexpr(a)
	evalexpr(b)
	pcl_gen(opc)
	setmode_u(b)
end

proc do_if(unit a,b,c)=
	int lab1,lab2

	lab1:=createfwdlabel()

	genjumpcond(kjumpf,a,lab1)

	do_stmt(b)

	if c then
		lab2:=createfwdlabel()			!label past else part
		genjumpl(lab2)
		definefwdlabel(lab1)
		do_stmt(c)
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q,r
	int lab2

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
		genjumpcond(opc,q,lab)

	when j_eq,j_ne,j_lt,j_le,j_ge,j_gt then

		evalexpr(q)
		evalexpr(r)
		case p.tag
		when j_eq then opc:=kjumpeq
		when j_ne then opc:=kjumpne
		when j_lt then opc:=kjumplt
		when j_le then opc:=kjumple
		when j_ge then opc:=kjumpge
		when j_gt then opc:=kjumpgt
		esac
		pcl_gen(opc, pcl_genlabel(lab))
		setmode_u(q)

	when j_exprlist then
		while q and (r:=q.nextunit) do
			dx_expr(q)
			q:=r
		od

		genjumpcond(opc,q,lab)
	else			!other expression
		evalexpr(p)

		pcl_gen((opc|kjumptrue|kjumpfalse),pcl_genlabel(lab))
	endswitch
end

proc do_incr(unit a,int opc)=
int size

	evaladdr(a)
	pcl_gen(opc)
	setmode_u(a)
end

proc do_while (unit pcond, pbody) =
	int lab_b,lab_c,lab_d

	if pcond.tag=j_const and pcond.value then
		do_while1(pbody)
		return
	fi

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	genjumpl(lab_c)		!direct to condition code which is at the end

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	genjumpcond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_while1 (unit pbody) =
	int lab_b,lab_c,lab_d

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	genjumpl(lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc stacklooplabels(int a,b)=
!don't check for loop depth as that has been done during parsing
	continuestack[++loopindex]:=a
	breakstack[loopindex]:=b
end

proc do_dowhile(unit pbody, pcond) =
	int lab_b,lab_c,lab_d

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	genjumpcond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_for (unit pinit, pbody) =
	unit pcond,pincr
	int lab_b,lab_c,lab_d,lab_cond

	pcond:=pinit.nextunit
	pincr:=pcond.nextunit

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_cond:=createfwdlabel()

	if pinit.tag<>j_null then
		do_stmt(pinit)
	fi

	genjumpl(lab_cond)		!direct to condition code which is at the end

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	do_stmt(pincr)
	definefwdlabel(lab_cond)

	if pcond.tag<>j_null then
		genjumpcond(kjumpt,pcond,lab_b)
	else
		genjumpl(lab_b)
	fi
	definefwdlabel(lab_d)
	--loopindex
end

proc pushffparams(unit p)=
	[maxparams]unit paramlist
	int n

	n:=0
	while p do
		if n>=maxparams then gerror("TOO MANY PARAMS") fi
		++n
		paramlist[n]:=p
		p:=p.nextunit
	od

	for i:=n downto 1 do
		evalexpr(paramlist[i])
	od
end

proc dx_call(unit p,a,b)=
	ref paramrec pm
	int isfnptr,variadic,nparams,retmode,nbytes,retsize,m,nregparams

	retmode:=p.mode
	if retmode=tvoid then retmode:=tsint fi

	case a.tag
	when j_ptr then
!CPL "DXCALL",STRMODE(A.MODE)
		m:=a.mode
		while ttbasetype[m]=tref do
			m:=tttarget[m]
		od

		pm:=ttparams[m]
		isfnptr:=1

	else
		pm:=a.def.paramlist
		isfnptr:=0

	esac

	variadic:=pm.flags=pm_variadic
	nparams:=pm.nparams

	pcl_gen(ksetargs)
	pcl_setnargs(nparams)
!	pcl_setnvariadics(nvariadics)

	pushffparams(b)

	if not isfnptr then
		pcl_genx((p.mode<>tvoid|kcallfn|kcallproc),nparams,genmemaddr_d(a.def))
	else
		evalexpr(a.a)
		pcl_genx((p.mode<>tvoid|kcallfnptr|kcallprocptr), nparams)
	fi

	setmode_u(p)
end

proc do_return(unit a)=
	if a then
		dx_expr(a)
	fi

	pcl_gen((currproc.mode<>tvoid|kretfn|kretproc))
	setmode(currproc.mode)
end

proc dx_prepostincrx(unit p,a,int opc)=
	evaladdr(a)
	pcl_gen(opc)
	setmode(p.mode)
end

proc dx_convert(unit p,a)=
!convert unit a to type t, using conversion opc (uwiden_c etc)
	int ssize,tsize, t, opc

	ssize:=ttsize[a.mode]
	t:=p.mode
	opc:=p.opcode
	tsize:=ttsize[t]

	evalexpr(a)

	case opc
	when soft_c then
		return
	when hard_c then

		if tsize<ssize then				!narrow
			pcl_gen(knarrow)

		elsif tsize>ssize then			!widen
			pcl_gen(kwiden)
		else
			return

		fi

	when swiden_c, uwiden_c then
		pcl_gen(kwiden)

	when sfloat_c,ufloat_c then
		pcl_gen(kfloat)

	when sfix_c,ufix_c then
		pcl_gen(kfix)

	when fnarrow_c then
		pcl_gen(kfnarrow)

	when fwiden_c then
		pcl_gen(kfwiden)

	when narrow_c,truncate_c then
		pcl_gen(knarrow)

	else
		gerror_s("Convert op not implem: %s",convnames[opc])
	esac
end

proc do_decl(symbol d)=
	PCL_GENCOMMENT("DO DECL")
!	ref opndrec ax
!	unit a,dest
!	[256]char str
!	int nbytes
!
!!CPL "DODECL",D.NAME
!
!	a:=d.code
!
!!case ttbasetype[d.mode]
!!when tunion,tstruct then
!!	if a.tag<>j_makelist then
!!CPL "HERE1"
!!!		gerror("Dynamic struct init")
!!	fi
!!esac
!
!	if a.tag<>j_makelist then
!		if ttbasetype[d.mode]=tarray and a.tag=j_const then	!probably string lit
!			goto copyl
!		fi
!		if gettypecat(a)='R' then
!			ax:=floadexpr(a,xr0)
!			pcl_gen(kfmov,genmem_d(d),ax)
!		elsif a.tag<>j_const then
!			case ttsize[a.mode]
!			when 1,2,4,8 then
!				ax:=loadexpr(a,r0)
!				pcl_gen(kmov,genmem_d(d),ax)
!			else
!				dest:=createname(d)
!				dest.mode:=d.mode
!				do_assignblock(dest,a,r0)
!			esac
!		else
!			pcl_gen(kmov,genmem_d(d),evalexpr(a))
!		fi
!		return
!	fi
!
!	copyl::
!
!	nbytes:=ttsize[d.mode]
!	pushstack(32)
!
!	pcl_gen(klea, genreg(r10,ptrsize),genmem_d(d))
!
!!sprintf(&.str,"`$%s.%s.%d",currproc.name,d.name,int32(d.blockno))
!	fprint @&.str,"`$#.#.#",currproc.name,d.name,d.blockno
!
!	pcl_gen(kmov, genreg(r11,ptrsize), genname(&.str))
!
!	pcl_gen(kmov,genreg(r12,ptrsize),genint(nbytes))
!
!	pcl_gen(kcall,genname("memcpy*"))
!
!	popstack(32)
!
end

proc do_switch(unit p,a,b)=
!need to create switch levels, as they can be nested; nested case labels
!belong to the top switch level
!	[maxswitchrange]int labeltable				!sw_length+1 labels
!	[maxcases]int valuetable					!sw_length+1 labels
!	[maxswitchrange]byte flags					!flags to check dupl values
!	int defaultlabel							!index of fwd default label
!	int breakswlabel							!index of fwd break label
!	int switchlabel								!index of fwd break label
!	int lower, upper							!ower/upper ranges of switch case values
!	int length,value,ncases
!	byte serialsw
!	int i,index
!!int sw_index
!	ref caserec pcase
!	ref opndrec ax,bx
!
!!store current set of global values for outer switch
!	ref[]int old_labeltable
!	ref[]int old_valuetable
!	int old_ncases,old_lower
!	byte old_defaultseen
!	int old_defaultlabel
!	int old_breaklabel
!
!	pcase:=p.nextcase
!	ncases:=length:=0
!
!	while pcase do
!		++ncases
!		if ncases>maxcases then
!			gerror("Too many cases on one switch")
!		fi
!		valuetable[ncases]:=value:=pcase.value
!
!		if ncases=1 then
!			lower:=upper:=value
!		else
!			lower:=min(lower,value)
!			upper:=max(upper,value)
!		fi
!		pcase:=pcase.nextcase
!	od
!
!	if p.nextcase then
!		length:=upper-lower+1
!	else
!		length:=0
!	fi 
!
!!allocate fwd labels
!	defaultlabel:=createfwdlabel()		!(when no default:, same as breakswlabel)
!	breakswlabel:=createfwdlabel()
!
!	if length>maxswitchrange then
!
!!NOTES: SERIAL switch needs a way of checking duplicate case values.
!!Better if not an n-squared search
!!Short length switches should also be done serially (length<=8)
!!Then a dupl check is simpler
!
!		serialsw:=1
!
!		ax:=loadexpr(a)
!		for i:=1 to ncases do
!			labeltable[i]:=createfwdlabel()
!			pcl_gen(kcmp,ax,genint(valuetable[i]))
!			pcl_gen_cond(kjmpcc,eq_cond,pcl_genlabel(labeltable[i]))
!		od
!		pcl_gen(kjmp,pcl_genlabel(defaultlabel))
!
!	elsif length=0 then
!!GERROR("L=0")
!		pcl_gen(kjmp,pcl_genlabel(defaultlabel))
!
!	else
!		serialsw:=0
!		memset(&flags,0,length)				!clear value flags
!
!!fill table with defaults first
!		for i:=1 to length do
!			labeltable[i]:=defaultlabel
!		od
!
!!now, do labels for each case value
!		for i:=1 to ncases do
!			value:=valuetable[i]
!			index:=value-lower+1			!index of value within label table
!			labeltable[index]:=createfwdlabel()
!
!			if flags[index] then
!				gerror_s("Dupl case value: %d",cast(value))
!			fi
!			flags[index]:=1
!		od
!
!!need a label for the switchtable itself
!		switchlabel:=createfwdlabel()
!
!		evalexpr(a)
!		pcl_genxy(kswitch, lower, upper,pcl_pcl_genlabel(switchlabel))
!		pcl_gen(kopnd,pcl_pcl_genlabel(defaultlab))
!
!		definefwdlabel(switchlabel)
!
!		for i:=1 to length do
!			pcl_gen(kdq,pcl_genlabel(labeltable[i]))
!		od
!	fi
!
!!generate code for the switch body
!!I need to make available essential tables, offsets etc necessary for j-case
!!to be mappable to a label
!!note: if already in an outer switch, then must save those earlier vars
!!save outer switch vars
!	old_labeltable:=sw_labeltable
!	old_valuetable:=sw_valuetable
!	old_lower:=sw_lower
!	old_ncases:=sw_ncases
!	old_defaultseen:=sw_defaultseen
!	old_defaultlabel:=sw_defaultlabel
!	old_breaklabel:=sw_breaklabel
!
!!set globals
!	sw_labeltable:=&labeltable
!	sw_valuetable:=&valuetable		!NEEDED ONLY FOR COMPLEX SWITCH
!	sw_lower:=lower
!
!	sw_ncases:=(serialsw|ncases|0)
!	sw_defaultseen:=0
!	sw_defaultlabel:=defaultlabel
!	sw_breaklabel:=breakswlabel
!
!	do_stmt(b)						!switch body
!
!!need to note whether a default label has been generated; if not, define
!!default label here
!	if not sw_defaultseen then
!		definefwdlabel(defaultlabel)
!	fi
!!define breakswlabel here
!	definefwdlabel(breakswlabel)
!
!!restore any values of outer switch statement
!	sw_labeltable:=old_labeltable
!	sw_valuetable:=old_valuetable
!	sw_lower:=old_lower
!	sw_ncases:=old_ncases
!	sw_defaultseen:=old_defaultseen
!	sw_defaultlabel:=old_defaultlabel
!	sw_breaklabel:=old_breaklabel
end

proc dx_ifx(unit a,b,c)=
	int lab1, lab2,isreal

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
	lab2:=createfwdlabel()

	isreal:=gettypecat(b)='R'

	genjumpcond(kjumpf,a,lab1)
	evalexpr(b)

	genjumpl(lab2)
	definefwdlabel(lab1)

	evalexpr(c)

	definefwdlabel(lab2)
end

proc dx_binto(int opc,unit a,b,int regx=0)=
	dx_expr(a)
	dx_expr(b)
	pcl_gen(opc)
	setmode_u(b)
end

proc do_exprlist(unit a)=
	while a do
		do_stmt(a)
		a:=a.nextunit
	od
end

proc dx_exprlist(unit a)=
	while a do
		dx_expr(a)
		a:=a.nextunit
	od
end

proc dx_andorl(unit p) =
!do short-circuit evaluation of a&&b or a||b
!return operand containing 1 or 0
	int lab1,lab2

	lab1:=createfwdlabel()			!dest label of main condition (to end of if, or start if else)

	genjumpcond(kjumpf,p,lab1)

	lab2:=createfwdlabel()			!label past else part
	pcl_gen(kpush, pcl_genint(1))

	genjumpl(lab2)
	definefwdlabel(lab1)
	pcl_gen(kpush, pcl_genint(0))

	definefwdlabel(lab2)
end

proc dx_unary(int opc, unit a)=
	evalexpr(a)
	pcl_gen(opc)
	setmode_u(a)
end

proc dx_name(unit p)=

	case p.def.nameid
	when procid then
		pcl_gen(kpush,genmemaddr_u(p))
		setmode(tullong)
	else
		pcl_gen(kpush,genmem_u(p))
		setmode(p.def.mode)
	esac
end

proc dx_dot(unit p,a,b)=
!return from here is always a memory address mode, whatever am is
!
	evalexpr(a)
PCL_GENCOMMENT("DOT/OFFSET")
!	ax:=applyoffset(ax,p.offset,ttsize[p.mode])
end

proc genjumpl(int lab) =
	pcl_gen(kjump, pcl_genlabel(lab))
end

function gettypecat(unit a)int=
!return 'I' 'U' or 'F' (or 'P' for pointers). Other types return 0
!rough type info for the code generator
	return stdtypecat[ttbasetype[a.mode]]
end
