=== MA 24 ===
=== cc.m 0 0 1/24 ===
project =
    module cc_cli

!Global Data and Tables

    module cc_decls
    module cc_tables

!Lexing and Parsing
    module cc_lex
    module cc_parse

!Generate PCL
    module cc_genpcl
    module cc_blockpcl
    module cc_libpcl

!General

    module cc_lib
    module cc_support

!Diagnostics
    module cc_show
!    module cc_showdummy

!IL Backend
	module pc_api
	module pc_decls

	module pc_diags
!	module pc_diags_dummy

	module pc_tables

	module mc_GenMCL
	module mc_AuxMCL
	module mc_LibMCL
	module mc_StackMCL

	module mc_Decls as md
!	module mc_WriteASM
	module mc_WriteGAS

end
=== cc_blockpcl.m 0 0 2/24 ===
[maxnestedloops]int continuestack		!labels for continue/break
[maxnestedloops]int breakstack
int loopindex							!current level of nested loop/switch blocks


const maxswitchrange=500
const maxcases=maxswitchrange
const maxswitchdepth=20

ref[]i32 sw_labeltable			!set from do-switch
ref[]i32 sw_valuetable
int sw_lower
int sw_ncases					!1..n for serial switch; 0 for simple
byte sw_defaultseen				!starts at 0, set to 1 when default: seen
int sw_defaultlabel
int sw_breaklabel

const maxparams=32

macro getpclcond(op) = eq_cc+(op-jeq)

global proc do_stmt(unit p) =
	int oldclineno
	unit a, b
	symbol d

	return unless p

	oldclineno:=clineno
	clineno:=p.lineno
	cfileno:=p.fileno
	mmpos:=cfileno<<24+clineno

	a:=p.a
	b:=p.b

	switch p.tag
	when jblock then
		while a do
			do_stmt(a)
			a:=a.nextunit
		od

	when jdecl then
		do_decl(p.def)
!
	when jcallfn then
		dx_call(p, a, b, 0)

	when jreturn then
		do_return(p, a)

	when jassign then
		do_assign(a, b, 0)

	when jif, jifx then
		do_if(a, b, p.c)

	when jfor then
		do_for(a, b)

	when jwhile then
		do_while(a, b)

	when jdowhile then
		do_dowhile(a, b)

	when jgoto then
		do_goto(p.def)

	when jlabelstmt then
		do_labeldef(p.def)
		do_stmt(a)

	when jcasestmt then

		do_casestmt(p, a)

	when jdefaultstmt then
		sw_defaultseen:=1
		pc_gen(klabel, genlabel(sw_defaultlabel))
		do_stmt(a)

	when jbreaksw then
		genjumpl(sw_breaklabel)

	when jbreak then
		genjumpl(breakstack[loopindex])

	when jcontinue then
		genjumpl(continuestack[loopindex])

	when jswitch then
		do_switch(p, a, b)

	when jaddto then
		dx_binto(a, b, kaddto)

	when jsubto then
		dx_binto(a, b, ksubto)

	when jmulto then
		dx_binto(a, b, kmulto)

	when jdivto then
		dx_binto(a, b, (isrealcc(a.mode)|kdivto|kidivto))

	when jremto then
		dx_binto(a, b, kiremto)

	when jiandto then
		dx_binto(a, b, kbitandto)

	when jiorto then
		dx_binto(a, b, kbitorto)

	when jixorto then
		dx_binto(a, b, kbitxorto)

	when jshlto then
		dx_binto(a, b, kshlto)

	when jshrto then
		dx_binto(a, b, kshrto)

	when jpreincr, jpostincr then
		do_preincr(a, kincrto)

	when jpredecr, jpostdecr then
		do_preincr(a, kdecrto)

	when jexprlist then
		while a do
			do_stmt(a)
			a:=a.nextunit
		od

	else
!!assume standalone expression (assign/call/addto/incr done above)
		dx_expr(p)
!		pc_gen(keval)
		pc_gen(kunload)
		setmode_u((a|a|p))

	end switch

end

proc dx_expr(unit p, int am=0) =
	int oldclineno, value, m
	unit a, b
	[256]char str
	symbol d

	return unless p

	oldclineno:=clineno
	clineno:=p.lineno
	cfileno:=p.fileno

	a:=p.a
	b:=p.b
	m:=p.mode

	switch p.tag
	when jconst then
		dx_const(p)

	when jname then
		dx_name(p, am)
!
	when jwidenmem then
		dx_expr(a, am)

	when jfuncname then
		pc_gen(kload, genmemaddr_d(p.def))
		setmode(tu64)

	when jassign then
		do_assign(a, b, 1)
!!
	when jandl, jorl then
		dx_andorl(p)		!use non-short circuit versions for now

	when jnotl then
		if a.tag=jnotl then
			dx_expr(a.a)
			if not isboolexpr(a.a) then
				pc_gen(ktoboolt)
				setmode(tu32)
				setmode2(a.a.mode)
			fi
		else
			dx_expr(a)
			if not isboolexpr(a) then
				pc_gen(ktoboolf)
			else
				pc_gen(knot)
			fi
			setmode_u(a)
		fi

	when jistruel then
		dx_expr(a)
		unless isboolexpr(a) then
			pc_gen(ktoboolt)
			setmode(tu32)
			setmode2(a.mode)
		end

	when jexprlist then
		while a, a:=b do
			b:=a.nextunit

			if b and a.tag in [jassign, jconvert, jifx] then
				do_stmt(a)
			else
				dx_expr(a)
				if b and (a.mode<>tvoid or a.tag=jconvert) then
					pc_gen(keval)
				fi
			fi
		od

	when jcallfn then
		dx_call(p, a, b, 1)

	when jifx then
		dx_ifx(p, a, b, p.c)

	when jeq, jne, jlt, jle, jge, jgt then
		dx_eq(p, a, b)

	when jadd then
		if ttisref[a.mode] and ttsize[b.mode]<=4 then
			b.mode:=tu64
		fi
		dx_bin(a, b, kadd)
!
	when jsub then
		dx_bin(a, b, ksub)
!
	when jmul then
		dx_bin(a, b, kmul)

	when jdiv then
		dx_bin(a, b, (isrealcc(a.mode)|kdiv|kidiv))

	when jrem then
		dx_bin(a, b, kirem)

	when jiand then
		dx_bin(a, b, kbitand)

	when jior then
		dx_bin(a, b, kbitor)

	when jixor then
		dx_bin(a, b, kbitxor)

	when jshl then
		dx_bin(a, b, kshl)

	when jshr then
		dx_bin(a, b, kshr)

	when jptr then
		dx_ptr(p, a, am)

	when  jaddptr then
		dx_addptr(p, a, b, kaddpx, am)
!
	when  jsubptr then
		dx_addptr(p, a, b, ksubpx, am)
!
	when jconvert then
		if p.convmode=tvoid then
			dx_expr(a)
		else
			dx_convert(p, a, p.convmode, p.opcode)
		fi

	when jscale then
		dx_scale(p, a, b)

	when jneg then
		dx_expr(a)
		pc_gen(kneg)
		setmode_u(a)

	when jinot then
		dx_expr(a)
		pc_gen(kbitnot)
		setmode_u(a)

	when jpreincr, jpredecr then
		dx_preincrx(p, a)

	when jpostincr, jpostdecr then
		dx_postincrx(p, a)

	when jaddto then
		dx_binto(a, b, kaddto, 1)

	when jsubto then
		dx_binto(a, b, ksubto, 1)

	when jmulto then
		dx_binto(a, b, kmulto, 1)

	when jdivto then
		dx_binto(a, b, (isrealcc(a.mode)|kdivto|kidivto), 1)

	when jremto then
		dx_binto(a, b, kiremto, 1)

	when jiandto then
		dx_binto(a, b, kbitandto, 1)

	when jiorto then
		dx_binto(a, b, kbitorto, 1)

	when jixorto then
		dx_binto(a, b, kbitxorto, 1)

	when jshlto then
		dx_binto(a, b, kshlto, 1)

	when jshrto then
		dx_binto(a, b, kshrto, 1)

	when jaddrof then
		dx_addrof(p, a, am)

	when jdot then
		dx_dot(p, a, b, am)

	when jsetjmp then
		dx_expr(a)
		pc_gen(ksetjmp)

	when jlongjmp then
		dx_expr(a)
		dx_expr(b)
		pc_gen(klongjmp)

	else
		gerror_s("DX-EXPR: can't do tag: #", jtagnames[p.tag])
	end switch

	clineno:=oldclineno
end

proc dx_const(unit p)=
	int t:=ttbasetype[p.mode]


	if t in tfirstint..tlastint then
		pc_gen(kload, genint(p.value))

	elsecase t
	when tr32 then
		pc_gen(kload, genreal(p.xvalue, tpr32))

	when tr64 then
		pc_gen(kload, genreal(p.xvalue, tpr64))

	elsif t>=tfirstreal and t<=tlastreal then
		pc_gen(kload, genreal(p.xvalue, tpr64))

	elsif t=tref then
		if p.isstrconst then
			pc_gen(kload, genstring(p.svalue, p.slength))
		elsif p.iswstrconst then
			GERROR("CONST/WSTRING")
		else
			pc_gen(kload, genint(p.value))
		fi
	else
		gerror("const?")
	fi
	setmode(p.mode)
end

proc dx_name(unit p, int am)=
	symbol d:=p.def

	case d.nameid
	when staticid, frameid, paramid then
		if am then
			pc_gen(kload, genmemaddr_d(d))
			setmode(tu64)
		else
			pc_gen(kload, genmem_d(d))
			widen(p)
		fi
	else
		gerror("dxname")
	esac
end

proc dx_bin(unit a, b, int opc)=
	dx_expr(a)
	dx_expr(b)

	pc_gen(opc)
	setmode(a.mode)
end

proc dx_binto(unit a, b, int opc, res=0)=
!res=1 means value must be retained
	dx_expr(b)
	dx_expr(a, 1)
IF NOT RES and opc not in [kidivto, kiremto] THEN
	do_setinplace()
FI

	if res then
		pc_gen(kdupl)
		pc_genxy(kswapstk, 2, 3)
	fi
	pc_gen(opc)
	setmode(getmemmode(a))

	if res then
		pc_gen(kiload)				!don't need genix, as previous op is only addto etc
		setmode(getmemmode(a))
	fi
end

proc do_assign(unit a, b, int res)=

	dx_expr(b)

	if res then
!		pc_gen(kdupl)
		pc_gen(kdouble)
	fi

	case a.tag
	when jname then
		pc_gen(kstore, genmem_d(a.def))
		setmode(getmemmode(a))

	when jptr then
		dx_expr(a, 1)
		pc_genix(kistore)
		setmode(getmemmode(a))

	when jdot then
		dx_expr(a.a, 1)
		pc_gen(kload, genint(a.offset))
		setmode(tu64)
		pc_genix(kaddpx)
		pc_setscaleoff(1)
		setmode(getmemmode(a))

		pc_genix(kistore)
		pc_setscaleoff(1)
		setmode(getmemmode(a))

	else
		GERROR_S("DOASSIGN not ready: #", jtagnames[a.tag])
	esac
end

proc dx_ptr(unit p, a, int am)=
	dx_expr(a)
	if am=0 then				!for &, exit with pointer value
		pc_genix(kiload)
		widen(p)
	fi
end

proc dx_addptr(unit p, a, b, int opc, am)=
	dx_expr(a)
	dx_expr(b)
	pc_genix(opc)
	pc_setscaleoff(p.ptrscale)

	setmode(a.mode)
end

proc dx_addrof(unit p, a, int am)=
	dx_expr(a, 1)
end

proc dx_convert(unit p, a, int t, opc)=
!convert unit a to type t, using conversion opc (uwiden_c etc)
	int s, ssize, tsize

	s:=a.mode

	ssize:=ttsize[s]
	tsize:=ttsize[t]

	dx_expr(a)

	case opc
	when soft_c then
		return
	when hard_c then
!hard is an explicit cast for which no built-in code such as swiden_c has
!been detected. So just do a kind of type-punning, but ensure the sizes
!are correct

!		if stdcat[ttbasetype[s]]=realcat then gerror("Bad cast") fi
		if ttbasetype[s] in [tr32, tr64] then gerror("Bad cast") fi

		if tsize>ssize then			!widen
			pc_gen(kwiden)
		elsif tsize<ssize then
!			recase narrow_c
			goto dotruncate
			return
		fi

	when swiden_c, uwiden_c then
		if ssize=tsize then return fi
		pc_gen(kwiden)

	when sfloat_c, ufloat_c then
		pc_gen(kfloat)

	when sfix_c, ufix_c then
		pc_gen(kfix)

	when fwiden_c then
		pc_gen(kfwiden)

	when fnarrow_c then
		pc_gen(kfnarrow)

	when narrow_c, truncate_c then
dotruncate:
		pc_gen(ktruncate)

		setmode(ti32)
		setmode2(t)
		return

	else
		gerror_s("Convert op not implem: #", convnames[opc])
	esac

	setmode(t)
	setmode2(s)
end

proc do_if(unit a, b, c)=
	int lab1, lab2

	lab1:=createfwdlabel()

	genjumpcond(kjumpf, a, lab1)

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

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r
	int lab2

	q:=p.a
	r:=p.b

	case p.tag
	when jandl then
		case opc
		when kjumpf then
			genjumpcond(kjumpf, q, lab)
			genjumpcond(kjumpf, r, lab)
		when kjumpt then
			lab2:=createfwdlabel()
			genjumpcond(kjumpf, q, lab2)
			genjumpcond(kjumpt, r, lab)
			definefwdlabel(lab2)
		esac

	when jorl then
		case opc
		when kjumpf then
			lab2:=createfwdlabel()
			genjumpcond(kjumpt, q, lab2)
			genjumpcond(kjumpf, r, lab)
			definefwdlabel(lab2)
		when kjumpt then
			genjumpcond(kjumpt, q, lab)
			genjumpcond(kjumpt, r, lab)
		esac

	when jnotl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt, q, lab)
		when kjumpt then
			genjumpcond(kjumpf, q, lab)
		esac

	when jistruel then
		genjumpcond(opc, q, lab)

	when jeq, jne, jlt, jle, jge, jgt then

		gcomparejump(opc, p, q, r, lab)

	when jexprlist then
		while q and (r:=q.nextunit) do
			do_stmt(q)
			q:=r
		od

		genjumpcond(opc, q, lab)
	else			!other expression
		dx_expr(p)
		pc_gen(opc, genlabel(lab))
		setmode_u(p)
	end
end

proc gcomparejump(int jumpopc, unit p, lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int cond

	cond:=getpclcond(p.tag)			!jeq => keq etc
	if jumpopc=kjumpf then			!need to reverse condition
!		cond:=reversecond(cond)		!eqop => neop, etc
		cond:=revcc[cond]			!eqop => neop, etc
	fi

	dx_expr(lhs)
	dx_expr(rhs)

	pc_gen(kjumpcc, genlabel(lab))
	pccurr.condcode:=cond
	setmode_u(lhs)

end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pc_gen(kjump, genlabel(lab))
end

proc do_while (unit pcond, pbody) =
	int lab_b, lab_c, lab_d

	if pcond.tag=jconst and pcond.value then
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

	genjumpcond(kjumpt, pcond, lab_b)
!	setmode_u(pcond)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_while1 (unit pbody) =
	int lab_b, lab_c, lab_d

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

proc do_dowhile (unit pbody, pcond) =
	int lab_b, lab_c, lab_d

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	unless iscondfalse(pcond) then
		genjumpcond(kjumpt, pcond, lab_b)
	end


	definefwdlabel(lab_d)
	--loopindex
end

proc stacklooplabels(int a, b)=
	!don't check for loop depth as that has been done during parsing
	continuestack[++loopindex]:=a
	breakstack[loopindex]:=b
end

proc do_return(unit p, a)=
	psymbol e

	if a then
		dx_expr(a)
		pc_gen(kjumpret, genlabel(retindex))
		setmode_u(a)
	else
		genjumpl(retindex)
	fi
end

proc dx_call(unit p, a, b, int res)=
	ref paramrec pm
	int isfnptr, variadic, nparams, retmode, nbytes, retsize, m, nvariadics
	int nfixedparams, isfn, blockret
	[maxparams]unit paramlist
	[maxparams]byte paramconst			!1 when 'const' (up to nfixedparams only)
	[maxparams]i8 argattr
	int iparams, fparams
	symbol dblock, dtemp
	unit q

	retmode:=p.mode
	if retmode=tvoid then retmode:=ti32 fi

	isfn:=0

	case a.tag
	when jptr then
		m:=a.mode
		while ttbasetype[m]=tref do
			m:=tttarget[m]
		od

		isfn:=tttarget[m]<>tvoid
		pm:=ttparams[m]
		isfnptr:=1

	else
		pm:=a.def.paramlist
		isfnptr:=0
		isfn:=a.def.mode<>tvoid

	esac

	variadic:=pm.flags=pm_variadic
	nfixedparams:=pm.nparams
	nparams:=nvariadics:=0

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparams then gerror("maxparams") fi
		paramlist[++nparams]:=q
		paramconst[nparams]:=0

		if variadic and nparams>nfixedparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		fi
		if nparams<=nfixedparams then
			paramconst[nparams]:=ttconst[pm.mode]
			pm:=pm.nextparam
		fi
	od

	pc_gen(ksetcall)

	setmode_u(p)
	pccurr.nargs:=nparams

	iparams:=fparams:=0

	for i to nparams do
		q:=paramlist[i]
		if q.mode in [tr32, tr64] then
			argattr[i]:=-(++fparams)
		else
			argattr[i]:=++iparams
		fi
	od

	for i:=nparams downto 1 do			!downto 
		q:=paramlist[i]
		dx_expr(q)

		if nvariadics and i>=nvariadics and pccurr.mode=tpr32 then
			pc_gen(kfwiden)
			pccurr.mode:=tpr64
			pccurr.mode2:=tpr32
		fi

		pc_gen(ksetarg)
		setmode_u(q)
		pccurr.x:=i
		pccurr.y:=argattr[i]
	od

	if not isfnptr then
		pc_gen((isfn|kcallf|kcallp), genmemaddr_d(a.def))
	else
		dx_expr(a.a)
		pc_gen((isfn|kicallf|kicallp))
	fi

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
		setmode(getmemmode(p))
		if not res then
			pc_gen(kunload)
			setmode(getmemmode(p))
		else
			widen(p)
		fi
	fi

end

proc do_decl(symbol d)=
	unit a

	a:=d.code
	d.used:=1
	if d.pdef then d.pdef.used:=1 fi

	if a.tag<>jmakelist then
		if ttbasetype[d.mode]=tarray and a.tag=jconst then	!probably string lit
			goto copyl
		fi
		dx_expr(a)
		pc_gen(kstore, genmem_d(d))
		setmode(a.mode)
		return
	fi

copyl:
	pc_gen(kload, genmem(d.pdata))

	setmode(d.mode)
	pc_gen(kstore, genmem_d(d))
	setmode(d.mode)
end

proc do_for (unit pinit, pbody) =
	unit pcond, pincr
	int lab_b, lab_c, lab_d, lab_cond

	pcond:=pinit.nextunit
	pincr:=pcond.nextunit

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_cond:=createfwdlabel()

	if pinit.tag<>jnull then
		do_stmt(pinit)
	fi

	genjumpl(lab_cond)		!direct to condition code which is at the end

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	do_stmt(pincr)
	definefwdlabel(lab_cond)

	if pcond.tag<>jnull then
		genjumpcond(kjumpt, pcond, lab_b)
	else
		genjumpl(lab_b)
	fi
	definefwdlabel(lab_d)
	--loopindex
end

proc do_preincr(unit a, int incrop)=
	dx_expr(a, 1)
	do_setinplace()
	pc_gen(incrop)
!	setmode_u(a)
	setmode(getmemmode(a))

	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pc_setincr(1)

	if ttisref[m] then
		pc_setincr(ttsize[tttarget[m]])
	fi
end

proc dx_preincrx(unit p, a)=
	dx_expr(a, 1)
	do_setinplace()

	pc_gen((p.tag=jpreincr|kincrload|kdecrload))
	setincrstep(a.mode)

	WIDEN(A)

end

proc dx_postincrx(unit p, a)=
	dx_expr(a, 1)
	do_setinplace()

	pc_gen((p.tag=jpostincr|kloadincr|kloaddecr))
	setincrstep(a.mode)
	WIDEN(A)
end

proc dx_dot(unit p, a, b, int am)=
	dx_expr(a, 1)
	pc_gen(kload, genint(p.offset))
	setmode(tu64)

	if am=0 then
		pc_genix(kaddpx)
		setmode(getmemmode(p))
		pc_setscaleoff(1)

		pc_genix(kiload)
		widen(p)
	else
		pc_genix(kaddpx)
		setmode(getmemmode(p))
		pc_setscaleoff(1)

	fi

end

proc dx_eq(unit p, a, b)=
!apply =, <= etc between a and b, and get a logical result 1 or 0

	dx_expr(a)
	dx_expr(b)

	pc_gen(ksetcc)
	pccurr.condcode:=getpclcond(p.tag)
	setmode_u(a)
end

proc do_labeldef(symbol d)=
	if d.index<=0 then			!not already dealt with via goto
		d.index:=++mlabelno
	fi

	gencomment(d.name)
	pc_gen(klabel, genlabel(d.index))
end

proc do_goto(symbol d)=
	if d.index=0 then
		gerror_s("Label not defined: #", d.name)
	elsif d.index<0 then
		d.index:=++mlabelno	
	fi
	pc_gen(kjump, genlabel(d.index))
end

proc dx_ifx(unit p, a, b, c)=
	int lab1, lab2, ismult:=p.mode<>tvoid

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
	lab2:=createfwdlabel()

	if ismult then pc_gen(kstartmx) fi
	genjumpcond(kjumpf, a, lab1)

	dx_expr(b)
	if ismult then pc_gen(kresetmx); setmode_u(p) fi

	genjumpl(lab2)
	definefwdlabel(lab1)

	dx_expr(c)
	if ismult then pc_gen(kendmx); setmode_u(p) fi

	definefwdlabel(lab2)
end

proc do_casestmt(unit p, a)=
	int value

	if sw_ncases=0 then
		pc_gen(klabel, genlabel(sw_labeltable[p.value-sw_lower+1]))
	else
		value:=p.value
		for i:=1 to sw_ncases do
			if sw_valuetable[i]=value then
				pc_gen(klabel, genlabel(sw_labeltable[i]))
				exit
			fi
		else
			gerror("case: serial switch not found")
		od
	fi
	do_stmt(a)
end

proc do_switch(unit p, a, b)=
!need to create switch levels, as they can be nested; nested case labels
!belong to the top switch level
	[maxswitchrange]i32 labeltable				!sw_length+1 labels
	[maxcases]i32 valuetable					!sw_length+1 labels
	[maxswitchrange]byte flags					!flags to check dupl values
	int defaultlabel							!index of fwd default label
	int breakswlabel							!index of fwd break label
	int switchlabel								!index of fwd break label
	int lower, upper							!ower/upper ranges of switch case values
	int length, value, ncases
	byte serialsw
	int i, index
!int sw_index
	ref caserec pcase

!store current set of global values for outer switch
	ref[]i32 old_labeltable
	ref[]i32 old_valuetable
	int old_ncases, old_lower
	byte old_defaultseen
	int old_defaultlabel
	int old_breaklabel

	pcase:=p.nextcase
	ncases:=length:=0

	while pcase do
		++ncases
		if ncases>maxcases then
			gerror("Too many cases on one switch")
		fi
		valuetable[ncases]:=value:=pcase.value

		if ncases=1 then
			lower:=upper:=value
		else
			lower:=min(lower, value)
			upper:=max(upper, value)
		fi
		pcase:=pcase.nextcase
	od

	if p.nextcase then
		length:=upper-lower+1
	else
		length:=0
	fi 

!allocate fwd labels
	defaultlabel:=createfwdlabel()		!(when no default:, same as breakswlabel)
	breakswlabel:=createfwdlabel()

!	if length>maxswitchrange then
	if length>maxswitchrange OR NCASES<=8 then

!NOTES: SERIAL switch needs a way of checking duplicate case values.
!Better if not an n-squared search
!Short length switches should also be done serially (length<=8)
!Then a dupl check is simpler

		serialsw:=1

!		ax:=loadexpr(a)
		dx_expr(a)

		for i:=1 to ncases do
			labeltable[i]:=createfwdlabel()
			pc_gen(kload, genint(valuetable[i]))
			setmode(ti32)
			pc_gen(kjumpcc, genlabel(labeltable[i]))
			setmode(ti32)
			pccurr.condcode:=eq_cc
			if i<ncases then
				pccurr.popone:=1
			fi

		od

		genjumpl(defaultlabel)

	elsif length=0 then
		genjumpl(defaultlabel)

	else
		serialsw:=0
		memset(&flags, 0, length)				!clear value flags

!fill table with defaults first
		for i:=1 to length do
			labeltable[i]:=defaultlabel
		od

!now, do labels for each case value
		for i:=1 to ncases do
			value:=valuetable[i]
			index:=value-lower+1			!index of value within label table
			labeltable[index]:=createfwdlabel()

			if flags[index] then
				gerror_s("Dupl case value: #", strint(value))
			fi
			flags[index]:=1
		od

!need a label for the switchtable itself
		switchlabel:=createfwdlabel()

		dx_expr(a)
		pc_gen(kswitch, genlabel(switchlabel))
		setmode(ti32)
		pc_setxy(lower, lower+length-1)
		pc_gen(kopnd, genlabel(defaultlabel))
		setmode(ti32)
		definefwdlabel(switchlabel)

		for i:=1 to length do
			pc_gen(kswlabel, genlabel(labeltable[i]))
		od
		pc_gen(kendsw)
	fi

!generate code for the switch body
!I need to make available essential tables, offsets etc necessary for j-case
!to be mappable to a label
!note: if already in an outer switch, then must save those earlier vars
!save outer switch vars
	old_labeltable:=sw_labeltable
	old_valuetable:=sw_valuetable
	old_lower:=sw_lower
	old_ncases:=sw_ncases
	old_defaultseen:=sw_defaultseen
	old_defaultlabel:=sw_defaultlabel
	old_breaklabel:=sw_breaklabel

!set globals
	sw_labeltable:=&labeltable
	sw_valuetable:=&valuetable		!NEEDED ONLY FOR COMPLEX SWITCH
	sw_lower:=lower

	sw_ncases:=(serialsw|ncases|0)
	sw_defaultseen:=0
	sw_defaultlabel:=defaultlabel
	sw_breaklabel:=breakswlabel

	do_stmt(b)						!switch body

!need to note whether a default label has been generated; if not, define
!default label here
	if not sw_defaultseen then
		definefwdlabel(defaultlabel)
	fi
!define breakswlabel here
	definefwdlabel(breakswlabel)

!restore any values of outer switch statement
	sw_labeltable:=old_labeltable
	sw_valuetable:=old_valuetable
	sw_lower:=old_lower
	sw_ncases:=old_ncases
	sw_defaultseen:=old_defaultseen
	sw_defaultlabel:=old_defaultlabel
	sw_breaklabel:=old_breaklabel
end

proc dx_andorl(unit p)=
!do short-circuit evaluation of a&&b or a||b
!return operand containing 1 or 0
	int lab1, lab2

	lab1:=createfwdlabel()			!dest label of main condition (to end of if, or start if else)

	pc_gen(kstartmx)
	genjumpcond(kjumpf, p, lab1)

	lab2:=createfwdlabel()			!label past else part
	pc_gen(kload, genint(1))
	setmode(ti32)
	pc_gen(kresetmx)
	setmode_u(p.a)
	genjumpl(lab2)

	definefwdlabel(lab1)
	pc_gen(kload, genint(0))
	setmode(ti32)
	pc_gen(kendmx)
	setmode_u(p.a)

	definefwdlabel(lab2)
end

proc dx_scale(unit p, a, b)=
	int opc, scale:=p.scale, n

	dx_expr(a)
	if p.scale>=0 then
		pc_gen(kload, genint(p.scale))
		setmode(ti64)
		pc_gen(kmul)
	else
		pc_gen(kload, genint(-p.scale))
		setmode(ti64)
		pc_gen(kidiv)
	fi
	setmode_u(a)
end

proc widen(unit p) =

	if p.memmode=tvoid then
		setmode(p.mode)
		return
	fi

	int mode:=getmemmode(p)

	setmode(mode)

	if ttsize[mode]<4 and pccurr.opcode in [kload, kiload, kiloadx, 
		kincrload, kdecrload, kloadincr, kloaddecr, kcallf] then
		pc_gen(kwiden)
		setmode((mode in [ti8, ti16]|ti32|tu32))
		setmode2(mode)
	fi
end

proc do_setinplace=
	if pccurr.opcode=kload and pccurr.opndtype=memaddr_opnd then
		pccurr.inplace:=1
	fi
end

func isboolexpr(unit p)int=

	case p.tag
	when jnotl, jandl, jorl, jeq, jne, jlt, jle, jge, jgt then
		1
	else
		0
	esac
end
=== cc_cli.m 0 0 3/24 ===
global enumdata =
	load_pass,
	parse_pass,
	type_pass,
	pcl_pass,
	asm_pass,
end

global byte cc_pass			!one of the above, default is link_pass
global byte debugmode

ichar outfile			!base file
ichar outext="exe"

global byte highmem=1					!0/1/2 = normal/rip only/himem

global byte fshowst
global byte fshowast
global byte fshowpcl
global byte fshowpst
global byte fshowmcl
global byte fshortnames

global const logfile="mcc.log"

enumdata []ichar optionnames, []ref byte optvars, []byte optvalues =
	(parse_sw,		"parse",		&cc_pass,		parse_pass),
	(type_sw,		"type",			&cc_pass,		type_pass),
	(pcl_sw,		"p",			&cc_pass,		pcl_pass),
	(asm_sw,		"s",			&cc_pass,		asm_pass),

	(noconst_sw,	"noconst",		&fnoconst,		1),

	(inclpath_sw,	"i",			nil,			1),

	(showst_sw,		"st",			&fshowst,		1),
	(showast_sw,	"ast",			&fshowast,		1),
	(showpcl_sw,	"pcl",			&fshowpcl,		1),
	(showpst_sw,	"pst",			&fshowpst,		1),
	(showmcl_sw,	"mcl",			&fshowmcl,		1),

	(out_sw,		"o",			nil,			0),

	(norip_sw,		"norip",		&highmem,		0),
	(himem_sw,		"himem",		&highmem,		2),

	(options_sw,	"options",		nil,			0),
	(showil_sw	,	"il",			&fshowil,		1),
	(long64_sw	,	"long64",		&flong64,		1),
end


proc main=
	ichar file


CPL =PCLNAMES.LEN

	initdata()
!
	getinputoptions()

	initsearchdirs()

	initlogfile()

	fprintln "Compiling # to #", inputfile, outfile

	do_loadmodule()

	do_parsemodule()

	do_genpcl()

	do_asm()

!	closelogfile()

	stop 0
end

proc do_loadmodule=
!Used for main module. Will always be first module loaded, module list
!will be empty.
!Load file as string
!extract modulename
!call compilemodile(modulename, filename, source)
	ichar modulename
	[300]char path
	int status
	int i,flag

!set up special module to represent the whole program
	sourcefilenames[0]:="<dummy file>"
	sourcefilepaths[0]:="<dummy path>"
	sourcefiletext[0]:="<sourcefile0>"
	sourcefilesizes[0]:=strlen(sourcefiletext[0])

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)

	if not checkfile(inputfile) then					!don't use searchdirs
		loaderror("Can't load main module: #",inputfile)
	fi

	mainfileno:=loadsourcefile(inputfile,inputfile)
		
	modulename:=extractbasefile(inputfile)
	stmodule:=createdupldef(stprogram, addnamestr(modulename), moduleid)

	strcpy(path,extractpath(inputfile))
	if path[1] then
		++nsearchdirs
		for i:=nsearchdirs downto 2 do
			searchdirs[i]:=searchdirs[i-1]
		od
		searchdirs[1]:=ref char(pcm_copyheapstring(path))
	fi

end

proc do_parsemodule=
	int tt
	parsemodule()
end

proc do_genpcl=
	ref strbuffer d

	return unless cc_pass >= pcl_pass

	codegen_pcl()

	if cc_pass=pcl_pass then			!need discrete file
		d:=writeallpcl()

		writefile(changeext(outfile,"pcl"), d.strptr, d.length)
	end
end

!proc do_genmcl=
!	return unless cc_pass >= mcl_pass
!
!	if cc_pass=mcl_pass then			!need discrete file
!		pcl_writeasm(outfile)
!	fi
!end
!
proc do_asm=
	ref strbuffer asmstr
	filehandle f

	return unless cc_pass >= asm_pass

	genmcl()

	asmstr:=getassemstr()

	f:=fopen(outfile, "w")
	gs_println(asmstr, f)
	fclose(f)

	gs_free(asmstr)
end

proc initlogfile=
	if debugmode>=2 then
		remove(logfile)
		logdev:=cast(fopen(logfile,"w"))
	fi
end

proc initdata=
	pcm_init()
	lexsetup()
	inittypetables()

	highmem:=2

end

global func cgetsourceinfo(int pos, ichar &filename, &sourceline)int=
	filename:=sourcefilenames[pos.[24..32]]
	sourceline:="<line>"
	return pos.[0..23]
end

proc initsearchdirs=
	[300]char str1,str2
	int i

	searchdirs[++nsearchdirs]:=""

	searchdirs[++nsearchdirs]:="c:/dx/headers/"

	searchdirs[++nsearchdirs]:=pcm_copyheapstring(extractpath(os_gethostname()))

	for i to nincludepaths when includepaths[i]^ do
		searchdirs[++nsearchdirs]:=includepaths[i]
	od


end

proc getinputoptions=
	const slash='-'
	int i,j,k
	int paramno,pmtype,sw,ncolons
	ichar name,value,ext

	paramno:=1
	ncolons:=0

	do
		pmtype:=nextcmdparamnew(paramno,name,value,".c")
		case pmtype
		when pm_option then
			convlcstring(name)
			for sw to optionnames.len do
				if eqstring(name,optionnames[sw]) then
					do_option(sw,value)
					exit
				fi
		else
			println "Unknown option:",name
			stop 1
		od
		when pm_sourcefile then
			if inputfile then
				loaderror("One input file only")
			fi
			inputfile:=pcm_copyheapstring(name)

		when 0 then
			exit
		esac
	od

	outext:="asm"
	case cc_pass
	when 0 then
		cc_pass:=asm_pass
	when pcl_pass then
		outext:="pcl"
	end

	highmem:=2

	if inputfile=nil then
		println "MCC C Compiler 7.x"
		println "Usage:"
		println "    ",,cmdparams[0],"   prog[.c]          Compile to prog.asm"
		stop 1
	fi

	if outfile=nil then
		outfile:=pcm_copyheapstring(changeext(inputfile, outext))
	fi

	pcl_setflags(highmem:highmem, shortnames:fshortnames)
end

proc do_option(int sw, ichar value)=
	[300]char str
	int length
	ref byte p

	p:=optvars[sw]
	if p then
		p^:=optvalues[sw]
!
		if sw in parse_sw..pcl_sw then
			debugmode ior:=1
		fi

		if sw in showst_sw..showmcl_sw then
			debugmode ior:=2
		fi
		return
	fi

	case sw
	when inclpath_sw then
		if nincludepaths>maxincludepaths then
			loaderror("Too many include paths","")
		fi
		length:=strlen(value)
		case (value+length-1)^
		when '\\', '/' then
		else
			strcpy(str,value)
			strcat(str,"/")
			value:=str
		esac

		includepaths[++nincludepaths]:=pcm_copyheapstring(value)

	when out_sw then
		outfile:=pcm_copyheapstring(addext(value,outext))

	when options_sw then
		for i to optionnames.len do
			println "   ",optionnames[i]
		od

	esac
end

=== cc_decls.m 0 0 4/24 ===
import clib
global type unit = ref unitrec
global type symbol = ref strec

global const maxmodule=200
!global const maxlibfile=200
global const maxsourcefile=200

global macro pr(a,b) = a<<16+b

global record tokenrec = 		!should be 32-byte record
	union
		i64 value				!64-bit int
		real xvalue				!64-bit float
		u64 uvalue			!64-bit word
		ref char svalue			!pointer to string or charconst (not terminated)
		ref strec symptr		!pointer to symbol table entry for name
	end
	ref tokenrec nexttoken

	u32	lineno
	u8	fileno
	u8	symbol
	u8	subcode
	u8	flags

	i32 length					!length of name/string/char
	union
		i32 numberoffset			!offset of numeric token within file[fileno]
		i16 paramno				!for macro params
		i16 pasteno
	end
end

global record mparamrec =
	ref strec def
	ref mparamrec nextmparam
end

global record caserec =
	ref caserec nextcase
	int value
end

!param lists always have at least one 'parameter':
! ()				nparams=0	flags=pm_notset		mode=tnone
! (void)			nparams=0	flags=pm_empty		mode=tnone
! (...)				nparams=0	flags=pm_variadic	mode=tnone
! (t,u,v)			nparams=3	flags=0				mode=t (on 1st param)
! (t,u,v,...)		nparams=3	flags=pm_variadic	mode=t (on 1st param)

global record paramrec =
	ref strec def			!named param: st entry, otherwise nil
	ref paramrec nextparam
	i32 mode				!tnone when there are no normal params
	i16 nparams			!used on first param only
	i16 flags				!used on first param only
end

!mask bits for .flags of tokenrec; these can be combined if both are true
global const tk_macromask = 1		!is a name that is a macro def
global const tk_parammask = 2		!is a name that is a param def
global const tk_macrolit  = 4		!is an processed token that is a macro name
global const tk_pasted    = 8

global record fieldrec = 			!linear list of fields/anon fields in a struct
	ref strec def
	ref strec gendef				!generic version of def
	ref fieldrec nextfield			!list may be created in reverse order
	int offset						!offset from start of struct
end

global record strec =
	ichar name
	ref strec owner
	ref strec deflist
	ref strec deflistx
	ref strec nextdef
	ref strec nextdupl
	ref strec prevdupl
	psymbol pdef

	union
		ref paramrec nextparam
		ref unitrec callchain
		ref strec nextmacro
		ref fieldrec nextfield
	end
	union
		ref unitrec code
		ref tokenrec tokenlist
	end
	union
		ref paramrec paramlist
		ref mparamrec mparamlist
		ichar macrovalue
	end
	union
		i32 index					!enum/label index
		i32 offset
		i32 labelno				!normally used as .index
		byte oldsymbol				!for #define/#undef on keyword
	end

	u32 lineno
	union
		struct
			u16 blockno
			u16 namespace				!experimental: set to namespaces[.nameid]
		end
		u32 nsblock						!combined block no and namespace
	end

	i16 subcode
	u16 mode

	u16 nrefs
	byte namelen
	byte symbol

!	byte flags:(addrof:1, varparams:1, flmacro:1, used:1, ismain:1)
	byte flags:(addrof:1, varparams:1, flmacro:1, ismain:1, exported:1)
	byte nameid
	byte scope					!linkage type
	byte nparams				!no. formal params for procid/dllprocid

	byte align					!1, 2, 4, 8; max align for struct/union
	byte fileno

	psymbol pdata				!symbol such as $mod.func.name.1 for makelist data
	byte used
	[5]byte spare
end

global record fwdrec =
	ref fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

global record unitrec =
	union
		ref strec def
		i64 value
		u64 uvalue
		real xvalue
		ichar svalue
		ref u16 wsvalue
		ref strec labeldef
		ref caserec nextcase
		i32 ptrscale			!use for derefoffset/addoffset
		i32 offset				!for jdot
	end
	ref unitrec nextunit
	ref unitrec a	!single items, or linked lists
	ref unitrec b
	ref unitrec c

	i32 tag			!kcode tag number
	u32 lineno			!source lineno associated with item; fileno is in top byte

	union
		i32 opcode			!for conversion
		i32 index				!label index
		u32 uindex			!case index
		i32 slength			!for const/string
		i32 wslength
		i32 alength			!for deref units, length of any array before conversion to ptr
		i32 scale			!for scale unit (negative means divide)
		i32 aparams			!callfn/proc: no. actual params
		i32 count			!makelist, no. items
	end

	i32 mode
	i16 memmode				!none, or memmode for name, ptr etc
	i16 convmode				!conversion dest type
	byte fileno
	byte isstrconst			!for string consts: initialised with "..."
	byte iswstrconst
	byte spare1
end

global record dllprocrec =
	ichar name
	ref proc address
	int dllindex
end

global record procrec =
	ref strec def
	ref procrec nextproc
end
!
global const int maxtype=20'000

global int ntypes

global [0:maxtype]ref strec	ttnamedef
global [0:maxtype]i16	ttbasetype			!basic t-code
global [0:maxtype]int	ttlength			!0, or array length
global [0:maxtype]byte	ttconst				!1 when const
global [0:maxtype]i16	tttarget			!pointer target or array elem type
global [0:maxtype]i16	ttreftype			!0, or index of type that is a pointer to this one
global [0:maxtype]i16	ttconsttype			!0, or index of type that is a const version of this oneointer to this onee
global [0:maxtype]int	ttsize				!byte size
global [0:maxtype]byte	ttisref
!global [0:maxtype]byte	ttcat
global [0:maxtype]ref paramrec ttparams		!for modes involving function pointers
global [0:maxtype]ref strec tttypedef

global int trefchar							!set to to char* type
global int trefwchar						!set to to wchar* type

global ichar inputfile
global int mainfileno
global [0..maxsourcefile]ichar sourcefilenames
global [0..maxsourcefile]ichar sourcefilepaths
global [0..maxsourcefile]ichar sourcefiletext
global [0..maxsourcefile]i32 sourcefilesizes

global int nsourcefiles

global const maxsearchdirs=20
global const maxincludepaths=20

global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0
global [maxincludepaths]ichar includepaths
global int nincludepaths=0

global ref strec stprogram		!root into the symbol table
global ref strec stmodule		!main module

global filehandle logdev		!dest for diagnostics and output of tables


global const sourceext="c"

global int clineno=0		!set in codegen scanner
global int cfileno=0		!set in codegen scanner

global tokenrec lx				!provides access to current token data
global tokenrec nextlx

global int debug=0

global int hstsize	= 65536

global int hstmask				!filled in with hstsize-1

global ref[0:]ref strec hashtable

global const maxblock=2100,maxblockstack=100
global [0..maxblock]i32 blockowner
global [0..maxblock]i32 blockcounts
global [0..maxblockstack]i32 blockstack
global int currblockno,nextblockno,blocklevel
global ref strec currproc

global const maxnestedloops=64

global ichar dheaderfile=nil			!result of -d:file.h switch

global int structpadding=1
global int callbackflag=0

global int slineno,sfileno

global ichar oemname="MCC"
=== cc_genpcl.m 0 0 5/24 ===

global int retindex

!const maxnestedloops	= 50

!global [maxnestedloops,4]int loopstack
!global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit
!symbol dcmdskip

macro divider = gencomment("------------------------")

global proc codegen_pcl=
!generate code for module n
	symbol d,e
	ref procrec pp

	pcl_start(nil)

!do two passes: module decls first, then procs
	gencomment("1:Start of code")

	d:=stmodule.deflist
	while d do
		case d.nameid
		when staticid then
			dostaticvar(d)
		when procid then
			case d.scope
			when exported_scope then
				if d.code=nil then d.scope:=imported_scope fi
			when local_scope then
				if d.code=nil then gerror_s("Static fn not defined: #",d.name) fi
			esac
	
			e:=d.deflist
			while e do
				case e.nameid
				when staticid then
					dostaticvar(e)
				when frameid then
					if e.code then
						if e.code.tag=jmakelist or 
						    ttbasetype[e.mode]=tarray and e.code.tag=jconst then
							dostaticvar(e)
						fi
					fi
				esac
				e:=e.nextdef
			od

		esac
		d:=d.nextdef
	od
	gencomment("")

	d:=stmodule.deflist
	while d do
		case d.nameid
		when procid then
			if d.code then
				genprocdef(d)
			fi
		esac
		d:=d.nextdef
	od

	pcl_end()

end

proc genprocdef (symbol p) =
	symbol d
	psymbol e
	ref paramrec pm
	int ismain:=0

	if eqstring(p.name,"main") then
		ismain:=1
		p.ismain:=1
	fi
!
	currproc:=p

	pc_defproc(e:=getpsymbol(p), isentry:ismain)
	e.variadic:=p.paramlist.flags=pm_variadic

	d:=p.deflist
	while d, d:=d.nextdef do
		case d.nameid
		when paramid then
			pc_addparam(getpsymbol(d))

		when frameid then
			pc_addlocal(getpsymbol(d))

		esac
	od

	retindex:=createfwdlabel()

	divider()

!	do_enter(p)

	do_stmt(p.code)

	divider()

	if ismain then
		pc_gen(kload, genint(0))
		setmode(ti32)
		pc_gen(kjumpret, genlabel(retindex))
		setmode(ti32)
	fi

	definefwdlabel(retindex)

	pc_gen((p.mode<>tvoid|kretfn|kretproc))		!mcl checks stack is not empty for retfn
	setmode(p.mode)

	pc_endproc()
!
	gencomment("")
end

proc dostaticvar(symbol d)=
	[256]char str
	int align
	symbol e
	psymbol p

	return when d.scope=imported_scope

	align:=getalignment(d.mode)

	if d.code then
		if d.nameid=frameid then			!const init data for local var
			fprint @str,"$#.#.#",d.owner.name,d.name,d.blockno
			e:=createdupldef(nil,addnamestr(str),staticid)
			p:=getpsymbol(e)
			d.pdata:=p
			pc_gen(kistatic, genmem(p))
		else
			pc_gen(kistatic, genmem_d(d))
		fi

		setmode(d.mode)
		pc_setalign(align)
		genidata(d.code)
	else
		pc_gen(kzstatic, genmem_d(d))
		setmode(d.mode)
		pc_setalign(align)
	fi
end

proc genidata(unit p,int doterm=1,am=1,offset=0)=
	int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion
	unit q,a,b
	ref strec d
	r32 sx
	[256]char str
	[16]char str2

	t:=p.mode
	a:=p.a
	b:=p.b

	case p.tag
	when jmakelist then
		n:=p.count					!number of supplied params
		if ttbasetype[t]=tarray then
			length:=ttlength[t]			!actual length of array
			q:=a
			for i:=1 to n do
				genidata(q)
				q:=q.nextunit
			od
			if n<length then			!rest will be zeros
				doresb((length-n)*ttsize[tttarget[t]])
			fi
		else
			isunion:=ttbasetype[t]=tunion

			d:=ttnamedef[t].deflist
			size:=ttsize[t]				!total bytes in struct
			offset1:=offset2:=0			!offset so far; 1/2 are in idata/in struct
			q:=a
			for i:=1 to n do
				genidata(q,0)
				offset1+:=ttsize[q.mode]
				d:=d.nextdef
				if d and not isunion then
					offset2:=d.offset
				else
					offset2:=size
				fi

				padding:=offset2-offset1
				if padding>0 then
					doresb(offset2-offset1)
					offset1:=offset2
				fi
				q:=q.nextunit
			od
			if offset2<size then
				doresb(size-offset2)
			fi
		fi
		return
	when jconst then
		if isintcc(t) or isrealcc(t) then
			if t=tr32 then
				sx:=p.xvalue
				pc_gen(kdata,genint(ref u32(&sx)^))
			else

				pc_gen(kdata, genint(p.value))
			fi
			setmode(t)
		elsif ttbasetype[t]=tref then
			padding:=0
	doref:
			if p.value=0 then
				pc_gen(kdata, genint(0))

			elsif p.isstrconst then
				pc_gen(kdata, genstring(p.svalue, p.slength))

			elsif p.iswstrconst then
				GERROR("GENIDATA/WSTRING2")
				doresb(padding)
			else
				pc_gen(kdata, genint(p.value))
			fi
			setmode(t)

		elsif ttbasetype[t]=tarray then
			padding:=(ttlength[t]-p.slength)*ttsize[tttarget[t]]
			for i to p.slength do
				pc_gen(kdata, genint((p.svalue+i-1)^))
				setmode(tu8)
			od
			doresb(padding)

		else
			CPL strmode(t)
			GERROR("IDATA/SCALAR")
		fi
		return
	when jname, jfuncname then
		d:=p.def
		case d.nameid
		when staticid,procid then
			pc_gen(kdata, genmemaddr_d(d))
			pccurr.scale:=1
			pccurr.extra:=offset
			setmode(tu64)

		else
			gerror("Idata &frame",p)
		esac	
		return

	when jadd then
		if a.tag=jname and b.tag=jconst then
			d:=a.def
			case d.nameid
			when staticid then
				strcpy(str,"`")
				if d.scope=function_scope then
					strcat(str,currproc.name)
					strcat(str,",")
				fi
				strcat(str,d.name)
				strcat(str,"+")

				getstrint(b.value, str2)

				strcat(str,str2)
				pc_gen(kdata, genname(str))
			else
				gerror("Add/Idata &frame")
			esac	
		elsif a.tag=jconst and b.tag=jconst and ttbasetype[a.mode]=tref then		!ASSUME REF+REF
			print @str,a.value,,"+",,b.value
			pc_gen(kdata, genname(str))

		else
			gerror("1:Runtime or unsupported expr in static data")
		fi
		return

	when jaddrof then
		if a.tag=jptr then
			genidata(a.a,offset:offset)
		else
			genidata(a, am:0,offset:offset)
		fi

	when jaddptr,jsubptr then
		if b.tag<>jconst then gerror("Complex ptr expr in static data") fi
		genidata(a,offset:b.value*p.ptrscale+offset)

	when jconvert then
		genidata(a,offset:offset)

	else
		PRINTUNIT(NIL,P)
		gerror("2:Runtime expr in static data",p)
	esac
end

proc doresb(int n)=
	while n>=8 do
		pc_gen(kdata, genint(0))
		n-:=8
		setmode(tu64)
	od
	to n do
		pc_gen(kdata, genint(0))
		setmode(tu8)
	od

end
=== cc_lex.m 0 0 6/24 ===
! (C tokeniser module)
ref tokenrec tkptr=nil

int dowhitespace=0

const mcchdr = "mcc.h"

record stackinforec =
	ref char startptr
	ref char sptr
	i32 lineno
	i32 fileno
end

const maxmacroargs=200
tokenrec normaltkx
ref tokenrec normaltk = &normaltkx			!indicates use lexm to get tokens
int noexpand=0						!inhibit macro expansion for 'defined'

const maxnesting=20
[maxnesting]stackinforec lx_stack
int lx_stackindex
int ifcondlevel=0					!counts #if levels
[maxnesting]ichar headerpathlist	!remember path at each level
[300]char headerpath				!as set up by getsource()

const cr	= 13
const lf	= 10
const tab	= 9

ref char lxstart
ref char lxsptr
int lxhashvalue
ref char lxsvalue

[0..255]char alphamap
[0..255]char digitmap
[0..255]char commentmap
[0..255]char linecommentmap
[0..255]char spacemap

const int maxpastedtokens=7000
!const int maxpastedtokens=87000
global [maxpastedtokens]ichar pastedtokenlist
int npastedtokens=0
int isincludefile=0				!whether readng include file name

int firstsymbol=1

global int nhstsymbols
int hstthreshold				!limit above which new hst should be generated

global proc lexreadtoken=
!read next token into nextlx
!	int c,csum,hsum,dodir
	word c,csum,hsum,dodir
	ref char p,ss
	ichar searchstr
	byte cc

	nextlx.subcode:=0
	nextlx.flags:=0

	while lxsptr^ in [' ','\t'] do ++lxsptr od

	doswitch lxsptr++^
	when 'A'..'Z','a'..'z','$','_' then
doname:
		lxsvalue:=lxsptr-1
		hsum:=lxsvalue^

		while alphamap[c:=lxsptr++^] do
			hsum:=hsum<<4-hsum+c
		od
		--lxsptr
		nextlx.symbol:=namesym
		nextlx.length:=lxsptr-lxsvalue
		case c
		when '\'', '"' then
			if nextlx.length=1 then
				case lxsvalue^
				when 'l','L','u','U' then
					++lxsptr
					lxreadstring(c,1)
					return
				esac
			fi
		esac

		lxhashvalue:=hsum<<5-hsum

		lookup()						!clash, so do normal lookup to set lxsymptr
		return

	when '1'..'9' then					!can only be decimal
		case lxsptr^
		when ' ',')',cr,',',';' then		!assume single digit decimal
			nextlx.symbol:=intconstsym
			nextlx.subcode:=ti32
			nextlx.value:=(lxsptr-1)^-'0'
			nextlx.length:=1

			setnumberoffset(lxsptr-1-lxstart)
		else
			readdecimal(lxsptr-1)				!note: can also be real const;
		esac
		return

	when '0' then					!0, hex, binary or octal
		case lxsptr^
		when 'x','X' then
			++lxsptr
			readhex(lxsptr-2)
			return
		when 'b','B' then
			++lxsptr
			readbinary(lxsptr-2)
			return
		when '.' then
			readrealnumber(lxsptr-1,lxsptr-1,1,10)
			return
		when 'u','U','l','L' then
			readdecimal(lxsptr-1)				!note: can also be real const;
			return
		when ',', ')', ']', '}', ';', ' ',':',cr,lf,'&','=','?' then	!assume just zero
			nextlx.symbol:=intconstsym
			nextlx.subcode:=ti32
			nextlx.value:=0
			nextlx.length:=1
			setnumberoffset(lxsptr-1-lxstart)
			return
		else

			readoctal(lxsptr-1)
			return
		end case					!else assume just zero	

	when '#' then			!
		if nextlx.symbol=eolsym then
			nextlx.symbol:=lexhashsym

			return

		elsif lxsptr^='#' then
			++lxsptr
			nextlx.symbol:=hashhashsym
			return
		else
			nextlx.symbol:=hashsym
			return
		fi

	when '\\' then			!line continuation
		docase lxsptr^
		when cr,lf then
			exit
		when ' ',tab then
			++lxsptr
		else
			nextlx.symbol:=backslashsym
			return
		end docase

		(lxsptr-1)^:=' '	!convert \ to space
		++nextlx.lineno
		case lxsptr^
		when cr then
			++lxsptr			!point to lf
			lxsptr++^:=' '		!set lf to space (so that '#' processing works
		when lf then
			lxsptr++^:=' '
		else
		esac

	when '{' then
		nextlx.symbol:=lcurlysym
		return

	when '}' then
		nextlx.symbol:=rcurlysym
		return

	when '.' then
		case cc:=lxsptr^
		when '.' then				!.. or ...
			++lxsptr
			if lxsptr^='.' then
				++lxsptr
				nextlx.symbol:=ellipsissym
			else
				--lxsptr
				nextlx.symbol:=dotsym
				return
			fi
			return
		elsif c in '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
			readrealnumber(lxsptr,lxsptr,0,10)
			return
		else
			nextlx.symbol:=dotsym
			return
		end case

	when ',' then
		nextlx.symbol:=commasym
		return

	when ';' then
		nextlx.symbol:=semisym
		return

	when ':' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=assignsym
		else
			nextlx.symbol:=colonsym
		esac
		return

	when '(' then
		nextlx.symbol:=lbracksym
		return

	when ')' then
		nextlx.symbol:=rbracksym
		return

	when '[' then
		nextlx.symbol:=lsqsym
		return

	when ']' then
		nextlx.symbol:=rsqsym
		return

	when '|' then
		case lxsptr^
		when '|' then
			++lxsptr
			nextlx.symbol:=orlsym
		when '=' then
			++lxsptr
			nextlx.symbol:=iortosym
		else
			nextlx.symbol:=iorsym
		esac
		return

	when '^' then
		if lxsptr^='=' then
			++lxsptr
			nextlx.symbol:=ixortosym
		else
			nextlx.symbol:=ixorsym
		fi
		return

	when '?' then
		nextlx.symbol:=questionsym
		return

	when '~' then
		nextlx.symbol:=inotsym
		return

	when '+' then
		case lxsptr^
		when '+' then
			++lxsptr
			nextlx.symbol:=incrsym
		when '=' then
			++lxsptr
			nextlx.symbol:=addtosym
		else
			nextlx.symbol:=addsym
		esac
		return

	when '-' then
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=decrsym
		when '>' then
			++lxsptr
			nextlx.symbol:=idotsym
		when '=' then
			++lxsptr
			nextlx.symbol:=subtosym
		else
			nextlx.symbol:=subsym
		esac
		return

	when '*' then
		if lxsptr^='=' then
			++lxsptr
			nextlx.symbol:=multosym
		else
			nextlx.symbol:=mulsym
		fi
		return

	when '/' then
		case lxsptr^
		when '/' then					!comment to 
			readlinecomment()
			nextlx.symbol:=eolsym
			nextlx.length:=0
			return
		when '*' then
			readblockcomment()
		when '=' then
			++lxsptr
			nextlx.symbol:=divtosym
			return
		else
			nextlx.symbol:=divsym
			return
		esac

	when '%' then
		if lxsptr^='=' then
			++lxsptr
			nextlx.symbol:=remtosym
		else
			nextlx.symbol:=remsym
		fi
		return

	when '=' then
		case lxsptr^
		when '=' then
			nextlx.symbol:=eqsym
			++lxsptr
		else
			nextlx.symbol:=assignsym
		esac
		return

	when '<' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=lesym
		when '<' then
			if (++lxsptr)^='=' then
				++lxsptr
				nextlx.symbol:=shltosym
			else
				nextlx.symbol:=shlsym
			fi
		else
			nextlx.symbol:=ltsym
		end case
		return

	when '>' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=gesym
		when '>' then
			if (++lxsptr)^='=' then
				++lxsptr
				nextlx.symbol:=shrtosym
			else
				nextlx.symbol:=shrsym
			fi
		else
			nextlx.symbol:=gtsym
		end case
		return

	when '&' then
		case lxsptr^
		when '&' then
			++lxsptr
			nextlx.symbol:=andlsym
		when '=' then
			++lxsptr
			nextlx.symbol:=iandtosym
		else
			nextlx.symbol:=iandsym
		esac
		return

	when '\'' then
		lxreadstring('\'',0)
		return

	when '"' then
		lxreadstring('"',0)
		return

	when ' ',tab then

	when lf then
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0
		if dowhitespace then
			nextlx.svalue:=cast(lxsptr)

			docase (lxsptr++)^
			when ' ',tab then
			else
				--lxsptr
				exit
			end

			nextlx.length:=lxsptr-nextlx.svalue
		fi
		return
	when cr then				!ignore; always expect lf to follow

	when '!' then
		case lxsptr^
		when '=' then
			nextlx.symbol:=nesym
			++lxsptr
		else
			nextlx.symbol:=notlsym
		esac
		return

	when '@' then
		PRINTLN "@ SEEN",nextlx.lineno,sourcefilenames[nextlx.fileno],lx_stackindex

	when 0 then
	doeof:
		--lxsptr
		if lx_stackindex then
			unstacksourcefile()
			nextlx.symbol:=eolsym
		else
			nextlx.symbol:=eofsym
		fi
		return

	when 12 then

	else
		if 128<=(lxsptr-1)^<= 255then goto doname fi

		PRINTLN "ERROR CHAR",(lxsptr-1)^,int((lxsptr-1)^),lx_stackindex
		lxerror("ERROR CHAR")
		nextlx.symbol:=errorsym
		return

	end doswitch

end

function readexponent(int &badexpon)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
!exponent is always in base 10
	ref char numstart
	int length,neg,c
	i64 a

	neg:=0
	case lxsptr^
	when '+' then ++lxsptr
	when '-' then ++lxsptr; neg:=1
	esac

	numstart:=lxsptr
	length:=scannumber(10)-numstart

	if length=0 then
		badexpon:=1
		return 0
	fi

	a:=0

	to length do
		c:=numstart++^
		a:=a*10+c-'0'
	od

	return (neg|-a|a)
end

proc lxerror(ichar mess)=
	PRINTLN "\nLex error",mess,"in:",,sourcefilepaths[getfileno()],
	 "Line:",nextlx.lineno
	println
	stop 11
end

global proc lexsetup=
!do one-time setup:
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i

	inithashtable()
	fillhashtable()

	for i:=0 to 255 do
!		switch i
!		when 'A'..'Z','a'..'z','$','_','0'..'9' then
		if i in 'A'..'Z' or i in 'a'..'z' or i in '0'..'9' or i in ['_', '$'] then
			alphamap[i]:=1
		end
		if i in '0'..'9' then
!		when '0'..'9' then
			digitmap[i]:=1
		end
		commentmap[i]:=1
		linecommentmap[i]:=1
		spacemap[i]:=0
	od

	commentmap['*']:=0
	commentmap[0]:=0
	commentmap[lf]:=0

	linecommentmap[0]:=0
	linecommentmap['\\']:=0
	linecommentmap[lf]:=0

	spacemap[' ']:=1
	spacemap[tab]:=1

	normaltkx.symbol:=eolsym
	npastedtokens:=0
end

global proc printstrn(ichar s, int length,filehandle f=nil)=
	if length then
		if f=nil then
			print length:"v",,s:".*"
		else
			print @f,length:"v",,s:".*"
		fi
	fi
end

function scannumber(int base)ref char=
	ref char dest
	int c

	dest:=lxsptr

	doswitch c:=lxsptr++^
	when '0'..'9' then
		dest++^:=c
		if c>='0'+base then
			lxerror("Digit out of range")
		fi
	when 'A'..'F','a'..'f' then
			if base=16 then
			dest++^:=c
		else
			--lxsptr
			exit
		fi
	when '_','\'','`' then
	else
		--lxsptr
		exit
	end doswitch
	return dest
end

function lookup:int=
	int j, wrapped,length

	retry:
	j:=lxhashvalue iand hstmask
	wrapped:=0

	do
		nextlx.symptr:=hashtable^[j]
		length:=nextlx.symptr.namelen

		if not length then
			exit
		fi

		if length=nextlx.length then	!match on length
			if memcmp(nextlx.symptr.name,lxsvalue,length)=0 then	!match
				return 1
			fi
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	nextlx.symptr.name:=pcm_copyheapstringn(lxsvalue,nextlx.length)
	nextlx.symptr.namelen:=nextlx.length
	nextlx.symptr.symbol:=namesym

	++nhstsymbols

	return 0
end

global function gethashvalue(ichar s,int length=-1)word=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!assumes s is lower-case, as conversion not done
!	int c,hsum
	word c,hsum

	if length=-1 then
		length:=strlen(s)
	fi
	hsum:=0

	to length do
		hsum:=hsum<<4-hsum+word(s++^)
	od
	return hsum<<5 -hsum
end

proc inithashtable=
	hashtable:=pcm_alloc(hstsize*(ref void.bytes))
	hstmask:=hstsize-1

	for i:=0 to hstmask do
		hashtable^[i]:=pcm_allocz(strec.bytes)
	od

	nhstsymbols:=0
	hstthreshold:=(6*hstsize)/10

end

proc fillhashtable=
!populate hashtable with standard symbols
	int i

	for i:=1 to stnames.len do
		lxsvalue:=stnames[i]

!sourcedir names can be converted to user name types, which could have a
!zero appended in lex() as the assumption is they are still in-situ within the source
!But with compilers like gcc, these names are in read-only memory.
!So copy them to the heap.

		if stsymbols[i]=ksourcedirsym then
			lxsvalue:=pcm_copyheapstring(lxsvalue)
		fi
		nextlx.length:=strlen(lxsvalue)
		lxhashvalue:=gethashvalue(lxsvalue,nextlx.length)

		if lookup() then
			println stnames[i]
			abortprogram("Duplicate symbol table entry")
		fi

		nextlx.symptr.symbol:=stsymbols[i]
		nextlx.symptr.subcode:=stsubcodes[i]
	od

end

function dolexdirective:int=
!positioned just after '#' which is first symbol on a line
!read pp directive and act on it
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
	ref strec symptr,d
	ref char p,pstart,s
	int i,cond,c,syshdr,dir,length, allowmacros
	[300]char filename

	pstart:=lxsptr

	dir:=getlexdirective()
	if dir=0 then
		printstrn(pstart,lxsptr-pstart); println
		lxerror("Invalid # directive")
	fi

	case dir
	when includedir then
		isincludefile:=1

		while lxsptr^=' ' or lxsptr^=tab do ++lxsptr od
		allowmacros:=lxsptr^ <> '<'

		lexm()
		isincludefile:=0

		if nextlx.symbol=ltsym then
			syshdr:=1
			p:=filename

			if allowmacros then

				do
					lexm()
					case nextlx.symbol
					when eofsym, eolsym then
						lxerror("Bad include file")
					when gtsym then
						exit
					else
						s:=strtoken(&nextlx,length)
						memcpy(p,s,length)
						p+:=length
					esac
				od
			else
				do
					c:=lxsptr++^
					case c
					when '>' then
						exit
					when lf,0 then
						lxerror("include: > expected")
					else
						p++^:=c
					esac
				od
			fi
			p^:=0

		elsif nextlx.symbol=stringconstsym then
			syshdr:=0
			strcpy(filename,nextlx.svalue)
		else
			lxerror("include?")
		fi
		lexm()

		stacksourcefile(filename,syshdr)

	when definedir then
		dodefine()

	when undefdir then
		lexreadtoken()
		if nextlx.symbol<>namesym then
			lxerror("undef: name expected")
		fi
		d:=nextlx.symptr
		if d.nameid<>macroid then
!			println getstname(nextlx.symptr)
!			lxerror("#undef: can't find macro")
		else
			d.nameid:=nullid
			d.symbol:=nextlx.symptr.oldsymbol
			d.mparamlist:=nil
			d.flmacro:=0
		fi

	when ifdefdir then
		cond:=getifdef()
		goto doif

	when ifndefdir then
		cond:=not getifdef()
		goto doif

	when ifdir then
		cond:=getifexpr()
	doif:

		++ifcondlevel
		if cond then			!carry on reading code as normal
			return 0
		else
	doskipcode:
			dir:=skipcode()
			case dir
			when elifdir then
				cond:=getifexpr()
				if cond then			!do this
					return 0
				fi
				goto doskipcode
			when elsedir then			!do following code
			when endifdir then
				--ifcondlevel
			esac
		fi

	when elifdir, elsedir then			!encountered after true block
		if not ifcondlevel then
			lxerror("#if missing/elif/else")
		fi
		repeat
			dir:=skipcode()
		until dir=endifdir
		--ifcondlevel

	when endifdir then
		if not ifcondlevel then
			lxerror("#if missing/endif")
		fi
		--ifcondlevel

	when blankdir then
	when linedir then
		repeat
			lexreadtoken()
		until nextlx.symbol=eolsym
	when errordir then
		lexm()
		print "#ERROR:"; showtoken(&nextlx); println
		lxerror("ABORTING")

	when pragmadir then
		dopragmadir()

	else
	skip:
		println "DIRECTIVE NOT IMPL:",sourcedirnames[dir]
		lxsptr:=pstart
		nextlx.symbol:=lexhashsym
		return 1
		lxerror("Directive not implemented")
	esac
	return 0
end

function getlexdirective:int=
!at '#'; read directive, and return index; 0 on error
	ref strec d

	lexreadtoken()

	case nextlx.symbol
	when namesym then
	when eolsym then
		return blankdir
	when intconstsym then
		repeat
			lexreadtoken()
		until nextlx.symbol=eolsym or nextlx.symbol=eofsym
		return blankdir
	else
		return 0
	esac

	case nextlx.symptr.symbol
	when ksourcedirsym then
		return nextlx.symptr.subcode
	when kifsym then
		return ifdir
	when kelsesym then
		return elsedir
	when eolsym then
		return blankdir
	esac

	d:=nextlx.symptr
	if d.nameid=macroid then			!could have redefined 'define' etc
		if d.oldsymbol=ksourcedirsym then
			return d.subcode
		fi
	fi

	return 0
end

global proc startlex(ichar caption,int fileno)=
!s is a 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it

	ifcondlevel:=0
	lx_stackindex:=0
	noexpand:=0

	normaltk := &normaltkx			!indicates use lexm to get tokens

	lx_stackindex:=0
	ifcondlevel:=0
	firstsymbol:=1
	npastedtokens:=0
	isincludefile:=0
	tkptr:=nil

	lxstart:=lxsptr:=sourcefiletext[fileno]
	setfileno(fileno)
	nextlx.lineno:=1
	nextlx.numberoffset:=0

	nextlx.symbol:=eolsym
	nextlx.subcode:=0
	lex()
end

global proc endlex=
	if ifcondlevel then
		println ifcondlevel
		lxerror("#endif missing")
	fi
end

proc readlinecomment=
!positioned at second '/' of '//'

	do
		while linecommentmap[(++lxsptr)^] do od		!skip bulk of characters

		case lxsptr^
		when lf then
			++lxsptr
			exit
		when 0 then
			exit					!assume on last line not ending in newline char
		when '\\' then
			++lxsptr
			case lxsptr^
			when cr then			!skip following lf and loop
				lxsptr+:=2
				++nextlx.lineno
			when lf then			!loop
				++lxsptr
				++nextlx.lineno
			esac					!else ignore and loop
		esac
	od
	++nextlx.lineno
end

proc readblockcomment=
!positioned at '*' of '/*'

	do
		while commentmap[(++lxsptr)^] do od		!skip bulk of characters

		case lxsptr^
		when lf then
			++nextlx.lineno
		when 0 then
			lxerror("block comment eof")
		when '*' then
			if (lxsptr+1)^='/' then		!found end of comment
				lxsptr+:=2
				exit
			fi
		esac
	od
end

proc readhex(ref char pstart)=
!positioned at first char of hex number, after 0x/0X
	u64 aa
	word c
	int length,leading,ll,usigned
	ref char p

	aa:=0
	p:=lxsptr
	leading:=1
	ll:=usigned:=0
	length:=0

	doswitch c:=lxsptr++^
	when '1'..'9' then
		leading:=0
		aa:=aa*16+(c-'0')
		++length
	when '0' then
		if leading then
			++p			!ignore leading zeros
		else
			++length
			aa:=aa*16
		fi
	when 'A'..'F' then
		leading:=0
		++length
		aa:=aa*word(16)+(c-'A'+10)
	when 'a'..'f' then
		leading:=0
		++length
		aa:=aa*word(16)+(c-'a'+10)
	when '.','P','p' then
		--lxsptr
		readrealnumber(pstart,p,lxsptr-p,16)
		return
	when 'L','l' then
		++ll
		if ll>2 then lxerror("-LL?") fi
	when 'U','u' then
		if usigned then lxerror("-U?") fi
		usigned:=1
	else
		--lxsptr
		exit
	end doswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>16 then
		lxerror("Overflow in hex number")
	fi

	nextlx.symbol:=intconstsym
	if aa>u64(0x7FFF'FFFF'FFFF'FFFF) then
		nextlx.subcode:=tu64
	elsif aa>u64(0xFFFF'FFFF) then
		nextlx.subcode:=ti64
	elsif aa>u64(0x7FFF'FFFF) then
		nextlx.subcode:=tu32
	else
		nextlx.subcode:=ti32
	fi
	nextlx.value:=aa

	if ll=2 or ll=1 and flong64 then
		case nextlx.subcode
		when tu32 then nextlx.subcode:=tu64
		when ti32 then nextlx.subcode:=ti64
		esac
	fi

	if usigned then
		case nextlx.subcode
		when ti32 then nextlx.subcode:=tu32
		when ti64 then nextlx.subcode:=tu64
		esac
	fi

	checknumbersuffix()
end

proc readbinary(ref char pstart)=
!positioned at first char of binary number, after 0b/0B
	u64 aa
	int c,length,res,leading
	ref char p

	aa:=0
	p:=lxsptr
	leading:=1

	doswitch c:=lxsptr++^
	when '1' then
		leading:=0
	when '0' then
		if leading then ++p fi					!ignore leading zeros
	when '2'..'9' then
		lxerror("Binary bad digit")
	when '.' then
		lxerror("Binary fp")

	else
		--lxsptr
		exit
	end doswitch

	length:=lxsptr-p
	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>64 then
		lxerror("Overflow in binary number")
	fi

	to length do
		aa:=aa*2+p++^-'0'
	od

	nextlx.symbol:=intconstsym
	nextlx.subcode:=ti32
	if aa>=u64(0x7FFF'FFFF) then
		nextlx.subcode:=ti64
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readoctal(ref char pstart)=
!positioned at first char of octal number, after 0 (or at 8 or 9)
	u64 aa
	int c,length,res,leading,ll,usigned
	ref char p

	aa:=0
	p:=lxsptr
	leading:=1
	ll:=usigned:=0
	length:=0

	doswitch c:=lxsptr++^
	when '1'..'7' then
		leading:=0
		++length
	when '0' then
		if leading then
			++p				!ignore leading zeros
		else
			++length
		fi
	when '.' then
		--lxsptr
		readrealnumber(pstart,p,lxsptr-p,10)
		return
	when 'L','l' then
		++ll
		if ll>2 then lxerror("-LL?") fi
	when 'U','u' then
		if usigned then lxerror("-U?") fi
		usigned:=1
	else
		if alphamap[c] then
	doalpha:
			readalphanumeric(pstart)
			return
		fi
		--lxsptr
		exit
	end doswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>22 or length=22 and (res:=cmpstringn(p,"1777777777777777777777",22))>0 then
		lxerror("Overflow in octal number")
	fi

	to length do
		aa:=aa*8+p++^-'0'
	od

	nextlx.symbol:=intconstsym
	nextlx.subcode:=ti32
	if aa>=u64(0x7FFF'FFFF) then
		nextlx.subcode:=ti64
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readdecimal(ref char pstart)=
!positioned at first char of decimal number
!will read integer, unless ends with any of ".eE" than assumed to be real
	u64 aa
	int c,length,res,leading
	byte ll,usigned

	ref char p

	aa:=0
	ll:=usigned:=0

	p:=--lxsptr

	while digitmap[(++lxsptr)^] do od

	while p^='0' do ++p od
	length:=lxsptr-p

	doswitch c:=lxsptr++^
	when '.','E','e' then
		--lxsptr
		readrealnumber(pstart,p,lxsptr-p,10)
		return
	when 'L','l' then
		++ll
		if ll>2 then lxerror("-LL?") fi
	when 'U','u' then
		if usigned then lxerror("-U?") fi
		usigned:=1
	else
		if alphamap[c] then
			readalphanumeric(pstart)
			return
		fi
		--lxsptr
		exit
	end doswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>20 or length=20 and (res:=cmpstringn(p,"18446744073709551615",20))>0 then
		lxerror("Overflow in decimal number")
	fi

	to length do				!A..Z have been preprocessed so that they carry on from '9'
		aa:=aa*u64(10)+word(p++^-'0')
	od

	nextlx.symbol:=intconstsym

	case ll
	when 0 then
		if aa>=i32.max then
			nextlx.subcode:=ti64
		else
			nextlx.subcode:=ti32
		fi
		if usigned then
			if aa>=u64(0xFFFF'FFFF) then
				nextlx.subcode:=tu64
			else
				nextlx.subcode:=tu32
			fi
		else
			if aa>=u64(0x7FFF'FFFF) then
				nextlx.subcode:=ti64
			fi
		fi
	when 1 then
		if flong64 then recase 2 fi
		if usigned then
			if aa>=u64(0xFFFF'FFFF) then
				nextlx.subcode:=tu64
			else
				nextlx.subcode:=tu32
			fi
		else
			if aa>=u64(0x7FFF'FFFF) then
				nextlx.subcode:=ti64
			else
				nextlx.subcode:=ti32
			fi
		fi
	when 2 then
		if usigned then
			nextlx.subcode:=tu64
		else
			nextlx.subcode:=ti64
		fi
	esac

	nextlx.value:=aa
end

function checknumbersuffix:int=
!return type of the constant
!positioned at terminator character which might be a suffix
	char c

	docase c:=lxsptr++^
	when 'L','l','u','U' then
!	lxerror("Numeric SUFFIX")
	else
!		if alphamap[c] then
!*!		lxerror("Bad number suffix")
!		fi
		--lxsptr
		exit
	end

	return ti32			!don't bother for now
end

proc stacksourcefile(ichar file,int syshdr)=
	ref char sptr
	int fileno
	stackinforec info
	[500]char fullpath

	fileno:=getsourcefile(file,syshdr)
	if fileno=0 then
		println file,strlen(file)
		lxerror("Can't find include file")
	fi

	if lx_stackindex>=maxnesting then
		lxerror("Too many nested includes")
	fi
	++lx_stackindex

	fullpath[1]:=0
	if lx_stackindex>1 then
		strcpy(fullpath,headerpathlist[lx_stackindex-1])
	fi

	if headerpath[1] then
		strcat(fullpath,pcm_copyheapstring(headerpath))
	fi

	headerpathlist[lx_stackindex]:=pcm_copyheapstring(fullpath)

	info.startptr:=lxstart
	info.sptr:=lxsptr
	info.lineno:=nextlx.lineno
	info.fileno:=getfileno()
	lx_stack[lx_stackindex]:=info

	lxstart:=lxsptr:=sourcefiletext[fileno]
	setfileno(fileno)
	nextlx.lineno:=1
end

proc unstacksourcefile=
!called has checked that stack has >=1 entries
	ichar path
	stackinforec info

	path:=headerpathlist[lx_stackindex]
	pcm_free(path,strlen(path))

	info:=lx_stack[lx_stackindex--]
	lxstart:=info.startptr
	lxsptr:=info.sptr
	nextlx.lineno:=info.lineno
	setfileno(info.fileno)
end

function getsourcefile(ichar file,int syshdr)int=
!locate using search dirs; 
!read contents into memory, and return fileno
!returns 0 in case of error (file not found, memory problem)

	static [300]char filespec
	[300]char filespec2
	ichar hdrtext
	int i

	headerpath[1]:=0

	strcpy(filespec,file)
	convlcstring(filespec)

!check to see if already loaded
	for i:=1 to nsourcefiles do
		if eqstring(filespec,sourcefilenames[i]) then
			return i
		fi
	od

	strcpy(headerpath,extractpath(file))

	if headerpath[1] then
		if headerpath[1]='/' or headerpath[2]=':' and headerpath[3]='/' then
			if checkfile(file) then
					return loadsourcefile(file,file)
			fi
			return 0			!don't both looking anywhere else
		fi
	fi

	for i:=lx_stackindex downto 1 do
		strcpy(filespec,headerpathlist[i])
		strcat(filespec,file)

		if checkfile(filespec) then
			return loadsourcefile(filespec,file)
		fi
	od

	for i to nsearchdirs do
		strcpy(filespec,searchdirs[i])
		strcat(filespec,file)

		if checkfile(filespec) then
			strcpy(headerpath,extractpath(filespec))
			return loadsourcefile(filespec,file)
		fi
	od

	return 0
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
	reenter:

	lx:=nextlx				!grab that already read basic token

	lexm()			!read new token for next time around

	if lx.symbol=namesym and lx_stackindex=0 then
		(lx.symptr.name+lx.length)^:=0
	fi

	docase nextlx.symbol
	when namesym then
		nextlx.symbol:=nextlx.symptr.symbol			!convert to reserved word, type, op etc
		if nextlx.symbol=ksourcedirsym then
			nextlx.symbol:=namesym
		fi
		nextlx.subcode:=nextlx.symptr.subcode

		return

	when eolsym then								!lose eols
		lexm()
	else
		return	
	end docase

end

global function addnamestr(ichar name)ref strec=
!look up arbitrary name and return symptr to generic st entry

	tokenrec oldlx
	ref strec symptr

	oldlx:=nextlx
	nextlx.length:=strlen(name)
	lxhashvalue:=gethashvalue(name,nextlx.length)

	lxsvalue:=pcm_alloc(nextlx.length+1)
	memcpy(lxsvalue,name,nextlx.length+1)
	lookup()
	symptr:=nextlx.symptr

	nextlx:=oldlx

	return symptr
end

proc lxreadstring(int termchar,int fwide)=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter

	const maxlocalstr=2048
	[maxlocalstr]char str
	ref char dest,ws
	ref u16 wd,wd0
	int c,d,length,useheap

	if termchar='"' then
		nextlx.symbol:=(fwide|wstringconstsym|stringconstsym)
	else
		nextlx.symbol:=charconstsym
	fi

	nextlx.svalue:=lxsptr

	if lx_stackindex=0 and not fwide then
		dest:=lxsptr				!form string into same buffer
		ws:=dest					!for wide only
		useheap:=0
	else							!for headers that can be re-read, form string externally
		dest:=str
		ws:=dest					!for wide only
		useheap:=1
	fi
	length:=0

	do

		switch c:=lxsptr++^
		when '\\' then			!escape char
			if isincludefile then
				c:='/'
				goto normalchar
			fi
			c:=lxsptr++^
	reenter:
			switch c
			when 'a' then			!bell ('alert')
				c:=7
			when 'b' then			!backspace
				c:=8
			when 'f' then
				c:=12
			when 'n' then
				c:=lf
			when 'r' then
				c:=cr
			when 't' then			!tab
				c:=tab
			when 'v' then			!vertical tab
				c:=11
			when 'x' then	!2-digit hex code follows
				c:=0
!			to 2 do
				do
					switch d:=lxsptr++^
					when 'A','B','C','D','E','F' then
						c:=c*16+d-'A'+10
					when 'a','b','c','d','e','f' then
						c:=c*16+d-'a'+10
					when '0','1','2','3','4','5','6','7','8','9' then
						c:=c*16+d-'0'
					else
						--lxsptr
						exit
					end
				od
			when '0'..'7' then		!octal sequence
				c-:='0'				!get first digit
				to 2 do				!up to 2 more digits (some compilers will read N digits
					switch d:=lxsptr++^				!then check for overflow)
					when '0','1','2','3','4','5','6','7' then
						c:=c*8+d-'0'
					else
						--lxsptr
						exit
					end
				od

			when '"' then		!embedded double quote
				c:='"'
			when '\\' then
				c:='\\'
			when '\'' then			!embedded single quote
				c:='\''
			when cr then			!skip
				++nextlx.lineno
				if lxsptr^=lf then ++lxsptr fi
				nextloop
			when lf then
				++nextlx.lineno
				nextloop
			end						!else use the escaped character itself
		when '"','\'' then		!possible terminators
			if c=termchar then		!terminator char
				exit
			fi
		when 0 then
			println =nextlx.lineno
			lxerror("String not terminated")
		end switch
	normalchar:

		if not useheap then
			dest++^:=c
		elsif ++length<maxlocalstr then
			dest++^:=c
		else
			lxerror("Local str too long")
		fi
	od
	dest^:=0


	if fwide then			!need to put string on heap was will use 16-bit chars
		wd0:=wd:=pcm_alloc(length*2+2)
		to length do
			wd++^:=ws++^
		od
		wd^:=0
		nextlx.svalue:=cast(wd0)

	elsif useheap then
		nextlx.length:=length
		nextlx.svalue:=pcm_alloc(length+1)
		memcpy(nextlx.svalue,str,length+1)
	else
		nextlx.length:=dest-nextlx.svalue
	fi
end

proc addlisttoken(ref ref tokenrec ulist,ulistx,ref tokenrec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nexttoken:=p
	fi
	p.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlisttoken_copy(ref ref tokenrec ulist,ulistx,ref tokenrec q)=
!like addlisttoken but add copy of nextlx
!(as will likely be in nextlx)
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	ref tokenrec p

	p:=alloctoken()

	p^:=q^
	p.nexttoken:=nil

	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nexttoken:=p
	fi
	p.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlist_nextlx(ref ref tokenrec ulist,ulistx)=
!like addlisttoken but add copy of nextlx

	ref tokenrec p
	p:=alloctoken()
	p^:=nextlx
	p.nexttoken:=nil

	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nexttoken:=p
	fi
	p.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlisttoken_seq(ref ref tokenrec ulist,ulistx,ref tokenrec seq)=
	ref tokenrec tk

	while seq do
		tk:=alloctoken()
		tk^:=seq^

		if ulist^=nil then		!first
			ulist^:=ulistx^:=tk
		else
			ulistx.nexttoken:=tk
		fi
		tk.nexttoken:=nil
		ulistx^:=tk

		seq:=seq.nexttoken
	od
end

proc addlistmparam(ref ref mparamrec ulist,ulistx,ref mparamrec p)=
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextmparam:=p
	fi
	ulistx^:=p			!update end-of-list pointer
end

proc dodefine=
!'define' just seen

	ref mparamrec stlist,stlistx,p,q
	ref strec stname, d
	ref tokenrec tklist,tklistx,tk
	int nparams,ntokens,paramno

	lexreadtoken()
	if nextlx.symbol<>namesym then
		lxerror("define: name expected")
	fi
	stname:=nextlx.symptr
	stname.lineno:=nextlx.lineno
	stname.fileno:=nextlx.fileno

	stname.oldsymbol:=stname.symbol

	stname.symbol:=namesym
	stname.nameid:=macroid
	nparams:=0

	if lxsptr^='(' then
		++lxsptr
		stlist:=stlistx:=nil
		stname.flmacro:=1

		lexreadtoken()
		do
			case nextlx.symbol
			when namesym then			!next param
				d:=nextlx.symptr
				p:=stlist
				while p do
					if p.def=d then
						lxerror("Dupl macro param")
					fi
					p:=p.nextmparam
				od
				q:=pcm_alloc(mparamrec.bytes)
				q.def:=d
				q.nextmparam:=nil
				addlistmparam(&stlist,&stlistx,q)
				++nparams
				lexreadtoken()
				if nextlx.symbol=commasym then
					lexreadtoken()
				fi
			when rbracksym then
				exit
			when ellipsissym then					!I need to create a special symbol name
				d:=addnamestr("__VA_ARGS__")
				stname.varparams:=1		!flag macro as having a va/args as last param
				lexreadtoken()
				if nextlx.symbol<>rbracksym then
					lxerror("')' expected")
				fi

				q:=pcm_alloc(mparamrec.bytes)
				q.def:=d
				q.nextmparam:=nil
				addlistmparam(&stlist,&stlistx,q)
				++nparams
				exit
			else
				lxerror("macro params?")
			esac
		od
		stname.mparamlist:=stlist
	fi

!Now, loop reading tokens until eol
!Store tokens in list
	tklist:=tklistx:=nil
	ntokens:=0

	do
		lexreadtoken()
		case nextlx.symbol
		when eolsym,eofsym then
			exit
		when namesym then
			p:=stname.mparamlist
			paramno:=1
			while p do
				if p.def=nextlx.symptr then
					nextlx.flags ior:=tk_parammask
					nextlx.paramno:=paramno
					exit
				fi
				p:=p.nextmparam
				++paramno
			od
			if nextlx.symptr=stname then
				nextlx.flags ior:=tk_macromask
			fi
		esac

		++ntokens
		tk:=alloctoken()
		tk^:=nextlx
		addlisttoken(&tklist,&tklistx,tk)
	od

	stname.tokenlist:=tklist
	stname.nparams:=nparams
end

proc readalphanumeric(ref char pstart)=
!part-read numeric value starting at pstart is followed by non-numeric chars
!read rest of token starting from lxsptr, and form into a name token
	while alphamap[lxsptr++^] do od
	--lxsptr
	nextlx.svalue:=pstart
	nextlx.symbol:=rawnumbersym
	nextlx.length:=lxsptr-pstart
end

function inmacrostack(ref strec d, ref tokenrec macrostack)int=
!return 1 if d is part of the macrostack
!the macrostack is a linked list of strecs, but conveniently uses a list
!of tokens although it is not really a list of tokens

	while macrostack do
		if macrostack.symptr=d then return 1 fi
		macrostack:=macrostack.nexttoken
	od
	return 0
end

proc lexa(ref tokenrec &tk)=
	if tk=normaltk then
		lexreadtoken()
		return
	fi
	if tk=nil then
		nextlx.symbol:=eofsym
		return
	fi
	nextlx:=tk^
	tk:=tk.nexttoken
end

proc lexm=
!wrapper around lexreadtoken that applies macro expansion to names
	ref strec d
	static int doreset=0
	int newlineno

	do
		if tkptr then
			nextlx:=tkptr^
			tkptr:=tkptr.nexttoken
			if tkptr=nil then

				if nextlx.symbol=namesym and nextlx.symptr.nameid=macroid and peeklb() then
!fix pp bug: macro expansion ending with fn-macro name, with (...) following
!but at normal lexical level. Pick that up here
					setfileno(sfileno)
					nextlx.lineno:=slineno
					doreset:=0
					goto TEST1
				fi
				doreset:=1

			fi
			return
		fi

		if doreset then
			setfileno(sfileno)
			nextlx.lineno:=slineno
			doreset:=0
		fi

		if firstsymbol then
			firstsymbol:=0
			dospecialinclude()
		fi	
		lexreadtoken()
	TEST1:

		case nextlx.symbol
		when lexhashsym then

			if dolexdirective() then
				return
			fi

			nextloop
		when namesym then
			d:=nextlx.symptr
			case d.symbol
			when predefmacrosym then

				sfileno:=getfileno()
				slineno:=nextlx.lineno
				expandpredefmacro(d.subcode,&nextlx,slineno)
				doreset:=1					!can screw up line/file numbers
				return
			else
				if d.nameid<>macroid or noexpand then
					return
				fi
			esac
		else
			return
		esac
!have a macro. Now see whether this should be expanded
		sfileno:=getfileno()
		slineno:=nextlx.lineno
		if d.flmacro then		!function-like macro; need to peek for "("
			if not peeklb() then
				return
			fi
			tkptr:=expandfnmacro(d,nil,normaltk,1,newlineno)
			slineno:=newlineno
		else										!object-like macro: always expand
			tkptr:=expandobjmacro(d,nil,normaltk,1)
		fi

		if tkptr=nil then doreset:=1 fi			!immediately restore file/lineno

	od
end

function peeklb:int=
!look at lxsptr seqence and return 1 if the next token is a "(" left bracket
!lxsptr is left unchanged whatever the result
!only a simplistic approach is used, eg. 0 or 1 space then a "(" must be nextloop
!In theory, there could be any number and combination of spaces, tabs, newlines,
!comments, strings, #-directives between this point and the next token, or
!it could be inside the next #include or just outside this one.
	if lxsptr^='(' or (lxsptr^=' ' and (lxsptr+1)^='(') then
		return 1
	fi
	return 0
end

function peektk(ref tokenrec tk)int=
!version of peeklb that works on a token list rather than chars
!tk is the current token
	tk:=tk.nexttoken
	if tk=nil then			!nothing follows
		return 0
	fi
	if tk.symbol=lbracksym then
		return 1
	fi
	return 0
end

function expandobjmacro(ref strec m,ref tokenrec macrostack, &tksource,
		int frombaselevel)ref tokenrec=
	ref tokenrec tk,p,repl
	tokenrec newmacro
	int iscomplex,useshh,expanded
	ref strec d

	p:=tk:=m.tokenlist

	iscomplex:=useshh:=0
	while p do
		if p.symbol=namesym then
			d:=p.symptr
			if d.nameid=macroid or d.symbol=predefmacrosym then
				iscomplex:=1
				exit
			fi
		elsif p.symbol=hashhashsym then
			iscomplex:=useshh:=1
			exit
		fi

		p:=p.nexttoken
	od

	if not iscomplex then
		return tk
	fi

	newmacro.symptr:=m				!add m to macrostack
	newmacro.nexttoken:=macrostack

	if useshh then
		repl:=substituteargs(m,nil,nil,0,nil)
	else
		repl:=m.tokenlist
	fi

	tk:=scantokenseq(repl,&newmacro,expanded)
	return tk
end

function expandfnmacro(ref strec m, ref tokenrec macrostack, &tksource,
		int frombaselevel, &endlineno)ref tokenrec=
!positioned just before "(" token
!read arguments from source (need to use lexm(), reading from char-sourc or tokenlist)
!store args in special arg lists, and prepare args for expansion
!(for this version, args expanded on demand only)
!get tokenlist for m, do argument substitution, then scan it looking for new
!macros to expand
	[maxmacroargs]ref tokenrec args,expargs
	ref tokenrec repl,tk
	tokenrec newmacro
	int nargs,i,expanded

	nargs:=readmacrocall(m,&args,tksource)
	if frombaselevel then
		endlineno:=nextlx.lineno
	fi

	for i:=1 to nargs do
		expargs[i]:=nil
	od

	repl:=substituteargs(m,&args,&expargs,nargs,macrostack)

	newmacro.symptr:=m				!add m to macrostack
	newmacro.nexttoken:=macrostack

	repl:=scantokenseq(repl,&newmacro,expanded)
	return repl
end

function scantokenseq(ref tokenrec tk, macrostack,int &expanded)ref tokenrec=
!scan token sequence belonging to:
! The replacelist of an object macro
! The substituted replacement list of a function macro
! An argument of a macro
!scan object macro, but can also be an argument

!d is an object macro that may contains further macro definitions
!scan it, and produce a new tokenlist that contains expanded versions
!of nested macro calls
!macrostack is a list of active nested macro defs. This is stored as
!a linked list of tokenrec records, in reverse order. This is just for
!convenience; the .symptr field is used to refer to the macro st entry

	ref tokenrec newtk,newtkx	!new list of tokens
	ref tokenrec expandtk		!token seqence from expanding a macro
	ref tokenrec oldtk
	ref strec m
	tokenrec newmacro
	int noexpandflag,simple,dummy

	reenter:
	expanded:=0

	newtk:=newtkx:=nil
	noexpandflag:=0

	simple:=1
	oldtk:=tk

	while tk do
		case tk.symbol
		when namesym then
			if tk.symptr.nameid=macroid or tk.symptr.symbol=predefmacrosym then
				simple:=0
				exit
			fi
		esac

		if tk=nil then exit fi
		tk:=tk.nexttoken
	od

	if simple then
		return oldtk
	fi

	tk:=oldtk
	while tk do
		case tk.symbol
		when namesym then
			m:=tk.symptr
			if m.nameid=macroid and not noexpandflag then
!macro detected; check if candidate for expansion
				if tk.flags iand tk_macrolit or noexpand then
					goto simpletoken
				fi

				if inmacrostack(m,macrostack) then		!is an active macro name
					addlisttoken_copy(&newtk,&newtkx,tk)
					newtkx.flags ior:= tk_macrolit
					goto skip

				fi
	simple:=0
				if m.flmacro then
					if not peektk(tk) then goto simpletoken fi
					lexa(tk)
					expandtk:=expandfnmacro(m,macrostack,tk,1,dummy)
					addlisttoken_seq(&newtk,&newtkx,expandtk)
					expanded:=1
					nextloop
				else
					expandtk:=expandobjmacro(m,macrostack,tk,0)
					expanded:=1
					addlisttoken_seq(&newtk,&newtkx,expandtk)
				fi
			elsif m.symbol=kdefinedsym then
				noexpandflag:=1
				goto simpletoken
			elsif m.symbol=predefmacrosym then
				expandtk:=alloctokenz()
				expandpredefmacro(m.subcode,expandtk,slineno)
				addlisttoken_copy(&newtk,&newtkx,expandtk)
				goto skip2
			else
				noexpandflag:=0
				goto simpletoken
			fi
		else
	simpletoken:
			addlisttoken_copy(&newtk,&newtkx,tk)
		esac

	skip:
		if tk=nil then exit fi
	skip2:
		tk:=tk.nexttoken
	od

	if expanded then
		tk:=newtk
		goto reenter
	fi

	return newtk
end

function readmacrocall(ref strec d, ref[]ref tokenrec args, ref tokenrec &tksource)int=
!positioned just before "(" of a macro call
!read arguments for the macro, and store into args
!return total number of arguments
!each args^[i] entry is a list of tokenrecs
!Caller has already checked that "(" is next token, and this will be a function macro
!tksource will point to an input stream of tokens, but can also be nil, meaning
!read via lexm from actual source. (tksource can't be nil because it's at the
!end of ...

	int nparams,lbcount,paramno
	int nargs,usesvargs,varg
	ref tokenrec tklist,tklistx			!form list of tokens for argument

	lexa(tksource)

	if nextlx.symbol<>lbracksym then lxerror("rmc: no '('") fi

	nparams:=d.nparams
	nargs:=0
	if nparams=0 then				!) must follow
		lexa(tksource)
		if nextlx.symbol<>rbracksym then lxerror("rmc: ')' expected") fi
		return 0					!no args
	fi

	paramno:=1
	lbcount:=1
	tklist:=tklistx:=nil
	usesvargs:=d.varparams			!whether macro contains ... va/args
	varg:=0										!whether encountered ... yet in arguments

	do
		if paramno=nparams and usesvargs then varg:=1 fi
		lexa(tksource)

		case nextlx.symbol
		when commasym then
			if lbcount=1 and not varg then
				if tklist=nil then					!empty list: create place-holder token
					tklist:=alloctokenz()
					setfilenox(tklist,getfileno())
					tklist.symbol:=placeholdersym
				fi
				args^[paramno]:=tklist				!store this list
				tklist:=tklistx:=nil
				++paramno
			else
				goto addtoken
			fi

		when eofsym then
			lxerror("EOS in macro call")
		when lbracksym then
			++lbcount
			goto addtoken
		when rbracksym then
			if lbcount>1 then
				--lbcount
				addlist_nextlx(&tklist,&tklistx)
			else
				if tklist=nil then
					tklist:=alloctokenz()
					setfilenox(tklist,getfileno())
					tklist.symbol:=placeholdersym
				fi
				args^[paramno]:=tklist				!store this list
				exit
			fi
		else
	addtoken:
			addlist_nextlx(&tklist,&tklistx)
		esac
	od

	if paramno<>nparams then
		if paramno+1=nparams and usesvargs then		!no args for ... part, needs dummy arg
			args^[nparams]:=nil
		else
			lxerror("Wrong # macro params")
		fi
	fi
	return nparams
end

function substituteargs(ref strec m,ref[]ref tokenrec args,expargs, int nargs,
ref tokenrec macrostack)ref tokenrec=
!m is a macro def
!args/expargs are arguments that will replace any parameter names encountered
!in m's replacement list
!returns new replacement list with arguments inserted
	ref mparamrec params
	ref tokenrec seq,seqstart,lasttoken
	ref tokenrec newtk,newtkx,niltk,tkexp
	tokenrec tk
	int n,i,expanded

	const maxhashhash=250
	[maxhashhash]ref tokenrec hhpoints
	int nhashhash

	params:=m.mparamlist
	seq:=seqstart:=m.tokenlist		!input token sequence

	newtk:=newtkx:=nil				!output token sequence
	nhashhash:=0
	lasttoken:=nil

	while seq do
		case seq.symbol
		when hashsym then
			if nargs then
				seq:=seq.nexttoken
				if seq=nil then lxerror("# at end") fi
				unless seq.flags iand tk_parammask then
					lxerror("# not followed by param")
				end unless
				n:=seq.paramno

				stringify(args^[n],&tk)

				addlisttoken_copy(&newtk,&newtkx,&tk)
			else
				addlisttoken(&newtk,&newtkx,seq)
				newtkx.symbol:=lithashsym				!change to #'
			fi
		when hashhashsym then
			if seq=seqstart then lxerror("## at start") fi
			if nhashhash>=maxhashhash then lxerror("Too many ##") fi
			hhpoints[++nhashhash]:=newtkx

		elsif seq.symbol=namesym and seq.flags iand tk_parammask and nargs then		!args can be () if no "(...)" followed
			n:=seq.paramno
			if seq.nexttoken and seq.nexttoken.symbol=hashhashsym or
			   lasttoken and lasttoken.symbol=hashhashsym then
				addlisttoken_seq(&newtk,&newtkx,args^[n])
			else
				tkexp:=expargs^[n]
				if tkexp=nil then
					tkexp:=expargs^[n]:=scantokenseq(args^[n],macrostack,expanded)
				fi
				addlisttoken_seq(&newtk,&newtkx,tkexp)
			fi

		else
	doother:
			addlisttoken_copy(&newtk,&newtkx,seq)
		esac

		lasttoken:=seq
		seq:=seq.nexttoken
	od

	if nhashhash then
		niltk:=nil
		for i:=1 to nhashhash do
			pastetokens(hhpoints[i],(i<nhashhash | hhpoints[i+1]| niltk))
		od
	fi

	return newtk
end

function strtoken(ref tokenrec lp,int &length)ichar=
!convert token to a string
!return pointer to the string *which is likely to be unterminated*
!return length of the string in 'length'
!(not sure yet if -1 is a possible length, meaning the string is zero-terminated)
!display token contents naturally
!note that caller should copy the string involved as no promises can be 
!made to ownership
	ichar name,s
	tokenrec l
	l:=lp^

	case l.symbol
	when namesym then
	doname:
		length:=l.symptr.namelen
		return l.symptr.name

	when intconstsym,realconstsym then
		length:=l.length


		if getfilenox(&l) then
			return sourcefiletext[getfilenox(&l)]+getnumberoffsetx(&l)
		else
			return pastedtokenlist[l.pasteno]
		fi
	when rawnumbersym then
		length:=l.length
		return l.svalue

	when stringconstsym,wstringconstsym then
		s:=strstring(l.svalue,l.length,length,'"')
		return s

	when charconstsym then
		s:=strstring(l.svalue,l.length,length,'\'')
		return s

	when eolsym then
		if dowhitespace then
			length:=l.length+1
			s:=pcm_alloc(length)
			s^:=10		!'\n'
			memcpy(s+1,l.svalue,l.length)
		else
			length:=1
			return "\n"
		fi
		return s

	when eofsym then
		length:=0
		return ""

	when ktypespecsym, ktypequalsym, klinkagesym, kfnspecsym then
		goto doname

	else
		name:=shortsymbolnames[l.symbol]
		if length:=strlen(name) then
			if name^<>'k' then
				return name
			else
				length:=strlen(symbolnames[l.symbol]+1)
				return symbolnames[l.symbol]+1
			fi
		else
			return ""
		fi
	esac
	return ""
end

function strstring(ichar s,int length,&newlength,quotechar)ichar=
!stringify the string, which means converting control codes to
!escape sequences, and adding optional quotes

	ichar t,u

	t:=u:=pcm_alloc(length*2+4)
	if quotechar then
		u^:=quotechar
		++u
	fi
	convertstring(s,u,length)
	newlength:=strlen(t)
	if quotechar then
		(t+newlength)^:=quotechar
		++newlength
	fi
	return t
end

int lasttoken=0

global proc emittoken(ref tokenrec lp,ref strbuffer dest,int forcespace=0)=
!display token contents naturally
	int length
	ichar s

	if lp.symbol=eolsym and lasttoken=eolsym then
		return
	fi

	s:=strtoken(lp,length)

	if forcespace or needspace(lasttoken,lp.symbol) then
		gs_char(dest,' ')
	fi

	gs_strn(dest,s,length)


	lasttoken:=lp.symbol
end

global proc showtoken(ref tokenrec lp)=
	static strbuffer buffer
	static ref strbuffer dest=&buffer

	gs_init(dest)
	
	emittoken(lp,dest)
	
print dest.length:"v",,dest.strptr:".*"
end

proc stringify(ref tokenrec seq,dest)=
!stringify single or multiple token sequence, and store result as a single
!string token in dest
	ref char s
	int length,addspace
	static strbuffer buffer
	static ref strbuffer deststr=&buffer

	dest.symbol:=stringconstsym
	dest.nexttoken:=nil

	if seq.nexttoken=nil then		!single
		s:=strtoken(seq,length)
		dest.length:=length
		dest.svalue:=s
		return 
	fi

!now do multiple tokens into one string
	gs_init(deststr)
	lasttoken:=0
	addspace:=0
	while seq do
		emittoken(seq,deststr,forcespace:addspace)
		addspace:=1
		seq:=seq.nexttoken
	od

	dest.length:=length
	dest.svalue:=deststr.strptr
	dest.length:=deststr.length
end

proc pastetokens(ref tokenrec tk, &tknext)=
!tk points into a token sequence
!paste the token at tk with the one at tk.nexttoken, and replace
!tk with the new composite token; tk.nexttoken is removed
!tknext is either nil, or refers to the next pair of tokens to be pasted together;
!there is a problem when tk.nexttoken and tknext coincide, so something needs to
!be done in that case (set tknext to point to tk)

	ref tokenrec tk2
	int length1,length2
	ref char s,t,u
	tokenrec oldtoken,token
	ref char oldlxsptr
	int oldlx_stackindex

	tk2:=tk.nexttoken
	if tk2=tknext then tknext:=tk fi
	tk.nexttoken:=tk2.nexttoken				!lose second token

	if tk.symbol=placeholdersym then
		if tk2.symbol=placeholdersym then			!two placeholders; leave only first
		else										!ph/token; use second
			tk^:=tk2^								!also unlinks the tk2 token
		fi
	elsif tk2.symbol=placeholdersym then			!token/ph; leave only first
	else						!two normal tokens

		s:=strtoken(tk,length1)
		t:=strtoken(tk2,length2)

		u:=pcm_alloc(length1+length2)
		memcpy(u,s,length1)
		memcpy(u+length1,t,length2)
		(u+length1+length2)^:=0

		if npastedtokens>=maxpastedtokens then
			lxerror("Too many pasted tokens")
		fi
		pastedtokenlist[++npastedtokens]:=u

		oldtoken:=nextlx
		oldlxsptr:=lxsptr
		oldlx_stackindex:=lx_stackindex

		lxsptr:=u
		lx_stackindex:=0

		setfileno(0)
		nextlx.lineno:=0
		lexreadtoken()
		token:=nextlx
		lexreadtoken()

		if nextlx.symbol<>eofsym then
!			lxerror("token-paste error")
		fi

		nextlx:=oldtoken
		lxsptr:=oldlxsptr
		lx_stackindex:=oldlx_stackindex

		token.nexttoken:=tk.nexttoken
		setfilenox(&token,0)
		token.pasteno:=npastedtokens

	token.flags ior:=tk_pasted
		tk^:=token
	fi
end

function getifexpr:int=
	int sx
	int x

	lexm()
	x:=evalcondexpr(sx)

	if nextlx.symbol<>eolsym then
		lxerror("#if:eol expected")
	fi

	return x<>0
end

function evalcondexpr(int &sx)i64=
!Main entry point for pp expressions
!Will do conditional ?: expressions here
!Positioned at first symbol of expression, which is in nextlx (if a macro
!it will have been expanded, and this is the first token of that expansion)
	i64 x,y,z
	int sy,sz

	x:=evalorexpr(sx)

	if nextlx.symbol=questionsym then
		lexm()
		y:=evalcondexpr(sy)
		if nextlx.symbol<>colonsym then lxerror(": expected") fi
		lexm()
		z:=evalcondexpr(sz)
		if x then
			sx:=sy
			x:=y
		else
			sx:=sz
			x:=z
		fi
	fi

	return x
end

function evalorexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalandexpr(sx)
	while nextlx.symbol=orlsym do
		lexm()
		y:=evalandexpr(sy)
		x := (x or y|1|0)
	od

	return x
end

function evalandexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaliorexpr(sx)
	while nextlx.symbol=andlsym do
		lexm()
		y:=evaliorexpr(sy)
		x := (x and y|1|0)
	od

	return x
end

function evaliorexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalixorexpr(sx)
	while nextlx.symbol=iorsym do
		lexm()
		x ior:= evalixorexpr(sy)
	od

	return x
end

function evalixorexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaliandexpr(sx)
	while nextlx.symbol=ixorsym do
		lexm()
		x ixor:= evaliandexpr(sy)
	od

	return x
end

function evaliandexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaleqexpr(sx)
	while nextlx.symbol=iandsym do
		lexm()
		x iand:= evaleqexpr(sy)
	od

	return x
end

function evaleqexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalcmpexpr(sx)
	while (opc:=nextlx.symbol)=eqsym or opc=nesym do
		lexm()
		y:=evalcmpexpr(sy)
		case opc
		when eqsym then x := x = y
		when nesym then x := x <> y
		esac
	od

	return x
end

function evalcmpexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalshiftexpr(sx)
	while (opc:=nextlx.symbol)=ltsym or opc=lesym or opc=gesym or opc=gtsym do
		lexm()
		y:=evalshiftexpr(sy)
		case opc
		when ltsym then x := x < y
		when lesym then x := x <= y
		when gesym then x := x >= y
		when gtsym then x := x > y
		esac
	od

	return x
end

function evalshiftexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaladdexpr(sx)
	while (opc:=nextlx.symbol)=shlsym or opc=shrsym do
		lexm()
		y:=evaladdexpr(sy)
		case opc
		when shrsym then
			x := x>>y
		when shlsym then
			x := x<<y
		esac
	od

	return x
end

function evaladdexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalmulexpr(sx)
	while (opc:=nextlx.symbol)=addsym or opc=subsym do
		lexm()
		y:=evalmulexpr(sy)
		case opc
		when addsym then
			x +:= y
		when subsym then
			x -:= y
		esac
	od

	return x
end

function evalmulexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalunaryexpr(sx)
	while (opc:=nextlx.symbol)=mulsym or opc=divsym or opc=remsym do
		lexm()
		y:=evalunaryexpr(sy)
		if y=0 and opc<>mulsym then lxerror("#if:div by zero") fi
		case opc
		when mulsym then
			x *:= y
		when divsym then
			x := x/y
		when remsym then
			x := x rem y
		esac
	od

	return x
end

function evalunaryexpr(int &sx)i64=
	i64 x
	int opc

	case nextlx.symbol
	when addsym, subsym, notlsym, inotsym then
		opc:=nextlx.symbol
		lexm()
		x:=evalunaryexpr(sx)
		case opc
		when addsym then
			return x
		when subsym then
			return -x
		when notlsym then
			return not x
		when inotsym then
			return inot x
		esac
	esac

	return evalterm(sx)
end

function evalterm(int &sx)i64=
	i64 res
	int lb

	sx:=1
	case nextlx.symbol
	when namesym then
		case nextlx.symptr.symbol
		when kdefinedsym then
			noexpand:=1
			lb:=0
			lexm()
			if nextlx.symbol=lbracksym then
				lb:=1;
				lexm()
			fi
			if nextlx.symbol<>namesym then lxerror("defined?") fi
			res:=nextlx.symptr.nameid=macroid
			lexm()
			if lb then
				if nextlx.symbol<>rbracksym then lxerror("')' expected") fi
				lexm()
			fi
			noexpand:=0
		when ksizeofsym then
			lexm()
			if nextlx.symbol<>lbracksym then lxerror("'(' expected") fi
			lexm()
			if nextlx.symbol<>namesym then lxerror("name expected") fi
			case nextlx.symptr.symbol
			when ktypespecsym then
				res:=typespecsizes[nextlx.symptr.subcode]
			else
				lxerror("sizeof2")
			esac
			lexm()
			if nextlx.symbol<>rbracksym then lxerror("')' expected") fi
			lexm()
	
		else
			lexm()
			return 0
		esac
	when intconstsym then
		res:=nextlx.value
		lexm()
	when charconstsym then
		if nextlx.length=0 then
			res:=0
		else
			res:=nextlx.svalue^
		fi
		lexm()
	when lbracksym then
		lexm()
		res:=evalcondexpr(sx)
		if nextlx.symbol<>rbracksym then
			lxerror(") expected")
		fi
		lexm()
	else
	printsymbol(&nextlx)
	printstrn(nextlx.svalue,nextlx.length); println
		lxerror("evalterm?")
	esac

	return res
end

function getifdef:int=
!just read ifdef/ifndef
!read following name and return 1 if name is macro, or 0 if not
	int res
	ref strec d

	noexpand:=1
	lexreadtoken()
	noexpand:=0
	if nextlx.symbol<>namesym then lxerror("Name expected") fi
	d:=nextlx.symptr
	res:=0
	if d.nameid=macroid then
		res:=1
	elsif d.symbol=predefmacrosym then
		res:=1
	fi

	lexreadtoken()
	if nextlx.symbol<>eolsym then lxerror("EOL expected") fi

	return res
end

function skipcode:int=
!skip false branch of #if etc until matching #elif/else/endif
!return dir-code of that closing directive
	int level,dir
	ref byte pp

	level:=0						!count nested #if levels

	do
		fastreadtoken()

		case nextlx.symbol
		when lexhashsym then
			dir:=getlexdirective()
			case dir
			when ifdir, ifdefdir, ifndefdir then
				++level
			when elifdir, elsedir then
				if level=0 then
					return dir
				fi
			when endifdir then
				if level=0 then
					return dir
				fi
				--level
			esac
		when eofsym then
			lxerror("#if:Unexpected eof")
		esac
	od
	return 0
end

global proc fastreadtoken=
!read next token into nextlx
	int c,csum,hsum,commentseen,dodir,j
	ref char pstart,p
	ichar ss

	doswitch c:=lxsptr++^
	when '#' then			!
		p:=lxsptr-2
		dodir:=0
		while p>=lxstart do
			case p^
			when lf then		!# is first thing on a line
				dodir:=1
				exit
			when tab,' ' then	!might have leading white space
			else
				exit			!assume different hash symbol
			esac
			--p
		od
		if dodir or p<lxstart then
			nextlx.symbol:=lexhashsym
		return

		elsif lxsptr^='#' then
			++lxsptr
		fi

	when '/' then
		case lxsptr^
		when '/' then					!comment to 
			readlinecomment()
		when '*' then
			readblockcomment()
		esac
	
	when '\'' then
		lxreadstring('\'',0)

	when '"' then
		lxreadstring('"',0)

	when cr then
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0
		++lxsptr				!skip lf
	when lf then			!only lfs not preceded by cr
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0

	when 0 then
		--lxsptr
		if lx_stackindex then
			unstacksourcefile()
		else
			nextlx.symbol:=eofsym
			return
		fi

	when 12 then
	else
	end doswitch
end

function alloctoken:ref tokenrec=
	ref tokenrec tk
	tk:=pcm_alloc(tokenrec.bytes)
	return tk
end

function alloctokenz:ref tokenrec=
	ref tokenrec tk
	tk:=pcm_alloc(tokenrec.bytes)
	tk.nexttoken:=nil
	return tk
end

proc expandpredefmacro(int pdmcode,ref tokenrec tk,int lineno)=
	[256]char str
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	rsystemtime tm
	ichar s
	int fileno

	if noexpand then
		return
	fi

	case pdmcode
	when pdm_date then
		os_getsystime(&tm)

		fprint @str, "#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

		tk.symbol:=stringconstsym
		tk.svalue:=pcm_copyheapstring(str)

	when pdm_time then
		os_getsystime(&tm)

		fprint @str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

		tk.symbol:=stringconstsym
		tk.svalue:=pcm_copyheapstring(str)
	when pdm_file then
		tk.symbol:=stringconstsym
		fileno:=getfilenox(tk)
		if fileno=0 then fileno:=sfileno fi
		if sfileno then
			tk.svalue:=sourcefilenames[sfileno]
		else
			tk.svalue:="(File not available)"
		fi
	when pdm_func then
		tk.symbol:=stringconstsym
		if currproc then
			tk.svalue:=currproc.name
		else
			tk.svalue:="???"
		fi
	when pdm_line then
		tk.symbol:=intconstsym
		tk.value:=lineno
	when pdm_stdc then
		tk.symbol:=intconstsym
		tk.value:=1
	when pdm_mcc then
		tk.symbol:=intconstsym
		tk.value:=1
	else
		println pdmcode
		lxerror("PDM")
	esac

	if tk.symbol=stringconstsym then
		tk.length:=strlen(tk.svalue)
		tk.subcode:=trefchar
	else
		tk.subcode:=ti32
		s:=pcm_alloc(16)
		getstrint(tk.value,s)
		tk.length:=strlen(s)
		if npastedtokens>=maxpastedtokens then
			lxerror("2:Too many pasted tokens")
		fi
		pastedtokenlist[++npastedtokens]:=s
		setfilenox(tk,0)
		tk.pasteno:=npastedtokens
	fi
end

proc dopragmadir=
	lexm()
	if nextlx.symbol=namesym then
		if memcmp(nextlx.symptr.name,"pack",4)=0 then
			lexm()
			if nextlx.symbol<>lbracksym then lxerror("'(' expected") fi
			lexm()
			if nextlx.symbol=intconstsym then
				case nextlx.value
				when 1 then
					structpadding:=0
				else
					goto finish
					lxerror("Only pack(1) or () allowed")
				esac
				lexm()
			elsif nextlx.symbol=rbracksym then
				structpadding:=1
			fi
		fi
	fi
finish:
	while nextlx.symbol<>eolsym and nextlx.symbol<>eofsym do lexm() od
end

function needspace(int a,b)int=
	ichar aname, bname

	if a=0 then return 0 fi			!first token

	aname:=shortsymbolnames[a]
	bname:=shortsymbolnames[b]

	case bname^
	when 'n','k' then
		case aname^
		when 'n','k' then
			return 1
		esac
	when '-','+' then
		case aname^
		when '-','+' then
			return 1
		esac
	esac

	return 0
end

global proc dospecialinclude=
	stacksourcefile(mcchdr,1)
	if dheaderfile then
		stacksourcefile(dheaderfile,1)
	fi
end

proc setnumberoffset(int offset)=
!store offset into nextlx.numberoffset
!except that top byte is msb of fileno
	nextlx.numberoffset:=(nextlx.numberoffset iand 0xFF000000) ior (offset iand 0xFFFFFF)
end

proc setfileno(int fileno)=
	nextlx.fileno:=fileno iand 255
	nextlx.numberoffset := (nextlx.numberoffset iand 0xFFFFFF) ior((fileno iand 0xFF00)<<16)
end

proc setfilenox(ref tokenrec tk,int fileno)=

	tk.fileno:=fileno iand 255
	tk.numberoffset := (tk.numberoffset iand 0xFFFFFF) ior (fileno iand 0xFF00)<<16
end

global function getfileno:int=
	return (nextlx.numberoffset>>24)<<8 ior nextlx.fileno
end

global function getfilenox(ref tokenrec tk)int=
	return (tk.numberoffset>>24)<<8 ior tk.fileno
end

global function getnumberoffsetx(ref tokenrec tk)int=
	return tk.numberoffset iand 0xFFFFFF
end

proc readrealnumber(ref char pstart,intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, (or to "." if there was no prefix, then intlen=0)
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
	ref char fractstart
	int fractlen,expon,i,c,badexpon
	real basex,x,expbase
	const maxrealdigits=500
	[maxrealdigits]char realstr

	fractstart:=nil
	fractlen:=0
	expon:=0

	if lxsptr^='.' then		!read
		fractstart:=++lxsptr
		fractlen:=scannumber(base)-fractstart
	fi
	badexpon:=0

	case lxsptr^
	when 'e','E' then
		if base<>16 then
			++lxsptr
			expon:=readexponent(badexpon)
		fi
	when 'p','P' then
		if base=16 then
			++lxsptr
			expon:=readexponent(badexpon)
		fi
	esac

	if badexpon then
		--lxsptr
		readalphanumeric(pstart)
		return
	fi

	case lxsptr^
	when 'f','F','l','L' then
		++lxsptr
	else
		if alphamap[lxsptr^] then
			readalphanumeric(pstart)
			return
		fi
	esac

	if intlen+fractlen>maxrealdigits then
		lxerror("Real too long")
	fi
	if intlen then
		memcpy(&realstr,intstart,intlen)
	fi
	if fractlen then
		memcpy(&realstr[1]+intlen,fractstart,fractlen)
	fi

	expbase:=basex:=base

	if base=10 then
		expon-:=fractlen
	else
		expon-:=fractlen*4				!each hex digit is 4 binary bits
		expbase:=2.0
	fi

	x:=0.0

	for i:=1 to intlen+fractlen do		!digits already range-checked
		c:=realstr[i]
		if c>='0' and c<='9' then
			x:=x*basex+(c-'0')
		elsif c>'a' then
			x:=x*basex+c-'a'+10
		else
			x:=x*basex+c-'A'+10
		fi
	od

	if expon>=0 then
		to expon do
			x*:=expbase
		od
	else
		to -expon do
			x/:=expbase
		od
	fi

	nextlx.symbol:=realconstsym
	nextlx.subcode:=tr64
	nextlx.xvalue:=x

	setnumberoffset(intstart-lxstart)
	nextlx.length:=lxsptr-intstart
end

=== cc_lib.m 0 0 7/24 ===
global int autotypeno=0
global int nextafindex=0

function newstrec:ref strec=
	ref strec p
	p:=malloc(strec.bytes)
	clear p^

	p.lineno:=lx.lineno
	p.fileno:=lx.fileno

	return p
end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=jname
	u.def:=p

	return u
end

global function createunit0(int tag)ref unitrec=
	createunit3(tag, nil, nil, nil)
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
	createunit3(tag, p, nil, nil)
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
	createunit3(tag, p, q, nil)
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	u.b:=q
	u.c:=r
	return u
end

global function createconstunit(u64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.value:=a
	u.mode:=t
	return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
!for zero-terminated, length does not include terminator
!.slength will include it
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.svalue:=s
	u.mode:=trefchar
	if length=-1 then
		u.slength:=strlen(s)+1
	else
		u.slength:=length+1
	fi
	u.isstrconst:=1
	return u
end

global function createwstringconstunit(ref u16 s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.wsvalue:=s
	u.mode:=trefwchar
	u.wslength:=length+1
	u.iswstrconst:=1
	return u
end

global function nextautotype:ichar=
	static [32]char str

	print @str,"$T",,++autotypeno
	return str
end

global function createconstmode(int m)int=
!create const version of mode m
	int newm
	if ttconst[m] then return m fi
	if ttconsttype[m] then return ttconsttype[m] fi
	newm:=copymode(m)
	ttconsttype[m]:=newm
	ttconst[newm]:=1

	ttconsttype[newm]:=m			!use consttype to point back as well as forwards

	return newm
end

global function createrefmode(int m)int=
!create ref version of mode m (including when m is already a ref)
	int newm

	if ttreftype[m] then
		return ttreftype[m]
	fi
	newm:=createnewmode(tref)
	ttreftype[m]:=newm
	tttarget[newm]:=m
	ttisref[newm]:=1
!	ttcat[newm]:=d64cat

	return newm
end

global function createprocmode(int m, ref paramrec pm)int=
!create proc mode with return type
	int newm

	newm:=createnewmode(tproc)
	ttparams[newm]:=pm
	tttarget[newm]:=m
!	ttcat[newm]:=d64cat
	return newm
end

global function createarraymode(int m, length)int=
!create array of mode m (including when m is already a ref)
	int newm

	newm:=createnewmode(tarray)
	tttarget[newm]:=m
	ttlength[newm]:=length
	ttsize[newm]:=length*ttsize[m]
!	ttcat[newm]:=blockcat

	return newm
end

global function createenummode(ref strec e)int=
	int newm
	newm:=createnewmode(tenum)
	ttnamedef[newm]:=e

	return newm
end

global function createstructmode(ref strec s,int smode)int=
	int newm
	newm:=createnewmode(smode)
	ttnamedef[newm]:=s

	return newm
end

global function getautofieldname:ref strec=
!create auto-field name and return pointer to st entry
	[32]char str
	ichar name

	print @str,"$F",,++nextafindex

	name:=pcm_copyheapstring(str)
	return addnamestr(name)
end

global function strmode(int m,expand=1)ichar=		!STRMODE
	static [16384]char str

	istrmode(m,expand,str)

	return str
end

global proc istrmode(int m,expand=1,ichar dest)=		!ISTRMODE
	ref strec d,q
	int value,needcomma,x,i,target,t,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim,slength
	[100]char strdim,strlength
	ref paramrec pm

	if m<tlast then
		strcpy(dest,typename(m))
		return
	fi

	t:=ttbasetype[m]

	case t
	when tref then
		if ttconst[m] then
			strcpy(dest,"const ref ")
		else
			strcpy(dest,"ref ")
		fi
		target:=tttarget[m]
		if target>=0 and ttbasetype[tttarget[m]]=tstruct then
			strcat(dest,typename(tttarget[m]))
		else
			istrmode(tttarget[m],0,dest+strlen(dest))
		fi
	when tarray then
		if ttlength[m] then
			fprint @dest,"[#]",ttlength[m]
		else
			strcpy(dest,"[]")
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tenum then
		strcpy(dest,"enum ")
		strcat(dest,typename(m))

	when tstruct,tunion then
		if not expand then
			strcpy(dest,typename(m))
			return
		fi

		strcpy(dest,typename(ttbasetype[m]))
!		strcat(dest,"(")
		strcat(dest,"(....")
!		d:=ttnamedef[m]
!		needcomma:=0
!
!		q:=d.deflist
!		while q do
!			if needcomma then strcat(dest,",") fi
!			needcomma:=1
!			istrmode(q.mode,0,dest+strlen(dest))
!			strcat(dest," ")
!			strcat(dest,q.name)
!			q:=q.nextdef
!		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,typename(m))

	when tproc then
!		strcpy(dest,"proc(")
		strcpy(dest,"proc(....")
!		pm:=ttparams[m]
!		n:=pm.nparams
!		for i to n do
!			istrmode(pm.mode,0,dest+strlen(dest))
!			if i<>n then
!				strcat(dest,",")
!			fi
!			pm:=pm.nextparam
!		od
		strcat(dest,")")
		istrmode(tttarget[m],0,dest+strlen(dest))

	elsif t<tlast then
		strcpy(dest,typename(m))
		return
	else
	CPL typename(m)
		mcerror("NEWSTRMODE")
	esac
end

global function typename(int m)ichar=
	int basem
	static [300]char str

	basem:=ttbasetype[m]
	case basem
	when tstruct,tunion then
		strcpy(str,(basem=tstruct|"struct "|"union "))
		if ttnamedef[m] then
			strcat(str,ttnamedef[m].name)
			strcat(str,".")
			strcat(str,strint(ttnamedef[m].blockno))
		fi
		return str
	when tarray then
		return "<array>"
	when tenum then
		if ttnamedef[m] then
			return ttnamedef[m].name
		fi
		return "<enum>"
	else
		if ttconst[m] then
			strcpy(str,"const ")
			strcat(str,stdtypenames[basem])
			return str
		fi
		return stdtypenames[basem]
	esac
	return ""
end

global function allocunitrec:ref unitrec p=

	p:=malloc(unitrec.bytes)
	memset(p, 0, unitrec.bytes)
	p.lineno:=lx.lineno
	if lx.fileno<=255 then
		p.fileno:=lx.fileno
	fi
	return p
end

function copymode(int m)int=
	if ntypes>=maxtype then
		serror("Too many types")
	fi
	++ntypes

!copy fields that won't already be zero
	ttnamedef[ntypes]:=ttnamedef[m]
	ttbasetype[ntypes]:=ttbasetype[m]
	ttlength[ntypes]:=ttlength[m]
	ttconst[ntypes]:=ttconst[m]
	ttsize[ntypes]:=ttsize[m]
	tttarget[ntypes]:=tttarget[m]
	ttparams[ntypes]:=ttparams[m]
	ttisref[ntypes]:=ttisref[m]

	return ntypes
end

function createnewmode(int m)int=
!create new type unitialised except for given basetype m

	if ntypes>=maxtype then
		CPL =STRMODE(M)
		serror("Too many types/cnm")
	fi
	++ntypes

!leave length, const etc all zero
!copy basic size info from basetype

	ttbasetype[ntypes]:=m
	ttsize[ntypes]:=ttsize[m]

	return ntypes
end

global proc addlistunit(ref ref unitrec ulist,ulistx,ref unitrec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextunit:=p
	fi
	p.nextunit:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc addlistdef(ref ref strec ulist,ulistx,ref strec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextdef:=p
	fi
	p.nextdef:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc addlistparam(ref ref paramrec ulist,ulistx,ref paramrec p)=
!add paramrec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextparam:=p
	fi
	p.nextparam:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc checksymbol(int symbol)=
	[256]char str

	if lx.symbol<>symbol then
		fprint @str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]

	if lx.symbol=namesym then
		strcat(str," \"")
		strcat(str,getstname(lx.symptr))
		strcat(str,"\"")
	fi
		serror(str)
	fi
end

global proc skipsymbol(int symbol)=
	if lx.symbol<>symbol then checksymbol(symbol) fi
	lex()
end

global proc inittypetables=
	int i,j,size,bitsize,s,t,u

!Initialise type tt-tables from std types first all fields initially zero

	for i:=0 to tlast-1 do
		ttbasetype[i]:=i

		bitsize:=stdtypewidths[i]
		size:=bitsize/8

		ttsize[i]:=size
!		ttcat[i]:=stdcat[i]

	od
	ntypes:=tlast-1

	trefchar:=createrefmode(ti8)

	trefwchar:=createrefmode(tu16)

!do dominant table
	for i:=1 to dominantsetuptable.len do
		s:=dominantsetuptable[i,1]
		t:=dominantsetuptable[i,2]
		u:=dominantsetuptable[i,3]
		dominantmode[s,t]:=u
	od

!do conversion table
	for i:=1 to convsetuptable.len do
		s:=convsetuptable[i,1]
		t:=convsetuptable[i,2]
		u:=convsetuptable[i,3]
		conversionops[s,t]:=u
	od
end

global function createdupldef(ref strec owner,symptr, int id)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
	ref strec p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id
	p.namespace:=namespaces[id]
	if q:=symptr.nextdupl then			!1st in dupl list
		q.prevdupl:=p
	fi
	p.nextdupl:=q
	p.prevdupl:=symptr
	symptr.nextdupl:=p

	if owner then
		if owner.deflist=nil then			!first def
			owner.deflist:=owner.deflistx:=p
		else
			owner.deflistx.nextdef:=p
			owner.deflistx:=p
		fi
	fi

	return p
end

global function createnewmoduledef(ref strec owner,symptr)ref strec=
	ref strec p,q

	p:=createdupldef(owner,symptr,moduleid)
	return p
end

global function resolvename(ref strec owner, symptr, int ns, blockno)ref strec=
!symptr is a generic st entry for a name
!owner is the st entry where the name has been encountered (the current
! module, or a function)
!ns is code of the namespace that is being searched
!blockno is zero, if searched at file scope, or non-zero if searching
!from inside a function. Then, it will be the current block number
!where the name has been encountered
!Search the symbol table (usually the dupl list for symptr) for
!any instance of the name which matches in owner, matches the
!namespace, and is within the blockno hierarchy
!return symptr of the st entry when resolved, or nil if not resolved
	int nsblock
	ref strec d

	if symptr.nameid>macroid then
		return symptr
	fi

	if ns=ns_labels then
		return resolvelabel(owner,symptr)
	fi

	if blockno and blockcounts[blockno]=0 then blockno:=blockowner[blockno] fi

	do							!loop for each block level
		nsblock:=ns<<16 ior blockno
		d:=symptr				!reset dupl list
		while d:=d.nextdupl do
			if owner.nameid=procid  and d.owner<>owner and d.owner.nameid=procid then
				exit
			fi
			if d.owner=owner and d.nsblock=nsblock then
!				d.used:=1
				if d.used<255 then ++d.used fi

				return d
			fi
		od

		if blockno=0 then
			case owner.nameid
			when procid then			!was in function, now search filescope
					!(THIS MIGHT BE NEEDED FOR PARAM-SCOPES where block number is zero)
				owner:=stmodule
				redoloop
			when structtagid then		!was in struct; now try owner (proc/module/other struct)
				owner:=owner.owner
				if owner=nil then		!not sure if possible, but just in case...
					return nil
				fi
			else
				return nil
			esac
		elsif (blockno:=blockowner[blockno])=0 then		!try next block level
			owner:=stmodule				!block 0 means outside outer block, so switch to module scope
		fi

	od

	return nil
end

global function resolvelabel(ref strec owner, symptr)ref strec=
		ref strec d
		d:=symptr				!reset dupl list
		while d:=d.nextdupl do
			if owner.nameid=procid  and d.owner<>owner and d.owner.nameid=procid then
				exit
			fi

			if d.owner=owner and d.namespace=ns_labels then
				return d
			fi
		od

		return nil
end

global function checkdupl(ref strec owner, symptr, int ns, blockno)ref strec=
!Same params as resolvename.
!But here, search only current scope level to see if something of the
!same name, and in the same namespace, already exists
!Returns nil, if such a name was not found, or a symptr to it
!A returned symbol might be of a different nameid, but that would
!be an error usually as you can't have two identical names in the same namespace.
!Some kinds of names can have repeated declarations in the same scope
	int nsblock
	ref strec d

	d:=symptr

	nsblock:=ns<<16 ior blockno

	while d:=d.nextdupl do
		if d.owner=owner and d.nsblock=nsblock then
			return d
		fi
	od

	return nil
end

global function checkdupl_inproc(ref strec owner, symptr, int ns, blockno)ref strec=
!special version of checkdupl
!assumes that dupl list starts at last proc

	int nsblock
	ref strec d

	d:=symptr

	nsblock:=ns<<16 ior blockno

	while (d:=d.nextdupl) and d.owner=owner do
		if d.nsblock=nsblock then
			return d
		fi
	od

	return nil
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when tstruct,tunion then

		a:=ttnamedef[m].align
		if a=0 then
!		CPL("GETALIGN 0")
			RETURN 16
!		SERROR("GETALIGN 0")
		fi
		return a
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	esac
	cpl strmode(m),A
	serror("GETALIGN SIZE NOT 1248")

	return 0
end

global function getstname(ref strec d)ichar=
	static [256]char name
	memcpy(name,d.name,d.namelen)
	name[d.namelen+1]:=0
	return name
end

global function isrealcc(int m)int=
	m:=ttbasetype[m]
	return tfirstreal<=m<=tlastreal
!return tfirstreal<=m and m<=tlastreal
end

global function isintcc(int m)int=
	m:=ttbasetype[m]
	return tfirstint<=m<=tlastint
end

global func getpclmode(int t)int u=
	u:=stdpcl[ttbasetype[t]]

	if u=tpblock then
		case ttsize[t]
		when 8 then u:=tpu64
		when 4 then u:=tpu32
		when 2 then u:=tpu16
		when 1 then u:=tpu8
		esac
	fi
	return u
end

global proc addtolog(ichar filename, filehandle logdest)=
	filehandle f
	int c

	f:=fopen(filename,"rb")

	if f=nil then
 CPL "ATL ERROR",FILENAME; return fi

	do
		c:=fgetc(f)
		exit when c=c_eof
		fputc(c,logdest)
	od
	fclose(f)
end
=== cc_libpcl.m 0 0 8/24 ===
global function getpsymbol(symbol d)psymbol p=
	symbol e
	ichar name
	[256]char str


	return nil when d=nil

	if d.pdef then return d.pdef fi

	name:=d.name

	if d.nameid in [frameid, paramid] then
		strcpy(str, d.name)
		if d.blockno>1 then
			strcat(str, ".")
			strcat(str, strint(d.blockno))
		fi
	elsif d.nameid=staticid and d.owner and d.owner.nameid=procid then
		strcpy(str, d.owner.name)
		strcat(str, ".")
		strcat(str, d.name)
		if d.blockno>1 then
			strcat(str, ".")
        	strcat(str, strint(d.blockno))
		fi
	else
		strcpy(str, d.name)
	fi


	d.pdef:=p:=pc_makesymbol(str, name2pid[d.nameid])

	p.mode:=getpclmode(d.mode)
	p.size:=ttsize[d.mode]

	if d.owner and d.owner.owner then
		p.owner:=getpsymbol(d.owner)
	fi

	if d.scope=exported_scope then p.exported:=1 fi
	if d.exported then p.dllexport:=1 fi
	if d.scope=imported_scope then p.imported:=1; p.id:=import_id fi
	p.used:=d.used

	p.labelno:=d.index

	if d.nameid=procid and eqstring(d.name, "main") then
		d.ismain:=p.ismain:=1
	fi

	return p
end

global proc setmode(int mode)=
	pc_setmode(getpclmode(mode), ttsize[mode])
end

global proc setmode2(int mode)=
	pc_setmode2(getpclmode(mode))
end

global proc setmode_u(unit p)=
	int mode:=p.mode

	pc_setmode(getpclmode(mode), ttsize[mode])
end

global func genmem_d(symbol d)pcl=
	return genmem(getpsymbol(d))
end

global func genmemaddr_d(symbol d)pcl=
	return genmemaddr(getpsymbol(d))
end

global func definelabel:int =
	pc_gen(klabel,genlabel(++mlabelno))
	return mlabelno
end

global func createfwdlabel:int =
	return ++mlabelno
end

global proc definefwdlabel(int lab) =
	pc_gen(klabel,genlabel(lab))
end

=== cc_parse.m 0 0 9/24 ===
!Parse C Code

!const needcompoundblock=1
const needcompoundblock=0

ref strec ist_symptr


const maxtypemods=32
[maxnestedloops]byte looptypestack		!contains either 'L' or 'S' (loop or switch)
int loopindex							!current level of nested loop/switch blocks
[maxnestedloops]ref caserec casevaluestack		!linked list of case values for current switch

byte ingeneric=0

proc readmodule=
	int linkage,m,mbase,commaseen,wasdef
	unit p
	ref strec d
	ref paramrec pm
	int t,nitems,wasenum, exported

	while lx.symbol<>eofsym do
		nitems:=0
		case lx.symbol
		when semisym then
			serror("Extra semicolon 2")
		esac
		wasenum:=lx.symbol
		exported:=0

		if lx.symbol=kdeclspecsym then
			exported:=readdllexport()
		fi

		mbase:=readdeclspec(stmodule,linkage)

		if lx.symbol=kdeclspecsym then
			exported:=readdllexport()
		fi

		commaseen:=0

		docase lx.symbol
		when namesym, mulsym, lbracksym then
			++nitems

			m:=readtype(stmodule,d,mbase,pm)

			if d=nil then
				serror("Var name expected")
			fi

			if linkage=typedef_ss then
				if pm then
					m:=createprocmode(m,pm)
				fi
				d:=createtypedef(stmodule,d,m)

			elsif pm then
	readfn:
				if lx.symbol=lcurlysym and commaseen then serror("fn def after comma") fi

				d:=readfunction(d,m,linkage,pm,wasdef, exported)
				if wasdef then exit fi			!can't have comma-separate fn defs

			elsif ttbasetype[m]=tproc then
				pm:=ttparams[m]
				m:=tttarget[m]
				goto readfn

			else
				d:=readmodulevar(d,m,linkage)
			fi

			case lx.symbol
			when commasym then			!read next item
				commaseen:=1
				lex()
			else
				skipsymbol(semisym)
				exit
			esac
		else
			case ttbasetype[mbase]
			when tenum, tstruct, tunion then		!assume defining a [part]type only
				skipsymbol(semisym)
				exit
			when ti32 then				!allow for now, as it migt be an enum decl with no name
				skipsymbol(semisym)
				exit
			else
				serror_s("Decl error #",typename(mbase))
			esac
		end docase
	od
end

global function parsemodule:int=
	int size,t
	ref strec owner
	real tsecs

	loopindex:=ingeneric:=0
	ist_symptr:=nil
	memset(&casevaluestack,0,casevaluestack.bytes)

	startlex("PARSETEST",mainfileno)
	owner:=stmodule
	currproc:=nil
	loopindex:=0

	lex()

	readmodule()

	endlex()
	return 1
end

function readdeclspec(ref strec owner,int &linkage)int=
!At first symbol of a declspec, or possible declspec
!read declspec and basetype
!return typecode for basetype, and linkage (static etc)
!if no declspec follows (usually eof) returns 0

	record declrec=
		i32 typeno				!not set, int, float, char, struct, union, enum etc
		byte isconst				!0, or 1 when const used (more than 1 allowed)
		byte isvolatile				!0, or 1 when volatile used
		byte isrestrict
		byte linkage				!0, or static_ss etc; only one allowed
		byte isinline				!1 when inline used
		byte isshort				!1 when short used
		byte islong					!1 when long used (not short or long long)
		byte isllong				!1 when long long used (islong set to 0)
		byte issigned				!not set, signed
		byte isunsigned				!not set, unsigned
		byte isusertype				!1 if basetype set completely from typedef
									!so isshort/long etc or other basetype not allowed
	end
	declrec d
	unit p
	int t,mod,m,fstruct
	ref paramrec pm
	ref strec e

	memset(&d,0,d.bytes)
!clear d
	d.typeno:=tnotset

	fstruct:=mod:=0

	doswitch lx.symbol
    when kstdtypesym then
        d.typeno:=lx.subcode
        lex()

    when ktypespecsym then
		switch lx.subcode
		when ts_int, ts_char, ts_float, ts_double, ts_bool, ts_void then
			if d.typeno<>tnotset then
				if fstruct then checksymbol(semisym)
				else goto tserror
				fi
			fi
			d.typeno:=typespectypes[lx.subcode]

		when ts_short then
			if d.isshort or d.islong or d.isllong then goto tserror fi
			d.isshort:=mod:=1
		when ts_long then
			if d.isllong or d.isshort then goto tserror
			elsif d.islong then
				d.islong:=0
				d.isllong:=1
			else
				d.islong:=1
			fi
			mod:=1

		when ts_signed then
			if d.issigned or d.isunsigned then goto tserror fi
			d.issigned:=mod:=1
		when ts_unsigned then
			if d.issigned or d.isunsigned then goto tserror fi
			d.isunsigned:=mod:=1
		else

	tserror:
			serror_s("declspec/ts #",typespecnames[lx.subcode])
		end switch
		lex()

	when ktypequalsym then
		case lx.subcode
		when const_qual then
			unless fnoconst then
				d.isconst:=1
			end
		when volatile_qual then d.isvolatile:=1
		when restrict_qual then d.isrestrict:=1
		esac
		lex()

	when klinkagesym then
		if d.linkage then serror("Dual storage spec") fi
		d.linkage:=lx.subcode
		lex()

	when kfnspecsym then
		case lx.subcode
		when inline_fnspec then
			d.isinline:=1
		esac
		lex()
	when kstructsym,kunionsym then
		if d.typeno<>tnotset then serror("struct?") fi
		d.typeno:=readstructdecl(owner)
		d.isusertype:=1
		fstruct:=1

	when kenumsym then
		if d.typeno<>tnotset then serror("enum?") fi
		readenumdecl(owner)
		d.typeno:=ti32			!disregard enum 'type'; just use int
		d.isusertype:=1

	when namesym then			!should resolve to see if a user-type ...
								! ... unless a basetype already seen
		if d.typeno=tnotset and (m:=isusertype(owner))<>tnotset then
			if mod then			!unsigned etc without proper base type; assume name is not part o it
				d.typeno:=ti32
				exit
			fi
			d.typeno:=m
			d.isusertype:=1
			lex()
		else
			if d.typeno=tnotset and not mod then
				serror_s("Implicit decls not allowed: #",lx.symptr.name)
			fi

			if d.typeno=tnotset then d.typeno:=ti32 fi
			exit
		fi

	else
		exit
	end doswitch

	t:=(d.typeno<>tnotset|d.typeno|ti32)

	if not d.isusertype then				!otherwise everything should be set up
		case t
		when ti32 then
			if d.isshort then
				t:=(d.isunsigned|tu16|ti16)
			elsif d.islong then
				if flong64 then
					t:=(d.isunsigned|tu64|ti64)
				else
					t:=(d.isunsigned|tu32|ti32)
				fi
			elsif d.isllong then
				t:=(d.isunsigned|tu64|ti64)
			elsif d.isunsigned then
				t:=tu32
			fi
		when ti8 then
			if d.isshort or d.islong or d.isllong then serror("char decl?") fi
			t:=(d.isunsigned|tu8|ti8)
		when tr64 then
			if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("dbl decl?") fi

		else
			if mod then serror("declspec/float") fi
		esac
	fi

	if d.isconst then
		t:=createconstmode(t)
	fi

	linkage:=d.linkage
	return t
end

function istypestarter:int=
!return 1 when current symbol could start a type-spec
	ref strec d

	case lx.symbol
	when ktypespecsym, kstdtypesym then
		return 1
	when ktypequalsym then
!	return lx.subcode=const_qual
		return 1
	when namesym then
		d:=resolvename((currproc|currproc|stmodule),lx.symptr,ns_general,currblockno)
		if d then
			lx.symptr:=d
			return d.nameid=typeid
		fi
	when kstructsym,kunionsym,kenumsym then
		return 1
	end
	return 0

end

function istypestarter_next:int=
!return 1 when current symbol could start a type-spec
	ref strec d

	case nextlx.symbol
	when ktypespecsym, kstdtypesym then
		return 1
	when ktypequalsym then
!	return lx.subcode=const_qual
		return 1
	when namesym then
		d:=resolvename((currproc|currproc|stmodule),nextlx.symptr,ns_general,currblockno)
		if d then
			nextlx.symptr:=d
			return d.nameid=typeid
		fi
	when kstructsym,kunionsym,kenumsym then
		return 1
	end
	return 0

end

function readexpression:unit=
	unit p, ulist, ulistx
	int t

	case nextlx.symbol
	when  semisym,rbracksym then
		return readterm()
	esac

	p:=readassignexpr()

	if lx.symbol=commasym then		!
		ulist:=ulistx:=nil
		do
			addlistunit(&ulist,&ulistx,p)
			exit when lx.symbol<>commasym
			lex()
			p:=readassignexpr()
		od
		p:=createunit1(jexprlist,ulist)
		if ulistx then
			p.mode:=ulistx.mode
		fi

		return p
	fi
	return p
end

function readassignexpr:unit=
	unit p,q,r
	int opc,oldpmode

	case nextlx.symbol
	when commasym, semisym,rbracksym then
		return readterm()
	when assignsym then
		p:=readterm()
		opc:=lx.symbol
		goto gotp
	esac

	p:=readcondexpr()

	case opc:=lx.symbol
	when assignsym, multosym, divtosym, remtosym, addtosym, subtosym,
			shltosym, shrtosym, iandtosym, ixortosym, iortosym then
	gotp:
		lex()
		oldpmode:=p.mode
		checklvalue(p,1)
		q:=readassignexpr()
		if ttisref[p.mode] then
			return createassignopref(opc,p,q)
		fi

		q:=coercemode(q,oldpmode)
		if ttconst[oldpmode] then
			terror("Modifying read-only var")
		fi

		if p.tag=jptr and p.a.tag=jconst then
			terror("Modifying constant?")
		fi


		r:=createunit2(symboltojtag[opc],p,q)

		r.mode:=oldpmode
		return r
	end

	return p
end

function readcondexpr:unit=
	unit x,y,pcond
	int s,t,u

	pcond:=readorlexpr()

	if lx.symbol=questionsym then
		coercecond(pcond)

		lex()
		x:=readexpression()
		skipsymbol(colonsym)
		y:=readcondexpr()

		if u:=dominantmode[s:=ttbasetype[x.mode],t:=ttbasetype[y.mode]] then			!were both numeric
			x:=coercemode(x,u)
			y:=coercemode(y,u)
			if pcond.tag=jconst and x.tag=jconst and y.tag=jconst then
				return (pcond.value|x|y)
			fi

		elsif s=tref and t=tref then
			u:=x.mode
		elsif s=tref and t=ti32 and y.tag=jconst and y.value=0 then
			u:=x.mode
			coercemode(y,u)
		elsif s=ti32 and t=tref and x.tag=jconst and x.value=0 then
			u:=y.mode
			coercemode(x,u)
		elsif s=tstruct and t=tstruct then
			u:=x.mode
		elsif s=tunion and t=tunion then
			u:=x.mode
		elsif s=t=tvoid then
			u:=tvoid
		else
	CPL strmode(x.mode),strmode(y.mode)
			terror("?: incompatible types")
		fi

		pcond:=createunit3(jifx,pcond,x,y)
		pcond.mode:=u
	fi

	return pcond
end

function readorlexpr:unit=
	unit x,y

	x:=readandlexpr()

	while lx.symbol=orlsym do
		lex()
		y:=readandlexpr()
		coercecond(x)
		coercecond(y)

		if x.tag=jconst and y.tag=jconst then
			x.value := (x.value or y.value|1|0)
			nextloop
		fi
		x:=createunit2(jorl,x,y)
		x.mode:=ti32
	od

	return x
end

function readandlexpr:unit=
	unit x,y

	x:=readiorexpr()

	while lx.symbol=andlsym do
		lex()
		y:=readiorexpr()
		coercecond(x)
		coercecond(y)

		if x.tag=jconst and y.tag=jconst then
			x.value := (x.value and y.value|1|0)
			nextloop
		fi
		x:=createunit2(jandl,x,y)
		x.mode:=ti32
	od

	return x
end

function readiorexpr:unit=
	unit x,y
	int u

	x:=readixorexpr()

	while lx.symbol=iorsym do
		lex()
		y:=readixorexpr()

		if u:=dominantmode[ttbasetype[x.mode],ttbasetype[y.mode]] then			!were both numeric
			if u>=tr32 then terror("float|float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			terror("invalid | operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64 then
				x.value ior:= y.value
				nextloop
			esac
		fi
		x:=createunit2(jior,x,y)
		x.mode:=u
	od

	return x
end

function readixorexpr:unit=
	unit x,y
	int u

	x:=readiandexpr()

	while lx.symbol=ixorsym do
		lex()
		y:=readiandexpr()

		if u:=dominantmode[ttbasetype[x.mode],ttbasetype[y.mode]] then			!were both numeric
			if u>=tr32 then terror("float^float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			terror("invalid ^ operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64 then
				x.value ixor:= y.value
				nextloop
			esac
		fi
		x:=createunit2(jixor,x,y)
		x.mode:=u
	od

	return x
end

function readiandexpr:unit=
	unit x,y
	int u

	x:=readeqexpr()

	while lx.symbol=iandsym do
		lex()
		y:=readeqexpr()

		if u:=dominantmode[ttbasetype[x.mode],ttbasetype[y.mode]] then			!were both numeric
			if u>=tr32 then terror("float&float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			cpl strmode(x.mode)
			cpl strmode(y.mode)
			terror("invalid & operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64 then
				x.value iand:= y.value
				nextloop
			esac
		fi
		x:=createunit2(jiand,x,y)
		x.mode:=u
	od

	return x
end

function readeqexpr:unit=
	unit x,y
	int opc,s,t,u,ss,tt

	x:=readrelexpr()

	while (opc:=lx.symbol)=eqsym or opc=nesym do
		lex()
		y:=readrelexpr()

		if u:=dominantmode[s:=ttbasetype[x.mode],t:=ttbasetype[y.mode]] then			!were both numeric
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		elsif s=tref and t=tref then
			if (ss:=tttarget[x.mode])<>(tt:=tttarget[y.mode]) then
				if ss<>tvoid and tt<>tvoid then
					if not checkpointertypes(x.mode,y.mode,1) then	!'hard'
						terror("Comparing distinct pointers/eq")
					fi
				fi
			fi
		elsif s=tref and t=ti32 then
			if y.tag<>jconst or y.value<>0 then
				terror("Can't compare pointer to int")
			fi
		elsif s=ti32 and t=tref then
			if x.tag<>jconst or x.value<>0 then
				terror("Can't compare pointer to int2")
			fi
		else
			CPL =U
			terror("invalid == operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64,0 then			!0 when ref/ref ref/int int/ref
				if opc=eqsym then
					x.value := x.value = y.value
				else
					x.value := x.value <> y.value
				fi
				nextloop
			esac
		fi
		x:=createunit2(symboltojtag[opc],x,y)
		x.mode:=ti32
	od

	return x
end

function readrelexpr:unit=
	unit x,y
	int opc,s,t,u
	i64 a,b,c
	u64 aa,bb,cc

	x:=readshiftexpr()

	while (opc:=lx.symbol)=ltsym or opc=lesym or opc=gesym or opc=gtsym do
		lex()
		y:=readshiftexpr()

		if u:=dominantmode[s:=ttbasetype[x.mode],t:=ttbasetype[y.mode]] then			!were both numeric

			x:=coercemode(x,u)
			y:=coercemode(y,u)
		elsif s=tref and t=tref then
			if not checkpointertypes(x.mode,y.mode,1) then		!use 'hard' mode
				terror("Comparing distinct pointers/rel")
			fi
		else
			terror("invalid rel operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			a:=x.value; b:=y.value
			case u
			when ti32,ti64 then
				case opc
				when ltsym then c:=a<b
				when lesym then c:=a<=b
				when gesym then c:=a>=b
				else            c:=a>b
				esac
				x.value:=c
				nextloop
			when tu32,tu64 then
				aa:=x.value; bb:=y.value
				case opc
				when ltsym then cc:=aa<bb
				when lesym then cc:=aa<=bb
				when gesym then cc:=aa>=bb
				else            cc:=aa>bb
				esac
				x.value:=cc
				nextloop
			esac
		fi

		x:=createunit2(symboltojtag[opc],x,y)
		x.mode:=ti32
	od

	return x
end

function readshiftexpr:unit=
	unit x,y
	int opc,u

	x:=readaddexpr()

	while (opc:=lx.symbol)=shlsym or opc=shrsym do
		lex()
		y:=readaddexpr()

		coercebasetype(x)
		unless (u:=ttbasetype[x.mode])>=tfirstint and u<=tlastint then
			terror("shift:Not an int")
		end unless
		y:=coercemode(y,ti32)
!
		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64 then
				if opc=shlsym then
					x.value := x.value << y.value
				else
					x.value := x.value >> y.value
				fi
				nextloop
			when tu32,tu64 then
				if opc=shlsym then
					x.uvalue := x.uvalue << y.value
				else
					x.uvalue := x.uvalue >> y.value
				fi
				nextloop
			esac
		fi
		x:=createunit2((opc=shlsym|jshl|jshr),x,y)
		x.mode:=u
	od

	return x
end

function readaddexpr:unit=
	unit p,q
	int opc

	p:=readmulexpr()

	while (opc:=lx.symbol)=addsym or opc=subsym do
		lex()
		q:=readmulexpr()

		if opc=addsym then
			p:=createaddop(p,q)
		else
			p:=createsubop(p,q)
		fi
	od

	return p
end

function readmulexpr:unit=
	unit p,q
	int opc

	p:=readterm()

	while (opc:=lx.symbol)=mulsym or opc=divsym or opc=remsym do
		lex()
		q:=readterm()
		case opc
		when mulsym then
			p:=createmulop(p,q)
		when divsym then
			p:=createdivop(p,q)
		when remsym then
			p:=createremop(p,q)
		esac
	od

	return p
end

function readterm:unit=
	unit p, q
	int t,u,opc,shift,newlen,slength,tbase,fwide,newmode, tag
	ref char pbyte
	i64 a
	ref strec d
	ichar ss,s
	ref paramrec pm

	switch lx.symbol
	when intconstsym, realconstsym then
		p:=createconstunit(lx.value,lx.subcode)

		lex()
	when namesym then
		if lx.symptr.nameid<=macroid then
			d:=resolvename((currproc|currproc|stmodule),lx.symptr,ns_general,currblockno)
			if d=nil then
				serror_s("Undefined name ""#""", getstname(lx.symptr))
			fi
		else
			d:=lx.symptr
		fi

!		d.used:=1
!		if d.used<255 then ++d.used fi
		case d.nameid
		when enumid then
			p:=createconstunit(d.index,ti32)
		when procid then
			if nextlx.symbol<>lbracksym then
				p:=createunit0(jfuncname)
				p.def:=d
				p.mode:=createrefmode(createprocmode(d.mode,d.paramlist))
!			p.mode:=createprocmode(d.mode,d.paramlist)
			else
				goto doname
			fi

		else
	doname:
			p:=createname(d)
			p.mode:=t:=d.mode
			if ttbasetype[t]=tarray then
				p.alength:=ttlength[t]
				p:=createaddrofop(p)
				p.mode:=createrefmode(tttarget[t])
			elsif d.nameid<>procid and ttsize[t]<4  then
				fixmemopnd(p)
			elsif d.nameid=paramid then
			fi
		esac
		p.lineno:=lx.lineno
		lex()

	when stringconstsym,wstringconstsym then
		fwide:=lx.symbol=wstringconstsym
		s:=lx.svalue

		slength:=lx.length
		while nextlx.symbol=stringconstsym do		!combine consecutive strings
			newlen:=slength+nextlx.length
			ss:=pcm_alloc(newlen+1)
			memcpy(ss,s,slength)
			memcpy(ss+slength,nextlx.svalue,nextlx.length)
			(ss+newlen)^:=0
			s:=ss
			slength:=newlen
			lex()
		od

		if fwide then
			p:=createwstringconstunit(cast(s),slength)
		    p.wslength:=slength
			p.mode:=trefwchar
		else
			p:=createstringconstunit(s,slength)
!		    p.slength:=slength
			p.mode:=trefchar

		fi

		lex()

	when charconstsym then
		a:=0
		shift:=0
		pbyte:=lx.svalue
		if lx.length>8 then serror("char const too long") fi

		to lx.length do
			a:=a ior u64(pbyte^)<<shift
			shift+:=8
			++pbyte
		od
		p:=createconstunit(a,(lx.length<=4|ti32|ti64))
		lex()

	when addsym then
		lex()
		p:=readterm()

	when subsym then
		lex()
		p:=createnegop(readterm())

	when notlsym then
		lex()
		p:=readterm()
		coercecond(p)
		p:=createunit1(jnotl,p)
		p.mode:=ti32

		if p.a.tag=jnotl and p.a.a.tag=jnotl then
			p.a:=p.a.a.a
		fi

	when inotsym then
		lex()
		p:=createinotop(readterm())

	when iandsym then			!&
		lex()
!&* cancel, so detect this early to avoid more complicated code, which also
!has a bug when following term is an array that decays to a pointer; it ends up
!with an incorrect number of ptrs (one too many I think). The .alength trick
!doesn't work when the array is unbounded as in (*A)[]
!However, detecting &* doesn't cover &(*X) for example
!I need to have .alength plus also an array indicator. Fortunately array pointers
!and the use of &* mainly occur in my generated code
!
		if lx.symbol=mulsym then
			lex()
			p:=readterm()
		else
			p:=createaddrofop(readterm())
		fi

	when andlsym then			!&&
		serror("rt/&&label")

	when mulsym then			!*
		lex()
		p:=createptrop(readterm())

	when incrsym, decrsym then			!*
		opc:=symboltojtag[lx.symbol]
		lex()
		p:=createincrop(opc,readterm())

	when abssym then
		lex()
		skipsymbol(lbracksym)
		p:=createabsop(readexpression())
		skipsymbol(rbracksym)

	when lbracksym then			!(
		lex()
		if istypestarter() then
			t:=readcasttype(d,0,pm)
			skipsymbol(rbracksym)
			if lx.symbol=lcurlysym then
				serror("rt/compound lit")
			else
				p:=docast(readterm(),t)
			fi
		else
			p:=readexpression()
			skipsymbol(rbracksym)
		fi
	when ksizeofsym then
		if lx.subcode then
			lex()
			if lx.symbol=lbracksym then		!possible type
				lex()
				if istypestarter() then
					t:=readcasttype(d,0,pm)
					skipsymbol(rbracksym)
					p:=createconstunit(ttlength[t],tu64)
				else
					p:=readexpression()
					skipsymbol(rbracksym)
					p:=createsizeofop(p,1)
				fi
			else
				p:=createsizeofop(readterm(),1)
			fi
		else
			lex()
			if lx.symbol=lbracksym then		!possible type
				if istypestarter_next() then
					lex()
					t:=readcasttype(d,0,pm)
					skipsymbol(rbracksym)
					p:=createconstunit(ttsize[t],tu64)
				else
					p:=readterm()
					p:=createsizeofop(p)
				fi
			else
				p:=createsizeofop(readterm())
			fi
		fi

	when kgenericsym then
		p:=readgeneric()
	when kalignofsym then
		serror("rt/alignof")

	when ksetjmpsym then
		tag:=lx.subcode
		lex()
		checksymbol(lbracksym)
		lex()
		p:=readassignexpr()
		if tag=jlongjmp then
			checksymbol(commasym)
			lex()
			q:=readassignexpr()
		else
			q:=nil
		fi
		p:=createunit2(tag,p,q)
		p.mode:=ti32
		checksymbol(rbracksym)
		lex()


	else
	PS("RT")
		serror("Readterm?")
	end switch

!look at the suffix

	docase lx.symbol
	when lsqsym then
		lex()
		q:=readexpression()
		skipsymbol(rsqsym)
		p:=createindexop(p,q)

	when dotsym, idotsym then
		opc:=symboltojtag[lx.symbol]
		lex()
		checksymbol(namesym)
		d:=lx.symptr
		lex()

		p:=createdotop(opc,p,d)

	when lbracksym then
		lex()
		if lx.symbol=rbracksym then			!()
			q:=nil
			lex()
		else
			q:=readexprlist(nil)
			skipsymbol(rbracksym)
		fi
		p:=createcall(p,q)

	when incrsym then
		lex()
		p:=createincrop(jpostincr,p)

	when decrsym then
		lex()
		p:=createincrop(jpostdecr,p)

	else
		exit
	end docase

	return p
end

function readexprlist(unit p)unit=
! read comma-separated list, and return head of list (not as jmakelist etc)
!p=nil:		at start of first expr (not ")")
!p<>nil:	p will be head of the list; comma skipped so at start of next expr
	unit ulist, ulistx

	ulist:=ulistx:=p
	do
		p:=readassignexpr()
		addlistunit(&ulist,&ulistx,p)
		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od
	return ulist

end

function readmodulevar(ref strec d, int m, linkage)ref strec=
!read or function decl at module scope
	ref strec e
	int scope,emode

	e:=checkdupl(stmodule, d, ns_general, 0)

	if e then					!already exists
		if e.nameid<>staticid then
			serror_ss("var: name in use # #",e.name,namenames[e.nameid])
		fi
		emode:=e.mode
		if emode<>m then
			if not comparemode(emode,m) then
	redef:
				serror_s("var: redefining #",e.name)
			fi
			case ttbasetype[emode]
			when tarray then
				if ttlength[emode]=0 then			!replace empty array
					e.mode:=m
				elsif ttlength[m] and ttlength[emode]<>ttlength[m] then
					goto redef
				fi
			esac

		fi
		d:=e

!see how scope interacts with existing decl
		scope:=d.scope
		if scope=local_scope and linkage=none_ss or
		   scope=exported_scope and linkage=static_ss or
		   scope=imported_scope and linkage=static_ss then

!*!		serror("Linkage mismatch")

		elsif scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
		fi

	else
		d:=createdupldef(stmodule,d,staticid)
		d.mode:=m
		case linkage
		when static_ss then
			scope:=local_scope
		when extern_ss then
			scope:=imported_scope
		else
			scope:=exported_scope
		esac

	fi

	if lx.symbol=assignsym then
		if d.code then
			serror_s("Can't init twice #",d.name)
		fi
		if scope=imported_scope then
			serror_s("Can't init extern #",d.name)
		fi
		lex()
		d.code:=readinitexpr(stmodule,d.mode)
	fi

	d.scope:=scope
	return d
end

function readframevar(ref strec d,int m, linkage)ref strec=
	ref paramrec pm
	ref strec e
	int scope,id

	e:=checkdupl_inproc(currproc, d, ns_general, currblockno)

	if e then					!already exists
			serror_s("var: name in use #",e.name)
		d:=e

!see how scope interacts with existing decl
		scope:=d.scope
		if scope=local_scope and linkage=none_ss or
		   scope=exported_scope and linkage=static_ss or
		   scope=imported_scope and linkage=static_ss then
!*!		serror("Linkage2 mismatch")
		elsif scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
		fi
	else
		id:=frameid
		scope:=function_scope
		case linkage
		when static_ss then
			id:=staticid
		when extern_ss then
			scope:=imported_scope
			id:=staticid
		esac
		d:=createdupldef(currproc,d,id)
		d.mode:=m
		d.blockno:=currblockno
		blockcounts[currblockno]:=1
	fi

	if lx.symbol=assignsym then
		if d.code then
			serror_s("Can't init twice #",d.name)
		fi
		if scope=imported_scope then
			serror_s("Can't init extern #",d.name)
		fi
		lex()
		d.code:=readinitexpr(currproc,d.mode)
	fi

	d.scope:=scope

	return d
end

function readtype(ref strec owner, &d, int m, ref paramrec &pm)int=
	[maxtypemods]int modtype
	[maxtypemods]ref void modvalue
	ref paramrec pmx
	int nmodifiers,i
	nmodifiers:=0

	pm:=nil

	readnamedtype(owner,d, modtype,modvalue,nmodifiers)

!now apply modifiers to base type:
	for i:=nmodifiers downto 1 do
		case modtype[i]
		when 'A' then
			m:=createarraymode(m,int(modvalue[i]))
		when 'R' then
			m:=createrefmode(m)
		when 'C' then
			m:=createconstmode(m)
		when 'F' then
			pmx:=modvalue[i]

			if i=1 then				!indicate to caller that this is a normal function
				pm:=pmx
			else					!assume function pointer of some sort
				m:=createprocmode(m,pmx)
			fi
		esac
	od

	return m
end

proc readnamedtype(ref strec owner, &d, []int &modtype, []ref void &modvalue, int &nmodifiers)=
	int length
	[maxtypemods]int fconst
	int nrefs
	unit pdim

	d:=nil
	nrefs:=0

	if lx.symbol=kfnspecsym then
		lex()
	fi

	while lx.symbol=mulsym do			!pointer/qualifier loop
		++nrefs
		fconst[nrefs]:=0
		lex()
		while lx.symbol=ktypequalsym do
			case lx.subcode
			when const_qual then
				fconst[nrefs]:=1
			when volatile_qual, restrict_qual then
			else
				serror("rnt1")
			esac
			lex()
		od
	od

	case lx.symbol
	when namesym then
		d:=lx.symptr
		lex()
	when lbracksym then
		lex()
		readnamedtype(owner,d,modtype,modvalue,nmodifiers)
		skipsymbol(rbracksym)
	esac

	docase lx.symbol
	when lsqsym then
		lex()
		if lx.symbol=rsqsym then
			length:=0
		else
			pdim:=readassignexpr()
			if pdim.tag=jconst then
				length:=pdim.value
			else
				serror("Can't do VLAs")
			fi
			checksymbol(rsqsym)
IF LENGTH=0 THEN SERROR("ZERO LEN ARRAY") fi
		fi
		if length<0 then terror("Negative array dim") fi

		lex()
		modtype[++nmodifiers]:='A'
		modvalue[nmodifiers]:=ref void(length)

	when lbracksym then			!fn params
		lex()
		modtype[++nmodifiers]:='F'
		modvalue[nmodifiers]:=readparams(owner)
	else
		exit
	end docase

!now apply any pointers
	while nrefs do
		if fconst[nrefs] then
			modtype[++nmodifiers]:='C'
		fi
		modtype[++nmodifiers]:='R'
		--nrefs
	od
end

function readconstintexpr:int=
	unit p
	int val

	p:=readassignexpr()
	case p.tag
	when jconst then
		return p.value

	else
		serror_s("readconstint #",jtagnames[p.tag])
	esac
	return 0
end

function readinitexpr(ref strec owner, int m)unit=
	int count
	unit p

	p:=readinitexpr2(owner,m,1)

	return p
end

function readinitexpr2(ref strec owner, int m, istop)unit=
	unit ulist, ulistx, p
	int mbase,melem,mm
	int dim,count
	ref strec d,e
	int braces
	ichar newstr

	mbase:=ttbasetype[m]
	count:=0

	if lx.symbol=lcurlysym then
		lex()

		count:=0
		case mbase
		when tarray then
			dim:=ttlength[m]
			if not istop and dim=0 then terror("init/0-size array") fi
			melem:=tttarget[m]
!			if ttbasetype[melem]=tu8 and lx.symbol=stringconstsym then
			if ttbasetype[melem]=tchar and lx.symbol=stringconstsym then
				braces:=1
				goto doarraystring
			fi

		when tstruct,tunion then
			d:=ttnamedef[m]
			e:=d.deflist
			if e=nil then
				terror("init/Empty struct")
			fi
			melem:=e.mode
		else
			p:=readassignexpr()
			p:=coercemode(p,m)
			skipsymbol(rcurlysym)
			return p
		esac

		ulist:=ulistx:=nil
		do
			p:=readinitexpr2(owner,melem,0)
			++count

			case mbase		
			when tarray then
				if dim and count>dim then
					terror("Too many array elems")
				fi

				if ttbasetype[melem]=tarray and ttbasetype[tttarget[melem]]=tchar and p.mode=trefchar then
				else
					p:=coercemode(p,melem)
				fi
			when tstruct then

				mm:=e.mode

				if ttbasetype[mm]=tarray and ttbasetype[tttarget[mm]]=tu8 and p.mode=trefchar then
				else
					p:=coercemode(p,mm)
				fi

				e:=e.nextdef
				if e=nil then
					if lx.symbol=commasym and nextlx.symbol<>rcurlysym then
						terror("Too many struct elems")
					fi
				else
					melem:=e.mode
				fi
			when tunion then
				p:=coercemode(p,melem)
				ulist:=ulistx:=p
				goto donestruct
			esac

			addlistunit(&ulist,&ulistx,p)
			if lx.symbol<>commasym then
				exit
			fi
			if nextlx.symbol=rcurlysym then		! {10,20,30,} allowed
				lex()
				exit
			fi
			lex()
		od
		if mbase=tarray and dim=0 then
			ttlength[m]:=count
			ttsize[m]:=count*ttsize[melem]
		fi

	donestruct:
		skipsymbol(rcurlysym)
		p:=createunit1(jmakelist,ulist)
		p.count:=count

		p.mode:=m

	else
		braces:=0
		case mbase
		when tarray then
	doarraystring:
			if lx.symbol<>stringconstsym and lx.symbol<>wstringconstsym and 
				tttarget[m]<>tchar then
				terror("{} initialiser expected")
			fi

			p:=readassignexpr()
			case p.mode
			when trefchar then
			when trefwchar then
			else
				terror("Array init")
			esac
			P.MODE:=M

			if (dim:=ttlength[m])=0 then
				ttlength[m]:=ttsize[m]:=p.slength
			else
				if p.slength>dim and p.slength<>dim+1 then	!ASSUME last data char is zero
					terror("Init str too long")
				fi

				if p.slength<dim then
					newstr:=pcm_allocz(dim)
					memcpy(newstr, p.svalue, p.slength)
					p.svalue:=newstr
				fi

! terror("Init str too short") fi
				p.slength:=dim

			fi
			if braces then skipsymbol(rcurlysym) fi
			return p
		esac
		p:=readassignexpr()
		p:=coercemode(p,m)

	fi
	return p
end

proc pushblock=
	int n

	if blocklevel>=maxblockstack then
		serror("Too many block levels")
	fi
	if nextblockno>=maxblock then
		serror("Too many blocks")
	fi
	++blocklevel
	++nextblockno

	n:=currblockno

	int m:=blocklevel								!NEED TO ACCESS CONTAINING BLOCKS
													!VIA BLOCKSTACK

	while m and blockcounts[blockstack[m]]=0 do
		--m
    n:=blockstack[m]
	od

	blockowner[nextblockno]:=n

	currblockno:=blockstack[blocklevel]:=nextblockno
	blockcounts[currblockno]:=0
end

proc popblock=
	currblockno:=blockstack[--blocklevel]
end

function readcompoundstmt(int params):unit=
!read {...} statements
!positioned at first {, exit at symbol past final }
	unit ulist, ulistx, p,q

	ulist:=ulistx:=nil

	lex()			!skip {
	pushblock()
	if params then		!assume top block of function
		blockcounts[1]:=1
	fi

	while lx.symbol<>rcurlysym do
		p:=readstatement()

		if p=nil then nextloop fi				!might have been typedef etc
		if p.tag=jtempdecl then
			repeat
				q:=p.nextunit
				if p.def.code and p.def.nameid<>staticid then
					p.tag:=jdecl
					p.nextunit:=nil
					addlistunit(&ulist,&ulistx,p)
				fi
				p:=q
			until p=nil
		else
			addlistunit(&ulist,&ulistx,p)
		fi
	od
	lex()
	popblock()
	return createunit3(jblock,ulist,nil,ulistx)
end

function readblock(int ifelse=0)unit=

		if not needcompoundblock then
			return readstatement()
		fi
		if lx.symbol=kifsym and ifelse then
			return readstatement()
		fi

		if lx.symbol<>lcurlysym then
			serror("{...} statement expected")
		fi
		return readcompoundstmt(0)
end

function readstatement:unit=
	unit p,q
	ref strbuffer ss
	ref strec d
	int index

	switch lx.symbol
	when kifsym then
		return readifstmt()

	when kforsym then
		return readforstmt()

	when kwhilesym then
		return readwhilestmt()

	when kdosym then
		return readdostmt()

	when kreturnsym then
		return readreturnstmt()

	when kswitchsym then
		return readswitchstmt()

	when lcurlysym then
		return readcompoundstmt(0)

	when kgotosym then
		return readgotostmt()

	when kbreaksym then
		if loopindex then
			if looptypestack[loopindex]='L'then
				p:=createunit0(jbreak)
				lex()
			else
				p:=createunit0(jbreaksw)
				lex()
			fi
		else
			serror("break outside loop/sw")
		fi

	when kcontinuesym then
		index:=loopindex
		while index and looptypestack[index]<>'L' do --index od
		if index=0 then
			serror("continue outside loop")
		fi

		p:=createunit0(jcontinue)
		lex()

	when kcasesym then
		return readcaselabel()

	when kdefaultsym then
		lex()
		skipsymbol(colonsym)
		return createunit1(jdefaultstmt,readstatement())

	when semisym then
		lex()	
		return nil

	when namesym then
		if nextlx.symbol=colonsym then
			p:=createunit1(jlabelstmt,nil)

			d:=resolvename(currproc,lx.symptr,ns_labels,0)
			if d then
				if d.index=-1 then				!already defined
					cpl lx.symptr.name
					terror("2:Duplicate label")
				fi
			else
				d:=createdupldef(currproc,lx.symptr,labelid)
				d.mode:=tvoid
			fi
			d.index:=-1						!indicate defined

			p.def:=d
			lex()				!skip colon
			lex()
			if lx.symbol=rcurlysym then
			elsif istypestarter() or lx.symbol=klinkagesym then
			else
				p.a:=readstatement()
			fi
			return p
		else
			ist_symptr:=nil
			if isusertype(currproc)<>tnotset then
				goto doreaddecl
			fi
			if ist_symptr then lx.symptr:=ist_symptr fi		!make use of name resolve done by isusertype
			p:=readexpression()
		fi
	when ktypespecsym, kstdtypesym, ktypequalsym, klinkagesym, kfnspecsym,
		kstructsym,kunionsym,kenumsym then
	doreaddecl:
		return readlocaldecl()

	else						!assume expression
		p:=readexpression()
	end switch

	skipsymbol(semisym)

	return p
end

function readifstmt:unit p=
	unit pcond,pbody,pelse
	int lineno

	lex()
	lineno:=lx.lineno

	pcond:=readcond()
	coercecond(pcond)

	pbody:=readblock()

	pelse:=nil

	if lx.symbol=kelsesym then
		lex()

		pelse:=readblock(1)
	fi

	p:=createunit3(jif,pcond,pbody,pelse)
	p.lineno:=lineno

	if iscondtrue(pcond) then		!branch b only
		if pbody=nil then
			pbody:=createunit0(jblock)
		fi
		deleteunit(p,pbody)
	elsif iscondfalse(pcond) then	!branch c only
		if pelse=nil then
			pelse:=createunit0(jblock)
		fi
		deleteunit(p,pelse)
	fi

	return p;
end

global func iscondtrue(unit p)int =
	p.tag=jconst and p.value<>0
end

global func iscondfalse(unit p)int =
	p.tag=jconst and p.value=0
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

function readforstmt:unit=
	unit pinit, pcond, pincr, pbody, ulist,ulistx, p
	int linkage,hasblock,m,mbase
	ref paramrec pm
	ref strec d

	lex()
	skipsymbol(lbracksym)
	hasblock:=0

	if lx.symbol<>semisym then

		if istypestarter() then
			hasblock:=1
			pushblock()

			mbase:=readdeclspec(currproc,linkage)
			ulist:=ulistx:=nil

			docase lx.symbol
			when namesym, mulsym, lbracksym then

				m:=readtype(currproc,d,mbase,pm)
				if d=nil then
					serror("Var name expected")
				fi

				if linkage=typedef_ss or pm then
					serror("Not allowed in for stmt")
				fi
				d:=readframevar(d,m,linkage)

				if d.code then
					p:=createunit0(jdecl)
					p.def:=d
					addlistunit(&ulist,&ulistx,p)
				fi

				case lx.symbol
				when commasym then			!read next item
					lex()
				else
					exit
				esac
			else
				serror("For decl error")
			end docase
			pinit:=createunit3(jblock,ulist,nil,ulistx)

		else
			pinit:=readexpression()
		fi
	else
		pinit:=createunit0(jnull)
	fi
	skipsymbol(semisym)

	if lx.symbol<>semisym then
		pcond:=readexpression()
		coercecond(pcond)
	else
		pcond:=createunit0(jnull)
	fi
	skipsymbol(semisym)

	if lx.symbol<>rbracksym then
		pincr:=readexprstmt()
	else
		pincr:=nil
	fi
	skipsymbol(rbracksym)

	pushloop('L')
	pbody:=readblock()
	poploop()
	if hasblock then
		popblock()
	fi

	pinit.nextunit:=pcond			!the 3 for elements are linked together
	pcond.nextunit:=pincr

	return createunit2(jfor, pinit, pbody)
end

function readwhilestmt:unit=
	unit pcond,pbody

	lex()
	pcond:=readcond()
	coercecond(pcond)
	pushloop('L')
	pbody:=readblock()
	poploop()

	return createunit2(jwhile,pcond,pbody)
end

function readdostmt:unit=
	unit pbody,pcond
	lex()
	pushloop('L')
	pbody:=readblock()
	poploop()
	skipsymbol(kwhilesym)

	pcond:=readcond()
	coercecond(pcond)

	skipsymbol(semisym)
	return createunit2(jdowhile,pbody,pcond)
end

function readreturnstmt:unit=
	unit p
	lex()
	p:=nil

	if lx.symbol<>semisym then
		if currproc.mode=tvoid then
			terror("Can't return value in void function")
		fi

		p:=readexpression()
		p:=coercemode(p,currproc.mode)
		checksymbol(semisym)
	elsif currproc.mode<>tvoid then
		terror("Return value needed")
	fi
	lex()

	return createunit1(jreturn,p)
end

function readgotostmt:unit=
	ref strec d
	unit p

	lex()
	checksymbol(namesym)
	d:=resolvename(currproc,lx.symptr,ns_labels,0)
	if d=nil then					!assume fwd ref
		d:=createdupldef(currproc,lx.symptr,labelid)
		d.mode:=tvoid
	fi
	p:=createunit1(jgoto,nil)

	p.def:=d
	lex()				!skip colon
	skipsymbol(semisym)
	return p
end

function readswitchstmt:unit=
	unit pindex,pstmt,p

	lex()
	pindex:=readcond()			!not a condition, but it doesn't matter
	coercemode(pindex,ti32)

	pushloop('S')
	pstmt:=readblock()			!not a condition, but it doesn't matter
	p:=createunit2(jswitch, pindex, pstmt)
	p.nextcase:=casevaluestack[loopindex]

	poploop()
	return p
end

function readcaselabel:unit=
	unit p,q
	int value

	lex()					!skip case/default
	value:=readconstintexpr()
	skipsymbol(colonsym)

	p:=createunit1(jcasestmt,readstatement())

	p.value:=value

	addcasevalue(value)
	return p
end

function readexprstmt:unit=
	return readexpression()
end

function readcond:unit=
!should be at '(', read conditional expr
	unit pcond
	skipsymbol(lbracksym)
	pcond:=readexpression()
	skipsymbol(rbracksym)
	return pcond
end

function isusertype(ref strec owner)int=
!current symbol is a namesymbol
!return typeno if it resolves to a user type, otherwise tnotset
!will peek at following symbol, and returns 0 if "," or ";" follows
	ref strec d

	d:=resolvename(owner,lx.symptr,ns_general,currblockno)
	if d then
		if d.nameid=typeid then
			return d.mode
		fi
		ist_symptr:=d
	fi
	return tnotset
end

function readlocaldecl:unit=
!at typebase starter inside function or block
	int m,mbase,linkage,nitems,wasenum,wasdef
	ref strec d
	unit ulist,ulistx,p
	ref paramrec pm

	ulist:=ulistx:=nil

	wasenum:=lx.symbol
	mbase:=readdeclspec(currproc,linkage)
	nitems:=0

	docase lx.symbol
	when namesym, mulsym, lbracksym then
		++nitems

		m:=readtype(currproc,d,mbase,pm)
		if d=nil then
			serror("Var name expected")
		fi

		if linkage=typedef_ss then
			d:=createtypedef(currproc,d,m)
		elsif pm then
			if lx.symbol=lcurlysym then
				serror("Nested function")
			fi
			d:=readfunction(d,m,linkage,pm,wasdef, 0)
		else
			d:=readframevar(d,m,linkage)
			p:=createunit0(jtempdecl)
			p.def:=d
			addlistunit(&ulist,&ulistx,p)
		fi
		case lx.symbol
		when commasym then			!read next item
			lex()
		else
			skipsymbol(semisym)
			exit
		esac
	else
		case ttbasetype[mbase]
		when tenum, tstruct, tunion then		!assume defining a [part]type only
			skipsymbol(semisym)
			exit
	when ti32 then
		skipsymbol(semisym)
		exit

		else
			serror_s("Local decl error #",typename(m))
		esac
	end docase

	return ulist
end

function createtypedef(ref strec owner, symptr, int mode)ref strec=
!symptr is a generic symbol for the name
	ref strec d

	d:=checkdupl(owner,symptr,ns_general,currblockno)

	if d then			!existing name
		if d.nameid<>typeid then
			serror_s("Typedef name in use #",d.name)
		fi

		if d.mode<>mode then
			if not comparemode(d.mode, mode) then
				serror_s("Typedef redefined or can't match types #",d.name)
			fi
		fi
		return d
	fi

	d:=createdupldef(owner,symptr,typeid)

	d.mode:=mode
	tttypedef[mode]:=d

	d.blockno:=currblockno
	blockcounts[currblockno]:=1

	return d
end

function readparams(ref strec owner)ref paramrec=
	ref paramrec ulist,ulistx, pm, q
	int m,lastbasetype,nparams,variadic,flags,nnames
	ref strec d

	D:=NIL

	ulist:=ulistx:=nil
	variadic:=nparams:=nnames:=0

	lastbasetype:=tvoid

	int names:=0, nonames:=0,reported:=0

	while lx.symbol<>rbracksym do
		if lx.symbol=ellipsissym then
			variadic:=1
			lex()
			exit
		fi

		if istypestarter() then
			m:=readcasttype(d,1,pm,tvoid,&lastbasetype)
			if pm then			!was a fu nction; convert to fu nction pointer
				m:=createrefmode(createprocmode(m,pm))
			fi
		else
			if lastbasetype=tvoid then
				serror("Param type missing or misspelt")
			fi
			m:=readcasttype(d,1,pm, lastbasetype)

		fi

		case ttbasetype[m]
		when tarray then
			m:=createrefmode(tttarget[m])
		when tproc then
			m:=createrefmode(createprocmode(m,ttparams[m]))
		esac

		pm:=pcm_allocz(paramrec.bytes)
		pm.def:=d
		pm.mode:=m
		++nparams

		if d then names:=1 else nonames:=1 fi

	if names and nonames and not reported then
		reported:=1
	fi

		if d then
			++nnames
			q:=ulist
			while q do
				if q.def=d then
					serror_ss("Param name reused # #",d.name,namenames[d.nameid])
				fi
				q:=q.nextparam
			od

		fi

		addlistparam(&ulist,&ulistx,pm)
		case lx.symbol
		when commasym then
			lex()
		when ellipsissym, rbracksym then
		else
			serror("bad symbol in paramlist")
		esac
	od

	flags:=0
	skipsymbol(rbracksym)

	if variadic then
		flags:=pm_variadic
	elsif nparams=0 then
		flags:=pm_notset
	elsif nparams=1 and m=tvoid then
		flags:=pm_empty
		nparams:=0
		ulist.mode:=tvoid
	fi

	if ulist=nil then
		ulist:=pcm_allocz(paramrec.bytes)
	fi
	ulist.nparams:=nparams
	ulist.flags:=flags

	return ulist
end

function readcasttype(ref strec &d, int allowname=0,ref paramrec &pm,
	int m=tvoid, ref int mbase=nil)int=
!at first symbol of a type-spec
!ref paramrec pm
	ref strec owner
	int linkage

	owner:=(currproc|currproc|stmodule)

	linkage:=0
	d:=nil
	if m=tvoid then
		m:=readdeclspec(owner,linkage)
		if mbase then
			mbase^:=m
		fi

	fi
	pm:=nil

	case lx.symbol
	when namesym, mulsym, lbracksym, lsqsym then
		m:=readtype(owner,d, m, pm)
		if d and not allowname then
			serror_s("NAME not allowed in cast type #",d.name)
		fi
	esac

	return m
end

function readfunction(ref strec d, int m, linkage, ref paramrec pm, int &wasdef, exported)ref strec=
!have read function declaration, with ";" or "{" nextloop
!d is generic st entry for name
!m is return type
!pm is linked list of parameter types
!set up the declaration properly in symbol table, checking for duplicates etc
!read function body if {...} follows
!return wasdef=1 if {...} encountered, as looping in the caller will be affected

	ref strec f,owner
	int scope

	owner:=stmodule
	wasdef:=0

	f:=checkdupl(owner, d, ns_general, 0)

	if f then					!already exists
		if f.nameid<>procid then
			serror_s("fn: name in use #",d.name)
		fi
!COMPARE PARAM LISTS...
!	if e.paramlist<>pm then
!		serror("fn: params don't match previous")
!	fi
		d:=f

!see how scope interacts with existing decl
		scope:=d.scope
		if scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
		elsif linkage=static_ss then
			scope:=local_scope
		fi


	else
		d:=createdupldef(owner,d,procid)
		d.mode:=m
		case linkage
		when static_ss then
			scope:=local_scope
		when extern_ss then
			scope:=imported_scope
		else
			scope:=exported_scope
		esac

	fi

	d.paramlist:=pm
	d.scope:=scope

	if exported then
		d.exported:=1
	fi


	if lx.symbol=lcurlysym then

		wasdef:=1
		if d.code then
			serror_s("Can't define function twice #",d.name)
		fi
		if scope=imported_scope then
			d.scope:=exported_scope
		fi

		readfunctionbody(d)
		if lx.symbol=semisym then
			serror("; after function def")
		fi

	fi

	return d
end

proc readfunctionbody(ref strec f)=
!positioned just after '{'; return at '}' (checked by caller)
	ref strec e
	unit p
	ref paramrec pm
	int pmcount

	currproc:=f
	nextblockno:=currblockno:=0
	pmcount:=0

!add named patams
	pm:=f.paramlist
	to pm.nparams do
		if pm.def=nil then
!			serror("Param name missing")
		else
			e:=createdupldef(f,pm.def,paramid)
			if e.name^='$' then			!assume block ret param
				e.used:=1
			fi

			e.blockno:=1
			e.mode:=pm.mode
		fi
		pm:=pm.nextparam
		pmcount:=1
	od

	p:=readcompoundstmt(pmcount)

	currproc.code:=p
	currproc:=nil
end

function createnegop(unit p)unit=
	unit q
	int t

	t:=p.mode

	if p.tag=jconst then
		case t
		when ti32,ti64,tu64 then
			p.value:=-p.value
			return p
		when tu32 then
			p.value:=(-p.value) iand 0xFFFF'FFFF
			return p
		when tr64 then
			p.xvalue:=-p.xvalue
			return p
		esac
	fi
	retry:
	if t>=tfirstnum and t<=tlastnum then
		coercebasetype(p)
		q:=createunit1(jneg,p)
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
	else
	CPL strmode(t)
		terror("neg bad type")
	fi

	q.mode:=p.mode
	return q
end

function createabsop(unit p)unit=
	unit q
	int t

	t:=p.mode

	if p.tag=jconst then
		case t
		when ti32,ti64 then
			p.value:=abs(p.value)
			return p
		esac
	fi

	if isintcc(t) then
		coercebasetype(p)
		q:=createunit1(jabs,p)
	else
		terror("abs bad type")
	fi

	q.mode:=p.mode
	return q
end

function createinotop(unit p)unit=
	unit q
	int t

	t:=ttbasetype[p.mode]

	if p.tag=jconst then
		case t
		when ti32,ti64,tu32,tu64 then
			p.value:=inot p.value
			return p
		esac
	fi
	if isintcc(t) then
		coercebasetype(p)
		q:=createunit1(jinot,p)
	else
	cpl strmode(t)
		terror("! bad type")
	fi

	q.mode:=p.mode
	return q
end

function createptrop(unit p)unit=
	unit q
	int t,m

	if not ttisref[t:=p.mode] then
		PRINTUNIT(NIL,P)
		terror("* not pointer")
	fi
	m:=tttarget[t]

	case p.tag
	when jaddrof then
		q:=p.a
		q.mode:=tttarget[p.mode]
		fixmemopnd(q)
		return q
	esac

	q:=createunit1(jptr,p)
	q.mode:=m
	q:=arraytopointer(q)
	fixmemopnd(q)

	return q
end

function createincrop(int opc,unit p)unit=
!opc is jpreincr/decr or jpostincr/decr
	unit q
	int t

	t:=p.mode

	checklvalue(p,1)

	unless isintcc(t) and t<>tbool or ttisref[t] then
		terror("++ bad type")
	end unless
	q:=createunit1(opc,p)
	q.mode:=p.mode

	return q
end

function createaddrofop(unit p)unit=
	ref strec d
	unit q
	int t,u,alength

	alength:=0

	restartx:
	t:=p.mode
	if p.memmode then t:=p.memmode fi

	switch p.tag
	when jname then
P.DEF.ADDROF:=1
		if p.alength then
			t:=p.def.mode
			alength:=p.alength
		fi

	when jaddrof then
		if p.a.tag=jname and p.a.alength then		!sounds like ANAME => &ANAME
			p.mode:=createrefmode(p.a.def.mode)
	p.alength:=p.a.alength
			return p
		fi
	when jdot then
		q:=p.a
		if q.tag=jptr and q.a.tag=jconst then
			p:=createconstunit(p.offset+q.a.value, ti32)
			return p
		fi
		goto cad1
	when jaddptr then
		if p.alength then
			p.mode:=createrefmode(createarraymode(tttarget[p.mode],p.alength))
			return p
		fi
	when jwidenmem then
		p:=p.a
		goto restartx
	when jfuncname then
		return p
	else

	cad1:
		checklvalue(p)
	end switch

	p:=createunit1(jaddrof,p)
	p.mode:=createrefmode(t)
	p.alength:=alength

	return p
end

function createaddop(unit x,y)unit=
	unit z
	int s,t,u,opc,elemsize

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]
	opc:=jadd

	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)

	elsif s=tref then
	doaddref:
		u:=x.mode
		elemsize:=ttsize[tttarget[u]]
		if x.tag=jconst and y.tag=jconst then
			x.value +:=y.value*elemsize
			return x
		fi

		y:=coercemode(y,tptroffset)

		z:=createunit2(jaddptr,x,y)
		z.mode:=u
		z.ptrscale:=elemsize
		return z

	elsif t=tref then
		swap(x,y)
		goto doaddref
		terror("Sub bad types")
	fi

	if x.tag=jconst then
		if y.tag=jconst then
			return eval_add(opc,x,y,u)
		else
			swap(x,y)
		fi
		if y.value=0 then			!works for int/float
			return x				!x+0 => x
		fi
	fi
	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createsubop(unit x,y)unit=
	unit z
	int s,t,u,opc,elemsize

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]
	opc:=jsub

	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	elsif s=tref then
		if t<>tref then
			u:=x.mode
			elemsize:=ttsize[tttarget[u]]
			y:=coercemode(y,tptroffset)

			z:=createunit2(jsubptr,x,y)
			z.mode:=u
			z.ptrscale:=elemsize
			return z

		else							!ref-ref
			if x.tag=jconst and y.tag=jconst then
				x.value -:= y.value/ttsize[tttarget[x.mode]]
				x.mode:=ti32
				return x

			else
				z:=createunit2(opc,x,y)
				z.mode:=tptroffset
				z:=divunit(z,tttarget[x.mode])
				z.mode:=tptroffset
				return z
			fi
		fi
		y:=mulunit(y,tttarget[x.mode])
	else
		terror("Sub bad types")
	fi

	if x.tag=jconst and y.tag=jconst then
		return eval_sub(opc,x,y,u)
	fi
	if y.tag=jconst and y.value=0 then			!works for int/float
!		return x				!x-0 => x
	fi

	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createmulop(unit x,y)unit=
	unit z
	int s,t,u,opc

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]

	opc:=jmul
	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Mul bad types")
	fi

	if x.tag=jconst then
		if y.tag=jconst then
			return eval_mul(opc,x,y,u)
		else
			swap(x,y)
		fi
	fi

	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createdivop(unit x,y)unit=
	unit z
	int s,t,u,opc

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]

	opc:=jdiv
	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Div bad types")
	fi

	if x.tag=jconst and y.tag=jconst then
		return eval_div(opc,x,y,u)
	elsif y.tag=jconst and u=tr64 then
		opc:=jmul
		y.xvalue:=1.0/y.xvalue

	fi
	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createremop(unit x,y)unit=
	unit z
	int s,t,u,opc

	s:=ttbasetype[x.mode]
	t:=ttbasetype[y.mode]

	opc:=jrem
	if u:=dominantmode[s,t] then			!were both numeric
		if u=tr64 or u=tr32 then
			u:=ti32
		fi
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Rem bad types")
	fi

	if x.tag=jconst and y.tag=jconst then
		return eval_rem(opc,x,y,u)
	fi
	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

proc insertunit(unit p, int tag)=
!wrap extra unit around p, using given tag
	unit q
	q:=createunit0(0)			!empty unit
	q^:=p^
	p.tag:=tag
	p.a:=q
	p.b:=p.c:=nil
	p.lineno:=q.lineno
	p.nextunit:=q.nextunit
	p.memmode:=0

	q.nextunit:=nil
end

function eval_add(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64,tu32,tu64 then
		x.value +:= y.value
		return x
	when tr64 then
		x.xvalue +:= y.xvalue
		return x
	elsif ttbasetype[t]=tref then	!assume y is const 0 int of any sub-type
		x.value +:= y.value*ttsize[tttarget[t]]
		return x			!will not change x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_sub(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64,tu32,tu64 then
		x.value -:= y.value
		return x
	when tr64 then
		x.xvalue -:= y.xvalue
		return x
	elsif ttbasetype[t]=tref then
		if ttbasetype[y.mode]=tref then
			terror("EVALSUB/REF")
		fi
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_mul(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64,ti16,ti8 then
		x.value *:= y.value
		return x
	when tu32,tu64,tu16,tu8 then
		x.uvalue := x.uvalue*y.uvalue
		return x
	when tr64 then
		x.xvalue *:= y.xvalue
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_div(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64 then
		if y.value=0 then serror("div 0") fi
		x.value := x.value/y.value
		return x
	when tu32,tu64 then
		if y.value=0 then serror("div 0") fi
		x.uvalue := x.uvalue/y.uvalue
		return x
	when tr64 then
		x.xvalue /:= y.xvalue
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_rem(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64 then
		if y.value=0 then serror("rem 0") fi
		x.value := x.value rem y.value
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_convert(unit p, int t,opc)int=
!p contains a const unit, t is a target type, opc is conv op
!try and convert if possible
!return 1 if converted
	int s

	if opc=soft_c then
	dosoft:
		p.mode:=t
		return 1
	fi

	s:=p.mode
	if s=t then return 1 fi

	case s
	when ti32,ti16,ti8,ti64 then
		case t
		when tr64,tr32 then
			p.xvalue:=p.value
			p.mode:=t
			return 1
		when tu64,ti64,tu32,ti32,ti16,ti8,tu8,tu16 then
	dotrunc:
			case ttsize[t]
			when 1 then
				p.value iand:=255
				if stdsigned[t] then
					p.value:=i8(p.value)
				fi
			when 2 then
				p.value iand:=65535
				if stdsigned[t] then
					p.value:=i16(p.value)
				fi
			when 4 then
				p.value :=p.value iand 0xFFFF'FFFF
				if stdsigned[t] then
					p.value:=i32(p.value)
				fi
			esac
			goto dosoft
		esac
		if ttisref[t] then
			p.mode:=t
			return 1
		fi

	when tu32,tu8,tu16,tu64 then
		case t
		when tr64,tr32 then

			RETURN 0
			p.mode:=t
			return 1
		when tu64,ti64,ti32,tu32,tu64,tu16,ti8,tu8,ti16 then
			goto dotrunc
		esac
		if ttisref[t] then
			p.mode:=t
			return 1
		fi

	when tr64 then
		case t
		when ti32,ti64 then
			p.value:=p.xvalue
			p.mode:=t
			return 1
		when tu32,tu64 then
			p.value:=p.xvalue
			p.mode:=t
			return 1
		when tr32 then
			p.mode:=tr32
			return 1
		esac
	elsif ttisref[p.mode] then
		if not p.isstrconst then
			case t
			when ti32,ti64,tu32,tu64 then
				p.mode:=t
				return 1
			esac
		fi
	esac

	return 0
end

proc coercecond(unit p)=
!p is an expression used as a condition
!Ensure result is i32; it doesn't need to be 0 or 1
!Anything else has istrue added

	int t
	if (t:=p.mode)=ti32 then return fi

	retry:
	case ttbasetype[t]
	when tr32,tr64,tref then
		goto doint

	elsif isintcc(t) then
	doint:
		if p.tag=jconst and p.value then			!check all types as one 64-bit field
			p.value:=1
		elsif p.tag=jconst and not p.value then			!check all types as one 64-bit field
			p.value:=0
		else
			insertunit(p,jistruel)
		fi
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
	else

		serror_s("Invalid condition #",strmode(t))
	esac
	p.mode:=ti32
end

proc coercebasetype(unit p)=
	int t

	if (t:=p.mode)>=ti8 and t<=ti16 then
		p:=coercemode(p,ti32)
	elsif t>=tbool and t<=tu16 then
		p:=coercemode(p,tu32)
	fi
end

proc checklvalue(unit p, int assign=0)=

	case p.tag
	when jname then
	when jptr then

	when jfuncname then
		if assign then notlv fi

	when jwidenmem then
		case p.a.tag
		when jname,jptr,jdot then
			p^:=p.a^
		else
			terror("CHECKLV/WIDEN")
		esac

	when jdot then

	when jconst then
		if not ttisref[p.mode] then
			goto notlv
		fi
	when jconvert then
		if assign then notlv fi

	else
	notlv:
		printunit(nil,p)
		terror_s("value: #",jtagnames[p.tag])
	esac
end

function createcall(unit p,q)unit=
!p is unit on left of param list, while q is the param list as the head of a unitlist
!do type-checking on params, and construct and return callfn unit
!p can be a simple name, or an expression that should be a function po inter
	unit r,s,u
	ref strec d
	ref paramrec pm
	int i,nparams,aparams,retmode,mproc,m,c
	[1024]char str
	ichar ss,tt,uu
	ref strbuffer exprstr

	d:=nil

	case p.tag
	when jptr then
	doptr:
		mproc:=p.mode

		while ttbasetype[mproc]=tref do
			r:=createunit1(jptr,p)
			mproc:=tttarget[mproc]
			r.mode:=mproc
			p:=r
		od

		if ttbasetype[mproc]<>tproc then
			serror_s("Not function pointer: #",typename(mproc))
		fi

		pm:=ttparams[mproc]
		retmode:=tttarget[mproc]

	when jname,jfuncname then
		d:=p.def
		if d.nameid=procid then
			pm:=d.paramlist
			retmode:=d.mode
		else							!assume fnptr, but needs ptr unit
			goto doptr
		fi
	when jdot,jcallfn,jifx,jconvert,jexprlist then
		r:=createunit1(jptr,p)
		r.mode:=tttarget[p.mode]
		p:=r
		goto doptr

	else
		CPL =JTAGNAMES[P.TAG]
		PRINTUNIT(NIL,P)
		serror("ccall?")
	esac

	nparams:=pm.nparams
	aparams:=0

	s:=q
	while s do
		++aparams				!number of actual params supplied
		s:=s.nextunit
	od

	if aparams<nparams then
		terror("1:Too few args")
	elsif aparams>nparams and pm.flags<>pm_variadic and pm.flags<>pm_notset then
		if pm.flags<>pm_notset then
			cpl aparams,nparams


			terror("Too many args")
		fi
	fi

	s:=q

	for i:=1 to aparams do
		if i<=nparams then
			coercemode_inplace(s,pm.mode)
			pm:=pm.nextparam
		else					!assume variadic param
			if s.mode=tvoid then
				terror("Variadic param is void")
			fi
			coercebasetype(s)
		fi
		s:=s.nextunit
	od

	r:=createunit2(jcallfn,p,q)
	r.mode:=retmode
	fixmemopnd(r)
	r.aparams:=aparams

	return r
end

function arraytopointer(unit p)unit=
	unit q
	int offset
	int t,elemmode,refmode

	t:=p.mode
	elemmode:=tttarget[t]

	if ttbasetype[t]=tarray then
		refmode:=createrefmode(elemmode)
		case p.tag
		when jptr then
			p:=p.a

		when jdot then						!about to access array field
			offset:=p.offset
			p.tag:=jaddptr
			p.ptrscale:=1	!ttsize[elemmode]
			q:=createunit1(jaddrof,p.a)
			q.mode:=refmode
			p.a:=q
			p.b:=createconstunit(offset,ti32)

		else
			CPL "ATP:"
			printunit(nil,p)
			terror("ATP?")
		esac

		p.mode:=refmode
		p.alength:=ttlength[t]

	fi
	return p
end

function createindexop(unit p,q)unit=
!do p[q]
!convert to *(p+q)
	unit a

	a:=createaddop(p,q)
	return createptrop(a)
end

function readstructdecl(ref strec owner)int=
	ref strec d,e,currrecord
	ref strec ulist,ulistx,tagowner
	int funion,linkage,mbase,m
	int offset,recsize,maxsize,maxalignment,alignment,size
	ref paramrec pm
	ref fieldrec fieldlist,fl

	funion:=(lx.symbol=kunionsym)

	lex()				!skip 'struct' etc

	tagowner:=(currproc|currproc|stmodule)

	if lx.symbol=lcurlysym then				!anonymous struct tag
		d:=addnamestr(nextautotype())
	else
		checksymbol(namesym)
		d:=lx.symptr		!should be struct tag
		lex()

		if lx.symbol<>lcurlysym then			!reading incomplete enum
			e:=resolvename(tagowner,d,ns_tags,currblockno)
			if e then
				if e.nameid<>structtagid then
					serror_s("Struct tag in use #",e.name)
				fi

				return e.mode
			fi
!create new incomplete tag
			e:=createdupldef(tagowner,d,structtagid)
			e.mode:=createstructmode(e,(funion|tunion|tstruct))
			e.blockno:=currblockno
			blockcounts[currblockno]:=1
			return e.mode
		fi
	fi

!{ seen, so defining a new struct

	e:=checkdupl(tagowner,d,ns_tags,currblockno)

	if e then			!found in this linkage
		if e.nameid<>structtagid then
			serror_s("Struct tag in use #",e.name)
		fi
		if e.deflist then					!else filling in incomplete enum
			cpl "Prev",e.lineno iand 1677215, sourcefilenames[e.lineno>>24],sourcefilepaths[e.lineno>>24]
			serror_s("Redefining struct #",e.name)
		fi
	else						
		e:=createdupldef(tagowner,d,structtagid)
		e.mode:=createstructmode(e,(funion|tunion|tstruct))
		e.blockno:=currblockno
		blockcounts[currblockno]:=1
	fi

!e points to an def which has an empty {...} list
	lex()							!skip {

	currrecord:=e
	ulist:=ulistx:=nil
	offset:=maxsize:=recsize:=0
	maxalignment:=1
	fieldlist:=nil
	m:=-1

	while lx.symbol<>rcurlysym do
		mbase:=readdeclspec(currrecord,linkage)

		docase lx.symbol
		when namesym, mulsym, lbracksym then

			m:=readtype(currrecord,d,mbase,pm)
			if d=nil then
				serror("Field name expected")
			fi

			if linkage=typedef_ss or pm then
				serror("typedef or function inside struct")
			fi

			e:=checkdupl(currrecord, d, ns_fields, 0)

			if e then					!already exists
				serror_s("member name in use #",e.name)
			fi

			if linkage<>none_ss then
				serror("Can't use ss in struct")
			fi

	addanonfield:
			d:=createdupldef(nil,d,fieldid)
			d.mode:=m
!name is not linked in to record as they must be in sequence
			addlistdef(&ulist,&ulistx,d)
			currrecord.deflist:=ulist				!needed for dupl checking
			currrecord.deflistx:=ulistx
			d.owner:=currrecord
			alignment:=getalignment(m)
			if alignment>maxalignment then maxalignment:=alignment fi

			d.offset:=roundoffset(offset,alignment)
			size:=ttsize[m]
			recsize+:=d.offset-offset
			offset:=d.offset

			addnewfield(fieldlist,d,offset)

			if funion then
				maxsize:=max(maxsize,size)
			else
				offset+:=size
				recsize+:=size
			fi

			if lx.symbol=colonsym then
				lex()
				readassignexpr()
			fi

			case lx.symbol
			when commasym then			!read next item
				lex()
			else
				skipsymbol(semisym)
				exit
			esac
		when colonsym then				!apparently int:24 is allowed, with no member name
			lex()
			readassignexpr()
			skipsymbol(semisym)
			exit
		else
			case ttbasetype[mbase]
			when tstruct, tunion then		!assume defining a [part]type only
				d:=getautofieldname()
				m:=mbase
				goto addanonfield
			else
				if m=-1 then
					serror("Struct decl error")
				else
					serror_s("Struct decl error #",typename(m))
				fi
			esac
		end docase
	od

	skipsymbol(rcurlysym)

	currrecord.nextfield:=fieldlist
	ttsize[currrecord.mode]:=roundoffset((funion|maxsize|recsize),maxalignment)
	currrecord.align:=maxalignment

	m:=currrecord.mode
	if ttconsttype[m] then
		ttsize[ttconsttype[m]]:=ttsize[m]
	fi

	return currrecord.mode
end

function checkpointertypes(int s,t,hard)int=
!return 1 if pointer types s and t are compatible
!it is assumed that s is to be converted to t, or passed as a parameter expecting t
	int starget:=tttarget[s], ttarget:=tttarget[t]
	int sbase, tbase
	int sconst:=0,tconst:=0

	if ttconst[starget] then
		starget:=ttconsttype[starget]
		sconst:=1
	fi
	if ttconst[ttarget] then
		ttarget:=ttconsttype[ttarget]
		tconst:=1
	fi

	if not hard and sconst and not tconst then
		cpl strmode(s)
		cpl strmode(t)
		terror("const to non-const pointer")
	fi

	if starget=ttarget then return 1 fi

	s:=starget
	t:=ttarget
	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]

	if sbase in tfirstint..tlastint and tbase in tfirstint..tlastint then
		if ttsize[sbase]=ttsize[tbase] then		!ignore sign differences
			return 1
		fi
	fi

	if sbase=tvoid or tbase=tvoid then
		return 1
	fi

	if ttisref[s] and ttisref[t] then
		return checkpointertypes(s,t,hard)

	elsif ttbasetype[s]=tarray and ttbasetype[t]=tarray then
		if ttlength[s]<>ttlength[t] then
			if ttlength[s] and ttlength[t] then		!allow one dim to be 0
CPL "BAD REF[]"
RETURN 1
				return 0
			fi
		fi
		starget:=tttarget[s]
		ttarget:=tttarget[t]
		if starget=ttarget then return 1 fi
 
		if ttisref[starget] and ttisref[ttarget] then
			return checkpointertypes(starget,ttarget,hard)
		fi
		if ttbasetype[starget]=tarray and ttbasetype[ttarget]=tarray then
			return checkpointertypes(starget,ttarget,hard)
		fi
	elsif ttbasetype[s]=tproc and ttbasetype[t]=tproc then
		return 1				!NEED PROPER MATCH HERE
	fi

	return 0
end

function comparemode(int s,t)int=
!types s and t don't immediately match
!check further to see if they are compatible
!For example, if they are both arrays, then usually they will have different
!typenumbers. Arrays should match if they have the same element type, and
!same length, or one length is 0
!return 1 for compatible types

	if s=t then return 1 fi			!for when used recursively
	if ttbasetype[s]=tarray and ttbasetype[s]=tarray then
		if comparemode(tttarget[s],tttarget[t])=0 then
			return 0
		fi
		if ttlength[s]=0 or ttlength[t]=0 or ttlength[s]=ttlength[t] then
			return 1
		fi
	fi
	return 0
end

function readenumdecl(ref strec owner)int=
	ref strec d,e

	lex()				!skip 'enum'

	if lx.symbol=lcurlysym then				!anonymous enum tag
		readenumnames(owner)
		return tenum			!return generic enum
	fi

	checksymbol(namesym)
	d:=lx.symptr		!should be enum tag
	lex()

	if lx.symbol<>lcurlysym then			!reading incomplete enum
		e:=checkdupl(owner, d, ns_tags, currblockno)

		if e then
			if e.nameid<>enumtagid then
				serror_s("Enum tag in use #",e.name)
			fi
		fi

!create new incomplete enum tag
		e:=createdupldef(owner,d,enumtagid)
		e.mode:=createenummode(e)
		e.blockno:=currblockno
		blockcounts[currblockno]:=1
		return e.mode
	fi

!{ seen, so defining a new enum
	e:=checkdupl(owner,d,ns_tags,currblockno)

	if e then			!found in this linkage
		if e.nameid<>enumtagid then
			serror_s("Enum tag in use #",e.name)
		fi
		if e.deflist then					!else filling in incomplete enum
			serror_s("Redefining enum #",e.name)
		fi
	else						
		e:=createdupldef(owner,d,enumtagid)
		e.mode:=createenummode(e)
		e.blockno:=currblockno
		blockcounts[currblockno]:=1
	fi

!e points to an enum def which has an empty {...} list
!Now loop reading enum values

	readenumnames(owner)

	ttnamedef[e.mode]:=e
	return e.mode
end

proc readenumnames(ref strec owner)=
!at '{'; read set of enum names
	ref strec d,e
	ref strec ulist,ulistx
	int enumseq

	ulist:=ulistx:=nil
	enumseq:=0
	lex()

	case owner.nameid
	when procid,moduleid then		!fine
	else							!probably inside a struct
		owner:=(currproc|currproc|stmodule)
	esac

	while lx.symbol=namesym do
		d:=checkdupl(owner,lx.symptr,ns_general,currblockno)
		if d then
			serror_s("enum name reused #",d.name)
		fi
		d:=createdupldef(owner,lx.symptr,enumid)
		lex()
		if lx.symbol=assignsym then
			lex()
			enumseq:=readconstintexpr()
		fi
		d.index:=enumseq
		d.blockno:=currblockno
		blockcounts[currblockno]:=1
		++enumseq	
		if lx.symbol=commasym then
			lex()
		fi
	od
	skipsymbol(rcurlysym)
end

function createdotop(int opc, unit p,ref strec d)unit=
!opc is jdot or jidot
!Deal with field selection for p.d or p->d
	unit q,r,poffset,pb,pc
	ref strec e,f,prec,panon,pfield,gend
	int m,offset,scale
	ref fieldrec fl

!check that m is a proper pointer if needed, and a struct or union
	m:=p.mode
	if opc=jidot then			!
!	if ttbasetype[m]<>tref then
		if not ttisref[m] then
			serror("-> needs pointer")
		fi
		m:=tttarget[m]
	fi
	case ttbasetype[m]
	when tstruct,tunion then
	else
		serror(". -> not a struct")
	esac

!now need to resolve the field name d
	prec:=ttnamedef[m]				!r is record def

	f:=d
	while f:=f.nextdupl do
		if f.owner=prec then
			offset:=f.offset
			exit
		fi
	od

!not found; look for any anon fields
	if not f then
		gend:=d						!find generic field name version
		while gend.prevdupl do
			gend:=gend.prevdupl
		od

		fl:=prec.nextfield
		while fl do					!now search linear field list matching generic entries
			if fl.gendef=gend then
				f:=fl.def
				offset:=fl.offset
				exit
			fi
			fl:=fl.nextfield
		od
	fi

	if not f then
		terror_ss("Not a field of struct # #",d.name,strmode(m))
	fi


	poffset:=createconstunit(offset,ti32)

!will be p->field, or p.field
!p.field: *(p+offset)

	if opc=jidot then				!apply offset to lhs
		p:=createptrop(p)
	fi

	p:=createunit1(jdot,p)
	p.offset:=offset

	p.mode:=f.mode
	p:=arraytopointer(p)
	fixmemopnd(p)

	return p
end

function mulunit(unit p, int elemtype)unit=
!p is an int unit representing some offset i for *(A+i) or A[i]
!apply a scale so that is a byte offset
!t is the element type
	int elemsize

	if (elemsize:=ttsize[elemtype])<>1 then
		if p.tag=jconst then
			p.value:=p.value*elemsize
		else
			p:=createunit1(jscale,p)
			p.scale:=elemsize
			p.mode:=tptroffset
		fi
	fi
	return p
end

function divunit(unit p, int elemtype)unit=
	int elemsize

	if (elemsize:=ttsize[elemtype])<>1 then
		if p.tag=jconst then
			p.value:=p.value/elemsize
		else
			p:=createunit1(jscale,p)
			p.scale:=-elemsize
			p.mode:=tptroffset
		fi
	fi
	return p
end

function createassignopref(int opc, unit p,q)unit=
!opc is assignsym, addtosym etc
!do assign/addto when is a ref type
!return resulting unit
	int pmode,qmode,rmode,elemmode
	unit r

	pmode:=rmode:=p.mode
	elemmode:=tttarget[pmode]
	qmode:=q.mode

	case opc
	when assignsym then
		q:=coercemode(q,pmode)
		r:=createunit2(jassign,p,q)

	when addtosym then
		if ttisref[qmode] then		!ref+=ref
			serror("ptr+=ptr")
		fi

		q:=coercemode(q,tptroffset)					!ref+=int
		r:=createunit2(jaddto,p,mulunit(q,elemmode))

	when subtosym then
		if ttisref[qmode] then		!ref-=ref
			if not comparemode(pmode,qmode) then
				serror("-= refs don't match")
			fi
			r:=divunit(createunit2(jsub,p,q),elemmode)
			rmode:=ti32
		else								!ref-=int
			r:=createunit2(jsubto,p,mulunit(q,elemmode))
		fi
	else
		serror("Not allowed on ptrs")
	esac

	r.mode:=rmode
	return r
end

proc addnewfield(ref fieldrec &flist, ref strec d, int offset)=
!new field d has just been created for a record
!add it to the linear list of fields for the record
	ref strec e
	ref fieldrec f

	if d.name^<>'$' then			!normal field
		f:=pcm_allocz(f^.bytes)
		f.def:=d
		while d.prevdupl do			!look for generic entry
			d:=d.prevdupl
		od
		f.gendef:=d
		f.offset:=offset

		f.nextfield:=flist
		flist:=f

	else
		e:=ttnamedef[d.mode].deflist
		while e do
			addnewfield(flist,e,offset+e.offset)
			e:=e.nextdef
		od
	fi
end

proc pushloop(int looptype)=
!looptype is 'L' or 'S', ie a switch, so not really a loop
	if loopindex>=maxnestedloops then
		serror("Too many nested loop or switch")
	fi
	++loopindex
	looptypestack[loopindex]:=looptype
	casevaluestack[loopindex]:=nil

end

proc poploop=
	if loopindex then
		--loopindex
	else
		serror("poploop?")
	fi
end

proc addcasevalue(int value)=
	ref caserec p

	int index:=loopindex
	while index and looptypestack[index]<>'S' do
		--index
	od
	if index=0 then serror("case not inside switch stmt") fi

	p:=pcm_alloc(caserec.bytes)
	p.value:=value
	p.nextcase:=casevaluestack[index]
	casevaluestack[index]:=p
end

function roundoffset(int offset, alignment)int=
	int mask

	if structpadding then
		if alignment=1 then return offset fi
		mask:=alignment-1
		while offset iand mask do ++offset od
	fi
	return offset
end

proc fixmemopnd(unit p)=
	int t

!when p refers to a 1- 2- byte value, adjust the type
	if ingeneric then return fi

	case t:= ttbasetype[p.mode]
	when ti8,ti16,tu8,tu16,tbool then
		p.memmode:=t
		p.mode:=ti32
	esac
end

function docast(unit p,int t,hard=1,inplace=0)unit=
!apply cast to unit p
!if no cast needed, then just return p
	unit q
	int s,opc

	s:=p.mode

	retry:

	if s=t then return p fi
	opc:=0

	if s<16 and t<16 then
		opc:=conversionops[s,t]

	elsif ttisref[s] and ttisref[t] then
		if checkpointertypes(s,t,hard) then
			p.mode:=t
			return p
		fi

	elsif ttconst[s] then
		s:=ttconsttype[s]
		goto retry
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
!elsif ttisref[t] and (s>=tfirstint and s<=tlastint) and p.tag=jconst and p.value=0 then
	elsif ttisref[t] and isintcc(s) and p.tag=jconst and p.value=0 then
		opc:=soft_c
	fi

	if opc=0 then
		if not hard then
			cpl strmode(s)
			cpl strmode(t)

	PRINTUNIT(NIL,P)

			terror_ss("Can't do conversion # => #",typename(s),typename(t))
		fi
		opc:=hard_c
	fi

	case p.tag
	when jconst then		!try and convert
		if eval_convert(p,t,opc) then
			return p
		fi
	when jfuncname then
		p.mode:=t
		return p
	when jadd then
		if p.a.tag=jconst and p.b.tag=jconst then
			p.value:=p.a.value+p.b.value
			p.mode:=t
			p.tag:=jconst
			return p
		fi
	esac

	if inplace then
		insertunit(p,jconvert)
		p.convmode:=t
		p.mode:=getpromotedtype(t)
		p.opcode:=opc
		return nil
	else
		q:=createunit1(jconvert,p)
		q.opcode:=opc
		q.convmode:=t
		q.mode:=getpromotedtype(t)
	fi
	return q
end

function coercemode(unit p, int t)unit=
	int s,opc
	unit q

	if p.mode=t then return p fi
	docast(p,t,0,1)
	return p
end

proc coercemode_inplace(unit p, int t)=
	int s,opc
	unit q

	if p.mode=t then return fi
	docast(p,t,0,inplace:1)
end

function createsizeofop(unit p, int islength=0)unit=
	unit q
	int t,size

if islength and p.tag not in [jaddptr, jaddrof] then
printunit(nil,p)
 serror("Not array") fi

	t:=getmemmode(p)

	case p.tag
	when jname then
		if p.alength then
			size:=ttsize[p.def.mode]/p.alength			!take account of array

		else
			size:=ttsize[p.def.mode]			!take account of array
		fi
	when jconst then
		case t
		when trefchar then					!const string
			size:=p.slength
		when trefwchar then
			size:=p.wslength*2
		else
			size:=ttsize[t]
		esac

	when jptr then
		if ttisref[t] and p.alength then		!result of array=>ptr conversion
			size:=ttsize[tttarget[t]]*p.alength
		else
			size:=ttsize[t]
		fi

	when jaddptr then
		if p.alength then	!derived from array expr that converted to pointer
			if islength then
				size:=p.alength
			else
				size:=ttsize[tttarget[t]]*p.alength
			fi

		else
			goto cad1
		fi

	when jaddrof then
		if p.a.tag=jname and p.a.alength then
			if islength then
				size:=p.a.alength
			else
				size:=ttsize[p.a.def.mode]
			fi
		else
			size:=8
		fi

	when jwidenmem then
		return createsizeofop(p.a)

	else
	cad1:
		size:=ttsize[t]
	end case

	q:=createconstunit(size,tu64)
	return q
end

function readgeneric:unit=
!read generic construct; return chosen expr according to type of control expr
!at '_Generic'
	unit pexpr,pmatch,p
	ref paramrec pm
	int m,t,def,oldingeneric,count
	ref strec d

	lex()
	checksymbol(lbracksym)
	lex()
	oldingeneric:=ingeneric
	ingeneric:=1
	pexpr:=readassignexpr()
	ingeneric:=oldingeneric

	m:=pexpr.mode
	pmatch:=nil
	def:=0
	count:=0

	checksymbol(commasym)

	repeat						!at comma
		lex()					!skip comma
		if lx.symbol=kdefaultsym then
			if def then serror("generic/default twice") fi
			def:=1
			if count=0 then t:=-1 else t:=-2 fi
			lex()
		else
			t:=readcasttype(d,0,pm)
		fi
		checksymbol(colonsym)
		lex()
		p:=readassignexpr()

		if (t=-1 or t=m) then

			pmatch:=p
			++count
		fi
	until lx.symbol<>commasym

	checksymbol(rbracksym)
	lex()
	if not pmatch then serror("Generic: no type match") fi
	if count>1 then serror("Generic: multiple types match") fi

	return pmatch
end

global function getmemmode(unit p)int=
!return mode of p, but if p is a widening unit, see past that to
!the original memory mode
	if p.memmode then
		return p.memmode
	else
		return p.mode
	fi
end

func getpromotedtype(int t)int=
!if t is small, get promoted type
	if t=tvoid then return tvoid fi
	if ttsize[t]<4 then				!all 
		return ti32
	fi
	t
end

func readdllexport:int exported=
	exported:=0
	lex()
	checksymbol(lbracksym)
	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name, "dllexport") then
		exported:=1
	fi
	lex()
	checksymbol(rbracksym)
	lex()

	exported
end
=== cc_show.m 0 0 10/24 ===
int currfileno
int currlineno

strbuffer sbuffer
global ref strbuffer dest=&sbuffer
int destlinestart

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar


global proc printcode(filehandle f,ichar caption)=
	int i
	ref strec p

	println @f, caption

	p:=stmodule.deflist

	while p do
		case p.nameid
		when procid then
!		if p.scope<>imported_scope and p.code then
			if p.code then
				println @f,p.name,,"=",scopenames[p.scope]
				printunit(f,p.code,,"1")
				println @f
			fi
		esac
		p:=p.nextdef
	od
end

global proc printunit(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
!p is a tagrec
	ref unitrec q
	ref strec d
	int t,n,lincr
	ichar idname
	ref caserec pc

	if p=nil then
		return
	fi

	if p.tag>=jdummy then
		println "print unit: bad tag",p.tag
		stop 30
	fi

	if p.lineno then
		currlineno:=p.lineno
		currfileno:=p.fileno
	fi

	lincr:=1
	if level<0 then
		lincr:=-1
		print @dev,"             "
	fi

	print @dev,getprefix(abs(level),prefix,p)
	idname:=jtagnames[p.tag]
	if idname^='j' then ++idname fi

	print @dev,idname,,": "

	case p.tag
	when jname, jfuncname then
		d:=p.def

		print @dev,d.name,namenames[d.nameid]

		if d.code then
			print @dev," {",,jtagnames[d.code.tag],,"}"
		fi

		print @dev," ",,getdottedname(d)!,q

		if p.c then
			print @dev," Lastcall:",p.c
		fi

	when jtempdecl, jdecl, jgoto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

		println @dev
		printunit(dev,d.code,level+lincr,"1")
		return

	when jgoto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

	when jlabelstmt then
		print @dev,p.def.name!,"+ LABELED STATEMENT"

	when jcasestmt then
		print @dev,"Value:", p.value

	when jconst then
		t:=p.mode
		if t=trefchar then
			if not p.isstrconst then
				goto doref
			fi
	dostring:
			if p.slength>256 then
				print @dev,"""",,"(LONGSTR)",""" *",,p.slength
			else
				print @dev,"""",,p.svalue,,""" *",,p.slength
			fi
		elsif t=trefwchar then
			if not p.iswstrconst then
				goto doref
			fi
			print @dev,"""",,"(WSTRING)",""" *",,p.wslength
		elsif t>=ti8 and t<=ti64 then
			print @dev,p.value
		elsif t>=tu8 and t<=tu64 then
			print @dev,p.uvalue
		elsif isrealcc(t) then
			print @dev,p.xvalue
		elsif ttbasetype[t]=tref then
			if p.isstrconst then
				goto dostring
			fi
	doref:
			print @dev,ref void(p.value)
		elsif ttbasetype[t]=tarray then
			if p.isstrconst then
				goto dostring
			fi
			serror("PRINTUNIT/CONST/aRRAY")
		else
			cpl typename(t)
			CPL("PRINTUNIT BAD CONST")
!			serror("PRINTUNIT BAD CONST")
		fi
		print @dev," ",,strmode(t)
		if p.isstrconst then print @dev,"<STRCONST>" fi
		if p.iswstrconst then print @dev,"<WSTRCONST>" fi

	when jconvert then
		print @dev,convnames[p.opcode]
		print @dev," "
		print @dev,typename(p.a.mode)
		print @dev," => "
		print @dev,typename(p.convmode)

	when jscale then
		print @dev,"Scale:",p.scale

	when jaddptr,jsubptr then
		print @dev,"Ptrscale:",p.ptrscale

	when jswitch then
		pc:=p.nextcase
		n:=0
		while pc do ++n; pc:=pc.nextcase od

		print @dev,n

	when jcallfn then
		print @dev," Aparams:",p.aparams

	when jptr then

	when jdot then
		print @dev," Offset:",p.offset

	esac

	if p.memmode then
		print @dev, " Widen from:",strmode(p.memmode)
	fi

	if p.alength then print @dev," ALENGTH=",p.alength fi

	println @dev

	printunitlist(dev,p.a,level+lincr,"1")
	printunitlist(dev,p.b,level+lincr,"2")
	if p.tag<>jblock then					!.c is used to point to last element
		printunitlist(dev,p.c,level+lincr,"3")
	fi
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
	if p=nil then return fi

	while p do
		printunit(dev,p,level,prefix)
		p:=p.nextunit
	od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
	static [512]char str
	[512]char indentstr
	ichar modestr
	int length

	indentstr[1]:=0
	if level>10 then level:=10 fi

	strcpy(indentstr,"-----------------------")

	modestr:=strmode(p.mode,0)
	length:=strlen(modestr)
	if length<strlen(indentstr) then
		memcpy(indentstr,modestr,length)
	else
		strcpy(indentstr,modestr)
	fi

	to level do
		strcat(indentstr,"|---")
	od

	strcpy(str,getlineinfok())
	strcat(str,indentstr)
	strcat(str,prefix)
	if prefix^ then
		strcat(str," ")
	fi

	return str
end

global function getdottedname(ref strec p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	ref strec owner

	strcpy(str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(str2,str)
		strcpy(str,owner.name)
		strcat(str,".")
		strcat(str,str2)
		owner:=owner.owner
	od
	if p.blockno then
		print @str2,".",,p.blockno
		strcat(str,str2)
	fi
	return str
end

function getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @str,"# ",currfileno,currlineno:"z5",$
	return str
end

global proc printst(filehandle f,ref strec p,int level=0)=
	ref strec q

	if p.symbol<>namesym then
		mcerror("PRINTST not name")
	fi

	printstrec(f,p,level)

	q:=p.deflist

	while q<>nil do
		printst(f,q,level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f,ref strec p,int level)=
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset
	const tabstr="    "
	[256]char str
	int scope
	ref paramrec pm

	gs_init(d)

	offset:=0
	to level do
		gs_str(d,tabstr)
		offset+:=4
	od
	gs_str(d,":")

	if p.blockno then
		print @str,p.name,,".",,p.blockno

		gs_leftstr(d,str,28-offset,'-')
	else
		gs_leftstr(d,p.name,28-offset,'-')
	fi
	gs_leftstr(d,namenames[p.nameid],12,'.')
	col:=gs_getcol(d)

	gs_str(d,"[")

	gs_str(d,scopenames[p.scope])
	gs_str(d," ")
	gs_str(d,strint(p.exported))
	gs_str(d," ")

	if p.align then
		gs_str(d,"@@")
		gs_strint(d,p.align)
		gs_str(d," ")
	fi
	if p.varparams then
		gs_str(d,"Var ")
	fi
	if p.used then
		gs_str(d,"Used ")
	fi
	if p.nparams then
		fprint @str,"Pm:# ",p.nparams
		gs_str(d,str)
	fi

	gs_str(d,"]")
	gs_padto(d,col+10,'=')

	if p.owner then
		fprint @str,"(#)",p.owner.name
		gs_leftstr(d,str,18,' ')
	else
		gs_leftstr(d,"()",18,' ')
	fi

	case p.mode
	when tvoid then
		gs_str(d,"Void ")
	else
		gs_strsp(d,strmode(p.mode))
	esac

	case p.nameid
	when fieldid then
		gs_str(d,"Offset:")
		gs_strint(d,p.offset)

	when frameid,paramid then
		if p.code then
			gs_str(d,"= ...")

		fi
		gs_str(d," Offset: ")
		gs_strint(d,p.offset)

	when procid then

		gs_str(d,"Index:")
		gs_strint(d,p.index)

	when enumid then
		gs_str(d,"Enum:")
		gs_strint(d,p.index)

	when staticid then
		if p.code then
			gs_str(d,"= ...")
!			gs_strvar(d,strexpr(p.code))
		fi
		gs_str(d,"STATIC********")
	esac

	gs_str(d," ")

	gs_str(d,"Lineno:")
	gs_strint(d,p.lineno iand 16777215)
	gs_str(d," ")
	gs_str(d,sourcefilenames[p.lineno>>24])

	if p.nameid=procid then
		gs_line(d)
		pm:=p.paramlist
		while pm do
			gs_str(d,"		Param: ")
			gs_leftstr(d,(pm.def|pm.def.name|"Anon"),10,'-')
			gs_str(d,pmflagnames[pm.flags])
			gs_str(d," Mode:")
			gs_str(d,strmode(pm.mode))
			gs_str(d," Code:")
			gs_strint(d,cast(p.code))

			gs_line(d)
			pm:=pm.nextparam
		od
	fi

	gs_println(d,f)

	if p.code then
		case p.nameid
		when frameid,staticid then
			printunit(f,p.code,-3)
		esac
	fi
end

global proc printstflat(filehandle f)=
	int i
	ref strec p
	ref tokenrec lx
	println @f,"GLOBAL SYMBOL TABLE:"

	for i:=0 to hstsize-1 do
		p:=hashtable^[i]
		if p.name then
			case p.symbol
			when namesym,ktypespecsym, ksourcedirsym then
				println @f,i,p,":",getstname(p),symbolnames[p.symbol],namenames[p.nameid]
				p:=p.nextdupl
				while p do
					print   @f,"	",p,getstname(p),symbolnames[p.symbol],namenames[p.nameid],
						p.prevdupl
					println @f,"(From",(p.owner|getstname(p.owner)|"-"),,")"
					p:=p.nextdupl
				od
			esac
		fi
	od
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s",symbolnames[l.symbol])

	case l.symbol
	when namesym then
		printstrn(l.symptr.name,l.symptr.namelen)

	when intconstsym then
		print l.value,," "
		shownumberstr(lp)

	when realconstsym then
		print l.xvalue,," "
		shownumberstr(lp)

	when stringconstsym then
		print """"
		printstrn(l.svalue,l.length)
		print """"
	when charconstsym then
		print "'"
		printstrn(l.svalue,l.length)
		print "'"

	elsif l.subcode then
		print "#",l.subcode
	end

	println
end

global proc PS(ichar caption)=
	print caption,,":::"
	printsymbol(&lx)
end

global proc PSNEXT(ichar caption)=
	print caption,,":##"
	printsymbol(&nextlx)
end

!proc showast=
!	if fshowast then
!		printcode(logdev,"PROC AST")
!		println @logdev
!	fi
!end
!
!proc showst(ichar caption)=
!	println @logdev,"PROC",caption
!	printst(logdev, stmodule)
!	println @logdev
!end

proc shownumberstr(ref tokenrec l,filehandle f=nil)=
	ref char s

	if getfilenox(l) then
		s:=sourcefiletext[getfilenox(l)]+getnumberoffsetx(l)
	else
		s:=pastedtokenlist[l.pasteno]
	fi
	printstrn(s,l.length,f)

end

=== cc_showdummy.m 0 0 11/24 ===
strbuffer sbuffer
global ref strbuffer dest=&sbuffer

int currlineno

global proc printcode(filehandle f,ichar caption)=
end

global proc printunit(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=
end

global function strexpr(ref unitrec p)ref strbuffer=
	nil
end

global proc printst(filehandle f,ref strec p,int level=0)=
end

global proc printfilelist(filehandle f)=
end

global proc closelogfile=
end

global proc printsymbol(ref tokenrec lp)=
end

global proc PS(ichar caption)=
end

global proc PSNEXT(ichar caption)=
end

=== cc_support.m 0 0 12/24 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global proc mcerror(ichar mess)=
	println "\nMC Error:",mess
!os_getch()
	stop 40
end

global proc serror(ichar mess)=
	serror_gen(mess)
end

global proc serror_gen(ichar mess)=
	if currproc then
		print "\nIn function",currproc.name,," "
	ELSE
		CPL "OUTSIDE PROC"
	fi

	println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]
!	showmacrolineno()

	println
	println "**** Syntax Error:",mess,"****"
	println

	stop 1
end

global proc serror_ss(ichar mess,a,b)=
	[256]char str
!	sprintf(str,mess,a,b)
	fprint @str, mess,a,b
	serror_gen(str)
end

global proc serror_s(ichar mess,a)=
	[256]char str
!	sprintf(str,mess,a)
	fprint @str, mess, a
	serror_gen(str)
end

global proc terror_gen(ichar mess)=

	if currproc then
		println "\nIn function",currproc.name
	fi

	println "Type error:",mess,"on line",lx.lineno,sourcefilepaths[lx.fileno]

!	showmacrolineno()

	stop 1
end

global proc terror(ichar mess)=
	terror_gen(mess)
end

global proc terror_s(ichar mess,a)=
	[256]char str

	fprint @str, mess, a
	terror_gen(str)
end

global proc terror_ss(ichar mess,a,b)=
	[256]char str

	fprint @str, mess, a, b
	terror_gen(str)
end

global proc gerror_gen(ichar mess,ref unitrec p=nil)=
	int lineno,fileno

	if p then
		lineno:=p.lineno
		fileno:=p.fileno
	else
		lineno:=clineno
		fileno:=cfileno
	fi

	if currproc then
		print "In function",currproc.name,," "
	fi

	println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
	println
	println "**** Code Gen Error:",mess,"****"
	stop 1
end

global proc gerror(ichar mess,ref unitrec p=nil)=
	gerror_gen(mess,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
	[256]char str

	fprint @str, mess, s
	gerror_gen(str,p)
end

global proc loaderror(ichar mess,mess2="")=
	[512]char str

	fprint @str, mess, mess2
	println "Load Error:",str
	println "Stopping"
	stop 45
end

global function loadsourcefile(ichar file,shortfile)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
	ichar s

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles
	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(file)
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

	s:=cast(readfile(file))			!will overallocate by a few bytes
	if not s then				!unexpected error
		loaderror("LSF can't load ",file)
	fi

	sourcefiletext[nsourcefiles]:=s
	sourcefilesizes[nsourcefiles]:=rfsize
	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)

	return nsourcefiles
end

=== cc_tables.m 0 0 13/24 ===
global enumdata [0:]ichar stdtypenames, [0:]byte stdtypewidths,
		 [0:]byte stdsigned, [0:]byte stdpcl =

!                              bts  si  pcl      size
	(tvoid=0,		"void",		0,	0,	tpvoid),

	(ti8,			"i8",		8,	1,	tpi8),		! This ordering is important
	(ti16,			"i16",		16,	1,	tpi16),		!
	(ti32,			"i32",		32,	1,	tpi32),		!
	(ti64,			"i64",		64,	1,	tpi64),		!

	(tbool,			"bool",		8,	0,	tpu8),		! As is this
	(tu8,			"u8",		8,	0,	tpu8),		!
	(tu16,			"u16",		16,	0,	tpu16),		!
	(tu32,			"u32",		32,	0,	tpu32),		!
	(tu64,			"u64",		64,	0,	tpu64),		!

	(tr32,			"r32",		32,	0,	tpr32),		! And tr32 must be >= integer types
	(tr64,			"r64",		64,	0,	tpr64),		!

	(tenum,			"enum",		0,	0,	tpi32),		!
	(tref,			"ref",		64,	0,	tpu64),		! 
	(tproc,			"proc",		64,	0,	tpvoid),	!
	(tlabel,		"label",	64,	0,	tpvoid),	!
	(tblock,		"block",	0,	0,	tpblock),	!

	(tarray,		"array",	0,	0,	tpblock),	!
	(tstruct,		"struct",	0,	0,	tpblock),	!
	(tunion,		"union",	0,	0,	tpblock),	!

	(tnotset,		"notset",	0,	0,	tpvoid),	!

!User-defined types go here
	(tlast,			$,			0,	0,	tpvoid)		!

end

global const tchar=ti8
global const tfirstnum=ti8, tlastnum=tr64
global const tfirstint=ti8, tlastint=tu64
global const tfirstreal=tr32, tlastreal=tr64

global const tptroffset = ti64		!for 64-bit target

global enumdata []ichar typespecnames, []i32 typespectypes, []byte typespecsizes =
	(ts_void,		$,	tvoid,		0),
!	(ts_char,		$,	tu8,		1),
	(ts_char,		$,	ti8,		1),
	(ts_short,		$,	0,			2),
	(ts_long,		$,	0,			4),
!	(ts_long64,		$,	0,			8),
	(ts_int,		$,	ti32,		4),
	(ts_float,		$,	tr32,		4),
	(ts_double,		$,	tr64,		8),
	(ts_signed,		$,	0,			0),
	(ts_unsigned,	$,	0,			0),
	(ts_bool,		$,	tbool,		1),
	(ts_user,		$,	0,			0),
	(ts_struct,		$,	0,			0),
	(ts_union,		$,	0,			0),
	(ts_enum,		$,	0,			4),
	(ts_atomic,		$,	0,			0)
end

global enumdata [0:]ichar pmflagnames=
	(pm_normal=0,		$),		! Normal param
	(pm_notset,			$),		! ()     (applied to one dummy tnone param)
	(pm_empty,			$),		! (void) (applied to one dummy tnone param)
	(pm_variadic,		$)		! (...) or (t,u,v,...) (applied to dummy or first param)
end

!scope here refers to linkage across modules
global enumdata [0:]ichar scopenames=
	(no_scope=0,		"-"),		! 
	(function_scope,	"Fn"),		!within a function (note import/exported names can be declared in a block scope)
	(local_scope,		"Loc"),		!file-scope/not exported 
	(imported_scope,	"Imp"),		!imported from another module
	(exported_scope,	"Exp")		!file-scope/exported
end

global enumdata =
	none_ss=0,
	static_ss,
	auto_ss,
	register_ss,
	extern_ss,
	typedef_ss,
end

global enumdata =
	const_qual,
	volatile_qual,
	restrict_qual,
	atomic_qual,
end

global enumdata =
	inline_fnspec,
	noreturn_fnspec,
	callback_fnspec,
end

global enumdata =
	pdm_date,
	pdm_time,
	pdm_file,
	pdm_line,
	pdm_func,
	pdm_cdecl,
	pdm_mcc,
	pdm_stdc
end

global enumdata [0:]ichar jtagnames=

	(jnone=0,		$), !
	(jconst,		$), !
	(jnull,		$), !
	(jname,		$), !
	(jwidenmem,	$), !
	(jfuncname,	$), !
	(jblock,		$), !
	(jtempdecl,	$), !
	(jdecl,		$), !

!Statements

	(jreturn,		$), ! 
	(jreturnx,		$), ! 

	(jassign,		$), ! 
	(jif,			$), ! 
	(jfor,			$), ! 
	(jwhile,		$), ! 
	(jdowhile,		$), ! 
	(jgoto,		$), ! 
	(jlabelstmt,	$), ! 
	(jcasestmt,	$), ! 
	(jdefaultstmt,	$), ! 
	(jbreak,		$), ! [
	(jcontinue,	$), ! [
	(jswitch,		$), ! 
	(jbreaksw,		$), ! [
!	(jeval,		$), ! 

!Expressions and Operators

!Logical Operators

	(jandl,		"&& andl"), ! 
	(jorl,			"|| orl"), ! 
	(jnotl,		"! notl"), ! 
	(jistruel,		$), ! 

!Expressions and Operators

	(jmakelist,	$), ! 
	(jexprlist,	$), ! 

!	(jassignx,		$), ! 
	(jcallfn,		$), ! 
	(jifx,			$), ! 

!Binary Ops

	(jandand,		"&&"), ! a 

	(jeq,			"=="), ! a 
	(jne,			"!="), ! a 
	(jlt,			"<"), ! a 
	(jle,			"<="), ! a 
	(jge,			">="), ! a 
	(jgt,			">"), ! a 

	(jadd,			"+ add"), ! 
	(jsub,			"- sub"), ! 
	(jmul,			"* mul"), ! 
	(jdiv,			"/ div"), ! 
	(jrem,			"% mod"), ! 
	(jiand,		"& iand"), ! 
	(jior,			"| ior"), ! 
	(jixor,		"^ ixor"), ! 
	(jshl,			"<<"), ! a 
	(jshr,			">>"), ! a 

	(jdot,			$), ! 
	(jidot,		$), ! 
	(jindex,		$), ! 

	(jptr,			"ptr"), ! 
	(jaddptr,		"addptr"), ! 
	(jsubptr,		"subptr"), ! 
	(jaddrof,		"addrof &"), ! 
	(jconvert,		$), ! 
	(jscale,		$), ! 

!Monadic Ops

	(jneg,			"- neg"), ! 
	(jabs,			"abs"), ! 
	(jinot,		"~ inot"), ! a

!In-place operators

	(jaddto,		"+="), ! a b	a+:=b
	(jsubto,		"-="), ! a b
	(jmulto,		"*="), ! a b
	(jdivto,		"/="), ! a b
	(jremto,		"%="), ! a b
	(jiandto,		"&="), ! a b
	(jiorto,		"|="), ! a b
	(jixorto,		"^="), ! a b
	(jshlto,		"<<="), ! a b
	(jshrto,		">>="), ! a b

	(jpreincr,		"++ preincr"), ! a	++a
	(jpredecr,		"-- preincr"), ! a	--a
	(jpostincr,	"++ postincr"), ! a	a++
	(jpostdecr,	"-- postdecr"), ! a	a--

	(jsetjmp,		"setjmp"),
	(jlongjmp,		"longjmp"),

	(jdummy,		$)
end

global enumdata []ichar symbolnames, []ichar shortsymbolnames, []byte symboltojtag=

!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$,	"",		0),			! Lex error
	(dotsym,			$,	".",	jdot),		! "."
	(idotsym,			$,	"->",	jidot),	! "->"
	(lexhashsym,		$,	"#",	0),			! "#" as first symbol on line
	(hashsym,			$,	"#",	0),			! "#" within macro def
	(lithashsym,		$,	"#",	0),			! "#" literal hash (not stringify op)
	(hashhashsym,		$,	"##",	0),			! "##" within macro def
	(commasym,			$,	",",	0),			! ","
	(semisym,			$,	";",	0),			! ";"
	(colonsym,			$,	":",	0),			! ":"
	(assignsym,			$,	"=",	jassign),	! =
	(assignsym2,		$,	":=",	jassign),	! =
	(lbracksym,			$,	"(",	0),			! (
	(rbracksym,			$,	")",	0),			! )
	(lsqsym,			$,	"[",	0),			!	 [
	(rsqsym,			$,	"]",	0),			! ]
	(lcurlysym,			$,	"{",	0),			! {
	(rcurlysym,			$,	"}",	0),			! }
	(questionsym,		$,	"?",	0),			! ?
	(curlsym,			$,	"~",	0),			! ~
	(ellipsissym,		$,	"...",	0),			! ...
	(backslashsym,		$,	"\\",	0),			! \
	(addsym,			$,	"+",	jadd),		! +
	(subsym,			$,	"-",	jsub),		!
	(mulsym,			$,	"*",	jmul),		!
	(divsym,			$,	"/",	jdiv),		!
	(remsym,			$,	"%",	jrem),		!
	(iorsym,			$,	"|",	jior),		!
	(iandsym,			$,	"&",	jiand),	!
	(ixorsym,			$,	"^",	jixor),	!
	(orlsym,			$,	"||",	jorl),		!
	(andlsym,			$,	"&&",	jandl),	!
	(shlsym,			$,	"<<",	jshl),		!
	(shrsym,			$,	">>",	jshr),		!
	(inotsym,			$,	"~",	jinot),	!
	(notlsym,			$,	"!",	jnotl),	!
	(incrsym,			$,	"++",	jpreincr),	!
	(decrsym,			$,	"--",	jpredecr),	!
	(abssym,			$,	"abs",	jabs),		!

	(eqsym,				$,	"==",	jeq),		!
	(nesym,				$,	"!=",	jne),		!
	(ltsym,				$,	"<",	jlt),		!
	(lesym,				$,	"<=",	jle),		!
	(gesym,				$,	">=",	jge),		!
	(gtsym,				$,	">",	jgt),		!

	(addtosym,			$,	"+=",	jaddto),	!
	(subtosym,			$,	"-=",	jsubto),	!
	(multosym,			$,	"*=",	jmulto),	!
	(divtosym,			$,	"/=",	jdivto),	!
	(remtosym,			$,	"%=",	jremto),	!
	(iortosym,			$,	"|=",	jiorto),	!
	(iandtosym,			$,	"&=",	jiandto),	!
	(ixortosym,			$,	"^=",	jixorto),	!
	(shltosym,			$,	"<<=",	jshlto),	!
	(shrtosym,			$,	">>=",	jshrto),	!

	(eolsym,			$,	"",		0),			!
	(eofsym,			$,	"",		0),			!
	(rawnumbersym,		$,	"n",	0),			!
	(intconstsym,		$,	"n",	0),			!
	(realconstsym,		$,	"n",	0),			!
	(charconstsym,		$,	"s",	0),			!
	(wcharconstsym,		$,	"s",	0),			!
	(stringconstsym,	$,	"s",	0),			!
	(wstringconstsym,	$,	"s",	0),			!
	(whitespacesym,		$,	"w",	0),			!
	(placeholdersym,	$,	"",	0),			!

!Second half are tokens that can be yielded after a name lookup:
	(namesym,			$,	"k",	0),			! identifier symbol
	(ksourcedirsym,		$,	"k",	0),			! 
	(predefmacrosym,	$,	"k",	0),			! __LINE__ etc
	(kdeclspecsym,		$,	"k",	0),			! __declspec

	(ktypespecsym,		$,	"k",	0),			! INT, SHORT
	(kifsym,			$,	"k",	0),			! IF
	(kelsesym,			$,	"k",	0),			! ELSE
	(kcasesym,			$,	"k",	0),			! CASE
	(kdefaultsym,		$,	"k",	0),			! DEFAULT
	(kforsym,			$,	"k",	0),			! FOR
	(kwhilesym,			$,	"k",	0),			! WHILE
	(kdosym,			$,	"k",	0),			! DO
	(kreturnsym,		$,	"k",	0),			! RETURN
	(kbreaksym,			$,	"k",	0),			! BREAK
	(kcontinuesym,		$,	"k",	0),			! CONTINUE
	(kgotosym,			$,	"k",	0),			! GO/GOTO
	(kswitchsym,		$,	"k",	0),			! SWITCH
	(kstructsym,		$,	"k",	0),			! STRUCT
	(kunionsym	,		$,	"k",	0),			! UNION
	(klinkagesym,		$,	"k",	0),			! STATIC etc
	(ktypequalsym,		$,	"k",	0),			! CONST etc
	(kstdtypesym,		$,	"k",	0),			! ui32_t etc
	(kfnspecsym,		$,	"k",	0),			! INLINE etc
	(kalignassym,		$,	"k",	0),			! _ALIGNAS
	(kenumsym,			$,	"k",	0),			! ENUM
!	(kcallconvsym,		$,	"k",	0),			! CLANG etc
	(ksizeofsym,		$,	"k",	0),			! SIZEOF
	(kdefinedsym,		$,	"k",	0),			! DEFINED
	(kgenericsym,		$,	"k",	0),			! _GENERIC
	(kalignofsym,		$,	"k",	0),			! _ALIGNOF
	(ksetjmpsym,		$,	"k",	0),			! SETJMP etc

	(kdummysym,			$,	"",		0)			!
end

global enumdata []ichar sourcedirnames =
	(definedir,		$),
	(emitdir,		$),
	(ifdir,			$),
	(elifdir,		$),
	(elsedir,		$),
	(endifdir,		$),
	(includedir,	$),
	(ifdefdir,		$),
	(ifndefdir,		$),
	(undefdir,		$),
	(errordir,		$),
	(messagedir,	$),
	(blankdir,		$),
	(linedir,		$),
	(pragmadir,		$)
end

global enumdata =
	ns_none=0,			!not set
	ns_general,		!variables, functions, typedefs, enum names
	ns_tags,			!struct, union, enum tags
	ns_labels,			!label names
	ns_fields			!field names
end

global enumdata [0:]ichar namenames, [0:]i32 namespaces, [0:]byte name2pid=
	(nullid=0,		$,		ns_none,		0),			!Not assigned, or keyword/macro defined by .symbol
	(macroid,		$,		ns_none,		0),			!
	(programid,		$,		ns_none,		0),			!Main root
	(moduleid,		$,		ns_none,		0),			!
	(extmoduleid,	$,		ns_none,		0),			!
	(typeid,		$,		ns_general,		0),			!Type name in type, proc or module
	(procid,		$,		ns_general,		proc_id),	!Proc/method/function/op name
	(staticid,		$,		ns_general,		static_id),	!Static in type or proc or module
	(frameid,		$,		ns_general,		local_id),	!Local var
	(paramid,		$,		ns_general,		param_id),	!Local param
	(fieldid,		$,		ns_fields,		0),			!Field of Record or Class
	(enumid,		$,		ns_general,		0),			!Enum name, part of enum type only
	(enumtagid,		$,		ns_tags,		0),			!
	(structtagid,	$,		ns_tags,		0),			!
	(labelid,		$,		ns_labels,		label_id)	!Label name in proc only
end

global tabledata []ichar stnames, []i32 stsymbols, []i32 stsubcodes=

	("if",			kifsym,			jif),
	("else",		kelsesym,		0),
	("case",		kcasesym,		0),
	("default",		kdefaultsym,	0),
	("for",			kforsym,		0),
	("do",			kdosym,			0),
	("while",		kwhilesym,		0),
	("return",		kreturnsym,		0),
	("break",		kbreaksym,		0),
	("continue",	kcontinuesym,	0),
	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		0),

	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),

	("include",		ksourcedirsym,	includedir),
	("define",		ksourcedirsym,	definedir),
	("elif",		ksourcedirsym,	elifdir),
	("ifdef",		ksourcedirsym,	ifdefdir),
	("ifndef",		ksourcedirsym,	ifndefdir),
	("endif",		ksourcedirsym,	endifdir),
	("undef",		ksourcedirsym,	undefdir),
	("error",		ksourcedirsym,	errordir),
	("pragma",		ksourcedirsym,	pragmadir),
	("line",		ksourcedirsym,	linedir),

	("auto",		klinkagesym,		auto_ss),
	("register",	klinkagesym,		register_ss),
	("static",		klinkagesym,		static_ss),
	("extern",		klinkagesym,		extern_ss),
	("typedef",		klinkagesym,		typedef_ss),
	
	("const",		ktypequalsym,	const_qual),
	("volatile",	ktypequalsym,	volatile_qual),
	("restrict",	ktypequalsym,	restrict_qual),
	("_Atomic",		ktypequalsym,	atomic_qual),

	("inline",		kfnspecsym,		inline_fnspec),
	("_Noreturn",	kfnspecsym,		noreturn_fnspec),

	("_Alignas",	kalignassym,	0),

	("enum",		kenumsym,		0),

	("void",		ktypespecsym,	ts_void),
	("char",		ktypespecsym,	ts_char),
	("short",		ktypespecsym,	ts_short),
	("long",		ktypespecsym,	ts_long),
	("int",			ktypespecsym,	ts_int),
	("float",		ktypespecsym,	ts_float),
	("double",		ktypespecsym,	ts_double),
	("signed",		ktypespecsym,	ts_signed),
	("unsigned",	ktypespecsym,	ts_unsigned),

	("_Bool",		ktypespecsym,	ts_bool),
!
	("__DATE__",	predefmacrosym,	pdm_date),
	("__FILE__",	predefmacrosym,	pdm_file),
	("__LINE__",	predefmacrosym,	pdm_line),
	("__TIME__",	predefmacrosym,	pdm_time),
	("__MCC__",		predefmacrosym,	pdm_mcc),
	("__func__",	predefmacrosym,	pdm_func),
	("__FUNCTION__",	predefmacrosym,	pdm_func),

	("__declspec",	kdeclspecsym,	0),

!	("not",			notlsym,		0),
	("sizeof",		ksizeofsym,		0),
	("lengthof",	ksizeofsym,		1),
	("defined",		kdefinedsym,	0),
	("_Generic",	kgenericsym,	0),
	("_Alignof",	kalignofsym,	0),

	("$setjmp",		ksetjmpsym,		jsetjmp),
	("$longjmp",	ksetjmpsym,		jlongjmp),

	("$$dummy",		0,				0)
end

global enumdata [0:]ichar convnames =

	(no_conv=0,	$),
	(soft_c,	$),			!no conversion needed, just type change
	(hard_c,	$),			!explicit conversion, done as uwiden or narrow to match sizes

	(swiden_c,	$),			!widen with sign-extension	(1/2/4 to 4/8)
	(uwiden_c,	$),			!widen with zero-extension	(1/2/4 to 4/8)
	(sfloat_c,	$),			!signed int to float		(1/2/4/8 to 4/8)
	(ufloat_c,	$),			!unsigned int to float		(1/2/4/8 to 4/8)
	(sfix_c,	$),			!float to signed int		(4/8 to 1/2/4/8)
	(ufix_c,	$),			!float to unsigned int		(4/8 to 1/2/4/8)
	(fwiden_c,	$),			!float to wider float		(4 to 8)
	(fnarrow_c,	$),			!float to narrower float	(8 to 4)
	(narrow_c,	$),			!narrow without truncation	(8/4/2 to 4/2/1)
	(truncate_c,$),			!narrow and truncate		(8/4/2 to 4/2/1)
	(bool_c,	$)			!int to bool				(1/2/4/8 to 1)
end

!take two basic numeric types and determine which is more dominant
!zeros mean not supported (error, not both numbers etc)
!(table could have been 16x16 but means checking both basic types being in-range first)

!dominantmode[s,t] returns the dominant type of s and t, widened to int/uint as needed
global [0:32,0:32]byte dominantmode

!conversionops[s,t] gives conversion op to convert numeric types s to t
global [0:16,0:16]byte conversionops

!table used to set up dominanttable[]
!3rd entry is the more dominant of the first two (wided as needed to int/unsigned int)
global [][3]byte dominantsetuptable=(
	(ti8,	ti8,		ti32),
	(ti8,	ti16,	ti32),
	(ti8,	ti32,		ti32),
	(ti8,	ti64,	ti64),
	(ti8,	tbool,		ti32),
	(ti8,	tu8,		ti32),
	(ti8,	tu16,	ti32),
	(ti8,	tu32,		ti32),
	(ti8,	tu64,	ti64),
	(ti8,	tr32,		tr32),
	(ti8,	tr64,	tr64),
	(ti16,	ti8,		ti32),
	(ti16,	ti16,	ti32),
	(ti16,	ti32,		ti32),
	(ti16,	ti64,	ti64),
	(ti16,	tbool,		ti32),
	(ti16,	tu8,		ti32),
	(ti16,	tu16,	ti32),
	(ti16,	tu32,		ti32),
	(ti16,	tu64,	ti64),
	(ti16,	tr32,		tr32),
	(ti16,	tr64,	tr64),
	(ti32,		ti8,		ti32),
	(ti32,		ti16,	ti32),
	(ti32,		ti32,		ti32),
	(ti32,		ti64,	ti64),
	(ti32,		tbool,		ti32),
	(ti32,		tu8,		ti32),
	(ti32,		tu16,	ti32),
	(ti32,		tu32,		tu32),
	(ti32,		tu64,	ti64),
	(ti32,		tr32,		tr32),
	(ti32,		tr64,	tr64),
	(ti64,	ti8,		ti64),
	(ti64,	ti16,	ti64),
	(ti64,	ti32,		ti64),
	(ti64,	ti64,	ti64),
	(ti64,	tbool,		ti64),
	(ti64,	tu8,		ti64),
	(ti64,	tu16,	ti64),
	(ti64,	tu32,		ti64),
	(ti64,	tu64,	tu64),
	(ti64,	tr32,		tr32),
	(ti64,	tr64,	tr64),
	(tbool,		ti8,		ti32),
	(tbool,		ti16,	ti32),
	(tbool,		ti32,		ti32),
	(tbool,		ti64,	ti64),
	(tbool,		tbool,		tu32),
	(tbool,		tu8,		tu32),
	(tbool,		tu16,	tu32),
	(tbool,		tu32,		tu32),
	(tbool,		tu64,	tu64),
	(tbool,		tr32,		tr32),
	(tbool,		tr64,	tr64),
	(tu8,	ti8,		ti32),
	(tu8,	ti16,	ti32),
	(tu8,	ti32,		ti32),
	(tu8,	ti64,	ti64),
	(tu8,	tbool,		tvoid),
	(tu8,	tu8,		tu32),
	(tu8,	tu16,	tu32),
	(tu8,	tu32,		tu32),
	(tu8,	tu64,	tu64),
	(tu8,	tr32,		tr32),
	(tu8,	tr64,	tr64),
	(tu16,	ti8,		ti32),
	(tu16,	ti16,	ti32),
	(tu16,	ti32,		ti32),
	(tu16,	ti64,	ti64),
	(tu16,	tbool,		tu32),
	(tu16,	tu8,		tu32),
	(tu16,	tu16,	tu32),
	(tu16,	tu32,		tu32),
	(tu16,	tu64,	tu64),
	(tu16,	tr32,		tr32),
	(tu16,	tr64,	tr64),
	(tu32,		ti8,		ti32),
	(tu32,		ti16,	ti32),
	(tu32,		ti32,		tu32),
	(tu32,		ti64,	ti64),
	(tu32,		tbool,		tu32),
	(tu32,		tu8,		tu32),
	(tu32,		tu16,	tu32),
	(tu32,		tu32,		tu32),
	(tu32,		tu64,	tu64),
	(tu32,		tr32,		tr32),
	(tu32,		tr64,	tr64),
	(tu64,	ti8,		tu64),
	(tu64,	ti16,	tu64),
	(tu64,	ti32,		tu64),
	(tu64,	ti64,	tu64),
	(tu64,	tbool,		tu64),
	(tu64,	tu8,		tu64),
	(tu64,	tu16,	tu64),
	(tu64,	tu32,		tu64),
	(tu64,	tu64,	tu64),
	(tu64,	tr32,		tr32),
	(tu64,	tr64,	tr64),
	(tr32,	ti8,		tr64),
	(tr32,	ti16,	tr64),
	(tr32,	ti32,		tr64),
	(tr32,	ti64,	tr64),
	(tr32,	tbool,		tr64),
	(tr32,	tu8,		tr64),
	(tr32,	tu16,	tr64),
	(tr32,	tu32,		tr64),
	(tr32,	tu64,	tr64),
	(tr32,	tr32,		tr32),
	(tr32,	tr64,	tr64),
	(tr64,	ti8,		tr64),
	(tr64,	ti16,	tr64),
	(tr64,	ti32,		tr64),
	(tr64,	ti64,	tr64),
	(tr64,	tbool,		tr64),
	(tr64,	tu8,		tr64),
	(tr64,	tu16,	tr64),
	(tr64,	tu32,		tr64),
	(tr64,	tu64,	tr64),
	(tr64,	tr32,		tr64),
	(tr64,	tr64,	tr64),
)

!table used to set up conversionops
global [][3]byte convsetuptable=(
	(ti8,	ti8,	swiden_c),
	(ti8,	ti16,	swiden_c),
	(ti8,	ti32,	swiden_c),
	(ti8,	ti64,	swiden_c),
	(ti8,	tbool,	bool_c),
	(ti8,	tu8,	soft_c),
	(ti8,	tu16,	swiden_c),
	(ti8,	tu32,	swiden_c),
	(ti8,	tu64,	swiden_c),
	(ti8,	tr32,	sfloat_c),
	(ti8,	tr64,	sfloat_c),

	(ti16,	ti8,	truncate_c),
	(ti16,	ti16,	no_conv),
	(ti16,	ti32,	swiden_c),
	(ti16,	ti64,	swiden_c),
	(ti16,	tbool,	bool_c),
	(ti16,	tu8,	truncate_c),
	(ti16,	tu16,	soft_c),
	(ti16,	tu32,	swiden_c),
	(ti16,	tu64,	swiden_c),
	(ti16,	tr32,	sfloat_c),
	(ti16,	tr64,	sfloat_c),
	(ti32,	ti8,	truncate_c),

	(ti32,	ti16,	truncate_c),

	(ti32,	ti32,	no_conv),
	(ti32,	ti64,	swiden_c),
	(ti32,	tbool,	bool_c),
	(ti32,	tu8,	truncate_c),
	(ti32,	tu16,	truncate_c),
	(ti32,	tu32,	soft_c),
	(ti32,	tu64,	swiden_c),
	(ti32,	tr32,	sfloat_c),
	(ti32,	tr64,	sfloat_c),

	(ti64,	ti8,	truncate_c),
!	(ti64,	ti8,	narrow_c),

	(ti64,	ti16,	truncate_c),
	(ti64,	ti32,	truncate_c),
	(ti64,	ti64,	no_conv),
	(ti64,	tbool,	bool_c),

	(ti64,	tu8,	truncate_c),
!	(ti64,	tu8,	narrow_c),

	(ti64,	tu16,	truncate_c),
	(ti64,	tu32,	truncate_c),
	(ti64,	tu64,	soft_c),
	(ti64,	tr32,	sfloat_c),
	(ti64,	tr64,	sfloat_c),
	(tbool,	ti8,	soft_c),
	(tbool,	ti16,	uwiden_c),
	(tbool,	ti32,	uwiden_c),
	(tbool,	ti64,	uwiden_c),
	(tbool,	tbool,	no_conv),
	(tbool,	tu8,	soft_c),
	(tbool,	tu16,	uwiden_c),
	(tbool,	tu32,	uwiden_c),
	(tbool,	tu64,	uwiden_c),
	(tbool,	tr32,	ufloat_c),
	(tbool,	tr64,	ufloat_c),
	(tu8,	ti8,	soft_c),
	(tu8,	ti16,	uwiden_c),
	(tu8,	ti32,	uwiden_c),
	(tu8,	ti64,	uwiden_c),
	(tu8,	tbool,	bool_c),
	(tu8,	tu8,	soft_c),
	(tu8,	tu16,	uwiden_c),
	(tu8,	tu32,	uwiden_c),
	(tu8,	tu64,	uwiden_c),
	(tu8,	tr32,	ufloat_c),
	(tu8,	tr64,	ufloat_c),

	(tu16,	ti8,	truncate_c),
	(tu16,	ti16,	soft_c),
	(tu16,	ti32,	uwiden_c),
	(tu16,	ti64,	uwiden_c),
	(tu16,	tbool,	bool_c),
	(tu16,	tu8,	truncate_c),
	(tu16,	tu16,	no_conv),
	(tu16,	tu32,	uwiden_c),
	(tu16,	tu64,	uwiden_c),
	(tu16,	tr32,	ufloat_c),
	(tu16,	tr64,	ufloat_c),

	(tu32,	ti8,	truncate_c),
	(tu32,	ti16,	truncate_c),
	(tu32,	ti32,		soft_c),
	(tu32,	ti64,	uwiden_c),
	(tu32,	tbool,	bool_c),
	(tu32,	tu8,	truncate_c),
	(tu32,	tu16,	truncate_c),
	(tu32,	tu32,	no_conv),
	(tu32,	tu64,	uwiden_c),
	(tu32,	tr32,	ufloat_c),
	(tu32,	tr64,	ufloat_c),

	(tu64,	ti8,	truncate_c),
	(tu64,	ti16,	truncate_c),
	(tu64,	ti32,	truncate_c),
	(tu64,	ti64,	soft_c),
	(tu64,	tbool,	bool_c),
	(tu64,	tu8,	truncate_c),
	(tu64,	tu16,	truncate_c),
	(tu64,	tu32,	truncate_c),
	(tu64,	tu64,	no_conv),
	(tu64,	tr32,	ufloat_c),
	(tu64,	tr64,	ufloat_c),

	(tr32,	ti8,	sfix_c),
	(tr32,	ti16,	sfix_c),
	(tr32,	ti32,	sfix_c),
	(tr32,	ti64,	sfix_c),
	(tr32,	tbool,	ufix_c),
	(tr32,	tu8,	ufix_c),
	(tr32,	tu16,	ufix_c),
	(tr32,	tu32,	ufix_c),
	(tr32,	tu64,	ufix_c),
	(tr32,	tr32,	no_conv),
	(tr32,	tr64,	fwiden_c),

	(tr64,	ti8,	sfix_c),
	(tr64,	ti16,	sfix_c),
	(tr64,	ti32,	sfix_c),
	(tr64,	ti64,	sfix_c),
	(tr64,	tbool,	ufix_c),
	(tr64,	tu8,	ufix_c),
	(tr64,	tu16,	ufix_c),
	(tr64,	tu32,	ufix_c),
	(tr64,	tu64,	ufix_c),
	(tr64,	tr32,	fnarrow_c),
	(tr64,	tr64,	no_conv),

)

=== pc_api.m 0 0 14/24 ===
EXPORT INT PCLSEQNO
int STSEQNO

export pcl pcstart			!start of pcl block
export pcl pccurr			!point to current pcl op
export pcl pcend			!point to last allocated pclrec
global int pcalloc			!number of pclrecs allocated
byte pcfixed				!whether code is fixed up
int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

int initpcalloc=65536

const pcelemsize = pclrec.bytes

!global ichar longstring					!used in stropnd
!global int longstringlen

export int mlabelno
export byte phighmem
global byte fpshortnames

export func pcl_start(ichar name=nil)psymbol=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

	if name then
		currprog:=pc_makesymbol(name, program_id)
	fi

	pcalloc:=initpcalloc

	pcstart:=pcm_allocz(pcalloc*pcelemsize)
	pcend:=pcstart+pcalloc-8

	pccurr:=pcstart-1
	pcfixed:=0
	pcseqno:=0
	pcneedfntable:=0

	mlabelno:=0

	currprog

end

export proc pcl_end=
	if pccurr>=pccurr and pccurr.opcode<>kendprog then
		pc_gen(kendprog)
	fi	
!	pcldone:=1
end

export func pcl_writepcl(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writeallpcl()

	if filename then
		if pverbose then println "Writing PCL", filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

export func pcl_writepst(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writepst()

	if filename then
		if pverbose then println "Writing PST", filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

export proc pcl_setflags(int highmem=-1, verbose=-1, shortnames=-1) =

	if highmem>=0 then phighmem:=highmem fi

	if verbose>=0 then pverbose:=verbose fi
	if shortnames>=0 then fpshortnames:=shortnames fi
end

proc extendpclblock=
	int newpcalloc, lengthused
	pcl newpcstart

	newpcalloc:=pcalloc*2
	lengthused:=pccurr-pcstart+1

	newpcstart:=pcm_alloc(pcelemsize*newpcalloc)

	memcpy(newpcstart, pcstart, lengthused*pcelemsize)
	pcm_clearmem(newpcstart+lengthused, (newpcalloc-lengthused)*pcelemsize)

	pccurr:=newpcstart+(pccurr-pcstart)
	pcend:=newpcstart+newpcalloc-8

	pcm_free(pcstart, pcalloc*pcelemsize)

	pcstart:=newpcstart
	pcalloc:=newpcalloc
end

global func newpcl:pcl =
	if pccurr>=pcend then
		extendpclblock()
	fi

	++pccurr

	pccurr.pos:=mmpos
	PCCURR.SEQNO:=++PCLSEQNO

	return pccurr
end

export proc pc_gen(int opcode, pcl p=nil) =
	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
end

export proc pc_genix(int opcode, scale=1, offset=0) =
!originally intended for combinations of ptr ops to be combined into
!previous ones, but that has now been dropped.
!Instead any such reductions will be done in a separate pass, much simpler
	pcl p

	p:=newpcl()

	p.opcode:=opcode
	p.scale:=scale
	p.extra:=offset
end

export proc pc_genxy(int opcode, int x, y, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
	p.y:=y
end

!export proc pc_gencond(int opcode, cond, pcl p=nil) =
!
!	if p=nil then
!		p:=newpcl()
!	fi
!
!	p.opcode:=opcode
!	p.condcode:=cond
!end

export func genint(int a)pcl p=
	p:=newpcl()
	p.value:=a
	p.opndtype:=int_opnd
	return p
end

export func genreal(real x, int mode=tpr64)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=(mode=tpr64|real_opnd|r32_opnd)
	return p
end

!export func genrealimm(real x, int mode=tpr64)pcl p=
!	p:=newpcl()
!	p.xvalue:=x
!	p.opndtype:=(mode=tpr64|realimm_opnd|realimm32_opnd)
!	return p
!end

export func genstring(ichar s, int length=-1)pcl p=
!export func genstring(ichar s, int length)pcl p=
	p:=newpcl()

	if length<0 then
		length:=strlen(s)+1
	fi

	p.svalue:=pcm_copyheapstringn(s,length)		!will add an extra zero
	p.opndtype:=string_opnd
	p.slength:=length
	return p
end

export func genpcstrimm(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=strimm_opnd

	return p
end

export func genlabel(int a)pcl p=
	p:=newpcl()
	p.labelno:=a

	p.opndtype:=label_opnd
	return p
end

export func genmem(psymbol d)pcl p=

	p:=newpcl()

	p.def:=d

	p.opndtype:=mem_opnd
	return p
end

export func genmemaddr(psymbol d)pcl p=
	p:=newpcl()
	p.def:=d

	p.opndtype:=memaddr_opnd
	return p
end

export proc gencomment(ichar s)=
	return when fregoptim or fpeephole		!will get skipped anyway
	pc_gen(kcomment, genpcstrimm(s))
end

export func genname(ichar s)pcl=
	return genmem(pc_makesymbol(s, misc_id))
end

!export func gennameaddr(ichar s)pcl=
!	return genmemaddr(pc_makesymbol(s, misc_id))
!end

EXPORT func strpmode(int mode, size=0)ichar=
	static [32]char str

	strcpy(str, "")

	case mode
	when tpblock then
		strcpy(str, "mem:")
		strcat(str, strint(size))
		str
	when tpvoid then
		"---"
	else
		pstdnames[mode]
	esac
end

export proc pc_setmode(int m, size=0)=
	pccurr.mode:=m

	if size then
		pccurr.size:=size
	else
		pccurr.size:=psize[pccurr.mode]
	fi

	if pclhastype[pccurr.opcode]=2 then
		pccurr.mode2:=pccurr.mode
	fi
end

export proc pc_setmode2(int m)=
	pccurr.mode2:=m
end

export proc pc_setxy(int x, y)=
	pccurr.x:=x
	pccurr.y:=y
end

export proc pc_setscaleoff(int scale, offset:=0)=
	pccurr.scale:=scale
	pccurr.extra:=offset
end

!export proc pc_setoffset(int offset)=
!	pccurr.extra:=offset
!end

!export proc pc_addoffset(int offset)=
!	pccurr.extra+:=offset
!end

export proc pc_setincr(int n)=
	pccurr.stepx:=n
end

!export proc pc_setnargs(int n)=
!	pccurr.nargs:=n
!end

!export proc pc_setnvariadics(int n)=
!	pccurr.nvariadics:=n
!end

export proc pc_setalign(int n)=
	pccurr.align:=n
end

!global proc perror(ichar mess)=
!	perror_s(mess, nil)
!end
!
!global proc perror_s(ichar mess, param=nil)=
!	print "PCL error:", mess
!	if param then
!		print ":", param
!	fi
!
!	stop 1
!end
!
export func getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

!global proc xpclerror(ichar mess)=
!	println "PCL Error:", mess
!	println
!	stop 1
!end

export proc pc_addsymbol(psymbol d)=
	if psymboltable=nil then
		psymboltable:=psymboltablex:=d
	else
		psymboltablex.next:=d
		psymboltablex:=d
	fi
end

export func pc_makesymbol(ichar s, int id)psymbol d=
!Create a new st entry
!local/param/null-id names are not linked to psymbol table
!all others become part of main ST
!Only local/param have .owner set to currfunc

	d:=pcm_allocnfz(pstrec.bytes)
	d.name:=pcm_copyheapstring(s)
	d.seqno:=++stseqno

	case id
	when import_id then
		d.imported:=1
	when export_id then
		d.exported:=1
		id:=proc_id
	esac

	d.id:=id

	if id in [local_id, param_id] then
		d.owner:=currfunc
	elsif id then
		pc_addsymbol(d)
	fi

	d
end

global func getfullname(psymbol d, int backtick=0)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	int n:=0
	psymbol e:=d

	str[1]:=0
	if backtick then
		strcpy(str, "`")
	fi

	if d.imported then
		if backtick then
			strcat(str, d.name)
			strcat(str, "*")
		else
			strcat(str, d.name)
		fi
		return str
	fi

	if d.id in [local_id, param_id] then
		strcat(str, d.owner.name)
		strcat(str, ".")
		strcat(str, d.name)
		return str
	fi

	if backtick then
		strcat(str, d.name)
	else
		return d.name
	fi
end

global proc pcerrorstop(ichar filename, int lineno)=
	filehandle f
	f:=fopen("$error.tmp", "w")
	println @f, filename, lineno
	println
	fclose(f)
	stop 1
end

export proc pc_defproc(psymbol d, int mode=tpvoid, isentry=0)=
!	pclerror("Nested proc") when currfunc
	pc_gen(kproc, genmem(d))
	if mode=tpvoid then mode:=d.mode fi
	pc_setmode(mode)

!THIS .PCADDR is a dummy value; any non-nil value shows d is already defined
!A proper .pcaddr is filled in during runp fixup
!	if d.pcaddr then pclerror(addstr("Dupl proc:", d.name)) fi
	d.pcaddr:=pccurr

	if entryproc=nil and isentry then
		entryproc:=d
		d.isentry:=1
	fi

	currfunc:=d
end

!export proc pc_setimport(psymbol d)=
!!allow the use of pc_addlocal
!!use d=nil when done
!
!	currfunc:=d
!end

export proc pc_addparam(psymbol d)=
	psymbol p:=currfunc, q

	q:=p.nextparam

	if q=nil then
		p.nextparam:=d
	else
		while q.nextparam do q:=q.nextparam od		!look for last
		q.nextparam:=d
	fi
	if d.owner=nil then d.owner:=currfunc fi
	++currfunc.nparams

end

export proc pc_addlocal(psymbol d)=
	psymbol p:=currfunc, q

	q:=p.nextlocal

	if q=nil then
		p.nextlocal:=d
	else
		while q.nextlocal do q:=q.nextlocal od		!look for last
		q.nextlocal:=d
	fi
	if d.owner=nil then d.owner:=currfunc fi
	++currfunc.nlocals
end

export proc pc_endproc=
	pc_gen(kendproc)
	currfunc:=nil
end

EXPORT proc merror(ichar mess, ichar param="")=
	int lineno
	ichar filename, sourceline

	lineno:=cgetsourceinfo(mmpos, filename, sourceline)
	CPL =LINENO
	CPL =FILENAME

	if currfunc then
		println "Proc:", currfunc.name
	fi

	fprintln "MCL Error: # (#) on Line: # in #, PCL:#", mess, param, lineno, filename, ppseqno

	pcerrorstop(filename, lineno)
end

!export func pc_duplpst(psymbol d)psymbol e=
!	e:=pcm_allocnfz(pstrec.bytes)
!	e^:=d^
!	e.seqno:=++stseqno
!
!	e.next:=nil
!	e
!end
!
!
=== pc_decls.m 0 0 15/24 ===
!decls

export type psymbol = ref pstrec

export record pstrec = $caligned
!global record pstrec = $caligned
!global record pstrec =
	ichar name
	psymbol next
	psymbol nextparam
	union
		psymbol nextlocal
		pcl pcdata				!istatics: point to first kdata op
	end
	psymbol owner
	psymbol generic				!locals/params: version in global ST

	pcl pcaddr				!for procs: entry point to function

	byte id

	i32 offset

	byte imported				!only for import_id
	byte exported				!only for proc_id/static_id
	byte mode
	byte isentry
	u32 size

	byte addrof
	byte used					!0, or 1 to 255 (capped at 255)

	i32 labelno

	byte dllexport

	byte dllindex				!for dllproc: which dll in dlltable

	byte nretvalues				!function: number of return values (0 for proc)
	byte varparams				!0 or N; variadic params

	byte ismain					!1 if a proc to be part of func tables

!----------------------------------

	byte nparams
	byte variadic				!local function that uses ... variadic params (for C)
	i16 nlocals
	u32 seqno

end

export type pcl = ref pclrec

global record pclrec =
	byte opcode
	byte opndtype
	byte condcode						!for jumpcc/setcc
	byte mode

	u32 size

	union
		struct
			union
				i64	value
				r64	xvalue
				ichar	svalue			!also used for data
				int		labelno
				psymbol	def
				ivoid	asmcode
			end

			union						!two 32-bit params used according to opcode
				struct
					i32 x				!common access to these 1/2 extra attribs
					i32 y
				end

				struct					! (x,y) pointer ops
					i32 scale			! scale factor for offset
					i32 extra			! extra constant byte offset, already scaled
				end
				struct					! (x,y) call/etc
					i32 nargs			! number of args
					union
						i32 nvariadics	!call: 0, or arg # that is first variadic
						i32 nrealargs	!setcall: 1 if whole call sequence is simple
					end
				end
				struct					! (x,y) switch
					i32 minlab
					i32 maxlab
				end
				struct					! defproc/retproc/retfn
					i32 paramslots	! stack usage as 8-byte slots
					i32 localslots
				end

				i32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
				i32 align
				i32 popone			! (x) jumpcc: leave X on stack
				i32 slicelwb			! (x) for .upb
				i32 inplace			! (x) for load, means &A operand is for inplace update
				u32 slength			! string length: includes any terminator

			end
		end
	end

	u32 pos:(sourceoffset:24, fileno:8)
	i32 dummy:(mode2:8, seqno:24)
end

global const maxparams=32
global const maxlocals=256


global int bspill, bxspill		!no. to spill

global byte r10used				!these may be set in pass2 when occupied by params
global byte r11used

global byte localshadow			!1 if local, proc-wide shadow space used for a call

export int mmpos

global psymbol psymboltable, psymboltablex

global psymbol currprog
export psymbol currfunc
global psymbol blockretname
global psymbol entryproc		!entry point function

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer

!global const maxplibfile=50
!global [maxplibfile]ichar plibfiles
!global [maxplibfile]u64 plibinst
!global int nplibfiles

!global byte pcldone, mcldone, ssdone, objdone, exedone

export byte pverbose

export int assemtype='AA'

GLOBAL INT PPSEQNO

export byte fpeephole = 0
export byte fregoptim = 0
export byte fnoconst  = 0
export byte fshowil   = 0
export byte flong64   = 0
=== pc_diags.m 0 0 16/24 ===
!const fshowppseqno=1
const fshowppseqno=0

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

const tab1="    "
const tab2="        "

!const fshowsymbols=1
const fshowsymbols=0

global proc strpcl(pcl p)=
	[256]char str
	int opcode, n,x,y
	psymbol d, e

	const showformatted=1

	opcode:=p.opcode

!	if fshowppseqno then
!		psstr(strint(p.seqno,"z5"))
!		psstr("  ")
!	fi

	case opcode
	when klabel then
		strlabel(p.labelno,1)

		IF P.POPONE THEN
			PSSTR(" NOT USED")
		FI

		return
	when klabeldef then
		psstr("! ")
		psstr(p.def.name)
		psstr(":")
		return
	when kcomment then
		if p.svalue^ then
			psstr("!")
			psstr(p.svalue)
		ELSE
			PSSTR("! - - -")
		fi
		return
	when kproc then
		d:=p.def

		psstr("proc")

		psstr(" ")
		psname(d)

		psstr((p.def.exported|"::"|":"))
		if d.isentry then psstr(":") fi

		psline()

		e:=d.nextparam

		while e, e:=e.nextparam do
			if fshowppseqno then psstr("       ") fi
			psstr(tab1+"param    ")
			psstr(strpmode(e.mode, e.size))
			psstr("       ")
			psstr(e.name)
			psline()
		od

		e:=d.nextlocal
		while e, e:=e.nextlocal do
			if fshowppseqno then psstr("       ") fi
			psstr(tab1+"local    ")
			psstr(strpmode(e.mode, e.size))
			psstr("       ")
			psstr(e.name)
			psline()
		od

		if p.mode then
			if fshowppseqno then psstr("       ") fi
			psstr(tab1+"rettype  ")
			psstr(strpmode(P.mode))
			psline()
		fi

		if d.variadic then
			psstrline(tab1+"variadic")
		fi

		return

	when kendproc then
		psstr("endproc")
		psline()
		return

	when kendprog then
		psstr("endprog")
		return

	when kdata then

		if p.mode=tpblock then
			psdata(p)
			return
		fi

	when kistatic, kzstatic then
		skiptab

	esac

	psstr(tab1)
skiptab:


	case opcode
	when kjumpcc then
		strcpy(str, "jump")
		strcat(str, ccnames[p.condcode])
	when ksetcc then
		strcpy(str, "set")
		strcat(str, ccnames[p.condcode])
	else
		strcpy(str, pclnames[opcode])
	esac

	gs_leftstr(dest,str,9)

	str[1]:=0
	if p.mode then
		strcat(str, strpmode(p.mode, p.size))

		if pclhastype[opcode]=2 then
			strcat(str, "/")
			strcat(str, strpmode(p.mode2))
		fi
		STRCAT(STR, " ")
	fi
	gs_leftstr(dest,str,4)

	str[1]:=0
	n:=pclextra[opcode]
	if n then
		x:=p.x; y:=p.y
		if x or n=2 then			!don't show single 0 value
			strcat(str, "/")
			strcat(str, strint(p.x))
		fi

		if n=2 and y then
			strcat(str, "/")
			strcat(str, strint(y))
		fi
		STRCAT(STR, " ")
	fi	
	gs_leftstr(dest,str,5)

	if p.opndtype<>no_opnd then
		psstr(" ")
		psopnd(p)
	fi
	pstabto(40)

	if fshowppseqno then
		psstr("! ")
		psstr(strint(p.seqno,"z5"))
		psstr("  ")
	fi
end

proc psopnd(pcl p)=
	[512]char str
	int length
	psymbol d
	static ichar longstring
	ichar ss

	if p=nil then
		return
	fi

	case p.opndtype
	when int_opnd then
		psint(p.value)
	when real_opnd, realimm_opnd, realimm32_opnd, r32_opnd then
		if p.xvalue=infinity then
			fprint @str,"infinity"
		else
			print @str,p.xvalue:"e16.16"
		fi
		psstr(str)

	when string_opnd then
		length:=p.slength
		if length<str.len/2 then
			psstr("""")
			convertstring(p.svalue, str, length)

			psstr(str)
			psstr("""*")
			psint(p.slength)

		else
			PSSTR("LONGSTR")
		fi

	when mem_opnd then
		d:=p.def
		psstr(p.def.name)

		if p.opcode in [kistatic, kzstatic] then
			psstr(":")
			if d.exported then
				psstr(":")
			fi
		fi

	when memaddr_opnd then
		psstr("&")
		recase mem_opnd

	when label_opnd then
		fprint @str,"## ","#",p.labelno
		psstr(str)

	when no_opnd then
		return

	else
		psstr("<PCLOPND?>")
	esac
end

global func strpclstr(pcl p, int buffsize)ichar=
	gs_free(dest)
	gs_init(dest)
	destlinestart:=0
	strpcl(p)
	gs_char(dest,0)

	if dest.length>=buffsize then return "<BIGSTR>" fi

	dest.strptr
end

global proc writepcl(pcl p)=

	strpcl(p)
	case p.opcode
	when kproc then
	else
		gs_line(dest)
	esac


end

global func writeallpcl:ref strbuffer=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	psymbol d

	gs_init(dest)
	destlinestart:=dest.length

	gs_strln(dest, "!PROC PCL")

	d:=psymboltable
	while d, d:=d.next do
		if d.id=import_id then
			psstr("extproc    ")
			psstr(d.name)
			if d.variadic then
				psstr(" 1")
			fi
			psline()
		fi
	od

	p:=pcstart

	while p<=pccurr do
		writepcl(p)

		destlinestart:=dest.length
		++p
	od

	psline()

	return dest
end

global proc psstr(ichar s)=
	gs_str(dest,s)
end

global proc psstrx(ichar s)=
	gs_str(dest,s)
end

global proc psstrline(ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

global proc psline=
	GS_STR(DEST, "\n")
end

global proc psint(int a)=
	gs_str(dest,strint(a))
end

global proc psname(psymbol d)=
	gs_str(dest, d.name)
end

global proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col od
end

global proc strlabel(int labelno,colon=0)=
	psstr("#")
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

proc psdata(pcl p)=
	const perline = 20
	int n:=p.size, m
	ref byte q:=p.svalue

	if n=0 then return fi

	while n>0 do
		m:=n
		if m>=perline then
			m:=perline
		fi
		n-:=m
		psstr(tab1+"data mem:")
		psint(m)
		psstr("  ")
		if m<10 then psstr(" ") fi
		to m do
			psint(q^)
			psstr(" ")
			++q
		od
		if n then
			psline()
		fi
	od
end

global func writepst:ref strbuffer=
	byte localfile:=0
	int i:=0, j
	psymbol d, e

	gs_init(dest)

	psstrline("PROC PC Symbol table")
	psline()

	d:=psymboltable

	while d, d:=d.next do
PSSTR(STRINT(INT(D),"H"))
PSSTR(" ")
			writepsymbol(d, "25jl")

!			if d.id=proc_id then
			if d.id in [proc_id, import_id] then
				e:=d.nextparam
				j:=0
				while e, e:=e.nextparam do
					psstr("    ")
					writepsymbol(e, "21jl")
				od
				e:=d.nextlocal
				j:=0
				while e, e:=e.nextlocal do
					psstr("    ")
					writepsymbol(e, "21jl")
				od
			fi
PSLINE()
	od
	psline()

	return dest
end

proc writepsymbol(psymbol d, ichar fmt)=
	byte localfile:=0
	[256]char str

	print @str, d.seqno:"4", idnames[d.id]
	psstr(str)
	to 8-strlen(idnames[d.id]) do psstr(" ") od

	str[1]:=0

	print @str, d.name:fmt
	psstr(str)

	psstr(strpmode(d.mode, d.size))

	if d.id=proc_id then
		psstr(" Pm:")
		psint(d.nparams)
		psstr(" Loc:")
		psint(d.nlocals)
	fi

	if d.exported then psstr(" Exp") fi
	if d.imported then psstr(" Imp") fi
	if d.varparams then psstr(" Var:"); psint(d.varparams) fi
	if d.isentry then psstr(" ENTRY PT") fi

	if d.id=proc_id then psstr(" .PCADDR ="); PSSTR(STRINT(CAST(D.PCADDR),"H")) fi

	if d.owner then
		psstr(" (")
		psint(d.owner.seqno)
		psstr(" ")
		psstr(d.owner.name)
		psstr(")")
	fi	

	psline()
end

global func convertstring(ichar s, t, int length)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!length is that of s; final length may be up to 4 times as long

!returns actual length of t
	int c
	ichar t0:=t
	[16]char str

	to length do
		case c:=s++^
		when '"' then
			t++^:='\\'
			t++^:='"'
		when 10 then
			t++^:='\\'
			t++^:='n'
		when 13 then
			t++^:='\\'
			t++^:='r'
		when 9 then
			t++^:='\\'
			t++^:='t'
		when '\\' then
			t++^:='\\'
			t++^:='\\'
		elsif c in 32..126 then
			t++^:=c
		else
			t++^:='\\'			!octal
			print @str,c:"z3x8"
			t++^:=str[1]
			t++^:=str[2]
			t++^:=str[3]
		esac
	od

	t^:=0

	return t-t0
end

global func stropndstack(int indent=0)ichar=
	static [512]char str
	[512]char str2
	ichar s:=str, t

	if indent then
		fprint @s, "="*40 + "#:(", NOPERANDS
!		fprint @s, "="*20 + "#:(", NOPERANDS
	else
		fprint @s, "#:(", NOPERANDS
	fi

	for i to noperands do

		strcat(s, (noperands-i+1|"Z:", "Y:", "X:", "W:"|""))

		case pclloc[i]
		when reg_loc then				!loaded
			if ispfloat(pclmode[i]) then
				strcat(s, xregnames[pclreg[i]])
			else
				strcat(s, regnames[pclreg[i]])
			fi
		when temp_loc then				!in temp
			strcat(s, "T")
			strcat(s, strint(i))

		else
			strcat(s, "(==")
			strcat(s, ")")
		esac
		if pclcount[i]>1 then strcat(s, "@") fi
		strcat(s, "<")
		strcat(s, pstdnames[pclmode[i]])
		strcat(s, ">")

		if i<noperands then strcat(s,", ") fi
	od
	strcat(s,") ")

	ipadstr(str, 50)

	strcat(s,"WR:(")
	for r:=r0 to r9  do
		strcat(s,(regset[r]|"1 "|"0 "))
	od
	strcat(s,") ")

	strcat(s,"XWR:(")
	for r:=r0 to xregmax do
		strcat(s,(xregset[r]|"1 "|"0 "))
	od

	strcat(s,") hwstack:")
	strcat(s,strint(mstackdepth))
	strcat(s," noperands:")
	strcat(s,strint(noperands))
	strcat(s," ncalldepth:")
	strcat(s,strint(ncalldepth))
	return s
end

global proc showopndstack=
	mgencomment(stropndstack(1))
end
=== pc_diags_dummy.m 0 0 17/24 ===
global proc strpcl(pcl p)=
end

global func stropnd(pcl p)ichar=
	return nil
end

global func strpclstr(pcl p, int buffsize)ichar=
	nil
end

global proc writepcl(pcl p)=
end

global func writeallpcl:ref strbuffer=
ABORTPROGRAM("dummy diags")
	nil
end

global func writepst:ref strbuffer=
	writeallpcl()
!	nil
end

global func stropndstack(int indent=0)ichar=
	return nil
end

global proc showopndstack=
end

global fun convertstring(ichar s, t, int length)int = 0
=== pc_tables.m 0 0 18/24 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,

		[0:]byte psigned,

		[0:]byte pmin =						!promoted type when min width applies

	(tpvoid=0,    "void",    	0,	0, tpvoid),

	(tpr32,       "r32",    	4,	0, tpr32),
	(tpr64,       "r64",    	8,	0, tpr64),

	(tpu8,        "u8",      	1,	0, tpu32),
	(tpu16,       "u16",    	2,	0, tpu32),
	(tpu32,       "u32",    	4,	0, tpu32),
	(tpu64,       "u64",    	8,	0, tpu64),

	(tpi8,        "i8",      	1,	1, tpi32),
	(tpi16,       "i16",    	2,	1, tpi32),
	(tpi32,       "i32",    	4,	1, tpi32),
	(tpi64,       "i64",    	8,	1, tpi64),

	(tpblock,     "mem",   		0,	0, tpblock),
	(tpvector,    "vec",   		0,	0, tpvector),

	(tplast,      "$last",   	0,	0, 0),


end

global const tpref = tpu64

!.opndtype in pclrec

export enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),
	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(label_opnd,		$),
	(int_opnd,			$),
	(real_opnd,			$),
	(r32_opnd,			$),
	(string_opnd,		$),
	(strimm_opnd,		$),
	(realimm_opnd,		$),
	(realimm32_opnd,	$),
	(any_opnd,			$),		!(used in PCL parser)
end

!The top 4 stack operands are designated as:
!
!	- - - - - Z          1 operand used in IL instruction
!	- - - - Y Z          2 operands
!	- - - X Y Z          3 operands
!	- - W X Y Z          4 operands
!
!The stack notionally grows from left to right. Z is always top-of-stack
!
!Results may be shown as being stored in one of those same operands, eg.
!
!     Y := Y + Z            or:
!     Y +:= Z
!
!Here, Z is popped so that the Y operand becomes the new top-of-stack Z.
!But usually the new stack top is designated as Z':
!
!     Z' := Y + Z

!Immediate operand:
!   A			(various)
!Extra info:
!   op			opindex
!   fn			fnindex
!   cc			cond code
!   t[:size]    type (:size for block types)
!   u           secondary type for some ops (convert etc)
!   n			nargs for calls
!   s x			scale and offset for ptr/offset ops
!   x y			min/max lab index for switch
!	B			Secondary operand in a following kopnd instruction
!	C			Tertiary operand in a following kopnd instruction

!Stack usage is represented by (a b):
! a is the number of stack elements that will be popped
! b is the number of new stack elements that will be pushed
! Something like (1 1) can mean the same element stays in place

const MA = memaddr_opnd
const M  = mem_opnd
const L  = label_opnd
const S  = string_opnd
const A  = any_opnd

export enumdata [0:]ichar pclnames,
				[0:]byte pclhastype,
				[0:]byte pclextra =

!                       t  x      (a   b)
	(knop=0,       $+1, 0, 0),  ! (0 - 0) (          ) ?

	(kload,        $+1, 1, 1),  ! (0 - 1) (M L t i   ) Z' := M &M L &L 123 4.5 "abc"; i=1 for in-place ref
	(kiload,       $+1, 1, 0),  ! (1 - 1) (t         ) Z' := Z^
	(kiloadx,      $+1, 1, 2),  ! (2 - 1) (t d       ) Z' := (Y + Z*s + d)^

	(kstore,       $+1, 1, 0),  ! (1 - 0) (M t       ) M := Z
	(kistore,      $+1, 1, 0),  ! (2 - 0) (t         ) Z^ := Y
	(kistorex,     $+1, 1, 2),  ! (3 - 0) (t s d     ) (Y + Z*s + d)^ := X
	(kstorem,      $+1, 1, 0),  ! (2 - 1) (t         ) Z' :=(Y, Z) for mem:16

	(kdupl,        $+1, 0, 0),  ! (1 - 2) (          ) Z' := Y' := Z
	(kdouble,      $+1, 0, 0),  ! (1 - 2) (          ) Count extra instance of Z
	(kswapstk,     $+1, 0, 2),  ! (2 - 2) (a b       ) Swap(stack(a, 0), stack(b)); 1/2/3/4 = Z/Y/X/W
	(kunload,      $+1, 1, 0),  ! (1 - 0) (t         ) Pop stack

	(kcallp,       $+1, 0, 2),  ! (n - 0) (M n v     ) Call &M with nargs, then pop args; v = varargs
	(kicallp,      $+1, 0, 2),  ! (n - 0) (n v       ) Call Z with nargs, then pop args (a=n+1)
	(kretproc,     $+1, 0, 0),  ! (0 - 0) (          ) Return from proc
	(kcallf,       $+1, 1, 2),  ! (n - 1) (M t n v   ) Call &M, then pop args, leave retval; v = varrgs
	(kicallf,      $+1, 1, 2),  ! (n - 1) (t n v     ) Call Z, then pops args, leave retval (a=n+1)
	(kretfn,       $+1, 1, 0),  ! (0 - 0) (t         ) Return from func with Z=retval

	(kjump,        $+1, 0, 0),  ! (0 - 0) (L         ) goto L
	(kijump,       $+1, 1, 0),  ! (1 - 0) (          ) goto Z
	(kjumpcc,      $+1, 1, 1),  ! (2 - n) (L t c p   ) goto L when Y c Z; p=1: Z':=Y (b=0/1)
	(kjumpt,       $+1, 1, 0),  ! (1 - 0) (L t       ) goto L when Z is true
	(kjumpf,       $+1, 1, 0),  ! (1 - 0) (L t       ) goto L when Z is false
	(kjumpret,     $+1, 1, 0),  ! (1 - 0) (L t       ) goto L, common return point; deal with any ret value on stack

	(ksetcc,       $+1, 1, 0),  ! (2 - 1) (t c       ) Z' := Y cc Z

	(kswitch,      $+1, 1, 2),  ! (1 - 0) (L t x y   ) L=jumptab; B=elselab; x/y=min/max values
	(kswlabel,     $+1, 0, 0),  ! (0 - 0) (L         ) jumptable entry
	(kendsw,       $+1, 0, 0),  ! (0 - 0) (          ) Mark end of switch jumptable
	(kopnd,        $+1, 1, 0),  ! (0 - 0) (M L C t   ) Define auxiliary operand M or L

	(kadd,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y + Z

	(ksub,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y - Z
	(kmul,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y * Z
	(kdiv,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y / Z
	(kidiv,        $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y % Z
	(kirem,        $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y rem Z
	(kidivrem,     $+1, 1, 0),  ! (2 - 2) (t         ) Z' := divrem(Y, Z)
	(kbitand,      $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y iand Z
	(kbitor,       $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y ior Z
	(kbitxor,      $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y ixor Z
	(kshl,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y << Z
	(kshr,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y >> Z
	(kaddpx,       $+1, 1, 2),  ! (2 - 1) (t s d     ) Z' := Y + Z*s + d
	(ksubpx,       $+1, 1, 2),  ! (2 - 1) (t s d     ) Z' := Y - Z*s + s
	(ksubp,        $+1, 1, 1),  ! (2 - 1) (t s       ) Z' := (Y - Z)/s

	(kneg,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := -Z
	(kbitnot,      $+1, 1, 0),  ! (1 - 1) (t         ) Z' := inot Z
	(knot,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := not Z
	(ktoboolt,     $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := istrue Z; u is of type u; result is type t
	(ktoboolf,     $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := not istrue Z

	(kincrto,      $+1, 1, 1),  ! (1 - 0) (t n       ) Z^ +:= n
	(kdecrto,      $+1, 1, 1),  ! (1 - 0) (t n       ) Z^ -:= n
	(kincrload,    $+1, 1, 1),  ! (1 - 1) (t n       ) Z' := (Z +:= n)^
	(kdecrload,    $+1, 1, 1),  ! (1 - 1) (t n       ) Z' := (Z -:= n)^
	(kloadincr,    $+1, 1, 1),  ! (1 - 1) (t n       ) Z' := Z++^ (difficult to express step)
	(kloaddecr,    $+1, 1, 1),  ! (1 - 1) (t n       ) Z' := Z--^

	(kaddto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ +:= Y
	(ksubto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ -:= Y
	(kmulto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ *:= Y
	(kdivto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ /:= Y
	(kidivto,      $+1, 1, 0),  ! (2 - 0) (t         ) Z^ %:= Y
	(kiremto,      $+1, 1, 0),  ! (2 - 0) (t         ) Z^ rem:= Y
	(kbitandto,    $+1, 1, 0),  ! (2 - 0) (t         ) Z^ iand:= Y
	(kbitorto,     $+1, 1, 0),  ! (2 - 0) (t         ) Z^ ior:= Y
	(kbitxorto,    $+1, 1, 0),  ! (2 - 0) (t         ) Z^ ixor:= Y
	(kshlto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ <<:= Y
	(kshrto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ >>:= Y
	(kaddpxto,     $+1, 1, 1),  ! (2 - 0) (t s       ) Z^ +:= Y*s
	(ksubpxto,     $+1, 1, 1),  ! (2 - 0) (t s       ) Z^ -:= Y*s

	(kfloat,       $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Int   to real t
	(kfix,         $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Real   to int t
	(ktruncate,    $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,u) Mask to width of u, but type is widened to t
	(kwiden,       $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Mask to width of u, but type is widened to t
	(kfwiden,      $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r32 to r64
	(kfnarrow,     $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r64 to r32

	(kstartmx,     $+1, 0, 0),  ! (0 - 0) (          ) -
	(kresetmx,     $+1, 1, 0),  ! (0 - 0) (t         ) -
	(kendmx,       $+1, 1, 0),  ! (0 - 0) (t         ) -

	(kproc,        $+1, 0, 0),  ! (0 - 0) (M         ) ?
	(kendproc,     $+1, 0, 0),  ! (0 - 0) (          ) ?
	(kistatic,     $+1, 1, 0),  ! (0 - 0) (M t       ) Define idata label (must be followed by correct DATA ops)
	(kzstatic,     $+1, 1, 0),  ! (0 - 0) (M t       ) Define zdata label and reserve sufficient space
	(kdata,        $+1, 1, 2),  ! (0 - 0) (M L C t   ) Constant data. For block types, there can be multiple C values

	(klabel,       $+1, 0, 0),  ! (0 - 0) (          ) ?
	(klabeldef,    $+1, 0, 0),  ! (0 - 0) (          ) ?
	(ksetjmp,      $+1, 0, 0),  ! (1 - 0) (          ) For C
	(klongjmp,     $+1, 0, 0),  ! (1 - 1) (          ) For C

	(ksetcall,     $+1, 0, 1),  ! (0 - 0) (n s       ) n=args, s=1 for simple call

	(ksetarg,      $+1, 0, 2),  ! (0 - 0) (n1 n2     ) n1=arg no (LTR) n2=int or real arg no (maybe neg for real)
!	(kloadall,     $+1, 0, 0),  ! (0 - 0) (          ) ?

	(keval,        $+1, 0, 0),  ! (1 - 0) (          ) Evaluate Z [load to an actual register], then pop
	(kcomment,     $+1, 0, 0),  ! (0 - 0) (C         ) Comment C (a string)
	(kendprog,     $+1, 0, 0),  ! (0 - 0) (          ) End-of-program marker.
end

global const kerror = knop

export enumdata [0:]ichar ccnames, [0:]byte revcc =
	(no_cc=0,	"xx",	0),
	(eq_cc,		"eq",	ne_cc),
	(ne_cc,		"ne",	eq_cc),
	(lt_cc,		"lt",	ge_cc),
	(le_cc,		"le",	gt_cc),
	(ge_cc,		"ge",	lt_cc),
	(gt_cc,		"gt",	le_cc),
end

export enumdata [0:]ichar idnames
	(null_id=0,		"--"),			!Not set (used for overall program name)
	(import_id,		"Import"),		!Imported symbol (proc or static)
	(proc_id,		"Proc"),		!Local proc
	(static_id,		"Static"),		!Local static
	(local_id,		"Local"),		!Function local var
	(param_id,		"Param"),		!Function param
	(label_id,		"Label"),		!Used in assembly
	(export_id,		"Export"),		!Used by makesymbol, is converted to proc_id/.exported
	(misc_id,		"Misc"),		!?
	(program_id,	"Program"),		!?
end

=== mc_auxmcl.m 0 0 19/24 ===
!Auxially routines called by genmcl's PX handlers

ref mclrec mclframesetup

global proc initproc(psymbol d)=
!initialise genmcl pass through proc pcl code
	psymbol e

	clear regset
	clear xregset
	clear workregs
	clear workxregs
	int reg, xreg, n, r, npregs

!NEW CODE REQUIRED WHICH SETS NWORK/X/REGS
!Works with INFO=NIL, then not critical, but will be some of R0-R9
!Or uses info to decide how many and where; then they will be
!a combo of R0-R2, R9 downtowards R3 (depends on maxregvars), and possibly R12/R12
	nworkregs:=3
	workregs[r0]:=1					!these are always given
	workregs[r1]:=1
	workregs[r2]:=1

	nworkxregs:=2
	workxregs[r4]:=1
	workxregs[r5]:=1
	npregs:=0

	nworkregs:=10
	nworkxregs:=12
	for r in r3..r9 do workregs[r]:=1 od
	for r in r6..r15 do workxregs[r]:=1 od

	clear usedregs
	clear usedxregs
	clear pcltempflags
	r10used:=r11used:=0

	mstackdepth:=0
	noperands:=0

	frameoffset:=paramoffset:=framebytes:=0
	localshadow:=0

	nblocktemps:=0

	if d.mode=tpblock then
		e:=pc_makesymbol("$1x", misc_id)
		e.mode:=d.mode
		e.used:=1
		e.id:=param_id
		e.nextparam:=currfunc.nextparam
		e.owner:=currfunc
		currfunc.nextparam:=e
		blockretname:=e
	fi
end

global proc do_procentry(pcl p)=
	int retmode, ntemps, hasequiv, offset, size, reg
	mclopnd ax
	psymbol d
	[100]char str, newname
	int rr, ff

	setmclentry(mclprocentry)

	bspill:=bxspill:=0

	for r in r3..r9 when usedregs[r] do ++bspill od
	for r in r6..r15 when usedxregs[r] do ++bxspill od

	d:=currfunc.nextparam
	while d, d:=d.nextparam do
		d.offset:=paramoffset+16+(bspill+bxspill)*8
		genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))

		paramoffset+:=8
	od

	retmode:=currfunc.mode

	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do
		size:=psize[d.mode]
		if d.mode=tpblock then
			size:=d.size
		fi
		nextloop unless d.used				!also skips statics added in fixmain

		frameoffset-:=roundsizetg(size)
		d.offset:=frameoffset
		genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
	od

	ntemps:=0
	for i to maxoperands when pcltempflags[i] do
		++ntemps
		frameoffset-:=8
		ax:=pcltempopnds[i]
		ax.offset:=frameoffset
!		genmc(m_definetemp, mgenname(gettempname(currfunc,i)), mgenint(ax.offset))
		genmc(m_define, mgenname(gettempname(currfunc,i)), mgenint(ax.offset))
	od

	framebytes:=-frameoffset

	if (bspill+bxspill).odd then				!need an even number to keep stack alighnment correct
		unless framebytes iand 8 then
			framebytes+:=8
		end
	else
		if framebytes iand 8 then
			framebytes+:=8
		fi
	fi

	if localshadow then
		framebytes+:=32				!shadow space
	fi

!spill any bregs
	if bspill then
		for r:=r3 to r9 when usedregs[r] do
			genmc(m_push, mgenreg(r, tpu64))
		od
	fi

	if bxspill then
		ax:=mgenreg(r0, tpu64)
		for r:=xr6 to xr15 when usedxregs[r] do
			genmc(m_movq, ax, mgenxreg(r))
			genmc(m_push, ax)
		od
	fi

	MGENCOMMENT("?]]")
	MCLFRAMESETUP:=MCCODEX

	spillparams()

	MCOMM("---------------")
	RESETMCLENTRY()
end

global proc do_procexit=
	mclopnd ax
	int offset

	MCOMM("---------------")

	SETMCLENTRYF(mclframesetup)

	if framebytes or currfunc.nparams OR USEDREGS[RFRAME] then
		if usedregs[rframe] then
			genmc(m_push, dframeopnd)
			genmc(m_mov, dframeopnd, dstackopnd)
			pushstack(framebytes)
		else
			IF FRAMEBYTES THEN
				pushstack(framebytes+8)
			FI
		fi
	fi
	RESETMCLENTRYF()

	if framebytes or currfunc.nparams OR USEDREGS[RFRAME] then
		if usedregs[rframe] then
			popstack(framebytes)
			genmc(m_pop, dframeopnd)
		else
			IF FRAMEBYTES THEN
				popstack(framebytes+8)
			FI
		fi
	fi

	if bxspill then
		ax:=mgenreg(r10, tpu64)
		for r:=xr15 downto xr6 when usedxregs[r] do
			genmc(m_pop, ax)
			genmc(m_movq, mgenxreg(r), ax)
		od
	fi

	if bspill then
		for r:=r9 downto r3 when usedregs[r] do
			genmc(m_pop, mgenreg(r, tpu64))
		od
	fi

	genmc(m_ret)
end

proc spillparams=
	psymbol d
	mclopnd ax
	int offset:=16, regoffset:=0, xregoffset, firstoffset

	regoffset:=0

	d:=currfunc.nextparam

	if currfunc.variadic then				!C proc def using ...
		firstoffset:=d.offset				!param offsets may be pushed up

		for i:=currfunc.nparams to 3 do				!0-based; if nparams=2, loops over 2..3 as 0..1 are normal
			ax:=mgenindex(areg:rframe, size:8, offset:i*8+firstoffset)
			genmc(m_mov, ax, mgenreg(i+r10))
		od
	fi

	while d, d:=d.nextparam do
		if regoffset>3 then exit fi

		if d.used  then
			ax:=mgenindex(areg:rframe, size:8, offset:d.offset)
			case d.mode
			when tpr64 then
				genmc(m_movq, ax, mgenxreg(regoffset+xr0))
			when tpr32 then
				genmc(m_movd, changeopndsize(ax,4), mgenxreg(regoffset+xr0))
			else
				genmc(m_mov, ax, mgenreg(regoffset+r10))
			esac
		fi

		offset+:=8
		++regoffset
	od

end

global proc do_jumptruefalse(pcl p, int cond)=
	mclopnd ax, bx

	ax:=loadopnd(zz, pmode)

	if ispint(pmode) then
		genmc(m_test, ax,ax)

	else
		bx:=getworkregm(pmode)
		genmc(m_xorps+ispwide(pmode), bx, bx)
		genmc(m_comiss+ispwide(pmode), ax, bx)
	fi

	genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))

	poppcl()
end

global proc do_bitwise(pcl p, int opc)=
	mclopnd ax,bx

	ax:=loadopnd(yy, pmode)
	bx:=getopnd(zz, pmode)

	genmc(opc, ax, bx)

	poppcl()
end

global proc do_shift(pcl p, int opc)=
	mclopnd ax, cx
	pcl y

	ax:=loadopnd(yy, pmode)

	y:=pclopnd[zz]

	if pclloc[zz]=pcl_loc and y.opndtype=int_opnd then
		genmc(opc, ax, mgenint(y.value))
	else
		genmc(m_push, mgenreg(r10)) when r10used
		cx:=loadparam(zz, tpu8, r10)
		genmc(opc,ax, cx)
		genmc(m_pop, mgenreg(r10)) when r10used
	fi
	poppcl()
end

proc setmclentry(ref mclrec p)=
!temporarily set mcl insertion before p

	mce_oldmccodex:=mccodex
	mccodex:=p
	mce_lastmcl:=p.lastmcl
	mce_nextmcl:=p.nextmcl
end

func resetmclentry:ref mclrec pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.lastmcl:=mce_lastmcl
	mccodex.nextmcl:=mce_nextmcl
	pnew:=mccodex
	mccodex:=mce_oldmccodex
	pnew
end

proc setmclentryf(ref mclrec p)=
!temporarily set mcl insertion before p

	mcf_oldmccodex:=mccodex
	mccodex:=p
	mcf_lastmcl:=p.lastmcl
	mcf_nextmcl:=p.nextmcl
end

func resetmclentryf:ref mclrec pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.lastmcl:=mcf_lastmcl
	mccodex.nextmcl:=mcf_nextmcl
	pnew:=mccodex
	mccodex:=mcf_oldmccodex
	pnew
end

global proc do_pushlowargs(int nargs, nvariadics=0, isptr=0, pstack=0)=
!

!nargs=0 to 4 /operands/, not using more than 4 slots
!load args to D10-13/X0-3
!does not do anything with the stack at all
! Params are categorised as follows:
! Variadic:
!   float:  load to both D and X registers
!   other:  load to D register only
! Normal/non-variadic:
!   float:  load to X register
!   other:  load to D register
!PSTACK is 1, if call* data structures ate not used, eg. via CALLRTS
!Then use mode info from pclstack

	mclopnd ax
	int j,k, nextireg, nextxreg, mode, imode, blockret
	psymbol dblock

	if nargs=0 then return fi

	if pstack then
		blockret:=0
	else
		blockret:=callblockret[ncalldepth]
	fi

	nextireg:=r10
	nextxreg:=xr0

	k:=0
	for i:=noperands downto noperands-nargs+1 do
		++k						!counts params from 1

		if k=1 and blockret then
			dblock:=newblocktemp(callblocksize[ncalldepth])
			dblock.used:=1
			genmc(m_lea, mgenreg(r10), mgenmem(dblock))

		else

			j:=i-isptr+BLOCKRET

			if pstack then
				mode:=pclmode[j]				!will never be a block
			else
				mode:=callargmode[ncalldepth,k]
			fi

			case mode
			when tpblock then
				ax:=loadparam(j, mode, nextireg)
				copyblockarg(ax, callargsize[ncalldepth,k], k)

			when tpr64, tpr32 then
				loadparam(j, mode, nextxreg)

				if nvariadics and k>=nvariadics then			!variadic floats go to both regs

					imode:=(mode=tpr32|tpu32|tpu64)
					genmc(m_mov, mgenreg(nextireg, imode), mgenreg(nextxreg, mode))
				fi
			else
doint:
				loadparam(j, mode, nextireg)
			esac
		fi

		++nextireg
		++nextxreg
	od
end

global proc do_getretvalue(pcl p)=
	int reg,xreg,i,n, m
	[10]int modes

	if p.mode then
		pushpcl_reg(p.mode, r0)
	fi
end

global func ismemaddr(int n)int=
	if pclloc[n]=pcl_loc and pclopnd[n].opndtype=memaddr_opnd then return 1 fi
	return 0
end

global proc do_incr(pcl p, int incrop, addop)=
	mclopnd mx

	mx:=getopnd_ind(zz, p.mode)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi
	poppcl()
end

global proc do_incrload(pcl p, int incrop, addop)=
	mclopnd ax, mx

	mx:=getopnd_ind(zz, pmode)
	ax:=getworkreg_rm(pclreg[zz], pmode)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	genmc(m_mov, ax, mx)

!now replace ax opnd with new value
	pclloc[zz]:=reg_loc
	pclopnd[zz]:=nil
	pclreg[zz]:=ax.reg
	pclmode[zz]:=pmode

end

global proc do_loadincr(pcl p, int incrop, addop)=
	mclopnd ax,mx

	mx:=getopnd_ind(zz, pmode)

	pushpcl_reg(pmode)			!to hold loaded value
	ax:=getopnd(zz, pmode)

	genmc(m_mov, ax, mx)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	swapopnds(yy,zz)
	poppcl()
end

global func scaleindex(mclopnd ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale fi

	mulimm(ax,scale)
	return 1
end

global proc mulimm(mclopnd ax, int n)=
!multiply operand in ax (a simple reg) by constant n
!will try efficient method if possible, otherwise use normal multiply 
	int shifts,m
	mclopnd bx

	case n
	when 0 then
		clearreg(ax)
		return
	when 1 then
		return
	when -1 then
		genmc(m_neg, ax)
		return
	esac

	shifts:=0
	m:=n

	while m.even do
		m>>:=1
		++shifts
	od

	if shifts then
		genmc(m_shl, ax, mgenint(shifts))
	fi

	case m
	when 1 then
		return
	when 3, 5, 9 then
		genmc(m_lea, ax, mgenindex(areg: ax.reg, ireg:ax.reg, scale:m-1))
	else						!mul needed anyway; forget the shift
		if shifts then
			mccodex.opcode:=m_imul2
			mccodex.b:=mgenint(n)
		else
!			genmc(m_imul2, ax, mgenint(n))

			bx:=getworkregm((ax.size=4|tpi32|tpi64))
			genmc(m_mov, bx, mgenint(n))
			genmc(m_imul2, ax, bx)

		fi
	esac
end

global func do_addrmode*(pcl p)mclopnd px =
!Top two stack elements are an array (yy) and index (zz)
!Return a operand which provdes the address mode to access the element,
!for either reading or writing
!The address mode will use 0, 1 or 2 registers. The registers may be 1 or 2
!associated with the pcl operands, or may be regvars.
!If for reading, caller will need to make their own arrangements for a dest reg.
!When Xb has to be loaded into a register anyway, then the caller can make use
!of that

	mclopnd ax,bx
	int scale, extra,offset, reg,regix
	psymbol d
	pcl q

	scale:=p.scale
	extra:=p.extra

	q:=isimmload(zz)
	if q then
		offset:=q.value*scale+extra	!for imm offset
	fi

	px:=nil

	if ismemaddr(yy) then
		d:=pclopnd[yy].def
		if d.id in [static_id, import_id] and phighmem=2 or D.ID=PARAM_ID AND D.MODE=TPBLOCK  then skip fi

		if q then			!memaddr/imm
			px:=mgenindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=loadopnd(zz, tpi64),scale)
			px:=mgenindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
skip:
		ax:=loadopnd(yy, tpu64)

		if pclloc[zz]=reg_loc then			!any/regvar
			reg:=pclreg[zz]
			regix:=scaleregvar(reg,scale,zz)
			px:=mgenindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)

		elsif q then						!any/imm	
			px:=mgenindex(areg:ax.reg, offset:offset)
		else
			scale:=scaleindex(bx:=loadopnd(zz, tpu64),scale)
			px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	fi

	px.size:=psize[p.mode]

	return px
end

function scaleregvar(int reg, &scale, n)int=
!When scale is 1/2/3/4, return reg (a regvar) and scale unchanged;
!otherwise set up a new register for operand n
!Copy reg to it, and scale. Return new reg, and set scale to 1
	int regix
	mclopnd ax

	if scale in [1,2,4,8] then return reg fi

	regix:=getworkireg()
	ax:=mgenreg(regix)

	IF SCALE=16 THEN
		genmc(m_lea, ax, mgenindex(ireg:reg, areg:reg, scale:1))
		scale:=8

	ELSE
		genmc(m_mov,ax, mgenreg(reg))
		mulimm(ax,scale)
		scale:=1
	FI

	pclloc[n]:=reg_loc
	pclreg[n]:=regix
	pclmode[n]:=tpi64
	pclopnd[n]:=nil

	return regix
end

global proc dolea(mclopnd ax, px)=
!do 'lea ax, px`, but suppress in cases like 'lea d0,[d0]'
	unless px.regix=px.valtype=px.offset=0 and px.reg=ax.reg then

		genmc(m_lea, ax, px)
	end
end

global proc do_binto(pcl p, int opc, fopc)=
	mclopnd ax,bx,rx

	if ispfloat(pmode) then
		do_binto_float(p, fopc)
		return
	fi

	ax:=getopnd_ind(zz, p.mode)
	bx:=loadopnd(yy, p.mode)

	genmc(opc,ax,bx)
	poppcl()
	poppcl()
end

global proc do_binto_float(pcl p, int opc)=
	mclopnd px,bx,cx

	pushpcl_reg(pmode)		!z^:=y => y^:=x; z is temo

	px:=getopnd_ind(yy, pmode)
	bx:=getopnd(xx, pmode)
	cx:=getopnd(zz, pmode)

	genmc(m_mov, cx, px)
	genmc(opc+ispwide(pmode), cx, bx)
	genmc(m_mov, px,cx)

	poppcl()
	poppcl()
	poppcl()
end

global proc do_shiftnto(pcl p, int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	mclopnd px, cx

	px:=getopnd_ind(zz, pmode)

	if pclloc[yy]=pcl_loc and pclopnd[yy].opndtype=int_opnd then
		genmc(opc, px, mgenint(pclopnd[yy].value))

	else
		genmc(m_push, mgenreg(r10)) when r10used

		cx:=loadparam(yy, tpu8, r10)
		genmc(opc, px, cx)

		genmc(m_pop, mgenreg(r10)) when r10used

	fi

	poppcl()
	poppcl()
end

global proc do_divrem(pcl p, int issigned, isdiv)=
!isdiv = 0/1/2 = rem/div/divrem
! Z' := Y % Z
	mclopnd ax, bx, px
	pcl q
	int opc, n, shifts
	byte fdivto:=0
	int locyy:=yy, loczz:=zz

	if p.opcode in [kidivto, kiremto] then
		swap(locyy, loczz)

		ax:=loadopnd(locyy, tpu64)
		fdivto:=1
		genmc(m_push, changeopndsize(ax,8))
		px:=makeopndind(ax, pmode)
		ax:=mgenreg(ax.reg, pmode)

		genmc(m_mov, ax, px)
	else
		ax:=loadopnd(locyy, pmode)
	fi

	q:=isimmload(loczz)

	if q and isdiv=1 then
		n:=q.value
		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			poppcl()
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts AND NOT FDIVTO then
				genmc((issigned|m_sar|m_shr), ax, mgenint(shifts))
				poppcl()
				return
			fi
		esac
	fi 

	bx:=loadopnd(loczz, pmode)

	saverdx()
	fixdivopnds(locyy, loczz)
	bx:=loadopnd(loczz, pmode)			!in case regs have changed

	if issigned then
		opc:=
			case psize[pmode]
			when 8 then	m_cqo
			when 4 then	m_cdq
			when 2 then	m_cwd
			else merror("div/u8"); 0
			esac
		genmc(opc)

		opc:=m_idiv
	else
!		genmc(m_xorx, mgenreg(r11),mgenreg(r11))
		clearreg(mgenreg(r11))
		opc:=m_div
	fi

	genmc(opc, bx)

	case isdiv
	when 0 then				!rem
		genmc(m_xchg, mgenreg(r0), mgenreg(r11))

	when 2 then				!divrem
		genmc(m_xchg, bx, mgenreg(r11))			!rem replace y-operand
		swapopndregs(r1)						!make sure it is in r1
		swapopnds(locyy,loczz)

	esac

	restorerdx()

	if fdivto then
		bx:=getworkregm(tpu64)
		genmc(m_pop, bx)
		genmc(m_mov, makeopndind(bx, pmode), getopnd(locyy, pmode))
		poppcl()
	fi

	if isdiv<>2 then
		poppcl()
	fi

end

proc fixdivopnds(int locyy, loczz)=
!two div operands exist as the top two operands, which will be
!in registers
!the div op requires that x is in d0, and y in any other register
!d11 also needs to be free, which will be the case is reg allocs only
!go up to d9, and d10/d11/12/13 are in use for win64 parameter passing
	int regx,regy,zop
	mclopnd bx, ax

	regx:=pclreg[locyy]
	regy:=pclreg[loczz]

	if regx=r0 then			!regy will be OK
		return
	fi

	bx:=getopnd(locyy, tpu64)
	ax:=getopnd(loczz, tpu64)

	if regy=r0 then			!need to swap then
		genmc(m_xchg, bx, ax)
		swapopnds(locyy,loczz)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset[r0]=0 then	!d0 not in use
		genmc(m_xchg, mgenreg(r0), bx)
		regset[regx]:=0				!switch registers for yy

		pclreg[locyy]:=r0
		regset[r0]:=1

		return
	fi

!need to move current occupier of r0
	for zop:=noperands downto 1 do
		if pclloc[zop]=reg_loc and pclreg[zop]=r0 then exit fi
	else
		return
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg, mgenreg(r0), getopnd(locyy, tpu64))	
	swap(pclreg[locyy], pclreg[zop])		!switch registers
end

proc saverdx=
	genmc(m_push, mgenreg(r11)) when r11used
end

proc restorerdx=
	genmc(m_pop, mgenreg(r11)) when r11used
end

global proc do_blockdata(pcl p) =
	ref byte s
	ref u64 d
	int n,nqwords,nwords,r

	n:=p.size
	return when n=0

	nwords:=n/8

	d:=cast(p.svalue)
	to nwords do
		genmc(m_dq, mgenint(d++^))
	od

	r:=n-nwords*8
	if r then
		genstring_db(cast(d), r, 'B')
	fi
	MGENCOMMENT("ENDDATA")

end

global proc copyblock(mclopnd ax,bx, int n, savedest=1)=
!ax,bx refer to memory; do ax:=bx for n bytes
!savedest=1 to ensure that the value in ax register is not modified

	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg, axreg
	byte saved:=0

	if n=16 then
		rx:=getworkregm(tpr64)
!
		genmc(m_movdqu, rx, bx)
		genmc(m_movdqu, ax, rx)

		return
	fi

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=getworkregm(tpu64)		!work reg

	offset:=0

	if 1<=nwords<=4 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax, targetsize)
		bx:=changeopndsize(bx, targetsize)

		to nwords do
			genmc(m_mov, rx, applyoffset(bx, offset))
			genmc(m_mov, applyoffset(ax, offset), rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop
		rcount:=getworkregm(tpu64)
		lab:=++mlabelno
		if savedest then
			axreg:=ax.reg
			genmc(m_push, mgenreg(axreg))
			saved:=1
		fi

		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)
		ax.size:=8

		genmc(m_mov, rcount, mgenint(nwords))
		genmc(m_labelx, mgenlabel(lab))
		genmc(m_mov, rx, bx)
		genmc(m_mov, ax, rx)

		genmc(m_add, mgenreg(ax.reg), mgenint(targetsize))
		genmc(m_add, mgenreg(bx.reg), mgenint(targetsize))

		genmc(m_dec, rcount)
		genmc_cond(m_jmpcc, ne_cond, mgenlabel(lab))

		offset:=0
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx, 4)
			genmc(m_mov, rx, applyoffset(bx, offset, 4))
			genmc(m_mov, applyoffset(ax, offset, 4), rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx, 2)
			genmc(m_mov, rx, applyoffset(bx, offset, 2))
			genmc(m_mov, applyoffset(ax, offset, 2), rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx, 1)
			genmc(m_mov, rx, applyoffset(bx, offset, 1))
			genmc(m_mov, applyoffset(ax, offset, 1), rx)
		fi
	fi
	if saved then
		genmc(m_pop, mgenreg(axreg))
	fi
end

global proc genstringtable=
	ref constrec p

	return unless cstringlist

	mgencomment("String Table")

	setsegment('I',8)

	if kk0used then
		genmc(m_labelx,mgenlabel(kk0used))
		gendb(0)
	fi

	p:=cstringlist
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))
		genstring_db(p.svalue, p.slength, strtype:1)
	od
end

global proc genstring_db(ichar s, int length, strtype)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
!strtype should be zero for a normal string, then a zero-terminator is added.
	int i, c, seqlen
	ref char seq

	if length=-1 then
		length:=strlen(s)
	fi

	if length=0 then
		gendb(0)
		return
	fi

	seqlen:=0

	to length do
		c:=s++^
		if c<32 or c>=127 or c in ['\"', '\\'] then
			if seqlen then
				gendbstring(seq, seqlen)
				seqlen:=0
			fi
			gendb(c)
		else
			if seqlen=0 then
				seqlen:=1
				seq:=s-1
			else
				++seqlen
			fi
		fi
	od
	if seqlen then
		gendbstring(seq,seqlen)
	fi
	if strtype=0 then
		gendb(0)
	fi
end

proc gendb(int a)=
	genmc(m_db,mgenint(a))
end

proc gendbstring(ichar s, int length)=
!string is printable, and doesn't include double quotes
	genmc(m_ascii,mgenstring(s,length))
end

proc gendq(int a)=
	genmc(m_dq,mgenint(a))
end

global proc genrealtable=
	ref constrec p

	return unless creallist or cr32list

	mgencomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))

		if p.xvalue=infinity then
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		else
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		fi
	od

	mgencomment("Real32 Table")
	p:=cr32list
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))
		if p.xvalue=infinity then
			genmc(m_dd, mgenint(i32@(r32(p.xvalue))))
		else
			genmc(m_dd, mgenrealimm(p.xvalue,tpr32))
		fi

	od
end

global proc genabsneg=
	if labneg32+labneg64 then
		setsegment('I',16)
	fi

	if labneg32 then
		mgencomment("labneg32")
		genmc(m_labelx,mgenlabel(labneg32))
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mgencomment("labneg64")
		genmc(m_labelx,mgenlabel(labneg64))
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mgencomment("labzero")
		genmc(m_labelx,mgenlabel(labzero))
		gendq(0)
	fi

	if labmask63 then
		mgencomment("mask63/offset64")
		genmc(m_labelx,mgenlabel(labmask63))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc(m_labelx,mgenlabel(laboffset64))
		gendq(0x43E0'0000'0000'0000)
	fi
end

global proc do_negreal(mclopnd ax, int mode)=
	if ispwide(pmode) then
		if not labneg64 then labneg64:=mcreatefwdlabel() fi
		genmc(m_xorpd, ax, mgenlabelmem(labneg64))
	else
		if not labneg32 then labneg32:=mcreatefwdlabel() fi
		genmc(m_xorps, ax, mgenlabelmem(labneg32))
	fi
end

global proc copyblockarg(mclopnd px, int size, ARGNO)=
!px refers to a block in a parameter register
!if px is nil, then called for block in zz that will be pushed

!copy the block to a block temp, and change px's register to
!refer to that new block

	psymbol dblock
	mclopnd ax, bx, axi, bxi

	IF PX=NIL THEN
		println "High block arg not copied in", currfunc.name,,"()"
		return
	FI

	dblock:=newblocktemp(size)
	dblock.used:=1


	if px then
		bx:=getworkregm(tpref)			!copy of px
		genmc(m_mov, bx, px)
	else
		bx:=loadopnd(zz, tpblock)
	fi
	ax:=getworkregm(tpref)			!refer to temp block

	genmc(m_lea, ax, mgenmem(dblock))

	copyblock(mgenireg(ax.reg), mgenireg(bx.reg), size)

	if px then
		genmc(m_lea, px, mgenmem(dblock))		!param points to temp
!	else
!		gen
	fi

!note: this is needed since there may be other blocks passed before the end
!of a CALL op, as those ax/bx regs would be tied up
!caller should ensure no workregs are in use

	freeworkregs(nil)
end

=== mc_decls.m 0 0 20/24 ===
export type mclopnd = ref mclopndrec

export record mclopndrec =
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		psymbol def
		i64 value		!immediate value
		r64 xvalue	!immediate real value, mainly for dq
		ichar svalue	!immediate string
		int labelno
		int sysfn
		int tempno
	end

	u16 misc: (			! bitfields
		size:5,		! one of 1 2 4 8
		scale:4,		! one of 1 2 4 8
		mode:3,			! R, X, imm, [mem]
		valtype:4)

	byte reg			!0, or main register
	byte regix			!0, or index register
	i32 offset			!additional offset to memory operands
end

export record mclrec = !$caligned
	ref mclrec lastmcl, nextmcl
	mclopnd a,b
	byte c
	byte opcode
	byte cond
	byte spare1
	u32 seqno
	union
		u32 mpos
		u32 lineno				!used by aa assembler
	end
	u32 spare2

	union
		[r0..r15]byte regfreed		!1 indicates work-register freed after this instr
		pair regfreedpr
	end
end

export enumdata =
	no_val=0,		!no operand
	intimm_val,		!immediate int
	realimm_val,	!immediate real (mainly for dq etc)
	realmem_val,	!indirect real (for movq etc)
	stringimm_val,	!immediate string, for comments, or address of string etc
	def_val,		!var/proc name
	label_val,		!label index
	name_val,		!immediate string must be output as ah unquoted name
	temp_val,		!index of pclopnd temp (later becomes ptr to descriptor?)
end

export enumdata []ichar mclnames =

	(m_procstart,		$),
	(m_procend,			$),
	(m_comment,			$),
	(m_labelname,		$),
	(m_define,			$),
	(m_trace,			$),
	(m_endx,			$),

	(m_labelx,			$),
	(m_nop,				$),

	(m_mov,				$),
	(m_push,			$),
	(m_pop,				$),
	(m_lea,				$),
	(m_cmovcc,			$),

	(m_movd,			$),
	(m_movq,			$),

	(m_movsx,			$),
	(m_movzx,			$),
	(m_movsxd,			$),

	(m_call,			$),
	(m_ret,				$),
	(m_leave,			$),
	(m_retn,			$),

	(m_jmp,				$),
	(m_jmpcc,			$),
	(m_xchg,			$),

	(m_add,				$),
	(m_sub,				$),
	(m_adc,				$),
	(m_sbb,				$),
	(m_imul,			$),
	(m_mul,				$),
	(m_imul2,			$),
	(m_imul3,			$),

	(m_idiv,			$),
	(m_div,				$),

	(m_andx,			$),
	(m_orx,				$),
	(m_xorx,			$),
	(m_test,			$),

	(m_cmp,				$),

	(m_shl,				$),
	(m_sar,				$),
	(m_shr,				$),
	(m_rol,				$),
	(m_ror,				$),
	(m_rcl,				$),
	(m_rcr,				$),

	(m_neg,				$),
	(m_notx,			$),

	(m_inc,				$),
	(m_dec,				$),

	(m_cbw,				$),
	(m_cwd,				$),
	(m_cdq,				$),
	(m_cqo,				$),
	(m_setcc,			$),

	(m_bsf,				$),
	(m_bsr,				$),

	(m_shld,			$),
	(m_shrd,			$),

	(m_sqrtss,			$),
	(m_sqrtsd,			$),

	(m_addss,			$),
	(m_addsd,			$),

	(m_subss,			$),
	(m_subsd,			$),

	(m_mulss,			$),
	(m_mulsd,			$),

	(m_divss,			$),
	(m_divsd,			$),

	(m_comiss,			$),
	(m_comisd,			$),
	(m_ucomisd,			$),

	(m_xorps,			$),
	(m_xorpd,			$),

	(m_andps,			$),
	(m_andpd,			$),

	(m_pxor,			$),
	(m_pand,			$),

	(m_cvtss2si,		$),
	(m_cvtsd2si,		$),

	(m_cvttss2si,		$),
	(m_cvttsd2si,		$),

	(m_cvtsi2ss,		$),
	(m_cvtsi2sd,		$),

	(m_cvtsd2ss,		$),
	(m_cvtss2sd,		$),

	(m_movdqa,			$),
	(m_movdqu,			$),

	(m_pcmpistri,		$),
	(m_pcmpistrm,		$),

	(m_fld,				$),
	(m_fst,				$),
	(m_fstp,			$),

	(m_fild,			$),
	(m_fist,			$),
	(m_fistp,			$),

	(m_fadd,			$),
	(m_fsub,			$),
	(m_fmul,			$),
	(m_fdiv,			$),
	(m_fsqrt,			$),
	(m_fsin,			$),
	(m_fcos,			$),
	(m_fsincos,			$),
	(m_fptan,			$),
	(m_fpatan,			$),
	(m_fabs,			$),
	(m_fchs,			$),

	(m_minss,			$),
	(m_maxss,			$),
	(m_minsd,			$),
	(m_maxsd,			$),

	(m_db,				$),
	(m_dw,				$),
	(m_dd,				$),
	(m_dq,				$),
	(m_ascii,			$),

	(m_isegment,		$),
	(m_zsegment,		$),
	(m_csegment,		$),

	(m_align,			$),
	(m_resb,			$),
	(m_resw,			$),
	(m_resd,			$),
	(m_resq,			$),

	(m_xlat,			$),
	(m_loopnz,			$),
	(m_loopz,			$),
	(m_loopcx,			$),
	(m_jecxz,			$),
	(m_jrcxz,			$),

	(m_cmpsb,			$),
	(m_cmpsw,			$),
	(m_cmpsd,			$),
	(m_cmpsq,			$),

	(m_rdtsc,			$),
	(m_popcnt,			$),
	(m_bswap,			$),

	(m_finit,			$),

	(m_fldz,			$),
	(m_fld1,			$),
	(m_fldpi,			$),
	(m_fld2t,			$),
	(m_fld2e,			$),
	(m_fldlg2,			$),
	(m_fldln2,			$),

	(m_cpuid,			$),

	(m_xxxx,			$),
	(m_halt,			$),
end

export enumdata [0:]ichar regnames =
	(rnone=0,	$),		!
	(r0,		$),		!d0 rax
	(r1,		$),		!d1 r10
	(r2,		$),		!d2 r11
	(r3,		$),		!d3 rdi
	(r4,		$),		!d4 rbx
	(r5,		$),		!d5 rsi
	(r6,		$),		!d6 r12
	(r7,		$),		!d7 r13
	(r8,		$),		!d8 r14
	(r9,		$),		!d9 r15
	(r10,		$),		!d10 rcx
	(r11,		$),		!d11 rdx
	(r12,		$),		!d12 r8
	(r13,		$),		!d13 r9
	(r14,		$),		!d14 rbp
	(r15,		$),		!d15 rsp

	(r16,		$),		!b0h ah
	(r17,		$),		!b1h bh
	(r18,		$),		!b10h ch
	(r19,		$),		!b11h dh
end

export const rframe = r14
export const rstack = r15

export enumdata [0:]ichar asmcondnames =

	(ov_cond=0,	"o"),
	(nov_cond,	"no"),

	(ltu_cond,	"b"),
	(geu_cond,	"ae"),

	(eq_cond,	"z"),
	(ne_cond,	"nz"),

	(leu_cond,	"be"),
	(gtu_cond,	"a"),

	(s_cond,	"s"),
	(ns_cond,	"ns"),

	(p_cond,	"p"),
	(np_cond,	"np"),

	(lt_cond,	"l"),
	(ge_cond,	"ge"),

	(le_cond,	"le"),
	(gt_cond,	"g"),

	(flt_cond,	"b"),
	(fge_cond,	"ae"),
	(fle_cond,	"be"),
	(fgt_cond,	"a"),
end

global const z_cond = eq_cond
global const nz_cond = ne_cond

!I use my own register designations Dn, An, Wn, Bn (8,4,2,1 bytes),
!which have a more sensible order than the official ones.
!The mapping is shown against Dn. Some (not all) of the official register
!names are used too

!Regindex is the ordinal value used to represent the register: 1..16
!This table is intended for initialising the global symbol table

export tabledata []ichar dregnames, []byte regsizes, []byte regindices =
	("rax",		8,	r0),
	("rbx",		8,	r4),
	("rcx",		8,	r10),
	("rdx",		8,	r11),
	("rsi",		8,	r5),
	("rdi",		8,	r3),
	("rbp",		8,	r14),
	("rsp",		8,	r15),
	("r8",		8,	r12),
	("r9",		8,	r13),
	("r10",		8,	r1),
	("r11",		8,	r2),
	("r12",		8,	r6),
	("r13",		8,	r7),
	("r14",		8,	r8),
	("r15",		8,	r9),

	("eax",		4,	r0),
	("ebx",		4,	r4),
	("ecx",		4,	r10),
	("edx",		4,	r11),
	("esi",		4,	r5),
	("edi",		4,	r3),
	("ebp",		4,	r14),
	("esp",		4,	r15),
	("r8d",		4,	r12),
	("r9d",		4,	r13),
	("r10d",	4,	r1),
	("r11d",	4,	r2),
	("r12d",	4,	r6),
	("r13d",	4,	r7),
	("r14d",	4,	r8),
	("r15d",	4,	r9),

	("ax",		2,	r0),
	("bx",		2,	r4),
	("cx",		2,	r10),
	("dx",		2,	r11),
	("si",		2,	r5),
	("di",		2,	r3),
	("bp",		2,	r14),
	("sp",		2,	r15),
	("r8w",		2,	r12),
	("r9w",		2,	r13),
	("r10w",	2,	r1),
	("r11w",	2,	r2),
	("r12w",	2,	r6),
	("r13w",	2,	r7),
	("r14w",	2,	r8),
	("r15w",	2,	r9),


	("al",		1,	r0),
	("bl",		1,	r4),
	("cl",		1,	r10),
	("dl",		1,	r11),

	("ah",		1,	r16),
	("bh",		1,	r17),
	("ch",		1,	r18),
	("dh",		1,	r19),

	("sil",		1,	r5),
	("dil",		1,	r3),
	("bpl",		1,	r14),
	("spl",		1,	r15),

	("r8b",		1,	r12),
	("r9b",		1,	r13),
	("r10b",	1,	r1),
	("r11b",	1,	r2),
	("r12b",	1,	r6),
	("r13b",	1,	r7),
	("r14b",	1,	r8),
	("r15b",	1,	r9),

end

export enumdata =
	no_seg=0,
	code_seg,
	idata_seg,
	zdata_seg,
	rodata_seg,
	impdata_seg,
end

export enumdata =
	extern_ref=0,	!is external
	fwd_ref,		!not yet reached
	back_ref,		!has been reached
end

export enumdata =
	a_none=0,
	a_reg,		! Ri
	a_imm,		! d including def name, label etc
	a_mem,		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
	a_cond,		! a condition code for jcc/setcc
	a_xreg,		! xmm register
end

!global const maxoperands=20
global const maxoperands=50

!following are continually updates as opnds are pushed, moved and popped
global [maxoperands]pcl		pclopnd			!pclrec describing opnd when not loaded
global [maxoperands]byte	pclreg			!>0 means in given register
global [maxoperands]byte	pclmode			!copy of mode, esp. if loaded (indicates reg/xreg)
global [maxoperands]byte	pclcount		!dupl count
global [maxoperands]byte	pclloc			!stores loc code

!following are reset per proc and augmented as it is processed
global [maxoperands]byte pcltempflags		!1 means a temp uses local storage
global [maxoperands]mclopnd pcltempopnds	!store mcl opnd for such a temp

global int noperands						!number of pcl operands, including wide
global int mstackdepth						!hw stack size (pcl operands, + extra for wide, + padding)

global enumdata =
	pcl_loc=0,				!operand still in pcl instruction
	reg_loc,				!is in register (look at mode for reg/xreg)
	temp_loc,				!overflow to temporary
end

global [r0..r15]byte workregs, workxregs		!1 indicates available work regs
global int nworkregs, nworkxregs				!no. workregs assigned
!global int maxregvars, maxxregvars				!no. reg vars available

global int xregmax


global [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
global [r0..r15]byte xregset		!same for xregs

global record pair =
	u64 low, high
end

global pair regsetpr @ regset
global const u64 invertbytes = 0x0101'0101'0101'0101

global [r0..r15]byte usedregs		!1 means used during proc
global [r0..r15]byte usedxregs		!1 means used during proc

global macro zz = noperands
global macro yy = noperands-1
global macro xx = noperands-2
global macro ww = noperands-3

export enumdata [0:]ichar xregnames =
	(xnone=0,	"-"),
	(xr0,		"xmm0"),
	(xr1,		"xmm1"),
	(xr2,		"xmm2"),
	(xr3,		"xmm3"),
	(xr4,		"xmm4"),
	(xr5,		"xmm5"),
	(xr6,		"xmm6"),
	(xr7,		"xmm7"),
	(xr8,		"xmm8"),
	(xr9,		"xmm9"),
	(xr10,		"xmm10"),
	(xr11,		"xmm11"),
	(xr12,		"xmm12"),
	(xr13,		"xmm13"),
	(xr14,		"xmm15"),
	(xr15,		"xmm15")
end

global const maxcalldepth=16
global [maxcalldepth]byte callalign					!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret				!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize				!size of any returned block
global [maxcalldepth, maxparams]byte callargmode	!arg mode set setarg
global [maxcalldepth, maxparams]u32 callargsize		!size incl block from setarg
global int ncalldepth

global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

export ref mclrec mccode, mccodex		!genmc adds to this linked list

global int currsegment=0

global mclopnd dstackopnd
global mclopnd dframeopnd

global [r0..r15,1..8]mclopnd regtable

global [-128..64]mclopnd frameregtable

global record constrec =
	union
		int value
		real xvalue
		ichar svalue
	end
	ref constrec nextconst
	int labelno
	int slength
end

global ref constrec cstringlist
global ref constrec creallist
global ref constrec cr32list

global psymbol currasmproc

export record relocrec =			!informal version
	ref relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
export record dbuffer =
	ref byte pstart
	union
		ref byte pcurr
		ref u16 pcurr16
		ref u32 pcurr32
		ref u64 pcurr64
	end
	ref byte pend
	int alloc
end

global int aaseqno
global int aapos

!The following are highly dependent on the ordering of the base types being:
! r32 r64 ints... block ..., with r32 having value 1
!They assume mode is not void, and for ispfloat, is not a block

global macro ispwide(m)  = m - 1
global macro ispfloat(m) = m <= tpr64
global macro ispint(m)   = m > tpr64	!when block type is not expected

EXPORT [1..8]byte regmodes=(tpu8, tpu16, 0, tpu32, 0,0,0, tpu64)

global byte pmode
global pcl currpcl

global ref mclrec mclprocentry
global ref mclrec mce_oldmccodex, mce_lastmcl, mce_nextmcl		!used by reset/setmclentry
global ref mclrec mcf_oldmccodex, mcf_lastmcl, mcf_nextmcl		!used by reset/setmclentry for frame setup

global const maxblocktemps=50
global [maxblocktemps]psymbol blockdefs
global int nblocktemps

global [pstdnames.bounds]byte ploadop

proc start=
	for i in ploadop.bounds do ploadop[i]:=m_nop od

	ploadop[tpu8]:=ploadop[tpu16]:=ploadop[tpu32]:=m_movzx
	ploadop[tpi8]:=ploadop[tpi16]:=ploadop[tpi32]:=m_movsx
	ploadop[tpr32]:=m_movd
	ploadop[tpr64]:=m_movq
	ploadop[tpu64]:=ploadop[tpi64]:=m_mov
end

=== mc_genmcl.m 0 0 21/24 ===

const fshowopndstack=0
!const fshowopndstack=1

GLOBAL INT DEBUG

global int frameoffset, paramoffset
global int framebytes

[pclnames.bounds]ref proc(pcl) px_handlertable

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global proc genmcl(ichar dummy=nil)=

!	return when mcldone

	IF FSHOWOPNDSTACK THEN CPL "********* ASM HAS PCL INFO *********" FI

	int tt:=os_clock()
	inithandlers()
	mclinit()

	currpcl:=pcstart

	int i:=0
	repeat
		convertpcl(currpcl)

		showopndstack() when fshowopndstack and currpcl.opcode not in [klabel, kcomment, kproc, kretproc, kendproc]

		++currpcl

	until currpcl>pccurr or currpcl.opcode=kendprog

	genrealtable()
	genabsneg()
	genstringtable()

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)

!	mcldone:=1

end

proc convertpcl(pcl p)=
	doshowpcl(p) when fshowil

	pmode:=p.mode
	currpcl:=p
	mmpos:=p.pos

	ppseqno:=p.seqno

	px_handlertable[p.opcode]^(p)

	[r0..r15]byte OLDREGSET
	pair oldregsetpr @ oldregset
	OLDREGSET:=REGSET
	clear regset
	clear xregset

	int reg
!
	for i to noperands do
		reg:=pclreg[i]
		if reg then
			if ispfloat(pclmode[i]) then
				xregset[reg]:=1
			else
				regset[reg]:=1
			fi
		fi
	od

	mccodex.regfreedpr.low ior:=oldregsetpr.low iand (regsetpr.low ixor invertbytes)
	mccodex.regfreedpr.high ior:=oldregsetpr.high iand (regsetpr.high ixor invertbytes)
end

proc inithandlers=
	static byte initdone=0
	ichar name, s
	int n

	if initdone then return fi

	n:=$getnprocs()

	for i to n do
		name:=$getprocname(i)
		if eqbytes(name,"px_",3) then
			for k in pclnames.bounds do
				s:=pclnames[k]
				if s^='k' then ++s fi				!some are kload, others just store
				if eqstring(s,name+3) then
					px_handlertable[k]:=$getprocaddr(i)
					exit
				fi
			else
				merror("Invalid handler name:",name)
			od
		fi
	od

	static [,2]byte dupltable = (

!mapping           =>
		(ktoboolf, 		ktoboolt),

		(kcallf,		kcallp),
		(kicallp,		kcallp),
		(kicallf,		kcallp),

		(kendmx,		kresetmx),

		(kidivto,		kidiv),
		(kiremto,		kirem)
		)

	for i to dupltable.len do
		px_handlertable[dupltable[i,1]]:=px_handlertable[dupltable[i,2]]
	end

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		fi
	od

	initdone:=1
end

proc doshowpcl(pcl p)=
	[1256]char str

	case p.opcode
	when kproc, kendproc, kistatic, kzstatic, kdata then
	else
		strcpy(str,"                       ")
		strcat(str,strpclstr(p, str.len))
		mgencomment(PCM_COPYHEAPSTRING(str))
	esac
end

proc unimpl(pcl p)=
	[100]char str
	fprint @str, "Unimpl: # (#)", pclnames[p.opcode], strpmode(pmode)
	CPL STR
	mgencomment(pcm_copyheapstring(str))
end

proc px_nop*(pcl p) =
! ?
!*!	unimpl(p)
end

proc px_dupl*(pcl p) =
! Z' := Y' := Z
	duplpcl()
end

proc px_double*(pcl p) =
! Count extra instance of Z (only works for top stack item)
	if ncalldepth then
		duplpcl()
	else
		++pclcount[noperands]
	fi
end

proc px_comment*(pcl p) =
! Comment C (a string)
!	unimpl(p)
end

proc px_proc*(pcl p) =
! ?
!Things that are remembered:

!PCLPROCDEF:	PCL op for kdefprocdef: used to repeat PASS2 pass for optimising
!				Note will normally skip back to following op, as below is for PASS1 only

!MCLPROCENTRY:	MCL op for dummy op (or anything that will work), used to insert
!				proc entry ops during do_procentry()

	currfunc:=p.def

	setsegment('C',1)

	genmc(m_procstart,mgenmemaddr(currfunc))
	genmc(m_labelname,mgenmemaddr(currfunc))

	initproc(currfunc)

!create dummy mcl op at which to insert hang proc-entry code onto later
	mgencomment("?>>")
	mclprocentry:=mccodex

end

proc px_endproc*(pcl p) =
! ?

	if noperands then

	cpl("PCL STACK NOT EMPTY"), CURRFUNC.NAME
	MCOMM("PCL STACK NOT EMPTY")
!		merror("PCL stack not empty")
	fi

	genmc(m_procend)
end

!proc px_endprog*(pcl p) =
!! End-of-program marker.
!	unimpl(p)
!end

proc px_istatic*(pcl p) =
! Define idata label (must be followed by correct db etc ops)
	setsegment('I',p.align)
	genmc(m_labelname,mgenmemaddr(p.def))
end

proc px_zstatic*(pcl p) =
! Define zdata label and reserve sufficient space
	psymbol d

	d:=p.def
	setsegment('Z',p.align)
	genmc(m_labelname,mgenmemaddr(d))

	genmc(m_resb, mgenint(p.size))
end

proc px_data*(pcl p) =
! Constant data. For block types, there can be multiple C values
	mclopnd ax
	int opc

	if p.mode=tpblock then
		do_blockdata(p)
		return
	fi

	case p.opndtype
	when int_opnd then
		ax:=mgenint(p.value)
	when realimm_opnd then
		ax:=mgenrealimm(p.xvalue,tpr64)
	when realimm32_opnd then
		ax:=mgenrealimm(p.xvalue,tpr32)
	when r32_opnd then
		ax:=mgenrealimm(p.xvalue, tpr32)

	when string_opnd then
		ax:=mgenlabel(getstringindex(p.svalue, p.slength))

	when memaddr_opnd then
		ax:=mgenmemaddr(p.def)
		ax.offset:=p.extra
!CPL "SETTING OFFSET", AX.OFFSET, MSTROPND(AX)
	when label_opnd then
		ax:=mgenlabel(p.labelno)

	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

	case p.size
	when 1 then opc:=m_db
	when 2 then opc:=m_dw
	when 4 then opc:=m_dd
	when 8 then opc:=m_dq
	else
CPL =P.SIZE, =STRPMODE(P.MODE)
		merror("DATA/not 1248")
	esac
!
	genmc(opc,ax)

end

proc px_label*(pcl p) =
	genmc(m_labelx, mgenlabel(p.labelno))
end

proc px_load*(pcl p) =
! Z' := M &M L &L 123 4.5 "abc"

	pushpcl(p)
end

proc px_store*(pcl p) =
! M := Z
	mclopnd ax, bx
	psymbol d

	bx:=loadopnd(zz, p.mode)

	if p.mode<>tpblock then
		ax:=mgenmem(p.def, p.mode)
		genmc(m_mov, ax, bx)

	else
		ax:=getworkregm(tpu64)

		d:=p.def
		genmc((d.id=param_id|m_mov|m_lea), ax, mgenmem(d, tpu64))
		ax:=makeopndind(ax, tpu64)

		bx:=makeopndind(bx, tpu64)
		copyblock(ax, bx, p.size)
	fi

	poppcl()
end

proc px_add*(pcl p) =
! Z' := Y + Z
	mclopnd ax, bx

	ax:=loadopnd(yy, p.mode)
	if ispint(p.mode) then
		if isimmload(zz) and pclopnd[zz].value=1 then
			genmc(m_inc, ax)
		else
			bx:=getopnd(zz, p.mode)
			genmc(m_add, ax, bx)
		fi
	else
		bx:=getopnd(zz, p.mode)
		genmc(m_addss+ispwide(p.mode), ax, bx)
	fi

	poppcl()
end

proc px_sub*(pcl p) =
! Z' := Y - Z
	mclopnd ax, bx

	ax:=loadopnd(yy, p.mode)
	if ispint(p.mode) then
		if isimmload(zz) and pclopnd[zz].value=1 then
			genmc(m_dec, ax)
		else
			bx:=getopnd(zz, p.mode)
			genmc(m_sub, ax, bx)
		fi
	else
		bx:=getopnd(zz, p.mode)
		genmc(m_subss+ispwide(p.mode), ax, bx)
	fi

	poppcl()
end

proc px_mul*(pcl p) =
! Z' := Y * Z
	mclopnd ax, bx
	int x

	ax:=loadopnd(yy, p.mode)

	if ispint(p.mode) then
		if isimmload(zz) then
			mulimm(ax, pclopnd[zz].value)

		else

!			bx:=getopnd(zz, p.mode)
			bx:=LOADopnd(zz, p.mode)
			genmc(m_imul2, ax, bx)
		fi

	else
		bx:=getopnd(zz, p.mode)
		genmc(m_mulss+ispwide(p.mode), ax, bx)
	fi

	poppcl()
end

proc px_div*(pcl p) =
! Z' := Y / Z
	mclopnd ax, bx

	ax:=loadopnd(yy, pmode)
	bx:=getopnd(zz, pmode)
	genmc(m_divss+ispwide(pmode), ax, bx)
	poppcl()
end

proc px_eval*(pcl p) =
! Evaluate Z [load to an actual register], then pop

	loadopnd(zz, p.mode)
	poppcl()
end

proc px_widen*(pcl p) =
! Z' := cast(Z,t) Mask to width of u, but type is widened to t
	mclopnd ax, bx

	if pmode=tpu64 and p.mode2=tpu32 then
		ax:=loadopnd(zz, tpu32)
		if mccodex.opcode<>m_mov then
			genmc(m_mov, ax, ax)			!sets upper half to zero, just in case
		fi
	else
		bx:=getopnd(zz, p.mode2)
		ax:=getworkregm(pmode)
		genmc((psigned[p.mode2]|m_movsx|m_movzx), ax, bx)
		setnewzz(ax.reg, pmode)
	fi

end

proc px_jump*(pcl p) =
! goto L
	int labno:=p.labelno
	pcl q:=p+1

	while q.opcode=kcomment do ++q od
	case q.opcode
	when klabel then
		if q.labelno=labno then return fi
		++q
		if q.opcode=klabel and q.labelno=labno then return fi
	when kjump then
		q.opcode:=knop
	esac

	genmc(m_jmp, mgenlabel(labno))
end

proc px_ijump*(pcl p)=
	genmc(m_jmp, getopnd(zz, tpu64))
	poppcl()
end

proc px_neg*(pcl p) =
! Z' := -Z
	mclopnd ax

	ax:=loadopnd(zz, pmode)

	if ispint(pmode) then
		genmc(m_neg,ax)
	else
		do_negreal(ax, pmode)
	fi
end

proc px_bitnot*(pcl p) =
! Z' := inot Z
	mclopnd ax
	ax:=loadopnd(zz, pmode)
	genmc(m_notx, ax)
end

proc px_not*(pcl p) =
! Z' := not Z
	mclopnd ax
	ax:=loadopnd(zz, pmode)
	genmc(m_xorx, changeopndsize(ax,1), mgenint(1, tpu8))
end

proc px_toboolt*(pcl p) =
! Z' := istrue Z
	mclopnd ax, bx, cx
	byte pmode2:=p.mode2

	ax:=loadopnd(zz, pmode2)

	if ispfloat(pmode2) then
		bx:=getworkregm(pmode2)
		cx:=getworkregm(tpu8)
		genmc(m_xorps+ispwide(pmode2), bx, bx)
		genmc(m_comiss+ispwide(pmode2), ax, bx)

		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), cx)
		genmc(m_movzx, changeopndsize(cx,4),cx)		!4 works for u32/u64
		setnewzz(cx.reg, pmode)

	else
		genmc(m_test, ax,ax)
		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)
		pclmode[xx]:=pmode
	fi
end

proc px_jumpcc*(pcl p) =
! goto L when Y c Z; p=1: Z':=Y (b=0/1)
	int mcond
	mclopnd ax,bx, lx


	mcond:=ucondcodes[p.condcode]
	lx:=mgenlabel(p.labelno)

	if pmode=tpblock then
MERROR("JUMPCC/BLOCK")
!		addimm(p.size)
!		swapopnds(1,3)
!		domaths(nil, "memcmp*", 3)
!		genmc(m_cmp, mgenreg(r0, tpi32), mgenint(0))
!		genmc_cond(m_jmpcc, mcond, lx)

	else

		ax:=loadopnd(yy, pmode)

		if ispint(pmode) then
			if isimmload(zz) and pclopnd[zz].value=0 and p.condcode in [eq_cc, ne_cc] then
				genmc(m_test, ax, ax)
			else
				bx:=getopnd(zz, pmode)
				if psigned[pmode] then
					mcond:=scondcodes[p.condcode]
				fi
				genmc(m_cmp, ax, bx)
			fi
		else
			bx:=getopnd(zz, pmode)
			genmc(m_comiss+ispwide(pmode), ax, bx)
		fi

		genmc_cond(m_jmpcc, mcond, lx)
		poppcl()

		unless p.popone then
			poppcl()
		end
	fi
end

proc px_jumpt*(pcl p) =
! goto L when Z is true
	do_jumptruefalse(p, nz_cond)
end

proc px_jumpf*(pcl p) =
! goto L when Z is false
	do_jumptruefalse(p,z_cond)
end

proc px_bitand*(pcl p) =
! Z' := Y iand Z
	do_bitwise(p, m_andx)
end

proc px_bitor*(pcl p) =
! Z' := Y ior Z
	do_bitwise(p, m_orx)
end

proc px_bitxor*(pcl p) =
! Z' := Y ixor Z
	do_bitwise(p, m_xorx)
end

proc px_shl*(pcl p) =
! Z' := Y << Z
	do_shift(p, m_shl)
end

proc px_shr*(pcl p) =
! Z' := Y >> Z
	do_shift(p, (psigned[pmode]|m_sar|m_shr))
end

proc px_retproc*(pcl p) =
! Return from proc
	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mgencomment("---")				!injection of entry code goes wrong otherwise
	fi

	do_procentry(p)

	do_procexit()
end

proc px_retfn*(pcl p) =
! Return from func with Z=retval
	mclopnd ax,bx

	if pmode=tpblock then
		bx:=mgenireg(r0)								!r0 points to local block value
		regset[r0]:=1
		ax:=getworkregm(tpref)
		genmc(m_mov, ax, mgenmem(blockretname))
		ax:=mgenireg(ax.reg)
		copyblock(ax, bx, p.size)
		genmc(m_mov, mgenreg(r0, tpu64), mgenmem(blockretname))
	fi

	px_retproc(p)
end

proc px_setcall*(pcl p) =
! ?
	saveopnds()

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	fi

	++ncalldepth

	if p.nargs<=4 then
		callalign[ncalldepth]:=mstackdepth.odd
	else
		callalign[ncalldepth]:=p.nargs.odd ixor mstackdepth.odd
	fi
	callblockret[ncalldepth]:=pmode=tpblock
	callblocksize[ncalldepth]:=p.size

	if callalign[ncalldepth] then
		pushslots(1)
	fi
end

proc px_setarg*(pcl p) =
! Mark Z as n'th argument (counting backwards)
	int n

	n:=p.x+callblockret[ncalldepth]

	callargmode[ncalldepth, n]:=pmode
	callargsize[ncalldepth, n]:=p.size

	if pmode=tpblock then
		pclmode[zz]:=tpu64
	else
		pclmode[zz]:=pmode
	fi

	if n>4 then
		pushopnd(zz, pmode, p.size)
	elsif pmode=tpblock then			!need to record its size
		callargsize[ncalldepth, n]:=p.size
	fi
end

proc px_callp*(pcl p) =
! Call &M with nargs, then pop args; v = varargs
	int nargs, nregargs, slots, isptr:=0, shadow:=0

	int blockret:=callblockret[ncalldepth]

	nargs:=p.nargs+blockret
	nregargs:=min(nargs, 4)

	if p.opcode in [kicallp, kicallf] then
		isptr:=1
	fi

	do_pushlowargs(nregargs, p.nvariadics, isptr)

	slots:=0
	if nargs<=4 then
		if mstackdepth then
			slots+:=4
			pushslots(4)					!shadowspace
			SLOTS+:=CALLALIGN[NCALLDEPTH]
		else
			localshadow:=1
		fi

	else
		slots:=nargs+callalign[ncalldepth]
		pushslots(4)						!shadowspace
	fi

	if isptr then
		genmc(m_call, loadopnd(zz, tpu64))
		poppcl()
	else
		genmc(m_call, mgenmemaddr(p.def))
	fi

	to nregargs-BLOCKRET do
		poppcl()
	od

	if slots then
		popslots(slots)
	fi

	if pmode then
		do_getretvalue(p)
	fi

	--ncalldepth
end

proc px_jumpret*(pcl p) =
! goto L, common return point; deal with any ret value on stack

	if pmode then
		IF NOPERANDS THEN				!ELSE ASSUME ASSEM WAS LAST
			loadparam(zz, pmode, r0)
			poppcl()
		FI
	fi

	px_jump(p)
end

proc px_startmx*(pcl p) =
! -
	saveopnds()
end

proc px_resetmx*(pcl p) =
! -
	movetoreg(r0)

	if p.opcode=kresetmx then
		poppcl()
	fi
end

proc px_incrto*(pcl p) =
! Z^ +:= n
	do_incr(p, m_inc, m_add)
end

proc px_decrto*(pcl p) =
! Z^ -:= n
	do_incr(p, m_dec, m_sub)
end

proc px_incrload*(pcl p) =
! Z' := (Z +:= n)^
	do_incrload(p, m_inc, m_add)
end

proc px_decrload*(pcl p) =
! Z' := (Z -:= n)^
	do_incrload(p, m_dec, m_sub)
end

proc px_loadincr*(pcl p) =
! Z' := Z++^ (difficult to express step)
	do_loadincr(p, m_inc, m_add)
end

proc px_loaddecr*(pcl p) =
! Z' := Z--^
	do_loadincr(p, m_dec, m_sub)
end

proc px_iload*(pcl p) =
! Z' := Z^
	mclopnd ax, px
	pcl nextpcl

	if pmode<>tpblock then

		px:=getopnd_ind(zz, pmode)

		nextpcl:=currpcl+1

		if nextpcl.opcode=kwiden then

			ax:=getworkreg_rm(getsharereg(px, nextpcl.mode), nextpcl.mode)

			genmc(ploadop[nextpcl.mode2], ax, px)
			setnewzz(ax.reg, nextpcl.mode)
			currpcl:=nextpcl
		else
			ax:=getworkreg_rm(getsharereg(px, pmode), pmode)
			genmc(m_mov, ax, px)
			setnewzz(ax.reg, pmode)
		fi

	else

		px:=getopnd_ind_simp(zz, pmode)

		ax:=getworkreg_rm(px.reg, tpu64)
		dolea(ax, px)
	fi

end

func getsharereg(mclopnd ax, int mode)int=
!if ax includes reg/regix, then try and use them
!return 0 if not reg available or not possibe
	byte reg:=ax.reg, regix:=ax.regix

	if ispfloat(mode) then return 0 fi

	if reg and (workregs[reg] or reg in r10..r13) then			!not a regvar
		return reg
	elsif regix and (workregs[regix] or reg in r10..r13) then
		return regix
	fi

	return 0
end

proc px_iloadx*(pcl p) =
! Z' := (Y + Z*s + d)^
	pcl z, nextpcl
	mclopnd ax, bx, px, fx

	px:=do_addrmode(p)

	if pmode=tpblock then
		ax:=getworkreg_rm(px.reg, tpu64)
		dolea(ax, px)
		poppcl()
		setnewzz(ax.reg, tpu64)

	else
		nextpcl:=currpcl+1

		if nextpcl.opcode=kwiden then
			ax:=getworkreg_rm(getsharereg(px, nextpcl.mode), nextpcl.mode)

			genmc(ploadop[nextpcl.mode2], ax, px)
			poppcl()
			setnewzz(ax.reg, nextpcl.mode)
			currpcl:=nextpcl
		else

			ax:=getworkreg_rm(getsharereg(px, pmode), pmode)

			genmc(m_mov, ax, px)
			poppcl()
			setnewzz(ax.reg, pmode)
		fi

	fi
end

proc px_istore*(pcl p) =
! Y^ := Z
	mclopnd bx, px

	bx:=loadopnd(yy, pmode)				!rhs to store into lhs

	px:=getopnd_ind(zz, pmode)

	if pmode=tpblock then
		px:=makesimpleaddr(px)
		bx:=makeopndind(bx, tpu64)

		copyblock(px, bx, p.size)

	else
		genmc(m_mov, px, bx)
	fi

	poppcl()
	poppcl()
end

proc px_istorex*(pcl p) =
! (Y + Z*s + d)^ := X
	mclopnd ax, cx, px
	pcl z

	cx:=loadopnd(xx, pmode)			!rhs
	px:=do_addrmode(p)

	if pmode=tpblock then
		px:=makesimpleaddr(px)
		cx:=makeopndind(cx, tpu64)
		copyblock(px, cx, p.size)

	else
		genmc(m_mov, px, cx)

	fi

	poppcl()
	poppcl()
	poppcl()
end

proc px_storem*(pcl p) =
! Z' := (Y, Z) for mem:16
	mclopnd ax, bx, px
	pcl z
	psymbol dblock

	if p.size<>16 then merror("Storem not 16") fi		!only Y/Z for now

	dblock:=newblocktemp(16)
!
	px:=mgenmem(dblock)
!
	bx:=loadopnd(zz, tpu64)
!
	genmc(m_mov, applyoffset(px, 8), bx)
	poppcl()
!
	bx:=loadopnd(zz, tpu64)
	genmc(m_mov, px, bx)

	genmc(m_lea, mgenreg(bx.reg,tpu64), px)
	setnewzz(bx.reg, tpu64)
end

proc px_addpx*(pcl p) =
! Z' := Y + Z*s + d
	mclopnd ax,cx

	cx:=do_addrmode(p)
	ax:=getworkreg_rm(cx.reg, tpu64)

	dolea(ax, cx)
	poppcl()

	setnewzz(ax.reg, tpu64)
end

proc px_subpx*(pcl p) =
! Z' := Y - Z*s + s
	int scale, extra, offset
	mclopnd ax,bx
	pcl z

	scale:=p.scale
	extra:=p.extra

	ax:=loadopnd(yy, tpu64)

	if z:=isimmload(zz) then
		genmc(m_sub, ax, mgenint(z.value*scale+extra))
	else
		bx:=loadopnd(zz, tpu64)
		scale:=scaleindex(bx, scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
			CPL =EXTRA
			MERROR("SUBREF/EXTRA")
!			genmc(m_add, ax, mgenint(extra))
		fi
	fi
	poppcl()
end

proc px_swapstk*(pcl p) =
! (Z', Y') := (Z, Y)
	swapopnds(noperands-p.x+1, noperands-p.y+1)
end

proc px_labeldef*(pcl p) =
! ?
	[100]char str
	strcpy(str, p.def.name)
	strcat(str, ":")
	MCOMM(str)

end

proc px_addto*(pcl p) =
! Z^ +:= Y
	do_binto(p, m_add, m_addss)
end

proc px_subto*(pcl p) =
! Z^ -:= Y
	do_binto(p, m_sub, m_subss)
end

proc px_multo*(pcl p) =
! Z^ *:= Y
	mclopnd ax,bx,cx
	pcl x

	if ispfloat(pmode) then
		do_binto_float(p, m_mulss)
		return
	fi

	if psize[pmode]=1 then merror("multo/byte") fi

	pushpcl_reg(tpi64)

!operands are now Y^ *:= X with Z used as working value

!xx yy zz = addr rhs workreg
	ax:=getopnd_ind(yy, pmode)
	bx:=getopnd(xx, pmode)
	cx:=getopnd(zz, pmode)

	genmc(m_mov, cx, ax)

	if x:=isimmload(xx) then
		mulimm(cx, x.value)
	else
		genmc(m_imul2, cx,bx)
	fi
	genmc(m_mov, ax,cx)

	poppcl()
	poppcl()
	poppcl()
end

proc px_bitandto*(pcl p) =
! Z^ iand:= Y
	do_binto(p,m_andx, 0)
end

proc px_bitorto*(pcl p) =
! Z^ ior:= Y
	do_binto(p,m_orx, 0)
end

proc px_bitxorto*(pcl p) =
! Z^ ixor:= Y
	do_binto(p,m_xorx, 0)
end

proc px_shlto*(pcl p) =
! Z^ <<:= Y
	do_shiftnto(p,m_shl)
end

proc px_shrto*(pcl p) =
! Z^ >>:= Y
	do_shiftnto(p,(psigned[pmode]|m_sar|m_shr))
end

proc px_fix*(pcl p) =
! Z' := cast(Z,t) Real u to int t
	mclopnd fx,ax
!
	fx:=loadopnd(zz, p.mode2)
	pushpcl_reg(pmode)

	ax:=getopnd(zz, pmin[pmode])
	genmc(m_cvttss2si+ispwide(p.mode2), ax, fx)

	swapopnds(yy,zz)
	poppcl()


	setnewzz(ax.reg, pmode)
end

proc px_float*(pcl p) =
! Z' := cast(Z,t) Int u to real t
	mclopnd ax,fx
	int lab,lab2
	byte pmode2:=p.mode2

	ax:=loadopnd(zz, pmode2)

	if psize[pmode2]<4 then merror("float/short") fi

	if psigned[pmode2] then
		pushpcl_reg(pmode)
		fx:=getopnd(zz, p.mode)

		genmc(m_cvtsi2ss+ispwide(pmode), fx, ax)
		swapopnds(yy,zz)

	elsif pmode2=tpu64 then								!u64 to r32/r64
		pushpcl_reg(tpr64)								!convert to r64 in all cases

		fx:=getopnd(zz, tpr64)

		lab:=mcreatefwdlabel()
		lab2:=mcreatefwdlabel()

		genmc(m_cmp, ax, mgenint(0))					!range of +ve i64?
		genmc_cond(m_jmpcc, lt_cond, mgenlabel(lab))
		genmc(m_cvtsi2sd, fx, ax)						!number is +ve i64
		genmc(m_jmp, mgenlabel(lab2))

		mdefinefwdlabel(lab)
		if not labmask63 then
			labmask63:=++mlabelno
			laboffset64:=++mlabelno
		fi
		genmc(m_andx,ax, mgenlabelmem(labmask63))		!clear top bit of u64 (subtract 2**63)
		genmc(m_cvtsi2sd, fx, ax)						!now in +ve i64 range
		genmc(m_addsd, fx, mgenlabelmem(laboffset64))	!add back 2**63 as float

		mdefinefwdlabel(lab2)							!done conv to r64
reduce:
		if pmode=tpr32 then								!for r64, reduce down
			genmc(m_cvtsd2ss, changeopndsize(fx, 4), fx)
			pclmode[zz]:=tpr32
		fi

		swapopnds(yy,zz)								!bring old int value to top
	else												!u32 to r32/r64
		pushpcl_reg(tpr64)								!convert to r64 in all cases

		fx:=getopnd(zz, tpr64)
		ax:=changeopndsize(ax, 8)						!eg A0 to D0

		genmc(m_cvtsi2sd, fx, ax)						!u64 (always in range) to r64

		goto reduce

	fi

	poppcl()
end

proc px_idiv*(pcl p) =
! Z' := Y % Z
	do_divrem(p, issigned:psigned[pmode], isdiv:1)
end

proc px_irem*(pcl p) =
! Z' := Y rem Z
	do_divrem(p, issigned:psigned[pmode], isdiv:0)
end

proc px_idivrem*(pcl p) =
! Z' := divrem(Y, Z)
	do_divrem(p, issigned:psigned[pmode], isdiv:2)
end

proc px_subp*(pcl p) =
! Z' := (Y - Z)/s
	mclopnd ax,bx
	int n

	ax:=loadopnd(yy, pmode)
	bx:=getopnd(zz, pmode)
	genmc(m_sub,ax,bx)

	if p.scale>1 then
		n:=ispoweroftwo(p.scale)
		if n then
			genmc(m_shr, ax, mgenint(n))
		else
			CPL P.SCALE
			MERROR("SUB/REF NOT POWER OF xx")
		fi
	fi

	poppcl()
end

proc px_switch*(pcl p) =
! L=jumptab; B=elselab; x/y=min/max values
	int minlab, maxlab, jumplab, elselab, reg
	mclopnd ax, bx, ax2

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=loadopnd(zz, pmode)
	if p.size<8 then
		genmc(m_movsx, ax2:=changeopndsize(ax,8), ax)
		ax:=ax2
	fi

	if minlab<>0 then
		genmc(m_sub, ax, mgenint(minlab))
	fi

	genmc(m_cmp,ax,mgenint(maxlab-minlab+1))
	genmc_cond(m_jmpcc,geu_cond,mgenlabel(elselab))

	if phighmem=2 then
		reg:=getworkireg()
		bx:=mgenreg(reg, tpref)

		genmc(m_lea, bx, mgenlabelmem(jumplab))

		genmc(m_jmp, mgenindex(ireg:ax.reg, areg:reg, scale:8))
	else
		genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))

	fi

	poppcl()
end

proc px_swlabel*(pcl p) =
! jumptable entry
	genmc(m_dq, mgenlabel(p.labelno))
end

proc px_endsw*(pcl p) =
! Mark end of switch jumptable
!	setsegment('C')
end

proc px_fwiden*(pcl p) =
! Z' := cast(Z,t) r32 to r64
	mclopnd fx
	fx:=loadopnd(zz,p.mode2)
	genmc(m_cvtss2sd, changeopndsize(fx,8), fx)
	pclmode[zz]:=tpr64
end

proc px_fnarrow*(pcl p) =
! Z' := cast(Z,t) r64 to r32
	mclopnd fx
	fx:=loadopnd(zz,p.mode2)
	genmc(m_cvtsd2ss, changeopndsize(fx,4), fx)
	pclmode[zz]:=tpr32
end

proc px_truncate*(pcl p) =
! Z' := cast(Z,u) Mask to width of u, but type is widened to t
	mclopnd ax
	byte pmode2:=p.mode2

!	if p.size<8 then merror("trunc32") FI

	ax:=loadopnd(zz, pmode2)
	if p.size<>psize[pmode2] then
		genmc(ploadop[pmode2], changeopndsize(ax, psize[pmode]), ax)
	fi
end

proc px_unload*(pcl p) =
! Pop stack
	poppcl()
end

proc px_setcc*(pcl p) =
! Z' := Y cc Z
	int cond
	mclopnd ax,bx,cx

	ax:=loadopnd(yy, pmode)
	bx:=getopnd(zz, pmode)
	cond:=ucondcodes[p.condcode]

	if pmode=tpblock then
		merror("setcc/block")

	elsif ispint(pmode) then
		if psigned[pmode] then
			cond:=scondcodes[p.condcode]
		fi
		genmc(m_cmp,ax,bx)
		cx:=changeopndsize(ax,1)

	else
		genmc(m_comiss+ispwide(pmode),ax,bx)

		cx:=getworkregm(tpu8)
		setnewzz(cx.reg, tpi64)
		swapopnds(yy,zz)
	fi

	genmc_cond(m_setcc, cond, cx)
	genmc(m_movzx, changeopndsize(cx,4), cx)

	poppcl()
end

proc px_addpxto*(pcl p) =
! Z^ +:= Y
	mclopnd ax,bx
	pcl z
!
	ax:=getopnd_ind(zz, pmode)

	if z:=isimmload(yy) then
		genmc(m_add, ax, mgenint(z.value*p.scale))
	else
		bx:=loadopnd(yy, pmode)
		mulimm(bx, p.scale)
		genmc(m_add, ax, bx)
	fi

	poppcl()
	poppcl()
end

proc px_subpxto*(pcl p) =
! Z^ -:= Y
	mclopnd ax, bx
	pcl z

	ax:=getopnd_ind(zz, pmode)

	if z:=isimmload(yy) then
!		genmc(m_sub, ax, mgenint(z.value*p.scale+p.extra))
		genmc(m_sub, ax, mgenint(z.value*p.scale))
	else
		bx:=loadopnd(yy, pmode)
		mulimm(bx, p.scale)
		genmc(m_sub, ax, bx)
		if p.extra then
			MERROR("SUBTOREF/EXTRA")
!			genmc(m_sub, ax, mgenint(extra))
		fi
	fi

	poppcl()
	poppcl()
end

proc px_divto*(pcl p) =
! Z^ /:= Y
	do_binto_float(p, m_divss)
end

proc px_setjmp*(pcl p)=
	mclopnd ax,bx
	int lab:=mcreatefwdlabel()

	bx:=getopnd_ind(zz, tpref)

	pushpcl_reg(tpref)

	ax:=getopnd(zz, tpref)
	genmc(m_mov, ax, mgenlabel(lab))
	genmc(m_mov, bx, ax)
	genmc(m_mov, applyoffset(bx,8), dstackopnd)
	genmc(m_mov, applyoffset(bx,16), dframeopnd)
	swapopnds(yy,zz)
	poppcl()
	clearreg(ax)

!since this is the end of this op anway, free any workregs in advance (freeing
!will be done again by convertpcl)
	freeworkregs(p)
	movetoreg(r0)
	mdefinefwdlabel(lab)

end

proc px_longjmp*(pcl p)=
	mclopnd ax,bx,cx

	bx:=loadopnd(zz, tpref)		!ret value
	ax:=getopnd_ind(yy, tpref)	!buffer

	genmc(m_mov, dstackopnd, applyoffset(ax,8))
	genmc(m_mov, dframeopnd, applyoffset(ax,16))

!	addreg_d64()
	pushpcl_reg(tpref)

	cx:=getopnd(zz, tpref)

	genmc(m_mov, cx, ax)		!load stored return address
	swapopnds(xx, zz)
	poppcl()					!addr of buffer

	swapopndregs(r0)			!move ret value to r0
	genmc(m_jmp, cx)			!
	swapopnds(yy, zz)
	poppcl()					!get rid of dest addr; leave ret value in r0
end
=== mc_libmcl.m 0 0 22/24 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

export const ctarget=0

!global int mclseqno
EXPORT int mclseqno
EXPORT int NMCLOPND

[-1..10]mclopnd smallinttable

global macro isframex(d) = (d.id in [local_id, param_id])

global macro mcomm = mgencomment

export proc mclinit=
	mclopnd a
	int r,s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") fi

	for r:=r0 to r15 do
		regtable[r,1]:=mgenreg0(r,1)
		regtable[r,2]:=mgenreg0(r,2)
		regtable[r,4]:=mgenreg0(r,4)
		regtable[r,8]:=mgenreg0(r,8)
	od

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=mgenreg(rframe, tpu64)
	dstackopnd:=mgenreg(rstack, tpu64)

	initmcdest()

	setsegment('C')

	for i in smallinttable.bounds do
		smallinttable[i]:=mgenint0(i,8)
	od

end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
!	clear rtsproclabels
end

EXPORT proc genmc(int opcode, mclopnd a=nil,b=nil)=		!used in do_mcl/assem in host
	ref mclrec m, oldm
	int labno

	m:=pcm_allocnfz(mclrec.bytes)

	m.opcode:=opcode
	m.seqno:=++mclseqno
	m.mpos:=mmpos

	m.a:=a
	m.b:=b

	case opcode
	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_labelx then
		labno:=a.labelno

	when m_mov then				!change to movd/q if needed
		if a.mode=a_xreg or (b and b.mode=a_xreg) then
			m.opcode:=(a.size=8|m_movq|m_movd)
		fi
	esac

	if mccode then
		m.lastmcl:=mccodex
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

export proc genmc_cond(int opcode, cond, mclopnd a=nil,b=nil)=
	genmc(opcode,a,b)
	mccodex.cond:=cond
end

global proc genmc_str(int opcode,ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode,mgenstring(s))
end

func newmclopnd:mclopnd a=
!	a:=pcm_allocz(mclopndrec.bytes)
	a:=pcm_allocnfz(mclopndrec.bytes)

++NMCLOPND
	return a
end

global func duplopnd(mclopnd a)mclopnd=
	mclopnd b
!	b:=pcm_alloc(mclopndrec.bytes)
	b:=pcm_allocnfz(mclopndrec.bytes)
	b^:=a^
	return b
end

EXPORT func mgenindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, psymbol def=nil)mclopnd=
!construct a mem address mode
	mclopnd a
	a:=newmclopnd()

	a.mode:=a_mem
	a.reg:=areg

	if areg=rframe or ireg=rframe then usedregs[rframe]:=1 fi

	a.regix:=ireg
	a.scale:=scale
	a.size:=size

	a.offset:=offset

	if labno then
		a.value:=labno
		a.valtype:=label_val
	elsif def then
		a.def:=def
		a.valtype:=def_val
		if isframex(def) then
			a.reg:=rframe
			usedregs[rframe]:=1
		fi
	fi

	return a
end

global proc mgencomment(ichar s)=
	genmc_str(m_comment,s)
end

export func mgenstring(ichar s,int length=-1)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm
	if length<0 then
		length:=strlen(s)
	fi
	a.svalue:=pcm_alloc(length+1)
	memcpy(a.svalue,s,length)
	(a.svalue+length)^:=0

	a.valtype:=stringimm_val
	a.size:=8
	return a
end

global func mgenname(ichar s)mclopnd=
	[64]char str
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstring(s)
	a.valtype:=name_val
	a.size:=8

	return a
end

global proc setsegment(int seg,align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
	int opc,oldalign

	if seg<>currsegment then
		case seg
		when 'I' then opc:=m_isegment
		when 'Z' then opc:=m_zsegment
		when 'C' then opc:=m_csegment
		when 'R' then MERROR("CAN'T DO RODATA SEG")
		else
			MERROR("BAD SEG CODE")
		esac
		if mccodex and mccodex.opcode in [m_isegment,m_zsegment,m_csegment] then
			mccodex.opcode:=opc
		else
			genmc(opc)
		fi

		currsegment:=seg
	fi

	if align>1 then
		if mccodex.opcode=m_align then
			oldalign:=mccodex.a.value
			if oldalign>=align then return fi
		fi
		genmc(m_align,mgenint(align))
	fi
end

global func changeopndsize(mclopnd a,int size)mclopnd=
	mclopnd b

	if a.size<>size then
		if a.mode=a_reg then
			b:=regtable[a.reg, size]
		else
			b:=duplopnd(a)
			b.size:=size
		fi
		return b
	fi
	return a
end

global func applyoffset(mclopnd a,int offset,int size=0)mclopnd=
!astr is an asm operand
!add possible byte offset
	mclopnd b

	if offset=0 and size=0 then
		return a
	fi
	b:=duplopnd(a)
	b.offset+:=offset
	if size then
		b.size:=size
	fi

	return b
end

export func mgenint(i64 x,int mode=tpi64)mclopnd a=
	int size:=psize[mode]

	if x in -1..10 and size=8 then
		return smallinttable[x]
	fi

	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global func mgenint0(i64 x,int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global func mgenrealmem(r64 x,int mode=tpr64)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_mem
	if ispwide(mode) then
		a.value:=getrealindex(x)
	else
		a.value:=getr32index(x)
	fi
	a.valtype:=label_val
	a.size:=psize[mode]
	return a
end

export func mgenrealimm(r64 x,int mode=tpr64)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=realimm_val
	a.size:=psize[mode]
	return a
end

EXPORT func mgenlabel(int x=0)mclopnd a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm

	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val

	return a
end

global func mgenlabelmem(int x)mclopnd a=
!x is a label index
!generate immediate operand containing label

	a:=mgenlabel(x)
	a.mode:=a_mem
	return a
end

export func mgenmem(psymbol d, int mode=tpu64)mclopnd a=
	int reg

	reg:=rnone
	if isframex(d) then
		reg:=rframe
		usedregs[rframe]:=1

	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	a.valtype:=def_val

	if mode then
		a.size:=psize[mode]
	else
		a.size:=min(d.size,8)
	fi

	return a
end

EXPORT func mgenmemaddr(psymbol d)mclopnd=
	mclopnd a

	d.addrof:=1

	a:=newmclopnd()
	a.mode:=a_imm

	a.def:=d
	a.valtype:=def_val
	a.size:=8

	return a
end

global func mgenreg0(int reg,size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size

IF SIZE=0 THEN MERROR("1:SIZE=0") FI
	return a
end

EXPORT func mgenxreg(int xreg, size=8)mclopnd=
	mclopnd a

	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
IF SIZE=0 THEN MERROR("2:SIZE=0") FI
	return a
end

EXPORT func mgenreg(int reg, mode=tpi64)mclopnd a =
	int size:=psize[mode]

	if ispfloat(mode) then
		a:=newmclopnd()
		a.mode:=a_xreg
		a.reg:=reg
		usedxregs[reg]:=1
		a.size:=psize[mode]
		a
	else
		if size=0 then size:=8 fi
		usedregs[reg]:=1

IF REG IN R10..R13 THEN REGSET[REG]:=1 FI

		if fuseregtable then
			return regtable[reg,size]
		fi
		return mgenreg0(reg,size)
	fi
end

!global func mgenregi(int reg, mode=tpi64)mclopnd a =
!	if fuseregtable then
!		return regtable[reg, psize[mode]]
!	fi
!	return mgenreg0(reg, psize[mode])
!end
!
global func mgenireg(int reg, mode=tpi64, offset=0)mclopnd=
	mclopnd a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=psize[mode]
	a.offset:=offset

	return a
end

global func mgentemp(int n, mode)mclopnd a=
!pcl temps are used to spill pcl operands from a register
!they will always be 64 bits

	int reg, size

	if pcltempflags[n] then			!already in use
		return changeopndsize(pcltempopnds[n], psize[mode])
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=rframe
	usedregs[rframe]:=1
	a.valtype:=temp_val
	a.size:=psize[mode]
	a.tempno:=n

	pcltempopnds[n]:=a
	pcltempflags[n]:=1

	return a
end

global func roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	if size iand 7=0 then return size fi
	return size+(8-(size iand 7))
end

!global proc merroropnd(ichar mess,int opndtype)=
!	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype]
!	PRINTLN
!	STOP 1
!!	stopcompiler(sourcefilepaths[mmpos>>24],mmpos iand 16777215)
!end
!
global func mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_labelx,mgenlabel(lab))
end

!global func mgenextname(ichar s)mclopnd=
!	[64]char str
!	psymbol d
!	static [20]psymbol table
!	static int ntable
!
!	strcpy(str,s)
!	str[strlen(s)]:=0			!lose final *
!
!	d:=findnamesym(str)
!
!	if not d then
!		d:=pcm_allocnfz(pstrec.bytes)
!
!		d.name:=pcm_copyheapstring(str)
!		d.id:=import_id
!		d.imported:=1
!		addnamesym(d)
!	fi
!
!	return mgenmemaddr(d)
!end

global proc pushslots(int nslots)=
	pushstack(nslots*8)
	mstackdepth+:=nslots
end

global proc popslots(int nslots)=
	popstack(nslots*8)
	mstackdepth-:=nslots
end

global proc pushstack(int n)=
	if n then
		genmc(m_sub,dstackopnd,mgenint(n))
	fi
end

global proc popstack(int n)=
	if n then
		genmc(m_add,dstackopnd,mgenint(n))
	fi
end

global func getstringindex(ichar s, int length)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if cstringlist and length=cstringlist.slength and eqbytes(cstringlist.svalue, s, length) then
		return cstringlist.labelno
	fi

	return addconst(cstringlist, cast(s), length)
end

global func addconst(ref constrec &clist, int value=0, length=0)int=
	ref constrec p
	p:=pcm_allocnfz(constrec.bytes)
	p.value:=value
	p.slength:=length
	p.labelno:=++mlabelno
	p.nextconst:=clist
	clist:=p
	return mlabelno
end

global func getrealindex(real x)int=
	return addconst(creallist,cast@(x,int))
end

global func getr32index(real x)int=
	return addconst(cr32list,cast@(x,int))
end

!global func ispoweroftwo(i64 x)int=
EXPORT func ispoweroftwo(i64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	i64 a
	int n

	a:=1
	n:=0
	to 60 do
		++n
		a:=a<<1
		if a=x then
			return n
		fi
	od
	return 0
end

global func newblocktemp(int size)psymbol=
	[16]char str
	psymbol d

	if nblocktemps>maxblocktemps then
		merror("Too many block temps")
	fi
	++nblocktemps

	fprint @str,"$B#",nblocktemps
	d:=pc_makesymbol(str, misc_id)
	d.mode:=tpblock
	d.size:=size
	d.used:=1
	d.id:=local_id
	d.nextlocal:=currfunc.nextlocal
 	d.owner:=currfunc
	currfunc.nextlocal:=d

	blockdefs[nblocktemps]:=d
	d
end

global proc clearreg(mclopnd ax)=
	if ax.size=8 then
		ax:=changeopndsize(ax,4)
	fi
	genmc(m_xorx, ax, ax)
end
=== mc_stackmcl.m 0 0 23/24 ===
!'PCS' Support - PCL Operand Stack 

global func getopnd(int n, mode, reg=rnone)mclopnd ax =
!get access mode for operand n
	mclopnd bx
	pcl a
	psymbol d

	a:=pclopnd[n]

	case pclloc[n]
	when reg_loc then
		return mgenreg(pclreg[n], mode)

	when temp_loc then
		bx:=mgentemp(n, mode)
		return bx
	esac

	case a.opndtype
	when mem_opnd then
		d:=a.def
		if mode=tpblock and d.id<>param_id then
			mode:=tpu64
			recase memaddr_opnd
		else
			ax:=mgenmem(a.def, mode)
		fi

	when memaddr_opnd then
		d:=a.def
		if d.id=param_id and d.mode=tpblock then		!pcl mode will be u64
			ax:=mgenmem(a.def, mode)
		else
			ax:=getworkreg_rm(reg, mode)
			genmc(m_lea, ax, mgenmem(a.def, mode))
		fi

	when int_opnd then
		CASE PSIZE[MODE]
		WHEN 2 THEN
			A.VALUE IAND:=0xFFFF
		WHEN 4 THEN
			A.VALUE IAND:=0xFFFF'FFFF
		ESAC

		bx:=mgenint(a.value, mode)
		if a.value in i32.bounds then			!keep as immediate
			ax:=bx
		else
			ax:=getworkreg_rm(reg, mode)
			genmc(m_mov, ax, bx)
		fi

	when real_opnd, r32_opnd then
		ax:=mgenrealmem(a.xvalue, mode)

	when string_opnd then
		ax:=getworkreg_rm(reg, mode)

		genmc(m_lea, ax, mgenlabelmem(getstringindex(a.svalue, a.slength)))

	when label_opnd then
		ax:=getworkreg_rm(reg, mode)

		genmc(m_lea, ax, mgenlabelmem(a.labelno))

	else
error:
		merror("getopnd", opndnames[a.opndtype])
	esac

	ax
end

global func loadopnd(int n, mode, reg = rnone)mclopnd ax =
!Load operand to designated register reg. If not provided, one is allocated
!If operand resides in a register already, and reg=0, then that is what is
!returned. But if it will be modified, either REG is needed, or an alternate
!scheme is needed to force a load to a different register

	ax:=getopnd(n, mode, reg)

	ax:=loadtoreg(ax, mode, reg)

	pclopnd[n]:=nil
	pclloc[n]:=reg_loc
	pclreg[n]:=ax.reg

	ax
end

global func loadparam(int n, mode, reg)mclopnd ax =
!Load operand to designated arg reg.
!If operand resides in a register already, and reg=0, then that is what is
!returned. But if it will be modified, either REG is needed, or an alternate
!scheme is needed to force a load to a different register
	ax:=getopnd(n, mode, reg)
	ax:=loadtoreg_m(ax, mode, reg)
	ax
end

global proc pushopnd(int n, mode, size)=
!Push a to hardware stack then pop it from pcl stack
!The hardware stack is popped on return from a call

	mclopnd ax, bx
	pcl p:=pclopnd[n]			!in case it is mem/int etc

!First look for operands that can be directly pushed without using a register

	if pclloc[n]=pcl_loc then	!p refers to operand
		case p.opndtype
		when mem_opnd then
			if size=8 then
				ax:=mgenmem(p.def, pmode)
				pushit
			fi
		when int_opnd then
			if p.value in i32.bounds then		!fits in d32 offset
				ax:=mgenint(p.value, tpi64)
				pushit
			fi

		when real_opnd then
			ax:=mgenrealmem(p.xvalue, tpr64)
			pushit

		esac

	fi

!need to go via register

	ax:=loadopnd(n, mode)

	if mode=tpblock then
		copyblockarg(ax, size, n)
		mode:=tpu64				!push the pointer
	fi


	if ax.mode=a_xreg then			!float register
		bx:=ax
		ax:=getworkregm((mode=4|tpu32|tpu64))
		genmc(m_mov, ax, bx)

	fi

pushit:
	genmc(m_push, changeopndsize(ax,8))

	poppcl()
	++mstackdepth

end

global func loadtoreg(mclopnd ax, int mode, reg)mclopnd=
!if ax is not a register operand, then load it to given register
!mode is needed to give type of register (float etc) and size
!It is assumed that if ax /is/ in a register, that it is the correct one, or doesn't matter
	mclopnd bx

	if ax.mode in [a_reg, a_xreg] then			!might already be in reg
		if not reg or ax.reg=reg then
			return ax
		fi
	fi

	bx:=getworkreg_rm(reg, mode)

	loadtoreg_common(bx, ax)

	bx
end

global func loadtoreg_m(mclopnd ax, int mode, reg)mclopnd=
!same as loadtoreg but if already in a register, will move it to required one if needed
	mclopnd bx

	if ax.mode in [a_reg, a_xreg] then			!already in register
		if ax.reg=reg then return ax fi			!in correct one
	fi

!need loading/moving to given reg
	bx:=mgenreg(reg, mode)

	loadtoreg_common(bx, ax)
!	genmc(m_mov, bx, ax)
	bx
end

proc loadtoreg_common(mclopnd bx, ax)=
	if ax.mode=a_imm and ax.valtype=intimm_val and ax.value=0 then
		bx:=changeopndsize(bx,4)
		clearreg(bx)
	
	else
		genmc(m_mov, bx, ax)
	fi

end

global proc pushpcl(pcl p)=
!Push a inline operand from pcl code to pcs
	int n

	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi

	n:=++noperands

	pclloc[n]:=pcl_loc

	pclopnd[n]:=p
	pclreg[n]:=0
	pclcount[n]:=1
	pclmode[n]:=p.mode
end

global proc pushpcl_reg(int mode, reg=rnone)=
!Push a new, empty pcs slot located in given register
	int n

	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi

	if reg=rnone then reg:=getworkreg(mode) fi

	n:=++noperands

	pclloc[n]:=reg_loc
	pclopnd[n]:=nil
	pclreg[n]:=reg
	pclcount[n]:=1
	pclmode[n]:=mode

	if ispfloat(mode) then
		xregset[reg]:=1
	else
		regset[reg]:=1
	fi

end

global proc poppcl=
	int n:=noperands

	if n<=0 then merror("poppcl/underflow") fi

	if pclcount[n]>1 then
		--pclcount[n]
		return
	fi

	--noperands
end

global proc duplpcl=
!ensure zz is in a register, duplicate into a new register
	int mode:=pclmode[zz]

	loadopnd(zz, mode)							!get zz to reg
	pushpcl_reg(mode)							!create new zz opnd, old is now yy

	genmc(m_mov, getopnd(zz, mode), getopnd(yy, mode))	!copy old to new
end

global func getworkireg:int r=

	to 10 do
		for r in r0..r13 do
			if workregs[r] and regset[r]=0 then
				regset[r]:=1
				return r
			fi
		od
		savenextopnd()
	od
	merror("No more work regs")
	0
end

global func getworkxreg:int=
	for r in r4..r15 do
		if workxregs[r] and xregset[r]=0 then
			xregset[r]:=1
			return r
		fi
	od
	merror("No more work xregs")
	0
end

global func getworkregm(int mode)mclopnd=
!return mcl opnd for a work reg
	return mgenreg(getworkreg(mode), mode)
end

global func getworkreg(int mode)int reg=
!return new work reg depending on mode
	if ispfloat(mode) then
		getworkxreg()
	else
		getworkireg()
	fi
end

global func getworkreg_rm(int reg, mode)mclopnd=
!return an mcl operand for a specific reg if provided, or
!it will allocate a work reg is not (ie. reg=rnone)

	if reg in [rnone, rframe] then
		return getworkregm(mode)
	fi

	mgenreg(reg, mode)
end

global proc saveopnd(int n, allregs=1)=
!if operand is in a volatile register, then save it in a temp
!allregs=1 to save both A and B regs (vol/nonval), which can include P regs if
!used as workregs; this is to save pne reg=opnd to a temp to free up a register
!allregs=0 to limit to A regs (possibly some P regs) only; normall for CALLs
!in order to preserve non-vol regs, as call will preserve the rest

!NOTE: operands that are unlikely to be unchanged from their value in
!pclrec, could be revert to pcl_loc. Eg. immediates, addresses, or any
!variable that is immutable

	int reg, mode
	mclopnd tx

	return unless pclloc[n]=reg_loc

	reg:=pclreg[n]
	mode:=pclmode[n]

	if ispint(mode) then
		if allregs or reg not in r3..r9 then
			genmc(m_mov, mgentemp(n,mode), mgenreg(reg,mode))
		fi
		regset[reg]:=0

	else
		if allregs or reg in r0..r5 then
			genmc(m_mov, mgentemp(n, mode), mgenxreg(reg, mode))
		fi
		xregset[reg]:=0
	fi

	pclloc[n]:=temp_loc
	pclreg[n]:=0

end
!
global proc saveopnds(int n=0)=
!save all operands other than top n
!assume this is to do with calls
	for i:=1 to noperands-n do
		saveopnd(i,0)
	od
end

global proc savenextopnd=

!starting from the first loaded, look for and save first reg-based opnd
!this does A/B/P regs if used
	for i:=1 to noperands do
		if pclloc[i]=reg_loc and ispint(pclmode[i]) then
			saveopnd(i,1)
			return
		fi
	od
end

!global proc savenextxopnd=
!!as savenextopnd but only does AX/BX/PX regs 
!	for i:=1 to noperands do
!		if pclloc[i]=reg_loc and ispfloat(pclmode[i]) then
!			saveopnd(i,1)
!			return
!		fi
!	od
!end

global proc movetoreg(int newreg)=
!move top of stack (assumed to be in reg) to newreg
!assumes integer reg
	int oldreg
	int mode:=pclmode[zz]

	loadopnd(zz, mode)

retry:

	oldreg:=pclreg[zz]

	if oldreg=newreg then
		return
	fi

	if ispfloat(mode) then
		if xregset[newreg] then
			MERROR("MOVE TO REG: XREG IN USE")
		fi
	else
		if regset[newreg] then
			for i to noperands do
				if ispint(mode) and pclreg[i]=newreg then
					swapopnds(i,zz)
					genmc(m_xchg, mgenreg(oldreg, tpu64), mgenreg(newreg,tpu64))
					retry
				fi
			od
		fi
	fi

	genmc(m_mov, mgenreg(newreg,mode), mgenreg(oldreg,mode))

	pclreg[zz]:=newreg

	if ispfloat(mode) then
		xregset[newreg]:=1
	else
		regset[newreg]:=1
	fi
end

global func getopnd_ind(int n=noperands, mode=tpi64)mclopnd=
!Get access mode to operand which is to be used as a pointer elsewhere
!So it needs first to in a register, if not already
	pcl a
	psymbol d

	if pclloc[n]=pcl_loc then
		a:=pclopnd[n]
		if a.opndtype=memaddr_opnd then
			d:=a.def
			unless d.id=param_id and d.mode=tpblock then
				return mgenmem(a.def, mode)
			end
		fi
	fi

	if pclloc[n]<>reg_loc then
		loadopnd(n, tpu64)
	fi

	return mgenireg(pclreg[n], mode)
end

global func getopnd_ind_simp(int n=noperands, mode=tpi64)mclopnd=
!version of getopnd_ind which always returns [reg]

	if pclloc[n]<>reg_loc then
		loadopnd(n, tpu64)
	fi

	return mgenireg(pclreg[n], mode)
end

global proc swapopnds(int m,n)=
!exchange pcl stack operands
	swap(pclopnd[m],	pclopnd[n])
	swap(pclloc[m],		pclloc[n])
	swap(pclreg[m],		pclreg[n])
	swap(pclmode[m],	pclmode[n])
	swap(pclcount[m],	pclcount[n])
end

global func isimmload(int n)pcl p=
!return nil if operand is not immediate integer
!otherwise return the pcl operand

	p:=pclopnd[n]
	if pclloc[n]=pcl_loc and p.opcode=kload and p.opndtype=int_opnd then p else nil fi
end

global proc setnewzz(int reg, mode)=
!some value has been into into given register
!create replace pcl[zz] with that new operand
!assume pclcount was 1 and stays at 1

	pclloc[zz]:=reg_loc
	pclopnd[zz]:=nil
	pclreg[zz]:=reg
	pclmode[zz]:=mode

end

global proc freeworkregs(pcl p)=
	int reg

!Clear everything first

!(Code is a copy of that used inline in convertpcl)
	clear regset
	clear xregset

!Then set the regs still in use as pcl opnds:

	for i to noperands do
		reg:=pclreg[i]
		if pclreg[i] then
			if ispfloat(pclmode[i]) then
				xregset[reg]:=1
			else
				regset[reg]:=1
			fi
		fi
	od

end

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2
!Note that swapondregs is best assumed to invalidate all existing mclopnds that
!refer to registers, as stuff if moved aound
!Also invalided are workregs that might use reg2, even if no mclopnd exists for it

	if not ispint(pclmode[zz]) then merror("SOR1") fi

!assume int regs

	int reg1:=pclreg[zz]

	if reg1=reg2 then return fi

	for i:=noperands-1 downto 1 do
		if pclloc[i]=reg_loc and pclreg[i]=reg2 then
			swap(pclreg[zz], pclreg[i])
			return
		fi
	else
!pcl op not found that occupies reg2, so it is assumed to be a work register
!that is no longer needed. If it /is/ needed

		regset[reg1]:=0				!make available (although done for next pcl op anyway)
		pclreg[zz]:=reg2
!		merror("swapopndregs/reg not found")
	od
end

global func makeopndind(mclopnd a, int mode=tpvoid)mclopnd=
	mclopnd b

	if a.mode<>a_reg then
		merror("makeopndind")
	fi

	return mgenireg(a.reg, mode)
end

global func makesimpleaddr(mclopnd ax)mclopnd bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg, reg, regix

	reg:=ax.reg
	regix:=ax.regix
	if reg=rframe then reg:=rnone fi

	if ax.mode<>a_mem then merror("MSA") fi

	if reg=rnone and regix=rnone then
		newreg:=getworkireg()
	elsif reg then				![reg] only; already simple
		return ax
	elsif regix then			![regix] only; may be scaled; use lea anyway
		newreg:=regix
	else						![reg+regix]
		newreg:=regix
	fi

	bx:=mgenireg(newreg)

	genmc(m_lea, mgenreg(newreg), ax)
	return bx
end

=== mc_writegas.m 0 0 24/24 ===
const fasmformat=2			!gas

export ichar asmext="asm"

[8, r0..r15]ichar nregnames

byte currseg

global func getassemstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d,e
	ref mclrec m
	[32]char str2,str3
	int i

	gs_init(pdest)

	asmstr("    .code64\n")
	asmstr("    .intel_syntax prefix\n")
	asmstr("\n")

!do globals and externs
	d:=psymboltable

	while d, d:=d.next do
		if d.exported then
			asmstr("    .global ")
			asmstr(getbasename(d.name))
			asmstr("\n")
		fi
	od
	asmstr("\n")
	m:=mccode
	i:=1
	while m do
		writemcl(i,m)
		++i
		m:=m.nextmcl
	od

	byte first:=1
	d:=psymboltable

	while d, d:=d.next do
		if d.dllexport then
			if first then
				first:=0
				asmstr("    .section .drectve\n")
			fi
			asmstr("    .ascii "" -export:\\""")
			asmstr(d.name)
			asmstr("\\""""\n")
		fi
	od
	asmstr("\n")

	return pdest
end

proc writemcl(int index,ref mclrec mcl)=
	strmcl(mcl)
	gs_line(pdest)
end

proc start=
	assemtype:='GAS'

!initialise table of official gp reg names

	for i in 1..8 when i in [1,2,4,8] do
		for r in r0..r15 do
			for k in dregnames.bounds do
				if regsizes[k]=i and regindices[k]=r then
					nregnames[i, r]:=dregnames[k]
				fi
			od
		od
	od
end


global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mclopnd a,b
	int opcode,cond,sizepref
	ichar s,comment
	psymbol d

	opcode:=mcl.opcode


	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
	comment:=nil

	case opcode
	when m_procstart then
		asmstr("# Proc ")
		asmstr(a.def.name)
		currasmproc:=a.def

		return

	when m_procend then
		asmstr("# End ")
		currasmproc:=nil

		return

	when m_comment then
		asmchar('# ')
		asmstr(a.svalue)
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		d:=a.def
		case a.valtype
		when def_val then
			asmstr(getdispname(d))
		when stringimm_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")

		if d.exported then
			if eqstring(getbasename(d.name), d.name) then
!				asmstr(":")
			else
				asmstr("\n")
				asmstr(getbasename(d.name))
!				asmstr("::")
				asmstr(":")
			fi
		fi

		return

	when m_labelx then
		if a.valtype=label_val then
			fprint @str,"L#:",a.value
		else
			recase m_labelname
		fi
		asmstr(str)
		return

	when m_define then
		asmstr("    .set ")
		asmstr(a.svalue)
		asmstr(", ")
		asmopnd(b)
		return

	when m_csegment then asmstr("    .text"); currseg:=code_seg; return
	when m_isegment then asmstr("    .data"); currseg:=idata_seg; return
	when m_zsegment then asmstr("    .bss"); currseg:=zdata_seg; return

	esac

	case opcode
	when m_jmpcc then
		print @opcname,"j",,asmcondnames[cond]

	when m_setcc then
		print @opcname,"set",,asmcondnames[cond]

	when m_cmovcc then
		print @opcname,"cmov",,asmcondnames[cond]

	when m_andx then
		strcpy(opcname,"and")
	when m_orx then
		strcpy(opcname,"or")
	when m_xorx then
		strcpy(opcname,"xor")
	when m_notx then
		strcpy(opcname,"not")

	when m_imul2 then
		strcpy(opcname,"imul")

	when m_movzx then
		if a.size=8 and b.size=4 then
			mcl.a:=a:=changeopndsize(a, 4)
			opcode:=m_mov
		fi
		recase else

	when m_movsx then
		if a.size=8 and b.size=4 then
			strcpy(opcname, "movsxd")
		else
			recase else
		fi

	when m_movd then
		if a.mode=a_xreg and b.mode=a_xreg then		!
			opcode:=m_movq
		fi

		recase else

    when m_mov then
        if a.mode=a_reg and b.mode=a_imm and b.valtype=intimm_val and b.value not in i32.bounds then
            mcl.opcode:=m_movq
        fi
		recase else

	when m_align then
		strcpy(opcname, ".align")
!		if currseg=zdata_seg then
!			strcpy(opcname, "alignb")
!		else
!			recase else
!		fi
	when m_resb then
		strcpy(opcname, ".space")

	when m_db then
		strcpy(opcname, ".byte")
	when m_dw then
		strcpy(opcname, ".word")
	when m_dd then
		strcpy(opcname, ".long")
	when m_dq then
		strcpy(opcname, ".quad")
	when m_ascii then
		strcpy(opcname, ".ascii")


	when m_endx then
		return

	ELSIF OPCODE>M_HALT THEN
		STRCPY(OPCNAME,STRINT(OPCODE))

	else
		strcpy(opcname,mclnames[opcode]+2)
	esac

	ipadstr(opcname,10," ")

	if not fasmformat then
		if a and b then
			fprint @str,"  #/#",a.size,b.size
		elsif a then
			fprint @str,"  #",a.size
		else
			strcpy(str,"  ")
		fi
	else
		strcpy(str,"  ")
	fi

	ipadstr(str,4)

	strcat(str,opcname)

	asmstr(str)

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode,a,b)
!
		asmopnd(a,sizepref)
		asmstr(",	")
		asmopnd(b,sizepref)

		if mcl.c then
			asmstr(",")
			asmstr(strint(mcl.c))
		fi

!		ASMSTR("; ")
!		ASMSTR(strint(a.size))
!		ASMSTR(" ")
!		ASMSTR(strint(b.size))
!		ASMSTR(" #")
!		ASMSTR(STRINT(MCL.SEQNO))
!!
	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0,opcode)
		else
			asmopnd(a,1,opcode)
		fi
	fi

!ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO))

end

!global func strmclstr(ref mclrec m)ichar=
!	gs_init(pdest)
!	strmcl(m)
!	return pdest.strptr
!end
!
global func mstropnd(mclopnd a,int sizeprefix=0,opcode=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus,t
	int offset,tc

	str[1]:=0

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
		if opcode=m_dq and a.valtype=intimm_val then
			if a.value in 0..9 then
				strcat(str,strint(a.value))
			else
				strcat(str,"0x")
				strcat(str,strword(a.value,"H"))
			fi
		else
			strcpy(str,strvalue(a))
		fi

	when a_mem then
		strcat(str,getsizeprefix(a.size,sizeprefix))
		strcat(str,"[")

		plus:=""
		if a.reg then
			strcat(str,strreg(a.reg,8))
			plus:=" + "
		fi
		if a.regix then
			strcat(str,plus)
			strcat(str,strreg(a.regix,8))
			plus:=" + "

			if a.scale>1 then
				strcat(str,"*")
				strcat(str,strint(a.scale))
			fi
		fi

		if a.valtype in [def_val,label_val, temp_val] then
			IF A.REG=A.REGIX=RNONE AND PHIGHMEM THEN
				STRCAT(STR, "%rip+")
			fi
			if plus^ then
				strcat(str,plus)
			fi
			strcat(str,strvalue(a))
	    elsif offset:=a.offset then
			print @str2,offset:" + "
			strcat(str,str2)
		fi
		strcat(str,"]")

	when a_xreg then
		return strxreg(a.reg,a.size)

	else
		println "BAD OPND",A.MODE
		return "<BAD OPND>"
	esac

	return str
end

global func strvalue(mclopnd a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	i64 value,offset,length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(str,"")

	case a.valtype
	when def_val then
		strcat(str,getdispname(def))

	addoffset:
		if offset:=a.offset then
			print @str2,(offset>0|"+"|""),,offset
			strcat(str,str2)
		fi

	when intimm_val then
		strcat(str,strint(value))

	when realimm_val then
		print @str,a.xvalue:"20.20"

	when realmem_val then
		strcat(str,"M")
		strcat(str,strreal(a.xvalue))

	when stringimm_val then
		strcat(str,"""")
		strcat(str,a.svalue)
		strcat(str,"""")

	when name_val then
		strcat(str,a.svalue)

	when label_val then
		strcat(str,"L")
		strcat(str,strint(a.labelno))
		goto addoffset

	when temp_val then
		return gettempname(currasmproc,a.tempno)

	else
		merror("Stropnd?")
	esac

	return str

end

global proc asmopnd(mclopnd a,int sizeprefix=0,opcode=0)=
	asmstr(mstropnd(a,sizeprefix,opcode))
end

global func getxregname(int reg,size=8)ichar=
	static [32]char str

	if reg=rnone then return "-" fi

!	if fasmformat then
		print @str,"%XMM",,reg-xr0
!	else
!		print @str,(size=8|"DX"|"SX"),,reg-xr0
!	fi
	return str
end

proc asmstr(ichar s)=
	gs_str(pdest,s)
end

proc asmchar(int c)=
	gs_char(pdest,c)
end

global func getdispname(psymbol d)ichar=
	static [256]char str

	return getfullname(d)
end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

	if fpshortnames then
		print @str,"T",,n
	else
		fprint @str,"#.$T#",getdispname(d),n
	fi
	str
end

func strreg(int reg, size=8)ichar=
	static [16]char str

	strcpy(str, "%")
	strcat(str, nregnames[size, reg])

	str
end

func strxreg(int reg, size=8)ichar=
	psymbol d

	d:=checkregvar(reg,1)

	if size=8 and d then
		return getdispname(d)
	else
		return getxregname(reg,size)
	fi
end

global func needsizeprefix(int opcode,mclopnd a,b)int=
	case opcode
	when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
		return 1

	when m_cvtss2si,m_cvtsd2si, m_cvttss2si,m_cvttsd2si then
		return 1
	when m_shl, m_shr, m_sar then
		if a.mode=a_mem then return 1 fi
		return 0
	esac

	if a.mode=a_reg or a.mode=a_xreg or b.mode=a_reg or b.mode=a_xreg then
		return 0
	fi
	return 1
end

global func getsizeprefix(int size,enable=0)ichar=
	if not enable then return "" fi
	case size
	when 1 then return "byte ptr"
	when 2 then return "word ptr"
	when 4 then return "dword ptr"
	when 8 then return "qword ptr"
	esac
	return ""
end

func checkregvar(int reg, ispfloat)psymbol d=
	RETURN NIL
end

=== END ===
1 cc.m 0 0
2 cc_blockpcl.m 0 0
3 cc_cli.m 0 0
4 cc_decls.m 0 0
5 cc_genpcl.m 0 0
6 cc_lex.m 0 0
7 cc_lib.m 0 0
8 cc_libpcl.m 0 0
9 cc_parse.m 0 0
10 cc_show.m 0 0
11 cc_showdummy.m 0 0
12 cc_support.m 0 0
13 cc_tables.m 0 0
14 pc_api.m 0 0
15 pc_decls.m 0 0
16 pc_diags.m 0 0
17 pc_diags_dummy.m 0 0
18 pc_tables.m 0 0
19 mc_auxmcl.m 0 0
20 mc_decls.m 0 0
21 mc_genmcl.m 0 0
22 mc_libmcl.m 0 0
23 mc_stackmcl.m 0 0
24 mc_writegas.m 0 0
