!Parser

int intabledata
ichar tabledataname=nil

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
byte yieldseen

macro readunit=readexpression()
macro readxunit=readunit

int currdllindex
int nextlambdaindex

const maxlisttype=20
[maxlisttype]int listtypestack
int nlisttype
int listtype				!0 / 'PARAM' / 'PRINT' / 'DICT'

global proc parsemodule(ifile pm)=
	unit p

	return when pm.compiled

	currmodule:=pm
	stcurrmodule:=currmodule.def

	startlex(currmodule)

	lex()
	lex()

	stcurrproc:=stcurrmodule

	p:=readsunit()

	stcurrmodule.code:=pm.ast:=p

	skipsemi()
	case lx.symbol
	when commasym then
		serror("Comma seq not allowed")
	when eofsym then
	else
		PS("EOF")
		serror("Bad symbol at eof")
	esac
end

function readexpression:unit p=
	p:=readterm2()

	if exprendset[lx.symbol] then return p fi

	if lx.symbol in [assignsym, deepcopysym] then
		return readassignment(p)
	else
		return readorterms(p)
	fi
end

function readassignment(unit p)unit=
	int pos,opc
	unit q,r

	if exprendset[lx.symbol] then return p fi

	p:=readorterms(p)

	if lx.symbol in [assignsym,deepcopysym] then
		opc:=lx.subcode
		pos:=lx.pos
		lex()
		p:=createunit2(opc,p,readassignment(readterm2()))
		p.pos:=pos
	fi
	return p
end
!
function readorterms(unit p)unit =
	int pos
	unit q, r

	if exprendset[lx.symbol] then return p fi

	p:=readandterms(p)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jorlto,p,readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jorl,p,readandterms(readterm2()))
		p.pos:=pos
	od

	while lx.symbol=pipesym do
		lex()
		q:=r:=readterm2()
		if q.tag=jcall then
			r:=q.b
			while r.nextunit, r:=r.nextunit do od
			r.nextunit:=p
			p:=q
		else
			p:=createunit2(jcall, q, p)
		fi

	od

	return p
end

function readandterms(unit p)unit =
	int pos

	p:=readcmpterms(p)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jandlto,p,readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jandl,p,readcmpterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readcmpterms(unit p)unit =
	int pos,n
	unit px,q
	[4]byte genops

	p:=readinterms(p)

	if not cmpopset[lx.symbol] then
		return p
	fi

	clear genops
	px:=p
	p:=createunit1(jcmpchain,p)
	n:=0				!n counts operand after the first

	while cmpopset[lx.symbol] do
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode
	
		pos:=lx.pos
		lex()

		q:=readinterms(readterm2())
		px.nextunit:=q
		px:=q

		q.pos:=pos
	od
!
	if n=1 then
		p.tag:=genops[1]
		q:=p.a
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	fi	

	return p
end

function readinterms(unit p)unit =
	int pos,opc

	p:=readrangeterm(p)

	docase lx.symbol
	when insym, notinsym, inxsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(opc,p,readrangeterm(readterm2()))
		p.pos:=pos
	else
		exit
	end docase

	return p
end

function readrangeterm(unit p)unit =
	int pos

	p:=readaddterms(p)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange,p,readaddterms(readterm2()))
		p.pos:=pos
	fi

	return p
end

function readaddterms(unit p)unit =
	int pos,opc,a,b
	unit q

	p:=readmulterms(p)
	while addopset[lx.symbol] do
		case opc:=lx.subcode
		when jaddrof then opc:=jappend
!		when jdaddrof then opc:=jconcat
		esac

		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jtocodes[opc],p,readassignment(readterm2()))
			p.pos:=pos
			exit
		fi

		q:=readmulterms(readterm2())
		p:=createunit2(opc,p,q)
		p.pos:=pos
	od

	return p
end

function readmulterms(unit p)unit =
	int pos,opc,a,b
	unit q

	p:=readpowerterms(p)

	while mulopset[lx.symbol] do
		opc:=lx.subcode
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jtocodes[opc],p,readassignment(readterm2()))
			p.pos:=pos
			exit
		fi

		p:=createunit2(opc,p,readpowerterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readpowerterms(unit p)unit =
	int pos

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jpower,p,readpowerterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readterm2:unit p=
	int pos

	pos:=lx.pos
	p:=readterm()
	p:=readtermsuffix(p,pos)
	return p
end

function readtermsuffix(unit p, int pos)unit=
	unit q,r
	ref char pbyte
	word64 a
	int opc,oldipl,shift,t,nparams

	docase lx.symbol
	when lbracksym then
		lex()
		q:=readslist(nparams,1)

		skipsymbol(rbracksym)
		p:=createunit2(jcall,p,q)
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr,p)
		lex()

	when lsqsym then
		p:=readindex(p,0)

	when dotsym then
		p:=readdotsuffix(p)

	when lcurlysym then
		p:=readkeyindex(p)

	when colonsym then
		case listtype
		when 'PARAM' then
			lex()
			p:=createunit2(jkeyword,p,readunit())
		when 'DICT' then
			lex()
			p:=createunit2(jkeyvalue,p,readunit())
		else
			exit
		esac

	when incrsym then
		case lx.subcode
		when jincrload then opc:=jloadincr
		when jdecrload then opc:=jloaddecr
		esac
		lex()
		p:=createunit1(opc,p)

	else
		exit
	end docase

	p.pos:=pos

	return p
end

function readterm:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t,nparams,length

	record dummy=
		union
			[20]char str
			int64 sa
		end
	end

	dummy ustr

	pos:=lx.pos

	switch lx.symbol
	when namesym then
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

	when intconstsym then
		p:=createintunit(lx.value)
		lex()

	when realconstsym then
		p:=createrealunit(lx.xvalue)
		lex()

	when stringconstsym then
		p:=createstringunit(lx.svalue)
		lex()

	when decimalconstsym then
		p:=createstringunit(lx.svalue)
		p.tag:=jdecimal
		lex()

	when charconstsym then
		length:=strlen(lx.svalue)
		ustr.sa:=0
!		sa:=0
		if length>8 then
			serror("char const too long")
		fi
		memcpy(&.ustr.str,lx.svalue,length)
		p:=createintunit(ustr.sa)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym then
		if lx.subcode=tvoid then
			lex()
			if lx.symbol=dotsym and nextlx.symbol=ktypesym then
				lex()
				lex()
				p:=createunit0(jtypeconst)
				p.mode:=tvoid
			else
				p:=createunit0(jnull)
			fi
		else
			p:=readcast()

		fi

	when addsym then
		p:=checkoperator()
		if not p then
			lex()
			p:=readterm2()
		fi

	when subsym  then
		p:=checkoperator()
		if not p then
			lex()
			if lx.symbol=assignsym then
				opc:=jneg
				goto dounary
			fi
			p:=readterm2()
			if p.tag=jintconst then
				p.value:=-p.value
			else
				p:=createunit1(jneg, p)
			fi
		fi

	when notlsym, istruelsym, inotsym, abssym, sqrsym, signsym,ascsym, chrsym,
			incrsym, decrsym, mathssym, maths2sym  then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			if lx.symbol=assignsym then
dounary:
				lex()
				p:=createunit1(jtocodes[opc],readterm2())
			else
				p:=createunit1(opc, readterm2())
			fi
		fi

	when mulsym, divsym, idivsym, iremsym, andlsym, orlsym,
		iandsym, iorsym, ixorsym, shlsym, shrsym, insym, notinsym, inxsym,
		eqsym,nesym,ltsym, lesym,gesym,gtsym,powersym, appendsym,
		concatsym,  daddrsym, propsym, isequalsym, specialopsym then
		unless p:=checkoperator() then
			serror("Operator?")
		end

	when lsqsym then
		p:=readset()

	when minsym, maxsym then
		if p:=checkoperator() then
		else
			p:=readpair(lx.subcode)
		fi

	when ksprintsym then
		p:=readsprint()

	when ksreadsym,ksreadlnsym then
		p:=readsread()

	when addrsym,ptrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc,readterm2())
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when compilervarsym then
		p:=readcompilervar()
		lex()

	when dollarsym then
		if intabledata then
			if tabledataname=nil then serror("$:No enum") fi
			p:=createstringunit(tabledataname,-1)
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(jupb,dollarstack[ndollar])
		fi
		lex()

	when dotsym,kglobalsym then
		lexchecksymbol(namesym)
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

	when kmapsym then
!		p:=readmap()
		p:=readpair(jmap)

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		skipsymbol(commasym)
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			skipsymbol(commasym)
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(jmax,p,q)
		p:=createunit2(jmin,q,r)

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym,kdocasesym,kswitchsym,kdoswitchsym then
		p:=readswitchcase()

	when kforsym then
		p:=readfor()

	when ktosym then
		p:=readto()

	when kdosym then
		p:=readdo()

	when kwhilesym then
		p:=readwhile()

	when krepeatsym then
		p:=readrepeat()

	when kloopsym then
		p:=readloopcontrol()

	when kreturnsym then
		p:=readreturn()

	when kstopsym then
		p:=readstop()

	when kprintsym then
		p:=readprint()

	when kreadsym then
		p:=readread()

	when ktrysym then	!todo
		p:=readtry()
!
	when kraisesym then	!todo
		lex()
		p:=createunit1(jraise,readunit())

	when kswapsym then			!swap using function syntax
		p:=readpair(jswap)
!
	when khostfnsym then
		p:=readhostparams(nil,1)

	when knilsym then
!		p:=createunit0((lx.subcode=1|jpnil|jnil))
		p:=createunit0(jnil)
		lex()

	when kstrincludesym then
		lex()
		p:=createunit1(jstrinclude,readterm2())

	when kevalsym then
		lex()
		p:=createunit1(jeval,readunit())


	when lcurlysym then
		p:=readlambda()

	else

error:
		cpl symbolnames[lx.symbol]:"d"
		serror("readterm?")
	end switch

	p.pos:=pos
	return p
end

function readsunit(int inwhile=0)unit=
	int lineno,m,globalflag,staticflag
	unit ulist,ulistx,p,q,r
	symbol stname

	lineno:=lx.pos
	ulist:=ulistx:=nil
	globalflag:=local_scope
	staticflag:=0

	repeat
		while lx.symbol=semisym do
			lex()
		od

		switch lx.symbol
		when kstaticsym then
			lex()
			staticflag:=1
			redoloop

		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode
			lex()
			redoloop

		when kprocsym,kfunctionsym then
			readprocdef(globalflag)
			globalflag:=local_scope

		when kvarsym then
			q:=readvardef(globalflag,staticflag)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist,ulistx,q)			!add one by-one
				q:=r
			od

			globalflag:=staticflag:=local_scope

		when kconstsym then
			if staticflag then serror("static?") fi
			readconstdef(globalflag)
			globalflag:=local_scope

		when ktypesym then
			readtypedef(globalflag)
			globalflag:=local_scope

		when krecordsym, kstructsym then
			readrecorddef(globalflag, nil)
			globalflag:=local_scope

		when ktabledatasym then
			readtabledef(globalflag)
			globalflag:=local_scope

		when kimportdllsym then
			readimportdll()

		when kmacrosym then
			readmacrodef(globalflag)
			globalflag:=local_scope

		when eofsym then
			exit

!these are needed to check for an empty sunit preceding
		when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,sendtosym,
				kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
			exit

		when namesym then
			case nextlx.symbol
!		when dcolonsym,colonsym then
			when colonsym then
				p:=createunit1(jlabeldef,createname(addsymbol(stcurrproc, lx.symptr, labelid, 0)))
				lex()
				lx.symbol:=semisym
				addlistunit(ulist,ulistx,p)
!		when namesym then
!			goto dovar
			else
				goto doexec
			esac
		when kdosym then				!u;u;u;do rather than u;u;u do
			if inwhile then
				exit
			fi
			goto doexec

		when kmodulesym, kimportsym then
			repeat
				lex()
			until lx.symbol=semisym

		when semisym then
		when lsqsym then
			doexec
!	elsif istypestarter() and nextlx.symbol<>lbracksym then
!		goto dovar

		else							!assume a statement

	doexec:
			p:=readunit()

			if p.tag=jname and lx.symbol=namesym then
				serror("Possibly var/let needed")
			fi
			addlistunit(ulist,ulistx,p)
			if lx.symbol=kdosym then
				exit
			fi

		end switch

	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,sendtosym,
		kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym,commasym,
		barsym,eofsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then			!empty or multiple => block
		return createunit1(jblock,ulist)
	else
		return ulist							!single => one unit
	fi
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

function readindex(unit p,int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q,plower,pupper

	lex()

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		fi
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

		p:=createunit2((dot|jdotindex|jindex),p,q)

		exit when lx.symbol<>commasym
		lex()
	od
	skipsymbol(rsqsym)
	return p
end

function readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
	unit q
	int t

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p,1)
		when namesym then
			p:=createunit2(jdot,p,createname(lx.symptr))
			lex()
		when  propsym then			!ought to check whether op is allowed in this form
doprop:
			p:=createunit1(lx.subcode,p)
			lex()
		when ktypesym then
			if p.tag<>jtypeconst then
				p:=createunit1(jtype,p)
			fi
			lex()

		when maxsym then
			lx.subcode:=jmaxvalue
			goto doprop

		when minsym then
			lx.subcode:=jminvalue
			lx.symbol:=propsym
			goto doprop
		when dollarsym then
			if p.tag not in [jname,jdot] then
				serror("...name.$ needed")
			fi
			p:=createunit1(jsymbol,p)
			lex()

		else
			serror("Unknown dot suffix")
		esac
	od
	return p
end

function readslist(int &nparams, ftrailing=0)unit=
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (just comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a function-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a,b,c	)
!eg: (a		!
	unit ulist,ulistx
	int oldinparamlist

	ulist:=ulistx:=nil
	nparams:=0

	skipsemi()
	if lx.symbol=rbracksym then		!empty list
		return ulist
	fi

	pushlisttype('PARAM')

int donulls:=1

	do
		skipsemi()
		case lx.symbol
		when commasym then
			serror("null comma expr not allowed")
		when rbracksym then
			exit
		else
			addlistunit(ulist,ulistx,readunit())
			++nparams
			if lx.symbol=commasym then
				lex()
				if lx.symbol=rbracksym then
					if nparams<>1 or not ftrailing then serror("Trailing comma") fi
					exit
				fi
			else
				skipsemi()
				if lx.symbol=rbracksym then
					exit
				fi
				serror("SLIST?")
			fi
		esac
	od

	poplisttype()
	return ulist
end

function readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond

!case lx.symbol
	case lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif,readunit(),createunit1(jblock,p))
	when kunlesssym then
		lex()
		return createunit2(jif, createunit1(jnotl,readunit()),createunit1(jblock,p))
	else
		return p
	esac
end

function readkeyindex(unit p)unit=
!at '{'
!syntax is:
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q

	lex()

	q:=readunit()

	if lx.symbol=commasym then
		lex()
		q.nextunit:=readunit()
	fi
	
	p:=createunit2(jkeyindex,p,q)

	skipsymbol(rcurlysym)
	return p
end

function readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following:
! (x)		simple expression
! ()		list with no elements
! (x,)		list with one element
! (x,x,...)		list
! (x|x|x])		if then else fi
! (x|x,... |x])	select then else end

! (s||s|s)	!list comp [SYNTAX TO BE REVISED]
!return positioned at symbol following closing ")"
	unit ulist,ulistx, p,q,r
	int oldirp,length,lower,lowerseen,elemtype,opc

	lex()					!first symbol of first expression
	ulist:=ulistx:=nil
	length:=0
	lower:=1
	lowerseen:=0

	elemtype:=tvoid

	if lx.symbol=stdtypesym and nextlx.symbol=colonsym then
		elemtype:=lx.subcode
		lex()
		lex()
	fi

	if lx.symbol=intconstsym and nextlx.symbol=colonsym then
		lower:=lx.value
		lowerseen:=1
		lex()
		lex()
	fi

!check symbol after "("

	case lx.symbol
	when rbracksym then			!empty list
		lex()
		p:=createunit0(jmakelist)
		p.length:=0
		p.lower:=lower
		p.elemtype:=elemtype
		return p
	elsif (binopset[lx.symbol] or unaryopset[lx.symbol] or lx.symbol=propsym) and
			nextlx.symbol=rbracksym then
		opc:=jpclcodes[lx.subcode]
		if lx.symbol=addrsym then opc:=kappend fi
doopc:
		if opc=kzero then pcerror("Bad op") fi

		p:=createunit0(joperator)
		p.pclopcode:=opc
		lex()
!		lex()
		skipsymbol(rbracksym)
		return p
	elsecase lx.symbol
	when specialopsym then
		case lx.subcode
		when '-' then opc:=kneg
		when '[]' then opc:=kindex
		else opc:=kzero
		esac
		doopc
	when insym then
		opc:=kin
		doopc
	when notinsym then
		opc:=knotin
		doopc
	when inxsym then
		opc:=kinx
		doopc
	when daddrsym then
		opc:=kconcat
		doopc

	else					!assume normal expression follows
		p:=readxunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()
		if lowerseen then
			p:=createunit2(jkeyvalue,createintunit(lower), p)
		fi

		return p

	when commasym then
		length:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(jmakelist,p)
			p.length:=length
			p.lower:=lower
			p.elemtype:=elemtype
			return p
		fi

!must be regular list
		ulist:=ulistx:=p
		repeat
			lex()							!skip comma
			if lx.symbol=rbracksym then		!allow ,) to end list
				exit
			fi
			if lx.symbol=commasym then
				serror(",, null expr not allowed")
			fi
			addlistunit(ulist,ulistx,readxunit())
			++length
			skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
		until lx.symbol<>commasym
		skipsymbol(rbracksym)
		p:=createunit1(jmakelist,ulist)
		p.length:=length
		p.lower:=lower
		p.elemtype:=elemtype
		return p

	when barsym then			!ifx/selectx expression; p is selector expression
		lex()
		q:=readxunit()
		case lx.symbol
		when barsym then		!(a|b|c)
			lex()
			r:=readsunit()
			skipsymbol(rbracksym)
			q.nextunit:=r
			return createunit2(jif,p,q)
		when rbracksym then
			lex()
			return createunit2(jif,p,q)

		esac

!assume selectx expression
		addlistunit(ulist,ulistx,q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist,ulistx,readxunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readxunit()
		skipsymbol(rbracksym)
		p.nextunit:=r
		return createunit2(jselect,p,ulist)

	when semisym then
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			fi
			addlistunit(ulist,ulistx,readunit())
		until lx.symbol<>semisym
		skipsymbol(rbracksym)
		return makeblock(ulist)


	else
		serror("(x ...")
	esac
	return nil
end

function readif:unit=
!at 'if'
	int line, kwd, lineno
	unit pthen,pcond, plist,plistx, pelse, p, pelsif

	line:=lx.pos

	kwd:=lx.symbol			!in case coming from elsecase etc

	lex()
	pcond:=readsunit()
	skipsemi()

	skipsymbol(kthensym)

	pthen:=readsunit()

	case lx.symbol
	when kelsifsym then
		lx.symbol:=kifsym		!for .kwd
		pelse:=readif()

	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kwd)
!		lex()
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
!	SERROR("ELSECASE NOT READY")
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kwd)
!		lex()
	esac

	pthen.nextunit:=pelse
	p:=createunit2(jif,pcond,pthen)
	p.pos:=line

	return p
end

proc checkend(int endkwd1, endkwd2=0, startline=0)=
!check end or end kwd1 or end kwd2, or is fi/esac/do
!return on symbol following any of that, which is expected to be semisym
	[256]char str

	skipsemi()

!symbol must be endsym or fi/esac/od which are endsym with subcode
!check I have end/fi/esac/cp
	if lx.symbol<>kendsym then
		serror("'End' expected")
	fi

	if lx.subcode then
		if lx.subcode in [endkwd1, endkwd2] then
			lex()
			return
		else
error:
			strcpy(str,"Mismatched end ")
			if startline then
				fprint @(&.str+strlen(&.str))," (from line #)",startline
			fi
			serror(&.str)
		fi
	fi

!only end was used, so skip that now
	lex()

!now, should be semi, or possibly kwd1/2
	if lx.symbol in [endkwd1, endkwd2] then
		lex()
	elsif lx.symbol<>semisym then
		error
	fi
end

function readunless:unit=
	int line
	unit pcond, pthen, pelse, p
	line:=lx.pos
	lex()
	pcond:=readsunit()
	skipsymbol(kthensym)

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		pelse:=nil
	fi
	checkend(kunlesssym)
!	lex()
	pthen.nextunit:=pelse
	p:=createunit2(jif,createunit1(jnotl,pcond),pthen)
	p.pos:=line
	return p
end

function readwhile:unit=
	int pos
	unit pcond, pbody, p

	pos:=lx.pos
	lex()

	pcond:=readsunit(1)

	if lx.symbol=commasym then
		lex()
		pcond.nextunit:=readsunit(1)
	fi

	skipsymbol(kdosym)
	pbody:=readsunit()

	checkend(kwhilesym,kdosym)
!	lex()

	p:=createunit2(jwhile,pcond,pbody)
	p.pos:=pos
	return p
end

function readrepeat:unit=
	int pos
	unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	skipsymbol(kuntilsym)
	pcond:=readunit()
	p:=createunit2(jrepeat,pbody,pcond)
	p.pos:=pos
	return p
end

function readfor:unit=
!on 'for'; syntax is:
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

	int line, opc, down, isforeach
	unit pstep, pvar, pcond, pfrom, pto, pelse, pbody, p, plist,pvar2

	line:=lx.pos
	isforeach:=lx.subcode
	lex()			!skip 'for'
	pvar:=readterm2()

	if pvar.tag<>jname then
		serror("For: name expected")
	else
		pvar.def.forindex:=1
	fi

	opc:=jforup
	pstep:=nil
	pcond:=nil
	pvar2:=nil

	if lx.symbol=commasym then			!double index
		lex()
		pvar2:=readterm2()
	fi

	if lx.symbol in [insym,inrevsym] then	!assume in/inrev
		down:=lx.symbol=inrevsym
		lex()
		plist:=readunit()

		case plist.tag
		when jmakerange then		!in a..b: simple iteration
			opc:=jforup
			pfrom:=plist.a
			pto:=plist.b
		when jbounds then			!
			plist.tag:=jboundsx
			opc:=jforupx
		else
			opc:=jforall
		esac
	else
		if lx.symbol=assignsym then
			lex()
			pfrom:=readunit()
		else
			pfrom:=createintunit(1)
		fi
		checksymbol(ktosym)
		down:=lx.subcode=1
		opc:=jforup
		lex()
		pto:=readunit()

		if lx.symbol=kbysym then
			lex()
			pstep:=readunit()
			if pstep.tag<>jintconst then serror("BY needs int constant") fi
			if pstep.value<0 then 
				serror("Step must be positive")
			elsif pstep.value=0 then
				serror("Zero step")
			fi
			pstep.value:=abs pstep.value
			if pstep.value=1 then		!by 1
				pstep:=nil
			fi
		fi
	fi

	if lx.symbol=kwhensym then
		lex()
		pcond:=readunit()
	fi
	skipsymbol(kdosym)
	pbody:=readsunit()

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif,pcond,pbody))
	fi
	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
		pbody.nextunit:=pelse
	else
		pelse:=nil
	fi
	checkend(kforsym,kdosym)
!	lex()


	case opc
	when jforall then
		pvar.nextunit:=plist
		plist.nextunit:=pvar2
		p:=createunit2((down|jforallrev|jforall),pvar,pbody)

	when jforupx then
		pvar.nextunit:=plist
		p:=createunit2((down|jfordownx|jforupx),pvar,pbody)
	else
		pvar.nextunit:=pfrom
		pfrom.nextunit:=pto
		pto.nextunit:=pstep
		p:=createunit2((down|jfordown|jforup),pvar,pbody)
	esac

	if isforeach then
		if p.tag=jforall then
			p.tag:=jforeach
		else
			serror("Foreach?")
		fi
	fi

	p.pos:=line

	if pvar2 and opc not in [jforall, jforallrev] then
		serror("for i,j not allowed")
	fi

	return p
end

function readdo:unit=
	unit p
	int line

	line:=lx.pos
	lex()
	p:=readsunit()
	checkend(kdosym)
	p:=createunit1(jdo,p)
	p.pos:=line
	return p
end

function readto:unit=
	int line,id
	unit p, pcount, pbody

	line:=lx.pos
	lex()

	pcount:=readunit()

	skipsymbol(kdosym)
	pbody:=readsunit()
	checkend(ktosym,kdosym)
!	lex()

	pcount.nextunit:=createavname()

	p:=createunit2(jto,pcount,pbody)
	p.pos:=line
	return p
end

function makeblock(unit p)unit=
	return createunit1(jblock,p)
end

function readvardef(int isglobal=0, isstatic=0)unit=
!positioned at 'var' 'let'
	int nvars,varid, opc
	symbol d
	unit ulist, ulistx, p

!	m:=readtypespec(lx.symbol=kvarsym)
	lex()
!	M:=TVAR

!	if stcurrproc.nameid=procid then
	if stcurrproc.nameid in [procid,anonprocid] then
		varid:=(isstatic|staticid|frameid)
	else
		varid:=staticid
	fi
	nvars:=0
	ulist:=ulistx:=nil

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, varid, isglobal)
!		storemode(stcurrproc,m,&d.mode)

		lex()

		case lx.symbol
		when assignsym,deepcopysym then
			opc:=lx.subcode
			if varid=staticid then
!				if stcurrproc.nameid=procid then
				if stcurrproc.nameid in [procid,anonprocid] then
					serror("Need '=' for static in proc")
				fi
			fi
			d.initcode:=(lx.symbol=assignsym|2|3)
			lex()
			d.code:=readunit()
			if varid=frameid then
				p:=createunit2(opc,createname(d),d.code)
				addlistunit(ulist, ulistx, p)
			fi

		when eqsym then
			if varid<>staticid then serror("Need ':=' for non-static") fi
			lex()

			d.initcode:=1
			d.code:=readunit()
		esac

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No vars declared")
	fi
	return ulist
end

proc readconstdef(int isglobal=0)=
!positioned at 'const'
	int nvars
	symbol d

	lex()

	nvars:=0
	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, constid, isglobal)
		lexchecksymbol(eqsym)
		lex()

		d.code:=readunit()

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No consts declared")
	fi
end

function readreturn:unit=
	unit p,q,r

	lex()
	q:=nil

	if exprstarterset[lx.symbol] then
		q:=readunit()
	fi
	p:=createunit1(jreturn, q)

	return readcondsuffix(p)
end

function readprint:unit=
	int opc, isfprint, fshowname, length
	unit pformat, pdev, printlist,printlistx, p,q
	ref strbuffer expr

	ichar s

	pushlisttype('PRINT')

	opc:=lx.subcode

	case opc
	when jfprint,jfprintln then
		isfprint:=1
	else
		isfprint:=0
	esac

	lex()

	printlist:=printlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi
	if isfprint then
		pformat:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

!if lx.symbol=semisym then
	if not exprstarterset[lx.symbol] then
		goto finish
	fi

	do
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(printlist,printlistx, createunit0(jnogap))
		when dollarsym then
			addlistunit(printlist,printlistx, createunit0(jspace))
			lex()
		else

			fshowname:=0
			if lx.symbol=eqsym then
				fshowname:=1
				lex()
			fi

			p:=readunit()
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem,p,readunit())
			fi

			if fshowname then
				expr:=strexpr(p)
				strbuffer_add(expr,"=")
				s:=expr.strptr

				iconvucn(expr.strptr,expr.length)

				addlistunit(printlist,printlistx,q:=createstringunit(s,expr.length))
			fi
			addlistunit(printlist,printlistx,p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	if opc=jprint and printlist=nil then
		serror("No print items")
	fi
	if opc=jfprint and printlist=nil and pformat=nil then
		serror("No print items")
	fi

	poplisttype()
	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		if pformat=nil then
			pformat:=makeblock(pformat)
		fi
		pformat.nextunit:=printlist
		return createunit2(opc,pdev,pformat)
		return pformat
	else

		return createunit2(opc,pdev,printlist)
	fi

end

function readread:unit=
	int opc
	unit pformat, pdev, readlist, readlistx, p

	pushlisttype('PRINT')
	opc:=lx.subcode
	lex()

	readlist:=readlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		if opc=jread then
			serror("@ on read")
		fi
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

	if not exprstarterset[lx.symbol] then
		goto finish
	fi

	do
		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(jfmtitem,p,readunit())
		fi
		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	poplisttype()
	return createunit2(opc,pdev,readlist)
end

function readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode
	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name,"all") then
		lex()
		p:=createunit1(opc,createintunit(0))

	elsif exprstarterset[lx.symbol] then
		p:=createunit1(opc,readintunit())
	else
		p:=createunit1(opc,createintunit(1))
	fi
	return readcondsuffix(p)
end

function readintunit:unit p=
	p:=readunit()
	if p.tag<>jintconst then
		serror("int expr needed")
	fi
	return p
end

function readswitchcase:unit=
	int pos, kwd, opc, lineno,rangeused, nwhen
	unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

	pos:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc
	opc:=lx.subcode			!pick up tag: kcase etc

	lex()

	skipsemi()
	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		fi
		pexpr:=CREATEUNIT0(JNONE)
	else
		pexpr:=readsunit()		!index expression
	fi

	pwhenlist:=pwhenlistx:=nil
	rangeused:=0
	nwhen:=0

	skipsemi()
	while lx.symbol=kwhensym do	!read list of when-then pairs
		pos:=lx.pos
		lex()
		pwhen:=pwhenx:=nil
		do
			p:=readunit()
			++nwhen
			p.pos:=pos
			if p.tag=jmakerange then rangeused:=1 fi
			addlistunit(pwhen,pwhenx,p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>kthensym then checksymbol(sendtosym) fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen,pwhen,pthen)
		pwhenthen.pos:=pos
		addlistunit(pwhenlist,pwhenlistx,pwhenthen)
	od

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()

		checkend(kwd)
!		lex()
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then

		lx.symbol:=kwd
		pelse:=readswitchcase()
	else
		PELSE:=NIL
		checkend(kwd)
!		lex()
	esac

	pexpr.nextunit:=pelse

	p:=createunit2(opc,pexpr,pwhenlist)
	p.pos:=pos
	return p
end

function readgoto:unit=
	lex()

	return readcondsuffix(createunit1(jgoto,readunit()))
end

function readstop:unit=
	unit p
	int i
	lex()
	if exprstarterset[lx.symbol] then
		p:=createunit1(jstop,readunit())
	else
		p:=createunit1(jstop,createintunit(0))
	fi
	return readcondsuffix(p)
end

function readcast:unit p=
!just seem basic type name
	int t,opc

	t:=lx.subcode
	lex()

	if t=trange and lx.symbol=lbracksym then
		lex()
		p:=readunit()
		if p.tag in [jkeyvalue,jkeyword] then
			p.tag:=jmakerangelen
		elsif p.tag=jmakerange then
		else
			serror("need a..b or a:n")
		fi
		skipsymbol(rbracksym)
		return p
	fi


!check for standalone value
	case lx.symbol
	when atsym,lbracksym then

	else						!convert to typeconst
		p:=createunit0(jtypeconst)
		p.mode:=t
		return p
	esac

	if lx.symbol=atsym then
		lex()
		opc:=jtypepun
	else
		opc:=jconvert
	fi
	checksymbol(lbracksym)
	p:=readterm()

	p:=createunit1(opc,p)
	storemode(stcurrproc,t,&p.mode)
	return p
end

function readset:unit=
!positioned at "["
	int length,nkeyvalues,oldinparamlist
	unit p,ulist,ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset,nil)
	when colonsym then
		lexchecksymbol(rsqsym)
		lex()
		return createunit1(jmakedict,nil)
	esac

	pushlisttype('DICT')

	p:=readunit()
	length:=1
	nkeyvalues:=0
	if p.tag=jkeyvalue then ++nkeyvalues fi

	ulist:=ulistx:=p

	while lx.symbol=commasym do
		lex()
		if lx.symbol=rsqsym then exit fi		!allow trailing comma
		addlistunit(ulist,ulistx,p:=readunit())
		if p.tag=jkeyvalue then ++nkeyvalues fi

		++length
		skipsemi()						!allow a,b,c;]
	od

	skipsymbol(rsqsym)

	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict,ulist)
	else
		p:=createunit1(jmakeset,ulist)
	fi
	p.length:=length
	poplisttype()
	return p
end

global proc readtabledef(int isglobal=0)=
!at 'tabledata' symbol
	int i,ncols,nrows,enums,nextenumvalue,startline, firstvalue
	int ltype,lower
	byte commas:=0, semis:=0
	unit ulist,ulistx, plower, p
	const maxcols=20
	[maxcols]symbol varnames
	[maxcols]unit plist,plistx
	symbol d, nameptr

	const maxrows=500

	enums:=lx.subcode						!whether there is an enums column
	lex()

	firstvalue:=nextenumvalue:=1
	
	nrows:=0			!number of data rows appearing
	ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
	while lx.symbol=namesym do
		if ++ncols>maxcols then
			serror("tabledata/too many columns")
		fi
		varnames[ncols]:=lx.symptr

		lex()
		if lx.symbol=commasym then
			lex()
		else
			exit
		fi
	od

	checkequals()
	lex()					!skip =

	skipsemi()
	startline:=lx.pos

	skipsemi()

	for i:=1 to ncols do
		plist[i]:=plistx[i]:=nil
	od
	ulist:=ulistx:=nil

	intabledata:=1
	do			!loop per row
		skipsemi()
		if ncols>0 then
			skipsymbol(lbracksym)
		fi
		if ++nrows>maxrows then
			serror("tabledata:too many rows")
		fi

		if enums then
			checksymbol(namesym)

			d:=addsymbol(stcurrproc,lx.symptr,enumid, isglobal)
			lex()

			case lx.symbol
			when eqsym then
				if nrows>1 then serror("tabledata '=' not 1st") fi
				lex()
				p:=readunit()
				if p.tag=jintconst then
					firstvalue:=nextenumvalue:=p.value
				else
					SERROR("TABLEDATA: COMPLEX ENUM VAL")
				fi
			esac

			d.index:=nextenumvalue++

			tabledataname:=d.name

			if ncols then				!comma always expected
				skipsymbol(commasym)		!check it
			fi
		fi

		for i:=1 to ncols do
			addlistunit(plist[i],plistx[i],readunit())
			if i=ncols then
				skipsymbol(rbracksym)
			else
				skipsymbol(commasym)
			fi
		od

		case lx.symbol
		when commasym then
			++commas
			lex()
!			if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
			if lx.symbol=kendsym then exit fi		!allow trailing comma on last entry
		else
			skipsemi()
			if lx.symbol=kendsym then exit fi
			++semis
		esac
!		if lx.symbol<>commasym then exit fi
!		lex()					!should be ( for next entry
!		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	od

	if semis and commas then serror("mixed commas") fi

	intabledata:=0

	skipsemi()
	checkend(ktabledatasym,startline:startline)

!enum				1 means enum 0th column present, 0 means not
!ncols				0, or number of actual expression columns
!nrows				Number of data rows
!enumtypename			"", or enum user type name to be created for enum names/values

!varnameptrs[1..ncols]		!Names of the list variables ([]strec]
!varlisttypes[1..ncols]		!Type of each list (or 0 if not provided)
!varelemttypes[1..ncols]	!Type of each element (or 0 if not provided)
!plist[1..ncols]		Each entry is a list of expression units for the column

!enumnames[1..nrows]	When enum=1, list of names/values in 0th column
!enumvalues[1..nrows]

	if nrows=0 then serror("No table data") fi

!!for each variable, add a vardef initialised to the list
!!add the decls for the vars
!
	for i:=1 to ncols do
!
		d:=addsymbol(stcurrproc,varnames[i],staticid,isglobal)

		p:=d.code:=createunit1(jmakelist,plist[i])
		p.length:=nrows
		p.lower:=firstvalue
	od
end

function readtry:unit=
	unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
	lex()

	ptry:=readsunit()
	pexceptlist:=pexceptlistx:=nil			!list of kexcept items

	while lx.symbol=kexceptsym do
		lex()
		exlist:=exlistx:=nil				!list of exception codes for this 'except'
		do
			addlistunit(exlist,exlistx,readunit())
			if lx.symbol<>commasym then exit fi
			lex()
		od
		skipsymbol(kthensym)
		px:=readsunit()
		addlistunit(pexceptlist,pexceptlistx,createunit2(jexcept,exlist,px))
	od
	checkend(ktrysym)
!	lex()

	return createunit2(jtry,ptry,pexceptlist)
end

function readsprint:unit=
	int opc,isfprint
	unit pformat, pdev, printlist, printlistx, p

	pushlisttype('PRINT')
	opc:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	case opc
	when jsfprint then
		isfprint:=1
	else
		isfprint:=0
	esac

	printlist:=printlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi
	if isfprint then
		pformat:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

	if lx.symbol=rbracksym then
		goto finish
	fi

	do
		if lx.symbol=commasym then		!assume extra comma, meaning nogap
			addlistunit(printlist,printlistx,createunit0(jnogap))
		else
			p:=readunit()
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem,p,readunit())
			fi
			addlistunit(printlist,printlistx,p)
		fi
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

finish:
	lex()
	if (opc=jprint or opc=jfprint) and printlist=nil then
		serror("No print items")
	fi

	poplisttype()
	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		pformat.nextunit:=printlist
		return createunit2(opc,pdev,pformat)
	else
		return createunit2(opc,pdev,printlist)
	fi

end

function readsread:unit=
!to work an item at a time:
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
	int opc
	unit pformat,pdev,p, readlist,readlistx

	pushlisttype('PRINT')
	opc:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	readlist:=readlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		if opc=jread then
			serror("@ on read")
		fi
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

	if lx.symbol=rbracksym then
		goto finish
	fi

	do
		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(jfmtitem,p,readunit())
		fi
		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

	finish:
	lex()
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	poplisttype()

	return createunit2(opc,pdev,readlist)
end

proc readimportdll=
!at 'importdll'
	[256]char str
	symbol stproc,d, stname
	int startpos, isfunc, isnew, libtype

	libtype:=lx.subcode

	lexchecksymbol(namesym)
	stname:=lx.symptr

	lexchecksymbol(eqsym)
	lex()

!check for an existing dll with the same name, as this could be adding to it

	isnew:=1
	d:=stname.nextdupl
!	while d do
!		if d.nameid=dllmoduleid then
!			stname:=d
!			isnew:=0
!			exit
!		fi
!		d:=d.nextdupl
!	od
	for i to nlibfiles do
		if eqstring(libtable[i].name, stname.name) then
			stname:=libtable[i]
			isnew:=0
			exit
		fi
	od

	if isnew then			!new
!		stname:=addsymbol(stprogram,stname,dllmoduleid,0)
		stname:=addsymbol(nil,stname,dllmoduleid,0)
		if nlibfiles>=maxlibfile then
			serror("Too many DLL libs")
		fi

		libtable[++nlibfiles]:=stname
		libtypes[+nlibfiles]:=libtype
		stname.index:=nlibfiles
	fi

	currdllindex:=stname.index

	startpos:=lx.pos
!------------------------------------
	do
		skipsemi()

		case lx.symbol
		when kfflangsym then
			lex()
		when kprocsym,kfunctionsym then
			isfunc:=lx.symbol=kfunctionsym
			lex()
			case lx.symbol
			when namesym then
				stproc:=addsymbol(stcurrproc, lx.symptr, dllprocid, 1)

			when stringconstsym then
				strcpy(str,lx.svalue)
				convlcstring(str)
				stproc:=addsymbol(stcurrproc, addglobalname(str), dllprocid, 1)
				stproc.truename:=pcm_copyheapstring(lx.svalue)
			else
				serror("fn name expected")
			esac

			stproc.misfunc:=isfunc
			stproc.isimport:=1

			if ndllprocs>=maxdllproc then
				serror("Too many DLL procs")
			fi
			dllproctable[++ndllprocs]:=stproc
			dllproclibindex[ndllprocs]:=currdllindex
			stproc.index:=ndllprocs

			lex()

			if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
				lexchecksymbol(namesym)

				d:=addsymbol(stproc.owner, lx.symptr, aliasid, 1)
				d.alias:=stproc
				lex()
			fi
			readffiparams(stproc)
		when ktypesym then
			readtypedef(1)
		when kendsym then
!			if nextlx.symbol=kimportdllsym then lex() fi
			exit
		else
			readpackvars(stcurrproc, dllvarid)

		esac
	od	
!--------------------------------
	checkend(kimportdllsym, startline:startpos)
end

proc readffiparams(symbol stproc)=
!at first symbol after func name
!return list of units with dllparam defs (can be empty)
!if there is a result type, then head of list will be a return def type
	int pret,ptype

	if lx.symbol=lbracksym then
		lex()
		if lx.symbol=rbracksym then
			lex()
		else
			ptype:=readtypespec()
			if lx.symbol in [commasym,rbracksym] then		!types only
				readtypeparams(stproc,ptype)
			else
				readtypenameparams(stproc,ptype)
			fi
		fi
	fi

	if lx.symbol in [colonsym,sendtosym] then
		if not stproc.misfunc then serror("Return type for proc?") fi
		lex()
	fi

	pret:=tvoid
	if stproc.misfunc then
		if lx.symbol=semisym then serror("Return type missing") fi
		pret:=readtypespec()
	fi

	storemode(stproc.owner,pret, &stproc.mode)
end

proc readtypeparams(symbol stproc, int ptype)=
!at symbol after ptype
	[32]char str
	int nparams
	symbol stname


	nparams:=0

	do
		++nparams
		print @str,"$",,nparams

		stname:=addsymbol(stproc, addglobalname(str), dllparamid, 0)
		storemode(stproc,ptype,&stname.mode)
		++stproc.nparams

		if lx.symbol=commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi
			ptype:=readtypespec()
		else
			exit
		fi
	od
	skipsymbol(rbracksym)
end

proc readtypenameparams(symbol stproc, int ptype)=
!at symbol after ptype
	symbol stname

	checksymbol(namesym)
	stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
	storemode(stproc,ptype,&stname.mode)
	++stproc.nparams
	lex()

	do

		if lx.symbol=eqsym then
			lex()
			stname.code:=readunit()
			stname.moptional:=1
		fi

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi

			if istypestarter() then			!new type
				ptype:=readtypespec()
			fi
			checksymbol(namesym)
			stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
			storemode(stproc,ptype,&stname.mode)
			++stproc.nparams
			lex()
		else
			exit
		esac
	od
	skipsymbol(rbracksym)
end

global proc readrecorddef(int isglobal, symbol d)=
!at 'record' symbol
	int kwd, baseclass, m, startline, caligned
	symbol nameptr

	baseclass:=0
	if d then			!entry from 'type name=record...'
		kwd:=ktypesym
		goto gotname
	fi

	kwd:=lx.symbol

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()

	if lx.symbol=lbracksym then
		lex()
		baseclass:=readtypespec()
		skipsymbol(rbracksym)
	fi

	checkequals()
	lex()

	d:=addsymbol(stcurrproc, nameptr, (kwd=krecordsym|recordid|typeid), isglobal)

	if baseclass then
		if baseclass>0 then serror("baseclass?") fi
		if nbaseclasses>=255 then
				serror("Too many base classes")
		fi
		++nbaseclasses
		storemode(stcurrproc,baseclass,&baseclasstable[nbaseclasses])
		d.baseclassindex:=nbaseclasses
		baseclassdef[nbaseclasses]:=d
	fi

gotname:

	skipsemi()
	startline:=lx.pos

	if kwd=krecordsym then
		m:=readrecordbody(d)
	else
		caligned:=0
		m:=readstructbody(d,caligned)
	fi

	checkend(krecordsym,startline:startline)
end

function readrecordbody(symbol owner)int=
!at first symbol of a class or record body (after 'type T=record',
! or after 'record T ='
!read fields, constants, types, methods.
!create initially anonymous record type, and return type code
!caller will attached to named type as needed.

!int kwd
	symbol oldstcurrproc, e
	int m

	m:=addanontype()

	oldstcurrproc:=stcurrproc
	stcurrproc:=owner

	docase lx.symbol
	when kconstsym then
		readconstdef(0)
	when kvarsym then
		readrecordfields(owner)
	when kfunctionsym,kprocsym then
		readprocdef(0)

	when krecordsym then
		readrecorddef(0, nil)
	when ktypesym then
		lex()
		serror("CLASS TYPE")
	when kendsym,rbracksym,rcurlysym then
		exit
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()
	elsif istypestarter() and nextlx.symbol<>lbracksym then
		readrecordfields(owner)
	else
		serror("Unknown record field decl")
	end

	ttfields[m]:=owner.deflist
	ttlength[m]:=owner.nfields
	ttlower[m]:=1
	ttbasetype[m]:=trecord

	createusertype(owner, m)

	e:=owner.deflist
	while e do
		addgenfield(e)
		e:=e.nextdef
	od

	ttsize[m]:=varsize*owner.nfields

	stcurrproc:=oldstcurrproc

	return m
end

proc readrecordfields(symbol owner)=
!positioned at 'var'; read one line of var defs for a record
	int nvars,offset,index
	symbol d

!	m:=readtypespec(1)
	lex()

	nvars:=0
	index:=owner.nfields
	offset:=index*varsize

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, fieldid, 0)
!		storemode(owner, m, &d.mode)
		d.atfield:=nil

		lex()

		if lx.symbol=atsym then
			lex()
			d.atfield:=readatfield()
!			readatfield()
			d.fieldoffset:=d.atfield.fieldoffset
			d.index:=d.atfield.index
		else
			d.fieldoffset:=offset
			offset+:=varsize
			d.index:=++index
		fi

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od


	if nvars=0 then
		serror("No fields")
	fi
	stcurrproc.nfields+:=nvars
end

function readstructbody(symbol owner, int caligned)int=
	int m, ngroups, nvars, t
	symbol d, e

	m:=addanontype()

	ngroups:=0

	do
		skipsemi()

		case lx.symbol
		when kstructsym then
			++ngroups
			lex()
			addstructflag(owner,structblockid)

		when kunionsym then
			++ngroups
			lex()
			addstructflag(owner,unionblockid)

		when kendsym then
			if nextlx.symbol in [kstructsym, kunionsym] then lex() fi

			if ngroups then
				--ngroups
				lex()
				addstructflag(owner,endblockid)
			else
				exit
			fi

!		when rbracksym then
!			exit

		else
			readpackvars(owner, structfieldid)
!			t:=readtypespec(0)
!
!			nvars:=0
!			while lx.symbol=namesym do
!				++nvars
!				d:=addsymbol(owner, lx.symptr,structfieldid, 0)
!				storemode(owner,t,&d.mode)
!				lex()
!
!				if lx.symbol<>commasym then
!					exit
!				fi
!				lexchecksymbol(namesym)
!			od
!			if nvars=0 then serror("struct decl?") fi
!			owner.nfields:=nvars
		esac
	od

	ttfields[m]:=owner.deflist
	ttlength[m]:=owner.nfields
!CPL =OWNER.NFIELDS

	ttlower[m]:=1
	ttcaligned[m]:=caligned
	ttbasetype[m]:=tstruct

	createusertype(owner, m)

	e:=owner.deflist
	while e do
		case e.nameid
		when structblockid, unionblockid, endblockid then
		else
			addgenfield(e)
		esac
		e:=e.nextdef
	od

	return m
end

proc addstructflag(symbol owner, int id)=
	static int structseqno
	[32]char str

	fprint @str,"$$#",++structseqno

	addsymbol(owner, addglobalname(str),id, 0)
end

proc readprocdef(int isglobal)=
!at 'proc' etc symbol; read proc def or declaration
	int kwd,startline, nparams, shortfun
	unit pcode
	symbol d, oldstcurrproc
	[256]char str

	kwd:=lx.symbol
	shortfun:=lx.subcode
	lexchecksymbol(namesym)
!
	if stcurrproc.nameid in [procid,anonprocid] then
		serror("Nested proc")
	fi

	oldstcurrproc:=stcurrproc			!usually module, but could be a record
	stcurrproc:=d:=addsymbol(stcurrproc,lx.symptr,procid,isglobal)

	addproc(d)

	lex()

	d.mode:=tvoid

	if lx.symbol=lbracksym then		!possible params
		lex()
		if lx.symbol<>rbracksym then
			readparams(d)
		else
			lex()
		fi
	fi

	checkequals()
	lex()

	startline:=lx.pos

	if not shortfun then
		d.code:=readsunit()
		checkend(kwd,startline:startline)
	else
		d.code:=readunit()
		checksymbol(semisym)
!		lex()
	fi

	if eqstring(d.name,"start") then
		currmodule.startfn:=d
	elsif eqstring(d.name,"main") then
		currmodule.mainfn:=d
	fi

	stcurrproc.misfunc:=kwd=kfunctionsym

	stcurrproc:=oldstcurrproc
end

function readatfield:symbol=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	symbol p,d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=stcurrproc.deflist
	while p do
		if eqstring(p.name,d.name) then
			return p
		fi

		p:=p.nextdef
	od
	serror_s("Can't find @ field",d.name)
	return nil
end

function istypestarter:int=
	case lx.symbol
	when stdtypesym, krefsym, kvarsym, kslicesym, lsqsym then
		return 1
	elsif lx.symbol=namesym then
		if nextlx.symbol=namesym then
			return 1
		fi
	esac
	return 0
end

proc readmacrodef(int isglobal)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported

	symbol stmacro, stname, owner

	lexchecksymbol(namesym)

	stmacro:=addsymbol(stcurrproc, lx.symptr, macroid, isglobal)
	owner:=stmacro

	lex()

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=addsymbol(owner,lx.symptr,macroparamid,0)
					stname.firstdupl:=lx.symptr

					lex()
					if lx.symbol=rbracksym then
						exit
					fi
					skipsymbol(commasym)
				else
					serror("macro def params")
				esac
			od
		fi
		lex()
	fi

	checkequals()
	lex()
	stmacro.code:=readunit()
end

function readhostparams(unit lhs,int isfn)unit=
!hostfn name has been read
!lhs is not null when lhs.hostfn(...) has been used
!currently at hostfn symbol
	int fnindex, nargs
	unit p,q

	fnindex:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	q:=readslist(nargs)

	skipsymbol(rbracksym)

	if lhs then
		lhs.nextunit:=q
		q:=lhs
	fi

	p:=createunit1(jcallhost,q)
	p.index:=fnindex

!	poplisttype()

	return p
end

proc pushlisttype(int ltype)=
	if nlisttype>=maxlisttype then
		serror("listtype overflow")
	fi
	listtypestack[++nlisttype]:=listtype
	listtype:=ltype
end

proc poplisttype=
	listtype:=listtypestack[nlisttype--]
end

function readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	locrec loc

	case lx.subcode
	when jcvlineno then
		loc.pm:=currmodule
		setlineno(&loc, lx.sourceoffset)
		return createintunit(loc.lineno)

	when jcvstrlineno then
		loc.pm:=currmodule
		setlineno(&loc, lx.sourceoffset)
		strcpy(str, strint(loc.lineno))

	when jcvmodulename then
		strcpy(&.str,currmodule.name)

	when jcvfilename then
!		strcpy(&.str,modules[currmoduleno].filename)
!		strcpy(&.str,currmodule.name)
		strcpy(&.str,currmodule.filespec)
	when jcvfunction then
		strcpy(&.str,(stcurrproc|stcurrproc.name|"<none>"))
	when jcvdate then
		os_getsystime(&tm)
		fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"
!
	when jcvtime then
		os_getsystime(&tm)
		fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

!	when jcvversion then x:=compilerversion
!	when jcvpclversion then x:=pclversion
	else
		serror("compiler var not impl")
	esac

	return createstringunit(pcm_copyheapstring(&.str))
end

function readpair(int opc)unit=
!should be at '(', but check
!read (a,b) and produce (opc, a, b ) unit

	ref unitrec a,b

	lexchecksymbol(lbracksym)
	lex()
	a:=readexpression()
	skipsymbol(commasym)
	b:=readexpression()

	if lx.symbol=commasym and opc=jmap then			!allow 3rd item
		lex()
		b.nextunit:=readexpression()
	fi
	skipsymbol(rbracksym)
	return createunit2(opc, a,b)
end

global proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

proc readtypedef(int isglobal)=
!at 'type'
	int ptype
	symbol d

	lexchecksymbol(namesym)
	d:=addsymbol(stcurrproc, lx.symptr, typeid, isglobal)

	lexchecksymbol(eqsym)
	lex()	

	if lx.symbol=krecordsym then
		lex()
		d.nameid:=recordid
		readrecorddef(isglobal, d)
		return
	fi

	ptype:=readtypespec(owner:d)

!CPL "USERTYPE",PTYPE,D.NAME,=NTYPES,STRMODE(PTYPE),STRMODE(NTYPES)

	createusertype(d, ptype)
end

function readtypespec(int allowvar=0, symbol owner=nil)int=
!full=1 to allow structdefs

	int flags, arraycode, oldipl
	int a,b,t, startline, caligned
	symbol d
	const maxdim=10
	[maxdim]unit lowerdims,lengthdims
	int ndims
	unit x,lowerx, upperx, lengthx

	case lx.symbol
	when lsqsym then
dolsq:
		lex()
		ndims:=0
		pushlisttype(0)
		do
			lowerx:=lengthx:=nil
			if lx.symbol=rsqsym or lx.symbol=commasym then		![]
			else
				x:=readunit()
				if x.tag=jmakerange then			![a..b] variable
					lowerx:=x.a
					upperx:=x.b
					if lowerx.tag=jintconst and upperx.tag=jintconst then
						lengthx:=createintunit(upperx.value-lowerx.value+1)
					else
						lengthx:=createunit2(jsub,upperx,lowerx)
						lengthx:=createunit2(jadd,lengthx,createintunit(1))
					fi
				else
					case lx.symbol
					when rsqsym,commasym then			![n]
						lengthx:=x
					when colonsym then				!a:n
						lowerx:=x
						lex()
						if not (lx.symbol=commasym or lx.symbol=rsqsym) then
							lengthx:=readunit()
						fi
					esac
				fi
			fi
			lowerdims[++ndims]:=lowerx
			lengthdims[ndims]:=lengthx
			exit when lx.symbol<>commasym
			lex()
		od
		skipsymbol(rsqsym)
		poplisttype()
		t:=readtypespec()

		for i:=ndims downto 1 do
			t:=makeaxtype(t,lowerdims[i],lengthdims[i])
		od
		return t

	when krefsym then
		lex()

		if lx.symbol=stdtypesym and lx.subcode=tvoid then
			lex()
			return makereftype(tvoid,owner)
		else
			return makereftype(readtypespec(),owner)
		fi

	when namesym then
		d:=lx.symptr
		lex()
		if lx.symbol=dotsym then
			lexchecksymbol(namesym)
			t:=newusertypex(d,lx.symptr)
			lex()
			return t
		else
			return newusertypex(d)
		fi

	when stdtypesym then
		case lx.subcode
		when tpackstrz then				!stringz
			lex()
			if lx.symbol=mulsym then
				lex()
				return makestrtype(tpackstrz, readunit())
			else
				return tstringz
			fi

		when tpackstrc then
			lexchecksymbol(mulsym)
			lex()
			return makestrtype(tpackstrc,readunit())

		when tarray then
			lexchecksymbol(lsqsym)
			goto dolsq

		else
			t:=lx.subcode
			case t
			when tint then t:=ti64
			when treal then t:=tr64
			esac

			lex()
			return t
		esac

	when krecordsym then
		if owner=nil then serror("anon record") fi
		lex()
		startline:=lx.pos
		t:=readrecordbody(owner)

		checkend(krecordsym,startline:startline)
		return t

	when kstructsym then
		if owner=nil then serror("anon struct") fi
		lex()
		caligned:=0
		if lx.symbol=kcalignedsym then
			caligned:=1
			lex()
		fi

		startline:=lx.pos
		t:=readstructbody(owner,caligned)

		checkend(kstructsym,startline:startline)
		return t

!	when kvarsym then
!		if not allowvar then
!			serror("var types not allowed")
!		fi
!		lex()
!		if lx.symbol=colonsym then
!			lex()
!			return readtypespec(0)
!		fi
!		return tvar
	when kslicesym then
		lexchecksymbol(lsqsym)
		lexchecksymbol(rsqsym)
		lex()
		t:=makeslicetype(readtypespec(0))

	else
		serror("Type expected")
	esac

	return t
end

proc readparams(symbol stproc)=
!just after '('
	int isbyref,isoptional
	symbol d

!CPL "READPARAMS_NAMES"

!assume one or more params
	isbyref:=isoptional:=0

	do
		if lx.symbol=addrsym then
			++isbyref
			lex()
		fi
		if lx.symbol=questionsym then
			++isoptional
			lex()
		fi
		checksymbol(namesym)
		d:=addsymbol(stproc, lx.symptr, paramid,0)
!		d.mode:=tvar
		++stproc.nparams

		lex()

		if lx.symbol=eqsym then
			isoptional:=1
!			if isbyref+isoptional then serror("Mixed/dupl &/?/=") fi
			lex()
			d.code:=readunit()
		fi

		if isbyref and isoptional then serror("Mixed byref/optional") fi

		d.mbyref:=isbyref
		d.moptional:=isoptional

		isbyref:=isoptional:=0

		if lx.symbol=commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi
		else
			exit
		fi
	od

	skipsymbol(rbracksym)
end

func checkoperator:unit p=
	int opc

	if nextlx.symbol in [commasym, rbracksym, semisym] then
		p:=createunit0(joperator)

		if lx.symbol=specialopsym then
			case lx.subcode
			when '-' then opc:=kneg
			when '[]' then opc:=kindex
			else opc:=kzero
			esac
			p.pclopcode:=opc
		else
			p.pclopcode:=jpclcodes[lx.subcode]
		fi
		lex()
		return p
	fi
	nil
end

func readlambda:unit p=
!at {
	[100]symbol params
	symbol oldstcurrproc, stproc, d
	[20]char str
	int nparams

	case stcurrproc.nameid
	when procid then
	when anonprocid then serror("Nested {}")
	else serror("{} not in fn")
	esac

	oldstcurrproc:=stcurrproc

	print @str,"$F",,++nextlambdaindex
	stproc:=addsymbol(stcurrproc, addnamestr(str), anonprocid, 0)
	stcurrproc:=stproc
	addproc(stproc)

	lex()
	nparams:=0

	if lx.symbol=namesym and nextlx.symbol in [commasym, colonsym] then
		do
			checksymbol(namesym)

			d:=addsymbol(stproc, lx.symptr, paramid, 0)
!*!			d.pindex:=++nparams
			params[++nparams]:=d

			lex()

			if lx.symbol<>commasym then exit fi
			lex()
		od
		checksymbol(colonsym)
		lex()
	fi

	stproc.nparams:=nparams
	stproc.misfunc:=1

!CPL =NPARAMS
!
!CPL "READ LAMBDA",STPROC.NAME, STPROC.MISFUNC

!*!	stproc.isfunc:=1
!*!	getparamoffsets(&params, nparams)

!read body of lambda
	stproc.code:=readsunit()
	skipsymbol(rcurlysym)

!	p:=createunit1(jmakeclosure, createname(stproc))
	p:=createname(stproc)

	stcurrproc:=oldstcurrproc
	return p
end

proc readpackvars(symbol owner, int id)=
!expected to be typed var-decl inside a struct or importdll/lib body
	int t, nvars
	symbol d

	t:=readtypespec(0)

	nvars:=0
	while lx.symbol=namesym do
		++nvars
		d:=addsymbol(owner, lx.symptr,id, 0)
		storemode(owner,t,&d.mode)
		lex()

		if lx.symbol<>commasym then
			exit
		fi
		lexchecksymbol(namesym)
	od
	if nvars=0 then serror("bad decl?") fi
end
