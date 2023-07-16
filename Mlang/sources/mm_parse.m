!M Language Parser

int intabledata=0		!1 means reading table data line; $ gives tabledataname
int inreadprint=0
int inparamlist=0
int inrecordbody=0
int inimportmodule=0
int labelseen=0
ichar tabledataname=nil

const maxprocstack=10
[maxprocstack]symbol procstack
int nprocstack=0

uflagsrec unionstring, unionpend
symbol unionlastvar=nil
symbol dretvar			!part of read-proc: nil, or symptr of retval variable

int try_level=0
int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
int insiderecord=0
int insidedllimport=0

const maxforloops=10
[maxforloops]symbol forindexvars
int nforloops

global filehandle docfile

global function parsemodule(int n)int=
	ref modulerec pm
	symbol owner
	unit p

	initparser()

	pm:=&moduletable[n]
	currmoduleno:=n

	stmodule:=pm.stmodule

	currproc:=stmodule

	stsubprog:=subprogtable[stmodule.moduleno].stsubprog
	currsubprog:=stsubprog

	startlex(pm.fileno)
	owner:=stmodule

	lex()

	pm.modulecode:=readmoduledefs(owner)

	return 1
end

global function readmoduledefs(symbol owner)unit =
!first symbol has been read
	symbol dimport,stimport
	int globalflag,i,callbackflag
	unit ulist,ulistx,p
	ichar name

	globalflag:=module_scope
	callbackflag:=0
	ulist:=ulistx:=nil

	do
		switch lx.symbol
		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode

			if globalflag=export_scope and stmodule.subprogno<>1 then
				globalflag:=program_scope
			fi

			lex()

		when kprocsym,kfunctionsym then	!todo
			readprocdef(owner,globalflag,callbackflag)
			callbackflag:=0
			globalflag:=module_scope

		when stdtypesym,krefsym,kicharsym,ktypeofsym,lsqsym,
			kdictsym,kslicesym then
dovar::
			readvardef(owner,globalflag,0,staticid, 0)
			globalflag:=module_scope

		when kmutsym then
			lex()
			readvardef(owner,globalflag,0,staticid,kmutsym)
			globalflag:=module_scope

		when kletsym then
			lex()
			readvardef(owner,globalflag,0,staticid,kletsym)
			globalflag:=module_scope

		when karraysym then
			lexchecksymbol(lsqsym)
			goto dovar

		when kimportmodulesym then
			readimportmodule(owner)

		when ktypesym then
			readtypedef(owner,globalflag)
			globalflag:=module_scope

		when kconstsym then
			readconstdef(owner,globalflag)
			globalflag:=module_scope

		when kclasssym,krecordsym then
			readclassdef(owner,globalflag)
			globalflag:=module_scope

		when ktabledatasym then
			readtabledef(owner,globalflag)
			globalflag:=module_scope

		when docstringsym then
			adddocstring(lx.svalue)
			lex()

		when semisym then
			lex()

		when eofsym then
			exit

		when kfflangsym then
			if lx.subcode=callbackff then
				callbackflag:=callbackff
				lex()
			else
				serror("fflang?")
			fi

		when kmacrosym then
			readmacrodef(owner,globalflag)
			globalflag:=module_scope

		when kheadersym then
			repeat
				lex()
			until lx.symbol=semisym

		when dotsym then
			SERROR("MODULE/DOT")
		when namesym then
			if istypestarter() then
				goto dovar
			fi
			goto doexec

		else
doexec::
			p:=readunit()
			addlistunit(ulist,ulistx,p)
		end switch
	od

	return ulist
end

proc initparser=

	unless nullunit then
		nullunit:=createunit0(jnull)
	end unless

	unless voidvarunit then
		voidvarunit:=createunit0(jvoidvar)
	end unless

	try_level:=0
	currproc:=nil
	varattribs:=0

	intabledata:=0		!1 means reading table data line; $ gives tabledataname
	inreadprint:=0
	inparamlist:=0
	inrecordbody:=0
	inimportmodule:=0
	ichar tabledataname:=""
	labelseen:=0

	ndollar:=0
end

global proc skipsemi=
	while lx.symbol=semisym do lex() od
end

global function makeblock(unit p)unit=
	if p and p.tag=jblock then return p fi
	return createunit1(jblock,p)
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

function getcurrline:int=
	return lx.pos
end

function checkbegin(int fbrack)int=
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
	int closesym

	skipsemi()

	if lx.symbol=lbracksym and fbrack then
		closesym:=rbracksym
		lex()
	else
		closesym:=kendsym
	fi
	return closesym
end

proc checkbeginend(int closesym,kwd,startline=0)=
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
	skipsemi()
!	if closesym=rbracksym or closesym=rcurlysym then
	if closesym=rbracksym then
		checksymbol(closesym)
	else
		checkend(closesym,kwd,startline:startline)
	fi
	lex()
end

global proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=
!at terminator symbol such as ), eof or 'end'
!check it matches what is expected
!endsym is symbol expected to match
!'end' can have optional keyword following; if present, it must match endkeyword
!Some loop ends (indicated by endkeyword=kforsym, etc) can be also be terminated with 'od'
!endsym should be lbracksym or kendsym
	[100]char str

!exit pointing to current symbol (to 'end', keyword after 'end', or ')')
	if endsym=lx.symbol=rbracksym then
		return
	fi

	if lx.symbol<>kendsym then
		strcpy(&.str,"Bad 'end' ")
	error::

		if startline then
			fprint @(&.str+strlen(&.str))," (from line #)",startline iand 16777215
		fi
		serror(&.str)
	fi

!'end', seen, but check that lx.subcode, if non-zero, is endkeywords or is in that set
	if lx.subcode=0 then					!plain end; for now, that always matches
!		serror("'end' by itself no longer valid")
		return
	fi

	unless (endkwd1 and endkwd1=lx.subcode) or (endkwd2 and endkwd2=lx.subcode) then
		strcpy(&.str,"Mismatched 'end'")
		goto error
	end unless
end

function readvardef(symbol owner,int scope=0,isstatic=0,varid=staticid, k)unit=
!positioned at symbol following 'mut' or 'let', which will at the first symbol of
!the type, or at the first name being defined if there is no type
!k is the keyword symbol used (let/mut), or set to 0 if no keyword has been used,
!then mut is assumed

!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]

	unit ulist,ulistx, p
	int nvars,m, initcode
	symbol stname

	ulist:=ulistx:=nil

	if istypestarter() then
		m:=readtypespec(owner)
	elsif k then
		m:=tauto
!		m:=tvariant
	else
		serror("Readvar?")
	fi

	nvars:=0
	while lx.symbol=namesym do

		++nvars
		stname:=getduplnameptr(owner,lx.symptr,varid)

		stname.scope:=scope

		stname.isstatic:=isstatic
		stname.islet:=(k=kletsym)
		if varid=dllvarid then
			stname.isimport:=1
		fi

		adddef(owner,stname)
		if varid=staticid then
			addstatic(stname)
		fi

		lex()

		if lx.symbol=colonsym then
			if m<>tauto then serror("Mixed var T x:T") fi
			lex()
			m:=readtypespec(owner)
		fi

		storemode(owner,m,stname.mode)

		if lx.symbol in [assignsym,eqsym,deepcopysym] then

!			initcode:=case lx.symbol when eqsym then 1 when assignsym then 2 else 3 esac
			case lx.symbol
			when eqsym then initcode:=1
			when assignsym then initcode:=2
			else initcode:=3
			esac

			if lx.symbol<>eqsym then
				if varid=staticid then
					serror("Non-variants can't use :=")
					if owner.nameid=procid then
						serror("Can't use := for statics inside procs")
					fi
					
				fi
			else
				if varid=frameid then
					serror("Need 'static' for '='")
					addstatic(stname)
				fi
			fi
			lex()
			if lx.symbol=kemptysym then
				lex()
				if varid<>frameid then serror("empty: not frame") fi
				p:=createunit1(jempty,createname(stname))
				addlistunit(ulist,ulistx,p)
			else
				stname.code:=readunit()
				stname.equals:=initcode
				if varid=frameid then
					p:=createunit2(jassign,createname(stname),stname.code)
					p.initlet:=1
					addlistunit(ulist,ulistx,p)
				fi
			fi

		elsif lx.symbol=atsym then
			if k=kletsym then serror("let@") fi
			lex()
			stname.atvar:=1
			stname.equivvar:=readunit()
		elsif k=kletsym then
			serror("let needs :=/=")
		fi

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

proc readconstdef(symbol owner,int scope=0)=
!at 'const' symbol
	int nconsts,deft,m
	symbol stname

	lex()

	nconsts:=0

	if istypestarter() then
		deft:=readtypespec(owner)
	else
		deft:=tauto
	fi

	while lx.symbol=namesym do
		stname:=getduplnameptr(owner,lx.symptr,constid)

		lex()

		checkequals()
		lex()
		stname.code:=readconstexpr(1)

		m:=deft

		storemode(owner,m,stname.mode)
		++nconsts

		stname.scope:=scope

		adddef(owner,stname)
		if scope=export_scope and stname.name^<>'$' then
			addexpconst(stname)
		fi

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nconsts=0 then
		serror("No consts declared")
	fi

end

function readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following::
! (x)		simple expression
! ()		list with no elements
! (x,)		list with one element
! (x,x,...)		list
! (x|x|x])		if then else fi
! (x|x,... |x])	select then else end

!return positioned at symbol following closing ")"
!listtag is jmakelist or jmakearray if 'array' was used

	unit ulist,ulistx, p,q,r, plower
	int oldirp,length, usecomma

	lex()					!first symbol of first expression
	ulist:=ulistx:=nil
	plower:=nil
	length:=0

	if lx.symbol=atsym then			!lwb override
		lex()
		oldirp:=inreadprint
		inreadprint:=1
		plower:=readunit()

		inreadprint:=oldirp
		checksymbol(colonsym)
		lex()

	elsif lx.symbol=intconstsym and nextlx.symbol=colonsym then
		plower:=createconstunit(lx.value,lx.subcode)
		plower.istrueconst:=1
		lex()
		lex()

	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then	!operator constant
		p:=createunit0(joperator)
		p.opcindex:=lx.subcode
		lex()
		lex()
		return p
	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=assignsym then	!operator:= constant
		p:=createunit0(joperator)
		p.pclop:=symbolgentoops[lx.symbol]
		lex()			!read :=
		lexchecksymbol(rbracksym)
		lex()
		return p
	fi

!check symbol after "("
	case lx.symbol
	when rbracksym then			!empty list
		lex()
		p:=createunit0(jmakelist)
		p.b:=plower
		p.length:=0
		return p
	else					!assume normal expression follows
		p:=readunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()

		return p

	when commasym then			!separate by comma or implicit newline
		usecomma:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(jmakelist,p)
			p.length:=1
			p.b:=plower
			return p
		fi
docomma::						!entry from implicit newline
		length:=1

!must be regular list
		ulist:=ulistx:=p

		if usecomma then
			repeat
				lex()							!skip comma
				if lx.symbol=rbracksym then		!allow ,) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(",, null expr not allowed")
				fi
				addlistunit(ulist,ulistx,readunit())
				++length
				skipsemi()
			until lx.symbol<>commasym
		else

			repeat
				skipsemi()
				if lx.symbol=rbracksym then		!allow ,) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(",, null expr not allowed")
				fi
				addlistunit(ulist,ulistx,readunit())
				++length
			until lx.symbol<>semisym
		fi

		checksymbol(rbracksym)
		lex()
		p:=createunit1(jmakelist,ulist)
		p.length:=length
		p.b:=plower
		return p

	when barsym then			!ifx/selectx expression; p is selector expression
		lex()
		q:=readunit()
		case lx.symbol
		when barsym then		!(a|b|c)
			lex()
			r:=readsunit()
			checksymbol(rbracksym)
			lex()
			return createunit3(jif,fixcond(p),q,r)
		when rbracksym then
			lex()
			return createunit3(jif,fixcond(p),q,nil)

		esac

!assume selectx expression
		addlistunit(ulist,ulistx,q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist,ulistx,readunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readunit()
		checksymbol(rbracksym)
		lex()
		return createunit3(jselect,p,ulist,r)

	when semisym then
		if lx.subcode=1 then
			usecomma:=0
			goto docomma
		fi
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			fi
			addlistunit(ulist,ulistx,readunit())
!			skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
		until lx.symbol<>semisym
		checksymbol(rbracksym)
		lex()

		return makeblock(ulist)


	else
		serror("(x ...")
	esac
	return nil
end

proc addlistparam(ref symbol ulist,ulistx,symbol p)=
!add unit p to unit structure ulist,^ulistx  which can be null
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^.nextparam:=p
	fi
	ulistx^:=p			!update end-of-list pointer
end

function readcast:unit=
!also reads standalone type value
!t<>tvoid means already has ty[e
	unit p
	int opc,t

	t:=readtypespec(currproc)

	case lx.symbol
	when rbracksym then
		p:=createunit0(jtypeconst)
		p.mode:=ttype
		p.value:=t
		return p

	when atsym then
		opc:=jtypepun
		lex()
	when dotsym then			!allow T.type, but also just T (followed by . which
								!might be T.minvalue etc)
		if nextlx.symbol=ktypesym then
			lex()
			p:=createunit0(jtypeconst)
			p.value:=t
			p.mode:=ttype
			lex()
		else					!leave dot to be processed by caller
			p:=createunit0(jtypeconst)
			p.value:=t
		fi
		return p
	else
		opc:=jconvert
	esac

	checksymbol(lbracksym)
	lex()
	p:=readunit()
	checksymbol(rbracksym)
	lex()

	p:=createunit1(opc,p)
	storemode(currproc,t,p.convmode)
	return p
end

function readopc:unit=
!op sym seen just before a term
	unit p,q,r
	int tag,opc,firstsym

	firstsym:=lx.symbol

	case lx.symbol
	when mathsopsym then
		tag:=junary
		opc:=lx.subcode
	when maths2opsym then
		tag:=jbin
		opc:=lx.subcode
	else
		tag:=junary
		opc:=symbolgenops[firstsym]
	esac

	lex()
	case firstsym
	when addsym then			!ignore +
		return readterm2()
	when subsym then			!convert minus to negate
		opc:=kneg
	when minsym,maxsym,maths2opsym then
		p:=readterm2()

		if p.tag=jmakelist then
			if p.length<>2 then serror("Needs (x,y)") fi
			q:=p.a
			r:=q.nextunit
			q.nextunit:=nil
			p:=createunit2(jbin,q,r)
			p.pclop:=opc
			return p
		else		!assume single operand
			SERROR("READOPC/SINGLE OPND?")
			return createunit1(opc,p)

		fi
	else
		if symboloptypes[firstsym]=bin_op then
			serror("Can't be used as unary op")
		fi

	esac

	if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
		lex()
		tag:=junaryto
		case firstsym
		when subsym then
			opc:=knegto
		else
			opc:=symbolgentoops[firstsym]
			if opc=0 then
				serror("op:= not available")
			fi
		esac
	fi

	p:=createunit1(tag,q:=readterm2())

	p.pclop:=opc

	if q.tag=jmakelist then
		serror("Too many opnds")
	fi

	return p
end

function readsprint:unit=
	int oldinreadprint,opc,isfprint
	unit pformat, pdev, printlist, printlistx, p

	oldinreadprint:=inreadprint
	inreadprint:=1
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

	finish::
	lex()
	inreadprint:=oldinreadprint
	if (opc=jprint or opc=jfprint) and printlist=nil then
		serror("No print items")
	fi

	if isfprint then
		if pformat.tag=jnull then
			serror("No fmt str")
		fi
		return createunit3(opc,pdev,pformat,printlist)
	else
		return createunit2(opc,pdev,printlist)
	fi
end

function readsread:unit=
!Need to check what sread/sreadln actually mean. I think they are actually supposed
!to work an item at a time::
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
	int oldinreadprint,opc
	unit pformat,pdev,p, readlist,readlistx

	oldinreadprint:=inreadprint
	inreadprint:=1
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

	finish::
	lex()
	inreadprint:=oldinreadprint
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit2(opc,pdev,readlist)
end

function readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	unit p
	ref modulerec currmodule:=&moduletable[currmoduleno]

	switch lx.subcode
	when jcvnil then
		p:=createconstunit(0,tref)
		lex()
		return p

	when jcvpi then
!		p:=createconstunit(int64@(3.14159'265358'979'3238'4626'433'832),treal)
		p:=createconstunit(int64@(pi),treal)
		lex()
		return p

	when jcvinfinity then
		p:=createconstunit(int64@(infinity),treal)
		lex()
		return p

	when jcvlineno then

		p:=createunit0(jcvlineno)
		lex()
		return p

	when jcvstrlineno then
		getstrint(getlineno(lx.pos),&.str)

	when jcvmodulename then
		strcpy(str,stmodule.name)

	when jcvfilename then

		strcpy(str,sourcefilepaths[currmodule.fileno])

	when jcvfunction then
		strcpy(&.str,currproc.name)

	when jcvdate then
		os_getsystime(&tm)
		fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

	when jcvtime then
		os_getsystime(&tm)
		fprint @&.str,"#:#:#",tm.hour:"z2",tm.minute:"z2",tm.second:"z2"

	when jcvtargetbits then
		lex()
		return createconstunit(targetbits,tint)
	when jcvtargetsize then
		lex()
		return createconstunit(targetsize,tint)
	when jcvtargetcode then
		strcpy(&.str,"wx64")

	when jcvversion then
		strcpy(&.str,"Compiler:BX Experimental")

	when jcvtrue,jcvfalse then
		p:=createconstunit(lx.subcode=jcvtrue,tbool64)
		lex()
		return p
	
	else
		serror_s("compiler var not impl: #",jtagnames[lx.subcode])
	end switch
	lex()

	return createstringconstunit(pcm_copyheapstring(&.str),-1)
end

function readcastx:unit=
!explicit cast using syntax::
! cast(expr)
! cast(expr,type)
! cast@(expr,type)
!at 'cast'
	int opc,m
	unit p

	lex()
	opc:=jconvert
	if lx.symbol=atsym then
		opc:=jtypepun
		lex()
	fi
	checksymbol(lbracksym)
	lex()
	m:=tvoid
	p:=readunit()
	if lx.symbol<>commasym then
		if opc=jtypepun then serror("@ type missing") fi
		opc:=jautocast
	else
		lex()
		m:=readtypespec(currproc)
	fi
	checksymbol(rbracksym)
	lex()

	p:=createunit1(opc,p)
	storemode(currproc,m,p.convmode)

	return p
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]
		serror(&.str)
	fi
end

global proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

global function readtypespec(symbol owner,int typedefx=0)int=
!at initial symbol of a type, or where type is expected
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either::
! moderec	Used when called from readtypedef. This is then filled in with the
!		details of the new mode, and the new version is returned
! nil		Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using int16 etc
	symbol d,e
	int t,kwd,fflang,sltype,w
	unit x,pupper,plx
	unit dim,length
	const maxdim=30
	[maxdim]unit dims
	int ndims,i,n,k

	case lx.symbol
	when lsqsym then		!array bounds
arraybounds::
		lex()

		ndims:=0
		inreadprint:=1
		do
			length:=nil				!both bounds unspecified
			if lx.symbol=rsqsym or lx.symbol=commasym then		![]
				dim:=nil
			else
				dim:=readunit()
				case lx.symbol
				when rsqsym,commasym then			![n]
				when colonsym then				!a:n
					lex()
					if not (lx.symbol=commasym or lx.symbol=rsqsym) then	!lower:length
						length:=readunit()
						dim:=createunit2(jkeyvalue,dim,length)
					else													!lower::
						dim:=createunit1(jkeyvalue,dim)
					fi
				esac
			fi
			if ndims>=maxdim then serror("Too many array dims") fi
			dims[++ndims]:=dim
			exit when lx.symbol<>commasym
			lex()
		od
		inreadprint:=0
		checksymbol(rsqsym)
		lex()
		t:=readtypespec(owner)

		for i:=ndims downto 1 do
			t:=createarraymode(owner,t,dims[i],(i=1|typedefx|0))
		od
		return t

	when stdtypesym then
		t:=lx.subcode
		lex()

	when namesym then
		d:=lx.symptr
		lex()

		if lx.symbol=dotsym then
			lexchecksymbol(namesym)
			t:=newtypename(d,lx.symptr)
			lex()
		else
			t:=newtypename(nil,d)
		fi

	when krecordsym,kstructsym then
		serror("Use 'record name =' syntax")

	when kunionsym then
		serror("Top-level union not allowed")

	when krefsym then		!ref T
		fflang:=0
	retry::

		lex()
		if lx.symbol=ktosym then lex() fi

		case lx.symbol
		when kprocsym,kfunctionsym then	!function pointer being created
			t:=readrefproc(owner,typedefx,fflang)

		when kfflangsym then
			fflang:=lx.subcode
			goto retry
!*!	elsif lx.symbol=namesym and lx.subcode=asmopcodesym and lx.symptr.index=m_label then
!*!		t:=createrefmode(owner,tlabel,typedefx)
		elsif lx.symbol=stdtypesym then
			case lx.subcode
			when tc8 then
				t:=trefchar
				if typedefx then tttarget[typedefx]:=tc8 fi
			else
				goto readtarget
			esac

			lex()
		else						!assume normal type
	readtarget::
			t:=readtypespec(owner)
			t:=createrefmode(owner,t,typedefx)
		esac

	when kicharsym then
		lex()
		t:=trefchar
		if typedefx then tttarget[typedefx]:=tc8 fi

	when ktypeofsym then
		lexchecksymbol(lbracksym)
		lexchecksymbol(namesym)

		t:=newtypename(cast(lx.symptr),nil)
		lexchecksymbol(rbracksym)
		lex()

	when kslicesym then
		t:=readslicetype(owner,lx.subcode,typedefx)

	when karraysym then
		lexchecksymbol(lsqsym)
		goto arraybounds
	else
		serror("Bad type starter")
	esac

	if typedefx then			!assume a simple alias
		ttbasetype[typedefx]:=ttbasetype[t]
	fi

	return t
end

function readslicetype(symbol owner, int slicetype, typedefx)int=
!positioned at 'slice'
!dim is nil, or lower-bound expression
	unit plower
	int t

	lexchecksymbol(lsqsym)
	lex()
	if lx.symbol<>rsqsym then
		inreadprint:=1
		plower:=readunit()
		inreadprint:=0
		checksymbol(colonsym)
		lexchecksymbol(rsqsym)
	else
		plower:=nil
	fi
	lex()
	t:=readtypespec(owner,typedefx)

	return createslicemode(owner,slicetype,t,plower,typedefx)
end

function readslist(int iscall=0,donulls)unit=
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

	skipsemi()
	if lx.symbol=rbracksym then		!empty list
		return ulist
	fi

	oldinparamlist:=inparamlist
	inparamlist:=iscall

	do
		skipsemi()
		case lx.symbol
		when commasym then
			if donulls then
				addlistunit(ulist,ulistx,createunit0(jnull))
			else
				serror("null comma expr not allowed")
			fi
			lex()
		when rbracksym then
			if donulls then
				addlistunit(ulist,ulistx,nullunit)
			fi
			exit
		else
			addlistunit(ulist,ulistx,readunit())
			if lx.symbol in [commasym,semisym] then
				lex()
				if lx.symbol=rbracksym then
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
	inparamlist:=oldinparamlist

	return ulist
end

function readindex(unit p,int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is::
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q,plower,pupper

	lex()

	if not dot then
		case lx.symbol
		when rsqsym then
	fullslice::
			lex()
			plower:=createunit1(junary,duplunit(p))
			plower.pclop:=klwb
			pupper:=createunit1(junary,duplunit(p))
			pupper.pclop:=kupb
			p:=createunit2(jslice, p, createunit2(jmakerange,plower, pupper))
			return p
		when rangesym,colonsym then
			lexchecksymbol(rsqsym)
			goto fullslice
		esac
	fi

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		fi
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

		if q.tag=jmakerange then		!convert into a discrete slice
			p:=createunit2((dot|jdotslice|jslice),p,q)
		else
			p:=createunit2((dot|jdotindex|jindex),p,q)
		fi

		exit when lx.symbol<>commasym
		lex()
	od
	checksymbol(rsqsym)
	lex()
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
		switch lx.symbol
		when lsqsym then
			p:=readindex(p,1)
		when namesym then
			p:=createunit2(jdot,p,createname(lx.symptr))
			lex()
		when propsym then
	doprop::
			p:=createunit1(junary,p)
			p.pclop:=lx.subcode
			lex()
		when bitfieldsym then
			p:=createunit1(jbitfield,p)
			p.bfcode:=lx.subcode
			lex()
		when ktypesym then			!.type, convert to .gettype
			case p.tag
			when jtypeconst then			!int.type=>int

			else
				p:=createunit1(jtypeof,p)
			esac
			lex()

		when maxsym then
			lx.subcode:=kmaxvalue
			goto doprop

		when minsym then
			lx.subcode:=kminvalue
			goto doprop
		when stdtypesym then
			if p.tag=jtypeconst and lx.subcode=trange then
				q:=createunit2(jmakerange,
					createunit1(junary,p),
					createunit1(junary,p))
				q.a.pclop:=kminvalue
				q.b.pclop:=kmaxvalue
			else
				error
			fi
			lex()
			p:=q

		else
	error::
			serror("Unknown dot suffix")
		end switch
	od
	return p
end

function readconstexpr(int needconst=1)unit=
	return readunit()
end

function readconstint:int=
!read expression that must yield a constant int value *now*; return value
	int64 x

!keep it simple for now
	if lx.symbol=intconstsym then
		x:=lx.value
		lex()
		return x
	elsif lx.symbol=subsym then
		lex()
		if lx.symbol=intconstsym then
			x:=lx.value
			lex()
			return -x
		fi
	fi

!later can only arbitrary expressions, provided they can be evaluated in this pass
	serror("Can't do complex expr")
	return 0
end

proc readprocdef(symbol procowner,int scope,fflang=0)=
!at 'proc' etc symbol; read proc def or declaration
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
	int kwd,startline,closesym, shortfun
	symbol stproc,q,stname

	kwd:=lx.symbol
	shortfun:=lx.subcode=1
	nforloops:=0

	assemmode:=1
	stproc:=readprocdecl(procowner,scope,fflang)
	assemmode:=0
	checkequals()

	lex()

	startline:=getcurrline()

	if not shortfun then
		closesym:=checkbegin(0)
	fi

	pushproc(stproc)
	nextavindex:=0

	IF DRETVAR THEN
		stname:=getduplnameptr(stproc,dretvar,frameid)
		storemode(procowner,stproc.mode,stname.mode)
		adddef(stproc,stname)
	fi

	addtoproclist(stproc)

	if shortfun then
		stproc.code:=readunit()
		checksymbol(semisym)
		lex()
	else
		stproc.code:=readsunit()
		checkbeginend(closesym,kwd,startline)
	fi

	stproc.code:=makeblock(stproc.code)

	if ndocstrings and docfile and stproc.scope>=program_scope then
		println @docfile,"proc",stproc.name
		for i to ndocstrings do
			print @docfile,docstrings[i]
			pcm_free(docstrings[i],strlen(docstrings[i]+1))
		od
		println @docfile

		ndocstrings:=0
	fi

	popproc()
end

global function readprocdecl(symbol procowner,int scope,fflang)symbol=
!at 'proc'  or 'function' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

	int kwd,varparams,try_level, nparams, nretvalues, isthreaded
	[maxtuplesize]int retmodes
!	int prettype@&retmodes

	ichar metadata, truename
	symbol pequiv, stproc, owner, paramlist,nameptr

	kwd:=lx.symbol				!remember keyword
	isthreaded:=lx.subcode=2

	pequiv:=nil
	metadata:=""
	truename:=nil
	varparams:=0
	try_level:=0

	lex()

	if lx.symbol=stringconstsym then		!assume dll truename
		truename:=pcm_copyheapstring(lx.svalue)
		convlcstring(lx.svalue)
		lx.symptr:=addnamestr(lx.svalue)
	else
		checksymbol(namesym)
	fi

	nameptr:=lx.symptr

	stproc:=getduplnameptr(procowner,nameptr,(insidedllimport|dllprocid|procid))
	if insidedllimport then scope:=subprog_scope fi
	stproc.isthreaded:=isthreaded

	if truename then
		stproc.truename:=truename
	fi

	adddef(procowner,stproc)
	if stproc.nameid=dllprocid then
		stproc.isimport:=1
	fi

	owner:=stproc
	pushproc(stproc)

	lex()
	if lx.symbol=mulsym then
		stproc.ishandler:=1
		lex()
	fi

	paramlist:=nil
	retmodes[1]:=tvoid
	nparams:=0
	nretvalues:=0

	nretvalues:=0
	if lx.symbol=lbracksym then		!possible params
		lex()
		if lx.symbol<>rbracksym then
			paramlist:=readparams(procowner,stproc,fflang,varparams,nparams)
			checksymbol(rbracksym)
		fi
		lex()

		if lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(owner,retmodes)
		elsif typestarterset[lx.symbol] or lx.symbol=namesym then
			nretvalues:=readreturntype(owner,retmodes)
		fi
	elsif lx.symbol=colonsym or lx.symbol=sendtosym then
		lex()
		nretvalues:=readreturntype(owner,retmodes)
	fi

	dretvar:=nil
	if nretvalues=1 then
		if lx.symbol=namesym then
			dretvar:=lx.symptr
			lex()
		fi
	fi

	unless nretvalues or (kwd<>kfunctionsym) then		!function: no result given
		serror("Function needs ret type")
	end unless

	if nretvalues and (kwd<>kfunctionsym) then		!proc: result given
		serror("Proc can't return value")
	fi

	stproc.paramlist:=paramlist
	stproc.nretvalues:=nretvalues

	case nretvalues
	when 0 then
		stproc.mode:=tvoid
	when 1 then
		storemode(procowner,retmodes[1],stproc.mode)
	else
		stproc.mode:=createtuplemode(procowner,retmodes,nretvalues,0)
	esac

	if lx.symbol=atsym then			!equivalence
		lexchecksymbol(namesym)
	SERROR("READPROCDEF @")
		lex()
		stproc.atvar:=1
	fi

	stproc.code:=nil

	case fflang
	when clangff,windowsff then
	else			!assume this language
		case procowner.nameid
		when moduleid then
		when dllmoduleid then
			serror("Need FF specifier")
		esac
	esac
	stproc.scope:=scope
	stproc.varparams:=varparams
	stproc.fflang:=fflang

	if procowner=stmodule then
		if stproc.namelen=5 and eqstring(stproc.name,"start") then
			moduletable[stmodule.moduleno].ststart:=stproc
			stproc.scope:=subprog_scope
		elsif stproc.namelen=4 and eqstring(stproc.name,"main") then
			moduletable[stmodule.moduleno].stmain:=stproc
			if stmodule.moduleno=mainmoduleno then
				stproc.scope:=export_scope
			fi
		fi
	fi

	popproc()

	return stproc
end

function readparams(symbol procowner,owner,int fflang,&varparams,&nparams)symbol=			!READPARAMS
!positioned at first symbol after '('; this is not ')'
!read list of params, return that list
!syntax is a list of names and/or types
!each param can optionally be followed by a default value
!finish pointing at ")"
	symbol stlist, stlistx, stname
	int parammode, pmode, m, isoptional,types

	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	parammode:=var_param
	types:=0

	if fflang=0 then fflang:=mlangff fi

	if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
		types:=1
	fi

	do										!expect type of name at start of loop
		parammode:=var_param
		isoptional:=0

		if types or istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
gotmode::

			if nparams=0 and lx.symbol in [commasym, rbracksym] then
				do
					[32]char str
					++nparams
					str[1]:='$'; str[2]:=0
					strcat(str, strint(nparams))
					stname:=getduplnameptr(owner,addnamestr(&.str),paramid)
					adddef(owner,stname)
!STNAME.ISLET:=1

					storemode(owner,pmode,stname.mode)
					stname.parammode:=parammode
					addlistparam(&stlist,&stlistx,stname)

					case lx.symbol
					when rbracksym then
						exit
					esac

					checksymbol(commasym)
					lex()
					if lx.symbol=ellipsissym then
						varparams:=1
						lex()
						exit
					fi

					pmode:=readtypespec(procowner)
				od
				return stlist
			fi

		elsif pmode=tvoid then
			serror("Type expected")
		fi

		case lx.symbol
		when insym then
			parammode:=in_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when koutsym,addrsym then
			parammode:=out_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when questionsym then
			isoptional:=1
			lex()
		when ellipsissym then
			varparams:=1
			lex()
			return stlist
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner,lx.symptr,paramid)
		adddef(owner,stname)
!STNAME.ISLET:=1
		lex()

		if parammode=out_param then
			m:=createrefmode(procowner,pmode)
		else
			m:=pmode
		fi

		storemode(owner,m,stname.mode)
		stname.parammode:=parammode
		stname.optional:=isoptional
		addlistparam(&stlist,&stlistx,stname)

		case lx.symbol
		when assignsym, eqsym then
			lex()
			stname.code:=readunit()
			stname.equals:=1
			stname.optional:=1
		esac

		case lx.symbol
		when commasym then
			lex()
		when rbracksym then
			exit
		else
			serror("nameparams1")
		esac
	od

return stlist
end

function readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond
	unit q

	switch lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif,fixcond(readunit()),createunit1(jblock,p))
	when kunlesssym then
		lex()
		q:=createunit1(jnotl,fixcond(readunit()))
		q.pclop:=knotl
		return createunit2(jif, q,createunit1(jblock,p))
	else
		return p
	end switch
end

function readif:unit=
!at 'if'
	int pos1, kwd, pos2
	unit clist,clistx, plist,plistx, pelse, p, pelsif

	pos1:=lx.pos
	kwd:=lx.symbol			!in case coming from elsecase etc

	clist:=clistx:=plist:=plistx:=pelse:=nil

	repeat
		lex()
		addlistunit(clist,clistx, fixcond(readsunit()))

		skipsemi()
		checksymbol(kthensym)
		lex()

		addlistunit(plist,plistx, readsunit())
		skipsemi()

	until lx.symbol<>kelsifsym

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym,kwd,0)
		lex()
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		checkend(kendsym,kwd,0)
		lex()
	esac

	p:=createunit3(jif,clist, plist,pelse)
	p.pos:=pos1
	return p
end

function readgoto(int gototag=jgoto)unit=
	symbol d
	unit p

	if lx.subcode=1 then		!go used
		lexchecksymbol(ktosym)
	fi
	lex()

	return readcondsuffix(createunit1(gototag,readunit()))
end

function readunless:unit=
	int pos
	unit pcond, pthen, pelse, p,q
	pos:=lx.pos
	lex()
	pcond:=fixcond(readsunit())
	checksymbol(kthensym)
	lex()

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		PELSE:=NIL
	fi
	checkend(kendsym,kunlesssym)
	lex()
	p:=createunit3(jif,q:=createunit1(jnotl,pcond),pthen,pelse)
	q.pclop:=knotl
	p.pos:=pos
	return p
end

function readswitchcase:unit=
	int pos1, kwd, opc, pos2,rangeused, nwhen
	unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

	pos1:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc

	opc:=lx.subcode			!pick up tag: kcase etc
!
	lex()

	skipsemi()
	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		fi
		pexpr:=nil
	else
		pexpr:=readsunit()		!index expression
	fi

	pwhenlist:=pwhenlistx:=nil
	rangeused:=0
	nwhen:=0

	skipsemi()
	while lx.symbol=kwhensym do	!read list of when-then pairs
		pos2:=lx.pos
		lex()
		pwhen:=pwhenx:=nil
		do
			p:=readunit()
			++nwhen
			p.pos:=pos2
			if p.tag=jmakerange then rangeused:=1 fi
			addlistunit(pwhen,pwhenx,p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>sendtosym then
			checksymbol(kthensym)
		fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen,pwhen,pthen)
		pwhenthen.pos:=pos2
		addlistunit(pwhenlist,pwhenlistx,pwhenthen)
	od

	if opc=jswitch and not rangeused then
		if nwhen<=8 then
			opc:=jcase
		fi
	fi

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()

		checkend(kendsym,kwd)
		lex()
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym,kwd)
		lex()
	esac

	p:=createunit3(opc,pexpr,pwhenlist,pelse)
	p.pos:=pos1
	return p
end

function readstop:unit=
	unit p
	int i
	lex()
	if exprstarter[lx.symbol] then
		p:=createunit1(jstop,readunit())
	else
		p:=createunit0(jstop)
	fi
	return readcondsuffix(p)
end

function readreturn:unit=
	unit p,q,r

	lex()
	if exprstarter[lx.symbol] then
		q:=readunit()
		p:=createunit1(jreturn,q)
		p.length:=1
	else
		p:=createunit0(jreturn)
		p.length:=0
	fi

	return readcondsuffix(p)
end

function readdo:unit=
	unit p
	int pos
	pos:=lx.pos
	lex()
	p:=readsunit()
	checkend(kendsym,kdosym)
	lex()
	p:=createunit1(jdo,p)
	p.pos:=pos
	return p
end

function readto:unit=
	int pos,id
	unit p, pcount, pbody

	pos:=lx.pos
	lex()

	pcount:=readunit()

	checksymbol(kdosym)
	lex()
	pbody:=readsunit()
	checkend(kendsym,ktosym,kdosym)
	lex()
	id:=frameid
	if currproc.nameid<>procid then id:=staticid fi

	p:=createunit3(jto,pcount,pbody,createname(getavname(currproc,id)))
	p.pos:=pos
	return p
end

function readwhile:unit=
	int pos
	unit pcond, pbody, pincr, p
	pos:=lx.pos
	lex()

	pcond:=fixcond(readsunit(1))
	pincr:=nil

	if lx.symbol=commasym then
		lex()
		pincr:=readsunit(1)
	fi

	checksymbol(kdosym)
	lex()
	pbody:=readsunit()

	if lx.symbol=kstepsym then
		if pincr then serror("Double incr") fi
		lex()
		pincr:=readsunit()
	fi

	checkend(kendsym,kwhilesym,kdosym)
	lex()

	p:=createunit3(jwhile,pcond,pbody,pincr)
	p.pos:=pos

	return p
end

function readrepeat:unit=
	int pos
	unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	checksymbol(kuntilsym)
	lex()
	pcond:=fixcond(readunit())
	p:=createunit2(jrepeat,pbody,pcond)
	p.pos:=pos

	return p
end

function readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode

	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name,"all") then
		lex()
		p:=createunit1(opc,createconstunit(0,tint))

	elsif exprstarter[lx.symbol] then
		p:=createunit1(opc,readconstexpr(1))
	else
		p:=createunit1(opc,createconstunit(1,tint))
	fi
	return readcondsuffix(p)
end

function readprint:unit=
	int oldinreadprint, opc, isfprint, fshowname, length
	unit pformat, pdev, printlist,printlistx, p,q
	ref strbuffer expr

	ichar s

	oldinreadprint:=inreadprint
	inreadprint:=1
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

	if not exprstarter[lx.symbol] then
		goto finish
	fi

	do
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(printlist,printlistx, createunit0(jnogap))
		when dollarsym then		!assume extra comma, meaning nogap
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

				addlistunit(printlist,printlistx,q:=createstringconstunit(s,expr.length))
			fi
			addlistunit(printlist,printlistx,p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish::
	inreadprint:=oldinreadprint
	if opc=jprint and printlist=nil then
		serror("No print items")
	fi
	if opc=jfprint and printlist=nil and pformat=nil then
		serror("No print items")
	fi

	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		return createunit3(opc,pdev,pformat,printlist)
	else
		return createunit2(opc,pdev,printlist)
	fi
end

function readread:unit=
	int oldinreadprint,opc
	unit pformat, pdev, readlist, readlistx, p, pread

	oldinreadprint:=inreadprint
	inreadprint:=1
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
		if lx.symbol=commasym then lex() fi
	fi

	if opc=jreadln then
		addlistunit(readlist,readlistx,createunit1(jreadln,pdev))
	fi

	if not exprstarter[lx.symbol] then
		goto finish
	fi

	do
		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			pformat:=readunit()
		else
			pformat:=nil
		fi

		pread:=createunit1(jread,pformat)

!

		p:=createunit2(jassign,p,pread)

		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish::
	inreadprint:=oldinreadprint
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit1(jblock,readlist)
end

function readfor:unit=
!on 'for'; syntax is::
! for [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for var[,var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/od
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/od

!AV codes:
!	I	loop index, always i64; will be 'i' (declared or not declared) or autovar
!	L	forall local variable; will be 'x' (declared or not declared); type is variable

	int pos, opc, kwd
	unit pindex, plocal				!for index; for index,local
	unit pfrom, pto, pstep, ptoinit	!for INDEX:=FROM to/downto TO [by STEP]/ INDEX in FROM..TO
	unit plist, passign				!for INDEX in/inrev LIST (also LIST.BOUNDS)
	unit pcond, pbody, pelse
	unit p
!
	pos:=lx.pos
	lex()						!skip 'for' kwd

	plocal:=nil
	ptoinit:=nil
	pindex:=readname()

	if nforloops>=maxforloops then
		serror("Too many for-loops")
	fi
	for i to nforloops do
		if forindexvars[i]=pindex.def then
			serror("Re-using nested loop index")
		fi
	od
	forindexvars[++nforloops]:=pindex.def

	if lx.symbol=commasym then
		lex()
		plocal:=readname()
	fi

	opc:=jforup
	pstep:=nil
	pcond:=nil

	if lx.symbol in [insym, inrevsym] then				!assume forall
		if lx.symbol=jinrev then
			opc:=jfordown				!tentative; may be changed to forall
		fi
		lex()

		plist:=readunit()

		if plist.tag=junary and plist.pclop=kbounds then
			pfrom:=getrangelwbunit(plist.a)
			pto:=getrangeupbunit(plist.a)
		elsif plist.tag=jmakerange then
			pfrom:=plist.a
			pto:=plist.b
		else
			opc:=(opc=jforup|jforall|jforallrev)
			pfrom:=getrangelwbunit(duplunit(plist))
			pto:=getrangeupbunit(duplunit(plist))
		fi

	else
		if lx.symbol=assignsym then
			lex()
			pfrom:=readunit()
		else
			pfrom:=createconstunit(1,tint)
		fi
		checksymbol(ktosym)
		opc:=(lx.subcode=1|jfordown|jforup)
		lex()
		pto:=readunit()

		if lx.symbol=kbysym then
			lex()
			pstep:=readconstexpr(0)
			if pstep.tag=jconst then
				if pstep.value=1 then		!by 1
					pstep:=nil
				fi
			fi
		fi
	fi

	if lx.symbol=kwhensym then
		lex()
		pcond:=fixcond(readunit())
	fi
	checksymbol(kdosym)
	lex()
	pbody:=readsunit()
	pelse:=nil

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	fi
	checkend(kendsym,kforsym,kdosym)
	lex()

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif,pcond,pbody))
	fi
	pbody.nextunit:=pelse

!forup/down layout
!	a:	pindex
!	b:	pfrom/pto/pstep
!	c:	pbody

!forall/rev layout
!	a:	pindex/plocal/pfrom/pto
!	b:	plist/passign
!	c:	pbody

	case opc
	when jforup, jfordown then
		if plocal then serror("for i,x?") fi
		pindex.avcode:='I'
		if pto.tag not in [jconst, jname] then
			plocal:=createname(getavname(currproc))
			plocal.avcode:='I'
			ptoinit:=createunit2(jassign, plocal, pto)
			pindex.nextunit:=ptoinit
			pto:=plocal
		fi

		pfrom.nextunit:=pto
		pto.nextunit:=pstep
		p:=createunit3(opc, pindex, pfrom, pbody)

	else										!assume forall/rev

		if plocal=nil then						!only for x
			plocal:=pindex
			pindex:=createname(getavname(currproc))
		fi
		pindex.avcode:='I'
		plocal.avcode:='L'
		pindex.nextunit:=plocal
		plocal.nextunit:=pfrom
		pfrom.nextunit:=pto

		passign:=createunit2(jassign,duplunit(plocal),
					createunit2(jindex,duplunit(plist),duplunit(pindex)))
		plist.nextunit:=passign

		p:=createunit3(opc, pindex, plist, pbody)

	esac

	p.pos:=pos
	--nforloops
	return p
end

function readname:unit p=
	p:=readterm2()
	if p.tag<>jname then serror("Name expected") fi
	return p
end

global proc readtypedef(symbol owner,int scope=0)=
!at 'type' symbol
	symbol sttype,stname
	int t,m

	lexchecksymbol(namesym)
	stname:=lx.symptr

	lex()
	checkequals()
	lex()

	sttype:=getduplnameptr(owner,stname,typeid)
	adddef(owner,sttype)
	m:=createusertype(sttype)
	ttusercat[m]:=1

	t:=readtypespec(sttype,m)		!should return filled-in version of m

	sttype.scope:=scope
	storemode(owner,t,sttype.mode)

	if t>=0 then
		if ttisinteger[t]+ttisreal[t] then
			tttarget[m]:=t
		elsif ttisref[t] then
		elsecase ttbasetype[t]
		when tarray then
		when tslice then
!		when tslice,tvector, tflex then
		when trecord then
		else
			tttarget[m]:=t
		fi
	else
		storemode(owner,t,tttarget[m])
	fi

	if t>=0 then
		copyttvalues(m,t)
	else
		ttbasetype[m]:=tpending
	fi
end

global proc readrecordfields(symbol owner,int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
	int nvars,offset
	symbol stname,stbitfield

	nvars:=0
	while lx.symbol=namesym do

		stname:=getduplnameptr(owner,lx.symptr,fieldid)
		storemode(owner,m,stname.mode)
		++nvars

		if unionpend.ulength then
			unionstr_copy(&stname.uflags,&unionpend)
			unionstr_concat(&unionstring,&unionpend)
			unionstr_clear(&unionpend)
		else
			unionstr_clear(&stname.uflags)
		fi
		unionlastvar:=stname			!filled in from outside with 'E' codes

		adddef(owner,stname)

		lex()

		case lx.symbol
		when atsym then
			lex()
			stname.atfield:=1
			stname.equivfield:=readequivfield(owner)
			if lx.symbol=addsym then
				lex()
				offset:=readconstint()
				if offset>stname.equivoffset.max then serror("Offset>255") fi
				stname.equivoffset:=offset
			fi

		when datsym then
			lexchecksymbol(intconstsym)
			case lx.value
			when 1,2,4,8 then
				stname.align:=lx.value
			when 0 then
				stname.align:=255
			else
				serror("@@ bad align")
			esac
			lex()
		when colonsym then				!read bitfields
!format is int : (a:1, b:3, c:2)
			lexchecksymbol(lbracksym)

			repeat
				lexchecksymbol(namesym)
				stbitfield:=getduplnameptr(owner,lx.symptr,fieldid)
				stbitfield.mode:=tbitfield
				adddef(owner,stbitfield)

				stbitfield.atfield:=1
				stbitfield.equivfield:=stname

				lexchecksymbol(colonsym)
				lexchecksymbol(intconstsym)
				stbitfield.bitfieldwidth:=lx.value
				lex()

			until lx.symbol<>commasym
			checksymbol(rbracksym)
			lex()

		esac

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No fields declared")
	fi
end

global proc readtabledef(symbol owner,int scope=0)=
!at 'tabledata' symbol
	int i,ncols,nrows,enums,nextenumvalue,firstval,lastval,startline,closesym
	int ltype
	unit plower
	symbol stvar,stenum,stgen
	const maxcols=20
	[maxcols]symbol varnameptrs
	[maxcols]int varlisttypes
	[maxcols]unit plist,plistx
	const maxrows=500
	[maxrows]int enumvalues

	enums:=lx.subcode				! means enumdata
	lex()

	tabledataname:=nil

	if lx.symbol=lbracksym then		!tabledata(...) read enum type
		if not enums then serror("Use 'enumdata'") fi
		enums:=1
		lex()
		checksymbol(rbracksym)
		lex()
	fi


	nextenumvalue:=1
	nrows:=0			!number of data rows appearing
	ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
	while lx.symbol<>eqsym do
		ltype:=readtypespec(owner)
		checksymbol(namesym)
		if ++ncols>maxcols then
			serror("tabledata/too many columns")
		fi
		varnameptrs[ncols]:=lx.symptr
		varlisttypes[ncols]:=ltype

		lex()
		if lx.symbol=commasym then
			lex()
		else
			exit
		fi
	od

	lex()					!skip =

	skipsemi()
	startline:=getcurrline()
	closesym:=checkbegin(0)

	skipsemi()
	firstval:=lastval:=0

	for i:=1 to ncols do
		plist[i]:=plistx[i]:=nil
	od

	intabledata:=1
	do			!loop per row
		skipsemi()
		if ncols>0 then
			checksymbol(lbracksym)
			lex()
		fi
		if ++nrows>maxrows then
			serror("tabledata:too many rows")
		fi

		if enums then
			checksymbol(namesym)
			stgen:=lx.symptr				!generic symbol entry
			tabledataname:=stgen.name		!allow to be picked up by $ lx.symbol
			lex()
			if lx.symbol=eqsym then
				lex()
				nextenumvalue:=readconstint()
			fi
			enumvalues[nrows]:=nextenumvalue

			stenum:=getduplnameptr(owner,stgen,constid)
			stenum.mode:=tint
			stenum.code:=createconstunit(nextenumvalue,tint)
			stenum.scope:=scope
			adddef(owner,stenum)
			if scope=export_scope then
				addexpconst(stenum)
			fi

			if nrows=1 then firstval:=nextenumvalue fi
			lastval:=nextenumvalue

			++nextenumvalue
			if ncols then				!comma always expected
				checksymbol(commasym)		!check it
				lex()
			fi
		fi

		for i:=1 to ncols do
			addlistunit(plist[i],plistx[i],readunit())
			if i=ncols then
				checksymbol(rbracksym)
			else
				checksymbol(commasym)
			fi
			lex()
		od

		if lx.symbol<>commasym then exit fi
		lex()					!should be ( for next entry
		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	od

	intabledata:=0

	skipsemi()
	checkbeginend(closesym,ktabledatasym,startline)

!Here, I have::

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

!for each variable, add a vardef initialised to the list
!add the decls for the vars

	for i:=1 to ncols do

		stvar:=getduplnameptr(owner,varnameptrs[i],staticid)
		stvar.code:=createunit1(jmakelist,plist[i])
		stvar.code.length:=nrows

		storemode(owner,varlisttypes[i],stvar.mode)
		stvar.scope:=scope

		adddef(owner,stvar)
		addstatic(stvar)
	od
end

global proc readclassdef(symbol owner,int scope)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, baseclass, m, startline, closesym, mrec, normalexit,isrecord, align
	symbol nameptr, sttype, newd, d,e

	kwd:=lx.symbol
	isrecord:=kwd=krecordsym

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()
	baseclass:=0
	if lx.symbol=lbracksym then
		lex()
		baseclass:=readtypespec(owner)
		checksymbol(rbracksym)
		lex()
	fi

	checkequals()
	lex()

	align:=0
	if lx.symbol=atsym then
		if lx.subcode=0 then
			lex()
			align:=readconstint()
		else
			lex()
		fi
		align:=1
	fi

	sttype:=getduplnameptr(owner,nameptr,typeid)
	adddef(owner,sttype)
	m:=createusertype(sttype)

	mrec:=createrecordmode(owner, m)
	storemode(owner,mrec,sttype.mode)

	storemode(owner,baseclass,sttype.baseclass)
	sttype.align:=align

	closesym:=checkbegin(1)

	startline:=getcurrline()

	readclassbody(sttype,kwd)

	checkbeginend(closesym,kwd,startline)

	sttype.scope:=scope
end

proc readclassbody(symbol owner,int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
!classkwd=kclasssym or krecordsym
	int kwd,t
	symbol d

	unionstr_clear(&unionstring)
	unionstr_clear(&unionpend)

	doswitch lx.symbol
	when kconstsym then
		readconstdef(owner,0)
	when kfunctionsym,kprocsym then
		kwd:=lx.symbol

		if owner.isimport then
			readprocdecl(owner,0,0)
		else
			readprocdef(owner,0)
		fi
	when kclasssym,krecordsym then
		readclassdef(owner,0)

	when ktypesym then
		readtypedef(owner)
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()

	when ktabledatasym then
		readtabledef(owner,0)

	when kmacrosym then
		readmacrodef(owner,0)

	when kstructsym,kunionsym then
		unionstr_append(&unionpend,(lx.symbol=kstructsym|'S'|'U'))
		unionlastvar:=nil
		lex()
	when kendsym,rbracksym then
		if unionstring.ulength then
			checkend(kendsym,(unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
			lex()
			if unionlastvar=nil or unionpend.ulength then
				serror("Empty union group")
			fi
			case unionstr_last(&unionlastvar.uflags)
			when 'E','*' then
			else
				unionstr_append(&unionlastvar.uflags,'*')
			esac
			unionstr_append(&unionlastvar.uflags,'E')
			unionstring.ulength--
		else
			exit
		fi

	when kmutsym then

		lex()
		if istypestarter() then
	readmut::
			++insiderecord
			t:=readtypespec(owner)
			--insiderecord
		else
			t:=tauto
		fi
		readrecordfields(owner,t)

	when kletsym then
		serror("Let not allowed")

	else
		if istypestarter() then
			goto readmut
!		serror("record:need var")
		else
			exit
		fi
	end doswitch
end

proc readimportmodule(symbol owner)=
!at 'importmodule' symbol
	int isnew,startline,closesym, libtype
	symbol d,stname,stname0

	if insidedllimport then serror("nested importdll") fi
	libtype:=lx.subcode

	lex()
	if lx.symbol=stringconstsym then
		stname:=addnamestr(lx.svalue)
	else
		checksymbol(namesym)
		stname:=lx.symptr
	fi

	lex()
	checkequals()
	lex()

!stname points to a nullid symbol
!check whether this module already exists

	isnew:=1
	d:=stname.nextdupl
	while d do
		if d.nameid=dllmoduleid then
			stname:=d
			isnew:=0
			exit
		fi
		d:=d.nextdupl
	od

	if isnew then			!new
		stname:=getduplnameptr(stmodule,stname,dllmoduleid)
		adddef(stmodule,stname)
		
		addlib(stname.name, libtype)

	stname.dllindex:=nlibfiles
	fi

	startline:=getcurrline()
	closesym:=checkbegin(0)

	insidedllimport:=1

	readimportbody(owner)

	insidedllimport:=0

	checkbeginend(closesym,kimportmodulesym,startline)

end

proc readimportbody(symbol owner)=
!positioned at first symbol of statement (which can be empty)
!return knode containing statement, or nil if not found (at 'end etc)
	int pos,fflang
	symbol d

	pos:=lx.pos

	do
		skipsemi()
		switch lx.symbol
		when kfflangsym then
			fflang:=lx.subcode
			lex()
			case lx.symbol
			when kprocsym,kfunctionsym then
				goto doproc
			esac

		when kprocsym,kfunctionsym then
			fflang:=0
doproc::
			d:=readprocdecl(owner,0,fflang)
			if ndllproctable>=maxdllproc then
				serror("Too many dll procs")
			fi
			dllproctable[++ndllproctable]:=d

		when ktypesym then
			readtypedef(owner,subprog_scope)

		when kconstsym then
			readconstdef(owner,subprog_scope)

		when kclasssym,krecordsym then
			readclassdef(owner,subprog_scope)

		when kmutsym then
			lex()
			readvardef(owner,subprog_scope,0,dllvarid, kmutsym)

		when stdtypesym,namesym,krefsym,kicharsym,ktypeofsym,lsqsym,
			kdictsym,kslicesym then
			readvardef(owner,subprog_scope,0,dllvarid, 0)

		when eofsym then
			exit

		when kendsym then
			exit
		else
			PS("symbol")
			serror("Not allowed in importmodule")
		end switch
	od
end

function readequivfield(symbol owner)symbol=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	symbol p,d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=owner.deflist
	while p do
		if eqstring(p.name,d.name) then
			return p
		fi

		p:=p.nextdef
	od
	cpl d.name
	serror("Can't find @ field")
	return nil
end

function readrefproc(symbol owner,int typedefx,int fflang)int=
!'ref' was seen, now positioned at 'proc' 'function' or 'method'
!read proc params and any result, return a complete ref proc spec
	int kwd,prettype,m,varparams,nparams
	[4]int retmodes
	symbol paramlist,stproc
	int rettype2, rettype3, nretvalues
	ichar name

	kwd:=lx.symbol				!remember whether proc or function
	
	lex()

	paramlist:=nil
	prettype:=tvoid
	nretvalues:=0

!need to create suitable holding typename in advance
	name:=nextautotype()
	stproc:=getduplnameptr(stmodule,addnamestr(name),typeid)
	adddef(stmodule,stproc)
	retmodes[1]:=tvoid

	if kwd=kfunctionsym then
		if lx.symbol=lbracksym then		!possible params
			lex()
			if lx.symbol<>rbracksym then
				paramlist:=readparams(owner,stproc,0,varparams,nparams)
				checksymbol(rbracksym)
			fi
			lex()
			if lx.symbol=colonsym or lx.symbol=sendtosym then
				lex()
				nretvalues:=readreturntype(stproc,retmodes)
			elsif typestarterset[lx.symbol] or lx.symbol=namesym then
				nretvalues:=readreturntype(stproc,retmodes)
			fi
		elsif lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(stproc,retmodes)
		fi
		if nretvalues=0 then
			serror("Function needs return type")
		end

		if nretvalues and kwd=kprocsym then		!proc: result given
			serror("Proc can't return value")
		fi
	else					!proc with no result
		if lx.symbol=lbracksym then		!possible params
			lex()
			if lx.symbol<>rbracksym then
				paramlist:=readparams(owner,stproc,0,varparams,nparams)
				checksymbol(rbracksym)
			fi
			lex()
		fi
		if typestarterset[lx.symbol] or lx.symbol=colonsym or lx.symbol=sendtosym then
			serror("proc can't have ret value")
		fi
	fi

	m:=createrefprocmode(owner,stproc,paramlist,kwd,prettype,typedefx)

	storemode(owner,retmodes[1],stproc.mode)
	stproc.nretvalues:=nretvalues

	ttnamedef[m]:=stproc
	stproc.fflang:=fflang
	stproc.varparams:=varparams

	return m
end

proc pushproc(symbol p)=
	if nprocstack>=maxprocstack then
		serror("Too many nested proc")
	fi
	procstack[++nprocstack]:=currproc
	currproc:=p
end

proc popproc=
	if nprocstack then
		currproc:=procstack[nprocstack--]
	else
		currproc:=stmodule
	fi
end

function makeastring:unit =
!current symbol is an 'astring', like a regular string constant, but intended
!to be a byte-array
!Simplest treatment, if not the most efficient, is to turn that into normal 
!makelist unit
	unit ulist,ulistx, p, pconst
	ref char s
	int length

	ulist:=ulistx:=nil

	s:=lx.svalue
	length:=astringlength
	to astringlength do
		pconst:=createconstunit(s^,ti64)
		addlistunit(ulist,ulistx,pconst)
		++s
	od

	if lx.subcode='Z' then
		pconst:=createconstunit(0,ti64)
		addlistunit(ulist,ulistx,pconst)
		++length
	fi

	p:=createunit1(jmakelist,ulist)
	p.length:=length
	return p
end

function readreturntype(symbol owner, []int &retmodes)int=
!read 1..maxtuplesize return types as part of function decl
	int nretvalues

	retmodes[1]:=readtypespec(owner)
	nretvalues:=1
	while lx.symbol=commasym do
		if nretvalues>=maxtuplesize then
			serror("Too many return values")
		fi
		lex()
		retmodes[++nretvalues]:=readtypespec(owner)
	od

	return nretvalues
end

function readset:unit=
!positioned at "["
	int length,nkeyvalues,oldirp
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

	length:=0
	nkeyvalues:=0

	ulist:=ulistx:=nil

	do
		oldirp:=inreadprint
		inreadprint:=0
		p:=readunit()
		inreadprint:=oldirp
		if p.tag=jkeyvalue then ++nkeyvalues fi
		++length

		addlistunit(ulist,ulistx,p)

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=rsqsym then exit fi
		when semisym then
			lexchecksymbol(rsqsym)
			exit
		when rsqsym then
			exit
		else
			serror("readset?")
		esac
		skipsemi()						!allow a,b,c;]
	od
	lex()

	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict,ulist)
	else
		p:=createunit1(jmakeset,ulist)
	fi
	p.length:=length
	return p
end

function istypestarter:int=
	if typestarterset[lx.symbol] then return 1 fi
	if lx.symbol=namesym then				!name ...
		case nextlx.symbol
		when namesym then					!name name
			return 1
		when addrsym then
			return 1
		esac
	fi
	return 0
end

global function readunit:unit p=
	unit pt
	int pos

	pt:=nil
	pos:=lx.pos
	pt:=readterm2()

	if jisexpr[pt.tag]=0 then
		return pt
	fi

	if endsexpr[lx.symbol] then
		return pt
	fi

	if lx.symbol=assignsym then
		lex()
		p:=readterm2()
		if endsexpr[lx.symbol] then
			p:=createunit2(jassign, pt, p)
			p.pos:=pos
			return p
		fi
		p:=createunit2(jassign, pt, readassignment(p))
	else
		p:=readassignment(pt)
		p.pos:=pos
	fi

	while lx.symbol=pipesym do
		lex()
		p:=createunit2(jcallfn, readassignment(), p)
	od

	return p
end

function readassignment(unit pt=nil)unit p=
	int pos,opc
	unit q

	p:=readorterms(pt)

	if (opc:=lx.symbol) in [assignsym, deepcopysym] then
		pos:=lx.pos
		lex()
		if lx.symbol=kemptysym then
			p:=createunit1(jempty, p)
			lex()
		else
			q:=readassignment(nil)
			if opc=deepcopysym then
				q:=createunit1(jcopy,q)
			fi
			p:=createunit2(jassign,p,q)
		fi
		p.pos:=pos
	fi
	return p
end

function readorterms(unit pt=nil)unit p=
	int pos

	p:=readandterms(pt)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto,p,readassignment())
			p.pclop:=korlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(jorl,p,readandterms())
		p.pclop:=korl
		p.pos:=pos
	od

	return p
end

function readandterms(unit pt=nil)unit p=
	int pos

	p:=readcmpterms(pt)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto,p,readassignment())
			p.pclop:=kandlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(jandl,p,readcmpterms())
		p.pclop:=kandl
		p.pos:=pos
	od

	return p
end

function readcmpterms(unit pt=nil)unit p=
	int pos,opc,n
	unit ulist,ulistx,q
	[4]byte genops

	p:=readinterms(pt)

	if lx.symbol not in [eqsym,cmpsym] then
		return p
	fi

	ulist:=ulistx:=p
	p:=createunit1(jcmpchain,p)
	n:=0				!n counts operand after the first
	clear genops

	doswitch lx.symbol
	when eqsym, cmpsym then
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms()
		addlistunit(ulist,ulistx,q)
		q.pos:=pos
	else
		exit
	end doswitch

	if n=1 then
		p.tag:=jcmp
		q:=p.a
		p.pclop:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	fi

	return p
end

function readinterms(unit pt=nil)unit p=
	int pos,opc
	p:=readrangeterm(pt)

	doswitch lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(jbin,p,readrangeterm())
		p.pclop:=opc
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readrangeterm(unit pt=nil)unit p=
	int pos,opc
	p:=readaddterms(pt)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange,p,readaddterms())
		p.pos:=pos
	fi

	return p
end

function readaddterms(unit pt=nil)unit p=
	int pos,sym, tag, genop
	p:=readmulterms(pt)

	doswitch sym:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym,
		 appendsym, concatsym then
		pos:=lx.pos
		genop:=lx.subcode
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto,p,readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin,p,readmulterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readmulterms(unit pt=nil)unit p=
	int pos,sym

	p:=readpowerterms(pt)

	doswitch sym:=lx.symbol
	when mulsym, divsym, idivsym, iremsym, shlsym, shrsym, idivremsym then
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto,p,readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin,p,readpowerterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readpowerterms(unit p=nil)unit=
	int pos

	if p=nil then
		p:=readterm2()
	fi

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jbin,p,readpowerterms())
		p.pclop:=kpower
		p.pos:=pos
	od

	return p
end

function readterm2:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t

	pos:=lx.pos

	p:=readterm()

	doswitch lx.symbol
	when lbracksym then
		lex()
		oldinrp:=inreadprint
		inreadprint:=0
		q:=readslist(1,1)
		checksymbol(rbracksym)
		lex()
		if p.tag=jsyscall then
			p.a:=q
		else
			p:=createunit2(jcallfn,p,q)
		fi
		inreadprint:=oldinrp
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr,p)
		lex()

	when lsqsym then
		p:=readindex(p,0)

	when dotsym then
		p:=readdotsuffix(p)

	when colonsym then
		if inreadprint then exit fi
		lex()
		q:=readunit()
		p:=createunit2((inparamlist|jkeyword|jkeyvalue),p,q)

	when incrsym then
		case lx.subcode
		when kincr then opc:=kloadincr
		when kdecr then opc:=kloaddecr
		esac
		lex()
		p:=createunit1(jincr,p)
		p.pclop:=opc

	when anddotsym then
		lexchecksymbol(lsqsym)
		lex()
		q:=readunit()
		if q.tag=jmakerange then
			p:=createunit2(janddotslice,p,q)
		else
			p:=createunit2(janddotindex,p,q)
		fi
		checksymbol(rsqsym)
		lex()

	when lcurlysym then
		serror("X{...} not ready")
	else
		exit
	end doswitch

	p.pos:=pos

	return p
end

function readterm:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t,length

	pos:=lx.pos

	switch lx.symbol
	when namesym then
		if nextlx.symbol=atsym then		!type-punning with user type
			p:=readcast()
		else
			p:=createname(lx.symptr)
			p.pos:=lx.pos
			lex()
		fi

	when intconstsym,realconstsym then
		p:=createconstunit(lx.value,lx.subcode)
		p.istrueconst:=1
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue,-1)
		lex()

	when astringconstsym then
		p:=makeastring()
		lex()

	when decimalconstsym then
		SERROR("DEC CONST")

	when charconstsym then
		length:=strlen(lx.svalue)
		if length>8 then serror("Char const too long") fi
		a:=0
		if length then
			memcpy(&a,lx.svalue,length)
		fi
		p:=createconstunit(a,tc64)
		p.istrueconst:=1
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym,krefsym,kicharsym,ktypeofsym then
		p:=readcast()

	when addsym, subsym, minsym, maxsym, abssym, inotsym,
		mathsopsym, sqrtsym, sqrsym, maths2opsym,signsym then
		p:=readopc()

	when notlsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jnotl, readterm2())
			p.pclop:=knotl
		fi

	when istruelsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jistruel, readterm2())
			p.pclop:=kistruel
		fi

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(jincr,readterm2())
		p.pclop:=opc

	when ksprintsym then
		p:=readsprint()

	when ksreadsym,ksreadlnsym then
		p:=readsread()

	when addrsym,daddrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc,readterm2())
		if p.a.tag=jcallfn then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when anddotsym then
		lex()
		p:=createunit1(jaddroffirst,readterm2())

	when compilervarsym then
		p:=readcompilervar()

	when kerrorsym then
		p:= createconstunit(lx.subcode,tint)
		lex()

	when dollarsym then
		if intabledata then
			p:=createstringconstunit(tabledataname,-1)
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(junary,dollarstack[ndollar])
			p.pclop:=kupb
		fi
		lex()

	when kcastsym then
		p:=readcastx()

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			checksymbol(commasym)
			lex()
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(jbin,p,q)
		q.pclop:=kmax
		p:=createunit2(jbin,q,r)
		p.pclop:=kmin

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym,kdocasesym,kswitchsym,kdoswitchsym then
		p:=readswitchcase()

	when krecasesym then
		p:=readrecase()

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

	when kswapsym then			!swap using function syntax
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		checksymbol(rbracksym)
		lex()
		p:=createunit2(jswap,p,q)

	when kevalsym then
		lex()
		p:=createunit1(jeval,readunit())

	when kassemsym then
		currproc.asmused:=1
		assemmode:=1
		if lx.subcode=0 then
			p:=readassemline()
		else
			p:=readassemblock()
		fi
		assemmode:=0

	when ksyscallsym then
		p:=createunit0(jsyscall)
		p.fnindex:=lx.subcode
		lex()

	when kstrincludesym then
		lex()
		p:=createunit1(jstrinclude,readterm2())

	when kemptysym then
		lex()
		p:=createunit1(jempty, readterm2())

	when kcopysym then
		lex()
		p:=createunit1(jcopy, readterm2())

	when lcurlysym then
		serror("{...} not ready")

	else
		cpl symbolnames[lx.symbol],=LX.SYMBOL, ISTYPESTARTER()
		serror("readterm?")
	end switch

	p.pos:=pos
	return p
end

proc readmacrodef(symbol owner, int scope)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported
!int kwd,varparams,try_level, prettype, nparams, rettype2, rettype3, nretvalues
!ichar metadata, truename
!symbol pequiv, stproc, owner, paramlist,nameptr

	symbol nameptr,stmacro, paramlist,paramlistx, stname

	lexchecksymbol(namesym)

	nameptr:=lx.symptr
	stmacro:=getduplnameptr(owner,nameptr,macroid)
	adddef(owner,stmacro)

	owner:=stmacro

	lex()

	paramlist:=paramlistx:=nil

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=getduplnameptr(owner,lx.symptr,macroparamid)
					adddef(owner,stname)
					addlistparam(&paramlist,&paramlistx,stname)
!					stname.nulldef:=lx.symptr

					lex()
					if lx.symbol=rbracksym then
						exit
					fi
					checksymbol(commasym)
					lex()
				else
					serror("macro def params")
				esac
			od
		fi
		lex()						!skip )
	fi
	stmacro.paramlist:=paramlist
	stmacro.scope:=scope

	checkequals()
	lex()
	stmacro.code:=readunit()
end

function readrecase:unit=
	lex()
	if lx.symbol=kelsesym then
		lex()
		return createunit0(jrecase)
	else
		return createunit1(jrecase,readunit())
	fi
end

proc adddocstring(ichar s)=
	if ndocstrings>docstrings.len then
		serror("Too many docstrings")
	fi
	docstrings[++ndocstrings]:=pcm_copyheapstringn(s,strlen(s))
end

function fixcond(unit p)unit=
	if not isbooltag[p.tag] then
		insertunit(p, jistruel)
		p.pclop:=kistruel
	fi
	return p
end

function readsunit(int inwhile=0)unit=
	int pos,m,sym,opc
	unit ulist,ulistx,p,q,r
	symbol stname

	pos:=lx.pos
	ulist:=ulistx:=nil

	repeat
		while lx.symbol=semisym do
			lex()
		od
		switch lx.symbol
		when kstaticsym then
			lex()
			if lx.symbol in [kletsym,kmutsym] then
				opc:=lx.symbol
				lex()
			else
!			opc:=kmutsym
				opc:=0
			fi
			readvardef(currproc,0,1,staticid,opc)

		when kprocsym,kfunctionsym then
			readprocdef(currproc,0)

		when stdtypesym,krefsym,kicharsym,ktypeofsym,kdictsym,kslicesym,lsqsym then
			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
				goto doexec
			else
				sym:=0
				goto dovar
			fi

		when karraysym then
			lexchecksymbol(lsqsym)
			sym:=0
			goto dovar

		when kmutsym,kletsym then
			sym:=lx.symbol
			lex()
	dovar::
			q:=readvardef(currproc,0,0,frameid,sym)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist,ulistx,q)		!add one by-one
				q:=r
			od

		when ktypesym then
			readtypedef(currproc,0)

		when kconstsym then
			readconstdef(currproc,0)

		when kclasssym,krecordsym then
			readclassdef(currproc,0)

		when docstringsym then
			adddocstring(lx.svalue)
			lex()

		when kmacrosym then
			readmacrodef(currproc,0)

		when ktabledatasym then
			readtabledef(currproc,0)

		when eofsym then
			cpl currproc.name
			serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
		when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,
				kelsecasesym,kelseswitchsym,kendsym then
			exit
!
		when namesym then
			case nextlx.symbol
			when dcolonsym then
				p:=createunit0(jlabeldef)
				stname:=getduplnameptr(currproc,lx.symptr,labelid)
				adddef(currproc,stname)
				p.def:=stname
!				p.trylevel:=try_level
				lex()
				lx.symbol:=semisym
				addlistunit(ulist,ulistx,p)
			when namesym then
				sym:=kmutsym
				goto dovar
			goto doexec

			else
				goto doexec
			esac
		when kdosym then				!u;u;u;do rather than u;u;u do
			if inwhile then
				exit
			fi
			goto doexec

		when semisym then

		when kstepsym then
			exit

		else							!assume a statement
	doexec::
			p:=readunit()
	doexec2::
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
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,
		kelsecasesym,kelseswitchsym,kendsym,commasym,
		barsym, kstepsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then
		return createunit1(jblock,ulist)
	else
		return ulist
	fi
end

