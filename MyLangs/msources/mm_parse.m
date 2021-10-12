import msys
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lex
import mm_lib
import mm_diags
import* mm_pcl
import pci_mcl

macro readunit=readassignment()

mut int intabledata=0		!1 means reading table data line; $ gives tabledataname
mut int inreadprint=0
mut int inparamlist=0
mut int inrecordbody=0
mut int inimportmodule=0
mut int labelseen=0
mut ichar tabledataname=nil

const maxprocstack=10
[maxprocstack]ref strec procstack
int nprocstack=0

uflagsrec unionstring, unionpend
ref strec unionlastvar=nil
ref strec dretvar			!part of read-proc: nil, or symptr of retval variable

int try_level=0
int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
int inmultexpr=0
int insiderecord=0
int insidedllimport=0
int yieldseen=0

const maxforloops=10
[maxforloops]ref strec forindexvars
int nforloops

global filehandle docfile

global function parsemodule(int n)int=
	modulerec m
	ref strec p, owner
	int globalflag,status

	initparser()

	m:=moduletable[n]
	currmoduleno:=n

	stmodule:=moduletable[n].stmodule
	currproc:=stmodule

	startlex("PARSEMODULE",n)

	owner:=stmodule

	lex()
	status:=readmoduledefs(owner)

	if not status then
		return 0
	fi

	return status
end

global function readmoduledefs(ref strec owner)int=
!first symbol has been read
	ref strec p,dimport,stimport
	int globalflag,i,callbackflag
	ichar name


	globalflag:=local_scope
	callbackflag:=0

	do
		switch lx.symbol
		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode
			lex()

		when kprocsym,kfunctionsym then	!todo
			readprocdef(owner,globalflag,callbackflag)
			callbackflag:=0
			globalflag:=local_scope

		when stdtypesym,namesym,lsqsym,krefsym,kicharsym,ktypeofsym,
			kdictsym,kslicesym then
			readvardef(owner,globalflag,0,staticid, 0)
			globalflag:=local_scope

		when kmutsym then
			lex()
			readvardef(owner,globalflag,0,staticid,kmutsym)
			globalflag:=local_scope

		when kletsym then
			lex()
			readvardef(owner,globalflag,0,staticid,kletsym)
			globalflag:=local_scope

		when kimportmodulesym then
			readimportmodule(owner)

		when kimportpathsym then
			lexchecksymbol(stringconstsym)
			lex()

		when kmapmodulesym then
			repeat
				lex()
			until lx.symbol in [semisym,eofsym]

		when ktypesym then
			readtypedef(owner,globalflag)
			globalflag:=local_scope

		when kconstsym then
			readconstdef(owner,globalflag)
			globalflag:=local_scope

		when kclasssym,krecordsym then
			readclassdef(owner,globalflag)
			globalflag:=local_scope

		when kenumsym then
			lex()
			readenumtype(owner,0,globalflag)
			globalflag:=local_scope

		when ktabledatasym then
			readtabledef(owner,globalflag)
			globalflag:=local_scope

		when docstringsym then
			adddocstring(lx.svalue)
			lex()

		when kimportsym then
			if globalflag then serror("glob/import?") fi
			lex()
			if lx.symbol=mulsym then
				lex()
			fi
			checksymbol(namesym)

!need to check that the import has been done (in case import stmt is badly placed)
!(note: can't detect a badly placed import if that lib has been loaded elsewhere)
			dimport:=lx.symptr
			name:=mapimport(dimport.name)

			for i:=1 to nmodules do
				if eqstring(name, moduletable[i].name) then
					stimport:=moduletable[i].stmodule
					exit
				fi
			else
				CPL lx.symptr.name
				serror("Import stmt out of position?")
			od
			lex()

			domappedalias(dimport,stimport)
			if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
				readimportalias(dimport)
			fi

		when semisym then
			lex()

		when eofsym then
			exit

		when kfflangsym then
			if lx.subcode=callbackff then
				callbackflag:=callbackff
				lex()
			else
				goto error
			fi

		when kmacrosym then
			readmacrodef(owner,globalflag)
			globalflag:=local_scope

		when dotsym then
			SERROR("MODULE/DOT")
		else
	error::
			PS("symbol")
			serror("Not allowed at module level")
		endswitch
	od

	return 1
end

proc initparser=

	unless nullunit then
		nullunit:=createunit0(j_null)
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

proc skipsemi=
	while lx.symbol=semisym do lex() od
end

function makeblock(unit p)unit=
	return createunit1(j_block,p)
end

function makestmtblock(unit p)unit=
	return createunit1(j_stmtblock,p)
end

proc checkequals=			!CHECKEQUALS
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

function getcurrline:int=
	return lx.pos
end

function checkbegin(int fbrack)int=				!CHECKBEGIN
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
	int closesym

	skipsemi()

	if lx.symbol=lbracksym and fbrack then
		closesym:=rbracksym
		lex()
	elsif lx.symbol=lcurlysym then
		closesym:=rcurlysym
		lex()
	else
		closesym:=kendsym
	fi
	return closesym
end

proc checkbeginend(int closesym,kwd,startline=0)=		!CHECKBEGINEND
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
	skipsemi()
	if closesym=rbracksym or closesym=rcurlysym then
		checksymbol(closesym)
	else
		checkend(closesym,kwd,startline:startline)
	fi
	lex()
end

proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=		!CHECKEND
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

function readvardef(ref strec owner,int isglobal=0,isstatic=0,varid=staticid, k)unit=
!positioned at symbol following 'mut' or 'let', which will at the first symbol of
!the type, or at the first name being defined if there is no type
!k is the keyword symbol used (let/mut), or set to 0 if no keyword has been used,
!then mut is assumed

!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]

	unit ulist,ulistx, p
	int nvars,m
	ref strec stname

	ulist:=ulistx:=nil

	if istypestarter() then
		m:=readtypespec(owner)
	elsif k then
		m:=tauto
	else
		serror("Readvar?")
	fi

	nvars:=0
	while lx.symbol=namesym do

		++nvars
		stname:=getduplnameptr(owner,lx.symptr,varid)

		stname.isglobal:=isglobal

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

		if lx.symbol in [assignsym,eqsym] then
			if lx.symbol=assignsym then
				if varid=staticid then
					serror("Need = on static not :=")
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
				p:=createunit1(j_empty,createname(stname))
				addlistunit(&ulist,&ulistx,p)
			else
				stname.code:=readunit()
				stname.equals:=1
				if varid=frameid then
					p:=createunit2(j_assign,createname(stname),stname.code)
					p.initlet:=1
					addlistunit(&ulist,&ulistx,p)
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

proc readconstdef(ref strec owner,int isglobal=0)=
!at 'const' symbol
	int nconsts,deft,m
	ref strec stname

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

		stname.isglobal:=isglobal

		adddef(owner,stname)
		if isglobal=export_scope then
			addconst(stname)
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
!listtag is j_makelist or j_makearray if 'array' was used

	unit ulist,ulistx, p,q,r, plower
	int oldirp,length

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
		p:=createunit0(j_operator)
		p.opcindex:=lx.subcode
		lex()
		lex()
		return p
	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=assignsym then	!operator:= constant
		p:=createunit0(j_operator)
		p.pclop:=symbolgentoops[lx.symbol]
		lex()			!read :=
		lexchecksymbol(rbracksym)
		lex()
		return p
	elsif istypestarter() then
		p:=readunit()
		checksymbol(rbracksym)
		lex()
		return p
	fi

!check symbol after "("
	case lx.symbol
	when rbracksym then			!empty list
		lex()
		p:=createunit0(j_makelist)
		p.b:=plower
		p.length:=0
		return p
	else					!assume normal expression follows
		p:=readxunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()

		return p

	when commasym then
		length:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(j_makelist,p)
			p.length:=length
			p.b:=plower
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
			addlistunit(&ulist,&ulistx,readxunit())
			++length
			skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
		until lx.symbol<>commasym
		checksymbol(rbracksym)
		lex()
		p:=createunit1(j_makelist,ulist)
		p.length:=length
		p.b:=plower
		return p

	when barsym then			!ifx/selectx expression; p is selector expression
		lex()
		q:=readxunit()
		case lx.symbol
		when barsym then		!(a|b|c)
			lex()
			r:=readsunit()
			checksymbol(rbracksym)
			lex()
			return createunit3(j_if,fixcond(p),q,r)
		when rbracksym then
			lex()
			return createunit3(j_if,fixcond(p),q,nil)

		esac

!assume selectx expression
		addlistunit(&ulist,&ulistx,q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(&ulist,&ulistx,readxunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readxunit()
		checksymbol(rbracksym)
		lex()
		return createunit3(j_select,p,ulist,r)

	else
		serror("(x ...")
	esac
	return nil
end

proc addlistparam(ref ref strec ulist,ulistx,ref strec p)=
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
		p:=createunit0(j_typeconst)
!	p.value:=t
		p.mode:=ttype
		return p

	when atsym then
		opc:=j_typepun
		lex()
	when dotsym then			!allow T.type, but also just T (followed by . which
								!might be T.minvalue etc)
		if nextlx.symbol=ktypesym then
			lex()
			p:=createunit0(j_typeconst)
			p.value:=t
			p.mode:=ttype
			lex()
		else					!leave dot to be processed by caller
			p:=createunit0(j_typeconst)
			p.value:=t
		fi
		return p
	else
		opc:=j_convert
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

function readopc:unit=			!READOPC
!op sym seen just before a term
	unit p,q,r
	int tag,opc,firstsym

	firstsym:=lx.symbol

	case lx.symbol
	when mathsopsym then
		tag:=j_unary
		opc:=lx.subcode
	when maths2opsym then
		tag:=j_bin
		opc:=lx.subcode
	else
		tag:=j_unary
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

		if p.tag=j_makelist then
			if p.length<>2 then serror("Needs (x,y)") fi
			q:=p.a
			r:=q.nextunit
			q.nextunit:=nil
			p:=createunit2(j_bin,q,r)
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
		tag:=j_unaryto
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

	if q.tag=j_makelist then
		serror("Too many opnds")
	fi

!*!evalmonop(p)
	return p
end

function readsprint:unit=			!READSPRINT
	int oldinreadprint,opc,isfprint
	unit pformat, pdev, printlist, printlistx, p

	oldinreadprint:=inreadprint
	inreadprint:=1
	opc:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	case opc
	when j_sfprint,j_cprint then
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
			addlistunit(&printlist,&printlistx,createunit0(j_nogap))
		else
			p:=readunit()
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(j_fmtitem,p,readunit())
			fi
			addlistunit(&printlist,&printlistx,p)
		fi
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

	finish::
	lex()
	inreadprint:=oldinreadprint
	if (opc=j_print or opc=j_fprint) and printlist=nil then
		serror("No print items")
	fi

	if isfprint then
		if pformat.tag=j_null then
			serror("No fmt str")
		fi
		return createunit3(opc,pdev,pformat,printlist)
	else
		return createunit2(opc,pdev,printlist)
	fi
end

function readsread:unit=		!READSREAD
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
		if opc=j_read then
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
			p:=createunit2(j_fmtitem,p,readunit())
		fi
		addlistunit(&readlist,&readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

	finish::
	lex()
	inreadprint:=oldinreadprint
	if opc=j_read and readlist=nil then
		serror("No read items")
	fi

	return createunit2(opc,pdev,readlist)
end

function readcompilervar:unit=		!READCOMPILERVAR
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	unit p
	ref modulerec currmodule:=&moduletable[currmoduleno]

	switch lx.subcode
	when j_cvnil then
		p:=createconstunit(0,tref)
		lex()
		return p

	when j_cvpi then
		p:=createconstunit(int64@(3.14159'265358'979'3238'4626'433'832),treal)
		lex()
		return p

	when j_cvlineno then

		p:=createunit0(j_cvlineno)
		lex()
		return p

	when j_cvstrlineno then
		getstrint(lx.lineno,&.str)

	when j_cvmodulename then
		strcpy(str,stmodule.name)

	when j_cvfilename then

		strcpy(str,sourcefilepaths[currmodule.fileno])

	when j_cvfunction then
		strcpy(&.str,currproc.name)

	when j_cvdate then
		os_getsystime(&tm)
		fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

	when j_cvtime then
		os_getsystime(&tm)
		fprint @&.str,"#:#:#",tm.hour:"z2",tm.minute:"z2",tm.second:"z2"

	when j_cvtargetbits then
		lex()
		return createconstunit(targetbits,tint)
	when j_cvtargetsize then
		lex()
		return createconstunit(targetsize,tint)
	when j_cvtargetcode then
		strcpy(&.str,"wx64")

	when j_cvversion then
		strcpy(&.str,"Compiler:BX Experimental")

	when j_cvtrue,j_cvfalse then
		p:=createconstunit(lx.subcode=j_cvtrue,tint)
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
	opc:=j_convert
	if lx.symbol=atsym then
		opc:=j_typepun
		lex()
	fi
	checksymbol(lbracksym)
	lex()
	m:=tvoid
	p:=readunit()
	if lx.symbol<>commasym then
		if opc=j_typepun then serror("@ type missing") fi
		opc:=j_autocast
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

proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

global function readtypespec(ref strec owner,int typedefx=0)int=			!READTYPESPEC
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
	ref strec d,e
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
						dim:=createunit2(j_keyvalue,dim,length)
					else													!lower::
						dim:=createunit1(j_keyvalue,dim)
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

	when kenumsym then		!enum
		lex()
		t:=readenumtype(owner,typedefx)

	when lbracksym then
		t:=readenumtype(owner,typedefx)

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

	else
		serror("Bad type starter")
	esac

	if typedefx then			!assume a simple alias
		ttbasetype[typedefx]:=ttbasetype[t]
	fi

	return t
end

function readslicetype(ref strec owner, int slicetype, typedefx)int=
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

function readslist(int iscall=0,donulls)unit=		!READSLIST
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (j_ust comma or terminator, which
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
				addlistunit(&ulist,&ulistx,createunit0(j_null))
			else
				serror("null comma expr not allowed")
			fi
			lex()
		when rbracksym then
			if donulls then
				addlistunit(&ulist,&ulistx,nullunit)
			fi
			exit
		else
			addlistunit(&ulist,&ulistx,readunit())
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

function readindex(unit p,int dot)unit=		!READINDEX
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
			plower:=createunit1(j_unary,duplunit(p))
			plower.pclop:=klwb
			pupper:=createunit1(j_unary,duplunit(p))
			pupper.pclop:=kupb
			p:=createunit2(j_slice, p, createunit2(j_makerange,plower, pupper))
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

		if q.tag=j_makerange then		!convert into a discrete slice
			p:=createunit2((dot|j_dotslice|j_slice),p,q)
		else
			p:=createunit2((dot|j_dotindex|j_index),p,q)
		fi

		exit when lx.symbol<>commasym
		lex()
	od
	checksymbol(rsqsym)
	lex()
	return p
end

function readdotsuffix(unit p)unit=		!READDOTSUFFIX
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
			p:=createunit2(j_dot,p,createname(lx.symptr))
			lex()
		when propsym then
	doprop::
			p:=createunit1(j_unary,p)
			p.pclop:=lx.subcode
			lex()
		when bitfieldsym then
			p:=createunit1(j_bitfield,p)
			p.bfcode:=lx.subcode
			lex()
		when ktypesym then			!.type, convert to .gettype
			case p.tag
			when j_typeconst then			!int.type=>int

			else
				p:=createunit1(j_typeof,p)
			esac
			lex()

		when maxsym then
			lx.subcode:=kmaxvalue
			goto doprop

		when minsym then
			lx.subcode:=kminvalue
			goto doprop
		when stdtypesym then
			if p.tag=j_typeconst and lx.subcode=trange then
				q:=createunit2(j_makerange,
					createunit1(j_unary,p),
					createunit1(j_unary,p))
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
		endswitch
	od
	return p
end

function readconstexpr(int needconst=1)unit=
	return readunit()
end

function readconstint:int=		!READCONSTINT
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

proc readprocdef(ref strec procowner,int isglobal,fflang=0)=
!at 'proc' etc symbol; read proc def or declaration
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
	int kwd,startline,closesym
	ref strec stproc,q,stname

	kwd:=lx.symbol
	yieldseen:=0
	nforloops:=0

	stproc:=readprocdecl(procowner,isglobal,fflang)

	checkequals()
	lex()

	startline:=getcurrline()

	closesym:=checkbegin(0)

	pushproc(stproc)
	nextavindex:=0

	IF DRETVAR THEN
		stname:=getduplnameptr(stproc,dretvar,frameid)
		storemode(procowner,stproc.mode,stname.mode)
		adddef(stproc,stname)
	fi

	addtoproclist(stproc)

	stproc.code:=readsunit()

	checkbeginend(closesym,kwd,startline)

	if yieldseen then
		stproc.nameid:=generatorid
	fi

	if ndocstrings and docfile and stproc.isglobal=export_scope then
		println @docfile,"proc",stproc.name
		for i to ndocstrings do
			println @docfile,docstrings[i]
			pcm_free(docstrings[i],strlen(docstrings[i]+1))
		od
		println @docfile

		ndocstrings:=0
	fi

	popproc()
end

global function readprocdecl(ref strec procowner,int isglobal,fflang)ref strec=
!at 'proc'  or 'function' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

	int kwd,varparams,try_level, nparams, nretvalues, isthreaded
	[maxtuplesize]int retmodes
	int prettype@&retmodes

	ichar metadata, truename
	ref strec pequiv, stproc, owner, paramlist,nameptr

	kwd:=lx.symbol				!remember keyword
	isthreaded:=lx.subcode

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
	if insidedllimport then isglobal:=program_scope fi
	stproc.isthreaded:=isthreaded

	if truename then
		stproc.truename:=truename
	fi

	if stproc.name^='$' and eqstring(stproc.name,"$init") then
		moduletable[stmodule.moduleno].stinitproc:=stproc
	fi

	adddef(procowner,stproc)
	if stproc.nameid=dllprocid then
		stproc.isimport:=1
	fi

	owner:=stproc
	pushproc(stproc)

	lex()

	paramlist:=nil
	prettype:=tvoid
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
	endunless

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
		stproc.mode:=createtuplemode(procowner,(&.retmodes,nretvalues),0)
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
!		if procowner.nameid<>dllmoduleid then
!			println stproc.name,fflangnames[fflang]
!			serror("FF should be in dll import")
!		fi
	else			!assume this language
		case procowner.nameid
		when moduleid then
		when dllmoduleid then
			serror("Need FF specifier")
		esac
	esac
	stproc.isglobal:=isglobal
	stproc.varparams:=varparams
	stproc.fflang:=fflang

	if procowner=stmodule and 
		(stproc.namelen=5 and eqstring(stproc.name,"start")) or 
		(stproc.namelen=4 and eqstring(stproc.name,"main")) then
		stproc.isglobal:=export_scope
	fi

	popproc()

	return stproc
end

function readparams(ref strec procowner,owner,int fflang,&varparams,&nparams)ref strec=			!READPARAMS
!positioned at first symbol after '('
!read list of params, return that list
!syntax is a list of names and/or types
!each param can optionally be followed by a default value
!finish pointing at ")"
	ref strec stlist, stlistx, stname, d
	int parammode, pmode, m, pmprefix

	[30]char str
	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	pmprefix:=0
	parammode:=var_param

	if fflang=0 then fflang:=mlangff fi

	if lx.symbol in [koutsym,addrsym] then
		parammode:=out_param
		pmprefix:=1
		lex()
	elsif lx.symbol=insym then
		parammode:=in_param
		pmprefix:=1
		lex()
	fi

	if lx.symbol=namesym and nextlx.symbol in [commasym,rbracksym] then	!types only
		pmode:=readtypespec(procowner)
typesonly::
		return readparams_types(procowner,owner,fflang,varparams,nparams,pmode,parammode)
	else
		pmode:=readtypespec(procowner)
		if lx.symbol in [commasym,rbracksym] then			!types only
			goto typesonly
		fi
	fi

!types+names
	if pmprefix then
		serror("&/out must be applied to param name")
	fi

	goto gotmode

	do										!expect type of name at start of loop
		if istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
		fi
gotmode::

		case lx.symbol
		when insym then
			parammode:=in_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when koutsym,addrsym then
			parammode:=out_param
			lex()
			if lx.symbol=colonsym then lex() fi
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner,lx.symptr,paramid)
		adddef(owner,stname)
		lex()
		if parammode=out_param then
			m:=createrefmode(procowner,pmode)
		else
			m:=pmode
		fi

		storemode(owner,m,stname.mode)
		stname.parammode:=parammode
		addlistparam(&stlist,&stlistx,stname)
		parammode:=var_param

		case lx.symbol
		when assignsym then
			lex()
dodefvalue::
			stname.code:=readunit()
			stname.equals:=1
			stname.optional:=1
		when eqsym then
			lex()
			goto dodefvalue
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

function readparams_types(ref strec procowner,owner,int fflang,&varparams,&nparams,
			int pmode, parammode)ref strec=
!read types-only non-empty parameter list, only for ffi
!positioned at first symbol after '('
	ref strec stlist, stlistx, stname
	int firstparam,m

	[30]char str
	stlist:=stlistx:=nil
	stname:=nil
	nparams:=0
	goto gotmode

	do
		if lx.symbol=ellipsissym then
			varparams:=nparams+1		!pos of 1st varparam
			lex()
			checksymbol(rbracksym)
			exit
		fi

		pmode:=readtypespec(procowner)
gotmode::
		++nparams
		print @&.str,"$",,nparams
		stname:=getduplnameptr(owner,addnamestr(&.str),paramid)
		adddef(owner,stname)
		if parammode=out_param then
			m:=createrefmode(procowner,pmode)
		else
			m:=pmode
		fi

		storemode(owner,m,stname.mode)
		stname.parammode:=parammode
		addlistparam(&stlist,&stlistx,stname)
		parammode:=var_param

		case lx.symbol
		when assignsym,eqsym then
			lex()
			stname.code:=readunit()
			stname.equals:=1
		when namesym then
			serror("Can't mixed unnamed/named params")
		endcase

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=addrsym then
				parammode:=out_param
				lex()
			fi
		when rbracksym then
			exit
		else
			serror("typeparams3")
		endcase

	od
	return stlist
end

function readcondsuffix(unit p)unit=			!READCONDSUFFIX
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
		return createunit2(j_if,fixcond(readunit()),createunit1(j_block,p))
	when kunlesssym then
		lex()
		q:=createunit1(j_notl,fixcond(readunit()))
		q.pclop:=knotl
		return createunit2(j_if, q,createunit1(j_block,p))
	else
		return p
	endswitch
end

function readif:unit=
!at 'if'
	int pos1, kwd, pos2
	unit pthen,pcond, plist,plistx, pelse, p, pelsif

	pos1:=lx.pos

	kwd:=lx.symbol			!in case coming from elsecase etc

	lex()
	pcond:=fixcond(readsunit())
	skipsemi()

	checksymbol(kthensym)
	lex()

	pthen:=readsunit()

	if lx.symbol=kelsifsym then
		pos2:=lx.pos
		plist:=plistx:=createunit2(j_elsif,pcond,pthen)

		while lx.symbol=kelsifsym do
			pos2:=lx.pos
			lex()
			pcond:=fixcond(readunit())
			checksymbol(kthensym)
			lex()
			pthen:=readsunit()
			pelsif:=createunit2(j_elsif,pcond,pthen)
			pelsif.pos:=pos2
			addlistunit(&plist,&plistx,pelsif)

		od

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
			PELSE:=NIL
			checkend(kendsym,kwd,0)
			lex()
		esac

		p:=createunit2(j_longif,plist,pelse)
		p.pos:=pos1
		return p
	fi

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym,kwd)
		lex()
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym,kwd)
		lex()
	esac

	p:=createunit3(j_if,pcond,pthen,pelse)
	p.pos:=pos1
	return p
end

function readgoto(int gototag=j_goto)unit=	!READGOTO
	ref strec d
	unit p

	if lx.subcode=1 then		!go used
		lexchecksymbol(ktosym)
	fi
	lex()

	if lx.symbol=namesym and nextlx.symbol<>ptrsym and nextlx.symbol<>lsqsym and
		nextlx.symbol<>dotsym then			!assume simple label
		p:=createname(lx.symptr)

		lex()
	else
		serror("GOTO LABEL EXPR")
	fi

	return readcondsuffix(createunit1(gototag,p))
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
	p:=createunit3(j_if,q:=createunit1(j_notl,pcond),pthen,pelse)
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
			if p.tag=j_makerange then rangeused:=1 fi
			addlistunit(&pwhen,&pwhenx,p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		checksymbol(kthensym)
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(j_whenthen,pwhen,pthen)
		pwhenthen.pos:=pos2
		addlistunit(&pwhenlist,&pwhenlistx,pwhenthen)
	od

!	if opc=j_switch and not rangeused then
!		if nwhen<=8 then
!			opc:=j_case
!		fi
!	fi

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
		p:=createunit1(j_stop,readunit())
	else
		p:=createunit0(j_stop)
	fi
	return readcondsuffix(p)
end

function readreturn:unit=
	unit p,q,r

	lex()
	if exprstarter[lx.symbol] then
		q:=readunit()
		p:=createunit1(j_return,q)
		p.length:=1
	else
		p:=createunit0(j_return)
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
	p:=createunit1(j_do,p)
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

	p:=createunit3(j_to,pcount,pbody,createname(getavname(currproc,id)))
!p:=createunit2(j_to,pcount,pbody)
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

	p:=createunit3(j_while,pcond,pbody,pincr)
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
	p:=createunit2(j_repeat,pbody,pcond)
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
	when j_fprint,j_fprintln,j_cprint,j_cprintln then
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
		if not exprstarter[lx.symbol] and opc=j_cprintln then
			goto finish
		fi
		pformat:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

	if not exprstarter[lx.symbol] then
		goto finish
	fi

	do
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(&printlist,&printlistx, createunit0(j_nogap))
		when dollarsym then		!assume extra comma, meaning nogap
			addlistunit(&printlist,&printlistx, createunit0(j_space))
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
				p:=createunit2(j_fmtitem,p,readunit())
			fi
			if fshowname then
				expr:=strexpr(p)
				strbuffer_add(expr,"=")
				s:=expr.strptr
				iconvucn(expr.strptr,expr.length)

				addlistunit(&printlist,&printlistx,q:=createstringconstunit(s,expr.length))
			fi
			addlistunit(&printlist,&printlistx,p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish::
	inreadprint:=oldinreadprint
	if opc=j_print and printlist=nil then
		serror("No print items")
	fi
	if opc=j_fprint and printlist=nil and pformat=nil then
		serror("No print items")
	fi
	if opc=j_cprint and printlist=nil and pformat=nil then
		serror("No cprint items")
	fi

	if isfprint then
		if pformat=nil and opc<>j_cprintln then
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
		if opc=j_read then
			serror("@ on read")
		fi
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() fi
	fi

	if opc=j_readln then
		addlistunit(&readlist,&readlistx,createunit1(j_readln,pdev))
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

		pread:=createunit1(j_read,pformat)

!

		p:=createunit2(j_assign,p,pread)

		addlistunit(&readlist,&readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish::
	inreadprint:=oldinreadprint
	if opc=j_read and readlist=nil then
		serror("No read items")
	fi

	return makestmtblock(readlist)
end

function readtry:unit=
	unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
	++try_level
	lex()

	ptry:=readsunit()
	pexceptlist:=pexceptlistx:=nil			!list of j_except items

	while lx.symbol=kexceptsym do
		lex()
		exlist:=exlistx:=nil				!list of exception codes for this 'except'
		do
			addlistunit(&exlist,&exlistx,readconstexpr())
			if lx.symbol<>commasym then exit fi
			lex()
		od
		checksymbol(kthensym)
		lex()
		px:=readsunit()
		addlistunit(&pexceptlist,&pexceptlistx,createunit2(j_except,exlist,px))
	od
	checkend(kendsym,ktrysym)
	lex()

	--try_level

	return createunit2(j_try,ptry,pexceptlist)
end

function readraise:unit=
	unit p

	lex()
	p:=readunit()
	return createunit1(j_raise,p)
end

function readfor:unit=
!on 'for'; syntax is::
! for var [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
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

	opc:=j_forup
	pstep:=nil
	pcond:=nil

	if lx.symbol in [insym, inrevsym] then				!assume forall
		if lx.symbol=j_inrev then
			opc:=j_fordown				!tentative; may be changed to forall
		fi
		lex()

		plist:=readunit()

		if plist.tag=j_unary and plist.pclop=kbounds then
			pfrom:=getrangelwbunit(plist.a)
			pto:=getrangeupbunit(plist.a)
		elsif plist.tag=j_makerange then
			pfrom:=plist.a
			pto:=plist.b
		else
			opc:=(opc=j_forup|j_forall|j_forallrev)
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
		opc:=(lx.subcode=1|j_fordown|j_forup)
		lex()
		pto:=readunit()

		if lx.symbol=kbysym then
			lex()
			pstep:=readconstexpr(0)
			if pstep.tag=j_const then
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
		pbody:=makeblock(createunit2(j_if,pcond,pbody))
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
	when j_forup, j_fordown then
		if plocal then serror("for i,x?") fi
		pindex.avcode:='I'
		if pto.tag not in [j_const, j_name] then
			plocal:=createname(getavname(currproc))
			plocal.avcode:='I'
			ptoinit:=createunit2(j_assign, plocal, pto)
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

		passign:=createunit2(j_assign,duplunit(plocal),
					createunit2(j_index,duplunit(plist),duplunit(pindex)))
		plist.nextunit:=passign

		p:=createunit3(opc, pindex, plist, pbody)

	esac

	p.pos:=pos
	--nforloops
	return p
end

function readname:unit p=
	p:=readterm2()
	if p.tag<>j_name then serror("Name expected") fi
	return p
end

global proc readtypedef(ref strec owner,int isglobal=0)=
!at 'type' symbol
	ref strec sttype,stname
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

	sttype.isglobal:=isglobal
	storemode(owner,t,sttype.mode)

	if t>=0 then
		if ttisallnum[t] then
			tttarget[m]:=t
		elsif ttisref[t] then
		elsecase ttbasetype[t]
		when tarray then
		when tslice then
		when trecord then
		when tenum then
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

global proc readrecordfields(ref strec owner,int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
	int nvars
	ref strec stname,stbitfield

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

		when datsym then
			lexchecksymbol(intconstsym)
			case lx.value
			when 1,2,4,8,16 then
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

global proc readtabledef(ref strec owner,int isglobal=0)=
!at 'tabledata' symbol
	int i,ncols,nrows,enums,nextenumvalue,firstval,lastval,startline,closesym
	int ltype
	unit plower
	ichar enumtypename
	ref strec stvar,stenum,stgen
	const maxcols=20
	[maxcols]ref strec varnameptrs
	[maxcols]int varlisttypes
	[maxcols]unit plist,plistx
	const maxrows=500
	[maxrows]int enumvalues

	lex()
	enums:=0						!whether there is an enums column
	enumtypename:=nil

	if lx.symbol=lbracksym then		!tabledate(...) read enum type
		enums:=1
		lex()
		if lx.symbol=namesym then		!named type
			enumtypename:=lx.symptr.name
			lex()
		fi					!else unnamed type (just named constants)
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
		checksymbol(lbracksym)
		lex()
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
			stenum.isglobal:=isglobal
			adddef(owner,stenum)

			if nrows=1 then firstval:=nextenumvalue fi
			lastval:=nextenumvalue

			++nextenumvalue
			if ncols then				!comma always expected
				checksymbol(commasym)		!check it
			fi
			lex()
		fi

		for i:=1 to ncols do
			addlistunit(&plist[i],&plistx[i],readunit())
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
		stvar.code:=createunit1(j_makelist,plist[i])
		stvar.code.length:=nrows

		storemode(owner,varlisttypes[i],stvar.mode)
		stvar.isglobal:=isglobal

		adddef(owner,stvar)
		addstatic(stvar)
	od
end

global proc readclassdef(ref strec owner,int isglobal)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, baseclass, m, startline, closesym, mrec, normalexit,isrecord, align
	ref strec nameptr, sttype, newd, d,e

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

	sttype.isglobal:=isglobal
end

proc readclassbody(ref strec owner,int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
!classkwd=kclasssym or krecordsym
	int kwd,t
	ref strec d

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

	when kenumsym then
		lex()
		readenumtype(owner,0,0)

	when ktabledatasym then
		readtabledef(owner,0)

	when kmacrosym then
		readmacrodef(owner,0)

	when kstructsym,kunionsym then
		unionstr_append(&unionpend,(lx.symbol=kstructsym|'S'|'U'))
		unionlastvar:=nil
		lex()
	when kendsym,rbracksym,rcurlysym then
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
			serror("need type")
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
	enddoswitch
end

function readenumtype(ref strec owner,int typedefx,isglobal=0)int=		!READENUMTYPE
!read enum def, and return typespec corresponding
!typedefx is nil, or an existing, but not yet filled-in, moderec
!positioned at possible begin symbol (or at first declaration in the record)
!This is because it can be called in two ways::
!1: type name = enum <begin>...	Formal record definition
!2: enum [name=]<begin>...		Informal definition (where strictly only (...) allowed)
	ref strec enumowner, stname, nameptr
	int isanon, index, startline, closesym, knownindex
	unit pone,pindex

	enumowner:=owner			!owner of enum typeid
	isanon:=0
	if not typedefx then			!informal declaration
		if lx.symbol=namesym then		!name provided
			stname:=getduplnameptr(owner,lx.symptr,typeid)
			owner:=stname
			lex()
			checkequals()
			lex()
			adddef(enumowner,owner)
		else
			isanon:=1
		fi
		checksymbol(lbracksym)
		lex()
	else
		owner:=ttnamedef[typedefx]
		startline:=getcurrline()
		closesym:=checkbegin(1)
	fi

!now loop reading enum items
	pone:=createconstunit(1,tint)
	pindex:=pone
	knownindex:=1
	index:=1

	while lx.symbol=namesym do
		nameptr:=lx.symptr
		lex()
		if lx.symbol=eqsym then	!= follows
			lex()
			pindex:=readunit()
			knownindex:=0
			if pindex.tag=j_const then
				knownindex:=1
				index:=pindex.value
			fi
		fi

		if not isanon then
			stname:=getduplnameptr(owner,nameptr,enumid)
		else
			stname:=getduplnameptr(enumowner,nameptr,constid)
		fi

		if knownindex then
			pindex:=createconstunit(index,ti64)
			stname.code:=pindex
			++index
		else
			stname.code:=pindex
			pindex:=createunit2(j_bin,pindex,pone)
			pindex.pclop:=kadd
		fi
		stname.mode:=tint

		if not isanon then
			adddef(owner,stname)
		else
			adddef(enumowner,stname)
		fi

		stname.isglobal:=isglobal

		if lx.symbol<>commasym then exit fi
		lex()
	od

	if not typedefx then
		checksymbol(rbracksym)
		lex()
	else
		checkbeginend(closesym,kenumsym,startline)
	fi

	if not isanon then
		typedefx:=createenummode(owner,typedefx)
		return typedefx
	else
		return tvoid
	fi
end

proc readimportmodule(ref strec owner)=
!at 'importmodule' symbol
	int isnew,startline,closesym
	ref strec d,stname,stname0


	if insidedllimport then serror("nested importdll") fi

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
		if eqstring(stname.name,"sys") then
			stsysmodule:=stname
		fi
		adddef(stmodule,stname)
		if ndllnametable>=maxdlllib then
			serror("Too many DLL libs")
		fi
		dllnametable[++ndllnametable]:=stname.name
		stname.dllindex:=ndllnametable
	fi

	startline:=getcurrline()
	closesym:=checkbegin(0)

	insidedllimport:=1

	readimportbody(owner)

	insidedllimport:=0

	checkbeginend(closesym,kimportmodulesym,startline)

end

proc readimportbody(ref strec owner)=
!positioned at first symbol of statement (which can be empty)
!return knode containing statement, or nil if not found (at 'end etc)
	int pos,fflang

	pos:=lx.pos

	do
		skipsemi()
		switch lx.symbol
		when kfflangsym then
			fflang:=lx.subcode
			lex()
			case lx.symbol
			when kprocsym,kfunctionsym then
				readprocdecl(owner,0,fflang)
			esac

		when kprocsym,kfunctionsym then
			readprocdecl(owner,0,0)

		when ktypesym then
			readtypedef(owner,program_scope)

		when kconstsym then
			readconstdef(owner,program_scope)

		when kclasssym,krecordsym then
			readclassdef(owner,program_scope)

		when kmutsym then
			lex()
			readvardef(owner,program_scope,0,dllvarid, kmutsym)

		when eofsym then
			exit

		when kendsym then
			exit
		else
			PS("symbol")
			serror("Not allowed in importmodule")
		endswitch
	od
end

function readequivfield(ref strec owner)ref strec=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	ref strec p,d

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

function readrefproc(ref strec owner,int typedefx,int fflang)int=			!READREFPROC
!'ref' was seen, now positioned at 'proc' 'function' or 'method'
!read proc params and any result, return a complete ref proc spec
	int kwd,prettype,m,varparams,nparams
	[4]int retmodes
	ref strec paramlist,stproc
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

	return m
end

proc pushproc(ref strec p)=
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

function readassemline:unit=
	lex()
	return assembleline(1)
end

function readassemblock:unit=
!read lines of assembler after < or assem
!fend=1 when terminated by 'end', 0 when terminator is '>'
!return single nassem unit or nsunit containing multiple nassems
	unit ulist,ulistx,u

	ulist:=ulistx:=nil

	do
		lex()			!first symbol on line
		case lx.symbol
		when eofsym then
			serror("EOF: 'End' missing in Assembler code")
		when kendsym then
			checkend(lx.symbol,kassemsym)
			lex()
			exit
		when semisym then		!assume blank line
		else				!assume some asm code follows
			u:=assembleline(0)
			addlistunit(&ulist,&ulistx,u)
		esac
	od

	return makeblock(ulist)
end

function assembleline(int oneline)unit=
!1st symbol of possible assembler line has been read
!assemble following symbols, end at eol or other separater symbol
!return nassem unit

!const escapesym=atsym
	unit dlist,dlistx,p,pname,q
	ichar name
	int opc,noperands
	ref strec stname

	dlist:=dlistx:=nil

!look at the start of a line first

	if lx.symbol=namesym and nextlx.symbol in [colonsym,dcolonsym] then	!normal label
		p:=createunit0(j_labeldef)
		stname:=getduplnameptr(currproc,lx.symptr,labelid)
		p.def:=stname
		adddef(currproc,stname)
		lex()			!skip colon
		if oneline then
			lex()
		fi
		return p

	elsif lx.symbol=mulsym then		!*name	macro invocation
		lexchecksymbol(namesym)
		pname:=createname(lx.symptr)
		pname.pos:=lx.pos

		lex()
		if lx.symbol<>semisym then
			repeat
				addlistunit(&dlist,&dlistx,readunit())
				if lx.symbol=commasym then
					lex()
				fi

			until lx.symbol in [semisym,eofsym]
		fi

		return createunit2(j_assemmacro,pname,dlist)
	fi

	case lx.symbol
	when andlsym then
		opc:=m_andx
	doop::
		p:=createunit0(j_assem)
		p.asmopcode:=opc
		lex()
	when orlsym then
		opc:=m_orx
		goto doop

	when xorlsym then
		opc:=m_xorx
		goto doop

	when notlsym then
		opc:=m_notx
		goto doop

	elsif lx.symbol=namesym then				!assume opcode

		p:=createunit0(j_assem)

		case lx.subcode
		when asmopcodesym then
			p.asmopcode:=lx.symptr.index

		when jmpccsym then
			p.asmopcode:=m_jmpcc
			p.cond:=lx.symptr.index
		when setccsym then
			p.asmopcode:=m_setcc
			p.cond:=lx.symptr.index
		when movccsym then
			p.asmopcode:=m_cmovcc
			p.cond:=lx.symptr.index
		else
	PS("ASM")
			serror("x64 op expected")
		esac

		lex()
	else
	PS("ASM")
		SERROR("ASM???")
	esac

!any labels and opcodes have been read; now look at any operands
	if lx.symbol not in [semisym,eofsym] then

	noperands:=0

		do
			q:=readassemopnd()

			++noperands
			case noperands
			when 1 then p.a:=q; p.hasa:=1
			when 2 then p.b:=q; p.hasb:=1
			when 3 then p.c:=q; p.hasc:=1
			else
				serror("Too many asm opnds")
			esac

			if lx.symbol<>commasym then
				exit
			else
				lex()
			fi
		od

	fi

	checksymbol(semisym)

	return p
end

function readassemopnd:unit p =
!positioned at 1st symbol of an assem operand, which is not ; or eol or eof
	int reg,regix,scale,prefixmode
	unit pcode

	case lx.symbol
	when intconstsym,realconstsym then
		return readunit()
	when namesym then
		case lx.symptr.subcode
		when regsym then
			p:=createunit0(j_assemreg)
			p.index:=lx.symptr.index
			p.regsize:=lx.symptr.regsize
			lex()
			return p
		when xregsym then
			p:=createunit0(j_assemxreg)
			p.index:=lx.symptr.index
			lex()
			return p
		esac
		return readunit()
	when addsym, subsym then
		return readunit()

	when stdtypesym then
		case lx.subcode
		when tu8,tu16,tu32,tu64 then
		else
			serror("Bad prefix")
		esac
		prefixmode:=lx.subcode
		lexchecksymbol(lsqsym)
		goto gotprefix

	when lsqsym then
		prefixmode:=tvoid
gotprefix::
		reg:=regix:=0
		pcode:=nil
		scale:=1

		lex()
		if lx.symbol=namesym and lx.symptr.subcode=regsym then
			reg:=lx.symptr.index
			lex()
		fi

!		if lx.symbol=addsym and nextlx.symbol=namesym and nextlx().symptr.subcode=regsym then
		if lx.symbol=addsym and nextlx.symbol=namesym and nextlx.symptr.subcode=regsym then
			lex()
		fi
		if lx.symbol=namesym and lx.symptr.subcode=regsym then
			regix:=lx.symptr.index
			lex()
		fi

		if lx.symbol=mulsym then
			lexchecksymbol(intconstsym)
			case scale:=lx.value
			when 1,2,4,8 then
			else
				serror("Bad scale")
			esac
			lex()
		fi

		case lx.symbol
		when addsym, subsym, intconstsym, namesym, lbracksym,ksyscallsym then
			pcode:=readunit()
		esac
		checksymbol(rsqsym)
		lex()
		p:=createunit1(j_assemmem,pcode)
		if regix=0 and scale>1 then
			regix:=reg
			reg:=0
		fi
		if pcode=nil and reg+regix=0 then serror("Empty []") fi
		p.reg:=reg
		p.regix:=regix
		p.scale:=scale
		p.prefixmode:=prefixmode
		return p

	else
		PS("BAD OPND")
		serror("ASM: Bad operand?")
	esac
	return nil
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
		addlistunit(&ulist,&ulistx,pconst)
		++s
	od

	if lx.subcode='Z' then
		pconst:=createconstunit(0,ti64)
		addlistunit(&ulist,&ulistx,pconst)
		++length
	fi

	p:=createunit1(j_makelist,ulist)
	p.length:=length
	return p
end

function readreturntype(ref strec owner, []int &retmodes)int=
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
		return createunit1(j_makeset,nil)
	when colonsym then
		lexchecksymbol(rsqsym)
		lex()
		return createunit1(j_makedict,nil)
	esac

	length:=0
	nkeyvalues:=0

	ulist:=ulistx:=nil

	do
		oldirp:=inreadprint
		inreadprint:=0
		p:=readunit()
		inreadprint:=oldirp
		if p.tag=j_keyvalue then ++nkeyvalues fi
		++length

		addlistunit(&ulist,&ulistx,p)

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
		p:=createunit1(j_makedict,ulist)
	else
		p:=createunit1(j_makeset,ulist)
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

function readassignment:unit p=
	int pos,opc

	if lx.symbol=namesym and nextlx.symbol=assignsym then
			pos:=lx.pos
			p:=createname(lx.symptr)
			lex()
			lex()
			p:=createunit2(j_assign,p, readassignment())
			p.pos:=lx.pos
			return p
	fi

	p:=readorterms()

	if (opc:=lx.symbol)=assignsym then
		pos:=lx.pos
		lex()
		if lx.symbol=kemptysym then
			p:=createunit1(j_empty, p)
			lex()
		else
			p:=createunit2(j_assign,p,readassignment())
		fi
		p.pos:=pos
	fi
	return p
end

function readorterms:unit p=
	int pos

	p:=readandterms()

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=korlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_orl,p,readandterms())
		p.pclop:=korl
		p.pos:=pos
	od

	return p
end

function readandterms:unit p=
	int pos

	p:=readcmpterms()

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=kandlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_andl,p,readcmpterms())
		p.pclop:=kandl
		p.pos:=pos
	od

	return p
end

function readcmpterms:unit p=
	int pos,opc,n
	unit ulist,ulistx,q
	[4]byte genops

	p:=readinterms()

	if lx.symbol not in [eqsym,cmpsym] then
		return p
	fi

	ulist:=ulistx:=p
	p:=createunit1(j_cmpchain,p)
	n:=0				!n counts operand after the first
!	memset(&genops,0,genops.bytes)
	clear genops

	doswitch lx.symbol
	when eqsym, cmpsym then
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms()
		addlistunit(&ulist,&ulistx,q)
		q.pos:=pos
	else
		exit
	end doswitch

	if n=1 then
		p.tag:=j_cmp
		q:=p.a
		p.pclop:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
		p.hasb:=1
	else
		p.cmpgenop:=genops
	fi

	return p
end

function readinterms:unit p=
	int pos,opc

	p:=readrangeterm()

	doswitch lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(j_bin,p,readrangeterm())
		p.pclop:=opc
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readrangeterm:unit p=
	int pos,opc

	p:=readaddterms()

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(j_makerange,p,readaddterms())
		p.pos:=pos
	fi

	return p
end

function readaddterms:unit p=
	int pos,sym, tag, genop

	p:=readmulterms()

	doswitch sym:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym then
		pos:=lx.pos
		genop:=lx.subcode
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_bin,p,readmulterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readmulterms:unit p=
	int pos,sym

	p:=readpowerterms()

	doswitch sym:=lx.symbol
	when mulsym, divsym, idivsym, iremsym, shlsym, shrsym then
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_bin,p,readpowerterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readpowerterms:unit p=
	int pos

	p:=readterm2()

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(j_bin,p,readpowerterms())
		p.pclop:=kpower
		p.pos:=pos
	od

	return p
end

function readterm2:unit=
!	int oldinrp,lineno,opc
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
		if p.tag=j_syscall then
			p.a:=q; p.hasa:=1
		else
			p:=createunit2(j_callfn,p,q)
		fi
		inreadprint:=oldinrp
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(j_ptr,p)
		lex()

	when lsqsym then
		p:=readindex(p,0)

	when dotsym then
		p:=readdotsuffix(p)

	when colonsym then
		if inreadprint then exit fi
		lex()
		q:=readunit()
		p:=createunit2((inparamlist|j_keyword|j_keyvalue),p,q)

	when incrsym then
		case lx.subcode
		when kincr then opc:=kloadincr
		when kdecr then opc:=kloaddecr
		esac
		lex()
		p:=createunit1(j_incr,p)
		p.pclop:=opc

	when anddotsym then
		lexchecksymbol(lsqsym)
		lex()
		q:=readunit()
		if q.tag=j_makerange then
			p:=createunit2(j_anddotslice,p,q)
		else
			p:=createunit2(j_anddotindex,p,q)
		fi
		checksymbol(rsqsym)
		lex()

	else
		exit
	enddoswitch

	p.pos:=pos

	return p
end

function readterm:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t,length
	u128 aa

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
		if length>16 then serror("Char const too long") fi
		if length>8 then
			aa:=0
			memcpy(&aa,lx.svalue,length)
			p:=createconstunit(cast(&aa),tu128)
		else
			a:=0
			if length then
				memcpy(&a,lx.svalue,length)
			fi
			p:=createconstunit(a,tc64)
		fi
		p.istrueconst:=1
		lex()

	when lbracksym then
!CPL "READLB"
		p:=readlbrack()

	when stdtypesym,krefsym,kicharsym,ktypeofsym then
		p:=readcast()

	when addsym, subsym, minsym, maxsym, abssym, inotsym,
		mathsopsym, sqrtsym, sqrsym, maths2opsym,signsym then
		p:=readopc()

	when notlsym then
!		if nextlx().symbol=assignsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(j_notl, readterm2())
			p.pclop:=knotl
		fi

	when istruelsym then
!		if nextlx().symbol=assignsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(j_istruel, readterm2())
			p.pclop:=kistruel
		fi

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(j_incr,readterm2())
		p.pclop:=opc

	when ksprintsym then
		p:=readsprint()

	when ksreadsym,ksreadlnsym then
		p:=readsread()

	when addrsym then
		lex()
		p:=createunit1(j_addrof,readterm2())
		if p.a.tag=j_callfn then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when anddotsym then
		lex()
		p:=createunit1(j_addroffirst,readterm2())

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
			p:=createunit1(j_unary,dollarstack[ndollar])
			p.pclop:=kupb
		fi
		lex()

	when kcastsym then
		p:=readcastx()

	when ktypeconstsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=createunit0(j_typeconst)

		p.value:=readtypespec(currproc)
		checksymbol(rbracksym)
		lex()

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=j_makerange then
			r:=q.b
			q:=q.a
		else
			checksymbol(commasym)
			lex()
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(j_bin,p,q)
		q.pclop:=kmax
		p:=createunit2(j_bin,q,r)
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

	when ktrysym then	!todo
		p:=readtry()

	when kraisesym then	!todo
		p:=readraise()

	when kyieldsym then
		lex()
		p:=createunit1(j_yield,readunit())
		yieldseen:=1

	when kswapsym then			!swap using function syntax
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		checksymbol(rbracksym)
		lex()
		p:=createunit2(j_swap,p,q)

	when kevalsym then
		lex()
		p:=createunit1(j_eval,readunit())

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
		p:=createunit0(j_syscall)
		p.fnindex:=lx.subcode
		lex()

	when kstrincludesym then
		lex()
		p:=createunit1(j_strinclude,readterm2())

	when kemptysym then
		lex()
		p:=createunit1(j_empty, readterm2())

	else
		cpl symbolnames[lx.symbol],=LX.SYMBOL
		serror("readterm?")
	endswitch

	p.pos:=pos
	return p
end

function readxunit:unit=
	return readsunit()
end

function readsunit(int inwhile=0)unit=
	int pos,m,sym,opc
	unit ulist,ulistx,p,q,r
	ref strec stname

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

		when stdtypesym,lsqsym,krefsym,kicharsym,ktypeofsym,kdictsym,kslicesym then
			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
				goto doexec
			else
				sym:=0
				goto dovar
			fi

		when kmutsym,kletsym then
			sym:=lx.symbol
			lex()
	dovar::
			q:=readvardef(currproc,0,0,frameid,sym)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(&ulist,&ulistx,q)		!add one by-one
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

		when kenumsym then		!enum
			lex()
			readenumtype(currproc,0)

		when kmacrosym then
			readmacrodef(currproc,0)

		when ktabledatasym then
			readtabledef(currproc,0)

		when eofsym then
			cpl currproc.name
			serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
		when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,
				kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
			exit
!
		when namesym then
			case nextlx.symbol
			when dcolonsym then
				p:=createunit0(j_labeldef)
				stname:=getduplnameptr(currproc,lx.symptr,labelid)
				adddef(currproc,stname)
				p.def:=stname
				p.trylevel:=try_level
				lex()
				lx.symbol:=semisym
				addlistunit(&ulist,&ulistx,p)
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
			if p.tag=j_name and lx.symbol=namesym then
				serror("Possibly var/let needed")
			fi
			addlistunit(&ulist,&ulistx,p)
			if lx.symbol=kdosym then
				exit
			fi

		endswitch
	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,
		kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym,commasym,
		barsym, kstepsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then
		return createunit1(j_block,ulist)
	else
		return ulist
	fi
end

proc readmacrodef(ref strec owner, int isglobal)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported
!int kwd,varparams,try_level, prettype, nparams, rettype2, rettype3, nretvalues
!ichar metadata, truename
!ref strec pequiv, stproc, owner, paramlist,nameptr

	ref strec nameptr,stmacro, paramlist,paramlistx, stname

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
					stname.nulldef:=lx.symptr

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
	stmacro.isglobal:=isglobal

	checkequals()
	lex()
	stmacro.code:=readunit()
end

proc readimportalias(ref strec dimport)=
!positioned at 'as'; read following name as alias for the import module name
!implement as a macro
	ref strec stmacro

	lexchecksymbol(namesym)			!alias name to use
	stmacro:=getduplnameptr(stmodule,lx.symptr,macroid)
	adddef(stmodule,stmacro)

	lex()

	stmacro.paramlist:=nil
	stmacro.code:=createname(dimport)
end

proc domappedalias(ref strec dimport, stimport)=
!dimport is generic name as it appears in source
!stimport will be actual strec for module, with actual module name
!create an alias for actual name, so I can use the generic name
	ref strec stmacro

	if eqstring(dimport.name,stimport.name) then
		return
	fi

	stmacro:=getduplnameptr(stmodule,dimport,macroid)
	adddef(stmodule,stmacro)
	stmacro.paramlist:=nil
	stmacro.code:=createname(stimport)
end

function readrecase:unit=
	lex()
	if lx.symbol=kelsesym then
		lex()
		return createunit0(j_recase)
	else
		return createunit1(j_recase,readunit())
	fi
end

proc adddocstring(ichar s)=
	if ndocstrings>docstrings.len then
		serror("Too many docstrings")
	fi
	docstrings[++ndocstrings]:=pcm_copyheapstringn(s,strlen(s))
end

function fixcond(unit p)unit=
	if not isboolunit(p) then
		insertunit(p, j_istruel)
		p.pclop:=kistruel
	fi
	return p
end
