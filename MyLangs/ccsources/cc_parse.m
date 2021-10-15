!Parse C Code

import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables

import cc_lex
import cc_lib

import* cc_pcl

!const needcompoundblock=1
const needcompoundblock=0

ref strec ist_symptr

INT INTYPEOF

const maxtypemods=32
[maxnestedloops]byte looptypestack		!contains either 'L' or 'S' (loop or switch)
int loopindex							!current level of nested loop/switch blocks
[maxnestedloops]ref caserec casevaluestack		!linked list of case values for current switch

byte iscallbackfnx
byte constantseen=0
byte ingeneric=0

REF STREC CURRFNNAME


proc readmodule=
	int linkage,m,mbase,commaseen,wasdef
	unit p
	ref strec d
	ref paramrec pm
	int t,nitems,wasenum

	while lx.symbol<>eofsym do
		nitems:=0
		case lx.symbol
		when kshowtypesym then
			lex()
			t:=readcasttype(d,0,pm)
			skipsymbol(semisym)
			println "Type is:",Strmode(t)
			next
		when kmccassertsym then
			nitems:=1
		when semisym then
			serror("Extra semicolon 2")
		esac
		wasenum:=lx.symbol

		if lx.symbol=kmccassertsym then nitems:=1 fi

		mbase:=readdeclspec(stmodule,linkage)
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
				constantseen:=0
			elsif pm then
	readfn::
				if lx.symbol=lcurlysym and commaseen then serror("fn def after comma") fi

				d:=readfunction(d,m,linkage,pm,wasdef)
				if wasdef then exit fi			!can't have comma-separate fn defs

			elsif ttbasetype[m]=tproc then
				pm:=ttparams[m]
				m:=tttarget[m]
				constantseen:=0
				goto readfn

			else
				d:=readmodulevar(d,m,linkage)
				constantseen:=0
			fi

			case lx.symbol
			when commasym then			!read next item
				commaseen:=1
				lex()
			else
				skipsymbol(semisym)
				exit
			esac
		when kconstantsym then
			constantseen:=1
			lex()
			next 2
		when kstructinfosym then
			readstructinfosym()
		else
			case ttbasetype[mbase]
			when tenum, tstruct, tunion then		!assume defining a [part]type only
				skipsymbol(semisym)
				exit
			when tsint then				!allow for now, as it migt be an enum decl with no name
				skipsymbol(semisym)
				exit
			else
				serror_s("Decl error %s",typename(mbase))
			esac
		enddocase

		if nitems=0 and fmodern then
			case ttbasetype[mbase]
			when tstruct,tunion,tenum then
			else
				if wasenum<>kenumsym then
					CPL =STRMODE(MBASE)
					serror("Empty declaration")
				fi
			esac
		fi

	od
end

global function parsemodule(int n)int=
	int size,t
	ref strec owner
	real tsecs

	loopindex:=iscallbackfnx:=constantseen:=ingeneric:=0
	ist_symptr:=nil
	memset(&casevaluestack,0,casevaluestack.bytes)
!	clear casevaluestack

	startlex("PARSETEST",moduletable[n].fileno)
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
		int32 typeno				!not set, int, float, char, struct, union, enum etc
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
!	byte iscallback				!1 if $callback fnspec used
	end
	declrec d
	unit p
	int t,mod,m,fstruct
	ref paramrec pm
	ref strec e

	memset(&d,0,d.bytes)
!	clear d
	fstruct:=mod:=0

	doswitch lx.symbol
	when ktypespecsym then
		switch lx.subcode
		when ts_int, ts_char, ts_float, ts_double, ts_bool, ts_void then
			if d.typeno then
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
		when ts_complex then
			if d.typeno and d.typeno<>tfloat and d.typeno<>tdouble then
				goto tserror
			fi
			d.typeno:=tcomplex
		else

	tserror::
			serror_s("declspec/ts %s",typespecnames[lx.subcode])
		endswitch
		lex()

	when ktypequalsym then
		case lx.subcode
		when const_qual then
			d.isconst:=1
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
		when callback_fnspec then
			callbackflag:=1
!		d.iscallback:=1
		esac
		lex()
	when kstructsym,kunionsym then
		if d.typeno then serror("struct?") fi
		d.typeno:=readstructdecl(owner)
		d.isusertype:=1
		fstruct:=1

	when kenumsym then
		if d.typeno then serror("enum?") fi
!	d.typeno:=readenumdecl(owner)
		readenumdecl(owner)
		d.typeno:=tsint			!disregard enum 'type'; just use int
		d.isusertype:=1

	when namesym then			!should resolve to see if a user-type ...
								! ... unless a basetype already seen
		if not d.typeno and (m:=isusertype(owner)) then
			if mod then			!unsigned etc without proper base type; assume name is not part o it
				d.typeno:=tsint
				exit
			fi
!		if mod then serror("Can't mod usertype") fi
			d.typeno:=m
			d.isusertype:=1
			lex()
		else
			if d.typeno=0 and not mod then
				serror_s("Implicit decls not allowed: %s",lx.symptr.name)
			fi

			if d.typeno=0 then d.typeno:=tsint fi
			exit
		fi

	when ktypeofsym then
		lex()
		skipsymbol(lbracksym)
		intypeof:=1
		p:=readterm()
		intypeof:=0
		skipsymbol(rbracksym)
		if d.typeno or mod then serror("typeof") fi
		d.typeno:=p.def.mode

	when kmccassertsym then
		dostaticassert()
	else
		exit
	end doswitch

	t:=(d.typeno|d.typeno|tsint)

	if not d.isusertype then				!otherwise everything should be set up
		case t
		when tsint then
			if d.isshort then
				t:=(d.isunsigned|tushort|tsshort)
			elsif d.islong then
				if wintarget then
					t:=(d.isunsigned|tuint|tsint)
				else
					t:=(d.isunsigned|tullong|tsllong)
				fi
			elsif d.isllong then
				t:=(d.isunsigned|tullong|tsllong)
			elsif d.isunsigned then
				t:=tuint
			fi
!	when tuchar then
		when tschar then
			if d.isshort or d.islong or d.isllong then serror("char decl?") fi
!		t:=(d.issigned|tschar|tuchar)
			t:=(d.isunsigned|tuchar|tschar)
		when tdouble then
			if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("dbl decl?") fi
!long double not supported; just use double
!		t:=tldouble
		when tcomplex then
			if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("Complex?") fi

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

	switch lx.symbol
	when ktypespecsym then
		return 1
	when ktypequalsym then
		return 1
	when namesym then
		d:=resolvename((currproc|currproc|stmodule),lx.symptr,ns_general,currblockno)
		if d then
			lx.symptr:=d
			return d.nameid=typeid
		fi
	when kstructsym,kunionsym,kenumsym then
		return 1
	endswitch
	return 0
end

function istypestarter_next:int=
!return 1 when current symbol could start a type-spec
	ref strec d

	switch nextlx.symbol
	when ktypespecsym then
		return 1
	when ktypequalsym then
		return 1
	when namesym then
		d:=resolvename((currproc|currproc|stmodule),nextlx.symptr,ns_general,currblockno)
		if d then
			nextlx.symptr:=d
			return d.nameid=typeid
		fi
	when kstructsym,kunionsym,kenumsym then
		return 1
	endswitch
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
		p:=createunit1(j_exprlist,ulist)
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

	switch opc:=lx.symbol
	when assignsym, multosym, divtosym, remtosym, addtosym, subtosym,
			shltosym, shrtosym, iandtosym, ixortosym, iortosym then
	gotp::
		lex()
		oldpmode:=p.mode
		checklvalue(p)
		q:=readassignexpr()
		if ttisref[p.mode] then
			return createassignopref(opc,p,q)
		fi

		q:=coercemode(q,oldpmode)
		if ttconst[oldpmode] then
			terror("Modifying read-only var")
		fi

		if q.tag=j_convert and opc=assignsym then
			q.convtomem:=1
		fi

		if p.tag=j_ptr and p.a.tag=j_const then
			terror("Modifying constant?")
		fi


		r:=createunit2(symboltojtag[opc],p,q)

		r.mode:=oldpmode
		return r
	endswitch

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
			if pcond.tag=j_const and x.tag=j_const and y.tag=j_const then
				return (pcond.value|x|y)
			fi

		elsif s=tref and t=tref then
			u:=x.mode
		elsif s=tref and t=tsint and y.tag=j_const and y.value=0 then
			u:=x.mode
			coercemode(y,u)
		elsif s=tsint and t=tref and x.tag=j_const and x.value=0 then
			u:=y.mode
			coercemode(x,u)
		elsif s=tstruct and t=tstruct then
			u:=x.mode
		elsif s=tunion and t=tunion then
			u:=x.mode
		elsif s=t=tvoid then
			u:=tvoid
		else
			CPL Strmode(x.mode),Strmode(y.mode)
			terror("?: incompatible types")
		fi

		pcond:=createunit3(j_ifx,pcond,x,y)
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

		if x.tag=j_const and y.tag=j_const then
			x.value := (x.value or y.value|1|0)
			next
		fi
		x:=createunit2(j_orl,x,y)
		x.mode:=tsint
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

		if x.tag=j_const and y.tag=j_const then
			x.value := (x.value and y.value|1|0)
			next
		fi
		x:=createunit2(j_andl,x,y)
		x.mode:=tsint
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
			if u>=tfloat then terror("float|float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			terror("invalid | operands")
		fi

		if x.tag=j_const and y.tag=j_const then
			case u
			when tsint,tsllong,tuint,tullong then
				x.value ior:= y.value
				next
			esac
		fi
		x:=createunit2(j_ior,x,y)
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
			if u>=tfloat then terror("float^float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			terror("invalid ^ operands")
		fi

		if x.tag=j_const and y.tag=j_const then
			case u
			when tsint,tsllong,tuint,tullong then
				x.value ixor:= y.value
				next
			esac
		fi
		x:=createunit2(j_ixor,x,y)
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
			if u>=tfloat then terror("float&float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			cpl Strmode(x.mode)
			cpl Strmode(y.mode)
			terror("invalid & operands")
		fi

		if x.tag=j_const and y.tag=j_const then
			case u
			when tsint,tsllong,tuint,tullong then
				x.value iand:= y.value
				next
			esac
		fi
		x:=createunit2(j_iand,x,y)
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
		elsif s=tref and t=tsint then
			if y.tag<>j_const or y.value<>0 then
				terror("Can't compare pointer to int")
			fi
		elsif s=tsint and t=tref then
			if x.tag<>j_const or x.value<>0 then
				terror("Can't compare pointer to int2")
			fi
		else
			terror("invalid == operands")
		fi

		if x.tag=j_const and y.tag=j_const then
			case u
			when tsint,tsllong,tuint,tullong,0 then			!0 when ref/ref ref/int int/ref
				if opc=eqsym then
					x.value := x.value = y.value
				else
					x.value := x.value <> y.value
				fi
				next
			esac
		fi
		x:=createunit2(symboltojtag[opc],x,y)
		x.mode:=tsint
	od
	
	return x
end

function readrelexpr:unit=
	unit x,y
	int opc,s,t,u
	int64 a,b,c
	word64 aa,bb,cc

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
			PRINTUNIT(NIL,X)
			PRINTUNIT(NIL,Y)
			CPL =STRMODE(X.MODE)
			CPL =STRMODE(Y.MODE)
			CPL =SYMBOLNAMES[OPC]

			terror("invalid rel operands")
		fi

		if x.tag=j_const and y.tag=j_const then
			a:=x.value; b:=y.value
			case u
			when tsint,tsllong then
				case opc
				when ltsym then c:=a<b
				when lesym then c:=a<=b
				when gesym then c:=a>=b
				else            c:=a>b
				esac
				x.value:=c
				next
			when tuint,tullong then
				aa:=x.value; bb:=y.value
				case opc
				when ltsym then cc:=aa<bb
				when lesym then cc:=aa<=bb
				when gesym then cc:=aa>=bb
				else            cc:=aa>bb
				esac
				x.value:=cc
				next
			esac
		fi

		x:=createunit2(symboltojtag[opc],x,y)
		x.mode:=tsint
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
		y:=coercemode(y,tsint)
!
		if x.tag=j_const and y.tag=j_const then
			case u
			when tsint,tsllong then
				if opc=shlsym then
					x.value := x.value << y.value
				else
					x.value := x.value >> y.value
				fi
				next
			when tuint,tullong then
				if opc=shlsym then
					x.uvalue := x.uvalue << y.value
				else
					x.uvalue := x.uvalue >> y.value
				fi
				next
			esac
		fi
		x:=createunit2((opc=shlsym|j_shl|j_shr),x,y)
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
	int t,u,opc,shift,newlen,slength,tbase,fwide,newmode
	ref char pbyte
	int64 a
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
				serror_s("Undefined name \"%s\"", getstname(lx.symptr))
			fi
		else
			d:=lx.symptr
		fi

		d.attribs.ax_used:=1
		case d.nameid
		when enumid then
			p:=createconstunit(d.index,tsint)
		when constantid then
			p:=createconstunit(d.code.value,d.mode)
		when procid then
			if nextlx.symbol<>lbracksym then
				p:=createunit0(j_funcname)
				p.def:=d
				p.mode:=createrefmode(createprocmode(d.mode,d.paramlist))
			else
				goto doname
			fi

		else
	doname::
			p:=createname(d)
			p.mode:=t:=d.mode
			if ttbasetype[t]=tarray then
				if not intypeof then
					p.alength:=ttlength[t]
					p:=createaddrofop(p)
					p.mode:=createrefmode(tttarget[t])
				fi
			elsif d.nameid<>procid and d.nameid<>constantid and ttsize[t]<4  then
				fixmemopnd(p)
			elsif d.nameid=paramid then
				if isstructunion(p.mode) then
					p.lineno:=lx.lineno
					p.mode:=createrefmode(p.mode)
					p:=createptrop(p)
					p.mode:=d.mode
				fi
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
		    p.slength:=slength
			p.mode:=trefchar

		fi

		lex()

	when kstrincludesym then
		p:=readstrinclude()

	when charconstsym then
		a:=0
		shift:=0
		pbyte:=lx.svalue
		if lx.length>8 then serror("char const too long") fi

		to lx.length do
			a:=a ior word64(pbyte^)<<shift
			shift+:=8
			++pbyte
		od
		p:=createconstunit(a,(lx.length<=4|tsint|tsllong))
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
		p:=createunit1(j_notl,p)
		p.mode:=tsint

		if p.a.tag=j_notl and p.a.a.tag=j_notl then
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

	when sqrtsym then
		lex()
		skipsymbol(lbracksym)
		p:=createsqrtop(readexpression())
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
		lex()
		if lx.symbol=lbracksym then		!possible type
			if istypestarter_next() then
				lex()
				t:=readcasttype(d,0,pm)
				skipsymbol(rbracksym)
				p:=createconstunit(ttsize[t],tullong)
			else
				p:=readterm()
				p:=createsizeofop(p)
			fi
		else
			p:=createsizeofop(readterm())
		fi

	when klengthofsym then
		lex()
		if lx.symbol=lbracksym then		!possible type
			lex()
			if istypestarter() then
				t:=readcasttype(d,0,pm)
				skipsymbol(rbracksym)
				p:=createconstunit(ttlength[t],tsint)
			else
				p:=readexpression()
				skipsymbol(rbracksym)
				p:=createlengthofop(p)
			fi
		else
			p:=createlengthofop(readterm())
		fi
	when kgenericsym then
		p:=readgeneric()
	when kalignofsym then
		serror("rt/alignof")
	when kstrtypesym then
		lex()
		skipsymbol(lbracksym)
		t:=readcasttype(d,0,pm)
		skipsymbol(rbracksym)
		p:=createstringconstunit(pcm_copyheapstring(Strmode(t)),-1)
	when kcputimesym then
		p:=createunit0(j_cputime)
		p.mode:=tsllong
		lex()

	else
	PS("RT")
		serror("Readterm?")
	endswitch

!look at the suffix

	doswitch lx.symbol
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
		p:=createincrop(j_postincr,p)

	when decrsym then
		lex()
		p:=createincrop(j_postdecr,p)
	else
		exit
	enddoswitch

	return p
end

function readexprlist(unit p)unit=
! read comma-separated list, and return head of list (not as j_makelist etc)
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
			serror_ss("var: name in use %s %s",e.name,namenames[e.nameid])
		fi
		emode:=e.mode
		if emode<>m then
			if not comparemode(emode,m) then
	redef::
				serror_s("var: redefining %s",e.name)
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
		d:=createdupldef(stmodule,d,(constantseen|constantid|staticid))
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
			serror_s("Can't init twice %s",d.name)
		fi
		if scope=imported_scope then
			serror_s("Can't init extern %s",d.name)
		fi
		lex()
		d.code:=readinitexpr(stmodule,d.mode)

		if d.nameid=constantid then
			unless tfirstint<=ttbasetype[d.mode]<=tlastreal then
				serror("constant only for int/float")
			end unless
			if d.code.tag<>j_const then
				serror("constant expr must be constant")
			fi
		fi

	elsif constantseen then
		serror("constant must be initialised")
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
			serror_s("var: name in use %s",e.name)
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
			serror_s("Can't init twice %s",d.name)
		fi
		if scope=imported_scope then
			serror_s("Can't init extern %s",d.name)
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
			if i=1 then				!indicate to caller that this is a normal function
				pm:=modvalue[1]
			else					!assume fu nction pointer of some sort
				m:=createprocmode(m,modvalue[i])
			fi
		esac
	od

	return m
end

proc readnamedtype(ref strec owner, &d,
			[]int &modtype, []ref void &modvalue, int &nmodifiers)=
	int length
	[maxtypemods]int fconst
	int nrefs
	unit pdim

	d:=nil
	nrefs:=0

	if lx.symbol=kfnspecsym then
		lex()				!ignore $callback etc (not needed in a type decl, only a function def)
	fi

!accumulate pointers
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
	when lbracksym then			!don't know how this would work...
		lex()
		readnamedtype(owner,d,modtype,modvalue,nmodifiers)
		skipsymbol(rbracksym)
	esac

	docase lx.symbol
	when lsqsym then			!array
		lex()
		if lx.symbol=rsqsym then
			length:=0
		else
			pdim:=readassignexpr()
			if pdim.tag=j_const then
				length:=pdim.value
			else

				serror("Can't do VLAs")
			fi
			checksymbol(rsqsym)
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
	enddocase

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
	when j_const then
		return p.value

	else
		serror_s("readconstint %s",jtagnames[p.tag])
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
			if ttbasetype[melem]=tuchar and lx.symbol=stringconstsym then
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

				if ttbasetype[melem]=tarray and ttbasetype[tttarget[melem]]=tuchar and p.mode=trefchar then
				else
					p:=coercemode(p,melem)
				fi
			when tstruct then

				mm:=e.mode

				if ttbasetype[mm]=tarray and ttbasetype[tttarget[mm]]=tuchar and p.mode=trefchar then
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

	donestruct::
		skipsymbol(rcurlysym)
		p:=createunit1(j_makelist,ulist)
		p.count:=count

		p.mode:=m

	else
		braces:=0
		case mbase
		when tarray then
	doarraystring::
			if lx.symbol<>stringconstsym and lx.symbol<>wstringconstsym and 
				lx.symbol<>kstrincludesym and tttarget[m]<>tuchar then
				terror("{} initialiser expected")
			fi

			p:=readassignexpr()
			if p.tag=j_const then p.strarray:=1 fi
			case p.mode
			when trefchar then
			when trefwchar then
			else
				terror("Array init")
			esac
			P.MODE:=M

			if (dim:=ttlength[m])=0 then
				ttlength[m]:=ttsize[m]:=p.slength+1
			else
				if p.slength>dim then
					terror("Init str too long")
				fi
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

		if p=nil then next fi				!might have been typedef etc
		if p.tag=j_tempdecl then
			repeat
				q:=p.nextunit
				if p.def.code and p.def.nameid<>staticid then
					p.tag:=j_decl
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
	return createunit3(j_block,ulist,nil,ulistx)
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
				p:=createunit0(j_break)
				lex()
			else
				p:=createunit0(j_breaksw)
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

		p:=createunit0(j_continue)
		lex()

	when kcasesym then
		return readcaselabel()

	when kdefaultsym then
		lex()
		skipsymbol(colonsym)
		return createunit1(j_defaultstmt,readstatement())

	when kshowmodesym then
		lex()
		p:=readexpression()
		ss:=strexpr(p)
		print "Mode is:",ss.strptr,":",Strmode(p.mode),"	on line",lx.lineno,
			"Size is",ttsize[p.mode]
		if ttisref[p.mode] then
			print " target size",ttsize[tttarget[p.mode]]
		fi
		println

	when kmccassertsym then
		dostaticassert()

	when semisym then
		lex()
		return nil

	when namesym then
		if nextlx.symbol=colonsym then
			p:=createunit1(j_labelstmt,nil)
			d:=resolvename(currproc,lx.symptr,ns_labels,0)
			if d then
				if d.index then
					cpl lx.symptr.name
					terror("Duplicate label")
				else
					d.index:=++labelno
				fi
			else
				d:=createdupldef(currproc,lx.symptr,labelid)
				d.mode:=tvoid
				d.index:=++labelno
			fi

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
			if isusertype(currproc) then
				goto doreaddecl
			fi
			if ist_symptr then lx.symptr:=ist_symptr fi		!make use of name resolve done by isusertype
			p:=readexpression()
		fi
	when ktypespecsym, ktypequalsym, klinkagesym, kfnspecsym,
		kstructsym,kunionsym,kenumsym,ktypeofsym then
	doreaddecl::
		return readlocaldecl()

	else						!assume expression
		p:=readexpression()
	endswitch

	skipsymbol(semisym)

	return p
end

function readifstmt:unit=
	unit pcond,pbody,pelse

	lex()
	pcond:=readcond()
	coercecond(pcond)

	pbody:=readblock()

	pelse:=nil

	if lx.symbol=kelsesym then
		lex()
		pelse:=readblock(1)
	fi

	return createunit3(j_if,pcond,pbody,pelse)
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
					p:=createunit0(j_decl)
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
			enddocase
			pinit:=createunit3(j_block,ulist,nil,ulistx)

		else
			pinit:=readexpression()
		fi
	else
		pinit:=createunit0(j_null)
	fi
	skipsymbol(semisym)

	if lx.symbol<>semisym then
		pcond:=readexpression()
		coercecond(pcond)
	else
		pcond:=createunit0(j_null)
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

	return createunit2(j_for, pinit, pbody)
end

function readwhilestmt:unit=
	unit pcond,pbody

	lex()
	pcond:=readcond()
	coercecond(pcond)
	pushloop('L')
	pbody:=readblock()
	poploop()

	return createunit2(j_while,pcond,pbody)
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
		return createunit2(j_dowhile,pbody,pcond)
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

	return createunit1(j_return,p)
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
	p:=createunit1(j_goto,nil)
	p.def:=d
	lex()				!skip colon
	skipsymbol(semisym)
	return p
end

function readswitchstmt:unit=
	unit pindex,pstmt,p

	lex()
	pindex:=readcond()			!not a condition, but it doesn't matter
	coercemode(pindex,tsint)

	pushloop('S')
	pstmt:=readblock()			!not a condition, but it doesn't matter
	p:=createunit2(j_switch, pindex, pstmt)
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

	p:=createunit1(j_casestmt,readstatement())
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
!return typeno if it resolves to a user type, otherwise 0
!will peek at following symbol, and returns 0 if "," or ";" follows
	ref strec d

	d:=resolvename(owner,lx.symptr,ns_general,currblockno)
	if d then
		if d.nameid=typeid then
			return d.mode
		fi
		ist_symptr:=d
	fi
	return 0
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
!PS("RLD")
			if lx.symbol=lcurlysym then
				serror("Nested function")
			fi
			d:=readfunction(d,m,linkage,pm,wasdef)
		else
			d:=readframevar(d,m,linkage)
			p:=createunit0(j_tempdecl)
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
	when tsint then
		skipsymbol(semisym)
		exit

		else
			serror_s("Local decl error %s",typename(m))
		esac
	enddocase

	if nitems=0 and fmodern then
		case ttbasetype[mbase]
		when tstruct,tunion,tenum then
		else
			if wasenum<>kenumsym then
				serror("Empty local declaration")
			fi
		esac
	fi

	return ulist
end

function createtypedef(ref strec owner, symptr, int mode)ref strec=
!symptr is a generic symbol for the name
	ref strec d

	d:=checkdupl(owner,symptr,ns_general,currblockno)

	if d then			!existing name
		if d.nameid<>typeid then
			serror_s("Typedef name in use %s",d.name)
		fi

		if d.mode<>mode then
			if not comparemode(d.mode, mode) then
				serror_s("Typedef redefined or can't match types %s",d.name)
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

	ulist:=ulistx:=nil
	variadic:=nparams:=nnames:=0

	if callbackflag then			!lex flag is out of step with parser
		iscallbackfnx:=1
		callbackflag:=0
	fi
	lastbasetype:=tvoid

	int names:=0, nonames:=0,reported:=0
	++NALLPROCS

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

		if d then
	 names:=1 else nonames:=1 fi

	if names and nonames and not reported then
	++NMIXED
		reported:=1
	fi

		if d then
			++nnames
			q:=ulist
			while q do
				if q.def=d then
					serror_ss("Param name reused %s %s",d.name,namenames[d.nameid])
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
		if fmodern then
			terror("() Params not allowed")
		else
			flags:=pm_notset
		fi
	elsif nparams=1 and m=tvoid then
		flags:=pm_empty
		nparams:=0
		ulist.mode:=tnone
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
			serror_s("NAME not allowed in cast type %s",d.name)
		fi
	esac

	return m
end

function readfunction(ref strec d, int m, linkage, ref paramrec pm, int &wasdef)ref strec=
!have read function declaration, with ";" or "{" next
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

	CURRFNNAME:=D
	f:=checkdupl(owner, d, ns_general, 0)

	if f then					!already exists
		if f.nameid<>procid then
			serror_s("fn: name in use %s",d.name)
		fi
!COMPARE PARAM LISTS...
!	if e.paramlist<>pm then
!		serror("fn: params don't match previous")
!	fi
		d:=f

!see how scope interacts with existing decl
		scope:=d.scope
		if scope=local_scope and linkage=none_ss or
		   scope=exported_scope and linkage=static_ss or
		   scope=imported_scope and linkage=static_ss then
!*!		serror("Linkage3 mismatch")
		elsif scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
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

	if iscallbackfnx then
		d.attribs.ax_callback:=1
		iscallbackfnx:=0
	fi

	d.paramlist:=pm
	d.scope:=scope

	if lx.symbol=lcurlysym then
		wasdef:=1
		if d.code then
			serror_s("Can't define function twice %s",d.name)
		fi
		if scope=imported_scope then
!		serror("Can't define imported function")
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
	if pm.def then			!params are named
		to pm.nparams do
			if pm.def=nil then
				serror("Param name missing")
			fi
			e:=createdupldef(f,pm.def,paramid)
			e.blockno:=1
			e.mode:=pm.mode
			pm:=pm.nextparam
			pmcount:=1
		od
	elsif pm.nparams then
		serror("Param names missing")
	fi

	p:=readcompoundstmt(pmcount)
	currproc.code:=p
	currproc:=nil
end

function createnegop(unit p)unit=
	unit q
	int t

	t:=p.mode

	if p.tag=j_const then
		case t
		when tsint,tsllong,tullong then
			p.value:=-p.value
			return p
		when tuint then
			p.value:=(-p.value) iand 0xFFFF'FFFF
			return p
		when tdouble then
			p.xvalue:=-p.xvalue
			return p
		esac
	fi
	retry::
	if t>=tfirstnum and t<=tlastnum then
		coercebasetype(p)
		q:=createunit1(j_neg,p)
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
	else
	CPL Strmode(t)
		terror("neg bad type")
	fi

	q.mode:=p.mode
	return q
end

function createabsop(unit p)unit=
	unit q
	int t

	t:=p.mode

	if p.tag=j_const then
		case t
		when tsint,tsllong then
			p.value:=abs(p.value)
			return p
		esac
	fi

!if t>=tfirstint and t<=tlastint then
	if isintcc(t) then
		coercebasetype(p)
		q:=createunit1(j_abs,p)
	else
		terror("abs bad type")
	fi

	q.mode:=p.mode
	return q
end

function createsqrtop(unit p)unit=
	unit q
	int t

	t:=p.mode

	if p.tag=j_const then
		case t
		when tfloat,tdouble then
			p.value:=sqrt(p.xvalue)
			return p
		esac
	fi

	coercemode(p,tdouble)

	q:=createunit1(j_sqrt,p)
	q.mode:=tdouble

	return q
end

function createinotop(unit p)unit=
	unit q
	int t

	t:=ttbasetype[p.mode]

	if p.tag=j_const then
		case t
		when tsint,tsllong,tuint,tullong then
			p.value:=inot p.value
			return p
		esac
	fi
	if isintcc(t) then
		coercebasetype(p)
		q:=createunit1(j_inot,p)
	else
	cpl Strmode(t)
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
	when j_addrof then
		q:=p.a
		if p.alength then
			q.mode:=tttarget[p.mode]
		fi
		return q
	esac

	q:=createunit1(j_ptr,p)
	q.mode:=m
	q:=arraytopointer(q)
	fixmemopnd(q)

	return q
end

function createincrop(int opc,unit p)unit=
!opc is j_preincr/decr or j_postincr/decr
	unit q
	int t

	t:=p.mode

	checklvalue(p)

	unless isintcc(t) and t<>tbool or ttisref[t] then
		terror("++ bad type")
	endunless
	q:=createunit1(opc,p)
	q.mode:=p.mode

	return q
end

function createlengthofop(unit p)unit=
	unit q
	int t,size

	t:=p.mode
	switch p.tag
	when j_name then
		size:=ttlength[p.def.mode]			!take account of array

	when j_const then
		if t=trefchar then					!const string
			size:=p.slength+1
		else
			size:=ttlength[t]
		fi

	when j_ptr then
		if ttisref[t] and p.alength then		!result of array=>ptr conversion
			size:=ttlength[tttarget[t]]*p.alength
		else
			size:=ttlength[t]
		fi
	when j_widenmem then
		return createsizeofop(p.a)

	else
		size:=ttlength[t]
	endswitch

	q:=createconstunit(size,tsint)
	return q
end

function createaddrofop(unit p)unit=
	ref strec d
	unit q
	int t,u,alength

	alength:=0

	restartx::
	t:=p.mode
	switch p.tag
	when j_name then
		if p.alength then
			t:=p.def.mode
			alength:=p.alength
		fi

	when j_addrof then
		if p.a.tag=j_name and p.a.alength then		!sounds like ANAME => &ANAME
			p.mode:=createrefmode(p.a.def.mode)
	p.alength:=p.a.alength
			return p
		fi
	when j_dot then
		q:=p.a
		if q.tag=j_ptr and q.a.tag=j_const then
			p:=createconstunit(p.offset+q.a.value, tsint)
			return p
		fi
		goto cad1
	when j_addptr then
		if p.alength then
			p.mode:=createrefmode(createarraymode(tttarget[p.mode],p.alength))
			return p
		fi
	when j_widenmem then
		p:=p.a
		goto restartx
	when j_funcname then
		return p
	else

	cad1::
		checklvalue(p)
	endswitch

	p:=createunit1(j_addrof,p)
	p.mode:=createrefmode(t)
	p.alength:=alength

	return p
end

function createaddop(unit x,y)unit=
	unit z
	int s,t,u,opc,elemsize

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]
	opc:=j_add

	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)

	elsif s=tref then
	doaddref::
		u:=x.mode
		elemsize:=ttsize[tttarget[u]]
		if x.tag=j_const and y.tag=j_const then
			x.value +:=y.value*elemsize
			return x
		fi

		y:=coercemode(y,tptroffset)

		z:=createunit2(j_addptr,x,y)
		z.mode:=u
		z.ptrscale:=elemsize
		return z

	elsif t=tref then
		swap(x,y)
		goto doaddref
	else
		terror("Sub bad types")
	fi

	if x.tag=j_const and y.tag=j_const then
		return eval_add(opc,x,y,u)
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
	opc:=j_sub

	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	elsif s=tref then
		if t<>tref then
			u:=x.mode
			elemsize:=ttsize[tttarget[u]]
			y:=coercemode(y,tptroffset)

			z:=createunit2(j_subptr,x,y)
			z.mode:=u
			z.ptrscale:=elemsize
			return z

		else							!ref-ref
			if x.tag=j_const and y.tag=j_const then
				x.value -:= y.value/ttsize[tttarget[x.mode]]
				x.mode:=tsint
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

	if x.tag=j_const and y.tag=j_const then
		return eval_sub(opc,x,y,u)
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

	opc:=j_mul
	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Mul bad types")
	fi

	if x.tag=j_const and y.tag=j_const then
		return eval_mul(opc,x,y,u)
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

	opc:=j_div
	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Div bad types")
	fi

	if x.tag=j_const and y.tag=j_const then
		return eval_div(opc,x,y,u)
	elsif y.tag=j_const and u=tdouble then
		opc:=j_mul
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

	opc:=j_rem
	if u:=dominantmode[s,t] then			!were both numeric
		if u=tdouble or u=tfloat then
			u:=tsint
		fi
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Rem bad types")
	fi

	if x.tag=j_const and y.tag=j_const then
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
	p.simple:=0
	p.nextunit:=q.nextunit

	q.nextunit:=nil
	end

	function eval_add(int opc,unit x,y,int t)unit=
	unit z

	case t
	when tsint,tsllong,tuint,tullong then
		x.value +:= y.value
		return x
	when tdouble then
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
	when tsint,tsllong,tuint,tullong then
		x.value -:= y.value
		return x
	when tdouble then
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
	when tsint,tsllong,tsshort,tschar then
		x.value *:= y.value
		return x
	when tuint,tullong,tushort,tuchar then
		x.uvalue := x.uvalue*y.uvalue
		return x
	when tdouble then
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
	when tsint,tsllong then
		if y.value=0 then serror("div 0") fi
		x.value := x.value/y.value
		return x
	when tuint,tullong then
		if y.value=0 then serror("div 0") fi
		x.uvalue := x.uvalue/y.uvalue
		return x
	when tdouble then
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
	when tsint,tsllong then
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
	dosoft::
		p.mode:=t
		return 1
	fi

	s:=p.mode
	if s=t then return 1 fi

	case s
	when tsint,tsshort,tschar,tsllong then
		case t
		when tdouble,tfloat then
			p.xvalue:=p.value
			p.mode:=t
			return 1
		when tullong,tsllong,tuint,tsint,tsshort,tschar,tuchar,tushort then
	dotrunc::
			case ttsize[t]
			when 1 then p.value iand:=255
			when 2 then p.value iand:=65535
			when 4 then p.value :=p.value iand 0xFFFF'FFFF
			esac

			goto dosoft
		esac
		if ttisref[t] then
			p.mode:=t
			return 1
		fi

	when tuint,tuchar,tushort,tullong then
		case t
		when tdouble,tfloat then

	RETURN 0
			p.mode:=t
			return 1
		when tullong,tsllong,tsint,tuint,tullong,tushort,tschar,tuchar,tsshort then
			goto dotrunc
		esac
		if ttisref[t] then
			p.mode:=t
			return 1
		fi

	when tdouble then
		case t
		when tsint,tsllong then
			p.value:=p.xvalue
			p.mode:=t
			return 1
		when tuint,tullong then
			p.value:=p.xvalue
			p.mode:=t
			return 1
		when tfloat then
			p.mode:=tfloat
			return 1
		esac
	elsif ttisref[p.mode] then
		if not p.isstrconst then
			case t
			when tsint,tsllong,tuint,tullong then
				p.mode:=t
				return 1
			esac
		fi
	esac

	return 0
end

proc coercecond(unit p)=
!p is an expression used as a condition
!make sure it is an int, or convert it to one
	int t
	if (t:=p.mode)=tsint then return fi

	retry::
	case ttbasetype[t]
	when tfloat,tdouble,tref then
		goto doint

	elsif isintcc(t) then
	doint::
		if p.tag=j_const and p.value then			!check all types as one 64-bit field
			p.value:=1
		elsif p.tag=j_const and not p.value then			!check all types as one 64-bit field
			p.value:=0
		else
			insertunit(p,j_istruel)
		fi
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
	else

		serror_s("Invalid condition %s",Strmode(t))
	esac
	p.mode:=tsint
end

proc coercebasetype(unit p)=
	int t

	if (t:=p.mode)>=tschar and t<=tsshort then
		p:=coercemode(p,tsint)
	elsif t>=tbool and t<=tushort then
		p:=coercemode(p,tuint)
	fi
end

proc checklvalue(unit p)=
	case p.tag
	when j_name then
		if p.def.nameid=constantid then
			serror("'constant' name can't be lvalue")
		fi
	when j_ptr then

	when j_funcname then

	when j_widenmem then
		case p.a.tag
		when j_name,j_ptr,j_dot then
			p^:=p.a^
		else
			terror("CHECKLV/WIDEN")
		esac

	when j_dot then

	when j_const then
		if not ttisref[p.mode] then
			goto notlv
		fi
	when j_convert then
		if p.a.tag=j_name then
!		p^:=p.a^
			return
		fi

	else
	notlv::
		printunit(nil,p)
		terror_s("Not lvalue: %s",jtagnames[p.tag])
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
	when j_ptr then
	doptr::
		mproc:=p.mode

		while ttbasetype[mproc]=tref do
			r:=createunit1(j_ptr,p)
			mproc:=tttarget[mproc]
			r.mode:=mproc
			p:=r
		od

		if ttbasetype[mproc]<>tproc then
			serror_s("Not function pointer: %s",typename(mproc))
		fi

		pm:=ttparams[mproc]
		retmode:=tttarget[mproc]

	when j_name,j_funcname then
		d:=p.def
		if d.nameid=procid then
			pm:=d.paramlist
			retmode:=d.mode
		else							!assume fnptr, but needs ptr unit
			r:=createunit1(j_ptr,p)

			r.mode:=tttarget[d.mode]
			R.MODE:=P.MODE
			p:=r

			goto doptr
		fi
	when j_dot,j_callfn,j_ifx,j_convert then
		r:=createunit1(j_ptr,p)
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

!checking params is a little tricky because of variadic params
!but there must be at least <nparams> actual params.

	if aparams<nparams then
		terror("Too few args")
	elsif aparams>nparams and pm.flags<>pm_variadic and pm.flags<>pm_notset then
		if pm.flags<>pm_notset then
			cpl aparams,nparams


			terror("Too many args")
		elsif fmodern then
			terror("Can't call () param function")
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

	r:=createunit2(j_callfn,p,q)
	r.mode:=retmode
	r.aparams:=aparams

	if d and eqstring(d.name,"printf") and q and q.tag=j_const and
			q.slength<str.len/2 then
		ss:=q.svalue
		tt:=&.str

		u:=q.nextunit
		while c:=ss++^ do
			if c='%' and ss^ in ['?','='] and u then
				if ss^='=' then
					++ss				!should be '?'
					exprstr:=strexpr(u)
					uu:=exprstr.strptr
					convucstring(uu)
					to exprstr.length do
						tt++^:=uu++^
					od
					tt++^:='='
				fi
				++ss

				tt++^:='%'
				case ttbasetype[u.mode]
				when tsint then
					tt++^:='d'
				when tsllong then
					tt++^:='l'
					tt++^:='l'
					tt++^:='d'
				when tuint then
					tt++^:='u'
				when tullong then
					tt++^:='l'
					tt++^:='l'
					tt++^:='u'
				when tfloat, tdouble,tldouble then
					tt++^:='f'
				when tref then
					if tttarget[u.mode]=tschar then
						tt++^:='s'
					elsif u.tag=j_funcname then
						tt++^:='s'
!					tt++^:='p'
						u.tag:=j_const
						u.svalue:=u.def.name
					    u.slength:=strlen(u.svalue)
						u.mode:=trefchar
						u.isstrconst:=1
						u.simple:=1
					else
						tt++^:='p'
					fi
				else
					tt++^:='?'
				esac
				u:=u.nextunit
			else
				tt++^:=c
			fi
		od
		tt^:=0
		q.svalue:=pcm_copyheapstring(&.str)
		q.slength:=strlen(&.str)

	fi

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
		when j_ptr then
			p:=p.a

		when j_dot then						!about to access array field
			offset:=p.offset
			p.tag:=j_addptr
			p.ptrscale:=0	!ttsize[elemmode]
			q:=createunit1(j_addrof,p.a)
			q.mode:=refmode
			p.a:=q
			p.b:=createconstunit(offset,tsint)

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
					serror_s("Struct tag in use %s",e.name)
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
			serror_s("Struct tag in use %s",e.name)
		fi
		if e.deflist then					!else filling in incomplete enum
			cpl "Prev",e.lineno iand 1677215, sourcefilenames[e.lineno>>24],sourcefilepaths[e.lineno>>24]
			serror_s("Redefining struct %s",e.name)
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
				serror_s("member name in use %s",e.name)
			fi

			if linkage<>none_ss then
				serror("Can't use ss in struct")
			fi

	addanonfield::
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
					serror_s("Struct decl error %s",typename(m))
				fi
			esac
		enddocase
	od

	skipsymbol(rcurlysym)

	currrecord.nextfield:=fieldlist
	ttsize[currrecord.mode]:=roundoffset((funion|maxsize|recsize),maxalignment)
	currrecord.attribs.ax_align:=maxalignment

	return currrecord.mode
end

function checkpointertypes(int s,t,hard)int=
!return 1 if pointer types s and t are compatible
!it is assumed that s is to be converted to t, or passed as a parameter expecting t
	int starget:=tttarget[s], ttarget:=tttarget[t]
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
		cpl Strmode(s)
		cpl Strmode(t)
		terror("const to non-const pointer")
	fi

	if starget=ttarget then return 1 fi
	s:=starget
	t:=ttarget
	if ttbasetype[s]=tvoid or ttbasetype[t]=tvoid then
		return 1
	fi

	if ttisref[s] and ttisref[t] then
		return checkpointertypes(s,t,hard)

	elsif ttbasetype[s]=tarray and ttbasetype[t]=tarray then
		if ttlength[s]<>ttlength[t] then
			if ttlength[s] and ttlength[t] then		!allow one dim to be 0
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
				serror_s("Enum tag in use %s",e.name)
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
			serror_s("Enum tag in use %s",e.name)
		fi
		if e.deflist then					!else filling in incomplete enum
			serror_s("Redefining enum %s",e.name)
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
			serror_s("enum name reused %s",d.name)
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
!opc is j_dot or j_idot
!Deal with field selection for p.d or p->d
	unit q,r,poffset,pb,pc
	ref strec e,f,prec,panon,pfield,gend
	int m,offset,scale
	ref fieldrec fl

!check that m is a proper pointer if needed, and a struct or union
	m:=p.mode
	if opc=j_idot then			!
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
		terror_ss("Not a field of struct %s %s",d.name,Strmode(m))
	fi


	poffset:=createconstunit(offset,tsint)

	if opc=j_idot then				!apply offset to lhs
		p:=createptrop(p)
	fi

	p:=createunit1(j_dot,p)
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
		if p.tag=j_const then
			p.value:=p.value*elemsize
		else
			p:=createunit1(j_scale,p)
			p.scale:=elemsize
			p.mode:=tptroffset
		fi
	fi
	return p
end

function divunit(unit p, int elemtype)unit=
	int elemsize

	if (elemsize:=ttsize[elemtype])<>1 then
		if p.tag=j_const then
			p.value:=p.value/elemsize
		else
			p:=createunit1(j_scale,p)
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
		r:=createunit2(j_assign,p,q)

	when addtosym then
		if ttisref[qmode] then		!ref+=ref
			serror("ptr+=ptr")
		fi

		q:=coercemode(q,tptroffset)					!ref+=int
		r:=createunit2(j_addto,p,mulunit(q,elemmode))

	when subtosym then
		if ttisref[qmode] then		!ref-=ref
			if not comparemode(pmode,qmode) then
				serror("-= refs don't match")
			fi
			r:=divunit(createunit2(j_sub,p,q),elemmode)
			rmode:=tsint
		else								!ref-=int
			r:=createunit2(j_subto,p,mulunit(q,elemmode))
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
	when tschar,tsshort then
		insertunit(p,j_widenmem)
		p.mode:=tsint
	when tuchar,tushort,tbool then
		insertunit(p,j_widenmem)
		p.mode:=tuint
	esac
end

function docast(unit p,int t,hard=1,inplace=0)unit=
!apply cast to unit p
!if no cast needed, then just return p
	unit q
	int s,opc

	s:=p.mode

	retry::

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
	elsif ttisref[t] and isintcc(s) and p.tag=j_const and p.value=0 then
		opc:=soft_c
	fi

	if opc=0 then
		if not hard then
			cpl Strmode(s)
			cpl Strmode(t)

	PRINTUNIT(NIL,P)

			terror_ss("Can't do conversion %s => %s",typename(s),typename(t))
		fi
		opc:=hard_c
	fi

	case p.tag
	when j_const then		!try and convert
		if eval_convert(p,t,opc) then
			return p
		fi
	when j_funcname then
		p.mode:=t
		return p
	when j_add then
		if p.a.tag=j_const and p.b.tag=j_const then
			p.value:=p.a.value+p.b.value
			p.mode:=t
			p.tag:=j_const
			return p
		fi
	esac

	if inplace then
		insertunit(p,j_convert)
		p.mode:=t
		p.opcode:=opc
		return nil
	else
		q:=createunit1(j_convert,p)
		q.opcode:=opc
		q.mode:=t
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

proc dostaticassert=
	int x
	[256]char str
	lex()
	skipsymbol(lbracksym);
	x:=readconstintexpr()
	skipsymbol(commasym)
	checksymbol(stringconstsym)
	if not x then
		memcpy(&.str,lx.svalue,lx.length)
		str[lx.length+1]:=0
		serror(&.str)
	fi
	lex()
	skipsymbol(rbracksym)
end

function createsizeofop(unit p)unit=
	unit q
	int t,size

	t:=p.mode
	switch p.tag
	when j_name then
		if p.alength then
			size:=ttsize[p.def.mode]/p.alength			!take account of array
		else
			size:=ttsize[p.def.mode]			!take account of array
		fi
	when j_const then
		case t
		when trefchar then					!const string
			size:=p.slength+1
		when trefwchar then
			size:=(p.wslength+1)*2
		else
			size:=ttsize[t]
		esac

	when j_ptr then
		if ttisref[t] and p.alength then		!result of array=>ptr conversion
			size:=ttsize[tttarget[t]]*p.alength
		else
			size:=ttsize[t]
		fi

	when j_addptr then
		if p.alength then	!derived from array expr that converted to pointer
			size:=ttsize[tttarget[t]]*p.alength
		else
			goto cad1
		fi

	when j_addrof then
		if p.a.tag=j_name and p.a.alength then
			size:=ttsize[p.a.def.mode]
		fi

	when j_widenmem then
		return createsizeofop(p.a)

	else
	cad1::
		size:=ttsize[t]
	endswitch

	q:=createconstunit(size,tullong)
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

proc readstructinfosym=
	ref strec d,e
	ref paramrec pm
	int m, nfields
	filehandle f
	ichar name
	[256]char str

	lex()
	m:=readcasttype(d,0,pm)

	if ttbasetype[m]<>tstruct then
		serror("Struct type expected")
	fi
	d:=tttypedef[m]

	e:=d.deflist
	nfields:=0
	while e do
		++nfields
		e:=e.nextdef
	od

	name:=d.name

	print @&.str,"$",,name,,"_info.h"

	f:=fopen(&.str,"w");

	println @f,"memberinfo_t $",,name,,"[] = {"

	e:=ttnamedef[m].deflist
	nfields:=0

	while e do
		println @f,"    {""#"", #,#,#,#,#,#}#", e.name,
			e.mode, ttbasetype[e.mode], tttarget[e.mode],
			ttsize[e.mode], e.offset, 0, (e.nextdef|","|"")
		++nfields
		e:=e.nextdef
	od

	println @f,"};"

	println @f,"enum {$#_length = #};",name,nfields

	fclose(f)
end

function getmemmode(unit p)int=
!return mode of p, but if p is a widening unit, see past that to
!the original memory mode
	if p.tag=j_widenmem then
		return p.a.mode
	else
		return p.mode
	fi
end

function readstrinclude:unit=
	unit p
	ichar text

	lex()
	checksymbol(lbracksym)
	lex()
	p:=readexpression()
	checksymbol(rbracksym)
	lex()
	if p.tag<>j_const or p.mode<>trefchar then
		serror("String const expected")
	fi

	text:=cast(readfile(p.svalue))
	if not text then
		serror_s("Can't read strinclude file: %s",p.svalue)
	fi

	return createstringconstunit(text,rfsize)
end

