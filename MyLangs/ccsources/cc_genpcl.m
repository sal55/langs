!M Compiler - x64 Target Code Generator 1
!import main
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
!import cc_libmcl
import cc_blockpcl

import* cc_pcl

global int retindex
global pcl pclastframe

global function codegen_pcl(int n)int=
!generate code for module n
	ref strec d,e

	pcl_start()

	stmodule:=moduletable[n].stmodule

!do two passes: module decls first, then procs

	d:=stmodule.deflist
	while d do
		case d.nameid
		when staticid then
			dostaticvar(d)
		when procid then
			e:=d.deflist
			while e do
				case e.nameid
				when staticid then
!					dostaticvar_fn(e)
					dostaticvar(e)
				when frameid then
					if e.code then
						if e.code.tag=j_makelist or 
						    ttbasetype[e.mode]=tarray and e.code.tag=j_const then
!							dostaticvar_fn(e)
							dostaticvar(e)
						fi
					fi
				esac
				e:=e.nextdef
			od

		esac
		d:=d.nextdef
	od
!	modulecode:=mccode

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

	return 1
end

proc genprocdef (ref strec p) =	!GENPROCDEF
	[256]char str
	int m,nparams
	ref strec d
	int n,lab,np,offset,reg,i,xreg,ismain,structret

!	setsegment('C')

!	initmcdest()
!	setalign(16)
	pcl_gencomment("!------------------------------------")

	currproc:=p
	pcl_gen(kprocdef, genmem_d(p))
	setmode(p.mode)
	nparams:=0

	d:=p.deflist
	while d, d:=d.nextdef do
		switch d.nameid
		when frameid then
			pcl_gen(klocal, genmem_d(d))
			setmode(d.mode)
			pclastframe:=pccurr
		when paramid then
			pcl_gen(kparam, genmem_d(d))
			setmode(d.mode)
			++nparams
		endswitch
	od

	ismain:=0
	if eqstring(p.name,"main") then
		ismain:=1
		if nparams then
			genmainprelude()
		fi
		if p.mode<>tsint then
			gerror("main needs int return type")
		fi
	fi

	genprocentry(ismain)
	retindex:=lab:=createfwdlabel()
	pcl_gencomment("-------------------------------------------------")

!PCL_GENCOMMENT("<FUNC BODY GOES HERE>")
	do_stmt(p.code)

	definefwdlabel(retindex)
	pcl_gencomment("-------------------------------------------------")

	if ismain then
		pcl_gen(kpush, pcl_genint(0))
		pcl_gen(kstop)
	else
		pcl_gen(kretproc)
!		genreturn()
	fi


	if p.mode<>tvoid then
		if not checkblockreturn(p.code) then
			unless ismain then
				gerror_s("Function needs explicit return statement: %s",p.name)
			end
		fi
	fi

	pcl_gen(kendproc)

	pcl_gencomment("")
end

function checkblockreturn(unit p)int=
!p is any statement
!check p, or the last statement of a block, or the last statement of any
!nested block, a return, or is a unit yielding some value (as explicit return
!statement not needed)
! return 1/0 for return/not return
	unit e,wt
	int m,res

	IF not FMODERN THEN RETURN 1 FI
	if p=nil then return 0 fi

	m:=p.mode

	case p.tag
	when j_return then			!that's an easy one...
		return 1
	when j_if then
		return checkblockreturn(p.b) and checkblockreturn(p.c)		!all branches must have a return

	when j_switch then
		RETURN 1;
		return checkblockreturn(p.b)

	when j_block then
		e:=p.a
		if e then
			while e and e.nextunit do
				e:=e.nextunit
			od
			return checkblockreturn(e)
		fi

	when j_labelstmt then
		return checkblockreturn(p.a)

	when j_goto, j_while, j_dowhile then
		return 1;

	esac
	return 0
end

proc dolabel(ref strec d)=
	[256]char str

	pcl_gen(klabelname,genmem_d(d))

!	strcpy(&.str,"`")
!	strcat(&.str,getfullname(d))
!	strcat(&.str,(isexported(d)|"::"|":"))
!
!	genmc(m_labelname,genname(&.str))
end

!proc dolabel_fn(ref strec d,int dollar=0)=
![256]char str
!
!!sprintf(&.str,"`%s%s.%s.%d:",(dollar|"$"|""),d.owner.name,d.name,int32(d.blockno))
!fprint @&.str,"`##.#.#:",(dollar|"$"|""),d.owner.name,d.name,d.blockno
!
!genmc(m_labelname,genname(&.str))
!end

proc dostaticvar(ref strec d)=
	int align

	case d.scope
	when imported_scope then
		return
	esac

	align:=getalignment(d.mode)

	if d.code then
		pcl_gen(kistatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
		genidata(d.code)
!		setsegment('I',align)
	else
		pcl_gen(kzstatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
	fi
end

!proc dostaticvar_fn(ref strec d)=
!!statics inside procs
!int align
!
!case d.scope
!when imported_scope then
!	dostaticvar(d)
!	return
!esac
!
!align:=getalignment(d.mode)
!
!if d.code then
!	setsegment('I',align)
!	dolabel_fn(d,d.nameid=frameid)
!	genidata(d.code)
!else
!	setsegment('Z',align)
!	dolabel_fn(d,0)
!
!	GENMC(M_RESB,GENINT(TTSIZE[D.MODE]))
!fi
!end

proc genprocentry(int ismain)=
	pcl_gen(kprocentry)
	pcl_setexported(ismain)
end

proc genidata(unit p,int doterm=1,am=1,offset=0)=
int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion
unit q,a,b
ref strec d
real32 sx
[256]char str
[16]char str2

t:=p.mode
a:=p.a
b:=p.b

PCL_GENCOMMENT("GENIDATA")

!case p.tag
!when j_makelist then
!	n:=p.count					!number of supplied params
!	if ttbasetype[t]=tarray then
!		length:=ttlength[t]			!actual length of array
!		q:=a
!		for i:=1 to n do
!			genidata(q)
!			q:=q.nextunit
!		od
!		if n<length then			!rest will be zeros
!			n:=(length-n)*ttsize[tttarget[t]]		!bytes left
!			while n>=8 do
!				genmc(m_dq,genint(0,8));
!				n-:=8
!			od
!			to n do
!				genmc(m_db,genint(0));
!			od
!		fi
!	else
!		isunion:=ttbasetype[t]=tunion
!
!		d:=ttnamedef[t].deflist
!		size:=ttsize[t]				!total bytes in struct
!		offset1:=offset2:=0			!offset so far; 1/2 are in idata/in struct
!		q:=a
!		for i:=1 to n do
!			genidata(q,0)
!			if ttbasetype[q.mode]=tref and q.strarray then	!IMMEDIATE STRING
!!CPL "IMMSTR"
!				offset1+:=q.slength
!			else
!				offset1+:=ttsize[q.mode]
!			fi
!			d:=d.nextdef
!			if d and not isunion then
!				offset2:=d.offset
!			else
!				offset2:=size
!			fi
!
!			padding:=offset2-offset1
!			if padding>0 then
!				padding:=offset2-offset1
!				if padding>0 then
!					genmc(m_resb,genint(padding))
!				fi
!				offset1:=offset2
!			fi
!			q:=q.nextunit
!		od
!		if offset2<size then
!			n:=size-offset2
!			while n>=8 do
!				genmc(m_dq,genint(0,8));
!				n-:=8
!			od
!			to n do
!				genmc(m_db,genint(0));
!			od
!		fi
!	fi
!	return
!when j_const then
!!	if t>=tfirstint and t<=tlastreal then
!
!	if isintcc(t) or isrealcc(t) then
!		if t=tfloat then
!			sx:=p.xvalue
!!			genmc(m_dd,genint(int@(sx),4))
!			genmc(m_dd,genint(int32@(sx),4))
!		else
!			genmc((ttsize[t]|m_db, m_dw, 0, m_dd, 0,0,0, m_dq|0),genint(p.value,ttsize[t]))
!		fi
!	elsif ttbasetype[t]=tref then
!!CPL "GENIDATA/REF",STRMODE(T)
!		padding:=0
!!CPL "REF1"
!doref::
!!CPL "REF2"
!		if p.value=0 then
!			genmc(m_dq,genint(0,8))
!		elsif p.strarray then					!immediate string (char[])
!!CPL "GENIDATA/STRARRAY",strmode(tttarget[t]),=PADDING
!			if ttsize[tttarget[t]]=1 then
!				genmc(m_defstr,genstrimm(p.svalue,p.slength))
!			else
!				genmc(m_defwstr,genwstrimm(p.wsvalue,p.wslength))
!			fi
!			if padding>0 then
!				genmc(m_resb,genint(padding))
!			fi
!
!		elsif p.isstrconst then
!!CPL "STRING"
!			genmc(m_dq, genstrimm(p.svalue,p.slength))
!			if padding>0 then
!				genmc(m_resb,genint(padding))
!			fi
!		elsif p.iswstrconst then
!!CPL "WSTRING"
!			genmc(m_dq, genwstrimm(p.wsvalue,p.wslength))
!			if padding>0 then
!				genmc(m_resb,genint(padding))
!			fi
!		else
!			genmc(m_dq, genint(p.value))
!		fi
!	elsif ttbasetype[t]=tarray then
!!CPL "ARRAYBASE",P.SLENGTH
!!		padding:=(ttlength[t]-p.slength)*ttsize[tttarget[t]]
!		padding:=(ttlength[t]-(p.slength+1))*ttsize[tttarget[t]]
!		goto doref
!	else
!		CPL Strmode(t)
!		GERROR("IDATA/SCALAR")
!	fi
!	return
!when j_name, j_funcname then
!	d:=p.def
!	case d.nameid
!	when staticid,procid then
!		ax:=genmemaddr_d(d)
!		if ax then
!			ax:=applyoffset(ax,offset)
!		fi
!		genmc((am=0 or ttsize[p.mode]=8|m_dq|m_dd), ax)
!
!	else
!		gerror("Idata &frame",p)
!	esac	
!	return
!when j_add then
!	if a.tag=j_name and b.tag=j_const then
!		d:=a.def
!		case d.nameid
!		when staticid then
!			strcpy(&.str,"`")
!			if d.scope=function_scope then
!				strcat(&.str,currproc.name)
!				strcat(&.str,",")
!			fi
!			strcat(&.str,d.name)
!			strcat(&.str,"+")
!
!!			sprintf(&.str2,"%lld",b.value)
!			getstrint(b.value, &.str2)
!
!			strcat(&.str,&.str2)
!			genmc(m_dq, genname(&.str))
!		else
!			gerror("Add/Idata &frame")
!		esac	
!	elsif a.tag=j_const and b.tag=j_const and ttbasetype[a.mode]=tref then		!ASSUME REF+REF
!!		sprintf(&.str,"%lld+%lld",a.value,b.value)
!		print @&.str,a.value,,"+",,b.value
!
!		genmc(m_dq,genname(&.str))
!
!	else
!		gerror("1:Runtime or unsupported expr in static data")
!	fi
!	return
!when j_addrof then
!	if a.tag=j_ptr then
!		genidata(a.a,offset:offset)
!	else
!		genidata(a, am:0,offset:offset)
!	fi
!
!when j_addptr,j_subptr then
!	if b.tag<>j_const then gerror("Complex ptr expr in static data") fi
!	genidata(a,offset:b.value*p.ptrscale+offset)
!
!when j_convert then
!	genidata(a,offset:offset)
!
!else
!PRINTUNIT(NIL,P)
!	gerror("2:Runtime expr in static data",p)
!esac
end

proc genmainprelude=

PCL_GENCOMMENT("MAIN PRELUDE")
!genassem("	sub	Dstack,152")
!genassem("	sub	Dstack,8")
!genassem("	lea	D0,[Dstack+8]")
!genassem("	push	D0")
!genassem("	sub	Dstack,32")
!genassem("	lea	D0,[Dstack+196]")
!genassem("	mov	[Dstack],D0")
!genassem("	lea	D0,[Dstack+184]")
!genassem("	mov	[Dstack+8],D0")
!genassem("	lea	D0,[Dstack+176]")
!genassem("	mov	[Dstack+16],D0")
!genassem("	mov	A0,0")
!genassem("	mov	[Dstack+24],A0")
!genassem("	mov	D10,[Dstack]")
!genassem("	mov	D11,[Dstack+8]")
!genassem("	mov	D12,[Dstack+16]")
!genassem("	mov	D13,[Dstack+24]")
!genassem("	call	__getmainargs*")
!genassem("	add	Dstack,48")
!genassem("	sub	Dstack,32")
!genassem("	mov	A0,[Dstack+180]")
!genassem("	mov	[Dstack],A0")
!genassem("	mov	D0,[Dstack+168]")
!genassem("	mov	[Dstack+8],D0")
!genassem("	mov	D10,[Dstack]")
!genassem("	mov	D11,[Dstack+8]")
!genassem("	call	.main")
!genassem("	mov A10,A0")
!genassem("	call exit*")
!pcl_gencomment("")
!genassem(".main::")
!
end

global function genmem_u(unit p)pcl=
	return pcl_genmem(getpst(p.def))
end

global function genmem_d(symbol d)pcl=
	return pcl_genmem(getpst(d))
end

global proc genpushmem_d(symbol d)=
	pcl_gen(kpush,pcl_genmem(getpst(d)))
end

global function genmemaddr_d(symbol d)pcl=
	return pcl_genmemaddr(getpst(d))
end

global function genmemaddr_u(unit p)pcl=
	return pcl_genmemaddr(getpst(p.def))
end

global proc genpushmemaddr_d(symbol d)=
	pcl_gen(kpush,pcl_genmemaddr(getpst(d)))
end

global proc setmode(int m)=
	pcl_settype(getpclmode(m),ttsize[m])
end

global proc setmode_u(unit p)=
	pcl_settype(getpclmode(p.mode),ttsize[p.mode])
end

proc getfullname(symbol d,ichar dest)=
!	if d.isglobal<>export_scope and d.owner and d.owner.nameid<>programid then
	if d.owner and d.owner.nameid<>programid then
		getfullname(d.owner,dest)
		strcat(dest,".")
	fi
	strcat(dest,d.name)
end

global function getpst(symbol d)psymbol p=
	[300]char str

	if d.pstdef then return d.pstdef fi

	d.pstdef:=p:=pcm_allocz(p^.bytes)

!IF D.ATVAR and D.EQUIVVAR THEN
!GERROR("GETPST/EQUIV")
!fi

	str[1]:=0
!	if d.nameid=dllprocid then
!		strcpy(str,d.name)
!	else
		getfullname(d, str)
!	fi
	p.name:=pcm_copyheapstring(str)
	p.symbol:=namesym

	return p
end

global function createfwdlabel:int =
	return ++labelno
end

global proc definefwdlabel(int lab) =
	pcl_gen(klabel,pcl_genlabel(lab))
end

global function definelabel:int =
	pcl_gen(klabel, pcl_genlabel(++labelno))
	return labelno
end

global function getpclmode(int t)int u=
	u:=stdtopcl[ttbasetype[t]]
end
