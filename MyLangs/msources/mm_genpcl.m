import msys
import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_tables
import mm_lib
import mm_diags
import mm_blockpcl

import* mm_pcl
import* pc_win64

const entrypointname = "start"
const entrypointname2 = "main"

global int retindex
global int initstaticsindex

global int framebytes, parambytes
global int retpending					!1 means not set
global pcl pclastframe
global int pcltempindex

global symbol pclcurrproc

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

global function codegen_pcl(int rts):int=
!generate code for module n
	symbol d,e
	ref procrec pp

	pcl_start(nunits)

	SCANSYMBOL(STPROGRAM)

	pp:=staticlist
	while pp do
		d:=pp.def
		dostaticvar(d)
		pp:=pp.nextproc
	od

	pcl_gencomment("")

	pp:=proclist
	while pp do
		d:=pp.def
		genprocdef(currproc:=d)
		pp:=pp.nextproc
	od

	pcl_endprog(0,dorts:rts)

	return 1
end

proc genprocdef (symbol p) =	!GENPROCDEF
	[1256]char str
	[1256]char name
	int paramoffset,nparams,retaddrbytes
	symbol d
	int n,lab,np,offset,reg,i,xreg,isstart,structret,isfloat,hasparams,hasequiv
	unit q
	pcl pcproc
	pcl oldpccode,oldpccurr, pp,qq

	d:=p.deflist

	isstart:=0
	if p.isglobal and (eqstring(p.name,entrypointname) or
						 eqstring(p.name,entrypointname2)) then
		isstart:=1
		p.index:=labelno
	fi

	retpending:=1

	MLINENO:=P.POS
	pcl_gen((p.isthreaded|kthreadedproc|kprocdef),genmem_d(p))
	setmode(p.mode)
	if p.isglobal=export_scope then
		pcl_setexported(1)
	fi

	if p.name^='$' then
		pcl_setrtsproc()
	fi

	pcproc:=pccurr
	pclastframe:=pcproc
	pcltempindex:=0

	d:=p.deflist
	while d do
		MLINENO:=D.POS
		case d.nameid
		when frameid then
			if not d.atvar then
				pcl_gen(klocal,genmem_d(d))
				setmode(d.mode)
				pclastframe:=pccurr
			fi
		when paramid then
			pcl_gen(kparam,genmem_d(d))
			setmode(d.mode)
		esac
		d:=d.nextdef
	od

	genprocentry(isstart)

	if isstart and msyslevel=2 then
		pcl_gen_sysproc(sysfn_init)
	fi

	retindex:=lab:=createfwdlabel()

	if isstart then
		for i:=nmodules downto 1 do
			d:=moduletable[i].stinitproc
			if d then
				pcl_gen(ksetargs)
				pcl_gen(kcallproc, genmemaddr_d(d))
				setmode(ti64)
			fi
		od
	fi

!	pcl_gencomment("-------------------------------------------------")
	evalunit(p.code)
!	pcl_gencomment("-------------------------------------------------")

	definefwdlabel(retindex)

	if isstart then
		pcl_gen(kpush,pcl_genint(0))
		pcl_gen(kstop)
		genreturn()
	else
		genreturn()
	fi

	if p.mode<>tvoid then
		if not checkblockreturn(p.code) then
			gerror_s("Function needs explicit return: ",p.name)
		fi
	fi

	pcl_gen(kendproc)

	pcl_gencomment("")
end

proc genprocentry(int isstart)=
!proc entry code

	pcl_gen(kprocentry)
end

proc dostaticvar(symbol d)=
	unit p

	if d.isimport then return fi

	if d.atvar=1 then
	elsif d.code then
		pcl_gen(kistatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
		genidata(d.code)
	else
dozstatic::
		pcl_gen(kzstatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
	fi
end

proc genidata(unit p,int doterm=1, am='A',offset=0)=
	int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion,tbase
	unit q,a,b
	symbol d
	real32 sx

	t:=p.mode
	mlineno:=p.lineno
	tbase:=ttbasetype[t]

	case p.tag
	when j_const then
		if ttisref[p.mode] then
			if p.mode=trefchar then
				if p.svalue then
					pcl_gen(kdq,pcl_genstring(p.svalue))
				else
					pcl_gen(kdq,pcl_genint(0))
				fi
			else
				pcl_gen(kdq,pcl_genint(p.value))
			fi
		elsif ttisreal[p.mode] then
			case ttsize[p.mode]
			when 4 then
				pcl_gen(kdd,pcl_genreal32(p.xvalue))
			when 8 then
				pcl_gen(kdq,pcl_genreal(p.xvalue))
			else
				gerror_s("IDATA/REAL:",strmode(p.mode),p)
			esac

		else						!assume int/word
			case ttsize[getmemmode_m(p)]
			when 1 then
				pcl_gen(kdb,pcl_genint(p.value))
			when 2 then
				pcl_gen(kdw,pcl_genint(p.value))
			when 4 then
				pcl_gen(kdd,pcl_genint(p.value))
			when 8 then
				pcl_gen(kdq,pcl_genint(p.value))
			when 16 then
				pcl_gen(kdq,pcl_genint(p.range_lower))
				pcl_gen(kdq,pcl_genint(p.range_upper))
			else
				gerror_s("IDATA/INT:",strmode(p.mode),p)
			esac

		fi

	when j_makelist then
		q:=p.a
		while q do
			genidata(q)
			q:=q.nextunit
		od

	when j_name then
		d:=p.def
		case d.nameid
		when staticid,procid,dllprocid then
			pcl_gen((am='P' or ttsize[p.mode]=8|kdq|kdd), genmemaddr_d(d))
			if offset then
				pcl_setscale(1)
				pcl_setoffset(offset)
			fi
		else
			gerror("Idata &frameXXX")
		esac
		return
	when j_convert then
		genidata(p.a)
	when j_shorten then
		a:=p.a
		case ttsize[p.mode]
		when 1 then
			pcl_gen(kdb,pcl_genint(a.value))
		when 2 then
			pcl_gen(kdw,pcl_genint(a.value))
		when 4 then
			pcl_gen(kdd,pcl_genint(a.value))
!	when 8 then
!		pcl_gen(kdq,pcl_genint(a.value))
!	when 16 then
!		pcl_gen(kdq,pcl_genint(a.range_lower))
!		pcl_gen(kdq,pcl_genint(a.range_upper))
		else
			gerror_s("IDATA/SHORTEN:",strmode(p.mode),p)
		esac

	when j_addrof,j_addroffirst then
		genidata(p.a,am:'P',offset:(p.b|p.b.value|0))
	else
		gerror_s("IDATA: ",jtagnames[p.tag],p)

	esac
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

global proc genpushmemaddr_d(symbol d)=
	pcl_gen(kpush,pcl_genmemaddr(getpst(d)))
end

global proc setmode(int m)=
	pcl_settype(getpclmode(m),ttsize[m])
end

global proc setmode_u(unit p)=
	pcl_settype(getpclmode(p.mode),ttsize[p.mode])
end

global function definelabel:int =
	pcl_gen(klabel,pcl_genlabel(++labelno))
	return labelno
end

global function createfwdlabel:int =
	return ++labelno
end

global proc definefwdlabel(int lab) =
	pcl_gen(klabel,pcl_genlabel(lab))
end

global proc genreturn=
!assume returning from currproc
	case currproc.nretvalues
	when 0 then
		pcl_gen(kretproc)
	when 1 then
		pcl_gen(kretfn)
		setmode(currproc.mode)

	else
		pcl_genx(kretfn,currproc.nretvalues)
	esac
end

global function reversecond(int pclop)int=
!reverse conditional operator
	case pclop
	when keq then pclop:=kne
	when kne then pclop:=keq
	when klt then pclop:=kge
	when kle then pclop:=kgt
	when kge then pclop:=klt
	when kgt then pclop:=kle
	esac

	return pclop
end

global proc stacklooplabels(int a,b,c,d)=
!don't check for loop depth as that has been done during parsing
	++loopindex
	if loopindex>maxnestedloops then
		gerror("Too many nested loops")
	fi

	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c
	loopstack[loopindex,4]:=d

end

global function findlooplabel(int k,n)int=
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") fi
	return loopstack[i,k]
end

global proc pcl_gen_sysfn(int fnindex, unit a=nil,b=nil,c=nil)=
	pcl_gen_sysproc(fnindex, a,b,c, 1)
end

function getslots(unit p)int=
	if ttbasetype[p.mode] in [ti128, tu128, tslice] then return 2 fi
	return 1
end

global proc pcl_gen_sysproc(int fnindex, unit a=nil,b=nil,c=nil, int asfunc=0)=
	int nargs:=0, opc
	pcl p
	opc:=0

	pcl_gen(ksetargs)
	p:=pccurr

	if c then evalunit(c); nargs+:=getslots(c) fi
	if b then evalunit(b); nargs+:=getslots(b) fi
	if a then evalunit(a); nargs+:=getslots(a) fi

	p.nargs:=nargs

	pcl_gen((asfunc|kcallfn|kcallproc), pcl_genmemaddr(getsysfnhandler(fnindex)))
	pcl_setnargs(nargs)
end

proc $init=
	zero_unit.tag:=j_const
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

global function newframetemp(symbol owner,int size)symbol d=
!create a new local frame var used for implicit temps
	[32]char str

	fprint @&.str,"$T#",++pcltempindex
	d:=pcm_allocz(strec.bytes)

	d.name:=pcm_copyheapstring(&.str)
	d.owner:=owner

	d.nameid:=frameid

	return d
end

global proc addframetemp(symbol d)=
	pcl oldpccurr,pnew,pnext

	oldpccurr:=pccurr

	pcl_gen(klocal,pcl_genmem(getpst(d)))
	setmode(d.mode)

!	pnew:=pccurr
!	pccurr:=oldpccurr
!	pclcurr.nextpcl:=nil
!
!	pnext:=pclastframe.nextpcl
!	pclastframe.nextpcl:=pnew
!	pnew.nextpcl:=pnext
!	pclastframe:=pnew

end

proc getfullname(symbol d,ichar dest)=
	if d.owner and d.owner.nameid<>programid then
		getfullname(d.owner,dest)
		strcat(dest,".")
	fi
	strcat(dest,d.name)
end

global function getpst(symbol d)psymbol p=
	[300]char str
	symbol e

	return nil when d=nil

	if d.pstdef then return d.pstdef fi
	str[1]:=0

	if d.name^='m' and (d.name+1)^='$' then
		for i to sysfnnames.len do
			if eqstring(d.name+2, sysfnnames[i]+6) then

				if sysfnhandlers[i] then
					return d.pstdef:=sysfnhandlers[i]
				fi

				d.pstdef:=p:=pcm_allocz(p^.bytes)
				getfullname(d, str)
				p.name:=pcm_copyheapstring(str)
					p.symbol:=namesym
				sysfnhandlers[i]:=p
				return p
			fi
		od
	fi

	d.pstdef:=p:=pcm_allocz(p^.bytes)

	if d.atvar and d.equivvar then
		p:=getpst(e:=d.equivvar.def)
		d.pstdef:=e.pstdef
		return p
	fi


	if d.nameid=dllprocid then
		if d.truename then
			strcpy(str,d.truename)
			p.istruename:=1
		else
			strcpy(str,d.name)
		fi
	else
		getfullname(d, str)
	fi
	p.name:=pcm_copyheapstring(str)
	p.symbol:=namesym

	return p
end

function getsysfnhandler(int fn)psymbol p=
	[300]char str

	if sysfnhandlers[fn] then
		return sysfnhandlers[fn]
	fi


	sysfnhandlers[fn]:=p:=pcm_allocz(p^.bytes)

	IF MSYSLEVEL=1 THEN
		strcpy(str,"msystemp.m$")
	else
!		strcpy(str,"msyslib.m")
		strcpy(str,"msysp.m$")
!		strcpy(str,"t.m$")
	fi
	strcat(str,sysfnnames[fn]+6)
	p.name:=pcm_copyheapstring(str)
	p.symbol:=namesym

	return p
end

proc doimportedproc(symbol d)=
	symbol e

	pcl_gen(kextproc,genmem_d(d))
	setmode(d.mode)

	e:=d.deflist
	while e, e:=e.nextdef do
		if e.nameid=paramid then
			pcl_gent(kextparam, getpclmode(e.mode))
		fi
	od
	if d.varparams then
		pcl_genxy(kextvariadics,d.varparams,0)
	fi

	pcl_gen(kendextproc)
end

proc scansymbol(symbol d)=
	symbol e
	if d.nameid=dllprocid and d.used then
		doimportedproc(d)
	fi

	case d.nameid
	when programid,moduleid then
	else
		return
	esac

	e:=d.deflist

	while e, e:=e.nextdef do
		scansymbol(e)
	od
end

global proc genpushint(int a,mode=tpi64)=
	pcl_gen(kpush, pcl_genint(a,mode))
end

global proc genpushreal(real x)=
	pcl_gen(kpush,pcl_genreal(x))
end

global proc genpushreal32(real x)=
	pcl_gen(kpush,pcl_genreal32(x))
end

global proc genpushstring(ichar s)=
	pcl_gen(kpush,pcl_genstring(s))
end

