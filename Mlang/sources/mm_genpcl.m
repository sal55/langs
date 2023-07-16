global int retindex
global int initstaticsindex

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

global const maxblocktemps=50
global [maxblocktemps]symbol blockdefs
global int nblocktemps
global symbol blockretname

int nvarlocals, nvarparams

macro divider = genpc_comment("------------------------")

global function codegen_pcl:int=
!generate code for module n
	symbol d,e
	ref procrec pp

	pcl_start(nunits)

!	for i to nlibfiles when libfiles[i]^<>'$' do
!		genpc((libtypes[i]='D'|kimportdll|kimportlib), genpc_name(libfiles[i]))
!	od
!	genpc_comment("")

	scansymbol(stprogram)

	pp:=staticlist
	while pp do
		d:=pp.def
		dostaticvar(d)
		pp:=pp.nextproc
	od

	genpc_comment("")

	pp:=proclist
	while pp do
		d:=pp.def
		genprocdef(currproc:=d)
		pp:=pp.nextproc
	od

	pcl_end()

	if (debugmode and fshowpcl) then
		pcl_writepclfile(pclfilename)
	fi

	return 1
end

proc genprocdef (symbol p) =	!GENPROCDEF
	ref modulerec ms
	symbol d

	ms:=&moduletable[p.moduleno]
	nblocktemps:=0

	if p=ms.stmain then
		genmaindef(p)
		return
	elsif p=ms.ststart then
		genstartdef(p)
		return
	fi

	mlineno:=p.pos
	doprocdef(p)

	retindex:=createfwdlabel()

	divider()
	evalunit(p.code)
	divider()

	definefwdlabel(retindex)

	genreturn()
	checkreturn(p)

	genpc(kendproc)

	genpc_comment("")
end

proc checkreturn(symbol p)=
	if p.mode<>tvoid then
		if not checkblockreturn(p.code) then
			gerror_s("Function needs explicit return: ",p.name)
		fi
	fi
end

proc dostaticvar(symbol d)=
	unit p
	int expflag:=0

	if d.isimport then return fi

	if d.scope = program_scope and d.name^='$' then
		if eqstring(d.name,"$cmdskip") then
			d.scope:=export_scope				!export from mlib subprog
		fi
	fi

	if d.atvar=1 then
		return
	elsif d.code then
		genpc(kistatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
		genidata(d.code)
	else
dozstatic::
		genpc(kzstatic,genmem_d(d))
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
	mlineno:=p.pos
	tbase:=ttbasetype[t]

	case p.tag
	when jconst then
		if ttisref[p.mode] then
			if p.mode=trefchar then
				if p.svalue then
					genpc(kdq,genpc_string(p.svalue))
				else
					genpc(kdq,genpc_int(0))
				fi
			else
				genpc(kdq,genpc_int(p.value))
			fi
		elsif ttisreal[p.mode] then
			case ttsize[p.mode]
			when 4 then
				genpc(kdd,genpc_real32(p.xvalue))
			when 8 then
				genpc(kdq,genpc_realimm(p.xvalue))
			else
				gerror_s("IDATA/REAL:",strmode(p.mode),p)
			esac

		else						!assume int/word
			case ttsize[getmemmode_m(p)]
			when 1 then
				genpc(kdb,genpc_int(p.value))
			when 2 then
				genpc(kdw,genpc_int(p.value))
			when 4 then
				genpc(kdd,genpc_int(p.value))
			when 8 then
				genpc(kdq,genpc_int(p.value))
			when 16 then
				genpc(kdq,genpc_int(p.range_lower))
				genpc(kdq,genpc_int(p.range_upper))
			else
				gerror_s("IDATA/INT:",strmode(p.mode),p)
			esac

		fi

	when jmakelist then
		q:=p.a
		while q do
			genidata(q)
			q:=q.nextunit
		od

	when jname then
		d:=p.def
		case d.nameid
		when staticid,procid,dllprocid then
			genpc((am='P' or ttsize[p.mode]=8|kdq|kdd), genmemaddr_d(d))
			if offset then
				pcl_setscale(1)
				pcl_setoffset(offset)
			fi
		when labelid then
!CPL "LABELID",D.INDEX
			if d.index=0 then d.index:=++mlabelno fi
			genpc(kdq, genpc_label(d.index))
		else
			gerror("Idata &frameXXX")
		esac
		return
	when jconvert then
		genidata(p.a)
	when jshorten then
		a:=p.a
		case ttsize[p.mode]
		when 1 then
			genpc(kdb,genpc_int(a.value))
		when 2 then
			genpc(kdw,genpc_int(a.value))
		when 4 then
			genpc(kdd,genpc_int(a.value))
		else
			gerror_s("IDATA/SHORTEN:",strmode(p.mode),p)
		esac

	when jaddrof,jaddroffirst then
		genidata(p.a,am:'P',offset:(p.b|p.b.value|0))
	else
		gerror_s("IDATA: ",jtagnames[p.tag],p)

	esac
end

global function genmem_u(unit p)pcl=
	return genpc_mem(p.def)
end

global function genmem_d(symbol d)pcl=
	return genpc_mem(d)
end

global proc genpushmem_d(symbol d)=
	genpc(kload,genpc_mem(d))
end

global function genmemaddr_d(symbol d)pcl=
	return genpc_memaddr(d)
end

global proc genpushmemaddr_d(symbol d)=
	genpc(kload,genpc_memaddr(d))
end

global proc setmode(int m)=
	pcl_settype(getpclmode(m),ttsize[m])
end

global proc setmode_u(unit p)=
	pcl_settype(getpclmode(p.mode),ttsize[p.mode])
end

global function definelabel:int =
	genpc(klabel,genpc_label(++mlabelno))
	return mlabelno
end

global function createfwdlabel:int =
	return ++mlabelno
end

global proc definefwdlabel(int lab) =
	genpc(klabel,genpc_label(lab))
end

global proc genreturn=
!assume returning from currproc
	case currproc.nretvalues
	when 0 then
		genpc(kretproc)
	when 1 then
		genpc(kretfn)
		setmode(currproc.mode)

	else
		genpc_x(kretfn,currproc.nretvalues)
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

global function reversecond_order(int pclop)int=
	case pclop
	when keq then pclop:=keq
	when kne then pclop:=kne
	when klt then pclop:=kgt
	when kle then pclop:=kge
	when kge then pclop:=kle
	when kgt then pclop:=klt
	esac

	return pclop
end

global proc stacklooplabels(int a,b,c)=
!don't check for loop depth as that has been done during parsing
	++loopindex
	if loopindex>maxnestedloops then
		gerror("Too many nested loops")
	fi

	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c

end

global function findlooplabel(int k,n)int=
!k is 1,2,3 for label A,B,C
!n is a 1,2,3, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") fi
	return loopstack[i,k]
end

global proc genpc_sysfn(int fnindex, unit a=nil,b=nil,c=nil)=
!CPL "GENSYSFN"
	genpc_sysproc(fnindex, a,b,c, 1)
end

global proc genpc_sysproc(int fnindex, unit a=nil,b=nil,c=nil, int asfunc=0)=
	[200]char str
	int nargs:=0, opc
	symbol d
	pcl p
	opc:=0

	genpc(ksetcall)
	p:=pccurr

	if c then evalunit(c); genpc(ksetarg); ++nargs fi
	if b then evalunit(b); genpc(ksetarg); ++nargs fi
	if a then evalunit(a); genpc(ksetarg); ++nargs fi

	p.nargs:=nargs

	d:=getsysfnhandler(fnindex)
	if d then
		genpc((asfunc|kcallf|kcallp), genpc_memaddr(d))
		pcl_setnargs(nargs)
	else
		genpc((asfunc|kcallf|kcallp), genpc_nameaddr(sysfnnames[fnindex]+3))
	fi
	pccurr.nargs:=nargs
end

proc start=
	zero_unit.tag:=jconst
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

global function getsysfnhandler(int fn)symbol p=
	[300]char str
	int report

	if sysfnhandlers[fn] then
		return sysfnhandlers[fn]
	fi

	strcpy(str,"m$")
	strcat(str,sysfnnames[fn]+3)	!"sf_stop" => "m$stop"

	ref procrec pp:=proclist
	while pp, pp:=pp.nextproc do
		if eqstring(pp.def.name, str) then
			sysfnhandlers[fn]:=pp.def
			return pp.def
		fi
	od

	report:=passlevel>asm_pass

	if report then
		println "Sysfn not found:",&.str
	fi
	if fn<>sf_unimpl then
		p:=getsysfnhandler(sf_unimpl)
		if p=nil and report then
			gerror("No m$unimpl")
		fi
		return p
	fi

	return nil
end

proc scansymbol(symbol d)=
	symbol e

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

global proc genpushint(int a)=
	genpc(kload, genpc_int(a))
end

global proc genpushreal(real x)=
	genpc(kload,genpc_real(x))
end

global proc genpushreal32(real x)=
	genpc(kload,genpc_real32(x))
end

global proc genpushstring(ichar s)=
	genpc(kload,genpc_string(s))
end

proc genmaindef(symbol p)=
	ref modulerec ms
	symbol d
	int m
!CPL "GENMAIN",P.NAME,P.OWNER.NAME

	mlineno:=p.pos
	doprocdef(p)

!	genlocals(p)

!	genpc(kprocentry)
!	iprocentry:=pccurr

	retindex:=createfwdlabel()

!	genpc_comment("....Call start() in each subprogram other than this")
	for i:=2 to nsubprogs do
		d:=moduletable[subprogtable[i].firstmodule].ststart
		docallproc(d)
	od
	d:=moduletable[subprogtable[1].firstmodule].ststart
	docallproc(d)

!...
!	genpc_comment("....Call start() in this module")

	divider()
	evalunit(p.code)
	divider()

	definefwdlabel(retindex)

	genpc(kload,genpc_int(0))
	genpc(kstop)
	genreturn()

	genpc(kendproc)
	genpc_comment("")
end

proc genstartdef(symbol p)=
	ref modulerec ms
	symbol d, e
	int lead:=0, m,s

	m:=p.moduleno
	s:=p.subprogno
	if subprogtable[s].firstmodule=m then
		lead:=1
	fi

	mlineno:=p.pos
	doprocdef(p)

	retindex:=createfwdlabel()

	if lead then
!		genpc_comment("Lead module....call start() in other modules")

		for i to nmodules when moduletosub[i]=s and i<>m do
			d:=moduletable[i].ststart
!			genpc_comment(str)
			docallproc(d)
		od
	fi

	divider()
	evalunit(p.code)
	divider()

	definefwdlabel(retindex)

	genreturn()

	genpc(kendproc)
	genpc_comment("")
end

proc initstaticvar(symbol d)=
	if d.code then
		evalunit(d.code)
	fi
	if d.equals=3 then
		genpc_comment("<deepcopy needed>")
!*!				genpc(kcopy)
	fi
	genpc(kstore,genmem_d(d))
end

proc docallproc(symbol d)=
!call a simple proc, eg. start(), with no args
	return unless d
	genpc(ksetcall)
	pcl_setnargs(0)

	genpc(kcallp, genmemaddr_d(d))
end

global function newblocktemp(int m)symbol=
	[16]char str
	symbol d

	if nblocktemps>maxblocktemps then
		gerror("Too many block temps")
	fi
	++nblocktemps

	fprint @str,"$T#",nblocktemps
	d:=getduplnameptr(currproc,addnamestr(str),frameid)
	d.used:=1
	ADDDEF(CURRPROC,D)

	d.mode:=m
	blockdefs[nblocktemps]:=d
	d
end

proc doprocdef(symbol p)=
	genpc((p.isthreaded|kthreadedproc|kprocdef),genmem_d(p))
	setmode(p.mode)
	pclprocdef:=pccurr

end
