=== MA 41 ===
=== mm.m 0 0 1/41 ===
!project =
	module mm_cli

	module mm_blockpcl

	module mm_assem
	module mm_decls

	module mm_diags
!	module mm_diags_dummy

!	module mm_export_dummy
	module mm_exportq
!	module mm_exportm

	module mm_genpcl

	module mm_lex
	module mm_lib

	module mm_libpcl

!	module mm_libsources
	module mm_libsources_dummy

	module mm_modules
	module mm_name
	module mm_parse

	module mm_support
	module mm_tables
	module mm_type

	import pcl
!	import pclrunx

!end

proc main=
	main2()
end

=== pcl.m 0 0 2/41 ===
!project =
!	module pc_api
!	module pc_decls
!
!!	module pc_diags
!	module pc_diags_dummy
!
!	module pc_run
!	module pc_runaux
!
!	module pc_tables
!
!	module mc_GenMCL_dummy
!	module mc_GenSS_dummy
!	module mc_Decls
!	module mc_OBJdecls
!	module mc_WriteASM_dummy
!	module mc_WriteEXE_dummy
!	module mc_WriteOBJ_dummy
!	module mx_run_dummy
!
!end
!
!export byte pc_userunpcl=1			!ask host to default to -runpcl option
!
project =
	module pc_api
	module pc_decls

	module pc_diags

	module pc_run
	module pc_runaux

	module pc_tables

	module mc_GenMCL
	module mc_AuxMCL
	module mc_LibMCL
	module mc_StackMCL

	module mc_GenSS

	module mc_Decls as md
	module mc_OBJdecls
	module mc_WriteASM

	module mc_WriteEXE
	module mc_WriteOBJ

	module mx_decls
	module mx_run
	module mx_lib
	module mx_write
end

export byte pc_userunpcl=0

!proc main=
!end


=== pc_api.m 0 0 3/41 ===
CONST REDUCE=0
!CONST REDUCE=1

INT PCLSEQNO

export pcl pcstart			!start of pcl block
export pcl pccurr			!point to current pcl op
export pcl pcend			!point to last allocated pclrec
global int pcalloc			!number of pclrecs allocated
byte pcfixed				!whether code is fixed up
int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

int initpcalloc=65536

const pcelemsize = pclrec.bytes

global ichar longstring					!used in stropnd
global int longstringlen
global ichar errormess

export func pcl_start(ichar name=nil, int nunits=0)psymbol=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

INT NN:=0X12345601

	if pcldone then pclerror("PCL start?") fi

	if name then
		currprog:=pc_makesymbol(name)
	fi

	pcalloc:=initpcalloc

	if nunits then				!use approx alloc of 10% more
		nunits:=nunits*9/8		!approx expected number of pcl ops
		while pcalloc<nunits do
			pcalloc*:=2
		od
	fi

	pcstart:=pcm_allocz(pcalloc*pcelemsize)
	pcend:=pcstart+pcalloc-8

	pccurr:=pcstart-1
	pcfixed:=0
	pcseqno:=0
	pcneedfntable:=0

	mmlabelno:=0

	currprog

end

export proc pcl_end=
	if pccurr>=pccurr and pccurr.opcode<>kendprog then
		pc_gen(kendprog)
	fi	
	pcldone:=1
end

!export proc pcl_reducetest=
!	int nold, nnew, seqno
!	pcl pc, newpc, newpcstart
!
!!RETURN
!CPL "TEST REDUCING PCL CODE"
!
!	nold:=pccurr-pcstart+1
!!	nnew:=0
!!
!!	pc:=pcstart
!!	to nold do
!!		case pc.opcode
!!		when kcomment then
!!!		when kdefproc then
!!!CPL "DEFPROC:", =PC.DEF.PCADDR.SEQNO, =PC.SEQNO
!!!			recase else
!!		else
!!			++nnew
!!		esac
!!		++pc
!!	od
!!
!	CPL =NOLD
!!	CPL =NNEW
!!
!	NNEW:=NOLD
!
!	pc:=pcstart
!	newpc:=newpcstart:=pcm_alloc(pcelemsize*nold)
!	seqno:=0
!
!	to nold do
!		newpc^:=pc^
!
!		IF PC.OPCODE=KDEFPROC THEN
!			NEWPC.DEF.PCADDR:=NEWPC
!IF EQSTRING(NEWPC.DEF.NAME, "main") then
!CPL "SET NEW MAINADDR",NEWPC, =NEWPCSTART,"//", PC-PCSTART, NEWPC-NEWPCSTART
!fi
!		FI
!
!!		newpc.seqno:=++seqno
!!		++newpc
!!		esac
!		++pc
!		++newpc
!	od
!
!CPL =NEWPCSTART
!CPL =NEWPC, NEWPC-NEWPCSTART
!
!
!	pcstart:=newpcstart
!	pccurr:=pcend:=newpcstart+nnew-1
!CPL "DONE REDUCE",=PCSTART, =PCCURR
!end
!
export proc pcl_reducetest=
	int nold, nnew, seqno
	pcl pc, newpc, newpcstart
	ref[]byte labelmap

	nold:=pccurr-pcstart+1
	nnew:=0
!RETURN
CPL "TEST REDUCING PCL CODE"
INT TT:=CLOCK()
!remove unused labels: first create a map of referenced labels

	labelmap:=pcm_allocz(mlabelno)

	pc:=pcstart
	while pc<=pccurr, ++pc do
		if pc.opcode<>klabel and pc.opndtype=label_opnd then
			labelmap[pc.labelno]:=1
		fi
	od


	pc:=pcstart
	to nold do
		case pc.opcode
		when kcomment then
!		when kdefproc then
!CPL "DEFPROC:", =PC.DEF.PCADDR.SEQNO, =PC.SEQNO
!			recase else
		when klabel then
			if labelmap[pc.labelno] then ++nnew fi
		else
			++nnew
		esac
		++pc
	od

	CPL =NOLD
	CPL =NNEW

!NNEW:=NOLD

	pc:=pcstart
	newpc:=newpcstart:=pcm_alloc(pcelemsize*(nnew+10))
	seqno:=0

	to nold do
		case pc.opcode
		when kcomment then
!PC.OPCODE:=KNOP
!RECASE ELSE
		when klabel then
			if labelmap[pc.labelno] then recase else fi			!else skipped
		when kdefproc,ktcproc then
			pc.def.pcaddr:=newpc
			recase else
		else
			newpc^:=pc^
			newpc.seqno:=++seqno
			++newpc
		esac
		++pc
	od

	pcstart:=newpcstart
	pccurr:=pcend:=newpcstart+nnew-1

CPL "REDUCE TIME",CLOCK()-TT

	pcm_free(labelmap, mlabelno)

end

export func pcl_writepcl(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writeallpcl()

	if filename then
		if pverbose then println "Writing PCL",filename fi
		writefile(filename, d.strptr, d.length)
		""
!		nil
	else
		d.strptr
	fi
end

export proc pcl_genmcl=
	genmcl()
end

export proc pcl_genss(int obj=0)=
	genmcl()
	genss(obj)
end

export func pcl_writeasm(ichar filename=nil, int atype='AA')ichar=
	ref strbuffer asmstr
	filehandle f

!	if assemtype<>atype then
!		pclerror("Wrong ASM Module")
!	fi

	genmcl()

	asmstr:=getassemstr()

	if filename then
		if pverbose then println "Writing", filename fi

		f:=fopen(filename,"w")
		gs_println(asmstr, f)
		fclose(f)

		gs_free(asmstr)
		nil
	else
		asmstr.strptr
	fi
end

export proc pcl_writeobj(ichar filename)=
	phighmem:=2
	genmcl()
	genss(1)
	writecoff(filename)
end

export proc pcl_writedll(ichar filename)=
	phighmem:=2
	genmcl()
	genss()
	genexe(nil, filename, 1)
	writeexe(filename, 1)
end

export proc pcl_writeexe(ichar filename)=

!INT TT:=CLOCK()
	genmcl()
!TT:=CLOCK()-TT
!CPL "MCL TIME IS", TT

	genss()
	genexe(nil, filename, 0)
	writeexe(filename, 0)
end

export proc pcl_writemx(ichar filename)=
!PCLERROR("NO WRITEMCX")
	genmcl()
	genss()
	writemcx(filename)
end

export proc pcl_exec(int cmdskip)=
!PCLERROR("NO RUN")
	genmcl()
	genss()
	runlibfile("dummy", cmdskip)
end

export proc pcl_setflags(int highmem=-1, verbose=-1, shortnames=-1,
	hostcmdskip=-1)=

!CPL "SETFLAGS", =HOSTCMDSKIP

	if highmem>=0 then phighmem:=highmem fi
	if verbose>=0 then pverbose:=verbose fi
	if shortnames>=0 then fpshortnames:=shortnames fi
	if hostcmdskip>=0 then pcmdskip:=hostcmdskip fi
end

proc extendpclblock=
	int newpcalloc, lengthused
	pcl newpcstart

	newpcalloc:=pcalloc*2
	lengthused:=pccurr-pcstart+1

	newpcstart:=pcm_alloc(pcelemsize*newpcalloc)

	memcpy(newpcstart,pcstart, lengthused*pcelemsize)
	pcm_clearmem(newpcstart+lengthused,(newpcalloc-lengthused)*pcelemsize)

	pccurr:=newpcstart+(pccurr-pcstart)
	pcend:=newpcstart+newpcalloc-8

	pcm_free(pcstart,pcalloc*pcelemsize)

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

	case opcode
	when kiload then
		if pccurr.opcode=kaddpx then
			pccurr.opcode:=kiloadx
			return
		fi
	when kistore then
		if pccurr.opcode=kaddpx then
			pccurr.opcode:=kistorex
			return
		fi

!	when kiloadx, kistorex, kaddpx then
!		if pccurr.opcode=kload and (pccurr-1).opcode=kaddpx then
!			s2:=pccurr.scale
!			d2:=pccurr.extra
!			n:=pccurr.value*s2	
!
	esac

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
!	P.SEQ:=++SEQ

end

export proc pc_genix(int opcode, scale=1, offset=0) =
!opcode is one of iloadx, istorex, addpx which be combined into a prior
!addpx/loadimm sequence.
	pcl p
	int n

!CPL "GENPCIX NEW", PCLNAMES[OPCODE], "CURR:", PCLNAMES[(PCCURR).OPCODE],"PREV:", PCLNAMES[(PCCURR-1).OPCODE]
!
	if REDUCE AND pccurr.opcode=kload and pccurr.opndtype=int_opnd then
		if opcode=ksubpx then
			-:=pccurr.value
			opcode:=kaddpx
!CPL "CHANGE SUBPX TO ADDPX"
		fi

		if (pccurr-1).opcode=kaddpx then
			p:=pccurr-1
			p.extra+:=pccurr.value*scale + offset
!CPL "ADDPX INCORPORATES LOAD IMM"

			p.opcode:=opcode			!addpx/loadimm/xxx => xxx
			clear pccurr^
			pccurr:=p					!lose loadimm
			return
		fi
	fi

	p:=newpcl()

	p.opcode:=opcode
	p.scale:=scale
	p.extra:=offset
end

export proc pc_genx(int opcode, int x, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
end

export proc pc_genxy(int opcode, int x,y, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
	p.y:=y
end

export proc pc_gencond(int opcode, cond, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.condcode:=cond
end

export func genint(int a)pcl p=
	p:=newpcl()
	p.value:=a
	p.opndtype:=int_opnd
	return p
end

export func genreal(real x, int mode=tpr64)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=(mode=tpr64|real_opnd|real32_opnd)
	return p
end

export func genrealimm(real x, int mode=tpr64)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=(mode=tpr64|realimm_opnd|realimm32_opnd)
	return p
end

export func genstring(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=string_opnd
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

!MLABELNO MAX:=A

	p.opndtype:=label_opnd
	return p
end

export func genmem(psymbol d)pcl p=
!	unit q

	p:=newpcl()
!	if d.atvar and d.equivvar then
!		q:=d.equivvar
!		case q.tag
!		when jname then
!			d:=q.def
!		when jconvert then
!			d:=q.a.def			!assume points to name
!		else
!			pclerror("genmen@")
!		esac
!	fi

	p.def:=d

	p.opndtype:=mem_opnd
	return p
end

export func genmemaddr(psymbol d)pcl p=
	p:=newpcl()
!	if d.atvar and d.equivvar then
!		d:=d.equivvar.def
!	fi
	p.def:=d

	p.opndtype:=memaddr_opnd
	return p
end

export func gendata(ref byte s, int length)pcl p=
	p:=newpcl()
	p.svalue:=s			! assume already saved on heap
	p.opndtype:=data_opnd
	p.mode:=tpblock
	p.size:=length

	return p
end

export proc gencomment(ichar s)=
	pc_gen(kcomment,genpcstrimm(s))
end

export func genname(ichar s)pcl=
	return genmem(pc_makesymbol(s))
end

export func gennameaddr(ichar s)pcl=
	return genmemaddr(pc_makesymbol(s))
end

export func genassem(ref void code)pcl p=
	p:=newpcl()
	p.asmcode:=code
	p.opndtype:=assem_opnd
	return p
end

EXPORT func strpmode(int mode, size=0)ichar=
	static [32]char str
!	int dprec@signed

	strcpy(str, "")

	case mode
	when tpblock then
		strcpy(str, "blk:")
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

export proc pc_setxy(int x,y)=
	pccurr.x:=x
	pccurr.y:=y
end

export proc pc_setscaleoff(int scale, offset:=0)=
	pccurr.scale:=scale
	pccurr.extra:=offset
end

export proc pc_setoffset(int offset)=
	pccurr.extra:=offset
end

export proc pc_addoffset(int offset)=
	pccurr.extra+:=offset
end

export proc pc_setincr(int n)=
	pccurr.stepx:=n
end

export proc pc_setnargs(int n)=
	pccurr.nargs:=n
end

export proc pc_setnvariadics(int n)=
	pccurr.nvariadics:=n
end

export proc pc_setalign(int n)=
	pccurr.align:=n
end

global proc perror(ichar mess)=
	perror_s(mess, nil)
end

global proc perror_s(ichar mess, param=nil)=
	print "PCL error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

global func getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

global proc pclerror(ichar mess)=
	println "PCL Error:", mess
	println
	stop 1
end

global proc pc_addsymbol(psymbol d)=
	if psymboltable=nil then
		psymboltable:=psymboltablex:=d
	else
		psymboltablex.next:=d
		psymboltablex:=d
	fi
end

export func pc_makesymbol(ichar s, int id=0)psymbol d=
	d:=pcm_allocnfz(pstrec.bytes)
	d.name:=pcm_copyheapstring(s)
	case id
	when import_id then
		d.imported:=1
	when export_id then
		d.exported:=1
		id:=proc_id
	esac

	d.id:=id

	case id
	when local_id, param_id then
		d.owner:=currfunc
	else
		d.owner:=currprog
	esac

	pc_addsymbol(d)
	d
end

global func getfullname(psymbol d, int backtick=0)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	[16]psymbol chain
	int n:=0
	psymbol e:=d

	if fpshortnames then return d.name fi

	if d.imported then
		if backtick then
			strcpy(str, "`")
			strcat(str, d.name)
			strcat(str, "*")
		else
			strcpy(str, d.name)
		fi
		return str
	fi

	repeat
		chain[++n]:=e
		e:=e.owner
	until e=nil !or e.id=program_id

	if backtick then
		strcpy(str, "`")
	else
		str[1]:=0
	fi
	strcat(str,chain[n].name)
	for i:=n-1 downto 1 do
		strcat(str,".")
		strcat(str,chain[i].name)
	od

	return str
end

global proc pcerrorstop(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	println
	fclose(f)
	stop 1
end

export proc pc_addplib(ichar name)=
	if nplibfiles>=maxplibfile then perror("Too many libs") fi
	plibfiles[++nplibfiles]:=name
end

export proc pc_defproc(psymbol d, int mode=tpvoid, threaded=0)=
	pclerror("Nested proc") when currfunc
	pc_gen((threaded|ktcproc|kdefproc), genmem(d))
	if mode=tpvoid then mode:=d.mode fi
	pc_setmode(mode)

	if d.pcaddr then pclerror(addstr("Dupl proc:",d.name)) fi
	d.pcaddr:=pccurr
	if entryproc=nil and eqstring(d.name,"main") then
		entryproc:=d
	fi

	currfunc:=d
end

export proc pc_addparam(psymbol d)=
	psymbol p:=currfunc, q

	pclerror("No proc") unless p

	q:=p.nextparam

	if q=nil then
		p.nextparam:=d
	else
		while q.nextparam do q:=q.nextparam od		!look for last
		q.nextparam:=d
	fi
	++currfunc.nparams

end

export proc pc_addlocal(psymbol d)=
	psymbol p:=currfunc, q

	pclerror("No proc") unless p

	q:=p.nextlocal

	if q=nil then
		p.nextlocal:=d
	else
		while q.nextlocal do q:=q.nextlocal od		!look for last
		q.nextlocal:=d
	fi
	++currfunc.nlocals
end

export proc pc_endproc=
	pclerror("No proc") unless currfunc
	pc_gen(kendproc)
	currfunc:=nil
end

export func addstr(ichar s, t)ichar=
static [256]char str
	strcpy(str, s)
	strcat(str, t)
	str
end


EXPORT proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

	if igetmsourceinfo then
		lineno:=igetmsourceinfo(mmpos, &filename, &sourceline)
		CPL =LINENO
		CPL =FILENAME
	else
		lineno:=0
		filename:="?"
	fi

!CPL "MERROR????"

	fprintln "MCL Error: # (#) on Line: # in #, PCL:#",mess,param, lineno, filename,ppseqno

	pcerrorstop(filename, lineno)
end

=== pc_decls.m 0 0 4/41 ===
!decls

export type psymbol = ref pstrec

global record pstrec = $caligned
	ichar name
	ref pstrec next
	ref pstrec nextparam
	ref pstrec nextlocal
	ref pstrec owner

	union
		pcl pcaddr					!for procs: entry point to function
		ref proc dlladdr			!for imported functions
		ivoid staddr				!var statics: address
	end

	byte symbol
	byte id
	byte subcode

	int32 offset

	byte imported				!only for import_id
	byte exported				!only for proc_id/static_id

	byte mode
	u32 size

	byte addrof
	byte nrefs
	byte reg
	byte atvar
	byte used
	byte reftype
	byte segment
	int16 stindex
	int16 importindex
	int32 labelno

	byte asmused			!1 when proc contains asmcode
	byte dllindex			!for dllproc: which dll in dlltable

	byte nretvalues			!function: number of return values (0 for proc)
	byte varparams			!0 or N; variadic params
	byte isthreaded			!
	byte ishandler			!1 if a proc to be part of func tables

!----------------------------------

	ref fwdrec fwdrefs	!fwd ref chain
!
	byte scope

	byte nparams
	byte variadic		!local function that uses ... variadic params (for C)
	i16 nlocals
	i16 impindex
	i16 expindex

end

export type pcl = ref pclrec

EXPORT record pclrec =
!global record pclrec =
	byte opcode
	byte opndtype
	byte condcode						!for jumpcc/setcc
	byte mode

	u32 size

	union
		struct
			union
				int64	value
				real64	xvalue
				ichar	svalue			!also used for data
				int		labelno
				psymbol	def
				ivoid	asmcode
			end

			union						!two 32-bit params used according to opcode
				struct
					int32 x				!common access to these 1/2 extra attribs
					int32 y
				end

				struct					! (x,y) pointer ops
					int32 scale			! scale factor for offset
					int32 extra			! extra constant byte offset, already scaled
				end
				struct					! (x,y) call/etc
					int32 nargs			! number of args
					int32 nvariadics	! 0, or arg # that is first variadic
				end
				struct					! (x,y) switch
					int32 minlab
					int32 maxlab
				end
				struct					! defproc/retproc/retfn
					int32 paramslots	! stack usage as 8-byte slots
					int32 localslots
				end

				int32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
				int32 align
				int32 popone			! (x) jumpcc: leave X on stack
				int32 slicelwb			! (x) for .upb

			end
		end
	end

	u32 pos:(sourceoffset:24, fileno:8)
	i32 dummy:(mode2:8, seqno:24)
end

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end


!* LOCALS: all that have been USED
!* PARAMS: all including not used ones (but need the USED flag)
!* HIGHPARAM max param (of 0..4) that has been used, to determine
!  if any spare PREG exists
!* NCALLS all calls including those hidden inside POWER etc
!* HIGHARG max argument (of 0..4) of any calls.
!* ASSEMUSED
!* LEAFPROC
!* Need ADDROF flags for each LOCAL and PARAM
!* MAXREGVARS how many locals would qualify as regvars
!* MAXXREGVARS how many locals would qualify as xregvars
!* R3USED (see below)

global int frameoffset
global int paramoffset
global int framebytes

global const maxparams=32
global const maxlocals=256


global int usedparams			!how many of pregs needed for used incoming params
global int usedxparams			!how many of pxregs needed for used incoming params

global int highreg				!highest D-reg used
global int highxreg				!highest X-reg used
global int bspill, bxspill		!no. to spill
global int bxspilloffset		!base frame offset of bxreg spill area

global byte r10used				!these may be set in pass2 when occupied by params
global byte r11used
global byte r13used

global int maxregvars			!how many locals would qualify for regvars
global int maxxregvars			!how many locals would qualify for xregvars

global int nproccalls			!number of calls including implicit ones (eg. ** and sin)
global int highargs				!max number of args (0-4) of any call
global macro leafproc =	nproccalls=0			!1 if leaf function
global byte localshadow			!1 if local, proc-wide shadow space used for a call

export byte assemused			!1 if assem used

global int passno

global int mmlabelno
export int mmpos

global psymbol psymboltable, psymboltablex

global psymbol currprog
global psymbol currfunc
global psymbol blockretname
global psymbol entryproc		!entry point function

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer

global const maxplibfile=50
global [maxplibfile]ichar plibfiles
global [maxplibfile]u64 plibinst
global int nplibfiles

export ref func (int pos, ichar &filename, &sourceline)int igetmsourceinfo

global byte pcldone, mcldone, ssdone

global byte pverbose
global int pcmdskip

export int assemtype='AA'

GLOBAL INT PPSEQNO

=== pc_diags.m 0 0 5/41 ===
const fshowppseqno=1
!const fshowppseqno=0

int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

const tab1="    "
const tab2="        "
!const tab1="\t"
!const tab2="\t\t"

!const fshowsymbols=1
const fshowsymbols=0

global proc strpcl(pcl p)=
	[256]char pmodestr
	[256]char str
	int opcode, defused
	ichar s
	psymbol d, e

	const showformatted=1

	opcode:=p.opcode

	if fshowppseqno then
		psstr(strint(p.seqno,"z5"))
		psstr("  ")
	fi

!RETURN

!PSSTR(STRINT(P.pos,"8ZH"))
!PSSTR(" ")

!CPL PCLNAMES[OPCODE], P.SEQNO
!STOP WHEN OS_GETCH()=27
!psstr(strint(getlineno(p.pos),"4"))
!psstr(" ")

	case opcode
	when klabel then
!CPL "PCL/LABEL", P.DEF.NAME,P.ISEXPORTED
		strlabel(p.labelno,1)
		return
	when klabeldef then
!CPL "PCL/LABEL", P.DEF.NAME,P.ISEXPORTED
		psstr("! ")
		psstr(p.def.name)
		psstr(":")
!		strlabel(p.labelno,1)
		return
	when kcomment then
		if p.svalue^ then
			psstr("!")
			psstr(p.svalue)
		fi
		return
	when kdefproc, ktcproc then
		d:=p.def

!		if p.def.isrts then
!			psstr("Procrts")
		if opcode=ktcproc then
			psstr("tcproc")
		else
			psstr("proc")
		fi

		psstr(" ")
		psname(d)

		e:=d.nextparam

		psstr("(")
		while e, e:=e.nextparam do
			psstr(strpmode(e.mode, e.size))
			psstr(" ")
			psstr(e.name)
			if e.nextparam then psstr(", ") fi
		od

		psstr(")")
!
!		PSSTR(STRPMODE(D.MODE))
		if p.mode then
			psstr(strpmode(P.mode))
		fi

		psstr((p.def.exported|"::"|":"))

		psline()

		e:=d.nextlocal
!CPL =E
		while e, e:=e.nextlocal do
			if fshowppseqno then psstr("       ") fi
			psstr(tab1)
			psstr(strpmode(e.mode, e.size))
			psstr(" ")
			psstr(e.name)
			psline()
		od

!CPL "DONE DEFPROC"

		return

	when kendproc then
		psstr("endproc")
!		psline()
		return

!	when klabelname then
!		psname(p.def)
!		psstr((p.def.scope=export_scope|"::"|":"))
!		return

	when kendprog then
		psstr("endprog")
		return

!	when kload then
!
!		case p.opndtype
!		when memaddr_opnd then opcode:=kload
!		when int_opnd, real_opnd, real32_opnd, string_opnd then opcode:=kload
!		esac

!	when kdb, kdw, kdd, kdq then
!		opcode:=kdata

	when kdata then

		if p.mode=tpblock then
			psdata(p)
			return
		fi

	when kistatic, kzstatic then
		skiptab

!	when kgetnfuns then
!		psstr(tab1+"load i64 $nprocs")
!		return
!
!	when kgetfname then
!		psstrline(tab1+"loadref  u64 $procname")
!doprocname:
!		psstrline(tab1+"exchpcl")
!		psstr(tab1+"iloadx   i64 8 -8")
!		return
!
!	when kgetfaddr then
!		psstrline(tab1+"loadref  u64 $procaddr")
!		doprocname

!	when kgetprocaddr then
!		psstrline(tab1+"load i64 $nprocs")
!		return
!
	esac

	psstr(tab1)
skiptab:


	case opcode
	when kjumpcc then
		strcpy(str, "jump")
		strcat(str, ccnames[p.condcode])
		if p.popone then
			strcat(str, "/1")
		fi
	when ksetcc then
		strcpy(str, "set")
		strcat(str, ccnames[p.condcode])
	else
		strcpy(str, pclnames[opcode])
	esac
	gs_leftstr(dest,str,9)


!CPL =OPNDNAMES[P.OPNDTYPE]
	if p.opndtype<>no_opnd then
		psstr(" ")
		psstr(stropnd(p))
!	else
!		pstabto(
	fi
	pstabto(40)

	if p.mode then
		psstr(strpmode(p.mode, p.size))
!		if pclhastype[opcode]=2 and p.mode<>p.mode2 then
		if pclhastype[opcode]=2 then
			psstr("/")
			psstr(strpmode(p.mode2))
		fi
	else
		psstr("---")
	fi


!	psstr("[")
!	psstr(strint(p.diff,"+"))
!	if p.wide then psstr(" W") fi
!	psstr("] ")


	if pclextra[opcode] and opcode<>kjumpcc then
		psstr(" (")
		psint(p.x)
		if pclextra[opcode]=2 then
			psstr(" ")
			psint(p.y)
		fi
		psstr(")")
	fi

!PSSTR(" SIZE:")
!PSINT(P.SIZE)
!

	if opcode=keval then psstr("\n") fi

!	if p.isglobal then psstr(" Isglobal") fi
!	if p.isvariadic then psstr(" Isvariadic") fi
end

global func stropnd(pcl p)ichar=
	static[512]char str
	int length
	psymbol d

!RETURN "<OPND>"

	if p=nil then
		return ""
	fi

!STRCPY(STR,"?")

!CPL OPNDNAMES[P.OPNDTYPE]

	str[1]:=0

	case p.opndtype
	when int_opnd then
		return strint(p.value)
	when real_opnd then
		if p.xvalue=infinity then
			fprint @str,"0x#",word@(p.xvalue):"h"
		else
			print @str,p.xvalue:"e16.16"
		fi

	when real32_opnd then
!		print @str,p.xvalue32:"e16.16"
		print @str, p.xvalue

	when realimm_opnd, realimm32_opnd THEN
		print @str,p.xvalue,"IMM"


	when string_opnd then
		if (length:=strlen(p.svalue))<str.len/2 then
			strcpy(str,"""")
			convertstring(p.svalue,&.str+1)
			strcat(str,"""")

!			strcat(str,""" (L")
!			strcat(str,strint(p.strindex))
!			strcat(str,")")
		else

!CPL "<LONGSTR>"
RETURN "<LONGSTR>"
!			if longstring then
!				pcm_free(longstring,longstringlen)
!			fi
!			longstringlen:=length*2
!			longstring:=pcm_alloc(longstringlen)
!			longstring^:='"'
!			length:=convertstring(p.svalue, longstring+1)
!			(longstring+length+1)^:='"'
!			(longstring+length+2)^:=0
!			return longstring
		fi

	when mem_opnd then
		d:=p.def
!CPL =D
!IF D=NIL THEN
!	STRCAT(STR, "<NULL DEF>")
!ELSE
		strcat(str, getfullname(p.def))
		if p.opcode in [kistatic, kzstatic] then
			strcat(str,":")
			if d.exported then
				strcat(str,":")
			fi
		fi
!FI


	when memaddr_opnd then
		strcpy(str, "&")
		recase mem_opnd

!		d:=p.def
!		fprint @str,"&##",(d.truename|"`"|""),d.name
!!		fprint @str,"&##[#]",(d.truename|"`"|""),d.name, (d.owner|d.owner.name|"-")
!
	when label_opnd then
		fprint @str,"L# ",p.labelno

	when no_opnd then
		return ""

	when assem_opnd then
		return strint(int(p.asmcode))

	when data_opnd then
!		fprint @str,"<Data * #>", p.size
		fprint @str,"<Data * # (#)>", p.size,p.svalue

	else
!CPL "BAD OPND"
		println OPNDNAMES[P.OPNDTYPE]
		return "<PCLOPND?>"
	esac

	return str
end

global func strpclstr(pcl p)ichar=
	gs_free(dest)
	gs_init(dest)
	destlinestart:=0
	strpcl(p)
	gs_char(dest,0)
!CPL "//",DEST.LENGTH
	dest.strptr
end

global proc writepcl(pcl p)=

	CASE P.OPCODE
	WHEN KSETARG, KSETCALL THEN
		RETURN
	ESAC

	strpcl(p)
	case p.opcode
	when kdefproc then
	when kstore, kistore, kistorex, keval then
		gs_line(dest)
		gs_line(dest)

	else
		gs_line(dest)
	esac


end

global func writeallpcl:ref strbuffer=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	psymbol d,e

!CPL "ALLPCL"

	gs_init(dest)
	destlinestart:=dest.length

!	gs_strln(dest, "PROC PCL")

	p:=pcstart

	while p<=pccurr do
!CPL =P, PCLNAMES[P.OPCODE]
		writepcl(p)

		destlinestart:=dest.length
		++p
	od

	psline()

!	if fshowsymbols then
!		writesymbols()
!	fi


	if longstring then
		pcm_free(longstring,longstringlen)
	fi
	return dest
!	return (dest.strptr,dest.length)
end

global proc psstr(ichar s)=
	gs_str(dest,s)
end

global proc psstrline(ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

global proc psline=
	gs_line(dest)
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
	psstr("L")
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

global proc psopnd(pcl p)=
	psstr(stropnd(p))
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
		psline()
	od
end

global func convertstring(ichar s, t)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!returns length of t
	int c
	ichar t0:=t
	[16]char str

	while c:=s++^ do
		case c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when 10 then
			t++^:='\\'
			t++^:='n'
		when 13 then
			t++^:='\\'
			t++^:='c'
		when 9 then
			t++^:='\\'
			t++^:='t'
		when '\\' then
			t++^:='\\'
			t++^:='\\'
		when 7,8,26,27 then
			t++^:='<'
			t++^:=c/10+'0'
			t++^:=(c rem 10)+'0'
			t++^:='>'
		elsif c in 32..126 then
			t++^:=c
		else
			t++^:='\\'
			t++^:='x'
			print @str,c:"z2h"
			t++^:=str[1]
			t++^:=str[2]
		esac
	od
	t^:=0

	return t-t0
end

proc fullname(ichar dest, psymbol d) =
	if d.owner then
		fullname(dest, d.owner)
		strcat(dest, ".")
	fi
	strcat(dest, d.name)
end

export proc pcl_writepst(filehandle f=nil)=
	byte localfile:=0
	[256]char str

	psymbol d
	int i:=0

	if f=nil then
		f:=fopen("PSYMTAB","wb")
		localfile:=1
	fi

	println @f, "PROC PC Symbol table"
	println @f

	d:=psymboltable

	while d, d:=d.next do
		++i

		print @f, i:"4", idnames[d.id],,":"
		to 8-strlen(idnames[d.id]) do print @f, " " od

		str[1]:=0

		fullname(str, d)

		print @f, str:"25jl"

		print @f, strpmode(d.mode, d.size)

		if d.id=proc_id then
			fprint @f, " Pm:# Loc:#", d.nparams, d.nlocals
		fi

		if d.exported then print @f," Exp" fi
		if d.imported then print @f," Imp" fi
		if d.isthreaded then print @f," TC" fi

		print @f, " Seg:",segmentnames[d.segment]

		println @f
	od
	println @f

	if localfile then
		fclose(f)
	fi

end


=== pc_run.m 0 0 6/41 ===
!PCL Interpreter

!int dostep=1
int dostep=0
int go

!PCL PPP

!const dostackcheck = 1
const dostackcheck = 0

!const magic = 0x1438'B6F2'778E'A388
!const magic = 0x1438'B6F2'778E'0000

macro getopcode      = pc.opcode
macro getseqno       = pc.seqno
macro steppc         = ++pc
macro getnargs       = pc.nargs
macro getnvars       = pc.nvariadics
macro getnparams     = pc.paramslots
macro getnlocals     = pc.localslots
macro getcond        = pc.condcode
macro getlabel       = labeltable[pc.labelno]
macro getlabel2      = labeltable[(pc+1).labelno]
macro getmode        = pc.mode
macro getmode2       = pc.mode2
macro getsize        = pc.size
macro getscale       = pc.scale
macro getextra       = pc.extra
macro getincr        = pc.stepx
macro getswmin       = pc.minlab
macro getswmax       = pc.maxlab
macro isfloat        = ispfloat(getmode)
macro issigned       = psigned[getmode]


ref[]pcl labeltable

global macro pcerror(a) = pcerrorx(pc, a)
global macro pcerror2(a,b) = pcerrorx(pc, a,b)

global func dispatch_loop(pcl pcentry, int cmain=0)int=
!	const stacksize = 10'000
	const stacksize = 70'000
	const callstacksize = 1'000
	const pcmask=15

!HEAP VERSION
!	ref[]int stack
!	ref [stacksize]real		xstack @stack
!	ref [stacksize]word		ustack @stack
!	ref [stacksize]ref void	pstack @stack
!
	pcl pc:=pcentry
	int sp:=0
	int fp:=0
	ref byte ptr

	ref byte pcb @pc
	u64 pci @pc

	int		a
	real	x	@a
	real32	sx	@a
	word	u	@a

	int		b
	real	y	@b
	real32	sy	@b
	word	v	@b

!	int opc
	ref u8  pu8		@ptr
	ref u16 pu16	@ptr
	ref u32 pu32	@ptr
	ref u64 pu64	@ptr
	ref i8  pi8		@ptr
	ref i16 pi16	@ptr
	ref i32 pi32	@ptr
	ref i64 pi64	@ptr
	ref r32 pr32	@ptr
	pcl newpc		@ptr
	u64 newpci		@ptr

	ref byte ptrb
	int n
	psymbol d

!STACK VERSION
	[stacksize]int		stack
	[stacksize]real		xstack @stack
	[stacksize]word		ustack @stack
	[stacksize]ref void	pstack @stack

	[callstacksize]u32 callstack
	[callstacksize]psymbol callstackst
	int callsp:=0
	[256]char str

	INT MAG, OLDSP

	macro zz = sp
	macro yy = sp-1
	macro xx = sp-2
	macro ww = sp-3

	if cmain then
		INT NCMD:=NCMDPARAMS
		REF[0:]ICHAR CMD := CMDPARAMS
		NCMD:=NCMD-PCMDSKIP
		CMD:=CAST(REF BYTE(CMD)+PCMDSKIP*8)
		PSTACK[++SP]:=CMD
		STACK[++SP]:=NCMD+1
		STACK[++SP]:=0
	fi


!HEAP VERSION
!	stack:=pcm_alloc(stacksize*int.bytes)

INT LASTPOS:=0

INT COUNT:=0

!	do
!!IF INT(PC) IAND 15 THEN
!!PCERROR("BAD PC")
!!FI
!!
!!	if sp>(stacksize-100) then pcerror("Stack overflow") fi
!!	if callsp>(callstacksize-100) then pcerror("Call Stack overflow") fi
!
!!	IF PC.OPCODE=KCALLP AND EQSTRING(PC.DEF.NAME,"debug") THEN DOSTEP:=1;GO:=0 FI
!	goto skip when pc.opcode in [kcomment, ksetarg, klabel]
!	goto skip when not dostep
!
!
!!	ichar filename, sourceline
!!	int lineno
!
!!	if igetmsourceinfo then
!!		if pc.pos<>lastpos then
!!			lineno:=igetmsourceinfo(pc.pos, &filename, &sourceline)
!!			lastpos:=pc.pos
!!		fi
!!		CPL "Line",lineno:"4","in", filename
!!	fi
!	CPL pc.seqno:"z4", PCLNAMES[GETOPCODE]:"9JL", strpmode(pc.mode, pc.size), opndnames[pc.opndtype], =SP, =FP
!!	if sp then
!!		for i in max(1, sp-10)..sp do
!!			print  i:"9"
!!		od
!!		println
!!		for i in max(1, sp-10)..sp do
!!!			print  stack[i]:"9"
!!!			print  "<",,stack[i]:"9",,"> "
!!			print stack[i]:"H9"
!!		od
!!		println
!!	fi
!
!!	print callsp,"Callstack:"
!!	for i in max(1,callsp-20).. callsp do
!!		fprint "#:# ", i, callstack[i]
!!	od
!!	println
!!
!	if not go then
!		CASE OS_GETCH()
!		WHEN 27 THEN STOP
!		WHEN 'R','r' THEN DOSTEP:=0
!		WHEN 'G','g' THEN go:=1
!		ESAC
!	fi
!skip:
!
!	switch getopcode
!
!	doswitch getopcode
	doswitchu getopcode
	when knop      then
		steppc

	when kload     then
!CPL "LOAD", PC.DEF.NAME, PC.DEF.OFFSET, PC.DEF.STADDR
		stack[++sp]:=pci_getopnd(pc, &stack[fp])
!CPL "LOAD2", =PSTACK[SP]
		steppc

freddy:
	when kiload    then
		stack[sp]:=pci_loadptr(pstack[sp], getmode)
		steppc

	when kiloadx   then
		a:=stack[sp--]				!index
		ptr := ref byte(pstack[sp]) + a*getscale + getextra
		stack[sp]:=pci_loadptr(ptr, getmode)
		steppc

	when kstore    then
		case pc.opndtype
		when mem_opnd then

			d:=pc.def
			if d.id=static_id then
				pi64:=d.staddr
			elsif getmode=tpblock and d.id=param_id then
				pi64:=pstack[fp+d.offset]
			else
				pi64:=&stack[fp+d.offset]
!CPL "STORE", D.NAME, D.OFFSET, STRPMODE(GETMODE, GETSIZE)
			fi

!PTRB:=PSTACK[SP]
!CPL =PTR, =PTRB, =D.NAME, =D.STADDR, =D.OFFSET
!CPL "STORING FROM", PTRB,"TO", PTR
!CPL =PTRB^
!CPL "STORE///",PSTACK[SP]
			pci_storeptr(ptr, stack[sp--], getmode, getsize)
!STOP
		else
			pcusopnd(pc)
		esac
		
		steppc

	when kistore   then
		ptr:=pstack[sp--]
		pci_storeptr(ptr, stack[sp--], getmode, getsize)
		steppc

	when kistorex  then
		a:=stack[sp--]				!index
		ptr := ref byte(pstack[sp--]) + a*getscale + getextra
!CPL "ISTOREX",PTR,A,GETSCALE, GETEXTRA,=SP, =FP,PSTACK[SP+1],=pc.seqno
		pci_storeptr(ptr, stack[sp--], getmode, getsize)
		steppc

	when kdupl, kdouble   then
		a:=stack[sp]
		stack[++sp]:=a
		steppc

	when kswapstk  then
!CPL =PC.X, =PC.Y
		swap(stack[sp-(pc.x-1)], stack[sp-(pc.y-1)])
		steppc

	when kunload   then
		--sp
		steppc

	when kopnd     then
		unimpl
		steppc

	when ktype     then
		unimpl
		steppc

	when kloadbit  then	! Z' := Y.[Z]
		stack[sp-1]:=stack[sp-1].[stack[sp]]
		--sp
		steppc

    when kloadbf   then	! Z' := X.[Y..Z]
		a:=pci_loadbf(stack[xx], stack[yy], stack[zz])
		sp-:=2
		stack[sp]:=a
		steppc

	when kstorebit then	! Y^.[Z] := X
		ptr:=pstack[yy]
		a:=pci_loadptr(ptr, getmode)
		a.[stack[zz]]:=stack[xx]

		pci_storeptr(ptr, a, getmode)
		sp-:=3
		steppc

	when kstorebf  then	! X^.[Y..Z] := W
		ptr:=pstack[xx]
		a:=pci_loadptr(ptr, getmode)
		a:=pci_storebf(a, stack[yy], stack[zz], stack[ww])
		pci_storeptr(ptr, a, getmode)
		sp-:=4
		steppc

	when kcallp, kcallf    then
		d:=pc.def
		if d.imported then
			n:=getnargs
			sp-:=n-1				!point to first arg
			a:=docalldll(d, nil, cast(&stack[sp]), n, getnvars, getmode)
			if pc.opcode=kcallp then
				--sp
			else
				stack[sp]:=a
			fi
			steppc
		elsif not d.pcaddr then
			pcerror2("Proc not defined:",d.name)
		else
			if dostackcheck then
				callstack[++callsp]:=sp-getnargs+(getopcode=kcallf|1|0)
			fi

			stack[++sp]:=int(pc+1) ior getnargs
			pc:=d.pcaddr

		fi

	when kretproc  then
		n:=getnparams
		sp-:=getnlocals
		fp:=stack[sp--]

		if dostackcheck then
			newpc:=pstack[sp--]
			sp-:=newpci iand pcmask
			if callsp<1 then pcerror("retp/call underflow") fi
			oldsp:=callstack[callsp--]
			if sp<>oldsp then
				fprint @str,"RETP/SP mismatch: old=# curr=# ",oldsp, sp
				pcerror(str)
			fi

!			pci:=newpci iand 0xFFFF'FFFF'FFFF'FFE0
			pci:=newpci iand inot pcmask

		else
			pc:=pstack[sp--]
			sp-:=pci iand pcmask
!			pci iand:=0xFFFF'FFFF'FFFF'FFF0
			pci iand:=inot pcmask

		fi

	when kretfn    then
		a:=stack[sp]
		n:=getnparams
		sp-:=getnlocals
		fp:=stack[--sp]

		if dostackcheck then
			newpc:=cast(stack[--sp])
			sp-:=newpci iand pcmask
			stack[sp]:=a

			if callsp<1 then pcerror("ref/call underflow") fi
			oldsp:=callstack[callsp--]
			if sp<>oldsp then
				fprint @str,"RETF/SP mismatch: old=# curr=# ", oldsp, sp
				pcerror(str)
			fi
!			pci:=newpci iand 0xFFFF'FFFF'FFFF'FFE0
			pci:=newpci iand inot pcmask
		else
			pc:=cast(stack[--sp])
			sp-:=pci iand pcmask
			stack[sp]:=a
!			pci iand:=0xFFFF'FFFF'FFFF'FFE0
			pci iand:=inot pcmask
		fi

	when kicallp   then
		ptr:=pstack[sp]
IF PTR=NIL THEN PCERROR("ICALLP NIL PTR") FI
!CPL "CALLP",PTR


		if newpc>=pcstart and newpc<=pccurr then		!assume local
icallp:
			if dostackcheck then
				callstack[++callsp]:=SP-1-GETNARGS+(GETOPCODE=KICALLF|1|0)
				CALLSTACKST[CALLSP]:=NIL
			fi
			stack[sp]:=int(pc+1) ior getnargs
			pc:=newpc
		else	
			n:=getnargs
			--sp
			sp-:=n-1
			docalldll(nil, cast(ptr), cast(&stack[sp]), n, getnvars, getmode)
			--sp
			steppc
		fi

	when kicallf   then
		ptr:=pstack[sp]
IF PTR=NIL THEN PCERROR("ICALLF NIL PTR") FI
!CPL "CALLF", PTR

		if newpc>=pcstart and newpc<=pccurr then		!assume local
			goto icallp
		else	
			n:=getnargs
			--sp
			sp-:=n-1
!CPL "DOING ICALL/DLL", N, =GETNVARS
			a:=docalldll(nil, cast(ptr), cast(&stack[sp]), n, getnvars, getmode)
!CPL
			stack[sp]:=a
			steppc
		fi


	when kjump     then
		pc:=getlabel

	when kijump    then
		unimpl
		steppc

	when kjumpcc   then
		if ispfloat(getmode) then
			n:=cmpreal(getcond, xstack[sp-1], xstack[sp])
		elsif psigned[getmode] then
			n:=cmpint(getcond, stack[sp-1], stack[sp])
		else
			n:=cmpword(getcond, stack[sp-1], stack[sp])
		fi

		if pc.popone and not n then
			--sp
		else
			sp-:=2
		fi

		if n then
			pc:=getlabel
		else
			steppc
		fi

	when kjumpt    then
		if stack[sp--] then				!ignore possibility of -0.0
			pc:=getlabel
		else
			steppc
		fi

	when kjumpf    then
		if stack[sp--]=0 then
			pc:=getlabel
		else
			steppc
		fi

	when kjumpret  then
		pc:=getlabel

	when kjumpretm then
		unimpl
		steppc

	when ksetcc    then
		case getmode
		when tpr64 then
			pcerror("setcc/r64")
		when tpr32 then
			pcerror("setcc/r32")
		elsif psigned[getmode] then
			n:=cmpint(getcond, stack[sp-1], stack[sp])
		else
			n:=cmpword(getcond, stack[sp-1], stack[sp])
		esac
		--sp
		stack[sp]:=n

		steppc

	when kstop     then
!OS_GETCH()
CPL "STOP SP=",SP-1, = PC.SEQNO
		return stack[sp--]

	when kto       then
		pi64:=pci_getopndaddr(pc+1, &stack[fp])	
		--(pi64^)
		if pi64^ then
			pc:=getlabel
		else
			pc+:=2
		fi

	when kforup    then
		ptr:=cast(pci_getopndaddr(pc+1, &stack[fp]))
		n:=pci_getopnd(pc+2, &stack[fp])
		pi64^+:=pc.stepx
		if pi64^ <= n then
			pc:=getlabel
		else
			pc+:=3
		fi

	when kfordown  then
		ptr:=cast(pci_getopndaddr(pc+1, &stack[fp]))
		n:=pci_getopnd(pc+2, &stack[fp])
		pi64^-:=pc.stepx
		if pi64^ >= n then
			pc:=getlabel
		else
			pc+:=3
		fi


	when kiswap    then
		if getmode=tpblock then pcerror("swap/block") fi
		ptr:=pstack[sp--]
		ptrb:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=pci_loadptr(ptrb, getmode)
		pci_storeptr(ptr, b, getmode)
		pci_storeptr(ptrb, a, getmode)
		steppc

	when kswitch   then
		a:=stack[sp--]
		if a in getswmin..getswmax then
			pc:=pc+a-getswmin+3
			pc:=labeltable[pc.labelno]
		else
			pc:=getlabel2
		fi

	when kswitchu  then
		unimpl
		steppc

	when kswlabel  then
		unimpl
		steppc

	when kendsw    then
		unimpl
		steppc

	when kclear    then
		memset(pstack[sp--], 0, pc.size)
		steppc

	when kassem    then
		unimpl
		steppc

	when kadd      then
		if ispfloat(getmode) then
			xstack[sp-1]+:=xstack[sp]
		else
			stack[sp-1]+:=stack[sp]
		fi
		--sp

		steppc

	when ksub      then
		if ispfloat(getmode) then
			xstack[sp-1]-:=xstack[sp]
		else
			stack[sp-1]-:=stack[sp]
		fi
		--sp

		steppc


	when kmul      then
		if ispfloat(getmode) then
			xstack[sp-1]*:=xstack[sp]
		else
			stack[sp-1]*:=stack[sp]
		fi
		--sp

		steppc

	when kdiv      then
		xstack[sp-1]:=xstack[sp-1]/xstack[sp]
		--sp
		steppc

	when kidiv     then
		if psigned[getmode] then
			stack[sp-1]:=stack[sp-1]/stack[sp]
		else
			ustack[sp-1]:=ustack[sp-1]/ustack[sp]
		fi
		--sp
		steppc

	when kirem     then
		if psigned[getmode] then
			stack[sp-1]:=stack[sp-1] rem stack[sp]
		else
			ustack[sp-1]:=ustack[sp-1] rem ustack[sp]
		fi
		--sp
		steppc

	when kidivrem  then
		unimpl
		steppc

	when kbitand   then
		stack[sp-1] iand:=stack[sp]
		--sp
		steppc

	when kbitor    then
		stack[sp-1] ior:=stack[sp]
		--sp
		steppc

	when kbitxor   then
		stack[sp-1] ixor:=stack[sp]
		--sp
		steppc

	when kshl      then
		ustack[sp-1] <<:= ustack[sp]
		--sp
		steppc

	when kshr      then
		if psigned[getmode] then
			stack[sp-1] >>:= stack[sp]
		else
			ustack[sp-1] >>:= ustack[sp]
		fi	
		--sp
		steppc

	when kmin      then
		if ispfloat(getmode) then
			xstack[sp-1] min:= xstack[sp]
		elsif psigned[getmode] then
			stack[sp-1] min:= stack[sp]
		else
			ustack[sp-1] min:= ustack[sp]
		fi
		--sp
		steppc

	when kmax      then
		if ispfloat(getmode) then
			xstack[sp-1] max:= xstack[sp]
		elsif psigned[getmode] then
			stack[sp-1] max:= stack[sp]
		else
			ustack[sp-1] max:= ustack[sp]
		fi
		--sp
		steppc

	when kaddpx    then
		a:=stack[sp--]				!index
		pstack[sp] := ref byte(pstack[sp]) + a*getscale + getextra
		steppc

	when ksubpx    then
		a:=stack[sp--]				!index
		pstack[sp] := ref byte(pstack[sp]) - a*getscale + getextra
		steppc

	when ksubp     then
		stack[sp-1]:=(stack[sp-1]-stack[sp])/getscale
		--sp
		steppc

	when kneg      then
		if ispfloat(getmode) then
			xstack[sp] := -xstack[sp]
		else
			stack[sp] := -stack[sp]
		fi
		steppc

	when kabs      then
		if ispfloat(getmode) then
			xstack[sp] := abs xstack[sp]
		else
			stack[sp] :=  abs stack[sp]
		fi
		steppc

	when kbitnot   then
		ustack[sp]:=inot ustack[sp]
		steppc

	when knot      then
		stack[sp] := stack[sp] ixor 1
		steppc

	when ktoboolt  then
		stack[sp]:=istrue stack[sp]
		steppc

	when ktoboolf  then
		stack[sp]:=not stack[sp]
		steppc

	when ksqr      then
		if ispfloat(getmode) then
			xstack[sp]*:=xstack[sp]
		else
			stack[sp]:=sqr stack[sp]
		fi

		steppc

	when ksqrt     then
!CPL "SQRT", XSTACK[SP]
		xstack[sp]:=sqrt(xstack[sp])
!CPL "SQRT2", XSTACK[SP]
		steppc

	when ksin      then
		unimpl
		steppc

	when kcos      then
		unimpl
		steppc

	when ktan      then
		unimpl
		steppc

	when kasin     then
		unimpl
		steppc

	when kacos     then
		unimpl
		steppc

	when katan     then
		unimpl
		steppc

	when klog      then
		unimpl
		steppc

	when klog10    then
		unimpl
		steppc

	when kexp      then
		unimpl
		steppc

	when kround    then
		unimpl
		steppc

	when kfloor    then
		unimpl
		steppc

	when kceil     then
		unimpl
		steppc

	when ksign     then
		unimpl
		steppc

	when katan2    then
		unimpl
		steppc

	when kpower    then
		unimpl
		steppc

	when kfmod     then
		unimpl
		steppc

	when kincrto   then
		doincr(pstack[sp--], getincr, getmode)
		steppc

	when kdecrto   then
		doincr(pstack[sp--], -getincr, getmode)
		steppc

	when kincrload then
		ptr:=pstack[sp]
		doincr(ptr, getincr, getmode)
		stack[sp]:=pci_loadptr(ptr, getmode)
		steppc

	when kdecrload then
		ptr:=pstack[sp]
		doincr(ptr, -getincr, getmode)
		stack[sp]:=pci_loadptr(ptr, getmode)
		steppc


	when kloadincr then
		ptr:=pstack[sp]
		stack[sp]:=pci_loadptr(ptr, getmode)
		doincr(ptr, getincr, getmode)
		steppc

	when kloaddecr then
		ptr:=pstack[sp]
		stack[sp]:=pci_loadptr(ptr, getmode)
		doincr(ptr, -getincr, getmode)
		steppc

	when kaddto    then		!Z^ +:= Y
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]

		if ispfloat(getmode) then
			a:=int@(real@(a)+real@(b))
		else
			a+:=b
		fi

		pci_storeptr(ptr, a, getmode)
		steppc

	when ksubto    then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]

		if ispfloat(getmode) then
			a:=int@(real@(a)-real@(b))
		else
			a-:=b
		fi

		pci_storeptr(ptr, a, getmode)
		steppc

	when kmulto    then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]

		if ispfloat(getmode) then
			a:=int@(real@(a)*real@(b))
		else
			a*:=b
		fi

		pci_storeptr(ptr, a, getmode)
		steppc

	when kdivto    then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]

		if ispfloat(getmode) then
			a:=int@(real@(a)/real@(b))
		else
			a:=a/b
		fi

		pci_storeptr(ptr, a, getmode)
		steppc

	when kidivto   then
		unimpl
		steppc

	when kiremto   then
		unimpl
		steppc

	when kbitandto then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]
		a iand:=b
		pci_storeptr(ptr, a, getmode)
		steppc

	when kbitorto  then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]
		a ior:=b
		pci_storeptr(ptr, a, getmode)
		steppc

	when kbitxorto then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]
		a ixor:=b
		pci_storeptr(ptr, a, getmode)
		steppc

	when kshlto    then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]
		a<<:=b
		pci_storeptr(ptr, a, getmode)
		steppc

	when kshrto    then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]
		if psigned[getmode] then
			a>>:=b
		else
			u>>:=v
		fi
		pci_storeptr(ptr, a, getmode)
		steppc

	when kminto    then
		unimpl
		steppc

	when kmaxto    then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]

		if isfloat then
			a:=int@(max(real@(a),real@(b)))
		elsif issigned then
			a max:=b
		else
			u max:=v
		fi

		pci_storeptr(ptr, a, getmode)
		steppc

	when kaddpxto  then
		pu64:=pstack[sp--]
		pu64^+:=stack[sp--]*getscale
		steppc

	when ksubpxto  then
		pu64:=pstack[sp--]
		pu64^-:=stack[sp--]*getscale
		steppc

	when knegto    then
		unimpl
		steppc

	when kabsto    then
		unimpl
		steppc

	when kbitnotto then
		unimpl
		steppc

	when knotto    then
		unimpl
		steppc

	when ktoboolto then
		unimpl
		steppc

	when ktypepun  then
!		unimpl
		steppc

	when kfloat    then
		if psigned[getmode2] then
			xstack[sp]:=stack[sp]
		else
			xstack[sp]:=ustack[sp]
		fi
		steppc

	when kfix      then
		stack[sp]:=xstack[sp]
		steppc

	when ktruncate then
		stack[sp]:=pci_loadptr(cast(&stack[sp]), getmode2)
		steppc

	when kwiden    then
		stack[sp]:=pci_loadptr(cast(&stack[sp]), getmode2)
		steppc

	when kfwiden   then				!no-op: value already widened
		steppc

	when kfnarrow  then				!will be narrowed on store
		steppc

	when kstartmx  then
		steppc

	when kresetmx  then
		steppc

	when kendmx    then
		steppc

	when kdefproc  then					!proc entry code
!CPL "DEFPROC ENTRY SP=",SP, =GETNLOCALS
IF PC.DEF.PCADDR<>PC THEN PCERROR("DEFPROC ADDR MISMATCH") FI

		stack[++sp]:=fp
		fp:=sp

		sp+:=getnlocals
!		if dostackcheck then
!			++sp
!			stack[sp]:=magic + SP-1
!CPL "DEFPROC STORE SP", SP
!		fi
		steppc

	when ktcproc   then
		unimpl
		steppc

	when kendproc  then
		unimpl
		steppc

	when kistatic  then
		unimpl
		steppc

	when kzstatic  then
		unimpl
		steppc

	when kdata     then
		unimpl
		steppc

	when klabel    then
		steppc

	when klabeldef then
!		unimpl
		steppc

	when ksetjmp   then
		ptr:=pstack[sp]
		pu64^:=cast(pc+1)				!label of next instr
		(pu64+1)^:=sp
		(pu64+2)^:=fp

		stack[sp]:=0

		steppc

	when klongjmp  then
		a:=stack[sp--]					!ret value
		ptr:=pstack[sp--]
		pc:=cast(pi64^)
		sp:=(pi64+1)^
		fp:=(pi64+2)^
		stack[++sp]:=a

	when ksetcall  then
!CPL "SETCALL", =SP
!		unimpl
		steppc

	when ksetarg   then
!		unimpl
		steppc

	when kloadall  then
		unimpl
		steppc

	when kstoresl  then
		unimpl
		steppc

	when kstoresld then
		unimpl
		steppc

	when ksliceupb then
		unimpl
		steppc

	when kslicelen then
		unimpl
		steppc

	when ksliceptr then
		unimpl
		steppc

	when keval     then
		--sp
		steppc

	when kcomment  then
		steppc

	when kendprog  then
		unimpl
		steppc

	else
unimpl:
		println
		fprintln "Unimpl: # at seq: #", pclnames[getopcode], getseqno
		println
		stop 1
	end doswitch
!	end end
	0
end

proc fixuppcl=
!allocate memory for statics
	pcl p
	psymbol d,e, dproc
	ref byte pdata
	int parambytes, framebytes
	int paramslots, localslots
	u64 a

	labeltable:=pcm_alloc((mlabelno+1)*pcl.bytes)

!do static fixups in two passes, as sometimes the ordering gets mixed up
!first pass allocates spaces, second deals with initialisation that may include
!references to static data declared later

	p:=pcstart
	while p<=pccurr, ++p do
		case p.opcode
		when kistatic, kzstatic then
			d:=p.def
			d.staddr:=pcm_allocz(p.size)
!CPL "SET ADDR", D.NAME, D.STADDR
		esac
!CPL P.SEQNO, PCLNAMES[P.OPCODE]

!IF P.SEQNO=36351 THEN PPP:=P FI


	od

!CPL "PPP=",PCLNAMES[PPP.OPCODE], PPP.SEQNO

!STOP

	p:=pcstart
	while p<=pccurr, ++p do
		case p.opcode
		when kistatic, kzstatic then
			pdata:=p.def.staddr

		when kdata then
			if p.mode<>tpblock then
				case p.opndtype
				when mem_opnd then
PCERRORX(P,"FIX/DATA/MEM")
				when memaddr_opnd then
					d:=p.def
!CPL "DATA/MEMADDR", P.DEF.NAME
					case d.id
					when static_id then
						a:=cast(d.staddr)
					when proc_id then
						a:=cast(d.pcaddr)
					when import_id then
						a:=cast(getdllfnptr(d))
					else
						pcerrorx(p,"data &mem")
					esac
					if a=0 then
						pcerrorx(p,"data &mem = nil")
					fi
				else
					a:=p.value
				esac

				memcpy(pdata, &a, p.size)
			else
				memcpy(pdata, p.svalue, p.size)
			fi
!CPL "STORED PDATA", PDATA^, =PDATA
			pdata+:=p.size

		when kdefproc then
			dproc:=d:=p.def
			e:=d.nextparam
			parambytes:=0
			while e, e:=e.nextparam do
				parambytes+:=8
				e.offset:=-(parambytes/8+1)
!CPL "PARAM", E.NAME, E.OFFSET
			od

			e:=d.nextlocal
			framebytes:=0
			while e, e:=e.nextlocal do
				e.offset:=framebytes/8+1
				framebytes+:=roundtoblock(E.size,8)
!CPL "LOCAL", E.NAME, E.OFFSET
			od

			p.paramslots:=paramslots:=parambytes/8
			p.localslots:=localslots:=framebytes/8

		when klabel then
			labeltable[p.labelno]:=p

		when kretproc, kretfn then
			p.paramslots:=paramslots
			p.localslots:=localslots

		esac
	od

end

export proc pcl_runpcl=
!	int tt:=clock()
	int stopcode

	loadlibs()

CPL "FIX1"

	fixuppcl()
CPL "FIX2"

!CPL "RUNPCL", PVERBOSE

	if entryproc=nil then
		pcerrorx(pcstart,"No 'main' entry point")
	fi

CPL =ENTRYPROC.PCADDR, PCSTART, PCCURR,"//", ENTRYPROC.PCADDR-PCSTART
!CPL "STOPPING"
!STOP

	docmdskip()

	if pverbose then
		println "Run PCL:"
	fi

	stopcode:=dispatch_loop(entryproc.pcaddr, entryproc.nparams=2)

!CPL "STOPPED"

!	tt:=clock()-tt
	if pverbose then
		println "Stopped",stopcode
		println
	fi

!	showprocs()


	stop stopcode
!	println
end
=== pc_runaux.m 0 0 7/41 ===


global func pci_getopnd(pcl p, ref i64 locals)i64 a =
!return operand value
!locals is a pointer stack[fp]

	psymbol d
	ref byte         ptr
	ref u8  pu8		@ptr
	ref u16 pu16	@ptr
	ref u32 pu32	@ptr
	ref u64 pu64	@ptr
	ref i8  pi8		@ptr
	ref i16 pi16	@ptr
	ref i32 pi32	@ptr
	ref i64 pi64	@ptr
	ref r32 pr32	@ptr
	ref r64 pr64	@ptr

!CPL "GETOPND", OPNDNAMES[P.OPNDTYPE]

	case p.opndtype
	when int_opnd then
		a:=p.value

	when mem_opnd then
		d:=p.def
!CPL "GO/MEM",D.NAME, =IDNAMES[D.ID]
		case d.id
		when static_id then
			pi64:=d.staddr
		else
!CPL "GET/LOCAL",D.NAME, D.OFFSET
			pi64:=locals+d.offset
			if d.mode=tpblock and d.id=param_id then pi64:=cast(pi64^) fi
		esac
!IF P.MODE=TPBLOCK THEN CPL "GETOPND/BLOCK/MEM" FI
!		if p.mode=tpblock then
!			a:=cast(pi64)
!		else
			a:=pci_loadptr(ptr, p.mode)
!CPL "LOADED",STRPMODE(P.MODE), A:"H", "FROM", PTR, PI64^:"h"
!		fi

	when memaddr_opnd then
		d:=p.def
		case d.id
		when local_id then
			a:=cast(locals+d.offset)
		when param_id then
			a:=cast(locals+d.offset)
			if d.mode=tpblock then			!need value of param not its address
				ptr:=cast(a)				!value contains reference to block
				a:=pu64^
			fi
		when import_id then
			a:=cast(getdllfnptr(d))

		else
			a:=cast(d.staddr)		!also does proc_id/import_id
		esac

	when string_opnd then
		a:=cast(p.svalue)

	when real_opnd, real32_opnd then
		a:=int@(p.xvalue)

	else
		pcusopnd(p)
	esac

	a
end

global func pci_loadptr(ref byte p, int mode)i64 =
!p points to a primitive or block
!any scaling/offset has been applied

	ref u8  pu8		@p
	ref u16 pu16	@p
	ref u32 pu32	@p
	ref u64 pu64	@p
	ref i8  pi8		@p
	ref i16 pi16	@p
	ref i32 pi32	@p
	ref i64 pi64	@p
	ref r32 pr32	@p
	ref r64 pr64	@p
	real x

	switch mode
	when tpblock then
!CPL "LOAD PTR/BLOCK", P
		cast(p)
	when tpr64 then
		pu64^
	when tpr32 then
		x:=pr32^				!widen to r64
		int@(x)					!typepun to i64

	when tpi64 then pi64^
	when tpi32 then pi32^
	when tpi16 then pi16^
	when tpi8  then pi8^

	when tpu64 then pu64^
	when tpu32 then pu32^
	when tpu16 then pu16^
	when tpu8  then pu8^
	else
		0
	end
end

global proc pci_storeptr(ref byte p, int a, mode, size=0) =
!p points to a primitive or block
!a represents any value including a real, or a block reference

	ref i8  pi8		@p
	ref i16 pi16	@p
	ref i32 pi32	@p
	ref i64 pi64	@p
	ref r32 pr32	@p
	real x

	case mode
	when tpblock then
!CPL "STORE BLOCK",=size,=P, =A:"h"
		memcpy(p, ref byte(a), size)

	when tpr32 then
		x:=real@(a)
		pr32^:=x

	elsecase psize[mode]
	when 8 then pi64^:=a
	when 4 then pi32^:=a
	when 2 then pi16^:=a
	else        pi8^ :=a
	esac
end

global func pci_getopndaddr(pcl p, ref i64 locals)ref i64 =
	psymbol d

	if p.opndtype<>mem_opnd then pcerrorx(p,"Not mem") fi

	d:=p.def
	if d.id=static_id then
		d.staddr
	else
		locals+d.offset
	fi
end

global proc pcerrorx(pcl p, ichar mess, param="")=
	println "PC Exec error:",mess, param, "seq:", (p|p.seqno|0)
	println
	stop 1
end

global proc pcusopnd(pcl p)=
	println "Unsupported operand:", opndnames[p.opndtype],"in", pclnames[p.opcode], p.seqno
	println
	stop 1
end

global func docalldll(psymbol d, ref proc fnaddr, ref[]int revargs, int nargs, nvars, retmode)int=
!d=st entry, fnaddr=nil; or d=nil; fnaddr is fn ptr
!note: args are in reverse order
	[100]i64 args
	int retval

	if fnaddr=nil then
		fnaddr:=getdllfnptr(d)
	fi

	for i:=nargs downto 1 do
		args[nargs-i+1]:=revargs[i]
	od

	retval:=os_calldllfunction(fnaddr, (retmode in [tpr32,tpr64]|'R'|'I'), nargs, &args, nil)

	retval
end

global func getdllfnptr(psymbol d)ref proc fnaddr=
	int libindex
	word dllinst
	ichar procname, libname

	fnaddr:=d.dlladdr
	return fnaddr when fnaddr

	procname:=d.name

	for i to nplibfiles do
		fnaddr:=os_getdllprocaddr(plibinst[i],procname)
		exit when fnaddr
	else
		pcerrorx(nil,"Can't find DLL func:",procname)
	od
	d.dlladdr:=fnaddr
	return fnaddr
end

global proc loadlibs=
	for i to nplibfiles do
		plibinst[i]:=os_getdllinst(plibfiles[i])
		if not plibinst[i] then
			pcerrorx(nil, "Can't load lib:",plibfiles[i])
		fi
	od
end

global func cmpreal(int cond, real x, y)int=
	case cond
	when eq_cc then x=y
	when ne_cc then x<>y
	when lt_cc then x<y
	when le_cc then x<=y
	when ge_cc then x>=y
	else            x>y
	esac
end

global func cmpint(int cond, x, y)int=
	case cond
	when eq_cc then x=y
	when ne_cc then x<>y
	when lt_cc then x<y
	when le_cc then x<=y
	when ge_cc then x>=y
	else            x>y
	esac
end

global func cmpword(int cond, word x, y)int=
	case cond
	when eq_cc then x=y
	when ne_cc then x<>y
	when lt_cc then x<y
	when le_cc then x<=y
	when ge_cc then x>=y
	else            x>y
	esac
end

global proc doincr(ref byte pu8, int incr, mode)=
!uses negative incr for decr
	ref u16 pu16	@pu8
	ref u32 pu32	@pu8
	ref u64 pu64	@pu8

	case psize[mode]
	when 8 then	pu64^+:=incr
	when 4 then	pu32^+:=incr
	when 2 then	pu16^+:=incr
	else		pu8^+:=incr
	esac
end

global proc docmdskip=
	psymbol d

	d:=psymboltable
	while d, d:=d.next do
		if eqstring(d.name, "$cmdskip") then
!CPL "DONE $CMDSKIP", =PCMDSKIP
			(ref byte(d.staddr))^:=pcmdskip
			exit
		fi
	od
end

global func pci_loadbf(word a, i, j)word=
!a.[i..j]:=x; return new a
	u64 mask                    ! ...0000000111100000    i=5, j=8, n=4
	int n

	if j<i then swap(i,j) fi
	n:=j-i+1					! width of bitfield

	mask:=inot(inot(0) << n)<<i

	return (a iand mask)>>i
end

global func pci_storebf(word a, i, j, x)word =
!a.[i..j]:=x; return new a
	u64 mask                    ! ...0000000111100000    i=5, j=8, n=4
	u64 n

	if j<i then swap(i,j) fi
	n:=j-i+1					! width of bitfield

	mask:=inot(inot(0) << n)<<i

	x:=x<<i iand mask

	a iand inot(mask) ior x
end
=== pc_tables.m 0 0 8/41 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,
		[0:]byte psigned,
		[0:]byte pfloat,
		[0:]byte pmin,						!promoted type when min width applies
		[0:]byte piwrb =					!int/word/real/block

	(tpvoid=0,    "void",    	0,	0,0,	tpvoid,		tpvoid),

	(tpr32,       "r32",    	4,	0,1,	tpr32,		tpr32),
	(tpr64,       "r64",    	8,	0,1,	tpr64,		tpr64),

	(tpu8,        "u8",      	1,	0,0,	tpu32,		tpu64),
	(tpu16,       "u16",    	2,	0,0,	tpu32,		tpu64),
	(tpu32,       "u32",    	4,	0,0,	tpu32,		tpu64),
	(tpu64,       "u64",    	8,	0,0,	tpu64,		tpu64),

	(tpi8,        "i8",      	1,	1,0,	tpi32,		tpi64),
	(tpi16,       "i16",    	2,	1,0,	tpi32,		tpi64),
	(tpi32,       "i32",    	4,	1,0,	tpi32,		tpi64),
	(tpi64,       "i64",    	8,	1,0,	tpi64,		tpi64),

	(tpblock,     "block",   	0,	0,0,	tpblock,	tpvoid),
	(tpvector,    "vector",   	0,	0,0,	tpvector,	tpvoid),

	(tplast,      "$last",   	0,	0,0,	0,			0),


end

global const tpref = tpu64

!.opndtype in pclrec

global enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),
	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(label_opnd,		$),
	(int_opnd,			$),
	(real_opnd,			$),
	(real32_opnd,		$),
	(string_opnd,		$),
	(strimm_opnd,		$),
	(assem_opnd,		$),
	(realimm_opnd,		$),
	(realimm32_opnd,	$),
	(data_opnd,			$),
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

export enumdata [0:]ichar pclnames, [0:]byte pclhastype, [0:]byte pclextra =
!                       t  x      (a  b)
	(knop=0,       $+1, 0, 0),  ! (0 - 0) (          ) ?

	(kload,        $+1, 1, 0),  ! (0 - 1) (M L t     ) Z' := M &M L &L 123 4.5 "abc"
	(kiload,       $+1, 1, 0),  ! (1 - 1) (t         ) Z' := Z^
	(kiloadx,      $+1, 1, 2),  ! (2 - 1) (t d       ) Z' := (Y + Z*s + d)^

	(kstore,       $+1, 1, 0),  ! (1 - 0) (M t       ) M := Z
	(kistore,      $+1, 1, 0),  ! (2 - 0) (t         ) Z^ := Y
	(kistorex,     $+1, 1, 2),  ! (3 - 0) (t s d     ) (Y + Z*s + d)^ := X

	(kdupl,        $+1, 0, 0),  ! (1 - 2) (          ) Z' := Y' := Z
	(kdouble,      $+1, 0, 0),  ! (1 - 2) (          ) Count extra instance of Z
	(kswapstk,     $+1, 0, 2),  ! (2 - 2) (a b       ) Swap(stack(a), stack(b)); 1/2/3/4 = Z/Y/X/W
	(kunload,      $+1, 1, 0),  ! (1 - 0) (t         ) Pop stack

	(kopnd,        $+1, 1, 0),  ! (0 - 0) (M L C t   ) Define auxiliary operand M or L
	(ktype,        $+1, 1, 0),  ! (0 - 0) (t         ) Define auxiliary type t

	(kloadbit,     $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y.[Z]
	(kloadbf,      $+1, 1, 0),  ! (3 - 1) (t         ) Z' := X.[Y..Z]
	(kstorebit,    $+1, 1, 0),  ! (3 - 0) (t         ) Y^.[Z] := X
	(kstorebf,     $+1, 1, 0),  ! (4 - 0) (t         ) X^.[Y..Z] := W

	(kcallp,       $+1, 0, 2),  ! (n - 0) (M n v     ) Call &M with nargs, then pop args; v = varargs
	(kicallp,      $+1, 0, 2),  ! (n - 0) (n v       ) Call Z with nargs, then pop args (a=n+1)
	(kretproc,     $+1, 0, 0),  ! (0 - 0) (          ) Return from proc
	(kcallf,       $+1, 1, 2),  ! (n - 1) (M t n v   ) Call &M, then pop args, leave retval; v = varrgs
	(kicallf,      $+1, 1, 2),  ! (n - 1) (t n v     ) Call Z, then pops args, leave retval (a=n+1)
	(kretfn,       $+1, 1, 0),  ! (0 - 0) (t         ) Return from func with Z=retval

	(kjump,        $+1, 0, 0),  ! (0 - 0) (L         ) goto L
	(kijump,       $+1, 0, 0),  ! (1 - 0) (          ) goto Z
	(kjumpcc,      $+1, 1, 1),  ! (2 - n) (L t c p   ) goto L when Y c Z; p=1: Z':=Y (b=0/1)
	(kjumpt,       $+1, 1, 0),  ! (1 - 0) (L t       ) goto L when Z is true
	(kjumpf,       $+1, 1, 0),  ! (1 - 0) (L t       ) goto L when Z is false
	(kjumpret,     $+1, 1, 0),  ! (1 - 0) (L t       ) goto L, common return point; deal with any ret value on stack
	(kjumpretm,    $+1, 1, 0),  ! (a - 0) (L t n     ) goto L, common return point; deal with any ret value on stack

	(ksetcc,       $+1, 1, 0),  ! (2 - 1) (t c       ) Z' := Y cc Z

	(kstop,        $+1, 0, 0),  ! (1 - 0) (          ) Stop Z

	(kto,          $+1, 1, 0),  ! (0 - 0) (L t       ) --B (aux); goto L when B<>0 
	(kforup,       $+1, 1, 1),  ! (0 - 0) (L t n     ) B:=n; goto L when B<=C
	(kfordown,     $+1, 1, 1),  ! (0 - 0) (L t n     ) B-:=n; goto L when B>=C

	(kiswap,       $+1, 1, 0),  ! (2 - 0) (t         ) swap(Y^,Z^)

	(kswitch,      $+1, 0, 2),  ! (1 - 0) (L x y     ) L=jumptab; B=elselab; x/y=min/max values
	(kswitchu,     $+1, 0, 2),  ! (1 - 0) (L x y     ) L=jumptab; B=elselab; x/y=min/max values
	(kswlabel,     $+1, 0, 0),  ! (0 - 0) (L         ) jumptable entry
	(kendsw,       $+1, 0, 0),  ! (0 - 0) (          ) Mark end of switch jumptable

	(kclear,       $+1, 1, 0),  ! (1 - 0) (t         ) Clear Z^

	(kassem,       $+1, 0, 1),  ! (0 - 0) (x         ) To be worked out....

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
	(kmin,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := min(Y, Z)
	(kmax,         $+1, 1, 0),  ! (2 - 1) (t         ) Z' := max(Y, Z)
	(kaddpx,       $+1, 1, 2),  ! (2 - 1) (t s d     ) Z' := Y + Z*s + d
	(ksubpx,       $+1, 1, 2),  ! (2 - 1) (t s d     ) Z' := Y - Z*s + s
	(ksubp,        $+1, 1, 1),  ! (2 - 1) (t s       ) Z' := (Y - Z)/s

	(kneg,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := -Z
	(kabs,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := abs Z
	(kbitnot,      $+1, 1, 0),  ! (1 - 1) (t         ) Z' := inot Z
	(knot,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := not Z
	(ktoboolt,     $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := istrue Z; u is of type u; result is type t
	(ktoboolf,     $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := not istrue Z
	(ksqr,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := sqr Z

	(ksqrt,        $+1, 1, 0),  ! (1 - 1) (t         ) Z' := sqrt Z
	(ksin,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := sin Z
	(kcos,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := cos Z
	(ktan,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := tan Z
	(kasin,        $+1, 1, 0),  ! (1 - 1) (t         ) Z' := asin Z
	(kacos,        $+1, 1, 0),  ! (1 - 1) (t         ) Z' := acos Z
	(katan,        $+1, 1, 0),  ! (1 - 1) (t         ) Z' := atan Z
	(klog,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := log Z
	(klog10,       $+1, 1, 0),  ! (1 - 1) (t         ) Z' := log10 Z
	(kexp,         $+1, 1, 0),  ! (1 - 1) (t         ) Z' := round Z
	(kround,       $+1, 1, 0),  ! (1 - 1) (t         ) Z' := round Z
	(kfloor,       $+1, 1, 0),  ! (1 - 1) (t         ) Z' := floor Z
	(kceil,        $+1, 1, 0),  ! (1 - 1) (t         ) Z' := ceil Z
	(ksign,        $+1, 1, 0),  ! (1 - 1) (t         ) Z' := sign Z

	(katan2,       $+1, 1, 0),  ! (2 - 1) (t         ) Z' := atan2(Y, Z)
	(kpower,       $+1, 1, 0),  ! (2 - 1) (t         ) Z' := Y ** Z
	(kfmod,        $+1, 1, 0),  ! (2 - 1) (t         ) Z' := fmod(Y, Z)

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
	(kminto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ min:= Y
	(kmaxto,       $+1, 1, 0),  ! (2 - 0) (t         ) Z^ max:= Y
	(kaddpxto,     $+1, 1, 1),  ! (2 - 0) (t s       ) Z^ +:= Y*s
	(ksubpxto,     $+1, 1, 1),  ! (2 - 0) (t s       ) Z^ -:= Y*s

	(knegto,       $+1, 1, 0),  ! (1 - 0) (t         ) -:= Z^
	(kabsto,       $+1, 1, 0),  ! (1 - 0) (t         ) abs:= Z^
	(kbitnotto,    $+1, 1, 0),  ! (1 - 0) (t         ) inot-:= Z^
	(knotto,       $+1, 1, 0),  ! (1 - 0) (t         ) not:= Z^
	(ktoboolto,    $+1, 1, 0),  ! (1 - 0) (t         ) istrue:= Z^

	(ktypepun,     $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := t(u@(Z^))
	(kfloat,       $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Int   to real t
	(kfix,         $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Real   to int t
	(ktruncate,    $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,u) Mask to width of u, but type is widened to t
	(kwiden,       $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Mask to width of u, but type is widened to t
	(kfwiden,      $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r32 to r64
	(kfnarrow,     $+1, 2, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r64 to r32

	(kstartmx,     $+1, 0, 1),  ! (0 - 0) (          ) -
	(kresetmx,     $+1, 1, 1),  ! (0 - 0) (t         ) -
	(kendmx,       $+1, 1, 1),  ! (0 - 0) (t         ) -

	(kdefproc,     $+1, 0, 0),  ! (0 - 0) (M         ) ?
	(ktcproc,      $+1, 0, 0),  ! (0 - 0) (M         ) ?
	(kendproc,     $+1, 0, 0),  ! (0 - 0) (          ) ?
	(kistatic,     $+1, 1, 0),  ! (0 - 0) (M t       ) Define idata label (must be followed by correct DATA ops)
	(kzstatic,     $+1, 1, 0),  ! (0 - 0) (M t       ) Define zdata label and reserve sufficient space
	(kdata,        $+1, 1, 0),  ! (0 - 0) (M L C t   ) Constant data. For block types, there can be multiple C values

	(klabel,       $+1, 0, 0),  ! (0 - 0) (          ) ?
	(klabeldef,    $+1, 0, 0),  ! (0 - 0) (          ) ?
	(ksetjmp,      $+1, 0, 0),  ! (1 - 0) (          ) For C
	(klongjmp,     $+1, 0, 0),  ! (1 - 1) (          ) For C

	(ksetcall,     $+1, 0, 1),  ! (0 - 0) (n         ) ?
	(ksetarg,      $+1, 0, 1),  ! (0 - 0) (n         ) ?
	(kloadall,     $+1, 0, 0),  ! (0 - 0) (          ) ?

	(keval,        $+1, 0, 0),  ! (1 - 0) (          ) Evaluate Z [load to an actual register], then pop
	(kcomment,     $+1, 0, 0),  ! (0 - 0) (C         ) Comment C (a string)
	(kendprog,     $+1, 0, 0),  ! (0 - 0) (          ) End-of-program marker.
!------------------------- -
	(kstoresl,     $+1, 1, 0),  ! (2 - 0) (M t       ) M := slice(Y, Z)
	(kstoresld,    $+1, 1, 0),  ! (2 - 1) (M t       ) M := slice(Y, Z); leave Y on stack
	(ksliceupb,    $+1, 1, 0),  ! (2 - 1) (M t       ) Z' := Z.upb
	(kslicelen,    $+1, 1, 0),  ! (2 - 1) (M t       ) Z' := Z.len
	(ksliceptr,    $+1, 1, 0),  ! (1 - 1) (t         ) Z' := Z.sliceptr
end

global const kerror = knop

export enumdata [0:]ichar ccnames =
	(no_cc=0,	"xx"),
	(eq_cc,		"eq"),
	(ne_cc,		"ne"),
	(lt_cc,		"lt"),
	(le_cc,		"le"),
	(ge_cc,		"ge"),
	(gt_cc,		"gt"),
end

export enumdata [0:]ichar idnames
	(Null_id=0,		"--"),			!Not set (used for overall program name)
	(import_id,		"Import"),		!Imported symbol (proc or static)
	(proc_id,		"Proc"),		!Local proc
	(static_id,		"Static"),		!Local static
	(local_id,		"Local"),		!Function local var
	(param_id,		"Param"),		!Function param
	(label_id,		"Label"),		!Used in assembly
	(export_id,		"Export"),		!Used by makesymbol, is converted to proc_id/.exported
end

=== mc_genmcl.m 0 0 9/41 ===
!const fshowpcl=1
!const fshowopndstack=1
const fshowpcl=0
const fshowopndstack=0

GLOBAL INT DEBUG

int framebytes
int pxoffset			!set by iloadx/istorex when a constant

[pclnames.bounds]ref proc(pcl) px_handlertable

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

!GLOBAL INT PPSEQNO

global proc genmcl=

	return when mcldone

	IF FSHOWPCL OR FSHOWOPNDSTACK THEN CPL "********* ASM HAS PCL INFO *********" FI

	inithandlers()
	mclinit()

!	if mcldone then merror("genmcl?") fi

	currpcl:=pcstart

	int i:=0
	repeat
		convertpcl(currpcl)

		showopndstack() when fshowopndstack and currpcl.opcode not in [klabel, kcomment, kdefproc, ktcproc, kretproc, kendproc]

		++currpcl

	until currpcl>pccurr or currpcl.opcode=kendprog

	genrealtable()
	genabsneg()
	genstringtable()

!	genmc(m_nop)
	mcldone:=1
end

proc convertpcl(pcl p)=

!RETURN WHEN P.OPCODE IN [KCOMMENT]
!CPL "CONV",PCLNAMES[P.OPCODE],debug

	doshowpcl(p) when fshowpcl
!	doshowpcl(p)

	pmode:=p.mode
	currpcl:=p
	mmpos:=p.pos

	ppseqno:=p.seqno

	px_handlertable[p.opcode]^(p)

	clear regset
	clear xregset

!Then set the regs still in use as pcl opnds:
	int reg

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

proc inithandlers=
	static byte initdone=0
	ichar name, s
	int n

	if initdone then return fi

n:=0x123456
n:=123456
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
		(ktoboolf, 		ktoboolt)

		(kcallf,		kcallp)
		(kicallp,		kcallp)
		(kicallf,		kcallp)

		(kendmx,		kresetmx)
		(ktcproc,		kdefproc)

		(ktcproc,		kdefproc)

		(kidivto,		kidiv)
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

	return unless fshowpcl

	case p.opcode
	when kdefproc, ktcproc, kretproc, kendproc, kistatic, kzstatic, kdata then
	else
		strcpy(&.str,"                       ")
		strcat(&.str,strpclstr(p))
!		mgencomment(&.str)
		mgencomment(PCM_COPYHEAPSTRING(&.str))
!		mgencomment("<PCL>")
	esac
end

proc unimpl(pcl p)=
	[100]char str
	fprint @str, "Unimpl: # (#)", pclnames[p.opcode], strpmode(pmode)
CPL STR
	mgencomment(pcm_copyheapstring(str))
!	merror("PCL op not ready:", pclnames[p.opcode])
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

proc px_opnd*(pcl p) =
! Define auxiliary operand M or L
	unimpl(p)
end

proc px_type*(pcl p) =
! Define auxiliary type t
	unimpl(p)
end

proc px_comment*(pcl p) =
! Comment C (a string)
!	unimpl(p)
end

proc px_defproc*(pcl p) =
! ?
!Things that are remembered:

!PCLPROCDEF:	PCL op for kdefprocdef: used to repeat PASS2 pass for optimising
!				Note will normally skip back to following op, as below is for PASS1 only

!MCLPROCENTRY:	MCL op for dummy op (or anything that will work), used to insert
!				proc entry ops during do_procentry()

!	passno:=1
!
	currfunc:=p.def
	nblocktemps:=0

	setsegment('C',1)

!CPL "FUNC:",CURRFUNC.NAME, =CURRFUNC, =CURRFUNC.NPARAMS, =CURRFUNC.NLOCALS

	genmc(m_procstart,mgenmemaddr(currfunc))
	genmc(m_labelname,mgenmemaddr(currfunc))

	initpass(currfunc)

!create dummy mcl op at which to insert hang proc-entry code onto later
	mgencomment(">>")
	mclprocentry:=mccodex

!CPL "DEFPROC", CURRFUNC.NAME, CURRFUNC.NPARAMS
	if currfunc.nparams=2 and eqstring(currfunc.name, "main") then
		fixmain()
	fi

end

proc px_endproc*(pcl p) =
! ?

	if noperands then

cpl("PCL STACK NOT EMPTY"), CURRFUNC.NAME
!MCOMM("PCL STACK NOT EMPTY")
!		merror("PCL stack not empty")
	fi

	genmc(m_procend)
!	checkopnds()

	if fppeephole then
!		peephole(mclprocentry)
	fi
end

proc px_endprog*(pcl p) =
! End-of-program marker.
	unimpl(p)
end

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
	when real32_opnd then
		ax:=mgenrealimm(p.xvalue, tpr32)

	when string_opnd then
		ax:=mgenlabel(getstringindex(p.svalue))

	when memaddr_opnd then
		ax:=mgenmemaddr(p.def)
		ax.offset:=p.extra
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

!CPL "STORE LOAD", STRPMODE(P.MODE)
	bx:=loadopnd(zz, p.mode)

	if p.mode<>tpblock then
		ax:=mgenmem(p.def, p.mode)
		genmc(m_mov, ax, bx)

	else
		ax:=getworkregm(tpu64)
		genmc(m_lea, ax, mgenmem(p.def, tpu64))
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
	bx:=getopnd(zz, p.mode)
	genmc((ispfloat(p.mode)|m_addss+ispwide(p.mode)|m_add), ax, bx)

	poppcl()
end

proc px_sub*(pcl p) =
! Z' := Y - Z
	mclopnd ax, bx

	ax:=loadopnd(yy, p.mode)
	bx:=getopnd(zz, p.mode)
	genmc((ispfloat(p.mode)|m_subss+ispwide(p.mode)|m_sub), ax, bx)

	poppcl()
end

proc px_mul*(pcl p) =
! Z' := Y * Z
	mclopnd ax, bx

	ax:=loadopnd(yy, p.mode)
	bx:=getopnd(zz, p.mode)
	genmc((ispfloat(p.mode)|m_mulss+ispwide(p.mode)|m_imul2), ax, bx)

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

	bx:=getopnd(zz, p.mode2)
	if bx.mode=a_mem then
		ax:=getworkregm(pmode)
	else
		bx:=loadopnd(zz, p.mode2)
		ax:=mgenregi(bx.reg, pmode)

	fi

	genmc((psigned[p.mode2]|m_movsx|m_movzx), ax, bx)
	setnewzz(ax.reg, pmode)

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

proc px_abs*(pcl p) =
! Z' := abs Z
	mclopnd ax,lx

	ax:=loadopnd(zz, pmode)

	if ispint(pmode) then
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_labelx, lx)

	else
		do_absreal(ax, pmode)
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

proc px_sqr*(pcl p) =
! Z' := sqr Z
	mclopnd ax

	ax:=loadopnd(zz, pmode)

	if ispint(pmode) then
		genmc(m_imul2,ax,ax)
	else
		genmc(m_mulss+ispwide(pmode),ax,ax)
	fi
end

proc px_sqrt*(pcl p) =
! Z' := sqrt Z
	mclopnd ax

	ax:=loadopnd(zz, pmode)
	genmc(m_sqrtss+ispwide(pmode),ax,ax)
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
		bx:=getopnd(zz, pmode)

		if ispint(pmode) then
			if psigned[pmode] then
				mcond:=scondcodes[p.condcode]
			fi
			genmc(m_cmp, ax, bx)

		else
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
		genmc(m_mov, mgenreg(r0), mgenmem(blockretname))
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

!	if p.x>4 then
	if n>4 then
!IF PMODE=TPBLOCK THEN CPL "PUSH HIGH/BLOCK", P.SIZE FI
		if pmode=tpblock then
			copyblockarg(nil, p.size, n)
		fi
		pushopnd(zz, pmode)
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

	highargs max:=nargs

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

!CPL =NREGARGS, =BLOCKRET

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
!		loadretval(zz, pmode, r0)
		loadparam(zz, pmode, r0)
		poppcl()
	fi

	px_jump(p)
end

proc px_jumpretm*(pcl p) =
! goto L, common return point; deal with any ret value on stack
	int n

!	CPL =P.NARGS
	for i to p.nargs do
		n:=noperands-i+1
		loadparam(n, pclmode[n], r0+i-1)
	od

	px_jump(p)
end

proc px_startmx*(pcl p) =
! -
	saveopnds()
end

proc px_resetmx*(pcl p) =
! -
!	if ispfloat(pmode) then
!		merror("RESETMULT/XREG")
!	fi

	movetoreg(r0)

	if p.opcode=kresetmx then
		poppcl()
	fi
end

proc px_stop*(pcl p) =
! Stop Z
	psymbol d

	loadparam(zz,tpu64, r10)

	d:=findnamesym("exit")
	if not d then
		d:=pc_makesymbol("exit")
		d.imported:=1
		addnamesym(d)
	fi

	genmc(m_call, mgenmemaddr(d))
	localshadow:=1
	highargs max:=1

	poppcl()
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


proc px_forup*(pcl p) =
! B:=n; goto L when B<=C
	do_for(p, m_inc, m_add, le_cond)
end

proc px_fordown*(pcl p) =
! B-:=n; goto L when B>=C
	do_for(p, m_dec, m_sub, ge_cond)
end

proc px_iload*(pcl p) =
! Z' := Z^
	mclopnd ax, px
	pcl nextpcl

!CPL "ILOAD", STRPMODE(PMODE)

	if pmode<>tpblock then
		px:=getopnd_ind(zz, pmode)
!CPL =MSTROPND(PX)
		if p.opcode<>kiload then
			px:=applyoffset(px, pxoffset)
		fi

		nextpcl:=currpcl+1

		if nextpcl.opcode=kwiden then
			ax:=getworkreg_rm(0, nextpcl.mode)
			genmc(ploadop[nextpcl.mode2], ax, px)
			setnewzz(ax.reg, nextpcl.mode)
			currpcl:=nextpcl
		else
			ax:=getworkreg_rm(0, pmode)
			genmc(m_mov, ax, px)
			setnewzz(ax.reg, pmode)
		fi

	else

		px:=getopnd_ind_simp(zz, pmode)


		if p.opcode<>kiload then
			px:=applyoffset(px, pxoffset)
		fi

		ax:=getworkreg_rm(px.reg, tpu64)
		dolea(ax, px)
	fi

end

proc px_iloadx*(pcl p) =
! Z' := (Y + Z*s + d)^
	pcl z, nextpcl
	mclopnd ax, bx, px, fx

	if z:=isimmload(zz) then
		pxoffset:=z.value*p.scale+p.extra
		poppcl()
		px_iload(p)
		return
	fi

	px:=do_addrmode(p)

	if pmode=tpblock then
		ax:=getworkreg_rm(px.reg, tpu64)
		dolea(ax, px)
		poppcl()
		setnewzz(ax.reg, tpu64)

	else
		nextpcl:=currpcl+1

		if nextpcl.opcode=kwiden then
			ax:=getworkreg_rm(0, nextpcl.mode)
			genmc(ploadop[nextpcl.mode2], ax, px)
			poppcl()
			setnewzz(ax.reg, nextpcl.mode)
			currpcl:=nextpcl
		else
			ax:=getworkreg_rm(0, pmode)
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
	if p.opcode<>kistore then
		px:=applyoffset(px, pxoffset)
	fi

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

	if z:=isimmload(zz) then
		pxoffset:=z.value*p.scale+p.extra
		poppcl()
		px_istore(p)
		return
	fi

	cx:=loadopnd(xx, pmode)			!rhs
	px:=do_addrmode(p)

	if pmode=tpblock then
		px:=makesimpleaddr(px)
		cx:=makeopndind(cx, tpu64)
		copyblock(px, cx, p.size)
		copyblock(px, cx, p.size)

	else
		genmc(m_mov, px, cx)

	fi

	poppcl()
	poppcl()
	poppcl()
end

proc px_addpx*(pcl p) =
! Z' := Y + Z*s + d
	mclopnd ax,cx

!P.MODE:=PMODE:=TPU64

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

proc px_to*(pcl p) =
! --B (aux); goto L when B<>0 
	pcl q
	mclopnd ax

	q:=currpcl:=p+1

	ax:=mgenmem(q.def)
	genmc(m_dec, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.labelno))
end

proc px_iswap*(pcl p) =
! swap(Y^,Z^) ref T/V
	mclopnd ax, bx

	mclopnd px:=getopnd_ind(yy, pmode)
	mclopnd qx:=getopnd_ind(zz, pmode)

	ax:=getworkregm(pmode)
	bx:=getworkregm(pmode)

	if pmode<>tpblock then
		genmc(m_mov, ax, px)
		genmc(m_mov, bx, qx)
		genmc(m_mov, qx, ax)
		genmc(m_mov, px, bx)
	else
		merror("swap/block")
	fi

	poppcl()
	poppcl()
end

proc px_swapstk*(pcl p) =
! (Z', Y') := (Z, Y)
!	swapopnds(yy,zz)
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
!CPL =STROPNDSTACK()
!CPL =STRPMODE(PMODE)

	swapopnds(yy,zz)
	poppcl()


	setnewzz(ax.reg, pmode)
!!CPL =STROPNDSTACK()
!	poppcl()
end

proc px_float*(pcl p) =
! Z' := cast(Z,t) Int u to real t
	mclopnd ax,fx
	int lab,lab2
	byte pmode2:=p.mode2

!CPL "FLOAT", =STRMODE(P.MODE),=STRMODE(P.MODE2),P

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

proc px_clear*(pcl p) =
! Clear Z^
	mclopnd ax

	ax:=getopnd_ind_simp(zz, tpu64)

	clearblock(ax, p.size)
	poppcl()
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
	mclopnd ax, bx

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=loadopnd(zz, pmode)
	if minlab<>0 then
		genmc(m_sub, ax, mgenint(minlab))
	fi
	genmc(m_cmp,ax,mgenint(maxlab-minlab+1))
	genmc_cond(m_jmpcc,geu_cond,mgenlabel(elselab))

	if phighmem then
		reg:=getworkireg()
		bx:=mgenreg(reg, pmode)

!		genmc(m_mov, bx, mgenlabel(jumplab))
		genmc(m_lea, bx, mgenlabelmem(jumplab))

		genmc(m_jmp, mgenindex(ireg:ax.reg, areg:reg, scale:8))
	else
		genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))
	fi

	poppcl()

	setsegment('I')
end

proc px_switchu*(pcl p) =
! L=jumptab; B=elselab; x/y=min/max values
	int minlab, maxlab, jumplab, elselab, reg
	mclopnd ax, bx

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=loadopnd(zz, pmode)

	if phighmem then
		reg:=getworkireg()
		bx:=mgenreg(reg, pmode)

!		genmc(m_mov, bx, mgenlabel(jumplab))
		genmc(m_lea, bx, mgenlabelmem(jumplab))

		genmc(m_jmp, mgenindex(ireg:ax.reg, areg:reg, scale:8, offset:-minlab*8))
	else
		genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab, offset:-minlab*8))
	fi

	poppcl()
end

proc px_swlabel*(pcl p) =
! jumptable entry
	genmc(m_dq, mgenlabel(p.labelno))
end

proc px_endsw*(pcl p) =
! Mark end of switch jumptable
	setsegment('C')
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

proc px_typepun*(pcl p) =
! Z' := t(u!(Z^))
	mclopnd ax,bx

	bx:=loadopnd(zz, p.mode2)
	ax:=getworkregm(pmode)
	genmc(m_mov, ax, changeopndsize(bx, ax.size))

	setnewzz(ax.reg, pmode)
end

proc px_unload*(pcl p) =
! Pop stack
	poppcl()
!	unimpl(p)
end

proc px_loadbit*(pcl p) =
! Z' := Y.[Z]
	mclopnd ax
	pcl z
	int i, m

	if z:=isimmload(zz) then
		i:=z.value
		m:=(i in 0..31|tpu32|tpu64)

		ax:=loadopnd(yy, m)
		if i then
			genmc(m_shr, ax, mgenint(i, m))

			goto skip when i=63

		fi
	else
		ax:=loadopnd(yy, pmode)
		genmc(m_push, mgenreg(r10)) when r10used
		genmc(m_shr, ax, loadparam(zz, tpu8, r10))
		genmc(m_pop, mgenreg(r10)) when r10used
	fi

	genmc(m_andx, changeopndsize(ax,4), mgenint(1, tpu32))

skip:
	poppcl()
end

proc px_assem*(pcl p) =
! To be worked out....
	if idomcl_assem then
		idomcl_assem(p.asmcode)
	else
		merror("No Assem handler")
	fi
end


proc px_sin*(pcl p) =
! Z' := sin Z
	do_maths(p,"sin*")
end

proc px_cos*(pcl p) =
! Z' := cos Z
	do_maths(p,"cos*")
end

proc px_tan*(pcl p) =
! Z' := tan Z
	do_maths(p,"tan*")
end

proc px_asin*(pcl p) =
! Z' := asin Z
	do_maths(p,"asin*")
end

proc px_acos*(pcl p) =
! Z' := acos Z
	do_maths(p,"acos*")
end

proc px_atan*(pcl p) =
! Z' := atan Z
	do_maths(p,"atan*")
end

proc px_log*(pcl p) =
! Z' := log Z
	do_maths(p,"log*")
end

proc px_log10*(pcl p) =
! Z' := log10 Z
	do_maths(p,"log10*")
end

proc px_exp*(pcl p) =
! Z' := round Z
	do_maths(p,"exp*")
end

proc px_round*(pcl p) =
! Z' := round Z
	do_maths(p,"round*")
end

proc px_floor*(pcl p) =
! Z' := floor Z
	do_maths(p,"floor*")
end

proc px_ceil*(pcl p) =
! Z' := ceil Z
	do_maths(p,"ceil*")
end

proc px_atan2*(pcl p) =
! Z' := atan2(Y, Z)
	swapopnds(yy,zz)
	do_maths(p,"atan2*", 2)
end

proc px_fmod*(pcl p) =
! Z' := fmod(Y, Z)
	swapopnds(yy,zz)
	do_maths(p,"fmod*", 2)
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

proc px_min*(pcl p) =
! Z' := min(Y, Z)
	if ispint(pmode) then
		do_max_int((psigned[pmode]|gt_cond|gtu_cond))
	else
		do_max_float(m_minss+ispwide(pmode))
	fi
end

proc px_max*(pcl p) =
! Z' := max(Y, Z)
	if ispint(pmode) then
		do_max_int((psigned[pmode]|lt_cond|ltu_cond))
	else
		do_max_float(m_maxss+ispwide(pmode))
	fi
end

proc px_power*(pcl p) =
! Z' := Y ** Z
	mclopnd ax,bx
	psymbol d

	if ispint(pmode) then
		d:=gethostfn(kpower)
		swapopnds(yy,zz)
		do_host(p, d, 2)
	else
		swapopnds(yy,zz)
		do_maths(p,"pow*",2)
	fi
end
!
proc px_minto*(pcl p) =
! Z^ min:= Y
	if ispint(pmode) then
		do_maxto_int((psigned[pmode]|le_cond|leu_cond), pmode)
	else
		do_maxto_real(leu_cond, pmode)
	fi
end

proc px_maxto*(pcl p) =
! Z^ max:= Y
	if ispint(pmode) then
		do_maxto_int((psigned[pmode]|ge_cond|geu_cond), pmode)
	else
		do_maxto_real(geu_cond, pmode)
	fi
end

proc px_negto*(pcl p) =
! -:= Z^
	mclopnd px, fx

	px:=getopnd_ind(zz, pmode)

	if ispint(pmode) then
		genmc(m_neg, px)
	else
		fx:=getworkregm(pmode)
		genmc(m_mov, fx, px)

		do_negreal(fx, pmode)

		genmc(m_mov, px, fx)
	fi
	poppcl()
end

proc px_absto*(pcl p) =
! abs:= Z^
	mclopnd px, ax, lx

	px:=getopnd_ind(zz, pmode)

	ax:=getworkregm(pmode)
	genmc(m_mov, ax, px)

	if ispint(pmode) then
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg, ax)
		genmc(m_labelx, lx)

	else
		do_absreal(ax, pmode)
	fi

	genmc(m_mov, px, ax)

	poppcl()
end

proc px_addpxto*(pcl p) =
! Z^ +:= Y
	mclopnd ax,bx
	pcl z
!
	ax:=getopnd_ind(zz, pmode)

	if z:=isimmload(yy) then
!		genmc(m_add, ax, mgenint(z.value*p.scale+p.extra))
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

proc px_bitnotto*(pcl p) =
! inot-:= Z^
	mclopnd px, fx

	px:=getopnd_ind(zz, pmode)

	genmc(m_notx, px)
	poppcl()
end

proc px_notto*(pcl p) =
! not:= Z^
	mclopnd px
	px:=getopnd_ind(zz, tpu8)
	genmc(m_xorx, changeopndsize(px,1), mgenint(1, tpu8))
end

proc px_toboolto*(pcl p) =
! istrue:= Z^
	mclopnd ax, bx, px

	px:=getopnd_ind(zz, tpu8)
	ax:=getworkregM(pmode)
	genmc(m_mov, ax, px)
	genmc(m_test, ax,ax)
	genmc_cond(m_setcc, ne_cond, bx:=changeopndsize(ax,1))
	genmc(m_movzx, changeopndsize(ax,4),bx)
	genmc(m_mov, px, ax)
end

proc px_sign*(pcl p) =
! Z' := sign Z
	mclopnd ax, bx
	mclopnd lx1, lx2, lx3
	byte gtop, ltop


	ax:=loadopnd(zz, pmode)

	bx:=getworkregm(tpi64)
		genmc(m_xorx, bx, bx)

	if ispfloat(pmode) then
		gtop:=gtu_cond; ltop:=ltu_cond
		genmc(m_comiss+ispwide(pmode), ax, mgenrealmem(0.0, pmode))
	else
		gtop:=gt_cond; ltop:=lt_cond
		genmc(m_cmp, ax, mgenint(0,pmode))
	fi

		genmc_cond(m_jmpcc, gtop, lx1:=mgenlabel(++mlabelno))
		genmc_cond(m_jmpcc, ltop, lx2:=mgenlabel(++mlabelno))
		genmc(m_jmp, lx3:=mgenlabel(++mlabelno))

	genmc(m_labelx, lx1)
		genmc(m_mov, changeopndsize(bx, 1), mgenint(1))
		genmc(m_jmp, lx3)
	genmc(m_labelx, lx2)
		genmc(m_dec, bx)
	genmc(m_labelx, lx3)

	setnewzz(bx.reg, tpi64)

end

proc px_loadbf*(pcl p) =
! Z' := X.[Y..Z]
	pcl y, z

	y:=isimmload(yy)
	z:=isimmload(zz)

	if y and z then
		do_loadbf_const(p, y.value, z.value)
	else
		do_loadbf_var(p)
	fi
end

proc px_storebit*(pcl p) =
! Y^.[Z] := X
	do_storebit(p)
end

proc px_storebf*(pcl p) =
! X^.[Y..Z] := W
	do_storebf(p)
end

proc px_slicelen*(pcl p) =
! Z' := Z.len

	mclopnd ax, bx

!assume slice (pmode=block)

	bx:=getopnd_ind(zz, tpi64)

	pushpcl_reg(tpi64)
	ax:=getopnd(zz, tpi64)

	genmc(m_mov, ax, applyoffset(bx, 8))

	if p.opcode=ksliceupb and p.slicelwb<>1 then
		genmc(m_add, ax, mgenint(p.slicelwb-1))
	fi

	swapopnds(yy,zz)
	poppcl()

end

proc px_sliceptr*(pcl p) =
! Z' := Z.sliceptr
	mclopnd ax, bx

	bx:=getopnd_ind(zz, tpi64)
	pushpcl_reg(tpi64)
	ax:=getopnd(zz, tpi64)

	genmc(m_mov, ax, bx)

	swapopnds(yy,zz)
	poppcl()
end

proc px_loadall*(pcl p) =
	checkallloaded()
end

proc px_setjmp*(pcl p)=
	mclopnd ax,bx
	int lab:=mcreatefwdlabel()

	bx:=getopnd_ind(zz, tpref)

!	addreg_d64()
	pushpcl_reg(tpref)

	ax:=getopnd(zz, tpref)
	genmc(m_mov, ax, mgenlabel(lab))
	genmc(m_mov, bx, ax)
	genmc(m_mov, applyoffset(bx,8), dstackopnd)
	genmc(m_mov, applyoffset(bx,16), dframeopnd)
	swapopnds(yy,zz)
	poppcl()
	genmc(m_xorx, ax, ax)

!since this is the of this op anway, free any workregs in advance (freeing
!will be done again by convertpcl)
	freeworkregs(p)
	movetoreg(r0)
	mdefinefwdlabel(lab)

end

proc px_longjmp*(pcl p)=
	mclopnd ax,bx,cx

!CPL "LONG JUMP"

!	checkallloaded()
!MCOMM("LONGJ")

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

!CPL STROPNDSTACK()
!CPL =PCLREG[YY]
!CPL =PCLREG[ZZ]
!freeworkregs(p)
!CPL STROPNDSTACK()
!CPL =PCLREG[ZZ], LOCNAMES[PCLLOC[ZZ]]

	swapopndregs(r0)			!move ret value to r0
	genmc(m_jmp, cx)			!
	swapopnds(yy, zz)
	poppcl()					!get rid of dest addr; leave ret value in r0
end
=== mc_auxmcl.m 0 0 10/41 ===
!Auxially routines called by genmcl's PX handlers

global proc initpass(psymbol d)=
!initialise genmcl pass through proc pcl code
	psymbol e

	clear regset
	clear xregset

	clear spillregs
	clear spillxregs

	clear pcltempflags
	r10used:=r11used:=r13used:=0

	mstackdepth:=0
	noperands:=0

	frameoffset:=paramoffset:=framebytes:=0
!	nlocals:=nparams:=0
	usedparams:=usedxparams:=0
	nproccalls:=highargs:=0
	localshadow:=0
	assemused:=0
	highreg:=highxreg:=0

	clear pcltempflags

	if d.mode=tpblock then
		e:=pc_makesymbol("$1x")
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
	int retmode, ntemps, hasequiv, offset, size
	mclopnd ax
	psymbol d
	[100]char str, newname

!	initpass1x(pclprocdef)
!
!CPL "DOPROCENTRY"

	SETMCLENTRY(MCLPROCENTRY)

	bspill:=bxspill:=bxspilloffset:=0
	if highreg>=r3 then bspill:=highreg-r2 fi		!no of regs d3..highreg
	if highxreg>=r6 then bxspill:=highxreg-r5 fi	!no of xregs x6..highxreg

	d:=currfunc.nextparam
	while d, d:=d.nextparam do
		if not d.reg then			!not a regvar
			d.offset:=paramoffset+16+bspill*8
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		fi
		paramoffset+:=8
	od

	retmode:=currfunc.mode

	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do
		size:=psize[d.mode]
		if d.mode=tpblock then
			size:=d.size
		fi

		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(size)
			d.offset:=frameoffset
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		fi
	od

	ntemps:=0
	for i to maxoperands when pcltempflags[i] do
		++ntemps
		frameoffset-:=8
		ax:=pcltempopnds[i]
		ax.offset:=frameoffset
		genmc(m_define, mgenname(gettempname(currfunc,i)), mgenint(ax.offset))
	od

	if bxspill then
		frameoffset-:=bxspill*8
		bxspilloffset:=frameoffset
	fi

	if currfunc.isthreaded then
		if currfunc.nlocals or currfunc.nparams then merror("Threaded proc has locals/params") fi
		if ntemps then merror("Threaded proc has temps") fi
		if bspill or bxspill then merror("Threaded proc has spill regs") fi
		resetmclentry()
		return
	fi

	framebytes:=-frameoffset

	if bspill.odd then				!need an even number to keep stack alighnment correct
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
		for r:=r3 to highreg do
			genmc(m_push, mgenreg(r))
		od
	fi

	if framebytes or currfunc.nparams then
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
		pushstack(framebytes)
	fi

	spillparams()

	if bxspill then
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, ax, mgenxreg(r))
		od
	fi
	MCOMM("---------------")
	RESETMCLENTRY()
end

global proc do_procexit=
	mclopnd ax
	int offset

	MCOMM("---------------")
	if currfunc.isthreaded then
		genmc(m_ret)				!in case no jump out exists
		return
	fi

	if bxspill then
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, mgenxreg(r), ax)
		od
	fi

	if framebytes or currfunc.nparams then
		popstack(framebytes)
		genmc(m_pop, dframeopnd)
	fi

	if bspill then
		for r:=highreg downto r3 do
			genmc(m_pop, mgenreg(r))
		od
	fi

	genmc(m_ret)
end

proc spillparams=
	psymbol d
	mclopnd ax
	int offset:=16, regoffset:=0, firstoffset

	regoffset:=0

	d:=currfunc.nextparam

!CPL "SPILLPARAMS", CURRFUNC.NAME, CURRFUNC.NPARAMS, =currFUNC.VARIADIC
!IF CURRFUNC.NPARAMS THEN
!	CPL =CURRFUNC.NEXTPARAM.OFFSET
!FI
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
			if not d.reg then
				ax:=mgenindex(areg:rframe, size:8, offset:d.offset)
				case d.mode
				when tpr64 then
					genmc(m_movq, ax, mgenxreg(regoffset+xr0))
				when tpr32 then
					genmc(m_movd, changeopndsize(ax,4), mgenxreg(regoffset+xr0))
				else
					genmc(m_mov, ax, mgenreg(regoffset+r10))
				esac
			elsif d.reg and d.reg<=r9 then		!move from pregs to bregs
				case d.mode
				when tpr64 then
					genmc(m_movq, mgenxreg(d.reg), mgenxreg(regoffset+xr0))
				else
					genmc(m_mov, mgenreg(d.reg), mgenreg(regoffset+r10))
				esac
			fi
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
!	bx:=loadopnd(zz, pmode)

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
	mce_nextmcl:=p.nextmcl
end

func resetmclentry:ref mclrec pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.nextmcl:=mce_nextmcl
	pnew:=mccodex
	mccodex:=mce_oldmccodex
	pnew
end

global proc do_pushlowargs(int nargs, nvariadics=0, isptr=0)=
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
	mclopnd ax
	int j,k, nextireg, nextxreg, mode, imode, blockret
	psymbol dblock

	if nargs=0 then return fi

	blockret:=callblockret[ncalldepth]

	highargs max:=nargs

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

			mode:=pclmode[j]

			case mode
			when tpblock then
!CPL "PUSH LOW/BLOCK", CALLARGSIZE[NCALLDEPTH,K]
				ax:=loadparam(j, mode, nextireg)
				copyblockarg(ax, callargsize[ncalldepth,k], k)

			when tpr64, tpr32 then
				loadparam(j, mode, nextxreg)
				if nvariadics and k>=nvariadics then			!variadic floats go to both regs

!I need to move xmm reg to int reg
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

!MERROR("DOGETRETVAL")
!MCOMM("DOGETRETVAL")
	if (p+1).opcode=ktype then
		n:=0
		while (++p).opcode=ktype do
			modes[++n]:=p.mode
		od
		currpcl:=p-1

		for i:=n downto 1 do 
			m:=modes[i]
			pushpcl_reg(m, (ispfloat(m)|multxregs[i]|multregs[i]))
		od

	else
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

!CPL "LOADINCR", STRPMODE(PMODE)
!MCOMM("LDI1")
	mx:=getopnd_ind(zz, pmode)
!CPL =MSTROPND(MX), MX.SIZE, =STRPMODE(PMODE)
!MCOMM("LDI2")

!!	pushpcl_reg(getworkireg(), tpu64)
	pushpcl_reg(pmode)			!to hold loaded value
!MCOMM("LDI3")
	ax:=getopnd(zz, pmode)
!MCOMM("LDI4")

	genmc(m_mov, ax, mx)
!MCOMM("LDI5")

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	swapopnds(yy,zz)
	poppcl()
end

global proc do_for(pcl p, int incop, addop, cond)=
	pcl q,r
	mclopnd ax,bx,cx,dx,mx
	int reg

	q:=p+1
	r:=currpcl:=q+1

	mx:=mgenmem(q.def)

	if q.def.reg then
		if p.stepx=1 then
			genmc(incop, mx)
		else
			genmc(addop, mx, mgenint(p.stepx))
		fi
		ax:=mx
	else
		ax:=mgenreg(getworkireg())
		genmc(m_mov, ax,mx)
		if p.stepx=1 then
			genmc(incop, ax)
		else
			genmc(addop, ax, mgenint(p.stepx))
		fi
		genmc(m_mov, mx, ax)
	fi

	if r.opndtype=int_opnd then
		bx:=mgenint(r.value)
	else
		bx:=mgenmem(r.def)
	fi

	genmc(m_cmp, ax, bx)
!*!	freeireg(ax.reg)
!
	genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
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

	case n
	when 0 then
		genmc(m_xorx, ax,ax)
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
			genmc(m_imul2, ax, mgenint(n))
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

!Q:=NIL
	q:=isimmload(zz)
	if q then
		offset:=q.value*scale+extra	!for imm offset
	fi

	px:=nil

!	if isregvaropnd(yy) then
!		if isregvaropnd(zz) then			!regvar/regvar
!			reg:=pca.reg
!			regix:=scaleregvar(reg,scale,zz)
!			px:=mgenindex(areg:pcb.reg,ireg:regix, offset:extra, scale:scale)
!
!		elsif isimm64(zz) then			!regvar/imm
!			px:=mgenindex(areg:pcb.reg, offset:offset)
!		else							!regvar/any
!			scale:=scaleindex(bx:=loadopnd(zz),scale)
!			px:=mgenindex(areg:pcb.reg, ireg:bx.reg, scale:scale, offset:extra)
!		fi
!GOTO SKIP

	if ismemaddr(yy) then
		d:=pclopnd[yy].def
!CPL "ISMEMADDR------------", D.MODE, STRPMODE(d.MODE)
		if d.id=static_id and phighmem=2 or D.ID=PARAM_ID AND D.MODE=TPBLOCK  then skip fi
!		if d.id=static_id and phighmem=2 then skip fi
!		if isregvaropnd(zz) then			!memaddr/regvar
!			reg:=pca.reg
!			regix:=scaleregvar(reg,scale,zz)
!			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)
!
		if q then			!memaddr/imm
!SKIP
!CPL "AM1"
			px:=mgenindex(def:d, offset:offset)
		else							!memaddr/any
!CPL "AM2"
!SKIP
			scale:=scaleindex(bx:=loadopnd(zz, tpi64),scale)
			px:=mgenindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
skip:
		ax:=loadopnd(yy, tpu64)

!		if isregvaropnd(zz) then			!any/regvar
!			reg:=pca.reg
!			regix:=scaleregvar(reg,scale,zz)
!			px:=mgenindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)
		if q then						!any/imm	
			px:=mgenindex(areg:ax.reg, offset:offset)
		else
			scale:=scaleindex(bx:=loadopnd(zz, tpu64),scale)
			px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	fi

	px.size:=psize[p.mode]
!CPL "AMX", =PX.SIZE

	return px
end

global proc dolea(mclopnd ax, px)=
!do 'lea ax, px`, but suppress in cases like 'lea d0,[d0]'
!CPL "DOLEA", MSTROPND(AX), MSTROPND(PX)
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

!	if size=8 and ismemaddr(yy) then
!		ax:=mgenmem(pcb.def)
!		reg:=getnextreg()
!		rx:=mgenreg(reg)
!		genmc(m_mov, rx, ax)
!		bx:=getopnd(zz)
!		genmc(opc,rx,bx)
!		genmc(m_mov, ax,rx)
!		freeireg(reg)
!	else
		ax:=getopnd_ind(zz, p.mode)
!CPL =STRPMODE(P.MODE)
		bx:=loadopnd(yy, p.mode)

		genmc(opc,ax,bx)
!	fi
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

		ax:=loadopnd(locyy, pmode)
		fdivto:=1
!		swap(locyy, loczz)
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
		genmc(m_xorx, mgenreg(r11),mgenreg(r11))
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

global proc clearblock(mclopnd ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

	mclopnd rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg

	oddbytes:=n rem 8		!will be zero, or 1..7

	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=getworkregm(tpu64)
	genmc(m_xorx, rx, rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,8)

		to nwords do
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop

!SPLIT INTO xx VERSIONS:
! NWORDS IS A MULTIPLE OF 4, so can write 4 words at a time, in 1/4 of iterations
! Or do one word at a time like now.
! nword is a multiple of 4 happens when N is a multiple of 32 bytes, which will
! always be the case for power-of-two sizes of 32 bytes or more. 32/64 may already
! be done without a loop. So non-part-unrolled version only really for odd array or
! struct sizes, such as [100]char.

		if nwords iand 3 then		!not 4n

			rcount:=mgenreg(countreg:=getworkireg())
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)

			genmc(m_mov,rcount,mgenint(nwords))
			genmc(m_labelx,mgenlabel(lab))
			genmc(m_mov,ax,rx)

			genmc(m_add,mgenreg(ax.reg),mgenint(8))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
		else
			rcount:=mgenreg(countreg:=getworkireg())
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)
			genmc(m_mov,rcount,mgenint(nwords/4))
			genmc(m_labelx,mgenlabel(lab))

			for i to 4 do
				genmc(m_mov,applyoffset(ax,offset),rx)
				offset+:=8
			od

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize*4))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
		fi
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx,4)
			genmc(m_mov,applyoffset(ax,offset,4),rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx,2)
			genmc(m_mov,applyoffset(ax,offset,2),rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx,1)
			genmc(m_mov,applyoffset(ax,offset,1),rx)
		fi
	fi
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
!		rx:=getworkregm(tur64)
		rx:=getworkregm(tpr64)
!
		genmc(m_movdqu, rx, bx)
		genmc(m_movdqu, ax, rx)

		return
	fi

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=getworkregm(tpu64)		!work reg

	offset:=0

!CPL =MSTROPND(AX)
!CPL =MSTROPND(BX)

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
!	setsegment('C',8)

	if kk0used then
		genmc(m_labelx,mgenlabel(kk0used))
		gendb(0)
	fi

	p:=cstringlist
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))
		genstring_db(p.svalue,length:-1, strtype:0)
	od
end

global proc genstring_db(ichar s, int length=-1, strtype)=
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
!		if c<32 or c>=127 or c='\"' then
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
	genmc(m_db,mgenstring(s,length))
end

proc gendq(int a)=
	genmc(m_dq,mgenint(a))
end

global proc genrealtable=
	ref constrec p

	return unless creallist or creal32list

	mgencomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))

		if p.xvalue=infinity then
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		else
			genmc(m_dq, mgenrealimm(p.xvalue,tpr64))
		fi
	od

	mgencomment("Real32 Table")
	p:=creal32list
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))
		if p.xvalue=infinity then
			genmc(m_dd, mgenint(int@(real32(p.xvalue))))
		else
			genmc(m_dd, mgenrealimm(p.xvalue,tpr32))
		fi

	od
end

global proc genabsneg=
	if lababs32+lababs64+labneg32+labneg64 then
		setsegment('I',16)
	fi

	if lababs32 then
		mgencomment("lababs32")
		genmc(m_labelx,mgenlabel(lababs32))
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mgencomment("lababs64")
		genmc(m_labelx,mgenlabel(lababs64))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
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

global proc do_maths(pcl p, ichar opname, int nargs=1)=
	do_callrts(p, opname, nil, nargs)
end

global proc do_host(pcl p, psymbol d, int nargs=1)=
	do_callrts(p, nil, d, nargs)
end

global proc do_callrts(pcl p, ichar opname, psymbol d, int nargs)=
	int slots

	saveopnds(nargs)
	slots:=0

	if mstackdepth.odd then
		pushslots(1)
		slots:=1
	fi

	highargs max:=nargs
	do_pushlowargs(nargs)

	if mstackdepth then
		slots+:=4
		pushslots(4)					!shadowspace
	else
		localshadow:=1
	fi

	if opname then
		genmc(m_call, mgenextname(opname))
	else
		genmc(m_call, mgenmemaddr(d))
	fi

	to nargs do
		poppcl()
	od

	if slots then
		popslots(slots)
	fi

	do_getretvalue(p)
end

global proc do_max_int(int cond)=
	mclopnd ax,bx

	ax:=loadopnd(yy, pmode)
	bx:=loadopnd(zz, pmode)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	poppcl()
end

global proc do_max_float(int opc)=
	mclopnd ax,bx
	ax:=loadopnd(yy, pmode)
	bx:=getopnd(zz, pmode)
	genmc(opc,ax,bx)
	poppcl()
end

global proc do_maxto_int(int cond, mode)=
	mclopnd ax,bx,lx
	int lab

!	if size<8 then merror("min/maxto size?") fi

	ax:=getopnd_ind(zz, pmode)
	bx:=loadopnd(yy, pmode)

	genmc(m_cmp, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_mov, ax,bx)
	genmc(m_labelx, lx)
	poppcl()
	poppcl()
end

global proc do_maxto_real(int cond, mode)=
	mclopnd px,ax,bx,lx
	int lab

	px:=getopnd_ind(zz, mode)
	bx:=loadopnd(yy, mode)

	pushpcl_reg(mode)

	ax:=getopnd(yy, pmode)

	genmc(m_mov, ax, px)

	genmc(m_comiss+ispwide(mode), ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_mov, px,bx)
	genmc(m_labelx, lx)
	poppcl()
	poppcl()
	poppcl()
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

global proc do_absreal(mclopnd ax, int mode)=
	if ispwide(pmode) then
		if not lababs64 then lababs64:=mcreatefwdlabel() fi
		genmc(m_andpd, ax, mgenlabelmem(lababs64))
	else
		if not lababs32 then lababs32:=mcreatefwdlabel() fi
		genmc(m_andps, ax, mgenlabelmem(lababs32))
	fi
end

global proc do_loadbf_const(pcl p, int i, j) =
	mclopnd ax, mx
	word mask

	ax:=loadopnd(xx, pmode)

	if j=63 then			!signed field includes sign bit; assume i>0
		genmc(m_sar, ax, mgenint(i))
	else

		if i then
			genmc(m_shr, ax, mgenint(i))
		fi

		mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
		if mask<=word(int32.max) then			!use immediate
			genmc(m_andx, ax, mgenint(mask))
		else
			mx:=loadopnd(yy, tpu64)
			genmc(m_mov, mx, mgenint(mask))
			genmc(m_andx, ax, mx)
		fi
	fi

	poppcl()
	poppcl()
end

global proc do_loadbf_var(pcl p) =
	merror("LOADBF_VAR")
end

global proc do_storebit(pcl p) =
!yy.[zz]:=xx; y.[z]:=x or b.[c]:=a
	mclopnd px, ax, cx, ix
	pcl q, r
	int i, offset
	byte mask1s, mask0s

	q:=isimmload(zz)
	r:=isimmload(xx)

	if q then						!a.[k] := 0/1/x
		px:=getopnd_ind(yy, tpu8)	!update only a specific byte
		i:=q.value	
		offset:=i/8					! byte offset 0..7
		i iand:=7					! i will be bit index 0..7
		px:=applyoffset(px, offset)	! point to that byte

		mask0s:=1<<i				!eg 00001000
		mask1s:=inot(1<<i)			!eg 11110111

		if r then
			if r.value=0 then
				genmc(m_andx, px, mgenint(mask1s, pmode))
			else
				genmc(m_orx, px, mgenint(mask0s, pmode))
			fi
		else
			ax:=loadopnd(xx, tpu8)
			genmc(m_andx, px, mgenint(mask1s, pmode))		!clear dest bit first
			if i then
				genmc(m_shl, ax, mgenint(i, tpu8))
			fi
			genmc(m_orx, px, ax)							!add in 0 or 1
		fi
	elsif r then								!A.[i]:=0/1/x
		px:=getopnd_ind(yy, pmode)				!update whole dest word

		if q=nil then								!A.[i]:=0/1
			ax:=getworkregm(tpu64)
			genmc(m_mov, ax, mgenint(1))
			
			cx:=mgenreg(r10,tpu64)
			genmc(m_push, cx) when r10used
			ix:=loadparam(zz, tpi64, r10)
			genmc(m_shl, ax, changeopndsize(cx, 1))
			genmc(m_pop, cx) when r10used

!Now have 00001000 for ezzmple in ax
			if r.value=0 then
				genmc(m_notx, ax)				!change to 111101111
				genmc(m_orx, px, ax)			!set to 0
			else								!set to 1 (assume r.value was 1)
				genmc(m_orx, px, ax)
			fi

		else									!A.[i]:=x
			merror("STOREBIT/VAR")
		fi
	fi

	poppcl()
	poppcl()
	poppcl()
end

global proc do_storebf(pcl p) =
	mclopnd ax,rx,mx,mx4,dx
	int i,j
	pcl q, r
	word mask

	q:=isimmload(yy)
	r:=isimmload(zz)

	if q=r=nil then
		merror("storebf not imm")
	fi

	dx:=loadopnd(ww, pmode)

	ax:=getopnd_ind(xx, pmode)

	i:=q.value
	j:=r.value

	mx:=getworkregm(tpu64)
	rx:=getworkregm(pmode)

	genmc(m_mov, rx, ax)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

	genmc(m_mov, mx, mgenint(mask))

	if i then
		genmc(m_shl, dx, mgenint(i))
	fi

	genmc(m_andx, rx, changeopndsize(mx, p.size))
	genmc(m_orx, rx, dx)

	genmc(m_mov, ax, changeopndsize(rx, p.size))

	poppcl()			!j
	poppcl()			!i
	poppcl()			!A
	poppcl()			!x?
end

global func gethostfn(int opc)psymbol d =
	if igethostfn=nil then
		merror("gethostfn?")
	fi
	d:=igethostfn(opc)
	if d=nil then
		merror("No host fn:", pclnames[opc])
	fi
	d
end

global proc copyblockarg(mclopnd px, int size, ARGNO)=
!px refers to a block in a parameter register
!if px is nil, then called for block in zz that will be pushed

!copy the block to a block temp, and change px's register to
!refer to that new block

	psymbol dblock
	mclopnd ax, bx, axi, bxi

!RETURN
	IF PX=NIL THEN
		println "High block arg not copied in", currfunc.name,,"()"
		return
!MERROR("COPY HIGH BLOCKARG NOT READY")
	FI


!CPL "COPY BLOCK ARG", ARGNO, SIZE
!	MCOMM(ADDSTR("COPY BLOCK ARG TO TEMP ", STRINT(SIZE)))
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

global proc fixmain=
!d is a main func with 2 params
!convert params to locald, add more locals needed for calling __getmainargs
	psymbol d:=currfunc, e
	psymbol dn, dargs, denv, dinfo

CPL "FIXMAIN", =PCMDSKIP, $CMDSKIP

	dn:=d.nextparam
	dargs:=dn.nextparam

!add 2 new locals
	denv:=pc_makesymbol("$env", local_id)
	denv.mode:=tpref
	denv.size:=8

	dinfo:=pc_makesymbol("$info", local_id)
	dinfo.mode:=tpblock
	dinfo.size:=128

	pc_addlocal(denv)
	pc_addlocal(dinfo)

!remove dn/dargs as params

	dn.nextparam:=dargs.nextparam:=d.nextparam:=nil
	d.nparams:=0
	dn.id:=local_id
	dargs.id:=local_id
!
!add them as locals

	pc_addlocal(dargs)
	pc_addlocal(dn)

	genmc(m_push, mgenreg(r0))
	genmc(m_lea , mgenreg(r0), mgenmem(dinfo))
	genmc(m_push, mgenreg(r0))
	genmc(m_sub, dstackopnd, mgenint(32))
	genmc(m_lea,  mgenreg(r10), mgenmem(dn))
	genmc(m_lea,  mgenreg(r11), mgenmem(dargs))
	genmc(m_lea,  mgenreg(r12), mgenmem(denv))
	genmc(m_xorx, mgenreg(r13), mgenreg(r13))
	genmc(m_call, mgenextname("__getmainargs*"))
!
	genmc(m_sub, dstackopnd, mgenint(48))

!do pcmdskip fixes
	if pcmdskip then
		genmc(m_sub, mgenmem(dn), mgenint(pcmdskip, tpi32))
		genmc(m_add, mgenmem(dargs), mgenint(pcmdskip*8))
	fi

end
=== mc_libmcl.m 0 0 11/41 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

int mclseqno

[-1..10]mclopnd smallinttable
[20]psymbol nametable
int nnametable

global macro isframex(d) = (d.id in [local_id, param_id])

global macro mcomm = mgencomment

global proc mclinit=
	mclopnd a
	int r,s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") fi

!CPL "MCLINIT"
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

	lab_funcnametable:=0
	lab_funcaddrtable:=0

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

!	m:=pcm_allocz(mclrec.bytes)
!	m:=pcm_alloc64z()

	m:=pcm_allocnfz(mclrec.bytes)
!	clear m^

!IF INT(B)=0XFFFF'FFFF'FFFF'FFF0 THEN
!CPL "GENMC",MCLNAMES[OPCODE],A,B
!	CPL "BAD B"
!	CPL "BAD B"
!	STOP
!FI


	m.opcode:=opcode
	m.seqno:=++mclseqno
	m.mpos:=mmpos

	m.a:=a
	m.b:=b

	case opcode
	when m_call then
		++nproccalls

	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_labelx then
		labno:=a.labelno

	when m_mov then				!change to movd/q if needed
		if a.mode=a_xreg or b.mode=a_xreg then
			m.opcode:=(a.size=8|m_movq|m_movd)
		fi

!IF A.SIZE<>B.SIZE and b.size THEN
!cpl =a.size, =b.size
!merror("MOV/SIZE MISMATCH")
!FI

	esac

	if mccode then
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi

end

global proc genmc_cond(int opcode, cond, mclopnd a=nil,b=nil)=
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

!++NMCLOPND
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

	a.regix:=ireg
	a.scale:=scale
	a.size:=size

	a.offset:=offset

	if labno then
		a.value:=labno
		a.valtype:=label_val
	elsif def then
		a.def:=def
		++def.nrefs
		a.valtype:=def_val
		if isframex(def) then
			a.reg:=rframe
		fi
	fi

	return a
end

global proc mgencomment(ichar s)=
!if not debugmode then return fi
	if s=nil or s^=0 then
		genmc(m_blank)
	else
		genmc_str(m_comment,s)
	fi
end

global func mgenstring(ichar s,int length=-1)mclopnd=
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

EXPORT func mgenint(int64 x,int mode=tpi64)mclopnd a=
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

global func mgenint0(int64 x,int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global func mgenrealmem(real64 x,int mode=tpr64)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_mem
	if ispwide(mode) then
		a.value:=getrealindex(x)
	else
		a.value:=getreal32index(x)
	fi
	a.valtype:=label_val
	a.size:=psize[mode]
	return a
end

global func mgenrealimm(real64 x,int mode=tpr64)mclopnd a=
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

global func mgenmem(psymbol d, int mode=tpvoid)mclopnd a=
	int reg

	if d.reg then
		if pfloat[d.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d)
		fi
	fi

	reg:=rnone
	if isframex(d) then
!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!			return frameregtable[d.offset]
!		fi

		reg:=rframe
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	++d.nrefs
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
	++d.nrefs

	a:=newmclopnd()
	a.mode:=a_imm

	a.def:=d
	++d.nrefs
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

!	if xreg=rnone then xreg:=++currxregno fi
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
		a.size:=psize[mode]
		a
	else
		if size=0 then size:=8 fi
		if fuseregtable then
			return regtable[reg,size]
		fi
		return mgenreg0(reg,size)
	fi
end

global func mgenregi(int reg, mode=tpi64)mclopnd a =
	if fuseregtable then
		return regtable[reg, psize[mode]]
	fi
	return mgenreg0(reg, psize[mode])
end


!global func mgenreg(int reg, mode=tpi64)mclopnd a=
!	a:=newmclopnd()
!	a.mode:=a_reg
!	a.reg:=reg
!	a.size:=size
!	return a
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

!EXPORT proc merror(ichar mess,ichar param="")=
!	int lineno
!	ichar filename, sourceline
!
!	if igetmsourceinfo then
!		lineno:=igetmsourceinfo(mmpos, &filename, &sourceline)
!		CPL =LINENO
!		CPL =FILENAME
!	else
!		lineno:=0
!		filename:="?"
!	fi
!
!!CPL "MERROR????"
!
!	fprintln "MCL Error: # (#) on Line: # in #, PCL:#",mess,param, lineno, filename,ppseqno
!
!	pcerrorstop(filename, lineno)
!end

global proc merroropnd(ichar mess,int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mmpos>>24],mmpos iand 16777215)
end

global func mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_labelx,mgenlabel(lab))
end

func checkregvar(int reg, ispfloat)psymbol d=
	RETURN NIL
end

global func mgenextname(ichar s)mclopnd=
	[64]char str
	psymbol d
	static [20]psymbol table
	static int ntable

	strcpy(&.str,s)
	str[strlen(s)]:=0			!lose final *

	d:=findnamesym(str)

	if not d then
		d:=pcm_allocnfz(pstrec.bytes)

		d.name:=pcm_copyheapstring(&.str)
		d.id:=import_id
		d.imported:=1
		addnamesym(d)
	fi

	return mgenmemaddr(d)
end

!global proc domcl_assem(unit pcode)=
!	return when not pcode or pcode.tag<>jassem
!
!	assemused:=1
!
!	genmc(pcode.asmopcode, genasmopnd(pcode.a),genasmopnd(pcode.b))
!	mccodex.cond:=pcode.cond
!
!	case pcode.asmopcode
!	when m_pcmpistri,m_pcmpistrm then
!		if pcode.c=nil or pcode.c.tag<>jconst then gerror("pcmpistr/no imm") fi
!		mccodex.c:=pcode.c.value
!
!	esac
!
!end

!func genasmopnd(unit p)mclopnd ax=
!!	[1..8]byte regmodes=(tpu8, tpu16, 0, tu32, 0,0,0, tpu64)
!	psymbol d
!	int offset,labno
!	unit a				!expr: nil/name/const/(add name, const)
!	unit x,y
!	psymbol e
!
!	if p=nil then return nil fi
!
!	case p.tag
!	when jassemreg then
!!		ax:=mgenreg(p.reg, p.regsize)
!		ax:=mgenreg(p.reg, regmodes[p.regsize])
!
!	when jconst then
!		ax:=mgenint(p.value)
!
!	when jassemmem then
!		a:=p.a
!		d:=nil
!		offset:=labno:=0
!
!		if a then
!			case a.tag
!			when jconst then
!				offset:=a.value
!			when jname then
!				d:=a.def
!				if d.nameid=labelid then
!					labno:=fizzsmlabel(d)
!					d:=nil
!				fi
!			when jbin then
!				x:=a.a
!				y:=a.b
!				if x.tag=jname and y.tag=jconst then
!					d:=x.def
!					if d.nameid=labelid then
!						labno:=fizzsmlabel(d)
!						d:=nil
!					fi
!				else
!					goto error
!				fi
!				offset:=(a.pclop in [kadd,kaddpx]|y.value|-y.value)
!			when junary then
!				if a.pclop<>kneg then merror("assume/unary") fi
!				unless a.a.tag=jconst then gerror("-name") end
!				offset:=-a.a.value
!			when jsyscall then
!MERROR("ASSEM/SYSFN?")
!!				labno:=getsysfnlabel(a.opcode)
!
!			else
!error:
!				cpl jtagnames[a.tag]
!				gerror("Can't do memexpr")
!			esac
!		fi
!		ax:=mgenindex(areg:p.reg, ireg:p.regix, scale:p.scale, size:ttsize[p.prefixmode],
!			offset:offset, labno:labno, def:d)
!
!	when jname then
!		d:=p.def
!		if d.nameid=labelid then
!			labno:=fizzsmlabel(d)
!			ax:=mgenlabel(labno)
!		else
!			ax:=mgenmemaddr(d)
!		fi
!
!	when jassemxreg then
!		ax:=mgenxreg(p.reg)
!	when jbin then				!assume add/sub
!		x:=p.a
!		y:=p.b
!		if x.tag=jname and y.tag=jconst then
!			d:=x.def
!			offset:=(p.pclop in [kadd,kaddpx]|y.value|-y.value)
!			if d.nameid=labelid then
!				labno:=fizzsmlabel(d)
!				ax:=mgenlabel(labno)
!			else
!				ax:=mgenmemaddr(d)
!			fi
!			ax.offset:=offset
!		else
!			gerror("ax:imm/add")
!		fi
!	else
!		cpl jtagnames[p.tag]
!		gerror("genasmopnd?")
!	esac
!
!	return ax
!
!end

!func fixasmlabel(psymbol d)int=
!!d.labelno contains the label number that is passed to PCL
!!PCL maintains a labelmap[] array to convert such labels to renumbered labels
!!Do that translation here, and return that new label
!!Note: mapped label is stored as negative value to indicate it's been done
!!Will return +ve mapped label
!
!	if d.labelno=0 then
!		gerror("FIXASMLABEL: zero")
!	fi
!	return d.labelno
!end

global func mgenregvar(psymbol d)mclopnd a=
	a:=mgenreg(d.reg, tpu64)
	isregvar[d.reg]:=1

	return a
end

global func mgenxregvar(psymbol d)mclopnd a=
	a:=mgenxreg(d.reg)
	isxregvar[d.reg]:=1

	return a
end

global func getopndcat(mclopnd ax)int =
!return mcl opnd category

	case ax.mode
	when a_reg, a_xreg then reg_cat						! R
	when a_imm then imm_cat								! d
	when a_mem then
		if ax.reg=ax.regix=0 then
			mem_cat										! [d]
		elsif ax.reg or ax.regix and ax.scale<=1 then
			ireg_cat									! [R]
		else
			regmem_cat									! [R+d] etc
		fi
	else
		no_cat
	esac
end

global func getprimreg(mclopnd ax)int =
!get primary reg value; only one should be active
!return 0 if no regs
!//error if both regs are active

	if ax.reg then
!		if ax.regix then merror("getprim?") fi
		ax.reg
	else
		ax.regix	!0 if no regs used
	fi
end

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

global func getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if cstringlist and eqstring(cstringlist.svalue,s) then
		return cstringlist.labelno
	fi

	return addconst(cstringlist, cast(s))
end

global func addconst(ref constrec &clist, int value)int=
	ref constrec p
	p:=pcm_allocnfz(constrec.bytes)
	p.value:=value
!CPL "ADDCONST",MLABELNO+1
	p.labelno:=++mlabelno
	p.nextconst:=clist
	clist:=p
	return mlabelno
end

global func getrealindex(real x)int=
	return addconst(creallist,cast@(x,int))
end

global func getreal32index(real x)int=
	return addconst(creal32list,cast@(x,int))
end

global func ispoweroftwo(int64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	int64 a
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

global proc axerror(ichar mess)=
	CPL "AX ERROR:", mess, "AASEQ:", aaseqno
	CPL
	STOP 1

end

global func newblocktemp(int size)psymbol=
	[16]char str
	psymbol d

	if nblocktemps>maxblocktemps then
		merror("Too many block temps")
	fi
	++nblocktemps

	fprint @str,"$B#",nblocktemps
	d:=pc_makesymbol(str)
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

global func findnamesym(ichar s)psymbol d=
!search for s in cache of named symbols

	for i to nnametable do
		if eqstring(s, nametable[i].name) then
			return nametable[i]
		fi
	od
	nil
end

global proc addnamesym(psymbol d)=
!add new name symbol, which should be unique

	if nnametable<nametable.len then
		nametable[++nnametable]:=d
	fi
end
=== mc_stackmcl.m 0 0 12/41 ===
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
!		return mgentemp(n, mode)
	esac

!In PCL rec

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
!		bx:=mgenint(a.value, psize[mode])

		CASE PSIZE[PMODE]
		WHEN 2 THEN
			A.VALUE IAND:=0xFFFF
		WHEN 4 THEN
			A.VALUE IAND:=0xFFFF'FFFF
		ESAC

		bx:=mgenint(a.value, mode)
!CPL "GETOPND", =MSTROPND(BX), BX.SIZE, =PSIZE[MODE], =BX.VALUE
		if a.value in i32.bounds then			!keep as immediate
			ax:=bx
		else
			ax:=getworkreg_rm(reg, mode)
			genmc(m_mov, ax, bx)
		fi

	when real_opnd, real32_opnd then
		ax:=mgenrealmem(a.xvalue, mode)

	when string_opnd then
		ax:=getworkreg_rm(reg, mode)

!		genmc(m_mov, ax, mgenlabel(getstringindex(a.svalue)))
		genmc(m_lea, ax, mgenlabelmem(getstringindex(a.svalue)))

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
!MCOMM("LOADOP1")

	ax:=getopnd(n, mode, reg)
	ax:=loadtoreg(ax, mode, reg)
!
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

!global func loadretval(int n, mode, reg)mclopnd ax =
!!Load operand to return register
!!reg will be r0 for most functions
!	ax:=getopnd(n, mode, reg)
!	ax:=loadtoreg_m(ax, mode, reg)
!	ax
!end

global proc pushopnd(int n, mode)=
!Push a to hardware stack then pop it from pcl stack
!The hardware stack is popped on return from a call

	mclopnd ax, bx
	pcl p:=pclopnd[n]			!in case it is mem/int etc

	if mode=tpvoid then mode:=pclmode[n] fi

!First look for operands that can be directly pushed without using a register

	if pclloc[n]=pcl_loc then	!p refers to operand
		case p.opndtype
		when mem_opnd then
			if psize[mode]=8 then
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

	return ax when ax.mode in [a_reg, a_xreg]

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
		genmc(m_xorx, bx, bx)
	
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
!	pushpcl_reg(getworkreg(mode), mode)				!create new zz opnd, old is now yy
	pushpcl_reg(mode)							!create new zz opnd, old is now yy

MCOMM("DUPLOP")
	genmc(m_mov, getopnd(zz, mode), getopnd(yy, mode))	!copy old to new
end

global func getworkireg:int=
	for r in r0..r13 do
		if workregs[r] and regset[r]=0 then
			regset[r]:=1
			spillregs[r]:=1
			return r
		fi
	od
	merror("No more work regs")
	0
end

global func getworkxreg:int=
	for r in r4..r15 do
		if workxregs[r] and xregset[r]=0 then
			xregset[r]:=1
			spillxregs[r]:=1
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

!	if reg=rnone then
	if reg in [rnone, rframe] then
		return getworkregm(mode)
	fi

	mgenreg(reg, mode)
end

proc start=
	for i in r0..r9 do
		workregs[i]:=1
	od

	for i in r4..r15 do
		workxregs[i]:=1
	od
end

global proc freeireg(int reg)=
	regset[reg]:=0
end

global proc freexreg(int reg)=
	xregset[reg]:=0
end

global proc freereg(int reg, mode)=
	if ispfloat(mode) then
		xregset[reg]:=0
	else
		regset[reg]:=0
	fi
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

	else
		if allregs or reg in r0..r5 then
			genmc(m_mov, mgentemp(n, mode), mgenxreg(reg, mode))
		fi
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

global proc savenextxopnd=
!as savenextopnd but only does AX/BX/PX regs 
	for i:=1 to noperands do
		if pclloc[i]=reg_loc and ispfloat(pclmode[i]) then
			saveopnd(i,1)
			return
		fi
	od
end

global proc movetoreg(int newreg)=
!move top of stack (assumed to be in reg) to newreg
!assumes integer reg
	int oldreg
	int mode:=pclmode[zz]

	loadopnd(zz, mode)
	oldreg:=pclreg[zz]

	if oldreg=newreg then
		return
	fi

	if regset[newreg] then
		merror("movereg/reg in use")
	fi

	genmc(m_mov, mgenreg(newreg,mode), mgenreg(oldreg,mode))

	pclreg[zz]:=newreg

	if ispfloat(mode) then
		xregset[newreg]:=1
		if newreg>=xr3 then highxreg max:=newreg fi
	else
		regset[newreg]:=1
		if newreg>=r10 then highreg max:=newreg fi
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

global proc checkallloaded=
	for i to noperands do
		if pclloc[i]=pcl_loc and pclopnd[i].opndtype=mem_opnd then
			loadopnd(i, pclopnd[i].mode)
		fi
	od
end

global func stropndstack(int indent=0)ichar=
	static [512]char str
	[512]char str2
	ichar s:=&.str, t

	if indent then
		fprint @s, "="*40 + "#:(", NOPERANDS
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
			strcat(s, "(")
int fs:=fpshortnames
fpshortnames:=1
			strcat(s, stropnd(pclopnd[i]))
fpshortnames:=fs
			strcat(s, ")")
		esac
		if pclcount[i]>1 then strcat(s, "@") fi
!		strcat(s, "@")
!		strcat(s, locnames[pclloc[i]])
		strcat(s, "<")
		strcat(s, pstdnames[pclmode[i]])
		strcat(s, ">")

		if i<noperands then strcat(s,", ") fi
	od
	strcat(s,") ")

	ipadstr(str, 50)

	strcat(s,"R:(")
	for r:=r0 to regmax do
		strcat(s,(regset[r]|"1 "|"0 "))
	od
	strcat(s,") ")

	strcat(s,"XR:(")
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
=== mc_genss.m 0 0 13/41 ===
const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

const wbit = 3

byte rex
byte sizeoverride					!32=>16 switch
byte addroverride					!32=>16 switch
byte f2override						!xmm regs
byte f3override						!xmm regs
byte nowmask						!disable w-bit
byte usesizeb						!1 tests opnd b for wmask

record amoderec =					!return from genrm
	byte modrm						!
	byte sib						!
	i8 usesib						!-1/0/1 = rip/not used/used
	byte dispsize					!0, 1 or 4
	int32 offset					!for dispsize = 1/4
end

mclopnd extraparam

int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
ref relocrec currrelocs
int nrelocs

int instrno=2

[r0..r15]byte ishighreg				!high regs have 0x40 (see start)

REF MCLREC CURRMCL
ref riprec ripentry

macro genbyte(x) = currdata.pcurr++^:=x

macro makemodrm(mode,opc,rm) = mode<<6+opc<<3+rm

export int psstime


global proc genss(int obj=0)=
	int index
	ref mclrec m

!CPL "////////////////GENSS"
!	if ssdone then axerror("genss?") fi
	return when ssdone

	psstime:=clock()
	initlib(mlabelno)

	ss_zdatalen:=0
	ss_zdata:=buffercreate()
	ss_idata:=buffercreate()
	ss_code:=buffercreate()
	ss_idatarelocs:=nil
	ss_coderelocs:=nil
	ss_nsymbols:=0

	switchseg(code_seg)

	aaseqno:=9999
	extraparam:=nil

!	fixregvar()

	m:=mccode
	index:=0

	while m do
		doinstr(m,++index)
		m:=m.nextmcl
	od

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		axerror("Zdata contains code or data")
	fi

	if obj then					!do fixups needed for .obj files
		ref riprec pr			!(exe module does its own fixups)
		ref byte codeaddr
		ref u32 offsetptr

		codeaddr:=bufferelemptr(ss_code, 0)
!CPL =CODEADDR
			pr:=riplist
			while pr, pr:=pr.next do
				offsetptr:=ref u32(codeaddr+pr.offset)
!			PRINTLN "**********  RIP:",PR.OFFSET, OFFSETPTR^
				offsetptr^-:=pr.immsize
		od
	fi

	ssdone:=1
	psstime:=clock()-psstime

end

proc doinstr(ref mclrec m,int index)=
	mclopnd a,b
	psymbol d,e
	int x,offset,shortjmp,n

	buffercheck(currdata)

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=nowmask:=usesizeb:=0

	a:=m.a
	b:=m.b

	++instrno
	aaseqno:=m.seqno
	ripentry:=nil
	CURRMCL:=M

!CPL "INSTR",MCLNAMES[M.OPCODE]

	switch m.opcode
	when m_procstart then
		CURRASMPROC:=M.A.DEF
!CPL "PROC",CURRASMPROC.NAME

	when m_procend then
	when m_define then

	when m_definereg then
	when m_deleted then

	when m_labelname then
		case a.valtype
		when stringimm_val then
		when def_val then
			d:=a.def
			d.reftype:=back_ref
!CPL "SETSEG1", D.NAME, CURRSEG
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)

			if d.exported then
				getstindex(d)
			fi

			dofwdrefs(d)
		esac

	when m_labelx then
		d:=labeldeftable[a.labelno]

		d.reftype:=back_ref
!CPL "SETSEG2",D.NAME, CURRSEG
		d.segment:=currseg
		d.offset:=getcurrdatalen(6)
		dofwdrefs(d)

	when m_call then
		do_call(a)

	when m_jmp then
		do_jmp(a,m)

	when m_jmpcc then
		d:=getdef(a,1)
		offset:=getrel32(d,getcurrdatalen(7)+1)
		if offset<0 then			!backjump
			if offset<-126 then
				genbyte(0x0F)
				genbyte(0x80+m.cond)
				gendword(offset-4)
			else
				genbyte(0x70+m.cond)
				genbyte(offset)
			fi
		else
			shortjmp:=checkshortjump(m,d)
			if not shortjmp then
				genbyte(0x0F)
				genbyte(0x80+m.cond)
				genrel32(a)
			else
				genbyte(0x70+m.cond)
				genrel8(a)
			fi
		fi

	when m_db then
		genopnd(a,1)
	when m_dw then
		genopnd(a,2)
	when m_dd then
		genopnd(a,4)
	when m_dq then
		genopnd(a,8)

!	when m_segment then
!		switchseg(a.value)

	when m_csegment then
		switchseg(code_seg)
	when m_isegment then
		switchseg(idata_seg)
	when m_zsegment then
		switchseg(zdata_seg)

	when m_nop, m_halt then
		genbyte(mclcodes[m.opcode])

	when m_cbw then
		genbyte(0x66)
		genbyte(0x98)

	when m_cwd then
		genbyte(0x66)
		genbyte(0x99)

	when m_cdq then
		genbyte(0x99)

	when m_cqo then
		genbyte(0x48)
		genbyte(0x99)

	when m_ret then
		genbyte(0xC3)

	when m_retn then
		if a.mode<>a_imm then axerror("retn?") fi
		genbyte(0xC2)
		genword(a.value)

	when m_push then
		do_push(a)

	when m_pop then
		do_pop(a)

	when m_inc, m_dec then
		do_inc(a,mclcodes[m.opcode])

	when m_neg, m_notx, m_mul, m_imul, m_div, m_idiv then
		do_neg(a,mclcodes[m.opcode])

	when m_add, m_sub, m_andx, m_orx, m_xorx, m_adc, m_sbb, m_cmp then
		do_arith(a,b, mclcodes[m.opcode])

	when m_mov then
		do_mov(a,b)

	when m_lea then
		do_lea(a,b)

	when m_movsx then
		do_movsx(a,b,0xBE)

	when m_movzx then
		do_movsx(a,b,0xB6)

	when m_movsxd then
		do_movsxd(a,b)

	when m_xchg then
		do_exch(a,b)

	when m_imul2 then
		do_imul2(a,b)

	when m_resb, m_resw, m_resd, m_resq then
		if a.mode=a_imm then
			n:=a.value*mclcodes[m.opcode]
			case currseg
			when code_seg then
				to n do genbyte(0x90) od
			when idata_seg then
				to n do genbyte(0) od
			else
				ss_zdatalen+:=n
			esac
		
		else
			axerror("resb?")
		fi

	when m_align then
		if a.mode=a_imm then
			x:=a.value
			if x<1 or x>16384 then axerror("align2") fi
			if currseg<>zdata_seg then
				while bufferlength(currdata) rem x do genbyte((currseg=code_seg|0x90|0)) od
			else
				while ss_zdatalen rem x do	++ss_zdatalen od
			fi
		else
			axerror("align?")
		fi

	when m_shl,m_shr,m_sar,m_rol,m_ror,m_rcl,m_rcr then
		do_shift(a,b,mclcodes[m.opcode])

	when m_test then
		do_test(a,b)

	when m_loopcx, m_loopz, m_loopnz then
		do_loop(a,mclcodes[m.opcode])

	when m_jecxz then
		do_jcxz(a,4)

	when m_jrcxz then
		do_jcxz(a,8)

	when m_xlat then
		genbyte(0xD7)

	when m_setcc then
		do_setcc(m.cond,a)

	when m_movd then
		do_movxmm(a,b,4)

	when m_movq then
		do_movxmm(a,b,8)

	when m_addss, m_subss, m_mulss, m_divss, m_sqrtss, m_minss, m_maxss then
		do_arithxmm(a,b,0xF3,mclcodes[m.opcode])

	when m_addsd, m_subsd, m_mulsd, m_divsd, m_sqrtsd, m_minsd, m_maxsd then
		do_arithxmm(a,b,0xF2,mclcodes[m.opcode])

	when m_andps,m_xorps then
		do_logicxmm(a,b,mclcodes[m.opcode],4)

	when m_andpd,m_xorpd,m_pand,m_pxor then
		do_logicxmm(a,b,mclcodes[m.opcode],8)

	when m_comiss then
		do_arithxmm(a,b,0,0x2F)

	when m_comisd then
		do_arithxmm(a,b,0x66,0x2F)

	when m_cvtss2sd then
		do_convertfloat(a,b,0xF3)

	when m_cvtsd2ss then
		do_convertfloat(a,b,0xF2)

	when m_cvtss2si then
		do_fix(a,b,0xF3,0x2D)

	when m_cvtsd2si then
		do_fix(a,b,0xF2,0x2D)

	when m_cvttss2si then
		do_fix(a,b,0xF3,0x2C)

	when m_cvttsd2si then
		do_fix(a,b,0xF2,0x2C)

	when m_cvtsi2ss then
		do_float(a,b,0xF3)

	when m_cvtsi2sd then
		do_float(a,b,0xF2)

	when m_param then
		extraparam:=a

	when m_cmovcc then
		do_cmovcc(m.cond, a,b)

	when m_fsqrt,m_fsin,m_fcos,m_fsincos,m_fptan, m_fpatan,m_fabs,m_fchs then
		genbyte(0xD9)
		genbyte(mclcodes[m.opcode])

	when m_fld, m_fst, m_fstp then
		do_fmem(a,1,mclcodes[m.opcode])

	when m_fild, m_fist, m_fistp then
		do_fmem(a,0,mclcodes[m.opcode])

	when m_fadd, m_fsub, m_fmul, m_fdiv then
		genbyte(0xDE)
		genbyte(mclcodes[m.opcode])

	when m_cmpsb then
		genbyte(0xA6)

	when m_cmpsw then
		genbyte(0x66)
		genbyte(0xA7)
	when m_cmpsd then
		genbyte(0xA7)
	when m_cmpsq then
		genbyte(0x48)
		genbyte(0xA7)

	when m_rdtsc then		!single opcodes that need a 0x0F prefix
		genbyte(0x0F)
		genbyte(mclcodes[m.opcode])

	when m_movdqa, m_movdqu then
		do_movdqx(a,b,mclcodes[m.opcode])

	when m_finit then
		genbyte(0xDB)
		genbyte(0xE3)

	when m_fldz, m_fld1, m_fldpi, m_fld2t, m_fld2e, m_fldlg2, m_fldln2 then
		genbyte(0xD9)
		genbyte(mclcodes[m.opcode])

	when m_popcnt then
		do_popcnt(a,b)

	when m_bsf, m_bsr then
		do_bsf(a,b,mclcodes[m.opcode])

	when m_cpuid then
		genbyte(0x0F)
		genbyte(0xA2)

	when m_bswap then
		do_bswap(a)

	when m_shld, m_shrd then
		do_dshift(a, b, m.c, mclcodes[m.opcode])

	when m_comment then
	when m_blank then
	else
		println "*** Can't do opcode",mclnames[m.opcode],"line",aaseqno,=M.OPCODE,=M_HALT
	CPL
	CPL
	AXERROR("STOPPING")
!	end switch
	end

end

!proc genbyte(int x)=
!	currdata.pcurr++^:=x
!end

proc start=
	ishighreg[r3]:=0x40
	ishighreg[r5]:=0x40
	ishighreg[r14]:=0x40
	ishighreg[r15]:=0x40
end

proc genword(int x)=
	addword(currdata,x)
end

proc gendword(int x)=
	adddword(currdata,x)
end

proc genqword(int64 x)=
	addqword(currdata,x)
end

proc genopnd(mclopnd a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
	ref char s
	int64 x
	int length

	if size=0 then size:=a.size fi

	case a.valtype
	when stringimm_val then
		s:=a.svalue
		length:=strlen(s)
		if length>100 then
			buffercheck(currdata,max(1024,length+1))
		fi
		while s^ do
			genbyte(s++^)
		od
		return
	WHEN NAME_VAL THEN
		PRINTLN "GENSS/NAME OPND"
	esac

	if getdef(a) and size<=2 then
		axerror("8/16-BIT RELOC")
	fi

	case size
	when 1 then
		genbyte(a.value)
	when 2 then
		genword(a.value)
	when 4 then
		case a.valtype
		when intimm_val then
			gendword(a.value)
		when realimm_val then
			real32 x32
			x32:=a.xvalue
			gendword(int@(x32))
!		when realmem_val then
!			CPL "		OPND/REALMEM4"
!		when stringimm_val then
!			CPL "		OPND/STRINGIMM4"
		when def_val,label_val then
			genabs32(a)
!		when name_val then
!			CPL "		OPND/NAME4"
		else
			cpl valtypenames[a.valtype]
			axerror("OPND/4/VALTYPE?")
		esac

	when 8 then
		case a.valtype
		when intimm_val then
			genqword(a.value)
		when realimm_val then
			genqword(int64@(a.xvalue))
!		when realmem_val then
!			CPL "		OPND/REALMEM8",ALINENO
!		when stringimm_val then
!			CPL "		OPND/STRINGIMM8"
		when def_val,label_val then
			genabs64(a)
!		when name_val then
!			CPL "		OPND/NAME8"
		else
			cpl valtypenames[a.valtype]
			axerror("OPND/8/VALTYPE?")
		esac

	esac
end

proc addrelocitem(int reloctype, psymbol d)=
	ref relocrec r
	int stindex, adjust

	stindex:=getstindex(d)

	adjust:=4
	if reloctype=addr64_rel then adjust:=8 fi

!	r:=pcm_alloc(relocrec.bytes)
	r:=pcm_allocnfz(relocrec.bytes)
	r.nextreloc:=currrelocs
	r.reloctype:=reloctype
	r.offset:=getcurrdatalen(1)-adjust
	r.stindex:=stindex

	++nrelocs
	currrelocs:=r
end

func getstindex(psymbol d)int=
!retrieve existing obj st index, or create new one
	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		fi
		d.stindex:=++ss_nsymbols
!CPL "GETSTINDEX",D.NAME,D

		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
			if d.imported then
!CPL "SETSEG3",D.NAME,CODE_SEG
				d.segment:=code_seg
			fi
		fi

	fi
	return d.stindex
end

proc genrel32(mclopnd a)=
!used by call/longjmp/ddoffset
	psymbol d

	d:=getdef(a)

	if d=nil then				!constant
		gendword(a.value)
		return
	fi

	case d.reftype
	when back_ref then
		if d.segment<>currseg then
			axerror("Rel label across segments")			!might be Ok if treated as external?
		fi
		gendword(d.offset-(getcurrdatalen(2)+4)+a.offset)
	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel32_rel)
		gendword(a.offset)
	else								!external psymbol
		gendword(a.offset)		!this is probably just zero
		addrelocitem(rel32_rel,d)
	esac
end

func getdef(mclopnd a,int dneeded=0)psymbol =
	psymbol d

	if a.mode in [a_mem,a_imm] then
		case a.valtype
		when label_val then
			return labeldeftable[a.labelno]
		when def_val then
			d:=a.def
			if d.reftype=0 then
				if not d.imported then
					d.reftype:=fwd_ref
				fi
			fi

			return d
		esac
	fi
	if dneeded then				!must return a non-nil value
		println opndnames_ma[a.mode],valtypenames[a.valtype]
		axerror("getdef/no def")
	fi
	return nil
end

proc genabs32(mclopnd a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then

		gendword(d.offset+a.offset)
		addrelocitem(addr32_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
!CPL "NEW1",D.FWDREFS, D.NAME,D
		if d.id in [local_id, param_id] then
			gendword(d.offset+a.offset)
		else
			gendword(a.offset)
			addrelocitem(addr32_rel,d)
		fi

	else								!external psymbol
		gendword(a.offset)					!this is probably just zero
		addrelocitem(addr32_rel,d)
	esac
end

proc genabs64(mclopnd a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then
		genqword(d.offset+a.offset)
		addrelocitem(addr64_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
		if d.id in [local_id, param_id] then
			genqword(d.offset+a.offset)
		else
			genqword(a.offset)
			addrelocitem(addr64_rel,d)
		fi

	else								!external psymbol
		genqword(a.offset)				!this is probably just zero
		addrelocitem(addr64_rel,d)
	esac
end

func getrel32(psymbol d,int offset)int=
!get rel difference between offset in this segment, and label d
	if d.reftype=back_ref then					!defined earlier in this segment
		if d.segment<>currseg then
			axerror("Rel label across segments2")
		fi
		return d.offset-(offset+1)
	else
		return int32.max
	fi
end

proc dofwdrefs(psymbol d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
	ref fwdrec f
	int offset, seg
	ref byte p8
	ref int32 p32
	ref int64 p64
	ref dbuffer data

	if d.fwdrefs=nil then return fi
	f:=d.fwdrefs

	while f do
		offset:=f.offset

		case f.reltype
		when rel32_rel then
			p32:=bufferelemptr(currdata,offset)
			p32^:=d.offset-offset-4

		when addr32_rel,addr64_rel then
			case f.seg
			when code_seg then data:=ss_code
			when zdata_seg then axerror("Fwd ref in zdata")
			when idata_seg then data:=ss_idata
			esac

			p32:=bufferelemptr(data,offset)
			if f.reltype=addr32_rel then
				p32^:=p32^+d.offset
			else
				p64:=cast(p32)
				p64^:=p64^+d.offset
			fi
		when rel8_rel then
			p8:=bufferelemptr(currdata,offset)
			p8^:=d.offset-offset-1
		else
			CPL RELOCNAMES[F.RELTYPE],D.NAME
			AXERROR("DOFWDREFS/CAN'T DO RELTYPE")
		esac

		f:=f.nextfwd
	od
end

proc genrex=
	if f2override then genbyte(0xF2) fi
	if f3override then genbyte(0xF3) fi
	if sizeoverride then genbyte(0x66) fi
	if addroverride then genbyte(0x67) fi

	if nowmask then rex.[wbit]:=0 fi

	if rex then genbyte(rex iand 15+0x40) fi
end

func isbytesized(int64 x)int=
	return -128<=x<=127
end

func isdwordsized(int64 x)int=
	return int32.min<=x<=int32.max
end

proc genamode(mclopnd a, amoderec am)=
!	int sib,mode,dispsize,offset
	psymbol d
	ref riprec pr

!	sib:=am>>16
!
!	mode:=(am>>8)iand 255
!	dispsize:=am iand 255

	genbyte(am.modrm)

	if am.usesib=1 then
		genbyte(am.sib)
	fi

	case am.dispsize			!disp bytes
	when 0 then
	when 1 then
		genbyte(am.offset)
	when 4 then
		if am.usesib=-1 then
			pr:=pcm_alloc(riprec.bytes)
			pr.next:=riplist
			pr.offset:=currdata.pcurr-currdata.pstart
			ripentry:=riplist:=pr
!CPL "CREATING RIP ENTRY",PR.OFFSET
!OS_GETCH()
		fi
		case a.mode
		when a_mem then
			case a.valtype
			when def_val, label_val then
				genabs32(a)
			when no_val, temp_val then
				gendword(am.offset)
			else
				axerror("genam/3")
			esac
		else
			CPL OPNDNAMES_MA[A.MODE]
			axerror("GENAMODE/MODE?")
		esac
	else
		axerror("genamode size 2/8")
	esac
end

proc setopsize(mclopnd a)=
	case a.size
	when 8 then			!override default 4 bytes
	    rex ior:=wmask
	when 4 then			!assume 4 bytes is default
	when 1 then			!assume set via specific opcodes
	when 2 then			!override default 4 bytes
		sizeoverride:=1
	else
		axerror("Operand size not set")
	esac
end

func getdispsize(mclopnd a, int32 &offset)int=
!look at imm/mem displacement, and return (0,1 or 4) and offset
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	psymbol d

	d:=getdef(a)
	offset:=a.offset

	if d then
		if d.id in [local_id, param_id] then
			offset+:=d.offset
		else
			return 4
		fi
	fi

	if offset then
		return (isbytesized(offset)|1|4)
	else
		return 0
	fi
end

proc checkhighreg(mclopnd a)=
	if a.mode=a_reg then
		rex ior:=ishighreg[a.reg]
	fi
end

proc do_loop(mclopnd a,int opc)=
	int offset

	offset:=getrel32(getdef(a,1),getcurrdatalen(9)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			axerror("loop jmp out of range")
		fi
		genbyte(opc)
		genbyte(offset)
	else
		axerror("Can't do loopxx fwd jump")
	fi
end

proc do_jcxz(mclopnd a,int opsize)=
	int offset

	offset:=getrel32(getdef(a,1),getcurrdatalen(10)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			axerror("jcxz jmp out of range")
		fi
		if opsize=4 then genbyte(0x67) fi
		genbyte(0xE3)
		genbyte(offset)
	else
		axerror("Can't do jcxz fwd jump")
	fi
end

proc do_call(mclopnd a)=
	int am, regcode
	case a.mode
	when a_imm then
		genbyte(0xE8)
		genrel32(a)
	else				!indirect call
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("call[]size")
		esac

		genxrm(0xFF, 2, a)

	esac
end

proc do_jmp(mclopnd a,ref mclrec m)=
	int am, regcode, offset, shortjmp
	psymbol d:=getdef(a)

	case a.mode
	when a_imm then
		offset:=getrel32(d, getcurrdatalen(11)+1)
		if offset<0 and offset>-126 then
			genbyte(0xEB)
			genbyte(offset)
		else
			shortjmp:=0
			if offset>0 then				!fwd jump
!check if destlabel occurs within next 8 instrs, then likely to need short disp
				shortjmp:=checkshortjump(m, d)
			fi

			if not shortjmp then
				genbyte(0xE9)
				genrel32(a)
			else
				genbyte(0xEB)
				genrel8(a)
			fi
		fi
	else				!indirect jump
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("jmp[]size")
		esac

		genxrm(0xFF, 4, a)
	esac

end

func getcurrdatalen(int id)int=
!I think that zdata-seg is only likely when id=6

	if currseg=zdata_seg then
		return ss_zdatalen
	fi
	return bufferlength(currdata)
end

proc do_cmovcc(int cond, mclopnd a,b)=
	if a.size<>b.size and b.size then
		axerror("1:Opnd size mismatch")
	fi
	if a.size=1 then axerror("cmov/byte") fi

	genrrm(0x0F'40+cond, a, b)
end

proc do_fmem(mclopnd a, int freal, code)=
!do fld/fild/fst/fstp/fist,fistp
!freal=1 for fld/etc, 0 for fild etc
!code is middle 3 bits of 2nd byte: 0=load, 2=store, 3=store+pop
	int am, regcode, mf

	if a.mode<>a_mem then
		axerror("fmem/not mem")
	fi

	if freal then
		case a.size
		when 4 then mf:=0
		when 8 then mf:=2
		when 10,16 then
			mf:=1
			case code
			when 0 then code:=5
			when 3 then code:=7
			else
				axerror("r80 not allowed")
			esac
		else
			CPL "SIZE=",A.SIZE
			axerror("fmem size")
		esac
	else
		case a.size
		when 2 then mf:=3
		when 4 then mf:=1
		when 8 then
			mf:=3
			case code
			when 0 then code:=5
			when 3 then code:=7
			else
				axerror("fst i64?")
			esac
		else
			axerror("fmem int size")
		esac
	fi
	
	genxrm(0xD9+mf<<1, code, a)
end

proc genrel8(mclopnd a)=
!a is a known fwd reference, and expected to be <=127 bytes
	psymbol d

	d:=getdef(a,1)

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
		genbyte(0)
	else								!external psymbol
		axerror("genrel8")
	fi
end

func checkshortjump(ref mclrec m,psymbol d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
!return 0
! d can be a named label, or a labelno; either should have .labelno set
	int n

	n:=0
	m:=m.nextmcl
	while m and n<=8 do
		case m.opcode
		when m_labelx then
!		++n
			if m.a.labelno=d.labelno then
				return 1
			fi
!		when m_comment, m_blank then
		when m_comment, m_blank, m_deleted then
		else
			++n
		esac
		m:=m.nextmcl
	od

	return 0
end

func addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
	ref fwdrec q

!	q:=pcm_alloc(fwdrec.bytes)
	q:=pcm_allocnfz(fwdrec.bytes)
	q.nextfwd:=p
	q.offset:=offset
	q.reltype:=reltype
	q.seg:=seg
	return q
end

proc switchseg(int newseg)=
	if newseg=currseg then return fi

	case currseg						!reloc linked list roots must be updated
	when code_seg then
		ss_coderelocs:=currrelocs
		ss_ncoderelocs:=nrelocs
	when idata_seg then
		ss_idatarelocs:=currrelocs
		ss_nidatarelocs:=nrelocs
	esac

	currseg:=newseg

	case currseg
	when code_seg then
		currdata:=ss_code
		currrelocs:=ss_coderelocs
		nrelocs:=ss_ncoderelocs
	when idata_seg then
		currdata:=ss_idata
		currrelocs:=ss_idatarelocs
		nrelocs:=ss_nidatarelocs
	when zdata_seg then
		currdata:=ss_zdata
	esac							!else 0, done at end to update linked lists

end

proc do_popcnt(mclopnd a,b)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi

	f3override:=1
	genrrm(0x0F'B8, a, b)
end

proc do_bsf(mclopnd a,b, int opc)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi
	if a.size<>b.size then axerror("bsf size") fi

	genrrm(0x0F<<8+opc, a, b)
end

proc extendsymboltable=
	ref[]psymbol oldsymboltable
	int oldsymboltablesize

	oldsymboltablesize:=ss_symboltablesize
	oldsymboltable:=ss_symboltable

	ss_symboltablesize*:=2

	ss_symboltable:=pcm_alloc(ref void.bytes*ss_symboltablesize)

	for i:=1 to ss_nsymbols do
		ss_symboltable[i]:=oldsymboltable[i]
	od

	pcm_free(oldsymboltable,ref void.bytes*oldsymboltablesize)
end

global proc initlib(int nlabels)=
	[256]char str

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0
	labeldeftable:=pcm_alloc(nlabels*ref void.bytes)

	for i to nlabels do
!		labeldeftable[i]:=pcm_allocz(strec.bytes)
		labeldeftable[i]:=pcm_allocnfz(pstrec.bytes)
		labeldeftable[i].labelno:=i
!		fprint @&.str,"(L#)",i
		fprint @&.str,"l#",i
		labeldeftable[i].name:=pcm_copyheapstring(&.str)
		labeldeftable[i].reftype:=fwd_ref
	od
end

global func buffercreate(int size=1024)ref dbuffer=
	ref dbuffer a

	a:=pcm_alloc(dbuffer.bytes)

	a.alloc:=size
	a.pstart:=a.pcurr:=pcm_alloc(a.alloc)
	a.pend:=a.pstart+a.alloc
	return a
end

proc bufferexpand(ref dbuffer a)=
	int newalloc,usedbytes
	ref byte p

	newalloc:=a.alloc*2
	usedbytes:=a.pcurr-a.pstart

	if usedbytes>a.alloc then
		println "dbuffer error"
		stop
	fi

	p:=pcm_alloc(newalloc)
	memcpy(p,a.pstart,usedbytes)
	a.pstart:=p
	a.pcurr:=p+usedbytes
	a.alloc:=newalloc
	a.pend:=p+newalloc
end

global proc buffercheck(ref dbuffer a,int n=1024)=
	while a.pend-a.pcurr<n do
		bufferexpand(a)
	od
end

global func bufferlength(ref dbuffer a)int=
	return a.pcurr-a.pstart
end

global func bufferelemptr(ref dbuffer a, int offset)ref void=
	return a.pstart+offset
end

global proc addword(ref dbuffer a, int x)=
	a.pcurr16^:=x
	++(a.pcurr16)
end

global proc adddword(ref dbuffer a, int x)=
	a.pcurr32^:=x
	++(a.pcurr32)
end

global proc addqword(ref dbuffer a, int64 x)=
	a.pcurr64^:=x
	++(a.pcurr64)
end

proc genxrm(int opcode, code, mclopnd b)=
!deal with /d instructions, where code = 0..7
	amoderec am
!	[0..7]byte opbytes @opcode

	setopsize(b)

	am:=genrm(0, code, b, 0)

	case currmcl.opcode
	when m_push, m_pop then rex.[wbit]:=0
	esac


!	if opbytes[2] then genbyte(opbytes[2]) fi		!extra opcodes will not be 0
!	if opbytes[1] then genbyte(opbytes[1]) fi
	if opcode.[16..23] then genbyte(opcode.[16..24]) fi
	genrex()
	if opcode.[8..15] then genbyte(opcode.[8..15]) fi

	genbyte(opcode)
	genamode(b,am)
end

proc genrrm(int opcode, mclopnd a, b)=
!deal with /r instructions; rrm = reg,reg/mem
!opcode represents 1, 2 and maybe 3-byte(?) opcodes, with last in lsb place
!a is a register mclopnd, b is a register or memory mclopnd, always
!when data direction is the other way, as in mov reg/mem, reg, then reverse mclopnds
!Output will be:
! Any override prefixes
! Any rex prefix
! 1 or 2 (or 3?) opcode bytes
! modrm byte
! possible sib byte
! Any address offset (which may be an imm value, or fwd label, or external etc)
!Any additional immediate data that follows should be added by the caller.
!There are two main modes:
! REG, REG   2 registers are involved; there is no address offset, no sib byte
! REG, MEM   1, 2 or 3 registers are involved. Last 2 may be base and index registers,
!            and the index register may be scaled
	amoderec am
!	[0..7]byte opbytes @opcode

!	checkhighreg(a)
	if a.mode=a_reg then rex ior:=ishighreg[a.reg] fi
	setopsize(a)

	if usesizeb then				!wmask comes from b
		rex.[wbit]:=0
		if b.size=8 then rex ior:=wmask fi
	fi

	am:=genrm(a.reg, 0, b, a.mode=a_xreg)
!	am:=genrm(a.reg, 0, b, a.mode=a_xreg)

!	genrex()

!	if opbytes[1] then genbyte(opbytes[1]) fi
!	if opbytes[2] then genbyte(opbytes[2]) fi		!extra opcodes will not be 0
	if opcode.[16..23] then genbyte(opcode.[16..24]) fi
	genrex()
	if opcode.[8..15] then genbyte(opcode.[8..15]) fi

	genbyte(opcode)
	genamode(b,am)
end

func getregcode(int reg, int mask, isxreg=0)int regcode=
!convert m-register code (1 to 16/20) to hardware code (0..7 plus any rex bits)
!mask is the rex bit to set for high registers
!isxreg is 1 for float registers, where I don't need to do the usual mapping

	if not isxreg then
		regcode:=regcodes[reg]
	else
		regcode:=reg-1			!xr1 is 1; xmm0 is 0
	fi

	if regcode>=8 then
		regcode-:=8
		rex ior:=mask
	fi
	regcode
end

proc checkimmrange(int value, size)=
!CPL =VALUE, =SIZE
	case size
	when 1 then
		unless -128<=value<=255 then axerror("exceeding byte value") end

	when 2 then
		unless -32768<=value<=65535 then axerror("exceeding word16 value") end
	else
		unless -0x8000'0000<=value<=0xFFFF'FFFF then axerror("2:exceeding word32 value") end
	esac
end

func genrm(int reg, opc, mclopnd b, int isxreg=0)amoderec=
!reg =  0:	opc is a 3-bit code that goes in reg field of mod-reg-rm
!reg >= r0:	reg is an M-register code, which is converted to a machine reg encoding
!			of 3 bits (to go in middle of modrm byte), and may set rex.r bit for high
!			regs; opc is 0 in this case
!
!b is mclopnd containing rhs reg value, or is mem mclopnd using base/index regs and addr
!For now, return same encoded value as old genrm

	static []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
	int mode, rm, scale, index, base
	int regix, code, ismem
	amoderec am

!	byte modrm						!
!	byte sib						!
!	byte usesib						!0, or 1 if sib used (sib=0 is a valid encoding)
!	byte dispsize					!0, 1 or 4
!	int32 offset					!for dispsize = 1/4


	clear am

!	if b.mode=a_mem and b.addrsize=4 then
!	if b.mode=a_mem then			!.ADDRSIZE is alwaus zero
!		addroverride:=1
!	fi

!deal with first reg/opc field

	if reg then				!else use opc as-is
		opc:=getregcode(reg, rmask, isxreg)
	fi

	case b.mode
	when a_reg, a_xreg then			!modrm can only ref to a single register
		rm:=getregcode(b.reg, bmask, b.mode=a_xreg)
		rex ior:=ishighreg[b.reg]

		am.modrm:=makemodrm(3,opc,rm)
		return am

	when a_mem then
		ismem:=1
		case b.valtype
		when def_val then
			if b.def.id=static_id then ismem:=2 fi
		when realmem_val then ismem:=2
		when label_val then ismem:=2
		esac

!	
!	(intimm_val,	$),		!immediate int
!	(realimm_val,	$),		!immediate real (mainly for dq etc)
!	(realmem_val,	$),		!indirect real (for movq etc)
!	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
!	(def_val,		$),		!var/proc name
!	(label_val,		$),		!label index
!!	(labelind_val,	$),		!label index
!	(name_val,		$),		!immediate string must be output as ah unquoted name
!	(temp_val,		$),		!index of pclopnd temp (later becomes ptr to descriptor?)
!!	(syscall_val,	$),		!

!		if b.valtype=def_val and b.def.nameid in [frameid, paramid] then
!			ismem:=1
!!	ELSIF IF B.VAL
!		fi

	else
		axerror("genrm not mem")
	esac

	mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
	scale:=0				!0=modrm only; 1/2/4/8 means sib used
!	dispsize:=0
!	sib:=-1

	reg:=b.reg
	regix:=b.regix

	if reg=regix=0 then						!address only
		mode:=0
		rm:=4
		scale:=1
		index:=4
		base:=5
		am.dispsize:=4

	elsif b.scale<=1 and regix=0 then			!simple address mode (no sib)
		am.dispsize:=getdispsize(b, am.offset)
		if am.dispsize then
			mode:=(am.dispsize=1|1|2)
		fi

		rm:=base:=getregcode(reg, bmask)

		if rm<>4 then
			if rm=5 and am.dispsize=0 then
				mode:=1; am.dispsize:=1
			fi
			index:=0
		else
			index:=4				!means no index
			scale:=1				!force sib

		fi
	elsif regix and reg=0 then
		am.dispsize:=4
		mode:=0
		rm:=4
		scale:=(b.scale|b.scale|1)
		base:=5
!		index:=regcodes[regix]
		index:=getregcode(regix, xmask)
		if regix=rstack then axerror("Scaled rstack?") fi

	else									!assume regix used; optional reg and disp
		am.dispsize:=getdispsize(b, am.offset)
!		dispsize:=getdispsize(b,offset)
		if am.dispsize then
			mode:=(am.dispsize=1|1|2)
		fi
		rm:=4

		scale:=(b.scale|b.scale|1)
		if reg=0 then
			base:=5
		else
			if reg in [rframe,r7] and am.dispsize=0 then
				mode:=1; am.dispsize:=1
			fi
			base:=getregcode(reg, bmask)
		fi

		if regix=0 then
			index:=4
		else
			index:=getregcode(regix, xmask)
			if not reg then
				am.dispsize:=4
			fi
			if regix=rstack and scale>1 then axerror("Can't scale rstack") fi
		fi
	fi

	if scale then
		am.sib:=scaletable[scale]<<6 + index<<3 + base
		am.usesib:=1
	fi

	if am.dispsize=4 and ismem then
		if reg or regix then
			if phighmem=2 AND ISMEM=2 then
				CPL "Addr32 can't use RIP, line",aaseqno,STRMCLSTR(CURRMCL)
!				axerror("Addr32 can't use RIP")
			fi
		elsif phighmem then
			am.usesib:=-1
			mode:=0
			rm:=5
		fi
	fi

	am.modrm:=makemodrm(mode, opc, rm)
	
	return am
end

proc do_arith(mclopnd a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
	psymbol d
	int opc, dispsize
	int64 x

	case a.mode
	when a_reg then
		case b.mode
		when a_reg,a_mem then
			opc:=code<<3 ior (a.size=1|0x02|0x03)
			genrrm(opc, a, b)

		when a_imm then
	doregimm:
			d:=getdef(b)
			if d then
				if a.size<4 then axerror("add imm/size") fi
				genxrm(0x81, code, a)
				genopnd(b,4)
				return
			fi

			x:=b.value
			dispsize:=1
			if a.size=1 then
				opc:=0x80
!				if x not in -128..127 then axerror("Exceeding i8 range") fi
				checkimmrange(x,1)
				if x not in -128..255 then axerror("Exceeding i8/u8 range") fi
			elsif -128<=x<=127 then
				opc:=0x83
			else
				checkimmrange(x,4)
				opc:=0x81
				dispsize:=(a.size=2|2|4)
			fi

			genxrm(opc, code, a)

			case dispsize
			when 1 then genbyte(x)
			when 2 then genword(x)
			when 4 then gendword(x)
			esac
			fixrip(dispsize)

		else
			axerror("ADD reg,???")
		esac

	when a_mem then
		case b.mode
		when a_reg then
			opc:=code<<3 ior (b.size=1|0x00|0x01)
			genrrm(opc, b, a)

		when a_imm then
			goto doregimm
		else
			axerror("ADD mem,???")
		esac

	else
	cpl opndnames_ma[code],=CODE
		axerror("1:Can't add to this opnd")
	esac
end

proc do_mov(mclopnd a,b)=
	int regcode, opc, dispsize
	int64 value
	psymbol d:=getdef(b)

	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			if a.size<>b.size and b.size then
CPL CURRASMPROC.NAME
CPL =A.SIZE, =B.SIZE
 axerror("2:Opnd size mismatch") fi
			genrrm((a.size=1|0x8A|0x8B), a, b)
		when a_imm then
			value:=b.value

			regcode:=getregcode(a.reg, bmask)
			setopsize(a)
			if d and a.size<=2 then axerror("mov imm?") fi

			case a.size
			when 1 then
				unless -128<=value<=255 then axerror("exceeding byte value") end
				genrex()
				genbyte(0xB0+regcode)
				genbyte(value)

			when 2 then
				unless -32768<=value<=65535 then axerror("exceeding word16 value") end
				genbyte(0x66)
				genrex()
				genbyte(0xB8+regcode)
				genword(value)
			when 4 then
				if d then
					genrex()
					genbyte(0xB8+regcode)
					genopnd(b,4)
				else
					unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
						CPL value,ref void(value)
						axerror("1:exceeding word32 value")
					end
doreg32:
					genrex()
					genbyte(0xB8+regcode)
					gendword(value)
				fi

			else							!assum 8 bytes
				if d then
					rex ior:=wmask
					genrex()
					genbyte(0xB8+regcode)
					genopnd(b,8)
				else
					if value>=0 and value<=0xFFFF'FFFF then		!mov r64,imm -> r32,imm
						rex.[wbit]:=0
						goto doreg32			!load 32-bit value which is zero-extended to 64
					fi
!there might be short form for negative values that fit into 32 bits, using other opcs
!but ignore that for now
					rex ior:=wmask
					genrex()
					genbyte(0xB8+regcode)
					genqword(value)
				fi

			esac

		else
			axerror("MOV REG/??")
		esac
	when a_mem then
		case b.mode
		when a_reg then
			if a.size=0 then a.size:=b.size fi
			if a.size<>b.size and a.size then axerror("3:Opnd size mismatch") fi
			genrrm((b.size=1|0x88|0x89), b, a)

		when a_imm then
			value:=b.value

			if a.size=0 then a.size:=1 fi
			if d and a.size<=2 then axerror("mov imm?") fi
			setopsize(a)
			opc:=(a.size=1|0xC6|0xC7)

			if not d then checkimmrange(value, a.size) fi

			genxrm(opc, 0, a)
			value:=b.value

			dispsize:=a.size
			case a.size
			when 1 then
				genbyte(value)
	
			when 2 then
				genword(value)
			when 4,8 then
				genopnd(b,4)
				dispsize:=4
			esac
			fixrip(dispsize)	
		else
			axerror("MOV MEM/?")
		esac
	else
		axerror("MOV ?/..")
	esac
end

proc do_push(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("pushreg not 64-bit") fi
		code:=getregcode(a.reg, bmask)
		rex.[wbit]:=0
		genrex()
		genbyte(0x50+code)

	when a_imm then
		if getdef(a) then
			genbyte(0x68)
			genopnd(a,4)
		elsif isbytesized(a.value) then
			genbyte(0x6A)
			genbyte(a.value)
		elsif isdwordsized(a.value) then
			genbyte(0x68)
			gendword(a.value)
		else
			axerror("push imm value too large")
		fi

	when a_mem then
		if a.size<>8 then axerror("push not 64-bit") fi
		genxrm(0xFF, 6, a)

	else
		axerror("push opnd?")
	esac
end

proc do_pop(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("popreg not 64-bit") fi
		code:=getregcode(a.reg, bmask)
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a.size<>8 then axerror("pop not 64-bit") fi
		genxrm(0x8F, 0, a)
	else
		axerror("pop opnd?")
	esac
end

proc do_inc(mclopnd a,int code)=
!inc/dec

	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xFE|0xFF), code, a)
	else
		axerror("inc/opnd?")
	esac
end

proc do_neg(mclopnd a,int code)=
!neg/not/mul/imul/div/idiv
	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xF6|0xF7), code, a)
	else
		axerror("neg/div/etc opnd?")
	esac
end

proc do_lea(mclopnd a,b)=
	int regcode, am

	unless a.mode=a_reg and b.mode=a_mem then
		axerror("LEA not reg/mem")
	end

	if a.size<4 then axerror("LEA size error") fi

	genrrm(0x8D, a, b)
end

proc do_movsx(mclopnd a,b,int opc)=
!opc=B6 for movzx, and BE for movsx
	int am, regcode

	if a.mode<>a_reg then axerror("movsx not reg") fi

	if a.size=8 and b.size=4 then
		if opc=0xBE then
			do_movsxd(a,b)
		else						!movsx 4->8 bytes, do normal move 4->4
			a:=regtable[a.reg,4]
			do_mov(a,b)
		fi
		return
	fi

	if a.size=1 or a.size<=b.size then axerror("movsx size error") fi
	if opc=0xB6 and b.size=4 then axerror("movsx 4=>8 bytes?") fi

	case b.mode
	when a_reg then
	when a_mem then
		if b.size=0 then axerror("movsx need size prefix") fi
		if b.size=8 then axerror("movsx size 8") fi
	else
		axerror("movsx not reg/mem")
	esac

	genrrm(0x0F<<8+(b.size=1|opc|opc+1), a, b)
end

proc do_exch(mclopnd a,b)=
	int regcode, am

	if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then		!simple r0/reg
		if a.reg<>r0 then				!get a to be r0
			swap(a,b)
		fi
		if a.size<>b.size then axerror("exch size") fi

		setopsize(a)
		regcode:=getregcode(b.reg, bmask)
		genrex()
		genbyte(0x90+regcode)
		return
	fi

	if a.mode=a_mem then swap(a,b) fi

	unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then axerror("exch opnds") end
	if b.size=0 and b.mode=a_mem then b.size:=a.size fi
	if a.size<>b.size then axerror("exch size") fi

	genrrm((a.size=1|0x86|0x87), a, b)
end

proc do_movsxd(mclopnd a,b)=
	int regcode, am

	if b.mode=a_mem and b.size=0 then b.size:=4 fi

	if a.size<>8 or b.size>4 then axerror("movsxd size") fi

	if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("movsxd opnds")
	fi

	genrrm(0x63, a, b)
end

proc do_imul2(mclopnd a,b)=
	int regcode, am, opc, dispsize
	int64 value

	if a.mode<>a_reg then
		axerror("imul2 opnds")
	fi
	if b.size=0 then b.size:=a.size fi
	if a.size=1 then axerror("imul2 byte") fi

	case b.mode
	when a_reg,a_mem then
		if a.size<>b.size then axerror("imul2 size") fi

		genrrm(0x0F'AF, a, b)

	when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
		if getdef(b) then axerror("mul/label") fi
		value:=b.value

		if -128<=value<=127 then
			opc:=0x6B
		else
			opc:=0x69
		fi

		genrrm(opc, a, a)

		if -128<=value<=127 then
			genbyte(value)
			dispsize:=1
		elsif a.size=2 then
			genword(value)
			dispsize:=2
		else
			gendword(value)
			dispsize:=4
		fi
		fixrip(dispsize)
	else
		axerror("imul2 opnds")
	esac
end

proc do_shift(mclopnd a,b,int code)=
	int w,opc,needdisp

	if a.mode<>a_reg and a.mode<>a_mem then axerror("shift opnds1?") fi
	if getdef(b) then axerror("shift/label") fi
	w:=(a.size=1|0|1)
	needdisp:=0

	case b.mode
	when a_imm then
		if b.value=1 then
			opc:=0xD0+w
		else
			opc:=0xC0+w
			needdisp:=1
		fi
	when a_reg then
		if b.reg<>r10 or b.size<>1 then axerror("cl or b10 needed") fi
		opc:=0xD2+w
	else
		axerror("shift opnds2?")
	esac

	genxrm(opc, code, a)

	if needdisp then genbyte(b.value); fixrip(1) fi
end

proc do_test(mclopnd a,b)=
	int64 value
	int opc, am, regcode

	if a.mode=a_reg and a.reg=r0 and b.mode=a_imm then
		value:=b.value
		case a.size
		when 1 then
			genbyte(0xA8)
			genbyte(value)
		when 2 then
			genbyte(0x66)
			genbyte(0xA9)
			genword(value)
		when 4 then
			genbyte(0xA9)
			gendword(value)
		else
			genbyte(0x48)
			genbyte(0xA9)
			gendword(value)
		esac

	elsif (a.mode=a_reg or a.mode=a_mem) and b.mode=a_imm then
		genxrm((a.size=1|0xF6|0xF7), 0, a)

		case a.size
		when 1 then
			genbyte(value)
		when 2 then
			genword(value)
		else
			gendword(value)
		esac
		fixrip(a.size)

	elsif a.mode in [a_reg, a_mem] and b.mode=a_reg then
domemreg:
		genrrm((a.size=1|0x84|0x85), a, b)

	elsif a.mode=a_reg and b.mode=a_mem then
		swap(a,b)
		goto domemreg
	else
		axerror("test opnds")
	fi

end

proc do_setcc(int cond, mclopnd b)=
!a is cond
!b is byte reg/mem

	if b.mode not in [a_reg, a_mem] or b.size>1 then axerror("setcc opnd/size") fi

	genxrm(0x0F'90+cond, 0, b)
end

proc checksize(mclopnd a, int size1=0, size2=0)=
	if a.size=0 then axerror("Need size") fi
	if size1 and a.size not in [size1,size2] then
		CPL =A.SIZE
		axerror("Wrong size")
	fi
end

proc do_arithxmm(mclopnd a,b,int prefix,opc)=
	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("arithxmm opnds")
	fi

	if prefix then genbyte(prefix) fi
	genrrm(0x0F<<8+opc, a, b)
end

proc do_logicxmm(mclopnd a,b,int opc,size)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("logicxmm opnds")
	fi

	if size=8 then genbyte(0x66) fi

	genrrm(0x0F<<8+opc, a, b)
end

proc do_convertfloat(mclopnd a,b,int prefix)=
!cvtss2sd and cvtsd2ss
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("convertfloat opnds")
	fi
	genbyte(prefix)
	nowmask:=1
	genrrm(0x0F'5A, a,b)
end

proc do_fix(mclopnd a,b,int prefix,opc)=
	int am, regcode

	if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("fix opnds")
	fi

	checksize(a, 4, 8)
	
	b.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	genrrm(0x0F<<8+opc, a, b)
end

proc do_float(mclopnd a,b,int prefix)=
!cvtss2si and cvtsd2si
	if a.mode<>a_xreg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("float opnds")
	fi

	checksize(b, 4, 8)
!
	a.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	usesizeb:=1
	genrrm(0x0F'2A, a, b)
end

proc do_movxmm(mclopnd a,b,int size)=
!do movd/movq depending on size being 4 or 8
	int am, regcode, regcode1, regcode2

	case a.mode
	when a_reg then
		case b.mode
		when a_xreg then
			if a.size<>size then axerror("1:movdq size") fi
			b.size:=a.size

			sizeoverride:=1
			genrrm(0x0F'7E, b, a)

		else
			axerror("movdq reg,?")
		esac
	when a_xreg then
		case b.mode
		when a_reg then
			a.size:=b.size
			if b.size<>size then axerror("3:movdq size") fi
			sizeoverride:=1
			genrrm(0x0F'6E, a, b)

		when a_xreg then
			a.size:=b.size
			f3override:=1
			genrrm(0x0F'7E, a, b)

		when a_mem then
			if b.size=0 then b.size:=a.size fi
!			if b.size<>size then axerror("31:movdq size") fi
			if b.size<>size then axerror("31:movdq size") fi
!
			if size=4 then
				sizeoverride:=1
				nowmask:=1
				genrrm(0x0F'6E, a, b)

			else
				f3override:=1
				nowmask:=1
				genrrm(0x0F'7E, a, b)
			fi

		else
			axerror("movdq xreg,?")
		esac
	when a_mem then
		case b.mode
		when a_xreg then
			if a.size and a.size<>size then axerror("5:movdq size") fi

			sizeoverride:=1
			genrrm((size=4|0x0F'7E|0x0F'D6), b,a)

		else
			axerror("movdq mem,?")
		esac

	else
		axerror("movdq opnds")
	esac
end

proc fixrip(int dispsize)=
	ref byte codeaddr
	ref u32 offsetptr

	if not ripentry then return fi

	case dispsize
	when 0 then return
	when 1,2,4 then
	else
CPL =DISPSIZE
		axerror("fixrip disp?")
	esac
	ripentry.immsize:=dispsize
end

proc do_bswap(mclopnd a)=
	int code
	if a.mode<>a_reg or a.size<4 then axerror("bswap reg>") fi

	setopsize(a)

	code:=getregcode(a.reg, bmask)

	genrex()
	genbyte(0x0F)
	genbyte(0xC8 + code)
end

proc do_movdqx(mclopnd a, b, int prefix)=
	prefix:=prefix<<16 + 0x0F<<8

	if a.size=0 then a.size:=16 fi
	if b.size=0 then b.size:=16 fi

!	if a.size=0 then a.size:=8 fi
!	if b.size=0 then b.size:=8 fi

	if a.mode=a_mem then
		genrrm(prefix+0x7F, b, a)
	else
		genrrm(prefix+0x6F, a, b)
	fi
end

proc do_dshift(mclopnd a, b, int c, opc)=

	if a.size=0 then a.size:=b.size fi
	if a.size<>b.size or a.size<=1 then axerror("dshift/size") fi

	sizeoverride:=0
	genrrm(0x0F<<8+opc, b, a)
	genbyte(c)
end

=== mc_decls.m 0 0 14/41 ===
export type mclopnd = ref mclopndrec

export record mclopndrec =
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		psymbol def
		int64 value		!immediate value
		real64 xvalue	!immediate real value, mainly for dq
		ichar svalue	!immediate string
		int labelno
		int sysfn
		int tempno
	end

!	byte size			!byte size of operand: usually 1,2,4,8,16
!	byte mode			!a_reg etc, low level operand details
!	byte reg			!0, or main register
!	byte regix			!0, or index register
!
!	byte valtype		!interpretation of def/code/value/svalue
!	byte scale			!1, or scale factor for regix
!	int32 offset		!extra offset to label for mem/imm modes

	u16 misc: (			! bitfields
		size:5,		! one of 1 2 4 8
		scale:4,		! one of 1 2 4 8
		mode:3,			! R, X, imm, [mem]
		valtype:4)

!	BYTE MODE
!	BYTE SIZE
!	BYTE SCALE
!	BYTE VALTYPE

	byte reg			!0, or main register
	byte regix			!0, or index register
	i32 offset			!additional offset to memory operands
!	

end

export record mclrec =
	ref mclrec nextmcl
	mclopnd a,b
	byte opcode
	union
		byte cond
		byte isglobal
		byte sysindex
	end
!	byte fileno
	byte c
!	byte spare1, spare2
	int seqno
	u32 mpos
!	int xxpos:(sourceoffset:24, fileno:8)
!	ichar xcomment
	[r0..r15]byte regend		!1 indicates register freed.
end

global enumdata [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(intimm_val,	$),		!immediate int
	(realimm_val,	$),		!immediate real (mainly for dq etc)
	(realmem_val,	$),		!indirect real (for movq etc)
	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
	(def_val,		$),		!var/proc name
	(label_val,		$),		!label index
!	(labelind_val,	$),		!label index
	(name_val,		$),		!immediate string must be output as ah unquoted name
	(temp_val,		$),		!index of pclopnd temp (later becomes ptr to descriptor?)
!	(syscall_val,	$),		!
end

export enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_procstart,		$,		0,		0),		!
	(m_procend,			$,		0,		0),		!
	(m_comment,			$,		0,		0),		!
	(m_blank,			$,		0,		0),		!
	(m_deleted,			$,		0,		0),		!
	(m_labelname,		$,		0,		0),		!
	(m_define,			$,		0,		0),		!
	(m_definereg,		$,		0,		0),		!

	(m_labelx,			$,		1,		0),		!
	(m_nop,				$,		0,		0x90),		!
	(m_param,			$,		1,		0),		!
!	(m_assembly,		$,		1,		0),		!
!	(m_proc,			$,		1,		0),		!

	(m_mov,				$,		2,		0),		!
	(m_push,			$,		1,		0),		!
	(m_pop,				$,		1,		0),		!
	(m_lea,				$,		2,		0),		!
	(m_cmovcc,			$,		2,		0),		!

	(m_movd,			$,		2,		0),		!
	(m_movq,			$,		2,		0),		!

	(m_movsx,			$,		2,		0),		!
	(m_movzx,			$,		2,		0),		!
	(m_movsxd,			$,		2,		0),		!

	(m_call,			$,		1,		0xE8),		!
	(m_ret,				$,		0,		0xC3),	!
	(m_leave,			$,		0,		0xC9),	!
	(m_retn,			$,		1,		0),		!

	(m_jmp,				$,		1,		0xE9),	!
	(m_jmpcc,			$,		2,		0),		!
	(m_xchg,			$,		2,		0),		!

	(m_add,				$,		2,		0),		!
	(m_sub,				$,		2,		5),		!
	(m_adc,				$,		2,		2),		!
	(m_sbb,				$,		2,		3),		!
	(m_imul,			$,		1,		5),		!
	(m_mul,				$,		1,		4),		!
	(m_imul2,			$,		2,		0),		!
	(m_imul3,			$,		3,		0),		!

	(m_idiv,			$,		1,		7),		!
	(m_div,				$,		1,		6),		!

	(m_andx,			$,		2,		0x04),	!
	(m_orx,				$,		2,		0x01),	!
	(m_xorx,			$,		2,		0x06),	!
	(m_test,			$,		2,		0),		!

	(m_cmp,				$,		2,		0x07),	!

	(m_shl,				$,		2,		0x04),	!
	(m_sar,				$,		2,		0x07),	!
	(m_shr,				$,		2,		0x05),	!
	(m_rol,				$,		2,		0x00),	!
	(m_ror,				$,		2,		0x01),	!
	(m_rcl,				$,		2,		0x02),	!
	(m_rcr,				$,		2,		0x03),	!

	(m_neg,				$,		1,		3),		!
	(m_notx,			$,		1,		2),		!

	(m_inc,				$,		1,		0),		!
	(m_dec,				$,		1,		1),		!

	(m_cbw,				$,		0,		0),	!
	(m_cwd,				$,		0,		0),	!
	(m_cdq,				$,		0,		0),		!
	(m_cqo,				$,		0,		0),		!
	(m_setcc,			$,		2,		0),		!

	(m_bsf,				$,		2,		0xBC),	!
	(m_bsr,				$,		2,		0xBD),	!

	(m_shld,			$,		2,		0xA4),	!
	(m_shrd,			$,		2,		0xAC),	!

	(m_sqrtss,			$,		2,		0x51),	!
	(m_sqrtsd,			$,		2,		0x51),	!

	(m_addss,			$,		2,		0x58),	!
	(m_addsd,			$,		2,		0x58),	!

	(m_subss,			$,		2,		0x5C),	!
	(m_subsd,			$,		2,		0x5C),	!

	(m_mulss,			$,		2,		0x59),	!
	(m_mulsd,			$,		2,		0x59),	!

	(m_divss,			$,		2,		0x5E),	!
	(m_divsd,			$,		2,		0x5E),	!

	(m_comiss,			$,		2,		0),		!
	(m_comisd,			$,		2,		0),		!

	(m_xorps,			$,		2,		0x57),	!
	(m_xorpd,			$,		2,		0x57),	!

	(m_andps,			$,		2,		0x54),	!
	(m_andpd,			$,		2,		0x54),	!

	(m_pxor,			$,		2,		0xEF),	!
	(m_pand,			$,		2,		0xDB),	!

	(m_cvtss2si,		$,		2,		0),		!
	(m_cvtsd2si,		$,		2,		0),		!

	(m_cvttss2si,		$,		2,		0),		!
	(m_cvttsd2si,		$,		2,		0),		!

	(m_cvtsi2ss,		$,		2,		0),		!
	(m_cvtsi2sd,		$,		2,		0),		!

	(m_cvtsd2ss,		$,		2,		0),		!
	(m_cvtss2sd,		$,		2,		0),		!

	(m_movdqa,			$,		2,		0x66),	!
	(m_movdqu,			$,		2,		0xF3),	!

	(m_pcmpistri,		$,		3,		0x63),	!
	(m_pcmpistrm,		$,		3,		0x62),	!

	(m_fld,				$,		1,		0),		!
	(m_fst,				$,		1,		2),		!
	(m_fstp,			$,		1,		3),		!

	(m_fild,			$,		1,		0),		!
	(m_fist,			$,		1,		2),		!
	(m_fistp,			$,		1,		3),		!

	(m_fadd,			$,		0,		0xC1),	!
	(m_fsub,			$,		0,		0xE9),	!
	(m_fmul,			$,		0,		0xC9),	!
	(m_fdiv,			$,		0,		0xF9),	!
	(m_fsqrt,			$,		0,		0xFA),	!
	(m_fsin,			$,		0,		0xFE),	!
	(m_fcos,			$,		0,		0xFF),	!
	(m_fsincos,			$,		0,		0xFB),	!
	(m_fptan,			$,		0,		0xF2),	!
	(m_fpatan,			$,		0,		0xF3),	!
	(m_fabs,			$,		0,		0xE1),	!
	(m_fchs,			$,		0,		0xE0),	!

	(m_minss,			$,		2,		0x5D),	!
	(m_maxss,			$,		2,		0x5F),	!
	(m_minsd,			$,		2,		0x5D),	!
	(m_maxsd,			$,		2,		0x5F),	!

	(m_db,				$,		1,		0),		!
	(m_dw,				$,		1,		0),		!
	(m_dd,				$,		1,		0),		!
	(m_dq,				$,		1,		0),		!
!	(m_ddoffset,		$,		1,		0),		!

!	(m_segment,			$,		1,		0),		!
	(m_isegment,		$,		0,		0),		!
	(m_zsegment,		$,		0,		0),		!
	(m_csegment,		$,		0,		0),		!

	(m_align,			$,		1,		0),		!
	(m_resb,			$,		1,		1),		!
	(m_resw,			$,		1,		2),		!
	(m_resd,			$,		1,		4),		!
	(m_resq,			$,		1,		8),		!

	(m_xlat,			$,		0,		0xD7),	!
	(m_loopnz,			$,		1,		0xE0),	!
	(m_loopz,			$,		1,		0xE1),	!
	(m_loopcx,			$,		1,		0xE2),	!
	(m_jecxz,			$,		1,		0xE3),	!
	(m_jrcxz,			$,		1,		0xE3),	!

	(m_cmpsb,			$,		0,		0),		!
	(m_cmpsw,			$,		0,		0),		!
	(m_cmpsd,			$,		0,		0),		!
	(m_cmpsq,			$,		0,		0),		!

	(m_rdtsc,			$,		0,		0x31),	!
	(m_popcnt,			$,		2,		0),		!
	(m_bswap,			$,		1,		0),		!

	(m_finit,			$,		0,		0),		!

	(m_fldz,			$,		0,		0xEE),	!
	(m_fld1,			$,		0,		0xE8),	!
	(m_fldpi,			$,		0,		0xEB),	!
	(m_fld2t,			$,		0,		0xE9),	!
	(m_fld2e,			$,		0,		0xEA),	!
	(m_fldlg2,			$,		0,		0xEC),	!
	(m_fldln2,			$,		0,		0xED),	!

	(m_cpuid,			$,		0,		0),		!

	(m_halt,			$,		0,		0xF4),	!
end

export enumdata [0:]ichar regnames, [0:]byte regcodes =
	(rnone=0,	$,	0),			!
	(r0,		$,	0),			!d0 rax
	(r1,		$,	10),		!d1 r10
	(r2,		$,	11),		!d2 r11
	(r3,		$,	7),			!d3 rdi
	(r4,		$,	3),			!d4 rbx
	(r5,		$,	6),			!d5 rsi
	(r6,		$,	12),		!d6 r12
	(r7,		$,	13),		!d7 r13
	(r8,		$,	14),		!d8 r14
	(r9,		$,	15),		!d9 r15
	(r10,		$,	1),			!d10 rcx
	(r11,		$,	2),			!d11 rdx
	(r12,		$,	8),			!d12 r8
	(r13,		$,	9),			!d13 r9
	(r14,		$,	5),			!d14 rbp
	(r15,		$,	4),			!d15 rsp

	(r16,		$,	4),			!b0h ah
	(r17,		$,	7),			!b1h bh
	(r18,		$,	5),			!b10h ch
	(r19,		$,	6),			!b11h dh
end

export const rframe = r14
export const rstack = r15

global enumdata [0:]ichar condnames, [0:]ichar asmcondnames,
		[0:]int asmrevcond =

	(ov_cond=0,	"ov",	"o",		nov_cond),
	(nov_cond,	"nov",	"no",		ov_cond),

	(ltu_cond,	"ltu",	"b",		geu_cond),
	(geu_cond,	"geu",	"ae",		ltu_cond),

	(eq_cond,	"eq",	"z",		ne_cond),
	(ne_cond,	"ne",	"nz",		eq_cond),

	(leu_cond,	"leu",	"be",		gtu_cond),
	(gtu_cond,	"gtu",	"a",		leu_cond),

	(s_cond,	"s",	"s",		ns_cond),
	(ns_cond,	"ns",	"ns",		s_cond),

	(p_cond,	"p",	"p",		np_cond),
	(np_cond,	"np",	"np",		p_cond),

	(lt_cond,	"lt",	"l",		ge_cond),
	(ge_cond,	"ge",	"ge",		lt_cond),

	(le_cond,	"le",	"le",		gt_cond),
	(gt_cond,	"gt",	"g",		le_cond),

	(flt_cond,	"flt",	"b",		fge_cond),		!special floating point codes
	(fge_cond,	"fge",	"ae",		flt_cond),
	(fle_cond,	"fle",	"be",		fgt_cond),
	(fgt_cond,	"fgt",	"a",		fle_cond)
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
	("d0",		8,	r0),		!rax	d0..d9 are for general use
	("d1",		8,	r1),		!r10	d0..d2 are volatile in ABI
	("d2",		8,	r2),		!r11

	("d3",		8,	r3),		!rdi	d3..d9 are preserved across funcs in ABI
	("d4",		8,	r4),		!rbx
	("d5",		8,	r5),		!rsi
	("d6",		8,	r6),		!r12
	("d7",		8,	r7),		!r13
	("d8",		8,	r8),		!r14
	("d9",		8,	r9),		!r15

	("d10",		8,	r10),		!rcx	d10..d13 are win64 ABI register passing regs
	("d11",		8,	r11),		!rdx	..
	("d12",		8,	r12),		!r8		..
	("d13",		8,	r13),		!r9		..

	("d14",		8,	r14),		!rbp	frame pointer
	("d15",		8,  r15),		!rsp	stack pointer

	("a0",		4,	r0),
	("a1",		4,	r1),
	("a2",		4,	r2),
	("a3",		4,	r3),
	("a4",		4,	r4),
	("a5",		4,	r5),
	("a6",		4,	r6),
	("a7",		4,	r7),
	("a8",		4,	r8),
	("a9",		4,	r9),
	("a10",		4,	r10),
	("a11",		4,	r11),
	("a12",		4,	r12),
	("a13",		4,	r13),
	("a14",		4,	r14),
	("a15",		4,  r15),

	("w0",		2,	r0),
	("w1",		2,	r1),
	("w2",		2,	r2),
	("w3",		2,	r3),
	("w4",		2,	r4),
	("w5",		2,	r5),
	("w6",		2,	r6),
	("w7",		2,	r7),
	("w8",		2,	r8),
	("w9",		2,	r9),
	("w10",		2,	r10),
	("w11",		2,	r11),
	("w12",		2,	r12),
	("w13",		2,	r13),
	("w14",		2,	r14),
	("w15",		2,  r15),


	("b0",		1,	r0),
	("b1",		1,	r1),
	("b2",		1,	r2),
	("b3",		1,	r3),
	("b4",		1,	r4),
	("b5",		1,	r5),
	("b6",		1,	r6),
	("b7",		1,	r7),
	("b8",		1,	r8),
	("b9",		1,	r9),
	("b10",		1,	r10),
	("b11",		1,	r11),
	("b12",		1,	r12),
	("b13",		1,	r13),
	("b14",		1,	r14),
	("b15",		1,  r15),
	("b16",		1,  r16),
	("b17",		1,  r17),
	("b18",		1,  r18),
	("b19",		1,  r19),

	("_b20",	0,  0),			!dummy entry marks start of official names

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

export []ichar xmmregnames = (
	"xmm0",
	"xmm1",
	"xmm2",
	"xmm3",
	"xmm4",
	"xmm5",
	"xmm6",
	"xmm7",
	"xmm8",
	"xmm9",
	"xmm10",
	"xmm11",
	"xmm12",
	"xmm13",
	"xmm14",
	"xmm15")

export []ichar fregnames = (
	"st0",
	"st1",
	"st2",
	"st3",
	"st4",
	"st5",
	"st6",
	"st7")

export []ichar mregnames = (
	"mmx0",
	"mmx1",
	"mmx2",
	"mmx3",
	"mmx4",
	"mmx5",
	"mmx6",
	"mmx7")

!global enumdata [0:]ichar condnames =
!
!	(ov_cond	= 0,	"o"),
!	(nov_cond	= 1,	"no"),
!
!	(ltu_cond	= 2,	"b"),
!	(geu_cond	= 3,	"ae"),
!
!	(eq_cond	= 4,	"z"),
!	(ne_cond	= 5,	"nz"),
!
!	(leu_cond	= 6,	"be"),
!	(gtu_cond	= 7,	"a"),
!
!	(s_cond		= 8,	"s"),
!	(ns_cond	= 9,	"ns"),
!
!	(p_cond		= 10,	"p"),
!	(np_cond	= 11,	"np"),
!
!	(lt_cond	= 12,	"l"),
!	(ge_cond	= 13,	"ge"),
!
!	(le_cond	= 14,	"le"),
!	(gt_cond	= 15,	"g"),
!end

export tabledata []ichar jmpccnames, []byte jmpcccodes =
	("jo",		ov_cond),
	("jno",		nov_cond),
	("jb",		ltu_cond),
	("jae",		geu_cond),
	("jz",		eq_cond),
	("jnz",		ne_cond),
	("jbe",		leu_cond),
	("ja",		gtu_cond),
	("js",		s_cond),
	("jns",		ns_cond),
	("jp",		p_cond),
	("jnp",		np_cond),
	("jl",		lt_cond),
	("jge",		ge_cond),
	("jle",		le_cond),
	("jg",		gt_cond),
	("jc",		ltu_cond),
	("jnc",		geu_cond),
end


export tabledata []ichar setccnames, []byte setcccodes =
	("seto",	ov_cond),
	("setno",	nov_cond),
	("setb",	ltu_cond),
	("setae",	geu_cond),
	("setz",	eq_cond),
	("setnz",	ne_cond),
	("setbe",	leu_cond),
	("seta",	gtu_cond),
	("sets",	s_cond),
	("setns",	ns_cond),
	("setp",	p_cond),
	("setnp",	np_cond),
	("setl",	lt_cond),
	("setge",	ge_cond),
	("setle",	le_cond),
	("setg",	gt_cond),
end

export tabledata []ichar cmovccnames, []byte cmovcccodes =
	("cmovo",	ov_cond),
	("cmovno",	nov_cond),
	("cmovb",	ltu_cond),
	("cmovae",	geu_cond),
	("cmovz",	eq_cond),
	("cmovnz",	ne_cond),
	("cmovbe",	leu_cond),
	("cmova",	gtu_cond),
	("cmovs",	s_cond),
	("cmovns",	ns_cond),
	("cmovp",	p_cond),
	("cmovnp",	np_cond),
	("cmovl",	lt_cond),
	("cmovge",	ge_cond),
	("cmovle",	le_cond),
	("cmovg",	gt_cond),
end

export enumdata [0:]ichar segmentnames =
	(no_seg=0,		$),
	(code_seg,		$),
	(idata_seg,		$),
	(zdata_seg,		$),
	(rodata_seg,	$),
	(impdata_seg,	$),
end

export enumdata [0:]ichar reftypenames =
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

export enumdata [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
	(a_cond,	$),		! a condition code for jcc/setcc
	(a_xreg,	$),		! xmm register
end

!Categories mcl operands a different way

global enumdata [0:]ichar opndcatnames =
	(no_cat,		$),		! -
	(imm_cat,		$),		! d				Immediate only
	(reg_cat,		$),		! D or X
	(ireg_cat,		$),		! [D]			No displacement
	(mem_cat,		$),		! [d]			Displacement only
	(regmem_cat,	$),		! [R+d] [R+R*s] Any combo that is not ireg or mem
end

export int mlabelno
!global byte foptimise

global const maxoperands=20

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

global enumdata [0:]ichar locnames =
	(pcl_loc=0,	"pend"),				!operand still in pcl instruction
	(reg_loc,	"reg"),					!is in register (look at mode)
	(temp_loc,	"temp"),				!overflow to temporary
end

!global const regmax=r9				!can use r0 to regmax inclusive; only those regs

global const regmax=r4				!can use r0 to regmax inclusive; only those regs
!global const regmax=r5				!can use r0 to regmax inclusive; only those regs

!global const regmax=r3				!can use r0 to regmax inclusive; only those regs
!global const regmax=r2				!can use r0 to regmax inclusive; only those regs

!global const xregmax=xr15
global const xregmax=xr8

!global int regtop					!current highest reg used; 0 means no reg used
!global int xregtop
!
!global int stackworkregs			!no. of regs used as work ones
!global int nworkregs				!no. of param regs used as work ones

global [r0..r15]byte workregs, workxregs		!1 indicates available work regs
global [r0..r15]byte spillregs, spillxregs		!1 means register has been written to

global [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
global [r0..r15]byte xregset		!same for xregs

global [r0..r15]byte isregvar
global [r0..r15]byte isxregvar
!
!These vars give info on the resources used by a proc

!global [r0..r15]byte allregmap		!all regs used
!global [r0..r15]byte allxregmap		!all xregs used

!global int inf_proccalls
!global int inf_proclocals
!global int inf_procxlocals
!
!global int inf_leafproc
!global int inf_highreg
!global int inf_highxreg
!global int inf_mazzrgs
!export int inf_assem
!
!global int inf_r10used		!these may be set in pass2 when occupied by params
!global int inf_r11used
!global int inf_r13used

!global [16]int inf_dsaveregs
!global [16]int inf_xsaveregs
!global int inf_ndsaveregs	!set in procentry; at one or both will be zero
!global int inf_ndsavepush
!global int inf_nxsaveregs
!global int inf_dsaveoffset
!global int inf_xsaveoffset
!
!global [16]int dsaveregs
!global [16]int xsaveregs
!global int ndsaveregs	!set in procentry; at one or both will be zero
!global int ndsavepush
!global int nxsaveregs
!global int dsaveoffset
!global int xsaveoffset
!global int needstackframe
!global int framebytes
!global int needshadow48
!global int needshadow32		!has value 0, 32 or 40, the actual spaced needed
!
global byte noxorclear		!1 to suppress xor optimisation
!

!global const wd = 4
!global const xx = 3
!global const yy = 2
!global const zz = 1
!
!global const yy = 2
!global const zz = 1
!
!global const zz = 1

!global macro wd = noperands-3
!global macro xx = noperands-2
!global macro yy = noperands-1
!global macro zz = noperands
!
!global macro yy = noperands-1
!global macro zz = noperands
!
!global macro zz = noperands

global macro zz = noperands
global macro yy = noperands-1
global macro xx = noperands-2
global macro ww = noperands-3

global enumdata [0:]ichar xregnames =
	(xnone=0,	$),
	(xr0,		$),
	(xr1,		$),
	(xr2,		$),
	(xr3,		$),
	(xr4,		$),
	(xr5,		$),
	(xr6,		$),
	(xr7,		$),
	(xr8,		$),
	(xr9,		$),
	(xr10,		$),
	(xr11,		$),
	(xr12,		$),
	(xr13,		$),
	(xr14,		$),
	(xr15,		$)
end

!global pcl procdefpcl
!global symbol procdef

global const maxcalldepth=16
global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret	!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize	!size of any returned block
global [maxcalldepth,4]u32 callargsize	!size of any block pushed in low args
global int ncalldepth

!global const maxparams=32
!global const maxlocals=256
!
!!these are reset at each procdef
!global [maxparams]symbol paramdefs
!global [maxlocals]symbol localdefs
!global int nparams, nlocals

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

export ref mclrec mccode, mccodex		!genmc adds to this linked list

global int currsegment=0

!global int frameoffset
!global int paramoffset

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
end

global ref constrec cstringlist
global ref constrec vstringlist
global ref constrec creallist
global ref constrec creal32list

global psymbol currasmproc

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

global record relocrec =			!informal version
	ref relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
global record dbuffer =
	ref byte pstart
	union
		ref byte pcurr
		ref word16 pcurr16
		ref word32 pcurr32
		ref word64 pcurr64
	end
	ref byte pend
	int alloc
end

global int ss_zdatalen
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

!const max_ss_symbols=32768				!exported to coff
global const init_ss_symbols=32768				!exported to coff
!global const init_ss_symbols=16384
global ref []psymbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref[]psymbol labeldeftable

global int aaseqno

!export enumdata []ichar segmentnames =
!	(code_seg,		"code"),
!	(idata_seg,		"idata"),
!	(zdata_seg,		"zdata"),
!	(rodata_seg,	"rodata"),
!	(impdata_seg,	$),
!end

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
global ref mclrec mce_oldmccodex, mce_nextmcl		!used by reset/setmclentry

global byte fppeephole
global byte fpregoptim
global byte fpshortnames
global byte fpcheckunusedlocals
export byte phighmem

global record riprec =
	ref riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

!global record fwdrec =
!	ref fwdrec nextfwd
!	int32 offset
!	int16 reltype
!	int16 seg
!end

global ref riprec riplist

export ref proc (ref void) idomcl_assem
export ref func (int)psymbol igethostfn

global const maxblocktemps=50
global [maxblocktemps]psymbol blockdefs
global int nblocktemps

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)


global [pstdnames.bounds]byte ploadopx

!proc start=
!	for i in ploadopx.bounds do ploadopx[i]:=m_nop od
!
!	ploadopx[tpu8]:=ploadopx[tpu16]:=ploadopx[tpu32]:=m_movzx
!	ploadopx[tpi8]:=ploadopx[tpi16]:=ploadopx[tpi32]:=m_movsx
!	ploadopx[tpr32]:=m_movd
!	ploadopx[tpr64]:=m_movq
!	ploadopx[tpu64]:=ploadopx[tpi64]:=m_mov
!
!!CPL "DONE MC_DECLS/START",PLOADOPX.LWB,PLOADOPX.UPB
!!FOR I IN PLOADOP.BOUNDS DO
!!	CPL I, PSTDNAMES[I],MCLNAMES[PLOADOP[I]], MCLNAMES[PLOADOPX[I]]
!!OD
!end

global [pstdnames.bounds]byte ploadop

proc start=
	for i in ploadop.bounds do ploadop[i]:=m_nop od

	ploadop[tpu8]:=ploadop[tpu16]:=ploadop[tpu32]:=m_movzx
	ploadop[tpi8]:=ploadop[tpi16]:=ploadop[tpi32]:=m_movsx
	ploadop[tpr32]:=m_movd
	ploadop[tpr64]:=m_movq
	ploadop[tpu64]:=ploadop[tpi64]:=m_mov

!CPL "DONE MC_DECLS/START",PLOADOPX.LWB,PLOADOPX.UPB
!FOR I IN PLOADOP.BOUNDS DO
!	CPL I, PSTDNAMES[I],MCLNAMES[PLOADOP[I]], MCLNAMES[PLOADOPX[I]]
!OD
end

=== mc_objdecls.m 0 0 15/41 ===
global record imagefileheader =
	word16	machine
	word16	nsections
	word32	timedatestamp
	word32	symtaboffset
	word32	nsymbols
	word16	optheadersize
	word16	characteristics
end

global record imagedir =
	word32	virtualaddr
	word32	size
end

global record optionalheader =			!exe/dll only
	word16  magic
	byte     majorlv
	byte     minorlv
	word32 codesize
	word32 idatasize
	word32 zdatasize
	word32 entrypoint
	word32 codebase
!	word32 datebase		!32-bit exe files only
	word64	imagebase
	word32 sectionalignment
	word32 filealignment
	word16  majorosv
	word16  minorosv
	word16  majorimagev
	word16  minorimagev
	word16  majorssv
	word16  minorssv
	word32 win32version
	word32 imagesize
	word32 headerssize
	word32 checksum
	word16  subsystem
	word16  dllcharacteristics
	word64   stackreserve
	word64   stackcommit
	word64   heapreserve
	word64   heapcommit
	word32 loaderflags
	word32 rvadims
	imagedir exporttable
	imagedir importtable
	imagedir resourcetable
	imagedir exceptiontable
	imagedir certtable
	imagedir basereloctable
	imagedir debug
	imagedir architecture
	imagedir globalptr
	imagedir tlstable
	imagedir loadconfigtable
	imagedir boundimport
	imagedir iat
	imagedir delayimportdescr
	imagedir clrheader
	imagedir reserved
end

global record imagesectionheader =
	[8]char name
	union
		word32	physical_address
		word32	virtual_size
	end
	word32	virtual_address
	word32	rawdata_size
	word32	rawdata_offset
	word32	relocations_ptr
	word32	linenos_offset
	word16	nrelocs
	word16	nlinenos
	word32	characteristics
end

global record imagesymbol =
	union
		[8]char shortname
		struct
			word32	shortx
			word32	longx
		end
		word64 longname
	end
	word32	value
	int16	sectionno
	word16	symtype
	byte	storageclass
	byte	nauxsymbols
end

global record importdirrec =
	word32	implookuprva
	word32	timedatestamp
	word32	fwdchain
	word32	namerva
	word32	impaddressrva
end

global record coffrelocrec =
	int32	virtualaddr
	int32	stindex
	int16	reloctype
end

global enumdata [0:]ichar relocnames =
	(abs_rel = 0,	$),
	(addr64_rel,	$),
	(addr32_rel,	$),
	(addr32nb_rel,	$),
	(rel32_rel,		$),
	(rel321_rel,	$),
	(rel8_rel,		$),				!used within assembler only, not in coff format
end

global record auxsectionrec = 
	int32 length
	int16 nrelocs
	int16 nlines
	int32 checksum
	int16 sectionno
	int32 dummy
end

global record sectionrec =
	union
		ref dbuffer data		!copy of ss_zdata etc
		ref byte bytedata		!added later, eg, import dir block
	end
	ichar name					!name like ".bss" as it will be in obj/exe file
	int segtype					!code_seg etc
	int rawsize					!in file
	int rawoffset				!offset in exe file
	int virtsize				!in image
	int virtoffset				!offset from imagebase
	ref relocrec relocs			!for idata/code: reloc info needs to be processed
	int nrelocs					!
end

global record importrec = 				!details about all imported symbols
	psymbol def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	psymbol def				!full st entry
	ichar name					!name of symbol (extracted from lib.name if needed)
end

global record dllrec =					!all imported libraries
	ichar name					!name of library, including .dll
	int nprocs					!no. of imports which use this library
	int nametableoffset			!start of name table in impdir
	int addrtableoffset			!start of addr table (IAT)
	int dllnameoffset			!offset of name within impdir
	int dllextraoffset			!offset of mysterious region just before the name
end

global record exportdirrec =
	word32 exportflags
	word32 timedatestamp
	word16 majorversion
	word16 minorversion
	word32 namerva
	word32 ordinalbase
	word32 naddrtable
	word32 nnamepointers
	word32 expaddressrva
	word32 namepointerrva
	word32 ordtablerva
end
=== mc_writeasm.m 0 0 16/41 ===
!export int assemtype='AA'

const fasmformat=1
!const fasmformat=0

proc writemcl(int index,ref mclrec mcl)=

	case mcl.opcode
	when m_deleted then
	else
		strmcl(mcl)
		gs_line(pdest)
	esac
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
!CPL "STRMCL",MCLNAMES[OPCODE],A,B
	comment:=nil

	case opcode
	when m_procstart then
		asmstr(";Proc ")
		asmstr(a.def.name)
		currasmproc:=a.def

		return

	when m_procend then
		asmstr(";End ")
		currasmproc:=nil

		return

	when m_blank then
		return
	when m_comment then
		asmchar(';')
		asmstr(a.svalue)
		GOTO DOCOMMENTS
		return
	when m_deleted then
		asmstr("; <deleted>")
		GOTO DOCOMMENTS
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
			asmstr("\n`")
			asmstr(d.name)
			asmstr("::")
		fi
		return

	when m_labelx then
		fprint @&.str,"L#:",a.value
		asmstr(&.str)
		return

	when m_define then
		asmstr("          ")
		asmstr(a.svalue)
		asmstr(" = ")
		asmopnd(b)
		return

	when m_definereg then
		asmstr("          ")
		asmstr(a.svalue)
		asmstr(" = ")

		case b.mode
		when a_reg then
			asmstr(getregname(b.reg, b.size))
		else
			asmstr(getxregname(b.reg, b.size))
		esac
		return

	esac

	case opcode
	when m_jmpcc then
		print @&.opcname,"j",,asmcondnames[cond]

	when m_setcc then
		print @&.opcname,"set",,asmcondnames[cond]

	when m_cmovcc then
		print @&.opcname,"cmov",,asmcondnames[cond]

!	when m_call then
!		strcpy(&.opcname,"call")
	when m_andx then
		strcpy(&.opcname,"and")
	when m_orx then
		strcpy(&.opcname,"or")
	when m_xorx then
		strcpy(&.opcname,"xor")
	when m_notx then
		strcpy(&.opcname,"not")

	ELSIF OPCODE>M_HALT THEN
		STRCPY(&.OPCNAME,STRINT(OPCODE))

	else
		strcpy(&.opcname,mclnames[opcode]+2)
	esac

!	ipadstr(&.opcname,10," ")
	ipadstr(&.opcname,(opcode=m_dq|4|10)," ")

	if not fasmformat then
		if a and b then
			fprint @&.str,"  #/#",a.size,b.size
		elsif a then
			fprint @&.str,"  #",a.size
		else
			strcpy(&.str,"  ")
		fi
	else
		strcpy(&.str,"  ")
	fi


!	ipadstr(&.str,10)
	ipadstr(&.str,4)

	strcat(&.str,&.opcname)

	asmstr(&.str)
!RETURN

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

		ASMSTR("; ")
		ASMSTR(strint(a.size))
		ASMSTR(" ")
		ASMSTR(strint(b.size))
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

ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO))

DOCOMMENTS:

end

global func strmclstr(ref mclrec m)ichar=
	gs_init(pdest)
	strmcl(m)
	return pdest.strptr
end

global func mstropnd(mclopnd a,int sizeprefix=0,opcode=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus,t
	int offset,tc

	str[1]:=0

!RETURN "<OPND>"
!
!CHECKOPND(A)

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
		if opcode=m_dq and a.valtype=intimm_val then
			if a.value in 0..9 then
				strcat(&.str,strint(a.value))
			else
				strcat(&.str,"0x")
				strcat(&.str,strword(a.value,"H"))
			fi
		else
			strcpy(&.str,strvalue(a))
		fi

	when a_mem then
!RETURN "<MEM>"
		case a.valtype
		when intimm_val then
			strcpy(&.str,strint(a.value))
		when realimm_val then
			strcpy(&.str,strreal(a.xvalue))
		when realmem_val then
			fprint @&.str,"M#",a.xvalue
		esac

		strcat(&.str,getsizeprefix(a.size,sizeprefix))
		strcat(&.str,"[")

		plus:=""
		if a.reg then
			strcat(&.str,strreg(a.reg,8))
			plus:=" + "
		fi
		if a.regix then
			strcat(&.str,plus)
			strcat(&.str,strreg(a.regix,8))
			plus:=" + "

			if a.scale>1 then
				strcat(&.str,"*")
				strcat(&.str,strint(a.scale))
			fi
		fi

		if a.valtype in [def_val,label_val, temp_val] then
			if plus^ then
				strcat(&.str,plus)
			fi
			strcat(&.str,strvalue(a))
	    elsif offset:=a.offset then
			print @&.str2,offset:" + "
			strcat(&.str,&.str2)
		fi
		strcat(&.str,"]")

	when a_xreg then
		return strxreg(a.reg,a.size)

	else
		println "BAD OPND",A.MODE
		return "<BAD OPND>"
	esac

	return &.str
end

global func strvalue(mclopnd a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	int64 value,offset,length
	ichar ss

!RETURN "<STRVAL>"
	def:=a.def
	value:=a.value

	strcpy(&.str,"")

	case a.valtype
	when def_val then
		strcat(&.str,getdispname(def))

	addoffset:
		if offset:=a.offset then
			print @&.str2,(offset>0|"+"|""),,offset
			strcat(&.str,&.str2)
		fi

	when intimm_val then
		strcat(&.str,strint(value))

	when realimm_val then
		print @&.str,a.xvalue:"20.20"

	when realmem_val then
		strcat(&.str,"M")
		strcat(&.str,strreal(a.xvalue))

	when stringimm_val then
		strcat(&.str,"""")
		strcat(&.str,a.svalue)
		strcat(&.str,"""")

	when name_val then
		strcat(&.str,a.svalue)
!		strcat(&.str,"!name")

	when label_val then
		strcat(&.str,"L")
		strcat(&.str,strint(a.labelno))
		goto addoffset

	when temp_val then
!CPL =CURRASMPROC
!RETURN "<TEMP>"

		return gettempname(currasmproc,a.tempno)

	else
		merror("Stropnd?")
	esac

!STRCAT(STR, VALTYPENAMES[A.VALTYPE])
	return &.str

end

global proc asmopnd(mclopnd a,int sizeprefix=0,opcode=0)=
	asmstr(mstropnd(a,sizeprefix,opcode))
end

global func getregname(int reg,size=8)ichar=
	static [1..17]ichar prefix=("B","W","","A","","","","D","","","","","","","","Q","N")
	static [32]char str
	[16]char str2
	ichar rs
	int size2

	size2:=size
	if size2>16 then
		size2:=17
	FI

	case reg
	when rnone then return "-"
	when rframe then rs:="fp"
	when rstack then rs:="sp"
	else
		getstrint(reg-r0,&.str2)
		rs:=&.str2
	esac

	print @&.str,prefix[size2],,rs
	return &.str
end

global func getxregname(int reg,size=8)ichar=
	static [32]char str

	if reg=rnone then return "-" fi

	if fasmformat then
		print @&.str,"XMM",,reg-xr0
	else
		print @&.str,(size=8|"DX"|"SX"),,reg-xr0
	fi
	return &.str
end

proc asmstr(ichar s)=
	gs_str(pdest,s)
end

proc asmchar(int c)=
	gs_char(pdest,c)
end

global func getdispname(psymbol d)ichar=
	static [256]char str

	if d.reg then
!		fprint @str,"#.#",(d.ispfloat|"X"|"R"), name
		fprint @str,"#.#","R", d.name
		return str
	fi

!	if fpshortnames or d.imported then
	if fpshortnames then
		return d.name
	fi

	return getfullname(d,backtick:1)

end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

	if fpshortnames or d=nil then
		print @str,"T",,n
	else
		fprint @str,"#.$T#",getdispname(d),n
	fi
	str
end

func strreg(int reg, size=8)ichar=
	psymbol d

	d:=checkregvar(reg,0)

	if size=8 and d then
		return getdispname(d)
	else
		getregname(reg,size)
	fi
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

export func getassemstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d,e
	ref mclrec m
	[32]char str2,str3
	int i

	gs_init(pdest)

!	for i to nlibfiles when libfiles[i]^<>'$' do
!		asmstr("          ")
!		asmstr("importdll ")
!		asmstr(libfiles[i])
!		gs_line(pdest)
!	od
!
	case phighmem
	when 1 then asmstr("    $userip\n")
	when 2 then asmstr("    $highmem\n")
	esac

	m:=mccode
	i:=1
	while m do
		writemcl(i,m)
		++i
		m:=m.nextmcl
	od

	return pdest
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
	when 1 then return "byte "
	when 2 then return "u16 "
	when 4 then return "u32 "
	when 8 then return "u64 "
	esac
	return ""
end

func checkregvar(int reg, ispfloat)psymbol d=
	RETURN NIL
end

proc start=
	assemtype:='AA'
end
=== mc_writeexe.m 0 0 17/41 ===
!Create .exe file from SS-data (code, data, reloc and psymbol tables)
!Call order:
! initsectiontable()
! genexe()
! writeexe(filename)

[maxplibfile]int64 libinsttable
[maxplibfile]ichar libinstnames
[maxplibfile]int libnotable			!index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
	ref basereloc nextitem
	word32 address				!virtual address
	int32 reloctype
end

ref basereloc basereloclist
int nbaserelocs
int maxrelocaddr
const maxbaseblock=500
[maxbaseblock]int blockbases
[maxbaseblock]int32 blockcounts
[maxbaseblock]int32 blockbytes
[maxbaseblock]byte blockpadding
int nbaseblocks
int basetablesize


const filealign = 512
!const filealign = 32
const sectionalign = 4096
const exe_imagebase = 0x40'0000
const dll_imagebase = 0x1000'0000

global int imagebase

int imagesize
int filesize
ref[]int64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
psymbol stentrypoint				!psymbol to be the entry point
psymbol stentrypoint2
psymbol stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

ref byte importdir				!allowed section data for import directort in .idata

global const maximports = 3000
global [0..maximports]importrec importtable
global int nimports

global const maxexports = 1000
global [maxexports]exportrec exporttable
global int nexports
ichar dllfilename
int isdll

!global const maxlibs = 50
const maxlibs = 50
global [maxlibs]dllrec dlltable
global int ndlls

ref byte datastart
ref byte dataptr
ichar userentrypoint

int exportdirvirtaddr
int exportdirvirtsize
int exportdiroffset				!from start of imp dir

int blockdirvirtaddr
int blockdirvirtsize
int blockdiroffset

global proc writeexe(ichar outfile, int dodll)=
	imagefileheader header
	optionalheader optheader
	int offset,i
	int64 aa

	dllfilename:=extractfile(outfile)

	isdll:=dodll

	datastart:=dataptr:=pcm_allocz(filesize)

	writedosstub()
	writepesig()
	writefileheader()
	writeoptheader()
	for i to nsections do
		writesectionheader(&sectiontable[i])
	od
	writepadding(sectiontable[1].rawoffset)
	for i to nsections do
		writesectiondata(&sectiontable[i])
	od

	if pverbose then
		println "EXE size:  ", dataptr-datastart:"10s,jr"
		println
	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

global proc genexe(ichar entrypoint, outfile, int dodll)=
!manipulate the ss data to fill in all the details needed for exe format
	int offset
	ref byte codeaddr				!mem address of start of code seg
	ref u32 offsetptr

	initsectiontable()

	dllfilename:=extractfile(outfile)
	isdll:=dodll

	imagebase:=(isdll|dll_imagebase|exe_imagebase)

	userentrypoint:=entrypoint
	loadlibs()
	scanst()				!build dll/import tables

	getoffsets()

	relocdata(&sectiontable[csect])
	relocdata(&sectiontable[dsect])

	codeaddr:=bufferelemptr(sectiontable[csect].data, 0)

	if phighmem then
!println "Doing RIP relocs..."
		ref riprec pr

		pr:=riplist
		while pr, pr:=pr.next do
			offsetptr:=ref u32(codeaddr+pr.offset)
			offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)
			offsetptr^:=offset	
		od
	fi
end

proc loadlibs=
!load library instances
	int i
	int64 hinst
	ichar file
	[300]char filename

	for i to nplibfiles when plibfiles[i]^<>'$' do
		strcpy(&.filename, plibfiles[i])
		hinst:=os_getdllinst(&.filename)
		if hinst=0 then
			cpl "File:",&.filename
			axerror("Can't load search lib")
		fi
		libinsttable[i]:=hinst
		libinstnames[i]:=pcm_copyheapstring(&.filename)
	od
end

global proc initsectiontable=
!set up the section table

	sectiontable[csect].name:=".text"
	sectiontable[csect].segtype:=code_seg
	sectiontable[csect].data:=ss_code
	sectiontable[csect].virtsize:=bufferlength(ss_code)

	if pverbose then
		println "Code size: ", bufferlength(ss_code):"10s,jr","bytes"
		println "Idata size:", bufferlength(ss_idata):"10s,jr"
		println "Code+Idata:", bufferlength(ss_code)+bufferlength(ss_idata):"10s,jr"
		println "Zdata size:", ss_zdatalen:"10s,jr"
	fi

	if bufferlength(ss_idata)=0 then
		addqword (ss_idata,0)
	fi

	sectiontable[dsect].name:=".data"
	sectiontable[dsect].segtype:=idata_seg
	sectiontable[dsect].data:=ss_idata

	sectiontable[dsect].virtsize:=bufferlength(ss_idata)
	sectiontable[dsect].rawsize:=roundtoblock(sectiontable[dsect].virtsize,filealign)
	sectiontable[dsect].nrelocs:=ss_nidatarelocs
	sectiontable[dsect].relocs:=ss_idatarelocs

	if ss_zdatalen=0 then
		ss_zdatalen:=16
	fi

	sectiontable[zsect].name:=".bss"
	sectiontable[zsect].segtype:=zdata_seg
!	sectiontable[zsect].rawsize:=roundtoblock(ss_zdatalen,filealign)
	sectiontable[zsect].virtsize:=ss_zdatalen


!note: rawsize will be recalculated later after thunk table is added
	sectiontable[csect].rawsize:=roundtoblock(sectiontable[csect].virtsize,filealign)
	sectiontable[csect].nrelocs:=ss_ncoderelocs
	sectiontable[csect].relocs:=ss_coderelocs

	sectiontable[isect].name:=".idata"
	sectiontable[isect].segtype:=impdata_seg
	sectiontable[isect].virtsize:=0
	sectiontable[isect].rawsize:=0

	nsections:=4
end

func extractlibname(ichar name, int &libno,moduleno)ichar=
!if name contains a dot, eg lib.abc, then set libno to index of "lib", and return "abc"
!otherwise return original name
	ref char s,name2
	[256]char str
	[256]char str2
	int i

	name2:=nil

	reenter:
	s:=name
	libno:=0

	while s^ do
		if s^='.' then			!assume lib.name
			memcpy(&.str,name,s-name)
			str[s-name+1]:=0
			strcat(&.str,".dll")

			for i:=1 to ndlls do
				if eqstring(&.str,dlltable[i].name) then
					libno:=i
					++dlltable[libno].nprocs
					return (name2|name2|s+1)
				fi
			od
			if ndlls>=maxlibs then axerror("Too many libs") fi
			libno:=++ndlls

			dlltable[libno].name:=pcm_copyheapstring(&.str)
			dlltable[libno].nprocs:=1
			return (name2|name2|s+1)
		fi

		++s
	od

!do explicit search
	int n

	for i:=1 to nplibfiles when libinsttable[i] do
		if os_getdllprocaddr(libinsttable[i],name) then
			n:=i
			exit				!don't need the actual address; just whether it exists
		fi
	else
		CPL NAME
		axerror("Can't find external function")
	od

!found in search lib n
	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	fi

!first use of this lib
	strcpy(&.str, plibfiles[n])
	strcat(&.str,".dll")
	if ndlls>=maxlibs then axerror("2:Too many libs") fi
	libno:=++ndlls

	dlltable[libno].name:=pcm_copyheapstring(&.str)
	dlltable[libno].nprocs:=1
	libnotable[n]:=libno

	return name
end

proc scanst=
!scan symbol table and build dll and imports list
!this version assumes dlls are encoded into the name of each import
!(otherwise, it means requiring a list of dlls and loading/searching for
!the names: doing real linker work.)

	int i,libno
	psymbol d
	ichar name, libname, basename

	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.imported then
			if nimports>=maximports then axerror("genexe: Too many imports") fi
			++nimports
			name:=extractlibname(d.name,libno,1)
			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		elsif d.exported then
			basename:=getbasename(d.name)
			if userentrypoint then
				if eqstring(basename,userentrypoint) then
					stentrypoint:=d
				fi
			else
				if eqstring(basename,"main") and not isdll then
					stentrypoint:=d
				fi
			fi

			if nexports>=maxexports then axerror("gendll: Too many exports") fi
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=getbasename(d.name)
		fi
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref word32 p32
	ref word64 p64
	psymbol d
	int offset,index,thunkoffset,iatoffset

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]
		index:=d.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset

		case r.reloctype
		when rel32_rel then
			if not d.imported then
				axerror("rel32/not imported")
			fi
			(ref word32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
!IF R.RELOCTYPE=ADDR32_REL THEN
!CPL "///ADDR32 REL"
!FI
			if d.imported then
				(ref word32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				u:=nil
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				else
					PRINTLN D.NAME,D.SEGMENT
					AXERROR("RELOCDATA/SEG?")

				esac
					p32:=cast(p+r.offset)
					if r.reloctype=addr32_rel then
						p32^:=p32^+u.virtoffset+imagebase
					else
						p64:=cast(P32)
						p64^:=p64^+u.virtoffset+imagebase
					fi
			fi
		else
			cpl relocnames[r.reloctype]
			axerror("Can't do this rel type")
		esac

		r:=r.nextreloc
	od

end

proc getbaserelocs(ref sectionrec s)=
!	ref sectionrec u
	ref relocrec r
	ref byte p
	psymbol d
	int index

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]

		case r.reloctype
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.imported then
			else
!				case d.segment
!				when zdata_seg then u:=&sectiontable[zsect]
!				when idata_seg then u:=&sectiontable[dsect]
!				when code_seg then u:=&sectiontable[csect]
!				esac

IF R.RELOCTYPE=ADDR32_REL THEN
!PRINTLN "??BASE RELOC",(D.SEGMENT=CODE_SEG|"CODE"|"DATA"),(R.RELOCTYPE=ADDR32_REL|"ADDR32"|"ADDR64")
ELSE
				newbasereloc(s.virtoffset+r.offset, r.reloctype)
FI

			fi
		esac

		r:=r.nextreloc
	od

end

proc writerecordx(ref void r, int length)=
	memcpy(dataptr,r,length)
	dataptr+:=length
end

proc writedosstub=
!write 128-byte dos stub to dataptr
	static []byte stubdata = (
	0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 
	0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 
	0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 
	0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD, 
	0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68, 
	0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72, 
	0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F, 
	0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E, 
	0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20, 
	0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A, 
	0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)

	writerecordx(&stubdata,stubdata.bytes)
end

proc writepesig=
	dataptr++^:='P'
	dataptr++^:='E'
	dataptr++^:=0
	dataptr++^:=0
end

proc writepadding(int offset)=
!offset is the next desired offset in the file
	dataptr:=datastart+offset			!data will have been cleared
end

proc writefileheader=
	imagefileheader header

	clear header

	header.machine:=0x8664
	header.nsections:=nsections
	header.optheadersize:=optionalheader.bytes
	header.characteristics:=0x22F
	if isdll then
		header.characteristics:=0x22E ior 0x2000
	fi

	writerecordx(&header,header.bytes)
end

proc writeoptheader=
	optionalheader header

	clear header

	header.magic:=0x20B
	header.majorlv:=1
	header.minorlv:=0
	header.codesize:=sectiontable[csect].rawsize
	header.idatasize:=sectiontable[dsect].rawsize+sectiontable[isect].rawsize
	header.zdatasize:=roundtoblock(sectiontable[zsect].virtsize,filealign)
	
	if stentrypoint=nil then
		stentrypoint:=stentrypoint2
	fi

	if stentrypoint=nil then
		if userentrypoint then
			cpl userentrypoint
			axerror("User entry point not found")
		else
			if not isdll then
				axerror("Entry point not found: main")
			fi
		fi
	else
		header.entrypoint:=sectiontable[csect].virtoffset+stentrypoint.offset
	fi

	header.codebase:=sectionalign
	header.imagebase:=imagebase
	header.sectionalignment:=sectionalign
	header.filealignment:=filealign
	header.majorosv:=4
	header.minorosv:=0
	header.majorssv:=5
	header.minorssv:=2
	header.imagesize:=imagesize
	header.headerssize:=sectiontable[1].rawoffset
	header.subsystem:=3

	header.stackreserve:=4194304
	header.stackcommit:=2097152

	header.heapreserve:=1048576
	header.heapcommit:=4096
	header.rvadims:=16

	header.importtable.virtualaddr:=sectiontable[isect].virtoffset
	header.importtable.size:=sectiontable[isect].virtsize-exportdirvirtsize-blockdirvirtsize

	if isdll then
		header.dllcharacteristics:=0x40		!relocatable
		header.exporttable.virtualaddr:=exportdirvirtaddr
		header.exporttable.size:=exportdirvirtsize

		header.basereloctable.virtualaddr:=blockdirvirtaddr
		header.basereloctable.size:=blockdirvirtsize
	fi

	header.iat.virtualaddr:=fileiatoffset
	header.iat.size:=fileiatsize

	writerecordx(&header,header.bytes)

end

proc writesectionheader(ref sectionrec s)=
	imagesectionheader sheader

	clear sheader

	strcpy(&sheader.name[1],s.name)
	sheader.virtual_size:=s.virtsize
	sheader.virtual_address:=s.virtoffset
	sheader.rawdata_offset:=s.rawoffset
	sheader.rawdata_size:=s.rawsize

	int64 aa
	case s.segtype
	when zdata_seg then
		aa:=0xC050'0080
		sheader.characteristics:=aa
!		sheader.characteristics:=0xC050'0080
	when idata_seg then
		aa:=0xC050'0040
		sheader.characteristics:=aa
!		sheader.characteristics:=0xC050'0040
	when code_seg then
		aa:=0x6050'0020
		sheader.characteristics:=aa
!		sheader.characteristics:=0x6050'0020
	when impdata_seg then
		aa:=0xC030'0040
		sheader.characteristics:=aa
!		sheader.characteristics:=0xC030'0040
	esac
	writerecordx(&sheader,sheader.bytes)
end

proc writesectiondata(ref sectionrec s)=
	case s.segtype
	when impdata_seg then
		writerecordx(s.bytedata,s.virtsize)		!rest of section will be zeros
		if s.rawsize>s.virtsize then
			dataptr+:=(s.rawsize-s.virtsize)
		fi

	when zdata_seg then					!nothing goes to disk
!		dataptr+:=s.rawsize
	else
		writerecordx(bufferelemptr(s.data,0),s.rawsize)
	esac
end

proc writeexporttable(ref byte pstart)=
	const maxexports=2000
	[maxexports]int sortindex
	ref exportdirrec phdr := cast(pstart)
	ref word32 paddrtable
	ref word32 pnametable
	ref word16 pordtable
	ref char pdllname
	ref char pnames
	int addrtableoffset
	int nametableoffset
	int ordtableoffset
	int dllnameoffset
	int namesoffset
	int virtoffset
	int sectionno
	psymbol d
	ichar basename

	phdr.timedatestamp:=0x5f89f4f8

	phdr.ordinalbase:=1
	phdr.naddrtable:=nexports
	phdr.nnamepointers:=nexports

!these are offsets from the start of the export data, from the start of the export dir
	addrtableoffset:=exportdirrec.bytes
	nametableoffset:=addrtableoffset+nexports*4
	ordtableoffset:=nametableoffset+nexports*4
	dllnameoffset:=ordtableoffset+nexports*2
	namesoffset:=dllnameoffset+strlen(dllfilename)+1

!virtoffset must be added to all above basic offsets, before being written to the file 
	virtoffset:=sectiontable[isect].virtoffset+exportdiroffset

!work out pointers into memory to receive the data
	paddrtable:=cast(pstart+addrtableoffset)
	pnametable:=cast(pstart+nametableoffset)
	pordtable:=cast(pstart+ordtableoffset)
	pdllname:=cast(pstart+dllnameoffset)
	pnames:=cast(pstart+namesoffset)

!fill in rest of export dir
	phdr.namerva:=dllnameoffset+virtoffset
	phdr.expaddressrva:=addrtableoffset+virtoffset
	phdr.namepointerrva:=nametableoffset+virtoffset
	phdr.ordtablerva:=ordtableoffset+virtoffset

	strcpy(pdllname,dllfilename)

!address table
	if nexports>maxexports then
		axerror("Too many exports - can't sort")
	fi

	sortexports(sortindex)

	for i to nexports do
!		d:=exporttable[i].def
		d:=exporttable[sortindex[i]].def
		basename:=exporttable[sortindex[i]].name
		sectionno:=getsectionno(d.segment)

		strcpy(pnames,basename)
		pnametable^:=namesoffset+virtoffset
		++pnametable
		namesoffset+:=strlen(basename)+1
		pnames+:=strlen(basename)+1

		paddrtable^:=d.offset+sectiontable[sectionno].virtoffset
		++paddrtable
		pordtable^:=i-1
		++pordtable
	od
end

func getexporttablesize:int=
	int size

	size:=exportdirrec.bytes
	size+:=nexports*4			!address table entries
	size+:=nexports*4			!name pointers
	size+:=nexports*2			!ordinal table

	size+:=strlen(dllfilename)+1
	for i to nexports do
		size+:=strlen(exporttable[i].def.name)+1
	od

	return size
end

proc newbasereloc(int addr, reltype)=
	ref basereloc p

	p:=pcm_allocnfz(basereloc.bytes)
	p.address:=addr
	p.reloctype:=reltype

	p.nextitem:=basereloclist

	basereloclist:=p
	++nbaserelocs
	maxrelocaddr max:=addr

end

proc scanbaserelocs=
!go through all the relocs and build the block tables, and work out overall size
!	int maxaddr:=maxrelocaddr+4096
	int baseaddr,addr,nextblock
	ref basereloc p

	baseaddr:=0x1000
	nbaseblocks:=0

	repeat
		nextblock:=baseaddr+0x1000
		if nbaseblocks>=maxbaseblock then axerror("Too many blocks") fi
		++nbaseblocks
		blockbases[nbaseblocks]:=baseaddr
		blockcounts[nbaseblocks]:=0


		p:=basereloclist
		while p do
			addr:=p.address
			if addr>=baseaddr and addr<nextblock then
!				println "	",addr:"h",addr-baseaddr:"h", relocnames[p.reloctype]
				++blockcounts[nbaseblocks]
			fi

			p:=p.nextitem
		od

		baseaddr:=nextblock
	until baseaddr>maxrelocaddr

	for i to nbaseblocks when blockcounts[i] do
!	for i to nbaseblocks do
		if blockcounts[i].odd then
			++blockcounts[i]
			++blockpadding[i]
		fi
		blockbytes[i]:=blockcounts[i]*2+8
		basetablesize+:=blockbytes[i]
	od
end

proc writebasereloctable(ref byte pstart)=
	
	ref word32 p32
	ref word16 p16
	int baseaddr,addr,nextblock
	ref basereloc q

	p32:=cast(pstart)

	for i to nbaseblocks when blockcounts[i] do
		p32^:=blockbases[i]
		++p32
		p32^:=blockbytes[i]
		++p32
		p16:=cast(p32)

		q:=basereloclist
		baseaddr:=blockbases[i]
		nextblock:=baseaddr+4096

		while q do
			addr:=q.address
			if addr>=baseaddr and addr<nextblock then
				p16^:=addr-baseaddr+(q.reloctype=addr32_rel|3|10)<<12
				++p16
			fi
!
			q:=q.nextitem
		od
		if blockpadding[i] then p16++^:=0 fi

		p32:=cast(p16)

	od
end

proc sortexports([]int &sortindex)=
!Sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
	psymbol d,e

!First, store 1..nexports into sortindex
	for i to nexports do
		sortindex[i]:=i
	od

!do bubble sort for now
	int swapped

	repeat
		swapped:=0
		for i:=1 to nexports-1 do

			d:=exporttable[sortindex[i]].def
			e:=exporttable[sortindex[i+1]].def

			if strcmp(getbasename(d.name), getbasename(e.name))>0 then

				swapped:=1
				swap(sortindex[i], sortindex[i+1])
			fi
		od
	until not swapped

end

func getsectionno(int segment)int=
	case segment
	when zdata_seg then zsect
	when idata_seg then dsect
	when code_seg then csect
	else axerror("GSN"); 0
	esac
end

proc getoffsets=
!apply file/image offsets to sectiontable
	int fileoffset, imageoffset,i,diroffset,impdirno,hinttableoffset,j,n
	int codesize,length,thunkoffset,offset,dirstartoffset

	fileoffset:=128+4+imagefileheader.bytes+optionalheader.bytes	!dosstub+sig
	fileoffset+:=imagesectionheader.bytes*nsections

	fileoffset:=roundtoblock(fileoffset,filealign)
	imageoffset:=4096

!Need to increase size of code segment to incorporate the thunk table
	ref byte pcode
	codesize:=sectiontable[csect].virtsize
	pcode:=bufferelemptr(ss_code,codesize)
	while codesize iand 7 do pcode++^:=0x90; ++codesize od
	thunkoffset:=codesize
	codesize+:=nimports*8

	sectiontable[csect].virtsize:=codesize
	sectiontable[csect].rawsize:=roundtoblock(codesize,filealign)

!have to actually add the extra memory now.
	buffercheck(ss_code, codesize-thunkoffset+16)		!just ensure it's there for now

	for i:=1 to nsections do
		if sectiontable[i].segtype<>zdata_seg then
			sectiontable[i].rawoffset:=fileoffset
		fi
		if sectiontable[i].segtype<>zdata_seg then
			fileoffset:=roundtoblock(fileoffset+sectiontable[i].virtsize,filealign)
		fi
		sectiontable[i].virtoffset:=imageoffset

		if sectiontable[i].segtype=impdata_seg then
			diroffset:=imageoffset
			impdirno:=i
		fi

		imageoffset:=roundtoblock(imageoffset+sectiontable[i].virtsize,sectionalign)
	od

	if isdll then
		getbaserelocs(&sectiontable[csect])
		getbaserelocs(&sectiontable[dsect])
	fi

!Work out offsets within import directory
!assume dll/imports have been set up
!diroffset starts off as virtual offset of start of impdata section

	diroffset+:=(ndlls+1)*importdirrec.bytes			!need blank entry as terminator

!diroffset now points to import name table
!usual arrangements is for all import name table, followed by all import addr tables

	for i to ndlls do
		dlltable[i].nametableoffset:=diroffset				!data will be filled in later
		diroffset+:=(dlltable[i].nprocs+1)*8
	od
	fileiatoffset:=diroffset
	for i to ndlls do
		dlltable[i].addrtableoffset:=diroffset				!data will be filled in later
		diroffset+:=(dlltable[i].nprocs+1)*8
	od
	fileiatsize:=diroffset-fileiatoffset

!diroffset now points to hint/name table, which is shared by all libs
!At this point, I need to write into an actual impdata segment, which doesn't
!exist yet. So I need to do a first pass over the import names to work out the size
	hinttableoffset:=diroffset
	for i to nimports do
		length:=strlen(importtable[i].name)+3
		if length iand 1 then ++length fi		!keep even
		importtable[i].hintnameoffset:=diroffset
		diroffset+:=length
	od

!need also space for the names of the libs

!need I think to get to next multiple of four
	diroffset:=roundtoblock(diroffset,4)

	for i to ndlls do
		length:=strlen(dlltable[i].name)+1
		if length iand 1 then ++length fi		!keep even
		dlltable[i].dllextraoffset:=diroffset
		diroffset+:=dlltable[i].nprocs*4		!space for back-links to dir entry
		dlltable[i].dllnameoffset:=diroffset
		diroffset+:=length
	od

	dirstartoffset:=sectiontable[impdirno].virtoffset

	if isdll then
		exportdirvirtaddr:=diroffset
		exportdiroffset:=diroffset-dirstartoffset
		exportdirvirtsize:=getexporttablesize()

		diroffset+:=exportdirvirtsize

		scanbaserelocs()

		blockdirvirtaddr:=diroffset
		blockdiroffset:=diroffset-dirstartoffset
		blockdirvirtsize:=basetablesize
		diroffset+:=blockdirvirtsize
	fi

	offset:=diroffset-dirstartoffset

!offset contains now the overall size of the import directory
!diroffset contains is the overall size of the image

!finish off last section data, and compute final file and image sizes
	sectiontable[impdirno].virtsize:=offset
	sectiontable[impdirno].rawsize:=roundtoblock(offset,filealign)
	filesize:=roundtoblock(fileoffset+offset,filealign)

	imagesize:=roundtoblock(imageoffset+(diroffset-dirstartoffset),sectionalign)

	ref byte pimpdir

	pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

!prepare the thunk area in the code segment
	ref importdirrec pdir
	ref int64 paddr,pname
	int iatoffset
	pdir:=cast(pimpdir)

!start fill in details within the import directory section
	for i:=1 to ndlls do
		pdir.implookuprva:=dlltable[i].nametableoffset
		pdir.impaddressrva:=dlltable[i].addrtableoffset
		pdir.namerva:=dlltable[i].dllnameoffset
		++pdir

		iatoffset:=dlltable[i].addrtableoffset
		paddr:=cast(pimpdir+iatoffset-dirstartoffset)
		pname:=cast(pimpdir+dlltable[i].nametableoffset-dirstartoffset)
		for j to nimports when importtable[j].libno=i do
			pname^:=paddr^:=importtable[j].hintnameoffset
			importtable[j].iatoffset:=iatoffset
			iatoffset+:=8
			++pname
			++paddr
		od
	od

!Fill in the hint/name table
	ref byte phint
	ref word32 pextra

	for i to nimports do
		phint:=pimpdir+importtable[i].hintnameoffset-dirstartoffset
		phint+:=2					!leave hint as 0
		strcpy(cast(phint),importtable[i].name)
	od
!same for lib names (no hint here, but re-use phint anyway)
	int xxx
	xxx:=dirstartoffset
	for i to ndlls do
		pextra:=cast(pimpdir+dlltable[i].dllextraoffset-dirstartoffset)
		for j to dlltable[i].nprocs do
			pextra^:=xxx
			++pextra
		od
		xxx+:=importdirrec.bytes
		phint:=pimpdir+dlltable[i].dllnameoffset-dirstartoffset
		strcpy(cast(phint),dlltable[i].name)
	od

	if isdll then
		writeexporttable(ref byte(pimpdir)+exportdiroffset)
		writebasereloctable(ref byte(pimpdir)+blockdiroffset)
	fi

!write the thunk table
	ref byte thunkptr,codebase
	int thunkaddr
	thunkptr:=bufferelemptr(ss_code,thunkoffset)
	codebase:=bufferelemptr(ss_code,0)

	for i to nimports do
		importtable[i].thunkoffset:=thunkptr-codebase
		if phighmem=0 then
			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x24
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(ref int32(thunkptr)^:=thunkaddr)
			thunkptr+:=4
		else					!use rip mode

			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(ref int32(thunkptr)^:=getripoffset(int(thunkptr-codebase),thunkaddr-imagebase))
			thunkptr+:=4
			thunkptr++^:=0x90
		fi
	od
!-----------------------------------------------
end

func getripoffset(int addr, dest, int extra=0)int=
!work out the rip offset for a d32 field at <addr>, to <dest>
!opbytes is the number of opcode bytes that precede the field
!addr is offset of d32 field with codeseg, from start of code segment
!dest is offset within image, relative to imagebase
!extra is 0/1/2/4 bytes of imm data that some instructions will have

	addr+:=sectiontable[csect].virtoffset		!now is offset rel to imagebase
	dest-(addr+4)-extra
end

=== mc_writeobj.m 0 0 18/41 ===
!NEEDS REVISING TO MATCH UNLIMITED SS_SYMBOLTABLE size used for EXE
!and also unlimited strings

int symtaboffset

ref byte datastart
ref byte dataptr

[0..13'000]imagesymbol symboltable

int nsymbols

int stoffset=0				!usually +7 to convert ss_symboltable indices to symboltable

const maxstring=5000
[maxstring]ichar stringtable
[maxstring]int stringlengths
int nextstringoffset=0
int nstrings=0

global proc writecoff(ichar outfile)=
	imagefileheader header
	imagesectionheader zsection, isection, csection
	int offset
	int64 aa

	clear header
	clear zsection
	clear isection
	clear csection

	header.machine:=0x8664
	header.nsections:=3

	strcpy(&zsection.name[1],".bss")
	zsection.rawdata_size:=ss_zdatalen

	zsection.characteristics:=0xC040'0080

	if ss_nidatarelocs>=65536 or ss_ncoderelocs>=65536 then
		axerror("Too many relocs (exceeds 16-bit field)")
	fi

	strcpy(&isection.name[1],".data")
	isection.rawdata_size:=bufferlength(ss_idata)
	isection.nrelocs:=ss_nidatarelocs

	isection.characteristics:=0xC050'0040

	strcpy(&csection.name[1],".text")
	csection.rawdata_size:=bufferlength(ss_code)
	csection.nrelocs:=ss_ncoderelocs

	csection.characteristics:=0x6050'0020

	initsymboltable(outfile)

	convertsymboltable()

	offset:=imagefileheader.bytes

	offset+:=imagesectionheader.bytes*3

	if isection.nrelocs then
		isection.relocations_ptr:=offset
		offset+:=isection.nrelocs*coffrelocrec.bytes
	fi

	if csection.nrelocs then
		csection.relocations_ptr:=offset
		offset+:=csection.nrelocs*coffrelocrec.bytes
	fi

	isection.rawdata_offset:=offset
	offset+:=isection.rawdata_size

	csection.rawdata_offset:=offset
	offset+:=csection.rawdata_size

!create symbol table and string table

	header.symtaboffset:=offset
	offset+:=nsymbols*imagesymbol.bytes
	header.nsymbols:=nsymbols

	offset+:=nextstringoffset

!Allocate data block in memory for coff image
	datastart:=dataptr:=malloc(offset)

	writerecord(&header,header.bytes)
	writerecord(&zsection,zsection.bytes)
	writerecord(&isection,isection.bytes)
	writerecord(&csection,csection.bytes)
	writerelocs(ss_idatarelocs,ss_nidatarelocs)
	writerelocs(ss_coderelocs,ss_ncoderelocs)

	writedata(ss_idata)
	writedata(ss_code)

	writesymboltable()

	writestringtable()

	if pverbose then
		println "Writing file:",outfile
	fi
	writefile(outfile,datastart,dataptr-datastart)

end

proc writerecord(ref void r, int length)=
	memcpy(dataptr,r,length)
	dataptr+:=length
end

proc writerelocs(ref relocrec r,int nrelocs)=
	static coffrelocrec s
	psymbol d

	return when nrelocs=0

	while r do
		case r.reloctype
		when addr32_rel, addr64_rel then		!change to section entry
			d:=ss_symboltable^[r.stindex]

			case d.segment
			when zdata_seg then s.stindex:=2
			when idata_seg then s.stindex:=4
			when code_seg then s.stindex:=6
			when 0 then							!external; leave stindex pointing to symbol
				s.stindex:=r.stindex+stoffset
			else
				axerror("wrelocs/bad seg")
			esac

		else
			s.stindex:=r.stindex+stoffset
		esac

		s.reloctype:=r.reloctype
		case phighmem
		when 0 then
		when 2 then
			IF R.RELOCTYPE=ADDR32_REL THEN
				S.RELOCTYPE:=REL32_REL
				R.RELOCTYPE:=REL32_REL
			FI
		else
			axerror("OBJ/phighmem 1?")
		esac

		s.virtualaddr:=r.offset


		memcpy(dataptr,&s,s.bytes)
		dataptr+:=s.bytes

		r:=r.nextreloc
	od
end

proc writedata(ref dbuffer data)=
	memcpy(dataptr, bufferelemptr(data,0), bufferlength(data))
	dataptr+:=bufferlength(data)
end

proc writesymboltable=
	int i
	for i:=1 to nsymbols do
		writerecord(&symboltable[i],imagesymbol.bytes)
	od
end

proc writestringtable=
!should immediately follow symboltable
	ref int32 p
	int i,n

	p:=cast(dataptr)
	p^:=nextstringoffset
	dataptr+:=4

	for i to nstrings do
		n:=stringlengths[i]+1
		memcpy(dataptr,stringtable[i],n)
		dataptr+:=n
	od
end

function makesymbol(ichar name, int value=0, sectionno=0,symtype=0,storage=0,naux=0)ref imagesymbol=
	static imagesymbol r
	int length, namelen

!CPL "MS",NAME, =SECTIONNO

	namelen:=strlen(name)

	if namelen<8 then
		strcpy(&r.shortname[1],name)
	elsif namelen=8 then
		memcpy(&r.shortname[1],name,namelen)
	else
		r.shortx:=0
		r.longx:=addstringentry(name,namelen)
	fi
	r.value:=value
	r.sectionno:=sectionno
	r.symtype:=symtype
	r.storageclass:=storage
	r.nauxsymbols:=naux
	return &r
end

proc addsymbol(ref imagesymbol r)=
	if nsymbols>=symboltable.upb then
		axerror("as:Too many symbols")
	fi
	memcpy(&symboltable[++nsymbols],r,imagesymbol.bytes)
end

proc initsymboltable(ichar filename)=
!add first few special symbols to coff symboltable

	nsymbols:=0

	addsymbol(makesymbol(".file",storage:103, sectionno:-2,naux:1))
	addsymbol(strtoaux(filename))

	addsymbol(makesymbol(".bss", storage:3, sectionno:1, naux:1))
	addsymbol(cast(sectiontoaux(nil, 0)))

	addsymbol(makesymbol(".data", storage:3, sectionno:2, naux:1))
	addsymbol(cast(sectiontoaux(ss_idata, ss_nidatarelocs)))

	addsymbol(makesymbol(".text", storage:3, sectionno:3, naux:1))
	addsymbol(cast(sectiontoaux(ss_code, ss_ncoderelocs)))
end

function strtoaux(ref char s)ref imagesymbol=
!turn string s into 18-byte imagesymbol record
	static imagesymbol r
	ref byte p:=cast(&r)
	int n

	memset(p,0,r.bytes)
!	clear p^

	n:=0
	while s^<>0 and n<r.bytes do
		p++^:=s++^
		++n
	od

	return &r
end

function sectiontoaux(ref dbuffer data, int nrelocs)ref auxsectionrec=
!!turn segment into into aux section/reloc entry for symboltable
	static auxsectionrec r

	clear r

	if data=nil then			!zdata
		r.length:=ss_zdatalen
	else
		r.length:=bufferlength(data)

	fi
	r.nrelocs:=nrelocs
	return &r
end

function addstringentry(ichar s, int length)int=
!assume s is longer than 8 chars
!add string table entry, return offset to string, as it would be in the coff string table
!assume s in stable memory so doesn't need copying
	int offset

	offset:=nextstringoffset
	if nstrings>maxstring then
		axerror("W:too many strings")
	fi
	stringtable[++nstrings]:=s
	stringlengths[nstrings]:=length

	nextstringoffset+:=length+1

	return offset
end

proc convertsymboltable=
!scan ss_symboltable and generate coff symboltable equivalents
	psymbol s
	ichar name
	int i,sect, scope

	stoffset:=nsymbols-1

	nstrings:=0
	nextstringoffset:=4

	for i to ss_nsymbols do
		s:=ss_symboltable^[i]
!CPL "SS",I,S.NAME,SS_NSYMBOLS

		name:=s.name

		case s.segment
		when zdata_seg then sect:=1
		when idata_seg then sect:=2
		when code_seg then sect:=3
		else sect:=0
		esac

IF S.IMPORTED THEN SECT:=0 FI

		SCOPE:=0
		if s.imported or s.exported then
			scope:=2
		else
			scope:=3
		fi

!		addsymbol(makesymbol(s.name, sectionno:sect, storage:scope, value:s.offset))
!		addsymbol(makesymbol(getfullname(s), sectionno:sect, storage:scope, value:s.offset))
		addsymbol(makesymbol(getqualname(s), sectionno:sect, storage:scope, value:s.offset))

	od
end

func getqualname(psymbol d)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	[16]psymbol chain
	int n:=0
	psymbol e:=d

	if d.imported or d.exported and eqstring(d.name, "main") then
		return d.name
	fi

	repeat
		chain[++n]:=e
		e:=e.owner
	until e=nil !or e.id=program_id

	strcpy(str,chain[n].name)
	for i:=n-1 downto 1 do
		strcat(str,".")
		strcat(str,chain[i].name)
	od

!	return str
	return pcm_copyheapstring(str)
end

=== mx_decls.m 0 0 19/41 ===
!Declarations for M-Code scheme
!Terms:
! MCU		MCode Unit, binary code/data/imports/relocs for whole program (LIBREC)
! MCB		MCU rendered to flat data block, written out as .mx/.ml file
! MCX		MCU with allocations, imports and fixups done to make it ready to run
! LIB		Informal reference to MCU or MCX in-memory data; or to MX/ML file
!
! MCU is created from SS data (which normally was used to generate EXE)
! An MCU block is either converted to MCB which is then sent to an MX/ML file;
! or it is directly fixed up into MCX to allow immediate execution
!
! MCB can be read from a file into a memory block. MCB data contains a lineat
! set of tagged data blocks. Those blocks are scanned to form a normal MCU/LIBREC
! data-structure. That MCU can be fixed up like above to make it ready to be
! run or any function to be called.


!single byte tags in mcx file

export const mcxsig = 'MCX\x1A'

export enumdata [0:]ichar mcxdirnames =
	(pad_dir = 0,		$),		! nothing follows except next tag; for padding/alignment
	(version_dir,		$),		! STR string follows with version code (stringz)
	(code_dir,			$),		! N(u32) then N bytes of code data
	(idata_dir,			$),		! N(u32) then N bytes init data
	(zdata_dir,			$),		! N(u32) (no data follows)
	(reloc_dir,			$),		! N(u32) then N records follow
	(dlls_dir,			$),		! N(u32) then N STR items, the DLL base names
	(libs_dir,			$),		! N(u32) then N STR items, the MCX base names (ML libs)
	(importsymbols_dir,	$),		! N(u32) then N STR items, the imported names
	(exportsymbols_dir,	$),		! N(u32) then N STR items, the exported names
	(exportsegs_dir,	$),		! N(u32) then N u8 items, each is a segment code
	(exportoffsets_dir,	$),		! N(u32) then N u32 items, each an offset in the segment
	(entry_dir,			$),		! N(u32) N is a byte offset within code segment for entry point
	(end_dir,			$),		! nothing follows; end of file
end

!Reloc item record
! For Locabs-codes, the field contains the offset of the local symbol within target segment
! For Imp-codes, the field contains zero bytes

export record mcxreloc =
	u32		offset			! Offset with .segment of the reloc item
	union
		u16		stindex			! For Imp-codes, index into global import tables
		byte	targetsegment	! For Loc-codes, target segment refered to
	end
	byte	segment			! Segment containing the reloc item
	byte	reloctype		! Reloc code (see enums); also sets size of reloc item
end



!Relocation codes

export enumdata [0:]ichar mcxrelocnames =
	(no_rel = 0,		$),

	(locabs32_rel,	"locabs32"),		! add target segment address to 32-bit offset
	(locabs64_rel,	"locabs64"),		! add target segment address to 64-bit offset

	(impabs32_rel,	"impabs32"),		! replace 32-bit 0-field with address of imported symbol
	(impabs64_rel,	"impabs64"),		! replace 64-bit 0-field with address of imported symbol

	(imprel32_rel,	"imprel32"),		! replace 32-bit 0-field with offset of thunk entry for symbol
end

! Explanation of reloc codes
! No reloc
!	For local call/jmp, which are /only/ within code segment, no fixups are needed
!
! Locabs32/Locabs64
!	Reloc field contains offset of location within target segment, plus any
!   constant offset (eg. A+3 has offset of A, plus 3)
!   Baseaddr of that segment is added to that offset
!
! Impabs32/64
!	Reloc field contains any local offset (eg. the 3 in A+3)
!	Symbol index is used (via xlate to global index) to get abs address of symbol
!   Reloc field is replaced with 32/64 bits of that address plus the original value
!
! Imprel32
!	Only used for imported names, and only for CALL. Reloc field may be zeros
!	Reloc field will be changed to relative offset thunk table at end of code segment
!	Thunk table (indexed by local import index), is populated with JMPs to values
!	stored in address table which follows immediately
!	That address table needs to be populated with abs addresses of those imports
!	(Calls to LIB rather than DLL can have CALL offset replaced with direct offset to the
!	imported function, provided top 31 bits of address are zero.)

!export enumdata []ichar segmentnames =
!	(code_seg,		"code"),
!	(idata_seg,		"idata"),
!	(zdata_seg,		"zdata"),
!	(rodata_seg,	"rodata"),
!	(impdata_seg,	$),
!end

!Describe an MCX program loaded into memory


export record librec=
!The first section describes data residing in a file and loaded into these vars
!(code block is loaded directly into an actual executable block, with thunk/
!address table space added)

	ichar version

	int codesize			! bytes in code block, excluding thunk/addr tables
	int idatasize			! bytes in idata block
	int zdatasize			! bytes in zdata block (no data; created on fixup)

	int nrelocs				! size of reloctable
	int	ndlllibs			! size of imported dll names
	int	nlibs				! size of imported libnames
	int nimports			! size of imports/importlib tables
	int nexports			! size of exports/exportsegs/exportoffsets tables

	ref byte codeptr		! executable code block (includes thunk/addr table)
	ref byte idataptr		! initialised data block

	ref[]mcxreloc	reloctable		! table of reloc entries
	ref[]ichar		dllnames		! base names of imported dll files (no extension)
	ref[]ichar		libnames		! base names of imported mcx files (no extension)
	ref[]ichar		importnames		! names of imported symbols
	ref[]ichar		exports			! names of exported symbols
	ref[]byte		exportsegs		! segment where each is located
	ref[]u64		exportoffsets	! offsets within each segment

	u64 entryoffset					! offset within code block where execution will start
									! value of 0xFFFFFFFF (is u32 in file) means not set

!The next section is filled in after loading

	ref byte zdataptr				! zeroed data block
	int codexsize					! bytes in thunk/addr tables that follow code
	ref[]u64		exportaddr		! fully fixed-up addresses of exported symbols (not in file)
	ref[]int16		importxreftable	! map symbol index to global one

	ichar			filespec		!full path
	ichar			libname			!base name of library
	ref byte		entryaddr		!start address (left at nil when entryoffset not set)
	int				libno			!index of this entry in progtable
end

global const maxdlls =		20
global const maxlibs =		20
global const maxsymbols =	3000

!Global DLL tables

global [maxdlls]ichar		dllnametable
global [maxdlls]u64			dllinsttable
global int ndlllibs

!Global Prog table

global [maxlibs]ichar		libnametable
global [maxlibs]ref librec	libtable
global [maxlibs]byte		librelocated		!1 when relocated
global [maxlibs]byte		libinitdone			!1 when entry point called
global int nlibs

!Global import tables

global [maxsymbols]ichar	symbolnametable	! Name of symbol
global [maxsymbols]byte		symboldefined	! 1 when fully resolved with address
global [maxsymbols]ref void	symboladdress	! Abs address
global [maxsymbols]int16	symbollibindex	! Lib index where defined
global [maxsymbols]byte		symboldllindex	! DLL index of library where found
global int nsymbols

export int nsymimports=0, nsymexports=0
=== mx_run.m 0 0 20/41 ===
!Translate SS data directly into MCU block, then try and run that

global func writememlib(ichar filename)ref librec plib=
!write ss to mcu
	int n, k
	librec lib

	clear lib

	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	lib.version:="0.1234"

	lib.filespec:=filename
	lib.libname:=pcm_copyheapstring(extractbasefile(filename))
	lib.libno:=1

	countsymbols()
	writerelocs(&lib)

	lib.zdatasize:=ss_zdatalen
	lib.codesize:=bufferlength(ss_code)
	lib.idatasize:=bufferlength(ss_idata)

	lib.codeptr:=bufferelemptr(ss_code,0)
	lib.idataptr:=bufferelemptr(ss_idata,0)

	int ndlls:=0, nlibs:=0
	for i to nplibfiles when plibfiles[i]^<>'$' do
!		if libtypes[i]='D' then ++ndlls else ++nlibs fi
		++ndlls
	od

	lib.ndlllibs:=ndlls
	lib.nlibs:=nlibs

	lib.dllnames:=pcm_alloc(ichar.bytes*ndlls)
	lib.libnames:=pcm_alloc(ichar.bytes*nlibs)

	k:=0
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nplibfiles when plibfiles[i]^<>'$' do
		lib.dllnames[++k]:=plibfiles[i]
	od

!	k:=0
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='L' do
!		lib.libnames[++k]:=libfiles[i]
!	od

	addsymbols(&lib)
	plib:=pcm_allocnfz(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

proc roundsegment(ref dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	od
end

proc writerelocs(ref librec lib)=
	ref relocrec oldr
	mcxreloc newr
	int n, k
	psymbol d
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	lib.nrelocs:=ss_nidatarelocs+ss_ncoderelocs
	lib.reloctable:=pcm_alloc(lib.nrelocs*mcxreloc.bytes)

	k:=0

	for i in code_seg..idata_seg do
		oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

		while oldr, oldr:=oldr.nextreloc do
			clear newr

			newr.offset:=oldr.offset
			newr.segment:=(i=code_seg|idata_seg|code_seg)

			d:=ss_symboltable[oldr.stindex]

			case oldr.reloctype
			when rel32_rel then
				if d.imported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.imported then
					newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
					newr.stindex:=d.impindex
				else
					if oldr.reloctype=addr32_rel then
						newr.reloctype:=locabs32_rel
					else
						newr.reloctype:=locabs64_rel
					fi
					newr.targetsegment:=d.segment
				fi
			else
				axerror("reloc?")
			esac

			lib.reloctable[++k]:=newr

		od
	od
end

proc addsymbols(ref librec lib)=
	psymbol d, stentry:=nil
	u64 epoffset:=-1
	int n, k
	ichar name


	lib.nimports:=nsymimports
	lib.nexports:=nsymexports
	lib.importnames:=pcm_alloc(nsymimports*ichar.bytes)
	lib.exports:=pcm_alloc(nsymexports*ichar.bytes)
	lib.exportsegs:=pcm_alloc(nsymexports)
	lib.exportoffsets:=pcm_alloc(nsymexports*u64.bytes)

	k:=0
	for i to ss_nsymbols when ss_symboltable[i].impindex do
		d:=ss_symboltable[i]
		lib.importnames[++k]:=d.name
	od

	k:=0
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			if eqstring(d.name, "main") then
				stentry:=d
			fi
			lib.exports[++k]:=d.name
			lib.exportsegs[k]:=d.segment
			lib.exportoffsets[k]:=d.offset
		fi
	od

	if stentry then
		lib.entryoffset:=stentry.offset
	else
		lib.entryoffset:=-1
	fi
end

global proc countsymbols=
	psymbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.exported then d.expindex:=++nsymexports fi
		if d.imported then d.impindex:=++nsymimports fi
	od
end

global proc runlibfile(ichar filename, int cmdskip)=
!LOADERROR("RUNLIBFILE")

	ref librec plib

	plib:=writememlib(filename)

	loadmemmcu(plib)
	fixuplib(plib)

!	if fshowmx then
!		LOADERROR("SHOWMX missing")
!!		initlogfile()
!!		showlibs()
!!		closelogfile()
!	else
		runprogram(plib, cmdskip)
!	fi
end

=== mx_lib.m 0 0 21/41 ===
!subprog libmx
!module mx_lib
!module mx_show
!module mx_decls
!module mx_run
!module mx_write

!$sourcepath "/ax/"
!module aa_disasm


global enumdata [0:]ichar rsegmentnames =
	(no_seg=0,		$),
	(code_rseg,		$),
	(idata_rseg,		$),
	(zdata_rseg,		$),
	(rodata_rseg,	$),
	(impdata_rseg,	$),
end

global func readlibfile(ichar filespec, ref byte p)ref librec plib=
!p points to an MCB block; scan that into an MCU descriptor (librec)

	librec lib
	u64 sig
	int dir,n,tablesize
	ref byte q

	clear lib

	sig:=readu32(p)
	if sig<>mcxsig then
		println "Bad sig - not MCX file"
		stop 1
	fi

	lib.filespec:=pcm_copyheapstring(filespec)
	lib.libname:=pcm_copyheapstring(extractbasefile(filespec))

	doswitch dir:=readbyte(p)
	when version_dir then
		lib.version:=readstring(p)

	when zdata_dir then
		lib.zdatasize:=readu32(p)
!		lib.zdataptr:=pcm_allocz(lib.zdatasize)

	when idata_dir then
		lib.idatasize:=n:=readu32(p)
		lib.idataptr:=pcm_alloc(n)
		memcpy(lib.idataptr, p, n)	
		p+:=n

	when code_dir then
		lib.codesize:=n:=readu32(p)
		lib.codeptr:=p				!for now, point into file image
		p+:=n

	when dlls_dir then
		lib.ndlllibs:=n:=readu32(p)
		lib.dllnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.dllnames[i]:=readstring(p)
		od

	when libs_dir then
		lib.nlibs:=n:=readu32(p)
		lib.libnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.libnames[i]:=readstring(p)
		od
	when importsymbols_dir then
		lib.nimports:=n:=readu32(p)
		lib.importnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.importnames[i]:=readstring(p)
		od

	when exportsymbols_dir then
		lib.nexports:=n:=readu32(p)
		lib.exports:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.exports[i]:=readstring(p)
		od

	when exportsegs_dir then
		n:=readu32(p)
		lib.exportsegs:=pcm_alloc(n)
		for i to n do
			lib.exportsegs[i]:=readbyte(p)
		od

	when exportoffsets_dir then
		n:=readu32(p)
		lib.exportoffsets:=pcm_alloc(u64.bytes*n)
		for i to n do
			lib.exportoffsets[i]:=readu32(p)
		od

	when reloc_dir then
		lib.nrelocs:=n:=readu32(p)
		n:=lib.nrelocs*mcxreloc.bytes
		lib.reloctable:=pcm_alloc(n)
		memcpy(lib.reloctable, p, n)
		p+:=n

	when entry_dir then
		lib.entryoffset:=readu32(p)

	when end_dir then
		exit

	when pad_dir then

	else
		println "Unknown directive:",mcxdirnames[dir]
		stop
	end doswitch

	plib:=pcm_allocnfz(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

func readbyte(ref byte &p)int=
	return p++^
end

func readu32(ref byte &p)u64 x=
	x:=ref u32(p)^
	p+:=4
	x
end

func readstring(ref byte &p)ichar s=
	s:=pcm_copyheapstring(p)

	while (++p)^ do od
	++p

	return s
end

global proc alloclibdata(ref librec lib)=
	int tablesize, n
	ref byte p

	lib.zdataptr:=pcm_allocz(lib.zdatasize)

	tablesize:=lib.nimports*16			!add in thunk table+address table
	n:=lib.codesize

	p:=os_allocexecmem(n+tablesize)		!MUST BE EXECUTABLE MEMORY
	if p=nil then
		error("Can't alloc code memory")
	fi
	memcpy(p, lib.codeptr, n)

	memset(p+n, 0, tablesize)
!	memset(p+n, 0xAA, tablesize)

	lib.codeptr:=p
	lib.codexsize:=tablesize

	lib.exportaddr:=pcm_alloc(u64.bytes*lib.nexports)
	lib.importxreftable:=pcm_alloc(i16.bytes*lib.nimports)

	if lib.entryoffset<>0xFFFF'FFFF then
		lib.entryaddr:=lib.codeptr+lib.entryoffset
	fi
end

global proc error(ichar mess, param="")=
	if param^ then
		fprintln mess,param
	else
		println mess
	fi
	println "Aborting"
	stop 1
end

global proc loadmemmcu(ref librec lib)=
!load mcu into lib tables and load any dependencies

	int newlib
	ichar name:=lib.libname

	checknew(name,lib.filespec)

	newlib:=mxaddlib(name)
	libtable[newlib]:=lib

	loadimports(lib)
end

global proc checknew(ichar name, filename)=
	if findlib(name) then
		error("Lib already exists:",filename)
	fi
end

global func findlib(ichar name)int n=
!find an existing library existing

	for i to nlibs do
		if eqstring(name,libnametable[i]) then return i fi
	od
	return 0
end

global func mxaddlib(ichar name)int n=
!add a new lib slot with given name
	if nlibs>=maxlibs then 
		error("Too many libs")
	fi

	libnametable[++nlibs]:=name
	return nlibs
end

!export proc fixuplib(ref librec lib)=
global proc fixuplib(ref librec lib)=
!do second fixup pass, which is done across global symbols, but then 
!all relocs are done for all libs which are not yet relocated

!	alloclibdata(lib)
!	donewlib(lib)					!update global tables

!global fixups
!	loadimports()

	loaddlls()				!global
	checksymbols()			!global
	dorelocations()			!all libs
end

proc loaddlls=
!load all dll instances
	u64 inst

	for i to ndlllibs when not dllinsttable[i] do
		inst:=os_getdllinst(dllnametable[i])
		if inst=0 then
			error("Can't find DLL: #", dllnametable[i])
		fi
		dllinsttable[i]:=inst
    od
end

func finddllsymbol(ichar name, int &dllindex)ref void p=
!look up symbol in any of the DLLs
!return address, or void if not found
!dllindex is set to dll where it was found

	dllindex:=0
	for i to ndlllibs do
		p:=os_getdllprocaddr(dllinsttable[i], name)
		if p then
			dllindex:=i
			return p
		fi
	od

	return nil
end

proc checksymbols=
	int dllindex,undef:=0
	ref void p

	for i to nsymbols when not symboldefined[i] do
		p:=finddllsymbol(symbolnametable[i], dllindex)
		if p then
			symboladdress[i]:=p
			symboldllindex[i]:=dllindex
			symboldefined[i]:=1
		else
			println "Undef",symbolnametable[i]
			++undef
		fi
	od

	if undef then
!		error("Symbols Undefined")
	fi
end

proc dorelocations=
	for i to nlibs when not librelocated[i] do
		reloclib(libtable[i])
	od
end

proc reloclib(ref librec lib)=
	int index, targetoffset
	ichar name
	ref byte p
	ref byte q
	ref u64 qaddr		!to import address table
	mcxreloc r

!do thunk tables first
	p:=lib.codeptr+lib.codesize
	qaddr:=cast(p+lib.nimports*u64.bytes)

	for i to lib.nimports do
		name:=lib.importnames[i]
		p++^:=0x48
		p++^:=0xFF
		p++^:=0x24
		p++^:=0x25
		(ref u32(p)^:=cast(qaddr))
		p+:=4

		index:=lib.importxreftable[i]
		qaddr++^:=cast(symboladdress[index])

	od

!Now do the actual relocations
	for i to lib.nrelocs do
		r:=lib.reloctable[i]
		case r.segment
		when code_rseg then p:=lib.codeptr+r.offset
		when idata_rseg then p:=lib.idataptr+r.offset
		when zdata_rseg then p:=lib.zdataptr+r.offset
		esac

		case r.reloctype
		when locabs32_rel then
			targetoffset:=ref u32(p)^
			case r.targetsegment
			when code_rseg then
				(ref u32(p)^ := cast(lib.codeptr+targetoffset))
			when idata_rseg then
				(ref u32(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_rseg then
				(ref u32(p)^ := cast(lib.zdataptr+targetoffset))
			esac

		when locabs64_rel then
			targetoffset:=ref u32(p)^
			case r.targetsegment
			when code_rseg then
				(ref u64(p)^ := cast(lib.codeptr+targetoffset))
			when idata_rseg then
				(ref u64(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_rseg then
				(ref u64(p)^ := cast(lib.zdataptr+targetoffset))
			esac

		when impabs64_rel then

			index:=lib.importxreftable[r.stindex]			!global index
			(ref u64(p)^+:=cast(symboladdress[index],u64))

		when impabs32_rel then
			index:=lib.importxreftable[r.stindex]			!global index
!			(ref u32(p)^+:=cast(symboladdress[index],u32))
			(ref u32(p)^+:=cast(symboladdress[index],u64))

		when imprel32_rel then
			if r.segment<>code_rseg then error("imprel32?") fi
			index:=r.stindex								!local index
			q:=lib.codeptr+lib.codesize+(index-1)*8

			(ref u32(p)^ := q-(p+4))	!offset to thunk entry
		esac

	od

	librelocated[lib.libno]:=1

end

global proc loadimports(ref librec plib)=
! load imported libs
! do first fixup pass which sets up tables adds imports/exports to global table
! This is done per libs and can be called on imported sub-libs

	ref librec qlib
	ichar name

	for i to plib.nlibs do
		dosublib(plib.libnames[i])
	od

	alloclibdata(plib)
	dosymbols(plib)
end

proc dosublib(ichar name)=
	ref librec qlib
	int n:=findlib(name)

	if not n then									!not already loaded
		n:=mxaddlib(name)
		println "Loading sublib", name
		qlib:=loadlibfile(addext(name,"ml"),n)		!get mcu
		loadimports(qlib)						!recursive call
	fi
end

global func loadlibfile(ichar filename, int libno)ref librec plib=
!read mcb file into memory, process it into a new librec
	ref byte p

	p:=readmxfile(filename)
	if p=nil then
		error("Can't find #",filename)
	fi

	plib:=readlibfile(filename,p)
	plib.libno:=libno
	libtable[libno]:=plib	
end

proc dosymbols(ref librec lib)=
!Add any dll libs to global table (libs already done)
!Then deal with imported and exported symbols

	int ix, libx, dllx
	ref byte baseaddr

	for i to lib.ndlllibs do
		adddll(lib.dllnames[i])
	od

	for i to lib.nimports do
		ix:=addsymbol(lib.importnames[i])
		lib.importxreftable[i]:=ix
	od

	for i to lib.nexports do
		ix:=addsymbol(lib.exports[i])
		if symboldefined[ix] then
			CPL "Dupl symbol:",lib.exports[i]
			NEXTLOOP
		fi
		symboldefined[ix]:=1

		case lib.exportsegs[i]
		when code_rseg then baseaddr:=lib.codeptr
		when idata_rseg then baseaddr:=lib.idataptr
		when zdata_rseg then baseaddr:=lib.zdataptr
		else baseaddr:=nil
		esac

		symboladdress[ix]:=cast(baseaddr+lib.exportoffsets[i])
		symbollibindex[ix]:=lib.libno

	od
end

func readmxfile(ichar filename)ref byte p=
!read in mx/ml file into an mcb block, add end_dir byte at the end just in case
!return pointer to mcb block

	p:=readfile(filename)
	return nil when p=nil
	(p+rfsize)^:=end_dir		!add eof-marker

	return p
end

proc adddll(ichar name)=
	for i to ndlllibs do
		if eqstring(name,dllnametable[i]) then return fi
	od

	if ndlllibs>=maxdlls then 
		error("Too many DLLs")
	fi

	dllnametable[++ndlllibs]:=name
end

func addsymbol(ichar name)int=
	for i to nsymbols do
		if eqstring(name,symbolnametable[i]) then return i fi
	od

	if nsymbols>=maxsymbols then 
		error("Too many Imports")
	fi

	symbolnametable[++nsymbols]:=name
	return nsymbols
end

proc setspecialglobals(int cmdskip)=
!adjust cmdparams visible to application by setting $cmdskip flag
	for i to nsymbols when symbolnametable[i]^='$' do
		if eqstring(symbolnametable[i],"$cmdskip") then
!CPL "FOUND CMDSKIP"
			(ref byte(symboladdress[i])^:=cmdskip)
		fi
	od
end

global proc runprogram(ref librec lib, int cmdskip=0)=
	ref proc fnptr
	int libno:=lib.libno

	for i to nlibs when i<>libno and not libinitdone[i] do
		calllibinit(libtable[i])
	od

	if lib.entryaddr=nil then
		error("No entry point found")
	fi

	setspecialglobals(cmdskip)

	fnptr:=cast(lib.entryaddr)

	fnptr()

	libinitdone[libno]:=1
end

global proc calllibinit(ref librec lib)=
	ref proc fnptr
	int libno:=lib.libno

	if lib.entryaddr then
		fnptr:=cast(lib.entryaddr)
		fnptr()
	fi
	libinitdone[lib.libno]:=1
end

global func findsymbol(ichar name)ref void=

	for i to nsymbols do
		if eqstring(symbolnametable[i], name) then
			return symboladdress[i]
		fi
	od
	return nil
end

global func loadmx(ichar filename)ref librec plib=
!load mx/ml into mcu then scan for other imported libraries
	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=mxaddlib(name)

	plib:=loadlibfile(filename,newlib)

	loadimports(plib)
	return plib
end

global func loadmemmcb(ichar filename, ref byte p)ref librec plib=
!read from mcb block in memory
!'filename' is just an identifying string

	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=mxaddlib(name)
	plib:=readlibfile(filename,p)
	plib.libno:=newlib
	libtable[newlib]:=plib	

	loadimports(plib)
	return plib
end

=== mx_write.m 0 0 22/41 ===
!Translate SS data directly into MCB block, then write as mx/ml file

ref dbuffer dest

psymbol entrypoint

global proc writemcx(ichar filename)=
	int n

	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	dest:=buffercreate()

	genword32(mcxsig)

	genbyte(version_dir)
	genstring("0.1234")

	countsymbols()
	writerelocs()

	genbyte(zdata_dir)
	genword32(ss_zdatalen)

	genbyte(code_dir)
	genword32(n:=bufferlength(ss_code))
	genblock(bufferelemptr(ss_code,0), n)

	genbyte(idata_dir)
	genword32(n:=bufferlength(ss_idata))

	genblock(bufferelemptr(ss_idata,0), n)

	int ndlls:=0, nlibs:=0
	for i to nplibfiles when plibfiles[i]^<>'$' do
		++ndlls
	od

	genbyte(dlls_dir)
	genword32(ndlls)
!	for i to nplibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nplibfiles when plibfiles[i]^<>'$' do
		genstring(plibfiles[i])
	od

!	genbyte(libs_dir)
!	genword32(nlibs)
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='L' do
!		genstring(libfiles[i])
!	od

	writesymbols()

	genbyte(end_dir)

!CPL "WRITE MX FILE",FILENAME, =DEST.PSTART,DEST.PCURR-DEST.PSTART

	writefile(filename, dest.pstart, dest.pcurr-dest.pstart)
end

global proc writerelocs=
	ref relocrec oldr
	mcxreloc newr
	int n,count
	psymbol d
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	genbyte(reloc_dir)
	genword32(n:=ss_nidatarelocs+ss_ncoderelocs)

	count:=0

	for i in code_seg..idata_seg do
		oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

		while oldr, oldr:=oldr.nextreloc do
			++count
			clear newr

			newr.offset:=oldr.offset
			newr.segment:=(i=code_seg|idata_seg|code_seg)

			d:=ss_symboltable[oldr.stindex]
!CPL "WRITERELOCS",D.NAME,=D.ISIMPORT

			case oldr.reloctype
			when rel32_rel then
				if d.imported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.imported then
					newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
					newr.stindex:=d.impindex
				else
					if oldr.reloctype=addr32_rel then
						newr.reloctype:=locabs32_rel
					else
						newr.reloctype:=locabs64_rel
					fi
					newr.targetsegment:=d.segment
				fi
			else
				axerror("reloc?")
			esac

			genblock(&newr, newr.bytes)

		od
	od
end

!global proc countsymbols=
!	symbol d
!	for i:=1 to ss_nsymbols do
!		d:=ss_symboltable[i]
!		if d.scope=export_scope then d.expindex:=++nsymexports fi
!		if d.isimport then d.impindex:=++nsymimports fi
!	od
!end

proc writesymbols=
	psymbol d
!	u64 epoffset:=-1
	int n
	ichar name

	genbyte(importsymbols_dir)
	genword32(nsymimports)

	for i to ss_nsymbols when ss_symboltable[i].impindex do
		d:=ss_symboltable[i]
		genstring(d.name)
	od

	genbyte(exportsymbols_dir)
	genword32(nsymexports)

	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			if eqstring(d.name, "main") then
				entrypoint:=d
			fi
			genstring(d.name)
		fi
	od

	genbyte(exportsegs_dir)
	genword32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genbyte(d.segment)
		fi
	od

	genbyte(exportoffsets_dir)
	genword32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genword32(d.offset)
		fi
	od

	genbyte(entry_dir)		!must be present; writes 0xFFFFFFFF when no entry point
	if entrypoint then
		genword32(entrypoint.offset)
	else
		genword32(0xFFFF'FFFF)
	fi
end

proc roundsegment(ref dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	od
end

proc genbyte(int x)=
	buffercheck(dest,1)
	dest.pcurr++^:=x
end

proc genword32(int x)=
	buffercheck(dest,4)
	dest.pcurr32++^:=x
end

proc genstring(ichar s)=
	genblock(s, strlen(s)+1)
end

proc genblock(ref void p, int length)=
	buffercheck(dest,length)
	memcpy(dest.pcurr, p, length)
	dest.pcurr+:=length
end
=== mm_cli.m 0 0 23/41 ===

global ichar syslibname=""

!macro SHOW(m) = println m
macro SHOW(m) = eval 0

!main production options; passnames are also file extensions for outputs

global enumdata []ichar passnames =
!								Output (when this is the final step)
	(ma_pass,		"ma"),			! .ma     These are are special
	(getst_pass,	"list"),		! .list
	(getproj_pass,	"proj"),		! .prog

	(pcl_pass,		"pcl"),			! .pcl
	(runpcl_pass,	"pci"),			! interpret
	(mcl_pass,		"asm"),			! .asm
	(obj_pass,		"obj"),			! .obj (via .asm and aa)
	(dll_pass,		"dll"),			! .dll
	(exe_pass,		"exe"),			! .exe
	(mx_pass,		"mx"),			! .mx
	(run_pass,		"(run)"),		! run in-memory
end

!options used in debugging
global enumdata []ichar dpassnames =
	(dload_pass,	$),
	(dparse_pass,	$),
	(dfixup_pass,	$),
	(dname_pass,	$),
	(dtype_pass,	$),
	(dpcl_pass,		$),
	(dmcl_pass,		$),		!all-inclusive up to this point (includes all prev passes)
	(dss_pass,		$),		!only one of these 3 will be done
end

enumdata []ichar optionnames, []byte optionvalues =

!special outputs
	(ma_sw,			"ma",			ma_pass),
	(getst_sw,		"getst",		getst_pass),
	(getproj_sw,	"getproj",		getproj_pass),

!normal production outputs
	(pcl_sw,		"pcl",			pcl_pass),
	(runpcl_sw,		"runp",			runpcl_pass),
	(asm_sw,		"asm",			mcl_pass),
	(nasm_sw,		"nasm",			mcl_pass),
	(obj_sw,		"obj",			obj_pass),
	(dll_sw,		"dll",			dll_pass),
	(exe_sw,		"exe",			exe_pass),		!default
	(mx_sw,			"mx",			mx_pass),
	(run_sw,		"run",			run_pass),		!default with ms.exe

!debugging options; these set debug mode; above are production mode
	(dload_sw,		"dload",		dload_pass),
	(dparse_sw,		"dparse",		dparse_pass),
	(dfixup_sw,		"dfixup",		dfixup_pass),
	(dname_sw,		"dname",		dname_pass),
	(dtype_sw,		"dtype",		dtype_pass),
	(dpcl_sw,		"dpcl",			dpcl_pass),
	(dmcl_sw,		"dmcl",			dmcl_pass),
	(dss_sw,		"dss",			dss_pass),

	(sys_sw,		"sys",			2),
	(minsys_sw,		"min",			1),
	(nosys_sw,		"nosys",		0),

	(opt_sw,		"opt",			0),
	(peephole_sw,	"peep",			0),
	(regoptim_sw,	"regs",			0),

!diagnostic outputs, only relevant for debug mode
	(ast1_sw,		"ast1",			0),
	(ast2_sw,		"ast2",			0),
	(ast3_sw,		"ast3",			0),
	(showpcl_sw,	"showpcl",		0),
	(showasm_sw,	"showasm",		0),
	(st_sw,			"st",			0),
	(stflat_sw,		"stflat",		0),
	(types_sw,		"types",		0),
	(showss_sw,		"showss",		0),
	(showmodules_sw,"modules",		0),
	(shortnames_sw,	"shortnames",	0),

	(pst_sw,		"pst",			0),

	(time_sw,		"time",			0),
	(v_sw,			"v",			2),
	(vv_sw,			"vv",			3),
	(quiet_sw,		"q",			0),
	(help_sw,		"h",			0),
	(help2_sw,		"help",			0),
	(ext_sw,		"ext",			0),
	(out_sw,		"out",			0),
	(outpath_sw,	"outpath",		0),
	(unused_sw,		"unused",		0),

	(rip_sw,		"rip",			1),
	(himem_sw,		"himem",		2),
end


byte msfile

global const logfile="mx.log"

ichar outext=""				!for reporting of primary o/p file

global int startclock, endclock
global int cmdskip

ichar inputfile

global int loadtime
global int parsetime
global int resolvetime
global int typetime
global int pcltime
global int mcltime
global int sstime
global int exetime
global int compiletime

global proc main2=
!proc main=
	unit p,q,r
	int m,fileno,ntokens,t,tt

CPL "BB COMPILER", =NCMDPARAMS


!CPL =STREC.BYTES
!CPL =MCLREC.BYTES
!CPL =PCLNAMES.LEN

	IF ASSEMTYPE='NASM' THEN
		CPL "ASM/NASM VERSION"
	FI

	startclock:=os_clock()
	initdata()

	getinputoptions()

	if prodmode then
		production_compiler()
	else
		debug_compiler()
	fi

	if fverbose>=2 then println "Finished." fi
end

proc debug_compiler=
!CPL "DEBUG COMPILE", =DPASSNAMES[DPASSLEVEL]

	println "Debug Compilation"

	loadproject(inputfile)

	do_parse(fshowast1) when dpasslevel>=dparse_pass

	do_name(fshowast2) when dpasslevel>=dname_pass

	do_type(fshowast3) when dpasslevel>=dtype_pass

	do_genpcl(fshowpcl or fshowpst) when dpasslevel>=dpcl_pass

	do_genmcl(fshowasm) when dpasslevel>=dmcl_pass

	pcl_genss() when dpasslevel>=dss_pass

	showtimings() when fshowtiming

	showlogfile()

end

proc production_compiler=
	println "Production:"
	showcompilemess()

! Do early passes common to all options

	loadproject(inputfile)

	do_parse()
	do_name()
	do_type()
	do_genpcl(passlevel=pcl_pass)

	pcl_runpcl() when passlevel=runpcl_pass				!terminates

! Special outputs can be done at this point (some could be done earlier)

	do_writema(inputfile)	when passlevel=ma_pass			! terminates
	do_getinfo(inputfile)	when passlevel in [getst_pass, getproj_pass]		! terminates
	do_writeexports()		when passlevel=dll_pass


! Deal with chosen output kind

	if passlevel>=mcl_pass then
		do_genmcl(passlevel=mcl_pass)

		case passlevel
		when obj_pass then
			pcl_writeobj(changeext(outfile, "obj"))	

		when exe_pass then
			pcl_writeexe(changeext(outfile, "exe"))	

		when dll_pass then
			pcl_writedll(changeext(outfile, "dll"))	

		when mx_pass then
			pcl_writemx(changeext(outfile, "mx"))	

		when run_pass then
			pcl_exec(cmdskip)
!			do_genss()
!			do_run()

		esac
	fi

CPL "PCL:",PCCURR-PCSTART, PCCURR.SEQNO

	sstime:=psstime
	showtimings() when fshowtiming
end

proc showcompilemess=
	if fverbose>=1 then
		if passlevel=run_pass then
			if not msfile or fverbose>1 then
				println "Compiling",inputfile,"to memory"
			fi
		else
			fprintln "Compiling # to #",inputfile,changeext(outfile,passnames[passlevel])
		fi
	fi
end

proc do_parse(int flog=0)=
	if fverbose=3 then println "PARSE" fi

	int tt:=clock()

	for i to nmodules do
		parsemodule(modules[i])
	od
	parsetime:=clock()-tt

	if prodmode or dpasslevel>=dfixup_pass then
		if fverbose=3 then println "FIXUP" fi
		fixusertypes()
	fi

	fixstartprocs()
!
	if flog then showast("AST1") fi
end

proc do_name(int flog=0)=
	if fverbose=3 then println "NAME" fi

	int tt:=clock()
	rx_typetable()

!	TX_TYPETABLE()

	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
	resolvetime:=clock()-tt

	if flog then showast("AST2") fi
end

proc do_type(int flog=0)=
	if fverbose=3 then println "TYPE" fi

	int tt:=clock()
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od
	tx_allprocs()
	typetime:=clock()-tt

	if flog then showast("AST3") fi
end

proc do_genpcl(int flog=0)=
	if fverbose=3 then println "GENPCL" fi

	int tt:=clock()

	codegen_pcl()

	pcltime:=clock()-tt


PCL_REDUCETEST()

!CPL "GENPCL",=FLOG, =FSHOWPCL

	if flog then
		if fshowpcl or passlevel=pcl_pass then
!CPL "WRITEPCL", OUTFILE
			pcl_writepcl(changeext(outfile, "pcl"))
		fi

		if fshowpst then
			pcl_writepst()
		fi

	fi

end

proc do_genmcl(int flog=0)=
	if fverbose=3 then println "GENMCL" fi

	int tt:=clock()

	pcl_genmcl()
	mcltime:=clock()-tt

	if flog then
		pcl_writeasm(changeext(outfile, "asm"))
	fi
end

!proc do_genss(int fshowss=0)=
!	if fverbose=3 then println "GENSS" fi
!
!	int tt:=clock()
!	genss()
!	sstime:=clock()-tt
!end
!
!proc do_genexe=
!	if fverbose=3 then println "GENEXE" fi
!
!	int tt:=clock()
!
!	genexe(nil, changeext(outfile, outext), passlevel=dll_pass)
!
!	exetime:=clock()-tt
!end

proc initdata=
	imodule pm
	ifile pf

	pcm_init()
	lexsetup()
	initassemsymbols()
	init_tt_tables()
	initbblib()

	pm:=pcm_allocz(modulerec.bytes)

	pm.name:="PROGRAM"

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
	pm.stmodule:=stprogram
	modules[0]:=pm

!	pm.file:=pf:=newsourcefile()

	igetmsourceinfo:=cast(mgetsourceinfo)
	idomcl_assem:=cast(domcl_assem)
	igethostfn:=cast(findhostfn)

end

proc getinputoptions=
	int paramno,pmtype,sw,ncolons,passfixed,extlen
	ichar name,value,ext
	[300]char filespec

!	prodmode:=1
	paramno:=1
	ncolons:=0

	if eqstring(extractfile(os_gethostname()),"ms.exe") then
		msfile:=1
		fverbose:=0
		do_option(run_sw, "")
	fi

	while pmtype:=nextcmdparamnew(paramno,name,value,"m") do
		case pmtype
		when pm_option then

			convlcstring(name)
			for sw to optionnames.len do
				if eqstring(name,optionnames[sw]) then
					do_option(sw,value,paramno)
					exit
				fi
			else
				println "Unknown option:",name
				stop 99
			od
		when pm_sourcefile then
			if inputfile then
				loaderror("Specify one lead module only")
			fi
			convlcstring(name)
			inputfile:=pcm_copyheapstring(name)

			if passlevel in [run_pass, runpcl_pass] then
				cmdskip:=paramno-1+$CMDSKIP
CPL "///////////",CMDSKIP
				exit
			fi

		when pm_libfile then
			loaderror("Lib files go in module headers")
		else
			loaderror("Invalid params")
		esac

	od

	if prodmode=debugmode=0 then
		passlevel:=exe_pass
		outext:="exe"
		prodmode:=1
	fi

	if passlevel in [obj_pass, dll_pass] or
		passlevel=mcl_pass and assemtype='NASM' then highmem:=2
	fi
!	phighmem:=highmem

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "	",,cmdparams[0],"filename[.m]     # Compile project to executable"
		println "	",,cmdparams[0],"-help            # Other options"
		stop

	else
!default output
		outfile:=pcm_copyheapstring(inputfile)
!		if fwritema then
!			outext:="ma"
!		fi

		if destfilename then
			outfile:=destfilename
		fi

		if destfilepath then
			strcpy(&.filespec,destfilepath)
			strcat(extractfile(&.filespec), outfile)
			outfile:=pcm_copyheapstring(&.filespec)	
		fi
	fi

	ext:=extractext(inputfile)
	extlen:=strlen(ext)
	strcpy(filespec, changeext(cmdparams[0],ext))
	convlcstring(filespec)
!CPL =FILESPEC,=INPUTFILE
	if eqstring(filespec, inputfile) and passlevel=exe_pass then
		strcpy(&.filespec+strlen(filespec)-extlen-1, "2.m")
		outfile:=pcm_copyheapstring(filespec)
		println "New dest=",outfile
	fi

	pcl_setflags(highmem:highmem, verbose:fverbose>=2, shortnames:fshortnames,
		hostcmdskip:cmdskip)

end

proc do_option(int sw, ichar value, int paramno=0)=
	static byte outused, outpathused

	if sw in ma_sw..run_sw then
		if prodmode then loaderror("dupl prod option:",OPTIONNAMES[SW]) fi
		passlevel:=optionvalues[sw]
		prodmode:=1
		outext:=passnames[sw]

		if sw=asm_sw and assemtype<>'AA' or sw=nasm_sw and assemtype<>'NASM' then
			loaderror("Wrong WRITEASM")
		fi

		if sw=runpcl_pass then			!in case occurs at end
			cmdskip:=paramno-1+$CMDSKIP
		fi

		return

	elsif sw in dload_sw..dss_sw then
		if debugmode then loaderror("dupl debug option") fi
		dpasslevel:=sw-dload_sw+1
		debugmode:=1
		return

	fi	

	case sw
	when ast1_sw then fshowast1:=1
	when ast2_sw then fshowast2:=1
	when ast3_sw then fshowast3:=1
	when showpcl_sw then fshowpcl:=1
	when showasm_sw then fshowasm:=1
	when st_sw then fshowst:=1
	when stflat_sw then fshowstflat:=1
	when pst_sw then fshowpst:=1
	when types_sw then fshowtypes:=1
	when showss_sw then fshowss:=1
	when showmodules_sw then fshowmodules:=1

	when sys_sw, minsys_sw, nosys_sw then msyslevel:=optionvalues[sw]

	when opt_sw then fpeephole:=1; fregoptim:=1
	when peephole_sw then fpeephole:=1
	when regoptim_sw then fregoptim:=1

	when time_sw then fshowtiming:=1
	when v_sw, vv_sw, quiet_sw then fverbose:=optionvalues[sw]

	when help_sw, help2_sw then showhelp(); stop
	when ext_sw then dointlibs:=0

	when out_sw then
		if outpathused then loaderror("mixed out/path") fi
		destfilename:=pcm_copyheapstring(value)
		outused:=1

	when outpath_sw then
		if outused then loaderror("mixed out/path") fi
		if (value+strlen(value)-1)^ not in ['\\','/'] then
			loaderror("Path needs to end with \\ or /")
		fi
		destfilepath:=pcm_copyheapstring(value)
		outpathused:=1

	when unused_sw then fcheckunusedlocals:=1

	when shortnames_sw then fshortnames:=1

	when rip_sw, himem_sw then highmem:=optionvalues[sw]

	end case

end

proc showcaption=
	println "M Compiler [M7]", $date, $time
end

global proc showhelp=
	static ichar helptext=strinclude(langhelpfile)
	println helptext
end

global proc initassemsymbols=
!initialise hash table from kwddata
	[32]char str
	int i

!	for i to md.mclnames.len when i<>m_sub do
	for i to md.mclnames.len do
		addreservedword(md.mclnames[i]+2,asmopcodesym,i)
	od

	for i to md.dregnames.len do
		addreservedword(md.dregnames[i],regsym,md.regindices[i],md.regsizes[i])
	od


	for i to md.xmmregnames.len do
		addreservedword(md.xmmregnames[i],xregsym,i)
	od

	for i to md.fregnames.len do
		addreservedword(md.fregnames[i],fregsym,i)
	od

	for i to md.mregnames.len do
		addreservedword(md.mregnames[i],mregsym,i)
	od

	for i to md.jmpccnames.len do
		addreservedword(md.jmpccnames[i],jmpccsym,md.jmpcccodes[i])
	od

	for i to md.setccnames.len do
		addreservedword(md.setccnames[i],setccsym,md.setcccodes[i])
	od

	for i to md.cmovccnames.len do
		addreservedword(md.cmovccnames[i],movccsym,md.cmovcccodes[i])
	od

	for i to segmentnames.upb do
		strcpy(&.str,segmentnames[i])
		str[strlen(&.str)-3]:=0
		addreservedword(pcm_copyheapstring(&.str),segnamesym,i)
	od

	static []ichar regnames=("aframe","dframe","astack","dstack","dprog","dsptr")
	static []byte regnos=(r14,r14, r15,r15, r8, r9)
	static []byte sizes=(4,8,4,8,8,8)
	for i to regnames.len do
		addreservedword(regnames[i], regsym, regnos[i], sizes[i])
	od

end

proc do_writeexports=
	[300]char str

	strcpy(str, extractbasefile(outfile))
	writeexports(outfile, changeext(str, "dll"))
!	stop
end

func getoutfilename(ichar file,ext)ichar=
	return pcm_copyheapstring(changeext(file,ext))
end

proc fixstartprocs=
!make sure each module has a start proc
!make sure the lead module has a main proc
	imodule ms
	isubprog ps
	symbol d
	unit p, q
	int s

	for i to nsubprogs do
		ps:=subprogs[i]
		if ps.mainmodule=0 then
!CPL "FSP: SUBPROG",ps.name,"HAS NO MAIN MODULE"
			ps.mainmodule:=ps.firstmodule
		fi
	od


	for i to nmodules do
		ms:=modules[i]
		if ms.ststart then
			subproghasstart[ms.subprogno]:=1
		fi
	od

	for i to nmodules do
		ms:=modules[i]
		if ms.ststart=nil then
			s:=ms.subprogno
!!			if subproghasstart[s] and subprogs[s].firstmodule=i then
			if subproghasstart[s] and subprogs[s].mainmodule=i then
!CPL "ADDING START TO MODULE", MS.NAME
				ms.ststart:=addstartproc(ms.stmodule,"start", program_scope,i)
			fi
		fi

	od
end

func addstartproc(symbol owner, ichar name, int scope,moduleno)symbol stproc=
	stproc:=getduplnameptr(owner,addnamestr(name),procid)
	stproc.scope:=scope
	stproc.moduleno:=moduleno
	stproc.subprogno:=moduletosub[moduleno]
	stproc.code:=makeblock(nil)
	adddef(owner,stproc)
	addtoproclist(stproc)

	return stproc
end

=== mm_blockpcl.m 0 0 24/41 ===
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

global psymbol pnprocs, pprocname, pprocaddr


global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)
macro evallv(p) = evalref(p)
macro evalunitx(p, isref) = (isref|evalref(p)|evalunit(p))
macro evalblock(p) = evalunit(p)

global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a,b,c

	if p=nil then return fi
	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

!CPL "EVALUNIT", JTAGNAMES[P.TAG]

	switch p.tag
	when jconst         then do_const(p)
	when jnull          then
	when jname          then do_name(p)
	when jblock then
		do_block(p)
	when jcall          then
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
		pc_gen(keval)
		setmode_u(a)

	when jandl          then do_andl(p,a,b)
	when jorl           then do_orl(p,a,b)
	when jcmp           then do_setcc(p,a,b)
	when jcmpchain      then do_setccchain(p,a)

	when jbin           then do_bin(p,a,b)
	when jindex         then do_index(p,a,b)

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
	when jisfalsel      then do_isfalsel(p,a)

	when jincr          then
		if p.pclop in [kincrto, kdecrto] then
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
		pc_gen(kassem,genassem(p))
		setmode_u(p)

	when jclear         then do_empty(p,a)

	when jsourceline then
 		gencomment(" ")
 		gencomment(p.a.svalue)

	else
		GERROR_S("UNSUPPORTED TAG ",JTAGNAMES[P.TAG])
		return
	end switch

	if p.mode<>tvoid and not p.resultflag then
		case p.tag
		when jassign, jcall, jsyscall then

		else
IF NOT JSOLO[P.TAG] THEN
GERROR("NOT ALLOWED BY ITSELF")
FI

			pc_gen(kunload)
			setmode_u(p)
		esac
	fi

end

proc evalref(unit p, q=nil)=
	unit a,b,c
	a:=p.a
	b:=p.b
	c:=p.c
	mmpos:=p.pos

	case p.tag
	when jname then
		genpushmemaddr_d(p.def)
		if q then					!addrof may have optional byte offset
			genpushint(q.value)
			pc_genix(kaddpx)
			setmode(tu8)
		fi
		setmode(tu64)
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
!		elsif ttisblock[p.mode] then
!			evalunit(p)

		else
			PRINTUNIT(P)
			gerror("evalref")
		esac
	esac
end

proc evalarray(unit p)=
	case ttbasetype[p.mode]
	when tslice then
		evalunit(p)
		pc_gen(ksliceptr)
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

	case p.tag
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

	when jnotl, jisfalsel then
		case opc
		when kjumpf then
			genjumpcond(kjumpt,q,lab)
		when kjumpt then
			genjumpcond(kjumpf,q,lab)
		esac

	when jistruel then
		genjumpcond(opc, q, lab)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc,q,lab)

	when jcmp then

		gcomparejump(opc,p.pclcond,q,r,lab)

	when jinrange then
		evalunit(q)

		if opc=kjumpt then
			lab2:=createfwdlabel()
			evalunit(r.a)
			pc_gencond(kjumpcc, lt_cc, genlabel(lab2))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pc_gencond(kjumpcc, le_cc, genlabel(lab))
			setmode_u(q)
			definefwdlabel(lab2)
		else
			evalunit(r.a)
			pc_gencond(kjumpcc, lt_cc, genlabel(lab))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pc_gencond(kjumpcc, gt_cc, genlabel(lab))
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
					pc_gencond(kjumpcc, eq_cc, genlabel(lab2))
					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, ne_cc, genlabel(lab))
				fi
				setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s, s:=s.nextunit do
				evalunit(s)
				pc_gencond(kjumpcc, eq_cc, genlabel(lab))
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
					pc_genxy(kswapstk, 1,2)

					pc_gencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), genlabel(lab))

					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, reversecond(p.cmpgenop[i]), genlabel(lab))
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
					pc_genxy(kswapstk,1,2)
					pc_gencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), genlabel(lab2))
					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, p.cmpgenop[i], genlabel(lab))
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
		pc_gen(opc,genlabel(lab))
		if ttisblock[p.mode] then gerror("jumpt/f") fi
		setmode(p.mode)
	end
end

proc gcomparejump(int jumpopc,int cond,unit lhs,rhs,int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	evalunit(lhs)
	evalunit(rhs)

	pc_gencond(kjumpcc, cond, genlabel(lab))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pc_gen(kjump,genlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #",mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] or mode=tbool then
		genpushint(p.value)
	elsif ttisreal[mode] then
		genpushreal(p.xvalue, getpclmode(mode))

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
		setmode(tu64)
	when labelid then
		if d.index=0 then
			d.index:=++mlabelno
		fi
		if p.resultflag then		!get label address
			pc_gen(kload, genlabel(d.index))
		else
			pc_gen(kjump, genlabel(d.index))
		fi
		p.resultflag:=0
		p.mode:=tvoid
		setmode(tu64)

	when fieldid then
		genpushint(d.offset)
		setmode(ti64)

	else
!GENCOMMENT("LOAD NAME")
		genpushmem_d(d)
		setmode(d.mode)

	esac
end

proc do_stop(unit p,a) =
	if a then
		evalunit(a)
	else
		pc_gen(kload,genint(0))
	fi
	pc_gen(kstop)
end

proc do_andl(unit p,a,b) =
	int labfalse, labend

	pc_gen(kstartmx)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf,a,labfalse)
	genjumpcond(kjumpf,b,labfalse)

	genpushint(1)
	pc_gen(kresetmx)
	setmode(ti64)

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pc_gen(kendmx)
	setmode(ti64)

	definefwdlabel(labend)
end

proc do_orl(unit p,a,b) =
	int labtrue, labfalse, labend

	pc_gen(kstartmx)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt,a,labtrue)
	genjumpcond(kjumpf,b,labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	pc_gen(kresetmx)
	setmode(ti64)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pc_gen(kendmx)
	setmode(ti64)

	definefwdlabel(labend)
end

proc do_notl(unit p,a) =
	evalunit(a)
	pc_gen(p.pclop)
	setmode(ti64)
end

proc do_istruel(unit p,a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	fi
!CPL "ISTR", STRMODE(P.MODE), STRMODE(A.MODE)
	pc_gen(ktoboolt)
	setmode_u(p)
	setmode2(a.mode)
end

proc do_isfalsel(unit p,a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	fi
!CPL "ISFA", STRMODE(P.MODE), STRMODE(A.MODE)
	pc_gen(ktoboolf)
	setmode_u(p)
	setmode2(a.mode)
end

proc do_typepun(unit p, a) =
	evalunit(a)

	if a.tag=jname then
		a.def.addrof:=1
	fi

	if a.mode=p.mode then return fi
	pc_gen(ktypepun)
	setmode(p.convmode)
	setmode2(a.mode)
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

proc do_assign(unit p,a,b) =
!fstore=1 when result is needed

	if a.tag=jname and not a.def.used then
		RETURN
	FI

	case b.tag
	when jmakelist then					!a:=(x,y,z)
		if not p.resultflag then
			do_assignblock(p,a,b)		!(avoids pushing/popping block data)
			return
		fi

	when jslice then					!a:=b[i..j] etc
		if a.tag=jname then
			doslice(b, b.a, b.b, a.def, p.resultflag)
			return
		fi
	esac

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
		pc_gen(kdouble)
	fi

	case a.tag
	when jname then
		if a.def.nameid in [procid, dllprocid, labelid] then GERROR("Assign to proc?") fi

		pc_gen(kstore, genmem_u(a))

	when jptr then
		evalref(a)
		pc_gen(kistore)
		setmode_u(a)

	when jdotindex then
		evalref(a.a)
		evalunit(a.b)
		pc_gen(kstorebit)
		setmode_u(a.a)
		return
	when jdotslice then
		evalref(a.a)
		evalunit(a.b.a)
		evalunit(a.b.b)
		pc_gen(kstorebf)
		setmode_u(a.a)
		return

	when jif then
		do_if(a, a.a, a.b, a.c, 1)
		pc_gen(kistore)
		setmode_u(a)


	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	esac

	setmode_u(a)
end

proc do_bin(unit p,a,b) =

	evalunit(a)
	evalunit(b)

	if p.pclop in [kaddpx, ksubpx] then
		pc_genix(p.pclop, ttsize[tttarget[a.mode]])
	else
		pc_gen(p.pclop)
		if p.pclop=ksubp then
			pc_setscaleoff(ttsize[tttarget[a.mode]])
		fi
	fi

	setmode_u(p)
end

proc do_setcc(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	pc_gencond(ksetcc, p.pclcond)
	setmode_u(a)
end

proc do_setccchain(unit p,q) =
	int lab1,lab2,i,cond
	unit r

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	i:=1

	pc_gen(kstartmx)

	evalunit(q)
	while r do
		evalunit(r)
		cond:=reversecond(p.cmpgenop[i])
		if r.nextunit then
			pc_genxy(kswapstk,1,2)
			cond:=reversecond_order(cond)
		fi

		pc_gencond(kjumpcc, cond, genlabel(lab1))
		if r.nextunit then pccurr.popone:=1 fi

		setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	od

	genpushint(1)
	pc_gen(kresetmx)
	setmode(ti64)
	pc_gen(kjump, genlabel(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	pc_gen(kendmx)
	setmode(ti64)
	definefwdlabel(lab2)
end

proc do_binto(unit p,a,b)=
	evalunit(b)
	evallv(a)

	pc_gen(p.pclop)
	setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pc_setscaleoff(ttsize[tttarget[a.mode]])
	fi
end

proc do_unary(unit p,a) =
	if p.pclop in [kklen, kkupb] then			!assume working on slice

		evalref(a)
		pc_gen(p.pclop)
		setmode_u(a)
		pccurr.slicelwb:=ttlower[a.mode]
		return
	fi

	evalunit(a)
	pc_gen(p.pclop)
	setmode_u(a)
end

proc do_unaryto(unit p,a)=
	evallv(a)

	pc_gen(p.pclop)
	setmode_u(a)
end

proc do_ptr(unit p,a)=
	evalunit(a)
	pc_gen(kiload)
	setmode_u(p)
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.index=0 then
		d.index:=++mlabelno
	fi

	pc_gen(klabeldef, genmemaddr_d(d))

	pc_gen(klabel,genlabel(d.index))
end

proc do_goto(unit a)=
	symbol d

	if a.tag=jname and a.def.nameid=labelid then
		d:=a.def
		if d.index=0 then
			d.index:=++mlabelno
		fi
		pc_gen(kjump, genlabel(d.index))
	else
		evalunit(a)
		pc_gen(kijump)
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
	pc_gen(kstore,genmem_u(cvar))
	setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b,lab_c,lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		pc_gencond(kjumpcc, le_cc, genlabel(lab_d))
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

	pc_gen(kto,genlabel(lab_b))
	pc_gen(kopnd,genmem_u(cvar))
	setmode(ti64)

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
	if ismult then pc_gen(kstartmx) fi

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf,pcond,lab2)

		evalunitx(plist,isref)
		if ismult then pc_gen(kresetmx); setmode_u(p) fi

		if pcond.nextunit or pelse then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
	od

	if pelse then
		evalunitx(pelse,isref)
		if ismult then pc_gen(kendmx); setmode_u(p) fi
	fi
	definefwdlabel(labend)
end

proc do_return(unit p,a) =
	if a then
		evalunit(a)

		pc_gen(kjumpret, genlabel(retindex))
		setmode_u(a)
	else
		genjumpl(retindex)
	fi
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
	pc_gen(kjumpretm, genlabel(retindex))
	pc_setnargs(nparams)
	p.resultflag:=1
end

proc do_callproc(unit p,a,b) =
	[maxparams]unit paramlist
	int nparams,isptr,nvariadics, nret, isfn
	int nfixedparams
	symbol d, e
	ref[]int32 pmult
	unit q

	isptr:=0
!	isfn:=p.tag=jcallfn


	case a.tag
	when jname then
		d:=a.def

	when jptr then
		d:=ttnamedef[a.mode]
		isptr:=1
	else
		gerror("call/not ptr")
	esac

!	isfn:=p.resultflag
	isfn:=d.mode<>tvoid
!CPL "CALLPROC",P.A.DEF, =ISFN, =P.RESULTFLAG

	nparams:=0
	nvariadics:=0

	q:=b
	nfixedparams:=0
	e:=d.deflist
	while e, e:=e.nextdef do
		if e.nameid=paramid then ++nfixedparams fi
	od

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparams then gerror("maxparams") fi
		paramlist[++nparams]:=q

		if d.varparams and nparams>=nfixedparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		fi
	od

	pc_gen(ksetcall)
	setmode_u(p)

	pccurr.nargs:=nparams

	for i:=nparams downto 1 do			!downto 
		q:=paramlist[i]
		evalunit(q)
		if nvariadics and i>=nvariadics and pccurr.mode=tpr32 then
			pc_gen(kfwiden)
			pccurr.size:=8
			pccurr.mode:=tpr64
			pccurr.mode2:=tpr32
		fi

		pc_gen(ksetarg)
		setmode_u(q)
		pccurr.x:=i
	od
!
	if not isptr then
		pc_gen((isfn|kcallf|kcallp), genmemaddr_d(d))
	else
		evalunit(a.a)
		pc_gen((isfn|kicallf|kicallp))
	fi

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
		setmode_u(p)
	fi

	if d.nretvalues>1 and isfn then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			pc_gen(ktype)
			setmode(pmult[i])
		od
	fi

	if isfn and not p.resultflag then
		pc_gen(kunload)
		setmode_u(p)
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
			nextloop
		when jspace then
			genpc_sysproc(sf_print_space)
			q:=q.nextunit
			nextloop
		else
			fmt:=nil
			r:=q
			m:=q.mode
		esac

		case ttbasetype[m]
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
		esac

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
	pc_gen(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pc_setincr(1)

	if ttisref[m] then
		pc_setincr(ttsize[tttarget[m]])
	fi
end

proc do_incrload(unit p,a) =
	evallv(a)
	pc_gen(p.pclop)
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

	unit pto, pstep, pelse, px, ptoinit, ptemp
	int lab_b,lab_c,lab_d,lab_e
	int a,b,stepx

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit
	ptoinit:=pindex.nextunit

	if pto.tag=jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.parammode=byref_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	fi

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
	pc_gen(kstore,genmem_u(pindex))
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
			pc_gen(kjump, genlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
!			pc_gen((down|kjumpgt|kjumplt),genlabel(lab_e))
			pc_gencond(kjumpcc, (down|gt_cc|lt_cc),genlabel(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			pc_gencond(kjumpcc, (down|lt_cc|gt_cc),genlabel(lab_e))
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
		pc_genx((down|kfordown|kforup),stepx, genlabel(lab_b))
		setmode_u(pindex)
	else
		pc_genx((down|kfordown|kforup),1, genlabel(lab_b))
		setmode_u(pindex)
	fi

	pc_gen(kopnd, genmem_u(pindex))
	setmode(ti64)

	case pto.tag
	when jconst then
		pc_gen(kopnd, genint(pto.value))
		setmode(ti64)
	when jname then
		pc_gen(kopnd, genmem_u(pto))
		setmode(ti64)
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

	unit plocal, pfrom, pto, pelse, passign
	int lab_b,lab_c,lab_d,lab_e
	int a,b
	symbol dto

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
	pc_gen(kstore, genmem_u(pindex))
	setmode_u(pindex)

	if pto.tag not in [jconst, jname] then
		evalunit(pto)
		dto:=getavname(currproc)
		pc_gen(kstore, genmem_d(dto))
		setmode(ti64)
		pto:=createname(dto)
		pto.mode:=dto.mode
		pto.resultflag:=1
	fi

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pc_gen(kjump, genlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pc_gencond(kjumpcc, (down|gt_cc|lt_cc),genlabel(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			pc_gencond(kjumpcc, (down|lt_cc|gt_cc),genlabel(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	pc_genx((down|kfordown|kforup),1, genlabel(lab_b))
	setmode_u(pindex)

	pc_gen(kopnd, genmem_u(pindex))
	setmode(ti64)
	case pto.tag
	when jconst then
		pc_gen(kopnd, genint(pto.value))
	when jname then
		pc_gen(kopnd, genmem_u(pto))
	else
		PC_GEN(KOPND, GENMEM_D(DTO))
!		gerror("forall/to: not const or name")
	esac
	setmode(ti64)

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
	pc_gen(kiswap)
	setmode_u(a)
end

proc do_convert(unit p,a) =

	case p.convcode
	when kksoftconv then
		evalunit(a)
		return
	when kkerror then
		gerror("CONV/ERROR")

	else
		evalunit(a)
		pc_gen(convtopcl[p.convcode])
	esac
	setmode_u(p)

	setmode2(p.convmode)
end

global func checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions,
!or -1 when offsets cannot be combined
	int offset

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
		pc_genix(kaddpx)
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
	pc_gen(kload, genint(offset))
	setmode(ti64)

	pc_genix(kiloadx, 1)
	setmode_u(pdot)
end

proc do_storedot(unit pdot,pfield, rhs) =
	int offset
	unit a,pname

	evalunit(rhs)
	if pdot.resultflag then
		pc_gen(kdouble)
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
	pc_gen(kload, genint(offset))
	setmode(ti64)

	pc_genix(kistorex, 1)

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
	pc_genix(kiloadx, scale, offset)
	setmode_u(p)
end

proc do_storeindex(unit p,parray,pindex,rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(parray,pindex)

	evalunit(rhs)
	if p.resultflag then
		pc_gen(kdouble)
	fi

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pc_genix(kistorex, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	setmode_u(p.a)
end

proc do_indexref(unit parray,pindex) =
	int addoffset,scale
	addoffset:=getindexoffset(parray,pindex)

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pc_genix(kaddpx, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	setmode(tttarget[parray.mode])
end

func getindexoffset(unit parray, &pindex)int offset=
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
	int lab_a, lab_d, labjump, elselab, labstmt,ax,bx,ismult
	[0..maxlabels]pcl labels
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
	dorange:
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

	if ismult then pc_gen(kstartmx) fi

	evalunit(pindex)
	pc_genxy((isfast|kswitchu|kswitch), minlab, maxlab,genlabel(labjump))
	setmode(ti64)
	pc_gen(kopnd,genlabel(elselab))

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		pc_gen(kswlabel,genlabel(elselab))
		labels[i]:=pccurr
	od
	pc_gen(kendsw)

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
		if ismult then pc_gen(kresetmx); setmode_u(p) fi
		if isfast then
			evalunit(pindex)
			pc_genxy(kswitchu, minlab, maxlab,genlabel(labjump))
			setmode(ti64)
			pc_gen(kopnd,genlabel(elselab))
		else
			genjumpl((loopsw|lab_a|lab_d))
		fi
		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse,isref)
		if ismult then pc_gen(kendmx); setmode_u(p) fi
	fi

	if loopsw then
		if isfast then
			evalunit(pindex)
			pc_genxy(kswitchu, minlab, maxlab,genlabel(labjump))
			setmode(ti64)
			pc_gen(kopnd,genlabel(elselab))
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
	[maxlabels]pcl labels
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

	if ismult then pc_gen(kstartmx) fi
	evalunit(a)

	pc_genxy(kswitch, 1, n, genlabel(labjump))
	setmode(ti64)
	pc_gen(kopnd, genlabel(elselab))


	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		pc_gen(kswlabel,genlabel(elselab))
		labels[i]:=pccurr
	od
	pc_gen(kendsw)

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q,isref)
		if ismult then pc_gen(kresetmx); setmode_u(p) fi
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c,isref)
	if ismult then pc_gen(kendmx); setmode_u(p) fi

	definefwdlabel(labend)
end

proc do_case(unit p,pindex,pwhenthen,pelse, int loopsw,isref) =
	const maxcase=500
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, ismult

	int lab_abc, lab_d, labelse
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

	if ismult then pc_gen(kstartmx) fi

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
!			if w.nextunit or wt.nextunit then pc_gen(kdouble) fi
			evalunit(w)
			pc_gencond(kjumpcc, eq_cc, genlabel(w.whenlabel:=labtable[ncases]))
			if w.nextunit or wt.nextunit then pccurr.popone:=1 fi
			setmode_u(w)
			w:=w.nextunit
		od

		wt:=wt.nextunit
	od

skip:
	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		evalunitx(unittable[i],isref)
		if ismult then pc_gen(kresetmx); setmode_u(p) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse,isref)
		if ismult then pc_gen(kendmx); setmode_u(p) fi
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

	pc_gen(kloadbit)
	setmode(ti64)
end

proc do_dotslice(unit p,a,b) =
	evalunit(a)
	evalunit(b.a)
	evalunit(b.b)

	pc_gen(kloadbf)
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

!CPL "DO_SYSCALL"

	setfunctab()
!	if pnprocs=nil then
!		pnprocs:=pc_makesymbol("$nprocs")
!		pprocname:=pc_makesymbol("$procname")
!		pprocaddr:=pc_makesymbol("$procaddr")
!	fi

	case p.fnindex
	when sf_getnprocs then
		pc_gen(kload, genmem(pnprocs))

	when sf_getprocname then
		pc_gen(kload, genmemaddr(pprocname))
		setmode(tu64)
		evalunit(a)
		pc_genix(kiloadx, 8, -8)

	when sf_getprocaddr then
		pc_gen(kload, genmemaddr(pprocaddr))
		setmode(tu64)
		evalunit(a)
		pc_genix(kiloadx, 8, -8)

	else
		GENCOMMENT("SYSCALL/GENERIC")
	esac
	setmode(ti64)
end

proc doslice(unit p,a,b, symbol d, int res) =
!generate separate code for (ptr, length) parts

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
			pc_gen(ksub)
			setmode(ti64)
			genpushint(1)
			pc_gen(kadd)
		fi
		setmode(ti64)

	fi

	pc_gen((res|kstoresld|kstoresl), genmem_d(d))
	setmode(tslice)
end

proc do_assignblock(unit p,a,b) =
!fstore=1 when result is needed
!method used is:
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
	int nlhs,nrhs
	symbol d

	nlhs:=a.length

	case b.tag
	when jcall then
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
			pc_gen(kunload)
			setmode(ttmult[d.mode,i])
		od
	fi
end

proc do_assignmm(unit a,b)=
!(a,b,c):=(x,y,z)
	pushrhs(b.a)			!push rhs elements in right-to-left order
	pc_gen(kloadall)
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
		case a.tag
		when jname then
			pc_gen(kstore,genmem_u(a))
		when jindex, jslice,jdot then
			evalref(a)
			pc_gen(kistore)

		when jptr then
			evalunit(a.a)
			pc_gen(kistore)

		when jif, jcase, jswitch, jselect then
			evalref(a)
			pc_gen(kistore)

		when jdotindex then
			evalref(a.a)
			evalunit(a.b)
			pc_gen(kstorebit)

		else
			cpl jtagnames[a.tag]
			gerror("Bad mult assign element")
		esac

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
	pc_gen(kclear)
	setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value)
end
=== mm_assem.m 0 0 25/41 ===
global func readassemline:unit=
	lex()
	return assembleline(1)
end

global func readassemblock:unit=
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
!			lex()
			exit
		when semisym then		!assume blank line
		else				!assume some asm code follows
			u:=assembleline(0)
			addlistunit(ulist,ulistx,u)
		esac
	od

	return makeblock(ulist)
end

func assembleline(int oneline)unit=
!1st symbol of possible assembler line has been read
!assemble following symbols, end at eol or other separater symbol
!return nassem unit

!const escapesym=atsym
	unit dlist,dlistx,p,pname,q
	int opc,noperands
	ref strec stname

	dlist:=dlistx:=nil

!look at the start of a line first

	if lx.symbol=namesym and nextlx.symbol=colonsym then	!normal label
		p:=createunit0(jlabeldef)
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
				addlistunit(dlist,dlistx,readunit())
				if lx.symbol=commasym then
					lex()
				fi

			until lx.symbol in [semisym,eofsym]
		fi

		return createunit2(jassemmacro,pname,dlist)
	fi

	case lx.symbol
	when andlsym then
		opc:=m_andx
	doop:
		p:=createunit0(jassem)
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
	when kprocsym then
		if lx.subcode=1 then
			opc:=m_sub
			goto doop
		fi
!		recase else
		$else

	elsif lx.symbol=namesym then				!assume opcode

		p:=createunit0(jassem)

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
$else:
	PS("ASM")
		SERROR("ASM???")
	esac

!any labels and opcodes have been read; now look at any operands
	if lx.symbol not in [semisym,eofsym] then

	noperands:=0

		do
			q:=readassemopnd()

			if ++noperands<=3 then
				p.abc[+noperands]:=q
			else
				serror("Too many asm opnds")
			fi

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

func readassemopnd:unit p =
!positioned at 1st symbol of an assem operand, which is not ; or eol or eof
	int reg,regix,scale,prefixmode
	unit pcode

	case lx.symbol
	when intconstsym,realconstsym then
		return readunit()
	when namesym then
		case lx.symptr.subcode
		when regsym then
			p:=createunit0(jassemreg)
			p.index:=lx.symptr.index
			p.regsize:=lx.symptr.regsize
			lex()
			return p
		when xregsym then
			p:=createunit0(jassemxreg)
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
gotprefix:
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
		p:=createunit1(jassemmem,pcode)
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

global proc domcl_assem(unit pcode)=
	return when not pcode or pcode.tag<>jassem

	assemused:=1

	genmc(pcode.asmopcode, genasmopnd(pcode.a),genasmopnd(pcode.b))
	mccodex.cond:=pcode.cond

	case pcode.asmopcode
	when m_pcmpistri,m_pcmpistrm, m_shld, m_shrd then
		if pcode.c=nil or pcode.c.tag<>jconst then gerror("pcmpistr/no imm") fi
		mccodex.c:=pcode.c.value

	esac

end

func genasmopnd(unit p)mclopnd ax=
!	[1..8]byte regmodes=(tpu8, tpu16, 0, tu32, 0,0,0, tpu64)
	psymbol d
	int offset,labno
	unit a				!expr: nil/name/const/(add name, const)
	unit x,y
!	symbol e

	if p=nil then return nil fi

	case p.tag
	when jassemreg then
!		ax:=mgenreg(p.reg, p.regsize)
		ax:=mgenreg(p.reg, regmodes[p.regsize])

	when jconst then
		ax:=mgenint(p.value)

	when jassemmem then
		a:=p.a
		d:=nil
		offset:=labno:=0

		if a then
			case a.tag
			when jconst then
				offset:=a.value
			when jname then
				d:=getpsymbol(a.def)
				if d.id=label_id then
					labno:=fixasmlabel(d)
					d:=nil
				fi
			when jbin then
				x:=a.a
				y:=a.b
				if x.tag=jname and y.tag=jconst then
					d:=getpsymbol(x.def)
					if d.id=label_id then
						labno:=fixasmlabel(d)
						d:=nil
					fi
				else
					goto error
				fi
				offset:=(a.pclop in [kadd,kaddpx]|y.value|-y.value)
			when junary then
				if a.pclop<>kneg then merror("assume/unary") fi
				unless a.a.tag=jconst then gerror("-name") end
				offset:=-a.a.value
			when jsyscall then
MERROR("ASSEM/SYSFN?")
!				labno:=getsysfnlabel(a.opcode)

			else
error:
				cpl jtagnames[a.tag]
				gerror("Can't do memexpr")
			esac
		fi
		ax:=mgenindex(areg:p.reg, ireg:p.regix, scale:p.scale, size:ttsize[p.prefixmode],
			offset:offset, labno:labno, def:d)

	when jname then
		d:=getpsymbol(p.def)
		if d.id=label_id then
			labno:=fixasmlabel(d)
			ax:=mgenlabel(labno)
		else
			ax:=mgenmemaddr(d)
		fi

	when jassemxreg then
		ax:=mgenxreg(p.reg)
	when jbin then				!assume add/sub
		x:=p.a
		y:=p.b
		if x.tag=jname and y.tag=jconst then
			d:=getpsymbol(x.def)
			offset:=(p.pclop in [kadd,kaddpx]|y.value|-y.value)
			if d.id=label_id then
				labno:=fixasmlabel(d)
				ax:=mgenlabel(labno)
			else
				ax:=mgenmemaddr(d)
			fi
			ax.offset:=offset
		else
			gerror("ax:imm/add")
		fi
	else
		cpl jtagnames[p.tag]
		gerror("genasmopnd?")
	esac

	return ax

end

func fixasmlabel(psymbol d)int=
!d.labelno contains the label number that is passed to PCL
!PCL maintains a labelmap[] array to convert such labels to renumbered labels
!Do that translation here, and return that new label
!Note: mapped label is stored as negative value to indicate it's been done
!Will return +ve mapped label

	if d.labelno=0 then
		gerror("FIXASMLABEL: zero")
	fi
	return d.labelno
end

=== mm_decls.m 0 0 26/41 ===
global const maxmodule=200
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=200
!global const maxsuppfile=50

global type symbol		= ref strec
global type unit  		= ref unitrec
global type imodule   	= ref modulerec
global type ifile   	= ref filerec
global type isubprog  	= ref subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =
	byte symbol
	byte subcode
	word16 slength				!string length
	word32 pos: (sourceoffset:24, fileno:8)

	union
		ref strec symptr		!pointer to symbol table entry for name
		int64 value				!64-bit int
		real xvalue				!64-bit float
		word64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
	end
end

global record procrec =
	symbol def
	ref procrec nextproc
end

global record typenamerec=
	symbol owner			!owner of scope where typename was encountered
							!moduleno required by resolvetypename can be derived from owner
!A/B used as follows
!  nil B			Simple typename B
!  A   B			Dotted pair A.B
!  A   nil          I think represents a typeof(x) where x is a name
	symbol defa
	union
		symbol defb
		symbol def
	end
	ref int32 pmode
end

global record posrec=
	word32 pos: (sourceoffset:24, fileno:8)
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record strec = $caligned
	ichar name
	ref strec owner
	ref strec deflist
	ref strec deflistx
	ref strec nextdef
	ref strec nextdupl
	ref strec firstdupl			!point to generic version
	psymbol pdef				!pcl st version

	unit code			!var idata/proc body/taggedunion tag value/etc

	int32 mode
	byte namelen
	byte symbol
	byte nameid
	byte subcode

	int32 index				!misc; eg. holds label numbers
	int32 offset

	word32 pos: (sourceoffset:24, fileno:8)
	word16 flags: (
		isstatic:1,
		used:1,
		txdone:1,
		circflag:1,

		islet:1,
		addrof:1,
!		noreg:1,
		ishandler:1,

		atfield:1,
		atvar:1,
		istabdata:1,			!mark parallel enum/tabdata arrays

		issubprog:1,			!set in resolvetopname: module is also a subprog

		isimport:1)

	byte moduleno
	byte subprogno

	unit equivvar

	struct				!when a proc
		ichar truename			!for imported name only
		ref strec paramlist

		byte asmused			!1 when proc contains asmcode
		byte dllindex			!for dllproc: which dll in dlltable

		byte nretvalues			!function: number of return values (0 for proc)
		byte varparams			!0 or 1; variadic params in B and FF
		byte isthreaded			!0 or 1; variadic params in B and FF
	end

	struct						!when a record or record field
		ref strec equivfield
		uflagsrec uflags
		int32 baseclass
		byte bitfieldwidth		!width of bitfield in record
		byte align				!0, 2, 4, 8, 16 or 255 (auto-align)
		byte bitoffset			!0..31 for bitfields in records
		byte equivoffset
	end

	struct				!when a param name
		ref strec nextparam
		byte parammode			!0=byval_param, in_param, byref_param
		byte optional			!0 or 1	
		byte variadic			!variadic parameter for B code
		byte dummy3				!variadic parameter for B code
	end

	int16 regsize
	int16 maxalign		!for record types (doesn't fit above)

	byte scope
	byte equals			!for vars/params: 1/2/3 means =/:=/::= used

end


global record unitrec =
	byte tag				!jcode tag number
	byte insptr
	byte txcount
	byte spare
	word32 pos: (sourceoffset:24, fileno:8)

	unit nextunit

	union
		struct
			union
				unit	a
				symbol	def
				symbol	labeldef
				int64	value
				word64	uvalue
				real64	xvalue
				ichar	svalue
				int64	range_lower
			end

			union
				unit	b
				int64	range_upper
			end

			union
				unit	c
				[4]i16	cmppclmode
			end
		end
		[3]unit abc
	end

	union						!misc stuff depends on tag
		struct					!const string
			word32 slength
			byte isastring
			char strtype		!0/'B'/'S' = normal / bindata / strdata
		end

		struct					!name
			byte dottedname		!for jname: 1=resolved from fully qualified dotted seq
			byte avcode			!jname for/autovars: 'I','T','S' = index/to/step autovars
		end

		union					!asssemreg/xreg/mem
			struct
				byte reg
				byte regix
				byte scale
				byte prefixmode

				byte regsize
				byte cond
				byte spare2,spare3
			end
			word64 reginfo
		end

		union					!for makelist
			word32 length		!number of elements
			byte makearray		!1 for makelist to create array-var not list-var
		end
		byte addroffirst	!1 for jnameaddr when derived from &.name

		word32 offset			!for jdot
		int32 whenlabel			!label no associated with when expr; for recase
		int32 swapvar			!for j-swap: 1 when swapping var:ref

		struct
			union
				int16 bitopindex	!
				int16 opcindex		!operator nodes
				int16 fnindex		!sf_add_var etc
				int16 condcode		!pcl_eq etc; for jeq etc
				int16 asmopcode		!for jassem
				int16 bfcode
			end
		end
		int32 index
		[4]byte cmpgenop			!cmpchain: up to 8 genops
	end

	int32 mode
	union
		int32 convmode	!convert/typepun: source/target(?) mode (will be widened to give unit mode)
!		int32 memmode	!name/ptr/index/dot: void=LVALUE; non-void=RVALUE
		int32 elemmode	!for jnew/newvar
	end

	byte moduleno
	byte subprogno
	byte initlet		!1 for an assignment that initialises a let
	byte isconst		!1 for jconst, and jmakerange with const range

	byte resultflag		!1 when the result of this unit is needed; 0=void or discarded
	union
		byte pclop			!generic operator for jbin, incr etc
		byte propcode		!kklen etc
		byte inv			!notin
		byte convcode		!kkfix etc
	end
	byte istrueconst	!1 for actual "123" etc, not result of reduction
	byte pclcond		!eq_cc etc
end

!global record fwdrec =
!	ref fwdrec nextfwd
!	int32 offset
!	int16 reltype
!	int16 seg
!end

!global record riprec =
!	ref riprec next
!	u32 offset			!within code segment, offset of d32 field
!	i32 immsize			!0,1,4 bytes of trailing imm field
!end

global record modulerec=
	ichar	name				!module name and base filename
	ifile	file
	int16	moduleno			!useful if using pointer to a source rec
	int16	subprogno
	int16	fileno
	byte	issyslib
	byte	islead				!1 if lead module in sp

	union
		symbol	stmodule
		symbol	def
	end

	symbol	stsubprog
	symbol	stmacro

	symbol	ststart				!nil, or st entry of start()
	symbol	stmain				!nil, or st entry of main()
end

global record filerec=
	ichar	name				!module name and base filename
	ichar	filename			!base filename + extension
	ichar	path				!path where file resides
	ichar	filespec			!full file path
	ichar	text				!pointer to source text, 0-terminated
	ichar	dupl				!for ma files
	int		size				!source file size includes terminator

	byte	issyslib			!1 if a system module
	byte	issupport			!1 if a support file (strinclude); MAY BE STORED ELSEWHERE
	byte	compiled			!1 if compiled
	byte	islead				!1 if lead module in sp

	int16	subprogno
	int16	moduleno			!0, or moduleno

	int16	fileno				!refers to self
	int16	spare

end

global record subprogrec =
	ichar name
	int16 firstmodule			!will be header module or same as mainmodule if no header
	int16 mainmodule			!0, or module containing 'main'
	int16 lastmodule			!always first..lastmodule
!	int16 compiled				!1 if compiled
	byte flags:(compiled:1, issyslib:1)
	byte subprogno
end

global [0..maxmodule]imodule	modules
global [0..maxmodule]byte		moduletosub				!convert module no to subprog no
global [0..maxsubprog]isubprog	subprogs
global [0..maxsourcefile]ifile	sources
global [0..maxsubprog]byte		subproghasstart

global int nmodules
global int nsubprogs
global int nsourcefiles
global int nlibfiles

global symbol stprogram		!root into the symbol table
global symbol stmodule		!main module
global symbol stsubprog
global symbol stsysmodule	!optional sys module (needed for name resolving)
global symbol alldeflist		!link together all (user) symbols
global int currmoduleno				!used when compiling modules

global tokenrec lx				!provides access to current token data
global tokenrec nextlx			!provides access to next token

global [0..maxlibfile]ichar libfiles
!global [maxsuppfile]ichar suppfiles
!global int nsuppfiles

global int mainsubprogno		!index of main subprog (eg. may be before/after syslib)

!global const int maxtype=6'000
global const int maxtype=16'000

global int ntypes

global [0..maxtype]symbol		ttnamedef
global [0..maxtype]symbol		ttowner			!for ttlowerexpr/rtlengthexpr

global [0..maxtype]int32		ttbasetype		!basetype
global [0..maxtype]ichar		ttname

global [0..maxtype]word32		ttsize
global [0..maxtype]byte			ttsizeset
global [0..maxtype]int32		ttlower 		!.lbound (default 1)
global [0..maxtype]int32		ttlength 		!elements in array/record/tuple
global [0..maxtype]ref[]int32	ttmult 			!ttlength elements in tuple

global [0..maxtype]unit			ttdimexpr		!length, lower:length, or lower..upper

global [0..maxtype]int32		tttarget 		!for array/ref types
global [0..maxtype]byte			ttusercat
global [0..maxtype]int32		ttlineno

global [0..maxtype]byte			ttsigned		!is i8 i16 i32 i64
global [0..maxtype]byte			ttisreal		!is r32 r64
global [0..maxtype]byte			ttisinteger		!is i8..i64/u8..u64/c8..c64
global [0..maxtype]byte			ttisshort		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte			ttisref			!is a pointer

global [0..maxtype]byte			ttcat			!is a variant
global [0..maxtype]byte			ttisblock		!is a variant

!global const int maxtypename=4'000
global const int maxtypename=8'000
!global const int maxtypename=12'000
global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..symbolnames.upb]byte typestarterset

global symbol currproc

!global int alineno=0

!global int debug=0
global int assemmode=0
global int headermode=0

global ref procrec proclist,proclistx			!linked list of all procs
global ref procrec staticlist,staticlistx		!linked list of all static
global ref procrec constlist,constlistx		!linked list of all export consts

global unit nullunit

!global int targetbits=64
!global int targetsize=8

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global byte msyslevel=2		!0/1/2 = none/min/normal
global byte mvarlib=0		!0/1 = none/yes
global byte fvarnames=0		!display of names in asm/mcl

!global byte freadma			!1 when .ma file is being compiled
!global byte fwritema

global byte fshowtiming
global byte fshowss
global byte fshowpcl
global byte fshowasm
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowpst
global byte fshowstflat
global byte fshowtypes
global byte fshowmodules
global byte fgetst
global byte fgetproj
global byte fnofile
global byte fpeephole
global byte fregoptim
global byte fcheckunusedlocals=0
global byte highmem=0

global byte dointlibs=fsyslibs
!
!!pcl/mcl-pass are relevant only for x64 target, and not allowed for 
!global enumdata []ichar passnames =
!	(load_pass,		$),
!	(parse_pass,	$),
!	(fixup_pass,	$),
!	(name_pass,		$),
!	(type_pass,		$),
!	(pcl_pass,		$),
!	(mcl_pass,		$),		!all-inclusive up to this point (includes all prev passes)
!	(asm_pass,		$),		!only one of these 3 will be done
!	(obj_pass,		$),		!
!	(exe_pass,		$),		!
!	(dll_pass,		$),		!
!	(run_pass,		$),		!will do up to .exe then run the .exe
!
!end

!passlevel used for compiler debug only
global int passlevel=0
global int dpasslevel=0
global int prodmode=0
global int debugmode=0
global int libmode=0					!1 means eventual ML/LIB target
!global int mxstub=0						!1 to write prog.exe with run.exe+prog.mx
global int fshortnames					!mcl/asm display

global ichar outfile					!one of the following two
global ichar destfilename				!nil, or override outfile
global ichar destfilepath				!nil, or set path of outfile
!global ichar asmfilename				!set from outfile
!global ichar pclfilename				!
!global ichar objfilename				!
!global ichar exefilename				!
!global ichar libfilename				!
!global ichar dllfilename				!
!global ichar expfilename				!

global symbol extendtypelist

global int nunits
global int nunitsmem
GLOBAL INT NUNITS1
!GLOBAL INT NGLOBALS

!global const langnameuc		= "M"
!global const langname		= "m"
!global const langext		= "m"
!global const langextma		= "ma"
!global const langextmauc	= "MA"
!global const langlibname	= "mlib"

!global const langhomedir	= "C:/mx64/"
global const langhomedir	= "C:/bx/"
!global const langhomedir	= "./"

!global const langhomedir	= "C:/mxp/"
!global const langhomedir	= "./"
global const langhelpfile	= "mm_help.txt"


=== mm_diags.m 0 0 27/41 ===
int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

!const tab1="  "
!const tab2="    "
const tab1="\t"
const tab2="\t\t"

!const fshowsymbols=1
const fshowsymbols=0

global proc printst(filehandle f,ref strec p,int level=0)=
	ref strec q

!	if p.symbol<>namesym then
!CPL NAMENAMES[P.NAMEID], P.NAME, P.OWNER.NAME
!		mcerror("PRINTST not name")
!	fi

	printstrec(f,p,level)

	q:=p.deflist

	while q<>nil do
		printst(f,q,level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f,ref strec p,int level)=
	strec dd
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset,n
	const tabstr="    "
	[256]char str

	gs_init(d)

	print @str, p
	gs_str(d,str)
	gs_str(d," ")

	offset:=0
	to level do
		gs_str(d,tabstr)
		offset+:=4
	od
	gs_str(d,":")

	gs_leftstr(d,p.name,28-offset,'-')
	gs_leftstr(d,namenames[p.nameid],12,'.')

	col:=gs_getcol(d)
	dd:=p^


	gs_str(d,"[")
	if p.isimport then
		gs_str(d,"Imp ")
	else
!CPL =P.NAME,P.OWNER.NAME, =P.SCOPE, SCOPENAMES[P.SCOPE]
!CPL =P.NAME, =P.SCOPE, SCOPENAMES[P.SCOPE]
!!		gs_str(d,(p.scope|"Sub ","Prog ", "Exp "|"Mod "))
!		gs_str(d,(p.scope|"Sub ","Prog ", "Exp "|"Mod "))
!GS_STR(D,"//")
		gs_str(d,SCOPENAMES[P.SCOPE])
		gs_str(d," ")
	fi

	if dd.isstatic then
		gs_str(d,"Stat")
	fi

	if dd.nameid=paramid and dd.parammode then
		gs_str(d,parammodenames[dd.parammode])
	fi

	if dd.align then
		gs_str(d,"@@")
		gs_strint(d,dd.align)
		gs_str(d," maxalign:")
		gs_strint(d,dd.maxalign)
		gs_str(d," ")
	fi
	if dd.optional then
		gs_str(d,"Opt ")
	fi
	if dd.varparams then
		gs_str(d,"Var:")
		gs_strint(d,dd.varparams)
		gs_str(d," ")
	fi

	if dd.moduleno then
		if dd.nameid<>subprogid then
			print @&.str,"Modno#",,dd.moduleno
		else
			print @&.str,"Subno#",,dd.subprogno
		fi
		gs_str(d,&.str)
	fi

	if dd.used then
		gs_str(d,"U ")
	fi

	if dd.isthreaded then
		gs_str(d,"Threaded ")
	fi


	gs_str(d,"]")
	gs_padto(d,col+10,'=')

	if p.owner then
		fprint @&.str,"(#)",p.owner.name
		gs_leftstr(d,&.str,18,'-')
	else
		gs_leftstr(d,"()",18,'-')
	fi

	case p.mode
	when tvoid then
		gs_str(d,"Void ")
	else
		GS_STRINT(D,P.MODE)
		GS_STR(D,":")

		gs_str(d,strmode(p.mode))
		gs_str(d," ")
	esac

	case p.nameid
	when fieldid,paramid then
		gs_str(d," Offset:")
		gs_strint(d,p.offset)
		if p.mode=tbitfield then
			gs_str(d," Bitoffset:")
			gs_strint(d,p.bitoffset)
			gs_str(d,":")
			gs_strint(d,p.bitfieldwidth)
		fi

		sprintf(&.str,"%.*s",int(p.uflags.ulength),&p.uflags.codes)
		print @&.str,p.uflags.ulength:"v",ichar(&p.uflags.codes):".*"
		gs_str(d," UFLAGS:")
		gs_str(d,&.str)
		gs_str(d,"-")
		gs_strint(d,p.uflags.ulength)

		if p.code then
			gs_str(d,"/:=")
			gs_strvar(d,strexpr(p.code))
		fi

		if p.nameid=paramid and p.variadic then
			gs_str(d,"...")
		fi
	when procid then

		gs_str(d,"Index:")
		gs_strint(d,p.index)

		gs_str(d," Nret:")
		gs_strint(d,p.nretvalues)

	when dllprocid then
		gs_str(d,"Index/PCaddr:")
		gs_strint(d,p.index)
		if p.truename then
			gs_str(d," Truename:")
			gs_str(d,p.truename)
		fi

	when staticid then
		if p.code then
			gs_str(d,"=")
			gs_strvar(d,strexpr(p.code))
		fi

	when frameid then
		if p.code then
			gs_str(d,":=")
			gs_strvar(d,strexpr(p.code))
		fi

	when constid then
		gs_str(d,"Const:")
		gs_strvar(d,strexpr(p.code))

	when typeid then
		if p.baseclass then
			gs_str(d,"Baseclass:")
			GS_STR(D,"<HAS BASECLASS>")
		fi
!	when enumid then
!		gs_str(d,"Enum:")
!		gs_strint(d,p.index)
!	when dllmoduleid then
!		gs_str(d,"DLL#:")
!		gs_strint(d,p.dllindex)
	esac

	if p.atfield then
		gs_str(d," @")
		gs_str(d,p.equivfield.name)
		gs_str(d," +")
		gs_strint(d,p.equivoffset)
	fi
	if p.atvar then
		gs_strvar(d,strexpr(p.equivvar))
	fi

!gs_str(d," Module# ")
!gs_strint(d,p.moduleno)
!
	gs_str(d," Lineno: ???")
!gs_strint(d,p.lineno iand 16777215)

	gs_println(d,f)

	case p.nameid
	when constid,frameid,staticid,macroid then
		if p.code then
			printunit(p.code,dev:f)
		fi
	esac
end

global proc printstflat(filehandle f)=
symbol p
println @f,"GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
!cpl i
!	if hashtable[i].name and hashtable[i].symbol=namesym then
	p:=hashtable[i]
	if p=nil then nextloop fi
	case p.symbol
	when namesym then
		println @f,i,p,":",p.name,symbolnames[p.symbol],namenames[p.nameid]
		p:=p.nextdupl
		while p do
			println @f,"	",p,p.name,symbolnames[p.symbol],namenames[p.nameid],
				"(From",(p.owner|p.owner.name|"-"),,")"
			p:=p.nextdupl
		od
	esac
od
end

global proc printcode(filehandle f,ichar caption)=
ref strec p
ref procrec pp

pp:=proclist

while pp do
	p:=pp.def
!CPL "PRINTCODE",P.NAME,P.CODE

	print @f,p.name,,"=",(p.scope|"Sub","Prog","Exp"|"Mod")
	if p.owner.nameid=typeid then
		print @f," in record",p.owner.name
	fi
	println @f
	printunit(p.code,0,"1",dev:f)
	println @f
	pp:=pp.nextproc
od
end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=
!p is a tagrec
	ref unitrec q
	ref strec d
	int t
	ichar idname
	int64 a
	real32 x32
	static int cmpchain=0

	if p=nil then
		return
	fi

!CPRINTLN "PRINTUNIT"!,JTAGNAMES[P.TAG]
!CPL "PRINTUNIT",JTAGNAMES[P.TAG]
!RETURN

	if p.pos then
!CPL $LINENO,P.POS
		currlineno:=getlineno(p.pos)
!CPL $LINENO
		currfileno:=p.fileno
	fi
!CPL $LINENO

	print @dev,p,":"
!CPL $LINENO
	print @dev,getprefix(level,prefix,p)
!CPL $LINENO

	idname:=jtagnames[p.tag]
!CPL $LINENO
	print @dev,idname,,": "

!CPL $LINENO
	case p.tag
	when jname then
		d:=p.def

		print @dev,d.name,namenames[d.nameid]

		if d.code then
			print @dev," {",,jtagnames[d.code.tag],,"}"
		fi

		print @dev," ",,getdottedname(d)!,q
		print @dev,(p.dottedname|" {Dotted}"|"")

		if p.c then
			print @dev," Lastcall:",p.c
		fi

		if p.addroffirst then
			print @dev," Addroffirst."
		fi

		print @dev," Moduleno:",p.moduleno

		if p.avcode then print @dev," AV:",char(p.avcode) fi

	PRINT @DEV,=P.INDEX


	when jlabeldef then
		println @dev,p.def.name

	when jconst then
		t:=p.mode
		a:=p.value
		if t=trefchar then
			if p.slength>256 then
				print @dev,"""",,"(LONGSTR)",""" *",,p.slength
			elsif p.slength then
				print @dev,"""",,p.svalue,,""" *",,p.slength
			else
				print @dev,""""""
			fi

		elsecase ttbasetype[t]
		when ti64,ti32,ti16,ti8 then print @dev,int64(a)
		when tu64,tu32,tu16,tu8 then print @dev,word64(a)
		when tc64,tc8 then print @dev,chr(a)

!	when ti32,ti64,ti8,ti16 then
!		print @dev,p.value
!	when tu32,tu64,tu8,tu16 then
!		print @dev,p.uvalue
!		when tr32 then
!			x32:=p.xvalue
!			print @dev,real64(x32)
		when tr32, tr64 then
			print @dev,p.xvalue
		when tref then
			if p.value then
				print @dev,"#",,p.value,P.SLENGTH
			else
				print @dev,"NIL"
			fi
		when tbool then
			print @dev,(p.value|"True"|"False")
		when tarray then
			print @dev, "<ARRAY>",=P.STRTYPE,=P.SLENGTH
		else
			println =typename(t),typename(ttbasetype[t])
			PRINT @DEV,"<PRINTUNIT BAD CONST PROBABLY VOID"
		fi
		print @dev," ",,typename(t)
		if p.isastring then
!			print @dev," <isstr>"
			fprint @dev," <isstr>(#)",p.strtype
		fi

		if p.whenlabel then
			print @dev," *L",,p.whenlabel
		fi

	when jdecimal then
		print @dev,p.svalue,"Len:",p.slength

	when jtypeconst then
		print @dev,typename(p.mode),typename(p.value)

!when joperator then
!	print @dev,jtagnames[p.opcode]+1

	when jbitfield then
		print @dev,bitfieldnames[p.bfcode]+3

	when jconvert,jtypepun then
!	print @dev,pclnames[p.pclop]," Convmode:",strmode(p.convmode)
		print @dev," Convmode:",strmode(p.convmode)

	when jmakelist then
		print @dev,"Len:",p.length," Makeax:",p.makearray

	when jdot then
		print @dev,"Offset:",p.offset

	when jindex, jptr then

	when jexit,jredo,jnext then
		print @dev,"#",,p.index

	when jsyscall then
		print @dev,sysfnnames[p.fnindex]+3

	when jassem then
!	print @dev,mclnames[p.index]+2
!	if p.index in [m_jmpcc, m_setcc, m_cmovcc] then
!		print @dev," ",condnames[p.cond],=P.COND
!	fi

	when jassemreg then
!	print @dev,regnames[p.reg],"size:",p.regsize

	when jassemxreg then
!	print @dev,xmmregnames[p.reg]

	when jassemmem then
!	ichar plus
!	plus:=""
!	if p.prefixmode then print @dev,strmode(p.prefixmode) fi
!	print @dev,"["
!	if p.reg then 
!		print @dev,regnames[p.reg]
!		plus:="+"
!	fi
!	if p.regix then 
!		print @dev,plus,,regnames[p.regix]
!!		plus:="+"
!	fi
!	if p.scale>1 then
!		print @dev,"*",,p.scale
!	fi
!	print @dev,"]"
!
!when junary, jbin, junaryto, jbinto, jcmp then
!	if p.opindex then
!		print @dev,pclnames[p.opindex]
!	else
!		print @dev,pclnames[p.pclop]
!	fi

	when jmakeset then
!	if p.isconst then
!		print @dev,p.range_lower,,"..",,p.range_upper
!	fi
	when jcmpchain then
		for i to p.cmpgenop.len do
			if p.cmpgenop[i]=0 then exit fi
			print @dev,ccnames[p.cmpgenop[i]],," "
		od
	esac

	if p.isconst then
		print @dev," Is const"
	else
		print @dev," Not const"
	fi

	case p.tag
	when jbin, jbinto, junary, junaryto, jincr,
		jandl, jorl, jnotl, jistruel then
		if p.pclop then
			fprint @dev," Pcl<#>",pclnames[p.pclop]
		else
			fprint @dev," no-op"
		fi
	when jprop then
		fprint @dev, " Prop<#>", propnames[p.propcode]
	when jconvert then
		fprint @dev, " Conv<#>", convnames[p.convcode]
	when jcmp then
		fprint @dev," Pclcond<#>",ccnames[p.pclcond]
	esac


	println @dev

	for i to jsubs[p.tag] do
		printunitlist(dev,p.abc[i],level+1,strint(i))
	od
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=
	if p=nil then return fi

	while p do
		printunit(p,level,prefix,dev)
		p:=p.nextunit
	od
end

func getprefix(int level,ichar prefix,ref unitrec p)ichar=
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr
	ichar isexpr

	indentstr[1]:=0
	if level>10 then level:=10 fi

	to level do
		strcat(&.indentstr,"- ")
	od

	isexpr:="S"
	if jisexpr[p.tag] then isexpr:="E" fi

	case p.tag
	when jif, jswitch, jcase, jselect then
		if p.mode=tvoid then
			isexpr:="S"
		fi
	esac

	fprint @&.modestr,"# #:#",isexpr,(p.resultflag|"RES"|"---"),strmode(p.mode)
	modestr[256]:=0

	strcat(&.modestr,"-----------------------------")
	modestr[17]:=' '
	modestr[18]:=0

	strcpy(&.str,getlineinfok())
	strcat(&.str,&.modestr)
	strcat(&.str,&.indentstr)
	strcat(&.str,prefix)
	if prefix^ then
		strcat(&.str," ")
	fi

	return &.str
end

func getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @&.str,"# # ",CURRFILENO:"Z2",currlineno:"z4"
	return &.str
end

global proc printmodelist(filehandle f)=
	int mbase
	static ichar tab="\t"

	PRINTLN @F,=NTYPENAMES
	FOR I TO NTYPENAMES DO
		PRINTLN @F,I,TYPENAMES[I].DEF.NAME
	OD
	PRINTLN @F

	println @f,"MODELIST",ntypes

	for m:=0 to ntypes do
!for m:=tlast to ntypes do
		println @f,m:"4",strmode(m)
		mbase:=ttbasetype[m]

		println @f,tab,"Basetype:",mbase,strmode(mbase)
		println @f,tab,"ttname:",ttname[m]
		println @f,tab,"ttnamedef:",ttnamedef[m],(ttnamedef[m]|ttnamedef[m].name|"-")
		println @f,tab,"Target:",strmode(tttarget[m])
		println @f,tab,"Size:",ttsize[m],"Sizeset",ttsizeset[m]
		fprintln @f,"# Bounds: #..#  Length:#",tab,ttlower[m],ttlower[m]+ttlength[m]-1,ttlength[m]
		if mbase=ttuple then
			print @f,tab,"Mult:"
			for i to ttlength[m] do print @f,strmode(ttmult[m,i]),," " od
			println @f
		fi
		println @f,tab,"Signed:",ttsigned[m]
		println @f,tab,"Isreal:",ttisreal[m]
		println @f,tab,"Isinteger:",ttisinteger[m]
		println @f,tab,"Isshort:",ttisshort[m]
		println @f,tab,"Isref:",ttisref[m]
		println @f,tab,"Isblock:",ttisblock[m]
		println @f
	od
end

global proc showprojectinfo(filehandle dev)=
	imodule pm
	isubprog ps
	static ichar tab="    "
	ichar s
	byte isfirst, ismain

	println @dev,"Project Structure:"
	println @dev,"---------------------------------------"
	println @dev,"Modules",nmodules
	for i to nmodules do
		pm:=modules[i]

!CPL "SPI",I,PM

		if i>1 and pm.subprogno<>modules[i-1].subprogno then
			println @dev
		fi
!cpl $LINENO,MODULETOSUB[I]
		ps:=subprogs[moduletosub[i]]

			isfirst:=ps.firstmodule=i
			ismain:=ps.mainmodule=i

!			if isfirst and ismain then s:="*#"
!			elsif isfirst then s:="* "
!			elsif ismain then s:="# "
!			else s:="  " 
!			fi
			if isfirst and ismain then s:="hm"
			elsif isfirst then s:="h "
			elsif ismain then s:="m "
			else s:="  " 
			fi

			print @dev, tab,i:"2",s,
			pm.name:"16jl", "Sys:",pm.issyslib,
			"Sub:",subprogs[pm.subprogno].name,"Fileno:",pm.fileno

!cpl $LINENO
		if pm.stmacro then
			print @dev," Alias:",pm.stmacro.name
		fi
!cpl $LINENO
		if pm.stmain then
			print @dev,$,pm.stmain.name,":",scopenames[pm.stmain.scope],pm.stmain
		fi
!cpl $LINENO
		if pm.ststart then
			print @dev,$,pm.ststart.name,":",scopenames[pm.ststart.scope],pm.ststart
		fi
!cpl $LINENO

		println @dev
	od
	println @dev

	println @dev,"Subprograms",nsubprogs, =mainsubprogno
	for i to nsubprogs do
		ps:=subprogs[i]
!CPL "SPI2",I,Ps
		println @dev, tab,i,ps.name,"Sys:",ps.issyslib!,=PS.STSUBPROG

		if ps.firstmodule then
			print @dev, tab,tab
			for j:=ps.firstmodule to ps.lastmodule do
!				print @dev, $,modules[j].name
				print @dev, $,modules[j].name,"(",MODULES[J].STSUBPROG,")"
			od
			println @dev
		fi
	od
	println @dev

	println @dev,"Sourcefiles",nsourcefiles
	ifile pf
	for i to nsourcefiles do
		pf:=sources[i]
		fprintln @dev, "  #:  Name=# File=# Path=# Spec=# Size=#",
			i:"2",pf.name:"jl16", pf.filename:"jl18", pf.path:"20jl", pf.filespec:"30jl", pf.size:"7"
	od
	println @dev

	println @dev,"Link files",nlibfiles
	for i to nlibfiles do
		println @dev, tab, libfiles[i]:"16jl"
	od
	println @dev

!	println @dev,"Supp files",nsuppfiles
!	for i to nsuppfiles do
!		println @dev, tab, suppfiles[i]:"16jl"
!	od
!	println @dev

!	println @dev,"Header Variables:"
!	for i to headervars.len do
!		fprintln @dev,"\t#: #",headervarnames[i],headervars[i]
!	od
!	println @dev
!	println @dev,"---------------------------------------"

!	println @dev,"Symboltable:"
!	symbol d:=stprogram.deflist
!	while d, d:=d.nextdef do
!		ichar id
!		case d.nameid
!		when moduleid then id:="Mod"
!		when subprogid then id:="Sub"
!		else id:="---"
!		esac
!		fprintln @dev,"    # # (m#, s#)",d.name,id,d.moduleno, d.subprogno
!	od


end

global proc showlogfile=
	[256]char str
	filehandle logdev
	int size
	ref strbuffer ss

	if not debugmode then return fi

	logdev:=fopen(logfile,"w")

!
!CPL "PRESS KEY..."; if OS_GETCH()=27 then stop fi

	if fshowmodules then showprojectinfo(logdev) fi

	if fshowasm and dpasslevel>=dmcl_pass then
		println @logdev,"PROC ASSEMBLY"
		addtolog(changeext(outfile, "asm"),logdev)
	fi
!CPL "PRESS KEY2..."; OS_GETCH()

	if fshowpcl and dpasslevel>=dpcl_pass then
		addtolog(changeext(outfile, "pcl"),logdev)
	fi
	if fshowpst and dpasslevel>=dpcl_pass then
		addtolog("PSYMTAB", logdev)
	fi

	if fshowast3 and dpasslevel>=dtype_pass then addtolog("AST3", logdev) fi
	if fshowast2 and dpasslevel>=dname_pass then addtolog("AST2", logdev) fi
	if fshowast1 and dpasslevel>=dparse_pass then addtolog("AST1", logdev) fi

	if fshowst then
		showsttree("SYMBOL TABLE",logdev)
	fi
	if fshowstflat then
		showstflat("FLAT SYMBOL TABLE",logdev)
	fi
!
	if fshowtypes then
		printmodelist(logdev)
	fi
!

	size:=getfilesize(logdev)
	fclose(logdev)

	if size then
CPL "PRESS KEY..."; if OS_GETCH()=27 then stop fi
!		print @&.str,"\\m\\ed.bat -w ",logfile
		print @&.str,"\\m\\ed.bat ",logfile

!		if checkfile(langname+langname+".m") then
		if checkfile("mm.m") then
!			os_execwait(&.str,1,nil)
			os_execwait(&.str,0,nil)
		else
			println "Diagnostic outputs written to",logfile
		fi
	fi
end

proc showstflat(ichar caption,filehandle f)=
	println @f,"PROC",caption
	printstflat(f)
	println @f
end

proc showsttree(ichar caption,filehandle f)=
	println @f,"PROC",caption
	printst(f,stprogram)
	println @f

	println @f, "Proc List:"
	ref procrec pp:=proclist
	while pp do
		symbol d:=pp.def
		fprintln @f,"#	#.# (#) Mod:",d,d.owner.name, d.name:"20jl", namenames[d.nameid],
			d.moduleno
		pp:=pp.nextproc
	od
	println @f,"End\n"

	println @f, "DLL Proc List:"
	for i to ndllproctable do
		d:=dllproctable[i]
		fprintln @f,"#	#.# (#) Mod:",d,d.owner.name, d.name:"20jl", namenames[d.nameid],
			d.moduleno
	od
	println @f,"End\n"
end

global proc showast(ichar filename)=
	filehandle f

	f:=fopen(filename,"w")
	return unless f

	println @f,"PROC",filename
	printcode(f,"")
	println @f
	fclose(f)
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s",symbolnames[l.symbol])

	switch l.symbol
	when namesym then
		printstrn(l.symptr.name,l.symptr.namelen)

		if l.subcode then
			fprint " [#]",symbolnames[l.subcode]
		fi

	when intconstsym then
		case l.subcode
		when tint then print l.value,"int"
		when tword then print l.uvalue,"word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """"
		printstr(l.svalue)
		print """",strlen(l.svalue)
	when charconstsym then
		print "'"
		printstr(l.svalue)
		print "'"
!	when decimalconstsym then
!		printstr(l.svalue)
!		print "L"
	when assignsym,addrsym,ptrsym,rangesym,
		andlsym,orlsym,eqsym,cmpsym,addsym,subsym,
		mulsym,divsym,idivsym,iremsym,iandsym,iorsym,ixorsym,shlsym,shrsym,
		minsym,maxsym,powersym then
		print symbolnames[l.symbol]
	elsif l.subcode then
		fprint "SUBCODE:",l.subcode
!	fprint "#",symbolnames[l.subcode]
	end

	print $,=lx.fileno
	println

end

proc showtime(ichar caption, int t)=
	fprintln "# # ms # %", caption:"12jl", t:"5", (t*100.0)/compiletime:"5.1jr"
end

global proc showtimings=
	endclock:=os_clock()
	compiletime:=endclock-startclock

	showtime("Load:",		loadtime)
	showtime("Parse:",		parsetime)
	showtime("Resolve:",	resolvetime)
	showtime("Type:",		typetime)
	showtime("PCL:",		pcltime)
	showtime("MCL:",		mcltime)
	showtime("SS:",			sstime)
	showtime("EXE:",		exetime)
	println "-----------------------------"
	showtime("Total:",		compiletime)
end

=== mm_exportq.m 0 0 28/41 ===
strbuffer sbuffer
ref strbuffer dest=&sbuffer

const expscope=export_scope

global proc writeexports(ichar basefile, modulename)=
	ref strec d
	ref procrec pp
	filehandle f
	ichar outfile

	outfile:=pcm_copyheapstring(changeext(basefile,"q"))

	println "Writing exports file to",outfile

	gs_init(dest)
	for i:=tuser to ntypes do
		d:=ttnamedef[i]
		if d.scope=export_scope and d.name^<>'$' then

			case ttbasetype[i]
			when trecord then
				exportrecord(d)
			else
				wxstr("    global type ")
				wxstr(d.name)
				wxstr(" = ")
				wxstr(strmode(d.mode,0))
				wxline()
			esac
		fi
	od

	pp:=staticlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
!CPL =D.NAME
!			if not d.istabdata then gerror("Exporting statics to Q") fi
			exportstatic(d)
		fi
	od
	if staticlist then wxline() fi

	pp:=constlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportconst(d)
		fi
	od
	if constlist then wxline() fi

!	wxstr((passlevel=dll_pass|"importdll "|"importlib "))
	wxstr("importdll ")
!	wxstr("importlib ")
	wxstr(modulename)
	wxstrln(" =")

	pp:=proclist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if d.scope=export_scope then
			exportproc(d)
		fi
	od

	wxstrln("end importdll")

	f:=fopen(outfile,"wb")
	gs_println(dest,f)
	fclose(f)
end

proc exportstatic(ref strec d)=
	unit p:=d.code, q
	[1024]char str

	return when d.code=nil

	return when p.tag<>jmakelist


!CPL "EXPORTS",P.LENGTH,"LOWER:",TTLOWER[P.MODE]
!PRINTUNIT(P)

	wxstr("global var ")
!	wxmode(d.mode)
!	wxstr(" ")
	wxstr(d.name)
	wxstr(" = (")
	wxstr(strint(ttlower[p.mode]))
	wxstrln(":\\")

	q:=p.a
	for i to p.length do
		wxstr("    ")
		case q.tag
		when jconst then
			case q.mode
			when ti64,tu8 then
				wxstr(strint(q.value))
			when trefchar then
				if strlen(q.svalue)>str.len/2 then gerror("expstr") fi
				wxstr("""")
				convertstring(q.svalue, str)
				wxstr(str)
				wxstr("""")
			ELSE
CPL =STRMODE(Q.MODE)
				GERROR("EXPLIST TYPE?")
			esac
			wxstrln(",")
		else
			gerror("explist?")
		esac
		q:=q.nextunit
	od
	wxstrln(")")
	wxline()
end

proc exportconst(ref strec d)=
	wxstr("global const ")
!	wxmode(d.mode)
!	wxstr(" ")
	wxstr(d.name)
	wxstr(" = ")
	jevalx2(dest,d.code)
	wxline()
end

proc exportproc(ref strec d)=
	ref strec e
	int currmode,needcomma

	wxstr("    ")
	wxstr((d.mode=tvoid|"proc "|"func "))
	wxstr(d.name)
	wxstr("(")

	e:=d.deflist
	needcomma:=0
	currmode:=tvoid

	while e do
		if e.nameid=paramid then
			if needcomma then wxstr(",") fi
			if e.parammode<>byref_param then
				if e.mode<>currmode then
					wxmode(e.mode)
					wxstr(" ")
					currmode:=e.mode
				fi
			else
				wxmode(tttarget[e.mode])
				wxstr(" &")
				currmode:=tvoid
			fi
			wxstr(e.name)
			if e.code then
				wxstr("=")
				if ttisref[e.mode] and e.code.tag=jconst and e.code.value=0 then
					wxstr("nil")
				else
					jevalx2(dest,e.code)
				fi
			fi
			needcomma:=1
		fi
		e:=e.nextdef
	od

	wxstr(")")
	if d.mode then
		wxstr(" => ")
		wxmode(d.mode)
	fi
	wxline()
end

proc wxstr(ichar s)=
	gs_str(dest,s)
end

proc wxstrln(ichar s)=
	gs_strln(dest,s)
end

proc wxline=
	gs_line(dest)
end

proc exportrecord(ref strec d)=
	ref strec e
	ref char flags
	int flag,indent
	const tab="    "

	e:=d.deflist

	wxstr("global type ")
	wxstr(d.name)
	wxstr(" = struct ")
	wxline()

	indent:=1

	while e do
		if e.nameid=fieldid then
			flags:=cast(&e.uflags)
			docase flags^
			when 'S' then
				to indent do wxstr(tab) od
				wxstrln("struct")
				++indent
				++flags
			when 'U' then
				to indent do wxstr(tab) od
				wxstrln("union")
				++indent
				++flags
			else
				exit
			end docase

			to indent do wxstr(tab) od
			wxmode(e.mode)
			wxstr(" ")
			wxstrln(e.name)

			do
				flag:=flags++^
				case flag
				when '*'  then
				when 'E' then
					--indent
					to indent do wxstr(tab) od
					wxstrln("end")
				else
					exit
				esac
			od
		fi

		e:=e.nextdef
	od

	wxstrln("end")
	wxline()
end

proc wxmode(int mode)=
	ichar name
	if mode>=tuser then
		name:=ttnamedef[mode].name
		if name^<>'$' then
			wxstr(name)
			return
		fi
	fi
	wxstr(strmode(mode,0))
end
=== mm_genpcl.m 0 0 29/41 ===
global int retindex
global int initstaticsindex

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

int nvarlocals, nvarparams

macro divider = gencomment("------------------------")

global proc codegen_pcl=
!generate code for module n
	symbol d
	ref procrec pp

	pcl_start(nil, nunits)

	dolibs()

	pp:=staticlist
	while pp do
		d:=pp.def
		dostaticvar(d)
		pp:=pp.nextproc
	od

	gencomment("")

	pp:=proclist
	while pp do
		d:=pp.def
		genprocdef(currproc:=d)
		pp:=pp.nextproc
	od

	scanprocs()

	pcl_end()

end

proc genprocdef (symbol p) =
	imodule ms

	ms:=modules[p.moduleno]
!	nblocktemps:=0

	if p=ms.stmain and moduletosub[p.moduleno]=mainsubprogno then
		genmaindef(p)
		return
	elsif p=ms.ststart then
		genstartdef(p)
		return
	fi

	mmpos:=p.pos
	doprocdef(p)

	retindex:=createfwdlabel()

	divider()
	evalunit(p.code)
	divider()

	definefwdlabel(retindex)

	genreturn()

	pc_endproc()

	gencomment("")
end

proc dolibs=
	for i to nlibfiles when libfiles[i]^<>'$' do
		pc_addplib(libfiles[i])
	od
end

proc dostaticvar(symbol d)=

	if d.isimport then return fi

	if d.scope = program_scope and d.name^='$' then
		if eqstring(d.name,"$cmdskip") then
			d.scope:=export_scope				!export from mlib subprog
		fi
	fi

	if d.atvar=1 then
		return
	elsif d.code then
		pc_gen(kistatic,genmem_d(d))
		setmode(d.mode)
		pc_setalign(getalignment(d.mode))
		genidata(d.code)
	else
dozstatic:
		pc_gen(kzstatic,genmem_d(d))
		setmode(d.mode)
		pc_setalign(getalignment(d.mode))
	fi

end

proc genidata(unit p,int doterm=1, am='A',offset=0)=
	[2000]byte data
	int t,tbase
	byte allbytes, nbytes
	unit q,a
	symbol d
	ref char s

	t:=p.mode
	mmpos:=p.pos
	tbase:=ttbasetype[t]

	case p.tag
	when jconst then
		if ttisref[t] then
			if t=trefchar then
				if p.svalue then
					pc_gen(kdata,genstring(p.svalue))
				else
					pc_gen(kdata,genint(0))
				fi
			else
				pc_gen(kdata,genint(p.value))
			fi
			setmode(ti64)
		elsif ttisreal[t] then
			pc_gen(kdata,genrealimm(p.xvalue, t))
			setmode(t)

		elsif ttbasetype[t]=tarray then
			IF P.STRTYPE=0 THEN GERROR("IDATA/ARRAY/NOT BLOCKDATA") FI
			pc_gen(kdata, gendata(p.svalue, p.slength))

		else						!assume int/word
			pc_gen(kdata,genint(p.value))
			setmode_u(p)
		fi

	when jmakelist then
		q:=p.a

		allbytes:=1
		nbytes:=0
		while q, q:=q.nextunit do
			if q.tag=jconst and q.mode=tu8 and nbytes<data.len then
				data[++nbytes]:=q.value
			else
				allbytes:=0
				exit
			end
		end

		if allbytes and nbytes then		!was all byte constants, not in data[1..nbytes]
!GENPC_COMMENT("SYNTH DATA OP:")
			pc_gen(kdata, gendata(pcm_copyheapstringn(cast(&data), nbytes), nbytes))
		else
			q:=p.a
			while q, q:=q.nextunit do
				genidata(q)
			od
		fi

	when jname then
		d:=p.def
		case d.nameid
		when staticid,procid,dllprocid then
			pc_gen(kdata, genmemaddr_d(d))
			if offset then
				pc_setscaleoff(1, offset)
			fi
			if am='P' then
				setmode(tu64)
			else
				setmode(t)
			fi
		when labelid then
			if d.index=0 then d.index:=++mlabelno fi
			pc_gen(kdata, genlabel(d.index))
			setmode(ti64)
		else
			gerror("Idata &frameXXX")
		esac
		return
	when jconvert then
		genidata(p.a)
	when jshorten then
		pc_gen(kdata,genint(p.a.value))
		setmode(t)

	when jaddrof,jaddroffirst then
		genidata(p.a,am:'P',offset:(p.b|p.b.value|0))
	else
		gerror_s("IDATA: ",jtagnames[p.tag],p)

	esac
end

global func genmem_u(unit p)pcl=
	return genmem(getpsymbol(p.def))
end

global func genmem_d(symbol d)pcl=
	return genmem(getpsymbol(d))
end

global proc genpushmem_d(symbol d)=
	pc_gen(kload,genmem(getpsymbol(d)))
end

global func genmemaddr_d(symbol d)pcl=
	return genmemaddr(getpsymbol(d))
end

global proc genpushmemaddr_d(symbol d)=
	pc_gen(kload,genmemaddr(getpsymbol(d)))
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

global proc genreturn=
!assume returning from currproc
	case currproc.nretvalues
	when 0 then
		pc_gen(kretproc)
	when 1 then
		pc_gen(kretfn)
		setmode(currproc.mode)

	else
		pc_genx(kretfn,currproc.nretvalues)
	esac
end

global func reversecond(int cc)int=
!reverse conditional operator
	case cc
	when eq_cc then cc:=ne_cc
	when ne_cc then cc:=eq_cc
	when lt_cc then cc:=ge_cc
	when le_cc then cc:=gt_cc
	when ge_cc then cc:=lt_cc
	when gt_cc then cc:=le_cc
	esac

	return cc
end

global func reversecond_order(int cc)int=
	case cc
	when eq_cc then cc:=eq_cc
	when ne_cc then cc:=ne_cc
	when lt_cc then cc:=gt_cc
	when le_cc then cc:=ge_cc
	when ge_cc then cc:=le_cc
	when gt_cc then cc:=lt_cc
	esac

	return cc
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

global func findlooplabel(int k,n)int=
!k is 1,2,3 for label A,B,C
!n is a 1,2,3, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") fi
	return loopstack[i,k]
end

global proc genpc_sysfn(int fnindex, unit a=nil,b=nil,c=nil)=
	genpc_sysproc(fnindex, a,b,c, 1)
end

global proc genpc_sysproc(int fnindex, unit a=nil,b=nil,c=nil, int asfunc=0)=
	int nargs:=0, opc
	symbol d
	pcl p
	opc:=0

	pc_gen(ksetcall)
	p:=pccurr

	if c then evalunit(c); pc_gen(ksetarg); ++nargs fi
	if b then evalunit(b); pc_gen(ksetarg); ++nargs fi
	if a then evalunit(a); pc_gen(ksetarg); ++nargs fi

	p.nargs:=nargs

	d:=getsysfnhandler(fnindex)
	if d then
!CPL "SYSPROC",D.NAME
		pc_gen((asfunc|kcallf|kcallp), genmemaddr(getpsymbol(d)))
		pc_setnargs(nargs)
	else
		pc_gen((asfunc|kcallf|kcallp), gennameaddr(sysfnnames[fnindex]+3))
	fi
	pccurr.nargs:=nargs
end

proc start=
	zero_unit.tag:=jconst
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

global func getsysfnhandler(int fn)symbol p=
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

!	report:=passlevel>asm_pass
	report:=1
	report:=0

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

global func findhostfn(int opc)psymbol=
!called from pcl/mcl backend. opc refers to a PCL op

	case opc
	when kpower then			!assume for i64
		getpsymbol(getsysfnhandler(sf_power_i64))

	else
		nil
	esac
end

global proc genpushint(int a)=
	pc_gen(kload, genint(a))
	setmode(ti64)
end

global proc genpushreal(real x, int mode)=
	pc_gen(kload,genreal(x, mode))
	setmode(mode)
end

global proc genpushstring(ichar s)=
	pc_gen(kload,genstring(s))
	setmode(tu64)
end

proc genmaindef(symbol p)=
	symbol d

	mmpos:=p.pos
	doprocdef(p)

	retindex:=createfwdlabel()
	for i to nsubprogs when i<>mainsubprogno do
		d:=modules[subprogs[i].mainmodule].ststart
		docallproc(d)
	od
	d:=modules[subprogs[mainsubprogno].mainmodule].ststart
	docallproc(d)

	divider()
	evalunit(p.code)
	divider()

	definefwdlabel(retindex)

	pc_gen(kload, genint(0))
	pc_gen(kstop)
	genreturn()

	pc_endproc()
	gencomment("")
end

proc genstartdef(symbol p)=
	symbol d
	int lead:=0, m,s

	m:=p.moduleno
	s:=p.subprogno

	if s=mainsubprogno and p.moduleno=subprogs[s].mainmodule then
		LEAD:=1
	elsif p.moduleno=subprogs[s].firstmodule then
		LEAD:=2
	fi

	mmpos:=p.pos
	doprocdef(p)

	retindex:=createfwdlabel()

	if lead then
		for i to nmodules when moduletosub[i]=s and i<>m do
			d:=modules[i].ststart
			docallproc(d)
		od
	fi

	divider()
	evalunit(p.code)
	divider()

	definefwdlabel(retindex)

	genreturn()

	pc_endproc()
	gencomment("")
end

proc initstaticvar(symbol d)=
	if d.code then
		evalunit(d.code)
	fi
	pc_gen(kstore,genmem_d(d))
end

proc docallproc(symbol d)=
!call a simple proc, eg. start(), with no args
	return unless d
	pc_gen(ksetcall)
	pc_setnargs(0)

	pc_gen(kcallp, genmemaddr_d(d))
end

proc doprocdef(symbol d)=
	psymbol p
	symbol e

	pc_defproc(p:=getpsymbol(d), threaded:d.isthreaded)

!	psymbol q, pm, pmx, pl, plx
!	e:=d.deflist
!	pm:=pmx:=pl:=plx:=nil
!	while e, e:=e.nextdef do
!		case e.nameid
!		when paramid then
!			q:=getpsymbol(e)
!
!			if pm=nil then
!				p.nextparam:=pm:=pmx:=q
!			else
!				pmx.nextparam:=q
!				pmx:=q
!			fi
!			++p.nparams
!		when frameid then
!			unless e.atvar and e.equivvar then
!				q:=getpsymbol(e)
!				if pl=nil then
!					p.nextlocal:=pl:=plx:=q
!				else
!					plx.nextlocal:=q
!					plx:=q
!				fi
!				++p.nlocals
!			end
!		esac
!	od

!CPL D.NAME, D.OWNER.NAME

	e:=d.deflist
	while e, e:=e.nextdef do
		case e.nameid
		when paramid then
			pc_addparam(getpsymbol(e))

		when frameid then
			unless e.atvar and e.equivvar then
				pc_addlocal(getpsymbol(e))
			end

		esac
	od
end

proc scanprocs=
	const maxprocs=1000
	[maxprocs]psymbol proctable
	pcl currpcl
	int nprocs:=0

	currpcl:=pcstart

	repeat
		if currpcl.opcode in [kdefproc,ktcproc] and currpcl.def.ishandler then
			if nprocs>=maxprocs then gerror("PCL proctab overflow") fi
			proctable[++nprocs]:=currpcl.def
		fi
		++currpcl
	until currpcl>pccurr

	if nprocs=0 and pnprocs=nil then
		pnprocs:=pc_makesymbol("$nprocs")
		pnprocs.mode:=tpi64
		goto finish
	fi

	setfunctab()

!CPL "SCANP", =PNPROCS, =PPROCADDR

	pc_gen(kistatic, genmem(pprocaddr))
	pccurr.mode:=tpblock
	pccurr.size:=nprocs*8

	for i to nprocs do
		pc_gen(kdata, genmemaddr(proctable[i]))
		setmode(tu64)
	od

	pc_gen(kistatic, genmem(pprocname))
	pccurr.mode:=tpblock
	pccurr.size:=nprocs*8

	for i to nprocs do
		pc_gen(kdata, genstring(proctable[i].name))
		setmode(tu64)
	od

finish:
!IF NPROCS THEN
	pc_gen(kistatic, genmem(pnprocs))
	setmode(ti64)
	pc_gen(kdata, genint(nprocs))
	setmode(ti64)
!FI

end

global proc setfunctab=
	if pnprocs=nil then
		pnprocs:=pc_makesymbol("$nprocs", static_id)
		pprocname:=pc_makesymbol("$procname", static_id)
		pprocaddr:=pc_makesymbol("$procaddr", static_id)
	fi
end
=== mm_lex.m 0 0 30/41 ===
macro hashc(hsum,c)=hsum<<4-hsum+c
!macro hashw(hsum)=(hsum<<5-hsum)
macro hashw(hsum)=hsum

const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsource_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]tokenrec lxnextlx_stack
[maxstackdepth]byte lximport_stack
global int sourcelevel=0
global int lximport

const cr	= 13
const lf	= 10
const tab	= 9

ref char lxsource
ref char lxstart
ref char lxsptr
int lxifcond

int lxfileno
global const hstsize	= 65536
global const hstmask	= hstsize-1

global [0:hstsize]symbol hashtable
[0..255]byte namemap			!0/1/2 = other/name/name-upper

ichar u64maxstr="18446744073709551615"

global proc lex=
	int lena,lenb
	ref char p

	lx:=nextlx				!grab that already read basic token
	lx.sourceoffset:=lxstart-lxsource

!	DO
!	lexreadtoken()
!
!	switch nextlx.symbol
	docase lexreadtoken(); nextlx.symbol
	when eolsym then
		if lx.symbol in [commasym, lsqsym, lbracksym] or
			symboloptypes[lx.symbol]=bin_op and not assemmode and 
			lx.symbol not in [maxsym, minsym] then
		else
			nextlx.symbol:=semisym
			nextlx.subcode:=1
			EXIT
		fi

	when kincludesym then
		doinclude()
!		if doinclude() then		!don't skip symbol
!			exit
!		fi

	when namesym then
		case nextlx.subcode
		when unitnamesym then
			case lx.symbol
			when intconstsym then
				case nextlx.symptr.index
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
!				when thousand_unit then lx.value *:= 1 thousand
!				when kilo_unit then lx.value *:= 1024
!				when mega_unit then lx.value *:= 1048576
!				when giga_unit then lx.value *:= (1048576*1024)
				else
					lxerror("Can't do this unit index")
				esac
				lx.subcode:=setinttype(lx.value)
			when realconstsym then
				lxerror("Unit suffix after float not implem")
			else
				nextlx.symbol:=namesym
				exit
			esac

!		when kheadersym then
!			if not headermode then
!				nextlx.symbol:=namesym
!			else
!				nextlx.symbol:=kheadersym
!				nextlx.subcode:=nextlx.symptr.index
!			fi
!			exit
		else
			nextlx.symbol:=namesym
			exit
		esac

	when rawxnamesym then
		nextlx.symbol:=namesym
		exit

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=1
		else
			exit
		fi

	else
		exit
	end docase
!	end switch
!	od

!	nextlx.pos :=nextlx.pos ior lxfileno<<24
	nextlx.fileno:=lxfileno

end

global proc lexreadtoken=
!read next token into nextlx
	int c,hsum
	ref char sptr, lxsvalue
	int length,commentseen
	ref char p, q
	byte instr
	[256]char str

	nextlx.subcode:=0

	doswitch lxstart:=lxsptr; lxsptr++^
	when 'a'..'z','_','$' then
		lxsvalue:=lxsptr-1
	doname:
		hsum:=lxsvalue^

		sptr:=lxsptr

		docase namemap[c:=sptr++^]
		when 1 then
			hsum:=hsum<<4-hsum+c
		when 2 then
			(sptr-1)^:=c+' '
			hsum:=hsum<<4-hsum+c+' '
		else
			lxsptr:=sptr-1
			exit
		end docase

		if c='"' then
			if lxsvalue+1=ref char(lxsptr) then
				case c:=toupper(lxsvalue^)
				when  'F','R' then 
					readrawstring()
					return
!				when  'S','B' then 
				when  'S','B','A' then 
					readarraystring(c)
					return
				esac
			fi
		fi

		lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))

		return

	when 'A'..'Z' then
		lxsvalue:=lxsptr-1
		lxsvalue^+:=32
		goto doname

	when '0'..'9' then
		lxstart:=lxsptr-1
		case lxsptr^
		when ')',cr,',',' ' then		!assume single digit decimal
			nextlx.symbol:=intconstsym
			nextlx.subcode:=tint
			nextlx.value:=lxstart^-'0'
		when 'x','X' then
			case lxstart^
			when '0' then		!0x
				++lxsptr
				readhex()
			when '2' then
				++lxsptr
				readbin()
			else
				lxerror("Bad base")
			esac
		else
			--lxsptr
			readdec()
		esac
		return

	when '!' then			!comment to eol
docomment:
		docase c:=lxsptr++^
		when 13 then
			++lxsptr
			exit
		when 10 then
			exit
		when 0 then
			--lxsptr
			exit
		end
		nextlx.symbol:=eolsym
		return

	when '#' then
		nextlx.symbol:=hashsym
		return

	when '\\' then			!line continuation

!two stages:
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		docase lxsptr++^			!read until end of this line
		when cr then
!			++nextlx.pos
			++lxsptr				!skip lf
			exit
		when lf then
!			++nextlx.pos
			exit
		when 0 then
			nextlx.symbol:=eofsym
			--lxsptr
			return
		when ' ',tab then
		when '!' then
			commentseen:=1
		else
			if not commentseen then
				lxerror("\\ not followed by eol")
			fi
		end docase
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

		docase lxsptr++^
		when cr then
			++lxsptr				!skip lf
		when lf then
		when ' ',tab then
		else
			--lxsptr
			exit
		end docase

	when '{' then
		nextlx.symbol:=lcurlysym
		return

	when '}' then
		nextlx.symbol:=rcurlysym
		return

	when '.' then
		case lxsptr^
		when '.' then				!.. or ...
			++lxsptr
			if lxsptr^='.' then
				++lxsptr
				nextlx.symbol:=ellipsissym
			else
				nextlx.symbol:=rangesym
				nextlx.subcode:=jmakerange		!helps treat as opsym which all have k-code as subcode
			fi
			return
		elsif lxsptr^ in '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
LXERROR(".123 not done")
!			readrealnumber(nil,0,10)
			return
		else
			nextlx.symbol:=dotsym
			return
		esac

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
			nextlx.subcode:=jassign		!helps treat as opsym which all have k-code as subcode
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
!		if lxsptr^='|' then
!			++lxsptr
!			nextlx.symbol:=dbarsym
!		else
			nextlx.symbol:=barsym
!		fi
		return

	when '^' then
		nextlx.symbol:=ptrsym
		return

	when '@' then
!		if lxsptr^='@' then
!			++lxsptr
!			nextlx.symbol:=datsym
!		else
			nextlx.symbol:=atsym
!		fi
		return

	when '?' then
!		nextlx.symbol:=questionsym
!		return
		p:=str; q:=lxsptr+1
		while q^ not in [cr, lf, 0] do
			p++^:=q++^
		od
		p^:=0

		nextlx.svalue:=pcm_copyheapstring(str)
		nextlx.symbol:=questionsym
		return


	when '~' then
!		nextlx.symbol:=curlsym
!		return

	when '+' then
		nextlx.symbol:=addsym
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kincrto
			return
		fi
		return

	when '-' then
		nextlx.symbol:=subsym
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kdecrto
			return
		when '>' then
			++lxsptr
			nextlx.symbol:=pipesym
			return
		esac
		return

	when '*' then
		if lxsptr^='*' then
			++lxsptr
			nextlx.symbol:=powersym
		else
			nextlx.symbol:=mulsym
		fi
		return

	when '/' then
		nextlx.symbol:=divsym
		return

	when '%' then
		nextlx.symbol:=idivsym
		return

	when '=' then
		case lxsptr^
		when '>' then
			nextlx.symbol:=sendtosym
			++lxsptr
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=eq_cc
		esac
		return

	when '<' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.subcode:=le_cc
		when '>' then
			++lxsptr
			nextlx.subcode:=ne_cc
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
		else
			nextlx.subcode:=lt_cc
		esac
		return

	when '>' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=cmpsym
			nextlx.subcode:=ge_cc
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
		else
			nextlx.symbol:=cmpsym
			nextlx.subcode:=gt_cc
		esac
		return

	when '&' then
		case lxsptr^
			when '&' then
			++lxsptr
			nextlx.symbol:=daddrsym
			nextlx.subcode:=jdaddrvv
		when '.' then
			++lxsptr
			nextlx.symbol:=anddotsym
			nextlx.subcode:=0
		else
			nextlx.symbol:=addrsym
			nextlx.subcode:=jaddrof
		esac
		return

	when '\'' then
		lxreadstring('\'')
		return

	when '"' then
		lxreadstring('"')
		return

	when '`' then
		readrawxname()
		return

	when ' ',tab then

	when cr then
		++lxsptr				!skip lf
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		nextlx.symbol:=eolsym
		return

	when 0 then
		if sourcelevel then
			unstacksource()
			RETURN
		else
			nextlx.symbol:=eofsym
			--lxsptr
			return
		fi

	else
		lxerror("Unknown char")
!		nextlx.symbol:=errorsym
		return

	end doswitch

end

global proc lexsetup=
!do one-time setup:
! clear the hash table and populated it with reserved words

	inithashtable()
end

global proc printstrn(ichar s, int length)=
	if length then
		print length:"v",s:".*"
	fi
end

proc readrawstring=
!positioned at " of F"
!read raw string
	ichar dest
	int c

	nextlx.symbol:=stringconstsym
	nextlx.svalue:=++lxsptr

	dest:=lxsptr				!form string into same buffer

!CPL "RAWSTRING",LXSPTR^

	docase c:=lxsptr++^
	when '"' then
!CPL "QUOTE SEEN"
		if lxsptr^='"' then		!repeated, assume embedded term char
			dest++^:='"'
			++lxsptr
		else			!was end of string
			(lxsptr-1)^:=0
			exit
		fi
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
!CPL "READ<",C:"C",">NEXT=",LXSPTR^,INT(LXSPTR^)
		dest++^:=c
	end docase
end

proc lookup(ref char name, int length, hashindex)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped, j
	symbol d

	j:=hashindex iand hstmask

	d:=hashtable[j]
	wrapped:=0
!CPL "LOOK"

	do
		if d=nil then exit fi

!		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
		if d.namelen=length and memcmp(d.name,name,length)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbol
			nextlx.subcode:=d.subcode
			return
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
		d:=hashtable[j]
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

!	d:=pcm_allocz(strec.bytes)
	d:=pcm_allocnfz(strec.bytes)

	hashtable[j]:=d

	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbol:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbol
!	nextlx.subcode:=d.subcode
end

func lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int j, wrapped, hashvalue

	j:=gethashvaluez(name) iand hstmask

	lx.symptr:=hashtable[j]
	wrapped:=0

	do
		if lx.symptr=nil then
			exit
		elsif eqstring(lx.symptr.name,name) then	!match
			println "Lex dupl name:",name
			stop 1 
!			lxerror("sys dupl name?")
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("SYS:HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
		lx.symptr:=hashtable[j]
	od

!exit when not found; new name will go in entry pointed to by lxsymptr
!	lx.symptr:=pcm_allocz(strec.bytes)
	lx.symptr:=pcm_allocnfz(strec.bytes)
	hashtable[j]:=lx.symptr

	lx.symptr.name:=name				!assume can be shared (stored in a table)
	lx.symptr.namelen:=strlen(name)
	lx.symptr.symbol:=namesym			!usually replaced with actual symbol details

	return 0
end

func gethashvaluez(ichar s)int=
!get identical hash func to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hashc(hsum,c)
	od
	return hashw(hsum)
end

proc inithashtable=
!populate hashtable with standard symbols
	int i
	memset(&hashtable,0,hashtable.bytes)


	for i:=1 to stnames.len do
		lookupsys(stnames[i])

		lx.symptr.symbol:=stsymbols[i]

		case stsymbols[i]
!		when unitnamesym, kheadersym then
		when unitnamesym then
			lx.symptr.index:=stsubcodes[i]
			lx.symptr.subcode:=stsymbols[i]
			lx.symptr.symbol:=namesym		!masquerades as normal identifier
		else
			lx.symptr.subcode:=stsubcodes[i]
		esac
	od
!CPL "INITHASHDONE"
!!
!PRINTHASHTABLE()

end

global proc printhashtable=
	println "Hashtable:"

!	for i:=0 to hstsize-1 do
!		if hashtable[i] then
!CPL I,HASHTABLE[I].NAME
!		fi
!	od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=
	lookupsys(name)

	lx.symptr.symbol:=namesym
	lx.symptr.subcode:=symbol
	lx.symptr.index:=subcode

	lx.symptr.regsize:=regsize
end

proc doinclude=
!	ref strec symptr
!	ref char p
	ichar file
!	int i,lastsymbol,cond,fileno,length
!	[256]char str
	ifile pf

	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
	file:=nextlx.svalue
	convlcstring(file)
	file:=addext(file,"m")		!add in extension if not present; assume same as source

	pf:=getsupportfile(file, path:sources[lxfileno].path)
	lexreadtoken()
	stacksource(pf.fileno)
end

global proc startlex(ifile file)=
!start processing one of the file in sourcefile tables as source code
!assume it is a complete header or module

	lxsource:=lxsptr:=file.text

	nextlx.pos:=0
	lxfileno:=file.fileno

!CPL "STARTLEX",FILE.NAME, FILE.FILENO

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global func addnamestr(ichar name)ref strec=
	tokenrec oldlx
	ref strec symptr

	oldlx:=nextlx
	lookup(name,strlen(name), gethashvaluez(name))
	symptr:=nextlx.symptr
	nextlx:=oldlx

	return symptr
end

global proc ps(ichar caption)=
	print "PS:",caption,,": "
	printsymbol(&lx)
end

global proc psnext(ichar caption)=
	print caption,,": "
	printsymbol(&nextlx)
end

global proc psx(ichar caption)=
	print caption,,": "
	printsymbol(&lx)
	print "	"
	printsymbol(&nextlx)
end

global proc stacksource(int fileno, isimport=0)=
!introduce new source level for macros or include files
!not used for main source

	if sourcelevel>=maxstackdepth then
		lxerror("Include file/macro overflow")
	fi
	++sourcelevel
	lxstart_stack[sourcelevel]:=lxstart
	lxsource_stack[sourcelevel]:=lxsource
	lxsptr_stack[sourcelevel]:=lxsptr
	lxfileno_stack[sourcelevel]:=lxfileno
	lxnextlx_stack[sourcelevel]:=nextlx
	lximport_stack[sourcelevel]:=lximport
	lximport:=isimport

	lxsource:=lxsptr:=sources[fileno].text

	nextlx.pos:=0
	lxfileno:=fileno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global proc unstacksource=
	if sourcelevel>0 then			!check that some source is stacked
		lxstart:=lxstart_stack[sourcelevel]
		lxsource:=lxsource_stack[sourcelevel]
		lxsptr:=lxsptr_stack[sourcelevel]
		nextlx:=lxnextlx_stack[sourcelevel]
		lxfileno:=lxfileno_stack[sourcelevel]
		lximport:=lximport_stack[sourcelevel]

		--sourcelevel
	fi
end

proc readarraystring(int prefix)=
	++lxsptr
	lxreadstring('"')

	if prefix='S' then
		nextlx.subcode:='S'
	else
!		nextlx.slength
		--NEXTLX.SLENGTH
		nextlx.subcode:='B'
	fi
end

func setinttype(word64 a)int=
	if a<=u64(0x7FFF'FFFF'FFFF'FFFF) then
		return ti64
	else
		return tu64
	fi
end

proc readrawxname=
	int c,hsum,length

	nextlx.svalue:=lxsptr
	hsum:=0

	while namemap[c:=lxsptr++^] do
		hsum:=hsum<<4-hsum+c
	od
	--lxsptr

	length:=lxsptr-nextlx.svalue

	if length=0 then
		lxerror("Bad ` name")
	fi
	lookup(nextlx.svalue,length, hashw(hsum))
	nextlx.symbol:=rawxnamesym

	return
end

proc lxerror_s(ichar mess,s)=
	lxerror(mess)
end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

	ichar s,t
	int c, d, length, hasescape, a, n
	[8]char str

	if termchar='"' then
		nextlx.symbol:=stringconstsym
	else
		nextlx.symbol:=charconstsym
		nextlx.subcode:=tint
	fi

!do a first pass that terminates length of final string
	length:=0
	hasescape:=0
	t:=nil

	for pass:=1 to 2 do
!CPL =PASS
		s:=lxsptr
		do
			case c:=s++^
			when '\\' then			!escape char
				hasescape:=1
				c:=s^
				if c>='A'  and c<='Z' then c+:=' ' fi
				++s
				case c
				when 'a' then			!bell ('alert')
					c:=7
				when 'b' then			!backspace
					c:=8
				when 'c','r' then		!carriage return
					c:=cr
				when 'e' then			!escape
					c:=27
				when 'f' then			!formfeed
					c:=12
				when 'h' then
					while s^ <> '\\' do
						c:=readhexcode(&s,2,1)
						if pass=2 then
							t^:=c
						fi
						++t
					od
					++s
					--t					!will overwrite last byte

				when 'l','n' then		!linefeed, or linux/c-style newline
					c:=lf
				when 't' then			!tab
					c:=9
				when 'u','v' then		!reserved for unicode, like \x but with 4 hex digits
					t +:= getutf8(readhexcode(&s, (c='u'|4|6)), (pass=2|t|nil))
					nextloop

				when 'w' then			!windows-style cr-lf
					if pass=2 then
						t^:=cr
					fi
					++t
					c:=lf
				when 'x' then	!2-digit hex code follows
					c:=readhexcode(&s,2)
				when 'y' then			!CCI/SM backwards tab
					c:=16
				when 'z' then			!null (not fully supported in code)
					c:=0
				elsecase c
				when '"' then			!embedded double quote
					c:='"'
				when '\\' then
					c:='\\'
				when '\'' then			!embedded single quote
					c:='\''
				when '0' then
					c:=0
				else
					str[1]:=c; str[2]:=0
					lxerror_s("Unknown string escape: \\%s",&.str)
				end
			when '"','\'' then		!possible terminators
				if c=termchar then		!terminator char
					if s^=c then		!repeated, assume embedded term char
						++s
					else			!was end of string
						exit
					fi
				fi
HASESCAPE:=1
			when cr,lf,0 then
				lxerror("String not terminated")
			esac

			if pass=2 then
				t^:=c
			fi
			++t

		od

		if pass=1 then
			length:=int(t)
!			println "LENGTH IS", LENGTH
!			println =LENGTH
!			println =HASESCAPE
			nextlx.slength:=length+1
			if hasescape then
				nextlx.svalue:=t:=pcm_alloc(length+1)
			elsif length=0 then
				nextlx.svalue:=""
				lxsptr:=s
				return
			else
				nextlx.svalue:=pcm_copyheapstringn(lxsptr,length)
				lxsptr:=s
				return
			fi

		else
			t^:=0
			lxsptr:=s
		fi
	od
end

func readhexcode(ref ref char s, int n, sp=0)int a=
!read n hex digits from from char ptr, and step ptr in the caller
	int c
	a:=0
	for i to n do

		if sp and i.odd then
!CPL "CHECKSP",=SP,=I
			repeat
				c:=(s^)++^
			until c<>' '
		else
			c:=(s^)++^
		fi

		if c in 'A'..'F' then
			a:=a*16+c-'A'+10
		elsif c in 'a'..'f' then
			a:=a*16+c-'a'+10
		elsif c in '0'..'9' then
			a:=a*16+c-'0'
!		elsif c='\\' then
!			--(s^)
!			exit
		else
			lxerror("Bad hex digit")
		fi
	od
	a
end

func getutf8(int c, ref char s)int n =
!convert unicode char c to utf8 sequence at s, consisting of 1-4 bytes, and
!return the number of bytes. s will be zero-terminated
!On error, return zero
	[16]char str
	if s=nil then s:=str fi

	if c<=0x7F then
		n:=1
		s++^:=c

	elsif c<=0x7FF then
		n:=2
		s++^:=2x110_00000 + c.[10..6]
		s++^:=2x10_000000 + c.[5..0]

	elsif c<=0xFFFF then
		n:=3
		s++^:=2x1110_0000 + c.[15..12]
		s++^:=2x10_000000 + c.[11..6]
		s++^:=2x10_000000 + c.[5..0]
	elsif c<=0x10FFFF then
		n:=4
		s++^:=2x11110_000 + c.[20..18]
		s++^:=2x10_000000 + c.[17..12]
		s++^:=2x10_000000 + c.[11..6]
		s++^:=2x10_000000 + c.[5..0]
	else
		n:=0
	fi

	s^:=0
	n
end

proc readdec=
	int c
	ref char dest, destend, pstart
	int islong, length
	[1024]char str
	word a

	islong:=0

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			a:=a*10+c-'0'
			dest++^:=c
		elsecase c
		when 'e','E' then
			lxsptr:=pstart
			readreal()
			return
		when '.' then
			if lxsptr^<>'.' then
				lxsptr:=pstart
				readreal()
				return
			fi
			--lxsptr
			exit

		when '_','\'' then
		when 'l','L' then
			nodecimal()

		when 'b','B' then
			length:=dest-&.str
			if length>64 then lxerror("bin overflow") fi
			dest:=&.str
			a:=0
			to length do
				if dest^>='2' then lxerror("bad bin digit") fi
				a:=a*2+dest++^-'0'
			od
			finish

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>20 or length=20 and strncmp(str,u64maxstr,20)>0 then
		nodecimal()
	fi

finish:
	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readhex=
	int c
	ref char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			a:=a*16+c-'0'
			dest++^:=c

		elsif c in 'A'..'F' then
			dest++^:=c
			a:=a*16+c-'A'+10
		elsif c in 'a'..'f' then
			dest++^:=c-32
			a:=a*16+c-'a'+10

		elsecase c
		when '_','\'' then
		when 'l','L' then
			nodecimal()

		when '.' then
			--lxsptr
			exit

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>16 then
		LXERROR("MAKEDEC")
!		makedecimal(&.str,length,16)
		return
!		lxerror("u64 overflow")
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readbin=
	int c
	ref char dest, destend, pstart
	int length
	[1024]byte str
	word a

	pstart:=lxsptr

	dest:=&.str
	destend:=dest+str.len-10
	a:=0

	do
		case c:=lxsptr++^
		when '0', '1' then
			a:=a*2+c-'0'
			dest++^:=c

		when '_','\'' then
		when 'l','L' then
			nodecimal()

		when '.' then
			--lxsptr
			exit

		elsif c in '2'..'9' then
			lxerror("bin bad digit")
		else
			--lxsptr
			exit
		esac

		if dest>=destend then lxerror("bin overflow") fi
	end
	length:=dest-&.str

	if length>64 then
		nodecimal()
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=setinttype(a)
	nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

	int c,negexpon,dotseen,length, fractlen, expon, expseen
	real x
	[1024]char str
	ichar dest, destend

	dest:=&.str
	destend:=dest+str.len-100
	length:=negexpon:=dotseen:=expseen:=expon:=fractlen:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			dest++^:=c
			++length
			if dotseen then ++fractlen fi
		elsecase c
		when '.' then
			if dotseen then --lxsptr; exit fi
			dotseen:=1
			dest++^:=c

		when 'e','E' then
			if expseen then lxerror("double expon") fi
			expseen:=1
			dest++^:=c
			while lxsptr^=' ' do ++lxsptr od
			if lxsptr^ in ['+','-'] then
				if lxsptr^='-' then negexpon:=1 fi
				dest++^:=lxsptr++^
			fi

			expon:=0
			do
				if (c:=lxsptr++^) in '0'..'9' then
					expon:=expon*10+c-'0'
					dest++^:=c
					if dest>=destend then lxerror("expon?") fi
				elsecase c
				when '_','\'' then
				when 'l','L' then
					dest^:=0
					nodecimal()
					return
				else
					--lxsptr
					exit all
				fi
			end

		when '_','\'' then

		when 'l','L' then
			nodecimal()
			return
		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("r64lit too long") fi
	end
	dest^:=0

!------------------------------------------------------------
! Fast way to convert for ordinary numbers (1e100 migt be slower!)
!------------------------------------------------------------
!CPL =NEGEXPON
	if negexpon then expon:=-expon fi
	expon-:=fractlen
	x:=0.0

	for i:=1 to length+dotseen do		!digits already range-checked
		c:=str[i]
		if c<>'.' then
			x:=x*10.0+c-'0'
		fi
	od

	if expon>=0 then
		to expon do
			x*:=10.0
		od
	else
		to -expon do
			x/:=10.0
		od
	fi

	nextlx.xvalue:=x
!------------------------------------------------------------
! Best way to covert: more accurate representation, but slower
!------------------------------------------------------------
!	nextlx.xvalue:=strtod(str,nil)
!------------------------------------------------------------

	nextlx.symbol:=realconstsym
	nextlx.subcode:=treal

end

proc nodecimal=
	lxerror("Decimal not ready")
end

proc start=
	for c in namemap.bounds do
		if c in 'a'..'z' or c in '0'..'9' or c in ['_','$'] then
			namemap[c]:=1
		elsif c in 'A'..'Z' then
			namemap[c]:=2				!upper case
		fi
	od
end

=== mm_lib.m 0 0 31/41 ===
int autotypeno=0
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=32768
ref unitrec unitheapptr=nil
int remainingunits=0

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

ref strbuffer jdest

global ichar framevarname			!normally nil, set to frame var def to display in comment

!global macro isnum(m)  = (m in tfirstnum..tlastnum)
!global macro isnumx(m) = (m in tfirstnum..tlastnum)
!global macro isnumf(m) = (m in [tr64, tr32])
!global macro isnumi(m) = (m in [ti64, tu64, tc64])
!global macro isbool(m) = (m in [tbool8, tbool64])

global macro isnum(m)  = m <= tlastnum
global macro isnumx(m) = m <= tlastnum
global macro isnumf(m) = m <= tr32
global macro isnumi(m) = (m in [ti64, tu64, tc64])
global macro isbool(m) = (m in [tbool8, tbool64])

global macro isint(m) = m>tr32

global func newstrec:symbol=
	symbol p

!	p:=pcm_alloc(strec.bytes)
!	clear p^

	p:=pcm_allocnfz(strec.bytes)

	p.pos:=lx.pos
	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]
	return p
end

global func getduplnameptr(symbol owner,symptr,int id)symbol=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
	symbol p

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

	p.nextdupl:=symptr.nextdupl
	p.firstdupl:=symptr
	symptr.nextdupl:=p

	return p
end

global proc adddef(symbol owner,p)=
!add new st def p, to existing deflist of owner
!assumes p already has a .owner link to owner, but is not yet part of owner's deflist
!pgeneric points to the 'generic' entry for this name in the main hash table.
!this is needed as it contains the head of the dupl list for this name (linking
!all instances of this name).
!Usually the dupl list is checked to ensure that there are no existing names
!with this same owner. (pgeneric can be nil to avoid this check.)
!ASSUMES THAT P IS LAST THING ADDED TO HEAD OF DUPLLIST (just after PGENERIC)
	symbol q

	if q:=p.nextdupl then
		if q.owner=owner then
			cpl q.name,"in",owner.name
			serror("Duplicate name")
		fi
	fi

	if owner.deflist=nil then			!first def
		owner.deflist:=p
	else
		owner.deflistx.nextdef:=p
	fi

	owner.deflistx:=p
end

global func createname(symbol p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=jname
	u.def:=p
++NUNITS1

	return u
end

global func createunit0(int tag)ref unitrec=
	ref unitrec u

++NUNITS1

	u:=allocunitrec()
	u.tag:=tag
	return u
end

global func createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u
!++NUNITS1
	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	return u
end

global func createunit2(int tag, ref unitrec p,q)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q
	return u
end

global func createunit3(int tag, ref unitrec p,q,r)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	u.b:=q
	u.c:=r
	return u
end

global proc insertunit(unit p,int tag)=
!wrap extra unit around p, with given tag
!p itself is effectively changed
	unit q,nextunit
	int mode

	q:=allocunitrec()
	q^:=p^
	mode:=q.mode
	nextunit:=q.nextunit
	q.nextunit:=nil

	clear p^

	p.tag:=tag
	p.pos:=q.pos
	p.a:=q
	p.mode:=mode
	p.nextunit:=nextunit
	p.resultflag:=q.resultflag
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

global func createconstunit(word64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.value:=a
	u.mode:=t

++NUNITS1

	u.isconst:=1
	return u
end

global func createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.svalue:=s
	u.mode:=trefchar
	u.isastring:=1
!++NUNITS1

	if length=-1 then
		u.slength:=strlen(s)+1
!		u.slength:=strlen(s)
	else
		u.slength:=length
	fi
	return u
end

global func newtypename(symbol a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	fi
	++ntypenames
	typenames[ntypenames].defa:=a		!leave .owner/.pmode to be filled in
	typenames[ntypenames].defb:=b		!used type's mode is used

	typenamepos[ntypenames].pos:=lx.pos

	return -ntypenames
end

global func createusertype(symbol stname)int=
!create new, named user type
	if ntypes>=maxtype then
	cpl ntypes,stname.name
		serror("Too many types")
	fi

	++ntypes
	ttname[ntypes]:=stname.name

	ttnamedef[ntypes]:=stname
	ttbasetype[ntypes]:=tvoid
	ttlineno[ntypes]:=lx.pos

	stname.mode:=ntypes

	return ntypes
end

global func createusertypefromstr(ichar name)int=
!create new, named user type
	symbol stname

	stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
	return createusertype(stname)
end

global func getrangelwbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.a
	else
		p:=createunit1(jprop,p)
		p.propcode:=kklwb
		return p
	fi
end

global func getrangeupbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.b
	else
		p:=createunit1(jprop,p)
		p.propcode:=kkupb
		return p
	fi
end

global func createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
!		for k:=tlast to ntypes do
!			if ttusercat[k]=0 and ttbasetype[k]=tarray and tttarget[k]=target and
!					sameunit(dimexpr, ttdimexpr[k],owner, ttowner[k]) then
!				return k
!			fi
!		od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tarray
	ttlower[m]:=1
	ttdimexpr[m]:=dimexpr
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	ttisblock[m]:=1

	return m
end

func sameunit(unit p,q, symbol powner=nil, qowner=nil)int=
!p are q are units just parses; no name resolving or type checking
!do a simple check to see if they are the same unit
	if p=q then return 1 fi
	if p=nil or q=nil then return 0 fi

	if p.tag<>q.tag then return 0 fi

	case p.tag
	when jconst then
		return p.value=q.value
	when jmakerange,jkeyvalue then
		return sameunit(p.a, q.a) and sameunit(p.b, q.b)
	when jname then
		if p.def=q.def and powner=qowner then
			return 1
		fi
	esac

	return 0

end

global func createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=
!lower is lower bound of array
	int atype,m

	atype:=tarray

	if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=atype
	ttlower[m]:=lower
	ttlength[m]:=length
	IF TARGET<0 THEN
		SERROR("CREATEARRAYMODEK/TARGET NOT RESOLVED")
	FI
	ttsize[m]:=length*ttsize[target]

	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttisblock[m]:=1

	return m
end

global func nextautotype:ichar=
	static [32]char str

	print @&.str,"$T",,++autotypeno
	return &.str
end

global func createslicemode(symbol owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int m

	if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=slicetype
	if dimexpr then
		ttdimexpr[m]:=dimexpr
	else
		ttlower[m]:=1
	fi
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttisblock[m]:=1

	return m
end

global func createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int m

	if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tslice
	ttlower[m]:=lower
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner
	ttisblock[m]:=1

	return m
end

global func createrefmode(symbol owner,int target,typedefx=0)int=
	int k,m
!	int a,b

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes when ttisref[k] do
			if tttarget[k]=target then
				return k
			fi
		od
!		FI
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	storemode(owner,target,tttarget[m])
	ttbasetype[m]:=tref
	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1

	return m
end

global func createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=
!create a ref proc mode; (can't create a proc mode by itself, as it's meaningless)
	int m, mproc

	mproc:=createusertype(stproc)
	stproc.paramlist:=paramlist

	stproc.mode:=prettype
	ttbasetype[mproc]:=tproc

!don't bother looking for similar proc sig; each one is unique
	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	tttarget[m]:=mproc
	ttbasetype[m]:=tref

	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1

	return m
end

global proc copyttvalues(int dest, source)=
	ttsigned[dest]		:= ttsigned[source]
	ttisreal[dest]		:= ttisreal[source]
	ttisinteger[dest]	:= ttisinteger[source]
	ttisshort[dest]		:= ttisshort[source]
	ttisref[dest]		:= ttisref[source]
	ttisblock[dest]		:= ttisblock[source]
end

global func getdottedname(symbol p)ichar=
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	symbol owner

	strcpy(&.str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(&.str2,&.str)
		strcpy(&.str,owner.name)
		strcat(&.str,".")
		strcat(&.str,&.str2)
		owner:=owner.owner
	od
	return &.str
end

global func getavname(symbol owner,int id=frameid)symbol=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

	if id=frameid and owner.nameid<>procid then
		serror("Auto frame not in proc")
	fi

	if id=frameid then
		print @&.str,"av_",,++nextavindex
	else
		print @&.str,"sv_",,++nextsvindex
	fi

	name:=pcm_copyheapstring(&.str)
	addnamestr(name)

	p:=getduplnameptr(owner,addnamestr(name),id)
	p.used:=1

	p.mode:=tint

!CPL "GETAVNAME",NAME,OWNER.NAME

	adddef(owner,p)
	return p
end

global proc unionstr_clear(ref uflagsrec u)=
	((ref word64(u))^:=0)		!clear flags and length togetjer
end

global proc unionstr_append(ref uflagsrec u, int c)=
	if u.ulength=(u.codes.len-1) then
		serror("Uflags overflow/a")
	fi
	++u.ulength
	u.codes[u.ulength]:=c
end

global proc unionstr_concat(ref uflagsrec u, v)=
	int ulen,vlen,i

	ulen:=u.ulength
	vlen:=v.ulength
	if ulen+vlen>u.codes.len then
		serror("Uflags overflow/c")
	fi
	for i:=1 to vlen do
		u.codes[i+ulen]:=v.codes[i]
	od
	u.ulength:=ulen+vlen
end

global func unionstr_last(ref uflagsrec u)int=
	if u.ulength then
		return u.codes[u.ulength]
	fi
	return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
	memcpy(u,v,uflagsrec.bytes)
end

global func createrecordmode(symbol owner,int typedefx)int=
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def:
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	fi
	ttbasetype[m]:=trecord
	ttusercat[m]:=1
	ttisblock[m]:=1

	return m
end

global func createtuplemode(symbol owner,[]int &elements,int elementslen, typedefx)int=
	int m

	if typedefx=0 then
		m:=createusertype(owner)
	else
		m:=typedefx
	fi
	ttbasetype[m]:=ttuple
	ttusercat[m]:=1
	ttlength[m]:=elementslen
	ttmult[m]:=pcm_alloc(elementslen*int32.bytes)
	for i to elementslen do
		storemode(owner,elements[i],ttmult[m,i])
	od

	return m
end

global func convertstring(ichar s, t)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!returns length of t
	int c
	ichar t0:=t
	[16]char str

	while c:=s++^ do
		case c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when 10 then
			t++^:='\\'
			t++^:='n'
		when 13 then
			t++^:='\\'
			t++^:='c'
		when 9 then
			t++^:='\\'
			t++^:='t'
		when '\\' then
			t++^:='\\'
			t++^:='\\'
		when 7,8,26,27 then
			t++^:='<'
			t++^:=c/10+'0'
			t++^:=(c rem 10)+'0'
			t++^:='>'
		elsif c in 32..126 then
			t++^:=c
		else
			t++^:='\\'
			t++^:='x'
			print @str,c:"z2h"
			t++^:=str[1]
			t++^:=str[2]
		esac
	od
	t^:=0

	return t-t0
end

global func strexpr(ref unitrec p)ref strbuffer=
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jevalx2(exprstr,p)
	return exprstr
end

global proc jevalx2(ref strbuffer dest, ref unitrec p)=			!JEVAL
	jdest:=dest
	jevalx(p)
end

global proc jevalx(ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q,a,b
	[500]char str

	if p=nil then
		return
	fi

	a:=p.a
	b:=p.b

	case p.tag
	when jconst then

		case ttbasetype[p.mode]
		when ti32,ti64,ti8,ti16 then
			getstrint(p.value,&.str)
		when tu32,tu64,tu8,tu16 then
			strcpy(&.str,strword(p.uvalue))
		when tc8,tc64 then
			str[1]:=p.uvalue
			str[0]:=0

		when treal,tr32 then
			print @&.str,p.xvalue
		when tref then
			if p.mode=trefchar and p.isastring then
				if p.slength>str.len/2 then
					strcpy(&.str,"LONGSTR)")
				else
					convertstring(p.svalue,&.str)
				fi
				jadditem("""")
				jadditem(&.str)
				jadditem("""")
				return
			else
				print @&.str,ref void(p.value)
			fi
		else
			strcpy(&.STR,"<EVAL/CONST PROBABLY VOID>")
		esac
		jadditem(&.str)

	when jname then
		jadditem(p.def.name)

	when jbin,jcmp then

		strcpy(&.str,pclnames[p.pclop])
		jadditem("(")
		jevalx(a)
		jadditem(&.str)
		jevalx(b)
		jadditem(")")

	when junary, jistruel, jnotl then

		strcpy(&.str,pclnames[p.pclop])
		jadditem(&.str)
		jadditem("(")

		if a.tag=jtypeconst then
			jadditem(STRMODE(a.value))
		else
			jevalx(a)
		fi
		jadditem(")")

	when jprop then

		strcpy(&.str,propnames[p.propcode])
		jadditem(&.str)
		jadditem("(")
		jevalx(a)
		jadditem(")")

	when jcall then
		jevalx(a)
		jadditem("(")

		q:=b
		while q do
			jevalx(q)
			q:=q.nextunit
			if q then jadditem(",") fi
		od
		jadditem(")")

	when jindex,jdotindex,jslice,jdotslice then
		jevalx(a)
		if p.tag=jdotindex or p.tag=jdotslice then
			jadditem(".")
		fi
		jadditem("[")
		jevalx(b)
		jadditem("]")

	when jdot then
		jevalx(a)
		jadditem(".")
		jevalx(b)

	when jmakelist then
		jadditem("(")

		q:=a
		while q do
			jevalx(q)
			q:=q.nextunit
			if q then jadditem(",") fi
		od
		jadditem(")")

	when jmakerange then
		jadditem("(")
		jevalx(a)
		jadditem("..")
		jevalx(b)
		jadditem(")")

	when jassign then
		jevalx(a)
		jadditem(":=")
		jevalx(b)

	when jif then
		jadditem("(")
		jevalx(a)
		jadditem("|")
		jevalx(b)
		jadditem("|")
		jevalx(p.c)
		jadditem(")")

	when jtypeconst then
		jadditem(strmode(p.mode))

	when jconvert,jtypepun then

		jadditem(strmode(p.convmode))
		if p.tag=jtypepun then
			jadditem("@")
		fi
		jadditem("(")
		jevalx(a)
		jadditem(")")

	when jshorten then

		jadditem("shorten(")
		jevalx(a)
		jadditem(")")
	when jautocast then

		jadditem("cast(")
		jevalx(a)
		jadditem(")")
	when jkeyvalue then
		jevalx(a)
		jadditem(":")
		if b then
			jevalx(p.b)
		else
			jaddstr("-")
		fi

	when jptr then
		jevalx(a)
		jadditem("^")

	when jblock then
		jadditem("<JBLOCK>")

	when jnull then
		jaddstr("<nullunit>")

	when jaddrof then
		jadditem("&")
		jevalx(a)
		if b then
			jaddstr("+")
			gs_strint(jdest,b.value)
		fi

	when jaddroffirst then
		jadditem("&.")
		jevalx(a)

	when jtypestr then
		jadditem("TYPESTR(")
		jevalx(a)
		jadditem(")")

!	when jcvlineno, jcvfilename, jcvmodulename then
	when jcvfilename, jcvmodulename then
		jaddstr("$")
		jaddstr(jtagnames[p.tag]+1)

	when jbitfield then
		jevalx(a)
		jaddstr(".")
		jaddstr(bitfieldnames[p.bitopindex])

	when jfmtitem then
		jevalx(a)
		jaddstr(":")
		jevalx(b)

	when jsyscall then
		jaddstr(sysfnnames[p.fnindex]+3)
		jaddstr("(")
		if a then jevalx(a) fi
		jaddstr(")")
	when jincr then
		jaddstr("incr ")
		jevalx(a)
	when jstrinclude then
		jaddstr("strinclude ")
		jevalx(a)

	else
		CPL jtagnames[p.tag]
		gerror("CAN'T DO JEVAL",p)
	end
end

proc jadditem(ichar s)=
	gs_additem(jdest,s)
end

proc jaddstr(ichar s)=
	gs_str(jdest,s)
end

global func strmode(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global func strmode2(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=
	symbol d,q
	int needcomma,i,target,mbase,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim
	[100]char strdim
	ichar prefix
	typenamerec tn

	if m<0 then
		strcpy(dest,"*")
		tn:=typenames[-m]

!		if tn.defb=nil then			!assume typeof
!			strcat(dest,"typeof(")
!			strcat(dest,tn.defa.name)
!			strcat(dest,")")
!	    else
			if tn.defa then
				strcat(dest,tn.defa.name)
				strcat(dest,".")
			fi
			strcat(dest,tn.def.name)
!		fi
		return
	fi

	if m<tlast and m<>tref then
		strcpy(dest,typename(m))
		return
	fi

	case mbase:=ttbasetype[m]
	when tref then
		strcpy(dest,"ref ")
		target:=tttarget[m]
		if target>=0 and ttbasetype[target]=trecord then
			strcat(dest,typename(target))
		else
			istrmode(tttarget[m],0,dest+strlen(dest))
		fi

	when tarray then
		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
!			fprint @dest,"@[#]",&.strdim
			fprint @dest,"@[#<#>",&.strdim,M
		else
			if ttlength[m] then
				if ttlower[m]=1 then
					fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
				else
					fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
				fi
			else
				if ttlower[m]=1 then
					fprint @dest,"[]"
				else
					fprint @dest,"[#:]",ttlower[m]
				fi
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tslice then
		prefix:=stdnames[mbase]

		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
			fprint @dest,"@#[#:]",prefix,&.strdim
		else
			if ttlower[m]=1 then
				strcpy(dest,prefix)
				strcat(dest,"[]")
			else
				fprint @dest,"#[#:]",prefix,ttlower[m]
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when trecord then
		if not expand then
			strcpy(dest,typename(m))
			return
		fi
		strcpy(dest,"")
		if expand<>2 then
			strcat(dest,typename(ttbasetype[m]))
		fi
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist

		while q, q:=q.nextdef do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,"void")

	when tuser then
		strcpy(dest,typename(m))
	when tproc then

		d:=ttnamedef[m]

		strcpy(dest,"proc(")
		q:=d.paramlist
		needcomma:=0
		while q<>nil do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		od
		strcat(dest,")")
		if d.mode<>tvoid then
			istrmode(d.mode,0,dest+strlen(dest))
		fi

	when ttuple then
		strcpy(dest,"Tuple(")
		n:=ttlength[m]
		for i to n do
			istrmode(ttmult[m,i],0,dest+strlen(dest))
			if i<n then strcat(dest,",") fi
		od

		strcat(dest,")")

	when tbitfield then
		strcpy(dest,"bitfield")

	elsif ttbasetype[m]<tlast then
		strcpy(dest,"Alias for:")
		istrmode(tttarget[m],0,dest+strlen(dest))

	else
		println typename(m),STRMODE(TTBASETYPE[M])
		mcerror("NEWSTRMODE")
	esac
end

global proc addtoproclist(symbol d)=
	ref procrec pp


!	pp:=pcm_allocz(procrec.bytes)
	pp:=pcm_allocnfz(procrec.bytes)

	if proclist=nil then
		proclist:=proclistx:=pp
	else
		proclistx.nextproc:=pp
		proclistx:=pp
	fi
!
	pp.def:=d
end

global proc addstatic(symbol d)=
	ref procrec pp
!	pp:=pcm_alloc(procrec.bytes)
	pp:=pcm_allocnfz(procrec.bytes)

	if staticlist=nil then
		staticlist:=staticlistx:=pp
	else
		staticlistx.nextproc:=pp
		staticlistx:=pp
	fi

	pp.def:=d
end

global proc addexpconst(symbol d)=
	ref procrec pp
!	pp:=pcm_alloc(procrec.bytes)
	pp:=pcm_allocnfz(procrec.bytes)

	if constlist=nil then
		constlist:=constlistx:=pp
	else
		constlistx.nextproc:=pp
		constlistx:=pp
	fi
	pp.def:=d
end

global func typename(int m)ichar=
	if m>=0 then
		return ttname[m]
	fi
	return typenames[-m].def.name

end

global func allocunitrec:ref unitrec=
	ref unitrec p

	++nunits
	nunitsmem+:=unitrec.bytes

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.pos:=lx.pos
!CPL =LX.POS, LX.FILENO
		p.moduleno:=currmoduleno
		p.subprogno:=moduletosub[currmoduleno]
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
	p.pos:=lx.pos

	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]

!---------------------
!	p:=pcm_allocnfz(unitrec.bytes)
!
!	p.pos:=lx.pos
!
!	p.moduleno:=currmoduleno
!	p.subprogno:=moduletosub[currmoduleno]
!
!

	return p
end

global func createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

	p.nextdupl:=symptr.nextdupl
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

global func createnewmoduledef(symbol owner,symptr, int id=moduleid)symbol=
	return createdupldef(owner,symptr,id)
end

global func duplunit(unit p,int lineno=0)unit=
	unit q
	if p=nil then return nil fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=duplunit(q.abc[i])
	od

	return q
end

global func isconstunit(unit a)int=
	return a.isconst
end

global proc getownername(symbol d, ichar dest)=
	symbol owner

	owner:=d.owner

	if owner=nil or owner.nameid=programid then return fi
	getownername(owner,dest)
	strcat(dest,owner.name)
	strcat(dest,".")
end

global func getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when trecord then
		return 8
	elsif ttisblock[m] then
		return 8
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	when 0 then
		return 8
	esac
	cpl Strmode(m)
	gerror("GETALIGN SIZE NOT 1248")

	return 0
end

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer
end

global func storemode(symbol owner, int m, int32 &pmode)int =
	ref typenamerec r

	if m>=0 then
		pmode:=m
		return m
	fi

	r:=&typenames[-m]

	if r.pmode=nil then
		r.owner:=owner
		pmode:=m
		r.pmode:=&pmode

	IF R.PMODE=NIL THEN SERROR("PMODE=NIL") FI

		return m
	fi

!Already one instance of this mode; need a new slot
	m:=newtypename(r.defa, r.defb)
	r:=&typenames[-m]

	r.owner:=owner
	pmode:=m
	r.pmode:=&pmode
	return m
end

global func gettypebase(int m)int=
	case ttbasetype[m]
	when ti8,ti16,ti32 then ti64
!	when tu8,tu16,tu32 then tu64
	when tu8,tu16,tu32 then ti64

	when tr32 then tr64

	when tc8 then tc64
	else
		m
	esac
end

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
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

global func getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
	unit a

	if p.tag<>jcall then txerror("multass/need multfn") fi
!	if p.tag not in [jcallfn,jcallproc] then txerror("multass/need multfn") fi
	a:=p.a

	case a.tag
	when jname then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	esac
end

global func getpclmode(int t)int u=
	u:=stdpcl[ttbasetype[t]]

!CPL "GETPCLMODE", STRMODE(T),STRMODE(U), =TTISBLOCK[T]

!	if u=tblock and ttisblock[t] then
	if u=tpblock then
		case ttsize[t]
		when 8 then u:=tpu64
		when 4 then u:=tpu32
		when 2 then u:=tpu16
		when 1 then u:=tpu8
		esac
	fi
!CPL =STRMODE(U)
	return u
end

!global func getstringindex(ichar s)int=
!	if s=nil then			!assume nil
!		kk0used:=++mlabelno
!		return kk0used
!	fi
!
!	if cstringlist and eqstring(cstringlist.svalue,s) then
!		return cstringlist.labelno
!	fi
!
!	return addconst(cstringlist, cast(s))
!end
!
!global func addconst(ref constrec &clist, int value)int=
!	ref constrec p
!	p:=pcm_allocnfz(constrec.bytes)
!	p.value:=value
!!CPL "ADDCONST",MLABELNO+1
!	p.labelno:=++mlabelno
!	p.nextconst:=clist
!	clist:=p
!	return mlabelno
!end
!
!global func getrealindex(real x)int=
!	return addconst(creallist,cast@(x,int))
!end
!
!global func getreal32index(real x)int=
!	return addconst(creal32list,cast@(x,int))
!end

=== mm_libpcl.m 0 0 32/41 ===
global function getpsymbol(symbol d)psymbol p=
	symbol e

	return nil when d=nil

	if d.pdef then return d.pdef fi

	if d.atvar and d.equivvar then
		getpsymbol(e:=getequivdef(d))
		d.pdef:=e.pdef
		return e.pdef
	fi

	d.pdef:=p:=pc_makesymbol((d.truename|d.truename|d.name), name2pid[d.nameid])

	p.symbol:=namesym

	p.mode:=getpclmode(d.mode)

	p.size:=ttsize[d.mode]

	if d.owner and d.owner.owner then
		p.owner:=getpsymbol(d.owner)
	fi

	if d.scope=export_scope then p.exported:=1 fi
	if d.nameid in [dllprocid, dllvarid] then p.imported:=1 fi
	p.used:=d.used
	p.labelno:=d.index
	p.ishandler:=d.ishandler
	p.isthreaded:=d.isthreaded
	p.varparams:=d.varparams

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

func getequivdef(symbol d)symbol=
!assume that d.atvar/d.equivvar are set
	unit p

	p:=d.equivvar
	case p.tag
	when jname then
		p.def
	when jconvert then
		p.a.def			!assume points to name
	else
		gerror("geteqv")
		nil
	esac
end
=== mm_libsources_dummy.m 0 0 33/41 ===
global const fsyslibs = 0

global proc loadbuiltins=
end
=== mm_modules.m 0 0 34/41 ===
ichar fileext="m"

global func loadsp(ichar filename, int mainsub=0)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=100
	const maxsubs=100
	[maxmods]ichar modnames
	[maxmods]symbol aliases
	[maxmods]ichar paths
	[maxsubs]ichar subnames
	int nmods:=0, nsubs:=0, hdrcode
	int firstmod, lastmod, issyslib:=0
	imodule pm
	symbol d, stalias
	ichar path, name, ext, file2
	byte proj:=0, sepheader:=0

	if eqstring(extractbasefile(filename), syslibname) then
		issyslib:=1
	fi

	ext:=extractext(filename)
	if not eqstring(ext, "m") then fileext:=pcm_copyheapstring(ext) fi

	pm:=loadmodule(filename, issyslib)

	if pm=nil then
		


		loaderror("Can't load lead module: ", filename)
	fi
	path:=pm.file.path

	for i to nsubprogs do
		if eqstring(pm.name, subprogs[i].name) then
			loaderror("Subprog already loaded: ", sp.name)
		fi
	od

!reader header info
	startlex(pm.file)
	lex()
	skipsemi()

	if lx.symbol=kprojectsym then
		proj:=1
		lexchecksymbol(eqsym)
		lex()
	fi

	do
		skipsemi()
		case lx.symbol
		when kheadersym then
			hdrcode:=lx.subcode
			lex()
			case hdrcode
			when hdr_module then
				checksymbol(namesym)
				name:=lx.symptr.name

				if not eqstring(name, pm.name) then
					if nmods>=maxmods then loaderror("Too many modules in header") fi
					modnames[++nmods]:=name
					paths[nmods]:=path
					aliases[nmods]:=nil

				fi
				if nextlx.symbol=namesym and eqstring(nextlx.symptr.name,"as") then
					lex()
					lex()
					if lx.symbol=namesym then
						stalias:=lx.symptr
						lex()
					else
						checksymbol(stringconstsym)
						stalias:=addnamestr(lx.svalue)
					fi
					aliases[nmods]:=stalias
				fi

			when hdr_import then
				checksymbol(namesym)
				if nsubs>=maxsubs then loaderror("Too many imports in header") fi
				subnames[++nsubs]:=lx.symptr.name

			when hdr_linkdll then
				checksymbol(namesym)
				addlib(lx.symptr.name)

!			when hdr_file then
!				checksymbol(stringconstsym)
!				if nsuppfiles>=maxsuppfile then
!					loaderror("Too many supps")
!				fi
!				suppfiles[++nsuppfiles]:=lx.svalue

			when hdr_sourcepath then
				checksymbol(stringconstsym)
				path:=pcm_copyheapstring(lx.svalue)

			else
				loaderror("Hdr cmd not ready")
			esac
			lex()

		when semisym then
		else
			exit
		esac
	od

	if proj then
		checkend(kendsym, kprojectsym)
	fi
	skipsemi()
	if lx.symbol=eofsym then
		sepheader:=1
	fi

!process nested imports
	for i to nsubs do
		if eqstring(subnames[i],pm.name) then loaderror("Importing self") fi
		loadsp(getmodulefilename(path, subnames[i]))
	od

!create new subprog entry
	if nsubprogs>=maxsubprog then loaderror("Too many subprogs") fi
	sp:=pcm_allocz(subprogrec.bytes)
	subprogs[++nsubprogs]:=sp
	sp.subprogno:=nsubprogs

	if mainsub then
!		loadsyslib()
		mainsubprogno:=nsubprogs
	fi

	firstmod:=nmodules+1
	lastmod:=firstmod+nmods
	if lastmod>maxmodule then loaderror("Too many modules") fi
	nmodules:=lastmod
	pm.subprogno:=nsubprogs
	pm.islead:=1
	pm.moduleno:=firstmod
	pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
	d.moduleno:=firstmod
	d.subprogno:=nsubprogs

	moduletosub[firstmod]:=nsubprogs

	sp.name:=pm.name
	sp.firstmodule:=firstmod

	sp.mainmodule:=0

	sp.lastmodule:=lastmod
	sp.issyslib:=issyslib

!create new set of modules[] entries and load those other modules
!create stmodule entries for each module
	modules[firstmod]:=pm

	for i to nmods do
		pm:=loadmodule(getmodulefilename(paths[i], modnames[i], issyslib), issyslib)
		stalias:=aliases[i]
		if not pm then
			loaderror("Can't load: ",modnames[i])
		fi
		modules[firstmod+i]:=pm
		pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
		pm.subprogno:=nsubprogs
		
		if stalias then
			pm.stmacro:=getduplnameptr(stprogram, stalias, macroid)
			adddef(stprogram, pm.stmacro)
			pm.stmacro.paramlist:=nil
			pm.stmacro.code:=createname(d)
		fi

		d.moduleno:=pm.moduleno:=firstmod+i
		d.subprogno:=nsubprogs
		moduletosub[d.moduleno]:=nsubprogs

		for j to nmodules when eqstring(modules[i].name, pm.name) do
			serror_s("Dupl mod name:", pm.name)
		od
	od

	return sp
end

global func loadmodule(ichar filespec, int issyslib=0)imodule pm=
	ifile pf

	pf:=loadsourcefile(filespec, issyslib)
	return nil when pf=nil

	pm:=pcm_allocz(modulerec.bytes)

	pm.name:=pf.name
	pm.file:=pf
	pm.fileno:=pf.fileno
	pm.issyslib:=issyslib

	return pm
end


global func loadsourcefile(ichar filespec, int issyslib=0)ifile pf=
	ichar s,filename
	[300]char str

	filename:=extractfile(filespec)

!look for file already loaded, or preloaded due to built-in syslib or .ma file:
	for i to nsourcefiles do
		if eqstring(filename, sources[i].filename) and sources[i].issyslib=issyslib then
			return sources[i]
		fi
	od

	pf:=newsourcefile()

	pf.filespec:=pcm_copyheapstring(filespec)
	pf.path:=pcm_copyheapstring(extractpath(filespec))
	pf.name:=pcm_copyheapstring(extractbasefile(filespec))
	pf.filename:=pcm_copyheapstring(filename)
	pf.issyslib:=issyslib
	pf.fileno:=nsourcefiles

	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		return nil
	fi
	pf.text:=s
	pf.size:=rfsize

	if passlevel=ma_pass then
		pf.dupl:=pcm_copyheapstring(s)
	fi

	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return pf
end

func getmodulefilename(ichar path, name, int issyslib=0)ichar =
	static [300]char str

!need to sort out search path etc

	strcpy(str, path)
	strcat(str, name)
	strcat(str, ".")
!	strcat(str, ".m")
	strcat(str, (issyslib|"m"|fileext))
	return str
end

global proc addlib(ichar libname)=
	for i to nlibfiles do
		if eqstring(libfiles[i],libname) then return fi
	od
	if nlibfiles>=maxlibfile then
		loaderror("Too many libs")
	fi
	libfiles[++nlibfiles]:=libname
end

proc loadsyslib=
	[300]char str

	case msyslevel
	when 0 then
		return
	when 1 then
		syslibname:="msysmin"
	else
!CPL "USING MSYSLIB",PASSNAMES[PASSLEVEL]
!		syslibname:=(passlevel=runpcl_pass|"msyslibc"|"msyslib")
		syslibname:="msyslib"
	esac

	strcpy(str, syslibname)

	if dointlibs then				!bundled sys files
		strcpy(str, syslibname)
	else
		strcpy(str, langhomedir)
		strcat(str, syslibname)
	fi

	strcat(str, ".m")
	loadsp(str)
end

global proc loadproject(ichar file)=
	[300]char str
	ichar file2

	if dointlibs then
		loadbuiltins()
	fi

	loadsyslib()

!try .ma version of .m not present
	if not checkfile(file) then
		file2:=pcm_copyheapstring(changeext(file,"ma"))
		if checkfile(file2) then file:=file2 fi
	fi

	if eqstring(extractext(file),"ma") then
		loadmafile(file)
		strcpy(str, changeext(file,"m"))			!assume lead module has same name as ma file
		file:=&.str
	fi

	loadsp(file, 1)

	addlib("msvcrt")
	addlib("user32")
	addlib("gdi32")
	addlib("kernel32")

!	if fwritema then			!do duplicates
!		for i to nsourcefiles do
!			sources[i].dupl:=pcm_copyheapstring(sources[i].text)
!		od
!	fi
end

func readfileline(ichar s)ichar =
!s points into a string representing an entire file, which should be 0-terminated
!read the line s until the next eol or eof, into a line buffer
!return a pointer to the next line
	[2048]char str
	ichar t:=str
	int n, c

	n:=0
	docase c:=s++^
	when 0 then
		--s
		exit
	when 10 then
		exit
	else
		if n<str.len then
			t++^:=c
		fi
	end docase

	t^:=0

	readln @&.str
	return s
end

func findnextlineheader(ichar s)ichar=
!starting from s, find next sequence of lf followed by ===
!return nil if eof found
!otherwise return pointer to just after the ===
	int c

	docase c:=s++^
	when 0 then
		return nil
	when 10 then
		if s^='=' and (s+1)^='=' and (s+2)^='=' then
			return s+3
		fi
	end docase

	return nil
end

proc loadmafile(ichar filespec, ichar builtinstr=nil)=
!load ma file from disk
!unless filespec is nil, then direct from builtinstr
!return name of lead module
	ichar s,t
	[100]char name
	int sys,support
	ifile pf

!	freadma:=1

	if filespec then
		s:=cast(readfile(filespec))
		if s=nil then							!file not found on disk
			loaderror("Can't find MA file ",filespec)
		fi
	else
		s:=builtinstr
	fi

!need to scan file pickuping the file headers, and populating sourctables

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"ma") then
		loaderror("MA: bad header")
	fi

	--s					!point to previous lf

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in MA file")
			exit
		fi
		s:=readfileline(s)

		readstr(name,'n')
		read sys,support

		if eqstring(name,"end") then
			exit
		fi
		if nsourcefiles>=maxsourcefile then
			loaderror("Too many files in MA")
		fi

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("MA error")
		fi

		pf:=newsourcefile()

		pf.filename:=pf.filespec:=pcm_copyheapstring(name)
		pf.name:=pcm_copyheapstring(extractbasefile(name))
		pf.size:=t-s-3
		pf.text:=s
		pf.path:=pf.filespec:=""
		pf.issyslib:=sys
		pf.issupport:=support
		s:=t
	od
!
	for i to nsourcefiles do
		pf:=sources[i]
		(pf.text+pf.size)^:=0
	od
end


=== mm_name.m 0 0 35/41 ===
symbol currstproc
int allowmodname=0
int noexpand, noassem
int macrolevels

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

global proc rx_typetable=
	symbol d
!	symbol currproc

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			d:=ttnamedef[i]
			if d.baseclass then
				do_baseclass(d)
			fi
		fi
	od


!	ref procrec pp
!	unit pcode
!
!	pp:=proclist
!	while pp, pp:=pp.nextproc do
!		currproc:=pp.def
!
!		if ttisshort[currproc.mode] then
!			rxerror("proc short ret type")
!		 fi
!
!		d:=currproc.deflist
!		while d, d:=d.nextdef do
!			if d.nameid=paramid then
!				if ttisblock[d.mode] and d.parammode<>byref_param then
!!CPL "ISBLOCK",D.NAME, STRMODE(D.MODE),TTSIZE[D.MODE]
!					d.parammode:=byref_param
!					d.mode:=createrefmode(nil, d.mode)
!				fi
!			fi
!		od
!	od

end

global proc rx_unit(symbol owner, unit p)=
	symbol d
	unit a,b
	int n,oldnoexpand,oldnoassem,oldtag,useparams

	a:=p.a
	b:=p.b
	mmpos:=p.pos

!CPL "RXUNIT", JTAGNAMES[P.TAG]
	switch p.tag
	when jname then
!CPL "RXUNIT/NAME",P.DEF.NAME
		resolvename(owner,p)
		if P.TAG=JNAME AND p.def.nameid=macroid and not noexpand then
			++macrolevels
			expandmacro(p,p,nil)
			rx_unit(owner,p)
			--macrolevels
		fi
!		d:=p.def
!!CPL "RESOLVEDNAME", D.NAME, NAMENAMES[D.NAMEID], PARAMMODENAMES[D.PARAMMODE]
!		if d.nameid=paramid and d.parammode=byref_param then
!!CPL "RX:AUTODEREF",D.NAME; PRINTUNIT(P)
!			insertunit(p, jptr)
!!CPL "RX:AUTODEREF2",D.NAME; PRINTUNIT(P)
!		fi

	when jkeyword then
		rx_unit(owner,b)		!do param value only

	when jdot then
		resolvedot(owner,p)

	when jcall then
		oldtag:=p.tag

		if a.tag=jname then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=jname then
			d:=a.def
			case d.nameid
			when typeid then		!change to type conversion
				p.tag:=jconvert
				storemode(owner,d.mode,p.convmode)
				p.a:=b
				if b.nextunit then
					p.a:=createunit1(jmakelist,b)
					n:=0
					while b do
						++n
						b:=b.nextunit
					od
					p.a.length:=n
				fi
			when macroid then
				++macrolevels
				if d.deflist then			!macro uses params
					expandmacro(p,a,b)
					b:=nil
					useparams:=0
				else						!macro has no params
					expandmacro(p,a,nil)
					useparams:=1
				fi

				rx_unit(owner,p)
				--macrolevels

				if useparams and p.tag<>jcall then
					insertunit(p,oldtag)
					p.b:=b					!note b may be nil
				FI

			esac
		fi

	when jandl, jorl then
		rx_unit(owner,a)
		rx_unit(owner,b)
!		if not isbooltag[a.tag] then insertunit(a,jistruel); a.convcode:=kktoboolt fi
!		if not isbooltag[b.tag] then insertunit(b,jistruel); b.convcode:=kktoboolt fi
		if not isbooltag[a.tag] then insertunit(a,jistruel) fi
		if not isbooltag[b.tag] then insertunit(b,jistruel) fi

	when jistruel then
	doistruel:
		rx_unit(owner,a)

		if isbooltag[a.tag] then
			deleteunit(p,a)
		fi
!		goto doabc

	when jnotl then
!CPL "NOTL1"
		rx_unit(owner,a)
!CPL "NOTL2"
		case a.tag
		when jnotl then
			deleteunit(p,a)
			p.tag:=jistruel
			a:=p.a
			goto doistruel

		when jistruel then
			a.tag:=jisfalsel
			deleteunit(p,a)
			a:=p.a
!			goto doistruel
		when jisfalsel then
			a.tag:=jistruel
			deleteunit(p,a)
			a:=p.a
!			goto doistruel
		elsif not isbooltag[a.tag] then
			p.tag:=jisfalsel
			a:=p.a
		esac
!		goto doabc

	when jassemmacro then
		resolvename(owner,a)
		if not noexpand then
			++macrolevels
			oldnoassem:=noassem
			noassem:=1
			expandmacro(p,a,b)
			noassem:=oldnoassem
			rx_unit(owner,p)
			--macrolevels
		fi

	else
doabc:
		for i to jsubs[p.tag] do
			rx_unitlist(owner,p.abc[i])
		od
	end switch
end

global func rx_module(int n)int=
	currmoduleno:=n

	rx_passdef(stprogram,modules[n].stmodule)

	return 1
end

global proc rx_deflist(symbol owner,p)=
	symbol pstart:=p
	while p do
		rx_passdef(owner,p)
		p:=p.nextdef
	od
end

global proc rx_passdef(symbol owner,p)=
	symbol d

	case p.nameid
	when moduleid,dllmoduleid then
		rx_deflist(p,p.deflist)

	when procid then
		rx_deflist(p,p.deflist)
		currstproc:=p
		rx_unit(p,p.code)
		currstproc:=nil

	when dllprocid then
		rx_deflist(p,p.deflist)

	when constid,staticid,frameid,paramid then
		if p.atvar then
			rx_unit(owner,p.equivvar)
		fi
		if p.code then
			rx_unit(owner,p.code)
		fi
	when typeid then
		rx_deflist(p,p.deflist)

	else
	esac
end

proc rx_unitlist(symbol owner, unit p)=
	while p do
		rx_unit(owner,p)
		p:=p.nextunit
	od
end

global func resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol =
!stnewname points to a symrec with generic nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)

!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match

!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int extcount, subprogno
	symbol p,q, powner,extdef,moddef
	[10]symbol ambiglist

	if owner.nameid=procid then
		q:=owner.deflist
		while q, q:=q.nextdef do
			if q.firstdupl=stnewname then		!immediate match
				return q
			fi
		od
	fi

	p:=stnewname.nextdupl
	subprogno:=moduletosub[moduleno]

	extcount:=0
	extdef:=moddef:=nil

	while p, p:=p.nextdupl do						!p is next candidate
!CPL "RTNLOOP",P.NAME, NAMENAMES[P.NAMEID]
		powner:=p.owner								!the owner of that entry

!CPL "OWNER",POWNER.NAME, NAMENAMES[POWNER.NAMEID]
		case powner.nameid
		when moduleid then							!candidate is file-scope item
			if powner.moduleno=moduleno then		!same module
				return p
			elsif p.scope then	!matches an external module
!CPL "SCOPE?",SCOPENAMES[P.SCOPE],"//",=POWNER.SUBPROGNO, =SUBPROGNO
				if powner.subprogno=subprogno or		!within same subprog
					 p.scope=program_scope or
					 p.isimport then 				!visible outside subprog
!CPL "VISIBLE AS IMPORT"
					++extcount			!if an ext match is closest, there can only be one
					extdef:=p
					if extcount<ambiglist.len then
						ambiglist[extcount]:=extdef
					fi
				fi
			fi
!CPL "MATCHED MODULE BUT FAILED SP TEST"

		when typeid then					!only for code inside a record def
			if powner=owner or powner=owner.owner then		!immediate match
				return p					!looks at 2 nested record levels only
			fi

		when programid then					!p is a module
			case p.nameid
			when moduleid, subprogid then	!match a module/subprog name
!CPL "MATCH A MODULE",NAMENAMES[P.NAMEID],MODULES[MODULENO].NAME,=MODULENO, =P.MODULENO
!CPL =MODUletoSUB[MODULENO], MODULETOSUB[P.MODULENO]
!cpl P.NAME
				if subprogno=moduletosub[p.moduleno] then
					moddef:=p
				else
					for i to nsubprogs do
						if eqstring(p.name, subprogs[i].name) then
							p.issubprog:=1				!in case not yet set
							moddef:=p
							exit
						fi
					od
				fi
			when macroid then
				return p

			esac

		esac
	od

	if allowmod and moddef then
		return moddef
	fi

	if extdef then
		if extcount>1 then
			if not eqstring(extdef.owner.name, "mclib") then
				for i:=1 to extcount do
					extdef:=ambiglist[i]
					println i,extdef.owner.name,namenames[extdef.owner.nameid]
				od
				if not eqstring(extdef.owner.name, "mclib") then
					rxerror_s("Ambiguous ext name: #",extdef.name)
				fi
			fi
		fi
		return extdef
	fi
	return nil
end

global proc resolvename(symbol owner, unit p)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	symbol d,e
	int moduleno, mode,islet

	d:=p.def
	moduleno:=p.moduleno

	if d.nameid<>nullid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)

	if not e then
		islet:=0
		mode:=tvoid
		case p.avcode
		when 'I', 'T', 'S' then mode:=ti64; islet:=1
		when 'L','A' then mode:=tany
		esac

		if mode=tvoid then
			[300]CHAR STR
			STRCPY(STR, D.NAME)
			CONVUCSTRING(STR)
			rxerror_s("pcl:Undefined: #",STR,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e.pos:=p.pos
			e.islet:=islet
		fi
	fi

	e.used:=1

	p.def:=e
end

global func finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

!CPL "FINDFUPL"

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl

	while pdupl do
!CPL "DUPL LOOP",PDUPL.NAME,PDUPL.OWNER.NAME
		if pdupl.owner=d then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

global func finddupl_sub(symbol d, pdupl)symbol=
!version of finddupl where d is a subprog
	int subprogno

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl
	subprogno:=d.subprogno

	while pdupl do
		if pdupl.owner.subprogno=subprogno then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

proc resolvedot(symbol owner,unit p)=
	unit lhs,rhs
	symbol d,e,t
	int m,moduleno,subprogno,oldallowmod

	moduleno:=p.moduleno
	subprogno:=p.subprogno
	lhs:=p.a
	rhs:=p.b
	e:=rhs.def				!p.b will be a name type (could perhaps be stored as p.def)

	oldallowmod:=allowmodname
	allowmodname:=lhs.tag=jname
	rx_unit(owner,lhs)
	allowmodname:=oldallowmod
	d:=lhs.def

!!CPL "DOT",NAMENAMES[D.NAMEID]
!!CPL "DOT",D.NAME,NAMENAMES[D.NAMEID], D.ISSUBPROG
!!PRINTUNIT(P)
!!
!!IF D.ISSUBPROG THEN
!!	CPL "SUBPROG/DOT", D.NAME, D.SUBPROGNO
!!FI
!IF LHS.TAG=JNAME THEN
!CPL "-------------",=D.NAME
!	CPL =MODULES[MODULENO].NAME, =MODULENO
!	CPL =SUBPROGS[SUBPROGNO].NAME, =SUBPROGNO
!	CPL =D.MODULENO
!	CPL =D.SUBPROGNO,=NSUBPROGS
!
!	if d.nameid=moduleid and d.subprogno<>subprogno then
!CPL "DETECTED SUBPROG.NAME from another SUBPROG"
!	fi

	case lhs.tag
	when jname then
		case d.nameid
		when moduleid, typeid, procid, typeid then

!			if d.nameid=moduleid and d
			if d.nameid=moduleid and d.subprogno<>subprogno then
!				recase subprogid
				dosubprogid
			fi


!			if d.subprogno then
!				recase subprogid
!			fi
			e:=finddupl(d,e)

!CPL "DOT",D.NAME, =E,=OWNER.NAME,"//",namenames[D.NAMEID]

			if e then
				if d.nameid=moduleid then
					if e.subprogno<>subprogno then
						if e.scope<program_scope AND NOT E.ISIMPORT then
!CPL 
!CPL =E.NAME, =E.ISIMPORT
							rxerror_s("Need export to import '#'",e.name)
						fi
					elsif e.moduleno<>moduleno then
						if not e.scope then
							rxerror_s("Need global to import '#'",e.name)
						fi
					fi
				fi
domodule:
				p.tag:=jname			!convert to dot to name
				p.a:=p.b:=nil
				p.def:=e
				case e.nameid
!				when enumid then
				when constid then
				esac
			else
				rxerror_s("Can't resolve .#",p.b.def.name,p)
			fi

		when frameid, staticid, paramid then		!.x applied to normal var
			m:=d.mode
			case ttbasetype[m]
			when trecord then
			when tref then
				do
					m:=tttarget[m]
					case ttbasetype[m]
					when trecord then
						exit
					when tref then
					else
						rxerror("2:Record expected")
					esac
				od
			else
				rxerror("Record expected")
			esac
			t:=ttnamedef[m]

			e:=finddupl(t,e)
			if e then
				p.b.def:=e
			else
				rxerror_s("Not a field: #",rhs.def.name)
			fi
		when subprogid then
dosubprogid:
!CPL "SUBPROGID?"
			e:=finddupl_sub(d,e)
			if e then
				if e.subprogno<>subprogno then
					if e.scope<program_scope AND NOT E.ISIMPORT then
						rxerror_s("Need export to import '#'",e.name)
					fi
				fi
				goto domodule
			else
				rxerror_s("Can't resolve sub.#",p.b.def.name,p)
			fi

		esac

	else
!Can't fully resolve at this time; leave to next pass
		unless e.nextdupl then
			rxerror_s("Not a field: #",e.name)
		end unless
	esac
end

proc fixmode(ref typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
	ref int32 pmode
	symbol a,d,e,f,owner
	int m,moduleno

	pmode:=p.pmode

	m:=-pmode^					!typename index

	d:=owner:=p.owner
	while d.nameid<>moduleid do d:=d.owner od
	moduleno:=d.moduleno

	a:=p.defa
	d:=p.defb

	if a=nil and d then			!simple type name V
		e:=resolvetopname(owner,d,moduleno,0)

!	elsif d=nil and a then		!typeno
!		rxerror("Fixmode can't do typeof yet")
!	else						!assume a.d type reference
!		e:=resolvetopname(owner,a,moduleno,0)
!		if e then
!			f:=e.deflist
!			e:=nil
!			while f do
!				if f.nameid=typeid and f.firstdupl=d then
!					e:=f
!					exit
!				fi
!				f:=f.nextdef
!			od
!
!		fi

	fi

	if e and e.nameid=typeid then
		pmode^:=e.mode

	else
		rxerror_s("2:Can't resolve tentative type: #",d.name)
	fi
end

global proc fixusertypes=
	ref typenamerec p
	int npasses,notresolved
	symbol d

	npasses:=0
	repeat
		++npasses
		notresolved:=0

		for i to ntypenames do
			p:=&typenames[i]

			if p.pmode^<0 then
				mmpos:=typenamepos[i].pos
				fixmode(p)
				if p.pmode^<0 then
					++notresolved
				fi
			fi
		od

		if npasses>5 then
			println "Type phase errors - check these user types:"

			for i to ntypenames do
				p:=&typenames[i]

				if p.pmode^<0 then
					d:=p.defb
					if d=nil then d:=p.defa fi
					println "	",d.name
				fi
			od

			rxerror("Fixtypes: too many passes (cyclic ref?)")
		fi

	until notresolved=0
end

func addframevar(symbol owner, d, int moduleno, mode)symbol=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
	symbol e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,e.mode)
	adddef(owner,e)
	return e
end

func copylistunit(unit p)unit=
	unit q

	unit plist,plistx
	plist:=plistx:=nil
	while p do
		q:=copyunit(p)
		addlistunit(plist,plistx,q)
		p:=p.nextunit
	od
	return plist
end

func copyunit(unit p)unit=
	unit q
	symbol d

	if p=nil then return nil fi

!need to quickly check if a name unit is a macroparam

	if p.tag=jname then
		d:=p.def
		for i to nmacroparams do
			if macroparamsgen[i]=d then
				return copyunit(macroargs[i])
				exit
			fi
		od
	fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	for i to jsubs[q.tag] do
		q.abc[i]:=copylistunit(q.abc[i])
	od

	return q
end

proc replaceunit(unit p,q)=
!replace p with q, keeping same address of p, and same next pointer
!original contents discarded
	unit pnext
	pnext:=p.nextunit
	p^:=q^
	p.nextunit:=pnext
end

proc expandmacro(unit p, a, b)=
!a is a macro name unit, b is a macro parameter list (rx-processed), which
!can be nil
!p is either the call-unit as this may originally have been, or the same as a:
!M => (NAME)
!M(A,B) => (CALL NAME (A,B))
!Need to replace M or M(A,B) with the duplicated AST from a.code.
!When a name appears in the AST which is a local macroparam name, then that is
!replaced by the corresponding argument from B;
!The new code replaces P (either CALL or NAME)
!Supplied args may be more than macro takes (error) or may be less (error,
!or allow default arg values to be specified)
	symbol d,pm
	unit pnew
	int ignoreargs

	if macrolevels>10 then
		rxerror("Too many macro levels (recursive macro?)")
	fi

	d:=a.def

!CPL "EXPANDMACRO",D.NAME

!First step: get list of macro formal parameters

	pm:=d.paramlist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.firstdupl

		pm:=pm.nextparam
	od

!now get macro args into a list
	nmacroargs:=0

	while b do
		if nmacroargs>=maxmacroparams then
			rxerror("macro arg overflow")
		fi
		macroargs[++nmacroargs]:=b
		b:=b.nextunit
	od

	if nmacroargs<nmacroparams then
		PRINTLN =NMACROARGS, NMACROPARAMS
		rxerror("Too few macro args")
	fi

	ignoreargs:=0
	if nmacroargs>0 and nmacroparams=0 then		!ignore extra params
		ignoreargs:=1
		nmacroargs:=nmacroparams:=0

	elsif nmacroargs>nmacroparams then
		rxerror("Too many macro args")
	fi

	pnew:=copyunit(d.code)

	if not ignoreargs then				!normal expansion
		replaceunit(p,pnew)
	else								!keep call and paramlist; just replace fn name
		p.a:=pnew						!with expansion
	fi
end

proc duplfield(symbol owner,p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

	if p.code then
		serror("DUPLFIELD")
	fi

!Need to copy whatever are relevant attributes

	q.atfield:=p.atfield
	q.flags:=p.flags

	q.uflags:=p.uflags		!for .uflags
	storemode(owner,p.mode,q.mode)
end

proc do_baseclass(symbol p)=
!p is class name, which has a baseclass, do the copying necessary for
!inheriting fields
	symbol d,e,newd,dbase
	int normalexit

	dbase:=ttnamedef[p.baseclass]
	d:=dbase.deflist

	while d do				!for each element of base class
		e:=p.deflist

		normalexit:=1
		while e do			!for each element of new class
			if eqstring(d.name,e.name) then
				normalexit:=0
				exit
			fi
			e:=e.nextdef
		od
		if normalexit then
!duplicate d in this class; keep it simple for now
!(procs will need a more elaborate duplication, and really needs to share code)
			case d.nameid
			when procid,linkid then
				newd:=getduplnameptr(p,d,linkid)
				newd.equivfield:=d
			else
				newd:=getduplnameptr(p,d,d.nameid)
				duplfield(p.owner,d,newd)
			esac
			adddef(p,newd)
		fi
		d:=d.nextdef
	od
end
=== mm_parse.m 0 0 36/41 ===
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

int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
int insiderecord=0
int insidedllimport=0

const maxforloops=10
[maxforloops]symbol forindexvars
int nforloops

global func parsemodule(imodule pm)int=
	symbol owner

	initparser()

	currmoduleno:=pm.moduleno

	stmodule:=pm.stmodule

	currproc:=stmodule

!	stsubprog:=subprogs[stmodule.moduleno].stsubprog

	startlex(pm.file)
	owner:=stmodule

!CPL "PARSING",PM.NAME

	lex()
!
!!=========================================
!int t:=os_clock()
!int ntokens:=0
!
!!	repeat
!!		lex()
!!		++ntokens
!!!PS("TOKEN")
!!	until lx.symbol=eofsym
!
!	repeat
!		lexreadtoken()
!PSNEXT("HELLO")
!		++ntokens
!	until nextlx.symbol=eofsym
!
!
!
!t:=os_clock()-t
!
!CPL "LEX TIME=",t
!CPL =ntokens
!
!STOP
!!=========================================
!
	readmoduledefs(owner)

!CPL "DONE PARSING", PM.MODULECODE

!IF PM.MODULECODE=NIL THEN
!	CPL "**** NULL MODULE CODE"
!	STOP
!FI

	return 1
end

global proc readmoduledefs(symbol owner) =
!first symbol has been read
	int globalflag

	globalflag:=module_scope

	do
!PS("RMLOOP")
		switch lx.symbol
		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode

!CPL "GLOBAL",SCOPENAMES[GLOBALFLAG], STMODULE.SUBPROGNO
!			if globalflag=export_scope and stmodule.subprogno<>1 then
			if globalflag=export_scope and stmodule.subprogno<>nsubprogs then
				globalflag:=program_scope
			fi

			lex()

		when kprocsym,kfunctionsym then	!todo
			readprocdef(owner,globalflag)
			globalflag:=module_scope

		when stdtypesym, krefsym, kicharsym, lsqsym, kslicesym then
dovar:
			readvardef(owner,globalflag,0,staticid, 0)
			globalflag:=module_scope

!		when kmutsym then
!			lex()
!			readvardef(owner,globalflag,0,staticid,kmutsym)
!			globalflag:=module_scope
!
		when kletsym then
			lex()
			readvardef(owner,globalflag,0,staticid,kletsym)
			globalflag:=module_scope

		when kimportmodulesym then
			readimportmodule(owner)

		when ktypesym then
			readtypedef(owner,globalflag)
			globalflag:=module_scope

		when kconstsym then
			readconstdef(owner,globalflag)
			globalflag:=module_scope

		when krecordsym then
			readclassdef(owner,globalflag)
			globalflag:=module_scope

		when ktabledatasym then
			readtabledef(owner,globalflag)
			globalflag:=module_scope

		when semisym then
			lex()

		when eofsym then
			exit

		when kmacrosym then
			readmacrodef(owner,globalflag)
			globalflag:=module_scope

		when kprojectsym then
!CPL "PROJECT INFO IN CODE"
			repeat
!CPL "PROJECT BLOCK"
				lex()
			until lx.symbol in [kendsym,eofsym]
			checkend(kendsym, kprojectsym)

		when kheadersym then
!CPL "HEADER INFO IN CODE"
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
doexec:
		serror("Code outside a function")
		end switch
	od
end

proc initparser=

	unless nullunit then
		nullunit:=createunit0(jnull)
	end unless

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

global func makeblock(unit p)unit=
	if p and p.tag=jblock then return p fi
	return createunit1(jblock,p)
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

func getcurrline:int=
	return lx.pos
end

func checkbegin(int fbrack)int=
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
		checksymbollex(closesym)
!		lex()
	else
		checkend(closesym,kwd,startline:startline)
	fi
end

global proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=
!at terminator symbol such as ), eof or 'end'
!check it matches what is expected
!endsym is symbol expected to match
!'end' can have optional keyword following; if present, it must match endkeyword
!Some loop ends (indicated by endkeyword=kforsym, etc) can be also be terminated with 'od'
!endsym should be lbracksym or kendsym
	[100]char str

	skipsemi()

!exit pointing to current symbol (to 'end', keyword after 'end', or ')')
	if endsym=lx.symbol=rbracksym then
		return
	fi

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
!	elsif lx.symbol<>semisym then
!		error
	fi
end

func readvardef(symbol owner,int scope=0,isstatic=0,varid=staticid, k)unit=
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

		storemode(owner,m,stname.mode)

		if lx.symbol in [assignsym,eqsym] then

!			initcode:=case lx.symbol when eqsym then 1 when assignsym then 2 else 3 esac
			case lx.symbol
			when eqsym then initcode:=1
			when assignsym then initcode:=2
			else initcode:=3
			esac
			stname.used:=1

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

!			if ttbasetype[m]=tarray and tttarget[m]=tu8 and lx.symbol=lbracksym then
!				stname.code:=readbxdata()
!			else
				stname.code:=readunit()
!			fi

			stname.equals:=initcode
			if varid=frameid then
				p:=createunit2(jassign,createname(stname),stname.code)
				p.initlet:=1
				addlistunit(ulist,ulistx,p)
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

func readlbrack:unit=
!positioned at "("
!termsym is rbracksym
!read one of the following:
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
		checksymbollex(colonsym)
!		lex()

	elsif lx.symbol=intconstsym and nextlx.symbol=colonsym then
		plower:=createconstunit(lx.value,lx.subcode)
!		plower.istrueconst:=1
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
docomma:						!entry from implicit newline
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

		checksymbollex(rbracksym)
!		lex()
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
			checksymbollex(rbracksym)
!			lex()
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
		checksymbollex(rbracksym)
!		lex()
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
		checksymbollex(rbracksym)
!		lex()

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

func readcast:unit=
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
								!might be T.min etc)
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

	checksymbollex(lbracksym)
!	lex()
	p:=readunit()
	checksymbollex(rbracksym)
!	lex()

	p:=createunit1(opc,p)
	storemode(currproc,t,p.convmode)
	return p
end

func readopc:unit=
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
	when minsym,maxsym,maths2opsym,
iandsym, iorsym, ixorsym then
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

func readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	unit p
	imodule currmodule:=modules[currmoduleno]

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
!		pc_gen(kloadimm, getlineno(lx.pos)
		p:=createconstunit(getlineno(lx.pos), ti64)
!		p:=createunit0(jcvlineno)
		lex()
		return p

	when jcvstrlineno then
		getstrint(getlineno(lx.pos),&.str)

	when jcvmodulename then
		strcpy(str,stmodule.name)

	when jcvfilename then
		strcpy(str,sources[currmodule.fileno].filespec)

	when jcvfunction then
		strcpy(&.str,currproc.name)

	when jcvdate then
		os_getsystime(&tm)
		fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

	when jcvtime then
		os_getsystime(&tm)
		fprint @&.str,"#:#:#",tm.hour:"z2",tm.minute:"z2",tm.second:"z2"

!	when jcvtargetbits then
!		lex()
!		return createconstunit(targetbits,tint)
!	when jcvtargetsize then
!		lex()
!		return createconstunit(targetsize,tint)
!	when jcvtargetcode then
!		strcpy(&.str,"wx64")

	when jcvversion then
		strcpy(&.str,"Compiler:M6.4")

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

func readcastx:unit=
!explicit cast using syntax:
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
	checksymbollex(lbracksym)
!	lex()
	m:=tvoid
	p:=readunit()
	if lx.symbol<>commasym then
		if opc=jtypepun then serror("@ type missing") fi
		opc:=jautocast
	else
		lex()
		m:=readtypespec(currproc)
	fi
	checksymbollex(rbracksym)
!	lex()

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

global proc checksymbollex(int symbol)=
	checksymbol(symbol)
	lex()
end

global func readtypespec(symbol owner,int typedefx=0)int=
!at initial symbol of a type, or where type is expected
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either:
! moderec	Used when called from readtypedef. This is then filled in with the
!		details of the new mode, and the new version is returned
! nil		Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using int16 etc
	symbol d,e
	int t,kwd,sltype,w
	unit x,pupper,plx
	unit dim,length
	const maxdim=30
	[maxdim]unit dims
	int ndims,i,n,k

	case lx.symbol
	when lsqsym then		!array bounds
arraybounds:
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
					else													!lower:
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
		checksymbollex(rsqsym)
!		lex()
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
	retry:

		lex()
		if lx.symbol=ktosym then lex() fi

		case lx.symbol
		when kprocsym,kfunctionsym then	!func pointer being created
			t:=readrefproc(owner,typedefx)

		when stdtypesym then
			case lx.subcode
			when tc8 then
				t:=trefchar
				if typedefx then tttarget[typedefx]:=tc8 fi
			else
				goto readtarget
			esac

			lex()

		when kvoidsym then
			lex()
			t:=tvoid
			gottarget
		else						!assume normal type
readtarget:
			t:=readtypespec(owner)
gottarget:
			t:=createrefmode(owner,t,typedefx)
		esac

	when kicharsym then
		if lx.subcode=tc8 then
			t:=trefchar
		else
			t:=tref
		fi
		if typedefx then tttarget[typedefx]:=lx.subcode fi
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

func readslicetype(symbol owner, int slicetype, typedefx)int=
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

func readslist(int iscall=0,donulls)unit=
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

func readindex(unit p,int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q,plower,pupper

	lex()

	if not dot then
		case lx.symbol
		when rsqsym then
	fullslice:
			lex()
			plower:=createunit1(junary,duplunit(p))
			plower.pclop:=kklwb
			pupper:=createunit1(junary,duplunit(p))
			pupper.pclop:=kkupb
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
	checksymbollex(rsqsym)
!	lex()
	return p
end

func readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
	unit q,r,p2

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p,1)
		when namesym then
			p:=createunit2(jdot,p,createname(lx.symptr))
			lex()
		when propsym then
			if lx.subcode=kkbounds then
!				q:=createunit1(junary, p); q.pclop:=klwb
!				r:=createunit1(junary, p); r.pclop:=kupb
				q:=createunit1(jprop, duplunit(p))
				r:=createunit1(jprop, duplunit(p))
				if p.tag=jtypeconst then
					q.propcode:=kkminval
					r.propcode:=kkmaxval
				else
					q.propcode:=kklwb
					r.propcode:=kkupb
				fi
!				q:=createunit1(junary, p); q.pclop:=klwb
!				r:=createunit1(junary, p); r.pclop:=kupb

				p2:=createunit2(jmakerange, q, r)
				deleteunit(p,p2)
			else

	doprop:
				p:=createunit1(jprop,p)
				p.pclop:=lx.subcode
			fi
			lex()

		when bitfieldsym then
			p:=createunit1(jbitfield,p)
			p.bfcode:=lx.subcode
			lex()
		when ktypesym then			!.type, convert to .gettype
			case p.tag
			when jtypeconst then			!int.type=>int

			else
SERROR("RDS:TYPEOF")
!				p:=createunit1(jtypeof,p)
			esac
			lex()

		when maxsym then
			lx.subcode:=kkmaxval
			goto doprop

		when minsym then
			lx.subcode:=kkminval
			goto doprop
		when stdtypesym then
			if p.tag=jtypeconst and lx.subcode=trange then
				q:=createunit2(jmakerange,
					createunit1(junary,p),
					createunit1(junary,p))
				q.a.propcode:=kkminval
				q.b.propcode:=kkmaxval
			else
				error
			fi
			lex()
			p:=q



		else
	error:
			serror("Unknown dot suffix")
		esac
	od
	return p
end

func readconstexpr(int needconst=1)unit=
	return readunit()
end

func readconstint:int=
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

proc readprocdef(symbol procowner,int scope)=
!at 'proc' etc symbol; read proc def or declaration
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
	int kwd,startline,closesym, shortfun
	symbol stproc,stname

	kwd:=lx.symbol
	shortfun:=lx.subcode=1
	nforloops:=0

	assemmode:=1
	stproc:=readprocdecl(procowner,scope)
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

!++NALLPROCS

	addtoproclist(stproc)

	if shortfun then
		stproc.code:=readunit()
		checksymbollex(semisym)
!		lex()
	else
		stproc.code:=readsunit()
		checkbeginend(closesym,kwd,startline)
	fi
!CPL "AFTER PROC",NESTING,MAXNEST

	stproc.code:=makeblock(stproc.code)

	popproc()
end

global func readprocdecl(symbol procowner,int scope)symbol=
!at 'proc'  or 'function' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

	int kwd,varparams, nparams, nretvalues, isthreaded
	int subprogno
	[maxtuplesize]int retmodes
	imodule ms
	isubprog ps

	ichar metadata, truename
	symbol pequiv, stproc, owner, paramlist,nameptr

	kwd:=lx.symbol				!remember keyword
	isthreaded:=lx.subcode=2

	pequiv:=nil
	metadata:=""
	truename:=nil
	varparams:=0

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
			paramlist:=readparams(procowner,stproc,varparams,nparams)
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
	SERROR("READPROCDEF @")
		lexchecksymbol(namesym)
		lex()
		stproc.atvar:=1
	fi

	stproc.code:=nil

	stproc.scope:=scope
	stproc.varparams:=varparams

	if procowner=stmodule then
		if stproc.namelen=5 and eqstring(stproc.name,"start") then
			modules[stmodule.moduleno].ststart:=stproc
			stproc.scope:=subprog_scope
			dosigcheck
		elsif stproc.namelen=4 and eqstring(stproc.name,"main") then
			ms:=modules[stmodule.moduleno]
			ps:=subprogs[stmodule.subprogno]

			if ps.mainmodule then serror("More than one main() in SP") fi
			ps.mainmodule:=stmodule.moduleno
			ms.stmain:=stproc

			if ps.subprogno=mainsubprogno then
				stproc.scope:=export_scope
dosigcheck:
				if stproc.paramlist or stproc.mode<>tvoid then
					serror("Wrong 'main/start' sig")
				fi

			fi
		fi
	fi

	popproc()

	return stproc
end

func readparams(symbol procowner,owner,int &varparams,&nparams)symbol=			!READPARAMS
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
	parammode:=byval_param
	types:=0

	if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
		types:=1
	fi

	do										!expect type of name at start of loop
		parammode:=byval_param
		isoptional:=0

		if types or istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
gotmode:

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

					checksymbollex(commasym)
!					lex()
					if lx.symbol=ellipsissym then
						varparams:=nparams
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
		when addrsym then
			parammode:=byref_param
			lex()
			if lx.symbol=colonsym then lex() fi
!		when questionsym then
!			isoptional:=1
!			lex()
		when ellipsissym then
			varparams:=nparams
			lex()
			return stlist
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner,lx.symptr,paramid)
		adddef(owner,stname)
!STNAME.ISLET:=1
		lex()

		if parammode=byref_param then
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

func readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond
	unit q

	case lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif,fixcond(readunit()),createunit1(jblock,p))
	when kunlesssym then
		lex()
		q:=createunit1(jnotl,fixcond(readunit()))
		q.pclop:=knot
		return createunit2(jif, q,createunit1(jblock,p))
	else
		return p
	esac
end

func readif:unit=
!at 'if'
	int pos1, kwd
	unit clist,clistx, plist,plistx, pelse, p

	pos1:=lx.pos
	kwd:=lx.symbol			!in case coming from elsecase etc
	lex()
	skipsemi()

	clist:=clistx:=plist:=plistx:=pelse:=nil

	if lx.symbol=kelsifsym then
		lex()
	fi
	nextif

	repeat
		lex()
nextif:
		addlistunit(clist,clistx, fixcond(readsunit()))

		skipsemi()
		checksymbollex(kthensym)
!		lex()

		addlistunit(plist,plistx, readsunit())
		skipsemi()

	until lx.symbol<>kelsifsym

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym,kwd,0)
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		checkend(kendsym,kwd,0)
	esac

	p:=createunit3(jif,clist, plist,pelse)
	p.pos:=pos1
	return p
end

func readgoto(int gototag=jgoto)unit=
	lex()

	return readcondsuffix(createunit1(gototag,readunit()))
end

func readunless:unit=
	int pos
	unit pcond, pthen, pelse, p,q
	pos:=lx.pos
	lex()
	pcond:=fixcond(readsunit())
	checksymbollex(kthensym)
!	lex()

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		PELSE:=NIL
	fi
	checkend(kendsym,kunlesssym)
	p:=createunit3(jif,q:=createunit1(jnotl,pcond),pthen,pelse)
	q.pclop:=knot
	p.pos:=pos
	return p
end

func readswitchcase:unit=
	int pos1, kwd, opc, pos2,rangeused, nwhen
	unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen

	pos1:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc

!IF KWD IN [KDOSWITCHSYM, KDOCASESYM] THEN ++NDO; ++NLOOPS FI

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
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym,kwd)
	esac

	p:=createunit3(opc,pexpr,pwhenlist,pelse)
	p.pos:=pos1
	return p
end

func readstop:unit=
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

func readreturn:unit=
	unit p,q

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

func readdo:unit=
	unit p
	int pos
!++NLOOPS
!++NDO
	pos:=lx.pos
	lex()
	p:=readsunit()
	checkend(kendsym,kdosym)
	p:=createunit1(jdo,p)
	p.pos:=pos
	return p
end

func readto:unit=
	int pos,id
	unit p, pcount, pbody

!++NLOOPS
	pos:=lx.pos
	lex()

	pcount:=readunit()

	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()
	checkend(kendsym,ktosym,kdosym)
	id:=frameid
	if currproc.nameid<>procid then id:=staticid fi

	p:=createunit3(jto,pcount,pbody,createname(getavname(currproc,id)))
	p.pos:=pos
	return p
end

func readwhile:unit=
	int pos
	unit pcond, pbody, pincr, p
	pos:=lx.pos
	lex()
!++NLOOPS

	pcond:=fixcond(readsunit(1))
	pincr:=nil

	if lx.symbol=commasym then
		lex()
		pincr:=readsunit(1)
	fi

	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()

	checkend(kendsym,kwhilesym,kdosym)

	p:=createunit3(jwhile,pcond,pbody,pincr)
	p.pos:=pos

	return p
end

func readrepeat:unit=
	int pos
	unit pbody, pcond, p
!++NLOOPS


	pos:=lx.pos
	lex()
	pbody:=readsunit()
	checksymbollex(kuntilsym)
!	lex()
	pcond:=fixcond(readunit())
	p:=createunit2(jrepeat,pbody,pcond)
	p.pos:=pos

	return p
end

func readloopcontrol:unit=
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

func readprint:unit=
	int oldinreadprint, opc, isfprint, fshowname
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

!				addlistunit(printlist,printlistx,q:=createstringconstunit(s,expr.length+1))
				addlistunit(printlist,printlistx,q:=createstringconstunit(s,expr.length))
			fi
			addlistunit(printlist,printlistx,p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
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

func readread:unit=
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

	finish:
	inreadprint:=oldinreadprint
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit1(jblock,readlist)
end

func readfor:unit=
!on 'for'; syntax is:
! for [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for var[,var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/od
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/od

!AV codes:
!	I	loop index, always i64; will be 'i' (declared or not declared) or autovar
!	L	forall local variable; will be 'x' (declared or not declared); type is variable

	int pos, opc
	unit pindex, plocal				!for index; for index,local
	unit pfrom, pto, pstep, ptoinit	!for INDEX:=FROM to/downto TO [by STEP]/ INDEX in FROM..TO
	unit plist, passign				!for INDEX in/inrev LIST (also LIST.BOUNDS)
	unit pcond, pbody, pelse
	unit p
!
!++NLOOPS
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

!		if plist.tag=junary and plist.pclop=kbounds then
!			pfrom:=getrangelwbunit(plist.a)
!			pto:=getrangeupbunit(plist.a)
!		elsif plist.tag=jmakerange then
		if plist.tag=jmakerange then
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
	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()
	pelse:=nil

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	fi
	checkend(kendsym,kforsym,kdosym)

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

func readname:unit p=
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

!		when datsym then
!			lexchecksymbol(intconstsym)
!			case lx.value
!			when 1,2,4,8 then
!				stname.align:=lx.value
!			when 0 then
!				stname.align:=255
!			else
!				serror("@@ bad align")
!			esac
!			lex()
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
			checksymbollex(rbracksym)
!			lex()

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
		checksymbollex(rbracksym)
!		lex()
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
			checksymbollex(lbracksym)
!			lex()
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
				if nrows<>1 then serror("enum=x, 1st row only") fi
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
				checksymbollex(commasym)		!check it
!				lex()
			fi
		fi

		for i:=1 to ncols do
			addlistunit(plist[i],plistx[i],readunit())
			if i=ncols then
				checksymbollex(rbracksym)
			else
				checksymbollex(commasym)
			fi
!			lex()
		od

		if lx.symbol<>commasym then exit fi
		lex()					!should be ( for next entry
		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	od

	intabledata:=0

	skipsemi()
	checkbeginend(closesym,ktabledatasym,startline)

!Here, I have:

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
		stvar.istabdata:=1

		storemode(owner,varlisttypes[i],stvar.mode)
		stvar.scope:=scope

		adddef(owner,stvar)
		addstatic(stvar)
	od
end

global proc readclassdef(symbol owner,int scope)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, baseclass, m, startline, closesym, mrec, isrecord, align
	symbol nameptr, sttype

	kwd:=lx.symbol
	isrecord:=kwd=krecordsym

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()
	baseclass:=0
	if lx.symbol=lbracksym then
		lex()
		baseclass:=readtypespec(owner)
		checksymbollex(rbracksym)
!		lex()
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
	int kwd,t,lbcount:=0

	unionstr_clear(&unionstring)
	unionstr_clear(&unionpend)

	docase lx.symbol
	when kconstsym then
		readconstdef(owner,0)
	when kfunctionsym,kprocsym then
		kwd:=lx.symbol

		if owner.isimport then
			readprocdecl(owner,0)
		else
			readprocdef(owner,0)
		fi
	when krecordsym then
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
		if lx.symbol=lbracksym then ++lbcount; lex() fi
	when kendsym,rbracksym then
		if unionstring.ulength then
			if lx.symbol=rbracksym and lbcount then
				lex()
				--lbcount
			else
				checkend(kendsym,(unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
			fi
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

	when kvarsym then

		lex()
		if istypestarter() then
	readmut:
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
	end docase

	if lbcount then serror("LB?") fi

end

proc readimportmodule(symbol owner)=
!at 'importmodule' symbol
	int isnew,startline,closesym
	symbol stname,stname0

	if insidedllimport then serror("nested importdll") fi
!	libtype:=lx.subcode

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
!	d:=stname.nextdupl
!	while d do
!		if d.nameid=dllmoduleid then
!			stname:=d
!			isnew:=0
!			exit
!		fi
!		d:=d.nextdupl
!	od

	for i to nlibfiles do
		if eqstring(libfiles[i], stname.name) then
!			stname:=libtable[i]
			isnew:=0
			exit
		fi
	od

!	if isnew then			!new
!		stname:=getduplnameptr(stmodule,stname,dllmoduleid)
!!		adddef(stmodule,stname)
!
!		addlib(stname.name, libtype)
!
!		stname.dllindex:=nlibfiles
!	fi

	if isnew then			!new
!		stname:=getduplnameptr(stmodule,stname,dllmoduleid)
!		adddef(stmodule,stname)
!CPL =D

		addlib(stname.name)

!		stname.dllindex:=nlibfiles
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
	int pos
	symbol d

	pos:=lx.pos

	do
		skipsemi()
		case lx.symbol
		when kprocsym,kfunctionsym then
doproc:
			d:=readprocdecl(owner,0)
			if ndllproctable>=maxdllproc then
				serror("Too many dll procs")
			fi
			dllproctable[++ndllproctable]:=d

		when ktypesym then
			readtypedef(owner,subprog_scope)

		when kconstsym then
			readconstdef(owner,subprog_scope)

		when krecordsym then
			readclassdef(owner,subprog_scope)

		when kvarsym then
			lex()
			readvardef(owner,subprog_scope,0,dllvarid, kvarsym)

		when stdtypesym,namesym,krefsym,kicharsym,lsqsym,kslicesym then
			readvardef(owner,subprog_scope,0,dllvarid, 0)

		when eofsym then
			exit

		when kendsym then
			exit
		else
			PS("symbol")
			serror("Not allowed in importmodule")
		esac
	od
end

func readequivfield(symbol owner)symbol=
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

func readrefproc(symbol owner,int typedefx)int=
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
				paramlist:=readparams(owner,stproc,varparams,nparams)
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
				paramlist:=readparams(owner,stproc,varparams,nparams)
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

!func makeastring:unit =
!!current symbol is an 'astring', like a regular string constant, but intended
!!to be a byte-array
!!Simplest treatment, if not the most efficient, is to turn that into normal 
!!makelist unit
!	unit ulist,ulistx, p, pconst
!	ref char s
!	int length
!
!	ulist:=ulistx:=nil
!
!	s:=lx.svalue
!	length:=astringlength
!	to astringlength do
!		pconst:=createconstunit(s^,ti64)
!		addlistunit(ulist,ulistx,pconst)
!		++s
!	od
!
!	if lx.subcode='Z' then
!		pconst:=createconstunit(0,ti64)
!		addlistunit(ulist,ulistx,pconst)
!		++length
!	fi
!
!	p:=createunit1(jmakelist,ulist)
!	p.length:=length
!	return p
!end

func readreturntype(symbol owner, []int &retmodes)int=
!read 1..maxtuplesize return types as part of func decl
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

func readset:unit=
!positioned at "["
	int length,nkeyvalues,oldirp
	unit p,ulist,ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset,nil)
!	when colonsym then
!		lexchecksymbol(rsqsym)
!		lex()
!		return createunit1(jmakedict,nil)
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

func istypestarter:int=
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

global func readunit:unit p=
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
		p:=createunit2(jcall, readassignment(), p)
	od

	return p
end

func readassignment(unit pt=nil)unit p=
	int pos,opc
	unit q

	p:=readorterms(pt)

	if (opc:=lx.symbol) = assignsym then
		pos:=lx.pos
		lex()
		q:=readassignment(nil)
		p:=createunit2(jassign,p,q)
		p.pos:=pos
	fi
	return p
end

func readorterms(unit pt=nil)unit p=
	int pos

	p:=readandterms(pt)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
SERROR("OR:=")
!			lex()
!			p:=createunit2(jbinto,p,readassignment())
!			p.pclop:=korto
!			p.pos:=pos
!			exit
		fi

		p:=createunit2(jorl,p,readandterms())
		p.pos:=pos
	od

	return p
end

func readandterms(unit pt=nil)unit p=
	int pos

	p:=readcmpterms(pt)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
SERROR("AND:=")
!			lex()
!			p:=createunit2(jbinto,p,readassignment())
!			p.pclop:=kandto
!			p.pos:=pos
!			exit
		fi

		p:=createunit2(jandl,p,readcmpterms())
		p.pos:=pos
	od

	return p
end

func readcmpterms(unit pt=nil)unit p=
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

	docase lx.symbol
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
	end docase

	if n=1 then
		p.tag:=jcmp
		q:=p.a
		p.pclcond:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	fi

	return p
end

func readinterms(unit pt=nil)unit p=
	int pos,opc
	p:=readrangeterm(pt)

	docase lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

!		p:=createunit2(jbin,p,readrangeterm())
!		p.pclop:=opc
		p:=createunit2(jin,p,readrangeterm())
!		p.inv:=opc=kknotin
		p.inv:=opc
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readrangeterm(unit pt=nil)unit p=
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

func readaddterms(unit pt=nil)unit p=
	int pos,sym, tag, genop

	p:=readmulterms(pt)

	docase sym:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym then
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
	end docase

	return p
end

func readmulterms(unit pt=nil)unit p=
	int pos,sym

	p:=readpowerterms(pt)

	docase sym:=lx.symbol
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
	end docase

	return p
end

func readpowerterms(unit p=nil)unit=
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

func readterm2:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t

	pos:=lx.pos

	p:=readterm()

	docase lx.symbol
	when lbracksym then
		lex()
		oldinrp:=inreadprint
		inreadprint:=0
		q:=readslist(1,1)
		checksymbollex(rbracksym)
!		lex()
		if p.tag=jsyscall then
			p.a:=q
		else
			p:=createunit2(jcall,p,q)
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
		when kincrto then opc:=kloadincr
		when kdecrto then opc:=kloaddecr
		esac
		lex()
		p:=createunit1(jincr,p)
		p.pclop:=opc

!	when anddotsym then
!SERROR("AND DOT")
!		lexchecksymbol(lsqsym)
!		lex()
!		q:=readunit()
!		p:=createunit2(janddotindex,p,q)
!		checksymbollex(rsqsym)
!!		lex()

	when lcurlysym then
		serror("X{...} not ready")
	else
		exit
	end docase

	p.pos:=pos

	return p
end

func readterm:unit=
	unit p,q,r
	word64 a
	int opc,pos,length
	byte strtype
	ichar s
	[32]u64 cstr

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
!		p.istrueconst:=1
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue,lx.slength)
		p.strtype:=lx.subcode			!0/1/2 = str/bindata/strdata
		lex()

	when charconstsym then
		length:=lx.slength-1
!		length:=lx.slength
		if length>8 then serror("Char const too long") fi
		a:=0
		if length then
			memcpy(&a,lx.svalue,length)
		fi
		p:=createconstunit(a,tc64)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym,krefsym,kicharsym then
!CPL "RT CAST"
		p:=readcast()

	when addsym, subsym, minsym, maxsym, abssym, inotsym,
iandsym, iorsym, ixorsym,
		mathsopsym, sqrtsym, sqrsym, maths2opsym,signsym then
		p:=readopc()

	when notlsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jnotl, readterm2())
			p.pclop:=knot
		fi

!	when byteswapsym then
!		lex()
!		p:=createunit1(junary, readterm2())
!		p.pclop:=kbyteswap

	when istruelsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jistruel, readterm2())
!			p.pclop:=ktoboolt
		fi

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(jincr,readterm2())
		p.pclop:=opc

!	when ksreadsym,ksreadlnsym then
!		p:=readsread()
!
	when addrsym,daddrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc,readterm2())
		if p.a.tag=jcall then
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

	when dollarsym then
		if intabledata then
			if lx.subcode=1 then			!need char type
				cstr[1]:=0
				strcpy(cast(&cstr), tabledataname)
				p:=createconstunit(cstr[1], tu64)
			else
				s:=tabledataname
				if nextlx.symbol=addsym then
					lex()
					lex()
					checksymbol(intconstsym)
					s+:=lx.value
				fi
				p:=createstringconstunit(s,-1)
			fi
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(junary,dollarstack[ndollar])
			p.propcode:=kkupb
		fi
		lex()

	when kcastsym then
		p:=readcastx()

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
!		lex()
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			checksymbollex(commasym)
!			lex()
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

	when kswapsym then			!swap using func syntax
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
!		lex()
		q:=readunit()
		checksymbollex(rbracksym)
!		lex()
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
		strtype:=lx.subcode
		lex()
		p:=createunit1(jstrinclude,readterm2())
		p.strtype:=strtype

	when kclearsym then
		lex()
		p:=createunit1(jclear, readterm2())

!	when kcopysym then
!		lex()
!		p:=createunit1(jcopy, readterm2())

	when lcurlysym then
		serror("{...} not ready")

	when kslicesym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
!		lex()
		q:=readunit()
		checksymbollex(rbracksym)
!		lex()
		p:=createunit2(jmakeslice, p, q)

	else
DOELSE:
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
					checksymbollex(commasym)
!					lex()
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

func readrecase:unit=
	lex()
	if lx.symbol=kelsesym then
		lex()
		return createunit0(jrecase)
	else
		return createunit1(jrecase,readunit())
	fi
end

func fixcond(unit p)unit=
	if not isbooltag[p.tag] then
		insertunit(p, jistruel)
!		p.convcode:=kktoboolt
	fi
	return p
end

func readsunit(int inwhile=0)unit=
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
			if lx.symbol in [kletsym,kvarsym] then
				opc:=lx.symbol
				lex()
			else
!			opc:=kmutsym
				opc:=0
			fi
			readvardef(currproc,0,1,staticid,opc)

		when kprocsym,kfunctionsym then
			readprocdef(currproc,0)

		when stdtypesym,krefsym,kicharsym,kslicesym,lsqsym then
!IF LX.SYMBOL=KSLICESYM THEN CPL "/SLICE IN READSU"; PSNEXT("SLICE")FI

			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
				goto doexec
			else
				sym:=0
				goto dovar
			fi

		when kvarsym,kletsym then
			sym:=lx.symbol
			lex()
	dovar:
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

		when krecordsym then
			readclassdef(currproc,0)

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
!			when dcolonsym then
!			when colonsym,dcolonsym then
			when colonsym then
				p:=createunit0(jlabeldef)
				stname:=getduplnameptr(currproc,lx.symptr,labelid)
				adddef(currproc,stname)
				p.def:=stname
				lex()
				lx.symbol:=semisym
				addlistunit(ulist,ulistx,p)
			when namesym then
				sym:=kvarsym
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

!		when questionsym then
!			lx.symbol:=semisym
		when questionsym then
			p:=createunit1(jsourceline, createstringconstunit(lx.svalue, -1))
!			addlistunit(ulist,ulistx,q)		!add one by-one
			LX.SYMBOL:=SEMISYM
!			lex()
			doexec3


		else							!assume a statement
	doexec:
			p:=readunit()
	doexec2:
			if p.tag=jname and lx.symbol=namesym then
				serror("Possibly var/let needed")
			fi
	doexec3:
			addlistunit(ulist,ulistx,p)
			if lx.symbol=kdosym then
				exit
			fi

		end switch
	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,
		kelsecasesym,kelseswitchsym,kendsym,commasym,
		barsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then
		return createunit1(jblock,ulist)
	else
		return ulist
	fi
end

func readbxdata:unit p =
!EXPERIMENTAL CODE TO SPEED UP BYTE ARRAY INITS
!This assumes a sequence of intconsts, but needs to backtrack
!and general a normal makelist if any non-intconsts are seen
!This backtracking is not present.

!at '(', and initialising a byte-array
!this test assumes all values are intconst ones,
!and creates a data-string object
!	int curralloc:=16, n:=0
	int curralloc:=4, n:=0
	ref byte q, r, qnew
!CPL "READBXDATA"

	p:=nil
	q:=r:=pcm_alloc(curralloc)

	do
		lex()
		skipsemi()
		if lx.symbol<>intconstsym then
			exit
		fi

		if n=curralloc then
			curralloc*:=2
CPL "REALLOC",CURRALLOC,N
			qnew:=pcm_alloc(curralloc)
			memcpy(qnew, q, n)
			r:=qnew+(r-q)
			pcm_free(q, n)
			q:=qnew
		fi

		r++^:=lx.value
		++n

		lex()
		if lx.symbol<>commasym then
			exit
		fi
	od
	checksymbol(rbracksym)
	lex()

	p:=createstringconstunit(q, n)
	p.strtype:=1

	p
end

=== mm_support.m 0 0 37/41 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global func newsourcefile:ifile pf=
	pf:=pcm_allocz(filerec.bytes)
	if nsourcefiles>=maxsourcefile then loaderror("Too many sources") fi
	sources[++nsourcefiles]:=pf
	pf.fileno:=nsourcefiles
	pf
end

global proc mcerror(ichar mess)=
	println "MC Error:",mess

	stop 1
end

global proc serror_gen(ichar mess)=
	showdivider('*')
!	println "Syntax Error:"
	println "Syntax Error:",MESS

!STOP 1

	showerrorsource(lx.pos, currproc)

	println mess

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

proc showdivider(char64 ch)=
	to 87 do
		print ch
	od
	println
end

proc showerrorsource(int pos, symbol stproc=nil)=
	int fileno:=getfileno(pos), lineoffset
	ichar errorline,s

	fprintln "    Line:     #",getlineno(pos)
	if stproc and stproc.nameid=procid then
		fprintln "    Function: #()", stproc.name
	fi
	fprintln "    Module:   # (#)", sources[fileno].name,sources[fileno].filespec
	showdivider('-')

	s:=errorline:=getsourceline(pos)
	lineoffset:=getsourcepos(pos)-errorline

	to 6 do print " " od
	while s^ not in [10,0] do
		print s++^
	od
	println
	s:=errorline
	to 6 do print " " od
	to lineoffset do
		if s^=9 then print '\t' else print ' ' fi
		++s
	od
	println "^"
	showdivider('-')
end

global proc stopcompiler(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	fclose(f)
CPL "PRESS key"; OS_GETCH()
	println
	println
	stop 1
end

global proc serror(ichar mess)=

	serror_gen(mess)
end

global proc serror_s(ichar mess,a)=
	[256]char str
	fprint @&.str,mess,a
	serror_gen(&.str)
end

global proc error_gen(int pass,ichar mess,unit p=nil)=
!general error handling for passes name, type and code gen
!pass='N' 'T' or 'G'
	int pos

	if p then
		pos:=p.pos
	else
		pos:=mmpos
	fi

!	if currproc and currproc^.nameid=procid then
!		print "In function",currproc^.name,," "
!	fi

!!	println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
!	println "On line",lineno iand 16777215,"in file",sourcefilespecs[fileno]
!	println

	showdivider('*')
	case pass
	when 'N' then println "RX Name Error: "
	when 'T' then println "TX Type Error: "
	when 'G' then println "GX Code Gen Error: "
	when 'A' then println "AX Code Gen Error: "
	esac

!CPL "ERROR SOURCE:",POS.[0..23],POS.[24..31]
	showerrorsource(pos, currproc)
!CPL "DONE2"

	println mess

	stopcompiler(sources[getfileno(pos)].filespec,getlineno(pos))
end

global proc rxerror(ichar mess,unit p=nil)=
	error_gen('N',mess,p)
end

global proc gerror(ichar mess,unit p=nil)=
	error_gen('G',mess,p)
end

global proc txerror(ichar mess,unit p=nil)=
	error_gen('T',mess,p)
end

global proc txerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @&.str,mess,a
	error_gen('T',&.str,p)
end

global proc txerror_ss(ichar mess,a,b)=
	[256]char str
	fprint @&.str,mess,a,b
	error_gen('T',&.str)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @&.str,mess,a
	error_gen('N',&.str,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
	[256]char str

	fprint @&.str,mess,s
	error_gen('G',&.str,p)
end

global proc lxerror_gen(ichar mess)=

	println "On line",getlineno(lx.pos),"in file",sources[lx.fileno].filespec
!CPL =LX.FILENO, SOURCES[LX.FILENO].NAME

	println
	println "**** Lex Error:",mess,"****"
	println

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

global proc lxerror(ichar mess)=
	lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
!	[512]char str
!	if strchr(mess,'#') then
!		println mess,mess2,mess3
!	else
!		print @str,mess
!	fi

	println "Load Error:",mess,mess2,mess3
	println "Stopping"
	stop 1
end

global proc gs_additem(ref strbuffer dest,ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

	d:=dest^.strptr

	if dest^.length then
		lastchar:=(d+dest^.length-1)^
		nextchar:=s^
		if isalphanum(lastchar) and isalphanum(nextchar) then
			strbuffer_add(dest," ")
		fi
	fi
	strbuffer_add(dest,s)
end

global proc gs_copytostr(ref strbuffer source,ref char s)=
	if source^.length then
		memcpy(s,source^.strptr,source^.length)
		(s+source^.length)^:=0
	else
		s^:=0
	fi
end

global func isalphanum(int c)int=
	if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
		return 1
	fi
	return 0
end

global proc init_tt_tables=
	int i,size,bitsize
	int s,t,u,v

!Initialise type tt-tables from std types first all fields initially zero

	for i:=0 to tlast-1 do

		ttname[i]:=stdnames[i]
		ttbasetype[i]:=i
		bitsize:=stdsize[i]*8

		switch bitsize
		when 0 then
			size:=0
		when 1,2,4 then
			size:=1
		else
			size:=bitsize/8
		end switch

		ttsize[i]:=size

		case i
		when ti8,ti16,ti32,ti64 then
			ttsigned[i]:=1
			ttisinteger[i]:=1
		when tu8, tu16, tu32, tu64, tc8, tc64 then
			ttisinteger[i]:=1
		when tr32, tr64 then
			ttisreal[i]:=1
		when tref, trefchar, trefbit then
			ttisref[i]:=1
		esac

!		if stdcat[i]=intcat and size<8 then
		if ttisinteger[i] and size<8 then
			ttisshort[i]:=1
		fi

		ttlower[i]:=1

		if i in [trecord, trange, tarray, tslice] then
			ttisblock[i]:=1
		fi

	od

	ttbasetype[trefchar]:=tref
	tttarget[trefchar]:=tc8

	ntypes:=tlast-1

	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
end

global func getsupportfile(ichar filename, ext="", path="")ifile =
!filename is a rel/abs/base filespec (rel path, abs path or none)
!syslib=1: look first inside INT list
!syslib=0 or not found in int (or -EXT mode0):
!	fbundled; load from list of bundled files from .ma fle
!	filename has rel path/no path: appl to given path
!	filename has abs path: use as is
!look up filename at that final location only (not multiple places)
!issupport=0 for a support file; helps with bundled files where there may
!be duplicates

	[300]char filespec,filespec2
	ichar file
	int fileno
	ifile pfile

	file:=filename

!CPL "GETSUPPORTFILE",FILENAME

	if fverbose=3 then
		fprintln "Get file:# (ext:#) (path:#)",filename,ext, path
	fi

	if ext^ then
		strcpy(filespec,addext(filename,ext))
		file:=&.filespec
	fi

	for i to nsourcefiles do
		if eqstring(file, sources[i].filename) and not sources[i].issyslib then
!CPL "FOUND PRELOADED /SUPPORT/ FILE:",FILE
			return sources[i]
		fi
	od

!CPL "SUPPORT NOT PRELOADED"

	if not isabspath(file) then
		strcpy(filespec2,path)
		strcat(filespec2,file)
		file:=&.filespec2
	fi

	if fverbose=3 and fileno then
		println "Checkfile:",file
	fi
	if file=nil or not checkfile(file) then
		loaderror("Can't find file: ",filename)
	fi

	pfile:=loadsourcefile(file)
	if fverbose=3 and pfile then
		println "Found:",file
	fi

	pfile.issupport:=1
	return pfile
end

func isabspath(ichar filespec)int=
	ichar path:=extractpath(filespec)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		return 1
	fi
	return 0
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end

global func getfileno(word pos)int fileno=
	fileno:=pos.[24..31]
	if fileno<1 or fileno>nsourcefiles then
RETURN 1
!		abortprogram("No file no")
	fi
	return fileno
end

global func getlineno(word pos)int=
	ichar source := getsourcestart(pos)
	ichar sline:=getsourceline(pos)
	ichar s:=sline
	int lineno:=1

	while s>=source do
		if s^=10 then ++lineno fi
		--s
	od

	return lineno
end

func getsourceline(word pos)ichar=
	ichar source := getsourcestart(pos)
	ichar s :=  getsourcepos(pos)

	while s>source and s^<>10 do --s od
	if s^=10 then ++s fi

	return s
end

func getsourcestart(word pos)ichar=
	return sources[getfileno(pos)].text
end

func getsourcepos(word pos)ichar=
	return sources[getfileno(pos)].text+pos.[0..23]
end

global func mgetsourceinfo(int pos, ichar &filename, &sourceline)int=
	int lineno

	lineno:=getlineno(pos)
	sourceline:=getsourcestart(pos)
!	filename:=sources[getfileno(pos)].filename
	filename:=sources[getfileno(pos)].filespec

	lineno
end


global proc do_writema(ichar inpfile)=
	[300]char filename
	[maxsourcefile]int sflist
	filehandle f
	int offset, nfiles, fileno

	return unless passlevel=ma_pass

CPL "WRITEMA"

!	strcpy(filename, changeext(subprogs[nsubprograms].fitrs.filespec,langextma))
	strcpy(filename, changeext(inpfile, "ma"))

!first build a table of source files to be o/p
	nfiles:=0

	for i to nsourcefiles when not sources[i].issyslib do
		sflist[++nfiles]:=i
	od

	if nfiles=0 then loaderror("MA: no files") fi

	f:=fopen(filename,"wb")
	if not f then loaderror("Can't create MA file ",filename) fi

	if fverbose then
		println "Writing ",filename
	fi
	fprintln @f,"=== MA # ===",nfiles

	for i to nfiles do
		fileno:=sflist[i]

		fprintln @f,"=== # # # #/# ===",
			sources[fileno].filename,
			sources[fileno].issyslib,
			sources[fileno].issupport,
			i,nfiles

		offset:=getfilepos(f)
!CPL "WRITE",=FILENO,SOURCES[FILENO].NAME, REF VOID(SOURCES[FILENO].DUPL)
!CPL "WRITE",=FILENO,SOURCES[FILENO].NAME, REF VOID(SOURCES[FILENO].TEXT)
		writerandom(f,cast(sources[fileno].dupl),offset,sources[fileno].size)
	od

	println @f,"=== END ==="

	for i to nfiles do
		fprintln @f,"# #",i,sources[sflist[i]].filename
!		println sources[sflist[i]].filename
	od
!	for i to nsuppfiles do
!		println suppfiles[i]
!	od

	fclose(f)
	stop
end

global proc do_getinfo(ichar filename)=
	filehandle f
	ichar fs
	imodule pm

!	return unless passlevel=getst_pass

	if passlevel=getst_pass then
		f:=fopen(fs:=changeext(filename,"list"),"wb")
		if f then
			println "Writing",fs
			getst(f,stprogram)
			fclose(f)
		fi
	fi

	if passlevel=getproj_pass then
		f:=fopen(fs:=changeext(filename,"proj"),"wb")
		if f then
			println "Writing",fs
			for i to nmodules do
				pm:=modules[i]
				println @f,pm.name:"16jl", subprogs[pm.subprogno].name:"16jl",
					pm.file.filespec:"q",
					pm.issyslib
			od

!			getprofinfi(f,stprogram)
			fclose(f)
		fi
	fi
end

proc getst(filehandle f, symbol d)=
	symbol q

	getstrec(f,d)

	q:=d.deflist

	while q, q:=q.nextdef do
		getst(f,q)
	od
end

proc getstrec(filehandle f, symbol d)=
	ichar name

	case d.nameid
	when procid, dllprocid, typeid, constid, staticid,
		 macroid, dllvarid then
	else
		return
	esac

	if d.owner and d.owner.nameid<>moduleid then
		return									!only module-level names
	fi

	print @f, subprogs[moduletosub[d.moduleno]].name:"10jl",$

	print @f,d.owner.name:"12jl",$
	print @f,d.name:"18jl",$

	case d.nameid
	when procid then
		name:=(d.mode|"funcid"|"procid")
	when dllprocid then
		name:=(d.mode|"dllfuncid"|"dllprocid")
	else
		name:=namenames[d.nameid]
	esac

	print @f,name:"10jl"

	print @f,getlineno(d.pos):"5",$

	case d.scope
	when module_scope then name:="Module"
	when subprog_scope then name:="Subprog"
	when program_scope then name:="Program"
	else name:="Export"				!assume export scope
	esac

	print @f, name,$

	if d.isimport then
		print @f,"Import "
	fi

	print @f,strmode(d.mode):"10jlq",$
	print @f,sources[modules[d.moduleno].fileno].filespec:"q"
	println @f


!	if d.nameid=procid then

!* Module name
!* Entity name (if a module, then repeated)
!* Entity type: PROC/FUNC, DLLPROC, MODULE, TYPE, CONST, STATIC, MACRO
!* Line number of declaration/definition
!* Possibly, any type
!* Possibly, scope
!
!
!
!
!* Entity type: PROC/FUNC, DLLPROC, MODULE, TYPE, CONST, STATIC, MACRO
!	IF D.OWNER.NAMEID<>MODULEID THEN RETURN FI
!

!PRINTLN @F,"DUMP:",D.NAME,D.OWNER.NAME
!PRINTLN"DUMP:",D.OWNER.NAME:"13JL",D.NAME:"16JL", NAMENAMES[D.NAMEID]

end
=== mm_tables.m 0 0 38/41 ===
include "mm_types.m"

global enumdata []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
	(sf_init,				$,	0,	0),
	(sf_print_startfile,	$,	0,	0),
	(sf_print_startstr,		$,	0,	0),
	(sf_print_startptr,		$,	0,	0),
	(sf_print_startcon,		$,	0,	0),
	(sf_print_setfmt,		$,	0,	0),
	(sf_print_nogap,		$,	0,	0),
	(sf_print_space,		$,	0,	0),
	(sf_print_i64,			$,	0,	0),
	(sf_print_i64_nf,		$,	0,	0),
	(sf_print_u64,			$,	0,	0),
	(sf_print_r64,			$,	0,	0),
	(sf_print_r32,			$,	0,	0),
	(sf_print_str,			$,	0,	0),
	(sf_print_str_nf,		$,	0,	0),
	(sf_print_strsl,		$,	0,	0),
	(sf_print_ptr,			$,	0,	0),
	(sf_print_ptr_nf,		$,	0,	0),
	(sf_print_c8,			$,	0,	0),
	(sf_print_bool,			$,	0,	0),
!	(sf_print_var,			$,	0,	0),
	(sf_print_newline,		$,	0,	0),
	(sf_print_end,			$,	0,	0),
	(sf_read_i64,			$,	0,	0),
	(sf_read_r64,			$,	0,	0),
	(sf_read_str,			$,	0,	0),
	(sf_read_fileline,		$,	0,	0),
	(sf_read_strline,		$,	0,	0),
	(sf_read_conline,		$,	0,	0),

	(sf_getnprocs,			$,	0,	1),		!access functions
	(sf_getprocname,		$,	0,	1),
	(sf_getprocaddr,		$,	0,	1),

	(sf_power_i64,			$,	0,	1),
!	(sf_sign_i64,			$,	0,	1),
!	(sf_sign_r64,			$,	0,	1),
	(sf_unimpl,				$,	0,	1),

end
!
global [sysfnnames.len]symbol sysfnhandlers

!global [sysfnnames.len]int sysfnproclabels

!global int mmpos
!global byte fshowpst


!!---
global enumdata [0:]ichar jtagnames,
				   [0:]byte jsubs, [0:]byte jisexpr, [0:]byte jsolo =
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unary or binary op
!jsolo = 1 means unit is allowed standalone without its value being used

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!	L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!	u means .a/b/c pointing to a single unit
!	u/nil means can be nil

![a=u] means a is a unit/list, or is nil

	(jnone=0,		$,	0,		0,	0), ! For tagname lookups when tag is zero
	(jconst,		$,	0,		3,	0), ! value/etc=value, typeno=type code
	(jnull,			$,	0,		3,	0), ! Place holder unit: means 'param no present' when used where a param is expected
	(jvoidvar,		$,	0,		3,	0), ! create void variant
	(jname,			$,	0,		3,	0), ! def=nameptr
	(jnamelv,		$,	0,		3,	0), ! def=nameptr
	(jblock,		$,	1,		0,	1), ! a=L
	(jdecimal,		$,	0,		3,	0), ! svalue=str, slength
	(jassem,		$,	3,		0,	1), ! svalue=str, slength
	(jassemmacro,	$,	0,		0,	0), !
	(jassemreg,		$,	0,		0,	0), !
	(jassemxreg,	$,	0,		0,	0), !
	(jassemmem,		$,	1,		0,	0), !
	(jstrinclude,	$,	1,		3,	0), !
	(jsourceline,	$,	1,		3,	0), !

!Logical Operators

	(jandl,			$,	2,		2,	0), ! A B	This group are for conditional expressions (no result)
	(jorl,			$,	2,		2,	0), ! A B

	(jnotl,			$,	1,		1,	0), ! a
	(jistruel,		$,	1,		1,	0), ! a
	(jisfalsel,		$,	1,		1,	0), ! a

!Expressions and Operators

	(jmakelist,		$,	2,		3,	0), ! a=L, b=[u], length=N; element list/lower bound expr
	(jmakerange,	$,	2,		3,	0), ! A B
	(jmakeset,		$,	1,		3,	0), ! a=L, length=N
	(jmakedict,		$,	1,		3,	0), !
	(jmakeslice,	$,	2,		3,	0), !
	(jreturnmult,	$,	0,		3,	0), !

	(jkeyword,		$,	1,		3,	0), ! def=st entry
	(jkeyvalue,		$,	2,		3,	0), ! A B
	(jassign,		$,	2,		3,	1), ! A B a := x
	(jassignmm,		$,	2,		3,	1), ! A B (a,b,c) := (x,y,z)
	(jassignms,		$,	2,		3,	1), ! A B (a,b,c) := x
	(jassignmdrem,	$,	2,		3,	1), ! A B (a,b) := x divrem y
!	(jcallfn,		$,	2,		3,	1), ! A B
	(jcall,			$,	2,		3,	1), ! A B

	(jcmp,			$,	2,		2,	0), ! A B
	(jcmpchain,		$,	2,		1,	0), ! A B
	(jbin,			$,	2,		2,	0), ! A B
	(junary,		$,	2,		1,	0), ! A B
	(jprop,			$,	2,		1,	0), ! A B
	(jbinto,		$,	2,		2,	0), ! A B
	(junaryto,		$,	1,		1,	0), ! A B
	(jincr,			$,	1,		3,	0), ! a	++a
	(jin,			$,	2,		2,	0), ! A B

	(jinrev,		$,	2,		2,	0), ! A B
	(jinrange,		$,	2,		2,	0), ! A B
	(jinset,		$,	2,		2,	0), ! A B

	(jstringz,		$,	0,		3,	0), ! A B

	(jindex,		$,	2,		3,	0), ! A B		a[b]
	(jindexlv,		$,	2,		3,	0), ! A B		a[b]
	(jslice,		$,	2,		3,	0), ! A B		a[b.a..b.b]
!	(jnewslice,		$,	2,		3,	0), ! A B		slice(a,b)

	(jdot,			$,	2,		3,	0), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotlv,		$,	2,		3,	0), ! A B opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(jdotindex,		$,	2,		3,	0), ! A B		a[b]
	(jdotslice,		$,	2,		3,	0), ! A B		a[b]
!	(janddotindex,	$,	2,		3,	0), ! A B		a[b]

	(jptr,			$,	1,		3,	0), ! a		a^
	(jptrlv,		$, 	1,		3,	0), ! a		a^
	(jaddrof,		$,	1,		3,	0), ! a		&a
	(jaddroffirst,	$,	1,		3,	0), ! a		&a
	(jdaddrvv,		$,	1,		3,	0), ! a		&&a
!	(jdaddrtv,		$,	1,		3,	0), ! a		&&a (from jdaddrvv)
	(jconvert,		$,	1,		3,	0), ! typeno=T a		T(a)			T
	(jshorten,		$,	1,		3,	0), !
	(jautocast,		$,	1,		3,	0), ! typeno=T a		T(a)			T
	(jtypepun,		$,	1,		3,	0), ! typeno=T a		T@(a)			T
	(jtypeconst,	$,	0,		3,	0), ! typeno=T			typeconst(T)
	(joperator,		$,	0,		3,	0), ! opcode=opc
	(jupper,		$,	1,		3,	0), ! a		$					T

	(jbitwidth,		$,	1,		1,	0), ! a
	(jbytesize,		$,	1,		1,	0), ! a
	(jtypestr,		$,	0,		1,	0), ! a
!	(jsliceptr,		$,	0,		1,	0), ! a
	(jbitfield,		$,	1,		3,	0), ! a

	(jminvalue,		$,	1,		3,	0), ! a
	(jmaxvalue,		$,	1,		3,	0), ! a

!Translator Variables

	(jcvlineno,		$,	0,		3,	0), !
	(jcvstrlineno,	$,	0,		3,	0), ! 
	(jcvmodulename,	$,	0,		3,	0), ! 
	(jcvfilename,	$,	0,		3,	0), ! 
	(jcvfunction,	$,	0,		3,	0), ! 
	(jcvdate,		$,	0,		3,	0), ! 
	(jcvtime,		$,	0,		3,	0), ! 
	(jcvversion,	$,	0,		3,	0), ! 
	(jcvtypename,	$,	0,		3,	0), ! 
!	(jcvtargetbits,	$,	0,		3,	0), ! 
!	(jcvtargetsize,	$,	0,		3,	0), ! 
!	(jcvtargetcode,	$,	0,		3,	0), ! 
	(jcvnil,		$,	0,		3,	0), ! 
	(jcvpi,			$,	0,		3,	0), ! 
	(jcvinfinity,	$,	0,		3,	0), ! 
	(jcvtrue,		$,	0,		3,	0), ! 
	(jcvfalse,		$,	0,		3,	0), ! 

	(jwhenthen,		$,	2,		0,	0), ! a=L b=u
	(jfmtitem,		$,	2,		3,	0), ! A B  x/fmtstr
	(jnogap,		$,	0,		3,	0), ! 
	(jspace,		$,	0,		3,	0), ! 

!Statements

!	(jcallproc,		$,	2,		0,	1), ! a=fn b=L, length
	(jreturn,		$,	1,		0,	0), ! a=x/nil
	(jsyscall,		$,	1,		3,	1), ! a=x or nil

!	(jassign,		$,	0,		3,	0), ! A B
	(jto,			$,	3,		0,	0), ! a=N, b=body, c=tempvar/nil, def=name
	(jif,			$,	3,		3,	1), ! condcode a=then b=else
	(jforup,		$,	3,		0,	0), ! 
	(jfordown,		$,	3,		0,	0), !
	(jforall,		$,	3,		0,	0), !
	(jforallrev,	$,	3,		0,	0), !
	(jwhile,		$,	3,		0,	1), ! a=x b=u
	(jrepeat,		$,	2,		0,	1), ! a=u b=x
	(jgoto,			$,	1,		0,	1), ! a=x
	(jlabeldef,		$,	0,		0,	0), ! def=nameptr
	(jredo,			$,	0,		0,	1), ! [a=x]
	(jnext,			$,	0,		0,	1), ! [a=x]
	(jexit,			$,	0,		0,	1), ! [a=x]
	(jdo,			$,	1,		0,	1), ! [a=u
	(jcase,			$,	3,		3,	1), ! a=x b=L [c=else]		L is series of whenthen pairs
	(jdocase,		$,	3,		0,	1), ! a=x b=L [c=else]
	(jswitch,		$,	3,		3,	1), ! a=x b=L [c=else]
	(jdoswitch,		$,	3,		0,	1), ! a=x b=L [c=else]
	(jdoswitchu,	$,	3,		0,	1), ! a=x b=L [c=else]
	(jswap,			$,	2,		0,	1), ! A B
	(jselect,		$,	3,		3,	1), ! Not implemented
	(jrecase,		$,	1,		0,	0), ! Not implemented
!	(jrecaseelse,	$,	0,		0,	0), ! Not implemented

	(jprint,		$,	2,		0,	1), ! [a=dev] b=L
	(jprintln,		$,	2,		0,	1), ! [a=dev] b=L
	(jfprint,		$,	3,		0,	1), ! [a=dev] b=fmtstr c=L
	(jfprintln,		$,	3,		0,	1), ! [a=dev] b=fmtstr c=L
!	(jsprint,		$,	2,		0,	0), !         b=L 
!	(jsfprint,		$,	2,		0,	0), !         b=L
	(jread,			$,	2,		0,	1), ! [a=dev] b=L
	(jreadln,		$,	2,		0,	1), ! [a=dev] b=L
!	(jsread,		$,	2,		0,	0), ! [a=dev] b=L
!	(jsreadln,		$,	2,		0,	0), ! [a=dev] b=L
	(jstop,			$,	1,		0,	0), ! [a=x]
	(jeval,			$,	1,		3,	1), ! "
!	(jstack,		$,	1,		0,	0), ! "
!	(junstack,		$,	1,		0,	0), ! "
	(jclear,		$,	1,		1,	1), ! "

!	(jdummy,		$,	0,		3,	0)
end

global enumdata []ichar bitfieldnames=
	(bf_msb,		$),
	(bf_lsb,		$),
	(bf_msbit,		$),
	(bf_lsbit,		$),
	(bf_msw,		$),
	(bf_lsw,		$),
	(bf_odd,		$),
	(bf_even,		$),
end

global enumdata [0:]ichar optypenames =
	(no_op=0,		$),
	(bin_op,		$),
	(mon_op,		$),
	(prop_op,		$),
end

!!---
global enumdata []ichar symbolnames,
					[]byte symboloptypes,
					[]byte symbolgenops,
					[]byte symbolgentoops,
					[]byte symbolopprios,
					[]byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
	(dotsym,			".",		0,			0,	0,	0,	0),		! "."
	(anddotsym,			"&.",		0,			0,	0,	0,	1),		! "&."
	(commasym,			",",		0,			0,	0,	0,	0),		! ","
	(semisym,			";",		0,			0,	0,	0,	0),		! ";"
	(colonsym,			":",		0,			0,	0,	0,	0),		! ":"
	(assignsym,			":=",		bin_op,		0,	0,	1,	0),		! :=
	(sendtosym,			"=>",		0,			0,	0,	0,	0),		! =>
	(pipesym,			"->",		0,			0,	0,	0,	0),		! ->
	(lbracksym,			"(",		0,			0,	0,	0,	1),		! (
	(rbracksym,			")",		0,			0,	0,	0,	0),		! )
	(lsqsym,			"[",		0,			0,	0,	0,	1),		! [
	(rsqsym,			"]",		0,			0,	0,	0,	0),		! ]
	(lcurlysym,			"{",		0,			0,	0,	0,	0),		! {
	(rcurlysym,			"}",		0,			0,	0,	0,	0),		! }
	(ptrsym,			"^",		0,			0,	0,	0,	1),		! ^
	(barsym,			"|",		0,			0,	0,	0,	0),		! |
!	(dbarsym,			"||",		0,			0,	0,	0,	0),		! ||
	(atsym,				"@",		0,			0,	0,	0,	0),		! @
!	(datsym,			"@@",		0,			0,	0,	0,	0),		! @@
	(questionsym,		"?",		0,			0,	0,	0,	0),		! ?
	(addrsym,			"&",		0,			0,	0,	0,	1),		! &
	(daddrsym,			"&&",		0,			0,	0,	0,	0),		! &&
!	(curlsym,			"~",		0,			0,	0,	0,	0),		! ~
	(rangesym,			"..",		bin_op,		0,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,			0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,			0,			0,			0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		kadd,		kaddto,		4,	1),
	(subsym,			"-",		bin_op,		ksub,		ksubto,		4,	1),
	(mulsym,			"*",		bin_op,		kmul,		kmulto,		3,	0),
	(divsym,			"/",		bin_op,		kdiv,		kdivto,		3,	0),
	(idivsym,			"%",		bin_op,		kidiv,		kidivto,	3,	0),
	(iremsym,			"rem",		bin_op,		kirem,		kiremto,	3,	0),
	(idivremsym,		"divrem",	bin_op,		kidivrem,	0,			3,	0),
	(iandsym,			"iand",		bin_op,		kbitand,	kbitandto,	4,	0),
	(iorsym,			"ior",		bin_op,		kbitor,		kbitorto,	4,	0),
	(ixorsym,			"ixor",		bin_op,		kbitxor,	kbitxorto,	4,	0),
	(shlsym,			"<<",		bin_op,		kshl,		kshlto,		3,	0),
	(shrsym,			">>",		bin_op,		kshr,		kshrto,		3,	0),
	(minsym,			"min",		bin_op,		kmin,		kminto,		4,	1),
	(maxsym,			"max",		bin_op,		kmax,		kmaxto,		4,	1),
	(andlsym,			"and",		bin_op,		0,			0,			7,	0),
	(orlsym,			"or",		bin_op,		0,			0,			8,	0),
	(xorlsym,			"xor",		bin_op,		0,			0,			8,	0),

	(eqsym,				"=",		bin_op,		0,			0,			6,	1),
	(cmpsym,			"cmp",		bin_op,		0,			0,			6,	1),
	(powersym,			"**",		bin_op,		kpower,		0,			2,	0),
	(insym,				"in",		bin_op,		0,			0,			6,	0),
	(notinsym,			"notin",	bin_op,		0,			0,			6,	0),
	(inrevsym,			"inrev",	0,			0,			0,			0,	0),

!	(negsym,			"$neg",		mon_op,		kneg,		0,			0,	1),
	(notlsym,			"not",		mon_op,		knot,		knotto,		0,	1),
	(istruelsym,		"istrue",	mon_op,		0,			0,	0,	1),
	(inotsym,			"inot",		mon_op,		kbitnot,	kbitnotto,	0,	1),
	(abssym,			"abs",		mon_op,		kabs,		kabsto,		0,	1),
	(signsym,			"sign",		mon_op,		ksign,		0,			0,	1),
	(sqrtsym,			"sqrt",		mon_op,		ksqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		ksqr,		0,			0,	1),
!	(byteswapsym,		"byteswap",	mon_op,		kbyteswap,	0,			0,	1),

	(propsym,			$,			prop_op,		0,			0,			0,	0),
	(mathsopsym,		$,			0,	0,	0,	0,	1),		! sin etc
	(maths2opsym,		$,			0,	0,	0,	0,	1),		! atan2 etc

	(bitfieldsym,		$,			0,	0,	0,	0,	0),		! Special bit selections
	(eolsym,			$,			0,	0,	0,	0,	0),		! End of line
	(eofsym,			$,			0,	0,	0,	0,	0),		! Eof seen
	(rawxnamesym,		$,			0,	0,	0,	0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(incrsym,			$,			0,	0,	0,	0,	1),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,			0,	0,	0,	0,	1),		! 123 32 bits signed
	(realconstsym,		$,			0,	0,	0,	0,	1),		! 123.4 64 bits
	(charconstsym,		$,			0,	0,	0,	0,	1),		! 'A' or 'ABCD'
	(stringconstsym,	$,			0,	0,	0,	0,	1),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		$,			0,	0,	0,	0,	0),		! 
	(namesym,			$,			0,	0,	0,	0,	1),		! identifier symbol
	(kincludesym,		$,			0,	0,	0,	0,	0),		! INCLUDE
	(kstrincludesym,	$,			0,	0,	0,	0,	1),		! SINCLUDE/BINCLUDE
	(regsym,			$,			0,	0,	0,	0,	0),		! x64 registers
	(xregsym,			$,			0,	0,	0,	0,	0),		! XMM registers
	(fregsym,			$,			0,	0,	0,	0,	0),		! ST registers
	(mregsym,			$,			0,	0,	0,	0,	0),		! MMX registers
	(jmpccsym,			$,			0,	0,	0,	0,	0),		! 
	(setccsym,			$,			0,	0,	0,	0,	0),		! 
	(movccsym,			$,			0,	0,	0,	0,	0),		! 
	(segnamesym,		$,			0,	0,	0,	0,	0),		! 
	(asmopcodesym,		$,			0,	0,	0,	0,	0),		! MOV etc

	(stdtypesym,		$,			0,	0,	0,	0,	1),		! INT, CHAR etc
	(kicharsym,			$,			0,	0,	0,	0,	1),		! ICHAR IVOID
	(kifsym,			$,			0,	0,	0,	0,	1),		! 
	(kthensym,			$,			0,	0,	0,	0,	0),		! 
	(kelsifsym,			$,			0,	0,	0,	0,	0),		! 
	(kelsesym,			$,			0,	0,	0,	0,	0),		! 
	(kelsecasesym,		$,			0,	0,	0,	0,	0),		! 
	(kelseswitchsym,	$,			0,	0,	0,	0,	0),		! 
	(kendsym,			$,			0,	0,	0,	0,	0),		! 
	(kunlesssym,		$,			0,	0,	0,	0,	0),		! 
	(kcasesym,			$,			0,	0,	0,	0,	1),		! CASE
	(kdocasesym,		$,			0,	0,	0,	0,	0),		! DOCASE
	(krecasesym,		$,			0,	0,	0,	0,	0),		! RECASE
	(kwhensym,			$,			0,	0,	0,	0,	0),		! 
	(kforsym,			$,			0,	0,	0,	0,	0),		! FOR
	(ktosym,			$,			0,	0,	0,	0,	0),		! TO/DOWNTO
	(kbysym,			$,			0,	0,	0,	0,	0),		! 
	(kdosym,			$,			0,	0,	0,	0,	0),		! 
	(kwhilesym,			$,			0,	0,	0,	0,	0),		! 
	(krepeatsym,		$,			0,	0,	0,	0,	0),		! 
	(kuntilsym,			$,			0,	0,	0,	0,	0),		! 
	(kreturnsym,		$,			0,	0,	0,	0,	0),		! 
	(kstopsym,			$,			0,	0,	0,	0,	0),		! 
	(kloopsym,			$,			0,	0,	0,	0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$,			0,	0,	0,	0,	0),		! GO/GOTO
	(kswitchsym,		$,			0,	0,	0,	0,	0),		! SWITCH
	(kdoswitchsym,		$,			0,	0,	0,	0,	0),		! DOSWITCH
	(kprintsym,			$,			0,	0,	0,	0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(kreadsym,			$,			0,	0,	0,	0,	0),		! READ/READLN
	(kprocsym,			$,			0,	0,	0,	0,	0),		! PROC
	(kfunctionsym,		$,			0,	0,	0,	0,	0),		! FUNCTION
	(klabelsym,			$,			0,	0,	0,	0,	0),		! LABEL
	(krecordsym,		$,			0,	0,	0,	0,	0),		! RECORD
	(kstructsym,		$,			0,	0,	0,	0,	0),		! STRUCT
	(kunionsym,			$,			0,	0,	0,	0,	0),		! UNION
	(kimportmodulesym,	$,			0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
	(kprojectsym,		$,			0,	0,	0,	0,	0),		! PROJECT
	(ktypesym,			$,			0,	0,	0,	0,	0),		! TYPE
	(krefsym,			$,			0,	0,	0,	0,	1),		! REF
	(kvoidsym,			$,			0,	0,	0,	0,	1),		! VOID
	(kvarsym,			$,			0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,			0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,			0,	0,	0,	0,	0),		! SLICE/SLICE2D
	(kmacrosym,			$,			0,	0,	0,	0,	0),		! MACRO
!	(koperatorsym,		$,			0,	0,	0,	0,	0),		! OPERATOR
	(kconstsym,			$,			0,	0,	0,	0,	0),		! 
	(kclearsym,			$,			0,	0,	0,	0,	0),		! CLEAR
	(kheadersym,		$,			0,	0,	0,	0,	0),		! MODULE
	(kglobalsym,		$,			0,	0,	0,	0,	0),		! global
	(kstaticsym,		$,			0,	0,	0,	0,	0),		! STATIC

	(kcastsym,			$,			0,	0,	0,	0,	1),		! CAST
	(compilervarsym,	$,			0,	0,	0,	0,	1),		! $lineno etc
	(dollarsym,			$,			0,	0,	0,	0,	1),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,			0,	0,	0,	0,	0),		! EVAL
	(ktabledatasym,		$,			0,	0,	0,	0,	0),		! tabledata
	(kclampsym,			$,			0,	0,	0,	0,	1),			! CLAMP
	(kswapsym,			$,			0,	0,	0,	0,	0),		! SWAP
	(kassemsym,			$,			0,	0,	0,	0,	0),		! ASM/ASSEM
	(ksyscallsym,		$,			0,	0,	0,	0,	1),		! $getprocname etc
end

global enumdata []ichar headerdirnames =
	(hdr_module,		$),
	(hdr_import,		$),
	(hdr_sourcepath,	$),
	(hdr_linkdll,		$),
!	(hdr_file,			$),
!	(hdr_linklib,		$),
!	(hdr_exportmodule,	$),
end

global enumdata [0:]ichar scopenames=
	(Module_scope=0,	"Local"), ! 		!module
	(subprog_scope,		"Global"), ! 		!inter-subprog
	(program_scope,		"Program"), ! 		!inter-module
	(export_scope,		"Export"), ! 		!inter-program
end

global enumdata =
!	thousand_unit,
	million_unit,
	billion_unit,
!	kilo_unit,
!	mega_unit,
!	giga_unit
end

global enumdata [0:]ichar parammodenames=
	(byval_param=0,		"Byval "),
	(byref_param,		"Byref "),
end

global enumdata [0:]ichar namenames, [0:]byte name2pid =
	(nullid=0,		$,		0),				!Not assigned
	(programid,		$,		0),				!Main root
	(subprogid,		$,		0),
	(moduleid,		$,		0),				!Current or imported module
	(dllmoduleid,	$,		0),				!
	(typeid,		$,		0),				!Type name in type, proc or module
	(procid,		$,		proc_id),		!Proc/method/function/op name
	(dllprocid,		$,		import_id),		!Dll Proc/func name
	(dllvarid,		$,		0),				!Dll variable name
	(constid,		$,		0),				!Named constant in type, proc or module
	(staticid,		$,		static_id),		!Static in type or proc or module
	(frameid,		$,		local_id),		!Local var
	(paramid,		$,		param_id),		!Local param
	(fieldid,		$,		0),				!Field of Record or Class
	(labelid,		$,		label_id),		!Label name in proc only
	(macroid,		$,		0),				!Name of macro
	(macroparamid,	$,		0),				!Macro formal parameter name
	(linkid,		$,		0),				!Name in class defined in a base class
end

global enumdata []ichar propnames =
	(kksliceptr,	$),
	(kklen,			$),
	(kklwb,			$),
	(kkupb,			$),
	(kkbounds,		$),
	(kkbitwidth,	$),
	(kkbytesize,	$),
	(kktypestr,		$),
	(kkminval,		$),
	(kkmaxval,		$),
end

!!---
global tabledata []ichar stnames, []byte stsymbols, []i16 stsubcodes=

	("if",			kifsym,			jif),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		jif),
	("else",		kelsesym,		0),
	("elsecase",	kelsecasesym,	jcase),
	("elseswitch",	kelseswitchsym,	jswitch),
	("case",		kcasesym,		jcase),
	("docase",		kdocasesym,		jdocase),
	("recase",		krecasesym,		jrecase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("return",		kreturnsym,		0),
	("stop",		kstopsym,		0),
	("redoloop",	kloopsym,		jredo),
	("nextloop",	kloopsym,		jnext),
	("exit",		kloopsym,		jexit),
	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("doswitchu",	kdoswitchsym,	jdoswitchu),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
	("print",		kprintsym,		jprint),
	("println",		kprintsym,		jprintln),
	("fprint",		kprintsym,		jfprint),
	("fprintln",	kprintsym,		jfprintln),
!	("sprint",		ksprintsym,		jsprint),
!	("sfprint",		ksprintsym,		jsfprint),

	("cp",			kprintsym,		jprint),
	("cpl",			kprintsym,		jprintln),

	("read",		kreadsym,		jread),
	("readln",		kreadsym,		jreadln),
	("cast",		kcastsym,		jconvert),

	("function",	kfunctionsym,	0),
	("func",		kfunctionsym,	0),
	("proc",		kprocsym,		0),
	("fun",			kfunctionsym,	1),
!	("sub",			kprocsym,		1),
	("threadedproc",kprocsym,		2),

	("type",		ktypesym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),
	("let",			kletsym,		0),

	("include",		kincludesym,	0),
	("binclude",	kstrincludesym,	'B'),
	("sinclude",	kstrincludesym,	'S'),
	("strinclude",	kstrincludesym,	'S'),

	("macro",		kmacrosym,		0),

	("assem",		kassemsym,		1),
	("asm",			kassemsym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),

	("$getnprocs",		ksyscallsym,	sf_getnprocs),
	("$getprocname",	ksyscallsym,	sf_getprocname),
	("$getprocaddr",	ksyscallsym,	sf_getprocaddr),

	("importdll",	kimportmodulesym,	0),
	("project",		kprojectsym,		0),
	("unless",		kunlesssym,			0),

	("global",		kglobalsym,		subprog_scope),
	("export",		kglobalsym,		export_scope),

	("swap",		kswapsym,		0),

	("void",		kvoidsym,		0),
	("int",			stdtypesym,		tint),
	("word",		stdtypesym,		tword),
	("real",		stdtypesym,		treal),

	("ichar",		kicharsym,		tc8),
	("ivoid",		kicharsym,		tvoid),

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),
	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("byte",		stdtypesym,		tu8),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),

	("char",		stdtypesym,		tc8),
	("c8",			stdtypesym,		tc8),
	("char8",		stdtypesym,		tc8),
	("c64",			stdtypesym,		tc64),
	("char64",		stdtypesym,		tc64),

	("bool64",		stdtypesym,		tbool64),
	("bool",		stdtypesym,		tbool64),
	("bool8",		stdtypesym,		tbool8),

	("label",		stdtypesym,		tlabel),

	("slice",		kslicesym,		tslice),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),

	("$lineno",		compilervarsym,	jcvlineno),
	("$strlineno",	compilervarsym,	jcvstrlineno),
	("$filename",	compilervarsym,	jcvfilename),
	("$modulename",	compilervarsym,	jcvmodulename),
	("$function",	compilervarsym,	jcvfunction),
	("$date",		compilervarsym,	jcvdate),
	("$time",		compilervarsym,	jcvtime),
	("$version",	compilervarsym,	jcvversion),
	("$typename",	compilervarsym,	jcvtypename),
!	("$targetbits",	compilervarsym,	jcvtargetbits),
!	("$targetsize",	compilervarsym,	jcvtargetsize),
!	("$targetname",	compilervarsym,	jcvtargetname),
!	("$targetcode",	compilervarsym,	jcvtargetcode),
	("nil",			compilervarsym,	jcvnil),
	("pi",			compilervarsym,	jcvpi),
	("true",		compilervarsym,	jcvtrue),
	("false",		compilervarsym,	jcvfalse),
	("infinity",	compilervarsym,	jcvinfinity),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
!	("in",			insym,			kkin),
!	("notin",		notinsym,		kknotin),
	("in",			insym,			0),
	("notin",		notinsym,		1),
	("inrev",		inrevsym,		0),
	("rem",			iremsym,		0),
	("divrem",		idivremsym,		0),
	("min",			minsym,			0),
	("max",			maxsym,			0),

	("not",			notlsym,		0),
	("inot",		inotsym,		0),
	("istrue",		istruelsym,		0),
	("abs",			abssym,			kabs),
!	("$neg",		negsym,			0),
!	("byteswap",	byteswapsym,	0),

	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		ksin),
	("cos",			mathsopsym,		kcos),
	("tan",			mathsopsym,		ktan),
	("asin",		mathsopsym,		kasin),
	("acos",		mathsopsym,		kacos),
	("atan",		mathsopsym,		katan),
	("log",			mathsopsym,		klog),
	("log10",		mathsopsym,		klog10),
	("exp",			mathsopsym,		kexp),
	("round",		mathsopsym,		kround),
	("floor",		mathsopsym,		kfloor),
	("ceil",		mathsopsym,		kceil),

	("atan2",		maths2opsym,	katan2),
	("fmod",		maths2opsym,	kfmod),

	("sliceptr",	propsym,		kksliceptr),
	("len",			propsym,		kklen),
	("lwb",			propsym,		kklwb),
	("upb",			propsym,		kkupb),
	("bounds",		propsym,		kkbounds),
	("bitwidth",	propsym,		kkbitwidth),
	("bytes",		propsym,		kkbytesize),
	("typestr",		propsym,		kktypestr),

	("msb",			bitfieldsym,	bf_msb),
	("lsb",			bitfieldsym,	bf_lsb),
	("msbit",		bitfieldsym,	bf_msbit),
	("lsbit",		bitfieldsym,	bf_lsbit),
	("msw",			bitfieldsym,	bf_msw),
	("lsw",			bitfieldsym,	bf_lsw),
	("odd",			bitfieldsym,	bf_odd),
	("even",		bitfieldsym,	bf_even),

	("fi",			kendsym,		kifsym),
	("esac",		kendsym,		kcasesym),
	("od",			kendsym,		kdosym),

	("$caligned",	atsym,			1),
	("clear",		kclearsym,		0),

	("module",		kheadersym,		hdr_module),
	("import",		kheadersym,		hdr_import),
	("$sourcepath",	kheadersym,		hdr_sourcepath),
	("linkdll",		kheadersym,		hdr_linkdll),
end

global enumdata [0:]ichar convnames, [0:]byte convtopcl =
	(kkerror=0,     $,		0),
!	(kktypepun,     $,		0),
	(kkfloat,       $,		kfloat),
	(kkfix,         $,		kfix),
	(kktruncate,    $,		ktruncate),
	(kkwiden,       $,		kwiden),
	(kkfwiden,      $,		kfwiden),
	(kkfnarrow,     $,		kfnarrow),
	(kksoftconv,    $,		0),
	(kktoboolt,     $,		ktoboolt),
	(kkharderr,     $,		0),
	(kksofttrun,    $,		0),
	(kkichar2sl,    $,		0),
	(kkax2slice,    $,		0),
	(kkcx2ichar,    $,		0),
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
		kicharsym, kslicesym)

global [tr64..tc64, tr64..tc64]int16 softconvtable = (
!To: r64			r32			i64			u64			c64				 From:
	(kksoftconv,	kkfnarrow,	kkfix,		kkfix,		kkfix),			!r64
	(kkfwiden,		kksoftconv,	kkfix,		kkfix,		kkfix),			!r32
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv),	!i64
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv),	!u64
	(kkfloat,		kkfloat,	kksoftconv,	kksoftconv,	kksoftconv))	!c64

global [symbolnames.lwb..symbolnames.upb]byte endsexpr
global []byte exprendsymbols=(rbracksym,rsqsym,kthensym,kelsifsym,
			kelsesym, kuntilsym, kdosym, kendsym, commasym, barsym,
			semisym, ktosym)

global [jtagnames.lwb..jtagnames.upb]byte isbooltag

proc start=
	int genop, s,t, a, specop

	for i to exprendsymbols.len do
		endsexpr[exprendsymbols[i]]:=1
	od

	isbooltag[jcmp]:=1
	isbooltag[jcmpchain]:=1
	isbooltag[jandl]:=1
	isbooltag[jorl]:=1
	isbooltag[jnotl]:=1
	isbooltag[jistruel]:=1
	isbooltag[jisfalsel]:=1
	isbooltag[jinrange]:=1
	isbooltag[jinset]:=1
end

=== mm_type.m 0 0 39/41 ===
const nolv=0
const needlv=1

const maxparams=100
const maxfields=200
int countedfields
int inassem
int inidata

proc tpass(unit p, int t=tany, lv=nolv)=
	symbol d
	unit a,b,c, q
	int oldmmpos,m,nparams,paramtype,restype,amode
	static int depth

	if p=nil then return fi
	if depth=100 then
		txerror("TX looping detected")
	fi
	++depth

	oldmmpos:=mmpos

!CPL "TPASS------------------------", JTAGNAMES[P.TAG]

	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	p.resultflag:=t<>tvoid

	switch p.tag
	when jname then
		tx_name(p,t,lv)
	when jconst, jdecimal then

	when jtypeconst then
		p.mode:=ti64

	when jbytesize, jbitwidth then
		tpass(a)
		p.mode:=ti64

	when jbin, jcmp then
		tx_bin(p,a,b)

	when jin then
		tx_in(p,a,b)

	when junary then
		tx_unary(p,a)

	when jprop then
		tx_prop(p,a)

	when jbinto then
		tx_binto(p,a,b)

	when junaryto then
		tx_unaryto(p,a)

	when jassign then
		tx_assign(p,a,b,t)

	when jaddrof then
		if a.tag=jptr then
			deleteunit(p,a)
			deleteunit(p,p.a)
			tpass(p,t)
		else
			tpasslv(a)
			p.mode:=createrefmode(nil,a.mode)
		fi

	when jaddroffirst then
		tx_addroffirst(p,a,t)

	when jif then
		tx_if(p,a,b,c,t,lv)

	when jindex then
		tx_index(p,a,b,t,lv)

	when jptr then
		tx_ptr(p,a,t,lv)

	when jcall then
		tx_callproc(p,a,b,t)

	when jdot then
		tx_dot(p,a,b,lv)

	when jandl, jorl then
		tx_andl(p,a,b)

	when jnotl, jistruel, jisfalsel then
		tx_notl(p,a)

!	when jistruel then
!		tx_istruel(p,a)

	when jconvert then
		tx_convert(p,a,1)

	when jtypepun then
		tx_typepun(p,a)

	when jincr then
		tx_incrto(p,a,t)

	when jmakerange then
		tx_makerange(p,a,b)

	when jswap then
		tx_swap(p,a,b)

	when jselect then
		tx_select(p,a,b,c,t,lv)

	when jswitch, jdoswitch, jdoswitchu then
		tx_switch(p,a,b,c,t,lv)

	when jcase, jdocase then
		tx_case(p,a,b,c,t,lv)

	when jdotindex, jdotslice then
		tx_dotindex(p,a,b,lv)

	when jslice then
		tx_slice(p,a,b)

	when jblock then
		tx_block(p,a,t,lv)

	when jeval then
		tpass(a,tany)

	when jdo then
		tpass(a,tvoid)

	when jreturn then
		tx_return(p,a,t)

	when jprint,jprintln,jfprint,jfprintln then

		tx_unitlist(a)
		fixchararray(a)

		while b do
			if b.tag=jfmtitem then
				tpass(c:=b.a)
				tpass(b.b,trefchar)
			else
				tpass(c:=b)
			fi
			fixchararray(c)
			b:=b.nextunit
		od
		tx_unitlist(p.c)

	when jforup, jfordown then
		tx_for(a,b,c)

	when jforall, jforallrev then
		tx_forall(a,b,c)

	when jto then
		tpass(a,ti64)
		tpass(b,tvoid)
		tpass(c,ti64)		!when autovar present

	when jautocast then
		tpass(a)
		if t=tany then txerror("cast() needs type") fi
		coerceunit(a,t,1)
		deleteunit(p,a)

	when jmakelist then
		tx_makelist(p,a,t,lv)

	when jstop then
		tpass(a,ti64)

	when jexit,jredo, jnext then
		tx_exit(p,a)

	when jgoto then
		tx_goto(p,a)

	when jlabeldef then

	when jwhile then
		tpass(a,tbool)
		if iscondtrue(a) then
			p.tag:=jdo
			p.a:=b
		elsif iscondfalse(a) then
			p.tag:=jnull
		fi
		tpass(b,tvoid)
		tpass(c,tvoid)

	when jrepeat then
		tpass(a,tvoid)
		tpass(b)
		if iscondtrue(b) or iscondfalse(b) then txerror("repeat/const cond") fi

	when jnogap, jspace then

	when jassem then
		if t<>tvoid then
			p.mode:=t
		fi

		inassem:=1
		tx_unitlist(a)
		tx_unitlist(b)
		tx_unitlist(c)
		inassem:=0

	when jassemreg,jassemxreg then
	when jassemmem then
		tpass(a)

	when jtypestr then
		tpass(a)
		if a.tag=jtypeconst then
			m:=a.value
		else
			tpass(a)
			m:=a.mode
		fi
		p.tag:=jconst
		p.mode:=trefchar
		p.a:=nil
		p.svalue:=pcm_copyheapstring(strmode(m,0))
		p.slength:=strlen(p.svalue)+1
		p.isastring:=1

	when jfmtitem then
		tpass(a)
		tpass(b)

	when jreadln then
		tpass(a)

	when jread then
		if a then
			tpass(a,tc64)
		fi
		if ttisinteger[t] or ttisreal[t] then
			t:=gettypebase(t)
		fi
		p.mode:=t
	when jrecase then
		if a then
			tpass(a,ti64)
			if a.tag<>jconst then
				txerror("recase must be const")
			fi
		fi

!	when jcvlineno then
!		p.mode:=ti64
	when jcvfilename,jcvmodulename then
		p.mode:=trefchar

	when jbitfield then
		tx_bitfield(p,a,lv)

	when jsyscall then
		restype:=tvoid
		paramtype:=tvoid
		case p.fnindex
		when sf_getnprocs then restype:=ti64
		when sf_getprocname then paramtype:=ti64; restype:=trefchar;
		when sf_getprocaddr then paramtype:=ti64; restype:=tref 
		esac

		if paramtype<>tvoid then
			if a=nil then txerror("sys: arg missing") fi
			tpass(a,paramtype)
			if a.nextunit then txerror("sys: too many args") fi
		elsif a then txerror("sys: too many args")
		fi

		p.mode:=restype

	when jcmpchain then
		tx_cmpchain(p,a)

	when jclear then
		tpasslv(a)
		case ttbasetype[a.mode]
		when trecord, tarray then
!CPL "CLEAR BLOCK"
		else
			txerror("Clear scalar?")
		esac


	when jshorten then

	when jstrinclude then
		tx_strinclude(p,a)

	when jmakeslice then
CPL "TPASS/MAKESLICE"
		tx_makeslice(p,a,b,t)

	when jmakeset then
		tx_makeset(p,a,t)

	when jsourceline then

	else
		CPL "TXUNIT: CAN'T DO:",jtagnames[p.tag]
	doelse:

		for i to jsubs[p.tag] do
			tx_unitlist(p.abc[i],t)
		od
	end switch

	tevaluate(p)

	case p.tag
	when jmakelist, jreturn then
	else
		if t<>tany and t<>tvoid and p.mode<>t then		!does not already match
			coerceunit(p,t)			!apply soft conversion
		fi
	esac

	IF T=TVOID THEN
		CASE P.TAG
		WHEN JCONST, JBIN, JUNARY, JCMP THEN
!			TXERROR("Eval needed")
		WHEN JNAME THEN
			unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
!				TXERROR("Eval needed2")
			end

		esac
	fi

	mmpos:=oldmmpos
	--depth
end

global proc tx_allprocs=
	ref procrec pp
	unit pcode

!	pp:=proclist
!	while pp, pp:=pp.nextproc do
!		currproc:=pp.def
!		pcode:=currproc.code
!
!		if ttisshort[currproc.mode] then
!			mmpos:=currproc.pos
!			txerror("proc short ret type")
!		 fi
!
!		symbol d:=currproc.deflist
!		while d, d:=d.nextdef do
!			if d.nameid=paramid then
!				if ttisblock[d.mode] and d.parammode<>byref_param then
!					d.parammode:=byref_param
!					d.mode:=createrefmode(nil, d.mode)
!				fi
!			fi
!		od
!	od
!
	pp:=proclist
	while pp do
		currproc:=pp.def
		pcode:=currproc.code


!CPL "PROC",CURRPROC.NAME
!PRINTUNIT(PCODE)

	    tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

		case ttbasetype[currproc.mode]
		when tvoid then		!PROC
		when ttuple then	!MULT FN
		else				!REGULAR FN
			if pcode.tag<>jreturn then
				insertunit(pcode,jreturn)
				pcode.mode:=currproc.mode
				pcode.resultflag:=1
			fi
		esac

		pp:=pp.nextproc
	od
end

proc tx_block(unit p,a, int t,lv)=
	while a and a.nextunit do
		tpass(a,tvoid)
		a:=a.nextunit
	od
	if a then
!		tx_unitlist(a,t,lv)
		tpass(a,t,lv)
		p.mode:=(t<>tvoid|a.mode|tvoid)
	fi
end

global proc tx_typetable=
	symbol d

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			tx_passdef(d:=ttnamedef[i])
		fi
		setmodesize(i)
	od
end

proc setmodesize(int m)=
	int size,target

	if ttsize[m] then return fi


	mmpos:=ttlineno[m]
	case ttbasetype[m]
	when tarray then
		setarraysize(m)
	when trecord then
		setrecordsize(m)
	when tvoid,tproc then
	when tslice then
		setslicesize(m)
	when tauto then
		TXERROR("SETMODESIZE/AUTO?")
	when tany then

	when tpending then
		target:=tttarget[m]
		setmodesize(target)

		ttbasetype[m]:=ttbasetype[target]
		ttsize[m]:=ttsize[target]
		ttlower[m]:=ttlower[target]
		ttlength[m]:=ttlength[target]
		ttnamedef[m]:=ttnamedef[target]

	when ttuple then

	else
		if size:=ttsize[ttbasetype[m]] then
			ttsize[m]:=size
			return
		fi
		println "SIZE 0:",strmode(m),=m,=stdnames[ttbasetype[m]]
		println "Can't set mode size"
	esac
end

proc setarraysize(int m)=
	int lower,length,elemsize,target,size
	unit pdim,a,b

	if ttsizeset[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		a:=pdim.a
		b:=pdim.b
		rx_unit(ttowner[m],pdim)

		case pdim.tag
		when jmakerange then
			tpass(a)
			tpass(b)
			lower:=getconstint(a)
			length:=getconstint(b)-lower+1
		when jkeyvalue then
			tpass(a)
			lower:=getconstint(a)
			if b then
				tpass(b)
				length:=getconstint(b)
			else
				length:=0
			fi
		else
			tpass(pdim)
			length:=getconstint(pdim)
			lower:=1
		esac
	else
		lower:=1
		length:=0
	fi

	if length<0 then txerror("Neg length") fi
	ttdimexpr[m]:=nil

	ttlower[m]:=lower
	ttlength[m]:=length

	target:=tttarget[m]
	setmodesize(target)
	elemsize:=ttsize[tttarget[m]]
	ttsize[m]:=size:=length*elemsize
	ttsizeset[m]:=1

	checkblocktype(m)
end

proc setslicesize(int m)=
	unit pdim

	if ttsize[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		rx_unit(ttowner[m],pdim)
		tpass(pdim)
		ttlower[m]:=getconstint(pdim)
		ttdimexpr[m]:=nil
	else
		ttlower[m]:=1
	fi

	setmodesize(tttarget[m])
	ttsize[m]:=ttsize[tslice]
end

global func tx_module(int n)int=
	modulerec m
	symbol d
	int globalflag,status

	currmoduleno:=n

	tx_passdef(modules[n].stmodule)

	return 1
end

global proc tx_passdef(symbol p)=
	symbol d
	int oldmmpos
	unit q

	if p.txdone then
		return
	fi

	oldmmpos:=mmpos
	mmpos:=p.pos

	d:=p.deflist
	while d do
		tx_passdef(d)
		d:=d.nextdef
	od

	q:=p.code

	case p.nameid
	when procid then
		currproc:=nil
		currproc:=nil
	when constid then
		tx_namedconst(p)
	when staticid, frameid, paramid then
		tx_namedef(p)
	esac

	p.txdone:=1
!CPL "@@@DONE TX",P.NAME
	mmpos:=oldmmpos
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
	while p do
		tpass(p,t)
		p:=p.nextunit
	od
end

proc tx_namedef(symbol d)=
	int m,mold,inidataold
	unit dcode,pequiv

	if d.circflag then
		txerror("Circular reference detected")
	fi
	if d.txdone then return fi

	m:=d.mode
	setmodesize(m)

	dcode:=d.code

	d.circflag:=1

	if d.atvar then
		pequiv:=d.equivvar
		if pequiv.tag=jaddrof then deleteunit(pequiv,pequiv.a) fi
		if pequiv.tag<>jname then
			txerror("@name needed")
		fi
		tpass(pequiv)
	fi

	if dcode and d.nameid<>frameid then
		mold:=m
		m:=gettypebase(m)

		if ttbasetype[m]=tslice and dcode.tag=jconst and dcode.mode=trefchar then
			tpass(dcode,trefchar)
		else
			inidataold:=inidata
!CPL "NAMEDEF/DCODE",STRMODE(M)
!PRINTUNIT(DCODE)
			inidata:=1
!CPL "INIT DCODE",D.NAME,STRMODE(D.MODE)
			tpass(dcode,m)
!PRINTUNIT(DCODE)
			inidata:=inidataold
		fi
		d.circflag:=0
		d.txdone:=1
		if ttbasetype[m]=tarray and ttlength[m]=0 then
			d.mode:=dcode.mode
		fi

		if mold<>m then
			if ttisinteger[m] and ttisshort[mold] then
				insertunit(d.code,jshorten)
				d.code.mode:=mold
			elsif mold=tr32 then
				d.code.mode:=mold
			fi
		fi

		if d.nameid=staticid then
			checkconstexpr(d.code)
		fi

	elsif dcode and d.nameid=frameid and ttbasetype[m]=tarray and ttlength[m]=0 then
		tpass(dcode,m)
		d.mode:=dcode.mode
		d.circflag:=0
		d.txdone:=1

	else
		d.circflag:=0
		d.txdone:=1
	fi
end

global proc tx_namedconst(symbol d)=
	int m

	if d.circflag then
		txerror("Circular const reference detected")
	fi

	unit q
	if d.txdone then return fi
	q:=d.code

	m:=d.mode

	d.circflag:=1
	tpass(q,(m=tauto|tany|m))

	d.circflag:=0
	checkconstexpr(q)
	if m=tauto then
		d.mode:=q.mode
	fi

!CPL "NAMEDC",STRMODE(D.MODE)
	case ttbasetype[d.mode]
	when tref then
		if d.mode<>trefchar then
			txerror("Bad const type")
		fi
	esac

	d.txdone:=1
end

proc checkconstexpr(unit p)=
!check whether p is const expr
	unit q
	int pmode

	case p.tag
	when jconst, jtypeconst then
		return
	when jmakelist then
		q:=p.a
		while q do
			checkconstexpr(q)
			q:=q.nextunit
		od

	when jconvert then
		if ttbasetype[p.a.mode]=tref then
!CPL "CCE/CONV/REF", STRMODE(TTTARGET[P.A.MODE])
!			if tttarget[p.a.mode]=tvoid then
				p.a.mode:=p.mode
				deleteunit(p,p.a)
!			else
!				goto error
!			fi
		else
			goto error
		fi

	when jshorten then
		checkconstexpr(p.a)

	when jaddrof, jaddroffirst then
		case p.a.tag
		when jname then
		else
			goto error
		esac

	when jname then
		if p.def.nameid=fieldid then return fi
		if p.def.nameid=procid then return fi
		if p.def.nameid=labelid then return fi
		error
	else
	error:
		println =jtagnames[p.tag],STRMODE(P.MODE)
	 PRINTUNIT(P)
		txerror("Getconstexpr: not const")
	esac
end

func getconstint(unit q)int64=
	checkconstexpr(q)

	if ttisinteger[q.mode] or q.tag=jtypeconst then
		return q.value
	elsif ttisreal[q.mode] then
		return q.xvalue
	else
		cpl strmode(q.mode)
		txerror("Getconstint: not int32/64")
	fi
	return 0
end

proc makenewconst(unit p,int64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

	p.tag:=jconst
	p.a:=p.b:=nil
	p.value:=x
	p.isconst:=1
	if t<>tvoid then
		p.mode:=t
	fi
end

proc tx_name(unit p,int t,lv)=
	symbol d
	int oldmmpos
	unit pcode
	oldmmpos:=mmpos

IF P.TXCOUNT THEN
!CPL =P.TXCOUNT,P.DEF.NAME
RETURN
FI
++P.TXCOUNT

	d:=p.def
!CPL "/////////////////NAME", D.NAME,D.TXDONE
	mmpos:=d.pos

	case d.nameid
	when constid then			!note: currently, rxpass converts names to constants

		if lv then txerror("&const") fi

		tx_namedconst(d)
		pcode:=d.code

		p.tag:=jconst
		p.def:=nil
		p.a:=nil
	    p.c:=nil

		if pcode.tag=jconvert then		!assume c_soft
			p.value:=pcode.a.value

		else
			p.value:=pcode.value
		fi

		p.slength:=pcode.slength
		p.mode:=d.mode
		p.isconst:=1
		p.isastring:=pcode.isastring

	when staticid,frameid,paramid then
		if d.islet and lv then
!			println D.NAME,=LV,D.ISLET
			txerror_s("Can't use 'let' as lvalue: ",d.name)
		fi

		tx_namedef(d)

		if not inassem then
			p.mode:=d.mode
			if d.parammode=byref_param then
IF NOT P.INSPTR THEN
!CPL "INSERT PTR",P.INSPTR
++P.INSPTR
				insertunit(p, jptr)
				p.mode:=tttarget[d.mode]
			fi
FI
			twiden(p,lv)
!CPL "OUTPARAM4", =STRMODE(P.MODE)
!PRINTUNIT(P)

		else
			p.mode:=trefchar
		fi

	when procid,dllprocid then

!		if lv then
!CPL =LV

! txerror("&proc?") fi
!
		p.mode:=trefproc	!use generic refproc mode (yields return type of actual proc mode
				!after a call op, or actual refproc in other context. Don't use actual
				!refproc here, to avoid generating thousands of ref proc modes, one
				!for each call, that will never be needed

	when labelid then
		p.mode:=treflabel

	when moduleid then
		txerror_s("Module name can't be used on it's own: #",d.name)

	when fieldid then
		p.tag:=jconst
		p.def:=nil
		p.a:=nil
	    p.c:=nil

		p.value:=d.offset

		p.mode:=ti64
		p.isconst:=1


	when typeid then
		p.tag:=jtypeconst
		p.value:=d.mode
		p.mode:=ti64

	when dllvarid then
		if d.code then
			txerror("Can't init dllvar")
		fi
		p.mode:=d.mode

	else
		mmpos:=p.pos
		txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
	esac
	mmpos:=oldmmpos

end

proc tx_bin(unit p,a,b)=
!deal with most binary ops
	unit q
	int amode,bmode,abase,bbase,cmode, resmode, relop, simpleset

	tpass(a)
	tpass(b)
	amode:=a.mode
	bmode:=b.mode

	case p.pclop
	when kadd then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if ttisref[bmode] and a.isastring and b.isastring then
				combinestrings(p)
				return
			fi
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.pclop:=kaddpx
				p.mode:=amode
				return
			fi
		fi

	when ksub then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if ttisref[bmode] then
				if comparemodes(amode, bmode) then
					p.pclop:=ksubp
					p.mode:=ti64
					return
				else
					txerror("ref-ref: not compat")
				fi
			fi
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.pclop:=ksubpx
				p.mode:=amode
				return
			fi

		fi

!	when keq, kne, klt, kle, kge, kgt then
!		if dobinnumx(p,a,b) then
!			p.mode:=tbool
!			return
!		fi
!		p.mode:=tbool
!		if ttisref[amode] and ttisref[bmode] then
!			if not comparemodes(amode, bmode) then
!				txerror("Cmp ref/ref not compat")
!			fi
!			return
!		fi
!		if p.pclop in [keq, kne] then
!			if comparemodes(amode, bmode) then
!				return
!			fi
!		fi

	when kmul then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if a.isastring and ttisinteger[b.mode] and b.tag=jconst then
				mulstrings(p)
				return
			fi
		fi


	when kdiv then
		if isnumi(amode) and isnumi(bmode) then p.pclop:=kidiv; goto doidiv fi
		if dobinnumf(p,a,b) then return fi
		if isnum(amode) and isnum(bmode) then
			p.mode:=tr64
			coerceunit(a,tr64)
			coerceunit(b,tr64)
			return
		fi

	when kidiv, kirem, kidivrem, kbitand, kbitor, kbitxor then
doidiv:
		if dobinnumi(p,a,b) then return fi

	when kmin, kmax then
		if dobinnumx(p,a,b) then return fi

	when kpower then
		if dobinnumx(p,a,b) then return fi

	when kfmod, katan2 then
		coerceunit(a,tr64)
		coerceunit(b,tr64)
		p.mode:=tr64
		return
!		if dobinnumf(p,a,b) then return fi

	when kshl, kshr then
		if isnumi(amode) then
			coerceunit(b,ti64)
			p.mode:=amode
			return
		fi

!	when kkin, kknotin then
!		tx_in(p,a,b)
!		return

	elsif p.pclcond then
		if dobinnumx(p,a,b) then
			p.mode:=tbool
			return
		fi
		p.mode:=tbool
		if ttisref[amode] and ttisref[bmode] then
			if not comparemodes(amode, bmode) then
				txerror("Cmp ref/ref not compat")
			fi
			return
		fi
		if p.pclcond in [eq_cc, ne_cc] then
			if comparemodes(amode, bmode) then
				return
			fi
		fi

	else
		txerror("txbin?")
	esac

cpl pclnames[p.pclop]
	TXERROR_SS("BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode))
!
!	p.mode:=resmode
end

proc tx_binto(unit p,a,b)=
	int abase, bbase, amode,bmode, opc

	tpasslv(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if p.pclop=kdivto and ttisinteger[abase] then
		p.pclop:=kidivto
	fi

	p.mode:=tvoid

	case p.pclop
	when kaddto then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			txerror("to:ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddpxto
			return
		fi
	when ksubto then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubpxto
			return
		fi
	when kshlto, kshrto, kbitandto, kbitorto, kbitxorto then
		coerceunit(b,ti64)
		return
!ELSE
!CPL "BINTO", PCLNAMES[P.PCLOP]

	esac

	if isnum(abase) and isnum(bbase) then	!num op num
		coerceunit(b,abase)

	elsif ttisshort[abase] and isnum(bbase) then
		coerceunit(b,abase)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode(bmode))
		fi
	fi
end

func getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if isnum(abase) and isnum(bbase) then
!		return max(abase,bbase)

		return min(abase,bbase)
	fi
	if not comparemodes(amode, bmode) then
		txerror("Getdom: no dominant mode")
	fi
	return amode
end

proc tx_cmpchain(unit p,a)=
	int u,genop
	unit q,r

	q:=a
	while q do
		tpass(q,tany)

		if q=a then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	q:=a
	r:=a.nextunit
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	for i:=1 to p.cmpgenop.len do
		genop:=p.cmpgenop[i]
		if genop=0 then exit fi

		p.cmppclmode[i]:=getpclmode(u)
	od

	p.mode:=ti64
!	p.mode:=tbool
end

proc tx_callproc (unit p,a,pargs,int t)=
!deal with both callproc and callfn (perhaps calldll too)
	unit q
	symbol d,e,pm
	[maxparams]symbol paramlist
	[maxparams]unit arglist,newarglist
	int nparams,i,j,k,nargs,m,kwdused,qm, ismproc
	ichar name

	tpass(a)

	nargs:=nparams:=0
	ismproc:=0

	retry:

	case a.tag
	when jname then
		d:=a.def

		if d.nameid in [procid, dllprocid] then
			ismproc:=d.nameid=procid
getparams:
			e:=d.deflist
			while e do
				if e.nameid=paramid then
					if nparams>=maxparams then txerror("Param overflow") fi
					paramlist[++nparams]:=e
				fi
				e:=e.nextdef
			od

		else					!assume fn ptr
			while ttbasetype[a.mode]=tref do
				insertunit(a,jptr)
				a.mode:=tttarget[a.mode]
			od
			goto dorefproc
		fi

	when jif,jselect,jblock then
		TXERROR("Can't do ifx/function")

	else
	dorefproc:
		if a.tag=jdot then
			tmethodcall(p,a,pargs)
			a:=p.a
			pargs:=p.b
			goto retry
		fi

		if ttbasetype[a.mode]<>tproc then
			txerror("Function pointer expected")
		fi

		d:=ttnamedef[a.mode]

		if d=nil then txerror("Function expected") fi
		goto getparams
	esac

	q:=pargs
	while q do
		if nargs>=maxparams then txerror("Param overflow") fi
		arglist[++nargs]:=q
		q:=q.nextunit
	od

	p.mode:=d.mode				!type returned by func (will be void for procs)

!	if p.mode=tvoid and p.tag=jcallfn then
!		p.tag:=jcallproc
!	fi

	if p.mode and t<>tvoid then
		twiden(p,nolv)
	fi

	if d.varparams then
		for i to nargs do

			if i<=nparams then
				tpass(arglist[i],paramlist[i].mode)
			else
				tpass(arglist[i])
			fi
		od
!		if t=tvoid then
!			p.tag:=jcallproc
!		fi
		return

	fi

!I have formal params in paramlist, and actual args in arglist
!Now create new set of args in arglist, which maps any keyword parameters,
!while missing args (represented as nullunit) replaced with nil

!Create new set of actual parameters in params(), with optional/default values filled in
!and any conversions applied
	k:=0
	kwdused:=0
	for i to nparams do
		newarglist[i]:=nil
	od

	for i to nargs do
		q:=arglist[i]
		case q.tag
		when jkeyword then
			name:=q.a.def.name
			for j to nparams do
				if eqstring(paramlist[j].name,name) then
					exit
				fi
			else
				txerror_s("Can't find kwd param: #",name)
			od

			if newarglist[j] then
				txerror_s("Kwd: # already used or was implicit",name)
			fi
			newarglist[j]:=q.b
			kwdused:=1

!		when jnull then			!missing param
!CPL "JNULL"
!			if kwdused then
!				txerror("Normal param follows kwd")
!			fi
!			q:=nil
!			goto doregparam
		else
!doregparam:
			if kwdused then
				txerror("Normal param follows kwd")
			fi
			if k>=nparams then
				cpl =k, =nparams
				txerror("Too many params supplied")
			fi
			newarglist[++k]:=(q.tag=jnull|nil|q)
		esac
	od

!scan params, and fill in optional/default params as needed

	for i to nparams do
		q:=newarglist[i]			!will be nil of not supplied
		pm:=paramlist[i]			!formal param (an st entry)
		if q=nil then
			unless pm.optional then
				txerror_s("Param not optional: #",strint(i))
			end
			if pm.code then		!provide default value
				newarglist[i]:=duplunit(pm.code,p.pos)
			else
				newarglist[i]:=createconstunit(0,ti64)
			fi
		fi
	od

!final pass: do type-pass on each param, and apply any conversion
!I also need to build a new argument list for the call unit
	unit ulist:=nil, ulistx:=nil

	for i to nparams do
		pm:=paramlist[i]
		q:=newarglist[i]

		if pm.parammode=byref_param then
			tpass(q,m:=tttarget[pm.mode],needlv)
			qm:=q.mode

			if not comparemodes(qm,m) then
				txerror_ss("&param: type mismatch",strmode(qm), strmode(m))
			fi

			insertunit(q,jaddrof)
			q.mode:=pm.mode
		else
			tpass(q,pm.mode)
		fi

		addlistunit(ulist, ulistx, q)
		q.nextunit:=nil
	od
	p.b:=ulist

!	if t=tvoid then
!		p.tag:=jcallproc
!	fi
end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.pclop
	when katan, klog, klog10, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos,
			kfloor, kceil then
		coerceunit(a,tr64)
		resmode:=tr64

	when kneg, kabs, ksqr then
		txerror("not num") when not isnum(amode)

	when kbitnot, knot, ktoboolt then
		txerror("toboolt") when not isint(amode)

!	when ksliceptr then
!		tx_sliceptr(p,a)
!		return
	when ksign then
		resmode:=ti64

ELSE
CPL "TTT", PCLNAMES[P.PCLOP]
	esac

	p.mode:=resmode
end

proc tx_prop(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

!	if p.pclop=kksliceptr then
!		p.tag:=junary
!		p.pclop:=ksliceptr
!		tx_unary(p,a)
!		return
!	fi

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.propcode
	when kklwb, kkupb, kklen, kkbounds then
		do_bounds(p,a)
		return

	when kkbytesize,kkbitwidth then
!		size:=ttsize[(a.tag=jtypeconst|a.value|amode)]*(p.pclop=kbytesize|1|8)
!		size:=ttsize[(a.tag=jtypeconst|a.value|getmemmode(a))]*(p.pclop=kbytesize|1|8)
		size:=ttsize[(a.tag=jtypeconst|a.value|a.mode)]*(p.propcode=kkbytesize|1|8)
		makenewconst(p,size)
		resmode:=ti64

	when kkminval, kkmaxval then
!CPL "TX MIN/MAX"
		resmode:=ti64
		if a.tag=jtypeconst then
			mbase:=ttbasetype[a.value]
		else
!			mbase:=ttbasetype[getmemmode(a)]
			mbase:=ttbasetype[a.mode]
		fi

		if p.propcode=kkminval then
			case mbase
			when ti8 then x:=-128
			when ti16 then x:=-32768
			when ti32 then x:=-2_147_483_648
			when ti64 then x:=int64.min
			when tu8,tu16,tu32,tu64,tc8,tc64 then x:=0
			else
 	           txerror_s("Can't do minvalue on #",strmode(mbase))
			esac
		else
			case mbase
			when ti8 then x:=127
			when ti16 then x:=32767
			when ti32 then x:=2_147_483_647
			when ti64 then x:=0x7fff'ffff'ffff'ffff
			when tu8,tc8 then x:=255
			when tu16 then x:=65535
			when tu32 then x:=4294967295
			when tu64 then x:=0; --x; resmode:=tu64
			else
				txerror_s("Can't do maxvalue on #",strmode(mbase))
			esac
		fi
		p.tag:=jconst
		p.a:=nil
		p.value:=x
		p.isconst:=1

	when kktypestr then
		p.tag:=jconst
		if a.tag=jtypeconst then
			amode:=a.value
		else
!			amode:=getmemmode(a)
			amode:=a.mode
		fi

		p.mode:=trefchar
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return
	when kksliceptr then
		p.tag:=junary
		p.pclop:=ksliceptr
		tx_sliceptr(p,a)
		return
!		tx_unary(p,a)
!		return


ELSE
CPL "PROP", PCLNAMES[P.PCLOP]
	esac

	p.mode:=resmode
end

proc tx_unaryto(unit p,a)=
	tpasslv(a)

	case p.pclop
	when kbitnotto, knotto, ktoboolto then
		txerror("Not int") when not isint(a.mode)
	esac

	p.mode:=tvoid
end

proc tx_if(unit p,pcond,plist,pelse, int t,lv) =
	unit pc:=pcond, pl:=plist
	int u

	u:=tvoid
	if t<>tany then u:=t fi

!CPL "IF",=STRMODE(T)
!CP "A:";PRINTUNIT(P.B)

	while pc, (pc:=pc.nextunit; pl:=pl.nextunit) do
		tpass(pc)
		tpass(pl,t,lv)

		if t=tany then
			if u=tvoid then
				u:=pl.mode
			elsif lv then
				if not comparemodes(u, pl.mode) then
					txerror("IF/LV?")
				fi
			else
				u:=getdominantmode(u,pl.mode)
			fi
		fi
	od

	if t<>tvoid and pelse=nil then
		txerror("else needed")
	fi
	tpass(pelse,t,lv)

	if t=tany then
		if lv then
			if not comparemodes(u, pelse.mode) then
				txerror("IF/LV2?")
			else
				u:=getdominantmode(u,pelse.mode)
			fi
		fi
	fi

	if t<>tvoid then
		pl:=plist
		while pl, pl:=pl.nextunit do
			if t=tany then
				coerceunit(pl,u)
			fi
		od
		if t=tany then
			coerceunit(pelse,u)
		fi
		p.mode:=u
	fi

	if pcond.nextunit=plist.nextunit=nil then
!CPL "CHECKCOND", ISCONDTRUE(PCOND)
!PRINTUNIT(PCOND)
		if iscondtrue(pcond) then		!branch b only
			deleteunit(p,plist)
		elsif iscondfalse(pcond) then	!branch c only
			if pelse=nil then
				pelse:=createunit0(jblock)
			fi
			deleteunit(p,pelse)
		fi
	fi
end

proc tx_incrto(unit p,a,int t)=
	tpasslv(a)

	if t<>tvoid then
		case p.pclop
		when kincrto then p.pclop:=kincrload
		when kdecrto then p.pclop:=kdecrload
		esac
!		p.mode:=gettypebase(a.mode)
		p.mode:=a.mode
	else				!a++ a-- to ++a --a
		case p.pclop
		when kloadincr then p.pclop:=kincrto
		when kloaddecr then p.pclop:=kdecrto
		esac
		p.mode:=tvoid
	fi

	twiden(p,0)
end

proc tx_for(unit pindex,pfrom,pbody)=
	unit pto, pstep, plocal, plist
	int u

	pto:=pfrom.nextunit
	pstep:=pto.nextunit

	tpass(pindex)
	if pindex.tag<>jname then
		txerror("Loop index not a variable")
	fi
	u:=pindex.mode
	tpass(pindex.nextunit)

	tpass(pfrom,u)
	tpass(pto,u)

	tpass(pstep,u)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_forall(unit pindex,plist,pbody)=
	unit plocal,pfrom,pto,passign
	int u,mlist,elemtype

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit

	tpass(pindex,ti64)
	tpass(pfrom,ti64)
	tpass(pto,ti64)

	tpass(plist)
	mlist:=plist.mode

	case ttbasetype[mlist]
	when tarray then
		elemtype:=tttarget[mlist]
	when tslice then
		elemtype:=tttarget[mlist]
	else
		txerror("forall/can't iterate")
	esac

	tpass(plocal)
	if plocal.mode=tany then
		plocal.mode:=elemtype
		plocal.def.mode:=elemtype
	fi

	tpass(passign)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_index(unit p,a,b,int t,lv) =
!p is an index unit
!a is an array, b is an index
!t is the needed type for the element being indexed
	int amode,emode,pmode,tmode,tbasemode

	tpass(a,,lv)
	deref(a,t<>tvoid)
	amode:=a.mode

	tpass(b,ti64)			!index

	if ttbasetype[amode] not in [tarray, tslice] then
		txerror_s("Can't index: #",strmode(amode))
	fi
	p.mode:=tttarget[amode]
	twiden(p,lv)
end

proc tx_makerange(unit p,a,b)=
	int amode,bmode

	tpass(a,ti64)
	tpass(b,ti64)

	amode:=a.mode
	bmode:=b.mode

	coerceunit(a,ti64)
	coerceunit(b,ti64)
	p.mode:=trange
end

proc tx_ptr(unit p,a,int t,lv)=
	symbol d


	tpass(a)
!CPL "TXPTR"; PRINTUNIT(P)

	case ttbasetype[a.mode]
	when tvoid then
		txerror("Deref Void")
	when tref then
		p.mode:=tttarget[a.mode]

	when tslice then
CPL "DEREF SLICE"
!		txerror("Can't deref slice")
	else
		txerror("PTR: need ref T")
	esac
!CPL "TXPTR2"; PRINTUNIT(P)

	twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]symbol fieldlist
	int i,nfields,indent,nrfields,size,index, maxalign
	symbol d,e
	ref char flags
	const ss='S', ee='E'
	int flag

	if ttsize[m] then return fi

	d:=ttnamedef[m]
	e:=d.deflist
	nfields:=0

	fieldlist[++nfields]:=symbol(ss)

	while e do
		if e.nameid=fieldid then
			if nfields>=maxfields then
				gerror("srs:too many fields")
			fi

			setmodesize(e.mode)
			flags:=cast(&e.uflags)
			docase flags^
			when 'S', 'U' then
				flag:=flags^
				fieldlist[++nfields]:=symbol(flag)
				++flags
			else
				exit
			end docase

			fieldlist[++nfields]:=e

			do
				flag:=flags++^
				case flag
				when '*'  then
				when 'E' then
					fieldlist[++nfields]:=symbol(ee)
				else
					exit
				esac
			od
		fi

		e:=e.nextdef
	od

	fieldlist[++nfields]:=symbol(ee)
	fieldlist[nfields+1]:=nil			!terminator

	countedfields:=0
	index:=2
	maxalign:=1
	scanrecord('S',&fieldlist,index,size,0, d.align, maxalign)

	if d.align then
		size:=roundoffset(size,maxalign)
		d.maxalign:=maxalign
	else
		d.maxalign:=1
	fi

	ttsize[m]:=size
	ttlength[m]:=countedfields
	ttlower[m]:=1

	checkblocktype(m)
end

proc checkblocktype(int m)=
	case ttsize[m]
	when 1,2,4,8 then
		ttisblock[m]:=0
	esac
end

proc scanrecord(int state,ref[]symbol fields, int &index, &isize, offset, calign, &maxalign)=
 	symbol e,f,ea
	int size:=0,fieldsize,bitoffset:=0, alignment, newoffset

	while f:=fields^[index++] do
		case int(f)
		when 'S','U' then
			scanrecord(int(f),fields, index,fieldsize, offset, calign, maxalign)
		when 'E' then			!end of this nested block
			if state='U' then ++countedfields fi
			isize:=size
			return
		else
			if f.mode=tbitfield then
				fieldsize:=0
				ea:=f.equivfield
				f.offset:=ea.offset
				f.bitoffset:=bitoffset
				bitoffset+:=f.bitfieldwidth
				if bitoffset>ttsize[f.equivfield.mode]*8 then
					txerror("Bit fields overflow type")
				fi

			elsif f.atfield then
				bitoffset:=0
				e:=f.equivfield
				fieldsize:=0
				f.offset:=e.offset+f.equivoffset
			else
				bitoffset:=0
				if state='S' then ++countedfields fi
				fieldsize:=ttsize[f.mode]
				if calign then
					alignment:=getalignment(f.mode)
					if alignment>maxalign then maxalign:=alignment fi
					newoffset:=roundoffset(offset,alignment)
					size+:=newoffset-offset
				else
					newoffset:=offset
				fi
				f.offset:=newoffset
				offset:=newoffset
			fi
		esac
		if state='S' then
			offset+:=fieldsize
			size+:=fieldsize
		else
			size:=max(size,fieldsize)
		fi
	od
end

func roundoffset(int offset, alignment)int=
	int mask

	if alignment=1 then return offset fi
	mask:=alignment-1
	while offset iand mask do ++offset od

	return offset
end

proc tx_convert(unit p,a,int hard=0)=
	case a.tag
	when jmakelist then
		tx_makelist(a,a.a,p.convmode,nolv)
	else
		tpass(a)
		coerceunit(a,p.convmode,hard)
	esac
	deleteunit(p,a)			!get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
	int alength,tlength,elemtype,newt, i, nfields,isconst, m
	unit q,b
	symbol e

	alength:=p.length
	newt:=0
	isconst:=1

	tlength:=ttlength[t]

	if tlength then
		if alength<tlength then
			txerror_ss("Too few elements",strint(alength), strint(tlength))
		elsif alength>tlength then
			txerror_ss("Too many elements",strint(alength), strint(tlength))
		fi
	fi

	case ttbasetype[t]
	when tarray then
		elemtype:=tttarget[t]
		if tlength=0 then
			newt:=createarraymodek(nil, elemtype, ttlower[t],alength,0)
		else
			newt:=t
		fi
		q:=a
		while q do
			tpass(q,elemtype,lv)

			unless q.tag=jconst then isconst:=0 end
			q:=q.nextunit
		od

		p.mode:=newt

	when trecord then
		e:=ttnamedef[t].deflist
		q:=a
		while q and e do
			if e.nameid=fieldid then 
				while e.mode=tbitfield do
					e:=e.nextdef
					if not e then exit fi
				od

				tpass(q,e.mode,lv)

				unless q.tag=jconst then isconst:=0 end
				q:=q.nextunit
			fi

			e:=e.nextdef
		od
		while e and (e.nameid<>fieldid or e.mode=tbitfield) do
			e:=e.nextdef
		od
		if q or e then
			txerror("Can't initialise unions")
		fi
		p.mode:=t

	when tslice then
CPL "TSLICE"
!!		if a=nil or (b:=a.nextunit; b=nil) or b.nextunit then
!!			txerror("bad slice init")
!!		fi
!
!		if a=nil then
!			txerror("bad slice init")
!		else
!			b:=a.nextunit
!			if b=nil or b.nextunit then
!				txerror("bad slice init")
!			fi
!		fi
!
!
!		p.b:=b
!!		p.hasb:=1
!		a.nextunit:=nil
!		tpass(a,,lv)
!		if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
!		if tttarget[a.mode]<>tvoid then
!			if not comparemodes(tttarget[a.mode],tttarget[t]) then
!				txerror("slice/ptr mismatch")
!			fi
!		fi
!
!		tpass(b,ti64)
!		p.mode:=t
!CPL "MKSLICE1"
!		p.tag:=jmakeslice
!		p.resultflag:=1

	else
		txerror_s("Unknown makelist type: #",strmode(t))
	esac

	p.isconst:=isconst

	tpass(p.b,ti64)				!lower


IF P.TAG<>JMAKESLICE THEN

	if not inidata and isconst then
		e:=getavname(currproc,staticid)
		e.mode:=t
		addstatic(e)
		q:=createunit0(jnone)
		q^:=p^
		e.code:=q
		p.tag:=jname
		p.def:=e
	fi
FI
end

proc tx_makeslicefromlist(unit p,a, int t)=
CPL "MAKESLICE/TX"

TXERROR("MAKESLICE FROM LIST NOT READY")

!	p.b:=a.nextunit
!	a.nextunit:=nil
!	tpass(a)
!
!	if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
!	if tttarget[a.mode]<>tvoid then
!		if not comparemodes(a.mode,createrefmode(nil,tTtarget[t])) then
!			txerror("slice/ptr mismatch")
!		fi
!	fi
!
!	tpass(p.b,ti64)
!	p.mode:=t
!CPL "MKSLICE2"
!	p.tag:=jmakeslice
!	p.resultflag:=1
!
!	tpass(p.b,ti64)

end

proc tx_makeslice(unit p, a,b, int t)=
CPL "MAKESLICE/TX"
!	p.b:=a.nextunit
!	a.nextunit:=nil
	tpass(a)

	if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
	if tttarget[a.mode]<>tvoid then
		if not comparemodes(a.mode,createrefmode(nil,tTtarget[t])) then
			txerror("slice/ptr mismatch")
		fi
	fi

	tpass(b,ti64)
	p.mode:=t
CPL "MKSLICE2"
!	p.tag:=jmakeslice
	p.resultflag:=1

!	tpass(p.b,ti64)

end

proc tx_makeset(unit p,a, int t)=
	p.isconst:=1
!CPL "MAKESET"

!	if ttbasetype[t]=tslice then
!		tx_makeslice(p,a,t)
!		return
!	fi

	while a, a:=a.nextunit do
		tpass(a)

		if not a.isconst then
			p.isconst:=0
		fi
	od

	p.mode:=tvoid
end

proc tx_dot(unit p,a,b,int lv)=
	int recmode,recbasemode,i,j,newtag,tmode
	unit q,pindex
	symbol d,dequiv

	tpass(a)			!lhs, yeields ref array type

	recmode:=a.mode

	recbasemode:=ttbasetype[recmode]

	while recbasemode=tref do
		tmode:=tttarget[recmode]
		insertunit(a,jptr)
		recmode:=a.mode:=tmode
		recbasemode:=ttbasetype[recmode]
	od

	if ttbasetype[recmode]<>trecord then
		txerror("Bad record type")
	fi

	d:=b.def

	if d.nameid=nullid then			!not resolved; lhs mode wasn't available
		d:=b.def:=resolvefield(d,recmode)
	fi

	if d.mode=tbitfield then
		i:=d.bitoffset
		j:=i+d.bitfieldwidth-1
		dequiv:=d.equivfield

		b.def:=dequiv				!change from bitfield field to containing int
		b.mode:=dequiv.mode
		p.offset:=d.offset

		if i=j then					!single bit
			pindex:=createconstunit(i,ti64)
			newtag:=jdotindex
		else						!bit slice
			pindex:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
			pindex.mode:=trange
			pindex.a.resultflag:=1
			pindex.b.resultflag:=1
			newtag:=jdotslice
		fi

		p.mode:=b.mode
		twiden(p,lv)
		insertunit(p,newtag)
		p.mode:=tu64
		p.b:=pindex
		p.a.resultflag:=1
		p.b.resultflag:=1
		p.resultflag:=1

		return

	fi

	b.mode:=d.mode
	p.mode:=d.mode

	p.offset:=d.offset
	twiden(p,lv)
end

func resolvefield(symbol d, int m)symbol=
	symbol e,t

	case ttbasetype[m]
	when trecord then
	when tref then
		m:=tttarget[m]
		if ttbasetype[m]<>trecord then
			txerror("3:record expected")
		fi
	else
		txerror("4:record expected")
	esac
	t:=ttnamedef[m]

	e:=finddupl(t,d)
	if not e then
		txerror_s("Not a field: #",d.name)
	fi
	return e
end

proc tx_andl(unit p,a,b)=
	tpass(a,tbool)
	tpass(b,tbool)

	p.mode:=tbool
end

proc convintconst(unit p,int64 x)=
!convert unit p into int const x
	p.tag:=jconst
	p.mode:=ti64
	p.a:=p.b:=p.c:=nil
	p.value:=x
	p.isconst:=1
end

proc tx_sliceptr(unit p,a)=
	int m,tmode

	tpass(a)
	m:=a.mode

	case ttbasetype[m]
	when tslice then
	else
		txerror_s("SLICEPTR #",strmode(m))
	esac

!for when ptr is to be pointer to the array
	tmode:=createarraymodek(nil, tttarget[m], ttlower[m],0,0)

!for when ptr is to be pointer to the array element (although either can be
!cast to the other); although try alternate .sliceptr versions too
!tmode:=tttarget[m]

	p.mode:=createrefmode(nil,tmode)
end

proc tx_swap(unit p,a,b)=
	int av, bv

	tpasslv(a)
	tpasslv(b)

	if not comparemodes(a.mode,b.mode) then
		txerror("SWAP: type mismatch")
	fi

	p.mode:=tvoid
end

proc tx_select(unit p,a,b,c, int t,lv)=
	int i,u
	unit q

	tpass(a,ti64)

	q:=b
	while q do
		tpass(q,t,lv)
		if q=b then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	tpass(c,t,lv)
	u:=getdominantmode(u,c.mode)

	q:=b
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	p.mode:=u
end

proc tx_case(unit p,a,b,c, int t,lv)=
	int amode,u
	unit wt,w

	if p.tag=jdocase and lv then gerror("&docase") fi

	tpass(a)

	if a=nil then
		amode:=tany
	else
		amode:=a.mode
	fi

	if ttisinteger[amode] and ttsize[amode]<8 then
		coerceunit(a,tint)
		amode:=tint
	fi
	u:=tvoid

	wt:=b
	while wt do				!whenthen chain
		w:=wt.a
		while w do				!each expr between when...then
			tpass(w)
			if w.tag=jmakerange then
				unless ttisinteger[amode] then txerror("case: need int index") end
			else
				if amode=tany then
						if not isbooltag[w.tag] then
							TXERROR("CASE/BOOL?")
							insertunit(w,jistruel)
						fi
				else
					coerceunit(w,amode)
				fi
			fi
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)			!process block
		if t<>tvoid then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi
		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		fi
	elsif t<>tvoid then
		txerror("case needs else")
	fi

	if t<>tvoid then
		p.mode:=u
	else
		p.mode:=tvoid
	fi

end

proc tx_notl(unit p,a)=
	tpass(a)
	p.mode:=tbool
end

proc tx_typepun(unit p,a)=
	int smode,tmode
	case a.tag
	when jmakelist then
		TXERROR("TYPEPUN/LIST")
	else
		tpass(a)

		smode:=ttbasetype[a.mode]
		tmode:=ttbasetype[p.convmode]

		unless ttisreal[smode] and ttisinteger[tmode] or
			ttisinteger[smode] and ttisreal[tmode] then
			txerror("Invalid type-punning; only real<->int")
		end
		IF TMODE IN [TI32, TU32] THEN TMODE:=TI64 FI
		p.mode:=tmode
	esac
end

proc tx_exit(unit p,a)=
	if a=nil then return fi
	tpass(a,ti64)
	if a.tag<>jconst then
		txerror("exit/etc not const")
	fi
	p.index:=a.value
	p.a:=nil
end

proc tx_goto(unit p,a)=
	int m

	tpass(a)
	m:=a.mode

	if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
		txerror("goto: not label")
	fi
end

proc tx_switch(unit p,a,b,c,int t,lv)=
	[0:2048]byte valueset
	unit wt, w
	int ax,bx,i,u



	if p.tag=jdoswitch and lv then gerror("&doswitch") fi

	tpass(a,ti64)

	memset(&valueset,0,valueset.bytes)
	u:=tvoid

	wt:=b
	while wt do

		w:=wt.a
		while w do
			tpass(w)

			if not isconstunit(w) then
PRINTUNIT(W)
 txerror("Switch not constant") fi

			case ttbasetype[w.mode]
			when trange then			!assume makerange
				ax:=w.a.value
				bx:=w.b.value
	dorange:
				for i:=ax to bx do
					if i<valueset.lwb or i>valueset.upb then
						txerror("switch: value out of range")
					fi
					if valueset[i] then
						cpl i
						txerror("Duplicate switch value")
					fi
					valueset[i]:=1
				od
			else
				coerceunit(w,ti64,0)
				tevaluate(w)
				if w.tag<>jconst then
					txerror("Switch value: not const int")
				fi
				ax:=bx:=w.value
				goto dorange
			esac
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)

		if t=tany then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi

		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		fi
	elsif t<>tvoid then
		txerror("switch needs else")
	fi

	if t<>tvoid then
		w:=b.a
		while w do				!all elseif unots
			if t=tany then
				coerceunit(b.b,u)
			fi
			w.mode:=b.b.mode
			w:=w.nextunit
		od
		if t=tany then
			coerceunit(c,u)
			p.mode:=u
		else
			p.mode:=t
		fi
	else
		p.mode:=tvoid
	fi
end

proc tx_addroffirst(unit p,a,int t)=
!&.x maps to &x[x.lwb]
	int m

	tpass(a)

	m:=a.mode
	if ttbasetype[m]<>tarray then
		txerror("&. ref[] expected")
	fi

	m:=createrefmode(nil,tttarget[m])
	if a.tag=jname then
		a.addroffirst:=1
	fi
	p.mode:=m
end

proc tx_return(unit p,a, int t)=
 	int m,nvalues,nret,i
	ref[]int32 pmult
	unit q

	m:=currproc.mode
	nret:=currproc.nretvalues
	pmult:=ttmult[currproc.mode]

	if a=nil then
		if nret then
			txerror("return value(s) missing")
		fi
		return
	elsif nret=0 then
		txerror("Superfluous return value")
	fi

	if a.tag=jmakelist then
		a.tag:=jreturnmult
		if a.length<>nret then
			case ttbasetype[m]
			when trecord, tarray then
				txerror("return constructor not supported")
			else
				txerror("Wrong number of return values")
			esac
		fi
		q:=a.a				!point to list of return values
		for i to nret do
			tpass(q,pmult[i])
			q:=q.nextunit
		od

		deleteunit(p,a)			!don't need return
		if t=tvoid then
			p.mode:=tvoid
		else
			p.mode:=ttuple
		fi

	else
		if nret>1 then txerror("RETERROR?") fi
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			p.mode:=tvoid
		else
			deleteunit(p,a)
!			P.MODE:=A.MODE
		fi
	fi

	IF TTISSHORT[P.MODE] THEN TXERROR("SHORT RET TYPE") FI
end

proc tx_dotindex(unit p,a,b,int lv) =
!a.[b], a is an int
	int pmode
	unit i,j

	tpass(a,,lv)			!lhs

	pmode:=tu64

!CPL "DOTIX1"; PRINTUNIT(P)

	if not ttisinteger[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.convmode:=tu64
			a.resultflag:=1
!CPL "DOTIX2"; PRINTUNIT(P)

		else
			txerror("a.[i]: not int/str value")
		fi
	fi

	tpass(b)			!index

	case ttbasetype[b.mode]
	when trange then
		i:=b.a
		j:=b.b
		if i.tag=j.tag=jconst then
			if i.value>j.value then
				swap(b.a,b.b)
			fi
		fi
	else					!assume simple index
		coerceunit(b,ti64)
	esac

	p.mode:=pmode
!CPL "DOTIX3"; PRINTUNIT(P)
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

	tpass(a)			!lhs
	tpass(b)			!will be a range

	if a.mode=trefchar then
		p.mode:=createslicemodek(currproc,tc8,1,0)
	else
		deref(a)
		case ttbasetype[a.mode]
		when tarray then
			p.mode:=createslicemodek(currproc,tttarget[a.mode],1, 0)

		when tslice then
			p.mode:=a.mode

		else
			CPL =STRMODE(A.MODE)
			txerror("a[i..j]: not array")
		esac
	fi
end

proc twiden(unit p, int lv)=
!intended for widening narrow types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
	int m,u,mbase

	mbase:=ttbasetype[m:=p.mode]

	if mbase=tvoid then return fi		!nothing to widen (error?)
	if lv then return fi				!lv, keep memory mode as dest

	if not ttisshort[mbase] then return fi	!no widening needed

	case p.tag
!	when jname, jptr, jindex, jdot, jcallproc, jcallfn, jincrload, jloadincr then
	when jname, jptr, jindex, jdot, jcall, jincr then
		insertunit(p, jconvert)
		p.convcode:=kkwiden
		p.convmode:=m
		p.mode:=gettypebase(m)
	else
		PRINTUNIT(P)
		txerror_s("widen? #",jtagnames[p.tag])
	esac
end

proc tstringslice(unit p, int slicemode)=
!p is a string; insert conversions to turn it into a slice:
	unit a,b,prange
	int length

	if tttarget[slicemode]<>tc8 then
		txerror("Not char slice")
	fi
!
	a:=p
	insertunit(p,jslice)


	if p.a.tag=jconst then
	else
		b:=duplunit(p.a)
		insertunit(b,junary)
		prange:=createunit2(jmakerange,createconstunit(1,ti64),b)

		prange.mode:=trange
		p.b:=prange
	fi

	p.mode:=slicemode
end

proc tx_bitfield(unit p,a,int lv)=
	int i,j,bitsize,topbit
	unit r

	tpass(a,,lv)

	if not ttisinteger[a.mode] and not ttisref[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.convmode:=tu64
			a.resultflag:=1
		else
			txerror("Int/ref needed")
		fi
	fi

	bitsize:=ttsize[ttbasetype[a.mode]]*8
	topbit:=bitsize-1

	case p.bfcode
	when bf_lsb then
		i:=0; j:=7

	when bf_msb then
		j:=topbit
		i:=topbit-7

	when bf_lsbit then
		i:=j:=0

	when bf_odd,bf_even then
		if lv then
			txerror("Can't assign")
		fi
		i:=j:=0

	when bf_msbit then
		i:=j:=topbit

	when bf_lsw then
		i:=0
		j:=bitsize/2-1

	when bf_msw then
		i:=bitsize/2
		j:=topbit
	else
		CPL P.BFCODE
		TXERROR("BITFIELD")
	esac

	if i=j then			!single bit
		p.tag:=jdotindex
		p.b:=createconstunit(i,ti64)
		p.resultflag:=1
		p.b.resultflag:=1

		if p.bitopindex=bf_even then
			p.mode:=tu64
			addnotl(p)
		fi

	else
		r:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
		r.a.resultflag:=1
		r.b.resultflag:=1
		r.mode:=trange
		p.tag:=jdotslice
		p.b:=r
	fi

	p.mode:=tu64
end

proc deref(unit a, int needres=1)=
!a is a unit that needs to be dereferenced because it's about to used as:
! a[i]
! a[i..j]
! a.lwb, a.upb, a.len
!Ie in an array context
	int abasemode, tmode

	abasemode:=ttbasetype[a.mode]

	while abasemode=tref do
		tmode:=tttarget[a.mode]

		insertunit(a,jptr)
		a.mode:=tmode

		abasemode:=ttbasetype[a.mode]
	od

end

proc tmethodcall(unit p, pdot, pargs)=
	int mrec
	unit prec, pfield, pfunc
	symbol d,e

	prec:=pdot.a
	pfield:=pdot.b
	mrec:=prec.mode
	d:=pfield.def

	e:=resolvefield(d,mrec)

	if e=nil then
		txerror_s("Can't resolve method:",d.name)
	fi

	pfunc:=createname(e)
	pfunc.mode:=e.mode
	prec.nextunit:=pargs

	p.a:=pfunc
	p.b:=prec
end

proc do_bounds(unit p,a) =
	int m,mbase,opc,lower,upper

	deref(a)

	m:=a.mode
	if a.tag=jtypeconst then m:=a.value fi

	mbase:=ttbasetype[m]
	p.mode:=ti64

	case p.pclop
	when kklwb then
		case mbase
		when tarray,tslice then
			convintconst(p,ttlower[m])
			return
		else
error:
			txerror_s("lwb/upb/len?",strmode(m))
		esac

	when kkupb then
		case mbase
		when tarray then
			convintconst(p,ttlower[m]+ttlength[m]-1)
		when tslice then
			p.tag:=junary
			p.pclop:=ksliceupb
		else
			goto error
		esac

	when kklen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.tag:=junary
			p.pclop:=kslicelen
		else
			goto error
		esac
	when kkbounds then
		p.mode:=trange
		case mbase
		when tarray then
			p.range_lower:=ttlower[m]
			p.range_upper:=p.range_lower+ttlength[m]-1
			p.tag:=jconst
			p.a:=p.b:=p.c:=nil
			p.isconst:=1
			return

		when tslice then
!		when ti32 then
!			convintconst(p,int32.max-int32.min+1)
!			return
		else
			goto error
		esac
	esac
end

proc addnotl(unit p)=
	insertunit(p,jnotl)
	p.mode:=tbool
	p.pclop:=knot
end

proc tevaluate(unit p)=
	unit a,b,pname
	int offset

	int tag:=p.tag

	if jisexpr[tag]=2 then
		tevalbinop(p)

	elsif jisexpr[tag]=1 then
		tevalmonop(p)

	elsecase tag
	when jmakerange then
		a:=p.a
		b:=p.b
		if ttsize[a.mode]<=8 then			!const range only for 32-bits
			tevaluate(a)
			tevaluate(b)
			if a.tag=jconst and b.tag=jconst then
				p.isconst:=a.isconst iand b.isconst
			fi
		fi

	when jaddrof then
		a:=p.a

		pname:=addrdotindex(a, offset)

		if pname then
			deleteunit(a,pname)
			if p.b=nil then
				p.b:=createconstunit(offset,ti64)
			else 
				p.b.value+:=offset
			fi
		fi
	fi

end

func addrdotindex(unit p, int &offset)unit q=
	int axmode

	case p.tag
	when jdot then
		if p.a.tag=jname then
			offset:=p.offset
			return p.a
		else
			q:=addrdotindex(p.a,offset)
			offset+:=p.offset
			return q
		fi
	when jindex then
		axmode:=p.a.mode
		if p.b.tag=jconst then
			if p.a.tag=jname then
				offset:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				return p.a
			else
				q:=addrdotindex(p.a,offset)
				if q then
					offset+:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				fi
				return q
			fi
		else
			return nil
		fi
	else
		return nil
	esac

end

proc tevalbinop(unit p)=
	int64 a,b,c,offset
	real x,y,z
	unit lhs, rhs

	lhs:=p.a
	rhs:=p.b

	unless lhs.tag=rhs.tag=jconst then
!		if lhs.tag=jaddrof and rhs.tag=jconst AND P.PCLOP=KADDREFX then		!ASSUME ADD/SUBREFX
		if lhs.tag=jaddrof and rhs.tag=jconst then		!ASSUME ADD/SUBREFX
			if lhs.a.tag=jname then			!reduce addrof(a)+k => addrof(a,k)
				offset:=rhs.value*ttsize[tttarget[lhs.mode]]
				if p.pclop=ksubpx then
					offset:=-offset
				fi
				if lhs.b=nil then
					lhs.b:=createconstunit(offset,ti64)
				else
					lhs.b.value+:=offset
				fi
				deleteunit(p,lhs)
			fi
		fi
		return
	end

	if ttisreal[p.mode] then
		x:=p.a.xvalue
		y:=p.b.xvalue
	else
		a:=p.a.value
		b:=p.b.value
	fi

	case p.mode
	when ti64, tu64 then

		case p.pclop
		when kadd then c:=a+b
		when ksub then c:=a-b
		when kmul then c:=a*b
		when kidiv then
			if b=0 then txerror("x/0") fi
			c:=a/b
		when kirem then
			if b=0 then txerror("x rem 0") fi
			c:=a rem b
		when kshl then c:=a<<b

!		when keq then c:=a=b
!		when kne then c:=a<>b
!		when klt then c:=a<b
!		when kle then c:=a<=b
!		when kge then c:=a>=b
!		when kgt then c:=a>b

		when kbitand then c:=a iand b
		when kbitor then c:=a ior b
		when kpower then c:=a ** b
		else
			return
		end

	when tr64,tr32 then

		case p.pclop
		when kadd then z:=x+y
		when ksub then z:=x-y
		when kmul then z:=x*y
!		when kdiv then z:=x/y
		when kpower then z:=x**y

		else
			return
		end
	else
		return
	esac
!
	if ttisreal[p.mode] then
		makenewconst(p,int64@(z))
	else
		makenewconst(p,c)
	fi
end

proc tevalmonop(unit p)=
	int64 a,b,c
	real x,z

	unless p.a.tag=jconst then
		return
	end

!CPL "EVALMON", PCLNAMES[P.PCLOP], JTAGNAMES[P.TAG]
!PRINTUNIT(P)
!
	a:=p.a.value
	x:=p.a.xvalue

	case p.mode
	when ti64, tu64 then

		if p.tag in [jistruel, jisfalsel] then dobool fi

		case p.pclop
		when kneg then c:=-a

!		when ktoboolt then
!
!CPL "EVALMONO/XXTOBOOLT1"
!
! c:=istrue a; p.mode:=tbool
		when knot then c:=not a; p.mode:=tbool
		when kbitnot then c:=inot a
		when kabs then c:=abs a

		else
			return
		esac
	when tr64, tr32 then
		case p.pclop
		when kneg then z:=-x
		when katan then z:=atan(x)
		when ksqrt then z:=sqrt(x)

		else
			return
		esac

	when tbool then

dobool:
		case p.tag
		when jistruel then c:=istrue a; p.mode:=tbool
		when jisfalsel then c:=not a; p.mode:=tbool
		elsecase p.pclop
		when ktoboolt then c:=istrue a; p.mode:=tbool
		when knot then c:=not a; p.mode:=tbool
		esac
	else
		return
	esac

	if ttisreal[p.mode] then
		makenewconst(p,int64@(z))
	else
		makenewconst(p,c)
	fi
!CPL "NEW:"
!PRINTUNIT(P)
end

func iscondtrue(unit p)int =
	p.tag=jconst and p.value<>0
end

func iscondfalse(unit p)int =
	p.tag=jconst and p.value=0
end

proc fixchararray(unit a)=
!turn []char into ichar at certain points
	if a and ttbasetype[a.mode]=tarray and tttarget[a.mode]=tc8 then
		coerceunit(a,trefchar,0)
	fi
end

proc combinestrings(unit p)=
!p is (add, a, b) where a and b are string constants.
	unit a:=p.a, b:=p.b
	int alen:=a.slength
	int blen:=b.slength
	int clen, needterm
	byte atype:=a.strtype, btype:=b.strtype, ctype
	ichar s

	if atype=btype='B' then
		needterm:=0
		ctype:='B'
	elsif atype='B' or btype='B' then
		txerror("Mixed str+bin strings")
	else					!both are string/strdata
		--alen				!lose zero terminator
		--blen
!CPL "CS1",ALEN,BLEN

		needterm:=1
		if atype='S' or btype='S' then		!either strdata then both are
			ctype:='S'
		else
			ctype:=0
		fi
	fi
	clen:=alen+blen

	if blen=0 then
		deleteunit(p,a)
		return
	elsif alen=0 then
		deleteunit(p,b)
		return
	fi

	s:=pcm_alloc(clen+needterm)
	memcpy(s,a.svalue,alen)
	memcpy(s+alen,b.svalue,blen)
	if needterm then
		(s+clen)^:=0
	fi

	deleteunit(p,a)
	p.slength:=clen+needterm
	p.svalue:=s
	p.strtype:=atype
end

proc mulstrings(unit p)=
!p is (add, a, b) where a and b are string constants.
	unit a:=p.a, b:=p.b
	int alen:=a.slength
	int scale:=b.value
	int clen, needterm
	byte atype:=a.strtype, ctype
	ichar s, t

	--alen				!lose zero terminator

	needterm:=1
	if atype='S' then needterm:=1 fi

	clen:=alen*scale
	if scale<1 or clen<1 or clen>100000 or alen<1 then txerror("mulstr") fi

	t:=s:=pcm_alloc(clen+needterm)
	to scale do
		memcpy(t,a.svalue,alen)
		t+:=alen
	od
	if needterm then
		(s+clen)^:=0
	fi

	deleteunit(p,a)
	p.slength:=clen+needterm
	p.svalue:=s
	p.strtype:=atype
end

proc tx_strinclude(unit p,a)=
	int fileno
	ifile pf

	tpass(a)
	if a.tag<>jconst or not a.isastring then
		txerror("strincl/not string")
	fi

	fileno:=modules[p.moduleno].fileno

	pf:=getsupportfile(a.svalue,path:sources[fileno].path)

	a.svalue:=pf.text
	a.slength:=pf.size+1
	a.strtype:=p.strtype

	if a.strtype='B' then				!string
		--a.slength						!there will already be zero-terminator
	fi
!
!CPL "DONE STRINCL",A.STRTYPE
	deleteunit(p,a)
end

proc coerceunit(unit p, int t, hard=0)=
	int opc, s:=p.mode, n

	if t=tvoid or s=t then return fi
	if s=tvoid then
		txerror("Void expression/return value missing")
	fi

	if s=t then return fi

!	opc:=getconversionop(s,t, hard)

	int sbase:=ttbasetype[s]
	int tbase:=ttbasetype[t]

!FPRINTLN "GCO # => #", STRMODE(S), STRMODE(T)

	opc:=kkerror
	int starg:=tttarget[s]
	int ttarg:=tttarget[t]

	if s=trefchar then sbase:=trefchar fi
	if t=trefchar then tbase:=trefchar fi

	if sbase in tfirstnum..tlastnum then
		if tbase in tfirstnum..tlastnum then
			opc:=softconvtable[sbase,tbase]
		elsecase tbase
		when tref, trefchar then
			opc:=kksoftconv
checkhard:
			if not hard then opc:=kkharderr fi
		elsif tbase in tfirstshort..tlastshort then
			if ttisinteger[sbase] then
				if not hard then				!needed for idata init
					opc:=kksofttrun
				else
					opc:=kktruncate
				fi
			fi
		elsecase tbase
		when tbool then
			opc:=kktoboolt
		when ttype then
			opc:=kksoftconv
		fi

	elsecase sbase
	when tbool then
		if tbase in [ti64, tu64] then
			opc:=kksoftconv
		fi

	when tref then
		case tbase
		when ti64, tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if starg=tvoid or ttarg=tvoid then			!at least one is ref void
				opc:=kksoftconv
			else
checkref:
				opc:=kksoftconv
				if not comparemodes(s,t) then
					checkhard
				fi
			fi
		when trefchar then
			checkref
		when tbool then
			opc:=kktoboolt
		end

	when trefchar then
!CPL "REFCHAR"
!FPRINTLN "GCO/REFCHAR # => #", STRMODE(S), STRMODE(T),=HARD
		case tbase
		when ti64,tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if comparemodes(s,t) or hard then
				opc:=kksoftconv
!CPL "SOFT"
			else
!CPL "HARD1"
				opc:=kkharderr
			fi
		when tbool then
			opc:=kktoboolt
		when tslice then
!			if ttarg not in [tc8, tu8] then
			if ttarg in [tc8, tu8] then
				opc:=kkichar2sl
			fi
		when tarray then
			if p.tag=jconst and p.strtype then
!CPL "CONSTR STR:",=P.ISSTRINCL,=T
!PRINTUNIT(P)		
				opc:=kksoftconv
				n:=ttlength[t]
				if n=0 then
					ttlength[t]:=p.slength/ttsize[tttarget[p.mode]]
					ttsize[t]:=p.slength
				else
					txerror("Array not empty")
				fi
!				CPL =N, =P.SLENGTH
			fi


		end

	when tarray then
		case tbase
		when tarray then
			if comparemodes(s,t) then
				opc:=kksoftconv
			fi
		when tslice then
			if comparemodes(starg, ttarg) then
				opc:=kkax2slice
			fi

		when trefchar then
			if starg in [tc8, tu8] then
				opc:=kkcx2ichar
			fi
		when tref then
			if ttarg=tvoid then
				opc:=kkcx2ichar
			fi
!	ELSE
!CPL "GETCONV =>ARRAY",STRMODE(S)

		esac

	when tslice then
		case tbase
		when tslice then
			if comparemodes(s,t) then
				opc:=kksoftconv
			fi
		when tref then
			if ttarg=tvoid or comparemodes(starg, ttarg) then
				opc:=ksliceptr
			fi

!		when tbool then
		esac

	when ttype then
		if tbase<=tlastnum then
			opc:=kksoftconv

		fi
	fi

!CPL "BEFORE APPLY",STRMODE(T),PCLNAMES[OPC]
!PRINTUNIT(P)
	applyconversion(p,s,t,opc)
!CPL "AFTER APPLY"
!PRINTUNIT(P)


end

proc applyconversion(unit p, int s,t, opc)=
!deal with conversion op applied to p:
! do nothing
! report error
! insert special node
! attempt compile-time conversion
! insert convert node
! set p's mode etc

	case opc
	when kkerror then
		txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

	when kkharderr then
!CPL =S,=T,=TTTARGET[T],"//",STRMODE(40),=TTLENGTH[40]
		txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

	when kksoftconv then
!		if p.tag=jaddrof or comparemodes(s,t) then
			p.mode:=t
			return
!		fi
	when kksofttrun then
		if tevalconvert(p,s,t,opc) then
			return
		fi
		insertunit(p,jshorten)
		p.mode:=t			!don't use the short target mode
		return

	when kkax2slice then
		insertunit(p,jslice)
		p.mode:=t
		return
	when kkichar2sl then
		tstringslice(p,t)
		return

	when kkcx2ichar then
		insertunit(p,jaddroffirst)
		p.mode:=trefchar
		return
	esac

	if tevalconvert(p,s,t,opc) then		!try and apply it directly
		return
	fi

!have to add an explict conversion node
	insertunit(p, jconvert)
	p.pclop:=opc

	p.convmode:=s
	p.resultflag:=1

!???
	if ttisshort[t] then
		p.convmode:=t
		t:=gettypebase(t)
	fi

	p.mode:=t
end

proc checkmodes(int s,t)=
	if not comparemodes(s,t) then
		txerror_ss("Type-compare error: # <-> #",strmode(s), strmode2(t))
	fi
end

func comparemodes(int s,t)int=
!return 1 if modes s,t are compatible. That is, ref s/ref t would be interchangeable.
!a direct compare may be false because refs/arrays but be constructed at
!different times
	int sbase, tbase, starg, ttarg
	symbol d,e

	if s=t then return 1 fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]
	starg:=tttarget[s]
	ttarg:=tttarget[t]

	if sbase=tbase then
		case sbase
		when tref then
			if starg=tvoid or ttarg=tvoid then
				return 1
			fi
			return comparemodes(starg,ttarg)

		when tarray then
			if not comparemodes(starg, ttarg) then return 0 fi
			if ttlength[s]=ttlength[t] or ttlength[s]=0 or ttlength[t]=0 then
				return 1
			fi
		when tslice then
			return comparemodes(starg, ttarg)

		when tproc then
			d:=ttnamedef[s]
			e:=ttnamedef[t]
			if d and e then
				if not comparemodes(d.mode,e.mode) then return 0 fi
				if d.paramlist=nil and e.paramlist=nil then return 1 fi
			fi
		esac

	elsif sbase=tc8 and tbase=tu8 or sbase=tu8 and tbase=tc8 then
		return 1
	else
!else needs complex param/result-matching
!...
	fi
	return 0
end

func tevalconvert(unit p,int s,t,opc)int=
!conversion op opc to convert from s to t is about to be applied to be
!try and do that at compile time to avoid adding a runtime conversion
!return 1 if it could apply it, 0 if it couldn't
!caller should have already evaluated p to reduce constants etc
	real x,z
	int a,c,sbase,tbase
!
	if p.tag<>jconst then
		return 0
	fi
	a:=p.value
	x:=p.xvalue

	case pr(s,    t)
	when pr(ti64, tr64), pr(ti64, tr32) then
		z:=a

	when pr(tr64, ti64) then
		c:=x

	when pr(tr64, tr32) then
!		z:=real32(x)
!		z:=real32(x)
		Z:=X

!	when pr(ti64, tu64), pr(tu64, ti64), pr(tc64, ti64) then
!		c:=p.value
	when pr(ti64, tu8) then
		c:=byte(a)
	when pr(ti64, ti16) then
		c:=i16(a)

	else
		if ttisinteger[s] and ttisinteger[t] and ttsize[s]=ttsize[t] then
			c:=a
		else
			sbase:=ttbasetype[s]
			tbase:=ttbasetype[t]
			if sbase=tbase then return 1 fi
			return 0
		fi
	esac

	if ttisreal[t] then
		makenewconst(p,int64@(z),t)

	else
		makenewconst(p,c,t)
	fi

	return 1
end

proc tx_assign(unit p,a,b,int t)=
	int m,mm,needres:=t<>tvoid
	symbol d

	case a.tag
	when jmakelist then
		if b.tag=jmakelist then
			if needres then txerror("Mult assign has no result") fi
			tx_assignmultmult(p,a,b)
		else
			tx_assignmultscalar(p,a,b,t)
		fi
		return
	when jdotindex, jdotslice then
		tx_dotindex(a,a.a,a.b,needlv)
		tpass(b,a.mode)
		p.mode:=ti64
		return
	esac

	if a.tag=jname and a.def.islet and p.initlet then
		tpass(a)
	else
		tpasslv(a)
	fi
	m:=a.mode

	a.resultflag:=needres

	if ttbasetype[m]=tslice and b.tag=jmakelist then
!CPL "ASSIGN/SLICE"
		tx_makeslicefromlist(b,b.a,m)
		p.mode:=m

!	elsif ttisshort[m] and needres then
!		p.memmode:=m
!		p.mode:=gettypebase(m)
!		tpass(b,p.mode)

	else
		if b.pclop in [kidiv, kirem] then		!CAN'T JUST OVERRIDE MODE
			tpass(b)
		elsif b.tag=jread then
			tpass(b,m)
		else
			mm:=m
			if ttisshort[m] then
				mm:=gettypebase(m)
			fi
			case b.tag
			when jautocast then
				tpass(b,mm)
			when jmakelist then
				tpass(b,m)
			else
				tpass(b,mm)
			esac
			p.mode:=mm

		fi
	fi
end

proc tx_assignmultmult(unit pp,a,b)=
!mult:=mult
	unit p,q,lhs,rhs

	pp.tag:=jassignmm

	if a.length<>b.length then
		txerror("Mult assign: count mismatch")
	fi
	if a.length=0 then
		txerror("Invalid assignment")
	fi
	rhs:=b.a
	lhs:=a.a

	p:=lhs
	while p, p:=p.nextunit do
		tpasslv(p)
	od

	p:=lhs

	q:=rhs
	while q, (p:=p.nextunit; q:=q.nextunit) do
		tpass(q,p.mode)
	od
end

proc tx_assignmultscalar(unit pp,a,b,int t)=
!assign 'scalar' to mult LHS, but it might be a tuple type or be an expandable one
	unit p,q, alist:=a.a
	int nretmodes,i, alength:=a.length
	ref[]int32 pmult
	symbol d				!point to def containing return mode info

	nretmodes:=0
	pp.tag:=jassignms

	tpass(b,tany)

	case ttbasetype[b.mode]
	when ttuple then
		d:=getprocretmodes(b)
		nretmodes:=d.nretvalues

		if ttbasetype[d.mode]<>ttuple then txerror("Not a tuple") fi

		if alength>nretmodes then
			txerror("mult ass/mult returns don't agree in number")
		fi
		if nretmodes<=1 then
			txerror("mult ass rhs needs fn yielding 2+ values")
		fi

		p:=alist
		pmult:=ttmult[d.mode]
		i:=1

		while p, p:=p.nextunit do
			tpasslv(p,pmult[i++])
		od
	when tslice then
		if alength<>2 then txerror("(a,b):=slice") fi
		tpasslv(alist,createrefmode(nil, tttarget[b.mode]))
		tpasslv(alist.nextunit,ti64)

	when trange then
	when trecord then

	elsif b.tag=jbin and b.pclop=kidivrem then
		if alength<>2 then txerror("(a,b):=divrem") fi
		tpasslv(alist,b.mode)
		tpasslv(alist.nextunit,b.mode)
		pp.tag:=jassignmdrem

	else
		txerror_s("Can't expand to mult values:",strmode(b.mode))
	esac

	pp.mode:=t
end

proc tpasslv(unit p, int t=tany)=
!process p as lvalue, but require it to be of type t
!however no conversion is done (not allowed); only a compare is done
	tpass(p,,needlv)
	if t not in [tany, tvoid] then
		if not comparemodes(p.mode, t) then
			txerror_ss("PassLV type mismatch: #:=#",strmode(p.mode), strmode2(t))
		fi
	fi
end

func dobinnumx(unit p,a,b)int=
!Try and apply this to binary operands:
!	NUMX	NUMX	DOM
!a and b have already been processed, but not coerced to any type yet

	int amode:=a.mode, bmode:=b.mode, cmode

	if isnum(amode) and isnum(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi

	if isnum(amode) and isbool(bmode) then
		p.mode:=amode
		coerceunit(b,amode)
		return 1
	elsif isbool(amode) and isnum(bmode) then
		p.mode:=bmode
		coerceunit(a,bmode)
		return 1
	fi


	return 0
end

func dobinnumf(unit p,a,b)int=
!Try and apply this to binary operands:
!	NUMF	NUMF	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

!	if amode=ti64 then coerceunit(a, tr64); amode:=tr64 fi
!	if bmode=ti64 then coerceunit(b, tr64); bmode:=tr64 fi

	if isnumf(amode) and isnumf(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

func dobinnumi(unit p,a,b)int=
!Try and apply this to binary operands:
!	NUMI	NUMI	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

	if isnumi(amode) and isnumi(bmode) then
		p.mode:=cmode:=min(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

func tx_in(unit p,a,b)int=
	int simpleset, amode, bmode
	unit q

	tpass(a)
	tpass(b)
	amode:=a.mode
	bmode:=b.mode

	coerceunit(a,ti64)

	simpleset:=1
	if b.tag=jmakeset then
		q:=b.a
		while q, q:=q.nextunit do
			if not ttisinteger[q.mode] then
				simpleset:=0
				exit
			fi
		od
	fi

	if isnum(a.mode) and b.tag in [jmakerange, jmakeset] and simpleset then
		p.tag:=(b.tag=jmakerange|jinrange|jinset)
	else
		txerror("doin")
	fi
	p.mode:=tbool

!	if p.pclop=kknotin then
	if p.inv then
		addnotl(p)
	fi
	return 1
end
=== mm_types.m 0 1 40/41 ===
global enumdata  [0:]ichar stdnames,
        [0:]byte stdsize,
        [0:]byte stdpcl =

!    type         name       bits     pcl
    (tvoid=0,     "void",       0,    tpvoid),

    (tr64,        "r64",        8,    tpr64),
    (tr32,        "r32",        4,    tpr32),
    (ti64,        "i64",        8,    tpi64),
    (tu64,        "u64",        8,    tpu64),
    (tc64,        "c64",        8,    tpu64),

    (tbool64,     "bool64",     8,    tu64),

    (tref,        "ref",        8,    tpu64),
    (trecord,     "rec",        0,    tpblock),
    (trange,      "range",     16,    tpblock),

    (tarray,      "array",       0,   tpblock),
    (tslice,      "slice",      16,   tpblock),

    (tc8,         "c8",          1,   tpu8),
    (tbool8,      "b8",          1,   tpu8),
    (ti8,         "i8",          1,   tpi8),
    (ti16,        "i16",         2,   tpi16),
    (ti32,        "i32",         4,   tpi32),
    (tu8,         "u8",          1,   tpu8),
    (tu16,        "u16",         2,   tpu16),
    (tu32,        "u32",         4,   tpu32),

    (trefchar,    "ichar",       8,   tpu64),
    (trefbit,     "refbit",     16,   tpu64),

    (tauto,       "auto",        0,   tpu64),
    (tany,        "any",         0,   tpu64),
    (tproc,       "proc",        0,   tpu64),
    (tlabel,      "label",       0,   tpu64),
    (ttype,       "type",        8,   tpu64),
    (tbitfield,   "bitfl",       8,   tpu64),
    (ttuple,      "tuple",       0,   tpu64),
    (tpending,    "pend",        0,   tpu64),
    (tblock,      "block",       8,   tpblock),

    (tlast,       "last ",       0,   tpvoid),
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tbool	= tbool64

!global const tfirstnum	= tc64
!global const tlastnum	= tr64

global const tfirstnum	= tr64
global const tlastnum	= tc64

global const tfirstshort	= tc8
global const tlastshort		= tu32

global const maxtuplesize = 4

global int trefproc
global int treflabel
=== mm_help.txt 0 1 41/41 ===
M Compiler Generating x64 native code - Windows Version

Whole-program compiler builds entire program from the lead module
into a executable file.

    mm main              # Create main.exe from lead module main.m
    mm main.m            # Same (.m extension is default)

Options:

    -exe                 # Generate .exe executable file (default)
    -dll                 # Generate .dll library (and _lib.m or .q import module)
    -obj                 # Generate single .obj file
    -asm                 # Generate intermediate ASM file only
    -ml                  # Generate .ml library file
    -mx                  # Generate .mx executable
    -run                 # Run program in-memory
    -ma                  # Create .ma file combining source/support files

    -opt                 # Apply simple optimiser

    -out:file            # Name of output file 



=== END ===
1 mm.m
2 pcl.m
3 pc_api.m
4 pc_decls.m
5 pc_diags.m
6 pc_run.m
7 pc_runaux.m
8 pc_tables.m
9 mc_genmcl.m
10 mc_auxmcl.m
11 mc_libmcl.m
12 mc_stackmcl.m
13 mc_genss.m
14 mc_decls.m
15 mc_objdecls.m
16 mc_writeasm.m
17 mc_writeexe.m
18 mc_writeobj.m
19 mx_decls.m
20 mx_run.m
21 mx_lib.m
22 mx_write.m
23 mm_cli.m
24 mm_blockpcl.m
25 mm_assem.m
26 mm_decls.m
27 mm_diags.m
28 mm_exportq.m
29 mm_genpcl.m
30 mm_lex.m
31 mm_lib.m
32 mm_libpcl.m
33 mm_libsources_dummy.m
34 mm_modules.m
35 mm_name.m
36 mm_parse.m
37 mm_support.m
38 mm_tables.m
39 mm_type.m
40 mm_types.m
41 mm_help.txt
