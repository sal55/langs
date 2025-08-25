=== MA 79 ===
=== ccp.m 0 0 1/79 ===
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

!Bundled headers

    module cc_headers
!   module cc_headersx

!Diagnostics
!    module cc_show
    module cc_showdummy

!IL Backend
    $sourcepath "c:/mx/"
    import pclp
!    import pclint
end
=== pclp.m 0 0 2/79 ===
project =
	module pc_api
	module pc_decls

!	module pc_diags
	module pc_diags_dummy
	module pc_reduce

! Interpreter
	module pc_run
	module pc_runaux

! Tables (eg. types and IL opcodes)
	module pc_tables

!	module pc_genc
!	module pc_auxc
!	module pc_libc

	module mc_GenMCL
	module mc_AuxMCL
	module mc_LibMCL
	module mc_StackMCL
	module mc_Optim

	module mc_GenSS

	module mc_Decls as md
	module mc_OBJdecls
	module mc_WriteASM
!	module mc_WriteASM_small
!	module mc_WriteNASM

	module mc_WriteEXE
	module mc_WriteOBJ

!	module mc_writess
!	module mc_disasm
	module mc_writess_dummy

	module mx_decls
	module mx_run
	module mx_lib
	module mx_write

end

export byte pc_userunpcl=0
=== pc_api.m 0 0 3/79 ===
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

global ichar longstring					!used in stropnd
global int longstringlen

export int mlabelno
export byte phighmem
export byte pfullsys
global byte fpshortnames

export ref proc (ref void) idomcl_assem
export ref func (ref void)int icheckasmlabel
export ref func (int)psymbol igethostfn


!PROC START=
!!CPL "PCL API",PCLEXTRA[KSETARG]
!CPL "PCL API",PCLNAMES.LEN
!CPL "PCL API",PSTREC.BYTES
!END



export func pcl_start(ichar name=nil, int nunits=0)psymbol=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

!CPL =PSTREC.BYTES

	if pcldone then pclerror("PCL start?") fi

	if name then
		currprog:=pc_makesymbol(name, program_id)
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

	mlabelno:=0

	currprog

end

export proc pcl_end=
	if pccurr>=pccurr and pccurr.opcode<>kendprog then
		pc_gen(kendprog)
	fi	
	pcldone:=1
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

export func pcl_writepst(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writepst()

	if filename then
		if pverbose then println "Writing PST",filename fi
		writefile(filename, d.strptr, d.length)
		""
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

export func pcl_writess(ichar filename=nil, int obj=0)ichar =
	ref strbuffer ssstr
	filehandle f

	genmcl()
	genss(obj)

	ssstr:=writessdata(not obj)

	if filename then
		f:=fopen(filename,"w")
		gs_println(ssstr, f)
		fclose(f)

		gs_free(ssstr)
		nil
	else
		ssstr.strptr
	fi
end

export func pcl_writeasm(ichar filename=nil, int atype='AA')ichar=
	ref strbuffer asmstr
	filehandle f

!	if assemtype<>atype then
!		pclerror("Wrong ASM Module")
!	fi

	if assemtype='NASM' then
		phighmem:=2
	fi

	genmcl()

	asmstr:=getassemstr()

	if filename then
!CPL "WRITEASM/FILE"
		if pverbose then println "Writing", filename fi

		f:=fopen(filename,"w")
		gs_println(asmstr, f)
		fclose(f)

		gs_free(asmstr)
		nil
	else
!CPL "WRITEASM/STR"
		asmstr.strptr
	fi
end

export proc pcl_writeobj(ichar filename)=
!	phighmem:=2

	genmcl()
	genss(1)

PHIGHMEM:=0
CPL =PHIGHMEM

	int tt:=os_clock()
	writecoff(filename)
	objtime:=os_clock()-tt
end

export proc pcl_writedll(ichar filename)=
	phighmem:=2
	genmcl()
	genss()
	int tt:=os_clock()
	writeexe(filename, 1)
	exetime:=os_clock()-tt
end

export proc pcl_writeexe(ichar filename)=

!CPL "WX",$LINENO
	genmcl()
!CPL "WX",$LINENO

	genss()
!CPL "WX",$LINENO
	int tt:=os_clock()
!CPL "WX",$LINENO
	writeexe(filename, 0)
!CPL "WX",$LINENO
	exetime:=os_clock()-tt
end

export proc pcl_writemx(ichar filename)=
	genmcl()
	genss()
	writemcx(filename)
end

!export proc pcl_writeclang(ichar filename)=
!	writeclang(filename)
!end

export proc pcl_exec=
!	pcmdskip:=cmdskip
	genmcl()
	genss()
	runlibfile("dummy", pcmdskip)
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
	p.opndtype:=(mode=tpr64|real_opnd|r32_opnd)
	return p
end

export func genrealimm(real x, int mode=tpr64)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=(mode=tpr64|realimm_opnd|realimm32_opnd)
!CPL "GENREALIMM", OPNDNAMES[P.OPNDTYPE], STRPMODE(MODE)
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

export func gendata(ref byte s, int length)pcl p=
	p:=newpcl()
	p.svalue:=s			! assume already saved on heap
	p.opndtype:=data_opnd
	p.mode:=tpblock
	p.size:=length

	return p
end

export proc gencomment(ichar s)=
	return when fregoptim or fpeephole		!will get skipped anyway
!	RETURN WHEN DOREDUCE			!comments suppressed as they get in the way
!STATIC INT CCC
!CPL "COMMENT",++CCC
	pc_gen(kcomment,genpcstrimm(s))
end

export func genname(ichar s)pcl=
	return genmem(pc_makesymbol(s, misc_id))
end

export func gennameaddr(ichar s)pcl=
	return genmemaddr(pc_makesymbol(s, misc_id))
end

export func genassem(ref void code)pcl p=
	p:=newpcl()
	p.asmcode:=code
	p.opndtype:=assem_opnd
	return p
end

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

export func getbasename(ichar s)ichar t=
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

!	if fpshortnames then return d.name fi

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

!CPL "ADDPLIB",NAME

!	plibfiles[++nplibfiles]:=pcm_copyheapstring(name)
	plibfiles[++nplibfiles]:=pcm_copyheapstring(changeext(name,""))
end

export proc pc_defproc(psymbol d, int mode=tpvoid, isentry=0, threaded=0)=
	pclerror("Nested proc") when currfunc
	pc_gen((threaded|ktcproc|kproc), genmem(d))
	if mode=tpvoid then mode:=d.mode fi
	pc_setmode(mode)

!THIS .PCADDR is a dummy value; any non-nil value shows d is already defined
!A proper .pcaddr is filled in during runp fixup
	if d.pcaddr then pclerror(addstr("Dupl proc:",d.name)) fi
	d.pcaddr:=pccurr

	if entryproc=nil and isentry then
		entryproc:=d
		d.isentry:=1
	fi

	currfunc:=d
end

export proc pc_setimport(psymbol d)=
!allow the use of pc_addlocal
!use d=nil when done

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
	if d.owner=nil then d.owner:=currfunc fi
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
	if d.owner=nil then d.owner:=currfunc fi
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
		lineno:=igetmsourceinfo(mmpos, filename, sourceline)
		CPL =LINENO
		CPL =FILENAME
	else
		lineno:=0
		filename:="?"
	fi

	if currfunc then
		println "Proc:", currfunc.name
	fi

	fprintln "MCL Error: # (#) on Line: # in #, PCL:#",mess,param, lineno, filename,ppseqno

	pcerrorstop(filename, lineno)
end

export func pc_duplpst(psymbol d)psymbol e=
	e:=pcm_allocnfz(pstrec.bytes)
	e^:=d^
!*!	e.generic:=d			!assume d is the original
	e.seqno:=++stseqno

	e.next:=nil
	e
end

export proc pcl_cmdskip(int cmdskip, psymbol dcmdskip=nil)=
	pcmdskip:=cmdskip
end

export func convertstring(ichar s, t)int=
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
			t++^:='r'
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

=== pc_decls.m 0 0 4/79 ===
!decls

export type psymbol = ref pstrec

global record pstrec = $caligned
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

	ref procinforec info		!procs: info to help codegen	

	union
		pcl pcaddr				!for procs: entry point to function
		ref proc dlladdr		!for imported functions
		ivoid staddr			!var statics: address
		psymbol cprocowner		!C target:var statics: owner proc
	end
	ref fwdrec fwdrefs			!fwd ref chain

	byte id
	byte ksymbol				!0, or _rw code for reserved words (used by pp)
	byte opcode					!for opcode_rw
	byte subcode				!for jumpcc_rw/setcc_rw/type_rw

	i32 offset

	byte imported				!only for import_id
	byte exported				!only for proc_id/static_id
	byte mode
	byte isentry
	u32 size

	byte addrof
	byte nrefs
	byte reg
	byte atvar
	byte used					!0, or 1 to 255 (capped at 255)
	byte reftype
	byte segment
	byte hasdot

	i16 stindex
	i16 importindex
	i32 labelno

	byte flags:(asmused:1, chasstatics:1)

!	byte asmused				!1 when proc contains asmcode
	byte dllindex				!for dllproc: which dll in dlltable

	byte nretvalues				!function: number of return values (0 for proc)
	byte varparams				!0 or N; variadic params

	byte isthreaded				!
	byte ishandler				!1 if a proc to be part of func tables
	byte ismain					!1 if a proc to be part of func tables
	byte SPARE

!----------------------------------

	byte nparams
	byte variadic				!local function that uses ... variadic params (for C)
	i16 nlocals
	i16 impindex
	i16 expindex
	u32 seqno

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

			end
		end
	end

	u32 pos:(sourceoffset:24, fileno:8)
	i32 dummy:(mode2:8, seqno:24)
end

export record fwdrec =
	ref fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

export type procinfo = ref procinforec

export record procinforec =
	byte pcldepth			!max pcl opnd depth (will include high-args)
	byte nparams			!copy of info in pstrec; may exclude not used?
	byte nlocals
	byte isleaf				!1 when there are no calls (explicit or implicit)
	byte nmaxargs			!0, or maxargs of any call (may be capped at 4)
	byte assemused			!1 if inline assembly used (can't optimise)
	byte mcldone			!
	byte hasblocks			!whether block modes are used (that means copyblock etc)
end

global procinfo pinfo

!global int frameoffset
!global int paramoffset
!global int framebytes

global const maxparams=32
global const maxlocals=256


!global int usedparams			!how many of pregs needed for used incoming params
!global int usedxparams			!how many of pxregs needed for used incoming params
!
!global int highreg				!highest D-reg used
!global int highxreg				!highest X-reg used
global int bspill, bxspill		!no. to spill
!global int bxspilloffset		!base frame offset of bxreg spill area

global byte r10used				!these may be set in pass2 when occupied by params
global byte r11used

!global int maxregvars			!how many locals would qualify for regvars
!global int maxxregvars			!how many locals would qualify for xregvars

global byte localshadow			!1 if local, proc-wide shadow space used for a call

export int mmpos

global psymbol psymboltable, psymboltablex

global psymbol currprog
export psymbol currfunc
global psymbol blockretname
global psymbol entryproc		!entry point function

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer

global const maxplibfile=50
global [maxplibfile]ichar plibfiles
global [maxplibfile]u64 plibinst
global int nplibfiles

export ref func (int pos, ichar &filename, &sourceline)int igetmsourceinfo

global byte pcldone, mcldone, ssdone, objdone, exedone

export byte pverbose
global int pcmdskip
!global psymbol pdcmdskip			!reference to $cmdskip symbol

export int assemtype='AA'

GLOBAL INT PPSEQNO

!!GLOBAL CONST DOREDUCE=1
!GLOBAL CONST DOREDUCE=0

GLOBAL CONST REDUCELABELS=1
!GLOBAL CONST REDUCELABELS=0

export byte fpeephole = 1
export byte fregoptim = 1 

export int mcltime, sstime, objtime, exetime

EXPORT ICHAR $PMODULENAME

EXPORT [PCLNAMES.BOUNDS]INT PCLFLAGS

EXPORT INT PSTARTCLOCK

!export const ctarget=0
=== pc_diags_dummy.m 0 0 5/79 ===
global proc pshowlogfile=
end

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

!global func stropndstack(int indent=0)ichar=
!	return nil
!end
!
!global proc showopndstack=
!end

=== pc_reduce.m 0 0 6/79 ===
export proc pcl_reducetest=
	int nn, seqno, lab, lab2, nargs
	pcl pc, newpc, pcnext, pcnext2, pcproc
	ref[]u16 labelmap
	psymbol pdef
	[maxcalldepth]pcl callstack
	int ncall
	int nprocs:=0, nleaf:=0, nallparams:=0, nalllocals:=0, offset

	nn:=pccurr-pcstart+1

!CPL =REDUCELABELS
	goto skip unless reducelabels

	pc:=pcstart
	labelmap:=pcm_allocz(mlabelno*u16.bytes)

	while pc<=pccurr, ++pc do
		case pc.opcode
		when klabel then				!don't include these
!			labelpclmap[pc.labelno]:=pc			!map labelno to pcl instr
		when kassem then
			if icheckasmlabel then
				lab:=icheckasmlabel(pc.asmcode)		!non-zero means a label number from ast
				if lab then
					++labelmap[lab]
				fi
			fi
!
		else
			if pc.opndtype=label_opnd then
				++labelmap[pc.labelno]
			fi
		esac
	od

skip:
	pc:=pcstart
	newpc:=pcstart-1			!point to last copied instr (none to start)
	seqno:=0

	to nn do
		pcnext:=pc+1

		if nargs:=pclargs[pc.opcode] then
			if nargs=9 then			!explicit calls
				nargs:=pc.nargs
			fi
			pinfo.nmaxargs := max(pinfo.nmaxargs, nargs)
		fi

!		if pc.mode=tpblock and pinfo then pinfo.hasblocks:=1 fi
		if pc.mode=tpblock and pinfo and pc.size<>16 then pinfo.hasblocks:=1 fi

		case pc.opcode
		when kcomment then

		when klabel then
			if not reducelabels then recase else fi
			if labelmap[pc.labelno] then
				recase else
			fi			!else skipped

		when kproc,ktcproc then
			++nprocs
			pdef:=pc.def
			pdef.pcaddr:=newpc+1
			pinfo:=pcm_alloc(procinforec.bytes)
			pdef.info:=pinfo
			pinfo.isleaf:=1
			pinfo.nparams:=pdef.nparams
			pinfo.nlocals:=pdef.nlocals
			nallparams+:=pdef.nparams
			nalllocals+:=pdef.nlocals
			ncall:=0
			pcproc:=newpc+1

			recase else

		when kcallp, kcallf, kicallp, kicallf then
			pinfo.isleaf:=0
			--ncall
			recase else

		when ksetcall then
!			if ncall then
!				for i:=ncall downto 1 do
!					callstack[i].simple:=0
!				od
!			else
!				pc.simple:=1
!			fi
			++newpc
			newpc^:=pc^
			newpc.seqno:=++seqno
			callstack[++ncall]:=newpc

		when kendproc then
			if pinfo.isleaf then ++nleaf fi
			pinfo:=nil

			recase else

		when kassem then
			pinfo.assemused:=1
			recase else

		when kiload, kistore then
			if newpc^.opcode=kaddpx and pc.mode<>tpblock then
				newpc.mode:=pc.mode
				newpc.opcode:=(pc.opcode=kiload|kiloadx|kistorex)
			else
				recase else
			fi

		when kwiden then
			if pcnext.opcode=ktruncate and pc.mode2=pcnext.mode2 then
				++pc				!widen and truncate cancel out
			elsif pcnext.opcode in [kjumpf, kjumpt] then
				pcnext.mode:=pc.mode2			!widen t/u; jumpf t -> jumpf u

			else
				recase else
			fi
		
		when ktruncate then
			if newpc.opcode in [kload, kiload, kiloadx] and newpc.mode=pc.mode2 then
					!truncating to same width (widen has been removed)
			else
				recase else
			fi

		when kload then				!addpx/load imm/addpx -> single addpx
			if pc.opndtype=int_opnd and pcnext.opcode=newpc.opcode=kaddpx then
				newpc.extra +:= pc.value*pcnext.scale+pcnext.extra
				++pc					!skip this load and following addpx
			elsif pcnext.opcode=kunload then
				++pc					!skip load/unload
			else
				recase else
			fi

!		when kaddpx then
!			if newpc.opcode=kload and newpc.opndtype=int_opnd and pcnext.opcode=kload and
!				(pcnext+1).opcode=kaddpx then
!				pcnext2:=pcnext+1
!				pcnext2.extra+:=newpc.value*pc.scale+pc.extra
!				newpc^:=pcnext^				!skip loadimm and 1st addpx
!				pc:=pcnext2					!process next addpx next
!CPL "REDUCE: LOAD IMM/ADDPX/LOAD/ADDPX"
!!CPL "REDUCE: LOAD IMM/ADDPX/LOAD IMM/ADDPX"
!			fi
!
!			recase else

		else
			++newpc
			newpc^:=pc^
			newpc.seqno:=++seqno

			if newpc.opndtype=memaddr_opnd then
				unless newpc.opcode=kload and newpc.inplace then
					newpc.def.addrof:=1
				end
			fi

		esac
!skip:
		++pc
	od

	pccurr:=newpc
	pcm_free(labelmap, mlabelno) when reducelabels
end

=== pc_run.m 0 0 7/79 ===
!PCL Interpreter

!int dotrace=1
int dotrace=0


int dostep=1
!int dostep=0
int go

INT SEQNO

!INT ALLCALLS
!INT LEAFCALLS

pstrec emptyst

CONST INDENT="  "

const dostackcheck = 1
!const dostackcheck = 0

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

global ref[]pcl labeltable

global macro pcerror(a) = pcerrorx(pc, a)
global macro pcerror2(a,b) = pcerrorx(pc, a,b)

global func dispatch_loop(pcl pcentry, int cmain=0)int=
!	const stacksize = 10'000
	const stacksize = 70'000
	const callstacksize = 10'000
	const pcmask=15

!HEAP VERSION
	ref[]int stack
		ref [stacksize]real		xstack @stack
		ref [stacksize]word		ustack @stack
		ref [stacksize]ref void	pstack @stack

	pcl pc:=pcentry
		ref byte pcb @pc
		u64 pci @pc

	int sp:=0
	int fp:=0
	int		a
		real	x	@a
		r32	sx	@a
		word	u	@a
	ref byte ptr
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

	int		b
		real	y	@b
		r32	sy	@b
		word	v	@b

!	int opc

	ref byte ptrb
	int n
	psymbol d

!STACK VERSION
!	[stacksize]int		stack
!		[stacksize]real		xstack @stack
!		[stacksize]word		ustack @stack
!		[stacksize]ref void	pstack @stack
!
	[callstacksize]u32 callstack
	[callstacksize]psymbol callstackst
	int callsp:=0
	[256]char str

	INT MAG, OLDSP

	macro zz = sp
	macro yy = sp-1
	macro xx = sp-2
	macro ww = sp-3

	stack:=pcm_alloc(stacksize*int.bytes)

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

	int lastpos:=0, count:=0

IF DOTRACE THEN
	IF NOT DOSTACKCHECK THEN
		ABORTPROGRAM("Need 'dostackcheck=1'")
	FI
	EMPTYST.NAME:="<Fn ptr>"
fi

!!========================================================================
!	do
!
!	if sp>(stacksize-100) then pcerror("Stack overflow") fi
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
!
!++count
!	CPL count:"3",=pc, pc.seqno:"z4", PCLNAMES[GETOPCODE]:"9JL", strpmode(pc.mode, pc.size), opndnames[pc.opndtype], =SP, =FP
!
!!	CPL pc.seqno:"z4", PCLNAMES[GETOPCODE]:"9JL", strpmode(pc.mode, pc.size), opndnames[pc.opndtype], =SP, =FP
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
!
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
!!========================================================================

!CPL "NEW PC RUN"
!
!
!	doswitch getopcode
	doswitchu getopcode
!	docase getopcode

	when knop      then
		steppc

	when kload     then
		if pc.opndtype=int_opnd then
			stack[++sp]:=pc.value
		else
			stack[++sp]:=pci_getopnd(pc, &stack[fp])
		fi
		steppc

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
			fi
			pci_storeptr(ptr, stack[sp--], getmode, getsize)

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
		pci_storeptr(ptr, stack[sp--], getmode, getsize)
		steppc

	when kdupl, kdouble   then
		a:=stack[sp]
		stack[++sp]:=a
		steppc

	when kswapstk  then
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
		if stack[xx] then			!set bit
			a.[stack[zz]]:=1
		else
			a.[stack[zz]]:=0
		fi

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
		if dotrace then

IF DOSTACKCHECK THEN
TO CALLSP DO PRINT INDENT OD
FI
!			fprintln "# Call:   # #", ++seqno, d.name, getlineno(pc),=SP,=FP
			fprintln "# Call:   # # SP=# FP=#", ++seqno, d.name, getlineno(pc)
		fi
!++ALLCALLS
!
!IF D.INFO AND D.INFO.ISLEAF THEN
!	++LEAFCALLS
!FI

		if dostackcheck then
			if sp>(stacksize-100) then pcerror("Stack overflow") fi
		fi

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
				callstackst[callsp]:=d
			fi

			stack[++sp]:=int(pc+1) ior getnargs
			pc:=d.pcaddr

		fi

	when kretproc  then

!		if dotrace AND CALLSP IN XXX then
!IF DOSTACKCHECK THEN
!TO CALLSP-1 DO PRINT INDENT OD
!FI
!			fprintln "# Return: # #", seqno, callstackst[callsp].name, getlineno(pc),=SP, =FP
!		fi
!
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

			pci:=newpci iand inot pcmask

		else
			pc:=pstack[sp--]
			sp-:=pci iand pcmask
			pci iand:=inot pcmask

		fi

	when kretfn    then

!		if dotrace AND CALLSP IN XXX then
!IF DOSTACKCHECK THEN
!TO CALLSP-1 DO PRINT INDENT OD
!FI
!			fprintln "# Return: # #", seqno, callstackst[callsp].name, getlineno(pc),=SP,=FP
!		fi
!
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
			pci:=newpci iand inot pcmask
		else
			pc:=cast(stack[--sp])
			sp-:=pci iand pcmask
			stack[sp]:=a
			pci iand:=inot pcmask
		fi

	when kicallp   then
!		if dotrace then
!			fprintln "# Call:   <fn_ptr> #", ++seqno, getlineno(pc)
!		fi

		ptr:=pstack[sp]
IF PTR=NIL THEN PCERROR("ICALLP NIL PTR") FI


		if newpc>=pcstart and newpc<=pccurr then		!assume local
icallp:
			if dostackcheck then
				callstack[++callsp]:=SP-1-GETNARGS+(GETOPCODE=KICALLF|1|0)
				CALLSTACKST[CALLSP]:=&EMPTYST
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
!		if dotrace then
!			fprintln "# Call:   <fn_ptr> #", ++seqno, getlineno(pc)
!		fi
		ptr:=pstack[sp]
IF PTR=NIL THEN PCERROR("ICALLF NIL PTR") FI

		if newpc>=pcstart and newpc<=pccurr then		!assume local
			goto icallp
		else	
			n:=getnargs
			--sp
			sp-:=n-1
			a:=docalldll(nil, cast(ptr), cast(&stack[sp]), n, getnvars, getmode)
			stack[sp]:=a
			steppc
		fi


	when kjump     then
		pc:=getlabel

	when kijump    then
		pc:=pstack[sp--]

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

	when kswitch, kswitchu   then
		a:=stack[sp--]
		if a in getswmin..getswmax then
			pc:=getlabel+1+a-getswmin				!get to index swlabel op
			pc:=getlabel					!get dest label from there
		else
			pc:=getlabel2
		fi

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
		xstack[sp]:=sqrt(xstack[sp])
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
		if ispfloat(getmode) then
			xstack[sp-1]:=xstack[sp-1]**xstack[sp]
		else
			stack[sp-1]:=stack[sp-1]**stack[sp]
		fi
		--sp

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

	when kdivto, kidivto    then
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


	when kiremto   then
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]

		a:=a rem b

		pci_storeptr(ptr, a, getmode)
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
		ptr:=pstack[sp--]
		a:=pci_loadptr(ptr, getmode)
		b:=stack[sp--]

		if isfloat then
			a:=int@(min(real@(a),real@(b)))
		elsif issigned then
			a min:=b
		else
			u min:=v
		fi

		pci_storeptr(ptr, a, getmode)
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

	when kproc  then					!proc entry code
		stack[++sp]:=fp
		fp:=sp

		sp+:=getnlocals


		if dotrace then
IF DOSTACKCHECK THEN
TO CALLSP DO PRINT INDENT OD
FI
!			fprintln "# Enter:  #", seqno, pc.def.name
			fprintln "# Enter:  # SP=# FP=#", seqno, pc.def.name, SP,FP
		fi

!		if dostackcheck then
!			++sp
!			stack[sp]:=magic + SP-1
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
!		IF DOTRACE THEN
!			IF PC.OPNDTYPE=MEM_OPND THEN
!				fprintln "# Call:   # #", ++seqno, pc.def.name, getlineno(pc)
!			ELSE
!				fprintln "# Call:   <FN PTR> #", ++seqno, getlineno(pc)
!			FI
!		fi
		steppc

	when ksetarg   then
!		unimpl
		steppc

	when kloadall  then
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

	when kinitdswx then

	else
unimpl:
		println
CPL =GETOPCODE
		fprintln "Unimpl: # at seq: #", pclnames[getopcode], getseqno
		println
		stop 1
!	end doswitch
	end
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
		when kproc, ktcproc then
			p.def.pcaddr:=p
		esac
	od

	p:=pcstart
	while p<=pccurr, ++p do
		case p.opcode
		when kistatic, kzstatic then
			d:=p.def
			d.staddr:=pcm_allocz(p.size)
		esac
	od

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
			pdata+:=p.size

		when kproc then
			dproc:=d:=p.def
			e:=d.nextparam
			parambytes:=0
			while e, e:=e.nextparam do
				parambytes+:=8
				e.offset:=-(parambytes/8+1)
			od

			e:=d.nextlocal
			framebytes:=0
			while e, e:=e.nextlocal do
				e.offset:=framebytes/8+1
				framebytes+:=roundtoblock(E.size,8)
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
!	int tt:=os_clock()
	int stopcode

	loadlibs()

	fixuppcl()

!CPL "COMPILE TO PCL:", OS_CLOCK()-PSTARTCLOCK
	if entryproc=nil then
		pcerrorx(pcstart,"No 'main' entry point")
	fi

	docmdskip()

!CPL "RUNPCL", ENTRYPROC.PCADDR
!CPL "RUNPCL", ENTRYPROC.PCADDR.SEQNO
!CPL "RUNPCL", PCLNAMES[ENTRYPROC.PCADDR.OPCODE]
!CPL "RUNPCL", ENTRYPROC.PCADDR.DEF.NAME

	if pverbose then
		println "Run PCL:"
	fi

!CPL "START DISPATCH"

	stopcode:=dispatch_loop(entryproc.pcaddr, entryproc.nparams=2)

!CPL "All Calls: ",ALLCALLS:"12s,"
!CPL "Leaf Calls:",LEAFCALLS:"12s,"

	if pverbose then
		println "Stopped",stopcode
		println
	fi

	stop stopcode
end

func getlineno(pcl pc)int=
	ichar filename, sourceline

	if igetmsourceinfo then
		igetmsourceinfo(pc.pos, filename, sourceline)
	else
		0
	fi
end
=== pc_runaux.m 0 0 8/79 ===


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

	case p.opndtype
	when int_opnd then
		a:=p.value

	when mem_opnd then
		d:=p.def
		case d.id
		when static_id then
			pi64:=d.staddr
		else
			pi64:=locals+d.offset
			if d.mode=tpblock and d.id=param_id then pi64:=cast(pi64^) fi
		esac

		a:=pci_loadptr(ptr, p.mode)

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

	when real_opnd, r32_opnd then
		a:=int@(p.xvalue)

	when label_opnd then
		a:=cast(labeltable[p.labelno])

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

	if p=nil then pclerror("Null ptr access") fi

	switch mode
	when tpblock then
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

	if p=nil then pclerror("Null ptr access") fi

	case mode
	when tpblock then
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
		if eqstring(getbasename(d.name), "$cmdskip") then
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
=== pc_tables.m 0 0 9/79 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,

		[0:]byte psigned,

		[0:]byte pint,
		[0:]byte pfloat,

		[0:]byte pmin,						!promoted type when min width applies
		[0:]byte xxpiwrb =					!int/word/real/block

	(tpvoid=0,    "void",    	0,	0, 0,0,	tpvoid,		tpvoid),

	(tpr32,       "r32",    	4,	0, 0,1,	tpr32,		tpr32),
	(tpr64,       "r64",    	8,	0, 0,1,	tpr64,		tpr64),

	(tpu8,        "u8",      	1,	0, 1,0,	tpu32,		tpu64),
	(tpu16,       "u16",    	2,	0, 1,0,	tpu32,		tpu64),
	(tpu32,       "u32",    	4,	0, 1,0,	tpu32,		tpu64),
	(tpu64,       "u64",    	8,	0, 1,0,	tpu64,		tpu64),

	(tpi8,        "i8",      	1,	1, 1,0,	tpi32,		tpi64),
	(tpi16,       "i16",    	2,	1, 1,0,	tpi32,		tpi64),
	(tpi32,       "i32",    	4,	1, 1,0,	tpi32,		tpi64),
	(tpi64,       "i64",    	8,	1, 1,0,	tpi64,		tpi64),

	(tpblock,     "mem",   		0,	0, 0,0,	tpblock,	tpvoid),
	(tpvector,    "vec",   		0,	0, 0,0,	tpvector,	tpvoid),

	(tplast,      "$last",   	0,	0, 0,0,	0,			0),


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
	(r32_opnd,		$),
	(string_opnd,		$),
	(strimm_opnd,		$),
	(assem_opnd,		$),
	(realimm_opnd,		$),
	(realimm32_opnd,	$),
	(data_opnd,			$),
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
				[0:]byte pclextra,
				[0:]byte pclhasopnd,
				[0:]byte pclargs =

!                       t  x op args    (a  b)
	(knop=0,       $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) ?

	(kload,        $+1, 1, 1, A, 0),  ! (0 - 1) (M L t i   ) Z' := M &M L &L 123 4.5 "abc"; i=1 for in-place ref
	(kiload,       $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := Z^
	(kiloadx,      $+1, 1, 2, 0, 0),  ! (2 - 1) (t d       ) Z' := (Y + Z*s + d)^

	(kstore,       $+1, 1, 0, M, 0),  ! (1 - 0) (M t       ) M := Z
	(kistore,      $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ := Y
	(kistorex,     $+1, 1, 2, 0, 0),  ! (3 - 0) (t s d     ) (Y + Z*s + d)^ := X
	(kstorem,      $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' :=(Y, Z) for mem:16

	(kdupl,        $+1, 0, 0, 0, 0),  ! (1 - 2) (          ) Z' := Y' := Z
	(kdouble,      $+1, 0, 0, 0, 0),  ! (1 - 2) (          ) Count extra instance of Z
	(kswapstk,     $+1, 0, 2, 0, 0),  ! (2 - 2) (a b       ) Swap(stack(a, 0), stack(b)); 1/2/3/4 = Z/Y/X/W
	(kunload,      $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) Pop stack

	(kopnd,        $+1, 1, 0, A, 0),  ! (0 - 0) (M L C t   ) Define auxiliary operand M or L
	(ktype,        $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) Define auxiliary type t

	(kloadbit,     $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := Y.[Z]
	(kloadbf,      $+1, 1, 0, 0, 2),  ! (3 - 1) (t         ) Z' := X.[Y..Z]
	(kstorebit,    $+1, 1, 0, 0, 2),  ! (3 - 0) (t         ) Y^.[Z] := X
	(kstorebf,     $+1, 1, 0, 0, 2),  ! (4 - 0) (t         ) X^.[Y..Z] := W

	(kcallp,       $+1, 0, 2,MA, 9),  ! (n - 0) (M n v     ) Call &M with nargs, then pop args; v = varargs
	(kicallp,      $+1, 0, 2, 0, 9),  ! (n - 0) (n v       ) Call Z with nargs, then pop args (a=n+1)
	(kretproc,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Return from proc
	(kcallf,       $+1, 1, 2,MA, 9),  ! (n - 1) (M t n v   ) Call &M, then pop args, leave retval; v = varrgs
	(kicallf,      $+1, 1, 2, 0, 9),  ! (n - 1) (t n v     ) Call Z, then pops args, leave retval (a=n+1)
	(kretfn,       $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) Return from func with Z=retval

	(kjump,        $+1, 0, 0, L, 0),  ! (0 - 0) (L         ) goto L
	(kijump,       $+1, 1, 0, 0, 0),  ! (1 - 0) (          ) goto Z
	(kjumpcc,      $+1, 1, 1, L, 0),  ! (2 - n) (L t c p   ) goto L when Y c Z; p=1: Z':=Y (b=0/1)
	(kjumpt,       $+1, 1, 0, L, 0),  ! (1 - 0) (L t       ) goto L when Z is true
	(kjumpf,       $+1, 1, 0, L, 0),  ! (1 - 0) (L t       ) goto L when Z is false
	(kjumpret,     $+1, 1, 0, L, 0),  ! (1 - 0) (L t       ) goto L, common return point; deal with any ret value on stack
	(kjumpretm,    $+1, 1, 0, L, 0),  ! (a - 0) (L t n     ) goto L, common return point; deal with any ret value on stack

	(ksetcc,       $+1, 1, 0, 0, 0),  ! (2 - 1) (t c       ) Z' := Y cc Z

	(kstop,        $+1, 0, 0, 0, 0),  ! (1 - 0) (          ) Stop Z

	(kto,          $+1, 1, 0, L, 0),  ! (0 - 0) (L t       ) --B (aux); goto L when B<>0 
	(kforup,       $+1, 1, 1, L, 0),  ! (0 - 0) (L t n     ) B+:=n; goto L when B<=C
	(kfordown,     $+1, 1, 1, L, 0),  ! (0 - 0) (L t n     ) B-:=n; goto L when B>=C

	(kiswap,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) swap(Y^,Z^)

	(kswitch,      $+1, 1, 2, L, 0),  ! (1 - 0) (L t x y   ) L=jumptab; B=elselab; x/y=min/max values
	(kswitchu,     $+1, 0, 2, L, 0),  ! (1 - 0) (L x y     ) L=jumptab; B=elselab; x/y=min/max values
	(kswlabel,     $+1, 0, 0, L, 0),  ! (0 - 0) (L         ) jumptable entry
	(kendsw,       $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Mark end of switch jumptable

	(kclear,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) Clear Z^

	(kassem,       $+1, 0, 0, A, 0),  ! (0 - 0) (x         ) To be worked out....

	(kadd,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y + Z

	(ksub,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y - Z
	(kmul,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y * Z
	(kdiv,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y / Z
	(kidiv,        $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y % Z
	(kirem,        $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y rem Z
	(kidivrem,     $+1, 1, 0, 0, 0),  ! (2 - 2) (t         ) Z' := divrem(Y, Z)
	(kbitand,      $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y iand Z
	(kbitor,       $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y ior Z
	(kbitxor,      $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y ixor Z
	(kshl,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y << Z
	(kshr,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := Y >> Z
	(kmin,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := min(Y, Z)
	(kmax,         $+1, 1, 0, 0, 0),  ! (2 - 1) (t         ) Z' := max(Y, Z)
	(kaddpx,       $+1, 1, 2, 0, 0),  ! (2 - 1) (t s d     ) Z' := Y + Z*s + d
	(ksubpx,       $+1, 1, 2, 0, 0),  ! (2 - 1) (t s d     ) Z' := Y - Z*s + s
	(ksubp,        $+1, 1, 1, 0, 0),  ! (2 - 1) (t s       ) Z' := (Y - Z)/s

	(kneg,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := -Z
	(kabs,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := abs Z
	(kbitnot,      $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := inot Z
	(knot,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := not Z
	(ktoboolt,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := istrue Z; u is of type u; result is type t
	(ktoboolf,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := not istrue Z
	(ksqr,         $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := sqr Z

	(ksqrt,        $+1, 1, 0, 0, 0),  ! (1 - 1) (t         ) Z' := sqrt Z
	(ksin,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := sin Z
	(kcos,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := cos Z
	(ktan,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := tan Z
	(kasin,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := asin Z
	(kacos,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := acos Z
	(katan,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := atan Z
	(klog,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := log Z
	(klog10,       $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := log10 Z
	(kexp,         $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := round Z
	(kround,       $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := round Z
	(kfloor,       $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := floor Z
	(kceil,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := ceil Z
	(ksign,        $+1, 1, 0, 0, 1),  ! (1 - 1) (t         ) Z' := sign Z

	(katan2,       $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := atan2(Y, Z)
	(kpower,       $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := Y ** Z
	(kfmod,        $+1, 1, 0, 0, 2),  ! (2 - 1) (t         ) Z' := fmod(Y, Z)

	(kincrto,      $+1, 1, 1, 0, 0),  ! (1 - 0) (t n       ) Z^ +:= n
	(kdecrto,      $+1, 1, 1, 0, 0),  ! (1 - 0) (t n       ) Z^ -:= n
	(kincrload,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := (Z +:= n)^
	(kdecrload,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := (Z -:= n)^
	(kloadincr,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := Z++^ (difficult to express step)
	(kloaddecr,    $+1, 1, 1, 0, 0),  ! (1 - 1) (t n       ) Z' := Z--^

	(kaddto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ +:= Y
	(ksubto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ -:= Y
	(kmulto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ *:= Y
	(kdivto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ /:= Y
	(kidivto,      $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ %:= Y
	(kiremto,      $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ rem:= Y
	(kbitandto,    $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ iand:= Y
	(kbitorto,     $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ ior:= Y
	(kbitxorto,    $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ ixor:= Y
	(kshlto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ <<:= Y
	(kshrto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ >>:= Y
	(kminto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ min:= Y
	(kmaxto,       $+1, 1, 0, 0, 0),  ! (2 - 0) (t         ) Z^ max:= Y
	(kaddpxto,     $+1, 1, 1, 0, 0),  ! (2 - 0) (t s       ) Z^ +:= Y*s
	(ksubpxto,     $+1, 1, 1, 0, 0),  ! (2 - 0) (t s       ) Z^ -:= Y*s

	(knegto,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) -:= Z^
	(kabsto,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) abs:= Z^
	(kbitnotto,    $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) inot-:= Z^
	(knotto,       $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) not:= Z^
	(ktoboolto,    $+1, 1, 0, 0, 0),  ! (1 - 0) (t         ) istrue:= Z^

	(ktypepun,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := t(u@(Z^))
	(kfloat,       $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Int   to real t
	(kfix,         $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Real   to int t
	(ktruncate,    $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,u) Mask to width of u, but type is widened to t
	(kwiden,       $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Mask to width of u, but type is widened to t
	(kfwiden,      $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r32 to r64
	(kfnarrow,     $+1, 2, 0, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r64 to r32

	(kstartmx,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) -
	(kresetmx,     $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) -
	(kendmx,       $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) -

	(kproc,        $+1, 0, 0, M, 0),  ! (0 - 0) (M         ) ?
	(ktcproc,      $+1, 0, 0, M, 0),  ! (0 - 0) (M         ) ?
	(kendproc,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) ?
	(kistatic,     $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define idata label (must be followed by correct DATA ops)
	(kzstatic,     $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define zdata label and reserve sufficient space
	(kdata,        $+1, 1, 0, A, 0),  ! (0 - 0) (M L C t   ) Constant data. For block types, there can be multiple C values
	(kinitdswx,    $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Following two ops initialise doswitchx jumptable

	(klabel,       $+1, 0, 0, L, 0),  ! (0 - 0) (          ) ?
	(klabeldef,    $+1, 0, 0,MA, 0),  ! (0 - 0) (          ) ?
	(ksetjmp,      $+1, 0, 0, 0, 0),  ! (1 - 0) (          ) For C
	(klongjmp,     $+1, 0, 0, 0, 0),  ! (1 - 1) (          ) For C

	(ksetcall,     $+1, 0, 1, 0, 0),  ! (0 - 0) (n s       ) n=args, s=1 for simple call

	(ksetarg,      $+1, 0, 2, 0, 0),  ! (0 - 0) (n1 n2     ) n1=arg no (LTR) n2=int or real arg no (maybe neg for real)
	(kloadall,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) ?

	(keval,        $+1, 0, 0, 0, 0),  ! (1 - 0) (          ) Evaluate Z [load to an actual register], then pop
	(kcomment,     $+1, 0, 0, 0, 0),  ! (0 - 0) (C         ) Comment C (a string)
	(kendprog,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) End-of-program marker.
!------------------------- -
!these are only used in textual PCL code

	(kparam,       $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define param
	(klocal,       $+1, 1, 0, M, 0),  ! (0 - 0) (M t       ) Define local
	(krettype,     $+1, 1, 0, 0, 0),  ! (0 - 0) (t         ) Define return type
	(kvariadic,    $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) Variadic C function
	(kaddlib,      $+1, 0, 0, S, 0),  ! (0 - 0) (S         ) Define import library
	(kextproc,     $+1, 0, 0, M, 0),  ! (0 - 0) (M         ) Define imported proc
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

=== mc_genmcl.m 0 0 10/79 ===
const fshowpcl=1
!const fshowopndstack=1
!const fshowpcl=0
const fshowopndstack=0

!global const docalltrace=1
!global const docalltrace=0

GLOBAL INT DEBUG

global int frameoffset, paramoffset
global int framebytes

[pclnames.bounds]ref proc(pcl) px_handlertable

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global proc genmcl(ichar dummy=nil)=

	return when mcldone

	IF FSHOWPCL OR FSHOWOPNDSTACK THEN CPL "********* ASM HAS PCL INFO *********" FI

!CPL =CURRFUNC

	int tt:=os_clock()
	inithandlers()
	mclinit()

	currpcl:=pcstart


	int i:=0
	repeat
		convertpcl(currpcl)

		showopndstack() when fshowopndstack and currpcl.opcode not in [klabel, kcomment, kproc, ktcproc, kretproc, kendproc]

		++currpcl

	until currpcl>pccurr or currpcl.opcode=kendprog

!CPL "DONE CONVERTPCL"

	genrealtable()
!CPL $LINENO
	genabsneg()
	genstringtable()
!CPL $LINENO

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)

!CPL $LINENO
	if fpeephole then
!CPL "SKIPPING PEEPHOLE"
		peephole()
	fi
!CPL $LINENO
!CPL "-----------------------", =MLABELNO


	mcldone:=1

	mcltime:=os_clock()-tt
!CPL $LINENO

end

!FUNC CHECKFPUSED(MCLOPND A)int=
!	RETURN 0 WHEN A=NIL
!	if a.reg=rframe or a.regix=rframe then return 1 fi
!	0
!END
!
proc convertpcl(pcl p)=

!RETURN WHEN P.OPCODE IN [KCOMMENT]
!CPL "    CONV",PCLNAMES[P.OPCODE]
	doshowpcl(p) when fshowpcl

!PCLFLAGS[P.OPCODE]++

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

	mccodex.regfreedpr.low ior:=oldregsetpr.low iand ((regsetpr.low ior isregvarpr.low) ixor invertbytes)
	mccodex.regfreedpr.high ior:=oldregsetpr.high iand ((regsetpr.high ior isregvarpr.high) ixor invertbytes)
!U64 A:=MC.REGFREEDPR.LOW


!CP "//",MC.REGFREEDPR.LOW:"Z16H","..."

!FOR R IN R0..R13 DO
!	IF OLDREGSET[R] AND NOT REGSET[R] AND NOT ISREGVAR[R] THEN
!!		MCCODEX.REGFREED[R]:=1
!		MC.REGFREED[R]:=1
!	FI
!OD
!FOR R IN R0..R13 DO
!IF MC.REGFREEDPR.LOW<>0 OR MC.REGFREEDPR.HIGH<>0 THEN CPL "REGFREED NOT ZERO"
!!MC.REGFREEDPR.LOW:"H"
! FI
!FOR R IN R0..r7 DO
!MC.REGFREED[R]:=0
!	IF OLDREGSET[R] AND NOT REGSET[R] AND NOT ISREGVAR[R] THEN
!		MC.REGFREED[R]:=1
!		MC.REGFREED[R]:=oldregset[r] iand ((regset[r] ior isregvar[r]) ixor 1)
!		MC.REGFREED[R] IOR:=oldregset[r] iand ((regset[r] ior isregvar[r]) ixor 1)
!		MC.REGFREED[R]:=oldregset[r] iand inot regset[r] iand inot isregvar[r]
!	FI
!OD
!U64 B:=MC.REGFREEDPR.LOW
!IF A<>B THEN
!CPL "MISMATCH",A,B
!FI
!CPL A,B,A=B
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
		(77, 88),
		(ktoboolf, 		ktoboolt),

		(kcallf,		kcallp),
		(kicallp,		kcallp),
		(kicallf,		kcallp),

		(kendmx,		kresetmx),
		(ktcproc,		kproc),

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

	return unless fshowpcl

	case p.opcode
	when kproc, ktcproc, kretproc, kendproc, kistatic, kzstatic, kdata then
	else
		strcpy(&.str,"                       ")
		strcat(&.str,strpclstr(p, str.len))
		mgencomment(PCM_COPYHEAPSTRING(&.str))
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
!*!	mgencomment("--")

	if currfunc.nparams=2 and currfunc.isentry then
		fixmain()
	fi
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
	when r32_opnd then
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

!	ax:=loadopnd(yy, p.mode)
!	bx:=getopnd(zz, p.mode)
!	genmc((ispfloat(p.mode)|m_addss+ispwide(p.mode)|m_add), ax, bx)

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

			bx:=getopnd(zz, p.mode)
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

	if n>4 then
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

!	dopreg:=currpreg:=0

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

proc px_jumpretm*(pcl p) =
! goto L, common return point; deal with any ret value on stack
	int n, reg

	n:=zz-p.nargs+1
	to p.nargs do
		loadopnd(n, pclmode[n])
		++n
	od

	reg:=r0
	to p.nargs do
		if reg=r3 and ispint(pclmode[zz]) then reg:=r10 fi
		movetoreg(reg)
		poppcl()
		++reg
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

	genmc(m_call, mgenextname("exit*"))

	localshadow:=1
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

	if pmode<>tpblock then

		if pclloc[zz]=regvar_loc then
			px:=mgenireg(pclreg[zz], pmode)
		else
			px:=getopnd_ind(zz, pmode)
		fi

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


	if pclloc[zz]=regvar_loc then
		px:=mgenireg(pclreg[zz], pmode)
	else
		px:=getopnd_ind(zz, pmode)
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

	setsegment('I')
end

proc px_switchu*(pcl p) =
! L=jumptab; B=elselab; x/y=min/max values
	int minlab, maxlab, jumplab, reg
	mclopnd ax, bx

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno

	ax:=loadopnd(zz, pmode)

	if phighmem=2 then
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
	clearreg(bx)
!	genmc(m_xorx, bx, bx)

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

proc px_loadall*(pcl p) =
	checkallloaded()
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

proc px_initdswx*(pcl p)=		!ignore for mcl/x64
end
=== mc_auxmcl.m 0 0 11/79 ===
!Auxially routines called by genmcl's PX handlers

INT NNN
GLOBAL INT NAUXPROCS
GLOBAL INT NAUXNOFRAME

ref mclrec mclframesetup

proc allocregvars(int skipparams, isleaf)=
!skipparams is 1 when func is main or variadic

	[4]psymbol params, xparams				!exinclude leaf procs
	[4]psymbol leafparams, xleafparams		!leaf procs only
	[4]byte leafparamno, xleafparamno			!index needed for leaf proc to get right preg
	[32]psymbol locals, xlocals
	int nparams:=0, nxparams:=0
	int nleafparams:=0, nxleafparams:=0
	int nlocals:=0, nxlocals:=0, n, reg, xreg
	int nl, np
	int nlx, npx
	psymbol d

	if maxregvars+maxxregvars=0 then		!disabled
		return
	fi

!note: only args 1-4 considered, even though some may be unsuitable, leaving a slot
!for arg 5+ to fill
	if not skipparams then
		d:=currfunc.nextparam
		n:=0
		while d, d:=d.nextparam do
			++n
			if d.used and not d.atvar and not d.addrof and n<=4 then
				if not isleaf then
					if pint[d.mode] then
						if nparams<4 then
							params[++nparams]:=d
						fi
					elsif pfloat[d.mode] then
						if nxparams<4 then
							xparams[++nxparams]:=d
						fi
					fi
				else						!leaf
					if pint[d.mode] then
						if nleafparams<4 then
							leafparams[++nleafparams]:=d
							leafparamno[nleafparams]:=n
						fi
					elsif pfloat[d.mode] then
						if nxleafparams<4 then
							xleafparams[++nxleafparams]:=d
							xleafparamno[nxleafparams]:=n
						fi
					fi
				fi
			fi
		od
	fi

	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do

		if d.used and not d.atvar and not d.addrof then
			if pint[d.mode] then
				if nlocals<locals.len then
					locals[++nlocals]:=d
				fi
			elsif pfloat[d.mode] and nxlocals<xlocals.len then
				xlocals[++nxlocals]:=d
			fi
		fi
	od

!now allocate regs
	if nlocals=0 then						!params only; no locals
		np:=min(maxregvars, nparams)		!(unlikely regvars is below 4)
		nl:=0
	elsif nparams=0 then					!locals only; no params, or is leaf
		nl:=min(maxregvars, nlocals)
		np:=0
	else									!both locals and params
		nl:=nlocals
		np:=nparams
		n:=np+nl-maxregvars					!n is excess number; neg is OK
		if n>0 then							!at least one short
			--np; --n						!lose one param
			if n>0 and np>0 then			!lose 2nd param
				--np; --n
			fi
			if n>0 then						!still short; lose excess from locals
				nl-:=n
			fi
		fi
	fi

!now allocate regvars

	reg:=r3
	for i to nl do
		d:=locals[i]
		d.reg:=reg
		isregvar[reg]:=1
		++reg
	od

!CPL =NL, =NP

	for i to np do
		d:=params[i]
		d.reg:=reg
		isregvar[reg]:=1
		++reg
	od

!do regvars for leaf procs
	for i to nleafparams do
		d:=leafparams[i]
		reg:=leafparamno[i]+r10-1
		if reg=r10 then r10used:=1 fi
		if reg=r11 then r11used:=1 fi
		d.reg:=reg
		isregvar[reg]:=1
	od

!now allocate xregs
	if nxlocals=0 then						!params only; no locals
		npx:=min(maxxregvars, nxparams)		!(unlikely regvars is below 4)
		nlx:=0
	elsif nxparams=0 then					!locals only; no params, or is leaf
		nlx:=min(maxxregvars, nxlocals)
		npx:=0
	else									!both locals and params
		nlx:=nxlocals
		npx:=nxparams
		n:=npx+nlx-maxregvars					!n is excess number; neg is OK
		if n>0 then							!at least one short
			--npx; --n						!lose one param
			if n>0 and npx>0 then			!lose 2nd param
				--npx; --n
			fi
			if n>0 then						!still short; lose excess from locals
				nlx-:=n
			fi
		fi
	fi

!now allocate Xregvars

	reg:=r15
	for i to nlx do
		d:=xlocals[i]
		d.reg:=reg
		isxregvar[reg]:=1
		--reg
	od

	for i to npx do
		d:=xparams[i]
		d.reg:=reg
		isxregvar[reg]:=1
		--reg
	od

!do regvars for leaf procs
	for i to nxleafparams do
		d:=xleafparams[i]
		reg:=xleafparamno[i]+r0-1
		d.reg:=reg
		isxregvar[reg]:=1
	od
end


global proc initproc(psymbol d)=
!initialise genmcl pass through proc pcl code
	psymbol e
	ref procinforec pinfi

	clear regset
	clear xregset
	clear workregs
	clear workxregs
	clear isregvar
	clear isxregvar
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
	maxregvars:=maxxregvars:=0
	npregs:=0
	pinfo:=currfunc.info

	if pinfo=nil then
!NOINFO:
!CPL "NO PINFO"

		nworkregs:=10
		nworkxregs:=12
		for r in r3..r9 do workregs[r]:=1 od
		for r in r6..r15 do workxregs[r]:=1 od
	else	
!GOTO NOINFO
		npregs:=min(4, max(currfunc.nparams, pinfo.nmaxargs))
		nworkregs:=4

		if pinfo.hasblocks then ++nworkregs fi
		nworkxregs:=5

		n:=nworkregs-3

		if npregs<=3 and n then		!use at least one preg
			workregs[r13]:=1
			--n
			if npregs<=2 and n then
				workregs[r12]:=1
				--n
			fi
		fi

		r:=r9					!do any remaining workregs from r9 downwards
		to n do					!allocate from r9 down
			workregs[r--]:=1
		od

		r:=r6
		to nworkxregs-2 do					!allocate from r6 up
			workxregs[r++]:=1
		od
	fi

	for r in r3..r9 when not workregs[r] do ++maxregvars od
	for r in r6..r15 when not workxregs[r] do ++maxxregvars od

!CPL =NWORKREGS, =MAXREGVARS
!FOR R IN R0..R15 DO
!	IF WORKREGS[R] THEN CPL "WORK:", GETREGNAME(R) FI
!OD

!	println currfunc.name,,":",=nworkregs, =nworkxregs, =npregs, =MAXREGVARS, =MAXXREGVARS
!	cp "  "; for r in r0..r13 when workregs[r] do print getregname(r),$ od; cpl
!	cp "  "; for r in r0..r15 when workxregs[r] do print getxregname(r),$ od; cpl

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

	return unless fregoptim and currfunc.info
!CPL "DOING REGALLOC"

!.info will be available here

	if currfunc.info.assemused then
		return
	fi

	allocregvars(currfunc.ismain or currfunc.variadic, currfunc.info.isleaf)
end

global proc do_procentry(pcl p)=
	int retmode, ntemps, hasequiv, offset, size, reg
	mclopnd ax
	psymbol d
	[100]char str, newname
	int rr, ff

	setmclentry(mclprocentry)

	bspill:=bxspill:=0
!	if highreg>=r3 then bspill:=highreg-r2 fi		!no of regs d3..highreg
!	if highxreg>=r6 then bxspill:=highxreg-r5 fi	!no of xregs x6..highxreg

	unless currfunc.info and currfunc.info.assemused then
		for r in r3..r9 when usedregs[r] or isregvar[r] do ++bspill od

		for r in r6..r15 when usedxregs[r] or isxregvar[r] do ++bxspill od
	end

	d:=currfunc.nextparam
	while d, d:=d.nextparam do
		IF D.ATVAR THEN MERROR("@PARAM") FI
		if not d.reg then			!not a regvar
			d.offset:=paramoffset+16+(bspill+bxspill)*8
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		else						!assume regvar
			rr:=d.reg
			ff:=usedregs[rframe]
			d.reg:=0

			genmc(m_definereg, mgenmem(d), mgenreg(rr, d.mode))
			d.reg:=rr
			usedregs[rframe]:=ff
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
		nextloop unless d.used

		if d.atvar then
			hasequiv:=1

        elsif d.reg then
			rr:=d.reg
			ff:=usedregs[rframe]
			d.reg:=0
			genmc(m_definereg, mgenmem(d), mgenreg(rr, d.mode))
			d.reg:=rr
			usedregs[rframe]:=ff

        else
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

	if currfunc.isthreaded then
		if currfunc.nlocals or currfunc.nparams then merror("Threaded proc has locals/params") fi
		if ntemps then merror("Threaded proc has temps") fi

!*!		if bspill or bxspill then merror("Threaded proc has spill regs") fi

		resetmclentry()
		return
	fi

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
		for r:=r3 to r9 when usedregs[r] or isregvar[r] do
			genmc(m_push, mgenreg(r, tpu64))
		od
	fi

	if bxspill then
		ax:=mgenreg(r0, tpu64)
		for r:=xr6 to xr15 when usedxregs[r] or isxregvar[r] do
!			ax:=mgenindex(areg:rframe, size:8, offset:offset)
!			offset+:=8
			genmc(m_movq, ax, mgenxreg(r))
			genmc(m_push, ax)
		od
	fi

	MGENCOMMENT("?]]")
	MCLFRAMESETUP:=MCCODEX

	spillparams()

!MCOMM("COPY PARAMS TO REGVARS?")

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

	SETMCLENTRYF(mclframesetup)

	if framebytes or currfunc.nparams then
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

	if framebytes or currfunc.nparams then
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
			elsif d.reg then
				if ispfloat(d.mode) then
					if d.reg>=xr4 then				!not in-situ param
						genmc(m_movq, mgenxreg(d.reg), mgenxreg(regoffset+xr0, d.mode))
					fi
				elsif d.reg<=r9 then				!not in-situ param
					genmc(m_mov, mgenreg(d.reg, d.mode), mgenreg(regoffset+r10,d.mode))
				fi
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

global proc do_for(pcl p, int incop, addop, cond)=
	pcl q,r
	mclopnd ax,bx,cx,dx,mx
	int reg

	q:=p+1
	r:=currpcl:=q+1

	mx:=mgenmem(q.def, pmode)

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

	genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
end

!global proc do_for(pcl p, int incop, addop, cond)=
!THIS VERSION USES JMPEQ, so that an upper limit of i64.max/u64.max is handled.
!But behaviour changes a little:
!  loop index after normal termination will be LIMIT not LIMIT+1 or LIMIT-1
!  loop index can't be modified inside the loop, so that it has a
!  value beyond the limit

!	pcl q,r
!	mclopnd ax,bx,cx,dx,mx
!	int reg, lab
!
!	q:=p+1
!	r:=currpcl:=q+1
!
!	lab:=++mlabelno
!
!	mx:=mgenmem(q.def, pmode)
!
!	if q.def.reg then
!		ax:=mx
!	else
!		ax:=mgenreg(getworkireg())
!		genmc(m_mov, ax,mx)
!	fi
!
!	if r.opndtype=int_opnd then
!		bx:=mgenint(r.value)
!	else
!		bx:=mgenmem(r.def)
!	fi
!
!	genmc(m_cmp, ax, bx)
!	genmc_cond(m_jmpcc, z_cond, mgenlabel(lab))
!
!	if q.def.reg then
!		if p.stepx=1 then
!			genmc(incop, ax)
!		else
!			genmc(addop, ax, mgenint(p.stepx))
!		fi
!	else
!		if p.stepx=1 then
!			genmc(incop, ax)
!		else
!			genmc(addop, ax, mgenint(p.stepx))
!		fi
!		genmc(m_mov, mx, ax)
!	fi
!
!	genmc(m_jmp, mgenlabel(p.labelno))
!
!	genmc(m_labelx,mgenlabel(lab))
!
!
!end

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

	q:=isimmload(zz)
	if q then
		offset:=q.value*scale+extra	!for imm offset
	fi

	px:=nil

	if pclloc[yy]=regvar_loc then
		if pclloc[zz]=regvar_loc then		!regvar/regvar
			reg:=pclreg[zz]
			regix:=scaleregvar(reg,scale,zz)
			px:=mgenindex(areg:pclreg[yy],ireg:regix, offset:extra, scale:scale)
!
		elsif q then						!regvar/imm
			px:=mgenindex(areg:pclreg[yy], offset:offset)
		else								!regvar/any
			scale:=scaleindex(bx:=loadopnd(zz, pclmode[zz]),scale)
			px:=mgenindex(areg:pclreg[yy], ireg:bx.reg, scale:scale, offset:extra)
		fi

	elsif ismemaddr(yy) then
		d:=pclopnd[yy].def
		if d.id=static_id and phighmem=2 or D.ID=PARAM_ID AND D.MODE=TPBLOCK  then skip fi

		if pclloc[zz]=regvar_loc then			!memaddr/regvar
			reg:=pclreg[zz]
			regix:=scaleregvar(reg,scale,zz)
			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)
!
		elsif q then			!memaddr/imm
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

!MCOMM("ONE")
	ax:=getopnd_ind(zz, p.mode)
!MCOMM("TWO")
!MCOMM(MSTROPND(AX))
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

		ax:=loadopnd(locyy, pmode)
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

global proc clearblock(mclopnd ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

!CPL "CLEARBLOCK", MSTROPND(AX), N

	mclopnd rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg

	oddbytes:=n rem 8		!will be zero, or 1..7

	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=getworkregm(tpu64)
	clearreg(rx)

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

!		genmc(m_movdqa, rx, bx)
!		genmc(m_movdqa, ax, rx)

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

	return unless creallist or cr32list

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
		if mask<=word(i32.max) then			!use immediate
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
				genmc(m_andx, px, ax)			!set to 0
			else								!set to 1 (assume r.value was 1)
				genmc(m_orx, px, ax)
			fi

		else									!A.[i]:=x
			merror("STOREBIT/VAR")
		fi
	else
			merror("Storebit: both vars")
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
	ichar name, namec

	if igethostfn=nil then

!try manual seach through pcl code
		case opc
		when kpower then
			name:="msys.m$power_i64"		!msys or msysc
			namec:="msysc.m$power_i64"
		else
			name:=nil
		esac

		if name then
			psymbol ps:=psymboltable
			while ps, ps:=ps.next do
				if eqstring(name, ps.name) or eqstring(namec, ps.name) then
					return ps
				fi
			od
		fi

		merror("gethostfn?", pclnames[opc])
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

global proc fixmain=
!d is a main func with 2 params
!convert params to locald, add more locals needed for calling __getmainargs
	psymbol d:=currfunc, e
	psymbol dn, dargs, denv, dinfo
	mclopnd ax

	dn:=d.nextparam
	dargs:=dn.nextparam

!add 2 new locals
!	denv:=pc_makesymbol("$env", local_id)
	denv:=pc_makesymbol("$env", static_id)
	denv.mode:=tpref
	denv.size:=8

!	dinfo:=pc_makesymbol("$info", local_id)
	dinfo:=pc_makesymbol("$info", static_id)
	dinfo.mode:=tpblock
	dinfo.size:=128


	setsegment('Z',8)
	genmc(m_labelx, mgenmemaddr(dinfo))
	genmc(m_resb, mgenint(128))
	genmc(m_labelx, mgenmemaddr(denv))
	genmc(m_resb, mgenint(8))
	setsegment('C',1)
	pc_addlocal(denv)
	pc_addlocal(dinfo)

!remove dn/dargs as params

	dn.nextparam:=dargs.nextparam:=d.nextparam:=nil
	d.nparams:=0
	dn.id:=local_id
	dn.used:=1
	dargs.id:=local_id
	dargs.used:=local_id
!
!add them as locals

	pc_addlocal(dargs)
	pc_addlocal(dn)

	genmc(m_push, ax:=mgenreg(r0))
	genmc(m_lea , ax, mgenmem(dinfo))
DINFO.ADDROF:=1
	genmc(m_push, ax)
	genmc(m_sub, dstackopnd, mgenint(32))
	genmc(m_lea,  mgenreg(r10), mgenmem(dn))
DN.ADDROF:=1
	genmc(m_lea,  mgenreg(r11), mgenmem(dargs))
DARGS.ADDROF:=1
	genmc(m_lea,  mgenreg(r12), mgenmem(denv))
DENV.ADDROF:=1
	clearreg(mgenreg(r13))
	genmc(m_call, mgenextname("__getmainargs*"))
!
	genmc(m_sub, dstackopnd, mgenint(48))

!do pcmdskip fixes
	if pcmdskip then
		genmc(m_sub, mgenmem(dn), mgenint(pcmdskip, tpi32))
		genmc(m_add, mgenmem(dargs), mgenint(pcmdskip*8))
	fi

end
=== mc_libmcl.m 0 0 12/79 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

export const ctarget=0

!global int mclseqno
EXPORT int mclseqno
EXPORT int NMCLOPND

[-1..10]mclopnd smallinttable
[20]psymbol nametable
int nnametable

global macro isframex(d) = (d.id in [local_id, param_id])

global macro mcomm = mgencomment

export proc mclinit(int bypass=0)=
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

	lab_funcnametable:=0
	lab_funcaddrtable:=0

	for i in smallinttable.bounds do
		smallinttable[i]:=mgenint0(i,8)
	od

!bypass is used when directly using mcl api (eg. from an external assembler)
!then genmcl(), called from pcl functions, is a no-op
	if bypass then
		mcldone:=1
	fi
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
		++def.nrefs
		a.valtype:=def_val
		if isframex(def) then
			a.reg:=rframe
			usedregs[rframe]:=1
		fi
	fi

	return a
end

global proc mgencomment(ichar s)=
!if not debugmode then return fi
!	if s=nil or s^=0 then
!		genmc(m_blank)
!	else
		genmc_str(m_comment,s)
!	fi
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

	if d.reg then
		if pfloat[d.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d, mode)
		fi
	fi

	reg:=rnone
	if isframex(d) then
!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!			return frameregtable[d.offset]
!		fi

		reg:=rframe
		usedregs[rframe]:=1

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
!EXPORT func mgenreg(int reg, mode)mclopnd a =
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

global func mgenregi(int reg, mode=tpi64)mclopnd a =
	if fuseregtable then
		return regtable[reg, psize[mode]]
	fi
	return mgenreg0(reg, psize[mode])
end

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

global func mgenregvar(psymbol d, int mode)mclopnd a=
	a:=mgenreg(d.reg, mode)
!	isregvar[d.reg]:=1

	return a
end

global func mgenxregvar(psymbol d)mclopnd a=
	a:=mgenxreg(d.reg)
	isxregvar[d.reg]:=1

	return a
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

global proc axerror(ichar mess)=
!	CPL "AX ERROR:", mess, "AASEQ:", aaseqno, =aapos
	CPL "AX ERROR:", mess, "Line:", aapos
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
	else
		merror("Ext nametab overflow")
	fi
end

export proc callproc(ichar cpname, name, int lineno)=
RETURN
end

func mgenstringx(ichar s)mclopnd=
	mgenlabelmem(getstringindex(s))
end

global proc clearreg(mclopnd ax)=
	if ax.size=8 then
		ax:=changeopndsize(ax,4)
	fi
	genmc(m_xorx, ax, ax)
end
=== mc_stackmcl.m 0 0 13/79 ===
!'PCS' Support - PCL Operand Stack 

global func getopnd(int n, mode, reg=rnone)mclopnd ax =
!get access mode for operand n
	mclopnd bx
	pcl a
	psymbol d

	a:=pclopnd[n]

	case pclloc[n]
!	when reg_loc then
	when reg_loc, regvar_loc then
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

		genmc(m_lea, ax, mgenlabelmem(getstringindex(a.svalue)))

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

	if pclloc[n]=regvar_loc then			!force a load to a workreg
		if reg=rnone then
			reg:=getworkreg(mode)
		fi
	fi

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
!		genmc(m_xorx, bx, bx)
	
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

	if p.opndtype=mem_opnd and p.def.reg then
		pclreg[n]:=p.def.reg
		pclloc[n]:=regvar_loc
	fi

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

!MCOMM("DUPLOP")
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
!CPL "GWI"
!FOR R:=R0 TO R13 DO
!	CP REGSET[R],$
!OD
!CPL
!FOR R:=R0 TO R13 DO
!	CP WORKREGS[R],$
!OD
!CPL
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

	if ispint(mode) and isregvar[reg] or isxregvar[reg] then
		return getworkregm(mode)
	fi

	mgenreg(reg, mode)
end

!global proc freeireg(int reg)=
!	regset[reg]:=0
!end

!global proc freexreg(int reg)=
!	xregset[reg]:=0
!end

!global proc freereg(int reg, mode)=
!	if ispfloat(mode) then
!		xregset[reg]:=0
!	else
!		regset[reg]:=0
!	fi
!end
!
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
!		if newreg>=xr3 then highxreg max:=newreg fi
	else
		regset[newreg]:=1
!		if newreg>=r10 then highreg max:=newreg fi
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

!		if pclopnd[i].opndtype=mem_opnd and pclloc[i] in [pcl_loc, regvar_loc] then
!			loadopnd(i, pclopnd[i].mode)
!		fi

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
		when reg_loc, regvar_loc then				!loaded
			if ispfloat(pclmode[i]) then
				strcat(s, xregnames[pclreg[i]])
			else
				strcat(s, regnames[pclreg[i]])
			fi
			if pclloc[i]=regvar_loc then strcat(s, "*") fi
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
		strcat(s, "<")
		strcat(s, pstdnames[pclmode[i]])
		strcat(s, ">")

		if i<noperands then strcat(s,", ") fi
	od
	strcat(s,") ")

	ipadstr(str, 50)

	strcat(s,"WR:(")
!	for r:=r0 to r9 when workregs[r] do
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
=== mc_optim.m 0 0 14/79 ===
global proc peephole=
	ref mclrec m, m2,m3
	int lab1,lab2

	if not fpeephole then return fi
!CPL "PEEP"

	m:=mccode.nextmcl

	while m, m:=m.nextmcl do 
		m2:=m.nextmcl
		m3:=m2.nextmcl

		case m.opcode
		when m_endx then
			exit

		when m_mov then
			case m2.opcode
			when m_mov then					!mov/mov
				if isreg0(m.a) and m.a=m2.b and endr0(m2) then		!mov r0,x/mov x,r0
					if isreg10(m2.a) then
					elsif isreg(m2.a) or isreg(m.b) then				!one x is a reg
						m.a:=m2.a
						deletemcl(m2)
					fi
				elsif isreg0(m.a) and m.a=m2.b and isreg10(m2.a) and m3.opcode=m_call and
						endr0(m3) then
					m.a:=m2.a
					deletemcl(m2)
				fi
			when m_test then				!mov/test
				if isreg0(m.a) and m.a=m2.a=m2.b and isreg(m.b) and endr0(m3) then		!mov r0,x/test r0,r0
					m.opcode:=m_test
					m.a:=m.b
					m:=deletemcl(m2)
				fi
			when m_cmp then					!mov r0, reg/cmp r0,x
				if isreg0(m.a) and m.a=m2.a and isreg(m.b) and endr0(m3) then
					m.opcode:=m_cmp
					m.a:=m.b
					m.b:=m2.b
					deletemcl(m2)
				fi
			when m_add, m_sub then
				if isreg(m.a) and m.a=m2.a and isreg(m.b) and isconst(m2.b) then
					m.opcode:=m_lea
					m.b:=mgenindex(areg:m.b.reg, offset: (m2.opcode=m_add|m2.b.value|-m2.b.value))
					deletemcl(m2)
				fi
			when m_inc, m_dec then
				if isreg(m.a) and m.a=m2.a and isreg(m.b) then
					m.opcode:=m_lea
					m.b:=mgenindex(areg:m.b.reg, offset: (m2.opcode=m_inc|1|-1))
					deletemcl(m2)
				fi
			when m_jmp then
				if isreg0(m.a) and isreg0(m2.a) then
					m.opcode:=m_jmp
					m.a:=m.b
					m.b:=nil
					deletemcl(m2)
				fi
			esac

		when m_andx then
			if m2.opcode=m_test then				!and r0../test r0,r0 -> and r0.. only
				if isreg0(m.a) and m.a=m2.a=m2.b and endr0(m3) then
					m:=deletemcl(m2)
				fi
			fi
		when m_xorx then
			if m2.opcode=m_mov then					!xor r0,r0; mov reg, r0
				if isreg0(m.a) and m.a=m.b and isreg(m2.a) and isreg0(m2.b) and endr0(m2) then
					m.a:=m.b:=m2.a
					m:=deletemcl(m2)
				fi
			fi

		when m_jmpcc then
			if m2.opcode=m_jmp and m3.opcode=m_labelx and m.a.labelno=m3.a.labelno and endr0(m) then
				m.cond:=asmrevcond[m.cond]
				m.a:=m2.a
				m:=deletemcl(m2)

			fi

!		when m_add then
!			if m2.opcode in [m_add, m_sub] then
!				if isreg(m.a) and m.a=m2.a and isreg(m.b) and isconst(m2.b) then
!STATIC INT AA
!!					m.opcode:=m_lea
!!					m.b:=mgenindex(areg:m.b.reg, offset: (m2.opcode=m_add|m2.b.value|-m2.b.value))
!!					deletemcl(m2)
!CPL "ADD/ADD/SUB NN",++AA
!				fi
!			fi
!
!!!		when m_jmp then			!this uses more bytes than it saves, when self-hosting
!!!			if m.a.mode=a_imm and m2.opcode=m_labelx and m.a.labelno=m2.a.labelno then
!!!				m:=deletemcl(m)
!!			FI

		esac
	od

end

func isreg(mclopnd a)int=
	return a.mode=a_reg
end

func isreg0(mclopnd a)int=
	if not a then return 0 fi
	if a.mode=a_reg and a.reg=r0 then return 1 fi
	return 0
end

func isreg10(mclopnd a)int=
	if not a then return 0 fi
	if a.mode=a_reg and a.reg=r10 then return 1 fi
	return 0
end

func isreg00(ref mclrec m)int=
	if isreg(m.a) and m.a=m.b then return 1 fi
	0
end

func isconst(mclopnd a)int=
	if not a then return 0 fi
	if a.mode=a_imm and a.valtype=intimm_val then
		return 1
	fi
	return 0
end

func sameoperand(mclopnd a,b)int=
	return memcmp(a,b,mclopndrec.bytes)=0
end

func sameregopnd(mclopnd a,b)int=
!check if same register operand
	unless a.mode=b.mode=a_reg then return 0 end
	return a.reg=b.reg
end

!func deletemcl(ref mclrec p, ichar comment=nil)ref mclrec =
func deletemcl(ref mclrec p)ref mclrec =
!delete p; return following instr
	ref mclrec a,b

	a:=p.lastmcl
	b:=p.nextmcl
	if a=nil or b=nil then merror("delmcl?") fi

!	if comment then
!		p.opcode:=m_comment
!		p.a:=mgenstring(pcm_copyheapstring(comment))
!	else
		a.nextmcl:=b
		b.lastmcl:=a
!	fi

	b
end

func endr0(ref mclrec m)int=
	return m.regfreed[r0]
end
=== mc_genss.m 0 0 15/79 ===
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

GLOBAL record amoderec =					!return from genrm
!record amoderec =					!return from genrm
	byte modrm						!
	byte sib						!
	i8 usesib						!-1/0/1 = rip/not used/used
	byte dispsize					!0, 1 or 4
	i32 offset					!for dispsize = 1/4
end

mclopnd extraparam

int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
ref relocrec currrelocs
int nrelocs

[r0..r15]byte ishighreg				!high regs have 0x40 (see start)

REF MCLREC CURRMCL
ref riprec ripentry

macro genbyte(x) = currdata.pcurr++^:=x

macro makemodrm(mode,opc,rm) = mode<<6+opc<<3+rm

global proc genss(int obj=0)=
	int index
	ref mclrec m

	return when ssdone

	sstime:=os_clock()

	initlib(mlabelno)
!CPL "INITLAB 50"
!	initlib(50)

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
!CPL "DONE GENSS LOOP"

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		axerror("Zdata contains code or data")
	fi

	if obj then					!do fixups needed for .obj files
		ref riprec pr			!(exe module does its own fixups)
		ref byte codeaddr
		ref u32 offsetptr

		codeaddr:=bufferelemptr(ss_code, 0)
			pr:=riplist
			while pr, pr:=pr.next do
				offsetptr:=ref u32(codeaddr+pr.offset)
				offsetptr^-:=pr.immsize
		od
	fi

	ssdone:=1
	sstime:=os_clock()-sstime

end

proc doinstr(ref mclrec m,int index)=
	mclopnd a,b
	psymbol d,e
	int x,offset,shortjmp,n

!CPL "DOINSTR",MCLNAMES[M.OPCODE], M.SEQNO

	if currdata.pend-currdata.pcurr<1024 then
		bufferexpand(currdata)
	fi

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=nowmask:=usesizeb:=0

	a:=m.a
	b:=m.b

	aaseqno:=m.seqno
	aapos:=m.mpos
	ripentry:=nil
	CURRMCL:=M

	switch m.opcode
	when m_procstart then
		CURRASMPROC:=M.A.DEF
!CPL "PROC", CURRASMPROC.NAME

	when m_procend then
	when m_define then

	when m_definereg then

	when m_labelname then
		case a.valtype
		when stringimm_val then
		when def_val then
			d:=a.def
!CPL "LABEL", D.NAME
			d.reftype:=back_ref
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)

			if d.exported then
				getstindex(d)
			fi

			dofwdrefs(d)
		esac

	when m_labelx then

		if a.valtype=def_val then			!named label (probably from assembler)
			d:=a.def
		else
			d:=labeldeftable[a.labelno]
		fi
	
		d.reftype:=back_ref
		d.segment:=currseg
		d.offset:=getcurrdatalen(6)

		if d.exported then
			getstindex(d)
		fi

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
			buffercheck(currdata, x)
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

	when m_comisd, m_ucomisd then
		do_arithxmm(a,b,0x66,mclcodes[m.opcode])

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

!	when m_param then
!		extraparam:=a

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

	when m_comment, m_endx then
	else
		println "*** Can't do opcode",mclnames[m.opcode],"line",aaseqno,=M.OPCODE,=M_HALT
	CPL
	CPL
	AXERROR("STOPPING")
!	end switch
	end

end

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

proc genqword(i64 x)=
	addqword(currdata,x)
end

proc genopnd(mclopnd a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
	ref char s
	i64 x
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
			r32 x32
			x32:=a.xvalue
			gendword(i32@(x32))
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
			genqword(i64@(a.xvalue))
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

		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
			if d.imported then
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
		return i32.max
	fi
end

proc dofwdrefs(psymbol d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
	ref fwdrec f
	int offset, seg
	ref byte p8
	ref i32 p32
	ref i64 p64
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

func isbytesized(i64 x)int=
	return -128<=x<=127
end

func isdwordsized(i64 x)int=
	return i32.min<=x<=i32.max
end

proc genamode(mclopnd a, amoderec am)=
	psymbol d
	ref riprec pr

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

func getdispsize(mclopnd a, i32 &offset)int=
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
	mclopnd a

	n:=0
	m:=m.nextmcl
	while m and n<=8 do
		case m.opcode
		when m_labelx then
			a:=m.a

			case a.valtype
			when label_val then
				if a.labelno=d.labelno then return 1 fi
			when def_val then
				if a.def=d then return 1 fi
			esac

		when m_comment, m_endx then
		when m_resb then
			return 0
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
	psymbol d

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0
	labeldeftable:=pcm_alloc(nlabels*ref void.bytes)

!IF NLABELS<MLABELNO THEN
!CPL "INITLAB: BAD LABEL COUNT"
!CPL "INITLAB: BAD LABEL COUNT"
!!SROP
!STOP
!FI
!
!CPL "//INITLIB", NLABELS
!STOP

	for i to nlabels do
		d:=labeldeftable[i]:=pcm_allocnfz(pstrec.bytes)

!CPL "SETTING LAB",I,"TO",D
		d.labelno:=i
		fprint @&.str,"l#",i
		d.name:=pcm_copyheapstring(&.str)
		d.reftype:=fwd_ref
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

global proc addqword(ref dbuffer a, i64 x)=
	a.pcurr64^:=x
	++(a.pcurr64)
end

proc genxrm(int opcode, code, mclopnd b)=
!deal with /d instructions, where code = 0..7
	amoderec am

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
	case size
	when 1 then
		unless -128<=value<=255 then axerror("exceeding byte value") end

	when 2 then
		unless -32768<=value<=65535 then axerror("exceeding u16 value") end
	else
		unless -0x8000'0000<=value<=0xFFFF'FFFF then axerror("2:exceeding u32 value") end
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

	clear am

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

	else
		axerror("genrm not mem")
	esac

	mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
	scale:=0				!0=modrm only; 1/2/4/8 means sib used

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
SIMPLE:
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

IF B.SCALE<=1 THEN					!try and avoid sib
		SWAP(REG, REGIX)
		GOTO SIMPLE
FI

		am.dispsize:=4
		mode:=0
		rm:=4
		scale:=(b.scale|b.scale|1)
		base:=5
		index:=getregcode(regix, xmask)
		if regix=rstack then axerror("Scaled rstack?") fi

	else									!assume regix used; optional reg and disp
		am.dispsize:=getdispsize(b, am.offset)
		if am.dispsize then
			mode:=(am.dispsize=1|1|2)
		fi
		rm:=4

		scale:=(b.scale|b.scale|1)
!CP "SCAD"
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
	i64 x

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
	i64 value
	psymbol d:=getdef(b)

	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			if a.size<>b.size and b.size then axerror("2:Opnd size mismatch") fi

			genrrm((a.size=1|0x8A|0x8B), a, b)

		when a_imm then
			value:=b.value

			regcode:=getregcode(a.reg, bmask)
			setopsize(a)
			if d and a.size<=2 then axerror("mov imm?") fi

			CHECKHIGHREG(A)

			case a.size
			when 1 then
				unless -128<=value<=255 then axerror("exceeding byte value") end
				genrex()
				genbyte(0xB0+regcode)
				genbyte(value)

			when 2 then
				unless -32768<=value<=65535 then axerror("exceeding u16 value") end
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
						axerror("1:exceeding u32 value")
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

	if a.size<4 then
CPL =A.SIZE
 axerror("LEA size error") fi
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
	i64 value

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
	i64 value
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

=== mc_decls.m 0 0 16/79 ===
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

export enumdata [0:]ichar valtypenames =
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
!	(m_blank,			$,		0,		0),		!
!	(m_deleted,			$,		0,		0),		!
	(m_labelname,		$,		0,		0),		!
	(m_define,			$,		0,		0),		!
	(m_definereg,		$,		0,		0),		!
	(m_trace,			$,		0,		0),		!
	(m_endx,			$,		0,		0),		!

	(m_labelx,			$,		1,		0),		!
	(m_nop,				$,		0,		0x90),		!
!	(m_param,			$,		1,		0),		!
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
	(m_jmpcc,			$,		1,		0),		!
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
	(m_setcc,			$,		1,		0),		!

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
	(m_comisd,			$,		2,		0x2F),	!
	(m_ucomisd,			$,		2,		0x2E),	!

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

	(m_xxxx,			$,		0,		0xF4),	!
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

export enumdata [0:]ichar condnames, [0:]ichar asmcondnames,
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

global enumdata [0:]ichar locnames =
	(pcl_loc=0,	"pend"),				!operand still in pcl instruction
	(reg_loc,	"reg"),					!is in register (look at mode for reg/xreg)
	(regvar_loc,"regvar"),				!lives in register (look at mode for reg/xreg)
	(temp_loc,	"temp"),				!overflow to temporary
end

global [r0..r15]byte workregs, workxregs		!1 indicates available work regs
global int nworkregs, nworkxregs				!no. workregs assigned
global int nregvars, nxregvars					!no. reg vars allocated (consec regs)
global int maxregvars, maxxregvars				!no. reg vars available

global int xregmax


global [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
global [r0..r15]byte xregset		!same for xregs

global [r0..r15]byte isregvar
global [r0..r15]byte isxregvar

global record pair =
	u64 low, high
end

global pair regsetpr @ regset
global pair isregvarpr @ isregvar
global const u64 invertbytes = 0x0101'0101'0101'0101

global [r0..r15]byte usedregs		!1 means used during proc
global [r0..r15]byte usedxregs		!1 means used during proc

global byte noxorclear		!1 to suppress xor optimisation

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

global const maxcalldepth=32
global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret	!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize	!size of any returned block
global [maxcalldepth,4]u32 callargsize	!size of any block pushed in low args
global int ncalldepth

global int lababs32, lababs64
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
end

global ref constrec cstringlist
global ref constrec vstringlist
global ref constrec creallist
global ref constrec cr32list

global psymbol currasmproc

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

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

!global byte fpshortnames
global byte fpcheckunusedlocals
!export byte phighmem

global record riprec =
	ref riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

!global record fwdrec =
!	ref fwdrec nextfwd
!	i32 offset
!	i16 reltype
!	i16 seg
!end

global ref riprec riplist

!export ref proc (ref void) idomcl_assem
!export ref func (ref void)int icheckasmlabel
!export ref func (int)psymbol igethostfn

global const maxblocktemps=50
global [maxblocktemps]psymbol blockdefs
global int nblocktemps

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)


global [pstdnames.bounds]byte ploadopx

global [pstdnames.bounds]byte ploadop

proc start=
	for i in ploadop.bounds do ploadop[i]:=m_nop od

	ploadop[tpu8]:=ploadop[tpu16]:=ploadop[tpu32]:=m_movzx
	ploadop[tpi8]:=ploadop[tpi16]:=ploadop[tpi32]:=m_movsx
	ploadop[tpr32]:=m_movd
	ploadop[tpr64]:=m_movq
	ploadop[tpu64]:=ploadop[tpi64]:=m_mov
end

=== mc_objdecls.m 0 0 17/79 ===
global record imagefileheader =
	u16	machine
	u16	nsections
	u32	timedatestamp
	u32	symtaboffset
	u32	nsymbols
	u16	optheadersize
	u16	characteristics
end

global record imagedir =
	u32	virtualaddr
	u32	size
end

global record optionalheader =			!exe/dll only
	u16  magic
	byte     majorlv
	byte     minorlv
	u32 codesize
	u32 idatasize
	u32 zdatasize
	u32 entrypoint
	u32 codebase
!	u32 datebase		!32-bit exe files only
	u64	imagebase
	u32 sectionalignment
	u32 filealignment
	u16  majorosv
	u16  minorosv
	u16  majorimagev
	u16  minorimagev
	u16  majorssv
	u16  minorssv
	u32 win32version
	u32 imagesize
	u32 headerssize
	u32 checksum
	u16  subsystem
	u16  dllcharacteristics
	u64   stackreserve
	u64   stackcommit
	u64   heapreserve
	u64   heapcommit
	u32 loaderflags
	u32 rvadims
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
		u32	physical_address
		u32	virtual_size
	end
	u32	virtual_address
	u32	rawdata_size
	u32	rawdata_offset
	u32	relocations_ptr
	u32	linenos_offset
	u16	nrelocs
	u16	nlinenos
	u32	characteristics
end

global record imagesymbol =
	union
		[8]char shortname
		struct
			u32	shortx
			u32	longx
		end
		u64 longname
	end
	u32	value
	i16	sectionno
	u16	symtype
	byte	storageclass
	byte	nauxsymbols
end

global record importdirrec =
	u32	implookuprva
	u32	timedatestamp
	u32	fwdchain
	u32	namerva
	u32	impaddressrva
end

global record coffrelocrec =
	i32	virtualaddr
	i32	stindex
	i16	reloctype
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
	i32 length
	i16 nrelocs
	i16 nlines
	i32 checksum
	i16 sectionno
	i32 dummy
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
	u32 exportflags
	u32 timedatestamp
	u16 majorversion
	u16 minorversion
	u32 namerva
	u32 ordinalbase
	u32 naddrtable
	u32 nnamepointers
	u32 expaddressrva
	u32 namepointerrva
	u32 ordtablerva
end
=== mc_writeasm.m 0 0 18/79 ===
!export int assemtype='AA'

!const fshowseq=1
const fshowseq=0

!const useintelregs=1
const useintelregs=0

!const showsizes=1
const showsizes=0

!const showfreed=1
const showfreed=0

!const fextendednames=1			!include module name in qualified names
const fextendednames=0

[8, r0..r15]ichar nregnames

[r0..r15]psymbol regvars		!nil, or strec when it uses that reg
[r0..r15]psymbol xregvars

proc writemcl(int index,ref mclrec mcl)=

!	case mcl.opcode
	
	if mcl.opcode=m_comment and mcl.a.svalue^='?' then
	else
		strmcl(mcl)
		gs_line(pdest)
	fi
!	esac
end

global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mclopnd a,b
	int opcode,cond,sizepref
	ichar s,comment
	psymbol d

	opcode:=mcl.opcode
	str[1]:=0

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
	comment:=nil

	case opcode
	when m_procstart then
		asmstr(";Proc ")
		asmstr(a.def.name)
		currasmproc:=a.def
		clear regvars
		clear xregvars

		return

	when m_procend then
		asmstr(";End\n")
		currasmproc:=nil

		return

	when m_comment then
		asmchar(';')
		asmstr(a.svalue)
		return
	when m_endx then
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
				asmstr(":")
			else
				asmstr("\n`")
				asmstr(getbasename(d.name))
				asmstr("::")
			fi
		fi


		return

	when m_labelx then
		if a.valtype=label_val then
			fprint @str,"L#:",a.value
		else
			recase m_labelname
		fi
		asmstr(&.str)
		return

	when m_define then
		asmstr("    ")
		asmstr(a.svalue)
		asmstr(" = ")
		asmopnd(b)
		return

	when m_definereg then
		d:=a.def
		asmstr("    ")
		asmstr(getdispname(d))
		if ispfloat(d.mode) then
			xregvars[d.reg]:=d
		else
			regvars[d.reg]:=d
		fi

!		asmstr(a.svalue)
		asmstr(" = ")

		case b.mode
		when a_reg then
			asmstr(getregname(b.reg, b.size))

		else
			asmstr(getxregname(b.reg, b.size))
		esac
		return

!	WHEN M_TRACE THEN
!!		ASMSTR(SINCLUDE("c:\\px\\trace.aa"))
!		ASMSTR(SINCLUDE("c:trace.aa"))
!
!		RETURN

	esac

	case opcode
	when m_jmpcc then
		print @&.opcname,"j",,asmcondnames[cond]

	when m_setcc then
		print @&.opcname,"set",,asmcondnames[cond]

	when m_cmovcc then
		print @&.opcname,"cmov",,asmcondnames[cond]

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

	ipadstr(&.opcname,(opcode=m_dq|4|10)," ")

	ipadstr(&.str,4)

	strcat(&.str,&.opcname)

	asmstr(&.str)

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

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0,opcode)
		else
			asmopnd(a,1,opcode)
		fi
	fi

	if showsizes then
		if a then
			asmstr("  ; ")
			asmstr(strint(a.size))
			if b then
				asmstr("/")
				asmstr(strint(b.size))
			fi
		fi
	fi

IF SHOWFREED THEN
BYTE FIRST:=1
	FOR R IN R0..R15 WHEN MCL.REGFREED[R] DO
		IF FIRST THEN
			FIRST:=0
			ASMSTR(" #======<")
		ELSE
			ASMSTR(" ")
		FI
		ASMSTR(GETREGNAME(R))
	OD
	IF NOT FIRST THEN
		ASMSTR(">")
	FI
FI

IF FSHOWSEQ THEN ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO)) FI
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
	i64 value,offset,length
	ichar ss

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

	when label_val then
		strcat(&.str,"L")
		strcat(&.str,strint(a.labelno))
		goto addoffset

	when temp_val then
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

	if useintelregs then
		return nregnames[size, reg]
	fi

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

	print @&.str,"XMM",,reg-xr0
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

		IF FEXTENDEDNAMES THEN
			fprint @str,"##R.#.#", (fpshortnames|""|"`"),(pfloat[d.mode]|"X"|""), $PMODULENAME,(fpshortnames|d.name|getfullname(d))
		else
			fprint @str,"##R.#", (fpshortnames|""|"`"),(pfloat[d.mode]|"X"|""), (fpshortnames|d.name|getfullname(d))
		fi

		return str
	fi

	if fpshortnames then
		return d.name
	else
		return getfullname(d,backtick:1)
	fi

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

	d:=regvars[reg]
!D:=NIL

	if d and psize[d.mode]=size then
		return getdispname(d)
	fi
	getregname(reg,size)
end

func strxreg(int reg, size=8)ichar=
	psymbol d

	d:=xregvars[reg]

	if size=8 and d then
		getdispname(d)
	else
		getxregname(reg,size)
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

proc start=
	byte flag

	assemtype:='AA'

	if useintelregs then
		for i in 1..8 when i in [1,2,4,8] do
			for r in r0..r15 do
				flag:=0
				for k in dregnames.bounds do
					if flag then
						if regsizes[k]=i and regindices[k]=r then
							nregnames[i, r]:=dregnames[k]
						fi
					elsif regsizes[k]=0 then
						flag:=1
					fi
				od
			od
		od
	fi
end
=== mc_writeexe.m 0 0 19/79 ===
!Create .exe file from SS-data (code, data, reloc and psymbol tables)

[maxplibfile]i64 libinsttable
[maxplibfile]ichar libinstnames
[maxplibfile]int libnotable			!index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
	ref basereloc nextitem
	u32 address				!virtual address
	i32 reloctype
end

ref basereloc basereloclist
int nbaserelocs
int maxrelocaddr
const maxbaseblock=500
[maxbaseblock]int blockbases
[maxbaseblock]i32 blockcounts
[maxbaseblock]i32 blockbytes
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
ref[]i64 thunktable				!point into code segment
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

global proc writeexe(ichar outfile, int dodll, ichar entrypoint=nil)=
	return when exedone

	genexe1(entrypoint, outfile, dodll)
	genexe2(outfile, dodll)

	exedone:=1
end

global proc genexe1(ichar entrypoint, outfile, int dodll)=
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
		ref riprec pr

		pr:=riplist
		while pr, pr:=pr.next do
			offsetptr:=ref u32(codeaddr+pr.offset)
			offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)
			offsetptr^:=offset	
		od
	fi
end

global proc genexe2(ichar outfile, int dodll)=
!construct the exe image in memory, then write out the file
	imagefileheader header
	optionalheader optheader
	int offset,i
	i64 aa

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

	if pverbose=2 then
		println "EXE size:  ", dataptr-datastart:"10s,jr"
		println
	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

proc loadlibs=
!load library instances
	int i
	i64 hinst
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
		if pverbose=2 then
			println "Idata size:", bufferlength(ss_idata):"10s,jr"
			println "Code+Idata:", bufferlength(ss_code)+bufferlength(ss_idata):"10s,jr"
			println "Zdata size:", ss_zdatalen:"10s,jr"
		fi
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
!				if eqstring(basename,"main") and not isdll then
				if d.isentry and not isdll then
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
	ref u32 p32
	ref u64 p64
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
			(ref u32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.imported then
				(ref u32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
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
!	header.subsystem:=2

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

	i64 aa
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
	ref u32 paddrtable
	ref u32 pnametable
	ref u16 pordtable
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
	
	ref u32 p32
	ref u16 p16
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
	repeat
		int swapped:=0
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
	ref i64 paddr,pname
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
	ref u32 pextra

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
			(ref i32(thunkptr)^:=thunkaddr)
			thunkptr+:=4
		else					!use rip mode

			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(ref i32(thunkptr)^:=getripoffset(int(thunkptr-codebase),thunkaddr-imagebase))
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

=== mc_writeobj.m 0 0 20/79 ===
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
	i64 aa

	return when objdone

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

	objdone:=1

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
	ref i32 p
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
	stringtable[++nstrings]:=pcm_copyheapstring(s)

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

		if s.exported then
			name:=getbasename(name)
!		else
!			name:=getfullname(s)
		fi

		addsymbol(makesymbol(name, sectionno:sect, storage:scope, value:s.offset))

	od
end
=== mc_writess_dummy.m 0 0 21/79 ===
export function writessdata(int fexe)ref strbuffer=
	nil
end
=== mx_decls.m 0 0 22/79 ===
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
	ref[]i16		importxreftable	! map symbol index to global one

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
global [maxsymbols]i16	symbollibindex	! Lib index where defined
global [maxsymbols]byte		symboldllindex	! DLL index of library where found
global int nsymbols

export int nsymimports=0, nsymexports=0
=== mx_run.m 0 0 23/79 ===
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
			if d.isentry then
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
CPL "NO MAIN FOUND"
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

=== mx_lib.m 0 0 24/79 ===
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
!
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
!CPL "RUN/MX: NO ALLOCZ"
!	lib.zdataptr:=pcm_alloc(lib.zdatasize)

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
!	for i to nsymbols when symbolnametable[i]^='$' do
	for i to nsymbols do
		if eqstring(symbolnametable[i],"msys.$cmdskip") or
			eqstring(symbolnametable[i],"$cmdskip") then

			(ref byte(symboladdress[i])^:=cmdskip)
!			(ref byte(symboladdress[i])^:=0)
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

=== mx_write.m 0 0 25/79 ===
!Translate SS data directly into MCB block, then write as mx/ml file

ref dbuffer dest

psymbol entrypoint

global proc writemcx(ichar filename)=
	int n

	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	dest:=buffercreate()

	genu32(mcxsig)

	genbyte(version_dir)
	genstring("0.1234")

	countsymbols()
	writerelocs()

	genbyte(zdata_dir)
	genu32(ss_zdatalen)

	genbyte(code_dir)
	genu32(n:=bufferlength(ss_code))
	genblock(bufferelemptr(ss_code,0), n)

	genbyte(idata_dir)
	genu32(n:=bufferlength(ss_idata))

	genblock(bufferelemptr(ss_idata,0), n)

	int ndlls:=0, nlibs:=0
	for i to nplibfiles when plibfiles[i]^<>'$' do
		++ndlls
	od

	genbyte(dlls_dir)
	genu32(ndlls)
!	for i to nplibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nplibfiles when plibfiles[i]^<>'$' do
		genstring(plibfiles[i])
	od

	writesymbols()

	genbyte(end_dir)

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
	genu32(n:=ss_nidatarelocs+ss_ncoderelocs)

	count:=0

	for i in code_seg..idata_seg do
		oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

		while oldr, oldr:=oldr.nextreloc do
			++count
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

			genblock(&newr, newr.bytes)

		od
	od
end

proc writesymbols=
	psymbol d
	int n
	ichar name

	genbyte(importsymbols_dir)
	genu32(nsymimports)

	for i to ss_nsymbols when ss_symboltable[i].impindex do
		d:=ss_symboltable[i]
		genstring(d.name)
	od

	genbyte(exportsymbols_dir)
	genu32(nsymexports)

	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
!			if eqstring(d.name, "main") then
			if d.isentry then
				entrypoint:=d
			fi
			genstring(d.name)
		fi
	od

	genbyte(exportsegs_dir)
	genu32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genbyte(d.segment)
		fi
	od

	genbyte(exportoffsets_dir)
	genu32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genu32(d.offset)
		fi
	od

	genbyte(entry_dir)		!must be present; writes 0xFFFFFFFF when no entry point
	if entrypoint then
		genu32(entrypoint.offset)
	else
		genu32(0xFFFF'FFFF)
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

proc genu32(int x)=
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
=== cc_cli.m 0 0 26/79 ===
enumdata []ichar passnames, []ichar extnames =
	(load_pass,		$,		""),
	(pp_pass,		$,		"i"),
	(parse_pass,	$,		""),
	(type_pass,		$,		""),
	(pcl_pass,		$,		"pcl"),
	(runpcl_pass,	$,		"(int)"),
	(mcl_pass,		$,		"asm"),
	(nasm_pass,		$,		"nasm"),
	(asm_pass,		$,		"asm"),
	(mx_pass,		$,		"mx"),
	(obj_pass,		$,		"obj"),
	(dll_pass,		$,		"dll"),
	(exe_pass,		$,		"exe"),
	(run_pass,		$,		"(run)"),
end

byte cc_pass			!one of the above, default is link_pass
byte debugmode
ichar outfile			!base file
ichar outext="exe"
int cmdskip
int ttt

global byte fverbose=1			!0/1/2 = quiet/normal/extra
global byte fshowincludes=0

global byte dointheaders=1				!allow internal std headers
global byte highmem=1					!0/1/2 = normal/rip only/himem

byte fshowst
byte fshowstflat
byte fshowast
byte fshowpcl
byte fshowpst
byte fshowmcl
byte fshowss
byte fshowtypes
byte fshowfiles
byte fshowpaths
byte fshowheaders
byte fwriteheaders
byte fshowlog
byte fshowtiming
byte fgendll
byte fstdout
byte fshortnames
global byte fwriteerrors=1			!whether to writer $errors.tmp


ichar entrypointname

enumdata []ichar optionnames, []ref byte optvars, []byte optvalues =
	(load_sw,		"load",			&cc_pass,		load_pass),
	(pp_sw,			"e",			&cc_pass,		pp_pass),
	(ppi_sw,		"ei",			&cc_pass,		pp_pass),
	(parse_sw,		"parse",		&cc_pass,		parse_pass),
	(type_sw,		"type",			&cc_pass,		type_pass),
	(pcl_sw,		"p",			&cc_pass,		pcl_pass),
	(pcli_sw,		"pi",			&cc_pass,		pcl_pass),
	(runpcl_sw,		"i",			&cc_pass,		runpcl_pass),
	(mcl_sw,		"mcl",			&cc_pass,		mcl_pass),
	(asm_sw,		"s",			&cc_pass,		asm_pass),
	(asm2_sw,		"a",			&cc_pass,		asm_pass),
	(nasm_sw,		"nasm",			&cc_pass,		nasm_pass),
	(obj_sw,		"c",			&cc_pass,		obj_pass),
	(obj2_sw,		"obj",			&cc_pass,		obj_pass),
	(mx_sw,			"mx",			&cc_pass,		mx_pass),
	(dll_sw,		"dll",			&cc_pass,		dll_pass),
	(exe_sw,		"exe",			&cc_pass,		exe_pass),
	(run_sw,		"r",			&cc_pass,		run_pass),

!	(asm2_sw,		"asm",			&cc_pass,		asm_pass),

	(noregs_sw,		"noregs",		&fregoptim,		0),
	(nopeep_sw,		"nopeep",		&fpeephole,		0),
	(noopt_sw,		"no",			nil,			0),
!	(opt2_sw,		"o2",			&foptimise,		2),
!	(opt3_sw,		"o3",			&foptimise,		3),

	(paths_sw,		"paths",		&fshowpaths,	1),
	(headers_sw,	"headers",		&fshowheaders,	1),

	(inclpath_sw,	"incl",			nil,			1),
	(showincl_sw,	"includes",		&fshowincludes,	1),
!	(mh1_sw,		"mheaders",		&fmheaders,		'M'),
!	(mh2_sw,		"qheaders",		&fmheaders,		'Q'),

	(showst_sw,		"showst",		&fshowst,		1),
	(showstflat_sw,	"showstflat",	&fshowstflat,	1),
	(showast_sw,	"showast",		&fshowast,		1),
	(showpcl_sw,	"showpcl",		&fshowpcl,		1),
	(showpst_sw,	"showpst",		&fshowpst,		1),
	(showmcl_sw,	"showmcl",		&fshowmcl,		1),
	(showss_sw,		"showss",		&fshowss,		1),
	(showtypes_sw,	"showtypes",	&fshowtypes,	1),
	(showfiles_sw,	"showfiles",	&fshowfiles,	1),

	(time_sw,		"time",			&fshowtiming,	1),
	(time2_sw,		"time2",		&fshowtiming,	2),
	(v_sw,			"v",			&fverbose,		2),
	(vv_sw,			"vv",			&fverbose,		3),
	(quiet_sw,		"q",			&fverbose,		0),
	(csize_sw,		"cs",			&pverbose,		1),
	(size_sw,		"ss",			&pverbose,		2),
	(help_sw,		"h",			nil,			0),
	(help2_sw,		"help",			nil,			0),
	(ext_sw,		"ext",			&dointheaders,	0),
	(writeheaders_sw,"writeheaders",&fwriteheaders,	1),
	(out_sw,		"o",			nil,			0),
	(stdout_sw,		"stdout",		&fstdout,		1),
	(shortnames_sw,	"shortnames",	&fshortnames,	1),

	(norip_sw,		"norip",		&highmem,		0),
	(himem_sw,		"himem",		&highmem,		2),
end

const logfile="mcc.log"

int totallines=0
int nstringobjects=0

![sysparams.len]ichar extraparams	!after ":"
![sysparams.len]ichar extravalues
!int nextraparams=0

int startclock, loadtime, parsetime, pcltime, compiletime
int inittime

proc main=
	ichar file

	startclock:=os_clock()
PSTARTCLOCK:=STARTCLOCK
	
	starttiming()
	initdata()

	getinputoptions()

	initsearchdirs()

	if fverbose=3 then showsearchdirs() fi

	initlogfile()
	inittime:=gettiming()

	if fverbose then
		fprintln "Compiling # to #", inputfile, outfile
	fi

$PMODULENAME:=PCM_COPYHEAPSTRING(EXTRACTBASEFILE(INPUTFILE))

	do_loadmodule()

	do_preprocess()

	do_parsemodule()

	do_genpcl()

	case cc_pass
	when runpcl_pass then
		pcl_runpcl()

	when mcl_pass then
		do_genmcl()

!	when nasm_pass then
	when asm_pass then
		do_asm()

	when obj_pass then
		do_obj()
		

	when dll_pass then
		do_dll()

	when exe_pass then
		do_exe()

	when mx_pass then
		do_mx()

	when run_pass then
		do_run()

	else
		if cc_pass>=mcl_pass then
			println passnames[cc_pass],"not ready"
		fi
	esac

	if fverbose>=2 then
		println "Done."
	fi

	if fshowtiming then
		showtiming()
	fi

	closelogfile()

	stop 0
end

proc do_preprocess=
	if cc_pass=pp_pass then
		lex_preprocess_only(inputfile, outfile, fstdout)
		stop 0
	fi
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

	if fverbose=3 then
		CPL "Loading:",inputfile
	fi
	starttiming()

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

	strcpy(&.path,extractpath(inputfile))
	if path[1] then
		++nsearchdirs
		for i:=nsearchdirs downto 2 do
			searchdirs[i]:=searchdirs[i-1]
		od
		searchdirs[1]:=ref char(pcm_copyheapstring(&.path))
	fi

	loadtime:=gettiming()
end

proc do_parsemodule=
	int tt
	starttiming()
	parsemodule()
	parsetime:=(tt:=gettiming())
!	if fshowtiming=2 then fprintln "[parse #:#]",inputfiles[m]:"15jl",tt fi

!	showast()
end

proc do_genpcl=
	return unless cc_pass >= pcl_pass

	starttiming()
	codegen_pcl()
	pcltime:=gettiming()

	pcl_reducetest() when fregoptim or fpeephole

!	if fshowpcl then
!		println @logdev, pcl_writepcl(nil)
!
	if cc_pass=pcl_pass then			!need discrete file
		pcl_writepcl(outfile)
	fi

!	if fshowpst then
!		pcl_writepst()
!	fi

end

proc do_genmcl=
	return unless cc_pass >= mcl_pass

	if cc_pass=mcl_pass then			!need discrete file
		pcl_writeasm(outfile)
	fi
end

proc do_asm=
	return unless cc_pass >= asm_pass
	pcl_writeasm(outfile)
end

proc do_obj=
	return unless cc_pass = obj_pass
	pcl_writeobj(outfile)
end

proc do_dll=
	return unless cc_pass = dll_pass
	pcl_writedll(outfile)
end

proc do_exe=
	return unless cc_pass = exe_pass
	pcl_writeexe(outfile)
end

proc do_mx=
	return unless cc_pass = mx_pass
	pcl_writemx(outfile)
end

proc do_run=
	return unless cc_pass = run_pass
	pcl_exec()
end

proc initlogfile=
	if debugmode>=2 then
		remove(logfile)
		logdev:=cast(fopen(logfile,"w"))
	fi
end

proc closelogfile=
	[100]char str
	int pos

	return unless debugmode>=2


	if fshowmcl and cc_pass>=mcl_pass then
		println @logdev, "PROC ASM"
		println @logdev, pcl_writeasm(nil)
	fi

	if fshowpcl and cc_pass>=pcl_pass then
		println @logdev, "!PROC PCL"
		println @logdev, pcl_writepcl(nil)
		if fshowpst then
			pcl_writepst("PSYMTAB")

			addtolog("PSYMTAB", logdev)
		fi
	fi

	showast()

	if fshowst then
		showst("ST")
	fi

	if fshowstflat then
		showstflat("STFLAT")
	fi

	if fshowtypes then
		printmodelist(logdev)
	fi
!
	fclose(cast(logdev))
CPL "PRESS KEY"; STOP WHEN OS_GETCH()=27

	print @&.str,"\\m\\scripts\\med.bat ",logfile


	if checkfile("cc.m") then
		os_execwait(&.str,0,nil)
	else
		println "Diagnostic outputs written to",logfile
	fi
end

proc initdata=
	pcm_init()
	lexsetup()
	inittypetables()
	initcclib()

!init libfiles
	nlibfiles:=0
	libfiles[++nlibfiles]:="msvcrt"
	libfiles[++nlibfiles]:="gdi32"
	libfiles[++nlibfiles]:="user32"
	libfiles[++nlibfiles]:="kernel32"

	igetmsourceinfo:=cast(cgetsourceinfo)
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

!	if dointheaders=0 then
		searchdirs[++nsearchdirs]:="c:/cx/headers/"
!	fi

	searchdirs[++nsearchdirs]:=pcm_copyheapstring(extractpath(os_gethostname()))

	for i to nincludepaths when includepaths[i]^ do
		searchdirs[++nsearchdirs]:=includepaths[i]
	od


end

proc showsearchdirs=
	int i

	println "Include search paths:"
	if dointheaders then
		println "0: Internal standard headers (disable with -ext)"
	fi

	for i to nsearchdirs do
		if searchdirs[i]^ then
			println i,,":",searchdirs[i]
		else
			println i,,": ."
		fi
	od
	println
end

proc showast=

	if fshowast then
		printcode(logdev,"PROC AST")
		println @logdev
	fi
end

proc showstflat(ichar caption)=
	println @logdev,"PROC",caption
	printstflat(logdev)
	println @logdev
end

proc showst(ichar caption)=
	println @logdev,"PROC",caption
	printst(logdev, stmodule)
	println @logdev
end

proc showfiles=
	println "Sourcefiles:"

	for i:=1 to nsourcefiles do
		cpl i,":",sourcefilepaths[i],sourcefilenames[i],"Size:",sourcefilesizes[i]
	od
	println
end

proc showtime(ichar caption, int t)=
	fprintln "# # ms # %", caption:"12jl", t:"5", (t*100.0)/compiletime:"5.1jr"
end

proc showtiming=

	compiletime:=os_clock()-startclock

	showtime("Init:",		inittime)
	showtime("Load:",		loadtime)
	showtime("Parse:",		parsetime)
	showtime("PCL:",		pcltime)
	showtime("MCL:",		mcltime)
	showtime("SS:",			sstime)
	showtime("EXE:",		exetime)
	println "-----------------------------"
	showtime("Total:",		compiletime)
end

proc getinputoptions=
	const slash='-'
	int i,j,k
	int paramno,pmtype,sw,ncolons
	ichar name,value,ext

	paramno:=1
	ncolons:=0

	if pc_userunpcl then
		cc_pass:=runpcl_pass
		fverbose:=0
	fi

	if eqstring(extractfile(os_gethostname()),"cs.exe") then
!		msfile:=1
		fverbose:=0
		do_option(run_sw, "")
	fi

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

			if cc_pass in [run_pass, runpcl_pass] then
				cmdskip:=paramno-1+$cmdskip
				pci_target:=cc_pass=runpcl_pass
				exit
			fi
		when pm_libfile then
			if nlibfiles>=maxlibfile then
				loaderror("Too many lib files")
			fi
			libfiles[++nlibfiles]:=pcm_copyheapstring(name)
!		when pm_colon then
!			if ++ncolons>1 then
!				name:=":"
!				value:=nil
!		!		recase pm_extra
!				goto doextra
!			fi
		when 0 then
			exit
		esac
	od

!CPL INPUTFILES[1]

	if cc_pass=0 then cc_pass:=exe_pass fi
	if cc_pass in [dll_pass,obj_pass, nasm_pass] then
		highmem:=2
	elsif cc_pass in [mx_pass,  run_pass] then
		highmem:=0
	fi
	outext:=extnames[cc_pass]

	if inputfile=nil and not fwriteheaders then
		showcaption()
		println "Usage:"
		println "    ",,cmdparams[0],"   prog[.c]          Compile prog.c to prog.exe"
		println "    ",,cmdparams[0],"-r prog[.c]          Compile prog.c and run"
		println "    ",,cmdparams[0],"-i prog[.c]          Compile prog.c and interpret"
		println "    ",,cmdparams[0],"-help                Show all options"
		stop 1
	fi

	if fwriteheaders then
		writeheaders()
		stop 20
	fi

	if outfile=nil then
		outfile:=pcm_copyheapstring(changeext(inputfile, outext))
	fi

	pcl_setflags(highmem:highmem, shortnames:fshortnames)
	pcl_cmdskip(cmdskip)
end

proc do_option(int sw, ichar value)=
	[300]char str
	int length
	ref byte p

	p:=optvars[sw]
	if p then
		p^:=optvalues[sw]
!
		if sw in load_sw..mcl_sw then
			debugmode ior:=1
		fi

		if sw in showst_sw..showfiles_sw then
			debugmode ior:=2
		fi

		if sw=runpcl_sw and inputfile then
			loaderror("-RUNP OUT OF ORDER")
		fi
		if sw in [ppi_sw, pcli_sw] then
			pci_target:=1
		fi

!		if sw in exe_sw..ml_sw then
!			outext:=optionnames[sw]
!		fi
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
			strcpy(&.str,value)
			strcat(&.str,"/")
			value:=&.str
		esac

		includepaths[++nincludepaths]:=pcm_copyheapstring(value)

	when help_sw,help2_sw then	showhelp()

	when out_sw then
		outfile:=pcm_copyheapstring(addext(value,outext))

	when noopt_sw then
		fregoptim:=fpeephole:=0

	esac
end

proc showincludepaths=
	println "Include paths",nincludepaths
	for i to nincludepaths do
		println i,includepaths[i]
	od
	println
end

proc showhelp=
	showcaption()
	println strinclude "cc_help.txt"

	stop 23
end

proc showextrainfo=
	static ichar infotext=strinclude "info.txt"

	println infotext

	stop 24
end

proc showcaption=
	println "CC C Compiler",$date,$time
end

proc starttiming =
	ttt:=os_clock()
end

func gettiming:int=
	os_clock()-ttt
end
=== cc_decls.m 0 0 27/79 ===
import clib
global type unit = ref unitrec
global type symbol = ref strec

global const maxmodule=200
global const maxlibfile=200
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
!		ref strec paramlist
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
	byte flags:(addrof:1, varparams:1, flmacro:1, ismain:1)
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
!global const int maxtype=20'000
!global const int maxtype=60'000
global const int maxtype=80'000

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
global [0:maxtype]byte	ttisblock
global [0:maxtype]byte	ttsigned			!set up in genmcl
global [0:maxtype]i32	ttshared			!no. of shared instances
global [0:maxtype]ref paramrec ttparams		!for modes involving function pointers
global [0:maxtype]ref strec tttypedef

global int trefchar							!set to to char* type
global int trefwchar						!set to to wchar* type

global ichar inputfile
global int mainfileno
global [0..maxlibfile]ichar libfiles
global [0..maxsourcefile]ichar sourcefilenames
global [0..maxsourcefile]ichar sourcefilepaths
global [0..maxsourcefile]ichar sourcefiletext
global [0..maxsourcefile]i32 sourcefilesizes

global int nsourcefiles
global int nlibfiles

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
global ref unitrec nullunit

global int clineno=0		!set in codegen scanner
global int cfileno=0		!set in codegen scanner

global tokenrec lx				!provides access to current token data
global tokenrec nextlx

!global int tlex=0		!timing

global int debug=0

!global int hstsize	= 16384
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

global ichar mclstr
global int mclstrlen

global int nunits

global const maxpmodule = maxmodule-1
global const maxpheader = 100
global const maxplib = 100

global [maxpmodule]ichar pmodulelist
global [maxpheader]ichar pheaderlist
global [maxplib]ichar pliblist

global int npmodules
global int npheaders
global int nplibs

global byte pci_target			!1 when using -runp

!GLOBAL INT NALLCALLS
!GLOBAL INT NUSESTACK
!GLOBAL INT NUSEMIXEDSTACK
=== cc_tables.m 0 0 28/79 ===
global enumdata [0:]ichar stdtypenames, [0:]byte stdtypewidths,
		 [0:]byte stdsigned, [0:]byte stdpcl, [0:]byte stdsize =

!                              bts  si  pcl      size
	(tvoid=0,		"void",		0,	0,	tpvoid,		0),

	(ti8,			"i8",		8,	1,	tpi8,		1),		! This ordering is important
	(ti16,			"i16",		16,	1,	tpi16,		2),		!
	(ti32,			"i32",		32,	1,	tpi32,		4),		!
	(ti64,			"i64",		64,	1,	tpi64,		8),		!

	(tbool,			"bool",		8,	0,	tpu8,		1),		! As is this
	(tu8,			"u8",		8,	0,	tpu8,		1),		!
	(tu16,			"u16",		16,	0,	tpu16,		2),		!
	(tu32,			"u32",		32,	0,	tpu32,		4),		!
	(tu64,			"u64",		64,	0,	tpu64,		8),		!

	(tr32,			"r32",		32,	0,	tpr32,		4),		! And tr32 must be >= integer types
	(tr64,			"r64",		64,	0,	tpr64,		8),		!

	(tenum,			"enum",		0,	0,	tpi32,		0),		!
	(tref,			"ref",		64,	0,	tpu64,		0),		! 
	(tproc,			"proc",		64,	0,	tpvoid,		0),		!
	(tlabel,		"label",	64,	0,	tpvoid,		0),		!
	(tblock,		"block",	0,	0,	tpblock,	0),		!

	(tarray,		"array",	0,	0,	tpblock,	0),		!
	(tstruct,		"struct",	0,	0,	tpblock,	0),		!
	(tunion,		"union",	0,	0,	tpblock,	0),		!

	(tnotset,		"notset",	0,	0,	tpvoid,		0),		!

!User-defined types go here
	(tlast,			$,			0,	0,	tpvoid,		0)		! 	!

end

global const tchar=ti8
global const tfirstnum=ti8, tlastnum=tr64
global const tfirstint=ti8, tlastint=tu64
global const tfirstreal=tr32, tlastreal=tr64

global const tptroffset = ti64		!for 64-bit target

global enumdata [0:]ichar catnames =
    (voidcat=0,     $),         ! Not set

    (intcat,        $),         ! i32 i64 u32 u64
    (realcat,       $),         ! r32 r64
    (shortcat,      $),         ! i8 i16 u8 u16
    (blockcat,      $),         ! u64 pointer to block data
end


global enumdata []ichar typespecnames, []i32 typespectypes, []byte typespecsizes =
	(ts_void,		$,	tvoid,		0),
!	(ts_char,		$,	tu8,		1),
	(ts_char,		$,	ti8,		1),
	(ts_short,		$,	0,			2),
	(ts_long,		$,	0,			4),
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

!Call conventions
global enumdata []ichar cccnames=

	(open_cc=0,		$), ! Not set: either own or clang, depending on whether fn was defined
	(own_cc,		$), ! Internal (x86/x64)
	(clang_cc,		$), ! External (all x64; clang only x86)
	(stdcall_cc,	$), ! (x86 non-clang)
	(callback_cc,	$), ! Internal when called from External

	(dummy_cc,		$)	! 
end

global enumdata [0:]ichar linkagenames=
	(none_ss=0,		$),
	(static_ss,		$),
	(auto_ss,		$),
	(register_ss,	$),
	(extern_ss,		$),
	(typedef_ss,	$)
end

global enumdata []ichar typequalnames=
	(const_qual,	$),
	(volatile_qual,	$),
	(restrict_qual,	$),
	(atomic_qual,	$)
end

global enumdata []ichar fnspecnames=
	(inline_fnspec,		$),
	(noreturn_fnspec,	$),
	(callback_fnspec,	$),
end

global enumdata =
	pdm_date,
	pdm_time,
	pdm_file,
	pdm_line,
	pdm_func,
	pdm_cdecl,
	pdm_mcc,
	pdm_mcci,
	pdm_stdc
end

global enumdata [0:]ichar jtagnames=

	(jnone=0,		$), !
	(jconst,		$), !
	(jnull,		$), !
	(jname,		$), !
!	(jnameaddr,	$), !
	(jwidenmem,	$), !
	(jfuncname,	$), !
	(jblock,		$), !
	(jtempdecl,	$), !
	(jdecl,		$), !
!	(jtypeof,		$), !
!	(jmakeref,		$), !

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
	(jgt,			">"), ! a 
	(jge,			">="), ! a 

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
!	(jdotref,		$), ! 
	(jindex,		$), ! 

	(jptr,			"ptr"), ! 
!	(jptroffset,	"ptroffset *"), ! 
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
!	(placeholdersym,	$,	"<PH>",	0),			!
	(placeholdersym,	$,	"",	0),			!

!Second half are tokens that can be yielded after a name lookup:
	(namesym,			$,	"k",	0),			! identifier symbol
	(ksourcedirsym,		$,	"k",	0),			! 
	(predefmacrosym,	$,	"k",	0),			! __LINE__ etc

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
!	(strincludedir,	$),
	(pragmadir,		$)
end

global enumdata [0:]ichar namespacenames=
	(ns_none=0,		$),			!not set
	(ns_general,	$),			!variables, functions, typedefs, enum names
	(ns_tags,		$),			!struct, union, enum tags
	(ns_labels,		$),			!label names
	(ns_fields,		$)			!field names
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

	("int8",		kstdtypesym,	ti8),
	("int16",		kstdtypesym,	ti16),
	("int32",		kstdtypesym,	ti32),
	("int64",		kstdtypesym,	ti64),

	("uint8",		kstdtypesym,	tu8),
	("uint16",		kstdtypesym,	tu16),
	("uint32",		kstdtypesym,	tu32),
	("uint64",		kstdtypesym,	tu64),

	("__DATE__",	predefmacrosym,	pdm_date),
	("__FILE__",	predefmacrosym,	pdm_file),
	("__LINE__",	predefmacrosym,	pdm_line),
!	("__STDC__",	predefmacrosym,	pdm_stdc),
	("__TIME__",	predefmacrosym,	pdm_time),
!	("__cdecl",		predefmacrosym,	pdm_cdecl),
	("__MCC__",		predefmacrosym,	pdm_mcc),
	("__MCCI__",	predefmacrosym,	pdm_mcci),
	("__func__",	predefmacrosym,	pdm_func),
	("__FUNCTION__",	predefmacrosym,	pdm_func),

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

global []int badexprs=(
jconst,
jname,
jifx,
jandl,
jorl,
jnotl,
jistruel,
jexprlist,
jandand,
jeq,
jne,
jlt,
jle,
jge,
jgt,
jadd,
jsub,
jmul,
jdiv,
jrem,
jiand,
jior,
jixor,
jshl,
jshr,
jdot,
jidot,
jindex,
jptr,
jaddptr,
jsubptr,
jneg,
jabs,
jinot)

=== cc_lex.m 0 0 29/79 ===
! (C tokeniser module)
ref tokenrec tkptr=nil

int dowhitespace=0

GLOBAL int NINCLUDES

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

ref strbuffer destcopy
!const int maxpastedtokens=7000
const int maxpastedtokens=87000
[maxpastedtokens]ichar pastedtokenlist
int npastedtokens=0
int isincludefile=0				!whether readng include file name

int firstsymbol=1
ref byte reallxsptr

global int nhstsymbols
int hstthreshold				!limit above which new hst should be generated

global proc lex_preprocess_only(ichar infile, outfile, int toconsole=0)=
	ref char psource
	int ntokens,nlines,fileno,size
	int LENGTH
	i64 nchars,t,hashtot,symtot
	real tsecs
	static strbuffer sbuffer
	static ref strbuffer dest=&sbuffer
	filehandle f

	dowhitespace:=1
	fileno:=loadsourcefile(infile,infile)

	psource:=cast(sourcefiletext[fileno])

	size:=sourcefilesizes[fileno]

	nlines:=ntokens:=0
	hashtot:=symtot:=0
	t:=os_clock()

	destcopy:=dest
	gs_init(dest)

	lxsptr:=psource
	lxstart:=lxsptr
	nextlx.lineno:=1
	setfileno(1)
	ifcondlevel:=0

	stacksourcefile(mcchdr,1)

	nextlx.symbol:=eolsym

	repeat
		lexm()
		++ntokens

!		if showtokens then
			emittoken(&nextlx,dest)
!		fi

	until nextlx.symbol=eofsym

	if ifcondlevel then
		lxerror("#endif missing")
	fi

	if showtokens then
		if toconsole then
			gs_println(dest,nil)
		else
			f:=fopen(outfile,"wb")
			gs_println(dest,f)
			fclose(f)
		fi
	fi
end

global proc lexreadtoken=
!read next token into nextlx
!	int c,csum,hsum,dodir
	word c,csum,hsum,dodir
	ref char p,ss
	ichar searchstr

	nextlx.subcode:=0
	nextlx.flags:=0

!	while lxsptr^=' ' do ++lxsptr od
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

!		ss:=pcm_alloc(nextlx.length+1)		!zero-term in lex(), as headers may need to be
!		memcpy(ss,lxsvalue,nextlx.length)	!re-tokenised
!		(ss+nextlx.length)^:=0
!		lxsvalue:=ss

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
		switch lxsptr^
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
		end switch					!else assume just zero	

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
		switch lxsptr^
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
		when '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
			readrealnumber(lxsptr,lxsptr,0,10)
			return
		else
			nextlx.symbol:=dotsym
			return
		end switch

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
		switch lxsptr^
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
		end switch
		return

	when '>' then
		switch lxsptr^
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
		end switch
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
			doswitch (lxsptr++)^
			when ' ',tab then
			else
				--lxsptr
				exit
			end
!			while spacemap[(++lxsptr)^] do od
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

	when 0xEF then			!BOM
		lxsptr+:=2

	else
		if 128<=(lxsptr-1)^<= 255then goto doname fi

		PRINTLN "ERROR CHAR",(lxsptr-1)^,int((lxsptr-1)^),lx_stackindex
		lxerror("ERROR CHAR")
		nextlx.symbol:=errorsym
		return

	end doswitch

end

proc readrealnumber(ref char pstart,intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, (or to "." if there was no prefix, then intlen=0)
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
	ref char fractstart
	int fractlen,expon,i,c,badexpon,n,adj
	real basex,x,expbase,f,y,y2,g
	i64 aa,cc,pref
	const maxrealdigits=500
	[maxrealdigits+12]char realstr
	ref char rs
	[32]char expstr
	u64 xx1,xx2

	if base<>10 then
		old_readrealnumber(pstart,intstart,intlen,base)
		return
	fi

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

	if base=16 then
		realstr[1]:='0'
		realstr[2]:='x'
		rs:=&realstr[3]
		pref:=2
	else
		rs:=&realstr[1]
		pref:=0
	fi

	if intlen+fractlen>maxrealdigits then
		lxerror("Real too long")
	fi
	if intlen then
		memcpy(rs,intstart,intlen)
	fi
	if fractlen then
		memcpy(rs+intlen,fractstart,fractlen)
	fi

	expbase:=basex:=base

	if base=10 then
		expon-:=fractlen
	else
		expon-:=fractlen*4				!each hex digit is 4 binary bits
		expbase:=2.0
	fi

	realstr[pref+intlen+fractlen+1]:=0

	print @&.expstr,(base=10|"e"|"p"),,expon

	strcat(&.realstr,&.expstr)
	if base<>10 then
		lxerror("Non-base-10 floats temporarily unavailable")
	fi

	x:=strtod(&.realstr,nil)

	nextlx.symbol:=realconstsym
	nextlx.subcode:=tr64
	nextlx.xvalue:=x

	setnumberoffset(intstart-lxstart)
	nextlx.length:=lxsptr-intstart
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
	println
	println
!	os_getch()
	stop 11
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

global proc lexsetup=
!do one-time setup:
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i

	inithashtable()
	fillhashtable()

	for i:=0 to 255 do
		switch i
!		when 'A'..'Z','a'..'z','$','_','0'..'9' then
		when 'A'..'Z','a'..'z','$','_','0'..'9', 128..255 then
			alphamap[i]:=1
		end
		switch i
		when '0'..'9' then
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

!++NCLASHES

		if ++j>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	if nhstsymbols>=hstthreshold then
		newhashtable()

		lxhashvalue:=gethashvalue(lxsvalue,nextlx.length)
		goto retry
	fi

!	nextlx.symptr.name:=lxsvalue
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
			p:=&.filename

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
			strcpy(&.filename,nextlx.svalue)
		else
			lxerror("include?")
		fi
		lexm()

	IF FSHOWINCLUDES THEN
		PRINTLN "INCLUDE",&.filename,"FROM",sourcefilepaths[getfileno()],nextlx.lineno,
			=nsourcefiles
	FI
	++NINCLUDES

		stacksourcefile(&.filename,syshdr)

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

global proc PS(ichar caption)=
	print caption,,":::"
	printsymbol(&lx)
end

global proc PSNEXT(ichar caption)=
	print caption,,":##"
	printsymbol(&nextlx)
end

global function gethashtablesize:int=
	int i,n

	n:=0
	for i:=0 to hstmask do
		if hashtable^[i].name then
			++n
		fi
	od

	return n
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

!	if aa>=i32.max then
!		nextlx.subcode:=ti64
!	else
!		nextlx.subcode:=ti32
!	fi

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

	doswitch c:=lxsptr++^
	when 'L','l','u','U' then
!	lxerror("Numeric SUFFIX")
	else
		if alphamap[c] then
!*!		lxerror("Bad number suffix")
		fi
		--lxsptr
		exit
	end doswitch

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
		strcpy(&.fullpath,headerpathlist[lx_stackindex-1])
	fi

	if headerpath[1] then
		strcat(&.fullpath,pcm_copyheapstring(&.headerpath))
	fi

	headerpathlist[lx_stackindex]:=pcm_copyheapstring(&.fullpath)

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

	strcpy(&.filespec,file)
	convlcstring(&.filespec)

!check to see if already loaded
	for i:=1 to nsourcefiles do
		if eqstring(&.filespec,sourcefilenames[i]) then
			return i
		fi
	od

!see if a builtin header

	if dointheaders then
		hdrtext:=findheader(&.filespec)
		if hdrtext then
			return loadbuiltin(&.filespec,hdrtext)
		fi
	fi

	if eqstring(file, mcchdr) then
		return loadbuiltin(filespec, strinclude(mcchdr))
	fi

	strcpy(&.headerpath,extractpath(file))

	if headerpath[1] then
		if headerpath[1]='/' or headerpath[2]=':' and headerpath[3]='/' then
			if checkfile(file) then
					return loadsourcefile(file,file)
			fi
			return 0			!don't both looking anywhere else
		fi
	fi

	for i:=lx_stackindex downto 1 do
		strcpy(&.filespec,headerpathlist[i])
		strcat(&.filespec,file)

		if checkfile(&.filespec) then
			return loadsourcefile(&.filespec,file)
		fi
	od

	for i to nsearchdirs do
		strcpy(&.filespec,searchdirs[i])
		strcat(&.filespec,file)

		if checkfile(&.filespec) then
			strcpy(&.headerpath,extractpath(&.filespec))
			return loadsourcefile(&.filespec,file)
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

proc shownumberstr(ref tokenrec l,filehandle f=nil)=
	ref char s

	if getfilenox(l) then
		s:=sourcefiletext[getfilenox(l)]+getnumberoffsetx(l)
	else
		s:=pastedtokenlist[l.pasteno]
	fi
	printstrn(s,l.length,f)

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
		dest:=&.str
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
		memcpy(nextlx.svalue,&.str,length+1)
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

proc showtokens(ichar caption,ref tokenrec tk)=
	print caption,,"<"
	while tk do
		showtoken(tk)
		tk:=tk.nexttoken
	od
	println ">"
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
!repeat lexreadtoken() until nextlx.symbol=eolsym

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
!CPL "EXPAND PDM 2"
!				expandpredefmacro(m.subcode,expandtk,nextlx.lineno)
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
	convertstringc(s,u,length)
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
!lxerror("Undefined macro name in cpp expr")
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

proc freetokens(ref tokenrec tk)=
	ref tokenrec nexttk

	while tk do
		nexttk:=tk.nexttoken
		tk:=nexttk
	od
end

global proc fastreadtoken=
!read next token into nextlx
	int c,csum,hsum,commentseen,dodir,j
	ref char pstart,p
	ichar ss

!	nextlx.subcodex:=0

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

		fprint @&.str, "#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

		tk.symbol:=stringconstsym
		tk.svalue:=pcm_copyheapstring(&.str)

	when pdm_time then
		os_getsystime(&tm)

		fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

		tk.symbol:=stringconstsym
		tk.svalue:=pcm_copyheapstring(&.str)
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
	when pdm_mcci then
		tk.symbol:=intconstsym
		tk.value:=pci_target
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
		elsif memcmp(nextlx.symptr.name,"module",6)=0 then
			addbuildinfo('M')
!		elsif memcmp(nextlx.symptr.name,"except",6)=0 then
!			addbuildinfo('X')
		elsif memcmp(nextlx.symptr.name,"header",6)=0 then
			addbuildinfo('H')
		elsif memcmp(nextlx.symptr.name,"link",4)=0 then
			addbuildinfo('L')
		fi
	fi
finish:
	while nextlx.symbol<>eolsym and nextlx.symbol<>eofsym do lexm() od
end

proc addbuildinfo(int code)=
	ichar file
	int j

	lexm()
	if nextlx.symbol<>stringconstsym then lxerror("Str expected") fi
	file:=pcm_copyheapstring(nextlx.svalue)

	case code
	when 'M' then
		if npmodules>=maxpmodule then lxerror("TMM")  fi
		pmodulelist[++npmodules]:=file

	when 'H' then
		if npheaders>=maxpheader then lxerror("TMH")  fi
		pheaderlist[++npheaders]:=file
	when 'L' then
		if nplibs>=maxplib then lxerror("TMLM") fi
		pliblist[++nplibs]:=file
	esac
	lexm()
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

function getfileno:int=
	return (nextlx.numberoffset>>24)<<8 ior nextlx.fileno
end

function getfilenox(ref tokenrec tk)int=
	return (tk.numberoffset>>24)<<8 ior tk.fileno
end

function getnumberoffsetx(ref tokenrec tk)int=
	return tk.numberoffset iand 0xFFFFFF
end

global proc freehashtable=
!free the user name entries in the hash table
!leave reserved words etc alone
	ref strec d,e,f

	for i:=0 to hstmask do
		d:=hashtable^[i]
		if d.name and d.symbol=namesym then
			if d.nameid=macroid then
				freetokens(d.tokenlist)
			fi
			f:=d.nextdupl
			while f do
!				freestentry(f)
				e:=f.nextdupl
				pcm_free(f,strec.bytes)
				f:=e
			od
			pcm_clearmem(hashtable^[i],strec.bytes)
		elsif d.name then
			d.nextdupl:=nil
		fi
	od
end

proc regenlookup(ref strec d)=
	int j, wrapped,length
	ref strec e

	j:=gethashvalue(d.name,d.namelen) iand hstmask
	wrapped:=0

	do
		e:=hashtable^[j]
		length:=e.namelen

		if not length then
			PCM_FREE(HASHTABLE^[J],STREC.BYTes)
			hashtable^[j]:=d
			++nhstsymbols
			return
		fi

		if length=d.namelen then	!match on length
			if memcmp(e.name,d.name,length)=0 then	!match
				lxerror("regenhst dupl?")
			fi
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("REGENHST FULL?")
			fi
			wrapped:=1
			j:=0
		fi
	od
end

proc newhashtable=
	ref[0:]ref strec oldhashtable
	int oldhstsize
	ref strec d

!remember old hst
	oldhashtable:=hashtable
	oldhstsize:=hstsize
!generate new, blank hst
	hstsize*:=2
	hstmask:=hstsize-1
	nhstsymbols:=0
	hstthreshold:=(6*hstsize)/10

	hashtable:=pcm_alloc(hstsize*(ref void.bytes))

	for i:=0 to hstmask do
		hashtable^[i]:=pcm_allocz(strec.bytes)
	od

!now, rehash all existing hashentries
	for i:=0 to oldhstsize-1 do
		d:=oldhashtable^[i]
		if d.name then
			regenlookup(d)
		fi
	od

	pcm_free(oldhashtable,oldhstsize*(ref void.bytes))
end

proc old_readrealnumber(ref char pstart,intstart, int intlen, base)=
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

global function issimpleconstmacro(ref strec m)int=
!return 1 if a d is a macro defined as simple int or float
!then it will not be expanded
	ref tokenrec tk
	static []ichar specialnames=("stdin","stdout","stderr")

!don't expand special names
	for i to specialnames.len do
		if eqstring(specialnames[i],m.name) then
			return 2
		fi
	od

	tk:=m.tokenlist

	if tk and tk.nexttoken=nil then
		if tk.symbol=intconstsym or tk.symbol=realconstsym then
			return 1
		fi
	fi
	return 0
end
=== cc_parse.m 0 0 30/79 ===
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
	int t,nitems,wasenum

	while lx.symbol<>eofsym do
		nitems:=0
		case lx.symbol
		when semisym then
			serror("Extra semicolon 2")
		esac
		wasenum:=lx.symbol

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
			elsif pm then
	readfn:
				if lx.symbol=lcurlysym and commaseen then serror("fn def after comma") fi

				d:=readfunction(d,m,linkage,pm,wasdef)
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

	if fverbose=3 then println "Parsing:",inputfile fi

	loopindex:=ingeneric:=0
	ist_symptr:=nil
	memset(&casevaluestack,0,casevaluestack.bytes)

	startlex("PARSETEST",mainfileno)
	owner:=stmodule
	currproc:=nil
	loopindex:=0

	lex()

!!=========================================
!t:=os_clock()
!int ntokens:=0
!
!!	repeat
!!		lex()
!!		++ntokens
!!	until lx.symbol=eofsym
!!
!	repeat
!		lexreadtoken()
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
				t:=(d.isunsigned|tu32|ti32)
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

	switch lx.symbol
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
	end switch
	return 0

end

function istypestarter_next:int=
!return 1 when current symbol could start a type-spec
	ref strec d

	switch nextlx.symbol
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
	end switch
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

	switch opc:=lx.symbol
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
	end switch

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
		    p.slength:=slength
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
		p:=createincrop(jpostincr,p)

	when decrsym then
		lex()
		p:=createincrop(jpostdecr,p)

	else
		exit
	end doswitch

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
			d:=readfunction(d,m,linkage,pm,wasdef)
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

function readfunction(ref strec d, int m, linkage, ref paramrec pm, int &wasdef)ref strec=
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

!CPL "C/PTR",STRMODE(M)
!CPL "C/PTR",STRMODE(P.MODE)
!CPL "C/PTR",STRMODE(TTTARGET[P.MODE])
!
	case p.tag
	when jaddrof then
		q:=p.a
!		if p.alength then
			q.mode:=tttarget[p.mode]
!		ELSE
!			q.mode:=tttarget[p.mode]
!		FI
!CPL "DONE", STRMODE(Q.MODE)
		fixmemopnd(q)
!CPL "DONE", STRMODE(Q.MODE)
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

!RETURN 0
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
!		p.uvalue:=p.xvalue
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
!elsif aparams>nparams and pm.flags<>pm_variadic then
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

	if ttsize[currrecord.mode] in [1,2,4,8] then ttisblock[currrecord.mode]:=0 fi

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

!CPL "DOCAST", STRMODE(S), STRMODE(T), S, T

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

	switch p.tag
	when jname then
		if p.alength then
			size:=ttsize[p.def.mode]/p.alength			!take account of array

		else
			size:=ttsize[p.def.mode]			!take account of array
		fi
	when jconst then
		case t
		when trefchar then					!const string
			size:=p.slength+1
		when trefwchar then
			size:=(p.wslength+1)*2
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
	end switch

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
=== cc_genpcl.m 0 0 31/79 ===

global int retindex
global int initstaticsindex

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit
!symbol dcmdskip

int nvarlocals, nvarparams

macro divider = gencomment("------------------------")

global proc codegen_pcl=
!generate code for module n
	symbol d,e
	ref procrec pp

	if fverbose=3 then println "GenPCL:",inputfile fi

	pcl_start(nil, nunits)

	dolibs()

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
!	gencomment("Procs:")

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

!INT NLOC:=0, NPARAMS:=0
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

	do_stmt(p.code)

	divider()

	definefwdlabel(retindex)

	if ismain then
		pc_gen(kload, genint(0))
		setmode(ti32)
		pc_gen(kstop)
	fi

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
				pc_gen(kdata, genstring(p.svalue))

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
				strcpy(&.str,"`")
				if d.scope=function_scope then
					strcat(&.str,currproc.name)
					strcat(&.str,",")
				fi
				strcat(&.str,d.name)
				strcat(&.str,"+")

				getstrint(b.value, &.str2)

				strcat(&.str,&.str2)
				pc_gen(kdata, genname(&.str))
			else
				gerror("Add/Idata &frame")
			esac	
		elsif a.tag=jconst and b.tag=jconst and ttbasetype[a.mode]=tref then		!ASSUME REF+REF
			print @&.str,a.value,,"+",,b.value
			pc_gen(kdata, genname(&.str))

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

proc pushint(int a)=
	pc_gen(kload, genint(a))
end

proc dolibs=
	for i to nlibfiles do
		pc_addplib(libfiles[i])
	od
end

=== cc_blockpcl.m 0 0 32/79 ===
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

const maxparams=64

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
!CPL "DXEXPR", JTAGNAMES[P.TAG], =am

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
			pc_gen(ktoboolt)
			setmode(tu32)
			setmode2(a.a.mode)
		else
			dx_expr(a)
			pc_gen(knot)
			setmode_u(a)
		fi

	when jistruel then
		dx_expr(a)
		pc_gen(ktoboolt)
		setmode(tu32)
		setmode2(a.mode)

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
		dx_binto(a, b, kdivto, 1)

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
			pc_gen(kload, genstring(p.svalue))
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
!	dx_expr(b)
!	if res then pc_gen(kdupl) fi
!	dx_expr(a, 1)
!	pc_gen(getopc(opc, a))
!	setmode(getmemmode(a))
end

proc do_assign(unit a, b, int res)=

	do_fixwiden(a, b)

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

	switch p.tag
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
	end switch
end

proc gcomparejump(int jumpopc, unit p, lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int cond

	cond:=getpclcond(p.tag)			!jeq => keq etc
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	do_fixwiden(lhs, rhs)

	dx_expr(lhs)
	dx_expr(rhs)

	pc_gen(kjumpcc, genlabel(lab))
	pccurr.condcode:=cond
	setmode_u(lhs)

end

function getpclcond(int op)int=
	case op
	when jeq then return eq_cc
	when jne then return ne_cc
	when jlt then return lt_cc
	when jle then return le_cc
	when jge then return ge_cc
	when jgt then return gt_cc
	esac
	return 0
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

		if currproc.ismain then
			dx_expr(a)
			pc_gen(kstop)
		else
			dx_expr(a)
			pc_gen(kjumpret, genlabel(retindex))
			setmode_u(a)
		fi
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

!		isfn:=tttarget[m]<>tvoid and res
		isfn:=tttarget[m]<>tvoid
		pm:=ttparams[m]
		isfnptr:=1

	else
		pm:=a.def.paramlist
		isfnptr:=0
!		isfn:=a.def.mode<>tvoid and res
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

!++NALLCALLS
!IF FPARAMS>8 OR IPARAMS>8 THEN
!!CPL D.NAME
!	for i to nparams do
!		q:=paramlist[i]
!		if q.mode in [tr32, tr64] then
!			CP "R "
!		else
!			CP "I"
!		fi
!	od
!	CPL
!	++NUSESTACK
!FI
!IF FPARAMS>8 AND IPARAMS>8 THEN
!	++NUSEMIXEDSTACK
!FI


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

	do_fixwiden(a, b)

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
!CPL "CASE", I, VALUETABLE[I]
			labeltable[i]:=createfwdlabel()
!			if i<ncases then
!				pc_gen(kdouble)
!			fi
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

proc do_fixwiden(unit a, b)=
!if both widen to same type, then remove the widen
RETURN

!	if a.memmode and a.memmode=b.memmode and a.mode=b.mode then		!widening both
!		a.mode:=a.memmode
!		b.mode:=b.memmode
!		a.memmode:=tvoid
!		b.memmode:=tvoid
!	fi
end
=== cc_libpcl.m 0 0 33/79 ===
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

=== cc_lib.m 0 0 34/79 ===
global int autotypeno=0
global int nextafindex=0

const int unitheapsize=50000
ref unitrec unitheapptr=nil
int remainingunits=0

function newstrec:ref strec=
	ref strec p
	p:=pcm_alloc(strec.bytes)
!	memset(p,0,strec.bytes)
	clear p^

	p.lineno:=lx.lineno
	p.fileno:=lx.fileno

	return p
end

global proc initcclib=

end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=jname
	u.def:=p

	return u
end

global function createunit0(int tag)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q
	return u
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
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.svalue:=s
	u.mode:=trefchar
	if length=-1 then
		u.slength:=strlen(s)
	else
		u.slength:=length
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
!if length=-1 then
!	u.slength:=strlen(s)
!else
		u.wslength:=length
!fi
	u.iswstrconst:=1
	return u
end

global function getoptocode(int opc)int=		!GETOPTOCODE
!opc is kadd etc
!return matching kaddto, etc
	static [0:jtagnames.len]i16 opctotable
	int n,opcto,i
	[20]char str

	opcto:=opctotable[opc]
	if opcto then return opcto fi				!find

!assume memoising table not filled in for this opc

	strcpy(&.str,jtagnames[opc])					!"add" etc
	strcat(&.str,"to")							!"addto" etc

	for i:=0 to jtagnames.upb do
		if eqstring(jtagnames[i],&.str) then
			opctotable[opc]:=i
			return i
		fi
	od

	cpl jtagnames[opc]
	serror("Can't find -to version")
	return 0
end

global function getconstvalue(ref unitrec p,int ID=0)i64=	!GETCONSTVALUE
!extract value from kconst
	if p and p.tag=jconst then
		return p.value
	fi
	serror("GCV Not constant")
	return 0
end

global function nextautotype:ichar=
	static [32]char str

!sprintf(&.str,"$T%d",i32(++autotypeno))
	print @&.str,"$T",,++autotypeno
	return &.str
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
!	ttcat[newm]:=ttcat[m]

	return newm
end

global function createrefmode(int m)int=
!create ref version of mode m (including when m is already a ref)
	int newm

	if ttreftype[m] then
		++ttshared[ttreftype[m]]
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
	ttisblock[newm]:=1

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
	ttisblock[newm]:=1

	return newm
end

global proc setnameptr(ref unitrec p)=		!SETNAMEPTR
!p is a just created j...def unit which has a nameptr in the .a parameter
!set up an xref from the strec back to the -def unit
!Set up a pointer in the associated strec that points back to q

	p.def.code:=p
end

global function getautofieldname:ref strec=
!create auto-field name and return pointer to st entry
	[32]char str
	ichar name

!sprintf(&.str,"$F%d",i32(++nextafindex))
	print @&.str,"$F",,++nextafindex

	name:=pcm_copyheapstring(&.str)
	return addnamestr(name)
end

global func convertstringc(ichar s, t,int length=-1)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
	int c
	[20]char str
	ichar t0

	if length=-1 then
		length:=strlen(s)
	fi

	t0:=t

	to length do
		c:=s++^
		switch c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when '\'' then
			t++^:='\\'
			t++^:='\''
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
		when 7 then
			t++^:='\\'
			t++^:='a'
		when 8 then
			t++^:='\\'
			t++^:='b'
		when 12 then
			t++^:='\\'
			t++^:='f'
		when 11 then
			t++^:='\\'
			t++^:='v'
		else
			if c<32 or c>=127 then
!			sprintf(&.str,"\\%03o",i32(c))
				fprint @&.str,"\\#o",c:"z3"
				t++^:=str[1]
				t++^:=str[2]
				t++^:=str[3]
				t++^:=str[4]
			else
				t++^:=c
			fi
		end switch
	od
	t^:=0
	return t-t0
end

global function getopcjname(int opc)ichar=		!GETOPCJNAME
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
	static [20]char str
	ichar name,s

	name:=jtagnames[opc]
	s:=strchr(name,' ')
	if s then
		memcpy(&.str,name,s-name)
		str[s-name+1]:=0
		return &.str
	else
		return name
	fi
end

global function strmode(int m,expand=1)ichar=		!STRMODE
	static [16384]char str

	istrmode(m,expand,&.str)

	return &.str
end

global function strmode2(int m,expand=1)ichar=		!STRMODE
	static [16384]char str

	istrmode(m,expand,&.str)

	return &.str
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
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist
		while q do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,typename(m))

	when tproc then
		strcpy(dest,"proc(")
		pm:=ttparams[m]
		n:=pm.nparams
		for i to n do
			istrmode(pm.mode,0,dest+strlen(dest))
			if i<>n then
				strcat(dest,",")
			fi
			pm:=pm.nextparam
		od
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
		strcpy(&.str,(basem=tstruct|"struct "|"union "))
		if ttnamedef[m] then
			strcat(&.str,ttnamedef[m].name)
			strcat(&.str,".")
			strcat(&.str,strint(ttnamedef[m].blockno))
		fi
		return &.str
	when tarray then
		return "<array>"
	when tenum then
		if ttnamedef[m] then
			return ttnamedef[m].name
		fi
		return "<enum>"
	else
		if ttconst[m] then
			strcpy(&.str,"const ")
			strcat(&.str,stdtypenames[basem])
			return &.str
		fi
		return stdtypenames[basem]
	esac
	return ""
end

global function allocunitrec:ref unitrec=
	ref unitrec p
	ref i64 q
	int nwords

	++nunits

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.lineno:=lx.lineno

		if lx.fileno<=255 then
			p.fileno:=lx.fileno
		fi
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
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
	ttisblock[ntypes]:=ttisblock[m]

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
		fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]

	if lx.symbol=namesym then
		strcat(&.str," \"")
		strcat(&.str,getstname(lx.symptr))
		strcat(&.str,"\"")
	fi
		serror(&.str)
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

		if i in [tarray, tstruct] then
			ttisblock[i]:=1
		fi

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

global function createnewproc(ref strec owner,symptr)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
	ref strec p,q

	p:=createdupldef(owner,symptr,procid)

	q:=p
	while q:=q.nextdupl do
		if q.owner=owner then
			cpl q.name,"in",owner.name
			serror("Dupl proc name")
		fi
!	q:=q.nextdupl
	od

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

global function isexported(ref strec d)int=
	if d.nameid=procid then
		if d.code and (d.scope=imported_scope or d.scope=exported_scope) then
			return 1
		fi
	else
		if d.scope=exported_scope then
			return 1
		fi
	fi
	return 0
end

global function isimported(ref strec d)int=
	if d.nameid=procid then
		if d.code=nil and (d.scope=imported_scope or d.scope=exported_scope) then
			return 1
		fi
	else
		if d.scope=imported_scope then
			return 1
		fi
	fi
	return 0
end

global function getstname(ref strec d)ichar=
	static [256]char name
	memcpy(&.name,d.name,d.namelen)
	name[d.namelen+1]:=0
	return &.name
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

global function ispoweroftwo(i64 x)int=
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

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

GLOBAL PROC PAUSE(ichar mess="PAUSE")=
	CP MESS
	OS_GETCH()
	CPL
END

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

=== cc_support.m 0 0 35/79 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global proc stopcompiler(ichar filename,int lineno)=
	if fwriteerrors then
		filehandle f
		f:=fopen("$error.tmp","w")
		println @f,filename,lineno
		fclose(f)
	fi
	println
	println
	stop 1
end

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
	showmacrolineno()

	println
	println "**** Syntax Error:",mess,"****"
	println

	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc serror_ss(ichar mess,a,b)=
	[256]char str
!	sprintf(&.str,mess,a,b)
	fprint @str, mess,a,b
	serror_gen(&.str)
end

global proc serror_s(ichar mess,a)=
	[256]char str
!	sprintf(&.str,mess,a)
	fprint @str, mess, a
	serror_gen(&.str)
end

global proc terror_gen(ichar mess)=

	if currproc then
		println "\nIn function",currproc.name
	fi

	println "Type error:",mess,"on line",lx.lineno,sourcefilepaths[lx.fileno]

	showmacrolineno()

	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc terror(ichar mess)=
	terror_gen(mess)
end

global proc terror_s(ichar mess,a)=
	[256]char str

	fprint @str, mess, a
	terror_gen(&.str)
end

global proc terror_ss(ichar mess,a,b)=
	[256]char str

	fprint @str, mess, a, b
	terror_gen(&.str)
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
	stopcompiler(sourcefilepaths[fileno],lineno)
end

global proc gerror(ichar mess,ref unitrec p=nil)=
	gerror_gen(mess,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
	[256]char str

	fprint @str, mess, s
	gerror_gen(&.str,p)
end

global function nextpoweroftwo(int x)int=
!return next power of 2 >= x

	if x=0 then return 0 fi

	int a:=1
	while a<x do
		a<<:=1
	od
	return a
end

global proc loaderror(ichar mess,mess2="")=
	[512]char str

	fprint @str, mess, mess2
	println "Load Error:",&.str
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

function splicelines(ichar s)ichar=
	ichar t,u

	t:=u:=pcm_alloc(strlen(s)+1)

	while s^ do
		if s^='\\' and (s+1)^=10 then s+:=2
		elsif s^='\\' and (s+1)^=13 and (s+2)^=10 then s+:=3
		else t++^ := s++^
		fi
		t^:=0
	od
	return u
end

global function loadbuiltin(ichar shortfile,hdrtext)int=
!loading build-in header with text at hdrtext
!Name of header is in 'file'.
	ichar s

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles
	sourcefilepaths[nsourcefiles]:="<builtin>"
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
	sourcefiletext[nsourcefiles]:=pcm_copyheapstring(hdrtext)

	sourcefilesizes[nsourcefiles]:=strlen(hdrtext)
	return nsourcefiles
end

proc gs_copytostr(ref strbuffer source,ref char s)=
	if source.length then
		memcpy(s,source.strptr,source.length)
		(s+source.length)^:=0
	else
		s^:=0
	fi
end

global proc gs_additem(ref strbuffer dest,ichar s)=		!GENITEM
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

	d:=dest.strptr

	if dest.length then
		lastchar:=(d+dest.length-1)^
		nextchar:=s^
		if isalphanum(lastchar) and isalphanum(nextchar) then
			strbuffer_add(dest," ")
		fi
	fi
	strbuffer_add(dest,s)
end

function isalphanum(int c)int=
	if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
		return 1
	fi
	return 0
end

proc showmacrolineno=
	if slineno then
!	println "	(Last macro invoked near line",
!		slineno,"in file",sourcefilenames[sfileno],,")"
	fi
end
=== cc_headers.m 0 0 36/79 ===
!Built-in standard headers

global int builtinheaders=1

ichar h_assert		=	strinclude "headers/assert.h"
ichar h_ctype		=	strinclude "headers/ctype.h"
ichar h_errno		=	strinclude "headers/errno.h"
ichar h_fenv		=	strinclude "headers/fenv.h"
ichar h_float		=	strinclude "headers/float.h"
ichar h_inttypes	=	strinclude "headers/inttypes.h"
ichar h_stdint		=	strinclude "headers/stdint.h"
ichar h_limits		=	strinclude "headers/limits.h"
ichar h_locale		=	strinclude "headers/locale.h"
ichar h__ansi		=	strinclude "headers/_ansi.h"
ichar h_math		=	strinclude "headers/math.h"
ichar h_setjmp		=	strinclude "headers/setjmp.h"
ichar h_signal		=	strinclude "headers/signal.h"
ichar h_stdarg		=	strinclude "headers/stdarg.h"
ichar h_stdbool		=	strinclude "headers/stdbool.h"
ichar h_stddef		=	strinclude "headers/stddef.h"
ichar h_stdio		=	strinclude "headers/stdio.h"
ichar h_stdlib		=	strinclude "headers/stdlib.h"
ichar h__syslist	=	strinclude "headers/_syslist.h"
ichar h_string		=	strinclude "headers/string.h"
ichar h_time		=	strinclude "headers/time.h"
ichar h_utime		=	strinclude "headers/utime.h"
ichar h_unistd		=	strinclude "headers/unistd.h"
ichar h_safelib		=	strinclude "headers/safelib.h"
ichar h_wchar		=	strinclude "headers/wchar.h"
ichar h_wctype		=	strinclude "headers/wctype.h"
ichar h_systypes	=	strinclude "headers/sys/types.h"
ichar h_sysstat		=	strinclude "headers/sys/stat.h"
ichar h_systimeb	=	strinclude "headers/sys/timeb.h"
ichar h_sysutime	=	strinclude "headers/sys/utime.h"
ichar h_memory		=	strinclude "headers/memory.h"

!ichar h_windows		=	strinclude "headers/windows.h"

ichar h_fcntl		=	strinclude "headers/fcntl.h"
ichar h_io			=	strinclude "headers/io.h"
ichar h_direct		=	strinclude "headers/direct.h"
ichar h_process		=	strinclude "headers/process.h"
ichar h_malloc		=	strinclude "headers/malloc.h"
ichar h_conio		=	strinclude "headers/conio.h"
ichar h_winsock2	=	strinclude "headers/winsock2.h"
ichar h__mingw		=	strinclude "headers/_mingw.h"
!ichar h_shellapi	=	strinclude "headers/shellapi.h"
ichar h_windowsx	=	strinclude "headers/windowsx.h"

global tabledata []ichar stdhdrnames, []ref ichar stdhdrtext =
	("assert.h",	&h_assert),
	("ctype.h",		&h_ctype),
	("errno.h",		&h_errno),
	("fenv.h",		&h_fenv),
	("float.h",		&h_float),
	("inttypes.h",	&h_inttypes),
	("stdint.h",	&h_stdint),
	("limits.h",	&h_limits),
	("locale.h",	&h_locale),
	("_ansi.h",		&h__ansi),
	("math.h",		&h_math),
	("setjmp.h",	&h_setjmp),
	("signal.h",	&h_signal),
	("stdarg.h",	&h_stdarg),
	("stdbool.h",	&h_stdbool),
	("stddef.h",	&h_stddef),
	("stdio.h",		&h_stdio),
	("stdlib.h",	&h_stdlib),
	("_syslist.h",	&h__syslist),
	("string.h",	&h_string),
	("time.h",		&h_time),
	("utime.h",		&h_utime),
	("unistd.h",	&h_unistd),
	("safelib.h",	&h_safelib),
	("wchar.h",		&h_wchar),
	("wctype.h",	&h_wctype),
	("sys/types.h",	&h_systypes),
	("sys/stat.h",	&h_sysstat),
	("sys/timeb.h",	&h_systimeb),
	("sys/utime.h",	&h_sysutime),
	("malloc.h",	&h_malloc),

!	("windows.h",	&h_windows),

	("fcntl.h",		&h_fcntl),
	("io.h",		&h_io),
	("direct.h",	&h_direct),
	("process.h",	&h_process),
	("memory.h",	&h_memory),
	("conio.h",		&h_conio),
	("winsock2.h",	&h_winsock2),
	("_mingw.h",	&h__mingw),
!	("shellapi.h",	&h_shellapi),
	("windowsx.h",	&h_windowsx)
end

global function findheader(ichar name)ichar=
	int i
	[256]char newname
	ichar s,t

	if strchr(name,'\\') then
		s:=name; t:=&.newname
		while s^ do
			if s^='\\' then
				t++^:='/'
			else
				t++^:=s^
			fi
			++s
		od
		t^:=0
		name:=&.newname
	fi

	for i:=1 to stdhdrnames.len do
		if eqstring(name,stdhdrnames[i]) then
			return stdhdrtext[i]^
		fi
	od
	return nil
end

global proc writeheaders=
	filehandle f
	ichar ifile
	int i
	for i:=1 to stdhdrnames.len do
		ifile:=changeext(stdhdrnames[i],"hdr")
		println "Writing internal",stdhdrnames[i],"as",ifile
		f:=fopen(ifile,"wb")
		fwrite(stdhdrtext[i]^,1,strlen(stdhdrtext[i]^),f)
		fclose(f)
	od
end

global function isheaderfile(ichar file)int=
	for i:=1 to stdhdrnames.len do
		if eqstring(stdhdrnames[i],file) then
			return 1
		fi
	od
	return 0
end
=== cc_showdummy.m 0 0 37/79 ===
strbuffer sbuffer
global ref strbuffer dest=&sbuffer

int currlineno

global proc printcode(filehandle f,ichar caption)=
end

global proc printunit(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=
end

global proc printmodelist(filehandle f)=
end

global function strexpr(ref unitrec p)ref strbuffer=
!CPL "STREXPR"
	nil
end

global proc printst(filehandle f,ref strec p,int level=0)=
end

global proc printstflat(filehandle f)=
end

global function writeallpcl:ref strbuffer=
!CPL "ALLPCL"
	nil
end

global function strpclstr(pcl p)ichar=
!CPL "STRPCL"
	nil
end

global function stropndstack(int indent=0)ichar=
!CPL "STROS"
	nil
end

global proc showopndstack=
!"SHOW"
end

global proc printfilelist(filehandle f)=
end
=== info.txt 0 1 38/79 ===
    The 'MCC' C Compiler comprises:

    mcc.exe            Compiles to .asm files
    aa.exe             Assembles .asm files to .obj files
                       Assemblers and links .asm/.dll files to .exe
    Standard headers   A minimal set inside mcc.exe
    windows.h          As a standalone file

    Input files:

      prog             This is prog.c as the extension is optional
      prog.c
      lib.dll          Include .dll library when generating .exe
      @file            Read parameters and optons from given file

    Options:

      -exe             (DEFAULT) Compile all modules to one .exe file via .asm files
      -e               Preprocess each module to .i file
      -s               Compile each module to .asm file
      -c               Compile each module .obj via .asm

      -out:file        Specify output file for -exe only

    For .exe output, it will be named based on the first input file. Otherwise
    use -out option

    .obj files can be linked using gcc on Windows. This option is
    needed to be able to generate .dll files. However, this will not
    work on newer gcc versions because mcc's generated code is not position
    independent, and will only work loaded in the low 2GB of address space.

    Libraries msvcrt.dll, gdi32.dll, user32.dll and kernel32.dll are
    automatically included as search libraries for imported functions.

    Other kinds of binary libraries or files (.a, .lib, .obj etc) are not supported.

    Omissions, Restrictions and Bugs (highlights only as there are dozens):

      * No VLAs, compound literals, designated initialisers
      * Restrictions on complexity of data initialisers
=== assert.h 0 1 39/79 ===
/* Header assert.h */

//#define assert(x) 0
#define assert(x)
=== ctype.h 0 1 40/79 ===
/* Header ctype.h */

int tolower(int);
int toupper(int);
int isalpha(int);
int isdigit(int);
int isspace(int);
int isalnum(int);
int isupper(int);
int islower(int);

int isxdigit(int);
int iscntrl(int);
int isgraph(int);
int ispunct(int);
int isprint(int);
int __isascii(int);
#define isascii __isascii

int __toascii(int);
#define toascii __toascii
=== errno.h 0 1 41/79 ===
/* Header errno.h */

#ifndef $errno
#define $errno

extern int* _errno(void);
#define errno (*_errno())

#define EPERM 1
#define ENOENT 2
#define ESRCH 3
#define EINTR 4
#define EIO 5
#define ENXIO 6
#define E2BIG 7
#define ENOEXEC 8
#define EBADF 9
#define ECHILD 10
#define EAGAIN 11
#define ENOMEM 12
#define EACCES 13
#define EFAULT 14
#define ENOTBLK 15
#define EBUSY 16
#define EEXIST 17
#define EXDEV 18
#define ENODEV 19
#define ENOTDIR 20
#define EISDIR 21
#define EINVAL 22
#define ENFILE 23
#define EMFILE 24
#define ENOTTY 25
#define ETXTBSY 26
#define EFBIG 27
#define ENOSPC 28
#define ESPIPE 29
#define EROFS 30
#define EMLINK 31
#define EPIPE 32
#define EDOM 33
#define ERANGE 34
#define ENOMSG 35
#define EIDRM 36
#define ECHRNG 37
#define EL2NSYNC 38
#define EL3HLT 39
#define EL3RST 40
#define ELNRNG 41
#define EUNATCH 42
#define ENOCSI 43
#define EL2HLT 44
#define EDEADLK 45
#define ENOLCK 46
#define EBADE 50
#define EBADR 51
#define EXFULL 52
#define ENOANO 53
#define EBADRQC 54
#define EBADSLT 55
#define EDEADLOCK 56
#define EBFONT 57
#define ENOSTR 60
#define ENODATA 61
#define ETIME 62
#define ENOSR 63
#define ENONET 64
#define ENOPKG 65
#define EREMOTE 66
#define ENOLINK 67
#define EADV 68
#define ESRMNT 69
#define ECOMM 70
#define EPROTO 71
#define EMULTIHOP 74
#define ELBIN 75
#define EOVERFLOW 76
#define EBADMSG 77
#define ENOTUNIQ 80
#define EBADFD 81
#define EREMCHG 82
#define ELIBACC 83
#define ELIBBAD 84
#define ELIBSCN 85
#define ELIBMAX 86
#define ELIBEXEC 87
#define ENOSYS 88
#define ENMFILE 89
#define ENOTEMPTY 90
#define ENAMETOOLONG 91
#define EILSEQ 92
#define __ELASTERROR 2000

#endif
=== fenv.h 0 1 42/79 ===
/* Header fenv.h */

int feclearexcept(int);
int fetestexcept(int);

#define FE_INVALID 1
#define FE_DENORMAL 2
#define FE_INEXACT 32
#define FE_DIVBYZERO 4 
#define FE_OVERFLOW 8
#define FE_UNDERFLOW 16
#define FE_STACKFAULT 64
#define FE_ALL_EXCEPT (FE_INVALID|FE_DENORMAL|FE_INEXACT|FE_DIVBYZERO|FE_OVERFLOW|FE_UNDERFLOW)
=== float.h 0 1 43/79 ===
/* Header float.h */

//#define DBL_MAX_10_EXP 308
//#define DBL_MANT_DIG 53

#define FLT_RADIX 2

#define FLT_DIG 6
#define FLT_MIN_EXP -125
#define FLT_MIN 1.17549435E-38F // decimal constant
#define FLT_MIN 0X1P-126F // hex constant
#define FLT_TRUE_MIN 1.40129846E-45F // decimal constant
#define FLT_TRUE_MIN 0X1P-149F // hex constant
#define FLT_HAS_SUBNORM 1
#define FLT_MIN_10_EXP -37
#define FLT_MAX_EXP +128
#define FLT_MAX 3.40282347E+38F // decimal constant
#define FLT_MAX 0X1.fffffeP127F // hex constant
#define FLT_MAX_10_EXP +38
#define FLT_EPSILON 1.19209290e-07F
#define FLT_MANT_DIG 23

#define DBL_MANT_DIG 53
#define DBL_EPSILON 2.2204460492503131E-16 // decimal constant
//#define DBL_EPSILON 0X1P-52 // hex constant
#define DBL_DECIMAL_DIG 17
#define DBL_DIG 15
#define DBL_MIN_EXP -1021
#define DBL_MIN 2.2250738585072014E-308 // decimal constant
//#define DBL_MIN 0X1P-1022 // hex constant
#define DBL_TRUE_MIN 4.9406564584124654E-324 // decimal constant
//#define DBL_TRUE_MIN 0X1P-1074 // hex constant
#define DBL_HAS_SUBNORM 1
#define DBL_MIN_10_EXP -307
#define DBL_MAX_EXP +1024
#define DBL_MAX 1.7976931348623157E+308 // decimal constant
//#define DBL_MAX 0X1.fffffffffffffP1023 // h
#define DBL_MAX_10_EXP +308

#define LDBL_MIN DBL_MIN
#define LDBL_MAX DBL_MAX
#define LDBL_EPSILON 2.2204460492503131E-16
#define LDBL_MANT_DIG 53
#define LDBL_MIN_EXP -1021
#define LDBL_MAX_EXP +1024

int     _isnan(double);
#define isnan _isnan
=== inttypes.h 0 1 44/79 ===
/* Header inttypes.h */

#include <stdint.h>

/* fprintf macros for signed types */
#define PRId8 "d"
#define PRId16 "d"
#define PRId32 "d"
#define PRId64 "I64d"

#define PRIdLEAST8 "d"
#define PRIdLEAST16 "d"
#define PRIdLEAST32 "d"
#define PRIdLEAST64 "I64d"

#define PRIdFAST8 "d"
#define PRIdFAST16 "d"
#define PRIdFAST32 "d"
#define PRIdFAST64 "I64d"

#define PRIdMAX "I64d"

#define PRIi8 "i"
#define PRIi16 "i"
#define PRIi32 "i"
#define PRIi64 "I64i"

#define PRIiLEAST8 "i"
#define PRIiLEAST16 "i"
#define PRIiLEAST32 "i"
#define PRIiLEAST64 "I64i"

#define PRIiFAST8 "i"
#define PRIiFAST16 "i"
#define PRIiFAST32 "i"
#define PRIiFAST64 "I64i"

#define PRIiMAX "I64i"

#define PRIo8 "o"
#define PRIo16 "o"
#define PRIo32 "o"
#define PRIo64 "I64o"

#define PRIoLEAST8 "o"
#define PRIoLEAST16 "o"
#define PRIoLEAST32 "o"
#define PRIoLEAST64 "I64o"

#define PRIoFAST8 "o"
#define PRIoFAST16 "o"
#define PRIoFAST32 "o"
#define PRIoFAST64 "I64o"

#define PRIoMAX "I64o"

/* fprintf macros for unsigned types */
#define PRIu8 "u"
#define PRIu16 "u"
#define PRIu32 "u"
#define PRIu64 "I64u"


#define PRIuLEAST8 "u"
#define PRIuLEAST16 "u"
#define PRIuLEAST32 "u"
#define PRIuLEAST64 "I64u"

#define PRIuFAST8 "u"
#define PRIuFAST16 "u"
#define PRIuFAST32 "u"
#define PRIuFAST64 "I64u"

#define PRIuMAX "I64u"

#define PRIx8 "x"
#define PRIx16 "x"
#define PRIx32 "x"
#define PRIx64 "I64x"

#define PRIxLEAST8 "x"
#define PRIxLEAST16 "x"
#define PRIxLEAST32 "x"
#define PRIxLEAST64 "I64x"

#define PRIxFAST8 "x"
#define PRIxFAST16 "x"
#define PRIxFAST32 "x"
#define PRIxFAST64 "I64x"

#define PRIxMAX "I64x"

#define PRIX8 "X"
#define PRIX16 "X"
#define PRIX32 "X"
#define PRIX64 "I64X"

#define PRIXLEAST8 "X"
#define PRIXLEAST16 "X"
#define PRIXLEAST32 "X"
#define PRIXLEAST64 "I64X"

#define PRIXFAST8 "X"
#define PRIXFAST16 "X"
#define PRIXFAST32 "X"
#define PRIXFAST64 "I64X"

#define PRIXMAX "I64X"

=== stdint.h 0 1 45/79 ===
/* Header stdint.h */

#ifndef $stdint
#define $stdint

typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long int uint64_t;

typedef long long int intptr_t;
typedef unsigned long long int uintptr_t;
typedef long long intmax_t;
typedef unsigned long long uintmax_t;

typedef char int_least8_t;
typedef unsigned char uint_least8_t;
typedef short int_least16_t;
typedef unsigned short uint_least16_t;
typedef int int_least32_t;
typedef unsigned int uint_least32_t;
typedef long long int_least64_t;
typedef unsigned long long uint_least64_t;

#define INTMAX_C(a) (a##LL)
#define UINTMAX_C(a) (a##ULL)

typedef char int_fast8_t;
typedef unsigned char uint_fast8_t;
typedef short int_fast16_t;
typedef unsigned short uint_fast16_t;
typedef int int_fast32_t;
typedef unsigned int uint_fast32_t;
typedef long long int_fast64_t;
typedef unsigned long long uint_fast64_t;


#define INT8_MIN -128
#define INT8_MAX 127
#define INT16_MIN -32768
#define INT16_MAX 32767
#define INT32_MIN 0x80000000
#define INT32_MAX 0x7FFFFFFF
#define INT64_MIN 0x8000000000000000
#define INT64_MAX 0x7FFFFFFFFFFFFFFF

#define UINT8_MAX 255
#define UINT16_MAX 65535
#define UINT32_MAX 0xFFFFFFFF
#define UINT64_MAX 0xFFFFFFFFFFFFFFFF

#define INTPTR_MIN 0x8000000000000000
#define INTPTR_MAX 0x7FFFFFFFFFFFFFFF
#define UINTPTR_MAX 0xFFFFFFFFFFFFFFFF

#define UINT64_MAX 0xFFFFFFFFFFFFFFFF
#define SIZE_MAX 0xFFFFFFFFFFFFFFFF

#define PTRDIFF_MIN 0x8000000000000000
#define PTRDIFF_MAX 0x7FFFFFFFFFFFFFFF

#define WCHAR_MIN 0
#define WCHAR_MAX 65535

#define WINT_MIN 0
#define WINT_MAX 65535

#define UINT64_C(x) (x##ull)

#endif
=== limits.h 0 1 46/79 ===
/* Header limits.h */

#define CHAR_BIT 8

#define CHAR_MIN 0
#define CHAR_MAX 255

#define UCHAR_MIN 0
#define UCHAR_MAX 255

#define SCHAR_MIN -128
#define SCHAR_MAX 127

#define SHRT_MIN -32768
#define SHRT_MAX 32767

#define USHRT_MIN 0
#define USHRT_MAX 65535

#define INT_MIN -2147483648
#define INT_MAX  2147483647

#define UINT_MIN 0
#define UINT_MAX 4294967295

#define LONG_MIN -2147483648
#define LONG_MAX  2147483647

#define ULONG_MIN 0
#define ULONG_MAX 4294967295

#define LLONG_MIN -9223372036854775808LL
#define LLONG_MAX  9223372036854775807LL

#define ULLONG_MIN 0
#define ULLONG_MAX 0xFFFFFFFFFFFFFFFFLL

#define SIZE_MAX ULLONG_MAX
=== locale.h 0 1 47/79 ===
/* Header locale.h */

#ifndef $locale
#define $locale

#define LC_ALL 0
#define LC_COLLATE 1
#define LC_CTYPE 2
#define LC_MONETARY 3
#define LC_NUMERIC 4
#define LC_TIME 5

struct lconv {
	char *decimal_point;
	char *thousands_sep;
	char *grouping;
	char *int_curr_symbol;
	char *currency_symbol;
	char *mon_decimal_point;
	char *mon_thousands_sep;
	char *mon_grouping;
	char *positive_sign;
	char *negative_sign;
	char int_frac_digits;
	char frac_digits;
	char p_cs_precedes;
	char p_sep_by_space;
	char n_cs_precedes;
	char n_sep_by_space;
	char p_sign_posn;
	char n_sign_posn;
};
char *setlocale(int category, const char *locale);

struct lconv *localeconv(void);

char * setlocale(int,const char *);
#endif
=== _ansi.h 0 1 48/79 ===
/* Header _ansi.h */
=== math.h 0 1 49/79 ===
/* Header math.h */

#define HUGE_VAL 1.7976931348623156e+308

double floor(double);
double ceil(double);
double sqrt(double);
double sin(double);
double cos(double);
double tan(double);
double fmod(double,double);
double asin(double);
double acos(double);
double atan(double);
double log(double);
double log10(double);
double exp(double);
double modf(double,double*);
double atan2(double,double);
double pow(double,double);
double fabs(double);
double sinh(double);
double cosh(double);
double tanh(double);
double frexp(double,int*);
double ldexp(double,int);
int isnan(double);

#define ldexpl ldexp

float sinf(float);
float cosf(float);
float tanf(float);
float expf(float);
float powf(float, float);
float logf(float);

float sqrtf(float);
float acosf(float);
float atan2f(float,float);
float ceilf(float);

double acosh (double);
long double acoshl(long double);
long double acoshl(long double);
float acoshf(float);

double asinh (double);
long double asinhl(long double);
long double sinhl(long double);
float asinhf(float);

double atanh(double);
long double atanhl(long double);
float atanhf(float);

long double expm1l(long double);
double expm1(double);
float expm1f(float);
double _expm1i(int);

double cbrt(double);
long double cbrtl(long double);
float cbrtf(float);

double trunc(double x);
long double truncl(long double);
float truncf(float);

double round(double);
float roundf(float);
long double roundl(long double);
double _roundi(int);

long double fminl(long double,long double);
float fminf(float ,float);
double fmin(double,double);

//float fabsf(float);
#define fabsf fabs

double lgamma (double);
long double lgammal(long double);
float lgammaf(float);

long double tgammal(long double);
double tgamma(double);
float tgammaf(float);

double log1p(double);
long double log1pl(long double);
float log1pf(float);

long double log10l(long double);
float log10f(float);
double _log10i(int);
double log10(double);

double erf(double);
long double erfl(long double);
float erff(float);

double hypot (double, double);
double _hypot(double,double);
long double hypotl(long double,long double);
float hypotf(float,float);

double nextafter (double, double);
long double nextafterl(long double,long double);
float nextafterf(float,float);

double nexttoward(double,long double);
long double nexttowardl(long double,long double);
float nexttowardf(float,long double);

double erfc(double);
long double erfcl(long double);
float erfcf(float);

int _finite(double);
#define finite _finite

float floorf(float);

double _copysign(double,double);
#define copysign _copysign

long double fmaxl(long double,long double);
double fmax(double,double);
float fmaxf(float,float);
float fmodf(float,float);

//long double exp2l(long double);
double exp2(double);
float exp2f(float);

//double log2(double);
#define log2(x) (log(x)*1.442695041)

#define M_PI 3.1415926535897932384625433
#define M_PI_2 (M_PI/2.0)
#define M_2_PI 0.63661977236758134308

int isinf(double);

int _finite(double);

#define isfinite _finite

int isfinitef(float);
int isfinitel(long double);

=== setjmp.h 0 1 50/79 ===
/* Header setjmp.h */


#ifndef $setjmphdr
#define $setjmphdr 1

typedef int jmp_buf[128];

//void $mcclongjmp(jmp_buf, int);
//int $mccsetjmp(jmp_buf);

#define setjmp $setjmp
#define longjmp $longjmp

#endif

=== signal.h 0 1 51/79 ===
/* Header signal.h */

#define SIGINT    2
#define SIGILL    4
#define SIGFPE    8
#define SIGSEGV  11
#define SIGTERM  15
#define SIGBREAK 21
#define SIGABRT  22

#define SIG_DFL (void (*)(int))0
#define SIG_IGN (void (*)(int))1
#define SIG_SGE (void (*)(int))3
#define SIG_ACK (void (*)(int))4

#define SIG_ERR (void (*)(int))-1

extern void (*signal(int, void (*)(int)))(int);

extern int raise(int);


typedef int sig_atomic_t;
=== stdarg.h 0 1 52/79 ===
/* Header stdarg.h */

#ifndef $STDARG
 #define $STDARG

//coded for x64 target as used by mcc (with first four params also on stack)

 typedef char *	va_list;

 #if __MCCI__
  #define va_start(ap,v) ap=((va_list)&v-8)
  #define va_arg(ap,t) *(t*)((ap-=8)+8)
  #define va_copy(dest,src) (dest=src)
  #define va_end(ap)	( ap = (va_list)0 )

 #else
  #define va_start(ap,v) ap=((va_list)&v+8)
  #define va_arg(ap,t) *(t*)((ap+=8)-8)
  #define va_copy(dest,src) (dest=src)
  #define va_end(ap)	( ap = (va_list)0 )

 #endif

#endif
=== stdbool.h 0 1 53/79 ===
/* Header stdbool.h */

#define bool unsigned char
#define true 1
#define false 0


=== stddef.h 0 1 54/79 ===
/* Header stddef.h */

#ifndef $stddef
#define $stddef

typedef signed long long int ssize_t;
typedef unsigned long long int size_t;

#define _WCHAR_T_DEFINED
typedef unsigned short wchar_t;
//typedef signed short wchar_t;

#define NULL ((void*)0)

#define offsetof(a,b) (size_t) &( ((a*)0) -> b)

typedef long long int ptrdiff_t;

#endif // stddef
=== stdio.h 0 1 55/79 ===
/* Header stdio.h */

#ifndef $stdio
#define $stdio 1

#define __attribute__(x)

#ifndef $valist
	typedef char* va_list;
	#define $valist
#endif

#include <stddef.h>

typedef long long int fpos_t;

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define	STDIN_FILENO	0
#define	STDOUT_FILENO	1
#define	STDERR_FILENO	2

#define EOF (-1)
#define FILENAME_MAX 260

#define L_tmpnam 10

typedef struct {
	char *_ptr;
	int   _cnt;
	char *_base;
	int   _flag;
	int   _file;
	int   _charbuf;
	int   _bufsiz;
	char *_tmpfname;
} FILE;

extern char* __iob_func(void);

#define stdin ((FILE*)(__iob_func()))
#define stdout ((FILE*)(__iob_func()+sizeof(FILE)))
#define stderr ((FILE*)(__iob_func()+sizeof(FILE)*2))

#define _IOREAD 0x0001
#define _IOWRT 0x0002

#define _IOFBF 0x0000
#define _IOLBF 0x0040
#define _IONBF 0x0004

#define _IOMYBUF 0x0008
#define _IOEOF 0x0010
#define _IOERR 0x0020
#define _IOSTRG 0x0040
#define _IORW 0x0080

#define BUFSIZ 512

FILE* fopen(const char*, const char*);
int fclose(FILE*);
long ftell(FILE*);
//long long int _ftelli64(FILE*);
int fseek(FILE*,long,int);
int _fseeki64(FILE*,long long int,int);

size_t fread(void*, size_t, size_t, FILE*);
size_t fwrite(const void*, size_t, size_t, FILE*);
int remove(const char*);
int rename(const char *,const char *);
FILE* freopen(const char*, const char*, FILE*);
FILE* _wfopen(const wchar_t*,const wchar_t *);

int printf(const char*, ...);
int sprintf(char*,const char*, ...);
int fprintf(FILE*,const char*, ...);
int sscanf(const char*, const char*, ...);
int scanf(const char*, ...);
int fscanf(FILE *,const char *, ...);
int _snprintf(char *,size_t,const char*,...);
#define snprintf _snprintf
int _vsnprintf(char*, size_t, const char*, va_list);
int vsnprintf(char*,size_t,const char*,va_list);
int vsprintf(char*, const char*, va_list);
int _wremove(const wchar_t*);
int _wrename(const wchar_t*,const wchar_t*);

typedef char* va_list;

int vfprintf(FILE*, const char*, va_list);
int vprintf(const char*, va_list);

int puts(const char*);
char* fgets(char*, int, FILE*);
int fputs(const char*, FILE*);
int fgetc(FILE*);
int fputc(int, FILE*);
int ungetc(int, FILE*);
int getchar(void);
int putchar(int);
int fflush(FILE *);
int getc(FILE *);
int putc(int, FILE *);

int feof(FILE*);
int ferror(FILE*);
void clearerr(FILE*);

int fileno(FILE*);
int _fileno(FILE*);
int setvbuf(FILE*,char*,int,size_t);
FILE* _popen(const char*, const char*);
int _pclose(FILE*);
int _unlink(const char *);
#define unlink _unlink;
FILE* _fdopen(int, const char *);
#define fdopen _fdopen
int fgetpos(FILE*, fpos_t*);
int fsetpos(FILE*, const fpos_t*);
void perror(char*);
void setbuf(FILE*, char*);

void rewind(FILE*);

FILE* tmpfile(void);

char* tmpnam(char*);
wchar_t getwc(FILE *);

extern void* _wenviron;

#endif

=== stdlib.h 0 1 56/79 ===
/* Header stdlib.h */

#ifndef $stdlib
#define $stdlib 1

#include <stddef.h>

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#define RAND_MAX 32767

void* malloc(size_t);
void* realloc(void*, size_t);
void* calloc(size_t, size_t);

void free(void*);

void exit(int);

int system(const char*);
int _wsystem(const wchar_t*);

int abs(int);
long labs(long);
long long llabs(long);
int rand(void);
void srand(unsigned int);
int atoi(const char*);
long atol(const char*);
long long atoll(const char*);
long long atoi64(const char*);
double atof(const char *);
int	atexit(void (*)(void));
char *_itoa(int,char *,int);

#define itoa _itoa

void qsort(void*, size_t, size_t, int (*)(void*, void*));

typedef struct {
	long long int quot;
	long long int rem;
} lldiv_t;

lldiv_t lldiv(long long int, long long int);

char* getenv(const char*);
wchar_t* _wgetenv(const wchar_t*);
int _wputenv(const wchar_t*);


double strtod(const char*, char**);

//float strtof(const char *, char**);
#define strtof strtod

long double strtold(const char*, char**);
void abort(void);
long int strtol(const char*, char**, int);

long double strtold(const char *,char **);

long long int _strtoi64(const char *,char **,int);
#define strtoll _strtoi64

unsigned long long int _strtoui64(const char *,char **,int);

unsigned long long int _strtoui64(const char*, char**, int);
#define strtoull _strtoui64
unsigned long strtoul(const char*, char**, int);

size_t _msize(void *);
#define msize _msize

void* bsearch(const void *,const void *, size_t, size_t,
		int (*_compar)(const void *,const void *));


char* _fullpath(char*, const char*, size_t);


#endif
=== _syslist.h 0 1 57/79 ===
/* Header _syslist.h */
=== string.h 0 1 58/79 ===
/* Header string.h */

#include <stddef.h>

void* memcpy(void*, const void*, size_t);
void* memmove(const void*, const void*, size_t);
void* memset(void*, int, size_t);
int memcmp(const void*, const void*, size_t);
void* memchr(const void *, int, size_t);

size_t strlen(const char*);
char* strcpy(char*, const char*);
char* strncpy(char*, const char*, size_t);
char* strcat(char*, const char*);
int strcmp(const char*, const char*);
int strncmp(const char*, const char*, size_t);
char* strchr(const char*, int);
char* strstr(const char*, const char*);
char* strrchr(const char*, int);
int _stricmp(const char*, const char*);
#define stricmp _stricmp
#define strcasecmp _stricmp



int _strnicmp(const char*, const char*, size_t);
#define strnicmp _strnicmp
char* strncat(char*, const char*, size_t);
char* strtok(char*, const char*);

size_t strcspn(const char*, const char*);
size_t strspn(const char*, const char*);
int strcoll(const char *, const char *);

unsigned long strtoul(const char*, char**, int);

char* strerror(int);
char* strpbrk(const char*, const char*);
size_t strxfrm(char*, const char *, size_t);

char* _strupr(char*);
char* _strlwr(char*);
#define strupr _strupr
#define strlwr _strlwr

char*strnupr(char*,size_t);
char*strnlwr(char*,size_t);
int strtrim(char*);
char*strrev(char*);

char* _strdup(const char*);
#define strdup _strdup

int _wcsicmp(const wchar_t *,const wchar_t *);

wchar_t *wcspbrk(const wchar_t*,const wchar_t*);

size_t wcslen(const wchar_t*);
=== time.h 0 1 59/79 ===
/* Header time.h */

#ifndef $time
#define $time

#include <stddef.h>

typedef long clock_t;
clock_t clock(void);

//typedef long time_t;
#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
typedef long long int time_t;
#endif

struct tm
{
  int	tm_sec;
  int	tm_min;
  int	tm_hour;
  int	tm_mday;
  int	tm_mon;
  int	tm_year;
  int	tm_wday;
  int	tm_yday;
  int	tm_isdst;
};

//struct tm *localtime(time_t*);
struct tm* _localtime32(time_t*);
#define localtime _localtime32

time_t _time64(time_t *_timer);
#define time _time64

#define CLOCKS_PER_SEC 1000

struct tm *gmtime(const time_t*);
size_t strftime(char *, size_t, const char *, const struct tm *);
time_t mktime(struct tm *);
double difftime(time_t, time_t);

char* asctime(const struct tm*);
//char* ctime(const time_t *_time);
char* _ctime64(const time_t *_time);
#define ctime _ctime64

#endif
=== utime.h 0 1 60/79 ===
/* sys/utime.h header */

#ifndef $utime
#define $utime

struct _utimbuf {
	long actime;
	long modtime;
};


struct utimbuf {
	long actime;
	long modtime;
};

struct __utimbuf64 {
  long long int actime;
  long long int modtime;
};

int _utime64(const char *_Filename,struct __utimbuf64 *_Time);

int utime(const char *_Filename,struct utimbuf *_Utimbuf) {
  return _utime64(_Filename,(struct __utimbuf64 *)_Utimbuf);
}

#endif
=== unistd.h 0 1 61/79 ===
/* unistd.h header */

unsigned int _sleep(unsigned int);
#define sleep _sleep

#define alarm(x) {puts("ALARM"); exit(1);}

int _write(int,void*,unsigned int);
#define write _write

int _close(int);
#define close _close
=== safelib.h 0 1 62/79 ===
/* Header safelib.h */
=== wchar.h 0 1 63/79 ===
/* Header wchar.h */

#include <stddef.h>


#define _WCHAR_T_DEFINED
typedef unsigned short wint_t;
typedef unsigned short wchar_t;
//typedef signed short wchar_t;

size_t wcslen(const wchar_t*);
wchar_t* wcscpy(wchar_t*,const wchar_t*);
//wchar_t* _wgetenv(constwchar_t*);
wchar_t* wcstok(wchar_t*,wchar_t*,wchar_t**);
wchar_t* wcstok_s(wchar_t*,wchar_t*,wchar_t**);

wchar_t* wcschr(wchar_t*,wchar_t);
wchar_t* wcsstr(const wchar_t*,const wchar_t*);


int wcscmp(const wchar_t*,const wchar_t*);
typedef long _off_t;
=== wctype.h 0 1 64/79 ===
/* Header wctype.h */
=== types.h 0 1 65/79 ===
/* types.h */
#ifndef $systypes
#define $systypes 1

typedef long int off_t;
typedef long int ino_t;
typedef unsigned int dev_t;

typedef long long time_t;

#endif
=== stat.h 0 1 66/79 ===
/* stat.h */

#ifndef $sysstat
#define $sysstat

#include <stddef.h>

struct _stat {
	unsigned int	st_dev;
	unsigned short	st_ino;
	unsigned short	st_mode;
	short			st_nlink;
	short			st_uid;
	short			st_gid;
	unsigned long	st_rdev;
	unsigned int	st_size;
	unsigned long long int	st_atime;
	unsigned long long int	st_mtime;
	unsigned long long int	st_ctime;
};

#define stat _stat
#define _stati64 _stat

#define _S_IFMT 0xF000
#define _S_IFDIR 0x4000
#define _S_IFCHR 0x2000
#define _S_IFIFO 0x1000
#define _S_IFREG 0x8000
#define _S_IREAD 0x0100
#define _S_IWRITE 0x0080
#define _S_IEXEC 0x0040

#define S_IFMT 0xF000
#define S_IFDIR 0x4000
#define S_IFCHR 0x2000
#define S_IFIFO 0x1000
#define S_IFREG 0x8000
#define S_IREAD 0x0100
#define S_IWRITE 0x0080
#define S_IEXEC 0x0040

#define S_ISCHR(nd) (((nd) & S_IFMT) == S_IFCHR)
#define S_ISDIR(nd) (((nd) & S_IFMT) == S_IFDIR)
#define S_ISFIFO(nd) (((nd) & S_IFMT) == S_IFIFO)
#define S_ISREG(nd) (((nd) & S_IFMT) == S_IFREG)


#define	_S_IRWXU	(_S_IREAD | _S_IWRITE | _S_IEXEC)
#define	_S_IXUSR	_S_IEXEC
#define	_S_IWUSR	_S_IWRITE

#define	S_IRWXU		_S_IRWXU
#define	S_IXUSR		_S_IXUSR
#define	S_IWUSR		_S_IWUSR
#define	S_IRUSR		_S_IRUSR
#define	_S_IRUSR	_S_IREAD

#define S_IRGRP    (S_IRUSR >> 3)
#define S_IWGRP    (S_IWUSR >> 3)
#define S_IXGRP    (S_IXUSR >> 3)
#define S_IRWXG    (S_IRWXU >> 3)

#define S_IROTH    (S_IRGRP >> 3)
#define S_IWOTH    (S_IWGRP >> 3)
#define S_IXOTH    (S_IXGRP >> 3)
#define S_IRWXO    (S_IRWXG >> 3)

//#define	S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
//#define	S_ISFIFO(m)	(((m) & S_IFMT) == S_IFIFO)
//#define	S_ISCHR(m)	(((m) & S_IFMT) == S_IFCHR)
#define	S_IFBLK	0x3000	/* Block: Is this ever set under w32? */
#define	S_ISBLK(m)	(((m) & S_IFMT) == S_IFBLK)
#define	S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)



int stat(const char *, struct stat*);

int _fstati64(int, struct stat*);

int fstat(int, struct stat *);
#define _fstat fstat

int _wstati64(const wchar_t,struct _stati64 *buffer);  



#endif
=== timeb.h 0 1 67/79 ===
/* timeb.h */

#ifndef $timeb
#define $timeb

#include <time.h>

struct _timeb {
	time_t time;
	unsigned short millitm;
	short timezone;
	short dstflag;
};
#define timeb _timeb

void _ftime64(struct _timeb*);
#define _ftime _ftime64
#define ftime _ftime64

#endif
=== memory.h 0 1 68/79 ===
#include <malloc.h>
=== fcntl.h 0 1 69/79 ===
/* FCNTL.H */

int _setmode(int,int);

#ifndef _INC_FCNTL
#define _INC_FCNTL

#define _O_RDONLY 0x0000
#define _O_WRONLY 0x0001
#define _O_RDWR 0x0002
#define _O_APPEND 0x0008
#define _O_CREAT 0x0100
#define _O_TRUNC 0x0200
#define _O_EXCL 0x0400
#define _O_TEXT 0x4000
#define _O_BINARY 0x8000
#define _O_WTEXT 0x10000
#define _O_U16TEXT 0x20000
#define _O_U8TEXT 0x40000
#define _O_ACCMODE (_O_RDONLY|_O_WRONLY|_O_RDWR)

#define _O_RAW _O_BINARY
#define _O_NOINHERIT 0x0080
#define _O_TEMPORARY 0x0040
#define _O_SHORT_LIVED 0x1000

#define _O_SEQUENTIAL 0x0020
#define _O_RANDOM 0x0010

#if !defined(NO_OLDNAMES) || defined(_POSIX)
#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY
#define O_RDWR _O_RDWR
#define O_APPEND _O_APPEND
#define O_CREAT _O_CREAT
#define O_TRUNC _O_TRUNC
#define O_EXCL _O_EXCL
#define O_TEXT _O_TEXT
#define O_BINARY _O_BINARY
#define O_RAW _O_BINARY
#define O_TEMPORARY _O_TEMPORARY
#define O_NOINHERIT _O_NOINHERIT
#define O_SEQUENTIAL _O_SEQUENTIAL
#define O_RANDOM _O_RANDOM
#define O_ACCMODE _O_ACCMODE
#endif

#endif
=== io.h 0 1 70/79 ===

#ifndef $io
#define $io

#include <stdint.h>
#include <wchar.h>
#include <time.h>

/* io.h */

#define _A_NORMAL 0x00
#define _A_RDONLY 0x01
#define _A_HIDDEN 0x02
#define _A_SYSTEM 0x04
#define _A_SUBDIR 0x10
#define _A_ARCH 0x20


//#message "IO included"
struct _finddata_t {
        unsigned    attrib;
        time_t      time_create;
        time_t      time_access;
        time_t      time_write;
        unsigned long    size;
        char        name[260];
};

int _isatty(int);

int _read(int, void*, unsigned int);
#define read _read

int _open(const char*, int, ...);
#define open _open

long _lseek(int, long, int);
#define lseek _lseek

int _close(int);
#define close _close

intptr_t _get_osfhandle(int);
int _open_osfhandle(intptr_t,int);

int _setmode(int,int);
#define setmode _setmode

int _wchmod(const wchar_t*, int);

int _chmod(const char *, int);
#define chmod _chmod

int _dup(int);
#define dup _dup

intptr_t _findfirst(const char*, struct _finddata_t*);
int _findnext(intptr_t, struct _finddata_t*);
int _findclose(intptr_t);

int _access(const char*,int);
#define access _access

int _wunlink(const wchar_t*);


#endif
=== direct.h 0 1 71/79 ===
#ifndef $direct
#define $direct

typedef struct {
 unsigned int total_clusters;
 unsigned int avail_clusters;
 unsigned int sectors_per_cluster;
 unsigned int bytes_per_sector;
} diskfree_t;

int chdir(const char *);
char* _getcwd(char *, int);
#define getcwd _getcwd
int _mkdir(const char *);
int _rmdir(const char *);
int _wrmdir(const wchar_t*);
int _wmkdir(const wchar_t*);

#define mkdir _mkdir
#define rmdir _rmdir

int _chdrive(int);
char* _getdcwd(int, char *, int);
wchar_t* _wgetcwd(wchar_t*, int);
int _wchdir(const wchar_t*);
int _getdrive(void);
int _chdir(const char*);
#define chdir _chdir

unsigned long _getdrives(void);
unsigned int _getdiskfree(unsigned int, diskfree_t*);

#endif

=== process.h 0 1 72/79 ===
/* Header process.h */
#ifndef $process
#define $process

#include <stdint.h>

#define P_WAIT		0
#define P_NOWAIT	1
#define P_OVERLAY	2
#define P_DETACH	4
#define WAIT_CHILD 0
#define _P_WAIT P_WAIT
#define _P_NOWAIT P_NOWAIT
#define _P_OVERLAY P_OVERLAY

int _spawnvp(int, const char*, const char*const*);
#define spawnvp _spawnvp

void endthread(void);
unsigned long _beginthreadex(void *,unsigned,unsigned (*)(void *),void *,unsigned,unsigned *);
void _endthreadex(unsigned);
int _cwait(int*, int, int);
#define cwait _cwait
//int _System(const char *cmd,int nCmdShow);

intptr_t _execve(const char*, const char*const*, const char*const*);
#define execve _execve

//#define _WAIT_CHILD	0
//#endif
#endif

=== malloc.h 0 1 73/79 ===
#include <stddef.h>

void* malloc(size_t);
void* realloc(void *, size_t);
void  free(void *);
void *_alloca(size_t);

#define alloca malloc
=== conio.h 0 1 74/79 ===
/* conio.h */
#include <stddef.h>
#include <wchar.h>

int _getch(void);
int _kbhit(void);
wint_t _getwch(void);

#define getch _getch
#define kbhit _kbhit

int _putch(int);
int _cprintf(char *, ...);
int _getche(void);
int _ungetch(int);

#define putch _putch
#define cprintf _cprintf
#define getche _getche
#define ungetch _ungetch
=== winsock2.h 0 1 75/79 ===
#ifndef _WINSOCK2_H
#define _WINSOCK2_H

#include <_mingw.h>
#include <windows.h>

#ifndef FD_SETSIZE
#define FD_SETSIZE 64
#endif

typedef unsigned int SOCKET;

typedef struct fd_set {
  unsigned int   fd_count;
  SOCKET  fd_array[FD_SETSIZE];
} fd_set;

extern int __stdcall __WSAFDIsSet(SOCKET,fd_set *);

#ifndef FD_CLR
#define FD_CLR(fd,set) do { unsigned int __i;\
for (__i = 0; __i < ((fd_set *)(set))->fd_count ; __i++) {\
    if (((fd_set *)(set))->fd_array[__i] == (fd)) {\
    while (__i < ((fd_set *)(set))->fd_count-1) {\
        ((fd_set*)(set))->fd_array[__i] = ((fd_set*)(set))->fd_array[__i+1];\
        __i++;\
    }\
    ((fd_set*)(set))->fd_count--;\
    break;\
    }\
}\
} while (0)
#endif

#ifndef FD_SET
/* this differs from the define in winsock.h and in cygwin sys/types.h */
#define FD_SET(fd, set) do { unsigned int __i;\
for (__i = 0; __i < ((fd_set *)(set))->fd_count ; __i++) {\
    if (((fd_set *)(set))->fd_array[__i] == (fd)) {\
        break;\
    }\
}\
if (__i == ((fd_set *)(set))->fd_count) {\
    if (((fd_set *)(set))->fd_count < FD_SETSIZE) {\
        ((fd_set *)(set))->fd_array[__i] = (fd);\
        ((fd_set *)(set))->fd_count++;\
    }\
}\
} while(0)
#endif

#ifndef FD_ZERO
#define FD_ZERO(set) (((fd_set *)(set))->fd_count=0)
#endif

#ifndef FD_ISSET
#define FD_ISSET(fd, set) __WSAFDIsSet((SOCKET)(fd), (fd_set *)(set))
#endif

#ifndef _TIMEVAL_DEFINED /* also in sys/time.h */
#define _TIMEVAL_DEFINED
struct timeval {
    long tv_sec;
    long tv_usec;
};
#endif

struct in_addr {
    union {
        struct { unsigned char s_b1,s_b2,s_b3,s_b4; } S_un_b;
        struct { unsigned short s_w1,s_w2; } S_un_w;
        unsigned long S_addr;
    } S_un;
};
#define s_addr  S_un.S_addr

struct sockaddr_in {
    short sin_family;
    unsigned short sin_port;
    struct in_addr sin_addr;
    char sin_zero[8];
};

struct sockaddr {
    unsigned short sa_family;
    char sa_data[14];
};

struct hostent {
    char *h_name;
    char **h_aliases;
    short h_addrtype;
    short h_length;
    char **h_addr_list;
};
#define h_addr h_addr_list[0]

#define WSADESCRIPTION_LEN  256
#define WSASYS_STATUS_LEN   128
typedef struct WSAData {
    WORD wVersion;
    WORD wHighVersion;
    char szDescription[WSADESCRIPTION_LEN+1];
    char szSystemStatus[WSASYS_STATUS_LEN+1];
    unsigned short iMaxSockets;
    unsigned short iMaxUdpDg;
    char *lpVendorInfo;
} WSADATA;
typedef WSADATA *LPWSADATA;

#define INVALID_SOCKET (SOCKET)(~0)
#define SOCK_STREAM  1
#define SO_REUSEADDR 4
#define AF_INET 2
#define MSG_PEEK 0x2
#define INADDR_ANY (unsigned long)0x00000000
#define INADDR_LOOPBACK 0x7f000001
#define SOL_SOCKET 0xffff

#define SD_RECEIVE  0x00
#define SD_SEND     0x01
#define SD_BOTH     0x02

#define h_errno WSAGetLastError()

#define WSABASEERR 10000
#define TRY_AGAIN (WSABASEERR+1002)

SOCKET __stdcall accept(SOCKET, struct sockaddr *, int *);
int __stdcall bind(SOCKET, const struct sockaddr *, int);
int __stdcall closesocket(SOCKET);
int __stdcall connect(SOCKET, const struct sockaddr *, int);
struct hostent *__stdcall gethostbyname(const char *);
int __stdcall gethostname(char *, int);
int __stdcall getpeername(SOCKET, struct sockaddr *, int *);
int __stdcall getsockname(SOCKET, struct sockaddr *, int *);
unsigned long __stdcall htonl(unsigned long);
unsigned short __stdcall htons(unsigned short);
int __stdcall listen(SOCKET, int);
unsigned long __stdcall ntohl(unsigned long);
unsigned short __stdcall ntohs(unsigned short);
int __stdcall recv(SOCKET, char *, int, int);
int __stdcall recvfrom(SOCKET, char *, int, int, struct sockaddr *, int *);
int __stdcall select(int nfds, fd_set *, fd_set *, fd_set *, const struct timeval *);
int __stdcall send(SOCKET, const char *, int, int);
int __stdcall sendto(SOCKET, const char *, int, int, const struct sockaddr *, int);
int __stdcall setsockopt(SOCKET, int, int, const char *, int);
int __stdcall shutdown(SOCKET, int);
int __stdcall WSACleanup(void);
int __stdcall WSAGetLastError(void);
int __stdcall WSAStartup(WORD, LPWSADATA);

SOCKET __stdcall socket(int, int, int);

#endif

=== _mingw.h 0 1 76/79 ===
/*
 * _mingw.h
 *
 *  This file is for TinyCC and not part of the Mingw32 package.
 *
 *  THIS SOFTWARE IS NOT COPYRIGHTED
 *
 *  This source code is offered for use in the public domain. You may
 *  use, modify or distribute it freely.
 *
 *  This code is distributed in the hope that it will be useful but
 *  WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESS OR IMPLIED ARE HEREBY
 *  DISCLAIMED. This includes but is not limited to warranties of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 */

#ifndef __MINGW_H
#define __MINGW_H

/* some winapi files define these before including _mingw.h --> */
#undef __cdecl
#undef _X86_
#undef WIN32
/* <-- */

#include <stddef.h>
#include <stdarg.h>

#define __int8 char
#define __int16 short
#define __int32 int
#define __int64 long long
#define _HAVE_INT64

#define __cdecl
#define __declspec(x) __attribute__((x))
#define __unaligned __attribute__((packed))
#define __fastcall __attribute__((fastcall))

#define __MSVCRT__ 1
#undef _MSVCRT_
#define __MINGW_IMPORT extern __declspec(dllimport)
#define __MINGW_ATTRIB_NORETURN
#define __MINGW_ATTRIB_CONST
#define __MINGW_ATTRIB_DEPRECATED
#define __MINGW_ATTRIB_MALLOC
#define __MINGW_ATTRIB_PURE
#define __MINGW_ATTRIB_NONNULL(arg)
#define __MINGW_NOTHROW
#define __GNUC_VA_LIST

#define _CRTIMP extern
#define __CRT_INLINE extern __inline__

#define _CRT_ALIGN(x) __attribute__((aligned(x)))
#define DECLSPEC_ALIGN(x) __attribute__((aligned(x)))
#define _CRT_PACKING 8
#define __CRT_UNALIGNED
#define _CONST_RETURN

#ifndef _TRUNCATE
#define _TRUNCATE ((size_t)-1)
#endif

#define __CRT_STRINGIZE(_Value) #_Value
#define _CRT_STRINGIZE(_Value) __CRT_STRINGIZE(_Value)
#define __CRT_WIDE(_String) L ## _String
#define _CRT_WIDE(_String) __CRT_WIDE(_String)

#ifdef _WIN64
#define __stdcall
#define _AMD64_ 1
#define __x86_64 1
#define _M_X64 100 /* Visual Studio */
#define _M_AMD64 100 /* Visual Studio */
#define USE_MINGW_SETJMP_TWO_ARGS
#define mingw_getsp tinyc_getbp
#define __TRY__
#else
#define __stdcall __attribute__((__stdcall__))
#define _X86_ 1
#define _M_IX86 300 /* Visual Studio */
#define WIN32 1
#define _USE_32BIT_TIME_T
#ifdef __arm__
#define __TRY__
#else
#define __TRY__ void __try__(void**), *_sehrec[6]; __try__(_sehrec);
#endif
#endif

/* in stddef.h */
#define _SIZE_T_DEFINED
#define _SSIZE_T_DEFINED
#define _PTRDIFF_T_DEFINED
#define _WCHAR_T_DEFINED
#define _UINTPTR_T_DEFINED
#define _INTPTR_T_DEFINED
#define _INTEGRAL_MAX_BITS 64

#ifndef _TIME32_T_DEFINED
#define _TIME32_T_DEFINED
typedef long __time32_t;
#endif

#ifndef _TIME64_T_DEFINED
#define _TIME64_T_DEFINED
typedef long long __time64_t;
#endif

#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
#ifdef _USE_32BIT_TIME_T
typedef __time32_t time_t;
#else
typedef __time64_t time_t;
#endif
#endif

#ifndef _WCTYPE_T_DEFINED
#define _WCTYPE_T_DEFINED
typedef wchar_t wctype_t;
#endif

#ifndef _WINT_T
#define _WINT_T
typedef short wint_t;
#endif

typedef int errno_t;
#define _ERRCODE_DEFINED

typedef struct threadlocaleinfostruct *pthreadlocinfo;
typedef struct threadmbcinfostruct *pthreadmbcinfo;
typedef struct localeinfo_struct _locale_tstruct,*_locale_t;

/* for winapi */
#define _ANONYMOUS_UNION
#define _ANONYMOUS_STRUCT
#define DECLSPEC_NORETURN
#define DECLARE_STDCALL_P(type) __stdcall type
#define NOSERVICE 1
#define NOMCX 1
#define NOIME 1
#define __INTRIN_H_
#ifndef DUMMYUNIONNAME
#  define DUMMYUNIONNAME
#  define DUMMYUNIONNAME1
#  define DUMMYUNIONNAME2
#  define DUMMYUNIONNAME3
#  define DUMMYUNIONNAME4
#  define DUMMYUNIONNAME5
#endif
#ifndef DUMMYSTRUCTNAME
#  define DUMMYSTRUCTNAME
#endif
#ifndef WINVER
# define WINVER 0x0502
#endif
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x502
#endif

#define __C89_NAMELESS
#define __MINGW_EXTENSION
#define WINAPI_FAMILY_PARTITION(X) 1
#define MINGW_HAS_SECURE_API

#endif /* __MINGW_H */
=== windowsx.h 0 1 77/79 ===
/* Header windowsx.h */

#ifndef $windowsx
#define $windowsx 1

#define GET_X_LPARAM(lp)                        ((int)(short)LOWORD(lp))
#define GET_Y_LPARAM(lp)                        ((int)(short)HIWORD(lp))

#endif // windowsx

=== cc_help.txt 0 1 78/79 ===
C Subset Compiler for 64-bit Windows

Normal use:           Compiles prog.c to:

    cc      prog      prog.exe (default)
    cc -r   prog      in-memory native code then execute
    cc -i   prog      in-memory IL then interpret

    cc -exe prog      prog.exe
    cc -dll prog      prog.dll
    cc -obj prog      prog.obj
    cc -a   prog      prog.asm
    cc -n   prog      prog.nasm
    cc -mx  prog      prog.mx
    cc -p   prog      prog.pcl (textual IL)
    cc -e   prog      prog.i   (preprocess only)

Other options:

    -incl:path        Add path to search for includes
    -ext              Used std headers external to compiler
    -opt              Optimise native code
    -out:file         Name output file (extension can be added)
    -rip              Use RIP address modes
    -himem            Generate PIC code (automatic with -obj/-dll)
    @file             Read files and options from a file
    file.dll          Include one or more extra dynamic libs
    -c                Same as -obj
    -s                Same as -asm
    -ei               Same as -e, but when result is interpreted

Notes:

    * Compiles single module only (use bcc driver program for multiple-modules)
    * For -i and -r, options and any .dlls must appear before the C source file
    * .c extension is optional on input file
=== mcc.h 0 1 79/79 ===
#define __attribute__(x)
#define _WIN32
#define WIN32
#define __WIN32__
#define __inline
#define __dllimport(x)
#define __declspec(x)
#define __stdcall
#define CALLBACK $callback
#define __cdecl
#define EXTERN_C extern
#define DECLSPEC_IMPORT
#define __32BIT__
#define register
#define __MCCC__

typedef signed char		i8;
typedef short			i16;
typedef int				i32;
typedef long long int	i64;
typedef unsigned char			u8;
typedef unsigned short			u16;
typedef unsigned int			u32;
typedef unsigned long long int	u64;

typedef unsigned char byte;

typedef float r32;
typedef double r64;

=== END ===
1 ccp.m 0 0
2 pclp.m 0 0
3 pc_api.m 0 0
4 pc_decls.m 0 0
5 pc_diags_dummy.m 0 0
6 pc_reduce.m 0 0
7 pc_run.m 0 0
8 pc_runaux.m 0 0
9 pc_tables.m 0 0
10 mc_genmcl.m 0 0
11 mc_auxmcl.m 0 0
12 mc_libmcl.m 0 0
13 mc_stackmcl.m 0 0
14 mc_optim.m 0 0
15 mc_genss.m 0 0
16 mc_decls.m 0 0
17 mc_objdecls.m 0 0
18 mc_writeasm.m 0 0
19 mc_writeexe.m 0 0
20 mc_writeobj.m 0 0
21 mc_writess_dummy.m 0 0
22 mx_decls.m 0 0
23 mx_run.m 0 0
24 mx_lib.m 0 0
25 mx_write.m 0 0
26 cc_cli.m 0 0
27 cc_decls.m 0 0
28 cc_tables.m 0 0
29 cc_lex.m 0 0
30 cc_parse.m 0 0
31 cc_genpcl.m 0 0
32 cc_blockpcl.m 0 0
33 cc_libpcl.m 0 0
34 cc_lib.m 0 0
35 cc_support.m 0 0
36 cc_headers.m 0 0
37 cc_showdummy.m 0 0
38 info.txt 0 1
39 assert.h 0 1
40 ctype.h 0 1
41 errno.h 0 1
42 fenv.h 0 1
43 float.h 0 1
44 inttypes.h 0 1
45 stdint.h 0 1
46 limits.h 0 1
47 locale.h 0 1
48 _ansi.h 0 1
49 math.h 0 1
50 setjmp.h 0 1
51 signal.h 0 1
52 stdarg.h 0 1
53 stdbool.h 0 1
54 stddef.h 0 1
55 stdio.h 0 1
56 stdlib.h 0 1
57 _syslist.h 0 1
58 string.h 0 1
59 time.h 0 1
60 utime.h 0 1
61 unistd.h 0 1
62 safelib.h 0 1
63 wchar.h 0 1
64 wctype.h 0 1
65 types.h 0 1
66 stat.h 0 1
67 timeb.h 0 1
68 memory.h 0 1
69 fcntl.h 0 1
70 io.h 0 1
71 direct.h 0 1
72 process.h 0 1
73 malloc.h 0 1
74 conio.h 0 1
75 winsock2.h 0 1
76 _mingw.h 0 1
77 windowsx.h 0 1
78 cc_help.txt 0 1
79 mcc.h 0 1
