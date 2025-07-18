=== MA 24 ===
=== pc.m 0 0 1/24 ===
module pp_cli
module pp_decls
module pp_lex
module pp_parse

!$sourcepath "c:/bx/"
import pcl
!import pclint

=== pcl.m 0 0 2/24 ===
project =
	module pc_api
	module pc_decls

	module pc_diags
!	module pc_diags_dummy
	module pc_reduce

	module pc_run_dummy

! Tables (eg. types and IL opcodes)
	module pc_tables

	module mc_GenMCL
!	module mc_GenMCL_dummy
	module mc_AuxMCL
	module mc_LibMCL
	module mc_StackMCL
!	module mc_Optim

	module mc_GenSS_dummy

	module mc_Decls as md
	module mc_OBJdecls
	module mc_WriteASM
!	module mc_WriteASM_dummy
!	module mc_WriteASM_small
!	module mc_WriteNASM

	module mc_WriteEXE_dummy
	module mc_WriteOBJ_dummy

!	module mc_writess
!	module mc_disasm
	module mc_writess_dummy

!	module mx_decls
!	module mx_run
!	module mx_lib
	module mx_run_dummy

end

export byte pc_userunpcl=0

=== pc_api.m 0 0 3/24 ===
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




export func pcl_start(ichar name=nil, int nunits=0)psymbol=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

!CPL =PSTREC.BYTES
!CPL =PSTREC.NREFS
!CPL =PSTREC.LABELNO
!CPL =PSTREC.VARPARAMS
!CPL =PSTREC.EXPINDEX


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
	e.generic:=d			!assume d is the original
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

=== pc_decls.m 0 0 4/24 ===
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
!--
	byte id
	byte ksymbol				!0, or _rw code for reserved words (used by pp)
	byte opcode					!for opcode_rw
	byte subcode				!for jumpcc_rw/setcc_rw/type_rw

	i32 offset
!--
	struct						!for params only
		byte paramcat				!pc_reg/move/stack/spill
		byte fromreg				!pc_reg: move from .fromreg to .reg
	end
	byte mode
	byte addrof
	u32 size

!--
	byte nrefs
	byte reg
	byte reftype
	byte segment

	i16 stindex
	i16 importindex
!--
	i32 labelno

	u16 flags:(asmused:1, chasstatics:1, imported:1, exported:1,
				 isentry:1, atvar:1, used:1, hasdot:1,
			isthreaded:1, ishandler:1, ismain:1)

	byte dllindex				!for dllproc: which dll in dlltable

	byte nretvalues				!function: number of return values (0 for proc)
!--
	byte varparams				!0 or N; variadic params
	byte scope

	byte nparams
	byte variadic				!local function that uses ... variadic params (for C)

	i16 nlocals
	i16 impindex
!--
	i16 expindex
	u32 seqno

!CPL PCLREC.NREFS
!CPL PCLREC.LABELNO
!CPL PCLREC.VARPARAMS
!CPL PCLREC.EXPINDEX


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
						i32 nvariadics	! 0, or arg # that is first variadic
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
=== pc_diags.m 0 0 5/24 ===
const fshowppseqno=1
!const fshowppseqno=0

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

!PSSTR("<PCL>")
!PSSTR(OPNDNAMES[P.OPNDTYPE])

!psstr(strint(getlineno(p.pos),"4"))
!psstr(" ")

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
	when kproc, ktcproc then
		d:=p.def

		showprocinfo(d)

		if opcode=ktcproc then
			psstr("tcproc")
		else
			psstr("proc")
		fi

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
!		if p.popone then
!			strcat(str, "/1")
!		fi
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
		psstr(stropnd(p))
	fi
	pstabto(40)

	if fshowppseqno then
		psstr("! ")
		psstr(strint(p.seqno,"z5"))
		psstr("  ")
	fi
end

global func stropnd(pcl p)ichar=
	static[512]char str
!	static[32]char str
	int length
	psymbol d
	static ichar longstring

	if p=nil then
		return ""
	fi

	str[1]:=0

	case p.opndtype
	when int_opnd then
		return strint(p.value)
	when real_opnd, realimm_opnd, realimm32_opnd, r32_opnd then
!RETURN "<REAL>"
!CPL "HERE"
		if p.xvalue=infinity then
!CPL "INF"
!			fprint @str,"0x#",word@(p.xvalue):"h"
			fprint @str,"infinity"
		else
			print @str,p.xvalue:"e16.16"
		fi

!	when r32_opnd then
!		print @str, p.xvalue

!	when realimm_opnd, realimm32_opnd THEN
!		print @str,p.xvalue
!STRCAT(STR, OPNDNAMES[P.OPNDTYPE])


	when string_opnd then
		if (length:=strlen(p.svalue))<str.len/2 then
			strcpy(str,"""")
			convertstring(p.svalue,&.str+1)
			strcat(str,"""")

		else

			if longstring then
				pcm_free(longstring,longstringlen)
			fi
			longstringlen:=length*2
			longstring:=pcm_alloc(longstringlen)
			longstring^:='"'
			length:=convertstring(p.svalue, longstring+1)
			(longstring+length+1)^:='"'
			(longstring+length+2)^:=0
			return longstring
		fi

	when mem_opnd then
		d:=p.def
		strcat(str, p.def.name)

		if p.opcode in [kistatic, kzstatic] then
			strcat(str,":")
			if d.exported then
				strcat(str,":")
			fi
		fi

	when memaddr_opnd then
		strcpy(str, "&")
		recase mem_opnd

	when label_opnd then
		fprint @str,"## ","#",p.labelno

	when no_opnd then
		return ""

	when assem_opnd then
		return strint(int(p.asmcode))

	when data_opnd then
		fprint @str,"<Data * # (#)>", p.size,p.svalue

	else
		println "---------",OPNDNAMES[P.OPNDTYPE]
		return "<PCLOPND?>"
	esac

	return str
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

	for i to nplibfiles do
		psstr("addlib    """)
		psstr(plibfiles[i])
		psstr("""")
		psline()
	od
	psline() when nplibfiles

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

	if longstring then
		pcm_free(longstring,longstringlen)
	fi
	return dest
end

global proc psstr(ichar s)=
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
		if not d.ksymbol then
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
		fi
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
	if d.isthreaded then psstr(" TC") fi
!*!	if d.reg then psstr(" "); psstr(regnames[d.reg]) fi
	if d.hasdot then psstr(" Dot") fi
	if d.isentry then psstr(" ENTRY PT") fi

	if d.id=proc_id then psstr(" .PCADDR ="); PSSTR(STRINT(CAST(D.PCADDR),"H")) fi

	if d.owner then
		psstr(" (")
		psint(d.owner.seqno)
		psstr(" ")
		psstr(d.owner.name)
		psstr(")")
	fi	

!	if ctarget and d.id=static_id and d.cprocowner then
!		psstr(" (Proc:")
!		psstr(d.cprocowner.name)
!		psstr(") (D:")
!!		psint(cast(d.pcdata))
!		psstr(strint(cast(d.pcdata),"H"))
!		psstr(")")
!	fi
!	if ctarget and d.id=proc_id and d.chasstatics then
!		psstr(" Has statics")
!!		psint(d.chasstatics)
!	fi

	psline()
end

proc showprocinfo(psymbol d)=
	[256]char str
	procinfo p
	psymbol e

	p:=d.info
	return unless p

	fprint @str, "PROC INFO FOR: #", d.name
	psstrline(str)

	fprint @str, "  Params:   #", p.nparams
	psstrline(str)

	fprint @str, "  Locals:   #", p.nlocals
	psstrline(str)

	fprint @str, "  Leaf:     #", p.isleaf
	psstrline(str)

	fprint @str, "  Nmaxargs: #", p.nmaxargs
	psstrline(str)

	fprint @str, "  Assem:    #", p.assemused
	psstrline(str)

	fprint @str, "  MCLdone:  #", p.mcldone
	psstrline(str)

	fprint @str, "  Hasblocks:#", p.hasblocks
	psstrline(str)

	psline()
	e:=d.nextparam
	while e, e:=e.nextparam do
		fprint @str,"  Pm: # used:#, addrof:#",e.name, e.used, e.addrof
		psstrline(str)
	od
	e:=d.nextlocal
	while e, e:=e.nextlocal do
		fprint @str,"  Loc: # used:#, addrof:#",e.name, e.used, e.addrof
		psstrline(str)
	od
	psline()

end

=== pc_reduce.m 0 0 6/24 ===
export proc pcl_reducetest=
	int nn, seqno, lab, lab2, nargs
	pcl pc, newpc, pcnext, pcx, pcproc
	ref[]u16 labelmap
	psymbol pdef
	[maxcalldepth]pcl callstack
	int ncall
	int nprocs:=0, nleaf:=0, nallparams:=0, nalllocals:=0

	nn:=pccurr-pcstart+1

CPL =REDUCELABELS
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

CPL "DONE REDUCE"

	pccurr:=newpc
	pcm_free(labelmap, mlabelno) when reducelabels
end

=== pc_run_dummy.m 0 0 7/24 ===
export proc pcl_runpcl=
end
=== pc_tables.m 0 0 8/24 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,

		[0:]byte psigned,

		[0:]byte pint,
		[0:]byte pfloat,

		[0:]byte pmin,						!promoted type when min width applies
		[0:]byte piwrb =					!int/word/real/block

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

	(ksetarg,      $+1, 0, 2, 0, 0),  ! (0 - 0) (n         ) ?
	(kloadall,     $+1, 0, 0, 0, 0),  ! (0 - 0) (          ) ?

	(keval,        $+1, 1, 0, 0, 0),  ! (1 - 0) (          ) Evaluate Z [load to an actual register], then pop
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

=== mc_genmcl.m 0 0 9/24 ===
!const fshowpcl=1
!const fshowopndstack=1
const fshowpcl=0
const fshowopndstack=0

!global const docalltrace=1
!global const docalltrace=0

GLOBAL INT DEBUG

!global int framebytes, frameoffset, paramoffset

[pclnames.bounds]ref proc(pcl) px_handlertable

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
![6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global proc genmcl(ichar dummy=nil)=

!CPL "DOING GENMCL"
	return when mcldone

!CPL "DOING GENMCL2"

	IF FSHOWPCL OR FSHOWOPNDSTACK THEN CPL "********* ASM HAS PCL INFO *********" FI

!CPL =CURRFUNC

	int tt:=os_clock()

	inithandlers()
!	mclinit()
	initmcdest()

	currpcl:=pcstart

	int i:=0
	repeat
		convertpcl(currpcl)

		showopndstack() when fshowopndstack and currpcl.opcode not in [klabel, kcomment, kproc, ktcproc, kretproc, kendproc]

		++currpcl

	until currpcl>pccurr or currpcl.opcode=kendprog

CPL "DONE CONVERTPCL"
!
!	genrealtable()
!!CPL $LINENO
!	genabsneg()
	genstringtable()
!!CPL $LINENO

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)

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

	pmode:=p.mode
	currpcl:=p
	mmpos:=p.pos

	ppseqno:=p.seqno

	px_handlertable[p.opcode]^(p)

!	[r0..r15]byte OLDREGSET
!	pair oldregsetpr @ oldregset
!	OLDREGSET:=REGSET
	clear regset				!clear work reg flags

	int reg
!!
!	for i to noperands do
!		reg:=pclreg[i]
!		if reg then				!reset work reg occupied by a pcl opnd
!			regset[reg]:=1
!		fi
!	od

!	mccodex.regfreedpr.low ior:=oldregsetpr.low iand ((regsetpr.low ior isregvarpr.low) ixor invertbytes)
!	mccodex.regfreedpr.high ior:=oldregsetpr.high iand ((regsetpr.high ior isregvarpr.high) ixor invertbytes)
end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
!	clear rtsproclabels
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
		(ktoboolf, 		ktoboolt)

		(kcallf,		kcallp)
		(kicallp,		kcallp)
		(kicallf,		kcallp)

		(kendmx,		kresetmx)
		(ktcproc,		kproc)

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

proc unimpl(pcl p)=
	[100]char str
	fprint @str, "Unimpl: # (#)", pclnames[p.opcode], strpmode(pmode)
!	CPL STR
	mgencomment(pcm_copyheapstring(str))
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

proc px_nop*(pcl p) =
! ?
!*!	unimpl(p)
end

!proc px_dupl*(pcl p) =
!! Z' := Y' := Z
!	duplpcl()
!end

!proc px_double*(pcl p) =
!! Count extra instance of Z (only works for top stack item)
!	if ncalldepth then
!		duplpcl()
!	else
!		++pclcount[noperands]
!	fi
!end

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
!CPL $LINENO

	setsegment('C',1)
!CPL $LINENO

	genmc(m_procstart)
!CPL $LINENO
	mgendef(currfunc)
	genmc(m_labelname)
	mgendef(currfunc)
!CPL $LINENO

	do_proccode_a()
!CPL $LINENO
!create dummy mcl op at which to insert hang proc-entry code onto later
	mgencomment("?>>")
	mclprocentry:=mccodex
!CPL $LINENO

!	if currfunc.nparams=2 and currfunc.isentry then
!		fixmain()
!	fi
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
	genmc(m_labelname)
	mgendef(p.def)
end

proc px_zstatic*(pcl p) =
! Define zdata label and reserve sufficient space
	psymbol d

	d:=p.def
	setsegment('Z',p.align)
!	genmc(m_labelname,mgenmemaddr(d))
	genmc(m_labelname)
	mgendef(d)

	if d.exported then
		genmc(m_labelname)
		mgenname(getbasename(d.name))
	fi


	genmc(m_dotzero)
	mgenint(p.size)
end

proc px_data*(pcl p) =
! Constant data. For block types, there can be multiple C values
	int opc

	if p.mode=tpblock then
		do_blockdata(p)
		return
	fi

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
	genmc(opc)

	case p.opndtype
	when int_opnd then
		mgenint(p.value)
	when realimm_opnd then
		mgenrealimm(p.xvalue)
!	when realimm32_opnd then
!		mgenrealimm(p.xvalue)
!	when r32_opnd then
!		mgenrealimm(p.xvalue, tpr32)

	when string_opnd then
		mgenlabel(getstringindex(p.svalue))

	when memaddr_opnd then
		mgendef(p.def)
		moffset(p.extra)

	when label_opnd then
		mgenlabel(p.labelno)

	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

end

!proc do_blockdata(pcl p) =
!	ref byte s
!	ref u64 d
!	int n,nqwords,nwords,r
!
!	n:=p.size
!	return when n=0
!
!	nwords:=n/8
!
!	d:=cast(p.svalue)
!	to nwords do
!		genmc_int(m_dq, d++^)
!	od
!
!	r:=n-nwords*8
!	if r then
!		genstring_db(cast(d), r, 'B')
!	fi
!	MGENCOMMENT("ENDDATA")
!
!end
!
proc px_label*(pcl p) =
	genmc(m_label)
	mgenlabel(p.labelno)
end

proc px_jump*(pcl p) =
	int labno:=p.labelno
	pcl q:=p+1

!	while q.opcode=kcomment do ++q od
!	case q.opcode
!	when klabel then
!		if q.labelno=labno then return fi
!		++q
!		if q.opcode=klabel and q.labelno=labno then return fi
!	when kjump then
!		q.opcode:=knop
!	esac
!
	genmc_label(m_b, labno)
end

proc px_stop*(pcl p) =
! Stop Z
	psymbol d

!	loadparam(zz,tpu64, r10)
	loadopnd(zz,tpu64, r0)

!	genmc_reg(m_mov, r0)
!	mgenint(0)
!
	genmc(m_bl)
	mgenname("exit")

!	localshadow:=1
	poppcl()
end

proc px_retproc*(pcl p) =
! Return from proc
	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mgencomment("---")				!injection of entry code goes wrong otherwise
	fi

	do_proccode_b()
	do_proccode_c()
end

proc px_retfn*(pcl p) =
! Return from func with Z=retval
!*!	mclopnd ax,bx


	if pmode=tpblock then
MERROR("RETFN/BLOCK")
!		bx:=mgenireg(r0)								!r0 points to local block value
!		regset[r0]:=1
!		ax:=getworkregm(tpref)
!		genmc(m_mov, ax, mgenmem(blockretname))
!		ax:=mgenireg(ax.reg)
!		copyblock(ax, bx, p.size)
!		genmc(m_mov, mgenreg(r0, tpu64), mgenmem(blockretname))
	fi

	px_retproc(p)
end

proc px_load*(pcl p) =
! Z' := M &M L &L 123 4.5 "abc"

	pushpcl(p)
end

proc px_store*(pcl p) =
! M := Z
	int bx

	bx:=loadopnd(zz, p.mode)
	storeopnd(p, bx)
	poppcl()

end

proc px_eval*(pcl p) =
! Evaluate Z [load to an actual register], then pop

!MCOMM("EVAL")

	loadopnd(zz, p.mode, getworkreg(p.mode))
	poppcl()
end

proc px_add*(pcl p) =
! Z' := Y + Z
	do_binop(p, m_add, m_fadd)
end

proc px_sub*(pcl p) =
! Z' := Y + Z
	do_binop(p, m_sub, m_fsub)
end

proc px_mul*(pcl p) =
! Z' := Y + Z
	do_binop(p, m_mul, m_fmul)
end

proc px_setcall*(pcl p) =
! ?
	saveopnds()

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	fi

	++ncalldepth

	callblockret[ncalldepth]:=pmode=tpblock
	callblocksize[ncalldepth]:=p.size
	callpending[ncalldepth]:=0
end

proc px_setarg*(pcl p) =
! Mark Z as n'th argument (counting backwards)
	byte pending
	byte n:=p.x

!Note: not copying block args when they are stack args

	case p.y
	when 1 then							!pushed: first/odd operand
		pushopnds(zz, zz)
		poppcl()

	when 2 then							!first or second of pair

		pending:=callpending[ncalldepth]
		if pending then
			pushopnds(pending, zz)
			callpending[ncalldepth]:=0
			to zz-pending+1 do
				poppcl()
			od
		else
			callpending[ncalldepth]:=zz
		fi

	else								!register-passed
		if pmode=tpblock then			!need to record its size
			if n>8 then merror("block8") fi
			callargsize[ncalldepth, n]:=p.size
		fi

		poppcl()

	esac
end

=== mc_auxmcl.m 0 0 10/24 ===
!ref mclrec mclframesetup

global int nsaveregs, nsavefregs		!number of integer/float non-vols to be saved
global int nspilled						!spilled int/float registers

global int framesize					!counts bytes in stack frame
!global int framebytes, frameoffset, paramoffset
global int paramstackoffset
global int paramspilloffset
global int fplrsize						!0 or 16

global proc do_proccode_a=
!initialise genmcl pass through proc pcl code
	psymbol d
	ref procinforec pinfi
	int regargs, fregargs				!how many args can be in regs
	int reglocals, freglocals			!how many locals reside in non-vols
	int regmoved:=0, fregmoved:=0		!how many of f/regargs can be moved to nonvol regs
	ref procinforec pinfo

	clear regset
	int reg, xreg, n, fn, r, npregs, nextreg, nextfreg

	npregs:=0
	pinfo:=currfunc.info

	clear pcltempflags

	noperands:=0
	tempreg:=(currfunc.mode=tpblock|rnone|r8)

	framesize:=0

	nblocktemps:=0

	pinfo:=currfunc.info
	nsaveregs:=nsavefregs:=nspilled:=0

	(regargs, fregargs, reglocals, freglocals):=countregvars()

	unless fregoptim and pinfo then
		d:=currfunc.nextparam
		while d, d:=d.nextparam do			!set all regargs to 'spill'
			if d.paramcat=pmc_reg then
				d.paramcat:=pmc_spill
			fi
		od
		return
	end

	if not pinfo.isleaf then
!get number of args to be moved from their regs (rest need to be spilled)
		regmoved:=getmoveparams(regargs, 10, reglocals, 2)
		fregmoved:=getmoveparams(fregargs, 16, freglocals, 4)
	fi
	println =REGARGS, =FREGARGS, =REGLOCALS, =FREGLOCALS, =REGMOVED, =FREGMOVED

	nsaveregs:=regmoved+reglocals		!how many non-vols to be save in each file
	nsavefregs:=fregmoved+freglocals

!this next bit is tricky:
!* Scan paramlist and set pcm_move for f/regmoved args, and spill for rest
!  (This is for non-leaf)
!* Scan local vars and set reg code for first reglocals/freglocals
!  Note: start register will affected by f/regmoved

	nextreg:=regvara
	nextfreg:=xregvara

!for leaf, regarg settings can all stay
	if not pinfo.isleaf then		!all regargs to be moved, or set to spill
!do move/spill on regargs
		n:=regmoved
		fn:=fregmoved
		d:=currfunc.nextparam
		while d, d:=d.nextparam do
			if d.paramcat=pmc_reg then
				if pint[d.mode] and n then
					d.paramcat:=pmc_move
					d.fromreg:=d.reg
					d.reg:=nextreg++
					--n
				elsif pfloat[d.mode] and fn then
					d.paramcat:=pmc_move
					d.fromreg:=d.reg
					d.reg:=nextfreg++
					--fn
				else
!					d.reg:=rnone
					d.paramcat:=pmc_spill
				fi
			fi
		od

	fi

!At this point next/freg may be incremented
!Now scan locals and assign first reglocals/freglocals to next register
!It is easier to do separate loops for int/float

	nextreg:=regvara
	nextfreg:=xregvara
	d:=currfunc.nextlocal
	n:=reglocals
	while d and n, d:=d.nextlocal do
		if d.used and not d.atvar and not d.addrof and pint[d.mode] then
			d.reg:=nextreg++
			--n
		fi
	od

	d:=currfunc.nextlocal
	n:=freglocals
	while d and n, d:=d.nextlocal do
		if d.used and not d.atvar and not d.addrof and pfloat[d.mode] then
			d.reg:=nextfreg++
			--n
		fi
	od

!at this point:
!* All params have a PMC code
!* All pmc_reg params have .reg set
!* All pmc_move params have .reg/.fromreg
!* All locals which will reside in a regvar will have .reg set

!For the rest (spill/stack params, or ordinary locals, no frame offsets have been
!assign. That may be done at function end. It doesn't affect codegen since
!variables will be shown by name, not offset

!Actual spill/move code still has to be done

end

global proc do_proccode_b=
! Stack layout (grows downwards)
!	| ...
!	| Stack arg2
! A	| Stack arg1			There can be 0 stack args
!	| ----------
!	| LR					Saved FP/LR when used
! B	| FP
!	| ----------
!	! Spilled args			N slots, can be zero
!	| ----------
!	! Local vars			Ones not in regvars
!	| ----------
!	| Local Temps			Temps created during code gen
! C	| [Stack adj]			Extra slot may be added to keep stack/framesize 16-byte aligned
!	| ----------
!	| Saved nonvol regs		(May have dummy slot to keep alignment)
! D	| Saved nonvol fregs	(May also have dummy slot)
! Framesize used to step SP is C-B, which SP pointing to C.
! Nonvols are saved by pushing within the function's own stack space. Used code will
! then have SP starting to D



	int retmode, ntemps, hasequiv, offset, size, reg
!*!	mclopnd ax
	psymbol d
	[100]char str, newname
	int r, n

	setmclentry(mclprocentry)

	fplrsize:=16				!assume fp/lr are pushed

	framesize:=0

!	framesize+:=nsaveregs*8
!	if nsaveregs.odd then framesize+:=8 fi			!keep aligned
!
!	framesize+:=nsavefregs*8
!	if nsavefregs.odd then framesize+:=8 fi


	d:=currfunc.nextparam
	while d, d:=d.nextparam do if d.paramcat=pmc_spill then ++nspilled fi od

	framesize+:=nspilled*8							!may be odd; fixed later

!note that fp will be set to point to lower o pushed lr/fp

!stackargs: assumed that any stack-adj is at top end
	paramstackoffset:=fplrsize				!fp-rel offset of lowest stack arg				
	paramspilloffset:=-framesize			!fp-rel offset of lowest spilled location

	d:=currfunc.nextparam
	while d, d:=d.nextparam do
		if d.atvar then merror("@param") fi

		case d.paramcat
		when pmc_reg, pmc_move then
			genmc_def(m_definereg, d)

		when pmc_spill then
			genmc_def(m_define, d)

			d.offset:=paramspilloffset
!CPL D.NAME,=FPLRSIZE, =FRAMESIZE, =PARAMSPILLOFFSET, =NSPILLED,=D.OFFSET
			paramspilloffset+:=8

		when pmc_stack then
			genmc_def(m_define, d)
			d.offset:=paramstackoffset
			paramstackoffset+:=8

		esac
	od

!	retmode:=currfunc.mode

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
			genmc_def(m_definereg, d)

        else
			framesize+:=roundsizetg(size)
			d.offset:=-framesize
			genmc_def(m_define, d)
		fi
	od

	ntemps:=0
CPL =NTEMPS
!	for i to maxoperands when pcltempflags[i] do
!		++ntemps
!		frameoffset-:=8
!!*!		ax:=pcltempopnds[i]
!!*!		ax.offset:=frameoffset
!MGENCOMMENT("DEFINE3")
!!		genmc(m_define, mgenname(gettempname(currfunc,i)), mgenint(ax.offset))
!	od

	if framesize iand 8 then
CPL "FRAME ADJ"
		framesize+:=8				!keep stackframe a multiple of 16 bytes
	fi

!	MCOMM("FPLR:      ", STRINT(FPLRSIZE))
!	MCOMM("NSAVEREGS: ", STRINT(NSAVEREGS))
!	MCOMM("NSAVEFREGS:", STRINT(NSAVEFREGS))
!	MCOMM("NSPILLED:  ", STRINT(NSPILLED))
!	MCOMM("FRAMESIZE: ", STRINT(FRAMESIZE))

!	MCOMM("<FRAME SETUP CODE>")

	genmc_reg(m_push, rframe, rlink)
	genmc_reg(m_mov, rframe, rstack)
	if framesize then
		genmc_reg(m_sub, rstack)
		mgenint(framesize)
	fi

!MGENCOMMENT("SAVE NONVOL REGS")
	saveregs(nsaveregs, regvara)
	saveregs(nsavefregs, xregvara)

!	MGENCOMMENT("?]]")
!	MCLFRAMESETUP:=MCCODEX

!MGENCOMMENT("SPILL REG ARGS")
	spillparams()

!MCOMM("COPY PARAMS TO REGVARS?")

	MCOMM("---------------")
	resetmclentry()
end

global proc do_proccode_c=
	int offset

	MCOMM("---------------")

!	MCOMM("RESTORE NONVOL REGS")
	restoreregs(nsavefregs, xregvara)
	restoreregs(nsaveregs, regvara)

!
!	if framebytes or currfunc.nparams then
!		if usedregs[rframe] then
!MGENCOMMENT("POP FRAMEBYTES1")
!!			popstack(framebytes)
!!			genmc(m_pop, dframeopnd)
!		else
!			IF FRAMEBYTES THEN
!MGENCOMMENT("POP FRAMEBYTES2")
!!				popstack(framebytes+8)
!			FI
!		fi
!	fi

!	MCOMM("<FUNC LEAVE CODE>")

	if framesize then
		genmc_reg(m_add, rstack)
		mgenint(framesize)
	fi
!	genmc_reg(m_push, rframe, rlink)
	genmc_reg(m_pop, rlink, rframe)

	genmc(m_ret)
end

func countregvars: int, int, int, int =
!scan params in currfunc, and allocate pmc code.
!It returns:
! f/regargs: how many reg-passed args can stay in regs
! f/regvars: how many locals are eligible for regvars (limited by number of nonvol regs)
!regargs are guaranteed to be able stay in regs for leaf funcs; otherwise some
!can be moved to spare regvars, else spilled. But that is determined elsewhere
!here, .reg is only set for regargs, and hold register used to parsing (may be moved or spilled)

	psymbol d
	int ireg:=0, freg:=0, basereg			!ireg/freg traverse r0-r7 and v0-v7
	int regargs:=0, fregargs:=0				!how many args can be in regs
	int reglocals:=0, freglocals:=0			!how many locals reside in non-vols
!	int regmoved:=0, fregmoved:=0			!how many of f/regargs can be moved to nonvol regs
	byte skipregargs:=currfunc.ismain or currfunc.variadic

	ref int preg, pregargs					!point to either ireg/regargs or freg/fregargs

!	CPL "SCAN PARAMS FOR", CURRFUNC.NAME, CURRFUNC.VARIADIC
	d:=currfunc.nextparam

	while d, d:=d.nextparam do
!		println "Param", d.name:"12jl", pmcnames[d.paramcat]

		if pint[d.mode] or currfunc.variadic then
			preg:=&ireg
			pregargs:=&regargs
			basereg:=r0
		else
			preg:=&freg
			pregargs:=&fregargs
			basereg:=v0
		fi

		if preg^>=8 then				!keep on stack
			d.paramcat:=pmc_stack
		else
			d.reg:=basereg + preg^

			if skipregargs then			!main() or variadic (only for C)
				d.paramcat:=pmc_spill
				++(preg^)

			else
				if d.used then
					if not d.addrof then
						d.paramcat:=pmc_reg
						++(pregargs^)
					else
						d.paramcat:=pmc_spill
					fi
				else
					d.paramcat:=pmc_ignore
				fi
				++(preg^)
			fi
		fi
	od

	d:=currfunc.nextparam
	while d, d:=d.nextparam do
		println "Param", d.name:"12jl", strpmode(d.mode):"7jl", pmcnames[d.paramcat],
			(d.reg|strreg(d.reg,d.size)|"-")
	od

!count how many regvars will be used for i/f regs
	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do
		if d.used and not d.atvar and not d.addrof then
			if pint[d.mode] and reglocals<10 then
				++reglocals
			elsif pfloat[d.mode] and freglocals<16 then
				++freglocals
			fi
		fi
	od

	return (regargs, fregargs, reglocals, freglocals)
end

func getmoveparams(int npreg, rmax, &nlocals, pmin)int nmove=
!This is called for non-lead funcs only (leaf ones, params stay in their regs)
!It will return the number of those params that can be moved to non-vol registers
!normally used for local variables.
!It will be called once for int params, and again for floats
! npreg		Number or args in registers that are candidates for moving. (Not included
!			are ones that are no used/ignored, and ones that have & so need spilling)
! rmax		Number of non-vol registers available (10 for int, 16 for floats)
! nlocals	Number of those initially allocated to local vars, from 0 to rmax
! pmin		Is the minimum number param regs that ought to be moved (2 for int; 4 for floats)
!			Note pmin may be less than npreg; then it is adjusted down.

!It will return the number NPMOVE of npregs that can be moved (it will be first come first served)
!and it will be in the first section of the non-vols (from r19 or d8)
!To deliver the mininum, may mean reducing 'nlocals' in the caller
!So in the caller, the first NPMOVE of eligible params will have 'pmc_move' set.
!PMC codes or .reg are not changed here

	int spare

	pmin min:=npreg				!don't deliver minimums if there aren't that many params anyway

	spare:=rmax-nlocals			!how many spare nvreg slots available (0..rmax)

	if npreg<=spare then		!all can be accommodated
		return npreg
	fi

	nmove:=spare				!OK, only do that many

!look at minimums
	while nmove<pmin and nlocals do			!(nlocals shouldn't reach zero)
		--nlocals
		++nmove
	od

	nmove
end

proc spillparams=
	psymbol d
!*!	mclopnd ax
	int offset:=16, regoffset:=0, xregoffset, firstoffset

	d:=currfunc.nextparam
	while d, d:=d.nextparam do
		case d.paramcat
		when pmc_spill then
			genmc_rm(m_str, d.reg, rframe)
!CPL "SPILL", D.NAME, D.OFFSET,D.REG
!			moffset(d.offset)
			mgenint(d.offset)
			minside()
		when pmc_move then
!MCOMM("REGARG/MOVE",STRREG(D.REG), STRREG2(D.FROMREG))
			genmc_reg(m_mov, d.reg, d.fromreg)

		esac
	od

end

proc saveregs(int nregs, reg)=
	int n
	n:=0
	while n<nregs-1 do
		genmc_reg(m_push, reg, reg+1)
		reg+:=2
		n+:=2
	od
	if n<nregs then
		genmc_reg(m_push, reg, reg)
	fi
end

proc restoreregs(int nregs, reg)=
	int n
	return unless nregs

	reg+:=nregs-1				!refers to last reg rather than first

	if nregs.odd then
		genmc_reg(m_pop, reg, reg)
		--reg
		--nregs
	fi

	n:=0
	while n<nregs-1 do
		genmc_reg(m_pop, reg, reg-1)
		reg-:=2
		n+:=2
	od
end

global proc genstringtable=
	ref constrec p

	return unless cstringlist

	mgencomment("String Table")

	setsegment('I',8)

	if kk0used then
		genmc_label(m_label, kk0used)
		gendb(0)
	fi

	p:=cstringlist
	while p, p:=p.nextconst do
		genmc_label(m_label, p.labelno)
		genstring_db(p.svalue,length:-1, strtype:1)
	od
end

global proc genstring_db(ichar s, int length=-1, strtype)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
!strtype should be zero for a normal string, then a zero-terminator is added.
	int i, c, seqlen
	ref char seq

CPL "GENSTRING/DB", =LENGTH

	if length=-1 then
		length:=strlen(s)
	fi

	if length=0 then
		gendb(0)
		return
	fi

	genmc_string((strtype|m_asciiz|m_ascii), s)
end

proc gendb(int a)=
	genmc_int(m_db, a)
end

proc gendbstring(ichar s, int length)=
!string is printable, and doesn't include double quotes
	genmc(m_db)
	mgenstring(s)
end

proc gendq(int a)=
	genmc_int(m_dq, a)
end

global proc genrealtable=
	ref constrec p

	return unless creallist or cr32list

	mgencomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc_label(m_label, p.labelno)

		if p.xvalue=infinity then
			genmc_int(m_dq, u64@(p.xvalue))
		else
			genmc(m_dq)
			mgenrealimm(p.xvalue,tpr64)
		fi
	od

	mgencomment("Real32 Table")
	p:=cr32list
	while p, p:=p.nextconst do
		genmc_label(m_label, p.labelno)
		if p.xvalue=infinity then
			genmc_int(m_dd, int@(r32(p.xvalue)))
		else
			genmc(m_dd)
			mgenrealimm(p.xvalue,tpr32)
		fi

	od
end

global proc genabsneg=
	if lababs32+lababs64+labneg32+labneg64 then
		setsegment('I',16)
	fi

	if lababs32 then
		mgencomment("lababs32")
		genmc_label(m_label, lababs32)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mgencomment("lababs64")
		genmc_label(m_label, lababs64)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
		mgencomment("labneg32")
		genmc_label(m_label, labneg32)
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mgencomment("labneg64")
		genmc_label(m_label, labneg64)
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mgencomment("labzero")
		genmc_label(m_label, labzero)
		gendq(0)
	fi

	if labmask63 then
		mgencomment("mask63/offset64")
		genmc_label(m_label, labmask63)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc_label(m_label, laboffset64)
		gendq(0x43E0'0000'0000'0000)
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
		genmc_int(m_dq, d++^)
	od

	r:=n-nwords*8
!CPL "DOBLOCKDATA",=N,=NWORDS,=R
	s:=cast(d)
	for i to r do
		genmc_int(m_db, s++^)
	od
!
!	if r then
!!CPL "BLOCK/END"
!		genmc_string(m_ascii), s)
!!		genstring_db(cast(d), r, 0)
!	fi
!	MGENCOMMENT("ENDDATA")

end

proc setmclentry(mcl p)=
!temporarily set mcl insertion before p

	mce_oldmccodex:=mccodex
	mccodex:=p
	mce_lastmcl:=p.lastmcl
	mce_nextmcl:=p.nextmcl
end

func resetmclentry:mcl pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.lastmcl:=mce_lastmcl
	mccodex.nextmcl:=mce_nextmcl
	pnew:=mccodex
	mccodex:=mce_oldmccodex
	pnew
end

proc setmclentryf(mcl p)=
!temporarily set mcl insertion before p

	mcf_oldmccodex:=mccodex
	mccodex:=p
	mcf_lastmcl:=p.lastmcl
	mcf_nextmcl:=p.nextmcl
end

func resetmclentryf:mcl pnew =
!restore mcl insertion point to normal
!restireturn mcl instruction that followed	
	mccodex.lastmcl:=mcf_lastmcl
	mccodex.nextmcl:=mcf_nextmcl
	pnew:=mccodex
	mccodex:=mcf_oldmccodex
	pnew
end

global proc do_binop(pcl p, int iopc, fopc) =
! Z' := Y op Z
	int ax, bx, cx

	bx:=loadopnd(yy, p.mode)
	cx:=loadopnd(zz, p.mode)

	if isregvar(bx) then
		ax:=getworkreg(p.mode)
		pclreg[yy]:=ax
	else
		ax:=bx
	fi

	genmc_reg((pint[p.mode]|iopc|fopc), ax, bx, cx)

	poppcl()
end

=== mc_libmcl.m 0 0 11/24 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

export const ctarget=0

!global int mclseqno
EXPORT int mclseqno
EXPORT int NMCLOPND

[20]psymbol nametable
int nnametable

global macro isframex(d) = (d.id in [local_id, param_id])

!global macro mcomm = mgencomment

export proc mclinit(int bypass=0)=
	int r,s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") fi

	initmcdest()

	setsegment('C')

	lab_funcnametable:=0
	lab_funcaddrtable:=0

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

global proc genmc(int opcode)=				!used in do_mcl/assem in host
	mcl m, oldm
	int labno

	m:=pcm_allocnfz(mclrec.bytes)

	m.opcode:=opcode
	m.seqno:=++mclseqno
	m.mpos:=mmpos

!CPL "GENMC", MCLNAMES[OPCODE]

	if mccode then
		m.lastmcl:=mccodex
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
!CPL "GENMC", MCLNAMES[OPCODE], MCCODE, MCCODEX
end

global proc genmc_reg(int opcode, int a, b=rnone, c=rnone)=
!assume 1-3 reg args provided
	int nregs, rm

	genmc(opcode)

	mccodex.a:=a
	mccodex.b:=b
	mccodex.c:=c

	mccodex.asize:=8

	nregs:=1
	rm:=rm_reg
	if b then
		nregs:=2
		mccodex.bsize:=8
		if c then
			nregs:=3
			mccodex.csize:=8
		fi
	fi

	mccodex.nregs:=nregs
	mccodex.regmode:=rm

end

global proc genmc_rm(int opcode, int a, b)=
	genmc(opcode)

	mccodex.a:=a
	mccodex.b:=b
	mccodex.asize:=8
	mccodex.bsize:=8
	mccodex.nregs:=2
	mccodex.regmode:=rm_rm
end

global proc genmc_rrm(int opcode, int a, b, c)=
	genmc(opcode)

	mccodex.a:=a
	mccodex.b:=b
	mccodex.c:=c
	mccodex.asize:=8
	mccodex.bsize:=8
	mccodex.csize:=8
	mccodex.nregs:=3
	mccodex.regmode:=rm_rrm
end

global proc genmc_rmm(int opcode, int a, b, c)=
	genmc_rrm(opcode, a,b,c)
	mccodex.regmode:=rm_rmm
end

global proc genmc_cond(int opcode, int cond)=
	genmc(opcode)
	mccodex.condcode:=cond
end

global proc genmc_label(int opcode, int labelno)=
	genmc(opcode)
	mgenlabel(labelno)
end

global proc genmc_condlabel(int opcode, int cond, labelno)=
	genmc(opcode)
	mccodex.condcode:=cond
	mgenlabel(labelno)
end

global proc genmc_int(int opcode, int value)=
	genmc(opcode)
	mgenint(value)
end

global proc genmc_string(int opcode, ichar svalue)=
	genmc(opcode)
	mgenstring(svalue)
end

global proc genmc_name(int opcode, ichar svalue)=
	genmc(opcode)
	mgenname(svalue)
end

global proc genmc_def(int opcode, psymbol d)=
	genmc(opcode)
	mgendef(d)
end

global proc msetsize(int size)=
	mccodex.asize:=size
	mccodex.bsize:=size
	mccodex.csize:=size
end

global proc msetsizea(int size)=
	mccodex.asize:=size
end

global proc msetsizeb(int size)=
	mccodex.bsize:=size
end

global proc msetsizec(int size)=
	mccodex.csize:=size
end

global proc msuffix(int suffix)=
	mccodex.suffix:=suffix
end

global proc mregext(int opc, shift)=
	mccodex.regext:=opc
	mccodex.shift:=shift
end

global proc minside=
	mccodex.inside:=1
end

global proc mexcl=
	mccodex.excl:=1
end

global proc mlo12=
	mccodex.lo12:=1
end

global proc mhash=
	mccodex.hash:=1
end

global proc mgenint(int value)=
	mccodex.value:=value
	mccodex.valtype:=intimm_val
end

global proc mgenrealimm(real xvalue, int mode=tpr64)=
	mccodex.xvalue:=xvalue
	mccodex.valtype:=realimm_val
end

global proc mgenlabel(int labelno)=
	mccodex.labelno:=labelno
	mccodex.valtype:=label_val
end

global proc moffset(int offset)=
	mccodex.offset:=offset
end

global proc mgencomment(ichar s)=
	genmc(m_comment)
	mgenstring(s)
end

global proc mcomm(ichar s, t="", u="")=
	[256]char str
	print @str, s,t,u
	mgencomment(pcm_copyheapstring(str))
end

global proc mgenstring(ichar s)=
	mccodex.svalue:=pcm_copyheapstring(s)
	mccodex.valtype:=stringimm_val
end

!global proc mgenmemaddr(psymbol d)=
!	mccodex.def:=d
!	mccodex.valtype:=memaddr_val
!end

global proc mgendef(psymbol d)=
	mccodex.def:=d
	mccodex.valtype:=def_val
end

global proc mgenname(ichar s)=
	mccodex.svalue:=s
	mccodex.valtype:=name_val
end

!global func mgenname(ichar s)regopnd=
!	[64]char str
!	regopnd a
!	a:=newregopnd()
!	a.mode:=a_imm
!	a.svalue:=pcm_copyheapstring(s)
!	a.valtype:=name_val
!	a.size:=8
!
!	return a
!end
!
global proc setsegment(int seg,align=1)=
!!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
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
			oldalign:=mccodex.value
			if oldalign>=align then return fi
		fi
!*!		genmc(m_align,mgenint(align))
	fi
end

!global func applyoffset(regopnd a,int offset,int size=0)regopnd=
!!astr is an asm operand
!!add possible byte offset
!	regopnd b
!
!	if offset=0 and size=0 then
!		return a
!	fi
!	b:=duplopnd(a)
!	b.offset+:=offset
!	if size then
!		b.size:=size
!	fi
!
!	return b
!end
!
!export func mgenint(i64 x,int mode=tpi64)regopnd a=
!	int size:=psize[mode]
!
!	if x in -1..10 and size=8 then
!		return smallinttable[x]
!	fi
!
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	a.value:=x
!	a.valtype:=intimm_val
!	a.size:=size
!
!	return a
!end
!
!global func mgenint0(i64 x,int size=8)regopnd a=
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	a.value:=x
!	a.valtype:=intimm_val
!	a.size:=size
!
!	return a
!end
!
!global func mgenrealmem(r64 x,int mode=tpr64)regopnd a=
!	a:=newregopnd()
!	a.mode:=a_mem
!	if ispwide(mode) then
!		a.value:=getrealindex(x)
!	else
!		a.value:=getr32index(x)
!	fi
!	a.valtype:=label_val
!	a.size:=psize[mode]
!	return a
!end
!
!export func mgenrealimm(r64 x,int mode=tpr64)regopnd a=
!	a:=newregopnd()
!	a.mode:=a_imm
!	a.xvalue:=x
!	a.valtype:=realimm_val
!	a.size:=psize[mode]
!	return a
!end
!
!EXPORT func mgenlabel(int x=0)regopnd a=
!!x is a label index
!!generate immediate operand containing label
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	if x=0 then x:=++mlabelno fi
!	a.value:=x
!	a.valtype:=label_val
!
!	return a
!end
!
!global func mgenlabelmem(int x)regopnd a=
!!x is a label index
!!generate immediate operand containing label
!
!	a:=mgenlabel(x)
!	a.mode:=a_mem
!	return a
!end
!
!export func mgenmem(psymbol d, int mode=tpu64)regopnd a=
!	int reg
!
!	if d.reg then
!		if pfloat[d.mode] then
!			return mgenxregvar(d)
!		else
!			return mgenregvar(d, mode)
!		fi
!	fi
!
!	reg:=rnone
!	if isframex(d) then
!!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!!			return frameregtable[d.offset]
!!		fi
!
!		reg:=rframe
!		usedregs[rframe]:=1
!
!	fi
!
!	a:=newregopnd()
!	a.mode:=a_mem
!	a.reg:=reg
!	a.def:=d
!	++d.nrefs
!	a.valtype:=def_val
!
!	if mode then
!		a.size:=psize[mode]
!	else
!		a.size:=min(d.size,8)
!	fi
!
!	return a
!end
!
!EXPORT func mgenmemaddr(psymbol d)regopnd=
!	regopnd a
!
!	d.addrof:=1
!	++d.nrefs
!
!	a:=newregopnd()
!	a.mode:=a_imm
!
!	a.def:=d
!	++d.nrefs
!	a.valtype:=def_val
!	a.size:=8
!
!	return a
!end

export func mgenreg(int reg, size)regopnd a =
!reg is r0/r30/v0/v31
	a.reg:=reg
	a.size:=size

	a
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
!*!	genmc(m_label,mgenlabel(lab))
end

!global func mgenextname(ichar s)regopnd=
!	[64]char str
!	psymbol d
!	static [20]psymbol table
!	static int ntable
!
!	strcpy(&.str,s)
!	str[strlen(s)]:=0			!lose final *
!
!	d:=findnamesym(str)
!
!	if not d then
!		d:=pcm_allocnfz(pstrec.bytes)
!
!		d.name:=pcm_copyheapstring(&.str)
!		d.id:=import_id
!		d.imported:=1
!		addnamesym(d)
!	fi
!
!	return mgenmemaddr(d)
!end
!
!global func mgenregvar(psymbol d, int mode)regopnd a=
!	a:=mgenreg(d.reg, mode)
!!	isregvar[d.reg]:=1
!
!	return a
!end
!
!global func mgenxregvar(psymbol d)regopnd a=
!	a:=mgenxreg(d.reg)
!	isxregvar[d.reg]:=1
!
!	return a
!end

!global func getprimreg(regopnd ax)int =
!!get primary reg value; only one should be active
!!return 0 if no regs
!!//error if both regs are active
!
!	if ax.reg then
!!		if ax.regix then merror("getprim?") fi
!		ax.reg
!	else
!		ax.regix	!0 if no regs used
!	fi
!end

global proc pushslots(int nslots)=
!*!	pushstack(nslots*8)
!	mstackdepth+:=nslots
end

global proc popslots(int nslots)=
!*!	popstack(nslots*8)
!	mstackdepth-:=nslots
end

global proc pushstack(int n)=
	if n then
!*!		genmc(m_sub,dstackopnd,mgenint(n))
	fi
end

!global proc popstack(int n)=
!	if n then
!		genmc(m_add,dstackopnd,mgenint(n))
!	fi
!end
!
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
	CPL "AX ERROR:", mess, "AASEQ:", aaseqno
	CPL
	STOP 1

end

!global func newblocktemp(int size)psymbol=
!	[16]char str
!	psymbol d
!
!	if nblocktemps>maxblocktemps then
!		merror("Too many block temps")
!	fi
!	++nblocktemps
!
!	fprint @str,"$B#",nblocktemps
!	d:=pc_makesymbol(str, misc_id)
!	d.mode:=tpblock
!	d.size:=size
!	d.used:=1
!	d.id:=local_id
!	d.nextlocal:=currfunc.nextlocal
! 	d.owner:=currfunc
!	currfunc.nextlocal:=d
!
!	blockdefs[nblocktemps]:=d
!	d
!end

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

!func mgenstringx(ichar s)regopnd=
!	mgenlabelmem(getstringindex(s))
!end
!
!global proc clearreg(regopnd ax)=
!	if ax.size=8 then
!		ax:=changeopndsize(ax,4)
!	fi
!	genmc(m_xorx, ax, ax)
!end
!

=== mc_stackmcl.m 0 0 12/24 ===
!'PCS' Support - PCL Operand Stack 

global func loadopnd(int n, mode, reg=rnone)int ax =
!return registers containing operand
!get access mode for operand n

	pcl a
	psymbol d
	int labno, px, oldreg
!CPL "-------------LOAD OPND1", LOCNAMES[PCLLOC[N]]

	a:=pclopnd[n]

	case pclloc[n]
!	when reg_loc then
	when reg_loc, regvar_loc then
		oldreg:=pclreg[n]
		if reg<>rnone and oldreg<>reg then
			genmc_reg((reg<v0 | m_mov | m_fmov), reg, oldreg)
		else
			reg:=oldreg
		fi
		return reg

	when temp_loc then
MERROR("LOADOP/TEMP")
!		ax:=mgentemp(n, mode)
!		return ax
	esac

	ax:=getworkreg(mode, reg)

!CPL "-------------LOAD OPND", OPNDNAMES[A.OPNDTYPE]
	case a.opndtype
	when mem_opnd then
		d:=a.def
		if mode=tpblock and d.id<>param_id then
			mode:=tpu64
			domemaddr
		else
domem:
			if d.id in [param_id, local_id] then		!stack frame
!CPL "MEM/LOCAL"
				genmc_rm(m_ldr, ax, rframe)
				msetsizea(d.size)
				mgendef(d)
				minside()
			else										!static
!CPL "MEM/STATIC"
				px:=gettempreg()
				genmc_reg(m_adrp, px)
				mgendef(d)
				genmc_reg(m_add, px, px)
				mgendef(d)
				mlo12()
				genmc_rm(m_ldr, ax, px)
				msetsizea(d.size)
			fi
		fi

	when memaddr_opnd then
		d:=a.def
		if d.id=param_id and d.mode=tpblock then		!pcl mode will be u64
			domem
		else
domemaddr:
			if d.id in [param_id, local_id] then		!stack frame
				genmc_reg(m_add, ax, rframe)
				mgendef(d)

			else										!static
				genmc_reg(m_adrp, ax)
				mgendef(a.def)
				genmc_reg(m_add, ax, ax)
				mgendef(a.def)
				mlo12()
			fi
		fi

	when int_opnd then
		genmc_reg(m_mov, ax)
		mgenint(a.value)

	when real_opnd, r32_opnd then
		genmc_reg(m_fmov, ax)
		mgenrealimm(a.xvalue)

	when string_opnd then
		labno:=getstringindex(a.svalue)
		loadlabel

	when label_opnd then
		labno:=a.labelno
loadlabel:
		genmc_reg(m_adrp, ax)
		mgenlabel(labno)
		genmc_reg(m_add, ax, ax)
		mgenlabel(labno)
		mlo12()

	else
error:
		merror("getopnd", opndnames[a.opndtype])
	esac

	ax
end

global proc storeopnd(pcl p, int ax) =
!p is a kstore op; store value in register to variable specified by p.def
!That variable is top pcl stack item, but it is popped by the caller
!The caller will already have loaded that item to the register

	int px
	psymbol d

	if p.mode<>tpblock then
		d:=p.def

		if d.id in [param_id, local_id] then		!stack frame
			if d.reg then
				genmc_reg((pfloat[p.mode]|m_fmov|m_mov), d.reg, ax)
			else
				genmc_rm(m_str, ax, rframe)
				mgendef(d)
				minside()
			fi
		else
			px:=gettempreg()
			genmc_reg(m_adrp, px)
			mgendef(d)
			genmc_reg(m_add, px, px)
			mgendef(d)
			mlo12()
			genmc_rm(m_str, ax, px)
		fi
		msetsizea(p.size)	

	else
		MERROR("STOREOPND/BLOCK")

!		ax:=getworkireg()
!		genmc(m_lea, ax, mgenmem(p.def, tpu64))
!		ax:=makeopndind(ax, tpu64)
!
!		bx:=makeopndind(bx, tpu64)
!		copyblock(ax, bx, p.size)
	fi

end

!global func OLDloadopnd(int n, mode, reg = rnone)mclopnd ax =
!!Load operand to designated register reg. If not provided, one is allocated
!!If operand resides in a register already, and reg=0, then that is what is
!!returned. But if it will be modified, either REG is needed, or an alternate
!!scheme is needed to force a load to a different register
!
!	ax:=getopnd(n, mode, reg)
!
!	if pclloc[n]=regvar_loc then			!force a load to a workreg
!		if reg=rnone then
!			reg:=getworkreg(mode)
!		fi
!	fi
!
!	ax:=loadtoreg(ax, mode, reg)
!
!	pclopnd[n]:=nil
!	pclloc[n]:=reg_loc
!	pclreg[n]:=ax.reg
!
!	ax
!end

!global func loadparam(int n, mode, reg)mclopnd ax =
!!Load operand to designated arg reg.
!!If operand resides in a register already, and reg=0, then that is what is
!!returned. But if it will be modified, either REG is needed, or an alternate
!!scheme is needed to force a load to a different register
!	ax:=getopnd(n, mode, reg)
!	ax:=loadtoreg_m(ax, mode, reg)
!	ax
!end
!
!global func loadretval(int n, mode, reg)mclopnd ax =
!!Load operand to return register
!!reg will be r0 for most functions
!	ax:=getopnd(n, mode, reg)
!	ax:=loadtoreg_m(ax, mode, reg)
!	ax
!end

global proc pushopnds(int a, b)=
!Push opnds a, b to hardware stack
	int ax, bx

	ax:=loadopnd(a, pclmode[a])

	if a=b then
		genmc_reg(m_push, ax, ax)
	else
		bx:=loadopnd(b, pclmode[b])
		genmc_reg(m_push, ax, bx)
	fi

end

!global func loadtoreg(mclopnd ax, int mode, reg)mclopnd=
!!if ax is not a register operand, then load it to given register
!!mode is needed to give type of register (float etc) and size
!!It is assumed that if ax /is/ in a register, that it is the correct one, or doesn't matter
!	mclopnd bx
!
!	if ax.mode in [a_reg, a_xreg] then			!might already be in reg
!		if not reg or ax.reg=reg then
!			return ax
!		fi
!	fi
!
!	bx:=getworkreg_rm(reg, mode)
!
!	loadtoreg_common(bx, ax)
!
!	bx
!end

!global func loadtoreg_m(mclopnd ax, int mode, reg)mclopnd=
!!same as loadtoreg but if already in a register, will move it to required one if needed
!	mclopnd bx
!
!	if ax.mode in [a_reg, a_xreg] then			!already in register
!		if ax.reg=reg then return ax fi			!in correct one
!	fi
!
!!need loading/moving to given reg
!	bx:=mgenreg(reg, mode)
!
!	loadtoreg_common(bx, ax)
!!	genmc(m_mov, bx, ax)
!	bx
!end

!proc loadtoreg_common(mclopnd bx, ax)=
!	if ax.mode=a_imm and ax.valtype=intimm_val and ax.value=0 then
!		bx:=changeopndsize(bx,4)
!		clearreg(bx)
!!		genmc(m_xorx, bx, bx)
!	
!	else
!		genmc(m_mov, bx, ax)
!	fi
!
!end

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

!global proc pushpcl_reg(int mode, reg=rnone)=
!!Push a new, empty pcs slot located in given register
!	int n
!
!	if noperands>=maxoperands then
!		merror("PCL stack overflow")
!	fi
!
!	if reg=rnone then reg:=getworkreg(mode) fi
!
!	n:=++noperands
!
!	pclloc[n]:=reg_loc
!	pclopnd[n]:=nil
!	pclreg[n]:=reg
!	pclcount[n]:=1
!	pclmode[n]:=mode
!
!	if ispfloat(mode) then
!		xregset[reg]:=1
!	else
!		regset[reg]:=1
!	fi
!
!end

global proc poppcl=
	int n:=noperands

	if n<=0 then merror("poppcl/underflow") fi

	if pclcount[n]>1 then
		--pclcount[n]
		return
	fi

	--noperands
end

!global proc duplpcl=
!!ensure zz is in a register, duplicate into a new register
!	int mode:=pclmode[zz]
!
!	loadopnd(zz, mode)							!get zz to reg
!!	pushpcl_reg(getworkreg(mode), mode)				!create new zz opnd, old is now yy
!!*!	pushpcl_reg(mode)							!create new zz opnd, old is now yy
!
!!MCOMM("DUPLOP")
!	genmc(m_mov, getopnd(zz, mode), getopnd(yy, mode))	!copy old to new
!end

global func getworkireg:int r=

	to 10 do
		for r in workrega..workregb do
			if regset[r]=0 then
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
	for r in workxrega..workxregb do
		if regset[r]=0 then
			regset[r]:=1
			return r
		fi
	od
	merror("No more work xregs")
	0
end

global func getworkreg(int mode, reg=rnone)int=
!return new work reg depending on mode
	if reg then return reg fi

	if ispfloat(mode) then
		getworkxreg()
	else
		getworkireg()
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
!	mclopnd tx

	return unless pclloc[n]=reg_loc

	reg:=pclreg[n]
	mode:=pclmode[n]

	if allregs or reg in workrega..workregb or reg in workxrega..workxregb then
MCOMM("SAVE TO TEMP")
!*!		genmc_reg(m_mov, mgentemp(n,mode), mgenreg(reg,mode))
	fi
	regset[reg]:=0

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

	if regset[newreg] then
		for i to noperands do
			if pclreg[i]=newreg then
				swapopnds(i,zz)
MCOMM("EXCH REGS")
!				genmc(m_xchg, mgenreg(oldreg, tpu64), mgenreg(newreg,tpu64))
				retry
			fi
		od
	fi

	genmc_reg((newreg<v0 |m_mov|m_fmov), newreg, oldreg)

	pclreg[zz]:=newreg

	regset[newreg]:=1
end

!global func getopnd_ind(int n=noperands, mode=tpi64)mclopnd=
!!Get access mode to operand which is to be used as a pointer elsewhere
!!So it needs first to in a register, if not already
!	pcl a
!	psymbol d
!
!	if pclloc[n]=pcl_loc then
!		a:=pclopnd[n]
!		if a.opndtype=memaddr_opnd then
!			d:=a.def
!			unless d.id=param_id and d.mode=tpblock then
!				return mgenmem(a.def, mode)
!			end
!		fi
!	fi
!
!	if pclloc[n]<>reg_loc then
!		loadopnd(n, tpu64)
!	fi
!
!	return mgenireg(pclreg[n], mode)
!end

!global func getopnd_ind_simp(int n=noperands, mode=tpi64)mclopnd=
!!version of getopnd_ind which always returns [reg]
!
!	if pclloc[n]<>reg_loc then
!		loadopnd(n, tpu64)
!	fi
!
!	return mgenireg(pclreg[n], mode)
!end

global proc swapopnds(int m,n)=
!exchange pcl stack operands
	swap(pclopnd[m],	pclopnd[n])
	swap(pclloc[m],		pclloc[n])
	swap(pclreg[m],		pclreg[n])
	swap(pclmode[m],	pclmode[n])
	swap(pclcount[m],	pclcount[n])
end

!global func isimmload(int n)pcl p=
!!return nil if operand is not immediate integer
!!otherwise return the pcl operand
!
!	p:=pclopnd[n]
!	if pclloc[n]=pcl_loc and p.opcode=kload and p.opndtype=int_opnd then p else nil fi
!end

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

!Then set the regs still in use as pcl opnds:

	for i to noperands do
		reg:=pclreg[i]
		if pclreg[i] then
			regset[reg]:=1
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

!global func makeopndind(mclopnd a, int mode=tpvoid)mclopnd=
!	mclopnd b
!
!	if a.mode<>a_reg then
!		merror("makeopndind")
!	fi
!
!	return mgenireg(a.reg, mode)
!end

!global func makesimpleaddr(mclopnd ax)mclopnd bx=
!!assume ax is an ireg, but need a simple one with areg set but not ireg
!	int newreg, reg, regix
!
!	reg:=ax.reg
!	regix:=ax.regix
!	if reg=rframe then reg:=rnone fi
!
!	if ax.mode<>a_mem then merror("MSA") fi
!
!	if reg=rnone and regix=rnone then
!		newreg:=getworkireg()
!	elsif reg then				![reg] only; already simple
!		return ax
!	elsif regix then			![regix] only; may be scaled; use lea anyway
!		newreg:=regix
!	else						![reg+regix]
!		newreg:=regix
!	fi
!
!	bx:=mgenireg(newreg)
!
!	genmc(m_lea, mgenreg(newreg), ax)
!	return bx
!end

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
			strcat(s, strreg(pclreg[i]))
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

	strcat(s,"IWR:(")
!	for r:=r0 to r9 when workregs[r] do
	for r in workrega..workregb  do
		strcat(s,(regset[r]|"1 "|"0 "))
	od
	strcat(s,") ")

	strcat(s,"FWR:(")
	for r in workxrega..workxregb  do
		strcat(s,(regset[r]|"1 "|"0 "))
	od

	strcat(s,") noperands:")
	strcat(s,strint(noperands))
	strcat(s," ncalldepth:")
	strcat(s,strint(ncalldepth))

	return s
end

global proc showopndstack=
	mgencomment(stropndstack(1))
end

global func gettempreg:int=
	if tempreg then
		return tempreg
	fi
	return getworkireg()
end

global func isregvar(int reg)int=
	if reg in regvara..regvarb or reg in xregvara..xregvarb then
		1
	else
		0
	fi
end
=== mc_genss_dummy.m 0 0 13/24 ===
export int psstime

global proc genss(int obj=0)=
end
=== mc_decls.m 0 0 14/24 ===

export record regopnd =
	byte reg
	byte size
end

!export record mclrec = $caligned
export record mclrec =
	mcl lastmcl, nextmcl
	union
		struct
			byte a, b, c
		end
		[3]byte regs
	end
	union
		struct
			byte asize, bsize, csize
		end
		[3]byte sizes
	end

	byte opcode
	byte regmode				!which of r/r,r etc (see rm_mode enums)

	union
		i64 value
		r64 xvalue
		ichar svalue
		int labelno
		psymbol def
	end
	i32	offset					!additional label offset
	byte regext					!LSL ASR UXxx SXxx opcode used to shift/ext reg
	byte shift					!shift amount for regext
	byte flags: (excl:1, hash:1, inside:1, lo12:1)		!"!", "#", or label inside []
	byte valtype				!which of value/xvalue...def are in use

	u32 seqno
	u32 mpos

	union
		u16 suffix				!opcode extension (eg. 'SB'
		byte condcode			!cond for either Bcc suffix, or as final operand
	end
	byte nregs					!number of reg opnds, 0 to 3

end

export type mcl = ref mclrec

export enumdata [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(intimm_val,	$),		!immediate int
	(realimm_val,	$),		!immediate real (mainly for dq etc)
	(realmem_val,	$),		!indirect real (for movq etc)
	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
	(def_val,		$),		!var/proc name
	(label_val,		$),		!label index
!	(labelind_val,	$),		!label index
	(name_val,		$),		!immediate string must be output as an unquoted name
	(temp_val,		$),		!index of pclopnd temp (later becomes ptr to descriptor?)
!	(syscall_val,	$),		!
end

export enumdata []ichar mclnames =

	(m_procstart,	$),
	(m_procend,		$),
	(m_comment,		$),
!	(m_blank,		$),
!	(m_deleted,		$),
	(m_labelname,	$),
	(m_define,		$),
	(m_definereg,	$),
	(m_trace,		$),
	(m_endx,		$),

	(m_label,		$),
	(m_nop,			$),

	(m_db,			"m_.byte"),
	(m_dw,			"m_.half"),
	(m_dd,			"m_.word"),
	(m_dq,			"m_.xword"),
	(m_ascii,		"m_.ascii"),
	(m_asciiz,		"m_.asciz"),

	(m_isegment,	"m_.data"),
	(m_zsegment,	"m_.data"),
	(m_csegment,	"m_.text"),

	(m_align,		$),
	(m_resb,		$),
	(m_resw,		$),
	(m_resd,		$),
	(m_resq,		$),

	(m_add,			$),
	(m_adr,			$),
	(m_adrp,		$),
	(m_and,			$),
	(m_asr,			$),
	(m_b,			$),
	(m_bcc,			$),
	(m_bl,			$),
	(m_blr,			$),
	(m_br,			$),
	(m_cmn,			$),
	(m_cmp,			$),
	(m_csel,		$),
	(m_cset,		$),
	(m_eor,			$),

	(m_fabs,		$),
	(m_fadd,		$),
	(m_fcmp,		$),
	(m_fcmpe,		$),
	(m_fcvt,		$),
	(m_fcvtzs,		$),
	(m_fdiv,		$),
	(m_fmov,		$),
	(m_fmul,		$),
	(m_fneg,		$),
	(m_fsub,		$),


	(m_ldp,			$),
	(m_ldr,			$),
	(m_lsl,			$),
	(m_lsr,			$),
	(m_mov,			$),
	(m_movi,		$),
	(m_movk,		$),
	(m_mul,			$),
	(m_mvn,			$),
	(m_neg,			$),
	(m_negs,		$),
	(m_orr,			$),
	(m_ret,			$),
	(m_ror,			$),
	(m_scvtf,		$),
	(m_sdiv,		$),
	(m_smull,		$),
	(m_stp,			$),
	(m_str,			$),
	(m_sub,			$),
	(m_sxtb,		$),
	(m_sxth,		$),
	(m_sxtw,		$),
	(m_udiv,		$),
	(m_umulh,		$),
	(m_umull,		$),
	(m_uxtw,		$),

	(m_dotzero,		"m_.zero"),
	(m_push,		$),
	(m_pop,			$),

end

export enumdata [0:]ichar regnames =
	(rnone=0,	$),			! means no register/non used

	(r0,		$),			!integer regs (X/W in asm)
	(r1,		$),
	(r2,		$),
	(r3,		$),
	(r4,		$),
	(r5,		$),
	(r6,		$),
	(r7,		$),
	(r8,		$),
	(r9,		$),
	(r10,		$),
	(r11,		$),
	(r12,		$),
	(r13,		$),
	(r14,		$),
	(r15,		$),
	(r16,		$),
	(r17,		$),
	(r18,		$),
	(r19,		$),
	(r20,		$),
	(r21,		$),
	(r22,		$),
	(r23,		$),
	(r24,		$),
	(r25,		$),
	(r26,		$),
	(r27,		$),
	(r28,		$),
	(r29,		$),			!also rframe
	(r30,		$),			!also rlink

	(rstack,	$),			!special regs
	(rzero,		$),

	(v0,		$),			!float regs (D/S/H/B in asm)
	(v1,		$),
	(v2,		$),
	(v3,		$),
	(v4,		$),
	(v5,		$),
	(v6,		$),
	(v7,		$),
	(v8,		$),
	(v9,		$),
	(v10,		$),
	(v11,		$),
	(v12,		$),
	(v13,		$),
	(v14,		$),
	(v15,		$),
	(v16,		$),
	(v17,		$),
	(v18,		$),
	(v19,		$),
	(v20,		$),
	(v21,		$),
	(v22,		$),
	(v23,		$),
	(v24,		$),
	(v25,		$),
	(v26,		$),
	(v27,		$),
	(v28,		$),
	(v29,		$),
	(v30,		$),
	(v31,		$),
end

export const rframe = r29
export const rlink  = r30

export const rfirst = r0
export const rlast = v31

export enumdata [0:]ichar condnames, [0:]int asmrevcond =

	(eq_cond=0,	$, 	ne_cond),
	(ne_cond, 	$, 	eq_cond),

	(cs_cond, 	$, 	cc_cond),		!carry set / unsigned >= (also hs)
	(cc_cond, 	$, 	cs_cond),		!carry clear / unsigned < (or lo)

	(mi_cond, 	$, 	pl_cond),		!minus
	(pl_cond, 	$, 	mi_cond),		!plus (>= 0)

	(vs_cond, 	$, 	vc_cond),		!signed overflow (v set)
	(vc_cond, 	$, 	vs_cond),		!no signed overflow (v clear)

	(hi_cond, 	$, 	ls_cond),		!unsigned >
	(ls_cond, 	$, 	hi_cond),		!unsigned <=

	(ge_cond, 	$, 	lt_cond),		!signed >=
	(lt_cond, 	$, 	ge_cond),		!signed <

	(gt_cond, 	$, 	le_cond),		!signed >
	(le_cond, 	$, 	gt_cond),		!signed <=

	(al_cond, 	$, 	nv_cond),		!always
	(nv_cond, 	$, 	al_cond),		!always
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

!reg-operand pattern
export enumdata [0:]ichar rmnames =
	(rm_none=0,	$),			! no regs used
	(rm_reg,	$),			! R [,R [,R]]  Use .nregs for count
	(rm_rm,		$),			! R, [R]
	(rm_rrm,	$),			! R, R, [R]
	(rm_rmm,	$),			! R, [R, R]
end

global enumdata [0:]ichar pmcnames =
	(pmc_ignore=0,	"Ignore"),
	(pmc_stack,		"Stack"),
	(pmc_reg,		"Reg"),
	(pmc_spill,		"Spill"),
	(pmc_move,		"Move"),
end	

global const maxoperands=20

!following are continually updates as opnds are pushed, moved and popped
global [maxoperands]pcl		pclopnd			!pclrec describing opnd when not loaded
global [maxoperands]byte	pclreg			!>0 means in given register
global [maxoperands]byte	pclmode			!copy of mode, esp. if loaded (indicates reg/xreg)
global [maxoperands]byte	pclcount		!dupl count
global [maxoperands]byte	pclloc			!stores loc code

!following are reset per proc and augmented as it is processed
global [maxoperands]byte pcltempflags		!1 means a temp uses local storage
global [maxoperands]mclrec pcltempopnds	!store mcl opnd for such a temp

global int noperands						!number of pcl operands, including wide

global enumdata [0:]ichar locnames =
	(pcl_loc=0,	"pend"),				!operand still in pcl instruction
	(reg_loc,	"reg"),					!is in register (look at mode for reg/xreg)
	(regvar_loc,"regvar"),				!lives in register (look at mode for reg/xreg)
	(temp_loc,	"temp"),				!overflow to temporary
end

global [rfirst..rlast]byte regset		!register in-use flags: 0/1: free/in-use
!global [rfirst..rlast]byte isregvar		!1 if reg var and in-use

global const workrega  = r9,  workregb  = r15
global const workxrega = v16, workxregb = v31

global const regvara  = r19,  regvarb  = r28
global const xregvara  = v8,  xregvarb  = v15

global int tempreg						!helper reg: r7/r8 or rnone

!global record pair =
!	u64 low, high
!end

!global pair regsetpr @ regset
!global pair isregvarpr @ isregvar
global const u64 invertbytes = 0x0101'0101'0101'0101

!global [rfirst..rlast]byte usedregs		!1 means used during proc

global byte noxorclear					!1 to suppress xor optimisation

global macro zz = noperands
global macro yy = noperands-1
global macro xx = noperands-2
global macro ww = noperands-3

global const maxcalldepth=16
!global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret	!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize	!size of any returned block
global [maxcalldepth,8]u32 callargsize	!size of any block pushed in low args
global [maxcalldepth]byte callpending	!opnd number waiting for a paired opnd for push

global int ncalldepth

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

export mcl mccode, mccodex		!genmc adds to this linked list

global int currsegment=0

global regopnd dstackopnd
global regopnd dframeopnd
global regopnd noreg

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

!The following are highly dependent on the ordering of the base types being:
! r32 r64 ints... block ..., with r32 having value 1
!They assume mode is not void, and for ispfloat, is not a block

global macro ispwide(m)  = m - 1
global macro ispfloat(m) = m <= tpr64
global macro ispint(m)   = m > tpr64	!when block type is not expected

EXPORT [1..8]byte regmodes=(tpu8, tpu16, 0, tpu32, 0,0,0, tpu64)

global byte pmode
global pcl currpcl

global mcl mclprocentry
global mcl mce_oldmccodex, mce_lastmcl, mce_nextmcl		!used by reset/setmclentry
global mcl mcf_oldmccodex, mcf_lastmcl, mcf_nextmcl		!used by reset/setmclentry for frame setup

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


!global [pstdnames.bounds]byte ploadopx
!
!global [pstdnames.bounds]byte ploadop
!
proc start=
!	for i in ploadop.bounds do ploadop[i]:=m_nop od
!
!	ploadop[tpu8]:=ploadop[tpu16]:=ploadop[tpu32]:=m_movzx
!	ploadop[tpi8]:=ploadop[tpi16]:=ploadop[tpi32]:=m_movsx
!	ploadop[tpr32]:=m_movd
!	ploadop[tpr64]:=m_movq
!	ploadop[tpu64]:=ploadop[tpi64]:=m_mov
end

=== mc_objdecls.m 0 0 15/24 ===
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
=== mc_writeasm.m 0 0 16/24 ===
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

![8, r0..r15]ichar nregnames
!
[regnames.bounds]psymbol regvars		!nil, or strec when it uses that reg
![r0..r15]psymbol xregvars

proc writemcl(int index,mcl m)=

!	case m.opcode
	
!	if m.opcode=m_comment and m.a.svalue^='?' then
!	else
		strmcl(m)
		gs_line(pdest)
!	fi
!	esac
end

global proc strmcl(mcl m)=
	static [512]char str
	[128]char opcname
	[16]char ccstr
!	mclopnd a,b
	int opcode,cond,sizepref
	ichar s,comment
	psymbol d

	opcode:=m.opcode
	str[1]:=0

!	cond:=m.cond
!	a:=m.a
!	b:=m.b
	comment:=nil

!CPL "STRMCL", MCLNAMES[M.OPCODE]

	case opcode
	when m_procstart then
		d:=m.def
		asmstr("# Proc ")
		asmstr(d.name)

		currasmproc:=m.def
		clear regvars

		return

	when m_procend then
		asmstr("# End\n")
		currasmproc:=nil

		return

	when m_comment then
		asmstr("# ")
		asmstr(m.svalue)
		return
	when m_endx then
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		case m.valtype
		when def_val then
			d:=m.def
			asmstr(getdispname(d))
			if d.id=proc_id and d.exported then
				asmstr(":\n")
				asmstr(getbasename(d.name))
			fi

		when stringimm_val, name_val then
			asmstr(m.svalue)
!			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")
		

		return

	when m_label then
		if m.valtype=label_val then
			fprint @str,"L#:",m.value
		else
			recase m_labelname
		fi
		asmstr(str)
		return

	when m_define then
		d:=m.def
		asmstr("    .set ")
		asmstr(getdispname(d))
		asmstr(", ")
!CPL "///",=D.OFFSET, D.NAME

		asmint(d.offset)
		return

	when m_definereg then
		d:=m.def
		asmstr("    ")
		asmstr(getdispname(d))
		regvars[d.reg]:=d

!		asmstr(a.svalue)
		asmstr(" .req ")

		asmstr(strreg(d.reg, d.size))

!		case b.mode
!		when a_reg then
!			asmstr(getregname(b.reg, b.size))
!
!		else
!			asmstr(getxregname(b.reg, b.size))
!		esac
		return
	esac

	strcpy(opcname, mclnames[opcode]+2)

	case opcode
	when m_bcc then
		strcpy(ccstr, condnames[m.condcode])
		ccstr[3]:=0				!first two letters only
CPL =M.CONDCODE, condnames[m.condcode]
		strcpy(&opcname[2], ccstr)

	when m_ldr, m_str then
		if m.suffix then
			opcname[4]:=m.suffix
			opcname[5]:=m.suffix>>8
			opcname[6]:=0
			
		fi
	esac

!	ipadstr(opcname,(opcode=m_dq|4|10)," ")
	ipadstr(opcname,10," ")

	ipadstr(str,4)

	strcat(str,opcname)

	asmstr(str)

	case m.regmode
	when rm_none then			!no registers involved
	when rm_reg then			!r,r,r
!		ASMSTR(" RRR")
		asmregopnds(m)
	else						!r with [r] combos
		asmregmemopnds(m)
	esac

	if m.valtype and not m.inside then		!additional non-reg opnd
		if m.regmode<>rm_none then
			asmstr(", ")
		fi
		if m.lo12 then
			asmstr(":lo12:")
		fi
		asmstr(strvalue(m))
	fi


!	asmreg(m.a, 1)
!	asmreg(m.b)
!	asmreg(m.c)


!IF FSHOWSEQ THEN ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO)) FI
end

proc asmregopnds(mcl m)=
	for i to m.nregs do
		if i>1 then asmstr(", ") fi
		asmreg(m.regs[i], m.sizes[i])
	od

	if m.regext then
		asmstr(" ")
		asmstr(mclnames[m.regext])
		asmstr(" #")
		asmint(m.shift)
	fi

end

proc asmregmemopnds(mcl m)=

	for i to m.nregs do
		if i>1 then asmstr(", ") fi

		if m.regmode=rm_rrm then
			if i=3 then
				asmstr("[")
			fi
		elsif i=2 then
			asmstr("[")
		fi

		asmreg(m.regs[i], m.sizes[i])
	od

	if m.regext then
		asmstr(" ")
		asmstr(mclnames[m.regext])
		asmstr(" #")
		asmint(m.shift)
	fi

	if m.valtype and m.inside then
!CPL "///",=M.LO12, =M.VALTYPE; OS_GETCH()
!	if m.valtype then
		asmstr(", ")
		if m.lo12 then
			asmstr(":lo12:")
		fi
		asmstr(strvalue(m))
	fi

	asmstr("]")
	if m.excl then asmstr("!") fi
end

global func strmclstr(mcl m)ichar=
	gs_init(pdest)
	strmcl(m)
	return pdest.strptr
end

!global func mstropnd(mclopnd a,int sizeprefix=0,opcode=0)ichar=
!	static [512]char str
!	[128]char str2
!	ichar plus,t
!	int offset,tc
!
!"<STROPND>"

!	str[1]:=0
!
!	case a.mode
!	when a_reg then
!		return strreg(a.reg, a.size)
!
!	when a_imm then
!		if opcode=m_dq and a.valtype=intimm_val then
!			if a.value in 0..9 then
!				strcat(str,strint(a.value))
!			else
!				strcat(str,"0x")
!				strcat(str,strword(a.value,"H"))
!			fi
!		else
!			strcpy(str,strvalue(a))
!		fi
!
!	when a_mem then
!		case a.valtype
!		when intimm_val then
!			strcpy(str,strint(a.value))
!		when realimm_val then
!			strcpy(str,strreal(a.xvalue))
!		when realmem_val then
!			fprint @str,"M#",a.xvalue
!		esac
!
!		strcat(str,getsizeprefix(a.size,sizeprefix))
!		strcat(str,"[")
!
!		plus:=""
!		if a.reg then
!			strcat(str,strreg(a.reg,8))
!			plus:=" + "
!		fi
!		if a.regix then
!			strcat(str,plus)
!			strcat(str,strreg(a.regix,8))
!			plus:=" + "
!
!			if a.scale>1 then
!				strcat(str,"*")
!				strcat(str,strint(a.scale))
!			fi
!		fi
!
!		if a.valtype in [def_val,label_val, temp_val] then
!			if plus^ then
!				strcat(str,plus)
!			fi
!			strcat(str,strvalue(a))
!	    elsif offset:=a.offset then
!			print @str2,offset:" + "
!			strcat(str,str2)
!		fi
!		strcat(str,"]")
!
!	when a_xreg then
!		return strxreg(a.reg,a.size)
!
!	else
!		println "BAD OPND",A.MODE
!		return "<BAD OPND>"
!	esac
!
!	return str
!end

global func strvalue(mcl m)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	i64 value,offset,length
	ichar ss
	static ichar longstring

!RETURN "<STRVAL>"
!RETURN VALTYPENAMES[M.VALTYPE]


	def:=m.def
	value:=m.value

	strcpy(str,"")

	case m.valtype
	when def_val then
		strcat(str,getdispname(def))

	addoffset:
		if offset:=m.offset then
			print @str2,(offset>0|"+"|""),,offset
			strcat(str,str2)
		fi

	when intimm_val then
		strcat(str,strint(value))
!STRCAT(STR, "0x")
!		strcat(str,strint(value,"H"))

	when realimm_val then
		print @str,m.xvalue:"20.20"

	when realmem_val then
		strcat(str,"M")
		strcat(str,strreal(m.xvalue))

	when stringimm_val then
!STRCAT(STR, "SIM")
!		strcat(str,"""")
!		strcat(str,m.svalue)
!		strcat(str,"""")

		if (length:=strlen(m.svalue))<str.len/2 then
			strcpy(str,"""")
			convertstring(m.svalue,&.str+1)
			strcat(str,"""")

		else

			if longstring then
				pcm_free(longstring,longstringlen)
			fi
			longstringlen:=length*2
			longstring:=pcm_alloc(longstringlen)
			longstring^:='"'
			length:=convertstring(m.svalue, longstring+1)
			(longstring+length+1)^:='"'
			(longstring+length+2)^:=0
			return longstring
		fi


	when name_val then
		strcat(str,m.svalue)

	when label_val then
		strcat(str,"L")
		strcat(str,strint(m.labelno))
		goto addoffset

!	when temp_val then
!		return gettempname(currasmproc,m.tempno)

	else
		merror("Stropnd?")
	esac

	return str
end

!global proc asmopnd(mclopnd a,int sizeprefix=0,opcode=0)=
!	asmstr(mstropnd(a,sizeprefix,opcode))
!end

proc asmstr(ichar s)=
	gs_str(pdest,s)
end

proc asmint(int a)=
	asmstr(strint(a))
end

proc asmchar(int c)=
	gs_char(pdest,c)
end

global func getdispname(psymbol d)ichar=
	static [256]char str

	if d.reg then

		IF FEXTENDEDNAMES THEN
			fprint @str,"#R.#.#", (pfloat[d.mode]|"X"|""), $PMODULENAME,(fpshortnames|d.name|getfullname(d))
		else
			fprint @str,"#R.#", (pfloat[d.mode]|"X"|""), (fpshortnames|d.name|getfullname(d))
		fi

		return str
	fi

	if fpshortnames then
		return d.name
	else
		return getfullname(d)
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

!func strreg(int reg, size)ichar=
!	[8]char str
!	ichar prefix
!
!	case reg
!	when rstack then return "SP"
!	when rlink then return "LR"
!	elsif reg in r0..r30 then
!		fprint @str, "##", (size<=4 |"W"|"X"), reg-r0
!	elsif reg in v0..v31 then
!		case size
!		when 8 then prefix:="D"
!		when 4 then prefix:="S"
!		when 2 then prefix:="H"
!		else        prefix:="B"
!		esac
!		print @str, prefix,,reg-v0
!	else
!		return "r?"
!	esac
!
!	str
!end

global func strreg2(int reg, size=8)ichar=
	static [16]char str
	strcpy(str, strreg(reg, size))
	str
end

global func strreg(int reg, size=8)ichar=
	[8]char str
	ichar prefix
	psymbol d

	d:=regvars[reg]
!	D:=NIL

	if d and d.size=size then
		return getdispname(d)
	fi

	case reg
	when rstack then return "sp"
	when rlink then return "lr"
	when rframe then return "fp"
	elsif reg in r0..r30 then
		fprint @str, "##", (size<=4 |"w"|"x"), reg-r0
	elsif reg in v0..v31 then
		case size
		when 8 then prefix:="d"
		when 4 then prefix:="s"
		when 2 then prefix:="h"
		else        prefix:="b"
		esac
		print @str, prefix,,reg-v0
	else
		return "r?"
	esac

	str
end

export func getassemstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d,e
	mcl m
	[32]char str2,str3
	int i

CPL "GETASSEMSTR------------"

	gs_init(pdest)

	d:=psymboltable

	while d, d:=d.next do
!		if d.imported then
!			asmstr("    extern ")
!			asmstr(d.name)
!			asmstr("\n")
!		fi
		if d.exported then
			asmstr("    global ")
			asmstr(getbasename(d.name))
			asmstr("\n")
		fi
	od
	asmstr("\n")

	m:=mccode
	i:=1
	while m do
!CPL "ASMSTR LOOP:", M, MCLNAMES[M.OPCODE]
		writemcl(i,m)
		++i
		m:=m.nextmcl
	od

!CPL "DONE ASM", PDEST.LENGTH

	return pdest
end

proc asmreg(int reg, size)=
	asmstr(strreg(reg, size))
end
=== mc_writeexe_dummy.m 0 0 17/24 ===
global proc genexe(ichar entrypoint, outfile, int dodll)=
abortprogram("No genexe")
end

global proc writeexe(ichar outfile, int dodll)=
abortprogram("No genexe")
end
=== mc_writeobj_dummy.m 0 0 18/24 ===
global proc writecoff(ichar outfile)=
end

=== mc_writess_dummy.m 0 0 19/24 ===
export function writessdata(int fexe)ref strbuffer=
	nil
end
=== mx_run_dummy.m 0 0 20/24 ===
global proc runlibfile(ichar filename, int cmdskip)=
	abortprogram("No Run")
end

global proc writemcx(ichar filename)=
end
=== pp_cli.m 0 0 21/24 ===
global enumdata []ichar passnames =
!								Output (when this is the final step)
	(pcl_pass,		"pcl"),			! .pcl
	(runpcl_pass,	"(int)"),			! interpret
	(mcl_pass,		"asm"),			! .asm
	(obj_pass,		"obj"),			! .obj (via .asm and aa)
	(dll_pass,		"dll"),			! .dll
	(exe_pass,		"exe"),			! .exe
	(mx_pass,		"mx"),			! .mx
!	(clang_pass,	"c"),			! .c
	(run_pass,		"(run)"),		! run in-memory
end

enumdata []ichar optionnames, []byte optionvalues =

!normal production outputs
	(pcl_sw,		"p",			pcl_pass),
	(runpcl_sw,		"i",			runpcl_pass),
	(asm_sw,		"a",			mcl_pass),
	(nasm_sw,		"nasm",			mcl_pass),
	(obj_sw,		"obj",			obj_pass),
	(dll_sw,		"dll",			dll_pass),
	(exe_sw,		"exe",			exe_pass),		!default
	(mx_sw,			"mx",			mx_pass),
!	(clang_sw,		"clang",		clang_pass),
	(run_sw,		"r",			run_pass),		!default with ms.exe

	(noopt_sw,		"no",			0),
	(opt_sw,		"opt",			0),
	(peephole_sw,	"peep",			0),
	(regoptim_sw,	"regs",			0),

!diagnostic outputs, only relevant for debug mode
	(showpcl_sw,	"showpcl",		0),
	(showasm_sw,	"showasm",		0),
	(showc_sw,		"showc",		0),
	(showss_sw,		"showss",		0),
	(shortnames_sw,	"shortnames",	0),

	(pst_sw,		"pst",			0),

	(rip_sw,		"rip",			1),
	(himem_sw,		"himem",		2),
end

int passlevel = exe_pass

byte fshowpcl
byte fshowasm
byte fshowpst
byte fshowss
byte fshowc

int cmdskip
byte highmem

const maxlabel=1 million

ichar inputfile
ichar outputfile
ichar outext

psymbol dentry=nil
!int labelno

[pclnames.bounds]byte isdirective

const maxlocals=1000
[maxlocals]psymbol localvars			!a proc's params and locals
int nlocals

proc main=
	ichar source

!CPL "HI THERE"
!CPL =PSTREC.BYTES
!CPL =REGREC.BYTES
!CPL "---"
!CPL =MCLREC2.A
!CPL =MCLREC2.B
!CPL =MCLREC2.C
!CPL =MCLREC2.RMODE
!CPL =MCLREC2.VALUE
!CPL =MCLREC2.offset
!CPL =MCLREC2.seqno
!CPL =MCLREC2.mpos
!CPL =MCLREC2.lo12

!	byte spare1
!	u32 seqno
!	u32 mpos
!	byte lo12
!	u32 spare2
!


	getinputoptions()

CPL =FREGOPTIM
CPL =FPEEPHOLE
!FREGOPTIM:=0
FPEEPHOLE:=0

	println "Processing", inputfile, "to", outputfile

!CPL "PP", $LINENO
	source:=loadsourcefile(inputfile)
!CPL "PP", $LINENO

	parsefile(source)				!parse to internal PCL
!CPL "PP", $LINENO

	if checkundefined() then
		println "Errors seen"
	fi

!CPL =FREGOPTIM
!CPL =FPEEPHOLE

	pcl_reducetest() when fregoptim or fpeephole


!CPL "PP", $LINENO
	pcl_cmdskip(cmdskip)

!CPL "PP", $LINENO
	case passlevel
	when pcl_pass		then pcl_writepcl(outputfile)
	when runpcl_pass	then pcl_runpcl()
    when mcl_pass       then pcl_writeasm(outputfile)
	when obj_pass		then pcl_writeobj(outputfile)
	when dll_pass		then pcl_writedll(outputfile)
	when exe_pass		then pcl_writeexe(outputfile)
	when mx_pass		then pcl_writemx(outputfile)
!	when clang_pass		then pcl_writeclang(outputfile)
	when run_pass		then pcl_exec()
	else
		loaderror("Bad pass")
	esac

	SHOWPCL(outputfile)

end

proc showpcl(ichar outputfile)=
	[256]char str
	ichar filename, ss
	filehandle f

	return when fshowpcl+fshowpst+fshowasm+fshowc+fshowss=0

	f:=fopen("pp.log","wb")

	if fshowc then
		addtolog(outputfile, f)
	fi

	if fshowasm then
		if passlevel<>mcl_pass then
			pcl_writeasm(outputfile)
		fi
		addtolog(outputfile, f)
	fi
!
	if fshowss then
		ss:=pcl_writess()
		println @f, ss
	fi
!
	if fshowpcl then
		pcl_writepcl(filename:=changeext(outputfile,".pct"))
		addtolog(filename, f)
	fi

	if fshowpst then
		pcl_writepst("psymtab")
		addtolog("psymtab", f)
	fi

	fclose(f)

	print "Press key..."
	stop when os_getch()=27
!
!	fprint @str,"copy/b # + psymtab pp.log", outputfile
!!	cpl =str
!!	os_execwait(str)
!	system(str)
!
	print @str,"\\m\\scripts\\med.bat pp.log"
	os_execwait(str,0,nil)

end

proc getinputoptions=
	int paramno,pmtype,sw,extlen
	ichar name,value,ext
	[300]char filespec

	paramno:=1

	while pmtype:=nextcmdparamnew(paramno,name,value,"pcl") do
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
				exit
			fi

		else
			loaderror("Invalid params")
		esac

	od

	if passlevel in [obj_pass, dll_pass] or
		passlevel=mcl_pass and assemtype='NASM' then highmem:=2
	fi

	if inputfile=nil then
		println "Usage:"
		println "    ",,cmdparams[0],"filename[.pcl]"
		println "Options:      Output:"
		println "    -exe      EXE file (default)"
		println "    -dll      DLL file"
		println "    -obj      OBJ file"
		println "    -mx       MX file"
		println "    -a        ASM file"
		println "    -i        RUN as PCL code (interpret)"
		println "    -r        RUN as native code"
		stop
	fi

	case passlevel
	when pcl_pass then outext:="pct"
!	when clang_pass then outext:="c"
	else outext:=passnames[passlevel]
	esac

	outputfile:=pcm_copyheapstring(changeext(inputfile, outext))

	pcl_setflags(highmem:highmem, shortnames:0)

end

proc do_option(int sw, ichar value, int paramno=0)=
	static byte outused, outpathused

	if sw in pcl_sw..run_sw then
		passlevel:=optionvalues[sw]
		outext:=passnames[sw]

!		if sw=asm_sw and assemtype<>'AA' or sw=nasm_sw and assemtype<>'NASM' then
!			loaderror("Wrong WRITEASM")
!		fi

		if sw=runpcl_pass then			!in case occurs at end
			cmdskip:=paramno-1+$CMDSKIP
		fi

		return

	fi	

	case sw
	when showpcl_sw then fshowpcl:=1
	when showasm_sw then fshowasm:=1
	when showc_sw then fshowc:=1
	when pst_sw then fshowpst:=1
	when showss_sw then fshowss:=1
	when noopt_sw then fpeephole:=fregoptim:=0

	when rip_sw, himem_sw then highmem:=optionvalues[sw]
!
	end case

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

func loadsourcefile(ichar filename)ichar s =
	s:=readfile(filename)
	if s=nil then
		println "Can't open", filename
		stop 1
	fi
	s
end	

proc loaderror(ichar mess)=
	println "PP error:", mess
	println
	stop 1
end

=== pp_decls.m 0 0 22/24 ===
!decls


global record lexrec =
	union
		int		value			!for intsym
		real	xvalue			!for realsym
		ichar	svalue			!for stringsym
		psymbol	symptr			!for any reserved word/identifier
	end
	byte	symbol				!token: intsym, namesym etc
	byte	ksymbol				!0, or opcode/jumpcc/setcc when a reserved word
	byte	SPARE1
	byte	SPARE3
	u32		lineno			!within .pcl input source file
end

global lexrec lx

global enumdata []ichar symbolnames, []byte symtoopnd =
	(errorsym,		$,		0),					!           Extra info returned in:
	(namesym,		$,		mem_opnd),			! abc		(.svalue, .symptr)
!	(localsym,		$,		mem_opnd),			! abc		(.svalue, .symptr) (temp local symbols)
	(labelsym,		$,		label_opnd),		! #123		(.value)
	(loclabelsym,	$,		label_opnd),		! %123		(.value)
	(intsym,		$,		int_opnd),			! 123		(.value)
	(realsym,		$,		real_opnd),			! 1.23		(.xvalue)
	(stringsym,		$,		string_opnd),		! "abc"		(.svalue)
	(colonsym,		$,		0),			! :
	(addrsym,		$,		memaddr_opnd),		! &
	(divsym,		$,		0),					! /

!	(opcodesym,		$,		0),					! load etc	(.value)
!	(jumpccsym,		$,		0),					! jumpeq etc	(.value)
!	(setccsym,		$,		0),					! seteq etc	(.value)
!	(typesym,		$,		0),					! i64 etc	(.value)
	(eolsym,		$,		0),					! blank line or comment line
	(eofsym,		$,		0),					! end of file

end

!these codes appear in 
global enumdata []ichar rwnames =
	(not_rw,		$),					! load etc	(.value)
	(opcode_rw,		$),					! load etc	(.value)
	(jumpcc_rw,		$),					! jumpeq etc	(.value)
	(setcc_rw,		$),					! seteq etc	(.value)
	(type_rw,		$),					! i64 etc	(.value)
	(inf_rw,		$),					! infinity
end


=== pp_lex.m 0 0 23/24 ===
!lex

ref char lxstart, lxsptr

macro hashc(hsum,c) = hsum<<4-hsum+c
macro hashw(hsum)   = hsum<<5-hsum

global const hstsize	= 65536
global const hstmask	= hstsize-1

global [0:hstsize]psymbol	hashtable
global psymbol stvoid

[0..255]char alphamap
const cr=13, lf=10

global psymbol currproc=cast(1234)

global proc lex=
!read next token into lx
	[256]char str
	int c,hsum,hashindex,length, hasdot

	IF LX.SYMBOL=EOLSYM THEN ++LX.LINENO FI

	lx.value:=0

	doswitch lxstart:=lxsptr; c:=lxsptr++^
	when 'A'..'Z','a'..'z','_','$','.' then
		hsum:=c
		hasdot:=0
		str[1]:=c
		length:=1

		while alphamap[c:=lxsptr++^] do
			str[++length]:=c
			hsum:=hashc(hsum,c)
			hasdot ior:= int(c='.')
		od
		--lxsptr

		str[++length]:=0
		lookup(str, hashw(hsum))
		lx.symbol:=namesym
		lx.symptr.hasdot:=hasdot

		return

	when '0'..'9' then
		if c='0' and lxsptr^ in ['x', 'X'] then
			++lxsptr
			readhex()
		else
			--lxsptr
			readdec(0)
		fi
		return

	when '-' then
		if lxsptr^ not in '0'..'9' then lxerror("-123") fi
		readdec(1)
		return

	when '!', ';' then			!comment to eol or blank line
		while (c:=lxsptr++^) not in [lf, 0] do od
		if c=0 then --lxsptr fi

		lx.symbol:=eolsym
		return

	when '#','%' then					!label
		if lxsptr^ not in '0'..'9' then lxerror("Label?") fi
		readdec(0)
		lx.symbol:=(c='#'|labelsym|loclabelsym)
		return

	when ' ', '\t', cr then				!white space

	when ':' then
		lx.symbol:=colonsym
		return

	when '/' then
		lx.symbol:=divsym
		return

	when '&' then
		lx.symbol:=addrsym
		return

	when '"' then
		readstring()
		return

	when lf then
		lx.symbol:=eolsym
		return

	when 0 then		!eof
		--lxsptr
		lx.symbol:=eofsym
		return

	else
CPL =C
		lxerror("Bad token")

	end doswitch

end

global proc startlex(ichar source)=
	lxstart:=lxsptr:=source
	lx.lineno:=1

end

proc inithashtable=
	[16]char str

	for i in pclnames.bounds do
		addreservedword(pclnames[i], opcode_rw, i)
	od
	for i in pstdnames.bounds do
		addreservedword(pstdnames[i], type_rw, i)
	od

	for i in eq_cc .. gt_cc do
		strcpy(str, "jump")
		strcat(str, ccnames[i])
		addreservedword(str, jumpcc_rw, i)
		strcpy(str, "set")
		strcat(str, ccnames[i])
		addreservedword(str, setcc_rw, i)
	od

	addreservedword("infinity", inf_rw, 0)

end

global proc printhashtable=
	psymbol d

	for i:=0 to hstmask do
		d:=hashtable[i]
		if d then
			println i, d.name, d.opcode, d.mode, rwnames[d.ksymbol]
		fi
	od
end

global proc addreservedword(ichar name, int ksymbol, subcode) =

	if lookup(name, gethashvaluez(name)) then
		lxerror(addstr("PCI:Dupl name:",name))
	fi

	lx.symptr.ksymbol:=ksymbol				!opcode/jumpcc/setccsym
	if ksymbol<>type_rw then
		lx.symptr.opcode:=subcode			!pcl code or cc code
	else
		lx.symptr.mode:=subcode				!typesym
	fi
end

func lookup(ref char name, int hashindex0)int=
!lookup rawnamesym with details in lx
!name points to zero-terminated string in temp buffer
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped, hashindex,INDEX,n
	psymbol d
	int j

	j:=hashindex0 iand hstmask

	d:=hashtable[j]
	wrapped:=0

	do
		if d=nil then exit fi

		if strcmp(d.name, name)=0 then	!match
			lx.symptr:=d
			lx.ksymbol:=d.ksymbol
			return 1
		fi

		if ++j>=hstsize then
			if wrapped then
				lxerror("Hashtab full")
			fi
			wrapped:=1
			j:=0
		fi
		d:=hashtable[j]
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	d:=pc_makesymbol(name, null_id)
	pc_addsymbol(d)
	hashtable[j]:=d

	lx.symptr:=d
	lx.ksymbol:=0

	return 0
end

proc readdec(int neg)=
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
			a:=a*10+c-'0'
			dest++^:=c
		elsif c in ['e','E'] then
			lxsptr:=pstart
			readreal(neg)
			return
		elsif c='.' then
			lxsptr:=pstart
			readreal(neg)
			return

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>20 or length=20 and strncmp(&.str, "18446744073709551615", 20) then
		lxerror("u64 overflow")
	fi

	lx.symbol:=intsym
	lx.value:=(neg|-a|a)
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

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&.str

	if length>16 then
		lxerror("u64 overflow")
	fi

	lx.symbol:=intsym
	lx.value:=a
end

proc readreal(int neg)=
!at '.', or had been in middle of int where . or e were seen, back at the start

	int c,n,negexpon,dotseen,length, fractlen, expon, expseen
	real x
	[1024]char str
	ichar dest, destend, pexpon

	dest:=&.str
	destend:=dest+str.len-100
	length:=negexpon:=dotseen:=expseen:=expon:=fractlen:=0

	do
		if (c:=lxsptr++^) in '0'..'9' then
			dest++^:=c
			++length
			if dotseen then ++fractlen fi
		elsif c= '.' then
			if dotseen then --lxsptr; exit fi
			dotseen:=1
			dest++^:=c


		elsif c in ['e','E'] then
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

				else
					--lxsptr
					exit all
				fi
			od

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("r64lit too long") fi
	od
	dest^:=0

	lx.xvalue:=strtod(str,nil)
	if neg then lx.xvalue:=-lx.xvalue fi
	lx.symbol:=realsym
end

function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
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

proc start=

	for i:=0 to 255 do
		if i in 'A'..'Z' or i in 'a'..'z' or
			i in '0'..'9' or i in ['$', '_', '.'] then
			alphamap[i]:=1
		fi
	od

	inithashtable()
!	printhashtable()

end

proc lxerror(ichar mess)=
	println "LEX error:",mess,"on line:",lx.lineno
	println
	stop 1
end

proc readstring=
	ichar s,t
	int c, d, length, hasescape
	[8]char str

	lx.symbol:=stringsym
	s:=lxsptr

!do a first pass that terminates length of final string
	length:=0
	hasescape:=0

	doswitch c:=lxsptr++^
	when '\\' then			!escape char
		c:=tolower(lxsptr^)
		++lxsptr
		hasescape:=1

		switch c
		when 'a','b','c','e','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
			++length
		when 'w' then
			++length
		when 'x' then	!2-digit hex code follows
			lxsptr+:=2
			++length
		else
			lxerror("Bad str escape")
		end switch
	when '"' then
		if c='"' then		!terminator char
			exit
		else
			++length
		fi
	when cr,lf,0 then
		lxerror("String not terminated")
	else
		++length
	end doswitch

	if length=0 then
		lx.svalue:=""
		return
	elsif not hasescape then
		lx.svalue:=pcm_copyheapstringn(s,length)
		return
	fi

!need to copy string to dest and expand the escape codes

	lx.svalue:=t:=pcm_alloc(length+1)

	do
		switch c:=s++^
		when '\\' then			!escape char
			switch c:=tolower(s++^)
			when 'c','r' then		!carriage return
				c:=cr
			when 'l','n' then		!linefeed, or linux/c-style newline
				c:=lf
			when 't' then			!tab
				c:=9
			when 'x' then	!2-digit hex code follows
				c:=0
				to 2 do
					case d:=s++^
!					switch d:=s++^
					when 'A','B','C','D','E','F' then
						c:=c*16+d-'A'+10
					when 'a','b','c','d','e','f' then
						c:=c*16+d-'a'+10
					when '0','1','2','3','4','5','6','7','8','9' then
						c:=c*16+d-'0'
					else
						lxerror("Bad \\x code")
					end
				od
			when '"' then		!embedded double quote
				c:='"'
			when '\\' then
				c:='\\'
			when '\'' then			!embedded single quote
				c:='\''
			else
				str[1]:=c; str[2]:=0
				lxerror(addstr("Unknown string escape: ",str))
			end
		when '"' then		!possible terminators
			if c='"' then		!terminator char
				if s^=c then		!repeated, assume embedded term char
					++s
				else			!was end of string
					exit
				fi
			fi
		when cr,lf,0 then
			lxerror("String not terminated")
		end switch

		t++^:=c
	od

	t^:=0
end

global proc printlx=
	psymbol d

	print lx.lineno,,":",symbolnames[lx.symbol],$

	case lx.symbol
	when namesym then
		d:=lx.symptr
		print d.name,$

		case lx.ksymbol
		when opcode_rw then
			print "(Opcode)"

		when jumpcc_rw, setcc_rw then
			print "(Jump/setcc:",ccnames[d.opcode],,")"

		when type_rw then
			print "(Type:",strpmode(d.mode),,")"
		else
			print "(Identifier)"
		esac

	when intsym then print lx.value
	when realsym then print lx.xvalue
	when stringsym then fprint """#""",lx.svalue
	when labelsym then print "#",,lx.value
	when loclabelsym then print "%",,lx.value
	esac
	println
end


=== pp_parse.m 0 0 24/24 ===
const maxlabel=1 million

psymbol dentry=nil
!int labelno

[pclnames.bounds]byte isdirective

const maxlocals=1000
[maxlocals]psymbol localvars			!a proc's params and locals
int nlocals


global proc parsefile(ichar source)=
	int condcode

	startlex(source)
	pcl_start("demoxxx")


	do
		lex()

		case lx.symbol
		when eolsym then
		when namesym then
			case lx.ksymbol
			when opcode_rw then

				parseinstr(lx.symptr.opcode)

			when jumpcc_rw then
				parseinstr(kjumpcc, lx.symptr.opcode)
			when setcc_rw then
				parseinstr(ksetcc, lx.symptr.opcode)
			else
				serror("Instr expected")
			esac
		when labelsym then
			if lx.value not in 1..maxlabel then serror("Bad label no") fi
			mlabelno max:=lx.value
			pc_gen(klabel, genlabel(lx.value))
			lex()
			checkcolon(1)

		when eofsym then
			exit
		else
			serror("Opcode expected")
		esac

		if lx.symbol<>eolsym then serror("Extraneous tokens on line") fi

	od

	pcl_end()

end

proc parseinstr(int opcode, condcode=0)=
!instr format:
! opcode type1 [/type2] [/attr1 [attr2]] [[&]operand] [:[::]]
!current token is the opcode

	int x, y, size, size2
	byte mode, mode2, ntypes, nattrs, opndtype, needopnd, n
	pcl p
	psymbol d, e
	ref byte q, r

	lex()

	ntypes:=pclhastype[opcode]
	nattrs:=pclextra[opcode]
	needopnd:=pclhasopnd[opcode]
	mode:=mode2:=tpvoid
	x:=y:=size:=opndtype:=0
	p:=nil

	if ntypes then
		if lx.symbol=divsym then skipmode fi		!leave as void
		mode:=readmode(size)
		if ntypes=2 then
			checksymbol(divsym)
			lex()
			mode2:=readmode(size2)
		fi
	fi
skipmode:
!CPL PCLNAMES[OPCODE],NATTRS

	if nattrs then
		if lx.symbol<>divsym and opcode=kjumpcc then skip fi		!/popone=0 is assumed

		if lx.symbol=namesym and lx.ksymbol=type_rw then
			mode:=readmode(size)
		fi

		if lx.symbol<>divsym then skip fi			!assume /0

		checksymbol(divsym)
		lex()
		checksymbol(intsym)
		x:=lx.value
		y:=0
		lex()

		if lx.symbol=divsym then
			if nattrs<2 then serror("Extra attribute") fi
			lex()
			checksymbol(intsym)
			y:=lx.value
			lex()
		fi
skip:
	fi

	if isdirective[opcode] then
		dodirective(opcode, mode, size, x, y)
		return
	fi

	if needopnd then				!needs operand
!CPL "OPND", SYMBOLNAMES[LX.SYMBOL]

		opndtype:=symtoopnd[lx.symbol]
!CPL "OPND2", OPNDNAMES[OPNDTYPE]

		case opndtype
		when 0 then
			serror("Missing or bad operand")
		when mem_opnd, label_opnd, string_opnd, memaddr_opnd then
			if needopnd not in [opndtype, any_opnd] then
				serror("Incorrect operand type")
			fi
		esac						!else any allowed, or a mixture


		case lx.symbol
		when intsym then
			if mode=tpblock then
				if opcode<>kdata then serror("block data?") fi
				q:=r:=pcm_alloc(size)
				for i to size do
					checksymbol(intsym)
					r++^:=lx.value
					lex() when i<size
				od
				p:=gendata(q, size)
			else
				p:=genint(lx.value)
			fi

		when realsym then
			p:=genreal(lx.xvalue, mode)
		when stringsym then
			p:=genstring(lx.svalue)
		when namesym then
			d:=lookuplocal(lx.symptr)
			if d.ksymbol=inf_rw then
				p:=genreal(infinity, mode)
			else
				p:=genmem(d)
			fi
		when addrsym then
			lex()
			checksymbol(namesym)
	
			d:=lookuplocal(lx.symptr)
			p:=genmemaddr(d)

		when labelsym then
			mlabelno max:=lx.value
			p:=genlabel(lx.value)
		else
			serror("Bad opnd type")
		esac

		lex()

	fi

	pc_gen(opcode, p)
	pccurr.condcode:=condcode

	pc_setmode(mode, size)
	if ntypes=2 then
		pc_setmode2(mode2)
	fi

	pc_setxy(x, y)
	pccurr.sourceoffset:=lx.lineno
end

proc start=
	static[]byte directives=(
		kproc, kendproc, kistatic, kzstatic, klocal, kparam, krettype,
		kaddlib, kextproc, ktcproc, kvariadic)

	for x in directives do
		isdirective[x]:=1
	od
end

proc serror(ichar mess)=
	println "Syntax error:", mess, "on line", lx.lineno
	println
	stop 1
end

proc skiptoeol=
	while lx.symbol<>eolsym do
		lex()
	od
end

proc checksymbol(int expected)=
	[256]char str

	if lx.symbol<>expected then
		fprint @str, "# expected, not #", symbolnames[expected], symbolnames[lx.symbol]
		serror(str)
	fi
end

func readmode(int &size)int m=
!positioned at mode token
	psymbol d:=lx.symptr
	if lx.symbol<>namesym or d.ksymbol<>type_rw then serror("Type expected") fi

	m:=d.mode
	size:=psize[m]
	lex()

	if m=tpblock then
		checkcolon(1)
		checksymbol(intsym)
		size:=lx.value
		lex()
	fi

	return m
end

func checkcolon(int n)int m=
!should at a colon symbol; check that, plus any more up to n colons in
!all. Return the number seen
!1 colon: label or local symbol defined (or can separate block type from its size)
!2 colons: symbol exported
!3 colons: symbol is the entry point
!exit with following token current

	m:=0
	while lx.symbol=colonsym do
		++m
		lex()
	od

	if m=0 then serror("Colon expected") fi
	if m>n then serror("Too many colons") fi

	return m
end

func lookuplocal(psymbol d)psymbol=

	if currfunc=nil then return d fi

!Look up any as yet undefined symbol in local list
	for i to nlocals do
		if localvars[i].generic=d then
			return localvars[i]
		fi
	od
	d
end

global func checkundefined:int=
	ref[]byte labelmap
	pcl pc
	int labno
	byte errors:=0

	labelmap:=pcm_allocz(mlabelno)

	pc:=pcstart
	while pc<=pccurr, ++pc do

		if pc.opcode=klabel then
			labno:=pc.labelno
			if labelmap[labno] then
				println "Duplicate label: #:",labno
				errors:=0
			fi
			labelmap[labno]:=1
		fi
	od

	pc:=pcstart
	while pc<=pccurr, ++pc do

		if pc.opcode<>klabel then
			case pc.opndtype
			when mem_opnd, memaddr_opnd then
				if pc.def.id=null_id then
					fprintln "Undefined name: # on line #", pc.def.name, pc.sourceoffset
					errors:=1
				fi
				pc.def.used:=1
			when label_opnd then
				labno:=pc.labelno
				if labelmap[pc.labelno]=0 then
					fprintln "Undefined label: ## on line #", "#", pc.labelno, pc.sourceoffset
					errors:=1
				fi
			esac
		fi
	od

	pcm_free(labelmap, mlabelno)
	errors
end

proc dodirective(int opcode, mode, size, x, y)=
!a directive opcode has been seen. Line has been processed beyond
!mode and attributes, and current token is what follows (usually a name)
!deal with these special ops here
	psymbol d, e
	int n

!do ones that don't define a name first
	case opcode
	when kaddlib then
		checksymbol(stringsym)
		pc_addplib(lx.svalue)
		lex()
		return
	when kendproc then
		pc_endproc()
		return

	when krettype then
		if currfunc=nil then serror("rettype?") fi
		currfunc.mode:=mode
		currfunc.size:=size
		return

	when kvariadic then
		currfunc.variadic:=1
		return
    esac

!the first define a new name

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	if opcode in [kparam, klocal] then
		if currfunc=nil then serror("Not in a proc") fi

!check for dupl local
		for i to nlocals do
			if localvars[i].generic=d then serror("Dupl local") fi
		od

!		e:=currfunc.nextparam
!		while e, e:=e.nextparam do if e.generic=d then serror("P:Dupl local") fi od
!		e:=currfunc.nextlocal
!		while e, e:=e.nextlocal do if e.generic=d then serror("L:Dupl local") fi od
!
		e:=pc_duplpst(d)
			
		if nlocals>=maxlocals then serror("Too many locals") fi
		localvars[++nlocals]:=e

		if opcode=kparam then
			e.id:=param_id
			pc_addparam(e)
		else
			e.id:=local_id
			pc_addlocal(e)
		fi
		e.mode:=mode
		e.size:=size

	else
		if currfunc then serror("Not allowed in proc") fi
		if d.id<>null_id then serror(addstr("Dupl name:",d.name)) fi

		nlocals:=0

		case opcode
		when kproc, ktcproc then
			n:=checkcolon(3)
			d.id:=proc_id
			if n>=2 then d.exported:=1 fi
			if n=3 then
				if dentry then serror("Dupl entry point") fi
				d.isentry:=1
				dentry:=d
			fi
			pc_defproc(d, mode, isentry:dentry<>nil, threaded:opcode=ktcproc)

		when kistatic, kzstatic then
			d.id:=static_id
			if checkcolon(2)=2 then
				d.exported:=1
			fi
			pc_gen(opcode, genmem(d))
			pc_setmode(mode, size)
			d.mode:=mode
			d.size:=size

		when kextproc then
			d.id:=import_id
			d.imported:=1
		else
			serror("Direct?")
		esac
	fi
end

=== END ===
1 pc.m 0 0
2 pcl.m 0 0
3 pc_api.m 0 0
4 pc_decls.m 0 0
5 pc_diags.m 0 0
6 pc_reduce.m 0 0
7 pc_run_dummy.m 0 0
8 pc_tables.m 0 0
9 mc_genmcl.m 0 0
10 mc_auxmcl.m 0 0
11 mc_libmcl.m 0 0
12 mc_stackmcl.m 0 0
13 mc_genss_dummy.m 0 0
14 mc_decls.m 0 0
15 mc_objdecls.m 0 0
16 mc_writeasm.m 0 0
17 mc_writeexe_dummy.m 0 0
18 mc_writeobj_dummy.m 0 0
19 mc_writess_dummy.m 0 0
20 mx_run_dummy.m 0 0
21 pp_cli.m 0 0
22 pp_decls.m 0 0
23 pp_lex.m 0 0
24 pp_parse.m 0 0
