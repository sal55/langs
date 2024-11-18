=== MA 24 ===
=== aa.m 0 0 1/24 ===
!project =

	module aa_cli

	module aa_decls
	module aa_lex
	module aa_lib
!	module aa_objdecls
!	module aa_mcxdecls
	module aa_parse
	module aa_show

	module aa_tables
!	module aa_writeexe
!	module aa_writemcx
!	module aa_writeobj

	$sourcepath "c:/px/"
!	module mc_objdecls

!	import pcl
	import pclax

!	$sourcepath "c:/bx/"
!
!	module mx_decls
!	module mx_run
!	module mx_lib
!	module mx_write



!	module aa_writessdummy
!end
=== pclax.m 0 0 2/24 ===
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

	module pc_diags_dummy

!	module pc_run
!	module pc_runaux
	module pc_run_dummy

	module pc_tables

!	module pc_genc
!	module pc_auxc
!	module pc_libc

	module mc_GenMCL_dummy
!	module mc_AuxMCL
	module mc_LibMCL
!	module mc_StackMCL

	module mc_GenSS

	module mc_Decls as md
	module mc_OBJdecls
	module mc_WriteASM
!	module mc_WriteNASM

	module mc_WriteEXE
	module mc_WriteOBJ

!	module mx_decls
!	module mx_run
	module mx_run_dummy
!	module mx_lib
!	module mx_write
end

export byte pc_userunpcl=0

!proc main=
!end


=== pc_api.m 0 0 3/24 ===
CONST REDUCE=0
!CONST REDUCE=1

INT PCLSEQNO
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
global ichar errormess

export func pcl_start(ichar name=nil, int nunits=0)psymbol=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

INT NN:=0X12345601

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

export proc pcl_reducetest=
	int nn, seqno, lab
	pcl oldpc, newpc
	ref[]u16 labelmap

	nn:=pccurr-pcstart+1

RETURN
!RETURN UNLESS REDUCELABELS

CPL "TEST REDUCING PCL CODE"
!remove unused labels: first create a map of referenced labels

	labelmap:=pcm_allocz(mlabelno*u16.bytes)

	oldpc:=pcstart
	while oldpc<=pccurr, ++oldpc do
		case oldpc.opcode
		when klabel then				!don't include these
		when kassem then
			if icheckasmlabel then
				lab:=icheckasmlabel(oldpc.asmcode)		!non-zero means a label number from ast
				if lab then
					++labelmap[lab]
				fi
			fi
!
		else
			if oldpc.opndtype=label_opnd then
				++labelmap[oldpc.labelno]
			fi
		esac
	od

	oldpc:=pcstart
	newpc:=pcstart-1			!point to last copied instr (none to start)
	seqno:=0

	to nn do
		case oldpc.opcode
		when kcomment then

		when klabel then
			IF NOT REDUCELABELS THEN RECASE ELSE FI
			if labelmap[oldpc.labelno] then recase else fi			!else skipped

		when kproc,ktcproc then
			oldpc.def.pcaddr:=newpc+1
			recase else
		else
			++newpc
			newpc^:=oldpc^
			newpc.seqno:=++seqno
		esac
		++oldpc
	od

CPL =PCCURR-PCSTART

	pccurr:=newpc
!CPL =PCCURR.SEQNO
CPL =PCCURR-PCSTART

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
	phighmem:=2
!CPL "WRITEOBJ", =PHIGHMEM
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
	genmcl()
	genss()
	writemcx(filename)
end

!export proc pcl_writeclang(ichar filename)=
!	writeclang(filename)
!end

export proc pcl_exec=
!PCLERROR("NO RUN")
!	pcmdskip:=cmdskip
	genmcl()
	genss()
	runlibfile("dummy", pcmdskip)
end

export proc pcl_setflags(int highmem=-1, verbose=-1, shortnames=-1) =
!export proc pcl_setflags(int highmem=-1, verbose=-1, shortnames=-1,
!	hostcmdskip=-1)=

!CPL "SETFLAGS", =HOSTCMDSKIP

	if highmem>=0 then phighmem:=highmem fi
	if verbose>=0 then pverbose:=verbose fi
	if shortnames>=0 then fpshortnames:=shortnames fi
!	if hostcmdskip>=0 then pcmdskip:=hostcmdskip fi
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

!	case opcode
!	when kiload then
!		if pccurr.opcode=kaddpx then
!			pccurr.opcode:=kiloadx
!			return
!		fi
!	when kistore then
!		if pccurr.opcode=kaddpx then
!			pccurr.opcode:=kistorex
!			return
!		fi
!
!!	when kiloadx, kistorex, kaddpx then
!!		if pccurr.opcode=kload and (pccurr-1).opcode=kaddpx then
!!			s2:=pccurr.scale
!!			d2:=pccurr.extra
!!			n:=pccurr.value*s2	
!!
!	esac

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

MLABELNO MAX:=A

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
!	int dprec@signed

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

!CPL "MAKESYM", S,IDNAMES[ID]
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
	[16]psymbol chain
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

!	repeat
!		chain[++n]:=e
!		e:=e.owner
!	until e=nil !or e.id=program_id
!
!	if backtick then
!		strcpy(str, "`")
!	else
!		str[1]:=0
!	fi
!	strcat(str,chain[n].name)
!	for i:=n-1 downto 1 do
!		strcat(str,".")
!		strcat(str,chain[i].name)
!	od
!
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

!global func getfullname(psymbol d, int backtick=0)ichar=
!!create fully qualified name into caller's dest buffer
!	static [256]char str
!	[16]psymbol chain
!	int n:=0
!	psymbol e:=d
!
!	if fpshortnames then return d.name fi
!
!	if d.imported then
!		if backtick then
!			strcpy(str, "`")
!			strcat(str, d.name)
!			strcat(str, "*")
!		else
!			strcpy(str, d.name)
!		fi
!		return str
!	fi
!
!	repeat
!		chain[++n]:=e
!		e:=e.owner
!	until e=nil !or e.id=program_id
!
!	if backtick then
!		strcpy(str, "`")
!	else
!		str[1]:=0
!	fi
!	strcat(str,chain[n].name)
!	for i:=n-1 downto 1 do
!		strcat(str,".")
!		strcat(str,chain[i].name)
!	od
!
!	return str
!end

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

!export proc pcl_$cmdskip(psymbol dcmdskip=nil)=
!CPL "PCL/$CMDSKIP", DCMDSKIP
!	pdcmdskip:=dcmdskip
!end
!
=== pc_decls.m 0 0 4/24 ===
!decls

export type psymbol = ref pstrec

!global record pstrec = $caligned
global record pstrec =
	ichar name
	psymbol next
	psymbol nextparam
	psymbol nextlocal
	psymbol owner
	psymbol generic				!locals/params: version in global GT

	union
		pcl pcaddr				!for procs: entry point to function
		ref proc dlladdr		!for imported functions
		ivoid staddr			!var statics: address
	end
	ref fwdrec fwdrefs			!fwd ref chain

	byte id
	byte ksymbol				!0, or _rw code for reserved words (used by pp)
	byte opcode					!for opcode_rw
	byte subcode				!for jumpcc_rw/setcc_rw/type_rw

	int32 offset

	byte imported				!only for import_id
	byte exported				!only for proc_id/static_id
	byte mode
	byte isentry
	u32 size

	byte addrof
	byte nrefs
	byte reg
	byte atvar
	byte used
	byte reftype
	byte segment
	byte hasdot

	int16 stindex
	int16 importindex
	int32 labelno

	byte asmused				!1 when proc contains asmcode
	byte dllindex				!for dllproc: which dll in dlltable

	byte nretvalues				!function: number of return values (0 for proc)
	byte varparams				!0 or N; variadic params

	byte isthreaded				!
	byte ishandler				!1 if a proc to be part of func tables
	byte ismain					!1 if a proc to be part of func tables
	byte scope

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

!global record fwdrec =
export record fwdrec =
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

global byte pcldone, mcldone, ssdone

global byte pverbose
global int pcmdskip
global psymbol pdcmdskip			!reference to $cmdskip symbol

export int assemtype='AA'

GLOBAL INT PPSEQNO

EXPORT BYTE REDUCELABELS
=== pc_diags_dummy.m 0 0 5/24 ===
global proc pshowlogfile=
end

global proc strpcl(pcl p)=
end

global func stropnd(pcl p)ichar=
	return nil
end

global func strpclstr(pcl p)ichar=
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

=== pc_run_dummy.m 0 0 6/24 ===
export proc pcl_runpcl=
end
=== pc_tables.m 0 0 7/24 ===
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

	(tpblock,     "mem",   	0,	0,0,	tpblock,	tpvoid),
	(tpvector,    "vec",   	0,	0,0,	tpvector,	tpvoid),

	(tplast,      "$last",   	0,	0,0,	0,			0),


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
	(real32_opnd,		$),
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
				[0:]byte pclhasopnd  =

!                       t  x op      (a  b)
	(knop=0,       $+1, 0, 0, 0),  ! (0 - 0) (          ) ?

	(kload,        $+1, 1, 0, A),  ! (0 - 1) (M L t     ) Z' := M &M L &L 123 4.5 "abc"
	(kiload,       $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := Z^
	(kiloadx,      $+1, 1, 2, 0),  ! (2 - 1) (t d       ) Z' := (Y + Z*s + d)^

	(kstore,       $+1, 1, 0, M),  ! (1 - 0) (M t       ) M := Z
	(kistore,      $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ := Y
	(kistorex,     $+1, 1, 2, 0),  ! (3 - 0) (t s d     ) (Y + Z*s + d)^ := X

	(kdupl,        $+1, 0, 0, 0),  ! (1 - 2) (          ) Z' := Y' := Z
	(kdouble,      $+1, 0, 0, 0),  ! (1 - 2) (          ) Count extra instance of Z
	(kswapstk,     $+1, 0, 2, 0),  ! (2 - 2) (a b       ) Swap(stack(a), stack(b)); 1/2/3/4 = Z/Y/X/W
	(kunload,      $+1, 1, 0, 0),  ! (1 - 0) (t         ) Pop stack

	(kopnd,        $+1, 1, 0, A),  ! (0 - 0) (M L C t   ) Define auxiliary operand M or L
	(ktype,        $+1, 1, 0, 0),  ! (0 - 0) (t         ) Define auxiliary type t

	(kloadbit,     $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y.[Z]
	(kloadbf,      $+1, 1, 0, 0),  ! (3 - 1) (t         ) Z' := X.[Y..Z]
	(kstorebit,    $+1, 1, 0, 0),  ! (3 - 0) (t         ) Y^.[Z] := X
	(kstorebf,     $+1, 1, 0, 0),  ! (4 - 0) (t         ) X^.[Y..Z] := W

	(kcallp,       $+1, 0, 2,MA),  ! (n - 0) (M n v     ) Call &M with nargs, then pop args; v = varargs
	(kicallp,      $+1, 0, 2, 0),  ! (n - 0) (n v       ) Call Z with nargs, then pop args (a=n+1)
	(kretproc,     $+1, 0, 0, 0),  ! (0 - 0) (          ) Return from proc
	(kcallf,       $+1, 1, 2,MA),  ! (n - 1) (M t n v   ) Call &M, then pop args, leave retval; v = varrgs
	(kicallf,      $+1, 1, 2, 0),  ! (n - 1) (t n v     ) Call Z, then pops args, leave retval (a=n+1)
	(kretfn,       $+1, 1, 0, 0),  ! (0 - 0) (t         ) Return from func with Z=retval

	(kjump,        $+1, 0, 0, L),  ! (0 - 0) (L         ) goto L
	(kijump,       $+1, 0, 0, 0),  ! (1 - 0) (          ) goto Z
	(kjumpcc,      $+1, 1, 1, L),  ! (2 - n) (L t c p   ) goto L when Y c Z; p=1: Z':=Y (b=0/1)
	(kjumpt,       $+1, 1, 0, L),  ! (1 - 0) (L t       ) goto L when Z is true
	(kjumpf,       $+1, 1, 0, L),  ! (1 - 0) (L t       ) goto L when Z is false
	(kjumpret,     $+1, 1, 0, L),  ! (1 - 0) (L t       ) goto L, common return point; deal with any ret value on stack
	(kjumpretm,    $+1, 1, 0, L),  ! (a - 0) (L t n     ) goto L, common return point; deal with any ret value on stack

	(ksetcc,       $+1, 1, 0, 0),  ! (2 - 1) (t c       ) Z' := Y cc Z

	(kstop,        $+1, 0, 0, 0),  ! (1 - 0) (          ) Stop Z

	(kto,          $+1, 1, 0, L),  ! (0 - 0) (L t       ) --B (aux); goto L when B<>0 
	(kforup,       $+1, 1, 1, L),  ! (0 - 0) (L t n     ) B:=n; goto L when B<=C
	(kfordown,     $+1, 1, 1, L),  ! (0 - 0) (L t n     ) B-:=n; goto L when B>=C

	(kiswap,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) swap(Y^,Z^)

	(kswitch,      $+1, 1, 2, L),  ! (1 - 0) (L t x y   ) L=jumptab; B=elselab; x/y=min/max values
	(kswitchu,     $+1, 0, 2, L),  ! (1 - 0) (L x y     ) L=jumptab; B=elselab; x/y=min/max values
	(kswlabel,     $+1, 0, 0, L),  ! (0 - 0) (L         ) jumptable entry
	(kendsw,       $+1, 0, 0, 0),  ! (0 - 0) (          ) Mark end of switch jumptable

	(kclear,       $+1, 1, 0, 0),  ! (1 - 0) (t         ) Clear Z^

	(kassem,       $+1, 0, 0, A),  ! (0 - 0) (x         ) To be worked out....

	(kadd,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y + Z

	(ksub,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y - Z
	(kmul,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y * Z
	(kdiv,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y / Z
	(kidiv,        $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y % Z
	(kirem,        $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y rem Z
	(kidivrem,     $+1, 1, 0, 0),  ! (2 - 2) (t         ) Z' := divrem(Y, Z)
	(kbitand,      $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y iand Z
	(kbitor,       $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y ior Z
	(kbitxor,      $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y ixor Z
	(kshl,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y << Z
	(kshr,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y >> Z
	(kmin,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := min(Y, Z)
	(kmax,         $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := max(Y, Z)
	(kaddpx,       $+1, 1, 2, 0),  ! (2 - 1) (t s d     ) Z' := Y + Z*s + d
	(ksubpx,       $+1, 1, 2, 0),  ! (2 - 1) (t s d     ) Z' := Y - Z*s + s
	(ksubp,        $+1, 1, 1, 0),  ! (2 - 1) (t s       ) Z' := (Y - Z)/s

	(kneg,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := -Z
	(kabs,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := abs Z
	(kbitnot,      $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := inot Z
	(knot,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := not Z
	(ktoboolt,     $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := istrue Z; u is of type u; result is type t
	(ktoboolf,     $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := not istrue Z
	(ksqr,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := sqr Z

	(ksqrt,        $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := sqrt Z
	(ksin,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := sin Z
	(kcos,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := cos Z
	(ktan,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := tan Z
	(kasin,        $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := asin Z
	(kacos,        $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := acos Z
	(katan,        $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := atan Z
	(klog,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := log Z
	(klog10,       $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := log10 Z
	(kexp,         $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := round Z
	(kround,       $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := round Z
	(kfloor,       $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := floor Z
	(kceil,        $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := ceil Z
	(ksign,        $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := sign Z

	(katan2,       $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := atan2(Y, Z)
	(kpower,       $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := Y ** Z
	(kfmod,        $+1, 1, 0, 0),  ! (2 - 1) (t         ) Z' := fmod(Y, Z)

	(kincrto,      $+1, 1, 1, 0),  ! (1 - 0) (t n       ) Z^ +:= n
	(kdecrto,      $+1, 1, 1, 0),  ! (1 - 0) (t n       ) Z^ -:= n
	(kincrload,    $+1, 1, 1, 0),  ! (1 - 1) (t n       ) Z' := (Z +:= n)^
	(kdecrload,    $+1, 1, 1, 0),  ! (1 - 1) (t n       ) Z' := (Z -:= n)^
	(kloadincr,    $+1, 1, 1, 0),  ! (1 - 1) (t n       ) Z' := Z++^ (difficult to express step)
	(kloaddecr,    $+1, 1, 1, 0),  ! (1 - 1) (t n       ) Z' := Z--^

	(kaddto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ +:= Y
	(ksubto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ -:= Y
	(kmulto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ *:= Y
	(kdivto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ /:= Y
	(kidivto,      $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ %:= Y
	(kiremto,      $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ rem:= Y
	(kbitandto,    $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ iand:= Y
	(kbitorto,     $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ ior:= Y
	(kbitxorto,    $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ ixor:= Y
	(kshlto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ <<:= Y
	(kshrto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ >>:= Y
	(kminto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ min:= Y
	(kmaxto,       $+1, 1, 0, 0),  ! (2 - 0) (t         ) Z^ max:= Y
	(kaddpxto,     $+1, 1, 1, 0),  ! (2 - 0) (t s       ) Z^ +:= Y*s
	(ksubpxto,     $+1, 1, 1, 0),  ! (2 - 0) (t s       ) Z^ -:= Y*s

	(knegto,       $+1, 1, 0, 0),  ! (1 - 0) (t         ) -:= Z^
	(kabsto,       $+1, 1, 0, 0),  ! (1 - 0) (t         ) abs:= Z^
	(kbitnotto,    $+1, 1, 0, 0),  ! (1 - 0) (t         ) inot-:= Z^
	(knotto,       $+1, 1, 0, 0),  ! (1 - 0) (t         ) not:= Z^
	(ktoboolto,    $+1, 1, 0, 0),  ! (1 - 0) (t         ) istrue:= Z^

	(ktypepun,     $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := t(u@(Z^))
	(kfloat,       $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Int   to real t
	(kfix,         $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Real   to int t
	(ktruncate,    $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,u) Mask to width of u, but type is widened to t
	(kwiden,       $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) Mask to width of u, but type is widened to t
	(kfwiden,      $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r32 to r64
	(kfnarrow,     $+1, 2, 0, 0),  ! (1 - 1) (t u       ) Z' := cast(Z,t) r64 to r32

	(kstartmx,     $+1, 0, 0, 0),  ! (0 - 0) (          ) -
	(kresetmx,     $+1, 1, 0, 0),  ! (0 - 0) (t         ) -
	(kendmx,       $+1, 1, 0, 0),  ! (0 - 0) (t         ) -

	(kproc,        $+1, 0, 0, M),  ! (0 - 0) (M         ) ?
	(ktcproc,      $+1, 0, 0, M),  ! (0 - 0) (M         ) ?
	(kendproc,     $+1, 0, 0, 0),  ! (0 - 0) (          ) ?
	(kistatic,     $+1, 1, 0, M),  ! (0 - 0) (M t       ) Define idata label (must be followed by correct DATA ops)
	(kzstatic,     $+1, 1, 0, M),  ! (0 - 0) (M t       ) Define zdata label and reserve sufficient space
	(kdata,        $+1, 1, 0, A),  ! (0 - 0) (M L C t   ) Constant data. For block types, there can be multiple C values
	(kparam,       $+1, 1, 0, M),  ! (0 - 0) (M t       ) Define param
	(klocal,       $+1, 1, 0, M),  ! (0 - 0) (M t       ) Define local
	(krettype,     $+1, 1, 0, 0),  ! (0 - 0) (t         ) Define return type
	(kvariadic,    $+1, 0, 0, 0),  ! (0 - 0) (          ) Variadic C function
	(kaddlib,      $+1, 0, 0, S),  ! (0 - 0) (S         ) Define import library
	(kextproc,     $+1, 0, 0, M),  ! (0 - 0) (M         ) Define imported proc

	(klabel,       $+1, 0, 0, L),  ! (0 - 0) (          ) ?
	(klabeldef,    $+1, 0, 0,MA),  ! (0 - 0) (          ) ?
	(ksetjmp,      $+1, 0, 0, 0),  ! (1 - 0) (          ) For C
	(klongjmp,     $+1, 0, 0, 0),  ! (1 - 1) (          ) For C

	(ksetcall,     $+1, 0, 1, 0),  ! (0 - 0) (n         ) ?
!	(ksetcall,     $+1, 0, 1, A),  ! (0 - 0) (n         ) ?

	(ksetarg,      $+1, 0, 1, 0),  ! (0 - 0) (n         ) ?
	(kloadall,     $+1, 0, 0, 0),  ! (0 - 0) (          ) ?

	(keval,        $+1, 0, 0, 0),  ! (1 - 0) (          ) Evaluate Z [load to an actual register], then pop
	(kcomment,     $+1, 0, 0, 0),  ! (0 - 0) (C         ) Comment C (a string)
	(kendprog,     $+1, 0, 0, 0),  ! (0 - 0) (          ) End-of-program marker.
!------------------------- -
	(kstoresl,     $+1, 1, 0, M),  ! (2 - 0) (M t       ) M := slice(Y, Z)
	(kstoresld,    $+1, 1, 0, M),  ! (2 - 1) (M t       ) M := slice(Y, Z); leave Y on stack
	(ksliceupb,    $+1, 1, 0, 0),  ! (2 - 1) (M t       ) Z' := Z.upb
	(kslicelen,    $+1, 1, 0, 0),  ! (2 - 1) (M t       ) Z' := Z.len
	(ksliceptr,    $+1, 1, 0, 0),  ! (1 - 1) (t         ) Z' := Z.sliceptr
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
!	(kwd_id=0,		"Kwd"),			!Is a keyword
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

=== mc_genmcl_dummy.m 0 0 8/24 ===
global proc genmcl=
end
=== mc_libmcl.m 0 0 9/24 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

int mclseqno

[-1..10]mclopnd smallinttable
[20]psymbol nametable
int nnametable

global macro isframex(d) = (d.id in [local_id, param_id])

global macro mcomm = mgencomment

export proc mclinit=
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

	m:=pcm_allocnfz(mclrec.bytes)

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
		if a.mode=a_xreg or (b and b.mode=a_xreg) then
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

export func mgenint(int64 x,int mode=tpi64)mclopnd a=
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

export func mgenrealimm(real64 x,int mode=tpr64)mclopnd a=
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

export func mgenmem(psymbol d, int mode=tpvoid)mclopnd a=
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

!CPL "GENEXTNAME", S

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

!CPL "ADDNAMESYM", D.NAME

	if nnametable<nametable.len then
		nametable[++nnametable]:=d
	else
		merror("Ext nametab overflow")
	fi
end
=== mc_genss.m 0 0 10/24 ===
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

=== mc_decls.m 0 0 11/24 ===
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
	union
		u32 mpos
		u32 lineno				!used by aa assembler
	end

!	int xxpos:(sourceoffset:24, fileno:8)
!	ichar xcomment
	[r0..r15]byte regend		!1 indicates register freed.
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
export ref func (ref void)int icheckasmlabel
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


!CPL "MCDECLS:START", =PSTREC.BYTES


!CPL "DONE MC_DECLS/START",PLOADOPX.LWB,PLOADOPX.UPB
!FOR I IN PLOADOP.BOUNDS DO
!	CPL I, PSTDNAMES[I],MCLNAMES[PLOADOP[I]], MCLNAMES[PLOADOPX[I]]
!OD
end

=== mc_objdecls.m 0 0 12/24 ===
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
=== mc_writeasm.m 0 0 13/24 ===
!export int assemtype='AA'

const useintelregs=1
!const useintelregs=0

!const showsizes=1
const showsizes=0

[8, r0..r15]ichar nregnames

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
	str[1]:=0

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
!CPL "STRMCL",MCLNAMES[OPCODE],A,B
	comment:=nil

!IF A THEN
!	CPL"###", MCLNAMES[OPCODE], VALTYPENAMES[A.VALTYPE]
!FI

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
!CPL "LAB: ", =VALTYPENAMES[A.VALTYPE]
		if a.valtype=label_val then
			fprint @str,"L#:",a.value
		else
			recase m_labelname
		fi
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

!		ASMSTR("; ")
!		ASMSTR(strint(a.size))
!		ASMSTR(" ")
!		ASMSTR(strint(b.size))
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

!ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO))

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
!CPL "VALUE:", =VALTYPENAMES[A.VALTYPE]
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
=== mc_writeexe.m 0 0 14/24 ===
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

=== mc_writeobj.m 0 0 15/24 ===
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

!CPL "OBJ/PRESS KEY"; STOP 1 WHEN OS_GETCH()=27

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

!CPL =NAMELEN

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
!CPL "ADDSYM",  =NSYMBOLS, =R.SHORTX,=R.LONGX

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
!	stringtable[++nstrings]:=s
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

!IF S.EXPORTED THEN
!CPL "EXPORTED:",S.NAME
!FI
		if s.exported then
			name:=getbasename(name)
!		else
!			name:=getfullname(s)
		fi

!CPL "CST",I,S.NAME, NAME


!		addsymbol(makesymbol(s.name, sectionno:sect, storage:scope, value:s.offset))
!CPL =GETFULLNAME(S)
		addsymbol(makesymbol(name, sectionno:sect, storage:scope, value:s.offset))
!		addsymbol(makesymbol(getfullname(s), sectionno:sect, storage:scope, value:s.offset))
!		addsymbol(makesymbol(getqualname(s), sectionno:sect, storage:scope, value:s.offset))

	od
end
=== mx_run_dummy.m 0 0 16/24 ===
global proc runlibfile(ichar filename, int cmdskip)=
	abortprogram("No Run")
end

global proc writemcx(ichar filename)=
end
=== aa_cli.m 0 0 17/24 ===
byte fshowasm
byte fshowss					!early ss
byte fshowsx					!late ss (I think)
byte fshowst					!symbol table
byte fshowpst					!pcl symbol table
byte fshowtiming
byte dologfile
const logfile = "aa.log"

global int axlevel = 0

global enumdata []ichar optionnames=
	(lex_sw,		"lex"),		!first four must be in this order
	(parse_sw,		"parse"),	!this generates mcl as it goes
	(asm_sw,		"asm"),		!o/p asm code
	(nasm_sw,		"nasm"),
	(gen_sw,		"gen"),		!does genss
	(obj_sw,		"obj"),		!ss->obj etc
	(ml_sw,			"ml"),
	(mx_sw,			"mx"),
	(dll_sw,		"dll"),
	(exe_sw,		"exe"),
	(run_sw,		"run"),

	(showasm_sw,	"showasm"),
	(ss_sw,			"ss"),
	(sx_sw,			"sx"),
	(st_sw,			"st"),
	(pst_sw,		"pst"),
	(time_sw,		"time"),
	(v_sw,			"v"),
	(q_sw,			"q"),
	(help_sw,		"help"),
	(out_sw,		"out"),

	(rip_sw,		"rip"),
	(himem_sw,		"himem"),
end

global ichar inputfile
ichar outputfile
ichar sourcecode
ichar outext

proc main=
	ichar ext
	ref strbuffer ss
	int ntokens,t,i,U,j

!CPL =STREC.BYTES

	T:=CLOCK()
	initall()

	getinputoptions()

!	PRINTLN =INPUTFILE
!	PRINTLN =OUTPUTFILE
!	PRINTLN =OUTEXT
!	PRINTLN =OPTIONNAMES[AXLEVEL]
!	PRINTLN =FSHOWASM
!	PRINTLN =FSHOWSS
!	PRINTLN =FSHOWSX
!	PRINTLN =FSHOWST
!	PRINTLN =FSHOWPST

	if fverbose then showcaption() fi
	if not fquiet then println "Assembling",inputfile,"to",outputfile fi

	loadsourcefile()

	if axlevel=lex_sw then
		lextest(inputfile)
		finish
	fi

	parsefile()

finish:
	showlogfile()
end

proc loadsourcefile=
	sourcecode:=cast(readfile(inputfile))
	if sourcecode=nil then
		loaderror_s("Can't load file:",inputfile)
	fi
end

proc parsefile=

	readmodule(sourcecode)

	checkundefined()

!	if nundefined then
!		println "Couldn't assemble - press key"
!		os_getch()
!		stop 1
!	fi
!
!	scanglobals()			!fixup any globals and imports
end

proc showlogfile=			!CLOSELOGFILE
	filehandle f
	[512]char str
	ichar s

	return when fshowasm+fshowss+fshowsx+fshowst+fshowpst=0

PRINTLN "PRESS KEY..."; STOP WHEN OS_GETCH()=27

	f:=fopen(logfile,"wb")

	println @f, "AA LOG FILE"


	if fshowst then
		showst(f)
	fi

	if fshowasm and axlevel>=parse_sw then
		println @f, "\nASM File:"
		s:=pcl_writeasm()
!PRINTLN "PRESS KEY2..."; STOP WHEN OS_GETCH()=27

		println @f, s
!		println @f, pcl_writeasm()
	fi

	fclose(f)

	print @str,"\\m\\scripts\\med.bat", logfile
	os_execwait(str,0,nil)
end

proc initall=
	pcm_init()
	initlex()
	initlib()
end

proc lextest(ichar file)=
CPL "LEXTEST"
	int nn:=0
	initsourcefile(sourcecode)

	lxsymbol:=eolsym
	while lxsymbol<>eofsym do
		lex()
		++nn
!		PRINTSYMBOL()
	od

	println "Lexing done:",nn,"tokens"
end

proc getinputoptions=
	const slash='-'
	int i,j,k
	int paramno,pmtype,sw
	ichar name,value,ext

	paramno:=1

	while pmtype:=nextcmdparamnew(paramno,name,value,".asm") do
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
				loaderror("More than one input file")
			fi
			inputfile:=pcm_copyheapstring(name)

		when pm_libfile then
			addsearchlib(convlcstring(name))
		esac
	od

	if inputfile=nil and nsearchlibs=0 then
		showcaption()
		println
		println "Usage:"
		println "	",,cmdparams[0],"filename[.asm]           # Assemble filename.asm to filename.exe"
		println "	",,cmdparams[0],"-help                    # Show other options"
		stop 1
	fi

	if fshowss or fshowsx or fshowasm then
		dologfile:=1
	fi

	if axlevel=0 then
		axlevel:=exe_sw
		outext:="exe"
	fi

	if axlevel in [dll_sw, obj_sw] then highmem:=2 fi

	if outputfile=nil then outputfile:=inputfile fi
	outputfile:=pcm_copyheapstring(changeext(outputfile, outext))

	addsearchlib("msvcrt")
	addsearchlib("gdi32")
	addsearchlib("user32")
	addsearchlib("kernel32")
end

proc do_option(int sw, ichar value)=

	switch sw
	when lex_sw..run_sw then
		axlevel:=sw
		outext:=optionnames[sw]			!not all are meaningful

	when showasm_sw then fshowasm:=1
	when ss_sw then fshowss:=1
	when sx_sw then fshowsx:=1
	when st_sw then fshowst:=1
	when pst_sw then fshowpst:=1
	when time_sw then fshowtiming:=1
	when v_sw then fverbose:=1
	when q_sw then fquiet:=1
	when help_sw then showhelp()
	when out_sw then outputfile:=pcm_copyheapstring(value)
	when rip_sw then highmem:=1
	when himem_sw then highmem:=2

	end switch

end

proc showhelp=
!	showcaption()
	println
	println strinclude "aa_help.txt"
	stop 1
end

proc showcaption=
	println "AA3 Assembler",$date
end

proc loaderror(ichar mess)=
	println "Error:",mess
	stop 1
end

proc loaderror_s(ichar mess,s)=
	[256]char str
	print @str, mess, s
	loaderror(str)
end

proc addsearchlib(ichar name)=
	[300]char str

	name:=changeext(name,"")

	for i to nsearchlibs do
		if eqstring(searchlibs[i],name) then return fi
	od

	if nsearchlibs>=maxsearchlibs then
		loaderror("Too many DLLs")
	fi
	++nsearchlibs
	searchlibs[nsearchlibs]:=pcm_copyheapstring(name)
end
=== aa_decls.m 0 0 18/24 ===
!AA/PCL Assembler Global Decls

global type symbol = ref strec

global record strec =
	ichar name			!name of symbol (named token/keyword or identifier)
	ref strec next		!in module name list
	psymbol pdef
	mclopnd expr	!named constants: valuerec([label],[value])

	byte symbol			!type of token, eg. namesym
	byte ksymbol		!type of keyword, eg. opcodesym
	byte subcode		!when used as keyword
	byte regsize		!for reg keywords

	BYTE ISCOND
	[3]byte spare
!	[4]byte spare
end

global record stlistrec =
	symbol def
	ref stlistrec nextitem
end

global int lxfileno=0	!*@ current source file number
global int lxlineno=0	!*@ current source line number

global int nsourcefiles=0	!no. of linear file names

global const maxsearchlibs=30
global const maximportlibs=30
export [maxsearchlibs]ichar searchlibs
export [maximportlibs]ichar importlibs
export int nsearchlibs
export int nimportlibs

!export int nsymimports=0, nsymexports=0

!global const hstsize=32768
global const hstsize=65536
!global const hstsize=131072
!global const hstsize=65536*4
!global const hstsize=1048576
!global const hstsize=1048576*2
!global const hstsize=1048576*4
!global const hstsize=1048576*8
!global const hstsize=1048576*16

global const hstmask=hstsize-1
global [0:hstsize]symbol lexhashtable

global symbol symboltable, symboltablex


global ref void logdev		!dest for diagnostics and output of tables

global int fverbose=0		!whether to display message for each pass
global int fquiet=0

global int LINECOUNT=0

global int nundefined=0
global int alineno=0

global int ss_zdatalen
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

global const init_ss_symbols=16384
global ref []symbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref stlistrec globalimportlist		!all global vars and imports across all moduls

global byte highmem						!0/1/2 = lowmem+norip/lowmem+rip/himem+rip

=== aa_lex.m 0 0 19/24 ===
!Tokeniser Module
macro testmode=0

const etx = 26
const cr  = 13
const lf  = 10

!the following returned by updated by lexreadtoken()

global int lxsymbol		!* main symbol kind
global int lxsubcode	!* for some symbols, which specific keyword

global int64 lxvalue
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
int lxhashvalue

global ref byte lxsptr		!@ points to next char in source
ref byte lxstart		!@ start of source code for this file
global symbol lxsymptr		!set by lookuplex()

[0..255]char namemap
[0..255]char commentmap

global proc lex=
!lowest level lex() function, reads names, numbers etc but does no lookups or translations
!returns results in lx-vars. Current source pointer should be in lxsptr
	ref byte ss
	int  c, hsum, length
	ref byte pstart

	lxsubcode:=0
	ss:=lxsptr

!CPL "LEX1", REF VOID(LXSPTR)

!	doswitchu c:=ss++^
	doswitch c:=ss++^

	when 'a'..'z','$','_','.' then
		pstart:=ss-1		!point to start of name in source buffer
		hsum:=c
	doname:

!		doswitch c:=ss++^
!		when 'a'..'z','0'..'9','_','$','.' then
!			hsum:=hsum<<4-hsum+c
!		when 'A'..'Z' then
!			(ss-1)^:=c+32
!			hsum:=hsum<<4-hsum+c+' '
!		else
!			--ss
!			exit
!		end

		docase namemap[c:=ss++^]
		when 1 then
			hsum:=hsum<<4-hsum+c
		when 2 then
			(ss-1)^:=c+' '
			hsum:=hsum<<4-hsum+c+' '
		else
			--ss
			exit
		end docase

		lxlength:=ss-pstart
		lxhashvalue:=hsum<<5 -hsum

		if lookuplex(cast(pstart),lxlength) then
			if lxsymptr.ksymbol then			!keywords take priority here
				lxsymbol:=lxsymptr.ksymbol
				lxsubcode:=lxsymptr.subcode
			else
				lxsymbol:=lxsymptr.symbol
			fi
		else
			lxsymbol:=namesym
		fi

		exit

	when 'A'..'Z' then
		pstart:=ss-1
		hsum:=pstart^:=c+32
!		pstart^:=pstart^+32
		goto doname

	when '0'..'9' then
		LXSPTR:=SS
		readnumber(c)
		RETURN
!		exit

	when '`' then
		pstart:=ss		!point to start of name in source buffer
		hsum:=0

		while namemap[c:=ss++^] do
			hsum:=hsum<<4-hsum+c
		od
		--ss

		lxsymbol:=namesym
		if pstart=ss then
			lxerror("NULL ` name")
		fi
		lxlength:=ss-pstart
		lxhashvalue:=hsum<<5-hsum

		if lookuplex(cast(pstart),lxlength) then
			lxsymbol:=lxsymptr.symbol			!can't be a keyword
			if lxsymbol=0 then					!assume was a keyword; use as name
				lxsymbol:=lxsymptr.symbol:=namesym
			fi
		fi
		exit

	when '!',';','#' then			!comment to eol

		while commentmap[ss++^] do od

		if (ss-1)^=0 then --ss fi
!
		++lxlineno

		lxsymbol:=eolsym
		exit

	when ',' then
		lxsymbol:=commasym
		exit

	when ':' then
		if ss^=':' then
			lxsymbol:=dcolonsym
			++ss
		else
			lxsymbol:=colonsym
		fi
		exit

	when '[' then
		lxsymbol:=lsqsym
		exit

	when ']' then
		lxsymbol:=rsqsym
		exit

	when '+' then
		lxsymbol:=addsym
		exit

	when '-' then
		lxsymbol:=subsym
		exit

	when '*' then
		lxsymbol:=mulsym
		exit

	when '/' then
		lxsymbol:=divsym
		exit

	when '=' then
		lxsymbol:=eqsym
		exit

	when '\'' then
		pstart:=ss

		do
			case ss++^
			when '\'' then
				exit
			when cr,lf then
				lxerror("String not terminated")
			esac
		od
		length:=ss-pstart-1
		lxvalue:=0
		for i:=length downto 1 do
			lxvalue:=lxvalue<<8+(pstart+i-1)^
		od
		lxsymbol:=intconstsym
		exit

	when '"' then
		pstart:=ss

		do
			case ss++^
			when '"' then
				lxsvalue:=cast(pstart)
				lxlength:=ss-pstart-1
				(lxsvalue+lxlength)^:=0
				lxsymbol:=stringconstsym
				exit all
			when cr,lf,etx,0 then
				lxerror("String not terminated")
			esac
		od

	when ' ',9 then

	when cr then			!lf expected to follow

	when lf then
		++lxlineno
		lxsymbol:=eolsym
		exit

	when 0,etx then
		lxsymbol:=eofsym
		--ss
		exit
	when 255 then				!needed if doswithu is used
	else
		lxsymbol:=errorsym
		lxvalue:=c
		exit

	end doswitch

	lxsptr:=ss

end

global proc initlex=
	lxsubcode:=0
	lxsymbol:=errorsym

	lxlineno:=0

	for c in namemap.bounds do
		if c in 'a'..'z' or c in '0'..'9' or c in ['_','$'] or c='.' then
			namemap[c]:=1
		elsif c in 'A'..'Z' then
			namemap[c]:=2				!upper case
		fi
		commentmap[c]:=1
	od

	commentmap[0]:=0
	commentmap[lf]:=0

	inithashtable()
end

proc readreal(ref[]char s,int slen, intlen,exponseen)=
!intstr is a string containing all digits, before and after decimal point
!intlen=0:  no decimal point, so fractional part is empty
!intlen<>0: length of integer part
!expon=1:   e/E was last char, so need to read exponent first
!expon=0:   No e/E seen, so no exponent
	int i,fractlen,expon,exponsign,c,digs
	int64 x

	if intlen=0 or intlen=slen then
		fractlen:=0
	else
		fractlen:=slen-intlen
	fi

	expon:=0
	exponsign:=0

	if exponseen then
		case c:=lxsptr++^
		when '+' then
		when '-' then
			exponsign:=1
		else
			--lxsptr
		esac

		digs:=0
		do
			if (c:=lxsptr++^) in '0'..'9' then
				expon:=expon*10+c-'0'
				++digs
			else
				--lxsptr
				exit
			fi
		od

		if digs=0 then
			lxerror("Exponent error")
		fi
		if exponsign then expon:=-expon fi
	fi

	expon:=expon-fractlen

	lxxvalue:=0.0

	for i:=1 to slen do
		c:=s^[i]
		lxxvalue:=lxxvalue*10.0+(c-'0')
	od

	if expon>0 then
		to expon do
			lxxvalue:=lxxvalue*10.0
		od
	elsif expon<0 then
		to -expon do
			lxxvalue:=lxxvalue/10.0
		od
	fi

	lxsymbol:=realconstsym
end

proc readnumber(int c)=
!A digit c 0..9 has just been read. Numeric formats are:
!1234
!0x1234
!2x1101
!Nx....		possible
	ref byte ss
	int d,intlen,slen
	[1256]char str

	ss:=lxsptr

	d:=ss^
	case d
	when 'x','X' then			!other base
		case c
		when '0' then			!hex
			++ss
			lxsptr:=ss
			readhex()
			return
		when '2' then			!binary
			++ss
			lxsptr:=ss
			readbinary()
			return
		else
			cpl c
			lxerror("Base not supported")
		esac
	esac

!assume decimal
	str[1]:=c
	slen:=1
	intlen:=0

	do
		if (c:=ss++^) in '0'..'9' then
			str[++slen]:=c
		elsecase c
		when '_','\'' then
		when '.' then
			intlen:=slen
		when 'e','E' then
			lxsptr:=ss
			readreal(&str,slen,intlen,1)
			return
		else
			--ss
			exit
		fi
	od

	lxsptr:=ss

	if intlen then
		readreal(&str,slen,intlen,0)
		return
	fi

	if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
		lxerror("Overflow in 64-bit value")
	fi

	lxsymbol:=intconstsym

	lxvalue:=0
	for i:=1 to slen do
		lxvalue:=lxvalue*10+str[i]-'0'
	od
end

proc readbinary=
!positioned at start of binary seq; 0 chars read yet
	int ndigs, c

	ndigs:=0
	lxvalue:=0
	do
		case c:=lxsptr++^
		when '0' then
			lxvalue:=lxvalue*2
			++ndigs
		when '1' then
			lxvalue:=lxvalue*2+1
			++ndigs
		when '_','\'','`' then
		elsif c in '2'..'9' then
			lxerror("Bad binary digit")
		else
			--lxsptr
			exit
		end
	od

	if ndigs=0 then
		lxerror("No bin digits")
	elsif ndigs>64 then
		lxerror("Overflow in binary number")
	fi
	lxsymbol:=intconstsym
end

proc readhex=
!positioned at start of hex seq; 0 chars read yet
	int ndigs,c

	ndigs:=0
	lxvalue:=0
	do
		if (c:=lxsptr++^) in '0'..'9' then
			lxvalue:=lxvalue*16+c-'0'
			++ndigs
		elsif c in 'A'..'F' then
			lxvalue:=lxvalue*16+(c-'A'+10)
			++ndigs
		elsif c in 'a'..'f' then
			lxvalue:=lxvalue*16+(c-'a'+10)
			++ndigs
		elsif c in ['_','\'','`'] then
		else
			--lxsptr
			exit
		fi
	od

	if ndigs=0 then
		lxerror("No hex digits")
	elsif ndigs>16 then
		lxerror("Overflow in hex number")
	fi
	lxsymbol:=intconstsym
end

global proc ps(ichar caption)=
	PRINT CAPTION,":"
	PRINTSYMBOL()
end

global proc printsymbol(filehandle dev=nil)=
	[1256]char str

	strcpy(&.str,symbolnames[lxsymbol])
	str[strlen(&.str)-2]:=0

	print @dev,&.str
	to 14-strlen(&.str) do print @dev," " od

	case lxsymbol
	when namesym then

		print @dev,lxsymptr.name

	when intconstsym then
		print @dev, lxvalue
	when realconstsym then
		print @dev, lxxvalue
	when stringconstsym then
		print @dev,"""",,lxsvalue,,""""!,,"end"
	when errorsym then
		print @dev,lxvalue
	else
		print @dev,symbolnames[lxsymbol]
		if lxsubcode then
			print " ",,lxsubcode
		fi

	end

	println @dev
end

proc clearhashtable=
!if defined in zdata, then will already be all zeros
!for i:=1 to hashtable.upb do
!	lexhashtable[i]:=void
!od
end

proc inithashtable=
!initialise hash table from kwddata
	[32]char str
	int i

	if hstsize>65536 then
!limit in place because of 16-bit-wide strec fields like .htindex
!	lxerror("hash table limited to 64K entries")
	fi

	clearhashtable()

	for i to mclnames.len do
		addreservedword(mclnames[i]+2,kopcodesym,i)
	od

	addreservedword("and", kopcodesym, m_andx)
	addreservedword("or", kopcodesym, m_orx)
	addreservedword("xor", kopcodesym, m_xorx)
	addreservedword("not", kopcodesym, m_notx)

	for i to dregnames.len do
		addreservedword(dregnames[i],kregsym,regindices[i])
		lxsymptr.regsize:=regsizes[i]
	od

	for i to xregnames.len do
		addreservedword(xregnames[i],kxregsym,i)
	od

	for i to fregnames.len do
		addreservedword(fregnames[i],kfregsym,i)
	od

	for i to mregnames.len do
		addreservedword(mregnames[i],kmregsym,i)
	od

	for i to jmpccnames.len do
		addreservedword(jmpccnames[i],kjmpccsym,jmpcccodes[i])
LXSYMPTR.ISCOND:=1
	od

	for i to setccnames.len do
		addreservedword(setccnames[i],ksetccsym,setcccodes[i])
LXSYMPTR.ISCOND:=1
	od

	for i to cmovccnames.len do
		addreservedword(cmovccnames[i],kmovccsym,cmovcccodes[i])
LXSYMPTR.ISCOND:=1
	od

	for i to prefixnames.len do
		addreservedword(prefixnames[i],kprefixsym,prefixsizes[i])
	od

	for i to segmentnames.len do
!		strcpy(&.str,segmentnames[i])
!		str[strlen(&.str)-3]:=0
!		addreservedword(pcm_copyheapstring(&.str),ksegnamesym,i)
		addreservedword(segmentnames[i],ksegnamesym,i)
	od

	addreservedword("aframe",kregsym,r14); lxsymptr.regsize:=4
	addreservedword("dframe",kregsym,r14); lxsymptr.regsize:=8
	addreservedword("dfp",kregsym,r14); lxsymptr.regsize:=8
	addreservedword("afp",kregsym,r14); lxsymptr.regsize:=4

	addreservedword("astack",kregsym,r15); lxsymptr.regsize:=4
	addreservedword("dstack",kregsym,r15); lxsymptr.regsize:=8
	addreservedword("dsp",kregsym,r15); lxsymptr.regsize:=8
	addreservedword("asp",kregsym,r15); lxsymptr.regsize:=4

	addreservedword("dprog",kregsym,r8); lxsymptr.regsize:=8
	addreservedword("dsptr",kregsym,r9); lxsymptr.regsize:=8

	addreservedword("importlib",kimportlibsym,0)
	addreservedword("importdll",kimportdllsym,0)
	addreservedword("$userip",khighmem,1)
	addreservedword("$highmem",khighmem,2)

!PRINTHASHTABLE(NIL)
!STOP
end

proc addreservedword(ichar name,int symbol,subcode)=
	lxhashvalue:=gethashvalue(name)
	if lookuplex(name,0) then
		cpl =name
		lxerror("DUPL NAME")
	fi

	lxsymptr.symbol:=0
	lxsymptr.ksymbol:=symbol
	lxsymptr.subcode:=subcode
end

function lookuplex(ichar name,int length=0)int=
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)
	symbol e

	int j,wrapped,insource,firstj

	insource:=length
	if length=0 then
		length:=strlen(name)
	fi

	firstj:=j:=(lxhashvalue iand hstmask)		!j=initial hash index

	wrapped:=0

	do
		lxsymptr:=lexhashtable[j]
		if lxsymptr=nil then				!unused entry, not found
			exit
		fi

		if memcmp(lxsymptr.name,name,length)=0 then			!match
			return 1
		fi

		if ++j>hstsize then		!wraparound
			if wrapped then
				println "???????HASHTABLE FULL",hstsize,lxlineno
				stop 1
			fi
			wrapped:=1
			j:=1
		fi
	od

!name not found
	if insource then
		name:=makestring(name,length)
	fi

	if lxsymptr=nil then
		lxsymptr:=pcm_allocz(strec.bytes)
		lexhashtable[j]:=lxsymptr
	fi

	lxsymptr.name:=name
	lxsymptr.symbol:=namesym
	lxsymptr.ksymbol:=0
!	lxsymptr.htindex:=j
!!CPL "SET HTFIRST",NAME
!	lxsymptr.htfirstindex:=firstj
	return 0
end

global proc initsourcefile(ichar source)=
	lxstart:=lxsptr:=cast(source)
	lxlineno:=1
end

global function addnamestr(ichar name)symbol=
!add a new name to the symbol table
!return symptr to new (or existing) generic name
	lxhashvalue:=gethashvalue(name)
	lookuplex(pcm_copyheapstring(name),0)
	return lxsymptr
end

global proc lxerror(ichar m)=			!LXERROR

	fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno

	stop 1
end

global function gethashvalue(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hsum<<4-hsum+c
	od
	return hsum<<5-hsum
end

global proc skiptoeol=
!read lex tokens until eol and consume it
!return entire line as string
!note, exit with lxsptr pointing at the cr (or lf) char
	repeat
		lex()
	until lxsymbol=eolsym or lxsymbol=eofsym
end

function makestring(ichar p,int length)ref char=
!turn counted/non-terminated string from any source, into independent heap string
	ref char s

	s:=pcm_alloc(length+1)
	memcpy(s,p,length)
	(s+length)^:=0
	return s
end
=== aa_lib.m 0 0 20/24 ===
const ptrsize=8

global int currsegment=0		!

!global opndrec dstackopnd
!global opndrec dframeopnd

global int labelno=0
global mclopnd zero_opnd=nil

!global ref mclrec mccode, mccodex

!strbuffer destv
!global ref strbuffer dest=&destv

!global [r0..r19, 1..8]mclopnd regtable

global proc initlib=
	zero_opnd:=mgenint(0)

!	int reg,size
!
!	for reg:=r0 to r15 do
!		for size:=1 to 8 do
!			case size
!			when 1,2,4,8 then
!				regtable[reg,size]:=genreg0(reg,size)
!			esac
!		od
!	od
!	for reg:=r16 to r19 do
!		regtable[reg,1]:=genreg0(reg,1)
!	od

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0

end

!global function xgetregname(int reg)ichar=
!	static [16]char str
!
!	print @&.str,"xmm",,reg-r0
!
!	return &.str
!end

export proc addsymbol(symbol d)=
	if symboltable=nil then
		symboltable:=symboltablex:=d
	else
		symboltablex.next:=d
		symboltablex:=d
	fi
end

!global proc adddef(symbol d)=
!	d.nextdef:=modulenamelist
!	modulenamelist:=d
!end
!
global proc addimport(symbol d)=
	ref stlistrec p

	p:=pcm_alloc(stlistrec.bytes)
	p.def:=d
	p.nextitem:=globalimportlist
	globalimportlist:=p
end

global proc createlabel(symbol symptr,int sym)=
!symptr is a generic st entry
	symptr.symbol:=sym
!	symptr.stindex:=0
	addsymbol(symptr)
end

global proc createnamedconst(symbol symptr, int value)=
	symptr.symbol:=namedconstsym
	symptr.expr:=mgenint(value)
	addsymbol(symptr)
end

global proc createregalias(symbol symptr,int regindex, regsize)=
	symptr.symbol:=kregsym
	symptr.ksymbol:=kregsym
	symptr.subcode:=regindex
	symptr.regsize:=regsize

	addsymbol(symptr)
end

global proc createxregalias(symbol symptr,int regindex)=
	symptr.symbol:=kxregsym
	symptr.ksymbol:=kxregsym
	symptr.subcode:=regindex

	addsymbol(symptr)
end

global proc gerror(ichar mess)=
	println "\nSS code gen error:",mess
	println "On line:", alineno
	println
	stop 1
end

global proc serror(ichar mess)=
	println "\nSyntax error: '",,mess,,"' on line",lxlineno,inputfile
	println
	stop 1
end

global proc serror_s(ichar mess, param)=
	[256]char str
	print @str, mess, param
	serror(str)
end

function inttostr(int64 a)ichar=
	static [64]char str

	getstrint(a,&.str)
	return &.str
end

function realtostr(real a)ichar=
	static [64]char str
	strcpy(&.str,strreal(a))
	return &.str
end

global function buffercreate(int size=1024)ref dbuffer=
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
		cpl
		cpl
		cpl
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

global function bufferlength(ref dbuffer a)int=
	return a.pcurr-a.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=
	return a.pstart+offset
end

global proc addbyte(ref dbuffer a, int x)=
	a.pcurr^:=x
	++a.pcurr
end

global proc addword(ref dbuffer a, int x)=
	a.pcurr16^:=x
	++a.pcurr16
end

global proc adddword(ref dbuffer a, int x)=
	a.pcurr32^:=x
	++a.pcurr32
end

global proc addqword(ref dbuffer a, int64 x)=
	a.pcurr64^:=x
	++a.pcurr64
end

global func getpsymbol(symbol d)psymbol p =
	byte id

	return nil when d=nil

	if d.pdef then
		p:=d.pdef
		if d.symbol=exportedsym then p.id:=export_id fi		!may be updated
		return d.pdef
	fi

	case d.symbol
	when localsym, fwdlocalsym then id:=static_id
	when importedsym then id:=import_id
	when exportedsym then id:=export_id
	else
		serror("getps")
	esac

	p:=d.pdef:=pc_makesymbol(d.name, id)

	p
end
=== aa_parse.m 0 0 21/24 ===
!Globals that describe expression. Syntax is:
! name [+/- immexpr]
!      [+/- immexpr]
psymbol exprlabeldef			!nil or ref to name
int64 exprvalue					!immexpr (0 if none)
real64 exprxvalue@exprvalue
int exprvaltype					!intimm_val or realimm_val, for immexpr part

global proc readmodule(ichar source)=
	symbol symptr
	int sym

!CPL "RM1"
	initsourcefile(source)
!CPL "RM2"
	mclinit()

	lxsymbol:=eolsym

	if lxsymbol=eolsym then
		mmpos:=lxlineno-1
	else
		mmpos:=lxlineno
	fi

!CPL "RM3"
	while lxsymbol=eolsym do

		lex()
!CPL "RM4",symbolnames[lxsymbol]

		switch lxsymbol
		when kopcodesym then
			readinstr()

		when namesym then
			symptr:=lxsymptr
			lex()
			sym:=lxsymbol
			case sym
			when eqsym then
				lex()
				case lxsymbol
				when kregsym then
					createregalias(symptr,lxsymptr.subcode, lxsymptr.regsize)
					lex()
				when kxregsym then
					createxregalias(symptr,lxsymptr.subcode)
					lex()
				else
					readexpression()
					if exprlabeldef then serror("Name not allowed") fi
					createnamedconst(symptr, exprvalue)
				esac

			when colonsym,dcolonsym then
				createlabel(symptr,(sym=colonsym|localsym|exportedsym))
				genmc(m_labelx, mgenmem(getpsymbol(symptr)))
!				symptr.reftype:=fwd_ref
				lxsymbol:=eolsym
				redoloop
			else
				println symptr.name
				serror("colon expected after label")
			esac

		when fwdlocalsym then
			symptr:=lxsymptr
			lex()
			case lxsymbol
			when eqsym then
				serror_s("Redefining label as const:",symptr.name)
			when colonsym,dcolonsym then
!				genmc(m_labelx, genlab(symptr))
				genmc(m_labelx, mgenmem(getpsymbol(symptr)))
				symptr.symbol:=(lxsymbol=colonsym|localsym|exportedsym)
!				symptr.reftype:=fwd_ref
				lxsymbol:=eolsym
				redoloop
			else
				serror("Instruction expected")
			esac

		when importedsym then
			serror_s("Defining imported name:",lxsymptr.name)
		when localsym, exportedsym then
			serror_s("Redefining symbol:",lxsymptr.name)
		when namedconstsym then
			serror_s("2:Const redefined:",lxsymptr.name)

		when kjmpccsym then
			readcondinstr(m_jmpcc)

		when ksetccsym then
			readcondinstr(m_setcc)

		when kmovccsym then
			readcondinstr(m_cmovcc)

		when eolsym then			!blank or comment line
		when eofsym then
			return
		when kimportlibsym then
			lex()
			checksymbol(namesym)
			if nimportlibs>=maximportlibs then serror("Too many import libs") fi
			for i to nimportlibs do
				if eqstring(importlibs[i],lxsymptr.name) then	!already imported
					exit
				fi
			else
				importlibs[++nimportlibs]:=lxsymptr.name
			od
			lex()

		when kimportdllsym then
			lex()
			checksymbol(namesym)
			if nsearchlibs>=maxsearchlibs then serror("Too many DLLs") fi
			for i to nsearchlibs do
				if eqstring(searchlibs[i],lxsymptr.name) then	!already imported
					exit
				fi
			else
				searchlibs[++nsearchlibs]:=lxsymptr.name
			od
			lex()

		when khighmem then
			highmem:=lxsubcode
			lex()

		else
			println "Unknown symbol (possibly redefining regvar):",symbolnames[lxsymbol]
		end switch
	od
	serror("EOL expected")
end

global proc checkundefined=
	int i
	symbol d

	d:=symboltable
	while d, d:=d.next do
		if d.symbol=fwdlocalsym then
			println "Undefined:",padstr(d.name,20)
			++nundefined
		fi
	od
end

proc checksymbol(int symbol)=
	[265]char str

	if lxsymbol<>symbol then
		fprint @&.str,"# expected not #",symbolnames[symbol],symbolnames[lxsymbol]

		serror(&.str)
	fi
end

proc readinstr=
!deal with opcode symbol
	int opcode, n
	mclopnd a,b,c

	opcode:=lxsubcode

	lex()

	switch opcode
	when m_db, m_dw, m_dd, m_dq then
		do
			if lxsymbol=stringconstsym then
				a:=mgenstring(lxsvalue)
				lex()
				genmc(opcode,a)
			else
				a:=readmclopnd()
				genmc(opcode,a)
			fi
			if lxsymbol=commasym then
				lex()
			else
				exit
			fi
		od

	when m_imul3 then
		a:=readmclopnd()
		checksymbol(commasym)
		lex()
		b:=readmclopnd()
		checksymbol(commasym)
		lex()
		c:=readmclopnd()
		SERROR("IMUL3 CAN'T DO 3 OPNDS")

	when m_pcmpistri,m_pcmpistrm, m_shld, m_shrd then
		a:=readmclopnd()
		checksymbol(commasym)
		lex()
		b:=readmclopnd()
		checksymbol(commasym)
		lex()
		c:=readmclopnd()
		if c.mode<>a_imm then serror("pcmpistr/not int") fi
		genmc(opcode,a,b)
		mccodex.c:=c.value

!	when m_proc then
!		while lxsymbol<>eolsym do lex() od
	else
		a:=b:=nil
		n:=0
		if lxsymbol<>eolsym then
			a:=readmclopnd()
			n:=1
			if lxsymbol=commasym then
				lex()
				b:=readmclopnd()
				n:=2
			fi
		fi 

!IF A THEN
!CPL =A, =VALTYPENAMES[A.VALTYPE], =A.DEF
!FI


		if mclnopnds[opcode]<n then
			serror("Missing operand")
		elsif mclnopnds[opcode]>n then
			serror("Extra operand")
		fi
		genmc(opcode,a,b)
	end

end

proc readcondinstr(int opc)=
	mclopnd a,b
	int cond, n

	cond:=lxsubcode
	lex()
	a:=readmclopnd()
	b:=nil
	n:=1

	if lxsymbol=commasym and opc=m_cmovcc then		!ignore dest
		lex()
		b:=readmclopnd()
		n:=2
	fi


!	nopnds:=(a=nil|0|(b=nil|1|2))
!	if nopnds=2 and opc in [m_pcmpistri,m_pcmpistrm] then nopnds:=3 fi

	if n<mclnopnds[opc] then
		serror("C:Too few mclopnds")
	elsif n>mclnopnds[opc] then
		serror("C:Too many mclopnds")
	fi

	genmc_cond(opc, cond, a, b)
end

function readmclopnd:mclopnd=
!position at start of mclopnd
!read reg, expression or complex mclopnd, and return an opndrec
	static [0..8]byte types = (tpu8, tpu8, tpu16, 0, tpu32, 0,0,0, tpu64)
	mclopnd p
	int size

	case lxsymbol
	when kregsym then
		p:=mgenreg(lxsubcode, types[lxsymptr.regsize])
		lex()
		return p
	when lsqsym then
		lex()
		return readaddrmode(0)
	when kxregsym then
		p:=mgenxreg(lxsubcode)
		lex()
		return p
	when kprefixsym then
		size:=lxsubcode
		lex()
		checksymbol(lsqsym)
		lex()
		return readaddrmode(size)

	else
		readexpression()

		if exprlabeldef then
			p:=mgenmemaddr(exprlabeldef)
			p.offset:=exprvalue
		elsif exprvaltype=intimm_val then
			return mgenint(exprvalue)
		else
			return mgenrealimm(exprxvalue)
		fi
		return p

	end
	return nil
end

proc readexpression =
!reads expr into global expr* variables

	psymbol labelx
	int64 valuex
	byte op:=0

	exprlabeldef:=nil
	exprvalue:=0
	exprvaltype:=intimm_val

!PS("RX1")
	readexprdef()					!optional name

!PS("RX2")
	if exprlabeldef then
		case lxsymbol
		when intconstsym, realconstsym, namedconstsym then
			operror
!		when addsym then op:='+'; lex()
!		when subsym then op:='-'
		esac
	else							!no name, start from nothing
		if lxsymbol=addsym then lex(); op:='+'
		elsif lxsymbol=subsym then lex(); op:='-'
		else op:='+'
		fi

	fi

!PS("RX3")
	docase lxsymbol
	when addsym then
		if op then
operror:
			serror("op?")
		fi
		op:='+'
		lex()
	when subsym then
		if op then operror fi
		op:='-'
		lex()

	when mulsym then
		if op then operror fi
		op:='*'
		lex()

	when divsym then
		if op then operror fi
		op:='/'
		lex()

	when intconstsym then
doint:
		if exprvaltype=realimm_val then serror("int/real") fi


		case op
		when '+' then exprvalue+:=lxvalue
		when '-' then exprvalue-:=lxvalue
		when '*' then exprvalue*:=lxvalue
		when '/' then exprvalue:=exprvalue/lxvalue
		else operror
		esac
		lex()
		op:=0

	when realconstsym then
doreal:
		if exprvaltype=intimm_val then
			exprxvalue:=exprvalue
			exprvaltype:=realimm_val
		fi

		case op
		when '+' then exprxvalue+:=lxxvalue
		when '-' then exprxvalue-:=lxxvalue
		when '*' then exprxvalue*:=lxxvalue
		when '/' then exprxvalue:=exprxvalue/lxxvalue
		else operror
		esac
		lex()
		op:=0

	when namedconstsym then
		if lxsymptr.expr.valtype=intimm_val then
			lxvalue:=lxsymptr.expr.value
			doint
		else
			lxxvalue:=real@(lxsymptr.expr.value)
			doreal
		fi

	when rsqsym, eolsym then
		exit

	else
		serror("const expected")
	end docase

	if op then operror fi
	if exprlabeldef and exprvaltype=realimm_val then
		serror("real offset")
	fi
!PS("RXEND")

end

proc readexprdef=
!read term into exprlabeldef/exprvalue
	symbol symptr
	real x
	exprlabeldef:=nil
	exprvalue:=0
	exprvaltype:=intimm_val

	case lxsymbol
	when fwdlocalsym, localsym, exportedsym then
		exprlabeldef:=getpsymbol(lxsymptr)
		lex()
		if lxsymbol=mulsym then		!is extern name
			serror("* applied to non-extern label or applied inconsistently")
		fi

	when importedsym then
		exprlabeldef:=getpsymbol(lxsymptr)
		lex()
		if lxsymbol<>mulsym then		!is extern name
			serror_s("* missing or applied inconsistently", lxsymptr.name)
		fi
		lex()

	when namesym then
		symptr:=lxsymptr
		lex()
		if lxsymbol=mulsym then		!is extern name
			createlabel(symptr,importedsym)
			lex()
		else
			createlabel(symptr,fwdlocalsym)
		fi
		exprlabeldef:=getpsymbol(symptr)
	esac
end

proc readreg(int &reg,&regsize,&scale)=
!positioned at reg symbol
!read R or R*n for address mode
!return (reg, regsize, scale); scale is 0 when no *n

	reg:=lxsubcode
	regsize:=lxsymptr.regsize
	lex()
	if lxsymbol=mulsym then
		lex()
		checksymbol(intconstsym)
		case lxvalue
		when 1,2,4,8 then
		else
			serror("*n must be 1,2,4,8")
		esac
		scale:=lxvalue
		lex()
	else
		scale:=0
	fi
end

function readaddrmode(int size)mclopnd=
![" just seen, positioned at next symbol
!syntax: [Reg+Reg*scale+expr], all items optional, but at least one must be present
!read optional reg, index reg, scale factor, label, and offset
!size is 0, or is 1,2,4,8 when an override was used
	int reg,regsize,scale,regix, addrsize, regixsize, scaleix
	mclopnd p

	reg:=regix:=0
	regsize:=regixsize:=0
	scale:=scaleix:=0

	if lxsymbol=kregsym then
		readreg(reg,regsize,scale)
!	n:=1
		case lxsymbol
		when addsym then
			lex()
			if lxsymbol=kregsym then
				readreg(regix,regixsize,scaleix)

				case lxsymbol
				when addsym,subsym then
					readexpression()
				esac

			else
				readexpression()
			fi
		when subsym then
			readexpression()
		esac
	else
		readexpression()
	fi

	if scale and scaleix then serror("Two *N scales") fi
!if reg=0 and regix=0 and 0 then	serror("Empty address mode") fi
	checksymbol(rsqsym)
	lex()

	if scale and not scaleix then
		swap(reg,regix)
		swap(regsize,regixsize)
		swap(scale,scaleix)
	fi
	if scaleix=0 then scaleix:=1 fi

	if regsize and regixsize and regsize<>regixsize then serror("Addr reg size mismatch") fi

	if exprvaltype<>intimm_val then serror("addr offset") fi
	p:=mgenindex(areg:reg, ireg:regix, scale:scaleix, def:exprlabeldef, size:size,
		 offset:exprvalue)
	return p
end
=== aa_show.m 0 0 22/24 ===
global proc showst(filehandle f)=
	symbol d

	println @f, "AA Symbol Table"

!PRINTHASHTABLE(F)
	d:=symboltable
	while d, d:=d.next do
		print @f, d.name:"12jl", symbolnames[d.symbol]
		if d.symbol=namedconstsym then
			print @f, $, d.expr.value
		fi

		println @f
	od



end

global proc printhashtable(filehandle f)=
	symbol r
	int count,i

	count:=0
	for i:=0 to lexhashtable.upb do
		r:=lexhashtable[i]
		if R AND r.name then
			count+:=1
!			if not r.ksymbol or eqstring(r.name, "push") then
!			if not r.ksymbol then
IF R.ISCOND THEN
				println @f,r.name, R.ksymbol, CONDNAMES[r.subcode]
		fi
		fi
	od
!CPL
println @f, count," items in table",hstsize
end

=== aa_tables.m 0 0 23/24 ===
!x64 Assembler Tables

global enumdata []ichar symbolnames=
	(errorsym,			$),		! Lex error
	(commasym,			$),		! ","
	(colonsym,			$),		! ":"
	(dcolonsym,			$),		! "::"
	(lsqsym,			$),		! [
	(rsqsym,			$),		! ]

	(addsym,			$),		! +
	(subsym,			$),		! -
	(mulsym,			$),		! *
	(divsym,			$),		! /

	(eqsym,				$),		! =

	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen

	(hashsym,			$),		! #

	(intconstsym,		$),		! 123 64 bits signed
	(realconstsym,		$),		! 123.4 64 bits
	(stringconstsym,	$),		! "ABC"

	(namesym,			"name"),		! raw name
	(namedconstsym,		"const"),		! name = expr
	(fwdlocalsym,		"forward"),		! name
	(localsym,			"local"),		! name:
	(importedsym,		"import"),		! name*
	(exportedsym,		"export"),		! name::

	(kopcodesym,		$),		! mov etc
	(kregsym,			$),		! d0, r5, eax etc
	(kxregsym,			$),		! xmm0 etc
	(kfregsym,			$),		! st0 etc
	(kmregsym,			$),		! mmx1 etc
	(kjmpccsym,			$),		! jz etc
	(ksetccsym,			$),		! setz etc
	(kmovccsym,			$),		! cmovz etc
	(kprefixsym,		$),		! dword etc
	(ksegnamesym,		$),		! idata etc
	(kimportlibsym,		$),		! importlib
	(kimportdllsym,		$),		! importdll
	(khighmem,			$),		! userip/highmem

	(kdummysym,			$)		!
end

global []ichar xregnames = (
	"xmm0",				! x0..x3 are used for parameter passing in ABI
	"xmm1",
	"xmm2",
	"xmm3",

	"xmm4",				! x4..x5 are volatile
	"xmm5",

	"xmm6",				! x6..x15 are preserved across functions in ABI
	"xmm7",
	"xmm8",
	"xmm9",
	"xmm10",
	"xmm11",
	"xmm12",
	"xmm13",
	"xmm14",
	"xmm15")

global tabledata []ichar prefixnames, []byte prefixsizes =
	("byte",	1),		
	("u8",		1),		

	("word",	2),
	("word16",	2),
	("u16",		2),

	("word32",	4),
	("dword",	4),
	("u32",		4),

	("word64",	8),
	("qword",	8),
	("u64",		8),

	("tword",	10),
	("word80",	10),
	("u80",		10),

	("word128",	16),
	("u128",	16)
end
=== aa_help.txt 0 1 24/24 ===
'AA' Assembler-Linker for Win64

Assembles ASM files written in a special syntax to EXE, DLL or OBJ format.

Usage:

    aa prog            Assemble prog.asm to prog.exe
    aa prog -dll       Assemble prog.asm to prog.dll
    aa prog -obj       Assemble prog.asm to prog.obj (needs ext. linker)

    aa a b c           Assemble&link modules a.asm, b.asm, c.asm into a.exe

Options:

    -out:file          Name output file (default is .exe applied to 1st module)
    -exe               Generate executable (default)
    -obj               Generate object file (one .obj file for multiple i/p files)
    file.dll           Include library in list of DLLs to search

    -rip               Use rip-relative addressing
    -himem             Generate relocatable code that can run above 2GB

    @file              Read options and files from @ file

-himem option includes -rip. -himem is automatic for OBJ/DLL outputs.

Can only link to external DLL libraries; not other .o/.obj/.lib/.a files.
(For that, generate OBJ output and use an external linker.)

DLLs msvcrt.dll, user32.dll, gdi32.dll, user32.dll are automatically included.
Others can be specified in the ASM file using 'importdll' directives, or on
the command line.
=== END ===
1 aa.m
2 pclax.m
3 pc_api.m
4 pc_decls.m
5 pc_diags_dummy.m
6 pc_run_dummy.m
7 pc_tables.m
8 mc_genmcl_dummy.m
9 mc_libmcl.m
10 mc_genss.m
11 mc_decls.m
12 mc_objdecls.m
13 mc_writeasm.m
14 mc_writeexe.m
15 mc_writeobj.m
16 mx_run_dummy.m
17 aa_cli.m
18 aa_decls.m
19 aa_lex.m
20 aa_lib.m
21 aa_parse.m
22 aa_show.m
23 aa_tables.m
24 aa_help.txt