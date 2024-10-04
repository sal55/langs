=== MA 38 ===
=== cc.m 0 0 1/38 ===
!CLI
!project =
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

!	module cc_headers
    module cc_headersx

	module cc_export

!Diagnostics
	module cc_show
!   module cc_showdummy

	$sourcepath "c:/bx/"
	import pc_api


!end
=== pc_api.m 0 0 2/38 ===
project =
	module pc_decls
	module pc_diags
!	module pc_diags_dummy
	module pc_exp
	module pc_lib
	module pc_tables

	module mc_AuxMCL
	module mc_GenMCL
	module mc_GenSS
	module mc_LibMCL
	module mc_Decls as md
	module mc_OBJdecls
	module mc_Optim
	module mc_StackMCL
	module mc_WriteASM
!	module mc_WriteASM_dummy
!	module mc_WriteNASM
	module mc_WriteEXE
	module mc_WriteOBJ

	module mx_decls
	module mx_run
	module mx_lib
	module mx_write

end

!
!
!
!
!
!
!
!
!
!
=== pc_decls.m 0 0 3/38 ===
!decls

export type psymbol = ref pstrec

global record pstrec = $caligned
	ichar name
	ref pstrec next
	ref pstrec nextparam
	ref pstrec nextlocal
	ref pstrec owner

	byte symbol
	byte id
	byte subcode

!	union
!		int32 index				!misc
!		int32 labelno			!for mcl anonymous labels; and for proc labels?
!		word32 blocksize		!for block return temps (stored as frameids)
!	end
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

!	word16 flags: (
!		isstatic:1,
!		used:1,
!		txdone:1,
!		circflag:1,
!
!		islet:1,
!		addrof:1,
!		noreg:1,
!		ishandler:1,
!
!		atfield:1,
!		atvar:1,
!		istabdata:1,			!mark parallel enum/tabdata arrays
!
!		issubprog:1,			!set in resolvetopname: module is also a subprog
!
!		isimport:1)

	byte asmused			!1 when proc contains asmcode
	byte dllindex			!for dllproc: which dll in dlltable

	byte nretvalues			!function: number of return values (0 for proc)
	byte varparams			!0 or 1; variadic params in B and FF
	byte isthreaded			!
	byte ishandler			!1 if a proc to be part of func tables

!----------------------------------

	ref fwdrec fwdrefs	!fwd ref chain
!	byte reftype		!label pass 2: extern/back/fwd
!	byte segment		!label pass 2: code_seg etc or 0
!
!	int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
!	int16 importindex	!genexe: index into import table
!
!	int16 impindex
!	int16 expindex
!	byte reg
!
	byte scope
!	byte equals			!for vars/params: 1/2/3 means =/:=/::= used

	byte nparams
	i16 nlocals
	i16 impindex
	i16 expindex

end

export type pcl = ref pclrec

global record pclrec =
	byte opcode
	byte opndtype
	byte condcode						!for jumpcc/setcc
	byte size

	u32 blocksize

	union
		struct
			union
				int64 value
				real64 xvalue
				ichar svalue			!also used for data
				int labelno
				psymbol def
				ref void asmcode
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
!					union
					int32 nvariadics	! 0, or arg # that is first variadic
!						int32 isblockret
!					end
				end
				struct					! (x,y) switch
					int32 minlab
					int32 maxlab
				end

!				struct					!for fix/float, source type
!					byte oldcat			!for trunc, it is truncation type
!					byte oldsize
!					union
!						byte oldsigned
!						byte oldwide
!					end
!				end
!
				int32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
				int32 align
				int32 nret				! (x) setretmult: no. of return values
				int32 popone			! (x) jumpcc: leave X on stack
				int32 slicelwb			! (x) for .upb

			end
		end
	end

	u32 pos:(sourceoffset:24, fileno:8)
	byte mode
	byte mode2
	U16 SEQNO

!	[2]byte spare2
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

global psymbol currfunc
global psymbol blockretname

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer

global const maxplibfile=50
global [0..maxplibfile]ichar plibfiles
global int nplibfiles

export ref func (int pos, ichar &filename, &sourceline)int igetmsourceinfo

global byte pcldone, mcldone, ssdone

global byte pverbose

export int assemtype='AA'
=== pc_diags.m 0 0 4/38 ===
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

global proc pshowlogfile=
	[256]char str
	filehandle logdev
	int size
	ref strbuffer ss

!	if not debugmode then return fi
!
!	logdev:=fopen(logfile,"w")
!
!!
!!CPL "PRESS KEY..."; if OS_GETCH()=27 then stop fi
!
!	if fshowmodules then showprojectinfo(logdev) fi
!
!	if fshowasm and dpasslevel>=dmcl_pass then
!		println @logdev,"PROC ASSEMBLY"
!		addtolog(changeext(outfile, "asm"),logdev)
!	fi
!!CPL "PRESS KEY2..."; OS_GETCH()
!
!	if fshowpcl and dpasslevel>=dpcl_pass then
!		addtolog(changeext(outfile, "pcl"),logdev)
!	fi
!	if fshowast3 and dpasslevel>=dtype_pass then addtolog("AST3", logdev) fi
!	if fshowast2 and dpasslevel>=dname_pass then addtolog("AST2", logdev) fi
!	if fshowast1 and dpasslevel>=dparse_pass then addtolog("AST1", logdev) fi
!
!	if fshowst then
!		showsttree("SYMBOL TABLE",logdev)
!	fi
!	if fshowstflat then
!		showstflat("FLAT SYMBOL TABLE",logdev)
!	fi
!!
!	if fshowtypes then
!		printmodelist(logdev)
!	fi
!!
!	size:=getfilesize(logdev)
!	fclose(logdev)
!
!	if size then
!CPL "PRESS KEY..."; if OS_GETCH()=27 then stop fi
!!		print @&.str,"\\m\\ed.bat -w ",logfile
!		print @&.str,"\\m\\ed.bat ",logfile
!
!!		if checkfile(langname+langname+".m") then
!		if checkfile("mm.m") then
!!			os_execwait(&.str,1,nil)
!			os_execwait(&.str,0,nil)
!		else
!			println "Diagnostic outputs written to",logfile
!		fi
!	fi
end

global proc strpcl(pcl p)=
	[256]char pmodestr
	[256]char str
	int opcode, defused
	ichar s
	psymbol d, e

	const showformatted=1

	opcode:=p.opcode

PSSTR(STRINT(P.SEQNO,"Z5"))
PSSTR(" ")
!PSSTR(STRINT(P.pos,"8ZH"))
!PSSTR(" ")

!CPL PCLNAMES[OPCODE], P.SEQNO

!psstr(strint(getlineno(p.pos),"4"))
!psstr(" ")

	case opcode
	when klabel then
!CPL "PCL/LABEL", P.DEF.NAME,P.ISEXPORTED
		strlabel(p.labelno,1)
		return
	when klabeldef then
!CPL "PCL/LABEL", P.DEF.NAME,P.ISEXPORTED
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
		psstr(strpmode(p.mode, (p.mode=tpblock|p.blocksize|p.size)))
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
		print p.xvalue

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

!	CASE P.OPCODE
!	WHEN KSETARG, KSETCALL THEN
!		RETURN
!	ESAC

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
	int n:=p.blocksize, m
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

export proc pcl_writepst=
	filehandle f
	[256]char str

	psymbol d
	int i:=0

	f:=fopen("PSYMTAB","wb")

	println @f, "PROC PC Symbol table"
	println @f

	d:=psymboltable

	while d, d:=d.next do
		++i

		print @f, i:"4", idnames[d.id],,":"
		to 8-strlen(idnames[d.id]) do print @f, " " od

		str[1]:=0
		fullname(str, d)
!		if d.owner then
!			strcpy(str, d.owner.name)
!			strcat(str, ".")
!		fi
!		strcat(str, d.name)

		print @f, str:"25jl"

		print @f, strpmode(d.mode, d.size)

		if d.id=proc_id then
			fprint @f, " Pm:# Loc:#", d.nparams, d.nlocals
		fi

		if d.exported then print @f," Exp" fi
		if d.imported then print @f," Imp" fi
		if d.isthreaded then print @f," TC" fi

!		print @f, " Seg:",segmentnames[d.segment]

		println @f
	od
	println @f

	fclose(f)

end
=== pc_exp.m 0 0 5/38 ===
export func pcl_writepcl(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writeallpcl()

	if filename then
		if pverbose then println "Writing PCL",filename fi
		writefile(filename, d.strptr, d.length)
		nil
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
	genmcl()
	genss(1)
	writecoff(filename)
end

export proc pcl_writedll(ichar filename)=
	genmcl()
	genss()
	genexe(nil, filename, 1)
	writeexe(filename, 1)
end

export proc pcl_writeexe(ichar filename)=
	genmcl()
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

export proc pcl_setflags(int highmem=-1, verbose=-1)=
	if highmem>=0 then phighmem:=highmem fi
	if verbose>=0 then pverbose:=verbose fi
end
=== pc_lib.m 0 0 6/38 ===
!CONST REDUCE=0
CONST REDUCE=1

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

export proc pcl_start(int nunits=0)=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

	if pcldone then pclerror("PCL start?") fi

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
end

export proc pcl_end=
	if pccurr>=pccurr and pccurr.opcode<>kendprog then
		pc_gen(kendprog)
	fi	
	pcldone:=1
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

!CPL "GENPCIX",=SCALE

	if REDUCE AND pccurr.opcode=kload and pccurr.opndtype=int_opnd then

		if opcode=ksubpx then
			-:=pccurr.value
			opcode:=kaddpx
		fi

		if (pccurr-1).opcode=kaddpx then
			p:=pccurr-1
			p.extra+:=pccurr.value*scale + offset

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
	p.blocksize:=length

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
		pccurr.size:=pccurr.blocksize:=size
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
	d.id:=id

	pc_addsymbol(d)
	d
end

global func getfullname(psymbol d)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	[16]psymbol chain
	int n:=0
	psymbol e:=d

	if d.imported then
		if assemtype='AA' then
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

	if assemtype='AA' then
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
=== pc_tables.m 0 0 7/38 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,
		[0:]byte psigned,
		[0:]byte pfloat,
		[0:]byte pmin =						!promoted type when min width applies

	(tpvoid=0,    "void",    	0,	0,0,	tpvoid),

	(tpr32,       "r32",    	4,	0,1,	tpr32),
	(tpr64,       "r64",    	8,	0,1,	tpr64),

	(tpu8,        "u8",      	1,	0,0,	tpu32),
	(tpu16,       "u16",    	2,	0,0,	tpu32),
	(tpu32,       "u32",    	4,	0,0,	tpu32),
	(tpu64,       "u64",    	8,	0,0,	tpu64),

	(tpi8,        "i8",      	1,	1,0,	tpi32),
	(tpi16,       "i16",    	2,	1,0,	tpi32),
	(tpi32,       "i32",    	4,	1,0,	tpi32),
	(tpi64,       "i64",    	8,	1,0,	tpi64),

	(tpblock,     "block",   	0,	0,0,	tpblock),
	(tpvector,    "vector",   	0,	0,0,	tpvector),

	(tplast,      "$last",   	0,	0,0,	0),


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

export enumdata [0:]ichar pclnames,
			[0:]byte pclhastype,
			[0:]byte pclextra =

!                       t  x      (a  b)
	(knop=0,       $+1, 0, 0),  ! (0 - 0) (          ) ?

	(kload,        $+1, 1, 0),  ! (0 - 1) (M L t     ) Z' := M &M L &L 123 4.5 "abc"
	(kiload,       $+1, 1, 0),  ! (1 - 1) (t         ) Z' := Z^
	(kiloadx,      $+1, 1, 2),  ! (2 - 1) (t d       ) Z' := (Y + Z*s + d)^

	(kstore,       $+1, 1, 0),  ! (1 - 0) (M t       ) M := Z
	(kistore,      $+1, 1, 0),  ! (2 - 0) (t         ) Z^ := Y
	(kistorex,     $+1, 1, 2),  ! (3 - 0) (t s d    ) (Y + Z*s + d)^ := X

	(kdupl,        $+1, 0, 0),  ! (1 - 2) (          ) Z' := Y' := Z
	(kdouble,      $+1, 0, 0),  ! (1 - 2) (          ) Count extra instance of Z (only works for top stack item)
	(kswapstk,     $+1, 0, 0),  ! (2 - 2) (          ) (Z', Y') := (Z, Y)
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
	(ktoboolt,     $+1, 1, 0),  ! (1 - 1) (t         ) Z' := istrue Z
	(ktoboolf,     $+1, 1, 0),  ! (1 - 1) (t         ) Z' := not istrue Z
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
	(kaddpxto,     $+1, 1, 2),  ! (2 - 0) (t s d     ) Z^ +:= Y
	(ksubpxto,     $+1, 1, 2),  ! (2 - 0) (t s d     ) Z^ -:= Y

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
	(kistatic,     $+1, 1, 0),  ! (0 - 0) (M t       ) Define idata label (must be followed by correct db etc ops)
	(kzstatic,     $+1, 1, 0),  ! (0 - 0) (M t       ) Define zdata label and reserve sufficient space
	(kdata,        $+1, 1, 0),  ! (0 - 0) (M L C t   ) Constant data. For block types, there can be multiple C values

	(klabel,       $+1, 0, 0),  ! (0 - 0) (          ) ?
	(klabeldef,    $+1, 0, 0),  ! (0 - 0) (          ) ?
	(ksetjmp,      $+1, 0, 0),  ! (1 - 0) (          ) For C
	(klongjmp,     $+1, 0, 0),  ! (1 - 1) (          ) For C

	(ksetcall,     $+1, 0, 1),  ! (0 - 0) (n         ) ?
	(ksetarg,      $+1, 0, 1),  ! (0 - 0) (n         ) ?
	(kloadall,     $+1, 0, 0),  ! (0 - 0) (          ) ?

!------------------------- -
	(kstoresl,     $+1, 1, 0),  ! (2 - 0) (M t       ) M := slice(Y, Z)
	(kstoresld,    $+1, 1, 0),  ! (2 - 1) (M t       ) M := slice(Y, Z); leave Y on stack
	(ksliceupb,    $+1, 1, 0),  ! (2 - 1) (M t       ) Z' := Z.upb
	(kslicelen,    $+1, 1, 0),  ! (2 - 1) (M t       ) Z' := Z.len
	(ksliceptr,    $+1, 1, 0),  ! (1 - 1) (t         ) Z' := Z.sliceptr

	(keval,        $+1, 0, 0),  ! (1 - 0) (          ) Evaluate Z [load to an actual register], then pop
	(kcomment,     $+1, 0, 0),  ! (0 - 0) (C         ) Comment C (a string)
	(kendprog,     $+1, 0, 0),  ! (0 - 0) (          ) End-of-program marker.

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
	(Null_id=0,		"--"),		!Not set
	(import_id,		"Import"),		!Imported symbol (proc or static)
	(proc_id,		"Proc"),		!Local proc
	(static_id,		"Static"),		!Local static
	(local_id,		"Local"),		!Function local var
	(param_id,		"Param"),		!Function param
	(label_id,		"Label"),		!Used in assembly
end

=== mc_auxmcl.m 0 0 8/38 ===
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
	int offset:=16, regoffset:=0

	regoffset:=0

	d:=currfunc.nextparam
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
	mclopnd ax

	ax:=loadopnd(zz, pmode)

	if ispint(pmode) then
		genmc(m_test, ax,ax)
		genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
	else
		merror("jumpt/f real?")
	fi

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
!			if ispfloat(mode) then
!CPL "PUSHLOW"
			if mode in [tpr64, tpr32] then
				loadparam(j, mode, nextxreg)
				if nvariadics and k>=nvariadics then			!variadic floats go to both regs

!I need to move xmm reg to int reg
					imode:=(mode=tpr32|tpu32|tpu64)
					genmc(m_mov, mgenreg(nextireg, imode), mgenreg(nextxreg, mode))
				fi
			else
				loadparam(j, mode, nextireg)
			fi
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

!	pushpcl_reg(getworkireg(), tpu64)
	pushpcl_reg(tpu64)
	ax:=getopnd(zz, tpu64)

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
		if d.id=static_id and phighmem=2 then skip fi
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
!CPL "AMX", MSTROPND(PX)

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
		genmc(m_push, ax)
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
		genmc(m_cqo)
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

	n:=p.blocksize
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
=== mc_genmcl.m 0 0 9/38 ===
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

GLOBAL INT PPSEQNO

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

end

proc px_endproc*(pcl p) =
! ?
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

	genmc(m_resb,mgenint(p.blocksize))
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

	bx:=loadopnd(zz, p.mode)

	if p.mode<>tpblock then
		ax:=mgenmem(p.def, p.mode)
		genmc(m_mov, ax, bx)

	else
		ax:=getworkregm(tpu64)
		genmc(m_lea, ax, mgenmem(p.def, tpu64))
		ax:=makeopndind(ax, tpu64)

		bx:=makeopndind(bx, tpu64)
		copyblock(ax, bx, p.blocksize)
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
	mclopnd ax, bx
	ax:=loadopnd(zz, pmode)
	genmc(m_test, ax,ax)
	genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), bx:=changeopndsize(ax,1))
	genmc(m_movzx, changeopndsize(ax,4),bx)
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
		bx:=mgenreg(r0)								!r0 points to local block value
		regset[r0]:=1
		ax:=makesimpleaddr(mgenmem(blockretname))		!ax is implicit block ref param

		copyblock(ax, bx, p.blocksize)
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
	callblocksize[ncalldepth]:=p.blocksize

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
		pushopnd(zz, pmode)
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

	if pmode<>tpblock then
		px:=getopnd_ind(zz, pmode)
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

		copyblock(px, bx, p.blocksize)

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
		copyblock(px, cx, p.blocksize)

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
	swapopnds(yy,zz)
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
	setnewzz(ax.reg, pmode)
	poppcl()
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

	clearblock(ax, p.blocksize)
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
	genmc(m_cvtss2sd, changeopndsize(fx,4), fx)
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
	unimpl(p)
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
		genmc(m_add, ax, mgenint(z.value*p.scale+p.extra))
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
		genmc(m_sub, ax, mgenint(z.value*p.scale+p.extra))
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
=== mc_genss.m 0 0 10/38 ===
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

global proc genss(int obj=0)=
	int index
	ref mclrec m

!CPL "////////////////GENSS"
!	if ssdone then axerror("genss?") fi
	return when ssdone

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

=== mc_libmcl.m 0 0 11/38 ===
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
		return pcltempopnds[n]
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

EXPORT proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

!CPL =MMPOS:"H"
!CPL =MMPOS:"H"
!CPL =PPSEQNO
!
!CPL =IGETMSOURCEINFO
	if igetmsourceinfo then
		lineno:=igetmsourceinfo(mmpos, &filename, &sourceline)
		CPL =LINENO
		CPL =FILENAME
	else
		lineno:=0
		filename:="?"
	fi


	fprintln "MCL Error: # (#) on Line: # in #, PCL:#",mess,param, lineno, filename,ppseqno

	pcerrorstop(filename, lineno)
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
=== mc_decls.m 0 0 12/38 ===
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
	(pcl_loc=0,	$),					!operand still in pcl instruction
	(reg_loc,	$),					!is in register (look at mode)
	(temp_loc,	$),					!overflow to temporary
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
global [maxcalldepth]u32 callblocksize	!size of any block
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

=== mc_objdecls.m 0 0 13/38 ===
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
=== mc_optim.m 0 0 14/38 ===
global proc peephole(ref mclrec m)=
	ref mclrec m2,m3,mtarget,lastmcl
	int lab1,lab2
	lastmcl:=nil

	if not fppeephole then return fi

!CPL "PEEPHOLE"

	do
		m2:=m.nextmcl
		while m2 and m2.opcode in [m_comment, m_deleted] do m2:=m2.nextmcl od

		switch m.opcode
		when m_procstart then

		when m_procend then
			exit

		when m_jmp then
dojmp:
	GOTO SKIP

		when m_jmpcc then
			if m2.opcode<>m_jmp then goto dojmp fi
!jcc followed by jmp; detect jcc L1; jmp L2; L1: and replace with:
! jncc L2; <deleted>; L1
			lab1:=m.a.labelno
			m3:=m2.nextmcl
			if m3.opcode=m_labelx and m3.a.labelno=lab1 then
				m.a:=mgenlabel(m2.a.labelno)
				m.cond:=asmrevcond[m.cond]
				deletemcl(m2,102)
			fi

		when m_test then
			case lastmcl.opcode
			when m_andx, m_orx, m_xorx then
				if sameregopnd(m.a,m.b) and sameregopnd(m.a,lastmcl.a) then
					deletemcl(m,103)
				fi
			esac

		when m_movzx then
			if m.a.mode=a_reg and m.a.size=8 and m.b.size<4 then
				m.a:=changeopndsize(m.a,4)
			fi
		when m_mov then
			if isreg0(m.a) and isregopnd(m.b) then
				if isreg0(m2.b) and m2.regend[r0] AND M2.A.SIZE=8 then
					m2.b:=m.b
					deletemcl(m,106)
					skip
				fi

				if not isreg0(m2.a) then skip fi
				m3:=m2.nextmcl

				if m2.opcode=m_cmp and m3.opcode=m_jmpcc and m3.regend[r0] then
					m2.a:=m.b
					deletemcl(m,107)
				elsif m2.opcode=m_test and isreg0(m2.b) and
						m3.opcode=m_jmpcc and m3.regend[r0] then
					m2.a:=m.b
					m2.b:=m.b
					deletemcl(m,108)
				elsif m2.opcode in [m_inc, m_dec] and isreg0(m2.a) then
					m.opcode:=m_lea
					m.b:=mgenindex(areg:m.b.reg,offset:(m2.opcode=m_inc|1|-1))
					deletemcl(m2,120)
					redoloop
				elsif m2.opcode in [m_add, m_sub] and isreg0(m2.a) then
					if isconst(m2.b) and (m2.b.value in int32.min..int32.max) then
						m.opcode:=m_lea
						m.b:=mgenindex(areg:m.b.reg,
							offset:(m2.opcode=m_add|m2.b.value|-m2.b.value))
						deletemcl(m2,121)
						redoloop
					elsif isregopnd(m2.b) and m2.opcode=m_add then
						m.opcode:=m_lea
						m.b:=mgenindex(areg:m.b.reg,ireg:m2.b.reg)
						deletemcl(m2,122)
						redoloop
					fi
				fi
			fi

			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg and
				m.b.reg=m2.a.reg and sameoperand(m.a,m2.b) then
				deletemcl(m2,141)
			fi

			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg then
				 if m.a.mode=a_mem and sameoperand(m.a, m2.b) then		!mov [MEM1],Da; mov Db,[MEM1] => mov Db,Da
					m2.b:=mgenreg(m.b.reg)
				fi
			fi
		when m_xorx then
			if isreg0(m.a) and isreg0(m.b) then
				if isreg0(m2.b) and m2.regend[r0] then
					m2.b:=mgenint(0)
					deletemcl(m,110)
				fi
			fi

		when m_lea then
			if isreg0(m.a) and m2.opcode=m_mov then
				if isregopnd(m2.a) and isreg0(m2.b) and m2.regend[r0] then
					m.a:=m2.a
					deletemcl(m2,131)
				fi
			fi

		end switch

skip:
		lastmcl:=m
		m:=m2
	od
end

func isreg(mclopnd a, int reg=rnone)int=
	if not a then return 0 fi
	if a.mode<>a_reg then return 0 fi
	if reg=rnone then return 0 fi
	return reg=a.reg
end

func isreg0(mclopnd a)int=
	if not a then return 0 fi
	if a.mode=a_reg and a.reg=r0 then return 1 fi
	return 0
end

func isregopnd(mclopnd a)int=
	if not a then return 0 fi
	if a.mode=a_reg and isregvar[a.reg] then return 1 fi
	return 0
end

func isconst(mclopnd a)int=
	if not a then return 0 fi
	if a.mode=a_imm and a.valtype=intimm_val then
		return 1
	fi
	return 0
end

proc deletemcl(ref mclrec m,int id=0)=
	[128]char str
	m.opcode:=m_deleted
end

func sameoperand(mclopnd a,b)int=
	return memcmp(a,b,mclopndrec.bytes)=0
end

func sameregopnd(mclopnd a,b)int=
!check if same register operand
	unless a.mode=b.mode=a_reg then return 0 end
	return a.reg=b.reg
end

=== mc_stackmcl.m 0 0 15/38 ===
!'PCS' Support - PCL Operand Stack 

global func getopnd(int n, mode, reg=rnone)mclopnd ax =
!get access mode for operand n
	mclopnd bx
	pcl a
	psymbol d

!MCOMM("GETOP1")

	a:=pclopnd[n]

	case pclloc[n]
	when reg_loc then
		return mgenreg(pclreg[n], mode)

	when temp_loc then
		return mgentemp(n, mode)
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
!CPL "GETMEMOPND", MSTROPND(AX)

	when memaddr_opnd then
		d:=a.def
		if d.id=param_id and d.mode=tpblock then		!pcl mode will be u64
			ax:=mgenmem(a.def, mode)
		else
			ax:=getworkreg_rm(reg, mode)
			genmc(m_lea, ax, mgenmem(a.def, mode))
		fi

	when int_opnd then
		bx:=mgenint(a.value, psize[mode])
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
!MCOMM("GETOPX")

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

!MCOMM("LTR")
!CPL =MSTROPND(AX)
!	if ax.mode=a_imm and ax.valtype=intimm_val and ax.value=0 then
!CPL "LOADING ZERO"
!	
!	fi
	loadtoreg_common(bx, ax)

!	genmc(m_mov, bx, ax)
	bx
end

global func loadtoreg_m(mclopnd ax, int mode, reg)mclopnd=
!same as loadtoreg but if already in a register, will move it to required one if needed
	mclopnd bx

	if ax.mode in [a_reg, a_xreg] then			!already in register
		if ax.reg=reg then return ax fi			!in correct one
	fi

!need loading/moving to given reg
!MCOMM("LTR/M")
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

!	if pclloc[n]=reg_loc then					!means in register
!CPL "BEFORE", STROPNDSTACK()
!
!		freereg(pclreg[n], pclmode[n])
!CPL "AFTER", STROPNDSTACK()
!	fi

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
!			freeireg(reg)
		fi

	else
		if allregs or reg in r0..r5 then
			genmc(m_mov, mgentemp(n, mode), mgenxreg(reg, mode))
!			freexreg(reg)
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

	loadopnd(zz, pclmode[zz])
	oldreg:=pclreg[zz]

	if oldreg=newreg then
		return
	fi

	if regset[newreg] then
		merror("movereg/reg in use")
	fi

	genmc(m_mov, mgenreg(newreg), mgenreg(oldreg))

!	freeireg(oldreg)
	pclreg[zz]:=newreg


	if ispfloat(pclmode[xx]) then
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

!	if pclreg[zz] then		!zz already using a register
!		freereg(reg, mode)
!	fi

	pclloc[zz]:=reg_loc
	pclopnd[zz]:=nil
	pclreg[zz]:=reg
	pclmode[zz]:=mode

!	if ispfloat(mode) then		!in case was same one freed above
!		xregset[reg]:=1
!	else
!		regset[reg]:=1
!	fi
end

global proc freeworkregs(PCL P)=

!Clear everything first

	clear regset
	clear xregset

!Then set the regs still in use as pcl opnds:

	for i to noperands when pclreg[i] do
		if ispfloat(pclmode[i]) then
			xregset[i]:=1
		else
			xregset[i]:=1
		fi
	od

!And ones use for regvars:
!......TBD........


end

!global proc showregs(ichar mess, PCL P)=
!
!CP MESS,":",PCLNAMES[P.OPCODE]," FREEWORKREGS: "
!FOR I:=R0 TO R5 DO
!	PRINT REGSET[I],$
!OD
!FPRINT "// PCL(#):",NOPERANDS
!FOR I TO NOPERANDS DO
!	IF PCLREG[I] THEN
!		PRINT "R",,PCLREG[I]-1,$
!	ELSE
!		PRINT "-"
!	FI
!OD
!CPL
!END

!global proc checkloaded(

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2

	int reg1:=pclreg[zz]

	if reg1=reg2 then return fi

	for i:=noperands-1 downto 1 do
		if pclloc[i]=reg_loc and pclreg[i]=reg2 then
			swap(pclreg[zz], pclreg[yy])
			return
		fi
	else
		CPL CURRFUNC.NAME
		merror("swapopndregs/reg not found")
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
	int newreg, reg

	reg:=ax.reg
	if reg=rframe then reg:=rnone fi

	if reg and not ax.regix then return ax fi
	newreg:=(reg | reg | (ax.regix | ax.regix | getworkireg()))
	bx:=mgenireg(newreg)

!CPL "MS2", MSTROPND(BX), 
	genmc(m_lea, mgenreg(newreg), ax)
	return bx
end

global func stropndstack(int indent=0)ichar=
	static [512]char str
	[512]char str2
	ichar s:=&.str, t

	if indent then
		fprint @s, "                                     #:(", NOPERANDS
	else
		fprint @s, "#:(", NOPERANDS
	fi

	for i to noperands do
STRCAT(S,"<")
STRCAT(S,LOCNAMES[PCLLOC[I]])
STRCAT(S,">")
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
			strcat(s, stropnd(pclopnd[i]))
			strcat(s, ")")
		esac

		IF PCLCOUNT[I]>1 THEN STRCAT(S, "+") FI

		if i<noperands then strcat(s,",") fi
	od

	strcat(s,") D:(")
	for r:=r0 to regmax do
		strcat(s,(regset[r]|"1 "|"0 "))
	od
	strcat(s,") ")

	strcat(s,"<")
	for i to noperands do
		strcat(s, pstdnames[pclmode[i]])
		strcat(s," ")
	od
	strcat(s,">")

	strcat(s,"X:(")
	for r:=r0 to xregmax do
		strcat(s,(xregset[r]|"1 "|"0 "))
	od

	strcat(s,") hwstack:")
	strcat(s,strint(mstackdepth))
	strcat(s," noperands:")
	strcat(s,strint(noperands))
	strcat(s," ncalldepth:")
	strcat(s,strint(ncalldepth))
!	strcat(s," callslots[]:")
!	strcat(s,strint(callslots[ncalldepth]))
	return s
end

global proc showopndstack=
	mgencomment(stropndstack(1))
end

global proc checkallloaded=
	for i to noperands do
		if pclloc[i]=pcl_loc and pclopnd[i].opndtype=mem_opnd then
			loadopnd(i, pclopnd[i].mode)
		fi
	od
end

=== mc_writeasm.m 0 0 16/38 ===
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

	return getfullname(d)

end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

	if fpshortnames then
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

=== mc_writeexe.m 0 0 17/38 ===
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
!const dll_imagebase = 0x40'0000
!const dll_imagebase = 0x41'0000
!const dll_imagebase = 0x23'0000'0000
!const dll_imagebase = 0x24'0000'0000

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

!CPL "WRITEEXE",=OUTFILE

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
!			PRINTLN "====", =CODEADDR:"H", =PR.OFFSET:"H", =OFFSETPTR:"H"
!			PRINTLN "**********  RIP:",=OFFSETPTR^:"H",=IMAGEBASE:"h",=PR.IMMSIZE
			offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)

!CPL "RIP RELOC LOOP:",OFFSETPTR^,OFFSET
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

!	for i to nplibfiles when plibtypes[i]='D' do

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
			if d.imported then
				(ref word32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				u:=nil
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				else
					CPL D.NAME,D.SEGMENT
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
!CPL "??BASE RELOC",(D.SEGMENT=CODE_SEG|"CODE"|"DATA"),(R.RELOCTYPE=ADDR32_REL|"ADDR32"|"ADDR64")
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

!RETURN
	for i to nbaseblocks when blockcounts[i] do
!	for i to nbaseblocks do
!CPL "BASERELOC",I,=BASETABLESIZE,ref byte(P32)-PSTART,=PSTART, =P32
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
!CPL =DIROFFSET
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

!CPL =DEST:"H"
	addr+:=sectiontable[csect].virtoffset		!now is offset rel to imagebase
!CPL =ADDR:"H"

	dest-(addr+4)-extra
end

=== mc_writeobj.m 0 0 18/38 ===
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

=== mx_decls.m 0 0 19/38 ===
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
=== mx_run.m 0 0 20/38 ===
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

=== mx_lib.m 0 0 21/38 ===
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

=== mx_write.m 0 0 22/38 ===
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
=== cc_cli.m 0 0 23/38 ===
enumdata []ichar passnames, []ichar extnames =
	(load_pass,		$,		""),
	(pp_pass,		$,		"i"),
	(parse_pass,	$,		""),
	(type_pass,		$,		""),
	(pcl_pass,		$,		"pcl"),
	(mcl_pass,		$,		"asm"),
	(nasm_pass,		$,		"nasm"),
	(asm_pass,		$,		"asm"),
	(mx_pass,		$,		"mx"),
	(obj_pass,		$,		"obj"),
	(dll_pass,		$,		"dll"),
	(exe_pass,		$,		"exe"),
	(run_pass,		$,		""),
end

byte cc_pass			!one of the above, default is link_pass
byte debugmode
ichar outfile			!base file
ichar outext="exe"

global byte fverbose=1			!0/1/2 = quiet/normal/extra
global byte fshowincludes=0
global byte foptimise=0			!whether to generate optimised j-codes
global byte fmheaders

global macro fpeephole = foptimise iand 1
global macro fregoptim = foptimise iand 2

global byte dointheaders=1				!allow internal std headers
global byte highmem=0			!0/1/2 = normal/rip only/himem

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
global byte fwriteerrors=1			!whether to writer $errors.tmp


ichar entrypointname

enumdata []ichar optionnames, []ref byte optvars, []byte optvalues =
	(load_sw,		"load",			&cc_pass,		load_pass),
	(pp_sw,			"pp",			&cc_pass,		pp_pass),
	(parse_sw,		"parse",		&cc_pass,		parse_pass),
	(type_sw,		"type",			&cc_pass,		type_pass),
	(pcl_sw,		"pcl",			&cc_pass,		pcl_pass),
	(mcl_sw,		"mcl",			&cc_pass,		mcl_pass),
	(asm_sw,		"s",			&cc_pass,		asm_pass),
	(nasm_sw,		"nasm",			&cc_pass,		nasm_pass),
	(obj_sw,		"c",			&cc_pass,		obj_pass),
	(mx_sw,			"mx",			&cc_pass,		mx_pass),
	(dll_sw,		"dll",			&cc_pass,		dll_pass),
	(exe_sw,		"exe",			&cc_pass,		exe_pass),
	(run_sw,		"run",			&cc_pass,		run_pass),

	(opt_sw,		"opt",			&foptimise,		3),
	(opt1_sw,		"o1",			&foptimise,		1),
	(opt2_sw,		"o2",			&foptimise,		2),
	(opt3_sw,		"o3",			&foptimise,		3),

	(paths_sw,		"paths",		&fshowpaths,	1),
	(headers_sw,	"headers",		&fshowheaders,	1),

	(inclpath_sw,	"i",			nil,			1),
	(showincl_sw,	"includes",		&fshowincludes,	1),
	(mh1_sw,		"mheaders",		&fmheaders,		'M'),
	(mh2_sw,		"qheaders",		&fmheaders,		'Q'),

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
	(v2_sw,			"v2",			&fverbose,		3),
	(quiet_sw,		"q",			&fverbose,		0),
	(help_sw,		"h",			nil,			0),
	(help2_sw,		"help",			nil,			0),
	(info_sw,		"info",			nil,			0),
	(ext_sw,		"ext",			&dointheaders,	0),
	(writeheaders_sw,"writeheaders",&fwriteheaders,	1),
	(out_sw,		"out",			nil,			0),
	(stdout_sw,		"stdout",		&fstdout,		1),

	(rip_sw,		"rip",			&highmem,		1),
	(himem_sw,		"himem",		&highmem,		2),
end

const logfile="mcc.log"

int totallines=0
int nstringobjects=0

[sysparams.len]ichar extraparams	!after ":"
[sysparams.len]ichar extravalues
int nextraparams=0

global int progstart, compiletime
int loadtime, parsetime, pcltime, mcltime, asmtime, objtime, linktime, resettime
int inittime

proc main=
	ichar file

!	CPL =STREC.BYTES
!	CPL =SSREC.BYTES
!	CPL =ATTRIBREC.BYTES
!	CPL =PCLREC.BYTES
!	CPL =MCLREC.BYTES
!	CPL =PCLSTACKREC.BYTES
!	CPL =TOKENREC.BYTES


!MCLTEST()


	progstart:=os_clock()
	
	starttiming()
	initdata()

	getinputoptions()

!CPL =DEBUGMODE

	initsearchdirs()

	if fverbose=3 then showsearchdirs() fi

	initlogfile()
	inittime:=gettiming()


	fprintln "Compiling # to #", inputfile, outfile

!CPL "SS3"

	do_loadmodule()

	do_preprocess()
!CPL "SS4"

	do_parsemodule()
!CPL "SS5"

!	do_writemqheaders()

	do_genpcl()

!CPL "SS6",PASSNAMES[CC_PASS]
	case cc_pass
	when mcl_pass then
		do_genmcl()

!	when nasm_pass then
	when asm_pass then
		do_asm()

!	when mx_pass then
!		do_mx()
!	when obj_pass then
!		do_obj()

!	when dll_pass then
!		do_dll()

	when exe_pass then
		do_exe()

	when run_pass then
		do_run()

	else
		if cc_pass>=mcl_pass then
			println passnames[cc_pass],"not ready"
		fi
	esac


!CPL "DONE"
	if fverbose>=2 then
		println "Done."
	fi


	if fshowtiming then
		showtiming()
	fi

	closelogfile()

	stop 0
end

!proc compile_debug=
!	int pass
!	int nextmodule
!	ichar file
!	ichar istr
!
!	PRINTLN "Debug Compile:", debugnames[debuglevel]
!
!	if cc_pass=preprocess_pass then loaderror("debug -E?") fi
!	if ninputfiles>1 then loaderror("Debug: one file only") fi
!
!	initsearchdirs()
!
!	logdest:=2
!	initlogfile()
!
!	do_loadmodule(1)
!
!	if debuglevel>=parse_level then parsemodule(1) fi
!
!	if debuglevel>=genpcl_level then codegen_pcl(1) fi
!
!CPL "SS1"
!	if debuglevel>=genmcl_level then pcl_genmcl() fi
!CPL "SS2"
!
!	if debuglevel=genasm_level then pcl_writeasm(changeext(inputfiles[1],"asm")) fi
!CPL "SS3"
!
!!	if debuglevel>=genss_level then genss() fi
!
!!CPL "DONE"
!!CPL "DONE"
!!CPL "DONE"
!!CPL "DONE"
!!STOP
!
!!Show diagnostics
!
!!CPL =DESTFILENAME
!
!!	if debuglevel>=genexe_level then writeexefile(destfilename,0) fi
!
!
!	if fverbose=3 then println "Doing diags..." fi
!
!!	RESETCOMPILER()
!
!!	if fshowss then showss(0) fi
!
!CPL "SS6"
!	if fshowmcl then
!CPL "SS7"
!		istr:=pcl_writeasm(nil)
!CPL "SS8"
!		println @logdev, istr		
!	fi
!CPL "SS9"
!
!	if fshowpcl then
!!CPL =LOGDEV,"PCL"
!!		showpcl("dummy",1)
!		istr:=pcl_writepcl(nil)
!		println @logdev, istr		
!	fi
!
!	if fshowast then
!		showast(1)
!	fi
!
!	if fshowst then
!		showst("ST",1)
!	fi
!
!	if fshowstflat then
!		showstflat("STFLAT")
!	fi
!
!	if fshowtypes then
!		printmodelist(logdev)
!	fi
!!
!	if fshowfiles then
!		printfilelist(logdev)
!	fi
!REPORTSTUFF()
!
!!CPL =NALLPROCS
!PAUSE("PRESS KEY")
!
!	closelogfile()
!end

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

	loadtime+:=gettiming()
end

proc do_parsemodule=
	int tt
	starttiming()
	parsemodule()
	parsetime+:=(tt:=gettiming())
!	if fshowtiming=2 then fprintln "[parse #:#]",inputfiles[m]:"15jl",tt fi

!	showast()
end

proc do_genpcl=
	return unless cc_pass >= pcl_pass

	codegen_pcl()

!	if fshowpcl then
!		println @logdev, pcl_writepcl(nil)
!
	if cc_pass=pcl_pass then			!need discrete file
		pcl_writepcl(outfile)


	fi
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

proc do_exe=
	return unless cc_pass = exe_pass

	pcl_writeexe(outfile)
end

proc do_run=
	return unless cc_pass = run_pass

!	pcl_exec(cmdskip)
	pcl_exec(0)
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

CPL "PRESS KEY"
STOP WHEN OS_GETCH()=27

!CPL $LINENO

	if fshowmcl and cc_pass>=mcl_pass then
		println @logdev, "PROC ASM"
		println @logdev, pcl_writeasm(nil)
	fi
!CPL $LINENO, FSHOWPCL, FSHOWPST

	if fshowpcl then
		println @logdev, "PROC PCL"
		println @logdev, pcl_writepcl(nil)
!CPL "SHOWPCL", =FSHOWPST
!CPL $LINENO
		if fshowpst then
			pcl_writepst()
		fi
	fi
!CPL $LINENO

!OS_GETCH()

	showast()
!CPL $LINENO

	if fshowst then
		showst("ST")
	fi
!CPL $LINENO

	if fshowstflat then
		showstflat("STFLAT")
	fi
!CPL $LINENO

	if fshowtypes then
		printmodelist(logdev)
	fi
!
!CPL $LINENO

	fclose(cast(logdev))

	print @&.str,"\\m\\scripts\\med.bat -w ",logfile
	print @&.str,"\\m\\scripts\\med.bat ",logfile


	if checkfile("cc.m") then
!		os_execwait(&.str,1,nil)
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
!		if logdest=2 then
!			println @logdev			!make sure pos is not 0 (will stop displaying if first in file)
!		fi

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

	showtime("Init:",		inittime)
	showtime("Load:",		loadtime)
	showtime("Parse:",		parsetime)
	showtime("PCL:",		pcltime)
	showtime("MCL:",		mcltime)
	showtime("ASM:",		asmtime)
	showtime("OBJ:",		objtime)
	showtime("Reset:",		resettime)
!	showtime("SS:",			sstime)
!	showtime("EXE:",		exetime)
	showtime("Link:",		linktime)
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

!CPL =$CMDSKIP

	do
		pmtype:=nextcmdparamnew(paramno,name,value,".c")
!CPL PARAMNO, =PMNAMES[PMTYPE], name, VALUE
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

		when pm_libfile then
			if nlibfiles>=maxlibfile then
				loaderror("Too many lib files")
			fi
			libfiles[++nlibfiles]:=pcm_copyheapstring(name)
		when pm_colon then
			if ++ncolons>1 then
				name:=":"
				value:=nil
		!		recase pm_extra
				goto doextra
			fi
		when pm_extra then
			doextra:
			extraparams[++nextraparams]:=pcm_copyheapstring(name)
			extravalues[nextraparams]:=pcm_copyheapstring(value)
		when 0 then
			exit
		esac
	od

!CPL INPUTFILES[1]

	if cc_pass=0 then cc_pass:=exe_pass fi
	if cc_pass in [dll_pass,obj_pass, nasm_pass] then
		highmem:=2
	fi
	outext:=extnames[cc_pass]

	if inputfile=nil and not fwriteheaders then
		showcaption()
		println "Usage:"
		println "	",,sysparams[1],"prog[.c]                 # Compile prog.c to prog.exe"
		println "	",,sysparams[1],"-help                    # Show options"
		println "	",,sysparams[1],"-info                    # Further info"
		stop 22
	fi

	if fwriteheaders then
		writeheaders()
		stop 20
	fi

	if outfile=nil then
		outfile:=pcm_copyheapstring(changeext(inputfile, outext))
	fi

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
	!		loaderror("Path should end with / or \\: #",value)
		esac

		includepaths[++nincludepaths]:=pcm_copyheapstring(value)

	when help_sw,help2_sw then	showhelp()
	when info_sw then			showextrainfo()

	when out_sw then
		outfile:=pcm_copyheapstring(addext(value,outext))

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
	println strinclude "help.txt"
!	println sinclude("help.txt")

	stop 23
end

proc showextrainfo=
	static ichar infotext=strinclude "info.txt"

	println infotext

	stop 24
end

proc showcaption=
	println "MCC C Compiler",$date,$time
end

INT TTT
proc starttiming =
!	static int tt
	ttt:=os_clock()
end

func gettiming:int=
!	os_clock()-starttiming.tt
	os_clock()-ttt
end
=== cc_decls.m 0 0 24/38 ===
import clib
global type unit = ref unitrec
global type symbol = ref strec

global const maxmodule=200
global const maxlibfile=200
global const maxsourcefile=200

global macro pr(a,b) = a<<16+b

global record tokenrec = 		!should be 32-byte record
	union
		int64 value				!64-bit int
		real xvalue				!64-bit float
		word64 uvalue			!64-bit word
		ref char svalue			!pointer to string or charconst (not terminated)
		ref strec symptr		!pointer to symbol table entry for name
	end
	ref tokenrec nexttoken

	u32	lineno
	u8	fileno
	u8	symbol
	u8	subcode
	u8	flags

	int32 length					!length of name/string/char
	union
		int32 numberoffset			!offset of numeric token within file[fileno]
		int16 paramno				!for macro params
		int16 pasteno
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
	int32 mode				!tnone when there are no normal params
	int16 nparams			!used on first param only
	int16 flags				!used on first param only
end

!mask bits for .flags of tokenrec; these can be combined if both are true
global const tk_macromask = 1		!is a name that is a macro def
global const tk_parammask = 2		!is a name that is a param def
global const tk_macrolit  = 4		!is an processed token that is a macro name
global const tk_pasted    = 8

!global record attribrec =		!keep this 16 bytes
!	byte ax_static				!0 or 1
!	byte ax_equals					!0 or 1 if = used (static/frame vars only)
!	byte ax_varparams				!0 or 1	
!	byte ax_used				!0 or 1	
!	byte ax_forward				!0 or 1: 1 means forward decl of label or function
!	byte ax_frame				!0 or 1: 1 when frameid/paramid
!	byte ax_autovar				!0 or 1: 1 when an autovar with "$" in the name
!	byte ax_nparams				!no. formal params for procid/dllprocid
!
!!	byte SPARE
!	byte ax_moduleno
!!	byte ax_loop				!1 if a loop label
!	union
!		byte ax_align				!1, 2, 4, 8; max align for struct/union
!!		byte ax_dllindex		!for dllproc: which dll in dlltable
!!		byte ax_extmodno		!for proc call chains: module no of proc owner
!		byte ax_flmacro			!function-like macro; used when no params
!	end
!end

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
		int32 index					!enum/label index
		int32 offset
		int32 labelno				!normally used as .index
		byte oldsymbol				!for #define/#undef on keyword
	end

	word32 lineno
	union
		struct
			word16 blockno
			word16 namespace				!experimental: set to namespaces[.nameid]
		end
		word32 nsblock						!combined block no and namespace
	end

	int16 subcode
	word16 mode

	u16 nrefs
	byte namelen
	byte symbol

	byte flags:(addrof:1, varparams:1, flmacro:1, used:1, ismain:1)
	byte nameid
	byte scope					!linkage type
	byte nparams				!no. formal params for procid/dllprocid

	byte align					!1, 2, 4, 8; max align for struct/union
!	byte reg
!	byte noreg
	byte fileno

!ss stuff

	[14]byte spare

!	ref fwdrec fwdrefs		!fwd ref chain
!	int32 stindex			!label pass 2: 0, or 1-based index within coff symboltable
!	int16 impindex
!	int16 expindex
!
!	byte reftype			!label pass 2: extern/back/fwd
!	byte segment			!label pass 2: code_seg etc or 0
!
!	int16 importindex		!genexe: index into import table
end

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record unitrec =
	union
		ref strec def
		int64 value
		word64 uvalue
		real xvalue
		ichar svalue
		ref word16 wsvalue
		ref strec labeldef
		ref caserec nextcase
		int32 ptrscale			!use for derefoffset/addoffset
		int32 offset				!for jdot
	end
	ref unitrec nextunit
	ref unitrec a	!single items, or linked lists
	ref unitrec b
	ref unitrec c

	int32 tag			!kcode tag number
	word32 lineno			!source lineno associated with item; fileno is in top byte

	union
		int32 opcode			!for conversion
		int32 index				!label index
		word32 uindex			!case index
		int32 slength			!for const/string
		int32 wslength
		int32 alength			!for deref units, length of any array before conversion to ptr
		int32 scale			!for scale unit (negative means divide)
		int32 aparams			!callfn/proc: no. actual params
		int32 count			!makelist, no. items
	end

	int32 mode
	int16 memmode				!none, or memmode for name, ptr etc
	int16 convmode				!conversion dest type
	byte fileno
	byte isstrconst			!for string consts: initialised with "..."
	byte iswstrconst
	byte spare1
end

!global record modulerec =
!	ichar name
!	ref strec stmodule
!	int fileno
!	ichar asmstr
!	ichar mhdrstr
!	[maxmodule]byte importmap
!end
!
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
!global const int maxtype=5'000
!global const int maxtype=20'000
!global const int maxtype=30'000
!global const int maxtype=40'000
!global const int maxtype=50'000
global const int maxtype=64'000

global int ntypes
!global int ntypesreset

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
global [0:maxtype]int32	ttshared			!no. of shared instances
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
global [0..maxsourcefile]int32 sourcefilesizes

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
!global int optflag=0			!1=stdoptimise; 0=disabled


global const sourceext="c"
global ref unitrec nullunit

!global byte fverbose=1			!whether to display message for each pass
!global byte fshowincludes=0
!global byte foptimise=0		!whether to generate optimised j-codes

!global int wintarget=1		!one of these three is true
!global int lintarget=0
!global int nostarget=0
global int clineno=0		!set in codegen scanner
global int cfileno=0		!set in codegen scanner

!global int fastasm=0		!1 to pass asm source via memory
!global int fcallback=0		!1 to make all functions callbacks
!global int flinesplicing=0	!1 to deal with \ line continuations
!global ichar assemsource=nil
!global [maxmodule]ichar assemsources

global tokenrec lx				!provides access to current token data
global tokenrec nextlx

!global int tlex=0		!timing

global int debug=0

!global int hstsize	= 16384
global int hstsize	= 65536
!global int hstsize	= 131072
!global int hstsize	= 131072*2

global int hstmask				!filled in with hstsize-1

global ref[0:]ref strec hashtable

global const maxblock=2100,maxblockstack=100
global [0..maxblock]int32 blockowner
global [0..maxblock]int32 blockcounts
global [0..maxblockstack]int32 blockstack
global int currblockno,nextblockno,blocklevel
global ref strec currproc

!global int labelno=0
global const maxnestedloops=64

!global byte dointheaders=1				!allow internal std headers
global ichar dheaderfile=nil			!result of -d:file.h switch

global int structpadding=1
global int callbackflag=0

global int slineno,sfileno

global ichar oemname="MCC"

global ichar mclstr
global int mclstrlen

global int nunits

!GLOBAL INT NLOOKUPS
!GLOBAL INT NCLASHES
!GLOBAL INT NMIXED
!GLOBAL INT NALLPROCS

!GLOBAL INT NMAXPROCS
!GLOBAL INT NPROCPROCS
!GLOBAL SYMBOL STMAXMODULE
!

!global byte highmem=0			!0/1/2 = normal/rip only/himem

global const maxpmodule = maxmodule-1
global const maxpheader = 100
global const maxplib = 100

global [maxpmodule]ichar pmodulelist
global [maxpheader]ichar pheaderlist
global [maxplib]ichar pliblist

global int npmodules
global int npheaders
global int nplibs

!GLOBAL INT NLCURLY, NRCURLY
GLOBAL INT NALLPROCS
GLOBAL INT NALLLOCALS
GLOBAL INT NMAXLOCALS
GLOBAL SYMBOL MAXLOCALPROC
GLOBAL [0:100]INT LOCHIST
GLOBAL INT NLINES
=== cc_tables.m 0 0 25/38 ===
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


global enumdata []ichar typespecnames, []int32 typespectypes, []byte typespecsizes =
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
global enumdata []ichar ccnames=

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

	(jcallproc,	$), ! 
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

global enumdata [0:]ichar namenames, [0:]int32 namespaces, [0:]byte name2pid=
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

global tabledata []ichar stnames, []int32 stsymbols, []int32 stsubcodes=

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

	("__DATE__",	predefmacrosym,	pdm_date),
	("__FILE__",	predefmacrosym,	pdm_file),
	("__LINE__",	predefmacrosym,	pdm_line),
!	("__STDC__",	predefmacrosym,	pdm_stdc),
	("__TIME__",	predefmacrosym,	pdm_time),
!	("__cdecl",		predefmacrosym,	pdm_cdecl),
	("__MCC__",		predefmacrosym,	pdm_mcc),
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

=== cc_lex.m 0 0 26/38 ===
! (C tokeniser module)
ref tokenrec tkptr=nil

int dowhitespace=0

GLOBAL int NINCLUDES

const mcchdr = "mcc.h"

record stackinforec =
	ref char startptr
	ref char sptr
	int32 lineno
	int32 fileno
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
const int maxpastedtokens=7000
!const int maxpastedtokens=87000
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
	int64 nchars,t,hashtot,symtot
	real tsecs
	static strbuffer sbuffer
	static ref strbuffer dest=&sbuffer
	filehandle f
!	ICHAR SS

!CPL "PREPROCESS"

	dowhitespace:=1
	fileno:=loadsourcefile(infile,infile)

!	strcpy(&.outfile,changeext(infile,"i"))

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

!	nextlx.subcodex:=0

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

		ss:=pcm_alloc(nextlx.length+1)		!zero-term in lex(), as headers may need to be
		memcpy(ss,lxsvalue,nextlx.length)	!re-tokenised
		(ss+nextlx.length)^:=0
		lxsvalue:=ss

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
++NLINES
		case lxsptr^
		when cr then
			++lxsptr			!point to lf
			lxsptr++^:=' '		!set lf to space (so that '#' processing works
		when lf then
			lxsptr++^:=' '
		else
!		lxerror("\\ not followed by newline")	
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
++NLINES
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
		!codes above 127 can be names; later, decode the actual unicode from
		!the utf8 sequence, and check for correct ranges of chars that are allowed
!		if (lxsptr-1)^ in 128..255 then goto doname fi
		if 128<=(lxsptr-1)^<= 255then goto doname fi
!		if 128<=(lxsptr-1)^<= 255then recase 'A' fi

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
	int64 aa,cc,pref
	const maxrealdigits=500
	[maxrealdigits+12]char realstr
	ref char rs
	[32]char expstr
	word64 xx1,xx2

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
	int64 a

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

	nextlx.symptr.name:=lxsvalue
	nextlx.symptr.namelen:=nextlx.length
	nextlx.symptr.symbol:=namesym

!CPL LXSVALUE

!PRINTLN NEXTLX.LENGTH:"V",, LXSVALUE:".*"

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
!		hashtable^[i]:=pcm_allocz(strec.bytes)
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
!		println "#ERROR:...."
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
++NLINES
				++nextlx.lineno
			when lf then			!loop
				++lxsptr
++NLINES
				++nextlx.lineno
			esac					!else ignore and loop
!		lxerror("line comment LINE CONT")
		esac
	od
	++nextlx.lineno
++NLINES
end

proc readblockcomment=
!positioned at '*' of '/*'

	do
		while commentmap[(++lxsptr)^] do od		!skip bulk of characters

		case lxsptr^
		when lf then
++NLINES
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
	word64 aa
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

!CPL =AA

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
	word64 aa
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
	word64 aa
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
	word64 aa
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
		aa:=aa*word64(10)+word(p++^-'0')
	od

	nextlx.symbol:=intconstsym
	nextlx.subcode:=ti32

	case ll
	when 0,1 then
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
	else
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
!CPL "HERE"
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
	ref word16 wd,wd0
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
!CPL "STR LOOP",LXSPTR^
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
++NLINES
				++nextlx.lineno
				if lxsptr^=lf then ++lxsptr fi
				nextloop
			when lf then
++NLINES
				++nextlx.lineno
				nextloop
			end						!else use the escaped character itself
		when '"','\'' then		!possible terminators
			if c=termchar then		!terminator char
				exit
			fi
!		when lf,0 then
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
!			(d.name+d.namelen)^:=0			!zero-term param name; it might be an identifier
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
	convertstring(s,u,length)
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
++NLINES
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0
		++lxsptr				!skip lf
	when lf then			!only lfs not preceded by cr
++NLINES
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
!	sprintf(s,"%lld",tk.value)
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

!CPL "ADDBUILDINFO", CODE:"C"
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

!proc printhashtable(ichar caption)=
!	ref strec d
!
!	println caption,,":"
!	for i:=0 to  hstsize-1 do
!		d:=hashtable^[i]
!		if d.name then
!			println i,":",d.name
!		else
!			println i,": ----"
!		fi
!	od
!	println
!end

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
=== cc_parse.m 0 0 27/38 ===
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
!CPL =STRMODE(MBASE)
		commaseen:=0

		docase lx.symbol
		when namesym, mulsym, lbracksym then
			++nitems

			m:=readtype(stmodule,d,mbase,pm)

			if d=nil then
				serror("Var name expected")
			fi

!CPL =STRMODE(M), =PM, =D.NAME, =D.MODE, =STRMODE(MBASE)
!
!if pm then fixparams(d, pm, m) fi

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

!CPL "//PARSEMODULE",STMODULE

	loopindex:=ingeneric:=0
	ist_symptr:=nil
	memset(&casevaluestack,0,casevaluestack.bytes)
!clear casevaluestack

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
!
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
!=========================================


	readmodule()

	endlex()
!CPL "DONE PARSING"
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
	when ktypespecsym then
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
	when ktypespecsym then
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
!		when ti32,ti64 then
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
!				if ttbasetype[ss]=tproc and ttbasetype[tt]=tproc then
!				elsif ttbasetype[ss]=tstruct and ttbasetype[tt]=tstruct then
!				else
	CPL =STRMODE(X.MODE), STRMODE(Y.MODE)
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
			terror("invalid == operands")
		fi

!IF X.MODE=Y.MODE=TR32 OR X.MODE=Y.MODE=TR64 THEN
!CPL "COMPARE FLOAT FOR EQUALITY"
!SERROR("FLOAT=FLOAT")
!FI

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
!		if tttarget[x.mode]<>tttarget[y.mode] then
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
				serror_s("Undefined name ""#""", getstname(lx.symptr))
			fi
		else
			d:=lx.symptr
		fi

		d.used:=1
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

	when charconstsym then
		a:=0
		shift:=0
		pbyte:=lx.svalue
		if lx.length>8 then serror("char const too long") fi

!IF LX.LENGTH>1 THEN
!CPL "MULTICHAR CONST:",LX.SVALUE
!FI

		to lx.length do
			a:=a ior word64(pbyte^)<<shift
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
!CHECKTYPE(D)
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

!CHECKTYPE(D)

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
!			pmx:=fixparams(modvalue[i], m)
			pmx:=modvalue[i]

			if i=1 then				!indicate to caller that this is a normal function
				pm:=pmx
			else					!assume function pointer of some sort
				m:=createprocmode(m,pmx)
			fi
		esac
	od

!CPL "READTYPE",STRMODE(M)

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

		if istop then
!			if lx.symbol=intconstsym and lx.value=0 and nextlx.symbol=rcurlysym then
!				CPL "INIT CLEAR {0}"
!			elsif lx.symbol=rcurlysym then
!				CPL "INIT CLEAR {}"
!			fi
		fi

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

!				if ttbasetype[melem]=tarray and ttbasetype[tttarget[melem]]=tu8 and p.mode=trefchar then
				if ttbasetype[melem]=tarray and ttbasetype[tttarget[melem]]=tchar and p.mode=trefchar then
				else
					p:=coercemode(p,melem)
				fi
			when tstruct then

				mm:=e.mode

				if ttbasetype[mm]=tarray and ttbasetype[tttarget[mm]]=tu8 and p.mode=trefchar then
!				if ttbasetype[mm]=tarray and ttbasetype[tttarget[mm]]=tchar and p.mode=trefchar then
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
!				tttarget[m]<>tu8 then
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

!retry:

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
!CPL "READSEMI"
!SERROR("NULL STMT NOT ALLOWED")
		lex()	
		return nil

	when namesym then
		if nextlx.symbol=colonsym then
			p:=createunit1(jlabelstmt,nil)
			d:=resolvename(currproc,lx.symptr,ns_labels,0)
			if d then
				if d.index then
					cpl lx.symptr.name
					terror("Duplicate label")
				else
					d.index:=++mlabelno
				fi
			else
				d:=createdupldef(currproc,lx.symptr,labelid)
				d.mode:=tvoid
				d.index:=++mlabelno
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
			if isusertype(currproc)<>tnotset then
				goto doreaddecl
			fi
			if ist_symptr then lx.symptr:=ist_symptr fi		!make use of name resolve done by isusertype
			p:=readexpression()
		fi
	when ktypespecsym, ktypequalsym, klinkagesym, kfnspecsym,
		kstructsym,kunionsym,kenumsym then
	doreaddecl:
		return readlocaldecl()

!when kconstantsym then
!	constantseen:=1
!CPL "KCONST"
!	lex()
!	retry

	else						!assume expression
		p:=readexpression()
!	TESTEXPR(p)
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


!IF LX.SYMBOL=LCURLYSYM AND NEXTLX.SYMBOL IN [KFORSYM,KWHILESYM,KSWITCHSYM,KDOSYM] THEN
!CPL "INSTANCE OF 'ELSE { FOR/WHILE/SWITCH/DO}", lx.lineno, sourcefilenames[lx.fileno]
!FI

		pelse:=readblock(1)
	fi

	p:=createunit3(jif,pcond,pbody,pelse)
	p.lineno:=lineno
	return p;
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
!CPL "CALL RESOLVE/GOTO"
		d:=resolvename(currproc,lx.symptr,ns_labels,0)
!CPL "DONE",D
		if d=nil then					!assume fwd ref
			d:=createdupldef(currproc,lx.symptr,labelid)
			d.mode:=tvoid
!		d.index:=++labelno
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
!	coercemode(pindex,tu64)

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
!p.index:=value
!CPL =VALUE

!p.uindex:=value
	p.value:=value
!CPL =P.UINDEX

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
!PS("RLD")
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

!ADDTYPEDEF(D)

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
!CPL "READPARAMS",OWNER.NAME

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
!SERROR("Array type detected in parameter list")
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

!IF NPARAMS>4 THEN
!CPL =NPARAMS
!FI

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
!	INT LINE
!
!	LINE:=LX.LINENO

	owner:=stmodule
	wasdef:=0

!CPL "FUNC"
!++NPROCS


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
!		if scope=local_scope and linkage=none_ss or
!		   scope=exported_scope and linkage=static_ss or
!		   scope=imported_scope and linkage=static_ss then
!		serror("Linkage3 mismatch")
		if scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
		elsif linkage=static_ss then
			scope:=local_scope
		fi


	else
!SKIP:
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
!		serror("Can't define imported function")
		fi

		readfunctionbody(d)
		if lx.symbol=semisym then
			serror("; after function def")
		fi
	fi

!!CPL "FUNCT",D.NAME,D.DEFLIST
!IF EQSTRING(d.name, "sqlite3VdbeExec") then
!
!
!REF STREC E:=D.DEFLIST
!
!WHILE E, E:=E.NEXTDEF DO
!	cpl e.name, strmode(e.mode)
!OD
!fi

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
!if pm.def then			!params are named
		to pm.nparams do
			if pm.def=nil then
!			serror("Param name missing")
			else
				e:=createdupldef(f,pm.def,paramid)
!CPL "RFB/PARAM",E.NAME
				if e.name^='$' then			!assume block ret param
					e.used:=1
				fi

				e.blockno:=1
				e.mode:=pm.mode
			fi
			pm:=pm.nextparam
			pmcount:=1
		od
!elsif pm.nparams then
!	serror("Param names missing")
!fi

!CPL =PMCOUNT, F.NAME


	p:=readcompoundstmt(pmcount)

!REF STREC D:=CURRPROC.DEFLIST
!INT NN:=0
!
!WHILE D, D:=D.NEXTDEF DO
!	IF D.NAMEID=FRAMEID THEN ++NALLLOCALS; ++NN
!CP D.NAME,$
! FI
!
!
!OD
!CPL
!CPL =CURRPROC.NAME, NN
!	LOCHIST[NN]++
!	IF MAXLOCALPROC=NIL OR NN>NMAXLOCALS THEN
!		MAXLOCALPROC:=CURRPROC
!		NMAXLOCALS:=NN
!	FI
!

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

!if t>=tfirstint and t<=tlastint then
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

	case p.tag
	when jaddrof then
		q:=p.a
		if p.alength then
			q.mode:=tttarget[p.mode]
		fi
		fixmemopnd(q)
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
!unless t>=tfirstint and t<=tlastint and t<>tbool or ttisref[t] then
!if isreal(t) and opc in [jpreincr, jpredecr] then
!	q:=createunit2((opc=jpreincr|jaddto|jsubto),p,createconstunit(word64@(1.0),tr64))
!	q.mode:=p.mode
!	return q
!fi

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
!CPL "ADDROF/NAME"
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
!CPL "DOT1"
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
!	if isintcc(y.mode) then			!y is const
!		case y.value
!		when 0 then
!			return createconstunit(0,y.mode)
!
!		when 1 then
!			return x
!		esac
!	fi

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
!		u:=ti64
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
!	x.uvalue *:= y.uvalue
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

!CPL "EVALCONVERT",CONVNAMES[OPC],JTAGNAMES[P.TAG],=OPC

	if opc=soft_c then
	dosoft:
		p.mode:=t
		return 1
	fi

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
			when 4 then p.value :=p.value iand 0xFFFF'FFFF
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
!	if y.xvalue=0 then serror("div 0.0") fi
!CPL "ULLONG TO FLOAT"
!		if p.value>=0 then
!			p.xvalue:=p.uvalue
!		else
!			p.xvalue:=-p.value
!		fi
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
!CPL "CONVERT FROM REF",p.isstrconst
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

!elsif t>=tfirstint and t<=tlastint then
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
!CPL "DOT2"
			p^:=p.a^
		else
			terror("CHECKLV/WIDEN")
		esac

	when jdot then
!CPL "DOT3"

!when jptroffset then

	when jconst then
		if not ttisref[p.mode] then
			goto notlv
		fi
	when jconvert then
!	if p.a.tag=jname then
!!		p^:=p.a^
!		return
!	fi
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

!CPL "CALL:"
!PRINTUNIT(NIL,P)

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
!CPL =STRMODE(MPROC), =STRMODE(TTBASETYPE[MPROC]),=STRMODE(TPROC)
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
!		r:=createunit1(jptr,p)
!
!		r.mode:=tttarget[d.mode]
!		R.MODE:=P.MODE
!		p:=r

			goto doptr
		fi
!when jdot,jcallfn,jifx then
	when jdot,jcallfn,jifx,jconvert,jexprlist then
!CPL "DOT4"
		r:=createunit1(jptr,p)
		r.mode:=tttarget[p.mode]
		p:=r
		goto doptr

!when jcallfn then
!	r:=createunit1(jptr,p)
!	r.mode:=tttarget[p.mode]
!	p:=r
!	goto doptr

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
!CPL "DOT5"
			offset:=p.offset
			p.tag:=jaddptr
			p.ptrscale:=1	!ttsize[elemmode]
!		p.a.mode:=refmode
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
!CHECKTYPE(D)
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

!CPL "CPT1",STRMODE(S), STRMODE(T)
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

!CPL "CPT2",STRMODE(SBASE), STRMODE(TBASE)
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
!CPL "ARRAY TARGS",STRMODE(STARGET), STRMODE(TTARGET),STARGET, TTARGET
 
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
!CPL "CREATED ENUM NAME",D.NAME,"IN",OWNER.NAME,NAMENAMES[OWNER.NAMEID],CURRPROC,STMODULE
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
!		if lx.symbol=rcurlysym then			!this is allowed
!			serror("enum?")
!		fi
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

!CPL "DOT6"
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
!CPL "DOT7"

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

!CPL "DOCAST",STRMODE(S), STRMODE(T)

	retry:

!if t=tvoid then return p fi


	if s=t then return p fi
	opc:=0

	if s<16 and t<16 then
!CPL "DOCAST"
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

!CPL "DOCAST",STRMODE(T)

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

	t:=p.mode
	switch p.tag
!when jnameaddr then
!	size:=ttsize[p.def.mode]			!take account of array
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
		fi

	when jwidenmem then
		return createsizeofop(p.a)

	else
	cad1:
		size:=ttsize[t]
	end switch

!q:=createconstunit(size,ti32)
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
!IF COUNT=1 THEN
!CPL "MATCH",STRMODE(M)
!FI

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
=== cc_genpcl.m 0 0 28/38 ===

global int retindex
global int initstaticsindex

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit
symbol dcmdskip

!global const maxblocktemps=50
!global [maxblocktemps]symbol blockdefs
!global int nblocktemps
!global symbol blockretname

int nvarlocals, nvarparams

macro divider = gencomment("------------------------")

!PROC DO_STMT(UNIT X)=
!GENCOMMENT("<BLOCK>")
!END


global proc codegen_pcl=
!generate code for module n
	symbol d,e
	ref procrec pp

!CPL "//CODE GEN PCL"
	if fverbose=3 then println "GenPCL:",inputfile fi

	pcl_start(nunits)

	dolibs()

!	stmodule:=moduletable[n].stmodule

!do two passes: module decls first, then procs
	gencomment("1:Start of code")

	d:=stmodule.deflist
	while d do
		case d.nameid
		when staticid then
			dostaticvar(d)
		when procid then
!CPL "SCANPROC",D.NAME,SCOPENAMES[D.SCOPE],=D.CODE
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
!CPL "DONE CODE GEN"

end

proc genprocdef (symbol p) =
	symbol d
	psymbol e
	int ismain:=0

	if eqstring(p.name,"main") then
		ismain:=1
		p.ismain:=1
		if p.paramlist.nparams=2 then
			ismain:=2
			docmdskip()
		fi

	fi
!
!CPL "PCL GENPROC",P.NAME

	currproc:=p

!	pc_gen(kdefproc, genmem_d(p))
!	setmode(p.mode)

	pc_defproc(getpsymbol(p))

	d:=p.deflist
	while d, d:=d.nextdef do
		case d.nameid
		when paramid then
			pc_addparam(getpsymbol(d))

		when frameid then
			pc_addlocal(getpsymbol(d))

		esac
	od
!
	if ismain=2 then
		fixmain(p)
	fi

	retindex:=createfwdlabel()

	divider()
!GERROR("GENPCL: DO_STMT MISSING")

	do_stmt(p.code)

	divider()

	definefwdlabel(retindex)

	if ismain then
		pc_gen(ksetcall)
		pc_gen(kload, genint(0))
		setmode(ti32)
		pc_genx(ksetarg, 1)

		e:=pc_makesymbol("exit", import_id)
		e.imported:=1
!		addnamesym(d)

!		pc_gen(kcallp, genname("exitxx*"))
		pc_gen(kcallp, genmemaddr(e))
		pccurr.nargs:=1

	fi

	pc_gen((p.mode<>tvoid|kretfn|kretproc))		!mcl checks stack is not empty for retfn
	setmode(p.mode)

!	pc_gen(kendproc)
	pc_endproc()
!
	gencomment("")
end

proc dostaticvar(symbol d)=
	[256]char str

	int align

	return when d.scope=imported_scope

	align:=getalignment(d.mode)

	if d.code then
!CPL "STATIC",D.NAME, NAMENAMES[D.NAMEID]
		if d.nameid=frameid then			!const init data for local var
			fprint @str,"$#.#.#",d.owner.name,d.name,d.blockno
			pc_gen(kistatic, genmem_d(addnamestr(str)))
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
	real32 sx
	[256]char str
	[16]char str2

	t:=p.mode
	a:=p.a
	b:=p.b

!CPL "GENIDATA",JTAGNAMES[P.TAG],STRMODE(P.MODE)
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
!	if t>=tfirstint and t<=tlastreal then

		if isintcc(t) or isrealcc(t) then
			if t=tr32 then
				sx:=p.xvalue
!				pc_gen(kdd,u32@(sx))
				pc_gen(kdata,genint(ref u32(&sx)^))
!				pc_gen(kdd,u32(sx))
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
!CPL "IDATA/STRCONST",P.SLENGTH
				pc_gen(kdata, genstring(p.svalue))

			elsif p.iswstrconst then
GERROR("GENIDATA/WSTRING2")
!				genmc(m_dq, genwstrimm(p.wsvalue,p.wslength))
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

!!			goto doref
!			goto doarray
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

proc fixmain(symbol p)=
	symbol dnargs, dargs
	symbol denv
	symbol dinfo
	psymbol pp:=p.pdef, pgetargs

	dnargs:=p.deflist
	dargs:=dnargs.nextdef

	dnargs.nameid:=frameid
	dargs.nameid:=frameid

	denv:=createdupldef(p,addnamestr("$env"),frameid)
	denv.mode:=createrefmode(trefchar)

	dinfo:=createdupldef(p,addnamestr("$info"),frameid)
	dinfo.mode:=createarraymode(tu8,128)

	dnargs.used:=1
	dargs.used:=1
	dinfo.used:=1
	denv.used:=1

!generate call to __getmainargs
	gencomment("Implement main(n,x)")

	pc_gen(ksetcall)
	pc_gen(kload, genmemaddr_d(dinfo))
	setmode(dinfo.mode)
	pc_genx(ksetarg, 5)

	pc_gen(kload, genint(0))
	setmode(ti32)
	pc_genx(ksetarg, 4)

	pc_gen(kload, genmemaddr_d(denv))
	setmode(tu64)
	setmode(denv.mode)
	pc_genx(ksetarg, 3)

	pc_gen(kload, genmemaddr_d(dargs))
	setmode(tu64)
	pc_genx(ksetarg, 2)

	pc_gen(kload, genmemaddr_d(dnargs))
	setmode(tu64)
	pc_genx(ksetarg, 1)

	pgetargs:=pc_makesymbol("__getmainargs", import_id)
	pgetargs.imported:=1

!	pc_gen(kcallp, genmemaddr_d(addnamestr("__getmainargs*")))
	pc_gen(kcallp, genmemaddr(pgetargs))
	pccurr.nargs:=5

	pc_addlocal(dinfo.pdef)
	pc_addlocal(denv.pdef)


	pc_gen(kload, genmem_d(dargs))
	setmode(ti64)
	pc_gen(kload, genmem_d(dcmdskip))
	setmode(ti64)
	pc_gen(kaddpx)
	pc_setscaleoff(8)	
	pc_gen(kstore, genmem_d(dargs))
	setmode(ti64)

	pc_gen(kload, genmem_d(dnargs))
	setmode(ti32)
	pc_gen(kload, genmem_d(dcmdskip))
	setmode(ti32)
	pc_gen(ksub)
	setmode(ti32)
	pc_gen(kstore, genmem_d(dnargs))
	setmode(ti32)

	divider()
end

proc docmdskip=
	dcmdskip:=createdupldef(stmodule, addnamestr("$cmdskip"), staticid)
	dcmdskip.scope:=exported_scope
	dcmdskip.mode:=ti64

	dostaticvar(dcmdskip)
end

proc dolibs=
	for i to nlibfiles do
		pc_addplib(libfiles[i])
	od
end

=== cc_blockpcl.m 0 0 29/38 ===
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
	unit a,b
	symbol d

	return unless p

	oldclineno:=clineno
	clineno:=p.lineno
	cfileno:=p.fileno
	mmpos:=cfileno<<24+clineno

!CPL "DOSTMT",JTAGNAMES[P.TAG],currproc.name

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
		dx_call(p,a,b,0)

	when jreturn then
		do_return(a)

	when jassign then
		do_assign(a,b,0)

	when jif, jifx then
		do_if(a,b,p.c)

	when jfor then
		do_for(a,b)

	when jwhile then
		do_while(a,b)

	when jdowhile then
		do_dowhile(a,b)

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
		do_switch(p,a,b)

	when jaddto then
		dx_binto(a,b, kaddto)

	when jsubto then
		dx_binto(a,b, ksubto)

	when jmulto then
		dx_binto(a,b, kmulto)

	when jdivto then
		dx_binto(a,b, (isrealcc(a.mode)|kdivto|kidivto))

	when jremto then
		dx_binto(a,b, kiremto)

	when jiandto then
		dx_binto(a,b, kbitandto)

	when jiorto then
		dx_binto(a,b, kbitorto)

	when jixorto then
		dx_binto(a,b, kbitxorto)

	when jshlto then
		dx_binto(a,b, kshlto)

	when jshrto then
		dx_binto(a,b, kshrto)

	when jpreincr, jpostincr then
		do_preincr(a, kincrto)

	when jpredecr, jpostdecr then
		do_preincr(a, kdecrto)

	when jexprlist then
!CPL "DO/EXPRLIST"
		while a do
			do_stmt(a)
			a:=a.nextunit
		od

	else
!!assume standalone expression (assign/call/addto/incr done above)
		dx_expr(p)
		pc_gen(keval)
		setmode_u(a)

	end switch
end

proc dx_expr(unit p, int am=0) =
	int oldclineno,value,m
	unit a,b
	[256]char str
	symbol d

	return unless p

!CPL "DXEXPR",JTAGNAMES[P.TAG], currproc.name

	oldclineno:=clineno
	clineno:=p.lineno
	cfileno:=p.fileno

	a:=p.a
	b:=p.b
	m:=p.mode

	switch p.tag
	when jconst then
!GENCOMMENT("EXPR")
		dx_const(p)

	when jname then
		dx_name(p,am)
!
	when jwidenmem then
		dx_expr(a,am)
!		dx_widen(a,m)

	when jfuncname then
		pc_gen(kload, genmemaddr_d(p.def))

	when jassign then
		do_assign(a,b, 1)
!!
	when jandl,jorl then
		dx_andorl(p)		!use non-short circuit versions for now

	when jnotl then
		if a.tag=jnotl then
			dx_expr(a.a)
!			pc_gen(knotnotl)
			pc_gen(ktoboolt)
			setmode_u(a.a)
		else
			dx_expr(a)
			pc_gen(knot)
			setmode_u(a)
		fi

	when jistruel then
		dx_expr(a)
		pc_gen(getopc(ktoboolt,a))
		setmode_u(a)

	when jexprlist then
!CPL "DX/EXPRLIST"
		while a, a:=b do
			b:=a.nextunit

			if b and a.tag in [jassign, jconvert, jifx] then
!			if b and a.tag in [jassign] then
!CPL "DX/EXPRLIST/ASSIGN"
				do_stmt(a)
			else
				dx_expr(a)
				if b and (a.mode<>tvoid or a.tag=jconvert) then
					pc_gen(keval)
				fi
			fi
		od

	when jcallfn then
		dx_call(p,a,b, 1)

	when jifx then
		dx_ifx(p,a,b,p.c)

	when jeq,jne,jlt,jle,jge,jgt then
		dx_eq(p, a,b)

	when jadd then
		if ttisref[a.mode] and ttsize[b.mode]<=4 then
			b.mode:=tu64
		fi
		dx_bin(a,b, kadd)
!
	when jsub then
		dx_bin(a,b, ksub)
!		dx_sub(a,b)
!
	when jmul then
		dx_bin(a,b, kmul)

	when jdiv then
		dx_bin(a,b, (isrealcc(a.mode)|kdiv|kidiv))

	when jrem then
		dx_bin(a,b, kirem)

	when jiand then
		dx_bin(a,b, kbitand)

	when jior then
		dx_bin(a,b, kbitor)

	when jixor then
		dx_bin(a,b, kbitxor)

	when jshl then
		dx_bin(a,b, kshl)

	when jshr then
		dx_bin(a,b, kshr)

	when jptr then
		dx_ptr(p,a,am)

	when  jaddptr then
		dx_addptr(p,a,b, kaddpx, am)
!
	when  jsubptr then
		dx_addptr(p, a,b, ksubpx, am)
!
	when jconvert then
		if p.convmode=tvoid then
			dx_expr(a)
		else
			dx_convert(p,a,p.convmode, p.opcode)
		fi

	when jscale then
		dx_scale(p,a,b)

	when jneg then
		dx_expr(a)
		pc_gen(getopc(kneg,a))
		setmode_u(a)

	when jinot then
		dx_expr(a)
		pc_gen(kbitnot)
		setmode_u(a)

	when jpreincr, jpredecr then
		dx_preincrx(p,a)

	when jpostincr, jpostdecr then
		dx_postincrx(p,a)

	when jaddto then
		dx_binto(a,b, kaddto, 1)

	when jsubto then
		dx_binto(a,b, ksubto, 1)

	when jmulto then
		dx_binto(a,b, kmulto, 1)

	when jdivto then
		dx_binto(a,b, kdivto, 1)

	when jremto then
		dx_binto(a,b, kiremto, 1)

	when jiandto then
		dx_binto(a,b, kbitandto, 1)

	when jiorto then
		dx_binto(a,b, kbitorto, 1)

	when jixorto then
		dx_binto(a,b, kbitxorto, 1)

	when jshlto then
		dx_binto(a,b, kshlto, 1)

	when jshrto then
		dx_binto(a,b, kshrto, 1)

	when jaddrof then
		dx_addrof(p,a,am)

	when jdot then
		dx_dot(p,a,b,am)

	when jsetjmp then
		dx_expr(a)
		pc_gen(ksetjmp)

	when jlongjmp then
		dx_expr(a)
		dx_expr(b)
		pc_gen(klongjmp)

	else
		gerror_s("DX-EXPR: can't do tag: #",jtagnames[p.tag])
	end switch

	clineno:=oldclineno
end

proc dx_const(unit p)=
	int t:=ttbasetype[p.mode]


	if t in tfirstint..tlastint then
		pc_gen(kload, genint(p.value))

	elsecase t
	when tr32 then
		pc_gen(kload, genreal(p.xvalue,tpr32))

	when tr64 then
		pc_gen(kload, genreal(p.xvalue, tpr64))

	elsif t>=tfirstreal and t<=tlastreal then
		pc_gen(kload, genreal(p.xvalue,tpr64))

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
!GENCOMMENT("DXNAME")
			pc_gen(getopc(kload,p), genmem_d(d))
			widen(p)
!			setmode(getmemmode(p))
		fi
	else
		gerror("dxname")
	esac
end

proc dx_bin(unit a, b, int opc)=
	dx_expr(a)
	dx_expr(b)

	pc_gen(getopc(opc,a))
	setmode(a.mode)
end

proc dx_binto(unit a, b, int opc, res=0)=
!res=1 means value must be retained

	dx_expr(b)
	if res then pc_gen(kdupl) fi
	dx_expr(a,1)
	pc_gen(getopc(opc,a))
	setmode(getmemmode(a))
end

proc do_assign(unit a,b, int res)=
	dx_expr(b)

	if res then
		pc_gen(kdupl)
	fi

	case a.tag
	when jname then
		pc_gen(getopc(kstore,a), genmem_d(a.def))
		setmode(getmemmode(a))

	when jptr then
		dx_expr(a,1)
		pc_gen(getopc(kistore, a))
		setmode(getmemmode(a))

	when jdot then
		dx_expr(a.a,1)
		pc_gen(kload, genint(a.offset))
		pc_gen(kaddpx)
		pc_setscaleoff(1)
		setmode(getmemmode(a))

		pc_gen(getopc(kistore, a))
		pc_setscaleoff(1)
		setmode(getmemmode(a))

	else
		GERROR_S("DOASSIGN not ready: #",jtagnames[a.tag])
	esac
end

proc dx_ptr(unit p, a, int am)=
	dx_expr(a)
	if am=0 then				!for &, exit with pointer value
		pc_gen(getopc(kiload, p))
		widen(p)
!		setmode(getmemmode(p))
	fi
end

proc dx_addptr(unit p, a,b, int opc, am)=
	dx_expr(a)
	dx_expr(b)
	pc_gen(opc)
!CPL "ADDPTR",STRMODE(A.MODE),=P.PTRSCALE
!CPL "ADDPTR",STRMODE(TTTARGET[A.MODE])

!	pcl_setscale(ttsize[tttarget[a.mode]])
	pc_setscaleoff(p.ptrscale)

!	setmode(a.mode)
end

proc dx_addrof(unit p, a, int am)=
	dx_expr(a,1)
end

proc dx_convert(unit p,a, int t,opc)=
!convert unit a to type t, using conversion opc (uwiden_c etc)
	int s,ssize,tsize

	s:=a.mode

	ssize:=ttsize[s]
	tsize:=ttsize[t]

!CPL "CONVERT",STRMODE(S), STRMODE(T)
!GERROR("CONVERT ERROR TEST")

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
!IF TSIZE=2 THEN GERROR("WIDEN1") FI
			pc_gen(kwiden)
		elsif tsize<ssize then
!			recase narrow_c
			goto dotruncate
!CPL "NARROW HARDC"
			return
		fi

	when swiden_c, uwiden_c then
		if ssize=tsize then return fi
!IF TSIZE=2 THEN GERROR("WIDEN2") FI
		pc_gen(kwiden)

	when sfloat_c,ufloat_c then
		pc_gen(kfloat)

	when sfix_c,ufix_c then
		pc_gen(kfix)

	when fwiden_c then
		pc_gen(kfwiden)
		return

	when fnarrow_c then
		pc_gen(kfnarrow)
		return

	when narrow_c,truncate_c then
dotruncate:
		pc_gen(ktruncate)

		setmode(ti32)
		setmode2(t)
		return

	else
		gerror_s("Convert op not implem: #",convnames[opc])
	esac

	setmode(t)
	setmode2(s)
!CPL "TRUNC",=STRMODE(PCCURR.MODE.MODE)
!CPL "TRUNC",=STRMODE()
end

proc do_if(unit a,b,c)=
	int lab1,lab2

	lab1:=createfwdlabel()


	genjumpcond(kjumpf,a,lab1)

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

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q,r
	int lab2

	q:=p.a
	r:=p.b

	switch p.tag
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

	when jnotl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt,q,lab)
		when kjumpt then
			genjumpcond(kjumpf,q,lab)
		esac

	when jistruel then
		genjumpcond(opc,q,lab)

	when jeq,jne,jlt,jle,jge,jgt then

		gcomparejump(opc,p,q,r,lab)

	when jexprlist then
!CPL "COND/EXPRLIST"
		while q and (r:=q.nextunit) do
!			dx_expr(q)
			do_stmt(q)
			q:=r
		od

		genjumpcond(opc,q,lab)
	else			!other expression
		dx_expr(p)
		pc_gen(getopc(opc,p), genlabel(lab))
		setmode_u(p)
	end switch
end

proc gcomparejump(int jumpopc, unit p, lhs,rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int cond

	cond:=getpclcond(p.tag)			!jeq => keq etc
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	dx_expr(lhs)
	dx_expr(rhs)

	pc_gen(getopc(kjumpcc,lhs), genlabel(lab))
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
	int lab_b,lab_c,lab_d

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

	genjumpcond(kjumpt,pcond,lab_b)
	setmode_u(pcond)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_while1 (unit pbody) =
	int lab_b,lab_c,lab_d

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
	int lab_b,lab_c,lab_d

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	genjumpcond(kjumpt,pcond,lab_b)
	setmode_u(pcond)
	definefwdlabel(lab_d)
	--loopindex
end

proc stacklooplabels(int a,b)=
	!don't check for loop depth as that has been done during parsing
	continuestack[++loopindex]:=a
	breakstack[loopindex]:=b
end

proc do_return(unit a)=

	if a then
		if currproc.ismain then
			pc_gen(ksetcall)
			dx_expr(a)
			pc_genx(ksetarg, 1)
			pc_gen(kcallp, genname("exit*"))
			pccurr.nargs:=1
		else
			dx_expr(a)
			pc_gen(kjumpret, genlabel(retindex))
			setmode_u(a)
		fi
	else
		genjumpl(retindex)
	fi
end

proc dx_call(unit p,a,b, int res)=
	ref paramrec pm
	int isfnptr,variadic,nparams,retmode,nbytes,retsize,m,nvariadics
	int nfixedparams, isfn, blockret
	[maxparams]unit paramlist
	[maxparams]byte paramconst			!1 when 'const' (up to nfixedparams only)
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

		isfn:=tttarget[m]<>tvoid and res
		pm:=ttparams[m]
		isfnptr:=1

	else
		pm:=a.def.paramlist
		isfnptr:=0
		isfn:=a.def.mode<>tvoid and res

	esac

	variadic:=pm.flags=pm_variadic
	nfixedparams:=pm.nparams			!will include any extra block return
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
	od

!CPL =ISFN
!CPL =ISFNPTR


	if not isfnptr then
		pc_gen((isfn|kcallf|kcallp), genmemaddr_d(a.def))
	else
		dx_expr(a.a)
		pc_gen((isfn|kicallf|kicallp))
	fi

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
!CPL "CALL MODE",STRMODE(P.mode)
!CPL "CALL MODE",STRMODE(getmemmode(P))

!		setmode(p.mode)
		setmode(getmemmode(p))
	fi

	if isfn and not res then
		pc_gen(keval)
	fi
end

proc do_decl(symbol d)=
	unit a
	[256]char str

	a:=d.code
	d.used:=1

	if a.tag<>jmakelist then
		if ttbasetype[d.mode]=tarray and a.tag=jconst then	!probably string lit
			goto copyl
		fi
		dx_expr(a)
		pc_gen(getopc(kstore,a), genmem_d(d))
		setmode(a.mode)
		return
	fi

copyl:

GENCOMMENT("INIT DECL ARRAY")
!	pc_gen(ksetcall)
!
!	pc_gen(kload, ttsize[d.mode])
!	pc_gen_x(ksetarg, 3)
!
!	fprint @&.str,"`$#.#.#",currproc.name,d.name,d.blockno
!	pc_gen(kload, addnamestr(str))
!	pc_gen_x(ksetarg, 2)
!
!	pc_gen(kload, d)
!	pc_gen_x(ksetarg, 1)
!
!	pc_gen_name(kcallp, "memcpy*")
!	pccurr.nargs:=3
!!
	fprint @&.str,"$#.#.#",currproc.name,d.name,d.blockno
	pc_gen(kload, genmem_d(addnamestr(str)))

!	setsmode(d.mode)
	setmode(d.mode)
	pc_gen(kstore, genmem_d(d))
	setmode(d.mode)


end

proc do_for (unit pinit, pbody) =
	unit pcond,pincr
	int lab_b,lab_c,lab_d,lab_cond

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
		genjumpcond(kjumpt,pcond,lab_b)
	else
		genjumpl(lab_b)
	fi
	definefwdlabel(lab_d)
	--loopindex
end

proc do_preincr(unit a,int incrop)=
	dx_expr(a,1)
	pc_gen(incrop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pc_setincr(1)

	if ttisref[m] then
		pc_setincr(ttsize[tttarget[m]])
	fi
end

proc dx_preincrx(unit p,a)=
	dx_expr(a,1)

	pc_gen((p.tag=jpreincr|kincrload|kdecrload))
	setmode(getmemmode(a))
!	setmode(a.mode)
	setincrstep(a.mode)
end

proc dx_postincrx(unit p,a)=
	dx_expr(a,1)

	pc_gen((p.tag=jpostincr|kloadincr|kloaddecr))
	setmode(getmemmode(a))
!	setmode(a.mode)
	setincrstep(a.mode)
end

proc dx_dot(unit p,a,b, int am)=
	dx_expr(a,1)
	pc_gen(kload, genint(p.offset))
	setmode(tu32)

	if am=0 then
		pc_gen(kaddpx)
		setmode(getmemmode(p))
		pc_setscaleoff(1)

		pc_gen(getopc(kiload,p))
		widen(p)
!		setmode(getmemmode(p))
	else
		pc_gen(kaddpx)
		pc_setscaleoff(1)
	fi

end

proc dx_eq(unit p, a,b)=
!apply =, <= etc between a and b, and get a logical result 1 or 0

	dx_expr(a)
	dx_expr(b)

	pc_gen(getopc(ksetcc,a))
	pccurr.condcode:=getpclcond(p.tag)
	setmode_u(a)
end

proc do_labeldef(symbol d)=
	gencomment(d.name)
	pc_gen(klabel, genlabel(d.index))
end

proc do_goto(symbol d)=
	if d.index=0 then
		gerror_s("Label not defined: #",d.name)
	fi
	pc_gen(kjump, genlabel(d.index))
end

proc dx_ifx(unit p,a,b,c)=
	int lab1, lab2, ismult:=p.mode<>tvoid

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
	lab2:=createfwdlabel()

	if ismult then pc_gen(kstartmx) fi
	genjumpcond(kjumpf,a,lab1)

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

proc do_switch(unit p,a,b)=
!need to create switch levels, as they can be nested; nested case labels
!belong to the top switch level
	[maxswitchrange]i32 labeltable				!sw_length+1 labels
	[maxcases]i32 valuetable					!sw_length+1 labels
	[maxswitchrange]byte flags					!flags to check dupl values
	int defaultlabel							!index of fwd default label
	int breakswlabel							!index of fwd break label
	int switchlabel								!index of fwd break label
	int lower, upper							!ower/upper ranges of switch case values
	int length,value,ncases
	byte serialsw
	int i,index
!int sw_index
	ref caserec pcase

!store current set of global values for outer switch
	ref[]i32 old_labeltable
	ref[]i32 old_valuetable
	int old_ncases,old_lower
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
			lower:=min(lower,value)
			upper:=max(upper,value)
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

	if length>maxswitchrange then

!NOTES: SERIAL switch needs a way of checking duplicate case values.
!Better if not an n-squared search
!Short length switches should also be done serially (length<=8)
!Then a dupl check is simpler

		serialsw:=1

!		ax:=loadexpr(a)
		dx_expr(a)

		for i:=1 to ncases do
			labeltable[i]:=createfwdlabel()
			if i<ncases then
				pc_gen(kdupl)
			fi
			pc_gen(kload, genint(valuetable[i]))
			pc_gen(kjumpcc, genlabel(labeltable[i]))
			pccurr.condcode:=eq_cc
		od

		genjumpl(defaultlabel)

	elsif length=0 then
!GERROR("L=0")
		genjumpl(defaultlabel)

	else
		serialsw:=0
		memset(&flags,0,length)				!clear value flags

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
				gerror_s("Dupl case value: #",strint(value))
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
	int lab1,lab2

	lab1:=createfwdlabel()			!dest label of main condition (to end of if, or start if else)

	pc_gen(kstartmx)
	genjumpcond(kjumpf,p,lab1)

	lab2:=createfwdlabel()			!label past else part
	pc_gen(kload, genint(1))
	setmode(ti32)
	pc_gen(kresetmx)
	genjumpl(lab2)

	definefwdlabel(lab1)
	pc_gen(kload, genint(0))
	setmode(ti32)
	pc_gen(kendmx)

	definefwdlabel(lab2)
end

proc dx_scale(unit p,a,b)=
	int opc,scale:=p.scale,n

	dx_expr(a)
	if p.scale>=0 then
		pc_gen(kload, genint(p.scale))
		pc_gen(kmul)
	else
		pc_gen(kload, genint(-p.scale))
		pc_gen(kdiv)
	fi
	setmode_u(a)
end

func getopc(int opc, unit p)int=
	opc
end

proc widen(unit p) =
	int mode:=getmemmode(p)

	setmode(mode)

	if ttsize[mode]<4 and pccurr.opcode in [kload, kiload, kiloadx] then
		pc_gen(kwiden)
		setmode((mode in [ti8, ti16]|ti32|tu32))
		setmode2(mode)
	fi
end
=== cc_libpcl.m 0 0 30/38 ===
global function getpsymbol(symbol d)psymbol p=
	symbol e
	ichar name
	[256]char str


	return nil when d=nil

	if d.pdef then return d.pdef fi

	name:=d.name

	if d.blockno>1 then
		strcpy(str, name)
		strcat(str, ".")
		strcat(str, strint(d.blockno))
		name:=str
	fi

	d.pdef:=p:=pc_makesymbol(name, name2pid[d.nameid])

	p.symbol:=namesym
	p.mode:=getpclmode(d.mode)
	p.size:=ttsize[d.mode]

	if d.owner and d.owner.owner then
		p.owner:=getpsymbol(d.owner)
	fi

	if d.scope=exported_scope then p.exported:=1 fi
	if d.scope=imported_scope then p.imported:=1 fi
	p.used:=d.used
	p.labelno:=d.index

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

=== cc_lib.m 0 0 31/38 ===
global int autotypeno=0
global int nextafindex=0

!strbuffer exprstrvar
!ref strbuffer exprstr=&exprstrvar

const int unitheapsize=50000
ref unitrec unitheapptr=nil
int remainingunits=0

function newstrec:ref strec=
	ref strec p
	p:=pcm_alloc(strec.bytes)
	memset(p,0,strec.bytes)
!clear p^

	p.lineno:=lx.lineno
	p.fileno:=lx.fileno

!	p.moduleno:=currmoduleno
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

global function createconstunit(word64 a, int t)ref unitrec=
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

global function createwstringconstunit(ref word16 s, int length)ref unitrec=
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
	static [0:jtagnames.len]int16 opctotable
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

global function getconstvalue(ref unitrec p,int ID=0)int64=	!GETCONSTVALUE
!extract value from kconst
	if p and p.tag=jconst then
		return p.value
	fi
	serror("GCV Not constant")
	return 0
end

global function nextautotype:ichar=
	static [32]char str

!sprintf(&.str,"$T%d",int32(++autotypeno))
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

!CPL "CREATEREFTO",STRMODE(M)

	if ttreftype[m] then
!CPL "REF TO ALREADY EXISTS",NTYPES
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
!CPL "CREATEARRAYOF", STRMODE(M),LENGTH

!IF NTYPES>10000 THEN CPL =NTYPES FI

!	for i to ntypes do
!		if ttbasetype[i]=tarray and tttarget[i]=m and ttlength[i]=length then
!CPL "ALREADY EXISTS",I
!			++ttshared[i]
!			return i
!		fi
!	od
!

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
!	ttcat[newm]:=d32cat

	return newm
end

global function createstructmode(ref strec s,int smode)int=
	int newm
	newm:=createnewmode(smode)
	ttnamedef[newm]:=s
	ttisblock[newm]:=1

!	ttcat[newm]:=blockcat

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

!sprintf(&.str,"$F%d",int32(++nextafindex))
	print @&.str,"$F",,++nextafindex

	name:=pcm_copyheapstring(&.str)
	return addnamestr(name)
end

global func convertstring(ichar s, t,int length=-1)int=
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
!			sprintf(&.str,"\\%03o",int32(c))
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
!		sprintf(dest,"[%d]",int32(ttlength[m]))
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
!	strcpy(dest,"proc[PM](")
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
	ref int64 q
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

!CPL "CNM",STRMODE(M),M

	if ntypes>=maxtype then
	CPL =STRMODE(M)
		serror("Too many types/cnm")
	fi
	++ntypes

!leave length, const etc all zero
!copy basic size info from basetype

	ttbasetype[ntypes]:=m
	ttsize[ntypes]:=ttsize[m]
!	ttcat[ntypes]:=ttcat[m]

!!CPL "NEW TYPE",STRMODE(NTYPES)
!CPL "NEW TYPE",NTYPES

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
!	serror(symbolnames[symbol]+" expected, not "+symbolnames[lx.symbol])
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

!trefchar:=createrefmode(tu8)
!trefwchar:=createrefmode(tu16)

	trefchar:=createrefmode(ti8)

!trefwchar:=createrefmode(ti16)
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

!CPL "RESOLVENAME",SYMPTR.NAME

	if ns=ns_labels then
		return resolvelabel(owner,symptr)
	fi

	if blockno and blockcounts[blockno]=0 then blockno:=blockowner[blockno] fi

!	INT NLOOPS:=0

	do							!loop for each block level
!++NLOOPS
		nsblock:=ns<<16 ior blockno
		d:=symptr				!reset dupl list
		while d:=d.nextdupl do
!++NLOOPS
			if owner.nameid=procid  and d.owner<>owner and d.owner.nameid=procid then
				exit
			fi
			if d.owner=owner and d.nsblock=nsblock then
!CPL "FOUND",D.NAME, NLOOPS
				d.used:=1

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

global function isstructunion(int m)int=
	case ttbasetype[m]
	when tstruct,tunion then
		case ttsize[m]
		when 1,2,4,8 then
		else
		 return 1
		esac
	esac
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
!return tfirstint<=m and m<=tlastint
end

global function ispoweroftwo(int64 x)int=
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

=== cc_support.m 0 0 32/38 ===
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

!	sprintf(&.str,mess,a)
	fprint @str, mess, a
	terror_gen(&.str)
end

global proc terror_ss(ichar mess,a,b)=
	[256]char str

!	sprintf(&.str,mess,a,b)
	fprint @str, mess, a, b
	terror_gen(&.str)
end

global proc gerror_gen(ichar mess,ref unitrec p=nil)=
	int lineno,fileno

	if p then
!CPL "GERROR/P GIVEN"
		lineno:=p.lineno
		fileno:=p.fileno
	else
		lineno:=clineno
		fileno:=cfileno
	fi

	if currproc then
		print "In function",currproc.name,," "
	fi

!CPL =LINENO,CLINENO, P.LINENO,P.FILENO
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

!	sprintf(&.str,mess,s)
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

!	sprintf(&.str,mess,mess2)
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

!if flinesplicing then
!	s:=splicelines(s)
!fi

!CPL "SETFILETEXT2",=NSOURCEFILES,REF VOID S,=RFSIZE
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

!CPL "LBIN"

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles
	sourcefilepaths[nsourcefiles]:="<builtin>"
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

!CPL "LOADBUILTIN", HDRTEXT


!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
	sourcefiletext[nsourcefiles]:=pcm_copyheapstring(hdrtext)
!CPL "SETFILETEXT3",=NSOURCEFILES,REF VOID SOURCEFILETEXT[NSOURCEFILES]

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
=== cc_headersx.m 0 0 33/38 ===
!Built-in standard headers

global int builtinheaders=0

global function findheader(ichar name)ichar=
return nil
end

global proc writeheaders=
filehandle f
ichar ifile
int i
end

global function isheaderfile(ichar file)int=
return 0
end
=== cc_export.m 0 0 34/38 ===
strbuffer mmbuffer
ref strbuffer mm=&mmbuffer

global proc writemheader(ichar infile)=
	[300]char mfile
	ref strec d,e
	int m
	ref mparamrec q

	strcpy(mfile,pcm_copyheapstring(changeext(infile,(fmheaders='M'|".m"|".q"))))

	gs_init(mm)

	mmstr("importdll $")
	mmstr(extractbasefile(infile))
	mmstrln(" =")

	d:=stmodule.deflist

	while d do
		if isheaderfile(sourcefilenames[d.lineno>>24]) then
			d:=d.nextdef
			nextloop
		fi
		case d.nameid
		when staticid then
			mmstr("    ")
			mmmode(d.mode)
			mmstr(" ")
			mmstr(fixname(d.name))
			if d.code then
				mmstr(" =")
				mmstr(strexpr(d.code).strptr)
			fi
			mmline()

		when procid then
			writefunction(d)
		when typeid then
		when enumid then
			mmstr("    const ")
			mmleftstr(fixname(d.name),34)
			mmstr(" = ")
			mmint(d.index)
			mmline()

		when macroid then
			mmstr("macro ")
			mmstrln(fixname(d.name))
		when structtagid then
			writerecord(d.mode)
		esac
		d:=d.nextdef
	od
	mmstrln("end")

	for i:=0 to hstmask do
		e:=hashtable^[i]
		if e.name and e.symbol=namesym and e.nameid=macroid then
			if not isheaderfile(sourcefilenames[e.lineno>>24]) then
				if e.tokenlist then
					if e.tokenlist.nexttoken=nil and
						e.tokenlist.symbol in [intconstsym, realconstsym, namesym] then
						mmstr("global const ")
					else
						mmstr("global macro  ")
					fi
					mmstr(e.name)

					q:=e.mparamlist
					if q then
						mmstr("(")
						while q do
							mmstr(q.def.name)
							if q.nextmparam then
								mmstr(",")
							fi
							q:=q.nextmparam
						od

						mmstr(")")
					fi

					mmstr(" = ")
					showmacroseq(e.tokenlist)
					mmline()
				fi
			fi

		fi
	od

!	moduletable[1].mhdrstr:=mm.strptr

!	if logdest then
!		println @logdev,"M HEADERS\N========="
!		println @logdev,mm.strptr
!	fi

	println "Writing M Header:",mfile
	writefile(mfile,cast(mm.strptr),mm.length)

end

proc showmacroseq(ref tokenrec tk)=
	while tk do
		emittoken(tk,mm)
	tk:=tk.nexttoken
	od
end

proc mmstr(ichar s)=
	gs_str(mm,s)
end

proc mmleftstr(ichar s,int n)=
	gs_leftstr(mm,s,n)
end

proc mmstrln(ichar s)=
	gs_strln(mm,s)
end

proc mmint(int a)=
	[32]char str
	getstrint(a,str)
	gs_str(mm,str)
end

proc mmline()=
	gs_line(mm)
end

proc writefunction(ref strec d)=
	ichar file
	ref paramrec pm
	int n,isvar

	if d.mode=tvoid then
		mmstr("    proc ")
	else
		mmstr("    func ")
	fi
	mmstr("""")

	mmstr(d.name)

	mmstr("""")
	mmleftstr(" ",34-strlen(d.name))
	mmstr("(")

	pm:=d.paramlist
	n:=pm.nparams
	isvar:=pm.flags=pm_variadic
	for i to n do
		mmmode(pm.mode)
		if i<>n or isvar then
			mmstr(",")
		fi
		pm:=pm.nextparam
	od
	if isvar then
		mmstr("...")
	fi

	mmstr(")")

	if d.mode<>tvoid then
		mmmode(d.mode)
	fi

	mmline()
end

proc mmmode(int m,expand=1) =
	int t,u

	t:=ttbasetype[m]
	case t
	when tref then
		mmstr("ref ")
		u:=tttarget[m]
		if ttbasetype[u]=tproc then
			mmstr("void")
		else
			mmmode(tttarget[m])
		fi

	when tarray then
		mmstr("[")
		if ttlength[m] then
			mmint(ttlength[m])
		fi
		mmstr("]")
		mmmode(tttarget[m])

	when tenum then
		mmstr("int")

	when tstruct,tunion then
		mmstr(fixname(ttnamedef[m].name))

	when tproc then
		MMSTR("<PROC>")
	else
		mmstr(stdtypenames[t])
	esac
end

proc writerecord(int m, rectype='R', level=1)=
	ref strec d,e
	int emode

	to level do
		mmstr("    ")
	od
	++level

	d:=ttnamedef[m]
	if rectype='R' then
		if fmheaders='M' then
			mmstr("record ")
			mmstr(fixname(d.name))
			mmstrln(" = $caligned")
		else
			mmstr("type ")
			mmstr(fixname(d.name))
			mmstrln(" = struct $caligned")
		fi
	else
		mmstrln((rectype='S'|"struct"|"union"))
	fi

	e:=d.deflist
	if e=nil then
		to level do
			mmstr("    ")
		od
		mmstrln("int dummy    !empty record")
	fi

	while e do
		emode:=e.mode
		to level do
			mmstr("    ")
		od

		if strchr(e.name,'$') then
			case ttbasetype[emode]
			when tunion then
				writerecord(emode,'U',level)
			when tstruct then
				writerecord(emode,'S',level)
			esac
		else
			mmmode(e.mode)
			mmstr(" ")
			mmstrln(fixname(e.name))
		fi
		e:=e.nextdef
	od
	to level-1 do
		mmstr("    ")
	od
	mmstrln("end")
	mmline()
end

proc writefnptr(int m)=
	ref paramrec pm
	int isvar,n,target


	target:=tttarget[m]


	if target=tvoid then
		mmstr("clang proc(")
	else
		mmstr("clang func(")
	fi

	pm:=ttparams[m]
	n:=pm.nparams
	isvar:=pm.flags=pm_variadic
	for i to n do
		mmmode(pm.mode)
		if i<>n or isvar then
			mmstr(",")
		fi
		pm:=pm.nextparam
	od
	if isvar then
		mmstr("...")
	fi

	mmstr(")")

	if target<>tvoid then
		mmmode(target)
	fi
end

function fixname(ichar name)ichar=
	static []ichar reservedwords = (
		"function",
		"func",
		"read",
		"type",
		"next",
		"stop",
		"callback",
		"len",
		"$dummy"
	)
	[128]char str

	for i to reservedwords.len do
		if eqstring(reservedwords[i],name) then
			strcpy(str,name)
			strcat(str,"$")
			return pcm_copyheapstring(str)
		fi
	od

	return name
end
=== cc_show.m 0 0 35/38 ===
int currfileno
int currlineno

strbuffer sbuffer
global ref strbuffer dest=&sbuffer
int destlinestart

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar


global proc printcode(filehandle f,ichar caption)=
	int i
	ref strec p

!CPL "PRINTCODE",F

	println @f, caption

	p:=stmodule.deflist

	while p do
		case p.nameid
		when procid then
!		if p.scope<>imported_scope and p.code then
			if p.code then
				println @f,p.name,,"=",scopenames[p.scope]
				printunit(f,p.code,,"1")
				println @f
			fi
		esac
		p:=p.nextdef
	od
end

global proc printunit(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
!p is a tagrec
	ref unitrec q
	ref strec d
	int t,n,lincr
	ichar idname
	ref caserec pc

	if p=nil then
		return
	fi

	if p.tag>=jdummy then
		println "print unit: bad tag",p.tag
!	os_getch()
		stop 30
	fi

	if p.lineno then
		currlineno:=p.lineno
		currfileno:=p.fileno
	fi

	lincr:=1
	if level<0 then
		lincr:=-1
!	level:=-level
		print @dev,"             "
	fi

!PRINT @DEV,P:"10",," "

	print @dev,getprefix(abs(level),prefix,p)
	idname:=jtagnames[p.tag]
	if idname^='j' then ++idname fi

	print @dev,idname,,": "

	case p.tag
	when jname, jfuncname then
		d:=p.def

		print @dev,d.name,namenames[d.nameid]

		if d.code then
			print @dev," {",,jtagnames[d.code.tag],,"}"
		fi

		print @dev," ",,getdottedname(d)!,q

		if p.c then
			print @dev," Lastcall:",p.c
		fi

	when jtempdecl, jdecl, jgoto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

		println @dev
		printunit(dev,d.code,level+lincr,"1")
		return

	when jgoto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

	when jlabelstmt then
		print @dev,p.def.name!,"+ LABELED STATEMENT"

	when jcasestmt then
		print @dev,"Index:",p.index

	when jconst then
		t:=p.mode
		if t=trefchar then
			if not p.isstrconst then
				goto doref
			fi
	dostring:
			if p.slength>256 then
				print @dev,"""",,"(LONGSTR)",""" *",,p.slength
			else
				print @dev,"""",,p.svalue,,""" *",,p.slength
			fi
		elsif t=trefwchar then
			if not p.iswstrconst then
				goto doref
			fi
			print @dev,"""",,"(WSTRING)",""" *",,p.wslength
		elsif t>=ti8 and t<=ti64 then
			print @dev,p.value
		elsif t>=tu8 and t<=tu64 then
			print @dev,p.uvalue
		elsif isrealcc(t) then
			print @dev,p.xvalue
		elsif ttbasetype[t]=tref then
			if p.isstrconst then
				goto dostring
			fi
	doref:
			print @dev,ref void(p.value)
		elsif ttbasetype[t]=tarray then
			if p.isstrconst then
				goto dostring
			fi
			serror("PRINTUNIT/CONST/aRRAY")
		else
			cpl typename(t)
			serror("PRINTUNIT BAD CONST")
		fi
		print @dev," ",,strmode(t)
		if p.isstrconst then print @dev,"<STRCONST>" fi
		if p.iswstrconst then print @dev,"<WSTRCONST>" fi

	when jconvert then
		print @dev,convnames[p.opcode]
		print @dev," "
		print @dev,typename(p.a.mode)
		print @dev," => "
		print @dev,typename(p.convmode)

	when jscale then
		print @dev,"Scale:",p.scale

	when jaddptr,jsubptr then
		print @dev,"Ptrscale:",p.ptrscale

	when jswitch then
		pc:=p.nextcase
		n:=0
		while pc do ++n; pc:=pc.nextcase od

		print @dev,p.nextcase,n

	when jcallfn then
		print @dev," Aparams:",p.aparams

	when jptr then
!	if p.memtype then
!		print @dev," Memtype:",strmode(p.memtype)
!	fi

	when jdot then
		print @dev," Offset:",p.offset

	esac

	if p.memmode then
		print @dev, " Widen from:",strmode(p.memmode)
	fi

	if p.alength then print @dev," ALENGTH=",p.alength fi

	println @dev

	printunitlist(dev,p.a,level+lincr,"1")
	printunitlist(dev,p.b,level+lincr,"2")
	if p.tag<>jblock then					!.c is used to point to last element
		printunitlist(dev,p.c,level+lincr,"3")
	fi
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
	if p=nil then return fi

	while p do
		printunit(dev,p,level,prefix)
		p:=p.nextunit
	od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
	static [512]char str
	[512]char indentstr
	ichar modestr
	int length

	indentstr[1]:=0
	if level>10 then level:=10 fi

!strcpy(&.indentstr,"-----------------")
	strcpy(&.indentstr,"-----------------------")

	modestr:=strmode(p.mode,0)
	length:=strlen(modestr)
	if length<strlen(&.indentstr) then
		memcpy(&.indentstr,modestr,length)
	else
		strcpy(&.indentstr,modestr)
	fi

	to level do
		strcat(&.indentstr,"|---")
!	strcat(&.indentstr,"|------")
	od

	strcpy(&.str,getlineinfok())
	strcat(&.str,&.indentstr)
	strcat(&.str,prefix)
	if prefix^ then
		strcat(&.str," ")
	fi

	return &.str
end

global function getdottedname(ref strec p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	ref strec owner

	strcpy(&.str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(&.str2,&.str)
		strcpy(&.str,owner.name)
		strcat(&.str,".")
		strcat(&.str,&.str2)
		owner:=owner.owner
	od
	if p.blockno then
	!	sprintf(&.str2,".%d",int32(p.blockno))
		print @&.str2,".",,p.blockno
		strcat(&.str,&.str2)
	fi
	return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @&.str,"# ",currfileno,currlineno:"z5",$
	return &.str
end

global proc printst(filehandle f,ref strec p,int level=0)=
	ref strec q

	if p.symbol<>namesym then
		mcerror("PRINTST not name")
	fi

	printstrec(f,p,level)

	q:=p.deflist

	while q<>nil do
		printst(f,q,level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f,ref strec p,int level)=
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset
	const tabstr="    "
	[256]char str
	int scope
	ref paramrec pm

	gs_init(d)

	offset:=0
	to level do
		gs_str(d,tabstr)
		offset+:=4
	od
	gs_str(d,":")

	if p.blockno then
!	sprintf(&.str,"#.%d",p.name,int32(p.blockno))
		print @&.str,p.name,,".",,p.blockno

		gs_leftstr(d,&.str,28-offset,'-')
	else
		gs_leftstr(d,p.name,28-offset,'-')
	fi
	gs_leftstr(d,namenames[p.nameid],12,'.')
	col:=gs_getcol(d)

	gs_str(d,"[")

	gs_str(d,scopenames[p.scope])
	gs_str(d," ")

!	if p.isstatic then
!		gs_str(d,"Stat")
!	fi
	if p.align then
		gs_str(d,"@@")
		gs_strint(d,p.align)
		gs_str(d," ")
	fi
	if p.varparams then
		gs_str(d,"Var ")
	fi
	if p.used then
		gs_str(d,"Used ")
	fi
!	if p.ax_frame then
!		gs_str(d,"Frm ")
!	fi
!	if p.ax_autovar then
!		gs_str(d,"AV ")
!	fi
	if p.nparams then
!	sprintf(&.str,"Pm:%d ",int32(p.ax_nparams))
		fprint @&.str,"Pm:# ",p.nparams

		gs_str(d,&.str)
	fi

!	if p.moduleno then
!!	sprintf(&.str,"M#%d ",int32(p.ax_moduleno))
!		fprint @&.str,"M# ",p.moduleno
!		gs_str(d,&.str)
!	fi

	gs_str(d,"]")
	gs_padto(d,col+10,'=')

	if p.owner then
!	sprintf(&.str,"(#)",p.owner.name)
		fprint @&.str,"(#)",p.owner.name
		gs_leftstr(d,&.str,18,' ')
	else
		gs_leftstr(d,"()",18,' ')
	fi

	case p.mode
	when tvoid then
		gs_str(d,"Void ")
	else
		gs_strsp(d,strmode(p.mode))
	esac

	case p.nameid
	when fieldid then
		gs_str(d,"Offset:")
		gs_strint(d,p.offset)

	when frameid,paramid then
		if p.code then
			gs_str(d,"=")
!CPL =P.CODE,JTAGNAMES[P.CODE.TAG]
!STREXPR(P.CODE)

			gs_strvar(d,strexpr(p.code))
		fi
		gs_str(d," Offset: ")
		gs_strint(d,p.offset)

	when procid then

		gs_str(d,"Index:")
		gs_strint(d,p.index)

	when enumid then
		gs_str(d,"Enum:")
		gs_strint(d,p.index)

	when staticid then
		if p.code then
			gs_str(d,"=")
			gs_strvar(d,strexpr(p.code))
		fi
		gs_str(d,"STATIC********")
	esac

	gs_str(d," ")

	gs_str(d,"Lineno:")
	gs_strint(d,p.lineno iand 16777215)
	gs_str(d," ")
	gs_str(d,sourcefilenames[p.lineno>>24])

	if p.nameid=procid then
		gs_line(d)
		pm:=p.paramlist
		while pm do
			gs_str(d,"		Param: ")
			gs_leftstr(d,(pm.def|pm.def.name|"Anon"),10,'-')
!		gs_leftstr(d,strmode(pm.mode),16, ' ')
			gs_str(d,pmflagnames[pm.flags])
			gs_str(d," Mode:")
			gs_str(d,strmode(pm.mode))
			gs_str(d," Code:")
			gs_strint(d,cast(p.code))

			gs_line(d)
			pm:=pm.nextparam
		od
	fi

!	gs_str(d," MODE:")
!	gs_strint(d,p.mode)

	gs_println(d,f)

	if p.code then
		case p.nameid
		when frameid,staticid then
			printunit(f,p.code,-3)
		esac
	fi
end

global proc printstflat(filehandle f)=
	int i
	ref strec p
	ref tokenrec lx
	println @f,"GLOBAL SYMBOL TABLE:"

	for i:=0 to hstsize-1 do
		p:=hashtable^[i]
		if p.name then
			case p.symbol
	!		when namesym then
			when namesym,ktypespecsym, ksourcedirsym then
!CPL P.NAME

	!			println @f,i,p,":",p.name,symbolnames[p.symbol],namenames[p.nameid]
				println @f,i,p,":",getstname(p),symbolnames[p.symbol],namenames[p.nameid]
				p:=p.nextdupl
				while p do
					print   @f,"	",p,getstname(p),symbolnames[p.symbol],namenames[p.nameid],
						p.prevdupl
					println @f,"(From",(p.owner|getstname(p.owner)|"-"),,")"
					p:=p.nextdupl
				od
	!		else
	!			println @f,"not showing",p.name
			esac
		fi
	od
end

global function strexpr(ref unitrec p)ref strbuffer=
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jeval(exprstr,p)
	return exprstr
end

proc jeval(ref strbuffer dest, ref unitrec p)=
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	ref unitrec q
	[16000]char str
	int lb,t

!CPL "JEVAL",P,JTAGNAMES[P.TAG]

	case p.tag
	when jconst then
		if (t:=p.mode)=trefchar then
			if p.slength=0 then goto doref fi		!might be initialised to something else
			if not p.isstrconst then goto doref fi		!might be initialised to something else
			if p.slength>str.len/2 then
				strcpy(&.str,"LONGSTR)")
			else
				convertstring(p.svalue,&.str)
			fi
			gs_additem(dest,"""")
			gs_additem(dest,&.str)
			gs_additem(dest,"""")
			return
		elsif t>=ti8 and t<=ti64 then
!		sprintf(&.str,"%lld",p.value)
			getstrint(p.value, &.str)

		elsif t>=tu8 and t<=tu64 then
!		sprintf(&.str,"%llu",p.uvalue)
			strcpy(&.str,strword(p.uvalue))

		elsif t=tr64 or t=tr32 then
!		sprintf(&.str,"%f",p.xvalue)
			strcpy(&.str,strreal(p.xvalue))
		else
			case ttbasetype[p.mode]
			when tref then
	doref:
!			sprintf(&.str,"%p",p.svalue)
				print @&.str,ref void(p.svalue)
			when tarray then
				strcpy(&.str,"ARRAY")
			else
				CPL typename(p.mode)
	ABORTPROGRAM("EVAL/C")

			esac
		fi
		gs_additem(dest,&.str)

	when jname then
		gs_additem(dest,p.def.name)

	when jfuncname then
		gs_str(dest,"&")
		gs_additem(dest,p.def.name)

	when jandl,jorl,jandand,jeq,jne,jlt,jle,jgt,jge,jadd,jsub,jmul,jdiv,
			jrem,jiand,jior,jixor,jshl,jshr,
			jaddto,jsubto,jmulto,jdivto,
			jremto,jiandto,jiorto,jixorto,jshlto,jshrto 	then

		strcpy(&.str,getopcjname(p.tag))
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,&.str)
		jeval(dest,p.b)
		gs_additem(dest,")")

	when jneg,jabs,jinot,jnotl,jistruel then

		strcpy(&.str,getopcjname(p.tag))
!	strcpy(&.str,"getopcjname(p.tag)")
		gs_additem(dest,&.str)
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,")")

	when jcallfn then
		jeval(dest,p.a)
		gs_additem(dest,"(")

		q:=p.b
		while q do
			jeval(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,")")

	when jdot then
		jeval(dest,p.a)
		gs_additem(dest,".")
	GS_STR(DEST,"???")
!	jeval(dest,p.b)

	when jidot then
		jeval(dest,p.a)
		gs_additem(dest,"->")
		jeval(dest,p.b)

	when jmakelist,jexprlist then
		lb:=p.tag=jexprlist
		gs_additem(dest,(lb|"("|"{"))

		q:=p.a
		while q do
			jeval(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,(lb|")"|"}"))

	when jassign then
		jeval(dest,p.a)
		gs_additem(dest,"=")
		jeval(dest,p.b)

	when jifx then
		jeval(dest,p.a)
		gs_additem(dest,"?")
		jeval(dest,p.b)
		gs_additem(dest,":")
		jeval(dest,p.c)

	when jconvert then

		gs_additem(dest,strmode(p.mode))
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,")")

	when jptr then
		gs_additem(dest,"*(")
		jeval(dest,p.a)
		if p.b then
			gs_additem(dest,"+")
			jeval(dest,p.b)
		fi
		gs_additem(dest,")")

	when jblock then
		gs_additem(dest,"<JBLOCK>")

	when jpreincr then
		gs_additem(dest,"++")
		jeval(dest,p.a)

	when jpredecr then
		gs_additem(dest,"--")
		jeval(dest,p.a)

	when jpostincr then
		jeval(dest,p.a)
		gs_additem(dest,"++")

	when jpostdecr then
		jeval(dest,p.a)
		gs_additem(dest,"--")


	when jnull then
		gs_str(dest,"<nullunit>")

	when jscale then
		gs_str(dest,"scale((")
		jeval(dest,p.a)
		if p.scale>0 then
			gs_str(dest,")*")
			gs_strint(dest,p.scale)
		else
			gs_str(dest,")/")
			gs_strint(dest,-p.scale)
		fi
		gs_str(dest,")")
	when jaddptr then
		gs_str(dest,"(")
		jeval(dest,p.a)
		gs_str(dest,"+")
		jeval(dest,p.b)
		gs_str(dest,")")

	when jwidenmem then
		jeval(dest,p.a)


	else
!CPL JTAGNAMES[P.TAG]
	gs_str(dest,"<CAN'T DO JEVAL>")
	end
end

global proc printfilelist(filehandle f)=
!global [0..maxmodule]modulerec moduletable
!global [0..maxmodule]ichar inputfiles
!global [0..maxlibfile]ichar libfiles
!global [0..maxsourcefile]ichar sourcefilenames
!global [0..maxsourcefile]ichar sourcefilepaths
!global [0..maxsourcefile]ichar sourcefiletext
!global [0..maxsourcefile]int32 sourcefilesizes
	
	println @f,"Source files",nsourcefiles
	for i to nsourcefiles do
		fprintln @f,"# # (#)", i, sourcefilenames[i]:"12jl", sourcefilepaths[i]
	od
	println @f,"\nInput file:",inputfile
	println @f,"\nLibfiles",nlibfiles
	for i to nlibfiles do
		println @f,i, libfiles[i]
	od

end

global proc printmodelist(filehandle f)=
	int m, mbase
	const tab="\t"

	println @f,"PROC MODELIST",ntypes

	for m:=0 to ntypes do
		println @f,m:"4", strmode(m)
		mbase:=ttbasetype[m]
		if tttypedef[m] then println @f,tab,"Typedef:",tttypedef[m].name fi

		println @f,tab,"Basetype:",mbase,strmode(mbase)
		println @f,tab,"Name:",typename(m)

		println @f,tab,"ttnamedef:",ttnamedef[m],(ttnamedef[m]|ttnamedef[m].name|"-")
		println @f,tab,"Target:",strmode(tttarget[m])
		println @f,tab,"Size:",ttsize[m]
		println @f,tab,"Length:",ttlength[m]
		println @f,tab,"Isblock:",ttisblock[m]
		println @f,tab,"Const:",ttconst[m]
		println @f,tab,"Signed:",ttsigned[m]
		println @f,tab,"Ref:",ttreftype[m]
!		println @f,tab,"Isreal:",ttisreal[m]
!		println @f,tab,"Isinteger:",ttisinteger[m]
!		println @f,tab,"Isshort:",ttisshort[m]
		println @f,tab,"Constver:",strmode(ttconsttype[m])
		println @f,tab,"Shared:",ttshared[m]
		println @f
	od

	println @f
end

=== info.txt 0 1 36/38 ===
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
=== help.txt 0 1 37/38 ===
C Subset Compiler for 64-bit Windows

Normal use:

    mcc prog            Compile file prog.c to prog.exe
    mcc prog.c          Same (extension is optional)
    mcc a b c d.dll     Compile a.c, b.c, c.c and link with d.dll to a.exe

Options:

    -e              Write preprocessed output to prog.i
    -s              Compile to .asm file
    -c              Compile to .obj file
    -exe            (DEFAULT) compile and link to .exe file

    -i:path         Add include path
    -ext            Don't use internal standard headers
    -out:file       Name exe file

    @file           Read further files and options from a file

Other Options:

    -info           Show further information
    -time           Show compiler timing stats
    -writeheaders   Write out internal headers as .hdr (not .h) files
    -stdout         Write preprocessor output to console, rather than .i file
=== mcc.h 0 1 38/38 ===
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
1 cc.m
2 pc_api.m
3 pc_decls.m
4 pc_diags.m
5 pc_exp.m
6 pc_lib.m
7 pc_tables.m
8 mc_auxmcl.m
9 mc_genmcl.m
10 mc_genss.m
11 mc_libmcl.m
12 mc_decls.m
13 mc_objdecls.m
14 mc_optim.m
15 mc_stackmcl.m
16 mc_writeasm.m
17 mc_writeexe.m
18 mc_writeobj.m
19 mx_decls.m
20 mx_run.m
21 mx_lib.m
22 mx_write.m
23 cc_cli.m
24 cc_decls.m
25 cc_tables.m
26 cc_lex.m
27 cc_parse.m
28 cc_genpcl.m
29 cc_blockpcl.m
30 cc_libpcl.m
31 cc_lib.m
32 cc_support.m
33 cc_headersx.m
34 cc_export.m
35 cc_show.m
36 info.txt
37 help.txt
38 mcc.h
