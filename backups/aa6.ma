=== MA 16 ===
=== aa.m 0 0 1/16 ===
!project =

	module aa_cli

	module aa_decls
	module aa_genss
	module aa_lex
	module aa_lib
	module aa_objdecls
	module aa_mcxdecls
	module aa_parse

	module aa_tables
	module aa_writeexe
	module aa_writemcx
	module aa_writeobj

	module aa_disasm
	module aa_writess


!	$sourcepath "c:/bx/"
!
!	module mx_decls
!	module mx_run
!	module mx_lib
!	module mx_write



!	module aa_writessdummy
!end
=== aa_cli.m 0 0 2/16 ===
global int logdest=0			!no diagnostic output

byte fshowmcl
byte fshowss
byte fshowss2
byte fshowsx
byte fshowtiming

!global enumdata []ichar optionnames=
!	(lex_sw,		"lex"),		!first four must be in this order
!	(parse_sw,		"parse"),
!	(gen_sw,		"gen"),
!	(obj_sw,		"obj"),
!	(dll_sw,		"dll"),
!	(ml_sw,			"ml"),
!	(mx_sw,			"mx"),
!	(exe_sw,		"exe"),
!
!	(mcl_sw,		"mcl"),
!	(ss_sw,			"ss"),
!	(ss2_sw,		"ss2"),
!	(sx_sw,			"sx"),
!	(time_sw,		"time"),
!	(s_sw,			"s"),
!	(d_sw,			"d"),
!	(v_sw,			"v"),
!	(q_sw,			"q"),
!	(help_sw,		"help"),
!	(out_sw,		"out"),
!	(main_sw,		"main"),
!	(start_sw,		"start"),
!
!	(rip_sw,		"rip"),
!	(himem_sw,		"himem"),
!end

!global int axlevel = exe_sw

const logfile = "mx.log"

ichar inputfile
ichar outputfile

proc main=
	ichar ext
	ref strbuffer ss
	int ntokens,t,i,U,j

!CPL =STREC.BYTES

	T:=CLOCK()
	initall()

	getinputoptions()

	inputfile:=moduletable[1].filename

	initlogfile()

!CPL =HIGHMEM

	if axlevel=lex_sw then
		if nmodules>1 then loaderror("lex test/multi files") fi
		lextest(inputfile)
	else
		if outputfile=nil then
			outputfile:=inputfile
		fi
!		ext:=
			case axlevel
!			when exe_sw then "exe"
			when dll_sw then ext:="dll"
			when obj_sw then ext:="obj"
			when ml_sw then ext:="ml"
			when mx_sw then ext:="mx"
			else
				ext:="exe"
			esac
		outputfile:=pcm_copyheapstring(changeext(outputfile,ext))

		if not fquiet then
				println "Assembling",inputfile,"to",outputfile
		fi

		if fverbose then
			showcaption()
			println
		fi
		loadsourcefiles()
		parsemodules()
!T:=CLOCK()-T; CPL "PARSE TIME", T; T:=CLOCK()
!CPL "AA1"
		genss()
!T:=CLOCK()-T; CPL "GENSS TIME", T; T:=CLOCK()
		case axlevel
		when obj_sw then
!CPL "AA2"
!			if fshowss or fshowsx then
!				initsectiontable()					!need for display
!				ss:=writessdata(0)
!				gs_println(ss,logdev)
!			fi

			writess(outputfile)
			if fshowss or fshowsx then
				initsectiontable()					!need for display
				ss:=writessdata(0)
				gs_println(ss,logdev)
			fi

		when exe_sw, dll_sw then
!CPL "AA3"
			initsectiontable()
			if fshowss then
				ss:=writessdata(0)
				gs_println(ss,logdev)
			fi

			genexe(nil,outputfile, axlevel=dll_sw)
!T:=CLOCK()-T; CPL "GENEXE TIME", T; T:=CLOCK()
			if fshowsx then
				ss:=writessdata(1)
				gs_println(ss,logdev)
			fi

			writeexe(outputfile, axlevel=dll_sw)

		when ml_sw, mx_sw then
			if fshowss then
				ss:=writessdata(0)
				gs_println(ss,logdev)
			fi

!LOADERROR("NO WRITEMCX")
			writemcx(outputfile)

		esac
!CPL "AA5"

		if fshowmcl then
			ss:=writemclblock()
			gs_println(ss,logdev)
		fi
	fi

	if fshowtiming then
		T:=CLOCK()-T
		CPL "Time",T
	fi

	closelogfile()
	stop 0
end

proc loadsourcefiles=
	int i
	ichar source

	for i to nmodules do
		source:=cast(readfile(moduletable[i].filename))
		if source=nil then
			loaderror_s("Can't load file: %s",moduletable[i].filename)
		fi
		moduletable[i].source:=source
	od
end

proc parsemodules=
	int i
	ichar source

	for i to nmodules do
!CPL; CPL "PARSE",I
		currmoduleno:=i
		modulenamelist:=nil
		readmodule(i)

		checkundefined()
		if nundefined then
			println "Couldn't assemble - press key"
!			os_getch()
			stop 1
		fi

		scanglobals()			!fixup any globals and imports
		if fshowsx then
!			printmodulesymbols(logdev)
		fi
		if i<>nmodules then
			resethashtable()
		fi
	od

!CPL "AFTER LAST MODULE:"; PRINTHASHTABLE()
!CPL "////////////////////// PARSED"

if fshowsx then
!	printimportsymbols(logdev)
!	printdupltable(logdev)
fi

!Try scanning all mclcode to fix imports/exports. That is, all operands
!point to the same st entry
ref mclrec m

m:=mccode

while m do
	fixopnd(m.a)
	fixopnd(m.b)
	m:=m.nextmcl
od

end

proc fixopnd(ref opndrec a)=
	ref strec d
	if a=nil then return fi
	if a.labeldef then
		d:=a.labeldef
		if d.basedef then
			a.labeldef:=d.basedef
		fi
	fi
end

proc initlogfile=
	case logdest
	when 2 then
		remove(logfile)
		logdev:=cast(fopen(logfile,"w"))
	when 0,1 then
		logdev:=nil
	esac

end

proc closelogfile=			!CLOSELOGFILE
	[512]char str

	if logdest=2 then
		fclose(logdev)

CPL "PRESS KEY.."; OS_GETCH()

		print @&.str,f"\m\ed.bat",logfile

!CPL =&.STR

!		os_execwait(&.str,1,nil)
		os_execwait(&.str,0,nil)
	fi
	end

proc initall=
	pcm_init()
	initlex()
	initlib()
end

proc lextest(ichar file)=
	loadsourcefiles()
	initsourcefile(moduletable[1].source)

!INT T:=CLOCK()
	lxsymbol:=eolsym
	while lxsymbol<>eofsym do
		lex()
	od
!T:=CLOCK()-T
!CPL "LEX TIME", T
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
			if strchr(name,'*') or strchr(name,'?') then
				dowildcards(name)
			else
				addmodule(name)
			fi
		when pm_libfile then
			addsearchlib(convlcstring(name))
		esac
	od

	if nmodules=0 and nsearchlibs=0 then
		showcaption()
		println
		println "Usage:"
		println "	",,cmdparams[0],"filename[.asm]           # Assemble filename.asm to filename.exe"
		println "	",,cmdparams[0],"-help                    # Show other options"
		stop 1
	fi

	if fshowss or fshowsx or fshowss2 or fshowmcl then
		if logdest=0 then logdest:=2 fi
	fi

	if axlevel=dll_sw then
CPL "DLL->HIMEM"
		highmem:=2
	fi

	addsearchlib("msvcrt")
	addsearchlib("gdi32")
	addsearchlib("user32")
	addsearchlib("kernel32")

	if nmodules=0 then
		loaderror("No input files specified")
	fi
end

proc do_option(int sw, ichar value)=

	case sw
	when lex_sw, parse_sw, gen_sw, obj_sw, exe_sw, dll_sw, ml_sw, mx_sw then
		axlevel:=sw
	when mcl_sw then
		fshowmcl:=1
	when ss_sw then
		fshowss:=1
	when ss2_sw then
		fshowss2:=1
	when sx_sw then
		fshowsx:=1
	when time_sw then
		fshowtiming:=1
	when s_sw then
		logdest:=1
	when d_sw then
		logdest:=2
	when v_sw then
		fverbose:=1
	when q_sw then
		fquiet:=1
	when help_sw then
		showhelp()
	when out_sw then
		outputfile:=pcm_copyheapstring(value)
	when main_sw then
	when start_sw then
!		entrypointname:="start"

	when rip_sw then
		highmem:=1
	when himem_sw then
		highmem:=2

	esac

end

proc showhelp=
!	showcaption()
	println
	println strinclude "aa_help.txt"
	stop 1
end

proc showcaption=
	print "AA2.0 Assembler/Linker",$date
end

proc loaderror(ichar mess)=
	println "Error:",mess
	stop 1
end

proc loaderror_s(ichar mess,s)=
	[256]char str
	sprintf(&.str,mess,s)
	loaderror(&.str)
end

proc addmodule(ichar name)=
	if nmodules>=maxmodules then
		loaderror("Too many modules")
	fi
	++nmodules
	moduletable[nmodules].filename:=pcm_copyheapstring(name)
	moduletable[nmodules].name:=pcm_copyheapstring(extractfile(name))
	moduletable[nmodules].source:="<empty>"

end

proc addsearchlib(ichar name)=
	[300]char str

	if eqstring(extractext(name),"mcx") then
		addimportlib(name)
		return
	fi

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

proc addimportlib(ichar name)=
	[300]char str

	name:=changeext(name,"")

	for i to nimportlibs do
		if eqstring(importlibs[i],name) then return fi
	od

	if nimportlibs>=maximportlibs then
		loaderror("Too many LIBs")
	fi
	++nimportlibs
	importlibs[nimportlibs]:=pcm_copyheapstring(name)
end

proc showmodules=
	int i

	println "Modules:",nmodules
	for i:=1 to nmodules do
		println "  ",i,,":",
			padstr(moduletable[i].name,13),
			padstr(moduletable[i].filename,25),
			strlen(moduletable[i].source)
	od
	println
	println "DLL Libs:",nsearchlibs
	for i:=1 to nsearchlibs do
		println "  ",i,,":",searchlibs[i]
	od
	println
	println "Import Libs:",nimportlibs
	for i:=1 to nimportlibs do
		println "  ",i,,":",importlibs[i]
	od
	println
end

function getemptyst(ref strec d)ref strec=
!d is an existing strec
!create an new empty strec if needed (when d is also keyword name,
! and/or e is not nil), and return a pointer to that
!otherwise just return nil
	ref strec dnew

!CPL "GETEMPTY",D.NAME,=D
	if d.ksymbol then					!need a replacement strec
		dnew:=pcm_allocz(strec.bytes)
		dnew.name:=d.name
		dnew.namelen:=d.namelen
		dnew.ksymbol:=d.ksymbol
		dnew.subcode:=d.subcode
		dnew.regsize:=d.regsize
		return dnew
!RETURN D
	fi
	return nil
end

function findduplname(ref strec d)ref strec=
!look for any dupl global/export name to d

	ref strec e
	if d.basedef then
		return d.basedef
	fi

	e:=dupltable[d.htfirstindex]

	while e do
		if d.namelen=e.namelen and memcmp(d.name,e.name,d.namelen)=0 then
			d.basedef:=e
			return e
		fi
		e:=e.nextdupl
	od
	return nil
end

proc adddupl(ref strec d)=
!add the first dupl entry for d in dupltable
!the linked list is in reverse order, and generally ends up containing
!one element unless there are two or more names that share the same default
!hash table entry

	d.nextdupl:=dupltable[d.htfirstindex]
	dupltable[d.htfirstindex]:=d
end

proc scanglobals=
!have just finished parsing a module
!scan the symbols defined there to:
! * find any new imports/globals
! * find new imports/globals to merge with existing ones
!Then the entries in the hashtable must be purged, by substituting with
!either nil, or an empty value if keyword data or .basedef must be remembered

	ref strec d,e

	d:=modulenamelist

	while d do
		case d.symbol
		when importedsym then
			e:=findduplname(d)
			if e then
				case e.symbol
				when importedsym then			!no change
				when exportedsym then
					d.symbol:=exportedsym		!set both global
					d.reftype:=e.reftype:=fwd_ref
				esac
			else
!*!				addimport(d)
				adddupl(d)
			fi
		when exportedsym then
			e:=findduplname(d)
			if e then
				case e.symbol
				when importedsym then
					e.symbol:=exportedsym		!set both global
					d.reftype:=e.reftype:=fwd_ref
				when exportedsym then			!error?
					CPL MODULETABLE[D.MODULENO].NAME,D.NAME,D.HTINDEX
					CPL MODULETABLE[E.MODULENO].NAME,E.NAME,E.HTINDEX
					serror_s("Multiply-defined global: %s",d.name)
				esac
			else
				e:=d
!*!				addimport(d)
				adddupl(d)
			fi
		esac

		d:=d.nextdef
	od
end

proc resethashtable=
!have just finished parsing a module
!scan the symbols defined there to:
! * find any new imports/globals
! * find new imports/globals to merge with existing ones
!Then the entries in the hashtable must be purged, by substituting with
!either nil, or an empty value if keyword data or .basedef must be remembered

	ref strec d,e

!CPL "BEFORE RESET:"; PRINTHASHTABLE()

	d:=modulenamelist

	while d do
!CPL "RESETNAME",D.NAME
		lexhashtable[d.htindex]:=getemptyst(d)
		d:=d.nextdef
	od

	modulenamelist:=nil
!CPL "AFTER RESET:"; PRINTHASHTABLE()

end

proc dowildcards(ichar filespec)=
!filespec contains * and/or ?
!get list of files, default to .c extension (or require it), and
!add to inputfiles
	const maxfiles=250
	[maxfiles]ichar table
	int nfiles


	nfiles:=dirlist(filespec, &table, maxfiles)

	if nfiles=-1 then
		loaderror("Too many files")
	fi

	for i to nfiles do
		addmodule(table[i])
	od
end
=== aa_decls.m 0 0 3/16 ===
!MXA Assembler Global Decls

global const compilerversion="2018.1.22"

global type symbol = ref strec

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record riprec =
	ref riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

global type operand = ref opndrec

global record opndrec = !24 bytes
	ref strec labeldef	!nil, or handle of strec for label
	union
		int64 value		!const value/extra offset/cond code/string for comments
		real64 xvalue	!floating point value
		ref char svalue
	end
	byte mode		!a_reg etc, low level operand details
	byte size		!byte size of operand: 1,2,4,8
	byte reg		!0, or main register
	byte regix		!0, or index register

	byte scale		!0, or scale factor for regix
	byte addrsize	!4 or 8 for a_mem when regs are involved
	byte valtype	!0 (no value or int) or 'R'/'S'
	byte spare2
end

global record strec =
	ichar name			!name of symbol (named token/keyword or identifier)
	ref fwdrec fwdrefs	!fwd ref chain
	ref opndrec expr	!named constants: valuerec([label],[value])
	int32 offset		!label (pass 2): offset of label when encountered
	int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
	int32 importindex	!genexe: index into import table
	int32 PADDING

	byte symbol			!type of token, eg. namesym
	byte ksymbol		!type of keyword, eg. opcodesym
	byte subcode		!when used as keyword
	byte regsize		!for reg keywords

	byte XXX			!label pass 1: fwd/extern/local/global
	byte reftype		!label pass 2: extern/back/fwd
	byte segment		!label pass 2: code_seg etc or 0
	byte namelen

	ref strec basedef		!nil, or global/import def using this name
	ref strec nextdef		!in module name list
	ref strec nextdupl		!when part of in global import list

	int32 moduleno
	word32 htindex				!index into hashtable
	word32 htfirstindex			!initial index before stepping to avoid clashes

	word32 impindex			!for mcx o/p: 0 or index into compact import/export table
	word32 expindex

	[36]BYTE SPARE
end

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

global record modulerec =
	ichar filename
	ichar name
	ichar source
end

global record stlistrec =
	ref strec def
	ref stlistrec nextitem
end

global int lxfileno=0	!*@ current source file number
global int lxlineno=0	!*@ current source line number

global int nsourcefiles=0	!no. of linear file names

global const maxmodules=2000
global const maxsearchlibs=30
global const maximportlibs=30
global [maxmodules]modulerec moduletable
export [maxsearchlibs]ichar searchlibs
export [maximportlibs]ichar importlibs
global int nmodules
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
global [0:hstsize]ref strec lexhashtable
global [0:hstsize]ref strec dupltable		!link dupl names

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
global ref []ref strec ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref stlistrec globalimportlist		!all global vars and imports across all moduls

global ref strec modulenamelist			!all defs defined in last module
global int currmoduleno

global ref riprec riplist
!
global byte highmem						!0/1/2 = lowmem+norip/lowmem+rip/himem+rip

global int axlevel = exe_sw

global enumdata []ichar optionnames=
	(lex_sw,		"lex"),		!first four must be in this order
	(parse_sw,		"parse"),
	(gen_sw,		"gen"),
	(obj_sw,		"obj"),
	(dll_sw,		"dll"),
	(ml_sw,			"ml"),
	(mx_sw,			"mx"),
	(exe_sw,		"exe"),

	(mcl_sw,		"mcl"),
	(ss_sw,			"ss"),
	(ss2_sw,		"ss2"),
	(sx_sw,			"sx"),
	(time_sw,		"time"),
	(s_sw,			"s"),
	(d_sw,			"d"),
	(v_sw,			"v"),
	(q_sw,			"q"),
	(help_sw,		"help"),
	(out_sw,		"out"),
	(main_sw,		"main"),
	(start_sw,		"start"),

	(rip_sw,		"rip"),
	(himem_sw,		"himem"),
end

=== aa_genss.m 0 0 4/16 ===
const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

const wbit = 3
const rbit = 2
const xbit = 1
const bbit = 0

byte rex
byte sizeoverride					!32=>16 switch
byte addroverride					!32=>16 switch
byte f2override						!xmm regs
byte f3override						!xmm regs
byte nowmask						!disable w-bit
byte usesizeb						!1 tests opnd b for wmask

operand extraparam

int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
ref relocrec currrelocs
int nrelocs

ref mclrec currmcl
ref riprec ripentry					!nil, unless instr has an rip-reloc entry

enumdata [0:]ichar opnames =
	(add_op=0,	"add"),
	(or_op,		"or"),
	(adc_op,	"adc"),
	(sbb_op,	"sbb"),
	(and_op,	"and"),
	(sub_op,	"sub"),
	(xor_op,	"xor"),
	(cmp_op,	"cmp")
end

export proc genss=
	int index
	ref mclrec m

	ss_zdatalen:=0
	ss_zdata:=buffercreate()
	ss_idata:=buffercreate()
	ss_code:=buffercreate()
	ss_idatarelocs:=nil
	ss_coderelocs:=nil
	ss_nsymbols:=0

	switchseg(code_seg)

	alineno:=9999
	extraparam:=nil

	m:=mccode
	index:=0

	while m do
		alineno:=m.lineno
		ripentry:=nil

		doinstr(m,++index)
		m:=m.nextmcl
	od

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		gerror("Zdata contains code or data")
	fi

	if axlevel=obj_sw then
		ref riprec pr
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

end

proc doinstr(ref mclrec m,int index)=
	operand a,b
	ref strec d,e
	int x,offset,shortjmp,n

	CURRMCL:=M
	buffercheck(currdata)

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=nowmask:=usesizeb:=0

!CPL "DOINST",MCLNAMES[M.OPCODE], M.LINENO,=CURRDATA.ALLOC

	a:=m.a
	b:=m.b

	switch m.opcode
	when m_labelx then
		d:=a.labeldef

		d.reftype:=back_ref
		d.segment:=currseg
		d.offset:=getcurrdatalen(6)

		if d.symbol=exportedsym then
			getstindex(d)
		fi

		dofwdrefs(d)

	when m_call then
		do_call(a)

	when m_jmp then
		do_jmp(a,m)

	when m_jmpcc then
		offset:=getrel32(b.labeldef,getcurrdatalen(7)+1)

		if offset<0 then			!backjump
			if offset<-126 then
				genbyte(0x0F)
				genbyte(0x80+a.value)
				gendword(offset-4)
			else
				genbyte(0x70+m.a.value)
				genbyte(offset)
			fi
		else
			shortjmp:=checkshortjump(m,b.labeldef)
			if not shortjmp then
				genbyte(0x0F)
				genbyte(0x80+a.value)
				genrel32(b)
			else
				genbyte(0x70+a.value)
				genrel8(b)
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

	when m_segment then
		switchseg(a.value)
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

	when m_leave then
		genbyte(0xC9)

	when m_retn then
		if a.mode<>a_imm then gerror("retn?") fi
		genbyte(0xC2)
		genword(a.value)

	when m_push then
		do_push(a)

	when m_pop then
		do_pop(a)

	when m_inc, m_dec then
		do_inc(a,mclcodes[m.opcode])

	when m_neg, m_not, m_mul, m_imul, m_div, m_idiv then
		do_neg(a,mclcodes[m.opcode])

	when m_add, m_sub, m_and, m_or, m_xor, m_adc, m_sbb, m_cmp then
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

!when m_imul3 then
!	do_imul3(a,b[1],b[2])

	when m_resb, m_resw, m_resd, m_resq then
		if a.mode=a_imm then
			n:=a.value*mclcodes[m.opcode]
			buffercheck(currdata,n)
			case currseg
			when code_seg then
				to n do genbyte(0x90) od
			when idata_seg then
				to n do genbyte(0) od
			else
				ss_zdatalen+:=n
			esac
	
		else
			gerror("resb?")
		fi

	when m_align then
		if a.mode=a_imm then
			x:=a.value
!			if x not in 1..16384 then gerror("align2") fi
			if x<1 or x>16384 then gerror("align2") fi
			if currseg<>zdata_seg then
				while bufferlength(currdata) rem x do genbyte((currseg=code_seg|0x90|0)) od
			else
				while ss_zdatalen rem x do	++ss_zdatalen od
			fi
		else
			gerror("align?")
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
		do_setcc(a,b)

	when m_movd then
		do_movxmm(a,b,4)

	when m_movq then
		do_movxmm(a,b,8)


	when m_addss, m_subss, m_mulss, m_divss, m_sqrtss, m_minss, m_maxss then
		do_arithxmm(a,b,0xF3,mclcodes[m.opcode])

	when m_addsd, m_subsd, m_mulsd, m_divsd, m_sqrtsd, m_minsd, m_maxsd then
		do_arithxmm(a,b,0xF2,mclcodes[m.opcode])
	when m_comiss then
		do_arithxmm(a,b,0,0x2F)

	when m_comisd then
		do_arithxmm(a,b,0x66,0x2F)


	when m_andps,m_xorps then
		do_logicxmm(a,b,mclcodes[m.opcode],4)

	when m_andpd,m_xorpd, m_pand, m_pxor then
		do_logicxmm(a,b,mclcodes[m.opcode],8)

	when m_pcmpistri,m_pcmpistrm then
		do_pcmpistri(a,b,m.c,mclcodes[m.opcode])


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
		do_cmovcc(a,extraparam,b)

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

	when m_bswap then
		do_bswap(a)

	when m_shld, m_shrd then
		do_dshift(a, b, m.c, mclcodes[m.opcode])

	else
		println "*** CAN'T DO OPCODE",mclnames[m.opcode],"line",alineno
	end switch

end

proc genbyte(int x)=
	currdata.pcurr++^:=x
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

proc genopnd(operand a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
	ref char s
	int64 x
	int length

	if size=0 then size:=a.size fi

	switch a.mode
	when a_imm,a_mem then
	when a_string then
		s:=a.svalue
		length:=strlen(s)
		if length>100 then
			buffercheck(currdata,max(1024,length+1))
		fi
		while s^ do
			genbyte(s++^)
		od
		return
	else
		gerror("GENOPND/bad opnd")
	end switch

	if a.labeldef and size<=2 then
		gerror("8/16-BIT RELOC")
	fi

	case size
	when 1 then
		genbyte(a.value)
	when 2 then
		genword(a.value)
	when 4 then
		if a.labeldef then
			genabs32(a)
		else
			if a.valtype then		!was real
				gendword(getr32bits(a.xvalue))
			else
				gendword(a.value)
			fi
		fi
	when 8 then
		if a.labeldef then
			genabs64(a)
		else
			x:=a.value
			if a.valtype then
!*!				genqword(int64@(x))		!X IS INT64; WHAT'S HAPPENING HERE?
				genqword(x)

			else
				genqword(x)
			fi
		fi
	esac
end

proc addrelocitem(int reloctype, ref strec d)=
	ref relocrec r
	int stindex, adjust

	stindex:=getstindex(d)

	adjust:=4
	if reloctype=addr64_rel then adjust:=8 fi

	r:=pcm_alloc(relocrec.bytes)
	r.nextreloc:=currrelocs
	r.reloctype:=reloctype
	r.offset:=getcurrdatalen(1)-adjust
	r.stindex:=stindex

	++nrelocs
	currrelocs:=r
end

function getstindex(ref strec d)int=
!retrieve existing obj st index, or create new one

	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		fi
		d.stindex:=++ss_nsymbols
		ss_symboltable^[d.stindex]:=d
	fi
	return d.stindex
end

proc genrel32(operand a)=
!used by call/longjmp/ddoffset
	ref strec d

	d:=a.labeldef

	if d=nil then				!constant
		gendword(a.value)
		return
	fi

	case d.reftype
	when back_ref then
		if d.segment<>currseg then
			gerror("Rel label across segments")			!might be Ok if treated as external?
		fi
		gendword(d.offset-(getcurrdatalen(2)+4))
	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel32_rel)
		gendword(0)
	else								!external symbol
		gendword(a.value)				!this is probably just zero
		addrelocitem(rel32_rel,d)
	esac
end

proc genabs32(operand a)=
!absolute refs to labels
	ref strec d

	d:=a.labeldef

!CPL "GENABS32",D.NAME

	case d.reftype
	when back_ref then
		gendword(d.offset+a.value)
		addrelocitem(addr32_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
		gendword(a.value)
		addrelocitem(addr32_rel,d)

	else								!external symbol
		gendword(a.value)
		addrelocitem(addr32_rel,d)
	esac
end

proc genabs64(operand a)=
!absolute refs to labels
	ref strec d

	d:=a.labeldef

	case d.reftype
	when back_ref then
!CPL "BACKREF",=D.OFFSET, =A.VALUE
		genqword(d.offset+a.value)
		addrelocitem(addr64_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr32_rel,currseg)
!CPL "FWDREF", =A.VALUE
		genqword(a.value)
		addrelocitem(addr64_rel,d)

	else								!external symbol
!CPL "IMPREF",A.VALUE
		genqword(a.value)
		addrelocitem(addr64_rel,d)
	esac
end

function getrel32(ref strec d,int offset)int=
!get rel difference between offset in this segment, and label d

	if d.reftype=back_ref then					!defined earlier in this segment
		if d.segment<>currseg then
			gerror("Rel label across segments2")
		fi
		return d.offset-(offset+1)
	else
		return int32.max
	fi
end

proc dofwdrefs(ref strec d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
!	d.fwdrefs append:=(getcurrdatalen(),rel32_rel)
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
			when zdata_seg then gerror("Fwd ref in zdata")
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
	CPL RELOCNAMES[F.RELTYPE]
			GERROR("DOFWDREFS/CAN'T DO RELTYPE")
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

function isbytesized(int64 x)int=
	return -128<=x<=127
end

function isdwordsized(int64 x)int=
	return int32.min<=x<=int32.max
end

proc do_push(operand a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then gerror("pushreg not 64-bit") fi
		code:=getregcode(a.reg, bmask)
		rex.[wbit]:=0
		genrex()
		genbyte(0x50+code)

	when a_imm then
!		if a.labeldef then
!			genbyte(0x68)
!			genopnd(a,4)
!		elsif isbytesized(a.value) then
!			genbyte(0x6A)
!			genbyte(a.value)
!		elsif a.value in i16.min..i16.max then
!			genbyte(0x66)
!			genbyte(0x68)
!			genword(a.value)
!
!		elsif isdwordsized(a.value) then
!			genbyte(0x68)
!			gendword(a.value)
!		else
!			gerror("push imm value too large")
!		fi


		if a.labeldef then
			genbyte(0x68)
			genopnd(a,4)
		elsif isbytesized(a.value) then
			genbyte(0x6A)
			genbyte(a.value)
		elsif isdwordsized(a.value) then
			genbyte(0x68)
			gendword(a.value)
		else
			gerror("push imm value too large")
		fi


	when a_mem then
		if a.size<>8 then gerror("push not 64-bit") fi
		genxrm(0xFF, 6, a)

	else
		gerror("push opnd?")
	esac
end

proc do_pop(operand a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then gerror("popreg not 64-bit") fi
		code:=getregcode(a.reg, bmask)
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a.size<>8 then gerror("pop not 64-bit") fi
		genxrm(0x8F, 0, a)
	else
		gerror("pop opnd?")
	esac
end

proc do_inc(operand a,int code)=
!inc/dec

	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xFE|0xFF), code, a)
	else
		gerror("inc/opnd?")
	esac
end

proc do_neg(operand a,int code)=
!neg/not/mul/imul/div/idiv
	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xF6|0xF7), code, a)
	else
		gerror("neg/div/etc opnd?")
	esac
end

proc genamode(operand a,int am)=
	int sib,mode,dispsize
	ref riprec pr

	sib:=am>>16

	mode:=(am>>8)iand 255
	dispsize:=am iand 255

	genbyte(mode)			!modrm byte

	if sib>=0 then		!sib byte
		genbyte(sib)
	fi
	case dispsize			!disp bytes
	when 0 then
	when 1 then
		genbyte(a.value)
	when 4 then
!CPL "GENAMODE/4",=SIB
		if sib=-2 then
			pr:=pcm_alloc(riprec.bytes)
			pr.next:=riplist
			pr.offset:=currdata.pcurr-currdata.pstart
			ripentry:=riplist:=pr
!CPL "CREATING RIP ENTRY", PR.OFFSET
!OS_GETCH()
		fi
		if a.labeldef then
!CPL "GENAMODE32/LABEL",A.LABELDEF.NAME
			genabs32(a)
		else
!CPL "GENAMODE32/VALUE"
			gendword(a.value)
		fi

	else
		gerror("genamode size 2/8")
	esac
end

function makemodrm(int mode,opc,rm)int=
	return mode<<6+opc<<3+rm
end

proc setopsize(operand a)=
	case a.size
	when 1 then			!assume set via specific opcodes
	when 2 then			!override default 4 bytes
		sizeoverride:=1
	when 8 then			!override default 4 bytes
	    rex ior:=wmask
	when 4 then			!assume 4 bytes is default
	when 16 then
	else
		gerror("Operand size not set")
	esac
end

function getdispsize(operand a,int mand=1)int=
!look at imm/mem displacement, and return 0,1 or 4
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned

	if a.labeldef then return 4 fi
	if a.value or mand then
		return (isbytesized(a.value)|1|4)
	else
		return 0
	fi
end

proc genrmbyte(int mode,opc,rm)=
	genbyte(mode<<6+opc<<3+rm)
end

function makeam(int m,s,d)int=
!convert mode, sib, dispsize into 32-bit value:
! ssssssss ssssssss mmmmmmmm dddddddd
!return m<<16+s<<8+d
!note: s can be -1, so allow to extend into sign bit:
	return s<<16+m<<8+d
end

proc do_lea(operand a,b)=
	int regcode, am

	unless a.mode=a_reg and b.mode=a_mem then
		gerror("LEA not reg/mem")
	end

	if a.size<4 then gerror("LEA size error") fi

	genrrm(0x8D, a, b)
end

proc do_movsx(operand a,b,int opc)=
!opc=B6 for movzx, and BE for movsx
	int am, regcode

	if a.mode<>a_reg then gerror("movsx not reg") fi

	if a.size=8 and b.size=4 then
		if opc=0xBE then
			do_movsxd(a,b)
		else						!movsx 4->8 bytes, do normal move 4->4
			a:=regtable[a.reg,4]
			do_mov(a,b)
		fi
		return
	fi

	if a.size=1 or a.size<=b.size then gerror("movsx size error") fi
	if opc=0xB6 and b.size=4 then gerror("movsx 4=>8 bytes?") fi

	case b.mode
	when a_reg then
	when a_mem then
		if b.size=0 then gerror("movsx need size prefix") fi
		if b.size=8 then gerror("movsx size 8") fi
	else
		gerror("movsx not reg/mem")
	esac

	genrrm(0x0F<<8+(b.size=1|opc|opc+1), a, b)
end

proc checkhighreg(operand a)=
	if a.mode=a_reg then
		case a.reg
		when r5,r3,r14,r15 then
			rex ior:=0x40
		esac
	fi
end

proc do_exch(operand a,b)=
	int regcode, am

	if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then		!simple r0/reg
		if a.reg<>r0 then				!get a to be r0
			swap(a,b)
		fi
		if a.size<>b.size then gerror("exch size") fi

		setopsize(a)
		regcode:=getregcode(b.reg, bmask)
		genrex()
		genbyte(0x90+regcode)
		return
	fi

	if a.mode=a_mem then swap(a,b) fi

	unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then gerror("exch opnds") end
	if b.size=0 and b.mode=a_mem then b.size:=a.size fi
	if a.size<>b.size then gerror("exch size") fi

	genrrm((a.size=1|0x86|0x87), a, b)
end

proc do_movsxd(operand a,b)=
	int regcode, am

	if b.mode=a_mem and b.size=0 then b.size:=4 fi

	if a.size<>8 or b.size>4 then gerror("movsxd size") fi

	if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
		gerror("movsxd opnds")
	fi

	genrrm(0x63, a, b)
end

proc do_imul2(operand a,b)=
	int regcode, am, opc, dispsize:=0
	int64 value

	if a.mode<>a_reg then
		gerror("imul2 opnds")
	fi
	if b.size=0 then b.size:=a.size fi
	if a.size=1 then gerror("imul2 byte") fi

	case b.mode
	when a_reg,a_mem then
		if a.size<>b.size then gerror("imul2 size") fi

		genrrm(0x0F'AF, a, b)

	when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
		if b.labeldef then gerror("mul/label") fi
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
		gerror("imul2 opnds")
	esac
end

proc do_shift(operand a,b,int code)=
	int w,opc,needdisp

	if a.mode<>a_reg and a.mode<>a_mem then gerror("shift opnds1?") fi
	if b.labeldef then gerror("shift/label") fi
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
		if b.reg<>r10 or b.size<>1 then gerror("cl or b10 needed") fi
		opc:=0xD2+w
	else
		gerror("shift opnds2?")
	esac

	genxrm(opc, code, a)

	if needdisp then genbyte(b.value); fixrip(1) fi
end

proc do_test(operand a,b)=
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
		gerror("test opnds")
	fi

end

proc do_loop(operand a,int opc)=
	int offset

	offset:=getrel32(a.labeldef,getcurrdatalen(9)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			gerror("loop jmp out of range")
		fi
		genbyte(opc)
		genbyte(offset)
	else
		gerror("Can't do loopxx fwd jump")
	fi
end

proc do_jcxz(operand a,int opsize)=
	int offset

	offset:=getrel32(a.labeldef,getcurrdatalen(10)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			gerror("jcxz jmp out of range")
		fi
		if opsize=4 then genbyte(0x67) fi
		genbyte(0xE3)
		genbyte(offset)
	else
		gerror("Can't do jcxz fwd jump")
	fi
end

proc do_setcc(operand a,b)=
!a is cond
!b is byte reg/mem
	int am

	if b.mode not in [a_reg, a_mem] or b.size>1 then gerror("setcc opnd/size") fi

	genxrm(0x0F'90+a.value, 0, b)
end

proc do_arithxmm(operand a,b,int prefix,opc)=
	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("arithxmm opnds")
	fi

	if prefix then genbyte(prefix) fi
	genrrm(0x0F<<8+opc, a, b)
end

proc do_logicxmm(operand a,b,int opc,size)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("logicxmm opnds")
	fi

	if size=8 then genbyte(0x66) fi

	genrrm(0x0F<<8+opc, a, b)
end

proc do_convertfloat(operand a,b,int prefix)=
!cvtss2sd and cvtsd2ss
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("convertfloat opnds")
	fi
	genbyte(prefix)
	nowmask:=1
	genrrm(0x0F'5A, a,b)
end

proc do_fix(operand a,b,int prefix,opc)=
	int am, regcode



	if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("fix opnds")
	fi

	checksize(a, 4, 8)
	

!	if prefix=0xF2 and a.size<>8 or prefix=0xF3 and a.size<>4 then gerror("Fix size?") fi
	b.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	genrrm(0x0F<<8+opc, a, b)
end

proc do_float(operand a,b,int prefix)=
!cvtss2si and cvtsd2si
	if a.mode<>a_xreg or (b.mode<>a_reg and b.mode<>a_mem) then
		gerror("float opnds")
	fi

	checksize(b, 4, 8)
!
	a.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	usesizeb:=1
	genrrm(0x0F'2A, a, b)
end

proc do_call(operand a)=
	int am, regcode
	case a.mode
	when a_imm then
		genbyte(0xE8)
		genrel32(a)
	else				!indirect call
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			gerror("call[]size")
		esac

		genxrm(0xFF, 2, a)

	esac
end

proc do_jmp(operand a,ref mclrec m)=
	int am, regcode, offset, shortjmp

	case a.mode
	when a_imm then
		offset:=getrel32(a.labeldef,getcurrdatalen(11)+1)
		if offset<0 and offset>-126 then
			genbyte(0xEB)
			genbyte(offset)
		else
			shortjmp:=0
			if offset>0 then				!fwd jump
!check if destlabel occurs within next 8 instrs, then likely to need short disp
				shortjmp:=checkshortjump(m,a.labeldef)
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
			gerror("jmp[]size")
		esac

		genxrm(0xFF, 4, a)
	esac

end

function getcurrdatalen(int id)int=
!I think that zdata-seg is only likely when id=6

	if currseg=zdata_seg then
		return ss_zdatalen
	fi
	return bufferlength(currdata)
end

proc do_cmovcc(operand c,a,b)=
	int am, regcode
	if a.size<>b.size and b.size then
		gerror("Opnd size mismatch")
	fi
	if a.size=1 then gerror("cmov/byte") fi

	genrrm(0x0F'40+c.value, a, b)
end

proc do_fmem(operand a, int freal, code)=
!do fld/fild/fst/fstp/fist,fistp
!freal=1 for fld/etc, 0 for fild etc
!code is middle 3 bits of 2nd byte: 0=load, 2=store, 3=store+pop
	int am, regcode, mf

	if a.mode<>a_mem then
		gerror("fmem/not mem")
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
				gerror("r80 not allowed")
			esac
		else
			CPL "SIZE=",A.SIZE
			gerror("fmem size")
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
				gerror("fst i64?")
			esac
		else
			gerror("fmem int size")
		esac
	fi
	
	genxrm(0xD9+mf<<1, code, a)
end

function getr32bits(real x)int=
!when x is real, convert to real32 then return 32-bit bit pattern
	real32 sx:=x
!	return int32@(sx)
	return int@(sx)
end

proc genrel8(operand a)=
!a is a known fwd reference, and expected to be <=127 bytes
	ref strec d

	d:=a.labeldef

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
		genbyte(0)
	else								!external symbol
		gerror("genrel8")
	fi
end

function checkshortjump(ref mclrec m,ref strec d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
	int n

	n:=0
	m:=m.nextmcl
	while m and n<=8 do
		++n
		if m.opcode=m_labelx and m.a.labeldef=d then
			return 1
		fi

		m:=m.nextmcl
	od

	return 0
end

function addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
	ref fwdrec q

	q:=pcm_alloc(fwdrec.bytes)
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

proc do_popcnt(operand a,b)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi

	f3override:=1
	genrrm(0x0F'B8, a, b)
end

proc do_bsf(operand a,b, int opc)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi
	if a.size<>b.size then gerror("bsf size") fi

	genrrm(0x0F<<8+opc, a, b)
end

proc extendsymboltable=
	ref[]ref strec oldsymboltable
	int oldsymboltablesize

	oldsymboltablesize:=ss_symboltablesize
	oldsymboltable:=ss_symboltable

	ss_symboltablesize*:=2
	CPL "EXTENDING SYMBOL TABLE TO",SS_SYMBOLTABLESIZE

	ss_symboltable:=pcm_alloc(ref void.bytes*ss_symboltablesize)

	for i:=1 to ss_nsymbols do
		ss_symboltable^[i]:=oldsymboltable^[i]
	od

	pcm_free(oldsymboltable,ref void.bytes*oldsymboltablesize)
end

proc do_pcmpistri(operand a,b,int c,opc)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("pcmpistrx opnds")
	fi

	a.size:=b.size:=8

	sizeoverride:=1
	nowmask:=1
	genrrm(0x0F'3A'63, a, b)
	genbyte(c)
end

proc genxrm(int opcode, code, operand b)=
!deal with /d instructions, where code = 0..7
	int am
!	[0..7]byte opbytes @opcode

	setopsize(b)

	am:=newgenrm(0, code, b, 0)

	case currmcl.opcode
	when m_push, m_pop then rex.[wbit]:=0
	esac


!	if opbytes[2] then genbyte(opbytes[2]) fi		!extra opcodes will not be 0
!	if opbytes[1] then genbyte(opbytes[1]) fi
	if opcode iand 0xFF0000 then genbyte(opcode>>16 iand 255) fi
	genrex()
	if opcode iand 0xFF00 then genbyte(opcode>>8 iand 255) fi

	genbyte(opcode)
	genamode(b,am)
end

proc genrrm(int opcode, operand a, b)=
!deal with /r instructions; rrm = reg,reg/mem
!opcode represents 1, 2 and maybe 3-byte(?) opcodes, with last in lsb place
!a is a register operand, b is a register or memory operand, always
!when data direction is the other way, as in mov reg/mem, reg, then reverse operands
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
	int am
!	[0..7]byte opbytes @opcode

	setopsize(a)
	if usesizeb then				!wmask comes from b
		rex.[wbit]:=0
		if b.size=8 then rex ior:=wmask fi
	fi

	checkhighreg(a)

	am:=newgenrm(a.reg, 0, b, a.mode=a_xreg)


!	if opbytes[2] then genbyte(opbytes[2]) fi		!extra opcodes will not be 0
!	if opbytes[1] then genbyte(opbytes[1]) fi

	if opcode iand 0xFF0000 then genbyte(opcode>>16 iand 255) fi
	genrex()
	if opcode iand 0xFF00 then genbyte(opcode>>8 iand 255) fi

	genbyte(opcode)
	genamode(b,am)
end

function getregcode(int reg, int mask, isxreg=0)int regcode=
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
		unless -128<=value<=255 then gerror("exceeding byte value") end

	when 2 then
		unless -32768<=value<=65535 then gerror("exceeding word16 value") end
	else
		unless -0x8000'0000<=value<=0xFFFF'FFFF then gerror("2:exceeding word32 value") end
	esac
end

func newgenrm(int reg, opc, operand b, int isxreg=0)int=
!reg =  0:	opc is a 3-bit code that goes in reg field of mod-reg-rm
!reg >= r0:	reg is an M-register code, which is converted to a machine reg encoding
!			of 3 bits (to go in middle of modrm byte), and may set rex.r bit for high
!			regs; opc is 0 in this case
!
!b is operand containing rhs reg value, or is mem operand using base/index regs and addr
!For now, return same encoded value as old genrm

	static []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
	int mode, rm, scale, dispsize, needsib, sib, index, base
	int regix, code, ismem

	mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
	scale:=0				!0=modrm only; 1/2/4/8 means sib used
	dispsize:=0
	needsib:=0
	sib:=-1
	ismem:=0				!0/1/2 means no mem/const memaddr/symbol

	if b.mode=a_mem and b.addrsize=4 then
		addroverride:=1
	fi

!deal with first reg/opc field

	if reg then				!else use opc as-is
		opc:=getregcode(reg, rmask, isxreg)
	fi


	case b.mode
	when a_reg, a_xreg then			!modrm can only ref to a single register
		rm:=getregcode(b.reg, bmask, b.mode=a_xreg)
		checkhighreg(b)

		return makeam(makemodrm(3,opc,rm), sib, dispsize)

	when a_mem then
		ismem:=1
		if b.labeldef then ismem:=2 fi
	else
CPL =OPNDNAMES[B.MODE]
		gerror("genrm not mem")
	esac

	reg:=b.reg
	regix:=b.regix

	if reg=regix=0 then						!address only
		mode:=0
		rm:=4
		scale:=1
		index:=4
		base:=5
		dispsize:=4

	elsif b.scale<=1 and regix=0 then			!simple address mode (no sib)
		dispsize:=getdispsize(b,0)

		if dispsize then
			mode:=(dispsize=1|1|2)
		fi

		rm:=base:=getregcode(reg, bmask)

		if rm<>4 then
			if rm=5 and dispsize=0 then
				mode:=1; dispsize:=1
			fi
			index:=0
		else
			index:=4				!means no index
			scale:=1				!force sib

		fi
	elsif regix and reg=0 then
		dispsize:=4
		mode:=0
		rm:=4
		scale:=(b.scale|b.scale|1)
		base:=5
!		index:=regcodes[regix]
		index:=getregcode(regix, xmask)
		if regix=rstack then gerror("Scaled rstack?") fi

	else									!assume regix used; optional reg and disp
		dispsize:=getdispsize(b,0)
		if dispsize then
			mode:=(dispsize=1|1|2)
		fi
		rm:=4

		scale:=(b.scale|b.scale|1)
		if reg=0 then
			base:=5
		else
			if reg in [rframe,r7] and dispsize=0 then
				mode:=1; dispsize:=1
			fi
			base:=getregcode(reg, bmask)
		fi

		if regix=0 then
			index:=4
		else
			index:=getregcode(regix, xmask)
		fi

		if regix and not reg then
			dispsize:=4
		fi

		if regix=rstack and scale>1 then gerror("Can't scale rstack") fi
!
	fi

	if scale then
		sib:=scaletable[scale]<<6 + index<<3 + base
	fi

	if dispsize=4 and ismem then
		if reg or regix then
			if highmem=2 AND ISMEM=2 then
				CPL "Addr32 can't use RIP, line",alineno
!				gerror("Addr32 can't use RIP")
!ELSIF HIGHMEM=2 AND ISMEM=1 THEN
!CPL "ADDR32/ISMEM=1"
!GERROR("ADDR32/ISMEM1")
			fi
		elsif highmem then
			sib:=-2				!-2 instead of -1 signals an rip field
			mode:=0
			rm:=5
		fi
	fi

!OS_GETCH()
	return makeam(makemodrm(mode:mode,opc:opc,rm:rm), sib, dispsize)
end

proc do_mov(operand a,b)=
	int regcode, am, opc, dispsize
	int64 value

!CPL "NEW MOV"
	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			if a.size<>b.size and b.size then gerror("Opnd size mismatch") fi
			genrrm((a.size=1|0x8A|0x8B), a, b)

		when a_imm then
			value:=b.value

			regcode:=getregcode(a.reg, bmask)
			setopsize(a)
			if b.labeldef and a.size<=2 then gerror("mov imm?") fi

			case a.size
			when 1 then
				unless -128<=value<=255 then gerror("exceeding byte value") end
				genrex()
				genbyte(0xB0+regcode)
				genbyte(value)

			when 2 then
				unless -32768<=value<=65535 then gerror("exceeding word16 value") end
				genbyte(0x66)
				genrex()
				genbyte(0xB8+regcode)
				genword(value)
			when 4 then
				if b.labeldef then
					genrex()
					genbyte(0xB8+regcode)
					genopnd(b,4)
				else
					unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
						CPL value,ref void(value)
						gerror("1:exceeding word32 value")
					end
doreg32:
					genrex()
					genbyte(0xB8+regcode)
					gendword(value)
				fi

			else							!assum 8 bytes
				if b.labeldef then
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
			gerror("MOV REG/??")
		esac
	when a_mem then
		case b.mode
		when a_reg then
			if a.size=0 then a.size:=b.size fi
			if a.size<>b.size and a.size then gerror("Opnd size mismatch") fi
			genrrm((b.size=1|0x88|0x89), b, a)

		when a_imm then
			value:=b.value

			if a.size=0 then a.size:=1 fi
			if b.labeldef and a.size<=2 then gerror("mov imm?") fi
			setopsize(a)
			opc:=(a.size=1|0xC6|0xC7)

			if not b.labeldef then checkimmrange(value, a.size) fi

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
			gerror("MOV MEM/?")
		esac
	else
		gerror("MOV ?/..")
	esac
end

proc do_arith(operand a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
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
			if b.labeldef then
				if a.size<4 then gerror("add imm/size") fi
				genxrm(0x81, code, a)
				genopnd(b,4)
				return
			fi

			x:=b.value
			dispsize:=1
			if a.size=1 then
				opc:=0x80
!				if x not in -128..127 then gerror("Exceeding i8 range") fi
				checkimmrange(x,1)
				if x not in -128..255 then gerror("Exceeding i8/u8 range") fi
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
			gerror("ADD reg,???")
		esac

	when a_mem then
		case b.mode
		when a_reg then
			opc:=code<<3 ior (b.size=1|0x00|0x01)
			genrrm(opc, b, a)

		when a_imm then
			goto doregimm
		else
			gerror("ADD mem,???")
		esac

	else
	cpl opnames[code]
		gerror("Can't add to this opnd")
	esac
end

proc do_movxmm(operand a,b,int size)=
!do movd/movq depending on size being 4 or 8
	int am, regcode, regcode1, regcode2

	if b.size=0 then b.size:=size fi

	case a.mode
	when a_reg then
		case b.mode
		when a_xreg then
			if a.size<>size then gerror("1:movdq size") fi
			b.size:=a.size

			sizeoverride:=1
			genrrm(0x0F'7E, b, a)

		else
			gerror("movdq reg,?")
		esac
	when a_xreg then
		case b.mode
		when a_reg then
			a.size:=b.size
			if b.size<>size then gerror("3:movdq size") fi
			sizeoverride:=1
			genrrm(0x0F'6E, a, b)

		when a_xreg then
			a.size:=b.size
			f3override:=1
			genrrm(0x0F'7E, a, b)

		when a_mem then
!	CPL =B.SIZE
		a.size:=b.size
			if b.size<>size then gerror("3:movdq size") fi
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
			gerror("movdq xreg,?")
		esac

	when a_mem then
		case b.mode
		when a_xreg then
			if a.size and a.size<>size then gerror("5:movdq size") fi

			sizeoverride:=1
			genrrm((size=4|0x0F'7E|0x0F'D6), b,a)

		else
			gerror("movdq mem,?")
		esac
	else
		gerror("movdq opnds")
	esac

end

proc checksize(operand a, int size1=0, size2=0)=
!	[64]char str
	if a.size=0 then gerror("Need size") fi
	if size1 and a.size not in [size1,size2] then
		CPL =A.SIZE
		gerror("Wrong size")
	fi
!		fprint b@str,"Wrong size: need # not #", size, a.size
!		gerror(str)
!	fi
end

proc fixrip(int dispsize)=
	ref byte codeaddr
	ref u32 offsetptr

	if not ripentry then return fi
	if dispsize not in [1,2,4] then
		gerror("fixrip disp?")
	fi
	ripentry.immsize:=dispsize
end

proc do_bswap(operand a)=
	int code
	if a.mode<>a_reg or a.size<4 then gerror("bswap reg>") fi

	setopsize(a)

	code:=getregcode(a.reg, bmask)

	genrex()
	genbyte(0x0F)
	genbyte(0xC8 + code)
end

proc do_movdqx(operand a, b, int prefix)=
	prefix:=prefix<<16 + 0x0F<<8

	if a.size=0 then a.size:=16 fi
	if b.size=0 then b.size:=16 fi

	if a.mode=a_mem then
		genrrm(prefix+0x7F, b, a)
	else
		genrrm(prefix+0x6F, a, b)
	fi
end

proc do_dshift(operand a, b, int c, opc)=
	if a.size=0 then a.size:=b.size fi
	if a.size<>b.size or a.size<=1 then gerror("dshift/size") fi

	sizeoverride:=0
	genrrm(0x0F<<8+opc, b, a)
	genbyte(c)
end

=== aa_lex.m 0 0 5/16 ===
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
global ref strec lxsymptr		!set by lookuplex()

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
	od

	for i to setccnames.len do
		addreservedword(setccnames[i],ksetccsym,setcccodes[i])
	od

	for i to cmovccnames.len do
		addreservedword(cmovccnames[i],kmovccsym,cmovcccodes[i])
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

global proc printhashtable=
	ref strec r
	int count,i

	count:=0
	for i:=0 to lexhashtable.upb do
		r:=lexhashtable[i]
		if R AND r.name then
			count+:=1
!			if not r.ksymbol or eqstring(r.name, "push") then
			if not r.ksymbol then
!				println r.name, =r.htindex, =r.htfirstindex
			fi
		fi
	od
!CPL
!	println count," items in table",hstsize
end

function lookuplex(ichar name,int length=0)int=
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)
	ref strec e

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

		if lxsymptr.namelen=length and memcmp(lxsymptr.name,name,length)=0 then			!match
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
	lxsymptr.namelen:=length
	lxsymptr.symbol:=namesym
	lxsymptr.ksymbol:=0
	lxsymptr.htindex:=j
!CPL "SET HTFIRST",NAME
	lxsymptr.htfirstindex:=firstj
	lxsymptr.moduleno:=currmoduleno
	return 0
end

global proc initsourcefile(ichar source)=
	lxstart:=lxsptr:=cast(source)
	lxlineno:=1
end

global function addnamestr(ichar name)ref strec=
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
=== aa_lib.m 0 0 6/16 ===
const ptrsize=8

!fwdrec dummy1
!valuerec dummy2

global enumdata [0:]ichar opndnames =
	(a_none=0,	$),
	(a_reg,		$),
	(a_imm,		$),
 	(a_mem,		$),		!any memory modes: [d], [R], [R*4+R2+d+imm] etc
 	(a_cond,	$),		!a condition code for jcc/setcc
	(a_xreg,	$),		!xmm register
	(a_string,	$),		!immediate string (for comments)
end

!global type opndrec = record			!24 bytes
!	ref strec labeldef	!nil, or handle of strec for label
!	union
!		int64 value		!const value/extra offset/cond code/string for comments
!		real64 xvalue	!floating point value
!		ref char svalue
!	end
!	byte mode		!a_reg etc, low level operand details
!	byte size		!byte size of operand: 1,2,4,8
!	byte reg		!0, or main register
!	byte regix		!0, or index register
!
!	byte scale		!0, or scale factor for regix
!	byte addrsize	!4 or 8 for a_mem when regs are involved
!	byte valtype	!0 (no value or int) or 'R'/'S'
!	byte spare2
!end

global record mclrec =
    ref mclrec nextmcl
    ref opndrec a,b
    u16 opcode
    u16 c
    u32 lineno
end

!!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!!is needed at strategic points to ensure that are at least n bytes left
!global type dbuffer = record
!	ref byte pstart
!	union
!		ref byte pcurr
!		ref word16 pcurr16
!		ref word32 pcurr32
!		ref word64 pcurr64
!	end
!	ref byte pend
!	int alloc
!end

global int currsegment=0		!

global opndrec dstackopnd
global opndrec dframeopnd

global int labelno=0
global ref opndrec zero_opnd=nil

global ref mclrec mccode, mccodex

strbuffer destv
global ref strbuffer dest=&destv

global [r0..r19, 1..8]ref opndrec regtable

global proc initlib=
	zero_opnd:=genint(0)

int reg,size

	for reg:=r0 to r15 do
		for size:=1 to 8 do
			case size
			when 1,2,4,8 then
				regtable[reg,size]:=genreg0(reg,size)
			esac
		od
	od
	for reg:=r16 to r19 do
		regtable[reg,1]:=genreg0(reg,1)
	od

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0

end

global proc genmc(int opcode,ref opndrec a=nil,b=nil)=	!GENMC
ref mclrec m
int nopnds

!	m:=pcm_allocz(mclrec.bytes)
!	m:=pcm_allocz(mclrec.bytes)
	m:=pcm_alloc(mclrec.bytes)

	m.nextmcl:=nil

!CPL "SET MLINENO",symbolnames[lxsymbol],mclnames[opcode]
	if lxsymbol=eolsym then
		m.lineno:=lxlineno-1
	else
		m.lineno:=lxlineno
	fi

	m.opcode:=opcode

	nopnds:=(a=nil|0|(b=nil|1|2))
	if nopnds=2 and opcode in [m_pcmpistri,m_pcmpistrm] then nopnds:=3 fi

	if nopnds<mclnopnds[opcode] then
		serror("Too few operands")
	elsif nopnds>mclnopnds[opcode] then
		serror("Too many operands")
	fi

	m.a:=a
	m.b:=b

	if mccode then
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

global proc genmcstr(int opcode,ichar s)=	!GENMCSTR
!as genmc but uses a single immediate string operand

	genmc(opcode,genstrimm(s))
end

function newopnd(int mode)ref opndrec=
ref opndrec a

!	a:=pcm_allocz(opndrec.bytes)
	a:=pcm_allocz(opndrec.bytes)
	a.mode:=mode
	return a
end

global function genxreg(int xreg)ref opndrec=		!GENXREG
	ref opndrec a

	a:=newopnd(a_xreg)
	a.reg:=xreg
	a.size:=16
	return a
end

global function genindex(int areg=0,ireg=0,scale=1,ref opndrec x=nil,int size=0,addrsize=8)ref opndrec=		!GENINDEX
!construct a mem address mode
	ref opndrec a

	if x then							!existing operand filled in with value
!	a:=genmem_expr(x)				!fill in label and/or offset
		a:=x
		x.mode:=a_mem
	else
		a:=newopnd(a_mem)
	fi

	a.reg:=areg
	a.regix:=ireg
	a.scale:=scale
	a.size:=size
	a.addrsize:=addrsize
	return a
end

global function writemclblock:ref strbuffer=		!WRITEMCLBLOCK
	int i
	ref mclrec m

	gs_init(dest)

	gs_strln(dest,"MC CODE")

	m:=mccode
	i:=1

	while m do
		writemcl(i,m)
		m:=m.nextmcl
		++i
	od
	return dest			!only used when initstr=1, otherwise caller ignores
end

global proc gencomment(ichar s=nil)=			!GENCOMMENT
	if s=nil then
		genmc(m_blank)
	else
		genmcstr(m_comment,s)
	fi
end

global function genstrimm(ichar s)ref opndrec=			!GENSTRIMM
	ref opndrec a
	a:=newopnd(a_string)
	a.svalue:=s
	return a
end

function getsizetag(int size)ichar=			!GETSIZETAG
	case size
	when 1 then return "b"
	when 2 then return "h"
	when 4 then return "w"
	when 8 then return "d"
	esac
	GERROR("GETSIZETAG?")
!return tostr(size)
	return nil
end

proc writemcl(int index,ref mclrec mcl)=			!WRITEMCL
	[512]char mclstr
	[512]char str
	ichar semi

	strcpy(&.mclstr,strmcl(mcl))
	if mclstr[1]=0 then return fi

	case mcl.opcode
	when m_comment then
		semi:=";"
	else
		semi:=" "
	esac

!sprintf(&.str,"%03d %04d ",semi,index, mcl.lineno)
	print @&.str,semi:"z3",index:"z4",," "!, mcl.lineno

	gs_str(dest,&.str)
	gs_strln(dest,&.mclstr)
end

global function strmcl(ref mclrec mcl)ichar=			!STRMCL
	static [512]char str
	[128]char str2
	int opcode,sizepref

	opcode:=mcl.opcode

	case opcode
	when m_assem then
		return mcl.a.svalue
	when m_blank then
		return ""
	when m_comment then
!	if fshowcomments then
			strcpy(&.str,";")
			strcat(&.str,mcl.a.svalue)
			return &.str
!	fi
!when m_bsource then
!	strcpy(&str,";")
!	strcat(&str,mcl.a.svalue)

!when m_labelname then
!	strcpy(&str,mcl.a.svalue)
!	strcat(&str,":")
!	return &str

	when m_labelx then
		strcpy(&.str,mcl.a.labeldef.name)
		strcat(&.str,":")
		return &.str

	esac

	strcpy(&.str,"		")

	case opcode
	when m_jmpcc then
		strcat(&.str,"j")
		strcat(&.str,condnames[mcl.a.value])

	when m_setcc then
		strcat(&.str,"set")
		strcat(&.str,condnames[mcl.a.value])
	when m_cmovcc then
		strcat(&.str,"cmov")
	strcat(&.str,condnames[mcl.a.value])
	else
		strcat(&.str,mclnames[opcode]+2)
	esac

	ipadstr(&.str,12)

!s+:=tab+tab+leftstr(opcname,10)

	if mcl.a and mcl.b then		!2 operands
		sizepref:=needsizeprefix(mcl.opcode,mcl.a,mcl.b)

		strcat(&.str,stropnd(mcl.a,sizepref))
		strcat(&.str,",	")
		strcat(&.str,stropnd(mcl.b,sizepref))

	elsif mcl.a then								!1 operand
		if mcl.opcode=m_call then
			strcat(&.str,stropnd(mcl.a,0))
		else
			strcat(&.str,stropnd(mcl.a,1))
		fi
!else
!	opnds:=""
	fi

	case opcode
	when m_pcmpistri,m_pcmpistrm then
		fprint @&.str2,", #",mcl.c
		strcat(&.str,&.str2)
	esac

!s+:=opnds

	return &.str
end

global function stropnd(ref opndrec a,int sizeprefix=0)ichar=			!STROPND
	static [256]char str
	ichar plus,s
	int64 value
	ref strec d

	case a.mode
	when a_reg then
		return getregname(a.reg,a.size)
	when a_imm then
!	return STRVALUE(A.LABELDEF,A.VALUE)
		d:=a.labeldef
		value:=a.value
		if d then
			if d.symbol=namedconstsym then
				return inttostr(d.expr.value)
			fi

!		s:=d.name
			s:=GETFULLNAME(d)

			if value then
				if value>0 then
					strcpy(&.str,s)
					strcat(&.str,"+")
					strcat(&.str,inttostr(value))
				else
					strcpy(&.str,s)
					strcat(&.str,inttostr(value))
				fi
				return &.str
			else
				strcpy(&.str,s)
				return &.str
!			return s
			fi
		fi
		if a.valtype=0 then
			return inttostr(value)
		else
			return realtostr(real@(value))
		fi

	when a_mem then
		str[1]:=0
		strcat(&.str,getsizeprefix(a.size,sizeprefix))
		strcat(&.str,"[")
		plus:=""

		if a.reg then
			strcat(&.str,getregname(a.reg,a.addrsize))
			plus:="+"
		fi

		if a.regix then
			strcat(&.str,plus)
			strcat(&.str,getregname(a.regix,a.addrsize))
			plus:="+"
			if a.scale>1 then
				strcat(&.str,"*")
				strcat(&.str,inttostr(a.scale))
			fi
		fi

		if a.labeldef then
			strcat(&.str,plus)
			strcat(&.str,strdef(a.labeldef))
			plus:="+"
		fi

		if a.value>0 then
			strcat(&.str,plus)
			strcat(&.str,inttostr(a.value))
		elsif a.value<0 then
			strcat(&.str,inttostr(a.value))
		fi

		strcat(&.str,"]")
	when a_string then
		if strlen(a.svalue)>=str.len then
!		sprintf(&.str,"\"%s\"","<Long string>")
			print @&.str,"""<Long string>"""
		else
!		sprintf(&.str,"\"%s\"",a.svalue)
			print @&.str,"""",,a.svalue,,""""
		fi

	when a_cond then
		return opndnames[a.value]

	when a_xreg then
		return xgetregname(a.reg)

	else
		return "<BAD OPND>"
	esac

	return &.str
end

function strdef(ref strec def)ichar=			!STRDEF
	if def.symbol=namedconstsym then
		return inttostr(def.expr.value)
	fi
	return getfullname(def)
end

global proc setsegment(int seg)=		!SETSEGMENT
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
	if seg=currsegment then
		return
	fi
	case seg
	when 'D' then genmcstr(m_segment,".data")
	when 'Z' then genmcstr(m_segment,".bss")
	when 'C' then genmcstr(m_segment,".text")
	when 'R' then genmcstr(m_segment,".rodata")
	esac
	currsegment:=seg
!currzdataalign:=curridataalign:=0
	end

	function getsizeprefix(int size,enable=0)ichar=		!GETSIZEPREFIX
	if not enable then return "" fi
	case size
	when 1 then return "byte "
	when 2 then return "word "
	when 4 then return "dword "
	when 8 then return "qword "
!when 0 then return "<no size> "
	when 0 then return ""
	esac
	return "N:"
end

function needsizeprefix(int opcode,ref opndrec a,b)int=		!NEEDSIZEPREFIX

	case opcode
	when m_movsx,m_movzx then
		return 1
	when m_cvtsi2ss,m_cvtsi2sd then
		return 1
	esac

	if a.mode=a_reg or a.mode=a_xreg or b.mode=a_reg or b.mode=a_xreg then
		return 0
	fi
	return 1
end

global function genimm_expr(ref strec d, int64 value, int t, size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
	ref opndrec a

	a:=newopnd(a_imm)
	a.size:=size

	a.labeldef:=d
!CPL =A.LABELDEF, A.VALUE
	a.value:=value
!CPL =A.LABELDEF, A.VALUE
	a.valtype:=t

	return a
end

global function genint(int64 x,int size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
	ref opndrec a

!IF X=0 THEN ++NZEROS FI

	a:=newopnd(a_imm)
	a.size:=size
	a.value:=x

	return a
end

global function genlab(ref strec d,int size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
	ref opndrec a

	a:=newopnd(a_imm)
	a.size:=size
	a.labeldef:=d

	return a
end

global function genmem(ref strec d,int size=4)ref opndrec=
!simple memory operand without registers
	ref opndrec a

	a:=genlab(d,size)
	a.mode:=a_mem
	return a
end

global function genreg0(int reg,size=4)ref opndrec=	!GENREG

	ref opndrec a
	a:=newopnd(a_reg)
	a.reg:=reg
	a.size:=size
	return a
end

global function getfullname(ref strec d)ichar=
	static [256]char str
	ichar ms

	ms:=""
	if d.basedef then
		ms:=d.basedef.name
	fi

!sprintf(&.str,"<%s : #%d &:%8p SYM:%.*s M:%s>",
!	d.name,d.moduleno,d,
!	strlen(symbolnames[d.symbol])-3,symbolnames[d.symbol],
!	ms)

	fprint @&.str,"<# : ## &:# SYM:## M:#>",
		d.name,"#",d.moduleno,d:"8",
		strlen(symbolnames[d.symbol])-3:"v",symbolnames[d.symbol]:".*", ms

	return &.str
	return d.name
end

global function getregname(int reg,size=4)ichar=
	ichar prefix,rs
	static [32]char str

	case reg
	when rnone then return "-"
	when rframe then rs:="frame"
	when rstack then rs:="stack"
!when r16..r19 then
!	rs:=(reg-r15|"0H","1H","10H","11H"|"?")

	else
		rs:=inttostr(reg-r0)
	esac

	case size
	when 1 then prefix:="B"
	when 2 then prefix:="W"
	when 4 then prefix:="A"
	else prefix:="D"
	esac

	strcpy(&.str,prefix)
	strcat(&.str,rs)
	return &.str
end

global function xgetregname(int reg)ichar=
	static [16]char str

!sprintf(&.str,"xmm%d",reg-r0)
	print @&.str,"xmm",,reg-r0

	return &.str
end

global proc adddef(ref strec d)=
	d.nextdef:=modulenamelist
	modulenamelist:=d
end

global proc addimport(ref strec d)=
	ref stlistrec p

!CPL "ADDIMPORT",D.NAME
	p:=pcm_alloc(stlistrec.bytes)
	p.def:=d
	p.nextitem:=globalimportlist
	globalimportlist:=p
end

global proc createlabel(ref strec symptr,int symbol)=
!symptr is a generic st entry
	symptr.symbol:=symbol
	symptr.stindex:=0
	symptr.moduleno:=currmoduleno
	adddef(symptr)
end

global proc createnamedconst(ref strec symptr,ref opndrec expr)=
	symptr.symbol:=namedconstsym
	symptr.expr:=expr
	adddef(symptr)
end

global proc createregalias(ref strec symptr,int regindex, regsize)=
	symptr.symbol:=kregsym
	symptr.ksymbol:=kregsym
	symptr.subcode:=regindex
	symptr.regsize:=regsize

	adddef(symptr)
end

global proc createxregalias(ref strec symptr,int regindex)=
	symptr.symbol:=kxregsym
	symptr.ksymbol:=kxregsym
	symptr.subcode:=regindex

	adddef(symptr)
end

global proc gerror(ichar mess)=
	println "\nSS code gen error:",mess
	println "On line:", alineno
	println
	stop 1
end

global proc serror(ichar mess)=
	println "\nSyntax error: '",,mess,,"' on line",lxlineno,moduletable[currmoduleno].name
	stop 1
end

global proc serror_s(ichar mess, param)=
	[256]char str
	sprintf(&.str,mess, param)
	serror(&.str)
end

function inttostr(int64 a)ichar=
	static [64]char str

!sprintf(&.str,"%lld",a)
	getstrint(a,&.str)
	return &.str
end

function realtostr(real a)ichar=
	static [64]char str
!sprintf(&.str,"%f",a)
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

!global proc buffercheck(ref dbuffer a,int n=1024)=
global proc buffercheck(ref dbuffer a,int n=1024)=
	while a.pend-a.pcurr<n do
		bufferexpand(a)
	od
end

global function bufferlength(ref dbuffer a)int=
	return a.pcurr-a.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=
!IF OFFSET>=BUFFERLENGTH(A) THEN
!	GERROR("BUFFERELEMPTE/OVERFLOW")
!FI

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

=== aa_objdecls.m 0 0 7/16 ===
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

global enumdata [0:]ichar coffscopenames =
	(cofflocal_scope=0,	$),
	(export_scope,		$),
	(import_scope,		$),
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

global record importrec = 		!details about all imported symbols
	ref strec def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	ref strec def				!full st entry
	ichar name					!name of symbol (extracted from lib.name if needed)
end

global record dllrec =			!all imported libraries
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

=== aa_mcxdecls.m 0 0 8/16 ===
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

global enumdata []ichar segmentnames =
	(code_seg,		"code"),
	(idata_seg,		"idata"),
	(zdata_seg,		"zdata"),
	(rodata_seg,	"rodata"),
	(impdata_seg,	$),
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
=== aa_parse.m 0 0 9/16 ===
ref strec exprlabeldef
int64 exprvalue
int exprtype

global proc readmodule(int moduleno)=
	ref strec symptr
	int sym

!CPL "RM1"
	initsourcefile(moduletable[moduleno].source)
!CPL "RM2"

	lxsymbol:=eolsym

	genmc(m_segment,genint(code_seg))
!CPL "RM3"

!INT NN:=0
!DO
!++NN
!CPL "RM4"
! LEX()
!CPL "RM5"
!
!EXIT WHEN LXSYMBOL=EOFSYM
! OD
!CPL =NN,"TOKENS"
!RETURN
!
	while lxsymbol=eolsym do

		lex()

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
					createnamedconst(symptr,readexpression())
				esac

			when colonsym,dcolonsym then
				createlabel(symptr,(sym=colonsym|localsym|exportedsym))
				genmc(m_labelx, genlab(symptr))
				symptr.reftype:=fwd_ref
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
				serror_s("Redefining label as const: %s",symptr.name)
			when colonsym,dcolonsym then
				symptr.fwdrefs:=nil
				genmc(m_labelx, genlab(symptr))
				symptr.symbol:=(lxsymbol=colonsym|localsym|exportedsym)
				symptr.reftype:=fwd_ref
				lxsymbol:=eolsym
				redoloop
			else
				serror("Instruction expected")
			esac

		when importedsym then
			serror_s("Defining imported name: %s",lxsymptr.name)
		when localsym, exportedsym then
			serror_s("Redefining symbol: %s",lxsymptr.name)
		when namedconstsym then
			serror_s("2:Const redefined: %s",lxsymptr.name)

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
	ref strec d

	d:=modulenamelist
	while d do
		if d.symbol=fwdlocalsym then
			println "Undefined:",padstr(d.name,20)
			++nundefined
		fi
		d:=d.nextdef
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
	int opcode
	ref opndrec a,b,c

	opcode:=lxsubcode

	lex()

	switch opcode
	when m_db, m_dw, m_dd, m_dq then
		do
			if lxsymbol=stringconstsym then
				a:=genstrimm(lxsvalue)
				lex()
				genmc(opcode,a)
			else
				a:=readoperand()
				genmc(opcode,a)
			fi
			if lxsymbol=commasym then
				lex()
			else
				exit
			fi
		od
	when m_segment then
		checksymbol(ksegnamesym)
		genmc(m_segment,genint(lxsubcode))
		lex()

	when m_isegment then
		genmc(m_segment, genint(idata_seg))
	when m_zsegment then
		genmc(m_segment, genint(zdata_seg))
	when m_csegment then
		genmc(m_segment, genint(code_seg))

	when m_imul3 then
		a:=readoperand()
		checksymbol(commasym)
		lex()
		b:=readoperand()
		checksymbol(commasym)
		lex()
		c:=readoperand()
		SERROR("IMUL3 CAN'T DO 3 OPNDS")

	when m_pcmpistri,m_pcmpistrm, m_shld, m_shrd then
		a:=readoperand()
		checksymbol(commasym)
		lex()
		b:=readoperand()
		checksymbol(commasym)
		lex()
		c:=readoperand()
		if c.mode<>a_imm then serror("pcmpistr/not int") fi
		genmc(opcode,a,b)
		mccodex.c:=c.value

	when m_proc then
		while lxsymbol<>eolsym do lex() od
	else
		a:=b:=nil
		if lxsymbol<>eolsym then
			a:=readoperand()
			if lxsymbol=commasym then
				lex()
				b:=readoperand()
			fi
		fi 

		genmc(opcode,a,b)
	end

end

proc readcondinstr(int opc)=
	ref opndrec a,b

	a:=genint(lxsubcode)
	lex()
	b:=readoperand()

	if lxsymbol=commasym and opc=m_cmovcc then		!ignore dest
		genmc(m_param,b)							!store extra param as separate instr

		lex()
		b:=readoperand()
	fi

	genmc(opc,a,b)
end

function readoperand:ref opndrec=
!position at start of operand
!read reg, expression or complex operand, and return an opndrec
	ref opndrec p
	int size

	case lxsymbol
	when kregsym then
		p:=regtable[lxsubcode, lxsymptr.regsize]
		lex()
		return p
	when lsqsym then
		lex()
		return readaddrmode(0)
	when kxregsym then
		p:=genxreg(lxsubcode)
		lex()
		return p
	when kprefixsym then
		size:=lxsubcode
		lex()
		checksymbol(lsqsym)
		lex()
		return readaddrmode(size)

	else
		return readexpression()
	end
	return nil
end

function readexpression:ref opndrec=
	ref strec labelx
	int64 valuex
	int typex

	readterm()

	docase lxsymbol
	when addsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("add real") fi
		exprvalue+:=valuex

	when subsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("sub real") fi
		exprvalue:=valuex-exprvalue
	when mulsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("add real") fi
		exprvalue*:=valuex

	else
		exit
	end

	return genimm_expr(exprlabeldef,exprvalue,exprtype)
end

proc readterm=
!read term into exprlabeldef/exprvalue
	ref strec symptr
	real x
	exprlabeldef:=nil
	exprvalue:=0
	exprtype:=0

	switch lxsymbol
	when fwdlocalsym, localsym, exportedsym then
		exprlabeldef:=lxsymptr
		lex()
		if lxsymbol=mulsym then		!is extern name
			serror("* applied to non-extern label or applied inconsistently")
		fi

	when importedsym then
		exprlabeldef:=lxsymptr
		lex()
		if lxsymbol<>mulsym then		!is extern name
CPL LXSYMPTR.NAME
			serror("* missing or applied inconsistently")
		fi
		lex()

	when namedconstsym then
		exprlabeldef:=lxsymptr.expr.labeldef
		exprvalue:=lxsymptr.expr.value
		exprtype:=lxsymptr.expr.valtype

!		p:=lxsymptr.value
		lex()
	when namesym then
		symptr:=lxsymptr
		exprlabeldef:=symptr
		lex()
		if lxsymbol=mulsym then		!is extern name
			createlabel(symptr,importedsym)
			lex()
		else
			createlabel(symptr,fwdlocalsym)
		fi

	when intconstsym then
		exprvalue:=lxvalue
		lex()
	when realconstsym then
		exprvalue:=int64@(lxxvalue)
		exprtype:='R'
		lex()

	when subsym then
		lex()
		readterm()
		if not exprlabeldef then
			if not exprtype then
				exprvalue:=-exprvalue
			else
				x:=-(real@(exprvalue))
				exprvalue:=int64@(x)
			fi
		else
			serror("neg/label")
		fi
	when addsym then
		lex()
		readterm()

	else
!PS("RT?")
		serror("READTERM")
	end
!CPL "RT", EXPRTYPE


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

function readaddrmode(int size)ref opndrec=
![" just seen, positioned at next symbol
!syntax: [Reg+Reg*scale+expr], all items optional, but at least one must be present
!read optional reg, index reg, scale factor, label, and offset
!size is 0, or is 1,2,4,8 when an override was used
	int reg,regsize,scale,regix, regixsize, scaleix
	ref opndrec x
	ref opndrec p

	reg:=regix:=0
	regsize:=regixsize:=0
	scale:=scaleix:=0
	x:=nil

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
					x:=readexpression()
				esac

			else
				x:=readexpression()
			fi
		when subsym then
			x:=readexpression()
		esac
	else
		x:=readexpression()
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

	p:=genindex(areg:reg, ireg:regix, scale:scaleix, x:x, size:size,
		addrsize:(regsize=4 or regixsize=4|4|8))

!CPL "RAM", SIZE, P.SIZE, =addrsize

	return p
end
=== aa_tables.m 0 0 10/16 ===
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

	(eqsym,				$),		! =

	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen

	(hashsym,			$),		! #

	(intconstsym,		$),		! 123 64 bits signed
	(realconstsym,		$),		! 123.4 64 bits
	(stringconstsym,	$),		! "ABC"

	(namesym,			$),		! raw name
	(namedconstsym,		$),		! name = expr
	(fwdlocalsym,		$),		! name
	(localsym,			$),		! name:
	(importedsym,		$),		! name*
	(exportedsym,		$),		! name:

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

global enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_comment,			$,		0,		0),		!
	(m_blank,			$,		0,		0),		!
	(m_end,				$,		0,		0),		!

	(m_labelx,			$,		1,		0),		!
	(m_nop,				$,		0,		0x90),		!
	(m_param,			$,		1,		0),		!
	(m_assem,			$,		1,		0),		!
	(m_proc,			$,		1,		0),		!

	(m_mov,				$,		2,		0),		!
	(m_newmov,			$,		2,		0),		!
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
	(m_retn,			$,		1,		0),		!
	(m_leave,			$,		0,		0xC9),		!

	(m_jmp,				$,		1,		0xE9),	!
	(m_jmpcc,			$,		2,		0),		!
	(m_xchg,			$,		2,		0),		!

	(m_add,				$,		2,		0),		!
	(m_sub,				$,		2,		5),		!
	(m_adc,				$,		2,		2),		!
	(m_sbb,				$,		2,		3),		!
	(m_and,				$,		2,		0x04),	!
	(m_or,				$,		2,		0x01),	!
	(m_xor,				$,		2,		0x06),	!

	(m_newadd,			$,		2,		0),		!
	(m_newsub,			$,		2,		5),		!

	(m_test,			$,		2,		0),		!

	(m_imul,			$,		1,		5),		!
	(m_mul,				$,		1,		4),		!
	(m_imul2,			$,		2,		0),		!
	(m_imul3,			$,		3,		0),		!

	(m_idiv,			$,		1,		7),		!
	(m_div,				$,		1,		6),		!


	(m_cmp,				$,		2,		0x07),	!

	(m_shl,				$,		2,		0x04),	!
	(m_sar,				$,		2,		0x07),	!
	(m_shr,				$,		2,		0x05),	!
	(m_rol,				$,		2,		0x00),	!
	(m_ror,				$,		2,		0x01),	!
	(m_rcl,				$,		2,		0x02),	!
	(m_rcr,				$,		2,		0x03),	!

	(m_neg,				$,		1,		3),		!
	(m_not,				$,		1,		2),		!

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

	(m_sqrtsd,			$,		2,		0x51),	!
	(m_sqrtss,			$,		2,		0x51),	!
	(m_addss,			$,		2,		0x58),	!
	(m_subss,			$,		2,		0x5C),	!
	(m_mulss,			$,		2,		0x59),	!
	(m_divss,			$,		2,		0x5E),	!

	(m_addsd,			$,		2,		0x58),	!
	(m_subsd,			$,		2,		0x5C),	!
	(m_mulsd,			$,		2,		0x59),	!
	(m_divsd,			$,		2,		0x5E),	!

	(m_comiss,			$,		2,		0),		!
	(m_comisd,			$,		2,		0),		!
	(m_xorpd,			$,		2,		0x57),	!
	(m_xorps,			$,		2,		0x57),	!
	(m_andpd,			$,		2,		0x54),	!
	(m_andps,			$,		2,		0x54),	!
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

	(m_pcmpistri,		$,		3,		0x63),		!
	(m_pcmpistrm,		$,		3,		0x62),		!

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

	(m_segment,			$,		1,		0),		!
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

	(m_halt,			$,		0,		0xF4),	!
end

global enumdata [0:]ichar regnames, [0:]byte regcodes =
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

global const rframe = r14
global const rstack = r15


!I use my own register designations Dn, An, Wn, Bn (8,4,2,1 bytes),
!which have a more sensible order than the official ones.
!The mapping is shown against Dn. Some (not all) of the official register
!names are used too

!Regindex is the ordinal value used to represent the register: 1..16
!This table is intended for initialising the global symbol table

global tabledata []ichar dregnames, []byte regsizes, []byte regindices =
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

global []ichar fregnames = (
	"st0",
	"st1",
	"st2",
	"st3",
	"st4",
	"st5",
	"st6",
	"st7")

global []ichar mregnames = (
	"mmx0",
	"mmx1",
	"mmx2",
	"mmx3",
	"mmx4",
	"mmx5",
	"mmx6",
	"mmx7")

global enumdata [0:]ichar condnames =

	(ov_cond	= 0,	"o"),
	(nov_cond	,	"no"),

	(ltu_cond	,	"b"),
	(geu_cond	,	"ae"),

	(eq_cond	,	"z"),
	(ne_cond	,	"nz"),

	(leu_cond	,	"be"),
	(gtu_cond	,	"a"),

	(s_cond		,	"s"),
	(ns_cond	,	"ns"),

	(p_cond		,	"p"),
	(np_cond	,	"np"),

	(lt_cond	,	"l"),
	(ge_cond	,	"ge"),

	(le_cond	,	"le"),
	(gt_cond	,	"g"),
end

global tabledata []ichar jmpccnames, []byte jmpcccodes =
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

global tabledata []ichar setccnames, []byte setcccodes =
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
	("setc",	ltu_cond),
	("setnc",	geu_cond),
end

global tabledata []ichar cmovccnames, []byte cmovcccodes =
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
	("cmovc",	ltu_cond),
	("cmovnc",	geu_cond),
end

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

global enumdata [0:]ichar reftypenames =	!use during pass2
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end
=== aa_writeexe.m 0 0 11/16 ===
!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order:
! initsectiontable()
! genexe()
! writeexe(filename)

[maxsearchlibs]int64 libinsttable
[maxsearchlibs]ichar libinstnames
[maxsearchlibs]int libnotable			!index into dlltable

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
const sectionalign = 4096
const exe_imagebase = 0x40'0000
const dll_imagebase = 0x1000'0000
global word imagebase

int imagesize
int filesize
ref[]int64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
ref strec stentrypoint				!symbol to be the entry point
ref strec stentrypoint2
ref strec stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

ref byte importdir				!allowed section data for import directort in .idata

!global const maximports = 3000
global const maximports = 1000
global [maximports]importrec importtable
global int nimports

global const maxexports = 3000
global [maxexports]exportrec exporttable
[maxexports]int sortindex
global int nexports
ichar dllfilename
int isdll

global const maxlibs = 50
global [maxlibs]dllrec dlltable
global int ndlls

global ref byte datastart
global ref byte dataptr
global ichar userentrypoint

int exportdirvirtaddr
int exportdirvirtsize
int exportdiroffset				!from start of imp dir

int blockdirvirtaddr
int blockdirvirtsize
int blockdiroffset

export proc genexe(ichar entrypoint, outfile, int dodll)=
!manipulate the ss data to fill in all the details needed for exe format
	int offset
	ref byte codeaddr				!mem address of start of code seg
	ref u32 offsetptr

	dllfilename:=outfile
	isdll:=dodll

	imagebase:=(isdll|dll_imagebase|exe_imagebase)

	userentrypoint:=entrypoint
	loadlibs()
	scanst()				!build dll/import tables

	getoffsets()
	relocdata(&sectiontable[csect],0)
	relocdata(&sectiontable[dsect],1)

	codeaddr:=bufferelemptr(sectiontable[csect].data, 0)

	if highmem then
		println "Doing RIP relocs..."

		ref riprec pr

		pr:=riplist
		while pr, pr:=pr.next do
			offsetptr:=ref u32(codeaddr+pr.offset)
			offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)
			offsetptr^:=offset

	
		od
	fi
end

export proc writeexe(ichar outfile, int dodll)=
	imagefileheader header
	optionalheader optheader
	int offset,i
	int64 aa

	dllfilename:=outfile
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

	if fverbose then
		PRINTLN "Writing file:",outfile
	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

proc loadlibs=
!load library instances
	int i
	int64 hinst
	ichar file
	[300]char filename

	for i to nsearchlibs do
		strcpy(&.filename,searchlibs[i])
		strcat(&.filename,".dll")
		hinst:=os_getdllinst(&.filename)
		if hinst=0 then
			println searchlibs[i]
			println &.FILENAME
			gerror("Can't load search lib")
		fi
		libinsttable[i]:=hinst
		libinstnames[i]:=pcm_copyheapstring(&.filename)
	od
end

export proc initsectiontable=
!set up the section table

	sectiontable[csect].name:=".text"
	sectiontable[csect].segtype:=code_seg
	sectiontable[csect].data:=ss_code
	sectiontable[csect].virtsize:=bufferlength(ss_code)

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

!function roundtoblock(int n,align)int=
!!round up n until it is a multiple of filealign (which is a power of two)
!!return aligned value. Returns original if already aligned
!	if n iand (align-1)=0 then return n fi
!
!	return n+(align-(n iand (align-1)))
!end

function extractlibname(ichar name, int &libno,moduleno)ichar=
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
			if ndlls>=maxlibs then gerror("Too many libs") fi
			libno:=++ndlls

			dlltable[libno].name:=pcm_copyheapstring(&.str)
			dlltable[libno].nprocs:=1
			return (name2|name2|s+1)
		fi

		++s
	od

!do explicit search
	int n

	for i:=1 to nsearchlibs do
		if os_getdllprocaddr(libinsttable[i],name) then
			n:=i
			exit				!don't need the actual address; just whether it exists
		fi
	else
		println name,moduletable[moduleno].name
		gerror("Can't find external function")
	od

!found in search lib n
	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	fi

!first use of this lib
	strcpy(&.str,searchlibs[n])
	strcat(&.str,".dll")
	if ndlls>=maxlibs then gerror("2:Too many libs") fi
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
	ref strec d
	ichar name, libname

	for i:=1 to ss_nsymbols do

		d:=ss_symboltable^[i]
		case d.symbol
		when importedsym then
			if nimports>=maximports then gerror("genexe: Too many imports") fi
			++nimports

			name:=extractlibname(d.name,libno,d.moduleno)

			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		when exportedsym then
			if userentrypoint then
				if eqstring(d.name,userentrypoint) then
					stentrypoint:=d
				fi
			else
!				if eqstring(d.name,"main") and not isdll then
				if eqstring(d.name,"main") then
!				if eqstring(d.name,"main") or eqstring(d.name, "fred")then
					stentrypoint:=d
				elsif eqstring(d.name,"start") and not isdll then
					stentrypoint2:=d
				elsif eqstring(d.name,"dllmain") and isdll then
					stentrypoint:=d
				fi
			fi

			if stentrypoint and stentrypoint.segment<>code_seg then
				gerror("Entry point not in code seg")
			fi

!PRINTLN =SEGMENTNAMES[STENTRYPOINT.SEGMENT]

			if nexports>=maxexports then gerror("gendll: Too many exports") fi
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=d.name

		esac
	od
end

proc relocdata(ref sectionrec s,int isdata)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref word32 p32
	ref word64 p64
	ref strec d
	word thunkoffset
	int offset,index,iatoffset

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable^[r.stindex]
		index:=d.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset

		case r.reloctype
		when rel32_rel then
			if d.symbol<>importedsym then
				gerror("rel32/not imported")
			fi
			(ref word32(p+r.offset)^:=int(thunkoffset)-r.offset-4)

		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.symbol=importedsym then

				(ref word32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				esac

				p32:=cast(p+r.offset)
				IF R.RELOCTYPE=ADDR32_REL THEN
					p32^:=p32^+u.virtoffset+imagebase
				ELSE
					p64:=cast(p32)
					p64^:=p64^+u.virtoffset+imagebase
				FI
			fi
		else
			println relocnames[r.reloctype]
			gerror("Can't do this rel type")
		esac

		r:=r.nextreloc
	od
end

proc getbaserelocs(ref sectionrec s)=
	ref relocrec r
	ref byte p
	ref strec d
	int index

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable^[r.stindex]

		case r.reloctype
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.symbol=importedsym then
			else
IF R.RELOCTYPE=ADDR32_REL THEN
!PRINTLN "??BASE RELOC",(D.SEGMENT=CODE_SEG|"CODE"|"DATA"),(R.RELOCTYPE=ADDR32_REL|"ADDR32"|"ADDR64")
ELSE
!PRINTLN "DOING BASERELOC",D.NAME,SEGMENTNAMES[S.SEGTYPE]

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
		if stentrypoint=nil then
			stentrypoint:=stentrypoint3
			if stentrypoint then
				println "Using tertiary 'WinMain' entry point"
			fi
		fi
	fi
	if stentrypoint=nil then
		if userentrypoint then
			println userentrypoint
			gerror("User entry point not found")
		else
			if not isdll then
				gerror("Entry point not found: main or start")
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

	memset(&sheader,0,sheader.bytes)
!	clear sheader

	strcpy(&sheader.name[1],s.name)
	sheader.virtual_size:=s.virtsize
	sheader.virtual_address:=s.virtoffset
	sheader.rawdata_offset:=s.rawoffset
	sheader.rawdata_size:=s.rawsize

	int64 aa
	case s.segtype
	when zdata_seg then
		sheader.characteristics:=0xC050'0080
	when idata_seg then
		sheader.characteristics:=0xC050'0040
	when code_seg then
		sheader.characteristics:=0x6050'0020
	when impdata_seg then
!		sheader.characteristics:=0xC030'0040
		sheader.characteristics:=0x4030'0040
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
		if length.odd then ++length fi		!keep even
		importtable[i].hintnameoffset:=diroffset
		diroffset+:=length
	od

!need also space for the names of the libs

!need I think to get to next multiple of four
	diroffset:=roundtoblock(diroffset,4)

	for i to ndlls do
		length:=strlen(dlltable[i].name)+1
		if length.odd then ++length fi		!keep even
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
!diroffset contains the overall size of the image

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
		if highmem=0 then
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

function getsectionno(int segment)int=
	case segment
	when zdata_seg then zsect
	when idata_seg then dsect
	when code_seg then csect
	else gerror("GSN"); 0
	esac
end

proc writeexporttable(ref byte pstart)=
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
	ref strec d

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
		gerror("Too many exports - can't sort")
	fi

	sortexports(sortindex)

	for i to nexports do
		d:=exporttable[sortindex[i]].def
		sectionno:=getsectionno(d.segment)

		strcpy(pnames,d.name)
		pnametable^:=namesoffset+virtoffset
		++pnametable
		namesoffset+:=strlen(d.name)+1
		pnames+:=strlen(d.name)+1

		paddrtable^:=d.offset+sectiontable[sectionno].virtoffset
		++paddrtable
		pordtable^:=i-1
		++pordtable
	od
end

function getexporttablesize:int=
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

	p:=pcm_allocz(basereloc.bytes)
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
		if nbaseblocks>=maxbaseblock then gerror("Too many blocks") fi
		++nbaseblocks
		blockbases[nbaseblocks]:=baseaddr
		blockcounts[nbaseblocks]:=0


		p:=basereloclist
		while p do
			addr:=p.address
			if addr>=baseaddr and addr<nextblock then
				++blockcounts[nbaseblocks]
			fi

			p:=p.nextitem
		od

		baseaddr:=nextblock
	until baseaddr>maxrelocaddr

	for i to nbaseblocks when blockcounts[i] do
		if blockcounts[i] iand 1 then
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
				IF Q.RELOCTYPE=ADDR64_REL THEN
					p16^:=addr-baseaddr+(q.reloctype=addr32_rel|3|10)<<12
					++p16
				FI
			fi
!
			q:=q.nextitem
		od
		if blockpadding[i] then p16++^:=0 fi

		p32:=cast(p16)

	od
end

proc sortexports([]int &sortindex)=
!sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
	ref strec d,e
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

			if strcmp(d.name, e.name)>0 then
				swapped:=1
				swap(sortindex[i], sortindex[i+1])
			fi
		od
	until not swapped

end
=== aa_writemcx.m 0 0 12/16 ===
!Translate SS data directly into MCB block, then write as mx/ml file

ref dbuffer dest

symbol entrypoint

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

	genbyte(dlls_dir)
	genword32(nsearchlibs)
	for i to nsearchlibs when searchlibs[i]^<>'$' do
		genstring(searchlibs[i])
	od

	genbyte(libs_dir)
	genword32(nimportlibs)
	for i to nimportlibs when importlibs[i]^<>'$' do
		genstring(importlibs[i])
	od

	writesymbols()

	genbyte(end_dir)

!CPL "WRITE MX FILE",FILENAME, =DEST.PSTART,DEST.PCURR-DEST.PSTART

	writefile(filename, dest.pstart, dest.pcurr-dest.pstart)
end

global proc writerelocs=
	ref relocrec oldr
	mcxreloc newr
	int n,count
	symbol d
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
!CPL "WRITERELOCS",D.NAME,SYMBOLNAMES[D.SYMBOL]

			case oldr.reloctype
			when rel32_rel then
				if d.symbol=importedsym then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
!CPL =D.NAME
					gerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.symbol=importedsym then
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
				gerror("reloc?")
			esac

			genblock(&newr, newr.bytes)

		od
	od
end

global proc countsymbols=
	symbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.symbol=exportedsym then d.expindex:=++nsymexports fi
		if d.symbol=importedsym then d.impindex:=++nsymimports fi
	od
end

proc writesymbols=
	symbol d
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
=== aa_writeobj.m 0 0 13/16 ===
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

export proc writess(ichar outfile)=
	writecoff(outfile)
end

proc writerecord(ref void r, int length)=
	memcpy(dataptr,r,length)
	dataptr+:=length
end

proc writerelocs(ref relocrec r,int nrelocs)=
	static coffrelocrec s
	ref strec d

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
				gerror("wrelocs/bad seg")
			esac

		else
			s.stindex:=r.stindex+stoffset
		esac

		s.reloctype:=r.reloctype
		case highmem
		when 0 then
		when 2 then
			IF R.RELOCTYPE=ADDR32_REL THEN
				S.RELOCTYPE:=REL32_REL
				R.RELOCTYPE:=REL32_REL
			FI
		else
			gerror("OBJ/highmem 1?")
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

function makesymbol(ichar name,int namelen=0, value=0, sectionno=0,symtype=0,storage=0,naux=0)ref imagesymbol=
	static imagesymbol r
	int length

	if namelen=0 then namelen:=strlen(name) fi

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
		gerror("as:Too many symbols")
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
		gerror("W:too many strings")
	fi
	stringtable[++nstrings]:=s
	stringlengths[nstrings]:=length

	nextstringoffset+:=length+1

	return offset
end

proc convertsymboltable=
!scan ss_symboltable and generate coff symboltable equivalents
	ref strec s
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

		case s.symbol
		when fwdlocalsym,localsym then
			scope:=3
		when importedsym,exportedsym then
			scope:=2
		else
			scope:=0
		esac

		addsymbol(makesymbol(s.name,s.namelen,sectionno:sect, storage:scope, value:s.offset))

	od
end

proc writecoff(ichar outfile)=
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
		gerror("Too many relocs (exceeds 16-bit field)")
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

	if fverbose then
		println "Writing file:",outfile
	fi
	writefile(outfile,datastart,dataptr-datastart)

end

=== aa_disasm.m 0 0 14/16 ===

!const showmregs=1
const showmregs=0

const halt=0xF4

int nmodules
int xfchsmask_pd

enumdata [0:]ichar opnames =
	(add_op=0,	"add"),
	(or_op,		"or"),
	(adc_op,	"adc"),
	(sbb_op,	"sbb"),
	(and_op,	"and"),
	(sub_op,	"sub"),
	(xor_op,	"xor"),
	(cmp_op,	"cmp")
end

[0:]ichar condnames = 
("o", "no", "b","ae","z","nz","be","a","s","ns","p","np",
 "l","ge","le","g")

enumdata []ichar addrmodenames=		! rm modes
	(amreg,			$),				! R
	(ammem,			$),				! [R+d]
	(amrel,			$)				! [RIP+d]
end

const wmask = 2x1000
const rmask = 2x0100
const xmask = 2x0010
const bmask = 2x0001

const rstack=5						!1-base register codes
const rframe=6

int rex

int addrmode						!amreg/ammem/amrel
int rmreg							!0, or 1..16; adjusted middle value of modrm byte
int rmopc							!0 to 7; middle value of modrm byte 
int ripmode							!1 for rip-relative
int basereg							!0, or 1..16
int indexreg						!0, or 1..16
int scale							!1,2,4
int opsize							!1,2,4,8
int offset
int offsetsize						!1 or 4
int sizeoverride					!32=>16 switch
int addroverride					!32=>16 switch
int f2override						!xmm regs
int f3override						!xmm regs

[256]char deststr
ichar destptr

ref byte codeptr

global function decodeinstr(ref byte &cptr,baseaddr=nil)ichar=
!decode next instruction at codeptr
!return 1 if decoded, with codeptr stepped to start of next instruction
!return 0 when end-of-code seen (nop or 0x90)
	int n,w
	int opc,reg,op,xxx,oldopsize,dispsize
	ref byte pstart
	static [256]char str
	[128]char str2
	const maxinstrlen=14
	ichar s

	deststr[1]:=0

	pstart:=codeptr:=cptr

	rex:=0
	opsize:=1
	f2override:=f3override:=sizeoverride:=addroverride:=0
	basereg:=indexreg:=offset:=0

	retry:						!back here after prefix byte seen

	switch opc:=codeptr++^
	when 0x00,0x1, 0x08,0x9, 0x10,0x11, 0x18,0x19,
						0x20,0x21, 0x28,0x29, 0x30,0x31, 0x38,0x39 then	!arith R/M, R
		op:=opc>>3
		decodeaddr(opc iand 1)
		getsilx(basereg)
		getsil(rmreg)
		genstr(opnames[op])
		printaddrmode()
		genstr(", ")
		genstr(strreg(rmreg,opsize))

	when 0x02,0x3, 0x0A,0xB, 0x12,0x13, 0x1A,0x1B,
						0x22,0x23, 0x2A,0x2B, 0x32,0x33, 0x3A,0x3B then	!arith R,R/M
		op:=opc>>3
		decodeaddr(opc iand 1)
		genstr(opnames[op])
		genstr(" ")
		getsil(rmreg)
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0x04,0x5, 0x0C,0xD, 0x14,0x15, 0x1C,0x1D,
						0x24,0x25, 0x2C,0x2D, 0x34,0x35, 0x3C,0x3D then	!arith rAX,imm
		genstr(opnames[opc>>3])
		genstr(" ")
		if opc iand 1 then
			opsize:=4
			if sizeoverride then opsize:=2 fi
			if rex iand wmask then opsize:=8 fi
		fi
		genstr(strreg(1,opsize))
		genstr(", ")
		genintd(readimm())

	when 0x0F then
		decodetwobyteinstr()

	when 0x40 .. 0x4F then
		rex:=opc
!	if rex iand wmask then wopsize:=8 fi

		goto retry

	when 0x50 .. 0x57 then
		reg:=getreg(opc iand 7,rex iand bmask)
		genstr("push ")
		genstr(strreg(reg,8))

	when 0x58 .. 0x5F then
		reg:=getreg(opc iand 7,rex iand bmask)
		genstr("pop ")
		genstr(strreg(reg,8))

	when 0x63 then
		decodeaddr(1)
		genstr("movsxd ")
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		opsize:=4
		printaddrmode()

	when 0x66 then
		sizeoverride:=1
		goto retry

	when 0x67 then
		addroverride:=1
		goto retry

	when 0x68 then
		genstr("push ")
		if sizeoverride then
			genintd(readint16())
		else
			genintd(readint32())
		fi

	when 0x6A then
		genstr("push ")
		genintd(readsbyte())

	when 0x69, 0x6B then
		decodeaddr(1)
		if basereg<>rmreg then
			genstr("imul3")
			genstr(" ")
			genstr(strreg(rmreg,opsize))
			genstr(", ")
		else
			genstr("imul2")
		fi
		printaddrmode()
		genstr(", ")
		opsize:=(opc iand 2|1|opsize)
		genintd(readimm())

	when 0x70..0x7F then
		genstr("j")
		genstr(condnames[opc iand 15])
		genstr(" ")
		genintd(readsbyte())

	when 0x80..0x83 then			!arith r/m,imm
		decodeaddr(opc iand 1)
		genstr(opnames[rmopc])
		getsilx(basereg)
		printaddrmode()
		genstr(", ")
		if opc<>0x83 then
			genintd(readimm())
		else
			genintd(readsbyte())
		fi

	when 0x84, 0x85 then			!test reg,reg/mem
		decodeaddr(opc iand 1)
		getsilx(basereg)
		getsil(rmreg)
		genstr("test ")
		printaddrmode()
		genstr(", ")
		genstr(strreg(rmreg,opsize))

	when 0x86,0x87 then				!complex excg
		decodeaddr(opc iand 1)
		genstr("exch2 ")
		getsilx(basereg)
		getsil(rmreg)
		genstr(strreg(rmreg,opsize))
		genstr(",")
		printaddrmode()

	when 0x88, 0x89 then			!mov r/m,reg
		decodeaddr(opc iand 1)
		genstr("mov")
		getsilx(basereg)
		getsil(rmreg)

		printaddrmode()
		genstr(", ")
		genstr(strreg(rmreg,opsize))

	when 0x8A, 0x8B then			!mov reg,r/m
		decodeaddr(opc iand 1)
		genstr("mov ")
		getsilx(basereg)
		getsil(rmreg)
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0x8D then
		decodeaddr(1)
		genstr("lea ")
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0x8F then
		decodeaddr(1)
		opsize:=1
		genstr("pop")
		printaddrmode()

	when 0x90 then
		if rex then goto doexch fi
		genstr("nop")

	when 0x91..0x97 then			!exch eax/reg
	doexch:
		reg:=(opc iand 7)+1
		if rex iand bmask then reg+:=8 fi
		opsize:=(sizeoverride|2|4)
		if rex iand wmask then opsize:=8 fi
		genstr("xchg ")
		genstr(strreg(1,opsize))
		genstr(", ")
		genstr(strreg(reg,opsize))

	when 0x98 then
		if sizeoverride then
			genstr("cbw")
		else
			genstr("cbw???")
		fi
	when 0x99 then
		if sizeoverride then
			genstr("cwd")
		elsif rex iand wmask then
			genstr("cqo")
		else
			genstr("cdq")
		fi
	when 0x9B then genstr("wait")

	when 0x9C then genstr("pushf")
	when 0x9D then genstr("popf")
	when 0x9E then genstr("sahf")
	when 0x9F then genstr("lahf")

	when 0xA4..0xA7, 0xAA..0xAF then
		genstr((opc>>1 iand 7|"?","movs","cmps","?","stos","lods","scas"|"?"))
		if opc iand 1=0 then
			genstr("b")
		else
			if rex iand wmask then
				genstr("q")
			elsif sizeoverride then
				genstr("w")
			else
				genstr("d")
			fi
		fi

	when 0xA8, 0xA9 then				!test r0,imm
		genstr("test ")
		if opc iand 1 then
			opsize:=(sizeoverride |2|4)
			if rex iand wmask then opsize:=8 fi
		fi
		genstr(strreg(1,opsize))
		genstr(", ")
		genintd(readimm())

	when 0xB0..0xBF then			!mov reg,imm
		reg:=(opc iand 7)+1
		if rex iand bmask then reg+:=8 fi
		if (opc iand 2x1000) then
			opsize:=(sizeoverride |2|4)
			if rex iand wmask then opsize:=8 fi
		fi
		genstr("mov ")
!CPL "DIS",=REG
		getsil(reg)

		genstr(strreg(reg,opsize))
		genstr(", ")
		genintd(readimm8())

	when 0xC0, 0xC1, 0xD0..0xD3 then
		decodeaddr(opc iand 1)
		getsilx(basereg)
		genstr((rmopc+1|"rol","ror","rcl","rcr","shl","shr","?","sar"|"?"))
		printaddrmode()
		if opc<=0xC1 then
			genstr(", ")
			genintd(readbyte())
		else
			genstr((opc iand 2|", cl"|", 1"))
		fi

	when 0xC2 then
		genstr("retn ")
		genintd(readword16())

	when 0xC3 then
		genstr("ret")

	when 0xC6,0xC7 then
		decodeaddr(opc iand 1)
		genstr("mov")
		printaddrmode()
		genstr(", ")
		genintd(readimm())

	when 0xD7 then genstr("xlat")

	when 0xD8..0xDF then
		decode8087(opc iand 7)

	when 0xE0 then genstr("loopnz "); genintd(readsbyte())
	when 0xE1 then genstr("loopz "); genintd(readsbyte())
	when 0xE2 then genstr("loop "); genintd(readsbyte())

	when 0xE3 then
		if addroverride then
			genstr("jecxz ")
		else
			genstr("jrcxz ")
		fi
		genintd(readsbyte())

	when 0xE8 then
		genstr("call ")
		genintd(readint32())

	when 0xE9 then
		genstr("[4] jmp ")
		genintd(readint32())

	when 0xEB then
		genstr("jmp ")
		genintd(readsbyte())

	when 0xF2 then
		if codeptr^<>0x0F and (codeptr^<0x40 and codeptr^>0x4F) then
			genstr("repne")
		else
			f2override:=1
			goto retry
		fi
	when 0xF3 then
		if codeptr^<>0x0F and (codeptr^<0x40 and codeptr^>0x4F) then
			genstr("repe")
		else
			f3override:=1
			goto retry
		fi

	when 0xF4 then
!		println "	end of code [halt]"
!		return nil

	when 0xF6,0xF7 then
		decodeaddr(opc iand 1)
		getsilx(basereg)
		genstr((rmopc+1|"test","?","not","neg","mul","imul","div","idiv"|"?"))
		printaddrmode()
		if rmopc=0 then
			if opsize=8 then opsize:=4 fi
			genstr(", ")
			genintd(readimm())
		fi

	when 0xFE then
		w:=0
		goto doff

	when 0xFF then			!various
		w:=1
	doff:
		decodeaddr(w)
		case rmopc
		when 2x_000 then	!inc
			getsilx(basereg)
			genstr("inc")
		when 2x_001 then	!dec
			getsilx(basereg)
			genstr("dec")
		when 2x_010 then	!call
			opsize:=8
			genstr("icall")
		when 2x_100 then	!jmp
			opsize:=8
			genstr("jmp")
		when 2x_110 then	!push
			opsize:=8
			genstr("push")
		else
			println "FFxx?"
		esac
		printaddrmode()

	else
		genstr("Unknown opcode: ")
    genhex(opc)
	end switch

!at this point, deststr contains the decoded instruction
!need to put in address, bytes etc

!	if baseaddr then
		print @&.str,baseaddr:"z6h",,": "
!	else
!		print @&.str,pstart:"z6h",,": "
!	fi

	n:=codeptr-pstart
	to n do
		print @&.str2,int(pstart++^):"z2H",," "

		strcat(&.str,&.str2)
	od
	to maxinstrlen-n do
		strcat(&.str,"-- ")
	od
	strcat(&.str,&.deststr)

	cptr:=codeptr

	return &.str
end

proc decodetwobyteinstr=
!0F has been decoded
	int opc,rhssize,third,imm, reg
	ichar opcstr

	switch opc:=codeptr++^
	when 0x2A then					!cvtsi2ss/sd XMM, REG/MEM
		decodeaddr(1)
		if f3override then
			genstr("cvtsi2ss ")
		else
			genstr("cvtsi2sd ")
		fi
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(0)
		
	when 0x2C then					!cvt2ss/sd2si XMM, REG/MEM
		decodeaddr(1)
		if f3override then
			genstr("cvttss2si ")
			rhssize:=4
		else
			genstr("cvttsd2si ")
			rhssize:=8
		fi
		if rex iand wmask then
			genstr(strreg(rmreg,8))
		else
			genstr(strreg(rmreg,4))
		fi
		genstr(", ")
		opsize:=rhssize
		printaddrmode(1)

	when 0x2D then					!cvt2ss/sd2si XMM, REG/MEM
		decodeaddr(1)
		if f3override then
			genstr("cvtss2si ")
			rhssize:=4
		else
			genstr("cvtsd2si ")
			rhssize:=8
		fi
		if rex iand wmask then
			genstr(strreg(rmreg,8))
		else
			genstr(strreg(rmreg,4))
		fi
		genstr(", ")
		opsize:=rhssize
		printaddrmode(1)

	when 0x2F then					!comiss/comisd XMM, REG/MEM
		decodeaddr(1)
		if sizeoverride then
			opsize:=8
			genstr("comisd ")
		else
			opsize:=4
			genstr("comiss ")
		fi
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)

	when 0x3A then					!possible pcmpistri
		third:=codeptr++^

		case third
		when 0x63 then
			genstr("pcmpistri ")
		when 0x62 then
			genstr("pcmpistrm ")
		else
			genstr("Unknown opcode 2-byte opcode: 0F ")
		    genhex(opc)
			return
		esac

		decodeaddr(1)
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)
		genstr(", ")
		imm:=codeptr++^
		genintd(imm)

	when 0x40..0x4F then
		decodeaddr(1)
		genstr("cmov")
		genstr(condnames[opc iand 15])
		genstr(" ")
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0x51 then					!sqrtss/sd
		decodeaddr(1)
		opsize:=(f3override|4|8)
		genstr((opsize=4|"sqrtss "|"sqrtsd "))
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)

	when 0x54 then					!ANDPD
		decodeaddr(1)
		genstr((sizeoverride|"andpd "|"andps "))
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=(sizeoverride|8|4)
		printaddrmode(1)

	when 0x57 then					!XORPD
		decodeaddr(1)
		genstr((sizeoverride|"xorpd "|"xorps "))
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=(sizeoverride|8|4)
		printaddrmode(1)

	when 0x58 then					!addss/addsd
		opcstr:="adds"
	doarith:
		genstr(opcstr)
		decodeaddr(1)
		if f2override then
			opsize:=8
			genstr("d ")
		else
			opsize:=4
			genstr("s ")
		fi
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)

	when 0x59 then					!mulss/mulsd
		opcstr:="muls"
		goto doarith

	when 0x5A then					!cvtss2sd/cvtsd2ss
		decodeaddr(1)
		if f3override then
			genstr("cvtss2sd ")
			rhssize:=4
		else
			genstr("cvtsd2ss ")
			rhssize:=8
		fi
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=rhssize
		printaddrmode(1)

	when 0x5C then					!subss/subsd
		opcstr:="subs"
		goto doarith

	when 0x5D then
		opcstr:="mins"
		goto doarith

	when 0x5E then					!divss/divsd
		opcstr:="divs"
		goto doarith

	when 0x5F then
		opcstr:="maxs"
		goto doarith


	when 0x6E then					!mov X/MM, REG/MEM
		decodeaddr(1)
		opsize:=(rex iand wmask|8|4)
		genstr((opsize=4|"movd "|"movq "))
		if sizeoverride then		!xmm
			genstr(strxmm(rmreg))
		else
			genstr(strmmx(rmreg))
		fi
		genstr(", ")
		printaddrmode()

	when 0x6F then					!movdqa/dqu, X/MEM, X/X
		decodeaddr(1)
		opsize:=16
		if sizeoverride then		!66
			genstr("movdqa ")
		elsif f3override then		!F3
			genstr("movdqu ")
		else
			genstr("No 66/F3 ")
		fi
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)

	when 0x7E then					!mov REG/MEM, X/MM
		decodeaddr(1)
		if f3override then
			opsize:=8
			genstr("movq ")
			genstr(strxmm(rmreg))
			genstr(", ")
			printaddrmode(1)
		elsif rex iand wmask then
			opsize:=8
			genstr("movq ")
			printaddrmode()
			genstr(", ")
			genstr(strxmm(rmreg))
		else
			opsize:=4
			genstr("movd ")
			printaddrmode()
			genstr(", ")
			if sizeoverride then		!xmm
				genstr(strxmm(rmreg))
			else
				genstr(strmmx(rmreg))
			fi
		fi

	when 0x7F then					!movdqa/dqu, MEM/X
		decodeaddr(1)
		opsize:=16
		if sizeoverride then		!66
			genstr("movdqa ")
		elsif f3override then		!F3
			genstr("movdqu ")
		else
			genstr("No 66/F3 ")
		fi
		printaddrmode(1)
		genstr(", ")
		genstr(strxmm(rmreg))

	when 0x80..0x8F then			!long rel jumps
		genstr("[long] j")
		genstr(condnames[opc iand 15])
		genstr(" ")
		if sizeoverride then
			genintd(readint16())
		else
			genintd(readint32())
		fi

	when 0x90..0x9F then
		decodeaddr(0)
		genstr("set")
		genstr(condnames[opc iand 15])
		genstr(" ")
		getsilx(basereg)
		printaddrmode()

	when 0xA4, 0xAC then		!shld/shrd
		decodeaddr(1)
CPL =OPSIZE
		genstr((opc=0xA4|"shld"|"shrd"))
		printaddrmode(0)
		genstr(",")
		genstr(strreg(rmreg, opsize))	
		genstr(",")
		genstr(strint(codeptr++^))

	when 0xAF then
		decodeaddr(1)
		genstr("imul ")
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0xB6, 0xB7, 0xBE, 0xBF then
		decodeaddr(1)
!	opsize:=4
		genstr((opc<0xBE|"movzx "|"movsx "))
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		opsize:=(opc iand 1|2|1)
		printaddrmode()

	when 0xB8 then
		decodeaddr(1)
		genstr("popcnt ")
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0xBC, 0xBD then
		decodeaddr(1)
		genstr((opc=0xBC|"bsf "|"bsr "))
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0xC8..0xCF then			!BSWAP
		reg:=getreg(opc iand 7,rex iand bmask)
		opsize:=4
		if rex iand wmask then opsize:=8 fi
		genstr("bswap ")
		genstr(strreg(reg, opsize))

	when 0xD6 then
		decodeaddr(1)
		opsize:=8
		genstr("movq ")
		printaddrmode(1)
		genstr(",")
		genstr(strxmm(rmreg))	

	when 0xDB then					!PAND
		decodeaddr(1)
		genstr("pand ")
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=8	!(sizeoverride|8|4)
		printaddrmode(1)

	when 0xEF then					!PXOR
		decodeaddr(1)
		genstr("pxor ")
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=8	!(sizeoverride|8|4)
		printaddrmode(1)


	else
	error:
		genstr("Unknown opcode 2-byte opcode: 0F ")
    genhex(opc)
	end switch
end

proc decodeaddr(int w=0)=
!codeptr points to modrm byte, with possible sib and/or disp following
!decode modrm, sib and disp
!store result in amode:
! basereg		0 when not used
! indexreg
! scale			1,2,4,8 factor for indexreg
! offset		0, or any offset or abs address
! addrmode		rm-code
!the function returns the xxx value (middle part of modrm byte)
	int modrm,xxx,mode,sib,rm

	basereg:=indexreg:=0
	scale:=1
	offset:=0
	ripmode:=0

	if w then
		opsize:=(sizeoverride|2|4)
		if rex iand wmask then opsize:=8 fi
	else
		opsize:=1
	fi

	modrm:=codeptr++^

!CPL "DECODE", MODRM:"H"

	mode:=modrm>>6
	xxx:=(modrm>>3) iand 7
	rm:=modrm iand 7

!CPL =MODE
!CPL =XXX
!CPL =RM

	if mode=3 then		!plain register access
		basereg:=rm+1
		addrmode:=amreg
	elsif rm<>4 then				!not esp; no sib
		if mode=0 and rm=5 then		![ebp] is actually [rip+disp]
			offset:=readint32()		!
			ripmode:=1
			addrmode:=ammem

		else
			basereg:=rm+1
			addrmode:=ammem
			case mode
			when 1 then
				offset:=readsbyte()
			when 2 then
				offset:=readint32()
			esac
		fi
	else			!sib follows
		addrmode:=ammem
		sib:=readbyte()
		indexreg:=((sib>>3) iand 7)+1
		basereg:=(sib iand 7)+1
		scale:=(sib>>6+1|1,2,4,8|0)

		if mode=0 and basereg=rframe and indexreg=rstack then	!no base/index regs, only d32 disp
			indexreg:=basereg:=0
			offset:=readint32()

		elsif mode=0 and basereg=rframe  then	!no base/index regs, only d32 disp
			basereg:=0
			offset:=readint32()

		elsif mode=0 and indexreg=rstack then	!no index register, only base; no disp
			indexreg:=0

		else
			case mode
			when 1 then
				offset:=readsbyte()
			when 2 then
				offset:=readint32()
			esac
			if indexreg=rstack then				!stack means no index reg
				indexreg:=0
			fi
		fi

	fi

	if basereg and rex iand bmask then basereg+:=8 fi
	if indexreg and rex iand xmask then indexreg+:=8 fi

	rmreg:=xxx+1
	if rex iand rmask then rmreg+:=8 fi
	rmopc:=xxx
end

function readbyte:int=
	return codeptr++^
end

function readsbyte:int=
	return (ref int8(codeptr++))^
end

function readword16:word=
	word a
	a:=ref word16(codeptr)^
	codeptr+:=2
	return a
end

function readint16:int=
	int a
	a:=ref int16(codeptr)^
	codeptr+:=2
	return a
end

function readword32:word=
	word a
	a:=ref word32(codeptr)^
	codeptr+:=4
	return a
end

function readint32:int=
	int a
	a:=ref int32(codeptr)^
	codeptr+:=4
	return a
end

function readint64:int64=
	int64 a
	a:=ref int64(codeptr)^
	codeptr+:=8
	return a
end

function getreg(int regcode,upper)int=
	if upper then
		return regcode+8+1
	fi
	return regcode+1
end

global function strreg(int reg,opsize)ichar=
static []ichar regnames8=("al","cl","dl","bl","spl","bpl","sil","dil",
						"r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b",
				"ah","bh","ch","dh")

static []ichar regnames16=("ax","cx","dx","bx","sp","bp","si","di",
						"r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w")

static []ichar regnames32=("eax","ecx","edx","ebx","esp","ebp","esi","edi",
						"r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d")

static []ichar regnames64=("rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
						"r8","r9","r10","r11","r12","r13","r14","r15")

static []ichar mregnames8=("B0","B10","B11","B4","B15","B14","B5","B3",
						"B12","B13","B1","B2","B6","B7","B8","B9",
					"B16","B18","B19","B17")

static []ichar mregnames16=("W0","W10","W11","W4","Wsp","Wbp","W5","W3",
						"W12","W13","W1","W2","W6","W7","W8","W9")

static []ichar mregnames32=("A0","A10","A11","A4","Astack","Aframe","A5","A3",
						"A12","A13","A1","A2","A6","A7","A8","A9")

static []ichar mregnames64=("D0","D10","D11","D4","Dstack","Dframe","D5","D3",
						"D12","D13","D1","D2","D6","D7","D8","D9")

	if reg=0 then return "<>" fi

	if showmregs then
		case opsize
		when 1 then return mregnames8[reg]
		when 2 then return mregnames16[reg]
		when 4 then return mregnames32[reg]
		when 8 then return mregnames64[reg]
		esac
	else
		case opsize
		when 1 then return regnames8[reg]
		when 2 then return regnames16[reg]
		when 4 then return regnames32[reg]
		when 8 then return regnames64[reg]
		esac
	fi
	return ""
end

function strfreg(int freg)ichar=
!freg is 0-based
	static []ichar fregnames=("st0","st1","st2","st3","st4","st5","st6","st7")
	return fregnames[freg]
end

proc printaddrmode(int xmm=0)=
	static [100]char str
	ichar plus
	int addrsize

	genstr(" ")

	case addrmode
	when amreg then
		if xmm then
			genstr(strxmm(basereg))
		else
			getsilx(basereg)
			genstr(strreg(basereg,opsize))
		fi
		return
	esac

	case opsize
	when 1 then genstr("byte ")
	when 2 then genstr("word ")
	when 4 then genstr("dword ")
	when 8 then genstr("qword ")
	when 10 then genstr("tword ")
	when 16 then genstr("oword ")
	else
	CPL "///OPSIZE",opsize
	esac

	if ripmode then
		genstr("rip:")
	fi
	genstr("[")
	plus:=""
	addrsize:=(addroverride|4|8)

	if basereg then
		genstr(strreg(basereg,addrsize))
		plus:="+"
	fi
	if indexreg then
		genstr(plus)
		genstr(strreg(indexreg,addrsize))
GENSTR("<INDEX>")
		if scale>1 then
			genstr("*")
			genintd(scale)
		fi
		plus:="+"
	fi

	if offset or (basereg=0 and indexreg=0) then
!	print plus,,offset,"<",ref void(offset),,">"
		if basereg=0 and indexreg=0 then
			genintd(offset)
!			genhex(offset)
		else
			if offset>0 then genstr(plus) fi
			genintd(offset)
!			genhex(offset)
		fi
	fi
	genstr("]")
!	if addrmode=amrel then genstr("+RIP") fi
end

proc genstr(ichar s)=
	strcat(&.deststr,s)
end

proc genintd(int64 a)=
!	genstr("0x")
	genhex(a)
	genstr("H")
	unless a in 0..9 then
		genstr("(")
		genstr(strint(a))
		genstr(")")
	end
end

proc genhex(int64 a)=
	genstr(strint(a,"h"))
end

function readimm:int=
!read signed offset according to opsize

	case opsize
	when 1 then return readsbyte()
	when 2 then return readint16()
	when 4,8 then return readint32()			!64-bit uses 32-bit immediate
	esac
	return 0
end

function readimm8:int64=
!like readimm but can 8 bytes too
	if opsize<8 then return readimm() fi
	return readint64()
end

function strxmm(int reg)ichar=
	static [32]char str
	print @&.str,"xmm",,reg-1
	return &.str
end

function strmmx(int reg)ichar=
	static [32]char str

	print @&.str,"mmx",,reg-1
	return &.str
end

proc decode8087(int ttt)=
	byte bb
	int longopc,freg,shortopc,code

	bb:=codeptr++^			!following byte

	longopc:=ttt<<8+bb		!bottom 11 bits of 2-bytes opcode
	freg:=(bb iand 7)+1		!where bb specifies a register in bottom 3 bits

!first look at all dedicated opcodes before treating bb as modrm byte

	case longopc
	when 2x'110'1101'1001 then genstr("fcompp")
	when 2x'001'1110'0100 then genstr("ftst")
	when 2x'001'1110'0101 then genstr("fxam")
	when 2x'001'1110'1110 then genstr("fldz")
	when 2x'001'1110'1000 then genstr("fld1")
	when 2x'001'1110'1011 then genstr("fldpi")
	when 2x'001'1110'1001 then genstr("fldl2t")
	when 2x'001'1110'1010 then genstr("fldl2e")
	when 2x'001'1110'1100 then genstr("fldlg2")
	when 2x'001'1110'1101 then genstr("fldln2")

	when 2x'001'1111'1010 then genstr("fsqrt")
	when 2x'001'1111'1110 then genstr("fsin")
	when 2x'001'1111'1111 then genstr("fcos")
	when 2x'001'1111'1011 then genstr("fsincos")
	when 2x'001'1111'1101 then genstr("fscale")
	when 2x'001'1111'1000 then genstr("fprem")
	when 2x'001'1111'1100 then genstr("frndint")
	when 2x'001'1111'0100 then genstr("fxtract")
	when 2x'001'1110'0001 then genstr("fabs")
	when 2x'001'1110'0000 then genstr("fchs")

	when 2x'001'1111'0010 then genstr("fptan")
	when 2x'001'1111'0011 then genstr("fpatan")
	when 2x'001'1111'0000 then genstr("f2xm1")
	when 2x'001'1111'0001 then genstr("fyl2x")
	when 2x'001'1111'1001 then genstr("fyl2xp1")

	when 2x'011'1110'0011 then genstr("finit")
	when 2x'011'1110'0000 then genstr("feni")
	when 2x'011'1110'0001 then genstr("fdisi")

	when 2x'011'1110'0010 then genstr("fclex")

	when 2x'001'1111'0111 then genstr("fincstp")
	when 2x'001'1111'0110 then genstr("fdecstp")
	when 2x'001'1101'0000 then genstr("fnop")

	elsecase longopc iand 2x'111'11111'000			!ignore bottom 3 bits

	when 2x'001'11000'000 then genstr("fld "); genstr(strfreg(freg))
	when 2x'101'11010'000 then genstr("fst "); genstr(strfreg(freg))
	when 2x'101'11011'000 then genstr("fstp "); genstr(strfreg(freg))
	when 2x'001'11001'000 then genstr("fxch "); genstr(strfreg(freg))
	when 2x'000'11010'000 then genstr("fcom "); genstr(strfreg(freg))
	when 2x'000'11011'000 then genstr("fcomp "); genstr(strfreg(freg))
	when 2x'101'11000'000 then genstr("ffree "); genstr(strfreg(freg))

	elsecase longopc iand 2x'001'11111'000			!ignore bottom 3 bits and top 2

	when 2x'000'11000'000 then do87arith("fadd",ttt,freg)

	when 2x'000'11100'000 then do87arith("fsub",ttt,freg)
	when 2x'000'11101'000 then do87arith("fsubr",ttt,freg)

	when 2x'000'11001'000 then do87arith("fmul",ttt,freg)

	when 2x'000'11110'000 then do87arith("fdiv",ttt,freg)
	when 2x'000'11111'000 then do87arith("fdivr",ttt,freg)

	else	!finally, have to deal with modrm etc
		--codeptr					!put back modrm byte
		decodeaddr(0)			!code is middle bits
		shortopc:=ttt<<3 + rmopc

		case shortopc				!look at combination of ttt and code (middle bits of modrm)
		when 2x'111'101 then do87mem("fld",4)
		when 2x'011'101 then do87mem("fld",5)
		when 2x'111'100 then do87mem("fldbcd")

		when 2x'111'111 then do87mem("fstp",4)
		when 2x'011'111 then do87mem("fstp",5)
		when 2x'111'110 then do87mem("fstpbcd")

		when 2x'001'101 then do87mem("fldcw")
		when 2x'001'111 then do87mem("fstcw")
		when 2x'101'111 then do87mem("fstsw")

		when 2x'001'110 then do87mem("fstenv")
		when 2x'001'100 then do87mem("fldenv")
		when 2x'101'110 then do87mem("fsave")
		when 2x'101'100 then do87mem("frstor")

		elsecase shortopc iand 2x001'111		!ignore top two bits (mf code)

		when 2x'001'000 then do87mem("fld",ttt>>1)
		when 2x'001'010 then do87mem("fst",ttt>>1)
		when 2x'001'011 then do87mem("fstp",ttt>>1)
		when 2x'000'010 then do87mem("fcom",ttt>>1)
		when 2x'000'011 then do87mem("fcomp",ttt>>1)
		when 2x'000'000 then do87mem("fadd",ttt>>1)
		when 2x'000'100 then do87mem("fsub",ttt>>1)
		when 2x'000'101 then do87mem("fsubr",ttt>>1)
		when 2x'000'001 then do87mem("fmul",ttt>>1)
		when 2x'000'110 then do87mem("fdiv",ttt>>1)
		when 2x'000'111 then do87mem("fdivr",ttt>>1)

		else
			genstr("UNKNOWN x87 OPCODE")
		esac
	esac

end

proc do87arith(ichar opcstr, int ttt,freg)=
	int d, p

	d:=ttt iand 2x100		!d=0:  to st0; d<>0: to freg
	p:=ttt iand 2x010		!p<>0: pop after operation

	genstr(opcstr)
	if p then
		genstr("p")
	fi
	genstr(" ")

	if d=0 then
		genstr("st0, ")
    genstr(strfreg(freg))
	else
    genstr(strfreg(freg))
		genstr(", st0")
	fi
end

proc do87mem(ichar opcstr,int mf=-1)=
!mf has values 0,1,2,4 for type and width, when used; but also 4 for i64
	genstr("f")

	case mf
	when 2x'00 then opsize:=4
	when 2x'01 then genstr("i"); opsize:=4
	when 2x'10 then opsize:=8
	when 2x'11 then genstr("i"); opsize:=2
	when 4 then genstr("i"); opsize:=8
	when 5 then opsize:=10
	esac
	genstr(opcstr+1)

	genstr(" ")
	printaddrmode()
end

proc getsil(int &reg)=
!for certain byte-reg combinations, convert regs ah,ch,dh,bh to spl,bpl,sil,dil
	if opsize=1 and not rex and reg>=5 and reg<=8 then
		case reg
		when 5 then reg:=17
		when 6 then reg:=19
		when 7 then reg:=20
		when 8 then reg:=18
		esac

!		reg+:=12				!5..8 => 17..20


	fi
end

proc getsilx(int &reg)=
!as getsil but used for basereg, which must have addrmode=amreg
	if addrmode=amreg and opsize=1 and rex=0 and reg>=5 and reg<=8 then
		case reg
		when 5 then reg:=17
		when 6 then reg:=19
		when 7 then reg:=20
		when 8 then reg:=18
		esac

!		reg+:=12				!5..8 => 17..20
	fi
end
=== aa_writess.m 0 0 15/16 ===
global function writessdata(int fexe)ref strbuffer=
	gs_init(dest)
	wshowssdata(fexe)

	gs_line(dest)
	return dest
end

proc wshowssdata(int fexe)=
	gs_strln(dest,(fexe|"EXE FORMAT"|"AFTER GENSS"))

	wshowsections()
!
	gs_line(dest)

	wshowsectionrelocs2("Idata",ss_idatarelocs,ss_nidatarelocs)
	wshowsectionrelocs2("Code",ss_coderelocs,ss_ncoderelocs)

	gs_str(dest,"proc Section Zdata: ")
	gs_strint(dest,ss_zdatalen)
	gs_line(dest)

	wshowsectiondata(&sectiontable[dsect])
	wshowsectioncode(&sectiontable[csect])
	if fexe then
		wshowsectiondata(&sectiontable[isect])
	fi

	wshowsymboltable2()
	wshowimporttable()
	gs_strln(dest,"END OF GENSS")

end

proc wshowsectiondata(ref sectionrec d)=
int i,k,length,bb
	[128]char str,str2
	ref byte p

	gs_str(dest,"proc Section ")
	gs_str(dest,d.name)
	gs_str(dest," Size:")
	gs_strint(dest,d.virtsize)
	gs_line(dest)
	gs_line(dest)

	k:=0
	if d.segtype<>impdata_seg then
		p:=bufferelemptr(d.data,0)
	else
		p:=d.bytedata
	fi
	length:=d.virtsize

	str[1]:=0

	ref byte baseaddr:=cast(imagebase+d.virtoffset)

	print @&.str2,baseaddr:"Z8H",,": "

	gs_str(dest,&.str2)

	for i:=1 to length do
		bb:=p++^
		print @&.str2,bb:"z2H",," "
		gs_str(dest,&.str2)

		if 32<=bb<=127 then
			str2[1]:=bb
			str2[2]:=0
			strcat(&.str,&.str2)
		else
			strcat(&.str,".")
		fi
		if ++k=16 or i=length then
			if k<16 then
				to 16-k do
					gs_str(dest,"   ")
					strcat(&.str," ")
				od
			fi
			gs_str(dest,"	[")
			gs_str(dest,&.str)
			gs_strln(dest,"]")
			k:=0
			str[1]:=0
			baseaddr+:=16
			print @&.str2,baseaddr:"z8h",,": "
			gs_str(dest,&.str2)
		fi
	od
	if k=0 then
		gs_line(dest)
	fi

	gs_line(dest)
	if k then gs_line(dest) fi
end

proc wshowsectioncode(ref sectionrec p)=
ref byte codeptr,codeend,codestart
	int length,offset
	ichar s
	[16]char str

	gs_strln(dest, "proc Section Code")

	length:=p.virtsize
	codestart:=codeptr:=bufferelemptr(p.data,0)
	codeend:=codeptr+length

	ref byte baseaddr:=cast(imagebase+p.virtoffset)

	while codeptr<codeend do
		offset:=codeptr-codestart
!S:=NIL
		s:=decodeinstr(codeptr,baseaddr+offset)
		exit when s=nil

		print @&.str,offset:"4",," "
		gs_str(dest,&.str)

		gs_strln(dest,s)
	od

	gs_line(dest)
end

proc wshowsectionrelocs2(ichar caption,ref relocrec relocs, int nrelocs)=
	ref relocrec r

	gs_str(dest,"proc Section Relocs: ")
	gs_str(dest,caption)
	gs_str(dest," ")
	gs_strint(dest,nrelocs)
	gs_line(dest)

	r:=relocs

	while r do

		gs_str(dest,"Reloc: ")
		gs_str(dest,relocnames[r.reloctype])
		gs_str(dest," Offset: ")
		gs_strint(dest,r.offset)
		gs_str(dest," ST Index: ")
		gs_strint(dest,r.stindex)
		gs_str(dest," ")
		gs_str(dest,ss_symboltable^[r.stindex].name)
		gs_line(dest)

		r:=r.nextreloc
	od
	gs_line(dest)

end

proc gs_value(ichar caption, int64 value)=
	[256]char str

	strcpy(&.str,caption)
	strcat(&.str,":")
	ipadstr(&.str,20)
	gs_str(dest,&.str)

	fprint @&.str,"0x# #",value:"H",value
	gs_strln(dest,&.str)
end

proc wshowsymboltable2=

	gs_strln(dest,"Proc Symbol Table")
	int i
	for i:=1 to ss_nsymbols do
		gs_strint(dest,i)
		gs_str(dest,": ")
		gs_strln(dest,ss_symboltable^[i].name)
	od
	gs_line(dest)
end

proc wshowimporttable=
	[256]char str
	dllrec d
	importrec p


	gs_strln(dest,"Proc Dll List")
	int i
	for i:=1 to ndlls do
		gs_strint(dest,i)
		gs_str(dest,": ")
		gs_str(dest,dlltable[i].name)
		gs_str(dest," ")
		gs_strint(dest,dlltable[i].nprocs)
		gs_line(dest)
		gs_value("		Name Table Offset",dlltable[i].nametableoffset)
		gs_value("		Addr Table Offset",dlltable[i].addrtableoffset)
		gs_value("		DLL Name Offset  ",dlltable[i].dllnameoffset)
	od
	gs_line(dest)
	gs_strln(dest,"Proc Import List")

	for i:=1 to nimports do
		p:=importtable[i]

		gs_strint(dest,i)
		gs_str(dest,": ")
		if p.libno then
			strcpy(&.str,p.name)
			ipadstr(&.str,16)
			gs_str(dest,&.str)
			gs_str(dest," (")
			gs_str(dest,dlltable[p.libno].name)
			gs_strln(dest,")")

			gs_value("	IAT Offset        ",p.iatoffset)
			gs_value("	Thunk Offset      ",p.thunkoffset)
			gs_value("	Hint/Name Offset  ",p.hintnameoffset)

		else
			strcpy(&.str,p.name)
			ipadstr(&.str,20)
			gs_str(dest,&.str)
			gs_strln(dest," (---)")
		fi
	od
	gs_line(dest)
end

proc wshowsections=
	sectionrec s
	int i

	gs_strln(dest,"proc Section Headersxxx")
	gs_line(dest)

	for i:=1 to nsections do
		s:=sectiontable[i]

		gs_str(dest,"Section ")
		gs_strint(dest,i)
		gs_str(dest,": ")
		gs_str(dest,s.name)
		gs_str(dest,"  (")
		gs_str(dest,segmentnames[s.segtype])
		gs_strln(dest,")")

		gs_value("    Raw Offset",s.rawoffset)
		gs_value("    Raw Size",s.rawsize)
		gs_value("    Virtual Offset",s.virtoffset)
		gs_value("    Virtual Size",s.virtsize)
		gs_value("    Nrelocs",s.nrelocs)
		gs_value("    Data",int(s.data))
		gs_line(dest)

	od
end

=== aa_help.txt 0 1 16/16 ===
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
2 aa_cli.m
3 aa_decls.m
4 aa_genss.m
5 aa_lex.m
6 aa_lib.m
7 aa_objdecls.m
8 aa_mcxdecls.m
9 aa_parse.m
10 aa_tables.m
11 aa_writeexe.m
12 aa_writemcx.m
13 aa_writeobj.m
14 aa_disasm.m
15 aa_writess.m
16 aa_help.txt
