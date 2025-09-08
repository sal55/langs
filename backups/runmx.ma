=== MA 5 ===
=== runmx.m 0 0 1/5 ===
module runmxshow

module mx_decls
module mx_lib

!$sourcepath "/ax/"
module mc_disasm

!global  enumdata [0:]ichar segmentnames =
!	(no_seg=0,		$),
!	(code_seg,		$),
!	(idata_seg,		$),
!	(zdata_seg,		$),
!	(rodata_seg,	$),
!	(impdata_seg,	$),
!end

proc main=
	ref strbuffer ss
	ichar filename
	ref byte p
	ref librec plib
	int cmdskip, dorun

	dorun:=1
	for i to ncmdparams do
		filename:=cmdparams[i]
		if filename^='-' then
			if eqstring(filename,"-show") then
				dorun:=0
			else
				println "Unknown option:",filename
				stop 1
			fi
		else
			filename:=pcm_copyheapstring(addext(filename,"mx"))
			cmdskip:=i+$cmdskip
			exit
		fi
	else
		println "Usage:"
		fprintln "      #        filename[.mx]      Load and run .mx program", cmdparams[0]
		fprintln "      #  -show filename[.mx/.ml]  Load and display mx/ml file", cmdparams[0]
		stop 1
	od

CPL =$CMDSKIP
CPL =CMDSKIP

	initlogfile()

	plib:=loadmx(filename)
	fixuplib(plib)

	if dorun then
		runprogram(plib, cmdskip)
	else
		showlibs()
		closelogfile()
	fi

end

=== runmxshow.m 0 0 2/5 ===
int logdest=2

const logfile="rx.log"

ref void logdev		!dest for diagnostics and output of tables

strbuffer destv
ref strbuffer dest=&destv

global proc initlogfile=
	case logdest
	when 2 then
		remove(logfile)
		logdev:=cast(fopen(logfile,"w"))
	when 0,1 then
		logdev:=nil
	esac

end

global proc closelogfile=
	[512]char str

	if logdest=2 then
		fclose(logdev)

		print @&.str,f"\m\ed.bat",logfile

!		os_execwait(&.str,1,nil)
		os_execwait(&.str,0,nil)
	fi
end

global proc showlibs=
!show details of all libs, plus the global data
	showglobals(logdev)

	for i to nlibs do
!	for i to nlibs when libdefined[i] do
		showlib(libtable[i], logdev)
	od
end

global proc showlib(ref librec lib, filehandle logdev)=
	[300]char str
	u64 sig
	int dir,n
	ref byte q
	ref[]ichar names

	gs_init(dest)

	showstrln("-------------------------")
	showstr("LIBFILE: ")
	showstr(lib.libname)
	showstr(" ")
	showstrln(lib.filespec)

	print @str,"Version:",lib.version
	showstrln(str)

	showstrln("")

	fprint @str,"Zdata size: # #", lib.zdatasize, lib.zdataptr
	showstrln(str)

	fprint @str,"Idata size: # #", lib.idatasize, lib.idataptr
	showstrln(str)

	showsectiondata(lib.idataptr, lib.idatasize)

	fprint @str,"Code size: # # Extra:#", lib.codesize, lib.codeptr, lib.codexsize
	showstrln(str)
	showsectioncode(lib.codeptr, lib.codesize,lib.codexsize)
	showrelocs(lib)

	fprint @str,"DLL Libs #", n:=lib.ndlllibs
	showstrln(str)
	shownames(lib.dllnames,n)
	showstrln("")

	fprint @str,"Libs #", n:=lib.nlibs
	showstrln(str)
	shownames(lib.libnames,n)
	showstrln("")

	fprint @str,"Imports #", n:=lib.nimports
	showstrln(str)
	names:=lib.importnames
	for i to n do
		fprint @str,"   #: #", i, names[i]:"20jl"
		showstrln(str)
	od
	showstrln("")
!	shownames(lib.imports,n)

	fprint @str,"Exports #", n:=lib.nexports
	showstrln(str)
	names:=lib.exports
	showstrln("     Name                 Seg      Offset")
	showstrln("--------------------------------------------")

	for i to n do
		fprint @str,"#: # # #",
			i:"3", names[i]:"20jl",
			rsegmentnames[lib.exportsegs[i]]:"8jl",
			lib.exportoffsets[i]:"8zh"
		showstrln(str)
	od
	showstrln("")


	fprint @str,"Entry point offset:  #",lib.entryoffset
	showstrln(str)
	fprint @str,"Entry point address: #",lib.entryaddr
	showstrln(str)
	showstrln("")

FINISH:

	gs_println(dest,logdev)
end

proc showstr(ichar str)=
	gs_str(dest, str)
end

proc showstrln(ichar str)=
	gs_strln(dest, str)
end

proc showstrint(int a)=
	gs_strint(dest, a)
end

proc shownames(ref[]ichar names, int n)=
	[300]char str
	for i to n do
		fprint @str,"   #: #", i, names[i]
		showstrln(str)
	od
end

proc showrelocs(ref librec lib)=
	[300]char str
	mcxreloc r
	int n:=lib.nrelocs, m
	u64 targetoffset
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	fprint @str,"Relocs #", n:=lib.nrelocs
	showstrln(str)

	showstrln("     Type       Seg      Offset    Symbol/Target+Offset")
	showstrln("---------------------------------------------------------")

	for i to n do
		r:=lib.reloctable[i]
		fprint @str,"#: # # ##",
			i:"3", mcxrelocnames[r.reloctype]:"10jl",
			rsegmentnames[r.segment]:"8jl",
			r.offset:"8zh",,"  "

		m:=strlen(str)
		case r.reloctype
		when locabs32_rel, locabs64_rel then
			case r.segment
			when code_rseg then baseptr64:=cast(lib.codeptr+r.offset)
			when idata_rseg then baseptr64:=cast(lib.idataptr+r.offset)
			esac
			if r.reloctype=locabs32_rel then
				targetoffset:=baseptr32^
			else
				targetoffset:=baseptr64^
			fi

!CPL =BASEPTR32, =BASEPTR64,=BASEPTR32^, =BASEPTR64^


			print @&.str+m, rsegmentnames[r.targetsegment]:"6jlt:",,targetoffset:"8zh"
		else
!CPL "RELOC/OTHER",MCXRELOCNAMES[R.RELOCTYPE],R.STINDEX
			print @&.str+m, lib.importnames[r.stindex]
		esac
		showstrln(str)
	od
	showstrln("")
end

proc showsectiondata(ref byte p, int length)=
	int i,k,bb
	[128]char str,str2

	showstr("proc Section ")
	showstr("Idata:")
	showstr(" Size:")
	showstrint( length)
	gs_line(dest)
	gs_line(dest)

	k:=0

	str[1]:=0

	ref byte baseaddr:=nil

	print @&.str2,baseaddr:"Z8H",,": "

	showstr(&.str2)

	for i:=1 to length do
		bb:=p++^
		print @&.str2,bb:"z2H",," "
		showstr(&.str2)

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
					showstr("   ")
					strcat(&.str," ")
				od
			fi
			showstr("	[")
			showstr(&.str)
			showstrln("]")
			k:=0
			str[1]:=0
			baseaddr+:=16
			print @&.str2,baseaddr:"z8h",,": "
			showstr(&.str2)
		fi
	od
	if k=0 then
		gs_line(dest)
	fi

	gs_line(dest)
	if k then gs_line(dest) fi
end

proc showsectioncode(ref byte p, int length, extra)=
	ref byte codeptr,codeend, codeendx,codestart
	int offset
	ichar s
	[16]char str

	showstrln( "proc Section Code")

	codestart:=codeptr:=p
	codeend:=codeptr+length
	codeendx:=codeend+extra

!	ref byte baseaddr:=cast(imagebase+p.virtoffset)
	ref byte baseaddr:=nil

	while codeptr<codeendx do
		if codeptr=codeend then
			showstrln("")
		fi
		offset:=codeptr-codestart
!S:=NIL
		s:=decodeinstr(codeptr,baseaddr+offset)
		exit when s=nil

		print @&.str,offset:"4",," "
		showstr(&.str)

		showstrln(s)
	od

	gs_line(dest)
end

global proc showglobals(filehandle logdev)=
	[300]char str
	[300]char name

	gs_init(dest)
	showstrln("Global Tables\n")

	print @str, "DLLs:",ndlllibs
	showstrln(str)

	for i to ndlllibs do
		print @str,i,,":",dllnametable[i]:"16jl", dllinsttable[i]:"h"
		showstrln(str)
	od
	showstrln("")

	print @str, "LIBs:",nlibs
	showstrln(str)

	for i to nlibs do
		print @str,i,,":",libnametable[i]:"20jl", (librelocated[i]|"Relocated"|"-")
		showstrln(str)
	od
	showstrln("")

	print @str, "Global Symbols:",nsymbols
	showstrln(str)

	showstrln("     Name              Def Address       Lib        Dll")
	showstrln("-----------------------------------------------------------")

	for i to nsymbols do
		strcpy(name,symbolnametable[i])
		if strlen(name)>17 then
			strcat(name,"\n                      ")
		fi
		fprint @str,"#: # # #  # #",
			i:"3",
!			symbolnametable[i]:"17jl",
			&.name:"17jl",
			(symboldefined[i]|"Y"|"-"):"3JL",
			symboladdress[i]:"Z12H",
			(symbollibindex[i]|libnametable[symbollibindex[i]]|"-"):"10jl",
			(symboldllindex[i]|dllnametable[symboldllindex[i]]|"-"):"10jl"
!symboldllindex[i]
		showstrln(str)
	od
	showstrln("")

	gs_println(dest,logdev)
end
=== mx_decls.m 0 0 3/5 ===
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
=== mx_lib.m 0 0 4/5 ===
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

=== mc_disasm.m 0 0 5/5 ===

const showmregs=1
!const showmregs=0

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
		print @str,baseaddr:"z6h",,": "
!	else
!		print @str,pstart:"z6h",,": "
!	fi

	n:=codeptr-pstart
	to n do
		print @str2,int(pstart++^):"z2H",," "

		strcat(str,str2)
	od
	to maxinstrlen-n do
		strcat(str,"-- ")
	od
	strcat(str,deststr)

	cptr:=codeptr

	return str
end

proc decodetwobyteinstr=
!0F has been decoded
	int opc,rhssize,third,imm
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

	mode:=modrm>>6
	xxx:=(modrm>>3) iand 7
	rm:=modrm iand 7

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
	return (ref i8(codeptr++))^
end

function readword16:word=
	word a
	a:=ref u16(codeptr)^
	codeptr+:=2
	return a
end

function readint16:int=
	int a
	a:=ref i16(codeptr)^
	codeptr+:=2
	return a
end

function readword32:word=
	word a
	a:=ref u32(codeptr)^
	codeptr+:=4
	return a
END

function readint32:int=
	int a
	a:=ref i32(codeptr)^
	codeptr+:=4
	return a
END

function readi64:i64=
	i64 a
	a:=ref i64(codeptr)^
	codeptr+:=8
	return a
END

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
			genhex(offset)
		else
			if offset>0 then genstr(plus) fi
			genintd(offset)
		fi
	fi
	genstr("]")
!	if addrmode=amrel then genstr("+RIP") fi
end

proc genstr(ichar s)=
	strcat(deststr,s)
end

proc genintd(i64 a)=
	genstr(strint(a))
end

proc genhex(i64 a)=
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

function readimm8:i64=
!like readimm but can 8 bytes too
	if opsize<8 then return readimm() fi
	return readi64()
end

function strxmm(int reg)ichar=
	static [32]char str
	print @str,"xmm",,reg-1
	return str
end

function strmmx(int reg)ichar=
	static [32]char str

	print @str,"mmx",,reg-1
	return str
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
=== END ===
1 runmx.m 0 0
2 runmxshow.m 0 0
3 mx_decls.m 0 0
4 mx_lib.m 0 0
5 mc_disasm.m 0 0
