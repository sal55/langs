=== MA 38 ===
=== mm.m 0 0 1/38 ===
!project =
	module mm_cli
	module mm_blockpcl
	module mm_assem
	module mm_decls

	module mm_diags
!	module mm_diags_dummy

	module mm_export_dummy
!	module mm_exportq
!	module mm_exportm

	module mm_genpcl
	module mm_lex
	module mm_lib
	module mm_libpcl

	module mm_libsources
!	module mm_libsources_dummy

	module mm_modules
	module mm_name
	module mm_parse
	module mm_pcl
	module mm_support
	module mm_tables
	module mm_type
!	module mm_types

!x64 and exe backend

	module mc_genmcl
	module mc_genss
	module mc_libmcl
	module mc_decls as md
	module mc_objdecls
	module mc_optim
	module mc_stackmcl
	module mc_writeexe

!	module mc_writess
!	module mc_disasm

!'run' and mx/ml backend


	module mx_run
	import mx_lib
!	module mx_run_dummy

!Experimental PXL stuff
!	import pcl
!	module px_pxl
!	module px_show


!end

proc main=
	main2()
end

=== mx_lib.m 0 0 2/38 ===
!subprog libmx
!module mx_lib
!module mx_show
module mx_decls
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

global function readlibfile(ichar filespec, ref byte p)ref librec plib=
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

function readbyte(ref byte &p)int=
	return p++^
end

function readu32(ref byte &p)u64 x=
	x:=ref u32(p)^
	p+:=4
	x
end

function readstring(ref byte &p)ichar s=
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

export proc loadmemmcu(ref librec lib)=
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

global function findlib(ichar name)int n=
!find an existing library existing

	for i to nlibs do
		if eqstring(name,libnametable[i]) then return i fi
	od
	return 0
end

global function mxaddlib(ichar name)int n=
!add a new lib slot with given name
	if nlibs>=maxlibs then 
		error("Too many libs")
	fi

	libnametable[++nlibs]:=name
	return nlibs
end

export proc fixuplib(ref librec lib)=
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

function finddllsymbol(ichar name, int &dllindex)ref void p=
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

global function loadlibfile(ichar filename, int libno)ref librec plib=
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

function readmxfile(ichar filename)ref byte p=
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

function addsymbol(ichar name)int=
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

export proc runprogram(ref librec lib, int cmdskip=0)=
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

export func findsymbol(ichar name)ref void=

	for i to nsymbols do
		if eqstring(symbolnametable[i], name) then
			return symboladdress[i]
		fi
	od
	return nil
end

export function loadmx(ichar filename)ref librec plib=
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

export function loadmemmcb(ichar filename, ref byte p)ref librec plib=
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

=== mx_decls.m 0 0 3/38 ===
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

export const mcxsig = 'MCX\e'

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
=== mm_cli.m 0 0 4/38 ===

global ichar syslibname=""

!macro SHOW(m) = println m
macro SHOW(m) = eval 0

global enumdata []ichar passnames =
	(load_pass,		$),
	(parse_pass,	$),
	(fixup_pass,	$),
	(name_pass,		$),
	(type_pass,		$),
	(pcl_pass,		$),
	(mcl_pass,		$),		!all-inclusive up to this point (includes all prev passes)
	(ss_pass,		$),		!only one of these 3 will be done
	(exe_pass,		$),		!
end

!global [passnames.bounds]byte dopassfile
global [passnames.len]byte dopassfile

enumdata []ichar optionnames, []byte optionvalues =

	(load_sw,		"load",			load_pass),
	(fixup_sw,		"fixup",		fixup_pass),
	(parse_sw,		"parse",		parse_pass),
	(name_sw,		"name",			name_pass),
	(type_sw,		"type",			type_pass),
	(pcl_sw,		"pcl",			pcl_pass),
	(mcl_sw,		"mcl",			mcl_pass),
	(asm_sw,		"a",			mcl_pass),
	(obj_sw,		"obj",			mcl_pass),
	(dll_sw,		"dll",			exe_pass),
	(exe_sw,		"exe",			exe_pass),
	(run_sw,		"r",			ss_pass),

	(sys_sw,		"sys",			2),
	(minsys_sw,		"min",			1),
	(nosys_sw,		"nosys",		0),
!	(nofile_sw,		"nofile",		0),

	(ma_sw,			"ma",			1),

	(opt_sw,		"opt",			0),
	(peephole_sw,	"peep",			0),
	(regoptim_sw,	"regs",			0),

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

	(getst_sw,		"getst",		0),
	(getproj_sw,	"getproj",		0),

	(time_sw,		"time",			0),
	(v_sw,			"v",			2),
	(vv_sw,			"vv",			3),
	(quiet_sw,		"q",			0),
	(help_sw,		"h",			0),
	(help2_sw,		"help",			0),
	(ext_sw,		"ext",			0),
	(out_sw,		"o",			0),
	(outpath_sw,	"outpath",		0),
	(unused_sw,		"unused",		0),

	(rip_sw,		"rip",			1),
	(himem_sw,		"himem",		2),
end

byte fobj
byte fdll
byte fexe
byte frun
byte fmcl

byte msfile

global const logfile="mx.log"

ichar outext=""				!for reporting of primary o/p file

int startclock,endclock
int cmdskip

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
!	unit p,q,r
!	int m,fileno,ntokens,t,tt
!
!CPL =SUBPROGREC.BYTES
!CPL =MODULEREC.BYTES
!CPL =PCLREC.BYTES
!CPL =unitREC.BYTES
!CPL =STREC.BYTES
!CPL =mclrec.BYTES
!!CPL =unitREC.mode
!CPL =SYMBOLNAMES.LEN

	startclock:=os_clock()

	initdata()

	getinputoptions()

	loadproject(inputfile)

	if fverbose>=1 then
		if frun then
			if not msfile or fverbose>1 then
				println "Compiling",inputfile,"to memory"
			fi
		else
!			fprint "Compiling # to #",inputfile:"14jlp-",changeext(outfile,outext),$
			fprint "Compiling # to #",inputfile,changeext(outfile,outext),$
			println
		fi
	fi

	remove(logfile)

!CPL "COMPILING", INPUTFILE, $TIME

	do_parse()

	do_name()

	do_type()

	do_writema(inputfile)

	do_getinfo(inputfile)

	do_writeexports()

	do_genpcl()

	do_genmcl()

	do_genss()

	do_genexe()

	if fobj then do_obj() fi

	if fexe or fdll then do_exe() fi

	if frun then do_run() fi

!CPL =MCLSEQNO
!CPL =NPCL
!CPL =NUNITS
!CPL =NSYMBOLS
!CPL =NDEFS

!CPL =NTO
!CPL =NWHILE
!CPL =NREPEAT
!CPL =NFOR
!CPL =NDOCASE
!CPL =NDOSWITCH
!CPL =NEXIT
!CPL =NREDO
!CPL =NNEXT
!

	if debugmode then showlogfile() fi

	if fshowtiming then
		endclock:=os_clock()
		compiletime:=endclock-startclock
		showtimings()
	fi

	if fverbose>=2 then
		println "Finished."
	fi
end

proc do_parse=
	return when passlevel<parse_pass

	if fverbose=3 then println "PARSE",dopassfile[parse_pass] fi

	int tt:=clock()

	for i to nmodules do
		parsemodule(modules[i])
	od
	parsetime:=clock()-tt

	if passlevel>=fixup_pass then
		if fverbose=3 then println "FIXUP" fi
		fixusertypes()
	fi

	fixstartprocs()
!
	if dopassfile[parse_pass] then showast("AST1") fi
end

proc do_name=
	return when passlevel<name_pass

	if fverbose=3 then println "NAME",dopassfile[name_pass] fi

	int tt:=clock()
	rx_typetable()

!	TX_TYPETABLE()

	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
	resolvetime:=clock()-tt

	if dopassfile[parse_pass] then showast("AST2") fi
end

proc do_type=
	return when passlevel<type_pass
	if fverbose=3 then println "TYPE",dopassfile[type_pass] fi

	int tt:=clock()
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od
	tx_allprocs()
	typetime:=clock()-tt

	if dopassfile[type_pass] then showast("AST3") fi
end

proc do_genpcl=
	return when passlevel<pcl_pass
	if fverbose=3 then println "GENPCL",dopassfile[pcl_pass] fi

	int tt:=clock()

	codegen_pcl()

	pcltime:=clock()-tt


	if dopassfile[pcl_pass] then
		pcl_writepclfile(changeext(outfile, "pcl"))
	fi

end

proc do_genmcl=
	return when passlevel<mcl_pass
	if fverbose=3 then println "GENMCL",dopassfile[mcl_pass] fi

	int tt:=clock()

	genmcl()
	mcltime:=clock()-tt
!CPL =MCLTIME

	if dopassfile[mcl_pass] then
		ref strbuffer asmstr
		asmstr:=getmclstr()
		writegsfile(changeext(outfile, "asm"), asmstr)
		gs_free(asmstr)
	fi

end

proc do_genss=
	return when passlevel<ss_pass
	if fverbose=3 then println "GENSS",dopassfile[ss_pass] fi

	int tt:=clock()
	genss()
	sstime:=clock()-tt

end

proc do_genexe=
	return when passlevel<exe_pass
	if fverbose=3 then println "GENEXE",dopassfile[exe_pass] fi

	int tt:=clock()

!	initsectiontable()
	genexe(nil, changeext(outfile, outext), fdll)

	exetime:=clock()-tt
end

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
end

proc getinputoptions=
	int paramno,pmtype,sw,ncolons,passfixed,extlen
	ichar name,value,ext
	[300]char filespec

	prodmode:=1
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
					do_option(sw,value)
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

			if frun then
				cmdskip:=paramno-1+$CMDSKIP
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
		fexe:=1
		outext:="exe"
		prodmode:=1
	elsif prodmode and passlevel=0 then
		passlevel:=exe_pass
		fexe:=1
		outext:="exe"
	elsif debugmode and passlevel=0 then
		passlevel:=mcl_pass
		dopassfile[mcl_pass]:=1
		outext:="asm"
	fi
	if fmcl then dopassfile[mcl_pass]:=0 fi

	if passlevel<exe_pass then fshowss:=0 fi

	if msyslevel=-1 then
		msyslevel:=(prodmode|2|0)
	fi
	if fobj or fdll then highmem:=2 fi

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "	",,cmdparams[0],"filename[.m]     # Compile project to executable"
		println "	",,cmdparams[0],"-help            # Other options"
		stop

	else
!default output
		outfile:=pcm_copyheapstring(inputfile)
		if fwritema then
			outext:="ma"
		fi

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
	if eqstring(filespec, inputfile) and passlevel>=exe_pass and not frun then
		strcpy(&.filespec+strlen(filespec)-extlen-1, "2.m")
		outfile:=pcm_copyheapstring(filespec)
		println "New dest=",outfile
	fi

!	asmfilename:=getoutfilename(outfile,"asm")
!	pclfilename:=getoutfilename(outfile,"pcl")
!	objfilename:=getoutfilename(outfile,"obj")
!	exefilename:=getoutfilename(outfile,"exe")
!	libfilename:=getoutfilename(outfile,(libmode|"ml"|"mx"))
!	dllfilename:=getoutfilename(outfile,"dll")

!	strcpy(filespec,changeext(outfile,""))
!	strcat(filespec,"_lib")
!	mexpfilename:=getoutfilename(filespec,"m")
!	expfilename:=getoutfilename(outfile,"q")
end

proc do_option(int sw, ichar value)=
	static byte outused, outpathused

	if sw in load_sw .. run_sw then
		passlevel:=optionvalues[sw]
		outext:=optionnames[sw]
		if sw=asm_sw then
			outext:="asm"
		fi

	fi

	switch sw

!	when load_sw then
!	when parse_sw then
!	when fixup_sw then
!	when name_sw then
!	when type_sw then
!	when pcl_sw then
!	when asm_sw then
	when mcl_sw then
		fmcl:=1
	when obj_sw then
		fobj:=1

	when dll_sw then
		libmode:=1
		fdll:=1

	when exe_sw then
		fexe:=1

	when run_sw then
		frun:=1

	when ma_sw then
		fwritema:=1

	when ast1_sw then
		dopassfile[parse_pass]:=1

	when ast2_sw then
		dopassfile[name_pass]:=1

	when ast3_sw then
		dopassfile[type_pass]:=1

	when showpcl_sw then
		dopassfile[pcl_pass]:=1

	when showasm_sw then
		dopassfile[mcl_pass]:=1

	when st_sw then
		fshowst:=1

	when stflat_sw then
		fshowstflat:=1

	when types_sw then
		fshowtypes:=1

	when showss_sw then
		dopassfile[ss_pass]:=1

	when showmodules_sw then
		fshowmodules:=1

	when getst_sw then fgetst:=1
	when getproj_sw then fgetproj:=1

	when sys_sw, minsys_sw, nosys_sw then
		msyslevel:=optionvalues[sw]

!	when nofile_sw then fnofile:=1

	when opt_sw then fpeephole:=1; fregoptim:=1
	when peephole_sw then fpeephole:=1
	when regoptim_sw then fregoptim:=1

	when time_sw then fshowtiming:=1

	when v_sw, vv_sw, quiet_sw then fverbose:=optionvalues[sw]

	when help_sw,help2_sw then showhelp(); stop

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

	end switch

	if passlevel in [pcl_pass, mcl_pass, exe_pass] then
		dopassfile[passlevel]:=1
	fi
	for i to passnames.len do
		if i>passlevel then dopassfile[i]:=0 fi
	od

	if fnofile then
		clear dopassfile
	fi

	if sw in ast1_sw .. showmodules_sw then
		debugmode ior:=1
	fi
end

proc showcaption=
	println "M Compiler [M6.41]", $date, $time
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

!CPL "WX",PASSNAMES[PASSLEVEL],=LIBMODE
	if not fdll or not libmode then return fi

	strcpy(str, extractbasefile(outfile))
	writeexports(outfile, changeext(str, "dll"))
!	stop
end

function getoutfilename(ichar file,ext)ichar=
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

function addstartproc(symbol owner, ichar name, int scope,moduleno)symbol stproc=
	stproc:=getduplnameptr(owner,addnamestr(name),procid)
	stproc.scope:=scope
	stproc.moduleno:=moduleno
	stproc.subprogno:=moduletosub[moduleno]
	stproc.code:=makeblock(nil)
	adddef(owner,stproc)
	addtoproclist(stproc)

	return stproc
end

proc do_obj=
	[300]char str

	fprint @str, "aa -q -obj # -out:#", changeext(outfile,"asm"), changeext(outfile, "obj")
!	fprint @str, "aa -q -obj # ", changeext(outfile, "asm")

	if fverbose=3 then println str fi

	if system(str)<>0 then
		println "Couldn't assemble"
		stop 1
	fi
end

proc do_exe=
	writeexe(changeext(outfile, outext), fdll)
end

proc do_run=
!cpl "RUN"
	runlibfile("dummy", cmdskip)
!	runlibfile("dummy")
end

!global function writeobjfile(ichar filename)int=
!	[300]char str
!
!!LOADERROR("WRITEOBJ")
!	writeasmfile(asmfilename)
!
!	fprint @str, "aa -q -obj # -out:#", asmfilename, filename
!
!	if fverbose=2 then println str fi
!
!	if system(str)<>0 then
!		println "Couldn't assemble"
!		stop 1
!	fi
!	return 1
!end

!global function writeasmfile(ichar filename)int=
!	codegen_pcl()
!!LOADERROR("WRITEASM")
!
!	genmcl()
!
!	ref strbuffer asmstr
!	asmstr:=getmclstr()
!	writegsfile(filename,asmstr)
!	gs_free(asmstr)
!
!	return 1
!end
!
=== mm_blockpcl.m 0 0 5/38 ===
const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

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

global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)
macro evallv(p) = evalref(p)
macro evalunitx(p, isref) = (isref|evalref(p)|evalunit(p))
macro evalblock(p) = evalunit(p)

global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a,b,c

	if p=nil then return fi
	mlineno:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	switch p.tag
	when jconst         then do_const(p)
	when jnull          then
	when jname          then do_name(p)
	when jblock then
		do_block(p)
	when jcallproc, jcallfn then
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
		genpc(keval)
	when jandl          then do_andl(p,a,b)
	when jorl           then do_orl(p,a,b)

	when jmakerange     then GENPC_COMMENT("MAKERANGE")

	when jcmp           then do_setcc(p,a,b)
	when jcmpchain      then do_setccchain(p,a)

	when jbin           then do_bin(p,a,b)
	when jindex         then do_index(p,a,b)
	when jslice         then
		doslice(p,a,b, newblocktemp(p.mode), p.resultflag)

	when jmakeslice     then
		evalunit(a)
		evalunit(b)
		genpc((p.resultflag|kstoresliced|kstoreslice), genmem_d(newblocktemp(p.mode)))
		setmode(tslice)

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

	when jincr          then
		if p.pclop in [kincr, kdecr] then
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
		genpc(kassem,genpc_assem(p))
		setmode_u(p)

	when jcvlineno      then
		genpc(kload,genpc_int(getlineno(p.pos)))

	when jclear         then do_empty(p,a)
	else
		GERROR_S("UNSUPPORTED TAG ",JTAGNAMES[P.TAG])
		return
	end switch

	if p.mode<>tvoid and not p.resultflag then
		case p.tag
		when jassign, jcallproc, jsyscall then

		else
!CPL "UNLOADING?"
IF NOT JSOLO[P.TAG] THEN
GERROR("NOT ALLOWED BY ITSELF")
FI

			genpc(kunload)
			setmode_u(p)
		esac
	fi
end

proc evalref(unit p, q=nil)=
	unit a,b,c
	a:=p.a
	b:=p.b
	c:=p.c
	mlineno:=p.pos

!CPL "EVALREF",JTAGNAMES[P.TAG]


	case p.tag
	when jname then
		genpushmemaddr_d(p.def)
		if q then					!addrof may have optional byte offset
!CPL "EVALREF",P,Q,Q.VALUE
			genpushint(q.value)
			genpc(kaddrefx)
			pcl_setscale(1)
			setmode(tu8)
		fi
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
		elsif ttisblock[p.mode] then
			evalunit(p)

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
		genpc(ksliceptr)
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

	when jnotl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt,q,lab)
		when kjumpt then
			genjumpcond(kjumpf,q,lab)
		esac

	when jistruel then
		evalunit(q)

		genpc((opc=kjumpt|kjumptrue|kjumpfalse),genpc_label(lab))
		setmode_u(q)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc,q,lab)

	when jcmp then

		gcomparejump(opc,p.pclop,q,r,lab)

	when jinrange then
		evalunit(q)

		if opc=kjumpt then
			lab2:=createfwdlabel()
			evalunit(r.a)
			genpc(kjumplt, genpc_label(lab2))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			genpc(kjumple, genpc_label(lab))
			setmode_u(q)
			definefwdlabel(lab2)
		else
			evalunit(r.a)
			genpc(kjumplt, genpc_label(lab))
			setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			genpc(kjumpgt, genpc_label(lab))
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
					genpc(kjumpeq, genpc_label(lab2))
					pccurr.popone:=1
				else
					genpc(kjumpne, genpc_label(lab))
				fi
				setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s, s:=s.nextunit do
				evalunit(s)
				genpc(kjumpeq, genpc_label(lab))
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
					genpc(kswapopnds)

					genpc(condtopclop(reversecond_order(reversecond(p.cmpgenop[i])),kjumpeq), genpc_label(lab))

					pccurr.popone:=1
				else
					genpc(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq), genpc_label(lab))
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
					genpc(kswapopnds)
					genpc(condtopclop(reversecond_order(reversecond(p.cmpgenop[i])),kjumpeq), genpc_label(lab2))
					pccurr.popone:=1
				else
					genpc(condtopclop(p.cmpgenop[i],kjumpeq), genpc_label(lab))
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
		if p.mode not in [ti64,tu64,tbool64] then gerror_s("jumptrue/not i64:",strmode(p.mode)) fi

		genpc((opc=kjumpt|kjumptrue|kjumpfalse),genpc_label(lab))
		setmode(ti64)
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

	genpc(condtopclop(cond,kjumpeq),genpc_label(lab))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	genpc(kjump,genpc_label(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #",mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] or mode=tbool then
		genpushint(p.value)
	elsif ttisreal[mode] then
		if ttsize[mode]=4 then
			genpushreal32(p.xvalue)
		else
			genpushreal(p.xvalue)
		fi

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
	when labelid then
		if d.index=0 then
			d.index:=++mlabelno
		fi
		if p.resultflag then		!get label address
			genpc(kloadlabel, genpc_label(d.index))
		else
			genpc(kjump, genpc_label(d.index))
		fi
		p.resultflag:=0
		p.mode:=tvoid
!
	when fieldid then
		genpushint(d.offset)


	else
		genpushmem_d(d)
		setmode(getmemmode_m(p))
	esac
end

proc do_stop(unit p,a) =
	if a then
		evalunit(a)
	else
		genpc(kload,genpc_int(0))
	fi
	genpc(kstop)
end

proc do_andl(unit p,a,b) =
	int labfalse, labend

	genpc(kstartmx)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf,a,labfalse)
	genjumpcond(kjumpf,b,labfalse)

	genpushint(1)
	genpc(kresetmx)

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	genpc(kendmx)

	definefwdlabel(labend)
end

proc do_orl(unit p,a,b) =
	int labtrue, labfalse, labend

	genpc(kstartmx)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt,a,labtrue)
	genjumpcond(kjumpf,b,labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	genpc(kresetmx)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	genpc(kendmx)

	definefwdlabel(labend)
end

proc do_notl(unit p,a) =
	evalunit(a)
	genpc(p.pclop)
	setmode(ti64)
end

proc do_istruel(unit p,a) =
	evalunit(a)
	if a.mode=tbool then
		return
	fi
	genpc(p.pclop)
	setmode_u(a)
end

proc do_typepun(unit p, a) =
	evalunit(a)
	setmode_u(a)

	if a.tag=jname then
		a.def.addrof:=1
	fi

	if a.mode=p.mode then return fi
	genpc(ktypepun)
	setmode(p.convmode)
	pcl_setoldtype(a.mode)
!	pccurr.oldmode:=ttbasetype[a.mode]
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

proc do_assign(unit p,a,b) =
!fstore=1 when result is needed

!CPL "ASSIGN"
!PRINTUNIT(P)

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
		genpc(kdupl)
	fi

	case a.tag
	when jname then
		if a.def.nameid in [procid, dllprocid, labelid] then GERROR("Assign to proc?") fi

		genpc(kstore, genmem_u(a))

	when jptr then
		evalref(a)

storeptr:
		if pccurr.opcode=kaddrefx then 
			pccurr.opcode:=kistorex
		else
			genpc(kistore)
		fi
		setmode(getmemmode_m(a))

	when jdotindex then
		evalref(a.a)
		evalunit(a.b)
		genpc(kstorebit)
		setmode_u(a.a)
		return
	when jdotslice then
		evalref(a.a)
		evalunit(a.b.a)
		evalunit(a.b.b)
		genpc(kstorebf)
		setmode_u(a.a)
		return

	when jif then
		do_if(a, a.a, a.b, a.c, 1)
		storeptr

	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	esac

	setmode_u(a)
end

proc do_bin(unit p,a,b) =
	int offset

	evalunit(a)

	if pccurr.opcode=kaddrefx and
			p.pclop in [kaddrefx, ksubrefx] and
		ttisref[a.mode] and ttisinteger[b.mode] and b.tag=jconst then
		offset:=ttsize[tttarget[a.mode]]*b.value
		if p.pclop=kaddrefx then
			pcl_addoffset(offset)
		else
			pcl_addoffset(-offset)
		fi
		return
	fi

	evalunit(b)

	genpc(p.pclop)
	setmode_u(p)

!IF P.PCLOP=KADD THEN
!++NALLADDS
!	IF A.TAG IN [JCONST, JNAME] AND B.TAG IN [JCONST,JNAME] THEN
!		++NSIMPLEADDS
!	FI
!FI


	if ttisref[a.mode] and ttisinteger[b.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi

	if p.pclop=ksubref and ttisref[a.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi
end

proc do_setcc(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	genpc(condtopclop(p.pclop,kseteq))
	setmode_u(a)
end

proc do_setccchain(unit p,q) =
	int lab1,lab2,i,cond
	unit r

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	i:=1

	genpc(kstartmx)

	evalunit(q)
	while r do
		evalunit(r)
		cond:=reversecond(p.cmpgenop[i])
		if r.nextunit then
			genpc(kswapopnds)
			cond:=reversecond_order(cond)
		fi

!		genpc_cond(kjumpeq, cond, genlabel(lab1))
		genpc(condtopclop(cond,kjumpeq), genpc_label(lab1))
		if r.nextunit then pccurr.popone:=1 fi

		setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	od

	genpushint(1)
	genpc(kresetmx)
	genpc(kjump, genpc_label(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	genpc(kendmx)
	definefwdlabel(lab2)
end

proc do_binto(unit p,a,b)=
	evallv(a)
	evalunit(b)

	genpc(p.pclop)
	setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi

end

proc do_unary(unit p,a) =
	evalunit(a)

	genpc(p.pclop)
	setmode_u(p)
	if p.pclop in [klen,kupb] and ttbasetype[a.mode]=tslice then
!		if p.pclop=kupb then
			pccurr.slicelwb:=ttlower[a.mode]
!			pcl_setxy(ttlower[a.mode],0)
!		fi

		setmode(tslice)
	fi

end

proc do_unaryto(unit p,a)=
	evallv(a)

	genpc(p.pclop)
	setmode_u(a)
end

proc do_ptr(unit p,a)=
	pcl pprev

	evalunit(a)

	case pccurr.opcode
	when kaddrefx then 
		pccurr.opcode:=kiloadx
	when ksubrefx then
		pprev:=pccurr-1
		if pprev.opcode=kload and pprev.opndtype=int_opnd then
			pprev.value:=-pprev.value
			pccurr.opcode:=kiloadx
		fi
	else
		genpc(kiload)
	esac
	setmode(getmemmode_m(p))
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.index=0 then
		d.index:=++mlabelno
	fi
	print @&.str,d.name,,"::"
	genpc_comment(&.str)
	genpc(klabel,genpc_label(d.index))
end

proc do_goto(unit a)=
	symbol d

	if a.tag=jname and a.def.nameid=labelid then
		d:=a.def
		if d.index=0 then
			d.index:=++mlabelno
		fi
		genpc(kjump, genpc_label(d.index))
	else
		evalunit(a)
		genpc(kjumpptr)
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
	genpc(kstore,genmem_u(cvar))
	setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b,lab_c,lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		genpc(kjumple,genpc_label(lab_d))
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

	genpc(kto,genpc_label(lab_b))
	genpc(kopnd,genmem_u(cvar))

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
	if ismult then genpc(kstartmx) fi

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf,pcond,lab2)

		evalunitx(plist,isref)
		if ismult then genpc(kresetmx) fi

		if pcond.nextunit or pelse then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
	od

	if pelse then
		evalunitx(pelse,isref)
		if ismult then genpc(kendmx) fi
	fi
	definefwdlabel(labend)
end

proc do_return(unit p,a) =
!	if a then
!		evalunit(a)
!
!		genpc(ksetret)
!		setmode_u(a)
!	fi
!	genjumpl(retindex)
	if a then
		evalunit(a)

		genpc(kjumpret, genpc_label(retindex))
		setmode_u(a)
	else
!		genpc(kjumpret, genpc_label(retindex))
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
	genpc_x(ksetretmult, nparams)

	genjumpl(retindex)
	p.resultflag:=1
end

proc do_callproc(unit p,a,b) =
	[maxparams]unit paramlist
	int nparams,isptr,nvariadics, blockret, nret, isfn
	int nfixedparams
	symbol d,dblock,e
	ref[]int32 pmult
	unit q

	isptr:=0
	isfn:=p.tag=jcallfn

	case a.tag
	when jname then
		d:=a.def

	when jptr then
		d:=ttnamedef[a.mode]
		isptr:=1
	else
		gerror("call/not ptr")
	esac

	nparams:=0
	nvariadics:=0
	blockret:=0

	if ttisblock[p.mode] then
		blockret:=1
		nparams:=1
		paramlist[1]:=nil			!will be extra blocktemp
	fi

	q:=b
	nfixedparams:=blockret
	e:=d.deflist
	while e, e:=e.nextdef do
		if e.nameid=paramid then ++nfixedparams fi
	od

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparams then gerror("maxparams") fi
		paramlist[++nparams]:=q

		if d.varparams and nparams>=nfixedparams and nparams<=4 and nvariadics=0 then
!CPL "SETNV", =NPARAMS, =D.VARPARAMS, =NFIXEDPARAMS
			nvariadics:=nparams
		fi
	od
!CPL =NVARIADICS

	genpc(ksetcall)
	pccurr.nargs:=nparams

	for i:=nparams downto 1 do			!downto 
		q:=paramlist[i]
		if q then
			evalunit(q)
			if nvariadics and i>=nvariadics and pccurr.pcat=realcat and not pccurr.pwide then
				genpc(kfwiden)
				pccurr.psize:=8
				pccurr.pwide:=1
				pcl_setoldtype(tr32)
			fi

!			if i>4 then
				genpc(ksetarg)
				setmode_u(q)
				pccurr.x:=i
!			fi
		else								!temp block
			dblock:=newblocktemp(p.mode)
			dblock.used:=1
			genpc(kload, genmem_d(dblock))
			setmode(p.mode)
		fi
	od
!
!CPL =ISFN
!CPL =ISPTR

	if not isptr then
		genpc((isfn|kcallf|kcallp), genmemaddr_d(d))
	else
		evalunit(a.a)
		genpc((isfn|kicallf|kicallp))
	fi

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
		setmode(getmemmode_m(p))
	fi

	if d.nretvalues>1 and isfn then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			genpc(ktype)
!			genpc(kopnd, genint(0))
			setmode(pmult[i])
		od
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
	genpc(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pcl_setincr(1)

	if ttisref[m] then
		pcl_setincr(ttsize[tttarget[m]])
	fi
end

proc do_incrload(unit p,a) =
	evallv(a)
	genpc(p.pclop)
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

	case pto.tag
	when jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.parammode=out_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	when jconst, jname then
	else
		if pto.mode=ti64 then
			ptemp:=createname(newblocktemp(ti64))
			ptemp.mode:=ti64
			ptemp.resultflag:=1
			evalunit(pto)
			genpc(kstore, genmem_u(ptemp))
			setmode(ti64)
			pto:=ptemp
		else
			gerror("Complex TO")
		fi
	esac

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
	genpc(kstore,genmem_u(pindex))
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
			genpc(kjump, genpc_label(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			genpc((down|kjumpgt|kjumplt),genpc_label(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			genpc((down|kjumplt|kjumpgt),genpc_label(lab_e))
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
		genpc_x((down|kfordown|kforup),stepx, genpc_label(lab_b))
		setmode_u(pindex)
	else
		genpc_x((down|kfordown|kforup),1, genpc_label(lab_b))
		setmode_u(pindex)
	fi

	genpc(kopnd, genmem_u(pindex))
	case pto.tag
	when jconst then
		genpc(kopnd, genpc_int(pto.value))
	when jname then
		genpc(kopnd, genmem_u(pto))
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
	genpc(kstore, genmem_u(pindex))
	setmode_u(pindex)

!CPL"-------------FORALL",JTAGNAMES[PTO.TAG]


	if pto.tag not in [jconst, jname] then
		evalunit(pto)
		dto:=getavname(currproc)
		genpc(kstore, genmem_d(dto))
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
			genpc(kjump, genpc_label(lab_e))
		fi
	else
!CPL "FOR/FROM/NOT CONST"
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
!CPL =DOWN
			genpc((down|kjumpgt|kjumplt),genpc_label(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			genpc((down|kjumplt|kjumpgt),genpc_label(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	genpc_x((down|kfordown|kforup),1, genpc_label(lab_b))
	setmode_u(pindex)

	genpc(kopnd, genmem_u(pindex))
	case pto.tag
	when jconst then
		genpc(kopnd, genpc_int(pto.value))
	when jname then
		genpc(kopnd, genmem_u(pto))
	else
		GENPC(KOPND, GENMEM_D(DTO))
!		gerror("forall/to: not const or name")
	esac

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
	genpc(kswap)
	setmode_u(a)
end

proc do_convert(unit p,a) =
	int oldmode

	oldmode:=getpclmode(p.convmode)

	case p.pclop
	when ksoftconv then
		evalunit(a)
		return
	when kerror then
		gerror("CONV/ERROR")
	else
		evalunit(a)
		genpc(p.pclop)
	esac
	setmode_u(p)

	pcl_setoldtype(oldmode)
end

global function checkdotchain(unit p, &pname)int=
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
		genpc(kaddrefx)
		pcl_setscale(1)
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

	if offset then
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kiloadx
			pccurr.extra+:=offset
			finish
		else
			genpushint(offset)
			genpc(kiloadx)
		fi
	else
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kiloadx
			finish
		else
			genpc(kiload)
		fi
	fi
	pcl_setscale(1)
finish:
	setmode(getmemmode_m(pdot))
end

proc do_storedot(unit pdot,pfield, rhs) =
	int offset
	unit a,pname

	evalunit(rhs)
	if pdot.resultflag then
		genpc(kdupl)
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

	if offset then
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kistorex
			pccurr.extra+:=offset
			finish
		else
			genpushint(offset)
			genpc(kistorex)
		fi
	else
		if pccurr.opcode=kaddrefx then
			pccurr.opcode:=kistorex
			finish
		else
			genpc(kistore)
		fi
	fi
	pcl_setscale(1)
finish:
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
	genpc(kiloadx)

	pcl_setscale(scale)
	pcl_setoffset(offset)
finish:
	setmode(getmemmode_m(p))
end

proc do_storeindex(unit p,parray,pindex,rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(parray,pindex)

	evalunit(rhs)
	if p.resultflag then
		genpc(kdupl)
	fi

	evalarray(parray)
	evalunit(pindex)

	genpc(kistorex)
	setmode_u(p.a)

	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

proc do_indexref(unit parray,pindex) =
	int addoffset,scale
!cpl "DOINDEXREF"
	addoffset:=getindexoffset(parray,pindex)

	evalarray(parray)
	evalunit(pindex)

	genpc(kaddrefx)

	setmode(tttarget[parray.mode])
	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

function getindexoffset(unit parray, &pindex)int offset=
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
	[0..maxlabels]ref pclrec labels
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

	if ismult then genpc(kstartmx) fi

	evalunit(pindex)
	genpc_xy((isfast|kswitchu|kswitch), minlab, maxlab,genpc_label(labjump))
	genpc(kopnd,genpc_label(elselab))

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		genpc(kswitchlabel,genpc_label(elselab))
		labels[i]:=pccurr
	od
	genpc(kendswitch)

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
		if ismult then genpc(kresetmx) fi
		if isfast then
			evalunit(pindex)
			genpc_xy(kswitchu, minlab, maxlab,genpc_label(labjump))
			genpc(kopnd,genpc_label(elselab))
		else
			genjumpl((loopsw|lab_a|lab_d))
		fi
		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse,isref)
		if ismult then genpc(kendmx) fi
	fi

	if loopsw then
		if isfast then
			evalunit(pindex)
			genpc_xy(kswitchu, minlab, maxlab,genpc_label(labjump))
			genpc(kopnd,genpc_label(elselab))
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
	[maxlabels]ref pclrec labels
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

	if ismult then genpc(kstartmx) fi
	evalunit(a)

	genpc_xy(kswitch, 1, n, genpc_label(labjump))
	genpc(kopnd, genpc_label(elselab))


	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		genpc(kswitchlabel,genpc_label(elselab))
		labels[i]:=pccurr
	od
	genpc(kendswitch)

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q,isref)
		if ismult then genpc(kresetmx) fi
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c,isref)
	if ismult then genpc(kendmx) fi

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

	if ismult then genpc(kstartmx) fi

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
!			if w.nextunit or wt.nextunit then genpc(kdouble) fi
			evalunit(w)
			genpc(kjumpeq, genpc_label(w.whenlabel:=labtable[ncases]))
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
		if ismult then genpc(kresetmx) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse,isref)
		if ismult then genpc(kendmx) fi
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

	genpc(kloadbit)
	setmode(ti64)
end

proc do_dotslice(unit p,a,b) =
	evalunit(a)
	evalunit(b.a)
	evalunit(b.b)

	genpc(kloadbf)
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
	case p.fnindex
	when sf_getnprocs then
		genpc(kgetnprocs)
	when sf_getprocname then
		evalunit(a)
		genpc(kgetprocname)
	when sf_getprocaddr then
		evalunit(a)
		genpc(kgetprocaddr)

	else
		GENPC_COMMENT("SYSCALL/GENERIC")
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
			genpc(ksub)
			setmode(ti64)
			genpushint(1)
			genpc(kadd)
		fi
		setmode(ti64)

	fi

	genpc((res|kstoresliced|kstoreslice), genmem_d(d))
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
	when jcallfn then
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
			genpc(kunload)
			setmode(ttmult[d.mode,i])
		od
	fi
end

proc do_assignmm(unit a,b)=
!(a,b,c):=(x,y,z)
	pushrhs(b.a)			!push rhs elements in right-to-left order
	genpc(kloadall)
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
			genpc(kstore,genmem_u(a))
		when jindex, jslice,jdot then
			evalref(a)
			genpc(kistore,genpc_int(0))
		when jptr then
			evalunit(a.a)
			genpc(kistore,genpc_int(0))
		when jif, jcase, jswitch, jselect then
			evalref(a)
			genpc(kistore,genpc_int(0))
		when jdotindex then
			evalref(a.a)
			evalunit(a.b)
			genpc(kstorebit)
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
	genpc(kclear)
	setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value)
end

function condtopclop(int cond, baseop)int=
!turn keq etc into kjumpeq etc
!baseop is kjumpeq, kseteq, kselecteq
	return baseop+(cond-keq)
end
=== mm_assem.m 0 0 6/38 ===
global function readassemline:unit=
	lex()
	return assembleline(1)
end

global function readassemblock:unit=
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

function assembleline(int oneline)unit=
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

function readassemopnd:unit p =
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

=== mm_decls.m 0 0 7/38 ===
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

	unit code			!var idata/proc body/taggedunion tag value/etc

	int32 mode
	byte namelen
	byte symbol
	byte nameid
	byte subcode

	union
		int32 index					!needs to hold pcindex (also indices for all symbols or .bc files)
		int32 labelno				!for mcl anonymous labels; and for proc labels?
	end
	int32 offset

	word32 pos: (sourceoffset:24, fileno:8)
	word16 flags: (
		isstatic:1,
		used:1,
		txdone:1,
		circflag:1,

		islet:1,
		addrof:1,
		noreg:1,
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
		byte xxx				!0 windowsff. etc.

		byte nretvalues			!function: number of return values (0 for proc)
		byte varparams			!0 or 1; variadic params in B and FF
		byte isthreaded			!0 or 1; variadic params in B and FF
		int16 dummy1
	end

	struct				!when a record or record field
		ref strec equivfield
		uflagsrec uflags
		int32 baseclass
		byte bitfieldwidth		!width of bitfield in record
		byte align				!0, 2, 4, 8, 16 or 255 (auto-align)
		byte bitoffset		!0..31 for bitfields in records
		byte equivoffset
	end

	struct				!when a param name
		ref strec nextparam
		byte parammode			!0=var_param, in_param, out_param
		byte optional			!0 or 1	
		byte variadic			!variadic parameter for B code
		byte dummy3				!variadic parameter for B code
	end

	int16 nrefs
	int16 regsize
	int16 maxalign		!for record types (doesn't fit above)

!----------------------------------

	ref fwdrec fwdrefs	!fwd ref chain
	byte reftype		!label pass 2: extern/back/fwd
	byte segment		!label pass 2: code_seg etc or 0

	int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
	int16 importindex	!genexe: index into import table

	int16 impindex
	int16 expindex
	byte reg

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
		int32 memmode	!name/ptr/index/dot: void=LVALUE; non-void=RVALUE
		int32 elemmode	!for jnew/newvar
	end

	byte moduleno
	byte subprogno
	byte initlet		!1 for an assignment that initialises a let
	byte isconst		!1 for jconst, and jmakerange with const range

	byte resultflag		!1 when the result of this unit is needed; 0=void or discarded
	byte pclop			!generic operator for jbin, incr etc
	byte istrueconst	!1 for actual "123" etc, not result of reduction
	byte dummy
end

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
export [0..maxtype]ichar		ttname

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
global ref riprec riplist

!global int alineno=0

global int debug=0
global int assemmode=0
global int headermode=0

global ref procrec proclist,proclistx			!linked list of all procs
global ref procrec staticlist,staticlistx		!linked list of all static
global ref procrec constlist,constlistx		!linked list of all export consts

global unit nullunit

global int targetbits=64
global int targetsize=8

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global byte msyslevel=2		!0/1/2 = none/min/normal
global byte mvarlib=0		!0/1 = none/yes
global byte fvarnames=0		!display of names in asm/mcl

!global byte freadma			!1 when .ma file is being compiled
global byte fwritema

global byte fshowtiming
global byte fshowss
!global byte fshowpcl
!global byte fshowasm
!global byte fshowast1
!global byte fshowast2
!global byte fshowast3
global byte fshowst
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

!global const langnameuc		= "M"
!global const langname		= "m"
!global const langext		= "m"
!global const langextma		= "ma"
!global const langextmauc	= "MA"
!global const langlibname	= "mlib"

global const langhomedir	= "C:/mx6/"
!global const langhomedir	= "./"

!global const langhomedir	= "C:/mxp/"
!global const langhomedir	= "./"
global const langhelpfile	= "mm_help.txt"

!GLOBAL INT NMCL
!GLOBAL INT NMCLOPND
!GLOBAL INT NALLCALLS
!GLOBAL INT NLOCALSHADOW
!GLOBAL INT NALLADDS, NSIMPLEADDS

!GLOBAL INT NALLPROCS
!GLOBAL INT NPROCPROCS
!GLOBAL INT NMAXPROCS
!GLOBAL SYMBOL STMAXMODULE
!
!GLOBAL INT MAXNAMELEN
!GLOBAL ICHAR MAXNAME

!GLOBAL INT NTHEN
!GLOBAL INT NEND
!GLOBAL INT NGLOBALS
GLOBAL INT NDO
GLOBAL INT NTO
GLOBAL INT NFOR
GLOBAL INT NWHILE
GLOBAL INT NREPEAT
GLOBAL INT NDOCASE
GLOBAL INT NDOSWITCH
GLOBAL INT NEXIT
GLOBAL INT NREDO
GLOBAL INT NNEXT
GLOBAL INT NPCL
GLOBAL INT NSYMBOLS
GLOBAL INT NDEFS
GLOBAL INT NMCL



=== mm_diags.m 0 0 8/38 ===
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

		print @dev,d.name,namenames[d.nameid],d

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
		when tr32 then
			x32:=p.xvalue
			print @dev,real64(x32)
		when tr64 then
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
			print @dev,pclnames[p.cmpgenop[i]],," "
		od
	esac

	case p.tag
	when jname, jptr, jindex, jdot,jcallproc, jcallfn, jassign then
		if p.memmode=tvoid then
!		print @dev," LVALUE"
		else
			print @dev," WIDEN FROM:",strmode(p.memmode)
		fi
	esac

	if p.isconst then
		print @dev," Is const"
	else
		print @dev," Not const"
	fi

	case p.tag
	when jbin, jbinto, junary, junaryto, jcmp, jincr, jconvert,
		jandl, jorl, jnotl, jistruel then
		if p.pclop then
			fprint @dev," Pcl<#>",pclnames[p.pclop]
		else
			fprint @dev," no-op"
		fi
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

function getprefix(int level,ichar prefix,ref unitrec p)ichar=
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

function getlineinfok:ichar=			!GETLINEINFO
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
		println @f,tab,"Cat:",catnames[ttcat[m]]
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

!IF PASSLEVEL<EXE_PASS THEN
CPL "PRESS KEY..."; OS_GETCH()
!FI

	if fshowmodules then showprojectinfo(logdev) fi

	if dopassfile[mcl_pass] then
		println @logdev,"PROC ASSEMBLY"
		addtolog(changeext(outfile, "asm"),logdev)
	fi
!CPL "PRESS KEY2..."; OS_GETCH()
	if dopassfile[pcl_pass] then
		println @logdev,"PROC PCL"
		addtolog(changeext(outfile, "pcl"),logdev)
	fi
	if dopassfile[type_pass] then addtolog("AST3", logdev) fi
	if dopassfile[name_pass] then addtolog("AST2", logdev) fi
	if dopassfile[parse_pass] then addtolog("AST1", logdev) fi

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

global proc strpcl(pcl p)=
	[256]char pmodestr
	[256]char str
	int opcode, defused
	ichar s

	const showformatted=1

	opcode:=p.opcode

!psstr(strint(getlineno(p.pos),"4"))
!psstr(" ")

!STATIC INT CC
!
!CPL PCLNAMES[OPCODE],++CC
	case opcode
	when klabel then
!CPL "PCL/LABEL", P.DEF.NAME,P.ISEXPORTED
		strlabel(p.labelno,1)
		return
	when kcomment then
		if p.svalue^ then
			psstr("!")
			psstr(p.svalue)
		fi
		return
	when kprocdef, kthreadedproc then
!		if p.def.isrts then
!			psstr("Procrts")
		if opcode=kthreadedproc then
			psstr("tcproc")
		else
			psstr("proc")
		fi
		psstr(" ")
		psname(p.def)
!		psstr((p.isexported|"::"|":"))
		psstr((p.def.scope=export_scope|"::"|":"))
!		if p.pcat then
!			psstr(" ")
!			psstr(strpmode(p.pcat, p.psize, p.psigned))
!		fi
		psline()

!		genlocals(p)
		return

!	when kendproc then
!		psstr("endproc")
!!		psline()
!		return

!	when klabelname then
!		psname(p.def)
!		psstr((p.def.scope=export_scope|"::"|":"))
!		return

!	when kendprogram then
!		psstr("endprog")
!		return

!	when kload then
!		case p.opndtype
!		when memaddr_opnd then opcode:=kloadref
!		when int_opnd, real_opnd, real32_opnd, string_opnd then opcode:=kloadimm
!		esac

!	when kdb, kdw, kdd, kdq then
!		opcode:=kdata
!		psstr(tab1)

	when kdata then
		psdata(p)
		return

	when kistatic, kzstatic then
		skiptab

	when kgetnprocs then
		psstr(tab1+"load i64 $nprocs")
		return

	when kgetprocname then
		psstrline(tab1+"loadref  u64 $procname")
doprocname:
		psstrline(tab1+"exchpcl")
		psstr(tab1+"iloadx   i64 8 -8")
		return

	when kgetprocaddr then
		psstrline(tab1+"loadref  u64 $procaddr")
		doprocname

!	when kgetprocaddr then
!		psstrline(tab1+"load i64 $nprocs")
!		return
!
	esac

	psstr(tab1)
skiptab:
	s:=pclnames[opcode]
	if s^='k' then ++s fi
	strcpy(str, s)
	if opcode in kjumpeq..kjumpgt and p.popone then
		strcat(str, "/1")
	fi


	gs_leftstr(dest,str,9)

	if p.pcat then
		if opcode in [kload, kiload, kiloadx] then
			if p.pcat=intcat and p.psize<8 then
				psstr("i64/")
			fi
		fi

		psstr(strpmode(p.pcat, p.psize, p.psigned))
		if pclhastype[opcode]=2 then
			psstr("/")
			psstr(strpmode(p.oldcat, p.oldsize, p.oldsigned))
		fi
!		psstr(" ")

!		psstr(catnames[p.pcat])
!		psstr(" ")
	else
		psstr("--")
	fi

	if p.opndtype<>no_opnd then
		psstr(" ")
		psstr(stropnd(p))
!	else
!		pstabto(
	fi
!	pstabto(30)

!	psstr("[")
!	psstr(strint(p.diff,"+"))
!	if p.wide then psstr(" W") fi
!	psstr("] ")


	if pclextra[opcode] and opcode not in kjumpeq..kjumpgt then
		psstr(" ")
		psint(p.x)
		if pclextra[opcode]=2 then
			psstr(" ")
			psint(p.y)
		fi
	fi

	if opcode=keval then psstr("\n") fi

!	if p.isglobal then psstr(" Isglobal") fi
!	if p.isvariadic then psstr(" Isvariadic") fi
end

global function stropnd(pcl p)ichar=
	static[512]char str
	int length
	symbol d

!RETURN "<OPND>"

	if p=nil then
		return ""
	fi

!STRCPY(STR,"?")

!CPL OPNDNAMES[P.OPNDTYPE]

	case p.opndtype
	when int_opnd then
		return strint(p.value)
	when real_opnd then
		if p.xvalue=infinity then
			fprint @str,"0x#",word@(p.xvalue):"h"
		else
			fprint @str,"# !(L#)",p.xvalue:"e16.16", p.r64index
		fi

	when real32_opnd then
!		print @str,p.xvalue32:"e16.16"
		fprint @str,"# !(L#)",p.xvalue32:"e16.16", p.r32index

	WHEN REALIMM_OPND THEN
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
!RETURN "<LONGSTR>"
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

!	when mem_opnd, memaddr_opnd then
	when mem_opnd then
		d:=p.def

		if d.nameid in [frameid, paramid] then
			strcpy(str, ".")
			strcat(str, d.name)
		else
			strcpy(str, getfullname(d))
		fi

!		print @str,(d.truename|d.truename|d.name)
!		print @str,(d.truename|"`"|""),,d.name,"[",,d.owner.name,,"]"
		if p.opcode in [kistatic, kzstatic] then
			strcat(str,":")
			if d.scope=export_scope then
				strcat(str,":")
			fi
		fi

	when memaddr_opnd then
		d:=p.def
		fprint @str,"&##",(d.truename|"`"|""),d.name
!		fprint @str,"&##[#]",(d.truename|"`"|""),d.name, (d.owner|d.owner.name|"-")

	when label_opnd then
		fprint @str,"## ","#",p.labelno

	when no_opnd then
		return ""

	when assem_opnd then
		return strint(int(p.asmcode))

	when data_opnd then
!		fprint @str,"<Data * #>", p.psize
		fprint @str,"<Data * # (#)>", p.psize,p.svalue

	else
!CPL "BAD OPND"
		println OPNDNAMES[P.OPNDTYPE]
		return "<PCLOPND?>"
	esac

	return str
end

global function strpclstr(pcl p)ichar=
	gs_free(dest)
	gs_init(dest)
	destlinestart:=0
	strpcl(p)
	gs_char(dest,0)
!CPL "//",DEST.LENGTH
	dest.strptr
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

global proc psname(symbol d)=
	gs_str(dest,getfullname(d))
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

global proc writepcl(pcl p)=

	CASE P.OPCODE
	WHEN KSETARG, KSETCALL THEN
		RETURN
	ESAC

	strpcl(p)
	case p.opcode
	when kprocdef then
	else
		gs_line(dest)
	esac
end

global function writeallpcl:ref strbuffer=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	symbol d,e

	gs_init(dest)
	destlinestart:=dest.length

	scansymbols(stprogram)

	p:=pcstart

	while p<=pccurr do
		writepcl(p)

		destlinestart:=dest.length
		++p
	od

!	if fshowsymbols then
!		writesymbols()
!	fi

	scanprocs()


	if longstring then
		pcm_free(longstring,longstringlen)
	fi
	return dest
!	return (dest.strptr,dest.length)
end

global function stropndstack(int indent=0)ichar=
	static [512]char str
	[512]char str2
	ichar s:=&.str, t
	pclstackrec pc

	if indent then
		fprint @s, "                                     ("
	else
		fprint @s, "("
	fi

	for i to noperands do
		pc:=pclstack[i]
		case pc.loc
		when reg_loc, regvar_loc then
			strcat(s, regnames[pc.reg])

!		when regvar_loc then
!			strcat(s, regnames[pc.reg])
!			strcat(s, "=")
!			strcat(s, pc.def.name)

		when xreg_loc then
!CPL "SHOWSTACK",=PC.REG
			strcat(s, xregnames[pc.reg])
!		when xregvar_loc then
!			strcat(s, xregnames[pc.reg])
!			strcat(s, "=")
!			strcat(s, pc.def.name)


		when temp_loc then
			strcat(s, "Tmp")
		when immd64_loc then
			strcat(s,strint(pclvals[i].value))

		when string_loc then
			strcat(s,"<")
!			strcat(s,pclvals[i].svalue)
			strcat(s,strint(pclvals[i].strindex))
			strcat(s,">")

		when memaddr_loc then
			strcat(s,"&")
			strcat(s,pclvals[i].def.name)
		when mem_loc then
			strcat(s,pclvals[i].def.name)

		else
			strcat(s, "??")
		esac

		if i<noperands then strcat(s,",") fi
	od

	strcat(s,") (")
	for r:=r0 to regmax do
		strcat(s,(regset[r]|"1 "|"0 "))
	od
	strcat(s,") ")

	strcat(s,"<")
	for i to noperands do
		pc:=pclstack[i]
		strcat(s,catnames[pc.cat])
		strcat(s," ")
	od
	strcat(s,">")

	strcat(s,"(")
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
!CPL "SOS/DIAGS"
	mgencomment(stropndstack(1))
!	CPL (stropndstack(1))
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
		minsym,maxsym,powersym,samesym then
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

proc scansymbols(symbol d)=
	symbol e

	case d.nameid
	when programid,moduleid then
	when dllprocid then
!CPL "SS DLLPROC",D.NAME
		if d.used then
			psstr("extproc ")
			psstr(strpmode_m(d.mode))
			psstr(" ")
			psstr(getfullname(d))
!			psstr(d.name)
			psline()

			e:=d.deflist
			while e, e:=e.nextdef do
				if e.nameid=paramid then
					psstr(tab1+"extparam ")
					psstr(strpmode_m(e.mode))
					psline()
				fi
			od
			if d.varparams then
				psstr(tab1+"extvarpm ")
				psint(d.varparams)
				psline()
			fi

			psstr("endext")
			psline()
			psline()

		fi
	else
		return
	esac

	e:=d.deflist

	while e, e:=e.nextdef do
		scansymbols(e)
	od
end

proc scanprocs=
	const maxprocs=1000
	[maxprocs]symbol proctable
	pcl currpcl
	int nprocs:=0

	currpcl:=pcstart

	repeat
		if currpcl.opcode in [kprocdef,kthreadedproc] and currpcl.def.ishandler then
			if nprocs>=maxprocs then gerror("PCL proctab overflow") fi
			proctable[++nprocs]:=currpcl.def
		fi
		++currpcl
	until currpcl.opcode=kendprogram

!	goto finish when nprocs=0

	psstr("\nistatic mem:")
	psint(nprocs*8)
	psstrline(" $procaddr:")
	
	for i to nprocs do
		psstr(tab1+"data u64 ")
!		psstrline(proctable[i])
		psstrline(getfullname(proctable[i]))
	od

	psstr("\nistatic mem:")
	psint(nprocs*8)
	psstrline(" $procname:")

	for i to nprocs do
		psstr(tab1+"data u64 """)
		psstr(proctable[i].name)
		psstrline("""")
	od

finish:
	psstrline("\nistatic u64 $nprocs:")
	psstr(tab1+"data i64 ")
	psint(nprocs)
	psline()
end

!proc genlocals(pcl p)=
!	symbol d:=p.def
!	symbol e:=d.deflist
!
!!PSSTR("GENLOCALS\N")
!!	int m:=p.mode
!
!	while e, e:=e.nextdef do
!
!		case e.nameid
!		when frameid then
!			if not e.atvar then
!				psstr(tab1+"local    ")
!dolocal:
!				psstr(strpmode_m(e.mode))
!				psstr(" .")
!				psstr(e.name)
!				psline()
!!				genpc(klocal,genmem(d))
!!				setmode(d.mode)
!			fi
!		when paramid then
!			psstr(tab1+"param    ")
!			dolocal
!		esac
!	od
!
!	if d.mode<>tvoid then
!		psstr(tab1+"rettype  ")
!		psstr(strpmode_m(d.mode))
!		psline()
!	fi
!end

proc psdata(pcl p)=
	const perline = 20
	int n:=p.psize, m
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
=== mm_export_dummy.m 0 0 9/38 ===
!hello

global proc writeexports(ichar basefile, modulename)=
end
=== mm_genpcl.m 0 0 10/38 ===
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

global proc codegen_pcl=
!generate code for module n
	symbol d
	ref procrec pp

	pcl_start(nunits)

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
end

proc genprocdef (symbol p) =
	imodule ms

	ms:=modules[p.moduleno]
	nblocktemps:=0

	if p=ms.stmain and moduletosub[p.moduleno]=mainsubprogno then
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
dozstatic:
		genpc(kzstatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
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
	mlineno:=p.pos
	tbase:=ttbasetype[t]

!CPL "GENIDATA", am:"c"
!PRINTUNIT(P)

	case p.tag
	when jconst then
		if ttisref[t] then
			if t=trefchar then
				if p.svalue then
					genpc(kdq,genpc_string(p.svalue))
				else
					genpc(kdq,genpc_int(0))
				fi
			else
				genpc(kdq,genpc_int(p.value))
			fi
			setmode(ti64)
		elsif ttisreal[t] then
			case ttsize[t]
			when 4 then
				genpc(kdd,genpc_real32(p.xvalue))
			when 8 then
				genpc(kdq,genpc_realimm(p.xvalue))
			else
				gerror_s("IDATA/REAL:",strmode(t),p)
			esac

		elsif ttbasetype[t]=tarray then
!CPL "GENDATA/CONST ARRAY"
GENPC_COMMENT("<CONST ARRAY>")
			IF P.STRTYPE=0 THEN GERROR("IDATA/ARRAY/NOT BLOCKDATA") FI
!			s:=p.svalue
!			to p.slength do
!				genpc(kdb, genpc_int(s++^))
!			od
			genpc(kdata, genpc_data(p.svalue, p.slength))

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
				gerror_s("IDATA/INT:",strmode(t),p)
			esac
			setmode(getmemmode_m(p))
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
			genpc(kdata, genpc_data(pcm_copyheapstringn(cast(&data), nbytes), nbytes))
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
!CPL "NAME",D.NAME,STRMODE(T)
			genpc((am='P' or ttsize[t]=8|kdq|kdd), genmemaddr_d(d))
			if offset then
				pcl_setscale(1)
				pcl_setoffset(offset)
			fi
			if am='P' then
				setmode(tu64)
			else
				setmode(t)
			fi
		when labelid then
			if d.index=0 then d.index:=++mlabelno fi
			genpc(kdq, genpc_label(d.index))
			setmode(ti64)
		else
			gerror("Idata &frameXXX")
		esac
		return
	when jconvert then
		genidata(p.a)
	when jshorten then
		a:=p.a
		case ttsize[t]
		when 1 then
			genpc(kdb,genpc_int(a.value))
		when 2 then
			genpc(kdw,genpc_int(a.value))
		when 4 then
			genpc(kdd,genpc_int(a.value))
		else
			gerror_s("IDATA/SHORTEN:",strmode(t),p)
		esac
		setmode(t)

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
	symbol d

	mlineno:=p.pos
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

	genpc(kload,genpc_int(0))
	genpc(kstop)
	genreturn()

	genpc(kendproc)
	genpc_comment("")
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

	mlineno:=p.pos
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

	genpc(kendproc)
	genpc_comment("")
end

proc initstaticvar(symbol d)=
	if d.code then
		evalunit(d.code)
	fi
!	if d.equals=3 then
!		genpc_comment("<deepcopy needed>")
!!*!				genpc(kcopy)
!	fi
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
=== mm_lex.m 0 0 11/38 ===
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
			lx.subcode:=knotin
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
	ref char p
	byte instr

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
		when cr then
			++lxsptr
			exit
		when lf then
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
			nextlx.subcode:=kincr
			return
		fi
		return

	when '-' then
		nextlx.symbol:=subsym
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kdecr
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
		when '=' then
			++lxsptr
			nextlx.symbol:=samesym
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=keq
		esac
		return

	when '<' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.subcode:=kle
		when '>' then
			++lxsptr
			nextlx.subcode:=kne
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
		else
			nextlx.subcode:=klt
		esac
		return

	when '>' then
		nextlx.symbol:=cmpsym
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=cmpsym
			nextlx.subcode:=kge
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
		else
			nextlx.symbol:=cmpsym
			nextlx.subcode:=kgt
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

++NSYMBOLS

	hashtable[j]:=d

	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbol:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbol
!	nextlx.subcode:=d.subcode
end

function lookupsys(ref char name)int=
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

global function addnamestr(ichar name)ref strec=
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

function setinttype(word64 a)int=
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

!proc OLDlxreadstring(int termchar)=
!!start from char just after " or ' (termchar will be " or ')
!
!	ichar s,t
!	int c, d, length, hasescape, a
!	[8]char str
!
!	if termchar='"' then
!		nextlx.symbol:=stringconstsym
!	else
!		nextlx.symbol:=charconstsym
!		nextlx.subcode:=tint
!	fi
!
!	s:=lxsptr
!
!!do a first pass that terminates length of final string
!	length:=0
!	hasescape:=0
!
!	docase c:=lxsptr++^
!	when '\\' then			!escape char
!		c:=lxsptr^
!		if c in 'A'..'Z' then c+:=' ' fi
!		++lxsptr
!		hasescape:=1
!
!		case c
!		when 'w' then
!			length+:=2
!		when 'x' then	!2-digit hex code follows
!			lxsptr+:=2
!			++length
!		when 'u' then	!4-digit hex code follows
!			lxsptr+:=4
!			length+:=4	!utf8 expanded version might be up to 4 chars
!			hasescape:=2
!		when 'v' then	!6-digit hex code follows
!			lxsptr+:=6
!			length+:=4
!			hasescape:=2
!		else				!assume valid escape (bad ones detected in next pass)
!			++length
!		esac
!
!	when '"','\'' then		!possible terminators
!		if c=termchar then		!terminator char
!			if lxsptr^=c then		!repeated, assume embedded term char
!				hasescape:=1
!				++lxsptr
!				++length
!			else			!was end of string
!				exit
!			fi
!		else
!			++length
!		fi
!	when cr,lf,0 then
!		lxerror("String not terminated")
!	else
!		++length
!	end docase
!
!	if length>u16.max then lxerror("String too long") fi
!	nextlx.slength:=length+1
!!	nextlx.slength:=length
!
!!CPL =LENGTH
!
!	if length=0 then
!		nextlx.svalue:=""
!		return
!	elsif not hasescape then
!		nextlx.svalue:=pcm_copyheapstringn(s,length)
!		return
!	fi
!
!!need to copy string to dest and expand the escape codes
!
!	nextlx.svalue:=t:=pcm_alloc(length+1)
!
!	do
!		case c:=s++^
!		when '\\' then			!escape char
!			c:=s^
!			if c>='A'  and c<='Z' then c+:=' ' fi
!			++s
!			case c
!			when 'a' then			!bell ('alert')
!				c:=7
!			when 'b' then			!backspace
!				c:=8
!			when 'c','r' then		!carriage return
!					c:=cr
!			when 'e' then			!escape
!				c:=27
!			when 'f' then			!formfeed
!				c:=12
!			when 'l','n' then		!linefeed, or linux/c-style newline
!				c:=lf
!			when 't' then			!tab
!				c:=9
!			when 'u','v' then		!reserved for unicode, like \x but with 4 hex digits
!				t +:= getutf8(readhexcode(&s, (c='u'|4|6)), t)
!				nextloop
!
!			when 'w' then			!windows-style cr-lf
!				t++^:=cr
!				c:=lf
!			when 'x' then	!2-digit hex code follows
!				c:=readhexcode(&s,2)
!			when 'y' then			!CCI/SM backwards tab
!				c:=16
!			when 'z' then			!null (not fully supported in code)
!				c:=0
!			elsecase c
!			when '"' then			!embedded double quote
!				c:='"'
!			when '\\' then
!				c:='\\'
!			when '\'' then			!embedded single quote
!				c:='\''
!			when '0' then
!				c:=0
!			else
!				str[1]:=c; str[2]:=0
!				lxerror_s("Unknown string escape: \\%s",&.str)
!			end
!		when '"','\'' then		!possible terminators
!			if c=termchar then		!terminator char
!				if s^=c then		!repeated, assume embedded term char
!					++s
!				else			!was end of string
!					exit
!				fi
!			fi
!		when cr,lf,0 then
!			lxerror("String not terminated")
!		esac
!
!		t++^:=c
!	od
!
!	if hasescape=2 then			!utf8 sequences may have overallocated; get true length
!		nextlx.slength:=strlen(nextlx.svalue)+1		!EMBEDDED ZEROS WILL SCREW IT UP
!	fi
!
!	t^:=0
!end

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

=== mm_lib.m 0 0 12/38 ===
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

global macro isnum(m) = (m in tfirstnum..tlastnum)
global macro isnumx(m) = (m in tfirstnum..tlastnum)
global macro isnumf(m) = (m in [tr64, tr32])
global macro isnumi(m) = (m in [ti64, tu64, tc64])
global macro isbool(m) = (m in [tbool8, tbool64])

global function newstrec:symbol=
	symbol p

!	p:=pcm_alloc(strec.bytes)
!	clear p^

	p:=pcm_allocnfz(strec.bytes)

	p.pos:=lx.pos
	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]
	return p
end

global function getduplnameptr(symbol owner,symptr,int id)symbol=
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

++NDEFS

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

global function createname(symbol p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=jname
	u.def:=p
!++NUNITS1

	return u
end

global function createunit0(int tag)ref unitrec=
	ref unitrec u

!++NUNITS1

	u:=allocunitrec()
	u.tag:=tag
	return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u
!++NUNITS1
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

global function createconstunit(word64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.value:=a
	u.mode:=t

++NUNITS1

	u.isconst:=1
	return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
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

global function newtypename(symbol a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	fi
	++ntypenames
	typenames[ntypenames].defa:=a		!leave .owner/.pmode to be filled in
	typenames[ntypenames].defb:=b		!used type's mode is used

	typenamepos[ntypenames].pos:=lx.pos

	return -ntypenames
end

global function createusertype(symbol stname)int=
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

global function createusertypefromstr(ichar name)int=
!create new, named user type
	symbol stname

	stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
	return createusertype(stname)
end

global function getrangelwbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.a
	else
		p:=createunit1(junary,p)
		p.pclop:=klwb
		return p
	fi
end

global function getrangeupbunit(ref unitrec p)ref unitrec=
	if p.tag=jmakerange then
		return p.b
	else
		p:=createunit1(junary,p)
		p.pclop:=kupb
		return p
	fi
end

global function createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=
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

	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

function sameunit(unit p,q, symbol powner=nil, qowner=nil)int=
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

global function createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=
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
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function nextautotype:ichar=
	static [32]char str

	print @&.str,"$T",,++autotypeno
	return &.str
end

global function createslicemode(symbol owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
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
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
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
!	ttcat[m]:=stdcat[slicetype]
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function createrefmode(symbol owner,int target,typedefx=0)int=
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
	ttcat[m]:=intcat

	return m
end

global function createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=
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
	ttcat[m]:=intcat

	return m
end

global proc copyttvalues(int dest, source)=
	ttsigned[dest]		:= ttsigned[source]
	ttisreal[dest]		:= ttisreal[source]
	ttisinteger[dest]	:= ttisinteger[source]
	ttisshort[dest]		:= ttisshort[source]
	ttisref[dest]		:= ttisref[source]
	ttcat[dest]			:= ttcat[source]
	ttisblock[dest]		:= ttisblock[source]
end

global function getdottedname(symbol p)ichar=
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

global function getavname(symbol owner,int id=frameid)symbol=
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

global function unionstr_last(ref uflagsrec u)int=
	if u.ulength then
		return u.codes[u.ulength]
	fi
	return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
	memcpy(u,v,uflagsrec.bytes)
end

global function createrecordmode(symbol owner,int typedefx)int=
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
	ttcat[m]:=blockcat
	ttisblock[m]:=1

	return m
end

global function createtuplemode(symbol owner,[]int &elements,int elementslen, typedefx)int=
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

global function convertstring(ichar s, t)int=
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

global function strexpr(ref unitrec p)ref strbuffer=
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

	when jcallfn,jcallproc then
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

	when jcvlineno, jcvfilename, jcvmodulename then
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

global function strmode(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,&.str)
	return &.str
end

global function strmode2(int m,expand=1)ichar=
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

global function typename(int m)ichar=
	if m>=0 then
		return ttname[m]
	fi
	return typenames[-m].def.name

end

global function allocunitrec:ref unitrec=
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

global function createdupldef(symbol owner,symptr, int id)symbol=
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

global function createnewmoduledef(symbol owner,symptr, int id=moduleid)symbol=
	return createdupldef(owner,symptr,id)
end

global function duplunit(unit p,int lineno=0)unit=
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

global function checkblockreturn(unit p)int=
!p is any statement
!check p, or the last statement of a block, or the last statement of any
!nested block, a return, or is a unit yielding some value (as explicit return
!statement not needed)
! return 1/0 for return/not return
	unit e,wt
	int m

	if p=nil then return 0 fi

	m:=p.mode

	case p.tag
	when jreturn, jstop, jassem then
		return 1

	when jif then
		e:=p.b
		while e, e:=e.nextunit do
			if not checkblockreturn(e) then return 0 fi
		od

		return checkblockreturn(p.c)		!all branches must have a return

	when jblock then
		e:=p.a
		if e then
			while e and e.nextunit do
				e:=e.nextunit
			od
			return checkblockreturn(e)
		fi

	when jcase, jswitch, jdocase, jdoswitch then
		wt:=p.b
		while wt do
			if not checkblockreturn(wt.b) then
				return 0
			fi

			wt:=wt.nextunit
		od

		return checkblockreturn(p.c)		!else

	esac

	if jisexpr[p.tag] and m<>tvoid then
		return 1							!any non-void expr allowed as return value
	else
		return 0
	fi
end

global function isconstunit(unit a)int=
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

global function getalignment(int m)int=
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

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer
end

global function storemode(symbol owner, int m, int32 &pmode)int =
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

global function gettypebase(int m)int=
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

global function getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
	unit a

	if p.tag<>jcallfn then txerror("multass/need multfn") fi
!	if p.tag not in [jcallfn,jcallproc] then txerror("multass/need multfn") fi
	a:=p.a

	case a.tag
	when jname then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	esac
end

global function getmemmode(unit p)int =
	if p.memmode then
		return p.memmode
	fi
	return p.mode
end

global function getpclmode(int t)int u=
	u:=ttbasetype[t]

	case u
	when tc64 then u:=tu64
	when tc8 then u:=tu8
	when trecord, tarray then
		if not ttisblock[t] then
			case ttsize[t]
			when 8 then u:=tu64
			when 4 then u:=tu32
			when 2 then u:=tu16
			else u:=tu8
			esac
		else
			u:=tblock
		fi
	esac
	return u
end

global function getfullname(symbol d)ichar=
!create fully qualified name into caller's dest buffer
	static [128]char str
	[16]symbol chain
	int n:=0
	symbol e:=d

	if d.isimport then
		return (d.truename|d.truename|d.name)
	fi

	repeat
		chain[++n]:=e
		e:=e.owner
	until e=nil or e.nameid=programid

	strcpy(str,chain[n].name)
	for i:=n-1 downto 1 do
		strcat(str,".")
		if chain[i].truename then
			strcat(str,chain[i].truename)
		else
			strcat(str,chain[i].name)
		fi
	od

	return str
end

global function getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if cstringlist and eqstring(cstringlist.svalue,s) then
		return cstringlist.labelno
	fi

	return addconst(cstringlist, cast(s))
end

global function addconst(ref constrec &clist, int value)int=
	ref constrec p
	p:=pcm_allocnfz(constrec.bytes)
	p.value:=value
!CPL "ADDCONST",MLABELNO+1
	p.labelno:=++mlabelno
	p.nextconst:=clist
	clist:=p
	return mlabelno
end

global function getrealindex(real x)int=
	return addconst(creallist,cast@(x,int))
end

global function getreal32index(real x)int=
	return addconst(creal32list,cast@(x,int))
end

=== mm_libpcl.m 0 0 13/38 ===
global pcl pcstart			!start of pcl block
global pcl pccurr			!point to current pcl op
global pcl pcend			!point to last allocated pclrec
global int pcalloc			!number of pclrecs allocated
byte pcfixed				!whether code is fixed up
int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

int initpcalloc=65536

const pcelemsize = pclrec.bytes

global ichar longstring					!used in stropnd
global int longstringlen
global ichar errormess

global int mcldone

global proc pcl_start(int nunits=0)=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

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
	mcldone:=0
end

global proc pcl_end=
	if pccurr>=pccurr and pccurr.opcode<>kendprogram then
		genpc(kendprogram)
	fi	
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

global function newpcl:pcl =
	if pccurr>=pcend then
		extendpclblock()
	fi

	++pccurr

	pccurr.pos:=mlineno
++NPCL

	return pccurr
end

global proc genpc(int opcode, pcl p=nil) =
	static int seq

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	P.SEQ:=++SEQ


end

global proc genpc_x(int opcode, int x, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
end

global proc genpc_xy(int opcode, int x,y, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
	p.y:=y
end

global function genpc_int(int a)pcl p=
	p:=newpcl()
	p.value:=a
	p.opndtype:=int_opnd
	return p
end

global function genpc_real(real x)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=real_opnd
!GERROR("GRI")
	p.r64index:=getrealindex(x)
	return p
end

global function genpc_realimm(real x)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=realimm_opnd
	return p
end

global function genpc_real32(real x)pcl p=
	p:=newpcl()
	p.xvalue32:=x
	p.opndtype:=real32_opnd
!GERROR("GRI")
	p.r32index:=getreal32index(x)
	return p
end

global function genpc_string(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=string_opnd
!GERROR("GRI")
	p.strindex:=getstringindex(s)

	return p
end

global function genpc_strimm(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=strimm_opnd

	return p
end

global function genpc_label(int a)pcl p=
	p:=newpcl()
	p.labelno:=a
	p.opndtype:=label_opnd
	return p
end

global function genpc_mem(symbol d)pcl p=
	p:=newpcl()
	if d.atvar and d.equivvar then
		d:=d.equivvar.def
	fi
	p.def:=d

	p.opndtype:=mem_opnd
	return p
end

global function genpc_memaddr(symbol d)pcl p=
	p:=newpcl()
	if d.atvar and d.equivvar then
		d:=d.equivvar.def
	fi
	p.def:=d

	p.opndtype:=memaddr_opnd
	return p
end

global function genpc_data(ref byte s, int length)pcl p=
	p:=newpcl()
	p.svalue:=s			! assume already saved on heap
	p.opndtype:=data_opnd
	p.pcat:=blockcat
	p.psize:=length

	return p
end

global proc genpc_comment(ichar s)=
	genpc(kcomment,genpc_strimm(s))
end

global function genpc_name(ichar s)pcl=
	return genpc_mem(pcl_makesymbol(s))
end

global function genpc_nameaddr(ichar s)pcl=
	return genpc_memaddr(pcl_makesymbol(s))
end

global function genpc_assem(ref void code)pcl p=
	p:=newpcl()
	p.asmcode:=code
	p.opndtype:=assem_opnd
	return p
end

global function pcl_makesymbol(ichar s)symbol d =
	d:=addnamestr(s)
	return d
end

global func strpmode(int cat, size, signedx)ichar=
	static [32]char str
!	int dprec@signed
	int dprec:=signedx

	strcpy(str, "")

	case cat
	when intcat then
		strcat(str, (signedx|"i"|"u"))
		strcat(str, strint(size*8))
	when realcat then
!		strcat(str, (dprec|"r64"|"r32"))
		strcat(str, (dprec|"r64"|"r32"))
	when blockcat then
		strcat(str, "mem:")
		strcat(str, strint(size))
	else
		strcat(str, "-- ")
	esac

	&.str
end

global func strpmode_m(int m)ichar =
	int cat:=ttcat[m]

	if cat=realcat then
		strpmode(realcat, ttsize[m], m=tr64)
	else
		strpmode(cat, ttsize[m], ttsigned[m])
	fi
end

global proc pcl_settype(int t,size=0)=
!	pccurr.pmode:=t
	pccurr.psize:=size
	pccurr.pcat:=ttcat[t]
	if pccurr.pcat=realcat then
		pccurr.pwide:=t=tr64
	else
		pccurr.psigned:=ttsigned[t]
	fi
end

global proc pcl_setxy(int x,y)=
	pccurr.x:=x
	pccurr.y:=y
end

global proc pcl_setscale(int scale)=
	pccurr.scale:=scale
end

global proc pcl_setoffset(int offset)=
	pccurr.extra:=offset
end

global proc pcl_addoffset(int offset)=
	pccurr.extra+:=offset
end

global proc pcl_setincr(int n)=
	pccurr.stepx:=n
end

global proc pcl_setnargs(int n)=
	pccurr.nargs:=n
end

global proc pcl_setnvariadics(int n)=
	pccurr.nvariadics:=n
end

global proc pcl_setalign(int n)=
	pccurr.align:=n
end

global proc pcl_setoldtype(int t)=
!	pccurr.oldcat:=stdcat[t]
	pccurr.oldcat:=ttcat[t]
	pccurr.oldsize:=ttsize[t]
	if pccurr.oldcat=realcat then
		pccurr.oldwide:=t=tr64
	else
		pccurr.oldsigned:=ttsigned[t]
	fi
end

global function pcl_writepclfile(ichar filename)int=
	ref strbuffer d

	d:=writeallpcl()

	if fverbose=3 then
		println "Writing PCL",filename
	fi
	return writefile(filename,d.strptr,d.length)
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

global function getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end
=== mm_libsources.m 0 0 14/38 ===
global const fsyslibs = 1

global tabledata []ichar syslibnames, []ichar syslibtext =
	("msyslib.m",		strinclude "msyslib.m"),
	("msys.m",			strinclude "msys.m"),
	("mlib.m",			strinclude "mlib.m"),
	("mclib.m",			strinclude "mclib.m"),
	("mwindows.m",		strinclude "mwindows.m"),
	("mwindll.m",		strinclude "mwindll.m"),
end

global proc loadbuiltins=
!load built-in libs to sources[] list
	ifile pf
	ichar filename

	for i to syslibnames.len do
		filename:=syslibnames[i]
!CPL "PRELOADING SYSLIB",FILENAME
		pf:=newsourcefile()

		sources[nsourcefiles].name:=pcm_copyheapstring(extractbasefile(filename))
		sources[nsourcefiles].filename:=filename
		sources[nsourcefiles].text:=syslibtext[i]
!		sources[nsourcefiles].text:=pcm_copyheapstring(syslibtext[i])

!These files should not be part of .ma file, however check what happens
!if syslib file is part of strinclude: it may discover this one, and it
!needs to be duplicated! Unless syslib/support must match
!		if fwritema then
!			sources[nsourcefiles].dupl:=pcm_copyheapstring(syslibtext[i])
!		fi

!if i=4 then CPL SYSLIBTEXT[I] FI

		sources[nsourcefiles].size:=strlen(syslibtext[i])
!		sources[nsourcefiles].path:="<Builtins>"
		sources[nsourcefiles].path:=""
		sources[nsourcefiles].filespec:=filename
		sources[nsourcefiles].issyslib:=1
		sources[nsourcefiles].issupport:=0
!CPL "LOADED BI",=SOURCES[NSOURCEFILES].FILENAME
!CPL "LOADED BI",=SOURCES[NSOURCEFILES].FILESPEC
	od

!FOR I TO 5 DO
!	CPL I,=SOURCES[I].SIZE
!OD

end
=== mm_modules.m 0 0 15/38 ===
ichar fileext="m"

global func loadsp(ichar filename, int mainsub=0)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=250
	const maxsubs=250
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

!CPL "LSF",FILENAME

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
!CPL "LSF2",FILENAME
	if not s then				!unexpected error
		return nil
	fi
	pf.text:=s
	pf.size:=rfsize

	if fwritema then
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
		syslibname:="msystemp"
	else
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

function readfileline(ichar s)ichar =
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

function findnextlineheader(ichar s)ichar=
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


=== mm_name.m 0 0 16/38 ===
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
!				if ttisblock[d.mode] and d.parammode<>out_param then
!!CPL "ISBLOCK",D.NAME, STRMODE(D.MODE),TTSIZE[D.MODE]
!					d.parammode:=out_param
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
	mlineno:=p.pos

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
!		if d.nameid=paramid and d.parammode=out_param then
!!CPL "RX:AUTODEREF",D.NAME; PRINTUNIT(P)
!			insertunit(p, jptr)
!!CPL "RX:AUTODEREF2",D.NAME; PRINTUNIT(P)
!		fi

	when jkeyword then
		rx_unit(owner,b)		!do param value only

	when jdot then
		resolvedot(owner,p)

	when jcallproc, jcallfn then
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

				if useparams and p.tag not in [jcallproc, jcallfn] then
					insertunit(p,oldtag)
					p.b:=b					!note b may be nil
				FI

			else
				if d.mode=tvoid then
					p.tag:=jcallproc
				fi
			esac
		fi

	when jandl, jorl then
		rx_unit(owner,a)
		rx_unit(owner,b)
		if not isbooltag[a.tag] then insertunit(a,jistruel); a.pclop:=kistruel fi
		if not isbooltag[b.tag] then insertunit(b,jistruel); b.pclop:=kistruel fi

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
		if a.tag=jnotl then
			deleteunit(p,a)
			p.tag:=jistruel
			p.pclop:=kistruel
			a:=p.a
			goto doistruel
		fi
		if not isbooltag[a.tag] then
!CPL "NOTL3"; PRINTUNIT(P)
			insertunit(a,jistruel); a.pclop:=kistruel
!CPL "NOTL4"; PRINTUNIT(P)
			a:=p.a
		fi
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

global function rx_module(int n)int=
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

global function resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol =
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

global function finddupl(symbol d, pdupl)symbol=
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

global function finddupl_sub(symbol d, pdupl)symbol=
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
!
!CPL "HERE"
!!CPL =MODULES[D.MODULENO].NAME
!!CPL =SUBPROGS[D.SUBPROGNO].NAME
!!CPL =INT(SUBPROGS[D.SUBPROGNO].NAME)
!FI
!!
!

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
				mlineno:=typenamepos[i].pos
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

function addframevar(symbol owner, d, int moduleno, mode)symbol=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
	symbol e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,e.mode)
	adddef(owner,e)
	return e
end

function copylistunit(unit p)unit=
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

function copyunit(unit p)unit=
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
=== mm_parse.m 0 0 17/38 ===
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

global function parsemodule(imodule pm)int=
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
!	repeat
!		lex()
!		++ntokens
!!PS("TOKEN")
!	until lx.symbol=eofsym
!
!!	repeat
!!		lexreadtoken()
!!		++ntokens
!!	until nextlx.symbol=eofsym
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

		when stdtypesym,krefsym,kicharsym,lsqsym,
			kdictsym,kslicesym then
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

		when kclasssym,krecordsym then
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

global function makeblock(unit p)unit=
	if p and p.tag=jblock then return p fi
	return createunit1(jblock,p)
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

function getcurrline:int=
	return lx.pos
end

function checkbegin(int fbrack)int=
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

!!now, should be semi, or possibly kwd1/2
	if lx.symbol in [endkwd1, endkwd2] then
		lex()
!	elsif lx.symbol<>semisym then
!		error
	fi
end

function readvardef(symbol owner,int scope=0,isstatic=0,varid=staticid, k)unit=
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

function readlbrack:unit=
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

function readcast:unit=
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

function readopc:unit=
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

function readcompilervar:unit=
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
		p:=createunit0(jcvlineno)
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

function readcastx:unit=
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

global function readtypespec(symbol owner,int typedefx=0)int=
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
		when kprocsym,kfunctionsym then	!function pointer being created
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

function readslicetype(symbol owner, int slicetype, typedefx)int=
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

function readslist(int iscall=0,donulls)unit=
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

function readindex(unit p,int dot)unit=
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
			plower.pclop:=klwb
			pupper:=createunit1(junary,duplunit(p))
			pupper.pclop:=kupb
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

function readdotsuffix(unit p)unit=
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
			if lx.subcode=kbounds then
!				q:=createunit1(junary, p); q.pclop:=klwb
!				r:=createunit1(junary, p); r.pclop:=kupb
				q:=createunit1(junary, duplunit(p))
				r:=createunit1(junary, duplunit(p))
				if p.tag=jtypeconst then
					q.pclop:=kminvalue
					r.pclop:=kmaxvalue
				else
					q.pclop:=klwb
					r.pclop:=kupb
				fi
!				q:=createunit1(junary, p); q.pclop:=klwb
!				r:=createunit1(junary, p); r.pclop:=kupb

				p2:=createunit2(jmakerange, q, r)
				deleteunit(p,p2)
			else

	doprop:
				p:=createunit1(junary,p)
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
			lx.subcode:=kmaxvalue
			goto doprop

		when minsym then
			lx.subcode:=kminvalue
			goto doprop
		when stdtypesym then
			if p.tag=jtypeconst and lx.subcode=trange then
				q:=createunit2(jmakerange,
					createunit1(junary,p),
					createunit1(junary,p))
				q.a.pclop:=kminvalue
				q.b.pclop:=kmaxvalue
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

function readconstexpr(int needconst=1)unit=
	return readunit()
end

function readconstint:int=
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

global function readprocdecl(symbol procowner,int scope)symbol=
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
!					serror("Wrong 'main/start' sig")
				fi

			fi
		fi
	fi

	popproc()

	return stproc
end

function readparams(symbol procowner,owner,int &varparams,&nparams)symbol=			!READPARAMS
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
	parammode:=var_param
	types:=0

	if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
		types:=1
	fi

	do										!expect type of name at start of loop
		parammode:=var_param
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
						varparams:=1
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
		when insym then
			parammode:=in_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when addrsym then
			parammode:=out_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when questionsym then
			isoptional:=1
			lex()
		when ellipsissym then
			varparams:=1
			lex()
			return stlist
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner,lx.symptr,paramid)
		adddef(owner,stname)
!STNAME.ISLET:=1
		lex()

		if parammode=out_param then
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

function readcondsuffix(unit p)unit=
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
		q.pclop:=knotl
		return createunit2(jif, q,createunit1(jblock,p))
	else
		return p
	esac
end

function readif:unit=
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

function readgoto(int gototag=jgoto)unit=
	lex()

	return readcondsuffix(createunit1(gototag,readunit()))
end

function readunless:unit=
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
	q.pclop:=knotl
	p.pos:=pos
	return p
end

function readswitchcase:unit=
	int pos1, kwd, opc, pos2,rangeused, nwhen
	unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen

	pos1:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc

IF KWD=KDOSWITCHSYM THEN ++NDOSWITCH FI
IF KWD=KDOCASESYM THEN ++NDOCASE FI

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

function readstop:unit=
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

function readreturn:unit=
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

function readdo:unit=
	unit p
	int pos
++NDO
	pos:=lx.pos
	lex()
	p:=readsunit()
	checkend(kendsym,kdosym)
	p:=createunit1(jdo,p)
	p.pos:=pos
	return p
end

function readto:unit=
	int pos,id
	unit p, pcount, pbody

++NTO
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

function readwhile:unit=
	int pos
	unit pcond, pbody, pincr, p
	pos:=lx.pos
	lex()
++NWHILE

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

function readrepeat:unit=
	int pos
	unit pbody, pcond, p
++NREPEAT


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

function readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode

IF OPC=JEXIT THEN ++NEXIT FI
IF OPC=JREDO THEN ++NREDO FI
IF OPC=JNEXT THEN ++NNEXT FI

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

function readprint:unit=
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

function readread:unit=
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

function readfor:unit=
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
++NFOR
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

function readname:unit p=
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
!classkwd=kclasssym or krecordsym
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
	when kclasssym,krecordsym then
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

		when kclasssym,krecordsym then
			readclassdef(owner,subprog_scope)

		when kvarsym then
			lex()
			readvardef(owner,subprog_scope,0,dllvarid, kvarsym)

		when stdtypesym,namesym,krefsym,kicharsym,lsqsym,
			kdictsym,kslicesym then
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

function readequivfield(symbol owner)symbol=
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

function readrefproc(symbol owner,int typedefx)int=
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

!function makeastring:unit =
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

function readreturntype(symbol owner, []int &retmodes)int=
!read 1..maxtuplesize return types as part of function decl
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

function readset:unit=
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

function istypestarter:int=
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

global function readunit:unit p=
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
		p:=createunit2(jcallfn, readassignment(), p)
	od

	return p
end

function readassignment(unit pt=nil)unit p=
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

function readorterms(unit pt=nil)unit p=
	int pos

	p:=readandterms(pt)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto,p,readassignment())
			p.pclop:=korlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(jorl,p,readandterms())
		p.pclop:=korl
		p.pos:=pos
	od

	return p
end

function readandterms(unit pt=nil)unit p=
	int pos

	p:=readcmpterms(pt)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto,p,readassignment())
			p.pclop:=kandlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(jandl,p,readcmpterms())
		p.pclop:=kandl
		p.pos:=pos
	od

	return p
end

function readcmpterms(unit pt=nil)unit p=
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
		p.pclop:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	fi

	return p
end

function readinterms(unit pt=nil)unit p=
	int pos,opc
	p:=readrangeterm(pt)

	docase lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(jbin,p,readrangeterm())
		p.pclop:=opc
		p.pos:=pos
	else
		exit
	end docase

	return p
end

function readrangeterm(unit pt=nil)unit p=
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

function readaddterms(unit pt=nil)unit p=
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

function readmulterms(unit pt=nil)unit p=
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

function readpowerterms(unit p=nil)unit=
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

function readterm2:unit=
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
			p:=createunit2(jcallfn,p,q)
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
		when kincr then opc:=kloadincr
		when kdecr then opc:=kloaddecr
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

function readterm:unit=
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
			p.pclop:=knotl
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
			p.pclop:=kistruel
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
		if p.a.tag=jcallfn then
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
			p.pclop:=kupb
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

	when kswapsym then			!swap using function syntax
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

function readrecase:unit=
	lex()
	if lx.symbol=kelsesym then
		lex()
		return createunit0(jrecase)
	else
		return createunit1(jrecase,readunit())
	fi
end

function fixcond(unit p)unit=
	if not isbooltag[p.tag] then
		insertunit(p, jistruel)
		p.pclop:=kistruel
	fi
	return p
end

function readsunit(int inwhile=0)unit=
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

		when stdtypesym,krefsym,kicharsym,kdictsym,kslicesym,lsqsym then
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

		when kclasssym,krecordsym then
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

		else							!assume a statement
	doexec:
			p:=readunit()
	doexec2:
			if p.tag=jname and lx.symbol=namesym then
				serror("Possibly var/let needed")
			fi
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
CPL "READBXDATA"

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

=== mm_pcl.m 0 0 18/38 ===
!type system

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
	(data_opnd,			$),
end

global type pcl = ref pclrec

global record pclrec =
	byte opndtype
	byte opcode
	byte pcat
	byte SPAREpmode				!t in tables

	int32 psize

	union
		struct
			union
				int64 value
				real64 xvalue
				real32 xvalue32
				ichar svalue			!also used for data
				int labelno
				symbol def
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
					int32 nvariadics	! 0, or arg # that is first variadic
				end
				struct					! (x,y) switch
					int32 minlab
					int32 maxlab
				end

				struct					!for fix/float, source type
					byte oldcat			!for trunc, it is truncation type
					byte oldsize
					union
						byte oldsigned
						byte oldwide
					end
			end
!
			int32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
			int32 align
			int32 nret				! (x) setretmult: no. of return values
			int32 popone			! (x) jumpcc: leave X on stack
			int32 strindex			! (x) any string operand: index into string table
			int32 r64index			! (x) any real operand: index into real table
			int32 r32index			! (x) any real32 operand: index into real32 table
			int32 slicelwb			! (x) for .upb

			end
		end
	end
	u32 pos:(sourceoffset:24, fileno:8)

	union
		byte pwide							! r64 rather than r32 for pcat=real
		byte psigned						! whether pcat=intcat is signed
	end
	byte spare
	u16 SEQ
end


!Stack operands depend on A in (A B); the last (X/Y/Z) is pushed last and
!is top of stack:
!
! A = 1       X
! A = 2       X Y
! A = 3       X Y Z
! A = 4       X Y Z W
!
!It can be assumed that all are popped
!New stack contents are indicated by X' and sometimes X' Y' which are pushed

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
!                           T X     ! (A B)
	(kzero=0,			$,	0,0),	! (0 0)
	(knop,				$,	0,0),	! (0 0)
	(kstop,				$,	0,0),	! (1 0)	Stop X
	(kcomment,			$,	0,0),	! (0 0)	Comment A (a string)

	(kistatic,			$,  1,0),	! (0 0) (A,t) Define idata label (must be followed by correct db etc ops)
	(kzstatic,			$,	1,0),	! (0 0) (A,t) Define zdata labe and reserve sufficient space

	(kprocdef,			$,	1,0),	! (0 0) (A,t) Define proc A, of given return type
!	(kprocentry,		$,	0,0),	! (0 0)
	(kendproc,			$,	0,0),	! (0 0)
	(kendprogram,		"endprog",	0,0),	! (0 0)
	(kthreadedproc,		"tcproc",	1,0),	! (0 0) (A,t) Define proc A, of given return type

	(klabel,			$,	0,0),	! (0 0) (L) Define numbered label L

	(kload,				$,	1,0),	! (0 1) (X,t)	Push operand X of type t; X is anything pushable
	(kstore,			$,	1,0),	! (1 0) (L,t)	pop to label X
	(kloadlabel,		"loadref",	0,0),	! (0 1) (L)		Push address of label L
	(kdupl,				$,	1,0),	! (1 1+) ()		Step count for operand X
	(kswapopnds,		"swapstk",	1,0),	! (2 2) ()		(X', Y') := (Y, X)
	(kunload,			$,	1,0),	! (1 0) ()		Pop stack

	(kopnd,				$,	0,0),	! (0 0) (X) Define auxiliary operand X (not sure about extra stuff yet)
	(ktype,				$,	1,0),	! (0 0) (t) Define auxiliary type t
!	(kcopyblock,		$,	1,0),	! (1 1) (A, t) Copy block to A; X' := A

	(kiloadx,			"iloadx",	1,2),	! (2 1) (t,scale,offset) X' := (X+Y*scale+offset)^ using given type
	(kistorex,			"istorex",	1,2),	! (3 0) (t,scale,offset) (Y+Z*scale+offset)^:=X

	(kiload,			"iloadc",	1,0),	! (1 1) X' := X^
	(kistore,			"istorec",	1,0),	! (2 0) Y^:=X

	(kloadbit,			$,	1,0),	! (2 1)	X' := X.[Y]
	(kstorebit,			$,	1,0),	! (3 0) Y^.[Z]:=X

	(kloadbf,			$,	1,0),	! (3 1) X' := X.[Y..Z]
	(kstorebf,			$,	1,0),	! (4 0) Y^.[Zb..W]:=X

	(keval,				$,	1,0),	! (1 0) Evaluate X [load to an actual register], then pop

	(kcallp,			$,	0,2),	! (n 0) (A) Call &A with nargs, then pop args
	(kicallp,			$,	0,2),	! (n+1 0) Call X with nargs, then pop args
	(kretproc,			$,	0,0),	! (0 0) Return from proc

	(kcallf,			$,	1,2),	! (n 1) (A, t), Call &A, then pop args, leave retval
	(kicallf,			$,	1,2),	! (n+1 1) (t) Call X, then pops args, leave retval
	(kretfn,			$,	1,0),	! (0 0) (t) Return from function with X=retval

	(kjump,				$,	0,0),	! (0 0) (L) goto L
	(kjumpptr,			$,	0,0),	! (1 0) goto X

	(kjumpeq,			"jumpeq",	1,1),	! (2 0/1) (L,t,p1) goto L when X = Y; p1=1: X':=X
	(kjumpne,			"jumpne",	1,1),	! (2 0/1) (L,t,p1) goto L when <>
	(kjumplt,			"jumplt",	1,1),	! (2 0/1) (L,t,p1) goto L when X < Y
	(kjumple,			"jumple",	1,1),	! (2 0/1) (L,t,p1) goto L when <=
	(kjumpge,			"jumpge",	1,1),	! (2 0/1) (L,t,p1) goto L when >=
	(kjumpgt,			"jumpgt",	1,1),	! (2 0/1) (L,t,p1) goto L when >

	(kjumptrue,			"jumpt",	1,0),	! (1 0) (L,t) goto L when X is true
	(kjumpfalse,		"jumpf",	1,0),	! (1 0) (L,t) goto L when X is false
	(kjumpret,			"jumpret",	1,0),	! (1 0) (L,t) goto L return point, returning type t

	(kseteq,			"seteq",	1,0),	! (2 1) (t) X' := X = Y
	(ksetne,			"setne",	1,0),	! (2 1) (t) X' := X <> Y
	(ksetlt,			"setlt",	1,0),	! (2 1) (t) X' := X < Y
	(ksetle,			"setle",	1,0),	! (2 1) (t) X' := X <= Y
	(ksetge,			"setge",	1,0),	! (2 1) (t) X' := X >= Y
	(ksetgt,			"setgt",	1,0),	! (2 1) (t) X' := X > Y

	(kcasejumpeq,		$,	1,1),	! (2 1) (L,t) goto L when X=Y; pop Y, leave X

	(kto,				$,	0,0),	! (0 0) (L)(B,t) --B (aux); goto L when B<>0 

	(kforup,			$,	1,1),	! (0 0) (L,t,n)(B,t)(C,t) B+:=n; goto L when B<=C
	(kfordown,			$,	1,1),	! (0 0) (L,t,n)(B,t)(C,t) B-:=n; goto L when B>=C

	(kswap,				"iswap",	1,0),	! (2 0) (t) swap(X^,Y^) ref T/V

	(kstoreslice,		"storesl",	1,0),	! (2 0) (t) A:=slice(X, Y); pop X,Y
	(kstoresliced,		"storesld",	1,0),	! (2 1) (t) A:=slice(X, Y); leave A on stack

	(kswitch,			$,	0,2),	! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
	(kswitchu,			$,	0,2),	! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
	(kswitchlabel,		"swlabel",	0,0),	! (0 0) (L) jumptable entry
	(kendswitch,		"endsw",	0,0),	! (0 0)	Mark end of switch jumptable

	(kclear,			$,	1,0),	! (1 0) (t) Clear X^

!	(kdb,				"data",	0,0),	! (0 0) (X) Define a u8 data value
!	(kdw,				"data",	0,0),	! (0 0) (X) u16 value: ...
!	(kdd,				"data",	0,0),	! (0 0) (X) u32 value: u32/i32/r32, depends on operand
!	(kdq,				"data",	0,0),	! (0 0) (X) u64 value: u64/i64/r64/string/addr/label, depends on operan

	(kdb,				"db",	0,0),	! (0 0) (X) Define a u8 data value
	(kdw,				"dw",	0,0),	! (0 0) (X) u16 value: ...
	(kdd,				"dd",	0,0),	! (0 0) (X) u32 value: u32/i32/r32, depends on operand
	(kdq,				"dq",	0,0),	! (0 0) (X) u64 value: u64/i64/r64/string/addr/label, depends on operan

	(kdata,				"data",	0,0),	! (0 0) (X,t) strdata value

	(kassem,			$,	0,0),	! (0 0) to be worked out....

	(kadd,				$,	1,0),	! (2 1) (t) X' := X + Y
	(ksub,				$,	1,0),	! (2 1) (t)
	(kmul,				$,	1,0),	! (2 1) (t)
	(kdiv,				$,	1,0),	! (2 1) (t)
	(kidiv,				$,	1,0),	! (2 1) (t)
	(kirem,				$,	1,0),	! (2 1) (t)
	(kidivrem,			$,	1,0),	! (2 2) (t)
	(kiand,				"bitand",	1,0),	! (2 1) (t)
	(kior,				"bitor",	1,0),	! (2 1) (t)
	(kixor,				"bitxor",	1,0),	! (2 1) (t)
	(kshl,				$,	1,0),	! (2 1) (t)
	(kshr,				$,	1,0),	! (2 1) (t)
	(kin,				$,	1,0),	! (2 1) (t)
	(knotin,			$,	1,0),	! (2 1) (t)
	(kmin,				$,	1,0),	! (2 1) (t)
	(kmax,				$,	1,0),	! (2 1) (t)
	(ksame,				$,	1,0),	! (2 1) (t)
	(kandl,				$,	1,0),	! (2 1) (t)
	(korl,				$,	1,0),	! (2 1) (t)
	(kaddrefx,			"addpx",	1,2),	! (2 1) (t,scale,offset) X' := X + Y*scale + offset
	(ksubrefx,			"subpx",	1,2),	! (2 1) (t,scale,offset) X' := X - Y*scale + offset
	(ksubref,			"subp",	1,1),	! (2 1) (t,scale) X' := (X - Y)/scale

	(kneg,				$,	1,0),	! (1 1) (t) X' := -X
	(kabs,				$,	1,0),	! (1 1) (t)
	(kinot,				"bitnot",	1,0),	! (1 1) (t)
	(knotl,				"not",	1,0),	! (1 1) (t)
	(kistruel,			"notnot",	1,0),	! (1 1) (t)
	(ksqr,				$,	1,0),	! (1 1) (t)
!	(kbyteswap,			$,	1,0),	! (1 1) (t)

	(ksqrt,				$,	1,0),	! (1 1) (t) X' := sqrt(X)
	(ksin,				$,	1,0),	! (1 1) (t)
	(kcos,				$,	1,0),	! (1 1) (t)
	(ktan,				$,	1,0),	! (1 1) (t)
	(kasin,				$,	1,0),	! (1 1) (t)
	(kacos,				$,	1,0),	! (1 1) (t)
	(katan,				$,	1,0),	! (1 1) (t)
	(klog,				$,	1,0),	! (1 1) (t)
	(klog10,			$,	1,0),	! (1 1) (t)
	(kexp,				$,	1,0),	! (1 1) (t)
	(kround,			$,	1,0),	! (1 1) (t)
	(kfloor,			$,	1,0),	! (1 1) (t)
	(kceil,				$,	1,0),	! (1 1) (t)
	(ksign,				$,	1,0),	! (1 1) (t)
	(katan2,			$,	1,0),	! (1 1) (t)
	(kpower,			$,	1,0),	! (1 1) (t)
	(kfmod,				$,	1,0),	! (1 1) (t)

	(kincr,				"incrto",	1,1),	! (1 0) (t,step) X^+:=step
	(kdecr,				"decrto",	1,1),	! (1 0) (t,step) X^-:=step
	(kincrload,			$,	1,1),	! (1 1) (t,step) X' := (X+:=step)^
	(kdecrload,			$,	1,1),	! (1 1) (t,step) X' := (X-:=step)^
	(kloadincr,			$,	1,1),	! (1 1) (t,step) X' := X++^ (difficult to express step)
	(kloaddecr,			$,	1,1),	! (1 1) (t,step) X' := X--^

	(kaddto,			$,	1,0),	! (2 0) (t) X^ +:= Y
	(ksubto,			$,	1,0),	! (2 0) (t)
	(kmulto,			$,	1,0),	! (2 0) (t)
	(kdivto,			$,	1,0),	! (2 0) (t)
	(kidivto,			$,	1,0),	! (2 0) (t)
	(kiremto,			$,	1,0),	! (2 0) (t)
	(kiandto,			"bitandto",	1,0),	! (2 0) (t)
	(kiorto,			"bitorto",	1,0),	! (2 0) (t)
	(kixorto,			"bitxorto",	1,0),	! (2 0) (t)
	(kshlto,			$,	1,0),	! (2 0) (t)
	(kshrto,			$,	1,0),	! (2 0) (t)
	(kminto,			$,	1,0),	! (2 0) (t)
	(kmaxto,			$,	1,0),	! (2 0) (t)
	(kandlto,			$,	1,0),	! (2 0) (t)
	(korlto,			$,	1,0),	! (2 0) (t)
	(kaddrefxto,		"addpxto",	1,2),	! (2 0) (t,scale,offset) X^ +:= Y
	(ksubrefxto,		"subpxto",	1,2),	! (2 0) (t,scale,offset) X^ -:= Y

	(knegto,			$,	1,0),	! (1 0) (t) -:=X^
	(kabsto,			$,	1,0),	! (1 0) (t)
	(kinotto,			"bitnotto",	1,0),	! (1 0) (t)
	(knotlto,			"notto",	1,0),	! (1 0) (t)
	(kistruelto,		"notnotto",	1,0),	! (1 0) (t)

!for conversions, t is always the current operand type
!u is the new type. However, for conversions involving widening, the result
!must end up at at least 64 bit. So i64->u8 masks to 8 bits, the sign-extends to u64

!	(ktypepun,			$,	2,1),	! (1 1) (t,u)
	(ktypepun,			$,	2,0),	! (1 1) (t,u)

	(ksoftconv,			$,	2,0),	! (1 1) (t,u) temporary opcode used internally

	(kfloat,			$,	2,0),	! (1 1) (t,u) X' := cast(X,t) Int u to real t
	(kfix,				$,	2,0),	! (1 1) (t,u) X' := cast(X,t) Real u to int t
	(ktruncate,			$,	2,0),	! (1 1) (t,u) X' := cast(X,u) Mask to width of u, but type is widened to i64/u64
	(kfwiden,			$,	2,0),	! (1 1) (t,u) X' := cast(X,u) r32 to r64
	(kfnarrow,			$,	2,0),	! (1 1) (t,u) X' := cast(X,u) r64 to r32

!These ones e currently still needed by or all PCL targets

	(kstartmx,			$,	0,0),
	(kresetmx,			$,	0,0),
	(kendmx,			$,	0,0),
	(ksetret,			$,	1,0),	! (0 0) (t) Set X as return value of type t
!	(ksetret,			$,	1,0),	! (0 0) (t) Set X as return value of type t
	(ksetretmult,		$,	0,1), ! (0 0) (n) Set N return values
	(ksetcall,			$,	0,1), ! (nargs, nvars)
	(ksetarg,			$,	0,1), ! (nargs, nvars)
	(kloadall,			$,	0,0), ! Load all pending pcl mem ops to regs or temps

!these are ecial ones used reflection

	(kgetnprocs,		"getnfuns",	0,0), ! (0 1) X' := Get number of functions in function table
	(kgetprocname,		"getfname",	0,0), ! (1 1) X' := Getprocname(X) Name of nth function (1-based)
	(kgetprocaddr,		"getfaddr",	0,0), ! (1 1) X' := Getprocaddr(X) Addr of nth function (1-based)

!ops used ternally by M compiler until they can be replaced
!(usually they ll be turned into something else, constants etc)
	(keq,				$,	1,0),
	(kne,				$,	1,0),
	(klt,				$,	1,0),
	(kle,				$,	1,0),
	(kge,				$,	1,0),
	(kgt,				$,	1,0),

	(klen,				$,	0,1),
	(klwb,				$,	0,0),
	(kupb,				$,	0,1),
	(kbounds,			$,	0,0),
	(klenstr,			$,	0,0),
	(kbitwidth,			$,	0,0),
	(kbytesize,			$,	0,0),
	(kminvalue,			$,	0,0),
	(kmaxvalue,			$,	0,0),
	(ktypestr,			$,	0,0),
	(kerror,			$,	0,0),
	(kharderror,		$,	0,0),
	(karraytoslice,		$,	0,0),
	(kichartoslice,		$,	0,0),
	(ksofttruncshort,	$,	0,0),
	(kcharaxtoichar,	$,	0,0),
	(ksliceptr,			$,	0,0),

	(kloadref,			$,	0,0),
	(kloadimm,			$,	0,0),

	(klast,				$,	0,0),	! (0 0)
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

!global symbol currproc
global ref pclrec pclprocdef	!ref to kprocdef opcode of currproc

global int frameoffset
global int paramoffset
global int framebytes

global const maxparams=32
global const maxlocals=256

!these are reset at each procdef
global [maxparams]symbol paramdefs
global [maxlocals]symbol localdefs
global int nlocals				!no. of locals that are used, both int/float
global int nparams				!no. of params whether used or not, int and float

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

global byte assemused			!1 if assem used

global int passno

=== mm_support.m 0 0 19/38 ===
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
		pos:=mlineno
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

global proc axerror(ichar mess)=
	CPL =ALINENO
	error_gen('A',mess)
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

global function isalphanum(int c)int=
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
		bitsize:=stdbits[i]

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

		if stdcat[i]=intcat and size<8 then
			ttisshort[i]:=1
		fi

		ttlower[i]:=1

		ttcat[i]:=stdcat[i]
		ttisblock[i]:=stdcat[i]=blockcat

	od

	ttbasetype[trefchar]:=tref
	tttarget[trefchar]:=tc8

	ntypes:=tlast-1

	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
end

global function getsupportfile(ichar filename, ext="", path="")ifile =
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

function isabspath(ichar filespec)int=
	ichar path:=extractpath(filespec)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		return 1
	fi
	return 0
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end

global function getfileno(word pos)int fileno=
	fileno:=pos.[24..31]
	if fileno<1 or fileno>nsourcefiles then
RETURN 1
!		abortprogram("No file no")
	fi
	return fileno
end

global function getlineno(word pos)int=
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

function getsourceline(word pos)ichar=
	ichar source := getsourcestart(pos)
	ichar s :=  getsourcepos(pos)

	while s>source and s^<>10 do --s od
	if s^=10 then ++s fi

	return s
end

function getsourcestart(word pos)ichar=
	return sources[getfileno(pos)].text
end

function getsourcepos(word pos)ichar=
	return sources[getfileno(pos)].text+pos.[0..23]
end

global proc do_writema(ichar inpfile)=
	[300]char filename
	[maxsourcefile]int sflist
	filehandle f
	int offset, nfiles, fileno

	if not fwritema then
		return
	fi

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

	if fgetst then

CPL "GETST"

		f:=fopen(fs:=changeext(filename,"list"),"wb")
		if f then
			println "Writing",fs
!F:=NIL
			getst(f,stprogram)
			fclose(f)
		fi
	elsif fgetproj then
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
=== mm_tables.m 0 0 20/38 ===
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
	(sf_sign_i64,			$,	0,	1),
	(sf_sign_r64,			$,	0,	1),
	(sf_unimpl,				$,	0,	1),

end
!
global [sysfnnames.len]symbol sysfnhandlers

!global [sysfnnames.len]int sysfnproclabels

global int mlineno
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

!Logical Operators

	(jandl,			$,	2,		2,	0), ! A B	This group are for conditional expressions (no result)
	(jorl,			$,	2,		2,	0), ! A B
!	(jxorl,			$,	0,		2,	0), ! A B
	(jnotl,			$,	1,		1,	0), ! a
	(jistruel,		$,	1,		1,	0), ! a

!Expressions and Operators

	(jmakelist,		$,	2,		3,	0), ! a=L, b=[u], length=N; element list/lower bound expr
	(jmakerange,	$,	2,		3,	0), ! A B
	(jmakeset,		$,	1,		3,	0), ! a=L, length=N
	(jmakedict,		$,	1,		3,	0), !
	(jmakeslice,	$,	2,		3,	0), !
	(jreturnmult,	$,	1,		3,	0), !

	(jkeyword,		$,	1,		3,	0), ! def=st entry
	(jkeyvalue,		$,	2,		3,	0), ! A B
	(jassign,		$,	2,		3,	1), ! A B a := x
	(jassignmm,		$,	2,		3,	1), ! A B (a,b,c) := (x,y,z)
	(jassignms,		$,	2,		3,	1), ! A B (a,b,c) := x
	(jassignmdrem,	$,	2,		3,	1), ! A B (a,b) := x divrem y
	(jcallfn,		$,	2,		3,	1), ! A B

	(jcmp,			$,	2,		2,	0), ! A B
	(jcmpchain,		$,	2,		1,	0), ! A B
	(jbin,			$,	2,		2,	0), ! A B
	(junary,		$,	2,		1,	0), ! A B
	(jbinto,		$,	2,		2,	0), ! A B
	(junaryto,		$,	1,		1,	0), ! A B
	(jincr,			$,	1,		3,	0), ! a	++a

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

	(jcallproc,		$,	2,		0,	1), ! a=fn b=L, length
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
	(dotsym,			".",		0,	0,	0,	0,	0),		! "."
	(anddotsym,			"&.",		0,	0,	0,	0,	1),		! "&."
	(commasym,			",",		0,	0,	0,	0,	0),		! ","
	(semisym,			";",		0,	0,	0,	0,	0),		! ";"
	(colonsym,			":",		0,	0,	0,	0,	0),		! ":"
	(assignsym,			":=",		bin_op,	0,	0,	1,	0),		! :=
	(sendtosym,			"=>",		0,	0,	0,	0,	0),		! =>
	(pipesym,			"->",		0,	0,	0,	0,	0),		! ->
	(lbracksym,			"(",		0,	0,	0,	0,	1),		! (
	(rbracksym,			")",		0,	0,	0,	0,	0),		! )
	(lsqsym,			"[",		0,	0,	0,	0,	1),		! [
	(rsqsym,			"]",		0,	0,	0,	0,	0),		! ]
	(lcurlysym,			"{",		0,	0,	0,	0,	0),		! {
	(rcurlysym,			"}",		0,	0,	0,	0,	0),		! }
	(ptrsym,			"^",		0,	0,	0,	0,	1),		! ^
	(barsym,			"|",		0,	0,	0,	0,	0),		! |
!	(dbarsym,			"||",		0,	0,	0,	0,	0),		! ||
	(atsym,				"@",		0,	0,	0,	0,	0),		! @
!	(datsym,			"@@",		0,	0,	0,	0,	0),		! @@
	(questionsym,		"?",		0,	0,	0,	0,	0),		! ?
	(addrsym,			"&",		0,	0,	0,	0,	1),		! &
	(daddrsym,			"&&",		0,	0,	0,	0,	0),		! &&
!	(curlsym,			"~",		0,	0,	0,	0,	0),		! ~
	(rangesym,			"..",		bin_op,	0,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,	0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,	0,	0,	0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		kadd,		kaddto,		4,	1),
	(subsym,			"-",		bin_op,		ksub,		ksubto,		4,	1),
	(mulsym,			"*",		bin_op,		kmul,		kmulto,		3,	0),
	(divsym,			"/",		bin_op,		kdiv,		kdivto,		3,	0),
	(idivsym,			"%",		bin_op,		kidiv,		kidivto,	3,	0),
	(iremsym,			"rem",		bin_op,		kirem,		kiremto,	3,	0),
	(idivremsym,		"divrem",	bin_op,		kidivrem,	0,			3,	0),
	(iandsym,			"iand",		bin_op,		kiand,		kiandto,	4,	0),
	(iorsym,			"ior",		bin_op,		kior,		kiorto,		4,	0),
	(ixorsym,			"ixor",		bin_op,		kixor,		kixorto,	4,	0),
	(shlsym,			"<<",		bin_op,		kshl,		kshlto,		3,	0),
	(shrsym,			">>",		bin_op,		kshr,		kshrto,		3,	0),
	(minsym,			"min",		bin_op,		kmin,		kminto,		4,	1),
	(maxsym,			"max",		bin_op,		kmax,		kmaxto,		4,	1),
	(andlsym,			"and",		bin_op,		kandl,		kandlto,	7,	0),
	(orlsym,			"or",		bin_op,		korl,		korlto,		8,	0),
	(xorlsym,			"xor",		bin_op,		0,			0,			8,	0),

	(eqsym,				"=",		bin_op,		keq,		0,			6,	1),
	(cmpsym,			"cmp",		bin_op,		0,			0,			6,	1),
	(powersym,			"**",		bin_op,		kpower,		0,			2,	0),
	(samesym,			"==",		bin_op,		ksame,		0,			6,	0),
!	(ssmarkersym,		"===",		0,			0,			0,			0,	0),
	(insym,				"in",		bin_op,		kin,		0,			6,	0),
	(notinsym,			"notin",	bin_op,		knotin,		0,			6,	0),
	(inrevsym,			"inrev",	0,			0,			0,			0,	0),

!	(negsym,			"$neg",		mon_op,		kneg,		0,			0,	1),
	(notlsym,			"not",		mon_op,		knotl,		knotlto,	0,	1),
	(istruelsym,		"istrue",	mon_op,		kistruel,	kistruelto,	0,	1),
	(inotsym,			"inot",		mon_op,		kinot,		kinotto,	0,	1),
	(abssym,			"abs",		mon_op,		kabs,		kabsto,		0,	1),
	(signsym,			"sign",		mon_op,		ksign,		0,			0,	1),
	(sqrtsym,			"sqrt",		mon_op,		ksqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		ksqr,		0,			0,	1),
!	(byteswapsym,		"byteswap",	mon_op,		kbyteswap,	0,			0,	1),

	(propsym,				$,		prop_op,	0,			0,			0,	0),
	(mathsopsym,		$,		0,	0,	0,	0,	1),		! sin etc
	(maths2opsym,		$,		0,	0,	0,	0,	1),		! atan2 etc

	(bitfieldsym,		$,		0,	0,	0,	0,	0),		! Special bit selections
	(eolsym,			$,		0,	0,	0,	0,	0),		! End of line
	(eofsym,			$,		0,	0,	0,	0,	0),		! Eof seen
	(rawxnamesym,		$,		0,	0,	0,	0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(incrsym,			$,		0,	0,	0,	0,	1),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,		0,	0,	0,	0,	1),		! 123 32 bits signed
	(realconstsym,		$,		0,	0,	0,	0,	1),		! 123.4 64 bits
	(charconstsym,		$,		0,	0,	0,	0,	1),		! 'A' or 'ABCD'
	(stringconstsym,	$,		0,	0,	0,	0,	1),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		$,		0,	0,	0,	0,	0),		! 
	(namesym,			$,		0,	0,	0,	0,	1),		! identifier symbol
	(kincludesym,		$,		0,	0,	0,	0,	0),		! INCLUDE
	(kstrincludesym,	$,		0,	0,	0,	0,	1),		! SINCLUDE/BINCLUDE
	(regsym,			$,		0,	0,	0,	0,	0),		! x64 registers
	(xregsym,			$,		0,	0,	0,	0,	0),		! XMM registers
	(fregsym,			$,		0,	0,	0,	0,	0),		! ST registers
	(mregsym,			$,		0,	0,	0,	0,	0),		! MMX registers
	(jmpccsym,			$,		0,	0,	0,	0,	0),		! 
	(setccsym,			$,		0,	0,	0,	0,	0),		! 
	(movccsym,			$,		0,	0,	0,	0,	0),		! 
	(segnamesym,		$,		0,	0,	0,	0,	0),		! 
	(asmopcodesym,		$,		0,	0,	0,	0,	0),		! MOV etc

	(stdtypesym,		$,		0,	0,	0,	0,	1),		! INT, CHAR etc
	(ksubrangesym,		$,		0,	0,	0,	0,	0),		! SUBRANGE
	(kicharsym,			$,		0,	0,	0,	0,	1),		! ICHAR, IVOID
	(kifsym,			$,		0,	0,	0,	0,	1),		! 
	(kthensym,			$,		0,	0,	0,	0,	0),		! 
	(kelsifsym,			$,		0,	0,	0,	0,	0),		! 
	(kelsesym,			$,		0,	0,	0,	0,	0),		! 
	(kelsecasesym,		$,		0,	0,	0,	0,	0),		! 
	(kelseswitchsym,	$,		0,	0,	0,	0,	0),		! 
	(kelseselectsym,	$,		0,	0,	0,	0,	0),		! 
	(kendsym,			$,		0,	0,	0,	0,	0),		! 
	(kunlesssym,		$,		0,	0,	0,	0,	0),		! 
	(kcasesym,			$,		0,	0,	0,	0,	1),		! CASE
	(kdocasesym,		$,		0,	0,	0,	0,	0),		! DOCASE
	(krecasesym,		$,		0,	0,	0,	0,	0),		! RECASE
	(kwhensym,			$,		0,	0,	0,	0,	0),		! 
	(kforsym,			$,		0,	0,	0,	0,	0),		! FOR
	(ktosym,			$,		0,	0,	0,	0,	0),		! TO/DOWNTO
	(kbysym,			$,		0,	0,	0,	0,	0),		! 
	(kdosym,			$,		0,	0,	0,	0,	0),		! 
	(kwhilesym,			$,		0,	0,	0,	0,	0),		! 
	(krepeatsym,		$,		0,	0,	0,	0,	0),		! 
	(kuntilsym,			$,		0,	0,	0,	0,	0),		! 
	(kreturnsym,		$,		0,	0,	0,	0,	0),		! 
	(kstopsym,			$,		0,	0,	0,	0,	0),		! 
	(kloopsym,			$,		0,	0,	0,	0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$,		0,	0,	0,	0,	0),		! GO/GOTO
	(kswitchsym,		$,		0,	0,	0,	0,	0),		! SWITCH
	(kdoswitchsym,		$,		0,	0,	0,	0,	0),		! DOSWITCH
	(kprintsym,			$,		0,	0,	0,	0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(kreadsym,			$,		0,	0,	0,	0,	0),		! READ/READLN
	(kprocsym,			$,		0,	0,	0,	0,	0),		! PROC
	(kfunctionsym,		$,		0,	0,	0,	0,	0),		! FUNCTION
	(klabelsym,			$,		0,	0,	0,	0,	0),		! LABEL
	(krecordsym,		$,		0,	0,	0,	0,	0),		! RECORD
	(kstructsym,		$,		0,	0,	0,	0,	0),		! STRUCT
	(kunionsym,			$,		0,	0,	0,	0,	0),		! UNION
	(kimportmodulesym,	$,		0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
	(kprojectsym,		$,		0,	0,	0,	0,	0),		! PROJECT
	(ktypesym,			$,		0,	0,	0,	0,	0),		! TYPE
	(ktypealiassym,		$,		0,	0,	0,	0,	0),		! TYPEALIAS
	(kextendtypesym,	$,		0,	0,	0,	0,	0),		! EXTENDTYPE
	(krefsym,			$,		0,	0,	0,	0,	1),		! REF
	(kvoidsym,			$,		0,	0,	0,	0,	1),		! VOID
	(kvarsym,			$,		0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,		0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,		0,	0,	0,	0,	0),		! SLICE/SLICE2D
	(kdictsym,			$,		0,	0,	0,	0,	0),		! DICT
!	(kflexsym,			$,		0,	0,	0,	0,	0),		! FLEX
	(kmacrosym,			$,		0,	0,	0,	0,	0),		! MACRO
	(kexpandsym,		$,		0,	0,	0,	0,	0),		! EXPAND
	(koperatorsym,		$,		0,	0,	0,	0,	0),		! OPERATOR
	(kconstsym,			$,		0,	0,	0,	0,	0),		! 
!	(kenumsym,			$,		0,	0,	0,	0,	0),		! 
	(knewsym,			$,		0,	0,	0,	0,	0),		! NEW
!	(kdestroysym,		$,		0,	0,	0,	0,	0),		! DESTROY
	(kclearsym,			$,		0,	0,	0,	0,	0),		! CLEAR
	(kclasssym,			$,		0,	0,	0,	0,	0),		! CLASS
	(kheadersym,		$,		0,	0,	0,	0,	0),		! MODULE
	(kheadervarsym,		$,		0,	0,	0,	0,	0),		! MODULE
	(kglobalsym,		$,		0,	0,	0,	0,	0),		! global
	(kstaticsym,		$,		0,	0,	0,	0,	0),		! STATIC

!	(ktrysym,			$,		0,	0,	0,	0,	0),		! 
!	(kexceptsym,		$,		0,	0,	0,	0,	0),		! 
!	(kfinallysym,		$,		0,	0,	0,	0,	0),		! 
!	(kraisesym,			$,		0,	0,	0,	0,	0),		! 
!	(kyieldsym,			$,		0,	0,	0,	0,	0),		! 
	(kcastsym,			$,		0,	0,	0,	0,	1),		! CAST
	(compilervarsym,	$,		0,	0,	0,	0,	1),		! $lineno etc
	(dollarsym,			$,		0,	0,	0,	0,	1),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,		0,	0,	0,	0,	0),		! EVAL
	(ktabledatasym,		$,		0,	0,	0,	0,	0),		! tabledata
	(kstacksym,			$,		0,	0,	0,	0,	0),		! STACK/UNSTACK
	(kclampsym,			$,		0,	0,	0,	0,	1),			! CLAMP
	(kswapsym,			$,		0,	0,	0,	0,	0),		! SWAP
	(kassemsym,			$,		0,	0,	0,	0,	0),		! ASM/ASSEM
	(ksyscallsym,		$,		0,	0,	0,	0,	1),		! $getprocname etc
!	(kemitcsym,			$,		0,	0,	0,	0,	0),		! EMITC

!	(kdummysym,			$,		0,	0,	0,	0,	0),		!
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
	(var_param=0,		"Var "),
	(in_param,			"In "),
	(out_param,			"Out "),
	(optional_param,	"Opt "),
end

global enumdata [0:]ichar namenames
	(nullid=0,		$),		!Not assigned
	(programid,		$),		!Main root
	(subprogid,		$),
	(moduleid,		$),		!Current or imported module
	(dllmoduleid,	$),		!
	(typeid,		$),		!Type name in type, proc or module
	(procid,		$),		!Proc/method/function/op name
	(dllprocid,		$),		!Dll Proc/function name
	(dllvarid,		$),		!Dll variable name
	(constid,		$),		!Named constant in type, proc or module
	(staticid,		$),		!Static in type or proc or module
	(frameid,		$),		!Local var
	(paramid,		$),		!Local param
	(fieldid,		$),		!Field of Record or Class
	(labelid,		$),		!Label name in proc only
	(macroid,		$),		!Name of macro
	(macroparamid,	$),		!Macro formal parameter name
	(linkid,		$),		!Name in class defined in a base class
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
	("threadedproc",		kprocsym,		2),

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

	("$getnprocs",		ksyscallsym,		sf_getnprocs),
	("$getprocname",	ksyscallsym,		sf_getprocname),
	("$getprocaddr",	ksyscallsym,		sf_getprocaddr),

	("importdll",	kimportmodulesym,	0),
	("project",		kprojectsym,		0),
	("unless",		kunlesssym,			0),

	("global",		kglobalsym,		subprog_scope),
	("export",		kglobalsym,		export_scope),

	("swap",		kswapsym,		0),

	("void",		kvoidsym,		0),
!	("void",		stdtypesym,		tvoid),
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
!	("bitfl",		stdtypesym,		tu8),
!	("u1",			stdtypesym,		tu1),
!	("u2",			stdtypesym,		tu2),
!	("u4",			stdtypesym,		tu4),
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

!	("range",		stdtypesym,		trange),
!	("auto",		stdtypesym,		tauto),

	("label",		stdtypesym,		tlabel),

	("slice",		kslicesym,		tslice),
!	("array",		karraysym,		0),

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
	("in",			insym,			kin),
	("notin",		notinsym,		knotin),
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

	("sliceptr",	propsym,		ksliceptr),

	("len",			propsym,	klen),
	("lwb",			propsym,	klwb),
	("upb",			propsym,	kupb),
	("bounds",		propsym,	kbounds),
!	("lenstr",		propsym,	klenstr),
	("bitwidth",	propsym,	kbitwidth),
	("bytes",		propsym,	kbytesize),
!	("minvalue",	propsym,	kminvalue),
!	("maxvalue",	propsym,	kmaxvalue),
	("typestr",		propsym,	ktypestr),

	("msb",			bitfieldsym,	bf_msb),
	("lsb",			bitfieldsym,	bf_lsb),
	("msbit",		bitfieldsym,	bf_msbit),
	("lsbit",		bitfieldsym,	bf_lsbit),
	("msw",			bitfieldsym,	bf_msw),
	("lsw",			bitfieldsym,	bf_lsw),
	("odd",			bitfieldsym,	bf_odd),
	("even",		bitfieldsym,	bf_even),

	("fi",			kendsym,	kifsym),
	("esac",		kendsym,	kcasesym),
	("od",			kendsym,	kdosym),

	("$caligned",	atsym,			1),
	("clear",		kclearsym,		0),
!	("copy",		kcopysym,		0),

	("module",		kheadersym,		hdr_module),
	("import",		kheadersym,		hdr_import),
	("$sourcepath",	kheadersym,		hdr_sourcepath),
	("linkdll",		kheadersym,		hdr_linkdll),
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
		kicharsym, kslicesym, kdictsym)

!list of genops that have an int result, used to populate intresult[]
[]byte intresultlist = (
	kin, knotin, klwb, kupb, klen, klenstr, kbitwidth,
	kbytesize)

global [tc64..tr64, tc64..tr64]int16 softconvtable = (
!To: c64		u64			i64			r32			r64				 From:
	(ksoftconv,	ksoftconv,	ksoftconv,	kfloat,		kfloat),	 	!c64
	(ksoftconv,	ksoftconv,	ksoftconv,	kfloat,		kfloat),	 	!u64
	(ksoftconv,	ksoftconv,	ksoftconv,	kfloat,		kfloat), 		!i64
	(kfix,		kfix,		kfix,		ksoftconv,	kfwiden),	 	!r32
	(kfix,		kfix,		kfix,		kfnarrow,	ksoftconv)) 	!r64

global [pclnames.lwb..pclnames.upb]byte intresult

global [symbolnames.lwb..symbolnames.upb]byte endsexpr
global []byte exprendsymbols=(rbracksym,rsqsym,kthensym,kelsifsym,
			kelsesym, kuntilsym, kdosym, kendsym, commasym, barsym,
			semisym, ktosym)

global [jtagnames.lwb..jtagnames.upb]byte isbooltag

proc start=
	int genop, s,t, a, specop

!populate intresultlist
	for i in intresultlist.bounds do
		intresult[intresultlist[i]]:=1
	od

	for i to exprendsymbols.len do
		endsexpr[exprendsymbols[i]]:=1
	od

	isbooltag[jcmp]:=1
	isbooltag[jcmpchain]:=1
	isbooltag[jandl]:=1
	isbooltag[jorl]:=1
	isbooltag[jnotl]:=1
	isbooltag[jistruel]:=1
	isbooltag[jinrange]:=1
	isbooltag[jinset]:=1
end

=== mm_type.m 0 0 21/38 ===
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
	int oldmlineno,m,nparams,paramtype,restype,amode
	static int depth

	if p=nil then return fi
	if depth=100 then
		txerror("TX looping detected")
	fi
	++depth

	oldmlineno:=mlineno

!CPL "TPASS------------------------", JTAGNAMES[P.TAG]

	mlineno:=p.pos

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

	when junary then
		tx_unary(p,a)

	when jbinto then
		tx_binto(p,a,b)

	when junaryto then
		tpasslv(a)
		p.mode:=tvoid

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

	when jcallproc, jcallfn then
		tx_callproc(p,a,b,t)

	when jdot then
		tx_dot(p,a,b,lv)

	when jandl, jorl then
		tx_andl(p,a,b)

	when jnotl then
		tx_notl(p,a)

	when jistruel then
		tx_istruel(p,a)

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

	when jcvlineno then
		p.mode:=ti64
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

	mlineno:=oldmlineno
	--depth
end

global proc tx_allprocs=
	ref procrec pp
	unit pcode

	pp:=proclist
	while pp, pp:=pp.nextproc do
		currproc:=pp.def
		pcode:=currproc.code

		if ttisshort[currproc.mode] then
			mlineno:=currproc.pos
			txerror("proc short ret type")
		 fi

		symbol d:=currproc.deflist
		while d, d:=d.nextdef do
			if d.nameid=paramid then
				if ttisblock[d.mode] and d.parammode<>out_param then
					d.parammode:=out_param
					d.mode:=createrefmode(nil, d.mode)
				fi
			fi
		od
	od

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


	mlineno:=ttlineno[m]
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

global function tx_module(int n)int=
	modulerec m
	symbol d
	int globalflag,status

	currmoduleno:=n

	tx_passdef(modules[n].stmodule)

	return 1
end

global proc tx_passdef(symbol p)=
	symbol d
	int oldmlineno
	unit q

	if p.txdone then
		return
	fi

	oldmlineno:=mlineno
	mlineno:=p.pos

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
	mlineno:=oldmlineno
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
		error
	else
	error:
		println =jtagnames[p.tag],STRMODE(P.MODE)
	 PRINTUNIT(P)
		txerror("Getconstexpr: not const")
	esac
end

function getconstint(unit q)int64=
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
	int oldmlineno
	unit pcode
	oldmlineno:=mlineno

IF P.TXCOUNT THEN
!CPL =P.TXCOUNT,P.DEF.NAME
RETURN
FI
++P.TXCOUNT

	d:=p.def
!CPL "/////////////////NAME", D.NAME,D.TXDONE
	mlineno:=d.pos

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
			if d.parammode=out_param then
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
		mlineno:=p.pos
		txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
	esac
	mlineno:=oldmlineno

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
				p.pclop:=kaddrefx
				p.mode:=amode
				return
			fi
		fi

	when ksub then
		if dobinnumx(p,a,b) then return fi
		if ttisref[amode] then
			if ttisref[bmode] then
				if comparemodes(amode, bmode) then
					p.pclop:=ksubref
					p.mode:=ti64
					return
				else
					txerror("ref-ref: not compat")
				fi
			fi
			if isnum(bmode) then
				coerceunit(b,ti64)
				p.pclop:=ksubrefx
				p.mode:=amode
				return
			fi

		fi

	when keq, kne, klt, kle, kge, kgt then
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
		if p.pclop in [keq, kne] then
			if comparemodes(amode, bmode) then
				return
			fi
		fi

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

	when kidiv, kirem, kidivrem, kiand, kior, kixor then
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

	when kin, knotin then
		doin(p,a,b)
		return

	when kandl, korl then
		p.mode:=tbool
		if amode=bmode=tbool then return fi

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
			p.pclop:=kaddrefxto
			return
		fi
	when ksubto then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubrefxto
			return
		fi
	when kshlto, kshrto then
		coerceunit(b,ti64)
		return
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

function getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if isnum(abase) and isnum(bbase) then
		return max(abase,bbase)
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

	p.mode:=d.mode				!type returned by function (will be void for procs)

	if p.mode=tvoid and p.tag=jcallfn then
		p.tag:=jcallproc
	fi

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
		if t=tvoid then
			p.tag:=jcallproc
		fi
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

		if pm.parammode=out_param then
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

	if t=tvoid then
		p.tag:=jcallproc
	fi
end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.pclop
	when klwb, kupb, klen, kbounds then
!CPL "TX BOUNDS"
		do_bounds(p,a)
		return

	when kbytesize,kbitwidth then
!		size:=ttsize[(a.tag=jtypeconst|a.value|amode)]*(p.pclop=kbytesize|1|8)
		size:=ttsize[(a.tag=jtypeconst|a.value|getmemmode(a))]*(p.pclop=kbytesize|1|8)
		makenewconst(p,size)
		resmode:=ti64

	when kminvalue, kmaxvalue then
!CPL "TX MIN/MAX"
		resmode:=ti64
		if a.tag=jtypeconst then
			mbase:=ttbasetype[a.value]
		else
			mbase:=ttbasetype[getmemmode(a)]
		fi

		if p.pclop=kminvalue then
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

	when katan, klog, klog10, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos,
			kfloor, kceil then
		coerceunit(a,tr64)
		resmode:=tr64

	when ktypestr then
		p.tag:=jconst
		if a.tag=jtypeconst then
			amode:=a.value
		else
			amode:=getmemmode(a)
		fi

		p.mode:=trefchar
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return
	when ksliceptr then
		tx_sliceptr(p,a)
		return
	esac

	p.mode:=resmode
end

proc tx_if(unit p,pcond,plist,pelse, int t,lv) =
	unit pc:=pcond, pl:=plist
	int u

	u:=tvoid
	if t<>tany then u:=t fi

!CPL "IF",=STRMODE(T),=lv
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
		when kincr then p.pclop:=kincrload
		when kdecr then p.pclop:=kdecrload
		esac
		p.mode:=gettypebase(a.mode)
	else				!a++ a-- to ++a --a
		case p.pclop
		when kloadincr then p.pclop:=kincr
		when kloaddecr then p.pclop:=kdecr
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
		ttcat[m]:=intcat
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

function roundoffset(int offset, alignment)int=
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

function resolvefield(symbol d, int m)symbol=
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

proc tx_istruel(unit p,a)=
	int abase

	tpass(a)

	if isbooltag[a.tag] then
		deleteunit(p,a)
		return
	fi

	abase:=ttbasetype[a.mode]
	if abase=tref then abase:=ti64 fi

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
	when jname, jptr, jindex, jdot then
			p.memmode:=m				!non-void marks this as non-lv too
			p.mode:=gettypebase(m)
	when jcallproc,jcallfn then
		p.memmode:=m
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
	when klwb then
		case mbase
		when tarray,tslice then
			convintconst(p,ttlower[m])
			return
		else
error:
			txerror_s("lwb/upb/len?",strmode(m))
		esac

	when kupb then
		case mbase
		when tarray then
			convintconst(p,ttlower[m]+ttlength[m]-1)
		when tslice then
			p.pclop:=kupb
		else
			goto error
		esac

	when klen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.pclop:=klen
		else
			goto error
		esac
	when kbounds then
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
	p.pclop:=knotl
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

function addrdotindex(unit p, int &offset)unit q=
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
				if p.pclop=ksubrefx then
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
		when keq then c:=a=b
		when kne then c:=a<>b
		when klt then c:=a<b
		when kle then c:=a<=b
		when kge then c:=a>=b
		when kgt then c:=a>b
		when kandl then c:=a and b
		when korl then c:=a or b
		when kiand then c:=a iand b
		when kior then c:=a ior b
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

	a:=p.a.value
	x:=p.a.xvalue

	case p.mode
	when ti64, tu64 then

		case p.pclop
		when kneg then c:=-a

		when kistruel then c:=istrue a; p.mode:=tbool
		when knotl then c:=not a; p.mode:=tbool
		when kinot then c:=inot a
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
		case p.pclop
		when kistruel then c:=istrue a; p.mode:=tbool
		when knotl then c:=not a; p.mode:=tbool
		esac
	else
		return
	esac

	if ttisreal[p.mode] then
		makenewconst(p,int64@(z))
	else
		makenewconst(p,c)
	fi
end

function iscondtrue(unit p)int =
	p.tag=jconst and p.value<>0
end

function iscondfalse(unit p)int =
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

	opc:=kerror
	int starg:=tttarget[s]
	int ttarg:=tttarget[t]

	if s=trefchar then sbase:=trefchar fi
	if t=trefchar then tbase:=trefchar fi

	if sbase in tfirstnum..tlastnum then
		if tbase in tfirstnum..tlastnum then
			opc:=softconvtable[sbase,tbase]
		elsecase tbase
		when tref, trefchar then
			opc:=ksoftconv
checkhard:
			if not hard then opc:=kharderror fi
		elsif tbase in tfirstshort..tlastshort then
			if ttisinteger[sbase] then
				if not hard then				!needed for idata init
					opc:=ksofttruncshort
				else
					opc:=ktruncate
				fi
			fi
		elsecase tbase
		when tbool then
			opc:=kistruel
		when ttype then
			opc:=ksoftconv
		fi

	elsecase sbase
	when tbool then
		if tbase in [ti64, tu64] then
			opc:=ksoftconv
		fi

	when tref then
		case tbase
		when ti64, tu64 then
			opc:=ksoftconv
			checkhard
		when tref then
			if starg=tvoid or ttarg=tvoid then			!at least one is ref void
				opc:=ksoftconv
			else
checkref:
!CPL "HERE"
				opc:=ksoftconv
				if not comparemodes(s,t) then
					checkhard
				fi
			fi
		when trefchar then
			checkref
		when tbool then
			opc:=kistruel
		end

	when trefchar then
!CPL "REFCHAR"
!FPRINTLN "GCO/REFCHAR # => #", STRMODE(S), STRMODE(T),=HARD
		case tbase
		when ti64,tu64 then
			opc:=ksoftconv
			checkhard
		when tref then
			if comparemodes(s,t) or hard then
				opc:=ksoftconv
!CPL "SOFT"
			else
!CPL "HARD1"
				opc:=kharderror
			fi
		when tbool then
			opc:=kistruel
		when tslice then
!			if ttarg not in [tc8, tu8] then
			if ttarg in [tc8, tu8] then
				opc:=kichartoslice
			fi
		when tarray then
			if p.tag=jconst and p.strtype then
!CPL "CONSTR STR:",=P.ISSTRINCL,=T
!PRINTUNIT(P)		
				opc:=ksoftconv
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
				opc:=ksoftconv
			fi
		when tslice then
			if comparemodes(starg, ttarg) then
				opc:=karraytoslice
			fi

		when trefchar then
			if starg in [tc8, tu8] then
				opc:=kcharaxtoichar
			fi
		when tref then
			if ttarg=tvoid then
				opc:=kcharaxtoichar
			fi
!	ELSE
!CPL "GETCONV =>ARRAY",STRMODE(S)

		esac

	when tslice then
		case tbase
		when tslice then
			if comparemodes(s,t) then
				opc:=ksoftconv
			fi
		when tref then
			if ttarg=tvoid or comparemodes(starg, ttarg) then
				opc:=ksliceptr
			fi

!		when tbool then
		esac

	when ttype then
		if tbase<=tlastnum then
			opc:=ksoftconv

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
	when kzero then					!none needed
		return
	when kerror then
		txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

	when kharderror then
!CPL =S,=T,=TTTARGET[T],"//",STRMODE(40),=TTLENGTH[40]
		txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

	when ksoftconv then
!		if p.tag=jaddrof or comparemodes(s,t) then
			p.mode:=t
			return
!		fi
	when ksofttruncshort then
		if tevalconvert(p,s,t,opc) then
			return
		fi
		insertunit(p,jshorten)
		p.mode:=t			!don't use the short target mode
		return

	when karraytoslice then
		insertunit(p,jslice)
		p.mode:=t
		return
	when kichartoslice then
		tstringslice(p,t)
		return

	when kcharaxtoichar then
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

function comparemodes(int s,t)int=
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

function tevalconvert(unit p,int s,t,opc)int=
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
		z:=real32(x)

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

	elsif ttisshort[m] and needres then
		p.memmode:=m
		p.mode:=gettypebase(m)
		tpass(b,p.mode)

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

function dobinnumx(unit p,a,b)int=
!Try and apply this to binary operands:
!	NUMX	NUMX	DOM
!a and b have already been processed, but not coerced to any type yet

	int amode:=a.mode, bmode:=b.mode, cmode

	if isnum(amode) and isnum(bmode) then
		p.mode:=cmode:=max(amode, bmode)
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

function dobinnumf(unit p,a,b)int=
!Try and apply this to binary operands:
!	NUMF	NUMF	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

!	if amode=ti64 then coerceunit(a, tr64); amode:=tr64 fi
!	if bmode=ti64 then coerceunit(b, tr64); bmode:=tr64 fi

	if isnumf(amode) and isnumf(bmode) then
		p.mode:=cmode:=max(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

function dobinnumi(unit p,a,b)int=
!Try and apply this to binary operands:
!	NUMI	NUMI	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

	if isnumi(amode) and isnumi(bmode) then
		p.mode:=cmode:=max(amode, bmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

function doin(unit p,a,b)int=
	int simpleset
	unit q

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
	if p.pclop=knotin then
		addnotl(p)
	fi
	return 1
end
=== mc_genmcl.m 0 0 22/38 ===
!const fshowpcl=1
!const fshowopndstack=1
const fshowpcl=0
const fshowopndstack=0

ref mclrec mclprocentry
ref mclrec mce_oldmccodex, mce_nextmcl		!used by reset/setmclentry

pcl currpcl
[0..klast]ref proc(pcl) px_handlertable

[0..5]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[0..5]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

[]int multregs=(r0,r1,r2,r10,r11,r12)
[]int multxregs=(r0,r1,r2,r3,r4,r5)

global proc genmcl=
!	int tt

	if mcldone then return fi

	IF FCHECKUNUSEDLOCALS THEN
		CHECKGLOBALS()
	FI

!	tt:=clock()

	inithandlers()

	mclinit()

!	for i to nlibfiles when libfiles[i]^<>'$' do
!		genpc((libtypes[i]='D'|kimportdll|kimportlib), genpc_name(libfiles[i]))
!	od

	currpcl:=pcstart

	repeat
		convertpcl(currpcl)
		++currpcl
	until currpcl.opcode=kendprogram

	genabsneg()
	genstringtable()
 
	genrealtable()

	genfunctiontable()
	genmc(m_nop)
	mcldone:=1

!	mcltime:=clock()-tt
end

proc convertpcl(pcl p)=
	[1256]char str
	ichar ss
	int m

	if fshowpcl  then
		case p.opcode
		when klabel, kcomment, kprocdef, kthreadedproc,
			kretproc, kendproc then
		else
				strcpy(&.str,"                       ")
				strcat(str, strint(getlineno(p.pos)))
				strcat(str, " ")
				strcat(&.str,strpclstr(p))
				strcat(str, " (")
				strcat(str, sources[getfileno(p.pos)].name)
				strcat(str, " )")

				mgencomment(&.str)
		esac
	fi

	mlineno:=p.pos
!CPL PCLNAMES[P.OPCODE]

	px_handlertable[p.opcode]^(p)

	if fshowopndstack then
		case p.opcode
		when klabel, kcomment, kprocdef, kthreadedproc,
			kretproc, kendproc then
		else
			showopndstack()
		esac
	fi

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
				gerrorc("Invalid handler name:",name)
			od
		fi
	od

	static [,2]byte dupltable = (
		(kjumpne, 		kjumpeq)
		(kjumplt, 		kjumpeq)
		(kjumple, 		kjumpeq)
		(kjumpge, 		kjumpeq)
		(kjumpgt, 		kjumpeq)

		(ksetne, 		kseteq)
		(ksetlt, 		kseteq)
		(ksetle, 		kseteq)
		(ksetge, 		kseteq)
		(ksetgt, 		kseteq)
!
		(kcallf,		kcallp)
		(kicallp,		kcallp)
		(kicallf,		kcallp)
		(kthreadedproc,	kprocdef)

!		(kdb,			kdq)
!		(kdw,			kdq)
!		(kdd,			kdq)
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
!doesn't need a handler, but used as default handler for all opcodes
!for which its pc-handler doesn't exist
	[300]char str

	print @str,"Unimplemented Opcode:",pclnames[p.opcode]
	mgencomment(str)
	println "Unimplemented Opcode:",pclnames[p.opcode], P.OPCODE, =KDB, =KDQ
end

global proc gerrorc(ichar mess, param=nil)=
	print "MCL Gen error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

proc px_zero*(pcl p)=
	unimpl(p)
end

proc px_nop*(pcl p)=
!	unimpl(p)
end

proc px_stop*(pcl p)=
	symbol d

	loadparam(xa,r10)

	d:=pcl_makesymbol("exit")
	d.isimport:=1
	genmc(m_call, mgenmemaddr(d))
	localshadow:=1
	highargs max:=1

	delopnd()
end

proc px_comment*(pcl p)=
	mgencomment(p.svalue)
end

proc px_istatic*(pcl p)=
	setsegment('I',p.align)
	genmc(m_labelname,mgenmemaddr(p.def))
end

proc px_zstatic*(pcl p)=
	symbol d

	d:=p.def
	setsegment('Z',p.align)
	genmc(m_labelname,mgenmemaddr(d))

	genmc(m_resb,mgenint(p.psize))
end

proc px_procdef*(pcl p)=
!Things that are remembered:

!PCLPROCDEF:	PCL op for kprocdef: used to repeat PASS2 pass for optimising
!				Note will normally skip back to following op, as below is for PASS1 only

!MCLPROCENTRY:	MCL op for dummy op (or anything that will work), used to insert
!				proc entry ops during do_procentry()

	passno:=1

	pclprocdef:=currpcl
	currproc:=currpcl.def

	setsegment('C',1)

	mgencomment("DUMMY")

	genmc(m_procstart,mgenmemaddr(currproc))
	genmc(m_labelname,mgenmemaddr(currproc))

	initpass1(currpcl)

!create dummy mcl op at which to insert hang proc-entry code onto later
	mgencomment(">>")
	mclprocentry:=mccodex

end

proc px_retproc*(pcl p)=
	int offset

	if passno=1 then
		do_procentry1(p)
		do_procexit1()
	else
		do_procentry2(p)
		do_procexit2()
	fi

end

proc px_endproc*(pcl p)=

	if passno=1 then
		do_endproc1(p)
	else
		do_endproc2(p)
	fi

!	mgeninfos("High reg:  ",getregname(highreg))
!	mgeninfos("High xreg: ",getxregname(highxreg))
!	mgeninfo("Bspill: ",	bspill)
!	mgeninfo("Bxspill: ",	bxspill)
!	mgeninfo ("Calls:     ",nproccalls)
!	mgeninfos("Leaf func: ",(leafproc|"Yes"|"No"))
!	mgeninfos("Local shadow: ",(localshadow|"Yes"|"No"))
!	mgeninfo ("Max args:  ",highargs)
end

proc px_endprog*(pcl p)=
	unimpl(p)
end

proc px_label*(pcl p)=
	genmc(m_labelx, mgenlabel(p.labelno))
end

proc px_load*(pcl p)=
	case p.opndtype
	when mem_opnd then
		addmem(p)

	when memaddr_opnd then
		addmemaddr(p.def)
	when int_opnd then
		addimm(p.value)
	when real_opnd then
!		addimmx64(p.xvalue)
		addimmx64(p.r64index)
	when real32_opnd then
!		addimmx32(p.xvalue32)
		addimmx32(p.r32index)
	when string_opnd then
!		addstr(p.svalue)
		addstr(p.strindex)
	else
		merror("Load",opndnames[p.opndtype])
	esac
end

proc px_store*(pcl p)=
	mcloperand ax,bx


!	checkallloaded()

	case p.opndtype
	when mem_opnd then
		case p.pcat
		when intcat then
			genmc(m_mov, mgenmem(p.def), loadopnd(xa, p.psize))

		when realcat then
!			genmc(m_movd+p.pwide, mgenmem(p.def), loadopnd(xa, p.psize))
!			genmc(m_movd+p.pwide, mgenmem(p.def, p.psize), loadopnd(xa, p.psize))
			genmc(m_movd+p.pwide, mgenmem(p.def, (p.psize=8|tr64|tr32)), loadopnd(xa, p.psize))

		when blockcat then
			bx:=getopnd_ind()
			addmemaddr(p.def)
			ax:=getopnd_ind()

			copyblock(ax,bx,p.psize)

			delopnd()
		esac
	else
		merroropnd("POP",p.opndtype)
	esac

	delopnd()
end

proc px_iloadx*(pcl p)=
	mcloperand ax,bx,cx,fx

	cx:=do_addrmode(p)

	if pclstack[xb].loc<>reg_loc then
		pclstack[xb].loc:=reg_loc			!need to prepare it for result
		pclstack[xb].reg:=getnextreg()		!(although wasted for floats)
		pclstack[xb].cat:=intcat
	fi
	ax:=getopnd(xb)

!here, ax is a suitable dest reg (not used for float dest), cx is the access mode

	case p.pcat
	when intcat then
		if p.psize=8 then
			genmc(m_mov, ax, cx)
		else
			cx.size:=p.psize
			genmc((p.psigned|m_movsx|m_movzx), ax, cx)
		fi

	when realcat then
!need to turn ax into a float reg
		addxreg(p.pwide)
		swapopnds(xc, ya)
		fx:=getopnd(xc)

		genmc(m_movd+p.pwide, fx, cx)
		delopnd()

	when blockcat then
		genmc(m_lea, ax, cx)
	esac	

	delopnd()
end

proc px_istorex*(pcl p)=
	mcloperand ax,bx,cx,px
!CPL "ISTOREX",=PASSNO

	px:=do_addrmode(p)

!CPL =MC_LIBMCL.STROPND(PX)

	cx:=loadopnd(xc)

	case p.pcat
	when intcat then
!		genmc(m_mov, px,cx)
		px.size:=p.psize
		genmc(m_mov, px,changeopndsize(cx,p.psize))

	when realcat then
		genmc(m_movd+p.pwide, px,cx)

	when blockcat then
		copyblock(px,makeopndind(cx),p.psize)

	esac	

	delopnd()
	delopnd()
	delopnd()
end

proc px_iloadc*(pcl p)=
	mcloperand ax,px,cx,fx,bx

	if isregvaropnd(xa) and p.pcat<>blockcat then
		cx:=mgenireg(pclstack[xa].reg)
		ax:=makeregopnd(xa)
	elsif pclstack[xa].loc=memaddr_loc then
		cx:=mgenmem(pclvals[xa].def)
		ax:=makeregopnd(xa)
	else
		ax:=loadopnd()
		cx:=makeopndind(ax)
	fi

	case p.pcat
	when intcat then
		if p.psize=8 then
			genmc(m_mov, ax, cx)
		else
			genmc((p.psigned|m_movsx|m_movzx), ax, changeopndsize(cx,p.psize))
		fi

	when realcat then
		addxreg(p.pwide)
		swapopnds(xb,ya)
		fx:=getopnd(xb)

		genmc(m_movd+p.pwide, fx, changeopndsize(cx,p.psize))
		delopnd()

	when blockcat then		!nothing further needed

	esac	

end

proc px_istorec*(pcl p)=
	mcloperand ax,bx,cx,px
	int opc

	bx:=loadopnd(xb)

	if isregvaropnd(xa) and p.pcat<>blockcat then
		ax:=mgenireg(pclstack[xa].reg)
	else
		ax:=getopnd_ind(ya)
	fi

	case p.pcat
	when intcat then
		if p.psize=8 then
			genmc(m_mov, ax,bx)
		else
			genmc(m_mov, changeopndsize(ax,p.psize),changeopndsize(bx,p.psize))
		fi

	when realcat then
		genmc(m_movd+p.pwide, changeopndsize(ax,p.psize), bx)

	when blockcat then
		copyblock(ax,makeopndind(bx),p.psize)
	esac	

	delopnd()
	delopnd()
end

proc px_loadbit*(pcl p)=
	mcloperand ax
	int i

!	if pclstack[xa].loc<>immd64_loc then
!		merror("dotix i not imm")
!	fi

	ax:=loadopnd(xb)

	if pclstack[ya].loc=immd64_loc then
		i:=pclvals[ya].value

		if i then
			genmc(m_shr, ax, mgenint(i))
		fi
	else
		if r10used then
			genmc(m_push, mgenreg(r10))
		fi
		loadparam(ya, r10)
		genmc(m_shr, ax, mgenreg(r10,1))
		if r10used then
			genmc(m_pop, mgenreg(r10))
		fi
	fi

	genmc(m_andx, changeopndsize(ax,4), mgenint(1))

	delopnd()
end

proc px_storebit*(pcl p)=
	mcloperand ax,bx,cx,rx,mx, ix
	int i,size,cxfmt,rhs,axoffset

	if pclstack[xc].loc=immd64_loc then
		rhs:=pclvals[xc].value
		cx:=nil
	else
		cx:=loadopnd(xc)
	fi

	if pclstack[xa].loc<>immd64_loc then
!		if cx or r10used or r11used then merror("storebit not viable") fi
		if r10used or r11used then merror("storebit not viable") fi
		loadparam(xa, r10)								!cl has i
		genmc(m_mov, ix:=mgenreg(r11), mgenint(1))
		genmc(m_shl, ix, mgenreg(r10,1))				!ix has 1<<i

	else
		i:=pclvals[xa].value
		ix:=nil
	fi
	size:=p.psize

	axoffset:=xb

	addreg_d64()
	rx:=getopnd()
	addreg_d64()
	mx:=getopnd()

	ax:=getopnd_ind(xb-2,size:size)
	genmc((size=8|m_mov|m_movzx),rx,ax)

!	if ix then
!		genmc(m_notx, ix)					!ix has not (1<<i)
!		genmc(m_andx,rx,ix)
!	else
!		genmc(m_mov,mx,mgenint(inot(1<<i)))
!		genmc(m_andx,rx,mx)
!	fi
!
!	if cx then				!i will be const only
!		if ix then
!			genmc(m_shl, cx, mgenreg(r10,1))		!cl still has i
!		else
!			if i then genmc(m_shl, cx, mgenint(i)) fi
!		fi
!		genmc(m_orx, rx, cx)
!
!	elsif rhs<>0 then
!		if ix then
!			genmc(m_notx, ix)					!restore (1<<i)
!			genmc(m_orx, rx, ix)
!		else
!			genmc(m_orx, rx, mgenint(1<<i))
!		fi
!	fi

	if ix then
		genmc(m_notx, ix)					!ix has not (1<<i)
		genmc(m_andx,rx,ix)

		if cx then				!i will be const only
			genmc(m_shl, cx, mgenreg(r10,1))		!cl still has i
			genmc(m_orx, rx, cx)

		elsif rhs<>0 then
			genmc(m_notx, ix)					!restore (1<<i)
			genmc(m_orx, rx, ix)
		fi
	else
		genmc(m_mov,mx,mgenint(inot(1<<i)))
		genmc(m_andx,rx,mx)

		if cx then				!i will be const only
			if i then genmc(m_shl, cx, mgenint(i)) fi
			genmc(m_orx, rx, cx)

		elsif rhs<>0 then
			genmc(m_orx, rx, mgenint(1<<i))
		fi
	fi

	genmc(m_mov,ax,changeopndsize(rx,size))

	delopnd()			!mx
	delopnd()			!rx
	delopnd()			!bx/index
	delopnd()			!addr
	delopnd()		!value being stored
end

proc px_loadbf*(pcl p)=
	mcloperand ax,mx,mx4
	int i,j
	word mask

	if pclstack[yb].loc<>immd64_loc or pclstack[za].loc<>immd64_loc then
		merror("dotslice i/j not imm")
	fi

	ax:=loadopnd(xc)
	i:=pclvals[yb].value
	j:=pclvals[za].value

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
			mx:=makeregopnd(yb)
			genmc(m_mov, mx, mgenint(mask))
			genmc(m_andx, ax, mx)
		fi
	fi

	delopnd()
	delopnd()
end

proc px_storebf*(pcl p)=
	mcloperand ax,rx,mx,mx4,dx
	int i,j,size
	word mask

	if pclstack[yb].loc<>immd64_loc or pclstack[za].loc<>immd64_loc then
		merror("popdotslice i/j not imm")
	fi

	dx:=loadopnd(wd)

	size:=p.psize
	ax:=getopnd_ind(xc,size:size)

	i:=pclvals[yb].value
	j:=pclvals[za].value

	mx:=makeregopnd(yb)
	rx:=makeregopnd(za)

	loadtoreg(rx,ax,p.psize, p.psigned)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

	genmc(m_mov, mx, mgenint(mask))

	if i then
		genmc(m_shl, dx, mgenint(i))
	fi

	genmc(m_andx, rx, mx)
	genmc(m_orx, rx, dx)

	storefromreg(ax,rx,size)

	delopnd()			!j
	delopnd()			!i
	delopnd()			!A
	delopnd()		!x

end

proc px_unload*(pcl p)=
	delopnd()
end

proc px_eval*(pcl p)=
	loadopnd(xa)
	delopnd()
end

proc px_callp*(pcl p)=
	int nregargs:=min(p.nargs,4), slots, isptr:=0, shadow:=0

	if p.opcode in [kicallp, kicallf] then
		isptr:=1
	fi

	highargs max:=p.nargs

	do_pushlowargs(nregargs, p.nvariadics, isptr)

!++NALLCALLS

	slots:=0
	if p.nargs<=4 then
		if mstackdepth then
			slots+:=4
			pushslots(4)					!shadowspace
SLOTS+:=CALLALIGN[NCALLDEPTH]
!CPL "CALLA ARGS<=4 SLOTS",=SLOTS
!++NPUSHSS
!MGENCOMMENT("PUSHSS")
		else
			localshadow:=1
!IF CALLALIGN[NCALLDEPTH] THEN
!CPL "LOCALSHADOW",CALLALIGN[NCALLDEPTH]
!FI
!CPL "CALLB ARGS<=4 SLOTS",=SLOTS
!MGENCOMMENT("LOCALSS")
!++NLOCALSHADOW
		fi

	else
		slots:=p.nargs+callalign[ncalldepth]
		pushslots(4)						!shadowspace
!CPL "CALLC ARGS>4 SLOTS",=SLOTS
!++NPUSHSS
!MGENCOMMENT("PUSHSS")
	fi

	if isptr then
		genmc(m_call, loadopnd(xa))
		delopnd()
	else
		genmc(m_call, mgenmemaddr(p.def))
	fi

	to nregargs do
		delopnd()
	od

	if slots then
		popslots(slots)
	fi

!CPL "CALL2 SLOTS=",SLOTS
!CPL "CALL",P.PCAT,P.PMODE
	if p.pcat then
		dogetretvalue(p)
	fi

	--ncalldepth
end

proc px_retfn*(pcl p)=
	mcloperand ax,bx

	if p.pcat=blockcat then
		genmc(m_mov, mgenreg(r1), mgenmem(blockretname))
		ax:=mgenireg(r0)
		bx:=mgenireg(r1)
		regset[r0]:=1
		regset[r1]:=1
		copyblock(bx, ax, p.psize)

		regset[r0]:=0
		regset[r1]:=0
		genmc(m_mov, mgenreg(r0), mgenmem(blockretname))
	fi

	px_retproc(p)
end

proc px_jump*(pcl p)=
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

proc px_jumpptr*(pcl p)=
	genmc(m_jmp, getopnd(xa))
	delopnd()
end

proc px_jumpeq*(pcl p)=
	dojumpcc(p)
end

proc px_jumpt*(pcl p)=
	dojumptruefalse(p,nz_cond)
end

proc px_jumpf*(pcl p)=
	dojumptruefalse(p,z_cond)
end

proc px_seteq*(pcl p)=
	dosetcc(p)
end

proc px_casejumpeq*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when intcat then
		genmc(m_cmp, loadopnd(xb), getopnd(ya))
		genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
		delopnd()

	else
		merror("casejumpeq/notcat")
	esac
end

proc px_to*(pcl p)=
	pcl q
	mcloperand ax

	q:=currpcl:=p+1

	ax:=mgenmem(q.def)
	genmc(m_dec, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.labelno))
end

proc px_forup*(pcl p)=
	do_for(p, m_inc, m_add, le_cond)
end

proc px_fordown*(pcl p)=
	do_for(p, m_dec, m_sub, ge_cond)
end

proc px_iswap*(pcl p)=
	mcloperand ax,bx

	mcloperand px:=getopnd_ind(xb,p.psize)
	mcloperand qx:=getopnd_ind(ya,p.psize)

	ax:=mgenreg(getnextreg(),p.psize)
	bx:=mgenreg(getnextreg(),p.psize)

	case p.pcat
	when intcat then
		genmc(m_mov, ax, px)
		genmc(m_mov, bx, qx)
		genmc(m_mov, qx, ax)
		genmc(m_mov, px, bx)

	else
		merror("swap/not cat")
	esac

	freereg(ax.reg)
	freereg(bx.reg)

	delopnd()
	delopnd()
end

proc px_storesl*(pcl p)=
	mcloperand ax, bx

	addmemaddr(p.def)
	ax:=getopnd_ind()

	bx:=loadopnd(xc)
	genmc(m_mov, ax, bx)
	bx:=loadopnd(yb)
	genmc(m_mov, applyoffset(ax,8), bx)
	delopnd()
	delopnd()
	delopnd()
	if p.opcode=kstoresliced then
		addmemaddr(p.def)
	fi
end

proc px_storesld*(pcl p)=
	px_storesl(p)
end

proc px_switch*(pcl p)=
	int minlab, maxlab, jumplab, elselab, reg
	mcloperand ax, bx

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=loadopnd(xa)
	if minlab<>0 then
		genmc(m_sub,ax,mgenint(minlab))
	fi
	genmc(m_cmp,ax,mgenint(maxlab-minlab+1))
	genmc_cond(m_jmpcc,geu_cond,mgenlabel(elselab))

	if highmem then
		reg:=getnextreg()
		bx:=mgenreg(reg)
		genmc(m_mov, bx, mgenlabel(jumplab))
		genmc(m_jmp, mgenindex(ireg:ax.reg, areg:reg, scale:8))
		freereg(reg)
	else
		genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))
	fi

!	genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))

	delopnd()

	setsegment('I')
end

proc px_switchu*(pcl p)=
	int minlab, maxlab, jumplab, elselab, reg
	mcloperand ax, bx

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=loadopnd(xa)

	if highmem then
		reg:=getnextreg()
		bx:=mgenreg(reg)
		genmc(m_mov, bx, mgenlabel(jumplab))
		genmc(m_jmp, mgenindex(ireg:ax.reg, areg:reg, scale:8, offset:-minlab*8))

		freereg(reg)
	else
		genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab, offset:-minlab*8))
	fi

	delopnd()
end

proc px_swlabel*(pcl p)=
	genmc(m_dq, mgenlabel(p.labelno))
end

proc px_endsw*(pcl p)=
	setsegment('C')
end

proc px_clear*(pcl p)=
	mcloperand ax

	ax:=getopnd_ind()

	clearblock(ax,p.psize)
	delopnd()
end

proc do_data1248(pcl p)=
	mcloperand ax
	int opc

	case p.opndtype
	when int_opnd then
		ax:=mgenint(p.value)
	when real_opnd, realimm_opnd then
		ax:=mgenrealimm(p.xvalue,8)
	when real32_opnd then
		ax:=mgenrealimm(p.xvalue32,4)

	when string_opnd then
		ax:=mgenlabel(p.strindex)

	when memaddr_opnd then
		ax:=mgenmemaddr(p.def)
		ax.offset:=p.extra
	when label_opnd then
		ax:=mgenlabel(p.labelno)

	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

	case p.psize
	when 1 then opc:=m_db
	when 2 then opc:=m_dw
	when 4 then opc:=m_dd
	when 8 then opc:=m_dq
	else
		merror("DATA/not 1248")
	esac
!
	genmc(opc,ax)
end

proc px_data*(pcl p)=
	ref byte s
	ref u64 d
	int n,nqwords,nwords,r

	n:=p.psize
	return when n=0

!	nqwords:=(n-1)/32+1
!	nwords:=(n-1)/8+1
	nwords:=n/8

!	s:=p.svalue
!	to p.psize do
!		genmc(m_db, mgenint(s++^))
!	od

!	genstring(p.svalue, p.psize, 'B')

	d:=cast(p.svalue)
	to nwords do
		genmc(m_dq, mgenint(d++^))
	od

	r:=n-nwords*8
!CPL =N, =NWORDS, =R
	if r then
		genstring(cast(d), r, 'B')
	fi
	MGENCOMMENT("ENDDATA")

end

proc px_db*(pcl p)=
	p.pcat:=intcat
	p.psigned:=0
	p.psize:=1
	do_data1248(p)
end

proc px_dw*(pcl p)=
	p.pcat:=intcat
	p.psigned:=0
	p.psize:=2
	do_data1248(p)
end

proc px_dd*(pcl p)=
	p.pcat:=intcat
	p.psigned:=0
	p.psize:=4
	do_data1248(p)
end

proc px_dq*(pcl p)=
	p.pcat:=intcat
	p.psigned:=0
	p.psize:=8
	do_data1248(p)
end

!proc px_data*(pcl p)=
!	p.pcat:=intcat
!	p.psigned:=0
!!	p.psize:=
!	do_data1248(p)
!end

!proc px_dw*(pcl p)=
!	p.pcat:=intcat
!	p.psigned:=0
!	p.psize:=2
!	do_data1248(p)
!end
!
!proc px_dd*(pcl p)=
!	p.pcat:=intcat
!	p.psigned:=0
!	p.psize:=4
!	do_data1248(p)
!end
!
!proc px_dq*(pcl p)=
!	p.pcat:=intcat
!	p.psigned:=0
!	p.psize:=8
!	do_data1248(p)
!end

proc px_assem*(pcl p)=
	domcl_assem(p.asmcode)
	if p.pcat then
		dogetretvalue(p)
	fi
end

proc px_add*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when intcat then
		ax:=loadopnd(xb)
		bx:=getopnd(ya)
		genmc(m_add,ax,bx)

	when realcat then
		dobin_float(m_addss+p.pwide)

	esac
	delopnd()
end

proc px_sub*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when intcat then
		ax:=loadopnd(xb)
		if pclstack[xa].loc=immd64_loc and pclvals[xa].value=1 then
			genmc(m_dec, ax)
		else
			bx:=getopnd(ya)
			genmc(m_sub,ax,bx)
		fi
	when realcat then
		dobin_float(m_subss+p.pwide)

	esac
	delopnd()
end

proc px_mul*(pcl p)=
	mcloperand ax,bx

	case p.pcat
	when intcat then
		ax:=loadopnd(xb)
		if  pclstack[xa].loc=immd64_loc then
			mulimm(ax,pclvals[xa].value)
			delopnd()
			return
		fi

		bx:=getopnd(ya)
		genmc(m_imul2,ax,bx)

	when realcat then
		dobin_float(m_mulss+p.pwide)
	esac
	delopnd()
end

proc px_div*(pcl p)=
	dobin_float(m_divss+p.pwide)
	delopnd()
end

proc px_idiv*(pcl p)=
	do_divrem(p, issigned:p.psigned, isdiv:1)
end

proc px_irem*(pcl p)=
	do_divrem(p, issigned:p.psigned, isdiv:0)
end

proc px_idivrem*(pcl p)=
	do_divrem(p, issigned:p.psigned, isdiv:2)
end

proc px_bitand*(pcl p)=
	dobitwise(p, m_andx)
end

proc px_bitor*(pcl p)=
	dobitwise(p, m_orx)
end

proc px_bitxor*(pcl p)=
	dobitwise(p, m_xorx)
end

proc px_shl*(pcl p)=
	do_shift(p,m_shl)
end

proc px_shr*(pcl p)=
	do_shift(p,(p.psigned|m_sar|m_shr))
end

proc px_min*(pcl p)=
	if p.pcat=intcat then
		domax_int((p.psigned|gt_cond|gtu_cond))
	else
		domax_float(m_minss+p.pwide)
	fi
end

proc px_max*(pcl p)=
	if p.pcat=intcat then
		domax_int((p.psigned|lt_cond|ltu_cond))
	else
		domax_float(m_maxss+p.pwide)
	fi
end

proc px_addpx*(pcl p)=
	mcloperand ax,cx

	cx:=do_addrmode(p)

	if pclstack[xb].loc<>reg_loc then
		pclstack[xb].loc:=reg_loc			!need to prepare it for result
		pclstack[xb].cat:=intcat
		pclstack[xb].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=getopnd(xb)

	genmc(m_lea, ax, cx)
	delopnd()
end

proc px_subp*(pcl p)=
	mcloperand ax,bx
	int n

	ax:=loadopnd(xb)
!	bx:=loadopnd(ya)
	bx:=getopnd(ya)
	genmc(m_sub,ax,bx)

	if p.scale>1 then
		n:=ispoweroftwo(p.scale)
		if n then
			genmc(m_shr, ax, mgenint(n))
		else
CPL P.SCALE
			MERROR("SUB/REF NOT POWER OF TWO")
		fi
	fi

	delopnd()
end

proc px_subpx*(pcl p)=
	int scale, extra, offset
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=loadopnd(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(m_sub, ax, mgenint(pclvals[xa].value*scale+extra))
	else
		bx:=loadopnd(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
			MERROR("SUBREF/EXTRA")
!			genmc(m_add, ax, mgenint(extra))
		fi
	fi
	delopnd()
end

proc px_neg*(pcl p)=
	mcloperand ax

	if p.pcat=intcat then
		ax:=loadopnd(xa)
		genmc(m_neg,ax)

	elsif p.pwide then
		if not labneg64 then labneg64:=mcreatefwdlabel() fi
		genmc(m_xorpd,loadopnd(xa),mgenlabelmem(labneg64))
	else
		if not labneg32 then labneg32:=mcreatefwdlabel() fi
		genmc(m_xorps,loadopnd(xa),mgenlabelmem(labneg32))
	fi
end

proc px_negto*(pcl p)=
	genmc(m_neg, getopnd_ind(ya,size:p.psize))
	delopnd()
end

proc px_bitnotto*(pcl p)=
	genmc(m_notx, getopnd_ind(ya,size:p.psize))
	delopnd()
end

proc px_notto*(pcl p)=
	genmc(m_xorx, getopnd_ind(ya,size:p.psize), mgenint(1))
	delopnd()
end

proc px_abs*(pcl p)=
	mcloperand ax,lx

	if p.pcat=intcat then
		ax:=loadopnd(xa)
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_labelx, lx)

	elsif p.pwide then
		if not lababs64 then lababs64:=mcreatefwdlabel() fi
		genmc(m_andpd,loadopnd(xa),mgenlabelmem(lababs64))
	else
		if not lababs32 then lababs32:=mcreatefwdlabel() fi
		genmc(m_andps,loadopnd(xa),mgenlabelmem(lababs32))
	fi
end

proc px_bitnot*(pcl p)=
	mcloperand ax
	ax:=loadopnd(xa)
	genmc(m_notx,ax)
end

proc px_not*(pcl p)=
	mcloperand ax
	ax:=loadopnd(xa)
	genmc(m_xorx,changeopndsize(ax,1),mgenint(1))
end

proc px_notnot*(pcl p)=
	mcloperand ax, bx

!PRINTLN "ISTRUE",CATNAMES[P.PCAT]
	if p.pcat=intcat then
		ax:=loadopnd(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_setcc, ne_cond, bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)
	else
		merror("istruel/real")
	fi
end

proc px_sign*(pcl p)=
!	mcloperand ax,bx

	if p.pcat=intcat then
		do_syscall(sf_sign_i64,1,intcat, p.psize)
	else
		do_syscall(sf_sign_r64,1,realcat, p.psize)
	fi
end

proc px_sqr*(pcl p)=
	mcloperand ax

	ax:=loadopnd(xa)

	if p.pcat=intcat then
		genmc(m_imul2,ax,ax)
	else
		genmc(m_mulss+p.pwide,ax,ax)
	fi
end

proc px_sqrt*(pcl p)=
	mcloperand ax

	ax:=loadopnd(xa)
	genmc(m_sqrtss+p.pwide,ax,ax)
end

proc px_sin*(pcl p)=
	domaths(p,"sin*")
end

proc px_cos*(pcl p)=
	domaths(p,"cos*")
end

proc px_tan*(pcl p)=
	domaths(p,"tan*")
end

proc px_asin*(pcl p)=
	domaths(p,"asin*")
end

proc px_acos*(pcl p)=
	domaths(p,"acos*")
end

proc px_atan*(pcl p)=
	domaths(p,"atan*")
end

proc px_log*(pcl p)=
	domaths(p,"log*")
end

proc px_log10*(pcl p)=
	domaths(p,"log10*")
end

proc px_exp*(pcl p)=
	domaths(p,"exp*")
end

proc px_round*(pcl p)=
	domaths(p,"round*")
end

proc px_floor*(pcl p)=
	domaths(p,"floor*")
end

proc px_ceil*(pcl p)=
	domaths(p,"ceil*")
end

proc px_atan2*(pcl p)=
	swapopnds(xb,ya)
	domaths(p, "atan2*",2)
end

proc px_power*(pcl p)=
	mcloperand ax,bx

	if p.pcat=intcat then
		swapopnds(xb,ya)
		do_syscall(sf_power_i64,2,intcat, p.psize)
	else
		swapopnds(xb,ya)
		domaths(p,"pow*",2)
	fi
end

proc px_fmod*(pcl p)=
	swapopnds(xb,ya)
	domaths(p, "fmod*",2)
end

proc px_incrto*(pcl p)=
	doincr(p,m_inc, m_add)
end

proc px_decrto*(pcl p)=
	doincr(p,m_dec, m_sub)
end

proc px_incrload*(pcl p)=
	doincrload(p,m_inc, m_add)
end

proc px_decrload*(pcl p)=
	doincrload(p,m_dec, m_sub)
end

proc px_loadincr*(pcl p)=
	doloadincr(p,m_inc, m_add)
end

proc px_loaddecr*(pcl p)=
	doloadincr(p,m_dec, m_sub)
end

proc px_addto*(pcl p)=
	if p.pcat=intcat then
		dobinto_int(p,m_add)
	else
		dobinto_float(p,m_addss,p.pwide)
	fi
end

proc px_subto*(pcl p)=
	if p.pcat=intcat then
		dobinto_int(p,m_sub)
	else
		dobinto_float(p,m_subss,p.pwide)
	fi
end

proc px_multo*(pcl p)=
	mcloperand ax,bx,cx
	int size:=p.psize

!		ax:=getopnd_ind(xb,size:size)
!		bx:=loadopnd(ya,size)

	if p.pcat=intcat then
		addreg_d64()
IF SIZE=1 THEN MERROR("MULTO/BYTE") FI

		ax:=getopnd_ind(xc,size:size)
		bx:=getopnd(yb, size:size)
		cx:=getopnd(za, size:=size)

		genmc(m_mov, cx,ax)

		if  pclstack[xb].loc=immd64_loc then
			mulimm(cx, pclvals[xb].value)
		else
			genmc(m_imul2, cx,bx)
		fi
		genmc(m_mov, ax,cx)

		delopnd()
		delopnd()
		delopnd()
	else
		dobinto_float(p,m_mulss, p.pwide)
	fi
end

proc px_idivto*(pcl p)=
	merror("IDIVTO not ready")
end

proc px_divto*(pcl p)=
	dobinto_float(p,m_divss, p.pwide)
end

proc px_bitandto*(pcl p)=
	dobinto_int(p,m_andx)
end

proc px_bitorto*(pcl p)=
	dobinto_int(p,m_orx)
end

proc px_bitxorto*(pcl p)=
	dobinto_int(p,m_xorx)
end

proc px_shlto*(pcl p)=
	if p.psize=8 then
		do_shiftnto(p,m_shl)
	else
		merror("shlto/short")
	fi
end

proc px_shrto*(pcl p)=
	if p.psize=8 then
		do_shiftnto(p,(p.psigned|m_sar|m_shr))
	else
		merror("shrto/sort")
	fi
end

proc px_minto*(pcl p)=
	if p.pcat=intcat then
		domaxto_int((p.psigned|le_cond|leu_cond),p.psize)
	else
		domaxto_real(leu_cond, p.pwide)
	fi
end

proc px_maxto*(pcl p)=
	if p.pcat=intcat then
		domaxto_int((p.psigned|ge_cond|geu_cond),p.psize)
	else
		domaxto_real(geu_cond, p.pwide)
	fi
end

proc px_addpxto*(pcl p)=
	int scale, extra,offset
!
	scale:=p.scale
	extra:=p.extra
	offset:=pclvals[xa].value*scale+extra	!in case imm_d64

	mcloperand ax,bx,rx
	int reg,size

	if ismemaddr(xb) then
		ax:=mgenmem(pclvals[xb].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)

		genmc(m_mov, rx, ax)

		if pclstack[xa].loc=immd64_loc then
			genmc(m_add,rx,mgenint(offset))
		else
			bx:=loadopnd(ya)
			mulimm(bx,scale)
			genmc(m_add,rx,bx)
		fi

		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=getopnd_ind(xb)
		if pclstack[xa].loc=immd64_loc then
			genmc(m_add,ax,mgenint(offset))
		else
			bx:=loadopnd(ya)
			mulimm(bx,scale)
			genmc(m_add,ax,bx)
		fi
	fi
	delopnd()
	delopnd()
end

proc px_subpxto*(pcl p)=
	int scale, extra
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=getopnd_ind(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(m_sub, ax, mgenint(pclvals[xa].value*scale+extra))
	else
		bx:=loadopnd(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
			MERROR("SUBTOREF/EXTRA")
!			genmc(m_sub, ax, mgenint(extra))
		fi
	fi

	delopnd()
	delopnd()
end

proc px_typepun*(pcl p)=
	mcloperand ax,bx,cx

	bx:=loadopnd(xa)

	case p.pcat
	when intcat then
		case pclstack[xa].loc
		when xreg_loc then
			addreg_d64()
			ax:=getopnd(xa)
			if p.psize=8 then
				genmc(m_movq,ax,bx)
				swapopnds(xb,ya)
				delopnd()
			else
				cx:=changeopndsize(ax,4)
				genmc(m_movd, cx,bx)
				swapopnds(xb,ya)
				delopnd()

				genmc((p.psigned|m_movsx|m_movzx),ax,cx)
			fi

		when reg_loc then
		else
			goto error
		esac

	when realcat then
		if not p.pwide then error fi
		case pclstack[xa].loc
		when reg_loc then
			addxreg(1)
			ax:=getopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(xb,ya)
			delopnd()
		else
			goto error
		esac

	else
error:
		merrorc("TYPEPUN",p.pcat)
	esac

	pclstack[xa].cat:=p.pcat
end

proc px_float*(pcl p)=
	mcloperand ax,fx
	int lab,lab2

!MERROR("FLOAT")

	ax:=loadopnd(xa)

	if p.oldsize<8 then merror("float/short") fi

	if p.oldsigned then
		addxreg(p.pwide)
		fx:=getopnd(xa,p.psize)
		genmc(m_cvtsi2ss+p.pwide, fx, changeopndsize(ax,p.psize))
		swapopnds(xb,ya)
	else												!to u64
		if not p.pwide then merror("float/u64/r32") fi

		addxreg(1)
		fx:=getopnd(xa)

		lab:=mcreatefwdlabel()
		lab2:=mcreatefwdlabel()
		genmc(m_cmp, ax, mgenint(0))
		genmc_cond(m_jmpcc, lt_cond, mgenlabel(lab))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_jmp, mgenlabel(lab2))

		mdefinefwdlabel(lab)
		if not labmask63 then
			labmask63:=++mlabelno
			laboffset64:=++mlabelno
		fi
		genmc(m_andx,ax, mgenlabelmem(labmask63))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_addsd, fx, mgenlabelmem(laboffset64))
		mdefinefwdlabel(lab2)
		swapopnds(xb,ya)
	fi

	delopnd()
end

proc px_fix*(pcl p)=
	mcloperand fx,ax
!
	fx:=loadopnd(xa)
	addreg_d64()
	ax:=getopnd(xa)
	genmc(m_cvttss2si+p.oldwide, ax, fx)
	swapopnds(xb,ya)
	delopnd()
end

proc px_truncate*(pcl p)=
	mcloperand ax
	int mask

	case p.oldsize
	when 1 then mask:=255
	when 2 then mask:=65535
	when 4 then mask:=0xFFFF'FFFF
	esac

	ax:=loadopnd(xa)
	genmc(m_andx, ax, mgenint(mask))

	genmc((p.oldsigned|m_movsx|m_movzx), ax, changeopndsize(ax,p.oldsize))
end

proc px_fwiden*(pcl p)=
	mcloperand fx
	fx:=loadopnd()
	genmc(m_cvtss2sd, fx,fx)
	pclstack[xa].loc:=xreg_loc
	pclstack[xa].cat:=realcat
	pclstack[xa].wide:=1
end

proc px_fnarrow*(pcl p)=
	mcloperand ax:=loadopnd(xa)
	genmc(m_cvtsd2ss, ax,ax)
!	pclstack[xa].fmt:=xreg_x32
	pclstack[xa].loc:=xreg_loc
	pclstack[xa].cat:=realcat
	pclstack[xa].wide:=0
end

proc px_len*(pcl p)=
	mcloperand ax

	if p.pcat=blockcat then			!assume slice
		ax:=getopnd()
!
!		genmc(m_mov, mgenreg(ax.reg), applyoffset(getopnd_ind(),8))
		genmc(m_mov, ax:=mgenreg(ax.reg), applyoffset(getopnd_ind(),8))
		pclstack[xa].loc:=reg_loc
		pclstack[xa].cat:=intcat
		pclstack[xa].signedx:=1

		if p.opcode=kupb and p.slicelwb<>1 then
			genmc(m_ADD, ax, mgenint(p.slicelwb-1))
		fi

!		genmc(m_movzx, mgenreg(ax.reg), applyoffset(getopnd_ind(),8,4))
!		pclstack[xa].loc:=reg_loc
!		pclstack[xa].cat:=intcat
!		pclstack[xa].size:=8
!		pclstack[xa].signedx:=1
!
	else
		merror("len?")
	fi
end

proc px_upb*(pcl p)=
	px_len(p)				!for slices, len/upb are the same
end

proc px_bounds*(pcl p)=
	merrorc("bounds",p.pcat)
end

proc px_sliceptr*(pcl p)=
	mcloperand ax

	ax:=getopnd()
	genmc(m_mov, mgenreg(ax.reg), getopnd_ind())
	pclstack[xa].loc:=reg_loc
	pclstack[xa].cat:=intcat
	pclstack[xa].signedx:=1

end

proc px_startmx*(pcl p)=
	saveopnds()
end

proc px_resetmx*(pcl p)=
	if pclstack[xa].cat=realcat then
		merror("RESETMULT/XREG")
	fi

	movetoreg(r0)

	if p.opcode=kresetmx then
		delopnd()
	fi
end

proc px_endmx*(pcl p)=
	px_resetmx(p)
end

proc px_setret*(pcl p)=
	do_setret(r0,r0)

	regset[r0]:=0
	xregset[r0]:=0
end

proc px_jumpret*(pcl p)=
	if p.pcat then
		do_setret(r0,r0)
	fi

	regset[r0]:=0
	xregset[r0]:=0

	px_jump(p)

end

proc px_setretmult*(pcl p)=
	int k,wide

	k:=0

	for i:=1 to p.nret do
		++k
		do_setret(multregs[k],multxregs[k])
	od

	for i:=1 to k do
		regset[multregs[i]]:=xregset[multxregs[i]]:=0
	od
end

proc px_setcall*(pcl p)=
	saveopnds()
!STATIC INT MAXSETCALL
!INT NN:=1
!
!IF (P+1).OPCODE=KSETCALL THEN
!	++NN
!	IF (P+2).OPCODE=KSETCALL THEN
!		++NN
!		IF (P+3).OPCODE=KSETCALL THEN
!			++NN
!		FI
!	FI
!FI
!IF NN>MAXSETCALL THEN
!	CPL "SETCALL*",NN
!	MAXSETCALL:=NN
!FI

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	fi

	++ncalldepth

	if p.nargs<=4 then
		callalign[ncalldepth]:=mstackdepth.odd
!CPL "ARGS<=4 ALIGN",CALLALIGN[NCALLDEPTH],=MSTACKDEPTH
	else
		callalign[ncalldepth]:=p.nargs.odd ixor mstackdepth.odd
!CPL "ARGS>4 ALIGN",CALLALIGN[NCALLDEPTH], =P.NARGS, =MSTACKDEPTH
	fi

	if callalign[ncalldepth] then
		pushslots(1)
	fi
end

proc px_setarg*(pcl p)=
	if p.x>4 then
		pushopnd(xa)
	fi
end

proc px_dupl*(pcl p)=
	if ncalldepth then
		duploperand()
	else
		++pclstack[noperands].count
	fi

end

proc px_swapstk*(pcl p)=
	swapopnds(xb,ya)
end

!proc px_copyblock*(pcl p)=
!	mcloperand ax, bx
!
!	ax:=getopnd_ind()				!old block
!	addmem(p)
!	bx:=getopnd_ind()				!new block
!
!	copyblock(bx,ax,p.psize,1)
!
!	swapopnds(xb,ya)
!
!	delopnd()
!end
!
proc px_getnfuns*(pcl p)=
	mcloperand bx
	dosetfntable()
	addlabel(lab_funcnprocs)
	pclstack[xa].ilabel:=1
end

proc px_getfname*(pcl p)=
	mcloperand ax, bx
	int lab, reg

	lab:=(p.opcode=kgetprocname|lab_funcnametable|lab_funcaddrtable)

	dosetfntable()
	case p.pcat
	when intcat then
		ax:=loadopnd(xa)
		reg:=getnextreg()
		bx:=mgenreg(reg)
		genmc(m_mov, bx, mgenlabel(lab))

		genmc(m_mov, ax, mgenindex(ireg:ax.reg, areg:reg, scale:8,offset:-8))
!		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcnametable))

		freereg(reg)

	else
		merror("gpn")
	esac
end

proc px_getfaddr*(pcl p)=
!	mcloperand ax

	px_getfname(p)

!	dosetfntable()
!	case p.pcat
!	when intcat then
!		ax:=loadopnd(xa)
!		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcaddrtable))
!	else
!		merrort("gpa",p.pmode)
!	esac
end

proc dobin_float(int opc)=
	mcloperand ax,bx

	ax:=loadopnd(xb)
	bx:=getopnd(ya)

	genmc(opc,ax,bx)
end

proc do_pushlowargs(int nargs, nvariadics=0, isptr=0)=
!nargs=0 to 4 /operands/, not using more than 4 slots (for 2 wides, nargs=2)
!load args to D10-13/X0-3
!does not do anything with the stack at all
! Params are categorised as follows:
! Variadic:
!   float:  load to both D and X registers
!   other:  load to D register only
! Normal/non-variadic:
!   float:  load to X register
!   other:  load to D register

	int ireg, xreg, j,k, nextireg, nextxreg

	if nargs=0 then return fi

	highargs max:=nargs

	nextireg:=r10
	nextxreg:=xr0

	k:=0
	for i:=noperands downto noperands-nargs+1 do
		++k						!counts params from 1
		j:=i-isptr
		ireg:=nextireg
		xreg:=nextxreg

		if pclstack[j].cat=realcat then
			unless nvariadics and k>=nvariadics then ireg:=0 end
		else
			xreg:=0
		fi

		if ireg then loadparam(j,ireg) fi
		if xreg then loadxparam(j,xreg) fi

		++nextireg
		++nextxreg
	od
end

proc do_for(pcl p, int incop, addop, cond)=
	pcl q,r
	mcloperand ax,bx,cx,dx,mx
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
		ax:=mgenreg(getnextreg())
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
	freereg(ax.reg)
!

	genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
end

proc dojumpcc(pcl p)=
	int cond, offset
	mcloperand ax,bx, lx

	offset:=p.opcode-kjumpeq
	cond:=ucondcodes[offset]
	lx:=mgenlabel(p.labelno)

	if p.pcat=blockcat then
		addimm(p.psize)
		swapopnds(1,3)
		domaths(nil, "memcmp*", 3)
		genmc(m_cmp, mgenreg(r0,4), mgenint(0))
		genmc_cond(m_jmpcc,cond, lx)

	else

		ax:=loadopnd(xb)
		bx:=getopnd(ya)

		if p.pcat=intcat then
			if p.psigned then
				cond:=scondcodes[offset]
			fi
			genmc(m_cmp,ax,bx)

		else
			genmc(m_comiss+p.pwide,ax,bx)
		fi

		genmc_cond(m_jmpcc,cond, lx)
		delopnd()
		unless p.popone then
			delopnd()
		end
	fi
end

proc dosetcc(pcl p)=
	int cond, offset,isfloat
	mcloperand ax,bx,cx
	ref pclstackrec pc

	ax:=loadopnd(xb)
	bx:=getopnd(ya)
	isfloat:=0
	offset:=p.opcode-kseteq			!0..5

	case p.pcat
	when intcat then
		if p.psigned then
			cond:=scondcodes[offset]
		else
			cond:=ucondcodes[offset]
		fi
		genmc(m_cmp,ax,bx)

	when realcat then
		cond:=ucondcodes[offset]
		genmc(m_comiss+p.pwide,ax,bx)
		isfloat:=1
	else
		merror("setcc/block")
	esac

	if isfloat then
		cx:=mgenreg(getnextreg(),1)
		pc:=&pclstack[xb]
		if pc.loc=xreg_loc then
			freexreg(pc.reg)
		fi
		pc.loc:=reg_loc
		pc.reg:=cx.reg
		pc.cat:=intcat

	else
		cx:=changeopndsize(ax,1)
	fi

	genmc_cond(m_setcc,cond, cx)
	genmc(m_movzx, changeopndsize(cx,4), cx)

	if isfloat then

		freereg(cx.reg)
	fi

	delopnd()
end
!
proc do_setretfloat(int destreg)=
	int currreg
	mcloperand ax,rx

	rx:=mgenxreg(destreg)

	ax:=loadopnd(xa)
	currreg:=ax.reg

	case pclstack[xa].loc
	when xreg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				merror("setretfloat/dest in use")
			else
				genmc(m_movq, rx, ax)
				xregset[destreg]:=1
			fi
		fi
	else
		merror("setretf?", locnames[pclstack[xa].loc])
	esac
	delopnd()		!assume next is a jump to return point

end

proc do_setret(int destreg,destxreg)=
!make sure top-of-stack is in nth register for multi-value return
!for normal returns, n will be 1
!nth value must be in d0/d1/d2, or x0/x1/x2
!Value might not be on the stack
!prior registers not available. Current value will not be in previous
!regs as they will have been moved out

	int currreg
	mcloperand ax,rx

	if pclstack[xa].cat=realcat then
		do_setretfloat(destxreg)
		return
	fi

	rx:=mgenreg(destreg)

	ax:=loadopnd(xa)
	currreg:=ax.reg

	case pclstack[xa].loc
	when reg_loc then
		if currreg<>destreg then
			if regset[destreg] then
				swapopndregs(destreg)
				genmc(m_xchg, rx, ax)
			else
				genmc(m_mov, rx, ax)
			fi
		fi
	when regvar_loc then
		genmc(m_mov, rx, ax)
		pclstack[xa].loc:=reg_loc
		pclstack[xa].reg:=destreg
		regset[destreg]:=1

	else
		PRINTLN =LOCNAMES[PCLSTACK[NOPERANDS].LOC]
		merror("setret?")
	esac
	delopnd()						!assume next is a jump to return point
	regset[destreg]:=1
	mccodex.regend[destreg]:=0			!d0 will not be freed
end

proc dogetretvalue(pcl p)=
	int reg,xreg,i,n
	[10]int cats, sizes

	if (p+1).opcode=ktype then
		n:=0
		while (++p).opcode=ktype do
			cats[++n]:=p.pcat
			sizes[n]:=p.psize
		od
		currpcl:=p-1

		for i:=n downto 1 do 
			if cats[i]=intcat and sizes[i]<8 then
				merror("Short/wide mulret type")
			fi

			dogetretvalue_n(multregs[i],multxregs[i], cats[i],sizes[i])
		od

	else
		dogetretvalue_n(r0,r0,p.pcat, p.psize)
		if p.pcat=intcat and p.psize<8 then
			genmc((p.psigned|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.psize))
		fi
	fi
end

proc dogetretvalue_n(int reg,xreg,cat,size)=

	case cat
	when intcat,blockcat then
		addreg0(reg)
	when realcat then
		addxreg0(xreg, size=8)
	esac
end

proc do_shift(pcl p, int opc)=
	mcloperand ax
	ax:=loadopnd(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(opc, ax, mgenint(pclvals[xa].value))
	else
		if r10used then
			genmc(m_push, mgenreg(r10))
		fi
		loadparam(reg:r10)
		genmc(opc,ax, mgenreg(r10,1))
		if r10used then
			genmc(m_pop, mgenreg(r10))
		fi
	fi
	delopnd()
end

proc mulimm(mcloperand ax, int n)=
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

proc dojumptruefalse(pcl p, int cond)=
	mcloperand ax

	case p.pcat
	when intcat then
		ax:=loadopnd(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
	else
		merror("jumptrue/false real?")
	esac
	delopnd()
end

proc dobitwise(pcl p, int opc)=
	mcloperand ax,bx

	ax:=loadopnd(xb)
	bx:=getopnd(ya)
!	bx:=loadopnd(ya)
	genmc(opc,ax,bx)

	delopnd()
end

function do_addrmode(pcl p)mcloperand px =
!Top two stack elements are an array (xb) and index (ya)
!Return a operand which provdes the address mode to access the element,
!for either reading or writing
!The address mode will use 0, 1 or 2 registers. The registers may be 1 or 2
!associated with the pcl operands, or may be regvars.
!If for reading, caller will need to make their own arrangements for a dest reg.
!When Xb has to be loaded into a register anyway, then the caller can make use
!of that

	mcloperand ax,bx
	int scale, extra,offset, reg,regix
	symbol d

	scale:=p.scale
	extra:=p.extra
	offset:=pclvals[xa].value*scale+extra	!for imm offset

!	m:=p.pmode

	px:=nil

	if isregvaropnd(xb) then
		if isregvaropnd(ya) then			!regvar/regvar
			reg:=pclstack[xa].reg
			regix:=scaleregvar(reg,scale,xa)
			px:=mgenindex(areg:pclstack[xb].reg,ireg:regix, offset:extra, scale:scale)

		elsif isimm64(ya) then			!regvar/imm
			px:=mgenindex(areg:pclstack[xb].reg, offset:offset)
		else							!regvar/any
			scale:=scaleindex(bx:=loadopnd(ya),scale)
			px:=mgenindex(areg:pclstack[xb].reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	elsif ismemaddr(xb) then
		d:=pclvals[xb].def
		if d.nameid=staticid and highmem=2 then skip fi
		if isregvaropnd(ya) then			!memaddr/regvar
			reg:=pclstack[xa].reg
			regix:=scaleregvar(reg,scale,xa)
			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)

		elsif isimm64(ya) then			!memaddr/imm
			px:=mgenindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=loadopnd(ya),scale)
			px:=mgenindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
skip:
		ax:=loadopnd(xb)

		if isregvaropnd(ya) then			!any/regvar
			reg:=pclstack[xa].reg
			regix:=scaleregvar(reg,scale,xa)
			px:=mgenindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)
		elsif isimm64(ya) then			!any/imm
			px:=mgenindex(areg:ax.reg, offset:offset)
		else
			scale:=scaleindex(bx:=loadopnd(ya),scale)
			px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)

		fi
	fi

	if px.size=0 and p.psize in [1,2,4,8] then px.size:=p.psize fi
!CPL "END AM2:",=MC_LIBMCL.STROPND(PX)
	return px
end

function scaleregvar(int reg, &scale, n)int=
!When scale is 1/2/3/4, return reg (a regvar) and scale unchanged;
!otherwise set up a new register for operand n
!Copy reg to it, and scale. Return new reg, and set scale to 1
	int regix
	mcloperand ax

	if scale in [1,2,4,8] then return reg fi

	regix:=getnextreg()
	ax:=mgenreg(regix)

	IF SCALE=16 THEN
		genmc(m_lea, ax, mgenindex(ireg:reg, areg:reg, scale:1))
!		mulimm(ax,2)
		scale:=8


	ELSE
		genmc(m_mov,ax, mgenreg(reg))
		mulimm(ax,scale)
		scale:=1
	FI

	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=regix
	pclstack[n].cat:=intcat

	return regix
end

function scaleindex(mcloperand ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale fi

!	IF SCALE=16 THEN
!		MULIMM(AX,2)
!		RETURN 8
!	FI


	mulimm(ax,scale)
	return 1
end

function makeregopnd(int n)mcloperand ax=
!turn given pcl operand, which does not occupy a register,
!make it into register operand. Note that other characteristics, such
!as value/def for imm/mem/memaddr, are not affected
!offset = xa, yb etc

	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=getnextreg()

	return getopnd(n)
end

proc dobinto_int(pcl p, int opc)=
	mcloperand ax,bx,rx
	int reg,size

	size:=p.psize

	if size=8 and ismemaddr(xb) then
		ax:=mgenmem(pclvals[xb].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)
		genmc(m_mov, rx, ax)
		bx:=getopnd(ya)
		genmc(opc,rx,bx)
		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=getopnd_ind(xb,size:size)
		bx:=loadopnd(ya,size)

		genmc(opc,ax,bx)
	fi
	delopnd()
	delopnd()
end

proc dobinto_float(pcl p, int opc, wide)=
	mcloperand ax,bx,cx

	addxreg(wide)
	ax:=getopnd_ind(xc)
	bx:=getopnd(yb)
	cx:=getopnd(za)

	genmc(m_movd+wide, cx,ax)
	genmc(opc+wide, cx,bx)
	genmc(m_movd+wide, ax,cx)

	delopnd()
	delopnd()
	delopnd()
end

function reversemcond(int cond)int=

	case cond
	when z_cond then return nz_cond
	when nz_cond then return nz_cond

	when lt_cond then return ge_cond
	when le_cond then return gt_cond
	when ge_cond then return lt_cond
	when gt_cond then return le_cond

	when ltu_cond then return geu_cond
	when leu_cond then return gtu_cond
	when geu_cond then return ltu_cond
	when gtu_cond then return geu_cond
	esac
	return 0
end

proc do_divrem(pcl p, int issigned, isdiv)=
!isdiv = 0/1/2 = rem/div/divrem
	mcloperand bx
	int opc, n, shifts

	checkloaded(xb)

	if isdiv and pclstack[xa].loc=immd64_loc then
		n:=pclvals[xa].value
		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			delopnd()
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts then
				genmc((issigned|m_sar|m_shr), getopnd(xb), mgenint(shifts))
				delopnd()
				return
			fi
		esac
	fi 

	checkloaded(ya)
	saverdx()
	fixdivopnds()

	if issigned then
		genmc(m_cqo)
		opc:=m_idiv
	else
		genmc(m_xorx, mgenreg(r11),mgenreg(r11))
		opc:=m_div
	fi

	genmc(opc, bx:=getopnd(ya,p.psize))

	case isdiv
	when 0 then				!rem
		genmc(m_xchg,mgenreg(r0),mgenreg(r11))
	when 2 then				!divrem
		genmc(m_xchg, bx,mgenreg(r11))			!rem replace y-operand
		swapopndregs(r1)						!make sure it is in r1
		swapopnds(xb,ya)

	esac
	restorerdx()

	if isdiv<>2 then
		delopnd()
	fi

end

proc fixdivopnds=
!two div operands exist as the top two operands, which will be
!in registers
!the div op requires that x is in d0, and y in any other register
!d11 also needs to be free, which will be the case is reg allocs only
!go up to d9, and d10/d11/12/13 are in use for win64 parameter passing
	int regx,regy,zop

	regx:=pclstack[xb].reg
	regy:=pclstack[xa].reg

	if regx=r0 then			!regy will be OK
		return
	fi
	if regy=r0 then			!need to swap then
		genmc(m_xchg,getopnd(xb),getopnd(ya))
		swapopnds(xb,ya)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset[r0]=0 then	!d0 not in use
		genmc(m_xchg,mgenreg(r0),getopnd(xb))
		regset[regx]:=0
		pclstack[xb].reg:=r0
		regset[r0]:=1
		return
	fi

!need to move current occupier of r0
	for zop:=noperands downto 1 do
		if pclstack[zop].loc=reg_loc and pclstack[zop].reg=r0 then exit fi
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg,mgenreg(r0),getopnd(xb))	
	swap(pclstack[xb].reg,pclstack[zop].reg)		!switch registers

end

proc saverdx=
	if r11used then
		genmc(m_push, mgenreg(r11))
	fi
end

proc restorerdx=
	if r11used then
		genmc(m_pop, mgenreg(r11))
	fi
end

proc doincr(pcl p, int incrop, addop)=
!CPL =STRMODE(P.CAT)
	if p.stepx=1 then
		if ismemaddr(xa) then
			genmc(incrop, mgenmem(pclvals[xa].def))
		else
			genmc(incrop, getopnd_ind(xa,p.psize))
		fi
	else
		if ismemaddr(xa) then
			genmc(addop, mgenmem(pclvals[xa].def), mgenint(p.stepx))
		else
			genmc(addop, getopnd_ind(xa,p.psize), mgenint(p.stepx))
		fi
	fi
	delopnd()
end

proc doincrload(pcl p, int incrop, addop)=
	mcloperand ax, mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclvals[xa].def)
		ax:=makeregopnd(xa)
	else
		mx:=getopnd_ind(xa,p.psize)
		ax:=getopnd(xa)
	fi

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	if p.psize<=4 then
		genmc((p.psigned|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi
end

proc doloadincr(pcl p, int incrop, addop)=
	mcloperand ax,mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclvals[xa].def)
	else
		mx:=getopnd_ind(xa,p.psize)
	fi

	addreg_d64()
	ax:=getopnd()

	if p.psize<=4 then
		genmc((p.psize|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi

	if p.stepx=1 then
		genmc(incrop,mx)
	else
		genmc(addop,mx, mgenint(p.stepx))
	fi

	swapopnds(xb,ya)
	delopnd()
end

global proc do_syscall(int fnindex, nargs, retcat, retsize)=
!retcat = 0, intcat, realcat, x32cat, widecat
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
!++NPUSHSS
!MGENCOMMENT("PUSHSS")
	else
!++NLOCALSHADOW
!MGENCOMMENT("LOCALSS")
		localshadow:=1
	fi

	genmc(m_call, getsyscallname(fnindex))

	to nargs do
		delopnd()
	od

	if slots then
		popslots(slots)
	fi

	getretvalue_bycat(retcat, retsize)
end

proc getretvalue_bycat(int cat,size)=
	case cat
	when 0 then
		return
	when intcat then
		addreg0(r0)
	when realcat then
		addxreg0(r0, size=8)
	else
		merror("getval bycat")
	esac
end

proc px_loadref*(pcl p)=
	addlabel(p.labelno)
end

proc do_shiftnto(pcl p,int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	mcloperand px

	px:=getopnd_ind(xb)

	if pclstack[xa].loc=immd64_loc then
		genmc(opc, px, mgenint(pclvals[xa].value))
	else
		if r10used then merror("shiftto:cl in use") fi
		loadparam(xa,r10)
		genmc(opc, px, mgenreg(r10,1))
	fi

	delopnd()
	delopnd()
end

proc domax_float(int opc)=
	mcloperand ax,bx
	ax:=loadopnd(xb)
	bx:=getopnd(ya)
	genmc(opc,ax,bx)
	delopnd()
end

proc domax_int(int cond)=
	mcloperand ax,bx

	ax:=loadopnd(xb)
	bx:=loadopnd(ya)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	delopnd()
end

proc domaxto_real(int cond, wide)=
	mcloperand px,ax,bx,lx
	int lab

	px:=getopnd_ind(xb)
	bx:=loadopnd(ya)
	addxreg(wide)
	ax:=getopnd(xa)

	genmc(m_movd+wide, ax, px)

	genmc(m_comiss+wide, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movd+wide, px,bx)
	genmc(m_labelx, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_int(int cond, size)=
	mcloperand ax,bx,lx
	int lab

	if size<8 then merror("min/maxto size?") fi

	ax:=getopnd_ind(xb)
	bx:=loadopnd(ya)

	genmc(m_cmp, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_mov, ax,bx)
	genmc(m_labelx, lx)
	delopnd()
	delopnd()
end

proc dosetfntable=
	if lab_funcnametable=0 then
		lab_funcnametable:=++mlabelno
		lab_funcaddrtable:=++mlabelno
		lab_funcnprocs:=++mlabelno
	fi
end

proc loadtoreg(mcloperand rx, ax, int size, signedx)=
	if size=8 then
		genmc(m_mov, rx, ax)
	else
		genmc((signedx|m_movsx|m_movzx), rx, ax)
	fi
end

proc storefromreg(mcloperand ax, rx, int size)=
	genmc(m_mov, ax, changeopndsize(rx,size))
end

proc domaths(pcl p, ichar opname, int nargs=1)=
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

	genmc(m_call, mgenextname(opname))

	to nargs do
		delopnd()
	od

	if slots then
		popslots(slots)
	fi

	if p then dogetretvalue(p) fi
end

function getsyscallname(int fnindex)mcloperand=
	symbol d:=getsysfnhandler(fnindex)

	if d=nil then
		return mgenname(sysfnnames[fnindex]+3)
	else
		return mgenmemaddr(d)
	fi
end

!proc showgeninfo=
!	mgeninfos("High reg:  ",getregname(highreg))
!	mgeninfos("High xreg: ",getxregname(highxreg))
!	mgeninfo("Bspill: ",	bspill)
!	mgeninfo("Bxspill: ",	bxspill)
!	mgeninfo ("Calls:     ",nproccalls)
!	mgeninfos("Leaf func: ",(leafproc|"Yes"|"No"))
!	mgeninfos("Local shadow: ",(localshadow|"Yes"|"No"))
!	mgeninfo ("Max args:  ",highargs)
!end

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

proc spillparams=
	symbol d
	mcloperand ax
	int offset:=16, regoffset:=0

	regoffset:=0

	for i to nparams do
		if regoffset>3 then exit fi
		d:=paramdefs[i]
		if d.used  then
			if not d.reg then
				ax:=mgenindex(areg:rframe, size:8, offset:d.offset)
				case ttbasetype[d.mode]
				when tr64 then
					genmc(m_movq, ax, mgenxreg(regoffset+xr0))
				when tr32 then
					genmc(m_movd, changeopndsize(ax,4), mgenxreg(regoffset+xr0))
				else
					genmc(m_mov, ax, mgenreg(regoffset+r10))
				esac
			elsif d.reg and d.reg<=r9 then		!move from pregs to bregs
				case ttbasetype[d.mode]
				when tr64 then
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

proc do_procentry1(pcl p)=
	int retmode, ntemps, hasequiv, offset
	mcloperand ax
	symbol d
	[100]char str, newname

	initpass1x(pclprocdef)

	SETMCLENTRY(MCLPROCENTRY)

	bspill:=bxspill:=bxspilloffset:=0
	if highreg>=r3 then bspill:=highreg-r2 fi		!no of regs d3..highreg
	if highxreg>=r6 then bxspill:=highxreg-r5 fi	!no of xregs x6..highxreg

	for i to nparams do
		d:=paramdefs[i]
		if not d.reg then			!not a regvar
			d.offset:=paramoffset+16+bspill*8
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		fi
		paramoffset+:=8
	od

	retmode:=currproc.mode

	for i to nlocals do
		d:=localdefs[i]

		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(ttsize[d.mode])
			d.offset:=frameoffset
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		fi
	od

	ntemps:=0
	for i to maxoperands when pcltempflags[i] do
		++ntemps
		frameoffset-:=8
		ax:=pcltemps[i]
		ax.offset:=frameoffset
		genmc(m_define, mgenname(gettempname(currproc,i)), mgenint(ax.offset))
	od

	if bxspill then
		frameoffset-:=bxspill*8
		bxspilloffset:=frameoffset
	fi

	if currproc.isthreaded then
		if nlocals or nparams then merror("Threaded proc has locals/params") fi
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

	if framebytes or nparams then
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
!	mgencomment("... END PROC ENTRY CODE")
	RESETMCLENTRY()
end

proc do_procexit1=
	mcloperand ax
	int offset

	if currproc.isthreaded then
		genmc(m_ret)				!in case no jump out exists
		return
	fi

	if bxspill then
!		MGENCOMMENT("UNSPILL XREGS:")
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, mgenxreg(r), ax)
		od
	fi

	if framebytes or nparams then
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

proc do_endproc1(pcl p)=
!Pass 1 just completed

	genmc(m_procend)
	checkopnds()

	if fregoptim and not assemused then		!then Pass 2
		initpass2()							!Also sets up regvars

		currpcl:=pclprocdef					!will skip to following op

		mccodex:=mclprocentry				!redo mcl code (also proc entry injection point)
		mccodex.nextmcl:=nil
	elsif fpeephole then					!one pass only
		peephole(mclprocentry)
	fi
end

proc do_procentry2(pcl p)=
	int retmode, ntemps, hasequiv, offset
	mcloperand ax
	symbol d
	[100]char str, newname

	SETMCLENTRY(MCLPROCENTRY)

	bspill:=bxspill:=bxspilloffset:=0
	if highreg>=r3 then bspill:=highreg-r2 fi		!no of regs d3..highreg
	if highxreg>=r6 then bxspill:=highxreg-r5 fi	!no of xregs x6..highxreg

	for i to nparams do
		d:=paramdefs[i]
		if not d.reg then			!not a regvar
			d.offset:=paramoffset+16+bspill*8
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))

		elsif stdcat[getpclmode(d.mode)]=intcat then
			genmc(m_definereg, mgenname(getdispname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getdispname(d)), mgenxreg(d.reg))
		fi
		paramoffset+:=8
	od

	retmode:=currproc.mode

	for i to nlocals do
		d:=localdefs[i]
		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(ttsize[d.mode])
			d.offset:=frameoffset
			genmc(m_define, mgenname(getdispname(d)), mgenint(d.offset))
		elsif stdcat[getpclmode(d.mode)]=intcat then
			genmc(m_definereg, mgenname(getdispname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getdispname(d)), mgenxreg(d.reg))
		fi
	od
	ntemps:=0
	for i to maxoperands when pcltempflags[i] do
		++ntemps
		frameoffset-:=8
		ax:=pcltemps[i]
		ax.offset:=frameoffset
		genmc(m_define, mgenname(gettempname(currproc,i)), mgenint(ax.offset))
	od

	if bxspill then
		frameoffset-:=bxspill*8
		bxspilloffset:=frameoffset
	fi

	if currproc.isthreaded then
		if nlocals or nparams then merror("Threaded proc has locals/params") fi
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

	if framebytes or nparams then
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
		pushstack(framebytes)
	fi

	spillparams()

	if bxspill then
!		MGENCOMMENT("SPILL XREGS:")
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, ax, mgenxreg(r))
		od
	fi

!	mgencomment("... END PROC ENTRY CODE")
	RESETMCLENTRY()
end

proc do_procexit2=
	mcloperand ax
	int offset

	if currproc.isthreaded then
		genmc(m_ret)				!in case no jump out exists
		return
	fi

	if bxspill then
!		MGENCOMMENT("UNSPILL XREGS:")
		offset:=bxspilloffset
		for r:=xr6 to highxreg do
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
			offset+:=8
			genmc(m_movq, mgenxreg(r), ax)
		od
	fi

	if framebytes or nparams then
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

proc do_endproc2(pcl p)=
!Pass 2 just completed

!	showgeninfo()

	genmc(m_procend)
	checkopnds()

	if fpeephole then
		peephole(mclprocentry)
	fi

	passno:=1
end

proc px_loadall*(pcl p)=
	checkallloaded()
end
=== mc_genss.m 0 0 23/38 ===
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

mcloperand extraparam

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

global proc genss=
	int index
	ref mclrec m

!CPL "////////////////GENSS"
	initlib(mlabelno)

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

end

proc doinstr(ref mclrec m,int index)=
	mcloperand a,b
	symbol d,e
	int x,offset,shortjmp,n

	buffercheck(currdata)

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=nowmask:=usesizeb:=0

	a:=m.a
	b:=m.b

	++instrno
	alineno:=m.seqno
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
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)

			if d.scope=export_scope then
				getstindex(d)
			fi

			dofwdrefs(d)
		esac

	when m_labelx then
		d:=labeldeftable[a.labelno]

		d.reftype:=back_ref
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

	when m_segment then
		switchseg(a.value)

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
		println "*** Can't do opcode",mclnames[m.opcode],"line",alineno,=M.OPCODE,=M_HALT
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

proc genopnd(mcloperand a,int size=0)=
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

proc addrelocitem(int reloctype, symbol d)=
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

function getstindex(symbol d)int=
!retrieve existing obj st index, or create new one
	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		fi
		d.stindex:=++ss_nsymbols
		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
			if d.isimport then
				d.segment:=code_seg
			fi
		fi

	fi
	return d.stindex
end

proc genrel32(mcloperand a)=
!used by call/longjmp/ddoffset
	symbol d

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
	else								!external symbol
		gendword(a.offset)		!this is probably just zero
		addrelocitem(rel32_rel,d)
	esac
end

function getdef(mcloperand a,int dneeded=0)symbol =
	symbol d

	if a.mode in [a_mem,a_imm] then
		case a.valtype
		when label_val then
			return labeldeftable[a.labelno]
		when def_val then
			d:=a.def
			if d.reftype=0 then
				if not d.isimport then
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

proc genabs32(mcloperand a)=
!absolute refs to labels
	symbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then

		gendword(d.offset+a.offset)
		addrelocitem(addr32_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
!CPL "NEW1",D.FWDREFS, D.NAME,D
		if d.nameid in [frameid, paramid] then
			gendword(d.offset+a.offset)
		else
			gendword(a.offset)
			addrelocitem(addr32_rel,d)
		fi

	else								!external symbol
		gendword(a.offset)					!this is probably just zero
		addrelocitem(addr32_rel,d)
	esac
end

proc genabs64(mcloperand a)=
!absolute refs to labels
	symbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then
		genqword(d.offset+a.offset)
		addrelocitem(addr64_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
		if d.nameid in [frameid, paramid] then
			genqword(d.offset+a.offset)
		else
			genqword(a.offset)
			addrelocitem(addr64_rel,d)
		fi

	else								!external symbol
		genqword(a.offset)				!this is probably just zero
		addrelocitem(addr64_rel,d)
	esac
end

function getrel32(symbol d,int offset)int=
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

proc dofwdrefs(symbol d)=
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

function isbytesized(int64 x)int=
	return -128<=x<=127
end

function isdwordsized(int64 x)int=
	return int32.min<=x<=int32.max
end

proc genamode(mcloperand a, amoderec am)=
!	int sib,mode,dispsize,offset
	symbol d
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

proc setopsize(mcloperand a)=
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

function getdispsize(mcloperand a, int32 &offset)int=
!look at imm/mem displacement, and return (0,1 or 4) and offset
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	symbol d

	d:=getdef(a)
	offset:=a.offset

	if d then
		if d.nameid in [frameid, paramid] then
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

proc checkhighreg(mcloperand a)=
	if a.mode=a_reg then
		rex ior:=ishighreg[a.reg]
	fi
end

proc do_loop(mcloperand a,int opc)=
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

proc do_jcxz(mcloperand a,int opsize)=
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

proc do_call(mcloperand a)=
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

proc do_jmp(mcloperand a,ref mclrec m)=
	int am, regcode, offset, shortjmp
	symbol d:=getdef(a)

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

proc do_cmovcc(int cond, mcloperand a,b)=
	if a.size<>b.size and b.size then
		gerror("Opnd size mismatch")
	fi
	if a.size=1 then gerror("cmov/byte") fi

	genrrm(0x0F'40+cond, a, b)
end

proc do_fmem(mcloperand a, int freal, code)=
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

proc genrel8(mcloperand a)=
!a is a known fwd reference, and expected to be <=127 bytes
	symbol d

	d:=getdef(a,1)

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
		genbyte(0)
	else								!external symbol
		axerror("genrel8")
	fi
end

function checkshortjump(ref mclrec m,symbol d)int=
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

function addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
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

proc do_popcnt(mcloperand a,b)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi

	f3override:=1
	genrrm(0x0F'B8, a, b)
end

proc do_bsf(mcloperand a,b, int opc)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi
	if a.size<>b.size then gerror("bsf size") fi

	genrrm(0x0F<<8+opc, a, b)
end

proc extendsymboltable=
	ref[]symbol oldsymboltable
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
		labeldeftable[i]:=pcm_allocnfz(strec.bytes)
		labeldeftable[i].labelno:=i
		fprint @&.str,"(L#)",i
		labeldeftable[i].name:=pcm_copyheapstring(&.str)
		labeldeftable[i].reftype:=fwd_ref
	od
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

proc genxrm(int opcode, code, mcloperand b)=
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

proc genrrm(int opcode, mcloperand a, b)=
!deal with /r instructions; rrm = reg,reg/mem
!opcode represents 1, 2 and maybe 3-byte(?) opcodes, with last in lsb place
!a is a register mcloperand, b is a register or memory mcloperand, always
!when data direction is the other way, as in mov reg/mem, reg, then reverse mcloperands
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

func genrm(int reg, opc, mcloperand b, int isxreg=0)amoderec=
!reg =  0:	opc is a 3-bit code that goes in reg field of mod-reg-rm
!reg >= r0:	reg is an M-register code, which is converted to a machine reg encoding
!			of 3 bits (to go in middle of modrm byte), and may set rex.r bit for high
!			regs; opc is 0 in this case
!
!b is mcloperand containing rhs reg value, or is mem mcloperand using base/index regs and addr
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
			if b.def.nameid=staticid then ismem:=2 fi
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
		gerror("genrm not mem")
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
		if regix=rstack then gerror("Scaled rstack?") fi

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
			if regix=rstack and scale>1 then gerror("Can't scale rstack") fi
		fi
	fi

	if scale then
		am.sib:=scaletable[scale]<<6 + index<<3 + base
		am.usesib:=1
	fi

	if am.dispsize=4 and ismem then
		if reg or regix then
			if highmem=2 AND ISMEM=2 then
				CPL "Addr32 can't use RIP, line",alineno,STRMCLSTR(CURRMCL)
!				gerror("Addr32 can't use RIP")
			fi
		elsif highmem then
			am.usesib:=-1
			mode:=0
			rm:=5
		fi
	fi

	am.modrm:=makemodrm(mode, opc, rm)
	
	return am
end

proc do_arith(mcloperand a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
	symbol d
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
	cpl opndnames_ma[code]
		gerror("Can't add to this opnd")
	esac
end

proc do_mov(mcloperand a,b)=
	int regcode, opc, dispsize
	int64 value
	symbol d:=getdef(b)

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
			if d and a.size<=2 then gerror("mov imm?") fi

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
				if d then
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
			if d and a.size<=2 then gerror("mov imm?") fi
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
			gerror("MOV MEM/?")
		esac
	else
		gerror("MOV ?/..")
	esac
end

proc do_push(mcloperand a)=
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
			gerror("push imm value too large")
		fi

	when a_mem then
		if a.size<>8 then gerror("push not 64-bit") fi
		genxrm(0xFF, 6, a)

	else
		gerror("push opnd?")
	esac
end

proc do_pop(mcloperand a)=
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

proc do_inc(mcloperand a,int code)=
!inc/dec

	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xFE|0xFF), code, a)
	else
		gerror("inc/opnd?")
	esac
end

proc do_neg(mcloperand a,int code)=
!neg/not/mul/imul/div/idiv
	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xF6|0xF7), code, a)
	else
		gerror("neg/div/etc opnd?")
	esac
end

proc do_lea(mcloperand a,b)=
	int regcode, am

	unless a.mode=a_reg and b.mode=a_mem then
		gerror("LEA not reg/mem")
	end

	if a.size<4 then gerror("LEA size error") fi

	genrrm(0x8D, a, b)
end

proc do_movsx(mcloperand a,b,int opc)=
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

proc do_exch(mcloperand a,b)=
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

proc do_movsxd(mcloperand a,b)=
	int regcode, am

	if b.mode=a_mem and b.size=0 then b.size:=4 fi

	if a.size<>8 or b.size>4 then gerror("movsxd size") fi

	if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
		gerror("movsxd opnds")
	fi

	genrrm(0x63, a, b)
end

proc do_imul2(mcloperand a,b)=
	int regcode, am, opc, dispsize
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
		if getdef(b) then gerror("mul/label") fi
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

proc do_shift(mcloperand a,b,int code)=
	int w,opc,needdisp

	if a.mode<>a_reg and a.mode<>a_mem then gerror("shift opnds1?") fi
	if getdef(b) then gerror("shift/label") fi
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

proc do_test(mcloperand a,b)=
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

proc do_setcc(int cond, mcloperand b)=
!a is cond
!b is byte reg/mem

	if b.mode not in [a_reg, a_mem] or b.size>1 then gerror("setcc opnd/size") fi

	genxrm(0x0F'90+cond, 0, b)
end

proc checksize(mcloperand a, int size1=0, size2=0)=
	if a.size=0 then gerror("Need size") fi
	if size1 and a.size not in [size1,size2] then
		CPL =A.SIZE
		gerror("Wrong size")
	fi
end

proc do_arithxmm(mcloperand a,b,int prefix,opc)=
	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("arithxmm opnds")
	fi

	if prefix then genbyte(prefix) fi
	genrrm(0x0F<<8+opc, a, b)
end

proc do_logicxmm(mcloperand a,b,int opc,size)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("logicxmm opnds")
	fi

	if size=8 then genbyte(0x66) fi

	genrrm(0x0F<<8+opc, a, b)
end

proc do_convertfloat(mcloperand a,b,int prefix)=
!cvtss2sd and cvtsd2ss
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("convertfloat opnds")
	fi
	genbyte(prefix)
	nowmask:=1
	genrrm(0x0F'5A, a,b)
end

proc do_fix(mcloperand a,b,int prefix,opc)=
	int am, regcode

	if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
		gerror("fix opnds")
	fi

	checksize(a, 4, 8)
	
	b.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	genrrm(0x0F<<8+opc, a, b)
end

proc do_float(mcloperand a,b,int prefix)=
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

proc do_movxmm(mcloperand a,b,int size)=
!do movd/movq depending on size being 4 or 8
	int am, regcode, regcode1, regcode2

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
			if b.size=0 then b.size:=a.size fi
!			if b.size<>size then gerror("31:movdq size") fi
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
			if a.size and a.size<>size then gerror("5:movdq size") fi

			sizeoverride:=1
			genrrm((size=4|0x0F'7E|0x0F'D6), b,a)

		else
			gerror("movdq mem,?")
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
		gerror("fixrip disp?")
	esac
	ripentry.immsize:=dispsize
end

proc do_bswap(mcloperand a)=
	int code
	if a.mode<>a_reg or a.size<4 then gerror("bswap reg>") fi

	setopsize(a)

	code:=getregcode(a.reg, bmask)

	genrex()
	genbyte(0x0F)
	genbyte(0xC8 + code)
end

proc do_movdqx(mcloperand a, b, int prefix)=
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

proc do_dshift(mcloperand a, b, int c, opc)=

	if a.size=0 then a.size:=b.size fi
	if a.size<>b.size or a.size<=1 then gerror("dshift/size") fi

	sizeoverride:=0
	genrrm(0x0F<<8+opc, b, a)
	genbyte(c)
end

=== mc_libmcl.m 0 0 24/38 ===
const fasmformat=1
!const fasmformat=0

const fuseregtable=1
!const fuseregtable=0

const targetsize=8

global int fshowmsource=0

GLOBAL int mclseqno

[-1..10]mcloperand smallinttable

global macro isframex(d) = (d.nameid in [frameid, paramid])

global proc mclinit=
	mcloperand a
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

	dframeopnd:=mgenreg(rframe,8)
	dstackopnd:=mgenreg(rstack,8)

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

export proc genmc(int opcode, mcloperand a=nil,b=nil)=
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

	esac

	if mccode then
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi

end

global proc genmc_cond(int opcode, cond, mcloperand a=nil,b=nil)=
	genmc(opcode,a,b)
	mccodex.cond:=cond
end

global proc genmc_str(int opcode,ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode,mgenstring(s))
end

function newmclopnd:mcloperand a=
!	a:=pcm_allocz(mclopndrec.bytes)
	a:=pcm_allocnfz(mclopndrec.bytes)

!++NMCLOPND
	return a
end

global function duplopnd(mcloperand a)mcloperand=
	mcloperand b
!	b:=pcm_alloc(mclopndrec.bytes)
	b:=pcm_allocnfz(mclopndrec.bytes)
	b^:=a^
	return b
end

export function mgenxreg(int xreg,size=8)mcloperand=
	mcloperand a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
	return a
end

export function mgenindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, symbol def=nil)mcloperand=
!construct a mem address mode
	mcloperand a
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

global function getmclstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	symbol d,e
	ref mclrec m
	[32]char str2,str3
	int i

	gs_init(dest)

	for i to nlibfiles when libfiles[i]^<>'$' do
		asmstr("          ")
		asmstr("importdll ")
		asmstr(libfiles[i])
		gs_line(dest)
	od

	case highmem
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

	return dest
end

global function strmclstr(ref mclrec m)ichar=
	gs_init(dest)
	strmcl(m)
	return dest.strptr
end

global proc mgencomment(ichar s)=
!if not debugmode then return fi
	if s=nil or s^=0 then
		genmc(m_blank)
	else
		genmc_str(m_comment,s)
	fi
end

global function mgenstring(ichar s,int length=-1)mcloperand=
	mcloperand a
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

global function mgenname(ichar s)mcloperand=
	[64]char str
	mcloperand a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstring(s)
	a.valtype:=name_val
	a.size:=8

	return a
end

proc writemcl(int index,ref mclrec mcl)=

	case mcl.opcode
	when m_deleted then
	else
		strmcl(mcl)
		gs_line(dest)
	esac
end

global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mcloperand a,b
	int opcode,cond,sizepref
	ichar s,comment
	symbol d

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

		if d.scope=export_scope then
			asmstr("\n")
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

	when m_call then
		strcpy(&.opcname,"call")
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

!		ASMSTR("; ")
!		ASMSTR(strint(a.size))
!		ASMSTR(" ")
!		ASMSTR(strint(b.size))
!!		ASMSTR(" ")
!!		ASMSTR(STRINT(MCL.SEQNO))
!
	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0,opcode)
		else
			asmopnd(a,1,opcode)
		fi
	fi

	if mcl.c then
		asmstr(",")
		asmstr(strint(mcl.c))
	fi

!ASMSTR("	#"); ASMSTR(STRINT(MCL.SEQNO))

DOCOMMENTS:

end

global proc asmopnd(mcloperand a,int sizeprefix=0,opcode=0)=
	asmstr(stropnd(a,sizeprefix,opcode))
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

global function getsizeprefix(int size,enable=0)ichar=
	if not enable then return "" fi
	case size
	when 1 then return "byte "
	when 2 then return "word16 "
	when 4 then return "word32 "
	when 8 then return "word64 "
	esac
	return ""
end

global function needsizeprefix(int opcode,mcloperand a,b)int=
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

global function changeopndsize(mcloperand a,int size)mcloperand=
	mcloperand b

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

global function makeopndind(mcloperand a,int size=0)mcloperand=
	mcloperand b

	if a.mode<>a_reg then
		merror("makeopndind")
	fi

	return mgenireg(a.reg,size)
end

global function applyoffset(mcloperand a,int offset,int size=0)mcloperand=
!astr is an asm operand
!add possible byte offset
	mcloperand b

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

export function mgenint(int64 x,int size=8)mcloperand a=
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

export function mgenint0(int64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global function mgenrealmem(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_mem
	if size=8 then
		a.value:=getrealindex(x)
	else
		a.value:=getreal32index(x)
	fi
	a.valtype:=label_val
	a.size:=size
	return a
end

global function mgenrealimm(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=realimm_val
	a.size:=size
	return a
end

export function mgenlabel(int x=0)mcloperand a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm

	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val

	return a
end

global function mgenlabelmem(int x)mcloperand a=
!x is a label index
!generate immediate operand containing label

	a:=mgenlabel(x)
	a.mode:=a_mem
	return a
end

global function mgenregvar(symbol d)mcloperand a=
	a:=mgenreg(d.reg,8)
	isregvar[d.reg]:=1

	return a
end

global function mgenxregvar(symbol d, int size=8)mcloperand a=
	a:=mgenxreg(d.reg, size)
	isxregvar[d.reg]:=1

	return a
end

global function mgenmem(symbol d, int mode=0)mcloperand a=
	int reg

	if mode=0 then mode:=d.mode fi

	if d.reg then
		if ttisreal[mode] then
!CPL "REAL", STROPND(MGENXREGVAR(D))
			return mgenxregvar(d, ttsize[mode])
		else
!CPL "INT", STROPND(MGENREGVAR(D))
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

	a.size:=min(ttsize[mode],8)

	return a
end

export function mgenmemaddr(symbol d)mcloperand=
	mcloperand a

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

export function mgenreg(int reg,size=8)mcloperand=
	if fuseregtable then
		return regtable[reg,size]
	fi
	return mgenreg0(reg,size)
end

global function mgenreg0(int reg,size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size
	return a
end

global function mgenireg(int reg,size=8,offset=0)mcloperand=
	mcloperand a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=size
	a.offset:=offset

	return a
end

global func mgentemp(int n)mcloperand a=
	int reg, size

	if pcltempflags[n] then			!already in use
		return pcltemps[n]
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=rframe
	a.valtype:=temp_val
	a.size:=8
	a.tempno:=n

	pcltemps[n]:=a
	pcltempflags[n]:=1

	return a
end

global function roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	if size iand 7=0 then return size fi
	return size+(8-(size iand 7))
end

global function getregname(int reg,size=8)ichar=
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
	when rframe then rs:="frame"
	when rstack then rs:="stack"
	else
		getstrint(reg-r0,&.str2)
		rs:=&.str2
	esac

	print @&.str,prefix[size2],,rs
	return &.str
end

global function getxregname(int reg,size=8)ichar=
	static [32]char str

	if reg=rnone then return "-" fi

	if fasmformat then
		print @&.str,"XMM",,reg-xr0
	else
		print @&.str,(size=8|"DX"|"SX"),,reg-xr0
	fi
	return &.str
end

global function sameoperand(mcloperand a,b)int=
	return memcmp(a,b,mclopndrec.bytes)=0
end

global function sameregopnd(mcloperand a,b)int=
!check if same register operand
	unless a.mode=b.mode=a_reg then return 0 end
	return a.reg=b.reg
end

!global function getstringindex(ichar s)int=
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
!global function addconst(ref constrec &clist, int value)int=
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
!global function getrealindex(real x)int=
!	return addconst(creallist,cast@(x,int))
!end
!
!global function getreal32index(real x)int=
!	return addconst(creal32list,cast@(x,int))
!end

proc asmstr(ichar s)=
	gs_str(dest,s)
end

proc asmchar(int c)=
	gs_char(dest,c)
end

global function getdispname(symbol d)ichar=
	static [256]char str

	if fshortnames then
		return d.name
	fi
	ichar name:=getfullname(d)

	if d.reg then
!		fprint @str,"#.#",(d.isfloat|"X"|"R"), name
		fprint @str,"#.#","R", name
		return str
	fi

	if d.truename and d.isimport then
		strcpy(str,"`")
		strcat(str,d.truename)
		strcat(str,"*")
	elsif d.isimport then
		strcpy(str,name)
		strcat(str,"*")
	else		
		return name
	fi

	return str

end 

global function gettempname(symbol d, int n)ichar=
	static [128]char str

	fprint @str,"#.$P#",getdispname(d),n
	str
end

export proc merror(ichar mess,ichar param="")=
	fprintln "MCL Error: # (#) on Line: # in #",mess,param,
!	fprintln "MCL Error: # (#) [#]",mess,param,
		getlineno(mlineno), sources[getfileno(mlineno)].name
!	PRINTLN
!	STOP 1
	stopcompiler(sources[getfileno(mlineno)].filespec,getlineno(mlineno))
end

export proc merrorc(ichar mess,int cat)=
	[300]char str
	fprint @str, "MCL Cat not supported for (#)",mess
	merror(str, catnames[cat])
!	fprintln "MCL Type not supported: # (#) [#]",mess,ttname[t]
!	PRINTLN
!	STOP 1
!	stopcompiler(sourcefilepaths[getfilenpmlineno>>24],mlineno iand 16777215)
end

global proc merroropnd(ichar mess,int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
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
		genstring(p.svalue,length:-1, strtype:0)
	od
end

global proc genstring(ichar s, int length=-1, strtype)=
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
			genmc(m_dq, mgenrealimm(p.xvalue,8))
		fi
	od

	mgencomment("Real32 Table")
	p:=creal32list
	while p, p:=p.nextconst do
		genmc(m_labelx,mgenlabel(p.labelno))
		if p.xvalue=infinity then
			genmc(m_dd, mgenint(int@(real32(p.xvalue))))
		else
			genmc(m_dd, mgenrealimm(p.xvalue,4))
		fi

	od
end

global proc genabsneg=
	setsegment('I',16)

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

global function mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_labelx,mgenlabel(lab))
end

global function stropnd(mcloperand a,int sizeprefix=0,opcode=0)ichar=
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
!RETURN "<REG>"
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
			plus:="+"
		fi
		if a.regix then
			strcat(&.str,plus)
			strcat(&.str,strreg(a.regix,8))
			plus:="+"

			if a.scale>1 then
				strcat(&.str,"*")
				strcat(&.str,strint(a.scale))
			fi
		fi

		if a.valtype in [def_val,label_val, temp_val] then
			if plus^='+' then
				strcat(&.str,plus)
			fi
			strcat(&.str,strvalue(a))
	    elsif offset:=a.offset then
			print @&.str2,offset:"+"
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

function strreg(int reg, size=8)ichar=
	symbol d

	d:=checkregvar(reg,0)

	if size=8 and d then
		return getdispname(d)
	else
		getregname(reg,size)
	fi
end

function checkregvar(int reg, isfloat)symbol d=
	RETURN NIL
end

function strxreg(int reg, size=8)ichar=
	symbol d

	d:=checkregvar(reg,1)

	if size=8 and d then
		return getdispname(d)
	else
		return getxregname(reg,size)
	fi
end

global function strvalue(mcloperand a)ichar=
	static [512]char str
	[128]char str2
	symbol def
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

	when label_val then
		strcat(&.str,"L")
		strcat(&.str,strint(a.labelno))
		goto addoffset

	when temp_val then
		return gettempname(currasmproc,a.tempno)

	else
		merror("Stropnd?")
	esac

	return &.str

end

global function ismemaddr(int n)int=
	if pclstack[n].loc=memaddr_loc then return 1 fi
	return 0
end

global function isimm64(int n)int=
	if pclstack[n].loc=immd64_loc then return 1 fi
	return 0
end

global function isregvaropnd(int n)int=
	if pclstack[n].loc=regvar_loc then return 1 fi
	return 0
end

global proc copyblock(mcloperand ax,bx, int n, savedest=1)=
!ax,bx refer to memory; do ax:=bx for n bytes
!savedest=1 to ensure that the value in ax register is not modified

	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg, axreg
	byte saved:=0

IF N=16 THEN
	WORKREG:=GETNEXTXREG()
	RX:=MGENXREG(WORKREG)

!CPL "COPYBLOCK:"
!CPL =(STROPND(AX))
!CPL =(STROPND(BX))
!
!MGENCOMMENT(STROPND(AX))
!MGENCOMMENT(STROPND(BX))
!
	GENMC(M_MOVDQU, RX, BX)
	GENMC(M_MOVDQU, AX, RX)

	FREEXREG(WORKREG)
	RETURN
FI

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg

	offset:=0

	if 1<=nwords<=4 then		!use unrolled code (no loop)
!CPL "COPYBLOCK",NWORDS,"WORDS"
		ax:=changeopndsize(ax,targetsize)
		bx:=changeopndsize(bx,targetsize)

		to nwords do
			genmc(m_mov,rx,applyoffset(bx,offset))
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop
		rcount:=mgenreg(countreg:=getnextreg())	!count
		lab:=++mlabelno
		if savedest then
			axreg:=ax.reg
			genmc(m_push, mgenreg(axreg))
			saved:=1
		fi

		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)
		AX.SIZE:=8

		genmc(m_mov,rcount,mgenint(nwords))
		genmc(m_labelx,mgenlabel(lab))
		genmc(m_mov,rx,bx)
		genmc(m_mov,ax,rx)

		genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))
		genmc(m_add,mgenreg(bx.reg),mgenint(targetsize))

		genmc(m_dec,rcount)
		genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))
!		if saved then
!			genmc(m_pop, mgenreg(axreg))
!		fi

		offset:=0
		freereg(countreg)
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx,4)
			genmc(m_mov,rx,applyoffset(bx,offset,4))
			genmc(m_mov,applyoffset(ax,offset,4),rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx,2)
			genmc(m_mov,rx,applyoffset(bx,offset,2))
			genmc(m_mov,applyoffset(ax,offset,2),rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx,1)
			genmc(m_mov,rx,applyoffset(bx,offset,1))
			genmc(m_mov,applyoffset(ax,offset,1),rx)
		fi
	fi
		if saved then
			genmc(m_pop, mgenreg(axreg))
		fi


	freereg(workreg)
end

function makesimpleaddr(mcloperand ax)mcloperand bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg

	if ax.reg and not ax.regix then return ax fi
	newreg:=(ax.reg | ax.reg | (ax.regix | ax.regix | getnextreg()))
	bx:=mgenireg(newreg)

	genmc(m_lea, mgenreg(newreg), ax)
	return bx
end

global proc clearblock(mcloperand ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg
	genmc(m_xorx,rx,rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,targetsize)

		to nwords do
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop

!SPLIT INTO TWO VERSIONS:
! NWORDS IS A MULTIPLE OF 4, so can write 4 words at a time, in 1/4 of iterations
! Or do one word at a time like now.
! nword is a multiple of 4 happens when N is a multiple of 32 bytes, which will
! always be the case for power-of-two sizes of 32 bytes or more. 32/64 may already
! be done without a loop. So non-part-unrolled version only really for odd array or
! struct sizes, such as [100]char.

		if nwords iand 3 then		!not 4n

			rcount:=mgenreg(countreg:=getnextreg())	!count
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)

			genmc(m_mov,rcount,mgenint(nwords))
			genmc(m_labelx,mgenlabel(lab))
			genmc(m_mov,ax,rx)

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
			freereg(countreg)
		else
			rcount:=mgenreg(countreg:=getnextreg())	!count
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
			freereg(countreg)
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

	freereg(workreg)
end

global proc genfunctiontable=
	[256]char str
	ichar s,t
	pcl currpcl
	int firststringlab,nextlab,nprocs

	if lab_funcaddrtable=0 then return fi
	mgencomment("Function Table")
	nprocs:=0

!	setsegment('C',16)
	setsegment('I',16)
	genmc(m_labelx, mgenlabel(lab_funcaddrtable))
	currpcl:=pcstart
	repeat
		if currpcl.opcode in [kprocdef,kthreadedproc] then
!IF FFUNTAB OR CURRPCL.DEF.ISHANDLER THEN
IF CURRPCL.DEF.ISHANDLER THEN
			genmc(m_dq,mgenmemaddr(currpcl.def))
			++nprocs
FI
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	firststringlab:=0

	genmc(m_labelx, mgenlabel(lab_funcnametable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
!IF FFUNTAB OR CURRPCL.DEF.ISHANDLER THEN
IF CURRPCL.DEF.ISHANDLER THEN
			if firststringlab=0 then
				firststringlab:=nextlab:=++mlabelno
			else
				nextlab:=++mlabelno
			fi

			genmc(m_dq,mgenlabel(nextlab))
fi
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	nextlab:=firststringlab
	currpcl:=pcstart
	repeat
		if currpcl.opcode in [kprocdef,kthreadedproc] then
!IF FFUNTAB OR CURRPCL.DEF.ISHANDLER THEN
IF CURRPCL.DEF.ISHANDLER THEN
			genmc(m_labelx,mgenlabel(nextlab))
			s:=currpcl.def.name
			t:=s

			while s^ do
				if s^='.' then
					t:=s+1
				fi
				++s
			od
!			genstring(t,1)
			genstring(t, length:-1, strtype:0)
			++nextlab
FI
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	genmc(m_labelx, mgenlabel(lab_funcnprocs))
	genmc(m_dq, mgenint(nprocs))
end

global function mgenextname(ichar s)mcloperand=
	[64]char str
	symbol d

	strcpy(&.str,s)
	str[strlen(s)]:=0

	d:=pcm_allocnfz(strec.bytes)

	d.name:=pcm_copyheapstring(&.str)
	d.isimport:=1

	return mgenmemaddr(d)
end

global proc mgeninfo(ichar s, int value)=
	[256]char str
	fprint @&.str,"# #",s:"15jl",value
	genmc_str(m_comment,&.str)
end

global proc mgeninfos(ichar s, svalue)=
	[256]char str
	fprint @&.str,"# #",s:"15jl",svalue
	genmc_str(m_comment,&.str)
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

function genasmopnd(unit p)mcloperand ax=
	symbol d
	int offset,labno
	unit a				!expr: nil/name/const/(add name, const)
	unit x,y
	symbol e

	if p=nil then return nil fi

	case p.tag
	when jassemreg then
		ax:=mgenreg(p.reg,p.regsize)

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
				d:=a.def
				if d.nameid=labelid then
					labno:=fixasmlabel(d)
					d:=nil
				fi
			when jbin then
				x:=a.a
				y:=a.b
				if x.tag=jname and y.tag=jconst then
					d:=x.def
					if d.nameid=labelid then
						labno:=fixasmlabel(d)
						d:=nil
					fi
				else
					goto error
				fi
				offset:=(a.pclop in [kadd,kaddrefx]|y.value|-y.value)
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
		d:=p.def
		if d.nameid=labelid then
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
			d:=x.def
			offset:=(p.pclop in [kadd,kaddrefx]|y.value|-y.value)
			if d.nameid=labelid then
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

function fixasmlabel(symbol d)int=
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
=== mc_decls.m 0 0 25/38 ===
export type mcloperand = ref mclopndrec

export record mclopndrec =
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		symbol def
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
	mcloperand a,b
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
!	int xxpos:(sourceoffset:24, fileno:8)
	ichar xxcomment
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

	(ov_cond=0,		"ov",	"o",		nov_cond),
	(nov_cond,	"nov",	"no",		ov_cond),

	(ltu_cond,	"ltu",	"b",		geu_cond),
	(geu_cond,	"geu",	"ae",		ltu_cond),

	(eq_cond,		"eq",	"z",		ne_cond),
	(ne_cond,		"ne",	"nz",		eq_cond),

	(leu_cond,	"leu",	"be",		gtu_cond),
	(gtu_cond,	"gtu",	"a",		leu_cond),

	(s_cond,		"s",	"s",		ns_cond),
	(ns_cond,		"ns",	"ns",		s_cond),

	(p_cond,		"p",	"p",		np_cond),
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
	(a_wreg,	$),		! Wide integer register, means Ri and Ri+1
end

global int mlabelno
!global byte foptimise

global const maxoperands=20

global [maxoperands+10]pclstackrec pclstack
global [maxoperands+10]pclvaluerec pclvals
global int noperands				!number of pcl operands, including wide
global int mstackdepth				!hw stack size (pcl operands, + extra for wide, + padding)

global [maxoperands]byte pcltempflags
global [maxoperands]mcloperand pcltemps

global record pclstackrec =
	byte loc			!loc code (stack, reg, xreg usually)
	byte reg			!reg or xreg when in register
	byte cat			!voidcat, or x32/x64/wide/block/var cat
	byte size
	byte ilabel			!1 to load contents of label type, not its address
	byte count			!instances of opnd, usually 1
	union
		byte wide		!realcat: whether r64
		byte signedx	!intcat: whether signed
	end
	byte spare
end

global record pclvaluerec =
	union
		u64	value		!immediate value
		r64	xvalue		!
		r32	xvalue32	!
!		ichar svalue
		int strindex
		int r64index
		int r32index
		symbol def		!for memaddr
		struct
			int32 labno		!for labels
			int32 offset
		end
		mcloperand mopnd
	end
end

!note: the pclstack grows downwards, so that pclstack[1] varies: gets nearer
!to the start as the stack grows. pclstack[2] etc is above that, a moving window
!of operands near the top of the stack
!global ref[]pclstackrec pclstack
!global pclstackrec pclstackzero
global pclstackrec pclstackdefault

!Where any active operand is located:

global enumdata [0:]ichar locnames =
	(no_loc=0,		$),			! not set
	(reg_loc,		$),			! in a d64 register
	(xreg_loc,		$),			! in an x64
	(immd64_loc,	$),			! imm: d64 immediate value
	(immx64_loc,	$),			! imm: x64 immediate value
	(immx32_loc,	$),			! imm: x32 immediate value
	(memaddr_loc,	$),			! imm: address of variable
	(label_loc,		$),			! imm: label
	(mem_loc,		$),			! mem: contents of variable
	(ilabel_loc,	$),			! mem: contents of label
	(temp_loc,		$),			! mem: contents of pcl temp slot
	(regvar_loc,	$),			! contents of register-variable 
	(xregvar_loc,	$),			! contents of xregister-variabke 

!	(stack_loc,		$),			! Deprecated
	(string_loc,	$),			! Deprecated
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
!global int inf_maxargs
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
!global const xc = 3
!global const yb = 2
!global const za = 1
!
!global const xb = 2
!global const ya = 1
!
!global const xa = 1

global macro wd = noperands-3
global macro xc = noperands-2
global macro yb = noperands-1
global macro za = noperands

global macro xb = noperands-1
global macro ya = noperands

global macro xa = noperands

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

global mcloperand dstackopnd
global mcloperand dframeopnd

global [r0..r15,1..8]mcloperand regtable

global [-128..64]mcloperand frameregtable

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

global symbol currasmproc

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
global ref []symbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref[]symbol labeldeftable

global int alineno

!export enumdata []ichar segmentnames =
!	(code_seg,		"code"),
!	(idata_seg,		"idata"),
!	(zdata_seg,		"zdata"),
!	(rodata_seg,	"rodata"),
!	(impdata_seg,	$),
!end

=== mc_objdecls.m 0 0 26/38 ===
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
	symbol def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	symbol def				!full st entry
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
=== mc_optim.m 0 0 27/38 ===
global proc peephole(ref mclrec m)=
	ref mclrec m2,m3,mtarget,lastmcl
	int lab1,lab2
	lastmcl:=nil

	if not fpeephole then return fi

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

function isreg(mcloperand a, int reg=rnone)int=
	if not a then return 0 fi
	if a.mode<>a_reg then return 0 fi
	if reg=rnone then return 0 fi
	return reg=a.reg
end

function isreg0(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_reg and a.reg=r0 then return 1 fi
	return 0
end

function isregopnd(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_reg and isregvar[a.reg] then return 1 fi
	return 0
end

function isconst(mcloperand a)int=
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
=== mc_stackmcl.m 0 0 28/38 ===
global macro freereg(r) =
	(regset[r]:=0; mccodex.regend[r]:=1)
!	(regset[r]:=0)

global proc initpass1(pcl p)=
!done before pass1 at procdef
	symbol d:=p.def, e

	passno:=1

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

	clear pcltempflags
	r10used:=r11used:=r13used:=0

	mstackdepth:=0
	noperands:=0

	frameoffset:=paramoffset:=framebytes:=0
	nlocals:=nparams:=0
	usedparams:=usedxparams:=0
	nproccalls:=highargs:=0
	localshadow:=0
	assemused:=0
	highreg:=highxreg:=0


	nlocals:=nparams:=0
	clear pcltempflags

	if ttisblock[d.mode] then
!CPL "ISBLOCK",D.NAME
		e:=getduplnameptr(d,addnamestr("$1x"),paramid)
		e.mode:=d.mode
		e.used:=1
		blockretname:=e
		paramdefs[++nparams]:=e
	fi

	e:=d.deflist
!CPL "INITPASS1",=E.DEFLIST

!framedefs not done until initpass1x, to allow for any late blockdefs
	while e, e:=e.nextdef do
		case e.nameid
		when paramid then
			if nparams>=maxparams then merror("Too many params") fi
			paramdefs[++nparams]:=e
!CPL =NPARAMS, E.NAME
		esac
	od
end

global proc initpass1x(pcl p)=
	symbol d:=p.def, e

	e:=d.deflist

	while e, e:=e.nextdef do
		case e.nameid
		when frameid then
			if not e.atvar then
				if e.used then
					if nlocals>=maxlocals then merror("Too many locals") fi
					localdefs[++nlocals]:=e
				elsif fcheckunusedlocals then
!					println "Unused:", e.name:"16 jl","in:",e.owner.name 
					fprintln "Unused: # in: # (#)", e.name:"16 jl",e.owner.name, e.owner.owner.name
				fi

			fi
		esac
	od
end

global proc checkglobals=
	symbol d, e

	d:=stprogram.deflist

	while d, d:=d.nextdef do
		case d.nameid
		when moduleid then
!CPL "MODULE", D.NAME
			e:=d.deflist
			while e, e:=e.nextdef do
!				println "	var",e.name
!				if e.nameid in [staticid, procid] and not e.used and not e.ishandler then
				if e.nameid=staticid and not e.used then
					fprintln "Unused global: # in: # (#)", e.name:"16 jl",e.owner.name, e.owner.owner.name
				fi

			od

!			if not e.atvar then
!				if e.used then
!					if nlocals>=maxlocals then merror("Too many locals") fi
!					localdefs[++nlocals]:=e
!				elsif fcheckunusedlocals then
!!					println "Unused:", e.name:"16 jl","in:",e.owner.name 
!					fprintln "Unused: # in: # (#)", e.name:"16 jl",e.owner.name, e.owner.owner.name
!				fi
!
!			fi
		esac
	od
end

global proc initpass2=
!done before pass2 at procdef
	int reg
	symbol d
	passno:=2

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

	clear pcltempflags
	r10used:=r11used:=r13used:=0

	mstackdepth:=0
	noperands:=0

	frameoffset:=paramoffset:=framebytes:=0

!CPL "INITPASS2",=NLOCALS

!high/xregs were set in pass1. They can be extended here by use as regvars
!It is assumed that in pass2, highreg will be no higher than it was in pass1.
!It might be lower, but I don't know that.


!do regvars assignments

	if leafproc then
		for i to nparams do
			exit when i>4
			d:=paramdefs[i]
			if d.used and not d.addrof and not d.noreg and d.nrefs then
				case ttbasetype[d.mode]
				when ti64, tu64, tref then
					d.reg:=reg:=r10+i-1
					isregvar[reg]:=1
					if reg=r10 then r10used:=1 fi
					if reg=r11 then r11used:=1 fi
					if reg=r13 then r13used:=1 fi
				when tr64 then
					d.reg:=reg:=r0+i-1
					isxregvar[d.reg]:=1
				esac
			fi
		od
	fi


	for i to nlocals do
		d:=localdefs[i]
!CPL "CHECKLOCAL",D.NAME,=D.ADDROF,D.NREFS
		if not d.addrof and not d.noreg and d.nrefs and 
				ttbasetype[d.mode] not in [trecord, tarray] then
			case ttbasetype[d.mode]
			when ti64, tu64, tref then
				if highreg>=r9 then nextloop fi			!no more regs
				if highreg<r3 then
					highreg:=r3
				else
					++highreg
				fi
				d.reg:=highreg
				isregvar[highreg]:=1
			when tr64 then
				if highxreg>=r15 then nextloop fi			!no more regs
				if highxreg<r6 then
					highxreg:=r6
				else
					++highxreg
				fi
				d.reg:=highxreg
				isxregvar[highxreg]:=1
			esac
		fi
	od

	if not leafproc then
		for i to nparams do
			exit when i>4
			d:=paramdefs[i]
			if d.used and not d.addrof and not d.noreg and d.nrefs then
				case ttbasetype[d.mode]
				when ti64, tu64, tref then
					if ttbasetype[d.mode] not in [trecord, tarray] then
!CPL "2:TRY FOR REGVAR PARAM",d.name,STRMODE(D.MODE)
						if highreg>=r9 then nextloop fi			!no more regs
						if highreg<r3 then
							highreg:=r3
						else
							++highreg
						fi
						d.reg:=highreg
						isregvar[highreg]:=1
					fi
				esac
			fi
		od
	fi
end

global proc checkopnds=
!after endproc on pass1 or pass2
	symbol d

	if mstackdepth then
		println passno,"HW stack not empty",currproc.name,=mstackdepth
		MSTACKDEPTH:=0

!		MERROR("reset:mstackdepth?")
	fi
	if noperands then
		println passno,"Reset:pcl stack not empty:",currproc.name,=noperands
	fi

	for i in regset.bounds do
		if regset[i] or xregset[i] then
			println passno,"Reset: reg flag set",currproc.name,REGSET[I],XREGSET[I],=I
			exit
		fi
	od
end

proc start=
	pclstackdefault.cat:=intcat
	pclstackdefault.size:=8
	pclstackdefault.wide:=1
	pclstackdefault.count:=1
end

func newopnd(int loc=reg_loc)ref pclstackrec pc=
	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi
	pc:=&pclstack[++noperands]
	pc^:=pclstackdefault
	pc.loc:=loc
	pclvals[noperands].value:=0

	pc	
end

global proc duploperand=
!assume 64-bit operand
	int reg
	ref pclstackrec pc
	mcloperand ax
	
	checkloaded(xa)			!simplest if in a register

	++noperands
IF NOPERANDS>MAXOPERANDS THEN MERROR("OV1") FI

	pclstack[noperands]:=pclstack[noperands-1]
	pclvals[noperands]:=pclvals[noperands-1]

	pc:=&pclstack[noperands]
!!There is now a simple duplicate; but it will need more work depending
!!on the current format

	case pc.loc
	when reg_loc then			!need to be physically duplicated
		reg:=getnextreg()
		pclstack[ya].reg:=reg
		genmc(m_mov, ax:=mgenreg(reg), mgenreg(pclstack[xb].reg))

	when xreg_loc then			!need to be physically duplicated
		reg:=getnextxreg()
		pclstack[xa].reg:=reg
		genmc(m_movd+pc.wide, mgenxreg(reg,pc.size), mgenxreg(pclstack[xb].reg,pc.size))
	esac
end

global proc addlabel(int lab,offset=0)=
	newopnd(label_loc)
	pclvals[xa].labno:=lab
	pclvals[xa].offset:=offset
end

global proc addreg0(int reg)=
!turn return value in r0 into a new pclstack operand
!(modified for mult regs)
	ref pclstackrec pc
	pc:=newopnd(reg_loc)
	pc.reg:=reg
	if regset[reg] then
		merror("addreg0/reg in use")
	fi
	regset[reg]:=1
end

global proc addxreg0(int reg,wide)=
!turn return value in x0 into a new pclstack operand
	ref pclstackrec pc

	pc:=newopnd(xreg_loc)
	pc.reg:=reg
	pc.cat:=realcat
	pc.size:=wide*8
	pc.wide:=wide
	if xregset[reg] then merror("addxreg0/reg in use") fi
	xregset[reg]:=1
end

global proc addreg_d64=
!create new pcl opnd in any d64 reg
	ref pclstackrec pc
	pc:=newopnd(reg_loc)
	pc.reg:=getnextreg()
end

global proc addxreg(int wide)=
	ref pclstackrec pc
	pc:=newopnd(xreg_loc)
	pc.reg:=getnextxreg()
	pc.cat:=realcat
	pc.wide:=wide
end

global proc loadparam(int n=noperands, reg)=
!load pcl opnd n into given register
	int oldreg, value
	mcloperand ax, bx
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

	ax:=mgenreg(reg)
	oldreg:=pc.reg

	case pc.loc
	when reg_loc, regvar_loc then
		genmc(m_mov, ax, mgenreg(oldreg))

	when xreg_loc, xregvar_loc then
		genmc(m_movd+pc.wide, changeopndsize(ax,pc.size), mgenxreg(oldreg))

	when immd64_loc then
		value:=pcv.value
		if value=0 then
			ax:=mgenreg(reg,4)
			genmc(m_xorx, ax,ax)
		else
			genmc(m_mov, ax, mgenint(pcv.value))
		fi
	when immx64_loc then
		genmc(m_mov, ax, mgenlabelmem(pcv.r64index))

	when immx32_loc then
		genmc(m_mov, ax, mgenlabelmem(pcv.r32index))

	when string_loc then
!		genmc(m_mov,ax, mgenlabel(pcv.strindex))
		genmc(m_lea,ax, mgenlabelmem(pcv.strindex))

	when mem_loc then
		genmc(m_mov,ax,mgenmem(pcv.def))

	when memaddr_loc then
		genmc(m_lea,ax,mgenmem(pcv.def))

	when temp_loc then
		genmc(m_mov,ax,mgentemp(n))

	when label_loc then
		if pc.ilabel then
			bx:=mgenlabelmem(pcv.labno)
		else
			bx:=mgenlabel(pcv.labno)
		fi

		genmc(m_mov, ax, bx)

	else
!		CPL "LOADPARAM:",locnames[pc.loc]
!		MGENCOMMENT("****LOADPARAM??")
		MERROR("LOADPARAM??",locnames[pc.loc])
	esac
end

global proc loadxparam(int n=noperands, reg)=
	mcloperand ax
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

	ax:=mgenxreg(reg)

	case pc.loc
	when reg_loc, regvar_loc then
		genmc(m_movq, ax, mgenreg(pc.reg))

	when xreg_loc, xregvar_loc then
!		if pc.reg<>reg then
			genmc(m_movd+pc.wide, ax, mgenxreg(pc.reg))
!		fi

	when immx64_loc then
		genmc(m_movq, ax, mgenlabelmem(pcv.r64index))

	when immx32_loc then
		genmc(m_movd, ax, mgenlabelmem(pcv.r32index))

	when temp_loc then
		genmc(m_movd+pc.wide, ax, mgentemp(n))

	else
!		CPL "??LOADXPARAM",N,NOPERANDS
!		MGENCOMMENT("****LOADXPARAM??")
		MERROR("LOADXPARAM??",locnames[pc.loc])
	esac
end

global function getopnd(int n=noperands,size=8)mcloperand ax=
!get an access mode for pcl opnd n, which means a register operand, memory, 
! immediate etc
!It can mean loading to a register if opnd is currently on stack, or float imm, etc
!In that case, its loc will be updated
!getopnd should be called from top-of-stack down, in case they are currently
!on the hardware stack, which must be popped in sequence
!Any regvars stay in their regs, but this means they can't be modified

!int, float, or low half of wide
	int reg, value
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

	case pc.loc
	when reg_loc then
		return mgenreg(pc.reg,size)

	when regvar_loc then
		return mgenregvar(pcv.def)

	when xreg_loc then
		return mgenxreg(pc.reg,size)
!
	when xregvar_loc then
		return mgenxregvar(pcv.def)

	when temp_loc then
		return mgentemp(n)

	when immd64_loc then
		value:=pcv.value
		if int32.min<=value<=int32.max then
			return mgenint(value)
		fi
		ax:=mgenreg(getnextreg())
		genmc(m_mov, ax, mgenint(value))
		pc.reg:=ax.reg
		pc.loc:=reg_loc
		return ax

	when immx64_loc then
		return mgenlabelmem(pcv.r64index)

	when immx32_loc then
		return mgenlabelmem(pcv.r32index)

	when memaddr_loc then
		reg:=getnextreg()
		ax:=mgenreg(reg)
		genmc(m_lea,ax,mgenmem(pcv.def))
		pc.reg:=ax.reg
		pc.loc:=reg_loc
		return ax

	when mem_loc then
		return mgenmem(pcv.def)

	when string_loc then
!		return mgenlabel(getstringindex(pcv.svalue))
		return mgenlabel(pcv.strindex)

	when label_loc then
		MGENCOMMENT("GENLABELMEM")
		return mgenlabelmem(pcv.labno)

	else
		merror("GETOPND? ",locnames[pc.loc])
	esac
!
	return nil
end

global function loadopnd(int n=noperands, size=8)mcloperand =
!ensure operand is loaded to a register, either reg/xreg depending on category
!regvars must be loaded to a general register (eg. to do ops on it)

	mcloperand ax,bx
	int reg
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]

!special handling for regvars, which are already in regs
	case pc.loc
	when regvar_loc then
		genmc(m_mov, ax:=mgenreg(reg:=getnextreg(),size), mgenreg(pc.reg,size))
		pc.loc:=reg_loc
		pc.reg:=reg
		return ax
	when xregvar_loc then
		if pc.wide then
			genmc(m_movq, ax:=mgenxreg(reg:=getnextxreg()), mgenxreg(pc.reg))
			pc.loc:=xreg_loc
			pc.reg:=reg
			return ax
		else
			merror("loadopnd/xreg32")
		fi
	when label_loc then
		reg:=getnextreg()

		if pc.ilabel then
			bx:=mgenlabelmem(pcv.labno)
		else
			bx:=mgenlabel(pcv.labno)
		fi

		genmc(m_mov, ax:=mgenreg(reg,size), bx)
		pc.reg:=reg
		pc.loc:=reg_loc
		return ax
	esac

!get the access mode
	ax:=getopnd(n,size)
!if the current loc is now in a reg, then done
	if pc.loc in [reg_loc, xreg_loc] then return ax fi

!opnd not loaded; get it inoto a register via ax if needed
	case pc.cat
	when intcat then
		if pc.size<8 then merror("loadopnd/short") fi
		reg:=getnextreg()
		if pc.loc=immd64_loc and pcv.value=0 and not noxorclear then
			ax:=mgenreg(reg,4)
			genmc(m_xorx,ax,ax)
			bx:=mgenreg(reg,size)
		else
			genmc(m_mov, bx:=mgenreg(reg,size), changeopndsize(ax,size))
		fi
		pc.reg:=reg
		pc.loc:=reg_loc

	when realcat then
		genmc(m_movd+pc.wide, bx:=mgenxreg(reg:=getnextxreg(),pc.size), ax)
		pc.reg:=reg
		pc.loc:=xreg_loc
	else
		merror("Loadopnd:",catnames[pc.cat])
	esac

	return bx
end

global proc checkloaded(int n=noperands)=
	if pclstack[n].loc in [reg_loc, xreg_loc] then
		return
	fi
	loadopnd(n)
end

global proc checkallloaded=
	for i to noperands do
		case pclstack[i].loc
		when immd64_loc, immx64_loc, immx32_loc, memaddr_loc then
		else
			checkloaded(i)
		esac
	od
end

global function getopnd_ind(int index=noperands,size=8)mcloperand=
!int, float, or low half of wide
!
	case pclstack[index].loc
	when reg_loc then
		return mgenireg(pclstack[index].reg,size)
	esac

	loadopnd(index)

	return getopnd_ind(index,size)
end

global function getnextreg:int=
	to 16 do
		for r:=r0 to regmax do
			if regset[r]=0 then
				regset[r]:=1
				highreg max:=r
				return r
			fi
		od

!all regs occupied; need to free one
		savenextopnd()
	od
	merror("GNR")
	0
end

!global function getnextxreg(int firstop=1)int=
global function getnextxreg:int=
	int reg,firstreg

	to 16 do
		for r:=r4 to xregmax do
			if xregset[r]=0 then
				xregset[r]:=1
				highxreg max:=r
				return r
			fi
		od
	!all regs occupied; need to free one
		savenextxopnd()
	od
	merror("GNXR")
	0
end

global proc freexreg(int xr)=
	xregset[xr]:=0
end

global proc pushopnd(int n)=
!make sure operand n is on the hw stack; caller must know that all
!previous pclstack operands are already on the stack
	ref pclstackrec pc:=&pclstack[n]
	ref pclvaluerec pcv:=&pclvals[n]
	mcloperand ax

	case pc.loc
	when reg_loc then
		genmc(m_push, mgenreg(pc.reg))
		freereg(pc.reg)

	when regvar_loc then
		genmc(m_push, mgenreg(pc.reg))

	when xreg_loc then
		if r13used then merror("2:R13 in use") fi
		genmc(m_movq,mgenreg(r13), mgenxreg(pc.reg))
		genmc(m_push, mgenreg(r13))
		freexreg(pc.reg)

	when immd64_loc then
		genmc(m_push, mgenint(pcv.value))

	when immx64_loc then
		genmc(m_push, mgenlabelmem(pcv.r64index))

	when immx32_loc then
		if r13used then merror("4:R13 in use") fi
		genmc(m_mov, mgenreg(r13,4), mgenlabelmem(pcv.r32index))
		genmc(m_push, mgenreg(r13))

	when string_loc then
		genmc(m_push, mgenlabel(pcv.strindex))

	when memaddr_loc then
		if r13used then merror("3:R13 in use") fi
		genmc(m_lea, mgenreg(r13), mgenmem(pcv.def))
		genmc(m_push, mgenreg(r13))

	when mem_loc then
		genmc(m_push, mgenmem(pcv.def))

	else
		merror("Can't push opnd: #",locnames[pc.loc])
	esac

	delopnd()
	++mstackdepth
end

global proc popargs(int nargslots)=
!pop pcl opnds which were args to a call
!however, nargslots is the number of slots used, not actual args, since
!a wide argument was counted as two slots
	int cat

	while nargslots>0 do
		cat:=pclstack[xa].cat
		poparg()
		--nargslots
	od
end

global proc poparg=

	case pclstack[xa].loc
	when reg_loc then
		freereg(pclstack[xa].reg)
	when xreg_loc then freexreg(pclstack[xa].reg)
	when immd64_loc, string_loc, memaddr_loc, mem_loc,
			immx64_loc, immx32_loc then
	when regvar_loc then
	when xregvar_loc then
	else
		merror("poparg? #",locnames[pclstack[xa].loc])
	esac
	--noperands
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

global proc saveopnd(int n, allregs=1)=
!if operand is in a volatile register, then save it in a temp
!allregs=1 to save both A and B regs (vol/nonval), which can include P regs if
!used as workregs; this is to save pne reg=opnd to a temp to free up a register
!allregs=0 to limit to A regs (possibly some P regs) only; normall for CALLs
!in order to preserve non-vol regs, as call will preserve the rest

	int reg
	mcloperand tx
	ref pclstackrec pc:=&pclstack[n]

	case pc.loc
	when reg_loc then
		reg:=pc.reg
		if allregs or reg not in r3..r9 then
			genmc(m_mov, tx:=mgentemp(n), mgenreg(reg))
			pc.loc:=temp_loc
			pclvals[n].mopnd:=tx
			freereg(reg)
		fi

	when xreg_loc then
		reg:=pc.reg
		if allregs or reg in r0..r5 then
			genmc(m_movd+pc.wide, mgentemp(n), mgenxreg(reg,pc.size))
			pc.loc:=temp_loc
			freexreg(reg)
		fi
	when regvar_loc, xregvar_loc then

	when immd64_loc, memaddr_loc, mem_loc, immx64_loc, immx32_loc,
		temp_loc, string_loc then

	else
		merror("Can't save opnd: #",locnames[pc.loc])
	esac

end

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
		if pclstack[i].loc=reg_loc then
			saveopnd(i,1)
			return
		fi
	od
end

global proc savenextxopnd=
!as savenextopnd but only does AX/BX/PX regs 
	for i:=1 to noperands do
		if pclstack[i].loc=xreg_loc then
			saveopnd(i,1)
			return
		fi
	od
end

global proc movetoreg(int newreg)=
	int oldreg

	loadopnd()
	oldreg:=pclstack[xa].reg

	if oldreg=newreg then
		return
	fi

	if regset[newreg] then
!			println "movereg/reg in use"
!			mgencomment("movereg/reg in use")
			merror("movereg/reg in use")
	fi
	genmc(m_mov, mgenreg(newreg), mgenreg(oldreg))
	freereg(oldreg)
	pclstack[xa].reg:=newreg
	pclstack[xa].loc:=reg_loc
	regset[newreg]:=1
	if newreg>=r10 then highreg max:=newreg fi
end
!
global proc swapopnds(int m,n)=
!exchange top opndstack entry (m assumed to be 1) with n'th entry down
!uses notional index of stack with:
!	[1] meaning opndstack[noperands]
!	[n] meaning opndstack[noperands-n+1]
!NOTE: all operands m to n inclusive
!caller is responsible for this (getopnds(n) might ensure this when m=1)
!usually m=1
	pclstackrec t

	t:=pclstack[m]
	pclstack[m]:=pclstack[n]
	pclstack[n]:=t

	pclvaluerec u
	u:=pclvals[m]
	pclvals[m]:=pclvals[n]
	pclvals[n]:=u

!	swap(pclvals[m], pclvals[n])

end

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2

	int reg1:=pclstack[xa].reg

	if reg1=reg2 then return fi

	for i:=noperands-1 downto 1 do
		if pclstack[i].loc=reg_loc and pclstack[i].reg=reg2 then
			swap(pclstack[xa].reg, pclstack[xb].reg)
			return
		fi
	else
		CPL CURRPROC.NAME
		merror("swapopndregs/reg not found")
	od
end

global proc addmem(pcl p)=
	int reg
	symbol d:=p.def
	mcloperand ax
	ref pclstackrec pc

	case p.pcat
	when intcat then
		if p.psize=8 then
			if d.reg then
				pc:=newopnd(regvar_loc)
				pc.reg:=d.reg
			else
				newopnd(mem_loc)
			fi
			pclvals[xa].def:=d
		else
			pc:=newopnd(reg_loc)
			pc.reg:=reg:=getnextreg()
			ax:=getopnd(xa)
			genmc((p.psigned|m_movsx|m_movzx), ax, mgenmem(d))
		fi

	when realcat then
		pc:=newopnd(xreg_loc)
		pc.reg:=reg:=getnextxreg()
		pc.cat:=realcat
		pc.wide:=p.pwide
		pc.size:=p.psize

!		genmc(m_movd+p.pwide, mgenxreg(reg, p.psize),mgenmem(d))
		genmc(m_movd+p.pwide, mgenxreg(reg, p.psize),mgenmem(d, (p.psize=8|tr64|tr32)))

	when blockcat then
		pc:=newopnd(reg_loc)
		pc.reg:=reg:=getnextreg()
		genmc(m_lea,mgenreg(reg),mgenmem(d))

	esac
end

global proc addmemaddr(symbol d)=
	newopnd(memaddr_loc)
	pclvals[xa].def:=d
!	d.addrof:=1
end


global proc addimm(u64 a)=
	newopnd(immd64_loc)
	pclvals[xa].value:=a
end

global proc addimmx64(int r64index)=
	newopnd(immx64_loc)
	pclvals[xa].r64index:=r64index
	pclstack[xa].cat:=realcat
	pclstack[xa].wide:=1
	pclstack[xa].size:=8
end

global proc addimmx32(int r32index)=
	newopnd(immx32_loc)
	pclvals[xa].r32index:=r32index
	pclstack[xa].cat:=realcat
	pclstack[xa].wide:=0
	pclstack[xa].size:=4
end

global proc addstr(int strindex)=
	newopnd(string_loc)
	pclvals[xa].strindex:=strindex
end

global proc delopnd=
	int reg
	ref pclstackrec pc

	if noperands<=0 then
		MGENCOMMENT("****DELND/UNDERFLOW")
		RETURN
	fi

	pc:=&pclstack[noperands]

	if pc.count>1 then
		--pc.count
		return
	fi

	case pc.loc
	when reg_loc, regvar_loc then
		reg:=pc.reg

		freereg(reg)
	when xreg_loc, xregvar_loc then
		freexreg(pc.reg)
	when immd64_loc, memaddr_loc, immx64_loc, immx32_loc, mem_loc,
		regvar_loc, xregvar_loc, string_loc, temp_loc, label_loc then
	else
		merror("delopnd: can't do xreg etc",locnames[pc.loc])
	esac	

	--noperands
end

=== mc_writeexe.m 0 0 29/38 ===
!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order:
! initsectiontable()
! genexe()
! writeexe(filename)

[maxlibfile]int64 libinsttable
[maxlibfile]ichar libinstnames
[maxlibfile]int libnotable			!index into dlltable

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
symbol stentrypoint				!symbol to be the entry point
symbol stentrypoint2
symbol stentrypoint3

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

global proc writeexe(ichar outfile,int dodll)=
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

	if fverbose>=2 then
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

	if highmem then
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

	for i to nlibfiles when libfiles[i]^<>'$' do
		strcpy(&.filename,libfiles[i])
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

	if fverbose>=2 then
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

	for i:=1 to nlibfiles when libinsttable[i] do
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
	strcpy(&.str,libfiles[n])
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
	symbol d
	ichar name, libname, dname, basename

	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		dname:=(d.truename|d.truename|d.name)
		if d.isimport then
			if nimports>=maximports then axerror("genexe: Too many imports") fi
			++nimports
			name:=extractlibname(dname,libno,1)
			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		elsif d.scope=export_scope then
			basename:=getbasename(dname)
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
			exporttable[nexports].name:=getbasename(dname)
		fi
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref word32 p32
	ref word64 p64
	symbol d
	int offset,index,thunkoffset,iatoffset

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]
		index:=d.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset

		case r.reloctype
		when rel32_rel then
			if not d.isimport then
				axerror("rel32/not imported")
			fi
			(ref word32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.isimport then
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
	symbol d
	int index

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]

		case r.reloctype
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.isimport then
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
	symbol d
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
	symbol d,e

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

function getsectionno(int segment)int=
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

=== mx_run.m 0 0 30/38 ===
!Translate SS data directly into MCU block, then try and run that

global function writememlib(ichar filename)ref librec plib=
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
	for i to nlibfiles when libfiles[i]^<>'$' do
!		if libtypes[i]='D' then ++ndlls else ++nlibs fi
		++ndlls
	od

	lib.ndlllibs:=ndlls
	lib.nlibs:=nlibs

	lib.dllnames:=pcm_alloc(ichar.bytes*ndlls)
	lib.libnames:=pcm_alloc(ichar.bytes*nlibs)

	k:=0
!	for i to nlibfiles when libfiles[i]^<>'$' and libtypes[i]='D' do
	for i to nlibfiles when libfiles[i]^<>'$' do
		lib.dllnames[++k]:=libfiles[i]
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
	symbol d
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
				if d.isimport then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.isimport then
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
	symbol d, stentry:=nil
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
		lib.importnames[++k]:=(d.truename|d.truename|d.name)
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
	symbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.scope=export_scope then d.expindex:=++nsymexports fi
		if d.isimport then d.impindex:=++nsymimports fi
	od
end

global proc runlibfile(ichar filename, int cmdskip)=
LOADERROR("RUNLIBFILE")

!	ref librec plib
!
!	plib:=writememlib(filename)
!
!	loadmemmcu(plib)
!	fixuplib(plib)
!
!!	if fshowmx then
!!		LOADERROR("SHOWMX missing")
!!!		initlogfile()
!!!		showlibs()
!!!		closelogfile()
!!	else
!		runprogram(plib, cmdskip)
!!	fi
end

=== mm_types.m 0 1 31/38 ===
global enumdata  [0:]ichar stdnames,
        [0:]byte stdbits,
        [0:]byte stdcat =
!    type        name         bits  code    cat
    (tvoid=0,     "void",        0,   voidcat),

    (tc64,        "c64",        64,   intcat),
    (tu64,        "u64",        64,   intcat),
    (ti64,        "i64",        64,   intcat),
    (tr32,        "r32",        32,   realcat),
    (tr64,        "r64",        64,   realcat),

    (tbool64,     "bool64",     64,   intcat),
    (tref,        "ref",        64,   intcat),

    (trecord,     "rec",         0,   blockcat),
    (trange,      "range",     128,   blockcat),

    (tarray,      "array",       0,   blockcat),
    (tslice,      "slice",     128,   blockcat),

    (tc8,         "c8",          8,   intcat),
    (tbool8,      "b8",          8,   intcat),
    (ti8,         "i8",          8,   intcat),
    (ti16,        "i16",        16,   intcat),
    (ti32,        "i32",        32,   intcat),
    (tu8,         "u8",          8,   intcat),
    (tu16,        "u16",        16,   intcat),
    (tu32,        "u32",        32,   intcat),

    (trefchar,    "ichar",      64,   intcat),
    (trefbit,     "refbit",    128,   blockcat),

    (tauto,       "auto",        0,   voidcat),
    (tany,        "any",         0,   voidcat),
    (tproc,       "proc",        0,   voidcat),
    (tlabel,      "label",       0,   voidcat),
    (ttype,       "type",       64,   voidcat),
    (tbitfield,   "bitfl",       8,   voidcat),
    (ttuple,      "tuple",       0,   blockcat),
    (tpending,    "pend",        0,   voidcat),
    (tblock,      "block",       0,   blockcat),

    (tlast,       "last ",       0,   voidcat),
end

global enumdata [0:]ichar catnames =
    (voidcat=0,     $),         ! Not set

    (intcat,        $),         ! 8/16/32/64-bit signed/unsigned ints
    (realcat,       $),         ! 32/64-bit floats
    (blockcat,      $),         ! block data of any size including 1-8 bytes
end


global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tbool	= tbool64

global const tfirstnum	= tc64
global const tlastnum	= tr64

global const tfirstshort	= tc8
global const tlastshort		= tu32

global const maxtuplesize = 4

global int trefproc
global int treflabel
=== mm_help.txt 0 1 32/38 ===
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



=== msyslib.m 0 1 33/38 ===
module msys
module mlib
module mclib
module mwindows
module mwindll
=== msys.m 0 1 34/38 ===
global record procinforec=
	word16		fnindex
	byte		rettype
	byte		nparams
	[12]byte	paramlist
end

!for print/read routines
!------------------------------------------
export record fmtrec=	! (default)
	byte	minwidth	! n (0)   min field width (0 if not used or don't care)
	i8		precision	! .n (0)   number of decimals/significant figures/max width
	byte	base		! B,H or Xn (10)  2 to 16

	char	quotechar	! Qc (0)   0 or '"' or c
	char	padchar		! Pc, Z (' ')
	char	realfmt		! E,F,G ('f') 'e' or 'f' or 'g'

	char	plus		! (0)   0 or '+'
	char	sepchar		! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
	char	lettercase	! A,a ('A') 'A' or 'a'
	char	justify		! JL, JR, JC ('R') 'L' or 'R' or 'C'?
	char	suffix		! Tc (0)   0 or 'B' or 'H' or c
	char	usigned		! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
	char	charmode	! C,M (0)  0 or 'C' or 'M'	o/p int as int or single char or multi-char
	char	heapmode	! D (0)  'D' for str-functions, return ptr to heap string
	char	param		! Use int value for <fmtparam>
	byte	spare : (showtype:1, newline:1)
end

int fmtparam			!as set with :'V'

enumdata =
	std_io,file_io,str_io
end

const comma = ','

export int $cmdskip			!0 unless set by READMCX/etc

export int needgap			= 0
int outdev			= std_io
filehandle outchan	= nil
ref char fmtstr 	= nil

const maxiostack=10
[maxiostack]filehandle	outchan_stack
[maxiostack]int			outdev_stack
[maxiostack]ref char	fmtstr_stack
[maxiostack]byte		needgap_stack

[maxiostack]ref char	ptr_stack		!this one doesn't need pushing, as each is pointed to from outchan
int niostack=0

[0:]char digits=A"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

!Read buffer vars
export const rd_buffersize = 16384	!total capacity of line buffer

export ref char rd_buffer		! point to start of read buffer
export int rd_length			! length of this line (as read by readln)
export ref char rd_pos			! current position it's up to (next read starts here)
export ref char rd_lastpos		! set by sread() just before reading used for reread()

int termchar			! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals

[4096]char printbuffer
ichar printptr
int printlen

!------------------------------------------

!const maxparam=128
const maxparam=256
export int nsysparams
export int ncmdparams
export int nenvstrings
export [maxparam]ichar sysparams
!export ref[]ichar cmdparams
export ref[0:]ichar cmdparams
export ref[]ichar envstrings
!export [maxparam]ichar envstrings

const maxcallback=8
[0..maxcallback,8]word64 callbackstack
int ncallbacks=0

proc start=
	int32 nargs
	int nargs64
	ref[]ichar args
	static [128]byte startupinfo			! 68 or 104 bytes
	int res

!CPL "MSYS START"

	res:=__getmainargs(&nargs,cast(&args),cast(&envstrings),0,cast(&startupinfo))
	
	nsysparams:=nargs

	if nsysparams>maxparam then
		printf("Too many params\n")
		stop 50
	fi

	nargs64:=nargs			!bug when using 32-bit limit when compiled with mm
	for i:=1 to nargs64 do
		sysparams[i]:=args[i]
	od
	
!assume nsysparams is >=1, since first is always the program name
	ncmdparams:=nsysparams-($cmdskip+1)
	cmdparams:=cast(&sysparams[$cmdskip+1])

	int j:=1
	nenvstrings:=0
	while envstrings[j] do
		++nenvstrings
		++j
	od
end

proc pushio=
	if niostack>=maxiostack then
		printf("Too many io levels\n")
		stop 53
	fi
	++niostack
	outchan_stack[niostack]	:= outchan
	outdev_stack[niostack]	:= outdev
	fmtstr_stack[niostack]	:= fmtstr
	needgap_stack[niostack]	:= needgap
	needgap:=0
	fmtstr:=nil
	outchan:=nil
end

export proc m$print_startfile(ref void dev)=
	pushio()
	outchan:=cast(dev)
	if dev then
		outdev:=file_io
	else
		outdev:=std_io
	fi
	resetprintbuffer()
end

export proc m$print_startstr(ref char s)=
	ref ref char p
	pushio()

	ptr_stack[niostack]:=s
	p:=&ptr_stack[niostack]

	outchan:=cast(p)
	outdev:=str_io
end

export proc m$print_startptr(ref ref char p)=
	pushio()

	outchan:=cast(p)
	outdev:=str_io
end

export proc m$print_startcon=
	pushio()
	outdev:=std_io
	resetprintbuffer()
end

export proc m$print_setfmt(ref char format)=
	fmtstr:=format
end

export proc m$print_end=
	needgap:=0
	nextfmtchars(1)
	if niostack=1 and outdev in [std_io,file_io] then
		dumpprintbuffer()
	fi

	if niostack=0 then return fi
	outchan	:= outchan_stack[niostack]
	outdev	:= outdev_stack[niostack]
	fmtstr	:= fmtstr_stack[niostack]
	needgap	:= needgap_stack[niostack]

	--niostack
end

export proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
	[20]char s

	if fmtstyle=nil then
		fmtstyle:="z8H"
	fi
	m$print_u64(a,fmtstyle)
end

export proc m$print_ptr_nf(u64 a)=
	m$print_ptr(a)
end

export proc m$print_i64(int64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt
	int n

	nextfmtchars()
	if fmtstyle=nil then
		if a>=0 then
			n:=u64tostr(a,&.s,10,0)
		elsif a=i64.min then
			fmt:=defaultfmt
			dofmt

		else
			s[1]:='-'
			n:=u64tostr(-a,&s[2],10,0)+1
		fi

		printstr_n(&.s,n)

	else

		strtofmt(fmtstyle,-1,&fmt)
		if fmt.param='V' then
			fmtparam:=a
			needgap:=0
		else
dofmt:
			tostr_i64(a,&fmt)
		fi
	fi
	needgap:=1
end

export proc m$print_i64_nf(int64 a)=
	m$print_i64(a)
end

export proc m$print_bool(int64 a, ichar fmtstyle=nil)=
	if a then
		m$print_str("True",fmtstyle)
	else
		m$print_str("False",fmtstyle)
	fi
end

export proc m$print_u64(word64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt

	nextfmtchars()
	if fmtstyle=nil then
		sprintf(&.s,"%llu",a)
		printstr(&.s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_u64(a,&fmt)
	fi
	needgap:=1
end

export proc m$print_r64(real x,ichar fmtstyle=nil)=
	[360]char s
	fmtrec fmt

	nextfmtchars()
	if fmtstyle=nil then
		sprintf(&.s,"%f",x)
		printstr(&.s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_r64(x,&fmt)
	fi

	needgap:=1
end

export proc m$print_r32(real32 x,ichar fmtstyle=nil)=
	m$print_r64(x,fmtstyle)
end

global proc m$print_c8(int64 a,ichar fmtstyle=nil)=
	[16]char s
	int cc@s
	fmtrec fmt
	int n

	nextfmtchars()

	cc:=a	
	s[9]:=0

	n:=getutfsize(s)			!isolate size of next char

	printstr_n(s,n)
!	printstr(s)
	needgap:=1
end

export proc m$print_str(ichar s, fmtstyle=nil)=
	nextfmtchars()

	if s=nil then
		printstr("<null>")
		return
	fi

	fmtrec fmt
	if fmtstyle=nil then
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,-1,&fmt)
	fi
	needgap:=1
end

export proc m$print_strn(ichar s, int length, ichar fmtstyle=nil)=
	nextfmtchars()

	if s=nil then
		printstr("<null>")
		return
	fi

	fmtrec fmt
	if fmtstyle=nil then
		printstr_n(s,length)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,length,&fmt)
	fi
	needgap:=1
end

export proc m$print_str_nf(ichar s)=
	m$print_str(s)
end

export proc m$print_strsl(slice[]char s, ichar fmtstyle=nil)=
	nextfmtchars()
!	fmtrec fmt

	if fmtstyle=nil then
		printstr_n(cast(s.sliceptr),s.len)
!		printstr_n(cast(ss.str),ss.length)
	else
		abortprogram("FORMATED PRINT SLICE NOT READY")
!		strtofmt(fmtstyle,-1,&fmt)
!		tostr_str(s,s.len,&fmt)
	fi
	needgap:=1
end

export proc m$print_newline=
	needgap:=0
	nextfmtchars(1)
	printstr("\w")
end

export proc m$print_nogap=
	needgap:=0
end

export proc m$print_space=
	needgap:=0
	printstr(" ")
end

export proc printstr(ichar s)=
	printstr_n(s,strlen(s))
end

export proc printstr_n(ichar s,int n)=
	ref ref char p

!	return when n=0

	if niostack=1 and outdev in [std_io,file_io] then
!puts("ADDTO BUFF")
		addtobuffer(s,n)
	else
!printf("DUMPSTR %lld\n", n)
		dumpstr(s,n)
	fi
end

export proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
	if f=nil then
		printf("%.*s",length,s)
	else
		fprintf(f,"%.*s",length,s)
	fi
fi
end

proc printchar(int ch)=
	[4]char str

	str[1]:=ch
	str[2]:=0
	printstr_n(str,1)
end

global proc nextfmtchars(int lastx=0)=
	char c
	ref char pstart
	int n
	if not fmtstr then			!format not in use
		if needgap then
			printchar(' ')
		fi
		needgap:=0
		return
	fi

	pstart:=fmtstr
	n:=0

	do
		c:=fmtstr^
		case c
		when '#' then
			if lastx then
				goto skip
			fi
			++fmtstr
			if n then
				printstr_n(pstart,n)
			fi
			return
		when 0 then
			if n then
				printstr_n(pstart,n)
			elsif not lastx then
				printstr_n("|",1)
			fi
			return
		when '~' then
			if n then
				printstr_n(pstart,n)
				n:=0
			fi
			++fmtstr
			c:=fmtstr^
			if c then
				++fmtstr
				printchar(c)
			fi
			pstart:=fmtstr
		else
	skip:
			++n
			++fmtstr
		esac
	od
end

export proc strtofmt(ref char s,int slen,ref fmtrec fmt) =		!PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n	Width
!.n	Max width/precision
!A	Convert to upper case
!a	Convert to lower case
!B	Binary
!C	Show int as single n-bit (unicode) character
!D	Duplicate string returned via STRINT etc on heap
!E,F,G	Specify format for double (corresponds to C format codes)
!F
!G
!H	Hex
!JC	Justify centre
!JL	Justify left
!JR	Justify right
!M	Show int as multi-bit (unicode) character
!M	HEAPMODE???
!O	Octal
!Pc	Use padding char c
!Q	Add double quotes around string (and deal with embedded quotes)
!'	Add single quotes around string (and deal with embedded quotes)
!Sc	Use separator char c between every 3 or 4 digits
!Tc	Use terminator char c (typically B or H)
!U	Show ints as unsigned
!V	For ints, don't display: store value as parameter for subsequent '*'
!W	Unsigned
!Xn	Use base n (n is hex 0 to F)
!Z	Use "0" padding
!+	Always have + or - in front of integers
!~	Quote char is ~
!*	Same as n but uses parameter set with :'V' on previous int

	int c, base
	byte wset
	int n
	[0:100]char str

	fmt^:=defaultfmt

	if s=nil then return fi

	if slen=-1 then slen:=strlen(s) fi

	memcpy(&.str,s,slen)		!convert s/slen to zero-terminated string
	str[slen]:=0
	s:=&.str

	wset:=0
	while s^ do
		c:=s^
		++s
		if c='A' then fmt.lettercase:='A'
		elsif c='a' then fmt.lettercase:='a'
		elseswitch toupper(c)
		when 'B' then fmt.base:=2
		when 'H' then fmt.base:=16
		when 'O' then fmt.base:=8
		when 'X' then
			base:=0
			do
				c:=s^
				if c in '0'..'9' then
					base:=base*10+c-'0'
					++s
				else
					exit
				fi
			od
			if base in 2..16 then
				fmt.base:=base
			fi

		when 'Q' then fmt.quotechar:='"'
		when 'J' then
			fmt.justify:=toupper(s^)
			if s^ then
				++s
			fi
		when 'Z' then fmt.padchar:='0'
		when 'S' then
			fmt.sepchar:=s^
			if s^ then
				++s
			fi
		when 'P' then
			fmt.padchar:=s^
			if s^ then
				++s
			fi
		when 'T' then
			fmt.suffix:=s^
			if s^ then
				++s
			fi
		when 'U' then fmt.usigned:='W'
		when 'E' then fmt.realfmt:='e'
		when 'F' then fmt.realfmt:='f'
		when 'G' then fmt.realfmt:='g'
		when 'D' then fmt.heapmode:='D'
		when 'C' then fmt.charmode:='C'
		when 'M' then fmt.charmode:='M'
		when 'V' then fmt.param:='V'
		when 'Y' then fmt.showtype:=1
		when 'N' then fmt.newline:=1
		elsecase c
		when '.' then
			wset:=1
		when comma,'_' then fmt.sepchar:=c
		when '+' then fmt.plus:='+'
		when '~' then fmt.quotechar:='~'
		when '*' then
			n:=fmtparam
			goto gotwidth
		else
			if c>='0' and c<='9' then
				n:=c-'0'
				do
					c:=s^
					if s^=0 then
						exit
					fi
					if c>='0' and c<='9' then
						++s
						n:=n*10+c-'0'
					else
						exit
					fi
				od
gotwidth:
				if not wset then
					fmt.minwidth:=n
					wset:=1
				else
					fmt.precision:=n
				fi
			fi
		fi
	od
end

function domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
	[0:20]char str
	ref char q
	int i,nchars

	q:=&.str

	nchars:=n

	to n do
		if p^=0 then exit fi
		q^:=p^
		++q
		++p
	od
	q^:=0

	return expandstr(&.str,dest,strlen(&.str),fmt)
end

export function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =		!EXPANDSTR
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string; caller must take care of this
!result will be zero-terminated, for use in this module

	int i,w,m

!check to see if result is acceptable as it is
	w:=fmt.minwidth
	if w=0 or w<=n then		! allow str to be longer than minwidth
		strncpy(t,s,n)
		(t+n)^:=0
		return n
	fi

	if fmt.justify='L' then	! left-justify
		strncpy(t,s,n)
		t+:=n
		for i:=1 to w-n do
			t^:=fmt.padchar
			++t
		od
		t^:=0
	elsif fmt.justify='R' then
		if fmt.padchar='0' and fmt.base and (s^='-' or s^='+') then ! need to move sign outside 
			t^:=s^
			++t
			to w-n do
				t^:=fmt.padchar
				++t
			od
			strncpy(t,s+1,n-1)
			(t+n-1)^:=0
		else
			to w-n do
				t^:=fmt.padchar
				++t
			od
			strncpy(t,s,n)
			(t+n)^:=0
		fi

	else				! centre-justify?

		m:=(w-n+1)/2
		to m do
			t^:=fmt.padchar
			++t
		od
		strncpy(t,s,n)
		t+:=n
		to w-n-m do
			t^:=fmt.padchar
			++t
		od
		t^:=0

	fi
	return w
end

export function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
	[0:onesixty]char t
	u64 dd
	int i,j,k,g
	int cc
	int dummy
	ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
!		if base=10 then			!BUGGY FOR AA OVER I64.MAX
!			assem
!				mov		rcx, [aa]
!				mov		rax, rcx
!				mov		rdx, 7378697629483820647
!				imul	rdx
!				mov		rax, rdx
!				mov		rdx, rcx
!				sar		rdx, 63
!				sar		rax, 2
!				sub		rax, rdx
!				lea		rdx, [rax+rax*4]
!				add		rdx, rdx
!				sub		rcx, rdx
!				mov		[dd], rcx
!				mov		[aa], rax
!			end
!		else
			dd:=aa rem base
			aa:=aa/base
!		fi

		t[++i]:=digits[dd]

!BUG in separator logic, doesn't work when leading zeros used, eg. printing
!out a full length binary
!so perhaps move this out to expandstr
		++k
		if sep and aa<>0 and k=g then
			t[++i]:=sep
			k:=0
		fi
	until aa=0

	j:=i
	s0:=s
	while i do
		s^:=t[i--]
		++s
	od
	s^:=0

	return j
end

export function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt.unsigned forces ints to be treated as longs)
!returns length of s
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w,usigned
	const i64 mindint=0x8000'0000'0000'0000

	usigned:=0
	if fmt.usigned then
		usigned:=1
	fi
	if aa=mindint and not usigned then		! minint

		str[0]:='-'
		n:=i64mintostr(&str[1],fmt.base,fmt.sepchar)+1

	else
		if (not usigned and aa<0) or fmt.plus then
			if aa<0 then
				aa:=-aa
				str[0]:='-'
			else
				str[0]:='+'
			fi
			n:=u64tostr(aa,&str[1],fmt.base,fmt.sepchar)+1
		else
			n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)
		fi
	fi

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if (fmt.base>10 or fmt.suffix) and fmt.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

export function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt.base>10 or fmt.suffix and fmt.lettercase='a'	then	! need lower when
!		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

export function i64mintostr(ref char s,int base,int sep)int =		!I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
	[0:onesixty]char t
	int i,j,k,g,neg

	case base
	when 10 then
		strcpy(&t[0],"9223372036854775808")
		j:=3
	when 16 then
		strcpy(&t[0],"8000000000000000")
		j:=1
	when 2 then
		strcpy(&t[0],"1000000000000000000000000000000000000000000000000000000000000000")
		j:=7
	else
		strcpy(&t[0],"<mindint>")
	esac

	i:=strlen(&t[0])
	s+:=i
	if sep then
		s+:=j
	fi
	s^:=0

	k:=0
	g:=(base=10|3|4)

	while i do
		--s
		s^:=t[i-- -1]
		if sep and i and ++k=g then
			--s
			s^:=sep
			k:=0
		fi
	od
	return strlen(s)
end

export function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
!s is a string process according to fmtrec fmt^, and return result in t
!caller should check whether any changes are required to s (now it can just use s), but this
!check is done here anyway (with a simple copy to t)
!n is current length of s
!return length of t
!Three processing stages:
!1 Basic input string s
!2 Additions or mods: quotes, suffix, when conversion
!3 Width adjustment
!1 is detected here, 2 is done here, 3 is done by expandstr
	ref char u,v
	[256]char str
	int w,nheap		! whether any heap storage is used  bytes allocated

	nheap:=0

	if fmt.quotechar or fmt.lettercase then		! need local copy
		if n<256 then
			u:=&.str
		else
			nheap:=n+3					! allow for quotes+terminator
			u:=pcm_alloc(nheap)
		fi
		if fmt.quotechar then
			v:=u
			v^:=fmt.quotechar
			++v
			if n then
				strcpy(v,s)
				v+:=n
			fi
			v^:=fmt.quotechar
			++v
			v^:=0
			n+:=2
		else
			memcpy(u,s,n)
		fi
		case fmt.lettercase
		when 'a' then	! need lower when
			convlcstring(u)
		when 'A' then
			convucstring(u)
		esac
		s:=u
	fi

	w:=fmt.minwidth
	if w>n then
		n:=expandstr(s,t,n,fmt)
	else
		memcpy(t,s,n)
	fi
	if nheap then
		pcm_free(u,nheap)
	fi
	return n
end

proc tostr_i64(int64 a, ref fmtrec fmt)=
	[360]char str
	int n

!CPL "TOSTR/I64"

	case fmt.charmode
	when 0 then
		n:=i64tostrfmt(a,&.str,fmt)
	when 'M' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	else						!assume 'C'
		m$print_c8(a, nil)
!		printchar(a)			!no other formatting allowed
		return
	esac

	printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
	[360]char str
	int n

	case fmt.charmode
	when 'M' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C' then
		m$print_c8(a, nil)
!		printchar(a)			!no other formatting allowed
		return

	else
		n:=u64tostrfmt(a,&.str,fmt)
	esac

	printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
	[360]char str,str2
	[0:10]char cfmt
	int n

	cfmt[0]:='%'

	if fmt.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt.realfmt
		cfmt[4]:=0
		sprintf(&.str,&.cfmt,fmt.precision,x)
	else
		cfmt[1]:=fmt.realfmt
		cfmt[2]:=0
		sprintf(&.str,&.cfmt,x)
	fi

!at this point, n is the str length including signs and suffix

	n:=strlen(&.str)		! current length

	if n<fmt.minwidth then
		n:=expandstr(&.str,&.str2,n,fmt)
		strcpy(&.str,&.str2)
	fi

	printstr_n(&.str,n)
end

proc tostr_str(ref char s, int oldlen, ref fmtrec fmt) =
	int newlen,n
	ref char t

!try and work out size of formatted string
	if oldlen=-1 then
		oldlen:=strlen(s)
	fi
	newlen:=oldlen

	if fmt.quotechar or fmt.minwidth>newlen or fmt.lettercase or fmt.precision then
		if fmt.quotechar then
			newlen+:=2
		fi
		if fmt.minwidth>newlen then
			newlen:=fmt.minwidth
		fi
		t:=pcm_alloc(newlen+1)
		n:=strtostrfmt(s,t,oldlen,fmt)
		if fmt.precision then
			n min:=fmt.precision
		fi

		printstr_n(t,n)
		pcm_free(t,newlen+1)
	else
		printstr_n(s,oldlen)
	fi
end

function getfmt(ichar fmtstyle)ref fmtrec=
	static fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	fi
end

export function strint(int64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export proc getstrint(int64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

export function strword(word64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export function strreal(real a, ichar fmtstyle=nil)ichar=
	static [320]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_r64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export function getstr(ichar s, ref fmtrec fmt)ichar=
	if fmt.heapmode then
		return pcm_copyheapstring(s)
	else
		return s
	fi
end

proc initreadbuffer=
	if rd_buffer then return fi
	rd_buffer:=pcm_alloc(rd_buffersize)
	rd_buffer^:=0
	rd_pos:=rd_lastpos:=rd_buffer
end

global proc m$read_conline=
	initreadbuffer()

	readlinen(nil,rd_buffer,rd_buffersize)

	rd_length:=strlen(rd_buffer)
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

global proc m$read_fileline(filehandle f)=
	ichar p
	initreadbuffer()

	if f=filehandle(1) then
ABORTPROGRAM("READ CMDLINE")
!		rd_buffer^:=0
!		p:=getcommandlinea()
!		repeat
!			++p
!		until p^ in [' ','\t',0]
!		strcpy(rd_buffer, p)
!		rd_length:=strlen(rd_buffer)
!		rd_pos:=rd_buffer
!		rd_lastpos:=nil
		return
	fi

	readlinen(f,rd_buffer,rd_buffersize)

	rd_length:=strlen(rd_buffer)
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

global proc m$read_strline(ichar s)=
	int n

	initreadbuffer()
	n:=strlen(s)

	if n<rd_buffersize then
		strcpy(rd_buffer,s)
	else
		memcpy(rd_buffer,s,rd_buffersize-1)
		(rd_buffer+rd_buffersize-1)^:=0
	fi
	rd_length:=n
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

function readitem(int &itemlength)ref char =
!read next item from rd_buffer
!identify a substring that can contain a name, int, real, string or filename
!return updated position of s that points past the item and past the immediate
!terminator 
!information about the read item is returned in itemstr, which points to
!the start of the item, and in itemlength. Item excludes any surrounding whitespace
!Item can be quoted, then the item points inside the quotes
!Any embedded quotes are removed, and the characters moved up. The item will
!be that reduced subsequence
!NOTE THAT THIS IS DESTRUCTIVE. On reread, the input will be different.
!I can mitigate this by adding spaces between the end of the item, and the next item,
!overwriting also the terminator. But this won't restore the line if one of the next
!reads is literal, using 'L' or 'C' codes.
	ref char p,s,itemstr
	char quotechar, c

	unless rd_buffer then 
		initreadbuffer()
	end unless

	s:=rd_pos

!scan string, eliminating leading white space
	while s^=' ' or s^=9 do
		++s
	od

	itemstr:=s
	rd_lastpos:=rd_pos:=s

	if s^=0 then
		termchar:=0
		itemlength:=0
		return s
	fi

	quotechar:=0
	if s^='"' then
		quotechar:='"'
		++s
	elsif s^='\'' then
		quotechar:='\''
		++s
	fi

!loop reading characters until separator or end reached
	p:=itemstr:=s

	while s^ do
		c:=s++^
		case c
		when ' ', 9, comma, '=' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			fi
			termchar:=c
			exit
		else
	normalchar:
			if c=quotechar then
				if s^=quotechar then	! embedded quote
					p^:=c
					++s
					++p
				else					! end of name
					termchar:=s^
					if termchar in [',', '='] then
						++s
						termchar:=s^
					fi
					exit
				fi
			else
				p^:=c
				++p
			fi
		esac
	od

	if s^=0 then
		termchar:=0
	fi
	itemlength:=p-itemstr				! actual length of token
	rd_pos:=s

	return itemstr
end

export function strtoint(ichar s,int length=-1, word base=10)int64=
!return point to next char after terminator (which can be just off length of string)
	byte signd
	word64 aa
	word c,d

	itemerror:=0

	if length=-1 then
		length:=strlen(s)
	fi
!check for sign
	signd:=0
	if length and s^='-' then
		signd:=1; ++s; --length
	elsif length and s^='+' then
		++s; --length
	fi

	aa:=0
	while length do
		c:=s++^
		--length
		if c in 'A'..'F' then d:=c-'A'+10
		elsif c in 'a'..'f' then d:=c-'a'+10
		elsif c in '0'..'9' then d:=c-'0'
		elsif c in ['_', '\''] then
			nextloop
		else
			itemerror:=1
			exit
		fi

		if d>=base then
			itemerror:=1
			exit
		fi
		aa:=aa*base+d
	od

	if signd then
		return -aa
	else
		return aa
	fi
end

global function m$read_i64(int fmt=0)int64=
	ref char s
	int length,c
	int64 aa

	fmt:=toupper(fmt)

	case fmt
	when 'C' then
		rd_lastpos:=rd_pos
		if rd_pos^ then
			return rd_pos++^
		else
			return 0
		fi
	when 'T' then
		return termchar
	when 'E' then
		return itemerror
	esac

	s:=readitem(length)

	case fmt
	when 0,'I' then
		return strtoint(s,length)
	when 'B' then
		return strtoint(s,length,2)
	when 'H' then
		return strtoint(s,length,16)
	esac
	return 0
end

global function m$read_r64(int fmt=0)real=
	[512]char str
	ref char s
	int length
	int32 numlength
	real x

	s:=readitem(length)

	if length=0 or length>=str.len then		!assume not a real
		return 0.0
	fi
	memcpy(&.str,s,length)
	str[length+1]:=0

	itemerror:=0

	if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
		x:=0.0
		itemerror:=1
	fi

	return x
end

global proc m$read_str(ref char dest, int destlen=0,fmt=0)=
	ref char s
	int length,numlength
	real x

	itemerror:=0
	if fmt in ['L','l'] then
		s:=rd_pos
		length:=rd_buffer+rd_length-rd_pos

	else
		s:=readitem(length)

		if fmt in ['N','n'] then
			iconvlcn(s,length)
		fi
	fi

	if destlen>0 then
		if length>=destlen then
			length:=destlen-1
			itemerror:=1
		fi
	fi
	memcpy(dest,s,length)
	(dest+length)^:=0
end

export proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_str(dest,destlen,fmt)
end

export proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

export proc reread=
	rd_pos:=rd_lastpos
end

export function valint(ichar s, int fmt=0)int64=
	ref char old_pos, old_lastpos
	int64 aa

	initreadbuffer()
	old_pos:=rd_pos
	old_lastpos:=rd_lastpos

	rd_pos:=s
	aa:=m$read_i64(fmt)
	rd_pos:=old_pos
	rd_lastpos:=old_lastpos
	return aa
end

export function valreal(ichar s)real=
	ref char old_pos, old_lastpos
	real x

	initreadbuffer()
	old_pos:=rd_pos
	old_lastpos:=rd_lastpos

	rd_pos:=s
	x:=m$read_r64()
	rd_pos:=old_pos
	rd_lastpos:=old_lastpos
	return x
end

proc mclunimpl(ichar mess)=
	printf("MCL-UNIMPL: %s\n",mess)
	stop 1
end

proc dumpstr(ichar s, int n, fbuffer=0)=
!fbuffer=1 when outputting contents of buffer

	ref ref char p

	if outdev=str_io then
		p:=cast(outchan)
		if n then
			memcpy(p^,s,n)
			p^+:=n
		fi
		p^^:=0
		return
	fi

	return when n=0
	if fbuffer and n>=2 and outdev=std_io then
		--printptr				!point to last char
		if printptr^=10 then
			if (printptr-1)^=13 then		!crlf
				(printptr-1)^:=0
			else							!lf only
				printptr^:=0
			fi
			puts(printbuffer)
			return
		fi
	fi

	case outdev
	when std_io then
		printf("%.*s",n,s)
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	esac
end

proc dumpprintbuffer=
	if printlen then
		dumpstr(&.printbuffer,printlen,1)
	fi

	resetprintbuffer()
end

proc resetprintbuffer=
	printptr:=&.printbuffer
	printlen:=0
end

proc addtobuffer(ichar s, int n)=
	if printlen+n>=(printbuffer.len-8) then
		dumpprintbuffer()
	fi

	if n<printbuffer.len then
		memcpy(printptr,s,n)
		printptr+:=n
		printlen+:=n
		return
	fi

	dumpstr(s, n)			!don't bother with buffer
end

global function m$power_i64(int64 a,n)int64=
	if n<0 then
		return 0
	elsif n=0 then
		return 1
	elsif n=1 then
		return a
	elsif (n iand 1)=0 then
		return m$power_i64(sqr a,n/2)
	else			!assume odd
		return m$power_i64(sqr a,(n-1)/2)*a
	fi
end

func getutfsize(ref char s)int =
!work out the size in bytes of the ascii or utf8 character that s points to
	int a

	a:=s++^

	if a=0 then						!end of string
		0
	elsif a.[7]=0 then				!ascii
		1
	elsif a.[7..5]=2x110 then
		2
	elsif a.[7..4]=2x1110 then
		3
	elsif a.[7..3]=2x11110 then
		4
	else							!error: just assume a byte of random binary
		1
	fi
end

!export fun `fract(real x)real = fmod(x,1.0)
export fun fraction(real x)real = fmod(x,1.0)

export fun m$sign_i64(int a)int = (a<0|-1| (a>0|1|0))
export func m$sign_r64(real x)real =
	if x<0 then return -1 fi
	if x>0 then return 1 fi
	0
end
=== mlib.m 0 1 35/38 ===
!const mem_check=1
const mem_check=0

global [0..300]u64 allocupper
global int alloccode				!set by heapalloc
export int allocbytes				!set by heapalloc
export int fdebug=0
export int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

!GLOBAL REF VOID ALLOCBASE

byte pcm_setup=0

int show=0

global int memtotal=0
export int64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=(mem_check|500000|2)
[maxmemalloc+1]ref int32 memalloctable
[maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend			!points to first address past heap
ref byte pcheapptr

const int maxblockindex = 8 		!2048
export const int maxblocksize = 2048
export const int $maxblocksizexx = 2048

[0:maxblocksize+1]byte sizeindextable	!convert byte size to block index 1..maxblockindex

const int size16   = 1			!the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

export [0:9]ref word freelist

export record strbuffer =
	ichar strptr
	int32 length
	int32 allocated
end

export enumdata [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

[2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

!PROC START=
!CPL "MLIB START"
!END


export function pcm_alloc(int n)ref void =
	ref byte p


	if not pcm_setup then
		pcm_init()
	fi

!GOTO DOLARGE

	if n>maxblocksize then			!large block allocation
!DOLARGE:
		alloccode:=pcm_getac(n)
		allocbytes:=allocupper[alloccode]

		p:=allocmem(allocbytes)
		if not p then
			abortprogram("pcm_alloc failure")
		fi

		return p
	fi

!CPL "DOSMALL"

	alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc
	allocbytes:=allocupper[alloccode]
	smallmemtotal+:=allocbytes

	if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
		freelist[alloccode]:=ref word(int((freelist[alloccode])^))

		return p
	fi

!No items in freelists: allocate new space in this heap block
	p:=pcheapptr				!Create item at start of remaining pool in heap block
	pcheapptr+:=allocbytes			!Shrink remaining pool

	if pcheapptr>=pcheapend then		!Overflows?
		p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
		return p
	fi

	return p
end

export proc pcm_free(ref void p,int n) =
!n can be the actual size requested it does not need to be the allocated size
	int acode

	if n=0 then return fi

!GOTO DOLARGE

	if n>maxblocksize then		!large block
!DOLARGE:
!CPL "DOL"
		free(p)
		return
	fi

!CPL "DOSMALL"
	if p then
		acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

		smallmemtotal-:=allocupper[acode]
!
		cast(p,ref word)^:=word(int(freelist[acode]))
		freelist[acode]:=p
	fi
end

export proc pcm_freeac(ref void p,int alloc) =
	pcm_free(p,allocupper[alloc])
end

export proc pcm_clearmem(ref void p,int n) =
	memset(p,0,n)
end

export proc pcm_init =
!set up sizeindextable too
	int j,k,k1,k2
	int64 size
	const limit=1<<33

	alloccode:=0
	if pcm_setup then
		return
	fi

	pcm_newblock(0)

	for i to maxblocksize do	!table converts eg. 78 to 4 (4th of 16,32,64,128)
		j:=1
		k:=16
		while i>k do
			k:=k<<1
			++j
		od
		sizeindextable[i]:=j
	od

	allocupper[1]:=16
	size:=16

	for i:=2 to 27 do
		size*:=2
		allocupper[i]:=size
		if size>=threshold then
				k:=i
			exit
		fi
	od

	for i:=k+1 to allocupper.upb do
		size+:=alloc_step
		if size<limit then
			allocupper[i]:=size
			maxmemory:=size
		else
			maxalloccode:=i-1
			exit
		fi
		
	od
	pcm_setup:=1
end

export function pcm_getac(int size)int =
! convert linear blocksize from 0..approx 2GB to 8-bit allocation code

!sizeindextable scales values from 0 to 2048 to allocation code 0 to 9

	if size<=maxblocksize then
		return sizeindextable[size]		!size 0 to 2KB
	fi

	size:=(size+255)>>8					!scale by 256

!now same sizetable can be used for 2KB to 512KB (288 to 2KB)

	if size<=maxblocksize then
		return sizeindextable[size]+8
	fi

!sizetable now used for 512KB to 128MB (to 2KB)
	size:=(size+63)>>6					!scale by 256

	if size<=maxblocksize then
		return sizeindextable[size]+14
	fi

!size>2048, which means it had been over 128MB.
	size:=(size-2048+2047)/2048+22
	return size
end

export function pcm_newblock(int itemsize)ref void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
	static int totalheapsize
	ref byte p

	totalheapsize+:=pcheapsize
	alloccode:=0
	p:=allocmem(pcheapsize)	!can't free this block until appl terminates
	if p=nil then
		abortprogram("Can't alloc pc heap")
	fi
	memset(p,0,pcheapsize)

	pcheapptr:=p
	pcheapend:=p+pcheapsize

	if pcheapstart=nil then		!this is first block
		pcheapstart:=p
	fi
	pcheapptr+:=itemsize
	return ref u32(p)
end

export function pcm_round(int n)int =
!for any size n, return actual number of bytes that would be allocated
	static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

	if n>maxblocksize then
		return n
	else
		return allocbytes[sizeindextable[n]]
	fi
end

export function pcm_allocz(int n)ref void =
	ref void p
	p:=pcm_alloc(n)

	memset(p,0,n)
	return p
end

export function pcm_copyheapstring(ref char s)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	ref char q
	int n
	if s=nil then return nil fi

	n:=strlen(s)+1
	q:=pcm_alloc(n)
	memcpy(q,s,n)
	return q
end

export function pcm_copyheapstringn(ref char s,int n)ref char =
	ref char q
	if s=nil then return nil fi

	q:=pcm_alloc(n+1)
	memcpy(q,s,n)
	(q+n)^:=0
	return q
end

export function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	ref char q
	if length=0 then return nil fi

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

export function allocmem(int n)ref void =
	ref void p

	p:=malloc(n)
	if p then
		return p
	fi
	println n,memtotal
	abortprogram("Alloc mem failure")
	return nil
end

global function reallocmem(ref void p,int n)ref void =
	p:=realloc(p,n)
	return p when p
	println n
	abortprogram("Realloc mem failure")
	return nil
end

export proc abortprogram(ref char s) =
	println s
	print   "ABORTING: Press key..."
!os_getch()
	stop 5
end

export function getfilesize(filehandle handlex)int=
	word32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

export proc readrandom(filehandle handlex, ref byte mem, int offset, size) =
	int a
	fseek(handlex,offset,seek_set)
	a:=fread(mem,1,size,handlex)			!assign so as to remove gcc warning
end

export function writerandom(filehandle handlex, ref byte mem, int offset,size)int =
	fseek(handlex,offset,seek_set)
	return fwrite(mem,1,size,handlex)
end

export function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

export function getfilepos(filehandle file)int=
	return ftell(file)
end

export function readfile(ref char filename)ref byte =
	filehandle f
	int size
	ref byte m,p

	f:=fopen(filename,"rb")
	if f=nil then
		return nil
	fi
	rfsize:=size:=getfilesize(f)

	m:=malloc(size+2)		!allow space for etx/zeof etc

	if m=nil then
		return nil
	fi

	readrandom(f,m,0,size)
	p:=m+size			!point to following byte
	(ref u16(p)^:=0)	!add two zero bytes

	fclose(f)
	return m
end

export function writefile(ref char filename,ref byte data,int size)int =
	filehandle f
	int n

	f:=fopen(filename,"wb")
	if f=nil then
		return 0
	fi

	n:=writerandom(f,data,0,size)
	fclose(f)
	return n
end

export function checkfile(ref char file)int=
	filehandle f
	if f:=fopen(file,"rb") then
		fclose(f)
		return 1
	fi
	return 0
end

export proc readlinen(filehandle handlex,ref char buffer,int size) =
!size>2
	int ch
	ref char p
	int n
	[0:100]char buff
	byte crseen

	if handlex=nil then
		handlex:=filehandle(os_getstdin())
	fi
	if handlex=nil then
		n:=0
		p:=buffer
		do
			ch:=getchar()
			if ch=13 or ch=10 or ch=-1 then
				p^:=0
				return
			fi
			p++^:=ch
			++n
			if n>=(size-2) then
				p^:=0
				return
			fi
		od
	fi

	buffer^:=0
	if fgets(buffer,size-2,handlex)=nil then
		return
	fi

	n:=strlen(buffer)
	if n=0 then
		return
	fi

	p:=buffer+n-1		!point to last char
	crseen:=0
	while (p>=buffer and (p^=13 or p^=10)) do
		if p^=13 or p^=10 then crseen:=1 fi
		p--^ :=0
	od

!NOTE: this check doesn't work when a line simply doesn't end with cr-lf

	if not crseen and (n+4>size) then
		cpl size,n
		abortprogram("line too long")
	fi
end

export proc iconvlcn(ref char s,int n) =
	to n do
		s^:=tolower(s^)
		++s
	od
end

export proc iconvucn(ref char s,int n) =
	to n do
		s^:=toupper(s^)
		++s
	od
end

export function convlcstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=tolower(s^)
		++s
	od
	s0
end

export function convucstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=toupper(s^)
		++s
	od
	s0
end

export function changeext(ref char s,newext)ichar=
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
	static [260]char newfile
	[32]char newext2
	ref char sext
	int n

	strcpy(&newfile[1],s)

	case newext^
	when 0 then
		newext2[1]:=0
		newext2[2]:=0
	when '.' then
		strcpy(&newext2[1],newext)
	else
		strcpy(&newext2[1],".")
		strcat(&newext2[1],newext)
	esac


	sext:=extractext(s,1)			!include "." when it is only extension

	case sext^
	when 0 then						!no extension not even "."
		strcat(&newfile[1],&newext2[1])
	when '.' then						!no extension not even "."
		strcat(&newfile[1],&newext2[2])
	else							!has extension
		n:=sext-s-2			!n is number of chars before the "."
		strcpy(&newfile[1]+n+1,&newext2[1])
	esac

	return &newfile[1]
end

export function extractext(ref char s,int period=0)ichar=
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
	ref char t,u

	t:=extractfile(s)

	if t^=0 then			!s contains no filename
		return ""
	fi

!t contains filename+ext
	u:=t+strlen(t)-1		!u points to last char of t

	while u>=t do
		if u^='.' then		!start extension found
			if (u+1)^=0 then		!null extension
				return (period|"."|"")
			fi
			return u+1			!return last part of filename as extension exclude the dot
		fi
		--u
	od
	return ""			!no extension seen
end

export function extractpath(ref char s)ichar=
	static [0:260]char str
	ref char t
	int n

	t:=s+strlen(s)-1		!t points to last char

	while (t>=s) do
		case t^
		when '\\','/',':' then		!path separator or drive letter terminator assume no extension
			n:=t-s+1			!n is number of chars in path, which includes rightmost / or \ or :
			memcpy(&.str,s,n)
			str[n]:=0
			return &.str
		esac
		--t
	od
	return ""			!no path found
end

export function extractfile(ref char s)ichar=
	ref char t

	t:=extractpath(s)

	if t^=0 then			!s contains no path
		return s
	fi

	return s+strlen(t)		!point to last part of s that contains the file
	end

export function extractbasefile(ref char s)ichar=
	static [0:100]char str
	ref char f,e
	int n,flen

	f:=extractfile(s)
	flen:=strlen(f)
	if flen=0 then		!s contains no path
		return ""
	fi
	e:=extractext(f,0)

	if e^ then			!not null extension
		n:=flen-strlen(e)-1
		memcpy(&str,f,n)
		str[n]:=0
		return &.str
	fi
	if (f+flen-1)^='.' then
		memcpy(&str,f,flen-1)
		str[flen-1]:=0
		return &.str
	fi
	return f
end

export function addext(ref char s,ref char newext)ichar=
!when filespec has no extension of its own, add newext
	ref char sext

	sext:=extractext(s,1)

	if sext^=0 then						!no extension not even "."
		return changeext(s,newext)
	fi

	return s							!has own extension; use that
end

export function pcm_alloc32:ref void =
	ref byte p

	allocbytes:=32
	smallmemtotal+:=32

	if p:=ref byte(freelist[2]) then		!Items of this block size available
		freelist[2]:=ref word(int((freelist[2])^))
		return p
	fi

!No items in freelists: allocate new space in this heap block
	return pcm_alloc(32)
end

export proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

	smallmemtotal-:=32

	cast(p,ref word)^:=word(int(freelist[2]))
	freelist[2]:=p
end

export proc outbyte(filehandle f,int x)=
	fwrite(&x,1,1,f)
end

export proc outword16(filehandle f,word x)=
	fwrite(&x,2,1,f)
end

export proc outword32(filehandle f,word x)=
	fwrite(&x,4,1,f)
end

export proc outword64(filehandle f,word64 x)=
	fwrite(&x,8,1,f)
end

export proc outstring(filehandle f, ichar s)=
	fwrite(s,strlen(s)+1,1,f)
end

export proc outblock(filehandle f, ref void p, int n)=
	fwrite(p,n,1,f)
end

export function myeof(filehandle f)int=
	int c

	c:=fgetc(f)
	if c=c_eof then return 1 fi
	ungetc(c,f)
	return 0;
end

export proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
	int newlen,oldlen
	ichar newptr

!	IF N=0 THEN CPL "N=0" FI

	if n=-1 then
		n:=strlen(s)
	fi

	oldlen:=dest.length

	if oldlen=0 then				!first string
		dest.strptr:=pcm_alloc(n+1)
		dest.allocated:=allocbytes
		dest.length:=n				!length always excludes terminator
		memcpy(dest.strptr,s,n)
		(dest.strptr+n)^:=0
		return
	fi

	newlen:=oldlen+n
	if newlen+1>dest.allocated then
		newptr:=pcm_alloc(newlen+1)
		memcpy(newptr,dest.strptr,oldlen)
		dest.strptr:=newptr
		dest.allocated:=allocbytes
	fi

	memcpy(dest.strptr+oldlen,s,n)
	(dest.strptr+newlen)^:=0

	dest.length:=newlen
end

export proc gs_init(ref strbuffer dest)=
	pcm_clearmem(dest,strbuffer.bytes)
end

export proc gs_free(ref strbuffer dest)=
	if dest.allocated then
		pcm_free(dest.strptr,dest.allocated)
	fi
end

export proc gs_str(ref strbuffer dest,ichar s)=
	strbuffer_add(dest,s)
end

export proc gs_char(ref strbuffer dest,int c)=
	[16]char s

	s[1]:=c
	s[2]:=0

	strbuffer_add(dest,&.s,1)
end

export proc gs_strn(ref strbuffer dest,ichar s,int length)=
	strbuffer_add(dest,s,length)
end

export proc gs_strvar(ref strbuffer dest,s)=
	strbuffer_add(dest,s.strptr)
end

export proc gs_strint(ref strbuffer dest,int64 a)=
	strbuffer_add(dest,strint(a))
end

export proc gs_strln(ref strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

export proc gs_strsp(ref strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_str(dest," ")
end

export proc gs_line(ref strbuffer dest)=
	strbuffer_add(dest,"\w")
end

export function gs_getcol(ref strbuffer dest)int=
	return dest.length
end

export proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
	int col,i,n,slen
	[2560]char str
	col:=dest.length
	strcpy(&.str,s)
	slen:=strlen(s)
	n:=w-slen
	if n>0 then
		for i:=1 to n do
			str[slen+i]:=padch
		od
		str[slen+n+1]:=0
	fi
	gs_str(dest,&.str)
end

export proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
	gs_leftstr(dest,strint(a),w,padch)
end

export proc gs_padto(ref strbuffer dest,int col, ch=' ')=
	int n
	[2560]char str

	n:=col-dest.length
	if n<=0 then return fi
	for i:=1 to n do
		str[i]:=ch
	od
	str[n+1]:=0
	gs_str(dest,&.str)
end

export proc gs_println(ref strbuffer dest,filehandle f=nil)=
	if dest.length=0 then return fi
	(dest.strptr+dest.length)^:=0

	if f=nil then
		println dest.strptr,,"\c"
	else
		println @f,dest.strptr,,"\c"
	fi
end

export function nextcmdparamnew(int &paramno, ichar &name, &value, ichar defext=nil)int=
	static int infile=0
	static ichar filestart=nil
	static ichar fileptr=nil
	static byte colonseen=0
	ref char q
	ichar item,fileext
	ichar rest
	int length
	static [300]char str

	reenter:
	value:=nil
	name:=nil

	if infile then
		if readnextfileitem(fileptr,item)=0 then		!eof
			free(filestart)								!file allocated via malloc
			infile:=0
			goto reenter
		fi
	else
		if paramno>ncmdparams then
			return pm_end
		fi
		item:=cmdparams[paramno]
		++paramno

		length:=strlen(item)

		if item^='@' then		!@ file
			filestart:=fileptr:=readfile(item+1)
			if filestart=nil then
				println "Can't open",item
				stop 7
			fi
			infile:=1
			goto reenter
		fi

		if item^=':' then
			colonseen:=1
			return pm_colon
		fi
	fi

	value:=nil
	if item^='-' then
		name:=item+(colonseen|0|1)
		q:=strchr(item,':')
		if not q then
			q:=strchr(item,'=')
		fi
		if q then
			value:=q+1
			q^:=0
		fi
		return (colonseen|pm_extra|pm_option)
	fi

	fileext:=extractext(item,0)
	name:=item

	if fileext^=0 then							!no extension
		strcpy(&.str,name)
		if defext and not colonseen then
			name:=addext(&.str,defext)				!try .c
		fi
!	elsif eqstring(fileext,"dll") then
	elsif eqstring(fileext,"dll") or eqstring(fileext,"mcx") then
		return (colonseen|pm_extra|pm_libfile)
	fi
	return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
	ref char p,pstart,pend
	int n
	static [256]char str

	p:=fileptr

	reenter:
	do
		case p^
		when ' ','\t',13,10 then	!skip white space
			++p
		when 26,0 then				!eof
			return 0
		else
			exit
		esac
	od

	case p^
	when '!', '#' then			!comment
		++p
		docase p++^
		when 10 then
			goto reenter
		when 26,0 then
			fileptr:=p-1
			return 0
		else

		end docase
	esac


	case p^
	when '"' then				!read until closing "
		pstart:=++p
		do
			case p^
			when 0,26 then
				println "Unexpected EOF in @file"
				stop 8
			when '"' then
				pend:=p++
				if p^=',' then ++p fi
				exit
			esac
			++p
		od
	else
		pstart:=p
		do
			case p^
			when 0,26 then
				pend:=p
				exit
			when ' ','\t',',',13,10 then
				pend:=p++
				exit
			esac
			++p
		od
	esac

	n:=pend-pstart
	if n>=str.len then
		println "@file item too long"
		stop 9
	fi
	memcpy(&.str,pstart,n)
	str[n+1]:=0
	item:=&.str
	fileptr:=p

	return 1
end

export proc ipadstr(ref char s,int width,ref char padchar=" ")=
	int n

	n:=strlen(s)
	to width-n do
		strcat(s,padchar)
	od
end

export function padstr(ref char s,int width,ref char padchar=" ")ichar=
	static [256]char str

	strcpy(&.str,s)
	ipadstr(&.str,width,padchar)
	return &.str
end

export function chr(int c)ichar=
	static [8]char str

	str[1]:=c
	str[2]:=0
	return &.str
end

export function cmpstring(ichar s,t)int=
	int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function cmpstringn(ichar s,t,int n)int=
	int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

export function cmpbytes(ref void p,q,int n)int=
	int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function eqbytes(ref void p,q,int n)int=
	return memcmp(p,q,n)=0
end

export proc mseed(word64 a,b=0)=
	seed[1]:=a
	if b then
		seed[2]:=b
	else
		seed[2] ixor:=a
	fi
end

export function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
!	word64 x,y
	int x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

export function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

export function mrandomint(int n)int=
!positive random int value from 0 to n-1
	return mrandomp() rem n
end

export function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
	int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

export function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

export function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
	return mrandomp()/9223372036854775807.0
end

export function readline:ichar=
	readln
	return rd_buffer
end

export function findfunction(ichar name)ref void=
	for i to $getnprocs() do
		if eqstring($getprocname(i),name) then
			return $getprocaddr(i)
		fi
	od
	return nil
end

export function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
	if n iand (align-1)=0 then return n fi
	return n+(align-(n iand (align-1)))
end

export function pcm_allocnfz(int n)ref void =
!non-freeing allocator for small objects
!n should be a multiple of 8 bytes, but is rounded up here if needed
	ref byte p

!make n a multiple of 8
	if n iand 7 then
		n:=n+(8-(n iand 7))
	fi

	p:=pcheapptr					!Create item at start of remaining pool in heap block
	pcheapptr+:=n					!Shrink remaining pool

	if pcheapptr>=pcheapend then	!Overflows?
		p:=pcm_newblock(n)			!Create new heap block, and allocate from start of that
	fi

	return p
end

!export proc freddy=
!	PRINTLN "FREDDY"
!end
=== mclib.m 0 1 36/38 ===
export type filehandle=ref void

importdll $cstd=
	func malloc		(word64)ref void
	func realloc	(ref void, word)ref void
	proc free		(ref void)
	proc memset		(ref void, int32, word)
	proc memcpy		(ref void, ref void, word)
	proc memmove		(ref void, ref void, word)
	func clock		:int32
	func ftell		(filehandle)int32
	func fseek		(filehandle, int32, int32)int32
	func fread		(ref void, word, word, filehandle)word
	func fwrite		(ref void, word, word, filehandle)word
	func getc		(filehandle)int32
	func ungetc		(int32, filehandle)int32
	func fopen		(ichar a, b="rb")filehandle
	func fclose		(filehandle)int32
	func fgets		(ichar, int, filehandle)ichar
	func remove		(ichar)int32
	func rename		(ichar, ichar)int32
	func getchar	:int32
	proc putchar	(int32)
	proc setbuf		(filehandle, ref byte)

	func strlen		(ichar)int
	func strcpy		(ichar, ichar)ichar
	func strcmp		(ichar, ichar)int32
	func strncmp	(ichar, ichar, word)int32
	func strncpy	(ichar, ichar, word)word
	func memcmp		(ref void, ref void, word)int32
	func strcat		(ichar, ichar)ichar
	func tolower	(int32)int32
	func toupper	(int32)int32
	func isalpha	(int32)int32
	func isupper	(int32)int32
	func islower	(int32)int32
	func isalnum	(int32)int32
	func isspace	(int32)int32
	func strstr		(ichar, ichar)ichar
	func atol		(ichar)int
	func atoi		(ichar)int32
	func strtod		(ichar,ref ref char)real64
	func _strdup	(ichar)ichar

	func puts		(ichar)int32
	func printf		(ichar, ...)int32

	func sprintf	(ichar, ichar, ...)int32

	func sscanf		(ichar, ichar, ...)int32
	func scanf		(ichar, ...)int32

	func rand		:int32
	proc srand		(word32)
	func system		(ichar)int32

	func fgetc		(filehandle)int32
	func fputc		(int32,  filehandle)int32
	func fprintf	(filehandle, ichar, ...)int32
	func fputs		(ichar,  filehandle)int32
	func feof		(filehandle)int32
	func getch		:int32
	func _getch		:int32
	func kbhit		:int32
	func _mkdir		(ichar)int32
	func mkdir		(ichar)int32
	func strchr		(ichar,int32)ichar

	func _setmode	(int32,int32)int32

	proc _exit		(int32)
	proc "exit"		(int32)
!	proc `exit		(int32)
	func pow		(real,real)real

	func `sin 		(real)real
	func `cos		(real)real
	func `tan		(real)real
	func `asin		(real)real
	func `acos		(real)real
	func `atan 		(real)real
	func `log		(real)real
	func `log10		(real)real
	func `exp		(real)real
	func `floor		(real)real
	func `ceil		(real)real

	proc  qsort   	(ref void, word64, word64, ref proc)

end

export macro strdup=_strdup

importdll $cstdextra=
	func __getmainargs	(ref int32, ref void, ref void, int, ref void)int32
end

export const c_eof		=-1
export const seek_set	= 0
export const seek_curr	= 1
export const seek_end	= 2
=== mwindows.m 0 1 37/38 ===
const wm_destroy=2

export type wt_word	= word16
export type wt_wordpm	= word32
export type wt_bool	= word32
export type wt_dword	= word32
export type wt_wchar	= word16
export type wt_wcharpm	= word32
export type wt_char	= byte
export type wt_ichar	= ref char
export type wt_ptr		= ref void
export type wt_wndproc	= ref proc
export type wt_handle	= ref void
export type wt_int		= int32
export type wt_uint	= word32
export type wt_long	= int32
export type wt_wparam	= word
export type wt_lparam	= word
export type wt_point	= rpoint

export record rsystemtime =
	wt_word year
	wt_word month
	wt_word dayofweek
	wt_word day
	wt_word hour
	wt_word minute
	wt_word second
	wt_word milliseconds
end

importdll $windowsdlls=
!	func "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	func "GetStdHandle"(wt_dword)wt_handle
	func "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	func "SetConsoleCtrlHandler"(wt_wndproc,int)int
	func "SetConsoleMode"(wt_handle,wt_dword)int
	func "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	func "GetLastError":wt_dword
	func "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	func "GetExitCodeProcess"(wt_handle,wt_ptr)int
	func "CloseHandle"(wt_handle)int
	func "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	func "FlushConsoleInputBuffer"(wt_handle)int
	func "LoadLibraryA"(wt_ichar)wt_handle
!	func "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	func "GetProcAddress"(wt_handle,wt_ichar)ref void
	func "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	func "RegisterClassExA"(wt_ptr)wt_wordpm
	func "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)int
	func "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	proc "Sleep"(wt_dword)
	func "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	proc "ExitProcess"(wt_uint)
	proc "PostQuitMessage"(wt_int)

	proc "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	func "QueryPerformanceCounter"(ref int64)wt_bool
	func "QueryPerformanceFrequency"(ref int64)wt_bool

	func "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	func "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	proc "GetSystemTime"(ref rsystemtime)
	proc "GetLocalTime"(ref rsystemtime)

	func "GetTickCount64":u64
	func "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

	func "GetCommandLineA":ichar

	func "VirtualAlloc" (ref void, wt_dword, wt_dword, wt_dword)ref void
	func "VirtualProtect" (ref void, wt_dword, wt_dword, ref wt_dword)wt_bool

	func "WriteConsoleA" (ref void, ref void, i32, ref i32, ref void)wt_bool

	func "FindFirstFileA" (wt_ichar,ref rfinddata)wt_handle
	func "FindNextFileA"  (wt_handle, ref rfinddata)wt_bool
	func "FindClose"      (wt_handle)wt_bool
end

record input_record = $caligned
	wt_word	eventtype
!	word16	padding
		wt_bool	keydown			!key event record (was inside 'Event' union in win32)
		wt_word	repeatcount
		wt_word	virtualkeycode
		wt_word	virtualscancode
		union
			wt_word unicodechar
			wt_char asciichar
		end
		wt_dword controlkeystate
end

record rspoint=(int16 x,y)

record rsrect=
	int16 leftx,top,rightx,bottom
end

global record rpoint =
	wt_long x,y
end

record rconsole=
	rspoint size,pos
	word16 attributes
	rsrect window
	rspoint maxwindowsize
end

record rstartupinfo =
	wt_dword	size
	word32 dummy1
	wt_ichar	reserved
	wt_ichar	desktop
	wt_ichar	title
	wt_dword	x
	wt_dword	y
	wt_dword	xsize
	wt_dword	ysize
	wt_dword	xcountchars
	wt_dword	ycountchars
	wt_dword	fillattribute
	wt_dword	flags
	wt_word		showwindow
	wt_word		reserved2
	word32 dummy2
	wt_ptr		reserved4
	wt_handle	stdinput
	wt_handle	stdoutput
	wt_handle	stderror
end

record rprocess_information =
	wt_handle process
	wt_handle thread
	wt_dword processid
	wt_dword threadid
end

record rwndclassex =
	wt_uint		size
	wt_uint		style
	wt_wndproc	wndproc
	wt_int		clsextra
	wt_int		wndextra
	wt_handle	instance
	wt_handle	icon
	wt_handle	cursor
	wt_handle	background
	wt_ichar	menuname
	wt_ichar	classname
	wt_handle	iconsm
end

global record rmsg =
	wt_handle	hwnd
	wt_uint		message
	word32		dummy1
	wt_wparam	wParam
	wt_lparam	lParam
	wt_dword	time
	word32		dummy2
	wt_point	pt
end

record rfiletime =
	wt_dword lowdatetime
	wt_dword highdatetime
end

record rfinddata =
	wt_dword	fileattributes
	rfiletime	creationtime
	rfiletime	lastaccesstime
	rfiletime	lastwritetime
	wt_dword	filesizehigh
	wt_dword	filesizelow
	wt_dword	reserved0
	wt_dword	reserved1
	[260]char	filename
	[14]char		altfilename
	wt_dword	obs1, obs2
	wt_word		obs3
end

const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

const MEM_COMMIT				= 4096
const MEM_RESERVE				= 8192
const PAGE_EXECUTE				= 16
const PAGE_EXECUTE_READ			= 32
const PAGE_EXECUTE_READWRITE	= 64
const PAGE_NOACCESS				= 1


export wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

ref func (ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

export proc os_init=
	int i,count
	rconsole info

!general initialisation
	hconsole:=GetStdHandle(u32(-11))
	hconsolein:=GetStdHandle(u32(-10))

	lastkey.repeatcount:=0
	keypending:=0

	SetConsoleCtrlHandler(nil,1)

	SetConsoleMode(hconsole,1 ior 2)

	init_flag:=1

end

export func os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	wt_dword exitcode
	int status
	int cflags:=0

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	case newconsole
	when 0 then cflags := NORMAL_PRIORITY_CLASS
	when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
	when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
	esac

	si.size := rstartupinfo.bytes

CPL "EXECWAIT:",CMDLINE

	status:=CreateProcessA(
		nil,
		cmdline,
		nil,

		nil,
		1,
		cflags,

		nil,
		nil,
		&si,
		&xpi )

	if status=0 then		!fails
		status:=GetLastError()
		printf("Winexec error: %lld\n",status)
		return -1
	fi

	WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
	GetExitCodeProcess(xpi.process,&exitcode)

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return exitcode
end

export func os_execcmd(ichar cmdline, int newconsole=0)int =
	wt_dword exitcode
	int i,j,k

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

	si.size := rstartupinfo.bytes

	CreateProcessA( nil,
		cmdline,
		nil,
		nil,
		1,
		NORMAL_PRIORITY_CLASS ior (newconsole|CREATE_NEW_CONSOLE|0),
		nil,
		nil,
		&si,
		&xpi )

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return 1
end

export func os_getch:int=
	int k

	k:=os_getchx() iand 255

	return k
end

export func os_kbhit:int=
	wt_dword count

	unless init_flag then os_init() end

	GetNumberOfConsoleInputEvents(hconsolein,&count)
	return count>1
end

export func os_getdllinst(ichar name)u64=
	wt_handle hinst

	hinst:=LoadLibraryA(name)
	return cast(hinst)
end

export func os_getdllprocaddr(int hinst,ichar name)ref void=
	return GetProcAddress(cast(hinst),name)
end

export proc os_initwindows=
	os_init()
	os_gxregisterclass("pcc001")
end

export proc os_gxregisterclass(ichar classname)=
	const idcarrow=32512
	rwndclassex r
	static byte registered

	if registered then
		return
	fi

	clear r

	r.size:=r.bytes
	r.style:=8 ior 32
	r.wndproc:=cast(&mainwndproc)
	r.instance:=nil

	r.icon:=nil
	r.cursor:=LoadCursorA(nil,ref void(idcarrow))
	r.background:=cast(15+1)
	r.menuname:=nil
	r.classname:=classname
	r.iconsm:=nil

	if RegisterClassExA(&r)=0 then
		printf("Regclass error: %lld %lld\n",classname,GetLastError())
		stop 1
	end
	registered:=1
end

global function mainwndproc (
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)int=
	rmsg m
	int i,result
	int l
	static int count=0

	m.hwnd:=hwnd
	m.message:=message
	m.wParam:=wParam
	m.lParam:=lParam
	m.pt.x:=0
	m.pt.y:=0
	
	if (wndproc_callbackfn) then
		result:=(wndproc_callbackfn^)(&m)
	else
		result:=0
	fi

	if m.message=wm_destroy then
		return 0
	fi

	if not result then
		return DefWindowProcA(hwnd,message,wParam,lParam)
	else
		return 0
	fi
end

export proc os_setmesshandler(ref void addr)=
	wndproc_callbackfn:=addr
end

export func os_getchx:int=
!Q! function  os_getchx_c:int
!return a 32-bit value containing:
! 15..B0:	char code
! 23..16	virtual keycode
! 31..24	shift flags (.[24]=shift, .[25]=ctrl, .[26]=alt, .[27]=capslock)
	const rightaltmask	= 1
	const leftaltmask	= 2
	const leftctrlmask	= 8
	const rightctrlmask	= 4
	const shiftmask		= 16
	const capsmask		= 128
	const scrollmask	= 64
	int count
	int charcode,keyshift,keycode
	int altdown,ctrldown,shiftdown,capslock

!os_init() unless init_flag
	unless init_flag then os_init() end

	if keypending then
		lastkey:=pendkey
		keypending:=0
	else
		if lastkey.repeatcount=0 then
			repeat
				count:=0
				ReadConsoleInputA(hconsolein,&lastkey,1,&count)
			until (lastkey.eventtype=1 and lastkey.keydown=1)
		fi
	fi

!set shift flags

	altdown		:= ((lastkey.controlkeystate iand (leftaltmask ior rightaltmask))|1|0)
	ctrldown	:= ((lastkey.controlkeystate iand (leftctrlmask ior rightctrlmask))|1|0)
	shiftdown	:= ((lastkey.controlkeystate iand shiftmask)|1|0)
	capslock	:= ((lastkey.controlkeystate iand capsmask)|1|0)

	--lastkey.repeatcount		!count this key out

	charcode:=lastkey.asciichar
	keycode:=lastkey.virtualkeycode iand 255

	if charcode<0 then
		if charcode<-128 then
			charcode:=0
		else
			charcode+:=256
		fi
	fi

!for keycodes in range 186 to 223, which are all stand-alone punctuation keys, I might
!wish to set charcode to the appropriate printed char code (currently charcode will be
!zero, and keyboard handlers need to detect keycodes such as vkequals)
!....

	if altdown and ctrldown and charcode=166 then
		altdown:=ctrldown:=0
	else
		if altdown or ctrldown then
			charcode:=0
			if keycode>='A' and keycode<= 'Z' then
				charcode:=keycode-'@'
			fi
		fi
	fi

	keyshift:=capslock<<3 ior altdown<<2 ior ctrldown<<1 ior shiftdown

	return keyshift<<24 ior keycode<<16 ior charcode
end

export func os_getos=>ichar=
	return "W64"
end

export func os_gethostsize=>int=
	return 64
end

export func os_shellexec(ichar opc, file)int=
	return system(file)
end

export proc os_sleep(int a)=
	Sleep(a)
end

export func os_getstdin:filehandle =
	return fopen("con","rb")
end

export func os_getstdout:filehandle =
	return fopen("con","wb")
end

export func os_gethostname:ichar=
	static [300]char name
	static int n

	GetModuleFileNameA(nil,&.name,name.bytes)
	return &.name
end

export func os_getmpath:ichar=
	return F"C:\m\"
end

export func os_clock:int64=
	return clock()
end

export func os_ticks:int64=
	return GetTickCount64()
end

export func os_iswindows:int=
	return 1
end

export proc os_getsystime(ref rsystemtime tm)=
	GetLocalTime(tm)
end

export proc os_peek=
	int ticks
	static int lastticks
	[100]byte m
	ticks:=GetTickCount64()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end

export func os_allocexecmem(int n)ref byte=
	ref byte p
	u32 oldprot
	int status

	p := VirtualAlloc(nil, n, MEM_RESERVE ior MEM_COMMIT, PAGE_NOACCESS);
	if p = nil then return nil fi

	status := VirtualProtect(p, n, PAGE_EXECUTE_READWRITE, &oldprot);
	if status = 0 then return nil fi

	return p
end

export func dirlist(ichar filespec, ref[]ichar dest, int capacity, t=1)int=
!filespec is a filename (eg. "*.dwg") with possible drive/path; scan
!directory for all matching files:
! Store each file in dest array up to capacity
! Return:
!  -1:	capacity exceeded
!   N:  number of files found including 0 for no matching files

!t has this value
! +1  Include normal files only, no sub-directory names
! +2  Include directories
! +3  (+1 +2) Include all files including directories
! +4  Convert to lower case
	ref void hfind
	rfinddata file
	int nfiles:=0
	[300]char path
	[300]char fullfilename

	strcpy(path, extractpath(filespec))


	if (hfind:=findfirstfilea(filespec,&file))<>ref void(-1) then	!at least one file
		repeat
			if (file.fileattributes iand 16) then		!this is a directory
				if (t iand 2)=0 then nextloop fi		!no directories
			else						!this is a file
				if (t iand 1)=0 then nextloop fi
			fi
			if nfiles>=capacity then
				nfiles:=-1
				exit
			fi

			if (t iand 4) then				!to lower case
				convlcstring(file.filename)
!				convlcstring(&.file.filename)
			fi
			strcpy(fullfilename, path)
			strcat(fullfilename, file.filename)

			dest[++nfiles]:=pcm_copyheapstring(fullfilename)

		until not findnextfilea(hfind,&file)
		findclose(hfind)
	fi
	return nfiles
end
=== mwindll.m 0 1 38/38 ===
export function os_calldllfunction(
	ref proc fnaddr,
	int retcode, nargs,
	ref[]i64 args,
	ref[]byte argcodes)u64 =

	u64 a
	r64 x
	int nextra := 0, pushedbytes

!Stack is 16-byte aligned at this point

	if nargs<4 then
		nextra:=4-nargs			!need at least 4 slots for shadow space
	elsif nargs.odd then		!need one more for a 16-byte-aligned stack
		nextra:=1
	fi

	pushedbytes:=(nextra+nargs)*8

	to nextra do
		asm push 0
	od

	for i:=nargs downto 1 do
		a:=args[i]				!get generic 64-bit value to push
		asm push u64 [a]
	od

! blindly load first 4 args to both int/float regs, whether used or not,
! and assuming calling a variadic function whether it is or not

	assem
		mov D10,   [Dstack]
		movq XMM0, [Dstack]
		mov D11,   [Dstack+8]
		movq XMM1, [Dstack+8]
		mov D12,   [Dstack+16]
		movq XMM2, [Dstack+16]
		mov D13,   [Dstack+24]
		movq XMM3, [Dstack+24]
	end

	if retcode='I' then
		a:=(ref func:int64(fnaddr))^()
		asm add Dstack,[pushedbytes]
		return a

	else
		x:=(ref func:r64(fnaddr))^()
		asm add Dstack,[pushedbytes]
		return u64@(x)			!(type-punning cast)

	fi
end	
=== END ===
1 mm.m
2 mx_lib.m
3 mx_decls.m
4 mm_cli.m
5 mm_blockpcl.m
6 mm_assem.m
7 mm_decls.m
8 mm_diags.m
9 mm_export_dummy.m
10 mm_genpcl.m
11 mm_lex.m
12 mm_lib.m
13 mm_libpcl.m
14 mm_libsources.m
15 mm_modules.m
16 mm_name.m
17 mm_parse.m
18 mm_pcl.m
19 mm_support.m
20 mm_tables.m
21 mm_type.m
22 mc_genmcl.m
23 mc_genss.m
24 mc_libmcl.m
25 mc_decls.m
26 mc_objdecls.m
27 mc_optim.m
28 mc_stackmcl.m
29 mc_writeexe.m
30 mx_run.m
31 mm_types.m
32 mm_help.txt
33 msyslib.m
34 msys.m
35 mlib.m
36 mclib.m
37 mwindows.m
38 mwindll.m
