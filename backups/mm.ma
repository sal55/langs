=== MA 29 ===
=== mm.m 0 0 1/29 ===
!project =

	module mm_decls

	module mm_cli				! Command line interface

	module mm_lex				! Lexer, produces tokens
	module mm_parse				! Parser, produces ST, TT and AST1
	module mm_name				! Name resolution, AST1 to AST2
	module mm_type				! Type analysis, AST2 to AST3

	module mm_diags				! diagnostics
!	module mm_diags_dummy
!
	module mm_export_dummy		! Write exports files

!Embedded SYSLIB sources
!	module mm_libsources		!Embedded Syslib sources
	module mm_libsources_dummy
!
	module mm_modules			! Module handling

	module mm_support			! Source file handling

	module mm_tables			! Enumerations
	module mm_lib				! Support functions

!AST to PCL IL
	module pcl_decls			! Data structures, vars, enums and tables
	module pcl_gen				! General, and support routines
	module pcl_block			! Generate PCL sequence from proc body's AST
!
!PCL Support Library

	module pcl_api				! PCL-generating API and support routines
	module pcl_diags			! Dump PCL code by function
!
!!!PCL Interpreter
!	module mm_pclrun			! Fixups and dispatch loop


!PCL->MCL Inbetween modules
	module mcl_decls			! Data structures, vars, enums, and tables
	module mcl_gen				! ST/PCL to MCL AND some support
	module mcl_genaux			! Other support routines
	module mcl_stack			! PCL stack management (and some diags)
	module mcl_block			! PCL to MCL per functions
	module mcl_blockaux			! Support routines for PCL to MCL

!MCL Support Library
	module mcx_decls			! Data structures, vars, enums, and tables
	module mcx_asm				! Dump generated MCL as ASM source code
	module mcx_lib				! Low-level MCL-generating API and other support
!	module mcx_pedecls			! For PE format + some extra records
!	module mcx_optim			! Peephole MCL Optimiser

!	module mcx_genss			! MCL to SS binary code and data
!	module mcx_genexe			! SS data to EXE/DLL executable file

!MCL Uncomment these lines when no MCL modules presen

!global const mclpresent=0
!global const ctarget=0
!global proc genmcl=end
!global fun writeasm:ref void = nil


=== mm_decls.m 0 0 2/29 ===
global const maxmodule=300
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=300

global type symbol		= ref strec
global type unit  		= ref unitrec
global type imodule   	= ref modulerec
global type ifile   	= ref filerec
global type isubprog  	= ref subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =
	byte symbol
	byte subcode
	u16 slength				!string length; includes any zero term
	u32 pos: (sourceoffset:24, fileno:8)

	union
		ref strec symptr		!pointer to symbol table entry for name
		i64 value				!64-bit int
		real xvalue				!64-bit float
		u64 uvalue			!64-bit word
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
	ref i32 pmode
end

global record posrec=
	u32 pos: (sourceoffset:24, fileno:8)
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

!global record strec = $caligned
global record strec =
	ichar name
	ref strec owner
	ref strec deflist
	ref strec deflistx			!point to last in deflist

	ref strec nextdef
	ref strec firstdupl			!point to generic version
	ref strec nextdupl

	MCLOPND MX


	unit code					!vars with init data or proc bodies

	i32 mode					!modes must be i32 to allow fixup later
	byte namelen
	byte symbol
	byte subcode
	byte nameid					!procid etc

	union
		i32 lexindex			!for certain tokens
		i32 fnindex				!dll proc: index into table
		i32 labelno				!named labels: labelno
	end
	i32 offset

	u32 pos: (
		sourceoffset:24,		!char offset within source file
		fileno:8)				!sourcefile no

	u16 flags: (
		isstatic:1,
		hasdoswx:1,				!uses doswitchx in proc
		txdone:1,				!tx-processing flags
		circflag:1,

		islet:1,				!let-name initialised
		ishandler:1,			!proc is a handler (proc F*())

		atfield:1,				!field def has @ x (.equivfield will be set too, but that is shared)

ADDROF:1,						!TEMP ADDITIONS FOR MCL/PCL BACKEND
!IMPORTED:1,
!EXPORTED:1,

		used:1,

		isimport:1)				!for dllproc/dllvar

	byte moduleno				!module in which this name is defined
	byte subprogno				!enclosing sub-prog

	unit equivvar				!nil, or reference to @ var, as an expression

	u32 size					!used in backend, when set to basic 'tblock'

	union
		struct					!when a proc or dllproc
			ichar truename		!for imported name only
			ref strec paramlist
			pcl pccode			!generated pcl code
			u32	pclength		!number of pcl ops (for allocating post-gen tables)

			byte pcldepth			!max pcl opnd depth (will include high-args)
			byte nparams			!copy of info in pstrec; may exclude not used?
			byte nlocals
			byte isleaf				!1 when there are no calls (explicit or implicit)

			byte nmaxargs			!0, or maxargs of any call (may be capped at 4)
			byte mcldone			!
			byte hasblocks			!whether block modes are used (that means copyblock etc)

			byte dllindex		!for dllproc: which dll in dlltable
			byte nretvalues		!func: number of return values (0 for proc)
			byte varparams		!0 or number fixed params for variadic imports
		end

		struct					!when a record or record field
			ref strec equivfield	!@ x used in record field defs; ref to x
			uflagsrec uflags
			byte bitfieldwidth	!width of bitfield in record
			byte caligned		!caligned used
			byte bitoffset		!0..31 for bitfields in records
			byte equivoffset
		end

		struct					!when a param name
			ref strec nextparam
			byte byref			!0/1 = byvalue/byref
			byte optional		!0 or 1	
		end
	end

!	ref pclinforec pclinfo

	byte maxalign				!for record types (doesn't fit above)
!	byte used					!1 if var used at least once

	byte scope					!module_scope etc

!temporary, added for mcl backend which now uses same strec
	byte reg
!	u32 size
end

!global record unitrec = $caligned
global record unitrec =
	unit nextunit

	byte tag					!jtag number
	byte resultflag				!1 when the result of this unit is needed; 0=void or discarded
	byte insptr					!tx-processing flags
	byte txcount

	u32 pos: (
		sourceoffset:24,		!char offset in source file
		fileno:8)				!sourcefileno

	i32 mode
	i32 oldmode					!convert/typepun: original mode (usually that of any subnode)

	byte moduleno
	byte subprogno
	byte flags: (
		initlet:1,				!1 for an assignment that initialises a let
		isconst:1)				!1 for jconst, and jmakerange with const range

	union
		byte pclop				!kadd etc, for jbin, incr etc
		byte propcode			!kklen etc
		byte inv				!notin
!		byte convcode			!kkfix etc
		byte bfcode				!bf_even etc
		byte cvindex			!cv_lineno etc, compile var index
		byte fnindex			!sf_print etc
	end
	union
		byte mathsop			!maths_sin etc
		byte condcode			!eq_cc etc
	end

!	byte pmode					!
	byte avcode
	[2]byte SPARE

!Rest of unit is fields A, B, C. Not all will be used
!Jsubs[tag] says how many (0-3) will be sub-nodes
!The rest may be used for other purposes depending on jtag

	union
		unit	a				!subnode
		symbol	def				!jname
		i64		value			!jconst
		r64		xvalue			!jconst
		ichar	svalue			!jconst: string const or data-string
		i64		range_lower		!jmakerange only
	end

	union
		unit	b				!subnode
		i64		range_upper		!jmakerange only
	end

	union
		unit	c				!subnode

		struct					!const string
			u32  slength		!includes any zero term
			byte isastring
			char strtype		!0/'B'/'S' = normal / bindata / strdata
		end

		u32 length			!makelist: number of elements

		struct					!cmpchain
			[8]byte cmpgenop
		end

		u32 offset				!jdot
!		byte avcode				!jname for/autovars: 'I','T','S' = index/to/step autovars

		i32 loopindex			!jexit etc
		i32 whenlabel			!label associated with expr, for recase

	end
	[3]unit abc @ a				!alias a/b/c with an array
end

global record modulerec=
	ichar	name				!module name and base filename
	ifile	file
	i16		moduleno			!useful if using pointer to a source rec
	i16		subprogno
	i16		fileno
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

	i16	subprogno
	i16	moduleno			!0, or moduleno

	i16	fileno				!refers to self
	i16	spare

end

global record subprogrec =
	ichar name
	i16 firstmodule			!will be header module or same as mainmodule if no header
	i16 mainmodule			!0, or module containing 'main'
	i16 lastmodule			!always first..lastmodule
!	i16 compiled				!1 if compiled
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
global int currmoduleno				!used when compiling modules
global byte loadedfromma	!1 if source/support files are in loaded .ma file

global tokenrec lx				!provides access to current token data
global tokenrec nextlx			!provides access to next token

global [0..maxlibfile]ichar libfiles

global int mainsubprogno		!index of main subprog (eg. may be before/after syslib)

!global const int maxtype=6'000
global const int maxtype=16'000

global int ntypes

global [0..maxtype]symbol	ttnamedef
global [0..maxtype]symbol	ttowner			!for ttlowerexpr/rtlengthexpr

global [0..maxtype]i32		ttbasetype		!basetype
global [0..maxtype]ichar	ttname

global [0..maxtype]u32		ttsize
global [0..maxtype]byte		ttsizeset
global [0..maxtype]i32		ttlower 		!.lbound (default 1)
global [0..maxtype]i32		ttlength 		!elements in array/record/tuple
global [0..maxtype]ref[]i32	ttmult 			!ttlength elements in tuple

global [0..maxtype]unit		ttdimexpr		!length, lower:length, or lower..upper

global [0..maxtype]i32		tttarget 		!for array/ref types
global [0..maxtype]i32		ttlineno

global [0..maxtype]byte		ttsigned		!is i8 i16 i32 i64
global [0..maxtype]byte		ttisreal		!is r32 r64
global [0..maxtype]byte		ttisinteger		!is i8..i64/u8..u64/c8..c64
global [0..maxtype]byte		ttisshort		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte		ttisref			!is a pointer

!global const int maxtypename=4'000
!global const int maxtypename=8'000
global const int maxtypename=38'000

global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..symbolnames.upb]byte typestarterset

global symbol currproc

global int headermode=0

global ref procrec proclist,proclistx			!linked list of all procs
global ref procrec staticlist,staticlistx		!linked list of all static
global ref procrec constlist,constlistx		!linked list of all export consts

global unit nullunit

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global byte msyslevel=2		!0/1/2 = none/min/normal

global byte fshowtiming
global byte fshowss
global byte fshowasm
global byte fshowpcl
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte fshowmodules
global byte fshowdiags			!1 means any of the above set

global byte fcheckunusedlocals=0

global byte highmem=1			!enable rip by default
global byte clinux				!1 when clang_pass targeting linux

global byte dointlibs=fsyslibs

!passlevel used for compiler debug only
global int passlevel=0
global int libmode=0					!1 means eventual ML/LIB target
global int fshortnames					!mcl/asm display

global ichar outfile					!one of the following two
global ichar destfilename				!nil, or override outfile
global ichar destfilepath				!nil, or set path of outfile

!global const langhomedir	= "C:/mx/"
global const langhomedir	= "C:/bx/"
!global const langhomedir	= "C:/bx2/"

global const langhelpfile	= "mm_help.txt"

!global byte ctarget=0

global int pcltime
global int mcltime
global int sstime
global int exetime

global byte fregoptim = 1
global byte fpeephole

global int mmpos

global int mlabelno

global const reducelabels=1
!global const reducelabels=0

!GLOBAL INT NALLUNITS
!GLOBAL INT NCONSTUNITS
!GLOBAL INT NNAMEUNITS
!GLOBAL INT NSOLOUNITS
!GLOBAL INT NONEUNITS
!
!GLOBAL INT NREADUNITS
!GLOBAL INT NTOWER
!GLOBAL INT NEARLYRET
!GLOBAL INT NALLBINS
!GLOBAL INT NALLADDS
!GLOBAL INT NSIMPLEBINS
!
!
GLOBAL INT NALLPROCS
=== mm_cli.m 0 0 3/29 ===

global ichar syslibname=""

!macro SHOW(m) = println m
macro SHOW(m) = eval 0

!main production options; passnames are also file extensions for outputs

global enumdata []ichar passnames =
!								Output (when this is the final step)
	(load_pass,		$),
	(parse_pass,	$),
	(fixup_pass,	$),
	(name_pass,		$),
	(type_pass,		$),

	(ma_pass,		"ma"),			! .ma     These are are special
	(getst_pass,	"list"),		! .list
	(getproj_pass,	"proj"),		! .prog

	(pcl_pass,		"pcl"),			! .pcl (?)
	(mcl_pass,		"asm"),			! .asm
	(asm_pass,		"asm"),			! .asm
	(obj_pass,		"obj"),			! .obj (via .asm and aa)
	(dll_pass,		"dll"),			! .dll
	(exe_pass,		"exe"),			! .exe
	(mx_pass,		"mx"),			! .mx
	(run_pass,		"(run)"),		! run in-memory
end

enumdata []ichar optionnames, []byte optionvalues =

!special outputs
	(ma_sw,			"ma",			ma_pass),
	(getst_sw,		"getst",		getst_pass),
	(getproj_sw,	"getproj",		getproj_pass),

!normal production outputs
	(load_sw,		"load",			load_pass),
	(parse_sw,		"parse",		parse_pass),
	(fixup_sw,		"fixup",		fixup_pass),
	(name_sw,		"name",			name_pass),
	(type_sw,		"type",			type_pass),
	(pcl_sw,		"p",			pcl_pass),
	(mcl_sw,		"mcl",			mcl_pass),
	(asm_sw,		"a",			asm_pass),
	(obj_sw,		"obj",			obj_pass),
	(dll_sw,		"dll",			dll_pass),
	(dll2_sw,		"d",			dll_pass),
	(exe_sw,		"exe",			exe_pass),		!default
	(mx_sw,			"mx",			mx_pass),
	(run_sw,		"r",			run_pass),		!default with ms.exe

	(sys_sw,		"sys",			2),
	(minsys_sw,		"min",			1),
	(nosys_sw,		"nosys",		0),
	(clinux_sw,		"linux",		0),

	(noopt_sw,		"no",			0),
	(nopeephole_sw,	"nopeep",		0),
	(noregoptim_sw,	"noregs",		0),

!diagnostic outputs
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
	(pst_sw,		"pst",			0),

	(shortnames_sw,	"shortnames",	0),


	(time_sw,		"time",			0),
	(v_sw,			"v",			2),
	(vv_sw,			"vv",			3),
	(quiet_sw,		"q",			0),
	(csize_sw,		"cs",			1),
	(size_sw,		"ss",			2),
	(help_sw,		"h",			0),
	(help2_sw,		"help",			0),
	(ext_sw,		"ext",			0),
	(out_sw,		"o",			0),
	(outpath_sw,	"outpath",		0),
	(unused_sw,		"unused",		0),

	(norip_sw,		"norip",		0),
	(himem_sw,		"himem",		2),
end


byte msfile

global const logfile="mx.log"

ichar outext=""				!for reporting of primary o/p file

global int startclock, endclock
global int cmdskip

global ichar inputfile

global int loadtime
global int parsetime
global int resolvetime
global int typetime
global int ctime
global int compiletime

global proc main=
!STOP
!proc main=
	unit p,q,r
	int m,fileno,ntokens,t,tt

!cpl "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
!
!CPL =strec.bytes
!CPL =unitrec.bytes
!CPL =MCLOPNDREC.bytes
!CPL =JTAGNAMES.LEN
!CPL =pclrec.bytes
!CPL =pclopndrec.bytes

!for s in pclnames do
!	fprintln "proc px_#*(pcl p) =", s
!	println "\tunimpl(p)"
!	println "end"
!	println
!od

!for s in jtagnames do
!	fprintln "\twhen j# then", s
!	fprintln "\t\ttx := do_#(p)", s
!	println
!od
!
	startclock:=os_clock()
	initdata()

	getinputoptions()

!CPL =FREGOPTIM
!CPL =FPEEPHOLE

	showcompilemess()

! Do early passes common to all options

	loadproject(inputfile)

	do_parse()
	do_name()
	do_type()

! Special outputs can be done at this point
	do_writema(inputfile)	when passlevel=ma_pass			! terminates
	do_getinfo(inputfile)	when passlevel in [getst_pass, getproj_pass]		! terminates
	do_writeexports()		when passlevel=dll_pass

!CPL =PASSNAMES[PASSLEVEL]

	case passlevel
	when pcl_pass then
		do_genpcl()

	when mcl_pass, asm_pass then
		do_genmcl()

	when obj_pass then
		do_writeobj()	

	when exe_pass then
		do_writeexe()	

	when dll_pass then
		do_writedll()	

	when mx_pass then
		do_writemx()	

	when run_pass then
		do_run()

	esac

	showsurveys()

	do_outputs()
	showlogfile()

	showtimings() when fshowtiming

	if fverbose=3 then println "Finished." fi

end

proc showcompilemess=
	if fverbose>=1 and not msfile then
		fprintln "Compiling # to #",inputfile,changeext(outfile,(ctarget|"c"|passnames[passlevel]))
	fi
end

proc do_parse=
!	if fverbose=3 then println "PARSE" fi

	return unless passlevel>=parse_pass

	int tt:=clock()

	for i to nmodules do
		parsemodule(modules[i])
	od
	parsetime:=clock()-tt

	if passlevel>=fixup_pass then
!		if fverbose=3 then println "FIXUP" fi
		fixusertypes()
	fi

	fixstartprocs()
!
	if fshowast1 then showast("AST1") fi
end

proc do_name=
!	if fverbose=3 then println "NAME" fi
	return unless passlevel>=name_pass

	int tt:=clock()
	rx_typetable()

	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
	resolvetime:=clock()-tt

	if fshowast2 then showast("AST2") fi
end

proc do_type=
!	if fverbose=3 then println "TYPE" fi

	return unless passlevel>=type_pass
	int tt:=clock()
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od
	tx_allprocs()
	typetime:=clock()-tt
	if fshowast3 then showast("AST3") fi
end

proc initdata=
	imodule pm
	ifile pf

	pcm_init()
	lexsetup()
	init_tt_tables()
	initbblib()

	pm:=pcm_allocz(modulerec.bytes)

	pm.name:="PROGRAM"

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
	pm.stmodule:=stprogram
	modules[0]:=pm

!*!	igetmsourceinfo:=cast(mgetsourceinfo)

!	idomcl_assem:=cast(domcl_assem)
!	igethostfn:=cast(findhostfn)

end

proc getinputoptions=
	int paramno,pmtype,sw,extlen
	ichar name,value,ext
	[300]char filespec

!	if tc_useruntcl then
!		passlevel:=runtcl_pass
!		fverbose:=0
!	fi
	paramno:=1

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

!CPL =PASSNAMES[PASSLEVEL]

			if passlevel= run_pass then
				cmdskip:=paramno-1+$CMDSKIP
!CPL "EXITG1"
				exit
			fi

		when pm_libfile then
			loaderror("Lib files go in module headers")
		else
			loaderror("Invalid params")
		esac

	od

	if passlevel=0 then
		if not ctarget then
			passlevel:=exe_pass
			outext:="exe"
		else
			passlevel:=mcl_pass
			outext:="c"
		fi
	fi

	case passlevel
	when obj_pass, dll_pass then
		highmem:=2
	when mcl_pass then
!*!		if assemtype='NASM' then highmem:=2 fi
	when mx_pass, run_pass then
		highmem:=0
	esac

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "   ",cmdparams[0]," prog[.m]     Compile prog.m to prog.exe"
		println "   ",cmdparams[0]," -h           Show all options"
		stop

	else
!default output
		outfile:=pcm_copyheapstring(inputfile)

		if destfilename then
			outfile:=destfilename
		fi

		if destfilepath then
			strcpy(filespec,destfilepath)
			strcat(extractfile(filespec), outfile)
			outfile:=pcm_copyheapstring(filespec)	
		fi
	fi

	ext:=extractext(inputfile)
	extlen:=strlen(ext)
	strcpy(filespec, changeext(cmdparams[0],ext))
	convlcstring(filespec)
	if eqstring(filespec, inputfile) and passlevel=exe_pass then
		strcpy(&filespec[1]+strlen(filespec)-extlen-1, "2.m")
		outfile:=pcm_copyheapstring(filespec)
		println "New dest=",outfile
	fi

!*!	tcl_setflags(highmem:highmem, shortnames:fshortnames)
!*!	tcl_cmdskip(cmdskip)
!*!	if msyslevel=2 then pfullsys:=1 fi

end

proc do_option(int sw, ichar value, int paramno=0)=
	static byte outused, outpathused
	byte newpass

!CPL "DOOPTION", OPTIONNAMES[SW], PASSLEVEL

	if sw in ma_sw..run_sw then
		newpass:=optionvalues[sw]
		if passlevel and newpass<>passlevel then
			loaderror("Conflicting pass:", optionnames[sw])
		fi
		passlevel:=newpass
		outext:=passnames[sw]

		return
	fi

	if sw in ast1_sw..pst_sw then
		fshowdiags:=1
	fi

	case sw
	when ast1_sw then fshowast1:=1
	when ast2_sw then fshowast2:=1
	when ast3_sw then fshowast3:=1
	when showasm_sw then fshowasm:=1
	when showpcl_sw then fshowpcl:=1
	when st_sw then fshowst:=1
	when stflat_sw then fshowstflat:=1
	when types_sw then fshowtypes:=1
	when showss_sw then fshowss:=1
	when showmodules_sw then fshowmodules:=1

	when clinux_sw then clinux:=1

	when sys_sw, minsys_sw, nosys_sw then msyslevel:=optionvalues[sw]

	when noopt_sw then fpeephole:=fregoptim:=0
	when nopeephole_sw then fpeephole:=0
	when noregoptim_sw then fregoptim:=0

	when time_sw then fshowtiming:=1
	when v_sw, vv_sw, quiet_sw then fverbose:=optionvalues[sw]
!	when csize_sw, size_sw then pverbose:=optionvalues[sw]

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

	when norip_sw, himem_sw then highmem:=optionvalues[sw]

	end case

end

proc showcaption=
	println "M Compiler [M8.0]", $date, $time
end

global proc showhelp=
	println strinclude(langhelpfile)
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
			if subproghasstart[s] and subprogs[s].mainmodule=i then
				ms.ststart:=addstartproc(ms.stmodule,"start", program_scope,i)
!				ms.ststart.pclinfo:=pcm_allocnfz(pclinforec.bytes)
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

PROC SHOWSURVEYS=
!
!CPL "Read Units:   ", NREADUNITS:"s,12jr"
!CPL "Tower levels: ", NTOWER:"S,12JR"
!CPL "Early returns:", NEARLYRET:"S,12JR"
!CPL "All Bins:     ", NALLBINS:"S,12JR"
!!CPL "All Adds:     ", NALLADDS:"S,12JR"
!CPL "Simple Bins:  ", NSIMPLEBINS:"S,12JR"
!
!CPL =NALLUNITS
!CPL =NCONSTUNITS
!CPL =NNAMEUNITS
!CPL =NSOLOUNITS, NSOLOUNITS-(NCONSTUNITS+NNAMEUNITS)
!CPL =NONEUNITS
!CPL =NUNITS0
!CPL =NUNITS1
!CPL =NUNITS2
!CPL =NUNITS3

!CPL =MLABELNO

!CPL =NALLEXPR
!CPL =NFASTEXPR

!CPL =NALLGENPCHIST[0]
!CPL =NALLGENPCHIST[1]
!CPL =NALLGENPCHIST[2]
!CPL =NALLGENPCHIST[3]
CPL =NALLPROCS
END

proc do_genpcl=
!CPL "GENPCL/////"
	if not pclpresent then loaderror("No PCL") fi
	genpcl()
end

proc do_genmcl=

	if not mclpresent then loaderror("No MCL") fi

	genpcl()

!CPL "SS", $lineno
	genmcl()
!CPL "SS", $lineno
end

proc do_writeobj=
	CPL "WRITEOBJ NOT READY"
end

proc do_writeexe=
	CPL "WRITEEXE NOT READY"
end

proc do_writedll=
	CPL "WRITEDLL NOT READY"
end

proc do_writemx=
	CPL "WRITEMX NOT READY"
end

proc dowritepcl=
	ichar filename
	ref strbuffer pclstr
	filehandle f

	pclstr:=writeallpcl()

	filename:="PCL"

	if fverbose>=2 then println "Writing PCL" fi

	f:=fopen(filename, "w")
	gs_println(pclstr, f)
	fclose(f)

	gs_free(pclstr)
end

proc dowriteasm=
	ichar filename
	ref strbuffer asmstr
	filehandle f

!CPL "-----------WRITE MCL"

!	if assemtype='NASM' then
!		phighmem:=2
!	fi

	asmstr:=writeasm()

	STOP when asmstr=nil

	filename:=changeext(outfile, "asm")

	if fverbose>=2 then println "Writing ASM", filename fi

	f:=fopen(filename, "w")
	gs_println(asmstr, f)
	fclose(f)

	gs_free(asmstr)
end

proc do_run=
!this to run native code, not interpreter
!	if not runpresent then loaderror("No RUNPCL") fi
	CPL "RUN NOT READY"
end

proc do_outputs=
!Any diagnostic or requested textual outputs
!These will be written to a dedicated file here
!If appearing in a composite log file, then they are appended ther
!Some stuff like 'showproject info' is only done there, direct to logfile
!CPL "DOOUT------------", =FSHOWPCL, =passlevel, =PCL_PASS

	if fshowpcl and passlevel>=pcl_pass then
		dowritepcl()
	fi

!'mcl'/'fshowasm' is also used for C target from backend, so adjust file exts
	if fshowasm and passlevel>=mcl_pass or passlevel=asm_pass then
!		CPL "WRITEMCL"
		dowriteasm()
	fi

!	if fshowast3 then showast("AST3") fi
!	if fshowss and passlevel>=obj_pass then
!
!	fi

end
=== mm_lex.m 0 0 4/29 ===
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
!global const hstsize	= 65536*4
global const hstmask	= hstsize-1

global [0:hstsize]symbol hashtable
[0..255]byte namemap			!0/1/2 = other/name/name-upper

ichar u64maxstr="18446744073709551615"

global proc lex=
	int lena,lenb
	ref char p

	lx:=nextlx				!grab that already read basic token
	lx.sourceoffset:=lxstart-lxsource

	docase lexreadtoken(); nextlx.symbol
	when eolsym then
		unless keepeol[lx.symbol] then
			nextlx.symbol:=semisym
			nextlx.subcode:=1
			exit
		end

	when kincludesym then
		doinclude()

	when namesym then
		case nextlx.subcode
		when unitnamesym then
			case lx.symbol
			when intconstsym then
				case nextlx.symptr.lexindex
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
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

	when '!','#' then			!comment to eol
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

!	when '#' then
!		nextlx.symbol:=hashsym
!		return

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

!	when '?' then
!		p:=str; q:=lxsptr+1
!		while q^ not in [cr, lf, 0] do
!			p++^:=q++^
!		od
!		p^:=0
!
!		nextlx.svalue:=pcm_copyheapstring(str)
!		nextlx.symbol:=questionsym
!		return
!

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
		nextlx.symbol:=addrsym
		nextlx.subcode:=jaddrof
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

	docase c:=lxsptr++^
	when '"' then
		if lxsptr^='"' then		!repeated, assume embedded term char
			dest++^:='"'
			++lxsptr
		else			!was end of string
!			(lxsptr-1)^:=0
			exit
		fi
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		dest++^:=c
	end docase
	nextlx.slength:=lxsptr-nextlx.svalue
	nextlx.svalue:=pcm_copyheapstringn(nextlx.svalue, nextlx.slength)
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

	do
		if d=nil then exit fi

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
		when unitnamesym then
			lx.symptr.lexindex:=stsubcodes[i]
			lx.symptr.subcode:=stsymbols[i]
			lx.symptr.symbol:=namesym		!masquerades as normal identifier
		else
			lx.symptr.subcode:=stsubcodes[i]
		esac
	od
end

global proc printhashtable=
	println "Hashtable:"

!	for i:=0 to hstsize-1 do
!		if hashtable[i] then
!		fi
!	od
end

!global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=
!	lookupsys(name)
!
!	lx.symptr.symbol:=namesym
!	lx.symptr.subcode:=symbol
!	lx.symptr.index:=subcode
!
!	lx.symptr.regsize:=regsize
!end

proc doinclude=
	ichar file
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
!CPL "RAX/STR"
	else
		--NEXTLX.SLENGTH
		nextlx.subcode:='B'
!CPL "RAX/BIN"
	fi
end

func setinttype(u64 a)int=
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
					lxerror_s("Unknown string escape: \\%s",str)
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
			nextlx.slength:=length+1
!CPL "LXREADSTRING", LENGTH, NEXTLX.SLENGTH
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

	dest:=str
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

		when 'b','B' then
			length:=dest-&str[1]
			if length>64 then lxerror("bin overflow") fi
			dest:=str
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
	length:=dest-&str[1]

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

	dest:=str
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

		when '.' then
			--lxsptr
			exit

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("Numlit too long") fi
	end
	length:=dest-&str[1]

	if length>16 then
		LXERROR("MAKEDEC")
		return
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

	dest:=str
	destend:=dest+str.len-10
	a:=0

	do
		case c:=lxsptr++^
		when '0', '1' then
			a:=a*2+c-'0'
			dest++^:=c

		when '_','\'' then

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
	length:=dest-&str[1]

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
	u64 a

	dest:=str
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
				else
					--lxsptr
					exit all
				fi
			end

		when '_','\'' then

		else
			--lxsptr
			exit
		fi

		if dest>=destend then lxerror("r64lit too long") fi
	end
	dest^:=0

	if expseen and expon>=0 and not dotseen then		!read as integer
		a:=0
		for i to length do				!digits already range checked
			a:=a*10+str[i]-'0'
		od
		to expon do
			a:=a*10
		od
		nextlx.symbol:=intconstsym
		nextlx.subcode:=setinttype(a)
		nextlx.value:=a
		return
	fi


!------------------------------------------------------------
! Fast way to convert for ordinary numbers (1e100 migt be slower!)
!------------------------------------------------------------
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

!IF EXPSEEN AND NOT DOTSEEN THEN
!	CPL "READREAL NO DOT", X
!FI
end

proc nodecimal=
	lxerror("u64 const overflow")
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

=== mm_parse.m 0 0 5/29 ===
!M Language Parserxxx

const fastexpr=1
!const fastexpr=0

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

	startlex(pm.file)
	owner:=stmodule

	lex()
!

!CPL "PARSE", PM.NAME


!!=========================================
!int t:=os_clock()
!int ntokens:=0
!CPL "******************** LEX TEST ****************"
!
!!	repeat
!!		lex()
!!		++ntokens
!!!PS("TOKEN")
!!	until lx.symbol=eofsym
!
!	repeat
!		lexreadtoken()
!!PSNEXT("HELLO")
!		++ntokens
!	until nextlx.symbol=eofsym
!
!!CPL =NMODULES
!
!t:=os_clock()-t
!
!CPL "LEX TIME=", t
!CPL =ntokens
!
!STOP
!!=========================================
!
	readmoduledefs(owner)
	return 1
end

global proc readmoduledefs(symbol owner) =
!first symbol has been read
	int globalflag

	globalflag:=module_scope

	do
		switch lx.symbol
		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode

!			if globalflag=export_scope and stmodule.subprogno<>1 then
			if globalflag=export_scope and stmodule.subprogno<>nsubprogs then
				globalflag:=program_scope
			fi

			lex()

		when kprocsym, kfuncsym then	!todo
			readprocdef(owner, globalflag)
			globalflag:=module_scope

		when stdtypesym, krefsym, kicharsym, lsqsym, kslicesym then
dovar:
			readvardef(owner, globalflag, 0, staticid, 0)
			globalflag:=module_scope

		when kletsym then
			lex()
			readvardef(owner, globalflag, 0, staticid, kletsym)
			globalflag:=module_scope

		when kimportmodulesym then
			readimportmodule(owner)

		when ktypesym then
			readtypedef(owner, globalflag)
			globalflag:=module_scope

		when kconstsym then
			readconstdef(owner, globalflag)
			globalflag:=module_scope

		when krecordsym then
			readclassdef(owner, globalflag)
			globalflag:=module_scope

		when ktabledatasym then
			readtabledef(owner, globalflag)
			globalflag:=module_scope

		when semisym then
			lex()

		when eofsym then
			exit

		when kmacrosym then
			readmacrodef(owner, globalflag)
			globalflag:=module_scope

		when kprojectsym then
			repeat
				lex()
			until lx.symbol in [kendsym, eofsym]
			checkend(kendsym, kprojectsym)

		when kheadersym then
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
		serror("Code outside a func")
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
	return createunit1(jblock, p)
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

proc checkbeginend(int closesym, kwd, startline=0)=
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
	skipsemi()
!	if closesym=rbracksym or closesym=rcurlysym then
	if closesym=rbracksym then
		checksymbollex(closesym)
!		lex()
	else
		checkend(closesym, kwd, startline:startline)
	fi
end

global proc checkend(int endsym, endkwd1, endkwd2=0, startline=0)=
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
			strcpy(str, "Mismatched end ")
			if startline then
				fprint @(&str[1]+strlen(str)), " (from line #)", startline
			fi
			serror(str)
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

func readvardef(symbol owner, int scope=0, isstatic=0, varid=staticid, k)unit=
!positioned at symbol following 'mut' or 'let', which will at the first symbol of
!the type, or at the first name being defined if there is no type
!k is the keyword symbol used (let/mut), or set to 0 if no keyword has been used, 
!then mut is assumed

!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]

	unit ulist, ulistx, p
	int nvars, m, initcode
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
		stname:=getduplnameptr(owner, lx.symptr, varid)

		stname.scope:=scope

		stname.isstatic:=isstatic

		stname.islet:=(k=kletsym)
		if varid=dllvarid then
			stname.isimport:=1
		fi

		adddef(owner, stname)
!		if varid=staticid and owner.nameid<>procid then
		if varid=staticid then
!CPL "MM/ADDSTATIC", STNAME.NAME, OWNER.NAME
			addstatic(stname)
		fi

		lex()

		storemode(owner, m, stname.mode)

		if lx.symbol in [assignsym, eqsym] then

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
				fi
!				addstatic(stname)
			fi
			lex()

			stname.code:=readunit()

			if varid=frameid then
				p:=createunit2(jassign, createname(stname), stname.code)
				p.initlet:=1
				addlistunit(ulist, ulistx, p)
			fi

		elsif lx.symbol=atsym then
			if k=kletsym then serror("let@") fi
			lex()
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

proc readconstdef(symbol owner, int scope=0)=
!at 'const' symbol
	int nconsts, deft, m
	symbol stname

	lex()

	nconsts:=0

	if istypestarter() then
		deft:=readtypespec(owner)
	else
		deft:=tauto
	fi

	while lx.symbol=namesym do
		stname:=getduplnameptr(owner, lx.symptr, constid)

		lex()

		checkequals()
		lex()
		stname.code:=readconstexpr(1)

		m:=deft

		storemode(owner, m, stname.mode)
		++nconsts

		stname.scope:=scope

		adddef(owner, stname)
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
! (x, )		list with one element
! (x, x, ...)		list
! (x|x|x])		if then else fi
! (x|x, ... |x])	select then else end

!return positioned at symbol following closing ")"
!listtag is jmakelist or jmakearray if 'array' was used

	unit ulist, ulistx, p, q, r, plower
	int oldirp, length, usecomma

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
		plower:=createconstunit(lx.value, lx.subcode)
!		plower.istrueconst:=1
		lex()
		lex()

	elsif binopset[lx.symbol] and nextlx.symbol=rbracksym then	!operator constant
!	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then	!operator constant
		p:=createunit0(joperator)
		p.pclop:=symbolgenops[lx.symbol]
		lex()
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
			p:=createunit1(jmakelist, p)
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
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				fi
				addlistunit(ulist, ulistx, readunit())
				++length
				skipsemi()
			until lx.symbol<>commasym
		else

			repeat
				skipsemi()
				if lx.symbol=rbracksym then		!allow , ) to end list
					exit
				fi
				if lx.symbol=commasym then
					serror(", , null expr not allowed")
				fi
				addlistunit(ulist, ulistx, readunit())
				++length
			until lx.symbol<>semisym
		fi

		checksymbollex(rbracksym)
!		lex()
		p:=createunit1(jmakelist, ulist)
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
			p:=createunit3(jif, fixcond(p), q, r)
			return p
		when rbracksym then
			lex()
			p:=createunit3(jif, fixcond(p), q, nil)
			return p
		esac

!assume selectx expression
		addlistunit(ulist, ulistx, q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a, | using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist, ulistx, readunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readunit()
		checksymbollex(rbracksym)
!		lex()
		return createunit3(jselect, p, ulist, r)

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
			addlistunit(ulist, ulistx, readunit())
!			skipsemi()						!allow a, b, c;) (works better with a, b, c\ followed by comment on next line followed by ")")
		until lx.symbol<>semisym
		checksymbollex(rbracksym)
!		lex()

		return makeblock(ulist)


	else
		serror("(x ...")
	esac
	return nil
end

proc addlistparam(ref symbol ulist, ulistx, symbol p)=
!add unit p to unit structure ulist, ^ulistx  which can be null
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
	int opc, t

	t:=readtypespec(currproc)

	case lx.symbol
	when rbracksym then
		p:=createunit0(jtypeconst)
		p.mode:=ti64
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
			p.mode:=ti64
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

	p:=createunit1(opc, p)
	storemode(currproc, t, p.oldmode)
	return p
end

func readopc:unit=
!op sym seen just before a term
	unit p, q, r
	int tag, opc, firstsym, mathsop

	firstsym:=lx.symbol

	case lx.symbol
	when mathsopsym then
		tag:=junary
		mathsop:=lx.subcode
		opc:=kmaths
	when maths2opsym then
		tag:=jbin
		mathsop:=lx.subcode
		opc:=kmaths2
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
	when minsym, maxsym, maths2opsym, iandsym, iorsym, ixorsym then
		p:=readterm2()

		if p.tag=jmakelist then
			if p.length<>2 then serror("Needs (x, y)") fi
			q:=p.a
			r:=q.nextunit
			q.nextunit:=nil
			p:=createunit2(jbin, q, r)
			p.pclop:=opc
			p.mathsop:=mathsop
			return p
		else		!assume single tclopnd
			SERROR("READOPC/SINGLE OPND?")
			return createunit1(opc, p)

		fi
	else
		if binopset[firstsym] then
!		if symboloptypes[firstsym]=bin_op then
			serror("Can't be used as unary op")
		fi

	esac

	if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
		lex()
		tag:=junaryto
		case firstsym
		when subsym then
			opc:=kneg
		else
			opc:=symbolgenops[firstsym]
			if opc=0 then
				serror("op:= not available")
			fi
		esac
	fi

	p:=createunit1(tag, q:=readterm2())

	p.pclop:=opc
	p.mathsop:=mathsop

	if q.tag=jmakelist then
		serror("Too many opnds")
	fi

	return p
end

func readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	unit p
	imodule currmodule:=modules[currmoduleno]

	switch lx.subcode
	when cv_nil then
		p:=createconstunit(0, tref)
		lex()
		return p

	when cv_pi then
!		p:=createconstunit(i64@(3.14159'265358'979'3238'4626'433'832), treal)
		p:=createconstunit(i64@(pi), treal)
		lex()
		return p

	when cv_infinity then
		p:=createconstunit(i64@(infinity), treal)
		lex()
		return p

	when cv_lineno then
!		tc_gen(kloadimm, getlineno(lx.pos)
		p:=createconstunit(getlineno(lx.pos), ti64)
!		p:=createunit0(cv_lineno)
		lex()
		return p

	when cv_strlineno then
		getstrint(getlineno(lx.pos), str)

	when cv_modulename then
		strcpy(str, stmodule.name)

	when cv_filename then
		strcpy(str, sources[currmodule.fileno].filespec)

	when cv_func then
		strcpy(str, currproc.name)

	when cv_date then
		os_getsystime(&tm)
		fprint @str, "#-#-#", tm.day, monthnames[tm.month], tm.year:"4"

	when cv_time then
		os_getsystime(&tm)
		fprint @str, "#:#:#", tm.hour:"z2", tm.minute:"z2", tm.second:"z2"

	when cv_version then
		strcpy(str, "Compiler:M6.4")

	when cv_true, cv_false then
		p:=createconstunit(lx.subcode=cv_true, tbool64)
		lex()
		return p
	
	else
		serror_s("compiler var not impl: #", jtagnames[lx.subcode])
	end switch
	lex()

	return createstringconstunit(pcm_copyheapstring(str), -1)
end

func readcastx:unit=
!explicit cast using syntax:
! cast(expr)
! cast(expr, type)
! cast@(expr, type)
!at 'cast'
	int opc, m
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

	p:=createunit1(opc, p)
	storemode(currproc, m, p.oldmode)

	return p
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @str, "# expected, not #", symbolnames[symbol], symbolnames[lx.symbol]
		serror(str)
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

global func readtypespec(symbol owner, int typedefx=0)int=
!at initial symbol of a type, or where type is expected
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either:
! moderec	Used when called from readtypedef. This is then filled in with the
!		details of the new mode, and the new version is returned
! nil		Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using i16 etc
	symbol d, e
	int t, kwd, sltype, w
	unit x, pupper, plx
	unit dim, length
	const maxdim=30
	[maxdim]unit dims
	int ndims, i, n, k

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
				when rsqsym, commasym then			![n]
				when colonsym then				!a:n
					lex()
					if not (lx.symbol=commasym or lx.symbol=rsqsym) then	!lower:length
						length:=readunit()
						dim:=createunit2(jdim, dim, length)
					else													!lower:
						dim:=createunit1(jdim, dim)
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
			t:=createarraymode(owner, t, dims[i], (i=1|typedefx|0))
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
			t:=newtypename(d, lx.symptr)
			lex()
		else
			t:=newtypename(nil, d)
		fi

	when krecordsym, kstructsym then
		serror("Use 'record name =' syntax")

	when kunionsym then
		serror("Top-level union not allowed")

	when krefsym then		!ref T
	retry:

		lex()
		if lx.symbol=ktosym then lex() fi

		case lx.symbol
		when kprocsym, kfuncsym then	!func pointer being created
			t:=readrefproc(owner, typedefx)

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
			t:=createrefmode(owner, t, typedefx)
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
		t:=readslicetype(owner, lx.subcode, typedefx)

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
	t:=readtypespec(owner, typedefx)

	return createslicemode(owner, slicetype, t, plower, typedefx)
end

func readslist(int iscall=0, donulls)unit=
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (just comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a func-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a, b, c	)
!eg: (a		!
	unit ulist, ulistx
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
				addlistunit(ulist, ulistx, createunit0(jnull))
			else
				serror("null comma expr not allowed")
			fi
			lex()
		when rbracksym then
			if donulls then
				addlistunit(ulist, ulistx, nullunit)
			fi
			exit
		else
			addlistunit(ulist, ulistx, readunit())
			if lx.symbol in [commasym, semisym] then
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

func readindex(unit p, int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x, ...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q, plower, pupper

	lex()

	if not dot then
		case lx.symbol
		when rsqsym then
	fullslice:
			lex()
			plower:=createunit1(junary, duplunit(p))
			plower.pclop:=kklwb
			pupper:=createunit1(junary, duplunit(p))
			pupper.pclop:=kkupb
			p:=createunit2(jslice, p, createunit2(jmakerange, plower, pupper))
			return p
		when rangesym, colonsym then
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
			p:=createunit2((dot|jdotslice|jslice), p, q)
		else
			p:=createunit2((dot|jdotindex|jindex), p, q)
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
	unit q, r, p2

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p, 1)
		when namesym then
			p:=createunit2(jdot, p, createname(lx.symptr))
			lex()
		when propsym then
			if lx.subcode=kkbounds then
				q:=createunit1(jprop, duplunit(p))
				r:=createunit1(jprop, duplunit(p))
				if p.tag=jtypeconst then
					q.propcode:=kkminval
					r.propcode:=kkmaxval
				else
					q.propcode:=kklwb
					r.propcode:=kkupb
				fi

				p2:=createunit2(jmakerange, q, r)
				deleteunit(p, p2)
			else
	doprop:
				p:=createunit1(jprop, p)
				p.pclop:=lx.subcode
			fi
			lex()

		when bitfieldsym then
			p:=createunit1(jbitfield, p)
			p.bfcode:=lx.subcode
			lex()
		when ktypesym then			!.type, convert to .gettype
			case p.tag
			when jtypeconst then			!int.type=>int

			else
				SERROR("RDS:TYPEOF")
!				p:=createunit1(jtypeof, p)
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
					createunit1(junary, p), 
					createunit1(junary, p))
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
	i64 x

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

proc readprocdef(symbol procowner, int scope)=
!at 'proc' etc symbol; read proc def or declaration
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
	int kwd, startline, closesym, shortfun
	symbol stproc, stname

++NALLPROCS

	kwd:=lx.symbol
	shortfun:=lx.subcode=1
	nforloops:=0

	stproc:=readprocdecl(procowner, scope)
	checkequals()

!	stproc.pclinfo:=pcm_allocnfz(pclinforec.bytes)

	lex()

	startline:=getcurrline()

	if not shortfun then
		closesym:=checkbegin(0)
	fi

	pushproc(stproc)
	nextavindex:=0

	IF DRETVAR THEN
		stname:=getduplnameptr(stproc, dretvar, frameid)
		storemode(procowner, stproc.mode, stname.mode)
		adddef(stproc, stname)
	fi

	addtoproclist(stproc)

	if shortfun then
		stproc.code:=readunit()
		checksymbollex(semisym)
	else
		stproc.code:=readsunit()
		checkbeginend(closesym, kwd, startline)
	fi

	stproc.code:=makeblock(stproc.code)

	popproc()
end

global func readprocdecl(symbol procowner, int scope)symbol=
!at 'proc'  or 'func' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax:
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

	int kwd, varparams, nparams, nretvalues
	int subprogno
	[maxtuplesize]int retmodes
	imodule ms
	isubprog ps

	ichar metadata, truename
	symbol pequiv, stproc, owner, paramlist, nameptr

	kwd:=lx.symbol				!remember keyword

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

	stproc:=getduplnameptr(procowner, nameptr, (insidedllimport|dllprocid|procid))
	if insidedllimport then scope:=subprog_scope fi

	if truename then
		stproc.truename:=truename
	fi

	adddef(procowner, stproc)
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
			paramlist:=readparams(procowner, stproc, varparams, nparams)
			checksymbol(rbracksym)
		fi
		lex()

		if lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(owner, retmodes)
		elsif typestarterset[lx.symbol] or lx.symbol=namesym then
			nretvalues:=readreturntype(owner, retmodes)
		fi
	elsif lx.symbol=colonsym or lx.symbol=sendtosym then
		lex()
		nretvalues:=readreturntype(owner, retmodes)
	fi

	dretvar:=nil
	if nretvalues=1 then
		if lx.symbol=namesym then
			dretvar:=lx.symptr
			lex()
		fi
	fi

	unless nretvalues or (kwd<>kfuncsym) then		!func: no result given
		serror("Function needs ret type")
	end unless

	if nretvalues and (kwd<>kfuncsym) then		!proc: result given
		serror("Proc can't return value")
	fi

	stproc.paramlist:=paramlist
	stproc.nretvalues:=nretvalues

	case nretvalues
	when 0 then
		stproc.mode:=tvoid
	when 1 then
		storemode(procowner, retmodes[1], stproc.mode)
	else
		stproc.mode:=createtuplemode(procowner, retmodes, nretvalues, 0)
	esac

	stproc.code:=nil

	stproc.scope:=scope
	stproc.varparams:=varparams

	if procowner=stmodule then
		if stproc.namelen=5 and eqstring(stproc.name, "start") then
			modules[stmodule.moduleno].ststart:=stproc
			stproc.scope:=subprog_scope
			dosigcheck
		elsif stproc.namelen=4 and eqstring(stproc.name, "main") then
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

func readparams(symbol procowner, owner, int &varparams, &nparams)symbol=			!READPARAMS
!positioned at first symbol after '('; this is not ')'
!read list of params, return that list
!syntax is a list of names and/or types
!each param can optionally be followed by a default value
!finish pointing at ")"
	symbol stlist, stlistx, stname
	int byref, pmode, m, isoptional, types

	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	types:=0

	if lx.symbol=namesym and nextlx.symbol in [commasym, rbracksym] then
		types:=1
	fi

	do										!expect type of name at start of loop
		byref:=0
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
					stname:=getduplnameptr(owner, addnamestr(str), paramid)
					adddef(owner, stname)

					storemode(owner, pmode, stname.mode)
					stname.byref:=byref
					addlistparam(&stlist, &stlistx, stname)

					case lx.symbol
					when rbracksym then
						exit
					esac

					checksymbollex(commasym)
!					lex()
					if lx.symbol=ellipsissym then
						varparams:=nparams		!no. of fixed params
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
			byref:=1
			lex()
			if lx.symbol=colonsym then lex() fi
		when ellipsissym then
			varparams:=nparams
			lex()
			return stlist
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner, lx.symptr, paramid)
		adddef(owner, stname)
		lex()

		if byref then
			m:=createrefmode(procowner, pmode)
		else
			m:=pmode
		fi

		storemode(owner, m, stname.mode)
		stname.byref:=byref
		stname.optional:=isoptional
		addlistparam(&stlist, &stlistx, stname)

		case lx.symbol
		when assignsym, eqsym then
			lex()
			stname.code:=readunit()
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
		return createunit2(jif, fixcond(readunit()), createunit1(jblock, p))
	when kunlesssym then
		lex()
		q:=createunit1(jnotl, fixcond(readunit()))
!		q.pclop:=knot
		return createunit2(jif, q, createunit1(jblock, p))
	else
		return p
	esac
end

func readif:unit=
!at 'if'
	int pos1, kwd
	unit clist, clistx, plist, plistx, pelse, p

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
		addlistunit(clist, clistx, fixcond(readsunit()))

		skipsemi()
		checksymbollex(kthensym)

		if lx.symbol=colonsym then
			if clist=clistx and kwd=kifsym then
				lex()
				p:=createunit3(jif, clist, readunit(), nil)
				p.pos:=pos1
				return p
			else
				serror("then: not allowed")
			fi
		fi

		addlistunit(plist, plistx, readsunit())
		skipsemi()

	until lx.symbol<>kelsifsym

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym, kwd, 0)
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		checkend(kendsym, kwd, 0)
	esac

	p:=createunit3(jif, clist, plist, pelse)
	p.pos:=pos1
	return p
end

func readgoto(int gototag=jgoto)unit=
	lex()

	return readcondsuffix(createunit1(gototag, readunit()))
end

func readunless:unit=
	int pos
	unit pcond, pthen, pelse, p, q

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
	checkend(kendsym, kunlesssym)
	p:=createunit3(jif, q:=createunit1(jnotl, pcond), pthen, pelse)
!	q.pclop:=knot
	p.pos:=pos
	return p
end

func readswitchcase:unit=
	int pos1, kwd, opc, pos2, rangeused, nwhen
	unit pexpr, pwhenlist, pwhenlistx, pwhen, pwhenx, pelse, p, pthen, pwhenthen, pjump

	pos1:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc

	opc:=lx.subcode			!pick up tag: kcase etc
	pjump:=nil
!
	lex()

	skipsemi()

	if opc=jdoswitchx then
		checksymbollex(lbracksym)
		pjump:=readunit()
		checksymbollex(rbracksym)
		currproc.hasdoswx:=1
	fi

	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		fi
		pexpr:=nil
	else
		pexpr:=readsunit()			!index expression
		pexpr.nextunit:=pjump		!for doswitchx
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
			addlistunit(pwhen, pwhenx, p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>sendtosym then
			checksymbol(kthensym)
		fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen, pwhen, pthen)
		pwhenthen.pos:=pos2
		addlistunit(pwhenlist, pwhenlistx, pwhenthen)
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

		checkend(kendsym, kwd)
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym, kwd)
	esac

	p:=createunit3(opc, pexpr, pwhenlist, pelse)
	p.pos:=pos1

	return p
end

func readstop:unit=
	unit p
	int i
	lex()
	if exprstarter[lx.symbol] then
		p:=createunit1(jstop, readunit())
	else
		p:=createunit0(jstop)
	fi
	return readcondsuffix(p)
end

func readreturn:unit=
	unit p, q

	lex()
	if exprstarter[lx.symbol] then
		q:=readunit()
		p:=createunit1(jreturn, q)
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

	pos:=lx.pos
	lex()
	p:=readsunit()
	checkend(kendsym, kdosym)
	p:=createunit1(jdo, p)
	p.pos:=pos
	return p
end

func readto:unit=
	int pos, id
	unit p, pcount, pbody

	pos:=lx.pos
	lex()

	pcount:=readunit()

	checksymbollex(kdosym)
	pbody:=readsunit()
	checkend(kendsym, ktosym, kdosym)
	id:=frameid
	if currproc.nameid<>procid then id:=staticid fi

	p:=createunit3(jto, pcount, pbody, createname(getavname(currproc, id)))
!	p:=createunit2(jto, pcount, pbody)
	p.pos:=pos
	return p
end

func readwhile:unit=
	int pos
	unit pcond, pbody, pincr, p
	pos:=lx.pos
	lex()


	pcond:=fixcond(readsunit(1))
	pincr:=nil

	if lx.symbol=commasym then
		lex()
		pincr:=readsunit(1)
	fi

	checksymbollex(kdosym)
!	lex()
	pbody:=readsunit()

	checkend(kendsym, kwhilesym, kdosym)

	p:=createunit3(jwhile, pcond, pbody, pincr)
	p.pos:=pos

	return p
end

func readrepeat:unit=
	int pos
	unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	checksymbollex(kuntilsym)
!	lex()
	pcond:=fixcond(readunit())
	p:=createunit2(jrepeat, pbody, pcond)
	p.pos:=pos

	return p
end

func readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode

	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name, "all") then
		lex()
		p:=createunit1(opc, createconstunit(0, tint))

	elsif exprstarter[lx.symbol] then
		p:=createunit1(opc, readconstexpr(1))
	else
		p:=createunit1(opc, createconstunit(1, tint))
	fi
	return readcondsuffix(p)
end

func readprint:unit=
	int oldinreadprint, opc, isfprint, fshowname
	unit pformat, pdev, printlist, printlistx, p, q
	ref strbuffer expr

	ichar s

	oldinreadprint:=inreadprint
	inreadprint:=1
	opc:=lx.subcode

	case opc
	when jfprint, jfprintln then
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
			addlistunit(printlist, printlistx, createunit0(jnogap))
		when dollarsym then		!assume extra comma, meaning nogap
			addlistunit(printlist, printlistx, createunit0(jspace))
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
				p:=createunit2(jfmtitem, p, readunit())
			fi
			if fshowname then
				expr:=strexpr(p)
				strbuffer_add(expr, "=")
				s:=expr.strptr
				iconvucn(expr.strptr, expr.length)

				addlistunit(printlist, printlistx, q:=createstringconstunit(s, expr.length))
			fi
			addlistunit(printlist, printlistx, p)
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
		return createunit3(opc, pdev, pformat, printlist)
	else
		return createunit2(opc, pdev, printlist)
	fi
end

func readread:unit=
	int oldinreadprint, opc
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
		addlistunit(readlist, readlistx, createunit1(jreadln, pdev))
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

		pread:=createunit1(jread, pformat)

!

		p:=createunit2(jassign, p, pread)

		addlistunit(readlist, readlistx, p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	inreadprint:=oldinreadprint
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit1(jblock, readlist)
end

func readfor:unit=
!on 'for'; syntax is:
! for [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for var[, var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/od
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/od

!AV codes:
!	I	loop index, always i64; will be 'i' (declared or not declared) or autovar
!	L	forall local variable; will be 'x' (declared or not declared); type is variable

	int pos, opc
	unit pindex, plocal				!for index; for index, local
	unit pfrom, pto, pstep, ptoinit	!for INDEX:=FROM to/downto TO [by STEP]/ INDEX in FROM..TO
	unit plist, passign				!for INDEX in/inrev LIST (also LIST.BOUNDS)
	unit pcond, pbody, pelse
	unit p

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
		if lx.symbol=inrevsym then
			opc:=jfordown						!tentative; may be changed to forall
		fi
		lex()

		plist:=readunit()

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
			pfrom:=createconstunit(1, tint)
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
	checkend(kendsym, kforsym, kdosym)

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif, pcond, pbody))
	fi
	pbody.nextunit:=pelse

!forup/down layout
!	a:	pindex/ptoinit
!	b:	pfrom/pto/pstep
!	c:	pbody/pelse

!forall/rev layout
!	a:	pindex/plocal/pfrom/pto
!	b:	plist/passign
!	c:	pbody/pelse

	case opc
	when jforup, jfordown then
		if plocal then serror("for i, x?") fi
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

		passign:=createunit2(jassign, duplunit(plocal), 
					createunit2(jindex, duplunit(plist), duplunit(pindex)))
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

global proc readtypedef(symbol owner, int scope=0)=
!at 'type' symbol
	symbol sttype, stname
	int t, m

	lexchecksymbol(namesym)
	stname:=lx.symptr

	lex()
	checkequals()
	lex()

	sttype:=getduplnameptr(owner, stname, typeid)
	adddef(owner, sttype)
	m:=createusertype(sttype)

	t:=readtypespec(sttype, m)		!should return filled-in version of m

	sttype.scope:=scope
	storemode(owner, t, sttype.mode)

	if t>=0 then
		if ttisinteger[t]+ttisreal[t] then
			tttarget[m]:=t
		elsif ttisref[t] then
		elsecase ttbasetype[t]
		when tarray then
		when tslice then
!		when tslice, tvector, tflex then
		when trecord then
		else
			tttarget[m]:=t
		fi
	else
		storemode(owner, t, tttarget[m])
	fi

	if t>=0 then
		copyttvalues(m, t)
	else
		ttbasetype[m]:=tpending
	fi
end

global proc readrecordfields(symbol owner, int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
	int nvars, offset
	symbol stname, stbitfield

	nvars:=0
	while lx.symbol=namesym do

		stname:=getduplnameptr(owner, lx.symptr, fieldid)
		storemode(owner, m, stname.mode)
		++nvars

		if unionpend.ulength then
			unionstr_copy(&stname.uflags, &unionpend)
			unionstr_concat(&unionstring, &unionpend)
			unionstr_clear(&unionpend)
		else
			unionstr_clear(&stname.uflags)
		fi
		unionlastvar:=stname			!filled in from outside with 'E' codes

		adddef(owner, stname)

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

		when colonsym then				!read bitfields
!format is int : (a:1, b:3, c:2)
			lexchecksymbol(lbracksym)

			repeat
				lexchecksymbol(namesym)
				stbitfield:=getduplnameptr(owner, lx.symptr, fieldid)
				stbitfield.mode:=tbitfield
				adddef(owner, stbitfield)

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

global proc readtabledef(symbol owner, int scope=0)=
!at 'tabledata' symbol
	int i, ncols, nrows, enums, nextenumvalue, firstval, lastval, startline, closesym
	int ltype
	symbol stvar, stenum, stgen
	const maxcols=20
	[maxcols]symbol varnameptrs
	[maxcols]int varlisttypes
	[maxcols]unit plist, plistx
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

			stenum:=getduplnameptr(owner, stgen, constid)
			stenum.mode:=tint
			stenum.code:=createconstunit(nextenumvalue, tint)
			stenum.scope:=scope
			adddef(owner, stenum)
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
			addlistunit(plist[i], plistx[i], readunit())
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
	checkbeginend(closesym, ktabledatasym, startline)

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

		stvar:=getduplnameptr(owner, varnameptrs[i], staticid)
		stvar.code:=createunit1(jmakelist, plist[i])
		stvar.code.length:=nrows
!		stvar.istabdata:=1

		storemode(owner, varlisttypes[i], stvar.mode)
		stvar.scope:=scope

		adddef(owner, stvar)
		addstatic(stvar)
	od
end

global proc readclassdef(symbol owner, int scope)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, m, startline, closesym, mrec, isrecord, align
	symbol nameptr, sttype

	kwd:=lx.symbol
	isrecord:=kwd=krecordsym

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()
	checkequals()
	lex()

	align:=0
	if lx.symbol=atsym and lx.subcode=1 then	!$caligned only
		lex()
		align:=1
	fi

	sttype:=getduplnameptr(owner, nameptr, typeid)
	adddef(owner, sttype)
	m:=createusertype(sttype)

	mrec:=createrecordmode(owner, m)
	storemode(owner, mrec, sttype.mode)

!	storemode(owner, baseclass, sttype.baseclass)
	sttype.caligned:=align

	closesym:=checkbegin(1)

	startline:=getcurrline()

	readclassbody(sttype, kwd)

	checkbeginend(closesym, kwd, startline)

	sttype.scope:=scope
end

proc readclassbody(symbol owner, int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
	int kwd, t, lbcount:=0

	unionstr_clear(&unionstring)
	unionstr_clear(&unionpend)

	docase lx.symbol
	when kconstsym then
		readconstdef(owner, 0)
	when kfuncsym, kprocsym then
		kwd:=lx.symbol

		if owner.isimport then
			readprocdecl(owner, 0)
		else
			readprocdef(owner, 0)
		fi
	when krecordsym then
		readclassdef(owner, 0)

	when ktypesym then
		readtypedef(owner)
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()

	when ktabledatasym then
		readtabledef(owner, 0)

	when kmacrosym then
		readmacrodef(owner, 0)

	when kstructsym, kunionsym then
		unionstr_append(&unionpend, (lx.symbol=kstructsym|'S'|'U'))
		unionlastvar:=nil
		lex()
		if lx.symbol=lbracksym then ++lbcount; lex() fi
	when kendsym, rbracksym then
		if unionstring.ulength then
			if lx.symbol=rbracksym and lbcount then
				lex()
				--lbcount
			else
				checkend(kendsym, (unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
			fi
			if unionlastvar=nil or unionpend.ulength then
				serror("Empty union group")
			fi
			case unionstr_last(&unionlastvar.uflags)
			when 'E', '*' then
			else
				unionstr_append(&unionlastvar.uflags, '*')
			esac
			unionstr_append(&unionlastvar.uflags, 'E')
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
		readrecordfields(owner, t)

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
	int isnew, startline, closesym
	symbol stname, stname0

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

	for i to nlibfiles do
		if eqstring(libfiles[i], stname.name) then
!			stname:=libtable[i]
			isnew:=0
			exit
		fi
	od

	if isnew then			!new
		addlib(stname.name)
	fi

	startline:=getcurrline()
	closesym:=checkbegin(0)

	insidedllimport:=1

	readimportbody(owner)

	insidedllimport:=0

	checkbeginend(closesym, kimportmodulesym, startline)

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
		when kprocsym, kfuncsym then
doproc:
			d:=readprocdecl(owner, 0)
			if ndllproctable>=maxdllproc then
				serror("Too many dll procs")
			fi
			dllproctable[++ndllproctable]:=d

		when ktypesym then
			readtypedef(owner, subprog_scope)

		when kconstsym then
			readconstdef(owner, subprog_scope)

		when krecordsym then
			readclassdef(owner, subprog_scope)

		when kvarsym then
			lex()
			readvardef(owner, subprog_scope, 0, dllvarid, kvarsym)

		when stdtypesym, namesym, krefsym, kicharsym, lsqsym, kslicesym then
			readvardef(owner, subprog_scope, 0, dllvarid, 0)

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
	symbol p, d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=owner.deflist
	while p do
		if eqstring(p.name, d.name) then
			return p
		fi

		p:=p.nextdef
	od
	cpl d.name
	serror("Can't find @ field")
	return nil
end

func readrefproc(symbol owner, int typedefx)int=
!'ref' was seen, now positioned at 'proc' 'func' or 'method'
!read proc params and any result, return a complete ref proc spec
	int kwd, prettype, m, varparams, nparams
	[4]int retmodes
	symbol paramlist, stproc
	int rettype2, rettype3, nretvalues
	ichar name

	kwd:=lx.symbol				!remember whether proc or func
	
	lex()

	paramlist:=nil
	prettype:=tvoid
	nretvalues:=0
	varparams:=0

!need to create suitable holding typename in advance
	name:=nextautotype()
	stproc:=getduplnameptr(stmodule, addnamestr(name), typeid)
	adddef(stmodule, stproc)
	retmodes[1]:=tvoid

	if kwd=kfuncsym then
		if lx.symbol=lbracksym then		!possible params
			lex()
			if lx.symbol<>rbracksym then
				paramlist:=readparams(owner, stproc, varparams, nparams)
				checksymbol(rbracksym)
			fi
			lex()
			if lx.symbol=colonsym or lx.symbol=sendtosym then
				lex()
				nretvalues:=readreturntype(stproc, retmodes)
			elsif typestarterset[lx.symbol] or lx.symbol=namesym then
				nretvalues:=readreturntype(stproc, retmodes)
			fi
		elsif lx.symbol=colonsym or lx.symbol=sendtosym then
			lex()
			nretvalues:=readreturntype(stproc, retmodes)
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
				paramlist:=readparams(owner, stproc, varparams, nparams)
				checksymbol(rbracksym)
			fi
			lex()
		fi
		if typestarterset[lx.symbol] or lx.symbol=colonsym or lx.symbol=sendtosym then
			serror("proc can't have ret value")
		fi
	fi

	m:=createrefprocmode(owner, stproc, paramlist, kwd, prettype, typedefx)

	storemode(owner, retmodes[1], stproc.mode)
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
	int length
	unit p, ulist, ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset, nil)
	esac

	length:=0

	ulist:=ulistx:=nil

	do
		p:=readunit()
		++length

		addlistunit(ulist, ulistx, p)

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
		skipsemi()						!allow a, b, c;]
	od
	lex()

!	if nkeyvalues then
!		if length>nkeyvalues then serror("dict: mixed elements") fi
!		p:=createunit1(jmakedict, ulist)
!	else
		p:=createunit1(jmakeset, ulist)
!	fi
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

	pos:=lx.pos
	pt:=readterm2()

	IF FASTEXPR THEN
		if jisexpr[pt.tag]=0 then
			return pt
		fi

		if endsexpr[lx.symbol] then
			return pt
		fi
	FI

	if lx.symbol=assignsym then
		lex()
		p:=readterm2()
		IF FASTEXPR THEN
			if endsexpr[lx.symbol] then
				p:=createunit2(jassign, pt, p)
				p.pos:=pos
				return p
			fi
		FI
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
	int pos
	unit q

	p:=readorterms(pt)

	if lx.symbol = assignsym then
		pos:=lx.pos
		lex()
		q:=readassignment(nil)
		p:=createunit2(jassign, p, q)
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
		fi

		p:=createunit2(jorl, p, readandterms())
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
		fi

		p:=createunit2(jandl, p, readcmpterms())
		p.pos:=pos
	od

	return p
end

func readcmpterms(unit pt=nil)unit p=
	int pos, opc, n
	unit ulist, ulistx, q
	[8]byte genops

	p:=readinterms(pt)

	if lx.symbol not in [eqsym, cmpsym] then
		return p
	fi

	ulist:=ulistx:=p
	p:=createunit1(jcmpchain, p)
	n:=0				!n counts tclopnd after the first
	clear genops

	docase lx.symbol
	when eqsym, cmpsym then
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms()
		addlistunit(ulist, ulistx, q)
		q.pos:=pos
	else
		exit
	end docase

	if n=1 then
		p.tag:=jcmp
		q:=p.a
		p.condcode:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpgenop:=genops
	fi

	return p
end

func readinterms(unit pt=nil)unit p=
	int pos, opc
	p:=readrangeterm(pt)

	docase lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(jinrange, p, readrangeterm())		!can change to inset later

		p.inv:=opc
		p.pos:=pos
	else
		exit
	end docase

	return p
end

func readrangeterm(unit pt=nil)unit p=
	int pos, opc
	p:=readaddterms(pt)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange, p, readaddterms())
		p.pos:=pos
	fi

	return p
end

func readaddterms(unit pt=nil)unit p=
	int pos, tag, pclop

	p:=readmulterms(pt)

	while symboladdmul[lx.symbol]='A' do
		pclop:=symbolgenops[lx.symbol]
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.pclop:=pclop
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin, p, readmulterms())
		p.pclop:=pclop
		p.pos:=pos
	od

	return p
end

func readmulterms(unit pt=nil)unit p=
	int pos, pclop

	p:=readpowerterms(pt)

	while symboladdmul[lx.symbol]='M' do
		pclop:=symbolgenops[lx.symbol]
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p, readassignment())
			p.pclop:=pclop
			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin, p, readpowerterms())
		p.pclop:=pclop
		p.pos:=pos
	od

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
		p:=createunit2(jbin, p, readpowerterms())
		p.pclop:=kpower
		p.pos:=pos
	od

	return p
end

func readterm2:unit=
	unit p, q, r
	ref char pbyte
	u64 a
	int oldipl, opc, oldinrp, pos, shift, t

	pos:=lx.pos

	p:=readterm()

	docase lx.symbol
	when lbracksym then
		lex()
		oldinrp:=inreadprint
		inreadprint:=0
		q:=readslist(1, 1)
		checksymbollex(rbracksym)
!		lex()
		if p.tag=jsyscall then
			p.a:=q
		else
			p:=createunit2(jcall, p, q)
		fi
		inreadprint:=oldinrp
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr, p)
		lex()

	when lsqsym then
		p:=readindex(p, 0)

	when dotsym then
		p:=readdotsuffix(p)

	when colonsym then
		if inreadprint then exit fi
		lex()
		q:=readunit()

		if inparamlist then
			p:=createunit2(jkeyword, p, q)
		else
			serror("A:B?")
		fi

	when incrsym then
		case lx.subcode
		when kincrto then opc:=kloadincr
		when kdecrto then opc:=kloaddecr
		esac
		lex()
		p:=createunit1(jincr, p)
		p.pclop:=opc

	when lcurlysym then
		serror("X{...} not ready")
	else
		exit
	end docase

	p.pos:=pos

	return p
end

func readterm:unit=
	unit p, q, r
	u64 a
	int opc, pos, length
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

	when intconstsym, realconstsym then
		p:=createconstunit(lx.value, lx.subcode)
!		p.istrueconst:=1
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue, lx.slength)
		p.strtype:=lx.subcode			!0/1/2 = str/bindata/strdata
		lex()

	when charconstsym then
		length:=lx.slength-1
		if length>8 then serror("Char const too long") fi
		a:=0
		if length then
			memcpy(&a, lx.svalue, length)
		fi
		p:=createconstunit(a, tc64)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym, krefsym, kicharsym then
!CPL "RT CAST"
		p:=readcast()

	when addsym, subsym, minsym, maxsym, abssym, inotsym, 
iandsym, iorsym, ixorsym, 
		mathsopsym, sqrtsym, sqrsym, maths2opsym, signsym then
		p:=readopc()

	when notlsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jnotl, readterm2())
!			p.pclop:=knot
		fi

	when istruelsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(jistruel, readterm2())
		fi

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(jincr, readterm2())
		p.pclop:=opc

	when addrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc, readterm2())
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

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
				p:=createstringconstunit(s, -1)
			fi
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(jprop, dollarstack[ndollar])
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

		q:=createunit2(jbin, p, q)
		q.pclop:=kmax
		p:=createunit2(jbin, q, r)
		p.pclop:=kmin

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym, kdocasesym, kswitchsym, kdoswitchsym then
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
		p:=createunit2(jswap, p, q)

	when kevalsym then
		lex()
		p:=createunit1(jeval, readunit())

	when ksyscallsym then
		p:=createunit0(jsyscall)
		p.fnindex:=lx.subcode
		lex()

	when kstrincludesym then
		strtype:=lx.subcode
		lex()
		p:=createunit1(jstrinclude, readterm2())
		p.strtype:=strtype

	when kclearsym then
		lex()
		p:=createunit1(jclear, readterm2())

	when lcurlysym then
		serror("{...} not ready")

	when kslicesym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbollex(commasym)
		q:=readunit()
		checksymbollex(rbracksym)
CPL "MAKESLICE"
		p:=createunit2(jmakeslice, p, q)

	else
DOELSE:
		cpl symbolnames[lx.symbol], =LX.SYMBOL, ISTYPESTARTER()
		serror("readterm?")
	end switch

	p.pos:=pos
	return p
end

proc readmacrodef(symbol owner, int scope)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported
!int kwd, varparams, try_level, prettype, nparams, rettype2, rettype3, nretvalues
!ichar metadata, truename
!symbol pequiv, stproc, owner, paramlist, nameptr

	symbol nameptr, stmacro, paramlist, paramlistx, stname

	lexchecksymbol(namesym)

	nameptr:=lx.symptr
	stmacro:=getduplnameptr(owner, nameptr, macroid)
	adddef(owner, stmacro)

	owner:=stmacro

	lex()

	paramlist:=paramlistx:=nil

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=getduplnameptr(owner, lx.symptr, macroparamid)
					adddef(owner, stname)
					addlistparam(&paramlist, &paramlistx, stname)

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
		return createunit1(jrecase, readunit())
	fi
end

func fixcond(unit p)unit=
	checknotempty(p)
	if not isbooltag[p.tag] then
		insertunit(p, jistruel)
!		p.convcode:=kktoboolt
	fi
	return p
end

func readsunit(int inwhile=0)unit=
	int pos, m, sym, opc
	unit ulist, ulistx, p, q, r
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
			if lx.symbol in [kletsym, kvarsym] then
				opc:=lx.symbol
				lex()
			else
!			opc:=kmutsym
				opc:=0
			fi
			readvardef(currproc, 0, 1, staticid, opc)

		when kprocsym, kfuncsym then
			readprocdef(currproc, 0)

		when stdtypesym, krefsym, kicharsym, kslicesym, lsqsym then
			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
				goto doexec
			else
				sym:=0
				goto dovar
			fi

		when kvarsym, kletsym then
			sym:=lx.symbol
			lex()
	dovar:
			q:=readvardef(currproc, 0, 0, frameid, sym)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist, ulistx, q)		!add one by-one
				q:=r
			od

		when ktypesym then
			readtypedef(currproc, 0)

		when kconstsym then
			readconstdef(currproc, 0)

		when krecordsym then
			readclassdef(currproc, 0)

		when kmacrosym then
			readmacrodef(currproc, 0)

		when ktabledatasym then
			readtabledef(currproc, 0)

		when eofsym then
			cpl currproc.name
			serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
		when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, 
				kelsecasesym, kelseswitchsym, kendsym then
			exit
!
		when namesym then
			case nextlx.symbol
			when colonsym then
				p:=createunit0(jlabeldef)
				stname:=getduplnameptr(currproc, lx.symptr, labelid)
				adddef(currproc, stname)
				p.def:=stname
				lex()
				lx.symbol:=semisym
				addlistunit(ulist, ulistx, p)
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
	doexec3:
			addlistunit(ulist, ulistx, p)
			if lx.symbol=kdosym then
				exit
			fi

		end switch
	until lx.symbol<>semisym

	case lx.symbol
	when rbracksym, kthensym, kelsifsym, kelsesym, kuntilsym, kwhensym, kdosym, 
		kelsecasesym, kelseswitchsym, kendsym, commasym, 
		barsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then
		return createunit1(jblock, ulist)
	else
		return ulist
	fi
end

proc checknotempty(unit p)=
	if p=nil or p.tag=jblock and p.a=nil then
		serror("Empty sunit")
	fi
end
=== mm_name.m 0 0 6/29 ===
symbol currstproc
int allowmodname=0
int noexpand
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
		fi
	od
end

global proc rx_unit(symbol owner, unit p)=
	symbol d
	unit a,b
	int n,oldnoexpand,oldtag,useparams

	a:=p.a
	b:=p.b
	mmpos:=p.pos

	switch p.tag
	when jname then
		resolvename(owner,p)
		if P.TAG=JNAME AND p.def.nameid=macroid and not noexpand then
			++macrolevels
			expandmacro(p,p,nil)
			rx_unit(owner,p)
			--macrolevels
		fi

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
				storemode(owner,d.mode,p.oldmode)
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
		if not isbooltag[a.tag] then insertunit(a,jistruel) fi
		if not isbooltag[b.tag] then insertunit(b,jistruel) fi

	when jistruel then
	doistruel:
		rx_unit(owner,a)

		if isbooltag[a.tag] then
			deleteunit(p,a)
		fi

	when jnotl then
		rx_unit(owner,a)
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
		when jisfalsel then
			a.tag:=jistruel
			deleteunit(p,a)
			a:=p.a
		elsif not isbooltag[a.tag] then
			p.tag:=jisfalsel
			a:=p.a
		esac

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
		if p.equivvar then
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
		powner:=p.owner								!the owner of that entry

		case powner.nameid
		when moduleid then							!candidate is file-scope item
			if powner.moduleno=moduleno then		!same module
				return p
			elsif p.scope then	!matches an external module
				if powner.subprogno=subprogno or		!within same subprog
					 p.scope=program_scope or
					 p.isimport then 				!visible outside subprog
					++extcount			!if an ext match is closest, there can only be one
					extdef:=p
					if extcount<ambiglist.len then
						ambiglist[extcount]:=extdef
					fi
				fi
			fi

		when typeid then					!only for code inside a record def
			if powner=owner or powner=owner.owner then		!immediate match
				return p					!looks at 2 nested record levels only
			fi

		when programid then					!p is a module
			case p.nameid
			when moduleid, subprogid then	!match a module/subprog name
				if subprogno=moduletosub[p.moduleno] then
					moddef:=p
				else
					for i to nsubprogs do
						if eqstring(p.name, subprogs[i].name) then
!							p.issubprog:=1				!in case not yet set
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
			rxerror_s("tcl:Undefined: #",STR,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e.pos:=p.pos
			e.islet:=islet
		fi
	fi

!	if e.used<255 then ++e.used fi
	e.used:=1

	p.def:=e
end

global func finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl

	while pdupl do
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

	case lhs.tag
	when jname then
		case d.nameid
		when moduleid, typeid, procid, typeid then

			if d.nameid=moduleid and d.subprogno<>subprogno then
				dosubprogid
			fi

			e:=finddupl(d,e)

			if e then
				if d.nameid=moduleid then
					if e.subprogno<>subprogno then
						if e.scope<program_scope AND NOT E.ISIMPORT then
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
	ref i32 pmode
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
=== mm_type.m 0 0 7/29 ===
!MACRO SETMODE(P, M) = P.MODE:=M

const nolv=0
const needlv=1

global const maxparams=100
const maxfields=200
int countedfields
int inidata

macro setmode(p, m) = p.mode:=m
macro setbool(p) = p.mode:=tbool

!proc tpass(unit p, int t=tany, lv=nolv, hard=0)=
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

	when jconst then

	when jtypeconst then
		p.mode:=ti64

	when jbytesize, jbitwidth then
		tpass(a)
		p.mode:=ti64

	when jbin, jcmp then
		tx_bin(p,a,b)

	when jinrange then
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
!			tpass(p,t,lv,hard)
			tpass(p,t,lv)
		else
			tpasslv(a)
!			setmode(p, createrefmode(nil,a.mode))
			p.mode:=createrefmode(nil,a.mode)
		fi

!	when jaddroffirst then
!		tx_addroffirst(p,a,t)
!
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

	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
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
		setmode(p, t)
	when jrecase then
		if a then
			tpass(a,ti64)
			if a.tag<>jconst then
				txerror("recase must be const")
			fi
		fi

	when jcompilervar then
		if p.cvindex in [cv_filename, cv_modulename] then
			setmode(p, trefchar)
		fi

	when jbitfield then
		tx_bitfield(p,a,lv)

	when jsyscall then
		restype:=tvoid
		paramtype:=tvoid
		case p.fnindex
		when sf_getnprocs then restype:=ti64
		when sf_getprocname then paramtype:=ti64; restype:=trefchar
		when sf_getprocaddr then paramtype:=ti64; restype:=tref 
		esac

		if paramtype<>tvoid then
			if a=nil then txerror("sys: arg missing") fi
			tpass(a,paramtype)
			if a.nextunit then txerror("sys: too many args") fi
		elsif a then txerror("sys: too many args")
		fi

		setmode(p, restype)

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
		tx_makeslice(p,a,b,t)

	when jmakeset then
		tx_makeset(p,a,t)

	when joperator then
		p.mode:=ti64

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
!			coerceunit(p,t, hard)			!apply soft conversion
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

!IF P.MODE AND NOT P.PMODE THEN
!CPL "PMODE NOT SET:"
!PRINTUNIT(P)
!FI


!CPL "TPASS/END", JTAGNAMES[P.TAG]

!P.PMODE:=GETPCLMODE(P.MODE)
	mmpos:=oldmmpos
	--depth
end

global proc tx_allprocs=
	ref procrec pp
	unit pcode

	pp:=proclist
	while pp do
		currproc:=pp.def
		pcode:=currproc.code

	    tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

		case ttbasetype[currproc.mode]
		when tvoid then		!PROC
		when ttuple then	!MULT FN
		else				!REGULAR FN
			if pcode.tag<>jreturn then
!			if NOT CTARGET AND pcode.tag<>jreturn then
				insertunit(pcode,jreturn)
				setmode(pcode, currproc.mode)
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
		tpass(a,t,lv)
		setmode(p, (t<>tvoid|a.mode|tvoid))
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

!CPL "SETARRAYSIZE"

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
		when jdim then
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
!CPL "SAS", LENGTH

	target:=tttarget[m]
	setmodesize(target)
	elemsize:=ttsize[tttarget[m]]
	ttsize[m]:=size:=length*elemsize
	ttsizeset[m]:=1

!CPL "=>",LENGTH
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
	mmpos:=oldmmpos
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
	while p do
		tpass(p,t)
		p:=p.nextunit
	od
end

proc tx_namedef(symbol d)=
	int m,mold,inidataold, LENGTH
	unit dcode,pequiv

UNIT OLDDCODE

	if d.circflag then
		txerror("Circular reference detected")
	fi
	if d.txdone then return fi

	m:=d.mode
	setmodesize(m)

	dcode:=d.code

	d.circflag:=1

	if d.equivvar then
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
			inidata:=1
			tpass(dcode,m)
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
!				d.code.mode:=mold
				setmode(d.code, mold)
			elsif mold=tr32 then
!				d.code.mode:=mold
				setmode(d.code, mold)
			fi
		fi

		if d.nameid=staticid then
			checkconstexpr(d.code)
		fi

	elsif dcode and d.nameid=frameid and ttbasetype[m]=tarray and ttlength[m]=0 then
		LENGTH:=-1
		IF DCODE.TAG=JMAKELIST THEN
			LENGTH:=DCODE.LENGTH
		FI

		tpass(dcode,m)
		d.mode:=dcode.mode
		d.circflag:=0
		d.txdone:=1

!this is bodge to get correct array size when it depends on data. Since it's
!done via an AV which is copied, but dimensions of that seem to be set later on.
!Length is set directly from the makelist construct
		if ttlength[m]=0 and length then
			ttlength[m]:=length
			ttsize[m]:=length*ttsize[tttarget[m]]
		fi

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
			setmode(p.a, p.mode)
			deleteunit(p,p.a)
		else
			goto error
		fi

	when jshorten then
		checkconstexpr(p.a)

!	when jaddrof, jaddroffirst then
	when jaddrof then
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

func getconstint(unit q)i64=
	checkconstexpr(q)

	if ttisinteger[q.mode] or q.tag=jtypeconst then
		return q.value
	elsif ttisreal[q.mode] then
		return q.xvalue
	else
		cpl strmode(q.mode)
		txerror("Getconstint: not i32/64")
	fi
	return 0
end

proc makenewconst(unit p,i64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

	p.tag:=jconst
	p.a:=p.b:=nil
	p.value:=x
	p.isconst:=1
	if t<>tvoid then
		setmode(p, t)
	fi
end

proc tx_name(unit p,int t,lv)=
	symbol d
	int oldmmpos
	unit pcode
	oldmmpos:=mmpos

	return when p.txcount
	++p.txcount

	d:=p.def
	mmpos:=d.pos

!CPL "TXNAME", D.NAME

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

!		setmode(p, d.mode)
		p.mode:=d.mode

		p.isconst:=1
		p.isastring:=pcode.isastring

	when staticid,frameid,paramid then
		if d.islet and lv then
!			println D.NAME,=LV,D.ISLET
			txerror_s("Can't use 'let' as lvalue: ",d.name)
		fi

		tx_namedef(d)

		setmode(p, d.mode)
		if d.byref then
			if not p.insptr then
				++p.insptr
				insertunit(p, jptr)
				setmode(p, tttarget[d.mode])
			fi
		fi
		twiden(p,lv)

	when procid,dllprocid then

		setmode(p, trefproc)	!use generic refproc mode (yields return type of actual proc mode
				!after a call op, or actual refproc in other context. Don't use actual
				!refproc here, to avoid generating thousands of ref proc modes, one
				!for each call, that will never be needed

	when labelid then
		if t=tvoid then			!assume standalone label; treat as goto
			insertunit(p, jgoto)
		else
			setmode(p, treflabel)
		fi

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
		setmode(p, d.mode)

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
				setmode(p, amode)
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
				setmode(p, amode)
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

	when kidiv, kirem, kidivrem, kbitand, kbitor, kbitxor then
doidiv:
		if dobinnumi(p,a,b) then return fi

	when kmin, kmax then
		if dobinnumx(p,a,b) then return fi

	when kpower then
		if dobinnumx(p,a,b) then return fi

!	when kfmod, katan2 then
	when kmaths2 then
		coerceunit(a,tr64)
		coerceunit(b,tr64)
		p.mode:=tr64
		return

	when kshl, kshr then
		if isnumi(amode) then
			coerceunit(b,ti64)
			setmode(p, amode)
			return
		fi

	elsif p.condcode then
		if dobinnumx(p,a,b) then
			setbool(p)
			return
		fi
		setbool(p)
		if ttisref[amode] and ttisref[bmode] then
			if not comparemodes(amode, bmode) then
				txerror("Cmp ref/ref not compat")
			fi
			return
		fi
		if p.condcode in [eq_cc, ne_cc] then
			if comparemodes(amode, bmode) then
				return
			fi
		fi

	else
		txerror("txbin?")
	esac

cpl pclnames[p.pclop]
	CPL "BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode)
!	TXERROR_SS("BIN/CAN'T RESOLVE MODES",strmode(amode),strmode2(bmode))
end

proc tx_binto(unit p,a,b)=
	int abase, bbase, amode,bmode, opc

	tpasslv(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if p.pclop=kdiv and ttisinteger[abase] then
		p.pclop:=kidiv
	fi

	p.mode:=tvoid

	case p.pclop
	when kadd then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			txerror("to:ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddpx
			return
		fi
	when ksub then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubpx
			return
		fi
	when kshl, kshr, kbitand, kbitor, kbitxor then
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

func getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if isnum(abase) and isnum(bbase) then
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
				setmode(a, tttarget[a.mode])
			od
			goto dorefproc
		fi

	when jif,jselect,jblock then
		TXERROR("Can't do ifx/func")

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

	setmode(p, d.mode)				!type returned by func (will be void for procs)

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

		if pm.byref then
			tpass(q,m:=tttarget[pm.mode],needlv)
			qm:=q.mode

			if not comparemodes(qm,m) then
				txerror_ss("&param: type mismatch",strmode(qm), strmode(m))
			fi

!			UNLESS CTARGET AND Q.TAG=JCONVERT THEN

				insertunit(q,jaddrof)
				setmode(q, pm.mode)
!			ELSE
!				Q.TAG:=JADDROF
!				Q.MODE:=PM.MODE
!			END

		else
			tpass(q,pm.mode)
		fi

		addlistunit(ulist, ulistx, q)
		q.nextunit:=nil
	od
	p.b:=ulist
end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.pclop
!	when katan, klog, klog10, kexp, ksqrt,ksin,kcos,ktan, kasin, kacos,
	when kmaths then
		coerceunit(a,tr64)
		resmode:=tr64

	when kneg, kabs, ksqr, ksqrt then
		txerror("not num") when not isnum(amode)

	when kbitnot, knot, ktoboolt then
		txerror("toboolt") when not isint(amode)

!	when ksliceptr then
!		tx_sliceptr(p,a)
!		return
	when ksign then
		resmode:=ti64

	ELSE
		txerror_s("TXUNARY:",pclnames[p.pclop])
	esac

	setmode(p, resmode)
end

proc tx_prop(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh, resmode

	tpass(a)

	amode:=a.mode
	resmode:=amode

	case p.propcode
	when kklwb, kkupb, kklen, kkbounds, kksliceptr then
		do_bounds(p,a)

		return

	when kkbytesize,kkbitwidth then
		size:=ttsize[(a.tag=jtypeconst|a.value|a.mode)]*(p.propcode=kkbytesize|1|8)
		makenewconst(p,size)
		resmode:=ti64

	when kkminval, kkmaxval then
		resmode:=ti64
		if a.tag=jtypeconst then
			mbase:=ttbasetype[a.value]
		else
			mbase:=ttbasetype[a.mode]
		fi

		if p.propcode=kkminval then
			case mbase
			when ti8 then x:=-128
			when ti16 then x:=-32768
			when ti32 then x:=-2_147_483_648
			when ti64 then x:=i64.min
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
			amode:=a.mode
		fi

		setmode(p, trefchar)
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return

	ELSE
		txerror_s("Prop:", pclnames[p.propcode])
	esac

	setmode(p, resmode)
end

proc tx_unaryto(unit p,a)=
	tpasslv(a)

	case p.pclop
	when kbitnot, knot, ktoboolt then
		txerror("Not int") when not isint(a.mode)
	esac

	p.mode:=tvoid
end

proc tx_if(unit p,pcond,plist,pelse, int t,lv) =
	unit pc:=pcond, pl:=plist
	int u

	u:=tvoid
	if t<>tany then u:=t fi

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
		setmode(p, u)
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
		when kincrto then p.pclop:=kincrload
		when kdecrto then p.pclop:=kdecrload
		esac
		setmode(p, a.mode)
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
		setmode(plocal, elemtype)
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
	setmode(p, tttarget[amode])
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
	setmode(p, trange)
end

proc tx_ptr(unit p,a,int t,lv)=
	symbol d

	tpass(a)

	case ttbasetype[a.mode]
	when tvoid then
		txerror("Deref Void")
	when tref then
		setmode(p, tttarget[a.mode])

	when tslice then
		CPL "DEREF SLICE"
	else
		txerror("PTR: need ref T")
	esac

	twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]symbol fieldlist
	int i,nfields,indent,nrfields,size,index, maxalign
	symbol d,e
	ref char flags
	const ss='S', ee='E'
	int flag
	static int depth


	if ttsize[m] then return fi
	if ++depth>10 then serror("Recursive record?") fi

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
	scanrecord('S',&fieldlist,index,size,0, d.caligned, maxalign)

	if d.caligned then
		size:=roundoffset(size,maxalign)
		d.maxalign:=maxalign
	else
		d.maxalign:=1
		if size iand 7 = 0 then
			d.maxalign:=8
		elsif size iand 3 = 0 then
			d.maxalign:=4
		elsif size iand 1 = 0 then
			d.maxalign:=2
		fi
	fi

	ttsize[m]:=size
	ttlength[m]:=countedfields
	ttlower[m]:=1

	--depth
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
!CPL "CALIGN", =FIELDSIZE, =ALIGNMENT, =MAXALIGN, =STRMODE(F.MODE), =TTSIZE[F.MODE]
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
		tx_makelist(a,a.a,p.oldmode,nolv)
	else
		tpass(a)
		coerceunit(a,p.oldmode,hard)
!!NEW:
!		tpass(a, p.oldmode, hard:hard)
!!		coerceunit(a,p.oldmode,hard)
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

		setmode(p, newt)

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
		setmode(p, t)
		p.resultflag:=1

	when tslice then
CPL "TSLICE"

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
		q:=createunit0(jnull)
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
end

proc tx_makeslice(unit p, a,b, int t)=
	CPL "MAKESLICE/TX"
	tpass(a)

	if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
	if tttarget[a.mode]<>tvoid then
		if not comparemodes(a.mode,createrefmode(nil,tTtarget[t])) then
			txerror("slice/ptr mismatch")
		fi
	fi

	tpass(b,ti64)
	setmode(p, t)
CPL "MKSLICE2"
	p.resultflag:=1
end

proc tx_makeset(unit p,a, int t)=
	p.isconst:=1

	while a, a:=a.nextunit do
		tpass(a)

		if not a.isconst then
			p.isconst:=0
		fi
	od

	setmode(p, tvoid)
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
		recmode:=tmode
		setmode(a, tmode)
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
		setmode(b, dequiv.mode)
		p.offset:=d.offset

		if i=j then					!single bit
			pindex:=createconstunit(i,ti64)
			newtag:=jdotindex
		else						!bit slice
			pindex:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
			setmode(pindex, trange)
			pindex.a.resultflag:=1
			pindex.b.resultflag:=1
			newtag:=jdotslice
		fi

		setmode(p, b.mode)
		twiden(p,lv)
		insertunit(p,newtag)
		setmode(p, tu64)
		p.b:=pindex
		p.a.resultflag:=1
		p.b.resultflag:=1
		p.resultflag:=1

		return

	fi

	setmode(b, d.mode)
	setmode(p, d.mode)

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
	setbool(p)
end

proc convintconst(unit p,i64 x)=
!convert unit p into int const x
	p.tag:=jconst
!CPL "CCI1", STRMODE(P.MODE)
	p.mode:=ti64
!	p.mode:=ti64
!CPL "CCI2", STRMODE(P.MODE)
	p.a:=p.b:=p.c:=nil
	p.value:=x
	p.isconst:=1
!CPL "CCI3", STRMODE(P.MODE)
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

	setmode(p, createrefmode(nil,tmode))
end

proc tx_swap(unit p,a,b)=
	int av, bv

	tpasslv(a)
	tpasslv(b)

	if not comparemodes(a.mode,b.mode) then
		txerror("SWAP: type mismatch")
	fi

	setmode(p, tvoid)
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

	setmode(p, u)
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
		setmode(p, u)
	else
		setmode(p, tvoid)
	fi

end

proc tx_notl(unit p,a)=
	tpass(a)
	setbool(p)
end

proc tx_typepun(unit p,a)=
	int smode,tmode

	case a.tag
	when jmakelist then
		TXERROR("TYPEPUN/LIST")
	else
		tpass(a)

		smode:=ttbasetype[a.mode]
		tmode:=ttbasetype[p.oldmode]

!CPL =STRMODE(SMODE)
!CPL =STRMODE(TMODE)
!CPL =STRMODE(P.OLDMODE)

		case smode
		when tr64 then
			if tmode in [ti64, tu64] then
			else
				error
			fi
		when ti64, tu64 then
			case tmode
			when tr64 then
			when tr32 then
			else
				error
			esac

		when tr32 then
!CPL "SMODE=TR32", STRMODE(TMODE)
			case tmode
			when ti32 then tmode:=ti64			!will be widened to 64 bits
			when tu32 then tmode:=tu64
			else
error:			txerror("Typepun: invalid")
			esac
		esac

		setmode(p, tmode)
		p.oldmode:=smode

!CPL "------",STRMODE(P.MODE), STRMODE(P.OLDMODE)!, strmode(a.mode)
!PRINTUNIT(P)
	esac
end

proc tx_exit(unit p,a)=
	if a=nil then return fi
	tpass(a,ti64)
	if a.tag<>jconst then
		txerror("exit/etc not const")
	fi
	p.loopindex:=a.value
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

	if p.tag=jdoswitchx then
		tpass(a)
		tpass(a.nextunit)
		if ttbasetype[a.mode]<>tref then txerror("not ref") fi
	else
		tpass(a,ti64)
	fi

	memset(&valueset,0,valueset.bytes)
	u:=tvoid

	wt:=b
	while wt do

		w:=wt.a
		while w do
			tpass(w)

			if not isconstunit(w) then
				PRINTUNIT(W)
				txerror("Switch not constant")
			fi

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
			setmode(w, b.b.mode)
			w:=w.nextunit
		od
		if t=tany then
			coerceunit(c,u)
			setmode(p, u)
		else
			setmode(p, t)
		fi
	else
		setmode(p, tvoid)
	fi
end

proc tx_return(unit p,a, int t)=
 	int m,nvalues,nret,i
	ref[]i32 pmult
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
			setmode(p, tvoid)
		else
			setmode(p, ttuple)
		fi
		P.RESULTFLAG:=1

	else
		if nret>1 then txerror("RETERROR?") fi
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			setmode(p, tvoid)
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

	if not ttisinteger[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.oldmode:=tu64
			a.resultflag:=1

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

	setmode(p, pmode)
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

	tpass(a)			!lhs
	tpass(b)			!will be a range

	if a.mode=trefchar then
		setmode(p, createslicemodek(currproc,tc8,1,0))
	else
		deref(a)
		case ttbasetype[a.mode]
		when tarray then
			setmode(p, createslicemodek(currproc,tttarget[a.mode],1, 0))

		when tslice then
			setmode(p, a.mode)

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
	when jname, jptr, jindex, jdot, jcall, jincr then
		insertunit(p, jwiden)
		p.oldmode:=m
		setmode(p, gettypebase(m))
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

		setmode(prange, trange)
		p.b:=prange
	fi

	setmode(p, slicemode)
end

proc tx_bitfield(unit p,a,int lv)=
	int i,j,bitsize,topbit
	unit r

	tpass(a,,lv)

	if not ttisinteger[a.mode] and not ttisref[a.mode] then
		if ttisreal[a.mode] then
			insertunit(a,jtypepun)
			a.mode:=a.oldmode:=tu64
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

		if p.bfcode=bf_even then
			setmode(p, tu64)
			addnotl(p)
		fi

	else
		r:=createunit2(jmakerange,createconstunit(i,ti64),createconstunit(j,ti64))
		r.a.resultflag:=1
		r.b.resultflag:=1
		setmode(r, trange)
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
		setmode(a, tmode)

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
	setmode(pfunc, e.mode)
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
			p.tag:=junary			!code gen needs to look at type, and use .propcode
		else
			goto error
		esac

	when kklen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.tag:=junary
!			p.pclop:=klen
		else
			goto error
		esac
	when kkbounds then
		setmode(p, trange)
		case mbase
		when tarray then
			p.range_lower:=ttlower[m]
			p.range_upper:=p.range_lower+ttlength[m]-1
			p.tag:=jconst
			p.a:=p.b:=p.c:=nil
			p.isconst:=1
			return

		when tslice then
		else
			goto error
		esac
	when kksliceptr then
		if mbase<>tslice then txerror("Not slice") fi
		p.tag:=junary

	esac
end

proc addnotl(unit p)=
	insertunit(p,jnotl)
	setbool(p)
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
!		IF NOT CTARGET THEN
			a:=p.a

			pname:=addrdotindex(a, offset)

			if pname then
				deleteunit(a,pname)
				if p.b=nil then
					p.b:=createconstunit(offset,ti64)
					p.b.resultflag:=1
				else 
					p.b.value+:=offset
				fi
			fi
!		FI
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
	i64 a,b,c,offset
	real x,y,z
	unit lhs, rhs

	lhs:=p.a
	rhs:=p.b

	unless lhs.tag=rhs.tag=jconst then
!		if lhs.tag=jaddrof and rhs.tag=jconst AND P.TCLOP=KADDREFX then		!ASSUME ADD/SUBREFX
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
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	fi
end

proc tevalmonop(unit p)=
	i64 a,b,c
	real x,z

	unless p.a.tag=jconst then
		return
	end

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
		when knot then c:=not a; setbool(p)
		when kbitnot then c:=inot a
		when kabs then c:=abs a

		else
			return
		esac
	when tr64, tr32 then
		case p.pclop
		when kneg then z:=-x

!		when katan then z:=atan(x)
!		when ksqrt then z:=sqrt(x)

		else
			return
		esac

	when tbool then

dobool:
		case p.tag
		when jistruel then c:=istrue a; setbool(p)
		when jisfalsel then c:=not a; setbool(p)
		elsecase p.pclop
		when ktoboolt then c:=istrue a; setbool(p)
		when knot then c:=not a; setbool(p)
		esac
	else
		return
	esac

	if ttisreal[p.mode] then
		makenewconst(p,i64@(z))
	else
		makenewconst(p,c)
	fi
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

	if atype='S' then
		--alen				!lose zero terminator
		needterm:=1
	else
		needterm:=0
	fi

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

!CPL "TX STRINCLUDE", A.SVALUE, CURRPROC.NAME

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

	int sbase:=ttbasetype[s]
	int tbase:=ttbasetype[t]

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
!		elsif tbase in tfirstshort..tlastshort then
		elsif stdsize[tbase] in 1..4 and tbase<>tr32 then
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
!		when ttype then
!			opc:=kksoftconv
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
		case tbase
		when ti64,tu64 then
			opc:=kksoftconv
			checkhard
		when tref then
			if comparemodes(s,t) or hard then
				opc:=kksoftconv
			else
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
				opc:=kksoftconv
				n:=ttlength[t]
				if n=0 then
					ttlength[t]:=p.slength/ttsize[tttarget[p.mode]]
					ttsize[t]:=p.slength
				else
!!					txerror("Array not empty")
					CPL("Array not empty")
				fi
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
!CPL =STRMODE(STARG), =STRMODE(TBASE)
!PRINTUNIT(P)
!

!TXERROR("KKCX2ICHAR:A")
				opc:=kkcx2ichar
			fi
!RETURN
		when tref then
			if ttarg=tvoid then
				opc:=kkcx2ichar
!TXERROR("KKCX2ICHAR:B")
			fi
		esac

	when tslice then
		case tbase
		when tslice then
			if comparemodes(s,t) then
				opc:=kksoftconv
			fi
		when tref then
			if ttarg=tvoid or comparemodes(starg, ttarg) then
GERROR("COERCE/SLICEPTR")
!				opc:=ksliceptr
			fi

		esac

!	when ttype then
!		if tbase<=tlastnum then
!			opc:=kksoftconv
!
!		fi
	fi

	applyconversion(p,s,t,opc)
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
!		PRINTUNIT(P)

		txerror_ss("Can't do conversion: # => #",strmode(s),strmode2(t))

	when kkharderr then
		txerror_ss("Need explicit cast: # => #",strmode(s),strmode2(t))

	when kksoftconv then
		setmode(p, t)
		return
	when kksofttrun then
		if tevalconvert(p,s,t,opc) then
			return
		fi
		insertunit(p,jshorten)
		setmode(p, t)			!don't use the short target mode
		return

	when kkax2slice then
		insertunit(p,jslice)
		setmode(p, t)
		return
	when kkichar2sl then
		tstringslice(p,t)
		return

	when kkcx2ichar then
!TXERROR("KKCX2ICHAR-C")
!		insertunit(p,jaddroffirst)
		insertunit(p,jaddrof)
		setmode(p, trefchar)
		return
	esac

	if tevalconvert(p,s,t,opc) then		!try and apply it directly
		return
	fi

!have to add an explict conversion node
	int tag
	case opc
	when kkwiden then tag:=jwiden
	when kkfwiden then tag:=jfwiden
	when kkfnarrow then tag:=jfnarrow
	when kkfix then tag:=jfix
	when kkfloat then tag:=jfloat
	when kktruncate then tag:=jtruncate
	when kktoboolt then tag:=jtoboolt
	else
		txerror_s("applyconv? ",convnames[opc])
	esac

!	insertunit(p, jconvert)
!	p.pclop:=opc

	insertunit(p, tag)
	p.pclop:=opc

	p.oldmode:=s
	p.resultflag:=1

!???
	if ttisshort[t] then
		p.oldmode:=t
		t:=gettypebase(t)
	fi

	setmode(p, t)
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
		Z:=X

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
		makenewconst(p,i64@(z),t)

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
		tx_makeslicefromlist(b,b.a,m)
		setmode(p, m)

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
			setmode(p, mm)


!Eliminate widening when lhs is not wider than rhs (and when an Widen conversion is used
!which implies that rhs is < 8 bytes)

!			if b.tag=jconvert and b.convcode=kkwiden and
			if b.tag=jwiden and
				 ttsize[a.mode]<=ttsize[b.oldmode] and not needres then
				DELETEUNIT(B, B.A)
			fi

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
	ref[]i32 pmult
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

	setmode(pp, t)
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
!Try and apply this to binary tclopnds:
!	NUMX	NUMX	DOM
!a and b have already been processed, but not coerced to any type yet

	int amode:=a.mode, bmode:=b.mode, cmode

	if isnum(amode) and isnum(bmode) then
		cmode:=min(amode, bmode)
		setmode(p, cmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi

	if isnum(amode) and isbool(bmode) then
		setmode(p, amode)
		coerceunit(b,amode)
		return 1
	elsif isbool(amode) and isnum(bmode) then
		setmode(p, bmode)
		coerceunit(a,bmode)
		return 1
	fi


	return 0
end

func dobinnumf(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMF	NUMF	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

!	if amode=ti64 then coerceunit(a, tr64); amode:=tr64 fi
!	if bmode=ti64 then coerceunit(b, tr64); bmode:=tr64 fi

	if isnumf(amode) and isnumf(bmode) then
		cmode:=min(amode, bmode)
		setmode(p, cmode)
		coerceunit(a,cmode)
		coerceunit(b,cmode)
		return 1
	fi
	return 0
end

func dobinnumi(unit p,a,b)int=
!Try and apply this to binary tclopnds:
!	NUMI	NUMI	DOM
	int amode:=a.mode, bmode:=b.mode, cmode

	if isnumi(amode) and isnumi(bmode) then
		cmode:=min(amode, bmode)
		setmode(p, cmode)
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

		p.tag:=jinset

	fi

!CPL "TXIN:"
!PRINTUNIT(P)

	unless isnum(a.mode) and b.tag in [jmakerange, jmakeset] and simpleset then
		txerror("doin")
	end
!	setmode(p, tbool)
	setbool(p)

!	if p.pclop=kknotin then
	if p.inv then
		addnotl(p)
	fi
	return 1
end

!proc setmode(unit p, int mode)=
!	int u
!	p.mode:=mode
!	u:=stdpcl[ttbasetype[mode]]
!
!	if u=tblock then
!		case ttsize[mode]
!		when 8 then u:=tu64
!		when 4 then u:=tu32
!		when 2 then u:=tu16
!		when 1 then u:=tu8
!		esac
!	fi
!	p.pmode:=u
!end
!
=== mm_diags.m 0 0 8/29 ===
int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer

strbuffer v
ref strbuffer ds=&v

const tab1="\t"
const tab2="\t\t"

const tabstr="|--"


!const fshowsymbols=1
const fshowsymbols=0

global proc printst(filehandle f, ref strec p, int level=0)=
	ref strec q

	printstrec(f, p, level)

	q:=p.deflist

	while q<>nil do
		printst(f, q, level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f, ref strec p, int level)=
	strec dd
	ref byte q
	int col, offset, n
	const tabstr="    "
	[256]char str

	gs_init(ds)

	print @str, p
	dsstr(str)
	dsstr(" ")

	offset:=0
	to level do
		dsstr(tabstr)
		offset+:=4
	od
	dsstr(":")

	gs_leftstr(ds, p.name, 28-offset, '-')
	gs_leftstr(ds, namenames[p.nameid], 12, '.')

	col:=gs_getcol(ds)
	dd:=p^


	dsstr("[")
	if p.isimport then
		dsstr("Imp ")
	else
		dsstr(SCOPENAMES[P.SCOPE])
		dsstr(" ")
	fi

	if dd.isstatic then
		dsstr("Stat")
	fi

	if dd.nameid=paramid and dd.byref then
		dsstr("byref")
	fi

	if dd.caligned then
		dsstr(" $caligned")
	fi
	if dd.maxalign then
		dsstr(" maxalign:")
		dsint(dd.maxalign)
		dsstr(" ")
	fi
	if dd.optional then
		dsstr("Opt ")
	fi
	if dd.varparams then
		dsstr("Var:")
		dsint(dd.varparams)
		dsstr(" ")
	fi

	if dd.moduleno then
		if dd.nameid<>subprogid then
			print @str, "Modno#",,dd.moduleno
		else
			print @str, "Subno#",,dd.subprogno
		fi
		dsstr(str)
	fi

	if dd.used then
		dsstr("U ")
	fi

!	if dd.isthreaded then
!		dsstr("Threaded ")
!	fi
!

	dsstr("]")
	gs_padto(ds, col+10, '=')

	if p.owner then
		fprint @str, "(#)", p.owner.name
		gs_leftstr(ds, str, 18, '-')
	else
		gs_leftstr(ds, "()", 18, '-')
	fi

	case p.mode
	when tvoid then
		dsstr("Void ")
	else
		GS_STRINT(DS, P.MODE)
		GS_STR(DS, ":")

		dsstr(strmode(p.mode))
		dsstr(" ")
	esac

	case p.nameid
	when fieldid, paramid then
		dsstr(" Offset:")
		dsint(p.offset)
		if p.mode=tbitfield then
			dsstr(" Bitoffset:")
			dsint(p.bitoffset)
			dsstr(":")
			dsint(p.bitfieldwidth)
		fi

		sprintf(str, "%.*s", int(p.uflags.ulength), &p.uflags.codes)
		print @str, p.uflags.ulength:"v", ichar(&p.uflags.codes):".*"
		dsstr(" UFLAGS:")
		dsstr(str)
		dsstr("-")
		dsint(p.uflags.ulength)

		if p.code then
			dsstr("/:=")
			gs_strvar(ds, strexpr(p.code))
		fi

	when procid then

		dsstr("Index:")
		dsint(p.fnindex)

		dsstr(" Nret:")
		dsint(p.nretvalues)

	when dllprocid then
		dsstr("Index/PCaddr:")
		dsint(p.fnindex)
		if p.truename then
			dsstr(" Truename:")
			dsstr(p.truename)
		fi

	when staticid then
		if p.code then
			dsstr("=")
			gs_strvar(ds, strexpr(p.code))
		fi

	when frameid then
		if p.code then
			dsstr(":=")
			gs_strvar(ds, strexpr(p.code))
		fi

	when constid then
		dsstr("Const:")
		gs_strvar(ds, strexpr(p.code))

!	when enumid then
!		dsstr("Enum:")
!		dsint(p.index)
!	when dllmoduleid then
!		dsstr("DLL#:")
!		dsint(p.dllindex)
	esac

	if p.atfield then
		dsstr(" @")
		dsstr(p.equivfield.name)
		dsstr(" +")
		dsint(p.equivoffset)
	fi
	if p.equivvar then
		gs_strvar(ds, strexpr(p.equivvar))
	fi

!dsstr(" Module# ")
!dsint(p.moduleno)
!
	dsstr(" Lineno: ???")
!dsint(p.lineno iand 16777215)

	gs_println(ds, f)

	case p.nameid
	when constid, frameid, staticid, macroid then
		if p.code then
			printunit(p.code, dev:f)
		fi
	esac
end

global proc printstflat(filehandle f)=
symbol p
println @f, "GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
	p:=hashtable[i]
	if p=nil then nextloop fi

!	IF P.NEXTDUPL=NIL THEN NEXTLOOP FI

	case p.symbol
	when namesym then
		println @f, i:"5", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
		p:=p.nextdupl
		while p do
			print @f, "     ", p, p.name, symbolnames[p.symbol],,":",,namenames[p.nameid]
			if p.owner then
				fprint @f, " (From #:#)", p.owner.name, namenames[p.owner.nameid]
			fi

			println @f

			p:=p.nextdupl
		od
	esac
od
end

global proc printcode(filehandle f, ichar caption)=
ref strec p
ref procrec pp

pp:=proclist

while pp do
	p:=pp.def

	print @f, p.name,,"=", (p.scope|"Sub", "Prog", "Exp"|"Mod")
	if p.owner.nameid=typeid then
		print @f, " in record", p.owner.name
	fi
	println @f
	printunit(p.code, 0, 1, dev:f)
	println @f
	pp:=pp.nextproc
od
end

global proc printunit(ref unitrec p, int level=0, number=0, filehandle dev=nil)=
!p is a tagrec
	ref unitrec q
	ref strec d
	int t
	ichar idname
	i64 a
	r32 x32
	static int cmpchain=0

	if p=nil then
		return
	fi

	if p.pos then
		currlineno:=getlineno(p.pos)
		currfileno:=p.fileno
	fi

!	print @dev, p, ":"
	print @dev, getprefix(level, number, p)

	idname:=jtagnames[p.tag]
	print @dev, idname,,": "

	case p.tag
	when jname then
		d:=p.def

		print @dev, d.name
!		print @dev, d.name, namenames[d.nameid]

!		if d.code then
!			print @dev, " {",,jtagnames[d.code.tag],,"}"
!		fi

!		print @dev, " ",,getdottedname(d)!, q
!		print @dev, (p.dottedname|" {Dotted}"|"")

		if p.avcode then
			print @dev, " AV:", p.c:"c"
		fi

!		print @dev, " Moduleno:", p.moduleno
!
!!		if p.avcode then print @dev, " AV:", char(p.avcode) fi
!		if p.avcode then print @dev, " AV:", char(p.avcode), $ fi

	when jlabeldef then
		println @dev, p.def.name, p.def.labelno

	when jconst then
		t:=p.mode
		a:=p.value
		if t=trefchar then
			if p.slength>256 then
				print @dev, """",,"1:(longSTR)", """ *",,p.slength
			elsif p.slength then
				print @dev, """",,p.svalue,,""" *",,p.slength
			else
				print @dev, """"""
			fi

		elsecase ttbasetype[t]
		when ti64, ti32, ti16, ti8 then print @dev, i64(a)
		when tu64, tu32, tu16, tu8 then print @dev, u64(a)
		when tc64, tc8 then print @dev, chr(a)

		when tr32, tr64 then
			print @dev, p.xvalue
		when tref then
			if p.value then
				print @dev, "#",,p.value, P.SLENGTH
			else
				print @dev, "NIL"
			fi
		when tbool then
			print @dev, (p.value|"True"|"False")
		when tarray then
			print @dev, "<ARRAY>", =P.STRTYPE, =P.SLENGTH
		else
			println =typename(t), typename(ttbasetype[t])
			PRINT @DEV, "<PRINTUNIT BAD CONST PROBABLY VOID"
		fi
		print @dev, " ",,typename(t)
		if p.isastring then
!			print @dev, " <isstr>"
			fprint @dev, " <isstr>(#)", p.strtype
		fi

	when jtypeconst then
		print @dev, typename(p.value)

	when jbitfield then
		print @dev, bitfieldnames[p.bfcode]+3

	when jconvert, jtypepun, jtruncate, jfix, jfloat, jfwiden, jfwiden, jfnarrow then
		print @dev, " From mode:", strmode(p.oldmode)

	when jmakelist then
		print @dev, "Len:", p.length

	when jdot then
		print @dev, "Offset:", p.offset

	when jindex, jptr then

	when jexit, jredo, jnext then
		print @dev, "#",,p.loopindex

	when jsyscall then
		print @dev, sysfnnames[p.fnindex]+3

	when joperator then
		print @dev, pclnames[p.pclop]

!	when jmakeset then
	when jcmpchain then
		for i to p.cmpgenop.len do
			if p.cmpgenop[i]=0 then exit fi
			print @dev, ccnames[p.cmpgenop[i]],," "
		od
	esac

	if p.isconst then
		print @dev, " Is const"
!	else
!		print @dev, " Not const"
	fi

	case p.tag
	when jbin, jbinto, junary, junaryto, jincr then
		if p.pclop then
			fprint @dev, " <#>", pclnames[p.pclop]
			if p.pclop in [kmaths, kmaths2] then
				fprint @dev, " (#)", mathsnames[p.mathsop]
			fi
		else
			fprint @dev, " no-op"
		fi
	when jprop then
		fprint @dev, " Prop<#>", propnames[p.propcode]
	when jcmp then
		fprint @dev, " <#>", ccnames[p.condcode]
	esac


	println @dev

	for i to jsubs[p.tag] do
		printunitlist(dev, p.abc[i], level+1, i)
	od
end

proc printunitlist(filehandle dev, ref unitrec p, int level=0, number=0)=
	if p=nil then return fi

	while p do
		printunit(p, level, number, dev)
		p:=p.nextunit
	od
end

func getprefix(int level, number, ref unitrec p)ichar=
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr
	ichar isexpr

	indentstr[1]:=0
	if level>10 then level:=10 fi

	to level do
		strcat(indentstr, tabstr)
	od

	isexpr:="S"
	if jisexpr[p.tag] then isexpr:="E" fi

	case p.tag
	when jif, jswitch, jcase, jselect then
		if p.mode=tvoid then
			isexpr:="S"
		fi
	esac

!	fprint @modestr, "# #:#", isexpr, (p.resultflag|"RES"|"---"), strmode(p.mode)
	fprint @modestr, "# #:#", isexpr, (p.resultflag|"RES"|"---"), strmode(p.mode)
	modestr[256]:=0

	strcat(modestr, "-----------------------------")
	modestr[17]:=' '
	modestr[18]:=0

	str[1]:=0
	strcpy(str, getlineinfok())
	strcat(str, modestr)
	strcat(str, indentstr)
	strcat(str, strint(number))
!	if prefix^ then
		strcat(str, " ")
!	fi

	return str
end

func getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @str, "# # ", CURRFILENO:"Z2", currlineno:"z4"
	return str
end

global proc printmodelist(filehandle f)=
	int mbase
	static ichar tab="\t"

!	PRINTLN @F, =NTYPENAMES
!	FOR I TO NTYPENAMES DO
!		PRINTLN @F, I, TYPENAMES[I].DEF.NAME
!	OD
!	PRINTLN @F
!
	println @f, "MODELIST", ntypes

	for m:=0 to ntypes do
		println @f, m:"4", strmode(m)
		mbase:=ttbasetype[m]

		println @f, tab, "Basetype:", mbase, strmode(mbase)
		println @f, tab, "ttname:", ttname[m]
		println @f, tab, "ttnamedef:", ttnamedef[m], (ttnamedef[m]|ttnamedef[m].name|"-")
		println @f, tab, "Target:", strmode(tttarget[m])
		println @f, tab, "Size:", ttsize[m], "Sizeset", ttsizeset[m]
		fprintln @f, "# Bounds: #..#  Length:#", tab, ttlower[m], ttlower[m]+ttlength[m]-1, ttlength[m]
		if mbase=ttuple then
			print @f, tab, "Mult:"
			for i to ttlength[m] do print @f, strmode(ttmult[m, i]),," " od
			println @f
		fi
		println @f, tab, "Signed:", ttsigned[m]
		println @f, tab, "Isreal:", ttisreal[m]
		println @f, tab, "Isinteger:", ttisinteger[m]
		println @f, tab, "Isshort:", ttisshort[m]
		println @f, tab, "Isref:", ttisref[m]
		println @f
	od
end

global proc showprojectinfo(filehandle dev)=
	imodule pm
	isubprog ps
	static ichar tab="    "
	ichar s
	byte isfirst, ismain

	println @dev, "Project Structure:"
	println @dev, "---------------------------------------"
	println @dev, "Modules", nmodules
	for i to nmodules do
		pm:=modules[i]

		if i>1 and pm.subprogno<>modules[i-1].subprogno then
			println @dev
		fi
		ps:=subprogs[moduletosub[i]]

			isfirst:=ps.firstmodule=i
			ismain:=ps.mainmodule=i

			if isfirst and ismain then s:="hm"
			elsif isfirst then s:="h "
			elsif ismain then s:="m "
			else s:="  " 
			fi

			print @dev, tab, i:"2", s, 
			pm.name:"16jl", "Sys:", pm.issyslib, 
			"Sub:", subprogs[pm.subprogno].name, "Fileno:", pm.fileno

		if pm.stmacro then
			print @dev, " Alias:", pm.stmacro.name
		fi
		if pm.stmain then
			print @dev, $, pm.stmain.name, ":", scopenames[pm.stmain.scope], pm.stmain
		fi
		if pm.ststart then
			print @dev, $, pm.ststart.name, ":", scopenames[pm.ststart.scope], pm.ststart
		fi

		println @dev
	od
	println @dev

	println @dev, "Subprograms", nsubprogs, =mainsubprogno
	for i to nsubprogs do
		ps:=subprogs[i]
		println @dev, tab, i, ps.name, "Sys:", ps.issyslib!, =PS.STSUBPROG

		if ps.firstmodule then
			print @dev, tab, tab
			for j:=ps.firstmodule to ps.lastmodule do
				print @dev, $, modules[j].name, "(", MODULES[J].STSUBPROG, ")"
			od
			println @dev
		fi
	od
	println @dev

	println @dev, "Sourcefiles", nsourcefiles
	ifile pf
	for i to nsourcefiles do
		pf:=sources[i]
		fprintln @dev, "  #:  Name=# File=# Path=# Spec=# Size=#", 
			i:"2", pf.name:"jl16", pf.filename:"jl18", pf.path:"20jl", pf.filespec:"30jl", pf.size:"7"
	od
	println @dev

	println @dev, "Link files", nlibfiles
	for i to nlibfiles do
		println @dev, tab, libfiles[i]:"16jl"
	od
	println @dev
end

global proc showlogfile=
	[256]char str
	filehandle logdev
	int size
	ref strbuffer ss

	return unless fshowdiags

	logdev:=fopen(logfile, "w")

	if fshowmodules then showprojectinfo(logdev) fi

	if fshowasm and passlevel>=mcl_pass then
		if ctarget then
			println @logdev, "PROC CLANG"
			addtolog(changeext(outfile, "c"), logdev)
		else
			println @logdev, "PROC ASSEMBLY"
			addtolog(changeext(outfile, "asm"), logdev)
		fi
	fi

	if fshowpcl and passlevel>=pcl_pass then
		println @logdev, "PROC PCL"
		addtolog("PCL", logdev)
	fi

	if fshowss and passlevel>=obj_pass then
		addtolog("SS", logdev)
	fi

	if fshowast3 and passlevel>=type_pass then addtolog("AST3", logdev) fi
	if fshowast2 and passlevel>=name_pass then addtolog("AST2", logdev) fi
	if fshowast1 and passlevel>=parse_pass then addtolog("AST1", logdev) fi

	if fshowst then
		showsttree("SYMBOL TABLE", logdev)
	fi
	if fshowstflat then
		showstflat("FLAT SYMBOL TABLE", logdev)
	fi
!
	if fshowtypes then
		printmodelist(logdev)
	fi

	size:=getfilesize(logdev)
	fclose(logdev)

	if size then
CPL "PRESS KEY..."; if OS_GETCH()=27 then stop fi
		print @str, "\\m\\ed.bat ", logfile

		if checkfile("mm.m") then
			os_execwait(str, 0, nil)
		else
			println "Diagnostic outputs written to", logfile
		fi
	fi
end

proc showstflat(ichar caption, filehandle f)=
	println @f, "PROC", caption
	printstflat(f)
	println @f
end

proc showsttree(ichar caption, filehandle f)=
	println @f, "PROC", caption
	printst(f, stprogram)
	println @f

	println @f, "Proc List:"
	ref procrec pp:=proclist
	while pp do
		symbol d:=pp.def
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
		pp:=pp.nextproc
	od
	println @f, "End\n"

	println @f, "DLL Proc List:"
	for i to ndllproctable do
		d:=dllproctable[i]
		fprintln @f, "#	#.# (#) Mod:", d, d.owner.name, d.name:"20jl", namenames[d.nameid], 
			d.moduleno
	od
	println @f, "End\n"
end

global proc showast(ichar filename)=
	filehandle f

	f:=fopen(filename, "w")
	return unless f

	println @f, "PROC", filename
	printcode(f, "")
	println @f
	fclose(f)
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s", symbolnames[l.symbol])

	switch l.symbol
	when namesym then
		printstrn(l.symptr.name, l.symptr.namelen)

		if l.subcode then
			fprint " [#]", symbolnames[l.subcode]
		fi

	when intconstsym then
		case l.subcode
		when tint then print l.value, "int"
		when tword then print l.uvalue, "word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """"
		printstr(l.svalue)
		print """", strlen(l.svalue)

	when charconstsym then
		print "'"
		printstr(l.svalue)
		print "'"

	when assignsym, addrsym, ptrsym, rangesym, 
		andlsym, orlsym, eqsym, cmpsym, addsym, subsym, 
		mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym, shlsym, shrsym, 
		minsym, maxsym, powersym then
		print symbolnames[l.symbol], =L.SUBCODE
!	elsif l.subcode then
!	ELSE
	elsif l.subcode then
		fprint "SUBCODE:", l.subcode
!	fprint "#", symbolnames[l.subcode]
	end

	println $, =lx.fileno

end

proc showtime(ichar caption, int t)=
	fprintln "# # ms # %", caption:"12jl", t:"5", (t*100.0)/compiletime:"5.1jr"
end

global proc showtimings=
	endclock:=os_clock()
	compiletime:=endclock-startclock
!
	showtime("Load:", 		loadtime)
	showtime("Parse:", 		parsetime)
	showtime("Resolve:", 	resolvetime)
	showtime("Type:", 		typetime)
	showtime("PCL:", 		pcltime)
	showtime("MCL:", 		mcltime)
	showtime("SS:", 			sstime)
	showtime("EXE:", 		exetime)
	println "-----------------------------"
	showtime("Total:", 		compiletime)
end

proc dsstr(ichar s)=
	gs_str(ds, s)
end

proc dsint(int a)=
	gs_strint(ds, a)
end

=== mm_export_dummy.m 0 0 9/29 ===
!hello

global proc writeexports(ichar basefile, modulename)=
end
=== mm_libsources_dummy.m 0 0 10/29 ===
global const fsyslibs = 0

global proc loadbuiltins=
end
=== mm_modules.m 0 0 11/29 ===
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
	[maxsubs]ichar subpaths
	int nmods:=0, nsubs:=0, hdrcode
	int firstmod, lastmod, issyslib:=0
	imodule pm
	symbol d, stalias
	ichar path, name, ext, file2
	byte proj:=0, sepheader:=0

!CPL "LOADSP", =EXTRACTBASEFILE(FILENAME), =SYSLIBNAME

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
				subpaths[nsubs]:=path

			when hdr_linkdll then
				checksymbol(namesym)
				addlib(lx.symptr.name)

			when hdr_sourcepath then
				checksymbol(stringconstsym)
				unless loadedfromma then			!ignore paths for .ma
					path:=pcm_copyheapstring(lx.svalue)
				end
!CPL "SET PATH", PATH

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
!		loadsp(getmodulefilename(path, subnames[i]))
		loadsp(getmodulefilename(subpaths[i], subnames[i]))
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

!CPL "LOADMOD",FILESPEC, =ISSYSLIB
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

!CPL "LSF1", =FILENAME, =NSOURCEFILES, =ISSYSLIB

!look for file already loaded, or preloaded due to built-in syslib or .ma file:
	for i to nsourcefiles do
!CPL "LOOP", I, FILENAME, SOURCES[I].FILENAME, SOURCES[I].ISSYSLIB
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
!CPL "LSF", FILESPEC


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
	ichar name

	if dointlibs then				!bundled sys files
		str[1]:=0
	else
		strcpy(str, langhomedir)
	fi

	case msyslevel
	when 0 then
		return
	when 1 then
		name:=(ctarget or not os_iswindows()|"msysminc"|"msysmin")
	else				!full syslib
!CPL "FULLSYS"
		if os_iswindows() and not clinux then	!run on Windows
!CPL "WINDOWS", =CTARGET, FRUNTCL,=FGENTCL
			if ctarget then
				name:="msyswinc"
			else
!				name:="msyswin"
				NAME:="MSYSWINI"				!TEMPORARY FOR TCL TARGET
			fi
		else									!on Linux, or generating C for Linux on Windows
!CPL "LINUX"
			name:="msyslinc"
		fi
	esac

	strcat(str, name)

	SYSLIBNAME:=PCM_COPYHEAPSTRING(STR)
!IF FVERBOSE>=2 THEN
!	CPL =SYSLIBNAME
!FI
	strcat(str, ".m")


!CPL "SYSLIB:",STR

	loadsp(str)
end

global proc loadproject(ichar file)=
	[300]char str
	ichar file2

	int tt:=clock()
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
CPL "LOADING FROM MA FILE"
		loadmafile(file)
		loadedfromma:=1
		strcpy(str, changeext(file,"m"))			!assume lead module has same name as ma file
		file:=str
	fi

	loadsp(file, 1)

	addlib("msvcrt")
	if os_iswindows() then
		addlib("user32")
		addlib("gdi32")
		addlib("kernel32")
	end

	loadtime:=clock()-tt
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

	readln @&str[1]
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


=== mm_support.m 0 0 12/29 ===
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
	println "Syntax Error:",MESS

	showerrorsource(lx.pos, currproc)

	println mess

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

proc showdivider(c64 ch)=
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
	fprint @str,mess,a
	serror_gen(str)
end

global proc error_gen(int pass,ichar mess,unit p=nil)=
!general error handling for passes name, type and code gen
!pass='N' 'T' or 'G'
	int pos

	if p then
CPL "P.POS"
		pos:=p.pos
	else
		pos:=mmpos
	fi

	showdivider('*')
	case pass
	when 'N' then println "RX Name Error: "
	when 'T' then println "TX Type Error: "
	when 'G' then println "GX Code Gen Error: "
	when 'A' then println "AX Code Gen Error: "
	esac

	showerrorsource(pos, currproc)

	println mess

	stopcompiler(sources[getfileno(pos)].filespec,getlineno(pos))
end

global proc rxerror(ichar mess,unit p=nil)=
	error_gen('N',mess,p)
end

global proc gerror(ichar mess,unit p=nil)=
	error_gen('G',mess,p)
end

global proc gerroru(ichar mess,unit p)=
	[256]char str
	println @str, mess,,": Unsupported tag:", jtagnames[p.tag]
	error_gen('G', str, p)
end

global proc gerrorop(ichar mess, unit p)=
	[256]char str
	println @str, mess,,": Unsupported op:", pclnames[p.pclop]
	error_gen('G', str, p)
end

global proc txerror(ichar mess,unit p=nil)=
	error_gen('T',mess,p)
end

global proc txerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @str,mess,a
	error_gen('T',str,p)
end

global proc txerror_ss(ichar mess,a,b)=
	[256]char str
	fprint @str,mess,a,b
	error_gen('T',str)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
	[256]char str
	fprint @str,mess,a
	error_gen('N',str,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
	[256]char str

	fprint @str,mess,s
	error_gen('G',str,p)
end

global proc lxerror_gen(ichar mess)=

	println "On line",getlineno(lx.pos),"in file",sources[lx.fileno].filespec

	println
	println "**** Lex Error:",mess,"****"
	println

	stopcompiler(sources[lx.fileno].filespec,getlineno(lx.pos))
end

global proc lxerror(ichar mess)=
	lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
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

!CPL "GETSUPP1", FILENAME, =PATH

	file:=filename

	if fverbose=3 then
		fprintln "Get file:# (ext:#) (path:#)",filename,ext, path
	fi

	if ext^ then
		strcpy(filespec,addext(filename,ext))
		file:=filespec
	fi

	if loadedfromma then
		file:=pcm_copyheapstring(extractfile(file))
	fi	

	for i to nsourcefiles do
		if eqstring(file, sources[i].filename) and not sources[i].issyslib then
			return sources[i]
		fi
	od

	if not isabspath(file) then
		strcpy(filespec2,path)
		strcat(filespec2,file)
		file:=filespec2
	fi

	if fverbose=3 and fileno then
		println "Checkfile:",file
	fi

!CPL =FILE
!CPL =FILENAME

	if file=nil or not checkfile(file) then
		loaderror("Can't find file: ",file)
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
!
!CPL =FILENO
!CPL =POS.[0..23]

	if fileno<1 or fileno>nsourcefiles then
!		RETURN 1
		abortprogram("No file no")
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
	filename:=sources[getfileno(pos)].filespec

	lineno
end


global proc do_writema(ichar inpfile)=
	[300]char filename
	[maxsourcefile]int sflist
	filehandle f
	int offset, nfiles, fileno
	ifile pf

	return unless passlevel=ma_pass

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
		pf:=sources[sflist[i]]

		fprintln @f,"=== # # # #/# ===",
			pf.filename,
			pf.issyslib,
			pf.issupport,
			i,nfiles

		offset:=getfilepos(f)
		writerandom(f,cast(pf.dupl),offset,pf.size)
	od

	println @f,"=== END ==="

	for i to nfiles do
		pf:=sources[sflist[i]]
		println @f,i,pf.filename, pf. issyslib, pf.issupport
	od

	fclose(f)
	stop
end

global proc do_getinfo(ichar filename)=
	filehandle f
	ichar fs
	imodule pm

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

end
=== mm_tables.m 0 0 13/29 ===
!include "mm_types.m"

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

	(sf_getnprocs,			$,	0,	1),		!access funcs
	(sf_getprocname,		$,	0,	1),
	(sf_getprocaddr,		$,	0,	1),

	(sf_power_i64,			$,	0,	1),
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

!Key
!   A B C		Indicate single subnodes
!   A*			Etc, means a subnode can be a list
!   A+			Etc, means a list with one or two extra elements, so A2, A3

!                       N Expr  Solo
	(jnull=0,		$+1,	0,	3,	0), ! Place holder unit: means 'param no present' when used where a param is expected
	(jconst,		$+1,	0,	3,	0), ! int/real/string/range in .value/.xvalue/.svalue etc
	(jname,			$+1,	0,	3,	0), ! .def = ST entry
	(jblock,		$+1,	1,	0,	1), ! A*
	(jstrinclude,	$+1,	1,	3,	0), ! A; should be const string

!Logical Operators

	(jandl,			$+1,	2,	2,	0), ! A and B; A/B must be bools otherwise ISTRUE applied
	(jorl,			$+1,	2,	2,	0), ! A or B

	(jnotl,			$+1,	1,	1,	0), ! not A; A must be a bool (compiler generates jisfalsel if not)
	(jistruel,		$+1,	1,	1,	0), ! istrue A  zero/not-zero => 0/1
	(jisfalsel,		$+1,	1,	1,	0), ! isfalse A zero/not-zero => 1/0

!Expressions and Operators

	(jmakelist,		$+1,	2,	3,	0), ! (B: A*); B is lower bound; .length=# elements
	(jmakerange,	$+1,	2,	3,	0), ! A..B
	(jmakeset,		$+1,	1,	3,	0), ! [A*]; .length=# elements
	(jmakeslice,	$+1,	2,	3,	0), ! slice(A, B) A=ptr, B=length
	(jreturnmult,	$+1,	1,	0,	0), ! A*; uses .length; (set in TX from return/makelist

	(jkeyword,		$+1,	2,	3,	0), ! def=st entry
	(jdim,			$+1,	2,	3,	0), ! [A:] or [A:B] (set array dims by length)
	(jassign,		$+1,	2,	3,	1), ! A := B
	(jassignmm,		$+1,	2,	3,	1), ! (A*) := (B*); .length
	(jassignms,		$+1,	2,	3,	1), ! (A*) := B; .length
	(jassignmdrem,	$+1,	2,	3,	1), ! (A+) := B; B should be x divrem y 
	(jcall,			$+1,	2,	3,	1), ! A(B*)

	(jcmp,			$+1,	2,	2,	0), ! A cc B
	(jcmpchain,		$+1,	2,	1,	0), ! A* uses .cmpgenop/.cmpmode
	(jbin,			$+1,	2,	2,	0), ! A op B
	(junary,		$+1,	2,	1,	0), ! op A
	(jprop,			$+1,	2,	1,	0), ! prop A
	(jbinto,		$+1,	2,	2,	0), ! A op:= b
	(junaryto,		$+1,	1,	1,	0), ! op:= A
	(jincr,			$+1,	1,	3,	0), ! op A, A op

	(jinrange,		$+1,	2,	2,	0), ! A in B (B is range)
	(jinset,		$+1,	2,	2,	0), ! A in B (B is set)

	(jindex,		$+1,	2,	3,	0), ! A[B]
	(jslice,		$+1,	2,	3,	0), ! A[B..C]

	(jdot,			$+1,	2,	3,	0), ! A.B (B usu name); uses .offset in later stages
	(jdotindex,		$+1,	2,	3,	0), ! A.[B]
	(jdotslice,		$+1,	2,	3,	0), ! A.[B] (B must be jmakerange)

	(jptr,			$+1,	1,	3,	0), ! A^
	(jaddrof,		$+1,	2,	3,	0), ! &A

!NOTE conversion handling in the frontend needs to be reviewed, especially
!type punning

	(jconvert,		$+1,	1,	3,	0), ! Used internally; becomes specific mode in tx pass

	(jshorten,		$+1,	1,	3,	0), ! A; convert type of A to type of this node;
										! seems to be used in init data only; must be compile-time
	(jautocast,		$+1,	1,	3,	0), ! A (changed to jconvert by tx pass)
	(jtypepun,		$+1,	1,	3,	0), ! THIS NEEDS REVISING
	(jwiden,		$+1,	1,	3,	0), !
	(jfwiden,		$+1,	1,	3,	0), !
	(jfnarrow,		$+1,	1,	3,	0), !
	(jfix,			$+1,	1,	3,	0), !
	(jfloat,		$+1,	1,	3,	0), !
	(jtruncate,		$+1,	1,	3,	0), !
	(jtoboolt,		$+1,	1,	3,	0), !

	(jtypeconst,	$+1,	0,	3,	0), ! .value is the type code. The node itself has type i64
	(joperator,		$+1,	0,	3,	0), ! Uses .pclop

	(jbitwidth,		$+1,	1,	1,	0), ! A.bitwidth
	(jbytesize,		$+1,	1,	1,	0), ! A.bytes
	(jtypestr,		$+1,	0,	1,	0), ! A.typestr
	(jbitfield,		$+1,	1,	3,	0), ! A.odd etc (uses .bfcode)

	(jminvalue,		$+1,	1,	3,	0), ! A.min
	(jmaxvalue,		$+1,	1,	3,	0), ! A.max

	(jcompilervar,	$+1,	0,	3,	0), ! Uses .cvindex to denote compiler var)
	(jfmtitem,		$+1,	2,	3,	0), ! A:B within print items
	(jnogap,		$+1,	0,	3,	0), ! 
	(jspace,		$+1,	0,	3,	0), ! 

!Statements

	(jreturn,		$+1,	1,	0,	0), ! return [A]
	(jsyscall,		$+1,	1,	3,	1), ! FN(A*); .fnindex = sysfn no.

	(jto,			$+1,	3,	0,	0), ! to A do B od
	(jif,			$+1,	3,	3,	1), ! if A then B [else C] fi
	(jforup,		$+1,	3,	0,	0), ! for A := B+     to B2 [by B3] do C+ [else C2] od
	(jfordown,		$+1,	3,	0,	0), ! for A := B+ downto B2 [by B3] do C+ [else C2] od
	(jforall,		$+1,	3,	0,	0), ! for[all] [i,]x in L do body [else e] od
									!    A1=i; A2=x; A3=L.lwb; A4=L.upb; B=L; B2={x:=L[I];C1=body; C2=E
	(jforallrev,	$+1,	3,	0,	0), ! Same but with inrev
	(jwhile,		$+1,	3,	0,	1), ! while A [,C] do B od
	(jrepeat,		$+1,	2,	0,	1), ! repeat A until B
	(jgoto,			$+1,	1,	0,	1), ! goto A
	(jlabeldef,		$+1,	0,	0,	0), ! A:
	(jexit,			$+1,	0,	0,	1), ! exit     .loopindex
	(jredo,			$+1,	0,	0,	1), ! redoloop .loopindex
	(jnext,			$+1,	0,	0,	1), ! nextloop .loopindex
	(jdo,			$+1,	1,	0,	1), ! do A od
	(jcase,			$+1,	3,	3,	1), ! case A <B* = whenthen chain> [else C] end
	(jdocase,		$+1,	3,	0,	1), ! Same as case
	(jwhenthen,		$+1,	2,	0,	0), ! when A* then B (part of internal case/switch)

	(jswitch,		$+1,	3,	3,	1), ! Same as case
	(jdoswitch,		$+1,	3,	0,	1), ! Same as case
	(jdoswitchu,	$+1,	3,	0,	1), ! Same as case
	(jdoswitchx,	$+1,	3,	0,	1), ! Same as case
	(jswap,			$+1,	2,	0,	1), ! swap(A, B)
	(jselect,		$+1,	3,	3,	1), ! (A | B* | C)
	(jrecase,		$+1,	1,	0,	0), ! recase A; must be const

	(jprint,		$+1,	2,	0,	1), ! print   [@A,] B*
	(jprintln,		$+1,	2,	0,	1), ! println [@A,] B*
	(jfprint,		$+1,	3,	0,	1), ! print   [@A,] B, C*   B is fmtstr
	(jfprintln,		$+1,	3,	0,	1), ! println [@A,] B, C*   B is fmtstr
	(jread,			$+1,	2,	0,	1), ! read A*
	(jreadln,		$+1,	2,	0,	1), ! readln [@A] (items are in separate jread node)
	(jstop,			$+1,	1,	0,	0), ! stop [A]
	(jeval,			$+1,	1,	3,	1), ! eval A
	(jclear,		$+1,	1,	1,	1), ! clear A
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

global enumdata []ichar cvnames =
	(cv_lineno,		$),
	(cv_strlineno,	$),
	(cv_modulename,	$),
	(cv_filename,	$),
	(cv_func	,	$),
	(cv_date,		$),
	(cv_time,		$),
	(cv_version,	$),
	(cv_typename,	$),
	(cv_nil,		$),
	(cv_pi,			$),
	(cv_infinity,	$),
	(cv_true,		$),
	(cv_false,		$),
end

!!---
global enumdata []ichar symbolnames, []byte symbolgenops, []byte exprstarter,
 []byte symboladdmul =

!First half are basic tokens returned by lexreadtoken()
!                                genops       expr
	(dotsym,			".",		0,			0,	0),
	(commasym,			",",		0,			0,	0),
	(semisym,			";",		0,			0,	0),
	(colonsym,			":",		0,			0,	0),
	(sendtosym,			"=>",		0,			0,	0),
	(pipesym,			"->",		0,			0,	0),
	(lbracksym,			"(",		0,			1,	0),
	(rbracksym,			")",		0,			0,	0),
	(lsqsym,			"[",		0,			1,	0),
	(rsqsym,			"]",		0,			0,	0),
	(lcurlysym,			"{",		0,			0,	0),
	(rcurlysym,			"}",		0,			0,	0),
	(ptrsym,			"^",		0,			1,	0),
	(barsym,			"|",		0,			0,	0),
	(atsym,				"@",		0,			0,	0),
	(addrsym,			"&",		0,			1,	0),
	(ellipsissym,		"...",		0,			0,	0),

	(assignsym,			":=",		0,			0,	0),
	(rangesym,			"..",		0,			0,	0),
	(addsym,			"+",		kadd,		1,	'A'),
	(subsym,			"-",		ksub,		1,	'A'),
	(mulsym,			"*",		kmul,		0,	'M'),
	(divsym,			"/",		kdiv,		0,	'M'),
	(idivsym,			"%",		kidiv,		0,	'M'),
	(iremsym,			"rem",		kirem,		0,	'M'),
	(idivremsym,		"divrem",	kidivrem,	0,	'M'),
	(iandsym,			"iand",		kbitand,	0,	'A'),
	(iorsym,			"ior",		kbitor,		0,	'A'),
	(ixorsym,			"ixor",		kbitxor,	0,	'A'),
	(shlsym,			"<<",		kshl,		0,	'M'),
	(shrsym,			">>",		kshr,		0,	'M'),
	(minsym,			"in",		kmin,		1,	'A'),
	(maxsym,			"max",		kmax,		1,	'A'),
	(andlsym,			"and",		0,			0,	0),
	(orlsym,			"or",		0,			0,	0),
	(xorlsym,			"xor",		0,			0,	0),

	(eqsym,				"=",		0,			1,	0),
	(cmpsym,			"cmp",		0,			1,	0),
	(powersym,			"**",		kpower,		0,	0),
	(insym,				"in",		0,			0,	0),
	(notinsym,			"notin",	0,			0,	0),
	(inrevsym,			"inrev",	0,			0,	0),

	(notlsym,			"not",		knot,		1,	0),
	(istruelsym,		"istrue",	0,			1,	0),
	(inotsym,			"inot",		kbitnot,	1,	0),
	(abssym,			"abs",		kabs,		1,	0),
	(signsym,			"sign",		ksign,		1,	0),
	(sqrtsym,			"sqrt",		ksqrt,		1,	0),
	(sqrsym,			"sqr",		ksqr,		1,	0),

	(propsym,			$,			0,			0,	0),
	(mathsopsym,		$,			0,			1,	0),		! sin etc
	(maths2opsym,		$,			0,			1,	0),		! atan2 etc

	(bitfieldsym,		$,			0,			0,	0),		! Special bit selections
	(eolsym,			$,			0,			0,	0),		! End of line
	(eofsym,			$,			0,			0,	0),		! Eof seen
	(rawxnamesym,		$,			0,			0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(incrsym,			$,			0,			1,	0),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,			0,			1,	0),		! 123 32 bits signed
	(realconstsym,		$,			0,			1,	0),		! 123.4 64 bits
	(charconstsym,		$,			0,			1,	0),		! 'A' or 'ABCD'
	(stringconstsym,	$,			0,			1,	0),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		$,			0,			0,	0),		! 
	(namesym,			$,			0,			1,	0),		! identifier symbol
	(kincludesym,		$,			0,			0,	0),		! INCLUDE
	(kstrincludesym,	$,			0,			1,	0),		! SINCLUDE/BINCLUDE

	(stdtypesym,		$,			0,			1,	0),		! INT, CHAR etc
	(kicharsym,			$,			0,			1,	0),		! ICHAR IVOID
	(kifsym,			$,			0,			1,	0),		! 
	(kthensym,			$,			0,			0,	0),		! 
	(kelsifsym,			$,			0,			0,	0),		! 
	(kelsesym,			$,			0,			0,	0),		! 
	(kelsecasesym,		$,			0,			0,	0),		! 
	(kelseswitchsym,	$,			0,			0,	0),		! 
	(kendsym,			$,			0,			0,	0),		! 
	(kunlesssym,		$,			0,			0,	0),		! 
	(kcasesym,			$,			0,			1,	0),		! CASE
	(kdocasesym,		$,			0,			0,	0),		! DOCASE
	(krecasesym,		$,			0,			0,	0),		! RECASE
	(kwhensym,			$,			0,			0,	0),		! 
	(kforsym,			$,			0,			0,	0),		! FOR
	(ktosym,			$,			0,			0,	0),		! TO/DOWNTO
	(kbysym,			$,			0,			0,	0),		! 
	(kdosym,			$,			0,			0,	0),		! 
	(kwhilesym,			$,			0,			0,	0),		! 
	(krepeatsym,		$,			0,			0,	0),		! 
	(kuntilsym,			$,			0,			0,	0),		! 
	(kreturnsym,		$,			0,			0,	0),		! 
	(kstopsym,			$,			0,			0,	0),		! 
	(kloopsym,			$,			0,			0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$,			0,			0,	0),		! GO/GOTO
	(kswitchsym,		$,			0,			0,	0),		! SWITCH
	(kdoswitchsym,		$,			0,			0,	0),		! DOSWITCH
	(kprintsym,			$,			0,			0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(kreadsym,			$,			0,			0,	0),		! READ/READLN
	(kprocsym,			$,			0,			0,	0),		! PROC
	(kfuncsym,			$,			0,			0,	0),		! FUNCTION
	(klabelsym,			$,			0,			0,	0),		! LABEL
	(krecordsym,		$,			0,			0,	0),		! RECORD
	(kstructsym,		$,			0,			0,	0),		! STRUCT
	(kunionsym,			$,			0,			0,	0),		! UNION
	(kimportmodulesym,	$,			0,			0,	0),		! IMPORTDLL/IMPORTMODULE
	(kprojectsym,		$,			0,			0,	0),		! PROJECT
	(ktypesym,			$,			0,			0,	0),		! TYPE
	(krefsym,			$,			0,			1,	0),		! REF
	(kvoidsym,			$,			0,			1,	0),		! VOID
	(kvarsym,			$,			0,			0,	0),		! MUT
	(kletsym,			$,			0,			0,	0),		! LET
	(kslicesym,			$,			0,			0,	0),		! SLICE/SLICE2D
	(kmacrosym,			$,			0,			0,	0),		! MACRO
	(kconstsym,			$,			0,			0,	0),		! 
	(kclearsym,			$,			0,			0,	0),		! CLEAR
	(kheadersym,		$,			0,			0,	0),		! MODULE
	(kglobalsym,		$,			0,			0,	0),		! global
	(kstaticsym,		$,			0,			0,	0),		! STATIC

	(kcastsym,			$,			0,			1,	0),		! CAST
	(compilervarsym,	$,			0,			1,	0),		! $lineno etc
	(dollarsym,			$,			0,			1,	0),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,			0,			0,	0),		! EVAL
	(ktabledatasym,		$,			0,			0,	0),		! tabledata
	(kclampsym,			$,			0,			1,	0),			! CLAMP
	(kswapsym,			$,			0,			0,	0),		! SWAP
	(ksyscallsym,		$,			0,			1,	0),		! $getprocname etc
end

global enumdata []ichar headerdirnames =
	(hdr_module,		$),
	(hdr_import,		$),
	(hdr_sourcepath,	$),
	(hdr_linkdll,		$),
end

global enumdata [0:]ichar scopenames=
	(Module_scope=0,	"Local"), ! 		!module
	(subprog_scope,		"Global"), ! 		!inter-subprog
	(program_scope,		"Program"), ! 		!inter-module
	(export_scope,		"Export"), ! 		!inter-program
end

global enumdata =
	million_unit,
	billion_unit,
end

global enumdata [0:]ichar namenames=
	(nullid=0,		$),		!Not assigned
	(programid,		$),		!Main root
	(subprogid,		$),
	(moduleid,		$),		!Current or imported module
	(dllmoduleid,	$),		!
	(typeid,		$),		!Type name in type, proc or module
	(procid,		$),		!Proc/method/func/op name
	(dllprocid,		$),		!Dll Proc/func name
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
	("doswitchx",	kdoswitchsym,	jdoswitchx),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
	("print",		kprintsym,		jprint),
	("println",		kprintsym,		jprintln),
	("fprint",		kprintsym,		jfprint),
	("fprintln",	kprintsym,		jfprintln),

	("cp",			kprintsym,		jprint),
	("cpl",			kprintsym,		jprintln),

	("read",		kreadsym,		jread),
	("readln",		kreadsym,		jreadln),
	("cast",		kcastsym,		jconvert),

	("function",	kfuncsym,	0),
	("func",		kfuncsym,	0),
	("proc",		kprocsym,		0),
	("fun",			kfuncsym,	1),

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

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("byte",		stdtypesym,		tu8),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

	("char",		stdtypesym,		tc8),
	("c8",			stdtypesym,		tc8),
	("c64",			stdtypesym,		tc64),

	("bool64",		stdtypesym,		tbool64),
	("bool",		stdtypesym,		tbool64),
	("bool8",		stdtypesym,		tbool8),

	("label",		stdtypesym,		tlabel),

	("slice",		kslicesym,		tslice),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),

	("$lineno",		compilervarsym,	cv_lineno),
	("$strlineno",	compilervarsym,	cv_strlineno),
	("$filename",	compilervarsym,	cv_filename),
	("$modulename",	compilervarsym,	cv_modulename),
	("$function",	compilervarsym,	cv_func),
	("$date",		compilervarsym,	cv_date),
	("$time",		compilervarsym,	cv_time),
	("$version",	compilervarsym,	cv_version),
	("$typename",	compilervarsym,	cv_typename),
	("nil",			compilervarsym,	cv_nil),
	("pi",			compilervarsym,	cv_pi),
	("true",		compilervarsym,	cv_true),
	("false",		compilervarsym,	cv_false),
	("infinity",	compilervarsym,	cv_infinity),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
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

	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		maths_sin),
	("cos",			mathsopsym,		maths_cos),
	("tan",			mathsopsym,		maths_tan),
	("asin",		mathsopsym,		maths_asin),
	("acos",		mathsopsym,		maths_acos),
	("atan",		mathsopsym,		maths_atan),
	("log",			mathsopsym,		maths_log),
	("log10",		mathsopsym,		maths_log10),
	("exp",			mathsopsym,		maths_exp),
	("round",		mathsopsym,		maths_round),
	("floor",		mathsopsym,		maths_floor),
	("ceil",		mathsopsym,		maths_ceil),

	("atan2",		maths2opsym,	maths_atan2),
	("fmod",		maths2opsym,	maths_fmod),

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

global enumdata [0:]ichar convnames =
	(kkerror=0,     $),
	(kkfloat,       $),
	(kkfix,         $),
	(kktruncate,    $),
	(kkwiden,       $),
	(kkfwiden,      $),
	(kkfnarrow,     $),
	(kksoftconv,    $),
	(kktoboolt,     $),
	(kkharderr,     $),

	(kksofttrun,    $),
	(kkichar2sl,    $),
	(kkax2slice,    $),
	(kkcx2ichar,    $),
end

global enumdata [0:]ichar ccnames, [0:]ichar ccshortnames =
	(no_cc=0,	"xx",	"?"),
	(eq_cc,		"eq",	" = "),
	(ne_cc,		"ne",	" <> "),
	(lt_cc,		"lt",	" < "),
	(le_cc,		"le",	" <= "),
	(ge_cc,		"ge",	" >= "),
	(gt_cc,		"gt",	" > "),
end

global enumdata [0:]ichar mathsnames =
	(none_m = 0,		$),
	(maths_sin,			$+6),
	(maths_cos,			$+6),
	(maths_tan,			$+6),
	(maths_asin,		$+6),
	(maths_acos,		$+6),
	(maths_atan,		$+6),
	(maths_log,			$+6),
	(maths_log10,		$+6),
	(maths_exp,			$+6),
	(maths_round,		$+6),
	(maths_ceil,		$+6),
	(maths_floor,		$+6),
	(maths_fract,		$+6),
	(maths_sign,		$+6),
	(maths_fmod,		$+6),
	(maths_atan2,		$+6),
end

!global enumdata [0:]ichar pclnames =
!	(knop=0,	$+1),
!
!	(kadd,		$+1),
!	(ksub,		$+1),
!	(kmul,		$+1),
!	(kdiv,		$+1),
!	(kidiv,		$+1),
!	(kirem,		$+1),
!	(kbitand,	$+1),
!	(kbitor,	$+1),
!	(kbitxor,	$+1),
!	(kshl,		$+1),
!	(kshr,		$+1),
!	(kmin,		$+1),
!	(kmax,		$+1),
!	(kidivrem,	$+1),
!  
!	(kmaths2,	$+1),
!	(kpower,	$+1),
!  
!	(kaddpx,	$+1),
!	(ksubpx,	$+1),
!	(ksubp,		$+1),
!
!	(kneg,		$+1),
!	(kabs,		$+1),
!	(kbitnot,	$+1),
!	(knot,		$+1),
!	(ktoboolt,	$+1),
!	(ktoboolf,	$+1),
!  
!	(ksqr,		$+1),
!  
!	(ksqrt,		$+1),
!
!	(kmaths,	$+1),
!	(ksign,		$+1),
!
!	(kfloat,    $+1),
!	(kfix,		$+1),
!	(ktruncate,	$+1),
!	(kfwiden,	$+1),
!	(kfnarrow,	$+1),
!	(kwiden,	$+1),
!  
!	(ktypepun,	$+1),
!
!	(ktobool,	$+1),
! 
!	(kincrto,	$+1),
!	(kdecrto,	$+1),
!	(kincrload,	$+1),
!	(kdecrload,	$+1),
!	(kloadincr,	$+1),
!	(kloaddecr,	$+1)
!end
!

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,krecordsym,
		kicharsym, kslicesym)

global [tr64..tc64, tr64..tc64]i16 softconvtable = (
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

global [jtagnames.bounds]byte isbooltag

global [symbolnames.bounds]byte binopset
global [symbolnames.bounds]byte keepeol

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

	for i in assignsym..notinsym do binopset[i]:=1 od

	for i in keepeol.bounds do
		if i in [commasym, lsqsym, lbracksym] or binopset[i] and 
				i not in [maxsym, minsym] then
			keepeol[i]:=1
		fi
	od

end

=== mm_lib.m 0 0 14/29 ===
int autotypeno=0
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

ref strbuffer jdest

global ichar framevarname			!normally nil, set to frame var def to display in comment

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

!IF ID NOT IN [FRAMEID, PARAMID] THEN
	p.nextdupl:=symptr.nextdupl
	symptr.nextdupl:=p
!FI
	p.firstdupl:=symptr

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

	u:=allocunitrec(jname)
	u.def:=p

	return u
end

global func createunit0(int tag)ref unitrec=
	ref unitrec u

	u:=allocunitrec(tag)
	return u
end

global func createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec(tag)
	u.a:=p
	return u
end

global func createunit2(int tag, ref unitrec p,q)ref unitrec=
	ref unitrec u

	u:=allocunitrec(tag)

	u.a:=p
	u.b:=q
	return u
end

global func createunit3(int tag, ref unitrec p,q,r)ref unitrec=
	ref unitrec u

	u:=allocunitrec(tag)
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

	q:=allocunitrec(jnull)
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

global func createconstunit(u64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec(jconst)
	u.value:=a
	u.mode:=t

	u.isconst:=1
	return u
end

global func createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec(jconst)
	u.svalue:=s
	u.mode:=trefchar
	u.isastring:=1

	if length=-1 then
		u.slength:=strlen(s)+1
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
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tarray
	ttlower[m]:=1
	ttdimexpr[m]:=dimexpr
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	return m
end

global func createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=
!lower is lower bound of array
	int atype,m

	atype:=tarray

	if typedefx=0 then		!anon type
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

	return m
end

global func nextautotype:ichar=
	static [32]char str

	print @str,"$T",,++autotypeno
	return str
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

	return m
end

global func createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
	int m

	if typedefx=0 then		!anon type
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tslice
	ttlower[m]:=lower
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

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
end

global func getdottedname(symbol p)ichar=
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	symbol owner

	strcpy(str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(str2,str)
		strcpy(str,owner.name)
		strcat(str,".")
		strcat(str,str2)
		owner:=owner.owner
	od
	return str
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
		print @str,"av_",,++nextavindex
	else
		print @str,"sv_",,++nextsvindex
	fi

	name:=pcm_copyheapstring(str)
	addnamestr(name)

	p:=getduplnameptr(owner,addnamestr(name),id)
	p.used:=1

	p.mode:=tint

	adddef(owner,p)
	return p
end

global proc unionstr_clear(ref uflagsrec u)=
	((ref u64(u))^:=0)		!clear flags and length togetjer
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
	ttlength[m]:=elementslen
	ttmult[m]:=pcm_alloc(elementslen*i32.bytes)
	for i to elementslen do
		storemode(owner,elements[i],ttmult[m,i])
	od

	return m
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
	int length

	if p=nil then
		return
	fi

	a:=p.a
	b:=p.b

	case p.tag
	when jconst then

		case ttbasetype[p.mode]
		when ti32,ti64,ti8,ti16 then
			getstrint(p.value,str)
		when tu32,tu64,tu8,tu16 then
			strcpy(str,strword(p.value))
		when tc8,tc64 then
			str[1]:=p.value
			str[0]:=0

		when treal,tr32 then
			print @str,p.xvalue
		when tref then
			if p.mode=trefchar and p.isastring then
				if p.slength>str.len/4 then
					strcpy(str,"2:(LONGstr)")
				else

					convertstring(p.svalue, str, p.slength)
				fi
				jadditem("""")
				jadditem(str)
				jadditem("""")
				return
			else
				print @str,ref void(p.value)
			fi
		else
			strcpy(STR,"<EVAL/CONST PROBABLY VOID>")
		esac
		jadditem(str)

	when jname then
		jadditem(p.def.name)

	when jbin,jcmp then

		strcpy(str,pclnames[p.pclop])
		jadditem("(")
		jevalx(a)
		jadditem(str)
		jevalx(b)
		jadditem(")")

	when junary, jistruel, jnotl then

		strcpy(str,pclnames[p.pclop])
		jadditem(str)
		jadditem("(")

		if a.tag=jtypeconst then
			jadditem(STRMODE(a.value))
		else
			jevalx(a)
		fi
		jadditem(")")

	when jprop then

		strcpy(str,propnames[p.propcode])
		jadditem(str)
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

		jadditem(strmode(p.oldmode))
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
	when jdim then
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

!	when jaddroffirst then
!		jadditem("")
!		jevalx(a)

	when jtypestr then
		jadditem("TYPESTR(")
		jevalx(a)
		jadditem(")")

!	when jcvlineno, jcvfilename, jcvmodulename then
	when jcompilervar then
		jaddstr("$")
		jaddstr(cvnames[p.cvindex]+3)

	when jbitfield then
		jevalx(a)
		jaddstr(".")
		jaddstr(bitfieldnames[p.bfcode])

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
	istrmode(m,expand,str)
	return str
end

global func strmode2(int m,expand=1)ichar=
	static [4096]char str
	istrmode(m,expand,str)
	return str
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
			gs_copytostr(strexpr(ttdimexpr[m]),strdim)
			fprint @dest,"@[#<#>",&strdim[1],m
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
			gs_copytostr(strexpr(ttdimexpr[m]),strdim)
			fprint @dest,"@#[#:]",prefix,&strdim[1]
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

!CPL "ADDSTATIC", D.NAME, D.OWNER.NAME

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

global func allocunitrec(int tag)ref unitrec=
	ref unitrec p

	p:=pcm_allocnfz(unitrec.bytes)
	p.tag:=tag
	p.pos:=lx.pos
	p.moduleno:=currmoduleno
	p.subprogno:=moduletosub[currmoduleno]
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

!CPL "GETALIGN", STRMODE(M), STRMODE(TTTARGET[M])

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when trecord then
		a:=ttnamedef[m].maxalign
		if a=0 then
CPL "GAL0"
			 a:=8
		 fi
!CPL "  //RECORD/MAXALIGN=",A
		return a
	when tslice then
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

global func storemode(symbol owner, int m, i32 &pmode)int =
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
	a:=p.a

	case a.tag
	when jname then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	esac
end

global func convertstring(ichar s, t, int length)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!length is that of s; final length may be up to 4 times as long

!returns actual length of t
	int c
	ichar t0:=t
	[16]char str

	to length do
		case c:=s++^
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
!		when 7,8,26,27 then
!			t++^:='<'
!			t++^:=c/10+'0'
!			t++^:=(c rem 10)+'0'
!			t++^:='>'
		elsif c in 32..126 then
			t++^:=c
		else
!			t++^:='\\'			!hex
!			t++^:='x'
!			print @str,c:"z2h"
!			t++^:=str[1]
!			t++^:=str[2]

			t++^:='\\'			!octal
!			t++^:='x'
			print @str,c:"z3x8"
			t++^:=str[1]
			t++^:=str[2]
			t++^:=str[3]
		esac
	od

	t^:=0

	return t-t0
end

global func getpclmode(int t)int u=
	u:=stdpcl[ttbasetype[t]]

	if u=tblock then
		case ttsize[t]
		when 8 then u:=tu64
		when 4 then u:=tu32
		when 2 then u:=tu16
		when 1 then u:=tu8
		esac
	fi
	return u
end

=== pcl_decls.m 0 0 15/29 ===


global enumdata  [0:]ichar stdnames,
        [0:]byte stdsize,
        [0:]byte stdpcl,
		[0:]byte stdsigned,
		[0:]byte stdint,
		[0:]byte stdfloat,
		[0:]byte stdwide,
		[0:]byte stdmin =				!minimum width for int types

!    type         name       bits     pcl     sign int flt  wide  min
    (tvoid=0,     "void",       0,    tvoid,	0,	0,	0,	0,	0),

    (tr64,        "r64",        8,    tr64,		0,	0,	1,	1,	0),
    (tr32,        "r32",        4,    tr32,		0,	0,	1,	0,	0),
    (ti64,        "i64",        8,    ti64,		1,	1,	0,	0,	ti64),
    (tu64,        "u64",        8,    tu64,		0,	1,	0,	0,	tu64),
    (tc64,        "c64",        8,    tu64,		0,	1,	0,	0,	tu64),

    (ti8,         "i8",          1,   ti8,		1,	1,	0,	0,	ti64),
    (ti16,        "i16",         2,   ti16,		1,	1,	0,	0,	ti64),
    (ti32,        "i32",         4,   ti32,		1,	1,	0,	0,	ti64),
    (tu8,         "u8",          1,   tu8,		0,	1,	0,	0,	tu64),
    (tu16,        "u16",         2,   tu16,		0,	1,	0,	0,	tu64),
    (tu32,        "u32",         4,   tu32,		0,	1,	0,	0,	tu64),

    (tblock,      "block",       8,   tblock,	0,	0,	0,	0,	0),
    (tvector,     "vector",      8,   tvector,	0,	0,	0,	0,	0),

!Above were basic PCL types. Below are extras used by the front end

    (tc8,         "c8",          1,   tu8,		0,	1,	0,	0,	tu64),
    (tbool8,      "b8",          1,   tu8,		0,	1,	0,	0,	tu64),
    (tbool64,     "bool64",     8,    tu64,		0,	1,	0,	0,	tu64),

    (tref,        "ref",        8,    tu64,		0,	0,	0,	0,	tu64),
    (trecord,     "rec",        0,    tblock,	0,	0,	0,	0,	0),
    (trange,      "range",     16,    tblock,	0,	0,	0,	0,	0),

    (tarray,      "array",       0,   tblock,	0,	0,	0,	0,	0),
    (tslice,      "slice",      16,   tblock,	0,	0,	0,	0,	0),

    (trefchar,    "ichar",       8,   tu64,		0,	0,	0,	0,	tu64),
    (trefbit,     "refbit",     16,   tu64,		0,	0,	0,	0,	tu64),

    (tauto,       "auto",        0,   tu64,		0,	0,	0,	0,	0),
    (tany,        "any",         0,   tu64,		0,	0,	0,	0,	0),
    (tproc,       "proc",        0,   tu64,		0,	0,	0,	0,	0),
    (tlabel,      "label",       0,   tu64,		0,	0,	0,	0,	0),
    (tbitfield,   "bitfl",       8,   tu64,		0,	0,	0,	0,	0),
    (ttuple,      "tuple",       0,   tu64,		0,	0,	0,	0,	0),
    (tpending,    "pend",        0,   tu64,		0,	0,	0,	0,	0),

    (tlast,       "last ",       0,   tvoid,	0,	0,	0,	0,	0),
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tbool	= tbool64

global const tfirstnum	= tr64
global const tlastnum	= tc64

global const maxtuplesize = 4

global int trefproc
global int treflabel
global int taddr = tu64			!for pcl use

!global record pclinforec =	!extra info for functions in strec
!	byte pcldepth			!max pcl opnd depth (will include high-args)
!	byte nparams			!copy of info in pstrec; may exclude not used?
!	byte nlocals
!	byte isleaf				!1 when there are no calls (explicit or implicit)
!
!	byte nmaxargs			!0, or maxargs of any call (may be capped at 4)
!	byte assemused			!1 if inline assembly used (can't optimise)
!	byte mcldone			!
!	byte hasblocks			!whether block modes are used (that means copyblock etc)
!end

global type pcl = ref pclrec

global record pclrec =
	pcl next

	byte opcode
	byte mode, mode2
	byte condcode						!for jumpcc/setcc

!	mclopnd a
	union
		i64		value
		r64		xvalue
		ichar	svalue			!also used for data
		int		labelno
		symbol	def
	end

	u32 size

	union						!two 32-bit params used according to opcode
		struct
			i16 x				!common access to these 1/2 extra attribs
			i16 y
		end

		struct					! (x,y) pointer ops
			i16 scale			! scale factor for offset
			i16 extra			! extra constant byte offset, already scaled
		end

		struct					! (x,y) call/etc
			i16 nargs			! number of args
			union
				i16 nvariadics	!call: 0, or arg # that is first variadic
				i16 nrealargs	!setcall: 1 if whole call sequence is simple
			end
		end

		struct					! (x,y) switch
			i16 minlab
			i16 maxlab
		end

		struct					! defproc/retproc/retfn
			i16 paramslots		! stack usage as 8-byte slots
			i16 localslots
		end

		i16 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
		i16 align
		i16 popone				! (x) jumpcc: leave X on stack
		i16 slicelwb			! (x) for .upb
		i16 inplace				! (x) for load, means &A operand is for inplace update
		i32 mathsop				! (x) for maths: maths_sin etc

	end

!	u16		lineno				!within function? Can use rel offset if func has full offset
	union
		byte	opndtype			!operand type (value, def etc)
		byte	optype				!alias used as pcl opnd
	end
	U16 SPARE
!	byte	reg						!when used as pcl opnd
!	byte	count
end

global type pclopnd = ref pcsrec

global record pcsrec =
	union
		struct
			byte mode				!mode (copy of pcl loadop to start)
			byte reg				!0: register holding operand
			byte temp				!0, or 1: pcl loadop in temp mem location
			byte code				!0, or 1: pclopnds[] element refers to PCL load instr
			byte count				!dupl count (zero to start)


!			u32 psize				!copy of pcl load op size to start
		end
		u64 all						!use to restore whole record at once
	end
end

global enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),
	(reg_opnd,			$),				!in reg (regvarset shows if regvar)

	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(int_opnd,			$),
	(real_opnd,			$),				!will be [label]
	(string_opnd,		$),				!will be [label]
	(label_opnd,		$),				!will be the address of the label, so lea r,[label]

!	(temp_opnd,			$),

!	(data_opnd,			$),
end

global enumdata [0:]ichar pclnames,
!enumdata [0:]ichar pclnames,
				[0:]byte pclhastype,			!where t or t/u are used (0/1/2)
				[0:]byte pclextra,				!uses .x or .x/.y
				[0:]byte pclargs =				!no. of call-args, incl ops that may use calls (9 = special)

!                       t  x          (attrs     )
	(knop=0,       $+1, 0, 0,  0),  ! (          ) 

	(kload,        $+1, 1, 1,  0),  ! (M L t i   ) Z' := M &M L &L 123 4.5 "abc"; i=1 for in-place ref
	(kiload,       $+1, 1, 0,  0),  ! (t         ) Z' := Z^
	(kiloadx,      $+1, 1, 2,  0),  ! (t d       ) Z' := (Y + Z*s + d)^

	(kstore,       $+1, 1, 0,  0),  ! (M t       ) M := Z
	(kistore,      $+1, 1, 0,  0),  ! (t         ) Z^ := Y
	(kistorex,     $+1, 1, 2,  0),  ! (t s d     ) (Y + Z*s + d)^ := X
	(kstorem,      $+1, 1, 0,  0),  ! (t         ) Z' :=(Y, Z) for mem:16

	(kdupl,        $+1, 0, 0,  0),  ! (          ) Z' := Y' := Z
	(kdouble,      $+1, 0, 0,  0),  ! (          ) Count extra instance of Z
	(kswapstk,     $+1, 0, 2,  0),  ! (a b       ) Swap(stack(a, 0,  0), stack(b)); 1/2/3/4 = Z/Y/X/W
	(kunload,      $+1, 1, 0,  0),  ! (t         ) Pop stack

	(kopnd,        $+1, 1, 0,  0),  ! (M L C t   ) Define auxiliary operand M or L
	(ktype,        $+1, 1, 0,  0),  ! (t         ) Define auxiliary type t

	(kloadbit,     $+1, 1, 0,  2),  ! (t         ) Z' := Y.[Z]
	(kloadbf,      $+1, 1, 0,  2),  ! (t         ) Z' := X.[Y..Z]
	(kstorebit,    $+1, 1, 0,  2),  ! (t         ) Y^.[Z] := X
	(kstorebf,     $+1, 1, 0,  2),  ! (t         ) X^.[Y..Z] := W

	(kcallp,       $+1, 0, 2,  9),  ! (M n v     ) Call &M with nargs, then pop args; v = varargs
	(kicallp,      $+1, 0, 2,  9),  ! (n v       ) Call Z with nargs, then pop args (a=n+1)
	(kretproc,     $+1, 0, 0,  0),  ! (          ) Return from proc
	(kcallf,       $+1, 1, 2,  9),  ! (M t n v   ) Call &M, then pop args, leave retval; v = varrgs
	(kicallf,      $+1, 1, 2,  9),  ! (t n v     ) Call Z, then pops args, leave retval (a=n+1)
	(kretfn,       $+1, 1, 0,  0),  ! (t         ) Return from func with Z=retval

	(kjump,        $+1, 0, 0,  0),  ! (L         ) goto L
	(kijump,       $+1, 1, 0,  0),  ! (          ) goto Z
	(kjumpcc,      $+1, 1, 1,  0),  ! (L t c p   ) goto L when Y c Z; p=1: Z':=Y (b=0/1)
	(kjumpt,       $+1, 1, 0,  0),  ! (L t       ) goto L when Z is true
	(kjumpf,       $+1, 1, 0,  0),  ! (L t       ) goto L when Z is false
	(kjumpret,     $+1, 1, 0,  0),  ! (L t       ) goto 0, common return point; deal with any ret value on stack
	(kjumpretm,    $+1, 1, 0,  0),  ! (L t n     ) goto 0, common return point; deal with any ret value on stack

	(ksetcc,       $+1, 1, 0,  0),  ! (t c       ) Z' := Y cc Z

	(kstop,        $+1, 0, 0,  0),  ! (          ) Stop Z

	(kto,          $+1, 1, 0,  0),  ! (L t       ) --B (aux); goto L when B<>0 
	(kforup,       $+1, 1, 1,  0),  ! (L t n     ) B+:=n; goto L when B<=C
	(kfordown,     $+1, 1, 1,  0),  ! (L t n     ) B-:=n; goto L when B>=C

	(kiswap,       $+1, 1, 0,  0),  ! (t         ) swap(Y^,Z^)

	(kswitch,      $+1, 1, 2,  0),  ! (L t x y   ) L=jumptab; B=elselab; x/y=min/max values
	(kswitchu,     $+1, 0, 2,  0),  ! (L x y     ) L=jumptab; B=elselab; x/y=min/max values
	(kswlabel,     $+1, 0, 0,  0),  ! (L         ) jumptable entry

	(kclear,       $+1, 1, 0,  0),  ! (t         ) Clear Z^

	(kadd,         $+1, 1, 0,  0),  ! (t         ) Z' := Y + Z
	(ksub,         $+1, 1, 0,  0),  ! (t         ) Z' := Y - Z
	(kmul,         $+1, 1, 0,  0),  ! (t         ) Z' := Y * Z
	(kdiv,         $+1, 1, 0,  0),  ! (t         ) Z' := Y / Z
	(kidiv,        $+1, 1, 0,  0),  ! (t         ) Z' := Y % Z
	(kirem,        $+1, 1, 0,  0),  ! (t         ) Z' := Y rem Z
	(kidivrem,     $+1, 1, 0,  0),  ! (t         ) Z' := divrem(Y, Z)
	(kbitand,      $+1, 1, 0,  0),  ! (t         ) Z' := Y iand Z
	(kbitor,       $+1, 1, 0,  0),  ! (t         ) Z' := Y ior Z
	(kbitxor,      $+1, 1, 0,  0),  ! (t         ) Z' := Y ixor Z
	(kshl,         $+1, 1, 0,  0),  ! (t         ) Z' := Y << Z
	(kshr,         $+1, 1, 0,  0),  ! (t         ) Z' := Y >> Z
	(kmin,         $+1, 1, 0,  0),  ! (t         ) Z' := min(Y, Z)
	(kmax,         $+1, 1, 0,  0),  ! (t         ) Z' := max(Y, Z)
	(kaddpx,       $+1, 1, 2,  0),  ! (t s d     ) Z' := Y + Z*s + d
	(ksubpx,       $+1, 1, 2,  0),  ! (t s d     ) Z' := Y - Z*s + s
	(ksubp,        $+1, 1, 1,  0),  ! (t s       ) Z' := (Y - Z)/s

	(kneg,         $+1, 1, 0,  0),  ! (t         ) Z' := -Z
	(kabs,         $+1, 1, 0,  0),  ! (t         ) Z' := abs Z
	(kbitnot,      $+1, 1, 0,  0),  ! (t         ) Z' := inot Z
	(knot,         $+1, 1, 0,  0),  ! (t         ) Z' := not Z
	(ktoboolt,     $+1, 2, 0,  0),  ! (t u       ) Z' := istrue Z; u is of type u; result is type t
	(ktoboolf,     $+1, 2, 0,  0),  ! (t u       ) Z' := not istrue Z
	(ksqr,         $+1, 1, 0,  0),  ! (t         ) Z' := sqr Z

	(ksqrt,        $+1, 1, 0,  0),  ! (t         ) Z' := sqrt Z
	(kmaths,       $+1, 1, 1,  1),  ! (t op      ) Z' := maths(Z)
	(kmaths2,      $+1, 1, 1,  2),  ! (t op      ) Z' := maths2(Y, Z)
	(ksign,        $+1, 1, 0,  1),  ! (t         ) Z' := sign Z

	(kpower,       $+1, 1, 0,  2),  ! (t         ) Z' := Y ** Z

	(kincrto,      $+1, 1, 1,  0),  ! (t n       ) Z^ +:= n
	(kdecrto,      $+1, 1, 1,  0),  ! (t n       ) Z^ -:= n
	(kincrload,    $+1, 1, 1,  0),  ! (t n       ) Z' := (Z +:= n)^
	(kdecrload,    $+1, 1, 1,  0),  ! (t n       ) Z' := (Z -:= n)^
	(kloadincr,    $+1, 1, 1,  0),  ! (t n       ) Z' := Z++^ (difficult to express step)
	(kloaddecr,    $+1, 1, 1,  0),  ! (t n       ) Z' := Z--^

	(kaddto,       $+1, 1, 0,  0),  ! (t         ) Z^ +:= Y
	(ksubto,       $+1, 1, 0,  0),  ! (t         ) Z^ -:= Y
	(kmulto,       $+1, 1, 0,  0),  ! (t         ) Z^ *:= Y
	(kdivto,       $+1, 1, 0,  0),  ! (t         ) Z^ /:= Y
	(kidivto,      $+1, 1, 0,  0),  ! (t         ) Z^ %:= Y
	(kiremto,      $+1, 1, 0,  0),  ! (t         ) Z^ rem:= Y
	(kbitandto,    $+1, 1, 0,  0),  ! (t         ) Z^ iand:= Y
	(kbitorto,     $+1, 1, 0,  0),  ! (t         ) Z^ ior:= Y
	(kbitxorto,    $+1, 1, 0,  0),  ! (t         ) Z^ ixor:= Y
	(kshlto,       $+1, 1, 0,  0),  ! (t         ) Z^ <<:= Y
	(kshrto,       $+1, 1, 0,  0),  ! (t         ) Z^ >>:= Y
	(kminto,       $+1, 1, 0,  0),  ! (t         ) Z^ min:= Y
	(kmaxto,       $+1, 1, 0,  0),  ! (t         ) Z^ max:= Y
	(kaddpxto,     $+1, 1, 1,  0),  ! (t s       ) Z^ +:= Y*s
	(ksubpxto,     $+1, 1, 1,  0),  ! (t s       ) Z^ -:= Y*s

	(knegto,       $+1, 1, 0,  0),  ! (t         ) -:= Z^
	(kabsto,       $+1, 1, 0,  0),  ! (t         ) abs:= Z^
	(kbitnotto,    $+1, 1, 0,  0),  ! (t         ) inot-:= Z^
	(knotto,       $+1, 1, 0,  0),  ! (t         ) not:= Z^
	(ktoboolto,    $+1, 1, 0,  0),  ! (t         ) istrue:= Z^

	(ktypepun,     $+1, 2, 0,  0),  ! (t u       ) Z' := t(u@(Z^))
	(kfloat,       $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) Int   to real t
	(kfix,         $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) Real   to int t
	(ktruncate,    $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,u) Mask to width of u, but type is widened to t
	(kwiden,       $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) Mask to width of u, but type is widened to t
	(kfwiden,      $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) r32 to r64
	(kfnarrow,     $+1, 2, 0,  0),  ! (t u       ) Z' := cast(Z,t) r64 to r32

	(kstartmx,     $+1, 0, 0,  0),  ! (          ) -
	(kresetmx,     $+1, 1, 0,  0),  ! (t         ) -
	(kendmx,       $+1, 1, 0,  0),  ! (t         ) -

	(kinitdswx,    $+1, 0, 0,  0),  ! (          ) Following two ops initialise doswitchx jumptable

	(klabel,       $+1, 0, 0,  0),  ! (          ) ?
	(klabeldef,    $+1, 0, 0,  0),  ! (          ) ?

	(ksetcall,     $+1, 0, 1,  0),  ! (n s       ) n=args, s=1 for simple call
	(ksetarg,      $+1, 0, 2,  0),  ! (n1 n2     ) n1=arg no (LTR) n2=int or real arg no (maybe neg for real)

	(kloadall,     $+1, 0, 0,  0),  ! (          ) ?

	(keval,        $+1, 0, 0,  0),  ! (          ) Evaluate Z [load to an actual register], then pop
	(kcomment,     $+1, 0, 0,  0),  ! (C         ) Comment C (a string)
end


global const kerror = knop


global symbol entryproc		!entry point function

global symbol currfunc
global int retindex

global const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

proc start=
	zero_unit.tag:=jconst
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end


!global const maxoperands=20
!global [maxoperands]pcl    pstackx
!global [maxoperands]pcsrec pflags

!!following are continually updated as opnds are pushed, moved and popped
!global [maxoperands]unit	pclopnd			!pclrec describing opnd when not loaded
!global [maxoperands]byte	pclreg			!>0 means in given register
!global [maxoperands]byte	pclmode			!copy of mode, esp. if loaded (indicates reg/xreg)
!global [maxoperands]byte	pclcount		!dupl count
!global [maxoperands]byte	pclloc			!stores loc code
!
!following are reset per proc and augmented as it is processed
!global [maxoperands]byte pcltempflags		!1 means a temp uses local storage
!global [maxoperands]mclopnd pcltempopnds	!store mcl opnd for such a temp

!global int noperands						!number of pcl operands, including wide
!
!global macro zz = noperands
!global macro yy = noperands-1
!global macro xx = noperands-2
!global macro ww = noperands-3
=== pcl_gen.m 0 0 16/29 ===

global const pclpresent=1

macro divider = pc_comment("-"*40)

global pcl pcldoswx

global proc genpcl=
!generate pcl code for each function
	ref procrec pp
	symbol d
	int tt:=clock()

	pp:=proclist
	while pp do
		d:=pp.def
		genprocpcl(d)
!++NPROCS
!PCLLENGTH+:=D.PCLINFO.LENGTH

		pp:=pp.nextproc
	od

!CPL =NPROCS
!CPL =PCLLENGTH
!CPL real (PCLLENGTH)/nprocs
!CPL

	pcltime:=clock()-tt
end

proc genprocpcl (symbol p) =
	imodule ms
	byte ismain:=0

	ms:=modules[p.moduleno]
	pcldoswx:=nil
!	nblocktemps:=0

!-----------------
	currfunc:=p
	pcl_start()
	mmpos:=p.pos


	if p=ms.stmain and moduletosub[p.moduleno]=mainsubprogno then
		ismain:=1
		entryproc:=p
!		p.isentry:=1
		genmain(p)

	elsif p=ms.ststart then
		genstart(p)
	fi
!------------------


	retindex:=createfwdlabel()

	divider()

	if p.hasdoswx then
		pc_gen(kinitdswx)			!this op needed by C?
		pcldoswx:=pccurr			!code to be injected later after this instr
	fi

!	pc_comment("<EVALBLOCk>")

!CPL "PROC",P.NAME, P.OWNER.NAME, P.CODE
	evalunit(p.code)
!CPL "PROC",$LINENO

	if ismain then
		genpushint(0)
		pc_gen(kstop)
		pc_setmode(ti64)
	fi

	divider()
	definefwdlabel(retindex)
	genreturn()

	p.pccode:=pcl_end()
	p.pclength:=pclength

!CPL "DONE genpcl", P.NAME, P.CODE, =p

end

proc genmain(symbol p)=
	symbol d
	for i to nsubprogs when i<>mainsubprogno do
		d:=modules[subprogs[i].mainmodule].ststart
		docallproc(d)
	od
	d:=modules[subprogs[mainsubprogno].mainmodule].ststart
	docallproc(d)

	entryproc:=p
end

proc genstart(symbol p)=
	symbol d
	int lead:=0, m,s

	m:=p.moduleno
	s:=p.subprogno

	if s=mainsubprogno and p.moduleno=subprogs[s].mainmodule then
		LEAD:=1
	elsif p.moduleno=subprogs[s].firstmodule then
		LEAD:=2
	fi

	if lead then
		for i to nmodules when moduletosub[i]=s and i<>m do
			d:=modules[i].ststart
			docallproc(d)
		od
	fi
end

proc docallproc(symbol d)=
!call a simple proc, eg. start(), with no args
	return unless d
	pc_gen(ksetcall)
	pc_setnargs(0)

	pc_gen(kcallp, genmemaddr_d(d))
end

global proc genreturn=
!assume returning from currproc
	case currproc.nretvalues
	when 0 then
		pc_gen(kretproc)
	when 1 then
		pc_gen(kretfn)
		pc_setmode(currproc.mode)

	else
		pc_genx(kretfn, currproc.nretvalues)
	esac
end

global func genmem_u(unit p)pcl=
	return pgenmem(p.def)
end

global func genmem_d(symbol d)pcl=
	return pgenmem(d)
end

global proc genpushmem_d(symbol d)=
	pc_gen(kload, pgenmem(d))
end

global func genmemaddr_d(symbol d)pcl=
	return pgenmemaddr(d)
end

global proc genpushmemaddr_d(symbol d)=
	pc_gen(kload, pgenmemaddr(d))
end

global proc genpushint(int a)=
	pc_gen(kload, pgenint(a))
	pc_setmode(ti64)
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

global proc genpushreal(real x, int mode)=
	pc_gen(kload, pgenrealmem(x))
	pc_setmode(mode)
end

global proc genpushstring(ichar s)=
	pc_gen(kload, pgenstring(s))
	pc_setmode(tu64)
end

global func createfwdlabel:int =
	return ++mlabelno
end

global func definelabel:int =
	pc_gen(klabel, pgenlabel(++mlabelno))
	return mlabelno
end

global proc definefwdlabel(int lab) =
	pc_gen(klabel, pgenlabel(lab))
end

=== pcl_block.m 0 0 17/29 ===
!
!blockpcl

const dodotchains=1
!const dodotchains=0

INT NNN

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

global symbol pnprocs, pprocname, pprocaddr


global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)
macro evallv(p) = evalref(p)
macro evalunitx(p, isref) = (isref|evalref(p)|evalunit(p))
macro evalblock(p) = evalunit(p)

!global proc genpcl=
!!generate pcl code for each function
!	ref procrec pp
!	symbol d
!	int tt:=clock()
!
!	pp:=proclist
!	while pp do
!		d:=pp.def
!		genprocpcl(d)
!!++NPROCS
!!PCLLENGTH+:=D.PCLINFO.LENGTH
!
!		pp:=pp.nextproc
!	od
!
!!CPL =NPROCS
!!CPL =PCLLENGTH
!!CPL real (PCLLENGTH)/nprocs
!!CPL
!
!	pcltime:=clock()-tt
!end
!



global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a, b, c

	if p=nil then return fi

!CPL "EVALUNIT", JTAGNAMES[P.TAG]


	mmpos:=p.pos

	a:=p.a
	b:=p.b
	c:=p.c

	switch p.tag
	when jconst         then do_const(p)
	when jnull          then
	when jname          then do_name(p)
	when jblock then
		do_block(p)
	when jcall          then
		do_callproc(p, a, b)
	when jreturn        then do_return(p, a)
	when jreturnmult    then do_returnmult(p, a)
	when jassign        then do_assign(p, a, b)
	when jassignms      then do_assignms(a, b)
	when jassignmm      then do_assignmm(a, b)
	when jassignmdrem   then do_assignmdrem(a, b)
	when jto            then do_to(p, a, b)
	when jif            then do_if(p, a, b, c, 0)
	when jforup         then do_for(p, a, b, c, 0)
	when jfordown       then do_for(p, a, b, c, 1)
	when jforall        then do_forall(p, a, b, c, 0)
	when jforallrev     then do_forall(p, a, b, c, 1)
	when jwhile         then do_while(p, a, b, c)
	when jrepeat        then do_repeat(p, a, b)
	when jgoto          then do_goto(a)
	when jlabeldef      then do_labeldef(p)
	when jredo          then do_exit(p, 1)
	when jnext          then do_exit(p, 2)
	when jexit          then do_exit(p, 3)
	when jdo            then do_do(p, a, b)
	when jcase          then do_case(p, a, b, c, 0, 0)
	when jdocase        then do_case(p, a, b, c, 1, 0)
	when jswitch, jdoswitch, jdoswitchu, jdoswitchx then
		do_switch(p, a, b, c)
	when jrecase        then do_recase(p, a)
	when jswap          then do_swap(p, a, b)
	when jselect        then do_select(p, a, b, c, 0)
	when jprint, jprintln then
		do_print(p, a, b)
	when jfprint, jfprintln then
		do_print(p, a, b)
	when jread	        then do_read(p, a)
	when jreadln        then do_readln(a)
	when jstop          then do_stop(p, a)
	when jeval          then
		evalunit(a)
		pc_gen(keval)
		pc_setmode_u(a)

	when jandl          then do_andl(p, a, b)
	when jorl           then do_orl(p, a, b)
	when jcmp           then do_setcc(p, a, b)
	when jcmpchain      then do_setccchain(p, a)

	when jbin           then do_bin(p, a, b)
	when jindex         then do_index(p, a, b)

	when jdotindex      then do_dotindex(p, a, b)
	when jdotslice      then do_dotslice(p, a, b)
	when jdot           then do_dot(p)
	when jptr           then do_ptr(p, a)
	when jaddrof        then evalref(a, b)

!	when jconvert       then do_convert(p, a)
	when jwiden         then do_convert(p, kwiden)
	when jfwiden        then do_convert(p, kfwiden)
	when jfnarrow       then do_convert(p, kfnarrow)
	when jfloat         then do_convert(p, kfloat)
	when jfix           then do_convert(p, kfix)
	when jtruncate      then do_convert(p, ktruncate)
	when jtoboolt       then do_convert(p, ktoboolt)

	when jtypepun       then do_typepun(p, a)
	when jshorten       then do_shorten(p, a)
	when jtypeconst     then do_typeconst(p)

	when junary         then do_unary(p, a)

	when jnotl          then do_notl(p, a)
	when jistruel       then do_istruel(p, a)
	when jisfalsel      then do_isfalsel(p, a)

	when jincr          then
		if p.pclop in [kincrto, kdecrto] then
			do_incr(p, a)
		else
			do_incrload(p, a)
		fi
!
	when jbinto         then do_binto(p, a, b)
!
	when junaryto       then do_unaryto(p, a)
!
	when jsyscall then
		do_syscall(p, a)

	when jclear         then do_empty(p, a)

	when jslice then
		do_slice(p, a, b)

	else
		GERROR_S("UNSUPPORTED TAG ", JTAGNAMES[P.TAG])
		return
	end switch

!CPL "  EVALUNIT2", JTAGNAMES[P.TAG]
!CPL "EVALU", JTAGNAMES[P.TAG], STRMODE(P.MODE), =P.RESULTFLAG

	if p.mode<>tvoid and not p.resultflag then
!CPL "RESULT CREATED BUT NOT USED"
		case p.tag
		when jassign, jcall, jsyscall then
!CPL "  EVALA"

		else
!CPL "  EVALB"
IF NOT JSOLO[P.TAG] THEN
PRINTUNIT(P)
GERROR(ADDSTR("NOT ALLOWED BY ITSELF:", JTAGNAMES[P.TAG]))
FI

			pc_gen(kunload)
			pc_setmode_u(p)
		esac
	fi
!CPL "  EVALUNIT3", JTAGNAMES[P.TAG]

end

proc evalref(unit p, q=nil)=
	unit a, b, c
	a:=p.a
	b:=p.b
	c:=p.c
	mmpos:=p.pos

	case p.tag
	when jname then
		genpushmemaddr_d(p.def)
		pc_setmode(tu64)
		if q then					!addrof may have optional byte offset
			genpushint(q.value)
			pc_genix(kaddpx)
			pc_setmode(tu8)
		fi

	when jindex then
		do_indexref(a, b)

	when jdot then
		do_dotref(p)

	when jptr then
		evalunit(p.a)

	else
		case p.tag
		when jif then
			do_if(p, a, b, c, 1)
!		when jselect then
!			do_select(p, a, b, c, 1)
!		when jswitch then
!			do_switch(p, a, b, c, 1)
!		when jcase then
!			do_case(p, a, b, c, 0, 1)
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
		evalref(p)
		pc_gen(kiload)
		pc_setmode(tu64)
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

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r, s
	int lab2, i

	q:=p.a
	r:=p.b

	case p.tag
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

	when jnotl, jisfalsel then
		case opc
		when kjumpf then
			genjumpcond(kjumpt, q, lab)
		when kjumpt then
			genjumpcond(kjumpf, q, lab)
		esac

	when jistruel then
		genjumpcond(opc, q, lab)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc, q, lab)

	when jcmp then

		gcomparejump(opc, p.condcode, q, r, lab)

	when jinrange then
		evalunit(q)

		if opc=kjumpt then
			lab2:=createfwdlabel()
			evalunit(r.a)
			pc_gencond(kjumpcc, lt_cc, pgenlabel(lab2))
			pc_setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pc_gencond(kjumpcc, le_cc, pgenlabel(lab))
			pc_setmode_u(q)
			definefwdlabel(lab2)
		else
			evalunit(r.a)
			pc_gencond(kjumpcc, lt_cc, pgenlabel(lab))
			pc_setmode_u(q)
			pccurr.popone:=1
			evalunit(r.b)
			pc_gencond(kjumpcc, gt_cc, pgenlabel(lab))
			pc_setmode_u(q)
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
					pc_gencond(kjumpcc, eq_cc, pgenlabel(lab2))
					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, ne_cc, pgenlabel(lab))
				fi
				pc_setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s, s:=s.nextunit do
				evalunit(s)
				pc_gencond(kjumpcc, eq_cc, pgenlabel(lab))
				pc_setmode_u(q)
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
					pc_genxy(kswapstk, 1, 2)

					pc_gencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), pgenlabel(lab))

					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, reversecond(p.cmpgenop[i]), pgenlabel(lab))
				fi

				pc_setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(r)
				if r.nextunit then
					pc_genxy(kswapstk, 1, 2)
					pc_gencond(kjumpcc, reversecond_order(reversecond(p.cmpgenop[i])), pgenlabel(lab2))
					pccurr.popone:=1
				else
					pc_gencond(kjumpcc, p.cmpgenop[i], pgenlabel(lab))
				fi
				pc_setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else			!other, single expression
		evalunit(p)
		pc_gen(opc, pgenlabel(lab))
!*!		if ttisblock[p.mode] then gerror("jumpt/f") fi
		pc_setmode(p.mode)
	end
end

proc gcomparejump(int jumpopc, int cond, unit lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	evalunit(lhs)
	evalunit(rhs)

	pc_gencond(kjumpcc, cond, pgenlabel(lab))
	pc_setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pc_gen(kjump, pgenlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #", mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] or mode=tbool then
		genpushint(p.value)
	elsif ttisreal[mode] then
		genpushreal(p.xvalue, getpclmode(mode))

	elsif ttisref[mode] then
		if p.isastring then
!CPL "doconst/CONST3", P.SVALUE, p.strtype
if p.strtype='B' then gerror("1:B-str?") fi

			genpushstring(p.svalue)
		else
			genpushint(p.value)
		fi
	else
		gerror("do_const")
	fi
	pc_setmode(mode)
end

proc do_name(unit p)=
	symbol d

	d:=p.def
	case d.nameid
	when procid, dllprocid then
		genpushmemaddr_d(d)
		pc_setmode(tu64)
	when labelid then
		if d.labelno=0 then
			d.labelno:=++mlabelno
		fi
		if p.resultflag then		!get label address
			pc_gen(kload, pgenlabel(d.labelno))
			pc_setmode(tu64)
		else
			pc_gen(kjump, pgenlabel(d.labelno))
			p.mode:=tvoid
			p.resultflag:=0
		fi

	when fieldid then
		genpushint(d.offset)
		pc_setmode(ti64)

	else
		genpushmem_d(d)
		pc_setmode(d.mode)

	esac
end

proc do_stop(unit p, a) =
	if a then
		evalunit(a)
	else
		pc_gen(kload, pgenint(0))
		pc_setmode(ti64)
	fi
	pc_gen(kstop)
end

proc do_andl(unit p, a, b) =
	int labfalse, labend

	pc_gen(kstartmx)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf, a, labfalse)
	genjumpcond(kjumpf, b, labfalse)

	genpushint(1)
	pc_gen(kresetmx)
	pc_setmode(ti64)

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pc_gen(kendmx)
	pc_setmode(ti64)

	definefwdlabel(labend)
end

proc do_orl(unit p, a, b) =
	int labtrue, labfalse, labend

	pc_gen(kstartmx)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt, a, labtrue)
	genjumpcond(kjumpf, b, labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	pc_gen(kresetmx)
	pc_setmode(ti64)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pc_gen(kendmx)
	pc_setmode(ti64)

	definefwdlabel(labend)
end

proc do_notl(unit p, a) =
	evalunit(a)
	pc_gen(p.pclop)
	pc_setmode(ti64)
end

proc do_istruel(unit p, a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	fi
	pc_gen(ktoboolt)
	pc_setmode_u(p)
	pc_setmode2(a.mode)
end

proc do_isfalsel(unit p, a) =
	evalunit(a)
!	if a.mode=tbool then
!		return
!	fi
	pc_gen(ktoboolf)
	pc_setmode_u(p)
	pc_setmode2(a.mode)
end

proc do_typepun(unit p, a) =
	evalunit(a)

	if a.tag=jname then
		a.def.addrof:=1
	fi

	if a.mode=p.mode then return fi
	pc_gen(ktypepun)
	pc_setmode(p.oldmode)
	pc_setmode2(a.mode)
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

proc do_assign(unit p, a, b) =
!fstore=1 when result is needed

!cpl "ASSIGN", B.RESULTFLAG
!PRINTUNIT(P)

	if a.tag=jname and not a.def.used then
		RETURN
	FI

	case b.tag
	when jmakelist then					!a:=(x, y, z)
		if not p.resultflag then
			do_assignblock(p, a, b)		!(avoids pushing/popping block data)
			return
		fi

!	when jslice then					!a:=b[i..j] etc
!		if a.tag=jname then
!			doslice(b, b.a, b.b, a.def, p.resultflag)
!			return
!		fi
	esac

	case a.tag
	when jindex then
		do_storeindex(p, a.a, a.b, b)
		return
	when jslice then
GERROR("ASS/SLICE")

	when jdot then
		do_storedot(a, a.b, b)
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
		pc_setmode_u(a)

	when jdotindex then
		evalref(a.a)
		evalunit(a.b)
		pc_gen(kstorebit)
		pc_setmode_u(a.a)
		return
	when jdotslice then
		evalref(a.a)
		evalunit(a.b.a)
		evalunit(a.b.b)
		pc_gen(kstorebf)
		pc_setmode_u(a.a)
		return

	when jif then
		do_if(a, a.a, a.b, a.c, 1)
		pc_gen(kistore)
		pc_setmode_u(a)


	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	esac

	pc_setmode_u(a)
end

proc do_bin(unit p, a, b) =
	evalunit(a)
	evalunit(b)

	if p.pclop in [kaddpx, ksubpx] then
		pc_genix(p.pclop, ttsize[tttarget[a.mode]])
	else
		pc_gen(p.pclop)
		case p.pclop
		when ksubp then
			pc_setscaleoff(ttsize[tttarget[a.mode]])
		when kmaths2 then
			pccurr.mathsop:=p.mathsop
		esac
	fi

	pc_setmode_u(p)
end

proc do_setcc(unit p, a, b) =
	evalunit(a)
	evalunit(b)
	pc_gencond(ksetcc, p.condcode)
	pc_setmode_u(a)
end

proc do_setccchain(unit p, q) =
	int lab1, lab2, i, cond
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
			pc_genxy(kswapstk, 1, 2)
			cond:=reversecond_order(cond)
		fi

		pc_gencond(kjumpcc, cond, pgenlabel(lab1))
		if r.nextunit then pccurr.popone:=1 fi

		pc_setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	od

	genpushint(1)
	pc_gen(kresetmx)
	pc_setmode(ti64)
	pc_gen(kjump, pgenlabel(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	pc_gen(kendmx)
	pc_setmode(ti64)
	definefwdlabel(lab2)
end

proc do_binto(unit p, a, b)=
	evalunit(b)
	evallv(a)
	do_setinplace()

	pc_gen(getto_op(p.pclop))
	pc_setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pc_setscaleoff(ttsize[tttarget[a.mode]])
	fi
end

proc do_unary(unit p, a) =
	int adj
!
	if ttbasetype[a.mode]=tslice then
		evalref(a)
		case p.propcode
		when kklen, kkupb then
			genpushint(8)
			pc_genix(kiloadx, 1, 0)
			pc_setmode(ti64)
			if p.propcode=kkupb then
				adj:=ttlower[a.mode]-1
				if adj then
					genpushint(adj)
					pc_gen(kadd)
					pc_setmode(ti64)
				fi
			fi

		when kksliceptr then
			pc_gen(kiload)
			pc_setmode(tu64)

		esac

		return
	fi

	evalunit(a)
	pc_gen(p.pclop)

	if p.pclop=kmaths then
		pccurr.mathsop:=p.mathsop
	fi


	pc_setmode_u(a)
end

proc do_unaryto(unit p, a)=
	evallv(a)
	do_setinplace()

	pc_gen(getto_op(p.pclop))
	pc_setmode_u(a)
end

func getto_op(int opc)int=
	static [,2]byte table = (
	(kadd,       kaddto),
	(ksub,       ksubto),
	(kmul,       kmulto),
	(kdiv,       kdivto),
	(kidiv,      kidivto),
	(kirem,      kiremto),
	(kbitand,    kbitandto),
	(kbitor,     kbitorto),
	(kbitxor,    kbitxorto),
	(kshl,       kshlto),
	(kshr,       kshrto),
	(kmin,       kminto),
	(kmax,       kmaxto),
	(kaddpx,     kaddpxto),
	(ksubpx,     ksubpxto),

	(kneg,       knegto),
	(kabs,       kabsto),
	(kbitnot,    kbitnotto),
	(knot,       knotto))
!	(ktobool,    ktoboolto))

	for i to table.len do
		if opc=table[i,1] then
			return table[i,2]
		fi
	od

	gerror("No -to op")
	0
end


proc do_ptr(unit p, a)=
	evalunit(a)
	pc_gen(kiload)
	pc_setmode_u(p)
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.labelno=0 then
		d.labelno:=++mlabelno
	fi

	pc_gen(klabeldef, genmemaddr_d(d))

	pc_gen(klabel, pgenlabel(d.labelno))
end

proc do_goto(unit a)=
	symbol d

	if a.tag=jname and a.def.nameid=labelid then
		d:=a.def
		if d.labelno=0 then
			d.labelno:=++mlabelno
		fi
		pc_gen(kjump, pgenlabel(d.labelno))
	else
		evalunit(a)
		pc_gen(kijump)
	fi
end

proc do_do(unit p, a, b) =
	int lab_abc, lab_d

	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_d)

	evalblock(a)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_to(unit p, a, b) =
	unit cvar
	int lab_b, lab_c, lab_d, count

	cvar:=p.c

	a.mode:=ti64

	evalunit(a)
	pc_gen(kstore, genmem_u(cvar))
	pc_setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b, lab_c, lab_d)

!check for count being nonzero
	if a.tag<>jconst then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		pc_gencond(kjumpcc, le_cc, pgenlabel(lab_d))
		pc_setmode(ti64)

	else
		count:=a.value
		if count<=0 then
			genjumpl(lab_d)
		fi
	fi

	definefwdlabel(lab_b)
	evalblock(b)			!main body

	definefwdlabel(lab_c)

	pc_gen(kto, pgenlabel(lab_b))
	pc_setmode(ti64)
	pc_gen(kopnd, genmem_u(cvar))
	pc_setmode(ti64)

	definefwdlabel(lab_d)
	--loopindex
end

proc do_while(unit p, pcond, pbody, pincr) =
	int lab_b, lab_c, lab_d, lab_incr

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

	docond(kjumpt, pcond, lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p, a, b) =
	int lab_ab, lab_c, lab_d

	lab_ab:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_ab, lab_c, lab_d)

	evalblock(a)

	definefwdlabel(lab_c)

	unless b.tag=jconst and b.value=0 then
		docond(kjumpf, b, lab_ab)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_exit(unit p, int k) =
	int n, index

	index:=p.loopindex
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k, index)
	if n=0 then
		gerror("Bad exit/loop index", p)
	else
		genjumpl(n)
	fi
end

proc do_if(unit p, pcond, plist, pelse, int isref) =
	int labend, i, lab2, ismult

	labend:=createfwdlabel()
	ismult:=p.mode<>tvoid

	i:=0
	if ismult then pc_gen(kstartmx) fi

	while pcond, (pcond:=pcond.nextunit; plist:=plist.nextunit) do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf, pcond, lab2)

		evalunitx(plist, isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi

		if pcond.nextunit or pelse then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
	od

	if pelse then
		evalunitx(pelse, isref)
		if ismult then pc_gen(kendmx); pc_setmode_u(p) fi
	fi
	definefwdlabel(labend)
end

proc do_return(unit p, a) =
	if a then
		evalunit(a)

		pc_gen(kjumpret, pgenlabel(retindex))
		pc_setmode_u(a)
	else
		genjumpl(retindex)
	fi
end

proc do_returnmult(unit p, a) =
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
	pc_gen(kjumpretm, pgenlabel(retindex))
	pc_setnargs(nparams)
	p.resultflag:=1
end

proc do_callproc(unit p, a, b) =
	[maxparams]unit paramlist
	[maxparams]i8 argattr
	int nparams, isptr, nvariadics, nret, isfn
	int iparams, fparams
	int nfixedparams
	symbol d, e
	ref[]i32 pmult
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

	isfn:=d.mode<>tvoid

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
	pc_setmode_u(p)
	pccurr.nargs:=nparams

	iparams:=fparams:=0

	for i to nparams do
		q:=paramlist[i]
		argattr[i]:=0
		if q.mode in [tr32, tr64] then
			if ++fparams>8 then argattr[i]:=2 fi
		else
			if ++iparams>8 then argattr[i]:=2 fi
		fi
	od

	if fparams>8 and iparams>8 then
		gerror("Mixed stack args")
	fi
	iparams:=max(fparams, iparams)-8		!no. of stack args

	for i:=nparams downto 1 do
		if iparams.odd and argattr[i] then
			argattr[i]:=1					!change first/rightmost '2' to '1'
			iparams:=0
		fi
		q:=paramlist[i]
		evalunit(q)

		if nvariadics and i>=nvariadics and pccurr.mode=tr32 then
			pc_gen(kfwiden)
			pccurr.size:=8
			pccurr.mode:=tr64
			pccurr.mode2:=tr32

			pc_gen(ksetarg)
			pc_setmode(tr64)
		else
			pc_gen(ksetarg)
			pc_setmode_u(q)
		fi

		pccurr.x:=i
		pccurr.y:=argattr[i]
	od

	if not isptr then
		pc_gen((isfn|kcallf|kcallp), genmemaddr_d(d))
	else
		evalunit(a.a)
		pc_gen((isfn|kicallf|kicallp))
	fi

	pccurr.nargs:=nparams
    pccurr.nvariadics:=nvariadics

	if isfn then
		pc_setmode_u(p)
	fi

	if d.nretvalues>1 and isfn then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			pc_gen(ktype)
			pc_setmode(pmult[i])
		od
	fi

	if isfn and not p.resultflag then
		pc_gen(kunload)
		pc_setmode_u(p)
	fi

end

proc do_print(unit p, a, b) =
	unit q, r, fmt
	int m, fn, needprintend

	if a then
		needprintend:=1
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi
		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			genpc_sysproc(sf_print_startfile, a)
		when tc8 then
			genpc_sysproc(sf_print_startstr, a)
		when tref then
			genpc_sysproc(sf_print_startptr, a)
		else
			gerror("@dev?")
		esac
	else
		needprintend:=1
		genpc_sysproc(sf_print_startcon)
	fi

	q:=b

	case p.tag
	when jfprint, jfprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		fi
		genpc_sysproc(sf_print_setfmt, q)
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
			gerror_s("PRINT/T=#", strmode(m))
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
	when jprintln, jfprintln then
		genpc_sysproc(sf_print_newline)
	esac
	if needprintend then
		genpc_sysproc(sf_print_end)
	fi
end

proc do_incr(unit p, a) =
	evallv(a)
	do_setinplace()
	pc_gen(p.pclop)
	pc_setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pccurr.stepx:=1

	if ttisref[m] then
		pccurr.stepx:=ttsize[tttarget[m]]
	fi
end

proc do_incrload(unit p, a) =
	evallv(a)
	do_setinplace()
	pc_gen(p.pclop)
	pc_setmode_u(a)
	setincrstep(a.mode)
end

proc do_for(unit p, pindex, pfrom, pbody, int down) =
!Structure:
!	Forup/to
!		pindex -> [ptoinit]
!		pfrom -> pto -> [pstep]
!		pbody -> [pelse]
!When pto is complex, then pto refers to an AV variable, and ptoinit contains code
!to evaluate the complex pto, and assign it to that AV

	unit pto, pstep, pelse, px, ptoinit
	int lab_b, lab_c, lab_d, lab_e
	int a, b, stepx

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit
	ptoinit:=pindex.nextunit

	if pto.tag=jptr then
		px:=pto.a
		symbol d
		if px.tag=jname and (d:=px.def).nameid=paramid and
			 d.byref then
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
	pc_gen(kstore, genmem_u(pindex))
	pc_setmode_u(pindex)

	if ptoinit then			!using temp for limit
		ptoinit.resultflag:=0
		evalunit(ptoinit)
	fi

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pc_gen(kjump, pgenlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pc_gencond(kjumpcc, (down|gt_cc|lt_cc), pgenlabel(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			pc_gencond(kjumpcc, (down|lt_cc|gt_cc), pgenlabel(lab_e))
		fi
		pc_setmode_u(pindex)
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
		pc_genx((down|kfordown|kforup), stepx, pgenlabel(lab_b))
		pc_setmode_u(pindex)
	else
		pc_genx((down|kfordown|kforup), 1, pgenlabel(lab_b))
		pc_setmode_u(pindex)
	fi

	pc_gen(kopnd, genmem_u(pindex))
	pc_setmode(ti64)

	case pto.tag
	when jconst then
		pc_gen(kopnd, pgenint(pto.value))
		pc_setmode(ti64)
	when jname then
		pc_gen(kopnd, genmem_u(pto))
		pc_setmode(ti64)
	esac

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p, pindex, plist, pbody, int down) =
!Structure:
!	forall
!		pindex -> plocal -> pfrom -> pto
!		plist -> passign
!		pbody -> [pelse]

	unit plocal, pfrom, pto, pelse, passign
	int lab_b, lab_c, lab_d, lab_e
	int a, b
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
	pc_setmode_u(pindex)

	if pto.tag not in [jconst, jname] then
		evalunit(pto)
		dto:=getavname(currproc)
		pc_gen(kstore, genmem_d(dto))
		pc_setmode(ti64)
		pto:=createname(dto)
		pto.mode:=dto.mode
		pto.resultflag:=1
	fi

	if pfrom.tag=jconst and pto.tag=jconst then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pc_gen(kjump, pgenlabel(lab_e))
		fi
	else
		if pfrom.tag=jconst then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pc_gencond(kjumpcc, (down|gt_cc|lt_cc), pgenlabel(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			pc_gencond(kjumpcc, (down|lt_cc|gt_cc), pgenlabel(lab_e))
		fi
		pc_setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	pc_genx((down|kfordown|kforup), 1, pgenlabel(lab_b))
	pc_setmode_u(pindex)

	pc_gen(kopnd, genmem_u(pindex))
	pc_setmode(ti64)
	case pto.tag
	when jconst then
		pc_gen(kopnd, pgenint(pto.value))
	when jname then
		pc_gen(kopnd, genmem_u(pto))
	else
		PC_GEN(KOPND, GENMEM_D(DTO))
!		gerror("forall/to: not const or name")
	esac
	pc_setmode(ti64)

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_convert(unit p, int opc) =

!GERROR("DO CONVERT")

!	case p.convcode
!	when kksoftconv then
!		evalunit(a)
!		return
!	when kkerror then
!		gerror("CONV/ERROR")
!
!	else
	evalunit(p.a)
	pc_gen(opc)
	pc_setmode(p.mode)
	pc_setmode2(p.oldmode)
end

proc do_swap(unit p, a, b) =
	evallv(a)
	do_setinplace()
	evallv(b)
	do_setinplace()
	pc_gen(kiswap)
	pc_setmode_u(a)
end

global func checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions, 
!or -1 when offsets cannot be combined
	int offset

	case p.tag
	when jdot then
		offset:=checkdotchain(p.a, pname)
		return p.offset+offset

	else							!anything else, is the start expression
		pname:=p
		return 0
	esac
	return 0
end

proc do_dotref(unit pdot) =
	int imode:=createrefmode(nil, pdot.mode, 0)
	int offset
	unit a, pname


	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
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
	pc_setmode(imode)
end

proc do_dot(unit pdot) =
	int offset
	unit a, pname

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)
	pc_gen(kload, pgenint(offset))
	pc_setmode(ti64)

	pc_genix(kiloadx, 1)
	pc_setmode_u(pdot)
end

proc do_storedot(unit pdot, pfield, rhs) =
	int offset
	unit a, pname

	evalunit(rhs)
	if pdot.resultflag then
		pc_gen(kdouble)
	fi

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a, pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)
	pc_gen(kload, pgenint(offset))
	pc_setmode(ti64)

	pc_genix(kistorex, 1)

	pc_setmode_u(pdot)
end

proc do_index(unit p, parray, pindex) =
	int addoffset, scale, offset


!GENCOMMENT("INDEX/ILOADNEXT/block")
!	if ttisblock[p.mode] then
!CPL "INDEX/BLOCK"
!		do_indexref(parray, pindex)
!
!		return
!	fi

	addoffset:=getindexoffset(parray, pindex)

	evalarray(parray)
	scale:=ttsize[tttarget[parray.mode]]
	offset:=-ttlower[parray.mode]*scale + addoffset*scale

	evalunit(pindex)
!GENCOMMENT("INDEX/ILOADNEXT")

	pc_genix(kiloadx, scale, offset)
	pc_setmode_u(p)
!GENCOMMENT("...INDEX/ILOADNEXT")
end

proc do_storeindex(unit p, parray, pindex, rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(parray, pindex)

	evalunit(rhs)
	if p.resultflag then
		pc_gen(kdouble)
	fi

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pc_genix(kistorex, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	pc_setmode_u(p.a)
end

proc do_indexref(unit parray, pindex) =
	int addoffset, scale
	addoffset:=getindexoffset(parray, pindex)

	evalarray(parray)
	evalunit(pindex)

	scale:=ttsize[tttarget[parray.mode]]
	pc_genix(kaddpx, scale, -ttlower[parray.mode]*scale+addoffset*scale)
	pc_setmode(tttarget[parray.mode])
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

proc do_switch(unit p, pindex, pwhenthen, pelse, int isref=0) =
!'looptype' is set up here:
! 0 = switch	normal switch (range-checked)
! 1 = doswitch	looping switch (range-checked)
! 2 = doswitchu	looping switch via computed goto/indexed (both non-range-checked)
! 3 = doswitchx	looping switch via computed goto/labels

	const maxlabels = 1000
	int minlab, maxlab, n, iscomplex, i
	int lab_a, lab_d, labjump, elselab, labstmt, ax, bx, ismult, mode
	byte looptype, opc
	[0..maxlabels]pcl labels
	unit w, wt, pjump, pnext
	pcl psetup, pc, px, pw
	symbol djump

	case p.tag
	when jswitch then
		looptype:=0; opc:=kswitch
	when jdoswitch then
dodosw:
		looptype:=1; opc:=kswitch
	when jdoswitchu then
		if ctarget then dodosw fi			
		looptype:=2; opc:=kswitchu
	else
		looptype:=3
	esac

	ismult:=p.mode<>tvoid and looptype=0

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
					minlab := min(i, minlab)
					maxlab := max(i, maxlab)
				od
			when jconst then		!assume int
				ax:=bx:=w.value
				goto dorange
			else
				gerror_s("Switch when2: not const: #", strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	n:=maxlab-minlab+1
	if n>maxlabels then
		gerror("Switch too big")
	fi

	if looptype then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then pc_gen(kstartmx) fi

	if looptype=3 then		!need to initialise pointer to JT
		pjump:=pindex.nextunit
		if pjump.tag<>jname then gerror("doswx not name") fi
		djump:=pjump.def
		if ttbasetype[djump.mode]<>tref then gerror("doswx not ref") fi

!these two instrs to be moved to kinitdswx later
!	PC_COMMENT("JT LOAD")
		pw:=pccurr							!last op before the two new ops
		pc_gen(kload, pgenlabel(labjump))
		pc_setmode(tu64)
		px:=pccurr							!px=pw.next = 1st of two ops

		pc_gen(kstore, genmem_u(pjump))
		pc_setmode(tu64)

		if pcldoswx=nil then
			gerror("doswx in main?")
		fi

!Before:  A B PCLDOSWX C D ...  W X Y				!X Y are load/store above
!After:   A B PCLDOSWX X Y D ...  W

		pc:=pcldoswx.next					!point to C
		pcldoswx.next:=px
		px.next.next:=pc
		pw.next:=nil
		pccurr:=pw

	fi

	evalunit(pindex)

	if looptype<>3 then
		pc_genxy(opc, minlab, maxlab, pgenlabel(labjump))
		pc_setmode(ti64)
		if looptype<2 then
			pc_gen(kopnd, pgenlabel(elselab))
			pc_setmode(ti64)						!dummy type (kopnd is general purpose)
		fi
	else
!PC_COMMENT("J1")
		pc_gen(kijump)
		pc_setmode(tu64)
	fi

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		pc_gen(kswlabel, pgenlabel(elselab))
		labels[i]:=pccurr
	od
!*!	pc_gen(kendsw)


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

		evalunitx(wt.b, isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi

		case looptype
		when 0 then
			genjumpl(lab_d)
		when 1 then
			genjumpl(lab_a)
		when 2 then
			evalunit(pindex)
			pc_genxy(opc, minlab, maxlab, pgenlabel(labjump))
			pc_setmode(ti64)
		else
			evalunit(pindex)
!PC_COMMENT("J2")
			pc_gen(kijump)
			pc_setmode(tu64)
		esac

		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse, isref)
		if ismult then pc_gen(kendmx); pc_setmode_u(p) fi
	fi

	if looptype then
		case looptype
		when 1 then
			genjumpl(lab_a)
		when 2 then
			evalunit(pindex)
			pc_genxy(opc, minlab, maxlab, pgenlabel(labjump))
			pc_setmode(ti64)
		else
			evalunit(pindex)
!PC_COMMENT("J3")
			pc_gen(kijump)
			pc_setmode(tu64)
		esac
		--loopindex
	fi

	definefwdlabel(lab_d)
end

proc do_select(unit p, a, b, c, int isref) =
	const maxlabels=256
	[maxlabels]pcl labels
	int labend, labjump, n, i, elselab, labstmt, ismult
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

	pc_genxy(kswitch, 1, n, pgenlabel(labjump))
	pc_setmode(ti64)
	pc_gen(kopnd, pgenlabel(elselab))
	pc_setmode(ti64)


	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		pc_gen(kswlabel, pgenlabel(elselab))
		labels[i]:=pccurr
	od
!*!	pc_gen(kendsw)

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q, isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c, isref)
	if ismult then pc_gen(kendmx); pc_setmode_u(p) fi

	definefwdlabel(labend)
end

proc do_case(unit p, pindex, pwhenthen, pelse, int loopsw, isref) =
	const maxcase=500
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, ismult, a, b

	int lab_abc, lab_d, labelse
	unit w, wt, plower, pupper

	loopsw:=p.tag=jdocase

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	ismult:=p.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc, lab_abc, lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	if ismult then pc_gen(kstartmx) fi

	ncases:=0

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	if pwhenthen=nil then
		if ismult then gerror("case") fi
		goto skip
	fi

	evalunit(pindex)

!CPL "INCR CASED", CASEDEPTH

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
!			if w.tag=jmakerange then
!				plower:=w.a
!				pupper:=w.b
!				unless plower.tag=pupper.tag=jconst then
!					gerror("case/var-range")
!				end
!
!
!
!GERROR("CASE/RANGE", W)
!			else
				evalunit(w)
!			fi
			pc_gencond(kjumpcc, eq_cc, pgenlabel(w.whenlabel:=labtable[ncases]))
			if w.nextunit or wt.nextunit then
				pccurr.popone:=1
			fi
			pc_setmode_u(w)
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
		evalunitx(unittable[i], isref)
		if ismult then pc_gen(kresetmx); pc_setmode_u(p) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse, isref)
		if ismult then pc_gen(kendmx); pc_setmode_u(p) fi
	fi

	if loopsw then
		genjumpl(lab_abc)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi

!IF CASEDEPTH=0 THEN
!GERROR("CASE - DEPTH 0?")
!FI

	--casedepth
!CPL "DECR CASED", CASEDEPTH
end

proc do_dotindex(unit p, a, b) =
	evalunit(a)
	evalunit(b)

	pc_gen(kloadbit)
	pc_setmode(ti64)
end

proc do_dotslice(unit p, a, b) =
	evalunit(a)
	evalunit(b.a)
	evalunit(b.b)

	pc_gen(kloadbf)
	pc_setmode(ti64)
end

proc do_read(unit p, a) =
	int m

	m:=p.mode

	if a=nil then
		a:=pzero
	fi

	if ttisinteger[m] then
		genpc_sysfn(sf_read_i64, a)
	elsif ttisreal[m] and ttsize[m]=8 then
		genpc_sysfn(sf_read_r64, a)
	elsif m=trefchar then
		genpc_sysfn(sf_read_str, a)
	else
CPL =STRMODE(M)
		GERROR("CAN'T READ THIS ITEM")
	fi
	pc_setmode_u(p)
end

proc do_readln(unit a) =
	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			genpc_sysproc(sf_read_fileline, a)
		when tu8, tc8 then
			genpc_sysproc(sf_read_strline, a)
		else
			gerror("rd@dev?")
		esac
	else
		genpc_sysproc(sf_read_conline)
	fi
end

proc docond(int opc, unit p, int lab)=
	genjumpcond(opc, p, lab)
end

proc do_syscall(unit p, a)=

	setfunctab()

	case p.fnindex
	when sf_getnprocs then
		pc_gen(kload, pgenmem(pnprocs))

	when sf_getprocname then
		pc_gen(kload, pgenmemaddr(pprocname))
		pc_setmode(tu64)
		evalunit(a)
		pc_genix(kiloadx, 8, -8)

	when sf_getprocaddr then
		pc_gen(kload, pgenmemaddr(pprocaddr))
		pc_setmode(tu64)
		evalunit(a)
		pc_genix(kiloadx, 8, -8)

	else
		PC_COMMENT("SYSCALL/GENERIC")
	esac
	pc_setmode(ti64)
end

proc do_slice(unit p, a, b) =
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
		do_indexref(a, b.a)
		if b.a.tag=b.b.tag=jconst then
			genpushint(b.b.value-b.a.value+1)
		else
			evalunit(b.b)
			evalunit(b.a)
			pc_gen(ksub)
			pc_setmode(ti64)
			genpushint(1)
			pc_gen(kadd)
		fi
		pc_setmode(ti64)

	fi

	pc_gen(kstorem); pc_setmode(tslice)
end

proc do_assignblock(unit p, a, b) =
!fstore=1 when result is needed
!method used is:
! load ref to lhs
! load ref to rhs
! do block xfer, not using the stack

	if b.tag=jmakelist then
		if ttbasetype[a.mode]=tarray then
			do_assignarray(a, b)
		else
			do_assignrecord(a, b)
		fi
	else
		GERROR("ASSIGN BLOCK")
	fi
end

proc do_assignarray(unit a, b)=
	unit passign, pindex, pconst, q
	int index

	if ttbasetype[tttarget[a.mode]]=tc8 then
		gerror("Assignment not suitable for []char type")
	fi

	pconst:=createconstunit(1, ti64)
	pindex:=createunit2(jindex, a, pconst)
	passign:=createunit2(jassign, pindex, b.a)
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

proc do_assignrecord(unit a, b)=
	unit passign, pdot, pfield, q
	int m, fieldtype
	symbol d, e

	pfield:=createunit0(jname)
	pdot:=createunit2(jdot, a, pfield)
	passign:=createunit2(jassign, pdot, b.a)
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

proc do_assignms(unit a, b)=
	int nlhs, nrhs
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
		gerror("(a, b):=x; var only")
	esac

	poptomult(a)

	if nrhs>nlhs then
		d:=getprocretmodes(b)

		for i:=nlhs+1 to nrhs do
			pc_gen(kunload)
			pc_setmode(ttmult[d.mode, i])
		od
	fi
end

proc do_assignmm(unit a, b)=
!(a, b, c):=(x, y, z)
	pushrhs(b.a)			!push rhs elements in right-to-left order
	pc_gen(kloadall)
	poptomult(a.a)
end

proc do_assignmdrem(unit a, b)=
!(a, b):=x divrem y
	evalunit(b)
	poptomult(a.a)
end

proc poptomult(unit a)=
!a is a linked list; assign n popped values to each element in turn 
	repeat
		case a.tag
		when jname then
			pc_gen(kstore, genmem_u(a))
		when jindex, jslice, jdot then
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

		pc_setmode_u(a)

		a:=a.nextunit
	until a=nil
end

proc do_recase(unit p, a)=
	unit q, wt, w
	int destlab, casevalue

!CPL "DO_RECASE", CASEDEPTH

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

proc do_empty(unit p, a)=
	evallv(a)
	pc_gen(kclear)
	pc_setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value)
end

proc do_setinplace=
	if pccurr.opcode=kload and pccurr.opndtype=memaddr_opnd then
		pccurr.inplace:=1
	fi
end
=== pcl_api.m 0 0 18/29 ===
!EXPORT INT PCLSEQNO
int STSEQNO

global pcl pcstart			!start of pcl block
global pcl pccurr			!point to current pcl op
global pcl pcend			!point to last allocated pclrec

!byte pcfixed				!whether code is fixed up
global int pclength

global pcl pcmin, pcmax		!limits of pcl code generated for program

int initpcalloc=65536

global proc pc_gen(int opcode, pcl p=nil) =
	if p=nil then p:=newpcl() fi
	p.opcode:=opcode
end

global proc pc_genx(int opcode, int x, pcl p=nil) =
	if p=nil then p:=newpcl() fi
	p.opcode:=opcode
	pccurr.x:=x
end

global func newpcl:pcl p =

	p:=pcm_allocnfz(pclrec.bytes)

	pccurr.next:=p
	pccurr:=p

	++pclength

	return pccurr
end

global proc pcl_start =
!reset tcstart/tccurr for new TCL sequence (new proc or new init data)
	pcstart:=pcm_allocnfz(pclrec.bytes)
	pcstart.opcode:=knop
	pccurr:=pcstart
!	pcseqno:=0
	pclength:=0

	pcmin min:=pcstart

end

global func pcl_end:pcl pc=
!Terminate sequence; sets pcstart to nil so that pcl cannot be generated
!outside proc bodies etc
!But caller should copy tcstart value, or use its value returned here

	pcmax max:=pccurr
	pc:=pcstart
	if pc.opcode=knop then
		pc.next
	else
		pc
	fi
end

global proc pc_setmode(int m)=
	pccurr.mode:=getpclmode(m)
	pccurr.size:=ttsize[m]

	if pclhastype[pccurr.opcode]=2 then
		pccurr.mode2:=pccurr.mode
	fi
end

global proc pc_setmode_u(unit p)=
	pc_setmode(p.mode)
end

global proc pc_setmode2(int m)=
	pccurr.mode2:=getpclmode(m)
end

global proc pc_comment(ichar s)=
!*!	return when fregoptim or fpeephole		!will get skipped anyway
!	RETURN WHEN DOREDUCE			!comments suppressed as they get in the way
!STATIC INT CCC
!CPL "COMMENT",++CCC
	pc_gen(kcomment, pgenstring(s))
end

global proc pc_setnargs(int n)=
	pccurr.nargs:=n
end

global proc pc_genix(int opcode, scale=1, offset=0) =
!originally intended for combinations of ptr ops to be combined into
!previous ones, but that has now been dropped.
!Instead any such reductions will be done in a separate pass, much simpler
	pcl p:=newpcl()
	p.opcode:=opcode
	p.scale:=scale
	p.extra:=offset
end

global proc pc_gencond(int opcode, cond, pcl p=nil) =
	if p=nil then p:=newpcl() fi
	p.opcode:=opcode
	p.condcode:=cond
end

global proc pc_genxy(int opcode, int x, y, pcl p=nil) =
	if p=nil then p:=newpcl() fi
	p.opcode:=opcode
	p.x:=x
	p.y:=y
end

global proc pc_setscaleoff(int scale, offset:=0)=
	pccurr.scale:=scale
	pccurr.extra:=offset
end

global func pgenint(int a)pcl p=
	p:=newpcl()
	p.value:=a
	p.opndtype:=int_opnd
	return p
end

global func pgenrealmem(real x)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=real_opnd
	return p
end

!global func pgenrealimm(real x)pcl p=
!	p:=newpcl()
!	p.xvalue:=x
!	p.opndtype:=realimm_opnd
!	return p
!end

global func pgenstring(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=string_opnd
	return p
end

global func pgenlabel(int a)pcl p=
	p:=newpcl()
	p.labelno:=a

	p.opndtype:=label_opnd
	return p
end

global func pgenmem(symbol d)pcl p=

	p:=newpcl()

	p.def:=d

	p.opndtype:=mem_opnd
	return p
end

global func pgenmemaddr(symbol d)pcl p=
	p:=newpcl()
	p.def:=d

	p.opndtype:=memaddr_opnd
	return p
end

global proc pgencomment(ichar s)=
	return when fregoptim or fpeephole		!will get skipped anyway
!	RETURN WHEN DOREDUCE			!comments suppressed as they get in the way
!STATIC INT CCC
!CPL "COMMENT", ++CCC
	pc_gen(kcomment, pgenstring(s))
end

global func pgenname(ichar s)pcl=
	return pgenmem(makesymbol(s))
end

global func pgennameaddr(ichar s)pcl=
	return pgenmemaddr(makesymbol(s))
end

global func addstr(ichar s, t)ichar=
	static [256]char str
	strcpy(str, s)
	strcat(str, t)
	str
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

	pushsysarg(c, 3, nargs)
	pushsysarg(b, 2, nargs)
	pushsysarg(a, 1, nargs)
!
	p.nargs:=nargs

	d:=getsysfnhandler(fnindex)
	if d then
		pc_gen((asfunc|kcallf|kcallp), pgenmemaddr(d))
		pc_setnargs(nargs)
	else
!		pc_gen((asfunc|kcallf|kcallp), gennameaddr(sysfnnames[fnindex]+3))
		pc_gen((asfunc|kcallf|kcallp), pgenname(sysfnnames[fnindex]+3))
	fi
	pccurr.nargs:=nargs
end

global proc pushsysarg(unit p, int n, &nargs) =
!return 0 or 1 args pushed
	if p then
		evalunit(p)
		pc_gen(ksetarg)
		pc_setmode_u(p)
		pccurr.x:=n
		pccurr.y:=n			!ASSUMES ALL INTS; however this only important
							!for arm64, and only matters if more than 8 args
		++nargs
	fi
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
		println "Sysfn not found:",str
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

global proc setfunctab=
	if pnprocs=nil then
		pnprocs:=makesymbol("$nprocs", staticid)
!CPL "SET PNPROCS", PNPROCS
		pnprocs.mode:=ti64
		pprocname:=makesymbol("$procname", staticid)
		pprocaddr:=makesymbol("$procaddr", staticid)
	fi
end

global func makesymbol(ichar s, int id=nullid)symbol d=
	d:=newstrec()
	d.name:=pcm_copyheapstring(s)

	d.nameid:=id

	if id=staticid then
		addstatic(d)
	fi
	d
end

global func getfullname(symbol d, int backtick=0)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	int n:=0
	symbol e:=d
	ichar name

!	if fpshortnames then return d.name fi

	str[1]:=0
	if backtick then
		strcpy(str, "`")
	fi

!	if d.imported then
	if d.isimport then
		name:=(d.truename|d.truename|d.name)
		if backtick then
			strcat(str, name)
			strcat(str, "*")
		else
			strcat(str, name)
		fi
		return str
	fi

!STRCAT(STR, "GF:")
!CPL

	if d.equivvar then
!PRINTUNIT(D.EQUIVVAR)

		d:=d.equivvar.def
	fi


!CPL =D.NAME, =D.OWNER
	if d.owner then
		if d.owner.nameid=procid then
			strcat(str, d.owner.owner.name)
			strcat(str, ".")
		fi
		strcat(str, d.owner.name)
		strcat(str, ".")
	fi
	strcat(str, d.name)
!!	if d.nameid in [frameid, paramid] then
!!IF CURRFUNC=NIL THEN
!!ABORTPROGRAM("CURRFUNC=0\N\N")
!!FI
!!		strcat(str, currfunc.name)
!!		strcat(str, ".")
!		strcat(str, d.name)
!		return str
!	fi
!
!	if backtick then
!		strcat(str, d.name)
!	else
!		return d.name
!	fi
end
=== pcl_diags.m 0 0 19/29 ===
const fshowpclseqno=1
!const fshowpclseqno=0

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

const tab1="    "
const tab2="        "

ref[]byte labeltab

int pclseqno

global ichar longstring					!used in stropnd
global int longstringlen

global proc strpcl(pcl p)=
	[256]char str
	int opcode, n,x,y
	symbol d, e

	const showformatted=1

	opcode:=p.opcode

!	if fshowpclseqno then
!		psstr(string(pclseqno, "z5"))
!		psstr("  ")
!	fi

!CPL "STRPCL", PCLNAMES[P.OPCODE]

!PSSTR("<PCL>")
!PSINT(INT(P.A))
!PSSTR( " ")

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
	when kmaths, kmaths2 then
!		strcpy(str, "maths.")
		strcpy(str, "m.")
		strcat(str, mathsnames[p.mathsop])
	else
		strcpy(str, pclnames[opcode])
	esac

	gs_leftstr(dest,str,9)

	str[1]:=0
	if p.mode then
		strcat(str, strmode(p.mode))

		if pclhastype[opcode]=2 then
			strcat(str, "/")
			strcat(str, strmode(p.mode2))
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

	if p.opndtype then
		psstr(" ")
		psstr(stropnd(p))
	fi
	pstabto(40)

	if fshowpclseqno then
		psstr("! ")
		psstr(strint(pclseqno,"z5"))
		psstr("  ")
	fi
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

global func writeallpcl:ref strbuffer=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	ref procrec pp
	symbol d

	labeltab:=pcm_allocz(mlabelno)			!indexed 1..labelno

	gs_init(dest)
	destlinestart:=dest.length

	gs_strln(dest, "!PROC PCL")


	pp:=proclist
	while pp, pp:=pp.nextproc do
		currfunc:=d:=pp.def

		psprocsig(d)
		psprocbody(d)

		psstrline("End")
		psline()
	od


	pcm_free(labeltab, mlabelno)

	return dest
end

proc psstr(ichar s)=
	gs_str(dest,s)
end

proc psstrline(ichar s)=
	gs_str(dest,s)
	gs_line(dest)
end

proc psline=
	gs_line(dest)
end

proc psint(int a)=
	gs_str(dest,strint(a))
end

proc psname(symbol d)=
	gs_str(dest,getfullname(d))
end

proc psprocsig(symbol d)=
	symbol e
	byte comma:=0
	int lastmode:=tvoid, m, lastsize, size

	psstr("Proc ")
	psstr(d.name)
	psstr("(")

	e:=d.deflist

	while e, e:=e.nextdef do
		if e.nameid=paramid then
			if comma then psstr(", ") fi
			if e.mode<>lastmode then
				lastmode:=e.mode
				psstr(strmode(lastmode))
				psstr(" ")
			fi
			psstr(e.name)

			comma:=1
		fi
	od
	psstr(")")

	if d.mode then
		psstr(strmode(d.mode))
	fi
	psstrline(" =")


	e:=d.deflist
	comma:=0
	while e, e:=e.nextdef do
		if e.nameid in [frameid, staticid] then
			psstr(tab1)
			psstr(strmode(e.mode))
			psstr(" ")
			psstrline(e.name)
			comma:=1
		fi
	od
	if comma then psline() fi
end

proc psprocbody(symbol d)=
	pcl p

	p:=d.pccode
	return unless p

!do first pass populating label table

	while p, p:=p.next do
		if p.opcode<>klabel then
			if p.opndtype=label_opnd then
				labeltab[p.labelno]:=1
			fi
		fi
	od

	p:=d.pccode
	destlinestart:=dest.length
	pclseqno:=0

	while p, p:=p.next do
		++pclseqno

!UNLESS P.OPCODE IN [KSETCALL, KSETARG] THEN
		strpcl(p)
		gs_line(dest)
!END
		destlinestart:=dest.length
	od

	psline()
end

global proc strlabel(int labelno,colon=0)=
	psstr("L")
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

global proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col od
end

global func stropnd(pcl p)ichar=
	static[512]char str
!	static[32]char str
	int length
	symbol d
	static ichar longstring

	if p=nil then
		return ""
	fi

	str[1]:=0

	case p.opndtype
	when int_opnd then
		return strint(p.value)
	when real_opnd then
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
			convertstring(p.svalue, &str[1]+1, length)
			strcat(str,"""")

		else

			if longstring then
				pcm_free(longstring,longstringlen)
			fi
			longstringlen:=length*2
			longstring:=pcm_alloc(longstringlen)
			longstring^:='"'
			length:=convertstring(p.svalue, longstring+1, length)
			(longstring+length+1)^:='"'
			(longstring+length+2)^:=0
			return longstring
		fi

	when mem_opnd then
		d:=p.def
		strcat(str, p.def.name)

	when memaddr_opnd then
		strcpy(str, "&")
		recase mem_opnd

	when label_opnd then
		fprint @str,"## ","#",p.labelno

	when no_opnd then
		return ""

!	when data_opnd then
!		fprint @str,"<Data * # (#)>", p.size,p.svalue

	else
		println "---------",OPNDNAMES[P.OPNDTYPE]
		return "<PCLOPND?>"
	esac

	return str
end

=== mcl_decls.m 0 0 20/29 ===
global const maxoperands=16
!global [maxoperands]pclopndrec pstack
global [maxoperands]pcl	   pclopnds
global [maxoperands]pcsrec pclstack

!The following are reset per proc and augmented as it is processed
global [maxoperands]byte pcltempflags		!1 means a temp uses local storage
global [maxoperands]mclopnd pcltempopnds	!store mcl opnd for such a temp

global int noperands						!number of pcl operands, including wide

global macro zz = noperands
global macro yy = noperands-1
global macro xx = noperands-2
global macro ww = noperands-3

global int mstackdepth						!hw stack size (pcl operands, + extra for wide, + padding)

global const u64 invertbytes = 0x0101'0101'0101'0101

global const maxcalldepth=32
global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callblockret	!1 if fnc returns a block
global [maxcalldepth]u32 callblocksize	!size of any returned block
global [maxcalldepth,4]u32 callargsize	!size of any block pushed in low args
global int ncalldepth

!global symbol currfunc
global symbol blockretname
!global int mstackdepth						!hw stack size (pcl operands, + extra for wide, + padding)
global byte localshadow			!1 if local, proc-wide shadow space used for a call

global byte r10used				!these may be set in pass2 when occupied by params
global byte r11used

global const maxblocktemps=50
global [maxblocktemps]symbol blockdefs
global int nblocktemps

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)

global u64 workset					!'1' bits mean available as workreg in this proc
global u64 regset					!'1' bits mean workreg currently in use
global u64 usedset					!accumulates '1' bits from regset to show used regs in proc
global u64 regvarset				!'1' means holds a regvar
global u64 pclset					!'1' means reg hold pcl operand

global [stdnames.bounds]byte ploadopx
global [stdnames.bounds]byte ploadop

proc start=
	for i in ploadop.bounds do ploadop[i]:=m_nop od

	ploadop[tu8]:=ploadop[tu16]:=ploadop[tu32]:=m_movzx
	ploadop[ti8]:=ploadop[ti16]:=ploadop[ti32]:=m_movsx
	ploadop[tr32]:=m_movd
	ploadop[tr64]:=m_movq
	ploadop[tu64]:=ploadop[ti64]:=m_mov
end

global pcl currpcl

global int pmode
=== mcl_gen.m 0 0 21/29 ===
macro mdivider = mcomment("-"*40)

INT PCLLENGTH
INT NPROCS

global proc genmcl=
	ref strbuffer asmstr
	ref procrec pp
	symbol d
	int tt

	return when mcldone

	tt:=os_clock()
!CPL "GENMCL"

	inithandlers()
	mclinit()

	mcomment("Generated ASM")
!RETURN

	dolibfiles()

	pp:=staticlist
	while pp do
		d:=pp.def
!CPL "DOSTATIC",D, D.NAME
		dostaticvar(d)
		pp:=pp.nextproc
	od

	mcomment("")

!import list not needed for AA; only for NASM

!	for i to ndllproctable do
!		gendllproc(dllproctable[i])
!	od

	pp:=proclist
	while pp do
		d:=pp.def
		genprocmcl(d)
		pp:=pp.nextproc
	od

	genrealtable()
	genabsneg()
	genstringtable()

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)

	if fpeephole then
!*!		peephole()
	fi

	mcldone:=1

	mcltime:=os_clock()-tt

!CPL "AFTER MCL:", MCLTIME

!CPL $LINENO


end

global proc genprocmcl(symbol d) =
	currfunc:=d
	setsegment('C',1)

	genmc(m_procstart, genmemaddr(currfunc))
	genmc(m_labelname, genmemaddr(currfunc))

	mcomment("?>>")
	mclprocentry:=mccodex

	do_proccode_a()						!initialise offsets etc

	mcomment("?>>")
	mclprocentry:=mccodex

	mdivider()

!FIXAST(D.CODE)

	currpcl:=d.pccode
	noperands:=0

!CPL "GENPROC1", D.NAME
	while currpcl, currpcl:=currpcl.next do
		convertpcl(currpcl)				!currpcl may be stepped in handler
		exit when currpcl=nil
	od
!CPL "GENPROC2"

	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mcomment("---")					!injection of entry code goes wrong otherwise
	fi

	mdivider()

	do_proccode_b()						!do entry code (inject into start)
	do_proccode_c()						!do exit code

	genmc(m_procend)

	if noperands then
		println currfunc.name,,": PCL stack not empty"
		mcomment("PCL stack not empty")
	fi

PCLSET.[0]:=0
IF PCLSET THEN
CPL CURRFUNC.NAME, "PCLSET NOT EMPTY"
FI

!CPL "DONE", D.NAME
	currfunc:=nil

end

proc dolibfiles =
	mcomment("Lib files go here")
	mcomment("")
end

proc dostaticvar(symbol d) =

	return when d.isimport or d.equivvar

	if d.scope = program_scope and d.name^='$' then
		if eqstring(d.name,"$cmdskip") then
			d.scope:=export_scope				!export from mlib subprog
		fi
	fi

	setsegment((d.code|'I'|'Z'), getalignment(d.mode))
	genmc(m_labelname, genmemaddr(d))

	if d.code then
		genidata(d.code)
	else
		genmc(m_resb, genint(ttsize[d.mode]))
	fi
end

proc genidata(unit p)=
	[2000]byte data
	int t, tbase, offset
	byte allbytes, nbytes
	unit q, a
	symbol d
	ref char s
	mclopnd dx

	t:=p.mode
!CPL "GENIDATA",JTAGNAMES[P.TAG], STRMODE(T)
!PRINTUNIT(P)

	mmpos:=p.pos
	tbase:=ttbasetype[t]
	a:=p.a
!RETURN

	case p.tag
	when jconst then
		case tbase
		when tref then				!ref or string
			if t=trefchar and p.isastring then
				gerror("idata/empty str?") unless p.svalue
				if p.strtype='B' then gerror("1:B-str?") fi
				genmc(m_dq, genlabel(getstringindex(p.svalue)))
			else
				gendataint(p.value)
			fi

		when tr64 then
			genmc(m_dq, genrealimm(p.xvalue, tr64))

		when tr32 then
			genmc(m_dd, genrealimm(p.xvalue, tr32))

		when tarray then			!should be data string
			if p.strtype=0 then gerror("idata/array/not blockdata") fi
			doblockdata(p.svalue, p.slength)

		else						!assume integer
			gendataint(p.value, t)

		esac

	when jmakelist then
		q:=a

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
			doblockdata(cast(&data), nbytes)
		else
			q:=a
			while q, q:=q.nextunit do
				genidata(q)
			od
		fi

	when jname then
		d:=p.def
		offset:=0
doname:
		case d.nameid
		when staticid, procid, dllprocid then
			dx:=applyoffset(genmemaddr(d), offset)

		when labelid then
			if d.labelno=0 then d.labelno:=++mlabelno fi
			dx:=genlabel(d.labelno)
		else

			MCOMMENT("Idata &frameXXX")
!			gerror("Idata &frameXXX")
		esac
		genmc(m_dq, dx)

	when jconvert then
		genidata(p.a)

	when jshorten then
		gendataint(a.value, t)

	when jaddrof then
		if a.tag<>jname then recase else fi
		offset:=0
		if p.b then offset:=p.b.value fi
		d:=a.def
		doname

	else
		gerror_s("IDATA: ", jtagnames[p.tag], p)

	esac
end

proc gendataint(int a, mode=ti64)=
	static []byte opctable =(m_db, m_dw, 0, m_dd, 0,0, 0, m_dq)

	genmc(opctable[ttsize[mode]], genint(a))
end

!proc doblockdata(unit p) =
proc doblockdata(ref byte s, int n) =
!p contains jconst with array type; treat as data-string

	ref u64 d:=cast(s)
	int nwords, r

	return when n=0

	nwords:=n/8

	to nwords do
		genmc(m_dq, genint(d++^))
	od

	r:=n-nwords*8
	if r then
		s:=cast(d)
		to r do
			genmc(m_db, genint(s++^))
		od
	fi
	MCOMMENT("ENDDATA")

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
		(ktoboolf, 		ktoboolt),

		(kcallf,		kcallp),
		(kicallp,		kcallp),
		(kicallf,		kcallp),

		(kendmx,		kresetmx),

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

global proc doshowpcl(pcl p)=
	[1256]char str

	return unless fshowinlinepcl

	return when p.opcode in [kcomment, klabel]

	strcpy(str,"                       ")
	strcat(str,strpclstr(p, str.len))
	mcomment(PCM_COPYHEAPSTRING(str))
end

global proc unimpl(pcl p)=
	[100]char str
	fprint @str, "Unimpl: # (#)", pclnames[p.opcode], strmode(pmode)
	CPL STR
	mcomment(pcm_copyheapstring(str))
end

=== mcl_genaux.m 0 0 22/29 ===
global int nsaveregs, nsavefregs		!number of integer/float non-vols to be saved
global int nspilled						!spilled int/float registers

global int framesize					!counts bytes in stack frame
!global int framebytes, frameoffset, paramoffset
global int paramstackoffset
global int paramspilloffset

symbol dblockarg

int nsavedregs, nsavedxregs
!global int retindex

[20]symbol nametable
int nnametable

global proc do_proccode_a=
	symbol d
	int reg, xreg, n, r, npregs

	clear pcltempflags
	r10used:=r11used:=0
	usedset:=0

	workset:=0
	for r in r0..r9 do workset.[r]:=1 od			!int regs available
	for r in xr4..xr15 do workset.[r]:=1 od			!xregs

	regset:=usedset:=regvarset:=0

	PCLSET:=0

!CPL "-----------", USEDSET, CURRFUNC.NAME, =REGSET, =PCLSET

	nsavedregs:=nsavedxregs:=0

!MCOMMENT("PROCA")


	retindex:=createfwdlabel()
end

global proc do_proccode_b=
! Stack layout (grows downwards)
!	| ...
!	| Pushed arg 6
!	| Pushed arg 5
!	| Shadow space 32-byte		For spilled args (always present even with 0-3 args)
!	| ----------
!	| Pushed return address		Via 'call'
!	| ----------				Above done in caller; below in callee
!	| Pushed nonvol workregs	If extend to R3 and above
!	| Pushed nonvol workxregs	If extend to XR6 and above
!	| ----------				Above done in caller; below in callee
!	| Pushed FP					Save FP
!	| ----------
!	! Local vars				All locals (when used)
!	| ----------
!	| Temps						All temps
!	| ----------
!	| 32-byte shadow space		For any calls made in this func
!	| [Stack adj]				Extra slot may be added to keep stack pointer 16-byte aligned

	int retmode, hasequiv, offset, size, reg
	int nsavedbytes, paramoffset
	mclopnd ax
	symbol d
	[100]char str, newname
	int r, n, ntemps

!CPL "///PROCB"

!CPL $LINENO
!MCOMMENT("PROCB1")

	setmclentry(mclprocentry)
!MCOMMENT("PROCB2")

	framesize:=0
	dblockarg:=nil

!CPL =USEDSET:"H", =WORKSET:"H", =REGSET:"H"

	for r in r3..r9    when usedset.[r] do ++nsavedregs od
	for r in xr6..xr15 when usedset.[r] do ++nsavedxregs od

	nsavedbytes:=(nsavedregs+nsavedxregs)*8

!CPL $LINENO
!CPL CURRFUNC.NAME,"HIGHWORKREG=", STRREG(HIGHWORKREG)
!CPL CURRFUNC.NAME,"HIGHWORKXREG=", STRREG(HIGHWORKXREG)


!allocate offsets to args, and set defines

!CPL "PROC B", =CURRFUNC.NAME, =CURRFUNC.NPARAMS, STRPMODE(CURRFUNC.MODE, CURRFUNC.SIZE)

	if ttbasetype[currfunc.mode]=tblock then	!need to inject extra parameter
		GERROR("PROCB/BLOCKRET")
!		dblockarg:=tc_makesymbol("$block", param_id)
!		dblockarg.nextparam:=currfunc.nextparam
!		dblockarg.mode:=tblock
!		dblockarg.size:=currfunc.size
!
!		currfunc.nextparam:=dblockarg
!		++currfunc.nparams
	fi
!CPL "PROC B2", =CURRFUNC.NAME, =CURRFUNC.NPARAMS, STRPMODE(CURRFUNC.MODE, CURRFUNC.SIZE)

!IF NSAVEDBYTES THEN
!	CPL "Saving:", nsavedregs, nsavedxregs,"in", currfunc.name
!FI

	paramoffset:=16+nsavedbytes		!between top of stackframe and 1st param is fp/retaddr/saved

	d:=currfunc.deflist
	while d, d:=d.nextdef do
		case d.nameid
		when paramid then
!			if not d.reg then
				d.offset:=paramoffset
				paramoffset+:=8
!				if d.used then
					genmc_def(m_define, d)
!				elsif pcheckunusedlocals then
!					println "Unused param:", d.name, "in", currfunc.name
!				fi
!			else
!			fi

		when frameid then
!			if not d.used then
!				if pcheckunusedlocals then
!					println "Unused local:", d.name, "in", currfunc.name
!				fi
!				nextloop
!			fi

			size:=ttsize[d.mode]
!			if d.mode=tblock then
!				size:=ttsize[d.mode]
!			fi

			if d.equivvar then
!				MERROR("PCODEB/@")
			elsif not d.reg then
				framesize+:=roundsizetg(size)
				d.offset:=-framesize
				genmc_def(m_define, d)
			else
				int rr:=d.reg
				d.reg:=0
				genmc(m_definereg, genmem(d), mgenreg(rr, ttbasetype[d.mode]))
				d.reg:=rr
			fi
		esac
	od

	ntemps:=0
	for i to maxoperands when pcltempflags[i] do
		++ntemps
		framesize+:=8
		ax:=pcltempopnds[i]
		ax.offset:=-framesize
		genmc(m_definetemp, genname(gettempname(currfunc,i)), genint(ax.offset))
	od


	framesize+:=32									!shadow space
!CPL "FS AFTER SHADOW SPACE", FRAMESIZE

!Total stackframe size:
!    Nsavebytes
!   +Pushed fp
!	+Framesize (locals, temps, + 32 bytes shadow space)
!The first two already pushed; framesize will be added to sp
!This total needs to be odd, otherwise framesize is increased by 8

!Stack is misaligned on entry, to fix, requires a stackframe of an odd multiple of 8
!Stackframe size so far is framebytes+nsavedbytes+8 (last for pushed fp)
!CPL CURRFUNC.NAME, =FRAMESIZE, =NSAVEDBYTES, =framesize+NSAVEDBYTES+8


	unless (framesize+nsavedbytes+8) iand 8 then			!keep stack frame 16-byte aligned
!	if (framesize+nsavedbytes+8) iand 8 then			!keep stack frame 16-byte aligned
		framesize+:=8
!CPL "NEW FRAME",=FRAMESIZE
	end

	savevolregs(nsavedregs, nsavedxregs)

!generate stack entry code proper:

	genmc(m_push, dframeopnd)
	genmc(m_mov, dframeopnd, dstackopnd)
	pushstack(framesize)

!spill any args to shadow space
	spillparams()

!	MCOMM("="*40)
!MCOMMENT("PROCB3")
	resetmclentry()
!MCOMMENT("PROCB4")
end

global proc do_proccode_c=
	int offset
	mclopnd ax, bx

!	MCOMM("="*40)

!MCOMMENT("PROCC")
	genmc(m_label, genlabel(retindex))

!	if dblockarg then
!!		MCOMMENT("BLOCK RETURN COPY NEEDED")
!!D0 has address of block to be copied
!!It needs to be returned on D0 after copying
!MCOMMENT("BLOCKRET1")
!
!		ax:=genireg(r0)
!		bx:=genreg(r1)
!		genmc(m_mov, bx, genmem(dblockarg))
!		nextworkreg:=r2
!		copyblock(genireg(r1), ax, dblockarg.size)		!does not affect r0
!		genmc(m_xchg,  genreg(r0), bx)
!
!MCOMMENT("BLOCKRET2")
!
!	fi

	popstack(framesize)
	genmc(m_pop, dframeopnd)
	restorevolregs(nsavedregs, nsavedxregs)

	genmc(m_ret)
end

proc spillparams=
	symbol d
	mclopnd ax
	int offset:=16, regoffset:=0, xregoffset, firstoffset

	regoffset:=0

	d:=currfunc.deflist

!	if currfunc.variadic then				!C proc def using ...
!		firstoffset:=d.offset				!param offsets may be pushed up
!
!		for i:=currfunc.nparams to 3 do				!0-based; if nparams=2, loops over 2..3 as 0..1 are normal
!			ax:=genindex(areg:rframe, size:8, offset:i*8+firstoffset)
!			genmc(m_mov, ax, genreg(i+r10))
!		od
!	fi
!
	while d, d:=d.nextdef do
		if d.nameid=paramid then
			if regoffset>3 then exit fi

			if d.used or regoffset=0 then
				ax:=genindex(areg:rframe, size:8, offset:d.offset)
				case d.mode
				when tr64 then
					genmc(m_movq, ax, genxreg(regoffset+xr0))
				when tr32 then
					genmc(m_movd, changeopndsize(ax,4), genxreg(regoffset+xr0))
				else
					genmc(m_mov, ax, genreg(regoffset+r10))
				esac
			fi

			offset+:=8
			++regoffset
		fi
	od

end

proc savevolregs(int nregs, nxregs)=
	int reg
	mclopnd ax

!CPL "SAVEVOLS   ", NREGS, NXREGS, CURRFUNC.NAME, USEDSET:"H"

	if nregs then
		for r in r3..r9 when usedset.[r] do
			genmc(m_push, genreg(r))
			exit unless --nregs
		od
	fi

	if nxregs then
		ax:=genreg(r0)
		for r in xr6..xr15 when usedset.[r] do
			genmc(m_movq, ax, genreg(r))
			genmc(m_push, ax)
			exit unless --nxregs
		od
	fi
end

proc restorevolregs(int nregs, nxregs)=
	mclopnd ax

!CPL "RESTOREVOLS", NREGS, NXREGS, CURRFUNC.NAME, USEDSET:"H"
	if nxregs then
		ax:=genreg(r0)
		for r:=xr15 downto xr6 when usedset.[r] do
			genmc(m_pop, ax)
			genmc(m_movq, genreg(r), ax)
			exit unless --nxregs
		od
	fi

	if nregs then
		for r:=r9 downto r3 when usedset.[r] do
			genmc(m_pop, genreg(r))
			exit unless --nregs
		od
	fi
end

proc gendq(int a)=
	genmc_int(m_dq, a)
end

global proc genabsneg=
	if lababs32+lababs64+labneg32+labneg64 then
		setsegment('I', 16)
	fi

	if lababs32 then
		mcomment("lababs32")
		genmc_label(m_label, lababs32)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mcomment("lababs64")
		genmc_label(m_label, lababs64)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
		mcomment("labneg32")
		genmc_label(m_label, labneg32)
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mcomment("labneg64")
		genmc_label(m_label, labneg64)
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mcomment("labzero")
		genmc_label(m_label, labzero)
		gendq(0)
	fi

	if labmask63 then
		mcomment("mask63/offset64")
		genmc_label(m_label, labmask63)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc_label(m_label, laboffset64)
		gendq(0x43E0'0000'0000'0000)
	fi
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

!global func mdefinelabel:int =
!	genmc(m_label, genlabel(++mlabelno))
!	return mlabelno
!end
!
!global proc mdefinefwdlabel(int lab) =
!	genmc(m_label, genlabel(lab))
!end

global proc copyblock(mclopnd ax,bx, int n)=
!ax, bx refer to memory; do ax:=bx for n bytes

	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg, axreg

	if n=16 then
		rx:=genreg(gwrx())
		genmc(m_movdqu, rx, bx)
		genmc(m_movdqu, ax, rx)
		return
	fi

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=genreg(gwri())		!work reg

	offset:=0
		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)

	if nwords in 1..4 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax, targetsize)
		bx:=changeopndsize(bx, targetsize)

		to nwords do
			genmc(m_mov, rx, applyoffset(bx, offset))
			genmc(m_mov, applyoffset(ax, offset), rx)
			offset+:=8
		od

	elsif nwords then			!use a loop
		rcount:=genreg(gwri())
		lab:=++mlabelno

		ax.size:=8

		genmc(m_mov, rcount, mgenint(nwords))
		genmc(m_label, genlabel(lab))
		genmc(m_mov, rx, bx)
		genmc(m_mov, ax, rx)

		genmc(m_add, mgenreg(ax.reg), mgenint(targetsize))
		genmc(m_add, mgenreg(bx.reg), mgenint(targetsize))

		genmc(m_dec, rcount)
		genmc_cond(m_jmpcc, ne_cond, genlabel(lab))

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
end

global func makesimpleaddr(mclopnd ax)mclopnd bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg, reg, regix

!CPL "MSA CALLED", MSTROPND(AX)

	reg:=ax.reg
	regix:=ax.regix
	if reg=rframe then reg:=rnone fi

	if ax.mode<>a_mem then merror("MSA") fi

	if reg=rnone and regix=rnone then
		newreg:=gwri()
	elsif reg then				![reg] only; already simple
		return ax
	elsif regix then			![regix] only; may be scaled; use lea anyway
		newreg:=regix
	else						![reg+regix]
		newreg:=regix
	fi

	bx:=mgenireg(newreg)

	genmc(m_lea, genreg(newreg), ax)
	return bx
end
=== mcl_stack.m 0 0 23/29 ===
!PCL Operand Stack 

global func getopnd(int n, mode=pmode, reg=rnone)mclopnd ax =
!get access mode for operand n
	int oldreg, size
	mclopnd bx
	symbol d

	pcl p:=pclopnds[n]
	pcsrec ps:=pclstack[n]

	size:=stdsize[mode]

	if ps.reg then
		oldreg:=ps.reg
		if reg and oldreg<>reg then
			ax:=genreg(reg, size)
			bx:=genreg(oldreg, size)
			PCLSET.[OLDREG]:=0
			PCLSET.[REG]:=1
			genmc(m_mov, ax, bx)
		else
			ax:=genreg(oldreg, size)
		fi

	elsif ps.temp then
		ax:=mgentemp(n, mode)

	elsecase p.optype

	when mem_opnd then
		d:=p.def
		if mode=tblock and d.nameid<>paramid then
			mode:=tu64
			recase memaddr_opnd
		else
			ax:=genmem(p.def, size)
		fi

	when memaddr_opnd then
		d:=p.def
		if d.nameid=paramid and d.mode=tblock then		!pcl mode will be u64
			ax:=genmem(d, size)
		else
!			ax:=genreg(getworkregc(mode, reg))
			ax:=gwrm(mode, reg)
			genmc(m_lea, ax, genmem(d, size))
		fi

	when int_opnd then

		case size
		when 2 then
			p.value iand:=0xFFFF
		when 4 then
			p.value iand:=0xFFFF'FFFF
		esac

		bx:=genint(p.value, size)
		if p.value in i32.bounds then			!keep as immediate
			ax:=bx
		else
			ax:=gwrm(mode, reg)
			genmc(m_mov, ax, bx)
		fi

	when real_opnd then
		ax:=genrealmem(p.xvalue, size)

	when string_opnd then
		ax:=gwrm(tu64, reg)
		genmc(m_lea, ax, genlabelmem(getstringindex(p.svalue)))

	when label_opnd then
!		ax:=genreg(getworkireg(reg))
		ax:=gwrm(tu64, reg)

		genmc(m_lea, ax, genlabelmem(p.labelno))
!		else
!			GERROR("LABELDEF NOT DEF")
!		fi
!
	else
error:
		merror("getopnd", opndnames[p.optype])
	fi

	if ax.mode=a_reg then		!ensure correctly set to reg
		pclstack[n].reg:=ax.reg
		pclstack[n].temp:=0
		pclstack[n].code:=0
		pclset.[ax.reg]:=1
	fi

	ax
end

global func loadopndr(int n, mode=pmode, reg=rnone)mclopnd ax =
!must return with value in register. But it can be in-situ if it is a regvar
!and no newreg is specified
	pcl p:=pclopnds[n]
	pcsrec ps

	int size:=stdsize[mode]
	mclopnd bx

	ax:=getopnd(n, mode, reg)

	ps:=pclstack[n]
	if ps.reg then
		return ax
	fi

!CPL "LOADOPNDR", STRREG(REG), =SIZE

	if reg=rnone then
		if stdfloat[mode] then
			bx:=genxreg(reg:=gwrx(), size)
		else
			bx:=genreg(reg:=gwri(), size)
		fi
	else
		bx:=genreg(reg, size)
	fi

	if ps.temp then
		genmc(m_mov, bx, ax)

	elsecase p.optype
	when mem_opnd, int_opnd, real_opnd  then
		genmc(m_mov, bx, ax)

!	when memaddr_opnd then
!		d:=p.def
!		if d.nameid=paramid and d.mode=tblock then		!pcl mode will be u64
!			ax:=genmem(d, size)
!		else
!			ax:=genreg(getworkregc(mode, reg))
!			genmc(m_lea, ax, genmem(d, size))
!		fi
!
!	when string_opnd then
!		ax:=genreg(getworkireg())
!		genmc(m_lea, ax, genlabelmem(getstringindex(p.svalue)))
!
!	when label_opnd then
!		ax:=genreg(getworkireg())
!
!		genmc(m_lea, ax, genlabelmem(p.labelno))
!!		else
!!			GERROR("LABELDEF NOT DEF")
!!		fi
!
	else
error:
		merror("loadopndr", opndnames[p.optype])
	fi

	pclstack[n].reg:=reg
	pclstack[n].temp:=0
	pclstack[n].code:=0
	pclset.[reg]:=1

	bx
end

global func loadopndw(int n, mode=pmode)mclopnd ax =
!load opnd to reg where it will be written
!this only really affects regvars, and only when no dest is specified,
!as otherwise it will be copied there anyway
	mclopnd bx
	int oldreg, newreg

	ax:=loadopndr(n, mode)
	oldreg:=ax.reg

	if regvarset.[ax.reg] then			!need to copy elsewhere
!		newreg:=gwr(mode)
!		bx:=mgenreg(newreg, mode)
		bx:=gwrm(mode)
		genmc(m_mov, bx, ax)
		bx
	else
		ax
	fi
end

global func loadparam(int n, mode=pmode, reg)mclopnd ax =
	loadopndr(n, mode, reg)
end

!global func loadretval(int n, mode, reg)mclopnd ax =
!!Load operand to return register
!!reg will be r0 for most functions
!	ax:=getopnd(n, mode, reg)
!	ax:=loadtoreg_m(ax, mode, reg)
!	ax
!end

global proc pushopnd(int n, mode)=
!Push opnd n to hardware stack then pop it from pcl stack
!The hardware stack is popped on return from a call

	mclopnd ax, bx
	pcl p:=pclopnds[n]
	pcsrec ps:=pclstack[n]

	if mode=tvoid then mode:=p.mode fi

!First look for operands that can be directly pushed without using a register
	if ps.code then
		case p.optype
		when mem_opnd then
			if p.size=8 then
				ax:=genmem(p.def, p.size)
				pushit
			fi

		when int_opnd then
			if p.value in i32.bounds then		!fits in d32 offset
				ax:=genint(p.value, 4)
				pushit
			fi

		when real_opnd then
			if p.size=8 then
				ax:=genrealmem(p.xvalue)
				pushit
			fi
		esac
	fi

!!need to go via register

	ax:=loadopndr(n, mode)

	if ax.reg>=xr0 then			!float register
		bx:=ax
!		ax:=genreg(getworkireg(), p.size)
		ax:=gwrm(pmode)
		genmc(m_mov, ax, bx)
	fi

pushit:
	genmc(m_push, changeopndsize(ax,8))
	poppcl()
	++mstackdepth

end

!global func loadtoreg(mclopnd ax, int mode, reg)mclopnd=
!!if ax is not a register operand, then load it to given register
!!mode is needed to give type of register (float etc) and size
!!It is assumed that if ax /is/ in a register, that it is the correct one, or doesn't matter
!	mclopnd bx
!
!	if ax.mode=a_reg then			!might already be in reg
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
!
!global func loadtoreg_m(mclopnd ax, int mode, reg)mclopnd=
!!same as loadtoreg but if already in a register, will move it to required one if needed
!	mclopnd bx
!
!	if ax.mode=a_reg then			!already in register
!		if ax.reg=reg then return ax fi			!in correct one
!	fi
!
!!need loading/moving to given reg
!	bx:=genreg(reg, ttsize[mode])
!
!	loadtoreg_common(bx, ax)
!!	genmc(m_mov, bx, ax)
!	bx
!end

!proc loadtoreg_common(mclopnd bx, ax)=
!	if ax.mode=a_imm and ax.valtype=int_val and ax.value=0 then
!		bx:=changeopndsize(bx,4)
!		clearreg(bx)
!!		genmc(m_xorx, bx, bx)
!	
!	else
!		genmc(m_mov, bx, ax)
!	fi
!
!end


global proc pushpcl(pcl pc)=
!Push a inline operand from pcl code to pcs
!addrof is 1 to apply & to name units, creating a memaddr opnd
	int n
	pcl p
	pcsrec ps
!
	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi

	n:=++noperands

	pclopnds[n]:=pc

!	ps.all:=0

	ps.mode:=pc.mode
	ps.reg:=ps.temp:=0
	ps.code:=1
	ps.count:=1

	pclstack[n]:=ps
end

global proc pushpcl_reg(int mode, reg=rnone)=
!Push a new, empty pcs slot located in given register
	int n
	pcsrec ps

	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi

	if reg=rnone then reg:=gwr(mode) fi

	n:=++noperands

	ps.all:=0
	ps.reg:=reg
	ps.count:=1
	ps.mode:=mode
	pclstack[n]:=ps

	regset.[reg]:=1
	pclset.[reg]:=1

end

global proc poppcl=
	int n:=noperands

	if n<=0 then merror("poppcl/underflow") fi

	if pclstack[n].count>1 then
		--pclstack[n].count
		return
	fi

	pclset.[pclstack[n].reg]:=0		!will clear bit 0 if no reg used

	--noperands
end

global proc duplpcl=
!ensure zz is in a register, duplicate into a new register
	int mode:=pclstack[zz].mode

	loadopndr(zz, mode)							!get zz to reg
!	pushpcl_reg(getworkreg(mode), mode)				!create new zz opnd, old is now yy
	pushpcl_reg(mode)							!create new zz opnd, old is now yy

!MCOMM("DUPLOP")
	genmc(m_mov, getopnd(zz, mode), getopnd(yy, mode))	!copy old to new
end

global func gwri(int r=rnone)int =
	if r then return r fi

	to 10 do
		for r in r0..r13 do
			if workset.[r] and regset.[r]=0 then
				regset.[r]:=1
!CPL "USEDI", STRREG(R)
				usedset.[r]:=1
				return r
			fi
		od
		savenextopnd()
	od
	merror("No more work regs")
	0
end

global func gwrx(int r=rnone)int=
	if r then return r fi

	for r in xr4..xr15 do
		if workset.[r] and regset.[r]=0 then
			regset.[r]:=1
			usedset.[r]:=1
			return r
		fi
	od
	merror("No more work xregs")
	0
end

global func gwr(int mode=pmode, reg=rnone)int =
	if stdfloat[mode] then
		gwrx(reg)
	else
		gwri(reg)
	fi
end

global func gwrm(int mode=pmode, reg=rnone)mclopnd =

	if reg=rframe then reg:=rnone fi

	mgenreg(gwr(mode, reg), mode)

!	if stdfloat[mode] then
!		genxreg(gwrx(reg), stdsize[mode])
!	else
!		genreg(gwri(reg), stdsize[mode])
!	fi
end

global proc saveopnd(int n, allregs=1)=
!if operand is in a volatile register, then save it in a temp
!allregs=1 to save both A and B regs (vol/nonval), which can include P regs if
!used as workregs; this is to save pne reg=opnd to a temp to free up a register
!allregs=0 to limit to A regs (possibly some P regs) only; normall for CALLs
!in order to preserve non-vol regs, as call will preserve the rest

!NOTE: operands that are unlikely to be unchanged from their value in
!pclrec, could be revert to unit_loc. Eg. immediates, addresses, or any
!variable that is immutable

	int reg, size
	mclopnd tx
	pcsrec ps:=pclstack[n]

	reg:=ps.reg
	size:=stdsize[ps.mode]

	return unless reg

	if stdint[ps.mode] then
		if allregs or reg not in r3..r9 then
			genmc(m_mov, gentemp(n, size), genreg(reg, size))
		fi
	else
		if allregs or reg in xr0..xr5 then
			genmc(m_mov, gentemp(n, size), genxreg(reg, size))
		fi
	fi
	regset.[reg]:=0
	pclset.[reg]:=0

	ps.reg:=ps.code:=0
	ps.temp:=1
	pclstack[n]:=ps
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
		if pclstack[i].reg and stdint[pclstack[i].mode] then
			saveopnd(i,1)
			return
		fi
	od
end

global proc movetoreg(int newreg)=
!move top of stack (assumed to be in reg) to newreg
!assumes integer reg
	int oldreg, size
	pcsrec ps

	loadopndr(zz, pclstack[zz].mode)

retry:

	ps:=pclstack[zz]
	oldreg:=ps.reg
	size:=stdsize[ps.mode]

	if oldreg=newreg then
		return
	fi

	if stdfloat[ps.mode] then
		if regset.[newreg] then
			MERROR("MOVE TO REG: XREG IN USE")
		fi
	elsif regset.[newreg] then
		for i to noperands do
			if stdint[ps.mode] and pclstack[i].reg=newreg then
				swapopnds(i,zz)
				genmc(m_xchg, genreg(oldreg), genreg(newreg))
				retry
			fi
		od
	fi

	genmc(m_mov, genreg(newreg, size), genreg(oldreg, size))

	pclstack[zz].reg:=newreg

!CPL "MOVETOREG"

	regset.[oldreg]:=0
	regset.[newreg]:=1
end

global proc swapopnds(int m,n)=
!exchange pcl stack operands
	pclrec t

	swap(pclopnds[m], pclopnds[n])
	swap(pclstack[m], pclstack[n])
end

global proc setnewzz(int mode, reg)=
!some value has been into into given register
!create replace pcl[zz] with that new operand
!assume pclcount was 1 and stays at 1
	pcsrec ps

	ps:=pclstack[zz]

	pclset.[ps.reg]:=0

	ps.reg:=reg
	ps.temp:=ps.code:=0
	ps.mode:=mode
	pclset.[reg]:=1

	pclstack[zz]:=ps
end

global func stropndstack(int indent=0)ichar=
	static [512]char str
	[512]char str2
	ichar s:=str, t
	pcl p
	pcsrec ps

	if indent then
		fprint @s, "="*20 + "#:(", NOPERANDS
	else
		fprint @s, "#:(", NOPERANDS
	fi

STRCAT(S, "@")

	for i to noperands do
		p:=pclopnds[i]
		ps:=pclstack[i]

		strcat(s, (noperands-i+1|"Z:", "Y:", "X:", "W:"|""))

		if ps.reg then
			strcat(s, regnames[ps.reg])
!			if p.def then strcat(s, "*") fi
!			if p and p.opndtype=mem_opnd then
!				strcat(s, "(")
!				strcat(s, p.def.name)
!!				strcat(s, strint(int(p)))
!				strcat(s, ")")
!			fi

		elsif ps.temp then
			strcat(s, "T")
			strcat(s, strint(i))

		elsecase p.opndtype
		when int_opnd then
			strcat(s, strint(p.value))

		when mem_opnd then
			strcat(s, p.def.name)
!
		else
			strcat(s, "(")
!int fs:=fshortnames
!fshortnames:=1
!			strcat(s, mstropnd(pclopnd[i]))
!fshortnames:=fs
			strcat(s, ")")
		fi

		if ps.count>1 then strcat(s, "@") fi
		strcat(s, "<")
		strcat(s, stdnames[ps.mode])
		strcat(s, ">")

		if i<noperands then strcat(s,", ") fi
	od
	strcat(s,") ")

	ipadstr(str, 50)

STRCAT(STR, STRINT(PCLSET,"H"))
STRCAT(STR, " ")


	strcat(s,"WR:(")
!	for r:=r0 to r9 when workregs[r] do
	for r:=r0 to r9  do
		strcat(s,(regset.[r]|"1 "|"0 "))
	od
	strcat(s,") ")

	strcat(s,"XWR:(")
	for r:=xr4 to xr15 do
		strcat(s,(regset.[r]|"1 "|"0 "))
	od

	strcat(s,") hwstack:")
	strcat(s,strint(mstackdepth))
	strcat(s," noperands:")
	strcat(s,strint(noperands))
!	strcat(s," ncalldepth:")
!	strcat(s,strint(ncalldepth))
	return s
end

global proc showopndstack=
	mcomment(stropndstack(1))
end

!global func loadopndp(int n, mode)mclopnd =
!!turn given pcl opnd into a pointer. That is, load it into a reg if not
!!already there, and turn it into an IREG operand
!!This will only do simple pointers: one register, no index, and a zero offset
!!(Call can add an offset separately)
!	pclopnd p := &pstack[n]
!	mclopnd ax
!
!	case p.optype
!	when regptr_opnd then
!		gerror("makeptr?")
!	else
!		ax:=loadopndr(n, taddr)
!	esac
!
!!so, in a register
!	p.optype:=regptr_opnd
!!	p.reg2:=rnone
!!	p.offset:=0
!
!	mgenireg(p.reg, mode)
!end

global func loadopndpx(int scale=1, offset=0, mode)mclopnd =
!turn top two operands yy/zz, which are pointer/scaled index, into
!a single address mode represented by one new operands, zz'
!Also return that new address mode

!	pclopnd pa := &pstack[yy]
!	pclopnd pi := &pstack[zz]
!	mclopnd ax
!
!	case p.optype
!	when regptr_opnd then
!		gerror("makeptr?")
!	else
!		ax:=loadopndr(n, mode)
!	esac
!
!!so, in a register
!	p.optype:=regptr_opnd
!	p.reg2:=rnone
!	p.offset:=0
GERROR("LOADOPNDX NOT READY")
!
!	mgenimem(p.reg, pmode)
	nil
end

global proc storeopnd(pcl p, int n)=
!store opnd n (usually zz) to p which is always mem_opnd
	symbol d		:= p.def
	mclopnd ax, bx	:= loadopndr(n, p.mode)

	if p.mode=tblock then
		ax:=genreg(gwri())
		genmc(m_lea, ax, genmem(d))
		ax:=genireg(ax.reg)

		bx:=genireg(bx.reg)
		copyblock(ax, bx, p.size)

	else
		bx:=changeopndsize(bx, p.size)
		if d.reg then
			ax:=mgenreg(d.reg, p.mode)
		else
			ax:=genmem(d, p.size)
		fi
		genmc(m_mov, ax, bx)
	fi

end

global proc freeworkregs=
!copy of code in convert pcl
	regset:=pclset

!	for i to noperands do
!		if pclstack[i].reg then
!			regset.[pclstack[i].reg]:=1
!		fi
!	od
end

global func getopnd_ind(int n=noperands, mode=pmode, mem=0)mclopnd=
!Get access mode to operand which is to be used as a pointer elsewhere
!So it needs first to in a register, if not already
!mem=1 if allowed to return memory address mode, like [abc] or [dfp+abc]
	pcl p:=pclopnds[n]
	pcsrec ps:=pclstack[n]
	symbol d

!optimise for memaddr
	if mem and ps.code and p.opndtype=memaddr_opnd then
		d:=p.def
		unless d.nameid=paramid and ttbasetype[d.mode]=tblock then
			return mgenmem(d, mode)
		end
	fi

	unless ps.reg then
		loadopndr(n, tu64)			!ps may be out of date
	end

	return mgenireg(pclstack[n].reg, mode)
end

global func makeopndind(mclopnd a, int mode=pmode)mclopnd=
	mclopnd b

	if a.mode<>a_reg then
		merror("makeopndind")
	fi

	return mgenireg(a.reg, mode)
end

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2
!Note that swapondregs is best assumed to invalidate all existing mclopnds that
!refer to registers, as stuff if moved aound
!Also invalided are workregs that might use reg2, even if no mclopnd exists for it

	if not ispint(pclstack[zz].mode) then merror("SOR1") fi

!assume int regs

	int reg1:=pclstack[zz].reg

	if reg1=reg2 then return fi

	for i:=noperands-1 downto 1 do
		if pclstack[i].reg=reg2 then
			swap(pclstack[zz].reg, pclstack[i].reg)
			return
		fi
	else
!pcl op not found that occupies reg2, so it is assumed to be a work register
!that is no longer needed. If it /is/ needed

		regset.[reg1]:=0				!make available (although done for next pcl op anyway)
		pclstack[zz].reg:=reg2
		pclset.[reg2]:=1
!		merror("swapopndregs/reg not found")
	od
end

global func getopnd_ind_simp(int n=noperands, mode=ti64)mclopnd=
!version of getopnd_ind which always returns [reg]

	if not pclstack[n].reg then
		loadopndr(n, tu64)
	fi

	return mgenireg(pclstack[n].reg, mode)
end

global func getsharereg(int mode, mclopnd ax)int=
!if ax includes reg/regix, then try and use them
!return 0 if not reg available or not possibe
	byte reg:=ax.reg, regix:=ax.regix

	if ispfloat(mode) then return 0 fi

	if reg and (workset.[reg] or reg in r10..r13) then			!not a regvar
		return reg
	elsif regix and (workset.[regix] or reg in r10..r13) then
		return regix
	fi

	return 0
end

=== mcl_block.m 0 0 24/29 ===
!global const fshowinlinepcl=1
global const fshowinlinepcl=0

!global const fshowopndstack=1
global const fshowopndstack=0

!global const docalltrace=1
!global const docalltrace=0

GLOBAL INT DEBUG

!global int frameoffset, paramoffset
global int framebytes

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global [pclnames.bounds]ref proc(pcl) px_handlertable

global macro ispint(m) = stdint[m]
global macro ispfloat(m) = stdfloat[m]
global macro ispwide(m) = stdwide[m]

global proc convertpcl(pcl p)=
!	pcl q

!	PRINTLN "    CONV",PCLNAMES[P.OPCODE]
!CPL STROPNDSTACK()

	doshowpcl(p) when fshowpcl

!CPL STROPNDSTACK()

	pmode:=p.mode
!*!	mmpos:=p.pos
!*!	ppseqno:=p.seqno

	px_handlertable[p.opcode]^(p)

	showopndstack() when fshowopndstack and currpcl.opcode not in [klabel, kcomment]

!clear all work regs including ones holding active temps
	regset:=pclset
end

proc px_nop*(pcl p) =
!nop is sometimes used to replace deleted instructions like useless jumps

!	unimpl(p)
end

proc px_load*(pcl p) =
	pushpcl(p)
end

proc px_store*(pcl p) =
	storeopnd(p, zz)
	poppcl()
end

proc px_iload*(pcl p) =
! Z' := Z^
	mclopnd ax, px
	pcl nextpcl
	int reg

	if pmode<>tblock then

!		if pclloc[zz]=regvar_loc then
!			px:=mgenireg(pclreg[zz], pmode)
!		else
			px:=getopnd_ind(zz)
!		fi

		nextpcl:=currpcl.next

		if nextpcl.opcode=kwiden then
!			ax:=getworkreg_rm(getsharereg(px, nextpcl.mode), nextpcl.mode)
			ax:=gwrm(nextpcl.mode, getsharereg(nextpcl.mode, px))

			genmc(ploadop[nextpcl.mode2], ax, px)
			setnewzz(nextpcl.mode, ax.reg)
			currpcl:=nextpcl
		else
!			ax:=mgenreg(getworkregc(getsharereg(px, pmode), pmode), pmode)
			ax:=gwrm(pmode, getsharereg(pmode, px))
			genmc(m_mov, ax, px)
			setnewzz(pmode, ax.reg)
		fi

	else

		px:=getopnd_ind_simp(zz)

		ax:=gwrm(tu64, px.reg)
		dolea(ax, px)
	fi

end

proc px_iloadx*(pcl p) =
! Z' := (Y + Z*s + d)^
	pcl z, nextpcl
	mclopnd ax, bx, px, fx

	px:=do_addrmode(p)

	if pmode=tblock then
		ax:=gwrm(tu64, px.reg)
!		ax:=genreg(getworkregc(tu64, px.reg))
		dolea(ax, px)
		poppcl()
		setnewzz(tu64, ax.reg)

	else
		nextpcl:=currpcl.next

		if nextpcl.opcode=kwiden then
!			ax:=getworkreg_rm(getsharereg(px, nextpcl.mode), nextpcl.mode)
			ax:=gwrm(nextpcl.mode, getsharereg(nextpcl.mode, px))

			genmc(ploadop[nextpcl.mode2], ax, px)
			poppcl()
			setnewzz(nextpcl.mode, ax.reg)
			currpcl:=nextpcl
		else

!			ax:=getworkreg_rm(getsharereg(px, pmode), pmode)
			ax:=gwrm(pmode, getsharereg(pmode, px))

			genmc(m_mov, ax, px)
			poppcl()
			setnewzz(pmode, ax.reg)
		fi

	fi
end

proc px_istore*(pcl p) =
! Y^ := Z
	mclopnd bx, px
	int reg

	bx:=loadopndr(yy, pmode)				!rhs to store into lhs

	if reg:=pclstack[zz].reg then
		px:=mgenireg(reg, pmode)
	else
		px:=getopnd_ind(zz, pmode)
	fi

	if pmode=tblock then
		px:=makesimpleaddr(px)
		bx:=makeopndind(bx, tu64)

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

	cx:=loadopndr(xx, pmode)			!rhs
	px:=do_addrmode(p)

	if pmode=tblock then
		px:=makesimpleaddr(px)
		cx:=makeopndind(cx, tu64)
		copyblock(px, cx, p.size)

	else
		genmc(m_mov, px, cx)

	fi

	poppcl()
	poppcl()
	poppcl()
end

proc px_storem*(pcl p) =
	unimpl(p)
end

proc px_dupl*(pcl p) =
	unimpl(p)
end

proc px_double*(pcl p) =
	if ncalldepth then
		duplpcl()
	else
		++pclstack[noperands].count
	fi
end

proc px_swapstk*(pcl p) =
! (Z', Y') := (Z, Y)
!	swapopnds(yy, zz)
	swapopnds(noperands-p.x+1, noperands-p.y+1)
end

proc px_unload*(pcl p) =
	poppcl()
end

proc px_opnd*(pcl p) =
	unimpl(p)
end

proc px_type*(pcl p) =
	unimpl(p)
end

proc px_loadbit*(pcl p) =
! Z' := Y.[Z]
	mclopnd ax
	pcl z
	int i, m

	if z:=isimmint(zz) then
		i:=z.value
		m:=(i in 0..31|tu32|tu64)

		ax:=loadopndw(yy, m)
		if i then
			genmc(m_shr, ax, mgenint(i, m))

			goto skip when i=63

		fi
	else
		ax:=loadopndw(yy, pmode)
		genmc(m_push, mgenreg(r10)) when r10used
		genmc(m_shr, ax, loadparam(zz, tu8, r10))
		genmc(m_pop, mgenreg(r10)) when r10used
	fi

	genmc(m_and, changeopndsize(ax, 4), mgenint(1, tu32))

skip:
	poppcl()
end

proc px_loadbf*(pcl p) =
! Z' := X.[Y..Z]
	pcl y, z

	y:=isimmint(yy)
	z:=isimmint(zz)

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

proc px_callp*(pcl p) =
	int nargs, nregargs, slots, isptr:=0, shadow:=0

	int blockret:=callblockret[ncalldepth]

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
		genmc(m_call, loadopndr(zz, tu64))
		poppcl()
	else
		genmc(m_call, genmemaddr(p.def))
	fi

	to nregargs-BLOCKRET do
!CPL "CALL/POP", =NOPERANDS, =PCLSET:"H"
!CPL STROPNDSTACK()
		poppcl()
!CPL STROPNDSTACK()
	od

	if slots then
		popslots(slots)
	fi

	if pmode then
		do_getretvalue(p)
	fi

	--ncalldepth
end

proc px_retproc*(pcl p) =
	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mcomment("---")				!injection of entry code goes wrong otherwise
	fi

!	do_proccode_b()
!	do_proccode_c()
end

proc px_retfn*(pcl p) =
	mclopnd ax, bx


	if pmode=tblock then
		bx:=genireg(r0)								!r0 points to local block value
		regset.[r0]:=1
		ax:=gwrm()
		genmc(m_mov, ax, genmem(blockretname))
		ax:=genireg(ax.reg)
		copyblock(ax, bx, p.size)
		genmc(m_mov, mgenreg(r0, tu64), mgenmem(blockretname))
	fi

	px_retproc(p)
end

!proc px_callf*(pcl p) =
!	unimpl(p)
!end
!
proc px_jump*(pcl p) =
	int labno:=p.labelno
	pcl q:=p.next

	while q.opcode=kcomment do q:=q.next od
	case q.opcode
	when klabel then
		if q.labelno=labno then return fi
		q:=q.next
		if q.opcode=klabel and q.labelno=labno then return fi
	when kjump then
		q.opcode:=knop
	esac

	genmc(m_jmp, genlabel(labno))
end

proc px_ijump*(pcl p) =
	unimpl(p)
end

proc px_jumpcc*(pcl p) =
! goto L when Y c Z; p=1: Z':=Y (b=0/1)
	int mcond
	mclopnd ax, bx, lx
	pcl z

	mcond:=ucondcodes[p.condcode]
	lx:=genlabel(p.labelno)

	if pmode=tblock then
MERROR("JUMPCC/BLOCK")
!		addimm(p.size)
!		swapopnds(1, 3)
!		domaths(nil, "memcmp*", 3)
!		genmc(m_cmp, mgenreg(r0, tp32), mgenint(0))
!		genmc_cond(m_jmpcc, mcond, lx)

	else

		ax:=loadopndr(yy)

		if ispint(pmode) then
			if (z:=isimmint(zz)) and z.value=0 and p.condcode in [eq_cc, ne_cc] then
				genmc(m_test, ax, ax)
			else
				bx:=getopnd(zz)
				if stdsigned[pmode] then
					mcond:=scondcodes[p.condcode]
				fi
				genmc(m_cmp, ax, bx)
			fi
		else
			bx:=getopnd(zz)
MCOMM("A")
			genmc(m_comiss, ax, bx)
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
	do_jumptruefalse(p, z_cond)
end

proc px_jumpret*(pcl p) =
! goto L, common return point; deal with any ret value on stack

	if pmode=tvoid or noperands=0 then
		merror("Jumpret/no-arg")
	fi

	loadparam(zz, pmode, (stdfloat[pmode]|xr0|r0))
	poppcl()

	px_jump(p)
end

proc px_jumpretm*(pcl p) =
	unimpl(p)
end

proc px_setcc*(pcl p) =
! Z' := Y cc Z
	int cond
	mclopnd ax, bx, cx

	ax:=loadopndr(yy, pmode)
	bx:=getopnd(zz, pmode)
	cond:=ucondcodes[p.condcode]

	if pmode=tblock then
		merror("setcc/block")

	elsif ispint(pmode) then
		if stdsigned[pmode] then
			cond:=scondcodes[p.condcode]
		fi
		genmc(m_cmp, ax, bx)
		cx:=changeopndsize(ax, 1)

	else
MCOMM("B")
		genmc(m_comiss, ax, bx)

		cx:=gwrm(tu8)
		setnewzz(ti64, cx.reg)
		swapopnds(yy, zz)
	fi

	genmc_cond(m_setcc, cond, cx)
	genmc(m_movzx, changeopndsize(cx, 4), cx)

	poppcl()
end

proc px_stop*(pcl p) =
	loadparam(zz, tu64, r10)
	genmc(m_call, genextname("exit"))

	localshadow:=1
	poppcl()
end

proc px_to*(pcl p) =
	pcl q
	mclopnd ax

	q:=currpcl:=p.next

	ax:=mgenmem(q.def)
	genmc(m_dec, ax)
	genmc_cond(m_jmpcc, nz_cond, genlabel(p.labelno))
end

proc px_forup*(pcl p) =
! B+:=n; goto L when B<=C
	do_for(p, m_inc, m_add, le_cond)
end

proc px_fordown*(pcl p) =
! B-:=n; goto L when B>=C
	do_for(p, m_dec, m_sub, ge_cond)
end

proc px_iswap*(pcl p) =
	mclopnd ax, bx

	mclopnd px:=getopnd_ind(yy)
	mclopnd qx:=getopnd_ind(zz)

	ax:=gwrm()
	bx:=gwrm()

	if pmode<>tblock then
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

proc px_switch*(pcl p) =
! L=jumptab; B=elselab; x/y=min/max values
	int minlab, maxlab, jumplab, elselab, reg
	mclopnd ax, bx, ax2

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p.next
	elselab:=currpcl.labelno

	ax:=loadopndr(zz)
	if p.size<8 then
		genmc(m_movsx, ax2:=changeopndsize(ax, 8), ax)
		ax:=ax2
	fi

	if minlab<>0 then
		genmc(m_sub, ax, genint(minlab))
	fi

	genmc(m_cmp, ax, genint(maxlab-minlab+1))
	genmc_cond(m_jmpcc, geu_cond, genlabel(elselab))

	if highmem=2 then
		bx:=gwrm(tu64)

		genmc(m_lea, bx, genlabelmem(jumplab))

		genmc(m_jmp, genindex(ireg:ax.reg, areg:reg, scale:8))
	else
		genmc(m_jmp, genindex(ireg:ax.reg, scale:8, labno:jumplab))

	fi

	poppcl()

!	setsegment('I')
end

proc px_switchu*(pcl p) =
! L=jumptab; B=elselab; x/y=min/max values
	int minlab, maxlab, jumplab, reg
	mclopnd ax, bx

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno

	ax:=loadopndr(zz, pmode)

	if highmem=2 then
		bx:=gwrm()

!		genmc(m_mov, bx, mgenlabel(jumplab))
		genmc(m_lea, bx, genlabelmem(jumplab))

		genmc(m_jmp, genindex(ireg:ax.reg, areg:reg, scale:8, offset:-minlab*8))
	else
		genmc(m_jmp, genindex(ireg:ax.reg, scale:8, labno:jumplab, offset:-minlab*8))
	fi

	poppcl()
end

proc px_swlabel*(pcl p) =
! jumptable entry
	genmc(m_dq, genlabel(p.labelno))
end

!proc px_endsw*(pcl p) =
!! Mark end of switch jumptable
!	setsegment('C')
!end

proc px_clear*(pcl p) =
	mclopnd ax

	ax:=getopnd_ind_simp(zz, tu64)

	clearblock(ax, p.size)
	poppcl()
end

proc px_add*(pcl p) =
	mclopnd ax, bx
	pcl z

	ax:=loadopndw(yy)

	if stdint[p.mode] then
		if (z:=isimmint(zz)) and z.value=1 then
			genmc(m_inc, ax)
		else
			bx:=getopnd(zz)
			genmc(m_add, ax, bx)
		fi
	else
		bx:=getopnd(zz)
		genmc(m_addss, ax, bx)
	fi

	poppcl()
end

proc px_sub*(pcl p) =
	mclopnd ax, bx
	pcl z

	ax:=loadopndw(yy)

	if stdint[pmode] then
		if (z:=isimmint(zz)) and z.value=1 then
			genmc(m_dec, ax)
		else
			bx:=getopnd(zz)
			genmc(m_sub, ax, bx)
		fi
	else
		bx:=getopnd(zz)
		genmc(m_subss, ax, bx)
	fi

	poppcl()
end

proc px_mul*(pcl p) =
! Z' := Y * Z
	mclopnd ax, bx
	pcl z

	ax:=loadopndw(yy)

	if stdint[pmode] then
		if z:=isimmint(zz) then
			mulimm(ax, z.value)

		else

!			bx:=getopnd(zz)
			bx:=loadopndr(zz)
			genmc(m_imul2, ax, bx)
		fi

	else
		bx:=getopnd(zz)
		genmc(m_mulss, ax, bx)
	fi

	poppcl()
end

proc px_div*(pcl p) =
	mclopnd ax, bx

	ax:=loadopndw(yy)
	bx:=getopnd(zz)
	genmc(m_divss, ax, bx)
	poppcl()
end

proc px_idiv*(pcl p) =
! Z' := Y % Z
	do_divrem(p, issigned:stdsigned[pmode], isdiv:1)
end

proc px_irem*(pcl p) =
! Z' := Y rem Z
	do_divrem(p, issigned:stdsigned[pmode], isdiv:0)
end

proc px_idivrem*(pcl p) =
! Z' := divrem(Y, Z)
	do_divrem(p, issigned:stdsigned[pmode], isdiv:2)
end

proc px_bitand*(pcl p) =
! Z' := Y iand Z
	do_bitwise(p, m_and)
end

proc px_bitor*(pcl p) =
! Z' := Y ior Z
	do_bitwise(p, m_or)
end

proc px_bitxor*(pcl p) =
! Z' := Y ixor Z
	do_bitwise(p, m_xor)
end

proc px_shl*(pcl p) =
! Z' := Y << Z
	do_shift(p, m_shl)
end

proc px_shr*(pcl p) =
! Z' := Y >> Z
	do_shift(p, (stdsigned[pmode]|m_sar|m_shr))
end

proc px_min*(pcl p) =
! Z' := min(Y, Z)
	if ispint(pmode) then
		do_max_int((stdsigned[pmode]|gt_cond|gtu_cond))
	else
		do_max_float(m_minss)
	fi
end

proc px_max*(pcl p) =
! Z' := max(Y, Z)
	if ispint(pmode) then
		do_max_int((stdsigned[pmode]|lt_cond|ltu_cond))
	else
		do_max_float(m_maxss)
	fi
end

proc px_addpx*(pcl p) =
! Z' := Y + Z*s + d
	mclopnd ax, cx

!P.MODE:=PMODE:=TPU64

	cx:=do_addrmode(p)

	ax:=gwrm(tu64, cx.reg)

	dolea(ax, cx)
	poppcl()

	setnewzz(tu64, ax.reg)
end

proc px_subpx*(pcl p) =
! Z' := Y - Z*s + s
	int scale, extra, offset
	mclopnd ax, bx
	pcl z

	scale:=p.scale
	extra:=p.extra

	ax:=loadopndw(yy, tu64)

	if z:=isimmint(zz) then
		genmc(m_sub, ax, mgenint(z.value*scale+extra))
	else
		bx:=loadopndr(zz, tu64)
		scale:=scaleindex(bx, scale)
		if scale>1 then
			mulimm(bx, scale)
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

proc px_subp*(pcl p) =
! Z' := (Y - Z)/s
	mclopnd ax, bx
	int n

	ax:=loadopndw(yy)
	bx:=getopnd(zz)
	genmc(m_sub, ax, bx)

	if p.scale>1 then
		n:=ispoweroftwo(p.scale)
		if n then
			genmc(m_shr, ax, genint(n))
		else
			CPL P.SCALE
			MERROR("SUB/REF NOT POWER OF xx")
		fi
	fi

	poppcl()
end

proc px_neg*(pcl p) =
! Z' := -Z
	mclopnd ax

	ax:=loadopndw(zz)

	if ispint(pmode) then
		genmc(m_neg, ax)
	else
		do_negreal(ax, pmode)
	fi
end

proc px_abs*(pcl p) =
! Z' := abs Z
	mclopnd ax, lx

	ax:=loadopndw(zz)

	if ispint(pmode) then
		genmc(m_cmp, ax, mgenint(0, pmode))

		genmc_cond(m_jmpcc, ge_cond, lx:=genlabel(++mlabelno))
		genmc(m_neg, ax)
		genmc(m_label, lx)

	else
		do_absreal(ax, pmode)
	fi
end

proc px_bitnot*(pcl p) =
	mclopnd ax
	ax:=loadopndr(zz)
	genmc(m_not, ax)
end

proc px_not*(pcl p) =
	unimpl(p)
end

proc px_toboolt*(pcl p) =
! Z' := istrue Z
	mclopnd ax, bx, cx
	byte pmode2:=p.mode2

	ax:=loadopndw(zz, pmode2)

	if ispfloat(pmode2) then
		bx:=gwrm(pmode2)
		cx:=gwrm(tu8)
		genmc(m_xorps, bx, bx)
MCOMM("C")
		genmc(m_comiss, ax, bx)

		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), cx)
		genmc(m_movzx, changeopndsize(cx, 4), cx)		!4 works for u32/u64
		setnewzz(pmode, cx.reg)

	else
		genmc(m_test, ax, ax)
		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), bx:=changeopndsize(ax, 1))
		genmc(m_movzx, changeopndsize(ax, 4), bx)
		pclstack[xx].mode:=pmode
	fi
end

proc px_sqr*(pcl p) =
	mclopnd ax
	int opc

	ax:=loadopndw(zz)

	if ispint(pmode) then
		opc:=m_imul2
	else
		opc:=m_mulss
	fi
	genmc(opc, ax, ax)
end

proc px_sqrt*(pcl p) =
	mclopnd ax

	ax:=loadopndr(zz)
	genmc(m_sqrtss, ax, ax)
end

proc px_maths*(pcl p) =
	do_callrts(p, mathsnames[p.mathsop], 1)
end

proc px_maths2*(pcl p) =
	swapopnds(yy,zz)
	do_callrts(p, mathsnames[p.mathsop], 2)
end

proc px_sign*(pcl p) =
	unimpl(p)
end

proc px_power*(pcl p) =
! Z' := Y ** Z
	mclopnd ax, bx
	symbol d

	if ispint(pmode) then
		d:=findhostfn(kpower)
		unless d then merror("$power?") end
		swapopnds(yy, zz)
		do_callrts(p, nil, 2, d)
	else
		swapopnds(yy, zz)
		do_callrts(p, "pow", 2)
	fi
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
	mclopnd ax, bx, cx
	pcl x

	if ispfloat(pmode) then
		do_binto_float(p, m_mulss)
		return
	fi

	if p.size=1 then merror("multo/byte") fi

	pushpcl_reg(ti64)

!operands are now Y^ *:= X with Z used as working value

!xx yy zz = addr rhs workreg
	ax:=getopnd_ind(yy)
	bx:=getopnd(xx)
	cx:=getopnd(zz)

	genmc(m_mov, cx, ax)

	if x:=isimmint(xx) then
		mulimm(cx, x.value)
	else
		genmc(m_imul2, cx, bx)
	fi
	genmc(m_mov, ax, cx)

	poppcl()
	poppcl()
	poppcl()
end

proc px_bitandto*(pcl p) =
! Z^ iand:= Y
	do_binto(p, m_and, 0)
end

proc px_bitorto*(pcl p) =
! Z^ ior:= Y
	do_binto(p, m_or, 0)
end

proc px_bitxorto*(pcl p) =
! Z^ ixor:= Y
	do_binto(p, m_xor, 0)
end

proc px_shlto*(pcl p) =
! Z^ <<:= Y
	do_shiftnto(p, m_shl)
end

proc px_shrto*(pcl p) =
! Z^ >>:= Y
	do_shiftnto(p, (stdsigned[pmode]|m_sar|m_shr))
end

proc px_minto*(pcl p) =
! Z^ min:= Y
	if ispint(pmode) then
		do_maxto_int((stdsigned[pmode]|le_cond|leu_cond), pmode)
	else
		do_maxto_real(leu_cond, pmode)
	fi
end

proc px_maxto*(pcl p) =
! Z^ max:= Y
	if ispint(pmode) then
		do_maxto_int((stdsigned[pmode]|ge_cond|geu_cond), pmode)
	else
		do_maxto_real(geu_cond, pmode)
	fi
end

proc px_addpxto*(pcl p) =
! Z^ +:= Y
	mclopnd ax, bx
	pcl y
!
	ax:=getopnd_ind(zz, pmode)

	if y:=isimmint(yy) then
		genmc(m_add, ax, genint(y.value*p.scale))
	else
		bx:=loadopndw(yy, pmode)
		mulimm(bx, p.scale)
		genmc(m_add, ax, bx)
	fi

	poppcl()
	poppcl()
end

proc px_subpxto*(pcl p) =
! Z^ -:= Y
	mclopnd ax, bx
	pcl y

	ax:=getopnd_ind(zz)

	if y:=isimmint(yy) then
!		genmc(m_sub, ax, mgenint(z.value*p.scale+p.extra))
		genmc(m_sub, ax, mgenint(y.value*p.scale))
	else
		bx:=loadopndw(yy)
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

proc px_negto*(pcl p) =
	unimpl(p)
end

proc px_absto*(pcl p) =
	unimpl(p)
end

proc px_bitnotto*(pcl p) =
	unimpl(p)
end

proc px_notto*(pcl p) =
	unimpl(p)
end

proc px_toboolto*(pcl p) =
	unimpl(p)
end

proc px_fix*(pcl p) =
! Z' := cast(Z, t) Real u to int t
	mclopnd fx, ax
!
	fx:=loadopndr(zz, p.mode2)
	pushpcl_reg(pmode)

	ax:=getopnd(zz, stdmin[pmode])
	genmc(m_cvttss2si+ispwide(p.mode2), ax, fx)

	swapopnds(yy, zz)
	poppcl()

	setnewzz(pmode, ax.reg)
end

proc px_float*(pcl p) =
! Z' := cast(Z, t) Int u to real t
	mclopnd ax, fx
	int lab, lab2
	byte pmode2:=p.mode2

	ax:=loadopndr(zz, pmode2)

	if stdsize[pmode2]<4 then merror("float/short") fi

	if stdsigned[pmode2] then
		pushpcl_reg(pmode)
		fx:=getopnd(zz)

		genmc(m_cvtsi2ss+ispwide(pmode), fx, ax)
		swapopnds(yy, zz)

	elsif pmode2=tu64 then								!u64 to r32/r64
		pushpcl_reg(tr64)								!convert to r64 in all cases

		fx:=getopnd(zz, tr64)

		lab:=createfwdlabel()
		lab2:=createfwdlabel()

		genmc(m_cmp, ax, genint(0, ax.size))					!range of +ve i64?
		genmc_cond(m_jmpcc, lt_cond, genlabel(lab))
		genmc(m_cvtsi2sd, fx, ax)						!number is +ve i64
		genmc(m_jmp, genlabel(lab2))

		definefwdlabel(lab)

		if not labmask63 then
			labmask63:=++mlabelno
			laboffset64:=++mlabelno
		fi
		genmc(m_and, ax, genlabelmem(labmask63))		!clear top bit of u64 (subtract 2**63)
		genmc(m_cvtsi2sd, fx, ax)						!now in +ve i64 range
		genmc(m_addsd, fx, genlabelmem(laboffset64))	!add back 2**63 as float

		mdefinefwdlabel(lab2)							!done conv to r64
reduce:
		if pmode=tr32 then								!for r64, reduce down
			genmc(m_cvtsd2ss, changeopndsize(fx, 4), fx)
			pclstack[zz].mode:=tr32
		fi

		swapopnds(yy, zz)								!bring old int value to top
	else												!u32 to r32/r64
		pushpcl_reg(tr64)								!convert to r64 in all cases

		fx:=getopnd(zz, tr64)
		ax:=changeopndsize(ax, 8)						!eg A0 to D0

		genmc(m_cvtsi2sd, fx, ax)						!u64 (always in range) to r64

		goto reduce

	fi

	poppcl()
end

proc px_truncate*(pcl p) =
! Z' := cast(Z, u) Mask to width of u, but type is widened to t
	mclopnd ax
	byte pmode2:=p.mode2

!	if p.size<8 then merror("trunc32") FI

	ax:=loadopndw(zz, pmode2)
	if p.size<>stdsize[pmode2] then
		genmc(ploadop[pmode2], changeopndsize(ax, p.size), ax)
	fi
end

proc px_typepun*(pcl p) =
! Z' := t(u!(Z^))
	mclopnd ax, bx

	bx:=loadopndw(zz, p.mode2)
	ax:=gwrm()
	genmc(m_mov, ax, changeopndsize(bx, ax.size))

	setnewzz(pmode, ax.reg)
end

proc px_widen*(pcl p) =
! Z' := cast(Z, t) Mask to width of u, but type is widened to t
	mclopnd ax, bx

	if pmode=tu64 and p.mode2=tu32 then
		ax:=loadopndr(zz, tu32)
		if mccodex.opcode<>m_mov then
			genmc(m_mov, ax, ax)			!sets upper half to zero, just in case
		fi
	else
		bx:=getopnd(zz, p.mode2)
		ax:=gwrm()
		genmc((stdsigned[p.mode2]|m_movsx|m_movzx), ax, bx)
		setnewzz(pmode, ax.reg)
	fi

end

proc px_fwiden*(pcl p) =
	mclopnd fx
	fx:=loadopndw(zz, p.mode2)
	genmc(m_cvtss2sd, changeopndsize(fx, 8), fx)
	pclstack[zz].mode:=tr64
end

proc px_fnarrow*(pcl p) =
! Z' := cast(Z, t) r64 to r32
	mclopnd fx
	fx:=loadopndw(zz, p.mode2)
	genmc(m_cvtsd2ss, changeopndsize(fx, 4), fx)
	pclstack[zz].mode:=tr32
end

proc px_startmx*(pcl p) =
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

!RESET PCLSET
	PCLSET:=0
	FOR I TO NOPERANDS DO
		IF PCLSTACK[I].REG THEN
			PCLSET.[PCLSTACK[I].REG]:=1
		FI
	OD
MCOMM("AFTER RESET/ENDMX")
MCOMM(STROPNDSTACK())

end

proc px_initdswx*(pcl p) =
	unimpl(p)
end

proc px_label*(pcl p) =
	genmc(m_label, genlabel(p.labelno))
end

proc px_labeldef*(pcl p) =
	[100]char str
	strcpy(str, p.def.name)
	strcat(str, ":")
	mcomment(str)
end

proc px_setcall*(pcl p) =
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

	callblockret[ncalldepth]:=pmode=tblock
	callblocksize[ncalldepth]:=p.size

	if callalign[ncalldepth] then
		pushslots(1)
	fi
end

proc px_setarg*(pcl p) =
	int n

	n:=p.x+callblockret[ncalldepth]

	if n>4 then
		if pmode=tblock then
			copyblockarg(nil, p.size, n)
		fi
		pushopnd(zz, pmode)
	elsif pmode=tblock then			!need to record its size
		callargsize[ncalldepth, n]:=p.size
	fi
end

proc px_loadall*(pcl p) =
	unimpl(p)
end

proc px_eval*(pcl p) =
	loadopndr(zz)
	poppcl()
end

proc px_comment*(pcl p) =
	mcomment(p.svalue)
!	unimpl(p)
end
=== mcl_blockaux.m 0 0 25/29 ===

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
		genmc(m_lea, ax, genindex(areg: ax.reg, ireg:ax.reg, scale:m-1))
	else						!mul needed anyway; forget the shift
		if shifts then
			mccodex.opcode:=m_imul2
			mccodex.b:=genint(n)
		else
			genmc(m_imul2, ax, genint(n))
		fi
	esac

end

global proc copyblockarg(mclopnd px, int size, ARGNO)=
!px refers to a block in a parameter register
!if px is nil, then called for block in zz that will be pushed

!copy the block to a block temp, and change px's register to
!refer to that new block

	symbol dblock
	mclopnd ax, bx, axi, bxi

	IF PX=NIL THEN
		println "High block arg not copied in", currfunc.name, ,"()"
		return
	FI

MERROR("COPY BLOCK ARG")

!	dblock:=newblocktemp(size)
!	dblock.used:=1
!
!
!	if px then
!		bx:=getworkregm(tref)			!copy of px
!		genmc(m_mov, bx, px)
!	else
!		bx:=loadopnd(zz, tblock)
!	fi
!	ax:=getworkregm(tref)			!refer to temp block
!
!	genmc(m_lea, ax, mgenmem(dblock))
!
!	copyblock(mgenireg(ax.reg), mgenireg(bx.reg), size)
!
!	if px then
!		genmc(m_lea, px, mgenmem(dblock))		!param points to temp
!!	else
!!		gen
!	fi
!
!note: this is needed since there may be other blocks passed before the end
!of a CALL op, as those ax/bx regs would be tied up
!caller should ensure no workregs are in use

	freeworkregs()
end

global func isimmint(int n)pcl p=
	if pclstack[n].code then
		p:=pclopnds[n]
		if p.optype=int_opnd then
			return p
		fi
	fi
	nil
end

global func ismemaddr(int n)int=
	if pclstack[n].code and pclopnds[n].opndtype=memaddr_opnd then return 1 fi
	return 0
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
	symbol dblock

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

			mode:=pclstack[j].mode

			case mode
			when tblock then
				ax:=loadparam(j, mode, nextireg)
				copyblockarg(ax, callargsize[ncalldepth,k], k)

			when tr64, tr32 then
				loadparam(j, mode, nextxreg)
				if nvariadics and k>=nvariadics then			!variadic floats go to both regs

!I need to move xmm reg to int reg
					imode:=(mode=tr32|tu32|tu64)
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
			pushpcl_reg(m, (stdfloat[m]|multxregs[i]|multregs[i]))
		od

	else
		pushpcl_reg(p.mode, (stdfloat[p.mode]|xr0|r0))

	fi
end

global func newblocktemp(int size)symbol=
	[16]char str
	symbol d

	if nblocktemps>maxblocktemps then
		merror("Too many block temps")
	fi
	++nblocktemps

	fprint @str,"$B#",nblocktemps

	d:=makesymbol(str, frameid)
	d.mode:=tblock
	d.size:=size
	d.used:=1
!	d.nextlocal:=currfunc.nextlocal

	d.nextdef:=currfunc.deflist
	currfunc.deflist:=d

 	d.owner:=currfunc
!	currfunc.nextlocal:=d

	blockdefs[nblocktemps]:=d
	d
end

global proc do_for(pcl p, int incop, addop, cond)=
	pcl q, r
	mclopnd ax, bx, cx, dx, mx
	int reg

	q:=p.next
	r:=currpcl:=q.next

	mx:=mgenmem(q.def, pmode)

	if q.def.reg then
		if p.stepx=1 then
			genmc(incop, mx)
		else
			genmc(addop, mx, genint(p.stepx))
		fi
		ax:=mx
	else
!		ax:=genreg(getworkireg())
		ax:=gwrm(ti64)
		genmc(m_mov, ax, mx)
		if p.stepx=1 then
			genmc(incop, ax)
		else
			genmc(addop, ax, genint(p.stepx))
		fi
		genmc(m_mov, mx, ax)
	fi

	if r.opndtype=int_opnd then
		bx:=genint(r.value)
	else
		bx:=genmem(r.def)
	fi

	genmc(m_cmp, ax, bx)

	genmc_cond(m_jmpcc, cond, genlabel(p.labelno))
end

global proc do_jumptruefalse(pcl p, int cond)=
	mclopnd ax, bx

	ax:=loadopndr(zz)

	if ispint(pmode) then
		genmc(m_test, ax, ax)

	else
		bx:=gwrm()
		genmc(m_xorps, bx, bx)
MCOMM("D")
		genmc(m_comiss, ax, bx)
	fi

	genmc_cond(m_jmpcc, cond, genlabel(p.labelno))

	poppcl()
end

global proc do_incr(pcl p, int incrop, addop)=
	mclopnd mx

	mx:=getopnd_ind(zz)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, genint(p.stepx, p.size))
	fi
	poppcl()
end

global proc do_incrload(pcl p, int incrop, addop)=
	mclopnd ax, mx
	int reg
	pcsrec ps

	mx:=getopnd_ind(zz)

	reg:=mx.reg

	ax:=mgenreg(pclstack[zz].reg, pmode)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	genmc(m_mov, ax, mx)

!now replace ax opnd with new value
	clear ps

	ps.reg:=reg
	ps.mode:=pmode
	pclstack[zz]:=ps
!CPL "------", PS.REG, PS.TEMP

	pclopnds[zz]:=nil

end

global proc do_loadincr(pcl p, int incrop, addop)=
	mclopnd ax, mx

	mx:=getopnd_ind(zz)

	pushpcl_reg(pmode)			!to hold loaded value

	ax:=getopnd(zz)

	genmc(m_mov, ax, mx)

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	swapopnds(yy, zz)
	poppcl()
end

global proc do_bitwise(pcl p, int opc)=
	mclopnd ax, bx

	ax:=loadopndw(yy)
	bx:=getopnd(zz)

	genmc(opc, ax, bx)

	poppcl()
end

global proc do_shift(pcl p, int opc)=
	mclopnd ax, cx
	pcl y
	pcsrec ps:=pclstack[zz]

	ax:=loadopndw(yy)

	y:=pclopnds[zz]

	if ps.code and y.opndtype=int_opnd then
		genmc(opc, ax, mgenint(y.value))
	else
		genmc(m_push, mgenreg(r10)) when r10used
		cx:=loadparam(zz, tu8, r10)
		genmc(opc, ax, cx)
		genmc(m_pop, mgenreg(r10)) when r10used
	fi
	poppcl()
end

global proc do_max_int(int cond)=
	mclopnd ax, bx

	ax:=loadopndw(yy)
	bx:=loadopndr(zz)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	poppcl()
end

global proc do_max_float(int opc)=
	mclopnd ax, bx
	ax:=loadopndw(yy)
	bx:=getopnd(zz)
	genmc(opc, ax, bx)
	poppcl()
end

global proc do_binto(pcl p, int opc, fopc)=
	mclopnd ax, bx, rx

	if ispfloat(pmode) then
		do_binto_float(p, fopc)
		return
	fi

!MCOMM("ONE")
	ax:=getopnd_ind(zz, p.mode)
!MCOMM("TWO")
!MCOMM(MSTROPND(AX))
	bx:=loadopndr(yy, p.mode)

	genmc(opc, ax, bx)
	poppcl()
	poppcl()
end

global proc do_binto_float(pcl p, int opc)=
	mclopnd px, bx, cx

	pushpcl_reg(pmode)		!z^:=y => y^:=x; z is temo

	px:=getopnd_ind(yy)
	bx:=getopnd(xx)
	cx:=getopnd(zz)

	genmc(m_mov, cx, px)
	genmc(opc, cx, bx)
	genmc(m_mov, px, cx)

	poppcl()
	poppcl()
	poppcl()
end

global proc do_shiftnto(pcl p, int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	mclopnd px, cx
	pcsrec ps

	px:=getopnd_ind(zz)
	ps:=pclstack[yy]

	if ps.code and pclopnds[yy].opndtype=int_opnd then
		genmc(opc, px, mgenint(pclopnds[yy].value, pmode))

	else
		genmc(m_push, mgenreg(r10)) when r10used

		cx:=loadparam(yy, tu8, r10)
		genmc(opc, px, cx)

		genmc(m_pop, mgenreg(r10)) when r10used

	fi

	poppcl()
	poppcl()
end

global proc do_maxto_int(int cond, mode)=
	mclopnd ax, bx, lx
	int lab

!	if size<8 then merror("min/maxto size?") fi

	ax:=getopnd_ind(zz)
	bx:=loadopndr(yy)

	genmc(m_cmp, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=genlabel(lab))
	genmc(m_mov, ax, bx)
	genmc(m_label, lx)
	poppcl()
	poppcl()
end

global proc do_maxto_real(int cond, mode)=
	mclopnd px, ax, bx, lx
	int lab

	px:=getopnd_ind(zz, mode)
	bx:=loadopndr(yy, mode)

	pushpcl_reg(mode)

CPL =STRMODE(PMODE)
	ax:=getopnd(yy, pmode)
CPL =MSTROPND(AX)

	genmc(m_mov, ax, px)

MCOMM("E")
	genmc(m_comiss, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=genlabel(lab))
	genmc(m_mov, px, bx)
	genmc(m_label, lx)
	poppcl()
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

		ax:=loadopndw(locyy, pmode)
		fdivto:=1
		genmc(m_push, changeopndsize(ax, 8))
		px:=makeopndind(ax, pmode)
		ax:=mgenreg(ax.reg, pmode)

		genmc(m_mov, ax, px)
	else
		ax:=loadopndw(locyy, pmode)
	fi

	q:=isimmint(loczz)

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
				genmc((issigned|m_sar|m_shr), ax, genint(shifts, pmode))
				poppcl()
				return
			fi
		esac
	fi 

	bx:=loadopndr(loczz)

	saverdx()
	fixdivopnds(locyy, loczz)
	bx:=loadopndr(loczz)			!in case regs have changed

	if issigned then
		opc:=
			case stdsize[pmode]
			when 8 then	m_cqo
			when 4 then	m_cdq
			when 2 then	m_cwd
			else merror("div/u8"); 0
			esac
		genmc(opc)

		opc:=m_idiv
	else
!		genmc(m_xor, mgenreg(r11), mgenreg(r11))
		clearreg(mgenreg(r11))
		opc:=m_div
	fi

	genmc(opc, bx)

	case isdiv
	when 0 then				!rem
		genmc(m_xchg, mgenreg(r0, pmode), mgenreg(r11, pmode))

	when 2 then				!divrem
		genmc(m_xchg, bx, mgenreg(r11, pmode))			!rem replace y-operand
		swapopndregs(r1)						!make sure it is in r1
		swapopnds(locyy, loczz)

	esac

	restorerdx()

	if fdivto then
		bx:=gwrm(ti64)
		genmc(m_pop, bx)
		genmc(m_mov, makeopndind(bx, pmode), getopnd(locyy))
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
	int regx, regy, zop
	mclopnd bx, ax

	regx:=pclstack[locyy].reg
	regy:=pclstack[loczz].reg

	if regx=r0 then			!regy will be OK
		return
	fi

	bx:=getopnd(locyy, tu64)
	ax:=getopnd(loczz, tu64)

	if regy=r0 then			!need to swap then
		genmc(m_xchg, bx, ax)
		swapopnds(locyy, loczz)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset.[r0]=0 then	!d0 not in use
		genmc(m_xchg, mgenreg(r0), bx)
		regset.[regx]:=0				!switch registers for yy

		pclstack[locyy].reg:=r0
		regset.[r0]:=1

		return
	fi

!need to move current occupier of r0
	for zop:=noperands downto 1 do
		if pclstack[zop].reg=r0 then exit fi
	else
		return
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg, mgenreg(r0), getopnd(locyy, tu64))	
	swap(pclstack[locyy].reg, pclstack[zop].reg)		!switch registers
end

proc saverdx=
	genmc(m_push, mgenreg(r11)) when r11used
end

proc restorerdx=
	genmc(m_pop, mgenreg(r11)) when r11used
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
	symbol d
	pcl q
	pcsrec ys:=pclstack[yy], zs:=pclstack[zz]

	scale:=p.scale
	extra:=p.extra

	q:=isimmint(zz)
	if q then
		offset:=q.value*scale+extra	!for imm offset
	fi

	px:=nil

!	if pclloc[yy]=regvar_loc then
!!		if pclloc[zz]=regvar_loc then		!regvar/regvar
!!			reg:=pclreg[zz]
!!			regix:=scaleregvar(reg,scale,zz)
!!			px:=mgenindex(areg:pclreg[yy],ireg:regix, offset:extra, scale:scale)
!!
!!		elsif q then						!regvar/imm
!		if q then						!regvar/imm
!			px:=mgenindex(areg:pclreg[yy], offset:offset)
!		else								!regvar/any
!			scale:=scaleindex(bx:=loadopndr(zz, pclstack[zz].mode), scale)
!			px:=mgenindex(areg:pclreg[yy], ireg:bx.reg, scale:scale, offset:extra)
!		fi
!
!	elsif ismemaddr(yy) then
	if ismemaddr(yy) then
		d:=pclopnds[yy].def
		if d.nameid=staticid and highmem=2 or D.NAMEID=PARAMID AND D.MODE=TBLOCK  then skip fi

!		if pclloc[zz]=regvar_loc then			!memaddr/regvar
!			reg:=pclreg[zz]
!			regix:=scaleregvar(reg,scale,zz)
!			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)
!
!		elsif q then			!memaddr/imm
		if q then			!memaddr/imm
			px:=genindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=loadopndr(zz, ti64), scale)
			px:=genindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
skip:
		ax:=loadopndr(yy, tu64)

		if zs.reg then			!any/regvar
			regix:=scaleregvar(zs.reg, scale, zz)
			px:=genindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)

		elsif q then						!any/imm	
			px:=genindex(areg:ax.reg, offset:offset)
		else
			scale:=scaleindex(bx:=loadopndr(zz, tu64), scale)
			px:=genindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	fi

	px.size:=p.size

	return px
end

function scaleregvar(int reg, &scale, n)int=
!When scale is 1/2/3/4, return reg (a regvar) and scale unchanged;
!otherwise set up a new register for operand n
!Copy reg to it, and scale. Return new reg, and set scale to 1
	int regix
	mclopnd ax

	if scale in [1,2,4,8] then return reg fi

	regix:=gwri()
	ax:=mgenreg(regix)

	IF SCALE=16 THEN
		genmc(m_lea, ax, genindex(ireg:reg, areg:reg, scale:1))
		scale:=8

	ELSE
		genmc(m_mov,ax, genreg(reg))
		mulimm(ax,scale)
		scale:=1
	FI

	pcsrec ps
	clear ps

	ps.reg:=regix
	ps.mode:=ti64

	return regix
end

global proc dolea(mclopnd ax, px)=
!do 'lea ax, px`, but suppress in cases like 'lea d0,[d0]'
	unless px.regix=px.valtype=px.offset=0 and px.reg=ax.reg then
		genmc(m_lea, ax, px)
	end
end

global proc clearblock(mclopnd ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

!CPL "CLEARBLOCK", MSTROPND(AX), N

	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg

	oddbytes:=n rem 8		!will be zero, or 1..7

	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=gwrm(tu64)
	clearreg(rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax, 8)

		to nwords do
			genmc(m_mov, applyoffset(ax, offset), rx)
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

			rcount:=genreg(countreg:=gwri())
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)

			genmc(m_mov, rcount, genint(nwords))
			genmc(m_label, genlabel(lab))
			genmc(m_mov, ax, rx)

			genmc(m_add, genreg(ax.reg), genint(8))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, genlabel(lab))

			offset:=0
		else
			rcount:=genreg(countreg:=gwri())
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)
			genmc(m_mov, rcount, genint(nwords/4))
			genmc(m_label, genlabel(lab))

			for i to 4 do
				genmc(m_mov, applyoffset(ax, offset), rx)
				offset+:=8
			od

			genmc(m_add, genreg(ax.reg), genint(targetsize*4))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, genlabel(lab))

			offset:=0
		fi
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx, 4)
			genmc(m_mov, applyoffset(ax, offset, 4), rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx, 2)
			genmc(m_mov, applyoffset(ax, offset, 2), rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx, 1)
			genmc(m_mov, applyoffset(ax, offset, 1), rx)
		fi
	fi
end

global proc do_negreal(mclopnd ax, int mode)=
	if ispwide(pmode) then
		if not labneg64 then labneg64:=createfwdlabel() fi
		genmc(m_xorpd, ax, genlabelmem(labneg64))
	else
		if not labneg32 then labneg32:=createfwdlabel() fi
		genmc(m_xorps, ax, genlabelmem(labneg32))
	fi
end

global proc do_absreal(mclopnd ax, int mode)=
	if ispwide(pmode) then
		if not lababs64 then lababs64:=createfwdlabel() fi
		genmc(m_andpd, ax, genlabelmem(lababs64))
	else
		if not lababs32 then lababs32:=createfwdlabel() fi
		genmc(m_andps, ax, genlabelmem(lababs32))
	fi
end

global proc do_loadbf_const(pcl p, int i, j) =
	mclopnd ax, mx
	word mask

	ax:=loadopndw(xx)

	if j=63 then			!signed field includes sign bit; assume i>0
		genmc(m_sar, ax, mgenint(i))
	else

		if i then
			genmc(m_shr, ax, mgenint(i))
		fi

		mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
		if mask<=word(i32.max) then			!use immediate
			genmc(m_and, ax, genint(mask))
		else
			mx:=loadopndw(yy, tu64)
			genmc(m_mov, mx, genint(mask))
			genmc(m_and, ax, mx)
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

	q:=isimmint(zz)
	r:=isimmint(xx)

	if q then						!a.[k] := 0/1/x
		px:=getopnd_ind(yy, tu8)	!update only a specific byte
		i:=q.value	
		offset:=i/8					! byte offset 0..7
		i iand:=7					! i will be bit index 0..7
		px:=applyoffset(px, offset)	! point to that byte

		mask0s:=1<<i				!eg 00001000
		mask1s:=inot(1<<i)			!eg 11110111

		if r then
			if r.value=0 then
				genmc(m_and, px, mgenint(mask1s, pmode))
			else
				genmc(m_or, px, mgenint(mask0s, pmode))
			fi
		else
			ax:=loadopndw(xx, tu8)
			genmc(m_and, px, mgenint(mask1s, pmode))		!clear dest bit first
			if i then
				genmc(m_shl, ax, mgenint(i, tu8))
			fi
			genmc(m_or, px, ax)							!add in 0 or 1
		fi
	elsif r then								!A.[i]:=0/1/x
		px:=getopnd_ind(yy, pmode)				!update whole dest word

		if q=nil then								!A.[i]:=0/1
			ax:=gwrm(tu64)
			genmc(m_mov, ax, mgenint(1))
			
			cx:=mgenreg(r10, tu64)
			genmc(m_push, cx) when r10used
			ix:=loadparam(zz, ti64, r10)
			genmc(m_shl, ax, changeopndsize(cx, 1))
			genmc(m_pop, cx) when r10used

!Now have 00001000 for ezzmple in ax
			if r.value=0 then
				genmc(m_not, ax)				!change to 111101111
				genmc(m_and, px, ax)			!set to 0
			else								!set to 1 (assume r.value was 1)
				genmc(m_or, px, ax)
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
	mclopnd ax, rx, mx, mx4, dx
	int i, j
	pcl q, r
	word mask

	q:=isimmint(yy)
	r:=isimmint(zz)

	if q=r=nil then
		merror("storebf not imm")
	fi

	dx:=loadopndr(ww)

	ax:=getopnd_ind(xx)

	i:=q.value
	j:=r.value

	mx:=gwrm(tu64)
	rx:=gwrm(pmode)

	genmc(m_mov, rx, ax)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

	genmc(m_mov, mx, genint(mask))

	if i then
		genmc(m_shl, dx, genint(i))
	fi

	genmc(m_and, rx, changeopndsize(mx, p.size))
	genmc(m_or, rx, dx)

	genmc(m_mov, ax, changeopndsize(rx, p.size))

	poppcl()			!j
	poppcl()			!i
	poppcl()			!A
	poppcl()			!x?
end

global proc do_callrts(pcl p, ichar opname, int nargs, symbol d=nil)=
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
		genmc(m_call, genextname(opname))
	else
		genmc(m_call, genmemaddr(d))
	fi

	to nargs do
		poppcl()
	od

	if slots then
		popslots(slots)
	fi

	do_getretvalue(p)
end

global func findhostfn(int opc)symbol=
!called from pcl/mcl backend. opc refers to a PCL op

	case opc
	when kpower then			!assume for i64
		getsysfnhandler(sf_power_i64)

	else
		nil
	esac
end

=== mcx_decls.m 0 0 26/29 ===


global type mclopnd = ref mclopndrec

global record mclopndrec =
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		symbol def
		i64 value		!immediate value
		r64 xvalue		!immediate or memory real (depends on .mode)
		ichar svalue	!immediate string
		int labelno
		int sysfn
		struct
			i32 tempno
			byte lasttemp		!set to 1 if .islast applied to the temp
		end
	end

	u32 size			! usually one of 1/2/4/8, but can also be string/data length
	i32 offset			! additional offset to memory operands
	byte scale			! should be 1/2/4/8
	byte mode			!a_reg, a_imm, a_mem etc
	byte valtype		!int_val etc

	byte reg			!0, or main register
	byte regix			!0, or index register
end

!value types associated with a_imm or a_mem modes (not all combos valid):
global enumdata [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(int_val,		$),		!immediate int
	(real_val,		$),		!immediate real (as data) or mem (code)
	(string_val,	$),		!immediate (data) or label (code)
	(def_val,		$),		!var/proc name as imm or mem
	(label_val,		$),		!label index; imm/mem
	(name_val,		$),		!immediate string must be output as an unquoted name
	(temp_val,		$),		!index of pclopnd temp (later becomes ptr to descriptor?)
	(data_val,		$),		!data string; always immediate
end

global type mcl = ref mclrec

global record mclrec = !$caligned
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

!	union
!		[r0..r15]byte regfreed		!1 indicates work-register freed after this instr
!		pair regfreedpr
!	end
end

global enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes, []byte mclauto =

	(m_procstart,		$,		0,		0,		0),
	(m_procend,			$,		0,		0,		0),
	(m_comment,			$,		0,		0,		0),
!	(m_blank,			$,		0,		0,		0),
!	(m_deleted,			$,		0,		0,		0),
	(m_labelname,		$,		0,		0,		0),
	(m_define,			$,		0,		0,		0),
	(m_definereg,		$,		0,		0,		0),
	(m_definetemp,		$,		0,		0,		0),
	(m_trace,			$,		0,		0,		0),
	(m_endx,			$,		0,		0,		0),

	(m_label,			$,		1,		0,		0),
	(m_nop,				$,		0,		0x90,	0),
!	(m_param,			$,		1,		0,		0),
!	(m_assembly,		$,		1,		0,		0),
!	(m_proc,			$,		1,		0,		0),

	(m_mov,				$,		2,		0,		0),
	(m_push,			$,		1,		0,		0),
	(m_pop,				$,		1,		0,		0),
	(m_lea,				$,		2,		0,		0),
	(m_cmovcc,			$,		2,		0,		0),

	(m_movd,			$,		2,		0,		2),
	(m_movq,			$,		2,		0,		0),

	(m_movsx,			$,		2,		0,		0),
	(m_movzx,			$,		2,		0,		0),
	(m_movsxd,			$,		2,		0,		0),

	(m_call,			$,		1,		0xE8,	0),
	(m_ret,				$,		0,		0xC3,	0),
	(m_leave,			$,		0,		0xC9,	0),
	(m_retn,			$,		1,		0,		0),

	(m_jmp,				$,		1,		0xE9,	0),
	(m_jmpcc,			$,		1,		0,		0),
	(m_xchg,			$,		2,		0,		0),

	(m_adc,				$,		2,		2,		0),
	(m_sbb,				$,		2,		3,		0),
	(m_imul,			$,		1,		5,		0),
	(m_mul,				$,		1,		4,		0),
	(m_imul2,			$,		2,		0,		0),
	(m_imul3,			$,		3,		0,		0),

	(m_idiv,			$,		1,		7,		0),
	(m_div,				$,		1,		6,		0),

	(m_and,				$,		2,		0x04,	0),
	(m_or,				$,		2,		0x01,	0),
	(m_xor,				$,		2,		0x06,	0),
	(m_test,			$,		2,		0,		0),

	(m_cmp,				$,		2,		0x07,	0),

	(m_shl,				$,		2,		0x04,	0),
	(m_sar,				$,		2,		0x07,	0),
	(m_shr,				$,		2,		0x05,	0),
	(m_rol,				$,		2,		0x00,	0),
	(m_ror,				$,		2,		0x01,	0),
	(m_rcl,				$,		2,		0x02,	0),
	(m_rcr,				$,		2,		0x03,	0),

	(m_neg,				$,		1,		3,		0),
	(m_not,				$,		1,		2,		0),

	(m_inc,				$,		1,		0,		0),
	(m_dec,				$,		1,		1,		0),

	(m_cbw,				$,		0,		0,		0),
	(m_cwd,				$,		0,		0,		0),
	(m_cdq,				$,		0,		0,		0),
	(m_cqo,				$,		0,		0,		0),
	(m_setcc,			$,		1,		0,		0),

	(m_bsf,				$,		2,		0xBC,	0),
	(m_bsr,				$,		2,		0xBD,	0),

	(m_shld,			$,		2,		0xA4,	0),
	(m_shrd,			$,		2,		0xAC,	0),

	(m_sqrtss,			$,		2,		0x51,	2),
	(m_sqrtsd,			$,		2,		0x51,	0),

	(m_add,				$,		2,		0,		1),
	(m_addss,			$,		2,		0x58,	2),
	(m_addsd,			$,		2,		0x58,	0),

	(m_sub,				$,		2,		5,		1),
	(m_subss,			$,		2,		0x5C,	2),
	(m_subsd,			$,		2,		0x5C,	0),

	(m_mulss,			$,		2,		0x59,	2),
	(m_mulsd,			$,		2,		0x59,	0),

	(m_divss,			$,		2,		0x5E,	2),
	(m_divsd,			$,		2,		0x5E,	0),

	(m_comiss,			$,		2,		0,		2),
	(m_comisd,			$,		2,		0x2F,	0),
	(m_ucomisd,			$,		2,		0x2E,	0),

	(m_xorps,			$,		2,		0x57,	2),
	(m_xorpd,			$,		2,		0x57,	0),

	(m_andps,			$,		2,		0x54,	2),
	(m_andpd,			$,		2,		0x54,	0),

	(m_pxor,			$,		2,		0xEF,	0),
	(m_pand,			$,		2,		0xDB,	0),

	(m_cvtss2si,		$,		2,		0,		0),
	(m_cvtsd2si,		$,		2,		0,		0),

	(m_cvttss2si,		$,		2,		0,		0),
	(m_cvttsd2si,		$,		2,		0,		0),

	(m_cvtsi2ss,		$,		2,		0,		0),
	(m_cvtsi2sd,		$,		2,		0,		0),

	(m_cvtsd2ss,		$,		2,		0,		0),
	(m_cvtss2sd,		$,		2,		0,		0),

	(m_movdqa,			$,		2,		0x66,	0),
	(m_movdqu,			$,		2,		0xF3,	0),

	(m_pcmpistri,		$,		3,		0x63,	0),
	(m_pcmpistrm,		$,		3,		0x62,	0),

	(m_fld,				$,		1,		0,		0),
	(m_fst,				$,		1,		2,		0),
	(m_fstp,			$,		1,		3,		0),

	(m_fild,			$,		1,		0,		0),
	(m_fist,			$,		1,		2,		0),
	(m_fistp,			$,		1,		3,		0),

	(m_fadd,			$,		0,		0xC1,	0),
	(m_fsub,			$,		0,		0xE9,	0),
	(m_fmul,			$,		0,		0xC9,	0),
	(m_fdiv,			$,		0,		0xF9,	0),
	(m_fsqrt,			$,		0,		0xFA,	0),
	(m_fsin,			$,		0,		0xFE,	0),
	(m_fcos,			$,		0,		0xFF,	0),
	(m_fsincos,			$,		0,		0xFB,	0),
	(m_fptan,			$,		0,		0xF2,	0),
	(m_fpatan,			$,		0,		0xF3,	0),
	(m_fabs,			$,		0,		0xE1,	0),
	(m_fchs,			$,		0,		0xE0,	0),

	(m_minss,			$,		2,		0x5D,	2),
	(m_minsd,			$,		2,		0x5D,	0),
	(m_maxss,			$,		2,		0x5F,	2),
	(m_maxsd,			$,		2,		0x5F,	0),

	(m_db,				$,		1,		0,		0),
	(m_dw,				$,		1,		0,		0),
	(m_dd,				$,		1,		0,		0),
	(m_dq,				$,		1,		0,		0),
	(m_ascii,			$,		1,		0,		0),
	(m_asciiz,			$,		1,		0,		0),
!	(m_ddoffset,		$,		1,		0,		0),

!	(m_segment,			$,		1,		0,		0),
	(m_isegment,		$,		0,		0,		0),
	(m_zsegment,		$,		0,		0,		0),
	(m_csegment,		$,		0,		0,		0),

	(m_align,			$,		1,		0,		0),
	(m_resb,			$,		1,		1,		0),
	(m_resw,			$,		1,		2,		0),
	(m_resd,			$,		1,		4,		0),
	(m_resq,			$,		1,		8,		0),

	(m_xlat,			$,		0,		0xD7,	0),
	(m_loopnz,			$,		1,		0xE0,	0),
	(m_loopz,			$,		1,		0xE1,	0),
	(m_loopcx,			$,		1,		0xE2,	0),
	(m_jecxz,			$,		1,		0xE3,	0),
	(m_jrcxz,			$,		1,		0xE3,	0),

	(m_cmpsb,			$,		0,		0,		0),
	(m_cmpsw,			$,		0,		0,		0),
	(m_cmpsd,			$,		0,		0,		0),
	(m_cmpsq,			$,		0,		0,		0),

	(m_rdtsc,			$,		0,		0x31,	0),
	(m_popcnt,			$,		2,		0,		0),
	(m_bswap,			$,		1,		0,		0),

	(m_finit,			$,		0,		0,		0),

	(m_fldz,			$,		0,		0xEE,	0),
	(m_fld1,			$,		0,		0xE8,	0),
	(m_fldpi,			$,		0,		0xEB,	0),
	(m_fld2t,			$,		0,		0xE9,	0),
	(m_fld2e,			$,		0,		0xEA,	0),
	(m_fldlg2,			$,		0,		0xEC,	0),
	(m_fldln2,			$,		0,		0xED,	0),

	(m_cpuid,			$,		0,		0,		0),

	(m_halt,			$,		0,		0xF4,	0),
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

	(xr0,		$,	0),			!xmm0
	(xr1,		$,	1),
	(xr2,		$,	2),
	(xr3,		$,	3),
	(xr4,		$,	4),
	(xr5,		$,	5),
	(xr6,		$,	6),
	(xr7,		$,	7),
	(xr8,		$,	8),
	(xr9,		$,	9),
	(xr10,		$,	10),
	(xr11,		$,	11),
	(xr12,		$,	12),
	(xr13,		$,	13),
	(xr14,		$,	14),
	(xr15,		$,	15),

end

global const rframe = r14
global const rstack = r15

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

global enumdata [0:]ichar segmentnames =
	(no_seg=0,		$),
	(code_seg,		$),
	(idata_seg,		$),
	(zdata_seg,		$),
	(rodata_seg,	$),
	(impdata_seg,	$),
end

global enumdata [0:]ichar reftypenames =
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

global enumdata [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
end

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

global ref mclrec mccode, mccodex		!genmc adds to this linked list

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

global const init_ss_symbols=32768				!globaled to coff
global ref []symbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref[]symbol labeldeftable

global int aaseqno
global int aapos

global [1..8]byte regmodes=(tu8, tu16, 0, tu32, 0,0,0, tu64)

global ref mclrec mclprocentry
global ref mclrec mce_oldmccodex, mce_lastmcl, mce_nextmcl		!used by reset/setmclentry
global ref mclrec mcf_oldmccodex, mcf_lastmcl, mcf_nextmcl		!used by reset/setmclentry for frame setup

global byte fpcheckunusedlocals

global record riprec =
	ref riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

global ref riprec riplist
global byte mcldone

global const ctarget=0


global const mclpresent=1
=== mcx_asm.m 0 0 27/29 ===
!const fshowseq=1
const fshowseq=0

!const showsizes=1
const showsizes=0

!const showfreed=1
const showfreed=0

[r0..xr15]symbol regvars		!nil, or strec when it uses that reg

global int assemtype='AA'

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer

global func writeasm:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	symbol d, e
	ref mclrec m
	[32]char str2, str3
	int i

	gs_init(pdest)
!
	case highmem
	when 1 then asmstr("    $userip\n")
	when 2 then asmstr("    $highmem\n")
	esac

	m:=mccode
	i:=1
	while m do
		writemcl(i, m)
		++i
		m:=m.nextmcl
	od
	return pdest
end

proc writemcl(int index, ref mclrec mcl)=
	if mcl.opcode=m_comment and mcl.a.svalue^='?' then
	else
		strmcl(mcl)
!CPL "//"
		gs_line(pdest)
	fi
end

global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mclopnd a, b
	int opcode, cond, sizepref
	symbol d

	opcode:=mcl.opcode
	str[1]:=0

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b

!CPL "STRMCL", MCLNAMES[OPCODE], A, B

	case opcode
	when m_procstart then

		asmstr(";Proc ")
		asmstr(a.def.name)
		currfunc:=a.def
		clear regvars

		return

	when m_procend then
		asmstr(";End\n")
		currfunc:=nil

		return

	when m_comment then
		if a.svalue^ then
			asmchar(';')
			asmstr(a.svalue)
		fi
		return
	when m_endx then
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		d:=a.def
		case a.valtype
		when def_val then
			asmstr(getdispname(d))
		when string_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")

		if d.scope=export_scope then
!			if eqstring(getbasename(d.name), d.name) then
!				asmstr(":")
!			else
				asmstr("\n`")
				asmstr(getbasename(d.name))
				asmstr("::")
!			fi
		fi

!ASMSTR(" ")
!ASMSTR(STRINT(INT(D), "H"))

		return

	when m_label then
		if a.valtype=label_val then
			fprint @str, "L#:", a.value
		else
			recase m_labelname
		fi
		asmstr(&.str)
		return

	when m_define then
		d:=a.def
		asmstr("    ")
		asmstr(getdispname(d))
		asmstr(" = ")
		asmint(d.offset)
		return

	when m_definereg then
		d:=a.def
		asmstr("    ")
		asmstr(getdispname(d))
		regvars[d.reg]:=d

		asmstr(" = ")
		asmstr(getregname(b.reg, b.size))
		return

	when m_definetemp then
		asmstr("    ")
		asmstr(mstropnd(a))
!		asmstr(gettempname(currfunc, a.tempno))
		asmstr(" = ")
		asmint(b.value)
		return

	when m_asciiz then
		asmstr("    db ")
		asmstr(mstropnd(a))
		asmstr(", 0")
		return


	esac

	case opcode
	when m_jmpcc then
		print @&.opcname, "j", ,asmcondnames[cond]

	when m_setcc then
		print @&.opcname, "set", ,asmcondnames[cond]

	when m_cmovcc then
		print @&.opcname, "cmov", ,asmcondnames[cond]

	when m_and then
		strcpy(&.opcname, "and")
	when m_or then
		strcpy(&.opcname, "or")
	when m_xor then
		strcpy(&.opcname, "xor")
	when m_not then
		strcpy(&.opcname, "not")

	ELSIF OPCODE>M_HALT THEN
		STRCPY(&.OPCNAME, STRINT(OPCODE))

	else
		strcpy(&.opcname, mclnames[opcode]+2)
	esac

!	ipadstr(&.opcname, (opcode=m_dq|4|10), " ")
	ipadstr(&.opcname, 10, " ")
	ipadstr(&.str, 4)

	strcat(&.str, &.opcname)

	asmstr(&.str)

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode, a, b)
!
		asmopnd(a, sizepref)
		asmstr(", 	")
		asmopnd(b, sizepref)

		if mcl.c then
			asmstr(", ")
			asmstr(strint(mcl.c))
		fi

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a, 0, opcode)
		else
			asmopnd(a, 1, opcode)
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

	if fshowseq then
		asmstr(" ; ")
		asmstr(strint(mcl.seqno, "z5"))
	fi

end

global func strmclstr(ref mclrec m)ichar=
	gs_init(pdest)
	strmcl(m)
	return pdest.strptr
end

global func mstropnd(mclopnd a, int sizeprefix=0, opcode=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus, t
	int offset, tc

!RETURN "<OPND>"

	str[1]:=0

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
!CPL "OPND/IMM", VALTYPENAMES[A.VALTYPE]
		if opcode=m_dq and a.valtype=int_val then
!			if a.value in 0..9 then
				strcat(&.str, strint(a.value))
!			else
!				strcat(&.str, "0x")
!				strcat(&.str, strword(a.value, "H"))
!			fi
		else
			strcpy(&.str, strvalue(a))
		fi

	when a_mem then
!CPL "OPND/MEM", VALTYPENAMES[A.VALTYPE]
		strcat(&.str, getsizeprefix(a.size, sizeprefix))
		strcat(&.str, "[")

		plus:=""
		if a.reg then
			strcat(&.str, strreg(a.reg, 8))
			plus:=" + "
		fi
		if a.regix then
			strcat(&.str, plus)
			strcat(&.str, strreg(a.regix, 8))
			plus:=" + "

			if a.scale>1 then
				strcat(&.str, "*")
				strcat(&.str, strint(a.scale))
			fi
		fi

		if a.valtype in [def_val, label_val, temp_val] then
			if plus^ then
				strcat(&.str, plus)
			fi
			strcat(&.str, strvalue(a))
	    elsif offset:=a.offset then
			print @&.str2, offset:" + "
			strcat(&.str, &.str2)
		fi
		strcat(&.str, "]")

	else
		println "BAD OPND", A.MODE, =A
		return "<BAD OPND>"
	esac

	return &.str
end

global func strvalue(mclopnd a)ichar=
	static [512]char str
	[128]char str2
	symbol def
	i64 value, offset, length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(&.str, "")

	case a.valtype
	when def_val then
		strcat(&.str, getdispname(def))

	addoffset:
		if offset:=a.offset then
			print @&.str2, (offset>0|"+"|""), ,offset
			strcat(&.str, &.str2)
		fi

	when int_val then
		strcat(&.str, strint(value))

	when real_val then
		if a.size=8 then
			print @str, a.xvalue:"20.20"
		else
			print @str, r32(a.xvalue):"20.20"
		fi

!	when realmem_val then
!		strcat(str, "M")
!		strcat(str, strreal(a.xvalue))

	when string_val then
		strcat(str, """")
		strcat(str, a.svalue)
		strcat(str, """")

	when name_val then
		strcat(str, a.svalue)

	when label_val then
		strcat(str, "L")
		strcat(str, strint(a.labelno))
		goto addoffset

	when temp_val then
		return gettempname(currfunc, a.tempno)

	else
		merror("Stropnd?")
	esac

	return str
end

global proc asmopnd(mclopnd a, int sizeprefix=0, opcode=0)=
	asmstr(mstropnd(a, sizeprefix, opcode))
end

global func getregname(int reg, size=8)ichar=
	static [1..17]ichar prefix=("B", "W", "", "A", "", "", "", "D", "", "", "", "", "", "", "", "Q", "N")
	static [32]char str
	[16]char str2
	ichar rs
	int size2

!	if useintelregs then
!		return nregnames[size, reg]
!	fi

	size2:=size
	if size2>16 then
		size2:=17
	fi

	case reg
	when rnone then return "-"
	when rframe then rs:="fp"
	when rstack then rs:="sp"
	elsif reg>=xr0 then
		print @str, "XMM", ,reg-xr0
		return str

	else
		getstrint(reg-r0, str2)
		rs:=str2
	esac

	print @str, prefix[size2], ,rs
	return str
end

proc asmstr(ichar s)=
	gs_str(pdest, s)
end

proc asmint(int a)=
	asmstr(strint(a))
end

proc asmchar(int c)=
	gs_char(pdest, c)
end

global func getdispname(symbol d)ichar=
	static [256]char str

	if d.reg then
		fprint @str, "##R.#", (fshortnames|""|"`"), (stdfloat[d.mode]|"X"|""),
			 (fshortnames|d.name|getfullname(d))
		return str
	fi

	if fshortnames then
		return getbasename(d.name)
	else
		return getfullname(d, backtick:1)
	fi
end 

global func gettempname(symbol d, int n)ichar=
	static [128]char str

	if fshortnames or d=nil then
		print @str, "T", ,n
	else
		fprint @str, "#.$T#", getdispname(d), n
	fi
	str
end

global func strreg(int reg, size=8)ichar=
	symbol d

	d:=regvars[reg]

	if d and stdsize[ttbasetype[d.mode]]=size then
		return getdispname(d)
	fi
	getregname(reg, size)
end

global func needsizeprefix(int opcode, mclopnd a, b)int=
	case opcode
	when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
		return 1

	when m_cvtss2si, m_cvtsd2si, m_cvttss2si, m_cvttsd2si then
		return 1
	when m_shl, m_shr, m_sar then
		if a.mode=a_mem then return 1 fi
		return 0
	esac

	if a.mode=a_reg or b.mode=a_reg then
		return 0
	fi
	return 1
end

global func getsizeprefix(int size, enable=0)ichar=
	static []ichar table=("byte ", "u16 ", "", "u32 ", "","","", "u64 ")
	if not enable or size not in 1..8 then
		""
	else
		table[size]
	fi
end

=== mcx_lib.m 0 0 28/29 ===
const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

!global int mclseqno
global int mclseqno
global int NMCLOPND

[-1..10]mclopnd smallinttable
[20]symbol nametable
int nnametable

global macro isframex(d) = (d.nameid in [frameid, paramid])

global proc mclinit(int bypass=0)=
	mclopnd a
	int r, s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") fi

	for r:=r0 to r15 do
		regtable[r, 1]:=genreg0(r, 1)
		regtable[r, 2]:=genreg0(r, 2)
		regtable[r, 4]:=genreg0(r, 4)
		regtable[r, 8]:=genreg0(r, 8)
	od

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=genreg(rframe, 8)
	dstackopnd:=genreg(rstack, 8)

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

proc start=
	for i in smallinttable.bounds do
		smallinttable[i]:=genint0(i, 8)
	od
end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
!	clear rtsproclabels
end

EXPORT proc genmc(int opcode, mclopnd a=nil, b=nil)=		!used in do_mcl/assem in host
	ref mclrec m, oldm
	int labno

	m:=pcm_allocnfz(mclrec.bytes)

	m.opcode:=opcode
	m.seqno:=++mclseqno
	m.mpos:=mmpos

	m.a:=a
	m.b:=b

	case opcode
!	when m_lea then
!		if b and b.valtype=def_val then
!!*!			b.def.addrof:=1
!		fi
	when m_label then
		labno:=a.labelno

	when m_mov then				!change to movd/q if needed
		if a.reg>=xr0 or (b and b.reg>=xr0) then
			m.opcode:=(a.size=8|m_movq|m_movd)
		fi
	elsecase mclauto[opcode]
	when 1 then					!add/addss/addsd etc
		if a.mode=a_reg and a.reg>=xr0 then
			++m.opcode			!addss etc
			if a.size=8 then
				++m.opcode
			fi
		elsif b.mode=a_reg and b.reg>=xr0 then
			++m.opcode			!addss etc
			if b.size=8 then
				++m.opcode
			fi
		fi

	when 2 then					!sqrtss/sqrtsd etc
		if a.mode=a_reg and a.reg>=xr0 and a.size=8 then
			++m.opcode			!sqrtsd etc
		elsif b and b.mode=a_reg and b.reg>=xr0 and b.size=8 then
			++m.opcode			!sqrtsd etcc
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

global proc genmc_cond(int opcode, cond, mclopnd a=nil, b=nil)=
	genmc(opcode, a, b)
	mccodex.cond:=cond
end

global proc genmc_label(int opcode, labelno)=
	genmc(opcode, genlabel(labelno))
end

global proc genmc_string(int opcode, ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genstring(s))
end

global proc genmc_def(int opcode,  symbol d)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genmem(d))
end

global proc genmc_defaddr(int opcode,  symbol d)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genmemaddr(d))
end

global proc genmc_int(int opcode,  int a)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genint(a))
end

global proc genmc_name(int opcode,  ichar name)=
!as genmc but uses a single immediate string operand
	genmc(opcode, genname(name))
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

EXPORT func genindex(int areg=0, ireg=0, scale=1, offset=0, size=0, labno=0, symbol def=nil)mclopnd=
!construct a mem address mode
	mclopnd a
	a:=newmclopnd()

	a.mode:=a_mem
	a.reg:=areg

!	if areg=rframe or ireg=rframe then usedset.[rframe]:=1 fi

	a.regix:=ireg
	a.scale:=scale
	a.size:=size

	a.offset:=offset

	if labno then
		a.value:=labno
		a.valtype:=label_val
	elsif def then
		a.def:=def
!		++def.nrefs
		a.valtype:=def_val
		if isframex(def) then
			a.reg:=rframe
!			usedset.[rframe]:=1
		fi
	fi

	return a
end

global proc mcomment(ichar s)=
!if not debugmode then return fi
!	if s=nil or s^=0 then
!		genmc(m_blank)
!	else
		genmc_string(m_comment, s)
!	fi
end

global func genstring(ichar s, int length=-1)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm

	if length<0 then
		length:=strlen(s)
	fi

	a.svalue:=pcm_alloc(length+1)
	memcpy(a.svalue, s, length)
	(a.svalue+length)^:=0

	a.valtype:=string_val
	a.size:=8
	return a
end

global func gendata(ref byte p, int length)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstringn(p, length)

	a.valtype:=data_val
	a.size:=length
	return a
end

global func genname(ichar s)mclopnd=
	[64]char str
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstring(s)
	a.valtype:=name_val
	a.size:=8

	return a
end

global proc setsegment(int seg, align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
	int opc, oldalign

	if seg<>currsegment then
		case seg
		when 'I' then opc:=m_isegment
		when 'Z' then opc:=m_zsegment
		when 'C' then opc:=m_csegment
		when 'R' then MERROR("CAN'T DO RODATA SEG")
		else
			MERROR("BAD SEG CODE")
		esac
		if mccodex and mccodex.opcode in [m_isegment, m_zsegment, m_csegment] then
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
		genmc(m_align, genint(align))
	fi
end

global func changeopndsize(mclopnd a, int size)mclopnd=
	mclopnd b

	if a.size<>size then
		if a.reg<xr0 and a.mode=a_reg then
			b:=regtable[a.reg, size]
		else
			b:=duplopnd(a)
			b.size:=size
		fi
		return b
	fi
	return a
end

global func applyoffset(mclopnd a, int offset, int size=0)mclopnd=
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

global func genlabel(int x)mclopnd a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm

!	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val
	a.size:=8

	return a
end

global func genlabelmem(int x)mclopnd a=
!x is a label index
!generate immediate operand containing label

	a:=genlabel(x)
	a.mode:=a_mem
	return a
end

EXPORT func genmemaddr(symbol d)mclopnd=
	mclopnd a

!*!	d.addrof:=1
!	++d.nrefs

	a:=newmclopnd()
	a.mode:=a_imm

	a.def:=d
!	++d.nrefs
	a.valtype:=def_val
	a.size:=8

	return a
end

global func genint(i64 x, int size=8)mclopnd a=

	if x in -1..10 and size=8 then
		return smallinttable[x]
	fi

	a:=newmclopnd()
	a.mode:=a_imm

	case size
	when 1 then x iand:=255
	when 2 then x iand:=65535
	when 4 then x iand:=0xffff'ffff
	esac

	a.value:=x
	a.valtype:=int_val
	a.size:=size

	return a
end

global func genint0(i64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=int_val
	a.size:=size

	return a
end

global func genrealmem(r64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_mem

	if size=8 then
		a.value:=getrealindex(x)
	else
		a.value:=getr32index(x)

	fi
	a.valtype:=label_val
	a.size:=size
	return a
end

global func genrealimm(r64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=real_val
	a.size:=size
	return a
end

global func genmem(symbol d, int size=0)mclopnd a=
	int reg

	if size=0 then
		size:=min(ttsize[d.mode], 8)
	fi

!	IF D.MX and ttsize[d.mode]=d.mx.size then
!!CPL "REUSE D.MX"
!		return d.mx
!	fi

	if d.reg then
!		if stdfloat[d.mode] then
!			return genxregvar(d)
!		else
!			return genregvar(d, mode)
!		fi
	fi

	reg:=rnone
	if isframex(d) then
!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!			return frameregtable[d.offset]
!		fi

		reg:=rframe
!		usedset.[rframe]:=1

	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
!	++d.nrefs
	a.valtype:=def_val

	if size then
		a.size:=size
	else
		a.size:=min(ttsize[d.mode], 8)
	fi
	if a.size=0 then a.size:=8 fi

!	IF D.MX=NIL THEN D.MX:=A FI

	return a
end

global func genreg0(int reg, size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size

IF SIZE=0 THEN MERROR("1:SIZE=0") FI
	return a
end

EXPORT func genxreg(int reg, size=8)mclopnd=
	mclopnd a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_reg
	a.reg:=reg
	a.size:=size
IF SIZE=0 THEN MERROR("2:SIZE=0") FI
	return a
end

global func genreg(int reg, int size=8)mclopnd a =

!	if stdfloat[mode] and reg>=xr0 then
	if reg>=xr0 then
		genxreg(reg, size)
	else
		if size=0 then size:=8 fi
!		usedset.[reg]:=1

!IF REG IN R10..R13 THEN REGSET[REG]:=1 FI

		if fuseregtable then
			return regtable[reg, size]
		fi
		return genreg0(reg, size)
	fi
end

!global func genregi(int reg, mode=ti64)mclopnd a =
!!	if fuseregtable then
!!		return regtable[reg, stdsize[mode]]
!!	fi
!	return genreg0(reg, stdsize[mode])
!end

global func genireg(int reg, size=8, offset=0)mclopnd=
	mclopnd a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=size
	a.offset:=offset

	return a
end

global func roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	if size iand 7=0 then return size fi
	return size+(8-(size iand 7))
end

global proc merroropnd(ichar mess, int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]", mess, opndnames_ma[opndtype]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mmpos>>24], mmpos iand 16777215)
end

global func mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_label, genlabel(lab))
end

global func mdefinelabel:int =
	genmc(m_label, genlabel(++mlabelno))
	mlabelno
end

global func genextname(ichar s)mclopnd=
	[64]char str
	symbol d
	static [20]symbol table
	static int ntable

	strcpy(&.str, s)
!	str[strlen(s)]:=0			!lose final *

	d:=findnamesym(str)

	if not d then
		d:=pcm_allocnfz(strec.bytes)

		d.name:=pcm_copyheapstring(&.str)
		d.nameid:=dllprocid
		d.isimport:=1
		addnamesym(d)
	fi

	return genmemaddr(d)
end

!global func genregvar(symbol d, int mode)mclopnd a=
!	a:=genreg(d.reg, mode)
!!	isregvar[d.reg]:=1
!
!	return a
!end

!global func genxregvar(symbol d)mclopnd a=
!	a:=genxreg(d.reg)
!!	isxregvar[d.reg]:=1
!
!	return a
!end

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
		genmc(m_sub, dstackopnd, genint(n))
	fi
end

global proc popstack(int n)=
	if n then
		genmc(m_add, dstackopnd, genint(n))
	fi
end

global func getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if cstringlist and eqstring(cstringlist.svalue, s) then
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
	return addconst(creallist, cast@(x, int))
end

global func getr32index(real x)int=
	return addconst(cr32list, cast@(x, int))
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

global func findnamesym(ichar s)symbol d=
!search for s in cache of named symbols

	for i to nnametable do
		if eqstring(s, nametable[i].name) then
			return nametable[i]
		fi
	od
	nil
end

global proc addnamesym(symbol d)=
!add new name symbol, which should be unique

	if nnametable<nametable.len then
		nametable[++nnametable]:=d
	else
		merror("Ext nametab overflow")
	fi
end

global proc callproc(ichar cpname, name, int lineno)=
RETURN
end

func genstringx(ichar s)mclopnd=
	genlabelmem(getstringindex(s))
end

global proc clearreg(mclopnd ax)=
	if ax.size=8 then
		ax:=changeopndsize(ax, 4)
	fi
	genmc(m_xor, ax, ax)
end

global proc genstringtable=
	ref constrec p

	return unless cstringlist

	mcomment("String Table")

	setsegment('I', 8)

!	if kk0used then
!		genmc(m_label, genlabel(kk0used))
!		gendb(0)
!	fi

	p:=cstringlist
	while p, p:=p.nextconst do
		genmc_label(m_label, p.labelno)
		genstring_db(p.svalue, strtype:0)
	od
end

global proc mcomm(ichar s, t="", u="")=
	[256]char str
	print @str, s, t, u
	mcomment(pcm_copyheapstring(str))
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
	genmc(m_db,genint(a))
end

proc gendbstring(ichar s, int length)=
!string is printable, and doesn't include double quotes
	genmc(m_db,genstring(s,length))
end

global func gentemp(int n, size)mclopnd a=
!pcl temps are used to spill pcl operands from a register
!they will always be 64 bits

	int reg

	if pcltempflags[n] then			!already in use
		return changeopndsize(pcltempopnds[n], size)
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=rframe
!	usedset.[rframe]:=1
	a.valtype:=temp_val
	a.size:=size
	a.tempno:=n

	pcltempopnds[n]:=a
	pcltempflags[n]:=1

	return a
end

global proc genrealtable=
	ref constrec p

	return unless creallist or cr32list

	mcomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc(m_label,genlabel(p.labelno))

		if p.xvalue=infinity then
			genmc(m_dq, genint(u64@(p.xvalue)))
		else
			genmc(m_dq, genrealimm(p.xvalue, 8))
		fi
	od

	mcomment("Real32 Table")
	p:=cr32list
	while p, p:=p.nextconst do
		genmc(m_label,genlabel(p.labelno))
		if p.xvalue=infinity then
			genmc(m_dd, genint(int@(r32(p.xvalue)),4))
		else
			genmc(m_dd, genrealimm(p.xvalue,4))
		fi

	od
end

global proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

!	if igetmsourceinfo then
!		lineno:=igetmsourceinfo(mmpos, filename, sourceline)
!		CPL =LINENO
!		CPL =FILENAME
!	else
CPL "NO LINE INFO"
		lineno:=0
		filename:="?"
!	fi

!	if currfunc then
!		println "Proc:", currfunc.name
!	fi

	fprintln "MCL Error: # (#) on Line: # in #",mess,param, lineno, filename
OS_GETCH()
	stop 1
!	pcerrorstop(filename, lineno)
end

global func getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

!global func gentemp(int n, mode)mclopnd a=
!!pcl temps are used to spill pcl operands from a register
!!they will always be 64 bits
!	mgentemp(n, stdsize[mode])
!end

global func mgenint(i64 x, int mode=ti64)mclopnd a=
	genint(x, ttsize[mode])
end

global func mgenrealmem(r64 x, int mode=tr64)mclopnd a=
	genrealmem(x, ttsize[mode])
end

global func mgenrealimm(r64 x, int mode=tr64)mclopnd a=
	genrealimm(x, ttsize[mode])
end

global func mgenmem(symbol d, int mode=tvoid)mclopnd a=
	genmem(d, ttsize[mode])
end

global func mgenreg(int reg, int mode=tu64)mclopnd a =
	genreg(reg, ttsize[mode])
end

global func mgenireg(int reg, mode=tu64, offset=0)mclopnd=
	genireg(reg, ttsize[mode])
end

global func mgentemp(int n, mode)mclopnd a=
	gentemp(n, ttsize[mode])
end

=== mm_help.txt 0 1 29/29 ===
M Compiler for 64-bit Windows

Normal use:           Compiles lead module prog.m to:

    mm      prog      prog.exe (default)
    mm -r   prog      in-memory native code then execute
    mm -i   prog      in-memory IL then interpret

    mm -exe prog      prog.exe
    mm -dll prog      prog.dll
    mm -obj prog      prog.obj
    mm -a   prog      prog.asm
    mm -n   prog      prog.nasm
    mm -mx  prog      prog.mx
    mm -p   prog      prog.pcl (textual IL)
    mm -ma   prog     prog.ma (single amalgamated source file)

Other options:

    -ext              Used std headers external to compiler
    -opt              Optimise native code
    -out:file         Name output file (extension can be added)
    -rip              Use RIP address modes
    -himem            Generate PIC code (automatic with -obj/-dll)
    @file             Read files and options from a file
=== END ===
1 mm.m 0 0
2 mm_decls.m 0 0
3 mm_cli.m 0 0
4 mm_lex.m 0 0
5 mm_parse.m 0 0
6 mm_name.m 0 0
7 mm_type.m 0 0
8 mm_diags.m 0 0
9 mm_export_dummy.m 0 0
10 mm_libsources_dummy.m 0 0
11 mm_modules.m 0 0
12 mm_support.m 0 0
13 mm_tables.m 0 0
14 mm_lib.m 0 0
15 pcl_decls.m 0 0
16 pcl_gen.m 0 0
17 pcl_block.m 0 0
18 pcl_api.m 0 0
19 pcl_diags.m 0 0
20 mcl_decls.m 0 0
21 mcl_gen.m 0 0
22 mcl_genaux.m 0 0
23 mcl_stack.m 0 0
24 mcl_block.m 0 0
25 mcl_blockaux.m 0 0
26 mcx_decls.m 0 0
27 mcx_asm.m 0 0
28 mcx_lib.m 0 0
29 mm_help.txt 0 1
