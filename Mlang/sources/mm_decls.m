global const maxmodule=200
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=1000

global type symbol = ref strec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =		!should be 16-byte record
	byte symbol
	byte subcode
	word16 spare
	word32 pos: (sourceoffset:24, fileno:8)

	union
		ref strec symptr		!pointer to symbol table entry for name
		int64 value				!64-bit int
		real xvalue				!64-bit float
		word64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
	end
end

global record overloadrec =
	int32 amode
	int32 bmode
	int32 rmode
	int16 moduleno
	int16 flags
	unit fncode
	ref overloadrec nextoverload
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

global record strec =
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
		iscallback:1,
		addrof:1,
		noreg:1,
		ishandler:1,

!		equals:1,
		isequivtarget:1,
		atfield:1,
		atvar:1,

		isfloat:1,
		isimport:1,
		isqproc:1)

	byte moduleno
	byte subprogno

	unit equivvar

	struct				!when a proc
		ichar truename			!for imported name only
		ref strec paramlist

		byte asmused			!1 when proc contains asmcode
		byte dllindex			!for dllproc: which dll in dlltable
		byte fflang				!0 windowsff. etc.

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

	ref strec nextsym
	int16 impindex
	int16 expindex
	byte reg

	byte scope
	byte equals			!for vars/params: 1/2/3 means =/:=/::= used

end

global type unit   = ref unitrec

global record unitrec =
	byte tag				!jcode tag number
	[3]byte spare
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
		array[3]unit abc
	end

	union						!misc stuff depends on tag
		struct					!const string
			word32 slength
			byte isastring
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

global record modulerec =
	ichar name
	symbol stmodule
	symbol stsubprog
	ichar path			!path where module source file resides
	symbol ststart		!nil, or st entry of local start/main
	symbol stmain
	symbol stmacro		!will be turned into a macro
	unit modulecode		!any code outside of a module
	int16 fileno		!sourcefile table index once loaded
	int16 issyslib		!1 if system lib (different search rules)
	int16 subprogno

end

global record subprogrec =
	ichar name
	symbol stsubprog
	int issyslib		!1 if system lib (different search rules)
	ichar path			!path where import module source file resides
	int16 firstmodule
	int fileno
end

global symbol stprogram		!root into the symbol table
global symbol stmodule		!main module
global symbol stsubprog
global symbol stsysmodule	!optional sys module (needed for name resolving)
global symbol alldeflist		!link together all (user) symbols
global int currmoduleno				!used when compiling modules

global tokenrec lx				!provides access to current token data
global tokenrec nextlx			!provides access to next token

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]byte moduletosub				!convert module no to subprog no
global [0..maxsubprog]subprogrec subprogtable
global [0..maxsubprog]byte subproghasstart

global [0..maxlibfile]ichar libfiles
global [0..maxlibfile]byte libtypes

global [0..maxsourcefile]ichar sourcefilespecs		!full path
global [0..maxsourcefile]ichar sourcefilepaths		!path only
global [0..maxsourcefile]ichar sourcefilenames		!base filename only
global [0..maxsourcefile]byte sourcefilesys			!1 if a system module
global [0..maxsourcefile]byte sourcefilesupport		!1 is a support file:strinclude etc
global [0..maxsourcefile]ichar sourcefiletext		!text
global [0..maxsourcefile]ichar sourcefiledupl		!copy for ma file only
global [0..maxsourcefile]int sourcefilesizes
global int nmodules
global int nsubprogs
global int nsourcefiles
global int nlibfiles
global int mainmoduleno

global const int maxtype=6'000

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
global symbol currsubprog

!global int alineno=0

global int debug=0
global int assemmode=0
global int headermode=0

global ref procrec proclist,proclistx			!linked list of all procs
global ref procrec staticlist,staticlistx		!linked list of all static
global ref procrec constlist,constlistx		!linked list of all export consts

global unit nullunit, voidvarunit

global int targetbits=64
global int targetsize=8

global [20]ichar docstrings
global int ndocstrings

global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]symbol dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global byte msyslevel=2		!0/1/2 = none/min/normal
global byte mvarlib=0		!0/1 = none/yes
global byte fvarnames=0		!display of names in asm/mcl
global byte minos=0			!1 enables simpler windows module

global byte freadma			!1 when .ma file is being compiler
global byte fwritema
global byte fwriteexports
global byte fwritedocs

global byte fexe
global byte fobj
global byte fwritelibs
global byte fshowtiming
global byte fshowss
global byte fshowmx
global byte fshowpcl
global byte fshowasm
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte fshowoverloads
global byte fshowmodules
global byte fpeephole
global byte fregoptim
global byte fcheckunusedlocals=0
global byte fwindows
global byte flinux
global byte fx64
global byte fssonly
global byte fnofile
global byte ffuntab

global byte dointlibs=1

!pcl/mcl-pass are relevant only for x64 target, and not allowed for 
global enumdata []ichar passnames =
	(header_pass,	$),
	(load_pass,		$),
	(parse_pass,	$),
	(fixup_pass,	$),
	(name_pass,		$),
	(type_pass,		$),
	(pcl_pass,		$),
	(mcl_pass,		$),		!all-inclusive up to this point (includes all prev passes)
	(asm_pass,		$),		!only one of these 3 will be done
	(obj_pass,		$),		!
	(exe_pass,		$),		!
	(lib_pass,		$),		!
	(run_pass,		$),		!will do up to .exe then run the .exe
	(clang_pass,	$),
end

!passlevel used for compiler debug only
global int passlevel=0
global int prodmode=0
global int debugmode=0
global int libmode=0					!1 means eventual ML/LIB target
global int mxstub=0						!1 to write prog.exe with run.exe+prog.mx

global ichar outfile					!one of the following two
global ichar destfilename				!nil, or sets outfile
global ichar destfilepath				!nil, or sets path (can't be mixed with destfilename)
global ichar asmfilename				!set from outfile
global ichar pclfilename				!
global ichar exefilename				!
global ichar libfilename				!
global ichar objfilename				!
global ichar mafilename					!
global ichar expfilename				!

global symbol extendtypelist

global [0:jtagnames.len]ref overloadrec overloadtable

global int nunits

global const langnameuc		= "M"
global const langname		= "m"
global const langext		= "m"
global const langextma		= "ma"
global const langextmauc	= "MA"
global const langlibname	= "mlib"
global const langhomedir	= "C:/mx/"
global const langhelpfile	= "mm_help.txt"
