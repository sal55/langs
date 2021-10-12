import mm_decls
import mm_tables
!import mm_pclcommon
import* mm_pcl

global const maxmodule=200
global const maxlibfile=50
global const maxsourcefile=1000

global type unit   = ref unitrec
global type symbol = ref strec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =		!should be 16-byte record
	byte symbol
	byte subcode
	word16 spare
	word32 pos: (lineno:24, fileno:8)

	union
		ref strec symptr		!pointer to symbol table entry for name
		int64 value				!64-bit int
		real xvalue				!64-bit float
		word64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
!		ref qint qvalue			!128-bit int/word
		ref int128 pvalue128	!128-bit int/word
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

global record dllprocrec =
	ichar name
	ref proc address
	int dllindex
end

global record procrec =
	symbol def
	ref procrec nextproc
end

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
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
	word32 pos: (lineno:24, fileno:8)
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record strec =
	ichar name
	ref strec owner
	ref strec deflist
	union
		ref strec deflistx
!		ref pstrec pdef
		ref fwdrec fwdrefs	!fwd ref chain
	end
	ref strec nextdef
	ref strec nextdupl
	ref strec firstdupl			!point to generic version
	psymbol pstdef

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

	word32 pos: (lineno:24, fileno:8)
	word16 flags: (
		isglobal:2,
		isstatic:1,
		used:1,
		txdone:1,
		circflag:1,
		islet:1,

		iscallback:1,
		addrof:1,
		noreg:1,
		equals:1,
		isequivtarget:1,
		atfield:1,
		atvar:1,
		isimport:1)
	byte moduleno
	byte reg

	union
		struct				!when a proc
			ichar truename			!for imported name only
			ref strec paramlist

			byte asmused			!1 when proc contains asmcode
			byte dllindex			!for dllproc: which dll in dlltable
			byte extmodno			!for proc call chains: module no of proc owner
!			byte simplefunc
			byte fflang				!0 windowsff. etc.
			byte nretvalues			!function: number of return values (0 for proc)
			byte varparams			!0 or 1; variadic params in B and FF
			byte isthreaded			!0 or 1; variadic params in B and FF
		end

		struct				!when a record or record field
			ref strec equivfield
			uflagsrec uflags
			int32 baseclass
			byte bitfieldwidth		!width of bitfield in record
			byte align				!0, 2, 4, 8, 16 or 255 (auto-align)
!			byte at					!0, or 1 for @ fields, 2 for @ frame/static
			byte bitoffset		!0..31 for bitfields in records
		end

		struct				!when a param name
			ref strec nextparam
			byte parammode			!0=var_param, in_param, out_param
			byte optional			!0 or 1	
			byte variadic			!variadic parameter for B code
		end

		struct				!when a static or frame name
			unit equivvar
!			symbol equivvar
!			byte equals				!0 or 1 if @ used (static/frame vars only)
		end

		struct						!macro param
			ref void macro_dummy	!needs nextparam
			ref strec nulldef		!generic st entry
		end

		struct						!when a tagged union
			int32 enumtagmode
		end


		[24]byte dummy
	end

!	int32 size
	int16 nrefs
!	int16 pmode

	union
		int16 stindex		!label pass 2: 0, or 1-based index within coff symboltable
		int16 maxalign		!for record types (doesn't fit above)
		int16 regsize
	end
	byte reftype			!AX fields
	byte segment
	int16 importindex
!	[2]BYTE PADDING

end

global record unitrec =
	byte tag				!kcode tag number
	byte hasa, hasb, hasc	!whether .a, .b or .c points to a unit/unitlist
!	int32 lineno			!source lineno associated with item; fileno is in top byte
	word32 pos: (lineno:24, fileno:8)

	unit nextunit

	union
		struct
			union
				unit			a
				symbol		def
				symbol		labeldef
				int64			value
				word64			uvalue
				real64			xvalue
				ichar			svalue
				int64			range_lower
			end

			union
				unit			b
				int64			range_upper
			end
		end
		int128					value128
		word128					uvalue128
		struct
			word64				low128
			word64				high128
		end
	end

	union
		unit			c
		[4]int16		cmppclmode
	end

	union						!misc stuff depends on tag
		struct					!const string
			word32 slength
			byte isastring
		end

		struct					!name
			byte dottedname		!for j_name: 1=resolved from fully qualified dotted seq
			byte avcode			!j_name for/autovars: 'I','T','S' = index/to/step autovars
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
		byte addroffirst	!1 for j_nameaddr when derived from &.name

		word32 offset			!for j_dot
		byte ifretflag		!1 when 'if' in return value path (force "if" in C target)
		int32 whenlabel		!label no associated with when expr; for recase

		int32 trylevel

		struct
			union
!!				int16 opindex		!op_add_i64 etc
				int16 bitopindex	!
				int16 opcindex		!operator nodes
				int16 fnindex		!sysfn_add_var etc
				int16 condcode		!pcl_eq etc; for j_eq etc
				int16 asmopcode		!for j_assem
				int16 bfcode
			end
		end
		int32 index
		[4]byte cmpgenop			!cmpchain: up to 8 genops
	end

	int32 mode
	union
		int32 convmode	!convert/typepun: target mode (will be widened to give unit mode)
		int32 memmode	!name/ptr/index/dot: void=LVALUE; non-void=RVALUE
	end
	byte moduleno
	byte initlet		!1 for an assignment that initialises a let
	byte isconst		!1 for j_const, and j_makerange with const range
	byte resultflag		!1 when the result of this unit is needed; 0=void or discarded
	byte pclop			!generic operator for j_bin, incr etc
	byte pclmode		!eg kadd:i64

	byte istrueconst	!1 for actual "123" etc, not result of reduction
	byte spare
end


global record modulerec =
	ichar name
	symbol stmodule
	int fileno
	union
		ichar asmstr
		ichar clangstr
	end
	int strlength
	[maxmodule]byte importmap
	[maxmodule]byte importstar
	symbol stinitproc
	ref tokenrec tklist
!	int16 moduleno
end

global const maxsearchdirs=10
global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0

global symbol stprogram		!root into the symbol table
global symbol stmodule		!main module
global symbol stsysmodule	!optional sys module (needed for name resolving)
global symbol alldeflist		!link together all (user) symbols

global tokenrec lx				!provides access to current token data
global tokenrec nextlx			!provides access to next token

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]ichar inputfiles
global [0..maxlibfile]ichar libfiles
global [0..maxsourcefile]ichar sourcefilenames
global [0..maxsourcefile]ichar sourcefilepaths
global [0..maxsourcefile]ichar sourcefiletext
global [0..maxsourcefile]int sourcefilesizes
global [0..maxsourcefile]byte issupportfile
global int nmodules
global int nsourcefiles
global int ninputfiles
global int nlibfiles

!.ma file directory
global [0..maxsourcefile]ichar mafilenames
global [0..maxsourcefile]int mafilesizes
global [0..maxsourcefile]int mafileoffsets
global [0..maxsourcefile]ichar mafiletext
global [0..maxsourcefile]byte mafilefileno			!0 or index into sourcefile tables
global [0..maxsourcefile]byte mafilesupport			!1 means support file eg. for strinclude
global int nmafiles
global ichar mafilesource

global symbol currmodule
global int currmoduleno				!used when compiling modules

global const int maxtype=6'000

global int ntypes

global [0..maxtype]symbol ttnamedef
global [0..maxtype]symbol	ttowner		!for	ttlowerexpr/rtlengthexpr

global [0..maxtype]int32	ttbasetype		!basetype
global [0..maxtype]ichar	ttname

global [0..maxtype]int32	ttsize
global [0..maxtype]byte	ttsizeset
global [0..maxtype]int32	ttlower 		!.lbound (default 1)
global [0..maxtype]int32	ttlength 		!elements in array/record/tuple
global [0..maxtype]ref[]int32	ttmult 	!ttlength elements in tuple

global [0..maxtype]unit	ttdimexpr		!length, lower:length, or lower..upper

global [0..maxtype]int32	tttarget 		!for array/ref types
global [0..maxtype]int32	ttkeytype 		!for dict
global [0..maxtype]byte	ttusercat
global [0..maxtype]int32	ttlineno

!global [0..maxtype]byte	ttpcltype
!global [0..maxtype]byte	ttcat
!global [0..maxtype]byte	ttcat2

global [0..maxtype]byte	ttisint			!is i8 i16 i32 i64 i128
global [0..maxtype]byte	ttisword			!is u8 u16 u32 u64 u128
global [0..maxtype]byte	ttisreal			!is r32 r64
global [0..maxtype]byte	ttisinteger		!is i8..i64/u8..u64/c8..c64
!global [0..maxtype]byte	ttisnumeric		!is int/word/char/real
global [0..maxtype]byte	ttisallnum		!all numeric types including short/decimal
global [0..maxtype]byte	ttismainnum		!all numerics excl short
global [0..maxtype]byte	ttisshort		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte	ttisref			!is a pointer

!global const int maxtypename=4'000
!global const int maxtypename=8'000
global const int maxtypename=12'000
global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..maxtype]byte typestarterset

global symbol currproc

global int alineno=0

global int debug=0
global int assemmode=0

global ref procrec proclist,proclistx			!linked list of all procs
!global int nproclist
global ref procrec staticlist,staticlistx		!linked list of all static
!global int nstaticlist
global ref procrec constlist,constlistx		!linked list of all export consts
!global int nconstlist

global const maxmodulemap=25
global [maxmodulemap]ichar genericmodules
global [maxmodulemap]ichar actualmodules
global int nmodulemap

global unit nullunit

global int targetbits=64
global int targetsize=8

global [20]ichar docstrings
global int ndocstrings

global const maxdlllib=50
global const maxdllproc=500

global int ndllnametable
global int ndllproctable
global [maxdlllib]ichar dllnametable
global [maxdlllib]word64 dllinsttable
global [maxdllproc]dllprocrec dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global int msyslevel=2		!0/1/2 = none/min/normal
global byte fvarnames=0		!display of names in asm/mcl

global byte fbundled=0		!1 when .ma file is being compiler
global byte fwritema
global byte fwriteexports
global byte fwritedocs

global byte fexe
global byte fobj
global byte fwritelibs
global byte fshowtiming
global byte fshowss
global byte fshowpcl
global byte fshowasm
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
!global byte fshowpst
global byte fshowstflat
global byte fshowtypes
global byte fshowoverloads
global byte foptim
global byte fcheckunusedlocals=0
global byte fwindows
global byte flinux
global byte fx64
global byte fssonly
global byte fnofile
global byte fdorts=1
!global byte fshortnames

global byte dointlibs=1

!pcl/mcl-pass are relevant only for x64 target, and not allowed for 
global tabledata() []ichar passnames =
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
	(dll_pass,		$),		!
	(run_pass,		$),		!will do up to .exe then run the .exe
	(clang_pass,	$),
end

!passlevel used for compiler debug only
global int passlevel=0
global int prodmode=0
global int debugmode=0

global ichar outfile					!one of the following two
global ichar destfilename				!nil, or sets outfile
global ichar destfilepath				!nil, or sets path (can't be mixed with destfilename)
global ichar asmfilename				!set from outfile
global ichar pclfilename				!
global ichar exefilename				!
global ichar dllfilename				!
global ichar objfilename				!
global ichar mafilename					!
global ichar expfilename				!

global symbol extendtypelist

global [0:jtagnames.len]ref overloadrec overloadtable

global const maxcclibs=10				!libs passed to gcc/tcc
global [maxcclibs]ichar cclibtable
global int ncclibs

global ichar infotext					!mainprog.txt for c target; nil if not used

!GLOBAL INT NINDEX
!GLOBAL INT NFOR
!GLOBAL INT NTO
!GLOBAL INT NWHILE
!GLOBAL INT NDO
!GLOBAL INT NREPEAT
!GLOBAL INT NDOSWITCH
!GLOBAL INT NDOCASE
!GLOBAL INT NFUNCTIONS
!GLOBAL INT NPROCs
!GLOBAL INT NGOTO
!GLOBAL INT NEXIT
!GLOBAL INT NLONGEST
!GLOBAL [256]char LONGESTNAME
!GLOBAL INT NGENERIC
GLOBAL INT NUNITS
!

