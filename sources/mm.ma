mafile 35
  1 mm.m                 436     1395
  2 mm_decls.m         13616     1856
  3 clibnew.m           3395    15496
  4 mm_tables.m        45522    18917
  5 mm_mcldecls.m      13389    64467
  6 mm_start.m         19959    77881
  7 msysnew.m          46777    97864
  8 mlib.m             26301   144662
  9 oswindows.m        12880   170989
 10 mm_support.m       12861   183897
 11 mm_lib.m           39571   196782
 12 mm_lex.m           37051   236377
 13 mm_diags.m         12826   273454
 14 mm_genwx64.m        3488   286308
 15 mm_genpcl.m         9624   289823
 16 mm_libpcl.m        23175   299474
 17 mm_blockpcl.m      66526   322678
 18 mm_genmcl.m        77406   389231
 19 mm_libmcl.m        41720   466664
 20 ma_genss.m         46492   508410
 21 ma_decls.m          1820   554928
 22 ma_lib.m            2306   556772
 23 ma_objdecls.m       2922   559107
 24 ma_writeobj.m       7796   562058
 25 ma_writeexe.m      25620   569883
 26 ma_disasm.m        26047   595530
 27 mm_parse.m         85295   621603
 28 mm_name.m          16641   706923
 29 mm_type.m          63047   723589
 30 msysnew.m          46777   786661
 31 mlib.m             26301   833460
 32 clibnew.m           3395   859786
 33 oswindows.m        12880   863208
 34 oswindll.m          2135   876114
 35 mm_help.txt          865   878276
=== mm.m 1/35 ===
!mapmodule mm_sys => mm_sysnew
mapmodule mm_gen => mm_genwx64
mapmodule mm_oslib => mm_oswindows
!mapmodule mm_msys => mm_msys

!mapmodule mm_diags => mm_diags_dummy

import mm_decls
import mm_start

proc start=

!CPL =UNITREC.BYTES

	addmodulemapping("msys","msysnew")
	addmodulemapping("clib","clibnew")
	addmodulemapping("oslib","oswindows")
	addmodulemapping("osdll","oswindll")

	start_common(wx64_target)
end
=== mm_decls.m 2/35 ===
import clib
import mm_tables

import mm_mcldecls

global record fwdrec =
	var ref fwdrec nextfwd
	var int32 offset
	var int16 reltype
	var int16 seg
end


global macro ttelemtype = tttarget
global type unit = ref unitrec

global const maxmodule=50
global const maxlibfile=50
global const maxsourcefile=250

global record lexrec =		!should be 32-byte record
	union
		var int64 value				!64-bit int
		var real xvalue				!64-bit float
		var word64 uvalue			!64-bit word
		var ichar svalue			!pointer to string or charconst (not terminated)
		var ref qint qvalue			!128-bit int/word
	end
	var ref strec symptr		!pointer to symbol table entry for name

	var int32 hashvalue
	var int32 length					!length of name/string/char
	var int32 lineno

	var byte symbol
	var byte subcode
	var byte fileno
	var byte spare
end

global record uflagsrec =
	var [7]byte	codes
	var byte	ulength
end

global record fieldrec =
	var ichar name
	var int16 recordtype
	var int16 fieldtype
	var int32 fieldoffset
end

global record qint =	!store 128-bit signed or unsigned value
	var word64 lower
	var int64  upper
end

global record strec =
	var ichar name
	var ref strec owner
	var ref strec deflist
	var ref strec deflistx
	var ref strec nextdef
	var ref strec nextdupl
	var ref strec firstdupl			!point to generic version

	union
		var ref strec nextparam
	end
	var ref unitrec code
	union
		var ref strec paramlist
		var uflagsrec uflags
	end
	union
		var ref strec equivfield
		var ref unitrec equivvar
	end
	union
		var ichar truename			!for imported name only
		var ichar metadata
		var ichar macrovalue
		var ref strec nulldef		!macroparam: point to generic st entry
	end

	union
		var [4]int32 modelist
		var int32 mode
	end

	var byte namelen
	var byte symbol
	var byte nameid
	var byte txdone

	var int32 subcode
	union
		var int32 index					!needs to hold pcindex (also indices for all symbols or .bc files)
		var int32 labelno				!for mcl anonymous labels; and for proc labels?
	end
	union
		var int32 offset
		var int32 base_class
	end
	var int32 lineno
	var byte isglobal				!0/1/2 = local/global/export
	var byte isstatic				!0 or 1
	var byte equals				!0 or 1 if @ used (static/frame vars only)
	var byte at					!0 or 1 if @ used (fields only)
	var byte parammode			!0=var_param, in_param, out_param
	var byte optional			!0 or 1	
	union
		var byte varparams		!0 or 1	
		var byte bitoffset		!0..31 for bitfields in records
	end
	var byte used				!0 or 1 if referenced in an expression
	union
		var byte asmused			!1 when proc contains asmcode
		var byte bitfieldwidth	!width of bitfield in record
	end
	var byte circflag

	var byte fflang				!0 windowsff. etc.
	var byte moduleno
	var byte imported			!1=imported name; 0=local or global name
	var byte nretvalues			!function: number of return values (0 for proc)
	var byte namecat
	union
		var byte align			!0, 2, 4, 8, 16 or 255 (auto-align)
		var byte dllindex		!for dllproc: which dll in dlltable
		var byte extmodno		!for proc call chains: module no of proc owner
	end
	var byte islet
	var byte simplefunc

	var byte reftype			!AX fields
	var byte segment
	var ref fwdrec fwdrefs	!fwd ref chain
	var int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
	var int32 importindex
	var int32 regsize

end

global record unitrec =
	var int32 tag			!kcode tag number
	var int32 lineno			!source lineno associated with item; fileno is in top byte

	var ref unitrec nextunit

	union
		var ref strec def
		var int64 value
		var word64 uvalue
		var real xvalue
		var ichar svalue
		var ref qint qvalue
		var ref strec labeldef
		struct
			var int64 range_lower
			var int64 range_upper
		end
		struct
			var word64 value_lower
			var int64 value_upper
		end
		struct
			var word64 uvalue_lower
			var word64 uvalue_upper
		end
	end
	union
		var int32 opcode
		var int32 index
		var int32 whenlabel		!label no associated with when expr; for recase
		var int32 trylevel
		var int32 slength
		var int32 length			!for makelist
		var byte dottedname		!for j_name: 1=resolved from fully qualified dotted seq
		var int32 offset			!for j_dot
		struct
			var byte reg			!for assemreg/xreg/mem
			var byte regix
			var byte scale
			var byte prefixmode
			var byte regsize
			var byte cond
			var byte spare2,spare3
		end
		var word64 reginfo
	end

	var int32 mode
	var int32 moduleno
	var int32 addroffirst	!1 for j_nameaddr when derived from &.name
	var int32 isconst		!1 for j_const, and j_makerange with const range
	var int32 popflag		!1 when this unit needs to be popped after evaluation.
	var int32 ifretflag		!1 when 'if' in return value path (force "if" in C target)
	var int32 newmode
	var int32 isastring		!1 when a string const

	var ref unitrec a	!single items, or linked lists
	var ref unitrec b
	var ref unitrec c
end

global record modulerec =
	var ichar name
	var ref strec stmodule
	var int fileno
	union
		var ichar asmstr
		var ichar clangstr
	end
	var int strlength
	var [maxmodule]byte importmap
	var ref strec stinitproc
end

global record dllprocrec =
	var ichar name
	var ref proc address
	var int dllindex
end

global record procrec =
	var ref strec def
	var ref procrec nextproc
end

global record userxrec =
	var ref strec owner
	var ref int32 pmode
	var ref userxrec nextmode
end

global const int maxtype=4000
global const int maxuserxtype=4000

global var int ntypes
global var int nuserxtypes
global var int userxtypebase			!first index (growing downwards) of userxtypes in current module
global var ref userxrec userxmodelist	!list of all references to userx modes

global var [0:maxtype]int32 ttmodule		!module number
global var [0:maxtype]ref strec ttnamedef
global var [0:maxtype]ref strec ttowner		!for ttlowerexpr/rtlengthexpr

global var [0:maxtype]int32 ttbasetype		!basetype
global var [0:maxtype]ichar ttname
global var [0:maxtype]byte tttypecat

global var [0:maxtype]byte ttbitwidth
global var [0:maxtype]int32 ttsize
global var [0:maxtype]byte ttsizeset
global var [0:maxtype]int32 ttlower 		!.lbound (default 1)
global var [0:maxtype]int32 ttlength 		!elements in array/record (actual fields) (/string

global var [0:maxtype]unit ttdimexpr		!length, lower:length, or lower..upper

global var [0:maxtype]int32 tttarget 		!for array/ref types
global var [0:maxtype]int32 ttkeymode 		!for dict, and for some array-index exprs
global var [0:maxtype]byte ttusercat
global var [0:maxtype]int32 ttlineno

global var [0:maxtype]byte ttisint			!is i8 i16 i32 i64 i128
global var [0:maxtype]byte ttisword			!is u8 u16 u32 u64 u128
global var [0:maxtype]byte ttischar			!is c8 c16 c64
global var [0:maxtype]byte ttiswordchar		!is u8..u64/c8..c64
global var [0:maxtype]byte ttisreal			!is r32 r64
global var [0:maxtype]byte ttisinteger		!is i8..i64/u8..u64/c8..c64
global var [0:maxtype]byte ttisnumeric		!is int/word/char/real
global var [0:maxtype]byte ttisshortint		!is i8/i16/i32/u8/u16/u32/c8/c16
global var [0:maxtype]byte ttisshortreal	!is i8/i16/i32/u8/u16/u32/c8/c16
global var [0:maxtype]byte ttisbit			!is u1/u2/u4
global var [0:maxtype]byte ttisref			!is a pointer
global var [0:maxtype]byte tttypecode		!'I','U','R','P' (U is word/char)
global var [0:maxtype]byte ttisflex
global var [0:maxtype]byte ttisvariant
global var [0:maxtype]byte ttisflexvar

global var [0:maxtype]byte typestarterset

global var [0:maxuserxtype]ref strec ttnamedefx
global var [0:maxuserxtype]ref strec ttnamedefx2
global var [0:maxuserxtype]int ttlinenox
global var [0:maxuserxtype]int ttxmap
global var [0:maxuserxtype]byte ttxmoduleno

global var [0:symbolnames.len]byte exprstarterset

global var [0..maxmodule]modulerec moduletable
global var [0..maxmodule]ichar inputfiles
global var [0..maxlibfile]ichar libfiles
global var [0..maxsourcefile]ichar sourcefilenames
global var [0..maxsourcefile]ichar sourcefilepaths
global var [0..maxsourcefile]ichar sourcefiletext
global var [0..maxsourcefile]int sourcefilesizes
global var int nmodules
global var int nsourcefiles
global var int ninputfiles
global var int nlibfiles

!.ma file directory
global var [0..maxsourcefile]ichar mafilenames
global var [0..maxsourcefile]int mafilesizes
global var [0..maxsourcefile]int mafileoffsets
global var [0..maxsourcefile]ichar mafiletext
global var [0..maxsourcefile]byte mafilefileno			!0 or index into sourcefile tables
global var [0..maxsourcefile]byte mafilemult			!1 means could be parsed multiple times
global var int nmafiles
global var ichar mafilesource

global var ref strec currmodule
global var int currmoduleno				!used when compiling modules

global const maxsearchdirs=10
global var [maxsearchdirs]ichar searchdirs
global var int nsearchdirs=0

global var ref strec stprogram		!root into the symbol table
global var ref strec stmodule		!main module
global var ref strec stsysmodule	!optional sys module (needed for name resolving)
global var ref strec alldeflist		!link together all (user) symbols

!global var filehandle logdev		!dest for diagnostics and output of tables
global var int optflag=0		!1=stdoptimise; 0=disabled

global var ref unitrec nullunit

global const maxlibpaths=10
global var [maxlibpaths]ichar libpaths
global var int nlibpaths

global var int mlineno=0		!set in pclgen dispatcher
global var int alineno=0

global var int debug=0
global var int assemmode=0

global var int totalstrings=0

global const maxdlllib=50
global const maxdllproc=500

global var int ndlltable
global var int ndllproctable
global var [maxdlllib]ichar dlltable
global var [maxdlllib]word64 dllinsttable
global var [maxdllproc]dllprocrec dllproctable

global const int maxcmdparam=32
global var int ncmdparams
global var [0..maxcmdparam]ichar cmdparamtable

global var ref procrec proclist			!linked list of all procs
global var int nproclist
global var ref procrec staticlist		!linked list of all static
global var int nstaticlist

global var ref strec currproc

global tabledata() []ichar modenames =
	(compile_mode,	$),
	(link_mode,		$),
	(run_mode,		$),
end

global tabledata() []ichar targetnames, []ichar targetosnames, []ichar targetlangnames,
		 []ichar targetexts,
		 []byte tg_ctarget, []byte tg_targetbits, []byte tg_islinux=
	(wx64_target,	"wx64",	"windows",	"x64",		"asm", 0, 64, 0),		!Windows/x64
	(wc64_target,	"wc64",	"windows",	"clang",	"c",   1, 64, 0),		!Windows/C64
	(lx64_target,	"lx64",	"linux",	"x64",		"asm", 0, 64, 1),		!Linux/x64
	(lc64_target,	"lc64",	"linux",	"clang",	"c",   1, 64, 1),		!Linux/C64
	(lc32_target,	"lc32",	"linux",	"clang",	"c",   1, 32, 1),		!Linux/C32
	(wc32_target,	"wc32",	"windows",	"clang",	"c",   1, 32, 0),		!Windows/C32
end

global tabledata() []ichar ccnames=
	(gcc_cc,		$),
	(tcc_cc,		$),
	(bcc_cc,		$),
end

!all set by user options
!global var int passlevel
global var int target
!global var int ostarget

global var int ctarget=0		!set to 1 when target=c32/c64
global var int islinux=0		!set to 1 when target=linux

global var int targetbits		!set to txbits[target]
global var int targetsize		!set to targetbits/8

global var int fverbose=0		!whether to display message for each pass
global var int fquiet=0
global var byte fdebugcompiler		!1 for debug compile, 0 (default) for production compile

!global var int foptimise=0		!whether to generate optimised j-codes

global var int fhpcounter=0
global var byte fnomsys
global var int fvarnames=0		!display of names in asm/mcl

global var int fbundled=0		!1 when .ma file is being compiler
global var ichar mafilename
global var int fwritema

global var int fexe
global var int fobj
global var int fwritelibs
global var int fshowtiming
global var int fshowss
global var int fshowpcl1
global var int fshowpcl2
global var int fshowmcl1
global var int fshowasm
global var int fshowast1
global var int fshowast2
global var int fshowast3
global var int fshowst
global var int fshowstflat
global var int fshowtypes
global var int ccompiler = gcc_cc
global var int foptimise
!var int fwritema


global var int fcheckunusedlocals=0

global var int dointlibs=1

global var lexrec lx				!provides access to current token data
global var lexrec nextlx

global var int labelno=0

global var ichar infotext					!mainprog.txt for c target; nil if not used

global var int NALLLINES
global var int prescanmode

global const maxmodulemap=25
global var [maxmodulemap]ichar genericmodules
global var [maxmodulemap]ichar actualmodules
global var int nmodulemap

global const maxcclibs=10				!libs passed to gcc/tcc
global var [maxcclibs]ichar cclibtable
global var int ncclibs

global var INT NSMALL
global var INT NALLNOS
global var INT NASMINT
global var INT NASMINTSMALL

GLOBAL VAR INT NPCL
GLOBAL VAR INT NMCL
GLOBAL VAR INT NUNITS

global var int cc_mode			!compile_mode/link_mode/run_mode

!passlevel used for compiler debug only
global var int passlevel=6		!1=parse, 2=name, 3=type, 4=gen1, 5=gen2, 6=asm, 7=link

global var ichar outfile					!one of the following two
global var ichar outfilesource				!.asm or .c filename
global var ichar outfilebin				!.exe or .obj filename
global var ichar destfilename				!nil, or sets outfilebin

global var ichar linkoption				!exe or obj

global var [sysfnnames.len]int sysfnlabels
global var [sysfnnames.len]int sysfnproclabels
=== clibnew.m 3/35 ===
global type filehandle=ref void

importlib cstd=
!	clang function malloc	(wordm)ref void
	clang function malloc	(word64)ref void
	clang function realloc	(ref void, wordm)ref void
	clang proc     free		(ref void)
	clang proc     memset	(ref void, int32, wordm)
	clang proc     memcpy	(ref void, ref void, wordm)
	clang function clock	:int32
	clang function ftell	(filehandle)int32
	clang function fseek	(filehandle, int32, int32)int32
	clang function fread	(ref void, wordm, wordm, filehandle)wordm
	clang function fwrite	(ref void, wordm, wordm, filehandle)wordm
	clang function getc		(filehandle)int32
	clang function ungetc	(int32, filehandle)int32
	clang function fopen	(ichar,ichar="rb")filehandle
	clang function fclose	(filehandle)int32
	clang function fgets	(ichar, int, filehandle)ichar
	clang function remove	(ichar)int32
	clang function rename	(ichar, ichar)int32
	clang function getchar	:int32
	clang proc     putchar	(int32)
	clang proc     setbuf	(filehandle, ref byte)

	clang function strlen	(ichar)wordm
	clang function strcpy	(ichar, ichar)ichar
	clang function strcmp	(ichar, ichar)int32
	clang function strncmp	(ichar, ichar, wordm)int32
	clang function strncpy	(ichar, ichar, wordm)wordm
	clang function memcmp	(ref void, ref void, wordm)int32
	clang function strcat	(ichar, ichar)ichar
	clang function tolower	(int32)int32
	clang function toupper	(int32)int32
	clang function isalpha	(int32)int32
	clang function isupper	(int32)int32
	clang function islower	(int32)int32
	clang function isalnum	(int32)int32
	clang function isspace	(int32)int32
	clang function strstr	(ichar, ichar)ichar
	clang function atol		(ichar)intm
	clang function atoi		(ichar)int32
	clang function strtod	(ichar,ref ref char)real64

	clang function puts		(ichar)int32
	clang function printf	(ichar, ...)int32

	clang function sprintf	(ichar, ichar, ...)int32
!	clang function __mingw_sprintf	(ichar, ...)int32

	clang function sscanf	(ichar, ichar, ...)int32
	clang function scanf	(ichar, ...)int32

	clang function rand		:int32
	clang proc     srand	(word32)
	clang function system	(ichar)int32

	clang function fgetc	(filehandle)int32
	clang function fputc	(int32,  filehandle)int32
	clang function fprintf	(filehandle, ichar, ...)int32
	clang function fputs	(ichar,  filehandle)int32
	clang function feof		(filehandle)int32
	clang function getch	:int32
	clang function kbhit	:int32
	clang function _mkdir	(ichar)int32
	clang function mkdir	(ichar)int32
	clang function dummy	(real)real
	clang function strchr	(ichar,int32)ichar

	clang proc     _exit	(int32)
	clang proc     "exit"	(int32)
!	clang proc     `exit	(int32)
	clang function	pow		(real,real)real

	clang function	`sin	(real)real
	clang function	`cos	(real)real
	clang function	`tan	(real)real
	clang function	`asin	(real)real
	clang function	`acos	(real)real
	clang function	`atan	(real)real
	clang function	`log	(real)real
	clang function	`log10	(real)real
	clang function	`exp	(real)real
	clang function	`floor	(real)real
	clang function	`ceil	(real)real

	clang proc      qsort   (ref void, word64, word64, ref proc)

end

importlib cstdextra=
	clang function __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

global const c_eof		=-1
global const seek_set	= 0
global const seek_curr	= 1
global const seek_end	= 2
=== mm_tables.m 4/35 ===
global tabledata() [0:]ichar stdtypenames, [0:]byte stdtypebits,
 				   [0:]byte stdtypecode, [0:]byte stdtypecat, [0:]byte stdtypebase =
	(tvoid=0,		"void",		  0,	  0,	tvoid,		0),

	(ti8,			"i8",		  8,	'I',	tshort,		ti64),
	(ti16,			"i16",		 16,	'I',	tshort,		ti64),
	(ti32,			"i32",		 32,	'I',	tshort,		ti64),
	(ti64,			"i64",		 64,	'I',	tscalar,	ti64),
	(ti128,			"i128",		128,	'I',	twide,		ti128),

	(tu8,			"u8",		  8,	'U',	tshort,		tu64),
	(tu16,			"u16",		 16,	'U',	tshort,		tu64),
	(tu32,			"u32",		 32,	'U',	tshort,		tu64),
	(tu64,			"u64",	 	 64,	'U',	tscalar,	tu64),
	(tu128,			"u128",		128,	'U',	twide,		tu128),

	(tr32,			"r32",		 32,	'R',	tshort,		tr32),
	(tr64,			"r64",		 64,	'R',	tscalar,	tr64),

	(tc8,			"c8",		  8,	'U',	tshort,		tc64),
	(tc16,			"c16",		 16,	'U',	tshort,		tc64),
	(tc64,			"c64",		 64,	'U',	tscalar,	tc64),

	(tflexdecimal,	"dec",		 64,	  0,	tflex,		tflexdecimal),

	(tu1,			"u1",		  1,	  0,	tshort,		ti64),
	(tu2,			"u2",		  2,	  0,	tshort,		ti64),
	(tu4,			"u4",		  4,	  0,	tshort,		ti64),

	(tref,			"ref",		 64,	'P',	tscalar,	0),

	(tany,			$,			  0,	  0,	tvoid,		0),		!any marks end of scalar types

	(trefbit,		"refb",		128,	'P',	twide,		0),
	(tsubrange,		"subrng",	 64,	'I',	tscalar,	0),
	(tenum,			"enum",		 64,	'I',	tscalar,	0),
	(tbitfield,		"bf",		  8,	 'U',	tshort,		0),		!record bitfields

	(tcomplex64,	"cx",		128,	  0,	twide,		0),

	(trange64,		"range",	128,	  0,	twide,		0),
	(tslice,		"sx",		128,	'A',	twide,		0),

	(tfixedarray,	"ax",		  0,	'A',	tblock,		0),
	(tfixedbits,	"bx",		  0,	'A',	tblock,		0),
	(tfixedset,		"set",		  0,	'A',	tblock,		0),
	(tfixedrecord,	"rec",		  0,	  0,	tblock,		0),

	(tflexclass,	"class",	 64,	  0,	tflex,		0),
	(tflexstring,	"str",		 64,	'A',	tflex,		0),
	(tflexwstring,	"wstr",		 64,	'A',	tflex,		0),
	(tflexarray,	"fax",		 64,	'A',	tflex,		0),
	(tflexbits,		"fbits",	 64,	'A',	tflex,		0),
	(tflextable,	"ftab",		 64,	'A',	tflex,		0),
	(tflexset,		"fset",		 64,	'A',	tflex,		0),
	(tflexdict,		"dict",		 64,	'A',	tflex,		0),

	(tproc,			"proc",		  0,	  0,	tproc,		0),

!The following are broad categories used by some PCL ops
!	(tflex,			"flex",		 64,	  0,	tflex,	0),			!managed objects (tscalar when unmanaged)

	(tshort,		"d124",		 64,	  0,	tshort,		0),		!1/2/4-byte storage types
	(tshortfloat,	"x4",		 64,	  0,	tshortfloat,0),	!2/4-byte floats
	(tscalar,		"d8",		 64,	  0,	tscalar,	0),		!normal 64-bit value
	(tscalarfloat,	"x8",		 64,	  0,	tscalarfloat,	0),	!64-bit intended for float ops
	(twide,			"d16",		128,	  0,	twide,		0),			!128-bit double reg value
	(twidefloat,	"x16",		128,	  0,	twidefloat,	0),	!128-bit for float ops

	(tblock,		"blk",		  0,	  0,	tblock,		0),		!N-byte and 3/5/6/7/9/10..15-byte fixed size typs
	(tflex,			"flex",		 64,	  0,	tflex,		0),			!managed objects (tscalar when unmanaged)

!This is one is a category /and/ a specific type
	(tvariant,		"var",		128,	'A',	tvariant,	0),		!managed, variant type (twide when unmanaged)

	(tgeneric,		$,		 	  0,	  0,	tvoid,		0),
	(tauto,			$,		 	  0,	  0,	tvoid,		0),
	(ttype,			$,			 32,	  0,	tscalar,	0),
	(tlabel,		"lab",		  8,	  0,	tvoid,		0),
	(tmult,			"mult",		  0,	  0,	tvoid,		0),		!denote multiple return valus

	(tlast,			$,		 	  0,	  0,	0,		0),
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64

global const tfirstnumtype = ti8
global const tlastnumtype  = tr64

global var int trefproc
global var int treflabel
global var int trefchar

!These special type codes are from QS. They are used in tables of exported functions
!It happens that codes up to and including tp_r64 match Q's codes up to tr64

global tabledata() [0:]ichar packtypenames, [0:]int packtypewidths =
	(tp_void=0,		$,	0),
	(tp_i64,		$,	64),
	(tp_u64,		$,	64),
	(tp_r64,		$,	64),

	(tp_pvoid,		$,	64),	!here to trp64, must be same order as tvoid..tr64
	(tp_pi8,		$,	64), 
	(tp_pi16,		$,	64),
	(tp_pi32,		$,	64),
	(tp_pi64,		$,	64),
	(tp_pi128,		$,	64),
	(tp_pu8,		$,	64),
	(tp_pu16,		$,	64),
	(tp_pu32,		$,	64),
	(tp_pu64,		$,	64),
	(tp_pu128,		$,	64),
	(tp_pr32,		$,	64),
	(tp_pr64,		$,	64),

	(tp_pstruct,	$,	64),
	(tp_stringz,	$,	64),
	(tp_variant,	$,	64),
end

global tabledata() [0:]ichar jtagnames, [0:]byte jisexpr=
!Basic units; these don't follow normal rules of params needing to be units or lists

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
![a=u] means a is a unit/list, or is nil

	(j_none=0,		$,		0), ! For tagname lookups when tag is zero
	(j_const,		$,		1), ! value/etc=value, typeno=type code
	(j_null,		$,		1), ! Place holder unit: means 'param no present' when used where a param is expected
	(j_name,		$,		1), ! def=nameptr
	(j_block,		$,		0), ! a=L			List of statements
	(j_stmtblock,	$,		0), ! a=L			List of statements
	(j_decimal,		$,		1), ! svalue=str			Longint constructor
	(j_assem,		$,		0), ! svalue=str			Longint constructor
	(j_assemmacro,	$,		0), !
!	(j_assemlabel,	$,		0), !
!	(j_assemgloballabel,	$,		0), !
	(j_assemreg,	$,		0), !
	(j_assemxreg,	$,		0), !
	(j_assemmem,	$,		0), !

!!Comments and Info
!
!	(j_comment,		$,		1) ! a=string	General comment

!Assorted sub-units

!Expressions and Operators

!Logical Operators

	(j_andl,		$,		1), ! a b	This group are for conditional expressions (no result)
	(j_orl,			$,		1), ! a b
	(j_xorl,		$,		1), ! a b
	(j_notl,		$,		1), ! a
	(j_istruel,		$,		1), ! a

!Expressions and Operators

	(j_makelist,	$,		1), ! a=L [lower=Lwb]
	(j_makerange,	$,		1), !
	(j_makeset,		$,		1), !
	(j_makedict,	$,		1), !
	(j_exprlist,	$,		1), ! a=u...	List of expressions, as (a;b;c), rather than (a,b,c)
	(j_multexpr,	$,		1), !
	(j_returnmult,	$,		1), !

	(j_keyword,		$,		1), ! def=nameptr a
	(j_keyvalue,	$,		1), ! a b
	(j_assignx,		$,		1), ! a b
	(j_deepcopyx,	$,		1), ! a b
	(j_callfn,		$,		1), ! a b
	(j_callmfn,		$,		1), ! a b
	(j_applyop,		$,		0), ! opcode b c
	(j_applyopx,	$,		1), ! opcode b c

!Binary Ops

	(j_andand,		$,		1), ! a b

	(j_eq,			$,		1), ! a b		int
	(j_ne,			$,		1), ! a b
	(j_lt,			$,		1), ! a b
	(j_le,			$,		1), ! a b
	(j_gt,			$,		1), ! a b
	(j_ge,			$,		1), ! a b

	(j_isequal,		$,		1), ! a b		int

	(j_add,			$,		1), ! a b
	(j_sub,			$,		1), ! a b
	(j_mul,			$,		1), ! a b
	(j_div,			$,		1), ! a b
	(j_idiv,		$,		1), ! a b
	(j_irem,		$,		1), ! a b
	(j_idivrem,		$,		1), ! a b
	(j_iand,		$,		1), ! a b
	(j_ior,			$,		1), ! a b
	(j_ixor,		$,		1), ! a b
	(j_shl,			$,		1), ! a b		int?
	(j_shr,			$,		1), ! a b
	(j_in,			$,		1), ! a b		int
	(j_notin,		$,		1), ! a b		int
	(j_inrev,		$,		1), ! a b
	(j_inrange,		$,		1), ! a b
!	(j_notinrange,	$,		1), ! a b
	(j_inset,		$,		1), ! a b
!	(j_notinset,	$,		1), ! a b
	(j_min,			$,		1), ! a b
	(j_max,			$,		1), ! a b
	(j_subref,		$,		1), ! a b
	(j_addoffset,	$,		1), ! a b
	(j_suboffset,	$,		1), ! a b
	(j_concat,		$,		1), ! a b
	(j_append,		$,		1), ! a b
	(j_clamp,		$,		1), ! a b
	(j_left,		$,		1), ! a b
	(j_right,		$,		1), ! a b

	(j_head,		$,		1), ! a b
	(j_tail,		$,		1), ! a b
	(j_init,		$,		1), ! a b
	(j_last,		$,		1), ! a b
	(j_take,		$,		1), ! a b
	(j_drop,		$,		1), ! a b
	(j_dupl,		$,		1), ! a b
	(j_ireverse,	$,		1), ! a b
	(j_reverse,		$,		1), ! a b
	(j_insert,		$,		1), ! a b
	(j_delete,		$,		1), ! a b
	(j_prepend,		$,		1), ! a b
	(j_zip,			$,		1), ! a b
	(j_convlc,		$,		1), ! a b
	(j_convuc,		$,		1), ! a b
	(j_flexptr,		$,		1), ! a b
	(j_stringz,		$,		1), ! a b
	(j_sliceptr,	$,		1), ! a b

	(j_index,		$,		1), ! a b		a[b]
	(j_slice,		$,		1), ! a b		a[b]
	(j_dotindex,	$,		1), ! a b		a[b]
	(j_dotslice,	$,		1), ! a b		a[b]
	(j_anddotindex,	$,		1), ! a b		a[b]
	(j_anddotslice,	$,		1), ! a b		a[b]
	(j_keyindex,	$,		1), ! a b		a[b]

	(j_dot,			$,		1), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotattr,		$,		1), ! a b		a.&b
	(j_atan2,		$,		1), ! a b	atan2(a,b)			real
	(j_power,		$,		1), ! a b	a**b				int/real



	(j_ptr,			$,		1), ! a		a^
	(j_addrof,		$,		1), ! a		&a
	(j_addroffirst,	$,		1), ! a		&a
	(j_convert,		$,		1), ! typeno=T a		T(a)			T
	(j_convertref,	$,		1), ! typeno=T a		T(a)			T
	(j_autocast,	$,		1), ! typeno=T a		T(a)			T
	(j_typepun,		$,		1), ! typeno=T a		T@(a)			T
	(j_typeconst,	$,		1), ! typeno=T			typeconst(T)
	(j_operator,	$,		1), ! opcode=opc
	(j_upper,		$,		1), ! a		$					T

!Monadic Ops

	(j_neg,			$,		1), ! a		-a
	(j_abs,			$,		1), ! a		abs a
	(j_inot,		$,		1), ! a
	(j_chr,			$,		1), ! a
	(j_asc,			$,		1), ! a

	(j_sqrt,		$,		1), ! a
	(j_sqr,			$,		1), ! a
	(j_cube,		$,		1), ! a
	(j_sign,		$,		1), ! a
	(j_sin,			$,		1), ! a
	(j_cos,			$,		1), ! a
	(j_tan,			$,		1), ! a
	(j_asin,		$,		1), ! a
	(j_acos,		$,		1), ! a
	(j_atan,		$,		1), ! a
	(j_ln,			$,		1), ! a
	(j_lg,			$,		1), ! a
	(j_log,			$,		1), ! a
	(j_exp,			$,		1), ! a
	(j_round,		$,		1), ! a
	(j_floor,		$,		1), ! a
	(j_ceil,		$,		1), ! a
	(j_fract,		$,		1), ! a
	(j_fmod,		$,		1), ! a

	(j_lwb,			$,		1), ! a		a.lwb				int
	(j_upb,			$,		1), ! a							int
	(j_len,			$,		1), ! a							int
	(j_bounds,		$,		1), ! a							int
	(j_bitwidth,	$,		1), ! a
	(j_bytesize,	$,		1), ! a
	(j_typeof,		$,		1), ! a
	(j_typestr,		$,		1), ! a
!	(j_sliceptr,	$,		1), ! a
	(j_bitfield,	$,		1), ! a

	(j_minvalue,	$,		1), ! a
	(j_maxvalue,	$,		1), ! a

!Increment

	(j_preincrx,	$,		1), ! a	++a
	(j_predecrx,	$,		1), ! a	--a
	(j_postincrx,	$,		1), ! a	a++
	(j_postdecrx,	$,		1), ! a	a--
	(j_incr,		$,		1), ! a	++a
	(j_decr,		$,		1), ! a	--a

!In-place operators

	(j_addto,		$,		0), ! a b	a+:=b
	(j_subto,		$,		0), ! a b
	(j_multo,		$,		0), ! a b
	(j_divto,		$,		0), ! a b
	(j_idivto,		$,		0), ! a b
	(j_iremto,		$,		0), ! a b
	(j_iandto,		$,		0), ! a b
	(j_iorto,		$,		0), ! a b
	(j_ixorto,		$,		0), ! a b
	(j_shlto,		$,		0), ! a b
	(j_shrto,		$,		0), ! a b
	(j_minto,		$,		0), ! a b
	(j_maxto,		$,		0), ! a b
	(j_addoffsetto,	$,		0), ! a b
	(j_suboffsetto,	$,		0), ! a b

	(j_negto,		$,		0), ! a		-:=a
	(j_absto,		$,		0), ! a
	(j_inotto,		$,		0), ! a

	(j_isvoid,		$,		1), ! a
	(j_isdef,		$,		1), ! a
	(j_isint,		$,		1), ! a
	(j_isreal,		$,		1), ! a
	(j_isstring,	$,		1), ! a
	(j_islist,		$,		1), ! a
	(j_isrecord,	$,		1), ! a
	(j_isarray,		$,		1), ! a
	(j_isset,		$,		1), ! a
	(j_ispointer,	$,		1), ! a
	(j_ismutable,	$,		1), ! a

!Translator Variables

	(j_cvlineno,	$,		1), ! 
	(j_cvstrlineno,	$,		1), ! 
	(j_cvmodulename,$,		1), ! 
	(j_cvfilename,	$,		1), ! 
	(j_cvfunction,	$,		1), ! 
	(j_cvdate,		$,		1), ! 
	(j_cvtime,		$,		1), ! 
	(j_cvversion,	$,		1), ! 
	(j_cvtypename,	$,		1), ! 
	(j_cvtargetbits,$,		1), ! 
	(j_cvtargetsize,$,		1), ! 
	(j_cvtargetcode,$,		1), ! 

	(j_whenthen,	$,		0), ! a=L b=u
	(j_elsif,		$,		0), ! condcode, a=u.
	(j_fmtitem,		$,		1), ! a=x b=fmtstr
	(j_nogap,		$,		1), ! 

!Statements

	(j_callproc,	$,		0), ! a=x b=L
	(j_callmproc,	$,		0), ! a=x b=L
	(j_return,		$,		0), ! a=x or nil
	(j_syscall,		$,		0), ! a=x or nil

	(j_assign,		$,		0), ! a b
	(j_shallowcopy,	$,		0), ! a b
	(j_deepcopy,	$,		0), ! a b
	(j_to,			$,		0), ! a=x b=u [def=nameptr]		c is optional temp name
	(j_if,			$,		1), ! condcode a=then b=else
	(j_longif,		$,		1), ! a=(elsif ...) b=else		L is series of kelsif pairs
	(j_forup,		$,		0), ! a=x b=x c=x d=Body [e=Else]		Body is single item or list
	(j_fordown,		$,		0), ! a=x b=x c=x d=Body [e=Else]
	(j_forstep,		$,		0), ! a=x b=x c=x d=Body [e=Else] [f=Step]
	(j_forall,		$,		0), ! a=L b=L     d=Body [e=Else]	a is list of vars, b is list of values
	(j_forallrev,	$,		0), ! a=x b=x     d=Body [e=Else]
	(j_foreach,		$,		0), ! a=L b=L     d=Body [e=Else]	a is list of vars, b is list of values
	(j_foreachrev,	$,		0), ! a=x b=x     d=Body [e=Else]
	(j_cfor,		$,		0), ! a=x b=x     d=Body [e=Else]
	(j_while,		$,		0), ! a=x b=u
	(j_repeat,		$,		0), ! a=u b=x
	(j_goto,		$,		0), ! a=x
	(j_gotoblock,	$,		0), ! a=x
	(j_labeldef,	$,		0), ! def=nameptr
	(j_restart,		$,		0), ! [a=x]
	(j_redo,		$,		0), ! [a=x]
	(j_next,		$,		0), ! [a=x]
	(j_exit,		$,		0), ! [a=x]
	(j_do,			$,		0), ! [a=u
	(j_case,		$,		1), ! a=x b=L [c=else]		L is series of whenthen pairs
	(j_docase,		$,		0), ! a=x b=L [c=else]
	(j_switch,		$,		1), ! a=x b=L [c=else]
	(j_doswitch,	$,		0), ! a=x b=L [c=else]
	(j_swap,		$,		0), ! a b
	(j_select,		$,		1), ! Not implemented
	(j_recase,		$,		0), ! Not implemented

	(j_print,		$,		0), ! [a=dev] b=L
	(j_println,		$,		0), ! [a=dev] b=L
	(j_fprint,		$,		0), ! [a=dev] b=fmtstr c=L
	(j_fprintln,	$,		0), ! [a=dev] b=fmtstr c=L
	(j_cprint,		$,		0), ! [a=dev] b=fmtstr c=L
	(j_cprintln,	$,		0), ! [a=dev] b=fmtstr c=L
	(j_sprint,		$,		0), !         b=L 
	(j_sfprint,		$,		0), !         b=L
	(j_read,		$,		0), ! [a=dev] b=L
	(j_readln,		$,		0), ! [a=dev] b=L
	(j_sread,		$,		0), ! [a=dev] b=L
	(j_sreadln,		$,		0), ! [a=dev] b=L
	(j_stop,		$,		0), ! [a=x]
	(j_try,			$,		0), ! a=try block; b=except list
	(j_except,		$,		0), ! a=except block; b=exception code list (constants)
	(j_yield,		$,		0), ! "
	(j_raise,		$,		0), ! "
!	(j_callhostproc,$,		0), ! "
	(j_eval,		$,		1), ! "
	(j_lambda,		$,		1), ! "
	(j_emitc,		$,		1), ! "


	(j_dummy,		$,		1)
end

!Foreign function Specifiers
global tabledata() [0:]ichar fflangnames=

	(noff=0,		$), ! 
	(windowsff,		$), ! 
	(clangff,		$), ! 
	(qlangff,		$), ! 
	(blangff,		$), ! 
	(callbackff,	$), ! 

	(dummyff,		$) ! 
end

global const linuxff=clangff

global tabledata() []ichar bitfieldnames=
	(bf_msb,		$),
	(bf_lsb,		$),
	(bf_msbit,		$),
	(bf_lsbit,		$),
	(bf_msw,		$),
	(bf_lsw,		$),
	(bf_odd,		$),
	(bf_even,		$),
end

global tabledata() []ichar symbolnames=
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$),		! Lex error
	(dotsym,			$),		! "."
	(lexdotsym,			$),		! ".", used at bol to prefix lexical 
	(anddotsym,			$),		! "&."
	(commasym,			$),		! ","
	(semisym,			$),		! ";"
	(colonsym,			$),		! ":"
	(dcolonsym,			$),		! "::"
	(assignsym,			$),		! :=
!	(dispassignsym,		$),		! :== =:=
	(deepcopysym,		$),		! ::=
	(sendtosym,			$),		! =>
	(lbracksym,			$),		! (
	(rbracksym,			$),		! )
	(lsqsym,			$),		! [
	(rsqsym,			$),		! ]
	(lcurlysym,			$),		! {
	(rcurlysym,			$),		! }
	(ptrsym,			$),		! ^
	(barsym,			$),		! |
	(dbarsym,			$),		! ||
	(atsym,				$),		! @
	(datsym,			$),		! @@
	(questionsym,		$),		! ?
	(addrsym,			$),		! &
	(daddrsym,			$),		! &&
	(poundsym,			$),		! Œ Hmm, should be Pound A+156
	(curlsym,			$),		! ~
	(gatesym,			$),		! ª
	(rangesym,			$),		! ..
	(ellipsissym,		$),		! ...
	(hashsym,			$),		! #
	(opsym,				$),		! Any operator or property tag (use sets to distinguish)
	(opsym2,			$),		! Any operator or property tag (use sets to distinguish)
	(bitfieldsym,		$),		! Special bit selections
	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen
	(rawnamesym,		$),		! unassigned name before lookup
	(rawxnamesym,		$),		! unassigned name, case-sensitive, that is never a reserved word
	(docstringsym,		$),		! ! #comment used as documentation string
	(incrsym,			$),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$),		! 123 32 bits signed
	(decimalconstsym,	$),		! 123 or 123.4 decimal
	(realconstsym,		$),		! 123.4 64 bits
	(charconstsym,		$),		! 'A' or 'ABCD'
	(wcharconstsym,		$),		! 'A'W or 'ABCD'W (but don't have a syntax yet)
	(stringconstsym,	$),		! "ABC"
	(astringconstsym,	$),		! A"ABC"
	(wstringconstsym,	$),		! "ABC"W

!Second half are tokens that can be yielded after a name lookup::
	(unitnamesym,		$),		! 
	(namesym,			$),		! identifier symbol
	(ksourcedirsym,		$),		! 
!	(lexmacronamesym,	$),		! 
	(regsym,			$),		! x64 registers
	(xregsym,			$),		! XMM registers
	(fregsym,			$),		! ST registers
	(mregsym,			$),		! MMX registers
	(jmpccsym,			$),		! 
	(setccsym,			$),		! 
	(movccsym,			$),		! 
	(segnamesym,		$),		! 
	(asmopcodesym,		$),		! MOV etc

	(stdtypesym,		$),		! INT, CHAR etc
	(machinetypesym,	$),		! INTM etc
!	(packtypesym,		$),		! Byte etc
	(ktypeofsym,		$),		! TYPEOF
	(ksubrangesym,		$),		! SUBRANGE
	(koutsym,			$),		! OUT
	(kicharsym,			$),		! ICHAR
	(kifsym,			$),		! 
	(kthensym,			$),		! 
	(kelsifsym,			$),		! 
	(kelsesym,			$),		! 
	(kelsecasesym,		$),		! 
	(kelseswitchsym,	$),		! 
	(kelseselectsym,	$),		! 
	(kendsym,			$),		! 
	(kunlesssym,		$),		! 
	(kcasesym,			$),		! CASE
	(kdocasesym,		$),		! DOCASE
	(krecasesym,		$),		! RECASE
	(kwhensym,			$),		! 
	(kforsym,			$),		! 
	(kforallsym,		$),		! FORALL
	(ktosym,			$),		! TO/DOWNTO
	(kbysym,			$),		! 
	(kdosym,			$),		! 
	(kwhilesym,			$),		! 
	(krepeatsym,		$),		! 
	(kuntilsym,			$),		! 
	(kreturnsym,		$),		! 
	(kstopsym,			$),		! 
	(kloopsym,			$),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$),		! GO/GOTO
	(kswitchsym,		$),		! SWITCH
	(kdoswitchsym,		$),		! DOSWITCH
	(kprintsym,			$),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		$),		! SPRINT/SFPRINT
	(kreadsym,			$),		! READ/READLN
	(ksreadsym,			$),		! SREAD
	(ksreadlnsym,		$),		! SREADLN
	(kprocsym,			$),		! PROC
	(kfunctionsym,		$),		! FUNCTION
!	(kmethodsym,		$),		! METHOD
	(klabelsym,			$),		! LABEL
	(krecordsym,		$),		! RECORD
	(kstructsym,		$),		! STRUCT
	(kunionsym,			$),		! UNION
	(kimportsym,		$),		! IMPORT
	(kimportmodulesym,	$),		! IMPORTDLL/IMPORTMODULE
	(kimportpathsym,	$),		! IMPORTPATH
	(kmapmodulesym,		$),		! MAPMODULE
	(kmodulesym,		$),		! 
	(ktypesym,			$),		! TYPE
	(ktypeattrsym,		$),		! COMPACT/DERIVED
	(krefsym,			$),		! REF
	(kvarsym,			$),		! VAR
	(kletsym,			$),		! LET
	(kvariantsym,		$),		! VARIANT
	(kslicesym,			$),		! SLICE/DSLICE
	(kflexsym,			$),		! FLEX
!	(krangesym,			$),		! RANGE
!	(ksetsym,			$),		! SET
	(kmacrosym,			$),		! MACRO
	(kexpandsym,		$),		! EXPAND
	(koperatorsym,		$),		! OPERATOR
	(kconstsym,			$),		! 
	(klocalssym,		$),		! LOCALS
	(kenumsym,			$),		! 
	(knewsym,			$),		! NEW/HEAP
	(kclasssym,			$),		! CLASS
	(kdoblocksym,		$),		! DOBLOCK
	(kblockdefsym,		$),		! BLOCKDEF
	(kdirectivesym,		$),		! TARGET/MODULE
	(kfflangsym,		$),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		$),		! global
	(kstaticsym,		$),		! STATIC

	(ktrysym,			$),		! 
	(kexceptsym,		$),		! 
	(kfinallysym,		$),		! 
	(kraisesym,			$),		! 
	(kyieldsym,			$),		! 
	(kextendsym,		$),		!
	(kblocksym,			$),		!
	(kcastsym,			$),		! CAST
	(ktypeconstsym,		$),		! TYPECONST
	(compilervarsym,	$),		! $lineno etc
	(dollarsym,			$),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$),		! EVAL
	(ktabledatasym,		$),		! tabledata
	(kmapsym,			$),		! MAP/MAPL/MAPR/MAPLR
	(kapplyopsym,		$),		! APPLYOP
	(kstacksym,			$),		! STACK/UNSTACK
!	(kforwardsym,		$),		! FORWARD
	(kclampsym,			$),			! CLAMP
	(kswapsym,			$),		! SWAP
!	(kcondcompsym,		$),		! $WHEN
	(kerrorsym,			$),		! PC_ERROR etc
	(sysconstsym,		$),		! nil, etc
	(kassemsym,			$),		! ASM/ASSEM
	(kemitcsym,			$),		! emitc "abc"
	(ksyscallsym,		$),		! $get_procname etc

	(kdummysym,			$)		!
end

global tabledata() []ichar sourcedirnames =
	(emitcdir,	$),
	(ifdir,		$),
	(elsifdir,	$),
	(elsedir,	$),
	(endifdir,	$),
	(debuglinedir,	$),
	(includedir,	$),
	(endincludedir,	$),
	(commentdir,	$),
	(endcommentdir,	$),
	(strincludedir,	$),
	(binincludedir,	$),
	(cclibdir,		$),

	(targetlangdir,	$),
	(enddir,	$)
end

global tabledata() =
	(nil_const),
	(pi_const),
	(tab_const),
	(con_const)
end

global tabledata() =
	(thousand_unit),
	(million_unit),
	(billion_unit),
	(kilo_unit),
	(mega_unit),
	(giga_unit)
end

global tabledata() [0:]ichar parammodenames=
	(var_param=0,		"Var "),
	(in_param,			"In "),
	(out_param,			"Out "),
	(optional_param,	"Opt "),
end

global tabledata() [0:]ichar namecatnames, [0:]byte qualifiedname =
	(normal_cat=0,	 	"-",		0),
	(proc_cat, 			"proc",		1),
	(globalproc_cat,	"gproc",	1),
	(dllproc_cat,		"dllproc",	0),
	(dllmodule_cat,		"dllmodule",0),
	(dllvar_cat,		"dllvar",	0),
	(static_cat,		"static",	1),
	(frame_cat,			"frame",	1),
end

global tabledata() [0:]ichar namenames, [0:]byte defaultnamecat =
	(nullid=0,		$,	0),				!Not assigned (sometimes converted to genfieldid)
	(programid,		$,	0),				!Main root
	(moduleid,		$,	0),				!Current or imported module
	(dllmoduleid,	$,	dllmodule_cat),		!
	(typeid,		$,	0),				!Type name in type, proc or module
!	(classid,		$,	0),				!Class name
	(procid,		$,	proc_cat),		!Proc/method/function/op name
	(dllprocid,		$,	dllproc_cat),	!Dll Proc/function name
	(dllvarid,		$,	dllvar_cat),	!Dll variable name
	(constid,		$,	0),				!Named constant in type, proc or module
	(staticid,		$,	static_cat),	!Static var in type or proc or module
!	(staticletid,	$,	static_cat),	!Static let in type or proc or module
	(frameid,		$,	frame_cat),		!Local var
!	(frameletid,	$,	frame_cat),		!Local let
	(paramid,		$,	frame_cat),		!Local param
	(fieldid,		$,	0),				!Field of Record or Class
	(genfieldid,	$,	0),				!Generic Field of Record or Class
	(enumid,		$,	0),				!Enum name, part of enum type only
	(labelid,		$,	0),				!Label name in proc only
	(blockid,		$,	0),				!Codeblock label name in proc only
	(aliasid,		$,	0),				!Alias to another name
	(macroid,		$,	0),				!Name of macro
	(macroparamid,	$,	0),				!Macro formal parameter name
	(linkid,		$,	0)				!Name in class defined in a base class
end

global tabledata []ichar stnames, []int stsymbols, []int stsubcodes=

	("if",			kifsym,			j_if),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		j_if),
	("else",		kelsesym,		0),
	("elsecase",	kelsecasesym,	j_case),
	("elseswitch",	kelseswitchsym,	j_switch),
	("case",		kcasesym,		j_case),
	("docase",		kdocasesym,		j_docase),
	("recase",		krecasesym,		j_recase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("forall",		kforallsym,		j_forall),
	("foreach",		kforallsym,		j_foreach),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		1),
	("return",		kreturnsym,		0),
	("yield",		kyieldsym,		0),
	("stop",		kstopsym,		0),
	("restart",		kloopsym,		j_restart),
	("redo",		kloopsym,		j_redo),
	("loop",		kloopsym,		j_redo),
	("next",		kloopsym,		j_next),
	("exit",		kloopsym,		j_exit),
	("goto",		kgotosym,		0),
	("go",			kgotosym,		1),
	("switch",		kswitchsym,		j_switch),
	("doswitch",	kdoswitchsym,	j_doswitch),
	("tabledata",	ktabledatasym,	0),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
!	("$windows",	kcondcompsym,	windowsff),
!	("$linux",		kcondcompsym,	linuxff),

	("print",		kprintsym,		j_print),
	("println",		kprintsym,		j_println),
	("fprint",		kprintsym,		j_fprint),
	("fprintln",	kprintsym,		j_fprintln),
	("cprint",		kprintsym,		j_cprint),
	("cprintln",	kprintsym,		j_cprintln),
	("sprint",		ksprintsym,		j_sprint),
	("sfprint",		ksprintsym,		j_sfprint),

	("cp",			kprintsym,		j_print),
	("cpl",			kprintsym,		j_println),

	("read",		kreadsym,		j_read),
	("readln",		kreadsym,		j_readln),
	("cast",		kcastsym,		j_convert),

	("proc",		kprocsym,		0),
	("function",	kfunctionsym,	0),
	("threadedproc",		kprocsym,		1),
	("threadedfunction",	kfunctionsym,	1),
!	("method",		kmethodsym,		0),

! ("operator",	koperatorsym,		0),
	("type",		ktypesym,		0),
	("class",		kclasssym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),
	("let",			kletsym,		0),
!	("variant",		kvariantsym,	0),
	("any",			kvariantsym,	0),


	("include",		ksourcedirsym,	includedir),
	("strinclude",	ksourcedirsym,	strincludedir),
	("bininclude",	ksourcedirsym,	binincludedir),
	("emitc",		ksourcedirsym,	emitcdir),
	("cclib",		ksourcedirsym,	cclibdir),
	("macro",		kmacrosym,		0),
!	("expand",		kexpandsym,		0),
	("operator",	koperatorsym,	0),

	("assem",		kassemsym,		1),
	("asm",			kassemsym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),
	("enum",		kenumsym,		0),

	("$get_nprocs",		ksyscallsym,		sysfn_get_nprocs),
	("$get_procname",	ksyscallsym,		sysfn_get_procname),
	("$get_procaddr",	ksyscallsym,		sysfn_get_procaddr),

	("$get_nexports",	ksyscallsym,		sysfn_get_nexports),
	("$get_procexport",	ksyscallsym,		sysfn_get_procexport),

	("$nprocs",			ksyscallsym,		sysfn_nprocs),
	("$nexports",		ksyscallsym,		sysfn_nexports),
	("$procnames",		ksyscallsym,		sysfn_procnames),
	("$procaddrs",		ksyscallsym,		sysfn_procaddrs),
	("$procexports",	ksyscallsym,		sysfn_procexports),

	("importdll",	kimportmodulesym,	0),
	("importlib",	kimportmodulesym,	0),
	("import",		kimportsym,			0),
	("importpath",	kimportpathsym,		0),
	("mapmodule",	kmapmodulesym,		0),
	("unless",		kunlesssym,			0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("finally",		kfinallysym,	0),
	("raise",		kraisesym,		0),
	("out",			koutsym,		0),

	("global",		kglobalsym,		1),
	("export",		kglobalsym,		2),

	("clang",		kfflangsym,		clangff),
	("qlang",		kfflangsym,		qlangff),
	("windows",		kfflangsym,		windowsff),
	("callback",	kfflangsym,		callbackff),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),
!	("dint",		stdtypesym,		ti128),

	("word",		stdtypesym,		tword),

!	("dword",		stdtypesym,		tu128),
	("real",		stdtypesym,		treal),

	("ichar",		kicharsym,		0),

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),
	("int128",		stdtypesym,		ti128),

	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),
	("i128",		stdtypesym,		ti128),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),
	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("float32",		stdtypesym,		tr32),
	("float64",		stdtypesym,		tr64),

	("byte",		stdtypesym,		tu8),
	("u1",			stdtypesym,		tu1),
	("u2",			stdtypesym,		tu2),
	("u4",			stdtypesym,		tu4),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),
	("u128",		stdtypesym,		tu128),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),
	("word128",		stdtypesym,		tu128),

	("bit",			stdtypesym,		tu1),
	("bit2",		stdtypesym,		tu2),
	("bit4",		stdtypesym,		tu4),

	("char",		stdtypesym,		tc8),
	("wchar",		stdtypesym,		tc16),
	("char64",		stdtypesym,		tc64),

	("array",		stdtypesym,		tfixedarray),
	("bitarray",	stdtypesym,		tfixedbits),
	("complex",		stdtypesym,		tcomplex64),
	("string",		stdtypesym,		tflexstring),
	("wstring",		stdtypesym,		tflexwstring),
	("set",			stdtypesym,		tfixedset),
	("decimal",		stdtypesym,		tflexdecimal),
	("dict",		stdtypesym,		tflexdict),
	("range",		stdtypesym,		trange64),
!	("variant",		stdtypesym,		tvariant),
!	("label",		stdtypesym,		tlabel),
	("auto",		stdtypesym,		tauto),

	("intm",		machinetypesym,	'I'),
	("intp",		machinetypesym,	'i'),
	("wordm",		machinetypesym,	'W'),
	("wordp",		machinetypesym,	'w'),
	("slice",		kslicesym,		tslice),
	("flex",		kflexsym,		tflexarray),
!	("dslice",		kslicesym,		tdslice),
!	("slice2",		kslicesym,		tslice2),
	("typeof",		ktypeofsym,			0),
	("subrange",	ksubrangesym,		0),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
	("thousand",	unitnamesym,	thousand_unit),
	("kb",			unitnamesym,	kilo_unit),
	("mb",			unitnamesym,	mega_unit),
	("gb",			unitnamesym,	giga_unit),

	("$lineno",		compilervarsym,	j_cvlineno),
	("$strlineno",	compilervarsym,	j_cvstrlineno),
	("$filename",	compilervarsym,	j_cvfilename),
	("$modulename",	compilervarsym,	j_cvmodulename),
	("$function",	compilervarsym,	j_cvfunction),
	("$date",		compilervarsym,	j_cvdate),
	("$time",		compilervarsym,	j_cvtime),
	("$version",	compilervarsym,	j_cvversion),
	("$typename",	compilervarsym,	j_cvtypename),
	("$targetbits",	compilervarsym,	j_cvtargetbits),
	("$targetsize",	compilervarsym,	j_cvtargetsize),
!	("$targetname",	compilervarsym,	j_cvtargetname),
	("$targetcode",	compilervarsym,	j_cvtargetcode),
	("$",			dollarsym,		0),

	("and",			opsym,			j_andl),
	("or",			opsym,			j_orl),
	("xor",			opsym,			j_xorl),
	("iand",		opsym,			j_iand),
	("ior",			opsym,			j_ior),
	("ixor",		opsym,			j_ixor),
	("in",			opsym,			j_in),
	("notin",		opsym,			j_notin),
	("inrev",		opsym,			j_inrev),
	("rem",			opsym,			j_irem),
	("divrem",		opsym,			j_idivrem),
	("min",			opsym,			j_min),
	("max",			opsym,			j_max),

	("not",			opsym,			j_notl),
	("inot",		opsym,			j_inot),
	("istrue",		opsym,			j_istruel),
	("abs",			opsym,			j_abs),
	("$neg",		opsym,			j_neg),
!	("asc",			opsym,			j_asc),
!	("chr",			opsym,			j_chr),
	("sqrt",		opsym,			j_sqrt),
	("sqr",			opsym,			j_sqr),
	("cube",		opsym,			j_cube),
	("cos",			opsym,			j_cos),
	("sin",			opsym,			j_sin),
	("tan",			opsym,			j_tan),
	("asin",		opsym,			j_asin),
	("acos",		opsym,			j_acos),
	("atan",		opsym,			j_atan),
	("atan2",		opsym,			j_atan2),
	("sign",		opsym,			j_sign),
	("ln",			opsym,			j_ln),
	("log",			opsym,			j_log),
	("lg",			opsym,			j_lg),
	("exp",			opsym,			j_exp),
	("round",		opsym,			j_round),
	("floor",		opsym,			j_floor),
	("ceil",		opsym,			j_ceil),
	("fract",		opsym,			j_fract),
	("fmod",		opsym,			j_fmod),

	("head",		opsym,			j_head),
	("tail",		opsym,			j_tail),
	("init",		opsym,			j_init),
!	("last",		opsym,			j_last),
	("take",		opsym,			j_take),
	("drop",		opsym,			j_drop),
!	("left",		opsym,			j_left),
!	("right",		opsym,			j_right),
	("insert",		opsym,			j_insert),
	("delete",		opsym,			j_delete),
	("ireverse",	opsym,			j_ireverse),
	("reverse",		opsym,			j_reverse),
	("dupl",		opsym,			j_dupl),
	("zip",			opsym,			j_zip),
	("prepend",		opsym,			j_prepend),
	("append",		opsym,			j_append),
	("concat",		opsym,			j_concat),
	("convlc",		opsym,			j_convlc),
	("convuc",		opsym,			j_convuc),
	("flexptr",		opsym,			j_flexptr),
	("sliceptr",	opsym,			j_sliceptr),
	("stringz",		opsym,			j_stringz),

	("len",			opsym2,			j_len),
	("lwb",			opsym2,			j_lwb),
	("upb",			opsym2,			j_upb),
	("bounds",		opsym2,			j_bounds),
	("bitwidth",	opsym2,			j_bitwidth),
	("bytes",		opsym2,			j_bytesize),
	("minvalue",	opsym2,			j_minvalue),
	("maxvalue",	opsym2,			j_maxvalue),
	("typestr",		opsym2,			j_typestr),

	("isvoid",		opsym2,			j_isvoid),
	("isdef",		opsym2,			j_isdef),
	("isint",		opsym2,			j_isint),

	("msb",			bitfieldsym,	bf_msb),
	("lsb",			bitfieldsym,	bf_lsb),
	("msbit",		bitfieldsym,	bf_msbit),
	("lsbit",		bitfieldsym,	bf_lsbit),
	("msw",			bitfieldsym,	bf_msw),
	("lsw",			bitfieldsym,	bf_lsw),
	("odd",			bitfieldsym,	bf_odd),
	("even",		bitfieldsym,	bf_even),

	("endif",		kendsym,	kifsym),
	("fi",			kendsym,	kifsym),
	("endcase",		kendsym,	kcasesym),
	("esac",		kendsym,	kcasesym),
	("enddocase",	kendsym,	kdocasesym),
	("endswitch",	kendsym,	kswitchsym),
	("enddoswitch",	kendsym,	kdoswitchsym),
	("endfor",		kendsym,	kforsym),
	("endforall",	kendsym,	kforallsym),
	("od",			kendsym,	kdosym),
	("endproc",		kendsym,	kprocsym),
	("endfunction",	kendsym,	kfunctionsym),
	("endwhile",	kendsym,	kwhilesym),
	("endto",		kendsym,	ktosym),
	("enddo",		kendsym,	kdosym),
	("endunless",	kendsym,	kunlesssym),
	("endmodule",	kendsym,	kmodulesym),
	("endimportmodule",	kendsym,kimportmodulesym),
	("endtry",		kendsym,	ktrysym),
	("endrecord",	kendsym,	krecordsym),
	("endclass",	kendsym,	kclasssym),
	("endblock",	kendsym,	kblocksym),
	("endassem",	kendsym,	kassemsym),

	("nil",			sysconstsym,	nil_const),
	("con",			sysconstsym,	con_const),
!	("tab",			sysconstsym,	tab_const),
	("pi",			sysconstsym,	pi_const),

!	("eax",			registersym,	0),
!	("ebx",			registersym,	0),
!	("ecx",			registersym,	0),
!	("edx",			registersym,	0),
!	("esi",			registersym,	0),
!	("edi",			registersym,	0),
!	("esp",			registersym,	0),
!	("ebp",			registersym,	0),
!
!	("ax",			registersym,	0),
!	("bx",			registersym,	0),
!	("cx",			registersym,	0),
!	("dx",			registersym,	0),
!	("si",			registersym,	0),
!	("di",			registersym,	0),
!	("sp",			registersym,	0),
!	("bp",			registersym,	0),
!
!	("rax",			registersym,	0),
!	("rbx",			registersym,	0),
!	("rcx",			registersym,	0),
!	("rdx",			registersym,	0),
!	("rsi",			registersym,	0),
!	("rdi",			registersym,	0),
!	("rsp",			registersym,	0),
!	("rbp",			registersym,	0),
!
!	("r8",			registersym,	0),
!	("r9",			registersym,	0),
!	("r10",			registersym,	0),
!	("r11",			registersym,	0),
!	("r12",			registersym,	0),
!	("r13",			registersym,	0),
!	("r14",			registersym,	0),
!	("r15",			registersym,	0),
!
!	("al",			registersym,	0),
!	("bl",			registersym,	0),
!	("cl",			registersym,	0),
!	("dl",			registersym,	0),
!	("ah",			registersym,	0),
!	("bh",			registersym,	0),
!	("ch",			registersym,	0),
!	("dh",			registersym,	0),
!
!	("b0",			registersym,	0),
!	("b1",			registersym,	0),
!	("b2",			registersym,	0),
!	("b3",			registersym,	0),
!	("b4",			registersym,	0),
!	("b5",			registersym,	0),
!	("b6",			registersym,	0),
!	("b7",			registersym,	0),
!	("b8",			registersym,	0),
!	("b9",			registersym,	0),
!	("b10",			registersym,	0),
!	("b11",			registersym,	0),
!	("b12",			registersym,	0),
!	("b13",			registersym,	0),
!	("b14",			registersym,	0),
!	("b15",			registersym,	0),
!
!	("w0",			registersym,	0),
!	("w1",			registersym,	0),
!	("w2",			registersym,	0),
!	("w3",			registersym,	0),
!	("w4",			registersym,	0),
!	("w5",			registersym,	0),
!	("w6",			registersym,	0),
!	("w7",			registersym,	0),
!	("w8",			registersym,	0),
!	("w9",			registersym,	0),
!	("w10",			registersym,	0),
!	("w11",			registersym,	0),
!	("w12",			registersym,	0),
!	("w13",			registersym,	0),
!	("w14",			registersym,	0),
!	("w15",			registersym,	0),
!
!	("a0",			registersym,	0),
!	("a1",			registersym,	0),
!	("a2",			registersym,	0),
!	("a3",			registersym,	0),
!	("a4",			registersym,	0),
!	("a5",			registersym,	0),
!	("a6",			registersym,	0),
!	("a7",			registersym,	0),
!	("a8",			registersym,	0),
!	("a9",			registersym,	0),
!	("a10",			registersym,	0),
!	("a11",			registersym,	0),
!	("a12",			registersym,	0),
!	("a13",			registersym,	0),
!	("a14",			registersym,	0),
!	("a15",			registersym,	0),
!	("aframe",		registersym,	0),
!	("astack",		registersym,	0),
!
!	("d0",			registersym,	0),
!	("d1",			registersym,	0),
!	("d2",			registersym,	0),
!	("d3",			registersym,	0),
!	("d4",			registersym,	0),
!	("d5",			registersym,	0),
!	("d6",			registersym,	0),
!	("d7",			registersym,	0),
!	("d8",			registersym,	0),
!	("d9",			registersym,	0),
!	("d10",			registersym,	0),
!	("d11",			registersym,	0),
!	("d12",			registersym,	0),
!	("d13",			registersym,	0),
!	("d14",			registersym,	0),
!	("d15",			registersym,	0),
!	("dstack",		registersym,	0),
!	("dframe",		registersym,	0),
!	("dprog",		registersym,	0),
!	("dsptr",		registersym,	0),
!
!	("xmm0",		registersym,	0),
!	("xmm1",		registersym,	0),
!	("xmm2",		registersym,	0),
!	("xmm3",		registersym,	0),
!	("xmm4",		registersym,	0),
!	("xmm5",		registersym,	0),
!	("xmm6",		registersym,	0),
!	("xmm7",		registersym,	0),
!	("xmm8",		registersym,	0),
!	("xmm9",		registersym,	0),
!	("xmm10",		registersym,	0),
!	("xmm11",		registersym,	0),
!	("xmm12",		registersym,	0),
!	("xmm13",		registersym,	0),
!	("xmm14",		registersym,	0),
!	("xmm15",		registersym,	0),
!
!	("qword",		registersym,	0),

	("$$dummy",		0,				0)
end

!this pair of lists associates each of a set of ops, with a priority
global tabledata []int oplist,[]int oppriolist =
	(j_add,			4),
	(j_sub,			4),
	(j_mul,			3),
	(j_div,			3),
	(j_idiv,		3),
	(j_irem,		3),
	(j_idivrem,		3),
	!(j_ddiv,		3),
	(j_andl,		7),
	(j_orl,			8),
	(j_xorl,		6),
	(j_iand,		4),
	(j_ior,			4),
	(j_ixor,		4),
	(j_shl,			3),
	(j_shr,			3),
	!(j_rol,		3),
	!(j_ror,		3),
	(j_in,			6),
	(j_notin,		6),
	(j_inrev,		6),
	(j_eq,			6),
	(j_ne,			6),
	(j_lt,			6),
	(j_ge,			6),
	(j_le,			6),
	(j_gt,			6),
	(j_isequal,		6),
	!(j_testeq,		6),
	(j_min,			4),
	(j_max,			4),
	(j_power,		2),
	(j_atan2,		3),
	(j_addoffset,	4),
	(j_suboffset,	4),
	(j_subref,		4),
	(j_concat,		4),
	(j_append,		4),
	(j_assignx,		1),
	(j_deepcopyx,	1),
	(j_makerange,	5)
end

global var [0:jtagnames.len]byte jtagpriotable		!set up from the above

global tabledata() [0:]ichar convnames =
	(c_none=0,			$),
	(c_soft,			$),
	(c_hard,			$),
	(c_bool,			$),
	(c_iwiden,			$),
	(c_uwiden,			$),
	(c_ifloat,			$),
	(c_ufloat,			$),
	(c_ifix,			$),
	(c_ufix,			$),

	(c_diwiden,			$),
	(c_duwiden,			$),
	(c_difloat,			$),
	(c_dufloat,			$),
	(c_difix,			$),
	(c_dufix,			$),
	(c_dfnarrow,		$),
	(c_dfwiden,			$),

	(c_narrow,			$),
	(c_softtruncate,	$),
	(c_truncate,		$),
	(c_fnarrow,			$),
	(c_fwiden,			$),
	(c_inttoref,		$),
	(c_reftoint,		$),
	(c_reftoref,		$),
!	(c_arraytoslice,	$),

	(c_anytovariant,	$),
	(c_anytodecimal,	$),
	(c_ichartostring,	$),

	(c_varianttoany,	$),
	(c_decimaltoany,	$),
	(c_stringtoichar,	$),

	(c_error,			$)
end

!dominantmode[s,t] returns the dominant type of s and t, widened to int/uint as needed
!only use on base types, to keep indices under 32
global var [0:32,0:32]byte dominantmode

!conversionops[s,t] gives conversion op to convert numeric types s to t
!only use when both types are <16
global var [0:32,0:32]byte conversionops

!3rd value is what will be the dominant mode of the first two::
!There are two parts: width and prefered type
!The dominant width will be always be at least 64 bits, or 128 if one is 128 bits.
!THIS HAS CHANGED, so types will not always be widened to at least 64 bits, only
!to the greater of the two

!The dominant type will be the first from this list if at least one is present::
!  decimal, real, int, word, char, bool
!So ti8/tu32 will be ti64, as width/signed int take priority

!4th value is conversion neededed to get 1st type to 2nd; unconnected with 3rd
!NOTE: NEED TO ADD DECIMAL type to this table

const tdec=tflexdecimal

global var [][4]byte typesetuptable=(
!---------------------
	(ti64,	ti64,	ti64,	c_none),
	(ti64,	ti128,	ti128,	c_iwiden),

	(ti64,	tu64,	ti64,	c_soft),
	(ti64,	tu128,	ti128,	c_uwiden),

	(ti64,	tc64,	ti64,	c_softtruncate),

	(ti64,	tr32,	tr64,	c_ifloat),
	(ti64,	tr64,	tr64,	c_ifloat),
	(ti64,	tdec,	tdec,	c_diwiden),

	(ti64,	tref,	tvoid,	c_inttoref),
!---------------------
	(ti128,	ti64,	ti128,	c_softtruncate),
	(ti128,	ti128,	ti128,	c_none),

	(ti128,	tu64,	ti128,	c_soft),
	(ti128,	tu128,	ti128,	c_soft),

	(ti128,	tc64,	ti128,	c_softtruncate),

	(ti128,	tr32,	tdec,	c_ifloat),
	(ti128,	tr64,	tdec,	c_ifloat),
	(ti128,	tdec,	tdec,	c_difloat),

	(ti128,	tref,	tvoid,	c_inttoref),
!---------------------
	(tu64,	ti64,	ti64,	c_soft),
	(tu64,	ti128,	ti128,	c_uwiden),

	(tu64,	tu64,	tu64,	c_none),
	(tu64,	tu128,	tu128,	c_uwiden),

	(tu64,	tc64,	tu64,	c_softtruncate),

	(tu64,	tr32,	tr64,	c_ufloat),
	(tu64,	tr64,	tr64,	c_ufloat),
	(tu64,	tdec,	tdec,	c_dufloat),

	(tu64,	tref,	tvoid,	c_inttoref),
!---------------------
	(tu128,	ti64,	ti128,	c_soft),
	(tu128,	ti128,	ti128,	c_soft),

	(tu128,	tu64,	tu128,	c_none),
	(tu128,	tu128,	tu128,	c_none),

	(tu128,	tc64,	tu128,	c_softtruncate),

	(tu128,	tr32,	tdec,	c_ufloat),
	(tu128,	tr64,	tdec,	c_ufloat),
	(tu128,	tdec,	tdec,	c_dufloat),

	(tu128,	tref,	tvoid,	c_inttoref),

!---------------------
	(tc64,	ti64,	ti64,	c_soft),
	(tc64,	ti128,	ti128,	c_uwiden),

	(tc64,	tu64,	tu64,	c_soft),
	(tc64,	tu128,	tu128,	c_uwiden),

	(tc64,	tc64,	tc64,	c_none),

	(tc64,	tr32,	tr64,	c_ufloat),
	(tc64,	tr64,	tr64,	c_ufloat),
	(tc64,	tdec,	tdec,	c_dufloat),

	(tc64,	tref,	tvoid,	c_inttoref),

!---------------------
	(tr32,	ti64,	tr32,	c_ifix),
	(tr32,	ti128,	tr32,	c_ifix),

	(tr32,	tu64,	tr32,	c_ufix),
	(tr32,	tu128,	tdec,	c_ufix),

	(tr32,	tc64,	tr32,	c_ufix),

	(tr32,	tr32,	tr32,	c_none),
	(tr32,	tr64,	tr64,	c_fwiden),
	(tr32,	tdec,	tdec,	c_dfwiden),

	(tr32,	tref,	tvoid,	c_error),
!---------------------
	(tr64,	ti64,	tr64,	c_ifix),
	(tr64,	ti128,	tdec,	c_ifix),

	(tr64,	tu64,	tr64,	c_ufix),
	(tr64,	tu128,	tdec,	c_ufix),

	(tr64,	tc64,	tr64,	c_ufix),

	(tr64,	tr32,	tr64,	c_fnarrow),
	(tr64,	tr64,	tr64,	c_none),
	(tr64,	tdec,	tdec,	c_dfwiden),

	(tr64,	tref,	tvoid,	c_error),
!---------------------
	(tdec,	ti64,	tdec,	c_difix),
	(tdec,	ti128,	tdec,	c_difix),

	(tdec,	tu64,	tr64,	c_dufix),
	(tdec,	tu128,	tdec,	c_dufix),

	(tdec,	tc64,	tr64,	c_dufix),

	(tdec,	tr32,	tr64,	c_dfnarrow),
	(tdec,	tr64,	tr64,	c_dfnarrow),
	(tdec,	tdec,	tdec,	c_none),

	(tdec,	tref,	tvoid,	c_error),
!---------------------
	(tref,	ti64,	tvoid,	c_reftoint),
	(tref,	ti128,	tvoid,	c_error),

	(tref,	tu64,	tvoid,	c_reftoint),
	(tref,	tu128,	tvoid,	c_error),

	(tref,	tr32,	tvoid,	c_error),
	(tref,	tr64,	tvoid,	c_error),
	(tref,	tdec,	tvoid,	c_error),

	(tref,	tref,	tref,	c_reftoref))

!$-variables are lists of values that elsewhere need to be formed into
!actual sets. For M, a 'set' will be an array of bytes.
!
!symbols that can be starters for an expression (that can return a value)
global var []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,opsym,namesym,
	incrsym,intconstsym,decimalconstsym,realconstsym,charconstsym,stringconstsym,stdtypesym,
	ksprintsym,ksreadsym,ksreadlnsym,knewsym,dollarsym,compilervarsym, kclampsym,
	kapplyopsym,kerrorsym,krefsym, kcastsym, anddotsym, astringconstsym, ktypeofsym,
	kifsym, ksyscallsym)

!!symbols that represent loop keywords that can also be ended with 'od'
!global var []int $loopkwdset= (kforsym,kforallsym,kdosym,ktosym,kwhilesym,kcforsym)

global var []int D_typestarterset= (stdtypesym,lsqsym,krefsym,kenumsym,krecordsym,
		kicharsym, ktypeofsym, kslicesym, kflexsym)

global var [0..kdummysym]byte exprtermset	!EXPERIMENTAL

global var [0..jtagnames.upb]byte condopset			!contains 1 for j_eq/ne/lt/le/gt/ge

global var []byte D_boolunitset = (
	j_eq, j_ne, j_lt, j_le, j_ge, j_gt,
	j_andl, j_orl, j_notl, j_istruel, j_xorl, j_inrange, j_inset)

global var [0..jtagnames.upb]byte boolunitset

global var []byte D_refunitset = (					!whether & prefix op allowed
	j_name, j_dot, j_index, j_if, j_select, j_longif, j_exprlist, j_addrof, j_ptr,
	j_multexpr,j_dotindex,j_dotslice,j_bitfield)

global var [0..jtagnames.upb]byte refunitset

global var []byte D_binopset = (
	j_andl,j_orl,j_eq,j_ne,j_lt,j_le,j_gt,j_ge,j_add,j_sub,j_mul,j_div,j_idiv,
	j_irem,j_iand,j_ior,j_ixor,j_shl,j_shr,j_min,j_max,
	j_subref,j_addoffset,j_suboffset,j_concat,j_power, j_xorl, j_idivrem, j_atan2,j_fmod)

global var [0..jtagnames.upb]byte binopset

global var []byte D_monopset = (
	j_neg,j_abs,j_inot,j_sqr,j_cube,j_sign,j_sin,j_cos,j_tan,j_asin,
	j_acos,j_atan,j_ln,j_lg,j_log,j_exp,j_round,j_floor,j_ceil,j_fract,
	j_lwb,j_upb,j_len,j_bounds,j_sliceptr,
	j_bitwidth,j_bytesize, j_minvalue,j_maxvalue, j_notl,j_istruel)

global var [0..jtagnames.upb]byte monopset

global tabledata() []ichar sysfnnames =
	(sysfn_pushcallback,		$),
	(sysfn_popcallback,			$),
	(sysfn_mul_i128,			$),	
	(sysfn_idiv_i128,			$),
	(sysfn_dotindex,			$),
	(sysfn_dotslice,			$),
	(sysfn_popdotindex,			$),
	(sysfn_popdotslice,			$),
	(sysfn_power_i64,			$),
	(sysfn_callff_4,			$),
	(sysfn_callff_5,			$),
	(sysfn_callff_6,			$),
	(sysfn_callff_7,			$),
	(sysfn_callff_8,			$),
	(sysfn_callff_9,			$),
	(sysfn_callff_10,			$),
	(sysfn_callff_11,			$),
	(sysfn_callff_12,			$),
	(sysfn_callff_13,			$),
	(sysfn_callff_14,			$),
	(sysfn_init,				$),
!	(sysfn_initstatics,			$),
	(sysfn_stop,				$),
	(sysfn_print_startfile,		$),
	(sysfn_print_startstr,		$),
	(sysfn_print_startptr,		$),
	(sysfn_print_startcon,		$),
	(sysfn_print_setfmt,		$),
	(sysfn_print_nogap,			$),
	(sysfn_print_i64,			$),
	(sysfn_print_u64,			$),
	(sysfn_print_r64,			$),
	(sysfn_print_i128,			$),
	(sysfn_print_u128,			$),
	(sysfn_print_str,			$),
	(sysfn_print_ptr,			$),
	(sysfn_print_c8,			$),
	(sysfn_print_newline,		$),
	(sysfn_print_end,			$),
	(sysfn_read_i64,			$),
	(sysfn_read_r64,			$),
	(sysfn_read_fileline,		$),
	(sysfn_read_strline,		$),
	(sysfn_read_conline,		$),
!	(sysfn_fn_addresses,		$),

	(sysfn_get_nprocs,			$),
	(sysfn_get_nexports,		$),
	(sysfn_get_procname,		$),
	(sysfn_get_procaddr,		$),
	(sysfn_get_procexport,		$),

	(sysfn_nprocs,				$),
	(sysfn_nexports,			$),
	(sysfn_procnames,			$),
	(sysfn_procaddrs,			$),
	(sysfn_procexports,			$),

	(sysfn_sin,					$),
	(sysfn_cos,					$),
	(sysfn_tan,					$),
	(sysfn_asin,				$),
	(sysfn_acos,				$),
	(sysfn_atan,				$),
	(sysfn_ln,					$),
	(sysfn_lg,					$),
	(sysfn_log,					$),
	(sysfn_exp,					$),
	(sysfn_floor,				$),
	(sysfn_ceil,				$),
	(sysfn_fract,				$),
	(sysfn_round,				$),

end

!global var [sysfnnames.len]int sysfnlabels
=== mm_mcldecls.m 5/35 ===
import mm_decls
!import ax_decls

global tabledata() [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
	(a_cond,	$),		! a condition code for jcc/setcc
	(a_xreg,	$),		! xmm register
!	(a_string,	$),		! immediate string (for comments)
end

global tabledata() [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(intimm_val,	$),		!immediate int
	(realimm_val,	$),		!immediate real (mainly for dq etc)
	(realmem_val,	$),		!indirect real (for movq etc)
	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
	(def_val,		$),		!var/proc name
	(label_val,		$),		!label index
	(name_val,		$),		!immediate string must be output as ah unquoted name
	(syscall_val,	$),		!
end

global record opndrec =		!up to 32 bytes
	var ref strec labeldef	!nil, or handle of strec for label
	union
		var ref strec def
		var int64 value		!immediate value
		var real64 xvalue	!immediate real value, mainly for dq
		var ichar svalue	!immediate string
		var int labelno
		var int sysfn
	end

	var byte size			!byte size of operand: usually 1,2,4,8,16
	var byte mode			!a_reg etc, low level operand details
	var byte reg			!0, or main register
	var byte regix			!0, or index register

	var byte valtype		!interpretation of def/code/value/svalue
	var byte scale			!1, or scale factor for regix
	var int16 offset		!extra offset to label for mem/imm modes

	var byte addrsize	!4 or 8 for a_mem when regs are involved
!	var byte valtypex		!valtypex is 0 (no value or int) or 'R'/'S' in ma

end

global record mclrec =		!32 bytes
	var ref mclrec nextmcl
	var ref opndrec a,b
	var byte opcode
	union
		var byte cond
		var byte isglobal
		var byte sysindex
	end
	var byte fileno
	var byte spare
	var int lineno
end

global tabledata() []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_comment,			$,		0,		0),		!
	(m_blank,			$,		0,		0),		!
	(m_labelname,		$,		0,		0),		!
	(m_define,			$,		0,		0),		!

	(m_label,			$,		1,		0),		!
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

	(m_bsf,				$,		0,		0),		!

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
	(m_ddoffset,		$,		1,		0),		!

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

global tabledata() [0:]ichar regnames, [0:]byte regcodes =
	(rnone=0,	$,	0),			!
	(r0,		$,	0),			!d0 rax
	(r1,		$,	3),			!d1 rbx
	(r2,		$,	6),			!d2 rsi
	(r3,		$,	7),			!d3 rdi
	(r4,		$,	10),		!d4 r10
	(r5,		$,	11),		!d5 r11
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

global tabledata() [0:]ichar condnames, [0:]ichar asmcondnames =

	(ov_cond=0,	"ov",	"o"),
	(nov_cond=1,	"nov",	"no"),

	(ltu_cond=2,	"ltu",	"b"),
	(geu_cond=3,	"geu",	"ae"),

	(eq_cond=4,	"eq",	"z"),
	(ne_cond=5,	"ne",	"nz"),

	(leu_cond=6,	"leu",	"be"),
	(gtu_cond=7,	"gtu",	"a"),

	(s_cond=8,	"s",	"s"),
	(ns_cond=9,	"ns",	"ns"),

	(p_cond=10,	"p",	"p"),
	(np_cond=11,	"np",	"np"),

	(lt_cond=12,	"lt",	"l"),
	(ge_cond=13,	"ge",	"ge"),

	(le_cond=14,	"le",	"le"),
	(gt_cond=15,	"gt",	"g"),

	(flt_cond=16,	"flt",	"b"),		!special floating point codes
	(fge_cond=17,	"fge",	"ae"),
	(fle_cond=18,	"fle",	"be"),
	(fgt_cond=19,	"fgt",	"a")
end


!I use my own register designations Dn, An, Wn, Bn (8,4,2,1 bytes),
!which have a more sensible order than the official ones.
!The mapping is shown against Dn. Some (not all) of the official register
!names are used too

!Regindex is the ordinal value used to represent the register: 1..16
!This table is intended for initialising the global symbol table

global tabledata []ichar dregnames, []byte regsizes, []byte regindices =
	("d0",		8,	r0),		!rax	d0..d9 are for general use
	("d1",		8,	r1),		!rbx	(some registers have special uses with some
	("d2",		8,	r2),		!rsi     instructions, eg, b10 (cl) for var shift count)
	("d3",		8,	r3),		!rdi
	("d4",		8,	r4),		!r10
	("d5",		8,	r5),		!r11
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
	("rbx",		8,	r1),
	("rcx",		8,	r10),
	("rdx",		8,	r11),
	("rsi",		8,	r2),
	("rdi",		8,	r3),
	("rbp",		8,	r14),
	("rsp",		8,	r15),
	("r8",		8,	r12),
	("r9",		8,	r13),
	("r10",		8,	r4),
	("r11",		8,	r5),
	("r12",		8,	r6),
	("r13",		8,	r7),
	("r14",		8,	r8),
	("r15",		8,	r9),

	("eax",		4,	r0),
	("ebx",		4,	r1),
	("ecx",		4,	r10),
	("edx",		4,	r11),
	("esi",		4,	r2),
	("edi",		4,	r3),
	("ebp",		4,	r14),
	("esp",		4,	r15),
	("r8d",		4,	r12),
	("r9d",		4,	r13),
	("r10d",	4,	r4),
	("r11d",	4,	r5),
	("r12d",	4,	r6),
	("r13d",	4,	r7),
	("r14d",	4,	r8),
	("r15d",	4,	r9),

	("ax",		2,	r0),
	("bx",		2,	r1),
	("cx",		2,	r10),
	("dx",		2,	r11),
	("si",		2,	r2),
	("di",		2,	r3),
	("bp",		2,	r14),
	("sp",		2,	r15),
	("r8w",		2,	r12),
	("r9w",		2,	r13),
	("r10w",	2,	r4),
	("r11w",	2,	r5),
	("r12w",	2,	r6),
	("r13w",	2,	r7),
	("r14w",	2,	r8),
	("r15w",	2,	r9),


	("al",		1,	r0),
	("bl",		1,	r1),
	("cl",		1,	r10),
	("dl",		1,	r11),

	("ah",		1,	r16),
	("bh",		1,	r17),
	("ch",		1,	r18),
	("dh",		1,	r19),

	("sil",		1,	r2),
	("dil",		1,	r3),
	("bpl",		1,	r14),
	("spl",		1,	r15),

	("r8b",		1,	r12),
	("r9b",		1,	r13),
	("r10b",	1,	r4),
	("r11b",	1,	r5),
	("r12b",	1,	r6),
	("r13b",	1,	r7),
	("r14b",	1,	r8),
	("r15b",	1,	r9),

end

global var []ichar xmmregnames = (
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

global var []ichar fregnames = (
	"st0",
	"st1",
	"st2",
	"st3",
	"st4",
	"st5",
	"st6",
	"st7")

global var []ichar mregnames = (
	"mmx0",
	"mmx1",
	"mmx2",
	"mmx3",
	"mmx4",
	"mmx5",
	"mmx6",
	"mmx7")

!global tabledata() [0:]ichar condnames =
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
end

global tabledata() [0:]ichar segmentnames =
	(no_seg=0,		$),
	(code_seg,		$),
	(idata_seg,		$),
	(zdata_seg,		$),
	(rodata_seg,	$),
	(impdata_seg,	$),
end

global tabledata() [0:]ichar reftypenames =	!use during pass2
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

=== mm_start.m 6/35 ===
!New Q Whole Project Compiler

import msys
import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_lex
import mm_tables
import mm_parse
import mm_name
import mm_type
import mm_lib
import mm_diags

import mm_gen

IMPORT MA_GENSS


tabledata() []ichar optionnames=

	(exe_sw,		"exe"),			!(default)
	(obj_sw,		"obj"),

	(gcc_sw,		"gcc"),
	(tcc_sw,		"tcc"),
	(bcc_sw,		"bcc"),

	(opt_sw,		"opt"),

	(compile_sw,	"c"),
	(link_sw,		"link"),		!(default)
	(run_sw,		"run"),

	(load_sw,		"load"),
	(parse_sw,		"parse"),
	(ma_sw,			"ma"),
	(name_sw,		"name"),
	(type_sw,		"type"),
	(gen1_sw,		"gen1"),
	(gen2_sw,		"gen2"),
	(gen3_sw,		"gen3"),
	(gen4_sw,		"gen4"),


	(time_sw,		"time"),
	(v_sw,			"v"),
	(v2_sw,			"v2"),
	(quiet_sw,		"q"),
	(help_sw,		"h"),
	(help2_sw,		"help"),
	(ext_sw,		"ext"),
	(out_sw,		"out"),
	(nosys_sw,		"nosys"),
	(unused_sw,		"unused"),
	(debug_sw,		"debug"),
	(set_sw,		"set"),
	(writelibs_sw,	"writelibs")
end

const logfile="qx.log"

var int totallines=0
var int nstringobjects=0

!var int fexe
!var int fobj
!var int fwritelibs
!var int fshowtiming
!var int fshowpcl1					!set to 1 to display pcl
!var int fshowpcl2
!var int fshowmcl1
!var int fshowasm
!var int fshowast1
!var int fshowast2
!var int fshowast3
!var int fshowst
!var int fshowstflat
!var int fshowtypes
!var int ccompiler = gcc_cc
!var int foptimise
!!var int fwritema

!cc_mode used in production mode only

var [sysparams.len]ichar extraparams	!after ":"
var [sysparams.len]ichar extravalues
var int nextraparams=0

const maxoptionvar=25
var [maxoptionvar]ichar optionvars
var [maxoptionvar]ichar optionvalues
var int noptionvars

global var int startclock, endclock

global proc start_common(int itarget)=
!called from front-end

var int s,t,i
var int stopcode
var ichar file
var filehandle f
var int hashvalue,hv16

startclock:=os_clock()

target:=itarget

!CPL $LINENO
ctarget:=tg_ctarget[target]
islinux:=tg_islinux[target]
targetbits:=tg_targetbits[target]
targetsize:=targetbits/8

addoptionvar("target",targetnames[target])
addoptionvar("os",targetosnames[target])
addoptionvar("targetlang",targetlangnames[target])
addoptionvar("ctarget",(ctarget|"1"|"0"))
!CPL $LINENO

initdata()
!CPL $LINENO

getinputoptions()
!CPL $LINENO

if fdebugcompiler then
	debugcompiler()
	stop
fi

!CPL "NORMAL COMPILE"

if not fverbose and not fquiet then
	println "Compiling",inputfiles[1],"to",outfile
fi

initsearchdirs()

do_loadmodules()

do_parse()
do_writema()

do_name()
do_type()

do_codegen()

endclock:=os_clock()

if cc_mode=run_mode then
	do_runprog()
fi

if fverbose then
	println "Finished."
fi

if fshowtiming then
	showtiming()
fi

stop 0
end

proc debugcompiler=
var [200]char str
var int s,t,i
var int stopcode
var ichar file
var filehandle logdev

if ctarget then
	fshowasm:=passlevel>=5
else
	fshowss:=passlevel>=6
	fshowmcl1:=passlevel>=5
	fshowpcl1:=passlevel>=4
fi

!fshowast1:=1
!fshowast2:=passlevel>=2
fshowast3:=passlevel>=3
!fshowst:=1
!fshowtypes:=1
!
cc_mode:=0

println "*************DEBUG:Compiling",inputfiles[1],"to",outfile

CPL =PASSLEVEL

initsearchdirs()
remove(logfile)

do_loadmodules()

if passlevel>=1 then

	do_parse()
	if fshowast1 then showast("AST1") fi
	do_writema()
fi

if passlevel>=2 then
	do_name()
	if fshowast2 then showast("AST2") fi
fi

if passlevel>=3 then
	do_type()
	if fshowast3 then showast("AST3") fi
fi

if passlevel>=4 then
	do_codegen_debug()
fi

endclock:=os_clock()

if fshowtiming then
	showtiming()
fi

if fverbose then
	println "Finished."
fi

!CPL =FSHOWAST1
CPL =CTARGET

if fshowpcl1 or fshowpcl2 or fshowast1 or fshowast2 or fshowast3 or\
	fshowst or fshowstflat or fshowtypes or fshowmcl1 or fshowss or fshowasm then
	logdev:=fopen(logfile,"w")

	if fshowasm then addtolog(outfilesource,logdev) fi
	if fshowss then addtolog("SS",logdev) fi
!	if fshowmcl1 then addtolog("MCL",logdev) fi
!	if fshowmcl1 then addtolog("MCL.ASM",logdev) fi
	if fshowmcl1 then addtolog(outfilesource,logdev) fi
!	if fshowpcl2 then addtolog("PCL2",logdev) fi
	if fshowpcl1 then addtolog("PCL",logdev) fi
	if fshowast3 then addtolog("AST3",logdev) fi
	if fshowast2 then addtolog("AST2",logdev) fi
	if fshowast1 then addtolog("AST1",logdev) fi
	if fshowst then	showsttree("SYMBOL TABLE",logdev) fi
	if fshowstflat then	showstflat("FLAT SYMBOL TABLE",logdev) fi
	if fshowtypes then printmodelist(logdev) fi

	fclose(logdev)
!	sprintf(&.str,"\\m\\med.bat %s",logfile)
	print @&.str,"\\m\\med.bat",logfile

	if checkfile("mc.m") then
		os_execwait(&.str,1,nil)
!		os_execcmd(&.str,1)
	else
		println "Diagnostic outputs written to",logfile
	fi
fi

!CPL =NPCL:"s,",=PCLREC.BYTES
!CPL =NMCL:"s,",=MCLREC.BYTES
!CPL =NUNITS:"s,",=UNITREC.BYTES
!
stop 0
end

proc do_loadmodules=
	if fbundled then
		loadmafile()
	fi
	loadmainmodule(inputfiles[1])
end

proc do_parse=
	for i:=2 to nmodules do
		parsemodule(i)
	od
	parsemodule(1)

	fixusertypes()
end

proc do_name=
	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
end

proc do_type=
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od
end

proc do_runprog=
var [300]char str
var int i

if islinux then
	print @&.str,"./",,outfilebin
else
	strcpy(&.str,outfilebin)
fi

for i to nextraparams do
	strcat(&.str," ")
	strcat(&.str,extraparams[i])
	if extravalues[i] then
		strcat(&.str,":")
		strcat(&.str,extravalues[i])
	fi
od

os_execwait(&.str)

end

function loadmainmodule(ichar filespec)int=
!Used for main module. Will always be first module loaded, module list
!will be empty.
!Load file as string
!extract modulename
!call compilemodile(modulename, filename, source)
	var [100]char modulename
	var [300]char path
	var ref byte source
	var int status
	var modulerec m
	var int i,flag,fileno

!set up special module to represent the whole program
	pcm_clearmem(&moduletable[0],modulerec.bytes)

	sourcefilenames[0]:="<dummy file>"
	sourcefilepaths[0]:="<dummy path>"
	sourcefiletext[0]:="<sourcefile0>"
	sourcefilesizes[0]:=strlen(sourcefiletext[0])

	moduletable[0].name:="PROGRAM"
	moduletable[0].fileno:=0

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
	moduletable[0].stmodule:=stprogram

	fileno:=getmainfile(filespec)

	infotext:=nil
	if ctarget then
		infotext:=cast(readfile(changeext(filespec,"txt")))
	fi
	
	strcpy(&.modulename,extractbasefile(filespec))
	strcpy(&.path,extractpath(filespec))
	if path[1] then
		addsearchdir(&.path)
	fi

	addmodule(&.modulename,fileno,flag)

	addspecialtypes()

return 1
end

function addmodule(ichar modulename,int fileno,&exportflag)int=
!Add new module with given name
!Source for module is already loaded in <source>, of given length
!return module no just added

!return new module number, or 0
!The module entry is NOT linked to the module list until imports have been loaded.

var modulerec m
const maximports=maxmodule
var [maximports]ichar importnames
var [0..maximports]byte importflags
var [maximports]int importmoduleno
var int nimports,i,status,k,flag,j,newmodno
var ref modulerec pmodule

convlcstring(modulename)

pcm_clearmem(&m,m.bytes)

m.name:=pcm_copyheapstring(modulename)
m.fileno:=fileno

stmodule:=createnewmoduledef(stprogram,addnamestr(m.name))
m.stmodule:=stmodule

if nmodules>=maxmodule then
	loaderror("Too many modules",modulename)
fi

pmodule:=&moduletable[newmodno:=++nmodules]

pmodule^:=m
pmodule^.importmap[newmodno]:=1
m.stmodule^.moduleno:=newmodno

memset(&importflags,0,importflags.bytes)
nimports:=readimportlist(&m,&importnames,&importflags,maximports)

for i to nimports do
	flag:=0
	if fverbose=2 then
		println "Load import for",modulename,=importnames[i]
	fi
	k:=loadimport(importnames[i],flag,modulename)
	if flag then
		importflags[i]:=1
	fi
	pmodule^.importmap[k]:=1
	importmoduleno[i]:=k
od

!Deal with any "*" imports (or where export/endexport were used)
for i:=1 to nimports when importflags[i] do
	k:=importmoduleno[i]
	for j:=1 to nmodules do
		if moduletable[k].importmap[j] then		!add that to this module
			pmodule^.importmap[j]:=1
		fi
	od
od

exportflag:=importflags[0]

!CPL "&&&&&&&& ADDED MODULE",MODULENAME
return newmodno
end

function loadimport(ichar modulename,int &exportflag,ichar ownername)int=
!Look at request for adding module, which might already be loaded
!Checks also that modules are added in the right order
!Calls loadmodule to process the module and deal with /its/ imports
!return modulen no of the existing or newly added module

var int i,fileno
var ichar ifilespec
var [300]char filespec
var ref char source
var ichar newname

newname:=modulename

for i:=1 to nmodules do
	if eqstring(moduletable[i].name,newname) then		!already loaded
		return i
	fi
od

fileno:=getmodulefile(modulename,ownername)

return addmodule(newname,fileno, exportflag)
end

function readimportlist(ref modulerec m, ref[]ichar importnames,
							ref[0:]byte importflags, int maximports)int=
var int n,flag,exportflag,i
var ichar s
var [100]char name,libname
var ichar iname

startlex("IMPORTS",m^.fileno)

exportflag:=0

n:=0
do
	lexreadtoken()
	case nextlx.symbol
	when eofsym then
		exit
	when semisym,eolsym then

	when rawnamesym then
		flag:=0
		if checkname("import") then
			pslex()
			if nextlx.symbol=opsym and nextlx.subcode=j_mul then
				flag:=1
				pslex()
			fi

			if nextlx.symbol<>rawnamesym then
				abortprogram("import: modulename expected")
			fi
			if ++n>=maximports then			!allow for extra msys module
				abortprogram("too many imports")
			fi

!			strcpy(&.name,convertzstring(nextlx.svalue,nextlx.length))

!perform mapping of name
			iname:=mapimport(nextlx.svalue)
			importnames^[n]:=pcm_copyheapstring(iname)
			importflags^[n]:=flag

		elsif checkname("importpath") then
			pslex()
			if nextlx.symbol=stringconstsym then
				addsearchdir(nextlx.svalue)
				pslex()
			else
				abortprogram("string path expected")
			fi

		elsif checkname("mapmodule") then
			domapmodule()

		elsif checkname("as") then		!skip two tokens 'as name'
			pslex()
			pslex()

		else
			exit
		fi
	else
		exit
	esac
od

!make sure msys is included on first module
var int needmsys
var ichar msysname

!msysname:=(ctarget|"msysc"|"msys")
msysname:=(ctarget|"msysnewc"|"msysnew")

if nmodules=1 then
	needmsys:=1

	for i to n do
		if eqstring(importnames^[i],msysname) then
				needmsys:=0
				exit
		fi
	od

	if fnomsys then needmsys:=0 fi

	if needmsys then
		++n
		importnames^[n]:=pcm_copyheapstring(msysname)
!		importnames^[n]:="msys"
		importflags^[n]:=0
	fi
fi


importflags^[0]:=exportflag

return n
end

proc pslex=
	static var [256]char psname

	prescanmode:=1
	lexreadtoken()
	prescanmode:=0

	if nextlx.symbol=rawnamesym then
		strcpy(&.psname,convertzstring(nextlx.svalue, nextlx.length))
		nextlx.svalue:=&.psname
	fi
end

proc initdata=
	pcm_init()
	lexsetup()
	initassemsymbols()
	inittypetables()
	initqclib()
end

proc initsearchdirs=
var [300]char str1,str2
var int i

!DIFFERENT SETUP NEEDED FOR LINUX

nsearchdirs:=0
!addsearchdir("c:/mcc/")
addsearchdir("c:/mx/")
addsearchdir("c:/ax/")
addsearchdir(os_getmpath())
addsearchdir(os_gethostname())
addsearchdir("./")
end

proc addsearchdir(ichar path)=
	for i to nsearchdirs do
		if eqstring(searchdirs[i],path) then return fi
	od
	if nsearchdirs>maxsearchdirs then
		loaderror("Too many search paths")
	fi
	searchdirs[++nsearchdirs]:=pcm_copyheapstring(path)
end

proc showsearchdirs=
var int i

println "Import search paths:"
for i to nsearchdirs do
	if searchdirs[i]^ then
		println i,,":",searchdirs[i]
	else
		println i,,": ."
	fi
od
println
end

proc showast(ichar filename)=
	var filehandle f

	f:=fopen(filename,"w")
	return unless f

	println @f,"PROC",filename
	printcode(f,"")
	println @f
	fclose(f)
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
end

global proc showtiming=
var real t

t:=endclock-startclock

if nalllines then
	println "Time:",t
else
	println "Time:",t,nalllines
fi
end

proc getinputoptions=
const slash='-'
var int i,j,k
var int paramno,pmtype,sw,ncolons,passfixed
var ichar name,value,filename,ext

paramno:=2
ncolons:=0

while pmtype:=nextcmdparam(paramno,name,value,"m") do
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
		if ninputfiles>=maxmodule then
			loaderror("Too many input files")
		fi
		convlcstring(name)
		inputfiles[++ninputfiles]:=pcm_copyheapstring(name)
	when pm_libfile then
		if nlibfiles>=maxlibfile then
			loaderror("Too many lib files")
		fi
		libfiles[++nlibfiles]:=pcm_copyheapstring(name)
	when pm_colon then
		if ++ncolons>1 then
			name:=":"
			value:=nil
			goto doextra
		fi
	when pm_extra then
doextra::
		extraparams[++nextraparams]:=pcm_copyheapstring(name)
		extravalues[nextraparams]:=pcm_copyheapstring(value)
	esac

od

if cc_mode=0 then
!	cc_mode:=compile_mode
	cc_mode:=link_mode
fi

if linkoption=nil then linkoption:="exe" fi

if ninputfiles=0 and not fwritelibs then
	showcaption()
	println "Usage:"
	println "	",,sysparams[1],"filename[.m]     # Compile project to executable"
	println "	",,sysparams[1],"-help            # Other options"
	stop

elsif ninputfiles=1 then
	filename:=inputfiles[1]				!primary file name

	ext:=extractext(filename)
	if eqstring(ext,"ma") then
		fbundled:=1
		mafilename:=pcm_copyheapstring(filename)
		inputfiles[1]:=pcm_copyheapstring(changeext(filename,"m"))
	fi

	outfilesource:=pcm_copyheapstring(changeext(filename,targetexts[target]))

	if islinux and eqstring(linkoption,"exe") then
		linkoption:=""
	fi

	outfilebin:=pcm_copyheapstring(changeext(filename,linkoption))


	if cc_mode=compile_mode then
		if destfilename then outfilesource:=destfilename fi
		outfile:=outfilesource
	else
		if destfilename then outfilebin:=destfilename fi
		outfile:=outfilebin
	fi
!CPL =OUTFILE
!CPL =OUTFILESOURCE
!CPL =OUTFILEBIN
else
	loaderror("Specify one lead module only")
fi
end

proc do_option(int sw, ichar value)=
var int length

case sw
when compile_sw then
	cc_mode:=compile_mode

when link_sw then
	cc_mode:=link_mode

when run_sw then
	cc_mode:=run_mode

when gcc_sw then ccompiler:=gcc_cc
when tcc_sw then ccompiler:=tcc_cc
when bcc_sw then ccompiler:=bcc_cc

when opt_sw then foptimise:=1

when exe_sw then
	linkoption:="exe"

when obj_sw then
	linkoption:="obj"

when time_sw then
	fshowtiming:=1

when v_sw then
	fverbose:=1

when v2_sw then
	fverbose:=2

when quiet_sw then
	fquiet:=1

when help_sw,help2_sw then
	showhelp()
	stop

when ext_sw then
	dointlibs:=0

when writelibs_sw then
	fwritelibs:=1

when out_sw then
	destfilename:=pcm_copyheapstring(value)

when unused_sw then
	fcheckunusedlocals:=1

when nosys_sw then
	fnomsys:=1

when debug_sw then
	fdebugcompiler:=1

when load_sw then passlevel:=0
when parse_sw then passlevel:=1
when name_sw then passlevel:=2
when type_sw then passlevel:=3
when gen1_sw then passlevel:=4
when gen2_sw then passlevel:=5
when gen3_sw then passlevel:=6
when gen4_sw then passlevel:=7

when set_sw then
	dosetoptionvar(value)

when ma_sw then
	fwritema:=1

esac

end

proc showcaption=
println "M Compiler", $date, $time
end

proc addtolog(ichar filename, filehandle logdest)=
var filehandle f
var int c

f:=fopen(filename,"rb")

if f=nil then return fi

do
	c:=fgetc(f)
	exit when c=c_eof
	fputc(c,logdest)
od
fclose(f)
end

!proc showoptionvars=
!println "Optionvars",noptionvars
!for i to noptionvars do
!	cpl i,optionvars[i],"=",optionvalues[i]
!od
!cpl
!
!println "Modulemap",nmodulemap
!for i to nmodulemap do
!	cpl i,genericmodules[i],"=>",actualmodules[i]
!od
!cpl
!CPL "MODULES",nmodules
!for i:=0 to nmodules do
!	cpl i, moduletable[i].name
!od
!
!end

proc addoptionvar(ichar name, value)=
	if noptionvars>=maxoptionvar then
		abortprogram("Too many option vars")
	fi
	for i to noptionvars do
		if eqstring(name,optionvars[i]) then
			println name
			abortprogram("Dupl optionvar")
		fi
	od

	optionvars[++noptionvars]:=pcm_copyheapstring(name)
	if value=nil then
		optionvalues[noptionvars]:="1"
	else
		optionvalues[noptionvars]:=pcm_copyheapstring(value)
	fi
end

global proc addmodulemapping(ichar old, new, optionname=nil, valuename=nil)=
	var int option
!
!CPL "ADDMAPPING",OLD,NEW


	if optionname then					!conditional
		option:=findoptionvar(optionname)
		if option then
			if eqstring(optionname,optionvars[option]) then
				if not eqstring(optionvalues[option],valuename) then
					return
				fi
			fi
		else
			return						!just assume false when not found
!			println optionname
!			abortprogram("Can't find option")
		fi
	fi

	if nmodulemap>=maxmodulemap then
		abortprogram("Too many module mappings")
	fi
	for i to nmodulemap do
		if eqstring(old,genericmodules[i]) then
			println old
			abortprogram("Dupl module mapping")
		fi
	od
	genericmodules[++nmodulemap]:=pcm_copyheapstring(old)
	actualmodules[nmodulemap]:=pcm_copyheapstring(new)
end

proc dosetoptionvar(ichar s)=
	var ref char t
	var [256]char name
	var [256]char value

	if s=nil or s^=0 then
		abortprogram("set:no option")
	fi

	t:=&.name
	strcpy(t,s)
	value[1]:=0

	while t^ do
		if t^=':' then
			t^:=0
			strcpy(&.value,t+1)
			exit
		fi
		++t
	od

	if value[1]=0 then
		strcpy(&.value,"1")
	fi

	addoptionvar(&.name,&.value)
end

function findoptionvar(ichar name)int=
	for i to noptionvars do
		if eqstring(name, optionvars[i]) then
			return i
		fi
	od
	return 0
end

proc getpsname(ichar dest)=
!read raw name or string from prescan code from next symbol
!store string into dest, and move to next symbol
	var [64]char str

	pslex()	
	case nextlx.symbol
	when rawnamesym then
	when stringconstsym then
	when intconstsym then
		nextlx.svalue:=strint(int(nextlx.svalue))
		
	else
		abortprogram("map1")
	esac
	strcpy(dest,nextlx.svalue)
	pslex()
end

proc domapmodule=
!working in prescan using nextlx; have just seen "mapmodule"
	var [256]char genname, actualname,optionname,valuename
	var int cond,option

	getpsname(&.genname)

	if nextlx.symbol<>sendtosym then abortprogram("=> expected") fi
	getpsname(&.actualname)

	cond:=0

	if nextlx.symbol=rawnamesym and checkname("when") then
		getpsname(&.optionname)
		if nextlx.symbol=opsym and nextlx.subcode=j_eq then
			getpsname(&.valuename)
		else
			strcpy(&.valuename,"1")
		fi
		cond:=1
	fi

	while nextlx.symbol not in [eolsym,eofsym] do pslex() od

	if cond then
		addmodulemapping(&.genname, &.actualname, &.optionname, &.valuename)
	else
		addmodulemapping(&.genname, &.actualname, nil,nil)
	fi
end

global function mapimport(ichar name)ichar=
	for i to nmodulemap do
		if eqstring(name,genericmodules[i]) then
			return actualmodules[i]
		fi
	od
	return name
end

proc do_writema=
	if fwritema then
		if fbundled then
			loaderror("-ma used with .ma input")
		fi
		writemafile(inputfiles[1],destfilename)
		stop
	fi
end
=== msysnew.m 7/35 ===
import clib
import mlib

global record procinforec=
	var word16		fnindex
	var byte		rettype
	var byte		nparams
	var [12]byte	paramlist
end

!for print/read routines
!------------------------------------------
record fmtrec=	! (default)
	var byte	minwidth	! n (0)   min field width (0 if not used or don't care)
	var i8		precision	! .n (0)   number of decimals/significant figures/max width
	var byte	base		! B,H or Xn (10)  2 to 16

	var char	quotechar	! Qc (0)   0 or '"' or c
	var char	padchar		! Pc, Z (' ')
	var char	realfmt		! E,F,G ('f') 'e' or 'f' or 'g'

	var char	plus		! (0)   0 or '+'
	var char	sepchar		! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
	var char	lettercase	! A,a ('A') 'A' or 'a'
	var char	justify		! JL, JR, JC ('R') 'L' or 'R' or 'C'?
	var char	suffix		! Tc (0)   0 or 'B' or 'H' or c
	var char	usigned		! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
	var char	charmode	! C,D (0)  0 or 'C' or 'D'	o/p int as int or single char or double/multi-char
	var char	heapmode	! M (0)  'M' for str-functions, return ptr tp heap string
	var char	param		! Use int value for <fmtparam>
	var byte	spare
end

var int fmtparam			!as set with :'V'

enum (std_io,file_io,str_io)

const comma = ','

var int needgap			= 0
var int outdev			= std_io
var filehandle outchan	= nil
var ref char fmtstr 	= nil

const maxiostack=10
var [maxiostack]filehandle	outchan_stack
var [maxiostack]int			outdev_stack
var [maxiostack]ref char	fmtstr_stack
var [maxiostack]byte		needgap_stack

var [maxiostack]ref char	ptr_stack		!this one doesn't need pushing, as each is pointed to from outchan
var int niostack=0

var [0:]char digits=A"0123456789ABCDEF"
const onesixty=360
var fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

!Read buffer vars
const rd_buffersize = 16384	!total capacity of line buffer

var ref char rd_buffer		! point to start of read buffer
var int rd_length			! length of this line (as read by readln)
var ref char rd_pos			! current position it's up to (next read starts here)
var ref char rd_lastpos		! set by sread() just before reading used for reread()
var int termchar			! terminator char set by readxxx()
var int itemerror			!	set by some read functions, eg for reals

!------------------------------------------

const maxparam=128
global var int nsysparams
global var [maxparam]ichar sysparams

const maxcallback=8
var [0..maxcallback,8]word64 callbackstack
var int ncallbacks=0

var word64 mask63	= 0x7FFF'FFFF'FFFF'FFFF
var real offset64	= 9223372036854775808.0		! 2**63 as r64
var real offset32	= 9223372036854775808.0		! 2**63 as r32

global proc m$init=
var int32 nargs
var int nargs64
var ref[]ichar args
var ref[]ichar env
static var [128]byte startupinfo			! 68 or 104 bytes
var int res
var ichar s

res:=__getmainargs(&nargs,cast(&args),cast(&env),0,cast(&startupinfo))

nsysparams:=nargs

if nsysparams>maxparam then
	printf("Too many params\n")
	stop 50
fi

nargs64:=nargs			!bug when using 32-bit limit when compild with mm
for i:=1 to nargs64 do
	sysparams[i]:=args^[i]
od
end

global proc m$stop(int n)=

assem
	mov d10,[n]
	mov d0,`exit
	call m$callff_4
end

end

global threadedproc m$callff_4=
!0..4 params have been loaded to R10..13
!The foreign function address is in D0
!set up the stack, keeping it aligned, and call the function, adjusting the
!stack before returning.
!For functions rather than procs, the return value will be in A0/D0/XMM0

assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:
	sub Dstack,40			!add an extra 8 bytes to align
	call D0
	add Dstack,40			!unstack the dummy 4 params, plus 8 more bytes
	ret

aligned:
	sub Dstack,32
	call D0
	add Dstack,32
	ret
end

end

global threadedproc m$callff_5=
!one param already pushed. 
!
!There might be another way to do this:
!leave retaddr in place, move P5 this side of retaddr, but leave original P5
!there, and use retn 8 to skip it

assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr
	pop D2					!P5
	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack
	push D2					!P5
	sub Dstack,32
	call D0
	add Dstack,48			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr
	pop D2					!P5
	push D1					!push ret addr back
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,40			!pop all
	ret
end

end

global threadedproc m$callff_6=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,56			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6

	push D1					!push ret addr back

	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,48			!pop all
	ret
end

end

global threadedproc m$callff_7=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,64			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7

	push D1					!push ret addr back

	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,56			!pop all
	ret
end

end

global threadedproc m$callff_8=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,72			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8

	push D1					!push ret addr back

	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,64			!pop all
	ret
end

end

global threadedproc m$callff_9=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,80			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr
	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9

	push D1					!push ret addr back
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,72			!pop all
	ret
end

end

global threadedproc m$callff_10=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,88			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10

	push D1					!push ret addr back

	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,80			!pop all
	ret
end

end

global threadedproc m$callff_11=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,96			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11

	push D1					!push ret addr back

	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,88			!pop all
	ret
end

end

global threadedproc m$callff_12=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,104			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12

	push D1					!push ret addr back

	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,96			!pop all
	ret
end

end

global threadedproc m$callff_14=
static var word64 p13,p14
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12
	pop u64 [p13]			!P12
	pop u64 [p14]			!P14

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push u64 [p14]		!P14
	push u64 [p13]		!P13
	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,120			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12
	pop u64 [p13]			!P12
	pop u64 [p14]			!P14

	push D1					!push ret addr back

	push u64 [p14]		!P14
	push u64 [p13]		!P13
	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,112			!pop all
	ret
end

end

global proc m$pushcallback=
!save registers rbx, rsi,rdi, r12..r15 to small stack
!Note must take care not to overwrite any of those while saving

!if ncallbacks=maxcallback then
!	println "Callback overflow"
!	stop 1
!fi

assem
	inc word32 [ncallbacks]
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]

	mov [D4],rbx
	mov [D4+8],rsi
	mov [D4+16],rdi
	mov [D4+24],r12
	mov [D4+32],r13
	mov [D4+40],r14
	mov [D4+48],r15
end
end

global proc m$popcallback=
!restore registers rbx, rsi,rdi, r12..r15 from small stack
assem
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]
	mov rbx,[D4]
	mov rsi,[D4+8]
	mov rdi,[D4+16]
	mov r12,[D4+24]
	mov r13,[D4+32]
	mov r14,[D4+40]
	mov r15,[D4+48]
	dec word32 [ncallbacks]
end
end

global function m$getdotindex(word64 a,int i)int=
!return (a iand (1dw<<i))>>i
return (a iand (1<<i))>>i
end

global proc m$setdotindex(ref word64 a, int i,x)=
var ref word32 a32

!see comments on setdotslice
if i>=32 then
!	a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
	a^:=(a^ iand inot (1<<i)) ior (word64(x)<<i)
else
	a32:=cast(a)
	a32^:=(a32^ iand inot (1<<i)) ior (word(x)<<i)
fi
end

global function m$getdotslice(word64 a,int i,j)int=
if i>=j then
	return (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))
else
	return (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
fi
end

global proc m$setdotslice(ref word64 a, int i,j,word64 x)=
!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
var int w
var word64 mask64
var word mask
var ref word32 a32

if i>j then println "SETDOTSLICE?"; stop 52 fi

!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing
!to bytes beyond the 32-bit value
!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too

if j>=32 then
	mask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
	a^:=(a^ iand inot mask64) ior x<<i
else
	a32:=cast(a)
	mask:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
	a32^:=(a32^ iand inot mask) ior x<<i
fi

end

function m$get_nprocs:int=
	assem
		mov D0,[$nprocs]
	end
end

function m$get_procname(int n)ichar=
	assem
		lea D0,[$procnames]
		mov D1,[n]
		mov D0,[D0+D1*8-8]
!		mov D0,[sss]
	end
end

function m$get_procaddr(int n)ref proc=
	assem
		lea D0,[$procaddrs]
		mov D1,[n]
		mov D0,[D0+D1*8-8]
	end
end

global function m$get_nexports:int=
	assem
		mov D0,[$nexports]
	end
end

global function m$get_procexport(int n)ref void=
	assem
		lea D0,[$procexports]
		mov D1,[n]
		shl D1,1
		lea D0,[D0+D1*8-16]
	end
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

global proc m$print_startfile(ref void dev)=
	pushio()
	outchan:=cast(dev)
	if dev then
		outdev:=file_io
	else
		outdev:=std_io
	fi
end

global proc m$print_startstr(ref char s)=
	var ref ref char p
	pushio()

	ptr_stack[niostack]:=s
	p:=&ptr_stack[niostack]

	outchan:=cast(p)
	outdev:=str_io
end

global proc m$print_startptr(ref ref char p)=
	pushio()

	outchan:=cast(p)
	outdev:=str_io
end

global proc m$print_startcon=
	pushio()
	outdev:=std_io
end

global proc m$print_setfmt(ref char format)=
	fmtstr:=format
end

global proc m$print_end=
	needgap:=0
	nextfmtchars(1)
	if niostack=0 then return fi
	outchan	:= outchan_stack[niostack]
	outdev	:= outdev_stack[niostack]
	fmtstr	:= fmtstr_stack[niostack]
	needgap	:= needgap_stack[niostack]
	--niostack
end

global proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
	var [20]char s

	if fmtstyle=nil then
		fmtstyle:="z8H"
	fi
	m$print_u64(a,fmtstyle)
end

!global proc m$print_bool(int a,ichar fmtstyle=nil)=
!	[20]char s
!	nextfmtchars()
!	printstr((a|"T"|"F"))
!	needgap:=1
!end

global proc m$print_i64(int64 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt
	var int n

	nextfmtchars()

	if fmtstyle=nil then
		if a>=0 then
			n:=u64tostr(a,&.s,10,0)
		else
			s[1]:='-'
			n:=u64tostr(-a,&s[2],10,0)+1
		fi
		printstr_n(&.s,n)

	else
		strtofmt(fmtstyle,-1,&fmt)
		if fmt.param='V' then
			fmtparam:=a
!CPL "SET FMTPARAM TO",A
			needgap:=0
		else
			tostr_i64(a,&fmt)
		fi
	fi
	needgap:=1
end

global proc m$print_u64(word64 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt

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

global proc m$print_i128(int128 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	if a>=0 then
		tostr_u128(a,&fmt,0)
	else
		tostr_u128(-a,&fmt,1)
	fi

	needgap:=1
end

global proc m$print_u128(word128 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	tostr_u128(a,&fmt,0)
	needgap:=1
end

global proc m$print_r64(real x,ichar fmtstyle=nil)=
	var [360]char s
	var fmtrec fmt

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

global proc m$print_r32(real32 x,ichar fmtstyle=nil)=
	m$print_r64(x,fmtstyle)
end

global proc m$print_c8(int64 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt
	var int n

	nextfmtchars()

	s[1]:=a
	s[2]:=0
	printstr(&.s)
	needgap:=1
end

global proc m$print_str(ichar s, fmtstyle=nil)=
	nextfmtchars()
	var fmtrec fmt
	if fmtstyle=nil then
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,&fmt)
	fi
	needgap:=1
end

!global proc m$print_flexstr(object s, ichar fmtstyle=nil)=
!	nextfmtchars()
!	var fmtrec fmt
!
!	if fmtstyle=nil then
!		if s^.length then
!			printstr_n(s^.strptr,s^.length)
!		fi
!	else
!		strtofmt(fmtstyle,-1,&fmt)
!		tostr_str(str_stringz(s),&fmt)
!	fi
!	needgap:=1
!end

global proc m$print_newline=
	needgap:=0
	nextfmtchars(1)
	printstr("\w")
end

global proc m$print_nogap=
	needgap:=0
end

proc printstr(ichar s)=
	var int n
	var ref ref char p
	
	case outdev
	when std_io then
		printf("%s",s)
	when file_io then
		fprintf(outchan,"%s",s)
	when str_io then
		p:=cast(outchan)
		strcpy(p^,s)
		p^+:=strlen(s)
	esac
end

global proc printstr_n(ichar s,int n)=
	var ref ref char p

	case n
	when -1 then n:=strlen(s)
	when 0 then return
	esac

	case outdev
	when str_io then
		p:=cast(outchan)
		memcpy(p^,s,n)
		p^+:=n
		p^^:=0
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	when std_io then
		printf("%.*s",n,s)
	esac
end

global proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
!	emitc "printf(""%.*s"",(i32)length,s);"
	if f=nil then
		printf("%.*s",length,s)
	else
		fprintf(f,"%.*s",length,s)
	fi
fi
end

proc printchar(int ch)=
	var ref ref char p
	case outdev
	when std_io then
		printf("%c",ch)
	when file_io then
		fprintf(outchan,"%c",ch)
	when str_io then
		p:=cast(outchan)
		p^^:=ch
		p^+:=1
		p^^:=0
	esac
end

proc nextfmtchars(int lastx=0)=
	var char c
	var ref char pstart
	var int n

	if not fmtstr then			!format not in use
		if needgap then
			printchar(' ')
!		printstr_n(" ",1)
		fi
		needgap:=0
		return
	fi

	pstart:=fmtstr
	n:=0

	while (1) do
		c:=fmtstr^
		switch c
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
	skip::
			++n
			++fmtstr
		endswitch
	od
end

global proc strtofmt(ref char s,int slen,ref fmtrec fmt) =		!PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n	Width
!.n	Max width/precision
!A	Convert to upper when
!a	Convert to lower when
!B	Binary
!C	Show int as single n-bit (unicode) character
!D	Show int as multi-bit (unicode) character
!E,F,G	Specify format for double (corresponds to C format codes)
!F
!G
!H	Hex
!JC	Justify centre
!JL	Justify left
!JR	Justify right
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

	var char c
	var byte wset
	var int n
	var [0:100]char str

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
		switch c
		when 'B', 'b' then fmt^.base:=2
		when 'H', 'h' then fmt^.base:=16
		when 'O', 'o' then fmt^.base:=8
		when 'X', 'x' then
			c:=s^
			if c then
				switch c
				when '0'..'9' then c:=c-'0'
				when 'A'..'F' then c:=c-'A'+10
				when 'a'..'f' then c:=c-'a'+10
				else
					c:=10
				end
				fmt^.base:=c
				++s
			fi
		when 'Q', 'q' then fmt^.quotechar:='"'
		when '~' then fmt^.quotechar:='~'
		when 'J', 'j' then
			fmt^.justify:=toupper(s^)
			if s^ then
				++s
			fi
		when 'A' then fmt^.lettercase:='A'
		when 'a' then fmt^.lettercase:='a'
		when 'Z', 'z' then fmt^.padchar:='0'
		when 'S', 's' then
			fmt^.sepchar:=s^
			if s^ then
				++s
			fi
		when 'P', 'p' then
			fmt^.padchar:=s^
			if s^ then
				++s
			fi
		when 'T', 't' then
			fmt^.suffix:=s^
			if s^ then
				++s
			fi
		when 'W', 'w' then fmt^.usigned:='W'
		when 'E', 'e' then fmt^.realfmt:='e'
		when 'F', 'f' then fmt^.realfmt:='f'
		when 'G', 'g' then fmt^.realfmt:='g'
! when '0','1','2','3','4','5','6','7','8','9' then
		when '.' then
			wset:=1
		when comma,'_' then fmt^.sepchar:=c
		when '+' then fmt^.plus:='+'
		when 'D', 'd' then fmt^.charmode:='D'
		when 'C', 'c' then fmt^.charmode:='C'
		when 'M', 'm' then fmt^.heapmode:='M'
		when 'V','v' then fmt.param:='V'
		when '*' then
			n:=fmtparam
			goto gotwidth
!			if wset then
!CPL "FMT/* WSET",FMTPARAM
!				fmt.minwidth:=fmtparam
!			else
!CPL "FMT/*",FMTPARAM
!				fmt.precision:=fmtparam
!			fi
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
gotwidth::
				if not wset then
					fmt^.minwidth:=n
					wset:=1
				else
					fmt^.precision:=n
				fi
			fi
		endswitch
	od
end

function domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
	var [0:20]char str
	var ref char q
	var int i,nchars

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

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =		!EXPANDSTR
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt^.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string; caller must take care of this
!result will be zero-terminated, for use in this module

	var int i,w,m

!check to see if result is acceptable as it is
	w:=fmt^.minwidth
	if w=0 or w<=n then		! allow str to be longer than minwidth
		strncpy(t,s,n)
		(t+n)^:=0
		return n
	fi

	if fmt^.justify='L' then	! left-justify
		strncpy(t,s,n)
		t+:=n
		for i:=1 to w-n do
			t^:=fmt^.padchar
			++t
		od
		t^:=0
	elsif fmt^.justify='R' then
		if fmt^.padchar='0' and fmt^.base and (s^='-' or s^='+') then ! need to move sign outside 
			t^:=s^
			++t
			to w-n do
				t^:=fmt^.padchar
				++t
			od
			strncpy(t,s+1,n-1)
			(t+n-1)^:=0
		else
			to w-n do
				t^:=fmt^.padchar
				++t
			od
			strncpy(t,s,n)
			(t+n)^:=0
		fi

	else				! centre-justify?

		m:=(w-n+1)/2
		to m do
			t^:=fmt^.padchar
			++t
		od
		strncpy(t,s,n)
		t+:=n
		to w-n-m do
			t^:=fmt^.padchar
			++t
		od
		t^:=0

	fi
	return w
end

!function xdivrem(word64 a,b)word64,word64=
!	assem
!		xor rdx,rdx
!		mov rax,[a]
!		div u64 [b]
!		mov D1,rdx
!	end
!end

function xdivrem(word64 a,b, &remainder)word64=
	var word64 q,r
	assem
		xor rdx,rdx
		mov rax,[a]
		div u64 [b]
		mov [q],rax	
		mov [r],rdx	
	end
	remainder:=r
	return q
end

function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
	var [0:onesixty]char t
	var u64 dd
	var int i,j,k,g
	var int cc
	var int dummy
	var ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		aa:=xdivrem(aa,base,dd)
		t[++i]:=digits[dd]

!		t[++i]:=digits[aa rem base]
!		aa:=aa/base

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

function u128tostr(u128 aa,ref char s,word base,int sep)int =
!convert 128-bit int a to string in s^
!base is number base, usually 10 but can be 2 to 16
	var [0:160]char t
	var u64 dd
	var int i,j,k,g
	var int dummy
	var ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		aa:=xdivrem128(aa,base,dd)
		t[++i]:=digits[dd]

!		t[++i]:=digits[aa rem base]
!		aa:=aa/base

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

function xdivrem128(word128 a, word64 b, &remainder)word128=
	var word128 d,e,r
	var word rlow

	d:=a/b
	r:=a-d*b

	assem
		mov d0,[r]
		mov [rlow],d0
	end
	remainder:=rlow
	return d
end

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)
!returns length of s
	var [0:onesixty]char str				! allow for binary with separators!
	var int i,j,k,n,w,usigned
	static var u64 mindint=0x8000'0000'0000'0000

	usigned:=0
	if fmt^.usigned then
		usigned:=1
	fi

	if aa=mindint and not usigned then		! minint

		str[0]:='-'
		n:=i64mintostr(&str[1],fmt^.base,fmt^.sepchar)+1
	else
		if (not usigned and aa<-0) or fmt^.plus then
			if aa<0 then
				aa:=-aa
				str[0]:='-'
			else
				str[0]:='+'
			fi
			n:=u64tostr(aa,&str[1],fmt^.base,fmt^.sepchar)+1
		else
			n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)
		fi
	fi

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	var [0:onesixty]char str				! allow for binary with separators!
	var int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u128tostrfmt(i128 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	var [0:onesixty]char str				! allow for binary with separators!
	var int i,j,k,n,w

	n:=u128tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function i64mintostr(ref char s,int base,int sep)int =		!I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
	var [0:onesixty]char t
	var int i,j,k,g,neg

	switch base
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
	endswitch

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

function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
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
	var ref char u,v
	var [256]char str
	var int w,nheap		! whether any heap storage is used  bytes allocated

	nheap:=0

	if fmt^.quotechar or fmt^.lettercase then		! need local copy
		if n<256 then
			u:=&.str
		else
			nheap:=n+3					! allow for quotes+terminator
			u:=pcm_alloc(nheap)
		fi
		if fmt^.quotechar then
			v:=u
			v^:=fmt^.quotechar
			++v
			if n then
				strcpy(v,s)
				v+:=n
			fi
			v^:=fmt^.quotechar
			++v
			v^:=0
			n+:=2
		else
			memcpy(u,s,n)
		fi
		switch fmt^.lettercase
		when 'a' then	! need lower when
			convlcstring(u)
		when 'A' then
			convucstring(u)
		endswitch
		s:=u
	fi

	w:=fmt^.minwidth
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
	var [360]char str
	var int n

	case fmt^.charmode
	when 0 then
		n:=i64tostrfmt(a,&.str,fmt)
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	else						!assume 'C'
		printchar(a)			!no other formatting allowed
		return
	esac

	printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
	var [360]char str
	var int n

	case fmt^.charmode
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C','c' then
		printchar(a)			!no other formatting allowed
		return

	else
		n:=u64tostrfmt(a,&.str,fmt)
	esac

	printstr_n(&.str,n)
end

proc tostr_u128(word128 a, ref fmtrec fmt,int neg)=
	var [360]char str
	var int n

	case fmt^.charmode
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C','c' then
		printchar(a)			!no other formatting allowed
		return

	else
		if neg then
			str[1]:='-'
			n:=u128tostrfmt(a,&str[2],fmt)+1
		else
			n:=u128tostrfmt(a,&.str,fmt)
		fi
	esac

	printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
	var [360]char str,str2
	var [0:10]char cfmt
	var int n

	cfmt[0]:='%'

	if fmt^.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt^.realfmt
		cfmt[4]:=0
		sprintf(&.str,&.cfmt,fmt^.precision,x)
	else
		cfmt[1]:=fmt^.realfmt
		cfmt[2]:=0
		sprintf(&.str,&.cfmt,x)
	fi

!at this point, n is the str length including signs and suffix

!(TRY TAKING N FROM RESULT OF SPRINTF ABOVE)
	n:=strlen(&.str)		! current length

	if n<fmt^.minwidth then
		n:=expandstr(&.str,&.str2,n,fmt)
		strcpy(&.str,&.str2)
	fi

	printstr_n(&.str,n)
end

proc tostr_str(ref char s, ref fmtrec fmt) =
	var int oldlen,newlen,n
	var ref char t

!try and work out size of formatted string
	oldlen:=strlen(s)
	newlen:=oldlen

	if fmt^.quotechar or fmt^.minwidth>newlen or fmt^.lettercase or fmt.precision then
		if fmt^.quotechar then
			newlen+:=2
		fi
		if fmt^.minwidth>newlen then
			newlen:=fmt^.minwidth
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
	static var fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	fi
end

global function strint(int64 a, ichar fmtstyle=nil)ichar=
	static var[100]char str
	var ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

global proc getstrint(int64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

global function strword(word64 a, ichar fmtstyle=nil)ichar=
	static var[100]char str
	var ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

global function strreal(real a, ichar fmtstyle=nil)ichar=
	static var [320]char str
	var ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_r64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

function getstr(ichar s, ref fmtrec fmt)ichar=
	if fmt^.heapmode then
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
	initreadbuffer()
	readlinen(f,rd_buffer,rd_buffersize)

	rd_length:=strlen(rd_buffer)
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

global proc m$read_strline(ichar s)=
	var int n

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
	var ref char p,s,itemstr
	var char quotechar, c

	unless rd_buffer then 
		initreadbuffer()
!abortprogram("No readln")
	end unless


	s:=rd_pos

!scan string, eliminating leading white space
	while s^=' ' or s^=9 do
		++s
	od

	itemstr:=s				!assume starts here
	rd_lastpos:=rd_pos:=s

	if s^=0 then			! No more chars left to read return null string
		termchar:=0
		itemlength:=0
		return s
	fi

	quotechar:=0			! Allow possible enclosing single or double quotes
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
		switch c
		when ' ', 9, comma, '=' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			fi
			termchar:=c
			exit
		else
	normalchar::
			if c=quotechar then
				if s^=quotechar then	! embedded quote
					p^:=c
					++s
					++p
				else					! end of name
					termchar:=s^
					if termchar=',' or termchar='=' then
						++s
						termchar:=s^
					fi
					exit
				fi
			else
				p^:=c
				++p
			fi
		endswitch
	od

	if s^=0 then
		termchar:=0
	fi
	itemlength:=p-itemstr				! actual length of token
	rd_pos:=s

	return itemstr
end

global function strtoint(ichar s,int length=-1, base=10)int64=
!return point to next char after terminator (which can be just off length of string)
	var byte signd
	var word64 aa
	var char c,d

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
		switch c
		when 'A'..'F' then d:=c-'A'+10
		when 'a'..'f' then d:=c-'a'+10
		when '0'..'9' then d:=c-'0'
		when '_', '\'' then
			next
		else
			itemerror:=1
			exit
		endswitch

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
	var ref char s
	var int length,c
	var int64 aa

	case fmt
	when 'C','c' then
		rd_lastpos:=rd_pos
		if rd_pos^ then
			return rd_pos++^
		else
			return 0
		fi
	when 'T','t' then
		return termchar
	when 'E','e' then
		return itemerror
	esac

	s:=readitem(length)


	case fmt
	when 0,'I','i' then
		return strtoint(s,length)
	when 'B','b' then
		return strtoint(s,length,2)
	when 'H','h' then
		return strtoint(s,length,16)
	esac
	return 0
end

global function m$read_r64(int fmt=0)real=
	var [512]char str
	var ref char s
	var int length
	var int32 numlength
	var real x

	s:=readitem(length)

	if length=0 or length>=str.len then		!assume not a real
		return 0.0
	fi
	memcpy(&.str,s,length)
	str[length+1]:=0

	itemerror:=0

!CPL "CALLING SCCANF ON:",&.str,=X

	if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
!CPL =NUMLENGTH,=LENGTH
!		if numlength=length then x:=0.0 fi
		x:=0.0
		itemerror:=1
	fi

	return x
end

global proc m$read_str(ref char dest, int destlen=0,fmt=0)=
	var ref char s
	var int length,numlength
	var real x

	itemerror:=0
	if fmt='L' or fmt='l' then
		s:=rd_pos
		length:=rd_buffer+rd_length-rd_pos

	else
		s:=readitem(length)

		if fmt='N' or fmt='n' then
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

global proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_str(dest,destlen,fmt)
end

global proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

global proc reread=
	rd_pos:=rd_lastpos
end

global function valint(ichar s, int fmt=0)int64=
var ref char old_pos, old_lastpos
var int64 aa

initreadbuffer()
old_pos:=rd_pos
old_lastpos:=rd_lastpos

rd_pos:=s
aa:=m$read_i64(fmt)
rd_pos:=old_pos
rd_lastpos:=old_lastpos
return aa
end

global function valreal(ichar s)real=
var ref char old_pos, old_lastpos
var real x

initreadbuffer()
old_pos:=rd_pos
old_lastpos:=rd_lastpos

rd_pos:=s
x:=m$read_r64()
rd_pos:=old_pos
rd_lastpos:=old_lastpos
return x
end

proc iconvlcn(ref char s,int n) =		!ICONVLCN
to n do
	s^:=tolower(s^)
	++s
od
end

proc iconvucn(ref char s,int n) =		!ICONVUCN
to n do
	s^:=toupper(s^)
	++s
od
end

proc convlcstring(ref char s)=		!CONVLCSTRING
while (s^) do
	s^:=tolower(s^)
	++s
od
end

proc convucstring(ref char s)=		!CONVUCSTRING
while (s^) do
	s^:=toupper(s^)
	++s
od
end

!GLOBAL PROC M$PRINT_U32(word32 a, ref void fmt)=
!	m$print_u64(a,nil)
!end
!
!GLOBAL PROC M$PRINT_I32(int32 a, ref void fmt)=
!	m$print_i64(a,nil)
!end
!
!GLOBAL PROC M$STARTPRINT(ref void dev)=
!	m$print_startfile(dev)
!end
!
!GLOBAL PROC M$STARTPRINTCON=
!	m$print_startcon()
!end
!
!GLOBAL PROC M$ENDPRINT=
!	m$print_end()
!end

global threadedproc m$ufloat_r64u64=
	assem
		cmp D10,0
		jl fl1
!number is positive, so can treat like i64
		cvtsi2sd XMM15,D10
		ret
fl1:						!negative value
		and D10,[mask63]		!clear top bit (subtract 2**63)
		cvtsi2sd XMM15,D10
		addsd XMM15,[offset64]	!(add 2**63 back to result)
		ret
	end
end

global threadedproc m$ufloat_r64u32=
	assem
		mov D10,D10				! clear top half (already done if value just moved there)
		cvtsi2sd XMM15,D10
		ret
	end
end

global threadedproc m$ufloat_r32u32=
	assem
		mov D10,D10
		cvtsi2ss XMM15,D10
		ret
	end
end

global threadedproc m$ufloat_r32u64=
	assem
		cmp D10,0
		jl fl2
!number is positive, so can treat like i64
		cvtsi2ss XMM15,D10
		ret
fl2:						!negative value
		and D10,[mask63]		!clear top bit (subtract 2**63)
		cvtsi2ss XMM15,D10
		addss XMM15,[offset32]	!(add 2**63 back to result)
		ret
	end
end

!global function m$power_i64(int64 n,a)int64=
!if n<0 then
!	return 0
!elsif n=0 then
!	return 1
!elsif n=1 then
!	return a
!elsif (n iand 1)=0 then
!!	return ipower(a*a,n/2)
!	return m$power_i64(a*a,n/2)
!else			!assume odd
!	return m$power_i64(a*a,(n-1)/2)*a
!fi
!end

global function m$power_i64(int64 n,a)int64=
if n<0 then
	return 0
elsif n=0 then
	return 1
elsif n=1 then
	return a
elsif (n iand 1)=0 then
!	return ipower(a*a,n/2)
	return m$power_i64(n/2,sqr a)
else			!assume odd
	return m$power_i64((n-1)/2,sqr a)*a
fi
end

global proc m$intoverflow=
	abortprogram("Integer overflow detected")
end

global proc m$mul_i128(word128 bb,aa)=
!CPL "$MUL128"
	assem
		mov d2,[aa]			!a1
		mov d3,[aa+8]		!a2
		mov d4,[bb]			!b1
		mov d5,[bb+8]		!b2


		mov d0,d2			!a1
		imul2 d0,d5			!*b2	
		mov d6,d0			!=>d6

		mov d0,d3			!a2
		imul2 d0,d4			!*b1
		mov d7,d0			!=>d7

		mov d0,d2			!a1
		mul d4				!*b1
		add d11,d6			! + a1*b2<<64
		add d11,d7			! + a2*b1<<64
		mov d1,d11
	end
end

global proc m$idiv_i128(word128 bb,aa)=
!does 128/64 bits only
	assem
		mov d2,[aa]
		mov d3,[aa+8]

		mov d4,[bb]
		or d4,d4
		jz divbyzero

		mov d0,d3		!a2
		xor d11,d11
		div d4			!a2/b
		mov d6,d0		! => c2
		mul d4			!c2*b
		sub d3,d0		!a2-:=c2*b

		mov d0,d2
		mov d11,d3		!a2:a1
		div d4			!/b
		mov d1,d6
	end
	return

asm divbyzero:
CPL "DIV BY ZERO"
	stop 1
end

global proc m$dotindex(word i,a)=
!return a.[i] in d0
	assem
		mov d0,[a]
		mov cl,[i]
		shr d0,cl
		and d0,1
	end	
end

global proc m$dotslice(word j,i,a)=
!return a.[i..j] in d0; assumes j>=i
	assem
		mov d0,[a]
		mov rcx,[i]
		shr d0,cl
		sub rcx,[j]
		neg rcx				!j-1
		mov d2,0xFFFF'FFFF'FFFF'FFFE
		shl d2,cl
		not d2
		and d0,d2
	end	
end

global proc m$popdotindex(word i,ref word p,word x)=
!p^.[i]:=x
	assem
		mov d3,[p]
		mov cl,[i]
		mov d0,[d3]
		mov d1,1
		shl d1,cl			!000001000
		not d1				!111110111
		and d0,d1			!clear that bit in dest
		mov d1,[x]
		and d1,1
		shl d1,cl
		or d0,d1
		mov [d3],d0
	end	
end

global proc m$popdotslice(word j,i, ref word p, word x)=
!p^.[i..j]:=x
	assem
!d3 = p
!d4 = x, then shifted then masked x
!d5 = i
!d6 = clear mask

		mov d3,[p]
		mov d4,[x]
		mov d5,[i]
		mov rcx,d5			!i
		shl d4,cl			!x<<i
		mov rcx,[j]
		sub rcx,d5			!j-i
		inc rcx				!j-i+1
		mov d2,0xFFFF'FFFF'FFFF'FFFF
		shl d2,cl			!...111100000     (assume 5-bit slice)
		not d2				!...000011111
		mov rcx,d5			!i
		shl d2,cl			!...000011111000  (assume i=3)
		and d4,d2			!mask x (truncate extra bits)
		mov d0,[d3]
		not d2				!...111100000111
		and d0,d2			!clear dest bits
		or d0,d4			!add in new bits
		mov [d3],d0
	end	
end


global function m$sin(real x)real = {`sin(x)}
global function m$cos(real x)real = {`cos(x)}
global function m$tan(real x)real = {`tan(x)}
global function m$asin(real x)real = {`asin(x)}
global function m$acos(real x)real = {`acos(x)}
global function m$atan(real x)real = {`atan(x)}
global function m$ln(real x)real = {`log(x)}
!global function m$lg(real x)real = {`lg(x)}
global function m$log(real x)real = {`log10(x)}
global function m$exp(real x)real = {`exp(x)}
global function m$floor(real x)real = {`floor(x)}
global function m$ceil(real x)real = {`ceil(x)}
global function m$fract(real x)real = {abortprogram("FRACT");0}
global function m$round(real x)real = {abortprogram("ROUND");0}
=== mlib.m 8/35 ===
import msys
import clib
import oslib

!const mem_check=1
const mem_check=0

global var [0..300]u64 allocupper
global var int alloccode				!set by heapalloc
global var int allocbytes				!set by heapalloc
global var int fdebug=0
global var int rfsize

const threshold=1<<25
const alloc_step=1<<25
var word maxmemory
var int  maxalloccode

var byte pcm_setup=0

var int show=0

global var int memtotal=0
global var int64 smallmemtotal=0
global var int smallmemobjs=0
global var int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=500000
var [maxmemalloc+1]ref int32 memalloctable
var [maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
var ref byte pcheapstart
var ref byte pcheapend			!points to first address past heap
var ref byte pcheapptr

const int maxblockindex = 8 		!2048
global const int maxblocksize = 2048

var [0:maxblocksize+1]byte sizeindextable	!convert byte size to block index 1..maxblockindex

const int size16   = 1			!the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

GLOBAL var [0:9]ref wordp freelist

global record strbuffer =
	var ichar strptr
	var int32 length
	var int32 allocated
end

global tabledata() [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

var [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

global function pcm_alloc(int n)ref void =		!PCM_ALLOC
var ref byte p
!int i

if not pcm_setup then
	pcm_init()
!	abortprogram("need pcm_init")
fi

if n>maxblocksize then			!large block allocation
	alloccode:=pcm_getac(n)
	allocbytes:=allocupper[alloccode]

	p:=allocmem(allocbytes)
	if not p then
		abortprogram("pcm_alloc failure")
	fi

if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

	return p
fi

alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

if alloccode=0 then					!sizes below 16 bytes (can I adjust sizeindextable to?)
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

SMALLMEMTOTAL+:=ALLOCBYTES

if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
	freelist[alloccode]:=ref wordp(int((freelist[alloccode])^))

	return p
fi

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	return p
fi
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

return p
end

global proc pcm_free(ref void p,int n) =		!PCM_FREE
!n can be the actual size requested it does not need to be the allocated size
var int acode

if n=0 then return fi

if n>maxblocksize then		!large block
	if mem_check then removefrommemalloc(p,n) fi

	free(p)
	return
fi

if p then
	acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

	smallmemtotal-:=allocupper[acode]

	if mem_check then removefrommemalloc(p,allocupper[acode]) fi

	(ref wordp(p))^:=wordp(int(freelist[acode]))
	freelist[acode]:=p
fi
end

global proc pcm_freeac(ref void p,int alloc) =		!PCM_FREEAC
pcm_free(p,allocupper[alloc])
end

global proc pcm_copymem4(ref void p,q,int n) =	!PCM_COPYMEM4
!copy n bytes of memory from q to p.
!the memory spaces used are multiples of 16 bytes, but n itself could be anything
!n can be zero, and need not be a multiple of 4 bytes

memcpy(p,q,n)
end

global proc pcm_clearmem(ref void p,int n) =		!PCM_CLEARMEM
memset(p,0,n)
end

global proc pcm_init =		!PCM_INIT
!set up sizeindextable too
!sizeindextable[0] = 0
var int j,k,k1,k2
var int64 size
const limit=1<<33

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
!	if size>4 billion then
!		size+:=alloc_step
!	fi
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

global function pcm_getac(int size)int =		!PCM_GETAC
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

global function pcm_newblock(int itemsize)ref void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
static var int totalheapsize
var ref byte p

totalheapsize+:=pcheapsize
alloccode:=0
p:=allocmem(pcheapsize)	!can't free this block until appl terminates
if p=nil then
	abortprogram("Can't alloc pc heap")
fi

pcheapptr:=p
pcheapend:=p+pcheapsize

if pcheapstart=nil then		!this is first block
	pcheapstart:=p
fi
pcheapptr+:=itemsize
return ref u32(p)
end

global function pcm_round(int n)int =		!PCM_ROUND
!for any size n, return actual number of bytes that would be allocated
static var [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

if n>maxblocksize then
	return n
else
	return allocbytes[sizeindextable[n]]
fi
end

global function pcm_array(int n)int =		!PCM_ARRAY
!n bytes are needed for an array return the number of bytes to be actually allocated
var int m

if n<=maxblocksize then	!automatic rounding up used for small heap
	return pcm_round(n)
! allocbytes[sizeindextable[n]]
else				!devise some strategy probably doubling up.
	m:=2048
	while n>m do
		m<<:=1
	od
	return m
fi

end

global proc pcm_printfreelist(int size,ref wordp p) =		!PCM_PRINTFREELIST
println "Size: ",size
while p do
!	printf(" %llX",u64(p))
	print " ",,p:"h"
	p:=ref wordp(int(p^))
od
puts("")
end

global proc pcm_diags(ref char caption) =		!PCM_DIAGS
var int m

println "HEAP FREELISTS:",caption

m:=16
for i:=1 to 8 do
	pcm_printfreelist(m,freelist[i])
	m<<:=1
od
end

global function pcm_allocz(int n)ref void =		!PCM_ALLOCZ
var ref void p
p:=pcm_alloc(n)

memset(p,0,n)
return p
end

global function pcm_copyheapstring(ref char s)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
var ref char q
var int n
if s=nil then return nil fi

n:=strlen(s)+1
q:=pcm_alloc(n)
memcpy(q,s,n)
return q
end

global function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	var ref char q
	if length=0 then return nil fi

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

proc addtomemalloc(ref int32 ptr,int size)=
!add ptr to allocated table

!CPL "***************ADD TO ALLOC:",ptr,size

for i to maxmemalloc do
	if memalloctable[i]=ptr then
		CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
CPL
CPL
		stop 2
	fi

	if memalloctable[i]=nil then		!unused entry
		memalloctable[i]:=ptr
		memallocsize[i]:=size
		return
	fi
od
CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
stop 3
end

proc removefrommemalloc(ref int32 ptr,int size)=
!remove ptr to allocated table

!CPL "------------------************REMOVE FROM ALLOC:",ptr,size

for i to maxmemalloc do
	if memalloctable[i]=ptr then

if memallocsize[i]<>size then
	CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
!PCERROR("MEMERROR")
CPL
CPL
	abortprogram("MEMSIZE")
fi

		memalloctable[i]:=nil
		return
	fi
od
CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
CPL
CPL
abortprogram("MEM")
stop 4
end

global function allocmem(int n)ref void =		!ALLOCMEM
var ref void p

p:=malloc(n)
if (p) then
	return p
fi
println n,memtotal
abortprogram("Alloc mem failure")
return nil
end

global function reallocmem(ref void p,int n)ref void =		!REALLOCMEM
p:=realloc(p,n)
return p when p
println n
abortprogram("Realloc mem failure")
return nil
end

global proc abortprogram(ref char s) =		!ABORTPROGRAM
println s
print   "ABORTING: Press key..."
os_getch()
stop 5
end

global function getfilesize(filehandle handlex)int=		!GETFILESIZE
	var word32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

global proc readrandom(filehandle handlex, ref byte mem, int offset, size) =		!READRANDOM
	var int a
	fseek(handlex,offset,seek_set)
	a:=fread(mem,1,size,handlex)			!assign so as to remove gcc warning
end

global function writerandom(filehandle handlex, ref byte mem, int offset,size)int =		!WRITERANDOM
	fseek(handlex,offset,seek_set)
	return fwrite(mem,1,size,handlex)
end

global function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

global function getfilepos(filehandle file)int=
	return ftell(file)
end

global function readfile(ref char filename)ref byte =		!READFILE
var filehandle f
var int size
var ref byte m,p

f:=fopen(filename,"rb")
if f=nil then
	return nil
fi
rfsize:=size:=getfilesize(f)

m:=malloc(size+4)		!allow space for etx/zeof etc

if m=nil then
	return nil
fi

readrandom(f,m,0,size)
p:=m+size			!point to following byte
p^:=0
(p+1)^:=26
(p+2)^:=0			!allow use as string

fclose(f)
return m
end

global function writefile(ref char filename,ref byte data,int size)int =
var filehandle f
var int n

f:=fopen(filename,"wb")
if f=nil then
	return 0
fi

n:=writerandom(f,data,0,size)
fclose(f)
return n
end

global function checkfile(ref char file)int=		!CHECKFILE
var filehandle f
if f:=fopen(file,"rb") then
	fclose(f)
	return 1
fi
return 0
end

global proc readlinen(filehandle handlex,ref char buffer,int size) =		!READLINEN
!size>2
var int ch
var ref char p
var int n
var [0:100]char buff
var byte crseen

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

global proc iconvlcn(ref char s,int n) =		!ICONVLCN
to n do
	s^:=tolower(s^)
	++s
od
end

global proc iconvucn(ref char s,int n) =		!ICONVUCN
to n do
	s^:=toupper(s^)
	++s
od
end

global proc convlcstring(ref char s)=		!CONVLCSTRING
while (s^) do
	s^:=tolower(s^)
	++s
od
end

global proc convucstring(ref char s)=		!CONVUCSTRING
while (s^) do
	s^:=toupper(s^)
	++s
od
end

global function changeext(ref char s,newext)ichar=		!CHANGEEXT
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
static var [260]char newfile
var [32]char newext2
var ref char sext
var int n

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

global function extractext(ref char s,int period=0)ichar=		!EXTRACTEXT
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
var ref char t,u

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

global function extractpath(ref char s)ichar=		!EXTRACTPATH
static var [0:260]char str
var ref char t
var int n

t:=s+strlen(s)-1		!t points to last char

while (t>=s) do
	switch t^
	when '\\','/',':' then		!path separator or drive letter terminator assume no extension
		n:=t-s+1			!n is number of chars in path, which includes rightmost / or \ or :
		memcpy(&.str,s,n)
		str[n]:=0
		return &.str
	endswitch
	--t
od
return ""			!no path found
end

global function extractfile(ref char s)ichar=		!EXTRACTFILE
var ref char t

t:=extractpath(s)

if t^=0 then			!s contains no path
	return s
fi

return s+strlen(t)		!point to last part of s that contains the file
end

global function extractbasefile(ref char s)ichar=		!EXTRACTBASEFILE
static var [0:100]char str
var ref char f,e
var int n,flen

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

global function addext(ref char s,ref char newext)ichar=		!ADDEXT
!when filespec has no extension of its own, add newext
var ref char sext

sext:=extractext(s,1)

if sext^=0 then						!no extension not even "."
	return changeext(s,newext)
fi

return s							!has own extension; use that
end

global function alloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
var ref void p

p:=malloc((n+1)*size)

if not p then
	abortprogram("Alloctable failure")
fi
return p
end

global function zalloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
var ref int p

p:=alloctable(n,size)

pcm_clearmem(p,(n+1)*size)
return p
end

global proc checkfreelists(ichar s)=
var ref wordp p,q
var int64 aa

for i:=2 to 2 do
	p:=freelist[i]

	while p do
		aa:=int64(p)
		if aa>0xffff'FFFF or aa<100 then
			CPL s,"FREE LIST ERROR",i,p,q
!			os_getch(); stop 1
		fi
		q:=p
		p:=ref wordp(int(p^))
	od

od
end

global function pcm_alloc32:ref void =		!PCM_ALLOC
var ref byte p

allocbytes:=32
!smallmemtotal+:=32

!if p:=ref byte(freelist[2]) then		!Items of this block size available
!	freelist[2]:=ref wordp((freelist[2])^)
!	if mem_check then addtomemalloc(ref int32(p),32) fi
!	return p
!fi

!No items in freelists: allocate new space in this heap block

return pcm_alloc(32)
end

global proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

!CPL "PCMFREE32"
smallmemtotal-:=32
if mem_check then removefrommemalloc(p,32) fi
!(ref wordp(p))^:=wordp(freelist[2])
(ref wordp(p))^:=wordp(int(freelist[2]))
freelist[2]:=p
end

global proc outbyte(filehandle f,int x)=
fwrite(&x,1,1,f)
end

global proc outword16(filehandle f,word x)=
fwrite(&x,2,1,f)
end

global proc outword(filehandle f,word x)=
fwrite(&x,4,1,f)
end

global proc outword64(filehandle f,word64 x)=
fwrite(&x,8,1,f)
end

global function myeof(filehandle f)int=
var int c

c:=fgetc(f)
if c=c_eof then return 1 fi
ungetc(c,f)
return 0;
end

global function pcm_smallallocz(int n)ref void =
var ref byte p

if (alloccode:=sizeindextable[n])=0 then
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	memset(p,0,n)
	return p
fi

memset(p,0,n)

return p
end

!global function pcm_fastalloc(int n)ref void =
global function pcm_smallalloc(int n)ref void =
var ref byte p

if (alloccode:=sizeindextable[n])=0 then
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	return p
fi

return p
end

global proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
var int newlen,oldlen
var ichar newptr

IF N=0 THEN CPL "N=0" FI

if n=-1 then
	n:=strlen(s)
fi

oldlen:=dest^.length

if oldlen=0 then				!first string
	dest^.strptr:=pcm_alloc(n+1)
	dest^.allocated:=allocbytes
	dest^.length:=n				!length always excludes terminator
	memcpy(dest^.strptr,s,n)
	(dest^.strptr+n)^:=0
	return
fi

newlen:=oldlen+n
if newlen+1>dest^.allocated then
!CPL "REALLOC",NEWLEN
	newptr:=pcm_alloc(newlen+1)
	memcpy(newptr,dest^.strptr,oldlen)
	dest^.strptr:=newptr
	dest^.allocated:=allocbytes
fi

memcpy(dest^.strptr+oldlen,s,n)
(dest^.strptr+newlen)^:=0

dest^.length:=newlen
end

global proc gs_init(ref strbuffer dest)=			!INITGENSTR
pcm_clearmem(dest,strbuffer.bytes)
end

global proc gs_free(ref strbuffer dest)=
if dest^.allocated then
	pcm_free(dest^.strptr,dest^.allocated)
fi
end

global proc gs_str(ref strbuffer dest,ichar s)=			!GENSTR
strbuffer_add(dest,s)
end

global proc gs_char(ref strbuffer dest,int c)=
var [16]char s

s[1]:=c
s[2]:=0

strbuffer_add(dest,&.s,1)
end

global proc gs_strn(ref strbuffer dest,ichar s,int length)=
strbuffer_add(dest,s,length)
end

global proc gs_strvar(ref strbuffer dest,s)=			!GENSTR
strbuffer_add(dest,s^.strptr)
end

global proc gs_strint(ref strbuffer dest,int64 a)=
strbuffer_add(dest,strint(a))
end

global proc gs_strln(ref strbuffer dest,ichar s)=		!GENSTRLN
gs_str(dest,s)
gs_line(dest)
end

global proc gs_strsp(ref strbuffer dest,ichar s)=
gs_str(dest,s)
gs_str(dest," ")
end

global proc gs_line(ref strbuffer dest)=
strbuffer_add(dest,"\w")
end

global function gs_getcol(ref strbuffer dest)int=
return dest^.length
end

global proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
var int col,i,n,slen
var [2560]char str
col:=dest^.length
strcpy(&.str,s)
slen:=strlen(s)
n:=w-slen
!CPL =slen,=w,=n
if n>0 then
	for i:=1 to n do
		str[slen+i]:=padch
	od
	str[slen+n+1]:=0
fi
gs_str(dest,&.str)
end

global proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
gs_leftstr(dest,strint(a),w,padch)
end

global proc gs_padto(ref strbuffer dest,int col, ch=' ')=
var int n
var [2560]char str

n:=col-dest^.length
if n<=0 then return fi
for i:=1 to n do
	str[i]:=ch
od
str[n+1]:=0
gs_str(dest,&.str)
end

global proc gs_println(ref strbuffer dest,filehandle f)=
(dest.strptr+dest.length)^:=0

if f=nil then
	println dest.strptr,,"\c"
else
	println @f,dest.strptr,,"\c"
fi
end

global function nextcmdparam(int &paramno, ichar &name, &value, ichar defext=nil)int=
static var int infile=0
static var ichar filestart=nil
static var ichar fileptr=nil
static var byte colonseen=0
var ref char q
var ichar item,fileext
var ichar rest
var int length
static var [300]char str

reenter::
value:=nil
name:=nil

if infile then
	if readnextfileitem(fileptr,item)=0 then		!eof
		free(filestart)								!file allocated via malloc
		infile:=0
		goto reenter
	fi
else
	if paramno>nsysparams then
		return pm_end
	fi
	item:=sysparams[paramno]
	++paramno

	length:=strlen(item)

	if item^='@' then		!@ file
		filestart:=fileptr:=cast(readfile(item+1))
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
elsif eqstring(fileext,"dll") then
	return (colonseen|pm_extra|pm_libfile)
fi
return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
var ref char p,pstart,pend
var int n
static var [256]char str

p:=fileptr

reenter::
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

	enddocase
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

global proc ipadstr(ref char s,int width,ref char padchar=" ")=
var int n

n:=strlen(s)
to width-n do
	strcat(s,padchar)
od
end

global function padstr(ref char s,int width,ref char padchar=" ")ichar=
static var [256]char str

strcpy(&.str,s)
ipadstr(&.str,width,padchar)
return &.str
end

global function chr(int c)ichar=
static var[8]char str

str[1]:=c
str[2]:=0
return &.str
end

global function cmpstring(ichar s,t)int=
	var int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function cmpstringn(ichar s,t,int n)int=
	var int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

global function cmpbytes(ref void p,q,int n)int=
	var int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqbytes(ref void p,q,int n)int=
	return memcmp(p,q,n)=0
end

global proc mseed(word64 a,b=0)=
seed[1]:=a
if b then
	seed[2]:=b
else
	seed[2] ixor:=a
fi
end

global function mrandom:int =
!return pure 64-bit int value, positive or negative -2**63 to +2**63-1
!(cast result for unsigned value)
	var word64 x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

global function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

global function mrandomint(int n)int=
!positive random int value from 0 to n-1
	return mrandomp() rem n
end

global function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
	var int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

global function mrandomreal:real =
!positive random real value from 0 to 0.999999999999999999891579782751449556599254719913005828857421875
!upper limit is (2**63-1)/(2**63)
	return real(mrandomp())/9223372036854775808.0
end

global function checkpackfile:ref byte=
!find out if this executable contains extra packed files
!return 1 or 0

var int a,offset,i,size
var [100]char name
var [300]char exefile
var ref byte packexeptr			!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
var int packexesize				!byte size
var ref char packfilename
var int packfilesize
var ref byte packfileptr

macro getfileint(data,offset)=(ref int32(data+offset))^

strcpy(&exefile[1],os_gethostname())
println "Attempting to open",&exefile
packexeptr:=readfile(&exefile[1])

if not packexeptr then
	cpl "Can't open",&exefile,&packexeptr
	stop
fi

packexesize:=rfsize
cpl "File read OK. Size",packexesize
!STOP

a:=getfileint(packexeptr,packexesize-int32.bytes)
if a<>'PCAK' then
	free(packexeptr)
	packfileptr:=nil
	return nil
fi

offset:=getfileint(packexeptr,packexesize-int32.bytes*2)

packfilename:=cast(packexeptr+offset)
offset+:=strlen(packfilename)+1
packfilesize:=getfileint(packexeptr,offset)
packfileptr:=packexeptr+offset+int32.bytes

return packfileptr
end
=== oswindows.m 9/35 ===
import clib
import mlib

const wm_destroy=2

type wt_word	= word16
type wt_wordpm	= word32
type wt_bool	= word32
type wt_dword	= word32
type wt_wchar	= word16
type wt_wcharpm	= word32
type wt_char	= byte
type wt_ichar	= ref char
type wt_ptr		= ref void
type wt_wndproc	= ref proc
type wt_handle	= ref void
type wt_int		= int32
type wt_uint	= word32
type wt_long	= int32
type wt_wparam	= wordm
type wt_lparam	= wordm
type wt_point	= rpoint

global record rsystemtime =
	wt_word year
	wt_word month
	wt_word dayofweek
	wt_word day
	wt_word hour
	wt_word minute
	wt_word second
	wt_word milliseconds
end

importdll windowsdlls=
!	windows function "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	windows function "GetStdHandle"(wt_dword)wt_handle
	windows function "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	windows function "SetConsoleCtrlHandler"(wt_wndproc,int)int
	windows function "SetConsoleMode"(wt_handle,wt_dword)int
	windows function "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	windows function "GetLastError":wt_dword
	windows function "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	windows function "GetExitCodeProcess"(wt_handle,wt_ptr)int
	windows function "CloseHandle"(wt_handle)int
	windows function "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	windows function "FlushConsoleInputBuffer"(wt_handle)int
	windows function "LoadLibraryA"(wt_ichar)wt_handle
!	windows function "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	windows function "GetProcAddress"(wt_handle,wt_ichar)ref void
	windows function "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	windows function "RegisterClassExA"(wt_ptr)wt_wordpm
	windows function "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)intm
	windows function "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	windows proc     "Sleep"(wt_dword)
	windows function "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	windows proc     "ExitProcess"(wt_uint)
	windows proc	 "PostQuitMessage"(wt_int)

!	windows proc	 "MessageBoxA"(wt_int,wt_ichar,wt_ichar,wt_int)

	windows proc	 "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	windows function "QueryPerformanceCounter"(ref int64)wt_bool
	windows function "QueryPerformanceFrequency"(ref int64)wt_bool

	windows function "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	windows function "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	windows proc     "GetSystemTime"(ref rsystemtime)
	windows proc     "GetLocalTime"(ref rsystemtime)

	windows function "GetTickCount":wt_dword
	windows function "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

end

record input_record =
	var wt_word	eventtype
	var word16	padding
		var wt_bool	keydown			!key event record (was inside 'Event' union in win32)
		var wt_word	repeatcount
		var wt_word	virtualkeycode
		var wt_word	virtualscancode
		union
			var wt_word unicodechar
			var wt_char asciichar
		end
		var wt_dword controlkeystate
end

record rspoint=(var int16 x,y)

record rsrect=
	var int16 leftx,top,rightx,bottom
end

global record rpoint =
	var wt_long x,y
end

record rconsole=
	var rspoint size,pos
	var word16 attributes
	var rsrect window
	var rspoint maxwindowsize
end

record rstartupinfo =
	var wt_dword	size
!.if $64bit
	var word32 dummy1
!.endif
	var wt_ichar	reserved
	var wt_ichar	desktop
	var wt_ichar	title
	var wt_dword	x
	var wt_dword	y
	var wt_dword	xsize
	var wt_dword	ysize
	var wt_dword	xcountchars
	var wt_dword	ycountchars
	var wt_dword	fillattribute
	var wt_dword	flags
	var wt_word		showwindow
	var wt_word		reserved2
!.if $64bit
	var word32 dummy2
!.endif
	var	wt_ptr		reserved4
	var wt_handle	stdinput
	var wt_handle	stdoutput
	var wt_handle	stderror
end

record rprocess_information =
	var wt_handle process
	var wt_handle thread
	var wt_dword processid
	var wt_dword threadid
end

record rwndclassex =
	var wt_uint		size
	var wt_uint		style
	var wt_wndproc	wndproc
	var wt_int		clsextra
	var wt_int		wndextra
	var wt_handle	instance
	var wt_handle	icon
	var wt_handle	cursor
!	var wt_handle	background
	var wt_handle	background
	var wt_ichar	menuname
	var wt_ichar	classname
	var wt_handle	iconsm
end

global record rmsg =
	var wt_handle	hwnd
	var wt_uint		message
!.if $64bit
	var word32		dummy1
!.endif
	var wt_wparam	wParam
	var wt_lparam	lParam
	var wt_dword	time
!.if $64bit
	var word32		dummy2
!.endif
	var wt_point	pt
end

!wt_word x
const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

var wt_handle hconsole, hconsolein

var input_record lastkey, pendkey
var int keypending			!whether pendkey contains a new key event detected by flushkbd

var ref function(ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

var int init_flag=0

global proc os_init=
var int i,count
var rconsole info

!general initialisation
hconsole:=GetStdHandle(u32(-11))
hconsolein:=GetStdHandle(u32(-10))

lastkey.repeatcount:=0
keypending:=0

!CPL "OSINIT"
SetConsoleCtrlHandler(nil,1)

SetConsoleMode(hconsole,1 ior 2)
!SetConsoleMode(hconsole,1 )

init_flag:=1

end

global function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
var wt_dword exitcode
var int status
var int cflags:=0

var rstartupinfo si
var rprocess_information xpi

memset(&si,0,si.bytes)
memset(&xpi,0,xpi.bytes)

switch newconsole
when 0 then cflags := NORMAL_PRIORITY_CLASS
when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
endswitch

si.size := rstartupinfo.bytes

status:=CreateProcessA( nil,
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
	println "Winexec error:",status
	return -1
end

WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
GetExitCodeProcess(xpi.process,&exitcode)

CloseHandle(xpi.process)
CloseHandle(xpi.thread)

return exitcode
end

global function os_execcmd(ichar cmdline, int newconsole=0)int =
var wt_dword exitcode
var int i,j,k

var rstartupinfo si
var rprocess_information xpi

memset(&si,0,si.bytes)
memset(&xpi,0,xpi.bytes)

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

global function os_getch:int=
var int k

k:=os_getchx() iand 255

return k
end

global function os_kbhit:int=
var wt_dword count
!os_init() unless init_flag

unless init_flag then os_init() end
!unless initflag then: os_init()

GetNumberOfConsoleInputEvents(hconsolein,&count)
return count>1
end

global proc os_flushkeys=
FlushConsoleInputBuffer(hconsolein)
end

global function os_getconsolein:ref void=
return ref void(hconsolein)
end

global function os_getconsoleout:ref void=
return ref void(hconsole)
end

global function os_proginstance:ref void=
abortprogram("PROGINST")
return nil
end

global function os_getdllinst(ichar name)u64=
var wt_handle hinst

hinst:=LoadLibraryA(name)
return cast(hinst)
end

global function os_getdllprocaddr(intm hinst,ichar name)ref void=
return GetProcAddress(cast(int(hinst)),name)
end

global proc os_initwindows=
os_init()
os_gxregisterclass("pcc001")
end

global proc os_gxregisterclass(ichar classname)=
const idcarrow=32512
var rwndclassex r
static var byte registered

if registered then
	return
fi

memset(&r,0,r.bytes)
r.size:=r.bytes
r.style:=8 ior 32		!CS_DBLCLKS | CS_OWNDC
r.wndproc:=cast(&mainwndproc)
!r.wndproc:=&xmainwndproc
!r.wndproc:=&cmainwndproc
r.instance:=nil

r.icon:=nil		!loadicon(proginstance,"SCW32")
r.cursor:=LoadCursorA(nil,ref void(idcarrow))		!IDC_ARROW)
r.background:=cast(15+1)					!COLOR_BTNFACE+1
r.menuname:=nil
r.classname:=classname
r.iconsm:=nil	!loadicon(proginstance,"SCW32")

if RegisterClassExA(&r)=0 then
	println classname,GetLastError
	abortprogram("Registerclass error")
end
registered:=1
end

global callback function mainwndproc (\
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)intm=
var rmsg m
var int i,result
var intm l
static var int count=0

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

!callback proc timerproc(wt_handle hwnd, int msg, id, time)=
proc timerproc(wt_handle hwnd, int msg, id, time)=
println "TIMERPROC"
end

global proc os_setmesshandler(ref void addr)=
wndproc_callbackfn:=addr
end

global function os_getchx:int=
!Q! function os_getchx_c:int
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
var int count
var int charcode,keyshift,keycode
var int altdown,ctrldown,shiftdown,capslock

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

!CPL "CHARCODE2=%d %X\n",charcode,charcode
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

global function os_getos=>ichar=
if $targetbits=32 then
	return "W32"
else
	return "W64"
fi
end

global function os_gethostsize=>int=
return $targetbits
end

global function os_shellexec(ichar opc, file)int=
return system(file)
end

global proc  os_sleep(int a)=
Sleep(a)
end

global function os_getstdin:filehandle =
return fopen("con","rb")
end

global function os_getstdout:filehandle =
return fopen("con","wb")
end

global function os_gethostname:ichar=
static var [300]char name
static var int n

GetModuleFileNameA(nil,&.name,name.bytes)
strcat(&.name,"/")
return &.name
end

global function os_getmpath:ichar=
return F"C:\m\"
end

global proc os_exitprocess(int x)=
stop x
!ExitProcess(x)
end

global function os_clock:int64=
return clock()
end

global function os_getclockspersec:int64=
return 1000
end

global function os_iswindows:int=
return 1
end

global function os_filelastwritetime(ichar filename)int64=
var wt_handle f;
var int64 ctime,atime,wtime;

if filename=nil then				!used to test whether supported
	return 1
fi

f:=CreateFileA(filename,0x80000000,1,nil, 3,3,nil);
if int64(f)=-1 then
	return 0
fi

GetFileTime(f,&ctime,&atime,&wtime);
CloseHandle(f);

return wtime;
end

global proc os_getsystime(ref rsystemtime tm)=
GetLocalTime(tm)
end

global proc os_messagebox(ichar s,t)=
messageboxa(0,s,t,0)
end

global function os_hpcounter:int64=
var int64 a

queryperformancecounter(&a)
return a

end

global function os_hpfrequency:int64=
var int64 a

queryperformancefrequency(&a)
return a

end

global proc os_peek=
var int ticks
static var int lastticks
var [100]byte m
	ticks:=GetTickCount()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end
=== mm_support.m 10/35 ===
import clib
import msys
import mlib
import oslib

import mm_decls
import mm_lib
import mm_tables
import mm_gen

global var [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global function loadsourcefile(ichar filespec)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
	var ichar s,shortfile

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi

	shortfile:=extractfile(filespec)

	++nsourcefiles
	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(filespec)
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		loaderror("LSF can't load ",filespec)
	fi
	sourcefiletext[nsourcefiles]:=s
	if fwritema then
		mafiletext[nsourcefiles]:=pcm_copyheapstring(s)
	fi

	sourcefilesizes[nsourcefiles]:=rfsize
	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return nsourcefiles
end

global function loadbuiltin(ichar shortfile, text)int=
!loading build-in header with given text
!Name of header is in 'file'.
	var ichar s
	var [128]char str

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles
	fprint @&.str,"<Built-in: #>",shortfile

	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(&.str)
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
	sourcefiletext[nsourcefiles]:=pcm_copyheapstring(text)
	if fwritema then
		mafiletext[nsourcefiles]:=pcm_copyheapstring(text)
	fi

	sourcefilesizes[nsourcefiles]:=strlen(text)
	return nsourcefiles
end

global function loadbundledfile(ichar filespec)int fileno=
!loading bundled file
!Name of header is in 'file'.
	var ichar file


	file:=extractfile(filespec)

	for i to nmafiles do
		if eqstring(file,mafilenames[i]) then		!found
			fileno:=mafilefileno[i]
			if not fileno then					!cannot overflow sourcefiles; same limits?
				fileno:=++nsourcefiles
				mafilefileno[i]:=fileno

				sourcefilepaths[nsourcefiles]:=mafilenames[i]
				sourcefilenames[nsourcefiles]:=mafilenames[i]
				sourcefiletext[nsourcefiles]:=mafiletext[i]
				sourcefilesizes[nsourcefiles]:=mafilesizes[i]

				if mafilemult[i] then				!might be parses multiple times
					sourcefiletext[nsourcefiles]:=pcm_copyheapstring(mafiletext[i])
				fi
!			ELSE
!				CPL "FOUND BUNDLED FILE SUBSEQ TIME",FILE

			fi
			return fileno
		fi
	od

	loaderror("Can't find bundled file: # #",filespec)
	return 0
end

global proc mcerror(ichar mess)=
println "MC Error:",mess

stop 1
end

global proc serror_gen(ichar mess)=
if currproc and currproc^.nameid=procid then
	print "In function",currproc^.name,," "
fi

println "On line",lx.lineno iand 16777215,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]

println
println "**** Syntax Error:",mess,"****"
stopcompiler(sourcefilepaths[lx.fileno],lx.lineno iand 16777215)
end

proc stopcompiler(ichar filename,int lineno)=
	var filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	fclose(f)
	println
	println
	stop 1
end

global proc serror(ichar mess)=
serror_gen(mess)
end

global proc serror_s(ichar mess,a)=
var [256]char str
fprint @&.str,mess,a
serror_gen(&.str)
end

global proc error_gen(int pass,ichar mess,unit p=nil)=
!general error handling for passes name, type and code gen
!pass='N' 'T' or 'G'
var int lineno,fileno

if p then
	fileno:=p^.lineno>>24
	lineno:=p^.lineno iand 16777215
else

	fileno:=mlineno>>24
	lineno:=mlineno iand 16777215
fi

if currproc and currproc^.nameid=procid then
	print "In function",currproc^.name,," "
fi

println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
println
case pass
when 'N' then print "**** RX Name Error: "
when 'T' then print "**** TX Type Error: "
when 'G' then print "**** GX Code Gen Error: "
when 'A' then print "**** AX Code Gen Error: "
esac
println mess

os_getch()

stopcompiler(sourcefilepaths[fileno],lineno iand 16777215)
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
var [256]char str
fprint @&.str,mess,a
error_gen('T',&.str,p)
end

global proc txerror_ss(ichar mess,a,b)=
var [256]char str
fprint @&.str,mess,a,b
error_gen('T',&.str)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
var [256]char str
fprint @&.str,mess,a
error_gen('N',&.str,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
var [256]char str

fprint @&.str,mess,s
error_gen('G',&.str,p)
end

global proc lxerror_gen(ichar mess)=

println "On line",nextlx.lineno,"in file",sourcefilepaths[nextlx.fileno]

println
println "**** Lex Error:",mess,"****"
println

stopcompiler(sourcefilepaths[nextlx.fileno],nextlx.lineno)
end

global proc lxerror_s(ichar mess,a)=
var [256]char str
fprint @&.str,mess,a
lxerror_gen(&.str)
end

global proc lxerror(ichar mess)=
lxerror_gen(mess)
end

global function testelem(ref[0:]byte p,int n)int =		!TESTELEM
!caller must check that n is in range
return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =		!SETELEM
p^[n>>3] ior:= bytemasks[n iand 7]
end

global function nextpoweroftwo(int x)int=
!return next power of 2 >= x

if x=0 then return 0 fi

var int a:=1
while a<x do
	a<<:=1
od
return a
end

global proc loaderror(ichar mess,mess2="",mess3="")=
	var [512]char str
	fprint @&.str,mess,mess2,mess3

	println "Load Error:",&.str
	println "Stopping"
	stop 1
end

global proc gs_additem(ref strbuffer dest,ichar s)=		!GENITEM
!like genstr, but ensure there is white space separation as needed from the last output
var ichar d
var int lastchar,nextchar

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

global proc inittypetables=
var int i,size,bitsize
var int s,t,u,v

!Initialise type tt-tables from std types first all fields initially zero

for i:=0 to tlast-1 do

	ttname[i]:=stdtypenames[i]
	ttbasetype[i]:=i
	bitsize:=stdtypebits[i]

	switch bitsize
	when 0 then
		size:=0
	when 1,2,4 then
		size:=1
	else
		size:=bitsize/8
	endswitch

	ttsize[i]:=size
	ttbitwidth[i]:=bitsize

	ttisint[i]		:= stdtypecode[i]='I'
	ttisword[i]		:= stdtypecode[i]='U'
	case i
	when tc8,tc16,tc16 then
		ttischar[i]:=1			!else stays at 0
	esac

!	ttischar[i]		:= i=tc8 or i=tc16 or i=tc64
	ttiswordchar[i] := ttisword[i] ior ttischar[i]

	if stdtypecode[i]='R' then
		ttisreal[i]:=1
	fi

!	ttisreal[i]		:= stdtypecode[i]='R'

	ttisinteger[i]	:= ttisint[i] ior ttiswordchar[i]
	ttisnumeric[i]	:= ttisinteger[i] ior ttisreal[i]
	ttisshortint[i]	:= ttisinteger[i] and ttsize[i]<8

	ttisshortreal[i]:= i=tr32

!	ttisbit[i]		:= i=tu1 or i=tu2 or i=tu4
	case i
	when tu1,tu2,tu4 then
		ttisbit[i]:=1
	esac

!	ttisbit[i]		:= i=tu1 or i=tu2 or i=tu4

	if i=tref or i=trefproc then
		ttisref[i]		:= 1
	fi

	tttypecode[i]	:= stdtypecode[i]
	ttisflex[i]		:= stdtypecat[i]=tflex
	ttisvariant[i]	:= stdtypecat[i]=tvariant
	ttisflexvar[i]	:= ttisflex[i] ior ttisvariant[i]

	ttlower[i]:=1
od

ttsize[tref]:=targetsize
!ttsize[tref]:=8

ttbitwidth[tref]:=targetbits

ntypes:=tlast-1
ttbasetype[tenum]:=ti32

!set up dominant/conversion lookup tables from linear table
for i:=1 to typesetuptable.len do
	s:=typesetuptable[i,1]
	t:=typesetuptable[i,2]
	u:=typesetuptable[i,3]
	v:=typesetuptable[i,4]

	dominantmode[s,t]:=u
	conversionops[s,t]:=v

IF V IN [C_IWIDEN, C_UWIDEN] AND TTSIZE[S]=TTSIZE[T] THEN
	CPL "******* WIDEN SAME SIZE??"
FI

od
end

global proc addspecialtypes=
	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
	trefchar:=createrefmode(nil,tc8,0)
end

function findfile(ichar filename)ichar=
!look for file by searching through search dirs
!return nil when not found, or the name of the sucessful filespec
!locate module within search paths
!return full filespec
	static var [300]char filespec

	for i:=nsearchdirs downto 1 do
		strcpy(&.filespec,searchdirs[i])
		strcat(&.filespec,filename)

		if checkfile(&.filespec) then
			return &.filespec
		fi
	od

	return nil
end

global function findstdlib(ichar name)ichar=
	for i:=1 to stdlibnames.len do
		if eqstring(name,stdlibnames[i]) then
			return stdlibtext[i]
		fi
	od
	return nil
end

global function getmainfile(ichar filename)int =
!locate and load lead module filename
	if fbundled then
		return loadbundledfile(filename)
	fi
	if not checkfile(filename) then
		loaderror("Can't find main module: ##",filename)
	fi
	return loadsourcefile(filename)
end

global function getmodulefile(ichar modulename, ownername)int =
	var [300]char filename
	var ichar file,libtext

	strcpy(&.filename,addext(modulename,"m"))

	if fbundled then
		return loadbundledfile(&.filename)
	fi

	if dointlibs then
		libtext:=findstdlib(&.filename)
		if libtext then
			return loadbuiltin(&.filename,libtext)
		fi
	fi

	file:=findfile(&.filename)

	if file=nil then
		loaderror("Can't find import module: # imported in: #",modulename,ownername)
	fi
	return loadsourcefile(file)
end

global function getsupportfile(ichar filename)int =
	var ichar path,file

	if fbundled then
		return loadbundledfile(filename)
	fi

	path:=extractpath(filename)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		file:=filename
	else
		file:=findfile(filename)
	fi

	if file=nil or not checkfile(file) then
		loaderror("Can't find include file: # #",filename)
	fi

	return loadsourcefile(file)
end

global proc writemafile(ichar leadmodule,destfile)=
	var [256]char filename
	var filehandle f
	var [maxsourcefile]int fileoffsets, headeroffsets
	var int offset,nn,NEWOFFSET

	strcpy(&.filename, changeext(leadmodule,"ma"))

	if destfile then
		strcpy(&.filename,destfile)
	fi

	println "Writing MA File",&.filename

	f:=fopen(&.filename,"wb")
	if not f then loaderror("Can't create ma file #",&.filename) fi

	println @f,"mafile",nsourcefiles

	for i to nsourcefiles do
		print @f,i:"3",sourcefilenames[i]:"16jl",sourcefilesizes[i]:"7"
		headeroffsets[i]:=getfilepos(f)+1
		println @f,"         "
	od

	for i to nsourcefiles do
		fprintln @f,"=== # #/# ===",sourcefilenames[i],i,nsourcefiles

		offset:=getfilepos(f)
		fileoffsets[i]:=offset
		nn:=writerandom(f,cast(mafiletext[i]),offset,sourcefilesizes[i])
	od

!Note: the first "=" of the "===" that follows each file may be replaced
!by a zero-terminator after the .ma is loaded
	println @f,"=== end ==="

	for i to nsourcefiles do
		setfilepos(f,headeroffsets[i])
		print @f,fileoffsets[i]:"8"
	od
!
	fclose(f)
end

global proc loadmafile=
	var filehandle f
	var [16]char kwd
	var [256]char filename
	var int index, size, offset

	f:=fopen(mafilename,"rb")
	if not f then
		loaderror("Can't open ##",mafilename)
	fi

	readln @f

	readstr(&.kwd,'n',kwd.len)
	if not eqstring(&.kwd,"mafile") then
		loaderror("Bad sig in ma file: # '#'",mafilename,&.kwd)
	fi
	read nmafiles

	for i to nmafiles do
		readln @f,index
		readstr(&.filename,'n',filename.len)
		read size, offset
		mafilenames[i]:=pcm_copyheapstring(&.filename)
		mafilesizes[i]:=size
		mafileoffsets[i]:=offset
		mafilefileno[i]:=0
		mafilemult[i]:=0
	od
	fclose(f)

!Directory has been read. Now read whole file into memory, use directory
!to set up mafiletext values to each file, and add in terminator
	mafilesource:=cast(readfile(mafilename))
	if not mafilesource then loaderror("MA load?") fi

	for i to nmafiles do
		size:=mafilesizes[i]
		offset:=mafileoffsets[i]

		mafiletext[i]:=mafilesource+offset
		(mafilesource+offset+size)^:=0
	od
end
=== mm_lib.m 11/35 ===
import msys
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lex
import mm_diags

var int autotypeno=0
global var int nextavindex=0
var int nextsvindex=0

var strbuffer exprstrvar
var ref strbuffer exprstr=&exprstrvar

tabledata []int opc_codes, []ichar opc_names =
	(j_add,			"+"),
	(j_sub,			"-"),
	(j_mul,			"*"),
	(j_div,			"/"),
	(j_neg,			"-"),
	(j_eq,			"="),
	(j_ne,			"<>"),
	(j_lt,			"<"),
	(j_le,			"<="),
	(j_gt,			">"),
	(j_ge,			">="),
	(j_iand,		"iand"),
	(j_ior,			"ior"),
	(j_ixor,		"ixor"),
	(j_inot,		"inot"),
	(j_shl,			"<<"),
	(j_shr,			">>"),
	(j_andl,		"and"),
	(j_orl,			"or"),

	(j_notl,		"not"),

	(j_addto,		"+:="),
	(j_subto,		"-:="),
	(j_multo,		"*:="),
	(j_divto,		"/:="),
	(j_negto,		"-:="),
	(j_shlto,		"<<:="),
	(j_shrto,		">>:="),

	(j_preincrx,	"++"),
	(j_postincrx,	"++"),
	(j_predecrx,	"--"),
	(j_postdecrx,	"--"),

	(0,				"")
end

const int unitheapsize=50000
var ref unitrec unitheapptr=nil
var int remainingunits=0

var strbuffer sbuffer
global var ref strbuffer dest=&sbuffer

global var ichar framevarname			!normally nil, set to frame var def to display in comment

global function newstrec:ref strec=
var ref strec p
p:=pcm_alloc(strec.bytes)
memset(p,0,strec.bytes)

p^.lineno:=lx.lineno
p^.moduleno:=currmoduleno
return p
end

global proc initqclib=
var int i

!translate into an instant lookup format
for i:=1 to oplist.len do
	jtagpriotable[oplist[i]]:=oppriolist[i]
od

for i:=1 to D_exprstarterset.len do exprstarterset[D_exprstarterset[i]]:=1 od
for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od

for i:=1 to D_boolunitset.len do boolunitset[D_boolunitset[i]]:=1 od
for i:=1 to D_refunitset.len do refunitset[D_refunitset[i]]:=1 od

for i:=1 to D_binopset.len do binopset[D_binopset[i]]:=1 od
for i:=1 to D_monopset.len do monopset[D_monopset[i]]:=1 od

condopset[j_eq]:=1
condopset[j_ne]:=1
condopset[j_lt]:=1
condopset[j_le]:=1
condopset[j_ge]:=1
condopset[j_gt]:=1

exprtermset[opsym]:=1
exprtermset[atsym]:=1
exprtermset[lbracksym]:=1
exprtermset[ptrsym]:=1
exprtermset[lsqsym]:=1
exprtermset[lcurlysym]:=1
exprtermset[dotsym]:=1
exprtermset[colonsym]:=1
exprtermset[incrsym]:=1
exprtermset[anddotsym]:=1
exprtermset[assignsym]:=1
exprtermset[addrsym]:=1
exprtermset[rangesym]:=1
end

global function getduplnameptr(ref strec owner,symptr,int id)ref strec=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
var ref strec p,q

p:=newstrec()

p^.name:=symptr^.name
p^.namelen:=symptr^.namelen
p^.symbol:=namesym
p^.owner:=owner
p^.nameid:=id
p^.namecat:=defaultnamecat[id]

if id=frameid or id=paramid then
!	p^.frame:=1
fi

p^.nextdupl:=symptr^.nextdupl
p^.firstdupl:=symptr
symptr^.nextdupl:=p

return p
end

global proc adddef(ref strec owner,p)=
!add new st def p, to existing deflist of owner
!assumes p already has a .owner link to owner, but is not yet part of owner's deflist
!pgeneric points to the 'generic' entry for this name in the main hash table.
!this is needed as it contains the head of the dupl list for this name (linking
!all instances of this name).
!Usually the dupl list is checked to ensure that there are no existing names
!with this same owner. (pgeneric can be nil to avoid this check.)
!ASSUMES THAT P IS LAST THING ADDED TO HEAD OF DUPLLIST (just after PGENERIC)
var ref strec q

if q:=p^.nextdupl then
	if q^.owner=owner then
		cpl q^.name,"in",owner^.name
		serror("Duplicate name")
	fi
fi

if owner^.deflist=nil then			!first def
	owner^.deflist:=p
else
	owner^.deflistx^.nextdef:=p
fi
owner^.deflistx:=p

end

global proc adddef_nodupl(ref strec owner,p)=
!version of adddef() that doen't check for duplicates

if owner^.deflist=nil then			!first def
	owner^.deflist:=p
else
	owner^.deflistx^.nextdef:=p
fi
owner^.deflistx:=p
end

global function createname(ref strec p)ref unitrec=
var ref unitrec u

u:=allocunitrec()
u^.tag:=j_name
u^.def:=p

return u
end

global function createunit0(int tag)ref unitrec=
var ref unitrec u

u:=allocunitrec()
u^.tag:=tag
return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
var ref unitrec u
u:=allocunitrec()
u^.tag:=tag
u^.a:=p
return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
var ref unitrec u

u:=allocunitrec()

u^.tag:=tag
u^.a:=p
u^.b:=q
return u
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
var ref unitrec u

u:=allocunitrec()
u^.tag:=tag
u^.a:=p
u^.b:=q
u^.c:=r
return u
end

global proc insertunit(unit p,int tag)=		!INSERTUNIT
!wrap extra unit around p, with given tag
!p itself is effectively changed
var unit q,nextunit
var int mode

q:=allocunitrec()
q^:=p^
mode:=q^.mode
nextunit:=q^.nextunit
q^.nextunit:=nil

memset(p,0,unitrec.bytes)
p^.tag:=tag
p^.lineno:=q^.lineno
p^.a:=q
p^.mode:=mode
p^.nextunit:=nextunit
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
var unit r:=p^.nextunit
p^:=q^
p^.nextunit:=r
end

global function createconstunit(word64 a, int t)ref unitrec=
var ref unitrec u
u:=allocunitrec()
u^.tag:=j_const
u^.value:=a
u^.mode:=t

u^.isconst:=1
return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
var ref unitrec u
u:=allocunitrec()
u^.tag:=j_const
u^.svalue:=s
u^.mode:=trefchar
u^.isastring:=1
if length=-1 then
	u^.slength:=strlen(s)
else
	u^.slength:=length
fi
return u
end

global function getoptocode(int opc)int=		!GETOPTOCODE
!opc is kadd etc
!return matching kaddto, etc
static var [0:jtagnames.len]int16 opctotable
var int n,opcto,i
var [20]char str

opcto:=opctotable[opc]
if opcto then return opcto fi				!find

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

global function createtype(ref strec d)int=			!CREATETYPE
!name can be a string, or pointer to the st, or might be a type already
!convert to a moderec handle, and return that

!might be a string; turn it into a 
if d^.nameid=typeid then	!has already been resolved as type
	return d^.mode
fi
return createusertype(d)
end

global function createusertype(ref strec stname)int=		!CREATEUSERTYPE
!create new, named user type
if ntypes>=maxtype then
cpl ntypes,stname^.name
	serror("Too many types")
fi

++ntypes
ttname[ntypes]:=stname^.name

ttnamedef[ntypes]:=stname
ttbasetype[ntypes]:=tvoid
ttlineno[ntypes]:=lx.lineno

STNAME^.MODE:=NTYPES

return ntypes
end

global function createusertypefromstr(ichar name)int=		!CREATEUSERTYPE
!create new, named user type
var ref strec stname
!CPL "USERTYPE FROM STR",NAME

stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
adddef((stmodule|stmodule|stprogram),stname)
return createusertype(stname)
end

global function getconstvalue(ref unitrec p,int ID=0)int64=	!GETCONSTVALUE
!extract value from kconst
if p and p^.tag=j_const then
	return p^.value
fi
serror("GCV Not constant")
return 0
end

global function getrangelwbunit(ref unitrec p)ref unitrec=				!GETRANGELWB
if p^.tag=j_makerange then
	return p^.a
else
	return createunit1(j_lwb,p)
fi
end

global function getrangeupbunit(ref unitrec p)ref unitrec=				!GETRANGEUPB
if p^.tag=j_makerange then
	return p^.b
else
	return createunit1(j_upb,p)
fi
end

global function createarraymode(ref strec owner,int target,unit dimexpr, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
!length is length, unless lx<>nil!
var int atype,k,m

atype:=(target in [tu1, tu2, tu4]|tfixedbits|tfixedarray)

if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=atype and tttarget[k]=target and \
!			ttlower[k]=lower and ttlength[k]=length then
!			return k
!		fi
!	od
!CPL "ARRAY"
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=atype
ttlower[m]:=1
ttdimexpr[m]:=dimexpr
storemode(101,owner,target,&tttarget[m])
ttowner[m]:=owner

return m
end

global function createflexarraymode(ref strec owner,int target,int typedefx)int=		!CREATEARRAYMODE
var int atype,k,m

atype:=(target in [tu1, tu2, tu4]|tflexbits|tflexarray)

if typedefx=0 then		!anon type
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=atype
ttlower[m]:=1
!ttdimexpr[m]:=dimexpr
storemode(101,owner,target,&tttarget[m])
ttowner[m]:=owner
ttisflex[m]:=1
ttsize[m]:=targetsize

return m
end

global function createarraymodek(ref strec owner,int target,int lower,length, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
var int atype,k,m

atype:=tfixedarray

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

storemode(101,owner,target,&tttarget[m])
ttowner[m]:=owner

return m
end

global function createsetmode(ref strec owner,unit dimexpr, int typedefx)int=		!CREATEARRAYMODE
var int k,m

if typedefx=0 then		!anon type
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=tfixedset
ttlower[m]:=0
ttdimexpr[m]:=dimexpr
ttowner[m]:=owner

return m
end

global function createsetmodek(ref strec owner,int length, int typedefx)int=		!CREATEARRAYMODE
var int k,m

if typedefx=0 then		!anon type
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=tfixedset
ttlower[m]:=0
ttlength[m]:=length
ttowner[m]:=owner

return m
end

global function createdictmode(ref strec owner,int target,keymode, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
!length is length, unless lx<>nil!
var int atype,k,m

if typedefx=0 then		!anon type
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=tflexdict
ttlower[m]:=1
storemode(101,owner,target,&tttarget[m])
storemode(201,owner,keymode,&ttkeymode[m])
ttowner[m]:=owner
ttisflex[m]:=1

return m
end

global function nextautotype:ichar=
static var [32]char str

!sprintf(&.str,(ctarget|"_T$%lld"|"$T%lld"),++autotypeno)
print @&.str,(ctarget|"_T$"|"$T"),,++autotypeno
return &.str
end

global proc converttoslice(int t,sltype)=
ttbasetype[t]:=sltype
ttsize[t]:=ttsize[tslice]
end

global function createslicemode(ref strec owner,int target,unit dimexpr, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
var int k,m

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
if dimexpr then
	ttdimexpr[m]:=dimexpr
else
	ttlower[m]:=1
fi
storemode(178,owner,target,&tttarget[m])
ttowner[m]:=owner

return m
end

global function createslicemodek(ref strec owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
var int k,m

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
tttarget[m]:=target
ttowner[m]:=owner

return m
end

global function createstringmode(int t,length,typedefx)int=		!CREATESTRINGMODE
!create fixed-bound string mode
!length is max length of string (including any count or terminator)
!ts is tstring or tcstring
var int k,m

if typedefx=0 then			!typedefx=1 means creating usertype; can't share
	for k:=tlast to ntypes do
		if ttusercat[k]=0 and ttbasetype[k]=t and ttlength[k]=length then
			return k
		fi
	od
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=t
ttlower[m]:=(t=trefchar|1|0)
ttsize[m]:=length
ttlength[m]:=length

return m
end

global function createrefmode(ref strec owner,int target,typedefx=0)int=		!CREATEREFPACKMODE
var int k,m

if typedefx=0 then		!anon type
	for k:=tlast to ntypes do
		if ttusercat[k]=0 and ttbasetype[k]=tref and tttarget[k]=target then
!		if  ttbasetype[k]=trefpacked and tttarget[k]=target then
			return k
		fi
	od
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

storemode(102,owner,target,&tttarget[m])
ttbasetype[m]:=tref
ttsize[m]:=ttsize[tref]
ttisref[m]:=1
tttypecode[m]:='P'

return m
end

global function createrefbitmode(ref strec owner,int target,typedefx=0)int=		!CREATEREFPACKMODE
var int k,m

if typedefx=0 then		!anon type
	for k:=tlast to ntypes do
		if ttusercat[k]=0 and ttbasetype[k]=trefbit and tttarget[k]=target then
!		if  ttbasetype[k]=trefpacked and tttarget[k]=target then
			return k
		fi
	od
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

storemode(102,owner,target,&tttarget[m])
ttbasetype[m]:=trefbit
ttsize[m]:=ttsize[trefbit]

return m
end

global function createsubrangemode(ref strec owner,unit prange,int typedefx=0)int=		!CREATEREFPACKMODE
var int k,m

if typedefx=0 then		!anon type
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

ttbasetype[m]:=tsubrange
ttsize[m]:=ttsize[tsubrange]
ttdimexpr[m]:=prange

return m
end

global function createrefprocmode(ref strec owner,stproc, paramlist,int kwd, prettype,typedefx)int=		!CREATEREFPROCMODE
!create a ref proc mode; (can't create a proc mode by itself, as it's meaningless)
var int m, mproc

mproc:=createusertype(stproc)

stproc^.paramlist:=paramlist
stproc^.mode:=prettype
ttbasetype[mproc]:=tproc

!don't bother looking for similar proc sig; each one is unique
if typedefx=0 then		!anon type
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

storemode(103,owner,mproc,&tttarget[m])
ttbasetype[m]:=tref

ttsize[m]:=ttsize[tref]
ttisref[m]:=1

return m
end

global proc setnameptr(ref unitrec p)=		!SETNAMEPTR
!p is a just created j_...def unit which has a nameptr in the .a parameter
!set up an xref from the strec back to the -def unit
!Set up a pointer in the associated strec that points back to q

p^.def^.code:=p
end

global function getdottedname(ref strec p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
static var [256]char str
var [256]char str2
var ref strec owner

strcpy(&.str,p^.name)
owner:=p^.owner
while owner and owner^.nameid<>programid do
	strcpy(&.str2,&.str)
	strcpy(&.str,owner^.name)
	strcat(&.str,".")
	strcat(&.str,&.str2)
	owner:=owner^.owner
od
return &.str
end

global function getavname(ref strec owner,int id=frameid)ref strec=
!create auto-var name and return pointer to st entry
var ref strec p
var [32]char str
var ichar name

if id=frameid and owner^.nameid<>procid then
	serror("Auto frame var not in proc")
fi

if id=frameid then
	print @&.str,(ctarget|"av_"|"av$"),,++nextavindex
else
	print @&.str,(ctarget|"sv_"|"sv$"),++nextsvindex
fi

name:=pcm_copyheapstring(&.str)
addnamestr(name)

p:=getduplnameptr(owner,addnamestr(name),id)
p^.namecat:=frame_cat
p^.used:=1

p^.mode:=tint

adddef(owner,p)
return p
end

global proc unionstr_clear(ref uflagsrec u)=

((ref word64(u))^:=0)		!clear flags and length togetjer
end

global proc unionstr_append(ref uflagsrec u, int c)=
if u^.ulength=(u^.codes.len-1) then
	serror("Uflags overflow/a")
fi
++u^.ulength
u^.codes[u^.ulength]:=c
end

global proc unionstr_concat(ref uflagsrec u, v)=
var int ulen,vlen,i

ulen:=u^.ulength
vlen:=v^.ulength
if ulen+vlen>u^.codes.len then
	serror("Uflags overflow/c")
fi
for i:=1 to vlen do
	u^.codes[i+ulen]:=v^.codes[i]
od
u^.ulength:=ulen+vlen
end

global function unionstr_last(ref uflagsrec u)int=
if u^.ulength then
	return u^.codes[u^.ulength]
fi
return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
memcpy(u,v,uflagsrec.bytes)
end

global proc unionstr_print(ref uflagsrec u)=
printstrn(cast(&u^.codes),u^.ulength)
end

global function createrecordmode(ref strec owner,int t,typedefx)int=	!CREATERECORDMODE
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def::
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
var int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=t			!record/struct/union
ttusercat[m]:=1

return m
end

global function createenummode(ref strec owner,int typedefx)int=		!CREATEENUMMODE
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def::
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
var int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=tenum
ttusercat[m]:=1

return m
end

global proc convertstring(ichar s, t)=		!CONVERTSTRING
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
var int c

while c:=s++^ do
	switch c
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
	else
		t++^:=c
	endswitch
od
t^:=0
end

global function strexpr(ref unitrec p)ref strbuffer=		!STREXPR
!vx_makestring("",exprstr)
gs_init(exprstr)

jeval(exprstr,p)
return exprstr
end

proc jeval(ref strbuffer dest, ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
var unit q,a,b
var [500]char str

if p=nil then
	return
fi

a:=p^.a
b:=p^.b

switch p^.tag
when j_const then

	case ttbasetype[p^.mode]
	when ti32,ti64,ti8,ti16 then
		getstrint(p.value,&.str)
	when tu32,tu64,tu8,tu16 then
		strcpy(&.str,strword(p.uvalue))
	when tc8,tc16,tc64 then
		str[1]:=p.uvalue
		str[0]:=0
	when ti128,tu128 then
		print @&.str,p^.qvalue^.upper,,":",,p^.qvalue^.lower

	when treal then
		print @&.str,p^.xvalue
	when tref then
		if p^.mode=trefchar and p^.isastring then
			if p^.slength>str.len/2 then
				strcpy(&.str,"LONGSTR)")
			else
				convertstring(p^.svalue,&.str)
			fi
			gs_additem(dest,"""")
			gs_additem(dest,&.str)
			gs_additem(dest,"""")
			return
		else
			print @&.str,ref void(p^.value)
		fi
	else
		case ttbasetype[p^.mode]
		when trange64 then
			fprint @&.str,"#..#",p.qvalue.lower,p.qvalue.upper
		else
SPRINTF(&.STR,"<EVAL/CONST PROBABLY VOID>")
!			CPL typename(p^.mode),STRMODE(TTBASETYPE[P^.MODE])
!			rxerror("EVAL/CONST",p)
		esac
	esac
	gs_additem(dest,&.str)

when j_name then
	gs_additem(dest,p^.def^.name)

when j_andl,j_orl,j_andand,j_eq,j_ne,j_lt,j_le,j_gt,j_ge,j_add,j_sub,j_mul,j_div,j_idiv,
	j_irem,j_iand,j_ior,j_ixor,j_shl,j_shr,j_in,j_notin,j_inrev,j_min,j_max,
	j_subref,j_addoffset,j_suboffset,
	j_concat,j_atan2,j_power, j_xorl, j_isequal, j_idivrem, j_append then

	strcpy(&.str,getopcjname(p^.tag))
	gs_additem(dest,"(")
	jeval(dest,a)
	gs_additem(dest,&.str)
	jeval(dest,b)
	gs_additem(dest,")")

when j_neg,j_abs,j_inot,j_sqrt,j_sqr,j_cube,j_sign,j_sin,j_cos,j_tan,j_asin,
	j_acos,j_atan,j_ln,j_lg,j_log,j_exp,j_round,j_floor,j_ceil,j_fract,j_fmod, j_lwb,j_upb,j_len,	j_bitwidth,j_bytesize,
	j_minvalue,j_maxvalue,
	j_notl,j_istruel then

	strcpy(&.str,getopcjname(p^.tag))
	gs_additem(dest,&.str)
	gs_additem(dest,"(")
	jeval(dest,a)
	gs_additem(dest,")")

when j_callfn,j_callproc then
	jeval(dest,a)
	gs_additem(dest,"(")

	q:=b
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	gs_additem(dest,")")

when j_index,j_dotindex,j_slice,j_dotslice then
	jeval(dest,a)
	if p^.tag=j_dotindex or p^.tag=j_dotslice then
		gs_additem(dest,".")
	fi
	gs_additem(dest,"[")
	jeval(dest,b)
	gs_additem(dest,"]")

when j_dot then
	jeval(dest,a)
	gs_additem(dest,".")
	jeval(dest,b)

when j_makelist then
	gs_additem(dest,"(")

	q:=a
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	gs_additem(dest,")")

when j_makerange then
	gs_additem(dest,"(")
	jeval(dest,a)
	gs_additem(dest,"..")
	jeval(dest,b)
	gs_additem(dest,")")

when j_assignx then
	jeval(dest,a)
	gs_additem(dest,":=")
	jeval(dest,b)

when j_if then
	gs_additem(dest,"(")
	jeval(dest,a)
	gs_additem(dest,"|")
	jeval(dest,b)
	gs_additem(dest,"|")
	jeval(dest,p^.c)
	gs_additem(dest,")")

when j_typeconst then
	gs_additem(dest,strmode(p^.mode))

when j_convert then

	gs_additem(dest,strmode(p^.newmode))
	gs_additem(dest,"(")
	jeval(dest,a)
	gs_additem(dest,")")
when j_autocast then

	gs_additem(dest,"cast(")
	jeval(dest,a)
	gs_additem(dest,")")
when j_keyvalue then
	jeval(dest,a)
	gs_additem(dest,":")
	if b then
		jeval(dest,p^.b)
	else
		gs_str(dest,"-")
	fi

when j_ptr then
	jeval(dest,a)
	gs_additem(dest,"^")

when j_clamp then
	gs_additem(dest,"(")
	jeval(dest,a)
	gs_additem(dest,",")
	jeval(dest,b)
	gs_additem(dest,",")
	jeval(dest,p^.c)
	gs_additem(dest,")")

when j_block then
	gs_additem(dest,"<JBLOCK>")

when j_null then
	gs_str(dest,"<nullunit>")

when j_addrof then
	gs_additem(dest,"&")
	jeval(dest,a)

when j_addroffirst then
	gs_additem(dest,"&.")
	jeval(dest,a)

when j_convertref then
	gs_str(dest,"CONVERTREF<>")

when j_typestr then
	gs_additem(dest,"TYPESTR(")
	jeval(dest,a)
	gs_additem(dest,")")

when j_head, j_tail, j_init, j_last, j_take, j_drop, j_reverse, j_left, j_right,
	 j_convlc, j_convuc, j_flexptr, j_stringz then

	gs_str(dest,jtagnames[p^.tag]+2)
	gs_str(dest,"(")
	jeval(dest,a)
	case p^.tag
	when j_take,j_drop, j_convuc,j_convlc, j_left,j_right then
		gs_str(dest,",")
		jeval(dest,b)
	esac
	gs_str(dest,")")
when j_cvlineno, j_cvfilename, j_cvmodulename then
	gs_str(dest,"$")
	gs_str(dest,jtagnames[p^.tag]+2)

when j_bitfield then
	jeval(dest,a)
	gs_str(dest,".")
	gs_str(dest,bitfieldnames[p^.opcode])

when j_fmtitem then
	jeval(dest,a)
	gs_str(dest,":")
	jeval(dest,b)

when j_typeof then
	gs_str(dest,"typeof(")
	jeval(dest,a)
	gs_str(dest,")")

when j_syscall then
	gs_str(dest,sysfnnames[p.opcode]+6)
	gs_str(dest,"(")
	if a then jeval(dest,a) fi
	gs_str(dest,")")

else
	CPL jtagnames[p^.tag]
	gerror("CAN'T DO JEVAL",p)
end
end

global function getopcjname(int opc)ichar=		!GETOPCJNAME
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
var int i
var [20]char str

for i:=1 to opc_codes.len do		!look for dedicated op name
	if opc=opc_codes[i] then
		return opc_names[i]
	fi
od

return jtagnames[opc]+2				!return standard jtag name
end

global function strmode(int m,expand=1)ichar=		!STRMODE
static var [4096]char str
istrmode(m,expand,&.str)
return &.str
end

global function strmode2(int m,expand=1)ichar=		!STRMODE
static var [4096]char str
istrmode(m,expand,&.str)
return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=		!ISTRMODE
var ref strec d,q,e
var int value,needcomma,x,i,target,mbase
var strbuffer sxx
var ref strbuffer xx:=&sxx
var ref strbuffer sdim,slength
var [100]char strdim
var ichar prefix

if m<0 then
	strcpy(dest,"*")
	strcat(dest,ttnamedefx[-m]^.name)
	if ttnamedefx2[-m] then
		strcat(dest,".")
		strcat(dest,ttnamedefx2[-m]^.name)
	fi
	return
fi

if m<tlast and m<>tref then
	strcpy(dest,typename(m))
	return
fi

case mbase:=ttbasetype[m]
when tref,trefbit then
	strcpy(dest,(mbase=tref|"ref "|"refbit "))
	target:=tttarget[m]
	if target>=0 and ttbasetype[target]=tfixedrecord then
		strcat(dest,typename(target))
	else
		istrmode(tttarget[m],0,dest+strlen(dest))
	fi

when tfixedset then
	if ttdimexpr[m] then
		gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
!		sprintf(dest,"set[%s]",&.strdim)
		fprint @dest,"set[#]",&.strdim
	else
!		sprintf(dest,"set[%lld]",ttlength[m])
		fprint @dest,"set[#]",ttlength[m]
	fi

when tfixedarray,tfixedbits then
	if ttdimexpr[m] then
		gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
!		sprintf(dest,"@[%s]",&.strdim)
		fprint @dest,"@[#]",&.strdim
	else
		if ttlower[m]=1 then
!			sprintf(dest,"[%lld]",ttlength[m]+ttlower[m]-1)
			fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
		else
!			sprintf(dest,"[%lld..%lld]",ttlower[m],ttlength[m]+ttlower[m]-1)
			fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
		fi
	fi
	istrmode(tttarget[m],0,dest+strlen(dest))

when tslice then
	prefix:=(mbase<>tslice|""|stdtypenames[mbase])

	if ttdimexpr[m] then
		gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
!		sprintf(dest,"@slice[%s:]",&.strdim)
		fprint @dest,"@slice[#:]",&.strdim
	else
		if ttlower[m]=1 then
			strcpy(dest,"slice[]")
		else
			fprint @dest,"slice[#:]",ttlower[m]
		fi
	fi
	istrmode(tttarget[m],0,dest+strlen(dest))

when tflexarray,tflexbits then
	strcpy(dest,"flex[]")
	istrmode(tttarget[m],0,dest+strlen(dest))

when tflextable then
	if ttdimexpr[m] then
		gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
!		sprintf(dest,"[%s:*,*]",&.strdim)
		fprint @dest,"[#:*,*]",&.strdim
	else
		strcpy(dest,"[*,*]")
	fi
	istrmode(tttarget[m],0,dest+strlen(dest))

when tenum then
	strcpy(dest,"enum(")
	d:=ttnamedef[m]

	value:=1
	needcomma:=0
	q:=d^.deflist
	while q do
!	forall i,q in d.deflist do
		if needcomma then strcat(dest,",") fi
		needcomma:=1
		strcat(dest,q^.name)
		x:=q^.index
		if x<>value then
			value:=x
!			sprintf(dest+strlen(dest),"%lld",value)
			getstrint(value,dest+strlen(dest))
		fi
		++value
		q:=q^.nextdef
	od

	strcat(dest,")")

when tfixedrecord,tflexclass then
	strcpy(dest,"")
	if not expand then
		strcpy(dest,typename(m))
		return
	fi
	strcat(dest,typename(ttbasetype[m]))
	strcat(dest,"(")
	d:=ttnamedef[m]
	needcomma:=0

	q:=d^.deflist
	while q do
		if needcomma then strcat(dest,",") fi
		needcomma:=1
		istrmode(q^.mode,0,dest+strlen(dest))
		strcat(dest," ")
		strcat(dest,q^.name)
		q:=q^.nextdef
	od
	strcat(dest,")")

when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
	strcpy(dest,"void")

when tuser then
	strcpy(dest,typename(m))
when tproc then


	d:=ttnamedef[m]
	strcpy(dest,"proc(")
	q:=d^.paramlist
	needcomma:=0
	while q<>nil do
		if needcomma then strcat(dest,",") fi
		needcomma:=1
		istrmode(q^.mode,0,dest+strlen(dest))
		strcat(dest," ")
		strcat(dest,q^.name)
		q:=q^.nextdef
	od
	strcat(dest,")")
	if d^.mode<>tvoid then
		istrmode(d^.mode,0,dest+strlen(dest))
	fi

when trange64 then
	strcpy(dest,"range")

when tflexdict then
	strcpy(dest,"dict[")
	istrmode(ttkeymode[m],0,dest+strlen(dest))
	strcat(dest,"]")
	istrmode(tttarget[m],0,dest+strlen(dest))

when tsubrange then
	strcpy(dest,"subrange(")
	strcat(dest,strexpr(ttdimexpr[m])^.strptr)
	strcat(dest,")")

when tbitfield then
	strcpy(dest,"bitfield")

else
CPL typename(m)
	mcerror("NEWSTRMODE")
!	return "NEWSTRMODE:"+TOSTR(M)+":"+TOSTR(M.BASETYPENO)
esac
end

global function countunits(ref unitrec p)int=
var int n
n:=0
while p do
	++n
	p:=p^.nextunit
od
return n
end

global function finddefstr(ref strec owner,ichar name)ref strec=	!FINDDEFSTRING
!scan owner looking for a name
!return symptr if found, or nil
var ref strec d

d:=owner^.deflist
while d do
	if eqstring(d^.name,name) then
		return d
	fi
	d:=d^.nextdef
od

return nil
end

global proc addtoproclist(ref strec d)=
	var ref procrec pp
	++nproclist
	pp:=pcm_alloc(procrec.bytes)
	pp^.nextproc:=proclist
	proclist:=pp
	pp^.def:=d
end

global proc addstatic(ref strec d)=
	var ref procrec pp
	++nstaticlist
	pp:=pcm_alloc(procrec.bytes)
	pp^.nextproc:=staticlist
	staticlist:=pp
	pp^.def:=d
end

global function newusertypex(ref strec d,e=nil)int=
var int i

!first check whether same name has been used as userx type in this module
!for i:=userxtypebase to nuserxtypes do
!	if ttnamedefx[i]=d and ttnamedefx2[i]=e then			!same st entry, assume type
!		return -i
!	fi
!od

if nuserxtypes>=maxuserxtype then
	serror("Too many external user types")
fi
++nuserxtypes
ttnamedefx[nuserxtypes]:=d
ttnamedefx2[nuserxtypes]:=e
ttxmoduleno[nuserxtypes]:=currmoduleno
ttlinenox[nuserxtypes]:=lx.lineno iand 16777215
return -nuserxtypes

end

global function typename(int m)ichar=
if m>=0 then
	return ttname[m]
fi
return ttnamedefx[-m]^.name
end

global function allocunitrec:ref unitrec=
var ref unitrec p
var ref int64 q
var int nwords

++NUNITS

if remainingunits-- then
	p:=unitheapptr
	++unitheapptr
	p^.lineno:=lx.lineno
	p^.moduleno:=currmoduleno
	return p
fi

!need first or new heap
p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

memset(p,0,unitheapsize*unitrec.bytes)
remainingunits:=unitheapsize-1
++unitheapptr
p^.lineno:=lx.lineno
p^.moduleno:=currmoduleno
return p
end

global function createdupldef(ref strec owner,symptr, int id)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
var ref strec p,q

p:=newstrec()

p^.name:=symptr^.name
p^.namelen:=symptr^.namelen
p^.symbol:=namesym
p^.owner:=owner
p^.nameid:=id

p^.nextdupl:=symptr^.nextdupl
symptr^.nextdupl:=p

if owner then
	if owner^.deflist=nil then			!first def
		owner^.deflist:=owner^.deflistx:=p
	else
		owner^.deflistx^.nextdef:=p
		owner^.deflistx:=p
	fi
fi

return p
end

global function createnewmoduledef(ref strec owner,symptr)ref strec=
var ref strec p,q

p:=createdupldef(owner,symptr,moduleid)
return p
end

global proc storemode(INT ID,ref strec owner, int m, ref int32 p)=
var ref userxrec q
p^:=m
if m>=0 then return fi

q:=pcm_alloc(userxrec.bytes)
q^.owner:=owner

IF OWNER=NIL THEN
CPL =ID
SERROR("STOREMODE/OWNER=0")
FI

q^.pmode:=p
q^.nextmode:=userxmodelist
userxmodelist:=q
end

global function duplunit(unit p,int lineno=0)unit=
var unit q
if p=nil then return nil fi

q:=createunit0(p^.tag)

q^.a:=duplunit(p^.a,lineno)
q^.b:=duplunit(p^.b,lineno)
q^.c:=duplunit(p^.c,lineno)
q^.lineno:=(lineno|lineno|p^.lineno)
q^.value:=p^.value			!copy main field of each union
q^.opcode:=p^.opcode
q^.mode:=p^.mode
q^.moduleno:=p^.moduleno
q^.isastring:=p^.isastring

return q
end

!global proc addstr(ref char &p, ref char s)=
!while s^ do
!	p^:=s^
!	++p
!	++s
!od
!end
!
!global proc addchar(ref char &p, char c)=
!p^:=c
!++p
!end
!
!global proc addint(ref char &p, int64 x)=
!var [64]char str
!!sprintf(&.str,"%lld",x)
!!addstr(p,&.str)
!addstr(p,strint(x))
!end

global function iscallbackfn(ref strec p)int=
!return 1 if p is a function with clang atribute (needs caller stack adjust)

return p^.fflang=callbackff
end

global function isstringconst(unit p)int=
var int m,target
m:=p^.mode
if p^.tag=j_const and ttbasetype[m]=tref then
	target:=ttbasetype[tttarget[m]]
	if target=tc8 or p^.slength then
		return 1
	fi
fi

return 0
end

global function checkblockreturn(unit p)int=
!p is any statement
!check p, or the last statement of a block, or the last statement of any
!nested block, a return, or is a unit yielding some value (as explicit return
!statement not needed)
! return 1/0 for return/not return
var unit e,wt
var int m,res

if p=nil then return 0 fi
m:=p.mode

case p^.tag
when j_return then			!that's an easy one...
	return 1
when j_stop then
	return 1
when j_if then
	p.ifretflag:=1
	return checkblockreturn(p^.b) and checkblockreturn(p^.c)		!all branches must have a return

when j_longif then
	e:=p^.a
	p.ifretflag:=1
	while e do
		if not checkblockreturn(e^.b) then
			return 0
		fi
		e:=e^.nextunit
	od
	return checkblockreturn(p^.b)		!else must have return too
when j_block then
	e:=p^.a
	if e then
		while e and e^.nextunit do
			e:=e^.nextunit
		od
		return checkblockreturn(e)
	fi

when j_case, j_switch, j_docase, j_doswitch then
	p.ifretflag:=1
	wt:=p^.b
	while wt do
		if not checkblockreturn(wt^.b) then
			return 0
		fi

		wt:=wt^.nextunit
	od

	return checkblockreturn(p^.c)		!else

when j_assem then						!assume yes
	return 1
esac

if jisexpr[p.tag] and m<>tvoid then
	if ctarget then
		insertunit(p,j_return)
		p.mode:=m
	fi

	return 1							!any non-void expr allowed as return value
else
	return 0
fi
end

global function strqvalue(ref qint aa)ichar=
static var [64]char str

fprint @&.str,"#:#\n",aa.upper:"H",aa^.lower:"Z16H"
return &.str
end

global function makeqvalue(int64 a,scat)ref qint=
!t=ti128/tu128
!scat='U' or 'I' that represents type of a

var ref qint aa

aa:=pcm_alloc(qint.bytes)
aa^.lower:=a
aa^.upper:=0

if scat='I' and a<0 then
	aa^.upper:=0xFFFF'FFFF
fi
return aa
end

global function makeqvalue_ab(int64 a,b)ref qint=
var ref qint aa

aa:=pcm_alloc(qint.bytes)
aa^.lower:=a
aa^.upper:=b

return aa
end

global function isconstint(unit a)int=
if a^.isconst and isintmode(a^.mode) then return 1 fi
return 0
end

global function isconstunit(unit a)int=
return a^.isconst
end

global function faststrint(int a)ichar=
static var [-999..999,8]char smallints

!++NALLNOS

if smallints.lwb <= a <= smallints.upb then
!++NSMALL
	if smallints[a,1]=0 then
		getstrint(a,&.smallints[a])
	fi
	return &.smallints[a]
fi
return strint(a)
end

global function getfullname(ref strec d,int fromassem=0)ichar=
static var [256]char str

!	return d^.name
if not fromassem and fvarnames<>2 then
	case d^.nameid
	when frameid,paramid then
		if fvarnames then framevarname:=d^.name fi
		return faststrint(d.offset)
!		getstrint(d.offset,&.str)
!		return &.str
	esac
fi

if qualifiedname[d^.namecat] then
	str[1]:=0
	getownername(d,&.str)
	strcat(&.str,d^.name)
	return &.str

elsif d^.namecat=dllproc_cat then		!needs full case shown
!	sprintf(&.str,"`%s",(d^.truename|d^.truename|d^.name))
	print @&.str,"`",,(d^.truename|d^.truename|d^.name)
	return &.str

else
	return d^.name
fi
end

global proc getownername(ref strec d, ichar dest)=
var ref strec owner

owner:=d^.owner

if owner=nil or owner^.nameid=programid then return fi
getownername(owner,dest)
strcat(dest,owner^.name)
strcat(dest,".")
end

global function isintmode(int m)int=
	return ttisinteger[m]
end

global function isnumericmode(int m)int=
	return ttisnumeric[m]
end

global function isrefmode(int m)int=
	return ttisref[m]
end

global function strconstopnd(unit p)ichar=
!p is a const unit containing int/word/real
	static var [256]char str
	var int i,a,t
	var real32 x32

	t:=p^.mode
	a:=p^.value

	if t=trefchar then
		if p^.slength>=256 then
			print @&.str,"""",,"(LONGSTR)",""" *",,p^.slength
		elsif p^.slength then
			print @&.str,"""",,p^.svalue,,""" *",,p^.slength
		else
			print @&.str,""""""
		fi

	elsecase ttbasetype[t]
	when ti8 then print @&.str,int8(a)
	when ti16 then print @&.str,int16(a)
	when ti32 then print @&.str,int32(a)
	when ti64 then print @&.str,int64(a)
	when tu8 then print @&.str,word8(a)
	when tu16 then print @&.str,word16(a)
	when tu32 then print @&.str,word32(a)
	when tu64 then print @&.str,word64(a)
	when tc8,tc16,tc64 then
! print @&.str,chr(a)
 print @&.str,"C64"
	when tr32 then
		x32:=p^.xvalue
		print @&.str,real64(x32)
	when tr64 then
		print @&.str,p^.xvalue
	when ti128,tu128 then
		print @&.str,strqvalue(p^.qvalue)
	when trange64 then
		print @&.str,p^.qvalue^.lower,,"..",,p^.qvalue^.upper
	when tref then
		if p^.value then
			print @&.str,"#",,p^.value,P^.SLENGTH
		else
			print @&.str,"NIL"
		fi
	else
		cpl typename(t),typename(ttbasetype[t])
		gerror("STROPND CONST?")

	fi

	return &.str
end

global function gettypecat_t(int m)int=
	return stdtypecat[ttbasetype[m]]
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
var int a

case ttbasetype[m]
when tfixedarray then
	return getalignment(tttarget[m])
when tfixedrecord then
	RETURN 16
esac

!CPL "GETALIGN:",STRMODE(M)

a:=ttsize[m]
case a
when 1,2,4,8,16 then
!when 1,2,4,8 then
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
!otherwise return zero when x is negative, 0, 1 not a power of two, or more than 2**31
var int64 a
var int n

a:=1
n:=0
to 30 do
	++n
	a:=a<<1
	if a=x then
		return n
	fi
od
return 0
end

global proc addlistunit(ref unit ulist,ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextunit:=p
fi
ulistx^:=p			!update end-of-list pointer
end

global function issimpletype(int m)int=
	if ttisflexvar[m] then return 0 fi
	if stdtypecat[ttbasetype[m]]=tblock then return 0 fi
	return 1
end

global function getpacktype(int m)int=
!convert ti64 to tp_i64 etc
	var int target,mbase
	var ref strec d

	if m=trefchar then
		return tp_stringz
	fi
!	target:=ttbasetype[tttarget[m]]
	target:=tttarget[m]
	mbase:=ttbasetype[m]

	if mbase<=tr64 then
		case mbase
		when ti64 then return tp_i64
		when tu64 then return tp_u64
		when tr64 then return tp_r64
		when tvoid then return tp_void
		else
			gerror("getpacktype1")
		esac
	elsif ttisref[m] then
		if target in tvoid..tr64 then
			return tp_pvoid+(target-tvoid)
		fi
		d:=ttnamedef[target]
		if d and eqstring(d.name,"varrec") then
			return tp_variant
		fi
	fi
	return tp_void
end

=== mm_lex.m 12/35 ===
import msys
import mlib
import clib
!import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lib
import mm_mcldecls

GLOBAL VAR INT NLOOKUPS,NCLASHES

!const diagmode=1
const diagmode=0

const maxstackdepth=20
var [maxstackdepth]ref char lxstart_stack
var [maxstackdepth]ref char lxsptr_stack
var [maxstackdepth]int lxfileno_stack
var [maxstackdepth]int lxlineno_stack
var [maxstackdepth]byte isfile_stack
var int sourcelevel=0

const etx	= 26
const cr	= 13
const lf	= 10
const tab	= 9

var ref char lxstart
var ref char lxsptr
var int lxifcond
var int longsuffix			!for real nos

const hstsize	= 32768
const hstmask	= hstsize-1

global var [0:hstsize]strec hashtable

var []ichar maxnumlist=(
	"",					!1
	"1111111111111111111111111111111111111111111111111111111111111111",   	!2
	"11112220022122120101211020120210210211220",                          	!3
	"33333333333333333333333333333333",                                   	!4
	"2214220303114400424121122430",                                       	!5
	"3520522010102100444244423",                                          	!6
	"45012021522523134134601",                                            	!7
	"1777777777777777777777",                                             	!8
	"145808576354216723756",                                              	!9
	"18446744073709551615",                                               	!10
	"335500516A429071284",                                                	!11
	"839365134A2A240713",                                                 	!12
	"219505A9511A867B72",                                                 	!13
	"8681049ADB03DB171",                                                  	!14
	"2C1D56B648C6CD110",                                                  	!15
	"FFFFFFFFFFFFFFFF")                                                   	!16
var [maxnumlist.len]int maxnumlen

global proc lexreadtoken=
!read next token into nextlx
var int c,hsum,commentseen
var ref char pstart,pnext,p,ss

nextlx.subcode:=0

doswitch lxsptr++^
when 'a'..'z','$','_' then
	nextlx.svalue:=lxsptr-1
doname::
	hsum:=nextlx.svalue^

	nextlx.hashvalue:=0

	doswitch c:=lxsptr++^
	when 'A'..'Z' then
		(lxsptr-1)^:=c+' '
		hsum:=hsum<<4-hsum+c+' '
	when 'a'..'z','0'..'9','_','$' then
		hsum:=hsum<<4-hsum+c
	when '"' then
		--lxsptr
		if nextlx.svalue+1=ref char(lxsptr) then
			case nextlx.svalue^
			when  'F','f','R','r' then 
				readrawstring()
				return
			when  'A','a','Z','z' then 
				readarraystring(nextlx.svalue^)
				return
			esac
		fi
		exit
	else
		--lxsptr
		exit
	end doswitch

	nextlx.symbol:=rawnamesym
	nextlx.length:=lxsptr-nextlx.svalue
	nextlx.hashvalue:=hsum<<5-hsum
!CPL "READ RAWNAME",NEXTLX.LENGTH,NEXTLX.LENGTH:"V", NEXTLX.SVALUE:".*",NEXTLX.HASHVALUE

	return

when 'A'..'Z' then
	nextlx.svalue:=lxsptr-1
	nextlx.svalue^+:=32
!	hsum:=csum:=nextlx.svalue^
	goto doname

when '0'..'9' then
	c:=(lxsptr-1)^
	case lxsptr^
	when ' ',')',cr,',','|' then		!assume single digit decimal
!	when ' ',')',cr,',' then		!assume single digit decimal
		nextlx.symbol:=intconstsym
		nextlx.subcode:=tint
		nextlx.value:=c-'0'
	when 'x','X' then
		case c
		when '0' then		!0x
			++lxsptr
			readnumber(16)
		when '1' then
			lxerror("Bad base")
		else				!other base 2..9
			++lxsptr
			readnumber(c-'0')
		esac
	else
		--lxsptr
!		readnumber(10)
		readdecimalnumber()
	esac
	return

when '!' then			!comment to eol
docomment::
	doswitch c:=lxsptr++^
	when 13 then
		++lxsptr
		exit
	when 10 then
		exit
	when etx,0 then
		--lxsptr
		exit
	end
	++nextlx.lineno
++NALLLINES
	nextlx.symbol:=eolsym
	return

when '#' then			!docstring to eol
	nextlx.symbol:=hashsym
	return

when '\\' then			!line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
	commentseen:=0
	doswitch lxsptr++^			!read until end of this line
	when cr then
++NALLLINES
		++nextlx.lineno
		++lxsptr				!skip lf
		exit
	when lf then
++NALLLINES
		++nextlx.lineno
		exit
	when etx,0 then
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
enddoswitch
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

	doswitch lxsptr++^
	when cr then
		++nextlx.lineno
++NALLLINES
		++lxsptr				!skip lf
	when lf then
++NALLLINES
		++nextlx.lineno
	when ' ',tab then
	else
		--lxsptr
		exit
	enddoswitch
!	next

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
			nextlx.symbol:=rangesym
			nextlx.subcode:=j_makerange		!helps treat as opsym which all have k-code as subcode
		fi
		return
	when '0'..'9' then			!real const: deal with this after the switch
		--lxsptr
		readrealnumber(nil,0,10)
		return
	else
!		p:=lxsptr-2
!		if p<lxstart or p^=cr or p^=lf then
!			nextlx.symbol:=lexdotsym
!		else
			nextlx.symbol:=dotsym
!		fi
		return
	endswitch

when ',' then
	nextlx.symbol:=commasym
	return

when ';' then
	nextlx.symbol:=semisym
	return

when ':' then
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.symbol:=assignsym
		nextlx.subcode:=j_assignx		!helps treat as opsym which all have k-code as subcode
	when ':' then
		++lxsptr
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=deepcopysym
			nextlx.subcode:=j_deepcopyx
		else
			nextlx.symbol:=dcolonsym
		esac
	else
		nextlx.symbol:=colonsym
	endswitch
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
	if lxsptr^='|' then
		++lxsptr
		nextlx.symbol:=dbarsym
	else
		nextlx.symbol:=barsym
	fi
	return

when '^' then
	nextlx.symbol:=ptrsym
	return

when '@' then
	if lxsptr^='@' then
		++lxsptr
		nextlx.symbol:=datsym
	else
		nextlx.symbol:=atsym
	fi
	return

when '?' then
	nextlx.symbol:=questionsym
	return

!when 156 then		!'œ' in ansi font or whatever
!when 'œ' then		!'œ' in ansi font or whatever
!	nextlx.symbol:=poundsym
!	return
!
when '~' then
	nextlx.symbol:=curlsym
	return

!when 'ª' then
!	nextlx.symbol:=gatesym
!	return
!
when '+' then
	nextlx.symbol:=opsym
	if lxsptr^='+' then
		++lxsptr
		nextlx.symbol:=incrsym
		nextlx.subcode:=j_preincrx
		return
	else
		nextlx.subcode:=j_add
	fi
	return

when '-' then
	nextlx.symbol:=opsym
	if lxsptr^='-' then
		++lxsptr
		nextlx.symbol:=incrsym
		nextlx.subcode:=j_predecrx
		return
	else
		nextlx.subcode:=j_sub
	fi
	return

when '*' then
	nextlx.symbol:=opsym
	if lxsptr^='*' then
		++lxsptr
		nextlx.subcode:=j_power
	else
		nextlx.subcode:=j_mul
	fi
	return

when '/' then
	nextlx.symbol:=opsym
!	case lxsptr^
!	when '/' then
!		++lxsptr
!		nextlx.subcode:=j_ddiv
!	else
		nextlx.subcode:=j_div
!	esac
	return

when '%' then
	nextlx.symbol:=opsym
	nextlx.subcode:=j_idiv
	return

when '=' then
	case lxsptr^
	when '>' then
		nextlx.symbol:=sendtosym
		++lxsptr
	when '=' then
		nextlx.symbol:=opsym
		nextlx.subcode:=j_isequal
		++lxsptr
!	when ':' then
!		++lxsptr
!		if lxsptr^<>'=' then lxerror("=:?") fi
!		++lxsptr
!		nextlx.symbol:=dispassignsym
!		nextlx.subcode:=j_dispassign
!CPL "DISPASSIGN"
	else
		nextlx.symbol:=opsym
		nextlx.subcode:=j_eq
	esac
	return

when '<' then
	nextlx.symbol:=opsym
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.subcode:=j_le
	when '>' then
		++lxsptr
		nextlx.subcode:=j_ne
	when '<' then
		++lxsptr
		nextlx.subcode:=j_shl
	else
		nextlx.subcode:=j_lt
	endswitch
	return

when '>' then
	nextlx.symbol:=opsym
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.subcode:=j_ge
	when '>' then
		++lxsptr
		nextlx.subcode:=j_shr
	else
		nextlx.subcode:=j_gt
	endswitch
	return

when '&' then
	case lxsptr^
	when '&' then
		++lxsptr
		nextlx.symbol:=opsym
		nextlx.subcode:=j_andand
	when '.' then
		++lxsptr
		nextlx.symbol:=anddotsym
		nextlx.subcode:=0
	else
		nextlx.symbol:=addrsym
		nextlx.subcode:=j_addrof
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
	++nextlx.lineno
++NALLLINES
	nextlx.symbol:=eolsym
	return
when lf then			!only lfs not preceded by cr
	++nextlx.lineno
++NALLLINES
	nextlx.symbol:=eolsym
	return

when etx,0 then
	if sourcelevel then
		unstacksource()
	else
		nextlx.symbol:=eofsym
		--lxsptr
		return
	fi

else
	nextlx.symbol:=errorsym
!	nextlx.value:=c
	return

end doswitch

end

proc lxreadstring(int termchar)=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter
!
!NOTE: "(For this to work, \w is changed to \wl, as two characters are generated)"
!I don't get that, as the CR,LF replace the \ and w respectively. So I've removed
!the need to have \wl

static var [256]char psname
var ichar dest
var int c,d
var [8]char str

if termchar='"' then
	nextlx.symbol:=stringconstsym
!	nextlx.subcode:=tichar
else
	nextlx.symbol:=charconstsym
	nextlx.subcode:=tint
fi

if not prescanmode then
	dest:=cast(lxsptr)				!form string into same buffer
else								!put strings into local space
	dest:=&.psname					!must not overflow 255 characters
fi
nextlx.svalue:=dest

do
	switch c:=lxsptr++^
	when '\\' then			!escape char
		c:=lxsptr^
		if c>='A'  and c<='Z' then c+:=' ' fi
		++lxsptr
		switch c
		when 'a' then			!bell ('alert')
			c:=7
		when 'b' then			!backspace
			c:=8
		when 'c','r' then		!carriage return
				c:=cr
		when 'e' then			!end-of-text
			c:=26
		when 'f' then			!formfeed
			c:=12
		when 'l','n' then		!linefeed, or linux/c-style newline
			c:=lf
		when 's' then			!eScape
			c:=27
		when 't' then			!tab
			c:=9
!		when 'u' then			!reserved for unicode, like \x but with 4 hex digits
		when 'v' then			!vertical tab
			c:=11
		when 'w' then			!windows-style cr-lf
			dest++^:=cr
			c:=lf
		when 'x' then	!2-digit hex code follows
			c:=0
			to 2 do
				case d:=lxsptr++^
				when 'A','B','C','D','E','F' then
					c:=c*16+d-'A'+10
				when 'a','b','c','d','e','f' then
					c:=c*16+d-'a'+10
				when '0','1','2','3','4','5','6','7','8','9' then
					c:=c*16+d-'0'
				else
					lxerror("Bad \\x code")
				esac
			od
		when 'y' then			!CCI/SM backwards tab
			c:=16
		when 'z','0' then		!null (not fully supported in code)
			c:=0
		when '"','Q' then		!embedded double quote
			c:='"'
		when '\\' then
			c:='\\'
		when '\'' then			!embedded single quote
			c:='\''
		else
			str[1]:=c; str[2]:=0
!			println c,char(c),=nextlx.lineno
			lxerror_s("Unknown string escape: \\%s",&.str)
		end
	when '"','\'' then		!possible terminators
		if c=termchar then		!terminator char
			if lxsptr^=c then		!repeated, assume embedded term char
				++lxsptr
			else			!was end of string
				exit
			fi
		fi
	when cr,lf,etx,0 then
		lxerror("String not terminated")
	endswitch

	if not prescanmode then
		dest++^:=c
	else
		if dest-nextlx.svalue<(psname.len-5) then
			dest++^:=c
		fi
	fi
od
nextlx.length:=dest-nextlx.svalue
(nextlx.svalue+nextlx.length)^:=0
end

proc readnumber(int base)=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
var ref char pstart,dest
var int c
var ref char p

dest:=pstart:=lxsptr

if base=10 then
	doswitch c:=lxsptr++^
	when '0'..'9' then
		dest++^:=c
	when '_','\'','`' then
	else
		--lxsptr
		exit
	end doswitch
else
	dest:=scannumber(base)
	c:=lxsptr^
fi

switch c			!terminator character
when '.' then		!possible real number
	if (lxsptr+1)^<>'.' then

		readrealnumber(pstart,dest-pstart,base)
		return
	fi
when 'e','E' then
	if base<15 then
		readrealnumber(pstart,dest-pstart,base)
		return
	fi
when 'p','P' then
	if base>=15 then
		readrealnumber(pstart,dest-pstart,base)
		return
	fi
end switch

stringtonumber(pstart,dest-pstart,base)
end

proc readdecimalnumber=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
var ref char pstart,dest
var int c,n,base,suffix
var ref char p

dest:=pstart:=lxsptr
suffix:=0

doswitch c:=lxsptr++^
when '0'..'9' then
	dest++^:=c
when '_','\'','`' then
else
	--lxsptr
	exit
end doswitch

switch c			!terminator character
when '.' then		!possible real number
	if (lxsptr+1)^<>'.' then

		readrealnumber(pstart,dest-pstart,10)
		return
	fi
when 'e','E' then
	readrealnumber(pstart,dest-pstart,10)
	return
when 'b','B' then
	++lxsptr
	n:=dest-pstart
	p:=pstart
	to n do
		if p^<'0' or p^>'1' then
			lxerror("1101B: bad digit")
		fi
		++p
	od
	stringtonumber(pstart,n,2)
	return
when 'x','X' then				!eg. 12x5678 = base 12
	++lxsptr
	stringtodecimalnumber(pstart,dest-pstart)
	base:=nextlx.value
	if base>16 then lxerror("Number base over 16") fi
	readnumber(base)
	return
when 'l','L' then
	suffix:=c
	++lxsptr
when 'w','W' then
	suffix:=c
	++lxsptr

end switch

stringtodecimalnumber(pstart,dest-pstart,suffix)
end

proc readrealnumber(ichar intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, or is nil
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
var ref char fractstart,ss
var int fractlen,expon,i,c,n
var real basex,x
const maxrealdigits=500
var [maxrealdigits]char realstr
var [32]char str

fractstart:=nil
fractlen:=0
expon:=0
longsuffix:=0

if lxsptr^='.' then		!read
	fractstart:=++lxsptr
	fractlen:=scannumber(base)-fractstart
fi

case lxsptr^
when 'e','E' then
	if base<15 then
		++lxsptr
		expon:=readexponent(base)
	fi
when 'p','P' then
	if base>=15 then
		++lxsptr
		expon:=readexponent(base)
	fi
when 'l','L' then
	if longsuffix then lxerror("LL?") fi
	longsuffix:='L'
	++lxsptr

esac

if longsuffix='L' then
	ss:=pcm_alloc(intlen+fractlen+16)		!add ".", "e", exponent, 0 terminator
	memcpy(ss,intstart,intlen)
	memcpy(ss+intlen,".",1)
	memcpy(ss+intlen+1,fractstart,fractlen)
	memcpy(ss+intlen+fractlen+1,"e",1)
!	n:=sprintf(&.str,"%lld",expon)
	getstrint(expon,&.str)
	memcpy(ss+intlen+fractlen+2,&.str,strlen(&.str)+1)

	nextlx.symbol:=decimalconstsym
	nextlx.subcode:=tflexdecimal
	nextlx.svalue:=ss
	nextlx.length:=strlen(ss)
	return
fi

if intlen+fractlen>maxrealdigits then
	lxerror("Real too long")
fi
if intlen then
	memcpy(&realstr,intstart,intlen)
fi
if fractlen then
	memcpy(&realstr[1]+intlen,fractstart,fractlen)
fi

if base=10 then
	x:=readrealbest(intlen,fractlen,expon,&.realstr)
else
	basex:=base
	expon-:=fractlen
	x:=0.0
	for i:=1 to intlen+fractlen do		!digits already range-checked
		c:=realstr[i]
		if c>='0' and c<='9' then
			x:=x*basex+c-'0'
		elsif c>'a' then
			x:=x*basex+c-'a'+10
		else
			x:=x*basex+c-'A'+10
		fi
	od

	if expon>=0 then
		to expon do
			x*:=basex
		od
	else
		to -expon do
			x/:=basex
		od
	fi
fi

nextlx.symbol:=realconstsym
nextlx.subcode:=treal
nextlx.xvalue:=x
end

function readrealbest(int intlen,fractlen,expon, ichar realstr)real=
	var [32]char expstr

	(realstr+intlen+fractlen)^:=0
	expon-:=fractlen

!	sprintf(&.expstr,"e%lld",int32(expon))
	print @&.expstr,"e",,expon
	strcat(realstr,&.expstr)
	return strtod(realstr,nil)
end

function readexponent(int base)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
var ref char numstart,numend
var int expon,length,neg

neg:=0
case lxsptr^
when '+' then ++lxsptr
when '-' then ++lxsptr; neg:=1
esac

numstart:=lxsptr
length:=scannumber(base)-numstart

if length=0 then
	lxerror("Bad expon")
fi

stringtonumber(numstart, length, base)
return (neg|-nextlx.value|nextlx.value)
end

global proc printsymbol(ref lexrec lp)=
var lexrec l
l:=lp^

printf("%-18s",symbolnames[l.symbol])

case l.symbol
when rawnamesym then
	printstrn(l.svalue,l.length)
	print " (",,l.hashvalue,,")"
when namesym then
	printstrn(l.symptr^.name,l.symptr^.namelen)
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
	printstrn(l.svalue,l.length)
	print """"
when charconstsym then
	print "'"
	printstrn(l.svalue,l.length)
	print "'"
when decimalconstsym then
	printstrn(l.svalue,l.length)
	print "L"
when opsym,assignsym,addrsym,ptrsym,deepcopysym,rangesym then
	print jtagnames[l.subcode]
elsif l.subcode then
	print "#",l.subcode
end

println

end

proc stringtonumber(ichar s, int length, base)=
!convert decimal number s to an i64 value
!s contains only digits
!for hex, then a..f and A..F have been converted to '9'+1 to '9'+6
var int64 a
var word64 b
var int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do		!trim leading zeros
	++s
	--length
od

nextlx.symbol:=intconstsym

if length>maxnumlen[base] or \
		(length=maxnumlen[base] and strncmp(s,maxnumlist[base],length)>0) then
	if base<>16 then
		lxerror("longint const")

	else
		if length>32 or \
			(length=32 and strncmp(s,"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0) then
			lxerror("longint const")

		else						!greater than 64 bits, up to 128 bits

			if length=32 and strncmp(s,"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0 then
				nextlx.subcode:=tu128
			else
				nextlx.subcode:=ti128
			fi

			nextlx.qvalue:=stringtonumber128(s,length,16)
		fi
	fi
	return
fi

a:=0

if base<=10 then
	to length do
		a:=a*base+s++^-'0'
!		a:=a*10+s++^-'0'
	od
else
	to length do
		c:=s++^
		if c>='a' then
			a:=a*base+c-'a'+10
		elsif c>='A' then
			a:=a*base+c-'A'+10
		else
			a:=a*base+c-'0'
		fi
	od
fi

nextlx.value:=a

nextlx.subcode:=setinttype(a)
end

proc stringtodecimalnumber(ichar s, int length,suffix=0)=
var int64 a
var word64 b
var int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do		!trim leading zeros
	++s
	--length
od

nextlx.symbol:=intconstsym

if length>20 or \
		(length=20 and strncmp(s,"18446744073709551615",20)>0) or suffix then

	if length>39 or \
		(length=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
		if suffix='W' then
			lxerror("-W overflows 128 bits")
		fi
dolongint::
		nextlx.symbol:=decimalconstsym
		nextlx.subcode:=tflexdecimal
		nextlx.svalue:=pcm_copyheapstring(s)
		nextlx.length:=length
	else						!greater than 64 bits, up to 128 bits

		if suffix='L' then goto dolongint fi

		if (length=39 and strncmp(s,"170141183460469231731687303715884105727",39)>0) then
			nextlx.subcode:=tu128
		else
			nextlx.subcode:=ti128
		fi

		nextlx.qvalue:=stringtonumber128(s,length,10)
	fi
	return
fi

a:=0

to length do
	a:=a*10+s++^-'0'
od

nextlx.value:=a

nextlx.subcode:=setinttype(a)
end

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
! do maxnum support and such
var int i!,n
static var int n

for i to maxnumlist.len do
	maxnumlen[i]:=strlen(maxnumlist[i])
od

inithashtable()
end

global proc printstrn(ichar s, int length)=
if length then
	print length:"v",s:".*"
fi
end

function scannumber(int base)ref char=
!lxsptr is at possible first digit of number sequence
!scan digits until non-digit
!return pointer to next char after compacted sequence
!sequence can be updated in-place (to close gaps caused by separators)
!start of sequence will be at lxsptr
var ref char dest
var int c

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
when 'l','L' then
	longsuffix:='L'
	exit

else
	--lxsptr
	exit
end doswitch
return dest
end

proc readrawstring=
!positioned at " of F"
!read raw string
var ichar dest
var int c

nextlx.symbol:=stringconstsym
nextlx.svalue:=++lxsptr

dest:=lxsptr				!form string into same buffer

doswitch c:=lxsptr++^
when '"' then
	if lxsptr^='"' then		!repeated, assume embedded term char
		dest++^:='"'
		++lxsptr
	else			!was end of string
		(lxsptr-1)^:=0
		exit
	fi
when cr,lf,etx,0 then
	lxerror("Raw string not terminated")
	--lxsptr
	exit
else
	dest++^:=c
enddoswitch
nextlx.length:=dest-nextlx.svalue
end

function lookup:int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in nextlx.hashvalue
!return 1 (found) or 0 (not found)
!in either case, nextlx.symptr set to entry where name was found, or will be stored in
var int j, wrapped

j:=nextlx.hashvalue iand hstmask
nextlx.symptr:=&hashtable[j]
wrapped:=0

++NLOOKUPS

do
	case nextlx.symptr^.namelen
	when 0 then
		exit
	when nextlx.length then
		if memcmp(nextlx.symptr^.name,nextlx.svalue,nextlx.length)=0 then	!match
			return 1
		fi
	esac

++NCLASHES

	++nextlx.symptr
	if ++j>=hstsize then
		if wrapped then
			abortprogram("HASHTABLE FULL")
		fi
		wrapped:=1
		nextlx.symptr:=&hashtable[0]
		j:=0
	fi
od

!exit when not found; new name will go in entry pointed to by lxsymptr

nextlx.symptr^.name:=nextlx.svalue
nextlx.symptr^.namelen:=nextlx.length
nextlx.symptr^.symbol:=rawnamesym

return 0
end

global function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
var int c,hsum

if s^=0 then return 0 fi

hsum:=s++^

do
	c:=s++^
	exit when c=0
	hsum:=hsum<<4-hsum+c
od
return hsum<<5-hsum
end

proc inithashtable=
!populate hashtable with standard symbols
var int i
memset(&hashtable,0,hashtable.bytes)

for i:=1 to stnames.len do
!	nextlx.svalue:=stnames[i]
	nextlx.svalue:=pcm_copyheapstring(stnames[i])
	nextlx.length:=strlen(nextlx.svalue)
	nextlx.hashvalue:=gethashvaluez(nextlx.svalue)

	if lookup() then
		println stnames[i]
		abortprogram("Duplicate symbol table entry")
	fi

	nextlx.symptr^.symbol:=stsymbols[i]

	case stsymbols[i]
	when unitnamesym then
		nextlx.symptr^.index:=stsubcodes[i]
		nextlx.symptr^.subcode:=unitnamesym
		nextlx.symptr^.symbol:=rawnamesym		!masquerades as normal identifier
!	when registersym then
!		nextlx.symptr^.index:=0
!		nextlx.symptr^.subcode:=registersym
!		nextlx.symptr^.symbol:=rawnamesym		!masquerades as normal identifier
!	when asmopcodesym then
!		nextlx.symptr^.index:=stsubcodes[i]
!		nextlx.symptr^.subcode:=asmopcodesym
!		nextlx.symptr^.symbol:=rawnamesym		!masquerades as normal identifier
	else
		nextlx.symptr^.subcode:=stsubcodes[i]
	esac
od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=

!CPL "ADDRESERVEDWORD",NAME; OS_GETCH()
	nextlx.svalue:=pcm_copyheapstring(name)
	nextlx.length:=strlen(name)
	nextlx.hashvalue:=gethashvaluez(name)

	if lookup() then
CPL =NAME
		abortprogram("Dupl ASM symbol")
	fi

	nextlx.symptr^.symbol:=rawnamesym
	nextlx.symptr^.subcode:=symbol
	nextlx.symptr^.index:=subcode
	nextlx.symptr^.regsize:=regsize
end

function dolexdirective(int index)int=
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
var ref strec symptr
var ref char p
var ichar file
var int i,lastsymbol,cond,fileno
var [256]char str

case index
when strincludedir,binincludedir then
	lexreadtoken()
	if nextlx.symbol<>stringconstsym then
		if nextlx.symbol=rawnamesym and eqbytes(nextlx.svalue,"$filename",9) then
			file:=sourcefilepaths[nextlx.fileno]
		else
			lxerror("strincl: string expected")
		fi
	else
		file:=nextlx.svalue
	fi

	fileno:=getsupportfile(file)
	nextlx.svalue:=sourcefiletext[fileno]
	nextlx.length:=sourcefilesizes[fileno]

	nextlx.symbol:=(index=strincludedir|stringconstsym|astringconstsym)
	nextlx.subcode:='A'			!for use when an astring
	(nextlx.svalue+nextlx.length)^:=0			!sometimes .length is not used (eg. in newstringobj())
	return 1							!so get it right. Don't need the etx

when includedir then
	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
	file:=nextlx.svalue
	convlcstring(file)
	file:=addext(file,".m")		!add in extension if not present; assume same as source

	if fverbose then
		println "  Include:",file
	fi
	stacksourcefile(file)
	return 0

when emitcdir then
	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("emitc/not str") fi
	nextlx.symbol:=kemitcsym
	return 1

when cclibdir then
	lexreadtoken()
	if nextlx.symbol<>stringconstsym then lxerror("cclib/not str") fi
	if ncclibs>=maxcclibs then lxerror("Too many cc libs") fi
	cclibtable[++ncclibs]:=pcm_copyheapstring(nextlx.svalue)
	return 0

else
	cpl sourcedirnames[index]
	lxerror("Directive not implemented")
esac
return 0
END

proc lexreadline=
!read lex chars until eol
!returns with lxsptr pointing to what follows (crlf, etx etx)
!caller should remember lxsptr as start of text
!processing of next symbol deals with line counting

doswitch lxsptr^
when cr,lf then
	return
when etx,0 then
	--lxsptr
	return
else
	++lxsptr
enddoswitch
END

global proc startlex(ichar caption,int fileno)=
!s is an etx and 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it
!(This lex system doesn't deal with include files so there no nested sourcefiles.
!There are only macro expansions which are dealt with locally.)

lxsptr:=sourcefiletext[fileno]

!CPL "STARTLEX-----------"

nextlx.fileno:=fileno
nextlx.lineno:=1

nextlx.symbol:=semisym
nextlx.subcode:=0
end

global function convertzstring(ichar s, int length)ichar=
static var [300]char str

if length>str.len then
	abortprogram("convertzstr")
fi
memcpy(&.str,s,length)
str[length+1]:=0
return &.str
end

global function addnamestr(ichar name)ref strec=
var lexrec oldlx
var ref strec symptr

oldlx:=nextlx
nextlx.hashvalue:=gethashvaluez(name)

nextlx.length:=strlen(name)
nextlx.svalue:=pcm_alloc(nextlx.length+1)
memcpy(nextlx.svalue,name,nextlx.length+1)
lookup()
symptr:=nextlx.symptr

nextlx:=oldlx

return symptr
end

global function findname(ichar name)ref strec=
!find arbitrary name in st
!return strec of generic entry, or nil if not found
!should be used after all normal lex processing is done, as nextlx is overwritten

var lexrec oldlx
var ref strec symptr

nextlx.hashvalue:=gethashvaluez(name)
nextlx.length:=strlen(name)
nextlx.svalue:=name				!no need to copy, as now new entry is created

if lookup() then
	return nextlx.symptr
else
	return nil
fi
end

global proc PS1(ichar caption)=
!print "PS:",,caption,,":"
print caption,,":::"
printsymbol(&lx)
end

global proc PS2(ichar caption)=
print "	",,caption,,":##"
printsymbol(&nextlx)
end

global proc PS(ichar caption)=
PS1(caption)
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
!static int lastline=0
var int lineno,n,dir,namelen
var ref char p
var ref strec symptr

lx:=nextlx				!grab that already read basic token

lx.lineno:=int(lx.fileno)<<24+lx.lineno

reenter::

lexreadtoken()			!read new token for next time around

if lx.symbol=namesym then			!zero-terminate identifiers
	(lx.symptr^.name+lx.length)^:=0		!can only do so after next symbol is read
!note: there might be a problem if etx is next; answer is to use two etxs)
fi

!but... needs some processing first!
!CPL "LEX",NEXTLX.LENGTH:"V",NEXTLX.SVALUE:".*",SYMBOLNAMES[NEXTLX.SYMBOL], SYMBOLNAMES[NEXTLX.SUBCODE]

switch nextlx.symbol

when rawnamesym then
	if not lookup() then					!name not found
!CPL "RAW NOT FOUND",NEXTLX.LENGTH:"V",NEXTLX.SVALUE:".*"
!CPL =NEXTLX.SYMPTR,=NAMENAMES[NEXTLX.SYMPTR.NAMEID],SYMBOLNAMES[NEXTLX.SYMPTR.SYMBOL]
		nextlx.symbol:=namesym				!convert to actual identifier
		return
	fi

found::
!CPL "FOUND",NEXTLX.LENGTH:"V",NEXTLX.SVALUE:".*"
!CPL =NEXTLX.SYMPTR,=NAMENAMES[NEXTLX.SYMPTR.NAMEID],SYMBOLNAMES[NEXTLX.SYMPTR.SYMBOL]
	nextlx.symbol:=nextlx.symptr^.symbol			!convert to reserved word, type, op etc
	nextlx.subcode:=nextlx.symptr^.subcode

!deal with new set of symbols...
	switch nextlx.symbol

	when ksourcedirsym then
		if not dolexdirective(nextlx.subcode) then
			goto reenter
		fi
	when rawnamesym then					!might be user identifier (points to generic entry)

!ASSEMMODE:=1
!CPL "RAWNAME",SYMBOLNAMES[NEXTLX.SYMBOL], SYMBOLNAMES[NEXTLX.SUBCODE]

		if nextlx.subcode=unitnamesym and \
				(lx.symbol=intconstsym or lx.symbol=realconstsym) then
			case lx.symbol
			when intconstsym then
				if lx.subcode=ti128 or lx.subcode=tu128 then
					lxerror("No suffix on i128/u128")
				fi
				case nextlx.symptr^.index
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
				when thousand_unit then lx.value *:= 1 thousand
				when kilo_unit then lx.value *:= 1024
				when mega_unit then lx.value *:= 1048576
				when giga_unit then lx.value *:= (1048576*1024)
				else
					lxerror("Can't do this unit index")
				esac
				lx.subcode:=setinttype(lx.value)
			else
				lxerror("Unit suffix after float not implem")
			esac
			goto reenter
!		elsif nextlx.subcode=asmopcodesym and assemmode then
!			nextlx.symbol:=asmopcodesym
!			nextlx.subcode:=nextlx.symptr.index

		else
			nextlx.symbol:=namesym
			nextlx.svalue:=nextlx.symptr^.name
		fi
	when namesym then						!matches existing name
		lxerror("NEXT NAME!!!")

	when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,kforallsym,
			kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
			krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
			ktrysym,ktabledatasym,kassemsym,kifsym then

		if lx.symbol=kendsym then
			lx.subcode:=nextlx.symbol			!turn end if to single end/if token
			goto reenter
		fi
	when opsym then
		goto doopsym
	when sysconstsym then					!ST ENTRY LIMITED TO 16 bits signed
		case nextlx.subcode
		when con_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint
		when nil_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tref
		when pi_const then
			nextlx.symbol:=realconstsym
			nextlx.xvalue:=3.1415926535897932384626
!			nextlx.xvalue:=pi
			nextlx.subcode:=treal
		when tab_const then
			nextlx.symbol:=stringconstsym
!			nextlx.subcode:=tichar
			nextlx.svalue:="\t"
			nextlx.length:=1
		else
			lxerror("sysconst?")
		esac
	when machinetypesym then
		case nextlx.subcode
		when 'I','i' then nextlx.subcode:=(targetbits=32|ti32|ti64)
		when 'W','w' then nextlx.subcode:=(targetbits=32|tu32|tu64)
		esac
		nextlx.symbol:=stdtypesym
	end switch

when rawxnamesym then
	lookup()
!CPL "RAWX LOOKUP",NEXTLX.LENGTH:"V",NEXTLX.SVALUE:".*"
!CPL =NEXTLX.SYMPTR,=NAMENAMES[NEXTLX.SYMPTR.NAMEID],SYMBOLNAMES[NEXTLX.SYMPTR.SYMBOL]
	nextlx.symbol:=namesym				!convert to actual identifier
	return

when eolsym then

	switch lx.symbol
	when commasym, lsqsym, lbracksym, !ignore eol
		 assignsym,semisym then
		goto reenter
	when opsym then
		if not assemmode then
			goto reenter
		fi
		nextlx.symbol:=semisym
	else										!convert to semicolon
		nextlx.symbol:=semisym
	end switch

when stringconstsym then
	if lx.symbol=stringconstsym then
		n:=nextlx.length+lx.length
		p:=pcm_alloc(n+1)
		memcpy(p,lx.svalue,lx.length)
		memcpy(p+lx.length,nextlx.svalue,nextlx.length)
		(p+n)^:=0
		lx.svalue:=p
		lx.length:=n
		goto reenter
	fi


when opsym then
doopsym::
	if nextlx.subcode=j_in and lx.symbol=opsym and lx.subcode=j_notl then
		lx.subcode:=j_notin
		goto reenter
	fi
when eofsym then
endswitch

end

global proc showhashtablesize=
var int i,n

n:=0
for i:=0 to hstmask do
	if hashtable[i].name then
		++n
	fi
od
end

global function checkname(ichar name,int length=0)int=
!nextlx contains a rawnamesym
!return if if it is the same as name
!length is the name length, or 0 (-1 better for empty strings) to work it out
!note that nextlx.svalue is not zero-terminated

if length=0 then
	length:=strlen(name)
fi
if nextlx.length=length and memcmp(nextlx.svalue,name,length)=0 then
	return 1
fi
return 0
end

function getstrfile(ichar filename,int32 &length)ichar=
!locate module within search paths for strinclude
!return pointer to loaded/in-memory file (or nil on error)
	

var ichar file
static var [300]char filespec
var int i

for i:=nsearchdirs downto 1 do
	strcpy(&.filespec,searchdirs[i])
	strcat(&.filespec,filename)

	if checkfile(&.filespec) then
		file:=cast(readfile(&.filespec))
		length:=rfsize
		return file
	fi
od

return nil
end

proc stacksourcefile(ichar file,int ismainmodule=0)=
var int fileno
var ichar basefile,sptr,path

fileno:=getsupportfile(file)

stacksource(sourcefiletext[fileno],fileno,1)
end

proc stacksource(ichar sptr,int fileno,isfile)=
!introduce new source level for macros or include files
!not used for main source

if sourcelevel>=maxstackdepth then
	lxerror("Include file/macro overflow")
fi
++sourcelevel
lxstart_stack[sourcelevel]:=lxstart
lxsptr_stack[sourcelevel]:=lxsptr
lxfileno_stack[sourcelevel]:=nextlx.fileno
lxlineno_stack[sourcelevel]:=nextlx.lineno
isfile_stack[sourcelevel]:=isfile

lxstart:=lxsptr:=sptr
nextlx.lineno:=1
nextlx.fileno:=fileno
end

proc unstacksource=
if sourcelevel>0 then			!check that some source is stacked
	lxstart:=lxstart_stack[sourcelevel]
	lxsptr:=lxsptr_stack[sourcelevel]
	nextlx.lineno:=lxlineno_stack[sourcelevel]
	nextlx.fileno:=lxfileno_stack[sourcelevel]
	--sourcelevel
fi
end

proc readarraystring(int prefix)=
++lxsptr
lxreadstring('"')
nextlx.symbol:=astringconstsym
nextlx.subcode:=toupper(prefix)
end

proc qadd(ref qint aa,bb)=
!add bb to aa
var word64 low,high

low:=aa^.lower+bb^.lower
if aa^.lower>low then		!overflow
	++aa^.upper
fi

aa^.lower:=low
aa^.upper+:=bb^.upper
end

proc qadddigit(ref qint aa,int x)=
!add small positive int x to a; x can be zero
var word64 low,high

low:=aa^.lower+x
if aa^.lower>low then		!overflow
	++aa^.upper
fi

aa^.lower:=low
end

proc qshift(ref qint aa,int n)=
!show aa left by n bits
var word64 low,high,overflow

to n do
	overflow:=0
	if aa^.lower iand 0x8000'0000'0000'0000 then	!overflow
		overflow:=1
	fi
	aa^.lower<<:=1
	aa^.upper:=aa^.upper<<1+overflow

od
end

proc qmul10(ref qint aa)=
!multiply aa by 10
var word64 low,high
var qint bb

bb:=aa^
qshift(aa,2)		!*4
qadd(aa,&bb)		!*5
qshift(aa,1)		!*10
end

proc qmulbase(ref qint aa,int base)=
!multiply aa by 10
var word64 low,high
var qint bb
case base
when 16 then
	qshift(aa,4)
when 10 then
	qmul10(aa)
else
	bb:=aa^
	to base-1 do
		qadd(aa,&bb)
	od
esac
end

function stringtonumber128(ichar s, int length,base)ref qint=
var ref qint aa
var int c,d

aa:=pcm_allocz(qint.bytes)

to length do
	qmulbase(aa,base)

		c:=s++^

		if c>='a' then
			d:=c-'a'+10
		elsif c>='A' then
			d:=c-'A'+10
		else
			d:=c-'0'
		fi

	qadddigit(aa,d)
od

return aa
end

function setinttype(word64 a)int=
	if a<u64(0x7FFF'FFFF'FFFF'FFFF) then
		return ti64
	else
		return tu64
	fi
end

proc readrawxname=
	var int c,hsum

!	nextlx.subcode:=0

	nextlx.svalue:=lxsptr
	hsum:=0

	doswitch c:=lxsptr++^
	when 'A'..'Z','a'..'z','0'..'9','_','$' then
		hsum:=hsum<<4-hsum+c
	else
		--lxsptr
		exit
	end doswitch

	nextlx.symbol:=rawxnamesym
	nextlx.length:=lxsptr-nextlx.svalue
	nextlx.hashvalue:=hsum<<5-hsum

!CPL "READ RAWXNAME",NEXTLX.LENGTH,NEXTLX.LENGTH:"V", NEXTLX.SVALUE:".*",NEXTLX.HASHVALUE
	if nextlx.length=0 then
		lxerror("Bad ` name")
	fi

	return
end
=== mm_diags.m 13/35 ===
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lex
import mm_lib
import mm_mcldecls

var int currlineno



global proc printmodelist(filehandle f)=		!PRINTMODELIST
const wtypeno	= 4
const wname		= 13
const wbasetype	= 13
const wbitsize	= 3
const wtarget	= 12
const wnamedef	= 4
const wlower	= 5
const wupper	= 5
const wlength	= 6
!const wnallfields	= 4
const wsize		= 6
const wusercat	= 4
const wused		= 4
const wmode		= 32
var [256]char str
var ichar mstr
var strbuffer destv
var ref strbuffer dest := &destv
var int m

println @f,"MODELIST",ntypes,nuserxtypes

gs_init(dest)

gs_leftstr(dest,"#",wtypeno)
gs_leftstr(dest,"Name",wname)
gs_leftstr(dest,"Base",wbasetype)
gs_leftstr(dest,"Bit",wbitsize)
gs_leftstr(dest,"Target",wtarget)
gs_leftstr(dest,"Def",wnamedef)
gs_leftstr(dest,"Lwb",wlower)
gs_leftstr(dest,"Upb",wupper)
gs_leftstr(dest,"Len",wlength)
!gs_leftstr(dest,"AF",wnallfields)
gs_leftstr(dest,"Size",wsize)
gs_leftstr(dest,"Cat",wusercat)
gs_leftstr(dest,"Used",wused)
gs_leftstr(dest,"Mode",wmode)
gs_println(dest,f)

for m:=0 to ntypes do
	gs_init(dest)
	gs_leftint(dest,m,wtypeno)
	gs_leftstr(dest,typename(m),wname)
	gs_leftstr(dest,typename(ttbasetype[m]),wbasetype)
	gs_leftint(dest,ttbitwidth[m],wbitsize)

	if tttarget[m] then
		gs_leftstr(dest,typename(tttarget[m]),wtarget)
	else
		gs_leftstr(dest,"-",wtarget)
	fi
	if ttnamedef[m] then
		gs_leftstr(dest,"+",wnamedef)
	else
		gs_leftstr(dest,"-",wnamedef)
	fi

	case ttbasetype[m]
	when tfixedarray,tfixedrecord,tenum,tfixedbits,tfixedset then
		gs_leftint(dest,ttlower[m],wlower)
		gs_leftint(dest,ttlower[m]+ttlength[m]-1,wlower)
		gs_leftint(dest,ttlength[m],wlength)
	else
		gs_leftstr(dest,"",wlower)
		gs_leftstr(dest,"",wlower)
		gs_leftstr(dest,"",wlength)
	esac

	gs_leftint(dest,ttsize[m],wsize)
	gs_leftint(dest,ttusercat[m],wusercat)

	mstr:=strmode(m)
	if strlen(mstr)<16 then
		gs_str(dest,mstr)
	else
		gs_println(dest,f)
		gs_init(dest)
		gs_str(dest,mstr)
	fi
	gs_println(dest,f)
od

println @f

println @f,"USERXTYPES:",NUSERXTYPES
for m:=1 to nuserxtypes do
	gs_init(dest)
	gs_strint(dest,m)
	gs_str(dest,": ")
!	gs_strint(dest,int(ttnamedef[-m]))
	gs_str(dest,ttnamedefx[m]^.name)
	if ttnamedefx2[m] then
		gs_str(dest,".")
		gs_str(dest,ttnamedefx2[m]^.name)
	fi
	gs_str(dest," ")
	gs_strint(dest,ttxmap[m])

	gs_println(dest,f)
od

end

global proc printst(filehandle f,ref strec p,int level=0)=	!PRINTST
var ref strec q

if p^.symbol<>namesym then
	mcerror("PRINTST not name")
fi

printstrec(f,p,level)

q:=p^.deflist

while q<>nil do
	printst(f,q,level+1)
	q:=q^.nextdef
od
end

proc printstrec(filehandle f,ref strec p,int level)=		!PRINTSTREC
var strec dd
var ref byte q
var strbuffer v
var ref strbuffer d:=&v
var int col,offset,n
const tabstr="    "
var [256]char str

!CPL "PRINTSTREC",P^.NAME

gs_init(d)


offset:=0
to level do
	gs_str(d,tabstr)
	offset+:=4
od
gs_str(d,":")

gs_leftstr(d,p^.name,28-offset,'-')
gs_leftstr(d,namenames[p^.nameid],12,'.')

col:=gs_getcol(d)
dd:=p^


gs_str(d,"[")
if p^.imported then
	gs_str(d,(p^.imported=2|"Imp/CLIB "|"Imp "))
else
	gs_str(d,(p^.isglobal|"Glob ","Exp "|"Loc "))
fi

if dd.isstatic then
	gs_str(d,"Stat")
fi
if dd.fflang then
	gs_strsp(d,fflangnames[dd.fflang])
fi
if dd.parammode then
	gs_str(d,parammodenames[dd.parammode])
!	gs_str(d," ")
fi
if dd.align then
	gs_str(d,"@@")
	gs_strint(d,dd.align)
	gs_str(d," ")
fi
if dd.optional then
	gs_str(d,"Opt ")
fi
if dd.varparams then
	gs_str(d,"Var ")
fi
if dd.moduleno then
!	sprintf(&.str,"Modno#%lld ",dd.moduleno)
	print @&.str,"Modno#",,dd.moduleno
	gs_str(d,&.str)
fi
if dd.equals then
	gs_str(d,":= ")
fi

if dd.used then
	gs_str(d,"U ")
fi

gs_str(d,"]")
gs_padto(d,col+10,'=')

if p^.owner then
!	sprintf(&.str,"(%s)",p^.owner^.name)
	fprint @&.str,"(#)",p.owner.name
	gs_leftstr(d,&.str,18,'-')
else
	gs_leftstr(d,"()",18,'-')
fi

case p^.mode
when tvoid then
	gs_str(d,"Void ")
else
	GS_STRINT(D,P^.MODE)
	GS_STR(D,":")

	n:=p^.nretvalues
	if n=0 then n:=1 fi
	if n>1 then gs_str(d,"(") fi

	for i to n do
		gs_str(d,strmode(p^.modelist[i]))
		if i<n then
			gs_str(d,",")
		fi
	od
	if n>1 then gs_str(d,")") fi
	gs_str(d," ")
esac

case p^.nameid
when fieldid,paramid then
	gs_str(d," Offset:")
	gs_strint(d,p^.offset)
	if p^.mode=tbitfield then
		gs_str(d," Bitoffset:")
		gs_strint(d,p^.bitoffset)
		gs_str(d,":")
		gs_strint(d,p^.bitfieldwidth)
	fi


!	sprintf(&.str,"%.*s",int(p^.uflags.ulength),&p^.uflags.codes)
	print @&.str,p^.uflags.ulength:"v",ichar(&p^.uflags.codes):".*"
	gs_str(d," UFLAGS:")
	gs_str(d,&.str)
	gs_str(d,"-")
	gs_strint(d,p^.uflags.ulength)

	if p^.code then
		gs_str(d,":=")
		gs_strvar(d,strexpr(p^.code))
	fi

when genfieldid then
	gs_str(d,"Index:")
	gs_strint(d,p^.offset)

when procid then

	gs_str(d,"Index:")
	gs_strint(d,p^.index)

	gs_str(d," Nret:")
	gs_strint(d,p^.nretvalues)

	gs_str(d," Simple:")
	gs_strint(d,p^.simplefunc)

when dllprocid then
	gs_str(d,"Index/PCaddr:")
	gs_strint(d,p^.index)
	if p^.truename then
		gs_str(d," Truename:")
		gs_str(d,p^.truename)
	fi

when staticid then
	if p^.code then
		gs_str(d,"=")
		gs_strvar(d,strexpr(p^.code))
	fi

when frameid then
	if p^.code then
		gs_str(d,":=")
		gs_strvar(d,strexpr(p^.code))
	fi

when constid then
	gs_str(d,"Const:")
	gs_strvar(d,strexpr(p^.code))

when typeid then
	if p^.base_class then
		gs_str(d,"Baseclass:")
!		gs_str(d,ttname[p^.base_class])
		gs_str(d,typename(p^.base_class))
	fi
when enumid then
	gs_str(d,"Enum:")
	gs_strint(d,p^.index)
when dllmoduleid then
	gs_str(d,"DLL#:")
	gs_strint(d,p^.dllindex)
esac

case p^.at
when 2 then
	gs_str(d," @")
	gs_str(d,p^.equivfield^.name)
when 1 then
	gs_str(d," @")
	gs_strvar(d,strexpr(p^.equivvar))
esac

gs_str(d," Module# ")
gs_strint(d,p^.moduleno)

gs_str(d," Lineno:")
gs_strint(d,p^.lineno iand 16777215)

gs_println(d,f)

case p^.nameid
when constid,frameid,staticid,macroid then
	if p^.code then
		printunit(p^.code,dev:f)
	fi
esac


end

global proc printstflat(filehandle f)=
var int i
var ref strec p
var ref lexrec lx
println @f,"GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
!cpl i
!	if hashtable[i].name and hashtable[i].symbol=namesym then
	p:=&hashtable[i]
	if p^.name then
		case p^.symbol
		when rawnamesym then
			println @f,i,p,":",p^.name,symbolnames[p^.symbol],namenames[p^.nameid]
!			if p^.symbol=lexmacronamesym then
!!				lx:=p^.macrotoken
!				println @f,"			",p^.macrovalue
!			fi
			p:=p^.nextdupl
			while p do
				println @f,"	",p,p^.name,symbolnames[p^.symbol],namenames[p^.nameid],
					"(From",(p^.owner|p^.owner^.name|"-"),,")"
!					p^.prevdupl,"(From",(p^.owner|p^.owner^.name|"-"),,")"
				p:=p^.nextdupl
			od
		esac
	fi
od
end

global proc printcode(filehandle f,ichar caption)=
var int i
var ref strec p

!p:=moduletable[n].stmodule^.deflist
p:=stprogram^.deflist

println @f, caption, "PROGRAM"

while p do
	printmodulecode(f,p)
	p:=p^.nextdef
od
end

global proc printmodulecode(filehandle f,ref strec m)=
var int i
var ref strec p

p:=m^.deflist
currmodule:=m

println @f,"MODULE:",m^.name,namenames[m^.nameid]

while p do
	case p^.nameid
	when procid then
		if not p^.imported then
			println @f,p^.name,,"=",(p^.isglobal|"Global","Export"|"Local")
			printunit(p^.code,,"1",dev:f)
			println @f
		fi
	esac
	p:=p^.nextdef
od
end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
!p is a tagrec
var ref unitrec q
var ref strec d
var int t
var ichar idname
var int64 a
var real32 x32

if p=nil then
	return
fi

if p^.lineno then
	currlineno:=p^.lineno iand 16777215
fi

!print @dev,p^.popflag,,":"

print @dev,p,":"
print @dev,getprefix(level,prefix,p)

idname:=jtagnames[p^.tag]+2
print @dev,idname,,": "

case p^.tag
when j_name then
	d:=p^.def

	print @dev,d^.name,namenames[d^.nameid]

	if d^.code then
		print @dev," {",,jtagnames[d^.code^.tag],,"}"
	fi

	print @dev," ",,getdottedname(d)!,q
	print @dev,(p^.dottedname|" {Dotted}"|"")

	if p^.c then
		print @dev," Lastcall:",p^.c
	fi

	if p^.addroffirst then
		print @dev," Addroffirst."
	fi

	print @dev," Moduleno:",p^.moduleno

when j_labeldef then
	println @dev,p^.def^.name

when j_const,j_emitc then
	t:=p^.mode
	a:=p^.value
	if t=trefchar then
		if p^.slength>256 then
			print @dev,"""",,"(LONGSTR)",""" *",,p^.slength
		elsif p^.slength then
			print @dev,"""",,p^.svalue,,""" *",,p^.slength
		else
			print @dev,""""""
		fi

	elsecase ttbasetype[t]
	when ti64,ti32,ti16,ti8 then print @dev,int64(a)
	when tu64,tu32,tu16,tu8 then print @dev,word64(a)
	when tc64,tc8,tc16 then print @dev,chr(a)

!	when ti32,ti64,ti8,ti16 then
!		print @dev,p^.value
!	when tu32,tu64,tu8,tu16 then
!		print @dev,p^.uvalue
	when tr32 then
		x32:=p^.xvalue
		print @dev,real64(x32)
	when tr64 then
		print @dev,p^.xvalue
	when tref then
		if p^.value then
			print @dev,"#",,p^.value,P^.SLENGTH
		else
			print @dev,"NIL"
		fi
	when ti128,tu128 then
		print @dev,strqvalue(p^.qvalue)
	else
		case ttbasetype[t]
		when trange64 then
			print @dev,p^.qvalue^.lower,,"..",,p^.qvalue^.upper
		else
			cpl =typename(t),typename(ttbasetype[t])
PRINT @DEV,"<PRINTUNIT BAD CONST PROBABLY VOID"
!!			CPL("PRINTUNIT BAD CONST")
!			serror("PRINTUNIT BAD CONST")
		esac
	fi
	print @dev," ",,typename(t)
!	if p^.constdef then
!		print @dev," ",,p^.constdef^.name
!	fi
	if p^.isastring then
		print @dev," <isstr>"
	fi

	if p^.whenlabel then
		print @dev," *L",,p^.whenlabel
	fi

when j_decimal then
	print @dev,p^.svalue,"Len:",p^.slength

when j_typeconst then
	print @dev,typename(p^.mode),typename(p^.value)

when j_operator then
	print @dev,jtagnames[p^.opcode]+2

when j_bitfield then
	print @dev,bitfieldnames[p^.opcode]+3

when j_convert,j_typepun then
	print @dev,convnames[p^.opcode]," to:",strmode(p^.newmode)

when j_makelist,j_multexpr then
	print @dev,"Len:",p^.length

when j_dot then
	print @dev,"Offset:",p^.offset

when j_index, j_ptr then

when j_exit,j_redo,j_restart,j_next then
	print @dev,"#",,p^.index

when j_syscall then
	print @dev,sysfnnames[p.opcode]+6

when j_assem then
	print @dev,mclnames[p.index]+2
	if p.index in [m_jmpcc, m_setcc, m_cmovcc] then
		print @dev," ",condnames[p.cond],=P.COND
	fi

when j_assemreg then
	print @dev,regnames[p.reg],"size:",p.regsize

when j_assemxreg then
	print @dev,xmmregnames[p.reg]

when j_assemmem then
	var ichar plus
	plus:=""
	if p.prefixmode then print @dev,strmode(p.prefixmode) fi
	print @dev,"["
	if p.reg then 
		print @dev,regnames[p.reg]
		plus:="+"
	fi
	if p.regix then 
		print @dev,plus,,regnames[p.regix]
!		plus:="+"
	fi
	if p.scale>1 then
		print @dev,"*",,p.scale
	fi
	print @dev,"]"

esac

if p^.isconst then
	print @dev," Is const"
fi

println @dev

printunitlist(dev,p^.a,level+1,"1")
printunitlist(dev,p^.b,level+1,"2")
if p^.tag<>j_const then				!.c used for extra info
	printunitlist(dev,p^.c,level+1,"3")
fi

end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
if p=nil then return fi

while p do
	printunit(p,level,prefix,dev)
	p:=p^.nextunit
od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
static var [1024]char str
var [1024]char indentstr
var [16384]char modestr
var ichar isexpr

indentstr[1]:=0
if level>10 then level:=10 fi

to level do
	strcat(&.indentstr,"- ")
od

!sprintf(&.modestr,"%s",strmode(p^.mode))

isexpr:="S"
if jisexpr[p.tag] then isexpr:="E" fi

case p.tag
when j_if, j_switch, j_case, j_select then
	if p.mode=tvoid then
		isexpr:="S"
	fi
esac

!sprintf(&.modestr,"%s %s:%s",isexpr,(p^.popflag|"POP"|"---"),strmode(p^.mode))
fprint @&.modestr,"# #:#",isexpr,(p^.popflag|"POP"|"---"),strmode(p^.mode)
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
static var [40]char str

!sprintf(&.str,"%04d ",currlineno)
print @&.str,currlineno:"z4",," "
return &.str
end

=== mm_genwx64.m 14/35 ===
!Codegen for WX64 - x64 for Windows ABI

import mlib
import clib
import oslib

import mm_decls
import mm_lib

import mm_genpcl
import mm_libpcl
import mm_blockpcl

import mm_genmcl
import mm_libmcl
import mm_lex
import mm_tables

import ma_genss
import ma_writeobj
import ma_writeexe

import mm_mcldecls
!import mm_genasm
!import mm_assembler

global tabledata []ichar stdlibnames, []ichar stdlibtext =
	("msysnew.m",	strinclude "msysnew.m"),
	("mlib.m",		strinclude "mlib.m"),
	("clibnew.m",	strinclude "clibnew.m"),
	("oswindows.m",	strinclude "oswindows.m"),
	("oswindll.m",	strinclude "oswindll.m"),
end

global proc do_codegen_debug=
	var ref strbuffer ss

	if passlevel>=4 then
		codegen_pcl()
		if fshowpcl1 then showpcl("PCL") fi
	fi

	if passlevel>=5 then
		codegen_mcl()
!		if fshowmcl1 then showmcl("MCL.ASM") fi
		if fshowmcl1 then showmcl(outfilesource) fi
	fi

	if passlevel>=6 then
		genss()
		initsectiontable()
		if fshowss then
			showss("SS",0)
		fi
!CPL =OUTFILEBIN,=linkOPTION
		if eqstring(linkoption,"obj") then
			if fshowss then
				showss("SS",0)
			fi
			writess(outfilebin)
		else
			genexe(nil)
			if fshowss then
				showss("SS",1)
			fi
			writeexe(outfilebin)
		fi

	fi
end

global proc do_codegen=
	codegen_pcl()
	codegen_mcl()

	if cc_mode=link_mode then
		genss()
		initsectiontable()
		if eqstring(linkoption,"obj") then
			writess(outfilebin)
		else
			genexe(nil)
			writeexe(outfilebin)
		fi
	else
		showmcl(outfilesource)
	fi
end

global proc showpcl(ichar filename)=
	var ref strbuffer pclstr

	gs_init(dest)
	pclstr:=writepclcode(filename)

	writegsfile(filename,pclstr)
end

global proc showmcl(ichar filename)=
	var ref strbuffer mclstr

	gs_init(dest)
	mclstr:=writemclcode(filename)

	writegsfile(filename,mclstr)
end

global proc showss(ichar filename,int fexe)=
	var ref strbuffer ssstr

	gs_init(dest)
	ssstr:=writessdata(fexe)
	writegsfile(filename,ssstr)
end

proc writegsfile(ichar filename, ref strbuffer d)=
	var filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

global proc showhelp=
	static var ichar helptext=strinclude "mm_help.txt"
	println helptext
end

global proc initassemsymbols=
!initialise hash table from kwddata
var [32]char str
var int i

!CPL "INITASSEM"

for i to mclnames.len do
	addreservedword(mclnames[i]+2,asmopcodesym,i)
od

for i to dregnames.len do
	addreservedword(dregnames[i],regsym,regindices[i],regsizes[i])
od


for i to xmmregnames.len do
	addreservedword(xmmregnames[i],xregsym,i)
od

for i to fregnames.len do
	addreservedword(fregnames[i],fregsym,i)
od

for i to mregnames.len do
	addreservedword(mregnames[i],mregsym,i)
od

for i to jmpccnames.len do
	addreservedword(jmpccnames[i],jmpccsym,jmpcccodes[i])
od

for i to setccnames.len do
	addreservedword(setccnames[i],setccsym,setcccodes[i])
od

for i to cmovccnames.len do
	addreservedword(cmovccnames[i],movccsym,cmovcccodes[i])
od

!for i to segmentnames.len do
for i to segmentnames.upb do
	strcpy(&.str,segmentnames[i])
	str[strlen(&.str)-3]:=0
	addreservedword(pcm_copyheapstring(&.str),segnamesym,i)
od

addreservedword("aframe",regsym,r14,4)
addreservedword("dframe",regsym,r14,8)
addreservedword("astack",regsym,r15,4)
addreservedword("dstack",regsym,r15,8)
addreservedword("dprog",regsym,r8,8)
addreservedword("dsptr",regsym,r9,8)
end
=== mm_genpcl.m 15/35 ===
import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_tables
import mm_lib
import mm_libpcl
import mm_blockpcl
import mm_diags

!const entrypointname = "main"
const entrypointname = "start"
const entrypointname2 = "main"

global function codegen_pcl:int=
!generate code for module n
var ref strec d,e
var ref procrec pp

pclinit()

pp:=staticlist
while pp do
	d:=pp^.def
	dostaticvar(d)
	pp:=pp^.nextproc
od

genpc(k_blank)
genpc(k_csegment)

geninitproc()

pp:=proclist
while pp do
	genprocdef(currproc:=pp^.def)
	pp:=pp^.nextproc
od

allpclcode:=pccode

return 1
end

proc genprocdef (ref strec p) =	!GENPROCDEF
var [256]char str
var int paramoffset,nparams,retaddrbytes
var ref strec d
var int n,lab,np,offset,reg,i,xreg,isstart,structret,isfloat,hasparams,hasequiv
var unit q

!*!setalign(16)

!sprintf(&.str,"proc %s ----------------",p^.name)
fprint @&.str,"proc # ----------------",p.name
gencomment(&.str)

currproc:=p

!do local decls
frameoffset:=0
hasparams:=0
hasequiv:=0

d:=p^.deflist
while d do
	case d^.nameid
	when frameid then
		if d^.at then
			hasequiv:=1
		else
			frameoffset-:=roundsizetg(ttsize[d^.mode])
			d^.offset:=frameoffset
		fi
	when paramid then
		hasparams:=1
	esac
	d:=d^.nextdef
od

if hasequiv then
	d:=p^.deflist
	while d do
		case d^.nameid
		when frameid then
			if d^.at then
				d^.offset:=getconstframeoffset(d^.equivvar)
			fi
		esac
		d:=d^.nextdef
	od
fi
nparams:=0

d:=p^.deflist
retaddrbytes:=(frameoffset or hasparams|16|8)
paramoffset:=0

while d do
	if d^.nameid=paramid then
		d^.offset:=paramoffset+retaddrbytes
		if ttsize[d^.mode]=16 then
			paramoffset+:=16
		else
			paramoffset+:=8
		fi
		++nparams
	fi
	d:=d^.nextdef
od

framebytes:=-frameoffset
parambytes:=paramoffset
iscallbackproc:=iscallbackfn(p)

n:=targetsize-1					!round to 4 or 8
while framebytes iand n do ++framebytes; --frameoffset od
while parambytes iand n do ++parambytes od

!setsegment('C')
isstart:=0
if p.isglobal and (eqstring(p^.name,entrypointname) or
					 eqstring(p.name,entrypointname2)) then
	isstart:=1
	print @&.str,p.name,,"::"
!	genpc(k_userlabel,genstrimm(&.str))
	genpc(k_labelname,genstrimm(&.str))
!	genpc(k_labeldef,genstrimm("ABC"))
!	genpc(k_labelname,genname(&.str))
!	genpc(k_label, genlabel(++labelno))
	p.index:=labelno
fi

!if p.isglobal and eqstring(p^.name,entrypointname2) then
!	print @&.str,p.name,,"::"
!	genpc(k_labelname,genname(&.str))
!fi

genprocentry(framebytes,parambytes,isstart)

if p^.asmused then		!declare local vars as named offsets
	d:=p^.deflist
	while d do
		case d^.nameid
		when frameid,paramid then
			genframedef(d)
		esac
		d:=d^.nextdef
	od
fi

if isstart then
	do_syscallproc(sysfn_init,0)
!	do_syscallproc(sysfn_initstatics,0)
!CPL =INITSTATICSINDEX
!GENCOMMENT("88888888888888888888888888888888")

!	genjumpl(initstaticsindex)
!	genpc(k_jump, genlabel(initstaticsindex))
	genpc(k_call, genlabel(initstaticsindex))
!	genpc(k_call, genlabel(initstaticsindex))
!do_syscallproc(sysfn_initstatics,0)
fi

d:=p^.deflist
while d do
	if d^.nameid=frameid and gettypecat_t(d^.mode) in [tflex,tvariant] then
		initframedef(d)
	fi
	d:=d^.nextdef
od

retindex:=lab:=createfwdlabel()

gencomment("-------------------------------------------------")

GENCOMMENT("EXECUTABLE CODE GOES HERE")
evalunit(p^.code)

if p^.mode<>tvoid and p^.nretvalues<2 then

	isfloat:=ttisreal[p^.mode]
	if isfloat then
		makefloatopnds()
	fi

	if currproc^.simplefunc then
		genpc(k_moveretval)
	else
		genpc(k_popretval,genint(parambytes))
	fi
	setpclcat_u(p^.code)
	if isfloat then
		makefloatopnds()
	fi
fi

definefwdlabel(retindex)
gencomment("-------------------------------------------------")

d:=p^.deflist
while d do
	if d^.nameid in [frameid,paramid] and gettypecat_t(d^.mode) in [tflex,tvariant] then
		freeframevar(d)
	fi
	d:=d^.nextdef
od

if isstart then
!	pushstack(40)					!stack misaligned here
	pushstack(32)
	genpc(k_pushimm, zero_opnd);
!	do_syscallproc(stop_fn)
	do_syscallproc(sysfn_stop,1)
!	genpc(k_call, genname("msys.m$stop"))
else
	genreturn(framebytes,parambytes)
fi

if p^.mode<>tvoid then
	if not checkblockreturn(p^.code) then
		gerror_s("Function needs explicit return: ",p^.name)
	fi
fi

gencomment("")

end

proc genprocentry(int fbytes,pbytes,isstart)=
!proc entry code
	genpc(k_procentry,genmemaddr_d(currproc),genint(pbytes))
	pccodex^.b.fbytes:=fbytes
	pccodex^.isglobal:=isstart
end

proc genframedef(ref strec d)=
!if not fshowframevars then return fi
var [256]char str
var int offset

GENCOMMENT("GENFRAMEDEF")
!offset:=d^.offset
!
!if abs(offset)<1000 then
!!	sprintf(&.str,"\t%-20s = %3d",getfullname(d,1),offset)
!	fprint @&.str,"\t# = #",getfullname(d,1):"jr20",offset:"3"
!else
!!	sprintf(&.str,"\t%-20s = %d",getfullname(d,1),offset)
!	fprint @&.str,"\t# = #",getfullname(d,1):"jr20",offset
!fi
!genassem(&.str)
end

proc dostaticvar(ref strec d)=
	var unit p

	if d^.imported then return fi

	if d^.at=1 then
		p:=d^.equivvar

		case p^.tag
		when j_addrof then
			genpc(k_equiv,genmem_d(d),genmem_u(p^.a))
		when j_const then
			genpc(k_equiv,genmem_d(d),genint(p^.value))
		else
			printunit(p)
			gerror("equiv/not simple")
		esac
	elsif d^.code then
		if stdtypecat[ttbasetype[d^.mode]] not in [tflex,tvariant] then
			genpc(k_istatic,genmem_d(d))
			pccodex^.align:=getalignment(d^.mode)
			genidata(d^.code)
		else
			goto dozstatic
		fi
	else
dozstatic::
		genpc(k_zstatic,genmem_d(d),genint(ttsize[d^.mode]))
		pccodex^.align:=getalignment(d^.mode)
	fi
end

proc dostaticvarflex(ref strec d)=
	var unit p

!	if d^.imported or d^.at=1 or not d^.code then return fi
	if d^.imported or d^.at=1 then return fi

	if stdtypecat[ttbasetype[d^.mode]] not in [tflex,tvariant] then
		return
	fi

!initialise flex or variant static
	if d^.code then				!have an expression to assign
		evalunit(d^.code)
	else
		genpc(k_pushretslot)
		pccodex^.catmode:=tscalar
		callflexhandler(d^.mode,"new",0)
!		genpc(k_initmemz,genmem_d(d))
	fi
	genpc(k_popmem,genmem_d(d))
	pccodex^.catmode:=tscalar
!	setpclcat_t(d^.mode)
end

proc genidata(unit p,int doterm=1, am='A')=
var int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion,tbase
var unit q,a,b
var ref strec d
var real32 sx
var ref pclopndrec ax

t:=p^.mode
mlineno:=p^.lineno
tbase:=ttbasetype[t]

case p^.tag
when j_const then
	case tttypecode[p^.mode]
	when 'P' then
		if p^.mode=trefchar then
			if p^.svalue then
				genpc(k_dq,genstrimm(p^.svalue,p^.slength))
			else
				genpc(k_dq, genint(0))
			fi
		else
			genpc(k_dq,genint(p^.value))
		fi
	when 'R' then
		case ttsize[p^.mode]
		when 4 then
			genpc(k_dd,genreal(p^.xvalue,4))
		when 8 then
			genpc(k_dq,genreal(p^.xvalue))
		else
			gerror_s("IDATA/REAL:%s",strmode(p^.mode),p)
		esac

	else						!assume int/word
		ax:=genint(p^.value)
		case ttsize[p^.mode]
		when 1 then
			genpc(k_db,ax)
		when 2 then
			genpc(k_dw,ax)
		when 4 then
			genpc(k_dd,ax)
		when 8 then
			genpc(k_dq,ax)
		when 16 then
			genpc(k_dq,genint(p^.qvalue^.lower))
			genpc(k_dq,genint(p^.qvalue^.upper))
		else
			gerror_s("IDATA/INT:%s",strmode(p^.mode),p)
		esac

	esac

when j_makelist then
	q:=p^.a
	while q do
		genidata(q)
		q:=q^.nextunit
	od

when j_name then
	d:=p^.def
	case d^.nameid
	when staticid,procid,dllprocid then
		genpc((am='P' or ttsize[p^.mode]=8|k_dq|k_dd), genmemaddr_d(d))
	else
		gerror("Idata &frameXXX")
	esac	
	return
when j_convert then
	genidata(p^.a)
when j_addrof then
	genidata(p^.a,am:'P')
else
	gerror_s("IDATA: %s",jtagnames[p^.tag],p)

esac
end

proc geninitproc=
var ref procrec pp
var ref strec d

gencomment("proc ---------------- m$initstatics")

!genpc(k_procentry,genname("m$initstatics:"),genint(0))
!initstaticsindex:=sysfnlabels[sysfn_initstatics]:=definelabel()
initstaticsindex:=definelabel()


!genpc(k_procentry,genname("m$initstatics:"),genint(0))
pccodex^.b.fbytes:=0
pccodex^.isglobal:=1

GENCOMMENT("INIT CODE GOES HERE")
pp:=staticlist
while pp do
	d:=pp^.def
	dostaticvarflex(d)
	pp:=pp^.nextproc
od
gencomment("-------------------------------------------------")

for i:=nmodules downto 1 do
	d:=moduletable[i].stinitproc
	if d then
		genpc(k_call,genmemaddr_d(d),genint(0))
	fi
od

!genpc(k_procexit)
genpc(k_return)
gencomment("")
end

proc initframedef(ref strec d)=
!d is a flex/var type that must be initialised
!can't use any actual initial value, as that might not be done until later
!in the code

genpc(k_initmemz,genmem_d(d))
setpclcat_t(d^.mode)

end

proc freeframevar(ref strec d)=
!d is a flex/var type that must be freed

genpc(k_freemem,genmem_d(d))
setpclcat_t(d^.mode)

end

function getconstframeoffset(unit p)int=
!p is the @ equiv expression
!extract offset if possible
var unit a
var ref strec d
var int offset

a:=p^.a
d:=nil
offset:=0

!CPL =JTAGNAMES[P^.TAG]

case p^.tag
when j_addrof,j_addroffirst then
	if a^.tag=j_name then
		d:=a^.def
	fi
when j_add then
	if a^.tag=j_addroffirst and a^.a^.tag=j_name and p^.b^.tag=j_const then
		d:=a^.a^.def
		offset:=p^.b^.value
	fi
esac

if d=nil then
PRINTUNIT(P)
	gerror("Can't do @Frame var")
fi
if d^.nameid not in [frameid,paramid] then
	gerror("@ local static")
fi

return d^.offset+offset
end
=== mm_libpcl.m 16/35 ===
import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_tables
import mm_lib
!import mc_common

global var int retindex
global var int initstaticsindex

global const ptrsize=8

global record pclopndrec =
	union
		var ref strec def
		var unit code
		var int value
		var word uvalue
		var real xvalue
		var ichar svalue
		var int labelno
		var int pbytes
		var int floatmap
	end
	var int16 optype
	var int16 size			!for real: 4 or 8 bytes
	union
!		var int32 size
		var int32 fbytes
		var int32 nargs
	end
end

global record pclrec =
	var int16 opcode
	union
		var int16 cond
		var int16 align
		var int16 index
	end
	var int32 size
	var int16 mode

	var byte catmode
	var byte catmode2
	var byte isglobal
	var byte isfunction
	var byte isvariadic
	var byte SPARE3

	var int16 mode2
	var int16 fileno

	var int32 lineno
	var ref pclrec nextpcl
	var pclopndrec a,b
end

!Stack model opcodes
!NOTE: this model has changed so that a nominal 'stack' based on the
!register file of the target machine is used.
!That is called the Operand Stack or Opstack

!Stack operands labeled X,Y,Z::
!suffix c,b,a indicates stack-rel position: a is always top, b is next etc
!Xa				Xa is top of stack, when only operand; or Xa is new tos
!Xb, Ya			Ya is top of stack, when 2 operands
!Xc, Yb, Za		Za is top of stack, when 3 operand
!Inline operands are::
!	A or L		First operand
!	B			Second operand
!Use of X/Y/Z on rvalue will pop it unless specified otherwise
!Use of X (or Y) as lvalue will push a new value, replacing any popped values
!   T			Type of first or dummy operand

global tabledata() []ichar pclnames =

	(k_comment,		$),	! str
	(k_blank,		$),	!

	(k_label,		$),	! L::
	(k_labelname,	$),	!
!	(k_userlabel,	$),	!

	(k_procentry,	$),	!
	(k_procexit,	$),	!

	(k_pushall,		$),	!
	(k_startmult,	$),	!
	(k_resetmult,	$),	!
	(k_endmult,		$),	!

	(k_istatic,		$),	!
	(k_zstatic,		$),	!
!	(k_staticfn,	$),	!
	(k_equiv,		$),	!

	(k_pushmem,		$),	! Xa := A (mem strec)
	(k_pushconst,	$),	! Xa := A (const unit)
	(k_dpushconst,	$),	! Xa := A (const unit)
	(k_dpushmem,	$),	! Xa := A (mem strec)
	(k_pushimm,		$),	! Xa := A (imm int/word/real/string etc)
	(k_pushaddr,	$),	! Xa := &A (mem)
	(k_pushptr,		$),	! Xa := Xa^
	(k_pushretslot,	$),	! Xa := Empty(T)
	(k_pushffretval,$),	! Xa := <value in D0 or XMM0>
	(k_pushretval,	$),	! Xa := <value in D1/D0 or XMM0>
	(k_pushargs,	$),	! Force last operand to be on real stack

	(k_popmem,		$),	! A := Xa
	(k_popmemz,		$),	! A := Xa; dest not freed
	(k_popptr,		$),	! Ya^ := Xb
	(k_storemem,	$),	! (Xb, Ya) := Xa, A := Xa (duplicate stack then pop one)
	(k_storeptr,	$),	! ??(Xb, Ya) := Xa, A := Xa (duplicate stack then pop one)
	(k_popstack,	$),	! SP -:= A
	(k_popretval,	$),	! SP -:= A
	(k_moveretval,	$),	! Ensure return value is in suitable register
	(k_copyblock,	$),	! Xb^ := Ya^ for blocks
	(k_initmemz,	$),	! A := Empty
	(k_freemem,		$),	! free(A)
!	(k_dupl,		$),	! dupl(A)

!	(k_pushsx,		$),	! Xa := size-extend(A) (mem)
!	(k_pushzx,		$),	! Xa := zero-extend(A) (mem)
!	(k_poptx,		$),	! A := truncate(Xa)

	(k_free,		$),	! free(Xa), SP+:=1

	(k_add,			$),	! Xa := Xb+Ya
	(k_sub,			$),	! Xa := Xb-Ya
	(k_mul,			$),	! Xa := Xb*Ya
	(k_div,			$),	! Xa := Xb/Ya
	(k_idiv,		$),	! Xa := Xb%Ya
	(k_irem,		$),	! Xa := Xb rem Ya

	(k_neg,			$),	! Xa := -Xa
	(k_abs,			$),	! Xa := abs(Xa)
	(k_inot,		$),	! Xa := inot(Xa)
	(k_notl,		$),	! Xa := not(Xa)
	(k_istruel,		$),	! Xa := istrue(Xa)

	(k_call,		$),	! Call A, B args
	(k_return,		$),	! Return
	(k_callptr,		$),	! Call Xa, A args
	(k_callff,		$),	! Call A, B args
	(k_callptrff,	$),	! Call Xa, A args
	(k_syscall,		$),	! Call system function .index
	(k_jump,		$),	! Goto L
	(k_jumpcc,		$),	! Goto L when Xb cc Ya
	(k_jumpccimm,	$),	! Goto L when Xa cc A
	(k_jumptrue,	$),	! Goto L when Xa
	(k_jumpfalse,	$),	! Goto L when not Xa
	(k_jumpinyz,	$),	! Goto L when Xc in Yb..Za
	(k_jumpnotinyz,	$),	! Goto L when Xc notin Yb..Za
	(k_casejumpeq,	$),	! Goto L pop both when Xb=Ya; else pop Ya only
	(k_casejumpne,	$),	! Goto L pop Ya only when Xb<>Ya; else pop both

	(k_setjumpeq,	$),	! Goto L Xb=Ya; pop Ya only
	(k_setjumpeqx,	$),	! Goto L Xb=Ya; pop both
	(k_setjumpne,	$),	! Goto L Xb<>Ya; pop both

	(k_switch,		$),	!
	(k_switchlab,	$),	!
	(k_info,		$),	!
	(k_endswitch,	$),	!

	(k_setcc,		$),	! Xa := Xb cc Ya
	(k_compare,		$),	! Xa := (Xb>Ya|1|(Xb<Ya|-1|0))

	(k_isequal,		$),	! Xa := Xb == Ya

	(k_iand,		$),	! Xa := Xb iand Ya
	(k_iandc,		$),	! Xa := Xa iand A
	(k_ior,			$),	! Xa := Xb ior  Ya
	(k_ixor,		$),	! Xa := Xb ixor Ya
	(k_shl,			$),	! Xa := Xb << Ya
	(k_shr,			$),
	(k_shlc,		$),	! Xa := Xa << A
	(k_shrc,		$),
	(k_in,			$),
	(k_min,			$),
	(k_max,			$),
	(k_subref,		$),
	(k_addoffset,	$),
	(k_suboffset,	$),
	(k_concat,		$),
	(k_append,		$),
	(k_andl,		$),
	(k_orl,			$),

	(k_index,		$),
	(k_indexc,		$),
	(k_indexmem,	$),
	(k_indexmemc,	$),
	(k_popindex,	$),
	(k_popindexmem,	$),
	(k_popindexmemc,$),
	(k_storeindex,	$),
	(k_dotindex,	$),
	(k_anddotindex,	$),
	(k_dotslice,	$),
	(k_dot,			$),
	(k_popdot,		$),
	(k_storedot,	$),

	(k_indexref,	$),
	(k_indexmemref,	$),
	(k_indexmemcref,$),
!	(k_dotindexref,	$),
	(k_popdotindex,	$),
	(k_popdotslice,	$),
	(k_dotref,		$),

	(k_lwb,			$),
	(k_upb,			$),
	(k_len,			$),
	(k_bounds,		$),

	(k_sqrt,		$),
	(k_sqr,			$),
	(k_sign,		$),
	(k_sin,			$),
	(k_cos,			$),
	(k_tan,			$),
	(k_asin,		$),
	(k_acos,		$),
	(k_atan,		$),
	(k_ln,			$),
	(k_lg,			$),
	(k_log,			$),
	(k_exp,			$),
	(k_round,		$),
	(k_floor,		$),
	(k_ceil,		$),
	(k_fract,		$),
	(k_fmod,		$),
	(k_power,		$),

	(k_addto,		$),	! Xb^ +:= Ya (where result is needed, then dupl Ya first)
	(k_subto,		$),	! or: Ya^ +:= Xb (dupl Xb first if result needed)
	(k_multo,		$),	!
	(k_divto,		$),	!
	(k_idivto,		$),	!
	(k_iremto,		$),	!

	(k_iandto,		$),	! Xb^ iand:= Ya
	(k_iorto,		$),	!
	(k_ixorto,		$),	!
	(k_shlto,		$),	!
	(k_shrto,		$),
	(k_shlcto,		$),	! Xa^ <<:= A
	(k_shrcto,		$),
	(k_minto,		$),
	(k_maxto,		$),
	(k_addoffsetto,	$),
	(k_suboffsetto,	$),
	(k_concatto,	$),
	(k_appendto,	$),

	(k_addmemto,	$),	! A^ +:= Xa
	(k_submemto,	$),
	(k_iandmemto,	$),
	(k_iormemto,	$),
	(k_ixormemto,	$),
	(k_shlcmemto,	$),	! A^^ <<:= B
	(k_shrcmemto,	$),

	(k_negto,		$),
	(k_absto,		$),
	(k_inotto,		$),

	(k_incr,		$),	! ++A
	(k_decr,		$),	! --A
	(k_incrptr,		$),	! ++Xa^
	(k_decrptr,		$),	! --Xa^

	(k_preincrptrx,	$),
	(k_predecrptrx,	$),
	(k_postincrptrx,$),
	(k_postdecrptrx,$),

	(k_convert,		$),
	(k_iwiden,		$),
	(k_uwiden,		$),
	(k_ifloat,		$),
	(k_ufloat,		$),
	(k_ifix,		$),
	(k_ufix,		$),
	(k_narrow,		$),
	(k_softtruncate,$),
	(k_truncate,	$),
	(k_fnarrow,		$),
	(k_fwiden,		$),
	(k_typepun,		$),
	(k_ichartostring,	$),

	(k_swap,		$),
	(k_makerange,	$),
	(k_makelist,	$),
	(k_makeslice,	$),
	(k_slicelen,	$),
	(k_sliceupb,	$),
	(k_sliceptr,	$),

	(k_assem,		$),
	(k_csegment,	$),	!
	(k_isegment,	$),	!
	(k_zsegment,	$),	!
	(k_db,			$),
	(k_dw,			$),
	(k_dd,			$),
	(k_dq,			$),
	(k_resb,		$),
	(k_resw,		$),
	(k_resd,		$),
	(k_resq,		$),

!	(k_stop,		$),	! stop Xa

	(k_dummy,		$),
end

global tabledata() [0:]ichar opndnames =
	(no_opnd=0,			$),

	(mem_opnd,			$),	!j_name
	(memaddr_opnd,		$),

	(const_opnd,		$),	!j_const

	(intimm_opnd,		$),
	(wordimm_opnd,		$),
	(realimm_opnd,		$),
	(strimm_opnd,		$),

	(proc_opnd,			$),
	(dllproc_opnd,		$),
	(field_opnd,		$),
	(genfield_opnd,		$),
	(label_opnd,		$),
	(type_opnd,			$),
	(operator_opnd,		$),
	(syscall_opnd,		$),
	(assem_opnd,		$),
end

global var ref pclrec pccode, pccodex		!genpc adds to this linked list

global var ref pclrec allpclcode

var [1..4]pclopndrec pclbuffers				!operand data written into this circular buffer
var int nextpclindex=1						!next available slot

global var int framebytes			!local stackframe size
global var int parambytes
global var int frameoffset
global var int isthreadedproc
global var int iscallbackproc

var pclopndrec $zero_opnd
global var ref pclopndrec zero_opnd

global proc pclinit=
	$zero_opnd:=genint(0)^
	zero_opnd:=&$zero_opnd

	initpcdest()
end

global proc initpcdest=
!reset pccode/pccodex
!called should have saved any values from last linked list 

pccode:=pccodex:=nil
end

global proc genpc(int opcode, ref pclopndrec a=nil,b=nil)=
var ref pclrec p, oldp

p:=pcm_allocz(pclrec.bytes)
++NPCL

p^.lineno:=mlineno
p^.opcode:=opcode

if a then
	p^.a:=a^
	if b then
		p^.b:=b^
	fi
fi

addpcl(p)

end

proc addpcl(ref pclrec p)=

if pccode then
	pccodex^.nextpcl:=p
	pccodex:=p
else
	pccode:=pccodex:=p
fi
end

global proc genpc_condlab(int opcode, cond, int lab)=
genpc(opcode,genlabel(lab))
pccodex^.cond:=cond
end

global function lastpc:ref pclrec=
return pccodex
end

global proc genpcstr(int opcode,ichar s)=
!as genpc but uses a single immediate string operand

genpc(opcode,genstrimm(s))
end

function newpclopnd:ref pclopndrec=
var ref pclopndrec a

a:=&pclbuffers[nextpclindex++]

if nextpclindex>pclbuffers.len then
	nextpclindex:=1
fi
return a
end

global function duplopnd(ref pclopndrec a)ref pclopndrec=
var ref pclopndrec b
b:=newpclopnd()
if a=b then gerror("DUPLOPND/CLASH") fi
b^:=a^
return b
end

proc writepclblock(ref pclrec m)=
!block single block of mc code, usually belonging to one proc
!initstr=1 to initialise string o/p for single block
!initgenstr() when initstr
var int i

i:=1

while m do
	writepcl(i,m)
	++i
	m:=m^.nextpcl
od
end

global function writepclcode(ichar caption)ref strbuffer=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
var ref strec d
var ref procrec pp

gs_str(dest,"PROC ")
gs_strln(dest,caption)
gs_strln(dest,"---------------------------------------------")

writepclblock(allpclcode)

gs_strln(dest,"---------------------------------------------")

return dest
end

global proc gencomment(ichar s)=
if s=nil or s^=0 then
	genpc(k_blank)
else
	genpcstr(k_comment,s)
fi
end

global function genstrimm(ichar s,int length=-1)ref pclopndrec=
	var ref pclopndrec a
	a:=newpclopnd()
	a^.optype:=strimm_opnd
	if length<0 then
		length:=strlen(s)
	fi
	a^.svalue:=pcm_alloc(length+1)
	memcpy(a^.svalue,s,length+1)

!	a^.size:=length
	return a
end

global function genname(ichar s)ref pclopndrec=
	var ref pclopndrec a
	a:=newpclopnd()
	a^.optype:=strimm_opnd
	a^.svalue:=pcm_copyheapstring(s)
!	a^.size:=ptrsize
	return a
end

global function gensys(int fnindex)ref pclopndrec=
	var ref pclopndrec a
	a:=newpclopnd()
	a^.optype:=syscall_opnd
	a^.value:=fnindex
!	a^.size:=ptrsize
	return a
end

proc writepcl(int index,ref pclrec pcl)=
!	gs_strint(dest,pcl^.lineno iand 16777215)
	gs_leftint(dest,pcl^.lineno iand 16777215,4)
	gs_str(dest,"  ")
	gs_strln(dest,strpcl(pcl))
end

global function strpcl(ref pclrec pcl)ichar=
!static var [512]char str
static var [1512]char str
var [1512]char opnds
var [1256]char opnd2
var [1128]char opcname
var pclopndrec a,b
var int opcode,cond,sizepref

opcode:=pcl^.opcode

cond:=pcl^.cond
a:=pcl^.a
b:=pcl^.b

case opcode
!when k_assem then
!	return "<assem>"

when k_blank then
	return ""
when k_comment then
!	sprintf(&.str,";%s",a.svalue)
	print @&.str,";",,a.svalue
	return &.str

when k_labelname then				!label name will be complete and will have colon(s)
	return a.svalue

!when k_userlabel then
!!	sprintf(&.str,"!%s:",a.svalue)
!	fprint @&.str,"!#:",a.svalue
!	return &.str

when k_label then
!	if b then
!		sprintf(&.str,"L%d:%s	<%s>",a^.value,(a^.isglobal|":"|""),b^.def^.name)
!	else
!		sprintf(&.str,"L%d:%s",a^.value,(a^.isglobal|":"|""))
!	fi
	if b.optype then
!		sprintf(&.str,"L%d:	<%s>",a.value,b.def^.name)
		fprint @&.str,"L#:	<#>",a.value,b.def^.name
	else
!		sprintf(&.str,"L%d:",a.value)
		fprint @&.str,"L#:",a.value
	fi
	return &.str

when k_istatic then
!	sprintf(&.str,"istatic %s:",a.def^.name)
	fprint @&.str,"istatic #:",a.def.name
	return &.str

when k_zstatic then
!	sprintf(&.str,"zstatic %s: %d",a.def^.name, b.value)
	fprint @&.str,"zstatic #: #",a.def.name, b.value
	return &.str
esac

case opcode
when k_jumpcc then
!	sprintf(&.opcname,"jump.%s",jtagnames[cond]+2)
	print @&.opcname,"jump.",,jtagnames[cond]+2

when k_setcc then
!	sprintf(&.opcname,"set.%s",jtagnames[cond]+2)
	print @&.opcname,"set.",,jtagnames[cond]+2

!when k_syscall then
!	sprintf(&.opcname,"callsys    %.*s",strlen(syscallnames[cond])-3,syscallnames[cond])

else
	strcpy(&.opcname,pclnames[opcode]+2)
esac

if pcl^.catmode then
	strcat(&.opcname,".")
	strcat(&.opcname,stdtypenames[pcl^.catmode])
fi

ipadstr(&.opcname,16," ")

strcpy(&.str,"  ")


ipadstr(&.str,8)

strcat(&.str,&.opcname)

if a.optype and b.optype then		!2 operands
	strcpy(&.opnd2,stropnd(&b))
!	sprintf(&.opnds,"%s,\t%s", stropnd(&a),&.opnd2)
	fprint @&.opnds,"#,\t#", stropnd(&a),&.opnd2

elsif a.optype then								!1 operand
	strcpy(&.opnds,stropnd(&a))
else
	opnds[1]:=0
fi


if opnds[1] then
	strcat(&.str,&.opnds)
fi

if opcode=k_procentry then
!	sprintf(&.opnds,", Framesize: %d",pcl^.b.fbytes)
	print @&.opnds,", Framesize:",pcl.b.fbytes
	strcat(&.str,&.opnds)
fi

if pcl^.mode and pcl^.mode<>pcl^.catmode then
!	sprintf(&.opnds," (%s)",strmode(pcl^.mode))
	fprint @&.opnds," (#)",strmode(pcl^.mode)
	strcat(&.str,&.opnds)
fi

if pcl^.catmode2 or pcl^.mode2 then
!	sprintf(&.opnds," Target: %s (%s)",stdtypenames[pcl^.catmode2],
!			(pcl^.mode2|strmode(pcl^.mode2)|"-"))
	fprint @&.opnds," Target: # (#)",stdtypenames[pcl^.catmode2],
			(pcl^.mode2|strmode(pcl^.mode2)|"-")
	strcat(&.str,&.opnds)
fi

if pcl^.align and opcode<>k_syscall then
!	sprintf(&.opnds," Align:%d",pcl^.align)
	print @&.opnds," Align:",,pcl^.align
	strcat(&.str,&.opnds)
fi

if opcode in [k_callff, k_callptrff] then
!	sprintf(&.opnds," Nargs:%d",pcl^.a.nargs)
	print @&.opnds," Nargs:",,pcl^.a.nargs
	strcat(&.str,&.opnds)
fi

return &.str
end

global function stropnd(ref pclopndrec a)ichar=
static var [512]char str

case a^.optype
when no_opnd then
	return ""

when mem_opnd then
	return strmemopnd(a^.def)

when memaddr_opnd then
	return strmemaddropnd(a^.def)

when const_opnd then
	return strconstopnd(a^.code)

when intimm_opnd then
	print @&.str,a^.value

when wordimm_opnd then
	print @&.str,a^.uvalue

when realimm_opnd then
	print @&.str,a^.xvalue,(a.size=4|"*4"|"")

when strimm_opnd then
	if strlen(a^.svalue)>=str.len then
		return "<LONGSTR>"
	else
		fprint @&.str,"""#""",a^.svalue
	fi

when syscall_opnd then
	fprint @&.str,"<#: #>",sysfnnames[a^.value],sysfnlabels[a.value]

when assem_opnd then
!	fprint @&.str,"<#: #>",sysfnnames[a^.value],sysfnlabels[a.value]
	fprint @&.str,"<ASSEM>",jtagnames[a.code.tag]

when label_opnd then
	fprint @&.str,"L#",a^.labelno

else
cpl "BAD OPND",A^.OPTYPE
	return "<UNIMPL OPND>"
esac

return &.str
end

global function getprocname(ref strec d)ichar=
	case d^.name
	when "main" then
		return "main"
	when "start" then
		return "start"
	else
		return getdottedname(d)
	esac
	return ""
end

global function strlabel(int n)ichar=
static var [16]char str
!sprintf(&.str,"L%d",n)
print @&.str,"L",,n
return &.str
end

global function isframe(ref strec d)int=
!don't know how to mark non-frame temps
!might just look at enclosing proc
case d^.nameid
when frameid, paramid then
	return 1
esac
return 0
end

global proc genreturn(int fbytes,pbytes)=
!assume returning from currproc
var [256]char str
var int iscallback

	iscallback:=iscallbackfn(currproc)

	genpc(k_procexit)
end

global function genint(int64 x,int size=8)ref pclopndrec=
var ref pclopndrec a
a:=newpclopnd()
a^.optype:=intimm_opnd

a^.value:=x
return a
end

global function genqint(ref qint q)ref pclopndrec=
var ref pclopndrec a
GERROR("GENQINT")
return a
end

global function genreal(real64 x,int size=8)ref pclopndrec=
var ref pclopndrec a

a:=newpclopnd()
a^.optype:=realimm_opnd
a^.xvalue:=x
a^.size:=size
return a
end

global function genconst_u(unit p,int size=0)ref pclopndrec=
!assume p is a const unit, or possible a name (gives a name
	var ref pclopndrec a

	a:=newpclopnd()
	a^.optype:=const_opnd
	a^.code:=p
!	a^.mode:=p^.mode
!	a^.size:=(size|size|ttsize[p^.mode])

return a
end

global function genassem_u(unit p)ref pclopndrec=
!assume p is a const unit, or possible a name (gives a name
	var ref pclopndrec a

	a:=newpclopnd()
	a^.optype:=assem_opnd
	a^.code:=p
	return a
end

global function genlabel(int x,isglobal=0)ref pclopndrec=
!x is a label index
!generate immediate operand containing label
	var ref pclopndrec a

	a:=newpclopnd()
	a^.optype:=label_opnd
	a^.labelno:=x
	return a
end

global function genmem_u(unit p,int size=0)ref pclopndrec=
return genmem_d(p^.def,ttsize[p^.mode])
end

global function genmem_d(ref strec d,int size=0)ref pclopndrec=
	var ref pclopndrec a

	a:=newpclopnd()
	a^.optype:=mem_opnd

	a^.def:=d
!	a^.mode:=d^.mode
!	a^.size:=(size|size|ttsize[d^.mode])
	return a
end

global function genmemaddr_u(unit p)ref pclopndrec=
	return genmemaddr_d(p^.def)
end

global function genmemaddr_d(ref strec d)ref pclopndrec=
	var ref pclopndrec a

	a:=newpclopnd()
	a^.optype:=memaddr_opnd

	a^.def:=d
!	a^.size:=ptrsize

	return a
end

global function getopndsize_u(unit p)int=
	return ttsize[p^.mode]
end

global function getopndsize_d(ref strec d)int=
	return ttsize[d^.mode]
end

global function isintconst(unit p)int=
if p^.tag=j_const and isintmode(p^.mode) then
	return 1
fi
return 0
end

global function isint32const(unit p)int=
var int64 a
	if isintconst(p) and ttsize[p^.mode]<=8 then
		a:=p^.value
		if a<=int32.maxvalue and a >=int32.minvalue then
			return 1
		fi
	fi
	return 0
end

global function roundto(int64 a,n)int64=
!round a to be multiple of n
!n will be a power of two
--n
while (a iand n) do ++a od
return a
end

global proc pushstack(int n)=
!	GENCOMMENT("PUSHSTACK")
!	if n then
!		genpc(k_sub,dstackopnd,genint(n))
!	fi
end

global proc popstack(int n)=
!	GENCOMMENT("POPSTACK")
!	if n then
!		genpc(k_add,dstackopnd,genint(n))
!	fi
end

global function definelabel:int =
genpc(k_label,genlabel(++labelno))
return labelno
end

global function createfwdlabel:int =
return ++labelno
end

global proc definefwdlabel(int lab) =
genpc(k_label,genlabel(lab))
end

global proc genjumpl(int lab) =
genpc(k_jump,genlabel(lab))
end

global proc do_syscallproc(int fnindex, nparams, int retmode=tvoid)=
	var [256]char str

!	if fncall then
!		lsif simplefunc then
!		else
!			genpc(k_pushretslot)
!			setpclcat_u(p)
!			if ttisreal[p^.mode] then
!				makefloatopnds()
!			fi
!			pccodex^.mode:=p^.mode
!		fi
!	fi
!
	if nparams then
		genpc(k_pushargs)
	fi
	genpc(k_syscall,gensys(fnindex), genint(nparams))

!CPL "SYSCALL",STRMODE(RETMODE)

	if retmode then
		genpc(k_pushretval)			!dummy op: put d0/x0 return value onto opnd stack

		setpclcat_t(retmode)
		if ttisreal[retmode] then
			makefloatopnds()
		fi
		pccodex^.mode:=retmode
	fi
end
!
function strmemopnd(ref strec d)ichar=
	static var [256]char str
	fprint @&.str,"[#]",d^.name
	return &.str
end

function strmemaddropnd(ref strec d)ichar=
	static var [256]char str
	fprint @&.str,"&#",d^.name
	return &.str
end

global function roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
while size iand (targetsize-1) do ++size od
return size
end

global function getpclop(int tag)int=
!convert j-tag to matching pcl code
!not all tags need to be listed, only those treated as a group so that
!this mapping is convenient. Otherwise the pcl code is just hardcoded

static var [,2]int16 maptable = (
	(j_add,		k_add),
	(j_sub,		k_sub),
	(j_mul,		k_mul),
	(j_div,		k_div),
	(j_idiv,	k_idiv),
	(j_irem,	k_irem),
	(j_neg,		k_neg),
	(j_abs,		k_abs),
	(j_inot,	k_inot),
	(j_iand,	k_iand),
	(j_ior,		k_ior),
	(j_ixor,	k_ixor),
	(j_shl,		k_shl),
	(j_shr,		k_shr),
	(j_in,		k_in),
	(j_min,		k_min),
	(j_max,		k_max),
	(j_concat,	k_concat),
	(j_append,	k_append),
	(j_lwb,		k_lwb),
	(j_upb,		k_upb),
	(j_len,		k_len),
	(j_bounds,	k_bounds),
	(j_sqrt,	k_sqrt),
	(j_sqr,		k_sqr),
	(j_sign,	k_sign),
	(j_sin,		k_sin),
	(j_cos,		k_cos),
	(j_tan,		k_tan),
	(j_asin,	k_asin),
	(j_acos,	k_acos),
	(j_atan,	k_atan),
	(j_ln,		k_ln),
	(j_lg,		k_lg),
	(j_log,		k_log),
	(j_exp,		k_exp),
	(j_round,	k_round),
	(j_floor,	k_floor),
	(j_ceil,	k_ceil),
	(j_fract,	k_fract),
	(j_fmod,	k_fmod),

	(j_addto,	k_addto),
	(j_subto,	k_subto),		!to be completed...

	(0,	0))

static var [0..jtagnames.upb]int16 convtable
var int opc

if opc:=convtable[tag] then
	return opc
fi
for i to maptable.upb do
	if maptable[i,1]=tag then
		convtable[i]:=opc:=maptable[i,2]
		return opc
	fi
od
cpl =jtagnames[tag]
gerror("Can't find pcl op")
return 0
end

function strshortmode(int m)ichar=
	return stdtypenames[ttbasetype[m]]
end

global function islogical(unit p)int=			!ISLOGICAL
!return 1 if p is known to have a logical value
case p^.tag
when j_istruel,j_notl,j_andl,j_orl,j_xorl then
	return 1
esac
return 0
end

global proc makefloatopnds=
!turn operands of last pcl rec to float
	case pccodex^.opcode
	when k_index,k_dot, k_indexref, k_dotref,k_popindex,k_storeindex,
		 k_storedot, k_popdot, k_indexmem then
		case pccodex^.catmode2
		when tshort then pccodex^.catmode2:=tshortfloat
		when tscalar then pccodex^.catmode2:=tscalarfloat
		when twide then pccodex^.catmode2:=twidefloat
		esac
	else

		case pccodex^.catmode
		when tshort then pccodex^.catmode:=tshortfloat
		when tscalar then pccodex^.catmode:=tscalarfloat
		when twide then pccodex^.catmode:=twidefloat
		when tr64 then
			if pccodex^.opcode=k_pushconst then
				pccodex^.catmode:=tscalarfloat
			fi
		when tr32 then
			if pccodex^.opcode=k_pushconst then
				pccodex^.catmode:=tshortfloat
			fi
		esac
	esac
end

global proc setpclcat_t(int m)=
	pccodex^.catmode:=stdtypecat[ttbasetype[m]]
	pccodex^.mode:=m
end

global proc setpclcat_u(unit p)=
	setpclcat_t(p^.mode)
end

global proc setpclmode_u(unit p)=
	setpclmode_t(p^.mode)
end

global proc setpclmode_t(int m)=
	pccodex^.catmode:=ttbasetype[m]
	pccodex^.mode:=m
end

global proc callflexhandler(int m, ichar opname,int nargs) =
	static var [64]char str
	var int mbase:=ttbasetype[m]

GERROR("FLEX HANDLER")
!!	sprintf(&.str,"q%s.%s_%s",stdtypenames[mbase],stdtypenames[mbase],opname)
!	fprint @&.str,"q#.#_#",stdtypenames[mbase],stdtypenames[mbase],opname
!	do_syscallproc(&.str,nargs)
end

=== mm_blockpcl.m 17/35 ===
import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_tables
import mm_lib
import mm_libpcl
import mm_diags

const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

!const dodotchains=0
const dodotchains=1

const maxnestedloops	= 50

var [maxnestedloops,4]int loopstack
var int loopindex							!current level of nested loop/switch blocks

const maxparams=100

const maxswitchrange=500
const maxcases=maxswitchrange

const maxcasedepth=20
var [maxcasedepth]unit casestmt
var [maxcasedepth]int caseelse
var int casedepth

var ref[]int sw_labeltable			!set from do-switch
var ref[]int sw_valuetable
var int sw_lower
var int sw_ncases					!1..n for serial switch; 0 for simple
var byte sw_defaultseen				!starts at 0, set to 1 when default: seen
var int sw_defaultlabel
var int sw_breaklabel

var int maxreg=0

global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
var unit a,b
var ref strec d

	mlineno:=p^.lineno

	a:=p^.a
	b:=p^.b

	switch p^.tag
	when j_const         then do_const(p)
	when j_null          then do_null(p,a,b)
	when j_name          then do_name(p)
	when j_block,j_stmtblock         then do_block(p,a)
	when j_decimal       then do_decimal(p,a,b)
!	when j_whenthen      then do_whenthen(p,a,b)
!	when j_elsif         then do_elsif(p,a,b)
!	when j_fmtitem       then do_fmtitem(p,a,b)
!	when j_nogap         then do_nogap(p,a,b)
	when j_callproc      then do_callproc(p,a,b,0)
	when j_callmproc     then do_callproc(p,a,b,0)
	when j_return        then do_return(p,a,b)
	when j_returnmult    then do_returnmult(p,a,b)
	when j_assign        then do_assign(p,a,b,0)
!	when j_shallowcopy   then do_shallowcopy(p,a,b)
	when j_deepcopy      then do_assign(p,a,b,0)
	when j_to            then do_to(p,a,b)
	when j_if            then do_if(p,a,b,p^.c,0)
	when j_longif        then do_longif(p,a,b,0)
	when j_forup         then do_for(p,a,b,p^.c,0)
	when j_fordown       then do_for(p,a,b,p^.c,1)
!	when j_forstep       then do_forstep(p,a,b)
	when j_forall        then do_forall(p,a,b)
	when j_forallrev     then do_forallrev(p,a,b)
	when j_foreach       then do_foreach(p,a,b)
	when j_foreachrev    then do_foreachrev(p,a,b)
	when j_cfor          then do_cfor(p,a,b)
	when j_while         then do_while(p,a,b)
	when j_repeat        then do_repeat(p,a,b)
	when j_goto          then do_goto(p,a,b)
	when j_gotoblock     then do_gotoblock(p,a,b)
	when j_labeldef      then do_labeldef(p)
	when j_restart       then do_exit(p,1)
	when j_redo          then do_exit(p,2)
	when j_next          then do_exit(p,3)
	when j_exit          then do_exit(p,4)
	when j_do            then do_do(p,a,b)
	when j_case          then do_case(p,a,b,p^.c,0,0)
	when j_docase        then do_case(p,a,b,p^.c,1,0)
	when j_switch        then do_switch(p,a,b,p^.c,0,0)
	when j_doswitch      then do_switch(p,a,b,p^.c,1,0)
	when j_swap          then do_swap(p,a,b)
	when j_select        then do_select(p,a,b,p^.c,0)
	when j_print         then do_print(p,a,b)
	when j_println       then do_print(p,a,b)
	when j_fprint        then do_print(p,a,b)
	when j_fprintln      then do_print(p,a,b)
	when j_cprint        then do_cprint(p,a,b)
	when j_cprintln      then do_cprintln(p,a,b)
	when j_sprint        then do_sprint(p,a,b)
	when j_sfprint       then do_sfprint(p,a,b)
	when j_read	         then do_read(p,a)
	when j_readln        then do_readln(a)
	when j_sread         then do_sread(p,a,b)
	when j_sreadln       then do_sreadln(p,a,b)
	when j_stop          then do_stop(p,a)
	when j_try           then do_try(p,a,b)
	when j_except        then do_except(p,a,b)
	when j_yield         then do_yield(p,a,b)
	when j_raise         then do_raise(p,a,b)
!	when j_callhostproc  then do_callhostproc(p,a,b)
	when j_eval          then do_eval(p,a,b)
	when j_lambda        then do_lambda(p,a,b)
	when j_andl          then do_bin(p,a,b,k_iand)
	when j_orl           then do_bin(p,a,b,k_ior)
	when j_xorl          then do_xorl(p,a,b)
	when j_notl          then do_notl(p,a)
	when j_istruel       then do_istruel(p,a)
	when j_makelist      then do_makelist(p,a,b)
	when j_makerange     then do_makerange(p,a,b)
	when j_makeset       then do_makeset(p,a,b)
	when j_makedict      then do_makedict(p,a,b)
	when j_exprlist      then do_exprlist(p,a,b)
	when j_multexpr      then do_multexpr(p,a,b)
	when j_keyword       then do_keyword(p,a,b)
	when j_keyvalue      then do_keyvalue(p,a,b)
	when j_assignx       then do_assign(p,a,b,1)
	when j_deepcopyx     then do_assign(p,a,b,1)
	when j_callfn        then do_callproc(p,a,b,1)
	when j_callmfn       then do_callproc(p,a,b,1)
	when j_applyop       then do_applyop(p,a,b)
	when j_applyopx      then do_applyopx(p,a,b)
	when j_andand        then do_andand(p,a,b)
	when j_eq            then do_setcc(p,a,b)
	when j_ne            then do_setcc(p,a,b)
	when j_lt            then do_setccx(p,a,b)
	when j_le            then do_setccx(p,a,b)
	when j_ge            then do_setccx(p,a,b)
	when j_gt            then do_setccx(p,a,b)
	when j_isequal       then do_isequal(p,a,b)
	when j_add           then do_binx(p,a,b,k_add)
	when j_sub           then do_binx(p,a,b,k_sub)
	when j_mul           then do_muldivx(p,a,b,k_mul)
	when j_div           then do_binx(p,a,b,k_div)
	when j_idiv          then do_muldivx(p,a,b,k_idiv)
	when j_irem          then do_muldivx(p,a,b,k_irem)
!	when j_divrem        then do_bin(p,a,b,k_divrem)
	when j_iand          then do_bin(p,a,b,k_iand)
	when j_ior           then do_bin(p,a,b,k_ior)
	when j_ixor          then do_bin(p,a,b,k_ixor)
	when j_shl           then do_shl(p,a,b,k_shl,k_shlc)
	when j_shr           then do_shl(p,a,b,k_shr,k_shrc)
	when j_in            then do_bin(p,a,b,k_in)
!	when j_notin         then do_bin(p,a,b,k_)
!	when j_inrev         then do_bin(p,a,b)
	when j_min           then do_binx(p,a,b,k_min)
	when j_max           then do_binx(p,a,b,k_max)
	when j_addoffset     then do_bin(p,a,b,k_addoffset)
	when j_suboffset     then do_bin(p,a,b,k_suboffset)
	when j_subref        then do_bin(p,a,b,k_subref)
	when j_concat        then do_bin(p,a,b,k_concat)
	when j_append        then do_bin(p,a,b,k_append)
	when j_clamp         then do_clamp(p,a,b)
!	when j_index         then do_index(p,a,b)
	when j_index         then do_index(p,0)
	when j_slice         then do_slice(p,a,b)
	when j_dotindex      then do_dotindex(p,a,b)
	when j_dotslice      then do_dotslice(p,a,b)
	when j_anddotindex   then do_anddotindex(p,a,b)
	when j_anddotslice   then do_anddotslice(p,a,b)
	when j_keyindex      then do_keyindex(p,a,b)
!	when j_dot           then do_dot(p,a,p^.offset,0)
	when j_dot           then do_dot(p,0)
	when j_dotattr       then do_dotattr(p,a,b)
	when j_atan2         then do_atan2(p,a,b)
	when j_power         then do_power(p,a,b)
	when j_ptr           then do_ptr(p,a,b)
	when j_addrof        then do_addrof(p,a)
	when j_addroffirst   then do_addrof(p,a)
	when j_convert       then do_convert(p,a,b)
	when j_convertref    then do_convertref(p,a,b)
	when j_autocast      then do_autocast(p,a,b)
	when j_typepun       then do_typepun(p,a,b)
	when j_typeconst     then do_typeconst(p)
	when j_operator      then do_operator(p,a,b)
	when j_upper         then do_upper(p,a,b)
	when j_neg           then do_unaryx(p,a,k_neg)
	when j_abs           then do_unaryx(p,a,k_abs)
	when j_inot          then do_unary(p,a,k_inot)
	when j_chr           then do_chr(p,a,b)
	when j_asc           then do_asc(p,a,b)
	when j_sqrt          then
		evalunit(a)
		genpc(k_sqrt)
		setpclmode_u(a)

	when j_sqr           then do_sqr(p,a,b)
	when j_sign          then do_sign(p,a,b)
	when j_sin           then do_maths(p,a,sysfn_sin)
	when j_cos           then do_maths(p,a,sysfn_cos)
	when j_tan           then do_maths(p,a,sysfn_tan)
	when j_asin          then do_maths(p,a,sysfn_asin)
	when j_acos          then do_maths(p,a,sysfn_acos)
	when j_atan          then do_maths(p,a,sysfn_atan)
	when j_ln            then do_maths(p,a,sysfn_ln)
	when j_lg            then do_maths(p,a,sysfn_lg)
	when j_log           then do_maths(p,a,sysfn_log)
	when j_exp           then do_maths(p,a,sysfn_exp)
	when j_round         then do_maths(p,a,sysfn_round)
	when j_floor         then do_maths(p,a,sysfn_floor)
	when j_ceil          then do_maths(p,a,sysfn_ceil)
	when j_fract         then do_maths(p,a,sysfn_fract)
	when j_fmod          then do_fmod(p,a,b)
	when j_lwb           then do_lwb(p,a)
	when j_upb           then do_upb(p,a)
	when j_len           then do_len(p,a)
	when j_bounds        then do_bounds(p,a)
	when j_bitwidth      then do_bitwidth(p,a)
	when j_bytesize      then do_bytesize(p,a)
	when j_typeof        then do_typeof(p,a,b)
	when j_typestr       then do_typestr(p,a,b)
	when j_sliceptr      then do_sliceptr(p,a)
	when j_minvalue      then do_minvalue(p,a,b)
	when j_maxvalue      then do_maxvalue(p,a,b)

	when j_incr,j_decr	 then do_incr(p,a)

	when j_preincrx      then do_incrx(p,a,k_preincrptrx)
	when j_predecrx      then do_incrx(p,a,k_predecrptrx)
	when j_postincrx     then do_incrx(p,a,k_postincrptrx)
	when j_postdecrx     then do_incrx(p,a,k_postdecrptrx)

	when j_addto         then do_binto(p,a,b,k_addto)
	when j_subto         then do_binto(p,a,b,k_subto)
	when j_multo         then do_binto(p,a,b,k_multo)
	when j_divto         then do_binto(p,a,b,k_divto)
	when j_idivto        then do_binto(p,a,b,k_idivto)
	when j_iremto        then do_binto(p,a,b,k_iremto)
	when j_iandto        then do_binto(p,a,b,k_iandto)
	when j_iorto         then do_binto(p,a,b,k_iorto)
	when j_ixorto        then do_binto(p,a,b,k_ixorto)
	when j_shlto         then do_shlto(p,a,b,k_shlto,k_shlcto)
	when j_shrto         then do_shlto(p,a,b,k_shrto,k_shrcto)
	when j_minto         then do_binto(p,a,b,k_minto)
	when j_maxto         then do_binto(p,a,b,k_maxto)
	when j_addoffsetto   then do_binto(p,a,b,k_addoffsetto)
	when j_suboffsetto   then do_binto(p,a,b,k_suboffsetto)
	when j_negto         then do_unaryto(p,a,k_negto)
	when j_absto         then do_unaryto(p,a,k_absto)
	when j_inotto        then do_unaryto(p,a,k_inotto)
	when j_head, j_tail, j_init, j_last, j_flexptr, j_stringz,j_reverse,
		 j_dupl then
		doflexop(p,a,jtagnames[p^.tag]+2)

	when j_take, j_drop, j_left, j_right,j_convlc, j_convuc then
		do_binflex(p,a,b,0,jtagnames[p^.tag]+2)

	when j_recase        then do_recase(p,a)

!	when j_isvoid        then do_isvoid(p,a,b)
!	when j_isdef         then do_isdef(p,a,b)
!	when j_isint         then do_isint(p,a,b)
!	when j_isreal        then do_isreal(p,a,b)
!	when j_isstring      then do_isstring(p,a,b)
!	when j_islist        then do_islist(p,a,b)
!	when j_isrecord      then do_isrecord(p,a,b)
!	when j_isarray       then do_isarray(p,a,b)
!	when j_isset         then do_isset(p,a,b)
!	when j_ispointer     then do_ispointer(p,a,b)
!	when j_ismutable     then do_ismutable(p,a,b)

	when j_cvlineno      then do_cvlineno(p,a,b)
!	when j_cvstrlineno   then do_cvstrlineno(p,a,b)
	when j_cvmodulename  then do_cvmodulename(p,a,b)
	when j_cvfilename    then do_cvfilename(p,a,b)
!	when j_cvfunction    then do_cvfunction(p,a,b)
!	when j_cvdate        then do_cvdate(p,a,b)
!	when j_cvtime        then do_cvtime(p,a,b)
!	when j_cvversion     then do_cvversion(p,a,b)
!	when j_cvtypename    then do_cvtypename(p,a,b)
!	when j_cvtargetbits  then do_cvtargetbits(p,a,b)
!	when j_cvtargetsize  then do_cvtargetsize(p,a,b)
!	when j_cvtargetcode  then do_cvtargetcode(p,a,b)

	when j_syscall then
		if a then
			evalunit(a)
			do_syscallproc(p.opcode,1,p.mode)
		else
			do_syscallproc(p.opcode,0,p.mode)
		fi

	when j_assem         then do_assem(p,a)
	else
		gerror_s("UNSUPPORTED TAG: %s",JTAGNAMES[P^.TAG])
	endswitch

	if ttisreal[p^.mode] then
		makefloatopnds()
	fi

	if p^.popflag then
		if ttisflexvar[p^.mode] then
			callflexhandler(p^.mode,"free",1)
		elsif p^.mode=tmult then
			if p^.tag=j_callfn and p^.a^.tag=j_name then
				d:=p^.a^.def
				for i to d^.nretvalues do
					genpc(k_free)
					setpclcat_t(d^.modelist[i])
					if ttisreal[d^.modelist[i]] then
						makefloatopnds()
					fi
				od
			else
				gerror("Can't free mult/ret values")
			fi

		else
			genpc(k_free)
			setpclcat_t(p^.mode)
			if ttisreal[p^.mode] then
				makefloatopnds()
			fi
		fi
		p^.popflag:=0
	fi

end

proc evalref(unit p)=
	var unit a,b,c
	a:=p^.a
	b:=p^.b
	c:=p^.c

	switch p^.tag
	when j_name then
		genpc(k_pushaddr,genmem_d(p^.def))

	when j_index then
!		do_index(p,a,b,1)
		do_index(p,1)

	when j_dot then
!		do_dot(p,a,p^.offset,1)
		do_dot(p,1)

	when j_ptr then
		evalunit(p^.a)

	else
		case p^.tag
		when j_if then
			do_if(p,a,b,c,1)
		when j_longif then
			do_longif(p,a,b,1)
		when j_select then
			do_select(p,a,b,c,1)
		when j_switch then
			do_switch(p,a,b,c,0,1)
		when j_case then
			do_case(p,a,b,c,0,1)
		else
			PRINTUNIT(P)
			gerror("evalref")
		esac
	end switch
end

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
var pclopndrec lx,mx
var unit q,r,s
var int lab2

q:=p^.a
r:=p^.b

switch p^.tag
when j_andl then
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

when j_orl then
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

when j_notl then
	case opc
	when kjumpf then
		genjumpcond(kjumpt,q,lab)
	when kjumpt then
		genjumpcond(kjumpf,q,lab)
	esac

when j_istruel then
	genjumpcond(opc,q,lab)

when j_block then
	while q and q^.nextunit do
		evalunit(q)
		q:=q^.nextunit
	od
	genjumpcond(opc,q,lab)

when j_eq,j_ne,j_lt,j_le,j_ge,j_gt then

	gcomparejump(opc,p,q,r,lab)

when j_inrange then
	evalunit(q)
	evalunit(r^.a)
	evalunit(r^.b)
	genpc((opc=kjumpf|k_jumpnotinyz|k_jumpinyz),genlabel(lab))
	setpclmode_u(q)

when j_inset then
	s:=r^.a
	if s=nil then
		gerror("empty set")
	fi
	if s^.nextunit=nil then			!treat as x=y
		p^.tag:=j_eq
		gcomparejump(opc,p,q,s,lab)
		return
	fi

	if opc=kjumpf then
		lx:=genlabel(lab)^
		lab2:=createfwdlabel()
		mx:=genlabel(lab2)^
		evalunit(q)

		while s do
			evalunit(s)
			s:=s^.nextunit
			if s then
				genpc(k_setjumpeq,&mx)
			else
				genpc(k_setjumpne,&lx)
			fi
			setpclcat_u(q)
		od
		definefwdlabel(lab2)
	else
		lx:=genlabel(lab)^
		evalunit(q)

		while s do
			evalunit(s)
			s:=s^.nextunit
			genpc((s|k_setjumpeq|k_setjumpeqx),&lx)
			setpclcat_u(q)
		od
	fi

else			!other, single expression
	if ttisflexvar[p^.mode] then
		genpc(k_pushretslot)
		setpclcat_t(ti64)
		evalunit(p)
		callflexhandler(p^.mode,"istrue",1)
		genpc((opc=kjumpt|k_jumptrue|k_jumpfalse),genlabel(lab))
		setpclmode_t(ti64)
	else
		evalunit(p)

		genpc((opc=kjumpt|k_jumptrue|k_jumpfalse),genlabel(lab))
		setpclmode_u(p)
	fi
endswitch
end

proc gcomparejump(int jumpopc,unit p,lhs,rhs,int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	var int cond

	cond:=p^.tag				!eqop,neop, etc

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	if ttisflexvar[lhs^.mode] then
		do_comparejumpflex(cond,lhs,rhs,lab)
		return
	fi

	evalunit(lhs)

!	if isshortconst(rhs) then
!		genpc_condlab(k_jumpccimm,cond,lab)
!		pccodex^.b:=genconst_u(rhs)^
!	else
		evalunit(rhs)
		genpc_condlab(k_jumpcc,cond,lab)
!	fi
	setpclmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	genpc(k_jump,genlabel(lab))
end

function reversecond(int op)int=
!reverse conditional operator

	case op
	when j_eq then return j_ne
	when j_ne then return j_eq
	when j_lt then return j_ge
	when j_le then return j_gt
	when j_ge then return j_lt
	when j_gt then return j_le
	esac
	return 0
end

proc stacklooplabels(int a,b,c,d)=
!don't check for loop depth as that has been done during parsing
	++loopindex
	if loopindex>maxnestedloops then
		gerror("Too many nested loops")
	fi

	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c
	loopstack[loopindex,4]:=d

end

function findlooplabel(int k,n)int=
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
var int i

i:=loopindex-(n-1)		!point to entry
if i<1 or i>loopindex then gerror("Bad loop index") fi
return loopstack[i,k]
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: %s",mess)
end

proc do_const(unit p) =
	genpc(k_pushconst,genconst_u(p))
	case p^.mode
	when ti8,ti16,ti32 then
		setpclmode_t(ti64)
	when tu8,tu16,tu32 then
		setpclmode_t(tu64)
	else
		if p^.isastring then
			setpclmode_t(trefchar)
		else
			setpclmode_u(p)
		fi
	esac
end

proc do_null(unit p,a,b) =
	unimpl("do_null")
end

proc do_name(unit p) =
	var ref strec d

	d:=p^.def
	case d^.nameid
	when procid,dllprocid then
		genpc(k_pushaddr,genmem_u(p))
	when labelid then
		if d^.index=0 then
			d^.index:=++labelno
		fi
		genpc(k_jump, genlabel(d^.index))
		p^.popflag:=0

	else
		genpc(k_pushmem,genmem_u(p))
		setpclcat_u(p)

	esac
end

proc do_block(unit p,a) =
	while a and a^.nextunit do
		evalunit(a)
		a:=a^.nextunit
	od
	if a then
		evalunit(a)
	fi
end

proc do_decimal(unit p,a,b) =
	unimpl("do_decimal")
end

proc do_callproc(unit p,a,b,int fncall) =
var [maxparams]unit params
var int nparams,m,simplefunc
var ref strec d,e
var unit q

	case a^.tag
	when j_name then
		d:=a^.def
		simplefunc:=d^.simplefunc
	when j_ptr then
!		isfnptr:=1
		d:=ttnamedef[a^.mode]
		simplefunc:=1

		e:=d^.paramlist
		while e<>nil do
			unless issimpletype(d^.mode) then
				simplefunc:=0
			end
			e:=e^.nextdef
		od

	else
		gerror("call/not ptr")
	esac

	if d^.fflang in [clangff,windowsff] then
		do_callff(p,a,b,d,fncall)
		return
	fi

	if fncall then
		if p^.mode=tmult then
			for i:=d^.nretvalues downto 1 do
				genpc(k_pushretslot)
				m:=d^.modelist[i]
				setpclcat_t(m)
				if ttisreal[m] then
					makefloatopnds()
				fi
				pccodex^.mode:=m
			od
		elsif simplefunc then
		else
			genpc(k_pushretslot)
			setpclcat_u(p)
			if ttisreal[p^.mode] then
				makefloatopnds()
			fi
			pccodex^.mode:=p^.mode
		fi
	fi

	nparams:=0
	while b do
		params[++nparams]:=b
		b:=b^.nextunit
	od
	for i:=nparams downto 1 do
		q:=params[i]

		if isshortconst(q) then
			genpc(k_dpushconst,genconst_u(q))
			setpclcat_u(q)
		elsif isshortmem(q) then
			genpc(k_dpushmem,genmem_u(q))
			setpclcat_u(q)
		else
			evalunit(q)
		fi
	od
	genpc(k_pushargs)

	case a^.tag
	when j_name then
		genpc(k_call, genmemaddr_u(a),genint(nparams))
	else
		evalunit(a^.a)
		genpc(k_callptr,genint(nparams))
	esac

	if fncall and simplefunc then
		genpc(k_pushretval)			!dummy op: put d0/x0 return value onto opnd stack
		setpclcat_u(p)
		if ttisreal[p^.mode] then
			makefloatopnds()
		fi
		pccodex^.mode:=p^.mode
	fi
end

proc do_return(unit p,a,b) =
	if a then
		evalunit(a)
		if currproc^.simplefunc then
			genpc(k_moveretval)
		else
			genpc(k_popretval,genint(parambytes))
		fi
		setpclcat_u(a)
		if ttisreal[a^.mode] then
			makefloatopnds()
		fi
	fi

	if currproc^.simplefunc then
		genpc(k_procexit)
	else
		genjumpl(retindex)
	fi
end

proc do_returnmult(unit p,a,b) =
	var int offset

	offset:=0
	while a do
		evalunit(a)
		genpc(k_popretval,genint(parambytes+offset))
		setpclcat_u(a)

		if ttisreal[a^.mode] then
			makefloatopnds()
		fi
		offset+:=ttsize[a^.mode]
		a:=a^.nextunit
	od
	if p^.mode=tvoid then
		genjumpl(retindex)
	fi
end

proc do_assign(unit p,a,b, int fstore) =
!fstore=1 when result is needed
	var unit c
	var ref strec d
	var int offset

	if stdtypecat[ttbasetype[a^.mode]]=tblock and fstore=0 then
		do_assignblock(p,a,b)
		return
	fi

	if ttisflex[a^.mode] then
		do_assignflex(p,a,b,fstore)
		return
	fi

	case a^.tag
	when j_makelist then
		if fstore then gerror("multassign/store?") fi
		do_multassign(a,b)
		return
	when j_index then
		do_popindex(p,a.a, a.b, b, (fstore|k_storeindex|k_popindex))
		return
	when j_dot then
		do_popdot(p,a.a,b, a.offset,(fstore|k_storedot|k_popdot))
		return
	esac

	evalunit(b)

	switch a^.tag
	when j_name then
		genpc((fstore|k_storemem|k_popmem),genmem_u(a))
	when j_slice then
		evalref(a)
		genpc((fstore|k_storeptr|k_popptr),genint(0))
	when j_ptr then
		c:=a^.a
		offset:=0
		if c.tag in [j_add, j_sub] and c.b^.tag=j_const then
			offset:=(c.tag=j_add|c.b.value|-c.b.value)
			evalunit(c.a)
		else
			evalunit(c)
		fi
		genpc((fstore|k_storeptr|k_popptr),genint(offset))
	when j_if, j_longif, j_case, j_switch, j_select then
		evalref(a)
		genpc((fstore|k_storeptr|k_popptr),genint(0))
	when j_dotindex then
		evalref(a^.a)
		evalunit(a^.b)
		if fstore then
			gerror("storedotix?")
		else
			genpc(k_popdotindex)
		fi
		return
	when j_dotslice then
		evalref(a^.a)
		evalunit(a^.b^.a)
		evalunit(a^.b^.b)
		if fstore then
			gerror("storedotsl?")
		else
			genpc(k_popdotslice)
		fi
		return
	else
		cpl jtagnames[a^.tag]
		gerror("Can't assign")
	end switch

	setpclcat_u(a)

	if b^.mode=tmult then
		if fstore then gerror("chained assign not allowed") fi
!CPL "NORMAL ASSIGN: RHS IS MULT"
		if b^.tag<>j_callfn or b^.a^.tag<>j_name then
			gerror("assign/mult/call error")
		fi
		d:=b^.a^.def
!CPL "RET VALUES",D^.NRETVALUES
		for i:=2 to d^.nretvalues do
			genpc(k_free)
			setpclcat_t(d^.modelist[i])
			if ttisreal[d^.modelist[i]] then
				makefloatopnds()
			fi
		od
	fi

end

proc do_shallowcopy(unit p,a,b) =
	unimpl("do_shallowcopy")
end

!proc do_deepcopy(unit p,a,b) =
!	unimpl("do_deepcopy")
!end

proc do_to(unit p,a,b) =
	var unit avar
	var pclopndrec avaropnd
	var int lab_a,lab_b,lab_c,lab_d,count

	avar:=p^.c
	avaropnd:=genmem_u(avar)^

	lab_a:=definelabel()
	a^.mode:=ti64

	evalunit(a)
	genpc(k_popmem,&avaropnd)
	setpclcat_t(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_a,lab_b,lab_c,lab_d)

!check for count being nonzero
	if a^.tag<>j_const then			!assume const limit is non-zero
		genpc(k_pushmem,&avaropnd)
		setpclcat_t(ti64)
		genpc(k_pushimm,zero_opnd)
		genpc_condlab(k_jumpcc,j_le,lab_d)
		setpclmode_t(ti64)

!		genmc(m_cmp,avaropnd,zero_opnd)
!		genmc(m_jmpcc,genlabel(lab_d))
!		mccodex^.cond:=le_cond
	else
		count:=a^.value
		if count<=0 then
			genjumpl(lab_d)
		fi
	fi

	definefwdlabel(lab_b)
	evalunit(b)			!main body

	definefwdlabel(lab_c)

	genpc(k_decr,&avaropnd)
!	setpclmode_t(ti64)
	setpclcat_t(ti64)
	genpc(k_pushmem,&avaropnd)
	setpclcat_t(ti64)
	genpc(k_jumptrue,genlabel(lab_b))
	setpclmode_t(ti64)

	definefwdlabel(lab_d)
	--loopindex
end

proc do_if(unit p,a,b,c,int isref) =
	var int lab1,lab2,ismult

	ismult:=p^.mode<>tvoid
	if ismult then genpc(k_startmult) fi
	lab1:=createfwdlabel()

	genjumpcond(kjumpf,a,lab1)

	if isref then evalref(b) else evalunit(b) fi
	if ismult then genpc(k_resetmult) fi

	if c then
		lab2:=createfwdlabel()			!label past else part
		genjumpl(lab2)
		definefwdlabel(lab1)
		if isref then evalref(c) else evalunit(c) fi
		if ismult then genpc(k_endmult) fi
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_longif(unit p,a,b,int isref) =
	var int labend,i,lab2,ismult
	var unit pcond

	labend:=createfwdlabel()
	ismult:=p^.mode<>tvoid

	pcond:=a
	i:=0
	if ismult then genpc(k_startmult) fi
!IF ISMULT THEN
!	CPL "LONGIF/MULT"
!FI

	while pcond do
		++i
		lab2:=createfwdlabel()

		genjumpcond(kjumpf,pcond^.a,lab2)

!		evalunit(pcond^.b)
		if isref then evalref(pcond^.b) else evalunit(pcond^.b) fi
		if ismult then genpc(k_resetmult) fi

		if pcond^.nextunit or b then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
		pcond:=pcond^.nextunit
	od

	if b then
		if isref then evalref(b) else evalunit(b) fi
		if ismult then genpc(k_endmult) fi
	fi
	definefwdlabel(labend)
end

proc do_for(unit p,ivar,pbody,pautovar,int down) =
	var unit pfrom, pto, pstep, pelse, px, plimit
	var pclopndrec indexopnd,autoopnd
	var int lab_a,lab_b,lab_c,lab_d,lab_e
	var int a,b

	if ivar^.tag<>j_name then
		gerror("complex (non-i64) for-loop var?")
	fi

	pfrom:=ivar^.nextunit
	pto:=pfrom^.nextunit

	if pto^.tag=j_ptr then
		px:=pto^.a
		var ref strec d
		if px^.tag=j_name and (d:=px^.def)^.nameid=paramid and\
			 d^.parammode=out_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	fi

	pstep:=pto^.nextunit

	pelse:=pbody^.nextunit

	lab_a:=definelabel()
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

!	genlabel(lab_d)

	stacklooplabels(lab_a, lab_b, lab_c, lab_d)

!now start generating code
	evalunit(pfrom)
	indexopnd:=genmem_u(ivar)^
	genpc(k_popmem,&indexopnd)
	setpclcat_u(ivar)

	if pautovar then
		pautovar^.mode:=ti64
		evalunit(pto)
		genpc(k_popmem,genmem_u(pautovar))
		setpclcat_u(pautovar)
		plimit:=pautovar
	else
		plimit:=pto
!	elsecase pto^.tag
!	when j_const then
!		limopnd:=genconst_u(pto)^
!		limopc:=k_pushconst
!	when j_name then
!		limopnd:=genmem_u(pto)^
!		limopc:=k_pushmem
	fi

	if pfrom^.tag=j_const and pto^.tag=j_const then
		a:=pfrom^.value
		b:=pto^.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			genpc(k_jump, genlabel(lab_e))
!			gerror("For/0 iters")
		fi
!	elsif issimple(pfrom) then						!initial check needed
	else
!	XXX::
		if pfrom^.tag=j_const then				!reverse condition; compare mem:imm
			evalunit(plimit)
!			genpc(limopc,&limopnd)
!			setpclcat_u(pto)

!			evalunit(pfrom)
!			genpc_condlab(k_jumpcc,(down|j_gt|j_lt),lab_d)
			genpc_condlab(k_jumpccimm,(down|j_gt|j_lt),lab_e)
			pccodex^.b:=genconst_u(pfrom)^

		else
			genpc(k_pushmem,&indexopnd)
			setpclcat_u(ivar)

			evalunit(plimit)
			genpc_condlab(k_jumpcc,(down|j_lt|j_gt),lab_e)
		fi
		setpclmode_t(ti64)
!	else
!		goto xxx
!!		gerror("FOR/FROM/CX")
	fi

	definefwdlabel(lab_b)

	evalunit(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
!	lhs:=evalexpr(pstep,r0)
		if pstep^.tag=j_const then
			genpc(k_pushmem,&indexopnd)
			setpclcat_u(ivar)
			evalunit(pstep)
!			genpc((down|k_sub|k_add),gentos(ti64))
			genpc((down|k_sub|k_add))
			genpc(k_popmem,&indexopnd)
			setpclcat_u(ivar)
		else
			gerror("CAN'T DO FOR/BY/VAR")
!			rhs:=loadexpr(pstep,r0)
!			genmc((down|m_sub|m_add),genmem_u(ivar),rhs)
		fi
	else
		genpc((down|k_decr|k_incr),&indexopnd)
		setpclcat_t(ti64)
	fi

	genpc(k_pushmem,&indexopnd)
	setpclcat_u(ivar)

	if isshortconst(plimit) then
		genpc_condlab(k_jumpccimm,(down|j_ge|j_le),lab_b)
		pccodex^.b:=genconst_u(plimit)^
	else
		evalunit(plimit)
		genpc_condlab(k_jumpcc,(down|j_ge|j_le),lab_b)
	fi
	setpclmode_t(ti64)

	if pelse then
!CPL "FOR-ELSE SEEN",PELSE^.LINENO IAND 16777215, SOURCEFILENAMES[P^.LINENO>>24]
!GERROR("FOR-ELSE SEEN")
		definefwdlabel(lab_e)
		evalunit(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p,a,b) =
	unimpl("do_forall")
end

proc do_forallrev(unit p,a,b) =
	unimpl("do_forallrev")
end

proc do_foreach(unit p,a,b) =
	unimpl("do_foreach")
end

proc do_foreachrev(unit p,a,b) =
	unimpl("do_foreachrev")
end

proc do_cfor(unit p,a,b) =
	unimpl("do_cfor")
end

proc do_while(unit p,pcond,pbody) =
	var int lab_b,lab_c,lab_d

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_b, lab_c, lab_d)

	genjumpl(lab_c)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalunit(pbody)

	definefwdlabel(lab_c)

	genjumpcond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p,a,b) =
	var int lab_ab, lab_c, lab_d

	lab_ab:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_ab, lab_ab, lab_c, lab_d)

	evalunit(a)

	definefwdlabel(lab_c)

	unless b^.tag=j_const and b^.value=0 then
		genjumpcond(kjumpf,b,lab_ab)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_goto(unit p,a,b) =
	var ref strec d
	case a^.tag
	when j_name then
		d:=a^.def
		if d^.index=0 then
			d^.index:=++labelno
		fi
		genpc(k_jump, genlabel(d^.index))
	else
		gerror("GOTO PTR")
	esac
end

proc do_gotoblock(unit p,a,b) =
	unimpl("do_gotoblock")
end

proc do_labeldef(unit p) =
	var ref strec d
	d:=p^.def
	if d^.index=0 then
		d^.index:=++labelno
	fi
!	genpcstr(k_userlabel,getfullname(d))
	gencomment(d.name)
	genpc(k_label,genlabel(d^.index))
end

proc do_exit(unit p,int k) =
	var int n,index

	index:=p^.index
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k,index)
	if n=0 then
		gerror("Bad exit/loop index",p)
	else
		genjumpl(n)
	fi
end

proc do_do(unit p,a,b) =
	var int lab_abc,lab_d

	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_abc, lab_d)

	evalunit(a)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_case(unit p,pindex,pwhenthen,pelse, int loopsw,isref)=
	const maxcase=256
	var [maxcase]int labtable
	var [maxcase]unit unittable
	var int ncases

	var int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, ismult,labelse
	var unit w,wt

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	ismult:=p^.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc,lab_abc,lab_abc,lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	if ismult then genpc(k_startmult) fi
	evalunit(pindex)			!load test expr p to t

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	ncases:=0
	wt:=pwhenthen
	while wt do
		w:=wt^.a
		if ncases>=maxcase then
			gerror("too many cases")
		fi
		labtable[++ncases]:=createfwdlabel()
		unittable[ncases]:=wt^.b

		while w do
!CPL "WLOOP",JTAGNAMES[W^.TAG]
			evalunit(w)
			genpc(k_casejumpeq,genlabel(w^.whenlabel:=labtable[ncases]))
			setpclmode_u(w)
			w:=w^.nextunit
		od

		wt:=wt^.nextunit
	od

	genpc(k_free)				!pop index expression
	setpclcat_u(pindex)

	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		if isref then evalref(unittable[i]) else evalunit(unittable[i]) fi
		if ismult then genpc(k_resetmult) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		if isref then evalref(pelse) else evalunit(pelse) fi
		if ismult then genpc(k_endmult) fi
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

proc do_emptycase(unit p,pindex,pwhenthen,pelse, int loopsw,isref)=
	var int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, ismult
	var unit w,wt

	ismult:=p^.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc,lab_abc,lab_abc,lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	if ismult then genpc(k_startmult) fi
	if pindex then
		evalunit(pindex)			!load test expr p to t
	fi

	wt:=pwhenthen
	while wt do
		w:=wt^.a
		fmult:=w^.nextunit<>nil
		labnextwhen:=createfwdlabel()
		if fmult then
			labstmtstart:=createfwdlabel()
		fi

		while w do
			if pindex then
				evalunit(w)
				if w^.nextunit then
					genpc(k_casejumpeq,genlabel(labstmtstart))
				else
					genpc(k_casejumpne,genlabel(labnextwhen))
				fi
				setpclmode_u(w)

			else
				if w^.nextunit then
					genjumpcond(kjumpt,w,labstmtstart)
				else
					genjumpcond(kjumpf,w,labnextwhen)
				fi
			fi
			w:=w^.nextunit
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		if pindex then
			genpc(k_free)
			setpclcat_u(pindex)
		fi

		if isref then evalref(wt^.b) else evalunit(wt^.b) fi
		if ismult then genpc(k_resetmult) fi

		if not loopsw then
			if wt^.nextunit or pelse then
				genjumpl(lab_d)
			fi
		else
			genjumpl(lab_abc)
		fi
		definefwdlabel(labnextwhen)
		wt:=wt^.nextunit
	od

	if pindex then
		genpc(k_free)				!pop index expression
		setpclcat_u(pindex)
	fi

	if pelse then
		if isref then evalref(pelse) else evalunit(pelse) fi
		if ismult then genpc(k_endmult) fi
	fi

	if loopsw then
		genjumpl(lab_abc)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi
end

proc do_switch(unit p,pindex,pwhenthen,pelse,int loopsw,isref) =
	const maxlabels = 1000
	var int minlab,maxlab,n,iscomplex,i
	var int lab_a,lab_b,lab_d, labjump, elselab, labstmt,ax,bx,ismult
	var ref pclopndrec ixopnd
	var [0..maxlabels]ref pclrec labels
	var unit w,wt

	ismult:=p^.mode<>tvoid and not loopsw

	minlab:=1000000
	maxlab:=-1000000		!highest index seen

	n:=0				!no. different values
	iscomplex:=0			!whether complex switch

	wt:=pwhenthen
	while wt do
		w:=wt^.a
		while w do		!for each when expression
			case w^.tag
			when j_makerange then
				ax:=w^.a^.value
				bx:=w^.b^.value
	dorange::
				for i:=ax to bx do
					minlab := min(i,minlab)
					maxlab := max(i,maxlab)
				od
			when j_const then		!assume int
				ax:=bx:=w^.value
				goto dorange
			else
				gerror_s("Switch when2: not const: %s",strexpr(w)^.strptr)
			esac
			w:=w^.nextunit
		od
		wt:=wt^.nextunit
	od

!at this point::
! valueset: set of all switch values
! minlab: lowest index used in valueset
! maxlab: highest index used in valueset

	n:=maxlab-minlab+1
	if n>maxlabels then
		gerror("Switch too big")
	fi

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a,lab_a,lab_a,lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	if ismult then genpc(k_startmult) fi
	evalunit(pindex)

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	genpc(k_info,genint(minlab),genint(maxlab))
	genpc(k_switch,genlabel(labjump),genlabel(elselab))

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		genpc(k_switchlab,genlabel(elselab))
		labels[i]:=pccodex
	od
	genpc(k_endswitch)

!scan when statements again, o/p statements
!	if ismult then genpc(k_startmult) fi

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt^.a
		while w do
			case w^.tag
			when j_makerange then
				ax:=w^.a^.value
				bx:=w^.b^.value
			when j_const then
					ax:=bx:=int(w^.value)
			esac
			for i:=ax to bx do
				labels[i]^.a:=genlabel(labstmt)^
			od
			w:=w^.nextunit
		od
!		evalunit(wt^.b)
		if isref then evalref(wt^.b) else evalunit(wt^.b) fi
		if ismult then genpc(k_resetmult) fi
		genjumpl((loopsw|lab_a|lab_d))
		wt:=wt^.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
!		evalunit(pelse)
		if isref then evalref(pelse) else evalunit(pelse) fi
		if ismult then genpc(k_endmult) fi
	fi

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi
end

proc do_swap(unit p,a,b) =
	if a^.tag=j_name and b^.tag=j_name then
		evalunit(a)
		evalunit(b)
		genpc(k_popmem,genmem_u(a))
		setpclcat_u(b)
		genpc(k_popmem,genmem_u(b))
		setpclcat_u(a)
	else
		evalref(a)
		evalref(b)
		genpc(k_swap)
		setpclcat_u(a)
	fi
end

proc do_select(unit p,a,b,c,int isref) =
	const maxlabels=256
	var [maxlabels]ref pclrec labels
	var int labend,labjump,n,i,elselab,labstmt,ismult
	var unit q

	ismult:=p^.mode<>tvoid

	q:=b
	n:=0
	while q do
		if n>=maxlabels then gerror("selectx: too many labels") fi
		++n
		q:=q^.nextunit
	od

	labend:=createfwdlabel()
	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then genpc(k_startmult) fi
	evalunit(a)

	genpc(k_info,genint(1),genint(n))
	genpc(k_switch,genlabel(labjump),genlabel(elselab))

	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		genpc(k_switchlab,genlabel(elselab))
		labels[i]:=pccodex
	od
	genpc(k_endswitch)

	q:=b
	i:=0
!	if ismult then genpc(k_startmult) fi
	while q do
		labstmt:=definelabel()
		++i
		labels[i]^.a:=genlabel(labstmt)^
!		evalunit(q)
		if isref then evalref(q) else evalunit(q) fi
		if ismult then genpc(k_resetmult) fi
		genjumpl(labend)
		q:=q^.nextunit
	od

	definefwdlabel(elselab)

	if isref then evalref(c) else evalunit(c) fi
!	evalunit(c)
	if ismult then genpc(k_endmult) fi

	definefwdlabel(labend)

end

proc do_print(unit p,a,b) =
	var unit q,r
	var int m,widenop, fn

	if a then
		evalunit(a)

		if ttbasetype[a^.mode]<>tref then gerror("@dev no ref") fi
		case ttbasetype[tttarget[a^.mode]]
		when tvoid then
			do_syscallproc(sysfn_print_startfile,1)
		when tc8 then
			do_syscallproc(sysfn_print_startstr,1)
		when tref then
			do_syscallproc(sysfn_print_startptr,1)
		else
			gerror("@dev?")
		esac
	else
		do_syscallproc(sysfn_print_startcon,0)
	fi

	q:=b

	case p^.tag
	when j_fprint,j_fprintln then
		if ttbasetype[q^.mode]<>tref or ttbasetype[tttarget[q^.mode]]<>tc8 then
			gerror("string expected")
		fi
		evalunit(q)
!		genpc(k_pushargs)
		do_syscallproc(sysfn_print_setfmt,1)
		q:=p^.c
	esac

	while q do
		case q^.tag
		when j_fmtitem then
			evalunit(q^.b)
			genpc(k_pushargs)
			r:=q^.a
			m:=r^.mode
		when j_nogap then
			do_syscallproc(sysfn_print_nogap,0)
			q:=q^.nextunit
			next
		else
			genpc(k_pushimm,zero_opnd)
!			setpclcat_t(ti64,0)
			r:=q
			m:=q^.mode
		esac

		widenop:=0
		case ttbasetype[m]
		when ti64 then
			fn:=sysfn_print_i64
		when ti8,ti16,ti32 then
			fn:=sysfn_print_i64
			widenop:=k_iwiden
		when tu64 then
			fn:=sysfn_print_u64
		when tu8,tu16,tu32 then
			fn:=sysfn_print_u64
			widenop:=k_uwiden
		when tr32 then
			fn:=sysfn_print_r64
			widenop:=k_fwiden
		when tr64 then
			fn:=sysfn_print_r64
		when ti128 then
			fn:=sysfn_print_i128
		when tu128 then
			fn:=sysfn_print_u128
!		when tflexstring then
!			fn:=sysfn_print_flexstr
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tfixedarray and tttarget[tttarget[m]]=tc8 then
				fn:=sysfn_print_str
			else
				fn:=sysfn_print_ptr
			fi
		when tfixedarray then
			GERROR("PRINTARRAY")
!		do_printarray(x,m)
			q:=q^.nextunit
		when tfixedrecord then
			GERROR("PRINTRECORD")
		when tc8,tc16,tc64 then
			fn:=sysfn_print_c8

		else
			gerror_s("PRINT/T=%s",strmode(m))
		esac

		evalunit(r)
		if widenop then
			genpc(widenop)
			pccodex^.mode:=m
			case widenop
			when k_iwiden then pccodex^.mode2:=ti64
			when k_uwiden then pccodex^.mode2:=tu64
			when k_fwiden then pccodex^.mode2:=tr64
			esac
		fi
!		genpc(k_pushargs)
		do_syscallproc(fn,2)
		q:=q^.nextunit
	od

	case p^.tag
	when j_println,j_fprintln then
		do_syscallproc(sysfn_print_newline,0)
	esac
	do_syscallproc(sysfn_print_end,0)

end

proc do_read(unit p,a) =
	var int m

	if a then			!format
		evalunit(a)
	else
		genpc(k_pushimm, zero_opnd)
	fi

	m:=p.mode

	if ttisinteger[m] then
		do_syscallproc(sysfn_read_i64,1)
	elsif ttisreal[m] then
		do_syscallproc(sysfn_read_r64,1)
	else
		GERROR("CAN'T READ THIS ITEM")
	fi
	genpc(k_pushretval)			!dummy op: put d0/x0 return value onto opnd stack
	setpclcat_u(p)
	if ttisreal[p^.mode] then
		makefloatopnds()
	fi
	pccodex^.mode:=p^.mode
end

proc do_readln(unit a) =
!	var ref opndrec lhs,ax

	if a then
		evalunit(a)
		if ttbasetype[a^.mode]<>tref then gerror("@dev no ref") fi
		case ttbasetype[tttarget[a^.mode]]
		when tvoid then
			do_syscallproc(sysfn_read_fileline,1)
		when tu8 then
			do_syscallproc(sysfn_read_strline,1)
		else
			gerror("rd@dev?")
		esac
	else
		do_syscallproc(sysfn_read_conline,0)
	fi
end

proc do_cprint(unit p,a,b) =
	unimpl("do_cprint")
end

proc do_cprintln(unit p,a,b) =
	unimpl("do_cprintln")
end

proc do_sprint(unit p,a,b) =
	unimpl("do_sprint")
end

proc do_sfprint(unit p,a,b) =
	unimpl("do_sfprint")
end

proc do_sread(unit p,a,b) =
	unimpl("do_sread")
end

proc do_sreadln(unit p,a,b) =
	unimpl("do_sreadln")
end

proc do_stop(unit p,a) =
	if a then
		evalunit(a)
	else
		genpc(k_pushimm,zero_opnd)
	fi
	do_syscallproc(sysfn_stop,1)
!	genpc(k_stop)
end

proc do_try(unit p,a,b) =
	unimpl("do_try")
end

proc do_except(unit p,a,b) =
	unimpl("do_except")
end

proc do_yield(unit p,a,b) =
	unimpl("do_yield")
end

proc do_raise(unit p,a,b) =
	unimpl("do_raise")
end

!proc do_callhostproc(unit p,a,b) =
!	unimpl("do_callhostproc")
!end

proc do_eval(unit p,a,b) =
	evalunit(a)
end

proc do_lambda(unit p,a,b) =
	unimpl("do_lambda")
end

proc do_andl(unit p,a,b) =
	unimpl("do_andl")
end

proc do_orl(unit p,a,b) =
	unimpl("do_orl")
end

proc do_xorl(unit p,a,b) =
	unimpl("do_xorl")
end

proc do_notl(unit p,a) =
	evalunit(a)
	if not islogical(a) then
		genpc(k_istruel)
		setpclcat_u(a)
	fi
	genpc(k_notl)
!	setpclmode_u(a)
	setpclcat_u(a)
end

proc do_istruel(unit p,a) =
	if ttisflexvar[a^.mode] then
		doflexop(p,a,"istrue")
	else
		evalunit(a)
		if not islogical(a) then
			genpc(k_istruel)
		fi
		setpclcat_u(a)
	fi
end

proc do_makelist(unit p,a,b) =
	while a do			!will be in reverse order
		evalunit(a)
		a:=a^.nextunit
	od
	if ttbasetype[p^.mode]=tslice then
		genpc(k_makeslice)
	else
		genpc(k_makelist)
	fi
end

proc do_makerange(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	genpc(k_makerange)
end

proc do_makeset(unit p,a,b) =
	unimpl("do_makeset")
end

proc do_makedict(unit p,a,b) =
	unimpl("do_makedict")
end

proc do_exprlist(unit p,a,b) =
	unimpl("do_exprlist")
end

proc do_multexpr(unit p,a,b) =
	unimpl("do_multexpr")
end

proc do_keyword(unit p,a,b) =
	unimpl("do_keyword")
end

proc do_keyvalue(unit p,a,b) =
	unimpl("do_keyvalue")
end

!proc do_assignx(unit p,a,b) =
!	unimpl("do_assignx")
!end

!proc do_callfn(unit p,a,b) =
!	unimpl("do_callfn")
!end
!
!proc do_callmfn(unit p,a,b) =
!	unimpl("do_callmfn")
!end

proc do_applyop(unit p,a,b) =
	unimpl("do_applyop")
end

proc do_applyopx(unit p,a,b) =
	unimpl("do_applyopx")
end

proc do_andand(unit p,a,b) =
	unimpl("do_andand")
end

proc do_eq(unit p,a,b) =
	unimpl("do_eq")
end

proc do_ne(unit p,a,b) =
	unimpl("do_ne")
end

proc do_lt(unit p,a,b) =
	unimpl("do_lt")
end

proc do_le(unit p,a,b) =
	unimpl("do_le")
end

proc do_gt(unit p,a,b) =
	unimpl("do_gt")
end

proc do_ge(unit p,a,b) =
	unimpl("do_ge")
end

proc do_isequal(unit p,a,b) =
	unimpl("do_isequal")
end

proc do_binx(unit p,a,b,int opc) =
!x suffix means operands can be floating point

	if ttisflexvar[a^.mode] then
		do_binflex(p,a,b,opc)
		return
	fi

	evalunit(a)
	evalunit(b)
	genpc(opc)

	setpclmode_u(p)
end

proc do_muldivx(unit p,a,b,int opc) =
!used for mul, idiv, irem; only mul can be float
	var int n

	if ttisflexvar[a^.mode] then
		if opc=k_mul and ttisinteger[b^.mode] then
			do_binflex(p,a,b,0,"muli")
		else
			do_binflex(p,a,b,opc)
		fi
		return
	fi

	if opc=k_mul and ttisreal[a^.mode] then
		evalunit(a)
		evalunit(b)
		genpc(opc)
		setpclmode_u(p)
		return
	fi

	evalunit(a)
	if b^.tag=j_const and (n:=ispoweroftwo(b^.value)) then
		if opc=k_irem then		!might do ianding/etc but goes wrong if a is neg?
GOTO ISREM
		else
			genpc((opc=k_mul|k_shlc|k_shrc),genint(n))
		fi
	else
ISREM::
		evalunit(b)
		genpc(opc)
	fi
	setpclmode_u(p)
end

proc do_bin(unit p,a,b,int opc) =
	evalunit(a)
	evalunit(b)
	genpc(opc)
	case opc
	when k_addoffset, k_suboffset then
		setpclmode_u(a)
	else
		setpclmode_u(p)
	esac

	if opc=k_subref then
		pccodex^.mode2:=a^.mode
	fi
end

proc do_shl(unit p,a,b,int opc,opcc) =
	evalunit(a)
	if b^.tag=j_const then
		genpc(opcc,genint(b^.value))
	else
		evalunit(b)
		genpc(opc)
	fi
	setpclmode_u(p)
end

proc do_shlto(unit p,a,b,int opc,opcc) =

	if b^.tag=j_const then

		if ttisinteger[a.mode] and a.tag=j_name and ttsize[a.mode]=8 then
			case opcc
			when k_shlcto then opcc:=k_shlcmemto
			when k_shrcto then opcc:=k_shrcmemto
			esac

			genpc(opcc,genmem_u(a),genint(b^.value))
		else
			evalref(a)
			genpc(opcc,genint(b^.value))
		fi
	else
		evalref(a)
		evalunit(b)
		genpc(opc)
	fi
	setpclmode_u(a)
end

proc do_setcc(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	genpc(k_setcc)
	pccodex^.cond:=p^.tag
	setpclmode_u(a)
end

proc do_setccx(unit p,a,b) =
	if p^.tag not in [j_eq, j_ne] and ttisreal[a^.mode] then
		evalunit(a)
		evalunit(b)
	else
		evalunit(a)
		evalunit(b)
	fi
	genpc(k_setcc)
	pccodex^.cond:=p^.tag
	setpclmode_u(a)
end

proc do_clamp(unit p,a,b) =
	unimpl("do_clamp")
end

proc do_index(unit p,int doref=0) =
	var ref pclopndrec ix
	var ref pclrec px
	var unit q,a,b
	a:=p^.a
	b:=p^.b

	if ttisflexvar[a^.mode] then
		do_binflex(p,a,b,0,"index")
		return
	fi

!	ix:=nil
!	if b.tag=j_const then
!		ix:=genconst_u(b)		!set up when constant index
!	fi

	case a^.tag
	when j_name then
!		if ix then
!			genpc(k_indexmemc, genmem_u(a),ix)
!		else
!			evalunit(b)
!			genpc(k_indexmem, genmem_u(a))
!		fi
		if not doref and ttbasetype[a^.mode]<>tslice then
			evalunit(b)
			genpc(k_indexmem, genmem_u(a))
		else
			genpc(k_pushaddr,genmem_u(a))
			evalunit(b)
			genpc((doref|k_indexref|k_index))
		fi

	when j_ptr then
		evalunit(a.a)
!		if ix then
!			genpc(k_indexc,ix)
!		else
			evalunit(b)
			genpc((doref|k_indexref|k_index))
!		fi
	when j_index then
		do_index(a,1)
		evalunit(b)
		genpc((doref|k_indexref|k_index))
	when j_dot then
		do_dot(a, 1)
		evalunit(b)
		genpc((doref|k_indexref|k_index))

	else
		printunit(a)
		GERROR("Can't index this array")
	esac

!set element mode

	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode		!is void anyway?

	case ttbasetype[a^.mode]
	when tslice then
		pccodex^.catmode:=tslice
	else
		pccodex^.catmode:=tfixedarray
	esac
	pccodex^.mode:=a^.mode
end

proc do_slice(unit p,a,b) =
	if not ttisflexvar[a^.mode] then
		unimpl("slice/non-flex")
	fi

	genpc(k_pushretslot)
	setpclcat_t(tscalar)

	evalunit(b^.b)			!j
	evalunit(b^.a)			!i
	evalunit(a)

	callflexhandler(a^.mode,"slice",3)
end

proc do_dotindex(unit p,a,b) =
	var int bitno

	if ttisflexvar[a^.mode] then
		do_binflex(p,a,b,0,"indexc")
		return
	fi

	evalunit(a)

!	if b^.tag=j_const then
!		bitno:=b^.value
!		genpc(k_shrc,genint(bitno))
!		setpclmode_t(ti64)
!		genpc(k_iandc,genint(1))
!		setpclmode_t(ti64)
!	else
		evalunit(b)
		genpc(k_dotindex)
!	fi
end

proc do_dotslice(unit p,a,b) =
	var unit x,y
	var int bitno

	if ttisflexvar[a^.mode] then
		do_slice(p,a,b)
		return
	fi

	evalunit(a)

	if b^.tag<>j_makerange then
		gerror("dotslice not range")
	fi
	x:=b^.a
	y:=b^.b

!	if x^.tag=j_const and y^.tag=j_const then
!		bitno:=b^.value
!		genpc(k_shrc,genint(bitno))
!		setpclmode_t(ti64)
!		genpc(k_iandc,genint(1))
!		setpclmode_t(ti64)
!	else
		evalunit(b.a)
		evalunit(b.b)
		genpc(k_dotslice)
!	fi
end

proc do_anddotindex(unit p,a,b) =
	var int bitno
	evalunit(a)

	if b^.tag=j_const then
		bitno:=b^.value
		genpc(k_iandc,genint(1<<bitno))
		setpclmode_t(ti64)
	else
		evalunit(b)
		genpc(k_anddotindex)
	fi
end

proc do_anddotslice(unit p,a,b) =
	unimpl("do_anddotslice")
end

proc do_keyindex(unit p,a,b) =
	unimpl("do_keyindex")
end

proc do_dot(unit p, int doref) =
	var unit a,pname
	var int offset
	a:=p^.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		if offset<>-1 then
			offset+:=p.offset
			a:=pname
!			genpc(k_pushaddr,genmem_u(pname))
!			goto dorest
		fi
	else
		offset:=p^.offset
	fi

	case a^.tag
	when j_name then
		genpc(k_pushaddr,genmem_u(a))
	when j_ptr then
		evalunit(a.a)
	when j_dot then
		do_dot(a, 1)
	when j_index then
		do_index(a,1)
	else
		printunit(a)
		GERROR("Can't access this record")
	esac

dorest::

	genpc((doref|k_dotref|k_dot),genint(offset))

	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode
	pccodex^.mode2:=p^.mode

	pccodex^.catmode:=tfixedrecord
	pccodex^.mode:=a^.mode
end

function checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions,
!or -1 when offsets cannot be combined
	var int offset

	case p.tag
	when j_dot then
		offset:=checkdotchain(p.a,pname)
		if offset=-1 then
			return -1
		else
			return p.offset+offset
		fi

!	when j_name then				!would be last in chain; owner knows offset
!		pname:=p
!		return 0
!	else
!		return -1
	else							!anything else, is the start expression
		pname:=p
		return 0
	esac
return 0
end

proc do_dotattr(unit p,a,b) =
	unimpl("do_dotattr")
end

proc do_atan2(unit p,a,b) =
	unimpl("do_atan2")
end

proc do_power(unit p,a,b) =
	if ttisreal[a^.mode] then
		evalunit(b)			!reverse order, as will call c function
		evalunit(a)
	else
		evalunit(a)
		evalunit(b)
	fi
	genpc(k_power)
	setpclmode_u(a)
end

proc do_ptr(unit p,a,b) =

	if a^.tag in [j_add, j_sub] and a^.b^.tag=j_const then
!CPL "PTR+OFFSET"
		evalunit(a.a)
		genpc(k_pushptr,genint((a.tag=j_add|a.b.value|-a.b.value)))
	else
		evalunit(a)
		genpc(k_pushptr,genint(0))
	fi

	setpclcat_u(p)
end

proc do_addrof(unit p,a) =
	evalref(a)
!	unimpl("do_addrof")
end

proc do_convert(unit p,a,b) =
	var int opc

if p^.popflag then
	evalunit(a)
	return
fi

	case p^.tag
	when j_makelist, j_makeset then
	else
		case p^.opcode
		when c_ichartostring then
			genpc(k_pushretslot)
			setpclcat_t(tscalar)

			genpc(k_pushimm,genint((a^.tag=j_const|a^.slength|-1)))
			evalunit(a)

			callflexhandler(tflexstring,"make",2)
			return
		esac

		evalunit(a)

		if p^.opcode in [c_uwiden, c_iwiden] and \
			a^.tag in [j_index,j_dot,j_ptr] and ttsize[p^.mode]=8 then
			return
		fi

		switch p^.opcode
		when c_uwiden   then opc:=k_uwiden
		when c_iwiden   then opc:=k_iwiden
		when c_ufloat   then opc:=k_ufloat
		when c_ifloat   then opc:=k_ifloat
		when c_ufix     then opc:=k_ufix
		when c_ifix     then opc:=k_ifix
		when c_softtruncate then opc:=k_softtruncate
		when c_truncate then opc:=k_truncate
		when c_fnarrow  then opc:=k_fnarrow
		when c_fwiden   then opc:=k_fwiden
		when c_none then
			GERROR("BLOCKPCL/C_NONE")
		else
			cpl convnames[p^.opcode]
			gerror("No PCL convert op")
		endswitch

!		genpc_opt(k_convert,p^.mode)
		genpc(opc)
!		setpclmode_u(p)
		pccodex^.mode:=a^.mode
		pccodex^.mode2:=p^.mode
!		pccodex^.mode2:=ttbasetype[a^.mode]
		return
	esac
	unimpl("CONVERT/MAKELIST/CONSTR")
end

proc do_convertref(unit p,a,b) =
	unimpl("do_convertref")
end

proc do_autocast(unit p,a,b) =
	unimpl("do_autocast")
end

proc do_typepun(unit p,a,b) =
	evalunit(a)
	genpc(k_typepun)
	pccodex^.mode:=a^.mode
	pccodex^.mode2:=p^.mode
end

proc do_typeconst(unit p) =
	genpc(k_pushimm,genint(p^.value))
end

proc do_operator(unit p,a,b) =
	unimpl("do_operator")
end

proc do_upper(unit p,a,b) =
	unimpl("do_upper")
end

proc do_unaryx(unit p,a,int opc) =
!	if gettypecode_u(a)='R' then
!		evalunitf(a)
!	else
		evalunit(a)
!	fi
	genpc(opc)
	setpclmode_u(p)
end

proc do_unary(unit p,a,int opc) =
	evalunit(a)
	genpc(opc)
	setpclmode_u(p)
end

proc do_chr(unit p,a,b) =
	unimpl("do_chr")
end

proc do_asc(unit p,a,b) =
	unimpl("do_asc")
end

proc do_maths(unit p,a,int opc) =
	evalunit(a)
	do_syscallproc(opc,1)

	genpc(k_pushretval)			!dummy op: put d0/x0 return value onto opnd stack
	setpclcat_u(p)
	if ttisreal[p^.mode] then
		makefloatopnds()
	fi
	pccodex^.mode:=p^.mode
end

proc do_sqr(unit p,a,b) =
!	if gettypecode_u(a)='R' then
!		evalunitf(a)
!	else
		evalunit(a)
!	fi
	genpc(k_sqr)
	setpclmode_u(a)
end

proc do_sign(unit p,a,b) =
!	if gettypecode_u(a)='R' then
!		evalunitf(a)
!	else
		evalunit(a)
!	fi
!	genpc_op(k_sign)
	genpc(k_sign)
	setpclmode_u(a)
end

proc do_fmod(unit p,a,b) =
	unimpl("do_fmod")
end

proc do_lwb(unit p,a) =
	doflexop(p,a,"lwb")
end

proc do_upb(unit p,a) =
	if ttbasetype[a^.mode]=tslice then
		evalunit(a)
		genpc(k_sliceupb)
		pccodex^.mode:=a^.mode
	else
		doflexop(p,a,"upb")
	fi
end

proc do_len(unit p,a) =
	if ttbasetype[a^.mode]=tslice then
		evalunit(a)
		genpc(k_slicelen)
	else
		doflexop(p,a,"len")
	fi
end

proc do_bounds(unit p,a) =
	unimpl("do_bounds")
end

proc do_bitwidth(unit p,a) =
	unimpl("do_bitwidth")
end

proc do_bytesize(unit p,a) =
	unimpl("do_bytesize")
end

proc do_typeof(unit p,a,b) =
	unimpl("do_typeof")
end

proc do_typestr(unit p,a,b) =
	unimpl("do_typestr")
end

proc do_sliceptr(unit p,a) =
	evalunit(a)
	genpc(k_sliceptr)
end

proc do_minvalue(unit p,a,b) =
	unimpl("do_minvalue")
end

proc do_maxvalue(unit p,a,b) =
	unimpl("do_maxvalue")
end

proc do_incr(unit p,a) =
	if a^.tag=j_name then
		genpc((p^.tag=j_incr|k_incr|k_decr), genmem_u(a))
	else
		evalref(a)
		genpc((p^.tag=j_incr|k_incrptr|k_decrptr))
	fi
!	setpclmode_u(a)
	if ttbasetype[a^.mode]=tref then
		setpclmode_u(a)
	else
		setpclcat_u(a)
	fi
end

proc do_incrx(unit p,a, int opc) =
	evalref(a)
!	genpc_op(opc)
	genpc(opc)
	if ttbasetype[a^.mode]=tref then
		setpclmode_u(a)
	else
		setpclcat_u(a)
	fi
end

proc do_binto(unit p,a,b,int opc) =

	if ttisflexvar[a^.mode] then
		do_bintoflex(p,a,b,opc)
		return
	fi

	if opc in [k_addto,k_subto, k_iandto, k_iorto, k_ixorto] and 
		ttisinteger[a.mode] and a.tag=j_name and ttsize[a.mode]=8 then
		evalunit(b)
		case opc
		when k_addto then opc:=k_addmemto
		when k_subto then opc:=k_submemto
		when k_iandto then opc:=k_iandmemto
		when k_iorto then opc:=k_iormemto
		when k_ixorto then opc:=k_ixormemto
		esac

		genpc(opc,genmem_u(a))
	else
		evalref(a)
		evalunit(b)
		genpc(opc)
	fi

	setpclmode_u(a)
end

!proc do_subto(unit p,a,b) =
!	unimpl("do_subto")
!end
!
!proc do_multo(unit p,a,b) =
!	unimpl("do_multo")
!end
!
!proc do_divto(unit p,a,b) =
!	unimpl("do_divto")
!end
!
!proc do_iandto(unit p,a,b) =
!	unimpl("do_iandto")
!end
!
!proc do_iorto(unit p,a,b) =
!	unimpl("do_iorto")
!end
!
!proc do_ixorto(unit p,a,b) =
!	unimpl("do_ixorto")
!end
!
!proc do_minto(unit p,a,b) =
!	unimpl("do_minto")
!end
!
!proc do_maxto(unit p,a,b) =
!	unimpl("do_maxto")
!end

proc do_unaryto(unit p,a,int opc) =
	evalref(a)
	genpc(opc)
	setpclmode_u(a)
end

!proc do_absto(unit p,a,b) =
!	unimpl("do_absto")
!end
!
!proc do_inotto(unit p,a,b) =
!	unimpl("do_inotto")
!end

!proc do_isvoid(unit p,a,b) =
!	unimpl("do_isvoid")
!end
!
!proc do_isdef(unit p,a,b) =
!	unimpl("do_isdef")
!end
!
!proc do_isint(unit p,a,b) =
!	unimpl("do_isint")
!end
!
!proc do_isreal(unit p,a,b) =
!	unimpl("do_isreal")
!end
!
!proc do_isstring(unit p,a,b) =
!	unimpl("do_isstring")
!end
!
!proc do_islist(unit p,a,b) =
!	unimpl("do_islist")
!end
!
!proc do_isrecord(unit p,a,b) =
!	unimpl("do_isrecord")
!end
!
!proc do_isarray(unit p,a,b) =
!	unimpl("do_isarray")
!end
!
!proc do_isset(unit p,a,b) =
!	unimpl("do_isset")
!end
!
!proc do_ispointer(unit p,a,b) =
!	unimpl("do_ispointer")
!end
!
!proc do_ismutable(unit p,a,b) =
!	unimpl("do_ismutable")
!end

proc do_cvlineno(unit p,a,b) =
	genpc(k_pushimm,genint(p^.lineno iand 16777215))
!	setpclmode_t(ti64)
end

!proc do_cvstrlineno(unit p,a,b) =
!	unimpl("do_cvstrlineno")
!end
!
proc do_cvmodulename(unit p,a,b) =
	genpc(k_pushimm,genstrimm(moduletable[p^.moduleno].name))
!	setpclmode_t(trefchar)
end

proc do_cvfilename(unit p,a,b) =
!	genpc(k_pushconst,genstrimm(sourcefilenames[p^.lineno>>24]))
	genpc(k_pushimm,genstrimm(sourcefilenames[p^.lineno>>24]))
!	genpc(k_pushimm,genstrimm("ABC"))
!	setpclmode_t(trefchar)
end

!proc do_cvfunction(unit p,a,b) =
!	unimpl("do_cvfunction")
!end
!
!proc do_cvdate(unit p,a,b) =
!	unimpl("do_cvdate")
!end
!
!proc do_cvtime(unit p,a,b) =
!	unimpl("do_cvtime")
!end
!
!proc do_cvversion(unit p,a,b) =
!	unimpl("do_cvversion")
!end
!
!proc do_cvtypename(unit p,a,b) =
!	unimpl("do_cvtypename")
!end
!
!proc do_cvtargetbits(unit p,a,b) =
!	unimpl("do_cvtargetbits")
!end
!
!proc do_cvtargetsize(unit p,a,b) =
!	unimpl("do_cvtargetsize")
!end
!
!proc do_cvtargetcode(unit p,a,b) =
!	unimpl("do_cvtargetcode")
!end

proc do_assignblock(unit p,a,b) =
!fstore=1 when result is needed
!method used is::
! load ref to lhs
! load ref to rhs
! do block xfer, not using the stack

!CPL "ASSIGN BLCK",STRMODE(A^.MODE),STRMODE(B^.MODE)

	evalref(a)
	evalref(b)

	genpc(k_copyblock)
	pccodex^.mode:=a^.mode
end

proc do_callff(unit p,a,b,ref strec d,int fncall)=
!ref opndrec result,ax,callres,sx
var unit q
var int nargs,nparams,retmode,nbytes,retsize,i,n,allsimple
var int lab1,lab2,floatmap,mask
var [maxparams]ref strec paramlist
var [maxparams]unit params
var [maxparams]byte widenfloat
![maxparams]byte isfloat
!int isvariadic
var [256]char str

retmode:=p^.mode

!CPL =d^.varparams

!	if fncall then
!		genpc(k_pushretslot)
!		setpclcat_u(p)
!		if gettypecode_u(p)='R' then
!			makefloatopnds()
!		fi
!		pccodex^.mode:=p^.mode
!	fi

	nparams:=0
	floatmap:=0
	mask:=1
	while b do
		params[++nparams]:=b
		widenfloat[nparams]:=0
		if ttisreal[b^.mode] then
			floatmap ior:=mask

!NEED TO ONLY EXPAND FOR VARIADIC FOR EXTRA PARAMS where ... goes
			if ttsize[b.mode]=4 and d.varparams then
				widenfloat[nparams]:=1
			fi
		fi
		mask<<:=1

		b:=b^.nextunit
	od
	for i:=nparams downto 1 do
		evalunit(params[i])
		if widenfloat[i] then
			genpc(k_fwiden)
			pccodex.mode2:=tr64
			pccodex.mode:=tr32
		fi
	od
	genpc(k_pushargs)

	case a^.tag
	when j_name then
!		genpc(k_callff, genmemaddr_u(a),genint(floatmap))
		genpc(k_callff, genint(floatmap),genmemaddr_u(a))
	else
		evalunit(a^.a)
		genpc(k_callptrff,genint(floatmap))
	esac
	pccodex^.a.nargs:=nparams
	pccodex^.isfunction:=fncall
	pccodex^.isvariadic:=d^.varparams

	if fncall then
		genpc(k_pushffretval)
		setpclcat_u(p)
		if ttisreal[p^.mode] then
			makefloatopnds()
		fi
		pccodex^.mode:=p^.mode
	fi
end

proc do_recase(unit p,a)=
	var unit q,wt,w
	var int destlab

	if casedepth=0 then
		gerror("recase outside case stmt")
	fi
	q:=casestmt[casedepth]

	destlab:=0

	wt:=q^.b
	while wt do
		w:=wt^.a
		while w do
			if w^.tag=j_const and isintmode(w^.mode) and w^.value=a^.value then
				destlab:=w^.whenlabel
				exit all
			fi
			w:=w^.nextunit
		od
		wt:=wt^.nextunit
	od

	if destlab=0 then
		genjumpl(caseelse[casedepth])
	else
		genjumpl(destlab)
	fi
end

proc do_binflex(unit p,a,b,int opc,ichar opname=nil) =
	genpc(k_pushretslot)
	setpclcat_t(tscalar)

	evalunit(b)
	evalunit(a)

	if opc then
		case opc
		when k_add,k_sub,k_mul,k_div,k_idiv then
			callflexhandler(a^.mode,pclnames[opc]+2,2)
!		when k_mul then
!			if ttisinteger[b^.mode] then
!				callflexhandler(a^.mode,"muli",2)
!			else
!				recase k_add
!			fi

		else
			gerror("binflex/opc not supported")
		esac
	else
		callflexhandler(a^.mode,opname,2)
	fi
end

proc do_bintoflex(unit p,a,b,int opc) =

	evalunit(b)
	evalunit(a)

	if opc=k_addto and ttisinteger[b^.mode] and a^.mode=tflexstring then
		callflexhandler(a^.mode,"addtoc",2)
	elsecase opc
	when k_addto,k_subto,k_multo,k_divto,k_idivto then
		callflexhandler(a^.mode,pclnames[opc]+2,2)		!skip the "k_"
	else
		gerror("bintoflex/opc not supported")
	fi
end

proc doflexop(unit p,a,ichar opname)=
!do specialist unary ops
	genpc(k_pushretslot)
	if ttisflexvar[p^.mode] then
		setpclcat_t(tscalar)
	else
		setpclcat_u(p)
	fi

	evalunit(a)

	callflexhandler(a^.mode,opname,1)
end

proc do_comparejumpflex(int cond,unit lhs,rhs,int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	genpc(k_pushretslot)
	setpclcat_t(tscalar)

	evalunit(rhs)
	evalunit(lhs)

	case cond
	when j_eq, j_ne then
		callflexhandler(lhs^.mode,"equal",2)
		genpc((cond=j_eq|k_jumptrue|k_jumpfalse),genlabel(lab))
	else
		callflexhandler(lhs^.mode,"cmp",2)
		genpc(k_pushimm,zero_opnd)
		genpc_condlab(k_jumpcc,cond,lab)
	esac
	setpclmode_t(ti64)
end

proc do_assignflex(unit p,a,b, int fstore) =
!fstore=1 when result is needed

	if p^.tag in [j_deepcopy, j_deepcopyx] then
		if ttisflex[a^.mode] then
			genpc(k_pushretslot)
			setpclcat_t(tscalar)
			evalunit(b)
			callflexhandler(a^.mode,"dupl",1)
		else
			gerror("::= not approrpiate")
		fi
	else
		evalunit(b)
	fi

	switch a^.tag
	when j_name then
		genpc((fstore|k_storemem|k_popmem),genmem_u(a))
		setpclcat_u(a)
		return
	when j_index,j_dotindex then
		if fstore then
			gerror("str[]:=x.store?")
		fi
		evalunit(a^.b)
		evalunit(a^.a)

		callflexhandler(a^.a^.mode,(a^.tag=j_index|"indexto"|"indextoc"),3)	
!	when j_ptr then
!		evalunit(a^.a)
!		genpc((fstore|k_storeptr|k_popptr))
!	when j_if, j_longif, j_case, j_switch, j_select then
!		evalref(a)
!		genpc((fstore|k_storeptr|k_popptr))
!!		gerror("assign to if/select/etc")
!	when j_dotindex then
!		evalref(a^.a)
!		evalunit(a^.b)
!		if fstore then
!			gerror("storedotix?")
!		else
!			genpc(k_popdotindex)
!		fi
	else
		cpl jtagnames[a^.tag]
		gerror("Can't assign to flex expr")
	end switch

!	setpclcat_u(a)
end

proc do_assem(unit p,a) =
	genpc(k_assem,genassem_u(p))
	setpclcat_u(p)
end

proc pushrhs(unit a)=
if a=nil then return fi
pushrhs(a^.nextunit)
evalunit(a)
end

proc do_multassign(unit a,b)=
	var unit p
	var int nlhs,nrhs
	var ref strec d

	nlhs:=a^.length

	if b^.tag=j_callfn then
		evalunit(b)
		if b^.a^.tag<>j_name then
			gerror("multassign from fn: not simple fn")
		fi
		d:=b^.a^.def
		nrhs:=d^.nretvalues

	else
		nrhs:=b^.length
		pushrhs(b^.a)			!push rhs elements in right-to-left order

	fi

	a:=a^.a					!point to elements of makelist
	repeat
		switch a^.tag
		when j_name then
			genpc(k_popmem,genmem_u(a))
		when j_index, j_slice,j_dot then
			evalref(a)
			genpc(k_popptr,genint(0))
		when j_ptr then
			evalunit(a^.a)
			genpc(k_popptr,genint(0))
		when j_if, j_longif, j_case, j_switch, j_select then
			evalref(a)
			genpc(k_popptr,genint(0))
		when j_dotindex then
			evalref(a^.a)
			evalunit(a^.b)
			genpc(k_popdotindex)
		else
			cpl jtagnames[a^.tag]
			gerror("Bad mult assign element")
		end switch

		setpclcat_u(a)

		a:=a.nextunit
	until a=nil

	for i:=nlhs+1 to nrhs do
		genpc(k_free)
		setpclcat_t(d^.modelist[i])
		if ttisreal[d^.modelist[i]] then
			makefloatopnds()
		fi
	od

end

function isshortconst(unit p)int=

	if p^.tag=j_const and ttisinteger[p^.mode] and ttsize[p^.mode]<=8 and
		p^.value in int32.minvalue..int32.maxvalue then
			return 1
	fi
	return 0
end

function isshortmem(unit p)int=

	if p^.tag=j_name and ttisnumeric[p^.mode] and ttsize[p^.def^.mode]=8 and
		p^.def^.nameid in [frameid,paramid,staticid] then
			return 1
	fi
	return 0
end

proc do_popindex(unit p,a,b,c, int opc)=
!a[b]:=c
!	var ref pclopndrec ix
	var ref pclrec px
	var unit q

	evalunit(c)

	case a^.tag
	when j_name then
		genpc(k_pushaddr,genmem_u(a))
		evalunit(b)
		genpc(opc)

	when j_ptr then
		evalunit(a.a)
		evalunit(b)
		genpc(opc)

	when j_index then
!		do_index(a,a.a,a.b,1)
		do_index(a,1)
		evalunit(b)
		genpc(opc)
	when j_dot then
!		do_dot(a, a.a,a^.offset,1)
		do_dot(a, 1)
		evalunit(b)
		genpc(opc)
	else
		printunit(a)
		GERROR("Can popindex this array")
	esac

!set element mode
	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode		!is void anyway?

	case ttbasetype[a^.mode]
	when tslice then
		pccodex^.catmode:=tslice
	else
		pccodex^.catmode:=tfixedarray
	esac
	pccodex^.mode:=a^.mode
end

proc do_popdot(unit p,a,c, int offset,opc)=
!a.offset:=c
	var ref pclrec px
	var unit pname
	var int newoffset

	evalunit(c)

	if dodotchains then
		pname:=nil
		newoffset:=checkdotchain(a,pname)
		if newoffset<>-1 then
!CPL "POPDOTSIMPLE DOT CHAIN, combined offset =",=newoffset,=offset,=pname!.def.name
			offset+:=newoffset
			a:=pname

!			genpc(k_pushaddr,genmem_u(pname))
!			goto dorest
		fi
	else

	fi


	case a^.tag
	when j_name then
		genpc(k_pushaddr,genmem_u(a))

	when j_ptr then
		evalunit(a.a)

	when j_dot then
!		do_dot(a, a.a,a^.offset,1)
		do_dot(a, 1)

	when j_index then
		do_index(a,1)

	else
		printunit(a)
		GERROR("Can't popdot this record")
	esac

dorest::
	genpc(opc,genint(offset))

!set element mode
	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode		!is void anyway?

	pccodex^.catmode:=tfixedrecord
	pccodex^.mode:=a^.mode
end
=== mm_genmcl.m 18/35 ===
import msys
import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_tables
import mm_lib
import mm_lex

import mm_libmcl as mm
import mm_libpcl
import mm_mcldecls
import mm_diags

!const fshowpcl=1
const fshowpcl=0
!const fshowopndstack=1
const fshowopndstack=0

var [pclnames.len,tvoid..tvariant]ref proc(ref pclrec) handlertable

var ref pclrec currpclrec
var ref pclopndrec aa,bb

var int swmin,swmax

global proc codegen_mcl=
var ref pclrec p

inithandlers()

mclinit()

p:=allpclcode

gencomment_mc("Starting PCL->MCL:")

while p do
	convertpcl(p)
	p:=p^.nextpcl
od
gencomment_mc("Finished PCL->MCL:")

if noperands then
	CPL "OPERAND STACK -",NOPERANDS,"LEFT ON STACK"
fi

genabsneg()
genstringtable()
genrealtable()
genfunctiondata()

gensysfntable()


allmclcode:=mccode
end

proc convertpcl(ref pclrec p)=
	var [1256]char str
	var ichar ss

	if fshowpcl and fdebugcompiler then
		ss:=strpcl(p)
		if strlen(ss)<str.len then
!			sprintf(&.str,"- - - - - - - - - - - -%s",ss)
			print @&.str,"- - - - - - - - - - - -",,ss
		else
!			sprintf(&.str,"- - - - - - - - - - - -TOO LONG: %s",pclnames[p^.opcode])
			print @&.str,"- - - - - - - - - - - -TOO LONG:",pclnames[p^.opcode]
		fi
		gencomment_mc(&.str)
	fi

!CPL PCLNAMES[P.OPCODE],P.LINENO IAND 16777215, SOURCEFILENAMES[P.LINENO>>24]

	currpclrec:=p				!for use by pc_comment

	mlineno:=p^.lineno

	aa:=&p^.a
	bb:=&p^.b
	handlertable[p^.opcode,p^.catmode]^(p)

	case p^.opcode
	when k_comment,k_blank,k_label,k_labelname then
	else
		if fshowopndstack and fdebugcompiler then
			showopndstack()
!			showopndstack_S()
		fi
	esac
end

proc inithandlers=
	var ichar name

	var int n:=$get_nprocs()

	for i to n do
		name:=$get_procname(i)
		if eqbytes(name,"pc_",3) then
			dohandler(name,$get_procaddr(i))
		fi
	od

	for i:=1 to handlertable.len do
		for t:=tvoid to tvariant do
			if not handlertable[i,t] then
				handlertable[i,t]:=cast(&pc_dummy)
			fi
		od
	od

!	var int n:=m_getnprocs()
!
!	for i to n do
!		name:=m_getprocname(i)
!		if eqbytes(name,"pc_",3) then
!			dohandler(name,m_getprocaddr(i))
!		fi
!	od
!
!	for i:=1 to handlertable.len do
!		for t:=tvoid to tvariant do
!			if not handlertable[i,t] then
!				handlertable[i,t]:=cast(&pc_dummy)
!			fi
!		od
!	od
end

proc unimpl(ichar mess)=
!doesn't need a handler, but used as default handler for all opcodes
!for which its pc-handler doesn't exist
var [256]char str

	strcpy(&.str,"Unimpl: ")
	strcat(&.str,mess)
	gencomment_mc(&.str)
!	gerror_s("Unimplemented PCL opcode: %s",mess)

end

proc pc_dummy(ref pclrec p) =
var [256]char str

!sprintf(&.str,"%s_%s",pclnames[p^.opcode],stdtypenames[p^.catmode])
fprint @&.str,"#_#",pclnames[p^.opcode],stdtypenames[p^.catmode]

CPL "UNIMPL:",&.str,p^.lineno iand 16777215,sourcefilenames[p^.lineno>>24]
unimpl(&.str)
end

proc pc_comment(ref pclrec p) =
gencomment_mc(p.a.svalue)
!unimpl("comment")
end

proc pc_blank(ref pclrec p) =
	gencomment_mc(nil)
end

proc pc_label(ref pclrec p) =
	genmc(m_label,genlabel_mc(aa^.labelno))
!	unimpl("k_label")
end

proc pc_labelname(ref pclrec p) =
	genmc(m_labelname,genstrimm_mc(aa^.svalue))
end

proc pc_startmult(ref pclrec p) =
!	gencomment_mc("startmult")
	pushalloperands()			!make all current operands T
end

proc pc_resetmult(ref pclrec p) =
	var int reg

	if opndkind[noperands]='I' then
		reg:=opndstack[noperands]
		if reg=rtos then
			gerror("RESETMULT/I/TOS?")
		fi
		if noperands>1 and opndstack[noperands-1]<>rtos then
			gerror("RESETMULT/I/STACK ERROR")
		fi
		if reg<>r0 then
			genmc(m_mov, genreg(r0), genreg(reg))
			freereg(reg)
			regset[r0]:=1
			opndstack[noperands]:=r0
		fi
	else
		pushalloperands()				!should push just the one
	fi

	if p^.opcode=k_resetmult then
		--noperands
		if regset[r0] then freereg(r0) fi
	fi

end

proc pc_endmult(ref pclrec p) =
	pc_resetmult(p)
end

proc pc_pushall(ref pclrec p) =
	unimpl("k_pushall")
end

proc pc_procentry(ref pclrec p) =
	var ref opndrec ax
	var int iscallback,nparams,np,offset,reg

	framebytes:=bb^.fbytes
	parambytes:=bb^.pbytes

	if aa^.optype=strimm_opnd then
		genmc(m_labelname,genstrimm_mc(aa^.svalue))
		return
	fi

	currproc:=aa^.def
	iscallback:=iscallbackfn(currproc)

	genmc(m_labelname,genmemaddr_d_mc(currproc))
	genmc(m_label,genlabel_mc(++labelno))
	currproc.index:=labelno

	if p^.isglobal then
		mccodex^.isglobal:=p^.isglobal
	fi

	if framebytes or parambytes then
		genmc(m_push,dframeopnd)
		genmc(m_mov, dframeopnd,dstackopnd)

		if framebytes then
			pushstack_mc(framebytes)
		fi
	fi

	if iscallback then

		nparams:=parambytes/8			!only works for callbacks, as no wide params
		np:=min(4,nparams)
		offset:=16
		reg:=r10
		for i:=1 to np do
			ax:=genindex(areg:rframe,size:8,offset:offset)
			genmc(m_mov, ax, genreg(reg,8))
			offset+:=8
			++reg
		od

		genmc_sys(sysfn_pushcallback)
	fi
end

proc pc_procexit(ref pclrec p) =

	var int iscallback

	iscallback:=iscallbackfn(currproc)

!	if currproc^.nretvalues<>noperands then
!		gencomment_mc("procexit/opndstack mismatch")
!	fi

	if framebytes or parambytes then
		if framebytes then
			popstack_mc(framebytes)
		fi
		genmc(m_pop, dframeopnd)

		if iscallback then
			genmc_sys(sysfn_popcallback)
		fi
	
		if parambytes and not iscallback then
			genmc(m_retn,genint_mc(parambytes))
		else
			genmc(m_ret)
		fi	
	else
		if iscallback then
			genmc_sys(sysfn_popcallback)
		fi

		genmc(m_ret)
	fi

!	if currproc^.nretvalues then
!		popopnd()
!	fi

end

proc pc_zstatic(ref pclrec p) =
	var int m
	var ref strec d:=p^.a.def

	m:=d^.mode
	setsegment('Z',p^.align)
	genmc(m_labelname,genmemaddr_d_mc(d))
	genmc(m_resb,genint_mc(ttsize[m]))
!	setsegment('C')
end

proc pc_istatic(ref pclrec p) =
	setsegment('I',p^.align)
	genmc(m_labelname,genmemaddr_d_mc(p^.a.def))
end

proc pc_equiv(ref pclrec p) =
	var ref opndrec ax

	ax:=genmemaddr_d_mc(p^.a.def)
	case bb^.optype
	when intimm_opnd then
		genmc(m_define,ax,genint_mc(bb^.value))
	when mem_opnd then
		genmc(m_define,ax,genmemaddr_d_mc(p.b.def))
	else
CPL OPNDNAMES[BB^.OPTYPE]
GERROR("PC_EQUIV")
		genmc(m_define,ax,genint_mc(bb^.value))
	esac
end

!proc pc_dupl_flex(ref pclrec p)=
!	callflexhandler_mc(p,"dupl",0)		!0 official args
!	unimpl("DUPL/FLEX")
!end

proc pc_initmemz_flex(ref pclrec p) =
	newstackopnd_d8()
gerror("callflex init")
!	callflexhandler_mc(p,"new",0)
	getopnds(1)
	genmc_storemem_d8(p^.a.def)
	popopnd()
end

proc pc_pushmem_d8(ref pclrec p) =
	newopnd_d8()
	genmc_loadmem_d8(aa^.def)
end

proc pc_dpushmem_d8(ref pclrec p) =
!	newopnd_d8()
!	genmc_loadmem_d8(aa^.def)

	pushalloperands()
	newopnd_d8()			!set up to some dummy register
	genmc(m_push,genmem_d_mc(aa^.def))
	freereg(opndstack[noperands])
	opndstack[noperands]:=rtos
end

proc pc_pushmem_x8(ref pclrec p) =
	newopnd_x8()
	genmc_loadmem_x8(aa^.def)
end

proc pc_pushmem_d124(ref pclrec p) =
	newopnd_d8()
	genmc_loadmem_d124(aa^.def,ttbasetype[p^.mode])
end

proc pc_pushmem_d16(ref pclrec p) =
	newopnd_d16()
	genmc_loadmem_d16(aa^.def)
end

proc pc_pushmem_x4(ref pclrec p) =
	newopnd_x8()
	genmc_loadmem_x4(aa^.def)
end

proc pc_pushmem_flex(ref pclrec p) =
	newopnd_d8()
	genmc_loadmem_d8(aa^.def)

	genmc(m_inc,changeopndsize(genopndind(xa),4))

end

proc pc_pushconst_i64_u64_c64_ref_c8(ref pclrec p) =
	newopnd_d8()
	genmc_loadint_d8(aa^.code^.value)
end

proc pc_dpushconst_d8_d124(ref pclrec p) =
	pushalloperands()
	newopnd_d8()			!set up to some dummy register
	genmc(m_push,genint_mc(aa^.code^.value))
	freereg(opndstack[noperands])
	opndstack[noperands]:=rtos
end

proc pc_pushconst_i128_u128(ref pclrec p) =
	newopnd_d16()
	genmc_loadint_d16(aa^.code^.qvalue^.lower,aa^.code^.qvalue^.upper)
end

proc pc_pushconst_r64(ref pclrec p) =
!r64 to normal operand
	newopnd_d8()
	genmc_loadreal_d8(aa^.code^.xvalue)
end

proc pc_pushconst_r32(ref pclrec p) =
!r64 to normal operand
	newopnd_d8()
	genmc_loadreal_d4(aa^.code^.xvalue)
end

proc pc_pushconst_x8(ref pclrec p) =
!r64 to float operand
	newopnd_x8()
	genmc_loadreal_x8(aa^.code^.xvalue)
end

proc pc_pushconst_x4(ref pclrec p) =
!r64 to float operand
	newopnd_x8()
	genmc_loadreal_x4(aa^.code^.xvalue)
end

proc pc_pushconst_ref(ref pclrec p) =
	newopnd_d8()
!CPL "PUSHCONSTREF1"

	if p^.mode=trefchar and aa^.code^.value and aa^.code^.isastring then
		genmc_loadstr(aa^.code^.svalue,aa.code.slength)
	else
		genmc_loadint_d8(aa^.code^.value)
	fi
end

proc pc_pushimm(ref pclrec p) =
	newopnd_d8()
	case aa^.optype
	when intimm_opnd then genmc_loadint_d8(aa^.value)
	when wordimm_opnd then genmc_loadword_d8(aa^.uvalue)
	when realimm_opnd then
		if aa.size=4 then
			genmc_loadreal_d4(aa^.xvalue)
		else
			genmc_loadreal_d8(aa^.xvalue)
		fi
	when strimm_opnd then genmc_loadstr(aa^.svalue,aa.code.slength)
	else
		unimpl("k_pushimm")
	esac
end

proc pc_pushargs(ref pclrec p) =
	pushalloperands()
end

proc pc_pushaddr(ref pclrec p) =
	newopnd_d8()
	genmc_loadmemaddr(aa^.def)
end

proc pc_pushptr_d8_proc(ref pclrec p) =
	getopnds(1)
	genmc_loadptr_d8(aa^.value)
end

proc pc_pushptr_d16(ref pclrec p) =
	getopnds(1)
	genmc_loadptr_d16(aa^.value)
end

proc pc_pushptr_x8(ref pclrec p) =
	var ref opndrec px,fx
	getopnds(1)
	px:=genopndind(xa)

	newopnd_x8()
	fx:=genopnd(xa)

	genmc(m_movq,fx,px)

	swapopnds(1)
	popopnd()
end

proc pc_pushptr_x4(ref pclrec p) =
	var ref opndrec px,fx
	getopnds(1)
	px:=genopndind(xa)
	px:=changeopndsize(px,4)
!CPL =PX.SIZE

	newopnd_x8()
	fx:=genopnd(xa,4)

	genmc(m_movd,fx,px)

	swapopnds(1)
	popopnd()
end

proc pc_pushptr_d124(ref pclrec p) =
	getopnds(1)
	genmc_loadptr_d124(ttbasetype[p^.mode],aa^.value)
end

proc pc_pushretslot_d8_d124(ref pclrec p) =
	newstackopnd_d8()
end

proc pc_pushretslot_d16(ref pclrec p) =
	newstackopnd_d16()
end

proc pc_pushretslot_x8(ref pclrec p) =
	newstackopnd_x8()
end

proc pc_pushffretval_pushretval_d8_d124(ref pclrec p) =
!r0 contains a value returned from a foreign function
	if noperands and opndstack[noperands]<>rtos then
		GENCOMMENT_MC("1:pushffret: regs in use")
!		gerror("1:pushffret: regs in use")
	fi

	newretvalopnd_d8()
end

proc pc_pushffretval_pushretval_x8_x4(ref pclrec p) =
!r0 contains a value returned from a foreign function
	if noperands and opndstack[noperands]<>rtos then
		gerror("2:pushffret: regs in use")
	fi

	newretvalopnd_x8()
end

proc pc_pushretval_d16(ref pclrec p) =
!r0 contains a value returned from a foreign function
	if noperands and opndstack[noperands]<>rtos then
		GENCOMMENT_MC("3:pushret: regs in use")
!		gerror("1:pushffret: regs in use")
	fi

	newretvalopnd_d16()
end


proc pc_moveretval_d8_d124(ref pclrec p)=
!ensure that current operand is in d1/d0
	var int reg

	if noperands>1 and opndstack[noperands-1]<>rtos then
		gerror("moveretval: other regs not empty")
	fi

	getopnds(1)			!make sure in any register
	reg:=opndstack[noperands]

	if reg<>r0 then			!simply move it across
		genmc(m_mov,genreg(r0), genopnd(xa))
		freereg(reg)
	fi
	popopnd()

end

proc pc_moveretval_d16(ref pclrec p)=
!ensure that current operand is in d1/d0
	var int reg1,reg2

	if noperands>1 and opndstack[noperands-1]<>rtos then
		gerror("moveretval_d16: other regs not empty")
	fi

	getopnds(1)			!make sure in any register
	reg1:=opndstack[noperands]
	reg2:=opndreg2[noperands]

	if reg1<>r0 and reg2<>r1 then
		GERROR("MOVERETVAL/D16 - NOT IN D1:D0")
	FI
	popopnd()

end

proc pc_moveretval_x8_x4(ref pclrec p)=
!ensure that current operand is in d1/d0
	var int reg

	if noperands>1 and opndstack[noperands-1]<>rtos then
		gerror("moveretvalx: other regs not empty")
	fi

	getopnds(1)			!make sure in any register
	reg:=opndstack[noperands]

	if reg<>xr0 then			!simply move it across
		genmc(m_mov,genxreg(r0), genopnd(xa))
		freexreg(reg)
!		regset[r0]:=1
	fi
	popopnd()

end

proc pc_popmem_popmemz_d8(ref pclrec p) =
	if opndstack[noperands]<>rtos and opndkind[noperands]='F' then
		genmc_storemem_x8(p^.a.def)
		popopnd()
	else
		getopnds(1)
		genmc_storemem_d8(p^.a.def)
		popopnd()
	fi
end

proc pc_popmem_d16(ref pclrec p) =
	getopnds(1)
	genmc_storemem_d16(p^.a.def)
	popopnd()
end

proc pc_popmem_x8(ref pclrec p) =
	if opndstack[noperands]=rtos then
		getopnds(1)				!do via gp reg
		genmc_storemem_d8(p^.a.def)
		popopnd()
	elsif opndkind[noperands]='F' then
		genmc_storemem_x8(p^.a.def)
		popopnd()
	else
		gerrorc("popmem/x8 not in xreg")
	fi
end

proc pc_popmem_x4(ref pclrec p) =
	if opndstack[noperands]=rtos then
		getopnds(1)				!do via gp reg
		genmc_storemem_d124(p^.a.def,tu32)
		popopnd()
	elsif opndkind[noperands]='F' then
		genmc_storemem_x4(p^.a.def)
		popopnd()
	else
		gerrorc("popmem/x4 not in xreg")
	fi
end

proc pc_popmem_popmemz_d124(ref pclrec p) =
	getopnds(1)
	genmc_storemem_d124(p^.a.def,ttbasetype[p^.mode])
	popopnd()
end

proc pc_popmem_storemem_flex(ref pclrec p) =
	var int isstore:=p^.opcode=k_storemem
	var ref opndrec ax,bx

	if isstore then
		getopnds(1)			!get value to store
		genmc(m_inc,changeopndsize(genopndind(xa),4))
	fi
	pushalloperands()

!push address of dest, off-the-record
	ax:=genreg(r13)
	genmc(m_mov, ax, bx:=genmem_d_mc(aa^.def))
	genmc(m_push,ax)
gerror("callflex free")
!	callflexhandler_mc(p,"free",0)		!0 official args
	getopnds(1)
	genmc(m_mov, bx, genopnd(xa))

	if not isstore then
		popopnd()
	fi
end

proc pc_popptr_storeptr_flex(ref pclrec p) =
	var int isstore:=p^.opcode=k_storeptr
	var ref opndrec ax,bx

IF AA^.VALUE THEN
GERROR("POP/STOREPTR/FLEX - OFFSET NOT ZERO")
FI

	getopnds(1)				!the pointer
	ax:=genopnd(xa)			!reg with pointer
	pushalloperands()
	genmc(m_push,ax)			!push another unofficial copy of pointer

gerror("callflex free2")
!	callflexhandler_mc(p,"free",0)		!0 official args; free dest
	getopnds(2)				!value to store, and pointer to tst

	bx:=genopndind(ya)
	if isstore then
		genmc(m_inc,changeopndsize(bx,4))
	fi
	genmc(m_mov,bx,genopnd(xb))		!store flex value
	popopnd()						!lose ptr

	if not isstore then
		popopnd()
	fi
end

proc pc_popptr_d8_x8(ref pclrec p) =
	if opndstack[noperands-1]<>rtos and opndkind[noperands-1]='F' then	!
!both already available if using xreg
		genmc_storeptr_x8(aa^.value)
	else
		getopnds(2)
		genmc_storeptr_d8(aa^.value)
	fi
	popopnd()
	popopnd()
end

proc pc_popptr_d16(ref pclrec p) =
!d16 value on stack first, then the d8 pointer

	getopnds(2)
	genmc_storeptr_d16(aa^.value)
	popopnd()
	popopnd()
end

proc pc_popptr_x4(ref pclrec p) =
	if opndstack[noperands-1]<>rtos and opndkind[noperands-1]='F' then	!
!both already available if using xreg
		genmc_storeptr_x4(aa^.value)
	else
		getopnds(2)
		genmc_storeptr_x4(aa^.value)
	fi
	popopnd()
	popopnd()
end

proc pc_popptr_d124(ref pclrec p) =
	getopnds(2)
	genmc_storeptr_d124(ttbasetype[p^.mode],aa^.value)
	popopnd()
	popopnd()
end

proc pc_storeptr_d8(ref pclrec p) =
!like popptr, but keep the value on the opnd stack

	if opndstack[noperands-1]<>rtos and opndkind[noperands-1]='F' then	!
!both already available if using xreg
		genmc_storeptr_x8(aa^.value)
	else
		getopnds(2)
		genmc_storeptr_d8(aa^.value)
	fi
	popopnd()
!	popopnd()
end

proc pc_storeptr_d124(ref pclrec p) =
!like popptr, but keep the value on the opnd stack

	if opndstack[noperands-1]<>rtos and opndkind[noperands-1]='F' then	!
!both already available if using xreg
GERROR("STOREPTR/D124/F")
!		genmc_storeptr_x8()
	else
		getopnds(2)
		genmc_storeptr_d124(ttbasetype[p^.mode],aa^.value)
	fi
	popopnd()
!	popopnd()
end

proc pc_storemem_d8(ref pclrec p) =
	if opndstack[noperands]<>rtos and opndkind[noperands]='F' then
		genmc_storemem_x8(p^.a.def)
	else
		getopnds(1)
		genmc_storemem_d8(p^.a.def)
	fi
end

proc pc_storemem_d16(ref pclrec p) =
	getopnds(1)
	genmc_storemem_d16(p^.a.def)
end

proc pc_storemem_x8(ref pclrec p) =
	getopnds(1)
	genmc_storemem_x8(p^.a.def)
end

proc pc_storemem_d124(ref pclrec p) =
	if opndstack[noperands]<>rtos and opndkind[noperands]='F' then
GERROR("STOREMEM/D124/X")
!		genmc_storemem_x8(p^.a.def)
	else
		getopnds(1)
		genmc_storemem_d124(p^.a.def,ttbasetype[p^.mode])
	fi
end

proc pc_storeptr(ref pclrec p) =
	unimpl("k_storeptr")
end

proc pc_popstack(ref pclrec p) =
	unimpl("k_popstack")
end

proc pc_popretval_d16(ref pclrec p) =
	getopnds(1)
	genmc_storeretval_d16(p^.a.value)
	popopnd()
end

proc pc_popretval_d8_d124(ref pclrec p) =
	getopnds(1)
	genmc_storeretval_d8(p^.a.value)
	popopnd()
end

proc pc_popretval_x8(ref pclrec p) =
	getopnds(1)
	genmc_storeretval_x8(p^.a.value)
	popopnd()
end

!proc pc_poptx(ref pclrec p) =
!	unimpl("k_poptx")
!end
!
proc pc_free(ref pclrec p) =
!	case stdtypecat[ttbasetype[a^.mode]]
!	when flex_cat, variant_cat then
		unimpl("free/flex")
!	else
!		popstack_mc(ttsize[a^.mode])
!	esac
end

proc pc_free_d8_d124(ref pclrec p) =
	popopnd()
end

proc pc_free_d16(ref pclrec p) =
	popopnd()
end

proc pc_free_x4_x8(ref pclrec p) =
	popopnd()
end

proc pc_free_flex(ref pclrec p) =
	pushalloperands()
gerror("callflex free3")
!	callflexhandler_mc(p,"free",1)
!	popopnd()
end

proc pc_add_sub_i64_u64_c64_ref(ref pclrec p) =
	getopnds(2)
	genmc((p^.opcode=k_add|m_add|m_sub),genopnd(xb),genopnd(ya))

	popopnd()
end

proc pc_add_i128_u128(ref pclrec p) =
	getopnds(2)

	genmc(m_add, genopnd(xb), genopnd(ya))
	genmc(m_adc, genopndh(xb), genopndh(ya))

	popopnd()
end

proc pc_sub_i128_u128(ref pclrec p) =
	getopnds(2)

	genmc(m_sub, genopnd(xb), genopnd(ya))
	genmc(m_sbb, genopndh(xb), genopndh(ya))

	popopnd()
end

proc pc_add_sub_mul_div_r32_r64(ref pclrec p) =
	var int opc:=0

	getopnds(2)

	if p^.mode=tr32 then
		case p^.opcode
		when k_add then opc:=m_addss
		when k_sub then opc:=m_subss
		when k_mul then opc:=m_mulss
		when k_div then opc:=m_divss
		esac
	else
		case p^.opcode
		when k_add then opc:=m_addsd
		when k_sub then opc:=m_subsd
		when k_mul then opc:=m_mulsd
		when k_div then opc:=m_divsd
		esac
	fi
	genmc(opc,genopnd(xb),genopnd(ya))
	popopnd()
end

proc pc_mul_i64_u64(ref pclrec p) =
	getopnds(2)
	genmc(m_imul2,genopnd(xb),genopnd(ya))
!	if p^.catmode=ti64 then
!		genmc_cond(m_jmpcc,ov_cond,gensys_mc("msys.m$intoverflow"))
!		genmc_cond(m_jmpcc,ltu_cond,gensys_mc("msys.m$intoverflow"))
!	fi
	popopnd()
end

proc pc_mul_i128_u128(ref pclrec p) =
	pushalloperands()
	call128handler(sysfn_mul_i128)
end

proc pc_idiv_i128_u128(ref pclrec p) =
	pushalloperands()
	call128handler(sysfn_idiv_i128)
end

proc pc_idiv_irem_i64_u64(ref pclrec p) =
	var int opc

	getopnds(2)
	fixdivopnds_d8()

	opc:=m_idiv					!assume signed

	case p^.catmode
	when ti64 then
		genmc(m_cqo)
!	when ti32,ti16,ti8 then
!		genmc(m_cdq)
!
	else
		genmc(m_xorx, genreg(r11),genreg(r11))
		opc:=m_div
	esac

	genmc(opc, genopnd(ya,ttsize[p^.mode]))

	if p^.opcode=k_irem then
		genmc(m_xchg,genreg(r0),genreg(r11))
	fi

	popopnd()
end

!proc pc_irem(ref pclrec p) =
!	unimpl("k_irem")
!end

proc pc_neg_i64_u64_c64(ref pclrec p) =
	getopnds(1)
	genmc(m_neg,genopnd(xa))
end

proc pc_neg_i128_u128(ref pclrec p) =
	var ref opndrec ax1,ax2, bx1,bx2

	getopnds(1)

	ax1:=genopnd(xa)
	ax2:=genopndh(xa)

	bx1:=gettempopnd_d8()
	bx2:=gettempopnd_d8()

	genmc(m_xorx, bx1,bx1)
	genmc(m_xorx, bx2,bx2)
	genmc(m_sub,bx1,ax1)
	genmc(m_sbb,bx2,ax2)

!Here better to swap operand bx to ax
	opndstack[noperands]:=bx1^.reg
	opndreg2[noperands]:=bx2^.reg

!	genmc(m_xchg, ax1,bx1)
!	genmc(m_xchg, ax2,bx2)

	freetempopnd_d8(ax1)
	freetempopnd_d8(ax2)
end

proc pc_neg_r64(ref pclrec p) =
	getopnds(1)
	if not labneg64 then labneg64:=createfwdlabel() fi
	genmc(m_xorpd,genopnd(xa),genlabel_mem(labneg64))
end

proc pc_neg_r32(ref pclrec p) =
	getopnds(1)
	if not labneg32 then labneg32:=createfwdlabel() fi
	genmc(m_xorps,genopnd(xa),genlabel_mem(labneg32))
end

proc pc_neg(ref pclrec p) =
	unimpl("k_neg")
end

!proc pc_abs_i64_u64_i8_u8_i16_u16_i32_u32_c8_c16(ref pclrec p) =
proc pc_abs_i64_u64_c64(ref pclrec p) =
	var ref opndrec ax,lx
	var int lab
	getopnds(1)
	genmc(m_cmp,ax:=genopnd(xa,ttsize[p^.mode]),genint_mc(0))
	lab:=++labelno

	genmc_cond(m_jmpcc,ge_cond,lx:=genlabel_mc(lab))
	genmc(m_neg,ax)
	genmc(m_label,lx)
end

proc pc_abs_r32(ref pclrec p) =
	var ref opndrec ax,lx
	getopnds(1)
	if not lababs32 then lababs32:=createfwdlabel() fi
	genmc(m_andps,genopnd(xa),genlabel_mem(lababs32))

end

proc pc_abs_r64(ref pclrec p) =
	var ref opndrec ax,lx
	getopnds(1)
	if not lababs64 then lababs64:=createfwdlabel() fi
	genmc(m_andpd,genopnd(xa),genlabel_mem(lababs64))
!MCCODEX^.B.MODE:=A_MEM
end

proc pc_inot_i64_u64(ref pclrec p) =
	getopnds(1)
	genmc(m_notx,genopnd(xa))
end

proc pc_notl_d8_d124(ref pclrec p) =
	getopnds(1)
	genmc(m_xorx,genopnd(xa),genint_mc(1))
end

proc pc_istruel_d8_d124(ref pclrec p) =
	var ref opndrec ax
	var int lab

	getopnds(1)
	ax:=genopnd(xa)
	genmc(m_andx,ax,ax)
	lab:=createfwdlabel_mc()
	genmc_cond(m_jmpcc,eq_cond,genlabel_mc(lab))
	genmc(m_mov,ax,genint_mc(1))
	definefwdlabel_mc(lab)

end

proc pc_call(ref pclrec p) =
	case aa.optype
	when memaddr_opnd then
		genmc(m_call,genmemaddr_d_mc(aa^.def))
	when label_opnd then
		genmc(m_call,genlabel_mc(aa^.labelno))
	else
		gerror("pc/call")
	esac

	to bb^.value do
		poparg()
	od
end

proc pc_return(ref pclrec p) =
	genmc(m_ret)
end

proc pc_callptr(ref pclrec p) =
	getopnds(1)
	genmc(m_call,genopnd(xa))
	popopnd()					!get rid of fn ptr
	to aa^.value do
		poparg()
	od
end

proc pc_syscall(ref pclrec p) =
	var int fn
	fn:=aa.value
	genmc_sys(fn)

!	mccodex.a.mode:=a_mem

	to bb^.value do
		poparg()
	od
end

proc pc_callff(ref pclrec p) =
!CPL "CALLFF:",=D^.ATTRIBS.VARPARAMS
	genmc(m_mov,genreg(r0),genmemaddr_d_mc(bb^.def))

	docallff(aa^.nargs,p^.a.floatmap,p^.isfunction,p^.isvariadic)
end

proc pc_callptrff(ref pclrec p) =
	getopnds(1)
	if opndstack[noperands]<>r0 then
		genmc(m_mov,genreg(r0),genopnd(xa))
	fi
	popopnd()
	docallff(aa^.nargs,p^.a.floatmap,p^.isfunction,p^.isvariadic)
end

proc pc_jump(ref pclrec p) =
	genmc(m_jmp,genlabel_mc(aa^.labelno))
!	unimpl("k_jump")
end

proc pc_jumpcc_i64_u64_ref_c64(ref pclrec p) =
	var int cond

	getopnds(2)

	genmc(m_cmp,genopnd(xb),genopnd(ya))

!	if gettypecode_t(p^.catmode)='I' then
	if tttypecode[p^.catmode]='I' then
		cond:=getmclcond_i(p^.cond)
	else
		cond:=getmclcond_u(p^.cond)
	fi
	genmc_cond(m_jmpcc, cond, genlabel_mc(aa^.labelno))
	popopnd()
	popopnd()
end

proc pc_jumpccimm_i64_u64_c64(ref pclrec p) =
	var int cond

	getopnds(1)

	genmc(m_cmp,genopnd(xa),genint_mc(bb^.code^.value))

!	if gettypecode_t(p^.catmode)='I' then
	if tttypecode[p^.catmode]='I' then
		cond:=getmclcond_i(p^.cond)
	else
		cond:=getmclcond_u(p^.cond)
	fi
	genmc_cond(m_jmpcc, cond, genlabel_mc(aa^.labelno))
	popopnd()
end

proc pc_jumpcc_i128(ref pclrec p) =
	var ref opndrec lxtrue,lxfalse, ax1,ax2, bx1,bx2, cx1,cx2
	var int cond

	lxtrue:=genlabel_mc(aa^.labelno)
	lxfalse:=genlabel_mc(++labelno)

	getopnds(2)

	ax1:=genopnd(xb)
	ax2:=genopndh(xb)
	bx1:=genopnd(ya)
	bx2:=genopndh(ya)

	case p^.cond
	when j_eq then
		genmc(m_cmp,ax1,bx1)
		genmc_cond(m_jmpcc,ne_cond,lxfalse)
		genmc(m_cmp,ax2,bx2)
		genmc_cond(m_jmpcc,eq_cond,lxtrue)
		genmc(m_label,lxfalse)
	when j_ne then
		genmc(m_cmp,ax1,bx1)
		genmc_cond(m_jmpcc,ne_cond,lxtrue)
		genmc(m_cmp,ax2,bx2)
		genmc_cond(m_jmpcc,ne_cond,lxtrue)
	else
		genmc(m_sub, ax1,bx1)
		genmc(m_sbb, ax2,bx2)

		genmc(m_cmp,ax2, mm.zero_opnd)
		case p^.cond
		when j_lt then
			genmc_cond(m_jmpcc, lt_cond, lxtrue)
		when j_le then
			genmc_cond(m_jmpcc, lt_cond, lxtrue)
			genmc(m_orx,ax1,ax2)
			genmc_cond(m_jmpcc, eq_cond, lxtrue)
		when j_gt then
			genmc_cond(m_jmpcc, lt_cond, lxfalse)
			genmc(m_orx,ax1,ax2)
			genmc_cond(m_jmpcc, ne_cond, lxtrue)
			genmc(m_label,lxfalse)
		when j_ge then
			genmc_cond(m_jmpcc, ge_cond, lxtrue)
		esac
	esac

	popopnd()
	popopnd()
end

proc pc_jumpcc_u128(ref pclrec p) =
	var ref opndrec lxtrue,lxfalse, ax1,bx1,ax2,bx2
	var int cond1,cond2,cond3

	if p^.cond in [j_eq, j_ne] then
		pc_jumpcc_i128(p)
		return
	fi

	case p^.cond
	when j_gt then
		cond1:=gtu_cond
		cond2:=ltu_cond
		cond3:=gtu_cond
	when j_ge then
		cond1:=gtu_cond
		cond2:=ltu_cond
		cond3:=geu_cond
	when j_lt then
		cond1:=ltu_cond
		cond2:=gtu_cond
		cond3:=ltu_cond
	when j_le then
		cond1:=ltu_cond
		cond2:=gtu_cond
		cond3:=leu_cond
	esac

	lxtrue:=genlabel_mc(aa^.labelno)
	lxfalse:=genlabel_mc(++labelno)

	getopnds(2)
	ax1:=genopnd(xb)
	ax2:=genopndh(xb)
	bx1:=genopnd(ya)
	bx2:=genopndh(ya)

	genmc(m_cmp,ax2,bx2)
	genmc_cond(m_jmpcc, cond1, lxtrue)
	genmc_cond(m_jmpcc, cond2, lxfalse)
	genmc(m_cmp,ax1,bx1)
	genmc_cond(m_jmpcc, cond3, lxtrue)

	genmc(m_label,lxfalse)

	popopnd()
	popopnd()
end

proc pc_jumpcc_r32_r64(ref pclrec p) =
	var int cond

	getopnds(2)

	genmc((ttsize[p^.mode]=4|m_comiss|m_comisd),genopnd(xb),genopnd(ya))

	cond:=getmclcond_u(p^.cond)

	genmc_cond(m_jmpcc, cond, genlabel_mc(aa^.labelno))
	popopnd()
	popopnd()
end

!proc pc_jumpfalse_jumptrue_i64_u64_r64_ref_i8_u8_i16_u16_i32_u32_c8_c16(ref pclrec p) =
proc pc_jumpfalse_jumptrue_i64_u64_r64_ref_c64(ref pclrec p) =
	getopnds(1)

	genmc(m_cmp,genopnd(xa),genint_mc(0))

	genmc_cond(m_jmpcc, (p^.opcode=k_jumptrue|ne_cond|eq_cond), genlabel_mc(aa^.labelno))
	popopnd()
end

proc pc_jumpinyz_jumpnotinyz_i64_u64(ref pclrec p) =
	var ref opndrec ax,bx,cx,lx,nolx
	var int m,nolab

	getopnds(1)
	getopndn_d8(1)
	getopndn_d8(2)
	cx:=genreg(opndstack[noperands])
	bx:=genreg(opndstack[noperands-1])
	ax:=genreg(opndstack[noperands-2])

	lx:=genlabel_mc(aa^.labelno)
	m:=p^.catmode

	genmc(m_cmp,ax,bx)
	if p^.opcode=k_jumpinyz then
		nolx:=genlabel_mc(nolab:=createfwdlabel_mc())
		genmc_cond(m_jmpcc,(m=ti64|lt_cond|ltu_cond),nolx)
		genmc(m_cmp,ax,cx)
		genmc_cond(m_jmpcc,(m=ti64|le_cond|leu_cond),lx)
		definefwdlabel_mc(nolab)
	else
		genmc_cond(m_jmpcc,(m=ti64|lt_cond|ltu_cond),lx)
		genmc(m_cmp,ax,cx)
		genmc_cond(m_jmpcc,(m=ti64|gt_cond|gtu_cond),lx)
!		definefwdlabel(nolx)
	fi

	popopnd()
	popopnd()
	popopnd()
end

proc pc_casejumpeq_i64_u64_r64_ref_c64(ref pclrec p) =
	getopnds(2)
	genmc(m_cmp,genopnd(xb),genopnd(ya))

	genmc_cond(m_jmpcc, eq_cond, genlabel_mc(aa^.labelno))
	popopnd()
!	popopnd()
end

proc pc_setjumpeq_setjumpeqx_d8_d124(ref pclrec p) =
	getopnds(2)

	genmc(m_cmp,genopnd(xb,ttsize[p^.mode]),genopnd(ya,ttsize[p^.mode]))

	genmc_cond(m_jmpcc, eq_cond, genlabel_mc(aa^.labelno))
	popopnd()
	if p^.opcode=k_setjumpeqx then
		popopnd()
	fi
end

proc pc_setjumpne_d8_d124(ref pclrec p) =
	getopnds(2)
	genmc(m_cmp,genopnd(xb,ttsize[p^.mode]),genopnd(ya,ttsize[p^.mode]))

	genmc_cond(m_jmpcc, ne_cond, genlabel_mc(aa^.labelno))
	popopnd()
	popopnd()
end

!proc pc_casejumpne_i64_u64_r64(ref pclrec p) =
!	getopnds(2)
!	genmc(m_cmp,genopnd(xb),genopnd(ya))
!
!	genmc_cond(m_jmpcc, ne_cond, genlabel_mc(aa^.labelno))
!	popopnd()
!end

proc pc_switch(ref pclrec p) =
	var int lab1,lab2
	var ref opndrec ax

	lab1:=p^.a.labelno
	lab2:=p^.b.labelno

	getopnds(1)
	ax:=genopnd(xa)
	if swmin<>0 then
		genmc(m_sub,ax,genint_mc(swmin))
	fi
	genmc(m_cmp,ax,genint_mc(swmax-swmin+1))
	genmc_cond(m_jmpcc,geu_cond,genlabel_mc(lab2))
	genmc(m_jmp, genindex(ireg:ax^.reg,scale:8,labno:lab1))

	popopnd()

	setsegment('I')
!	genmc(m_label,genlabel_mc(lab1))
end

proc pc_switchlab(ref pclrec p) =
	genmc(m_dq,genlabel_mc(p^.a.labelno))
end

proc pc_endswitch(ref pclrec p) =
	setsegment('C')
end

proc pc_info(ref pclrec p) =
	swmin:=p^.a.value
	swmax:=p^.b.value
!	unimpl("k_info")
end

proc pc_setcc_i64_u64_ref_c64(ref pclrec p) =
	var ref opndrec ax,bx
	var int cond

	getopnds(2)

	genmc(m_cmp,ax:=genopnd(xb),genopnd(ya))

!	if gettypecode_t(p^.mode)='I' then
	if tttypecode[p^.mode]='I' then
		cond:=getmclcond_i(p^.cond)
	else
		cond:=getmclcond_u(p^.cond)
	fi
	genmc_cond(m_setcc, cond, bx:=changeopndsize(ax,1))

	genmc(m_movzx,changeopndsize(ax,4), bx)

	popopnd()
end

proc pc_setcc(ref pclrec p) =
	unimpl("k_setcc")
end

proc pc_compare(ref pclrec p) =
	unimpl("k_compare")
end

proc pc_isequal(ref pclrec p) =
	unimpl("k_isequal")
end

proc pc_iand_ior_ixor_i64_u64_c64(ref pclrec p) =
	var int opc

	getopnds(2)

	case p^.opcode
	when k_iand then opc:=m_andx
	when k_ior then opc:=m_orx
	when k_ixor then opc:=m_xorx
	esac

	genmc(opc,genopnd(xb),genopnd(ya))
	popopnd()
end

proc pc_iand_ior_ixor_i128_u128(ref pclrec p) =
	var int opc

	getopnds(2)

	case p^.opcode
	when k_iand then opc:=m_andx
	when k_ior then opc:=m_orx
	when k_ixor then opc:=m_xorx
	esac

	genmc(opc,genopnd(xb),genopnd(ya))
	genmc(opc,genopndh(xb),genopndh(ya))
	popopnd()
end

proc pc_iandc_i64_u64_c64(ref pclrec p) =
	var int opc

	getopnds(1)

	case p^.opcode
	when k_iandc then opc:=m_andx
!	when k_ior then opc:=m_or
!	when k_ixor then opc:=m_xor
	esac

	genmc(opc,genopnd(xa),genint_mc(aa^.value))
end

!proc pc_shl_i64_u64_i8_u8_i16_u16_i32_u32(ref pclrec p) =
proc pc_shl_i64_u64(ref pclrec p) =
	doshiftn(p, m_shl)
end

!proc pc_shr_i64_i8_i16_i32(ref pclrec p) =
proc pc_shr_i64(ref pclrec p) =
	doshiftn(p, m_sar)
end

!proc pc_shr_u64_u8_u16_u32(ref pclrec p) =
proc pc_shr_u64(ref pclrec p) =
	doshiftn(p, m_shr)
end

proc pc_shlc_i64_u64(ref pclrec p) =
	getopnds(1)

	genmc(m_shl,genopnd(xa),genint_mc(aa^.value))
end

!proc pc_shrc_i64_u64_i8_u8_i16_u16_i32_u32(ref pclrec p) =
proc pc_shrc_i64_u64(ref pclrec p) =
	getopnds(1)

	genmc((tttypecode[p^.catmode]='I'|m_sar|m_shr),genopnd(xa),genint_mc(aa^.value))
end

proc pc_shr(ref pclrec p) =
	unimpl("k_shr")
end

proc pc_in(ref pclrec p) =
	unimpl("k_in")
end

proc pc_min_max_i64_u64(ref pclrec p) =
	var ref opndrec ax,bx
	var int cond

	getopnds(2)
	ax:=genopnd(xb)
	bx:=genopnd(ya)

	if P^.opcode=k_min then
		cond:=(p^.catmode=ti64|gt_cond|gtu_cond)
	else
		cond:=(p^.catmode=ti64|lt_cond|ltu_cond)
	fi

	genmc(m_cmp,ax,bx)
	genmc_cond(m_cmovcc,cond,ax,bx)

	popopnd()

end

proc pc_min_max_r64(ref pclrec p) =
	var ref opndrec ax,bx

	getopnds(2)
	ax:=genopnd(xb)
	bx:=genopnd(ya)

	genmc((p^.opcode=k_min|m_minsd|m_maxsd),ax,bx)
	popopnd()

end

proc pc_addoffset_ref(ref pclrec p) =
!add offset to ptr to result in new pointer
!probably 2nd operand will not be constant (as that is done as regular add)
	var ref opndrec px,ax,ix
	var int size,n

	getopnds(2)
	px:=genopnd(xb)
	ax:=genopnd(ya)

	size:=ttsize[tttarget[p^.mode]]
!need to scale offset by size
	case size
	when 1,2,4,8 then
		ix:=genindex(areg:px^.reg, ireg:ax^.reg,scale:size)
		genmc(m_lea,px,ix)
	elsif n:=ispoweroftwo(size) then
		genmc(m_shl,ax,genint_mc(n))
		genmc(m_add,px,ax)
	else
		genmc(m_imul2,ax,genint_mc(size))
		genmc(m_add,px,ax)
	esac
	popopnd()
end

proc pc_subref_i64_u64(ref pclrec p) =
!add offset to ptr to result in new pointer
!probably 2nd operand will not be constant (as that is done as regular add)
	var ref opndrec px,qx
	var int size,n

	getopnds(2)
	px:=genopnd(xb)
	qx:=genopnd(ya)

	size:=ttsize[tttarget[p^.mode2]]
	genmc(m_sub,px,qx)

!need to scale offset by size
	if size<>1 then
		if n:=ispoweroftwo(size) then
			genmc(m_shr,px,genint_mc(ispoweroftwo(size)))
		else
GERROR("SUBPTR/NOT 2**N")
!			genmc(m_imul2,px,genint_mc(size))
		fi
	fi
	popopnd()
end

proc pc_suboffset_ref(ref pclrec p) =
!subtract two pointers to yield i64
	var ref opndrec px,qx,ix
	var int size,n

!CPL "SUBPTR",STRMODE(P^.MODE2)

	getopnds(2)
	px:=genopnd(xb)
	qx:=genopnd(ya)

	size:=ttsize[tttarget[p^.mode]]

!need to scale offset from bytes to elements by dividing
	if size=1 then
	elsif n:=ispoweroftwo(size) then
		genmc(m_shl,qx,genint_mc(n))
	elsif px^.reg=r0 then
		genmc(m_imul2,qx,genint_mc(size))
	fi
	genmc(m_sub,px,qx)
	popopnd()
end

proc pc_concat(ref pclrec p) =
	unimpl("k_concat")
end

proc pc_append(ref pclrec p) =
	unimpl("k_append")
end

proc pc_andl(ref pclrec p) =
	unimpl("k_andl")
end

proc pc_orl(ref pclrec p) =
	unimpl("k_orl")
end

proc pc_index_indexref_ax_sx(ref pclrec p) =
	var ref opndrec ax,ax2,ix,fx
	var int rega,regi,elemsize,offset,elemmode,lower,amode,scale,opc,reg2,n

	if p^.catmode=tslice then
		getopnds(2)						!get d16 array and d8 index
		opndkind[noperands-1]:='I'			!convert (ptr,len) to just ptr
		freereg(opndreg2[noperands-1])
		amode:=p^.mode
	else
		getopnds(2)
!		amode:=tttarget[p^.mode]
		amode:=p^.mode
	fi

!NOTE: ax will be an 64-bit operand containing a pointer. It will be overridden
!by the loaded values (for k_index), but it could be a different size
!(wide, block) or be a float. It means that operand's info needs updating.
!OPTIONS::
! * Allow the same operand to exist in two forms
! * Create a fresh operand, but then need to be able to pop the other
!   two while retaining the new one (may need to rotate top 3 opndstack elements)
!Probably second option is best, but is not needed for::
!   indexref (as new operand is d8 same as ax)
!   index/d8/d124 (same size and type dest as ax)
!   index/flex  (same size as ax)
!This applies also to K_DOT, and may apply also to PUSHPTR when target is
! float, wide, block or variant

	ax:=genopnd(xb)
	ix:=genopnd(ya)

	rega:=ax^.reg
	regi:=ix^.reg

	elemmode:=tttarget[amode]
	scale:=elemsize:=ttsize[elemmode]
	lower:=ttlower[amode]
	offset:=-lower*elemsize

	if scale not in [1,2,4,8] then
		if n:=ispoweroftwo(scale) then
			genmc(m_shl,ix,genint_mc(n))
		else
			genmc(m_imul2,ix,genint_mc(elemsize))
		fi
		scale:=1
	fi

	if p^.opcode=k_index then

		case p^.catmode2
		when tscalar then
			genmc(m_mov,ax,genindex(rega,regi,scale,offset,8))
		when tshort then
			case tttypecode[p^.mode2]
			when 'I' then
				opc:=m_movsx
			when 'U' then
				opc:=m_movzx
			when 'R' then
				GERROR("INDEX/REF/SHORTFLOAT")
			esac
			genmc(opc,ax,genindex(rega,regi,scale,offset,ttsize[p^.mode2]))

		when tscalarfloat then
			newopnd_x8()
			fx:=genopnd(xa)
			genmc(m_movq,fx,genindex(rega,regi,scale,offset,8))
			swapopnds(2)
			popopnd()

		when twide then
			ix:=genindex(rega,regi,scale,offset,16)
!need to expand main array ptr operand to be 16 bytes
			reg2:=getnextreg()
			opndreg2[noperands-1]:=reg2
			opndkind[noperands-1]:='W'
			ax2:=genreg(reg2)
			genmc(m_mov,ax2,applyoffset(ix,8))		!load msw first
			genmc(m_mov,ax,ix)						!load lsw, overwriting pointer

		when tshortfloat then
			newopnd_x8()
			fx:=genopnd(xa)
			genmc(m_movd,fx,genindex(rega,regi,scale,offset,4))
			swapopnds(2)
			popopnd()

		else
			cpl strmode(p^.catmode2)
			gerror("ref/index/catmode2")
		esac
	else
		genmc(m_lea,ax,genindex(rega,regi,scale,offset,ttsize[p^.mode2]))
	fi	

	popopnd()
end

proc pc_indexmem_ax(ref pclrec p) =
	var ref opndrec ax,ax2,ix,fx
	var int rega,regi,elemsize,offset,elemmode,lower,amode,scale,opc,reg2,n
	var ref strec d

!slices not supported by indexmem
	getopnds(1)
	amode:=p^.mode

!	ax:=genmem_mc_u(aa^.def)
	ix:=genopnd(xa)
	d:=aa^.def

!	rega:=ax^.reg
	regi:=ix^.reg

	elemmode:=tttarget[amode]
	scale:=elemsize:=ttsize[elemmode]
	lower:=ttlower[amode]
	offset:=-lower*elemsize

	if scale not in [1,2,4,8] then
		if n:=ispoweroftwo(scale) then
			genmc(m_shl,ix,genint_mc(n))
		else
			genmc(m_imul2,ix,genint_mc(elemsize))
		fi
		scale:=1
	fi

	case p^.catmode2
	when tscalar then
		genmc(m_mov,ix,genindex(ireg:regi,
					scale:scale,offset:offset,size:8,def:d))
	when tshort then
		case tttypecode[p^.mode2]
		when 'I' then
			opc:=m_movsx
		when 'U' then
			opc:=m_movzx
		when 'R' then
			GERROR("INDEX/REF/SHORTFLOAT")
		esac

!		rega:=0
!		if scale=1 then swap(rega,regi) fi

!		genmc(opc,ix,genindex(areg:rega, ireg:regi,
		genmc(opc,ix,genindex(ireg:regi,
					scale:scale,offset:offset,size:ttsize[p^.mode2],def:d))

	when tscalarfloat then
		newopnd_x8()
		fx:=genopnd(xa)
		genmc(m_movq,fx,genindex(ireg:regi,
					scale:scale,offset:offset,size:8,def:d))
		swapopnds(1)
		popopnd()

	when twide then
GERROR("INDEXMEM/WIDE")
		ix:=genindex(rega,regi,scale,offset,16)
!need to expand main array ptr operand to be 16 bytes
		reg2:=getnextreg()
		opndreg2[noperands-1]:=reg2
		opndkind[noperands-1]:='W'
		ax2:=genreg(reg2)
		genmc(m_mov,ax2,applyoffset(ix,8))		!load msw first
		genmc(m_mov,ax,ix)						!load lsw, overwriting pointer

	when tshortfloat then
!		GERROR("SHORT FLOAT")
		newopnd_x8()
		fx:=genopnd(xa)
		genmc(m_movd,fx,genindex(ireg:regi,
					scale:scale,offset:offset,size:4,def:d))
		swapopnds(1)
		popopnd()

	else
		cpl strmode(p^.catmode2)
		gerror("2:index/catmode2")
	esac
end

proc pc_popindex_storeindex_ax(ref pclrec p) =
!y[z]:=x

	var ref opndrec ax,ax2,ix,fx
	var int rega,regi,elemsize,offset,elemmode,lower,amode,scale,reg2,n

	getopnds(3)

	if p^.catmode=tslice then
		opndkind[noperands-1]:='I'			!convert (ptr,len) to just ptr
		freereg(opndreg2[noperands-1])
		amode:=p^.mode
	else
		amode:=p^.mode
	fi

	ax:=genopnd(yb)
	ix:=genopnd(za)

	rega:=ax^.reg
	regi:=ix^.reg

	elemmode:=tttarget[amode]
	scale:=elemsize:=ttsize[elemmode]
	lower:=ttlower[amode]
	offset:=-lower*elemsize

	if scale not in [1,2,4,8] then
		if n:=ispoweroftwo(scale) then
			genmc(m_shl,ix,genint_mc(n))
		else
			genmc(m_imul2,ix,genint_mc(elemsize))
		fi
		scale:=1
	fi

	case p^.catmode2
	when tscalar then
		ix:=genindex(rega,regi,scale,offset,8)
		genmc(m_mov,ix,genopnd(xc))
	when tshort then
		ix:=genindex(rega,regi,scale,offset,ttsize[p^.mode2])
		genmc(m_mov,ix,genopnd(xc,ttsize[p^.mode2]))

	when tscalarfloat then
		ix:=genindex(rega,regi,scale,offset,8)
		fx:=genopnd(xc)
		genmc(m_movq,ix,fx)

	when twide then
		ix:=genindex(rega,regi,scale,offset,16)

		genmc(m_mov,ix,genreg(opndstack[noperands-1]))
		genmc(m_mov,applyoffset(ix,8),genreg(opndreg2[noperands-1]))

	when tshortfloat then
		ix:=genindex(rega,regi,scale,offset,4)
		fx:=genopnd(xc)
		genmc(m_movd,ix,fx)


	else
		cpl strmode(p^.catmode2)
		gerror("popindex/catmode2")
	esac

	popopnd()
	popopnd()
	if p^.opcode=k_popindex then	!pop xc, the value being stored
		popopnd()
	fi
end

proc pc_dotindex(ref pclrec p) =
!x.[y]
	pushalloperands()
	calldothandler(sysfn_dotindex,2)
end

proc pc_dotslice(ref pclrec p) =
!x.[y..z]
	pushalloperands()
	calldothandler(sysfn_dotslice,3)
end

proc pc_popdotindex(ref pclrec p) =
!y^.[z]:=x
	pushalloperands()
	callpopdothandler(sysfn_popdotindex,3)
end

proc pc_popdotslice(ref pclrec p) =
!x^.[y..z]:=w
	pushalloperands()
	callpopdothandler(sysfn_popdotslice,4)
end

proc pc_dot_dotref_rec(ref pclrec p) =
!SEE NOTES IN PC_INDEX...
	var ref opndrec px,ax,fx
	var int regp,elemsize,offset,elemmode,rmode,opc

	getopnds(1)

	px:=genopnd(xa)
	ax:=px				!share same size etc for now

	regp:=px^.reg

!	rmode:=tttarget[p^.mode]
	rmode:=p^.mode
	elemmode:=p^.mode2
	elemsize:=ttsize[elemmode]
	offset:=p^.a.value

	if p^.opcode=k_dot then

		case p^.catmode2
		when tscalar then
			genmc(m_mov,ax,genireg(regp,8,offset))
		when tshort then
			px:=genireg(regp,ttsize[p^.mode2],offset)
			case tttypecode[p^.mode2]
			when 'I' then
				opc:=m_movsx
			when 'U' then
				opc:=m_movzx
			when 'R' then
				fx:=genxreg(xr15)
				fx:=genxreg(xr15)
				genmc(m_movd,fx,px)
				genmc(m_movq,ax,fx)
				return
			esac
!			genmc(opc,ax,genindex(rega,regi,scale,offset,ttsize[p^.mode2]))
			genmc(opc,ax,px)
		when tscalarfloat then
			newopnd_x8()
			fx:=genopnd(xa)
			genmc(m_movq,fx,genireg(regp,8,offset))
			swapopnds(1)
!			popopnd()
			popopnd()
		when tshortfloat then
			newopnd_x8()
			fx:=genopnd(xa)
			genmc(m_movd,fx,genireg(regp,4,offset))
			swapopnds(1)
			popopnd()

		else
			cpl strmode(p^.catmode2)
			gerror("dot/catmode2")
		esac
	else
		genmc(m_lea,ax,genireg(regp,ttsize[p^.mode2],offset))
	fi	

end

proc pc_popdot_storedot_rec(ref pclrec p) =
!Y.A:=X; A is a byte offset	
	var ref opndrec px,ax,fx
	var int regp,elemsize,offset,elemmode,rmode,opc

	getopnds(2)

	px:=genopnd(ya)
	ax:=px				!share same size etc for now

	regp:=px^.reg

!	rmode:=tttarget[p^.mode]
	rmode:=p^.mode
	elemmode:=p^.mode2
	elemsize:=ttsize[elemmode]
	offset:=p^.a.value

	case p^.catmode2
	when tscalar then
		px:=genireg(regp,8,offset)
		genmc(m_mov,px,genopnd(xb))
	when tshort then
!CPL "POPDOT HERE"
		px:=genireg(regp,ttsize[p^.mode2],offset)
		genmc(m_mov,px,changeopndsize(genopnd(xb),ttsize[p^.mode2]))
	when tscalarfloat then
		px:=genireg(regp,8,offset)
		genmc(m_movq,px,genopnd(xb))
	when tshortfloat then
		px:=genireg(regp,4,offset)
		genmc(m_movd,px,genopnd(xb))
	else
		cpl strmode(p^.catmode2)
		gerror("popdot/catmode2")
	esac

	popopnd()
	if p^.opcode=k_popdot then
		popopnd()
	fi

end

proc pc_lwb(ref pclrec p) =
	unimpl("k_lwb")
end

proc pc_upb(ref pclrec p) =
	unimpl("k_upb")
end

proc pc_len(ref pclrec p) =
	unimpl("k_len")
end

proc pc_bounds(ref pclrec p) =
	unimpl("k_bounds")
end

proc pc_sqrt_r64(ref pclrec p) =
	var ref opndrec ax
	getopnds(1)
	ax:=genopnd(xa)
	genmc(m_sqrtsd,ax,ax)
end

proc pc_sqr_i64(ref pclrec p) =
	var ref opndrec ax
	getopnds(1)
	ax:=genopnd(xa)
	genmc(m_imul2,ax,ax)
end

proc pc_sqr_r64(ref pclrec p) =
	var ref opndrec ax
	getopnds(1)
	ax:=genopnd(xa)
	genmc(m_mulsd,ax,ax)
end

proc pc_power_i64(ref pclrec p) =
	pushalloperands()
	call64handler(sysfn_power_i64)
end

proc pc_power_r64(ref pclrec p) =
	docmaths2("pow*")
end

proc pc_sign_i64_u64(ref pclrec p) =
	var ref opndrec ax,ax8,bx

	getopnds(1)
	ax:=genopnd(xa)
	bx:=changeopndsize(gettempopnd_d8(),1)

	genmc(m_cmp,ax,genint_mc(0))
	ax8:=changeopndsize(ax,1)

	genmc_cond(m_setcc,gt_cond,ax8)
	genmc_cond(m_setcc,lt_cond,bx)
	genmc(m_sub,ax8,bx)
	genmc(m_movsx,ax,ax8)
	freetempopnd_d8(bx)
end

proc pc_sign_r64(ref pclrec p) =
	var ref opndrec fx,ax,bx,ax64

	getopnds(1)
	fx:=genopnd(xa)
	ax:=changeopndsize(ax64:=gettempopnd_d8(),1)
	bx:=changeopndsize(gettempopnd_d8(),1)

	if not labzero then labzero:=createfwdlabel() fi
	genmc(m_comisd,fx,genlabel_mem(labzero))

	genmc_cond(m_setcc,gtu_cond,ax)
	genmc_cond(m_setcc,ltu_cond,bx)
	genmc(m_sub,ax,bx)
	genmc(m_movsx,ax64,ax)
	genmc(m_cvtsi2sd,fx,ax64)

	freetempopnd_d8(ax)
	freetempopnd_d8(bx)
end

proc pc_sin_cos_tan_asin_acos_atan_ln_log_exp_floor_ceil_r64(ref pclrec p) =
var [32]char str
var ichar name

	case p^.opcode
	when k_ln,k_log then
		name:="log*"
	else
		name:=&.str
		strcpy(name,pclnames[p^.opcode]+2)
		strcat(name,"*")
	esac

	docmaths1(name)
end

proc pc_round(ref pclrec p) =
	unimpl("k_round")
end

proc pc_fract(ref pclrec p) =
	unimpl("k_fract")
end

proc pc_fmod(ref pclrec p) =
	unimpl("k_fmod")
end

proc pc_addto_subto_iandto_iorto_ixorto_i64_u64_ref(ref pclrec p) =
	var int opc

	getopnds(2)

	case p^.opcode
	when k_addto then opc:=m_add
	when k_subto then opc:=m_sub
	when k_iandto then opc:=m_andx
	when k_iorto then opc:=m_orx
	when k_ixorto then opc:=m_xorx
	esac

	genmc(opc, genopndind(xb), genopnd(ya))

	popopnd()
	popopnd()
end

proc pc_addmemto_submemto_iandmemto_iormemto_ixormemto_i64_u64_ref(ref pclrec p) =
	var int opc

	getopnds(1)

	case p^.opcode
	when k_addmemto then opc:=m_add
	when k_submemto then opc:=m_sub
	when k_iandmemto then opc:=m_andx
	when k_iormemto then opc:=m_orx
	when k_ixormemto then opc:=m_xorx
	esac

	genmc(opc, genmem_d_mc(aa^.def), genopnd(xa))

	popopnd()
end

proc pc_addto_subto_iandto_iorto_ixorto_i32_i16_i8_u32_u16_u8_c16_c8(ref pclrec p) =
	var int opc,size
	var ref opndrec px,ax

	getopnds(2)
	px:=genopndind(xb)
	size:=ttsize[p^.mode]
	px^.size:=size

	case p^.opcode
	when k_addto then opc:=m_add
	when k_subto then opc:=m_sub
	when k_iandto then opc:=m_andx
	when k_iorto then opc:=m_orx
	when k_ixorto then opc:=m_xorx
	esac

	genmc(opc, genopndind(xb), genopnd(ya,size))

	popopnd()
	popopnd()
end

proc pc_addto_multo_r64_r32(ref pclrec p) =
!x +:= y
	var int opc,movopc
	var ref opndrec px,ax

	getopnds(1)			!get y
	--noperands				!bodge as I need operands on mixed registers
	getopnds(1)			!get &x into int register
	++noperands

	if p.mode=tr64 then
		movopc:=m_movq
		case p^.opcode
		when k_addto then opc:=m_addsd
		when k_multo then opc:=m_mulsd
		esac
	else
		movopc:=m_movd
		case p^.opcode
		when k_addto then opc:=m_addss
		when k_multo then opc:=m_mulss
		esac
	fi

	px:=genopndind(xb)
	ax:=genopnd(ya)

	genmc(opc, ax,px)
	genmc(movopc, px,ax)

	popopnd()
	popopnd()
end

proc pc_subto_divto_r64_r32(ref pclrec p) =
!x -:= y
	var int opc,movopc
	var ref opndrec px,ax,bx

	getopnds(1)			!get y
	--noperands
	getopnds(1)			!get &x into int register
	++noperands

	if p.mode=tr64 then
		movopc:=m_movq
		case p^.opcode
		when k_subto then opc:=m_subsd
		when k_divto then opc:=m_divsd
		esac
	else
		movopc:=m_movd
		case p^.opcode
		when k_subto then opc:=m_subss
		when k_divto then opc:=m_divss
		esac
	fi

	px:=genopndind(xb)
	bx:=genopnd(ya)

	ax:=gettempopnd_x8()

	genmc(movopc, ax,px)
	genmc(opc, ax,bx)
	genmc(movopc, px,ax)

	freetempopnd_x8(ax)

	popopnd()
	popopnd()
end

proc pc_multo_i64_u64_i32_u32_i16_u16_i8_u8(ref pclrec p) =
	var ref opndrec px,ax
	var int size

	getopnds(2)

	px:=genopndind(xb)
	size:=ttsize[p^.mode]
	px^.size:=size

	ax:=genopnd(ya,size)

	genmc(m_imul2, ax,px)
	genmc(m_mov, px,ax)

	popopnd()
	popopnd()
end

proc pc_idivto_iremto_i64_u64(ref pclrec p) =
	var int opc
	var ref opndrec px,ax,bx,dx

	getopnds(2)
	fixdivopnds_d8()

	ax:=genreg(r0)
	bx:=genopnd(ya)

	genmc(m_xchg,ax,genreg(r13))
	px:=genireg(r13)

	dx:=genreg(r11)

	genmc(m_mov,ax,px)				!get x into r0

	if p^.catmode=ti64 then
		genmc(m_cqo)
		opc:=m_idiv
	else
		genmc(m_xorx, dx,dx)
		opc:=m_div
	fi

	genmc(opc, bx)

	genmc(m_mov,px,(p^.opcode=k_idivto|ax|dx))

	popopnd()
end

proc pc_shlto_i64_u64(ref pclrec p) =
	doshiftnto(p, m_shl)
end

proc pc_shlcto_i64_u64(ref pclrec p) =
	getopnds(1)
	genmc(m_shl,genopndind(xa),genint_mc(aa^.value))
	popopnd()
end

proc pc_shrcto_i64_u64(ref pclrec p) =
	getopnds(1)
	genmc((p^.catmode=ti64|m_sar|m_shr),genopndind(xa),genint_mc(aa^.value))
	popopnd()
end

proc pc_shlcmemto_i64_u64(ref pclrec p) =
	genmc(m_shl,genmem_d_mc(aa^.def),genint_mc(bb^.value))
end

proc pc_shrcmemto_i64_u64(ref pclrec p) =
	genmc((p^.catmode=ti64|m_sar|m_shr),genmem_d_mc(aa^.def),genint_mc(bb^.value))
end

proc pc_shrto_i64(ref pclrec p) =
	doshiftnto(p, m_sar)
end

proc pc_shrto_u64(ref pclrec p) =
	doshiftnto(p, m_shr)
end

proc pc_minto(ref pclrec p) =
	unimpl("k_minto")
end

proc pc_minto_maxto_i64_u64(ref pclrec p) =
	var ref opndrec px,bx,lx
	var int cond,lab

	getopnds(2)
	px:=genopndind(xb)
	bx:=genopnd(ya)

	if p^.opcode=k_minto then
		cond:=(p^.catmode=ti64|le_cond|leu_cond)
	else
		cond:=(p^.catmode=ti64|ge_cond|geu_cond)
	fi

	genmc(m_cmp,px,bx)
	lab:=++labelno

	genmc_cond(m_jmpcc,cond,lx:=genlabel_mc(lab))
	genmc(m_mov,px,bx)
	genmc(m_label,lx)
	popopnd()
	popopnd()

end

proc pc_minto_maxto_r64(ref pclrec p) =
	var ref opndrec px,x,fx,gx
	var int cond,lab

	getopnds(1)			!get y
	--noperands				!bodge as I need operands on mixed registers
	getopnds(1)			!get &x into int register
	++noperands

	px:=genopndind(xb)
	gx:=genopnd(ya)
	fx:=gettempopnd_x8()

	genmc(m_movq,fx,px)

	genmc((p^.opcode=k_minto|m_minsd|m_maxsd),fx,gx)
	genmc(m_movq,px,fx)

	freetempopnd_x8(fx)

	popopnd()
	popopnd()
end

proc pc_maxto(ref pclrec p) =
	unimpl("k_maxto")
end

proc pc_addoffsetto_suboffsetto_ref(ref pclrec p) =
	var ref opndrec px,ax,ix
	var int size,n

	getopnds(2)
	px:=genopndind(xb)
	ax:=genopnd(ya)

	size:=ttsize[tttarget[p^.mode]]

!!need to scale offset by size
	if size=1 then
	elsif n:=ispoweroftwo(size) then
		genmc(m_shl,ax,genint_mc(n))
	else
		genmc(m_imul2,ax,genint_mc(size))
	fi	

	genmc((p^.opcode=k_addoffsetto|m_add|m_sub),px,ax)
	popopnd()
	popopnd()
end

proc pc_concatto(ref pclrec p) =
	unimpl("k_concatto")
end

proc pc_appendto(ref pclrec p) =
	unimpl("k_appendto")
end

proc pc_negto_inotto_i64_u64(ref pclrec p) =
	getopnds(1)
	genmc((p^.opcode=k_neg|m_neg|m_notx), genopndind(xa))
	popopnd()
end

proc pc_negto_r64(ref pclrec p) =
	var ref opndrec px,fx

	getopnds(1)
	px:=genopndind(xa)

!	fx:=gettempopnd_x8()
!	genmc(m_movq,fx,px)
!	genmc(m_xorpd,fx,genname_mc("[fchsmask_pd]"))
!	fchsused:=1
!	genmc(m_movq,px,fx)
!	freetempopnd_x8(fx)

	px^.offset+:=7
	genmc(m_xorx,px,genint_mc(0x80))

	popopnd()
end

proc pc_incr_d8_d124(ref pclrec p) =
	genmc(m_inc,genmem_d_mc(aa^.def))
end

proc pc_decr_d8_d124(ref pclrec p) =
	genmc(m_dec,genmem_d_mc(aa^.def))
end

proc pc_incr_decr_ref(ref pclrec p) =
	var int size

	if (size:=ttsize[tttarget[p^.mode]])=1 then
		genmc((p^.opcode=k_incr|m_inc|m_dec),genmem_d_mc(aa^.def))
	else
		genmc((p^.opcode=k_incr|m_add|m_sub),genmem_d_mc(aa^.def),genint_mc(size))
	fi
end

proc pc_incrptr_decrptr_d8_d124(ref pclrec p) =
	var ref opndrec px

	getopnds(1)
	px:=genopndind(xa)
	px^.size:=ttsize[p^.mode]

	genmc((p^.opcode=k_incrptr|m_inc|m_dec),px)

	popopnd()
end

proc pc_incrptr_decrptr_ref(ref pclrec p) =
	var ref opndrec px
	var int size

	getopnds(1)
	px:=genopndind(xa)
!	px^.size:=size:=ttsize[tttarget[p^.mode]]
	size:=ttsize[tttarget[p^.mode]]

	if size=1 then
		genmc((p^.opcode=k_incrptr|m_inc|m_dec),px)
	else
		genmc((p^.opcode=k_incrptr|m_add|m_sub),px,genint_mc(size))
	fi

	popopnd()
end

proc pc_preincrptrx_predecrptrx_d8_d124(ref pclrec p) =
!top operand points to d8 value
!increment dest then replace with new value
	var ref opndrec ax, bx, px
	var int size

	size:=ttsize[p^.mode]
	getopnds(1)
	px:=genopndind(xa)			!register pointing to memory
	px^.size:=size
	newopnd_d8()
	bx:=genopnd(xa,size)			!register to contain new value

	genmc((p^.opcode=k_preincrptrx|m_inc|m_dec),px)
	genmc(m_mov, bx,px)

	swapopnds(1)				!bx with px
	popopnd()				!get rid of px
end

proc pc_postincrptrx_postdecrptrx_d8_d124(ref pclrec p) =
!top operand points to d8 value
!load old value then incr/decrement dest
	var ref opndrec ax, px
	var int size


	size:=ttsize[p^.mode]
	getopnds(1)
	px:=genopndind(xa)			!register pointing to memory
	px^.size:=size
	newopnd_d8()
	ax:=genopnd(xa,size)			!register to contain new value

	genmc(m_mov, ax,px)
	genmc((p^.opcode=k_postincrptrx|m_inc|m_dec),px)

	swapopnds(1)				!bx with px
	popopnd()				!get rid of px
end

proc pc_preincrptrx_predecrptrx_ref(ref pclrec p) =
!incr/decrement dest then load new value

	var ref opndrec ax, px
	var int size

	size:=ttsize[tttarget[p^.mode]]
	getopnds(1)
	px:=genopndind(xa)			!register pointing to memory
	ax:=genopnd(xa)			!register to contain new value

	if size=1 then
		genmc((p^.opcode=k_preincrptrx|m_inc|m_dec),px)
	else
		genmc((p^.opcode=k_preincrptrx|m_add|m_sub),px,genint_mc(size))
	fi
	genmc(m_mov, ax,changeopndsize(px,8))
end

proc pc_postincrptrx_postdecrptrx_ref(ref pclrec p) =
	var ref opndrec ax, px
	var int size

	size:=ttsize[tttarget[p^.mode]]
	getopnds(1)
	px:=genopndind(xa)			!register pointing to memory
!	px^.size:=size
	newopnd_d8()
	ax:=genopnd(xa)			!register to contain new value

	genmc(m_mov, ax,changeopndsize(px,8))
	if size=1 then
		genmc((p^.opcode=k_postincrptrx|m_inc|m_dec),px)
	else
		genmc((p^.opcode=k_postincrptrx|m_add|m_sub),px,genint_mc(size))
	fi

	swapopnds(1)				!bx with px
	popopnd()				!get rid of px
end


proc pc_postincrptrx(ref pclrec p) =
	unimpl("k_postincrptrx")
end

proc pc_postdecrptrx(ref pclrec p) =
	unimpl("k_postdecrptrx")
end

proc pc_uwiden_iwiden(ref pclrec p) =
	var ref opndrec ax,ax2, bx, lx
	var int oldsize,newsize,reg2,lab

	getopnds(1)

	oldsize:=ttsize[p^.mode]
	newsize:=ttsize[p^.mode2]

	ax:=genopnd(xa,8)
	if oldsize<8 then
!		ax:=genopnd_xa(newsize)
		bx:=changeopndsize(ax,oldsize)
		genmc((p^.opcode=k_uwiden|m_movzx|m_movsx),ax,bx)
	fi

	if newsize=16 then
		reg2:=getnextreg()
		ax2:=genreg(reg2)
		genmc(m_xorx,ax2,ax2)
		if ttisint[p^.mode2] then
			lab:=++labelno

			genmc(m_cmp,ax,genint_mc(0))
			genmc_cond(m_jmpcc,ge_cond,lx:=genlabel_mc(lab))
			genmc(m_neg,ax2)
			genmc(m_label,lx)
		fi
		opndreg2[noperands]:=reg2
		opndkind[noperands]:='W'
	fi
end

proc pc_ifix(ref pclrec p) =
	var ref opndrec ax, fx

	getopnds(1)
	fx:=genopnd(xa)
	newopnd_d8()
	ax:=genopnd(xa)

	genmc((ttsize[p^.mode]=4|m_cvttss2si|m_cvttsd2si),ax,fx)
	swapopnds(1)
	popopnd()
end

proc pc_ifloat(ref pclrec p) =
	var ref opndrec ax, fx
	var int isize:=ttsize[p^.mode]

	getopnds(1)
	ax:=genopnd(xa)
	if isize<>8 then
		genmc(m_movsx,ax,changeopndsize(ax,isize))
	fi

	newopnd_x8()
	fx:=genopnd(xa)

	genmc((ttsize[p^.mode2]=4|m_cvtsi2ss|m_cvtsi2sd),fx,ax)
	swapopnds(1)
	popopnd()
end

proc pc_fwiden(ref pclrec p)=
	var ref opndrec fx
	getopnds(1)
	fx:=genopnd(xa)
	genmc(m_cvtss2sd,fx,fx)
end

proc pc_fnarrow(ref pclrec p)=
	var ref opndrec fx
	getopnds(1)
	fx:=genopnd(xa)
	genmc(m_cvtsd2ss,fx,fx)
end

proc pc_softtruncate(ref pclrec p)=
	if ttsize[p^.mode]=16 then		!reduce to one register
		getopnds(1)
		freereg(opndreg2[noperands])
		opndkind[noperands]:='I'
	fi
end

proc pc_truncate(ref pclrec p)=
	UNIMPL("TRUNCATE")
!	var ref opndrec fx
!	getopnds(1)
!	fx:=genopnd(xa)
!	genmc(m_cvtsd2ss,fx,fx)
end

proc pc_typepun(ref pclrec p)=
	var int s,t
	var ref opndrec ax,fx
	s:=p^.mode
	t:=p^.mode2

!reduce any W operands to 
!	if (s in [ti128,tu128]) and ttisinteger[t] and ttsize[t]<=8 then
	if opndkind[noperands]='W' and ttsize[t]<=8 then
		getopnds(1)
		freereg(opndreg2[noperands])
		opndkind[noperands]:='I'
		s=ti64
	fi

	if ttisreal[s] and ttisinteger[t] then
		getopnds(1)
		fx:=genopnd(xa)
		ax:=gettempopnd_d8()
		genmc(m_movq,ax,fx)
		opndstack[noperands]:=ax^.reg
		opndkind[noperands]:='I'
		freetempopnd_x8(fx)

	elsif ttisinteger[s] and ttisreal[t] then
		getopnds(1)
		ax:=genopnd(xa)
		fx:=gettempopnd_x8()
		genmc(m_movq,fx,ax)
		opndstack[noperands]:=fx^.reg
		opndkind[noperands]:='F'
		freetempopnd_d8(ax)
!	elsif (s in [ti128,tu128]) and ttisinteger[t] and ttsize[t]<=8 then
!		getopnds(1)
!		freereg(opndreg2[noperands])
!		opndkind[noperands]:='I'
	elsif ttisinteger[s] and ttisinteger[t] then

	else
		unimpl("typepun")
	fi

end

proc pc_swap_d8(ref pclrec p) =
	var ref opndrec ax,bx, cx,dx
	getopnds(2)
	ax:=genopndind(xb)
	bx:=genopndind(ya)
	cx:=gettempopnd_d8()
	dx:=gettempopnd_d8()

	genmc(m_mov,cx,ax)
	genmc(m_mov,dx,bx)
	genmc(m_mov,ax,dx)
	genmc(m_mov,bx,cx)

	freetempopnd_d8(cx)
	freetempopnd_d8(dx)

	popopnd()
	popopnd()

!	unimpl("k_swapXXXUUU")
end

proc pc_swap_d124(ref pclrec p) =
	var ref opndrec px,qx, cx,dx
	var int size

	size:=ttsize[p^.mode]

	getopnds(2)
	px:=changeopndsize(genopndind(xb),size)
	qx:=changeopndsize(genopndind(ya),size)
	cx:=changeopndsize(gettempopnd_d8(),size)
	dx:=changeopndsize(gettempopnd_d8(),size)

	genmc(m_mov,cx,px)
	genmc(m_mov,dx,qx)
	genmc(m_mov,px,dx)
	genmc(m_mov,qx,cx)

	freetempopnd_d8(cx)
	freetempopnd_d8(dx)

	popopnd()
	popopnd()

!	unimpl("k_swapXXXUUU")
end

proc pc_makerange(ref pclrec p) =
	unimpl("k_makerange")
end

proc pc_makelist(ref pclrec p) =
	unimpl("k_makelist")
end

proc pc_makeslice(ref pclrec p) =
!turn x,y on stack into a single slice (ptr=x, length=y)
	getopnds(2)

!combine into one operand
	opndreg2[noperands-1]:=opndstack[noperands]
	opndkind[noperands-1]:='W'
	--noperands
end

proc pc_slicelen_sliceupb(ref pclrec p)=
!extract .length from (ptr,length)
	var int lower

	getopnds(1)
	freereg(opndstack[noperands])				!lose .ptr
	opndstack[noperands]:=opndreg2[noperands]	!make .length main opnd
	opndkind[noperands]:='I'
	lower:=ttlower[p^.mode]
	if lower<>1 then
		genmc(m_add,genopnd(xa),genint_mc(lower-1))
	fi
end

proc pc_sliceptr(ref pclrec p)=
	getopnds(1)
	freereg(opndreg2[noperands])				!lose .length
	opndkind[noperands]:='I'
end

proc pc_assem(ref pclrec p) =
	var unit pcode

	pcode:=p.a.code
!CPL "MCL/ASSEM",mclnames[pcode.opcode]
!GENCOMMENT_MC("MCL/ASSEM")
	genmc(pcode.opcode, genasmopnd(pcode.a),genasmopnd(pcode.b))
!	genmc(pcode.opcode)
!CPL =MCCODEX,MCLNAMES[MCCODEX.OPCODE]
	mccodex.cond:=pcode.cond

end

proc pc_assem_d8(ref pclrec p) =
	var unit pcode

!CPL "MCL/ASSEM/D8"

	newopnd_d8()
	pcode:=p.a.code

	genmc(pcode.opcode, genasmopnd(pcode.a),genasmopnd(pcode.b))
	mccodex.cond:=pcode.cond

end

function genasmopnd(unit p)ref opndrec ax=
	var ref strec d
	var int offset,labno
	var unit a				!expr: nil/name/const/(add name, const)
	var unit x,y

	if p=nil then return nil fi

	case p.tag
	when j_assemreg then
		ax:=genreg(p.reg,p.regsize)

	when j_const then
		ax:=genint_mc(p.value)

	when j_assemmem then
		a:=p.a
		d:=nil
		offset:=labno:=0

		if a then
			case a.tag
			when j_const then
				offset:=a.value
			when j_name then
				d:=a.def
				if d.nameid=labelid then
					if d.index=0 then d.index:=++labelno fi
					labno:=d.index
					d:=nil
				fi
			when j_add,j_sub then
!CPL "GENMCL/MEM/ADD"
				x:=a.a
				y:=a.b
				if x.tag=j_name and y.tag=j_const then
!CPL "---NAME/CONST",=y.value
					d:=x.def
					if d.nameid=labelid then
						if d.index=0 then d.index:=++labelno fi
						labno:=d.index
						d:=nil
					fi
				else
					goto error
				fi
				offset:=(a.tag=j_add|y.value|-y.value)
			when j_neg then
				unless a.a.tag=j_const then gerror("-name") end
				offset:=-a.a.value
			when j_syscall then
				labno:=getsysfnlabel(a.opcode)

			else
error::
				cpl jtagnames[a.tag]
				gerror("Can't do memexpr")
			esac
		fi

		ax:=genindex(areg:p.reg, ireg:p.regix, scale:p.scale, size:ttsize[p.prefixmode],
			offset:offset, labno:labno, def:d)

	when j_name then
		d:=p.def
		if d.nameid=labelid then
			if d.index=0 then
				d.index:=++labelno
			fi
			ax:=genlabel_mc(d.index)
		else
			ax:=genmemaddr_u_mc(p)
		fi

	when j_assemxreg then
		ax:=genxreg(p.reg)
	when j_add,j_sub then
		x:=p.a
		y:=p.b
		if x.tag=j_name and y.tag=j_const then
			d:=x.def
			offset:=(p.tag=j_add|y.value|-y.value)
			if d.nameid=labelid then
				if d.index=0 then
					d.index:=++labelno
				fi
				ax:=genlabel_mc(d.index)
			else
				ax:=genmemaddr_d_mc(d)
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

proc pc_db_dw_dd_dq(ref pclrec p) =
	var int opc
	var ref opndrec ax

	case p^.opcode
	when k_db then opc:=m_db
	when k_dw then opc:=m_dw
	when k_dd then opc:=m_dd
	when k_dq then opc:=m_dq
	esac

	case p^.a.optype
	when intimm_opnd then ax:=genint_mc(p^.a.value)
	when realimm_opnd then ax:=genrealimm_mc(p^.a.xvalue,p.a.size)
	when strimm_opnd then
		 ax:=genlabel_mc(getstringindex(aa.svalue,strlen(aa.svalue)))

	when memaddr_opnd then ax:=genmemaddr_d_mc(p^.a.def)
	else
		CPL OPNDNAMES[P^.A.OPTYPE]
		gerror("db/dq optype?")
	esac

	genmc(opc,ax)
end

proc pc_resb(ref pclrec p) =
	unimpl("k_resb")
end

proc pc_resw(ref pclrec p) =
	unimpl("k_resw")
end

proc pc_resd(ref pclrec p) =
	unimpl("k_resd")
end

proc pc_resq(ref pclrec p) =
	unimpl("k_resq")
end

proc pc_copyblock(ref pclrec p)=
	var int n,nwords,lab,oddbytes,offset
	var ref opndrec ax,bx,rx,rcount

	n:=ttsize[p^.mode]			!no. bytes to copy

	getopnds(2)				!the two pointers
	ax:=genopndind(xb)			!dest (ax:=bx)
	bx:=genopndind(ya)			!source

	offset:=0

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8			!number of word64s (ie. octobytes)

	rx:=gettempopnd_d8()			!work reg
	rcount:=gettempopnd_d8()		!count

	if 1<=nwords<=4 then		!use unrolled code (no loop)
		offset:=0
		ax:=changeopndsize(ax,targetsize)
		bx:=changeopndsize(bx,targetsize)

		to nwords do
			genmc(m_mov,rx,applyoffset(bx,offset))
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop
		lab:=++labelno

		genmc(m_mov,rcount,genint_mc(nwords))
		genmc(m_label,genlabel_mc(lab))
		genmc(m_mov,rx,bx)
		genmc(m_mov,ax,rx)

		genmc(m_add,genreg(ax^.reg),genint_mc(targetsize))
		genmc(m_add,genreg(bx^.reg),genint_mc(targetsize))

		genmc(m_dec,rcount)
		genmc_cond(m_jmpcc,ne_cond,genlabel_mc(lab))
		offset:=0
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

	freetempopnd_d8(rcount)
	freetempopnd_d8(rx)

	popopnd()
	popopnd()

end

proc pc_csegment(ref pclrec p)=
	setsegment('C')
end

proc pc_isegment(ref pclrec p)=
	setsegment('I')
end

proc pc_zsegment(ref pclrec p)=
	setsegment('Z')
end

proc showopndstack=
var [256]char str
var [8]char str2
var int reg

!	strcpy(&.str,"									(")
	str[1]:=0

!	sprintf(&.str,"                            %d (", noperands)
	fprint @&.str,"                            # (", noperands

	for i to noperands do
		reg:=opndstack[i]
		if reg=rtos then
			strcat(&.str,chr(opndkind[i]))
			strcat(&.str,"T ")
		elsecase opndkind[i]
		when 'I' then
			strcat(&.str,regnames[reg])
		when 'F' then
			strcat(&.str,xregnames[reg])
		when 'W' then
			strcat(&.str,regnames[reg])
			strcat(&.str,"/")
			strcat(&.str,regnames[opndreg2[i]])
		fi
		if i<noperands then strcat(&.str,",") fi
	od
	strcat(&.str,") (")
	for r:=r0 to regmax do
		strcat(&.str,(regset[r]|"1 "|"0 "))
	od
	strcat(&.str,") (")
	for r:=r0 to xregmax do
		strcat(&.str,(xregset[r]|"1 "|"0 "))
	od
	strcat(&.str,")")
	gencomment_mc(&.str)
end

proc showopndstack_s=
var [256]char str
var [8]char str2
var int reg

	strcpy(&.str,"(")
	for i to noperands do
		if opndkind[i] then
			str2[1]:=opndkind[i]
			str2[2]:=0
			strcat(&.str,&.str2)
		fi
		reg:=opndstack[i]
		strcat(&.str,(reg=rtos|"T"|regnames[reg]))
		if opndkind[i]='W' then
!			sprintf(&.str2,"/%d",opndreg2[i])
			print @&.str2,"/",,opndreg2[i]
			strcat(&.str,&.str2)
		fi
		if i<noperands then strcat(&.str,",") fi
	od
	strcat(&.str,") (")
	CP &.STR

FOR R:=R0 TO REGMAX DO
!	CP REGNAMES[R],,":",REGSET[R],,", "
	CP REGSET[R],,(r=regmax|""|", ")
OD
	CPL ")"

!	gencomment_mc(&.str)
end

function findop(ichar name)int=
	for i to pclnames.len do
		if eqstring(name,pclnames[i]+2) then
			return i
		fi
	od
	return 0
end

function findcat(ichar name)int=
	for i:=tvoid to tvariant do
		if eqstring(name,stdtypenames[i]) then
			return i
		fi
	od
	return 0
end

proc dohandler(ichar name, ref proc fnaddr)=
!name is handler name including "pc_" part
!separate out the "_"-separated portions which indicate opcode and category/type
!(of which that can be 0 to N of each kind)
!Look them up as either opcoded or categories, and build a list of each type
!Finally, scan the list filling in each handletable[opc, cat] entry with fnaddr
	var [128]char str
	const int maxopc=32
	var [maxopc]int opcodes
	var [maxopc]int catmodes
	var [maxopc]int oplengths
	var [maxopc]int catlengths
	var [maxopc]ichar parts
	var int nops, ncats,nparts,n
	var ref char s,t

	nops:=ncats:=nparts:=0

	s:=&.str
	t:=name+3

	parts[++nparts]:=s

	while t^ do
		if t^='_' then
			s++^:=0
			parts[++nparts]:=s

		else
			s++^:=t^
		fi
		++t
	od
	s^:=0

	for i to nparts do
		if n:=findop(parts[i]) then
			opcodes[++nops]:=n
		elsif n:=findcat(parts[i]) then
			catmodes[++ncats]:=n
		else
			gerror_s("Can't find opc/cat: %s",name)
		fi
	od

	if nops=0 then
		gerror("pclhandlers/no ops")
	fi
	if ncats=0 then
		catmodes[++ncats]:=tvoid
	fi
	for i to nops do
		for j to ncats do
			handlertable[opcodes[i],catmodes[j]]:=cast(fnaddr)
		od
	od
end

proc fixdivopnds_d8=
!two div operands exist as the top two operands, which will be
!in registers
!the div op requires that x is in d0, and y in any other register
!d11 also needs to be free, which will be the case is reg allocs only
!go up to d9, and d10/d11/12/13 are in use for win64 parameter passing
	var int regx,regy,zop

	regx:=opndstack[noperands-1]
	regy:=opndstack[noperands]

	if regx=r0 then			!regy will be OK
		return
	fi
	if regy=r0 then			!need to swap then
		genmc(m_xchg,genopnd(xb),genopnd(ya))
		swapopnds(1)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset[r0]=0 then	!d0 not in use
		genmc(m_xchg,genreg(r0),genopnd(xb))
		regset[regx]:=0
		opndstack[noperands-1]:=r0
		regset[r0]:=1
		return
	fi

!need to move current occupier of r0
	for zop:=1 to noperands do
		if opndstack[zop]=r0 then exit fi
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg,genreg(r0),genopnd(xb))	
	swap(opndstack[noperands-1],opndstack[zop])		!switch registers

end

proc doshiftn(ref pclrec p,int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
	var ref opndrec ax,cx,dx
	getopnds(2)

	ax:=genopnd(xb,ttsize[p^.mode])
	cx:=genopnd(ya)

!count needs to be in CL, which is R10
!Assume that is not in use in this simple allocator
	dx:=genreg(r10)
	genmc(m_xchg,cx,dx)
	dx:=genreg(r10,1)			!cl

	genmc(opc, ax, dx)

	popopnd()
end

proc doshiftnto(ref pclrec p,int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	var ref opndrec px,cx,dx
	getopnds(2)

	px:=genopndind(xb)
	cx:=genopnd(ya)

!count needs to be in CL, which is R10
!Assume that is not in use in this simple allocator
	dx:=genreg(r10)
	genmc(m_xchg,cx,dx)
	dx:=genreg(r10,1)			!cl

	genmc(opc, px, dx)

	popopnd()
	popopnd()
end

proc docallff(int nargs, floatmap, isfunction,isvariadic)=
!fn address should be loaded to d0, and all regs should be cleared
	var int n,mask
	var [256]char str

	n:=min(nargs,4)
	mask:=1

	for i:=0 to n-1 do
		if isvariadic then
			if floatmap iand mask then
				genmc(m_pop,genreg(r10+i))
				genmc(m_movq,genxreg(xr0+i),genreg(r10+i))
			else
				genmc(m_pop,genreg(r10+i))
			fi
		else
			if floatmap iand mask then
!may need special handling for r32, however, no info on such params 
				genmc(m_movq,genxreg(xr0+i),genireg(rstack))
				popstack_mc(8)
			else
				genmc(m_pop,genreg(r10+i))
			fi
		fi
		--noperands
		mask<<:=1
	od

	if nargs<=4 then
!CPL =SYSFN_CALLFF_4
!GENCOMMENT_MC("CALLFF/GENMC_SYS")
		genmc_sys(sysfn_callff_4)
	else
		genmc_sys(sysfn_callff_4+nargs-4)
	fi
!	mccodex.a.mode:=a_mem

!	genmc(m_call, genreg(r0))

	for i:=5 to nargs do
!		popopnd()
		poparg()
	od

end

proc docmaths1(ichar name)=
	pushalloperands()
	genmc(m_mov,genreg(r0),genname_mc(name))

	docallff(nargs:1, floatmap:1, isfunction:1, isvariadic:0)
	pc_pushffretval_pushretval_x8_x4(nil)
end

proc docmaths2(ichar name)=
	pushalloperands()

	genmc(m_mov,genreg(r0),genname_mc(name))

	docallff(nargs:2, floatmap:2x11, isfunction:1, isvariadic:0)
	pc_pushffretval_pushretval_x8_x4(nil)
end

!proc callflexhandler_mc(ref pclrec p,ichar opname,int nargs)=
!	var [32]char str
!!	sprintf(&.str,"q%s.%s_%s",stdtypenames[p^.mode],stdtypenames[p^.mode],opname)
!	fprint @&.str,"q#.#_#",stdtypenames[p^.mode],stdtypenames[p^.mode],opname
!	genmc(m_call, gensys_mc(&.str))
!	to nargs do
!		poparg()
!	od
!end
!
proc call128handler(int fnindex)=
!two 16-byte operands pushed (x*y called as fn(y,x))
!call special handler that returns result in d1:d0
!adjust operand stack to drop the original two, and set up one operand in d1:d0

	genmc_sys(fnindex)

	--noperands				!lose one operand
	opndstack[noperands]:=r0
	opndreg2[noperands]:=r1
	opndkind[noperands]:='W'
	regset[r0]:=1
	regset[r1]:=1
end

proc call64handler(int fnindex)=
!two 8-byte operands pushed (x*y called as fn(x,y))
!call special handler that returns result in d0
!adjust operand stack to drop the original two, and set up one operand in d1:d0

	genmc_sys(fnindex)

	--noperands				!lose one operand
	opndstack[noperands]:=r0
	opndkind[noperands]:='I'
	regset[r0]:=1
end

proc calldothandler(int fnindex, nargs)=
!called has pushed x,y, or x,y,z, to receive result in d0

	genmc_sys(fnindex)

	noperands-:=(nargs-1)		!lose 1 or 2 operands, to leave one in d0

	opndstack[noperands]:=r0
	opndkind[noperands]:='I'
	regset[r0]:=1
end

proc callpopdothandler(int fnindex, nargs)=
!called has pushed x,y,z or w,x,y,z; no result returned
	genmc_sys(fnindex)
	noperands-:=nargs
end

proc genstringtable=
	var int i, col

!	GENCOMMENT_MC("STRING TABLE; SETSEG NEXT")

!CPL "AT GENSTRTABLE"
	setsegment('I',8)
!	GENCOMMENT_MC("DONE SETSEG")

	if kk0used then
		genmc(m_label,genlabel_mc(kk0used))
		gendb(0)
	fi
	return unless nstrings


	for i to nstrings do
		genmc(m_label,genlabel_mc(stringlabtable[i]))

!		if stringlentable^[i]>1000 then
!			genlongstring(stringtable^[i],stringlentable^[i])
!		else
			genstring(stringtable^[i],stringlentable^[i],1)
!		fi
	od
end

global proc genstring(ichar s, int length,doterm)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
	var int i, c, seqlen
	var ref char seq

	if length=0 then
		gendb(0)
		return
	fi

	seqlen:=0

	to length do
		c:=s++^
		if c<32 or c>=127 or c='\"' then
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
	if doterm then
		gendb(0)
	fi
end

proc gendb(int a)=
	genmc(m_db,genint_mc(a))
end

proc gendw(int a)=
	genmc(m_dw,genint_mc(a))
end

proc gendbstring(ichar s, int length)=
!string is printable, and doesn't include double quotes
	genmc(m_db,genstrimm_mc(s,length))
end

proc gendq(int a)=
	genmc(m_dq,genint_mc(a))
end

proc gendqname(ref strec d)=
	genmc(m_dq,genmemaddr_d_mc(d))
end

proc gendqlabel(int lab)=
	genmc(m_dq,genlabel_mc(lab))
end

proc genrealtable=
	var real x

	return unless nreals

	gencomment_mc("Real Table")
	setsegment('I',8)

	for i to nreals do
		genmc(m_label,genlabel_mc(abs(reallabtable[i])))
		x:=realtable[i]

		if reallabtable[i]>0 then
			genmc(m_dq, genrealimm_mc(x,8))
		else
			genmc(m_dd, genrealimm_mc(x,4))
		fi
	od
end

proc genfunctiondata=
!generate tables of functions accessible by m$ functions in msys
!Will be in this format::
!m$fnaddresses::
!	dq m.fn1				!need fully qualified name
!	dq m.fn2
!	dq 0					!zero-terminator for when accessed sequentially
!m$fnnames::
!	dq $fn.1				!addres of zero-terminated string
!	dq $fn.2
!	dq 0
!$fn.1:	db 'function_name1',0
!$fn.2:	db 'function_name2',0
!m$

	var int i,nprocs,n,nexports,nparams,optflag
	var int labelbase

	var ref strec d,e
	const maxparams=100
	var [maxparams]ref strec params

	nprocs:=0

	setsegment('I',8)
	genmc(m_labelname,genstrimm_mc("m$fnaddresses:"))
	genmc(m_label, genlabel_mc(getsysfnlabel(sysfn_procaddrs)))

	for i:=1 to nmodules do
		d:=moduletable[i].stmodule^.deflist

		while d do
			if d^.nameid=procid then
				++nprocs
				gendqname(d)
			fi
			d:=d^.nextdef
		od
	od

	gendq(0)

	genmc(m_labelname,genstrimm_mc("m$fnnames:"))
	genmc(m_label, genlabel_mc(getsysfnlabel(sysfn_procnames)))
	n:=0
	labelbase:=labelno
	for i:=1 to nmodules do
		d:=moduletable[i].stmodule^.deflist
		while d do
			if d^.nameid=procid then
				gendqlabel(++labelno)
			fi
			d:=d^.nextdef
		od
	od
	gendq(0)

	n:=0
	for i:=1 to nmodules do
		d:=moduletable[i].stmodule^.deflist
		while d do
			if d^.nameid=procid then
				++n
				genmc(m_label,genlabel_mc(n+labelbase))
				gendbstring(d.name,-1)
				gendb(0)
			fi
			d:=d^.nextdef
		od
	od

	genmc(m_labelname,genstrimm_mc("m$fnnprocs:"))
	genmc(m_label, genlabel_mc(getsysfnlabel(sysfn_nprocs)))
	gendq(nprocs)

nprocs:=nexports:=0
!
!asmstrln("m$fnexports:")
	genmc(m_labelname,genstrimm_mc("!m$fnaddresses:"))
	genmc(m_label, genlabel_mc(getsysfnlabel(sysfn_procexports)))
!
	for i:=1 to nmodules do
		d:=moduletable[i].stmodule^.deflist

		while d do
			if d^.nameid=procid then
				++nprocs
				if d.isglobal=2 then

!layout is:
! record
!		word16	fnindex				!index into program-wide function tables
!		byte	rettype				!void when proc
!		byte	nparams				!clamped to 12 params max
!		[12]byte paramlist			!max 12 params (unused params have tp_void or 0)
! end

					++nexports
!					genmc(m_labelname,genstrimm_mc(d.name))
					gendw(nprocs)
					nparams:=0
					e:=d.paramlist
					while e do
						if e.nameid=paramid then
							 ++nparams
							if nparams>maxparams then gerror("Export: too many params") fi
							params[nparams]:=e
						fi
						e:=e.nextdef
					od
					nparams min:=12

					gendb(getpacktype(d.mode))
					gendb(nparams)

					for j to 12 do
!						asmchar(',')
						if j<=nparams then
							e:=params[j]
							optflag:=(e.optional|64|0)
							gendb(getpacktype(e.mode)+optflag)
						else
							gendb(0)
						fi
					od
				fi
			fi
			d:=d^.nextdef
		od
	od
	gendw(0)

	genmc(m_labelname,genstrimm_mc("!m$fnexports:"))
	genmc(m_label, genlabel_mc(getsysfnlabel(sysfn_nexports)))
	gendq(nexports)
!
end

proc gensysfntable=
!global var [sysfnnames.len]int sysfnlabels
!global var [sysfnnames.len]ref void sysfnaddr
var [256]char name
var int proclab
var ref strec d

gencomment_mc("SYSFN TABLE")
setsegment('I',8)
for i in sysfnnames when sysfnlabels[i] do

	case i
	when sysfn_nprocs, sysfn_nexports,
		 sysfn_procnames, sysfn_procaddrs, sysfn_procexports then
		next
	esac

	strcpy(&.name,"m$")
	strcat(&.name,sysfnnames[i]+6)
!	CPL i,sysfnnames[i]:"20jlp-",=sysfnlabels[i],sysfnproclabels[i]

	d:=findname(&.name)
	if not d then
		println "Can't find",&.name
		next
	fi

	proclab:=0
	while d do
		if d.nameid=procid then
			proclab:=d.labelno
			exit
		fi
		d:=d.nextdupl
	od
	next when not proclab

	genmc(m_label,genlabel_mc(sysfnlabels[i]))
	gendqlabel(proclab)


!	sysfnproclabels[i]:=proclab

!	CPL i,&.name:"20jlp-",=sysfnlabels[i],sysfnproclabels[i],=proclab
od

end

proc genabsneg=
	setsegment('I',16)

	if lababs32 then
gencomment_mc("lababs32")
		genmc(m_label,genlabel_mc(lababs32))
!		definefwdlabel(lababs32)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
gencomment_mc("lababs64")
		genmc(m_label,genlabel_mc(lababs64))
!		definefwdlabel(lababs64)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
gencomment_mc("labneg32")
		genmc(m_label,genlabel_mc(labneg32))
!		definefwdlabel(labneg32)
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
gencomment_mc("labneg64")
		genmc(m_label,genlabel_mc(labneg64))
!		definefwdlabel(labneg64)
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
gencomment_mc("labzero")
		genmc(m_label,genlabel_mc(labzero))
		gendq(0)
	fi
end
=== mm_libmcl.m 19/35 ===
!M Compiler - x64 Target Code Generator 3

!NOTE: LIKELY BUG IN REGISTER ALLOCATION:
!GETTEMPOPND() relies on there being available registers.
!There may not be when all have been used up. Since this is likely
!called after GETOPND_D8 etc, and because it lies outside the
!operand stack system, there is nothing it can do.

!Possible workaround: for all MCL handlers that require temporary operands
!like this, call a special function that checks for likely spare registers,
!and if not does a PUSHALLOPERANDS. This must be called before any GETOPND etc.
!Eg. Checkfreeregs(2) ensures that at least 2 64-bit registers are free, and if
!not then will push all first. A subsequent GETOPND will re-use 1-4 regisers
!depending on number of operands and whether 128-bits are needed. So really
!need about 6 working registers as minimum. (Possible that handlers that need
!temps only with one operand).

!Note that PUSHALLOPERANDS is not the most efficient way to ensure just 1 or 2
!more registers. Better is one that only pushes enough operands to clear enough
!registers. Candidates are the first operands in operand stack (looking left to
!right when very first operand is at the left) that is not on the stack.

import msys
import mlib
import clib
import oslib

import mm_decls
import mm_support
import mm_tables
import mm_lib
import mm_libpcl
import mm_mcldecls
import mm_genmcl
!import mc_common

const fuseregtable=1
!const fuseregtable=0

const fasmformat=1
!const fasmformat=0

global const xc = 2
global const yb = 1
global const za = 0

global const xb = 1
global const ya = 0

global const xa = 0


global var int ptrsize

global var int fshowmsource=0

global var int lababs32, lababs64
global var int labneg32, labneg64
global var int labzero
global var int kk0used=0

global var int retindex
global var int stackaligned
global const initial_stackalignment = 1

global const rtos=rnone			!means stack operand

!global const rcx=r10
!global const rdx=r11
!global const r14=rframe
!global const r15=rstack

global tabledata() [0:]ichar xregnames =
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

global var ref mclrec mccode, mccodex		!genmc adds to this linked list

global var int currsegment=0		!

global var int currzdataalign=0
global var int curridataalign=0

!global var int framebytes			!local stackframe size
!global var int parambytes
global var int frameoffset
global var int isthreadedproc
global var int iscallbackproc

global var int structretoffset			!0, or offset of R9 copy within struct
global var ref mclrec stacksetinstr		!caller of any fn: instr that sets sp
global var int currblocksize			!0, or set to largest block ret value
global var ref mclrec allmclcode
global var ichar allasmstr
global var int allasmstrlen

global var ref opndrec dstackopnd
global var ref opndrec dframeopnd

global var ref opndrec zero_opnd=nil
global var unit zero_unit

global var [r0..r15,1..16]ref opndrec regtable

const initstringsize	= 1024
const initrealsize		= 16

global var ref []ichar	stringtable
global var ref []int32    stringlentable
global var ref []int32    stringlabtable
global var ref []real	realtable
global var ref []int32	reallabtable

var int stringtablesize
var int realtablesize

global var int nstrings=0
global var int nreals=0

global const regmax=r8				!can use r0 to regmax inclusive; only those regs
global const xregmax=xr6
									!can appear in opndstack; rest must be rtos
global const maxoperands=100
global var [maxoperands]byte opndstack	!contains r0..regmax or rtos
global var [maxoperands]byte opndreg2	!2nd register for 'W' operands
global var [maxoperands]byte opndsize	!size of any block operands
global var [maxoperands]byte opndkind	!'I'/'F'/'W'/'B' for normal/float reg/128-bit wide reg/block
global var int noperands				!no. active operands, up to maxoperands

global var [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
global var [r0..r15]byte xregset		!same for xregs

global proc mclinit=

var int r,s

ptrsize:=targetsize

for r:=r0 to r15 do
	regtable[r,1]:=genreg0(r,1)
	regtable[r,2]:=genreg0(r,2)
	regtable[r,4]:=genreg0(r,4)
	regtable[r,8]:=genreg0(r,8)
	regtable[r,16]:=genreg0(r,16)
od

zero_opnd:=genint_mc(0)
zero_unit:=createconstunit(0,ti64)
zero_unit^.mode:=ti64
dframeopnd:=genreg(rframe,8)
dstackopnd:=genreg(rstack,8)

initmcdest()

setsegment('C')

stringtable:=pcm_alloc(ref void.bytes*initstringsize)
stringlentable:=pcm_alloc(int32.bytes*initstringsize)
stringlabtable:=pcm_alloc(int32.bytes*initstringsize)
realtable:=pcm_alloc(real.bytes*initrealsize)
reallabtable:=pcm_alloc(int32.bytes*initrealsize)

stringtablesize:=initstringsize
realtablesize:=initrealsize
end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
mccode:=mccodex:=nil
end

global proc genmc(int opcode, ref opndrec a=nil,b=nil)=
var ref mclrec m, oldm

m:=pcm_alloc(mclrec.bytes)
++NMCL

m^.lineno:=mlineno
m^.opcode:=opcode

m^.a:=a
m^.b:=b

case opcode
when m_mov then
	if a and a^.mode=a_reg and b and b^.mode=a_mem then
		oldm:=mccodex
		if oldm and oldm^.opcode=m_mov and oldm^.a^.mode=a_mem and oldm^.b^.mode=a_reg then
			if  sameoperand(a,oldm^.b) and sameoperand(oldm^.a,b) then
				return 			!don't generate the load
			fi
		fi
	fi
!when m_jmp then
!	case mccodex^.opcode
!	when m_ret, m_retn, m_jmp then
!		return
!	esac
!
esac

if mccode then
	mccodex^.nextmcl:=m
	mccodex:=m
else
	mccode:=mccodex:=m
fi
end

global proc genmc_cond(int opcode, cond, ref opndrec a=nil,b=nil)=
genmc(opcode,a,b)
mccodex^.cond:=cond
end

global function lastmc:ref mclrec=
return mccodex
end

global proc genmcstr(int opcode,ichar s)=
!as genmc but uses a single immediate string operand

genmc(opcode,genstrimm_mc(s))
end

function newmclopnd:ref opndrec=
var ref opndrec a
a:=pcm_allocz(opndrec.bytes)
return a
end

global function duplopnd(ref opndrec a)ref opndrec=
var ref opndrec b
b:=pcm_alloc(opndrec.bytes)
b^:=a^
return b
end

global function genxreg(int xreg,size=8)ref opndrec=
var ref opndrec a

a:=newmclopnd()

a^.mode:=a_xreg
a^.reg:=xreg
a^.size:=size
return a
end

global function genindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, ref strec def=nil)ref opndrec=
!construct a mem address mode
var ref opndrec a
a:=newmclopnd()

a^.mode:=a_mem
a^.reg:=areg

a^.regix:=ireg
a^.scale:=scale
!a^.size:=(size|size|scale)
a^.size:=size

a^.offset:=offset

if labno then
	a^.value:=labno
	a^.valtype:=label_val
elsif def then
	a^.def:=def
	a^.valtype:=def_val
	if isframe(def) then
		a^.reg:=rframe
	fi
fi

return a
end

proc writemclblock(ref mclrec m)=
!block single block of mc code, usually belonging to one proc
!initstr=1 to initialise string o/p for single block
!initgenstr() when initstr
var int i

i:=1
while m do
	writemcl(i,m)
	++i
	m:=m^.nextmcl
od
end

global function writemclcode(ichar caption)ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
var ref strec d

gs_init(dest)
gs_str(dest,"PROC ")
gs_strln(dest,caption)
gs_strln(dest,"!---------------------------------------------")

writemclblock(allmclcode)

gs_strln(dest,"!---------------------------------------------")

return dest
end

global proc gencomment_mc(ichar s)=
if s=nil or s^=0 then
!	genmc(m_blank)
else
	genmcstr(m_comment,s)
fi
end

global function genstrimm_mc(ichar s,int length=-1)ref opndrec=
var ref opndrec a
a:=newmclopnd()
a^.mode:=a_imm
if length<0 then
	length:=strlen(s)
fi
a^.svalue:=pcm_alloc(length+1)
memcpy(a^.svalue,s,length)
(a.svalue+length)^:=0
!a.svalue:=pcm_copyheapblock(s,length)

a^.valtype:=stringimm_val
a^.size:=ptrsize
!a^.slength:=length
return a
end

global function genname_mc(ichar s)ref opndrec=
var ref opndrec a
a:=newmclopnd()
a^.mode:=a_imm
a^.svalue:=pcm_copyheapstring(s)
a^.valtype:=name_val
a^.size:=ptrsize
return a
end

global proc genmc_sys(int fnindex)=
!CPL "HERE",SYSFNNAMES[FNINDEX],GETSYSFNLABEL(FNINDEX)
	genmc_cond(m_call, fnindex, genlabel_mc(getsysfnlabel(fnindex)))

	mccodex.a.mode:=a_mem
	mccodex.a.size:=8
end

global function getsysfnlabel(int fnindex)int=
	if sysfnlabels[fnindex]=0 then
		sysfnlabels[fnindex]:=++labelno
		return labelno
	fi
	return sysfnlabels[fnindex]
end

proc writemcl(int index,ref mclrec mcl)=

!gs_strln(dest,strmcl(mcl))
!ASMSTR("MCL::")
strmcl(mcl)
!ASMSTR("::END")
gs_line(dest)

end

global proc strmcl(ref mclrec mcl)=
static var [512]char str
var [128]char opcname
var ref opndrec a,b
var int opcode,cond,sizepref
var ichar s,comment

opcode:=mcl^.opcode

!CPL MCLNAMES[OPCODE],MCL.LINENO IAND 16777215,SOURCEFILENAMES[MCL.LINENO>>24]

cond:=mcl^.cond
a:=mcl^.a
b:=mcl^.b
comment:=nil

case opcode
!when m_assembly then
!	return mclnames[mcl.opcode]
!"<Massem>"

when m_blank then
!	return ""
	return
when m_comment then
	asmchar(';')
	asmstr(a.svalue)
	return

when m_labelname then				!label name will be complete and will have colon(s)
	case a^.valtype
	when def_val then
!		s:=a^.def^.name
		asmstr(getfullname(a.def))
	when stringimm_val then
		asmstr(a.svalue)
		return
	else
		gerror("strmcl/lab")
	esac

!	sprintf(&.str,"%s%s",s,(mcl^.isglobal|"::"|":"))
!	asmstr((mcl^.isglobal|"@@@::"|"@@@:"))
	asmstr((a.def.isglobal|"::"|":"))
	return

when m_label then
!CPL "STRMCL/LABEL",OPNDNAMES_MA[A.MODE],VALTYPENAMES[A.VALTYPE]

	fprint @&.str,"L#:#",a.value,(mcl^.isglobal|":"|"")
	asmstr(&.str)
	return

esac

case opcode
when m_jmpcc then
	print @&.opcname,"j",,asmcondnames[cond]

when m_setcc then
!	sprintf(&.opcname,"set%s",asmcondnames[cond])
	print @&.opcname,"set",,asmcondnames[cond]

when m_cmovcc then
!	sprintf(&.opcname,"cmov%s",asmcondnames[cond])
	print @&.opcname,"cmov",,asmcondnames[cond]

when m_call then
!	sprintf(&.opcname,"cmov%s",asmcondnames[cond])
	if cond then
		comment:=sysfnnames[cond]+6
	fi
	strcpy(&.opcname,"call")
when m_andx then
	strcpy(&.opcname,"and")
when m_orx then
	strcpy(&.opcname,"or")
when m_xorx then
	strcpy(&.opcname,"xor")
when m_notx then
	strcpy(&.opcname,"not")

else
	strcpy(&.opcname,mclnames[opcode]+2)
esac

ipadstr(&.opcname,10," ")

if not fasmformat then
	if a and b then
!	sprintf(&.str,"  %d/%d",a^.size,b^.size)
		fprint @&.str,"  #/#",a.size,b.size
	elsif a then
!	sprintf(&.str,"  %d",a^.size)
		fprint @&.str,"  #",a.size
	else
		strcpy(&.str,"  ")
	fi
else
	strcpy(&.str,"  ")
fi

ipadstr(&.str,10)

strcat(&.str,&.opcname)

asmstr(&.str)


if a and b then		!2 operands
	sizepref:=needsizeprefix(opcode,a,b)

	stropnd(a,sizepref)
	asmstr(",	")
	stropnd(b,sizepref)

elsif a and a^.mode then								!1 operand
	if opcode=m_call then
		stropnd(a,0)
	else
		stropnd(a,1)
	fi
!else
!	opnds[1]:=0
fi

if comment then
	asmstr("	!")
	asmstr(comment)
fi

end

global proc stropnd(ref opndrec a,int sizeprefix=0,debug=0)=
static var [512]char str
var [128]char str2
var ichar plus,t
var int offset,tc

case a^.mode
when a_reg then
	asmstr(getregname(a^.reg,a^.size))

when a_imm then
	strvalue(a)

when a_mem then
!CPL "STROPND/MEM",VALTYPENAMES[A.VALTYPE],A.OFFSET,A.LABELNO
	case a^.valtype
	when intimm_val then
!		sprintf(&.str, "#%lld",a^.value)
		print @&.str, "#",,a.value
		asmstr(&.str)
	when realimm_val then
		print @&.str, "#",,a.xvalue
		asmstr(&.str)
	when realmem_val then
		print @&.str, "M#",,a.xvalue
		asmstr(&.str)
	esac

!	sprintf(&.str,"%s[",getsizeprefix(a^.size,sizeprefix))
!CPL "STROPND",=A.SIZE

!	print @&.str,getsizeprefix(a^.size,sizeprefix),,"["
	asmstr(getsizeprefix(a^.size,sizeprefix))
	asmstr("[")

	plus:=""
	if a^.reg then
		asmstr(getregname(a^.reg,ptrsize))
		plus:="+"
	fi
	if a^.regix then
		asmstr(plus)
		asmstr(getregname(a^.regix,ptrsize))
		plus:="+"
		if a^.scale>1 then
!			sprintf(&.str2,"*%d",a^.scale)
			asmchar('*')
			asmint(a.scale)
		fi
	fi

	if a.valtype in [def_val,label_val] then
		if plus^='+' then
			asmstr(plus)
		fi
		strvalue(a)
!		if t<>'-' then
!			asmstr("strcat(&.str,plus)
!		fi
!		strcat(&.str,t)
    elsif offset:=a^.offset then
!		sprintf(&.str2,"%+d",offset)
		print @&.str2,offset:"+"
		asmstr(&.str2)
	fi
	asmchar(']')

when a_xreg then
	asmstr(fgetregname(a^.reg,a^.size))

else
CPL "BAD OPND",A.MODE
!GERROR("BAD OPND")
	asmstr("<BAD OPND>")
esac

end

global proc strvalue(ref opndrec a)=
static var [512]char str
!static var [10]char str
var [128]char str2
var ref strec def
var int64 value,offset,length
var ichar ss

def:=a^.def
value:=a^.value

case a^.valtype
when def_val then
	asmstr(getfullname(def))
	if def^.namecat=dllproc_cat then
		asmchar('*')
	fi

addoffset::
	if offset:=a^.offset then
!		sprintf(&.str2,"%s%lld",(offset>0|"+"|""),offset)
		print @&.str2,(offset>0|"+"|""),,offset
		asmstr(&.str2)
	fi

when intimm_val then
!	sprintf(&.str,"%lld",value)
!	getstrint(value,&.str)
	asmint(value)

when realimm_val then
	print @&.str,a.xvalue
	asmstr(&.str)

when realmem_val then
!	print @&.str,"M:",,a.xvalue
	print @&.str,a.xvalue
	asmstr(&.str)

when stringimm_val then
	asmchar('"')
	asmstr(a.svalue)
	asmchar('"')

when name_val then
	asmstr(a^.svalue)

when syscall_val then
	asmstr("XXX")

when label_val then
	asmchar('L')
	asmint(a.labelno)
	goto addoffset

!when sysfn_val then
!	return syscallnames[value]
!
else
esac

end

global proc setsegment(int seg,align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
var int opc

if seg<>currsegment then

	case seg
	when 'I' then opc:=m_isegment
	when 'Z' then opc:=m_zsegment
	when 'C' then opc:=m_csegment
	when 'R' then GERROR("CAN'T DO RODATA SEG")
	ELSE
		GERROR("BAD SEG CODE")
	esac
	if mccodex and mccodex^.opcode in [m_isegment,m_zsegment,m_csegment] then
		mccodex^.opcode:=opc
	else
		genmc(opc)
	fi

	currsegment:=seg
fi


if align<>1 then
	genmc(m_align,genint_mc(align))
fi
end

global function getprocname(ref strec d)ichar=
	case d^.name
	when "main" then
		return "main"
	when "start" then
		return "start"
	else
		return getdottedname(d)
	esac
	return ""
end

global function widenstr(ichar s,int w)int=
!take string s, return left-justified in field at least w wide
!extend w when s is longer, ensuring at least 2 spaces at right
!w is extended in 8-char increments, to ensure successive lines of names aren't too ragged
!return new length, not new padded string

while strlen(s)>=(w-2) do
	w+:=8
od  
return w
end

!global proc genassem(ichar s)=
!genmcstr(m_assembly,s)
!end
!
global function strlabel(int n)ichar=
static var [16]char str
!sprintf(&.str,"L%d",n)
print @&.str,"L",,n
return &.str
end

global function isframe(ref strec d)int=
!don't know how to mark non-frame temps
!might just look at enclosing proc
case d^.nameid
when frameid, paramid then
	return 1
esac
return 0
end

global function getsizeprefix(int size,enable=0)ichar=
if not enable then return "" fi
!CPL "GETSIZEP",SIZE,ENABLE
case size
when 1 then return "byte "
when 2 then return "word16 "
when 4 then return "word32 "
when 8 then return "word64 "
when 16 then return "word128 "
esac
!CPL "RETURNING N:"
!return "***N:"
return ""
end

global function needsizeprefix(int opcode,ref opndrec a,b)int=

case opcode
when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
	return 1

when m_cvtss2si,m_cvtsd2si, m_cvttss2si,m_cvttsd2si then
	return 1
when m_shl, m_shr, m_sar then
	if a^.mode=a_mem then return 1 fi
	return 0
esac

if a^.mode=a_reg or a^.mode=a_xreg or b^.mode=a_reg or b^.mode=a_xreg then
	return 0
fi
return 1
end

global function changeopndsize(ref opndrec a,int size)ref opndrec=
var ref opndrec b

if a^.size<>size then
	if a^.mode=a_reg then
		b:=regtable[a^.reg, size]
	else
		b:=duplopnd(a)
		b^.size:=size
	fi
	return b
fi
return a
end

global function applyoffset(ref opndrec a,int offset,int size=0)ref opndrec=
!astr is an asm operand
!add possible byte offset
var ref opndrec b

if offset=0 and size=0 then
	return a
fi
b:=duplopnd(a)
b^.offset+:=offset
if size then
	b^.size:=size
fi

return b
end

global function genint_mc(int64 x,int size=8)ref opndrec=
var ref opndrec a
a:=newmclopnd()
a^.mode:=a_imm

a^.value:=x
a^.valtype:=intimm_val

a^.size:=size
return a
end

global function genrealmem_mc(real64 x,int size=8)ref opndrec=
var ref opndrec a

!CPL "REALMEM",=SIZE

a:=newmclopnd()
a^.mode:=a_mem
a^.value:=getrealindex(x,size)
a^.valtype:=label_val
a^.size:=size
return a
end

global function genrealimm_mc(real64 x,int size=8)ref opndrec=
var ref opndrec a

a:=newmclopnd()
a^.mode:=a_imm
a^.xvalue:=x
a^.valtype:=realimm_val
a^.size:=size
return a
end

global function genimm(unit p,int size=0)ref opndrec=
!assume p is a const unit, or possible a name (gives a name
var ref opndrec a
var int t

a:=newmclopnd()
a^.mode:=a_imm

!a^.value:=p^.
case p^.tag
when j_const then
	t:=p^.mode
	case tttypecode[t]
	when 'U','I' then
		a^.value:=p^.value
		a^.valtype:=intimm_val
		a^.size:=(size|size|ttsize[t])
	when 'R' then
		a^.xvalue:=p^.xvalue
		a^.valtype:=realmem_val
		a^.size:=(size|size|ttsize[t])
	else
		gerror("GENIMM/MODE?")
	esac

when j_name then
	a^.def:=p^.def
	a^.size:=ttsize[p^.def^.mode]
else
	gerror("genimm/unit")
esac

return a
end

global function genlabel_mc(int x)ref opndrec=
!x is a label index
!generate immediate operand containing label
var ref opndrec a

a:=newmclopnd()
a^.size:=targetbits
a^.mode:=a_imm
a^.value:=x
a^.valtype:=label_val
return a
end

global function genlabel_mem(int x)ref opndrec=
!x is a label index
!generate immediate operand containing label
var ref opndrec a

a:=genlabel_mc(x)
a.mode:=a_mem
return a
end

global function genmem_u_mc(unit p,int size=0)ref opndrec=
return genmem_d_mc(p^.def,ttsize[p^.mode])
end

global function genmem_d_mc(ref strec d,int size=0)ref opndrec=
var ref opndrec a

a:=newmclopnd()
a^.mode:=a_mem

if isframe(d) then
	a^.reg:=rframe
fi
a^.def:=d
a^.valtype:=def_val

!CPL "GENMEM/U/MC",D.NAME,SIZE,STRMODE(D.MODE)
a^.size:=(size|size|ttsize[d^.mode])

return a
end

global function genmemaddr_u_mc(unit p)ref opndrec=
return genmemaddr_d_mc(p^.def)
end

global function genmemaddr_d_mc(ref strec d)ref opndrec=
var ref opndrec a

a:=newmclopnd()
a^.mode:=a_imm

if isframe(d) then
	a^.reg:=rframe
fi
a^.def:=d
a^.valtype:=def_val
a^.size:=ptrsize

return a
end

global function genreg(int reg,size=8)ref opndrec=
static var [0:9]int isnormal=(0, 1,1,0,1,0,0,0,1)
var ref opndrec a

if fuseregtable then
	return regtable[reg,size]
fi
return genreg0(reg,size)
end

global function genreg0(int reg,size=8)ref opndrec=
!global function genreg(int reg,size=4)ref opndrec=
var ref opndrec a

a:=newmclopnd()
a^.mode:=a_reg
a^.reg:=reg
a^.size:=size
return a
end

global function genireg(int reg,size=8,offset=0)ref opndrec=
var ref opndrec a

a:=newmclopnd()
a^.mode:=a_mem
a^.reg:=reg
a^.size:=size
a^.offset:=offset

return a
end

global function getopndsize_u(unit p)int=
return ttsize[p^.mode]
end

global function getopndsize_d(ref strec d)int=
return ttsize[d^.mode]
end

global function getmclcond(int opc,m)int=
var int signedx

signedx:=tttypecode[m]='I'

case opc
when j_eq then return eq_cond
when j_ne then return ne_cond
esac

if ttisreal[m]='R' then
	case opc
	when j_lt then return flt_cond
	when j_le then return fle_cond
	when j_ge then return fge_cond
	when j_gt then return fgt_cond
	esac
else
	case opc
	when j_lt then return (signedx|lt_cond|ltu_cond)
	when j_le then return (signedx|le_cond|leu_cond)
	when j_ge then return (signedx|ge_cond|geu_cond)
	when j_gt then return (signedx|gt_cond|gtu_cond)
	esac
fi

return 0
end

global function getmclcond_i(int opc)int=
	case opc
	when j_eq then return eq_cond
	when j_ne then return ne_cond
	when j_lt then return lt_cond
	when j_le then return le_cond
	when j_ge then return ge_cond
	when j_gt then return gt_cond
	esac

	return 0
end

global function getmclcond_u(int opc)int=
	case opc
	when j_eq then return eq_cond
	when j_ne then return ne_cond
	when j_lt then return ltu_cond
	when j_le then return leu_cond
	when j_ge then return geu_cond
	when j_gt then return gtu_cond
	esac

	return 0
end

global function roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
while size iand (targetsize-1) do ++size od
return size
end

global function getregname(int reg,size=8)ichar=
static var [1..17]ichar prefix=("B","W","","A","","","","D","","","","","","","","Q","N")
static var [32]char str
var [16]char str2
var ichar rs
var int size2

size2:=size
if size2>16 then
	size2:=17
FI

case reg
when rnone then return "-"
when rframe then rs:="frame"
when rstack then rs:="stack"
else
!	sprintf(&.str2,"%d",reg-r0)
	getstrint(reg-r0,&.str2)
	rs:=&.str2
esac

!sprintf(&.str,"%s%s",prefix[size2],rs)
print @&.str,prefix[size2],,rs
return &.str
end

global function fgetregname(int reg,size=8)ichar=
static var [32]char str

if fasmformat then
	print @&.str,"XMM",,reg-xr0
else
	print @&.str,(size=8|"DX"|"SX"),,reg-xr0
fi
return &.str
end

function issimple0(unit p)int=
!return 1 if p is simple: can be evaluated using no registers

case p^.tag
when j_const,j_name then
	return 1
esac
return 0
end

global function isintconst(unit p)int=
if p^.tag=j_const and isintmode(p^.mode) then
	return 1
fi
return 0
end

global function isint32const(unit p)int=
var int64 a
	if isintconst(p) and ttsize[p^.mode]<=8 then
		a:=p^.value
		if a<=int32.maxvalue and a >=int32.minvalue then
			return 1
		fi
	fi
	return 0
end

function sameoperand(ref opndrec a,b)int=
!check if same memory operand
if a^.mode<>b^.mode then return 0 fi
if a^.size<>b^.size then return 0 fi
if a^.value<>b^.value then return 0 fi
if a^.reg<>b^.reg then return 0 fi
if a^.regix<>b^.regix then return 0 fi
if a^.valtype<>b^.valtype then return 0 fi
if a^.scale<>b^.scale then return 0 fi

if a^.def and b^.def and a^.def=b^.def and a^.value=b^.value then
	return 1
elsif a^.def=nil and b^.def=nil and a^.value=b^.value then
	return 1
fi
return 0
end

global proc genmsource(int lineno)=			!GENBSOURCE
end

global function roundto(int64 a,n)int64=
!round a to be multiple of n
!n will be a power of two
--n
while (a iand n) do ++a od
return a
end

global proc pushstack_mc(int n)=
	if n then
		genmc(m_sub,dstackopnd,genint_mc(n))
!		if n iand 8 then
!			stackaligned ixor:=1
!		fi
	fi
end

global proc popstack_mc(int n)=
	if n then
		genmc(m_add,dstackopnd,genint_mc(n))
!		if n iand 8 then
!			stackaligned ixor:=1
!		fi
	fi
end

global function definelabel_mc:int =
genmc(m_label,genlabel_mc(++labelno))
return labelno
end

global function createfwdlabel_mc:int =
return ++labelno
end

global proc definefwdlabel_mc(int lab) =
genmc(m_label,genlabel_mc(lab))
end

global proc genjumpl_mc(int lab) =
genmc(m_jmp,genlabel_mc(lab))
end

global function getstringindex(ichar s,int length)int=

	if s=nil then			!assume nil
		kk0used:=++labelno
		return kk0used
	fi

	if nstrings>=stringtablesize then
		extendstringtable()
	fi

	if nstrings and eqstring(stringtable^[nstrings],s) then
		return stringlabtable^[nstrings]
	fi

	stringtable^[++nstrings]:=s
	stringlentable^[nstrings]:=length
	stringlabtable^[nstrings]:=++labelno

	return labelno
end

global function getrealindex(real x,int size)int=
	if nreals>=realtablesize then
		extendrealtable()
	fi

	realtable^[++nreals]:=x
	++labelno
	reallabtable^[nreals]:=(size=8|labelno|-labelno)
	return labelno
end

proc extendstringtable=
	var ref[]ichar oldstringtable
	var ref[]int32 oldstringlentable
	var ref[]int32 oldstringlabtable
	var int oldstringtablesize

	oldstringtablesize:=stringtablesize
	oldstringtable:=stringtable
	oldstringlentable:=stringlentable
	oldstringlabtable:=stringlabtable

	stringtablesize*:=2

	stringtable:=pcm_alloc(ichar.bytes*stringtablesize)
	stringlentable:=pcm_alloc(int32.bytes*stringtablesize)
	stringlabtable:=pcm_alloc(int32.bytes*stringtablesize)

	for i:=1 to nstrings do
		stringtable^[i]:=oldstringtable^[i]
		stringlentable^[i]:=oldstringlentable^[i]
		stringlabtable^[i]:=oldstringlabtable^[i]
	od

	pcm_free(oldstringtable,ichar.bytes*oldstringtablesize)
	pcm_free(oldstringlentable,int32.bytes*oldstringtablesize)
	pcm_free(oldstringlabtable,int32.bytes*oldstringtablesize)
end

proc extendrealtable=
	var ref[]real oldrealtable
	var ref[]int32 oldreallabtable
	var int oldrealtablesize

	oldrealtablesize:=realtablesize
	oldrealtable:=realtable
	oldreallabtable:=reallabtable

	realtablesize*:=2

	realtable:=pcm_alloc(real.bytes*realtablesize)
	reallabtable:=pcm_alloc(int32.bytes*realtablesize)

	for i:=1 to nreals do
		realtable^[i]:=oldrealtable^[i]
		reallabtable^[i]:=oldreallabtable^[i]
	od

	pcm_free(oldrealtable,real.bytes*oldrealtablesize)
	pcm_free(oldreallabtable,int32.bytes*oldrealtablesize)
end

global proc genmc_loadint_d8(int64 a)=
	genmc(m_mov,genreg(opndstack[noperands]),genint_mc(a))
end

global proc genmc_loadint_d16(word64 low,high)=
	genmc(m_mov,genreg(opndstack[noperands]),genint_mc(low))
	genmc(m_mov,genreg(opndreg2[noperands]),genint_mc(high))
end

global proc genmc_loadword_d8(word64 a)=
	GERROR("MC/PUSHWORD")
end

global proc genmc_loadreal_d8(real64 x)=
	genmc(m_mov,genreg(opndstack[noperands]),genrealmem_mc(x))
end

global proc genmc_loadreal_d4(real64 x)=
	genmc(m_mov,genreg(opndstack[noperands]),genrealmem_mc(x,4))
end

global proc genmc_loadreal_x8(real64 x)=
	genmc(m_movq,genxreg(opndstack[noperands]),genrealmem_mc(x))
end

global proc genmc_loadreal_x4(real64 x)=
	genmc(m_movd,genxreg(opndstack[noperands]),genrealmem_mc(x,4))
end

global proc genmc_loadmem_d8(ref strec d)=
	var ref opndrec ax,bx

	ax:=genreg(opndstack[noperands])
	bx:=genmem_d_mc(d,8)

	genmc(m_mov,ax,bx)
end

global proc genmc_loadmem_d16(ref strec d)=
	var ref opndrec ax1,ax2,bx

	ax1:=genreg(opndstack[noperands])
	ax2:=genreg(opndreg2[noperands])
	bx:=genmem_d_mc(d,8)

	genmc(m_mov,ax1,bx)
	genmc(m_mov,ax2,applyoffset(bx,8))
end

global proc genmc_loadmem_x8(ref strec d)=
	var ref opndrec ax,bx

	if opndkind[noperands]<>'F' then
		GERROR("LOADMEM/X8 REG NOT 'F'")
	fi

	ax:=genxreg(opndstack[noperands])
	bx:=genmem_d_mc(d,8)

	genmc(m_movq,ax,bx)
end

global proc genmc_loadmem_x4(ref strec d)=
	var ref opndrec ax,bx

	if opndkind[noperands]<>'F' then
		GERROR("LOADMEM/X4 REG NOT 'F'")
	fi

	ax:=genxreg(opndstack[noperands])
	bx:=genmem_d_mc(d,4)

	genmc(m_movd,ax,bx)
end

global proc genmc_loadmem_d124(ref strec d,int m)=
	var ref opndrec ax,bx,fx
	var int size
	size:=ttsize[m]

	ax:=genreg(opndstack[noperands],size)
	bx:=genmem_d_mc(d,size)

	genmc(m_mov,ax,bx)
end

global proc genmc_loadmemaddr(ref strec d)=
	var ref opndrec ax,bx

	ax:=genreg(opndstack[noperands])
	bx:=genmem_d_mc(d,8)

	genmc(m_lea,ax,bx)
end

global proc genmc_loadptr_d8(int offset)=
!top operand is a pointer to d8 memory
!replace ptr with contents of target
	var ref opndrec ax,bx

	ax:=genreg(opndstack[noperands])
	bx:=genireg(opndstack[noperands],offset:offset)

	genmc(m_mov,ax,bx)
end

global proc genmc_loadptr_d16(int offset)=
!top operand is a pointer to d8 memory
!operand is converted to a d16 wide operand, overwriting the pointer
!replace ptr with contents of target
	var ref opndrec ax1,ax2,px
	var int reg1,reg2

	reg1:=opndstack[noperands]
	reg2:=getnextreg()

	ax1:=genreg(reg1)
	ax2:=genreg(reg2)
	px:=genireg(reg1,offset:offset)

	genmc(m_mov,ax2,applyoffset(px,8))
	genmc(m_mov,ax1,px)

	opndreg2[noperands]:=reg2
	opndkind[noperands]:='W'

end

global proc genmc_storeptr_d8(int offset)=
!top operand is a pointer to d8 memory
	var ref opndrec ax,bx

	ax:=genireg(opndstack[noperands],offset:offset)
	bx:=genreg(opndstack[noperands-1])

	genmc(m_mov,ax,bx)
end

global proc genmc_storeptr_d16(int offset)=
!top operand is a pointer to d16 memory
	var ref opndrec ax,bx1,bx2

	ax:=genireg(opndstack[noperands],offset:offset)
	bx1:=genreg(opndstack[noperands-1])
	bx2:=genreg(opndreg2[noperands-1])

	genmc(m_mov,ax,bx1)
	genmc(m_mov,applyoffset(ax,8),bx2)
end

global proc genmc_storeptr_x8(int offset)=
	var ref opndrec ax,bx

	ax:=genireg(opndstack[noperands],offset:offset)
	bx:=genxreg(opndstack[noperands-1])

	genmc(m_movq,ax,bx)
end

global proc genmc_storeptr_x4(int offset)=
	var ref opndrec ax,bx

	ax:=genireg(opndstack[noperands],size:4,offset:offset)
	bx:=genxreg(opndstack[noperands-1],4)

	genmc(m_movd,ax,bx)
end

global proc genmc_loadptr_d124(int m,offset)=
!top operand is a pointer to d124 memory
	var ref opndrec ax,bx,fx

	ax:=genreg(opndstack[noperands])
	bx:=genireg(opndstack[noperands],ttsize[m],offset)

	case m
	when ti8,ti16,ti32 then
		genmc(m_movsx,ax,bx)
	when tu8,tu16,tu32,tc8,tc16 then
		genmc(m_movzx,ax,bx)
	when tr32 then
		fx:=genxreg(xr15)
		genmc(m_movd,fx,bx)
		genmc(m_cvtss2sd,fx,bx)
		genmc(m_movq,ax,fx)
	else
		GERROR("LOADPTR-D124?")
	esac
end

global proc genmc_storeptr_d124(int m,offset)=
!top operand is a pointer to d8 memory
	var ref opndrec ax,bx

	ax:=genireg(opndstack[noperands],ttsize[m],offset)
	bx:=genreg(opndstack[noperands-1],ttsize[m])

	genmc(m_mov,ax,bx)
end

global proc genmc_floadmem(ref strec d)=
	var ref opndrec ax,bx

	ax:=genxreg(opndstack[noperands])
	bx:=genmem_d_mc(d,8)

	genmc(m_movq,ax,bx)
end

global proc genmc_loadmemw(ref strec d)=
	GERROR("MC/PUSHMEMW")
end

global proc genmc_loadstr(ichar s, int length)=
	var ref opndrec ax

	ax:=genreg(opndstack[noperands])
	genmc(m_mov, ax, genlabel_mc(getstringindex(s,length)))
end

global proc pushalloperands=
	var int reg,reg2

!GENCOMMENT_MC("PUSHALLOPNDS")

	for i to noperands do
		reg:=opndstack[i]
		if reg=rtos then				!no more regs
			next
		fi
		case opndkind[i]				!will not be 'B' as that's taken care of
		when 'I' then
			genmc(m_push,genreg(reg))
			regset[reg]:=0
		when 'F' then
			genmc(m_movq,genreg(r13),genxreg(reg))
			genmc(m_push,genreg(r13))
!			genmc(m_mov,dstackopnd,genxreg(reg))
			xregset[reg]:=0
		when 'W' then
			genmc(m_push,genreg(reg2:=opndreg2[i]))
			genmc(m_push,genreg(reg))
			opndreg2[i]:=0
			regset[reg]:=0
			regset[reg2]:=0
		esac
		opndstack[i]:=rtos
	od

FOR R:=R0 TO REGMAX DO
	IF REGSET[R] OR XREGSET[R] THEN
CPL "REGS STILL IN USE AFTER PUSHALL"
GENCOMMENT_MC("REGS STILL IN USE AFTER PUSHALL")
!		GERROR("REGS STILL IN USE AFTER PUSHALL")
	FI
OD

end

global proc newopnd_d8=
!create new entry on operand stack, and try to make it a register operand
	var int reg

	if noperands>=maxoperands then
		gerrorc("newopnd_d8:opstack overflow")
		return
	fi

	reg:=getnextreg()
	++noperands
	opndstack[noperands]:=reg
	opndkind[noperands]:='I'
end

global proc newopnd_d16=
!create new entry on operand stack, and try to make it a register operand
	var int reg1,reg2

	if noperands>=maxoperands then
		gerrorc("newopnd_d16:opstack overflow")
		return
	fi

	checktwofreeregs()
	reg1:=getnextreg()
	reg2:=getnextreg()
	++noperands
	opndstack[noperands]:=reg1
	opndreg2[noperands]:=reg2
	opndkind[noperands]:='W'
end

global proc newopnd_x8=
	var int reg

	if noperands>=maxoperands then
		gerrorc("newopnd_x8:opstack overflow")
		return
	fi

	reg:=getnextxreg()
	++noperands
	opndstack[noperands]:=reg
	opndkind[noperands]:='F'
end

global proc newstackopnd_d8=
	pushalloperands()
	newopnd_d8()
	pushstack_mc(8)
	regset[opndstack[noperands]]:=0
	opndstack[noperands]:=rtos
end

global proc newstackopnd_d16=
	pushalloperands()
	newopnd_d16()
	pushstack_mc(16)
	regset[opndstack[noperands]]:=0
	regset[opndreg2[noperands]]:=0
	opndstack[noperands]:=rtos
end

global proc newstackopnd_x8=
	pushalloperands()
	newopnd_x8()
	pushstack_mc(8)
	xregset[opndstack[noperands]]:=0
	opndstack[noperands]:=rtos
end

global proc genmc_storemem_d8(ref strec d)=
	var ref opndrec ax,bx

	ax:=genmem_d_mc(d,8)
	bx:=genreg(opndstack[noperands])

	genmc(m_mov,ax,bx)
end

global proc genmc_storemem_d16(ref strec d)=
	var ref opndrec ax,bx1,bx2

	ax:=genmem_d_mc(d,8)
	bx1:=genreg(opndstack[noperands])
	bx2:=genreg(opndreg2[noperands])

	genmc(m_mov,ax,bx1)
	genmc(m_mov,applyoffset(ax,8),bx2)
end

global proc genmc_storemem_x8(ref strec d)=
	var ref opndrec ax,bx

	ax:=genmem_d_mc(d,8)
	bx:=genxreg(opndstack[noperands])

	genmc(m_movq,ax,bx)
end

global proc genmc_storemem_x4(ref strec d)=
	var ref opndrec ax,bx

	ax:=genmem_d_mc(d,4)
	bx:=genxreg(opndstack[noperands])

	genmc(m_movd,ax,bx)
end

global proc genmc_storemem_d124(ref strec d,int m)=
	var ref opndrec ax,bx

	ax:=genmem_d_mc(d,ttsize[m])
	bx:=genreg(opndstack[noperands],ttsize[m])
	genmc(m_mov,ax,bx)
end

global proc genmc_storeretval_d8(int offset)=
!offset is offset of special return value slot, from 1st parameter
!will be zero when no other params
	var ref opndrec ax,bx

	if framebytes=0 and parambytes=0 then
		ax:=genireg(rstack)
		ax^.offset:=offset+8			!allow for return address
	else
		ax:=genireg(rframe)
		ax^.offset:=offset+16			!allow for frameptr+return address
	fi

	bx:=genreg(opndstack[noperands])

	genmc(m_mov,ax,bx)
end

global proc genmc_storeretval_d16(int offset)=
	var ref opndrec ax,bx1,bx2

	if framebytes=0 and parambytes=0 then
		ax:=genireg(rstack)
		ax^.offset:=offset+8			!allow for return address
	else
		ax:=genireg(rframe)
		ax^.offset:=offset+16			!allow for frameptr+return address
	fi

	bx1:=genreg(opndstack[noperands])
	bx2:=genreg(opndreg2[noperands])

	genmc(m_mov,ax,bx1)
	genmc(m_mov,applyoffset(ax,8),bx2)
end

global proc genmc_storeretval_x8(int offset)=
!offset is offset of special return value slot, from 1st parameter
!will be zero when no other params
	var ref opndrec ax,bx

	ax:=genireg(rframe)
	ax^.offset:=offset+16				!allow for return address

	bx:=genxreg(opndstack[noperands])

	genmc(m_movq,ax,bx)
end

global proc genmc_storememx(ref strec d,int m)=
!-x is extending narrow types
	var ref opndrec ax,bx

	ax:=genmem_d_mc(d,ttsize[m])
	bx:=genreg(opndstack[noperands],ttsize[m])

	genmc(m_mov,ax,bx)
end

global proc genmc_storememw(ref strec d)=
	GERROR("MC/STOREMEMW")
end

global proc getopnds(int n)=
!pop last n operands
	var int reg
	var ref opndrec ax

	if n>noperands then
		gerrorc("getopnds stack underflow")
	fi

	for i:=noperands downto noperands-n+1 when opndstack[i]=rtos do

		case opndkind[i]
		when 'I' then
			reg:=getnextreg()
			genmc(m_pop,genreg(reg))
			opndstack[i]:=reg
		when 'W' then
			reg:=getnextreg()
			genmc(m_pop,genreg(reg))
			opndstack[i]:=reg
			reg:=getnextreg()
			genmc(m_pop,genreg(reg))
			opndreg2[i]:=reg
		when 'F' then
			reg:=getnextxreg()

			ax:=genreg(r13)
			genmc(m_pop,ax)
			genmc(m_movq,genxreg(reg),ax)

			opndstack[i]:=reg
			opndkind[i]:='F'
		else
			gerror("getopnds/block?")
		esac
	od
end

global proc getopndn_d8(int n)=
!get operand at offet n from top
!operands must be fetched in top-down order, not at random
	var int reg, nopnd
	nopnd:=noperands-n
	if n<1 then
		gerrorc("n:opstack underflow2")
		return
	fi
	reg:=opndstack[nopnd]

	if reg=rtos then
		reg:=getnextreg()

		genmc(m_pop,genreg(reg))
		opndstack[nopnd]:=reg
		opndkind[nopnd]:='I'
	fi
end

global proc popopnd=
!lose operand from operand stack
	var int reg

	if noperands<=0 then
		gerrorc("NO OPS: popopnd")
		return
	fi
	reg:=opndstack[noperands]

	case opndkind[noperands]
	when 'I' then
		if reg=rtos then
			popstack_mc(8)
		else
			freereg(reg)
		fi

	when 'F' then
		if reg=rtos then
			popstack_mc(8)
		else
			freexreg(reg)
		fi
	when 'W' then
		if reg=rtos then
			popstack_mc(16)
		else
			freereg(reg)
			freereg(opndreg2[noperands])
		fi
	when 'B' then
		gerror("popopnds/B?")
	esac

	--noperands

end

global proc poparg=
	if noperands<=0 then
		gerror("poparg?")
	fi

	if opndstack[noperands]<>rtos then
		gerror("poparg/arg not on stack")
	fi
	--noperands
end

global proc newretvalopnd_d8=
!all regs should be free; create a new operand for the value left in D0
!after calling a foreign function

	regset[r0]:=1
	++noperands
	opndstack[noperands]:=r0
	opndkind[noperands]:='I'
end

global proc newretvalopnd_d16=
!all regs should be free; create a new operand for the value left in D0
!after calling a foreign function

	regset[r0]:=1
	regset[r1]:=1
	++noperands
	opndstack[noperands]:=r0
	opndreg2[noperands]:=r1
	opndkind[noperands]:='W'
end

global proc newretvalopnd_x8=
!all regs should be free; create a new operand for the value left in XMM0
!after calling a foreign function

	xregset[r0]:=1
	++noperands
	opndstack[noperands]:=xr0
	opndkind[noperands]:='F'
end

global function getnextreg:int=
	to 2 do
		for r:=r0 to regmax do
			if regset[r]=0 then
				regset[r]:=1
				return r
			fi
		od
		pushalloperands()
	od
GERROR("NO FREE REGS")
	return r
end

global proc checktwofreeregs=
!ensure there are at least 2 free registers for a new d16 operand
!if not, then push everything 

	var int count:=0
	for r:=r0 to regmax do
		if regset[r]=0 then
			if ++count>=2 then
				return
			fi
		fi
	od
	pushalloperands()
end

function getnexttempreg:int=
	for r:=r0 to regmax do
		if regset[r]=0 then
			regset[r]:=1
			return r
		fi
	od
	gerror("No temp reg")
	return 0
end

function getnexttempxreg:int=
	for r:=r0 to xregmax do
		if xregset[r]=0 then
			xregset[r]:=1
			return r
		fi
	od
	gerror("No temp xreg")
	return 0
end

function getnextxreg:int=
	do
		for xr:=xr0 to xregmax do
			if xregset[xr]=0 then
				xregset[xr]:=1
				return xr
			fi
		od

		pushalloperands()
	od
	return 0
end

global proc freereg(int r)=
	regset[r]:=0
end

global proc freexreg(int xr)=
	xregset[xr]:=0
end

global proc gerrorc(ichar mess)=
CPL(mess)
	gencomment_mc(mess)
!	gerror(mess)
end

global function genopndind(int offset)ref opndrec=
	return genireg(opndstack[noperands-offset])
end

global function gettempopnd_d8:ref opndrec=
	return genreg(getnexttempreg())
end

global proc freetempopnd_d8(ref opndrec p)=
	freereg(p^.reg)
end

global function gettempreg_d8:int=
	return getnexttempreg()
end

global proc freetempreg_d8(int reg)=
	freereg(reg)
end

global function gettempopnd_x8:ref opndrec=
	return genxreg(getnexttempxreg())
end

global proc freetempopnd_x8(ref opndrec p)=
	freexreg(p^.reg)
end

global proc swapopnds(int n)=
!exchange top opndstack entry with n'th element down, ie. opndstack[noperands-n] etc
var int i:=noperands, j:=noperands-n

swap(opndstack[i],opndstack[j])
swap(opndreg2[i],opndreg2[j])
swap(opndsize[i],opndsize[j])
swap(opndkind[i],opndkind[j])
end

global function genopnd(int offset,size=8)ref opndrec=
!int, float, or low half of wide
	var int index
	index:=noperands-offset

	case opndkind[index]
	when 'I' then
		return genreg(opndstack[index],size)
	when 'F' then
		return genxreg(opndstack[index],size)
	when 'W' then
		return genreg(opndstack[index],size)
	else
		gerror("genopnd/bad kind2")
		return nil
	esac
end

global function genopndh(int offset)ref opndrec=
!top half of wide
	return genreg(opndreg2[noperands-offset],8)
end

proc asmstr(ichar s)=
	gs_str(dest,s)
end

proc asmchar(int c)=
	gs_char(dest,c)
end

proc asmint(int a)=
	gs_strint(dest,a)
end

!global proc genstring_asm(ichar s, int length,doterm)=
!!string table generated in ax pass, so is just text
!!this is target-specific, so should really be moved
!	var int i, c, seqlen
!	var ref char seq
!
!	if length=0 then
!		gendb_asm(0)
!		return
!	fi
!
!	seqlen:=0
!
!	to length do
!		c:=s++^
!		if c<32 or c>=127 or c='\"' then
!			if seqlen then
!				gendbstring_asm(seq, seqlen)
!				seqlen:=0
!			fi
!			gendb_asm(c)
!		else
!			if seqlen=0 then
!				seqlen:=1
!				seq:=s-1
!			else
!				++seqlen
!			fi
!		fi
!	od
!	if seqlen then
!		gendbstring_asm(seq,seqlen)
!	fi
!	if doterm then
!		gendb_asm(0)
!	fi
!end
!
!proc gendb_asm(int a)=
!	asmstr("	db ")
!	asmint(a)
!	gs_line(dest)
!
!!	genmc(m_db,genint_mc(a))
!end
!
!proc gendbstring_asm(ichar s, int length)=
!!string is printable, and doesn't include double quotes
!	asmstr("	db ")
!	asmchar('"')
!	gs_strn(dest,s,length)
!	asmchar('"')
!	gs_line(dest)
!!	genmc(m_db,genstrimm_mc(s,length))
!end
!
=== ma_genss.m 20/35 ===
import clib
import mlib
import oslib
!import ma_tables
import ma_decls
!import ma_lex
import ma_lib
import ma_objdecls

import mm_decls
import mm_tables
import mm_mcldecls
import mm_support
import mm_libmcl

VAR INT DEB
VAR REF WORD64 PPP

const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

var int rex
var int sizeoverride					!32=>16 switch
var int addroverride					!32=>16 switch
var int f2override						!xmm regs
var int f3override						!xmm regs

var ref opndrec extraparam

var int currseg=0
var ref dbuffer currdata				!copy of ss_idata or ss_code
var ref relocrec currrelocs
var int nrelocs

!var ref[]ref strec labeldeftable
var int instrno=2

VAR REF MCLREC CURRMCL

global proc genss=
var int index
var ref mclrec m

!CPL "GENSS",LABELNO

initlib(labelno)

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
	alineno:=m^.lineno iand 16777215
!CPL "WHILE LOOP",M,M.NEXTMCL
	doinstr(m,++index)
!CPL "DONE",M,M.NEXTMCL
	m:=m^.nextmcl
od

switchseg(0)					!update ss_currrelocs etc

if bufferlength(ss_zdata) then
	axerror("Zdata contains code or data")
fi
!CPL "DONE GENSS"
!for i to ss_nsymbols do
!	CPL I,SS_SYMBOLTABLE[I].NAME,":",
!		SS_SYMBOLTABLE[I].SEGMENT,
!		SEGMENTNAMES[SS_SYMBOLTABLE[I].SEGMENT]
!od

end

proc doinstr(ref mclrec m,int index)=
var ref opndrec a,b
var ref strec d,e
var int x,offset,shortjmp,n

buffercheck(currdata)

rex:=sizeoverride:=addroverride:=f2override:=f3override:=0

a:=m^.a
b:=m^.b

++instrno
alineno:=instrno

CASE M.OPCODE
WHEN M_COMMENT, M_BLANK THEN
	RETURN
ESAC

!CP "DOINSTR2",instrno,MCLNAMES[M.OPCODE]; IF A THEN CP " ",OPNDNAMES_MA[A.MODE], VALTYPENAMES[A.VALTYPE] FI; CPL
!IF PPP THEN CPL "DOINSTR",PPP^ FI
!
!OS_GETCH()

CURRMCL:=M


switch m^.opcode
when m_labelname then
!	d:=a^.labeldef
	case a.valtype
	when stringimm_val then
!		axerror("LABELNAME/STRING")
!IF DEB THEN CPL("LABELNAME/STRING"),A.SVALUE FI
	when def_val then

		d:=a.def
!CPL "*************LABELNAME",=D,=A.DEF,D.NAME,=D.ISGLOBAL

		d^.reftype:=back_ref
		d^.segment:=currseg
		d^.offset:=getcurrdatalen(6)

		if d.isglobal then
			getstindex(d)
		fi

		dofwdrefs(d)
	esac

when m_label then
!CPL "LABEL",A.LABELNO
	d:=labeldeftable[a.labelno]

	d^.reftype:=back_ref
	d^.segment:=currseg
	d^.offset:=getcurrdatalen(6)
	dofwdrefs(d)

when m_call then
	do_call(a)

when m_jmp then
	do_jmp(a,m)

when m_jmpcc then
!CPL "JMPCC",=B	!B.LABELNO
	d:=getdef(a,1)
!CPL "JMPCC1",D
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
!		genbyte(0xEB)
!		genrel8(a)
	fi

when m_db then
	genopnd(a,1)
when m_dw then
	genopnd(a,2)
when m_dd then
	genopnd(a,4)
when m_dq then
	genopnd(a,8)

when m_ddoffset then
	genrel32(a)

when m_segment then
	switchseg(a^.value)

when m_csegment then
	switchseg(code_seg)
when m_isegment then
	switchseg(idata_seg)
when m_zsegment then
	switchseg(zdata_seg)

when m_nop, m_halt then
	genbyte(mclcodes[m^.opcode])

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
	if a^.mode<>a_imm then axerror("retn?") fi
	genbyte(0xC2)
	genword(a^.value)

when m_push then
	do_push(a)

when m_pop then
	do_pop(a)

when m_inc, m_dec then
!CPL "----------INC/DEC"
!DEB IXOR:=1
!CPL =DEB
	do_inc(a,mclcodes[m^.opcode])

when m_neg, m_notx, m_mul, m_imul, m_div, m_idiv then
	do_neg(a,mclcodes[m^.opcode])

when m_add, m_sub, m_andx, m_orx, m_xorx, m_adc, m_sbb, m_cmp then
	do_arith(a,b, mclcodes[m^.opcode])

when m_mov then
!CPL "MOV1"
	do_mov(a,b)
!CPL "MOV2"
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
	if a^.mode=a_imm then
		n:=a^.value*mclcodes[m^.opcode]
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
	if a^.mode=a_imm then
		x:=a^.value
!		if x not in 1..16384 then axerror("align2") fi
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
	do_shift(a,b,mclcodes[m^.opcode])

when m_test then
	do_test(a,b)

when m_loopcx, m_loopz, m_loopnz then
	do_loop(a,mclcodes[m^.opcode])

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
	do_arithxmm(a,b,0xF3,mclcodes[m^.opcode])

when m_addsd, m_subsd, m_mulsd, m_divsd, m_sqrtsd, m_minsd, m_maxsd then
	do_arithxmm(a,b,0xF2,mclcodes[m^.opcode])

when m_andps,m_xorps then
	do_logicxmm(a,b,mclcodes[m^.opcode],4)

when m_andpd,m_xorpd then
	do_logicxmm(a,b,mclcodes[m^.opcode],8)

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
!	do_cmovcc(a,extraparam,b)
	do_cmovcc(m.cond, a,b)

when m_fsqrt,m_fsin,m_fcos,m_fsincos,m_fptan, m_fpatan,m_fabs,m_fchs then
	genbyte(0xD9)
	genbyte(mclcodes[m^.opcode])

when m_fld, m_fst, m_fstp then
	do_fmem(a,1,mclcodes[m^.opcode])

when m_fild, m_fist, m_fistp then
	do_fmem(a,0,mclcodes[m^.opcode])

when m_fadd, m_fsub, m_fmul, m_fdiv then
	genbyte(0xDE)
	genbyte(mclcodes[m^.opcode])

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
	genbyte(mclcodes[m^.opcode])

when m_movdqa, m_movdqu then
	do_movdqx(a,b,mclcodes[m^.opcode])

when m_finit then
	genbyte(0xDB)
	genbyte(0xE3)

when m_fldz, m_fld1, m_fldpi, m_fld2t, m_fld2e, m_fldlg2, m_fldln2 then
	genbyte(0xD9)
	genbyte(mclcodes[m^.opcode])

when m_popcnt then
	do_popcnt(a,b)

when m_comment, m_blank then

else
	println "*** Can't do opcode",mclnames[m^.opcode],"line",alineno
endswitch

end

proc genbyte(int x)=
currdata^.pcurr++^:=x
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

proc genopnd(ref opndrec a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
var ref char s
var int64 x
var int length

if size=0 then size:=a^.size fi
!IF DEB THEN CPL "GENOPND1",VALTYPENAMES[A.VALTYPE],=SIZE FI

case a.valtype
when stringimm_val then
	s:=a^.svalue
	length:=strlen(s)
	if length>100 then
		buffercheck(currdata,max(1024,length+1))
	fi
	while s^ do
		genbyte(s++^)
	od
	return
esac

if getdef(a) and size<=2 then
	axerror("8/16-BIT RELOC")
fi

!IF DEB THEN
!VAR REF STREC D
!D:=GETDEF(A)
!IF D THEN
!	CPL "-------------GENOPND",D.NAME
!FI
!FI

case size
when 1 then
	genbyte(a^.value)
when 2 then
	genword(a^.value)
when 4 then
!IF DEB THEN CPL "//GENOPND 4",VALTYPENAMES[A.VALTYPE] FI
	case a.valtype
	when intimm_val then
		gendword(a.value)
	when realimm_val then
!CPL "		OPND/REALIMM4"
		gendword(int32@(a.xvalue))
	when realmem_val then
CPL "		OPND/REALMEM4"
	when stringimm_val then
CPL "		OPND/STRINGIMM4"
	when def_val,label_val then
		genabs32(a)
	when name_val then
CPL "		OPND/NAME4"
	else
		cpl valtypenames[a.valtype]
		axerror("OPND/4/VALTYPE?")
	esac

when 8 then
!IF DEB THEN CPL "//GENOPND 8",VALTYPENAMES[A.VALTYPE] FI
	case a.valtype
	when intimm_val then
		genqword(a.value)
	when realimm_val then
!CPL "		OPND/REALIMM8",ALINENO
		genqword(int64@(a.xvalue))
	when realmem_val then
CPL "		OPND/REALMEM8",ALINENO
	when stringimm_val then
CPL "		OPND/STRINGIMM8"
	when def_val,label_val then
!IF DEB THEN CPL "DEF/LAB" FI
		genabs64(a)
	when name_val then
CPL "		OPND/NAME8"
	else
		cpl valtypenames[a.valtype]
		axerror("OPND/8/VALTYPE?")
	esac

esac
end

proc addrelocitem(int reloctype, ref strec d)=
var ref relocrec r
var int stindex, adjust

!IF DEB THEN CPL "ADDRELOC",D,D.NAME FI

!CPL "RELOC:"
stindex:=getstindex(d)

adjust:=4
if reloctype=addr64_rel then adjust:=8 fi

r:=pcm_alloc(relocrec.bytes)
r^.nextreloc:=currrelocs
r^.reloctype:=reloctype
r^.offset:=getcurrdatalen(1)-adjust
r^.stindex:=stindex

++nrelocs
currrelocs:=r
end

function getstindex(ref strec d)int=
!retrieve existing obj st index, or create new one
!CPL "GETSTINDEX",D.NAME,=D.ISGLOBAL,=NAMENAMES[D.NAMEID]

!IF DEB THEN CPL "GETSTINDEX" FI
if d^.stindex=0 then
	if ss_nsymbols>=ss_symboltablesize then
		extendsymboltable()
	fi
	d^.stindex:=++ss_nsymbols
	ss_symboltable^[d^.stindex]:=d


	if d.segment=0 then
		if d.nameid=dllprocid then
			d.segment:=code_seg
		fi
!		CPL "NEED TO SET SEGMENT:",D.NAME
	fi


fi
return d^.stindex
end

proc genrel32(ref opndrec a)=
!used by call/longjmp/ddoffset
var ref strec d

d:=getdef(a)

if d=nil then				!constant
!CPL "IS CONST"
	gendword(a^.value)
	return
fi

!IF DEB THEN CPL "GENREL32",D.NAME FI

case d^.reftype
when back_ref then
	if d^.segment<>currseg then
		axerror("Rel label across segments")			!might be Ok if treated as external?
	fi
!	gendword(d^.offset-(getcurrdatalen(2)+4))
	gendword(d^.offset-(getcurrdatalen(2)+4)+a.offset)
when fwd_ref then
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(3),rel32_rel)
!	gendword(0)
	gendword(a.offset)
else								!external symbol
!IF DEB THEN CPL "GENREL32/EXT"FI

!	gendword(0)				!this is probably just zero
	gendword(a.offset)		!this is probably just zero
	addrelocitem(rel32_rel,d)
esac
end

function getdef(ref opndrec a,int dneeded=0)ref strec =
	var ref strec d
!CPL "GETDEF",OPNDNAMES_MA[A.MODE],VALTYPENAMES[A.VALTYPE]

	if a.mode in [a_mem,a_imm] then
		case a.valtype
		when label_val then
			return labeldeftable[a.labelno]
		when def_val then
			d:=a.def
			if d.reftype=0 then
				if d.nameid<>dllprocid then
					d.reftype:=fwd_ref
				fi
			fi

!CPL ">>>>>>>>> GETDEF",D.NAME,NAMENAMES[D.NAMEID]
			return d
		esac
	fi
	if dneeded then				!must return a non-nil value
		println opndnames_ma[a.mode],valtypenames[a.valtype]
		axerror("getdef/no def")
	fi
	return nil
end

proc genabs32(ref opndrec a)=
!absolute refs to labels
var ref strec d

d:=getdef(a,1)

!CPL "GENABS32",D.NAME , =REFTYPENAMES[D.REFTYPE]

case d^.reftype
when back_ref then
!CPL "////BACK",=D.OFFSET,=A.OFFSET

	gendword(d^.offset+a.offset)
	addrelocitem(addr32_rel,d)

when fwd_ref then
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
	if d.nameid in [frameid,paramid] then
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

proc genabs64(ref opndrec a)=
!absolute refs to labels
var ref strec d

d:=getdef(a,1)

!IF DEB THEN CPL "************GENABS64",REFTYPENAMES[D.REFTYPE] FI

case d^.reftype
when back_ref then
!IF DEB THEN CPL "ABS64 BACK",=d^.offset,=A.OFFSET FI
	genqword(d^.offset+a.offset)
	addrelocitem(addr64_rel,d)

when fwd_ref then
!IF DEB THEN CPL "ABS64 FWD"FI
!	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(5),addr32_rel,currseg)
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
	if d.nameid in [frameid,paramid] then
		genqword(d.offset+a.offset)
	else
		genqword(a.offset)
		addrelocitem(addr64_rel,d)
	fi

else								!external symbol
!IF DEB THEN CPL "ABS64 EXT?" FI
	genqword(a.offset)				!this is probably just zero
	addrelocitem(addr64_rel,d)
esac
end

function getrel32(ref strec d,int offset)int=
!get rel difference between offset in this segment, and label d

!IF DEB THEN CPL "GETREL32",D.NAME,REFTYPENAMES[D.REFTYPE],OFFSET FI

if d^.reftype=back_ref then					!defined earlier in this segment
	if d^.segment<>currseg then
		axerror("Rel label across segments2")
	fi
	return d^.offset-(offset+1)
else
	return int32.maxvalue
fi
end

proc dofwdrefs(ref strec d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
!	d^.fwdrefs append:=(getcurrdatalen(),rel32_rel)
var ref fwdrec f
var int offset, seg
var ref byte p8
var ref int32 p32
var ref int64 p64
var ref dbuffer data

!IF DEB THEN CPL "DOFWDREFS",D.NAME FI

if d^.fwdrefs=nil then return fi

f:=d^.fwdrefs

while f do
	offset:=f^.offset

	case f^.reltype
	when rel32_rel then
		p32:=bufferelemptr(currdata,offset)
		p32^:=d^.offset-offset-4

	when addr32_rel,addr64_rel then
		case f^.seg
		when code_seg then data:=ss_code
		when zdata_seg then axerror("Fwd ref in zdata")
		when idata_seg then data:=ss_idata
		esac

!IF DEB THEN CPL "DOFWDREF" FI
		p32:=bufferelemptr(data,offset)
		if f^.reltype=addr32_rel then
			p32^:=p32^+d^.offset
		else
			p64:=cast(p32)
			p64^:=p64^+d^.offset
		fi
	when rel8_rel then
		p8:=bufferelemptr(currdata,offset)
		p8^:=d^.offset-offset-1
	else
CPL RELOCNAMES[F^.RELTYPE]
		GERROR("DOFWDREFS/CAN'T DO RELTYPE")
	esac

	f:=f^.nextfwd

od
end

proc genrex=
	if sizeoverride then
		genbyte(0x66)
	fi
	if addroverride then
		genbyte(0x67)
	fi
	if rex then
		if rex<0x40 then
			genbyte(0x40+rex)
		else
			genbyte(rex)
		fi
	fi
end

function isbytesized(int64 x)int=
return -128<=x<=127
end

function isdwordsized(int64 x)int=
return int32.minvalue<=x<=int32.maxvalue
end

proc do_push(ref opndrec a)=
var int code,am

	case a^.mode
	when a_reg then
		if a^.size<>8 then axerror("pushreg not 64-bit") fi
		code:=regcodes[a^.reg]
		if code>=8 then
			rex :=bmask
			code iand:=7
		fi
		genrex()
		genbyte(0x50+code)

	when a_imm then
		if getdef(a) then
			genbyte(0x68)
			genopnd(a,4)
		elsif isbytesized(a^.value) then
			genbyte(0x6A)
			genbyte(a^.value)
		elsif isdwordsized(a^.value) then
			genbyte(0x68)
			gendword(a^.value)
		else
			axerror("push imm value too large")
		fi

	when a_mem then
		if a^.size<>8 then axerror("push not 64-bit") fi
		am:=genrm(a,6)
		genrex()
		genbyte(0xFF)
		genamode(a,am)
	else
		axerror("push opnd?")
	esac
end

proc do_pop(ref opndrec a)=
var int code, am

	case a^.mode
	when a_reg then
		if a^.size<>8 then axerror("popreg not 64-bit") fi
		code:=regcodes[a^.reg]
		if code>=8 then
			rex :=bmask
			code iand:=7
		fi
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a^.size<>8 then axerror("pop not 64-bit") fi
		am:=genrm(a,0)
		genrex()
		genbyte(0x8F)
		genamode(a,am)
	else
		axerror("pop opnd?")
	esac
end

proc do_inc(ref opndrec a,int code)=
!inc/dec
var int opc, am

	opc:=(a^.size=1|0xFE|0xFF)

	case a^.mode
	when a_reg, a_mem then
		am:=genrm(a,code)
		checkhighreg(a)
		setopsize(a)
		genrex()
		genbyte(opc)
		genamode(a,am)

	else
		axerror("inc/opnd?")
	esac
end

proc do_neg(ref opndrec a,int code)=
!neg/not/mul/imul/div/idiv
var int opc, am

	opc:=(a^.size=1|0xF6|0xF7)

	case a^.mode
	when a_reg, a_mem then
		am:=genrm(a,code)
		checkhighreg(a)
		setopsize(a)
		genrex()
		genbyte(opc)
		genamode(a,am)

	else
		axerror("neg/div/etc opnd?")
	esac
end

proc genamode(ref opndrec a,int am)=
var int sib,mode,dispsize,offset
var ref strec d

sib:=am>>16

mode:=(am>>8)iand 255
dispsize:=am iand 255

genbyte(mode)			!modrm byte

if sib>=0 then		!sib byte
	genbyte(sib)
fi

!CPL "GENAMODE",=DISPSIZE,A.VALUE

case dispsize			!disp bytes
when 0 then
when 1 then
	getdispsize(a,offset)
	genbyte(offset)
when 4 then

!IF DEB THEN CPL OPNDNAMES_MA[A.MODE],VALTYPENAMES[A.VALTYPE],=A.LABELDEF,A.VALUE,A.DEF FI
	case a.mode
	when a_mem then

		case a.valtype
		when def_val, label_val then
!CPL "GENAMODE/MEM",VALTYPENAMES[A.VALTYPE],A.DEF
!CPL "GENAMODE/ABS32"
			genabs32(a)
		when no_val then
			getdispsize(a,offset)
			gendword(offset)
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

function makemodrm(int mode,opc,rm)int=
	return mode<<6+opc<<3+rm
end

proc setopsize(ref opndrec a)=
case a^.size
when 1 then			!assume set via specific opcodes
when 2 then			!override default 4 bytes
	sizeoverride:=1
when 8 then			!override default 4 bytes
    rex ior:=wmask
when 4 then			!assume 4 bytes is default
else
	axerror("Operand size not set")
esac
end

proc setaddrsize(ref opndrec a)=
if a^.mode=a_mem and a^.addrsize=4 then
	addroverride:=1
fi
end

function getdispsize(ref opndrec a, int &offset)int=
!look at imm/mem displacement, and return 0,1 or 4
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	var ref strec d

	d:=getdef(a)
	offset:=a.offset

	if d then
!CPL "GDS1",OFFSET
		case d.nameid
		when frameid, paramid then
			offset+:=d.offset
		else
!CPL "GDS2",OFFSET
			return 4
		esac
	fi
!CPL "GDS3",OFFSET
	if offset then
!CPL "GDS4",OFFSET,(ISBYTESIZED(OFFSET)|1|4)
		return (isbytesized(offset)|1|4)
	else
!CPL "GDS5",OFFSET
		return 0
	fi
end

function genrm(ref opndrec a,int opc)int=
!work out modrm, and possible sib and address offset sequence from
!operand a (a_mem) and middle bits x (0..7) of the modrm byte
!returns: (modrm, sib, dispsize)
! sib = -1 means no sib byte
! dispsize is 0 (no disp), 1 (8-bit), or 4 (32-bit)
!will also set rex bits as needed
!!                         0  1  2  3  4  5  6  7
!static var var scaletable=(0: 0, 0, 1, 0, 2, 0, 0, 3)
!                       1  2  3  4  5  6  7  8
static var []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
var int mode, rm, scale, dispsize, sib, index, base
var int reg, regix, code, offset

!CPL "--------------------"
!CPL "GENRM",MCLNAMES[CURRMCL.OPCODE]

mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
scale:=0				!0=modrm only; 1/2/4/8 means sib used
dispsize:=0
!needsib:=0
sib:=-1

if a^.mode=a_mem and a^.addrsize=4 then
	addroverride:=1
fi

case a^.mode
when a_reg then			!modrm can only ref to a single register
	code:=getregcodeb(a^.reg)
!	code:=regcodes[a^.reg]
!	if code>=8 then
!		rex ior:=bmask
!		code iand:=7
!	fi

	return makeam(makemodrm(3,opc,code), sib, dispsize)
when a_mem then
!CPL "GENRMMEM"

when a_xreg then
!CPL "GENRM/XREG"
!	code:=getregcoderx(a^.reg)
	code:=getregcodebx(a^.reg)

!	return makeam(makemodrm(3,code,opc), sib, dispsize)		!OLD
	return makeam(makemodrm(3,opc,code), sib, dispsize)		!NEW

else
	axerror("genrm not mem")
esac

reg:=a^.reg
regix:=a^.regix


if reg=regix=0 then						!address only
	mode:=0
	rm:=4
	scale:=1
	index:=4
	base:=5
	dispsize:=4

elsif a^.scale<=1 and regix=0 then			!simple address mode (no sib)
	dispsize:=getdispsize(a,offset)
!CPL "HERE",=DISPSIZE
	if dispsize then
		mode:=(dispsize=1|1|2)
	fi

	rm:=regcodes[reg]

	if rm<>4 and rm<>12 then
		base:=rm
!		if reg=rframe and dispsize=0 then
		if (rm=5 or rm=13) and dispsize=0 then
			mode:=1; dispsize:=1
		fi
		index:=0
	else
		index:=4				!means no index
		base:=rm
		scale:=1				!force sib

	fi
elsif regix and reg=0 then
	dispsize:=4
	mode:=0
	rm:=4
	scale:=(a^.scale|a^.scale|1)
	base:=5
	index:=regcodes[regix]
	if regix=rstack then axerror("Scaled rstack?") fi

else										!assume regix used; optional reg and disp
!CPL "GENRM/DOING DISPSIZE"
	dispsize:=getdispsize(a,offset)
!CPL =DISPSIZE
	if dispsize then
		mode:=(dispsize=1|1|2)
	fi
	rm:=4

	scale:=(a^.scale|a^.scale|1)
	if reg=0 then
		base:=5
	else
		if reg=rframe and dispsize=0 then
			mode:=1; dispsize:=1
		fi
		base:=regcodes[reg]
	fi

	if regix=0 then
		index:=4
	else
		index:=regcodes[regix]
	fi

	if regix and not reg then
		dispsize:=4
	fi

	if regix=rstack and scale>1 then axerror("Can't scale rstack") fi

fi

if index>=8 then rex ior:= xmask; index iand:=7 fi
if base>=8  then rex ior:= bmask; base  iand:=7 fi

if scale then
	sib:=scaletable[scale]<<6 + index<<3 + base
fi
rm iand:=7

return makeam(makemodrm(mode:mode,opc:opc,rm:rm), sib, dispsize)
end

proc genrmbyte(int mode,opc,rm)=
	genbyte(mode<<6+opc<<3+rm)
end

function makeam(int m,s,d)int=
!convert mode, sib, dispsize into 32-bit value::
! ssssssss ssssssss mmmmmmmm dddddddd
!return m<<16+s<<8+d
!note: s can be -1, so allow to extend into sign bit::
return s<<16+m<<8+d
end

proc do_arith(ref opndrec a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
var int am, regcode, opc, dispsize
var int64 x

case a^.mode
when a_reg then
	case b^.mode
	when a_reg,a_mem then
		regcode:=getregcoder(a^.reg)
		am:=genrm(b,regcode)
		checkhighreg(a)
		checkhighreg(b)
!		genrex()
		setopsize(a)
		opc:=code<<3 ior (a^.size=1|0x02|0x03)
		genrex()
		genbyte(opc)
		genamode(b,am)

	when a_imm then
doregimm::
		if getdef(b) then
!			if code not in [0..7] then axerror("non-add arith/label") fi
			if code<0 or code>7 then axerror("non-add arith/label") fi
			if a^.size<4 then axerror("add imm/size") fi
			am:=genrm(a,code)
			setopsize(a)
			genrex()
			genbyte(0x81)
			genamode(a,am)
			genopnd(b,4)
			return

		fi

		x:=b^.value
		dispsize:=1
		if a^.size=1 then
			opc:=0x80
		elsif -128<=x<=127 then
			opc:=0x83
		else
			unless -0x8000'0000 <= x <= 0xFFFF'FFFF then axerror("3:exceeding word32 value") end
			opc:=0x81
			dispsize:=(a^.size=2|2|4)
		fi

		am:=genrm(a,code)
		checkhighreg(a)
		setopsize(a)
		genrex()
		genbyte(opc)
		genamode(a,am)
		case dispsize
		when 1 then genbyte(x)
		when 2 then genword(x)
		when 4 then gendword(x)
		esac

	else
		axerror("ADD reg,???")
	esac

when a_mem then
	case b^.mode
	when a_reg then
		regcode:=getregcoder(b^.reg)
		am:=genrm(a,regcode)
		checkhighreg(b)
		setopsize(b)
		opc:=code<<3 ior (b^.size=1|0x00|0x01)
		genrex()
		genbyte(opc)
		genamode(a,am)

	when a_imm then
		go to doregimm
	else
		axerror("ADD mem,???")
	esac

else
	axerror("Can't add to this opnd")
esac
end

proc do_mov(ref opndrec a,b)=
var int regcode, am
var int64 value

case a^.mode
when a_reg then
	case b^.mode
	when a_reg, a_mem then
!CPL "MOV/REG/MEM"
		if a^.size<>b^.size and b^.size then
CPL =A.SIZE,B.SIZE
			axerror("Opnd size mismatch")
		fi
		checkhighreg(a)
		checkhighreg(b)
		regcode:=getregcoder(a^.reg)
		am:=genrm(b,regcode)

		setopsize(a)
		genrex()
		genbyte((a^.size=1|0x8A|0x8B))
		genamode(b,am)

	when a_imm then
		value:=b^.value
		regcode:=getregcodeb(a^.reg)
		if getdef(b) and a^.size<=2 then axerror("mov imm?") fi
		case a^.size
		when 1 then
			checkhighreg(a)
			case a^.reg
			when r2,r3,r14,r15 then
				rex ior:=0x40
			esac
			unless -128<=value<=255 then axerror("exceeding byte value") end
			genrex()
			genbyte(0xB0+regcode)
			genbyte(value)

		when 2 then
!			if value not in -32768..65535 then axerror("exceeding word16 value") fi
			unless -32768<=value<=65535 then axerror("exceeding word16 value") end
			genbyte(0x66)
			genrex()
			genbyte(0xB8+regcode)
			genword(value)
		when 4 then
			if getdef(b) then
				genrex()
				genbyte(0xB8+regcode)
				genopnd(b,4)
			else
!				unless -0x8000'0000<=value<=0xFFFF'FFFFu then
				unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
CPL value,ref void(value)
					axerror("1:exceeding word32 value")
				end
doreg32::
				genrex()
				genbyte(0xB8+regcode)
				gendword(value)
			fi

		else							!assum 8 bytes
			if getdef(b) then
				rex ior:=wmask
				genrex()
				genbyte(0xB8+regcode)
				genopnd(b,8)
			else
				if value>=0 and value<=0xFFFF'FFFF then
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
	case b^.mode
	when a_reg then
		if a^.size<>b^.size and a^.size then
			axerror("Opnd size mismatch")
		fi
		regcode:=getregcoder(b^.reg)
		checkhighreg(b)
		am:=genrm(a,regcode)
		setopsize(b)
		genrex()
		genbyte((b^.size=1|0x88|0x89))
		genamode(a,am)

	when a_imm then
		value:=b^.value
		am:=genrm(a,0)
		if getdef(b) and a^.size<=2 then axerror("mov imm?") fi

		if a^.size=0 then a^.size:=1 fi

		case a^.size
		when 0,1 then
			unless -128<=value<=255 then axerror("exceeding byte value") end

			setopsize(a)
			genrex()
			genbyte(0xC6)
			genamode(a,am)
			genbyte(value)

		when 2 then
			unless -32768<=value<=65535 then axerror("exceeding word16 value") end
			setopsize(a)
			genrex()
			genbyte(0xC7)
			genamode(a,am)
			genword(value)
		when 4,8 then
			if not getdef(b) then
!				unless -0x8000'0000<=value<=0xFFFF'FFFF then axerror("2:exceeding word32 value") end
				unless -0x7FFF'FFFF<=value<=0xFFFF'FFFF then axerror("2:exceeding word32 value") end
			fi
			setopsize(a)
			genrex()
			genbyte(0xC7)
			genamode(a,am)
			genopnd(b,4)
!			gendword(value)
		esac

	else
		axerror("MOV MEM/?")
	esac
else
	axerror("MOV ?/..")
esac
end

function getregcoder(int reg)int=
var int regcode

regcode:=regcodes[reg]
if regcode>=8 then
	regcode-:=8
	rex ior:=rmask
fi
return regcode
end

function getregcodeb(int reg)int=
var int regcode

regcode:=regcodes[reg]
if regcode>=8 then
	regcode-:=8
	rex ior:=bmask
fi
return regcode
end

function getregcodebx(int reg)int=
!do not translate reg code (I think, when xmm reg code etc)

var int regcode

regcode:=reg-1
if regcode>=8 then
	regcode-:=8
	rex ior:=bmask
fi
return regcode
end

function getregcoderx(int reg)int=
!do not translate reg code (I think, when xmm reg code etc)
var int regcode

regcode:=reg-1
if regcode>=8 then
	regcode-:=8
	rex ior:=rmask
fi
return regcode
end


proc do_lea(ref opndrec a,b)=
var int regcode, am

unless a^.mode=a_reg and b^.mode=a_mem then
	axerror("LEA not reg/mem")
end

if a^.size<4 then axerror("LEA size error") fi
regcode:=getregcoder(a^.reg)

am:=genrm(b,regcode)
setopsize(a)
genrex()
genbyte(0x8D)
genamode(b,am)

end

proc do_movsx(ref opndrec a,b,int opc)=
!opc=B6 for movzx, and BE for movsx
var int am, regcode

!CPL "MS1"

if a^.mode<>a_reg then axerror("movsx not reg") fi
!if a^.size=1 or a^.size<=b^.size then axerror("movsx size error") fi
!CPL "MS1a"

if a^.size=8 and b^.size=4 then
!CPL "MS1b"
	if opc=0xBE then
!CPL "MS1c"
		do_movsxd(a,b)
	else						!movsx 4->8 bytes, do normal move 4->4
		a:=regtable[a^.reg,4]
		do_mov(a,b)
	fi
	return
fi

!CPL "MS2"
!if (opc=0xBE and a^.size=8) or a^.size=1 or a^.size<=b^.size then axerror("movsx size error") fi
if a^.size=1 or a^.size<=b^.size then axerror("movsx size error") fi

if opc=0xB6 and b^.size=4 then axerror("movsx 4=>8 bytes?") fi
!CPL "MS3"

case b^.mode
when a_reg then
when a_mem then
	if b^.size=0 then axerror("movsx need size prefix") fi
	if b^.size=8 then axerror("movsx size 8") fi
else
	axerror("movsx not reg/mem")
esac
!CPL "MS4"

regcode:=getregcoder(a^.reg)

am:=genrm(b,regcode)
!CPL "MS5"
setopsize(a)
genrex()
genbyte(0x0F)
genbyte((b^.size=1|opc|opc+1))
genamode(b,am)
!CPL "MS9"
end

proc checkhighreg(ref opndrec a)=
if a^.mode=a_reg then
	case a^.reg
	when r2,r3,r14,r15 then
		rex ior:=0x40
	esac
fi
end

proc do_exch(ref opndrec a,b)=
var int regcode, am

if a^.mode=a_reg and b^.mode=a_reg and (a^.reg=r0 or b^.reg=r0) and a^.size<>1 then		!simple r0/reg
	if a^.reg<>r0 then				!get a to be r0
		swap(a,b)
	fi
	if a^.size<>b^.size then axerror("exch size") fi

	setopsize(a)
	regcode:=getregcodeb(b^.reg)
	genrex()
	genbyte(0x90+regcode)
	return
fi

if a^.mode=a_mem then swap(a,b) fi

unless a^.mode=a_reg and (b^.mode=a_reg or b^.mode=a_mem) then axerror("exch opnds") end
if b^.size=0 and b^.mode=a_mem then b^.size:=a^.size fi
if a^.size<>b^.size then axerror("exch size") fi

if a^.size=1 then
	checkhighreg(a)
	checkhighreg(b)
fi

regcode:=getregcoder(a^.reg)

am:=genrm(b,regcode)
setopsize(a)
genrex()
genbyte((a^.size=1|0x86|0x87))
genamode(b,am)

end

proc do_movsxd(ref opndrec a,b)=
var int regcode, am

if b^.mode=a_mem and b^.size=0 then b^.size:=4 fi

if a^.size<>8 or b^.size>4 then axerror("movsxd size") fi

!if a^.mode<>a_reg or b^.mode not in [a_reg,a_mem] then
if a^.mode<>a_reg or (b^.mode<>a_reg and b^.mode<>a_mem) then
	axerror("movsxd opnds")
fi

regcode:=getregcoder(a^.reg)
am:=genrm(b,regcode)

setopsize(a)
genrex()
genbyte(0x63)
genamode(b,am)

end

proc do_imul2(ref opndrec a,b)=
var int regcode, am, opc
var int64 value

if a^.mode<>a_reg then
	axerror("imul2 opnds")
fi
if b^.size=0 then b^.size:=a^.size fi
if a^.size=1 then axerror("imul2 byte") fi

case b^.mode
when a_reg,a_mem then
	if a^.size<>b^.size then axerror("imul2 size") fi
	regcode:=getregcoder(a^.reg)
	am:=genrm(b,regcode)

	setopsize(a)
	genrex()
	genbyte(0x0F)
	genbyte(0xAF)
	genamode(b,am)

when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
	if getdef(b) then axerror("mul/label") fi
	value:=b^.value
	regcode:=getregcoder(a^.reg)		!same reg used in two places
	regcode:=getregcodeb(a^.reg)
	opc:=0xC0+regcode<<3+regcode
	setopsize(a)
	genrex()

	if -128<=value<=127 then
		genbyte(0x6B)
		genbyte(opc)
		genbyte(value)
	elsif a^.size=2 then
		genbyte(0x69)
		genbyte(opc)
		genword(value)
	else
		genbyte(0x69)
		genbyte(opc)
		gendword(value)
	fi
else
	axerror("imul2 opnds")
esac
end

proc do_imul3(ref opndrec a,b,c)=
var int64 value
var int regcode1, regcode2, opc

if a^.mode<>a_reg or b^.mode<>a_reg then
	axerror("imul3 opnds")
fi
if a^.size=1 then axerror("imul3 byte") fi
if c^.mode<>a_imm then axerror("imul3 not imm") fi

value:=c^.value
regcode1:=getregcoder(a^.reg)
regcode2:=getregcodeb(b^.reg)
opc:=0xC0+regcode1<<3+regcode2
setopsize(a)
genrex()

if -128<=value<=127 then
	genbyte(0x6B)
	genbyte(opc)
	genbyte(value)
elsif a^.size=2 then
	genbyte(0x69)
	genbyte(opc)
	genword(value)
else
	genbyte(0x69)
	genbyte(opc)
	gendword(value)
fi
end

proc do_shift(ref opndrec a,b,int opc)=
var int am, w

if a^.mode<>a_reg and a^.mode<>a_mem then axerror("shift opnds1?") fi

am:=genrm(a,opc)
checkhighreg(a)
setopsize(a)
genrex()
w:=(a^.size=1|0|1)

case b^.mode
when a_imm then
	if getdef(b) then axerror("shift/label") fi
	if b^.value=1 then
		genbyte(0xD0+w)
		genamode(a,am)
	else
		genbyte(0xC0+w)
		genamode(a,am)
		genbyte(b^.value)
	fi
when a_reg then
	if b^.reg<>r10 or b^.size<>1 then axerror("cl or b10 needed") fi
	genbyte(0xD2+w)
	genamode(a,am)

else
	axerror("shift opnds2?")
esac
end

proc do_test(ref opndrec a,b)=
var int64 value
var int opc, am, regcode

if a^.mode=a_reg and a^.reg=r0 and b^.mode=a_imm then
	value:=b^.value
	case a^.size
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

elsif (a^.mode=a_reg or a^.mode=a_mem) and b^.mode=a_imm then
	opc:=(a^.size=1|0xF6|0xF7)
	value:=b^.value

	am:=genrm(a,0)
	checkhighreg(a)
	setopsize(a)
	genrex()
	genbyte(opc)
	genamode(a,am)
	case a^.size
	when 1 then
		genbyte(value)
	when 2 then
		genword(value)
	else
		gendword(value)
	esac

elsif a^.mode=a_reg and (b^.mode=a_reg or b^.mode=a_mem) then
doregmem::
	regcode:=getregcoder(a^.reg)
	am:=genrm(b,regcode)
	checkhighreg(a)
	checkhighreg(b)
!	genrex()
	setopsize(a)
	genrex()
	genbyte((a^.size=1|0x84|0x85))
	genamode(b,am)

elsif a^.mode=a_mem and b^.mode=a_reg then
	swap(a,b)
	goto doregmem
else
	axerror("test opnds")
fi

end

proc do_loop(ref opndrec a,int opc)=
var int offset

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

proc do_jcxz(ref opndrec a,int opsize)=
var int offset

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

proc do_setcc(int cond, ref opndrec a)=
!a is cond
!b is byte reg/mem
var int am

!CPL "****************SETCC",INSTRNO
if (a^.mode<>a_reg and a^.reg<>a_mem) or a^.size>1 then axerror("setcc opnd/size") fi

am:=genrm(a,0)
checkhighreg(a)
genrex()
!setopsize(1)
genrex()
genbyte(0x0F)
genbyte(0x90+cond)
genamode(a,am)
end

proc do_movxmm(ref opndrec a,b,int size)=
!do movd/movq depending on size being 4 or 8
var int am, regcode, regcode1, regcode2

case a^.mode
when a_reg then
	case b^.mode
	when a_xreg then
		if a^.size<>size then axerror("1:movdq size") fi

!		regcode:=getregcodeb(a^.reg)
!		am:=genrm(b,regcode)
!		setopsize(a)
!		genbyte(0x66)
!		genrex()
!		genbyte(0x0F)
!		genbyte(0x7E)
!		genamode(b,am)

		regcode:=getregcoderx(b^.reg)
		am:=genrm(a,regcode)
		setopsize(a)
		genbyte(0x66)
		genrex()
		genbyte(0x0F)
		genbyte(0x7E)
		genamode(b,am)

	else
		axerror("movdq reg,?")
	esac
when a_xreg then
	case b^.mode
	when a_reg then
!CPL "MOVXMM XREG/REG"
!		if b^.size<>size then axerror("2:movdq size") fi
!		regcode:=getregcodeb(b^.reg)
!		am:=genrm(a,regcode)
!		setopsize(b)
!		genbyte(0x66)
!		genrex()
!		genbyte(0x0F)
!		genbyte(0x6E)
!		genamode(a,am)

		if b^.size<>size then axerror("3:movdq size") fi
		regcode:=getregcoderx(a^.reg)
		am:=genrm(b,regcode)
		setopsize(b)
		genbyte(0x66)
		genrex()
		genbyte(0x0F)
		genbyte(0x6E)
		genamode(a,am)

	when a_xreg then
		regcode1:=getregcoderx(a^.reg)
		regcode2:=getregcodebx(b^.reg)
		genbyte(0xF3)
		genrex()
		genbyte(0x0F)
		genbyte(0x7E)
		genbyte(0xC0+regcode1<<3+regcode2)

	when a_mem then
		if b^.size and b^.size<>size then axerror("4:movdq size") fi
		regcode:=getregcoderx(a^.reg)
		am:=genrm(b,regcode)
		if size=4 then
			genbyte(0x66)
			genrex()
			genbyte(0x0F)
			genbyte(0x6E)
		else
			genbyte(0xF3)
			genrex()
			genbyte(0x0F)
			genbyte(0x7E)
		fi
		genamode(b,am)

	else
		axerror("movdq xreg,?")
	esac
when a_mem then
	case b^.mode
	when a_xreg then
		if a^.size and a^.size<>size then axerror("5:movdq size") fi
		regcode:=getregcoderx(b^.reg)
		am:=genrm(a,regcode)
		if size=4 then
			genbyte(0x66)
			genrex()
			genbyte(0x0F)
			genbyte(0x7E)
		else
			genbyte(0x66)
			genrex()
			genbyte(0x0F)
			genbyte(0xD6)
		fi
		genamode(a,am)

	else
		axerror("movdq mem,?")
	esac
else
	axerror("movdq opnds")
esac

end

proc do_arithxmm(ref opndrec a,b,int prefix,opc)=
var int am, regcode

!if a^.mode<>a_xreg or b^.mode not in [a_xreg, a_mem] then
if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	axerror("arithxmm opnds")
fi

if b^.mode=a_xreg then

!	regcode:=getregcodebx(b^.reg)
!	am:=genrm(a,regcode)
!	if prefix then genbyte(prefix) fi
!	genrex()
!	genbyte(0x0F)
!	genbyte(opc)
!	genamode(a,am)

	regcode:=getregcoderx(a^.reg)
	am:=genrm(b,regcode)
	if prefix then genbyte(prefix) fi
	genrex()
	genbyte(0x0F)
	genbyte(opc)
	genamode(a,am)
else
	regcode:=getregcoderx(a^.reg)
	am:=genrm(b,regcode)
	if prefix then genbyte(prefix) fi
	genrex()
	genbyte(0x0F)
	genbyte(opc)
	genamode(b,am)
fi
end

proc do_logicxmm(ref opndrec a,b,int opc,size)=
var int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	axerror("logicxmm opnds")
fi

if size=8 then
	genbyte(0x66)
fi

if b^.mode=a_xreg then
	regcode:=getregcodebx(b^.reg)
	am:=genrm(a,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(opc)
	genamode(a,am)
else
	regcode:=getregcoderx(a^.reg)
	am:=genrm(b,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(opc)
	genamode(b,am)
fi
end

proc do_convertfloat(ref opndrec a,b,int prefix)=
!cvtss2sd and cvtsd2ss
var int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	axerror("convertfloat opnds")
fi

genbyte(prefix)

if a^.mode=a_xreg then
!CPL "CF1"
!	regcode:=getregcodebx(b^.reg)
	regcode:=getregcodeRx(a^.reg)
	am:=genrm(b,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(0x5A)
	genamode(b,am)
else
!CPL "CF2"
	regcode:=getregcoderx(b^.reg)
	am:=genrm(a,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(0x5A)
	genamode(b,am)
fi
end

proc do_fix(ref opndrec a,b,int prefix,opc)=
!cvtss2si and cvtsd2si opc=2d
!cvttss2si and cvttsd2si opc=2c
!
var int am, regcode

if a^.mode<>a_reg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	axerror("fix opnds")
fi

genbyte(prefix)

if b^.mode=a_xreg then
	regcode:=getregcoder(a^.reg)
	am:=genrm(b,regcode)
	setopsize(a)
else
	regcode:=getregcoder(a^.reg)
	am:=genrm(b,regcode)
	setopsize(a)
fi

genrex()
genbyte(0x0F)
genbyte(opc)
genamode(b,am)
end

proc do_float(ref opndrec a,b,int prefix)=
!cvtss2si and cvtsd2si
var int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_reg and b^.mode<>a_mem) then
	axerror("float opnds")
fi

if b^.mode=a_mem then
	if b^.size=0 then b^.size:=4 fi
	if b^.size<>4 and b^.size<>8 then axerror("float size") fi
fi

genbyte(prefix)

regcode:=getregcoderx(a^.reg)
am:=genrm(b,regcode)
setopsize(b)
genrex()
genbyte(0x0F)
genbyte(0x2A)
genamode(b,am)
end

proc do_call(ref opndrec a)=
var int am, regcode
	case a^.mode
	when a_imm then
		genbyte(0xE8)
		genrel32(a)
	else				!indirect call
		case a^.size
		when 0 then a^.size:=8
		when 1,2,4 then
			axerror("call[]size")
		esac
		am:=genrm(a,2)
		setopsize(a)
		setaddrsize(a)
		genrex()
		genbyte(0xFF)
		genamode(a,am)

	esac
end

proc do_jmp(ref opndrec a,ref mclrec m)=
	var int am, regcode, offset, shortjmp
	var ref strec d

!CPL "JMP1",M.NEXTMCL,OPNDNAMES_MA[A.MODE], VALTYPENAMES[A.VALTYPE]
!CPL; STOP
!RETURN
	case a^.mode
	when a_imm then				!assume label_val
		case a.valtype
		when label_val,def_val then
			d:=getdef(a,1)
!			offset:=getrel32(d,getcurrdatalen(11)+1)
			offset:=getrel32(d,getcurrdatalen(11)+1)+a.offset
			if offset<0 and offset>-126 then
				genbyte(0xEB)
				genbyte(offset)
			else
				shortjmp:=0
				if offset>0 then				!fwd jump
!check if destlabel occurs within next 8 instrs, then likely to need short disp
!					shortjmp:=checkshortjump(m,d)
				fi

				if not shortjmp then
					genbyte(0xE9)
					genrel32(a)
				else
					genbyte(0xEB)
					genrel8(a)
				fi
			fi
		else
CPL VALTYPENAMES[A.VALTYPE]
			AXERROR("JMP/IMM NOT LABELNO")
		esac
	else				!indirect jump
		case a^.size
		when 0 then a^.size:=8
		when 1,2,4 then
			axerror("jmp[]size")
		esac
		am:=genrm(a,4)
		setopsize(a)
		setaddrsize(a)
		genrex()
		genbyte(0xFF)
		genamode(a,am)
	esac
!CPL "JMP2",M.NEXTMCL

end

function getcurrdatalen(int id)int=
!I think that zdata-seg is only likely when id=6

if currseg=zdata_seg then
	return ss_zdatalen
fi
return bufferlength(currdata)
end

proc do_cmovcc(int cond, ref opndrec a,b)=
var int am, regcode
	if a^.size<>b^.size and b^.size then
		axerror("Opnd size mismatch")
	fi
	if a^.size=1 then axerror("cmov/byte") fi
	regcode:=getregcoder(a^.reg)
	am:=genrm(b,regcode)

	setopsize(a)
	genrex()
	genbyte(0x0F)
	genbyte(0x40+cond)
	genamode(b,am)
end

proc do_fmem(ref opndrec a, int freal, code)=
!do fld/fild/fst/fstp/fist,fistp
!freal=1 for fld/etc, 0 for fild etc
!code is middle 3 bits of 2nd byte: 0=load, 2=store, 3=store+pop
var int am, regcode, mf

if a^.mode<>a_mem then
	axerror("fmem/not mem")
fi

if freal then
!CPL =CODE
	case a^.size
	when 4 then mf:=0
	when 8 then mf:=2
	when 16 then
		mf:=1
		case code
		when 0 then code:=5
		when 3 then code:=7
		else
			axerror("r80 not allowed")
		esac
	else
CPL "SIZE=",A^.SIZE
		axerror("fmem size")
	esac
else
	case a^.size
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

am:=genrm(a,code)
genrex()
genbyte(0xD9+mf<<1)
genamode(a,am)
!CPL "DONE FMEM"
end

function getr32bits(real x)int=
!when x is real, convert to real32 then return 32-bit bit pattern
var real32 sx:=x
return int32@(x)
end

proc genrel8(ref opndrec a)=
!a is a known fwd reference, and expected to be <=127 bytes
var ref strec d

!CPL "GENREL8"
d:=getdef(a,1)
!CPL =D.NAME,D.REFTYPE

if d^.reftype=fwd_ref then
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(3),rel8_rel)
	genbyte(0)
else								!external symbol
	axerror("genrel8")
fi
end

function checkshortjump(ref mclrec m,ref strec d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
!return 0
! d can be a named label, or a labelno; either should have .labelno set
var int n

RETURN 0
n:=0
m:=m^.nextmcl
while m and n<=8 do
	case m.opcode
	when m_label then
!		++n
		if m.a.labelno=d.labelno then
			return 1
		fi
	when m_comment, m_blank then
	else
		++n
	esac
!!while m and n<=12 do
!	++n
!!CPL "CHECKSHORT",MCLNAMES[M.OPCODE],(M.A|M.A.LABELNO|0), D.LABELNO
!	if m^.opcode=m_label and m.a.labelno=d.labelno then
!		return 1
!	fi

	m:=m^.nextmcl
od

return 0
end

function addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
var ref fwdrec q

q:=pcm_alloc(fwdrec.bytes)
q^.nextfwd:=p
q^.offset:=offset
q^.reltype:=reltype
q^.seg:=seg
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

proc do_movdqx(ref opndrec a,b, int opc)=
var int am,regcode

case a^.mode
when a_xreg then
	case b^.mode
	when a_xreg then
		regcode:=getregcodebx(b^.reg)
		am:=genrm(a,regcode)
		genbyte(opc)
		genrex()
		genbyte(0x0F)
		genbyte(0x6F)
		genamode(a,am)

	when a_mem then
		regcode:=getregcoderx(a^.reg)
		am:=genrm(b,regcode)
		genbyte(opc)
		genrex()
		genbyte(0x0F)
		genbyte(0x6F)
		genamode(b,am)

	else
		axerror("movdqx?")
	esac
when a_mem then
	case b^.mode
	when a_xreg then
		regcode:=getregcoderx(b^.reg)
		am:=genrm(a,regcode)
		genbyte(opc)
		genrex()
		genbyte(0x0F)
		genbyte(0x7F)
		genamode(a,am)

	else
		axerror("movdqx")
	esac
else
	axerror("movdqx")
esac

end

proc do_popcnt(ref opndrec a,b)=
var int am, regcode

if b^.mode=a_mem then
	if b^.size=0 then b^.size:=8 fi
fi

genbyte(0xF3)

!regcode:=getregcoderx(a^.reg)
regcode:=getregcodebx(a^.reg)
am:=genrm(b,regcode)
setopsize(a)
genrex()
genbyte(0x0F)
genbyte(0xB8)
genamode(b,am)
end

proc extendsymboltable=
	var ref[]ref strec oldsymboltable
	var int oldsymboltablesize

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

PROC SHOWOPND(ICHAR CAPTION,REF OPNDREC A)=
CPL CAPTION,OPNDNAMES_MA[A.MODE],VALTYPENAMES[A.VALTYPE]
END
=== ma_decls.m 21/35 ===
!MXA Assembler Global Decls
import mm_decls

global const compilerversion="2018.1.22"

global record relocrec =			!informal version
	var ref relocrec nextreloc
	var int reloctype
	var int offset
	var int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
global record dbuffer =
	var ref byte pstart
	union
		var ref byte pcurr
		var ref word16 pcurr16
		var ref word32 pcurr32
		var ref word64 pcurr64
	end
	var ref byte pend
	var int alloc
end

global const maxsearchlibs=30
global var [maxsearchlibs]ichar searchlibs
global var int nmodules
global var int nsearchlibs

global var int fverbose=0		!whether to display message for each pass
global var int fquiet=0
!global var ichar entrypointname = "start"

global var int LINECOUNT=0

global var int nundefined=0
!global var int alineno=0

global var int ss_zdatalen
global var ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global var ref dbuffer ss_idata
global var ref dbuffer ss_code
global var ref relocrec ss_idatarelocs
global var ref relocrec ss_coderelocs
global var int ss_nidatarelocs
global var int ss_ncoderelocs

!const max_ss_symbols=32768				!exported to coff
!global const init_ss_symbols=32768				!exported to coff
global const init_ss_symbols=16384
global var ref []ref strec ss_symboltable
global var int ss_nsymbols
global var int ss_symboltablesize

!global var ref stlistrec globalimportlist		!all global vars and imports across all moduls

global var ref[]ref strec labeldeftable

!global var ref strec modulenamelist			!all defs defined in last module
global var int currmoduleno

GLOBAL VAR INT NMCLASM
GLOBAL VAR INT NMCLOPNDSASM

=== ma_lib.m 22/35 ===
import clib
import msys
import mlib
!import ma_tables
import ma_decls
!import ma_lex

import mm_decls
import mm_mcldecls
import mm_support
import mm_lib

const ptrsize=8

global var int currsegment=0		!

!global var opndrec dstackopnd
!global var opndrec dframeopnd

!global var ref opndrec zero_opnd=nil

var strbuffer destv
global var ref strbuffer dest=&destv

!global var [r0..r19, 1..8]ref opndrec regtable

TYPE FREDDY=INT

global proc initlib(int nlabels)=
var [256]char str

!CPL "INITLIB/MA"

ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
ss_symboltablesize:=init_ss_symbols
ss_nsymbols:=0

labeldeftable:=pcm_alloc(nlabels*ref void.bytes)
for i to nlabels do
	labeldeftable[i]:=newstrec()
	labeldeftable[i].labelno:=i
	fprint @&.str,"(L#)",i
	labeldeftable[i].name:=pcm_copyheapstring(&.str)
	labeldeftable[i].reftype:=fwd_ref
od

end

function getsizetag(int size)ichar=			!GETSIZETAG
case size
when 1 then return "b"
when 2 then return "h"
when 4 then return "w"
when 8 then return "d"
esac
AXERROR("GETSIZETAG?")
!return tostr(size)
return nil
end

global function buffercreate(int size=1024)ref dbuffer=
var ref dbuffer a

a:=pcm_alloc(dbuffer.bytes)

a^.alloc:=size
a^.pstart:=a^.pcurr:=pcm_alloc(a^.alloc)
a^.pend:=a^.pstart+a^.alloc
return a
end

proc bufferexpand(ref dbuffer a)=
var int newalloc,usedbytes
var ref byte p

newalloc:=a^.alloc*2
usedbytes:=a^.pcurr-a^.pstart

if usedbytes>a^.alloc then
	println "dbuffer error"
	stop
fi

p:=pcm_alloc(newalloc)
memcpy(p,a^.pstart,usedbytes)
a^.pstart:=p
a^.pcurr:=p+usedbytes
a^.alloc:=newalloc
a^.pend:=p+newalloc
end

global proc buffercheck(ref dbuffer a,int n=1024)=
while a^.pend-a^.pcurr<n do
	bufferexpand(a)
od
end

global function bufferlength(ref dbuffer a)int=
return a^.pcurr-a^.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=
return a^.pstart+offset
end

global proc addbyte(ref dbuffer a, int x)=
a^.pcurr^:=x
++a^.pcurr
end

global proc addword(ref dbuffer a, int x)=
a^.pcurr16^:=x
++a^.pcurr16
end

global proc adddword(ref dbuffer a, int x)=
a^.pcurr32^:=x
++a^.pcurr32
end

global proc addqword(ref dbuffer a, int64 x)=
a^.pcurr64^:=x
++a^.pcurr64
end

=== ma_objdecls.m 23/35 ===

global record imagefileheader =
	var word16	machine
	var word16	nsections
	var word32	timedatestamp
	var word32	symtaboffset
	var word32	nsymbols
	var word16	optheadersize
	var word16	characteristics
end

global record imagedir =
	var word32	virtualaddr
	var word32	size
end

global record optionalheader =			!exe/dll only
	var word16  magic
	var byte     majorlv
	var byte     minorlv
	var word32 codesize
	var word32 idatasize
	var word32 zdatasize
	var word32 entrypoint
	var word32 codebase
!	var word32 datebase		!32-bit exe files only
	var word64	imagebase
	var word32 sectionalignment
	var word32 filealignment
	var word16  majorosv
	var word16  minorosv
	var word16  majorimagev
	var word16  minorimagev
	var word16  majorssv
	var word16  minorssv
	var word32 win32version
	var word32 imagesize
	var word32 headerssize
	var word32 checksum
	var word16  subsystem
	var word16  dllcharacteristics
	var word64   stackreserve
	var word64   stackcommit
	var word64   heapreserve
	var word64   heapcommit
	var word32 loaderflags
	var word32 rvadims
	var imagedir exporttable
	var imagedir importtable
	var imagedir resourcetable
	var imagedir exceptiontable
	var imagedir certtable
	var imagedir basereloctable
	var imagedir debug
	var imagedir architecture
	var imagedir globalptr
	var imagedir tlstable
	var imagedir loadconfigtable
	var imagedir boundimport
	var imagedir iat
	var imagedir delayimportdescr
	var imagedir clrheader
	var imagedir reserved
end

global record imagesectionheader =
	var [8]char name
	union
		var word32	physical_address
		var word32	virtual_size
	end
	var word32	virtual_address
	var word32	rawdata_size
	var word32	rawdata_offset
	var word32	relocations_ptr
	var word32	linenos_offset
	var word16	nrelocs
	var word16	nlinenos
	var word32	characteristics
end

global record imagesymbol =
	union
		var [8]char shortname
		struct
			var word32	shortx
			var word32	longx
		end
		var word64 longname
	end
	var word32	value
	var int16	sectionno
	var word16	symtype
	var byte	storageclass
	var byte	nauxsymbols
end

global record importdirrec =
	var word32	implookuprva
	var word32	timedatestamp
	var word32	fwdchain
	var word32	namerva
	var word32	impaddressrva
end

global record coffrelocrec =
	var int32	virtualaddr
	var int32	stindex
	var int16	reloctype
end

global tabledata() [0:]ichar relocnames =
	(abs_rel = 0,	$),
	(addr64_rel,	$),
	(addr32_rel,	$),
	(addr32nb_rel,	$),
	(rel32_rel,		$),
	(rel321_rel,	$),
	(rel8_rel,		$),				!used within assembler only, not in coff format
end

global tabledata() [0:]ichar coffscopenames =
	(cofflocal_scope=0,	$),
	(export_scope,		$),
	(import_scope,		$),
end

global record auxsectionrec = 
	var int32 length
	var int16 nrelocs
	var int16 nlines
	var int32 checksum
	var int16 sectionno
	var int32 dummy
end
=== ma_writeobj.m 24/35 ===
!NEEDS REVISING TO MATCH UNLIMITED SS_SYMOLBOLTABLE size used for EXE
!and also unlimited strings

import clib
import mlib
import mm_decls
import ma_objdecls
import ma_decls
import mm_mcldecls
import mm_support
import mm_tables
!import ma_tables
import ma_lib

var fwdrec dummy1

const showdump=1
!const showdump=0

var int symtaboffset

var ref byte datastart
var ref byte dataptr

![0..ss_symboltable.len+10]imagesymbol symboltable		!needs a few more than ss set of symbols
var [0..10'000]imagesymbol symboltable		!needs a few more than ss set of symbols

var int nsymbols

var int stoffset=0				!usually +7 to convert ss_symboltable indices to symboltable

const maxstring=5000
var [maxstring]ichar stringtable
var [maxstring]int stringlengths
var int nextstringoffset=0
var int nstrings=0

global proc writess(ichar outfile)=
writecoff(outfile)
end

proc writerecord(ref void r, int length)=
memcpy(dataptr,r,length)
dataptr+:=length
end

proc writerelocs(ref relocrec r,int nrelocs)=
static var coffrelocrec s
var ref strec d

return when nrelocs=0

while r do
	case r^.reloctype
	when addr32_rel, addr64_rel then		!change to section entry
		d:=ss_symboltable^[r^.stindex]

		case d^.segment
		when zdata_seg then s.stindex:=2
		when idata_seg then s.stindex:=4
		when code_seg then s.stindex:=6
		when 0 then							!external; leave stindex pointing to symbol
			s.stindex:=r^.stindex+stoffset
		else
			gerror("wrelocs/bad seg")
		esac

	else
		s.stindex:=r^.stindex+stoffset
	esac

	s.reloctype:=r^.reloctype
	s.virtualaddr:=r^.offset

	memcpy(dataptr,&s,s.bytes)
	dataptr+:=s.bytes

	r:=r^.nextreloc
od
end

proc writedata(ref dbuffer data)=
memcpy(dataptr, bufferelemptr(data,0), bufferlength(data))
dataptr+:=bufferlength(data)
end

proc writesymboltable=
var int i
for i:=1 to nsymbols do
	writerecord(&symboltable[i],imagesymbol.bytes)
od
end

proc writestringtable=
!should immediately follow symboltable
var ref int32 p
var int i,n

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
static var imagesymbol r
var int length

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
if nsymbols>=symboltable.len then
	gerror("as:Too many symbols")
fi
memcpy(&symboltable[++nsymbols],r,r^.bytes)
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
static var imagesymbol r
var ref byte p:=cast(&r)
var int n

memset(p,0,r.bytes)

n:=0
while s^<>0 and n<r.bytes do
	p++^:=s++^
	++n
od

return &r
end

function sectiontoaux(ref dbuffer data, int nrelocs)ref auxsectionrec=
!!turn segment into into aux section/reloc entry for symboltable
static var auxsectionrec r

memset(&r,0,r.bytes)

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
var int offset

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
var ref strec s
var ichar name
var int i,sect, scope

stoffset:=nsymbols-1

nstrings:=0
nextstringoffset:=4

for i to ss_nsymbols do
	s:=ss_symboltable^[i]

	name:=s^.name
	if s.truename then
		name:=s.truename
	fi

	if name=nil then		!skip numbered labels
		next
	fi

	case s^.segment
	when zdata_seg then sect:=1
	when idata_seg then sect:=2
	when code_seg then sect:=3
	else sect:=0
	esac

!CPL "CONVERTSYMBOLS/FWDLOCAL ETC...",S.NAME,=S.ISGLOBAL,NAMENAMES[S.NAMEID]
	if s.isglobal then
		scope:=2
	else
		scope:=3
	fi

!	case s^.symbol
!	when fwdlocalsym,localsym then
!		scope:=3
!	when importedsym,exportedsym then
!		scope:=2
!	else
!		scope:=0
!	esac

	addsymbol(makesymbol(name,s^.namelen,sectionno:sect, storage:scope, value:s^.offset))
!	addsymbol(makesymbol("abc",3,sectionno:sect, storage:scope, value:s^.offset))

od
end

proc writecoff(ichar outfile)=
var imagefileheader header
var imagesectionheader zsection, isection, csection
var int offset
var int64 aa

memset(&header,0,header.bytes)
memset(&zsection,0,imagesectionheader.bytes)
memset(&isection,0,imagesectionheader.bytes)
memset(&csection,0,imagesectionheader.bytes)

header.machine:=0x8664
header.nsections:=3

!zsection:=new(imagesectionheader)
strcpy(&zsection.name[1],".bss")
zsection.rawdata_size:=ss_zdatalen


aa:=0xc040'0080
zsection.characteristics:=AA			!BUG in compiler or assemble; need to assign indirectly
!zsection.characteristics:=0xC040'0080

if ss_nidatarelocs>65536 or ss_ncoderelocs>65536 then
	gerror("Too many relocs (exceeds 16-bit field)")
fi

!isection:=new(imagesectionheader)
strcpy(&isection.name[1],".data")
isection.rawdata_size:=bufferlength(ss_idata)
isection.nrelocs:=ss_nidatarelocs

AA:=0xC050'0040
isection.characteristics:=AA
!isection.characteristics:=0xC050'0040

strcpy(&csection.name[1],".text")
csection.rawdata_size:=bufferlength(ss_code)
csection.nrelocs:=ss_ncoderelocs

AA:=0x6050'0020
csection.characteristics:=AA
!csection.characteristics:=0x6050'0020

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

!if fverbose then
	CPL "Writing file:",outfile
!fi
writefile(outfile,datastart,dataptr-datastart)

end

=== ma_writeexe.m 25/35 ===
!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order::
! initsectiontable()
! genexe()
! writeexe(filename)

import clib
import mlib
import oslib
import mm_decls
import mm_support
import ma_objdecls
import mm_tables
import ma_decls
import ma_lib
import ma_disasm
import mm_mcldecls

!const maxsearchlibs=30
!!for now, use a fixed, built-in set of search libs
![]ichar searchlibs=("msvcrt","gdi32","user32","kernel32")
!int nsearchlibs=searchlibs.len
var [maxsearchlibs]int64 libinsttable
var [maxsearchlibs]ichar libinstnames
var [maxsearchlibs]int libnotable			!index into dlltable

record sectionrec =
	union
		var ref dbuffer data		!copy of ss_zdata etc
		var ref byte bytedata		!added later, eg, import dir block
	end
	var ichar name					!name like ".bss" as it will be in obj/exe file
	var int segtype					!code_seg etc
	var int rawsize					!in file
	var int rawoffset				!offset in exe file
	var int virtsize				!in image
	var int virtoffset				!offset from imagebase
	var ref relocrec relocs			!for idata/code: reloc info needs to be processed
	var int nrelocs					!
end

record importrec = 				!details about all imported symbols
	var ref strec def				!full st entry
	var int libno					!which dll lib this belongs to
	var ichar name					!name of symbol (extracted from lib.name if needed)
	var int hintnameoffset			!voffset of hint/name entry in impdir section
	var int iatoffset				!voffset of IAT entry
	var int thunkoffset				!offset within code section of thunk entry
end

record dllrec =					!all imported libraries
	var ichar name					!name of library, including .dll
	var int nprocs					!no. of imports which use this library
	var int nametableoffset			!start of name table in impdir
	var int addrtableoffset			!start of addr table (IAT)
	var int dllnameoffset			!offset of name within impdir
	var int dllextraoffset			!offset of mysterious region just before the name
end

!const zsect=1
!const dsect=2
!const csect=3
!const isect=4

const zsect=3
const dsect=2
const csect=1
const isect=4

const filealign = 512
!const filealign = 32
const sectionalign = 4096
const imagebase = 0x40'0000
var int imagesize
var int filesize
var ref[]int64 thunktable				!point into code segment
var int fileiatoffset
var int fileiatsize
var ref strec stentrypoint				!symbol to be the entry point
var ref strec stentrypoint2
var ref strec stentrypoint3

const maxsection = 10
var [maxsection]sectionrec sectiontable
var int nsections

var ref byte importdir				!allowed section data for import directort in .idata

const maximports = 3000
var [maximports]importrec importtable
var int nimports

const maxlibs = 50
var [maxlibs]dllrec dlltable
var int ndlls

var ref byte datastart
var ref byte dataptr
var ichar userentrypoint

global proc writeexe(ichar outfile)=
var imagefileheader header
var optionalheader optheader
var int offset,i
var int64 aa

!CPL "WRITEEXE",NSEARCHLIBS
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

!println =filesize, =dataptr-datastart			!these should match

!if fverbose then
	CPL "Writing file:",outfile
!fi

if writefile(outfile,datastart,dataptr-datastart)=0 then
	println "Error writing exe file (possibly still running)"
	stop 1
fi
end

global proc genexe(ichar entrypoint)=
!manipulate the ss data to fill in all the details needed for exe format


searchlibs[1]:="msvcrt"
searchlibs[2]:="gdi32"
searchlibs[3]:="user32"
searchlibs[4]:="kernel32"
nsearchlibs:=4	

!CPL "GENEXE",=NSEARCHLIBS

userentrypoint:=entrypoint
loadlibs()
scanst()				!build dll/import tables
getoffsets()
relocdata(&sectiontable[csect])
relocdata(&sectiontable[dsect])

end

proc loadlibs=
!load library instances
var int i
var int64 hinst
var ichar file
var [300]char filename

for i to nsearchlibs do
!	hinst:=os_getdllinst(searchlibs[i])
	strcpy(&.filename,searchlibs[i])
	strcat(&.filename,".dll")
	hinst:=os_getdllinst(&.filename)
	if hinst=0 then
		cpl searchlibs[i]
		axerror("Can't load search lib")
	fi
	libinsttable[i]:=hinst
	libinstnames[i]:=pcm_copyheapstring(&.filename)
od
end

global function writessdata(int fexe)ref strbuffer=
gs_init(dest)
showssdata(fexe)

gs_line(dest)
return dest
end

global proc initsectiontable=
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
!sectiontable[zsect].rawsize:=roundtoblock(ss_zdatalen,filealign)
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

proc showssdata(int fexe)=
gs_strln(dest,(fexe|"EXE FORMAT"|"AFTER GENSS"))

showsections()
gs_line(dest)

showsectionrelocs2("Idata",ss_idatarelocs,ss_nidatarelocs)
showsectionrelocs2("Code",ss_coderelocs,ss_ncoderelocs)

gs_str(dest,"proc Section Zdata: ")
gs_strint(dest,ss_zdatalen)
gs_line(dest)

showsectiondata(&sectiontable[dsect])
showsectioncode(&sectiontable[csect])

if fexe then
	showsectiondata(&sectiontable[isect])
fi

showsymboltable2()
showimporttable()
gs_strln(dest,"END OF GENSS")

end

proc showsectiondata(ref sectionrec d)=
var int i,k,length,bb
var [128]char str,str2
var ref byte p

gs_str(dest,"proc Section ")
gs_str(dest,d^.name)
gs_str(dest," Size:")
gs_strint(dest,d^.virtsize)
gs_line(dest)
gs_line(dest)

k:=0
if d^.segtype<>impdata_seg then
	p:=bufferelemptr(d^.data,0)
else
	p:=d^.bytedata
fi
length:=d^.virtsize

str[1]:=0

var ref byte baseaddr:=cast(imagebase+d^.virtoffset)

!sprintf(&.str2,"%08X: ",baseaddr)
print @&.str2,baseaddr:"Z8H",,": "

gs_str(dest,&.str2)

for i:=1 to length do
	bb:=p++^
!	sprintf(&.str2,"%02X ",bb)
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
!		sprintf(&.str2,"%08X: ",baseaddr)
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

proc showsectioncode(ref sectionrec p)=
var ref byte codeptr,codeend,codestart
var int length,offset
var ichar s
var [16]char str

gs_strln(dest, "proc Section Code")

length:=p^.virtsize
codestart:=codeptr:=bufferelemptr(p^.data,0)

codeend:=codeptr+length

var ref byte baseaddr:=cast(imagebase+p^.virtoffset)

while codeptr<codeend do
	offset:=codeptr-codestart
S:=NIL
	s:=decodeinstr(codeptr,baseaddr+offset)
	exit when s=nil

!	sprintf(&.str,"%4d ",offset)
	print @&.str,offset:"4",," "
	gs_str(dest,&.str)

	gs_strln(dest,s)
od

gs_line(dest)
end

proc showsectionrelocs2(ichar caption,ref relocrec relocs, int nrelocs)=
var ref relocrec r
var ref strec d
var ichar name

!RETURN
gs_str(dest,"proc Section Relocs: ")
gs_str(dest,caption)
gs_str(dest," ")
gs_strint(dest,nrelocs)
gs_line(dest)

r:=relocs

while r do

	gs_str(dest,"Reloc: ")
	gs_str(dest,relocnames[r^.reloctype])
	gs_str(dest," Offset: ")
	gs_strint(dest,r^.offset)
	gs_str(dest," ST Index: ")
	gs_strint(dest,r^.stindex)
	gs_str(dest," ")

	d:=ss_symboltable^[r^.stindex]
	if d.name then
		gs_str(dest,d.name)
	else
		gs_str(dest,"`L")
		gs_strint(dest,d.labelno)
	fi
	gs_line(dest)

	r:=r^.nextreloc
od
gs_line(dest)

end

proc gs_value(ichar caption, int64 value)=
var [256]char str

strcpy(&.str,caption)
strcat(&.str,":")
ipadstr(&.str,20)
gs_str(dest,&.str)

!sprintf(&.str,"0x%llX %lld",value,value)
fprint @&.str,"0x# #",value:"H",value
gs_strln(dest,&.str)
end

proc showsymboltable2=
	var ichar name

gs_strln(dest,"Proc Symbol Table")
var int i
var ref strec d

for i:=1 to ss_nsymbols do
	gs_strint(dest,i)
	gs_str(dest,": ")

!CPL "NAME",	REF VOID(ss_symboltable^[i]^.name)
	d:=ss_symboltable^[i]
	if d.name then
gs_strint(dest,d.offset)
gs_str(dest," ")
		gs_strln(dest,d.name)
	else
		gs_str(dest,"`L")
		gs_strint(dest,d.labelno)
		gs_line(dest)
	fi

od
gs_line(dest)
end

proc showimporttable=
var [256]char str
var dllrec d
var importrec p


gs_strln(dest,"Proc Dll List")
var int i
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

function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
if n iand (align-1)=0 then return n fi

return n+(align-(n iand (align-1)))
end

proc showsections=
var sectionrec s
var int i

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

function extractlibname(ichar name, int &libno,moduleno)ichar=
!if name contains a dot, eg lib.abc, then set libno to index of "lib", and return "abc"
!otherwise return original name
var ref char s,name2
var [256]char str
var [256]char str2
var int i

name2:=nil

reenter::
s:=name
libno:=0

!CPL "EXTRACT:",NAME
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
!		return s+1
	fi

	++s
od

!do explicit search
var int n

!CPL "EXPLICIT SEARCH",NAME

for i:=1 to nsearchlibs do
	if os_getdllprocaddr(libinsttable[i],name) then
		n:=i
		exit				!don't need the actual address; just whether it exists
	fi
else
	println name,moduletable[moduleno].name
	axerror("Can't find external function")
od

!found in search lib n
if libno:=libnotable[n] then			!already added this library
	++dlltable[libno].nprocs
	return name
fi

!first use of this lib
strcpy(&.str,searchlibs[n])
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

var int i,libno
var ref strec d
var ichar name, libname

!CPL "SCANST"

for i:=1 to ss_nsymbols do
	d:=ss_symboltable^[i]
!CPL "SS SYMBOLS",I,D.NAME

!CPL =IMPORTED(D)

	if imported(d) then
!CPL "SCANST/IMPORTED:",D.NAME
!	case d^.symbol
!	when importedsym then
		if nimports>=maximports then axerror("genexe: Too many imports") fi
		++nimports

!CPL "SS1"
		name:=extractlibname((d.truename|d.truename|d.name),libno,d^.moduleno)
!CPL "SS2",NAME
		importtable[nimports].libno:=libno			!0 if no lib
		importtable[nimports].name:=name				!original, or 2nd part of lib.name
		importtable[nimports].def:=d

		d^.importindex:=nimports
	elsif exported(d) then
!CPL "SCANST/EXPORTED:",D.NAME
!	when exportedsym then

		if userentrypoint then
!CPL "USEREP"
			if eqstring(d^.name,userentrypoint) then
				stentrypoint:=d
			fi
		else
!CPL "CHECKING EP",D.NAME
			if eqstring(d^.name,"main") then
				stentrypoint:=d
			elsif eqstring(d^.name,"start") then
				stentrypoint2:=d
			fi
		fi
	fi
!CPL "END LOOP"
od
end

proc relocdata(ref sectionrec s)=
var ref sectionrec u
var ref relocrec r
var ref byte p
var ref word32 p32
var ref strec d
var int offset,index,thunkoffset,iatoffset

!CPL "SKIP RELOC"
!RETURN

p:=bufferelemptr(s^.data,0)
r:=s^.relocs

while r do
	d:=ss_symboltable^[r^.stindex]
	index:=d^.importindex				!into importtable
	thunkoffset:=importtable[index].thunkoffset

	case r^.reloctype
	when rel32_rel then
		if not imported(d) then
			axerror("rel32/not imported")
		fi
		(ref word32(p+r^.offset)^:=thunkoffset-r^.offset-4)
!
	when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
		if imported(d) then
			(ref word32(p+r^.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
		else
			u:=nil
			case d^.segment
			when zdata_seg then u:=&sectiontable[zsect]
			when idata_seg then u:=&sectiontable[dsect]
			when code_seg then u:=&sectiontable[csect]
			else
CPL D.NAME,D.SEGMENT
if d.namelen=0 then
!	CPL "LABELNO",D.LABELNO
	for i in sysfnnames when sysfnlabels[i]=d.labelno do
		fprintln "(Sysfn: #)", sysfnnames[i]
		exit
	od
fi

				AXERROR("RELOCDATA/SEG?")
			esac

!CPL =P,=R.OFFSET,=U,SEGMENTNAMES[D.SEGMENT]

			p32:=cast(p+r^.offset)
!CPL =P32,=U.VIRTOFFSET,=IMAGEBASE:"H"
			p32^:=p32^+u^.virtoffset+imagebase

		fi
	else
		cpl relocnames[r^.reloctype]
		axerror("Can't do this rel type")
	esac

	r:=r^.nextreloc
od

end

proc writerecordx(ref void r, int length)=
memcpy(dataptr,r,length)
dataptr+:=length
end

proc writedosstub=
!write 128-byte dos stub to dataptr
static var []byte stubdata = (
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
var imagefileheader header

memset(&header,0,header.bytes)

header.machine:=0x8664
header.nsections:=nsections
header.optheadersize:=optionalheader.bytes
header.characteristics:=0x22F

writerecordx(&header,header.bytes)
end

proc writeoptheader=
var optionalheader header

memset(&header,0,header.bytes)

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
		cpl userentrypoint
		axerror("User entry point not found")
	else
!		cpl entrypointname
		axerror("Entry point not found: main or start")
!		cpl("Entry point not found: main or start")
	fi
fi
header.entrypoint:=sectiontable[csect].virtoffset+stentrypoint^.offset

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

!header.importtable.size:=ndlls*importdirrec.bytes
header.importtable.size:=0X80

header.iat.virtualaddr:=fileiatoffset
header.iat.size:=fileiatsize

writerecordx(&header,header.bytes)

end

proc writesectionheader(ref sectionrec s)=
var imagesectionheader sheader

memset(&sheader,0,sheader.bytes)

strcpy(&sheader.name[1],s^.name)
sheader.virtual_size:=s^.virtsize
sheader.virtual_address:=s^.virtoffset
sheader.rawdata_offset:=s^.rawoffset
sheader.rawdata_size:=s^.rawsize

var int64 aa
case s^.segtype
when zdata_seg then
	aa:=0xC050'0080
	sheader.characteristics:=aa
!	sheader.characteristics:=0xC050'0080
when idata_seg then
	aa:=0xC050'0040
	sheader.characteristics:=aa
!	sheader.characteristics:=0xC050'0040
when code_seg then
	aa:=0x6050'0020
	sheader.characteristics:=aa
!	sheader.characteristics:=0x6050'0020
when impdata_seg then
	aa:=0xC030'0040
	sheader.characteristics:=aa
!	sheader.characteristics:=0xC030'0040
esac
writerecordx(&sheader,sheader.bytes)
end

proc writesectiondata(ref sectionrec s)=

case s^.segtype
when impdata_seg then
	writerecordx(s^.bytedata,s^.virtsize)		!rest of section will be zeros
	if s^.rawsize>s^.virtsize then
		dataptr+:=(s^.rawsize-s^.virtsize)
	fi

when zdata_seg then					!nothing goes to disk
!	dataptr+:=s^.rawsize
else
	writerecordx(bufferelemptr(s^.data,0),s^.rawsize)
esac
end

proc getoffsets=
!apply file/image offsets to sectiontable
var int fileoffset, imageoffset,i,diroffset,impdirno,hinttableoffset,j,n
var int codesize,length,thunkoffset,offset,dirstartoffset

fileoffset:=128+4+imagefileheader.bytes+optionalheader.bytes	!dosstub+sig
fileoffset+:=imagesectionheader.bytes*nsections

fileoffset:=roundtoblock(fileoffset,filealign)
imageoffset:=4096

!Need to increase size of code segment to incorporate the thunk table
var ref byte pcode
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
offset:=diroffset-dirstartoffset

!offset contains now the overall size of the import directory
!diroffset contains is the overall size of the image

!finish off last section data, and compute final file and image sizes
sectiontable[impdirno].virtsize:=offset
sectiontable[impdirno].rawsize:=roundtoblock(offset,filealign)
filesize:=roundtoblock(fileoffset+offset,filealign)

imagesize:=roundtoblock(imageoffset+(diroffset-dirstartoffset),sectionalign)

var ref byte pimpdir

pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

!prepare the thunk area in the code segment
var ref importdirrec pdir
var ref int64 paddr,pname
var int iatoffset
pdir:=cast(pimpdir)

!start fill in details within the import directory section
for i:=1 to ndlls do
	pdir^.implookuprva:=dlltable[i].nametableoffset
	pdir^.impaddressrva:=dlltable[i].addrtableoffset
	pdir^.namerva:=dlltable[i].dllnameoffset
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
var ref byte phint
var ref word32 pextra

for i to nimports do
	phint:=pimpdir+importtable[i].hintnameoffset-dirstartoffset
	phint+:=2					!leave hint as 0
	strcpy(cast(phint),importtable[i].name)
od
!same for lib names (no hint here, but re-use phint anyway)
var int xxx
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

!write the thunk table
var ref byte thunkptr,codebase
var int thunkaddr
thunkptr:=bufferelemptr(ss_code,thunkoffset)
codebase:=bufferelemptr(ss_code,0)

for i to nimports do
	importtable[i].thunkoffset:=thunkptr-codebase
	thunkptr++^:=0x48
	thunkptr++^:=0xFF
	thunkptr++^:=0x24
	thunkptr++^:=0x25
	thunkaddr:=imagebase+importtable[i].iatoffset
	(ref int32(thunkptr)^:=thunkaddr)

	thunkptr+:=4
od
end

function imported(ref strec d)int=
!CPL "IS IMPORTED?",D.NAME,=D.ISGLOBAL,NAMENAMES[D.NAMEID]
	if d.nameid=dllprocid then return 1 fi

!	AXERROR("IMPORTED()")
	return 0
end

function exported(ref strec d)int=
	if d.nameid<>dllprocid and d.isglobal then return 1 fi
!	AXERROR("EXPORTED()")
	return 0
end
=== ma_disasm.m 26/35 ===
import clib
import msys
import oslib

const showmregs=1
!const showmregs=0

const halt=0xF4

var int abc
var real xyz

var int res2
var int lx

var int nmodules
var int xfchsmask_pd

tabledata() [0:]ichar opnames =
	(add_op=0,	"add"),
	(or_op,		"or"),
	(adc_op,	"adc"),
	(sbb_op,	"sbb"),
	(and_op,	"and"),
	(sub_op,	"sub"),
	(xor_op,	"xor"),
	(cmp_op,	"cmp")
end

var [0:]ichar condnames = \
("o", "no", "b","ae","z","nz","be","a","s","ns","p","np",
 "l","ge","le","g")

tabledata() []ichar addrmodenames=		! rm modes
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

var int rex

var int addrmode						!amreg/ammem/amrel
var int rmreg							!0, or 1..16; adjusted middle value of modrm byte
var int rmopc							!0 to 7; middle value of modrm byte 
var int basereg							!0, or 1..16
var int indexreg						!0, or 1..16
var int scale							!1,2,4
var int opsize							!1,2,4,8
var int offset
var int offsetsize						!1 or 4
var int sizeoverride					!32=>16 switch
var int addroverride					!32=>16 switch
var int f2override						!xmm regs
var int f3override						!xmm regs

var [256]char deststr
var ichar destptr

!==============================================================

var ref byte codeptr

global function decodeinstr(ref byte &cptr,baseaddr=nil)ichar=
!decode next instruction at codeptr
!return 1 if decoded, with codeptr stepped to start of next instruction
!return 0 when end-of-code seen (nop or 0x90)
var int n,w
var int opc,reg,op,xxx,oldopsize,dispsize
var ref byte pstart
static var [256]char str
var [128]char str2
const maxinstrlen=14
var ichar s

deststr[1]:=0

pstart:=codeptr:=cptr

rex:=0
opsize:=1
f2override:=f3override:=sizeoverride:=addroverride:=0
basereg:=indexreg:=offset:=0

retry::						!back here after prefix byte seen

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
	genintd(readint32())

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
!	genstr(strreg(rmreg,(rex iand wmask|8|4)))
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
!!	fprintf(f,"------------------------------------------------- NOP: END OF CODE")
!	println "	end of code [nop]"
!!	os_getch()
!	return nil

when 0x91..0x97 then			!exch eax/reg
doexch::
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
!CPL =opsize
!stop
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
!	println "	end of code [halt]"
	return nil

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
doff::
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
endswitch

!at this point, deststr contains the decoded instruction
!need to put in address, bytes etc

if baseaddr then
!	sprintf(&.str,"%06X: ",baseaddr)
	print @&.str,baseaddr:"z6h",,": "
else
!	sprintf(&.str,"%06X: ",pstart)
	print @&.str,pstart:"z6h",,": "
fi

n:=codeptr-pstart
to n do
!	sprintf(&.str2,"%02X ",pstart++^)
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
var int opc,rhssize
var ichar opcstr

switch opc:=codeptr++^
when 0x2A then					!cvtsi2ss/sd XMM, REG/MEM
	decodeaddr(1)
	if f3override then
!		opsize:=8
		genstr("cvtsi2ss ")
	else
!		opsize:=4
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
doarith::
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
!	opsize:=(opc iand 1|2|1)
	printaddrmode()

when 0xD6 then
	decodeaddr(1)
	opsize:=8
	genstr("movq ")
	printaddrmode(1)
	genstr(",")
	genstr(strxmm(rmreg))	

else
	genstr("Unknown opcode 2-byte opcode: 0F ")
    genhex(opc)
endswitch
end

proc decodeaddr(int w=0)=
!codeptr points to modrm byte, with possible sib and/or disp following
!decode modrm, sib and disp
!store result in amode::
! basereg		0 when not used
! indexreg
! scale			1,2,4,8 factor for indexreg
! offset		0, or any offset or abs address
! addrmode		rm-code
!the function returns the xxx value (middle part of modrm byte)
var int modrm,xxx,mode,sib,rm

basereg:=indexreg:=0
scale:=1
offset:=0
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

!IF REX IAND WMASK THEN CPL "WMASK=1" FI
!IF REX IAND RMASK THEN CPL "RMASK=1" FI
!IF REX IAND XMASK THEN CPL "XMASK=1" FI
!IF REX IAND BMASK THEN CPL "BMASK=1" FI
!CPL "MODRM USED:",=MODE,=XXX,=RM

if mode=3 then		!plain register access
	basereg:=rm+1
	addrmode:=amreg

!CPL "DECADD1 REG ONLY",STRREG(BASEREG,8)

elsif rm<>4 then				!not esp; no sib
	if mode=0 and rm=5 then		![ebp] is actually [rip+disp]
		offset:=readint32()
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

	if mode=0 and basereg=rframe then	!no base register, only index; disp is 32bits
		basereg:=0
		offset:=readint32()

	else
		case mode
		when 1 then
			offset:=readsbyte()
		when 2 then
			offset:=readint32()
		esac
	fi

	if indexreg=rstack then				!stack means no index reg
		indexreg:=0
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
var word a
a:=ref word16(codeptr)^
codeptr+:=2
return a
end

function readint16:int=
var int a
a:=ref int16(codeptr)^
codeptr+:=2
return a
end

function readword32:word=
var word a
a:=ref word32(codeptr)^
codeptr+:=4
return a
END

function readint32:int=
var int a
a:=ref int32(codeptr)^
codeptr+:=4
return a
END

function readint64:int64=
var int64 a
a:=ref int64(codeptr)^
codeptr+:=8
return a
END

function getreg(int regcode,upper)int=
if upper then
	return regcode+8+1
fi
return regcode+1
end

function strreg(int reg,opsize)ichar=
static var []ichar regnames8=("al","cl","dl","bl","ah","ch","dh","bh",
						"r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b",
				"spl","bpl","sil","dil")

static var []ichar regnames16=("ax","cx","dx","bx","sp","bp","si","di",
						"r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w")

static var []ichar regnames32=("eax","ecx","edx","ebx","esp","ebp","esi","edi",
						"r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d")

static var []ichar regnames64=("rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
						"r8","r9","r10","r11","r12","r13","r14","r15")

![]ichar mregnames8=("B0","B10","B11","B1","Bsp","Bbp","B2","B3",
static var []ichar mregnames8=("B0","B10","B11","B1","B16","B18","B19","B17",
						"B12","B13","B4","B5","B6","B7","B8","B9",
					"B14","B15","B2","B3")

static var []ichar mregnames16=("W0","W10","W11","W1","Wsp","Wbp","W2","W3",
						"W12","W13","W4","W5","W6","W7","W8","W9")

static var []ichar mregnames32=("A0","A10","A11","A1","Astack","Aframe","A2","A3",
						"A12","A13","A4","A5","A6","A7","A8","A9")

static var []ichar mregnames64=("D0","D10","D11","D1","Dstack","Dframe","D2","D3",
						"D12","D13","D4","D5","D6","D7","D8","D9")

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
static var []ichar fregnames=("st0","st1","st2","st3","st4","st5","st6","st7")
return fregnames[freg]
end

proc printaddrmode(int xmm=0)=
static var [100]char str
var ichar plus
var int addrsize

genstr(" ")

case addrmode
when amreg then
	if xmm then
		genstr(strxmm(basereg))
	else
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
if addrmode=amrel then genstr("+RIP") fi
end

proc genstr(ichar s)=
strcat(&.deststr,s)
end

proc genintd(int64 a)=
!var [32]char str
!sprintf(&.str,"%lld",a)
genstr(strint(a))
end

proc genhex(int64 a)=
!var [32]char str
!sprintf(&.str,"%llX",a)
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
static var [32]char str

!sprintf(&.str,"xmm%d",reg-1)
print @&.str,"xmm",,reg-1
return &.str
end

function strmmx(int reg)ichar=
static var [32]char str

!sprintf(&.str,"mmx%d",reg-1)
print @&.str,"mmx",,reg-1
return &.str
end

proc decode8087(int ttt)=
var byte bb
var int longopc,freg,shortopc,code

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
var int d, p

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
if opsize=1 and rex and reg>=5 and reg<=8 then
	reg+:=12				!5..8 => 17..20
fi
end

proc getsilx(int &reg)=
!as getsil but used for basereg, which must have addrmode=amreg

!for certain byte-reg combinations, convert regs ah,ch,dh,bh to spl,bpl,sil,dil
if addrmode=amreg and opsize=1 and rex and reg>=5 and reg<=8 then
	reg+:=12				!5..8 => 17..20
fi
end
=== mm_parse.m 27/35 ===
import msys
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lex
import mm_lib
import mm_start
import mm_diags
import mm_mcldecls

var int intabledata=0		!1 means reading table data line; $ gives tabledataname
var int inreadprint=0
var int inparamlist=0
var int inrecordbody=0
var int inimportmodule=0
var int labelseen=0
var ichar tabledataname=nil

const maxprocstack=10
var [maxprocstack]ref strec procstack
var int nprocstack=0

var uflagsrec unionstring, unionpend
var ref strec unionlastvar=nil
var ref strec dretvar			!part of read-proc: nil, or symptr of retval variable

var int try_level=0
var int varattribs=0

const maxdollarstack=10
var [maxdollarstack]unit dollarstack		!used for a[$]
var int ndollar=0
var int inmultexpr=0
var int insiderecord=0
var int insidedllimport=0

global function parsemodule(int n)int=
var modulerec m
var ref strec p, owner
var int globalflag,status

initparser()

m:=moduletable[n]
currmoduleno:=n

stmodule:=moduletable[n].stmodule
currproc:=stmodule

startlex("PARSEMODULE",m.fileno)

owner:=stmodule
lex()

!VAR INT NTOKENS:=0
!REPEAT
!	LEX()
!!CPL SYMBOLNAMES[LX.SYMBOL]
!++NTOKENS
!UNTIL LX.SYMBOL=EOFSYM
!CPL =NTOKENS
!CPL =NLOOKUPS
!CPL =NCLASHES
!CPL =NCLASHES/REAL (NLOOKUPS)
!STOP
!RETURN 1

!CPL "//PARSING MODULE",N, SOURCEFILENAMES[N]
!
!VAR INT NTOKENS:=0
!REPEAT
!	LEXREADTOKEN()
!PRINTSYMBOL(&NEXTLX)
!!CPL SYMBOLNAMES[NEXTLX.SYMBOL]
!++NTOKENS
!UNTIL NEXTLX.SYMBOL=EOFSYM
!CPL =NTOKENS
!STOP
!RETURN 1
!

status:=readmoduledefs(owner)
if not status then
	return 0
fi

return status
end

global function readmoduledefs(ref strec owner)int=
!first symbol has been read
var ref strec p,dimport,stimport
var int globalflag,i,callbackflag
var ichar name

globalflag:=0
callbackflag:=0

do
	switch lx.symbol
	when kglobalsym then
		if globalflag then serror("global global?") fi
		globalflag:=lx.subcode			!1 or 2
		lex()

	when kprocsym,kfunctionsym then	!todo
		readprocdef(owner,globalflag,callbackflag)
		callbackflag:=0
		globalflag:=0

	when stdtypesym,namesym,lsqsym,krefsym,kicharsym,ktypeofsym then
		serror("Need Var or Let")

	when kvarsym,kvariantsym then
		readvardef(owner,globalflag,0,staticid)
		globalflag:=0

	when kletsym then
		readvardef(owner,globalflag,0,staticid)
		globalflag:=0

	when kimportmodulesym then
		readimportmodule(owner)

	when kimportpathsym then
		lex()
		checksymbol(stringconstsym)
		lex()

	when kmapmodulesym then
		repeat
			lex()
		until lx.symbol in [semisym,eofsym]

	when ktypesym then
		readtypedef(owner,globalflag)
		globalflag:=0

	when kconstsym then
		readconstdef(owner,globalflag)
		globalflag:=0

	when kclasssym,krecordsym then
		readclassdef(owner,globalflag)
		globalflag:=0

	when kenumsym then
		lex()
		readenumtype(owner,0)

	when ktabledatasym then
		readtabledef(globalflag)
		globalflag:=0

	when docstringsym then
!		docstrings append:=lxvalue
!		lex()
		serror("DOCSTRING")

	when kimportsym then
		if globalflag then serror("glob/import?") fi
		lex()
		if lx.symbol=opsym and lx.subcode=j_mul then
			lex()
		fi
		checksymbol(namesym)

!need to check that the import has been done (in case import stmt is badly placed)
!(note: can't detect a badly placed import if that lib has been loaded elsewhere)
		dimport:=lx.symptr
		name:=mapimport(dimport^.name)
		for i:=1 to nmodules do
			if eqstring(name, moduletable[i].name) then
				stimport:=moduletable[i].stmodule
				exit
			fi
		else
			CPL lx.symptr^.name
			serror("Import stmt out of position?")
		od

		lex()
		domappedalias(dimport,stimport)
		if lx.symbol=namesym and eqstring(lx.symptr^.name,"as") then
			readimportalias(dimport)
		fi

	when semisym then
		lex()

	when eofsym then
		exit

	when kfflangsym then
		if lx.subcode=callbackff then
			callbackflag:=callbackff
			lex()
		else
			goto error
		fi

	when kmacrosym then
		readmacrodef(owner,globalflag)
		globalflag:=0

	when kexpandsym then
		serror("MODULE/EXPAND")

	when koperatorsym then
		serror("MODULE/OPERATOR")

	when dotsym then
		SERROR("MODULE/DOT")
	else
error::
		PS1("symbol")
		serror("Not allowed at module level")
	endswitch
od

return 1
end

proc initparser=

unless nullunit then
	nullunit:=createunit0(j_null)
end unless

try_level:=0
currproc:=nil
varattribs:=0

intabledata:=0		!1 means reading table data line; $ gives tabledataname
inreadprint:=0
inparamlist:=0
inrecordbody:=0
inimportmodule:=0
var ichar tabledataname:=""
labelseen:=0

ndollar:=0
end

proc skipsemi=
while lx.symbol=semisym do lex() od
end

function makeblock(unit p)unit=
return createunit1(j_block,p)
end

function makestmtblock(unit p)unit=
return createunit1(j_stmtblock,p)
end

proc checkequals=			!CHECKEQUALS
!check that "=" is current symbol
if not (lx.symbol=opsym and lx.subcode=j_eq) then
	serror("""="" expected")
fi
end

function getcurrline:int=
return lx.lineno
end

function checkbegin(int fbrack)int=				!CHECKBEGIN
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
var int closesym

skipsemi()

if lx.symbol=lbracksym and fbrack then
	closesym:=rbracksym
	lex()
elsif lx.symbol=lcurlysym then
	closesym:=rcurlysym
	lex()
else
	closesym:=kendsym
fi
return closesym
end

proc checkbeginend(int closesym,kwd,startline=0)=		!CHECKBEGINEND
!look for ) or ] or end [kwd] depending on closesym
!positioned at this symbol; exit at following symbol
skipsemi()
if closesym=rbracksym or closesym=rcurlysym then
	checksymbol(closesym)
else
	checkend(closesym,kwd,startline:startline)
fi
lex()
end

proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=		!CHECKEND
!at terminator symbol such as ), eof or 'end'
!check it matches what is expected
!endsym is symbol expected to match
!'end' can have optional keyword following; if present, it must match endkeyword
!Some loop ends (indicated by endkeyword=kforsym, etc) can be also be terminated with 'od'
!endsym should be lbracksym or kendsym
var [100]char str

!exit pointing to current symbol (to 'end', keyword after 'end', or ')')
if endsym=lx.symbol=rbracksym then
	return
fi

if lx.symbol<>kendsym then
	strcpy(&.str,"Bad 'end' ")
error::

	if startline then
		fprint @(&.str+strlen(&.str))," (from line #)",startline
	fi
	serror(&.str)
fi

!'end', seen, but check that lx.subcode, if non-zero, is endkeywords or is in that set
if lx.subcode=0 then					!plain end; for now, that always matches
!	serror("'end' by itself no longer valid")
	return
fi

unless (endkwd1 and endkwd1=lx.subcode) or (endkwd2 and endkwd2=lx.subcode) then
	strcpy(&.str,"Mismatched 'end'")
	goto error
end unless
end

function readvardef(ref strec owner,int isglobal=0,isstatic=0,varid=staticid)unit=
!positioned at 'var' 'let' or 'variant'
!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]
var unit ulist,ulistx, p
var int nvars,m,k
var ref strec stname

k:=lx.symbol
lex()
ulist:=ulistx:=nil

if k<>kvariantsym then
	if istypestarter() then
		m:=readtypespec(owner)
	else
		m:=tauto
	fi
else
	m:=tvariant
fi

nvars:=0
while lx.symbol=namesym do

	++nvars
	stname:=getduplnameptr(owner,lx.symptr,varid)

	storemode(1,owner,m,&stname^.mode)
	stname^.isglobal:=isglobal
	stname^.isstatic:=isstatic
	stname^.islet:=(k=kletsym)

	adddef(owner,stname)
	if varid=staticid then
		addstatic(stname)
	fi

	lex()

	if lx.symbol=assignsym or lx.symbol=opsym and lx.subcode=j_eq then
		if lx.symbol=assignsym then
			if varid=staticid then
				serror("Need = on static not :=")
			fi
		else
			if varid=frameid then
				serror("Need 'static' for '='")
				addstatic(stname)
			fi
		fi
		lex()
		stname^.code:=readunit()
		stname^.equals:=1
		if varid=frameid then
			p:=createunit2(j_assignx,createname(stname),stname^.code)
			addlistunit(&ulist,&ulistx,p)
		fi

	elsif lx.symbol=atsym then
		if k=kletsym then serror("let@") fi
		lex()
		stname^.at:=1
		stname^.equivvar:=readunit()
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

proc readconstdef(ref strec owner,int isglobal=0)=
!at 'const' symbol
var int nconsts,deft,m
var ref strec stname

!PS("RCD0")
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
	stname^.code:=readconstexpr(1)

!	if deft<>tauto then
		m:=deft
!	else
!		m:=stname^.code^.mode
!	fi

	storemode(2,owner,m,&stname^.mode)
	++nconsts

	stname^.isglobal:=isglobal

	adddef(owner,stname)

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
!read one of the following::
! (x)		simple expression
! ()		list with no elements
! (x,)		list with one element
! (x,x,...)		list
! (x|x|x])		if then else fi
! (x|x,... |x])	select then else end

! (s||s|s)	!list comp [SYNTAX TO BE REVISED]
!return positioned at symbol following closing ")"
var unit ulist,ulistx, p,q,r
var int oldirp,length

lex()					!first symbol of first expression
ulist:=ulistx:=nil
length:=0

if lx.symbol=opsym and nextlx.symbol=rbracksym then	!operator constant
	p:=createunit0(j_operator)
	p^.opcode:=lx.subcode
	lex()
	lex()
	return p
elsif lx.symbol=opsym and nextlx.symbol=assignsym then	!operator:= constant
	p:=createunit0(j_operator)
	p^.opcode:=getoptocode(lx.subcode)
	lex()			!read :=
	lex()			!read )
	checksymbol(rbracksym)
	lex()
	return p
fi

!check symbol after "("
case lx.symbol
when rbracksym then			!empty list
	lex()
	p:=createunit0(j_makelist)
	p^.length:=0
	return p
else					!assume normal expression follows
	p:=readxunit()
esac

!check symbol after "(expr"
case lx.symbol
when rbracksym then			!simple (x) expression
	lex()

	return p

when commasym then
	length:=1
	if nextlx.symbol=rbracksym then		!means one-element list
		lex()
		lex()
		p:=createunit1(j_makelist,p)
		p^.length:=length
		return p
	fi

!must be regular list
	ulist:=ulistx:=p
	repeat
		lex()							!skip comma
		if lx.symbol=rbracksym then		!allow ,) to end list
			exit
		fi
		if lx.symbol=commasym then
			serror(",, null expr not allowed")
		fi
		addlistunit(&ulist,&ulistx,readxunit())
		++length
		skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
	until lx.symbol<>commasym
	checksymbol(rbracksym)
	lex()
	p:=createunit1(j_makelist,ulist)
	p^.length:=length
	return p

when barsym then			!ifx/selectx expression; p is selector expression
	lex()
	q:=readxunit()
	case lx.symbol
	when barsym then		!(a|b|c)
		lex()
		r:=readsunit()
		checksymbol(rbracksym)
		lex()
		return createunit3(j_if,p,q,r)
	when rbracksym then
		lex()
		return createunit3(j_if,p,q,nil)

	esac

!assume selectx expression
	addlistunit(&ulist,&ulistx,q)	!start with one-element list
	checksymbol(commasym)
	if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
		repeat
			lex()				!skip comma
			addlistunit(&ulist,&ulistx,readxunit())
		until lx.symbol<>commasym
		checksymbol(barsym)
	else
		lex()					!skip |
	fi
	lex()
	r:=readxunit()
	checksymbol(rbracksym)
	lex()
	return createunit3(j_select,p,ulist,r)

else
	serror("(x ...")
esac
return nil
end

proc addlistparam(ref ref strec ulist,ulistx,ref strec p)=
!add unit p to unit structure ulist,^ulistx  which can be null
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextparam:=p
fi
ulistx^:=p			!update end-of-list pointer
end

function readcast(int t=tvoid)unit=			!READCAST
!also reads standalone type value
!t<>tvoid means already has ty[e
var unit p
var int opc,haslb

if t=tvoid then
	t:=readtypespec(currproc)
fi

case lx.symbol
when atsym then
	opc:=j_typepun
	lex()
when dotsym then			!allow T.type, but also just T (followed by . which
							!might be T.minvalue etc)
	if nextlx.symbol=ktypesym then
		lex()
		p:=createunit0(j_typeconst)
		storemode(4,currproc,t,cast(&p^.value))
		lex()
	else					!leave dot to be processed by caller
		p:=createunit0(j_typeconst)
		storemode(4,currproc,t,cast(&p^.value))
	fi
	return p
else
	opc:=j_convert
esac

if lx.symbol=lbracksym then
	lex()
	p:=readunit()
	checksymbol(rbracksym)
	lex()
else	
SERROR("INT A,B,C: MAY NEED VAR OR LET")
	p:=readterm2()
fi

p:=createunit1(opc,p)
storemode(5,currproc,t,&p^.newmode)
return p
end

function readopc:unit=			!READOPC
!op sym seen just before a term
var unit p,q
var int opc,opc2

opc:=lx.subcode
lex()
case opc
when j_add then			!ignore +
	return readterm2()
when j_sub then			!convert minus to negate
	opc:=j_neg
when j_min,j_max,j_atan2,j_fmod,j_concat,j_append,j_convuc, j_convlc,
	j_left,j_right,j_take,j_drop then	!allow some binary ops to have function format

	checksymbol(lbracksym)
	lex()
	p:=readunit()
	case lx.symbol
	when commasym then
		lex()
		q:=readunit()
		checksymbol(rbracksym)
		lex()
		return createunit2(opc,p,q)
	when rbracksym then
		if opc in [j_left,j_right,j_convuc,j_convlc] then
			lex()
			return createunit1(opc,p)
		fi
	esac
	serror("readopc: missing 2nd opnd")
else
	if binopset[opc] then
		serror("Can't be used as unary op")
	fi

esac

if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
	lex()
	opc:=getoptocode(opc)

!	serror("op:= not allowed")
fi

p:=createunit1(opc,readterm2())
!*!evalmonop(p)
return p
end

function readsprint:unit=			!READSPRINT
var int oldinreadprint,opc,isfprint
var unit pformat, pdev, printlist, printlistx, p

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lex()
checksymbol(lbracksym)
lex()

case opc
when j_sfprint,j_cprint then
	isfprint:=1
else
	isfprint:=0
esac

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

if lx.symbol=rbracksym then
	goto finish
fi

do
	if lx.symbol=commasym then		!assume extra comma, meaning nogap
		addlistunit(&printlist,&printlistx,createunit0(j_nogap))
	else
		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(j_fmtitem,p,readunit())
		fi
		addlistunit(&printlist,&printlistx,p)
	fi
	if lx.symbol<>commasym then exit fi
	lex()
od

checksymbol(rbracksym)

finish::
lex()
inreadprint:=oldinreadprint
if (opc=j_print or opc=j_fprint) and printlist=nil then
	serror("No print items")
fi

if isfprint then
	if pformat^.tag=j_null then
		serror("No fmt str")
	fi
	return createunit3(opc,pdev,pformat,printlist)
else
	return createunit2(opc,pdev,printlist)
fi
end

function readsread:unit=		!READSREAD
!Need to check what sread/sreadln actually mean. I think they are actually supposed
!to work an item at a time::
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
var int oldinreadprint,opc
var unit pformat,pdev,p, readlist,readlistx

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lex()
checksymbol(lbracksym)
lex()

readlist:=readlistx:=nil
pformat:=pdev:=nil

if lx.symbol=atsym then
	if opc=j_read then
		serror("@ on read")
	fi
	lex()
	pdev:=readunit()
	if lx.symbol=commasym then lex() else goto finish fi
fi

if lx.symbol=rbracksym then
	goto finish
fi

do
	p:=readunit()
	if lx.symbol=colonsym then
		lex()
		p:=createunit2(j_fmtitem,p,readunit())
	fi
	addlistunit(&readlist,&readlistx,p)
	if lx.symbol<>commasym then exit fi
	lex()
od

checksymbol(rbracksym)

finish::
lex()
inreadprint:=oldinreadprint
if opc=j_read and readlist=nil then
	serror("No read items")
fi

return createunit2(opc,pdev,readlist)
end

function readcompilervar:unit=		!READCOMPILERVAR
var [100]char str
var rsystemtime tm
static var []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
var unit p

case lx.subcode
when j_cvlineno then

	p:=createunit0(j_cvlineno)
	lex()
	return p

when j_cvstrlineno then
	getstrint(lx.lineno iand 16777215,&.str)

when j_cvmodulename then
	p:=createunit0(j_cvmodulename)
	lex()
	return p
!	strcpy(&.str,moduletable[currmoduleno].name)

when j_cvfilename then
	p:=createunit0(j_cvfilename)
	lex()
	return p

!	strcpy(&.str,sourcefilenames[moduletable[currmoduleno].fileno])

when j_cvfunction then
	strcpy(&.str,currproc^.name)

when j_cvdate then
	os_getsystime(&tm)
	fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

when j_cvtime then
	os_getsystime(&tm)
	fprint @&.str,"#:#:#",tm.hour:"z2",tm.minute:"z2",tm.second:"z2"

!when j_cvversion then x:=compilerversion
when j_cvtargetbits then
	lex()
	return createconstunit(targetbits,tint)
when j_cvtargetsize then
	lex()
	return createconstunit(targetsize,tint)
when j_cvtargetcode then
	strcpy(&.str,targetnames[target])

else
	serror_s("compiler var not impl: #",jtagnames[lx.subcode])
esac
lex()

return createstringconstunit(pcm_copyheapstring(&.str),-1)
end

function readcastx:unit=			!READCASTX
!explicit cast using syntax::
! cast(expr,type)
! cast@(expr,type)
!at 'cast'
lex()
return createunit1(j_autocast,readterm2())
end

global proc checksymbol(int symbol)=
var [100]char str

if lx.symbol<>symbol then
!	sprintf(&.str,"%s expected, not %s",symbolnames[symbol],symbolnames[lx.symbol])
	fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]
	serror(&.str)
fi
end

function readtypespec(ref strec owner,int typedefx=0)int=			!READTYPESPEC
!at initial symbol of a type, or where type is expected
!read simple type (which will have a single name) or a more elaborate type-spec
!returns a moderec handle
!typedefx is not a def, but either::
! moderec	Used when called from readtypedef. This is then filled in with the
!		details of the new mode, and the new version is returned
! nil		Called from outside readtypedef; then just returns a new moderec

!If the first symbol is not a stdtype, then it is assumed to be a usertype
!For stdtypes, I might implement :N and *N width-specifiers, as an alternative to just
!using int16 etc
var ref strec d,e
var int t,kwd,fflang,sltype,w
var unit x,pupper,plx
var unit dim,length
const maxdim=30
var [maxdim]unit dims
var int ndims,i,n,k

case lx.symbol
when lsqsym then		!array bounds
arraybounds::
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
					dim:=createunit2(j_keyvalue,dim,length)
				else													!lower::
					dim:=createunit1(j_keyvalue,dim)
				fi
			esac
		fi
		if ndims>=maxdim then serror("Too many array dims") fi
		dims[++ndims]:=dim
		exit when lx.symbol<>commasym
		lex()
	od
	inreadprint:=0
	checksymbol(rsqsym)
	lex()
	t:=readtypespec(owner)
	for i:=ndims downto 1 do
		t:=createarraymode(owner,t,dims[i],(i=1|typedefx|0))
	od
	return t

when stdtypesym then
	t:=lx.subcode
	lex()

	case t
	when tflexstring then
		if lx.symbol=opsym and lx.subcode=j_mul then
			lex()
			length:=readterm2()
			t:=createarraymode(owner,tc8,length,typedefx)
		fi
	when tfixedset then
		if lx.symbol=lsqsym then
			lex()
			if lx.symbol=opsym and lx.subcode=j_mul then
				t:=tflexset
				lex()
			else
				length:=readunit()
				t:=createsetmode(owner,length,typedefx)
			fi
			checksymbol(rsqsym)
			lex()
		else
			t:=tflexset
		fi
	when tflexdict then
		checksymbol(lsqsym)
		lex()
		k:=readtypespec(owner)
		checksymbol(rsqsym)
		lex()
		t:=readtypespec(owner)
		t:=createdictmode(owner,t,k,typedefx)
	esac

when namesym then
	d:=lx.symptr
	lex()
	if lx.symbol=dotsym then
		lex()
		checksymbol(namesym)
		t:=newusertypex(d,lx.symptr)
		lex()
	else
		t:=newusertypex(d)
	fi

when kenumsym then		!enum
	lex()
	t:=readenumtype(owner,typedefx)

when lbracksym then
	t:=readenumtype(owner,typedefx)

when krecordsym,kstructsym then
	serror("Use 'record name =' syntax")

when kunionsym then
	serror("Top-level union not allowed")

when krefsym then		!ref T
	fflang:=0
retry::
	lex()
	case lx.symbol
	when kprocsym,kfunctionsym then	!function pointer being created
		t:=readrefproc(owner,typedefx,fflang)
	when kfflangsym then
		fflang:=lx.subcode
		goto retry
	elsif lx.symbol=stdtypesym then
		case lx.subcode
		when tlabel then
			t:=treflabel
		when tu1,tu2,tu4 then
			t:=createrefbitmode(owner,lx.subcode,typedefx)
		when tc8 then
			t:=trefchar
		else
			goto readtarget
		esac

		lex()
	else						!assume normal type
readtarget::
		t:=readtypespec(owner)
		t:=createrefmode(owner,t,typedefx)
	esac

when kicharsym then
	lex()
	t:=trefchar

when ktypeofsym then
	lex()
	checksymbol(lbracksym)
	lex()
	checksymbol(namesym)

SERROR("TYPEOF/NO 'NAME' MODULE")
	if d then
		t:=d^.mode
	else
		serror("Typeof?")
	fi
	lex()
	checksymbol(rbracksym)
	lex()

when ksubrangesym then
	lex()
	x:=readunit()
	if x^.tag<>j_makerange then
		serror("range expected")
	fi

	t:=createsubrangemode(owner,x,typedefx)

when kslicesym then
	t:=readslicetype(owner,typedefx)

when kflexsym then
	t:=readflexarray(owner,typedefx)

when kvariantsym then
	lex()
	t:=tvariant

else
	serror("Bad type starter")
esac
return t
end

function readslicetype(ref strec owner, int typedefx)int=
!positioned at 'slice'
!dim is nil, or lower-bound expression
	var unit plower
	var int t

	lex()
	checksymbol(lsqsym)
	lex()
	if lx.symbol<>rsqsym then
		inreadprint:=1
		plower:=readunit()
		inreadprint:=0
		checksymbol(colonsym)
		lex()
		checksymbol(rsqsym)
	else
		plower:=nil
	fi
	lex()
	t:=readtypespec(owner,typedefx)
	return createslicemode(owner,t,plower,typedefx)
end

function readflexarray(ref strec owner, int typedefx)int=
!positioned at 'flex'
!dim is nil, or lower-bound expression
	var int t

	lex()
	checksymbol(lsqsym)
	lex()
	checksymbol(rsqsym)
	lex()
	t:=readtypespec(owner,typedefx)
	return createflexarraymode(owner,t,typedefx)
end

function readslist(int iscall=0,donulls)unit=		!READSLIST
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (j_ust comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a function-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a,b,c	)
!eg: (a		!
var unit ulist,ulistx
var int oldinparamlist

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
			addlistunit(&ulist,&ulistx,createunit0(j_null))
		else
			serror("null comma expr not allowed")
		fi
		lex()
	when rbracksym then
		if donulls then
			addlistunit(&ulist,&ulistx,nullunit)
		fi
		exit
	else
		addlistunit(&ulist,&ulistx,readunit())
		if lx.symbol=commasym then
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

function readindex(unit p,int dot)unit=		!READINDEX
!at '['; dot=0/1 for a[]/a.[]
!syntax is::
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
var unit q,plower,pupper

lex()

if not dot then
	case lx.symbol
	when rsqsym then
fullslice::
		lex()
		plower:=createunit1(j_lwb,duplunit(p))
		pupper:=createunit1(j_upb,duplunit(p))
		p:=createunit2(j_slice, p, createunit2(j_makerange,plower, pupper))
		return p
	when rangesym,colonsym then
		lex()
		checksymbol(rsqsym)
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

	if q^.tag=j_makerange then		!convert into a discrete slice
		p:=createunit2((dot|j_dotslice|j_slice),p,q)
	else
		p:=createunit2((dot|j_dotindex|j_index),p,q)
	fi
	exit when lx.symbol<>commasym
	lex()
od
checksymbol(rsqsym)
lex()
return p
end

function readdotsuffix(unit p)unit=		!READDOTSUFFIX
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
var unit q
var int t

while lx.symbol=dotsym do
	lex()
	switch lx.symbol
	when lsqsym then
		p:=readindex(p,1)
	when namesym then
		p:=createunit2(j_dot,p,createname(lx.symptr))
		lex()
	when opsym,opsym2 then			!ought to check whether op is allowed in this form
		p:=createunit1(lx.subcode,p)
		lex()
	when bitfieldsym then
		p:=createunit1(j_bitfield,p)
		p^.opcode:=lx.subcode
		lex()
	when lbracksym then			!use for variable attributes
		lex()
		p:=createunit2(j_dotattr,p,readunit())
		checksymbol(rbracksym)
		lex()
	when ktypesym then			!.type, convert to .gettype
		case p^.tag
		when j_typeconst then			!int.type=>int

		else
			p:=createunit1(j_typeof,p)
		esac
		lex()

	else
		serror("Unknown dot suffix")
	endswitch
od
return p
end

global function isconstexpr(unit p)int=		!ISCONSTEXPR
return p^.tag=j_const
end

function readkeyindex(unit p)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is::
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
var unit q,plower,pupper

lex()

p:=readunit()
q:=nil
if lx.symbol=commasym then
	lex()
	q:=readunit()
fi

p:=createunit2(j_keyindex,p,q)

checksymbol(rcurlysym)
lex()
return p
end

function readconstexpr(int needconst=1)unit=
return readunit()
end

function readconstint:int=		!READCONSTINT
!read expression that must yield a constant int value *now*; return value
var int64 x

!keep it simple for now
if lx.symbol=intconstsym then
	x:=lx.value
	lex()
	return x
elsif lx.symbol=opsym and lx.subcode=j_sub then
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

proc readprocdef(ref strec procowner,int isglobal,fflang=0)=
!at 'proc' etc symbol; read proc def or declaration
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
var int kwd,startline,closesym
var ref strec stproc,q,stname

kwd:=lx.symbol
stproc:=readprocdecl(procowner,isglobal,fflang)

checkequals()
lex()

startline:=getcurrline()

closesym:=checkbegin(0)

pushproc(stproc)
nextavindex:=0

IF DRETVAR THEN
	stname:=getduplnameptr(stproc,dretvar,frameid)
	storemode(1,stproc,stproc^.mode,&stname^.mode)
	adddef(stproc,stname)
fi

addtoproclist(stproc)
stproc^.code:=readsunit()
checkbeginend(closesym,kwd,startline)

stproc^.equals:=1

popproc()
end

global function readprocdecl(ref strec procowner,int isglobal,fflang)ref strec=
!at 'proc'  or 'function' 
!read proc declaration only, so exit at "=" or ";" symbol
!syntax::
!proc name: :/=> T [def]
!proc name(params) [def]
!proc name(params) [=>]T [def]
!return st entry of proc, and positioned at '=' or semi

var int kwd,varparams,try_level, prettype, nparams, nretvalues
var [4]int retmodes
var ichar metadata, truename
var ref strec pequiv, stproc, owner, paramlist,nameptr

kwd:=lx.symbol				!remember keyword
pequiv:=nil
metadata:=""
truename:=nil
varparams:=0
try_level:=0

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
if insidedllimport then isglobal:=1 fi

if truename then
	stproc^.truename:=truename
fi

if stproc^.name^='$' and eqstring(stproc^.name,"$init") then
	moduletable[stmodule^.moduleno].stinitproc:=stproc
fi

adddef(procowner,stproc)
if stproc^.nameid=dllprocid then
	stproc^.imported:=1
	if eqstring(procowner^.name,"cstd") then
		stproc^.imported:=2
	fi
fi

owner:=stproc
pushproc(stproc)

lex()

paramlist:=nil
prettype:=tvoid
nparams:=0
nretvalues:=0

if lx.symbol=opsym and lx.subcode=j_lt then			!look for metadata
	if stproc^.nameid=dllprocid then
		serror("Metadata on dllproc")
	fi
	lex()
	checksymbol(stringconstsym)
	stproc^.metadata:=lx.svalue

	lex()
	unless lx.symbol=opsym and (lx.subcode=j_gt or lx.subcode=j_ge) then
		serror(""">"" expected")
	end
	if lx.symbol=opsym and lx.subcode=j_ge then	!>= must become just =
		lx.subcode:=j_eq
	else
		lex()
	fi
fi

nretvalues:=0
if lx.symbol=lbracksym then		!possible params
	lex()
	if lx.symbol<>rbracksym then
		paramlist:=readparams(procowner,stproc,fflang,varparams,nparams)
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

if nretvalues then
	prettype:=retmodes[1]
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
endunless

if nretvalues and (kwd<>kfunctionsym) then		!proc: result given
	serror("Proc can't return value")
fi

stproc^.paramlist:=paramlist
stproc^.nretvalues:=nretvalues

for i to nretvalues do
	storemode(6,procowner,retmodes[i],&stproc.modelist[i])
od

if lx.symbol=atsym then			!equivalence
	lex()
	checksymbol(namesym)
	lex()
	stproc^.at:=1
fi

stproc^.code:=nil

case fflang
when clangff,windowsff,qlangff then
!	if procowner^.nameid<>dllmoduleid then
!		cpl stproc^.name,fflangnames[fflang]
!		serror("FF should be in dll import")
!	fi
else			!assume this language
	case procowner^.nameid
	when moduleid then
	when dllmoduleid then
		serror("Need FF specifier")
	esac
esac
stproc^.isglobal:=isglobal
stproc^.varparams:=varparams
stproc^.fflang:=fflang

if procowner=stmodule and \
	(stproc^.namelen=5 and eqstring(stproc^.name,"start")) or \
	(stproc^.namelen=4 and eqstring(stproc^.name,"main")) then
	stproc^.isglobal:=1
fi

popproc()

return stproc
end

function readparams(ref strec procowner,owner,int fflang,&varparams,&nparams)ref strec=			!READPARAMS
!positioned at first symbol after '('
!read list of params, return that list
!syntax is a list of names and/or types
!each param can optionally be followed by a default value
!finish pointing at ")"
	var ref strec stlist, stlistx, stname, d
	var int parammode, pmode, m

	var [30]char str
	stlist:=stlistx:=nil
	pmode:=tvariant
	nparams:=0

	if fflang=0 then fflang:=qlangff fi

	if lx.symbol in [koutsym,addrsym] or (lx.symbol=opsym and lx.subcode=j_in) then
		pmode:=tvariant
	elsif lx.symbol=namesym and nextlx.symbol in [commasym,rbracksym] then	!name only
		if fflang<>qlangff then				!assume type
			pmode:=readtypespec(procowner)
			return readparams_types(procowner,owner,fflang,varparams,nparams,pmode)
		else
			pmode:=tvariant
		fi
	else
		pmode:=readtypespec(procowner)
		if lx.symbol in [commasym,rbracksym] then
			return readparams_types(procowner,owner,fflang,varparams,nparams,pmode)
		fi
	fi
	goto gotmode

	do										!expect type of name at start of loop
		if istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
		fi
gotmode::

!name expected here, with optional in/out/& just before
		parammode:=var_param
		case lx.symbol
		when opsym then
			if lx.subcode<>j_in then serror("params/op?") fi
			parammode:=in_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when koutsym,addrsym then
			parammode:=out_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when questionsym then
			if pmode<>tvariant then serror("? not on variant") fi
			parammode:=optional_param
		esac

		checksymbol(namesym)
		++nparams
		stname:=getduplnameptr(owner,lx.symptr,paramid)
		adddef(owner,stname)
		lex()
		if parammode=out_param then
			m:=createrefmode(procowner,pmode)
		else
			m:=pmode
		fi

		storemode(7,owner,m,&stname^.mode)
		stname^.parammode:=parammode
		addlistparam(&stlist,&stlistx,stname)

		case lx.symbol
		when assignsym then
			lex()
dodefvalue::
			stname^.code:=readunit()
			stname^.equals:=1
			stname^.optional:=1
		when opsym then
			if lx.subcode=j_eq then
				lex()
				goto dodefvalue
			fi
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

function readparams_types(ref strec procowner,owner,int fflang,&varparams,&nparams,int pmode)ref strec=			!READPARAMS
!read types-only non-empty parameter list, only for ffi
!positioned at first symbol after '('
	var ref strec stlist, stlistx, stname
	var int firstparam,m

	var [30]char str
	stlist:=stlistx:=nil
!	pmode:=tvoid
	stname:=nil
	nparams:=0
	goto gotmode

	do
		if lx.symbol=ellipsissym then
			varparams:=1
			lex()
			checksymbol(rbracksym)
			exit
		fi

		pmode:=readtypespec(procowner)
gotmode::
		++nparams
		print @&.str,(ctarget|"_"|"$"),,nparams
		stname:=getduplnameptr(owner,addnamestr(&.str),paramid)
		adddef(owner,stname)
		m:=pmode
		storemode(8,owner,pmode,&stname^.mode)
		addlistparam(&stlist,&stlistx,stname)

		case lx.symbol
		when assignsym,opsym then
			if lx.symbol=opsym and lx.subcode<>j_eq then
				serror("rdparams")
			fi
			lex()
			stname^.code:=readunit()
			stname^.equals:=1
		when namesym then
			serror("Can't mixed unnamed/named params")
		endcase

		case lx.symbol
		when commasym then
			lex()
		when rbracksym then
			exit
		else
			serror("typeparams3")
		endcase

	od
!PS("HERE")
	return stlist
end

function readcondsuffix(unit p)unit=			!READCONDSUFFIX
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond

switch lx.symbol
when kwhensym then
	lex()
	return createunit2(j_if,readunit(),createunit1(j_block,p))
when kunlesssym then
	lex()
	return createunit2(j_if, createunit1(j_notl,readunit()),createunit1(j_block,p))
else
	return p
endswitch
end

function readif:unit=
!at 'if'
var int line, kwd, lineno
var unit pthen,pcond, plist,plistx, pelse, p, pelsif

line:=lx.lineno

kwd:=lx.symbol			!in case coming from elsecase etc

lex()
pcond:=readsunit()
skipsemi()

checksymbol(kthensym)
lex()

pthen:=readsunit()

if lx.symbol=kelsifsym then
	lineno:=lx.lineno
	plist:=plistx:=createunit2(j_elsif,pcond,pthen)

	while lx.symbol=kelsifsym do
		lineno:=lx.lineno
		lex()
		pcond:=readunit()
		checksymbol(kthensym)
		lex()
		pthen:=readsunit()
		pelsif:=createunit2(j_elsif,pcond,pthen)
		pelsif^.lineno:=lineno
		addlistunit(&plist,&plistx,pelsif)

	od

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym,kwd,0)
		lex()
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym,kwd,0)
		lex()
	esac

	p:=createunit2(j_longif,plist,pelse)
	p^.lineno:=line
	return p
fi

case lx.symbol
when kelsesym then		!get r=any else stmt or nil
	lex()
	pelse:=readsunit()
	checkend(kendsym,kwd)
	lex()
when kelsecasesym,kelseswitchsym then
	lx.symbol:=kwd
	pelse:=makeblock(readswitchcase())
else
	PELSE:=NIL
	checkend(kendsym,kwd)
	lex()
esac

p:=createunit3(j_if,pcond,pthen,pelse)
p^.lineno:=line
return p
end

function readgoto(int gototag=j_goto)unit=	!READGOTO
var ref strec d
var unit p

if lx.subcode=1 then		!go used
	lex()
	checksymbol(ktosym)
fi
lex()

if lx.symbol=namesym and nextlx.symbol<>ptrsym and nextlx.symbol<>lsqsym and \
	nextlx.symbol<>dotsym then			!assume simple label
	p:=createname(lx.symptr)

	lex()
else
	serror("GOTO LABEL EXPR")
fi

return readcondsuffix(createunit1(gototag,p))
end

function readunless:unit=
var int line
var unit pcond, pthen, pelse, p
line:=lx.lineno
lex()
pcond:=readsunit()
checksymbol(kthensym)
lex()

pthen:=readsunit()

if lx.symbol=kelsesym then
	lex()
	pelse:=readsunit()
else			!assume simple if-then
	PELSE:=NIL
fi
checkend(kendsym,kunlesssym)
lex()
p:=createunit3(j_if,createunit1(j_notl,pcond),pthen,pelse)
p^.lineno:=line
return p
end

function readswitchcase:unit=
var int line, kwd, opc, lineno,rangeused, nwhen
var unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

line:=lx.lineno
kwd:=lx.symbol			!remember kcasesym etc
opc:=lx.subcode			!pick up tag: kcase etc

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
	lineno:=lx.lineno
	lex()
	pwhen:=pwhenx:=nil
	do
		p:=readunit()
		++nwhen
		p^.lineno:=lineno
		if p^.tag=j_makerange then rangeused:=1 fi
		addlistunit(&pwhen,&pwhenx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od
	checksymbol(kthensym)
	lex()
	pthen:=readsunit()
	pwhenthen:=createunit2(j_whenthen,pwhen,pthen)
	pwhenthen^.lineno:=lineno
	addlistunit(&pwhenlist,&pwhenlistx,pwhenthen)
od

case lx.symbol
when kelsesym then		!get r=any else stmt or nil
	lex()
	pelse:=readsunit()

	checkend(kendsym,kwd)
	lex()
when kelsifsym then
	lx.symbol:=kwd
	pelse:=makeblock(readif())
when kelsecasesym, kelseswitchsym then
	lx.symbol:=kwd
	pelse:=makeblock(readswitchcase())
else
	PELSE:=NIL
	checkend(kendsym,kwd)
	lex()
esac

p:=createunit3(opc,pexpr,pwhenlist,pelse)
p^.lineno:=line
return p
end

function readstop:unit=
var unit p
var int i
lex()
if exprstarterset[lx.symbol] then
	p:=createunit1(j_stop,readunit())
else
	p:=createunit0(j_stop)
fi
return readcondsuffix(p)
end

function readreturn:unit=
var unit p,q,r

lex()
if exprstarterset[lx.symbol] then
	q:=readunit()
	p:=createunit1(j_return,q)
	p^.length:=1
!	while lx.symbol=commasym do
!		lex()
!		r:=readunit()
!		++p^.length
!		q^.nextunit:=r
!		q:=r
!	od
else
	p:=createunit0(j_return)
	p^.length:=0
fi

return readcondsuffix(p)
end

function readdo:unit=
	var unit p
	var int line

	line:=lx.lineno
	lex()
	p:=readsunit()
	checkend(kendsym,kdosym)
	lex()
	p:=createunit1(j_do,p)
	p^.lineno:=line
	return p
end

function readto:unit=
var int line,id
var unit p, pcount, pbody

line:=lx.lineno
lex()

pcount:=readunit()

checksymbol(kdosym)
lex()
pbody:=readsunit()
checkend(kendsym,ktosym,kdosym)
lex()
id:=frameid
if currproc^.nameid<>procid then id:=staticid fi

p:=createunit3(j_to,pcount,pbody,createname(getavname(currproc,id)))
p^.lineno:=line
return p
end

function readwhile:unit=
var int line,id
var unit pcond, pa, pb, pc, pbody, p

line:=lx.lineno
lex()

pcond:=readsunit(1)

checksymbol(kdosym)
lex()
pbody:=readsunit()

checkend(kendsym,kwhilesym,kdosym)
lex()

p:=createunit2(j_while,pcond,pbody)
p^.lineno:=line
return p
end

function readrepeat:unit=
var int line
var unit pbody, pcond, p

line:=lx.lineno
lex()
pbody:=readsunit()
checksymbol(kuntilsym)
lex()
pcond:=readunit()
p:=createunit2(j_repeat,pbody,pcond)
p^.lineno:=line
return p
end

function readloopcontrol:unit=
var int opc
var unit p

opc:=lx.subcode
lex()
if lx.symbol=namesym and eqstring(lx.symptr^.name,"all") then
	lex()
	p:=createunit1(opc,createconstunit(0,tint))

elsif exprstarterset[lx.symbol] then
	p:=createunit1(opc,readconstexpr(1))
else
	p:=createunit1(opc,createconstunit(1,tint))
!	p:=createunit0(opc)
fi
return readcondsuffix(p)
end

function readprint:unit=
var int oldinreadprint, opc, isfprint, fshowname, length
var unit pformat, pdev, printlist,printlistx, p,q
var ref strbuffer expr

var ichar s

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode

case opc
when j_fprint,j_fprintln,j_cprint,j_cprintln then
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
	if not exprstarterset[lx.symbol] and opc=j_cprintln then
		goto finish
	fi
	pformat:=readunit()
	if lx.symbol=commasym then lex() else goto finish fi
fi

if not exprstarterset[lx.symbol] then
	goto finish
fi

do
	if lx.symbol=commasym then		!assume extra comma, meaning nogap
		addlistunit(&printlist,&printlistx, createunit0(j_nogap))
	else

		fshowname:=0
		if lx.symbol=opsym and lx.subcode=j_eq then
			fshowname:=1
			lex()
		fi

		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(j_fmtitem,p,readunit())
		fi
		if fshowname then
			expr:=strexpr(p)
			strbuffer_add(expr,"=")
			s:=expr^.strptr
			iconvucn(expr^.strptr,expr^.length)

			addlistunit(&printlist,&printlistx,q:=createstringconstunit(s,expr^.length))
		fi
		addlistunit(&printlist,&printlistx,p)
	fi
	if lx.symbol<>commasym then exit fi
	lex()
od

finish::
inreadprint:=oldinreadprint
if opc=j_print and printlist=nil then
	serror("No print items")
fi
if opc=j_fprint and printlist=nil and pformat=nil then
	serror("No print items")
fi
if opc=j_cprint and printlist=nil and pformat=nil then
	serror("No cprint items")
fi

if isfprint then
	if pformat=nil and opc<>j_cprintln then
		serror("No fmt str")
	fi
	return createunit3(opc,pdev,pformat,printlist)
else
	return createunit2(opc,pdev,printlist)
fi
end

function readread:unit=
var int oldinreadprint,opc
var unit pformat, pdev, readlist, readlistx, p, pread

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lex()

readlist:=readlistx:=nil
pformat:=pdev:=nil

if lx.symbol=atsym then
	if opc=j_read then
		serror("@ on read")
	fi
	lex()
	pdev:=readunit()
!	if lx.symbol=commasym then lex() else goto finish fi
	if lx.symbol=commasym then lex() fi
fi

if opc=j_readln then
	addlistunit(&readlist,&readlistx,createunit1(j_readln,pdev))
fi

if not exprstarterset[lx.symbol] then
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

	pread:=createunit1(j_read,pformat)

	p:=createunit2(j_assignx,p,pread)

	addlistunit(&readlist,&readlistx,p)
	if lx.symbol<>commasym then exit fi
	lex()
od

finish::
inreadprint:=oldinreadprint
if opc=j_read and readlist=nil then
	serror("No read items")
fi

return makestmtblock(readlist)
end

function readtry:unit=
var unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
++try_level
lex()

ptry:=readsunit()
pexceptlist:=pexceptlistx:=nil			!list of j_except items

while lx.symbol=kexceptsym do
	lex()
	exlist:=exlistx:=nil				!list of exception codes for this 'except'
	do
		addlistunit(&exlist,&exlistx,readconstexpr())
		if lx.symbol<>commasym then exit fi
		lex()
	od
	checksymbol(kthensym)
	lex()
	px:=readsunit()
	addlistunit(&pexceptlist,&pexceptlistx,createunit2(j_except,exlist,px))
od
checkend(kendsym,ktrysym)
lex()

--try_level

return createunit2(j_try,ptry,pexceptlist)
end

function readraise:unit=
var unit p

lex()
p:=readunit()
return createunit1(j_raise,p)
end

function readfor:unit=
!on 'for'; syntax is::
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

var int line, opc, down
var unit pstep, pvar, pcond, pfrom, pto, pelse, prange, prangex, pautovars, pbody, p
var ref strec d

line:=lx.lineno
lex()			!skip 'for'
pvar:=readterm2()

if pvar^.tag<>j_name then
	serror("For: name expected")
fi

opc:=j_forup
pstep:=nil
pcond:=nil

if lx.symbol=opsym then	!assume in/inrev
	if lx.subcode=j_inrev then
		down:=j_fordown
	elsif lx.subcode<>j_in then
		serror("in/inrev expected")
	fi
	lex()
	prange:=readunit()

!now split prange into from/to parts
	pfrom:=getrangelwbunit(prange)
	pto:=getrangeupbunit(prange)

else
	if lx.symbol=assignsym then
		lex()
		pfrom:=readunit()
	else
		pfrom:=createconstunit(1,tint)
	fi
	checksymbol(ktosym)
	opc:=(lx.subcode=1|j_fordown|j_forup)
	lex()
	pto:=readunit()

	if lx.symbol=kbysym then
		if opc=j_fordown then
			serror("downto/by")
		fi
		lex()
		pstep:=readconstexpr(0)
		if pstep^.tag=j_const and pstep^.value=1 then		!by 1
			opc:=j_forup
			pstep:=nil
		elsif pstep^.tag=j_const and pstep^.value=-1 then	!by -1
			opc:=j_fordown
			pstep:=nil
		fi

	else
		pstep:=nil
	fi
fi

if lx.symbol=kwhensym then
	lex()
	pcond:=readunit()
fi
checksymbol(kdosym)
lex()
pbody:=readsunit()

if lx.symbol=kelsesym then
	lex()
	pelse:=readsunit()
else
	pelse:=nil
fi
checkend(kendsym,kforsym,kdosym)
lex()

if pcond<>nil then
	pbody:=makeblock(createunit2(j_if,pcond,pbody))
fi

!deal with complex limit
pautovars:=nil
unless pto^.tag=j_const or pto^.tag=j_name then
	pautovars:=createname(getavname(currproc))
	if pstep then
		unless pstep^.tag=j_const or pstep^.tag=j_name then
			pautovars^.nextunit:=createname(getavname(currproc))
		end
	fi
end

!have the following elements::
!pvar		never nil
!pfrom		never nil
!pto		never nil
!pstep		can be nil
!pbody		never nil (might be empty block)
!pelse		can be nil
!pautovar	can be nil

!suggest layout is::

! a = pvar, pfrom, pto, [pstep]
! b = pbody [pelse]
! c = [pautovar]

pvar^.nextunit:=pfrom

pfrom^.nextunit:=pto
pto^.nextunit:=pstep
pbody^.nextunit:=pelse

p:=createunit3(opc,pvar,pbody,pautovars)
p^.lineno:=line
return p
end

function readforall:unit=
!on 'forall'; syntax is::
! forall term|, in/inrev expr|, [when expr] do stmts [else stmts] end/od
var int opc, line, isforall
var unit pindex, pvar, pcond, plist, pbody, pelse,p, pfor, pautovar

line:=lx.lineno
opc:=lx.subcode
isforall:=opc=j_forall

lex()			!skip 'forall'
pvar:=readterm2()
pindex:=nil

if lx.symbol=commasym then
	lex()
	pindex:=pvar
	pvar:=readterm2()
fi

if pvar^.tag<>j_name then
	serror("forall var not name")
fi

pcond:=nil

if lx.symbol=opsym and (lx.subcode=j_in or lx.subcode=j_inrev) then
	if lx.subcode=j_inrev then opc:=(opc=j_forall|j_forallrev|j_foreachrev) fi
	lex()
	plist:=readunit()
else
	serror("in/inrev expected")
fi

if lx.symbol=kwhensym then
	lex()
	pcond:=readunit()
fi
checksymbol(kdosym)
lex()
pbody:=readsunit()

if lx.symbol=kelsesym then
	lex()
	pelse:=readsunit()
else
	pelse:=nil
fi
checkend(kendsym,kforallsym,kdosym)
lex()

if pindex=nil then		!need to add auto-var
	pindex:=createname(getavname(currproc))
fi

if pcond<>nil then
	pbody:=createunit2(j_if,pcond,pbody)
	pbody^.lineno:=line
fi

pautovar:=createname(getavname(currproc))		!stores limit

!now have the following elements
!pindex		user var or auto
!pvar
!plist
!pbody		never nil
!pelse		can be nil
!plimitav	can be nil when limit is const
!plistav	can be nil

!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, pvar, plist
! b = pbody, [pelse]
! c = pautovar

pindex^.nextunit:=pvar
pvar^.nextunit:=plist
pbody^.nextunit:=pelse

pfor:=createunit3(opc, pindex, pbody, pautovar)
pfor^.lineno:=line
return pfor
end

global proc readtypedef(ref strec owner,int isglobal=0)=
!at 'type' symbol
var ref strec sttype,stname
var int t,m

lex()
checksymbol(namesym)
stname:=lx.symptr

lex()
checkequals()
lex()

sttype:=nil

if sttype=nil then
	sttype:=getduplnameptr(owner,stname,typeid)
	adddef(owner,sttype)
	m:=createusertype(sttype)
	ttusercat[m]:=1
else
	m:=sttype^.mode
fi

t:=readtypespec(sttype,m)		!should return filled-in version of m

sttype^.isglobal:=isglobal

storemode(9,owner,t,&sttype^.mode)

!CPL "TYPEDEF:",T
if t>=0 then
	ttisint[m]		:= ttisint[t]
	ttisword[m]		:= ttisword[t]
	ttiswordchar[m]	:= ttiswordchar[t]
	ttisreal[m]		:= ttisreal[t]
	ttisinteger[m]	:= ttisinteger[t]
	ttisnumeric[m]	:= ttisinteger[t] ior ttisreal[t]
	ttisshortint[m]	:= ttisshortint[t]
	ttisshortreal[m]:= ttisshortreal[t]
	ttisbit[m]		:= ttisbit[t]
	ttisbit[m]		:= ttisbit[t]
	ttisref[m]		:= ttisref[t]
	tttypecode[m]	:= tttypecode[t]
fi
end

global proc readrecordfields(ref strec owner,int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
var int nvars
var ref strec stname,stbitfield

nvars:=0
while lx.symbol=namesym do

	stname:=getduplnameptr(owner,lx.symptr,fieldid)
!	stname^.mode:=m
	storemode(10,owner,m,&stname^.mode)
	++nvars

	if unionpend.ulength then
		unionstr_copy(&stname^.uflags,&unionpend)
		unionstr_concat(&unionstring,&unionpend)
		unionstr_clear(&unionpend)
	else
		unionstr_clear(&stname^.uflags)
	fi
	unionlastvar:=stname			!filled in from outside with 'E' codes

	adddef(owner,stname)

	lex()

	case lx.symbol
	when atsym then
		lex()
		stname^.at:=2
		stname^.equivfield:=readequivfield(owner)

	when datsym then
		lex()
		checksymbol(intconstsym)
		case lx.value
		when 1,2,4,8,16 then
			stname^.align:=lx.value
		when 0 then
			stname^.align:=255
		else
			serror("@@ bad align")
		esac
		lex()	
	when colonsym then				!read bitfields
		lex()
!format is var int : (a:1, b:3, c:2)
		checksymbol(lbracksym)

		repeat
			lex()
			checksymbol(namesym)
			stbitfield:=getduplnameptr(owner,lx.symptr,fieldid)
			stbitfield^.mode:=tbitfield
			adddef(owner,stbitfield)

			stbitfield^.at:=2
			stbitfield^.equivfield:=stname

			lex()
			checksymbol(colonsym)
			lex()
			checksymbol(intconstsym)
			stbitfield^.bitfieldwidth:=lx.value
			lex()

		until lx.symbol<>commasym
		checksymbol(rbracksym)
		lex()

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

global proc readtabledef(int isglobal=0)=
!at 'tabledata' symbol
var int i,ncols,nrows,enums,nextenumvalue,firstval,lastval,startline,closesym
var int ltype
var unit plower
var ichar enumtypename
var ref strec stvar,stenum,stgen
const maxcols=20
var [maxcols]ref strec varnameptrs
var [maxcols]int varlisttypes
var [maxcols]unit plist,plistx
const maxrows=500
var [maxrows]int enumvalues
!int nenums

lex()
enums:=0						!whether there is an enums column
enumtypename:=nil

if lx.symbol=lbracksym then		!tabledate(...) read enum type
	enums:=1
	lex()
	if lx.symbol=namesym then		!named type
		enumtypename:=lx.symptr^.name
		lex()
	fi					!else unnamed type (just named constants)
	checksymbol(rbracksym)
	lex()
fi

nextenumvalue:=1
nrows:=0			!number of data rows appearing
ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
while lx.symbol<>opsym do
	ltype:=readtypespec(currproc)
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

checkequals()
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
	checksymbol(lbracksym)
	lex()
	if ++nrows>maxrows then
		serror("tabledata:too many rows")
	fi

	if enums then
		checksymbol(namesym)
		stgen:=lx.symptr				!generic symbol entry
		tabledataname:=stgen^.name		!allow to be picked up by $ lx.symbol
		lex()
		if lx.symbol=opsym and lx.subcode=j_eq then
			lex()
			nextenumvalue:=readconstint()
		fi
		enumvalues[nrows]:=nextenumvalue

		stenum:=getduplnameptr(currproc,stgen,constid)
		storemode(11,currproc,tint,&stenum^.mode)
		stenum^.code:=createconstunit(nextenumvalue,tint)
		stenum^.isglobal:=isglobal
		adddef(currproc,stenum)

		if nrows=1 then firstval:=nextenumvalue fi
		lastval:=nextenumvalue

		++nextenumvalue
		if ncols then				!comma always expected
			checksymbol(commasym)		!check it
		fi
		lex()
	fi

	for i:=1 to ncols do
		addlistunit(&plist[i],&plistx[i],readunit())
		if i=ncols then
			checksymbol(rbracksym)
		else
			checksymbol(commasym)
		fi
		lex()
	od

	if lx.symbol<>commasym then exit fi
	lex()					!should be ( for next entry
	if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
od

intabledata:=0

skipsemi()
checkbeginend(closesym,ktabledatasym,startline)

!Here, I have::

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

	stvar:=getduplnameptr(currproc,varnameptrs[i],staticid)
	stvar^.code:=createunit1(j_makelist,plist[i])
	stvar^.code^.length:=nrows

	storemode(12,currproc,varlisttypes[i],&stvar^.mode)
	stvar^.isglobal:=isglobal

	adddef(currproc,stvar)
	addstatic(stvar)
od
end

global proc readclassdef(ref strec owner,int isglobal)=
!at 'class' symbol
!read enough of the class to be able to generate export data
var int kwd, baseclass, m, startline, closesym, mrec, normalexit,isrecord
var ref strec nameptr, sttype, newd, d,e

kwd:=lx.symbol
isrecord:=kwd=krecordsym

lex()
checksymbol(namesym)
nameptr:=lx.symptr

lex()
baseclass:=0
if lx.symbol=lbracksym then
	lex()
	baseclass:=readtypespec(owner)
	checksymbol(rbracksym)
	lex()
fi

checkequals()
lex()

sttype:=getduplnameptr(owner,nameptr,typeid)
adddef(owner,sttype)
m:=createusertype(sttype)

mrec:=createrecordmode(owner,(isrecord|tfixedrecord|tflexclass),m)
storemode(13,owner,mrec,&sttype^.mode)
sttype^.base_class:=baseclass

closesym:=checkbegin(1)

startline:=getcurrline()

readclassbody(sttype,kwd)

checkbeginend(closesym,kwd,startline)

if baseclass then
	d:=ttnamedef[baseclass]^.deflist
	while d do
!	forall d in baseclass.namedef.deflist do
		e:=sttype^.deflist
		normalexit:=1
		while e do
			if eqstring(d^.name,e^.name) then
				normalexit:=0
				exit
			fi
			e:=e^.nextdef
		od
		if normalexit then
!duplicate d in this class; keep it simple for now
!(procs will need a more elaborate duplication, and really needs to share code)
			case d^.nameid
			when procid,linkid then
				newd:=getduplnameptr(sttype,d,linkid)
				newd^.equivfield:=d
			else
				newd:=getduplnameptr(sttype,d,d^.nameid)
				duplfield(owner,d,newd)
			esac
			adddef(sttype,newd)
		fi
		d:=d^.nextdef
	od
fi

sttype^.isglobal:=isglobal
end

proc readclassbody(ref strec owner,int classkwd)=
!at first symbol of a class or record body
!read fields, constants, types, methods.
!classkwd=kclasssym or krecordsym
var int kwd,t
var ref strec d

unionstr_clear(&unionstring)
unionstr_clear(&unionpend)

doswitch lx.symbol
when kconstsym then
	readconstdef(owner,0)
when kfunctionsym,kprocsym then
	kwd:=lx.symbol
	if owner^.imported then
		readprocdecl(owner,0,0)
	else
		readprocdef(owner,0)
	fi
when kclasssym then
	lex()
	serror("CLASS CLASS")
when krecordsym then
	lex()
	serror("CLASS RECORD")
when ktypesym then
	lex()
	serror("CLASS TYPE")
when eofsym then
	serror("Class eof?")
	exit
when semisym then
	lex()

when namesym then			!assume user type
	++insiderecord
	t:=newusertypex(lx.symptr)
	--insiderecord
	lex()
	if lx.symbol=dotsym then
		serror("Can't do a.b type inside class")
	fi
	readrecordfields(owner,t)
when kstructsym,kunionsym then
	unionstr_append(&unionpend,(lx.symbol=kstructsym|'S'|'U'))
	unionlastvar:=nil
	lex()
when kendsym,rbracksym,rcurlysym then
	if unionstring.ulength then
		checkend(kendsym,(unionstr_last(&unionstring)='S'|kstructsym|kunionsym))
		lex()
		if unionlastvar=nil or unionpend.ulength then
			serror("Empty union group")
		fi
		case unionstr_last(&unionlastvar^.uflags)
		when 'E','*' then
		else
			unionstr_append(&unionlastvar^.uflags,'*')
		esac
		unionstr_append(&unionlastvar^.uflags,'E')
		unionstring.ulength--
	else
		exit
	fi

when kvarsym then
	lex()
	if not istypestarter() then
		serror("need type")
	fi
	++insiderecord
	t:=readtypespec(owner)
	--insiderecord
	readrecordfields(owner,t)

when kvariantsym then
	lex()
	readrecordfields(owner,tvariant)

when kletsym then
	serror("Let not allowed")

else
	if istypestarter() then
		serror("record:need var")
	else
		exit
	fi
enddoswitch
end

function readenumtype(ref strec owner,int typedefx,isglobal=0)int=		!READENUMTYPE
!read enum def, and return typespec corresponding
!typedefx is nil, or an existing, but not yet filled-in, moderec
!positioned at possible begin symbol (or at first declaration in the record)
!This is because it can be called in two ways::
!1: type name = enum <begin>...	Formal record definition
!2: enum [name=]<begin>...		Informal definition (where strictly only (...) allowed)
var ref strec enumowner, stname, nameptr
var int isanon, index, startline, closesym
var unit pone,pindex

enumowner:=owner			!owner of enum typeid
isanon:=0
if not typedefx then			!informal declaration
	if lx.symbol=namesym then		!name provided
		stname:=getduplnameptr(owner,lx.symptr,typeid)
		owner:=stname
		lex()
		checkequals()
		lex()
		adddef(enumowner,owner)
	else
		isanon:=1
	fi
	checksymbol(lbracksym)
	lex()
else
	owner:=ttnamedef[typedefx]
	startline:=getcurrline()
	closesym:=checkbegin(1)
fi

!now loop reading enum items
pone:=createconstunit(1,tint)
pindex:=pone
index:=1
!pindex:=nil

while lx.symbol=namesym do
	nameptr:=lx.symptr
	lex()
	if lx.symbol=opsym and lx.subcode=j_eq then	!= follows
		lex()
		pindex:=readunit()
	fi

	if not isanon then
		stname:=getduplnameptr(owner,nameptr,enumid)
		stname^.code:=pindex
		storemode(14,owner,tint,&stname^.mode)
		adddef(owner,stname)
	else
		stname:=getduplnameptr(enumowner,nameptr,constid)
		stname^.code:=pindex
		storemode(15,owner,tint,&stname^.mode)
		adddef(enumowner,stname)
	fi
	pindex:=createunit2(j_add,pindex,pone)

	stname^.isglobal:=isglobal

	if lx.symbol<>commasym then exit fi
	lex()
od

if not typedefx then
	checksymbol(rbracksym)
	lex()
else
	checkbeginend(closesym,kenumsym,startline)
fi

if not isanon then
	return createenummode(owner,typedefx)
else
	return tvoid
fi
end

proc duplfield(ref strec owner,p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

if p^.code then
	serror("DUPLFIELD")
fi

!Need to copy whatever are relevant attributes

!q^.attribs:=p^.attribs
q^.at:=p^.at

q^.uflags:=p^.uflags		!for ^.uflags
storemode(16,owner,p^.mode,&q^.mode)
end

proc readimportmodule(ref strec owner)=
!at 'importmodule' symbol
var int isnew,startline,closesym
var ref strec d,stname,stname0


if insidedllimport then serror("nested importdll") fi

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
d:=stname^.nextdupl
while d do
	if d^.nameid=dllmoduleid then
		stname:=d
		isnew:=0
		exit
	fi
	d:=d^.nextdupl
od

if isnew then			!new
	stname:=getduplnameptr(stmodule,stname,dllmoduleid)
	if eqstring(stname^.name,"sys") then
		stsysmodule:=stname
	fi
	adddef(stmodule,stname)
	if ndlltable>=maxdlllib then
		serror("Too many DLL libs")
	fi
	dlltable[++ndlltable]:=stname^.name
	stname^.dllindex:=ndlltable
fi

startline:=getcurrline()
closesym:=checkbegin(0)

insidedllimport:=1

readimportbody(owner)

insidedllimport:=0

checkbeginend(closesym,kimportmodulesym,startline)

end

proc readimportbody(ref strec owner)=
!positioned at first symbol of statement (which can be empty)
!return knode containing statement, or nil if not found (at 'end etc)
var int lineno,fflang

lineno:=lx.lineno

do
	skipsemi()
	switch lx.symbol
	when kfflangsym then
		fflang:=lx.subcode
		lex()
		case lx.symbol
		when kprocsym,kfunctionsym then
			readprocdecl(owner,0,fflang)
		esac	

	when kprocsym,kfunctionsym then
		readprocdecl(owner,0,0)

	when ktypesym then
		readtypedef(owner,0)

	when kconstsym then
		readconstdef(owner,1)

	when kclasssym,krecordsym then
		readclassdef(owner,0)

	when kvarsym then
		readvardef(owner,1,0,dllvarid)

	when eofsym then
		exit

	when kendsym then
		exit
	else
		PS1("symbol")
		serror("Not allowed in importmodule")
	endswitch
od
end

function readequivfield(ref strec owner)ref strec=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
var ref strec p,d

!lex()
checksymbol(namesym)
d:=lx.symptr
lex()

p:=owner^.deflist
while p do
	if eqstring(p^.name,d^.name) then
		return p
	fi

	p:=p^.nextdef
od
cpl d^.name
serror("Can't find @ field")
return nil
end

function readapplyop(int inexpr)unit=
var unit p,a,b

lex()
checksymbol(lbracksym)
lex()
p:=readunit()
checksymbol(commasym)
lex()
a:=readunit()
b:=nil

if lx.symbol=commasym then
	lex()
	b:=readunit()
fi
checksymbol(rbracksym)
lex()

return createunit3((inexpr|j_applyopx|j_applyop),p,a,b)
end

function readrefproc(ref strec owner,int typedefx,int fflang)int=			!READREFPROC
!'ref' was seen, now positioned at 'proc' 'function' or 'method'
!read proc params and any result, return a complete ref proc spec
var int kwd,prettype,m,varparams,nparams
var [4]int retmodes
var ref strec paramlist,stproc
var int rettype2, rettype3, nretvalues
var ichar name

kwd:=lx.symbol				!remember whether proc or function
lex()

paramlist:=nil
prettype:=tvoid
nretvalues:=0

!need to create suitable holding typename in advance
name:=nextautotype()
stproc:=getduplnameptr(stmodule,addnamestr(name),typeid)
adddef(stmodule,stproc)

if kwd=kfunctionsym then
	if lx.symbol=lbracksym then		!possible params
		lex()
		if lx.symbol<>rbracksym then
			paramlist:=readparams(owner,stproc,0,varparams,nparams)
!			axvarparams:=varparams
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
			paramlist:=readparams(owner,stproc,0,varparams,nparams)
			checksymbol(rbracksym)
		fi
		lex()
	fi
	if typestarterset[lx.symbol] or lx.symbol=colonsym or lx.symbol=sendtosym then
		serror("proc can't have ret value")
	fi
fi

m:=createrefprocmode(owner,stproc,paramlist,kwd,prettype,typedefx)

for i to nretvalues do
	storemode(17,owner,retmodes[i],&stproc^.modelist[i])
od
stproc^.nretvalues:=nretvalues

ttnamedef[m]:=stproc
stproc^.fflang:=fflang

return m
end

proc pushproc(ref strec p)=
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

function readassemline:unit=
lex()
return assembleline(1)
end

function readassemblock:unit=
!read lines of assembler after < or assem
!fend=1 when terminated by 'end', 0 when terminator is '>'
!return single nassem unit or nsunit containing multiple nassems
var unit ulist,ulistx,u

ulist:=ulistx:=nil

do
	lex()			!first symbol on line
	case lx.symbol
	when eofsym then
		serror("EOF: 'End' missing in Assembler code")
	when kendsym then
		checkend(lx.symbol,kassemsym)
		lex()
		exit
	when semisym then		!assume blank line
	else				!assume some asm code follows
		u:=assembleline(0)
		addlistunit(&ulist,&ulistx,u)
	esac
od

return makeblock(ulist)
end

function assembleline(int oneline)unit=
!1st symbol of possible assembler line has been read
!assemble following symbols, end at eol or other separater symbol
!return nassem unit

!const escapesym=atsym
var unit dlist,dlistx,p,pname,q
!var [64]char str
!var [512]char deststr
!var ref char pdest
var ichar name
var int opc,noperands
var ref strec stname

dlist:=dlistx:=nil

!CPL "ASSEM",lx.lineno iand 16777215

!look at the start of a line first

if lx.symbol=namesym and nextlx.symbol in [colonsym,dcolonsym] then	!normal label
	p:=createunit0(j_labeldef)
	stname:=getduplnameptr(currproc,lx.symptr,labelid)
	p^.def:=stname
	adddef(currproc,stname)
	lex()			!skip colon
	if oneline then
		lex()
	fi
	return p

elsif lx.symbol=opsym and lx.subcode=j_mul then		!*name	macro invocation
	lex()
	checksymbol(namesym)
	pname:=createname(lx.symptr)
	pname^.lineno:=lx.lineno

	lex()
	if lx.symbol<>semisym then
		repeat
			addlistunit(&dlist,&dlistx,readunit())
			if lx.symbol=commasym then
				lex()
			fi

		until lx.symbol in [semisym,eofsym]
	fi

	return createunit2(j_assemmacro,pname,dlist)
fi

if lx.symbol=opsym then
	case lx.subcode
	when j_andl then opc:=m_andx
	when j_orl then opc:=m_orx
	when j_xorl then opc:=m_xorx
	when j_notl then opc:=m_notx
	else
			serror("Asm op??")
	esac
	p:=createunit0(j_assem)
	p.opcode:=opc
	lex()

elsif lx.symbol=namesym then				!assume opcode

!CPL "ASSUMING OPCODE",SYMBOLNAMES[LX.SUBCODE]

	p:=createunit0(j_assem)

	case lx.subcode
	when asmopcodesym then
		p.opcode:=lx.symptr.index
	when jmpccsym then
		p.opcode:=m_jmpcc
		p.cond:=lx.symptr.index
	when setccsym then
		p.opcode:=m_setcc
		p.cond:=lx.symptr.index
	when movccsym then
		p.opcode:=m_cmovcc
		p.cond:=lx.symptr.index
	else
		serror("x64 op expected")
	esac

	lex()
else
	SERROR("ASM???")
fi

!any labels and opcodes have been read; now look at any operands
if lx.symbol not in [semisym,eofsym] then

noperands:=0

	do
		q:=readassemopnd()

		++noperands
		case noperands
		when 1 then p.a:=q
		when 2 then p.b:=q
		when 3 then p.c:=q
		else
			serror("Too many asm opnds")
		esac

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
	var int reg,regix,scale,prefixmode
	var unit pcode

	case lx.symbol
	when intconstsym,realconstsym then
		return readunit()
	when namesym then
		case lx.symptr^.subcode
		when regsym then
			p:=createunit0(j_assemreg)
			p.index:=lx.symptr.index
			p.regsize:=lx.symptr.regsize
			lex()
			return p
		when xregsym then
			p:=createunit0(j_assemxreg)
			p.index:=lx.symptr.index
			lex()
			return p
		esac
		return readunit()
	when opsym then
		return readunit()

	when stdtypesym then
		case lx.subcode
		when tu8,tu16,tu32,tu64 then
		else
			serror("Bad prefix")
		esac
		prefixmode:=lx.subcode
		lex()
		checksymbol(lsqsym)
		goto gotprefix

	when lsqsym then
		prefixmode:=tvoid
gotprefix::
		reg:=regix:=0
		pcode:=nil
		scale:=1

		lex()
		if lx.symbol=namesym and lx.symptr.subcode=regsym then
			reg:=lx.symptr.index
			lex()
		fi

		if lx.symbol=opsym and nextlx.symbol=namesym and nextlx.symptr.subcode=regsym then
			case lx.subcode
			when j_add then
				lex()
			else
				gerror("reg-reg etc")
			esac
		fi
		if lx.symbol=namesym and lx.symptr.subcode=regsym then
			regix:=lx.symptr.index
			lex()
		fi

!PS("PARSE3")
		if lx.symbol=opsym and lx.subcode=j_mul then
			lex()
			checksymbol(intconstsym)
			case scale:=lx.value
			when 1,2,4,8 then
			else
				serror("Bad scale")
			esac
			lex()
		fi

!PS("PARSE4")
		case lx.symbol
!		when opsym, intconstsym, namesym, lbracksym then
		when opsym, intconstsym, namesym, lbracksym,ksyscallsym then
!PS("PARSE5")
			pcode:=readunit()
		esac
		checksymbol(rsqsym)
		lex()
		p:=createunit1(j_assemmem,pcode)
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
		serror("ASM: Bad operand?")
	esac
	return nil
end

function makeastring:unit =
!current symbol is an 'astring', like a regular string constant, but intended
!to be a byte-array
!Simplest treatment, if not the most efficient, is to turn that into normal 
!makelist unit
var unit ulist,ulistx, p, pconst
var ref char s
var int length

ulist:=ulistx:=nil

s:=lx.svalue
length:=lx.length
to length do
	pconst:=createconstunit(s^,ti64)
	addlistunit(&ulist,&ulistx,pconst)
	++s
od

if lx.subcode='Z' then
	pconst:=createconstunit(0,ti64)
	addlistunit(&ulist,&ulistx,pconst)
	++length
fi

p:=createunit1(j_makelist,ulist)
p^.length:=length
return p
end

function readreturntype(ref strec owner, []int &retmodes)int=
!read 1, 2 or 3 return types as part of function decl
var int nretvalues

retmodes[1]:=readtypespec(owner)
nretvalues:=1
while lx.symbol=commasym do
	if nretvalues>=4 then
		serror("Too many return values")
	fi
	lex()
	retmodes[++nretvalues]:=readtypespec(owner)
od

return nretvalues
end

function readset:unit=
!positioned at "["
	var int length,nkeyvalues
	var unit p,ulist,ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(j_makeset,nil)
	when colonsym then
		lex()
		checksymbol(rsqsym)
		lex()
		return createunit1(j_makedict,nil)
	esac

	p:=readunit()
	length:=1
	nkeyvalues:=0
	if p^.tag=j_keyvalue then ++nkeyvalues fi

	ulist:=ulistx:=p

	while lx.symbol=commasym do
		lex()
		addlistunit(&ulist,&ulistx,p:=readunit())
		if p^.tag=j_keyvalue then ++nkeyvalues fi

		++length
		skipsemi()						!allow a,b,c;]
	od

	checksymbol(rsqsym)
	lex()
	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(j_makedict,ulist)
	else
		p:=createunit1(j_makeset,ulist)
	fi
	p^.length:=length
	return p
end

function istypestarter:int=
	if typestarterset[lx.symbol] or (lx.symbol=namesym and nextlx.symbol=namesym) then
		return 1
	fi
	return 0
end

function readunit:unit=
!a unit is an expression or statement only; not declarations
return readfactor(8)
end

function readfactor(int level)unit=
var unit p,q,r
var int opc,opprio,lineno,isassign

if level<=1 then		!level might be 0
	if lx.symbol=namesym then
		case nextlx.symbol
		when semisym,commasym then
			p:=createname(lx.symptr)
			p^.lineno:=lx.lineno
			lex()
			return p
		when opsym,assignsym then
			p:=createname(lx.symptr)
			p^.lineno:=lx.lineno
			lex()
			goto gotterm
		esac
	fi
	p:=readterm2()
else
	p:=readfactor(level-1)
fi

doswitch lx.symbol
when opsym, assignsym, addrsym, rangesym, deepcopysym then
gotterm::
	opc:=lx.subcode				!will be kadd, kassign, etc
	lineno:=lx.lineno

	if nextlx.symbol in [assignsym,deepcopysym] then
		lex()
		isassign:=1
		opprio:=jtagpriotable[j_assignx]
		opc:=getoptocode(opc)
	else
		isassign:=(opc=j_assignx or opc=j_deepcopyx)
		opprio:=jtagpriotable[opc]
	fi
	if opprio<>level then exit fi

	lex()

	if isassign then			!assign is right-to-left but also special
		q:=readunit()
	elsif opc=j_power then			!power is right-to-left
		q:=readfactor(level)
	else
		q:=readfactor(level-1)
	fi
	p:=createunit2(opc,p,q)
	p^.lineno:=lineno

else
	exit
enddoswitch
return p
end

function readterm2:unit=
!	int oldinrp,lineno,opc
	var unit p,q,r
var ref char pbyte
var word64 a
var int oldipl,opc,oldinrp,lineno,shift,t

	lineno:=lx.lineno

	p:=readterm()

	doswitch lx.symbol
	when lbracksym then
		lex()
		oldinrp:=inreadprint
		inreadprint:=0
		q:=readslist(1,1)
		checksymbol(rbracksym)
		lex()
		if p.tag=j_syscall then
			p.a:=q
		else
			p:=createunit2((p^.tag=j_dot|j_callmfn|j_callfn),p,q)
		fi
		inreadprint:=oldinrp
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(j_ptr,p)
		lex()

	when lsqsym then
		p:=readindex(p,0)

	when dotsym then
		p:=readdotsuffix(p)

	when lcurlysym then
		p:=readkeyindex(p)

	when colonsym then
		if inreadprint then exit fi
		lex()
		q:=readunit()
		p:=createunit2((inparamlist|j_keyword|j_keyvalue),p,q)

	when incrsym then
		case lx.subcode
		when j_preincrx then opc:=j_postincrx	!1
		when j_predecrx then opc:=j_postdecrx	!1
		esac
		lex()
		p:=createunit1(opc,p)

	when anddotsym then
		lex()
		checksymbol(lsqsym)
		lex()
		q:=readunit()
		if q^.tag=j_makerange then
			p:=createunit2(j_anddotslice,p,q)
		else
			p:=createunit2(j_anddotindex,p,q)
		fi
		checksymbol(rsqsym)
		lex()
	else
		exit
	enddoswitch

	p^.lineno:=lineno

	return p
end

function readterm:unit=
var unit p,q,r
var ref char pbyte
var word64 a
var int oldipl,opc,oldinrp,lineno,shift,t

	lineno:=lx.lineno

	switch lx.symbol
	when namesym then
		if nextlx.symbol=atsym then		!type-punning with user type
			p:=readcast()
		else
			p:=createname(lx.symptr)
			p^.lineno:=lx.lineno
			lex()
		fi

	when intconstsym,realconstsym then
		p:=createconstunit(lx.value,lx.subcode)
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue,lx.length)
		lex()

	when astringconstsym then
		p:=makeastring()
		lex()

	when decimalconstsym then
		(lx.svalue+lx.length)^:=0
		p:=createunit0(j_decimal)
		p^.svalue:=lx.svalue
		p^.slength:=lx.length
		p^.mode:=tflexdecimal
		lex()

	when charconstsym then
		a:=0
		shift:=0
		pbyte:=lx.svalue
		to lx.length do
			a:=a ior word64(pbyte^)<<shift
			shift+:=8
			++pbyte
		od
		if a<=0x7FFF'FFFF'FFFF'FFFF then
!			t:=ti64
			t:=tc64
		else
			t:=tc64
		fi
		p:=createconstunit(a,t)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym,krefsym,kicharsym,ktypeofsym then
		p:=readcast()

	when opsym then
		p:=readopc()

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc,readterm2())

	when ksprintsym then
		p:=readsprint()

	when ksreadsym,ksreadlnsym then
		p:=readsread()

	when addrsym then
		lex()
		p:=createunit1(j_addrof,readterm2())
		if p^.a^.tag=j_callfn then
			if p^.a^.b then
				serror("Params not allowed")
			fi
			p^.a:=p^.a^.a			!lose the call
		fi

	when anddotsym then
		lex()
		p:=createunit1(j_addroffirst,readterm2())

	when compilervarsym then
		p:=readcompilervar()

	when kerrorsym then
		p:= createconstunit(lx.subcode,tint)
		lex()

	when dollarsym then
		if intabledata then
			p:=createstringconstunit(tabledataname,-1)
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(j_upb,dollarstack[ndollar])
		fi
		lex()

	when kapplyopsym then
		p:=readapplyop(1)

	when kcastsym then
		p:=readcastx()

	when ktypeconstsym then
		lex()
		checksymbol(lbracksym)
		lex()
		p:=createunit0(j_typeconst)

		storemode(3,currproc,readtypespec(currproc),cast(&p^.value))
		checksymbol(rbracksym)
		lex()

	when kclampsym then
		lex()
		checksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		if lx.symbol=rbracksym and q^.tag=j_makerange then
			r:=q^.b
			q:=q^.a
		else
			checksymbol(commasym)
			lex()
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(j_max,p,q)
		p:=createunit2(j_min,q,r)

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

	when kforallsym then
		p:=readforall()

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

	when ktrysym then	!todo
		p:=readtry()

	when kraisesym then	!todo
		p:=readraise()

	when kswapsym then			!swap using function syntax
		lex()
		checksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		checksymbol(rbracksym)
		lex()
		p:=createunit2(j_swap,p,q)

	when kevalsym then
		lex()
		p:=createunit1(j_eval,readunit())

	when kassemsym then
		currproc^.asmused:=1
		assemmode:=1
		if lx.subcode=0 then
			p:=readassemline()
		else
			p:=readassemblock()
		fi
		assemmode:=0

	when lcurlysym then
		lex()
		p:=readsunit()
		checksymbol(rcurlysym)
		lex()
		p:=createunit1(j_lambda,p)

	when kemitcsym then
		p:=createstringconstunit(lx.svalue,lx.length)
		p.tag:=j_emitc
		lex()

	when ksyscallsym then
		p:=createunit0(j_syscall)
		p.opcode:=lx.subcode
		lex()

	else
		cpl symbolnames[lx.symbol],=LX.SYMBOL
		serror("readterm?")
	endswitch

	p^.lineno:=lineno
	return p
end

function readxunit:unit=
	return readsunit()
end

function readsunit(int inwhile=0)unit=
var int lineno,m
var unit ulist,ulistx,p,q,r
var ref strec stname

!if lx.symbol=colonsym then
!	lex()
!	skipsemi()
!	p:=readunit()
!	if lx.symbol=semisym then
!		lx.symbol:=kendsym
!	fi
!	return p
!fi

lineno:=lx.lineno
ulist:=ulistx:=nil

repeat
	while lx.symbol=semisym do
		lex()
	od
	switch lx.symbol
	when kstaticsym then
		lex()
		checksymbol(kvarsym)
		readvardef(currproc,0,1,staticid)

	when kprocsym,kfunctionsym then
		readprocdef(currproc,0)

	when stdtypesym,lsqsym,krefsym,kicharsym,ktypeofsym then
!assume cast
		goto doexec

	when kvarsym,kvariantsym,kletsym then
		q:=readvardef(currproc,0,0,frameid)
		while q do								!initialised decls involve code
			r:=q^.nextunit						!unlink from this block first
			q^.nextunit:=nil
			addlistunit(&ulist,&ulistx,q)		!add one by-one
			q:=r
		od
!			
	when ktypesym then
		readtypedef(currproc,0)

	when kconstsym then
		readconstdef(currproc,0)

	when kclasssym,krecordsym then
		readclassdef(currproc,0)

	when docstringsym then
		serror("DOCSTRING")
!		currproc.docstring append:=lxvalue
!		lex()

	when kenumsym then		!enum
		lex()
		readenumtype(currproc,0)

	when kmacrosym then
		readmacrodef(currproc,0)

	when eofsym then
		cpl currproc^.name
		serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,
			kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
		exit
!
	when namesym then
		if nextlx.symbol=dcolonsym then
			p:=createunit0(j_labeldef)
			stname:=getduplnameptr(currproc,lx.symptr,labelid)
			adddef(currproc,stname)
			p^.def:=stname
			p^.trylevel:=try_level
			lex()
			lx.symbol:=semisym
			addlistunit(&ulist,&ulistx,p)

		else
			goto doexec
		fi
	when kdosym then				!u;u;u;do rather than u;u;u do
		if inwhile then
			exit
		fi
		goto doexec

	when semisym then
	else							!assume a statement
doexec::
		p:=readunit()
		if p^.tag=j_name and lx.symbol=namesym then
			serror("Possibly var/let needed")
		fi
		addlistunit(&ulist,&ulistx,p)
		if lx.symbol=kdosym then
			exit
		fi

	endswitch
until lx.symbol<>semisym

case lx.symbol
when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,
	kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym,commasym,
	barsym then
else
	serror("Readsunit: "";"" expected, or bad unit starter")
esac

if ulist=nil or ulist^.nextunit then
	return createunit1(j_block,ulist)
else
	return ulist
fi
end

proc readmacrodef(ref strec owner, int isglobal)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported
!int kwd,varparams,try_level, prettype, nparams, rettype2, rettype3, nretvalues
!ichar metadata, truename
!ref strec pequiv, stproc, owner, paramlist,nameptr

var ref strec nameptr,stmacro, paramlist,paramlistx, stname

lex()
checksymbol(namesym)

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
				stname^.nulldef:=lx.symptr

				lex()
				if lx.symbol=rbracksym then
					exit
				fi
				checksymbol(commasym)
				lex()
			else
				serror("macro def params")
			esac
		od
	fi
	lex()						!skip )
fi
stmacro^.paramlist:=paramlist
stmacro^.isglobal:=isglobal

checkequals()
lex()
stmacro^.code:=readunit()
end

proc readimportalias(ref strec dimport)=
!positioned at 'as'; read following name as alias for the import module name
!implement as a macro
	var ref strec stmacro

	lex()					!alias name to use
	checksymbol(namesym)
	stmacro:=getduplnameptr(stmodule,lx.symptr,macroid)
	adddef(stmodule,stmacro)

	lex()

	stmacro^.paramlist:=nil
	stmacro^.code:=createname(dimport)
end

proc domappedalias(ref strec dimport, stimport)=
!dimport is generic name as it appears in source
!stimport will be actual strec for module, with actual module name
!create an alias for actual name, so I can use the generic name
	var ref strec stmacro

	if eqstring(dimport.name,stimport.name) then
		return
	fi

	stmacro:=getduplnameptr(stmodule,dimport,macroid)
	adddef(stmodule,stmacro)
	stmacro^.paramlist:=nil
	stmacro^.code:=createname(stimport)
end

function readrecase:unit=
	lex()
	return createunit1(j_recase,readunit())
end
=== mm_name.m 28/35 ===
import mlib
import clib

import mm_decls
import mm_tables
import mm_support
import mm_lib
import mm_type

var ref strec currstproc
var int allowmodname=0
var int noexpand, noassem
var int macrolevels

const maxmacroparams=50
var [maxmacroparams]ref strec macroparams
var [maxmacroparams]ref strec macroparamsgen
var [maxmacroparams]unit macroargs
var int nmacroparams
var int nmacroargs

global proc rx_unit(ref strec owner, unit p)=
var ref strec d
var unit a,b
var int n,oldnoexpand,oldnoassem

a:=p^.a
b:=p^.b
mlineno:=p^.lineno

switch p^.tag
when j_name then
	resolvename(owner,p)
	if p^.def^.nameid=macroid and not noexpand then
		++macrolevels
		expandmacro(p,p,nil)
		rx_unit(owner,p)
		--macrolevels
	fi

when j_keyword then
	rx_unit(owner,b)		!do param value only

when j_dot then
	resolvedot(owner,p)

when j_callproc, j_callfn, j_callmfn then
	if a^.tag=j_name then			!can expand possible macro if params not ready
		oldnoexpand:=noexpand; noexpand:=1
		rx_unit(owner,a)
		noexpand:=oldnoexpand
	else
		rx_unit(owner,a)
	fi
	rx_unitlist(owner,b)
	if a^.tag=j_name then
		d:=a^.def
		case d^.nameid
		when typeid then		!change to type conversion
			p^.tag:=j_convert
			storemode(17,owner,d^.mode,&p^.newmode)
			p^.a:=b
			p^.b:=nil
			if b^.nextunit then
				p^.a:=createunit1(j_makelist,b)
				n:=0
				while b do
					++n
					b:=b^.nextunit
				od
				p^.a^.length:=n
!				rxerror("cast on list")
			fi
		when macroid then
			++macrolevels
			expandmacro(p,a,b)
			rx_unit(owner,p)
			--macrolevels
		else
			if d^.mode=tvoid then
				p^.tag:=j_callproc
			fi
		esac
	fi

when j_eq,j_ne, j_lt,j_le,j_ge,j_gt then

	case p^.a^.tag
	when j_eq,j_ne, j_lt,j_le,j_ge,j_gt then
		converteqeq(owner,p)
	else
		go to doabc
	esac
	goto doabc

!when j_makelist then
!when j_index,j_dotindex then
!when j_upper then
!when j_hardconv then
!when j_assem then
!	if not noassem then
!!		rx_assem(owner,p,a,b)
!	fi

when j_assemmacro then
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

when j_forup,j_fordown then			!a will be j_name unit
	resolvename(owner,a,ti64)
	a:=a^.nextunit
	goto doabc

else
doabc::
	rx_unitlist(owner,a)
	if b then
		rx_unitlist(owner,b)
		if p^.c then rx_unitlist(owner,p^.c) fi
	fi
endswitch
end

global function rx_module(int n)int=
var modulerec m
var ref strec stmodule, d
var int globalflag,status

currmoduleno:=n

rx_passdef(stprogram,moduletable[n].stmodule)

return 1
end

global proc rx_deflist(ref strec owner,p)=
var ref strec pstart:=p
	while p do
		rx_passdef(owner,p)
		p:=p^.nextdef
	od
end

global proc rx_passdef(ref strec owner,p)=
var ref strec d

case p^.nameid
when moduleid,dllmoduleid then
	rx_deflist(p,p^.deflist)

when procid then
	fixmode(owner,p)
	rx_deflist(p,p^.deflist)
	currstproc:=p
	rx_unit(p,p^.code)
	currstproc:=nil

when dllprocid then
	fixmode(owner,p)
	rx_deflist(p,p^.deflist)

when constid,staticid,frameid,paramid then
	fixmode(owner,p)
	if p^.at=1 then
		rx_unit(owner,p^.equivvar)
	fi
	if p^.code then
		rx_unit(owner,p^.code)
	fi
when typeid then
	fixmode(owner,p)

else
esac
end

proc rx_unitlist(ref strec owner, unit p)=
while p do
	rx_unit(owner,p)
	p:=p^.nextunit
od
end

global function resolvetopname(ref strec owner,stnewname,int moduleno,fmodule)ref strec=
!stnewname points to a symrec with nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)
!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match
!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

var int i,m,extcount,modno
var ref strec p,powner,d,e,dlldef,extdef,moddef,extmod,q
var [10]ref strec ambiglist

if owner^.nameid=procid then
	q:=owner^.deflist
	while q do
		if q^.firstdupl=stnewname then		!use that match
			return q
		fi
		q:=q^.nextdef
	od
fi

p:=stnewname^.nextdupl

extcount:=0
extmod:=dlldef:=extdef:=moddef:=nil

while p do						!for each possibe st entry of the same name
	powner:=p^.owner			!the owner of that entry

	switch powner^.nameid
	when procid then
		if powner=owner then			!immediate match
			return p
		fi
	when moduleid then			!p is file-scope item
		if powner^.moduleno=moduleno then		!same module
			if owner^.nameid=moduleid then	!immediate match
				return p
			fi
			moddef:=p			!take note, but continue searching (in case proc etc)
		elsif moduletable[moduleno].importmap[powner^.moduleno] then
			if p^.isglobal then
								!matches an external module imported by this name's module
				++extcount			!if an ext match is closest, there can only be one
				extdef:=p
				if extcount<ambiglist.len then
					ambiglist[extcount]:=extdef
				fi
			fi
		fi
	when dllmoduleid then
		modno:=powner^.owner^.moduleno
		if modno=moduleno or moduletable[moduleno].importmap[modno] then
			dlldef:=p
		fi

	when typeid then
		if powner=owner then			!immediate match
			return p
		fi
	when programid then					!p is a module
		if p^.nameid=moduleid then		!match a module name
			if p^.moduleno=moduleno then
				if fmodule then
					return p			!immediate match (unless proc but that would have
				fi						!matched by now
			else						!ext module
				extmod:=p				!keep it in reserve
			fi
		fi
!	when macroid then
	endswitch

	p:=p^.nextdupl
od

!if here, then no immediate match
!either of moddef/dlldef will be set
if moddef then				!go with that first
	return moddef
fi
if extdef then
	if extcount>1 then
		for i:=1 to extcount do
			extdef:=ambiglist[i]
			println i,extdef^.owner^.name,namenames[extdef^.owner^.nameid]
		od
		rxerror_s("Ambiguous ext name: #",extdef^.name)
	fi
	return extdef
fi
if extmod then return extmod fi
return dlldef				!will be nil when no match
end

global proc resolvename(ref strec owner, unit p, int mode=tvoid)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	var ref strec d,e
	var unit q
	var int moduleno

	d:=p^.def
	moduleno:=p^.moduleno

	if d^.nameid<>nullid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)
	if not e then
		if mode=tvoid then
			rxerror_s("Undefined: #",d^.name,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e^.lineno:=p^.lineno
			e^.islet:=1
		fi
	fi

	e^.used:=1

	fixmode(owner,e)

	if e^.nameid=paramid and e^.parammode=out_param then
		p^.tag:=j_ptr
		p^.a:=createname(e)
		p^.def:=nil
	fi

	p^.def:=e			!update link in kcode

	case e^.nameid
	when procid then
		if e^.isglobal then e^.namecat:=globalproc_cat fi
	esac


end

global function finddupl(ref strec d, pdupl)ref strec=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

if pdupl^.nameid<>nullid then		!assume already resolved
	return pdupl
fi
pdupl:=pdupl^.nextdupl

while pdupl do
	if pdupl^.owner=d then
		return pdupl
	fi
	pdupl:=pdupl^.nextdupl
od
return nil
end

proc resolvedot(ref strec owner,unit p)=
var unit lhs,rhs
var ref strec d,e,t
var int m

lhs:=p^.a
rhs:=p^.b
e:=rhs^.def				!p.b will be a name type (could perhaps be stored as p^.def)

rx_unit(owner,lhs)

case lhs^.tag
when j_name then
	d:=lhs^.def
	case d^.nameid
	when moduleid,typeid,procid,typeid,dllmoduleid then
		e:=finddupl(d,e)
		if e then
			p^.tag:=j_name			!convert to dot to name
			p^.a:=p^.b:=nil
			p^.def:=e
			case e^.nameid
			when enumid then
			when constid then
			when macroid then
				if e^.nameid=macroid and not noexpand then
					++macrolevels
					expandmacro(p,p,nil)
					rx_unit(owner,p)
					--macrolevels
				fi
			esac
		else
			rxerror_s("Can't resolve .#",p^.b^.def^.name,p)
		fi

	when frameid, staticid, paramid then		!.x applied to normal var
		m:=d^.mode
		case ttbasetype[m]
		when tfixedrecord,tflexclass then
		when tref then
			do
				m:=tttarget[m]
				case ttbasetype[m]
				when tfixedrecord,tflexclass then
					exit
				when tref then
				else
					rxerror("2:record expected")
				esac
			od
		else
			rxerror("record expected")
		esac
		t:=ttnamedef[m]

		e:=finddupl(t,e)
		if e then
			p^.b^.def:=e
		else
			rxerror_s("Not a field: #",rhs^.def^.name)
		fi
	esac
!when j_ptr then

else
!Can't fully resolve at this time; leave to next pass
	unless e^.nextdupl then
		rxerror_s("Not a field: #",e^.name)
	endunless
esac
end

proc fixmode(ref strec owner, p)=
var ref strec d,e
var int m

m:=p^.mode

if m>=0 then return fi
m:=-m

if ttxmap[m] then				!already fixed
	p^.mode:=ttxmap[m]
	return
fi

if ttnamedefx2[m] then
	rxerror("Can't resolve a:b tentative types yet")
fi

d:=ttnamedefx[m]
e:=resolvetopname(owner,d,ttxmoduleno[m],0)

if e then
	ttxmap[m]:=e^.mode
	p^.mode:=e^.mode

else
MLINENO:=TTLINENOX[M]+TTXMODULENO[M]<<24

	rxerror_s("Can't resolve tentative type: #",d^.name)
fi

end

function fixmode2(ref strec owner, int m)int=
!if m is a userx type, fix it up and return fixed up mode
!otherwise just return m
var ref strec d,e
var [256]char str

if m>=0 then return m fi
m:=-m

if ttxmap[m] then				!already fixed
	return ttxmap[m]
fi

if ttnamedefx2[m] then
	rxerror("2:Can't resolve a:b tentative types yet")
fi

d:=ttnamedefx[m]

IF OWNER=NIL THEN
	CPL D^.NAME
	RXERROR("FIXMODE2 OWNER=0")
FI

e:=resolvetopname(owner,d,ttxmoduleno[m],0)

if e then
	ttxmap[m]:=e^.mode
	return e^.mode

else
	fprint @&.str,"# in module #, line:#",d^.name,moduletable[ttxmoduleno[m]].name,ttlinenox[m]

	MLINENO:=TTLINENOX[M]+TTXMODULENO[M]<<24

	rxerror_s("2:Can't resolve tentative type: #",&.str)
fi
return 0
end

global proc fixusertypes=
var ref userxrec p
var ref int pmode
var int m, rescan,i

for i:=1 to 2 do
	p:=userxmodelist
	rescan:=0

	while p do
		m:=p^.pmode^
		if m<0 then
			m:=fixmode2(p^.owner,m)
			if m<0 and i=2 and ttxmap[abs m] then
				m:=ttxmap[abs m]
			fi
			if m<0 then
				rescan:=1
			else
				p^.pmode^:=m
IF TTTARGET[M]=M THEN
	CPL =TTNAME[M]
	RXERROR("RECURSIVE TYPE?")
FI
			fi
		fi

		p:=p^.nextmode
	od
	if not rescan then exit fi

od
if rescan then
	RXERROR("FIXUSERTYPES PHASE ERROR")
fi

end

proc rx_assem(ref strec owner, unit p,a,b)=
!.a is just a const string containing the asm line, but with "#" inserted where
! resolved name is to go
!.b is a list of j_name units, each one pointing to a generic strec entry
! corresponding to the next # in the assem string
!First, resolve names
!Second, replace each # by resolved name

var unit q
var ref strec d
var ref char s,pdest
var [512]char str
var ref strbuffer expr
var int c

q:=b
while q do					!resolve name list
	if q^.tag=j_name then
		resolvename(owner, q)
	fi
	q:=q^.nextunit
od

pdest:=&.str

s:=a^.svalue
q:=b

while c:=s++^ do
	if c='#' then

		case q^.tag
		when j_name then

			d:=q^.def
			case d^.nameid
			when constid then
				tx_namedconst(d)
!				addint(pdest,d^.code^.value)
!			expr:=strexpr(d^.code)
!			addstr(pdest,expr^.strptr)
			when frameid, paramid then
!				addstr(pdest,"Aframe+")
!				addstr(pdest,getfullname(d,1))
			else
!				addstr(pdest,getfullname(d))
			esac
		when j_const then
!			if gettypecode_t(q^.mode)<>'I' then rxerror("assem/macro/not int") fi
			if not ttisint[q^.mode]<>'I' then rxerror("assem/macro/not int") fi
!			addint(pdest,q^.value)
		else
			rxerror("assem/macro/arg?")
		esac
		q:=q^.nextunit
	else
!		addchar(pdest,c)
	fi
od
pdest^:=0
a^.svalue:=pcm_copyheapstring(&.str)
a^.slength:=strlen(&.str)
end

global function resolve_equiv_name(ref strec owner,p)ref strec=
!@p or @p+offset used for a field offset
!owner is record type of which it should be a member
!currently, p is at strec that might be null
!return matching fieldid name
if p^.nameid=fieldid then
	return p
fi

RXERROR("RESOLVE EQUIV FIELD/COMPLEX")

return nil
end

function addframevar(ref strec owner, d, int moduleno, mode)ref strec=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
	var ref strec e
	e:=getduplnameptr(owner,d,frameid)
	storemode(1,owner,mode,&e^.mode)
	adddef(owner,e)
	return e
end

proc converteqeq(ref strec owner,ref unitrec p)=
!detect exprs such as a=b=c and convert to a=b and b=c
var int leftop,rightop
var ref unitrec w,y1,y2,z

w:=p^.a				!w is the x=y branch
y1:=w^.b				!split y into two
y2:=duplunit(y1)

z:=p^.b

leftop:=w^.tag
rightop:=p^.tag
p^.tag:=j_andl
p^.b:=createunit2(rightop,y2,z)
p^.b^.lineno:=p^.lineno

rx_unitlist(owner,w)
rx_unitlist(owner,y2)
rx_unitlist(owner,z)
end

function copylistunit(unit p)unit=
var unit q

var unit plist,plistx
plist:=plistx:=nil
while p do
	q:=copyunit(p)
	addlistunit(&plist,&plistx,q)
	p:=p^.nextunit
od
return plist
end

function copyunit(unit p)unit=
var unit q
var ref strec d

if p=nil then return nil fi

!need to quickly check if a name unit is a macroparam

if p^.tag=j_name then
	d:=p^.def
	for i to nmacroparams do
		if macroparamsgen[i]=d then
			return copyunit(macroargs[i])
			exit
		fi
	od
fi

q:=createunit0(p^.tag)

q^.a:=copylistunit(p^.a)
q^.b:=copylistunit(p^.b)
q^.c:=copylistunit(p^.c)
q^.lineno:=p^.lineno
q^.value:=p^.value			!copy main field of each union
q^.opcode:=p^.opcode
q^.mode:=p^.mode
q^.newmode:=p^.newmode
q^.moduleno:=p^.moduleno
q^.isastring:=p^.isastring
q^.nextunit:=nil

q.reginfo:=p.reginfo
!	union
!		var int32 opcode
!		var int32 index
!		var int32 whenlabel		!label no associated with when expr; for recase
!		var int32 trylevel
!		var int32 slength
!		var int32 length			!for makelist
!		var byte dottedname		!for j_name: 1=resolved from fully qualified dotted seq
!		var int32 offset			!for j_dot
!		struct
!			var byte reg			!for assemreg/xreg/mem
!			var byte regix
!			var byte scale
!			var byte prefixmode
!			var byte regsize
!			var byte cond
!			var byte spare2,spare3
!		end
!	end
!


return q
end

proc replaceunit(unit p,q)=
!replace p with q, keeping same address of p, and same next pointer
!original contents discarded
var unit pnext
pnext:=p^.nextunit
p^:=q^
p^.nextunit:=pnext
end

proc expandmacro(unit p, a, b)=
!is is a macro name unit, b is a macro parameter list (rx-processed), which
!can be nil
!p is either the call-unit as this may originally have been, or the same as a::
!M => (NAME)
!M(A,B) => (CALL NAME (A,B))
!Need to replace M or M(A,B) with the duplicated AST from a^.code.
!When a name appears in the AST which is a local macroparam name, then that is
!replaced by the corresponding argument from B;
!The new code replaces P (either CALL or NAME)
!Supplied args may be more than macro takes (error) or may be less (error,
!or allow default arg values to be specified)
var ref strec d,pm
var unit pnew
var int ignoreargs

if macrolevels>10 then
	rxerror("Too many macro levels (recursive macro?)")
fi

d:=a^.def

!First step: get list of macro formal parameters
pm:=d^.paramlist
nmacroparams:=0
while pm do
	if nmacroparams>=maxmacroparams then
		rxerror("macro param overflow")
	fi
	macroparams[++nmacroparams]:=pm
	macroparamsgen[nmacroparams]:=pm^.nulldef
	pm:=pm^.nextparam
od

!now get macro args into a list
nmacroargs:=0

while b do
	if nmacroargs>=maxmacroparams then
		rxerror("macro arg overflow")
	fi
	macroargs[++nmacroargs]:=b
	b:=b^.nextunit
od

if nmacroargs<nmacroparams then
	rxerror("Too few macro args")
fi

ignoreargs:=0
if nmacroargs>0 and nmacroparams=0 then		!ignore extra params
	ignoreargs:=1
	nmacroargs:=nmacroparams:=0

elsif nmacroargs>nmacroparams then
	rxerror("Too many macro args")
fi

pnew:=copyunit(d^.code)

if not ignoreargs then				!normal expansion
	replaceunit(p,pnew)
else								!keep call and paramlist; just replace fn name
	p^.a:=pnew						!with expansion
fi
end

=== mm_type.m 29/35 ===
import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lib
import mm_name
import mm_diags

!* LV=0		Allow normal rvalue
!* LV=1		Must be lvalue
!* LV=2		Is INDEX or DOT operand: block data catagories
!			should be treated as values, with ADDROF inserted, if possible.
!* LV=3		Must lvalue and insert ADDROF

tabledata() [0:]ichar lvnames =
	(no_lv=0,		$),
	(need_lv,		$),
	(addrof_lv,		$),
	(index_lv,		$),
	(indexlv_lv,	$),
end

const maxparams=100
const maxfields=200
var int countedfields
var int inassem

proc tpass(unit p, int t=tany, lv=no_lv)=
var ref strec d
var unit a,b
var int oldmlineno,m,nparams,paramtype,restype

if p=nil then return fi

if lv in [need_lv, addrof_lv, indexlv_lv] and not refunitset[p^.tag] then
	txerror("not allowed as lvalue")
fi

oldmlineno:=mlineno

mlineno:=p^.lineno

a:=p^.a
b:=p^.b

!CPL "TPASS",JTAGNAMES[P.TAG]

switch p^.tag
when j_name then
	tx_name(p,t,lv)
when j_const, j_decimal then

when j_typeconst then
	p^.mode:=ti64

when j_bytesize, j_bitwidth then
	tx_bytesize(p,a)

when j_add, j_sub then
	tx_add(p,a,b)

when j_mul, j_div, j_idiv,j_irem, j_min, j_max then
	tx_mul(p,a,b)

when j_assign,j_deepcopy then
	tx_assign(p,a,b,tvoid)

when j_assignx,j_deepcopyx then
	tx_assign(p,a,b,t)

when j_multexpr then
	while a do
		tpass(a)
		a:=a^.nextunit
	od

when j_atan2, j_fmod then
	tx_atan2(p,a,b)

when j_shl, j_shr then
	tx_shl(p,a,b)

when j_iand, j_ior, j_ixor then
	tx_iand(p,a,b)

when j_eq, j_ne then
	tx_eq(p,a,b)

when j_lt, j_le, j_ge, j_gt then
	tx_lt(p,a,b)

when j_addrof then
	if a^.tag=j_ptr then
		deleteunit(p,a)
		deleteunit(p,p^.a)
		tpass(p,t)
	else
		tpass(a,,need_lv)
		p^.mode:=createrefmode(nil,a^.mode)
	fi

when j_addroffirst then
	tx_addroffirst(p,a,t)

!when j_addrof, j_ptrto then
when j_addto, j_subto, j_multo, j_divto, j_idivto,j_shlto, j_shrto, j_minto, j_maxto,
		j_iremto then
	tx_addto(p,a,b)

when j_iandto, j_iorto, j_ixorto then
	tx_iandto(p,a,b)

when j_if then
	tx_if(p,a,b,p^.c,t,lv)

when j_longif then
	tx_longif(p,a,b,t,lv)

when j_index then
	tx_index(p,a,b,t,lv)

when j_ptr then
	tx_ptr(p,a,t,lv)

when j_callproc, j_callfn, j_callmproc, j_callmfn then
	tx_callproc(p,a,b,t)

when j_dot then
	tx_dot(p,a,b,lv)

when j_sqrt, j_sin, j_cos, j_tan, j_asin, j_acos, j_atan,
	 j_ln, j_lg, j_log, j_exp, j_round, j_floor, j_ceil, j_fract  then
	tx_sqrt(p,a)

when j_sign then
	tx_sign(p,a)

when j_power then
	tx_power(p,a,b)

when j_andl, j_orl then
	tx_andl(p,a,b)

when j_neg, j_abs, j_sqr, j_cube then
	tx_neg(p,a,b)

when j_inot then
	tx_inot(p,a)

when j_notl then
	tx_notl(p,a)

when j_istruel then
	tx_istruel(p,a)

when j_convert then
	tx_convert(p,a,1)

!!when j_autocast then
when j_typepun then
	tx_typepun(p,a)

when j_len then
	tx_len(p,a)

when j_lwb then
	tx_lwb(p,a)

when j_upb then
	tx_upb(p,a)

when j_bounds then
	tx_bounds(p,a)

when j_sliceptr then
	tx_sliceptr(p,a)

when j_preincrx, j_predecrx, j_postincrx, j_postdecrx then
	tx_preincr(p,a,t)

!when j_incr, j_decr then
!	tx_preincr(p,a)
!
when j_makerange then
	tx_makerange(p,a,b)

when j_makeset then
	tx_makeset(p,a,t)

when j_makedict then
	tx_makedict(p,a,t)

when j_swap then
	tx_swap(p,a,b)

when j_select then
	tx_select(p,a,b,p^.c,t,lv)

when j_switch, j_doswitch then
	tx_switch(p,a,b,p^.c,t,lv)

when j_case, j_docase then
	tx_case(p,a,b,p^.c,t,lv)

when j_exprlist then
	tx_exprlist(p,a,t)

!!when j_copymem then
!!when j_clearmem then
!!when j_stack then
!!when j_unstack then
!when j_cvtypename then

when j_dotindex, j_dotslice, j_anddotindex then
	tx_dotindex(p,a,b,lv)

when j_slice then
	tx_slice(p,a,b)

!when j_anddotindex then
!when j_dotslice then
!when j_anddotslice then
when j_minvalue, j_maxvalue then
	tx_minvalue(p,a)

!when j_addto,j_subto then
!when j_multo,j_divto,j_minto,j_maxto then
!when j_shlto, j_shrto then
!when j_iandto, j_iorto, j_ixorto then
!when j_negto, j_absto then
when j_negto, j_absto, j_inotto then
	tx_negto(p,a)

!when j_inotto then

when j_block,j_stmtblock then
	tx_block(p,a,t,lv)

when j_eval then
!	tx_unit(a)
	tpass(a,tany)
	p^.mode:=a^.mode

when j_do then
!	tx_unit(a)
	tpass(a,tvoid)


when j_return then
	tx_return(p,a,t)

when j_print,j_println,j_fprint,j_fprintln then
	tx_unitlist(a)
	tx_unitlist(b)
	tx_unitlist(p^.c)

when j_forup, j_fordown, j_forstep then
	tx_for(a,b,p^.c)

when j_to then
	tpass(a,ti64)
	tpass(b,tvoid)

when j_autocast then
	tpass(a)
	coerceunit(a,t,1)
	deleteunit(p,a)

when j_makelist then
	tx_makelist(p,a,t,lv)

when j_stop then
	tpass(a,ti64)

when j_exit,j_redo, j_restart, j_next then
	tx_exit(p,a)

when j_goto then
	tx_goto(p,a)

when j_labeldef then

when j_while then
	tcond(a)
	tpass(b,tvoid)

when j_repeat then
	tpass(a,tvoid)
	tcond(b)

when j_nogap then

when j_assem then
	if t<>tvoid then
		p^.mode:=t
	fi

	inassem:=1
	tx_unitlist(a)
	tx_unitlist(b)
	tx_unitlist(p.c)
	inassem:=0

when j_assemreg,j_assemxreg then
when j_assemmem then
	tpass(a)

when j_typeof then
	tpass(a)
	if a^.tag=j_typeconst then
		p^.value:=a^.value
	else
		p^.value:=a^.mode
	fi
	p^.tag:=j_typeconst
	p^.mode:=ti64
	p^.a:=nil

when j_typestr then
	tpass(a)
	if a^.tag=j_typeconst then
		m:=a^.value
	else
		tpass(a)
		m:=a^.mode
	fi
	p^.tag:=j_const
	p^.mode:=trefchar
	p^.svalue:=pcm_copyheapstring(strmode(m,0))
	p^.slength:=strlen(p^.svalue)
	p^.isastring:=1
	p^.a:=nil

!when j_whenthen then

when j_convertref then
	tpass(a)

when j_fmtitem then
	tpass(a)
	tpass(b)

when j_readln then
	tpass(a)

when j_read then
	if a then
		tpass(a,tc64)
	fi
	if ttisnumeric[t] then
		t:=stdtypebase[ttbasetype[t]]
	fi
	p.mode:=t

when j_in, j_notin then
	tx_in(p,a,b)

when j_recase then
	tpass(a,ti64)
	if a^.tag<>j_const then
		txerror("recase must be const")
	fi

when j_head, j_tail, j_init, j_last, j_take, j_drop, j_reverse, j_left, j_right,
	 j_convlc, j_convuc, j_flexptr, j_stringz, j_dupl then
	tx_head(p,a,b)

when j_prepend, j_append,j_concat then
	tx_concat(p,a,b)

when j_cvlineno then
	p^.mode:=ti64
when j_cvfilename,j_cvmodulename then
	p^.mode:=trefchar

when j_bitfield then
	tx_bitfield(p,a,lv)

when j_emitc then

when j_syscall then
	restype:=tvoid
	paramtype:=tvoid
	case p.opcode
	when sysfn_get_nprocs then restype:=ti64
	when sysfn_get_nexports then restype:=ti64
	when sysfn_get_procname then paramtype:=ti64; restype:=trefchar; 
	when sysfn_get_procaddr then paramtype:=ti64; restype:=tref; 
	when sysfn_get_procexport then paramtype:=ti64; restype:=tref; 
	esac

	if paramtype<>tvoid then
		if a=nil then txerror("sys: arg missing") fi
		tpass(a,paramtype)
		if a.nextunit then txerror("sys: too many args") fi
	elsif a then txerror("sys: too many args")
	fi

	p.mode:=restype

else
CPL "TXUNIT: CAN'T DO:",jtagnames[p^.tag]
doelse::
	tx_unitlist(a,t)
	tx_unitlist(b,t)
	tx_unitlist(p^.c,t)
endswitch

tevaluate(p)

case p^.tag
when j_makelist, j_return then
else
!if p^.tag<>j_makelist and p^.tag<>jthen
	if t<>tany and t<>tvoid and p^.mode<>t then		!does not already match
		coerceunit(p,t)			!apply soft conversion
	fi
esac
if t=tvoid then
	fixvoidunit(p)
fi

mlineno:=oldmlineno
end

proc tx_block(unit p,a, int t,lv)=
	while a and a^.nextunit do
		tpass(a,tvoid)
		a:=a^.nextunit
	od
	if a then
		tx_unitlist(a,t,lv)
		p^.mode:=(t<>tvoid|a^.mode|tvoid)
	fi
end

global proc tx_typetable=
	var int i,u

	for i:=tuser to ntypes do
		setmodesize(i)
	od
end

proc setmodesize(int m)=
	if ttsize[m] then return fi

	mlineno:=ttlineno[m]
	case ttbasetype[m]
	when tfixedarray then
		setarraysize(m)
	when tfixedrecord then
		setrecordsize(m)
	when tvoid,tproc then
	when tslice then
		setslicesize(m)
	when tflexdict then
	when tflexarray then
	when tflexbits then
	when tauto then
	else
		cpl "SIZE 0:",strmode(m),m,stdtypenames[ttbasetype[m]]
!		txerror("can't set mode size")
CPL"********",ttnamedef[m],ttowner[m],=M
		CPL("Can't set mode size")
	esac
end

proc setarraysize(int m)=
var int lower,length,elemsize,target
var unit pdim,a,b

	if ttsizeset[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		a:=pdim^.a
		b:=pdim^.b
		rx_unit(ttowner[m],pdim)

		case pdim^.tag
		when j_makerange then
			tpass(a)
			tpass(b)
			lower:=getconstint(a)
			length:=getconstint(b)-lower+1
		when j_keyvalue then
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

	ttdimexpr[m]:=nil

	ttlower[m]:=lower
	ttlength[m]:=length

	target:=tttarget[m]
	setmodesize(target)
	elemsize:=ttsize[tttarget[m]]
	ttsize[m]:=length*elemsize
	ttsizeset[m]:=1
end

proc setslicesize(int m)=
var unit pdim

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

proc tcond(unit p)=
var unit a,b

a:=p^.a
b:=p^.b

case p^.tag
when j_andl, j_orl, j_xorl then
	tcond(a)
	tcond(b)
when j_notl then
	tcond(a)
	if a^.tag=j_notl then
		deleteunit(p,a)
		p^.tag:=j_istruel
		if boolunitset[p^.a^.tag] then
			deleteunit(p,p^.a)
		fi
	fi

when j_istruel then
	tpass(a)
	if boolunitset[a^.tag] then
		deleteunit(p,a)
	fi
else
	tpass(p)
	twidenopnd(p)
	if not boolunitset[p^.tag] then
		insertunit(p,j_istruel)
	fi
esac


p^.mode:=ti64
end

global function tx_module(int n)int=
var modulerec m
var ref strec stmodule, d
var int globalflag,status

currmoduleno:=n

tx_passdef(moduletable[n].stmodule)

return 1
end

global proc tx_passdef(ref strec p)=
var ref strec d
var int oldmlineno,simplefunc
var unit q

if p^.txdone then
	return
fi

oldmlineno:=mlineno
mlineno:=p^.lineno
simplefunc:=1

d:=p^.deflist
while d do
	tx_passdef(d)

	if p^.nameid in [procid,frameid] and d^.nameid=paramid then
		unless issimpletype(d^.mode) then
			simplefunc:=0
		end
	fi

	d:=d^.nextdef
od

q:=p^.code

case p^.nameid
when procid then
	currproc:=p
    tpass(q,(currproc^.nretvalues>1|tmult|p^.mode))

!see if simple function
	if p^.nretvalues>1 or not issimpletype(p^.mode) then
		simplefunc:=0
	fi
	p^.simplefunc:=simplefunc

	currproc:=nil
when constid,enumid then
	tx_namedconst(p)
when staticid, frameid, paramid then
	tx_namedef(p)
!when TYPEID THEN
esac
p^.txdone:=1
mlineno:=oldmlineno
end

proc tx_unitlist(unit p, int t=tany, lv=no_lv)=
	while p do
		tpass(p,t)
		p:=p^.nextunit
	od
end

proc tx_namedef(ref strec d)=
var int m
var unit dcode

m:=d^.mode
!setarraysize(m)
setmodesize(m)

if d^.circflag then
	txerror("Circular reference detected")
fi
if d^.txdone then return fi
dcode:=d^.code

d^.circflag:=1

if d^.at=1 then
	tpass(d^.equivvar,tref)
fi

if dcode and d^.nameid<>frameid then
	if ttbasetype[m]=tslice and dcode^.tag=j_const and dcode^.mode=trefchar then
		tpass(dcode,trefchar)
	else
		tpass(dcode,m)
	fi
	d^.circflag:=0
	d^.txdone:=1
	if d^.nameid=staticid then
		checkconstexpr(d^.code)
	fi

	if ttbasetype[m]=tfixedarray and ttlength[m]=0 then
		d^.mode:=dcode^.mode
	fi
else
	d^.circflag:=0
	d^.txdone:=1
fi
end

global proc tx_namedconst(ref strec d)=
var int m

if d^.circflag then
	txerror("Circular const reference detected")
fi

var unit q
if d^.txdone then return fi
q:=d^.code

m:=d^.mode

d^.circflag:=1
tx_expr(q,(m=tauto|tany|m))

d^.circflag:=0
checkconstexpr(q)
if m=tauto then
	d^.mode:=q^.mode
fi

d^.txdone:=1
end

proc tx_expr(unit p, int t=tany)=
tpass(p,t)
end

proc checkconstexpr(unit p)=
!check whether p is const expr
var unit q
var int pmode

case p^.tag
when j_const then
	return
when j_makelist then
	q:=p^.a
	while q do
		checkconstexpr(q)
		q:=q^.nextunit
	od

when j_convertref then
	if tttarget[p^.a^.mode]=tvoid then
		p^.a^.mode:=p^.mode
		deleteunit(p,p^.a)
	else
		goto error
	fi

when j_convert then
!	if p^.opcode<>c_soft then
	case p^.opcode
	when c_soft,c_none,c_ichartostring then
	else
		goto error
	esac

when j_addrof then
	case p^.a^.tag
	when j_name then
	else
		goto error
	esac

else
error::
	println jtagnames[p^.tag],STRMODE(P^.MODE)
PRINTUNIT(P)
	txerror("Getconstexpr: not const")
esac
end

function getconstint(unit q, int t=tany)int64=
checkconstexpr(q)

case tttypecode[q^.mode]
when 'I','U' then
	if ttsize[q^.mode]=16 then
		GERROR("GETCONSTINT/128")
	fi
	return q^.value
when 'R' then
	return q^.xvalue
else
	cpl strmode(q^.mode)
	txerror("Getconstint: not int32/64")
esac
return 0
end

proc tevaluate(unit p)=
var unit a,b
var int ischar
var int tag:=p^.tag

if binopset[tag] then
	tevalbinop(p)

elsif monopset[tag] then
	tevalmonop(p)

elsecase tag
when j_makerange then
	a:=p^.a
	b:=p^.b
	if ttsize[a^.mode]<=8 then			!const range only for 32-bits
		tevaluate(a)
		tevaluate(b)
		if a^.tag=j_const and b^.tag=j_const then
			p^.isconst:=a^.isconst iand b^.isconst
		fi
	fi

when j_convert then
	tevalconvert(p)
fi

end

proc tevalbinop(unit p)=
var int64 a,b,c
var real x,y,z

if p^.a^.tag<>j_const or p^.b^.tag<>j_const then
	return
fi

if ttsize[p^.mode]>8 then return fi

case tttypecode[p^.mode]
when 'I' then
	a:=p^.a^.value
	b:=p^.b^.value

	switch p^.tag
	when j_add then

		c:=a+b
	when j_sub then
		c:=a-b
	when j_mul then
		c:=a*b
	when j_div,j_idiv then
		if b=0 then txerror("div by 0") fi
		c:=a/b

	when j_irem then
		if b=0 then txerror("div by 0") fi
		c:=a rem b

	when j_shl then
		c:=a<<b

	when j_shr then
		c:=a>>b

	when j_iand then
		c:=a iand b

	when j_ior then
		c:=a ior b

	when j_ixor then
		c:=a ixor b

	else
		return
	end
	makenewconst(p,c)

when 'R' then
	x:=p^.a^.xvalue
	y:=p^.b^.xvalue

	switch p^.tag
	when j_add then
		z:=x+y
	when j_sub then
		z:=x-y
	when j_mul then
		z:=x*y
	when j_div then
		if y=0 then txerror("div by 0") fi
		z:=x/y

	else
		return
	end
	makenewconst(p,int64@(z))
esac

end

proc tevalmonop(unit p)=
var unit a
var int64 ix,iy,iz
var real x,z

a:=p^.a

if ttsize[p^.mode]>8 then return fi

if a^.tag<>j_const then
	case p^.tag
	when j_bytesize then
		if a^.tag=j_typeconst then
			makenewconst(p,ttsize[a^.value])
		else
!CPL =STRMODE(A.MODE),TTSIZE[TREF]
			makenewconst(p,ttsize[a^.mode])
		fi
	when j_bitwidth then
		if a^.tag=j_typeconst then
			makenewconst(p,ttbitwidth[a^.value])
		else
			makenewconst(p,ttbitwidth[a^.mode])
		fi
	esac

	return
fi

case tttypecode[p^.mode]
when 'I','U' then

	ix:=a^.value

	switch p^.tag
	when j_neg then
		iz:=-ix

	when j_inot then
		iz:=inot ix

	when j_notl then
		iz:=not ix

	when j_abs then
		iz:=abs ix

	when j_bytesize then
		iz:=ttsize[p^.mode]
	else
		return
	end

	makenewconst(p,iz)

when 'R' then
	x:=a^.xvalue
	switch p^.tag
	when j_neg then
		z:=-x
	else
		return
	end
	makenewconst(p,int64@(z))

esac
end

proc tevalconvert(unit p)=
var unit q
var int64 a,b
var int32 a32
var word64 u
var real64 x
var real32 x32
var int s,t
var ref void pvoid

q:=p^.a

tevaluate(q)

s:=q^.mode
case p^.opcode
when c_soft then
DOSOFT::
	if not ctarget then
delmode::
		q^.mode:=p^.mode
		deleteunit(p,q)
		return
	elsif p.mode in [ti64,tu64] and q.mode=tc64 then
		goto delmode
	fi

when c_reftoref then
	if not ctarget then
		goto dosoft
	fi
	p^.tag:=j_convertref
	return
when c_inttoref,c_reftoint then
	goto dosoft

esac

if q^.tag<>j_const then
	return
fi

if s=trefchar then		!not much that can be done with strings
	return
fi

!assume numeric conversion of numeric constants
t:=ttbasetype[p^.newmode]

if s=t then
	deleteunit(p,q)
fi

x:=q^.xvalue
a:=q^.value
u:=q^.uvalue

case p^.opcode
when c_softtruncate,c_truncate then
	if s=ti128 or s=tu128 then
		a:=q^.qvalue^.lower
	fi
	case t
	when tu8,tc8 then	b:=a iand 255
	when tu16,tc16 then	b:=a iand 65535
	when tu32 then	b:=a iand 0xFFFF'FFFF
	when tu64,ti64,tc64 then	b:=a
	when ti8 then	b:=int64(int8(a iand 255))
	when ti16 then	b:=int64(int16(a iand 65535))
	when ti32 then	b:=int64(int32(a iand 0xFFFF'FFFF))
	else
CPL =STRMODE(S),"=>",STRMODE(T)
		txerror("EVALC/TRUNC")

	esac

	makenewconst(p,b)

when c_iwiden, c_uwiden then
	case t
	when ti128, tu128 then
!		pvoid:=makeqvalue(a,gettypecode_t(s))
		pvoid:=makeqvalue(a,tttypecode[s])
		makenewconst(p,int64@(pvoid))
	else
		makenewconst(p,u)
	esac

when c_ifloat then
	x:=real(a)
	makenewconst(p,int64@(x))
when c_ufloat then
	x:=real(a)
	makenewconst(p,int64@(x))
when c_ifix then
	a:=p^.value:=x
	makenewconst(p,int(x))

when c_ufix then
	TXERROR("UFIX")

when c_fwiden then
	txerror("EVALC/FWIDEN")

when c_fnarrow then
	makenewconst(p,int64@(x),tr32)

when c_narrow then txerror("EVALC/NARROW")
else
!	CPL "OTHER EVALC:",CONVNAMES[P^.OPCODE],P^.A^.VALUE
esac
end

proc makenewconst(unit p,int64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

p^.tag:=j_const
p^.value:=x
p^.a:=nil
p^.b:=nil
p^.isconst:=1
if t<>tvoid then
	p^.mode:=t
fi
end

proc tx_name(unit p,int t,lv)=
var ref strec d
var int oldmlineno
var unit pcode
oldmlineno:=mlineno

d:=p^.def
mlineno:=d^.lineno

switch d^.nameid
when constid,enumid then			!note: currently, rxpass converts names to constants

	if lv then txerror("&const") fi

	tx_namedconst(d)
	pcode:=d^.code

	p^.tag:=j_const
	p^.def:=nil
	p^.a:=nil

    p^.c:=nil

	if pcode^.tag=j_convert then		!assume c_soft
		p^.value:=pcode^.a^.value

	else
		p^.value:=pcode^.value
	fi

	p^.slength:=pcode^.slength
	p^.mode:=d^.mode
	p^.isconst:=1
	p^.isastring:=pcode^.isastring

when staticid,frameid,paramid then

if d^.islet and lv then
	txerror("Can't use 'let' var as lvalue")
fi

!CPL "TX/NAME",=INASSEM

	tx_namedef(d)

	if not inassem then
		p^.mode:=d^.mode
		twiden(p,lv)
	else
		p.mode:=trefchar
	fi

when procid,dllprocid then

	p^.mode:=trefproc	!use generic refproc mode (yields return type of actual proc mode
			!after a call op, or actual refproc in other context. Don't use actual
			!refproc here, to avoid generating thousands of ref proc modes, one
			!for each call, that will never be needed

when labelid,blockid then
	p^.mode:=treflabel

when moduleid then
	txerror_s("Module name can't be used on it's own: #",d^.name)

when fieldid then
	p^.mode:=d^.mode

when typeid then
	p^.tag:=j_typeconst
	p^.value:=d^.mode
	p^.mode:=ti64

when dllvarid then
	if d.code then
		txerror("Can't init dllvar")
	fi
	p.mode:=d.mode

else
	txerror_ss("TNAME? # #",namenames[d^.nameid],d^.name)
endswitch
mlineno:=oldmlineno

end

proc getdominantmode(int tag,s,t, &u,&v)=
var int sbase:=ttbasetype[s],tbase:=ttbasetype[t]

if sbase<=tany and tbase<=tany then
	u:=dominantmode[sbase,tbase]
	if u=tref then			!can't return ref void; choose one
		u:=s
	fi
	v:=u
	return
fi

u:=v:=s

case s
when tvariant then

when tflexstring,tflexarray,tflexbits then
	if isintmode(tbase) and tag=j_mul then
		v:=ti64
	fi
else
	u:=v:=t
	case tbase
	when tvariant,tflexstring,tflexarray,tflexbits then
	else
		u:=v:=0
	esac
esac

end

proc getdominantmodepp(unit p,a,b, int &u,&v)=
var int abase,bbase,amode,bmode,tag

abase:=ttbasetype[amode:=a^.mode]
bbase:=ttbasetype[bmode:=b^.mode]

u:=v:=amode
tag:=p^.tag

if abase=tref then						!special rules for refs
	if bbase=tref then					!ref+ref
		switch tag
		when j_eq,j_ne then
			if tttarget[amode]=tvoid or tttarget[bmode]=tvoid then
				return
			fi
		when j_lt,j_le,j_ge,j_gt, j_if then
		when j_sub then
			p^.tag:=j_subref
		else
			if p^.tag=j_add and amode=trefchar and bmode=trefchar and \
				a^.tag=j_const and b^.tag=j_const and a^.isastring and b^.isastring then
				joinstrings(p,a,b)
				return
			fi

			txerror("ref+ref")
		end switch

		if not comparemodes(amode,bmode) then
			u:=v:=0
		fi

	elsif ttisinteger[bbase] then		!ref+int
		unless tag in [j_add,j_sub] then
			txerror("ref+T")
		end
		p^.tag:=(tag=j_add|j_addoffset|j_suboffset)
		v:=ti64

	elsif bbase=tflexstring and amode=trefchar then
		u:=v:=bmode
	else								!error
		u:=v:=0
	fi

	return

elsif abase<=tany and abase<=tany then
	if ttisshortint[abase] then
		abase:=twidenshort(a)
	fi
	if ttisshortint[bbase] then
		bbase:=twidenshort(b)
	fi

	u:=dominantmode[abase,bbase]
	if u=tref then			!can't return ref void; choose one
		u:=amode
	fi
	v:=u
	return
fi

case amode
when tvariant then

when tflexstring,tflexarray,tflexbits then
	if isintmode(bbase) then
		if tag=j_mul then
			v:=ti64
!		elsif tag=j_addto and amode=tflexstring then
!			v:=ti64
		fi
	fi
else

	u:=v:=bmode
	case bbase
	when tvariant,tflexstring,tflexarray,tflexbits then
	else
		u:=v:=0
	esac
esac
end

proc coerceunit(unit p,int t,hard=0)=
!p's mode is not t; apply coercion
!hard=0: is an implicit conversion
!hard=1: is an explicit conversion
var int s,sbase,tbase,cc,starget,ttarget,result

if t=tvoid then return fi

s:=p^.mode

retry::

if s=t then return fi

if comparemodes(s,t) then return fi

if s=tvoid and t<>tvoid then
CPL "COERCE"; PRINTUNIT(P)

	txerror("Void type not allowed in expr")
fi

sbase:=ttbasetype[s]
tbase:=ttbasetype[t]
result:=t

!CPL "COERCE",STRMODE(S),"=>",STRMODE(T)
!CPL "COERCE2",STRMODE(SBASE),"=>",STRMODE(TBASE)

if sbase>=tany or tbase>=tany then			!at least one not simple scalar
	cc:=0
	case tbase
!	when tfixedarray then
	when tflexstring then
		if p^.mode=trefchar then
!		when trefchar then
			cc:=c_ichartostring
		fi
!	when tflexset then
	when tflexdecimal then
		cc:=c_anytodecimal
	when tvariant then
		cc:=c_anytovariant
	elsecase sbase
	when tfixedarray then
		case tbase
		when tslice then
			twholearrayslice(p,t)
			return
		esac
!	when tflexstring then

!	when tflexset then
	when tflexdecimal then
		cc:=c_decimaltoany
	when tvariant then
		cc:=c_varianttoany
	when tmult then
		if p^.tag=j_callfn and p^.a^.tag=j_name then
			s:=p^.a^.def^.mode
			goto retry
		else
			txerror("coerce/mult")
		fi

	esac

	if cc=0 then				!no conversion found
		if not hard then
			txerror_ss("Explicit cast needed: # => #",strmode(s),strmode2(t))
		else
			cc:=c_hard
		fi
	fi
elsif ttisinteger[sbase] and ttisshortint[tbase] then		!narrow
	cc:=(hard|c_truncate|c_softtruncate)
elsif ttisshortint[sbase] and ttisinteger[tbase] then		!widen
	twidenopnd(p)
!	if ttsize[t]=16 then
		p^.mode:=p^.newmode:=t
!	else
!		p.mode:=t
!	fi
	tevalconvert(p)
	return
elsif ttisshortint[sbase] and ttisreal[tbase] then			!short to float
	cc:=(ttisint[sbase]|c_ifloat|c_ufloat)
	goto gotcc
elsif ttisreal[sbase] and ttisshortint[tbase] then			!float to short
	cc:=(ttisint[tbase]|c_ifix|c_ufix)

else									!both under tany so scalars
	cc:=conversionops[sbase,tbase]

gotcc::
	case cc
	when c_error then
		txerror_ss("Conversion not allowed: # => #",strmode(s),strmode2(t))
	when c_none then
!		if ctarget then
!			insertunit(p,j_convert)
!			p^.opcode:=c_soft
!			p^.newmode:=t
!			p^.mode:=result
!			return
!		fi
	when c_reftoref then
		starget:=tttarget[s]
		ttarget:=tttarget[t]
		if starget=ttarget then
			return
		elsif starget=tvoid or ttarget=tvoid then
			if ctarget and starget<>tvoid then		!for C on lvalue, keep cast on ref void
				cc:=c_soft
			fi
		elsif not hard then
			if not comparemodes(s,t) then
				if ctarget and starget=tc8 and ttarget=ti8 then
					cc:=c_soft
				elsif ctarget and starget=ti8 and ttarget=tc8 then
					cc:=c_soft
				else
					cpl Strmode(s),"||",Strmode2(t)
					txerror("ref->ref needs explicit cast")
				fi
			fi
		fi
	when c_inttoref, c_reftoint then
		if not hard then
			txerror_ss("ref<=>int need explicit cast: # => #",strmode(s),strmode2(t))
		fi
	esac
fi

if cc=0 then
CPL "COERCEUNIT",STRMODE(S),STRMODE(T),HARD
PRINTUNIT(P)
	TXERROR("CONV CODE=0")
fi

insertunit(p,j_convert)
if cc=c_softtruncate and hard then
	cc:=c_truncate
fi
p^.opcode:=cc
p^.newmode:=t
p^.mode:=result
tevalconvert(p)
end

proc tx_add(unit p,a,b)=
!add/sub
var int u,v

tpass(a)
tpass(b)

getdominantmodepp(p,a,b,u,v)

if p^.tag=j_const then			!assume str+str => str
	return
fi

coerceunit(a,u)
coerceunit(b,v)
p^.mode:=u


if u=0 then					!probably not numeric
	txerror("add/no dom",p)
fi

coerceunit(a,u)
coerceunit(b,v)
p^.mode:=u

if p^.tag=j_subref and ttisref[v] then
	p^.mode:=ti64
fi

if ttbasetype[u]=tref then
	if not ctarget and b^.tag=j_const and ttbasetype[b^.mode]<>tref then			!scale offset and keep as normal add (won't be sub)
		b^.value*:=ttsize[tttarget[a^.mode]]
		case p^.tag					!lose the add/subptr as no special scaling needed
		when j_addoffset then p^.tag:=j_add
		when j_suboffset then p^.tag:=j_sub
		esac
	fi
fi

end

proc tx_mul(unit p,a,b)=
!mul/div/rem/min/max
var int u,v
tpass(a)
tpass(b)

!getdominantmode(p^.tag,a^.mode,b^.mode,u,v)
getdominantmodepp(p,a,b,u,v)
if u=tvoid then
	txerror("Bad mul/div/rem types",p)
fi

coerceunit(a,u)
coerceunit(b,v)
p^.mode:=u

if stdtypecode[u]<>'R' and p^.tag=j_div then
	p^.tag:=j_idiv
fi

end

proc tx_shl(unit p,a,b)=
!shl/shr
tpass(a)
tpass(b)

twidenopnd(a)

unless isintmode(a^.mode) then
	txerror("SHL/not int")
endunless

coerceunit(b,ti64)
p^.mode:=a^.mode
end

proc tx_iand(unit p,a,b)=
!iand/ior/ixor
var int u,v

tpass(a)
tpass(b)

getdominantmodepp(p,a,b,u,v)

unless ttisinteger[u] then
	txerror("IAND/not int")
endunless

coerceunit(a,u)
coerceunit(b,u)
p^.mode:=u
end

proc tx_eq(unit p,a,b)=
var int abase,bbase,atype,btype,u,v

tpass(a)
tpass(b)

if ttisref[a.mode] and ttisref[b.mode] and
	(tttarget[a.mode]=tvoid or tttarget[b.mode]=tvoid) then
else
	getdominantmodepp(p,a,b,u,v)
	if u=0 then
		txerror("EQ/NE")
	fi

	coerceunit(a,u)
	coerceunit(b,u)
fi

p^.mode:=ti64
end

proc tx_lt(unit p,a,b)=
var int amode,abase,bmode,bbase,u,v

tpass(a)
tpass(b)

getdominantmodepp(p,a,b,u,v)
if u=0 then
	txerror("lt/le/ge/gt")
fi

coerceunit(a,u)
coerceunit(b,u)

p^.mode:=ti64
end

proc tx_callproc (unit p,a,pargs,int t)=
!deal with both callproc and callfn (perhaps calldll too)
var unit q
var ref strec d,e,pm
var [maxparams]ref strec paramlist
var [maxparams]unit arglist,newarglist
var int nparams,i,j,k,nargs,m,kwdused,qm
var ichar name

tpass(a)

nargs:=nparams:=0

case a^.tag
when j_name then
	d:=a^.def

getparams::
	e:=d^.deflist
	while e do
		if e^.nameid=paramid then
			if nparams>=maxparams then txerror("Param overflow") fi
			paramlist[++nparams]:=e
		fi
		e:=e^.nextdef
	od

else
	d:=ttnamedef[a^.mode]
	goto getparams
esac

q:=pargs
while q do
	if nargs>=maxparams then txerror("Param overflow") fi
	arglist[++nargs]:=q
	q:=q^.nextunit
OD

p^.mode:=d^.mode				!type returned by function (will be void for procs)
if d^.nretvalues>1 then
	p^.mode:=tmult
fi

if p^.mode=tvoid and p^.tag=j_callfn then
	p^.tag:=j_callproc
fi

if p^.mode then
	twiden(p,no_lv)
fi

if d^.varparams then
	for i to nargs do

		if i<=nparams then
			tpass(arglist[i],paramlist[i]^.mode)
		else
			tpass(arglist[i])
		fi
		if targetbits=64 then
			twidenopnd(arglist[i])
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
	switch q^.tag
	when j_keyword then
		name:=q^.a^.def^.name
		for j to nparams do
			if eqstring(paramlist[j]^.name,name) then
				exit
			fi
		else
			txerror_s("Can't find kwd param: #",name)
		od

		if newarglist[j] then
			txerror_s("Kwd: # already used or was implicit",name)
		fi
		newarglist[j]:=q^.b
		kwdused:=1

	when j_null then			!missing param
		if kwdused then
			txerror("Normal param follows kwd")
		fi
		q:=nil
		goto doregparam
	else
doregparam::
		if kwdused then
			txerror("Normal param follows kwd")
		fi
		if k>=nparams then
			cpl =k, =nparams
			txerror("Too many params supplied")
		fi
		newarglist[++k]:=q
	endswitch
od

!scan params, and fill in optional/default params as needed
!!params[i>naparams] might be void if there were fewer actual parameters

for i to nparams do
	q:=newarglist[i]			!will be nil of not supplied
	pm:=paramlist[i]			!formal param (an st entry)
	if q=nil then
		unless pm^.optional then
			txerror_s("Param not optional: %lld",ref void(i))
		end
		if pm^.code then		!provide default value
			newarglist[i]:=duplunit(pm^.code,p^.lineno)
		else
			newarglist[i]:=createconstunit(0,ti64)
		fi
	fi
od

!final pass: do type-pass on each param, and apply any conversion
!I also need to build a new argument list for the call unit
var unit ulist:=nil, ulistx

for i to nparams do
	pm:=paramlist[i]
	q:=newarglist[i]

	if pm^.parammode=out_param then
		tpass(q,,need_lv)
		m:=tttarget[pm^.mode]
		qm:=q^.mode

		if not comparemodes(qm,m) then
			txerror("&param: type mismatch")
		fi

		insertunit(q,j_addrof)
		q^.mode:=createrefmode(nil,qm)

	else
		tpass(q,pm^.mode)
		if targetbits=64 then
			twidenopnd(q)
		fi
	fi

	if ulist=nil then
		ulist:=q
	else
		ulistx^.nextunit:=q
	fi
	ulistx:=q
	q^.nextunit:=nil
od
p^.b:=ulist
end

proc tx_neg(unit p,a,b)=
var int u

tpass(a)
!if not (ttflags[ttbasetype[a^.mode]] iand m_numeric) then
if not isnumericmode(a^.mode) then
	txerror("Neg: not numeric",a)
fi
twidenopnd(a)

u:=a^.mode
coerceunit(a,u)
p^.mode:=u
end

proc tx_if (unit p,a,b,c,int t,lv) =
	var int u,v

	tcond(a)

	tpass(b,t,lv)
	if t<>tvoid and not c then
		txerror("if needs else")
	fi
	tpass(c,t,lv)

	if t=tany then			!unknown types (eg. print)
		getdominantmodepp(p, b,c,u,v)
		coerceunit(b,u)
		coerceunit(c,u)
		p^.mode:=u
	else				!know exactly what type needed
		p^.mode:=t
	fi
end

proc tx_longif (unit p,a,b,int t,lv) =
	var unit q,r
	var int u,v

	u:=tvoid

	q:=a
	while q do				!all elseif unots
		tcond(q^.a)
		r:=q^.b
		tpass(r,t,lv)

		if t=tany then
			if u=tvoid then
				u:=r^.mode
			else
				getdominantmode(0,u,r^.mode,u,v)
			fi
		fi

		q:=q^.nextunit
	od

	if t<>tvoid and b=nil then
		txerror("longif needs else")
	fi
	tpass(b,t,lv)

	if t=tany then
		getdominantmode(0,u,b^.mode,u,v)
	fi

	if t<>tvoid then
		q:=a
		while q do				!all elseif unots
			if t=tany then
				coerceunit(q^.b,u)
			fi
			q^.mode:=q^.b^.mode
			q:=q^.nextunit
		od
		if t=tany then
			coerceunit(b,u)
		fi
		p^.mode:=b^.mode
	fi
end

proc tx_preincr(unit p,a,int t)=
tpass(a,,need_lv)
unless ttisinteger[a^.mode] or ttisref[a^.mode] then
	txerror("incr: not int/ref")
end

if t=tvoid then
	case p^.tag
	when j_preincrx, j_postincrx then p^.tag:=j_incr
	when j_predecrx, j_postdecrx then p^.tag:=j_decr
	esac

else
	p^.mode:=a^.mode	!only meaningful for -x versions
fi
if t<>tvoid then
	twiden(p,0)
fi
end

proc tx_for(unit a,pbody,c)=
!Need to allow for unsigned 
var unit pfrom, pto, pstep
var int u

pfrom:=a^.nextunit
pto:=pfrom^.nextunit
pstep:=pto^.nextunit

tpass(a)					!needs to be i32

u:=a^.mode

tpass(pfrom,u)
tpass(pto,u)
tpass(pstep,u)

tpass(pbody,tvoid)
tpass(pbody^.nextunit,tvoid)	!optional else
end

proc tx_index(unit p,a,b,int t,lv) =
!p is an index unit
!a is an array, b is an index
!t is the needed type for the element being indexed
var int amode,emode,pmode,abasemode,tmode,tbasemode

tpass(a,,lv)

tpass(b)			!index

amode:=a^.mode

case ttbasetype[b^.mode]
when trange64 then
	TXERROR("CAN'T DO SLICING YET")
else					!assume simple index
	coerceunit(b,ti64)
esac

!assume that rmode is an int, and qmode is not a var
abasemode:=ttbasetype[amode]

while abasemode=tref do
	tmode:=tttarget[amode]
	insertunit(a,j_ptr)
	amode:=a^.mode:=tmode
	abasemode:=ttbasetype[amode]
od

if stdtypecode[abasemode]<>'A' then
	txerror_s("Can't index: #",strmode(amode))
fi
case abasemode
when tvariant then
	emode:=tvariant
when tflexstring then
	emode:=tflexstring
else
	emode:=tttarget[amode]
esac

p^.mode:=emode
twiden(p,lv)
end

proc tx_makerange(unit p,a,b)=
var int u,v, amode,bmode

tpass(a)
tpass(b)

amode:=a^.mode
bmode:=b^.mode

if not ttisinteger[amode] or not ttisinteger[bmode] then
	txerror("range not int")
fi
if tttypecode[amode]<>tttypecode[bmode] then
	txerror("range: mixed i64/u64")
fi
getdominantmodepp(p,a,b,u,v)

if ttisint[amode] then
	coerceunit(a,ti64)
	coerceunit(b,ti64)
else
	coerceunit(a,tu64)
	coerceunit(b,tu64)
fi
p^.mode:=trange64
end

proc tx_makeset(unit p,a, int t)=
var int x,y,isconst
var int64 lower,upper
var ref void pvoid

if t=tvoid then
	txerror("open(var) set type")
fi

lower:=2 billion
upper:=-2 billion

isconst:=1

while a do
	tpass(a)

	if not a^.isconst then
		isconst:=0
	else
		case a^.tag
		when j_makerange then
			lower min:=a^.a^.value
			upper max:=a^.b^.value
		when j_const then
			coerceunit(a,ti64)
			lower min:=y:=a^.value
			upper max:=y:=a^.value
		esac
	fi
	a:=a^.nextunit
od

p^.isconst:=isconst
p^.range_lower:=lower
p^.range_upper:=upper
!if isconst then
!	p^.mode:=createsetmodek(nil,(p^.length|upper+1|0),0)
!else
	p^.mode:=tflexset
!fi
end

proc tx_makedict(unit p,a, int t)=
var int x,y,isconst,km,vm
var ref void pvoid

if t=tvoid then
	txerror("open(var) dict type")
fi

if ttbasetype[t]<>tflexdict then
	txerror("not dict type")
fi

km:=ttkeymode[t]
vm:=tttarget[t]

isconst:=1
while a do					!a is a keyvalue type
	tpass(a^.a,km)
	tpass(a^.b,vm)

	a:=a^.nextunit
od

p^.isconst:=isconst
p^.mode:=t
end

proc tx_ptr(unit p,a,int t,lv)=
var ref strec d

case p^.tag
when j_name then
	d:=p^.def
	case d^.nameid
	when staticid, frameid, paramid then
	else
		txerror_s("Can't use as ptr: ",d^.name)
	esac
esac
tpass(a)

case ttbasetype[a^.mode]
when tvoid then
	txerror("Deref Void")
when tref then
	p^.mode:=tttarget[a^.mode]
when tslice then
	txerror("Can't deref slice")
when tvariant then
	p^.mode:=tvariant
else
	txerror("PTR: need ref T")
esac
twiden(p,lv)
end

proc setrecordsize(int m)=
	var [maxfields+8]ref strec fieldlist
	var int i,nfields,indent,nrfields,size,index
	var ref strec d,e
	var ref char flags
	const ss='S', ee='E'
	var int flag

	if ttsize[m] then return fi

	d:=ttnamedef[m]
	e:=d^.deflist
	nfields:=0

	fieldlist[++nfields]:=ref strec@(ss)

	while e do
		if nfields>=maxfields then
			gerror("srs:too many fields")
		fi

		setmodesize(e^.mode)
		flags:=cast(&e^.uflags)
		docase flags^
		when 'S', 'U' then
			flag:=flags^
			fieldlist[++nfields]:=ref strec@(flag)
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
				fieldlist[++nfields]:=ref strec@(ee)
			else
				exit
			esac
		od

		e:=e^.nextdef
	od

	fieldlist[++nfields]:=ref strec@(ee)
	fieldlist[nfields+1]:=nil			!terminator

	countedfields:=0
	index:=2
	scanrecord('S',&fieldlist,index,size,0)
	ttsize[m]:=size
	ttlength[m]:=countedfields
	ttlower[m]:=1
end

proc scanrecord(int state,ref[]ref strec fields, int &index, &isize, offset)=
 	var ref strec e,f,ea
	var int size:=0,fieldsize,bitoffset

	while f:=fields^[index++] do
		case int(f)
		when 'S','U' then
			scanrecord(int(f),fields, index,fieldsize, offset)
		when 'E' then			!end of this nested block
			if state='U' then ++countedfields fi
			isize:=size
			return
		else
			if f^.mode=tbitfield then
				fieldsize:=0	
				ea:=f^.equivfield
				f^.offset:=ea^.offset
				f^.bitoffset:=bitoffset
				bitoffset+:=f^.bitfieldwidth
				if bitoffset>ttbitwidth[f^.equivfield^.mode] then
					txerror("Bit fields overflow type")
				fi

			elsif f^.at then
				bitoffset:=0
				e:=f^.equivfield
				fieldsize:=0
				ea:=resolve_equiv_name(f^.owner,e)
				f^.offset:=ea^.offset
			else
				bitoffset:=0
				if state='S' then ++countedfields fi
				fieldsize:=ttsize[f^.mode]
!		if ttisref[f.mode] then
!			fieldsize:=8
!		fi
				f^.offset:=offset
				f^.offset:=offset
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

proc tx_convert(unit p,a,int hard=0)=
if a^.tag=j_makelist then
	tx_makelist(a,a^.a,p^.newmode,no_lv)
else
	tpass(a)
	coerceunit(a,p^.newmode,hard)
fi
deleteunit(p,a)			!get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
	var int alength,tlength,elemtype,newt, i, nfields,isconst
	var unit q,b
	var ref strec e

	alength:=p^.length
	newt:=0
	isconst:=1

	tlength:=ttlength[t]

	if tlength then
		if alength<tlength then
			txerror("Too few elements")
		elsif alength>tlength then
			txerror("Too many elements")
		fi
	fi

	case ttbasetype[t]
	when tfixedarray then
		elemtype:=tttarget[t]
		if tlength=0 then
			newt:=createarraymodek(nil, elemtype, ttlower[t],alength,0)
		else
			newt:=t
		fi
		q:=a
		while q do
			tpass(q,elemtype,lv)
			unless q^.tag=j_const then isconst:=0 end
			q:=q^.nextunit
		od

		p^.mode:=newt

	when tfixedrecord then
		e:=ttnamedef[t]^.deflist
		q:=a
		while q and e do
			while e^.mode=tbitfield do
				e:=e^.nextdef
				if not e then exit fi
			od

			tpass(q,e^.mode,lv)
			unless q^.tag=j_const then isconst:=0 end
			q:=q^.nextunit
			e:=e^.nextdef
		od
		while e and e^.mode=tbitfield do
			e:=e^.nextdef
		od
		if q or e then
			txerror("Can't initialise unions")
		fi
		p^.mode:=t
	when tslice then
		if a=nil or (b:=a^.nextunit; b=nil) or b^.nextunit then
			txerror("bad slice init")
		fi
		tpass(a,,lv)
		if ttbasetype[a^.mode]<>tref then txerror("slice init not ref") fi
		tpass(b,ti64)
		p^.mode:=t

	when tflexarray then
		elemtype:=tttarget[t]
		q:=a
		while q do
			tpass(q,elemtype,lv)
			unless q^.tag=j_const then isconst:=0 end
			q:=q^.nextunit
		od

		p^.mode:=t

	when tvoid,tvariant then
		q:=a
		while q do
			tpass(q,,lv)
			unless q^.tag=j_const then isconst:=0 end
			q:=q^.nextunit
		od

		p^.mode:=tvariant
	else
		txerror_s("Unknown makelist type: #",strmode(t))
	esac

	p^.isconst:=isconst

end

proc tx_dot(unit p,a,b,int lv)=
var int recmode,recbasemode,i,j,newtag,tmode
var unit q,pindex
var ref strec d,dequiv

tpass(a,,lv)			!lhs, yeields ref array type

recmode:=a^.mode

recbasemode:=ttbasetype[recmode]

while recbasemode=tref do
	tmode:=tttarget[recmode]
	insertunit(a,j_ptr)
	recmode:=a^.mode:=tmode
	recbasemode:=ttbasetype[recmode]
od

if ttbasetype[recmode]<>tfixedrecord then
	txerror("Bad record type")
fi

d:=b^.def

if d^.nameid=nullid then			!not resolved; lhs mode wasn't available
	d:=b^.def:=resolvefield(d,recmode)
fi

if d^.mode=tbitfield then
	i:=d^.bitoffset
	j:=i+d^.bitfieldwidth-1
	dequiv:=d^.equivfield
	b^.def:=dequiv				!change from bitfield field to containing int
	b^.mode:=dequiv^.mode
	p^.offset:=d^.offset

	if i=j then					!single bit
		pindex:=createconstunit(i,ti64)
		newtag:=j_dotindex
	else						!bit slice
		pindex:=createunit2(j_makerange,createconstunit(i,ti64),createconstunit(j,ti64))
		pindex^.mode:=trange64
		newtag:=j_dotslice
	fi

	p^.mode:=b^.mode
	twiden(p,lv)
	insertunit(p,newtag)
	p^.mode:=tu64
	p^.b:=pindex

	return

fi

b^.mode:=d^.mode
p^.mode:=d^.mode

if ttisflexvar[d^.mode] then
	CPL "DOT:NEED TO REMOVE ADDR OF"
	removeaddrof(a)
FI

p^.offset:=d^.offset
twiden(p,lv)
end

function resolvefield(ref strec d, int m)ref strec=
	var ref strec e,t

	case ttbasetype[m]
	when tfixedrecord,tflexclass then
	when tref then
		m:=tttarget[m]
		if ttbasetype[m]<>tfixedrecord then
			txerror("3:record expected")
		fi
	else
		txerror("4:record expected")
	esac
	t:=ttnamedef[m]

	e:=finddupl(t,d)
	if not e then
		txerror_s("Not a field: #",d^.name)
	fi
	return e
end

function comparemodes(int s,t)int=
!return 1 if s/t are comparable modes
!a direct compare may be false because refs/arrays but be constructed at
!different times
var int sbase, tbase
var ref strec d,e

if s=t then return 1 fi
sbase:=ttbasetype[s]
tbase:=ttbasetype[t]

if sbase<>tbase then return 0 fi
case sbase
when tref then
	return comparemodes(tttarget[s],tttarget[t])
when tfixedarray then
	if comparemodes(tttarget[s],tttarget[t]) and (ttlength[s]=ttlength[t] or\
		ttlength[s]=0 or ttlength[t]=0) then
		return 1
	fi
when tslice then
	return comparemodes(tttarget[s],tttarget[t])

when tproc then
	d:=ttnamedef[s]
	e:=ttnamedef[t]
	if d and e then
		if not comparemodes(d^.mode,e^.mode) then return 0 fi
		if d^.paramlist=nil and e^.paramlist=nil then return 1 fi
	fi
!else needs complex param/result-matching
!...
esac
return 0
end

function isboolunit(unit p)int=
!check that unit p has an inherent bool result, and return 1 if so, otherwise 0.
!This is done without checking types, so an EQ unit will always be bool
!Used by caller to determine whether an istrue op needs to be inserted

return boolunitset[p^.tag]
end

proc checkbool(int m)=
!check that mode can be converted to bool, ie. can have istrue applied

unless tttypecode[m] then
!unless stdtypeflags[m] iand (m_numeric + m_ref) then
	txerror_s("Can't convert to bool: #",strmode(m))
end unless
end

proc tx_andl(unit p,a,b)=

tpass(a)
tpass(b)
if not isboolunit(a) then insertunit(a,j_istruel) fi
if not isboolunit(b) then insertunit(b,j_istruel) fi

checkbool(a^.mode)
checkbool(b^.mode)
p^.mode:=ti64
end

proc convintconst(unit p,int64 x)=				!CONVINTCONST
!convert unit p into int const x
p^.tag:=j_const
p^.mode:=ti64
p^.value:=x
p^.a:=p^.b:=p^.c:=nil
p^.isconst:=1
end

proc tx_upb(unit p,a)=
var int m

tpass(a)
m:=a^.mode

case ttbasetype[m]
when tfixedarray then
	convintconst(p,ttlower[m]+ttlength[m]-1)
when tslice,tflexarray,tflexstring then
else
	txerror_s("UPB #",strmode(m))
esac
p^.mode:=ti64
end

proc tx_len(unit p,a)=
var int m

tpass(a)
m:=a^.mode

case ttbasetype[m]
when tfixedarray then
	convintconst(p,ttlength[m])
when tslice,tflexarray,tflexstring then
else
	txerror_s("LEN #",strmode(m))
esac
p^.mode:=ti64
end

proc tx_lwb(unit p,a)=
var int m

tpass(a)
m:=a^.mode

case ttbasetype[m]
when tfixedarray then
	convintconst(p,ttlower[m])
when tslice then
	convintconst(p,ttlower[m])
when tflexstring then
	convintconst(p,1)
else
	txerror_s("LWB #",strmode(m))
esac
p^.mode:=ti64
end

proc tx_bounds(unit p,a)=
var int m,lower,upper

tpass(a)
m:=a^.mode

case ttbasetype[m]
when tfixedarray then
	lower:=ttlower[m]
	upper:=lower+ttlength[m]-1
when tslice then
	return
else
	txerror_s("BOUNDS #",strmode(m))
esac

p^.tag:=j_const
p^.mode:=trange64	!createrangemode(currproc,ti64,0)
p^.qvalue:=makeqvalue_ab(lower,upper)
p^.a:=p^.b:=p^.c:=nil
p^.isconst:=1
end

proc tx_sliceptr(unit p,a)=
var int m,tmode

tpass(a)
m:=a^.mode

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

p^.mode:=createrefmode(nil,tmode)
end

proc tx_inot(unit p,a)=
var int u,atype

tpass(a)

unless isintmode(a^.mode) then
	txerror("INOT/not int")
end unless
twidenopnd(a)

u:=ttbasetype[a^.mode]
coerceunit(a,u)
p^.mode:=u
end

proc tx_atan2(unit p,a,b)=
tpass(a,tr64)
tpass(b,tr64)
p^.mode:=tr64
end

proc tx_swap(unit p,a,b)=
tpass(a,,need_lv)
tpass(b,,need_lv)

if not comparemodes(a^.mode,b^.mode) then
	txerror("SWAP: type mismatch")
fi

p^.mode:=tvoid
end

proc tx_sqrt(unit p,a)=
tpass(a)

!unless ttflags[ttbasetype[a^.mode]] iand m_numeric then
unless isnumericmode(a^.mode) then
	txerror("maths: not numeric")
end unless
coerceunit(a,tr64)
p^.mode:=tr64
end

proc tx_select(unit p,a,b,c, int t,lv)=
var int i,u,v
var unit q

tpass(a,ti64)

q:=b
while q do
	tpass(q,t,lv)
	if q=b then
		u:=q^.mode
	else
		getdominantmode(0,u,q^.mode,u,v)
	fi

	q:=q^.nextunit
od

tpass(c,t,lv)
getdominantmode(0,u,c^.mode,u,v)

q:=b
while q do
	coerceunit(q,u)
	q:=q^.nextunit
od

p^.mode:=u
end

proc tx_case(unit p,a,b,c, int t,lv)=
var int amode,u,v
var unit wt,w

if p^.tag=j_docase and lv then gerror("&docase") fi

tpass(a)

if a=nil then
	amode:=tany
else
	amode:=a^.mode
fi

!if ttflags[amode] iand m_int and ttsize[amode]<8 then
if isintmode(amode) and ttsize[amode]<8 then
	coerceunit(a,tint)
	amode:=tint
fi
u:=tvoid

!CPL "CASE"

wt:=b
while wt do				!whenthen chain
	w:=wt^.a
	while w do				!each expr between when...then
		tpass(w)
		if w^.tag=j_makerange then
			unless isintmode(amode) then txerror("case: need int index") end
		else
			coerceunit(w,amode)
			if amode=tany and not isboolunit(w) then
				insertunit(w,j_istruel)
			fi
		fi
		w:=w^.nextunit
	od
	tpass(wt^.b,t,lv)			!process block
	if t<>tvoid then
		if u then
			getdominantmode(0,u,wt^.b^.mode,u,v)
		else
			u:=wt^.b^.mode
		fi
	fi
	wt:=wt^.nextunit
od

if c then
	tpass(c,t,lv)
	if t=tany then
		getdominantmode(0,u,c^.mode,u,v)
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

case a^.tag
when j_notl then
	deleteunit(p,a)
	p^.tag:=j_istruel
esac

if p^.tag=j_istruel and isboolunit(p^.a) then
	deleteunit(p,p^.a)
fi

checkbool(a^.mode)

p^.mode:=ti64
end

proc tx_istruel(unit p,a)=
tpass(a)

if isboolunit(a) then
	deleteunit(p,a)
fi

checkbool(a^.mode)

p^.mode:=ti64
end

proc tx_addto(unit p,a,b)=
var int issub, atype, btype, u

tpass(a,,need_lv)
tpass(b)

issub:=p^.tag=j_sub
atype:=ttbasetype[a^.mode]
btype:=ttbasetype[b^.mode]

if p^.tag=j_divto and stdtypecode[atype]<>'R' then
	p^.tag:=j_idivto
fi

u:=atype
case atype
when tref then
	if btype=tref then
		if not issub then
			txerror("ref+ref")
		fi
		if not comparemodes(a^.mode,b^.mode) then
			txerror("ref-ref bad types")
		fi
		u:=btype

	else
		u:=ti64
	fi
when tflexstring then
	if ttisinteger[btype] then
		u:=ti64
	fi

esac

coerceunit(b,u)

if isrefmode(atype) then
	if not ctarget and b^.tag=j_const and btype<>tref then			!scale offset and keep as normal add (won't be sub)
		b^.value*:=ttsize[tttarget[a^.mode]]
	else							!else convert to addptr/subptr
		p^.tag:=(p^.tag=j_addto|j_addoffsetto|j_suboffsetto)
	fi
fi
p^.mode:=tvoid
end

proc tx_iandto(unit p,a,b)=
tpass(a,,need_lv)
tpass(b,ttbasetype[a^.mode])

unless isintmode(a^.mode) then
	txerror("iand: not int")
end
p^.mode:=tvoid
end

proc tx_negto(unit p,a)=
var int issub, atype, btype, u

tpass(a,,need_lv)

p^.mode:=tvoid
end

proc tx_typepun(unit p,a)=
case a^.tag
when j_makelist then
	TXERROR("TYPEPUN/LIST")
else
	tpass(a)
	p^.mode:=p^.newmode
esac
end

proc tx_bytesize(unit p,a)=
tpass(a)
p^.mode:=ti64
end

proc tx_exit(unit p,a)=
if a=nil then return fi
tpass(a,ti64)
if a^.tag<>j_const then
	txerror("exit/etc not const")
fi
p^.index:=a^.value
p^.a:=nil

end

proc tx_goto(unit p,a)=
var int m

tpass(a)
m:=a^.mode
if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
	txerror("goto: not label")
fi
end

proc tx_switch(unit p,a,b,c,int t,lv)=
var [0:2001]byte valueset
var unit wt, w
var int ax,bx,i,u,v

if p^.tag=j_doswitch and lv then gerror("&doswitch") fi

tpass(a,ti64)

memset(&valueset,0,valueset.bytes)
u:=tvoid

wt:=b
while wt do

	w:=wt^.a
	while w do
		tpass(w)

		if not isconstunit(w) then txerror("Switch not constant") fi

		case ttbasetype[w^.mode]
		when trange64 then			!assume makerange
			ax:=w^.a^.value
			bx:=w^.b^.value
dorange::
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
			if w.tag<>j_const then
				txerror("Switch value: not const int")
			fi
			ax:=bx:=w^.value
			goto dorange
		esac
		w:=w^.nextunit
	od
	tpass(wt^.b,t,lv)

	if t=tany then
		if u then
			getdominantmode(0,u,wt^.b^.mode,u,v)
		else
			u:=wt^.b^.mode
		fi
	fi

	wt:=wt^.nextunit
od

if c then
	tpass(c,t,lv)
	if t=tany then
		getdominantmode(0,u,c^.mode,u,v)
	fi
elsif t<>tvoid then
	txerror("switch needs else")
fi

if t<>tvoid then
	w:=b^.a
	while w do				!all elseif unots
		if t=tany then
			coerceunit(b^.b,u)
		fi
		w^.mode:=b^.b^.mode
		w:=w^.nextunit
	od
	if t=tany then
		coerceunit(c,u)
		p^.mode:=u
	else
		p^.mode:=t
	fi
else
	p^.mode:=tvoid
fi
end

proc tx_power(unit p,a,b)=
var int u,v

tpass(a)
tpass(b)

u:=v:=dominantmode[a^.mode, b^.mode]
coerceunit(a,u)
coerceunit(b,v)
p^.mode:=u
end

proc tx_addroffirst(unit p,a,int t)=
!&.x maps to &x[x.lwb]
	var int m

	tpass(a)
	m:=a^.mode
	if ttbasetype[m]<>tfixedarray then
		txerror("&. ref[] expected")
	fi

	m:=createrefmode(nil,tttarget[m])
	if a^.tag=j_name then
		a^.addroffirst:=1
	fi
	p^.mode:=m
end

proc tx_minvalue(unit p,a)=
!x.minvalue/.maxvalue
var int u,tmax
var int64 x
var ref qint aa

if a^.tag=j_typeconst then
	u:=ttbasetype[a^.value]
dotypeconst::
    tmax:=ti64
    if p^.tag=j_minvalue then
        case u
        when ti8 then x:=-128
        when ti16 then x:=-32768
        when ti32 then x:=-2_147_483_648
        when ti64 then x:=int64.minvalue
        when ti128 then
			aa:=pcm_allocz(qint.bytes)
			aa^.lower:=0
			aa^.upper:=0x8000'0000'0000'0000
			x:=cast(aa)
			tmax:=ti128
        when tu8,tu16,tu32,tu64,tu128,tc8,tc16 then x:=0
        else
            txerror_s("Can't do minvalue on #",strmode(u))
        esac
    else
        case u
        when ti8 then x:=127
        when ti16 then x:=32767
        when ti32 then x:=2_147_483_647
        when ti64 then x:=0x7fff'ffff'ffff'ffff
        when ti128 then
			aa:=pcm_allocz(qint.bytes)
			aa^.lower:=0xFFFF'FFFF'FFFF'FFFF
			aa^.upper:=0x7FFF'FFFF'FFFF'FFFF
			x:=cast(aa)
			tmax:=ti128
        when tu8,tc8 then x:=255
        when tu16,tc16 then x:=65535
        when tu32 then x:=4294967295
        when tu64 then x:=0; --x; tmax:=tu64
        when tu128 then
			aa:=pcm_allocz(qint.bytes)
			aa^.lower:=aa^.upper:=0xFFFF'FFFF'FFFF'FFFF
			x:=cast(aa)
			tmax:=tu128
        else
            txerror_s("Can't do maxvalue on #",strmode(u))
        esac
    fi
    p^.tag:=j_const
    p^.value:=x
    p^.a:=nil
    p^.mode:=tmax
	p^.isconst:=1
else
	tpass(a)
	if a.tag=j_typeconst then
		u:=ttbasetype[a^.value]
	else
		u:=ttbasetype[a^.mode]
	fi
	goto dotypeconst
fi
end

proc tx_return(unit p,a, int t)=
 	var int m,nvalues,nret,i
	var unit q

	m:=currproc^.mode
	nret:=currproc^.nretvalues
	if a=nil then
		if nret then
			txerror("return value(s) missing")
		fi
		return
	elsif nret=0 then
		txerror("Superfluous return value")
	fi

	if a^.tag=j_makelist then
		a^.tag:=j_returnmult
		if a^.length<>nret then
			txerror("Wrong number of return values")
		fi
		q:=a^.a				!point to list of return values
		for i to nret do
			tpass(q,currproc^.modelist[i])
			q:=q^.nextunit
		od

		deleteunit(p,a)			!don't need return
		if t=tvoid then
			p^.mode:=tvoid
		else
			p^.mode:=tmult
		fi

	else
		if nret>1 then txerror("RETERROR?") fi
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			p^.mode:=tvoid
		else
			if not ctarget then
				deleteunit(p,a)
			else
				p.mode:=a.mode
			fi
		fi
	fi
end

proc tx_dotindex(unit p,a,b,int lv) =
!a.[b], a is an int
var int pmode
var unit i,j

tpass(a,,lv)			!lhs

pmode:=tu64

if not isintmode(a^.mode) then

	case a^.mode
	when tflexstring then
		if p^.tag=j_dotslice then
			pmode:=tflexstring
		else
			pmode:=tc64
		fi
	else
		txerror("a.[i]: not int/str value")
	esac
fi

tpass(b)			!index


case ttbasetype[b^.mode]
when trange64 then
	i:=b.a
	j:=b.b
	if i.tag=j.tag=j_const then
		if i.value>j.value then
			swap(b^.a,b^.b)
		fi
	fi
else					!assume simple index
	coerceunit(b,ti64)
esac

p^.mode:=pmode
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange
var int itypeno

tpass(a)			!lhs
tpass(b)			!will be a range

case ttbasetype[a^.mode]
when tfixedarray then
	tarrayslice(p,a,b)

when tflexarray,tflexbits, tflexstring then
	p^.mode:=a^.mode
when tslice then
	txerror("slice of slice not implemented")

else
	txerror("a[i..j]: not array")
esac
end

proc tx_assign(unit p,a,b,int t)=
var int m

case a^.tag
when j_makelist then
	tx_multassign(a,b)
when j_dotindex, j_dotslice then
	tx_dotindex(a,a^.a,a^.b,need_lv)
	tpass(b,ti64)
else
	tpass(a,,need_lv)
	m:=a^.mode

	if gettypecat_t(m)=tshort then
		tpass(b,stdtypebase[ttbasetype[m]])
		p^.mode:=m

		if t<>tvoid then
			twidenopnd(p)
		fi
	else
		tpass(b,m)
		p^.mode:=m
	fi
esac
end

proc tx_multassign(unit a,b)=
!a is a multexpr; b might be multexpr, or a function with multiple returns
var unit p,q,lhs,rhs
var int nretmodes,i
var ref strec d				!point to def containing return mode info

nretmodes:=0

if b^.tag<>j_makelist then

	tpass(b)
	d:=getprocretmodes(b)
	nretmodes:=d^.nretvalues

	if a^.length>nretmodes then
		txerror("mult ass/mult returns don't agree in number")
	fi
	if nretmodes<=1 then
		txerror("mult ass rhs needs fn yielding 2+ values")
	fi

	p:=a^.a
	i:=1
	while p do
		tpass(p,,need_lv)
		if p^.mode<>d^.modelist[i++] then
			txerror("mult ass/mult fn needs exact type match")
		fi
		p:=p^.nextunit
	od
	return
fi

if a^.length<>b^.length then
	txerror("Mult assign: count mismatch")
fi
if a^.length=0 then
	txerror("Invalid assignment")
fi
rhs:=b^.a
lhs:=a^.a

p:=lhs
while p do
	tpass(p,,need_lv)
	p:=p^.nextunit
od

p:=lhs

q:=rhs
while q do
	tpass(q,p^.mode)
	p:=p^.nextunit
	q:=q^.nextunit
od
end

proc tx_in(unit p,a,b)=
tpass(a)
unless isintmode(a^.mode) then
	txerror("in opnd must be int")
end

tpass(b)

p^.mode:=ti64
case b^.tag
when j_makerange then
	if p^.tag=j_notin then
		p^.tag:=j_inrange
		insertunit(p,j_notl)
	else
		p^.tag:=j_inrange
	fi

when j_makeset then
	if p^.tag=j_notin then
		p^.tag:=j_inset
		insertunit(p,j_notl)
	else
		p^.tag:=j_inset
	fi

elsecase tttypecode[b^.mode]
when 'I','U' then
else
	txerror("in rhs must be range/set")
esac

p^.mode:=ti64			!need two of these cos of insertunit
end

function getprocretmodes(unit p)ref strec=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
var ref strec d
var unit a

if p^.tag<>j_callfn then txerror("multass/need multfn") fi
a:=p^.a

case a^.tag
when j_name then
	return a^.def
else
	return ttnamedef[tttarget[a^.mode]]
esac
end

proc tx_exprlist(unit p,a,int t)=
var unit q

q:=a
while q and q^.nextunit do
	tpass(q)
	q:=q^.nextunit
od

!q points to last expr

tpass(q,t)
p^.mode:=q^.mode
end

proc tx_sign(unit p,a)=

tpass(a)
if ttisreal[a^.mode] then
	coerceunit(a,tr64)
	p^.mode:=tr64
else
	coerceunit(a,ti64)
	p^.mode:=ti64
fi
end

proc fixvoidunit(unit a)=
!a's result is not used as requested type is void
!deal with getting rid of an unwanted result, and convert
!some units to non-value-returning versions as they are handled differently

!CPL $FUNCTION,STRMODE(A^.MODE),=JTAGNAMES[A^.TAG]
	case a^.tag
	when j_assignx then
		a^.tag:=j_assign
!	when j_callfn then
!		a^.tag:=j_callproc
	when j_if, j_longif, j_case, j_switch, j_return, j_select,
		 j_block then
		if a^.mode<>tvoid then
			a^.popflag:=1
		fi

	when j_deepcopyx then
		a^.tag:=j_deepcopy

	when j_return then
		if a^.a then
		fi

	elsif a^.mode=tvoid then			!unit returns no value anyway

	else							!unit returns something, so pop
		a^.popflag:=1
	esac
end

proc twiden(unit p, int lv)=
!intended for widening narrows types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
	var int m,u,mbase

	mbase:=ttbasetype[m:=p^.mode]

	case lv
	when need_lv then               !assume is OK (since this is only called for mem opnds)
	when index_lv,indexlv_lv then
		unless ttisflexvar[m] then
			if lv=indexlv_lv or (stdtypecode[mbase]='A' and stdtypecat[mbase]=tblock) or\
				 mbase=tfixedrecord then		!insert ref
				if p^.tag=j_ptr then
					deleteunit(p,p^.a)
				else
					insertunit(p,j_addrof)
					p^.mode:=createrefmode(nil,m)
				fi
			fi
		end unless
	when addrof_lv then
		txerror("widen/addrof")
	esac
end

function twidenshort(unit p)int=
	if p^.tag=j_const then
		p^.mode:=stdtypebase[ttbasetype[p^.mode]]
		return p^.mode
	fi

	insertunit(p,j_convert)
	case p^.newmode:=stdtypebase[ttbasetype[p^.mode]]
	when ti64 then p^.opcode:=c_iwiden
	when tu64,tc64 then p^.opcode:=c_uwiden
	esac

	p^.mode:=p^.newmode

	return p^.mode
end

proc tx_head(unit p,a,b)=
!does all of head tail init drop reverse prepend append concat left right

	tpass(a)
	p^.mode:=a^.mode

	case p^.tag
	when j_flexptr then
		p^.mode:=createrefmode(nil,tu8,0)
	when j_stringz then
		p^.mode:=trefchar
	esac

	case ttbasetype[a^.mode]
	when tflexstring,tflexarray,tflexbits,tvariant then
	else
		txerror("head/etc can't be used with this type")
	esac

	case p^.tag
	when j_take,j_drop,j_left,j_right,j_convlc, j_convuc then
		if b=nil then
			case p^.tag
			when j_left,j_right then
				p^.b:=createconstunit(1,ti64)
			when j_convlc,j_convuc then
				p^.b:=createconstunit(-1,ti64)
			else
				txerror("count missing")
			esac
		else
			tpass(b,ti64)
		fi
	elsif b then
		txerror("Extra opnd")
	esac
end

proc tx_concat(unit p,a,b)=
!does all of head tail init drop reverse prepend append concat left right
	var int u,v

	tpass(a)
	tpass(b)
	p^.mode:=a^.mode
	case ttbasetype[a^.mode]
	when tflexstring,tflexarray,tflexbits,tvariant then
	else
		txerror("head/etc can't be used with this type")
	esac

	getdominantmodepp(p,a,b,u,v)
	coerceunit(a,u)
	coerceunit(b,v)

end

proc twidenopnd(unit p)=
	if ttisshortint[p^.mode] then
		twidenshort(p)
	fi
end

proc joinstrings(unit p,a,b)=
!do str+str; both a and b are const units with actual strings
	var int newlen,alen:=a^.slength, blen:=b^.slength
	var ref char newstr
	newlen:=alen+blen
	newstr:=pcm_alloc(newlen+1)

	if alen then memcpy(newstr,a^.svalue,alen) fi
	if blen then memcpy(newstr+alen,b^.svalue,blen) fi
	(newstr+alen+blen)^:=0

	a^.svalue:=newstr
	a^.slength:=newlen

	deleteunit(p,a)

end

proc removeaddrof(unit p)=
!p is a lhs of dot operator used for flex/var
!will need to remove any addrof that has been applied
if p=nil then return fi
case p^.tag
when j_addrof then
	deleteunit(p,p^.a)
when j_if then
	removeaddrof(p^.b)
	removeaddrof(p^.c)
else
	txerror("dot/flex: complex record expr, can't remove &")
esac

end

proc twholearrayslice(unit p, int slicemode)=
!p is an array; insert conversions to turn it into a slice:
! (makeslice, addrof p, length)
var int m,marray,length

m:=createrefmode(nil,marray:=p^.mode)
length:=ttlength[marray]
insertunit(p,j_addrof)
p^.mode:=m

insertunit(p,j_makelist)
p^.length:=2
p^.a^.nextunit:=createconstunit(length, ti64)
!p^.mode:=createslicemodek(currproc,tttarget[marray],ttlower[marray], 0)
p^.mode:=slicemode
end

proc tarrayslice(unit p,a,b)=
!p is a slice op; a is an array; b should be makerange
!transform it into a slice like this:
! (makeslice, (addrof (index p a)), (b-a+1))
var unit bi,bj,plength
var int m,marray,length

bi:=b^.a
bj:=b^.b

marray:=a^.mode
m:=tttarget[marray]

tpass(a,,index_lv)
insertunit(a,j_index)
a^.mode:=m
a^.b:=duplunit(bi)
insertunit(a,j_addrof)
a^.mode:=createrefmode(nil,m)

p^.tag:=j_makelist
p^.mode:=createslicemodek(currproc,m,1, 0)
p^.length:=2

p^.b:=nil

plength:=createunit2(j_sub,bj,bi)
plength^.mode:=ti64
tevalbinop(plength)
plength:=createunit2(j_add,plength,createconstunit(1,ti64))
plength^.mode:=ti64
tevalbinop(plength)

p^.a^.nextunit:=plength

PRINTUNIT(P)
end

proc tx_bitfield(unit p,a,int lv)=
var int i,j,bitsize,topbit
var unit r

tpass(a,,lv)

if not ttisinteger[a^.mode] then
	txerror("Int needed")
fi

bitsize:=ttbitwidth[ttbasetype[a^.mode]]
topbit:=bitsize-1

case p^.opcode
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
esac

if i=j then			!single bit
	p^.tag:=j_dotindex
	p^.b:=createconstunit(i,ti64)

	if p^.opcode=bf_even then
		p^.mode:=tu64
		insertunit(p,j_notl)
	fi

else
	r:=createunit2(j_makerange,createconstunit(i,ti64),createconstunit(j,ti64))
	r^.mode:=trange64
	p^.tag:=j_dotslice
	p^.b:=r
fi

p^.mode:=tu64
end
=== msysnew.m 30/35 ===
import clib
import mlib

global record procinforec=
	var word16		fnindex
	var byte		rettype
	var byte		nparams
	var [12]byte	paramlist
end

!for print/read routines
!------------------------------------------
record fmtrec=	! (default)
	var byte	minwidth	! n (0)   min field width (0 if not used or don't care)
	var i8		precision	! .n (0)   number of decimals/significant figures/max width
	var byte	base		! B,H or Xn (10)  2 to 16

	var char	quotechar	! Qc (0)   0 or '"' or c
	var char	padchar		! Pc, Z (' ')
	var char	realfmt		! E,F,G ('f') 'e' or 'f' or 'g'

	var char	plus		! (0)   0 or '+'
	var char	sepchar		! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
	var char	lettercase	! A,a ('A') 'A' or 'a'
	var char	justify		! JL, JR, JC ('R') 'L' or 'R' or 'C'?
	var char	suffix		! Tc (0)   0 or 'B' or 'H' or c
	var char	usigned		! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
	var char	charmode	! C,D (0)  0 or 'C' or 'D'	o/p int as int or single char or double/multi-char
	var char	heapmode	! M (0)  'M' for str-functions, return ptr tp heap string
	var char	param		! Use int value for <fmtparam>
	var byte	spare
end

var int fmtparam			!as set with :'V'

enum (std_io,file_io,str_io)

const comma = ','

var int needgap			= 0
var int outdev			= std_io
var filehandle outchan	= nil
var ref char fmtstr 	= nil

const maxiostack=10
var [maxiostack]filehandle	outchan_stack
var [maxiostack]int			outdev_stack
var [maxiostack]ref char	fmtstr_stack
var [maxiostack]byte		needgap_stack

var [maxiostack]ref char	ptr_stack		!this one doesn't need pushing, as each is pointed to from outchan
var int niostack=0

var [0:]char digits=A"0123456789ABCDEF"
const onesixty=360
var fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

!Read buffer vars
const rd_buffersize = 16384	!total capacity of line buffer

var ref char rd_buffer		! point to start of read buffer
var int rd_length			! length of this line (as read by readln)
var ref char rd_pos			! current position it's up to (next read starts here)
var ref char rd_lastpos		! set by sread() just before reading used for reread()
var int termchar			! terminator char set by readxxx()
var int itemerror			!	set by some read functions, eg for reals

!------------------------------------------

const maxparam=128
global var int nsysparams
global var [maxparam]ichar sysparams

const maxcallback=8
var [0..maxcallback,8]word64 callbackstack
var int ncallbacks=0

var word64 mask63	= 0x7FFF'FFFF'FFFF'FFFF
var real offset64	= 9223372036854775808.0		! 2**63 as r64
var real offset32	= 9223372036854775808.0		! 2**63 as r32

global proc m$init=
var int32 nargs
var int nargs64
var ref[]ichar args
var ref[]ichar env
static var [128]byte startupinfo			! 68 or 104 bytes
var int res
var ichar s

res:=__getmainargs(&nargs,cast(&args),cast(&env),0,cast(&startupinfo))

nsysparams:=nargs

if nsysparams>maxparam then
	printf("Too many params\n")
	stop 50
fi

nargs64:=nargs			!bug when using 32-bit limit when compild with mm
for i:=1 to nargs64 do
	sysparams[i]:=args^[i]
od
end

global proc m$stop(int n)=

assem
	mov d10,[n]
	mov d0,`exit
	call m$callff_4
end

end

global threadedproc m$callff_4=
!0..4 params have been loaded to R10..13
!The foreign function address is in D0
!set up the stack, keeping it aligned, and call the function, adjusting the
!stack before returning.
!For functions rather than procs, the return value will be in A0/D0/XMM0

assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:
	sub Dstack,40			!add an extra 8 bytes to align
	call D0
	add Dstack,40			!unstack the dummy 4 params, plus 8 more bytes
	ret

aligned:
	sub Dstack,32
	call D0
	add Dstack,32
	ret
end

end

global threadedproc m$callff_5=
!one param already pushed. 
!
!There might be another way to do this:
!leave retaddr in place, move P5 this side of retaddr, but leave original P5
!there, and use retn 8 to skip it

assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr
	pop D2					!P5
	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack
	push D2					!P5
	sub Dstack,32
	call D0
	add Dstack,48			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr
	pop D2					!P5
	push D1					!push ret addr back
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,40			!pop all
	ret
end

end

global threadedproc m$callff_6=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,56			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6

	push D1					!push ret addr back

	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,48			!pop all
	ret
end

end

global threadedproc m$callff_7=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,64			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7

	push D1					!push ret addr back

	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,56			!pop all
	ret
end

end

global threadedproc m$callff_8=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,72			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8

	push D1					!push ret addr back

	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,64			!pop all
	ret
end

end

global threadedproc m$callff_9=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,80			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr
	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9

	push D1					!push ret addr back
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address
	sub Dstack,32
	call D0
	add Dstack,72			!pop all
	ret
end

end

global threadedproc m$callff_10=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,88			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10

	push D1					!push ret addr back

	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,80			!pop all
	ret
end

end

global threadedproc m$callff_11=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,96			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11

	push D1					!push ret addr back

	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,88			!pop all
	ret
end

end

global threadedproc m$callff_12=
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,104			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12

	push D1					!push ret addr back

	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,96			!pop all
	ret
end

end

global threadedproc m$callff_14=
static var word64 p13,p14
assem
	test Astack,8			!should be 0 if aligned
	jz aligned
unaligned:					!need to move param5 down
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12
	pop u64 [p13]			!P12
	pop u64 [p14]			!P14

	push D1					!ret addr on top
	sub Dstack,8			!dummy value to align stack

	push u64 [p14]		!P14
	push u64 [p13]		!P13
	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,120			!pop all, including dummy value
	ret

aligned:					!one value and ret addr pushed, allow space for four more
	pop D1					!ret addr

	pop D2					!P5
	pop D3					!P6
	pop D4					!P7
	pop D5					!P8
	pop D6					!P9
	pop D7					!P10
	pop D8					!P11
	pop D9					!P12
	pop u64 [p13]			!P12
	pop u64 [p14]			!P14

	push D1					!push ret addr back

	push u64 [p14]		!P14
	push u64 [p13]		!P13
	push D9					!P12
	push D8					!P11
	push D7					!P10
	push D6					!P9
	push D5					!P8
	push D4					!P7
	push D3					!P6
	push D2					!P5 now this side of ret address

	sub Dstack,32
	call D0
	add Dstack,112			!pop all
	ret
end

end

global proc m$pushcallback=
!save registers rbx, rsi,rdi, r12..r15 to small stack
!Note must take care not to overwrite any of those while saving

!if ncallbacks=maxcallback then
!	println "Callback overflow"
!	stop 1
!fi

assem
	inc word32 [ncallbacks]
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]

	mov [D4],rbx
	mov [D4+8],rsi
	mov [D4+16],rdi
	mov [D4+24],r12
	mov [D4+32],r13
	mov [D4+40],r14
	mov [D4+48],r15
end
end

global proc m$popcallback=
!restore registers rbx, rsi,rdi, r12..r15 from small stack
assem
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]
	mov rbx,[D4]
	mov rsi,[D4+8]
	mov rdi,[D4+16]
	mov r12,[D4+24]
	mov r13,[D4+32]
	mov r14,[D4+40]
	mov r15,[D4+48]
	dec word32 [ncallbacks]
end
end

global function m$getdotindex(word64 a,int i)int=
!return (a iand (1dw<<i))>>i
return (a iand (1<<i))>>i
end

global proc m$setdotindex(ref word64 a, int i,x)=
var ref word32 a32

!see comments on setdotslice
if i>=32 then
!	a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
	a^:=(a^ iand inot (1<<i)) ior (word64(x)<<i)
else
	a32:=cast(a)
	a32^:=(a32^ iand inot (1<<i)) ior (word(x)<<i)
fi
end

global function m$getdotslice(word64 a,int i,j)int=
if i>=j then
	return (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))
else
	return (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
fi
end

global proc m$setdotslice(ref word64 a, int i,j,word64 x)=
!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
var int w
var word64 mask64
var word mask
var ref word32 a32

if i>j then println "SETDOTSLICE?"; stop 52 fi

!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing
!to bytes beyond the 32-bit value
!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too

if j>=32 then
	mask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
	a^:=(a^ iand inot mask64) ior x<<i
else
	a32:=cast(a)
	mask:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
	a32^:=(a32^ iand inot mask) ior x<<i
fi

end

function m$get_nprocs:int=
	assem
		mov D0,[$nprocs]
	end
end

function m$get_procname(int n)ichar=
	assem
		lea D0,[$procnames]
		mov D1,[n]
		mov D0,[D0+D1*8-8]
!		mov D0,[sss]
	end
end

function m$get_procaddr(int n)ref proc=
	assem
		lea D0,[$procaddrs]
		mov D1,[n]
		mov D0,[D0+D1*8-8]
	end
end

global function m$get_nexports:int=
	assem
		mov D0,[$nexports]
	end
end

global function m$get_procexport(int n)ref void=
	assem
		lea D0,[$procexports]
		mov D1,[n]
		shl D1,1
		lea D0,[D0+D1*8-16]
	end
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

global proc m$print_startfile(ref void dev)=
	pushio()
	outchan:=cast(dev)
	if dev then
		outdev:=file_io
	else
		outdev:=std_io
	fi
end

global proc m$print_startstr(ref char s)=
	var ref ref char p
	pushio()

	ptr_stack[niostack]:=s
	p:=&ptr_stack[niostack]

	outchan:=cast(p)
	outdev:=str_io
end

global proc m$print_startptr(ref ref char p)=
	pushio()

	outchan:=cast(p)
	outdev:=str_io
end

global proc m$print_startcon=
	pushio()
	outdev:=std_io
end

global proc m$print_setfmt(ref char format)=
	fmtstr:=format
end

global proc m$print_end=
	needgap:=0
	nextfmtchars(1)
	if niostack=0 then return fi
	outchan	:= outchan_stack[niostack]
	outdev	:= outdev_stack[niostack]
	fmtstr	:= fmtstr_stack[niostack]
	needgap	:= needgap_stack[niostack]
	--niostack
end

global proc m$print_ptr(u64 a,ichar fmtstyle=nil)=
	var [20]char s

	if fmtstyle=nil then
		fmtstyle:="z8H"
	fi
	m$print_u64(a,fmtstyle)
end

!global proc m$print_bool(int a,ichar fmtstyle=nil)=
!	[20]char s
!	nextfmtchars()
!	printstr((a|"T"|"F"))
!	needgap:=1
!end

global proc m$print_i64(int64 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt
	var int n

	nextfmtchars()

	if fmtstyle=nil then
		if a>=0 then
			n:=u64tostr(a,&.s,10,0)
		else
			s[1]:='-'
			n:=u64tostr(-a,&s[2],10,0)+1
		fi
		printstr_n(&.s,n)

	else
		strtofmt(fmtstyle,-1,&fmt)
		if fmt.param='V' then
			fmtparam:=a
!CPL "SET FMTPARAM TO",A
			needgap:=0
		else
			tostr_i64(a,&fmt)
		fi
	fi
	needgap:=1
end

global proc m$print_u64(word64 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt

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

global proc m$print_i128(int128 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	if a>=0 then
		tostr_u128(a,&fmt,0)
	else
		tostr_u128(-a,&fmt,1)
	fi

	needgap:=1
end

global proc m$print_u128(word128 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	tostr_u128(a,&fmt,0)
	needgap:=1
end

global proc m$print_r64(real x,ichar fmtstyle=nil)=
	var [360]char s
	var fmtrec fmt

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

global proc m$print_r32(real32 x,ichar fmtstyle=nil)=
	m$print_r64(x,fmtstyle)
end

global proc m$print_c8(int64 a,ichar fmtstyle=nil)=
	var [40]char s
	var fmtrec fmt
	var int n

	nextfmtchars()

	s[1]:=a
	s[2]:=0
	printstr(&.s)
	needgap:=1
end

global proc m$print_str(ichar s, fmtstyle=nil)=
	nextfmtchars()
	var fmtrec fmt
	if fmtstyle=nil then
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,&fmt)
	fi
	needgap:=1
end

!global proc m$print_flexstr(object s, ichar fmtstyle=nil)=
!	nextfmtchars()
!	var fmtrec fmt
!
!	if fmtstyle=nil then
!		if s^.length then
!			printstr_n(s^.strptr,s^.length)
!		fi
!	else
!		strtofmt(fmtstyle,-1,&fmt)
!		tostr_str(str_stringz(s),&fmt)
!	fi
!	needgap:=1
!end

global proc m$print_newline=
	needgap:=0
	nextfmtchars(1)
	printstr("\w")
end

global proc m$print_nogap=
	needgap:=0
end

proc printstr(ichar s)=
	var int n
	var ref ref char p
	
	case outdev
	when std_io then
		printf("%s",s)
	when file_io then
		fprintf(outchan,"%s",s)
	when str_io then
		p:=cast(outchan)
		strcpy(p^,s)
		p^+:=strlen(s)
	esac
end

global proc printstr_n(ichar s,int n)=
	var ref ref char p

	case n
	when -1 then n:=strlen(s)
	when 0 then return
	esac

	case outdev
	when str_io then
		p:=cast(outchan)
		memcpy(p^,s,n)
		p^+:=n
		p^^:=0
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	when std_io then
		printf("%.*s",n,s)
	esac
end

global proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
!	emitc "printf(""%.*s"",(i32)length,s);"
	if f=nil then
		printf("%.*s",length,s)
	else
		fprintf(f,"%.*s",length,s)
	fi
fi
end

proc printchar(int ch)=
	var ref ref char p
	case outdev
	when std_io then
		printf("%c",ch)
	when file_io then
		fprintf(outchan,"%c",ch)
	when str_io then
		p:=cast(outchan)
		p^^:=ch
		p^+:=1
		p^^:=0
	esac
end

proc nextfmtchars(int lastx=0)=
	var char c
	var ref char pstart
	var int n

	if not fmtstr then			!format not in use
		if needgap then
			printchar(' ')
!		printstr_n(" ",1)
		fi
		needgap:=0
		return
	fi

	pstart:=fmtstr
	n:=0

	while (1) do
		c:=fmtstr^
		switch c
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
	skip::
			++n
			++fmtstr
		endswitch
	od
end

global proc strtofmt(ref char s,int slen,ref fmtrec fmt) =		!PC_STRTOFMT
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!n	Width
!.n	Max width/precision
!A	Convert to upper when
!a	Convert to lower when
!B	Binary
!C	Show int as single n-bit (unicode) character
!D	Show int as multi-bit (unicode) character
!E,F,G	Specify format for double (corresponds to C format codes)
!F
!G
!H	Hex
!JC	Justify centre
!JL	Justify left
!JR	Justify right
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

	var char c
	var byte wset
	var int n
	var [0:100]char str

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
		switch c
		when 'B', 'b' then fmt^.base:=2
		when 'H', 'h' then fmt^.base:=16
		when 'O', 'o' then fmt^.base:=8
		when 'X', 'x' then
			c:=s^
			if c then
				switch c
				when '0'..'9' then c:=c-'0'
				when 'A'..'F' then c:=c-'A'+10
				when 'a'..'f' then c:=c-'a'+10
				else
					c:=10
				end
				fmt^.base:=c
				++s
			fi
		when 'Q', 'q' then fmt^.quotechar:='"'
		when '~' then fmt^.quotechar:='~'
		when 'J', 'j' then
			fmt^.justify:=toupper(s^)
			if s^ then
				++s
			fi
		when 'A' then fmt^.lettercase:='A'
		when 'a' then fmt^.lettercase:='a'
		when 'Z', 'z' then fmt^.padchar:='0'
		when 'S', 's' then
			fmt^.sepchar:=s^
			if s^ then
				++s
			fi
		when 'P', 'p' then
			fmt^.padchar:=s^
			if s^ then
				++s
			fi
		when 'T', 't' then
			fmt^.suffix:=s^
			if s^ then
				++s
			fi
		when 'W', 'w' then fmt^.usigned:='W'
		when 'E', 'e' then fmt^.realfmt:='e'
		when 'F', 'f' then fmt^.realfmt:='f'
		when 'G', 'g' then fmt^.realfmt:='g'
! when '0','1','2','3','4','5','6','7','8','9' then
		when '.' then
			wset:=1
		when comma,'_' then fmt^.sepchar:=c
		when '+' then fmt^.plus:='+'
		when 'D', 'd' then fmt^.charmode:='D'
		when 'C', 'c' then fmt^.charmode:='C'
		when 'M', 'm' then fmt^.heapmode:='M'
		when 'V','v' then fmt.param:='V'
		when '*' then
			n:=fmtparam
			goto gotwidth
!			if wset then
!CPL "FMT/* WSET",FMTPARAM
!				fmt.minwidth:=fmtparam
!			else
!CPL "FMT/*",FMTPARAM
!				fmt.precision:=fmtparam
!			fi
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
gotwidth::
				if not wset then
					fmt^.minwidth:=n
					wset:=1
				else
					fmt^.precision:=n
				fi
			fi
		endswitch
	od
end

function domultichar (ref char p,int n,ref char dest,ref fmtrec fmt)int =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
	var [0:20]char str
	var ref char q
	var int i,nchars

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

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =		!EXPANDSTR
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt^.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string; caller must take care of this
!result will be zero-terminated, for use in this module

	var int i,w,m

!check to see if result is acceptable as it is
	w:=fmt^.minwidth
	if w=0 or w<=n then		! allow str to be longer than minwidth
		strncpy(t,s,n)
		(t+n)^:=0
		return n
	fi

	if fmt^.justify='L' then	! left-justify
		strncpy(t,s,n)
		t+:=n
		for i:=1 to w-n do
			t^:=fmt^.padchar
			++t
		od
		t^:=0
	elsif fmt^.justify='R' then
		if fmt^.padchar='0' and fmt^.base and (s^='-' or s^='+') then ! need to move sign outside 
			t^:=s^
			++t
			to w-n do
				t^:=fmt^.padchar
				++t
			od
			strncpy(t,s+1,n-1)
			(t+n-1)^:=0
		else
			to w-n do
				t^:=fmt^.padchar
				++t
			od
			strncpy(t,s,n)
			(t+n)^:=0
		fi

	else				! centre-justify?

		m:=(w-n+1)/2
		to m do
			t^:=fmt^.padchar
			++t
		od
		strncpy(t,s,n)
		t+:=n
		to w-n-m do
			t^:=fmt^.padchar
			++t
		od
		t^:=0

	fi
	return w
end

!function xdivrem(word64 a,b)word64,word64=
!	assem
!		xor rdx,rdx
!		mov rax,[a]
!		div u64 [b]
!		mov D1,rdx
!	end
!end

function xdivrem(word64 a,b, &remainder)word64=
	var word64 q,r
	assem
		xor rdx,rdx
		mov rax,[a]
		div u64 [b]
		mov [q],rax	
		mov [r],rdx	
	end
	remainder:=r
	return q
end

function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
	var [0:onesixty]char t
	var u64 dd
	var int i,j,k,g
	var int cc
	var int dummy
	var ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		aa:=xdivrem(aa,base,dd)
		t[++i]:=digits[dd]

!		t[++i]:=digits[aa rem base]
!		aa:=aa/base

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

function u128tostr(u128 aa,ref char s,word base,int sep)int =
!convert 128-bit int a to string in s^
!base is number base, usually 10 but can be 2 to 16
	var [0:160]char t
	var u64 dd
	var int i,j,k,g
	var int dummy
	var ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		aa:=xdivrem128(aa,base,dd)
		t[++i]:=digits[dd]

!		t[++i]:=digits[aa rem base]
!		aa:=aa/base

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

function xdivrem128(word128 a, word64 b, &remainder)word128=
	var word128 d,e,r
	var word rlow

	d:=a/b
	r:=a-d*b

	assem
		mov d0,[r]
		mov [rlow],d0
	end
	remainder:=rlow
	return d
end

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt^.unsigned forces ints to be treated as longs)
!returns length of s
	var [0:onesixty]char str				! allow for binary with separators!
	var int i,j,k,n,w,usigned
	static var u64 mindint=0x8000'0000'0000'0000

	usigned:=0
	if fmt^.usigned then
		usigned:=1
	fi

	if aa=mindint and not usigned then		! minint

		str[0]:='-'
		n:=i64mintostr(&str[1],fmt^.base,fmt^.sepchar)+1
	else
		if (not usigned and aa<-0) or fmt^.plus then
			if aa<0 then
				aa:=-aa
				str[0]:='-'
			else
				str[0]:='+'
			fi
			n:=u64tostr(aa,&str[1],fmt^.base,fmt^.sepchar)+1
		else
			n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)
		fi
	fi

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	var [0:onesixty]char str				! allow for binary with separators!
	var int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u128tostrfmt(i128 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	var [0:onesixty]char str				! allow for binary with separators!
	var int i,j,k,n,w

	n:=u128tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function i64mintostr(ref char s,int base,int sep)int =		!I64MINTOSTR
!convert minint to string in s do not include minus sign
!return number of chars in string
	var [0:onesixty]char t
	var int i,j,k,g,neg

	switch base
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
	endswitch

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

function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
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
	var ref char u,v
	var [256]char str
	var int w,nheap		! whether any heap storage is used  bytes allocated

	nheap:=0

	if fmt^.quotechar or fmt^.lettercase then		! need local copy
		if n<256 then
			u:=&.str
		else
			nheap:=n+3					! allow for quotes+terminator
			u:=pcm_alloc(nheap)
		fi
		if fmt^.quotechar then
			v:=u
			v^:=fmt^.quotechar
			++v
			if n then
				strcpy(v,s)
				v+:=n
			fi
			v^:=fmt^.quotechar
			++v
			v^:=0
			n+:=2
		else
			memcpy(u,s,n)
		fi
		switch fmt^.lettercase
		when 'a' then	! need lower when
			convlcstring(u)
		when 'A' then
			convucstring(u)
		endswitch
		s:=u
	fi

	w:=fmt^.minwidth
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
	var [360]char str
	var int n

	case fmt^.charmode
	when 0 then
		n:=i64tostrfmt(a,&.str,fmt)
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	else						!assume 'C'
		printchar(a)			!no other formatting allowed
		return
	esac

	printstr_n(&.str,n)
end

proc tostr_u64(word64 a, ref fmtrec fmt)=
	var [360]char str
	var int n

	case fmt^.charmode
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C','c' then
		printchar(a)			!no other formatting allowed
		return

	else
		n:=u64tostrfmt(a,&.str,fmt)
	esac

	printstr_n(&.str,n)
end

proc tostr_u128(word128 a, ref fmtrec fmt,int neg)=
	var [360]char str
	var int n

	case fmt^.charmode
	when 'D','d' then
		n:=domultichar(ref char(&a),8,&.str,fmt)

	when 'C','c' then
		printchar(a)			!no other formatting allowed
		return

	else
		if neg then
			str[1]:='-'
			n:=u128tostrfmt(a,&str[2],fmt)+1
		else
			n:=u128tostrfmt(a,&.str,fmt)
		fi
	esac

	printstr_n(&.str,n)
end

proc tostr_r64(real x,ref fmtrec fmt) =
	var [360]char str,str2
	var [0:10]char cfmt
	var int n

	cfmt[0]:='%'

	if fmt^.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt^.realfmt
		cfmt[4]:=0
		sprintf(&.str,&.cfmt,fmt^.precision,x)
	else
		cfmt[1]:=fmt^.realfmt
		cfmt[2]:=0
		sprintf(&.str,&.cfmt,x)
	fi

!at this point, n is the str length including signs and suffix

!(TRY TAKING N FROM RESULT OF SPRINTF ABOVE)
	n:=strlen(&.str)		! current length

	if n<fmt^.minwidth then
		n:=expandstr(&.str,&.str2,n,fmt)
		strcpy(&.str,&.str2)
	fi

	printstr_n(&.str,n)
end

proc tostr_str(ref char s, ref fmtrec fmt) =
	var int oldlen,newlen,n
	var ref char t

!try and work out size of formatted string
	oldlen:=strlen(s)
	newlen:=oldlen

	if fmt^.quotechar or fmt^.minwidth>newlen or fmt^.lettercase or fmt.precision then
		if fmt^.quotechar then
			newlen+:=2
		fi
		if fmt^.minwidth>newlen then
			newlen:=fmt^.minwidth
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
	static var fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	fi
end

global function strint(int64 a, ichar fmtstyle=nil)ichar=
	static var[100]char str
	var ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

global proc getstrint(int64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

global function strword(word64 a, ichar fmtstyle=nil)ichar=
	static var[100]char str
	var ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

global function strreal(real a, ichar fmtstyle=nil)ichar=
	static var [320]char str
	var ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_r64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

function getstr(ichar s, ref fmtrec fmt)ichar=
	if fmt^.heapmode then
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
	initreadbuffer()
	readlinen(f,rd_buffer,rd_buffersize)

	rd_length:=strlen(rd_buffer)
	rd_pos:=rd_buffer
	rd_lastpos:=nil
end

global proc m$read_strline(ichar s)=
	var int n

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
	var ref char p,s,itemstr
	var char quotechar, c

	unless rd_buffer then 
		initreadbuffer()
!abortprogram("No readln")
	end unless


	s:=rd_pos

!scan string, eliminating leading white space
	while s^=' ' or s^=9 do
		++s
	od

	itemstr:=s				!assume starts here
	rd_lastpos:=rd_pos:=s

	if s^=0 then			! No more chars left to read return null string
		termchar:=0
		itemlength:=0
		return s
	fi

	quotechar:=0			! Allow possible enclosing single or double quotes
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
		switch c
		when ' ', 9, comma, '=' then		! separator
			if quotechar or p=s then			!can be considered part of name if inside quotes, or is only char
				goto normalchar
			fi
			termchar:=c
			exit
		else
	normalchar::
			if c=quotechar then
				if s^=quotechar then	! embedded quote
					p^:=c
					++s
					++p
				else					! end of name
					termchar:=s^
					if termchar=',' or termchar='=' then
						++s
						termchar:=s^
					fi
					exit
				fi
			else
				p^:=c
				++p
			fi
		endswitch
	od

	if s^=0 then
		termchar:=0
	fi
	itemlength:=p-itemstr				! actual length of token
	rd_pos:=s

	return itemstr
end

global function strtoint(ichar s,int length=-1, base=10)int64=
!return point to next char after terminator (which can be just off length of string)
	var byte signd
	var word64 aa
	var char c,d

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
		switch c
		when 'A'..'F' then d:=c-'A'+10
		when 'a'..'f' then d:=c-'a'+10
		when '0'..'9' then d:=c-'0'
		when '_', '\'' then
			next
		else
			itemerror:=1
			exit
		endswitch

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
	var ref char s
	var int length,c
	var int64 aa

	case fmt
	when 'C','c' then
		rd_lastpos:=rd_pos
		if rd_pos^ then
			return rd_pos++^
		else
			return 0
		fi
	when 'T','t' then
		return termchar
	when 'E','e' then
		return itemerror
	esac

	s:=readitem(length)


	case fmt
	when 0,'I','i' then
		return strtoint(s,length)
	when 'B','b' then
		return strtoint(s,length,2)
	when 'H','h' then
		return strtoint(s,length,16)
	esac
	return 0
end

global function m$read_r64(int fmt=0)real=
	var [512]char str
	var ref char s
	var int length
	var int32 numlength
	var real x

	s:=readitem(length)

	if length=0 or length>=str.len then		!assume not a real
		return 0.0
	fi
	memcpy(&.str,s,length)
	str[length+1]:=0

	itemerror:=0

!CPL "CALLING SCCANF ON:",&.str,=X

	if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
!CPL =NUMLENGTH,=LENGTH
!		if numlength=length then x:=0.0 fi
		x:=0.0
		itemerror:=1
	fi

	return x
end

global proc m$read_str(ref char dest, int destlen=0,fmt=0)=
	var ref char s
	var int length,numlength
	var real x

	itemerror:=0
	if fmt='L' or fmt='l' then
		s:=rd_pos
		length:=rd_buffer+rd_length-rd_pos

	else
		s:=readitem(length)

		if fmt='N' or fmt='n' then
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

global proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_str(dest,destlen,fmt)
end

global proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

global proc reread=
	rd_pos:=rd_lastpos
end

global function valint(ichar s, int fmt=0)int64=
var ref char old_pos, old_lastpos
var int64 aa

initreadbuffer()
old_pos:=rd_pos
old_lastpos:=rd_lastpos

rd_pos:=s
aa:=m$read_i64(fmt)
rd_pos:=old_pos
rd_lastpos:=old_lastpos
return aa
end

global function valreal(ichar s)real=
var ref char old_pos, old_lastpos
var real x

initreadbuffer()
old_pos:=rd_pos
old_lastpos:=rd_lastpos

rd_pos:=s
x:=m$read_r64()
rd_pos:=old_pos
rd_lastpos:=old_lastpos
return x
end

proc iconvlcn(ref char s,int n) =		!ICONVLCN
to n do
	s^:=tolower(s^)
	++s
od
end

proc iconvucn(ref char s,int n) =		!ICONVUCN
to n do
	s^:=toupper(s^)
	++s
od
end

proc convlcstring(ref char s)=		!CONVLCSTRING
while (s^) do
	s^:=tolower(s^)
	++s
od
end

proc convucstring(ref char s)=		!CONVUCSTRING
while (s^) do
	s^:=toupper(s^)
	++s
od
end

!GLOBAL PROC M$PRINT_U32(word32 a, ref void fmt)=
!	m$print_u64(a,nil)
!end
!
!GLOBAL PROC M$PRINT_I32(int32 a, ref void fmt)=
!	m$print_i64(a,nil)
!end
!
!GLOBAL PROC M$STARTPRINT(ref void dev)=
!	m$print_startfile(dev)
!end
!
!GLOBAL PROC M$STARTPRINTCON=
!	m$print_startcon()
!end
!
!GLOBAL PROC M$ENDPRINT=
!	m$print_end()
!end

global threadedproc m$ufloat_r64u64=
	assem
		cmp D10,0
		jl fl1
!number is positive, so can treat like i64
		cvtsi2sd XMM15,D10
		ret
fl1:						!negative value
		and D10,[mask63]		!clear top bit (subtract 2**63)
		cvtsi2sd XMM15,D10
		addsd XMM15,[offset64]	!(add 2**63 back to result)
		ret
	end
end

global threadedproc m$ufloat_r64u32=
	assem
		mov D10,D10				! clear top half (already done if value just moved there)
		cvtsi2sd XMM15,D10
		ret
	end
end

global threadedproc m$ufloat_r32u32=
	assem
		mov D10,D10
		cvtsi2ss XMM15,D10
		ret
	end
end

global threadedproc m$ufloat_r32u64=
	assem
		cmp D10,0
		jl fl2
!number is positive, so can treat like i64
		cvtsi2ss XMM15,D10
		ret
fl2:						!negative value
		and D10,[mask63]		!clear top bit (subtract 2**63)
		cvtsi2ss XMM15,D10
		addss XMM15,[offset32]	!(add 2**63 back to result)
		ret
	end
end

!global function m$power_i64(int64 n,a)int64=
!if n<0 then
!	return 0
!elsif n=0 then
!	return 1
!elsif n=1 then
!	return a
!elsif (n iand 1)=0 then
!!	return ipower(a*a,n/2)
!	return m$power_i64(a*a,n/2)
!else			!assume odd
!	return m$power_i64(a*a,(n-1)/2)*a
!fi
!end

global function m$power_i64(int64 n,a)int64=
if n<0 then
	return 0
elsif n=0 then
	return 1
elsif n=1 then
	return a
elsif (n iand 1)=0 then
!	return ipower(a*a,n/2)
	return m$power_i64(n/2,sqr a)
else			!assume odd
	return m$power_i64((n-1)/2,sqr a)*a
fi
end

global proc m$intoverflow=
	abortprogram("Integer overflow detected")
end

global proc m$mul_i128(word128 bb,aa)=
!CPL "$MUL128"
	assem
		mov d2,[aa]			!a1
		mov d3,[aa+8]		!a2
		mov d4,[bb]			!b1
		mov d5,[bb+8]		!b2


		mov d0,d2			!a1
		imul2 d0,d5			!*b2	
		mov d6,d0			!=>d6

		mov d0,d3			!a2
		imul2 d0,d4			!*b1
		mov d7,d0			!=>d7

		mov d0,d2			!a1
		mul d4				!*b1
		add d11,d6			! + a1*b2<<64
		add d11,d7			! + a2*b1<<64
		mov d1,d11
	end
end

global proc m$idiv_i128(word128 bb,aa)=
!does 128/64 bits only
	assem
		mov d2,[aa]
		mov d3,[aa+8]

		mov d4,[bb]
		or d4,d4
		jz divbyzero

		mov d0,d3		!a2
		xor d11,d11
		div d4			!a2/b
		mov d6,d0		! => c2
		mul d4			!c2*b
		sub d3,d0		!a2-:=c2*b

		mov d0,d2
		mov d11,d3		!a2:a1
		div d4			!/b
		mov d1,d6
	end
	return

asm divbyzero:
CPL "DIV BY ZERO"
	stop 1
end

global proc m$dotindex(word i,a)=
!return a.[i] in d0
	assem
		mov d0,[a]
		mov cl,[i]
		shr d0,cl
		and d0,1
	end	
end

global proc m$dotslice(word j,i,a)=
!return a.[i..j] in d0; assumes j>=i
	assem
		mov d0,[a]
		mov rcx,[i]
		shr d0,cl
		sub rcx,[j]
		neg rcx				!j-1
		mov d2,0xFFFF'FFFF'FFFF'FFFE
		shl d2,cl
		not d2
		and d0,d2
	end	
end

global proc m$popdotindex(word i,ref word p,word x)=
!p^.[i]:=x
	assem
		mov d3,[p]
		mov cl,[i]
		mov d0,[d3]
		mov d1,1
		shl d1,cl			!000001000
		not d1				!111110111
		and d0,d1			!clear that bit in dest
		mov d1,[x]
		and d1,1
		shl d1,cl
		or d0,d1
		mov [d3],d0
	end	
end

global proc m$popdotslice(word j,i, ref word p, word x)=
!p^.[i..j]:=x
	assem
!d3 = p
!d4 = x, then shifted then masked x
!d5 = i
!d6 = clear mask

		mov d3,[p]
		mov d4,[x]
		mov d5,[i]
		mov rcx,d5			!i
		shl d4,cl			!x<<i
		mov rcx,[j]
		sub rcx,d5			!j-i
		inc rcx				!j-i+1
		mov d2,0xFFFF'FFFF'FFFF'FFFF
		shl d2,cl			!...111100000     (assume 5-bit slice)
		not d2				!...000011111
		mov rcx,d5			!i
		shl d2,cl			!...000011111000  (assume i=3)
		and d4,d2			!mask x (truncate extra bits)
		mov d0,[d3]
		not d2				!...111100000111
		and d0,d2			!clear dest bits
		or d0,d4			!add in new bits
		mov [d3],d0
	end	
end


global function m$sin(real x)real = {`sin(x)}
global function m$cos(real x)real = {`cos(x)}
global function m$tan(real x)real = {`tan(x)}
global function m$asin(real x)real = {`asin(x)}
global function m$acos(real x)real = {`acos(x)}
global function m$atan(real x)real = {`atan(x)}
global function m$ln(real x)real = {`log(x)}
!global function m$lg(real x)real = {`lg(x)}
global function m$log(real x)real = {`log10(x)}
global function m$exp(real x)real = {`exp(x)}
global function m$floor(real x)real = {`floor(x)}
global function m$ceil(real x)real = {`ceil(x)}
global function m$fract(real x)real = {abortprogram("FRACT");0}
global function m$round(real x)real = {abortprogram("ROUND");0}
=== mlib.m 31/35 ===
import msys
import clib
import oslib

!const mem_check=1
const mem_check=0

global var [0..300]u64 allocupper
global var int alloccode				!set by heapalloc
global var int allocbytes				!set by heapalloc
global var int fdebug=0
global var int rfsize

const threshold=1<<25
const alloc_step=1<<25
var word maxmemory
var int  maxalloccode

var byte pcm_setup=0

var int show=0

global var int memtotal=0
global var int64 smallmemtotal=0
global var int smallmemobjs=0
global var int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=500000
var [maxmemalloc+1]ref int32 memalloctable
var [maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
var ref byte pcheapstart
var ref byte pcheapend			!points to first address past heap
var ref byte pcheapptr

const int maxblockindex = 8 		!2048
global const int maxblocksize = 2048

var [0:maxblocksize+1]byte sizeindextable	!convert byte size to block index 1..maxblockindex

const int size16   = 1			!the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

GLOBAL var [0:9]ref wordp freelist

global record strbuffer =
	var ichar strptr
	var int32 length
	var int32 allocated
end

global tabledata() [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

var [2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

global function pcm_alloc(int n)ref void =		!PCM_ALLOC
var ref byte p
!int i

if not pcm_setup then
	pcm_init()
!	abortprogram("need pcm_init")
fi

if n>maxblocksize then			!large block allocation
	alloccode:=pcm_getac(n)
	allocbytes:=allocupper[alloccode]

	p:=allocmem(allocbytes)
	if not p then
		abortprogram("pcm_alloc failure")
	fi

if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

	return p
fi

alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

if alloccode=0 then					!sizes below 16 bytes (can I adjust sizeindextable to?)
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

SMALLMEMTOTAL+:=ALLOCBYTES

if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
	freelist[alloccode]:=ref wordp(int((freelist[alloccode])^))

	return p
fi

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	return p
fi
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

return p
end

global proc pcm_free(ref void p,int n) =		!PCM_FREE
!n can be the actual size requested it does not need to be the allocated size
var int acode

if n=0 then return fi

if n>maxblocksize then		!large block
	if mem_check then removefrommemalloc(p,n) fi

	free(p)
	return
fi

if p then
	acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

	smallmemtotal-:=allocupper[acode]

	if mem_check then removefrommemalloc(p,allocupper[acode]) fi

	(ref wordp(p))^:=wordp(int(freelist[acode]))
	freelist[acode]:=p
fi
end

global proc pcm_freeac(ref void p,int alloc) =		!PCM_FREEAC
pcm_free(p,allocupper[alloc])
end

global proc pcm_copymem4(ref void p,q,int n) =	!PCM_COPYMEM4
!copy n bytes of memory from q to p.
!the memory spaces used are multiples of 16 bytes, but n itself could be anything
!n can be zero, and need not be a multiple of 4 bytes

memcpy(p,q,n)
end

global proc pcm_clearmem(ref void p,int n) =		!PCM_CLEARMEM
memset(p,0,n)
end

global proc pcm_init =		!PCM_INIT
!set up sizeindextable too
!sizeindextable[0] = 0
var int j,k,k1,k2
var int64 size
const limit=1<<33

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
!	if size>4 billion then
!		size+:=alloc_step
!	fi
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

global function pcm_getac(int size)int =		!PCM_GETAC
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

global function pcm_newblock(int itemsize)ref void=
!create new heap block (can be first)
!also optionally allocate small item at start
!return pointer to this item (and to the heap block)
static var int totalheapsize
var ref byte p

totalheapsize+:=pcheapsize
alloccode:=0
p:=allocmem(pcheapsize)	!can't free this block until appl terminates
if p=nil then
	abortprogram("Can't alloc pc heap")
fi

pcheapptr:=p
pcheapend:=p+pcheapsize

if pcheapstart=nil then		!this is first block
	pcheapstart:=p
fi
pcheapptr+:=itemsize
return ref u32(p)
end

global function pcm_round(int n)int =		!PCM_ROUND
!for any size n, return actual number of bytes that would be allocated
static var [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

if n>maxblocksize then
	return n
else
	return allocbytes[sizeindextable[n]]
fi
end

global function pcm_array(int n)int =		!PCM_ARRAY
!n bytes are needed for an array return the number of bytes to be actually allocated
var int m

if n<=maxblocksize then	!automatic rounding up used for small heap
	return pcm_round(n)
! allocbytes[sizeindextable[n]]
else				!devise some strategy probably doubling up.
	m:=2048
	while n>m do
		m<<:=1
	od
	return m
fi

end

global proc pcm_printfreelist(int size,ref wordp p) =		!PCM_PRINTFREELIST
println "Size: ",size
while p do
!	printf(" %llX",u64(p))
	print " ",,p:"h"
	p:=ref wordp(int(p^))
od
puts("")
end

global proc pcm_diags(ref char caption) =		!PCM_DIAGS
var int m

println "HEAP FREELISTS:",caption

m:=16
for i:=1 to 8 do
	pcm_printfreelist(m,freelist[i])
	m<<:=1
od
end

global function pcm_allocz(int n)ref void =		!PCM_ALLOCZ
var ref void p
p:=pcm_alloc(n)

memset(p,0,n)
return p
end

global function pcm_copyheapstring(ref char s)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
var ref char q
var int n
if s=nil then return nil fi

n:=strlen(s)+1
q:=pcm_alloc(n)
memcpy(q,s,n)
return q
end

global function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	var ref char q
	if length=0 then return nil fi

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

proc addtomemalloc(ref int32 ptr,int size)=
!add ptr to allocated table

!CPL "***************ADD TO ALLOC:",ptr,size

for i to maxmemalloc do
	if memalloctable[i]=ptr then
		CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
CPL
CPL
		stop 2
	fi

	if memalloctable[i]=nil then		!unused entry
		memalloctable[i]:=ptr
		memallocsize[i]:=size
		return
	fi
od
CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
stop 3
end

proc removefrommemalloc(ref int32 ptr,int size)=
!remove ptr to allocated table

!CPL "------------------************REMOVE FROM ALLOC:",ptr,size

for i to maxmemalloc do
	if memalloctable[i]=ptr then

if memallocsize[i]<>size then
	CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
!PCERROR("MEMERROR")
CPL
CPL
	abortprogram("MEMSIZE")
fi

		memalloctable[i]:=nil
		return
	fi
od
CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
CPL
CPL
abortprogram("MEM")
stop 4
end

global function allocmem(int n)ref void =		!ALLOCMEM
var ref void p

p:=malloc(n)
if (p) then
	return p
fi
println n,memtotal
abortprogram("Alloc mem failure")
return nil
end

global function reallocmem(ref void p,int n)ref void =		!REALLOCMEM
p:=realloc(p,n)
return p when p
println n
abortprogram("Realloc mem failure")
return nil
end

global proc abortprogram(ref char s) =		!ABORTPROGRAM
println s
print   "ABORTING: Press key..."
os_getch()
stop 5
end

global function getfilesize(filehandle handlex)int=		!GETFILESIZE
	var word32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

global proc readrandom(filehandle handlex, ref byte mem, int offset, size) =		!READRANDOM
	var int a
	fseek(handlex,offset,seek_set)
	a:=fread(mem,1,size,handlex)			!assign so as to remove gcc warning
end

global function writerandom(filehandle handlex, ref byte mem, int offset,size)int =		!WRITERANDOM
	fseek(handlex,offset,seek_set)
	return fwrite(mem,1,size,handlex)
end

global function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

global function getfilepos(filehandle file)int=
	return ftell(file)
end

global function readfile(ref char filename)ref byte =		!READFILE
var filehandle f
var int size
var ref byte m,p

f:=fopen(filename,"rb")
if f=nil then
	return nil
fi
rfsize:=size:=getfilesize(f)

m:=malloc(size+4)		!allow space for etx/zeof etc

if m=nil then
	return nil
fi

readrandom(f,m,0,size)
p:=m+size			!point to following byte
p^:=0
(p+1)^:=26
(p+2)^:=0			!allow use as string

fclose(f)
return m
end

global function writefile(ref char filename,ref byte data,int size)int =
var filehandle f
var int n

f:=fopen(filename,"wb")
if f=nil then
	return 0
fi

n:=writerandom(f,data,0,size)
fclose(f)
return n
end

global function checkfile(ref char file)int=		!CHECKFILE
var filehandle f
if f:=fopen(file,"rb") then
	fclose(f)
	return 1
fi
return 0
end

global proc readlinen(filehandle handlex,ref char buffer,int size) =		!READLINEN
!size>2
var int ch
var ref char p
var int n
var [0:100]char buff
var byte crseen

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

global proc iconvlcn(ref char s,int n) =		!ICONVLCN
to n do
	s^:=tolower(s^)
	++s
od
end

global proc iconvucn(ref char s,int n) =		!ICONVUCN
to n do
	s^:=toupper(s^)
	++s
od
end

global proc convlcstring(ref char s)=		!CONVLCSTRING
while (s^) do
	s^:=tolower(s^)
	++s
od
end

global proc convucstring(ref char s)=		!CONVUCSTRING
while (s^) do
	s^:=toupper(s^)
	++s
od
end

global function changeext(ref char s,newext)ichar=		!CHANGEEXT
!whether filespec has an extension or not, change it to newext
!newext should start with "."
!return new string (locally stored static string, so must be used before calling again)
static var [260]char newfile
var [32]char newext2
var ref char sext
var int n

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

global function extractext(ref char s,int period=0)ichar=		!EXTRACTEXT
!if filespec s has an extension, then return pointer to it otherwise return ""
!if s ends with ".", then returns "."
var ref char t,u

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

global function extractpath(ref char s)ichar=		!EXTRACTPATH
static var [0:260]char str
var ref char t
var int n

t:=s+strlen(s)-1		!t points to last char

while (t>=s) do
	switch t^
	when '\\','/',':' then		!path separator or drive letter terminator assume no extension
		n:=t-s+1			!n is number of chars in path, which includes rightmost / or \ or :
		memcpy(&.str,s,n)
		str[n]:=0
		return &.str
	endswitch
	--t
od
return ""			!no path found
end

global function extractfile(ref char s)ichar=		!EXTRACTFILE
var ref char t

t:=extractpath(s)

if t^=0 then			!s contains no path
	return s
fi

return s+strlen(t)		!point to last part of s that contains the file
end

global function extractbasefile(ref char s)ichar=		!EXTRACTBASEFILE
static var [0:100]char str
var ref char f,e
var int n,flen

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

global function addext(ref char s,ref char newext)ichar=		!ADDEXT
!when filespec has no extension of its own, add newext
var ref char sext

sext:=extractext(s,1)

if sext^=0 then						!no extension not even "."
	return changeext(s,newext)
fi

return s							!has own extension; use that
end

global function alloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
var ref void p

p:=malloc((n+1)*size)

if not p then
	abortprogram("Alloctable failure")
fi
return p
end

global function zalloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
var ref int p

p:=alloctable(n,size)

pcm_clearmem(p,(n+1)*size)
return p
end

global proc checkfreelists(ichar s)=
var ref wordp p,q
var int64 aa

for i:=2 to 2 do
	p:=freelist[i]

	while p do
		aa:=int64(p)
		if aa>0xffff'FFFF or aa<100 then
			CPL s,"FREE LIST ERROR",i,p,q
!			os_getch(); stop 1
		fi
		q:=p
		p:=ref wordp(int(p^))
	od

od
end

global function pcm_alloc32:ref void =		!PCM_ALLOC
var ref byte p

allocbytes:=32
!smallmemtotal+:=32

!if p:=ref byte(freelist[2]) then		!Items of this block size available
!	freelist[2]:=ref wordp((freelist[2])^)
!	if mem_check then addtomemalloc(ref int32(p),32) fi
!	return p
!fi

!No items in freelists: allocate new space in this heap block

return pcm_alloc(32)
end

global proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

!CPL "PCMFREE32"
smallmemtotal-:=32
if mem_check then removefrommemalloc(p,32) fi
!(ref wordp(p))^:=wordp(freelist[2])
(ref wordp(p))^:=wordp(int(freelist[2]))
freelist[2]:=p
end

global proc outbyte(filehandle f,int x)=
fwrite(&x,1,1,f)
end

global proc outword16(filehandle f,word x)=
fwrite(&x,2,1,f)
end

global proc outword(filehandle f,word x)=
fwrite(&x,4,1,f)
end

global proc outword64(filehandle f,word64 x)=
fwrite(&x,8,1,f)
end

global function myeof(filehandle f)int=
var int c

c:=fgetc(f)
if c=c_eof then return 1 fi
ungetc(c,f)
return 0;
end

global function pcm_smallallocz(int n)ref void =
var ref byte p

if (alloccode:=sizeindextable[n])=0 then
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	memset(p,0,n)
	return p
fi

memset(p,0,n)

return p
end

!global function pcm_fastalloc(int n)ref void =
global function pcm_smallalloc(int n)ref void =
var ref byte p

if (alloccode:=sizeindextable[n])=0 then
	alloccode:=1
fi
allocbytes:=allocupper[alloccode]

!No items in freelists: allocate new space in this heap block
p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	return p
fi

return p
end

global proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
var int newlen,oldlen
var ichar newptr

IF N=0 THEN CPL "N=0" FI

if n=-1 then
	n:=strlen(s)
fi

oldlen:=dest^.length

if oldlen=0 then				!first string
	dest^.strptr:=pcm_alloc(n+1)
	dest^.allocated:=allocbytes
	dest^.length:=n				!length always excludes terminator
	memcpy(dest^.strptr,s,n)
	(dest^.strptr+n)^:=0
	return
fi

newlen:=oldlen+n
if newlen+1>dest^.allocated then
!CPL "REALLOC",NEWLEN
	newptr:=pcm_alloc(newlen+1)
	memcpy(newptr,dest^.strptr,oldlen)
	dest^.strptr:=newptr
	dest^.allocated:=allocbytes
fi

memcpy(dest^.strptr+oldlen,s,n)
(dest^.strptr+newlen)^:=0

dest^.length:=newlen
end

global proc gs_init(ref strbuffer dest)=			!INITGENSTR
pcm_clearmem(dest,strbuffer.bytes)
end

global proc gs_free(ref strbuffer dest)=
if dest^.allocated then
	pcm_free(dest^.strptr,dest^.allocated)
fi
end

global proc gs_str(ref strbuffer dest,ichar s)=			!GENSTR
strbuffer_add(dest,s)
end

global proc gs_char(ref strbuffer dest,int c)=
var [16]char s

s[1]:=c
s[2]:=0

strbuffer_add(dest,&.s,1)
end

global proc gs_strn(ref strbuffer dest,ichar s,int length)=
strbuffer_add(dest,s,length)
end

global proc gs_strvar(ref strbuffer dest,s)=			!GENSTR
strbuffer_add(dest,s^.strptr)
end

global proc gs_strint(ref strbuffer dest,int64 a)=
strbuffer_add(dest,strint(a))
end

global proc gs_strln(ref strbuffer dest,ichar s)=		!GENSTRLN
gs_str(dest,s)
gs_line(dest)
end

global proc gs_strsp(ref strbuffer dest,ichar s)=
gs_str(dest,s)
gs_str(dest," ")
end

global proc gs_line(ref strbuffer dest)=
strbuffer_add(dest,"\w")
end

global function gs_getcol(ref strbuffer dest)int=
return dest^.length
end

global proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
var int col,i,n,slen
var [2560]char str
col:=dest^.length
strcpy(&.str,s)
slen:=strlen(s)
n:=w-slen
!CPL =slen,=w,=n
if n>0 then
	for i:=1 to n do
		str[slen+i]:=padch
	od
	str[slen+n+1]:=0
fi
gs_str(dest,&.str)
end

global proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
gs_leftstr(dest,strint(a),w,padch)
end

global proc gs_padto(ref strbuffer dest,int col, ch=' ')=
var int n
var [2560]char str

n:=col-dest^.length
if n<=0 then return fi
for i:=1 to n do
	str[i]:=ch
od
str[n+1]:=0
gs_str(dest,&.str)
end

global proc gs_println(ref strbuffer dest,filehandle f)=
(dest.strptr+dest.length)^:=0

if f=nil then
	println dest.strptr,,"\c"
else
	println @f,dest.strptr,,"\c"
fi
end

global function nextcmdparam(int &paramno, ichar &name, &value, ichar defext=nil)int=
static var int infile=0
static var ichar filestart=nil
static var ichar fileptr=nil
static var byte colonseen=0
var ref char q
var ichar item,fileext
var ichar rest
var int length
static var [300]char str

reenter::
value:=nil
name:=nil

if infile then
	if readnextfileitem(fileptr,item)=0 then		!eof
		free(filestart)								!file allocated via malloc
		infile:=0
		goto reenter
	fi
else
	if paramno>nsysparams then
		return pm_end
	fi
	item:=sysparams[paramno]
	++paramno

	length:=strlen(item)

	if item^='@' then		!@ file
		filestart:=fileptr:=cast(readfile(item+1))
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
elsif eqstring(fileext,"dll") then
	return (colonseen|pm_extra|pm_libfile)
fi
return (colonseen|pm_extra|pm_sourcefile)
end

function readnextfileitem(ichar &fileptr,&item)int=
var ref char p,pstart,pend
var int n
static var [256]char str

p:=fileptr

reenter::
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

	enddocase
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

global proc ipadstr(ref char s,int width,ref char padchar=" ")=
var int n

n:=strlen(s)
to width-n do
	strcat(s,padchar)
od
end

global function padstr(ref char s,int width,ref char padchar=" ")ichar=
static var [256]char str

strcpy(&.str,s)
ipadstr(&.str,width,padchar)
return &.str
end

global function chr(int c)ichar=
static var[8]char str

str[1]:=c
str[2]:=0
return &.str
end

global function cmpstring(ichar s,t)int=
	var int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function cmpstringn(ichar s,t,int n)int=
	var int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

global function cmpbytes(ref void p,q,int n)int=
	var int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function eqbytes(ref void p,q,int n)int=
	return memcmp(p,q,n)=0
end

global proc mseed(word64 a,b=0)=
seed[1]:=a
if b then
	seed[2]:=b
else
	seed[2] ixor:=a
fi
end

global function mrandom:int =
!return pure 64-bit int value, positive or negative -2**63 to +2**63-1
!(cast result for unsigned value)
	var word64 x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

global function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

global function mrandomint(int n)int=
!positive random int value from 0 to n-1
	return mrandomp() rem n
end

global function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
	var int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

global function mrandomreal:real =
!positive random real value from 0 to 0.999999999999999999891579782751449556599254719913005828857421875
!upper limit is (2**63-1)/(2**63)
	return real(mrandomp())/9223372036854775808.0
end

global function checkpackfile:ref byte=
!find out if this executable contains extra packed files
!return 1 or 0

var int a,offset,i,size
var [100]char name
var [300]char exefile
var ref byte packexeptr			!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
var int packexesize				!byte size
var ref char packfilename
var int packfilesize
var ref byte packfileptr

macro getfileint(data,offset)=(ref int32(data+offset))^

strcpy(&exefile[1],os_gethostname())
println "Attempting to open",&exefile
packexeptr:=readfile(&exefile[1])

if not packexeptr then
	cpl "Can't open",&exefile,&packexeptr
	stop
fi

packexesize:=rfsize
cpl "File read OK. Size",packexesize
!STOP

a:=getfileint(packexeptr,packexesize-int32.bytes)
if a<>'PCAK' then
	free(packexeptr)
	packfileptr:=nil
	return nil
fi

offset:=getfileint(packexeptr,packexesize-int32.bytes*2)

packfilename:=cast(packexeptr+offset)
offset+:=strlen(packfilename)+1
packfilesize:=getfileint(packexeptr,offset)
packfileptr:=packexeptr+offset+int32.bytes

return packfileptr
end
=== clibnew.m 32/35 ===
global type filehandle=ref void

importlib cstd=
!	clang function malloc	(wordm)ref void
	clang function malloc	(word64)ref void
	clang function realloc	(ref void, wordm)ref void
	clang proc     free		(ref void)
	clang proc     memset	(ref void, int32, wordm)
	clang proc     memcpy	(ref void, ref void, wordm)
	clang function clock	:int32
	clang function ftell	(filehandle)int32
	clang function fseek	(filehandle, int32, int32)int32
	clang function fread	(ref void, wordm, wordm, filehandle)wordm
	clang function fwrite	(ref void, wordm, wordm, filehandle)wordm
	clang function getc		(filehandle)int32
	clang function ungetc	(int32, filehandle)int32
	clang function fopen	(ichar,ichar="rb")filehandle
	clang function fclose	(filehandle)int32
	clang function fgets	(ichar, int, filehandle)ichar
	clang function remove	(ichar)int32
	clang function rename	(ichar, ichar)int32
	clang function getchar	:int32
	clang proc     putchar	(int32)
	clang proc     setbuf	(filehandle, ref byte)

	clang function strlen	(ichar)wordm
	clang function strcpy	(ichar, ichar)ichar
	clang function strcmp	(ichar, ichar)int32
	clang function strncmp	(ichar, ichar, wordm)int32
	clang function strncpy	(ichar, ichar, wordm)wordm
	clang function memcmp	(ref void, ref void, wordm)int32
	clang function strcat	(ichar, ichar)ichar
	clang function tolower	(int32)int32
	clang function toupper	(int32)int32
	clang function isalpha	(int32)int32
	clang function isupper	(int32)int32
	clang function islower	(int32)int32
	clang function isalnum	(int32)int32
	clang function isspace	(int32)int32
	clang function strstr	(ichar, ichar)ichar
	clang function atol		(ichar)intm
	clang function atoi		(ichar)int32
	clang function strtod	(ichar,ref ref char)real64

	clang function puts		(ichar)int32
	clang function printf	(ichar, ...)int32

	clang function sprintf	(ichar, ichar, ...)int32
!	clang function __mingw_sprintf	(ichar, ...)int32

	clang function sscanf	(ichar, ichar, ...)int32
	clang function scanf	(ichar, ...)int32

	clang function rand		:int32
	clang proc     srand	(word32)
	clang function system	(ichar)int32

	clang function fgetc	(filehandle)int32
	clang function fputc	(int32,  filehandle)int32
	clang function fprintf	(filehandle, ichar, ...)int32
	clang function fputs	(ichar,  filehandle)int32
	clang function feof		(filehandle)int32
	clang function getch	:int32
	clang function kbhit	:int32
	clang function _mkdir	(ichar)int32
	clang function mkdir	(ichar)int32
	clang function dummy	(real)real
	clang function strchr	(ichar,int32)ichar

	clang proc     _exit	(int32)
	clang proc     "exit"	(int32)
!	clang proc     `exit	(int32)
	clang function	pow		(real,real)real

	clang function	`sin	(real)real
	clang function	`cos	(real)real
	clang function	`tan	(real)real
	clang function	`asin	(real)real
	clang function	`acos	(real)real
	clang function	`atan	(real)real
	clang function	`log	(real)real
	clang function	`log10	(real)real
	clang function	`exp	(real)real
	clang function	`floor	(real)real
	clang function	`ceil	(real)real

	clang proc      qsort   (ref void, word64, word64, ref proc)

end

importlib cstdextra=
	clang function __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

global const c_eof		=-1
global const seek_set	= 0
global const seek_curr	= 1
global const seek_end	= 2
=== oswindows.m 33/35 ===
import clib
import mlib

const wm_destroy=2

type wt_word	= word16
type wt_wordpm	= word32
type wt_bool	= word32
type wt_dword	= word32
type wt_wchar	= word16
type wt_wcharpm	= word32
type wt_char	= byte
type wt_ichar	= ref char
type wt_ptr		= ref void
type wt_wndproc	= ref proc
type wt_handle	= ref void
type wt_int		= int32
type wt_uint	= word32
type wt_long	= int32
type wt_wparam	= wordm
type wt_lparam	= wordm
type wt_point	= rpoint

global record rsystemtime =
	wt_word year
	wt_word month
	wt_word dayofweek
	wt_word day
	wt_word hour
	wt_word minute
	wt_word second
	wt_word milliseconds
end

importdll windowsdlls=
!	windows function "VirtualAlloc"(wt_ptr, dint,wt_dword,wt_dword)wt_ptr
	windows function "GetStdHandle"(wt_dword)wt_handle
	windows function "GetConsoleScreenBufferInfo"(wt_handle,wt_ptr)int
	windows function "SetConsoleCtrlHandler"(wt_wndproc,int)int
	windows function "SetConsoleMode"(wt_handle,wt_dword)int
	windows function "CreateProcessA"(wt_ichar,wt_ichar,wt_ptr,wt_ptr, int,
						wt_dword, wt_ptr,wt_ichar,wt_ptr,wt_ptr)int
	windows function "GetLastError":wt_dword
	windows function "WaitForSingleObject"(wt_handle,wt_dword)wt_dword
	windows function "GetExitCodeProcess"(wt_handle,wt_ptr)int
	windows function "CloseHandle"(wt_handle)int
	windows function "GetNumberOfConsoleInputEvents"(wt_handle,wt_ptr)int
	windows function "FlushConsoleInputBuffer"(wt_handle)int
	windows function "LoadLibraryA"(wt_ichar)wt_handle
!	windows function "GetProcAddress"(wt_handle,wt_ichar)wt_wndproc
	windows function "GetProcAddress"(wt_handle,wt_ichar)ref void
	windows function "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	windows function "RegisterClassExA"(wt_ptr)wt_wordpm
	windows function "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)intm
	windows function "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	windows proc     "Sleep"(wt_dword)
	windows function "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	windows proc     "ExitProcess"(wt_uint)
	windows proc	 "PostQuitMessage"(wt_int)

!	windows proc	 "MessageBoxA"(wt_int,wt_ichar,wt_ichar,wt_int)

	windows proc	 "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	windows function "QueryPerformanceCounter"(ref int64)wt_bool
	windows function "QueryPerformanceFrequency"(ref int64)wt_bool

	windows function "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	windows function "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	windows proc     "GetSystemTime"(ref rsystemtime)
	windows proc     "GetLocalTime"(ref rsystemtime)

	windows function "GetTickCount":wt_dword
	windows function "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

end

record input_record =
	var wt_word	eventtype
	var word16	padding
		var wt_bool	keydown			!key event record (was inside 'Event' union in win32)
		var wt_word	repeatcount
		var wt_word	virtualkeycode
		var wt_word	virtualscancode
		union
			var wt_word unicodechar
			var wt_char asciichar
		end
		var wt_dword controlkeystate
end

record rspoint=(var int16 x,y)

record rsrect=
	var int16 leftx,top,rightx,bottom
end

global record rpoint =
	var wt_long x,y
end

record rconsole=
	var rspoint size,pos
	var word16 attributes
	var rsrect window
	var rspoint maxwindowsize
end

record rstartupinfo =
	var wt_dword	size
!.if $64bit
	var word32 dummy1
!.endif
	var wt_ichar	reserved
	var wt_ichar	desktop
	var wt_ichar	title
	var wt_dword	x
	var wt_dword	y
	var wt_dword	xsize
	var wt_dword	ysize
	var wt_dword	xcountchars
	var wt_dword	ycountchars
	var wt_dword	fillattribute
	var wt_dword	flags
	var wt_word		showwindow
	var wt_word		reserved2
!.if $64bit
	var word32 dummy2
!.endif
	var	wt_ptr		reserved4
	var wt_handle	stdinput
	var wt_handle	stdoutput
	var wt_handle	stderror
end

record rprocess_information =
	var wt_handle process
	var wt_handle thread
	var wt_dword processid
	var wt_dword threadid
end

record rwndclassex =
	var wt_uint		size
	var wt_uint		style
	var wt_wndproc	wndproc
	var wt_int		clsextra
	var wt_int		wndextra
	var wt_handle	instance
	var wt_handle	icon
	var wt_handle	cursor
!	var wt_handle	background
	var wt_handle	background
	var wt_ichar	menuname
	var wt_ichar	classname
	var wt_handle	iconsm
end

global record rmsg =
	var wt_handle	hwnd
	var wt_uint		message
!.if $64bit
	var word32		dummy1
!.endif
	var wt_wparam	wParam
	var wt_lparam	lParam
	var wt_dword	time
!.if $64bit
	var word32		dummy2
!.endif
	var wt_point	pt
end

!wt_word x
const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

var wt_handle hconsole, hconsolein

var input_record lastkey, pendkey
var int keypending			!whether pendkey contains a new key event detected by flushkbd

var ref function(ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

var int init_flag=0

global proc os_init=
var int i,count
var rconsole info

!general initialisation
hconsole:=GetStdHandle(u32(-11))
hconsolein:=GetStdHandle(u32(-10))

lastkey.repeatcount:=0
keypending:=0

!CPL "OSINIT"
SetConsoleCtrlHandler(nil,1)

SetConsoleMode(hconsole,1 ior 2)
!SetConsoleMode(hconsole,1 )

init_flag:=1

end

global function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
var wt_dword exitcode
var int status
var int cflags:=0

var rstartupinfo si
var rprocess_information xpi

memset(&si,0,si.bytes)
memset(&xpi,0,xpi.bytes)

switch newconsole
when 0 then cflags := NORMAL_PRIORITY_CLASS
when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
endswitch

si.size := rstartupinfo.bytes

status:=CreateProcessA( nil,
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
	println "Winexec error:",status
	return -1
end

WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
GetExitCodeProcess(xpi.process,&exitcode)

CloseHandle(xpi.process)
CloseHandle(xpi.thread)

return exitcode
end

global function os_execcmd(ichar cmdline, int newconsole=0)int =
var wt_dword exitcode
var int i,j,k

var rstartupinfo si
var rprocess_information xpi

memset(&si,0,si.bytes)
memset(&xpi,0,xpi.bytes)

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

global function os_getch:int=
var int k

k:=os_getchx() iand 255

return k
end

global function os_kbhit:int=
var wt_dword count
!os_init() unless init_flag

unless init_flag then os_init() end
!unless initflag then: os_init()

GetNumberOfConsoleInputEvents(hconsolein,&count)
return count>1
end

global proc os_flushkeys=
FlushConsoleInputBuffer(hconsolein)
end

global function os_getconsolein:ref void=
return ref void(hconsolein)
end

global function os_getconsoleout:ref void=
return ref void(hconsole)
end

global function os_proginstance:ref void=
abortprogram("PROGINST")
return nil
end

global function os_getdllinst(ichar name)u64=
var wt_handle hinst

hinst:=LoadLibraryA(name)
return cast(hinst)
end

global function os_getdllprocaddr(intm hinst,ichar name)ref void=
return GetProcAddress(cast(int(hinst)),name)
end

global proc os_initwindows=
os_init()
os_gxregisterclass("pcc001")
end

global proc os_gxregisterclass(ichar classname)=
const idcarrow=32512
var rwndclassex r
static var byte registered

if registered then
	return
fi

memset(&r,0,r.bytes)
r.size:=r.bytes
r.style:=8 ior 32		!CS_DBLCLKS | CS_OWNDC
r.wndproc:=cast(&mainwndproc)
!r.wndproc:=&xmainwndproc
!r.wndproc:=&cmainwndproc
r.instance:=nil

r.icon:=nil		!loadicon(proginstance,"SCW32")
r.cursor:=LoadCursorA(nil,ref void(idcarrow))		!IDC_ARROW)
r.background:=cast(15+1)					!COLOR_BTNFACE+1
r.menuname:=nil
r.classname:=classname
r.iconsm:=nil	!loadicon(proginstance,"SCW32")

if RegisterClassExA(&r)=0 then
	println classname,GetLastError
	abortprogram("Registerclass error")
end
registered:=1
end

global callback function mainwndproc (\
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)intm=
var rmsg m
var int i,result
var intm l
static var int count=0

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

!callback proc timerproc(wt_handle hwnd, int msg, id, time)=
proc timerproc(wt_handle hwnd, int msg, id, time)=
println "TIMERPROC"
end

global proc os_setmesshandler(ref void addr)=
wndproc_callbackfn:=addr
end

global function os_getchx:int=
!Q! function os_getchx_c:int
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
var int count
var int charcode,keyshift,keycode
var int altdown,ctrldown,shiftdown,capslock

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

!CPL "CHARCODE2=%d %X\n",charcode,charcode
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

global function os_getos=>ichar=
if $targetbits=32 then
	return "W32"
else
	return "W64"
fi
end

global function os_gethostsize=>int=
return $targetbits
end

global function os_shellexec(ichar opc, file)int=
return system(file)
end

global proc  os_sleep(int a)=
Sleep(a)
end

global function os_getstdin:filehandle =
return fopen("con","rb")
end

global function os_getstdout:filehandle =
return fopen("con","wb")
end

global function os_gethostname:ichar=
static var [300]char name
static var int n

GetModuleFileNameA(nil,&.name,name.bytes)
strcat(&.name,"/")
return &.name
end

global function os_getmpath:ichar=
return F"C:\m\"
end

global proc os_exitprocess(int x)=
stop x
!ExitProcess(x)
end

global function os_clock:int64=
return clock()
end

global function os_getclockspersec:int64=
return 1000
end

global function os_iswindows:int=
return 1
end

global function os_filelastwritetime(ichar filename)int64=
var wt_handle f;
var int64 ctime,atime,wtime;

if filename=nil then				!used to test whether supported
	return 1
fi

f:=CreateFileA(filename,0x80000000,1,nil, 3,3,nil);
if int64(f)=-1 then
	return 0
fi

GetFileTime(f,&ctime,&atime,&wtime);
CloseHandle(f);

return wtime;
end

global proc os_getsystime(ref rsystemtime tm)=
GetLocalTime(tm)
end

global proc os_messagebox(ichar s,t)=
messageboxa(0,s,t,0)
end

global function os_hpcounter:int64=
var int64 a

queryperformancecounter(&a)
return a

end

global function os_hpfrequency:int64=
var int64 a

queryperformancefrequency(&a)
return a

end

global proc os_peek=
var int ticks
static var int lastticks
var [100]byte m
	ticks:=GetTickCount()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end
=== oswindll.m 34/35 ===
import clib
import mlib

!IMPORT OSWINDLLC

global function os_calldllfunction(ref proc fnaddr,
		int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
	var word64 a
	var real64 x
	var int oddstack, nextra, pushedbytes

!	return os_calldllfunctionc(fnaddr,retcode,nargs,args,argcodes)

	oddstack:=nextra:=0

	assem
		test astack,8
		jz L100
		mov byte [oddstack],1
L100:
	end

	if oddstack then
		if nargs<5 then
			nextra:=5-nargs
		elsif nargs.even then
			nextra:=1
		fi

	else
		if nargs<4 then
			nextra:=4-nargs
		elsif nargs.odd then
			nextra:=1
		fi
	fi

	pushedbytes:=(nextra+nargs)*8

!RETURN 0

!CPL "D4"
	to nextra do
		assem
			push 0
		end
	od
!CPL "D5"

	for i:=nargs downto 1 do
		a:=args^[i]					!get generic 64-bit value to push
		assem
			push word64 [a]
		end
	od

!CPL =NEXTRA+NARGS,=pushedbytes,=oddstack

!load first 4 args to registers; this first version will blindly load 4 args
!(even if there are less) to both integer and xmm registers. Should be int/pointer
!types to integer regs; float types to xmm; and variadic to both
	assem
		mov D10,[Dstack]
		movq XMM0,[Dstack]
		mov D11,[Dstack+8]
		movq XMM1,[Dstack+8]
		mov D12,[Dstack+16]
		movq XMM2,[Dstack+16]
		mov D13,[Dstack+24]
		movq XMM3,[Dstack+24]
	end

	if retcode='I' then
		a:=((ref function:int64(fnaddr))^())
		asm add Dstack,[pushedbytes]
		return a
	else
		x:=((ref function:real64(fnaddr))^())
		asm add Dstack,[pushedbytes]
		return word64@(x)
	fi
end	

global function os_pushargs(ref[]word64 args, int nargs, nextra,
					ref proc fnaddr, int isfloat)word64=
!	a:=os_pushargs(&wordargs, na, nextra, fnaddr, retttype=tp_r64)
!implements central part of 'callapplproc' which needs to be in asm
	var word64 a
	var real64 x

CPL "PUSH ARGS",NARGS, NEXTRA

	to nextra do
		asm	push 0
	end

CPL "PUSH ARGS2"
	for i to nargs do
		a:=args[i]
		asm push word64 [a]
	od
CPL "PUSH ARGS3"

	if isfloat then
		x:=((ref function:real64(fnaddr))^())
		a:=int64@(x)
	else
		a:=((ref function:int64(fnaddr))^())
	fi

	return a
end
=== mm_help.txt 35/35 ===
'MM' M Compiler Generating x64 native code - Windows Version

Whole-program compiler builds entire program from the lead module
into a executable file.

    mm main              # Create main.exe from lead module main.m

    mm main.m            # Same (.m extension is default)

    mm -c main           # Create single-file main.asm intermediate ASM

Options:

    -c                    # Generate only intermediate ASM file only
    -exe                  # Generate .exe executable file
    -obj                  # Generate single .obj object file

    -out:file             # Name of output file 

    -run                  # For -exe mode only: run resulting executable

    @file                 # Read options from file

Example:

     mm -run prog : abc def

Any parameters for the new program must follow " : " (spaces needed).
=== end ===
