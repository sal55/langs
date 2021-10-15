import clib
import cc_tables
import* cc_pcl

global type unit = ref unitrec
global type symbol = ref strec

global const maxmodule=200
global const maxlibfile=200
global const maxsourcefile=600

global record tokenrec = 		!should be 32-byte record
	union
		int64 value				!64-bit int
		real xvalue				!64-bit float
		word64 uvalue			!64-bit word
		ref char svalue			!pointer to string or charconst (not terminated)
		ref strec symptr		!pointer to symbol table entry for name
	end
	ref tokenrec nexttoken

	union
		struct
			byte subcode
			byte flags
		end
		word16 subcodex
	end
	byte symbol
	byte fileno

	word32 lineno

	int32 length					!length of name/string/char
	union
		int32 numberoffset		!offset of numeric token within file[fileno]
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

global record attribrec =		!keep this 16 bytes
	byte ax_static				!0 or 1
	byte ax_equals					!0 or 1 if = used (static/frame vars only)
	byte ax_varparams				!0 or 1	
	byte ax_used				!0 or 1	
	byte ax_forward				!0 or 1: 1 means forward decl of label or function
	byte ax_frame				!0 or 1: 1 when frameid/paramid
	byte ax_autovar				!0 or 1: 1 when an autovar with "$" in the name
	byte ax_nparams				!no. formal params for procid/dllprocid

	byte ax_callback			!1 when proc is is a callback function
	byte ax_moduleno
	byte ax_loop				!1 if a loop label
	union
		byte ax_align				!1, 2, 4, 8; max align for struct/union
		byte ax_dllindex		!for dllproc: which dll in dlltable
		byte ax_extmodno		!for proc call chains: module no of proc owner
		byte ax_flmacro			!function-like macro; used when no params
	end
end

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
	union
		psymbol pstdef
!		ref void mclcode
!		ref void opnd
	end

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
		ref void address
		int offset
		byte oldsymbol				!for #define/#undef on keyword
	end

	word32 lineno
	int32 index					!enum/label index
	union
		struct
			word16 blockno
			word16 namespace				!experimental: set to namespaces[.nameid]
		end
		word32 nsblock						!combined block no and namespace
	end
	int16 subcode
	int16 mode

	byte namelen
	byte symbol
	byte nameid
	byte scope		!linkage type

	attribrec attribs
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
		int32 offset				!for j_dot
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
	byte simple
	byte fileno
	union
		byte callconv			!calling convention code for callfn/callproc, +128 if variadic
		byte convmem			!0, or 1 if conversion source is in memory
		byte isstrconst			!for string consts: initialised with "..."
	end
	union
		byte strarray			!for string consts: when idata type is char[] rather than char*
		byte convtomem			!0, or 1 if conversion result is to be stored to memory
	end
	byte iswstrconst
	byte spare1
	word16 spare2
!	word16 memtype
!	word32 spare4
end

global record modulerec =
	ichar name
	ref strec stmodule
	int fileno
	ichar asmstr
	ichar mhdrstr
	[maxmodule]byte importmap
end

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
!global const int maxtype=10'000
global const int maxtype=20'000

global int ntypes
global int ntypesreset

!global [0:maxtype]ichar	ttname
global [0:maxtype]ref strec	ttnamedef
global [0:maxtype]int	ttbasetype			!basic t-code
global [0:maxtype]int	ttlength			!0, or array length
global [0:maxtype]byte	ttconst				!1 when const
global [0:maxtype]byte	ttrestrict			!1 when restrict used
global [0:maxtype]byte	ttvolatile			!1 when volatile used
global [0:maxtype]int	ttusertype			!0, or index of struct/union/enum type
global [0:maxtype]int	tttarget			!pointer target or array elem type
global [0:maxtype]int	ttreftype			!0, or index of type that is a pointer to this one
global [0:maxtype]int	ttconsttype			!0, or index of type that is a const version of this oneointer to this onee
global [0:maxtype]int	ttsize				!byte size
global [0:maxtype]int	ttbitwidth			!bit in basic type (not arrays/structs)
global [0:maxtype]byte	ttisref
global [0:maxtype]ref paramrec ttparams		!for modes involving function pointers
global [0:maxtype]ref strec tttypedef

global int trefchar							!set to to char* type
global int trefwchar						!set to to wchar* type

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]ichar inputfiles
global [0..maxlibfile]ichar libfiles
global [0..maxsourcefile]ichar sourcefilenames
global [0..maxsourcefile]ichar sourcefilepaths
global [0..maxsourcefile]ichar sourcefiletext
global [0..maxsourcefile]int32 sourcefilesizes

global [0..maxmodule]ichar automodulenames		!auto modules
global int nmodules
global int nautomodules
global int nsourcefiles
global int ninputfiles
global int nlibfiles

global int currmoduleno				!used when compiling modules
global ref modulerec currmodule

global const maxsearchdirs=20
global const maxincludepaths=20

global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0
global [maxincludepaths]ichar includepaths
global int nincludepaths=0

global ref strec stprogram		!root into the symbol table
global ref strec stmodule		!main module

global filehandle logdev		!dest for diagnostics and output of tables
global int dologging=0			!1 when any diags requested
global int optflag=0			!1=stdoptimise; 0=disabled

global const sourceext="c"
global ref unitrec nullunit

global int fverbose=0		!whether to display message for each pass
global int fquiet=0
global int fshownames=0		!whether [dframe-8] or [dframe+a]
!global int fshownames=1		!whether [dframe-8] or [dframe+a]
global int fshowincludes=0
global int fautomodules=0

global int fmodern=1

global int foptimise=0		!whether to generate optimised j-codes

global int wintarget=1		!one of these three is true
global int lintarget=0
global int nostarget=0
global int clineno=0		!set in codegen scanner

global int fastasm=0		!1 to pass asm source via memory
global int flinesplicing=0	!1 to deal with \ line continuations
!global ichar assemsource=nil
!global [maxmodule]ichar assemsources

global tokenrec lx				!provides access to current token data
global tokenrec nextlx

!global int tlex=0		!timing

global int debug=0

global int hstsize	= 16384

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

global int dointheaders=1				!allow internal std headers
global ichar dheaderfile=nil			!result of -d:file.h switch

global int structpadding=1
global int callbackflag=0

global int slineno,sfileno

global ichar oemname="BCC"

GLOBAL INT NLOOKUPS
GLOBAL INT NCLASHES
GLOBAL INT NMIXED
GLOBAL INT NALLPROCS
