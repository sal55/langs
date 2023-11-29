!global const fixbytecodes=1		!convert bytecodes to handler addresses

!global int dispatchtype=fn_dispatch
global byte dispatchtype=asm_dispatch
global int hasbytecodes=1			!depends on dispatchcode

global type unit      	= ref unitrec
global type object    	= ref objrec
global type symbol    	= ref strec
!global type strobject 	= ref stringobjrec
global type variant   	= ref varrec
global type ifile   	= ref filerec
global type isubprog  	= ref subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global const hasrefmask = 0x100
global const varsize    = varrec.bytes

global record packfieldrec =
	object structobj			!owner record
	ichar name
	int32 packmode				!index into tables
	int32 offset				!byte offset
	int32 size					!size
	int32 length
end

global record procrec =			!used as linked list
	symbol def
	ref procrec nextproc
end

global record userxrec =
	symbol owner
	ref int16 pmode
	ref userxrec nextmode
end

global record strec =
	ichar name				! name (likely shared pointer with generic entry)
	symbol	owner
	symbol	deflist			! first child name
	symbol	deflistx		! points to last child

	symbol	nextdef			! next name in this list
	symbol	nextdupl		! next instance that shares the same name
	symbol	firstdupl		! first or generic name entry
!	union
		symbol alias		! used for aliasid
!		symbol captured		! localid: captured local in containing scope
!	end

	union
		u64 a
		ref int pcaddress		!procs/labels
		variant varptr			!statics
		ichar truename			!dll procs
		symbol atfield			!fields
		int labelno				!proc/label label# before fixup
	end
	union
		u64 b
		unit code				!proc body/initdata
		ref symbol topfieldlist		!structs; point to block of ttlength[mode] top fields
	end
	union
		u64 c
		struct
			int32 index			!frame/param/dllproc/enum/(const)
			int32 capindex		!localid index
		end
	end
	union
		u64 d
		struct
			int16 nparams		!procs/dllprocs
			int16 nlocals		!procs
!			int16 ncaptured		!anonprocs
		end
		struct
			int16 nfields		!records/structs
			int16 maxalign		!structs
!			int32 fieldoffset
			int16 fieldoffset
			int16 baseclassindex		!into baseclass tables
		end
		int genfieldindex		!generic
	end

	word16	subcode
	byte	moduleno
	byte	subprogno
	int16	mode
	int16	hint				!0/tvoid, or hinted mode when .mode=tvar
	u16		flags: (isglobal:2, isimport:1, mstatic:1, misfunc:1, mbyref:1,
							menumx:1,
							moptional:1,  mvarparams:1, isframe:1,
							iscaligned:1,initcode:2)
	byte	forindex		!1 when var is a for-loop index

	byte	symbolcode
	byte	nameid			! generic/static/proc etc

	byte	mutable			! will be 1 for variables; 0 for modules procs, label etc
	byte	namelen			! helps makes lookups faster
	byte	procfixed		! 1 when procs have been fixedup
end

global record lexrec =		!should be 32-byte record
	union
		int64 value				!64-bit int
		real xvalue				!64-bit float
		word64 uvalue			!64-bit word
		ichar svalue			!pointer to string or charconst (not terminated)
		ref strec symptr			!pointer to symbol table entry for name
	end

	int32 pos: (sourceoffset:24, moduleno:8)

	int16 symbol
	int16 subcode
end

global record uflagsrec =
	[7]byte	codes
	byte	ulength
end

global record fieldrec =
	ichar name
	int16 recordtype
	int16 fieldtype
	int32 fieldoffset
end

global record unitrec =
	union
		struct
			int16 tag
			union
				byte elemtype			!for array constructors
			end
			union
				byte nparams
				byte enumindex
			end
			int32 pos: (sourceoffset:24, moduleno:8)
		end
		ref void word1
	end

	unit nextunit

	union
		unit a
		symbol def
		symbol labeldef
		int64 value
		word64 uvalue
		real64 xvalue
		ichar svalue
		int64 range_lower
		int pclopcode
	end

	union
		unit b
		int64 range_upper
		int64 slength
		int16 mode
		[4]byte cmpgenop
		struct
			int32 length
			int32 lower
		end
		int64 index		!of enum name; or host index; or could be expr
	end
end

global lexrec nextlx
global lexrec lx
!global const targetbits=64

!global const maxsearchdirs=10
!global [maxsearchdirs]ichar searchdirs
!global int nsearchdirs=0

!global [5]ichar hostdirs
!global int nhostdirs

global int qpos
global int pcerrorpos
global ref filerec pcerrormodule

global const stacksize=70000
global [stacksize]varrec varstack
global variant sptr
global variant stacklimit
global ref byte frameptr

global ref int pcptr

global int stopped

global symbol stprogram			!root of global symbol table
global symbol stmodule			!main module
global symbol stsubprog
global symbol stcurrmodule		!current module during parse, name resolve, code gen
global symbol stcurrproc		!current proc during parse, rx/cocde, or
								! set to stcurrmodule when outside a proc
global ifile currmodule			!set via stcurrmodule.moduleno

global int debug

global int inproc

!Errors
!global [256]char errorline,errorpointer

global record locrec=
	isubprog sp             !owner sp
	ifile pm                !owner module
	symbol def				!if not nil, then containing proc, module etc
	ichar startline			!point to start of line in source
	int lineno              !line number within module
	int column				!if not zero, then column number
end

!Genfield Tables

global record genfieldrec=
	symbol def
	ref genfieldrec nextdef
end

global const maxgenfield=1000
global [maxgenfield]ref genfieldrec genfieldtable
global int ngenfields

global const maxlibfile=50
global const maxdllproc=2000

global int nlibfiles
global [maxlibfile]symbol libtable
global [maxlibfile]byte libtypes
global [maxlibfile]u64 dllinsttable		!instance table

global int ndllprocs
global [maxdllproc]symbol dllproctable
global [maxdllproc]byte dllproclibindex				!lib that dll proc belongs to
global [maxdllproc]ref void dllprocaddr			!pointer to external dll proc

global byte usebundled	 = 1			!whether to use internal libs

global enumdata []ichar dispatchnames=
	(lab_dispatch,		"-lab"),
	(sw_dispatch,		"-sw"),
	(fn_dispatch,		"-fn"),
	(debug_dispatch,	"-debug"),
	(fdebug_dispatch,	"-fdebug"),
	(asm_dispatch,		"-asm"),
end

global const int maxqparam=32
global int nqparams
global [maxqparam]ichar qparamtable

global ichar err_message
global varrec err_var1, err_var2
global ref int err_pcptr

global ref int stopseq		!point to a 'stop 0' sequence
global ref int raiseseq		!point to a sequence of several 'raise' cmdcodes

global ref procrec proclist, proclistx
global int nproclist

global ref proc pcl_callbackfn=nil	!address of *PCL* function (pcdata address)

global [0..255]object chrtable		!remember single-character objects

global byte fnosys
global byte fverbose

global [0:256]int16 baseclasstable
global [0:256]ref strec baseclassdef
global int nbaseclasses

global int lastretindex

global const maxsubprog=30
global const maxmodule=200

global record filerec=
	ichar	name				!module name and base filename ("<str>" is anon)
	ichar	path				!path where file resides
	ichar	filespec			!full file path
	ichar	text				!pointer to source text, 0-terminated
	int		size				!source file size includes terminator

	byte	isstring			!1 if a string rather than a file
	byte	issyslib			!1 if a system module
	byte	issupport			!1 if a support file (strinclude); MAY BE STORED ELSEWHERE
	byte	compiled			!1 if compiled

!	int16	subprogno
	byte	subprogno
	byte	islead				!1 if lead module in sp
	union
		int16	moduleno			!useful if using pointer to a source rec
		int16	fileno
	end

	unit	ast					!ast for module-level code

	ref int	pcstart				!nil, or points to generated bytecode for whole module
	ref int	pcend				!points to last allocated int
	int		pcsize				!pcl size as number of allocated ints (some spare)
	ref i32	pcsrcstart			!each entry is source-pos info (char offset into org source)

	union
		symbol	stmodule
		symbol	def
	end

	symbol	stsubprog
!	symbol	stmacro

	symbol	startfn				!nil, or st entry of start()
	symbol	mainfn				!nil, or st entry of main()
end

global record subprogrec =
	ichar name
	ichar path
	ichar filespec
	int16 firstmodule			!1st is lead module
	int16 lastmodule			!always first..lastmodule
	int16 compiled				!1 if compiled
	byte issyslib
	byte subprogno
end

global [0..maxmodule]ifile	modules
global [maxsubprog]isubprog	subprogs

global int nmodules
global int nsubprogs

global record varrec =
	union
		struct
			union
				struct
					byte	tag
					byte	hasref
					byte	bitoffset
					union
						byte	bitlength		!for refbit/tbit: 0=1 bit, N=bitfield
						byte	exceptiontype
						byte	genmarker		!1 means is a generator used as a marker
					end
				end
				word32		tagx
			end
			union
				word32 		elemtag
				word32 		frameptr_low
				struct
					i16		frameoffset
					i16		nexceptions
				end
			end
		end
		i64 dummy: (skip:16, range_lower:48)
	end
	union
		int64		value
		real64		xvalue
		word64		uvalue
		word64		range_upper
		object		objptr				!objects where hasref=1
		variant		varptr				!for refvar
		ref byte	ptr					!for refproc etc
		symbol		def					!for tsymbol
		ref int		retaddr
	end
end

export record objrec =
!1st 8 bytes
	word32 refcount
	struct
		byte flags: (lower:1, mutable:1, bittag:2)
		byte objtype
		union
			u16 elemtag
			u16 usertag
			u16 itertag
			struct
				byte bitoffset
				byte indexoffset
			end
			i16 lower16
!			i16 iterended		!0/1 = running/ended
		end
	end

!second 8 bytes (and end of short objects)
	union
		struct
			union
				int64		value
				real64		xvalue
				word64		uvalue
				ichar		strptr
				variant		varptr
				variant		genstack
				ref byte	ptr
				ref[0:]elemtype num
				word64 b
				ref int		retaddr
			end

!3rd 8 bytes
			union
				int64 length
				int64 lower64
				struct
					word32 rows
					word32 columns
				end
				word64 c
				ref byte frameptr
!				symbol		stgen
				struct
					int32 iterpos
					int32 iterupper
				end
			end

!4th 8 bytes (and end of long objects)
			union
				int64 alloc64				!object/item counts, not bytes
				object objptr2
				struct
					int16 neg
					int16 numtype
					int32 expon
				end
				struct
					word32 alloc32
					word32 dictitems
				end
				struct
					u16		genstacksize		!in varrecs
					byte	ngenparams			!params to gen func
				end
				word64 d
			end
		end
		[24]byte bignumdescr
	end
end

global [pclnames.lwb..pclnames.upb]int pclcounts

global record stringrec=
	ichar svalue
	int length
end

global int nalllines

global const devdir = "c:/qx/"

!QA files
global const maxqafile=100
global [maxqafile]ichar qafilenames
global [maxqafile]ichar qatext
global [maxqafile]int qasize
global int nqafiles					!non-0 means qa directory in use.

global enumdata []ichar optionnames, []ref byte optionvars, []byte optionvalues =
	(load_sw,		"load",			&runcode,			load_cc),
	(parse_sw,		"parse",		&runcode,			parse_cc),
	(names_sw,		"names",		&runcode,			names_cc),
	(gen_sw,		"gen",			&runcode,			gencode_cc),
	(fixup_sw,		"fixup",		&runcode,			fixup_cc),
	(run_sw,		"run",			&runcode,			run_cc),

	(ast1_sw,		"ast1",			&fshowast1,			1),
	(ast2_sw,		"ast2",			&fshowast2,			1),

	(pcl1_sw,		"pcl1",			&fshowpcl1,			1),
	(pcl2_sw,		"pcl2",			&fshowpcl2,			1),
	(pcl3_sw,		"pcl3",			&fshowpcl3,			1),

	(allsp_sw,		"allsp",		&fallsp,			1),

	(st_sw,			"st",			&fshowst,			1),
	(stflat_sw,		"stflat",		&fshowstflat,		1),
	(types_sw,		"types",		&fshowtypes,		1),
	(showmodules_sw,"modules",		&fshowmodules,		1),

	(fn_sw,			"fn",			&dispatchtype,		fn_dispatch),
	(asm_sw,		"asm",			&dispatchtype,		asm_dispatch),
	(debug_sw,		"debug",		&dispatchtype,		debug_dispatch),
	(fdebug_sw,		"fdebug",		&dispatchtype,		fdebug_dispatch),
	(sw_sw,			"sw",			&dispatchtype,		sw_dispatch),

	(opt_sw,		"opt",			&foptimise,			1),
	(asmopt_sw,		"asmopt",		nil,				0),

	(ext_sw,		"ext",			&usebundled,		0),
	(qa_sw,			"qa",			&fwriteqa,			1),
	(qas_sw,		"qas",			&fwriteqa,			2),

	(verbose_sw,	"v",			&fverbose,			1),
!	(time_sw,		"time",			&fshowtime,			1),

	(nosys_sw,		"nosys",		&fnosys,			1),
end
