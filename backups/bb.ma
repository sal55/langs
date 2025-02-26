=== MA 16 ===
=== bb.m 0 0 1/16 ===
!project =
	module qq_cli
!	module qq_api

	module qq_decls
!	module qq_dicts

!	module qq_jhandlers
!!
!	module qq_khandlers
!	module qq_host
	module qq_lex
	module qq_lib
	module qq_modules
	module qq_names

	module qq_parse
	module qq_PCLTABS
	module qq_pclgen
	module qq_pcllib
	module qq_resolve

!	module qq_syslibs
	module qq_syslibsdummy

	module qq_tables

	module qq_show
!	module qq_dummyshow

	module qq_showpcl
!	module qq_showpcldummy
!
!	module qq_vars
!end
=== qq_cli.m 0 0 2/16 ===
global const syslibname="sysp"
!global const syslibname="minsys"

global enumdata []ichar runnames =
	(load_cc,		$),
	(parse_cc,		$),
	(names_cc,		$),
	(gencode_cc,	$),
!	(optim_cc,		$),
	(fixup_cc,		$),
	(run_cc,		$),
end

global byte fshowpcl1
global byte fshowpcl2
global byte fshowpcl3
global byte fshowast1
global byte fshowast2
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte foptimise=0
global byte fwriteqa			!0, 1 or 2
global byte fshowmodules
global byte fallsp

global byte runcode  = run_cc

global ichar sourcestr

global ichar inputfile

global const maxstatic=11000
!global [maxstatic]variant statictable
global [maxstatic]symbol staticdefs
global int nstatics

global const maxproc=11000				!used for fixups
!global const maxproc=50000
global [maxproc]ref int proctable
global [maxproc]symbol procdefs
global int nprocs

ref strbuffer pclstr


int cmdstartindex

proc main=
	ichar source
	int i,nnames,t,tstart, stopcode
	unit p

CPL =PCLREC.BYTES

	initdata()

	getinputoptions()

!	if fverbose then
!		println dispatchnames[dispatchtype],"Opt:",foptimise
!	fi

!TESTPCL()
!
!STOP

	readqabundle()
	loadsyslib()

	compile_sp(inputfile)

	if fallsp then
		if fshowast1 and runcode=parse_cc then showast(nil, "AST1") fi
		if fshowast2 and runcode>parse_cc then showast(nil, "AST2") fi
		if (fshowpcl1 or fshowpcl2) and runcode=gencode_cc then showpcl(nil, 1) fi
		if fshowpcl3 and runcode=fixup_cc then showpcl(nil, 3) fi
	fi

!run the stack of sps (laters sps will be run as they are compiled)

	writeqafile()

!	for i to nsubprogs do
!		stopcode:=runprogram(subprogs[i])
!	od
INT TT:=CLOCK()
	stopcode:=runqprogram(subprogs[nsubprogs])
TT:=CLOCK()
IF RUNCODE=RUN_CC THEN CPL "RUNTIME:",TT;cpl FI
	showlogfile()

	stop stopcode
end

proc getinputoptions=
	int paramno,pmtype
	ichar name,value
	ichar appstr, appname
	ref function:ichar fnaddr

!fnaddr will be nil unless a built-in app exists
	fnaddr:=findfunction("getbuiltin_app")

	paramno:=1

	while pmtype:=nextcmdparamnew(paramno,name,value,"q") do
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
			if fnaddr then				!treat as data
				--paramno
				exit
			fi
			inputfile:=pcm_copyheapstring(name)
			exit				!leave any other files to app being run
		esac
	od

	if fnaddr then
		appstr:=fnaddr()
!		dobuiltin_app(appstr, appname)
LOADERROR("DO BUILT-IN")

!		dobuiltin_app(appstr)
	elsif not inputfile then
		println "Q5.2 Interpreter"
		println "Usage:"
		println "	",,sysparams[1],"filename[.q]"
		stop
	fi

	if dispatchtype in [debug_dispatch,fdebug_dispatch] then
		hasbytecodes:=1
	else
		hasbytecodes:=0
	fi

	cmdstartindex:=paramno

	setcli(cast(&cmdparams[cmdstartindex]),ncmdparams-cmdstartindex+1)
end

proc do_option(int sw, ichar value)=
	ref byte p

	p:=optionvars[sw]
	if p then
		p^:=optionvalues[sw]
		return
	fi

	case sw
	when asmopt_sw then
		foptimise:=1
		dispatchtype:=asm_dispatch
	esac

end

global proc compile_sp(ichar filename, source=nil)=
	ichar qafile
	isubprog sp
	int a, b

INT TT:=CLOCK()
	sp:=loadsp(filename, source)

	if runcode<parse_cc then return fi

	a:=sp.firstmodule
	b:=sp.lastmodule

	for m in a..b do
		parsemodule(modules[m])
	od
	fixusertypes()
	if fshowast1 and not fallsp then showast(sp,"AST1") fi

	return when runcode<names_cc

	tx_typetable()

	for m in a..b do
CPL "RESOLVE:",M
		rx_module(modules[m])
	od
	if fshowast2 and not fallsp then showast(sp,"AST2") fi

	return when runcode<gencode_cc
!
	for m in a..b do
		gencodemodule(sp, m)
	od

	if fshowpcl1 and not fallsp then showpcl(sp,1) fi

!	if foptimise and dispatchtype=asm_dispatch then
!		for m in a..b do
!			optimise_module(m)
!		od
!		if fshowpcl2 and not fallsp then showpcl(sp,2) fi
!	fi

!*!	fixup_sp(sp)

	resetcompiler()
end

proc setcli(ref []ichar cmds, int ncmds)=
	for i to ncmds do
		setcmdparam(i,cmds[i])
	od
end

proc writeqafile=
	[300]char filename
	[maxmodule]ifile sflist
	filehandle f
	int offset, nfiles, fileno
	ifile pm
	int leadmod

	if not fwriteqa then
		return
	fi
	strcpy(filename, changeext(inputfile,"qa"))

!first build a table of source files to be o/p
	nfiles:=0

	LEADMOD:=SUBPROGS[NSUBPROGS].FIRSTMODULE

	SFLIST[++NFILES]:=MODULES[LEADMOD]

	for i to nmodules WHEN I<>LEADMOD do
		pm:=modules[i]


		if pm.issyslib and fwriteqa=1 then		!no syslibs
			nextloop
		fi
		sflist[++nfiles]:=pm
	od

	if nfiles=0 then loaderror("QA:no files") fi

	f:=fopen(filename,"wb")
	if not f then loaderror("Can't create qa file #",filename) fi

	println "Writing ",filename
	fprintln @f,"=== QA # ===",nfiles

	for i to nfiles do
		pm:=sflist[i]

		fprintln @f,"=== #.q # # #/# ===",
			pm.name, pm.issyslib, pm.issupport,i, nfiles

		offset:=getfilepos(f)
		writerandom(f,cast(pm.text),offset,pm.size)
	od

	println @f,"=== END ==="

	for i to nfiles do
		fprintln @f,"# #.q",i,sflist[i].name
	od

	fclose(f)
	stop
end

proc initdata=
	lexinit()
!	inithostlib()

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)

	os_initwindows()

!	if dispatchtype=asm_dispatch then
!		initjhandlers()
!	fi

	firstusertype:=tlast+1

	deletetempfiles()

end

function runqprogram(isubprog sp)int=
	ref proc fnptr
	int cmd,SS
!	ref int pcxstart

	return 0 when runcode<run_cc
CPL "RUNQ NOT READY"

!
!	if fverbose then
!		println dispatchnames[dispatchtype],"Opt:",foptimise
!	fi
!
!	sptr:=&varstack[1]
!	stacklimit:=&varstack[stacksize-100]
!	pcxstart:=pcptr:=modules[sp.firstmodule].pcstart
!	pcerrorpos:=0
!	stopped:=0
!
!!	disploop()
!
!	return sptr.value
0
end

proc loadsyslib=
	[300]char str

!*!	setcmdmap()

	if fnosys then return fi

	if not fsyslibs then usebundled:=0 fi

	if usebundled then				!bundled sys files
		compile_sp(syslibname+".q")
	else
		strcpy(str, devdir)
		strcat(str, syslibname+".q")
		compile_sp(str)
	fi

	if runcode=run_cc and not fwriteqa then
		runqprogram(subprogs[1])
	fi
end

proc resetcompiler=
!called at end of compilesp so to reset globals for next sp

!should really recover any resources used here
	nuserxtypes:=0
	userxtypebase:=0
	ref userxrec userxmodelist:=nil
	CLEAR TTXMAP				!later limit to old nuserxtypes for efficiency

	firstusertype:=ntypes+1
end

proc setcmdparam(int index, ichar s)=
!build cmd params for pc program, or set size (usually smaller) when s is nil
	if s=nil then
		nqparams:=index
	elsif index<=maxqparam then
		qparamtable[index]:=pcm_copyheapstring(s)
		nqparams max:=index
	fi
end

=== qq_decls.m 0 0 3/16 ===
!global const fixbytecodes=1		!convert bytecodes to handler addresses

!global int dispatchtype=fn_dispatch
global byte dispatchtype=asm_dispatch
global int hasbytecodes=1			!depends on dispatchcode

GLOBAL TYPE OBJECT = REF VOID		!place holder


global type unit      	= ref unitrec
!global type object    	= ref objrec
global type symbol    	= ref strec
!global type strobject 	= ref stringobjrec
!global type variant   	= ref varrec
global type ifile   	= ref filerec
global type isubprog  	= ref subprogrec

global macro pr(a,b)	= (a<<16 ior b)

global const hasrefmask = 0x100

!global const varsize    = varrec.bytes
global const varsize    = 16

!global record packfieldrec =
!	object structobj			!owner record
!	ichar name
!	int32 packmode				!index into tables
!	int32 offset				!byte offset
!	int32 size					!size
!	int32 length
!end

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
!*!		variant varptr			!statics
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

	byte symbol
	byte subcode
	word16 slength
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
			byte tag
			union
				byte elemtype			!for array constructors
				byte flag				! for incr/in/assign/for etc (see jtag docs)
				byte condcode			! for jcmp
				byte mathsop			! for jmaths/2
				byte pclop				! for junary/jbin/junaryto/jbinto
				byte loopcode			! loop_exit etc for exit/redo/next
				byte jsubcode			! access all above via one field
			end
			[2]byte spare
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
	end

	union
		unit b
		int64 range_upper
		int64 slength
		int16 mode
		[4]byte cmpconds				!for jcmpchain
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

!global const stacksize=10000
!global [stacksize]varrec varstack
!global variant sptr
!global variant stacklimit
!global ref byte frameptr
!
!global ref int pcptr
!
!global int stopped

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

!global ichar err_message
!global varrec err_var1, err_var2
!global ref int err_pcptr

!global ref int stopseq		!point to a 'stop 0' sequence
!global ref int raiseseq		!point to a sequence of several 'raise' cmdcodes

global ref procrec proclist, proclistx
global int nproclist

global ref proc pcl_callbackfn=nil	!address of *PCL* function (pcdata address)

!global [0..255]object chrtable		!remember single-character objects

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

	pcl		pcstart				!nil, or points to generated bytecode for whole module
	pcl		pcend				!points to last allocated pcl rec
	int		pcsize				!pcl size as number of allocated ints (some spare)
	ref i32	pcsourcestart		!each entry is source-pos info (char offset into org source)

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

!global record varrec =
!	union
!		struct
!			union
!				struct
!					byte	tag
!					byte	hasref
!					byte	bitoffset
!					union
!						byte	bitlength		!for refbit/tbit: 0=1 bit, N=bitfield
!						byte	exceptiontype
!						byte	genmarker		!1 means is a generator used as a marker
!					end
!				end
!				word32		tagx
!			end
!			union
!				word32 		elemtag
!				word32 		frameptr_low
!				struct
!					i16		frameoffset
!					i16		nexceptions
!				end
!			end
!		end
!		i64 dummy: (skip:16, range_lower:48)
!	end
!	union
!		int64		value
!		real64		xvalue
!		word64		uvalue
!		word64		range_upper
!		object		objptr				!objects where hasref=1
!		variant		varptr				!for refvar
!		ref byte	ptr					!for refproc etc
!		symbol		def					!for tsymbol
!		ref int		retaddr
!	end
!end
!
!export record objrec =
!!1st 8 bytes
!	word32 refcount
!	struct
!		byte flags: (lower:1, mutable:1, bittag:2)
!		byte objtype
!		union
!			u16 elemtag
!			u16 usertag
!			u16 itertag
!			struct
!				byte bitoffset
!				byte indexoffset
!			end
!			i16 lower16
!!			i16 iterended		!0/1 = running/ended
!		end
!	end
!
!!second 8 bytes (and end of short objects)
!	union
!		struct
!			union
!				int64		value
!				real64		xvalue
!				word64		uvalue
!				ichar		strptr
!				variant		varptr
!				variant		genstack
!				ref byte	ptr
!				ref[0:]elemtype num
!				word64 b
!				ref int		retaddr
!			end
!
!!3rd 8 bytes
!			union
!				int64 length
!				int64 lower64
!				struct
!					word32 rows
!					word32 columns
!				end
!				word64 c
!				ref byte frameptr
!!				symbol		stgen
!				struct
!					int32 iterpos
!					int32 iterupper
!				end
!			end
!
!!4th 8 bytes (and end of long objects)
!			union
!				int64 alloc64				!object/item counts, not bytes
!				object objptr2
!				struct
!					int16 neg
!					int16 numtype
!					int32 expon
!				end
!				struct
!					word32 alloc32
!					word32 dictitems
!				end
!				struct
!					u16		genstacksize		!in varrecs
!					byte	ngenparams			!params to gen func
!				end
!				word64 d
!			end
!		end
!		[24]byte bignumdescr
!	end
!end

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

!GLOBAL INT NUNITS
!GLOBAL INT NPCL

global pcl stopseq		!point to a 'stop 0' sequence
global pcl raiseseq		!point to a sequence of several 'raise' cmdcodes


!temporarily moved from pclgen etc

global int nproclocals			!no. of locals
global pcl pproclocals			!pointer to pcl operand of kprocentry; may need updating
=== qq_lex.m 0 0 4/16 ===
const etx	= 26
const cr	= 13
const lf	= 10
const tab	= 9

ref char lxsource		!start of module
ref char lxstart		!start of this token
ref char lxsptr
int lxifcond
int longsuffix			!for real nos
int lxfileno

const hstsize	= 32768
!const hstsize	= 65536
const hstmask	= hstsize-1

int nextlxlength
global int lxlength

global [0:hstsize]strec hashtable
symbol hashtablelast

ichar u64maxstr="18446744073709551615"

[0..255]byte namemap			!0/1/2 = other/name/name-upper

global proc lexreadtoken=
!read next token into nextlx
int c,csum,hsum,commentseen
ref char pstart,pnext,p,ss

	nextlx.subcode:=0

	doswitch lxstart:=lxsptr; lxsptr++^
	when 'a'..'e','g'..'z','$','_' then
	dolower:
		nextlx.svalue:=lxsptr-1
	doname:
		hsum:=nextlx.svalue^

		docase namemap[c:=lxsptr++^]
		when 1 then
			hsum:=hsum<<4-hsum+c
		when 2 then
			(lxsptr-1)^:=c+' '
			hsum:=hsum<<4-hsum+c+' '
		else
			--lxsptr
			exit
		end docase

		lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)
		return

	when 'A'..'E','G'..'Z' then
	doupper:
		nextlx.svalue:=lxsptr-1
		nextlx.svalue^+:=32
		goto doname

	when 'f' then
		if lxsptr^<>'"' then
			goto dolower
		fi
		readrawstring()
		return

	when 'F' then
		if lxsptr^<>'"' then
			goto doupper
		fi
		readrawstring()
		return

	when '0'..'9' then
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

	when '!', '#' then			!comment to eol
	docomment:

		docase c:=lxsptr++^
		when 13 then
			++lxsptr
			exit
		when 10 then
			exit
		when etx,0 then
			--lxsptr
			exit
		end
		nextlx.symbol:=eolsym
		return

	when '\\' then			!line continuation

!two stages:
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		docase lxsptr++^			!read until end of this line
		when cr then
			++lxsptr				!skip lf
			++nalllines
			exit
		when lf then
!		++nextlx.pos
			++nalllines
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
		end
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

		docase lxsptr++^
		when cr then
			++lxsptr				!skip lf
			++nalllines
		when lf then
			++nalllines
		when ' ',tab then
		else
			--lxsptr
			exit
		end

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
			readreal()
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
		when ':' then
			++lxsptr
			case lxsptr^
			when '=' then
				++lxsptr
				nextlx.symbol:=assignsym
				nextlx.subcode:=1			!deep copy
			else
				error
			esac
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
		nextlx.symbol:=barsym
		return

	when '^' then
		nextlx.symbol:=ptrsym
		nextlx.subcode:=1				!when used as ^x
!		nextlx.subcode:=jptrto
		return

	when '@' then
		nextlx.symbol:=atsym
		return

	when '?' then
		nextlx.symbol:=questionsym
		return

!	when '~' then
!		nextlx.symbol:=curlsym
!		return

	when '+' then
		nextlx.symbol:=addsym
		nextlx.subcode:=kadd
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=0
		fi

		return

	when '-' then
		nextlx.symbol:=subsym
		nextlx.subcode:=ksub
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=1
		when '>' then
			++lxsptr
			nextlx.symbol:=pipesym
		esac
		return

	when '*' then
		nextlx.symbol:=mulsym
		nextlx.subcode:=kmul
		if lxsptr^='*' then
			++lxsptr
			nextlx.symbol:=powersym
			nextlx.subcode:=kpower
		fi
		return

	when '/' then
		nextlx.symbol:=divsym
		nextlx.subcode:=kdiv
		return

	when '%' then
		nextlx.symbol:=idivsym
		nextlx.subcode:=kidiv
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
		case lxsptr^
		when '=' then
			++lxsptr
			++lxsptr
			nextlx.symbol:=lesym
			nextlx.subcode:=le_cc
		when '>' then
			++lxsptr
			nextlx.symbol:=nesym
			nextlx.subcode:=ne_cc
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
			nextlx.subcode:=kshl
		else
			nextlx.symbol:=ltsym
			nextlx.subcode:=lt_cc
		esac
		return

	when '>' then
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=gesym
			nextlx.subcode:=ge_cc
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
			nextlx.subcode:=kshr
		else
			nextlx.symbol:=gtsym
			nextlx.subcode:=gt_cc
		esac
		return

	when '&' then
		case lxsptr^
		when '&' then
			++lxsptr
			nextlx.symbol:=concatsym
			nextlx.subcode:=kconcat
		else
			nextlx.symbol:=addrsym
			nextlx.subcode:=0
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
		++nalllines
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		nextlx.symbol:=eolsym
		++nalllines
		return

	when etx,0 then
		nextlx.symbol:=eofsym
		--lxsptr
		return

	else
		c:=(lxsptr-1)^
		if c=0xE2 and lxsptr^=0x88 and (lxsptr+1)^=0x9A then
			lxsptr+:=2
			nextlx.symbol:=mathssym
			nextlx.subcode:=mm_sqrt
			return
		fi



		if c>=128 then		!assume utf8
			goto doname
		fi
error:
		nextlx.symbol:=errorsym
		nextlx.value:=c
		return

	end doswitch
!end switch
!od

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

global proc lexinit=
!do one-time setup:
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i!,n
	static int n

	memset(&hashtable,0,hashtable.bytes)
	hashtablelast:=&hashtable[hstsize-1]

	inithashtable()
end

proc readrawstring=
!positioned at " of F"
!read raw string
	ichar pstart
	int length

	nextlx.symbol:=stringconstsym

	pstart:=++lxsptr
	length:=0

	docase lxsptr++^
	when '"' then
		exit
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		++length
	end

	nextlxlength:=length

	nextlx.svalue:=pcm_copyheapstringn(pstart,length)
end

global function lookup(ichar name, int length, hashindex)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in nextlxhashvalue
!return 1 (found) or 0 (not found)
!in either case, nextlx.symptr set to entry where name was found, or will be stored in
	int j,wrapped,n
	symbol d
	ref char s

	d:=&hashtable[hashindex]
	wrapped:=0

	do
		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbolcode
			nextlx.subcode:=d.subcode
			return 1
		elsif n=0 then
			exit
		fi

		if ++d>hashtablelast then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			d:=&hashtable[0]
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr
	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbolcode:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbolcode
	nextlx.subcode:=d.subcode

	return 0
end

global function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hsum<<4-hsum + c
	od
	return (hsum<<5-hsum) iand hstmask
end

proc start=
	for c in namemap.bounds do
		if c in 'a'..'z' or c in '0'..'9' or c in ['_','$'] or c in 128..255 then
			namemap[c]:=1
		elsif c in 'A'..'Z' then
			namemap[c]:=2				!upper case
		fi
	od
end

proc inithashtable=
!populate hashtable with standard symbols
	int i
	ichar name

	for i:=1 to stnames.len do
		addstname(stnames[i], stsymbols[i], stsubcodes[i])
	od

	for i to hostfnnames.upb when not hostinternal[i] do
		name:=hostfnnames[i]+2				!skip 'h_'
		addstname(name, khostfnsym, i)

	od
end

proc addstname(ichar name, int symbol, subcode)=
	if lookup(name,strlen(name),gethashvaluez(name)) then
		println name
		abortprogram("Dupl ST entry")
	fi

	nextlx.symptr.symbolcode:=symbol
	nextlx.symptr.subcode:=subcode
end

global proc startlex(ifile pm)=
!	if not fwriteqa then
!		lxsource:=lxsptr:=pm.text
!	else
		lxsource:=lxsptr:=pcm_copyheapstring(pm.text)
!	fi
	lxfileno:=pm.moduleno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
	nextlx.moduleno:=pm.moduleno
end

global function addnamestr(ichar name)symbol=
	lexrec oldlx
	symbol symptr

	oldlx:=nextlx

	nextlxlength:=strlen(name)
	nextlx.svalue:=pcm_alloc(nextlxlength+1)
	memcpy(nextlx.svalue,name,nextlxlength+1)
	lookup(nextlx.svalue, nextlxlength, gethashvaluez(name))
	symptr:=nextlx.symptr

	nextlx:=oldlx

	return symptr
end

global proc ps(ichar caption)=
	print caption,,":::"
	printsymbol(&lx)
end

global proc psnext(ichar caption)=
	print "	",,caption,,":##"
	printsymbol(&nextlx)
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
!static int lastline=0
	int lineno,n,dir,namelen
	ref char p
	symbol symptr

	lx:=nextlx				!grab that already read basic token

	lxlength:=nextlxlength
	lx.sourceoffset:=lxstart-lxsource

	reenter:

	lexreadtoken()			!read new token for next time around
	reenter2:

	case nextlx.symbol
	when unitnamesym then					!might be user identifier (points to generic entry)
		case lx.symbol
		when intconstsym then
			case nextlx.symptr.subcode
			when million_unit then lx.value *:= 1 million
			when billion_unit then lx.value *:= 1 billion
			when thousand_unit then lx.value *:= 1000
			else
				lxerror("Can't do this unit index")
			esac
			lx.subcode:=tint
			goto reenter
		when realconstsym then
			lxerror("unit symbol after float?")
		else
			nextlx.symbol:=namesym				!convert to actual identifier
		esac

	when sysconstsym then					!ST ENTRY LIMITED TO 16 bits signed
		case nextlx.subcode
		when con_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint
		when pi_const then
			nextlx.symbol:=realconstsym
			nextlx.xvalue:=pi
			nextlx.subcode:=treal
		when tab_const then
			nextlx.symbol:=stringconstsym
			nextlx.svalue:="\t"
			nextlxlength:=1
		when true_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=1
			nextlx.subcode:=tint
		when false_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint

		else
			lxerror("sysconst?")
		esac

	when eolsym then

		case lx.symbol
		when commasym, lsqsym, lbracksym, !ignore eol
			 assignsym,semisym then

			lexreadtoken()
			goto reenter2

			goto reenter
		elsif binopset[lx.symbol] and 	lx.symbol not in [maxsym, minsym] then
			lexreadtoken()
			goto reenter2

		esac
		nextlx.symbol:=semisym

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=insym
			lx.subcode:=1
			goto reenter
		fi
	esac

end

global proc lxerror_s(ichar mess,a)=
	[256]char str
	fprint @str,mess,a
	lxerror(&.str)
end

proc makedecimal(ichar s, int length,base)=
!create a decimal number token

	if base<>10 then
		LXERROR("MAKEDECIMAL/16/2")
	fi

	nextlx.symbol:=decimalconstsym
	nextlx.subcode:=tdecimal
	nextlx.svalue:=pcm_copyheapstringn(s,length)
	nextlxlength:=length
end

proc readdec=
	int c
	ref char dest, destend, pstart
	int islong, length
	[1024]byte str
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
			dest^:=0
			makedecimal(&.str,dest-&.str,10)
			return

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

	if length>20 or length=20 and strncmp(&.str,u64maxstr,20)>0 then
		makedecimal(&.str,length,10)
		return
	fi

finish:
	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
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
			dest^:=0
			makedecimal(&.str,dest-&.str,16)
			return

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
		makedecimal(&.str,length,16)
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
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
			dest^:=0
			makedecimal(&.str,dest-&.str,2)
			return

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
		makedecimal(&.str,length,2)
		return
	fi

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tint
	nextlx.value:=a
end

proc readreal=
!at '.', or had been in middle of int where . or e were seen, back at the start

	int c,n,negexpon,dotseen,length, fractlen, expon, expseen
	real x
	[1024]char str
	ichar dest, destend, pexpon

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
					makedecimal(&.str,dest-&.str,10)
					return
				else
					--lxsptr
					exit all
				fi
			end

		when '_','\'' then

		when 'l','L' then
			makedecimal(&.str,dest-&.str,10)
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

proc readrawxname=
	int c,hsum,length

	nextlx.svalue:=lxsptr
	hsum:=0

	while namemap[c:=lxsptr++^] do
		hsum:=hsum<<4-hsum+c
	od
	--lxsptr

	lookup(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)

	return
end
=== qq_lib.m 0 0 5/16 ===
int currlineno
global int nextavindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const maxlocalunits=500
![maxlocalunits]unitrec unitpool
int nlocalunits

ichar errormess

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global proc reportcterror(ichar errortype,mess,int pos, symbol currproc=nil)=
	locrec loc
!CPL "CT1",POS IAND 16777215, CURRPROC
	loc:=geterrorinfo(pos,currproc)
!CPL "CT2"
	println errortype,"Error:"
	println "    ",,mess
	println

!CPL "CT3"
	showerrorsource(loc)
!CPL "CT4"
	stopcompiler(loc)
end

global func geterrorinfo(word pos, symbol currproc=nil)locrec=
!slow is the low word of a string pointer into source code
!moduleno is the module number if known, otherwise 0 to work it out here
!Set up global error vars: errorline, errorpointer, errormodule, errorlineno
	int soffset, moduleno
	locrec loc

	clear loc
	soffset:=pos.[0..23]
	moduleno:=pos.[24..31]

!CPL =SOFFSET
!CPL =MODULENO

	if moduleno=0 then
		ABORTPROGRAM("GETERRORINFO: no module")
	fi
	if currproc=nil then
		ABORTPROGRAM("GETERRORINFO: no currproc")
	fi

	loc.pm:=modules[moduleno]
	loc.sp:=subprogs[loc.pm.subprogno]	
	loc.def:=currproc

!CPL "GEIX"
!	loc.lineno:=getlineno(loc.p.text, soffset, loc.loc.column)
	setlineno(&loc, soffset)

	return loc
end

!global function getlineno(ichar source, int offset, ichar startline, int &column)int=
global proc setlineno(ref locrec loc, int offset)=
!loc contains sp/pm, fill in startline/lineno/column given char offset within module
	ichar sline, s, source:=loc.pm.text
	

	sline:=source+offset

	while sline>source and sline^<>10 do --sline od
	if sline^=10 then ++sline fi
	loc.startline:=sline
	loc.column:=source+offset-sline

	s:=sline
	loc.lineno:=1
	while s>source do
		if s^=10 then ++loc.lineno fi
		--s
	od
end

proc showerrorsource(locrec loc)=
	ichar s
	println "Line:",loc.lineno,"in Module",loc.pm.name,,".q:"

	if loc.def then
		println "In function:",loc.def.name
	fi

!CPL "///STARTLINE",LOC.STARTLINE
	print " |"
	s:=loc.startline
	while s^ not in [13,10,26,0] do
		print s^
		++s
	od
	println "|"
	

!	println " |",errorline
!	println " |",errorpointer
end

global proc stopcompiler(locrec loc)=
	filehandle f
	f:=fopen("$error.tmp","w")
!	println @f,modulename,,".q",lineno
	println @f,loc.pm.filespec, loc.lineno
	fclose(f)
	println
	println

!	OS_GETCH()

	stop 1
end

!global proc prterror(ichar mess)=
!	reportcterror("Print",mess,qpos)
!end

global proc gerror(ichar mess,unit p=nil)=
!CPL "G1"
	reportcterror("Code Gen",mess,(p|p.pos|qpos),stcurrproc)
end

global proc gerror_s(ichar mess, param,unit p=nil)=
	[300]char str
	print @str, mess, param
	reportcterror("Code Gen",&.str,(p|p.pos|qpos),stcurrproc)
end

global proc serror(ichar mess)=
	reportcterror("Syntax",mess,lx.pos,stcurrproc)
end

global proc serror_s(ichar mess,param)=
	[300]char str
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	reportcterror("Syntax",str,lx.pos,stcurrproc)
end

global proc rxerror(ichar mess,unit p=nil)=

!CPL =P, =QPOS, =STCURRPROC

	reportcterror("Resolve",mess,(p|p.pos|qpos),stcurrproc)
end

global proc rxerror_s(ichar mess,param, unit p=nil)=
	[300]char str
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	rxerror(str,p)
end

global proc lxerror(ichar mess)=
	reportcterror("Lex",mess,lx.pos,stcurrproc)
end

global proc loaderror(ichar mess,mess2="")=
	[512]char str
	if strchr(mess,'#') then
!		fprint @str,mess,mess2,mess3
		fprint @str,mess,mess2
	else
		print @str,mess
	fi

	println "Load Error:",str
	println "Stopping"
	stop 1
end

global proc prterror(ichar mess)=
	println "Print error:",mess
	os_getch()
	stop 1
end

function allocunitrec:unit p=
!	p:=pcm_alloc(unitrec.bytes)
	p:=pcm_allocnfz(unitrec.bytes)
!	p:=pcm_alloc32()
!	p:=pcm_alloc64()
!	p:=malloc(64)
!	clear p^

!++NUNITS

!	p.word1:=p.nextunit:=p.a:=p.b:=nil
!	p.nextunit:=p.a:=p.b:=nil
	p.pos:=lx.pos
	return p
end

global function createintunit(int64 a)unit=
	unit u
!	allocunit(u)
	u:=allocunitrec()
	u.tag:=jintconst
	u.value:=a
	return u
end

global function createrealunit(real64 x)unit=
	unit u
!	allocunit(u)
	u:=allocunitrec()
	u.tag:=jrealconst
	u.xvalue:=x
	return u
end

global function createstringunit(ichar s, int slength=-1)unit=
	unit u
	if slength=-1 then
		slength:=strlen(s)
	fi

	u:=allocunitrec()
!	allocunit(u)
	u.tag:=jstringconst
	u.svalue:=pcm_alloc(slength+1)
	if slength then
		memcpy(u.svalue,s,slength)
	fi
	(u.svalue+slength)^:=0
	u.slength:=slength
	return u
end

global function createunit0(int tag)unit=
	unit u
	u:=allocunitrec()
!	allocunit(u)
	u.tag:=tag
	return u
end

global function createunit1(int tag, unit p)unit=
	unit u
	u:=allocunitrec()
!	allocunit(u)
	u.tag:=tag
	u.a:=p
	return u
end

global function createunit2(int tag, unit p,q)unit=
	unit u

	u:=allocunitrec()
!	allocunit(u)

	u.tag:=tag
	u.a:=p
	u.b:=q

	return u
end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
!	allocunit(u)
	u.tag:=jname
	u.def:=p

	return u
end

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
!p can be a list, then all are added

while p do

	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer

	p:=p.nextunit
od
end

global function createavname:unit=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

!	sprintf(&.str,"av$%d",++nextavindex)
	print @str, "av$",,++nextavindex

	name:=pcm_copyheapstring(&.str)
	p:=addnamestr(name)

	return createname(p)
end

global function convtostringz(ref char svalue,int length)ref char =
! a contains a string object which is a NON-zero-terminated string.
! Set up a pointer to a proper zero-terminated one and return that.
! This uses a ring of 3 static string objects it will only work for strings up to
! a certain length, and only if not more than 3 are needed for any single call.

	const strbufflen=2000
	static [0:strbufflen]char strbuffer1
	static [0:strbufflen]char strbuffer2
	static [0:strbufflen]char strbuffer3
	static [0:strbufflen]char strbuffer4
	static [0:strbufflen]char strbuffer5
	static [0:strbufflen]char strbuffer6
	static int strindex=0		!index of current buffer: cycles between 0,1,2
!	static [0:]ref [0:]char table=(
	static [0:]ref [0:0]char table=(

! [0:]ref [0..15]c8


		&strbuffer1,&strbuffer2,&strbuffer3,
		&strbuffer4,&strbuffer5,&strbuffer6)
!	cast(strbuffer1),cast(strbuffer2),cast(strbuffer3),
!	cast(strbuffer4),cast(strbuffer5),cast(strbuffer6))
	ref[0:]char p
	static ichar longstr=nil


	if length>=strbufflen then
		if longstr then
			free(longstr)
		fi
		longstr:=malloc(length+1)
		memcpy(longstr,svalue,length)
		(longstr+length)^:=0
		return longstr
	fi

	if svalue=nil then
		return ""
	fi

	if ++strindex=table.len then
		strindex:=0
	fi
	p:=table[strindex]
	memcpy(p,svalue,length)
!(p+length)^:=0
	p^[length]:=0
	return cast(p)
end

global function findprocname(ref proc fnptr)ichar=
	ichar name
	int n:=$getnprocs()

	for i to n do
		if $getprocaddr(i)=fnptr then
			return $getprocname(i)
		fi
	od

	return "?"
end

global function strexpr(unit p)ref strbuffer=
	gs_init(exprstr)
	jeval(p)
	return exprstr
end

global function strexpr_s(unit p)ichar=
	if p=nil then return "" fi
	gs_init(exprstr)
	jeval(p)
	return exprstr.strptr
end

proc jeval(unit p)=
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q
	[500]char str

!CPL "JEVAL",JTAGNAMES[P.TAG]

!	switch p.tag
	case p.tag
	when jintconst then
		additem(strint(p.value))

	when jrealconst then
		additem(strreal(p.value))

	when jstringconst then
		if p.slength>str.len/2 then
			strcpy(&.str,"LONGSTR)")
		else
			convertstring(p.svalue,&.str)
		fi
		additem("""")
		additem(&.str)
		additem("""")
!
	when jname then
		additem(p.def.name)

!	when jcall then
!		jeval(p.a)
!		additem("(")
!
!		q:=p.b
!		while q do
!			jeval(q)
!			q:=q.nextunit
!			if q then additem(",") fi
!		od
!		additem(")")

	when jcallhost then
		additem("Host<")
		additem(hostfnnames[p.index]+2)
		additem(">(")

		q:=p.a
		while q do
			jeval(q)
			q:=q.nextunit
			if q then additem(",") fi
		od
		additem(")")

	when jindex,jdotindex then
		jeval(p.a)
		if p.tag=jdotindex then
			additem(".")
		fi
		additem("[")
		jeval(p.b)
		additem("]")

	when jkeyindex then
		jeval(p.a)
		additem("{")
		jeval(p.b)
		additem("}")

	when jdot then
		jeval(p.a)
		additem(".")
		jeval(p.b)

	when jassign then
		jeval(p.a)
		additem(":=")
		jeval(p.b)

	when jtypeconst then
		additem(strmode(p.mode))
!
	when jconvert then

		additem(strmode(p.mode))
		additem("(")
		jeval(p.a)
		additem(")")

	when jkeyvalue then
		jeval(p.a)
		additem(":")
		jeval(p.b)

!	when jptr then
!		jeval(p.a)
!		additem("^")
!
!	when jptrto then
!		additem("^")
!		jeval(p.a)
!
	when jnil then
		additem("nil")

	when jsymbol then
		jeval(p.a)
		additem(".$")

	when jcmpchain then
		additem("CMPCHAIN:")
		q:=p.a
		jeval(q)

		for i to 4 do
			q:=q.nextunit
			if p.cmpconds[i]=0 then exit fi
			additem(jtagnames[p.cmpconds[i]])
			jeval(q)
		od

	elsif jflags[p.tag]=2 then
		strcpy(&.str,getopcname(p.tag))
		additem("(")
		jevallist(p.a)
		additem(&.str)
		jevallist(p.b)
		additem(")")

	elsif jflags[p.tag]=1 then
		strcpy(&.str,getopcname(p.tag))
		additem(&.str)
		additem("(")
		jevallist(p.a)
		additem(")")


	else
		CPL jtagnames[p.tag]
		loaderror("CAN'T DO JEVAL:",jtagnames[p.tag])
	end
end

proc jevallist(unit p)=
	unit q

	return unless p

	if p.nextunit then
		additem("(")
		q:=p
		while q do
			jeval(q)
			q:=q.nextunit
			if q then additem(",") fi
		od
		additem(")")
		return
	else
		jeval(p)
	fi
end

global proc additem(ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

	d:=exprstr.strptr

	if exprstr.length then
		lastchar:=(d+exprstr.length-1)^
		nextchar:=s^
		if isalphanum(lastchar) and isalphanum(nextchar) then
			strbuffer_add(exprstr," ")
		fi
	fi
	strbuffer_add(exprstr,s)
end

function isalphanum(int c)int=
	if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
!	if c in 'A'..'Z' or c in 'a'..'z' or c in '0'..'9' then
		return 1
	fi
	return 0
end

global function getopcname(int opc)ichar=
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
	jtagnames[opc]
end

global proc convertstring(ichar s, t)=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
int c

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
		end switch
	od
	t^:=0
end

global function createavnamex(symbol owner)unit p=
!local autovar needed from genpcl
!needs to update local vars

	symbol d
	p:=createavname()
	resolvename(owner,p)
	d:=p.def

	if d.nameid=frameid then
		++nproclocals
		d.index:=nproclocals
GERROR("CREATEAV/PROCLOCALS")

!		pproclocals^:=nproclocals
	fi							!else created at module level

	return p
end

global proc storemode(symbol owner, int m, ref int16 p)=
	ref userxrec q
!CPL "STOREMODE",STRMODE(M)
	p^:=m
	if m>=0 then return fi

	q:=pcm_alloc(userxrec.bytes)
	q.owner:=owner

	IF OWNER=NIL THEN
		SERROR("STOREMODE/OWNER=0")
	FI

	q.pmode:=p
	q.nextmode:=userxmodelist
	userxmodelist:=q
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

global function testelem(ref[0:]byte p,int n)int =
!caller must check that n is in range
	return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =
	p^[n>>3] ior:= bytemasks[n iand 7]
end

global proc setelemblock(ref[0:]byte p, int a,b) =
	int ax, bx, nwords, nx, alast,bfirst
	ref u64 q

	if a>b then return fi

    ax:=a iand inot 63
    bx:=b iand inot 63 + 64
	nx:=ax				!start of whole words
	alast:=bfirst:=-1

	nwords:=(bx-ax)/64

	if nwords=1 then
		if ax<>a or b<>(bx-1) then		!neither aligned, sequence is inside one word
			for i:=a to b do
				setelem(cast(p),i)
			od
			return
		fi
	else								!2 words or more
		if ax<>a then					!a not aligned
			--nwords
			nx:=ax+64
			alast:=nx-1
		fi
		if b<>bx-1 then					!b not aligned
			--nwords
			bfirst:=b iand inot 63
		fi

	fi

	if alast>=0 then					!part-word elements before whole words
		for i:=a to alast do
			setelem(cast(p),i)
		od
	fi

	q:=cast(&p[nx>>3])
	to nwords do
		q^:=0xFFFF'FFFF'FFFF'FFFF
		++q
	od

	if bfirst>=0 then				!part-word elements after whole worlds
		for i:=bfirst to b do
			setelem(cast(p),i)
		od
	fi
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

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

global proc skipsemi=
	while lx.symbol=semisym do lex() od
end

global proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @&.str,"# expected, not #",symbolnames[symbol]:"m",symbolnames[lx.symbol]:"m"
		serror(&.str)
	fi
end

global proc skipsymbol(int symbol)=
	checksymbol(symbol)
	lex()
end

=== qq_modules.m 0 0 6/16 ===

global func loadsp(ichar filename, source=nil)isubprog sp=
!source = nil:  load lead module and dependencies from given sourcefile
!source <> nil: source code is given directly. filename can give a name
! to that source text, or if nil, and internal name is applied

	const maxmods=100
	const maxsubs=100
	[maxmods]ichar modnames
	[maxsubs]ichar subnames
	int nmods:=0, nsubs:=0
	int firstmod, lastmod, issyslib:=0
	ifile pm
	symbol d
	[300]char path

!CPL "LOADSP",FILENAME

	if source then
		pm:=loadstring(filename, source)
		path[1]:=0
	else
		if eqstring(extractbasefile(filename), syslibname) then
			issyslib:=1
		fi

		pm:=loadsourcefile(filename, issyslib)
		if pm=nil then
			loaderror("Can't load lead module: #", filename)
		fi
		strcpy(path, pm.path)
	fi

	for i to nsubprogs do
		if eqstring(pm.name, subprogs[i].name) then
			loaderror("Subprog already loaded: #", sp.name)
		fi
	od

!reader header info
	startlex(pm)

	do
		lex()
		skipsemi()
		case lx.symbol
		when kmodulesym then
			lex()
			checksymbol(namesym)
			if not eqstring(lx.symptr.name, pm.name) then
				if nmods>=maxmods then loaderror("Too many modules in header") fi
				modnames[++nmods]:=lx.symptr.name
			fi

		when kimportsym then
			lex()
			checksymbol(namesym)
			if nsubs>=maxsubs then loaderror("Too many imports in header") fi
			subnames[++nsubs]:=lx.symptr.name

		when semisym then
		else
			exit
		esac
	od

!process nested imports
	for i to nsubs do
		if eqstring(subnames[i],pm.name) then loaderror("Importing self") fi
		compile_sp(getmodulefilename("", subnames[i]))				!recursive load
	od

!create new subprog entry
	if nsubprogs>=maxsubprog then loaderror("Too many subprogs") fi
	sp:=pcm_allocz(subprogrec.bytes)
	subprogs[++nsubprogs]:=sp
	sp.subprogno:=nsubprogs

	firstmod:=nmodules+1
	lastmod:=firstmod+nmods
	if lastmod>maxmodule then loaderror("Too many modules") fi
	nmodules:=lastmod
	pm.subprogno:=nsubprogs
	pm.islead:=1
	pm.moduleno:=firstmod
	pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
	d.moduleno:=firstmod

	sp.name:=pm.name
	sp.path:=pm.path
	sp.filespec:=pm.filespec
	sp.firstmodule:=firstmod
	sp.lastmodule:=lastmod
	sp.issyslib:=issyslib

!create new set of modules[] entries and load those other modules
!create stmodule entries for each module
	modules[firstmod]:=pm

	for i to nmods do
		pm:=loadsourcefile(getmodulefilename(path, modnames[i]), issyslib)
		if not pm then
			loaderror("Can't load: ##",modnames[i])
		fi
		modules[firstmod+i]:=pm
		pm.stmodule:=d:=createdupldef(stprogram,addnamestr(pm.name),moduleid)
		pm.subprogno:=nsubprogs
		d.moduleno:=pm.moduleno:=firstmod+i
	od

	return sp
end

func getmodulefilename(ichar path, name)ichar =
	static [300]char str

!need to sort out search path etc

	strcpy(str, path)
	strcat(str, name)
	strcat(str, ".q")
	return str
end

global func loadsourcefile(ichar filespec, int issyslib=0)ifile pm=
	ichar s,basefilename
	[300]char str

	pm:=pcm_allocz(filerec.bytes)

	basefilename:=extractbasefile(filespec)


	pm.filespec:=pcm_copyheapstring(filespec)
	pm.path:=pcm_copyheapstring(extractpath(filespec))
	pm.name:=pcm_copyheapstring(basefilename)
	pm.issyslib:=issyslib

	if nqafiles and loadqafile(pm) then
		return pm
	fi

	if issyslib and usebundled then
		pm.issyslib:=issyslib
		if not loadsysmodule(pm) then
			loaderror("LS:Can't load syslib",filespec)
		fi
!CPL "LOADED",FILESPEC,"FROM BUNDLE"
		return pm
	fi

!CPL "LOADING SOURCE", FILESPEC,"/",BASEFILENAME


	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		strcpy(str, "c:/m/libs/")
		strcat(str, basefilename)
		strcat(str, ".q")
!CPL "TRYING:",STR
		s:=cast(readfile(str))
		if not s then
			return nil
		fi
		CPL "LOADED FROM MLIBS"
	fi
!CPL "LOADED",FILESPEC,"FROM FILE"
	pm.text:=s

	pm.size:=rfsize

	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return pm
end

global func loadstring(ichar name, source)ifile pm=
	[16]char str
	static int nextstrname=0

	if name=nil then
		print @str, "S$",,++nextstrname
		name:=pcm_copyheapstring(str)
	fi

	pm:=pcm_allocz(filerec.bytes)

	pm.filespec:="<string>"
	pm.path:=""
	pm.name:=name

	pm.text:=source

	pm.size:=strlen(source)
	return pm
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

function loadqafile(ifile pm)int=
	ichar file
	[300]char filename

	strcpy(filename, extractfile(pm.filespec))
!	strcpy(filename, (pm.filespec))

	for i to nqafiles do
		if eqstring(filename,qafilenames[i]) then		!found
			pm.text:=qatext[i]
			pm.size:=qasize[i]
			return 1
		fi
	od
	return 0
end

global proc readqabundle=
	[100]char name
	ichar s, t
	int sys, support

	s:=extractext(inputfile)
	convlcstring(s)
	unless eqstring(s,"qa") then
		return
	end

!Input is a .qa file; load files into qa directory
	s:=readfile(inputfile)
	if s=nil then							!file not found on disk
		loaderror("Can't find QA file ##",inputfile)
	fi

!change inputfile from .qa to .q (later, change to suitable lead module within qa file)
	inputfile:=pcm_copyheapstring(changeext(inputfile,"q"))

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"qa") then
		loaderror("QA: bad header")
	fi

	--s					!point to previous lf

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in QA file")
			exit
		fi
		s:=readfileline(s)
		readstr(name,'n')
		read sys,support
!		println "Found file",name
		if eqstring(name,"end") then
			exit
		fi
		if nqafiles>=maxqafile then
			loaderror("Too many QA files")
		fi

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("QA error")
		fi

		++nqafiles

		qafilenames[nqafiles]:=pcm_copyheapstring(name)
		qasize[nqafiles]:=t-s-3
		qatext[nqafiles]:=s
		s:=t
	od
!
	for i to nqafiles do
		(qatext[i]+qasize[i])^:=0	
	od
end
=== qq_names.m 0 0 7/16 ===
!Symbol table handling

int sdsize, sdoffset
int sdaligned
int sdlevel
int sdmode
int sdnfields
int sdmaxalign
const int maxstructdepth=10
[maxstructdepth]byte sdunion		!1 if union model 0 for normal offset calc
[maxstructdepth]int sdmaxsize		!accumulate max size of union

global function addglobalname(ichar name)symbol=
!generic name encountered in namedef op. Convert to symbol reference
!will always return a generic strec, either existing, or just created
	lexrec oldlx
	symbol d

	oldlx:=nextlx

	lookup(name,strlen(name),gethashvaluez(name))

	d:=nextlx.symptr
	nextlx:=oldlx
	return d
end

function newstrec:symbol=
	symbol p
	p:=pcm_alloc(strec.bytes)
	memset(p,0,strec.bytes)

	return p
end

global function addsymbol(symbol owner,d, int id, isglobal)symbol e=
!d should be a generic symbol (or if nil, then when name is provided
!to create a suitable generic symbol0
!create a dedicated strec for it, link it in to the dupl chain of the generic
!version, and insert it a child of owner
	symbol f

	e:=newstrec()
	e.name:=d.name
	e.namelen:=d.namelen
	e.owner:=owner
	e.nameid:=id
	e.isframe:=id=frameid or id=paramid

!CPL "ADDSYM",=D.NAME
	if currmodule then
		e.moduleno:=currmodule.moduleno
	fi

	e.firstdupl:=d
	e.isglobal:=isglobal

!IF OWNER.NAMEID<>PROCID THEN
	return e when not owner			!not linked in to anything

IF OWNER.NAMEID NOT IN [PROCID, ANONPROCID] THEN
	e.nextdupl:=d.nextdupl
	d.nextdupl:=e


	if e.nextdupl and e.nextdupl.owner=owner then
		cpl e.name,"in",owner.name
		serror("AS:Duplicate name")
	fi
else
	f:=owner.deflist
	while f do
		if f.firstdupl=e.firstdupl then
			cpl e.name,"in",owner.name
			serror("AS2:Duplicate name")
		fi
		f:=f.nextdef
	od
fi

	if owner.deflist=nil then			!first def
		owner.deflist:=e
	else
		owner.deflistx.nextdef:=e
	fi
	owner.deflistx:=e

	return e
end

global proc addproc(symbol d)=
	ref procrec p

	p:=pcm_allocz(procrec.bytes)
	p.def:=d

	if proclist=nil then
		proclist:=p
	else
		proclistx.nextproc:=p
	fi
	proclistx:=p
	++nproclist
end

!global function createstroot(ichar name)symbol d=
!	d:=newstrec()
!	d.name:=pcm_copyheapstring(name)
!	d.namelen:=strlen(name)
!	d.nameid:=programid
!
!	return d
!end

global function newusertypex(ref strec d,e=nil)int=
	int i

	if nuserxtypes>=maxuserxtype then
		serror("Too many external user types")
	fi
	++nuserxtypes
	ttnamedefx[nuserxtypes]:=d
!	ttnamedefx2[nuserxtypes]:=e

	ttxmoduleno[nuserxtypes]:=stcurrmodule.moduleno
	return -nuserxtypes
end

global function resolvedottedname(symbol owner, d)symbol e=
!d should be generic

	e:=d.nextdupl
	while e and e.owner<>owner do
		e:=e.nextdupl
	od

	return e
end

global proc addgenfield(symbol d)=
	int index
	symbol dgen
	ref genfieldrec g


	dgen:=d.firstdupl
	index:=dgen.genfieldindex

!CPL "ADDGENFIELD",D.NAME,namenames[d.nameid],=INDEX

	if index=0 then			!first field with this name
		if ngenfields>=maxgenfield then
		gerror("Too many genfields")
		fi
		dgen.genfieldindex:=index:=++ngenfields
	fi

	g:=pcm_alloc(genfieldrec.bytes)
	g.def:=d
	g.nextdef:=genfieldtable[index]
	genfieldtable[index]:=g
end

!global function addusertype(symbol d)int=
!!d is the name of a new user type; the details have been set up inside it
!!but now create the actual type
!
!	if ntypes>=maxtype then pcerror("Too many types") fi
!
!	++ntypes
!	d.mode:=ntypes
!	ttnamedef[ntypes]:=d
!	ttname[ntypes]:=d.name
!
!	return ntypes
!
!end

global function makereftype(int target, symbol owner=nil)int=
!owner <> nil means used for new type so cannot reuse existing ref

	int newtype

	if owner=nil then
		for i:=tlast+1 to ntypes do
			if ttbasetype[i]=trefpack and tttarget[i]=target then
				return i
			fi
		od
	fi

	newtype:=addanontype()
	ttbasetype[newtype]:=trefpack

	storemode(stcurrproc,target,&tttarget[newtype])

	ttsize[newtype]:=8
	ttbitwidth[newtype]:=64
!	ttcat[newtype]:=refcat
	return newtype
end

global function makeaxtype(int target, unit plower, plength)int=
	int newtype,length

	newtype:=addanontype()

	ttbasetype[newtype]:=tvector
	storemode(stcurrproc, target, &tttarget[newtype])

	ttlower[newtype]:=1
	ttlengthexpr[newtype]:=plength
	ttlowerexpr[newtype]:=plower
!	ttcat[newtype]:=blockcat			!may be adjusted later
!	ttsize[newtype]:=length*ttsize[target]



	return newtype
end

global function makestrtype(int m, unit pwidth)int=
	int newtype

	newtype:=addanontype()
	ttbasetype[newtype]:=m
!	ttispacked[newtype]:=1
!	ttlength[newtype]:=width
	ttlengthexpr[newtype]:=pwidth
	ttlower[newtype]:=1
	ttowner[newtype]:=stcurrproc
!	ttsize[newtype]:=width
	return newtype
end

global function addanontype:int=
!d is the name of a new user type; the details have been set up inside it
!but now create the actual type
	[32]char str

	if ntypes>=maxtype then gerror("Too many types") fi

	++ntypes
!CPL "ADDANONTYPE",NTYPES
	print @str,"$T",,ntypes

!CPL "ADDANON TYPE", STCURRPROC.NAME
	ttname[ntypes]:=pcm_copyheapstring(str)
	ttowner[ntypes]:=stcurrproc

	return ntypes

end

global proc createusertype(symbol d, int m)=
	storemode(stcurrproc,m,&d.mode)

	if m>tlast and ttnamedef[m]=nil then
		ttnamedef[m]:=d
		ttname[m]:=d.name
		ttowner[m]:=d.owner
!		ttcat[m]:=gettypecat(m)
	fi
end

!global function roundoffset(int offset, alignment)int=
!	int mask
!
!	if alignment=1 then return offset fi
!	mask:=alignment-1
!	while offset iand mask do ++offset od
!
!	return offset
!end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tvector then
		return getalignment(tttarget[m])
!	when tpstruct then
	when tstruct then
!		return ttnamedef[m].structmaxalign
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	esac
	cpl ttname[m],a
	gerror("Getalign not 1248")

	return 0
end

global proc duplfield(ref strec p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

	if p.code then
		serror("DUPLFIELD")
	fi
	q.atfield:=p.atfield
	q.index:=p.index
	q.fieldoffset:=p.fieldoffset
end

proc writesig(symbol d, filehandle dev)=
	symbol e
	int n
	fprint @dev, "# #(", (d.misfunc|"function"|"proc"), d.name

	e:=d.deflist
!CPL "PARAMS",D.NPARAMS
	n:=0
	while e, e:=e.nextdef do
		if e.nameid=paramid then
			++n
			if e.moptional and e.code then
				fprint @dev,"#=#", e.name, strexpr(e.code).strptr
			elsif e.moptional then
				print @dev,"?",,e.name
			else
				print @dev,e.name
			fi

			if n<d.nparams then
				print @dev,", "
			fi
		fi
	od

	fprintln @dev,")	[#]", d.owner.name

end

global function createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbolcode:=namesym
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
=== qq_parse.m 0 0 8/16 ===
!Parser

int intabledata
ichar tabledataname=nil

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
byte yieldseen

macro readunit=readexpression()
macro readxunit=readunit

int currdllindex
int nextlambdaindex

const maxlisttype=20
[maxlisttype]int listtypestack
int nlisttype
int listtype				!0 / 'PARAM' / 'PRINT' / 'DICT'

global proc parsemodule(ifile pm)=
	unit p

	return when pm.compiled

	currmodule:=pm
	stcurrmodule:=currmodule.def

	startlex(currmodule)

!CPL "PARSE",PM.NAME

	lex()
	lex()
!INT TT:=CLOCK()
!INT NN:=0
!REPEAT
!	LEX()
!++NN
!UNTIL LX.SYMBOL=EOFSYM
!TT:=CLOCK()-TT
!CPL "LEX TIME",TT
!CPL =NN
!STOP

	stcurrproc:=stcurrmodule

	p:=readsunit()

	stcurrmodule.code:=pm.ast:=p

	skipsemi()



	case lx.symbol
	when commasym then
		serror("Comma seq not allowed")
	when eofsym then
	else
		PS("EOF")
		serror("Bad symbol at eof")
	esac
end

function readexpression:unit p=
	p:=readterm2()

	if exprendset[lx.symbol] then return p fi

	if lx.symbol = assignsym then
		return readassignment(p)
	else
		return readorterms(p)
	fi
end

function readassignment(unit p)unit=
	int pos, isdeep
	unit q,r

	if exprendset[lx.symbol] then return p fi

	p:=readorterms(p)

	if lx.symbol = assignsym then
		isdeep:=lx.subcode
		pos:=lx.pos
		lex()
		p:=createunit2(jassign,p,readassignment(readterm2()))
		p.flag:=isdeep
		p.pos:=pos

	fi
	return p
end
!
function readorterms(unit p)unit =
	int pos
	unit q, r

	if exprendset[lx.symbol] then return p fi

	p:=readandterms(p)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jorlto,p,readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jorl,p,readandterms(readterm2()))
		p.pos:=pos
	od

	while lx.symbol=pipesym do
		lex()
		q:=r:=readterm2()
		if q.tag=jcall then
			r:=q.b
			while r.nextunit, r:=r.nextunit do od
			r.nextunit:=p
			p:=q
		else
			p:=createunit2(jcall, q, p)
		fi

	od

	return p
end

function readandterms(unit p)unit =
	int pos

	p:=readcmpterms(p)

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jandlto,p,readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jandl,p,readcmpterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readcmpterms(unit p)unit =
!creates either jcmp unit (simple 2-operand cmp), or jcmpchain (3/4 operands)
	int pos,n
	unit px,q
	[4]byte conds

	p:=readinterms(p)

	if not cmpopset[lx.symbol] then
		return p
	fi

	clear conds
	px:=p
	p:=createunit1(jcmpchain,p)
	n:=0				!n counts operand after the first

	while cmpopset[lx.symbol] do
		++n
		if n>conds.len then serror("cmpchain: Too many items") fi
		conds[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms(readterm2())
		px.nextunit:=q
		px:=q

		q.pos:=pos
	od

	if n=1 then
		p.tag:=jcmp
		p.condcode:=conds[1]
		q:=p.a
		p.b:=q.nextunit
		q.nextunit:=nil
	else
		p.cmpconds:=conds
	fi	

	return p
end

function readinterms(unit p)unit =
	int pos, tag, flag

	p:=readrangeterm(p)

	docase lx.symbol
	when insym, inxsym then
		tag:=(lx.symbol=insym|jin|jinx)
		flag:=lx.subcode			!in/1 means not in

		pos:=lx.pos
		lex()

		p:=createunit2(tag,p,readrangeterm(readterm2()))
		p.flag:=flag
		p.pos:=pos
	else
		exit
	end docase

	return p
end

function readrangeterm(unit p)unit =
	int pos

	p:=readaddterms(p)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange,p,readaddterms(readterm2()))
		p.pos:=pos
	fi

	return p
end

function readaddterms(unit p)unit =
	int pos,opc,a,b
	unit q

	p:=readmulterms(p)
	while addopset[lx.symbol] do
		opc:=lx.subcode
		if lx.symbol=addrsym then
			opc:=kappend
		fi

		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto,p,readassignment(readterm2()))
			p.pclop:=opc
			p.pos:=pos
			exit
		fi

		q:=readmulterms(readterm2())
		p:=createunit2(jbin,p,q)
		p.pclop:=opc
		p.pos:=pos
	od

	return p
end

function readmulterms(unit p)unit =
	int pos,opc,a,b
	unit q

	p:=readpowerterms(p)

	while mulopset[lx.symbol] do
		opc:=lx.subcode
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jbinto, p,readassignment(readterm2()))
			p.pclop:=opc

			p.pos:=pos
			exit
		fi

		p:=createunit2(jbin,p,readpowerterms(readterm2()))
		p.pclop:=opc
		p.pos:=pos
	od

	return p
end

function readpowerterms(unit p)unit =
	int pos

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jbin,p,readpowerterms(readterm2()))
		p.pclop:=kpower
		p.pos:=pos
	od

	return p
end

function readterm2:unit p=
	int pos

	pos:=lx.pos
	p:=readterm()
	p:=readtermsuffix(p,pos)
	return p
end

function readtermsuffix(unit p, int pos)unit=
	unit q,r
	ref char pbyte
	word64 a
	int opc,oldipl,shift,t,nparams

	docase lx.symbol
	when lbracksym then
		lex()
		q:=readslist(nparams,1)

		skipsymbol(rbracksym)
		p:=createunit2(jcall,p,q)
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr,p)
		lex()

	when lsqsym then
		p:=readindex(p,0)

	when dotsym then
		p:=readdotsuffix(p)

	when lcurlysym then
		p:=readkeyindex(p)

	when colonsym then
		case listtype
		when 'PARAM' then
			lex()
			p:=createunit2(jkeyword,p,readunit())
		when 'DICT' then
			lex()
			p:=createunit2(jkeyvalue,p,readunit())
		else
			exit
		esac

	when incrsym then
		p:=createunit1(jloadincr,p)
		p.flag:=lx.subcode
		lex()

	else
		exit
	end docase

	p.pos:=pos

	return p
end

function readterm:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t,nparams,length
	byte flag
	ichar s

	record dummy=
		union
			[20]char str
			int64 sa
		end
	end

	dummy ustr

	pos:=lx.pos

	switch lx.symbol
	when namesym then
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

	when intconstsym then
		p:=createintunit(lx.value)
		lex()

	when realconstsym then
		p:=createrealunit(lx.xvalue)
		lex()

	when stringconstsym then
		p:=createstringunit(lx.svalue)
		lex()

	when decimalconstsym then
		p:=createstringunit(lx.svalue)
		p.tag:=jdecimal
		lex()

	when charconstsym then
		length:=strlen(lx.svalue)
		ustr.sa:=0
!		sa:=0
		if length>8 then
			serror("char const too long")
		fi
		memcpy(&.ustr.str,lx.svalue,length)
		p:=createintunit(ustr.sa)
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym then
		if lx.subcode=tvoid then
			lex()
			if lx.symbol=dotsym and nextlx.symbol=ktypesym then
				lex()
				lex()
				p:=createunit0(jtypeconst)
				p.mode:=tvoid
			else
				p:=createunit0(jvoid)
			fi
		else
			p:=readcast()

		fi

	when addsym then
CPL "ADD1"
		p:=checkoperator()
		if not p then
			lex()
			p:=readterm2()
		fi

	when subsym  then
		p:=checkoperator()
		if not p then
			lex()
			if lx.symbol=assignsym then
				opc:=kneg
				dounaryto
			fi
			p:=readterm2()
			if p.tag=jintconst then
				p.value:=-p.value
			else
				p:=createunit1(junary, p)
				p.pclop:=kneg
			fi
		fi

	when inotsym, abssym, ascsym, chrsym then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			if lx.symbol=assignsym then
dounaryto:
				lex()
				p:=createunit1(junaryto,readterm2())
				p.pclop:=opc
			else
				p:=createunit1(junary, readterm2())
				p.pclop:=opc
			fi
		fi

	when notlsym, istruelsym then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			if lx.symbol=assignsym then
				opc:=(opc=jnotl | jnotlto | jistruelto)
				lex()
			fi
			p:=createunit1(opc, readterm2())
		fi

	when incrsym, decrsym  then
		p:=checkoperator()
		if not p then
			opc:=lx.subcode
			lex()
			p:=createunit1(jincrload, readterm2())
			p.flag:=opc
		fi

	when mathssym  then
		opc:=lx.subcode
		lex()
		p:=createunit1(jmaths, readterm2())
		p.pclop:=opc

	when mulsym, divsym, idivsym, iremsym, idivremsym, andlsym, orlsym,
		iandsym, iorsym, ixorsym, shlsym, shrsym, insym, inxsym,
		eqsym,nesym,ltsym, lesym,gesym,gtsym,powersym, appendsym,
		concatsym, propsym, specialopsym then
		unless p:=checkoperator() then
			serror("Operator?")
		end

	when lsqsym then
		p:=readset()

	when minsym, maxsym then
		if p:=checkoperator() then
		else
			p:=readpair(jbin, lx.subcode)
		fi

	when maths2sym then
		if p:=checkoperator() then
		else
			p:=readpair(jmaths2, lx.subcode)
		fi

	when ksprintsym then
		p:=readsprint()

	when ksreadsym,ksreadlnsym then
		p:=readsread()

	when addrsym,ptrsym then
		flag:=lx.subcode
		lex()
		p:=createunit1(jaddrof, readterm2())
		p.flag:=flag
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when compilervarsym then
		p:=readcompilervar()
		lex()

	when dollarsym then
		if intabledata then
			if tabledataname=nil then serror("$:No enum") fi
			s:=tabledataname
			if nextlx.symbol=addsym then
				lex()
				lex()
				checksymbol(intconstsym)
				s+:=lx.value
			fi
			p:=createstringunit(s,-1)
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(jproperty, dollarstack[ndollar])
			p.pclop:=kupb
		fi
		lex()

	when dotsym,kglobalsym then
		lexchecksymbol(namesym)
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

	when kmapsym then
!		p:=readmap()
		p:=readpair(jmap)

	when kclampsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=readunit()
		skipsymbol(commasym)
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			skipsymbol(commasym)
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(jbin,p,q)
		q.pclop:=kmax
		p:=createunit2(jbin,q,r)
		q.pclop:=kmin

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym,kdocasesym,kswitchsym,kdoswitchsym then
		p:=readswitchcase()

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

	when kprintsym, questionsym then
		p:=readprint()

	when kreadsym then
		p:=readread()

	when ktrysym then	!todo
		p:=readtry()
!
	when kraisesym then	!todo
		lex()
		p:=createunit1(jraise,readunit())

	when kswapsym then			!swap using function syntax
		p:=readpair(jswap)
!
	when khostfnsym then
		p:=readhostparams(nil,1)

	when knilsym then
!		p:=createunit0((lx.subcode=1|jpnil|jnil))
		p:=createunit0(jnil)
		lex()

	when kstrincludesym then
		lex()
		p:=createunit1(jstrinclude,readterm2())

	when kevalsym then
		lex()
		p:=createunit1(jeval,readunit())


	when lcurlysym then
		p:=readlambda()

	else

error:
		cpl symbolnames[lx.symbol]:"d"
		serror("readterm?")
	end switch

	p.pos:=pos
	return p
end

function readsunit(int inwhile=0)unit=
	int lineno,m,globalflag,staticflag
	unit ulist,ulistx,p,q,r
	symbol stname

	lineno:=lx.pos
	ulist:=ulistx:=nil
	globalflag:=local_scope
	staticflag:=0

	repeat
		while lx.symbol=semisym do
			lex()
		od

		switch lx.symbol
		when kstaticsym then
			lex()
			staticflag:=1
			redoloop

		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode
			lex()
			redoloop

		when kprocsym,kfunctionsym then
			readprocdef(globalflag)
			globalflag:=local_scope

		when kvarsym then
			q:=readvardef(globalflag,staticflag)
			while q do								!initialised decls involve code
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
				addlistunit(ulist,ulistx,q)			!add one by-one
				q:=r
			od

			globalflag:=staticflag:=local_scope

		when kconstsym then
			if staticflag then serror("static?") fi
			readconstdef(globalflag)
			globalflag:=local_scope

		when ktypesym then
			readtypedef(globalflag)
			globalflag:=local_scope

		when krecordsym, kstructsym then
			readrecorddef(globalflag, nil)
			globalflag:=local_scope

		when ktabledatasym then
			readtabledef(globalflag)
			globalflag:=local_scope

		when kimportdllsym then
			readimportdll()

		when kmacrosym then
			readmacrodef(globalflag)
			globalflag:=local_scope

		when eofsym then
			exit

!these are needed to check for an empty sunit preceding
		when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,sendtosym,
				kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
			exit

		when namesym then
			case nextlx.symbol
!		when dcolonsym,colonsym then
			when colonsym then
				p:=createunit1(jlabeldef,createname(addsymbol(stcurrproc, lx.symptr, labelid, 0)))
				lex()
				lx.symbol:=semisym
				addlistunit(ulist,ulistx,p)
!		when namesym then
!			goto dovar
			else
				goto doexec
			esac
		when kdosym then				!u;u;u;do rather than u;u;u do
			if inwhile then
				exit
			fi
			goto doexec

		when kmodulesym, kimportsym then
			repeat
				lex()
			until lx.symbol=semisym

		when semisym then
		when lsqsym then
			doexec
!	elsif istypestarter() and nextlx.symbol<>lbracksym then
!		goto dovar

		else							!assume a statement

	doexec:
			p:=readunit()

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
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,sendtosym,
		kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym,commasym,
		barsym,eofsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then			!empty or multiple => block
		return createunit1(jblock,ulist)
	else
		return ulist							!single => one unit
	fi
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

function readindex(unit p,int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is:
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q,plower,pupper

	lex()

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		fi
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

		p:=createunit2((dot|jdotindex|jindex),p,q)

		exit when lx.symbol<>commasym
		lex()
	od
	skipsymbol(rsqsym)
	return p
end

function readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
	unit q
	int t
	byte flag

	while lx.symbol=dotsym do
		lex()
		case lx.symbol
		when lsqsym then
			p:=readindex(p,1)
		when namesym then
			p:=createunit2(jdot,p,createname(lx.symptr))
			lex()
		when  propsym then			!ought to check whether op is allowed in this form
doprop:
!CPL "DOT PROP",JTAGNAMES[LX.SUBCODE]
			p:=createunit1(jproperty, p)
			p.pclop:=lx.subcode
			lex()

		when ktypesym then
			if p.tag<>jtypeconst then
				flag:=1
dogettype:
				p:=createunit1(jgettype,p)
				p.pclop:=flag
			fi
			lex()

		when maxsym then
			lx.subcode:=kmaxval
			doprop

		when minsym then
			lx.subcode:=kminval
			doprop

		when miscpropsym then
			case lx.subcode
			when 'b' then flag:=0; dogettype
			when 'e' then flag:=2; dogettype
			else
				p:=createunit1(jisvoid,p)
				p.flag:=lx.subcode<>'v'			!0/1 = isvoid/isdef
				lex()
			esac

		when dollarsym then
			if p.tag not in [jname,jdot] then
				serror("...name.$ needed")
			fi
			p:=createunit1(jsymbol,p)
			lex()

		else
			serror("Unknown dot suffix")
		esac
	od
	return p
end

function readslist(int &nparams, ftrailing=0)unit=
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
	nparams:=0

	skipsemi()
	if lx.symbol=rbracksym then		!empty list
		return ulist
	fi

	pushlisttype('PARAM')

int donulls:=1

	do
		skipsemi()
		case lx.symbol
		when commasym then
			serror("null comma expr not allowed")
		when rbracksym then
			exit
		else
			addlistunit(ulist,ulistx,readunit())
			++nparams
			if lx.symbol=commasym then
				lex()
				if lx.symbol=rbracksym then
					if nparams<>1 or not ftrailing then serror("Trailing comma") fi
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

	poplisttype()
	return ulist
end

function readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond

!case lx.symbol
	case lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif,readunit(),createunit1(jblock,p))
	when kunlesssym then
		lex()
		return createunit2(jif, createunit1(jnotl,readunit()),createunit1(jblock,p))
	else
		return p
	esac
end

function readkeyindex(unit p)unit=
!at '{'
!syntax is:
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	unit q

	lex()

	q:=readunit()

	if lx.symbol=commasym then
		lex()
		q.nextunit:=readunit()
	fi
	
	p:=createunit2(jkeyindex,p,q)

	skipsymbol(rcurlysym)
	return p
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

! (s||s|s)	!list comp [SYNTAX TO BE REVISED]
!return positioned at symbol following closing ")"
	unit ulist,ulistx, p,q,r
	int oldirp,length,lower,lowerseen,elemtype,opc

	lex()					!first symbol of first expression
	ulist:=ulistx:=nil
	length:=0
	lower:=1
	lowerseen:=0

	elemtype:=tvoid

	if lx.symbol=stdtypesym and nextlx.symbol=colonsym then
		elemtype:=lx.subcode
		lex()
		lex()
	fi

	if lx.symbol=intconstsym and nextlx.symbol=colonsym then
		lower:=lx.value
		lowerseen:=1
		lex()
		lex()
	fi

!check symbol after "("

	case lx.symbol
	when rbracksym then			!empty list
		lex()
		p:=createunit0(jmakelist)
		p.length:=0
		p.lower:=lower
		p.elemtype:=elemtype
		return p

	elsif (binopset[lx.symbol] or unaryopset[lx.symbol] or lx.symbol=propsym) and
			nextlx.symbol=rbracksym then
		if lx.symbol=addrsym then
			opc:=kappend
		else
			opc:=lx.subcode
		fi
doopc:
		p:=createunit0(joperator)
		p.pclop:=opc
		lex()
!		lex()
		skipsymbol(rbracksym)
		return p

	elsecase lx.symbol
	when specialopsym then
		case lx.subcode
		when '-' then opc:=kneg
		when '[]' then opc:=kindex
		else opc:=knop
		esac
		doopc
	when insym then
		opc:=kin
		doopc
	when inxsym then
		opc:=kinx
		doopc

	else					!assume normal expression follows
		p:=readxunit()
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()
		if lowerseen then
			p:=createunit2(jkeyvalue,createintunit(lower), p)
		fi

		return p

	when commasym then
		length:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(jmakelist,p)
			p.length:=length
			p.lower:=lower
			p.elemtype:=elemtype
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
			addlistunit(ulist,ulistx,readxunit())
			++length
			skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
		until lx.symbol<>commasym
		skipsymbol(rbracksym)
		p:=createunit1(jmakelist,ulist)
		p.length:=length
		p.lower:=lower
		p.elemtype:=elemtype
		return p

	when barsym then			!ifx/selectx expression; p is selector expression
		lex()
		q:=readxunit()
		case lx.symbol
		when barsym then		!(a|b|c)
			lex()
			r:=readsunit()
			skipsymbol(rbracksym)
			q.nextunit:=r
			return createunit2(jif,p,q)
		when rbracksym then
			lex()
			return createunit2(jif,p,q)

		esac

!assume selectx expression
		addlistunit(ulist,ulistx,q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist,ulistx,readxunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readxunit()
		skipsymbol(rbracksym)
		p.nextunit:=r
		return createunit2(jselect,p,ulist)

	when semisym then
		ulist:=ulistx:=p
		repeat
			skipsemi()
			if lx.symbol=rbracksym then
				exit
			fi
			addlistunit(ulist,ulistx,readunit())
		until lx.symbol<>semisym
		skipsymbol(rbracksym)
		return makeblock(ulist)


	else
		serror("(x ...")
	esac
	return nil
end

function readif:unit=
!at 'if'
	int line, kwd, lineno
	unit pthen,pcond, plist,plistx, pelse, p, pelsif

	line:=lx.pos

	kwd:=lx.symbol			!in case coming from elsecase etc

	lex()
	pcond:=readsunit()
	skipsemi()

	skipsymbol(kthensym)

	pthen:=readsunit()

	case lx.symbol
	when kelsifsym then
		lx.symbol:=kifsym		!for .kwd
		pelse:=readif()

	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kwd)
!		lex()
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
!	SERROR("ELSECASE NOT READY")
		pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kwd)
!		lex()
	esac

	pthen.nextunit:=pelse
	p:=createunit2(jif,pcond,pthen)
	p.pos:=line

	return p
end

proc checkend(int endkwd1, endkwd2=0, startline=0)=
!check end or end kwd1 or end kwd2, or is fi/esac/do
!return on symbol following any of that, which is expected to be semisym
	[256]char str

	skipsemi()

!symbol must be endsym or fi/esac/od which are endsym with subcode
!check I have end/fi/esac/cp
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

!now, should be semi, or possibly kwd1/2
	if lx.symbol in [endkwd1, endkwd2] then
		lex()
	elsif lx.symbol<>semisym then
		error
	fi
end

function readunless:unit=
	int line
	unit pcond, pthen, pelse, p
	line:=lx.pos
	lex()
	pcond:=readsunit()
	skipsymbol(kthensym)

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		pelse:=nil
	fi
	checkend(kunlesssym)
!	lex()
	pthen.nextunit:=pelse
	p:=createunit2(jif,createunit1(jnotl,pcond),pthen)
	p.pos:=line
	return p
end

function readwhile:unit=
	int pos
	unit pcond, pbody, p

	pos:=lx.pos
	lex()

	pcond:=readsunit(1)

	if lx.symbol=commasym then
		lex()
		pcond.nextunit:=readsunit(1)
	fi

	skipsymbol(kdosym)
	pbody:=readsunit()

	checkend(kwhilesym,kdosym)
!	lex()

	p:=createunit2(jwhile,pcond,pbody)
	p.pos:=pos
	return p
end

function readrepeat:unit=
	int pos
	unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	skipsymbol(kuntilsym)
	pcond:=readunit()
	p:=createunit2(jrepeat,pbody,pcond)
	p.pos:=pos
	return p
end

function readfor:unit=
!on 'for'; syntax is:
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

	int line, opc, down, isforeach
	unit pstep, pvar, pcond, pfrom, pto, pelse, pbody, p, plist,pvar2

	line:=lx.pos
	isforeach:=lx.subcode
	lex()			!skip 'for'
	pvar:=readterm2()

	if pvar.tag<>jname then
		serror("For: name expected")
	else
		pvar.def.forindex:=1
	fi

	opc:=jfor
	pstep:=nil
	pcond:=nil
	pvar2:=nil
	down:=0

	if lx.symbol=commasym then			!double index
		lex()
		pvar2:=readterm2()
	fi

	if lx.symbol=insym then	!assume in/inrev
		lex()
		plist:=readunit()

		case plist.tag
		when jmakerange then		!in a..b: simple iteration
			pfrom:=plist.a
			pto:=plist.b
		when jbounds then			!
			plist.flag:=1
			opc:=jforx
		else
			opc:=jforall
		esac
	else
		if lx.symbol=assignsym then
			lex()
			pfrom:=readunit()
		else
			pfrom:=createintunit(1)
		fi
		checksymbol(ktosym)
		down:=lx.subcode=1
		lex()
		pto:=readunit()

		if lx.symbol=kbysym then
			lex()
			pstep:=readunit()
			if pstep.tag<>jintconst then serror("BY needs int constant") fi
			if pstep.value<0 then 
				serror("Step must be positive")
			elsif pstep.value=0 then
				serror("Zero step")
			fi
			pstep.value:=abs pstep.value
			if pstep.value=1 then		!by 1
				pstep:=nil
			fi
		fi
	fi

	if lx.symbol=kwhensym then
		lex()
		pcond:=readunit()
	fi
	skipsymbol(kdosym)
	pbody:=readsunit()

	if pcond<>nil then
		pbody:=makeblock(createunit2(jif,pcond,pbody))
	fi
	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
		pbody.nextunit:=pelse
	else
		pelse:=nil
	fi
	checkend(kforsym,kdosym)
!	lex()


	case opc
	when jforall then
		pvar.nextunit:=plist
		plist.nextunit:=pvar2
		p:=createunit2(opc,pvar,pbody)

	when jforx then
		pvar.nextunit:=plist
		p:=createunit2(opc,pvar,pbody)
	else
		pvar.nextunit:=pfrom
		pfrom.nextunit:=pto
		pto.nextunit:=pstep
		p:=createunit2(opc,pvar,pbody)
	esac
	p.flag:=down

	if isforeach then
		if p.tag=jforall then
			p.tag:=jforeach
		else
			serror("Foreach?")
		fi
	fi

	p.pos:=line

	if pvar2 and opc<>jforall then
		serror("for i,j not allowed")
	fi

	return p
end

function readdo:unit=
	unit p
	int line

	line:=lx.pos
	lex()
	p:=readsunit()
	checkend(kdosym)
	p:=createunit1(jdo,p)
	p.pos:=line
	return p
end

function readto:unit=
	int line,id
	unit p, pcount, pbody

	line:=lx.pos
	lex()

	pcount:=readunit()

	skipsymbol(kdosym)
	pbody:=readsunit()
	checkend(ktosym,kdosym)
!	lex()

	pcount.nextunit:=createavname()

	p:=createunit2(jto,pcount,pbody)
	p.pos:=line
	return p
end

function makeblock(unit p)unit=
	return createunit1(jblock,p)
end

function readvardef(int isglobal=0, isstatic=0)unit=
!positioned at 'var' 'let'
	int nvars,varid, opc
	symbol d
	unit ulist, ulistx, p

!	m:=readtypespec(lx.symbol=kvarsym)
	lex()
!	M:=TVAR

!	if stcurrproc.nameid=procid then
	if stcurrproc.nameid in [procid,anonprocid] then
		varid:=(isstatic|staticid|frameid)
	else
		varid:=staticid
	fi
	nvars:=0
	ulist:=ulistx:=nil

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, varid, isglobal)
!		storemode(stcurrproc,m,&d.mode)

		lex()

		case lx.symbol
		when assignsym then
			opc:=lx.subcode
			if varid=staticid then
!				if stcurrproc.nameid=procid then
				if stcurrproc.nameid in [procid,anonprocid] then
					serror("Need '=' for static in proc")
				fi
			fi
			d.initcode:=(lx.symbol=assignsym|2|3)
			lex()
			d.code:=readunit()
			if varid=frameid then
				p:=createunit2(opc,createname(d),d.code)
				addlistunit(ulist, ulistx, p)
			fi

		when eqsym then
			if varid<>staticid then serror("Need ':=' for non-static") fi
			lex()

			d.initcode:=1
			d.code:=readunit()
		esac

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

proc readconstdef(int isglobal=0)=
!positioned at 'const'
	int nvars
	symbol d

	lex()

	nvars:=0
	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, constid, isglobal)
		lexchecksymbol(eqsym)
		lex()

		d.code:=readunit()

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No consts declared")
	fi
end

function readreturn:unit=
	unit p,q,r

	lex()
	q:=nil

	if exprstarterset[lx.symbol] then
		q:=readunit()
	fi
	p:=createunit1(jreturn, q)

	return readcondsuffix(p)
end

function readprint:unit=
	int opc, flags, fshowname, length
	unit pformat, pdev, printlist,printlistx, p,q
	ref strbuffer expr

	ichar s

	pushlisttype('PRINT')

	opc:=jprint
	flags:=lx.subcode

	if lx.symbol=questionsym then
		flags:=pr_newline
	elsif flags iand pr_format then
		opc:=jfprint
	fi

	lex()

	printlist:=printlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi
	if opc=jfprint then
		pformat:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

!if lx.symbol=semisym then
	if not exprstarterset[lx.symbol] then
		goto finish
	fi

	do
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(printlist,printlistx, createunit0(jnogap))
		when dollarsym then
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

				addlistunit(printlist,printlistx,q:=createstringunit(s,expr.length))
			fi
			addlistunit(printlist,printlistx,p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	unless flags iand pr_newline then
		if opc=jprint and printlist=nil or opc=jfprint and printlist=nil and pformat=nil then
			serror("No print items")
		fi
	end

	poplisttype()
	if opc=jfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		if pformat=nil then
			pformat:=makeblock(pformat)
		fi
		pformat.nextunit:=printlist
		p:=createunit2(opc,pdev,pformat)
	else
		p:=createunit2(opc,pdev,printlist)
	fi

	p.flag:=flags
	return p
end

function readread:unit=
	int opc, flags
	unit pformat, pdev, readlist, readlistx, p

	pushlisttype('PRINT')
	flags:=lx.subcode
	lex()

	readlist:=readlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		if opc=jread then
			serror("@ on read")
		fi
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

	if not exprstarterset[lx.symbol] then
		goto finish
	fi

	do
		p:=readunit()
		if lx.symbol=colonsym then
			lex()
			p:=createunit2(jfmtitem,p,readunit())
		fi
		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish:
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	poplisttype()
	p:=createunit2(jread,pdev,readlist)
	p.flag:=flags
	return p
end

function readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode
	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name,"all") then
		lex()
		p:=createunit1(opc,createintunit(0))

	elsif exprstarterset[lx.symbol] then
		p:=createunit1(jloop, readintunit())
	else
		p:=createunit1(jloop, createintunit(1))
	fi
	p.loopcode:=opc

	return readcondsuffix(p)
end

function readintunit:unit p=
	p:=readunit()
	if p.tag<>jintconst then
		serror("int expr needed")
	fi
	return p
end

function readswitchcase:unit=
	int pos, kwd, opc, lineno,rangeused, nwhen
	unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

	pos:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc
	opc:=lx.subcode			!pick up tag: kcase etc

	lex()

	skipsemi()
	if lx.symbol=kwhensym then
		if kwd=kswitchsym then
			serror("switch expr missing")
		fi
		pexpr:=CREATEUNIT0(JNONE)
	else
		pexpr:=readsunit()		!index expression
	fi

	pwhenlist:=pwhenlistx:=nil
	rangeused:=0
	nwhen:=0

	skipsemi()
	while lx.symbol=kwhensym do	!read list of when-then pairs
		pos:=lx.pos
		lex()
		pwhen:=pwhenx:=nil
		do
			p:=readunit()
			++nwhen
			p.pos:=pos
			if p.tag=jmakerange then rangeused:=1 fi
			addlistunit(pwhen,pwhenx,p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>kthensym then checksymbol(sendtosym) fi
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(jwhenthen,pwhen,pthen)
		pwhenthen.pos:=pos
		addlistunit(pwhenlist,pwhenlistx,pwhenthen)
	od

	case lx.symbol
	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()

		checkend(kwd)
!		lex()
	when kelsifsym then
		lx.symbol:=kwd
		pelse:=makeblock(readif())
	when kelsecasesym, kelseswitchsym then

		lx.symbol:=kwd
		pelse:=readswitchcase()
	else
		PELSE:=NIL
		checkend(kwd)
!		lex()
	esac

	pexpr.nextunit:=pelse

	p:=createunit2(opc,pexpr,pwhenlist)
	p.pos:=pos
	return p
end

function readgoto:unit=
	lex()

	return readcondsuffix(createunit1(jgoto,readunit()))
end

function readstop:unit=
	unit p
	int i
	lex()
	if exprstarterset[lx.symbol] then
		p:=createunit1(jstop,readunit())
	else
		p:=createunit1(jstop,createintunit(0))
	fi
	return readcondsuffix(p)
end

function readcast:unit p=
!just seem basic type name
	int t,opc, pclop

	t:=lx.subcode
	lex()

	if t=trange and lx.symbol=lbracksym then
		lex()
		p:=readunit()
		if p.tag in [jkeyvalue,jkeyword] then
SERROR("MAKERANGELEN")
!			p.tag:=jmakerangelen
		elsif p.tag=jmakerange then
		else
			serror("need a..b or a:n")
		fi
		skipsymbol(rbracksym)
		return p
	fi


!check for standalone value
	case lx.symbol
	when atsym,lbracksym then

	else						!convert to typeconst
		p:=createunit0(jtypeconst)
		p.mode:=t
		return p
	esac

	if lx.symbol=atsym then
		lex()
		opc:=jtypepun
		pclop:=ktypepun
	else
		opc:=jconvert
		pclop:=kconvert
	fi
	checksymbol(lbracksym)
	p:=readterm()

	p:=createunit1(opc,p)
	p.pclop:=pclop
	storemode(stcurrproc,t,&p.mode)
	return p
end

function readset:unit=
!positioned at "["
	int length,nkeyvalues,oldinparamlist
	unit p,ulist,ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset,nil)
	when colonsym then
		lexchecksymbol(rsqsym)
		lex()
		return createunit1(jmakedict,nil)
	esac

	pushlisttype('DICT')

	p:=readunit()
	length:=1
	nkeyvalues:=0
	if p.tag=jkeyvalue then ++nkeyvalues fi

	ulist:=ulistx:=p

	while lx.symbol=commasym do
		lex()
		if lx.symbol=rsqsym then exit fi		!allow trailing comma
		addlistunit(ulist,ulistx,p:=readunit())
		if p.tag=jkeyvalue then ++nkeyvalues fi

		++length
		skipsemi()						!allow a,b,c;]
	od

	skipsymbol(rsqsym)

	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict,ulist)
	else
		p:=createunit1(jmakeset,ulist)
	fi
	p.length:=length
	poplisttype()
	return p
end

global proc readtabledef(int isglobal=0)=
!at 'tabledata' symbol
	int i,ncols,nrows,enums,nextenumvalue,startline, firstvalue
	int ltype,lower
	byte commas:=0, semis:=0
	unit ulist,ulistx, plower, p
	const maxcols=20
	[maxcols]symbol varnames
	[maxcols]unit plist,plistx
	symbol d, nameptr

	const maxrows=500

	enums:=lx.subcode						!whether there is an enums column
	lex()

	firstvalue:=nextenumvalue:=1
	
	nrows:=0			!number of data rows appearing
	ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
	while lx.symbol=namesym do
		if ++ncols>maxcols then
			serror("tabledata/too many columns")
		fi
		varnames[ncols]:=lx.symptr

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
	startline:=lx.pos

	skipsemi()

	for i:=1 to ncols do
		plist[i]:=plistx[i]:=nil
	od
	ulist:=ulistx:=nil

	intabledata:=1
	do			!loop per row
		skipsemi()
		if ncols>0 then
			skipsymbol(lbracksym)
		fi
		if ++nrows>maxrows then
			serror("tabledata:too many rows")
		fi

		if enums then
			checksymbol(namesym)

			d:=addsymbol(stcurrproc,lx.symptr,enumid, isglobal)
			lex()

			case lx.symbol
			when eqsym then
				if nrows>1 then serror("tabledata '=' not 1st") fi
				lex()
				p:=readunit()
				if p.tag=jintconst then
					firstvalue:=nextenumvalue:=p.value
				else
					SERROR("TABLEDATA: COMPLEX ENUM VAL")
				fi
			esac

			d.index:=nextenumvalue++

			tabledataname:=d.name

			if ncols then				!comma always expected
				skipsymbol(commasym)		!check it
			fi
		fi

		for i:=1 to ncols do
			addlistunit(plist[i],plistx[i],readunit())
			if i=ncols then
				skipsymbol(rbracksym)
			else
				skipsymbol(commasym)
			fi
		od

		case lx.symbol
		when commasym then
			++commas
			lex()
!			if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
			if lx.symbol=kendsym then exit fi		!allow trailing comma on last entry
		else
			skipsemi()
			if lx.symbol=kendsym then exit fi
			++semis
		esac
!		if lx.symbol<>commasym then exit fi
!		lex()					!should be ( for next entry
!		if lx.symbol=closesym then exit fi		!allow trailing comma on last entry
	od

	if semis and commas then serror("mixed commas") fi

	intabledata:=0

	skipsemi()
	checkend(ktabledatasym,startline:startline)

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

!!for each variable, add a vardef initialised to the list
!!add the decls for the vars
!
	for i:=1 to ncols do
!
		d:=addsymbol(stcurrproc,varnames[i],staticid,isglobal)

		p:=d.code:=createunit1(jmakelist,plist[i])
		p.length:=nrows
		p.lower:=firstvalue
	od
end

function readtry:unit=
	unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
	lex()

	ptry:=readsunit()
	pexceptlist:=pexceptlistx:=nil			!list of kexcept items

	while lx.symbol=kexceptsym do
		lex()
		exlist:=exlistx:=nil				!list of exception codes for this 'except'
		do
			addlistunit(exlist,exlistx,readunit())
			if lx.symbol<>commasym then exit fi
			lex()
		od
		skipsymbol(kthensym)
		px:=readsunit()
		addlistunit(pexceptlist,pexceptlistx,createunit2(jexcept,exlist,px))
	od
	checkend(ktrysym)
!	lex()

	return createunit2(jtry,ptry,pexceptlist)
end

function readsprint:unit=
	int opc, flags, isfprint
	unit pformat, pdev, printlist, printlistx, p

	pushlisttype('PRINT')
	opc:=jprint
	flags:=lx.subcode

	lexchecksymbol(lbracksym)
	lex()

	isfprint:=flags iand pr_format

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
			addlistunit(printlist,printlistx,createunit0(jnogap))
		else
			p:=readunit()
			if lx.symbol=colonsym then
				lex()
				p:=createunit2(jfmtitem,p,readunit())
			fi
			addlistunit(printlist,printlistx,p)
		fi
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

finish:
	lex()
	if (opc=jprint or opc=jfprint) and printlist=nil then
		serror("No print items")
	fi

	poplisttype()
	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		pformat.nextunit:=printlist
		p:=createunit2(opc,pdev,pformat)
	else
		p:=createunit2(opc,pdev,printlist)
	fi
	p.flag:=flags
	return p
end

function readsread:unit=
!to work an item at a time:
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
	int opc
	unit pformat,pdev,p, readlist,readlistx

	pushlisttype('PRINT')
	opc:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	readlist:=readlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		if opc=jread then
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
			p:=createunit2(jfmtitem,p,readunit())
		fi
		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

	finish:
	lex()
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	poplisttype()

	return createunit2(opc,pdev,readlist)
end

proc readimportdll=
!at 'importdll'
	[256]char str
	symbol stproc,d, stname
	int startpos, isfunc, isnew, libtype

	libtype:=lx.subcode

	lexchecksymbol(namesym)
	stname:=lx.symptr

	lexchecksymbol(eqsym)
	lex()

!check for an existing dll with the same name, as this could be adding to it

	isnew:=1
	d:=stname.nextdupl
!	while d do
!		if d.nameid=dllmoduleid then
!			stname:=d
!			isnew:=0
!			exit
!		fi
!		d:=d.nextdupl
!	od
	for i to nlibfiles do
		if eqstring(libtable[i].name, stname.name) then
			stname:=libtable[i]
			isnew:=0
			exit
		fi
	od

	if isnew then			!new
!		stname:=addsymbol(stprogram,stname,dllmoduleid,0)
		stname:=addsymbol(nil,stname,dllmoduleid,0)
		if nlibfiles>=maxlibfile then
			serror("Too many DLL libs")
		fi

		libtable[++nlibfiles]:=stname
		libtypes[+nlibfiles]:=libtype
		stname.index:=nlibfiles
	fi

	currdllindex:=stname.index

	startpos:=lx.pos
!------------------------------------
	do
		skipsemi()

		case lx.symbol
		when kprocsym,kfunctionsym then
			isfunc:=lx.symbol=kfunctionsym
			lex()
			case lx.symbol
			when namesym then
				stproc:=addsymbol(stcurrproc, lx.symptr, dllprocid, 1)

			when stringconstsym then
				strcpy(str,lx.svalue)
				convlcstring(str)
				stproc:=addsymbol(stcurrproc, addglobalname(str), dllprocid, 1)
				stproc.truename:=pcm_copyheapstring(lx.svalue)
			else
				serror("fn name expected")
			esac

			stproc.misfunc:=isfunc
			stproc.isimport:=1

			if ndllprocs>=maxdllproc then
				serror("Too many DLL procs")
			fi
			dllproctable[++ndllprocs]:=stproc
			dllproclibindex[ndllprocs]:=currdllindex
			stproc.index:=ndllprocs

			lex()

			if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
				lexchecksymbol(namesym)

				d:=addsymbol(stproc.owner, lx.symptr, aliasid, 1)
				d.alias:=stproc
				lex()
			fi
			readffiparams(stproc)
		when ktypesym then
			readtypedef(1)
		when kendsym then
!			if nextlx.symbol=kimportdllsym then lex() fi
			exit
		else
			readpackvars(stcurrproc, dllvarid)

		esac
	od	
!--------------------------------
	checkend(kimportdllsym, startline:startpos)
end

proc readffiparams(symbol stproc)=
!at first symbol after func name
!return list of units with dllparam defs (can be empty)
!if there is a result type, then head of list will be a return def type
	int pret,ptype

	if lx.symbol=lbracksym then
		lex()
		if lx.symbol=rbracksym then
			lex()
		else
			ptype:=readtypespec()
			if lx.symbol in [commasym,rbracksym] then		!types only
				readtypeparams(stproc,ptype)
			else
				readtypenameparams(stproc,ptype)
			fi
		fi
	fi

	if lx.symbol in [colonsym,sendtosym] then
		if not stproc.misfunc then serror("Return type for proc?") fi
		lex()
	fi

	pret:=tvoid
	if stproc.misfunc then
		if lx.symbol=semisym then serror("Return type missing") fi
		pret:=readtypespec()
	fi

	storemode(stproc.owner,pret, &stproc.mode)
end

proc readtypeparams(symbol stproc, int ptype)=
!at symbol after ptype
	[32]char str
	int nparams
	symbol stname


	nparams:=0

	do
		++nparams
		print @str,"$",,nparams

		stname:=addsymbol(stproc, addglobalname(str), dllparamid, 0)
		storemode(stproc,ptype,&stname.mode)
		++stproc.nparams

		if lx.symbol=commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi
			ptype:=readtypespec()
		else
			exit
		fi
	od
	skipsymbol(rbracksym)
end

proc readtypenameparams(symbol stproc, int ptype)=
!at symbol after ptype
	symbol stname

	checksymbol(namesym)
	stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
	storemode(stproc,ptype,&stname.mode)
	++stproc.nparams
	lex()

	do

		if lx.symbol=eqsym then
			lex()
			stname.code:=readunit()
			stname.moptional:=1
		fi

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi

			if istypestarter() then			!new type
				ptype:=readtypespec()
			fi
			checksymbol(namesym)
			stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
			storemode(stproc,ptype,&stname.mode)
			++stproc.nparams
			lex()
		else
			exit
		esac
	od
	skipsymbol(rbracksym)
end

global proc readrecorddef(int isglobal, symbol d)=
!at 'record' symbol
	int kwd, baseclass, m, startline, caligned
	byte lbopening:=0
	symbol nameptr

	baseclass:=0
	if d then			!entry from 'type name=record...'
		kwd:=ktypesym
		goto gotname
	fi

	kwd:=lx.symbol

	lexchecksymbol(namesym)
	nameptr:=lx.symptr

	lex()

	if lx.symbol=lbracksym then
		lex()
		baseclass:=readtypespec()
		skipsymbol(rbracksym)
	fi

	checkequals()
	lex()


	d:=addsymbol(stcurrproc, nameptr, (kwd=krecordsym|recordid|typeid), isglobal)

	if baseclass then
		if baseclass>0 then serror("baseclass?") fi
		if nbaseclasses>=255 then
				serror("Too many base classes")
		fi
		++nbaseclasses
		storemode(stcurrproc,baseclass,&baseclasstable[nbaseclasses])
		d.baseclassindex:=nbaseclasses
		baseclassdef[nbaseclasses]:=d
	fi

gotname:

	skipsemi()
	startline:=lx.pos

	if lx.symbol=lbracksym then lbopening:=1; lex() fi
	if kwd=krecordsym then
		m:=readrecordbody(d)
	else
		caligned:=0
		m:=readstructbody(d,caligned)
	fi

	if lbopening then
		checksymbol(rbracksym)
		lex()
	else
		checkend(krecordsym,startline:startline)
	fi
end

function readrecordbody(symbol owner)int=
!at first symbol of a class or record body (after 'type T=record',
! or after 'record T ='
!read fields, constants, types, methods.
!create initially anonymous record type, and return type code
!caller will attached to named type as needed.

!int kwd
	symbol oldstcurrproc, e
	int m, nfields

	m:=addanontype()

	oldstcurrproc:=stcurrproc
	stcurrproc:=owner

	docase lx.symbol
	when kconstsym then
		readconstdef(0)
	when kvarsym then
		readrecordfields(owner)
	when kfunctionsym,kprocsym then
		readprocdef(0)

	when krecordsym then
		readrecorddef(0, nil)
	when ktypesym then
		lex()
		serror("CLASS TYPE")
	when kendsym,rbracksym,rcurlysym then
		exit
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()
!	elsif istypestarter() and nextlx.symbol<>lbracksym then
!		readrecordfields(owner)
!
	else
		serror("Unknown record field decl")
	end

	e:=owner.deflist
	nfields:=0
	while e, e:=e.nextdef do
		if e.nameid=fieldid and not e.atfield then
			++nfields
		fi
	od

	owner.nfields:=nfields

	ttfields[m]:=owner.deflist
	ttlength[m]:=nfields
	ttlower[m]:=1
	ttbasetype[m]:=trecord

	createusertype(owner, m)

	e:=owner.deflist
	while e do
		addgenfield(e)
		e:=e.nextdef
	od

	ttsize[m]:=varsize*owner.nfields

	stcurrproc:=oldstcurrproc

	return m
end

proc readrecordfields(symbol owner)=
!positioned at 'var'; read one line of var defs for a record
	int nvars,offset,index
	symbol d

!	m:=readtypespec(1)
	lex()

	nvars:=0
	index:=owner.nfields

	d:=owner.deflist

	offset:=0
	while d, d:=d.nextdef do
		if d.nameid=fieldid and not d.atfield then
			offset+:=varsize
		fi
	od

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, fieldid, 0)
		d.atfield:=nil

		lex()

		if lx.symbol=atsym then
			lex()
			d.atfield:=readatfield()
			d.fieldoffset:=d.atfield.fieldoffset
			d.index:=d.atfield.index
		else
			d.fieldoffset:=offset
			offset+:=varsize
			d.index:=++index
		fi

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od


	if nvars=0 then
		serror("No fields")
	fi

!	stcurrproc.nfields+:=nvars
end

function readstructbody(symbol owner, int caligned)int=
	int m, ngroups, nvars, t
	symbol d, e

	m:=addanontype()

	ngroups:=0

	do
		skipsemi()

		case lx.symbol
		when kstructsym then
			++ngroups
			lex()
			addstructflag(owner,structblockid)

		when kunionsym then
			++ngroups
			lex()
			addstructflag(owner,unionblockid)

		when kendsym then
			if nextlx.symbol in [kstructsym, kunionsym] then lex() fi
doend:
			if ngroups then
				--ngroups
				lex()
				addstructflag(owner,endblockid)
			else
				exit
			fi
		when rbracksym then
			doend

!		when rbracksym then
!			exit

		else
			readpackvars(owner, structfieldid)
!			t:=readtypespec(0)
!
!			nvars:=0
!			while lx.symbol=namesym do
!				++nvars
!				d:=addsymbol(owner, lx.symptr,structfieldid, 0)
!				storemode(owner,t,&d.mode)
!				lex()
!
!				if lx.symbol<>commasym then
!					exit
!				fi
!				lexchecksymbol(namesym)
!			od
!			if nvars=0 then serror("struct decl?") fi
!			owner.nfields:=nvars
		esac
	od

	ttfields[m]:=owner.deflist
	ttlength[m]:=owner.nfields
!CPL =OWNER.NFIELDS

	ttlower[m]:=1
	ttcaligned[m]:=caligned
	ttbasetype[m]:=tstruct

	createusertype(owner, m)

	e:=owner.deflist
	while e do
		case e.nameid
		when structblockid, unionblockid, endblockid then
		else
			addgenfield(e)
		esac
		e:=e.nextdef
	od

	return m
end

proc addstructflag(symbol owner, int id)=
	static int structseqno
	[32]char str

	fprint @str,"$$#",++structseqno

	addsymbol(owner, addglobalname(str),id, 0)
end

proc readprocdef(int isglobal)=
!at 'proc' etc symbol; read proc def or declaration
	int kwd,startline, nparams, shortfun
	unit pcode
	symbol d, oldstcurrproc
	[256]char str

	kwd:=lx.symbol
	shortfun:=lx.subcode
	lexchecksymbol(namesym)
!
	if stcurrproc.nameid in [procid,anonprocid] then
		serror("Nested proc")
	fi

	oldstcurrproc:=stcurrproc			!usually module, but could be a record
	stcurrproc:=d:=addsymbol(stcurrproc,lx.symptr,procid,isglobal)

	addproc(d)

	lex()

	d.mode:=tvoid

	if lx.symbol=lbracksym then		!possible params
		lex()
		if lx.symbol<>rbracksym then
			readparams(d)
		else
			lex()
		fi
	fi

	checkequals()
	lex()

	startline:=lx.pos

	if not shortfun then
		d.code:=readsunit()
		checkend(kwd,startline:startline)
	else
		d.code:=readunit()
		checksymbol(semisym)
!		lex()
	fi

	if eqstring(d.name,"start") then
		currmodule.startfn:=d
	elsif eqstring(d.name,"main") then
		currmodule.mainfn:=d
	fi

	stcurrproc.misfunc:=kwd=kfunctionsym

	stcurrproc:=oldstcurrproc
end

function readatfield:symbol=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	symbol p,d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=stcurrproc.deflist
	while p do
		if eqstring(p.name,d.name) then
			return p
		fi

		p:=p.nextdef
	od
	serror_s("Can't find @ field",d.name)
	return nil
end

function istypestarter:int=
	case lx.symbol
	when stdtypesym, krefsym, kvarsym, lsqsym then
		return 1
	elsif lx.symbol=namesym then
		if nextlx.symbol=namesym then
			return 1
		fi
	esac
	return 0
end

proc readmacrodef(int isglobal)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported

	symbol stmacro, stname, owner

	lexchecksymbol(namesym)

	stmacro:=addsymbol(stcurrproc, lx.symptr, macroid, isglobal)
	owner:=stmacro

	lex()

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=addsymbol(owner,lx.symptr,macroparamid,0)
					stname.firstdupl:=lx.symptr

					lex()
					if lx.symbol=rbracksym then
						exit
					fi
					skipsymbol(commasym)
				else
					serror("macro def params")
				esac
			od
		fi
		lex()
	fi

	checkequals()
	lex()
	stmacro.code:=readunit()
end

function readhostparams(unit lhs,int isfn)unit=
!hostfn name has been read
!lhs is not null when lhs.hostfn(...) has been used
!currently at hostfn symbol
	int fnindex, nargs
	unit p,q

	fnindex:=lx.subcode
	lexchecksymbol(lbracksym)
	lex()

	q:=readslist(nargs)

	skipsymbol(rbracksym)

	if lhs then
		lhs.nextunit:=q
		q:=lhs
	fi

	p:=createunit1(jcallhost,q)
	p.index:=fnindex

!	poplisttype()

	return p
end

proc pushlisttype(int ltype)=
	if nlisttype>=maxlisttype then
		serror("listtype overflow")
	fi
	listtypestack[++nlisttype]:=listtype
	listtype:=ltype
end

proc poplisttype=
	listtype:=listtypestack[nlisttype--]
end

function readcompilervar:unit=
	[100]char str
	rsystemtime tm
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	locrec loc

	case lx.subcode
	when cv_lineno then
		loc.pm:=currmodule
		setlineno(&loc, lx.sourceoffset)
		return createintunit(loc.lineno)

	when cv_strlineno then
		loc.pm:=currmodule
		setlineno(&loc, lx.sourceoffset)
		strcpy(str, strint(loc.lineno))

	when cv_modulename then
		strcpy(&.str,currmodule.name)

	when cv_filename then
!		strcpy(&.str,modules[currmoduleno].filename)
!		strcpy(&.str,currmodule.name)
		strcpy(&.str,currmodule.filespec)
	when cv_function then
		strcpy(&.str,(stcurrproc|stcurrproc.name|"<none>"))
	when cv_date then
		os_getsystime(&tm)
		fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"
!
	when cv_time then
		os_getsystime(&tm)
		fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

!	when jcvversion then x:=compilerversion
!	when jcvpclversion then x:=pclversion
	else
		serror("compiler var not impl")
	esac

	return createstringunit(pcm_copyheapstring(&.str))
end

function readpair(int tag, pclop=knop)unit p=
!should be at '(', but check
!read (a,b) and produce (opc, a, b ) unit

	ref unitrec a,b

	lexchecksymbol(lbracksym)
	lex()
	a:=readexpression()
	skipsymbol(commasym)
	b:=readexpression()

	if lx.symbol=commasym and tag=jmap then			!allow 3rd item
		lex()
		b.nextunit:=readexpression()
	fi
	skipsymbol(rbracksym)
	p:=createunit2(tag, a,b)
	p.pclop:=pclop
	return p
end

global proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

proc readtypedef(int isglobal)=
!at 'type'
	int ptype
	symbol d

	lexchecksymbol(namesym)
	d:=addsymbol(stcurrproc, lx.symptr, typeid, isglobal)

	lexchecksymbol(eqsym)
	lex()	

	if lx.symbol=krecordsym then
		lex()
		d.nameid:=recordid
		readrecorddef(isglobal, d)
		return
	fi

	ptype:=readtypespec(owner:d)

!CPL "USERTYPE",PTYPE,D.NAME,=NTYPES,STRMODE(PTYPE),STRMODE(NTYPES)

	createusertype(d, ptype)
end

function readtypespec(int allowvar=0, symbol owner=nil)int=
!full=1 to allow structdefs

	int flags, arraycode, oldipl
	int a,b,t, startline, caligned
	symbol d
	const maxdim=10
	[maxdim]unit lowerdims,lengthdims
	int ndims
	unit x,lowerx, upperx, lengthx

	case lx.symbol
	when lsqsym then
dolsq:
		lex()
		ndims:=0
		pushlisttype(0)
		do
			lowerx:=lengthx:=nil
			if lx.symbol=rsqsym or lx.symbol=commasym then		![]
			else
				x:=readunit()
				if x.tag=jmakerange then			![a..b] variable
					lowerx:=x.a
					upperx:=x.b
					if lowerx.tag=jintconst and upperx.tag=jintconst then
						lengthx:=createintunit(upperx.value-lowerx.value+1)
					else
						lengthx:=createunit2(jbin,upperx,lowerx)
						lengthx.pclop:=ksub
						lengthx:=createunit2(jbin,lengthx,createintunit(1))
						lengthx.pclop:=kadd
					fi
				else
					case lx.symbol
					when rsqsym,commasym then			![n]
						lengthx:=x
					when colonsym then				!a:n
						lowerx:=x
						lex()
						if not (lx.symbol=commasym or lx.symbol=rsqsym) then
							lengthx:=readunit()
						fi
					esac
				fi
			fi
			lowerdims[++ndims]:=lowerx
			lengthdims[ndims]:=lengthx
			exit when lx.symbol<>commasym
			lex()
		od
		skipsymbol(rsqsym)
		poplisttype()
		t:=readtypespec()

		for i:=ndims downto 1 do
			t:=makeaxtype(t,lowerdims[i],lengthdims[i])
		od
		return t

	when krefsym then
		lex()

		if lx.symbol=stdtypesym and lx.subcode=tvoid then
			lex()
			return makereftype(tvoid,owner)
		else
			return makereftype(readtypespec(),owner)
		fi

	when namesym then
		d:=lx.symptr
		lex()
		if lx.symbol=dotsym then
			lexchecksymbol(namesym)
			t:=newusertypex(d,lx.symptr)
			lex()
			return t
		else
			return newusertypex(d)
		fi

	when stdtypesym then
		case lx.subcode
		when tpackstrz then				!stringz
			lex()
			if lx.symbol=mulsym then
				lex()
				return makestrtype(tpackstrz, readunit())
			else
				return tstringz
			fi

		when tpackstrc then
			lexchecksymbol(mulsym)
			lex()
			return makestrtype(tpackstrc,readunit())

		when tarray then
			lexchecksymbol(lsqsym)
			goto dolsq

		else
			t:=lx.subcode
			case t
			when tint then t:=ti64
			when treal then t:=tr64
			esac

			lex()
			return t
		esac

	when krecordsym then
		if owner=nil then serror("anon record") fi
		lex()
		startline:=lx.pos
		t:=readrecordbody(owner)

		checkend(krecordsym,startline:startline)
		return t

	when kstructsym then
		if owner=nil then serror("anon struct") fi
		lex()
		caligned:=0
		if lx.symbol=kcalignedsym then
			caligned:=1
			lex()
		fi

		startline:=lx.pos
		t:=readstructbody(owner,caligned)

		checkend(kstructsym,startline:startline)
		return t

!	when kvarsym then
!		if not allowvar then
!			serror("var types not allowed")
!		fi
!		lex()
!		if lx.symbol=colonsym then
!			lex()
!			return readtypespec(0)
!		fi
!		return tvar
	else
		serror("Type expected")
	esac

	return t
end

proc readparams(symbol stproc)=
!just after '('
	int isbyref,isoptional
	symbol d

!CPL "READPARAMS_NAMES"

!assume one or more params
	isbyref:=isoptional:=0

	do
		if lx.symbol=addrsym then
			++isbyref
			lex()
		fi
		if lx.symbol=questionsym then
			++isoptional
			lex()
		fi
		checksymbol(namesym)
		d:=addsymbol(stproc, lx.symptr, paramid,0)
!		d.mode:=tvar
		++stproc.nparams

		lex()

		if lx.symbol=eqsym then
			isoptional:=1
!			if isbyref+isoptional then serror("Mixed/dupl &/?/=") fi
			lex()
			d.code:=readunit()
		fi

		if isbyref and isoptional then serror("Mixed byref/optional") fi

		d.mbyref:=isbyref
		d.moptional:=isoptional

		isbyref:=isoptional:=0

		if lx.symbol=commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi
		else
			exit
		fi
	od

	skipsymbol(rbracksym)
end

func checkoperator:unit p=
	int opc

	if nextlx.symbol in [commasym, rbracksym, semisym] then
		p:=createunit0(joperator)

		if lx.symbol=specialopsym then
			case lx.subcode
			when '-' then opc:=kneg
			when '[]' then opc:=kindex
			else opc:=knop
			esac
			p.pclop:=opc
		else
			p.pclop:=lx.subcode
		fi
		lex()
		return p
	fi
	nil
end

func readlambda:unit p=
!at {
	[100]symbol params
	symbol oldstcurrproc, stproc, d
	[20]char str
	int nparams
	byte byref

	case stcurrproc.nameid
	when procid then
!	when procid, anonprocid then
	when anonprocid then serror("Nested {}")
	else serror("{} not in fn")
	esac

	oldstcurrproc:=stcurrproc

	print @str,"$F",,++nextlambdaindex
	stproc:=addsymbol(stcurrproc, addnamestr(str), anonprocid, 0)
	stcurrproc:=stproc
	addproc(stproc)

	lex()
	nparams:=0
	byref:=0
	if lx.symbol=addrsym then lex(); byref:=1 fi

	if lx.symbol=namesym and nextlx.symbol in [commasym, colonsym] then
		do
			checksymbol(namesym)

			d:=addsymbol(stproc, lx.symptr, paramid, 0)
!*!			d.pindex:=++nparams
			params[++nparams]:=d
			d.mbyref:=byref
			byref:=0

			lex()

			if lx.symbol<>commasym then exit fi
			lex()
		od
		checksymbol(colonsym)
		lex()
	fi

	stproc.nparams:=nparams
	stproc.misfunc:=1

!CPL =NPARAMS
!
!CPL "READ LAMBDA",STPROC.NAME, STPROC.MISFUNC

!*!	stproc.isfunc:=1
!*!	getparamoffsets(&params, nparams)

!read body of lambda
	stproc.code:=readsunit()
	skipsymbol(rcurlysym)

!	p:=createunit1(jmakeclosure, createname(stproc))
	p:=createname(stproc)

	stcurrproc:=oldstcurrproc
	return p
end

proc readpackvars(symbol owner, int id)=
!expected to be typed var-decl inside a struct or importdll/lib body
	int t, nvars
	symbol d

	t:=readtypespec(0)

	nvars:=0
	while lx.symbol=namesym do
		++nvars
		d:=addsymbol(owner, lx.symptr,id, 0)
		storemode(owner,t,&d.mode)
		lex()

		if lx.symbol<>commasym then
			exit
		fi
		lexchecksymbol(namesym)
	od
	if nvars=0 then serror("bad decl?") fi
end
=== qq_pcltabs.m 0 0 9/16 ===
global enumdata [0:]ichar opndnames=
							!   PCL1		PCL2
	(cnone=0,	$),
	(cvar,		$),			! d Symbol		Address of static/frame variable
	(cproc,		$),			! p Symbol		Address of pccode entry point
	(cdllproc,	$),			! x Int			Int Index into dllproc table

	(cgenfield,	$),			! g Symbol		Index into genfieldtable

	(clabel,	$),			! l Label no	Address of pccode instruction
	(cint,		$),			! i
	(creal,		$),			! r
	(cstring,	$),			! s Stringz		Address of static Object with string
	(cstringz,	$),			! z Stringz
	(ctype,		$),			! t Int			Typecode
	(csymbol,	$),			! d Symbol		Symbol
	(coperator,	$),			! o Int			Operator
	(cmaths,	$),			! m Int			Mathsop
	(chost,		$),			! h Int			Host index

	(clast,		"?")
end

!these aliases are used so that the enum table is tidier
const p = cproc
const v = cvar
const l = clabel
const x = cdllproc
const g = cgenfield
const i = cint
const r = creal
const s = cstring
const z = cstringz
const t = ctype
const d = csymbol
const o = coperator
const m = cmaths
const h = chost

global type pcl = ref pclrec

global record pclrec =
	byte opcode
	union						! Attribute codes
		byte condcode			! c		for jumpcc: eq_cc etc
		byte boolcode			! b		for jumpc: eq_cc etc
		byte isframe			! f		for pushv etc: whether framevar
		byte n					! n		nargs/etc
	end
	byte mode					! t		0/void, or optional type annotation info, or pushas code
	byte haslabel				!       1 when this instr is referenced as a table
	i16 x, y					! x y	Misc

	union						! Main operand codes
		symbol	def				! d v p
		i64		value			! i g
		u64		uvalue			! w
		r64		xvalue			! r
		ichar	svalue			! s
		int		labelno			! l
		pcl		labelref		! l
		i64		offset			!
		object	objptr			! z
		i64		typecode		! z
		byte	mathscode		! m		for kmaths/2: mm_sqrt etc
		byte	bintocode		! o		for addto etc: kadd etc
		i64		hostindex		! x		
		struct
			i32	usertag			! u		!these attributes overlap main operand
			i32	usertag2		! v
		end
	end
end

global enumdata  [0:]ichar pclnames, [0:]byte pclopnd, [0:]u32 pclattrs =
	(knop = 0,  $,  0, '    '),   ! simple nop
	(kskip,     $,  0, '    '),   ! ignore on pcl listing

	(kprocdef,  $,  p, '    '),   ! 
	(kprocent,  $,  0, 'n   '),   ! n=number of locals; 
	(kprocend,  $,  0, '    '),  
	(kendprog,  $,  0, '    '),
	(kcomment,  $,  0, '    '),
                                   
	(kpushv,    $,  v, 'f   '),   ! Push v
	(kpushvref, $,  v, 'f   '),   ! push &v
	(kpopv,     $,  v, 'f   '),   ! v := Z

	(kpushci,   $,  i, '    '),   ! Push i
	(kpushvoid, $,  0, '    '),   ! Push void 
	(kpushnil,  $,  0, '    '),   ! Push nil (ref void)
	(kpushcr,   $,  r, '    '),   ! Push r

	(kpushcs,   $,  s, '    '),   ! Push constant string object

	(kpushtype, $,  t, '    '),   ! Push type constant
	(kpushopc,  $,  o, '    '),   ! Push operator constant
	(kpushsym,  $,  d, '    '),   ! Push symbol reference

	(kpushptr,  $,  0, '    '),   ! Z' := Z^
	(kpopptr,   $,  0, '    '),   ! Z^ := Y

	(kzpopv,    $,  d, 'f   '),   ! v := Z; don't free v first

	(kdupl,     $,  0, '    '),   ! (Z',Y') := (share(Z), Z)
	(kcopy,     $,  0, '    '),   ! Z' := deepcopy(Z)
	(kswap,     $,  0, '    '),   ! swap(Z^, Y^)

	(kconvrefp, $,  0, '    '),   ! Change ref in Z to refpacked

	(kjump,     $,  l, '    '),   ! Jump to L
	(kjumpptr,  $,  0, '    '),   ! Jump to Z

	(kjumpc,    $,  l, 'b   '),   ! Jump to L when Z is b (true/false)

	(kjumpcc,   $,  l, 'c   '),   ! Jump to L when Y c Z

	(kjumpwhen, $,  l, 'b   '),   ! b=true:  Y = Z:  pop both, jump to L
								  !          Y <> Z: pop Z only; don't jump
								  ! b=false: Y = Z:  pop both; don't jump
								  !          Y <> Z  pop Z only; jump to L

	(kjumplab,  $,  l, '    '),   ! Jumptable entry

!	(kswitch,   $,  l, 'xy  '),   ! Jumptable has y-x+1 entries
	(kswitch,   $,  0, 'xy  '),   ! Jumptable has y-x+1 entries

	(ktov,      $,  v, 'f   '),   ! 

	(kforvci,   $,  l, '    '),   ! ++v; jump to l when v<=i in next 2 ops: pushv/pushci
	(kforvv,    $,  l, '    '),   ! ++v; jump to l when v<=v in next 2 ops

	(kfordvci,  $,  l, '    '),   ! 
	(kfordvv,   $,  l, '    '),   !  

	(kcallproc, $,  p, 'n   '),   ! Call &A; n is no. args
	(kcallptr,  $,  0, 'n   '),   ! Call X^; n is no. of params supplied; x is stack adjust
	(kreturn,   $,  0, 'n   '),   ! n is no. params to free; Return from function, with optional value in caller's retval slot
	(kpopret,   $,  i, '    '),   ! pop stack to caller's return slot; i = offset

	(kmodcall,  $,  d, '    '),   ! 
	(kmodret,   $,  0, '    '),   ! 

	(kcalldll,  $,  x, 'n   '),   ! Call dll function d (sysmbol); n=nargs

	(kcallhost, $,  h, '    '),   ! Call Q host function h (Host index)

	(kunshare,  $,  0, 'n   '),   ! Unshare and pop A var values on stack
	(kaddsp,    $,  0, 'n   '),   ! SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)

	(kstop,     $,  0, 'n   '),   ! Stop program with stopcode Z; n=1 to stop runproc instead

	(kmakelist, $,  0, 'xy  '),   ! x items on stack; make list with lwb y
	(kmakevrec, $,  0, 'xu  '),   ! x items on stack; make record of type u
	(kmakeax,   $,  0, 'xyuv'),   ! x items on stack; make array with lwb y, type u and elemtype v
	(kmakebits, $,  0, 'xyuv'),   ! x items on stack; make bits with lwb y, type u and elemtype v
	(kmaketrec, $,  0, 'xu  '),   ! x items on stack; make struct with type u
	(kmakeset,  $,  0, 'x   '),   ! x items on stack; make set
	(kmakerang, $,  0, '    '),   ! 2 items on stack; make range
	(kmakedict, $,  0, 'x   '),   ! x*2 items on stack (x key:val items); make dict
	(kmakedec,  $,  0, '    '),   ! Turn string on stack to decimal number

	(kincrptr,  $,  0, 'x   '),   ! Z^ +:= x
	(kincrtov,  $,  v, 'fx  '),   ! v +:= x
	(kloadincr, $,  0, 'x   '),   ! T := Z^; Z^ +:= x; Z' := T
	(kincrload, $,  0, 'x   '),   ! Z^ +:= x; Z' := Z^

	(kneg,      $,  0, '    '),   ! Z':= -Z
	(kabs,      $,  0, '    '),   ! Z' := abs Z
	(knotl,     $,  0, '    '),   ! Z' := not Z
	(kinot,     $,  0, '    '),   ! Z' := inot Z
	(kistruel,  $,  0, '    '),   ! Z' := istrue Z
	(kasc,      $,  0, '    '),   ! Z' := asc(Z)
	(kchr,      $,  0, '    '),   ! Z' := chr(Z)

	(kmaths,    $,  m, '    '),   ! Z' := op(Z)
	(kmaths2,   $,  m, '    '),   ! Z' := op(Y, Z)

	(kunaryto,  $,  o, '    '),   ! Z^ op:= Z
	(knotlto,   $,  0, '    '),   ! Z^ not:= Z

	(klen,      $,  0, '    '),   ! Z' := Z.len
	(klwb,      $,  0, '    '),   ! Z' := Z.lwb
	(kupb,      $,  0, '    '),   ! Z' := Z.upb
	(kbounds,   $,  0, 'n   '),   ! Z' := Z.bounds; n=1: one range value; n=2: two dims
	(kbytesize, $,  0, '    '),   ! Z' := Z.bytesize

	(ktype,     $,  0, 'n   '),   ! Z' := n=0/1/2 = basetype/elemtype
	(kdictsize, $,  0, '    '),   ! Z' := Z.dictsize
	(kisfound,  $,  0, '    '),   ! Z' := Z.isfound
	(kminval, 	$,  0, '    '),   ! Z' := Z.minvalue
	(kmaxval, 	$,  0, '    '),   ! Z' := Z.maxvalue

	(kistype,   $,  t, '    '),   ! Z' := Z.type/etc = t
	(kisvoid,   $,  0, 'n   '),   ! Z' := Z.isvoid (n=0) or not Z.isdef (n=1)
	(kconvert,  $,  t, '    '),   ! Z' := t(Z)
	(ktypepun,  $,  t, '    '),   ! Z' := t@(Z)

	(kadd,      $,  0, '    '),   ! Z' := Y + Z
	(ksub,      $,  0, '    '),   ! Z' := Y - Z
	(kmul,      $,  0, '    '),   ! Z' := Y * Z
	(kdiv,      $,  0, '    '),   ! Z' := Y / Z
	(kidiv,     $,  0, '    '),   ! Z' := Y % Z
	(kirem,     $,  0, '    '),   ! Z' := Y rem Z
	(kidivrem,  $,  0, '    '),   ! (Y', Z') := Y divrem Z
	(kiand,     $,  0, '    '),   ! Z' := Y iand Z
	(kior,      $,  0, '    '),   ! Z' := Y ior Z
	(kixor,     $,  0, '    '),   ! Z' := Y ixor Z
	(kshl,      $,  0, '    '),   ! Z' := Y << Z
	(kshr,      $,  0, '    '),   ! Z' := Y >> Z
	(kin,       $,  0, 'n   '),   ! Z' := Y in Z (n=0) or Y not in Z (n=1)
	(kinx,      $,  0, '    '),   ! Z' := Y inx Z
	(kcmp,      $,  0, 'c   '),   ! Z' := Y c Z
	(kmin,      $,  0, '    '),   ! Z' := min(Y, Z)
	(kmax,      $,  0, '    '),   ! Z' := max(Y, Z)
	(kconcat,   $,  0, '    '),   ! Z' := concat(Y, Z) or Y && Z
	(kappend,   $,  0, '    '),   ! Z' := append(Y, Z) or Y & Z

	(kpower,    $,  0, '    '),   ! Z' := Y ** Z

	(kbinto,    $,  o, '    '),   ! Y^ op:= Z

	(kandlto,   $,  0, '    '),   ! Y^ and:= Z
	(korlto,    $,  0, '    '),   ! Y^ or:= Z
	(kconcatto, $,  0, '    '),   ! Y^ concat:= Z or Y^ &&:= Z
	(kappendto, $,  0, '    '),   ! Y^ append:= Z or Y^ &:= Z

	(kdot,      $,  g, '    '),   ! Z' := Z.g
	(kpopdot,   $,  g, '    '),   ! Z.g := Y
	(kdotref,   $,  g, '    '),   ! Z' := &Z.g

	(kindex,    $,  0, '    '),   ! Z' := Y[Z]
	(kpopix,    $,  0, '    '),   ! Z' := Y[Z]:=X
	(kindexref, $,  0, '    '),   ! Z' := &Y[Z]

	(kkeyindex, $,  0, '    '),   ! Z' := X{Y, Z}
	(kpopkeyix, $,  0, '    '),   ! Y{Z} := X
	(kkeyixref, $,  0, '    '),   ! Z' := &X{Y, Z}

	(kdotix,    $,  0, '    '),   ! Z' := Y.[Z]
	(kpopdotix, $,  0, '    '),   ! Y.[Z] := X
	(kdotixref, $,  0, '    '),   ! Z' := &Y.[Z]

	(kexpand,   $,  0, 'n   '),   ! Z' := Expand Z into n objects are needed

	(kpushtry,  $,  l, 'xy  '),   ! Push try/except into; label/except code/no. exceptions
	(kraise,    $,  0, 'x   '),   ! Raise exception Z
	(kmap,      $,  0, '    '),   ! Z' := map(Y, Z)

	(klastpcl,  $,  0, '    ')
end

=== qq_pclgen.m 0 0 10/16 ===
!not opcodes, just used internally here for conditional jump logic
const kjumpt = 1
const kjumpf = 0

!loop stack data reused by GENMPL
const maxloopindex=20
[maxloopindex, 4]int loopstack
[maxloopindex]int trylevelstack
global int loopindex=0
int looptrylevel			!return by findlooplabel

const maxswitchrange=512
const maxlocals=300
const maxparams=100

const maxunits=400					!for constructors
int trylevel=0
!int currfunction=0				!0/1/2 = not a function/proc/function

!vars within curr procdef
int retindex						!common return point; label no
int retvaloffset					!offset of return value for procs (as stack slots)
int nprocparams						!no. of params
!global int nproclocals				!no. of locals
!global ref int pproclocals			!pointer to pcl operand of kprocentry; may need updating
const retaddrslots = 1				!+1 or +2, added to param indices (depends on return info size)
int procskiplabel

global proc evalunit(unit p, int res=1)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
!note: sometimes res can be 2, (passing on a res=2 from an outer stmt)
!that should be treated here same as 1 (res=2 has special meaning from pclhasvalue[] only)
	unit a, b
	symbol d
	int procflag, index

	qpos:=p.pos

	a:=p.a
	b:=p.b

	switch p.tag
	when jintconst then
		genpc_int(kpushci, p.value)

	when jrealconst then
		genpc_real(kpushcr, p.xvalue)

!	when jenumconstthen
!		genpc_int2(kpushenum, p.value, p.mode)

	when jnone then

	when jstringconst then
		pushstring(p.svalue, p.length)

	when jname then
		d:=p.def
		case d.nameid
		when paramid then
			genpc_name(kpushv, d)
			if d.mbyref then
				genpc(kpushptr)
			fi

		when frameid, staticid then
			genpc_name(kpushv, d)

		when labelid then
			if not res then
				if d.labelno=0 then
					d.labelno:=createfwdlabel()
				fi
				genpc_lab(kjump, d.labelno)
				return
			else
				genpc_name(kpushsym, d)
			fi

!		when dllvarid then
!			genpc_name(kpushx, d)

		else
			genpc_name(kpushsym, d)
		esac

	when jsymbol then			!assume a is jname
		if a.tag=jname then
			genpc_name(kpushsym, a.def)
		else
			gerror(".$ name expected")
		fi

	when jblock then
		if a then
			while a and a.nextunit do
				evalunit(a, 0)
				a:=a.nextunit
			od
			if a then
				evalunit(a, res)
			fi
		else
!			gerror("empty block")
		fi

	when jdecimal then
		pushstring(p.svalue, p.length)
		genpc(kmakedec)

	when jcall then
		do_call(p, a, b, res, procflag)
	when jreturn then
		do_return(p, a)
	when jcallhost then
		do_callhost(p, a, res)

	when jassign then
		do_assign(a, b, res, p.flag)
	when jto then
		do_to(p, a, b)
	when jif then
		do_if(p, a, b, b.nextunit, res)
	when jfor		then
		do_for(p, a, b
)
	when jforx then
		do_forx(p, a, b)

	when jforall, jforeach then
		do_forall(p, a, b)

	when jwhile then
		do_while(p, a, b)

	when jrepeat then
		do_repeat(p, a, b)

	when jgoto then
		if a.tag=jname and a.def.nameid=labelid then
			d:=a.def
			if d.labelno=0 then
				d.labelno:=createfwdlabel()
			fi
			genpc_lab(kjump, d.labelno)
		else
			evalunit(a)
			genpc(kjumpptr)
		fi

	when jlabeldef then
		d:=a.def
		if d.labelno=0 then
			d.labelno:=definelabel()
		else
			index:=d.labelno
			definefwdlabel(index)
		fi

	when jloop then
		do_loop(p)

	when jdo then
		do_do(p, a)
	when jcase, jdocase then
		do_case(p, a, b, res)
	when jswitch, jdoswitch then
		do_switch(p, a, b, res)
	when jswap then
		evalref(a)
		evalref(b)
		genpc(kswap)

	when jselect then
		do_select(a, b, res)
	when jprint then
		do_print(p, a, b)
	when jfprint then
		do_fprint(p, a, b, b.nextunit)
	when jread then
		do_read(p, a, b)

	when jstop then
		if a then
			evalunit(a)
		else
			genpc_int(kpushci, 0)
		fi
		genpc(kstop)

	when jtry then
		do_try(p, a, b)

	when jandl then
		do_andl(a, b)
	when jorl then
		do_orl(a, b)
	when jmakelist then
		do_pushlist(a, p.length)
		genpc_xy(kmakelist, p.length, p.lower)

	when jmakeset then
		do_pushlist(a, p.length)
		genpc_xy(kmakeset, p.length)

	when jmakedict then
		do_makedict(a, p.length)

	when jkeyvalue then
		evalunit(a)
		evalunit(b)

	when jmap then
		do_map(p, a, b)

	when jbin then
		case p.pclop
		when kidiv then
		do_idiv(a, b)
		when kirem then
		do_irem(a, b)
		else
			evalunit(a)
			evalunit(b)
			genpc(p.pclop)
		esac

	when jbinto then
		evalref(a)
		evalunit(b)
		genpc(kbinto)
		pccurr.bintocode:=p.pclop

	when junary then
		evalunit(a)
		genpc(p.pclop)

	when junaryto then
		evalref(a)
		genpc(kunaryto)
		pccurr.bintocode:=p.pclop

	when jdot then! do_bin(a, b, kdot)
		evalunit(a)
		genpc_name(kdot, b.def)

	when jindex then
		do_bin(a, b, kindex)

	when jdotindex then
		do_bin(a, b, kdotix)

	when jkeyindex then
		evalunit(a)
		evalunit(b)
		if b.nextunit then
			evalunit(b.nextunit)
		else
			genpc(kpushvoid)
		fi
		genpc(kkeyindex)

	when jptr then
		evalunit(a)
		genpc(kpushptr)

	when jaddrof then
		if p.flag=1 then			! ^x
			if a.tag=jptr then			!^a^ cancel out (a might be byref param)
				evalunit(a.a)
			else
				evalref(a)
			fi
		else
			evalref(a)
			genpc(kconvrefp)
		fi

	when jconvert then
		do_convert(p)

	when jtypepun then
		evalunit(a)
		genpc_int(ktypepun, p.mode)

	when jtypeconst then
		genpc_int(kpushtype, p.mode)

	when joperator then
		genpc_int(kpushopc, p.pclop)

	when jincrload, jloadincr then
		do_incr(p, a, res)
!
	when jnil then
		genpc(kpushnil)

	when jraise then
		evalunit(a)
		genpc(kraise)

	when jvoid then
		genpc(kpushvoid)

	when jeval then
		evalunit(a)
		genpc_n(kunshare, 1)

	else
		gerror_s("UNSUPPORTED TAG:", JTAGNAMES[P.TAG], p)
	end switch

	case jhasvalue[p.tag]
	when 0 then
		if res then
			gerror_s("Value expected:", jtagnames[p.tag])
		fi
	when 1 then
		if not res then
			if p.tag=jcall and procflag=1 then		!procs have no ret value
			elsif p.tag in [jincrload, jloadincr] then
			elsif p.tag=jcallhost and hostisfn[p.index]=0 then
			else
				genpc_n(kunshare, 1)
			fi
		fi
	esac						!else ignore when 2, as already dealt with
end

global proc gencodemodule(isubprog sp, int moduleno)=
	const maxanonprocs=100
	[maxanonprocs]symbol anonprocs
	int nanonprocs:=0

	symbol d, e
	int lab
	int a:=sp.firstmodule
	int b:=sp.lastmodule
	ifile pm:=modules[moduleno]
	pcl pc

	currmodule:=pm
	stcurrproc:=stcurrmodule:=currmodule.def

CPL "GENCODE", SP.NAME, PM.NAME

	resetpcl(pm.size)

!GOTO FINISH

	gencomment("Module data init code:")
!SKIP

	qpos:=0
	qpos.[24..31]:=moduleno

!CPL "///////////////", QPOS

!jump around stop/raise block needed for reentry
!	if n=1 then
	if moduleno=a then
		lab:=createfwdlabel()
		genpc_lab(kjump, lab)
		genpc_n(kstop, 1)
		stopseq:=pccurr

		raiseseq:=pccurr+1
		genpc_int(kpushci, 0)
		genpc(kraise)
		definefwdlabel(lab)
	fi

	d:=stcurrmodule.deflist
	while d do
		if d.nameid=staticid and d.code then
			evalunit(d.code)
			if d.initcode=3 then
				genpc(kcopy)
			fi
			genpc_name(kzpopv, d)
		elsif d.nameid=procid then
			e:=d.deflist
			while e do
				if e.nameid=staticid and e.code then
					evalunit(e.code)
					genpc_name(kzpopv, e)
				elsif e.nameid=anonprocid then
					if nanonprocs>=maxanonprocs then gerror("Too many anons") fi
					anonprocs[++nanonprocs]:=e
				fi
				e:=e.nextdef
			od
		fi
		d:=d.nextdef
	od	

	if moduleno=a then
		for i:=b downto a+1 do
			genpc_name(kmodcall, modules[i].def)
		od
		for i:=b downto a+1 do
			if modules[i].startfn then
				genpc_name(kcallproc, modules[i].startfn)
!				genopnd_int(0)
			fi
		od

		if currmodule.startfn then
			genpc_name(kcallproc, currmodule.startfn)
!			genopnd_int(0)
		fi

		if currmodule.mainfn then
			genpc_name(kcallproc, currmodule.mainfn)
!!			genopnd_int(0)
		fi

		evalunit(stcurrmodule.code, 0)
		genpc_int(kpushci, 0)
		genpc(kstop)
	else
		evalunit(stcurrmodule.code, 0)
		genpc(kmodret)
	fi

	gencomment("Procs:")
	d:=stcurrmodule.deflist
	while d do
		switch d.nameid
		when procid, anonprocid then
			do_procdef(d)
		when staticid then
!		when typeid then
		when recordid then
			e:=d.deflist
			while e, e:=e.nextdef do
				if e.nameid=procid then
					do_procdef(e)
				fi
			od

		when constid then
		when enumid then
		when labelid then
		when typeid then
		when dllprocid then
		when aliasid then
		when macroid then
		when dllvarid then
		else
			gerror_s("?Module def:", namenames[d.nameid])
		end switch

		d:=d.nextdef
	od	

	for i to nanonprocs do
		do_procdef(anonprocs[i])
	od

	genpc(kendprog)

!scan pcl operands and convert label operands to pcl ref
!	pc:=pcstart
!	while pc<=pccurr, ++pc do
!		if pclopnd[pc.opcode]=clabel then
!			lab:=pc.labelno
!			if labelpctable[lab]=nil then
!				gerror_s("Lab undef:",strint(lab))
!			fi
!			pc.labelref:=labelpctable[lab]
!			CPL "UPDATING LABEL",LAB,"TO:",PC.LABELREF,PC.LABELREF-PCSTART+1
!		fi
!	od

	pm.pcstart:=pcstart
!	pm.pcend:=pcend
	pm.pcend:=pccurr
	pm.pcsize:=pccurr-pcstart+1
	pm.pcsourcestart:=pcsourcestart

!CPL "------DONE GENPCL", PCLSTART, PCLNEXT-PCLSTART, PCLNAMES[PCLSTART^]

end

proc do_procdef(symbol p) =
	int nfreevars, nnofreevars
	int isfunc
	symbol oldcurrproc

	oldcurrproc:=stcurrproc			!might be a method

	stcurrproc:=p

	retindex:=createfwdlabel()
	isfunc:=p.misfunc

	genprocentry(p, nfreevars, nnofreevars)

	if p.code=nil then
		gerror_s("Empty proc body", p.name)
	else
		evalunit(p.code, isfunc)

		if isfunc then
!CPL "CHECK BODY", JTAGNAMES[P.CODE.TAG]
			if not checkblockreturn(p.code) then
				gerror("Func: return value missing")
			fi
		fi

	fi

	definefwdlabel(retindex)			!common return point
	genprocexit(nfreevars, nnofreevars, isfunc)
	genpc(kprocend)


!	if pproclocals^=0 then
!		p.labelno:=procskiplabel
!	fi
CPL "PROCDEF/PROCLOCALS"

	stcurrproc:=oldcurrproc
end

proc genprocentry(symbol p, int &nfreevars, &nnofreevars) =		!GENPROCENTRY
	[200]char str
	int n
	symbol d

	genpc_name(kprocdef, p)

	nprocparams:=nproclocals:=0

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			++nproclocals
			d.index:=nproclocals
		when paramid then
			++nprocparams
		esac

		d:=d.nextdef
	od

	d:=p.deflist
	n:=nprocparams

	while d, d:=d.nextdef do
		case d.nameid
		when paramid then
			--n
			d.index:=-(n+retaddrslots)
		esac

	od

	retvaloffset:=-(nprocparams+retaddrslots)
!
	p.labelno:=definelabel()
	genpc_n(kprocent, nproclocals)
	procskiplabel:=definelabel()

	pproclocals:=pccurr-1

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			if d.code then
				evalunit(d.code)
				if d.initcode=3 then
					genpc(kcopy)
				fi
				genpc_name(kzpopv, d)
			fi
		esac

		d:=d.nextdef
	od
end

proc genprocexit(int nfree, nnofree, isfunc)=		!GENPROCEXIT
	int offset

	if isfunc then
		offset:=-(nprocparams+1)*varsize
		genpc_int(kpopret, offset)
	fi
	if nproclocals then
		genpc_n(kunshare, nproclocals)
	fi

	genpc_n(kreturn, nprocparams)
end

proc evalref(unit p)=
	unit a, b, c
	symbol d
	int lab1, lab2
	a:=p.a
	b:=p.b

	switch p.tag
	when jname then
		d:=p.def
		if d.nameid in [procid, dllprocid] then
			gerror("^ not allowed")
		fi	

		if d.nameid=paramid and d.mbyref then
			genpc_name(kpushv, d)
		else
			genpc_name(kpushvref, d)
		fi

	when jdot then! do_binref(a, b, kdotref)
		evalunit(a)
		genpc_name(kdotref, b.def)
	when jindex then! do_binref(a, b, kindexref)
		evalunit(a)
		evalunit(b)
		genpc(kindexref)

	when jdotindex then! do_binref(a, b, kdotindexref)
!		evalunit(a)
		evalref(a)
		evalunit(b)
		genpc(kdotixref)

	when jkeyindex then! do_binref(a, b, kkeyindexref)
		evalunit(a)
		evalunit(b)
		if b.nextunit then gerror("Def val not allowed") fi
		genpc(kkeyixref)

	when jptr then
		evalunit(a)

	when jif then
		lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
		lab2:=createfwdlabel()

		genjumpcond(kjumpf, p.a, lab1)
		evalref(p.b)
		genjumpl(lab2)
		definefwdlabel(lab1)
		evalref(p.b.nextunit)
		definefwdlabel(lab2)
	else
!		case p.tag
!		when jif then
!			do_if(p, a, b, c, 1)
!		when jlongif then
!			do_longif(p, a, b, 1)
!		when jselect then
!			do_select(p, a, b, c, 1)
!		when jswitch then
!			do_switch(p, a, b, c, 0, 1)
!		when jcase then
!			do_case(p, a, b, c, 0, 1)
!		else
!			PRINTUNIT(P)
			gerror_s("evalref", jtagnames[p.tag])
!		esac
	end switch
end

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r, s
	int oldpos, lab2, i

	q:=p.a
	r:=p.b

	switch p.tag
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

	when jnotl then
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
		evalunit(q)
		evalunit(r)
		gcomparejump(opc, p.condcode, lab)

	when jcmpchain then
		r:=q.nextunit
		i:=1
		if opc=kjumpf then
			while r do
				evalunit(q)
				evalunit(r)
				gcomparejump(kjumpt, revconds[p.cmpconds[i]], lab)
				++i
				q:=r
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(q)
				evalunit(r)
				if r.nextunit then
					gcomparejump(kjumpt, revconds[p.cmpconds[i]], lab2)
				else
					gcomparejump(kjumpt, p.cmpconds[i], lab)
				fi
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else
		evalunit(p)
		genpc_lab(kjumpc, lab)
		pccurr.boolcode:=opc
	end switch
	qpos:=oldpos

end

proc gcomparejump(int opc, cond, lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode

	if opc=kjumpf then				!need to reverse condition
		cond:=revconds[cond]		!eq_cc => ne_cc, etc
	fi

	genpc_lab(kjumpcc, lab)
	pccurr.condcode:=cond
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	genpc_lab(kjump, lab)
end

global proc stacklooplabels(int a, b, c)=
!list of labels associated with a loop: a/b/c are redo/next/exit
	if loopindex>=maxloopindex then
		gerror("Too many nested loops")
	fi

	++loopindex
	loopstack[loopindex, 1]:=a
	loopstack[loopindex, 2]:=b
	loopstack[loopindex, 3]:=c
end

global proc unstacklooplabels=
	--loopindex
end

global function findlooplabel(int k, n)int=
!k is 1, 2, 3, 4 for label A, B, C, D
!n is a 1, 2, 3, etc, according to loop nesting index
	int i

	if n=0 then			!outermost loop
		i:=1
	else
		i:=loopindex-(n-1)		!point to entry
	fi

	if i<1 or i>loopindex then
		gerror("Bad loop index")
	fi

	looptrylevel:=trylevelstack[i]
	return loopstack[i, k]
end

proc do_assign(unit a, b, int res, deepcopy=0)=
	unit q
	int n

	if a.tag=b.tag=jmakelist then
		if res then gerror("mult/ass::=") fi
!		if deepcopy then gerror("mult/ass::=") fi
		do_multassign(a, b, deepcopy, res)
		return
	fi

	evalunit(b)
	if deepcopy then
		genpc(kcopy)
	fi

	do_store(a, res)
end

proc do_bin(unit a, b, int opc)=
	evalunit(a)
	evalunit(b)
	genpc(opc)
end

proc do_binref(unit a, b, int opc)=
	evalref(a)
	evalunit(b)
	genpc(opc)
end

proc do_unary(unit a, int opc)=
	evalunit(a)
	genpc(opc)
end

proc do_unaryref(unit a, int opc)=
	evalref(a)
	genpc(opc)
end

proc do_pushlist(unit a, int n)=
	while a, a:=a.nextunit do
		evalunit(a)
	od
end

proc do_makedict(unit a, int n)=
	to n do
		if a.tag=jkeyvalue then
			evalunit(a.a)
			evalunit(a.b)
		else
			gerror("dict not key:val")
		fi
		a:=a.nextunit
	od
	genpc_xy(kmakedict, n)
end

proc do_call(unit p, a, b, int res, &procflag)=
	int nargs, nsimple, isfunc, kwdindex
	symbol d
	unit c
	[maxparams]unit arglist

	isfunc:=1
	nargs:=nsimple:=0
	kwdindex:=0
	c:=b

	while c do
		arglist[++nargs]:=c
		if c.tag in [jintconst, jrealconst] then ++nsimple fi
		if c.tag=jkeyword then
			if kwdindex=0 then kwdindex:=nargs fi
		elsif kwdindex then
			gerror("Non-kwd follows kwd arg")
		fi
		c:=c.nextunit
	od

	case a.tag
	when jname then
		d:=a.def
retry:
		case d.nameid
		when procid, anonprocid then
			if d.misfunc then
				genpc(kpushvoid)
				nargs:=pushparams(d, arglist, nargs, kwdindex)
!				genpc_name(kcallfn, d)
				genpc_name(kcallproc, d)
			else					!proc call
				isfunc:=0
				nargs:=pushparams(d, arglist, nargs, kwdindex)
				genpc_name(kcallproc, d)
			fi
			pccurr.n:=nargs

		when dllprocid then
			if not d.misfunc then
				isfunc:=0
			else
				genpc(kpushvoid)
			fi
			nargs:=pushparams(d, arglist, nargs, kwdindex)
			genpc_name(kcalldll, d)
			pccurr.n:=nargs

		when aliasid then
			d:=d.alias
			goto retry
		when staticid, frameid, paramid then
			goto docallptr
!
		else
			gerror_s("CAN'T CALL:", namenames[d.nameid])
		esac
	when jdot then
		if kwdindex then docallptr fi		!share error
		genpc(kpushvoid)
		evalref(a.a)					!push &self arg
		for i to nargs do				!any extra ones
			evalunit(arglist[i])
		od
		evalunit(a)						!push lhs again, this time for dot
		genpc_n(kcallptr, ++nargs)

	else
docallptr:
		if kwdindex then gerror("Kwd params not allowed for fnptr") fi
		genpc(kpushvoid)
		for i to nargs do
			evalunit(arglist[i])
		od
		evalunit(a)
		genpc_n(kcallptr, nargs)
	esac

	if res and not isfunc then
		gerror("Func ret value expected")
	fi

	procflag:=not isfunc
end

function pushparams(symbol d, []unit &arglist, int nargs, kwdindex)int=
!push args for a known, named function
!will deal with missing/optional args, default values, and keyword params
!should work also for dll procs
!In all cases, first nparams items in d.deflist will be parameter names, 
!For dlls with no named params, the entries will be $1 etc.

	int nparams, extra, n
	[maxparams]symbol paramlist
	[maxparams]byte byreflist
	symbol e, p

	nparams:=d.nparams
	e:=d.deflist
	n:=0
	while e do
		++n
		paramlist[n]:=e
		byreflist[n]:=e.mbyref
		e:=e.nextdef
	od

	if kwdindex then
		pushkwdparams(d, arglist, nargs, kwdindex)
		return d.nparams
	fi

	extra:=0

	if nargs=nparams then
		for i to nargs do
			evalparam(arglist[i], byreflist[i])
		od
		return nargs
	elsif nargs<nparams then	!trailing args missing
		for i to nargs do
			evalparam(arglist[i], byreflist[i])
		od

		for i:=nargs+1 to nparams do
			p:=paramlist[i]
			if not p.code and not p.moptional then
				gerror_s("Param not optional:", strint(i))
			fi
			if p.code then
				if byreflist[i] then gerror("byref with default val") fi
				evalunit(p.code)
			else
				genpc(kpushvoid)
			fi
		od
		return nparams
	else						!nargs>nparams: variadic
		for i to nparams do
			evalparam(arglist[i], byreflist[i])
		od

		if not d.mvarparams then
			gerror("Too many args")
		fi
		for i:=nparams+1 to nargs do
			evalunit(arglist[i])			!o/p variadic args
		od
		return nargs
	fi
end

proc evalparam(unit a, int byref)=
	if byref then
		evalref(a)
	else
		evalunit(a)
	fi
end


proc pushkwdparams(symbol d, []unit &arglist, int nargs, kwdindex)=
	int nparams, i, j, k
	[maxparams]symbol paramlist
	[maxparams]byte byreflist
	[maxparams]unit keyunits
	unit p, q
	symbol e

	nparams:=d.nparams

	e:=d.deflist
	for i to nparams do
		paramlist[i]:=e
		byreflist[i]:=e.mbyref
		e:=e.nextdef
	od

	if nargs>nparams then
		gerror("Too many args")
	fi

	for i:=kwdindex to nparams do
		keyunits[i]:=nil			!indicate param not set
	od

	for i to kwdindex-1 do			!do positional params
		evalparam(arglist[i], byreflist[i])
	od

	for i:=kwdindex to nargs do
		p:=arglist[i]
		q:=p.a
		if q.tag<>jname then gerror("kwd not a name") fi
		e:=q.def
		k:=0
		for j:=1 to nparams do
			if eqstring(e.name, paramlist[j].name) then
				k:=j
				exit
			fi
		od

		if k=0 then gerror_s("Can't find kwd param:", e.name) fi
		if k<kwdindex then gerror_s("Kwd arg already positional:", e.name) fi
		if keyunits[k] then gerror_s("Repeating kwd arg:", e.name) fi

		keyunits[k]:=p.b
	od

	for i:=kwdindex to nparams do
		if keyunits[i]=nil then
			q:=paramlist[i].code
			if q=nil and not paramlist[i].moptional then
				gerror_s("Param not optional:", strint(i))
			fi
			keyunits[i]:=q			!q is nil when default value not set
		fi
	od

!	for i:=nparams downto kwdindex do
	for i:=kwdindex to nparams do
		if keyunits[i] then
			evalparam(keyunits[i], byreflist[i])
		elsif byreflist[i] then
			gerror("byref param not optional")
		else
			genpc(kpushvoid)
		fi
	od
end

proc do_if(unit p, a, b, pelse, int res)=
	int lab1, lab2

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)

	if pelse or res then lab2:=createfwdlabel() fi	!label past else part

	genjumpcond(kjumpf, a, lab1)

	evalunit(b, res)

	if pelse or res then
		genjumpl(lab2)
		definefwdlabel(lab1)
		if pelse then
			evalunit(pelse, res)
		else
			genpc(kpushvoid)
		fi
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_do(unit p, a)=
	int lab_abc, lab_d, lab_test
	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_d)

	evalunit(a, 0)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_loop(unit p) =
	int n, index

	index:=p.a.value
	if index=0 then index:=loopindex fi

	n:=findlooplabel(p.loopcode, index)
	if n=0 then
CPL "BAD LOOP"
!		gerror("Bad exit/loop index", p)
	else
		genjumpl(n)
	fi
end

proc do_to(unit p, pcount, pbody)=
	int lab_b, lab_c, lab_d
	symbol temp
	unit pav

	pav:=pcount.nextunit
	temp:=pav.def

	evalunit(pcount)
	genpc_name(kzpopv, temp)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_b, lab_c, lab_d)

!check for count being nonzero
	if pcount.tag<>jintconst then			!assume const limit is non-zero
		genpc_name(kpushv, temp)
		genpc_int(kpushci, 0)
		genpc_lab(kjumpcc, lab_d)
		pccurr.condcode:=le_cc

	elsif pcount.value<=0 then		!const <=0, skip body
		genpc_lab(kjump, lab_d)
	fi

	definefwdlabel(lab_b)
	evalunit(pbody, 0)
	definefwdlabel(lab_c)

	genpc_lab(ktov, lab_b)
	genopnd_name(temp)

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_while(unit p, pcond, pbody) =
	int lab_b, lab_c, lab_d, lab_incr
	unit pincr:=pcond.nextunit

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

	evalunit(pbody, 0)

	definefwdlabel(lab_c)

	if pincr then
		evalunit(pincr)
		definefwdlabel(lab_incr)
	fi

	genjumpcond(kjumpt, pcond, lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p, a, b) =
	int lab_b, lab_c, lab_d

	lab_b:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_b, lab_c, lab_d)

	evalunit(a, 0)

	definefwdlabel(lab_c)

	unless b.tag=jintconst and b.value=0 then
		genjumpcond(kjumpf, b, lab_b)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_for(unit p, pvar, pbody)=
! a = pvar, pfrom, pto, [pstep]
! b = pbody [pelse]
	unit pfrom, pto, pstep, pelse, plimit, pautovar
	symbol dvar, limitvar
	int lab_b, lab_c, lab_d, lab_e, opc, oldqpos
	int step, fromval, limit, jumpinto

	pfrom:=pvar.nextunit
	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pautovar:=nil
	if pstep then
		gerror("By N not implem")
	fi

	pelse:=pbody.nextunit

	dvar:=pvar.def

	if pto.tag not in [jintconst, jname] or
		 pto.tag=jname and pto.def.isframe<>dvar.isframe then
		pautovar:=createavnamex(stcurrproc)
	fi

	if p.flag then
		step:=-1
	else
		step:=1
	fi

	jumpinto:=1			!assume jumping straight into increment

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(lab_b, lab_c, lab_d)

	if pfrom.tag=jintconst then		!can incr/decr directly
		fromval:=pfrom.value
!see if limit is known
		if pto.tag=jintconst then
			limit:=pto.value
			if (step=-1 and fromval>=limit) or (step=1 and fromval<=limit) then 	!at least 1 iteration
				jumpinto:=0
			fi
		fi
		if jumpinto then
			if step<0 then
				++fromval
			else
				--fromval
			fi
			pfrom.value:=fromval
		fi
		genpc_int(kpushci, pfrom.value)

		genpc_name(kpopv, dvar)
	else
		evalunit(pfrom)
		genpc_name(kpopv, dvar)

		genpc_name(kincrtov, dvar)
		pccurr.x:=step

	fi

	if pautovar then
		evalunit(pto)
		limitvar:=pautovar.def
		genpc_name(kzpopv, limitvar)
		pto:=pautovar
	else
		limitvar:=pto.def
	fi

	if jumpinto then
		genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
	fi
	definefwdlabel(lab_b)

	evalunit(pbody, 0)				!do loop body

	definefwdlabel(lab_c)

	if pto.tag=jintconst then
		opc:=(step<0|kfordvci|kforvci)
	elsif dvar.isframe=limitvar.isframe then
		opc:=(step<0|kfordvv|kforvv)
	else
		gerror("for:mixed m/f vars")
	fi

	oldqpos:=qpos
	qpos:=p.pos
	genpc_lab(opc, lab_b)
	qpos:=oldqpos

	genpc_name(kpushv, dvar)

	if pto.tag=jintconst then
		genpc_int(kpushci, pto.value)
	else
		genpc_name(kpushv, limitvar)
	fi

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse, 0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_forx(unit p, pvar, pbody)=
! a = pvar, pbounds
! b = pbody [pelse]
	unit pbounds, pelse, plimit, pautovar
	symbol dvar, limitvar
	int lab_b, lab_c, lab_d, lab_e, opc

	pbounds:=pvar.nextunit

	pautovar:=createavnamex(stcurrproc)

	pelse:=pbody.nextunit
	dvar:=pvar.def

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(lab_b, lab_c, lab_d)

	evalunit(pbounds)				!stack has lwb, upb
	limitvar:=pautovar.def
	genpc_name(kzpopv+limitvar.isframe, limitvar)

	genpc_int(kpushci, 1)
	genpc(ksub)
	genpc_name(kpopv, dvar)		!from value

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:
	definefwdlabel(lab_b)

	evalunit(pbody, 0)				!do loop body

	definefwdlabel(lab_c)

	if dvar.isframe=limitvar.isframe then
		genpc_lab(kforvv, lab_b)
	else
		gerror("forx:mixed m/f")
	fi
	genpc_name(kpushv, dvar)
	genpc_name(kpushv, limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse, 0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_print(unit p, a, b)=
	int issprint
	unit x

	issprint:=p.flag iand pr_sprint

!global const pr_newline = 1
!global const pr_format = 2
!global const pr_sprint = 4


	if issprint then
CPL "//////ISSPRINT"
		callhostfn(h_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(h_startprint)
		else
			callhostfn(h_startprintcon)
		fi
	fi

	x:=b

	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(h_print)
		when jnogap then
			callhostfn(h_printnogap)
		when jspace then
			callhostfn(h_printspace)
		else
			evalunit(x)
			callhostfn(h_print_nf)
		esac
		x:=x.nextunit
	od

	if p.flag iand pr_newline then
		callhostfn(h_println)
	fi
	if issprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi
end

proc do_fprint(unit p, a, b, c)=
	int issfprint
	unit x

	issfprint:=p.flag iand pr_sprint

	if issfprint then
		callhostfn(h_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(h_startprint)
		else
			callhostfn(h_startprintcon)
		fi
	fi

	evalunit(b)					!format string
	callhostfn(h_setformat)

	x:=c
	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(h_print)
		when jnogap then
			callhostfn(h_printnogap)
		else
			genpc(kpushvoid)
			evalunit(x)
			callhostfn(h_print)
		esac
		x:=x.nextunit
	od

	if p.flag iand pr_newline then
		callhostfn(h_println)
	fi
	if issfprint then
		genpc(kpushvoid)
		callhostfn(h_strendprint)
	else
		callhostfn(h_endprint)
	fi

end

proc do_read(unit p, a, b)=
unit x, xloop

if p.flag iand pr_newline then
	if a then
		evalunit(a)
		callhostfn(h_readln)
	else
		genpc(kpushvoid)
		callhostfn(h_readln)
	fi
fi

xloop:=b
while xloop do
	x:=xloop
	genpc(kpushvoid)
	if x.tag=jfmtitem then
		evalunit(x.b)
		callhostfn(h_sread)
		x:=x.a
	else
		genpc(kpushvoid)
		callhostfn(h_sread)
	fi
	if x.tag=jname then
		genpc_name(kpopv, x.def)
	else
		evalref(x)
		genpc(kpopptr)
	fi
	xloop:=xloop.nextunit
od
end

proc do_forall(unit p, pindex, pbody)=
!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, plist, pvar
! b = pbody, [pelse]

	int lab_b, lab_c, lab_d, lab_e
	unit ploopvar, plist, pelse, plimitvar, plistvar
	symbol indexvar, limitvar, loopvar, listvar

	plist:=pindex.nextunit
	ploopvar:=plist.nextunit

	if ploopvar=nil then			!no discrete index var
		ploopvar:=pindex

		pindex:=createavnamex(stcurrproc)

	fi
	loopvar:=ploopvar.def

	plimitvar:=createavnamex(stcurrproc)

	limitvar:=plimitvar.def
	indexvar:=pindex.def

	if plist.tag<>jname or plist.def.isframe<>loopvar.isframe then			!complex list

		plistvar:=createavnamex(stcurrproc)

		listvar:=plistvar.def
		evalunit(plist)
		genpc_name(kzpopv, listvar)
	else
		plistvar:=plist
		listvar:=plistvar.def
	fi

	unless indexvar.isframe=loopvar.isframe=listvar.isframe then
		gerror("forall: mixed vars")
	end

	pelse:=pbody.nextunit

!set up initial loop var
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(lab_b, lab_c, lab_d)

!assume plist is a var where bounds are not known
!(can be optimised for a const range or a const list)
	genpc_name(kpushv+listvar.isframe, listvar)			!load the list
	genpc_n(kbounds, 2)				!extract bounds as (lower, upper); upper is tos

	genpc_name(kzpopv, limitvar)		!limit:=upb
	genpc_int(kpushci, 1)
	genpc(ksub)
	genpc_name(kzpopv, indexvar)		!index:=lwb-1 (will incr first thing)

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C:

	definefwdlabel(lab_b)

!start of iteration, set up next loop variable
	genpc_name(kpushv+listvar.isframe, listvar)
	evalunit(pindex)

	genpc((p.tag=jforall|kindex|kdotix))
	genpc_name(kpopv, loopvar)

	evalunit(pbody, 0)			!do loop body

	definefwdlabel(lab_c)

	if indexvar.isframe=limitvar.isframe then
		genpc_lab(kforvv, lab_b)
	else
		gerror("forall:mixed m/f")
	fi
	genopnd_name(indexvar)
	genopnd_name(limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse, 0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_case(unit p, pindex, pwhenthen, int res) =
!also temporarily deal wit switch/doswitch

	int lab_a, lab_d
	int loopsw, labnextwhen, labstmtstart, fmult
	unit w, wt, pelse

	if pindex.tag=jnone then
		do_case_nc(p, pindex, pwhenthen, res)
		return
	fi

	loopsw:=p.tag=jdocase or p.tag=jdoswitch
	pelse:=pindex.nextunit

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	evalunit(pindex)			!load test expr p to t

	wt:=pwhenthen
	while wt do
		w:=wt.a
		fmult:=w.nextunit<>nil

		labnextwhen:=createfwdlabel()

		if fmult then
			labstmtstart:=createfwdlabel()
		fi

		while w do
			evalunit(w)
			w:=w.nextunit
			if w then					!not last
				genpc_lab(kjumpwhen, labstmtstart)
				pccurr.boolcode:=1
			else
				genpc_lab(kjumpwhen, labnextwhen)
				pccurr.boolcode:=0
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b, res)

		if not loopsw then
			genjumpl(lab_d)
		else
			genjumpl(lab_a)
		fi
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	genpc_n(kunshare, 1)

	if pelse then
		evalunit(pelse, res)
	elsif res then
		genpc(kpushvoid)
	fi
	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
		definefwdlabel(lab_d)
	fi
end

proc do_case_nc(unit p, pindex, pwhenthen, int res) =
!when no control expression

	int lab_a, lab_d
	int labnextwhen, labstmtstart, fmult
	unit w, wt, pelse

	if p.tag<>jcase then gerror("case-nc") fi

	pelse:=pindex.nextunit

	lab_d:=createfwdlabel()

	wt:=pwhenthen
	while wt do
		w:=wt.a
		fmult:=w.nextunit<>nil

		labnextwhen:=createfwdlabel()

		if fmult then
			labstmtstart:=createfwdlabel()
		fi

		while w do
			evalunit(w)
			w:=w.nextunit
			if w then					!not last
				genpc_lab(kjumpc, labstmtstart)
				pccurr.boolcode:=1
			else
				genpc_lab(kjumpc, labnextwhen)
				pccurr.boolcode:=1
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b, res)

		genjumpl(lab_d)
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	if pelse then
		evalunit(pelse, res)
	elsif res then
		gerror("Needs Else branch")
!		genpc(kpushvoid)
	fi

	definefwdlabel(lab_d)
end

proc do_try(unit p, a, b) =
	int labend, labx
	unit ptry, x, pexcept, pexcode

	++trylevel
	labend:=createfwdlabel()
	ptry:=a
	labx:=createfwdlabel()

	pexcept:=b

	if pexcept=nil then
		gerror("try: no except")
	elsif pexcept.nextunit then
		gerror("Try:multiple except block not implemented")
	fi

	while pexcept do
		pexcode:=pexcept.a
		if pexcode=nil or pexcode.nextunit then
			gerror("Try:multiple except codes not implemented")
		fi
		genpc_lab(kpushtry, labx)
		genopnd_int(getconstvalue(pexcode))
		genopnd_int(1)
		evalunit(ptry, 0)
		genjumpl(labend)
		definefwdlabel(labx)
		evalunit(pexcept.b, 0)
		definefwdlabel(labend)
		pexcept:=pexcept.nextunit
	od

	genpc_n(kaddsp, 1)
	--trylevel
end

function unitstoarray(unit p, ref[]unit plist, int maxunits)int=
!convert a linked list of units to a linear list
!return number of units
	int n

	n:=0
	while p do
		if n>=maxunits then
			gerror("UTA Too many units")
		fi
		plist^[++n]:=p
		p:=p.nextunit
	od
	
	return n
end

proc do_select(unit pindex, pplist, int res)=
!generate selectx expression
	int n, labend, i, lab, elselab
	unit x, pelse

	[maxswitchrange]unit plist
	[maxswitchrange+1]int labels

	pelse:=pindex.nextunit

	n:=unitstoarray(pplist, &plist, maxswitchrange)

	if n>maxswitchrange then
		gerror("Selectx too complex")
	fi

	labend:=createfwdlabel()

	evalunit(pindex)
!	genpc_int2(kselect, n, 1)
	genpc_xy(kswitch, 1, n)

	for i:=1 to n do
		labels[i]:=pccurr-pcstart		!store destination code index
		genpc_lab(kjumplab, 0)
	od
	labels[n+1]:=pccurr-pcstart
	genpc_lab(kjumplab, 0)

!scan when statements again, o/p statements
	i:=1
	for i:=1 to n do
		x:=plist[i]
		lab:=definelabel()

GERROR("SELECT")
!		(pcstart+labels[i]+1)^:=lab
		evalunit(x, res)

		genjumpl(labend)	!break to end of statement
	od

	elselab:=definelabel()


GERROR("SELECT2")
!	(pcstart+labels[n+1]+1)^:=elselab

	if pelse then
		evalunit(pelse, res)
	elsif res then
		genpc(kpushvoid)
	fi

	genpc(knop)

	definefwdlabel(labend)
end

proc do_andl(unit x, y)=
	int a, b

	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpf, x, a)
	genjumpcond(kjumpf, y, a)

	genpc_int(kpushci, 1)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci, 0)
	genpc(knop)
	definefwdlabel(b)
end

proc do_orl(unit x, y)=
	int a, b
	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpt, x, a)
	genjumpcond(kjumpt, y, a)
	genpc_int(kpushci, 0)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci, 1)
	genpc(knop)
	definefwdlabel(b)
end

proc do_incr(unit p, a, int res)=
	symbol d
	int opc
	opc:=(p.tag=jincrload|kincrload|kloadincr)

	if res then
		do_unaryref(a, opc)
	elsif a.tag=jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			do_unaryref(a, kincrload)
		else
			genpc_name(kincrtov, a.def)
		fi
	else
		do_unaryref(a, kincrload)
	fi
end

proc do_callhost(unit p, a, int res)=
	int index:=p.index
	int isfunc:=hostisfn[index]
	int nargs, nparams, fparams
	[10]unit plist
	unit q


	if res and not isfunc then
		gerror("Host proc not a function")
	fi

	if isfunc then
		genpc(kpushvoid)
	fi

	nargs:=0
	q:=a

	while q do
		if nargs>plist.upb then
			gerror("Too many host args")
		fi
		plist[++nargs]:=q

		q:=q.nextunit
	od

!	if index=h_allparams and a=nil then
!		nparams:=1
!	else
		nparams:=nargs
!	fi

	if nparams=0 and hostlvset[index] then
		gerror("LV hostfn: needs 1+ params")
	fi
	fparams:=hostnparams[index]
	if nparams>fparams then
		gerror("Hostfn too many params")
	fi

	to fparams-nparams do
		genpc(kpushvoid)
	od

!Finally, push all the params, which need to be done in reverse order
	for i:=nparams downto 1 do
		if i=1 and hostlvset[index] then
			evalref(plist[i])
!		elsif i=1 and index=h_allparams and nargs=0 then
!			genpc_name(kpushvref, stcurrproc)
		else
			evalunit(plist[i])
		fi
	od  

	callhostfn(index, res)
end

proc callhostfn(int fnindex, calledasfn=0)=
!assume caller has verified that fn is a function when calledasfn is true
!called should have pushed retval as needed, and <aparams> params

	genpc_int(kcallhost, fnindex)
end

proc genfree(int n)=
	genpc_n(kunshare, n)
end

proc do_return(unit p, a)=
!CPL "RETURN", NAMENAMES[STCURRPROC.NAMEID], STRMODE(STCURRPROC.MODE), STCURRPROC.MISFUNC
	if a then
		if not stcurrproc.misfunc then gerror("Proc can't return a value") fi
		evalunit(a)
	else
		if stcurrproc.misfunc then gerror("Func needs return value") fi

!	elsif currfunction=2 then
!		gerror("function needs return value")
	fi

	genjumpl(retindex)
end

proc do_multassign(unit a, b, int deepcopy, res)=
	unit p, q
	[100]unit plist
	int n

	p:=a.a
	q:=b.a
	n:=0

	while p do
		if q=nil then gerror("Too few RHS elems") fi
		evalunit(q)
		if n>=plist.len then gerror("Too many elems") fi
		plist[++n]:=p

		p:=p.nextunit
		q:=q.nextunit
	od

	if q then gerror("Too few LHS elems") fi

	for i:=n downto 1 do
		if deepcopy then
			genpc(kcopy)
		fi

		do_store(plist[i])
	od
end

proc do_store(unit a, int res=0)=
!store stack value to a
	symbol d
	unit p
	[100]unit plist
	int n

	if res and a.tag<>jname then
		genpc(kdupl)
	fi

	case a.tag
	when jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			if res then genpc(kdupl) fi
			genpc_name(kpushv, d)
			genpc(kpopptr)
		elsif res then
			genpc(kdupl)
			genpc_name(kpopv, d)
!		elsif d.nameid=dllvarid then
!			genpc_name(kpopx, d)

		else
			genpc_name(kpopv, d)
		fi

	when jdot then
		evalunit(a.a)
		genpc_name(kpopdot, a.b.def)

	when jindex then
		do_bin(a.a, a.b, kpopix)

	when jdotindex then

		evalref(a.a)
		evalunit(a.b)
		genpc(kpopdotix)
	when jptr then
		evalunit(a.a)
		genpc(kpopptr)

	when jkeyindex then
		do_bin(a.a, a.b, kpopkeyix)

	when jmakelist then			!assign to multiple destinations
		n:=0
		p:=a.a
		while p do
			if n>=plist.len then gerror("Too many elems") fi
			plist[++n]:=p
			p:=p.nextunit
		od
		if n=0 then gerror("Empty lhs list") fi

		genpc_n(kexpand, n)
!		for i:=n downto 1 do
		for i:=1 to n do
			do_store(plist[i])
		od

	when jif then
		evalref(a)
		genpc(kpopptr)

	else
		gerror_s("Can't store to this unit yet:", jtagnames[a.tag], a)
	esac
end

function getconstvalue(unit p)int =
	if p and p.tag=jintconst then
		return p.value
	fi
	gerror("gcv Not const")
	return 0
end

proc do_convert(unit pconv)=
!apply type-conversion t on expression p

!also do constructors
	int n, elemmode, i, lowerx, lbound, m, mbase, nfields
	[maxunits]unit plist
	unit p

	m:=pconv.mode
	p:=pconv.a
	mbase:=ttbasetype[m]

!p.length is no. of elements, but it not used here(unitstoarray will count
!anyway). But a value of -1 (rather than 1) means a trailing comma was used.

	if p.tag<>jmakelist  OR MBASE=TREFPACK then		!assume regular type conversion
			if p.tag=jmakelist then
				deleteunit(p, p.a)
			fi
			evalunit(p)
			genpc_int(kconvert, m)
			return
!		fi
	fi

!a is a usertype
	n:=unitstoarray(p.a, &plist, maxunits)

	if n and plist[1].tag=jkeyvalue then
		case mbase
		when trecord, tstruct then
			do_makerecordkv(m, n, plist)
		else
			gerror("key:value not allowed")
		esac
		return
	fi

	for i:=1 to n do		!any elements need to be pushed
		evalunit(plist[i])
	od

	case mbase
	when trecord, tstruct then
		nfields:=ttlength[m]
		if n then
			checkelems(n, nfields, p)
		else				!allow 0 fields; use defaults of 0
			to nfields do
				genpc_int(kpushci, 0)
			od
			n:=nfields
		fi
		genpc_xy((mbase=trecord|kmakevrec|kmaketrec), n)
		pccurr.usertag:=m

	when tlist then		!probably just a list prefix used
		lowerx:=p.lower
		genpc_xy(kmakelist, n, lowerx)

	when tarray then
		genpc_xy(kmakeax, n, p.lower)
		pccurr.usertag:=tarray
		pccurr.usertag2:=p.elemtype

!	when tvector then
	when tvector then
		elemmode:=tttarget[m]
		lowerx:=ttlower[m]

		checkelems(n, ttlength[m], p)
		genpc_xy(kmakeax, lowerx, n)
		pccurr.usertag:=m
		pccurr.usertag2:=elemmode

	when tbits then
		if m=tbits then			!not user-defined
			genpc_xy(kmakebits, n, p.lower)
			pccurr.usertag:=tbits
			pccurr.usertag2:=(p.elemtype=tvoid|tu1|p.elemtype)
		else
			gerror("user-define bit array not ready")
		fi

	when tset then
		genpc_xy(kmakeset, n)

	else
		gerror_s("Convert list", strmode(mbase))
	esac
end

!proc do_case(unit p, pindex, pwhenthen, int res) =
proc checkelems(int n, length, unit p)=
	if n<length then
		gerror("Too few elements")
	elsif n>length then
		gerror("Too many elements")
	fi
end

proc do_switch(unit p, pindex, pwhenthen, int res) =
	int minlab, maxlab, x, y, i, n
	unit w, wt, pelse

	pelse:=pindex.nextunit
!first a first scan over the when expressions; work out range and whether simple or complex
	minlab:=1000000
	maxlab:=-1000000			!highest index seen

	n:=0				!no. different values
	wt:=pwhenthen

	while wt do
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				x:=getconstvalue(w.a)
				y:=getconstvalue(w.b)
dorange:
				for i:=x to y do
					minlab :=min(minlab, i)
					maxlab :=max(maxlab, i)
				od
			when jintconst then
				x:=y:=w.value
				goto dorange
			when jtypeconst then
				x:=y:=w.mode
				goto dorange
			else
				gerror_s("Switch when2: not const", strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	if maxlab-minlab<=maxswitchrange then
		do_simpleswitch(p, pindex, pwhenthen, pelse, minlab, maxlab, res)
		return
	fi

	gerror("COMPLEX SWITCH/NOT COMPLETE")
end

proc do_simpleswitch(unit p, pindex, pwhenthen, pelse, int a, b, res) =
!a..b is the range of values of the switch which have been checked to
!be in range in terms of span. But the actual values can be anything.
!For example, 1000000 to 10000250 is valid. So, an offset needs to be
!used to bring the range down to 0 to 250

	unit w, wt, q
	int loopsw, n, offset, x, y, x0, i, labstmt, elselab
	[1..maxswitchrange+1]int labels
	int lab_a, lab_b, lab_c, lab_d

	loopsw:=p.tag=jdoswitch

	n:=b-a+1
	offset:=a-1		!a..b becomes 1..n

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(lab_a, lab_a, lab_d)
	else
		lab_d:=createfwdlabel()
	fi
	elselab:=createfwdlabel()

	evalunit(pindex)

	genpc_xy(kswitch, a, b)

	for i:=1 to n do
		genpc_lab(kjumplab, 0)
GERROR("SWITCH")
!		labels[i]:=pccurr+1		!for now, store destination code index
	od

	genpc_lab(kjumplab, 0)			!else label
GERROR("SWITCH2")
!	labels[n+1]:=pccurr+1

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				x0:=getconstvalue(w.a)
				y:=getconstvalue(w.b)

			when jintconst then
				x0:=y:=w.value
			when jtypeconst then
				x0:=y:=w.mode
			esac

			for x:=x0 to y do
				i:=x-offset
GERROR("SWITCH3")
!				if labels[i]^ then			!should have been zero
!					cpl x, char(x)
!					gerror("Dupl switch value")
!				fi
!				labels[i]:=labstmt
			od
			w:=w.nextunit
		od

		evalunit(wt.b, res)

		if not loopsw then
			genjumpl(lab_d)
		else
			genjumpl(lab_a)
		fi
		wt:=wt.nextunit
	od

!fill in zero entries with else
	definefwdlabel(elselab)
	if pelse then		!do else part
		evalunit(pelse, res)
	fi	

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
		definefwdlabel(lab_d)
	fi

	for i:=1 to n do
GERROR("SWITCH4")
!		if labels[i]^=0 then
!			labels[i]^:=elselab
!		fi
	od
GERROR("SWITCH5")
!	labels[n+1]^:=elselab
end

proc do_makerecordkv(int m, nkeyvals, []unit &kvlist)=
	unit p
	[maxunits]unit plist
	int nfields, index
	symbol d:=ttnamedef[m], e, f, k

	e:=d.deflist
	nfields:=0

	while e, e:=e.nextdef do
		if e.nameid in [fieldid, structfieldid] and e.atfield=nil then
			++nfields
			plist[nfields]:=nil
		fi
	od

	for i to nkeyvals do
		k:=kvlist[i].a.def
		p:=kvlist[i].b

		e:=d.deflist
		f:=nil
		while e, e:=e.nextdef do
			if e.nameid in [fieldid, structfieldid] and e.firstdupl=k then
				f:=e
				exit
			fi
		od

		if not f then
			gerror_s("Can't find field:", k.name)
		fi
		index:=f.index
		if plist[index] then
			gerror_s("Dupl key:", k.name)
		fi
		plist[index]:=p
	od

	for i to nfields do
		if plist[i] then
			evalunit(plist[i])
		else
			genpc_int(kpushci, 0)
		fi
	od

	genpc_xy(kmakevrec, nfields)
	pccurr.usertag:=m
end

proc do_idiv(unit a, b)=
	int n

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		genpc_int(kpushci, n)
		genpc(kshr)
	else
		evalunit(b)
		genpc(kidiv)
	fi
end

proc do_irem(unit a, b)=
	int n
	word m

	evalunit(a)
	if b.tag=jintconst and (n:=ispoweroftwo(b.value)) then
		m:=inot(0xFFFF'FFFF'FFFF'FFFF << n)
		genpc_int(kpushci, M)
		genpc(kiand)
	else
		evalunit(b)
		genpc(kirem)
	fi
end

proc do_map(unit p, popcode, x)=
	evalunit(x)
	if x.nextunit then
		evalunit(x.nextunit)
	fi
	evalunit(popcode)
	genpc(kmap)

	int lab:=createfwdlabel()
	genpc_lab(kjump, lab)		!dummy jump to be moved to runtime-generated code
	genpc(knop)					!stop jump being optimised out
	definefwdlabel(lab)
end

proc pushstring(ichar s, int length)=
	genpc(kpushcs)
	ref stringrec ps:=pcm_alloc(stringrec.bytes)
	ps.svalue:=s
	ps.length:=length
	genopnd_int(cast(ps))
end

function checkblockreturn(unit p)int=
!p should be a block unit
!check that the last statement is a return; return 1/0 for return/not return
!just allow or check for return/if/longif for now
	ref unitrec q, r

	if p=nil then return 0 fi
!	if p.tag<>jblock then gerror("CBR?") fi
!
!	q:=p.a
!	if q=nil then return 0 fi		!empty block

!	while r:=q.nextunit do			!get q=last stmt in block
!		q:=r
!	od

	case jhasvalue[p.tag]
	when 0 then return 0
	when 1 then return 1				!assume simple value
	esac								!else 2

!assume complex unit

	case p.tag
	when jblock then
		q:=p.a
		if q=nil then return 0 fi		!empty block
		while r:=q.nextunit do			!get q=last stmt in block
			q:=r
		od
		return checkblockreturn(q)

!	when jreturn then			!that's an easy one...
!		return 1

	when jif then
		return checkblockreturn(p.b) and checkblockreturn(p.b.nextunit)		!all branches must have a return

	else								!assume yes
		return 1


	esac
	return 0
end

=== qq_pcllib.m 0 0 11/16 ===
const pclinitalloc=128

global pcl pcstart				!point to start of current pcl block
global pcl pccurr				!point to last create pcl rec
global pcl pcend				!point to last allocated int (with enough margin for on extra instr)
global int pcalloc				!ints allocated

global ref int32 pcsourcestart
global ref int32 pcsourcecurr

global int pclcurrlineno			!current line number
const pclelemsize=pclrec.bytes
const pcsrcelemsize=int32.bytes

global const labelinitalloc=8192
global ref[]pcl labelpctable		!labelpctable[L] refers to target instr of label L
global int labelalloc
global int nextlabelno
byte labelflag						!1 means next pcl op is labeled

!global [0..pclnames.upb]byte pclnopnds

GLOBAL PROC TESTPCL=

CPL $LINENO

	resetpcl(0)
CPL $LINENO

	genpc(knop)
	genpc(knop)
	genpc_int(kpushci, 12345)
	genpc(kadd)
	genpc(kmul)
	genpc(kendprog)

	filerec pm

	pm.name:="DUMMY"
	pm.pcstart:=pcstart
	pm.pcend:=pcend
	pm.pcsize:=pcend-pcstart
	pm.pcsourcestart:=pcsourcestart

	gs_init(pcldest)
	gs_strln(pcldest, "HELLO")

	writeallpcl(&pm, 1)

	gs_println(pcldest, nil)

end

proc start=
	int nn

	pcm_init()

!label/block tables are not needed after the pcl sequence has been
!generated. But they are not freed; they can be reused, with their
!current sizes, for the next module. (Might be inefficient if there is one
!very large module, then mainly small ones.)

	labelalloc:=labelinitalloc
	labelpctable:=pcm_alloc(int.bytes*labelalloc)
end

global proc resetpcl(int sourcesize)=
	int pclsize

	qpos:=0
	nextlabelno:=0
	pclcurrlineno:=0

!pcl dest is reallocated for each module
!Any current pcl data is presumably retained so that it can be run.

	pclsize:=sourcesize			!estimated num of pcl bytecode elements

	pcalloc:=1024					!min
	while pcalloc<pclsize do
		pcalloc<<:=1
	od

	pcstart:=pcm_allocz(pcalloc*pclelemsize)
	pccurr:=pcstart-1
	pcend:=pcstart+pcalloc-8			!allow margin

	pcsourcestart:=pcm_alloc(pcalloc*pcsrcelemsize)
	pcsourcecurr:=pcsourcestart

	pcm_clearmem(labelpctable, int.bytes*labelalloc)

end

global proc genpc(int opc)=

	++pccurr

	if pccurr>=pcend then
		extendpcldata()
	fi

!only do overflow check at start of an instruction
	pccurr.opcode:=opc
	pccurr.haslabel:=labelflag
	labelflag:=0

	++pcsourcecurr
	pcsourcecurr^:=qpos

end

global proc genopnd_int(int64 x)=
!no pcindex overflow check needed, as the genpc() check will be sufficient as
!it would allow for enough operands
	pccurr.value:=x
end

global proc genopnd_name(ref strec d)=
	pccurr.def:=d
end

global proc genpc_int(int opc, int64 a)=
	genpc(opc)
	pccurr.value:=a
end

global proc genpc_n(int opc, n)=
	genpc(opc)
	pccurr.n:=n
end

global proc genpc_xy(int opc, x, y=0)=
	genpc(opc)
	pccurr.x:=x
	pccurr.y:=y
end

global proc genpc_name(int opc, ref strec d)=
	genpc(opc)
	pccurr.def:=d
end

global proc genopnd_strz(ichar s)=
!s must be a heap string, be a constant, or otherwise be persistent
	pccurr.svalue:=s
end

global proc genopnd_str(object s)=
!s must be a heap string, be a constant, or otherwise be persistent
	pccurr.objptr:=s
end

global proc genopnd_obj(object p)=
	pccurr.objptr:=p
end

global proc genpc_real(int opc, real x)=
	genpc(opc)
	pccurr.xvalue:=x
end

global proc genpc_lab(int opc, int lab)=
	genpc(opc)
	pccurr.labelno:=lab
end

global proc genopnd_lab(int a)=
	pccurr.labelno:=a
end

global proc gencomment(ichar s)=
	genpc(kcomment)
	genopnd_strz(pcm_copyheapstring(s))
end

proc extendpcldata=
	int newpcalloc
	pcl newpcstart
	ref int32 newpcsourcestart

	newpcalloc:=pcalloc*2

!CPL "EXTENDING PCL TABLE TO",=PCLSTART

	newpcstart:=pcm_alloc(pclelemsize*newpcalloc)
	newpcsourcestart:=pcm_alloc(pcsrcelemsize*newpcalloc)

	memcpy(newpcstart,pcstart, (pccurr-pcstart)*pclelemsize)
	memcpy(newpcsourcestart,pcsourcestart, (pccurr-pcstart)*pcsrcelemsize)

	pccurr:=newpcstart+(pccurr-pcstart)
	pcend:=newpcstart+newpcalloc-10
	pcsourcecurr:=newpcsourcestart+(pcsourcecurr-pcsourcestart)

	pcm_free(pcstart,pcalloc*pclelemsize)
	pcm_free(pcsourcestart,pcalloc*pcsrcelemsize)

	pcstart:=newpcstart
	pcalloc:=newpcalloc
	pcsourcestart:=newpcsourcestart
end

global proc extendlabeltable=
	int newlabelalloc
	ref[]pcl newlabeltable

	newlabelalloc:=labelalloc*2

	newlabeltable:=pcm_alloc(pcl.bytes*newlabelalloc)

	memcpy(newlabeltable,labelpctable, labelalloc*pcl.bytes)

	pcm_free(labelpctable,labelalloc*pcl.bytes)

	labelpctable:=newlabeltable
	labelalloc:=newlabelalloc
end

global function definelabel:int=
	if nextlabelno>=labelalloc then extendlabeltable() fi
	++nextlabelno
	labelpctable[nextlabelno]:=pccurr+1
	labelflag:=1
	return nextlabelno
end

global function createfwdlabel:int=
	if nextlabelno>=labelalloc then extendlabeltable() fi
	++nextlabelno
	labelpctable[nextlabelno]:=nil
	labelflag:=1
	return nextlabelno
end

global proc definefwdlabel(int lab)=
	if labelpctable[lab] then serror("dupl label?") fi

	labelpctable[lab]:=pccurr+1
	labelflag:=1
end

global proc genxy(int x, y=0)=
	pccurr.x:=x
	pccurr.y:=y
end

!GLOBAL PROC SHOWLABS(ICHAR CAPTION)=
!	PRINT "    ",CAPTION,,": ("
!	FOR I TO NEXTLABELNO DO
!		CP LABELPCTABLE[I],$
!	OD
!	CPL ")",NEXTLABELNO
!END
!
=== qq_resolve.m 0 0 12/16 ===
int nprocs

int noexpand
int symbolmode
int macrolevels
int allowmodname

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

const maxstructfields=100
[maxstructfields]symbol structfields
int ntopfields, nallfields

global proc rx_module(ifile pm)=
	currmodule:=pm
	stcurrproc:=stcurrmodule:=currmodule.def
	nprocs:=0

!move this to end of proc to allow module vars generated by assignment
!to be visible inside procs

	rx_passdef(stprogram, stcurrmodule)

	if nprocs=0 then
		rx_unit(stcurrmodule,currmodule.ast)
!	elsif currmodule.ast.a then				!module block not empty
	elsif currmodule.ast then
		RX_UNIT(STCURRMODULE,CURRMODULE.AST)
	fi
end

global proc rx_passdef(symbol owner,p)=
	symbol d

	case p.nameid
!	when moduleid,dllmoduleid then
	when moduleid then
		rx_deflist(p,p.deflist)

	when procid, anonprocid then
		++nprocs
		fixmode(owner,p)
		rx_deflist(p,p.deflist, 0)
		stcurrproc:=p
		rx_unit(p,p.code)
		stcurrproc:=stcurrmodule
		rx_deflist(p,p.deflist, 1)

	when dllprocid then
		fixmode(owner,p)
		rx_deflist(p,p.deflist)

	when constid,staticid,frameid,paramid then
		fixmode(owner,p)
		if p.code then
			rx_unit(owner,p.code)
		fi
	when typeid,recordid then
		fixmode(owner,p)
		rx_deflist(p,p.deflist)

	esac
end

global proc rx_deflist(symbol owner, p, int doanon=0)=
!doanon=0: do all names except anonproc
!doanon=1: do only anonproc
!

	while p do
		if doanon and p.nameid=anonprocid or doanon=0 and p.nameid<>anonprocid then
			rx_passdef(owner,p)
		fi
		p:=p.nextdef
	od
end

global proc rx_unit(symbol owner, unit p)=
	symbol d
	unit a,b
	int n, flags, oldnoexpand,oldsymbolmode, nk

	a:=p.a
	b:=p.b
	qpos:=p.pos

	case p.tag
	when jname then
		resolvename(owner,p)
		if p.tag=jname and p.def.nameid=macroid and not noexpand then
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
		if a.tag=jname then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=jtypeconst then
			p.tag:=jconvert
			p.a:=b
			p.b:=nil
			p.mode:=a.mode

CPL "CALL TO CONVERT"

!			if ttbasetype[a.mode]=tenum then
!			else
				nk:=0
				p.a:=createunit1(jmakelist,b)
				n:=0
				while b do
					if b.tag=jkeyword then
						++nk
						b.tag:=jkeyvalue
					fi
					++n
					b:=b.nextunit
				od
				if nk and nk<>n then
					rxerror("Mixed key:value")
				fi
				if a.nextunit then n:=-n fi
				p.a.length:=n
!			fi
		elsif a.tag=jname and a.def.nameid=macroid then
			++macrolevels
			expandmacro(p,a,b)
			rx_unit(owner,p)
			--macrolevels
		fi

	when jbin, jmakerange then
		rx_unit(owner,a)
		if not b then rxerror("Binop missing opnd") fi
		rx_unit(owner,b)
		evalbinop(p,a,b)

	when junary, jproperty then
		rx_unit(owner,a)
		evalmonop(p)

	when jfor then			!a will be jname unit
		resolvename(owner,a,tint)
		a:=a.nextunit
		goto doabc

	when jconvert then
		rx_unit(owner,a)

		evalmonop(p)

	when jsymbol then
		oldnoexpand:=noexpand
		oldsymbolmode:=symbolmode
		noexpand:=1
		symbolmode:=1

		rx_unit(owner,a)
		noexpand:=oldnoexpand
		symbolmode:=oldsymbolmode

		case a.tag
		when jname then
		when jtypeconst then
			d:=ttnamedef[a.mode]

			if d then
				a.def:=d
				a.tag:=jname
			else
				rxerror("T.$?")
			fi

		else
printunit(a)
			rxerror(".$ not name")
		esac

	when jstrinclude then
!PCERROR("STRINCLUDE")
		rx_unit(owner,a)
		if a.tag<>jstringconst then
			rxerror("Not strconst")
		fi

		ifile pm
		pm:=loadsourcefile(a.svalue,0)

!		n:=getsupportfile(a.svalue,
!			path:sourcefilepaths[modules[p.moduleno].fileno],
!			issupport:1)
		a.svalue:=pm.text
		a.slength:=pm.size-1

		deleteunit(p,a)

	else
doabc:

		flags:=jflags[p.tag]
		if flags>=1 then rx_unitlist(owner,a) fi
		if flags=2 then rx_unitlist(owner,b) fi
	esac
end

proc rx_unitlist(symbol owner, unit p)=
	while p do
		rx_unit(owner,p)
		p:=p.nextunit
	od
end

proc evalmonop(unit p)=
	int a,c
	real x,z

	case p.tag
!	when jbytesize then
!		if p.a.tag=jtypeconst then
!			c:=ttsize[p.a.mode]
!			newint
!		fi

	elsecase p.a.tag
	when jintconst then
		a:=p.a.value

		case p.pclop
		when kneg then c:=-a
		when kabs then c:=abs(a)
		else
			return
		esac

newint:
		makeintconst(p,c)

	when jrealconst then
		x:=p.a.xvalue

		case p.pclop
		when kneg then z:=-x
		when kabs then z:=abs(x)
		else
			return
		esac

		makerealconst(p,z)
	else
		return 
	esac
end

proc evalbinop(unit p,lhs,rhs)=
	int a,b,c
	real x,y,z

CPL "EVALBIN", JTAGNAMES[P.TAG],pclnames[p.pclop]

	case pr(lhs.tag,rhs.tag)
	when pr(jintconst, jintconst) then
		a:=lhs.value
		b:=rhs.value

		case p.pclop
		when kadd then c:=a+b
		when ksub then c:=a-b
		when kmul then c:=a*b
		when kidiv then
			if b=0 then rxerror("x/0") fi
			c:=a/b
		when kpower then c:=a**b
		else
			return
		esac

		makeintconst(p,c)

	when pr(jrealconst, jrealconst) then
		x:=lhs.xvalue
		y:=rhs.xvalue

		case p.pclop
		when kadd then z:=x+y
		when ksub then z:=x-y
		when kmul then z:=x*y
		when kdiv then z:=x/y
		else
			return
		esac

		makerealconst(p,z)
	else
		return 
	esac
end

proc makeintconst(ref unitrec p,int64 value)=
!convert unit p, currently binop or monop, to a const
	p.tag:=jintconst
	p.a:=p.b:=nil
	p.value:=value
	p.mode:=tint
end

proc makerealconst(ref unitrec p,real64 xvalue)=
!convert unit p, currently binop or monop, to a const
	p.tag:=jrealconst
	p.a:=p.b:=nil
	p.xvalue:=xvalue
	p.mode:=treal
end

global proc resolvename(symbol owner, unit p, int mode=tvoid)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	symbol d,e,f
	unit q
	int moduleno, n

	d:=p.def
	moduleno:=p.moduleno

	if d.nameid<>genericid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)

	if not e then
		case owner.nameid
		when procid, anonprocid then			!add as framevar
			e:=p.def:=addsymbol(owner,d,frameid,0)
		when moduleid then
			e:=p.def:=addsymbol(owner,d,staticid,0)

		else
			rxerror_s("Undefined: #",d.name,p)
		esac
	else
$else:
retry:
!CPL "RESOLVED

		p.def:=e			!update link in kcode

		case e.nameid
		when constid then		!convert namedconst to const
IF SYMBOLMODE THEN
	RETURN
FI

			q:=e.code			!q is knamedconst unit; q.c is value
			rx_unit(owner,q)
			if q.tag not in [jintconst, jrealconst, jstringconst] then
				rxerror_s("Not const expr: #",jtagnames[q.tag])
			fi

			e.mode:=q.mode
			p.tag:=q.tag
			p.value:=q.value
			p.mode:=q.mode
			p.slength:=q.slength
		when enumid then
IF SYMBOLMODE THEN
	RETURN
FI
			p.tag:=jintconst
			p.value:=e.index
			p.mode:=tint

		when staticid then		!deal with python global accesses ?? WTF ???
		when typeid,recordid then
			p.tag:=jtypeconst
			p.mode:=p.def.mode

		when linkid then
			rxerror("FOUND LINK",p)
		when frameid, paramid then
			if stcurrproc.nameid=anonprocid and e.owner.nameid<>anonprocid then
				rxerror("Accessing transient vars from {}")
			fi

		esac
	fi
end

global function resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol=
!stnewname points to a symrec with nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)
!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match
!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int extcount,subprogno
	symbol p,q,powner,d,e,extdef,moddef
	[10]symbol ambiglist

	if owner.nameid=anonprocid then
		q:=owner.deflist
!CPL "SEARCHING FOR", STNEWNAME.NAME,"IN ANON:",OWNER.NAME
		while q, q:=q.nextdef do
!CPL "CHECKING",Q.NAME
			if q.firstdupl=stnewname then		!use that match
!CPL "FOUND", STNEWNAME.NAME,"IN ANON:",OWNER.NAME
				return q
			fi
		od
!CPL "NOT FOUND IN ANON:",STNEWNAME.NAME
		owner:=owner.owner
	fi

	if owner.nameid=procid then

!CP "------------SEARCHING PROC",OWNER.NAME,":"
!		q:=owner.deflist
!		while q, q:=q.nextdef do CP Q.NAME,$ OD
!		CPL

		q:=owner.deflist
		while q, q:=q.nextdef do
!CPL "CHECKING",Q.NAME
			if q.firstdupl=stnewname then		!use that match
!CPL "FOUND", STNEWNAME.NAME,"IN PROC:",OWNER.NAME
				return q
			fi
		od
	fi

	p:=stnewname.nextdupl
	subprogno:=modules[moduleno].subprogno

	extcount:=0
	extdef:=moddef:=nil

	while p, p:=p.nextdupl do						!p is next candidate
		powner:=p.owner

		case powner.nameid
		when moduleid then							!candidate is file-scope item
			if powner.moduleno=moduleno then		!same module
				return p
			elsif p.isglobal then	!matches an external module
!				if moduletosub[powner.moduleno]=subprogno or		!within same subprog
				if modules[powner.moduleno].subprogno=subprogno or		!within same subprog
					 p.isglobal=export_scope or
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
				if allowmod then
					moddef:=p
				fi
			when macroid then
				return p

			esac

		esac
	od

!if here, then no immediate match
!either of moddef/dlldef will be set
	if extdef then
		if extcount>1 then
			for i:=1 to extcount do
				extdef:=ambiglist[i]
				println i,extdef.owner.name,namenames[extdef.owner.nameid]
			od
			rxerror_s("Ambiguous ext name: #",extdef.name)
		fi
		return extdef
	fi

	return moddef				!will be nil when no match
end

proc resolvedot(symbol owner,unit p)=
	symbol qdef,rdef,d,newd,e,fielddef
	unit q,r
	int nfields,oldallowmod

!CPL "RD1"
	if symbolmode then
		resolvedot_sym(owner, p)
		return
	fi

!CPL "RD2"
	q:=p.a			!lhs
	r:=p.b			!rhs
	rdef:=r.def							!st entry for the field

	oldallowmod:=allowmodname
	allowmodname:=q.tag=jname
	rx_unit(owner,q)
	allowmodname:=oldallowmod

	case q.tag
	when jname then		!continue below

		d:=q.def
	when jtypeconst then	!was type
		d:=q.def
		goto dotype
	else					!assume expression
		rdef:=r.def
		goto doexprdot
	esac

!CPL =NAMENAMES[D.NAMEID]
	case d.nameid
!	when dllmoduleid,moduleid,typeid,procid,dllprocid then	!M./T./P./C. non-var lhs
	when moduleid,typeid,procid,dllprocid then	!M./T./P./C. non-var lhs
dotype:
		newd:=finddupl(d, rdef)
		if newd then					!found
			switch newd.nameid
			when enumid then			!convert whole thing to constant
				p.tag:=jintconst
				p.value:=newd.index
				p.mode:=tint
			when constid then
				q:=newd.code			!q is knamedconst unit; q.c is value
				case q.tag
				when jintconst then
					p.tag:=jintconst
					p.a:=p.b:=nil
					p.value:=q.value
					p.mode:=newd.mode

				else
					rxerror("Rxdot:const?",p)
				esac
			when typeid then
				p.tag:=jtypeconst
				p.mode:=newd.mode
				p.def:=newd
			when staticid then
				p.tag:=jname
				p.def:=newd

			when procid,dllprocid then
				p.tag:=jname
				p.a:=p.b:=nil
				p.def:=newd
			when macroid then
				if e.nameid=macroid and not noexpand then
					++macrolevels
					expandmacro(p,p,nil)
					rx_unit(owner,p)
					--macrolevels
				fi

			else
				cpl namenames[newd.nameid],,".",,newd.name
				rxerror("Rxdot:.name not allowed here",p)
			end switch

		else
			cpl d.name,,".",,rdef.name
			rxerror("Can't resolve",p)
		fi

	when frameid, staticid, paramid, fieldid, structfieldid then	!X. normal lhs
doexprdot:
		nfields:=0
		fielddef:=nil
		e:=rdef.nextdupl

!CPL "RD31"
		while e do
!CPL "RD32"
			case e.nameid
			when fieldid,structfieldid, constid, procid, typeid, staticid, dllprocid then
				++nfields
				fielddef:=e				!use this when unique
			esac
			e:=e.nextdupl
		od

!CPL "RD35",NFIELDS, RDEF
		case nfields
		when 0 then				!no field exists with this name
			cpl rdef.name
			rxerror("Can't find field")
		else					!dupl field
			if rdef.nameid<>genericid then
				rxerror("Field name not generic")
			fi
		esac

	else
!CPL "RD4"
		cpl namenames[d.nameid]
		rxerror("RXDOT:Unknown nameid",p)
	esac
!CPL "RDX"
end

proc resolvedot_sym(symbol owner,unit p)=
	symbol qdef,rdef,d,newd,e,fielddef
	unit q,r
	int nfields, oldallowmod

	q:=p.a			!lhs
	r:=p.b			!rhs
	rdef:=r.def							!st entry for the field

	oldallowmod:=allowmodname
	allowmodname:=q.tag=jname
	rx_unit(owner,q)
	allowmodname:=oldallowmod

	case q.tag
	when jname then		!continue below

		d:=q.def
	when jtypeconst then	!was type
		d:=q.def
		if symbolmode then
			newd:=finddupl(d, rdef)
			if newd=nil then
				rxerror_s("Can't resolve .",rdef.name)
			fi
			case newd.nameid
			when fieldid,structfieldid then
				CPL "*******FIELD.$"
			else
				rxerror_s(".$ ON type:",namenames[newd.nameid])
			esac

		fi
		goto dotype
	else					!assume expression
		rxerror("RXDOTSYM?")
	esac

	case d.nameid
!	when dllmoduleid,moduleid,typeid,procid, dllprocid then	!M./T./P./C. non-var lhs
	when moduleid,typeid,procid, dllprocid then	!M./T./P./C. non-var lhs
	dotype:
		newd:=finddupl(d, rdef)

		if newd then					!found
			p.tag:=jname
			p.a:=p.b:=nil
			p.def:=newd
		else
			rxerror_s(".$ Can't resolve",d.name)
		fi
!
	else
		rxerror_s("RX.$: Unknown nameid:",namenames[d.nameid],p)
	esac
end

global function finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

	if pdupl.nameid<>genericid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl

	while pdupl do
		if pdupl.owner=d then
!CPL "HERE/FD"
!			if pdupl.nameid in [aliasid,linkid] then
!				return d.equiv
!			fi

			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od

	return nil
end

proc expandmacro(unit p, a, b)=
!is is a macro name unit, b is a macro parameter list (rx-processed), which
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

	pm:=d.deflist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.firstdupl		!generic st entry
		pm:=pm.nextdef
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
	if jflags[q.tag] then
		q.a:=copylistunit(q.a)
		if jflags[q.tag]=2 then
			q.b:=copylistunit(q.b)
		fi
	fi
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

proc fixmode(ref strec owner, p)=
	ref strec d,e
	int m

	m:=p.mode

	if m>=0 then return fi
	m:=-m

	if ttxmap[m] then				!already fixed
		p.mode:=ttxmap[m]
		return
	fi

	d:=ttnamedefx[m]

	e:=resolvetopname(owner,d,ttxmoduleno[m],0)

	if e then
		ttxmap[m]:=e.mode
		p.mode:=e.mode

	else
		rxerror_s("Can't resolve type: #",d.name)
	fi
end

function fixmode2(ref strec owner, int m)int=
!if m is a userx type, fix it up and return fixed up mode
!otherwise just return m
	ref strec d,e
	[256]char str

	if m>=0 then return m fi
	m:=-m

	if ttxmap[m] then				!already fixed
		return ttxmap[m]
	fi

	d:=ttnamedefx[m]

	if owner=nil then rxerror("FM2/owner") fi

	e:=resolvetopname(owner,d,ttxmoduleno[m],0)

	if e then
		ttxmap[m]:=e.mode
		return e.mode
	else
		fprint @&.str,"# in module #, line:#",d.name,modules[ttxmoduleno[m]].name
		rxerror_s("2:Can't resolve type: #",&.str)
	fi
	return 0
end

global proc fixusertypes=
	ref userxrec p
	ref int pmode
	int m, rescan,i

!CPL "FIXUSERTYPES",NUSERXTYPES

	for i:=1 to 2 do
		p:=userxmodelist
		rescan:=0

		while p do
			m:=p.pmode^
			if m<0 then
				m:=fixmode2(p.owner,m)
				if m<0 and i=2 and ttxmap[abs m] then
					m:=ttxmap[abs m]
				fi
				if m<0 then
					rescan:=1
				else
					p.pmode^:=m

					if tttarget[m]=m then
						rxerror_s("recursive type?",ttname[m])
					fi
				fi
			fi

			p:=p.nextmode
		od
		if not rescan then exit fi

	od
	if rescan then
		rxerror("FUT Phase Error")
	fi

	for i to nbaseclasses do
		dobaseclass(i)
	od
end

global proc tx_typetable=
!CPL "TXTYPEABLE"
	for i:=tlast+1 to ntypes do
		converttype(i)
	od
end

function getconstint(symbol owner,unit a, int ownerid=0)int=
!process unit found in tt-tables, and convert to int
	rx_unit(owner, a)

	case a.tag
	when jintconst then
		return a.value
	when jrealconst then
		return a.xvalue
	else
		rxerror_s("Getconstint: not int/real",jtagnames[a.tag])
	esac
	return 0
end

global proc converttype(int m)=
!This 'conversion' is mainly about working out lengths and sizes and offsets
	symbol d,f,owner
	int first,a,b,index,length,lower, elemtype, nbits
	const int maxfield=256
	[maxfield+1]symbol fieldlist
	int oldmodno,pos,ownerid
	int maxalign, nfields, size
	unit plength, plower

	if ttsize[m] then return fi			!assume already done

	owner:=ttowner[m]

	plower:=ttlowerexpr[m]
	plength:=ttlengthexpr[m]

	case ttbasetype[m]
	when tpackstrc,tpackstrz then
		ttsize[m]:=ttlength[m]:=getconstint(owner,plength)
!
	when tvector then
		if m=tarray then CPL "CT:ARRAY/ARRAY" fi
		if ttowner[m] then
			ownerid:=ttowner[m].nameid
		else
			ownerid:=0
		fi
		if plower then
			ttlower[m]:=getconstint(owner,plower,ownerid)
		else
			ttlower[m]:=1
		fi

		if plength then
			ttlength[m]:=getconstint(owner,plength, ownerid)
		else
			ttlength[m]:=0
		fi
		elemtype:=tttarget[m]

		case elemtype
		when tu1,tu2,tu4 then
			nbits:=ttlength[m]*ttbitwidth[tttarget[m]]
			ttsize[m]:=(nbits-1)/8+1
		else
			converttype(tttarget[m])
			ttsize[m]:=ttlength[m]*ttsize[tttarget[m]]
		esac

	when tstruct then
		d:=ttnamedef[m]
		f:=d.deflist

		nfields:=0
		while f do
			if nfields>=maxfield then rxerror("Too many fields") fi
			fieldlist[++nfields]:=f
			f:=f.nextdef
		od

		fieldlist[nfields+1]:=nil
		ntopfields:=nallfields:=0
		maxalign:=1
		index:=1

		scanstruct(1, fieldlist, index, size, 0, ttcaligned[m], maxalign, 2)

		if ttcaligned[m] then
			size:=roundtoblock(size,maxalign)
			d.maxalign:=maxalign
		else
			d.maxalign:=1
		fi

		ttsize[m]:=size
		ttlower[m]:=1
		ttlength[m]:=ntopfields

		d.topfieldlist:=pcm_alloc(symbol.bytes*ntopfields)
		memcpy(d.topfieldlist,&structfields,symbol.bytes*ntopfields)

	when trecord then
!
	else
		CPL "CAN'T DO:",STRMODE(M),strmode(ttbasetype[m])
	esac
end

proc scanstruct(int smode, []symbol &fields, int &index, &isize, offset,
	calign, &maxalign, countmode)=
!process a span of struct fields
!smode=1/0 for structmode/unionmode
!index is next field in fieldlist
!offset=current offset
!maxalign=current max alignment, which can be updated
!isize returns the size of this span
!countmode=2/1/0 to with counting top-level/nested/union fields
	symbol f
	int newoffset, fieldsize, alignment
	int nfields, structmode, ndepth, size

	size:=0

	while f:=fields[index++] do
		case f.nameid
		when structfieldid then
			converttype(f.mode)
			fieldsize:=ttsize[f.mode]

			if calign then
				alignment:=getalignment(f.mode)
				maxalign max:=alignment
				newoffset:=roundtoblock(offset, alignment)
				size+:=newoffset-offset
			else
				newoffset:=offset
			fi
			f.fieldoffset:=newoffset
			F.INDEX:=INDEX-1
			offset:=newoffset
countfields:
			++nallfields
			if countmode then
				structfields[++ntopfields]:=f

			fi
		when structblockid then
			scanstruct(1, fields, index, fieldsize, offset, calign, maxalign,countmode)

		when unionblockid then
			scanstruct(0, fields, index, fieldsize, offset, calign, maxalign, (countmode|1|0))

		when endblockid then
			isize:=size
			return
		esac

		if smode then
			offset+:=fieldsize
			size+:=fieldsize
		else
			size:=max(size,fieldsize)
			countmode:=0
		fi

	od

	isize:=size				!end of fields; tread as endblock
end

proc dobaseclass(int baseclassindex)=
!do fixups needed for baseclass, that couldn't be in in parser until
!user types were fixed up
	ref strec sttype,d,e,newd
	int baseclass,normalexit

	baseclass:=baseclasstable[baseclassindex]
	sttype:=baseclassdef[baseclassindex]

	d:=ttnamedef[baseclass].deflist
	while d do
		e:=sttype.deflist
		normalexit:=1
		while e do
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
				newd:=addsymbol(sttype,d.firstdupl,linkid,0)
				newd.alias:=d
			else
				newd:=addsymbol(sttype,d.firstdupl,d.nameid,0)
				duplfield(d,newd)
				++sttype.nfields
				ttlength[sttype.mode]:=sttype.nfields
				newd.index:=sttype.nfields
				newd.fieldoffset:=(newd.index-1)*varsize
			esac
			addgenfield(newd)


		fi
		d:=d.nextdef
	od
end
=== qq_syslibsdummy.m 0 0 13/16 ===
global const fsyslibs = 0

global func loadsysmodule(ifile pm)int=
	return 0
end
=== qq_tables.m 0 0 14/16 ===
!!---
global enumdata	[0:]ichar stdtypenames,
					[0:]byte stdtypewidths =
!                   names       widths  V P M F
	(tvoid=0,		"void",			0),		! - means object is unassigned

! V Variant Types
	(tint,			"int",			64),	! - 64-bit signed int
	(treal,			"real",			64),	! - 64-bit float

	(tdecimal,		"decimal",		0),
	(trange,		"range",		64),	!
	(tset,			"set",			0),		! - Pascal-like bit-set (array of B)
	(tdict,			"dict",			0),		! - Dictionary of X:Y keys and values

	(tvector,		"vector",		0),		! - User-defined array
	(tbits,			"bits",			0),		! - Array of B

	(tstring,		"string",		0),		! - String of u8 elements
	(tlist,			"list",			0),		! - List of V
	(tarray,		"array",		0),		! - Array of T

	(trecord,		"record",		0),		! - Record of V
	(tstruct,		"struct",		0),		! - Record of T (contains tpackrecord instance)

	(trefvar,		"refvar",		64),	! - Pointer to V
	(trefbit,		"refbit",		128),	! - Pointer to B or bitfield
	(trefpack,		"refpack",		64),	! - Pounter to T

	(tsymbol,		"symbol",		64),	! - Named object
!	(tclosure,		"closure",		0),		! - Closure

	(ttype,			"type",			64),	! - Represents a type-code
	(toperator,		"operator",		64),	! - Represents an operator (as a bytecode op)
	(tretaddr,		"retaddr",		0),		! - Return address descriptor, only on stack 
	(texception,	"except",		0),		! - Exception descriptor, only on stack
	(tnumber,		"number",		0),		! - Only used with .istype


! T Pack Types
	(ti8,			"i8",			8),		! - Narrow numeric types
	(ti16,			"i16",			16),
	(ti32,			"i32",			32),
	(ti64,			"i64",			64),
	(tu8,			"u8",			8),
	(tu16,			"u16",			16),
	(tu32,			"u32",			32),
	(tu64,			"u64",			64),
	(tr32,			"r32",			32),
	(tr64,			"r64",			64),

	(tu1,			"u1",			1),
	(tu2,			"u2",			2),
	(tu4,			"u4",			4),

	(tpackstrc,		"packstrc",		0),		! - counted string field (uses get/setfs) (placeholder)
	(tpackstrz,		"packstrz",		0),		! - zero-terminated string field (placeholder)

	(tstringz,		"stringz",		64),	! - Pointer to zero-terminated string
	(trefproc,		"refproc",		64),	! - Pointer to native function (placeholder)

	(tslice,		"slice",		0),		! - slice of T (placeholder)

end

global const tlast=stdtypenames.upb

global const tlastvartag	= trefbit

!these codes are used in the comments
! o		pcl opcode for jbin, jbinto, junary, jproperty etc
! m		mm-code for jmaths/2
! n		for incr-ops, 0/1 means incr/decr
! n		for jisvoid/jin, normally 0, but 1 reverses to 'not isvoid/in'
! n		for FOR, 0/1 means up/down
! cc	for cmp, is a condition code, eq_cc/lt_cc etc
! cv	for jcvattr, is a compiler-var code

global enumdata [0:]ichar jtagnames,			! "jadd" etc
					[0:]byte jflags,			! 0/1/2 = 0, 1 or 2 subtrees
					[0:]byte jhasvalue = 		! whether yields a value (0, 1 or 2=special)

	(jnone = 0,		$,	0,	0),

	(jlabeldef,		$,	1,	0),

	(jassign,		$,	2,	2),			!n=0/1 for assign/deepcopy
	(jkeyword,		$,	2,	1),
	(jkeyvalue,		$,	2,	1),
	(joperator,		$,	0,	1),

	(jblock,		$,	1,	2),
	(jif,			$,	2,	2),
	(jselect,		$,	2,	2),
	(jwhenthen,		$,	2,	0),
	(jcase,			$,	2,	2),
	(jdocase,		$,	2,	0),
	(jswitch,		$,	2,	2),
	(jdoswitch,		$,	2,	0),
	(jrecase,		$,	1,	0),
	(jfor,			$,	2,	0),		! n=0/1 for up/down
	(jforx,			$,	2,	0),		! n=0/1
	(jforall,		$,	2,	0),
	(jforeach,		$,	2,	0),
	(jdo,			$,	1,	0),
	(jto,			$,	2,	0),
	(jwhile,		$,	2,	0),
	(jrepeat,		$,	2,	0),
	(jtry,			$,	2,	0),
	(jexcept,		$,	2,	0),
	(jraise,		$,	1,	0),
	(jcall,			$,	2,	1),
	(jcallhost,		$,	1,	1),
	(jnil,			$,	0,	1),
	(jswap,			$,	2,	0),
	(jgoto,			$,	1,	0),
	(jstop,			$,	1,	0),
	(jreturn,		$,	1,	2),
	(jeval,			$,	1,	0),

	(jtypeconst,	$,	0,	1),
	(jconvert,		$,	1,	1),
	(jtypepun,		$,	1,	1),
	(jmap,			$,	2,	1),
	(jcmpchain,		$,	1,	1),
	(jname,			$,	0,	1),
	(jsymbol,		$,	1,	1),

	(jintconst,		$,	0,	1),
	(jrealconst,	$,	0,	1),
	(jstringconst,	$,	0,	1),
	(jdecimal,		$,	0,	1),

	(jstrinclude,	$,	1,	1),
	(jdot,			$,	2,	1),
	(jindex,		$,	2,	1),
	(jdotindex,		$,	2,	1),
	(jkeyindex,		$,	2,	1),
	(jloop,			$,	2,	0),		!loopcode = loop_exit etc
	(jptr,			$,	1,	1),
	(jaddrof,		$,	1,	1),		!n=0/1 for &/^
	(jvoid,			$,	0,	1),		!value of 'void'

	(jprint,		$,	2,	0),		!n = set of pr_newline etc
	(jfprint,		$,	2,	0),
	(jnogap,		$,	0,	0),
	(jspace,		$,	0,	0),
	(jfmtitem,		$,	2,	0),
	(jread,			$,	2,	0),

	(jincrload,		$,	1,	1),		!n=0/1 for incr/decr
	(jloadincr,		$,	1,	1),		!n=0/1

	(junary,		$,	1,	1),		!opc is pcl op
	(jbin,			$,	2,	1),		!opc is pcl op

	(jmaths,		$,	1,	1),		!m is maths op
	(jmaths2,		$,	2,	1),		!m is maths op
	(jproperty,		$,	1,	1),		!opc is pcl op

	(jbounds,		$,	1,	1),		!n is 0/1 for range/2-vals

	(jgettype,		$,	1,	1),		!n is 0/1/2 for basetype/type/elemtype
	(jistype,		$,	1,	1),		!t is typecode to match
	(jisvoid,		$,	1,	1),		!n=0/1 for isvoid/isdef

	(jcmp,			$,	2,	1),		!cc is condcode
	(jandl,			$,	2,	1),
	(jorl,			$,	2,	1),
	(jnotl,			$,	1,	1),
	(jistruel,		$,	1,	1),
	(jin,			$,	2,	1),		!n=0/1 for in/not in
	(jinx,			$,	2,	1),
	(junaryto,		$,	1,	1),		!opc is pcl opl (kneg etc)
	(jbinto,		$,	2,	1),		!opc is pcl opl (kadd etc)
	(jandlto,		$,	2,	1),
	(jorlto,		$,	2,	1),
	(jnotlto,		$,	1,	1),
	(jistruelto,	$,	1,	1),
	(jappendto,		$,	1,	1),
	(jconcatto,		$,	1,	1),
	(jidivrem,		$,	0,	2),
	(jmakerange,	$,	2,	1),
	(jmakelist,		$,	1,	1),
	(jmakeset,		$,	1,	1),
	(jmakedict,		$,	1,	1),
	(jcvattr,		$,	0,	1),		!cv is compiler var code
end

global enumdata []u64 symbolnames=
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			'error'),		! Lex error
	(dotsym,			'dot'),		! "."
	(commasym,			'comma'),		! ","
	(semisym,			'semi'),		! ";"
	(colonsym,			'colon'),		! ":"
	(assignsym,			'assign'),		! :=
	(sendtosym,			'sendto'),		! =>
	(pipesym,			'pipe'),		! ->
	(lbracksym,			'lbrack'),		! (
	(rbracksym,			'rbrack'),		! )
	(lsqsym,			'lsq'),		! [
	(rsqsym,			'rsq'),		! ]
	(lcurlysym,			'lcurly'),		! {
	(rcurlysym,			'rcurly'),		! }
	(ptrsym,			'ptr'),		! ^
	(barsym,			'bar'),		! |
	(atsym,				'at'),		! @
	(questionsym,		'question'),		! ?
	(addrsym,			'addr'),		! &
	(rangesym,			'range'),		! ..
	(ellipsissym,		'ellipsis'),		! ...

	(addsym,			'add'),		! +
	(subsym,			'sub'),		! -
	(mulsym,			'mul'),		! *
	(divsym,			'div'),		! /
	(idivsym,			'idiv'),		! %
	(iremsym,			'irem'),		! rem
	(idivremsym,		'idivrem'),		! divrem
	(andlsym,			'andl'),		! and
	(orlsym,			'orl'),		! or
	(iandsym,			'iand'),		! iand
	(iorsym,			'ior'),		! ior
	(ixorsym,			'ixor'),		! xor
	(shlsym,			'shl'),		! <<
	(shrsym,			'shr'),		! >>

	(minsym,			'min'),		! min
	(maxsym,			'max'),		! max
	(appendsym,			'append'),		! append
	(concatsym,			'concat'),		! concat
	(insym,				'in'),		! in
	(inxsym,			'inx'),		! inx
	(powersym,			'power'),		! **

	(eqsym,				'eq'),		! =
	(nesym,				'ne'),		! <>
	(ltsym,				'lt'),		! <
	(lesym,				'le'),		! <=
	(gesym,				'ge'),		! >=
	(gtsym,				'gt'),		! >

	(notlsym,			'notl'),		! not
	(inotsym,			'inot'),		! inot
	(istruelsym,		'istruel'),		! istrue
	(abssym,			'abs'),		! abs
	(ascsym,			'asc'),		! asc
	(chrsym,			'chr'),		! chr

	(mathssym,			'maths'),		! sin etc
	(maths2sym,			'maths2'),		! atan2 etc
	(propsym,			'prop'),		! len etc
	(istypesym,			'istype'),		! .isint etc
	(miscpropsym,		'isvoid'),		! .isvoid/.elemtype etc

	(incrsym,			'incr'),		! -
	(decrsym,			'decr'),		! -

	(eolsym,			'eol'),		! End of line
	(eofsym,			'eof'),		! Eof seen
	(rawnamesym,		'rawname'),		! unassigned name before lookup
	(intconstsym,		'intconst'),		! 123 32 bits signed
	(decimalconstsym,	'decconst'),		! 123 or 123.4 decimal
	(realconstsym,		'fpconst'),		! 123.4 64 bits
	(charconstsym,		'chrconst'),		! 'A' or 'ABCD'
	(stringconstsym,	'strconst'),		! "ABC"

!Second half are tokens that can be yielded after a name lookup:
	(unitnamesym,		'unitname'),		! 
	(namesym,			'name'),		! identifier symbol

	(stdtypesym,		'stdtype'),		! INT, CHAR etc
	(kicharsym,			'ichar'),		! ICHAR
	(kifsym,			'if'),		! 
	(kthensym,			'then'),		! 
	(kelsifsym,			'elsif'),		! 
	(kelsesym,			'else'),		! 
	(kelsecasesym,		'elsecase'),		! 
	(kelseswitchsym,	'elsesw'),		! 
	(kelseselectsym,	'elsesel'),		! 
	(kendsym,			'end'),		! 
	(kunlesssym,		'unless'),		! 
	(kcasesym,			'case'),		! CASE
	(kdocasesym,		'docase'),		! DOCASE
	(krecasesym,		'recase'),		! RECASE
	(kwhensym,			'when'),		! 
	(kforsym,			'for'),		! 
	(ktosym,			'to'),		! TO/DOWNTO
	(kbysym,			'by'),		! 
	(kdosym,			'do'),		! 
	(kwhilesym,			'while'),		! 
	(krepeatsym,		'repeat'),		! 
	(kuntilsym,			'until'),		! 
	(kreturnsym,		'return'),		! 
	(kstopsym,			'stop'),		! 
	(kloopsym,			'loop'),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			'goto'),		! GO/GOTO
	(kswitchsym,		'switch'),		! SWITCH
	(kdoswitchsym,		'doswitch'),		! DOSWITCH
	(kprintsym,			'print'),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		'sprint'),		! SPRINT/SFPRINT
	(kreadsym,			'read'),		! READ/READLN
	(ksreadsym,			'sread'),		! SREAD
	(ksreadlnsym,		'sreadln'),		! SREADLN
	(kprocsym,			'proc'),		! PROC
	(kfunctionsym,		'func'),		! FUNCTION
	(klabelsym,			'label'),		! LABEL
	(krecordsym,		'record'),		! RECORD
	(kstructsym,		'struct'),		! STRUCT
	(kunionsym,			'union'),		! UNION
	(kmodulesym,		'module'),		!
	(kimportsym,		'import'),		!
	(kimportdllsym,		'importd'),		! IMPORTDLL
	(ktypesym,			'type'),		! TYPE
	(krefsym,			'ref'),		! REF
	(kvarsym,			'var'),		! VAR
	(kmacrosym,			'macro'),		! MACRO
	(koperatorsym,		'op'),		! OPERATOR
	(kconstsym,			'const'),		! 
	(kglobalsym,		'global'),		! global
	(kstaticsym,		'static'),		! STATIC
	(kcalignedsym,		'calign'),		! $CALIGNED

	(ktrysym,			'try'),		! 
	(kexceptsym,		'except'),		! 
	(kraisesym,			'raise'),		! 
	(kextendsym,		'extend'),		!
	(kblocksym,			'block'),		!
	(kcastsym,			'cast'),		! CAST
	(compilervarsym,	'compvar'),		! $lineno etc
	(dollarsym,			'dollar'),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			'eval'),		! EVAL
	(ktabledatasym,		'tabdata'),		! tabledata
	(kmapsym,			'map'),		! MAP
	(kclampsym,			'clamp'),		! CLAMP
	(kswapsym,			'swap'),		! SWAP
	(sysconstsym,		'sysconst'),		! nil, etc
	(khostfnsym,		'hostfn'),		! LEFT, CONVLC etc
	(knilsym,			'nil'),		! NIL/PNIL
	(kstrincludesym,	'strincl'),		! STRINCLUDE
	(specialopsym,		'specop'),		! $NEG, $INDEX etc
end

global enumdata =
	pi_const,
	tab_const,
	con_const,
	true_const,
	false_const
end

global enumdata =
	thousand_unit,
	million_unit,
	billion_unit
end

global enumdata [0:]ichar namenames =
	(genericid=0,	$),		! - 		Generic name, not yet resolved
	(programid,		$),		!
	(subprogid,		$),		!
	(moduleid,		$),		!
	(dllmoduleid,	$),		!
	(procid,		$),		!sub/fun/method/op name
	(anonprocid,	$),		!closure
	(dllprocid,		$),		!
	(dllvarid,		$),		!
	(recordid,		$),		!
	(typeid,		$),		!
	(fieldid,		$),		!
	(structfieldid,	$),		!
	(staticid,		$),		!Static var in module/proc/record
	(frameid,		$),		!Local var in proc
	(paramid,		$),		!param in proc
	(dllparamid,	$),		!dll param in dllproc
	(labelid,		$),		!Label name in proc only
	(constid,		$),		!Label name in proc only
	(enumid,		$),		!Label name in proc only
	(aliasid,		$),		!
	(linkid,		$),		!
	(macroid,		$),		!
	(macroparamid,	$),		!
	(structblockid,	$),		! pseudo names used
	(unionblockid,	$),		!
	(endblockid,	$),		!
end

global enumdata [0:]ichar objtypenames =
	(normal_obj=0,	$),
	(slice_obj,		$),
	(extslice_obj,	$)
end

global enumdata [0:]ichar scopenames=
	(local_scope=0,		$), ! 		!module
	(global_scope,		$), ! 		!global/inter-module
	(export_scope,		$), ! 		!export/inter-subprog
end

global tabledata []ichar stnames, []byte stsymbols, []byte stsubcodes=

	("if",			kifsym,			0),
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
	("foreach",		kforsym,		1),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		0),
	("return",		kreturnsym,		0),
	("stop",		kstopsym,		0),

	("redoloop",	kloopsym,		loop_redo),

	("nextloop",	kloopsym,		loop_next),

	("exit",		kloopsym,		loop_exit),

	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("tabledata",	ktabledatasym,	0),
	("enumdata",	ktabledatasym,	1),
	("clamp",		kclampsym,		0),
	("maps",		kmapsym,		0),
	("mapss",		kmapsym,		0),
	("eval",		kevalsym,		0),

	("print",		kprintsym,		0),
	("println",		kprintsym,		pr_newline),
	("fprint",		kprintsym,		pr_format),
	("fprintln",	kprintsym,		pr_format + pr_newline),
	("sprint",		ksprintsym,		pr_sprint),
	("sfprint",		ksprintsym,		pr_sprint + pr_format),

	("cp",			kprintsym,		0),
	("cpl",			kprintsym,		pr_newline),

	("read",		kreadsym,		0),
	("readln",		kreadsym,		pr_newline),

	("cast",		kcastsym,		13),

	("proc",		kprocsym,		0),
	("sub",			kprocsym,		1),

	("function",	kfunctionsym,	0),

	("func",		kfunctionsym,	0),
	("fun",			kfunctionsym,	1),
	("method",		kfunctionsym,	0),

	("type",		ktypesym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),

	("macro",		kmacrosym,		0),

	("static",		kstaticsym,		0),
	("$caligned",	kcalignedsym,	0),
	
	("const",		kconstsym,		0),

	("module",		kmodulesym,		0),
	("import",		kimportsym,		0),

	("importdll",	kimportdllsym,	'D'),
	("strinclude",	kstrincludesym,	0),
	("unless",		kunlesssym,		0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("raise",		kraisesym,		0),

	("global",		kglobalsym,		global_scope),
	("export",		kglobalsym,		export_scope),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),
	("real",		stdtypesym,		treal),

	("string",		stdtypesym,		tstring),
	("list",		stdtypesym,		tlist),
	("array",		stdtypesym,		tarray),
	("vector",		stdtypesym,		tvector),
	("bits",		stdtypesym,		tbits),
	("set",			stdtypesym,		tset),
	("dict",		stdtypesym,		tdict),
	("decimal",		stdtypesym,		tdecimal),
	("longint",		stdtypesym,		tdecimal),
	("typetype",	stdtypesym,		ttype),
	("range",		stdtypesym,		trange),
	("recordtype",	stdtypesym,		trecord),

	("cvoid",		stdtypesym,		tvoid),
	("i8",			stdtypesym,		ti8),
	("i16",			stdtypesym,		ti16),
	("i32",			stdtypesym,		ti32),
	("i64",			stdtypesym,		ti64),

	("bit",			stdtypesym,		tu1),
	("u1",			stdtypesym,		tu1),
	("u2",			stdtypesym,		tu2),
	("u4",			stdtypesym,		tu4),
	("byte",		stdtypesym,		tu8),
	("u8",			stdtypesym,		tu8),
	("u16",			stdtypesym,		tu16),
	("u32",			stdtypesym,		tu32),
	("u64",			stdtypesym,		tu64),

	("r32",			stdtypesym,		tr32),
	("r64",			stdtypesym,		tr64),

	("int8",		stdtypesym,		ti8),
	("int16",		stdtypesym,		ti16),
	("int32",		stdtypesym,		ti32),
	("int64",		stdtypesym,		ti64),

	("word8",		stdtypesym,		tu8),
	("word16",		stdtypesym,		tu16),
	("word32",		stdtypesym,		tu32),
	("word64",		stdtypesym,		tu64),

	("real32",		stdtypesym,		tr32),
	("real64",		stdtypesym,		tr64),

	("stringc",		stdtypesym,		tpackstrc),
	("stringz",		stdtypesym,		tpackstrz),
	("cstring",		stdtypesym,		tpackstrz),
	("ichar",		stdtypesym,		tstringz),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
!	("thousand",	unitnamesym,	thousand_unit),
	("as",			unitnamesym,	0),

	("$lineno",		compilervarsym,	cv_lineno),
	("$strlineno",	compilervarsym,	cv_strlineno),
	("$filename",	compilervarsym,	cv_filename),
	("$modulename",	compilervarsym,	cv_modulename),
	("$function",	compilervarsym,	cv_function),
	("$date",		compilervarsym,	cv_date),
	("$time",		compilervarsym,	cv_time),
!	("$version",	compilervarsym,	J_cvversion),
	("$",			dollarsym,		0),

	("and",			andlsym,		jandl),
	("or",			orlsym,			jorl),
	("iand",		iandsym,		kiand),
	("ior",			iorsym,			kior),
	("ixor",		ixorsym,		kixor),
	("in",			insym,			0),
	("inx",			inxsym,			0),
	("rem",			iremsym,		kirem),
	("divrem",		idivremsym,		kidivrem),
	("min",			minsym,			kmin),
	("max",			maxsym,			kmax),

	("not",			notlsym,		jnotl),
	("istrue",		istruelsym,		jistruel),
	("inot",		inotsym,		kinot),
	("abs",			abssym,			kabs),
	("asc",			ascsym,			kasc),
	("chr",			chrsym,			kchr),
	("sqrt",		mathssym,		mm_sqrt),
	("sqr",			mathssym,		mm_sqr),
	("cos",			mathssym,		mm_cos),
	("sin",			mathssym,		mm_sin),
	("tan",			mathssym,		mm_tan),
	("asin",		mathssym,		mm_asin),
	("acos",		mathssym,		mm_acos),
	("atan",		mathssym,		mm_atan),
	("atan2",		maths2sym,		mm_atan2),
	("sign",		mathssym,		mm_sign),
	("log",			mathssym,		mm_log),
	("log10",		mathssym,		mm_log10),
	("exp",			mathssym,		mm_exp),
	("round",		mathssym,		mm_round),
	("floor",		mathssym,		mm_floor),
	("ceil",		mathssym,		mm_ceil),
	("fract",		mathssym,		mm_fract),
	("fmod",		maths2sym,		mm_fmod),

	("append",		appendsym,		kappend),
	("concat",		concatsym,		kconcat),

	("len",			propsym,		klen),
	("lwb",			propsym,		klwb),
	("upb",			propsym,		kupb),
	("bounds",		propsym,		kbounds),
!	("bitwidth",	propsym,		jbitwidth),
	("bytes",		propsym,		kbytesize),
	("isfound",		propsym,		kisfound),
	("dictitems",	propsym,		kdictsize),

!	("odd"	,		propsym,		kmaxval),
!	("even",		propsym,		kmaxval),

!	("basetype",	propsym,		jbasetype),
!!	("usertype",	propsym,		jusertype),
	("basetype",	miscpropsym,	'b'),
	("elemtype",	miscpropsym,	'e'),

!	("dictitems",	miscpropsym,	'dict'),

	("isvoid",		miscpropsym,	'v'),
	("isdef",		miscpropsym,	'd'),
	("defined",		miscpropsym,	'd'),

	("isint",		istypesym,		tint),
	("isreal",		istypesym,		treal),
	("islist",		istypesym,		tlist),
	("isstring",	istypesym,		tstring),
	("isrange",		istypesym,		trange),
	("ispointer",	istypesym,		trefvar),
	("isarray",		istypesym,		tarray),
	("isrecord",	istypesym,		trecord),
	("isset",		istypesym,		tset),
	("isnumber",	istypesym,		tnumber),
!	("ismutable",	istypesym,		jismutable),
!	("odd",			istypesym,		jodd),
!	("even",		istypesym,		jeven),

	("fi",			kendsym,		kifsym),
	("esac",		kendsym,		kcasesym),
	("od",			kendsym,		kdosym),

	("nil",			knilsym,		0),
	("con",			sysconstsym,	con_const),
	("pi",			sysconstsym,	pi_const),
	("true",		sysconstsym,	true_const),
	("false",		sysconstsym,	false_const),

	("$neg",		specialopsym,	'-'),
!	("$index",		specialopsym,	'[]'),

	("$$dummy",		0,				0)
end

global enumdata [0:]ichar hostfnnames, [0:]byte hostnparams, [0:]byte hostisfn,
			[0:]byte hostinternal =
!                    name  np isfn int
	(h_dummy=0,         $,  0,  0,  1),
                                        
	(h_startprint,      $,  1,  0,  1), !startprint(x)  Set o/p dev for following print items
	(h_startprintcon,   $,  0,  0,  1), !startprintcon()    Set console dev for following print items
	(h_strstartprint,   $,  0,  0,  1), !strstartprint()    Set o/p dev for internal string
	(h_setformat,       $,  1,  0,  1), !setformat(x)   Set up format string for following print items up to str/endprint
	(h_endprint,        $,  0,  0,  1), !endprint() Restore o/p dev
	(h_strendprint,     $,  0,  1,  1), !strendprint()  Restore o/p dev, and return result as string
	(h_print,           $,  2,  0,  1),     !print(x,[y])   Print x, using default format code or y
	(h_print_nf,        $,  1,  0,  1),     !print(x)       Print x, using default format code
                                        
	(h_println,         $,  0,  0,  1), !println()  Print newline
	(h_printnogap,      $,  0,  0,  1), !printnogap()   Suppress any gap before next print item
	(h_printspace,      $,  0,  0,  1), !printspace     Extra space at beg or end
                                        
	(h_readln,          $,  1,  0,  1), !sreadln(x) Read line from console or device x, into read buffer
	(h_sreadln,         $,  1,  1,  0), !sreadln(x) Read line from console or device x, into read buffer
	(h_sread,           $,  1,  1,  0), !sread([x]) Read item from read buffer, with/without format code
	(h_rereadln,        $,  0,  0,  0), !sread([x]) Read item from read buffer, with/without format code
	(h_reread,          $,  0,  0,  0), !sread([x]) Read item from read buffer, with/without format code
                                        
	(h_strtoval,        $,  2,  1,  0), !
	(h_tostr,           $,  2,  1,  0), !
                                        
	(h_leftstr,         $,  3,  1,  0),
	(h_rightstr,        $,  3,  1,  0),
	(h_convlc,          $,  2,  1,  0),
	(h_convuc,          $,  2,  1,  0),
                                        
	(h_waitkey,         $,  0,  1,  0),
	(h_testkey,         $,  0,  1,  0),
	(h_execwait,        $,  3,  1,  0),
	(h_execcmd,         $,  3,  1,  0),
	(h_system,          $,  1,  1,  0),
                                        
	(h_makestr,         $,  2,  1,  0),
	(h_makeref,         $,  2,  1,  0),
                                        
	(h_new,             $,  4,  1,  0),
                                        
	(h_getcmdparam,     $,  1,  1,  0),
	(h_gethostname,     $,  0,  1,  0),
	(h_getprogname,     $,  0,  1,  0),
                                        
	(h_$setdebug,       $,  1,  0,  0),
	(h_$test2,          $,  2,  1,  0),
	(h_$test,           $,  3,  1,  0),
	(h_$refcount,       $,  1,  1,  0),
                                        
	(h_ticks,           $,  0,  1,  0),
	(h_clock,           $,  0,  1,  0),
	(h_sleep,           $,  1,  0,  0),
	(h_random,          $,  1,  1,  0),
	(h_gethash,         $,  1,  1,  0),
	(h_getos,           $,  0,  1,  0),
	(h_iswindows,       $,  0,  1,  0),
	(h_setmesshandler,  $,  1,  0,  0),
	(h_$getparam,       $,  1,  1,  0),
	(h_makeempty,       $,  1,  1,  0),
	(h_$smallmemtotal,  $,  0,  1,  0),
	(h_$id,             $,  1,  1,  0),
	(h_copy,            $,  1,  1,  0),
	(h_$nan,            $,  0,  1,  0),
	(h_$infinity,       $,  0,  1,  0),
                                        
	(h_$nprocs,         $,  0,  1,  0),
	(h_$procname,       $,  1,  1,  0),
	(h_$procref,        $,  1,  1,  0),
                                        
	(h_allocexec,       $,  1,  1,  0),
	(h_runnative,       $,  2,  1,  0),
	(h_setlwb,          $,  2,  0,  0),
                                        
	(h_last,            $,  0,  0,  1)
end

global []byte D_binopset = (
	andlsym, orlsym, eqsym, nesym, ltsym, lesym, gtsym, gesym, addsym,
	subsym, mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym,
	shlsym, shrsym, minsym, maxsym,	concatsym, powersym,
	idivremsym,  maths2sym, appendsym, addrsym )

global [0..symbolnames.upb]byte binopset

global []byte D_unaryopset = (
	notlsym, inotsym, abssym, istruelsym, ascsym, chrsym,
	mathssym)

global [0..symbolnames.upb]byte unaryopset

global []byte D_addopset=(addsym, subsym, iandsym, iorsym, ixorsym,
		concatsym, appendsym, minsym, maxsym, addrsym)

global []byte D_cmpopset=(eqsym, nesym, ltsym, lesym, gesym, gtsym)

global []byte D_mulopset=(mulsym, divsym, idivsym, iremsym, shlsym, shrsym, idivremsym)

global [0..symbolnames.upb]byte addopset
global [0..symbolnames.upb]byte cmpopset
global [0..symbolnames.upb]byte mulopset
global [0..symbolnames.upb]byte exprendset

global []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,namesym,
	incrsym,decrsym,intconstsym,decimalconstsym,realconstsym,charconstsym,
	stringconstsym,stdtypesym, kmapsym, lcurlysym,
!	ksprintsym,ksreadsym,ksreadlnsym,knewsym,dollarsym,compilervarsym, kclampsym,
	ksprintsym,ksreadsym,ksreadlnsym,dollarsym,compilervarsym, kclampsym,
	krefsym, kcastsym, ellipsissym,
	knilsym, khostfnsym, kifsym, krecordsym, kstructsym)

global [0:symbolnames.len]byte exprstarterset

!type tables
global const maxtype=250

global [0..maxtype]ichar ttname
global [0..maxtype]symbol ttnamedef
global [0..maxtype]int16 ttbasetype
global [0..maxtype]int16 tttarget

global [0..maxtype]int ttlower
global [0..maxtype]int ttlength

global [0..maxtype]unit ttlowerexpr
global [0..maxtype]unit ttlengthexpr

global [0..maxtype]int ttsize
global [0..maxtype]byte ttbitwidth
global [0..maxtype]symbol ttfields		!for initialially anonymous record field lists
global [0..maxtype]byte ttcaligned
global [0..maxtype]symbol ttowner
global int ntypes
global int firstusertype				!starts at ntypes+1, augments for each sp

global const int maxuserxtype=5000
global int nuserxtypes
global int userxtypebase			!first index (growing downwards) of userxtypes in current module
global ref userxrec userxmodelist	!list of all references to userx modes

global [0:maxuserxtype]symbol ttnamedefx
global [0:maxuserxtype]int ttxmap
global [0:maxuserxtype]byte ttxmoduleno

global [0..h_last]byte hostlvset

proc start=
!	translate into an instant lookup format
	int i

	for i:=1 to D_binopset.len do
		binopset[D_binopset[i]]:=1
		exprstarterset[D_binopset[i]]:=1
	od

	for i:=1 to D_unaryopset.len do
		unaryopset[D_unaryopset[i]]:=1
		exprstarterset[D_unaryopset[i]]:=1
	od

	for i:=1 to D_exprstarterset.len do exprstarterset[D_exprstarterset[i]]:=1 od

	exprendset[semisym]:=1
	exprendset[commasym]:=1
	exprendset[rsqsym]:=1
	exprendset[rbracksym]:=1
	exprendset[kendsym]:=1
	exprendset[kdosym]:=1
	exprendset[ktosym]:=1

	for i:=1 to D_addopset.len do addopset[D_addopset[i]]:=1 od
	for i:=1 to D_mulopset.len do mulopset[D_mulopset[i]]:=1 od
	for i:=1 to D_cmpopset.len do cmpopset[D_cmpopset[i]]:=1 od

	for i in 0..tlast do
		ttname[i]:=stdtypenames[i]
		ttbasetype[i]:=i
		ttlower[i]:=1
		ttbitwidth[i]:=stdtypewidths[i]
		ttsize[i]:=stdtypewidths[i]/8
	od

	ntypes:=tlast
end

!flags used with print/fprint/fprint
global const pr_newline = 1
global const pr_format = 2
global const pr_sprint = 4

global enumdata []ichar cvnames =
	(cv_lineno,		$),
	(cv_strlineno,	$),
	(cv_filename,	$),
	(cv_modulename,	$),
	(cv_function,	$),
	(cv_date,		$),
	(cv_time,		$),
end

global enumdata []ichar loopnames =
	(loop_redo,		$),		!must be in this order: start of loop body
	(loop_next,		$),		!end of loop body
	(loop_exit,		$),		!past end of loop
end

global enumdata []ichar mathsnames =
	(mm_sqrt,		$),
	(mm_sqr,		$),
	(mm_sin,		$),
	(mm_cos,		$),
	(mm_tan,		$),
	(mm_asin,		$),
	(mm_acos,		$),
	(mm_atan,		$),
	(mm_sign,		$),
	(mm_log,		$),
	(mm_log10,		$),
	(mm_exp,		$),
	(mm_round,		$),
	(mm_floor,		$),
	(mm_ceil,		$),
	(mm_fract,		$),
	(mm_fmod,		$),
	(mm_atan2,		$),
end

!can't start from 0, as 0 in cmpchain list means no more conds
global enumdata []ichar condnames, []byte revconds =
	(eq_cc,		"eq",	ne_cc),
	(ne_cc,		"ne",	eq_cc),
	(lt_cc,		"lt",	ge_cc),
	(le_cc,		"le",	gt_cc),
	(ge_cc,		"ge",	lt_cc),
	(gt_cc,		"gt",	le_cc),
end
=== qq_show.m 0 0 15/16 ===
!labels are just numbers 1,2,3 which index both of these tables
!labelblocktable is the pclblock no (as all labels are shared across the program)
!labeloffsettable is the offset into the pclblock

!global const labelinitalloc=8192
!global ref[]int labeloffsettable
!global int labelalloc
!global int nextlabelno
ref[0:]int labelmap
int currlineno
symbol currpclproc

strbuffer pclv
global ref strbuffer pcldest = &pclv

const logfile="qq.log"

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
!p is a tagrec
	ref unitrec q
	symbol d
	int t,flags
	ichar idname
	int64 a
	real32 x32

	if p=nil then
		return
	fi

	currlineno:=p.pos iand 16777215

!CPL "PRINTUNIT:",P,P.TAG,JTAGNAMES[P.TAG],=LEVEL,CURRLINENO


!if p.lineno then
!fi

	print @dev,p,":"
	print @dev,getprefix(level,prefix,p)

	idname:=jtagnames[p.tag]
	if idname^='j' then ++idname fi
	print @dev,idname,,": "


	case p.tag
	when jname then
		d:=p.def
!		print @dev,d.name,namenames[d.nameid],"Module:",p.moduleno
		if d.owner then print @dev,d.owner.name,,"." fi

		print @dev,d.name,namenames[d.nameid],"Module:",p.moduleno
		if d.truename and d.nameid=dllprocid then
			print @dev," ",d.truename
		fi

	when jintconst then
		print @dev,p.value

!	when jenumconst then
!		fprint @dev,"# (#:#)",p.value, strmode(p.mode),getenumname(p.mode, p.value)

	when jrealconst then
		print @dev,p.xvalue

	when jstringconst then
		fprint @dev,"""#""",p.svalue

	when jdecimal then
		print @dev,p.svalue,,"L"

	when jcmp then
		print @dev, condnames[p.condcode]

	when jcmpchain then
		for i to 4 do
!CP P.CMPGENOP[I],$
			if p.cmpconds[i]=0 then exit fi
			print @dev,condnames[p.cmpconds[i]],$
		od

	when joperator then
		print @dev, pclnames[p.pclop]

	when jassign, jin, jgettype, jisvoid, jincrload, jloadincr,
			jaddrof, jfor, jforx, jprint, jfprint, jread then
		print @dev,p.flag
!CPL JTAGNAMES.LEN

	when jbin, junary, jproperty, jbinto, junaryto then
		fprint @dev, "<#>",pclnames[p.pclop]+1

	when jmaths, jmaths2 then
		fprint @dev, "<#>",mathsnames[p.mathsop]+3

!when jmakestrtype then
!	print @dev, ttname[p.strtype]

!when jimport then
!	print @dev, p.def.name

!when jprocdef then
!	print @dev, p.def.name
!	if p.mglobal then print @dev, " (global)" fi
!	if p.isfn then print @dev, " (func)" fi
!
!when jrecorddef then
!	print @dev, p.def.name
!	if p.mglobal then print @dev, " (global)" fi
!
!
	when jmakelist then
		print @dev,p.lower,,":",=p.length,ttname[p.elemtype]

!when jnew then
!	print @dev,p.nparams

!when jframesize then
!	print @dev,"Framesize",p.value
!
!when jconvert,jtypepun then
!	print @dev,"Mode",ttname[p.mode]
!
	when jtypeconst,jconvert,jtypepun then
		print @dev,ttname[p.mode]

!when jpacktypeconst then
!	print @dev,getpacktypename(p.value)

!when kdecimal then
!	print @dev,p.svalue,"Len:",p.slength
!
!when ktypeconst then
!	print @dev,typename(p.mode),typename(p.value)
!
!when koperator then
!	print @dev,pclnames[p.opcode]+1
!
!when kconvert,ktypepun then
!	print @dev,convnames[p.opcode]," to:",strmode(p.newmode)
!
!when kmakelist,kmultexpr then
!	print @dev,"Len:",p.length
!
!when kdot then
!	print @dev,"Offset:",p.offset
!
	when jcallhost then
		print @dev,hostfnnames[p.index]+2

!when kindex, kptr then
!
!when kexit,kredo,krestart,knext then
!	print @dev,"#",,p.index
!
	esac

	println @dev
	flags:=jflags[p.tag]

	if flags>=1 then printunitlist(dev,p.a,level+1,"1") fi
	if flags=2 then printunitlist(dev,p.b,level+1,"2") fi

end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
	if p=nil then return fi

	while p do
		printunit(p,level,prefix,dev)
		p:=p.nextunit
	od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
	static [1024]char str
	[1024]char indentstr
	[16384]char modestr

	indentstr[1]:=0
	!if level>10 then level:=10 fi
	if level>20 then level:=10 fi

	to level do
		strcat(&.indentstr,"- ")
	od

	strcpy(&.str,getlineinfok())
	strcat(&.str,&.indentstr)
	strcat(&.str,prefix)
	if prefix^ then
		strcat(&.str," ")
	fi

	return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

!	sprintf(&.str,"%04d ",currlineno)
	strcpy(str,strint(currlineno,"z4"))
	return &.str
end

global proc printglobalsymbols(filehandle f=nil)=
	println @f,"PROC Global Symbol Table"
	println @f

	printst(f,stprogram)

!	println @f,"Global Proc Table",nglobalprocs
!	for i to nglobalprocs do
!		println @f,i,,":",globalproctable[i].name,namenames[globalproctable[i].nameid]
!	od

end

global proc printst(filehandle f,symbol p,int level=0)=
	ref strec q

	printstrec(f,p,level)

	q:=p.deflist

	while q<>nil do
		printst(f,q,level+1)
		q:=q.nextdef
	od
end

proc printstrec(filehandle f,symbol p,int level)=
	strec dd
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset,n
	const tabstr="    "
	[256]char str
	ichar s

!CPL "PRINTSTREC",P.NAME,NAMENAMES[P.NAMEID]

!RETURN

	offset:=0
	to level do
		print @f,tabstr
		offset+:=4
		col+:=4
	od

	print @f,padstr(p.name,22-offset,"-")
	print @f, padstr(namenames[p.nameid],12,".")

	col:=40
	dd:=p^


	if dd.isimport then
		print @f,"Imp "
	elsif dd.isglobal then
		print @f,(dd.isglobal|"Glob ","Exp "|"Local ")
	fi

	if dd.mbyref then
		print@f,"byref "
	fi
	if dd.moptional then
		print@f,"opt "
	fi

	if dd.moduleno then
		fprint @f,"Modno:#",dd.moduleno
	fi

	print @f,"=========="

	if dd.owner then
		fprint @str,"(#)",dd.owner.name
		print @f, padstr(&.str,18,"-")
	else
		print @f, padstr("()",18,"-")
	fi


	case dd.nameid
	when fieldid,frameid,paramid,enumid then
		print @f," Ix:",dd.index,," "
		if dd.nameid=fieldid and dd.atfield then
			print @f,"@",dd.atfield.name,$
		fi
		print @f," Offset:",dd.fieldoffset,," "

	when structfieldid then
		print @f," Offset:",dd.fieldoffset,," Ix:",DD.INDEX,$
	when recordid then
		print @f," Nfields:",dd.nfields,," "
	when procid, dllprocid, anonprocid then
		fprint @f," Nparms:# ",dd.nparams,=dd.misfunc

	esac	

	case dd.nameid
	when frameid, staticid,constid,macroid,paramid,dllparamid then
		if dd.code then
			case dd.initcode
			when 3 then s:="::="
			when 2 then s:=":="
			else s:="="
			esac

			print @f, s, strexpr(dd.code).strptr,$
		fi
	esac

	if dd.mode then
		fprint @f,"Mode:#",strmode(dd.mode),dd.mode
	fi
!	fprint @f,"Mode:#",(dd.mode)
!
!	PRINT @F," Moduleno:",P.MODULENO
!
	println @f
	ichar tab:="          "
end

global proc printtypetables(filehandle f)=
	symbol d

CPL "PRINT TYPE TABLES",NUSERXTYPES
	println @f,"PROC TYPE TABLES"
!	for m:=0 to ntypes do
	for m:=0 to ntypes do
!	for m:=tlast+1 to ntypes do
!		fprint @f, "#: #  (#)",i:"3",ttname[i]:"jl12",ttnamedef[i]
		fprintln @f, "#: # ",m:"3",ttname[m]:"jl12"
		d:=ttnamedef[m]

!		if d then
!			println @f,"	ST=",d
			println @f,"	ST=",d
			println @f,"	Len=",ttlength[m], "Lower",ttlower[m]
			println @f,"	Size=",ttsize[m]
			println @f,"	Basetype=",ttbasetype[m],ttname[ttbasetype[m]]
			println @f,"	Target=",tttarget[m],ttname[tttarget[m]]
!			println @f,"	Ispacked=",ttispacked[m]
			println @f,"	Caligned=",ttcaligned[m]

		d:=ttfields[m]
		if d then
			println @f,"	Fields:"
			while d do
				println @f,"		",d.name, (d.mode|strmode(d.mode)|"")
				d:=d.nextdef
			od
		fi

	od

	ref userxrec p
INT M
	p:=userxmodelist
!	rescan:=0

!global [0:maxuserxtype]symbol ttnamedefx
!!global [0:maxuserxtype]symbol ttnamedefx2
!global [0:maxuserxtype]int ttxmap
!global [0:maxuserxtype]byte ttxmoduleno
!
	for i:=1 to nuserxtypes do
		println @f, i, -i,ttnamedefx[i].name
	od
end

global proc showsttree=
	filehandle f
	ifile m
	symbol d
	ref genfieldrec g
	ref procrec p

	return unless fshowst

	f:=fopen("ST","w")
	printglobalsymbols(f)
!	printglobalsymbols_full(f)

	println @f
	println @f,"Modules",nmodules
	for i to nmodules do
		m:=modules[i]
		IF M THEN
			println @f,"	",,i,,":",m.name,=m.compiled,=m.pcstart,=m.pcsize
		ELSE
			PRINTLN @F,"MODULE",I,"MISSING"
		FI
	od

!	println @f
!	println @f,"Source Files",nsourcefiles
!	for i to nsourcefiles do
!!		println @f,"	",,i,,":",m.name,=m.startfn,=m.mainfn,=m.ast,=m.pcstart,=m.pcsize,
!		println @f,"	",,i,,":",sourcefilenames[i],=sourcefilesys[i],=sourcefilesupport[i]
!	od
!
	println @f
	println @f,"PROC Global GenField Table",ngenfields
	for i to ngenfields do
		g:=genfieldtable[i]
		if g=nil then nextloop fi
		fprintln @f,"   #) #:",i,g.def.name
		while g do
			d:=g.def
			println @f,"      ",d.name, namenames[d.nameid],d.owner.name
			g:=g.nextdef
		od
	od
	println @f


	println @f,"DLL Table", nlibfiles
	for i to nlibfiles do
		println @f, i,":",libtable[i].name, dllinsttable[i], libtypes[i]:"c"
	od
	println @f

	println @f,"DLL Proc Table", ndllprocs
	for i to ndllprocs do
		d:=dllproctable[i]
		println @f, i,":",d.name, dllproclibindex[i], dllprocaddr[i],(d.mvarparams|"Variadic"|""),
			libtypes[dllproclibindex[i]]:"c",=D.INDEX,=DLLPROCTABLE[D.INDEX],=D
	od
	println @f

	println @f,"All Proc Table",nproclist
	p:=proclist
	while p do
		println @f,"Proc:",p.def.name,p.def.owner.name
		p:=p.nextproc
	od
	println @f


	fclose(f)
end

global proc showtypes=
	filehandle f
	ref filerec m

	return unless fshowtypes
	return when runcode=run_cc

	f:=fopen("TYPES","w")
	printtypetables(f)

	fclose(f)
end

global proc showast(isubprog sp, ichar file)=
	filehandle f
	ifile pm
	symbol d
	int k,i

	return when runcode=run_cc

	f:=fopen(file,"w")
	return unless f

	println @f,"PROC",file,,":"


	if sp then
		showast2(f, sp)
	else
		for i to nsubprogs do
			showast2(f, subprogs[i])
		od
	fi

	fclose(f)
end

global proc showast2(filehandle f, isubprog sp)=
	ifile pm
	symbol d, e
	int k,i

	println @f,"Proc Subprog",sp.name,,": ******\n"
	for i:=sp.firstmodule to sp.lastmodule do
		pm:=modules[i]

		println @f,"Module:",pm.name
		printunit(pm.ast, dev:f)
		d:=pm.def.deflist
		while d, d:=d.nextdef do
!			if d.nameid=procid then
			if d.nameid=procid then
!CPL "PROC:",D.NAME
				println @f,"\n---PROC",d.name
				printunit(d.code, dev:f)

				e:=d.deflist
				while e, e:=e.nextdef do
					if e.nameid=anonprocid then
						println @f,"\n---ANONPROC",e.name
CPL "ANON",E.CODE
						printunit(e.code, dev:f)
					fi
				od

			fi
		od
		println @f
	od
end

global proc showlogfile=
	[256]char str
	filehandle logdev

!CPL "SHOWLOG1"
!OS_GETCH()

	if fshowpcl1+fshowpcl2+fshowpcl3+fshowast1+fshowast2+
			fshowst+fshowtypes+fshowmodules+fshowstflat=0 then return fi
!CPL "SHOWLOG2",RUNCODE, RUN_CC
	if runcode=run_cc then
		return
	fi

CPL "PRESS KEY"; OS_GETCH()

!CPL "SHOWLOG2"

	if fshowst then
		showsttree()
	fi
!CPL "SHOWLOG3"

	if fshowstflat then
		showstflat()
	fi
!CPL "SHOWLOG4"

	if fshowtypes then
		showtypes()
	fi

!CPL "SHOWLOG5"
	logdev:=fopen(logfile,"w")

!CPL "SHOWLOG3",=FSHOWMODULES
	if fshowmodules then showmoduleinfo(logdev) fi
!CPL "SHOWLOG6"

	if runcode>=fixup_cc and fshowpcl3 then addtolog("PCL3",logdev) fi
	if runcode>=gencode_cc and foptimise and fshowpcl2 then addtolog("PCL2",logdev) fi
	if runcode>=gencode_cc and fshowpcl1 then addtolog("PCL1",logdev) fi
	if runcode>=names_cc and fshowast2 then addtolog("AST2",logdev) fi
	if runcode>=parse_cc and fshowast1 then addtolog("AST1",logdev) fi
	if fshowst then addtolog("ST",logdev) fi
	if fshowstflat then addtolog("STFLAT",logdev) fi
	if fshowtypes then addtolog("TYPES",logdev) fi
	fclose(logdev)

!	fprint @&.str,"c:/m/scripts/med.bat -w #",logfile
	fprint @&.str,"c:/m/scripts/med.bat #",logfile
!CPL =&.STR
!os_GETCH()

!	os_execwait(&.str,1,nil)
	os_execwait(str,0,nil)

end

proc addtolog(ichar filename, filehandle logdest)=
filehandle f
int c

f:=fopen(filename,"rb")
if f=nil then return fi

do
	c:=fgetc(f)
	exit when c=c_eof
	fputc(c,logdest)
od
fclose(f)
end

global proc showstflat=
	filehandle f
	symbol p

	return unless fshowstflat

	f:=fopen("STFLAT","w")

	println @f,"GLOBAL FLAT SYMBOL TABLE:"

	for i:=0 to hashtable.upb-1 do
		p:=cast(&hashtable[i])
		if p.name then
			case p.symbolcode
			when namesym then
				println @f,i,p,":",p.name,symbolnames[p.symbolcode]:"m",namenames[p.nameid]
				p:=p.nextdupl
				while p do
					int sym:=p.symbolcode
					if sym=0 then sym:=errorsym fi
					println @f,"	",p,p.name,symbolnames[sym]:"m",namenames[p.nameid],
						"(From",(p.owner|p.owner.name|"-"),,")"
					p:=p.nextdupl
				od
			esac
		fi
	od
!
	fclose(f)
end

global proc showmoduleinfo(filehandle dev)=
	ifile pm
	ref subprogrec ps
	static ichar tab="    "

CPL "SMI0"
	println @dev,"Project Structure:"
	println @dev,"---------------------------------------"
	println @dev,"Modules",nmodules
	for i to nmodules do
		pm:=modules[i]
!CPL "SMI",I,PM

!		if i>1 and pm.subprogno<>modules[i-1].subprogno then
!			println @dev
!		fi
!
		print @dev, tab,i:"2",pm.name:"16jl", "Lead:",pm.islead, "Sys:",pm.issyslib, "Path:",pm.path,
			"Sub:",subprogs[pm.subprogno].name,"File:",pm.filespec
!		if pm.stmacro then
!			print @dev," Alias:",pm.stmacro.name
!		fi
!		print @dev, "START:",pm.startfn
!		if i=mainmoduleno then print @dev, "<MAIN>" fi
!	PRINT @DEV,"<TEMP MODULE INFO>"
		println @dev
	od
	println @dev

	println @dev,"Subprograms",nsubprogs
	for i to nsubprogs do
		ps:=subprogs[i]
		println @dev, tab,i,ps.name,"Sys:",ps.issyslib, "Path:",ps.path,
			 "Spec:",ps.filespec,"Comp:",ps.compiled
		if ps.firstmodule then
			print @dev, tab,tab,ps.firstmodule,ps.lastmodule,,": "
			for j:=ps.firstmodule to ps.lastmodule do
				print @dev, modules[j].name,$
			od
			println @dev
		fi
	od
	println @dev
!
!	println @dev,"Sourcefiles",nsourcefiles
!	for i to nsourcefiles do
!		println @dev, tab,i,sourcefilenames[i]
!		if sourcefilepaths[i]^ then println @dev, tab,tab,sourcefilepaths[i] fi
!		println @dev, tab,tab,sourcefilespecs[i]
!		println @dev, tab,tab,=sourcefilesizes[i]
!		println @dev, tab,tab,=sourcefilesys[i]
!		println @dev, tab,tab,=sourcefilesupport[i]
!!		println @dev, tab,tab,=sourcefiledupl[i]
!	od
!	println @dev
!
!!	println @dev,"Header Variables:"
!!	for i to headervars.len do
!!		fprintln @dev,"\t#: #",headervarnames[i],headervars[i]
!	od
!	println @dev
!	println @dev,"---------------------------------------"

	return unless stprogram
	println @dev,"Symboltable:"
	symbol d:=stprogram.deflist
	while d, d:=d.nextdef do
		ichar id
		case d.nameid
		when moduleid then id:="Mod"
		when subprogid then id:="Sub"
		else id:="---"
		esac
		fprintln @dev,"    # # (m#, s#)",d.name,id,d.moduleno, d.subprogno
	od
	println @dev

end

global proc printsymbol(ref lexrec lp)=
	lexrec l
	l:=lp^

!	printf("%-18s",symbolnames[l.symbol])
!	print symbolnames[l.symbol]:"18 jl"
	print symbolnames[l.symbol]:"m 18 jl"

	case l.symbol
	when namesym then
!	print l.symptr.name

		printstr_n(l.symptr.name,l.symptr.namelen)
	when intconstsym then
		case l.subcode
		when tint then print l.value,"int"
!		when tword then print l.uvalue,"word"
		else print l.value
		esac

	when realconstsym then
		print l.xvalue

	when stringconstsym then
		print """",$
		printstr(l.svalue)
		print $,""""
	when charconstsym then
		print "'",$
		printstr(l.svalue)
		print $,"'"
	when decimalconstsym then
		printstr(l.svalue)
		print "L"
	when assignsym,addrsym,ptrsym,rangesym then
		print jtagnames[l.subcode]
	elsif l.subcode then
		print "#",l.subcode
	end

	println

end

global function strmode(int t, expand=0)ichar=
	static [2048]char str

	istrmode(t,&.str,expand)
	return str
end

proc istrmode(int t, ichar dest,int expand=1)=
	static [2048]char str
	symbol d

	if t<0 then
		strcpy(dest,"*")
		strcat(dest,ttnamedefx[-t].name)
!		if ttnamedefx2[-t] then
!			strcat(dest,".")
!			strcat(dest,ttnamedefx2[-t].name)
!		fi
		return
	fi

!CPL "MM1",T

	if t<tlast then
!CPL "MM2",T,=TTNAME[T]
		strcpy(dest,ttname[t])
		return
	fi

	case ttbasetype[t]
	when trefpack then
		strcpy(dest,"ref ")
		istrmode(tttarget[t], dest+strlen(dest),0)
	when tvector then
		fprint @dest, "[#..#]",ttlower[t],ttlength[t]+ttlower[t]-1
		istrmode(tttarget[t], dest+strlen(dest),0)

!	when tslice then
!		strcpy(dest, "slice[]")
!		istrmode(tttarget[t], dest+strlen(dest),0)

	when tstruct then
!		if not expand then recase else fi
		if not expand then goto $else fi
		strcpy(dest,"struct(")
dostruct:
		d:=ttfields[t]
		while d, d:=d.nextdef do
			istrmode(d.mode, dest+strlen(dest),0)
			strcat(dest, " ")
			strcat(dest, d.name)
			if d.nextdef then
				strcat(dest, ", ")
			fi
		od
		strcat(dest,")")
	when trecord then
!		if not expand then recase else fi
		if not expand then goto $else fi
		strcpy(dest,"record(")
		goto dostruct

!	when tenum then
!!		if not expand then recase else fi
!		if not expand then $else fi
!		strcpy(dest,"enum(")
!		d:=ttfields[t]
!		while d, d:=d.nextdef do
!			if d.nameid=enumid and d.mode=t then
!				strcat(dest, d.name)
!				strcat(dest, " ")
!			fi
!		od
!		strcat(dest,")")

	else
$else:
!CPL "STRMODE BASETYPE"!,STDTYPENAMES[TTBASETYPE[T]]
		strcpy(dest,ttname[t])
	esac
end

global proc deletetempfiles=
	remove("PCL1")
	remove("PCL2")
	remove("PCL3")
	remove("AST1")
	remove("AST2")
	remove("TYPES")
	remove("STFLAT")
	remove("ST")
!	remove(logfile)
end
=== qq_showpcl.m 0 0 16/16 ===
int currlineno
symbol currpclproc

global proc showpcl(isubprog sp, int pass)=
	filehandle f

	return when runcode=run_cc

	gs_init(pcldest)
	gs_str(pcldest, "PROC ALL PCL pass:")
	gs_strint(pcldest, pass)
	gs_line(pcldest)

	if sp then
		showpcl2(sp, pass)
	else
		for i to nsubprogs do
			showpcl2(subprogs[i], pass)
		od
	fi

!CPL "SHOWPCL", PASS
	f:=fopen((pass|"PCL1", "PCL2"|"PCL3"), "w")
	if not f then return fi
	gs_println(pcldest, f)
!CPL "WROTE PCL FILE"
!OS_GETCH()

	fclose(f)
end

global proc showpcl2(isubprog sp, int pass)=

!CPL "SHOWPCL2", SP.NAME

	for i:=sp.firstmodule to sp.lastmodule do
!CPL "WALLPCL", I, MODULES[I].NAME
		writeallpcl(modules[i], pass)
	od
end

proc writepcl(pcl pcstart, pc, ref int32 pclsource, int pass, ichar sourcecode)=
!write pc instruction to ttdeststr, as a single line of pcl
!index is index of ins in pccode/pcdata
	[512]char str

	int cmdcode, a, soffset, moduleno, offset
	int attrs
	ref strec d
	const tabx="!      ----------"

!CPL "WRITEPCL",PCLNAMES[PC.opcode]

	cmdcode:=pc.opcode

	case cmdcode
	WHEN KSKIP THEN
		RETURN

	when kprocdef then
!	CPL "--PROCDEF", SYMBOL(PC^).NAME
		currpclproc:=pc.def
		gstr(tabx)
		gstr("Procdef:")
		gstr(currpclproc.name)
		gline()
		return
	when kprocend then
		gstr(tabx)
		gstrln("End")
		return
	esac

	offset:=pc-pcstart

	soffset:=(pclsource+offset)^
	currlineno:=soffset iand 16777215

	fprint @str, "# [#]: #: ", pc-1:"8zh", currlineno:"05jr", pc-pcstart-1:"4"
	gstr(&.str)

	if pc.haslabel then
		glabeldef(pc)
		gstr(&.str)
	fi

	case cmdcode
	when kprocdef then
		currpclproc:=pc.def
		return
	when kcomment then
		gstr("! ")
		gstrln(pc.svalue)
		return
	esac

	strcpy(&.str, pclnames[cmdcode]+1)

	a:=1
	gs_leftstr(pcldest, " ", 7, '-')
	gs_leftstr(pcldest, &.str, 10)
	gstr("     ")

	if pclopnd[cmdcode] then
		strcpy(str, writepclopnd(pcstart, pc, pass))
		gstr(str)
		gstr(" ")
	fi

	attrs:=pclattrs[cmdcode]
	if attrs<>'    ' then
		gstr("<")
		to 4 do
			case attrs.[0..7]
			when ' ' then
				exit
			when 'n', 'f', 'b' then
				gstrint(pc.n)
			when 'x' then
				gstrint(pc.x)
			when 'y' then
				gstrint(pc.y)
			when 'c' then
				gstr(condnames[pc.condcode])
			esac
			attrs>>:=8
			if attrs iand 255<>' ' then gstr(" ") fi

		od
		gstr(">")
	fi

	gline()
end

function writepclopnd(pcl pcstart, pc, int pass)ichar=
!f=o/p channel
!fmt=single operand code
!x is value of operand
!n is operand @ (1..4)
	static [512]char str, str2
	symbol d
	ichar suffix, s
	int slen
	object p
	ref stringrec ps

!IF PASS=3 THEN
!	RETURN "OPND"
!FI

	d:=symbol(pc.def)

	case pclopnd[pc.opcode]
	when cint then
		print @str, pc.value

	when creal then
		print @str, pc.xvalue

	when cstring then
		if pass<=2 then
			ps:=cast(pc.value)
			s:=ps.svalue
			slen:=ps.length
			goto dostring
		else
RETURN "<STR>"
!			p:=cast(x)
!			if (slen:=p.length)=0 then return """" fi
!			s:=p.strptr
!			goto dostring
		fi

	when cstringz then

		s:=pc.svalue
		slen:=strlen(s)
	dostring:
		if slen>=255 then slen:=255 fi
		memcpy(&.str, s, slen)			!truncate too-long strings
		str[slen+1]:=0
		convertstring(&.str, &.str2)
		fprint @str, """#""", &.str2

	when cvar then
		if pass<=2 then
			strcpy(str, d.name)
		else
RETURN "MEM"
!			d:=nil
!			for i to nstatics do
!				if statictable[i]=variant(x) then
!					d:=staticdefs[i]
!					exit
!				fi
!			od
!
!			fprint @str, "[#] (#:#)", x:"h", (d|d.owner.name|"?"), (d|d.name|"?")
		fi

	when csymbol then
		fprint @str, "[#]", d.name

	when cproc then
!		if pass<=2 then
			strcpy(str, d.name)
!		else
!CPL "CPROC", D
!RETURN "PROC"
!			d:=nil
!			for i to nprocs do
!				if proctable[i]=ref int(x) then
!					d:=procdefs[i]
!					exit
!				fi
!			od
!
!			fprint @str, "[#] (#)", x:"h", (d|d.name|"?")
!		fi

	when cdllproc then
!		fprint @str, "[DLL:#]", getdottedname(d)
		fprint @str, "[DLL:#]", d.name

	when chost then
		print @str, pc.hostindex, hostfnnames[pc.hostindex]+2

	when cgenfield then
		if pass<=2 then
			fprint @str, ".#", d.name
		else
!RETURN "GENFIELD"
			fprint @str, "## (#)", "#", pc.value, genfieldtable[pc.value].def.name
		fi
!
	when ctype then
		fprint @str, "T:# <#>", strmode(pc.typecode), pc.typecode

	when clabel then

!		fprint @str, "L#", pc.labelref-pcstart+1
		fprint @str, "L#", pc.labelno

	when coperator then
		fprint @str, "(#)", pclnames[pc.bintocode]

	else
	other:
		fprint @str, "<#>", opndnames[pclopnd[pc.opcode]]
	esac
	return str
end

global proc writeallpcl(ifile pm, int pass)=
!display code currently in pccode/pcopnd
	int cmd
	pcl pc, pclcode
	ref int32 pclsource
	ichar sourcecode

	currlineno:=0
CPL "WRITEALLPCL"


	if pass=3 and not hasbytecodes then
		gstrln("Can't show PCL; use -debug")
		return
	fi

	gstr("PCL FOR MODULE:")
	gstrln(pm.name)

	pc:=pclcode:=pm.pcstart
	pclsource:=pm.pcsourcestart
	sourcecode:=pm.text

	repeat
		cmd:=pc.opcode

		writepcl(pclcode, pc, pclsource, pass, sourcecode)
		++pc
	until cmd=kendprog

	gline()

end

global proc gstr(ichar s)=
	gs_str(pcldest,s)
end

global proc gstrln(ichar s)=
	gs_strln(pcldest,s)
end

global proc gline=
	gs_line(pcldest)
end

global proc gstrint(int a)=
	gs_strint(pcldest,a)
end

global proc glabeldef(pcl pc)=
!GSTRLN("LABELDEF")

!	gstr("L")
!	gstrint(pc-pcstart+1)
!	gstrln(": ")

	int lab:=0
	for i to nextlabelno do
		if pc=labelpctable[i] then lab:=i fi
	end

	gstr("L")
	gstrint(lab)
	gstrln(": ")

end

=== END ===
1 bb.m
2 qq_cli.m
3 qq_decls.m
4 qq_lex.m
5 qq_lib.m
6 qq_modules.m
7 qq_names.m
8 qq_parse.m
9 qq_pcltabs.m
10 qq_pclgen.m
11 qq_pcllib.m
12 qq_resolve.m
13 qq_syslibsdummy.m
14 qq_tables.m
15 qq_show.m
16 qq_showpcl.m
