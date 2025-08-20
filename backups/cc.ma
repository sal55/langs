=== MA 21 ===
=== cc.m 0 0 1/21 ===
project =
    module cc_cli

!Global Data and Tables

	MODULE DUMMY

    module cc_decls
    module cc_tables

!Lexing and Parsing
    module cc_lex
    module cc_parse

!Generate PCL
    module cc_gentcl
    module cc_blocktcl
    module cc_libtcl

!General

    module cc_lib
    module cc_support

!Bundled headers

!    module cc_headers
   module cc_headersx

!Diagnostics
    module cc_show
!    module cc_showdummy

!IL Backend
!    $sourcepath "c:/bx2/"
    $sourcepath "c:/bx/"
	module tc_api
	module tc_decls
	module tc_diags
	module tc_tables

!    import pcl
!   import pclint
end
=== cc_cli.m 0 0 2/21 ===
enumdata []ichar passnames, []ichar extnames =
	(load_pass,		$,		""),
	(pp_pass,		$,		"i"),
	(parse_pass,	$,		""),
	(type_pass,		$,		""),
	(tcl_pass,		$,		"tcl"),
	(runtcl_pass,	$,		"(int)"),
	(mcl_pass,		$,		"asm"),
	(nasm_pass,		$,		"nasm"),
	(asm_pass,		$,		"asm"),
	(mx_pass,		$,		"mx"),
	(obj_pass,		$,		"obj"),
	(dll_pass,		$,		"dll"),
	(exe_pass,		$,		"exe"),
	(run_pass,		$,		"(run)"),
end

byte cc_pass			!one of the above, default is link_pass
byte debugmode
ichar outfile			!base file
ichar outext="exe"
int cmdskip
int ttt

global byte fverbose=1			!0/1/2 = quiet/normal/extra
global byte fshowincludes=0

global byte dointheaders=1				!allow internal std headers
global byte highmem=1					!0/1/2 = normal/rip only/himem

byte fshowst
byte fshowstflat
byte fshowast
byte fshowtcl
byte fshowpst
byte fshowmcl
byte fshowss
byte fshowtypes
byte fshowfiles
byte fshowpaths
byte fshowheaders
byte fwriteheaders
byte fshowlog
byte fshowtiming
byte fgendll
byte fstdout
byte fshortnames
global byte fwriteerrors=1			!whether to writer $errors.tmp


ichar entrypointname

enumdata []ichar optionnames, []ref byte optvars, []byte optvalues =
	(load_sw,		"load",			&cc_pass,		load_pass),
	(pp_sw,			"e",			&cc_pass,		pp_pass),
	(ppi_sw,		"ei",			&cc_pass,		pp_pass),
	(parse_sw,		"parse",		&cc_pass,		parse_pass),
	(type_sw,		"type",			&cc_pass,		type_pass),
	(tcl_sw,		"p",			&cc_pass,		tcl_pass),
	(tcli_sw,		"pi",			&cc_pass,		tcl_pass),
	(runtcl_sw,		"i",			&cc_pass,		runtcl_pass),
	(mcl_sw,		"mcl",			&cc_pass,		mcl_pass),
	(asm_sw,		"s",			&cc_pass,		asm_pass),
	(asm2_sw,		"a",			&cc_pass,		asm_pass),
	(nasm_sw,		"nasm",			&cc_pass,		nasm_pass),
	(obj_sw,		"c",			&cc_pass,		obj_pass),
	(obj2_sw,		"obj",			&cc_pass,		obj_pass),
	(mx_sw,			"mx",			&cc_pass,		mx_pass),
	(dll_sw,		"dll",			&cc_pass,		dll_pass),
	(exe_sw,		"exe",			&cc_pass,		exe_pass),
	(run_sw,		"r",			&cc_pass,		run_pass),

!	(asm2_sw,		"asm",			&cc_pass,		asm_pass),

	(noregs_sw,		"noregs",		&fregoptim,		0),
	(nopeep_sw,		"nopeep",		&fpeephole,		0),
	(noopt_sw,		"no",			nil,			0),
!	(opt2_sw,		"o2",			&foptimise,		2),
!	(opt3_sw,		"o3",			&foptimise,		3),

	(paths_sw,		"paths",		&fshowpaths,	1),
	(headers_sw,	"headers",		&fshowheaders,	1),

	(inclpath_sw,	"incl",			nil,			1),
	(showincl_sw,	"includes",		&fshowincludes,	1),
!	(mh1_sw,		"mheaders",		&fmheaders,		'M'),
!	(mh2_sw,		"qheaders",		&fmheaders,		'Q'),

	(showst_sw,		"showst",		&fshowst,		1),
	(showstflat_sw,	"showstflat",	&fshowstflat,	1),
	(showast_sw,	"showast",		&fshowast,		1),
	(showtcl_sw,	"showtcl",		&fshowtcl,		1),
	(showpst_sw,	"showpst",		&fshowpst,		1),
	(showmcl_sw,	"showmcl",		&fshowmcl,		1),
	(showss_sw,		"showss",		&fshowss,		1),
	(showtypes_sw,	"showtypes",	&fshowtypes,	1),
	(showfiles_sw,	"showfiles",	&fshowfiles,	1),

	(time_sw,		"time",			&fshowtiming,	1),
	(time2_sw,		"time2",		&fshowtiming,	2),
	(v_sw,			"v",			&fverbose,		2),
	(vv_sw,			"vv",			&fverbose,		3),
	(quiet_sw,		"q",			&fverbose,		0),
	(csize_sw,		"cs",			&pverbose,		1),
	(size_sw,		"ss",			&pverbose,		2),
	(help_sw,		"h",			nil,			0),
	(help2_sw,		"help",			nil,			0),
	(ext_sw,		"ext",			&dointheaders,	0),
	(writeheaders_sw,"writeheaders",&fwriteheaders,	1),
	(out_sw,		"o",			nil,			0),
	(stdout_sw,		"stdout",		&fstdout,		1),
	(shortnames_sw,	"shortnames",	&fshortnames,	1),

	(norip_sw,		"norip",		&highmem,		0),
	(himem_sw,		"himem",		&highmem,		2),
end

const logfile="mcc.log"

int totallines=0
int nstringobjects=0

![sysparams.len]ichar extraparams	!after ":"
![sysparams.len]ichar extravalues
!int nextraparams=0

int startclock, loadtime, parsetime, tcltime, compiletime
int inittime

proc main=
	ichar file

	startclock:=os_clock()
PSTARTCLOCK:=STARTCLOCK
	
	starttiming()
	initdata()

	getinputoptions()

	initsearchdirs()

	if fverbose=3 then showsearchdirs() fi

	initlogfile()
	inittime:=gettiming()

	if fverbose then
		fprintln "Compiling # to #", inputfile, outfile
	fi

$PMODULENAME:=PCM_COPYHEAPSTRING(EXTRACTBASEFILE(INPUTFILE))

	do_loadmodule()

	do_preprocess()

	do_parsemodule()

	do_gentcl()

	case cc_pass
	when runtcl_pass then
!*!		tcl_runtcl()

	when mcl_pass then
!*!		do_genmcl()

!	when nasm_pass then
	when asm_pass then
!*!		do_asm()

	when obj_pass then
		do_obj()
!*!		

	when dll_pass then
!*!		do_dll()

	when exe_pass then
!*!		do_exe()

	when mx_pass then
!*!		do_mx()

	when run_pass then
!*!		do_run()

	else
		if cc_pass>=mcl_pass then
			println passnames[cc_pass],"not ready"
		fi
	esac

	if fverbose>=2 then
		println "Done."
	fi

	if fshowtiming then
		showtiming()
	fi

	closelogfile()

	stop 0
end

proc do_preprocess=
	if cc_pass=pp_pass then
		lex_preprocess_only(inputfile, outfile, fstdout)
		stop 0
	fi
end

proc do_loadmodule=
!Used for main module. Will always be first module loaded, module list
!will be empty.
!Load file as string
!extract modulename
!call compilemodile(modulename, filename, source)
	ichar modulename
	[300]char path
	int status
	int i,flag

	if fverbose=3 then
		CPL "Loading:",inputfile
	fi
	starttiming()

!set up special module to represent the whole program
	sourcefilenames[0]:="<dummy file>"
	sourcefilepaths[0]:="<dummy path>"
	sourcefiletext[0]:="<sourcefile0>"
	sourcefilesizes[0]:=strlen(sourcefiletext[0])

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)

	if not checkfile(inputfile) then					!don't use searchdirs
		loaderror("Can't load main module: #",inputfile)
	fi

	mainfileno:=loadsourcefile(inputfile,inputfile)
		
	modulename:=extractbasefile(inputfile)
	stmodule:=createdupldef(stprogram, addnamestr(modulename), moduleid)

	strcpy(&.path,extractpath(inputfile))
	if path[1] then
		++nsearchdirs
		for i:=nsearchdirs downto 2 do
			searchdirs[i]:=searchdirs[i-1]
		od
		searchdirs[1]:=ref char(pcm_copyheapstring(&.path))
	fi

	loadtime:=gettiming()
end

proc do_parsemodule=
	int tt
	starttiming()
	parsemodule()
	parsetime:=(tt:=gettiming())
!	if fshowtiming=2 then fprintln "[parse #:#]",inputfiles[m]:"15jl",tt fi

!	showast()
end

proc do_gentcl=
	return unless cc_pass >= tcl_pass

	starttiming()
	codegen_tcl()
	tcltime:=gettiming()

!*!	tcl_reducetest() when fregoptim or fpeephole

!	if fshowtcl then
!		println @logdev, tcl_writetcl(nil)
!
	if fshowtcl or cc_pass=tcl_pass then			!need discrete file
!	if cc_pass=tcl_pass then			!need discrete file
CPL "WRITETCL1"
		tcl_writetcl(outfile)
	fi

	if fshowpst then
		tcl_writepst("PSYMTAB")
	fi

end

proc do_genmcl=
	return unless cc_pass >= mcl_pass

	if cc_pass=mcl_pass then			!need discrete file
!*!		tcl_writeasm(outfile)
	fi
end

proc do_asm=
	return unless cc_pass >= asm_pass
!*!	tcl_writeasm(outfile)
end

proc do_obj=
	return unless cc_pass = obj_pass
!*!	tcl_writeobj(outfile)
end

proc do_dll=
	return unless cc_pass = dll_pass
!*!	tcl_writedll(outfile)
end

proc do_exe=
	return unless cc_pass = exe_pass
!*!	tcl_writeexe(outfile)
end

proc do_mx=
	return unless cc_pass = mx_pass
!*!	tcl_writemx(outfile)
end

proc do_run=
	return unless cc_pass = run_pass
!*!	tcl_exec()
end

proc initlogfile=
	if debugmode>=2 then
		remove(logfile)
		logdev:=cast(fopen(logfile,"w"))
	fi
end

proc closelogfile=
	[100]char str
	int pos

	return unless debugmode>=2


	if fshowmcl and cc_pass>=mcl_pass then
		println @logdev, "PROC ASM"
!*!		println @logdev, tcl_writeasm(nil)
	fi

	if fshowtcl and cc_pass>=tcl_pass then
		addtolog(changeext(outfile, "tcl"), logdev)
	fi
	if fshowpst and cc_pass>=tcl_pass then
		addtolog("PSYMTAB", logdev)
	fi

	showast()

	if fshowst then
		showst("ST")
	fi

	if fshowstflat then
		showstflat("STFLAT")
	fi

	if fshowtypes then
		printmodelist(logdev)
	fi
!
	fclose(cast(logdev))

CPL "PRESS KEY"; STOP WHEN OS_GETCH()=27
	print @&.str,"\\m\\scripts\\med.bat ",logfile


	if checkfile("cc.m") then
		os_execwait(&.str,0,nil)
	else
		println "Diagnostic outputs written to",logfile
	fi
end

proc initdata=
	pcm_init()
	lexsetup()
	inittypetables()
	initcclib()

!init libfiles
	nlibfiles:=0
	libfiles[++nlibfiles]:="msvcrt"
	libfiles[++nlibfiles]:="gdi32"
	libfiles[++nlibfiles]:="user32"
	libfiles[++nlibfiles]:="kernel32"

!*!	igetmsourceinfo:=cast(cgetsourceinfo)
end

global func cgetsourceinfo(int pos, ichar &filename, &sourceline)int=
	filename:=sourcefilenames[pos.[24..32]]
	sourceline:="<line>"
	return pos.[0..23]
end

proc initsearchdirs=
	[300]char str1,str2
	int i

	searchdirs[++nsearchdirs]:=""

!	if dointheaders=0 then
		searchdirs[++nsearchdirs]:="c:/cx/headers/"
!	fi

	searchdirs[++nsearchdirs]:=pcm_copyheapstring(extractpath(os_gethostname()))

	for i to nincludepaths when includepaths[i]^ do
		searchdirs[++nsearchdirs]:=includepaths[i]
	od


end

proc showsearchdirs=
	int i

	println "Include search paths:"
	if dointheaders then
		println "0: Internal standard headers (disable with -ext)"
	fi

	for i to nsearchdirs do
		if searchdirs[i]^ then
			println i,,":",searchdirs[i]
		else
			println i,,": ."
		fi
	od
	println
end

proc showast=

	if fshowast then
		printcode(logdev,"PROC AST")
		println @logdev
	fi
end

proc showstflat(ichar caption)=
	println @logdev,"PROC",caption
	printstflat(logdev)
	println @logdev
end

proc showst(ichar caption)=
	println @logdev,"PROC",caption
	printst(logdev, stmodule)
	println @logdev
end

proc showfiles=
	println "Sourcefiles:"

	for i:=1 to nsourcefiles do
		cpl i,":",sourcefilepaths[i],sourcefilenames[i],"Size:",sourcefilesizes[i]
	od
	println
end

proc showtime(ichar caption, int t)=
	fprintln "# # ms # %", caption:"12jl", t:"5", (t*100.0)/compiletime:"5.1jr"
end

proc showtiming=

	compiletime:=os_clock()-startclock

	showtime("Init:",		inittime)
	showtime("Load:",		loadtime)
	showtime("Parse:",		parsetime)
	showtime("PCL:",		tcltime)
	showtime("MCL:",		mcltime)
	showtime("SS:",			sstime)
	showtime("EXE:",		exetime)
	println "-----------------------------"
	showtime("Total:",		compiletime)
end

proc getinputoptions=
	const slash='-'
	int i,j,k
	int paramno,pmtype,sw,ncolons
	ichar name,value,ext

	paramno:=1
	ncolons:=0

!	if pc_useruntcl then
!		cc_pass:=runtcl_pass
!		fverbose:=0
!	fi

	if eqstring(extractfile(os_gethostname()),"cs.exe") then
!		msfile:=1
		fverbose:=0
		do_option(run_sw, "")
	fi

	do
		pmtype:=nextcmdparamnew(paramno,name,value,".c")
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
			if inputfile then
				loaderror("One input file only")
			fi
			inputfile:=pcm_copyheapstring(name)

			if cc_pass in [run_pass, runtcl_pass] then
				cmdskip:=paramno-1+$cmdskip
				pci_target:=cc_pass=runtcl_pass
				exit
			fi
		when pm_libfile then
			if nlibfiles>=maxlibfile then
				loaderror("Too many lib files")
			fi
			libfiles[++nlibfiles]:=pcm_copyheapstring(name)
!		when pm_colon then
!			if ++ncolons>1 then
!				name:=":"
!				value:=nil
!		!		recase pm_extra
!				goto doextra
!			fi
		when 0 then
			exit
		esac
	od

!CPL INPUTFILES[1]

	if cc_pass=0 then cc_pass:=exe_pass fi
	if cc_pass in [dll_pass,obj_pass, nasm_pass] then
		highmem:=2
	elsif cc_pass in [mx_pass,  run_pass] then
		highmem:=0
	fi
	outext:=extnames[cc_pass]

	if inputfile=nil and not fwriteheaders then
		showcaption()
		println "Usage:"
		println "    ",,cmdparams[0],"   prog[.c]          Compile prog.c to prog.exe"
		println "    ",,cmdparams[0],"-r prog[.c]          Compile prog.c and run"
		println "    ",,cmdparams[0],"-i prog[.c]          Compile prog.c and interpret"
		println "    ",,cmdparams[0],"-help                Show all options"
		stop 1
	fi

	if fwriteheaders then
		writeheaders()
		stop 20
	fi

	if outfile=nil then
		outfile:=pcm_copyheapstring(changeext(inputfile, outext))
	fi

!*!	tcl_setflags(highmem:highmem, shortnames:fshortnames)
!*!	tcl_cmdskip(cmdskip)
end

proc do_option(int sw, ichar value)=
	[300]char str
	int length
	ref byte p

	p:=optvars[sw]
	if p then
		p^:=optvalues[sw]
!
		if sw in load_sw..mcl_sw then
			debugmode ior:=1
		fi

		if sw in showst_sw..showfiles_sw then
			debugmode ior:=2
		fi

		if sw=runtcl_sw and inputfile then
			loaderror("-RUNP OUT OF ORDER")
		fi
		if sw in [ppi_sw, tcli_sw] then
			pci_target:=1
		fi

!		if sw in exe_sw..ml_sw then
!			outext:=optionnames[sw]
!		fi
		return
	fi

	case sw
	when inclpath_sw then
		if nincludepaths>maxincludepaths then
			loaderror("Too many include paths","")
		fi
		length:=strlen(value)
		case (value+length-1)^
		when '\\', '/' then
		else
			strcpy(&.str,value)
			strcat(&.str,"/")
			value:=&.str
		esac

		includepaths[++nincludepaths]:=pcm_copyheapstring(value)

	when help_sw,help2_sw then	showhelp()

	when out_sw then
		outfile:=pcm_copyheapstring(addext(value,outext))

	when noopt_sw then
		fregoptim:=fpeephole:=0

	esac
end

proc showincludepaths=
	println "Include paths",nincludepaths
	for i to nincludepaths do
		println i,includepaths[i]
	od
	println
end

proc showhelp=
	showcaption()
	println strinclude "cc_help.txt"

	stop 23
end

proc showextrainfo=
	static ichar infotext=strinclude "info.txt"

	println infotext

	stop 24
end

proc showcaption=
	println "CC C Compiler",$date,$time
end

proc starttiming =
	ttt:=os_clock()
end

func gettiming:int=
	os_clock()-ttt
end
=== dummy.m 0 0 3/21 ===
!global int sstime
!global int mcltime
!global int exetime

global proc genmcl()=end

global func getassemstr:ref strbuffer=nil end

global proc genss(int obj=0)=end

global proc writeexe(ichar outfile, int dodll, ichar entrypoint=nil)=end

export function writessdata(int fexe)ref strbuffer= nil end

export const ctarget=0

global const maxtuplesize = 4
=== cc_decls.m 0 0 4/21 ===
import clib
global type unit = ref unitrec
global type symbol = ref strec

global const maxmodule=200
global const maxlibfile=200
global const maxsourcefile=200

global macro pr(a,b) = a<<16+b

global record tokenrec = 		!should be 32-byte record
	union
		i64 value				!64-bit int
		real xvalue				!64-bit float
		u64 uvalue			!64-bit word
		ref char svalue			!pointer to string or charconst (not terminated)
		ref strec symptr		!pointer to symbol table entry for name
	end
	ref tokenrec nexttoken

	u32	lineno
	u8	fileno
	u8	symbol
	u8	subcode
	u8	flags

	i32 length					!length of name/string/char
	union
		i32 numberoffset			!offset of numeric token within file[fileno]
		i16 paramno				!for macro params
		i16 pasteno
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
	i32 mode				!tnone when there are no normal params
	i16 nparams			!used on first param only
	i16 flags				!used on first param only
end

!mask bits for .flags of tokenrec; these can be combined if both are true
global const tk_macromask = 1		!is a name that is a macro def
global const tk_parammask = 2		!is a name that is a param def
global const tk_macrolit  = 4		!is an processed token that is a macro name
global const tk_pasted    = 8

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
	psymbol pdef

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
		i32 index					!enum/label index
		i32 offset
		i32 labelno				!normally used as .index
		byte oldsymbol				!for #define/#undef on keyword
	end

	u32 lineno
	union
		struct
			u16 blockno
			u16 namespace				!experimental: set to namespaces[.nameid]
		end
		u32 nsblock						!combined block no and namespace
	end

	i16 subcode
	u16 mode

	u16 nrefs
	byte namelen
	byte symbol

!	byte flags:(addrof:1, varparams:1, flmacro:1, used:1, ismain:1)
	byte flags:(addrof:1, varparams:1, flmacro:1, ismain:1)
	byte nameid
	byte scope					!linkage type
	byte nparams				!no. formal params for procid/dllprocid

	byte align					!1, 2, 4, 8; max align for struct/union
	byte fileno

	psymbol pdata				!symbol such as $mod.func.name.1 for makelist data
	byte used
	[5]byte spare
end

global record fwdrec =
	ref fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

global record unitrec =
	union
		ref strec def
		i64 value
		u64 uvalue
		real xvalue
		ichar svalue
		ref u16 wsvalue
		ref strec labeldef
		ref caserec nextcase
		i32 ptrscale			!use for derefoffset/addoffset
		i32 offset				!for jdot
	end
	ref unitrec nextunit
	ref unitrec a	!single items, or linked lists
	ref unitrec b
	ref unitrec c

	i32 tag			!kcode tag number
	u32 lineno			!source lineno associated with item; fileno is in top byte

	union
		i32 opcode			!for conversion
		i32 index				!label index
		u32 uindex			!case index
		i32 slength			!for const/string
		i32 wslength
		i32 alength			!for deref units, length of any array before conversion to ptr
		i32 scale			!for scale unit (negative means divide)
		i32 aparams			!callfn/proc: no. actual params
		i32 count			!makelist, no. items
	end

	i32 mode
	i16 memmode				!none, or memmode for name, ptr etc
	i16 convmode				!conversion dest type
	byte fileno
	byte isstrconst			!for string consts: initialised with "..."
	byte iswstrconst
	byte spare1
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
!global const int maxtype=20'000
!global const int maxtype=60'000
global const int maxtype=80'000

global int ntypes

global [0:maxtype]ref strec	ttnamedef
global [0:maxtype]i16	ttbasetype			!basic t-code
global [0:maxtype]int	ttlength			!0, or array length
global [0:maxtype]byte	ttconst				!1 when const
global [0:maxtype]i16	tttarget			!pointer target or array elem type
global [0:maxtype]i16	ttreftype			!0, or index of type that is a pointer to this one
global [0:maxtype]i16	ttconsttype			!0, or index of type that is a const version of this oneointer to this onee
global [0:maxtype]int	ttsize				!byte size
global [0:maxtype]byte	ttisref
!global [0:maxtype]byte	ttcat
global [0:maxtype]byte	ttisblock
global [0:maxtype]byte	ttsigned			!set up in genmcl
global [0:maxtype]i32	ttshared			!no. of shared instances
global [0:maxtype]ref paramrec ttparams		!for modes involving function pointers
global [0:maxtype]ref strec tttypedef

global int trefchar							!set to to char* type
global int trefwchar						!set to to wchar* type

global ichar inputfile
global int mainfileno
global [0..maxlibfile]ichar libfiles
global [0..maxsourcefile]ichar sourcefilenames
global [0..maxsourcefile]ichar sourcefilepaths
global [0..maxsourcefile]ichar sourcefiletext
global [0..maxsourcefile]i32 sourcefilesizes

global int nsourcefiles
global int nlibfiles

global const maxsearchdirs=20
global const maxincludepaths=20

global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0
global [maxincludepaths]ichar includepaths
global int nincludepaths=0

global ref strec stprogram		!root into the symbol table
global ref strec stmodule		!main module

global filehandle logdev		!dest for diagnostics and output of tables


global const sourceext="c"
global ref unitrec nullunit

global int clineno=0		!set in codegen scanner
global int cfileno=0		!set in codegen scanner

global tokenrec lx				!provides access to current token data
global tokenrec nextlx

!global int tlex=0		!timing

global int debug=0

!global int hstsize	= 16384
global int hstsize	= 65536

global int hstmask				!filled in with hstsize-1

global ref[0:]ref strec hashtable

global const maxblock=2100,maxblockstack=100
global [0..maxblock]i32 blockowner
global [0..maxblock]i32 blockcounts
global [0..maxblockstack]i32 blockstack
global int currblockno,nextblockno,blocklevel
global ref strec currproc

global const maxnestedloops=64

global ichar dheaderfile=nil			!result of -d:file.h switch

global int structpadding=1
global int callbackflag=0

global int slineno,sfileno

global ichar oemname="MCC"

global ichar mclstr
global int mclstrlen

global int nunits

global const maxpmodule = maxmodule-1
global const maxpheader = 100
global const maxplib = 100

global [maxpmodule]ichar pmodulelist
global [maxpheader]ichar pheaderlist
global [maxplib]ichar pliblist

global int npmodules
global int npheaders
global int nplibs

global byte pci_target			!1 when using -runp

!GLOBAL INT NALLCALLS
!GLOBAL INT NUSESTACK
!GLOBAL INT NUSEMIXEDSTACK
=== cc_tables.m 0 0 5/21 ===
global enumdata [0:]ichar stdtypenames, [0:]byte stdtypewidths,
		 [0:]byte stdsigned, [0:]byte stdtcl, [0:]byte stdsize =

!                              bts  si  pcl      size
	(tvoid=0,		"void",		0,	0,	tpvoid,		0),

	(ti8,			"i8",		8,	1,	tpi8,		1),		! This ordering is important
	(ti16,			"i16",		16,	1,	tpi16,		2),		!
	(ti32,			"i32",		32,	1,	tpi32,		4),		!
	(ti64,			"i64",		64,	1,	tpi64,		8),		!

	(tbool,			"bool",		8,	0,	tpu8,		1),		! As is this
	(tu8,			"u8",		8,	0,	tpu8,		1),		!
	(tu16,			"u16",		16,	0,	tpu16,		2),		!
	(tu32,			"u32",		32,	0,	tpu32,		4),		!
	(tu64,			"u64",		64,	0,	tpu64,		8),		!

	(tr32,			"r32",		32,	0,	tpr32,		4),		! And tr32 must be >= integer types
	(tr64,			"r64",		64,	0,	tpr64,		8),		!

	(tenum,			"enum",		0,	0,	tpi32,		0),		!
	(tref,			"ref",		64,	0,	tpu64,		0),		! 
	(tproc,			"proc",		64,	0,	tpvoid,		0),		!
	(tlabel,		"label",	64,	0,	tpvoid,		0),		!
	(tblock,		"block",	0,	0,	tpblock,	0),		!

	(tarray,		"array",	0,	0,	tpblock,	0),		!
	(tstruct,		"struct",	0,	0,	tpblock,	0),		!
	(tunion,		"union",	0,	0,	tpblock,	0),		!

	(tnotset,		"notset",	0,	0,	tpvoid,		0),		!

!User-defined types go here
	(tlast,			$,			0,	0,	tpvoid,		0)		! 	!

end

global const tchar=ti8
global const tfirstnum=ti8, tlastnum=tr64
global const tfirstint=ti8, tlastint=tu64
global const tfirstreal=tr32, tlastreal=tr64

global const tptroffset = ti64		!for 64-bit target

global enumdata [0:]ichar catnames =
    (voidcat=0,     $),         ! Not set

    (intcat,        $),         ! i32 i64 u32 u64
    (realcat,       $),         ! r32 r64
    (shortcat,      $),         ! i8 i16 u8 u16
    (blockcat,      $),         ! u64 pointer to block data
end


global enumdata []ichar typespecnames, []i32 typespectypes, []byte typespecsizes =
	(ts_void,		$,	tvoid,		0),
!	(ts_char,		$,	tu8,		1),
	(ts_char,		$,	ti8,		1),
	(ts_short,		$,	0,			2),
	(ts_long,		$,	0,			4),
	(ts_int,		$,	ti32,		4),
	(ts_float,		$,	tr32,		4),
	(ts_double,		$,	tr64,		8),
	(ts_signed,		$,	0,			0),
	(ts_unsigned,	$,	0,			0),
	(ts_bool,		$,	tbool,		1),
	(ts_user,		$,	0,			0),
	(ts_struct,		$,	0,			0),
	(ts_union,		$,	0,			0),
	(ts_enum,		$,	0,			4),
	(ts_atomic,		$,	0,			0)
end

global enumdata [0:]ichar pmflagnames=
	(pm_normal=0,		$),		! Normal param
	(pm_notset,			$),		! ()     (applied to one dummy tnone param)
	(pm_empty,			$),		! (void) (applied to one dummy tnone param)
	(pm_variadic,		$)		! (...) or (t,u,v,...) (applied to dummy or first param)
end

!scope here refers to linkage across modules
global enumdata [0:]ichar scopenames=
	(no_scope=0,		"-"),		! 
	(function_scope,	"Fn"),		!within a function (note import/exported names can be declared in a block scope)
	(local_scope,		"Loc"),		!file-scope/not exported 
	(imported_scope,	"Imp"),		!imported from another module
	(exported_scope,	"Exp")		!file-scope/exported
end

!Call conventions
global enumdata []ichar cccnames=

	(open_cc=0,		$), ! Not set: either own or clang, depending on whether fn was defined
	(own_cc,		$), ! Internal (x86/x64)
	(clang_cc,		$), ! External (all x64; clang only x86)
	(stdcall_cc,	$), ! (x86 non-clang)
	(callback_cc,	$), ! Internal when called from External

	(dummy_cc,		$)	! 
end

global enumdata [0:]ichar linkagenames=
	(none_ss=0,		$),
	(static_ss,		$),
	(auto_ss,		$),
	(register_ss,	$),
	(extern_ss,		$),
	(typedef_ss,	$)
end

global enumdata []ichar typequalnames=
	(const_qual,	$),
	(volatile_qual,	$),
	(restrict_qual,	$),
	(atomic_qual,	$)
end

global enumdata []ichar fnspecnames=
	(inline_fnspec,		$),
	(noreturn_fnspec,	$),
	(callback_fnspec,	$),
end

global enumdata =
	pdm_date,
	pdm_time,
	pdm_file,
	pdm_line,
	pdm_func,
	pdm_cdecl,
	pdm_mcc,
	pdm_mcci,
	pdm_stdc
end

global enumdata [0:]ichar jtagnames=

	(jnone=0,		$), !
	(jconst,		$), !
	(jnull,		$), !
	(jname,		$), !
!	(jnameaddr,	$), !
	(jwidenmem,	$), !
	(jfuncname,	$), !
	(jblock,		$), !
	(jtempdecl,	$), !
	(jdecl,		$), !
!	(jtypeof,		$), !
!	(jmakeref,		$), !

!Statements

	(jreturn,		$), ! 
	(jreturnx,		$), ! 

	(jassign,		$), ! 
	(jif,			$), ! 
	(jfor,			$), ! 
	(jwhile,		$), ! 
	(jdowhile,		$), ! 
	(jgoto,		$), ! 
	(jlabelstmt,	$), ! 
	(jcasestmt,	$), ! 
	(jdefaultstmt,	$), ! 
	(jbreak,		$), ! [
	(jcontinue,	$), ! [
	(jswitch,		$), ! 
	(jbreaksw,		$), ! [
!	(jeval,		$), ! 

!Expressions and Operators

!Logical Operators

	(jandl,		"&& andl"), ! 
	(jorl,			"|| orl"), ! 
	(jnotl,		"! notl"), ! 
	(jistruel,		$), ! 

!Expressions and Operators

	(jmakelist,	$), ! 
	(jexprlist,	$), ! 

!	(jassignx,		$), ! 
	(jcallfn,		$), ! 
	(jifx,			$), ! 

!Binary Ops

	(jandand,		"&&"), ! a 

	(jeq,			"=="), ! a 
	(jne,			"!="), ! a 
	(jlt,			"<"), ! a 
	(jle,			"<="), ! a 
	(jgt,			">"), ! a 
	(jge,			">="), ! a 

	(jadd,			"+ add"), ! 
	(jsub,			"- sub"), ! 
	(jmul,			"* mul"), ! 
	(jdiv,			"/ div"), ! 
	(jrem,			"% mod"), ! 
	(jiand,		"& iand"), ! 
	(jior,			"| ior"), ! 
	(jixor,		"^ ixor"), ! 
	(jshl,			"<<"), ! a 
	(jshr,			">>"), ! a 

	(jdot,			$), ! 
	(jidot,		$), ! 
!	(jdotref,		$), ! 
	(jindex,		$), ! 

	(jptr,			"ptr"), ! 
!	(jptroffset,	"ptroffset *"), ! 
	(jaddptr,		"addptr"), ! 
	(jsubptr,		"subptr"), ! 
	(jaddrof,		"addrof &"), ! 
	(jconvert,		$), ! 
	(jscale,		$), ! 

!Monadic Ops

	(jneg,			"- neg"), ! 
	(jabs,			"abs"), ! 
	(jinot,		"~ inot"), ! a

!In-place operators

	(jaddto,		"+="), ! a b	a+:=b
	(jsubto,		"-="), ! a b
	(jmulto,		"*="), ! a b
	(jdivto,		"/="), ! a b
	(jremto,		"%="), ! a b
	(jiandto,		"&="), ! a b
	(jiorto,		"|="), ! a b
	(jixorto,		"^="), ! a b
	(jshlto,		"<<="), ! a b
	(jshrto,		">>="), ! a b

	(jpreincr,		"++ preincr"), ! a	++a
	(jpredecr,		"-- preincr"), ! a	--a
	(jpostincr,	"++ postincr"), ! a	a++
	(jpostdecr,	"-- postdecr"), ! a	a--

	(jsetjmp,		"setjmp"),
	(jlongjmp,		"longjmp"),

	(jdummy,		$)
end

global enumdata []ichar symbolnames, []ichar shortsymbolnames, []byte symboltojtag=

!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$,	"",		0),			! Lex error
	(dotsym,			$,	".",	jdot),		! "."
	(idotsym,			$,	"->",	jidot),	! "->"
	(lexhashsym,		$,	"#",	0),			! "#" as first symbol on line
	(hashsym,			$,	"#",	0),			! "#" within macro def
	(lithashsym,		$,	"#",	0),			! "#" literal hash (not stringify op)
	(hashhashsym,		$,	"##",	0),			! "##" within macro def
	(commasym,			$,	",",	0),			! ","
	(semisym,			$,	";",	0),			! ";"
	(colonsym,			$,	":",	0),			! ":"
	(assignsym,			$,	"=",	jassign),	! =
	(assignsym2,		$,	":=",	jassign),	! =
	(lbracksym,			$,	"(",	0),			! (
	(rbracksym,			$,	")",	0),			! )
	(lsqsym,			$,	"[",	0),			!	 [
	(rsqsym,			$,	"]",	0),			! ]
	(lcurlysym,			$,	"{",	0),			! {
	(rcurlysym,			$,	"}",	0),			! }
	(questionsym,		$,	"?",	0),			! ?
	(curlsym,			$,	"~",	0),			! ~
	(ellipsissym,		$,	"...",	0),			! ...
	(backslashsym,		$,	"\\",	0),			! \
	(addsym,			$,	"+",	jadd),		! +
	(subsym,			$,	"-",	jsub),		!
	(mulsym,			$,	"*",	jmul),		!
	(divsym,			$,	"/",	jdiv),		!
	(remsym,			$,	"%",	jrem),		!
	(iorsym,			$,	"|",	jior),		!
	(iandsym,			$,	"&",	jiand),	!
	(ixorsym,			$,	"^",	jixor),	!
	(orlsym,			$,	"||",	jorl),		!
	(andlsym,			$,	"&&",	jandl),	!
	(shlsym,			$,	"<<",	jshl),		!
	(shrsym,			$,	">>",	jshr),		!
	(inotsym,			$,	"~",	jinot),	!
	(notlsym,			$,	"!",	jnotl),	!
	(incrsym,			$,	"++",	jpreincr),	!
	(decrsym,			$,	"--",	jpredecr),	!
	(abssym,			$,	"abs",	jabs),		!

	(eqsym,				$,	"==",	jeq),		!
	(nesym,				$,	"!=",	jne),		!
	(ltsym,				$,	"<",	jlt),		!
	(lesym,				$,	"<=",	jle),		!
	(gesym,				$,	">=",	jge),		!
	(gtsym,				$,	">",	jgt),		!

	(addtosym,			$,	"+=",	jaddto),	!
	(subtosym,			$,	"-=",	jsubto),	!
	(multosym,			$,	"*=",	jmulto),	!
	(divtosym,			$,	"/=",	jdivto),	!
	(remtosym,			$,	"%=",	jremto),	!
	(iortosym,			$,	"|=",	jiorto),	!
	(iandtosym,			$,	"&=",	jiandto),	!
	(ixortosym,			$,	"^=",	jixorto),	!
	(shltosym,			$,	"<<=",	jshlto),	!
	(shrtosym,			$,	">>=",	jshrto),	!

	(eolsym,			$,	"",		0),			!
	(eofsym,			$,	"",		0),			!
	(rawnumbersym,		$,	"n",	0),			!
	(intconstsym,		$,	"n",	0),			!
	(realconstsym,		$,	"n",	0),			!
	(charconstsym,		$,	"s",	0),			!
	(wcharconstsym,		$,	"s",	0),			!
	(stringconstsym,	$,	"s",	0),			!
	(wstringconstsym,	$,	"s",	0),			!
	(whitespacesym,		$,	"w",	0),			!
!	(placeholdersym,	$,	"<PH>",	0),			!
	(placeholdersym,	$,	"",	0),			!

!Second half are tokens that can be yielded after a name lookup:
	(namesym,			$,	"k",	0),			! identifier symbol
	(ksourcedirsym,		$,	"k",	0),			! 
	(predefmacrosym,	$,	"k",	0),			! __LINE__ etc

	(ktypespecsym,		$,	"k",	0),			! INT, SHORT
	(kifsym,			$,	"k",	0),			! IF
	(kelsesym,			$,	"k",	0),			! ELSE
	(kcasesym,			$,	"k",	0),			! CASE
	(kdefaultsym,		$,	"k",	0),			! DEFAULT
	(kforsym,			$,	"k",	0),			! FOR
	(kwhilesym,			$,	"k",	0),			! WHILE
	(kdosym,			$,	"k",	0),			! DO
	(kreturnsym,		$,	"k",	0),			! RETURN
	(kbreaksym,			$,	"k",	0),			! BREAK
	(kcontinuesym,		$,	"k",	0),			! CONTINUE
	(kgotosym,			$,	"k",	0),			! GO/GOTO
	(kswitchsym,		$,	"k",	0),			! SWITCH
	(kstructsym,		$,	"k",	0),			! STRUCT
	(kunionsym	,		$,	"k",	0),			! UNION
	(klinkagesym,		$,	"k",	0),			! STATIC etc
	(ktypequalsym,		$,	"k",	0),			! CONST etc
	(kstdtypesym,		$,	"k",	0),			! ui32_t etc
	(kfnspecsym,		$,	"k",	0),			! INLINE etc
	(kalignassym,		$,	"k",	0),			! _ALIGNAS
	(kenumsym,			$,	"k",	0),			! ENUM
!	(kcallconvsym,		$,	"k",	0),			! CLANG etc
	(ksizeofsym,		$,	"k",	0),			! SIZEOF
	(kdefinedsym,		$,	"k",	0),			! DEFINED
	(kgenericsym,		$,	"k",	0),			! _GENERIC
	(kalignofsym,		$,	"k",	0),			! _ALIGNOF
	(ksetjmpsym,		$,	"k",	0),			! SETJMP etc

	(kdummysym,			$,	"",		0)			!
end

global enumdata []ichar sourcedirnames =
	(definedir,		$),
	(emitdir,		$),
	(ifdir,			$),
	(elifdir,		$),
	(elsedir,		$),
	(endifdir,		$),
	(includedir,	$),
	(ifdefdir,		$),
	(ifndefdir,		$),
	(undefdir,		$),
	(errordir,		$),
	(messagedir,	$),
	(blankdir,		$),
	(linedir,		$),
!	(strincludedir,	$),
	(pragmadir,		$)
end

global enumdata [0:]ichar namespacenames=
	(ns_none=0,		$),			!not set
	(ns_general,	$),			!variables, functions, typedefs, enum names
	(ns_tags,		$),			!struct, union, enum tags
	(ns_labels,		$),			!label names
	(ns_fields,		$)			!field names
end

global enumdata [0:]ichar namenames, [0:]i32 namespaces, [0:]byte name2pid=
	(nullid=0,		$,		ns_none,		0),			!Not assigned, or keyword/macro defined by .symbol
	(macroid,		$,		ns_none,		0),			!
	(programid,		$,		ns_none,		0),			!Main root
	(moduleid,		$,		ns_none,		0),			!
	(extmoduleid,	$,		ns_none,		0),			!
	(typeid,		$,		ns_general,		0),			!Type name in type, proc or module
	(procid,		$,		ns_general,		proc_id),	!Proc/method/function/op name
	(staticid,		$,		ns_general,		static_id),	!Static in type or proc or module
	(frameid,		$,		ns_general,		local_id),	!Local var
	(paramid,		$,		ns_general,		param_id),	!Local param
	(fieldid,		$,		ns_fields,		0),			!Field of Record or Class
	(enumid,		$,		ns_general,		0),			!Enum name, part of enum type only
	(enumtagid,		$,		ns_tags,		0),			!
	(structtagid,	$,		ns_tags,		0),			!
	(labelid,		$,		ns_labels,		label_id)	!Label name in proc only
end

global tabledata []ichar stnames, []i32 stsymbols, []i32 stsubcodes=

	("if",			kifsym,			jif),
	("else",		kelsesym,		0),
	("case",		kcasesym,		0),
	("default",		kdefaultsym,	0),
	("for",			kforsym,		0),
	("do",			kdosym,			0),
	("while",		kwhilesym,		0),
	("return",		kreturnsym,		0),
	("break",		kbreaksym,		0),
	("continue",	kcontinuesym,	0),
	("goto",		kgotosym,		0),
	("switch",		kswitchsym,		0),

	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),

	("include",		ksourcedirsym,	includedir),
	("define",		ksourcedirsym,	definedir),
	("elif",		ksourcedirsym,	elifdir),
	("ifdef",		ksourcedirsym,	ifdefdir),
	("ifndef",		ksourcedirsym,	ifndefdir),
	("endif",		ksourcedirsym,	endifdir),
	("undef",		ksourcedirsym,	undefdir),
	("error",		ksourcedirsym,	errordir),
	("pragma",		ksourcedirsym,	pragmadir),
	("line",		ksourcedirsym,	linedir),

	("auto",		klinkagesym,		auto_ss),
	("register",	klinkagesym,		register_ss),
	("static",		klinkagesym,		static_ss),
	("extern",		klinkagesym,		extern_ss),
	("typedef",		klinkagesym,		typedef_ss),
	
	("const",		ktypequalsym,	const_qual),
	("volatile",	ktypequalsym,	volatile_qual),
	("restrict",	ktypequalsym,	restrict_qual),
	("_Atomic",		ktypequalsym,	atomic_qual),

	("inline",		kfnspecsym,		inline_fnspec),
	("_Noreturn",	kfnspecsym,		noreturn_fnspec),

	("_Alignas",	kalignassym,	0),

	("enum",		kenumsym,		0),

	("void",		ktypespecsym,	ts_void),
	("char",		ktypespecsym,	ts_char),
	("short",		ktypespecsym,	ts_short),
	("long",		ktypespecsym,	ts_long),
	("int",			ktypespecsym,	ts_int),
	("float",		ktypespecsym,	ts_float),
	("double",		ktypespecsym,	ts_double),
	("signed",		ktypespecsym,	ts_signed),
	("unsigned",	ktypespecsym,	ts_unsigned),

	("_Bool",		ktypespecsym,	ts_bool),

	("int8",		kstdtypesym,	ti8),
	("int16",		kstdtypesym,	ti16),
	("int32",		kstdtypesym,	ti32),
	("int64",		kstdtypesym,	ti64),

	("uint8",		kstdtypesym,	tu8),
	("uint16",		kstdtypesym,	tu16),
	("uint32",		kstdtypesym,	tu32),
	("uint64",		kstdtypesym,	tu64),

	("__DATE__",	predefmacrosym,	pdm_date),
	("__FILE__",	predefmacrosym,	pdm_file),
	("__LINE__",	predefmacrosym,	pdm_line),
!	("__STDC__",	predefmacrosym,	pdm_stdc),
	("__TIME__",	predefmacrosym,	pdm_time),
!	("__cdecl",		predefmacrosym,	pdm_cdecl),
	("__MCC__",		predefmacrosym,	pdm_mcc),
	("__MCCI__",	predefmacrosym,	pdm_mcci),
	("__func__",	predefmacrosym,	pdm_func),
	("__FUNCTION__",	predefmacrosym,	pdm_func),

!	("not",			notlsym,		0),
	("sizeof",		ksizeofsym,		0),
	("lengthof",	ksizeofsym,		1),
	("defined",		kdefinedsym,	0),
	("_Generic",	kgenericsym,	0),
	("_Alignof",	kalignofsym,	0),

	("$setjmp",		ksetjmpsym,		jsetjmp),
	("$longjmp",	ksetjmpsym,		jlongjmp),

	("$$dummy",		0,				0)
end

global enumdata [0:]ichar convnames =

	(no_conv=0,	$),
	(soft_c,	$),			!no conversion needed, just type change
	(hard_c,	$),			!explicit conversion, done as uwiden or narrow to match sizes

	(swiden_c,	$),			!widen with sign-extension	(1/2/4 to 4/8)
	(uwiden_c,	$),			!widen with zero-extension	(1/2/4 to 4/8)
	(sfloat_c,	$),			!signed int to float		(1/2/4/8 to 4/8)
	(ufloat_c,	$),			!unsigned int to float		(1/2/4/8 to 4/8)
	(sfix_c,	$),			!float to signed int		(4/8 to 1/2/4/8)
	(ufix_c,	$),			!float to unsigned int		(4/8 to 1/2/4/8)
	(fwiden_c,	$),			!float to wider float		(4 to 8)
	(fnarrow_c,	$),			!float to narrower float	(8 to 4)
	(narrow_c,	$),			!narrow without truncation	(8/4/2 to 4/2/1)
	(truncate_c,$),			!narrow and truncate		(8/4/2 to 4/2/1)
	(bool_c,	$)			!int to bool				(1/2/4/8 to 1)
end

!take two basic numeric types and determine which is more dominant
!zeros mean not supported (error, not both numbers etc)
!(table could have been 16x16 but means checking both basic types being in-range first)

!dominantmode[s,t] returns the dominant type of s and t, widened to int/uint as needed
global [0:32,0:32]byte dominantmode

!conversionops[s,t] gives conversion op to convert numeric types s to t
global [0:16,0:16]byte conversionops

!table used to set up dominanttable[]
!3rd entry is the more dominant of the first two (wided as needed to int/unsigned int)
global [][3]byte dominantsetuptable=(
	(ti8,	ti8,		ti32),
	(ti8,	ti16,	ti32),
	(ti8,	ti32,		ti32),
	(ti8,	ti64,	ti64),
	(ti8,	tbool,		ti32),
	(ti8,	tu8,		ti32),
	(ti8,	tu16,	ti32),
	(ti8,	tu32,		ti32),
	(ti8,	tu64,	ti64),
	(ti8,	tr32,		tr32),
	(ti8,	tr64,	tr64),
	(ti16,	ti8,		ti32),
	(ti16,	ti16,	ti32),
	(ti16,	ti32,		ti32),
	(ti16,	ti64,	ti64),
	(ti16,	tbool,		ti32),
	(ti16,	tu8,		ti32),
	(ti16,	tu16,	ti32),
	(ti16,	tu32,		ti32),
	(ti16,	tu64,	ti64),
	(ti16,	tr32,		tr32),
	(ti16,	tr64,	tr64),
	(ti32,		ti8,		ti32),
	(ti32,		ti16,	ti32),
	(ti32,		ti32,		ti32),
	(ti32,		ti64,	ti64),
	(ti32,		tbool,		ti32),
	(ti32,		tu8,		ti32),
	(ti32,		tu16,	ti32),
	(ti32,		tu32,		tu32),
	(ti32,		tu64,	ti64),
	(ti32,		tr32,		tr32),
	(ti32,		tr64,	tr64),
	(ti64,	ti8,		ti64),
	(ti64,	ti16,	ti64),
	(ti64,	ti32,		ti64),
	(ti64,	ti64,	ti64),
	(ti64,	tbool,		ti64),
	(ti64,	tu8,		ti64),
	(ti64,	tu16,	ti64),
	(ti64,	tu32,		ti64),
	(ti64,	tu64,	tu64),
	(ti64,	tr32,		tr32),
	(ti64,	tr64,	tr64),
	(tbool,		ti8,		ti32),
	(tbool,		ti16,	ti32),
	(tbool,		ti32,		ti32),
	(tbool,		ti64,	ti64),
	(tbool,		tbool,		tu32),
	(tbool,		tu8,		tu32),
	(tbool,		tu16,	tu32),
	(tbool,		tu32,		tu32),
	(tbool,		tu64,	tu64),
	(tbool,		tr32,		tr32),
	(tbool,		tr64,	tr64),
	(tu8,	ti8,		ti32),
	(tu8,	ti16,	ti32),
	(tu8,	ti32,		ti32),
	(tu8,	ti64,	ti64),
	(tu8,	tbool,		tvoid),
	(tu8,	tu8,		tu32),
	(tu8,	tu16,	tu32),
	(tu8,	tu32,		tu32),
	(tu8,	tu64,	tu64),
	(tu8,	tr32,		tr32),
	(tu8,	tr64,	tr64),
	(tu16,	ti8,		ti32),
	(tu16,	ti16,	ti32),
	(tu16,	ti32,		ti32),
	(tu16,	ti64,	ti64),
	(tu16,	tbool,		tu32),
	(tu16,	tu8,		tu32),
	(tu16,	tu16,	tu32),
	(tu16,	tu32,		tu32),
	(tu16,	tu64,	tu64),
	(tu16,	tr32,		tr32),
	(tu16,	tr64,	tr64),
	(tu32,		ti8,		ti32),
	(tu32,		ti16,	ti32),
	(tu32,		ti32,		tu32),
	(tu32,		ti64,	ti64),
	(tu32,		tbool,		tu32),
	(tu32,		tu8,		tu32),
	(tu32,		tu16,	tu32),
	(tu32,		tu32,		tu32),
	(tu32,		tu64,	tu64),
	(tu32,		tr32,		tr32),
	(tu32,		tr64,	tr64),
	(tu64,	ti8,		tu64),
	(tu64,	ti16,	tu64),
	(tu64,	ti32,		tu64),
	(tu64,	ti64,	tu64),
	(tu64,	tbool,		tu64),
	(tu64,	tu8,		tu64),
	(tu64,	tu16,	tu64),
	(tu64,	tu32,		tu64),
	(tu64,	tu64,	tu64),
	(tu64,	tr32,		tr32),
	(tu64,	tr64,	tr64),
	(tr32,	ti8,		tr64),
	(tr32,	ti16,	tr64),
	(tr32,	ti32,		tr64),
	(tr32,	ti64,	tr64),
	(tr32,	tbool,		tr64),
	(tr32,	tu8,		tr64),
	(tr32,	tu16,	tr64),
	(tr32,	tu32,		tr64),
	(tr32,	tu64,	tr64),
	(tr32,	tr32,		tr32),
	(tr32,	tr64,	tr64),
	(tr64,	ti8,		tr64),
	(tr64,	ti16,	tr64),
	(tr64,	ti32,		tr64),
	(tr64,	ti64,	tr64),
	(tr64,	tbool,		tr64),
	(tr64,	tu8,		tr64),
	(tr64,	tu16,	tr64),
	(tr64,	tu32,		tr64),
	(tr64,	tu64,	tr64),
	(tr64,	tr32,		tr64),
	(tr64,	tr64,	tr64),
)

!table used to set up conversionops
global [][3]byte convsetuptable=(
	(ti8,	ti8,	swiden_c),
	(ti8,	ti16,	swiden_c),
	(ti8,	ti32,	swiden_c),
	(ti8,	ti64,	swiden_c),
	(ti8,	tbool,	bool_c),
	(ti8,	tu8,	soft_c),
	(ti8,	tu16,	swiden_c),
	(ti8,	tu32,	swiden_c),
	(ti8,	tu64,	swiden_c),
	(ti8,	tr32,	sfloat_c),
	(ti8,	tr64,	sfloat_c),

	(ti16,	ti8,	truncate_c),
	(ti16,	ti16,	no_conv),
	(ti16,	ti32,	swiden_c),
	(ti16,	ti64,	swiden_c),
	(ti16,	tbool,	bool_c),
	(ti16,	tu8,	truncate_c),
	(ti16,	tu16,	soft_c),
	(ti16,	tu32,	swiden_c),
	(ti16,	tu64,	swiden_c),
	(ti16,	tr32,	sfloat_c),
	(ti16,	tr64,	sfloat_c),
	(ti32,	ti8,	truncate_c),

	(ti32,	ti16,	truncate_c),

	(ti32,	ti32,	no_conv),
	(ti32,	ti64,	swiden_c),
	(ti32,	tbool,	bool_c),
	(ti32,	tu8,	truncate_c),
	(ti32,	tu16,	truncate_c),
	(ti32,	tu32,	soft_c),
	(ti32,	tu64,	swiden_c),
	(ti32,	tr32,	sfloat_c),
	(ti32,	tr64,	sfloat_c),

	(ti64,	ti8,	truncate_c),
!	(ti64,	ti8,	narrow_c),

	(ti64,	ti16,	truncate_c),
	(ti64,	ti32,	truncate_c),
	(ti64,	ti64,	no_conv),
	(ti64,	tbool,	bool_c),

	(ti64,	tu8,	truncate_c),
!	(ti64,	tu8,	narrow_c),

	(ti64,	tu16,	truncate_c),
	(ti64,	tu32,	truncate_c),
	(ti64,	tu64,	soft_c),
	(ti64,	tr32,	sfloat_c),
	(ti64,	tr64,	sfloat_c),
	(tbool,	ti8,	soft_c),
	(tbool,	ti16,	uwiden_c),
	(tbool,	ti32,	uwiden_c),
	(tbool,	ti64,	uwiden_c),
	(tbool,	tbool,	no_conv),
	(tbool,	tu8,	soft_c),
	(tbool,	tu16,	uwiden_c),
	(tbool,	tu32,	uwiden_c),
	(tbool,	tu64,	uwiden_c),
	(tbool,	tr32,	ufloat_c),
	(tbool,	tr64,	ufloat_c),
	(tu8,	ti8,	soft_c),
	(tu8,	ti16,	uwiden_c),
	(tu8,	ti32,	uwiden_c),
	(tu8,	ti64,	uwiden_c),
	(tu8,	tbool,	bool_c),
	(tu8,	tu8,	soft_c),
	(tu8,	tu16,	uwiden_c),
	(tu8,	tu32,	uwiden_c),
	(tu8,	tu64,	uwiden_c),
	(tu8,	tr32,	ufloat_c),
	(tu8,	tr64,	ufloat_c),

	(tu16,	ti8,	truncate_c),
	(tu16,	ti16,	soft_c),
	(tu16,	ti32,	uwiden_c),
	(tu16,	ti64,	uwiden_c),
	(tu16,	tbool,	bool_c),
	(tu16,	tu8,	truncate_c),
	(tu16,	tu16,	no_conv),
	(tu16,	tu32,	uwiden_c),
	(tu16,	tu64,	uwiden_c),
	(tu16,	tr32,	ufloat_c),
	(tu16,	tr64,	ufloat_c),

	(tu32,	ti8,	truncate_c),
	(tu32,	ti16,	truncate_c),
	(tu32,	ti32,		soft_c),
	(tu32,	ti64,	uwiden_c),
	(tu32,	tbool,	bool_c),
	(tu32,	tu8,	truncate_c),
	(tu32,	tu16,	truncate_c),
	(tu32,	tu32,	no_conv),
	(tu32,	tu64,	uwiden_c),
	(tu32,	tr32,	ufloat_c),
	(tu32,	tr64,	ufloat_c),

	(tu64,	ti8,	truncate_c),
	(tu64,	ti16,	truncate_c),
	(tu64,	ti32,	truncate_c),
	(tu64,	ti64,	soft_c),
	(tu64,	tbool,	bool_c),
	(tu64,	tu8,	truncate_c),
	(tu64,	tu16,	truncate_c),
	(tu64,	tu32,	truncate_c),
	(tu64,	tu64,	no_conv),
	(tu64,	tr32,	ufloat_c),
	(tu64,	tr64,	ufloat_c),

	(tr32,	ti8,	sfix_c),
	(tr32,	ti16,	sfix_c),
	(tr32,	ti32,	sfix_c),
	(tr32,	ti64,	sfix_c),
	(tr32,	tbool,	ufix_c),
	(tr32,	tu8,	ufix_c),
	(tr32,	tu16,	ufix_c),
	(tr32,	tu32,	ufix_c),
	(tr32,	tu64,	ufix_c),
	(tr32,	tr32,	no_conv),
	(tr32,	tr64,	fwiden_c),

	(tr64,	ti8,	sfix_c),
	(tr64,	ti16,	sfix_c),
	(tr64,	ti32,	sfix_c),
	(tr64,	ti64,	sfix_c),
	(tr64,	tbool,	ufix_c),
	(tr64,	tu8,	ufix_c),
	(tr64,	tu16,	ufix_c),
	(tr64,	tu32,	ufix_c),
	(tr64,	tu64,	ufix_c),
	(tr64,	tr32,	fnarrow_c),
	(tr64,	tr64,	no_conv),

)

global []int badexprs=(
jconst,
jname,
jifx,
jandl,
jorl,
jnotl,
jistruel,
jexprlist,
jandand,
jeq,
jne,
jlt,
jle,
jge,
jgt,
jadd,
jsub,
jmul,
jdiv,
jrem,
jiand,
jior,
jixor,
jshl,
jshr,
jdot,
jidot,
jindex,
jptr,
jaddptr,
jsubptr,
jneg,
jabs,
jinot)

=== cc_lex.m 0 0 6/21 ===
! (C tokeniser module)
ref tokenrec tkptr=nil

int dowhitespace=0

GLOBAL int NINCLUDES

const mcchdr = "mcc.h"

record stackinforec =
	ref char startptr
	ref char sptr
	i32 lineno
	i32 fileno
end

const maxmacroargs=200
tokenrec normaltkx
ref tokenrec normaltk = &normaltkx			!indicates use lexm to get tokens
int noexpand=0						!inhibit macro expansion for 'defined'

const maxnesting=20
[maxnesting]stackinforec lx_stack
int lx_stackindex
int ifcondlevel=0					!counts #if levels
[maxnesting]ichar headerpathlist	!remember path at each level
[300]char headerpath				!as set up by getsource()

const cr	= 13
const lf	= 10
const tab	= 9

ref char lxstart
ref char lxsptr
int lxhashvalue
ref char lxsvalue

[0..255]char alphamap
[0..255]char digitmap
[0..255]char commentmap
[0..255]char linecommentmap
[0..255]char spacemap

ref strbuffer destcopy
!const int maxpastedtokens=7000
const int maxpastedtokens=87000
[maxpastedtokens]ichar pastedtokenlist
int npastedtokens=0
int isincludefile=0				!whether readng include file name

int firstsymbol=1
ref byte reallxsptr

global int nhstsymbols
int hstthreshold				!limit above which new hst should be generated

global proc lex_preprocess_only(ichar infile, outfile, int toconsole=0)=
	ref char psource
	int ntokens,nlines,fileno,size
	int LENGTH
	i64 nchars,t,hashtot,symtot
	real tsecs
	static strbuffer sbuffer
	static ref strbuffer dest=&sbuffer
	filehandle f

	dowhitespace:=1
	fileno:=loadsourcefile(infile,infile)

	psource:=cast(sourcefiletext[fileno])

	size:=sourcefilesizes[fileno]

	nlines:=ntokens:=0
	hashtot:=symtot:=0
	t:=os_clock()

	destcopy:=dest
	gs_init(dest)

	lxsptr:=psource
	lxstart:=lxsptr
	nextlx.lineno:=1
	setfileno(1)
	ifcondlevel:=0

	stacksourcefile(mcchdr,1)

	nextlx.symbol:=eolsym

	repeat
		lexm()
		++ntokens

!		if showtokens then
			emittoken(&nextlx,dest)
!		fi

	until nextlx.symbol=eofsym

	if ifcondlevel then
		lxerror("#endif missing")
	fi

	if showtokens then
		if toconsole then
			gs_println(dest,nil)
		else
			f:=fopen(outfile,"wb")
			gs_println(dest,f)
			fclose(f)
		fi
	fi
end

global proc lexreadtoken=
!read next token into nextlx
!	int c,csum,hsum,dodir
	word c,csum,hsum,dodir
	ref char p,ss
	ichar searchstr

	nextlx.subcode:=0
	nextlx.flags:=0

!	while lxsptr^=' ' do ++lxsptr od
	while lxsptr^ in [' ','\t'] do ++lxsptr od

	doswitch lxsptr++^
	when 'A'..'Z','a'..'z','$','_' then
doname:
		lxsvalue:=lxsptr-1
		hsum:=lxsvalue^

		while alphamap[c:=lxsptr++^] do
			hsum:=hsum<<4-hsum+c
		od
		--lxsptr
		nextlx.symbol:=namesym
		nextlx.length:=lxsptr-lxsvalue
		case c
		when '\'', '"' then
			if nextlx.length=1 then
				case lxsvalue^
				when 'l','L','u','U' then
					++lxsptr
					lxreadstring(c,1)
					return
				esac
			fi
		esac

		lxhashvalue:=hsum<<5-hsum

!		ss:=pcm_alloc(nextlx.length+1)		!zero-term in lex(), as headers may need to be
!		memcpy(ss,lxsvalue,nextlx.length)	!re-tokenised
!		(ss+nextlx.length)^:=0
!		lxsvalue:=ss

		lookup()						!clash, so do normal lookup to set lxsymptr
		return

	when '1'..'9' then					!can only be decimal
		case lxsptr^
		when ' ',')',cr,',',';' then		!assume single digit decimal
			nextlx.symbol:=intconstsym
			nextlx.subcode:=ti32
			nextlx.value:=(lxsptr-1)^-'0'
			nextlx.length:=1

			setnumberoffset(lxsptr-1-lxstart)
		else
			readdecimal(lxsptr-1)				!note: can also be real const;
		esac
		return

	when '0' then					!0, hex, binary or octal
		switch lxsptr^
		when 'x','X' then
			++lxsptr
			readhex(lxsptr-2)
			return
		when 'b','B' then
			++lxsptr
			readbinary(lxsptr-2)
			return
		when '.' then
			readrealnumber(lxsptr-1,lxsptr-1,1,10)
			return
		when 'u','U','l','L' then
			readdecimal(lxsptr-1)				!note: can also be real const;
			return
		when ',', ')', ']', '}', ';', ' ',':',cr,lf,'&','=','?' then	!assume just zero
			nextlx.symbol:=intconstsym
			nextlx.subcode:=ti32
			nextlx.value:=0
			nextlx.length:=1
			setnumberoffset(lxsptr-1-lxstart)
			return
		else

			readoctal(lxsptr-1)
			return
		end switch					!else assume just zero	

	when '#' then			!
		if nextlx.symbol=eolsym then
			nextlx.symbol:=lexhashsym

			return

		elsif lxsptr^='#' then
			++lxsptr
			nextlx.symbol:=hashhashsym
			return
		else
			nextlx.symbol:=hashsym
			return
		fi

	when '\\' then			!line continuation
		docase lxsptr^
		when cr,lf then
			exit
		when ' ',tab then
			++lxsptr
		else
			nextlx.symbol:=backslashsym
			return
		end docase

		(lxsptr-1)^:=' '	!convert \ to space
		++nextlx.lineno
		case lxsptr^
		when cr then
			++lxsptr			!point to lf
			lxsptr++^:=' '		!set lf to space (so that '#' processing works
		when lf then
			lxsptr++^:=' '
		else
		esac

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
				--lxsptr
				nextlx.symbol:=dotsym
				return
			fi
			return
		when '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
			readrealnumber(lxsptr,lxsptr,0,10)
			return
		else
			nextlx.symbol:=dotsym
			return
		end switch

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
		case lxsptr^
		when '|' then
			++lxsptr
			nextlx.symbol:=orlsym
		when '=' then
			++lxsptr
			nextlx.symbol:=iortosym
		else
			nextlx.symbol:=iorsym
		esac
		return

	when '^' then
		if lxsptr^='=' then
			++lxsptr
			nextlx.symbol:=ixortosym
		else
			nextlx.symbol:=ixorsym
		fi
		return

	when '?' then
		nextlx.symbol:=questionsym
		return

	when '~' then
		nextlx.symbol:=inotsym
		return

	when '+' then
		case lxsptr^
		when '+' then
			++lxsptr
			nextlx.symbol:=incrsym
		when '=' then
			++lxsptr
			nextlx.symbol:=addtosym
		else
			nextlx.symbol:=addsym
		esac
		return

	when '-' then
		case lxsptr^
		when '-' then
			++lxsptr
			nextlx.symbol:=decrsym
		when '>' then
			++lxsptr
			nextlx.symbol:=idotsym
		when '=' then
			++lxsptr
			nextlx.symbol:=subtosym
		else
			nextlx.symbol:=subsym
		esac
		return

	when '*' then
		if lxsptr^='=' then
			++lxsptr
			nextlx.symbol:=multosym
		else
			nextlx.symbol:=mulsym
		fi
		return

	when '/' then
		case lxsptr^
		when '/' then					!comment to 
			readlinecomment()
			nextlx.symbol:=eolsym
			nextlx.length:=0
			return
		when '*' then
			readblockcomment()
		when '=' then
			++lxsptr
			nextlx.symbol:=divtosym
			return
		else
			nextlx.symbol:=divsym
			return
		esac

	when '%' then
		if lxsptr^='=' then
			++lxsptr
			nextlx.symbol:=remtosym
		else
			nextlx.symbol:=remsym
		fi
		return

	when '=' then
		case lxsptr^
		when '=' then
			nextlx.symbol:=eqsym
			++lxsptr
		else
			nextlx.symbol:=assignsym
		esac
		return

	when '<' then
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=lesym
		when '<' then
			if (++lxsptr)^='=' then
				++lxsptr
				nextlx.symbol:=shltosym
			else
				nextlx.symbol:=shlsym
			fi
		else
			nextlx.symbol:=ltsym
		end switch
		return

	when '>' then
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=gesym
		when '>' then
			if (++lxsptr)^='=' then
				++lxsptr
				nextlx.symbol:=shrtosym
			else
				nextlx.symbol:=shrsym
			fi
		else
			nextlx.symbol:=gtsym
		end switch
		return

	when '&' then
		case lxsptr^
		when '&' then
			++lxsptr
			nextlx.symbol:=andlsym
		when '=' then
			++lxsptr
			nextlx.symbol:=iandtosym
		else
			nextlx.symbol:=iandsym
		esac
		return

	when '\'' then
		lxreadstring('\'',0)
		return

	when '"' then
		lxreadstring('"',0)
		return

	when ' ',tab then

	when lf then
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0
		if dowhitespace then
			nextlx.svalue:=cast(lxsptr)
			doswitch (lxsptr++)^
			when ' ',tab then
			else
				--lxsptr
				exit
			end
!			while spacemap[(++lxsptr)^] do od
			nextlx.length:=lxsptr-nextlx.svalue
		fi
		return
	when cr then				!ignore; always expect lf to follow

	when '!' then
		case lxsptr^
		when '=' then
			nextlx.symbol:=nesym
			++lxsptr
		else
			nextlx.symbol:=notlsym
		esac
		return

	when '@' then
		PRINTLN "@ SEEN",nextlx.lineno,sourcefilenames[nextlx.fileno],lx_stackindex

	when 0 then
	doeof:
		--lxsptr
		if lx_stackindex then
			unstacksourcefile()
			nextlx.symbol:=eolsym
		else
			nextlx.symbol:=eofsym
		fi
		return

	when 12 then

	when 0xEF then			!BOM
		lxsptr+:=2

	else
		if 128<=(lxsptr-1)^<= 255then goto doname fi

		PRINTLN "ERROR CHAR",(lxsptr-1)^,int((lxsptr-1)^),lx_stackindex
		lxerror("ERROR CHAR")
		nextlx.symbol:=errorsym
		return

	end doswitch

end

proc readrealnumber(ref char pstart,intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, (or to "." if there was no prefix, then intlen=0)
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
	ref char fractstart
	int fractlen,expon,i,c,badexpon,n,adj
	real basex,x,expbase,f,y,y2,g
	i64 aa,cc,pref
	const maxrealdigits=500
	[maxrealdigits+12]char realstr
	ref char rs
	[32]char expstr
	u64 xx1,xx2

	if base<>10 then
		old_readrealnumber(pstart,intstart,intlen,base)
		return
	fi

	fractstart:=nil
	fractlen:=0
	expon:=0

	if lxsptr^='.' then		!read
		fractstart:=++lxsptr
		fractlen:=scannumber(base)-fractstart
	fi
	badexpon:=0

	case lxsptr^
	when 'e','E' then
		if base<>16 then
			++lxsptr
			expon:=readexponent(badexpon)
		fi
	when 'p','P' then
		if base=16 then
			++lxsptr
			expon:=readexponent(badexpon)
		fi
	esac

	if badexpon then
		--lxsptr
		readalphanumeric(pstart)
		return
	fi

	case lxsptr^
	when 'f','F','l','L' then
		++lxsptr
	else
		if alphamap[lxsptr^] then
			readalphanumeric(pstart)
			return
		fi
	esac

	if base=16 then
		realstr[1]:='0'
		realstr[2]:='x'
		rs:=&realstr[3]
		pref:=2
	else
		rs:=&realstr[1]
		pref:=0
	fi

	if intlen+fractlen>maxrealdigits then
		lxerror("Real too long")
	fi
	if intlen then
		memcpy(rs,intstart,intlen)
	fi
	if fractlen then
		memcpy(rs+intlen,fractstart,fractlen)
	fi

	expbase:=basex:=base

	if base=10 then
		expon-:=fractlen
	else
		expon-:=fractlen*4				!each hex digit is 4 binary bits
		expbase:=2.0
	fi

	realstr[pref+intlen+fractlen+1]:=0

	print @&.expstr,(base=10|"e"|"p"),,expon

	strcat(&.realstr,&.expstr)
	if base<>10 then
		lxerror("Non-base-10 floats temporarily unavailable")
	fi

	x:=strtod(&.realstr,nil)

	nextlx.symbol:=realconstsym
	nextlx.subcode:=tr64
	nextlx.xvalue:=x

	setnumberoffset(intstart-lxstart)
	nextlx.length:=lxsptr-intstart
end

function readexponent(int &badexpon)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
!exponent is always in base 10
	ref char numstart
	int length,neg,c
	i64 a

	neg:=0
	case lxsptr^
	when '+' then ++lxsptr
	when '-' then ++lxsptr; neg:=1
	esac

	numstart:=lxsptr
	length:=scannumber(10)-numstart

	if length=0 then
		badexpon:=1
		return 0
	fi

	a:=0

	to length do
		c:=numstart++^
		a:=a*10+c-'0'
	od

	return (neg|-a|a)
end

proc lxerror(ichar mess)=
	PRINTLN "\nLex error",mess,"in:",,sourcefilepaths[getfileno()],
	 "Line:",nextlx.lineno
	println
	println
	println
!	os_getch()
	stop 11
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s",symbolnames[l.symbol])

	case l.symbol
	when namesym then
		printstrn(l.symptr.name,l.symptr.namelen)

	when intconstsym then
		print l.value,," "
		shownumberstr(lp)

	when realconstsym then
		print l.xvalue,," "
		shownumberstr(lp)

	when stringconstsym then
		print """"
		printstrn(l.svalue,l.length)
		print """"
	when charconstsym then
		print "'"
		printstrn(l.svalue,l.length)
		print "'"

	elsif l.subcode then
		print "#",l.subcode
	end

	println
end

global proc lexsetup=
!do one-time setup:
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i

	inithashtable()
	fillhashtable()

	for i:=0 to 255 do
		switch i
!		when 'A'..'Z','a'..'z','$','_','0'..'9' then
		when 'A'..'Z','a'..'z','$','_','0'..'9', 128..255 then
			alphamap[i]:=1
		end
		switch i
		when '0'..'9' then
			digitmap[i]:=1
		end
		commentmap[i]:=1
		linecommentmap[i]:=1
		spacemap[i]:=0
	od

	commentmap['*']:=0
	commentmap[0]:=0
	commentmap[lf]:=0

	linecommentmap[0]:=0
	linecommentmap['\\']:=0
	linecommentmap[lf]:=0

	spacemap[' ']:=1
	spacemap[tab]:=1

	normaltkx.symbol:=eolsym
	npastedtokens:=0
end

global proc printstrn(ichar s, int length,filehandle f=nil)=
	if length then
		if f=nil then
			print length:"v",,s:".*"
		else
			print @f,length:"v",,s:".*"
		fi
	fi
end

function scannumber(int base)ref char=
	ref char dest
	int c

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
	else
		--lxsptr
		exit
	end doswitch
	return dest
end

function lookup:int=
	int j, wrapped,length

	retry:
	j:=lxhashvalue iand hstmask
	wrapped:=0

	do
		nextlx.symptr:=hashtable^[j]
		length:=nextlx.symptr.namelen

		if not length then
			exit
		fi

		if length=nextlx.length then	!match on length
			if memcmp(nextlx.symptr.name,lxsvalue,length)=0 then	!match
				return 1
			fi
		fi

!++NCLASHES

		if ++j>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			j:=0
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	if nhstsymbols>=hstthreshold then
		newhashtable()

		lxhashvalue:=gethashvalue(lxsvalue,nextlx.length)
		goto retry
	fi

!	nextlx.symptr.name:=lxsvalue
	nextlx.symptr.name:=pcm_copyheapstringn(lxsvalue,nextlx.length)
	nextlx.symptr.namelen:=nextlx.length
	nextlx.symptr.symbol:=namesym

	++nhstsymbols

	return 0
end

global function gethashvalue(ichar s,int length=-1)word=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!assumes s is lower-case, as conversion not done
!	int c,hsum
	word c,hsum

	if length=-1 then
		length:=strlen(s)
	fi
	hsum:=0

	to length do
		hsum:=hsum<<4-hsum+word(s++^)
	od
	return hsum<<5 -hsum
end

proc inithashtable=
	hashtable:=pcm_alloc(hstsize*(ref void.bytes))
	hstmask:=hstsize-1

	for i:=0 to hstmask do
		hashtable^[i]:=pcm_allocz(strec.bytes)
	od

	nhstsymbols:=0
	hstthreshold:=(6*hstsize)/10

end

proc fillhashtable=
!populate hashtable with standard symbols
	int i

	for i:=1 to stnames.len do
		lxsvalue:=stnames[i]

!sourcedir names can be converted to user name types, which could have a
!zero appended in lex() as the assumption is they are still in-situ within the source
!But with compilers like gcc, these names are in read-only memory.
!So copy them to the heap.

		if stsymbols[i]=ksourcedirsym then
			lxsvalue:=pcm_copyheapstring(lxsvalue)
		fi
		nextlx.length:=strlen(lxsvalue)
		lxhashvalue:=gethashvalue(lxsvalue,nextlx.length)

		if lookup() then
			println stnames[i]
			abortprogram("Duplicate symbol table entry")
		fi

		nextlx.symptr.symbol:=stsymbols[i]
		nextlx.symptr.subcode:=stsubcodes[i]
	od

end

function dolexdirective:int=
!positioned just after '#' which is first symbol on a line
!read pp directive and act on it
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
	ref strec symptr,d
	ref char p,pstart,s
	int i,cond,c,syshdr,dir,length, allowmacros
	[300]char filename

	pstart:=lxsptr

	dir:=getlexdirective()
	if dir=0 then
		printstrn(pstart,lxsptr-pstart); println
		lxerror("Invalid # directive")
	fi

	case dir
	when includedir then
		isincludefile:=1

		while lxsptr^=' ' or lxsptr^=tab do ++lxsptr od
		allowmacros:=lxsptr^ <> '<'

		lexm()
		isincludefile:=0

		if nextlx.symbol=ltsym then
			syshdr:=1
			p:=&.filename

			if allowmacros then

				do
					lexm()
					case nextlx.symbol
					when eofsym, eolsym then
						lxerror("Bad include file")
					when gtsym then
						exit
					else
						s:=strtoken(&nextlx,length)
						memcpy(p,s,length)
						p+:=length
					esac
				od
			else
				do
					c:=lxsptr++^
					case c
					when '>' then
						exit
					when lf,0 then
						lxerror("include: > expected")
					else
						p++^:=c
					esac
				od
			fi
			p^:=0

		elsif nextlx.symbol=stringconstsym then
			syshdr:=0
			strcpy(&.filename,nextlx.svalue)
		else
			lxerror("include?")
		fi
		lexm()

	IF FSHOWINCLUDES THEN
		PRINTLN "INCLUDE",&.filename,"FROM",sourcefilepaths[getfileno()],nextlx.lineno,
			=nsourcefiles
	FI
	++NINCLUDES

		stacksourcefile(&.filename,syshdr)

	when definedir then
		dodefine()

	when undefdir then
		lexreadtoken()
		if nextlx.symbol<>namesym then
			lxerror("undef: name expected")
		fi
		d:=nextlx.symptr
		if d.nameid<>macroid then
!			println getstname(nextlx.symptr)
!			lxerror("#undef: can't find macro")
		else
			d.nameid:=nullid
			d.symbol:=nextlx.symptr.oldsymbol
			d.mparamlist:=nil
			d.flmacro:=0
		fi

	when ifdefdir then
		cond:=getifdef()
		goto doif

	when ifndefdir then
		cond:=not getifdef()
		goto doif

	when ifdir then
		cond:=getifexpr()
	doif:

		++ifcondlevel
		if cond then			!carry on reading code as normal
			return 0
		else
	doskipcode:
			dir:=skipcode()
			case dir
			when elifdir then
				cond:=getifexpr()
				if cond then			!do this
					return 0
				fi
				goto doskipcode
			when elsedir then			!do following code
			when endifdir then
				--ifcondlevel
			esac
		fi

	when elifdir, elsedir then			!encountered after true block
		if not ifcondlevel then
			lxerror("#if missing/elif/else")
		fi
		repeat
			dir:=skipcode()
		until dir=endifdir
		--ifcondlevel

	when endifdir then
		if not ifcondlevel then
			lxerror("#if missing/endif")
		fi
		--ifcondlevel

	when blankdir then
	when linedir then
		repeat
			lexreadtoken()
		until nextlx.symbol=eolsym
	when errordir then
		lexm()
		print "#ERROR:"; showtoken(&nextlx); println
		lxerror("ABORTING")

	when pragmadir then
		dopragmadir()

	else
	skip:
		println "DIRECTIVE NOT IMPL:",sourcedirnames[dir]
		lxsptr:=pstart
		nextlx.symbol:=lexhashsym
		return 1
		lxerror("Directive not implemented")
	esac
	return 0
end

function getlexdirective:int=
!at '#'; read directive, and return index; 0 on error
	ref strec d

	lexreadtoken()

	case nextlx.symbol
	when namesym then
	when eolsym then
		return blankdir
	when intconstsym then
		repeat
			lexreadtoken()
		until nextlx.symbol=eolsym or nextlx.symbol=eofsym
		return blankdir
	else
		return 0
	esac

	case nextlx.symptr.symbol
	when ksourcedirsym then
		return nextlx.symptr.subcode
	when kifsym then
		return ifdir
	when kelsesym then
		return elsedir
	when eolsym then
		return blankdir
	esac

	d:=nextlx.symptr
	if d.nameid=macroid then			!could have redefined 'define' etc
		if d.oldsymbol=ksourcedirsym then
			return d.subcode
		fi
	fi

	return 0
end

global proc startlex(ichar caption,int fileno)=
!s is a 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it

	ifcondlevel:=0
	lx_stackindex:=0
	noexpand:=0

	normaltk := &normaltkx			!indicates use lexm to get tokens

	lx_stackindex:=0
	ifcondlevel:=0
	firstsymbol:=1
	npastedtokens:=0
	isincludefile:=0
	tkptr:=nil

	lxstart:=lxsptr:=sourcefiletext[fileno]
	setfileno(fileno)
	nextlx.lineno:=1
	nextlx.numberoffset:=0

	nextlx.symbol:=eolsym
	nextlx.subcode:=0
	lex()
end

global proc endlex=
	if ifcondlevel then
		println ifcondlevel
		lxerror("#endif missing")
	fi
end

global proc PS(ichar caption)=
	print caption,,":::"
	printsymbol(&lx)
end

global proc PSNEXT(ichar caption)=
	print caption,,":##"
	printsymbol(&nextlx)
end

global function gethashtablesize:int=
	int i,n

	n:=0
	for i:=0 to hstmask do
		if hashtable^[i].name then
			++n
		fi
	od

	return n
end

proc readlinecomment=
!positioned at second '/' of '//'

	do
		while linecommentmap[(++lxsptr)^] do od		!skip bulk of characters

		case lxsptr^
		when lf then
			++lxsptr
			exit
		when 0 then
			exit					!assume on last line not ending in newline char
		when '\\' then
			++lxsptr
			case lxsptr^
			when cr then			!skip following lf and loop
				lxsptr+:=2
				++nextlx.lineno
			when lf then			!loop
				++lxsptr
				++nextlx.lineno
			esac					!else ignore and loop
		esac
	od
	++nextlx.lineno
end

proc readblockcomment=
!positioned at '*' of '/*'

	do
		while commentmap[(++lxsptr)^] do od		!skip bulk of characters

		case lxsptr^
		when lf then
			++nextlx.lineno
		when 0 then
			lxerror("block comment eof")
		when '*' then
			if (lxsptr+1)^='/' then		!found end of comment
				lxsptr+:=2
				exit
			fi
		esac
	od
end

proc readhex(ref char pstart)=
!positioned at first char of hex number, after 0x/0X
	u64 aa
	word c
	int length,leading,ll,usigned
	ref char p

	aa:=0
	p:=lxsptr
	leading:=1
	ll:=usigned:=0
	length:=0

	doswitch c:=lxsptr++^
	when '1'..'9' then
		leading:=0
		aa:=aa*16+(c-'0')
		++length
	when '0' then
		if leading then
			++p			!ignore leading zeros
		else
			++length
			aa:=aa*16
		fi
	when 'A'..'F' then
		leading:=0
		++length
		aa:=aa*word(16)+(c-'A'+10)
	when 'a'..'f' then
		leading:=0
		++length
		aa:=aa*word(16)+(c-'a'+10)
	when '.','P','p' then
		--lxsptr
		readrealnumber(pstart,p,lxsptr-p,16)
		return
	when 'L','l' then
		++ll
		if ll>2 then lxerror("-LL?") fi
	when 'U','u' then
		if usigned then lxerror("-U?") fi
		usigned:=1
	else
		--lxsptr
		exit
	end doswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>16 then
		lxerror("Overflow in hex number")
	fi

	nextlx.symbol:=intconstsym
	if aa>u64(0x7FFF'FFFF'FFFF'FFFF) then
		nextlx.subcode:=tu64
	elsif aa>u64(0xFFFF'FFFF) then
		nextlx.subcode:=ti64
	elsif aa>u64(0x7FFF'FFFF) then
		nextlx.subcode:=tu32
	else
		nextlx.subcode:=ti32
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readbinary(ref char pstart)=
!positioned at first char of binary number, after 0b/0B
	u64 aa
	int c,length,res,leading
	ref char p

	aa:=0
	p:=lxsptr
	leading:=1

	doswitch c:=lxsptr++^
	when '1' then
		leading:=0
	when '0' then
		if leading then ++p fi					!ignore leading zeros
	when '2'..'9' then
		lxerror("Binary bad digit")
	when '.' then
		lxerror("Binary fp")

	else
		--lxsptr
		exit
	end doswitch

	length:=lxsptr-p
	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>64 then
		lxerror("Overflow in binary number")
	fi

	to length do
		aa:=aa*2+p++^-'0'
	od

	nextlx.symbol:=intconstsym
	nextlx.subcode:=ti32
	if aa>=u64(0x7FFF'FFFF) then
		nextlx.subcode:=ti64
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readoctal(ref char pstart)=
!positioned at first char of octal number, after 0 (or at 8 or 9)
	u64 aa
	int c,length,res,leading,ll,usigned
	ref char p

	aa:=0
	p:=lxsptr
	leading:=1
	ll:=usigned:=0
	length:=0

	doswitch c:=lxsptr++^
	when '1'..'7' then
		leading:=0
		++length
	when '0' then
		if leading then
			++p				!ignore leading zeros
		else
			++length
		fi
	when '.' then
		--lxsptr
		readrealnumber(pstart,p,lxsptr-p,10)
		return
	when 'L','l' then
		++ll
		if ll>2 then lxerror("-LL?") fi
	when 'U','u' then
		if usigned then lxerror("-U?") fi
		usigned:=1
	else
		if alphamap[c] then
	doalpha:
			readalphanumeric(pstart)
			return
		fi
		--lxsptr
		exit
	end doswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>22 or length=22 and (res:=cmpstringn(p,"1777777777777777777777",22))>0 then
		lxerror("Overflow in octal number")
	fi

	to length do
		aa:=aa*8+p++^-'0'
	od

	nextlx.symbol:=intconstsym
	nextlx.subcode:=ti32
	if aa>=u64(0x7FFF'FFFF) then
		nextlx.subcode:=ti64
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readdecimal(ref char pstart)=
!positioned at first char of decimal number
!will read integer, unless ends with any of ".eE" than assumed to be real
	u64 aa
	int c,length,res,leading
	byte ll,usigned

	ref char p

	aa:=0
	ll:=usigned:=0

	p:=--lxsptr

	while digitmap[(++lxsptr)^] do od

	while p^='0' do ++p od
	length:=lxsptr-p

	doswitch c:=lxsptr++^
	when '.','E','e' then
		--lxsptr
		readrealnumber(pstart,p,lxsptr-p,10)
		return
	when 'L','l' then
		++ll
		if ll>2 then lxerror("-LL?") fi
	when 'U','u' then
		if usigned then lxerror("-U?") fi
		usigned:=1
	else
		if alphamap[c] then
			readalphanumeric(pstart)
			return
		fi
		--lxsptr
		exit
	end doswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>20 or length=20 and (res:=cmpstringn(p,"18446744073709551615",20))>0 then
		lxerror("Overflow in decimal number")
	fi

	to length do				!A..Z have been preprocessed so that they carry on from '9'
		aa:=aa*u64(10)+word(p++^-'0')
	od

	nextlx.symbol:=intconstsym

!	if aa>=i32.max then
!		nextlx.subcode:=ti64
!	else
!		nextlx.subcode:=ti32
!	fi

	case ll
	when 0 then
		if aa>=i32.max then
			nextlx.subcode:=ti64
		else
			nextlx.subcode:=ti32
		fi
		if usigned then
			if aa>=u64(0xFFFF'FFFF) then
				nextlx.subcode:=tu64
			else
				nextlx.subcode:=tu32
			fi
		else
			if aa>=u64(0x7FFF'FFFF) then
				nextlx.subcode:=ti64
			fi
		fi
	when 1 then
		if usigned then
			if aa>=u64(0xFFFF'FFFF) then
				nextlx.subcode:=tu64
			else
				nextlx.subcode:=tu32
			fi
		else
			if aa>=u64(0x7FFF'FFFF) then
				nextlx.subcode:=ti64
			fi
		fi
	when 2 then
		if usigned then
			nextlx.subcode:=tu64
		else
			nextlx.subcode:=ti64
		fi
	esac

	nextlx.value:=aa
end

function checknumbersuffix:int=
!return type of the constant
!positioned at terminator character which might be a suffix
	char c

	doswitch c:=lxsptr++^
	when 'L','l','u','U' then
!	lxerror("Numeric SUFFIX")
	else
		if alphamap[c] then
!*!		lxerror("Bad number suffix")
		fi
		--lxsptr
		exit
	end doswitch

	return ti32			!don't bother for now
end

proc stacksourcefile(ichar file,int syshdr)=
	ref char sptr
	int fileno
	stackinforec info
	[500]char fullpath

	fileno:=getsourcefile(file,syshdr)
	if fileno=0 then
		println file,strlen(file)
		lxerror("Can't find include file")
	fi

	if lx_stackindex>=maxnesting then
		lxerror("Too many nested includes")
	fi
	++lx_stackindex

	fullpath[1]:=0
	if lx_stackindex>1 then
		strcpy(&.fullpath,headerpathlist[lx_stackindex-1])
	fi

	if headerpath[1] then
		strcat(&.fullpath,pcm_copyheapstring(&.headerpath))
	fi

	headerpathlist[lx_stackindex]:=pcm_copyheapstring(&.fullpath)

	info.startptr:=lxstart
	info.sptr:=lxsptr
	info.lineno:=nextlx.lineno
	info.fileno:=getfileno()
	lx_stack[lx_stackindex]:=info

	lxstart:=lxsptr:=sourcefiletext[fileno]
	setfileno(fileno)
	nextlx.lineno:=1
end

proc unstacksourcefile=
!called has checked that stack has >=1 entries
	ichar path
	stackinforec info

	path:=headerpathlist[lx_stackindex]
	pcm_free(path,strlen(path))

	info:=lx_stack[lx_stackindex--]
	lxstart:=info.startptr
	lxsptr:=info.sptr
	nextlx.lineno:=info.lineno
	setfileno(info.fileno)
end

function getsourcefile(ichar file,int syshdr)int=
!locate using search dirs; 
!read contents into memory, and return fileno
!returns 0 in case of error (file not found, memory problem)

	static [300]char filespec
	[300]char filespec2
	ichar hdrtext
	int i

	headerpath[1]:=0

	strcpy(&.filespec,file)
	convlcstring(&.filespec)

!check to see if already loaded
	for i:=1 to nsourcefiles do
		if eqstring(&.filespec,sourcefilenames[i]) then
			return i
		fi
	od

!see if a builtin header

	if dointheaders then
		hdrtext:=findheader(&.filespec)
		if hdrtext then
			return loadbuiltin(&.filespec,hdrtext)
		fi
	fi

	if eqstring(file, mcchdr) then
		return loadbuiltin(filespec, strinclude(mcchdr))
	fi

	strcpy(&.headerpath,extractpath(file))

	if headerpath[1] then
		if headerpath[1]='/' or headerpath[2]=':' and headerpath[3]='/' then
			if checkfile(file) then
					return loadsourcefile(file,file)
			fi
			return 0			!don't both looking anywhere else
		fi
	fi

	for i:=lx_stackindex downto 1 do
		strcpy(&.filespec,headerpathlist[i])
		strcat(&.filespec,file)

		if checkfile(&.filespec) then
			return loadsourcefile(&.filespec,file)
		fi
	od

	for i to nsearchdirs do
		strcpy(&.filespec,searchdirs[i])
		strcat(&.filespec,file)

		if checkfile(&.filespec) then
			strcpy(&.headerpath,extractpath(&.filespec))
			return loadsourcefile(&.filespec,file)
		fi
	od

	return 0
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
	reenter:

	lx:=nextlx				!grab that already read basic token

	lexm()			!read new token for next time around

	if lx.symbol=namesym and lx_stackindex=0 then
		(lx.symptr.name+lx.length)^:=0
	fi

	docase nextlx.symbol
	when namesym then
		nextlx.symbol:=nextlx.symptr.symbol			!convert to reserved word, type, op etc
		if nextlx.symbol=ksourcedirsym then
			nextlx.symbol:=namesym
		fi
		nextlx.subcode:=nextlx.symptr.subcode

		return

	when eolsym then								!lose eols
		lexm()
	else
		return	
	end docase

end

proc shownumberstr(ref tokenrec l,filehandle f=nil)=
	ref char s

	if getfilenox(l) then
		s:=sourcefiletext[getfilenox(l)]+getnumberoffsetx(l)
	else
		s:=pastedtokenlist[l.pasteno]
	fi
	printstrn(s,l.length,f)

end

global function addnamestr(ichar name)ref strec=
!look up arbitrary name and return symptr to generic st entry

	tokenrec oldlx
	ref strec symptr

	oldlx:=nextlx
	nextlx.length:=strlen(name)
	lxhashvalue:=gethashvalue(name,nextlx.length)

	lxsvalue:=pcm_alloc(nextlx.length+1)
	memcpy(lxsvalue,name,nextlx.length+1)
	lookup()
	symptr:=nextlx.symptr

	nextlx:=oldlx

	return symptr
end

proc lxreadstring(int termchar,int fwide)=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter

	const maxlocalstr=2048
	[maxlocalstr]char str
	ref char dest,ws
	ref u16 wd,wd0
	int c,d,length,useheap

	if termchar='"' then
		nextlx.symbol:=(fwide|wstringconstsym|stringconstsym)
	else
		nextlx.symbol:=charconstsym
	fi

	nextlx.svalue:=lxsptr

	if lx_stackindex=0 and not fwide then
		dest:=lxsptr				!form string into same buffer
		ws:=dest					!for wide only
		useheap:=0
	else							!for headers that can be re-read, form string externally
		dest:=&.str
		ws:=dest					!for wide only
		useheap:=1
	fi
	length:=0

	do
		switch c:=lxsptr++^
		when '\\' then			!escape char
			if isincludefile then
				c:='/'
				goto normalchar
			fi
			c:=lxsptr++^
	reenter:
			switch c
			when 'a' then			!bell ('alert')
				c:=7
			when 'b' then			!backspace
				c:=8
			when 'f' then
				c:=12
			when 'n' then
				c:=lf
			when 'r' then
				c:=cr
			when 't' then			!tab
				c:=tab
			when 'v' then			!vertical tab
				c:=11
			when 'x' then	!2-digit hex code follows
				c:=0
!			to 2 do
				do
					switch d:=lxsptr++^
					when 'A','B','C','D','E','F' then
						c:=c*16+d-'A'+10
					when 'a','b','c','d','e','f' then
						c:=c*16+d-'a'+10
					when '0','1','2','3','4','5','6','7','8','9' then
						c:=c*16+d-'0'
					else
						--lxsptr
						exit
					end
				od
			when '0'..'7' then		!octal sequence
				c-:='0'				!get first digit
				to 2 do				!up to 2 more digits (some compilers will read N digits
					switch d:=lxsptr++^				!then check for overflow)
					when '0','1','2','3','4','5','6','7' then
						c:=c*8+d-'0'
					else
						--lxsptr
						exit
					end
				od

			when '"' then		!embedded double quote
				c:='"'
			when '\\' then
				c:='\\'
			when '\'' then			!embedded single quote
				c:='\''
			when cr then			!skip
				++nextlx.lineno
				if lxsptr^=lf then ++lxsptr fi
				nextloop
			when lf then
				++nextlx.lineno
				nextloop
			end						!else use the escaped character itself
		when '"','\'' then		!possible terminators
			if c=termchar then		!terminator char
				exit
			fi
		when 0 then
			println =nextlx.lineno
			lxerror("String not terminated")
		end switch
	normalchar:

		if not useheap then
			dest++^:=c
		elsif ++length<maxlocalstr then
			dest++^:=c
		else
			lxerror("Local str too long")
		fi
	od
	dest^:=0


	if fwide then			!need to put string on heap was will use 16-bit chars
		wd0:=wd:=pcm_alloc(length*2+2)
		to length do
			wd++^:=ws++^
		od
		wd^:=0
		nextlx.svalue:=cast(wd0)

	elsif useheap then
		nextlx.length:=length

		nextlx.svalue:=pcm_alloc(length+1)
		memcpy(nextlx.svalue,&.str,length+1)
	else
		nextlx.length:=dest-nextlx.svalue
	fi
end

proc addlisttoken(ref ref tokenrec ulist,ulistx,ref tokenrec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nexttoken:=p
	fi
	p.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlisttoken_copy(ref ref tokenrec ulist,ulistx,ref tokenrec q)=
!like addlisttoken but add copy of nextlx
!(as will likely be in nextlx)
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	ref tokenrec p

	p:=alloctoken()

	p^:=q^
	p.nexttoken:=nil

	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nexttoken:=p
	fi
	p.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlist_nextlx(ref ref tokenrec ulist,ulistx)=
!like addlisttoken but add copy of nextlx

	ref tokenrec p
	p:=alloctoken()
	p^:=nextlx
	p.nexttoken:=nil

	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nexttoken:=p
	fi
	p.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlisttoken_seq(ref ref tokenrec ulist,ulistx,ref tokenrec seq)=
	ref tokenrec tk

	while seq do
		tk:=alloctoken()
		tk^:=seq^

		if ulist^=nil then		!first
			ulist^:=ulistx^:=tk
		else
			ulistx.nexttoken:=tk
		fi
		tk.nexttoken:=nil
		ulistx^:=tk

		seq:=seq.nexttoken
	od
end

proc addlistmparam(ref ref mparamrec ulist,ulistx,ref mparamrec p)=
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextmparam:=p
	fi
	ulistx^:=p			!update end-of-list pointer
end

proc dodefine=
!'define' just seen

	ref mparamrec stlist,stlistx,p,q
	ref strec stname, d
	ref tokenrec tklist,tklistx,tk
	int nparams,ntokens,paramno

	lexreadtoken()
	if nextlx.symbol<>namesym then
		lxerror("define: name expected")
	fi
	stname:=nextlx.symptr
	stname.lineno:=nextlx.lineno
	stname.fileno:=nextlx.fileno

	stname.oldsymbol:=stname.symbol

	stname.symbol:=namesym
	stname.nameid:=macroid
	nparams:=0

	if lxsptr^='(' then
		++lxsptr
		stlist:=stlistx:=nil
		stname.flmacro:=1

		lexreadtoken()
		do
			case nextlx.symbol
			when namesym then			!next param
				d:=nextlx.symptr
				p:=stlist
				while p do
					if p.def=d then
						lxerror("Dupl macro param")
					fi
					p:=p.nextmparam
				od
				q:=pcm_alloc(mparamrec.bytes)
				q.def:=d
				q.nextmparam:=nil
				addlistmparam(&stlist,&stlistx,q)
				++nparams
				lexreadtoken()
				if nextlx.symbol=commasym then
					lexreadtoken()
				fi
			when rbracksym then
				exit
			when ellipsissym then					!I need to create a special symbol name
				d:=addnamestr("__VA_ARGS__")
				stname.varparams:=1		!flag macro as having a va/args as last param
				lexreadtoken()
				if nextlx.symbol<>rbracksym then
					lxerror("')' expected")
				fi

				q:=pcm_alloc(mparamrec.bytes)
				q.def:=d
				q.nextmparam:=nil
				addlistmparam(&stlist,&stlistx,q)
				++nparams
				exit
			else
				lxerror("macro params?")
			esac
		od
		stname.mparamlist:=stlist
	fi

!Now, loop reading tokens until eol
!Store tokens in list
	tklist:=tklistx:=nil
	ntokens:=0

	do
		lexreadtoken()
		case nextlx.symbol
		when eolsym,eofsym then
			exit
		when namesym then
			p:=stname.mparamlist
			paramno:=1
			while p do
				if p.def=nextlx.symptr then
					nextlx.flags ior:=tk_parammask
					nextlx.paramno:=paramno
					exit
				fi
				p:=p.nextmparam
				++paramno
			od
			if nextlx.symptr=stname then
				nextlx.flags ior:=tk_macromask
			fi
		esac

		++ntokens
		tk:=alloctoken()
		tk^:=nextlx
		addlisttoken(&tklist,&tklistx,tk)
	od

	stname.tokenlist:=tklist
	stname.nparams:=nparams
end

proc readalphanumeric(ref char pstart)=
!part-read numeric value starting at pstart is followed by non-numeric chars
!read rest of token starting from lxsptr, and form into a name token
	while alphamap[lxsptr++^] do od
	--lxsptr
	nextlx.svalue:=pstart
	nextlx.symbol:=rawnumbersym
	nextlx.length:=lxsptr-pstart
end

function inmacrostack(ref strec d, ref tokenrec macrostack)int=
!return 1 if d is part of the macrostack
!the macrostack is a linked list of strecs, but conveniently uses a list
!of tokens although it is not really a list of tokens

	while macrostack do
		if macrostack.symptr=d then return 1 fi
		macrostack:=macrostack.nexttoken
	od
	return 0
end

proc showtokens(ichar caption,ref tokenrec tk)=
	print caption,,"<"
	while tk do
		showtoken(tk)
		tk:=tk.nexttoken
	od
	println ">"
end

proc lexa(ref tokenrec &tk)=
	if tk=normaltk then
		lexreadtoken()
		return
	fi
	if tk=nil then
		nextlx.symbol:=eofsym
		return
	fi
	nextlx:=tk^
	tk:=tk.nexttoken
end

proc lexm=
!wrapper around lexreadtoken that applies macro expansion to names
	ref strec d
	static int doreset=0
	int newlineno

	do
		if tkptr then
			nextlx:=tkptr^
			tkptr:=tkptr.nexttoken
			if tkptr=nil then

				if nextlx.symbol=namesym and nextlx.symptr.nameid=macroid and peeklb() then
!fix pp bug: macro expansion ending with fn-macro name, with (...) following
!but at normal lexical level. Pick that up here
					setfileno(sfileno)
					nextlx.lineno:=slineno
					doreset:=0
					goto TEST1
				fi
				doreset:=1

			fi
			return
		fi

		if doreset then
			setfileno(sfileno)
			nextlx.lineno:=slineno
			doreset:=0
		fi

		if firstsymbol then
			firstsymbol:=0
			dospecialinclude()
		fi	
		lexreadtoken()
	TEST1:

		case nextlx.symbol
		when lexhashsym then

			if dolexdirective() then
				return
			fi
!repeat lexreadtoken() until nextlx.symbol=eolsym

			nextloop
		when namesym then
			d:=nextlx.symptr
			case d.symbol
			when predefmacrosym then

				sfileno:=getfileno()
				slineno:=nextlx.lineno
				expandpredefmacro(d.subcode,&nextlx,slineno)
				doreset:=1					!can screw up line/file numbers
				return
			else
				if d.nameid<>macroid or noexpand then
					return
				fi
			esac
		else
			return
		esac
!have a macro. Now see whether this should be expanded
		sfileno:=getfileno()
		slineno:=nextlx.lineno
		if d.flmacro then		!function-like macro; need to peek for "("
			if not peeklb() then
				return
			fi
			tkptr:=expandfnmacro(d,nil,normaltk,1,newlineno)
			slineno:=newlineno
		else										!object-like macro: always expand
			tkptr:=expandobjmacro(d,nil,normaltk,1)
		fi

		if tkptr=nil then doreset:=1 fi			!immediately restore file/lineno

	od
end

function peeklb:int=
!look at lxsptr seqence and return 1 if the next token is a "(" left bracket
!lxsptr is left unchanged whatever the result
!only a simplistic approach is used, eg. 0 or 1 space then a "(" must be nextloop
!In theory, there could be any number and combination of spaces, tabs, newlines,
!comments, strings, #-directives between this point and the next token, or
!it could be inside the next #include or just outside this one.
	if lxsptr^='(' or (lxsptr^=' ' and (lxsptr+1)^='(') then
		return 1
	fi
	return 0
end

function peektk(ref tokenrec tk)int=
!version of peeklb that works on a token list rather than chars
!tk is the current token
	tk:=tk.nexttoken
	if tk=nil then			!nothing follows
		return 0
	fi
	if tk.symbol=lbracksym then
		return 1
	fi
	return 0
end

function expandobjmacro(ref strec m,ref tokenrec macrostack, &tksource,
		int frombaselevel)ref tokenrec=
	ref tokenrec tk,p,repl
	tokenrec newmacro
	int iscomplex,useshh,expanded
	ref strec d

	p:=tk:=m.tokenlist

	iscomplex:=useshh:=0
	while p do
		if p.symbol=namesym then
			d:=p.symptr
			if d.nameid=macroid or d.symbol=predefmacrosym then
				iscomplex:=1
				exit
			fi
		elsif p.symbol=hashhashsym then
			iscomplex:=useshh:=1
			exit
		fi

		p:=p.nexttoken
	od

	if not iscomplex then
		return tk
	fi

	newmacro.symptr:=m				!add m to macrostack
	newmacro.nexttoken:=macrostack

	if useshh then
		repl:=substituteargs(m,nil,nil,0,nil)
	else
		repl:=m.tokenlist
	fi

	tk:=scantokenseq(repl,&newmacro,expanded)
	return tk
end

function expandfnmacro(ref strec m, ref tokenrec macrostack, &tksource,
		int frombaselevel, &endlineno)ref tokenrec=
!positioned just before "(" token
!read arguments from source (need to use lexm(), reading from char-sourc or tokenlist)
!store args in special arg lists, and prepare args for expansion
!(for this version, args expanded on demand only)
!get tokenlist for m, do argument substitution, then scan it looking for new
!macros to expand
	[maxmacroargs]ref tokenrec args,expargs
	ref tokenrec repl,tk
	tokenrec newmacro
	int nargs,i,expanded

	nargs:=readmacrocall(m,&args,tksource)
	if frombaselevel then
		endlineno:=nextlx.lineno
	fi

	for i:=1 to nargs do
		expargs[i]:=nil
	od

	repl:=substituteargs(m,&args,&expargs,nargs,macrostack)

	newmacro.symptr:=m				!add m to macrostack
	newmacro.nexttoken:=macrostack

	repl:=scantokenseq(repl,&newmacro,expanded)
	return repl
end

function scantokenseq(ref tokenrec tk, macrostack,int &expanded)ref tokenrec=
!scan token sequence belonging to:
! The replacelist of an object macro
! The substituted replacement list of a function macro
! An argument of a macro
!scan object macro, but can also be an argument

!d is an object macro that may contains further macro definitions
!scan it, and produce a new tokenlist that contains expanded versions
!of nested macro calls
!macrostack is a list of active nested macro defs. This is stored as
!a linked list of tokenrec records, in reverse order. This is just for
!convenience; the .symptr field is used to refer to the macro st entry

	ref tokenrec newtk,newtkx	!new list of tokens
	ref tokenrec expandtk		!token seqence from expanding a macro
	ref tokenrec oldtk
	ref strec m
	tokenrec newmacro
	int noexpandflag,simple,dummy

	reenter:
	expanded:=0

	newtk:=newtkx:=nil
	noexpandflag:=0

	simple:=1
	oldtk:=tk

	while tk do
		case tk.symbol
		when namesym then
			if tk.symptr.nameid=macroid or tk.symptr.symbol=predefmacrosym then
				simple:=0
				exit
			fi
		esac

		if tk=nil then exit fi
		tk:=tk.nexttoken
	od

	if simple then
		return oldtk
	fi

	tk:=oldtk
	while tk do
		case tk.symbol
		when namesym then
			m:=tk.symptr
			if m.nameid=macroid and not noexpandflag then
!macro detected; check if candidate for expansion
				if tk.flags iand tk_macrolit or noexpand then
					goto simpletoken
				fi

				if inmacrostack(m,macrostack) then		!is an active macro name
					addlisttoken_copy(&newtk,&newtkx,tk)
					newtkx.flags ior:= tk_macrolit
					goto skip

				fi
	simple:=0
				if m.flmacro then
					if not peektk(tk) then goto simpletoken fi
					lexa(tk)
					expandtk:=expandfnmacro(m,macrostack,tk,1,dummy)
					addlisttoken_seq(&newtk,&newtkx,expandtk)
					expanded:=1
					nextloop
				else
					expandtk:=expandobjmacro(m,macrostack,tk,0)
					expanded:=1
					addlisttoken_seq(&newtk,&newtkx,expandtk)
				fi
			elsif m.symbol=kdefinedsym then
				noexpandflag:=1
				goto simpletoken
			elsif m.symbol=predefmacrosym then
				expandtk:=alloctokenz()
!CPL "EXPAND PDM 2"
!				expandpredefmacro(m.subcode,expandtk,nextlx.lineno)
				expandpredefmacro(m.subcode,expandtk,slineno)
				addlisttoken_copy(&newtk,&newtkx,expandtk)
				goto skip2
			else
				noexpandflag:=0
				goto simpletoken
			fi
		else
	simpletoken:
			addlisttoken_copy(&newtk,&newtkx,tk)
		esac

	skip:
		if tk=nil then exit fi
	skip2:
		tk:=tk.nexttoken
	od

	if expanded then
		tk:=newtk
		goto reenter
	fi

	return newtk
end

function readmacrocall(ref strec d, ref[]ref tokenrec args, ref tokenrec &tksource)int=
!positioned just before "(" of a macro call
!read arguments for the macro, and store into args
!return total number of arguments
!each args^[i] entry is a list of tokenrecs
!Caller has already checked that "(" is next token, and this will be a function macro
!tksource will point to an input stream of tokens, but can also be nil, meaning
!read via lexm from actual source. (tksource can't be nil because it's at the
!end of ...

	int nparams,lbcount,paramno
	int nargs,usesvargs,varg
	ref tokenrec tklist,tklistx			!form list of tokens for argument

	lexa(tksource)

	if nextlx.symbol<>lbracksym then lxerror("rmc: no '('") fi

	nparams:=d.nparams
	nargs:=0
	if nparams=0 then				!) must follow
		lexa(tksource)
		if nextlx.symbol<>rbracksym then lxerror("rmc: ')' expected") fi
		return 0					!no args
	fi

	paramno:=1
	lbcount:=1
	tklist:=tklistx:=nil
	usesvargs:=d.varparams			!whether macro contains ... va/args
	varg:=0										!whether encountered ... yet in arguments

	do
		if paramno=nparams and usesvargs then varg:=1 fi
		lexa(tksource)

		case nextlx.symbol
		when commasym then
			if lbcount=1 and not varg then
				if tklist=nil then					!empty list: create place-holder token
					tklist:=alloctokenz()
					setfilenox(tklist,getfileno())
					tklist.symbol:=placeholdersym
				fi
				args^[paramno]:=tklist				!store this list
				tklist:=tklistx:=nil
				++paramno
			else
				goto addtoken
			fi

		when eofsym then
			lxerror("EOS in macro call")
		when lbracksym then
			++lbcount
			goto addtoken
		when rbracksym then
			if lbcount>1 then
				--lbcount
				addlist_nextlx(&tklist,&tklistx)
			else
				if tklist=nil then
					tklist:=alloctokenz()
					setfilenox(tklist,getfileno())
					tklist.symbol:=placeholdersym
				fi
				args^[paramno]:=tklist				!store this list
				exit
			fi
		else
	addtoken:
			addlist_nextlx(&tklist,&tklistx)
		esac
	od

	if paramno<>nparams then
		if paramno+1=nparams and usesvargs then		!no args for ... part, needs dummy arg
			args^[nparams]:=nil
		else
			lxerror("Wrong # macro params")
		fi
	fi
	return nparams
end

function substituteargs(ref strec m,ref[]ref tokenrec args,expargs, int nargs,
ref tokenrec macrostack)ref tokenrec=
!m is a macro def
!args/expargs are arguments that will replace any parameter names encountered
!in m's replacement list
!returns new replacement list with arguments inserted
	ref mparamrec params
	ref tokenrec seq,seqstart,lasttoken
	ref tokenrec newtk,newtkx,niltk,tkexp
	tokenrec tk
	int n,i,expanded

	const maxhashhash=250
	[maxhashhash]ref tokenrec hhpoints
	int nhashhash

	params:=m.mparamlist
	seq:=seqstart:=m.tokenlist		!input token sequence

	newtk:=newtkx:=nil				!output token sequence
	nhashhash:=0
	lasttoken:=nil

	while seq do
		case seq.symbol
		when hashsym then
			if nargs then
				seq:=seq.nexttoken
				if seq=nil then lxerror("# at end") fi
				unless seq.flags iand tk_parammask then
					lxerror("# not followed by param")
				end unless
				n:=seq.paramno

				stringify(args^[n],&tk)

				addlisttoken_copy(&newtk,&newtkx,&tk)
			else
				addlisttoken(&newtk,&newtkx,seq)
				newtkx.symbol:=lithashsym				!change to #'
			fi
		when hashhashsym then
			if seq=seqstart then lxerror("## at start") fi
			if nhashhash>=maxhashhash then lxerror("Too many ##") fi
			hhpoints[++nhashhash]:=newtkx

		elsif seq.symbol=namesym and seq.flags iand tk_parammask and nargs then		!args can be () if no "(...)" followed
			n:=seq.paramno
			if seq.nexttoken and seq.nexttoken.symbol=hashhashsym or
			   lasttoken and lasttoken.symbol=hashhashsym then
				addlisttoken_seq(&newtk,&newtkx,args^[n])
			else
				tkexp:=expargs^[n]
				if tkexp=nil then
					tkexp:=expargs^[n]:=scantokenseq(args^[n],macrostack,expanded)
				fi
				addlisttoken_seq(&newtk,&newtkx,tkexp)
			fi

		else
	doother:
			addlisttoken_copy(&newtk,&newtkx,seq)
		esac

		lasttoken:=seq
		seq:=seq.nexttoken
	od

	if nhashhash then
		niltk:=nil
		for i:=1 to nhashhash do
			pastetokens(hhpoints[i],(i<nhashhash | hhpoints[i+1]| niltk))
		od
	fi

	return newtk
end

function strtoken(ref tokenrec lp,int &length)ichar=
!convert token to a string
!return pointer to the string *which is likely to be unterminated*
!return length of the string in 'length'
!(not sure yet if -1 is a possible length, meaning the string is zero-terminated)
!display token contents naturally
!note that caller should copy the string involved as no promises can be 
!made to ownership
	ichar name,s
	tokenrec l
	l:=lp^

	case l.symbol
	when namesym then
	doname:
		length:=l.symptr.namelen
		return l.symptr.name

	when intconstsym,realconstsym then
		length:=l.length


		if getfilenox(&l) then
			return sourcefiletext[getfilenox(&l)]+getnumberoffsetx(&l)
		else
			return pastedtokenlist[l.pasteno]
		fi
	when rawnumbersym then
		length:=l.length
		return l.svalue

	when stringconstsym,wstringconstsym then
		s:=strstring(l.svalue,l.length,length,'"')
		return s

	when charconstsym then
		s:=strstring(l.svalue,l.length,length,'\'')
		return s

	when eolsym then
		if dowhitespace then
			length:=l.length+1
			s:=pcm_alloc(length)
			s^:=10		!'\n'
			memcpy(s+1,l.svalue,l.length)
		else
			length:=1
			return "\n"
		fi
		return s

	when eofsym then
		length:=0
		return ""

	when ktypespecsym, ktypequalsym, klinkagesym, kfnspecsym then
		goto doname

	else
		name:=shortsymbolnames[l.symbol]
		if length:=strlen(name) then
			if name^<>'k' then
				return name
			else
				length:=strlen(symbolnames[l.symbol]+1)
				return symbolnames[l.symbol]+1
			fi
		else
			return ""
		fi
	esac
	return ""
end

function strstring(ichar s,int length,&newlength,quotechar)ichar=
!stringify the string, which means converting control codes to
!escape sequences, and adding optional quotes

	ichar t,u

	t:=u:=pcm_alloc(length*2+4)
	if quotechar then
		u^:=quotechar
		++u
	fi
	convertstringc(s,u,length)
	newlength:=strlen(t)
	if quotechar then
		(t+newlength)^:=quotechar
		++newlength
	fi
	return t
end

int lasttoken=0

global proc emittoken(ref tokenrec lp,ref strbuffer dest,int forcespace=0)=
!display token contents naturally
	int length
	ichar s

	if lp.symbol=eolsym and lasttoken=eolsym then
		return
	fi

	s:=strtoken(lp,length)

	if forcespace or needspace(lasttoken,lp.symbol) then
		gs_char(dest,' ')
	fi

	gs_strn(dest,s,length)


	lasttoken:=lp.symbol
end

global proc showtoken(ref tokenrec lp)=
	static strbuffer buffer
	static ref strbuffer dest=&buffer

	gs_init(dest)
	
	emittoken(lp,dest)
	
print dest.length:"v",,dest.strptr:".*"
end

proc stringify(ref tokenrec seq,dest)=
!stringify single or multiple token sequence, and store result as a single
!string token in dest
	ref char s
	int length,addspace
	static strbuffer buffer
	static ref strbuffer deststr=&buffer

	dest.symbol:=stringconstsym
	dest.nexttoken:=nil

	if seq.nexttoken=nil then		!single
		s:=strtoken(seq,length)
		dest.length:=length
		dest.svalue:=s
		return 
	fi

!now do multiple tokens into one string
	gs_init(deststr)
	lasttoken:=0
	addspace:=0
	while seq do
		emittoken(seq,deststr,forcespace:addspace)
		addspace:=1
		seq:=seq.nexttoken
	od

	dest.length:=length
	dest.svalue:=deststr.strptr
	dest.length:=deststr.length
end

proc pastetokens(ref tokenrec tk, &tknext)=
!tk points into a token sequence
!paste the token at tk with the one at tk.nexttoken, and replace
!tk with the new composite token; tk.nexttoken is removed
!tknext is either nil, or refers to the next pair of tokens to be pasted together;
!there is a problem when tk.nexttoken and tknext coincide, so something needs to
!be done in that case (set tknext to point to tk)

	ref tokenrec tk2
	int length1,length2
	ref char s,t,u
	tokenrec oldtoken,token
	ref char oldlxsptr
	int oldlx_stackindex

	tk2:=tk.nexttoken
	if tk2=tknext then tknext:=tk fi
	tk.nexttoken:=tk2.nexttoken				!lose second token

	if tk.symbol=placeholdersym then
		if tk2.symbol=placeholdersym then			!two placeholders; leave only first
		else										!ph/token; use second
			tk^:=tk2^								!also unlinks the tk2 token
		fi
	elsif tk2.symbol=placeholdersym then			!token/ph; leave only first
	else						!two normal tokens

		s:=strtoken(tk,length1)
		t:=strtoken(tk2,length2)

		u:=pcm_alloc(length1+length2)
		memcpy(u,s,length1)
		memcpy(u+length1,t,length2)
		(u+length1+length2)^:=0

		if npastedtokens>=maxpastedtokens then
			lxerror("Too many pasted tokens")
		fi
		pastedtokenlist[++npastedtokens]:=u

		oldtoken:=nextlx
		oldlxsptr:=lxsptr
		oldlx_stackindex:=lx_stackindex

		lxsptr:=u
		lx_stackindex:=0

		setfileno(0)
		nextlx.lineno:=0
		lexreadtoken()
		token:=nextlx
		lexreadtoken()

		if nextlx.symbol<>eofsym then
!			lxerror("token-paste error")
		fi

		nextlx:=oldtoken
		lxsptr:=oldlxsptr
		lx_stackindex:=oldlx_stackindex

		token.nexttoken:=tk.nexttoken
		setfilenox(&token,0)
		token.pasteno:=npastedtokens

	token.flags ior:=tk_pasted
		tk^:=token
	fi
end

function getifexpr:int=
	int sx
	int x

	lexm()
	x:=evalcondexpr(sx)

	if nextlx.symbol<>eolsym then
		lxerror("#if:eol expected")
	fi

	return x<>0
end

function evalcondexpr(int &sx)i64=
!Main entry point for pp expressions
!Will do conditional ?: expressions here
!Positioned at first symbol of expression, which is in nextlx (if a macro
!it will have been expanded, and this is the first token of that expansion)
	i64 x,y,z
	int sy,sz

	x:=evalorexpr(sx)

	if nextlx.symbol=questionsym then
		lexm()
		y:=evalcondexpr(sy)
		if nextlx.symbol<>colonsym then lxerror(": expected") fi
		lexm()
		z:=evalcondexpr(sz)
		if x then
			sx:=sy
			x:=y
		else
			sx:=sz
			x:=z
		fi
	fi

	return x
end

function evalorexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalandexpr(sx)
	while nextlx.symbol=orlsym do
		lexm()
		y:=evalandexpr(sy)
		x := (x or y|1|0)
	od

	return x
end

function evalandexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaliorexpr(sx)
	while nextlx.symbol=andlsym do
		lexm()
		y:=evaliorexpr(sy)
		x := (x and y|1|0)
	od

	return x
end

function evaliorexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalixorexpr(sx)
	while nextlx.symbol=iorsym do
		lexm()
		x ior:= evalixorexpr(sy)
	od

	return x
end

function evalixorexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaliandexpr(sx)
	while nextlx.symbol=ixorsym do
		lexm()
		x ixor:= evaliandexpr(sy)
	od

	return x
end

function evaliandexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaleqexpr(sx)
	while nextlx.symbol=iandsym do
		lexm()
		x iand:= evaleqexpr(sy)
	od

	return x
end

function evaleqexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalcmpexpr(sx)
	while (opc:=nextlx.symbol)=eqsym or opc=nesym do
		lexm()
		y:=evalcmpexpr(sy)
		case opc
		when eqsym then x := x = y
		when nesym then x := x <> y
		esac
	od

	return x
end

function evalcmpexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalshiftexpr(sx)
	while (opc:=nextlx.symbol)=ltsym or opc=lesym or opc=gesym or opc=gtsym do
		lexm()
		y:=evalshiftexpr(sy)
		case opc
		when ltsym then x := x < y
		when lesym then x := x <= y
		when gesym then x := x >= y
		when gtsym then x := x > y
		esac
	od

	return x
end

function evalshiftexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evaladdexpr(sx)
	while (opc:=nextlx.symbol)=shlsym or opc=shrsym do
		lexm()
		y:=evaladdexpr(sy)
		case opc
		when shrsym then
			x := x>>y
		when shlsym then
			x := x<<y
		esac
	od

	return x
end

function evaladdexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalmulexpr(sx)
	while (opc:=nextlx.symbol)=addsym or opc=subsym do
		lexm()
		y:=evalmulexpr(sy)
		case opc
		when addsym then
			x +:= y
		when subsym then
			x -:= y
		esac
	od

	return x
end

function evalmulexpr(int &sx)i64=
	i64 x,y
	int sy,opc

	x:=evalunaryexpr(sx)
	while (opc:=nextlx.symbol)=mulsym or opc=divsym or opc=remsym do
		lexm()
		y:=evalunaryexpr(sy)
		if y=0 and opc<>mulsym then lxerror("#if:div by zero") fi
		case opc
		when mulsym then
			x *:= y
		when divsym then
			x := x/y
		when remsym then
			x := x rem y
		esac
	od

	return x
end

function evalunaryexpr(int &sx)i64=
	i64 x
	int opc

	case nextlx.symbol
	when addsym, subsym, notlsym, inotsym then
		opc:=nextlx.symbol
		lexm()
		x:=evalunaryexpr(sx)
		case opc
		when addsym then
			return x
		when subsym then
			return -x
		when notlsym then
			return not x
		when inotsym then
			return inot x
		esac
	esac

	return evalterm(sx)
end

function evalterm(int &sx)i64=
	i64 res
	int lb

	sx:=1
	case nextlx.symbol
	when namesym then
		case nextlx.symptr.symbol
		when kdefinedsym then
			noexpand:=1
			lb:=0
			lexm()
			if nextlx.symbol=lbracksym then
				lb:=1;
				lexm()
			fi
			if nextlx.symbol<>namesym then lxerror("defined?") fi
			res:=nextlx.symptr.nameid=macroid
			lexm()
			if lb then
				if nextlx.symbol<>rbracksym then lxerror("')' expected") fi
				lexm()
			fi
			noexpand:=0
		when ksizeofsym then
			lexm()
			if nextlx.symbol<>lbracksym then lxerror("'(' expected") fi
			lexm()
			if nextlx.symbol<>namesym then lxerror("name expected") fi
			case nextlx.symptr.symbol
			when ktypespecsym then
				res:=typespecsizes[nextlx.symptr.subcode]
			else
				lxerror("sizeof2")
			esac
			lexm()
			if nextlx.symbol<>rbracksym then lxerror("')' expected") fi
			lexm()
	
		else
!lxerror("Undefined macro name in cpp expr")
			lexm()
			return 0
		esac
	when intconstsym then
		res:=nextlx.value
		lexm()
	when charconstsym then
		if nextlx.length=0 then
			res:=0
		else
			res:=nextlx.svalue^
		fi
		lexm()
	when lbracksym then
		lexm()
		res:=evalcondexpr(sx)
		if nextlx.symbol<>rbracksym then
			lxerror(") expected")
		fi
		lexm()
	else
	printsymbol(&nextlx)
	printstrn(nextlx.svalue,nextlx.length); println
		lxerror("evalterm?")
	esac

	return res
end

function getifdef:int=
!just read ifdef/ifndef
!read following name and return 1 if name is macro, or 0 if not
	int res
	ref strec d

	noexpand:=1
	lexreadtoken()
	noexpand:=0
	if nextlx.symbol<>namesym then lxerror("Name expected") fi
	d:=nextlx.symptr
	res:=0
	if d.nameid=macroid then
		res:=1
	elsif d.symbol=predefmacrosym then
		res:=1
	fi

	lexreadtoken()
	if nextlx.symbol<>eolsym then lxerror("EOL expected") fi

	return res
end

function skipcode:int=
!skip false branch of #if etc until matching #elif/else/endif
!return dir-code of that closing directive
	int level,dir
	ref byte pp

	level:=0						!count nested #if levels

	do
		fastreadtoken()

		case nextlx.symbol
		when lexhashsym then
			dir:=getlexdirective()
			case dir
			when ifdir, ifdefdir, ifndefdir then
				++level
			when elifdir, elsedir then
				if level=0 then
					return dir
				fi
			when endifdir then
				if level=0 then
					return dir
				fi
				--level
			esac
		when eofsym then
			lxerror("#if:Unexpected eof")
		esac
	od
	return 0
end

proc freetokens(ref tokenrec tk)=
	ref tokenrec nexttk

	while tk do
		nexttk:=tk.nexttoken
		tk:=nexttk
	od
end

global proc fastreadtoken=
!read next token into nextlx
	int c,csum,hsum,commentseen,dodir,j
	ref char pstart,p
	ichar ss

!	nextlx.subcodex:=0

	doswitch c:=lxsptr++^
	when '#' then			!
		p:=lxsptr-2
		dodir:=0
		while p>=lxstart do
			case p^
			when lf then		!# is first thing on a line
				dodir:=1
				exit
			when tab,' ' then	!might have leading white space
			else
				exit			!assume different hash symbol
			esac
			--p
		od
		if dodir or p<lxstart then
			nextlx.symbol:=lexhashsym
		return

		elsif lxsptr^='#' then
			++lxsptr
		fi

	when '/' then
		case lxsptr^
		when '/' then					!comment to 
			readlinecomment()
		when '*' then
			readblockcomment()
		esac
	
	when '\'' then
		lxreadstring('\'',0)

	when '"' then
		lxreadstring('"',0)

	when cr then
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0
		++lxsptr				!skip lf
	when lf then			!only lfs not preceded by cr
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0

	when 0 then
		--lxsptr
		if lx_stackindex then
			unstacksourcefile()
		else
			nextlx.symbol:=eofsym
			return
		fi

	when 12 then
	else
	end doswitch
end

function alloctoken:ref tokenrec=
	ref tokenrec tk
	tk:=pcm_alloc(tokenrec.bytes)
	return tk
end

function alloctokenz:ref tokenrec=
	ref tokenrec tk
	tk:=pcm_alloc(tokenrec.bytes)
	tk.nexttoken:=nil
	return tk
end

proc expandpredefmacro(int pdmcode,ref tokenrec tk,int lineno)=
	[256]char str
	static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	rsystemtime tm
	ichar s
	int fileno

	if noexpand then
		return
	fi

	case pdmcode
	when pdm_date then
		os_getsystime(&tm)

		fprint @&.str, "#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

		tk.symbol:=stringconstsym
		tk.svalue:=pcm_copyheapstring(&.str)

	when pdm_time then
		os_getsystime(&tm)

		fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

		tk.symbol:=stringconstsym
		tk.svalue:=pcm_copyheapstring(&.str)
	when pdm_file then
		tk.symbol:=stringconstsym
		fileno:=getfilenox(tk)
		if fileno=0 then fileno:=sfileno fi
		if sfileno then
			tk.svalue:=sourcefilenames[sfileno]
		else
			tk.svalue:="(File not available)"
		fi
	when pdm_func then
		tk.symbol:=stringconstsym
		if currproc then
			tk.svalue:=currproc.name
		else
			tk.svalue:="???"
		fi
	when pdm_line then
		tk.symbol:=intconstsym
		tk.value:=lineno
	when pdm_stdc then
		tk.symbol:=intconstsym
		tk.value:=1
	when pdm_mcc then
		tk.symbol:=intconstsym
		tk.value:=1
	when pdm_mcci then
		tk.symbol:=intconstsym
		tk.value:=pci_target
	else
		println pdmcode
		lxerror("PDM")
	esac

	if tk.symbol=stringconstsym then
		tk.length:=strlen(tk.svalue)
		tk.subcode:=trefchar
	else
		tk.subcode:=ti32
		s:=pcm_alloc(16)
		getstrint(tk.value,s)
		tk.length:=strlen(s)
		if npastedtokens>=maxpastedtokens then
			lxerror("2:Too many pasted tokens")
		fi
		pastedtokenlist[++npastedtokens]:=s
		setfilenox(tk,0)
		tk.pasteno:=npastedtokens
	fi
end

proc dopragmadir=
	lexm()
	if nextlx.symbol=namesym then
		if memcmp(nextlx.symptr.name,"pack",4)=0 then
			lexm()
			if nextlx.symbol<>lbracksym then lxerror("'(' expected") fi
			lexm()
			if nextlx.symbol=intconstsym then
				case nextlx.value
				when 1 then
					structpadding:=0
				else
					goto finish
					lxerror("Only pack(1) or () allowed")
				esac
				lexm()
			elsif nextlx.symbol=rbracksym then
				structpadding:=1
			fi
		elsif memcmp(nextlx.symptr.name,"module",6)=0 then
			addbuildinfo('M')
!		elsif memcmp(nextlx.symptr.name,"except",6)=0 then
!			addbuildinfo('X')
		elsif memcmp(nextlx.symptr.name,"header",6)=0 then
			addbuildinfo('H')
		elsif memcmp(nextlx.symptr.name,"link",4)=0 then
			addbuildinfo('L')
		fi
	fi
finish:
	while nextlx.symbol<>eolsym and nextlx.symbol<>eofsym do lexm() od
end

proc addbuildinfo(int code)=
	ichar file
	int j

	lexm()
	if nextlx.symbol<>stringconstsym then lxerror("Str expected") fi
	file:=pcm_copyheapstring(nextlx.svalue)

	case code
	when 'M' then
		if npmodules>=maxpmodule then lxerror("TMM")  fi
		pmodulelist[++npmodules]:=file

	when 'H' then
		if npheaders>=maxpheader then lxerror("TMH")  fi
		pheaderlist[++npheaders]:=file
	when 'L' then
		if nplibs>=maxplib then lxerror("TMLM") fi
		pliblist[++nplibs]:=file
	esac
	lexm()
end

function needspace(int a,b)int=
	ichar aname, bname

	if a=0 then return 0 fi			!first token

	aname:=shortsymbolnames[a]
	bname:=shortsymbolnames[b]

	case bname^
	when 'n','k' then
		case aname^
		when 'n','k' then
			return 1
		esac
	when '-','+' then
		case aname^
		when '-','+' then
			return 1
		esac
	esac

	return 0
end

global proc dospecialinclude=
	stacksourcefile(mcchdr,1)
	if dheaderfile then
		stacksourcefile(dheaderfile,1)
	fi
end

proc setnumberoffset(int offset)=
!store offset into nextlx.numberoffset
!except that top byte is msb of fileno
	nextlx.numberoffset:=(nextlx.numberoffset iand 0xFF000000) ior (offset iand 0xFFFFFF)
end

proc setfileno(int fileno)=
	nextlx.fileno:=fileno iand 255
	nextlx.numberoffset := (nextlx.numberoffset iand 0xFFFFFF) ior((fileno iand 0xFF00)<<16)
end

proc setfilenox(ref tokenrec tk,int fileno)=

	tk.fileno:=fileno iand 255
	tk.numberoffset := (tk.numberoffset iand 0xFFFFFF) ior (fileno iand 0xFF00)<<16
end

function getfileno:int=
	return (nextlx.numberoffset>>24)<<8 ior nextlx.fileno
end

function getfilenox(ref tokenrec tk)int=
	return (tk.numberoffset>>24)<<8 ior tk.fileno
end

function getnumberoffsetx(ref tokenrec tk)int=
	return tk.numberoffset iand 0xFFFFFF
end

global proc freehashtable=
!free the user name entries in the hash table
!leave reserved words etc alone
	ref strec d,e,f

	for i:=0 to hstmask do
		d:=hashtable^[i]
		if d.name and d.symbol=namesym then
			if d.nameid=macroid then
				freetokens(d.tokenlist)
			fi
			f:=d.nextdupl
			while f do
!				freestentry(f)
				e:=f.nextdupl
				pcm_free(f,strec.bytes)
				f:=e
			od
			pcm_clearmem(hashtable^[i],strec.bytes)
		elsif d.name then
			d.nextdupl:=nil
		fi
	od
end

proc regenlookup(ref strec d)=
	int j, wrapped,length
	ref strec e

	j:=gethashvalue(d.name,d.namelen) iand hstmask
	wrapped:=0

	do
		e:=hashtable^[j]
		length:=e.namelen

		if not length then
			PCM_FREE(HASHTABLE^[J],STREC.BYTes)
			hashtable^[j]:=d
			++nhstsymbols
			return
		fi

		if length=d.namelen then	!match on length
			if memcmp(e.name,d.name,length)=0 then	!match
				lxerror("regenhst dupl?")
			fi
		fi

		if ++j>=hstsize then
			if wrapped then
				abortprogram("REGENHST FULL?")
			fi
			wrapped:=1
			j:=0
		fi
	od
end

proc newhashtable=
	ref[0:]ref strec oldhashtable
	int oldhstsize
	ref strec d

!remember old hst
	oldhashtable:=hashtable
	oldhstsize:=hstsize
!generate new, blank hst
	hstsize*:=2
	hstmask:=hstsize-1
	nhstsymbols:=0
	hstthreshold:=(6*hstsize)/10

	hashtable:=pcm_alloc(hstsize*(ref void.bytes))

	for i:=0 to hstmask do
		hashtable^[i]:=pcm_allocz(strec.bytes)
	od

!now, rehash all existing hashentries
	for i:=0 to oldhstsize-1 do
		d:=oldhashtable^[i]
		if d.name then
			regenlookup(d)
		fi
	od

	pcm_free(oldhashtable,oldhstsize*(ref void.bytes))
end

proc old_readrealnumber(ref char pstart,intstart, int intlen, base)=
!'e' or '.' has been encountered, possibly after a string of digits
!intstart points to int prefix, (or to "." if there was no prefix, then intlen=0)
!lxsptr still points at '.', 'e' or 'E' (might be 'p' or 'P' for hex base)
!read entire numbers, convert to real value in nextlx.xvalue
	ref char fractstart
	int fractlen,expon,i,c,badexpon
	real basex,x,expbase
	const maxrealdigits=500
	[maxrealdigits]char realstr

	fractstart:=nil
	fractlen:=0
	expon:=0

	if lxsptr^='.' then		!read
		fractstart:=++lxsptr
		fractlen:=scannumber(base)-fractstart
	fi
	badexpon:=0

	case lxsptr^
	when 'e','E' then
		if base<>16 then
			++lxsptr
			expon:=readexponent(badexpon)
		fi
	when 'p','P' then
		if base=16 then
			++lxsptr
			expon:=readexponent(badexpon)
		fi
	esac

	if badexpon then
		--lxsptr
		readalphanumeric(pstart)
		return
	fi

	case lxsptr^
	when 'f','F','l','L' then
		++lxsptr
	else
		if alphamap[lxsptr^] then
			readalphanumeric(pstart)
			return
		fi
	esac

	if intlen+fractlen>maxrealdigits then
		lxerror("Real too long")
	fi
	if intlen then
		memcpy(&realstr,intstart,intlen)
	fi
	if fractlen then
		memcpy(&realstr[1]+intlen,fractstart,fractlen)
	fi

	expbase:=basex:=base

	if base=10 then
		expon-:=fractlen
	else
		expon-:=fractlen*4				!each hex digit is 4 binary bits
		expbase:=2.0
	fi

	x:=0.0

	for i:=1 to intlen+fractlen do		!digits already range-checked
		c:=realstr[i]
		if c>='0' and c<='9' then
			x:=x*basex+(c-'0')
		elsif c>'a' then
			x:=x*basex+c-'a'+10
		else
			x:=x*basex+c-'A'+10
		fi
	od

	if expon>=0 then
		to expon do
			x*:=expbase
		od
	else
		to -expon do
			x/:=expbase
		od
	fi

	nextlx.symbol:=realconstsym
	nextlx.subcode:=tr64
	nextlx.xvalue:=x

	setnumberoffset(intstart-lxstart)
	nextlx.length:=lxsptr-intstart
end

global function issimpleconstmacro(ref strec m)int=
!return 1 if a d is a macro defined as simple int or float
!then it will not be expanded
	ref tokenrec tk
	static []ichar specialnames=("stdin","stdout","stderr")

!don't expand special names
	for i to specialnames.len do
		if eqstring(specialnames[i],m.name) then
			return 2
		fi
	od

	tk:=m.tokenlist

	if tk and tk.nexttoken=nil then
		if tk.symbol=intconstsym or tk.symbol=realconstsym then
			return 1
		fi
	fi
	return 0
end
=== cc_parse.m 0 0 7/21 ===
!Parse C Code

!const needcompoundblock=1
const needcompoundblock=0

ref strec ist_symptr


const maxtypemods=32
[maxnestedloops]byte looptypestack		!contains either 'L' or 'S' (loop or switch)
int loopindex							!current level of nested loop/switch blocks
[maxnestedloops]ref caserec casevaluestack		!linked list of case values for current switch

byte ingeneric=0

proc readmodule=
	int linkage,m,mbase,commaseen,wasdef
	unit p
	ref strec d
	ref paramrec pm
	int t,nitems,wasenum

	while lx.symbol<>eofsym do
		nitems:=0
		case lx.symbol
		when semisym then
			serror("Extra semicolon 2")
		esac
		wasenum:=lx.symbol

		mbase:=readdeclspec(stmodule,linkage)
		commaseen:=0

		docase lx.symbol
		when namesym, mulsym, lbracksym then
			++nitems

			m:=readtype(stmodule,d,mbase,pm)

			if d=nil then
				serror("Var name expected")
			fi

			if linkage=typedef_ss then
				if pm then
					m:=createprocmode(m,pm)
				fi
				d:=createtypedef(stmodule,d,m)
			elsif pm then
	readfn:
				if lx.symbol=lcurlysym and commaseen then serror("fn def after comma") fi

				d:=readfunction(d,m,linkage,pm,wasdef)
				if wasdef then exit fi			!can't have comma-separate fn defs

			elsif ttbasetype[m]=tproc then
				pm:=ttparams[m]
				m:=tttarget[m]
				goto readfn

			else
				d:=readmodulevar(d,m,linkage)
			fi

			case lx.symbol
			when commasym then			!read next item
				commaseen:=1
				lex()
			else
				skipsymbol(semisym)
				exit
			esac
		else
			case ttbasetype[mbase]
			when tenum, tstruct, tunion then		!assume defining a [part]type only
				skipsymbol(semisym)
				exit
			when ti32 then				!allow for now, as it migt be an enum decl with no name
				skipsymbol(semisym)
				exit
			else
				serror_s("Decl error #",typename(mbase))
			esac
		end docase
	od
end

global function parsemodule:int=
	int size,t
	ref strec owner
	real tsecs

	if fverbose=3 then println "Parsing:",inputfile fi

	loopindex:=ingeneric:=0
	ist_symptr:=nil
	memset(&casevaluestack,0,casevaluestack.bytes)

	startlex("PARSETEST",mainfileno)
	owner:=stmodule
	currproc:=nil
	loopindex:=0

	lex()

!!=========================================
!t:=os_clock()
!int ntokens:=0
!
!!	repeat
!!		lex()
!!		++ntokens
!!	until lx.symbol=eofsym
!!
!	repeat
!		lexreadtoken()
!		++ntokens
!	until nextlx.symbol=eofsym
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
!

	readmodule()

	endlex()
	return 1
end

function readdeclspec(ref strec owner,int &linkage)int=
!At first symbol of a declspec, or possible declspec
!read declspec and basetype
!return typecode for basetype, and linkage (static etc)
!if no declspec follows (usually eof) returns 0

	record declrec=
		i32 typeno				!not set, int, float, char, struct, union, enum etc
		byte isconst				!0, or 1 when const used (more than 1 allowed)
		byte isvolatile				!0, or 1 when volatile used
		byte isrestrict
		byte linkage				!0, or static_ss etc; only one allowed
		byte isinline				!1 when inline used
		byte isshort				!1 when short used
		byte islong					!1 when long used (not short or long long)
		byte isllong				!1 when long long used (islong set to 0)
		byte issigned				!not set, signed
		byte isunsigned				!not set, unsigned
		byte isusertype				!1 if basetype set completely from typedef
									!so isshort/long etc or other basetype not allowed
	end
	declrec d
	unit p
	int t,mod,m,fstruct
	ref paramrec pm
	ref strec e

	memset(&d,0,d.bytes)
!clear d
	d.typeno:=tnotset

	fstruct:=mod:=0

	doswitch lx.symbol
    when kstdtypesym then
        d.typeno:=lx.subcode
        lex()

    when ktypespecsym then
		switch lx.subcode
		when ts_int, ts_char, ts_float, ts_double, ts_bool, ts_void then
			if d.typeno<>tnotset then
				if fstruct then checksymbol(semisym)
				else goto tserror
				fi
			fi
			d.typeno:=typespectypes[lx.subcode]

		when ts_short then
			if d.isshort or d.islong or d.isllong then goto tserror fi
			d.isshort:=mod:=1
		when ts_long then
			if d.isllong or d.isshort then goto tserror
			elsif d.islong then
				d.islong:=0
				d.isllong:=1
			else
				d.islong:=1
			fi
			mod:=1

		when ts_signed then
			if d.issigned or d.isunsigned then goto tserror fi
			d.issigned:=mod:=1
		when ts_unsigned then
			if d.issigned or d.isunsigned then goto tserror fi
			d.isunsigned:=mod:=1
		else

	tserror:
			serror_s("declspec/ts #",typespecnames[lx.subcode])
		end switch
		lex()

	when ktypequalsym then
		case lx.subcode
		when const_qual then
			d.isconst:=1
		when volatile_qual then d.isvolatile:=1
		when restrict_qual then d.isrestrict:=1
		esac
		lex()

	when klinkagesym then
		if d.linkage then serror("Dual storage spec") fi
		d.linkage:=lx.subcode
		lex()

	when kfnspecsym then
		case lx.subcode
		when inline_fnspec then
			d.isinline:=1
		esac
		lex()
	when kstructsym,kunionsym then
		if d.typeno<>tnotset then serror("struct?") fi
		d.typeno:=readstructdecl(owner)
		d.isusertype:=1
		fstruct:=1

	when kenumsym then
		if d.typeno<>tnotset then serror("enum?") fi
		readenumdecl(owner)
		d.typeno:=ti32			!disregard enum 'type'; just use int
		d.isusertype:=1

	when namesym then			!should resolve to see if a user-type ...
								! ... unless a basetype already seen
		if d.typeno=tnotset and (m:=isusertype(owner))<>tnotset then
			if mod then			!unsigned etc without proper base type; assume name is not part o it
				d.typeno:=ti32
				exit
			fi
			d.typeno:=m
			d.isusertype:=1
			lex()
		else
			if d.typeno=tnotset and not mod then
				serror_s("Implicit decls not allowed: #",lx.symptr.name)
			fi

			if d.typeno=tnotset then d.typeno:=ti32 fi
			exit
		fi

	else
		exit
	end doswitch

	t:=(d.typeno<>tnotset|d.typeno|ti32)

	if not d.isusertype then				!otherwise everything should be set up
		case t
		when ti32 then
			if d.isshort then
				t:=(d.isunsigned|tu16|ti16)
			elsif d.islong then
				t:=(d.isunsigned|tu32|ti32)
			elsif d.isllong then
				t:=(d.isunsigned|tu64|ti64)
			elsif d.isunsigned then
				t:=tu32
			fi
		when ti8 then
			if d.isshort or d.islong or d.isllong then serror("char decl?") fi
			t:=(d.isunsigned|tu8|ti8)
		when tr64 then
			if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("dbl decl?") fi

		else
			if mod then serror("declspec/float") fi
		esac
	fi

	if d.isconst then
		t:=createconstmode(t)
	fi

	linkage:=d.linkage
	return t
end

function istypestarter:int=
!return 1 when current symbol could start a type-spec
	ref strec d

	switch lx.symbol
	when ktypespecsym, kstdtypesym then
		return 1
	when ktypequalsym then
!	return lx.subcode=const_qual
		return 1
	when namesym then
		d:=resolvename((currproc|currproc|stmodule),lx.symptr,ns_general,currblockno)
		if d then
			lx.symptr:=d
			return d.nameid=typeid
		fi
	when kstructsym,kunionsym,kenumsym then
		return 1
	end switch
	return 0

end

function istypestarter_next:int=
!return 1 when current symbol could start a type-spec
	ref strec d

	switch nextlx.symbol
	when ktypespecsym, kstdtypesym then
		return 1
	when ktypequalsym then
!	return lx.subcode=const_qual
		return 1
	when namesym then
		d:=resolvename((currproc|currproc|stmodule),nextlx.symptr,ns_general,currblockno)
		if d then
			nextlx.symptr:=d
			return d.nameid=typeid
		fi
	when kstructsym,kunionsym,kenumsym then
		return 1
	end switch
	return 0

end

function readexpression:unit=
	unit p, ulist, ulistx
	int t

	case nextlx.symbol
	when  semisym,rbracksym then
		return readterm()
	esac

	p:=readassignexpr()

	if lx.symbol=commasym then		!
		ulist:=ulistx:=nil
		do
			addlistunit(&ulist,&ulistx,p)
			exit when lx.symbol<>commasym
			lex()
			p:=readassignexpr()
		od
		p:=createunit1(jexprlist,ulist)
		if ulistx then
			p.mode:=ulistx.mode
		fi

		return p
	fi
	return p
end

function readassignexpr:unit=
	unit p,q,r
	int opc,oldpmode

	case nextlx.symbol
	when commasym, semisym,rbracksym then
		return readterm()
	when assignsym then
		p:=readterm()
		opc:=lx.symbol
		goto gotp
	esac

	p:=readcondexpr()

	switch opc:=lx.symbol
	when assignsym, multosym, divtosym, remtosym, addtosym, subtosym,
			shltosym, shrtosym, iandtosym, ixortosym, iortosym then
	gotp:
		lex()
		oldpmode:=p.mode
		checklvalue(p,1)
		q:=readassignexpr()
		if ttisref[p.mode] then
			return createassignopref(opc,p,q)
		fi

		q:=coercemode(q,oldpmode)
		if ttconst[oldpmode] then
			terror("Modifying read-only var")
		fi

		if p.tag=jptr and p.a.tag=jconst then
			terror("Modifying constant?")
		fi


		r:=createunit2(symboltojtag[opc],p,q)

		r.mode:=oldpmode
		return r
	end switch

	return p
end

function readcondexpr:unit=
	unit x,y,pcond
	int s,t,u

	pcond:=readorlexpr()

	if lx.symbol=questionsym then
		coercecond(pcond)

		lex()
		x:=readexpression()
		skipsymbol(colonsym)
		y:=readcondexpr()

		if u:=dominantmode[s:=ttbasetype[x.mode],t:=ttbasetype[y.mode]] then			!were both numeric
			x:=coercemode(x,u)
			y:=coercemode(y,u)
			if pcond.tag=jconst and x.tag=jconst and y.tag=jconst then
				return (pcond.value|x|y)
			fi

		elsif s=tref and t=tref then
			u:=x.mode
		elsif s=tref and t=ti32 and y.tag=jconst and y.value=0 then
			u:=x.mode
			coercemode(y,u)
		elsif s=ti32 and t=tref and x.tag=jconst and x.value=0 then
			u:=y.mode
			coercemode(x,u)
		elsif s=tstruct and t=tstruct then
			u:=x.mode
		elsif s=tunion and t=tunion then
			u:=x.mode
		elsif s=t=tvoid then
			u:=tvoid
		else
	CPL strmode(x.mode),strmode(y.mode)
			terror("?: incompatible types")
		fi

		pcond:=createunit3(jifx,pcond,x,y)
		pcond.mode:=u
	fi

	return pcond
end

function readorlexpr:unit=
	unit x,y

	x:=readandlexpr()

	while lx.symbol=orlsym do
		lex()
		y:=readandlexpr()
		coercecond(x)
		coercecond(y)

		if x.tag=jconst and y.tag=jconst then
			x.value := (x.value or y.value|1|0)
			nextloop
		fi
		x:=createunit2(jorl,x,y)
		x.mode:=ti32
	od

	return x
end

function readandlexpr:unit=
	unit x,y

	x:=readiorexpr()

	while lx.symbol=andlsym do
		lex()
		y:=readiorexpr()
		coercecond(x)
		coercecond(y)

		if x.tag=jconst and y.tag=jconst then
			x.value := (x.value and y.value|1|0)
			nextloop
		fi
		x:=createunit2(jandl,x,y)
		x.mode:=ti32
	od

	return x
end

function readiorexpr:unit=
	unit x,y
	int u

	x:=readixorexpr()

	while lx.symbol=iorsym do
		lex()
		y:=readixorexpr()

		if u:=dominantmode[ttbasetype[x.mode],ttbasetype[y.mode]] then			!were both numeric
			if u>=tr32 then terror("float|float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			terror("invalid | operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64 then
				x.value ior:= y.value
				nextloop
			esac
		fi
		x:=createunit2(jior,x,y)
		x.mode:=u
	od

	return x
end

function readixorexpr:unit=
	unit x,y
	int u

	x:=readiandexpr()

	while lx.symbol=ixorsym do
		lex()
		y:=readiandexpr()

		if u:=dominantmode[ttbasetype[x.mode],ttbasetype[y.mode]] then			!were both numeric
			if u>=tr32 then terror("float^float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			terror("invalid ^ operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64 then
				x.value ixor:= y.value
				nextloop
			esac
		fi
		x:=createunit2(jixor,x,y)
		x.mode:=u
	od

	return x
end

function readiandexpr:unit=
	unit x,y
	int u

	x:=readeqexpr()

	while lx.symbol=iandsym do
		lex()
		y:=readeqexpr()

		if u:=dominantmode[ttbasetype[x.mode],ttbasetype[y.mode]] then			!were both numeric
			if u>=tr32 then terror("float&float") fi
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		else
			cpl strmode(x.mode)
			cpl strmode(y.mode)
			terror("invalid & operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64 then
				x.value iand:= y.value
				nextloop
			esac
		fi
		x:=createunit2(jiand,x,y)
		x.mode:=u
	od

	return x
end

function readeqexpr:unit=
	unit x,y
	int opc,s,t,u,ss,tt

	x:=readrelexpr()

	while (opc:=lx.symbol)=eqsym or opc=nesym do
		lex()
		y:=readrelexpr()

		if u:=dominantmode[s:=ttbasetype[x.mode],t:=ttbasetype[y.mode]] then			!were both numeric
			x:=coercemode(x,u)
			y:=coercemode(y,u)
		elsif s=tref and t=tref then
			if (ss:=tttarget[x.mode])<>(tt:=tttarget[y.mode]) then
				if ss<>tvoid and tt<>tvoid then
					if not checkpointertypes(x.mode,y.mode,1) then	!'hard'
						terror("Comparing distinct pointers/eq")
					fi
				fi
			fi
		elsif s=tref and t=ti32 then
			if y.tag<>jconst or y.value<>0 then
				terror("Can't compare pointer to int")
			fi
		elsif s=ti32 and t=tref then
			if x.tag<>jconst or x.value<>0 then
				terror("Can't compare pointer to int2")
			fi
		else
CPL =U
			terror("invalid == operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64,tu32,tu64,0 then			!0 when ref/ref ref/int int/ref
				if opc=eqsym then
					x.value := x.value = y.value
				else
					x.value := x.value <> y.value
				fi
				nextloop
			esac
		fi
		x:=createunit2(symboltojtag[opc],x,y)
		x.mode:=ti32
	od

	return x
end

function readrelexpr:unit=
	unit x,y
	int opc,s,t,u
	i64 a,b,c
	u64 aa,bb,cc

	x:=readshiftexpr()

	while (opc:=lx.symbol)=ltsym or opc=lesym or opc=gesym or opc=gtsym do
		lex()
		y:=readshiftexpr()

		if u:=dominantmode[s:=ttbasetype[x.mode],t:=ttbasetype[y.mode]] then			!were both numeric

			x:=coercemode(x,u)
			y:=coercemode(y,u)
		elsif s=tref and t=tref then
			if not checkpointertypes(x.mode,y.mode,1) then		!use 'hard' mode
				terror("Comparing distinct pointers/rel")
			fi
		else
			terror("invalid rel operands")
		fi

		if x.tag=jconst and y.tag=jconst then
			a:=x.value; b:=y.value
			case u
			when ti32,ti64 then
				case opc
				when ltsym then c:=a<b
				when lesym then c:=a<=b
				when gesym then c:=a>=b
				else            c:=a>b
				esac
				x.value:=c
				nextloop
			when tu32,tu64 then
				aa:=x.value; bb:=y.value
				case opc
				when ltsym then cc:=aa<bb
				when lesym then cc:=aa<=bb
				when gesym then cc:=aa>=bb
				else            cc:=aa>bb
				esac
				x.value:=cc
				nextloop
			esac
		fi

		x:=createunit2(symboltojtag[opc],x,y)
		x.mode:=ti32
	od

	return x
end

function readshiftexpr:unit=
	unit x,y
	int opc,u

	x:=readaddexpr()

	while (opc:=lx.symbol)=shlsym or opc=shrsym do
		lex()
		y:=readaddexpr()

		coercebasetype(x)
		unless (u:=ttbasetype[x.mode])>=tfirstint and u<=tlastint then
			terror("shift:Not an int")
		end unless
		y:=coercemode(y,ti32)
!
		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64 then
				if opc=shlsym then
					x.value := x.value << y.value
				else
					x.value := x.value >> y.value
				fi
				nextloop
			when tu32,tu64 then
				if opc=shlsym then
					x.uvalue := x.uvalue << y.value
				else
					x.uvalue := x.uvalue >> y.value
				fi
				nextloop
			esac
		fi
		x:=createunit2((opc=shlsym|jshl|jshr),x,y)
		x.mode:=u
	od

	return x
end

function readaddexpr:unit=
	unit p,q
	int opc

	p:=readmulexpr()

	while (opc:=lx.symbol)=addsym or opc=subsym do
		lex()
		q:=readmulexpr()

		if opc=addsym then
			p:=createaddop(p,q)
		else
			p:=createsubop(p,q)
		fi
	od

	return p
end

function readmulexpr:unit=
	unit p,q
	int opc

	p:=readterm()

	while (opc:=lx.symbol)=mulsym or opc=divsym or opc=remsym do
		lex()
		q:=readterm()
		case opc
		when mulsym then
			p:=createmulop(p,q)
		when divsym then
			p:=createdivop(p,q)
		when remsym then
			p:=createremop(p,q)
		esac
	od

	return p
end

function readterm:unit=
	unit p, q
	int t,u,opc,shift,newlen,slength,tbase,fwide,newmode, tag
	ref char pbyte
	i64 a
	ref strec d
	ichar ss,s
	ref paramrec pm

	switch lx.symbol
	when intconstsym, realconstsym then
		p:=createconstunit(lx.value,lx.subcode)

		lex()
	when namesym then
		if lx.symptr.nameid<=macroid then
			d:=resolvename((currproc|currproc|stmodule),lx.symptr,ns_general,currblockno)
			if d=nil then
				serror_s("Undefined name ""#""", getstname(lx.symptr))
			fi
		else
			d:=lx.symptr
		fi

!		d.used:=1
!		if d.used<255 then ++d.used fi
		case d.nameid
		when enumid then
			p:=createconstunit(d.index,ti32)
		when procid then
			if nextlx.symbol<>lbracksym then
				p:=createunit0(jfuncname)
				p.def:=d
				p.mode:=createrefmode(createprocmode(d.mode,d.paramlist))
!			p.mode:=createprocmode(d.mode,d.paramlist)
			else
				goto doname
			fi

		else
	doname:
			p:=createname(d)
			p.mode:=t:=d.mode
			if ttbasetype[t]=tarray then
				p.alength:=ttlength[t]
				p:=createaddrofop(p)
				p.mode:=createrefmode(tttarget[t])
			elsif d.nameid<>procid and ttsize[t]<4  then
				fixmemopnd(p)
			elsif d.nameid=paramid then
			fi
		esac
		p.lineno:=lx.lineno
		lex()

	when stringconstsym,wstringconstsym then
		fwide:=lx.symbol=wstringconstsym
		s:=lx.svalue
		slength:=lx.length
		while nextlx.symbol=stringconstsym do		!combine consecutive strings
			newlen:=slength+nextlx.length
			ss:=pcm_alloc(newlen+1)
			memcpy(ss,s,slength)
			memcpy(ss+slength,nextlx.svalue,nextlx.length)
			(ss+newlen)^:=0
			s:=ss
			slength:=newlen
			lex()
		od
		if fwide then
			p:=createwstringconstunit(cast(s),slength)
		    p.wslength:=slength
			p.mode:=trefwchar
		else
			p:=createstringconstunit(s,slength)
		    p.slength:=slength
			p.mode:=trefchar

		fi

		lex()

	when charconstsym then
		a:=0
		shift:=0
		pbyte:=lx.svalue
		if lx.length>8 then serror("char const too long") fi

		to lx.length do
			a:=a ior u64(pbyte^)<<shift
			shift+:=8
			++pbyte
		od
		p:=createconstunit(a,(lx.length<=4|ti32|ti64))
		lex()

	when addsym then
		lex()
		p:=readterm()

	when subsym then
		lex()
		p:=createnegop(readterm())

	when notlsym then
		lex()
		p:=readterm()
		coercecond(p)
		p:=createunit1(jnotl,p)
		p.mode:=ti32

		if p.a.tag=jnotl and p.a.a.tag=jnotl then
			p.a:=p.a.a.a
		fi

	when inotsym then
		lex()
		p:=createinotop(readterm())

	when iandsym then			!&
		lex()
!&* cancel, so detect this early to avoid more complicated code, which also
!has a bug when following term is an array that decays to a pointer; it ends up
!with an incorrect number of ptrs (one too many I think). The .alength trick
!doesn't work when the array is unbounded as in (*A)[]
!However, detecting &* doesn't cover &(*X) for example
!I need to have .alength plus also an array indicator. Fortunately array pointers
!and the use of &* mainly occur in my generated code
!
		if lx.symbol=mulsym then
			lex()
			p:=readterm()
		else
			p:=createaddrofop(readterm())
		fi

	when andlsym then			!&&
		serror("rt/&&label")

	when mulsym then			!*
		lex()
		p:=createptrop(readterm())

	when incrsym, decrsym then			!*
		opc:=symboltojtag[lx.symbol]
		lex()
		p:=createincrop(opc,readterm())

	when abssym then
		lex()
		skipsymbol(lbracksym)
		p:=createabsop(readexpression())
		skipsymbol(rbracksym)

	when lbracksym then			!(
		lex()
		if istypestarter() then
			t:=readcasttype(d,0,pm)
			skipsymbol(rbracksym)
			if lx.symbol=lcurlysym then
				serror("rt/compound lit")
			else
				p:=docast(readterm(),t)
			fi
		else
			p:=readexpression()
			skipsymbol(rbracksym)
		fi
	when ksizeofsym then
		if lx.subcode then
			lex()
			if lx.symbol=lbracksym then		!possible type
				lex()
				if istypestarter() then
					t:=readcasttype(d,0,pm)
					skipsymbol(rbracksym)
					p:=createconstunit(ttlength[t],tu64)
				else
					p:=readexpression()
					skipsymbol(rbracksym)
					p:=createsizeofop(p,1)
				fi
			else
				p:=createsizeofop(readterm(),1)
			fi
		else
			lex()
			if lx.symbol=lbracksym then		!possible type
				if istypestarter_next() then
					lex()
					t:=readcasttype(d,0,pm)
					skipsymbol(rbracksym)
					p:=createconstunit(ttsize[t],tu64)
				else
					p:=readterm()
					p:=createsizeofop(p)
				fi
			else
				p:=createsizeofop(readterm())
			fi
		fi

	when kgenericsym then
		p:=readgeneric()
	when kalignofsym then
		serror("rt/alignof")

	when ksetjmpsym then
		tag:=lx.subcode
		lex()
		checksymbol(lbracksym)
		lex()
		p:=readassignexpr()
		if tag=jlongjmp then
			checksymbol(commasym)
			lex()
			q:=readassignexpr()
		else
			q:=nil
		fi
		p:=createunit2(tag,p,q)
		p.mode:=ti32
		checksymbol(rbracksym)
		lex()


	else
	PS("RT")
		serror("Readterm?")
	end switch

!look at the suffix

	doswitch lx.symbol
	when lsqsym then
		lex()
		q:=readexpression()
		skipsymbol(rsqsym)
		p:=createindexop(p,q)

	when dotsym, idotsym then
		opc:=symboltojtag[lx.symbol]
		lex()
		checksymbol(namesym)
		d:=lx.symptr
		lex()

		p:=createdotop(opc,p,d)

	when lbracksym then
		lex()
		if lx.symbol=rbracksym then			!()
			q:=nil
			lex()
		else
			q:=readexprlist(nil)
			skipsymbol(rbracksym)
		fi
		p:=createcall(p,q)

	when incrsym then
		lex()
		p:=createincrop(jpostincr,p)

	when decrsym then
		lex()
		p:=createincrop(jpostdecr,p)

	else
		exit
	end doswitch

	return p
end

function readexprlist(unit p)unit=
! read comma-separated list, and return head of list (not as jmakelist etc)
!p=nil:		at start of first expr (not ")")
!p<>nil:	p will be head of the list; comma skipped so at start of next expr
	unit ulist, ulistx

	ulist:=ulistx:=p
	do
		p:=readassignexpr()
		addlistunit(&ulist,&ulistx,p)
		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od
	return ulist

end

function readmodulevar(ref strec d, int m, linkage)ref strec=
!read or function decl at module scope
	ref strec e
	int scope,emode

	e:=checkdupl(stmodule, d, ns_general, 0)

	if e then					!already exists
		if e.nameid<>staticid then
			serror_ss("var: name in use # #",e.name,namenames[e.nameid])
		fi
		emode:=e.mode
		if emode<>m then
			if not comparemode(emode,m) then
	redef:
				serror_s("var: redefining #",e.name)
			fi
			case ttbasetype[emode]
			when tarray then
				if ttlength[emode]=0 then			!replace empty array
					e.mode:=m
				elsif ttlength[m] and ttlength[emode]<>ttlength[m] then
					goto redef
				fi
			esac

		fi
		d:=e

!see how scope interacts with existing decl
		scope:=d.scope
		if scope=local_scope and linkage=none_ss or
		   scope=exported_scope and linkage=static_ss or
		   scope=imported_scope and linkage=static_ss then

!*!		serror("Linkage mismatch")

		elsif scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
		fi

	else
		d:=createdupldef(stmodule,d,staticid)
		d.mode:=m
		case linkage
		when static_ss then
			scope:=local_scope
		when extern_ss then
			scope:=imported_scope
		else
			scope:=exported_scope
		esac

	fi

	if lx.symbol=assignsym then
		if d.code then
			serror_s("Can't init twice #",d.name)
		fi
		if scope=imported_scope then
			serror_s("Can't init extern #",d.name)
		fi
		lex()
		d.code:=readinitexpr(stmodule,d.mode)
	fi

	d.scope:=scope
	return d
end

function readframevar(ref strec d,int m, linkage)ref strec=
	ref paramrec pm
	ref strec e
	int scope,id

	e:=checkdupl_inproc(currproc, d, ns_general, currblockno)

	if e then					!already exists
			serror_s("var: name in use #",e.name)
		d:=e

!see how scope interacts with existing decl
		scope:=d.scope
		if scope=local_scope and linkage=none_ss or
		   scope=exported_scope and linkage=static_ss or
		   scope=imported_scope and linkage=static_ss then
!*!		serror("Linkage2 mismatch")
		elsif scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
		fi
	else
		id:=frameid
		scope:=function_scope
		case linkage
		when static_ss then
			id:=staticid
		when extern_ss then
			scope:=imported_scope
			id:=staticid
		esac
		d:=createdupldef(currproc,d,id)
		d.mode:=m
		d.blockno:=currblockno
		blockcounts[currblockno]:=1
	fi

	if lx.symbol=assignsym then
		if d.code then
			serror_s("Can't init twice #",d.name)
		fi
		if scope=imported_scope then
			serror_s("Can't init extern #",d.name)
		fi
		lex()
		d.code:=readinitexpr(currproc,d.mode)
	fi

	d.scope:=scope

	return d
end

function readtype(ref strec owner, &d, int m, ref paramrec &pm)int=
	[maxtypemods]int modtype
	[maxtypemods]ref void modvalue
	ref paramrec pmx
	int nmodifiers,i
	nmodifiers:=0

	pm:=nil

	readnamedtype(owner,d, modtype,modvalue,nmodifiers)

!now apply modifiers to base type:
	for i:=nmodifiers downto 1 do
		case modtype[i]
		when 'A' then
			m:=createarraymode(m,int(modvalue[i]))
		when 'R' then
			m:=createrefmode(m)
		when 'C' then
			m:=createconstmode(m)
		when 'F' then
			pmx:=modvalue[i]

			if i=1 then				!indicate to caller that this is a normal function
				pm:=pmx
			else					!assume function pointer of some sort
				m:=createprocmode(m,pmx)
			fi
		esac
	od

	return m
end

proc readnamedtype(ref strec owner, &d, []int &modtype, []ref void &modvalue, int &nmodifiers)=
	int length
	[maxtypemods]int fconst
	int nrefs
	unit pdim

	d:=nil
	nrefs:=0

	if lx.symbol=kfnspecsym then
		lex()
	fi

	while lx.symbol=mulsym do			!pointer/qualifier loop
		++nrefs
		fconst[nrefs]:=0
		lex()
		while lx.symbol=ktypequalsym do
			case lx.subcode
			when const_qual then
				fconst[nrefs]:=1
			when volatile_qual, restrict_qual then
			else
				serror("rnt1")
			esac
			lex()
		od
	od

	case lx.symbol
	when namesym then
		d:=lx.symptr
		lex()
	when lbracksym then
		lex()
		readnamedtype(owner,d,modtype,modvalue,nmodifiers)
		skipsymbol(rbracksym)
	esac

	docase lx.symbol
	when lsqsym then
		lex()
		if lx.symbol=rsqsym then
			length:=0
		else
			pdim:=readassignexpr()
			if pdim.tag=jconst then
				length:=pdim.value
			else
				serror("Can't do VLAs")
			fi
			checksymbol(rsqsym)
IF LENGTH=0 THEN SERROR("ZERO LEN ARRAY") fi
		fi
		if length<0 then terror("Negative array dim") fi

		lex()
		modtype[++nmodifiers]:='A'
		modvalue[nmodifiers]:=ref void(length)

	when lbracksym then			!fn params
		lex()
		modtype[++nmodifiers]:='F'
		modvalue[nmodifiers]:=readparams(owner)
	else
		exit
	end docase

!now apply any pointers
	while nrefs do
		if fconst[nrefs] then
			modtype[++nmodifiers]:='C'
		fi
		modtype[++nmodifiers]:='R'
		--nrefs
	od
end

function readconstintexpr:int=
	unit p
	int val

	p:=readassignexpr()
	case p.tag
	when jconst then
		return p.value

	else
		serror_s("readconstint #",jtagnames[p.tag])
	esac
	return 0
end

function readinitexpr(ref strec owner, int m)unit=
	int count
	unit p

	p:=readinitexpr2(owner,m,1)

	return p
end

function readinitexpr2(ref strec owner, int m, istop)unit=
	unit ulist, ulistx, p
	int mbase,melem,mm
	int dim,count
	ref strec d,e
	int braces

	mbase:=ttbasetype[m]
	count:=0

	if lx.symbol=lcurlysym then
		lex()

		count:=0
		case mbase
		when tarray then
			dim:=ttlength[m]
			if not istop and dim=0 then terror("init/0-size array") fi
			melem:=tttarget[m]
!			if ttbasetype[melem]=tu8 and lx.symbol=stringconstsym then
			if ttbasetype[melem]=tchar and lx.symbol=stringconstsym then
				braces:=1
				goto doarraystring
			fi

		when tstruct,tunion then
			d:=ttnamedef[m]
			e:=d.deflist
			if e=nil then
				terror("init/Empty struct")
			fi
			melem:=e.mode
		else
			p:=readassignexpr()
			p:=coercemode(p,m)
			skipsymbol(rcurlysym)
			return p
		esac

		ulist:=ulistx:=nil
		do
			p:=readinitexpr2(owner,melem,0)
			++count

			case mbase		
			when tarray then
				if dim and count>dim then
					terror("Too many array elems")
				fi

				if ttbasetype[melem]=tarray and ttbasetype[tttarget[melem]]=tchar and p.mode=trefchar then
				else
					p:=coercemode(p,melem)
				fi
			when tstruct then

				mm:=e.mode

				if ttbasetype[mm]=tarray and ttbasetype[tttarget[mm]]=tu8 and p.mode=trefchar then
				else
					p:=coercemode(p,mm)
				fi

				e:=e.nextdef
				if e=nil then
					if lx.symbol=commasym and nextlx.symbol<>rcurlysym then
						terror("Too many struct elems")
					fi
				else
					melem:=e.mode
				fi
			when tunion then
				p:=coercemode(p,melem)
				ulist:=ulistx:=p
				goto donestruct
			esac

			addlistunit(&ulist,&ulistx,p)
			if lx.symbol<>commasym then
				exit
			fi
			if nextlx.symbol=rcurlysym then		! {10,20,30,} allowed
				lex()
				exit
			fi
			lex()
		od
		if mbase=tarray and dim=0 then
			ttlength[m]:=count
			ttsize[m]:=count*ttsize[melem]
		fi

	donestruct:
		skipsymbol(rcurlysym)
		p:=createunit1(jmakelist,ulist)
		p.count:=count

		p.mode:=m

	else
		braces:=0
		case mbase
		when tarray then
	doarraystring:
			if lx.symbol<>stringconstsym and lx.symbol<>wstringconstsym and 
				tttarget[m]<>tchar then
				terror("{} initialiser expected")
			fi

			p:=readassignexpr()
			case p.mode
			when trefchar then
			when trefwchar then
			else
				terror("Array init")
			esac
			P.MODE:=M

			if (dim:=ttlength[m])=0 then
				ttlength[m]:=ttsize[m]:=p.slength+1
			else
				if p.slength>dim then
					terror("Init str too long")
				fi
			fi
			if braces then skipsymbol(rcurlysym) fi
			return p
		esac
		p:=readassignexpr()
		p:=coercemode(p,m)

	fi
	return p
end

proc pushblock=
	int n

	if blocklevel>=maxblockstack then
		serror("Too many block levels")
	fi
	if nextblockno>=maxblock then
		serror("Too many blocks")
	fi
	++blocklevel
	++nextblockno

	n:=currblockno

	int m:=blocklevel								!NEED TO ACCESS CONTAINING BLOCKS
													!VIA BLOCKSTACK

	while m and blockcounts[blockstack[m]]=0 do
		--m
    n:=blockstack[m]
	od

	blockowner[nextblockno]:=n

	currblockno:=blockstack[blocklevel]:=nextblockno
	blockcounts[currblockno]:=0
end

proc popblock=
	currblockno:=blockstack[--blocklevel]
end

function readcompoundstmt(int params):unit=
!read {...} statements
!positioned at first {, exit at symbol past final }
	unit ulist, ulistx, p,q

	ulist:=ulistx:=nil

	lex()			!skip {
	pushblock()
	if params then		!assume top block of function
		blockcounts[1]:=1
	fi

	while lx.symbol<>rcurlysym do
		p:=readstatement()

		if p=nil then nextloop fi				!might have been typedef etc
		if p.tag=jtempdecl then
			repeat
				q:=p.nextunit
				if p.def.code and p.def.nameid<>staticid then
					p.tag:=jdecl
					p.nextunit:=nil
					addlistunit(&ulist,&ulistx,p)
				fi
				p:=q
			until p=nil
		else
			addlistunit(&ulist,&ulistx,p)
		fi
	od
	lex()
	popblock()
	return createunit3(jblock,ulist,nil,ulistx)
end

function readblock(int ifelse=0)unit=

		if not needcompoundblock then
			return readstatement()
		fi
		if lx.symbol=kifsym and ifelse then
			return readstatement()
		fi

		if lx.symbol<>lcurlysym then
			serror("{...} statement expected")
		fi
		return readcompoundstmt(0)
end

function readstatement:unit=
	unit p,q
	ref strbuffer ss
	ref strec d
	int index

	switch lx.symbol
	when kifsym then
		return readifstmt()

	when kforsym then
		return readforstmt()

	when kwhilesym then
		return readwhilestmt()

	when kdosym then
		return readdostmt()

	when kreturnsym then
		return readreturnstmt()

	when kswitchsym then
		return readswitchstmt()

	when lcurlysym then
		return readcompoundstmt(0)

	when kgotosym then
		return readgotostmt()

	when kbreaksym then
		if loopindex then
			if looptypestack[loopindex]='L'then
				p:=createunit0(jbreak)
				lex()
			else
				p:=createunit0(jbreaksw)
				lex()
			fi
		else
			serror("break outside loop/sw")
		fi

	when kcontinuesym then
		index:=loopindex
		while index and looptypestack[index]<>'L' do --index od
		if index=0 then
			serror("continue outside loop")
		fi

		p:=createunit0(jcontinue)
		lex()

	when kcasesym then
		return readcaselabel()

	when kdefaultsym then
		lex()
		skipsymbol(colonsym)
		return createunit1(jdefaultstmt,readstatement())

	when semisym then
		lex()	
		return nil

	when namesym then
		if nextlx.symbol=colonsym then
			p:=createunit1(jlabelstmt,nil)

			d:=resolvename(currproc,lx.symptr,ns_labels,0)
			if d then
				if d.index=-1 then				!already defined
					cpl lx.symptr.name
					terror("2:Duplicate label")
				fi
			else
				d:=createdupldef(currproc,lx.symptr,labelid)
				d.mode:=tvoid
			fi
			d.index:=-1						!indicate defined

			p.def:=d
			lex()				!skip colon
			lex()
			if lx.symbol=rcurlysym then
			elsif istypestarter() or lx.symbol=klinkagesym then
			else
				p.a:=readstatement()
			fi
			return p
		else
			ist_symptr:=nil
			if isusertype(currproc)<>tnotset then
				goto doreaddecl
			fi
			if ist_symptr then lx.symptr:=ist_symptr fi		!make use of name resolve done by isusertype
			p:=readexpression()
		fi
	when ktypespecsym, kstdtypesym, ktypequalsym, klinkagesym, kfnspecsym,
		kstructsym,kunionsym,kenumsym then
	doreaddecl:
		return readlocaldecl()

	else						!assume expression
		p:=readexpression()
	end switch

	skipsymbol(semisym)

	return p
end

function readifstmt:unit p=
	unit pcond,pbody,pelse
	int lineno

	lex()
	lineno:=lx.lineno

	pcond:=readcond()
	coercecond(pcond)

	pbody:=readblock()

	pelse:=nil

	if lx.symbol=kelsesym then
		lex()

		pelse:=readblock(1)
	fi

	p:=createunit3(jif,pcond,pbody,pelse)
	p.lineno:=lineno

	if iscondtrue(pcond) then		!branch b only
		if pbody=nil then
			pbody:=createunit0(jblock)
		fi
		deleteunit(p,pbody)
	elsif iscondfalse(pcond) then	!branch c only
		if pelse=nil then
			pelse:=createunit0(jblock)
		fi
		deleteunit(p,pelse)
	fi

	return p;
end

global func iscondtrue(unit p)int =
	p.tag=jconst and p.value<>0
end

global func iscondfalse(unit p)int =
	p.tag=jconst and p.value=0
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

function readforstmt:unit=
	unit pinit, pcond, pincr, pbody, ulist,ulistx, p
	int linkage,hasblock,m,mbase
	ref paramrec pm
	ref strec d

	lex()
	skipsymbol(lbracksym)
	hasblock:=0

	if lx.symbol<>semisym then

		if istypestarter() then
			hasblock:=1
			pushblock()

			mbase:=readdeclspec(currproc,linkage)
			ulist:=ulistx:=nil

			docase lx.symbol
			when namesym, mulsym, lbracksym then

				m:=readtype(currproc,d,mbase,pm)
				if d=nil then
					serror("Var name expected")
				fi

				if linkage=typedef_ss or pm then
					serror("Not allowed in for stmt")
				fi
				d:=readframevar(d,m,linkage)

				if d.code then
					p:=createunit0(jdecl)
					p.def:=d
					addlistunit(&ulist,&ulistx,p)
				fi

				case lx.symbol
				when commasym then			!read next item
					lex()
				else
					exit
				esac
			else
				serror("For decl error")
			end docase
			pinit:=createunit3(jblock,ulist,nil,ulistx)

		else
			pinit:=readexpression()
		fi
	else
		pinit:=createunit0(jnull)
	fi
	skipsymbol(semisym)

	if lx.symbol<>semisym then
		pcond:=readexpression()
		coercecond(pcond)
	else
		pcond:=createunit0(jnull)
	fi
	skipsymbol(semisym)

	if lx.symbol<>rbracksym then
		pincr:=readexprstmt()
	else
		pincr:=nil
	fi
	skipsymbol(rbracksym)

	pushloop('L')
	pbody:=readblock()
	poploop()
	if hasblock then
		popblock()
	fi

	pinit.nextunit:=pcond			!the 3 for elements are linked together
	pcond.nextunit:=pincr

	return createunit2(jfor, pinit, pbody)
end

function readwhilestmt:unit=
	unit pcond,pbody

	lex()
	pcond:=readcond()
	coercecond(pcond)
	pushloop('L')
	pbody:=readblock()
	poploop()

	return createunit2(jwhile,pcond,pbody)
end

function readdostmt:unit=
	unit pbody,pcond
	lex()
	pushloop('L')
	pbody:=readblock()
	poploop()
	skipsymbol(kwhilesym)

	pcond:=readcond()
	coercecond(pcond)

	skipsymbol(semisym)
	return createunit2(jdowhile,pbody,pcond)
end

function readreturnstmt:unit=
	unit p
	lex()
	p:=nil

	if lx.symbol<>semisym then
		if currproc.mode=tvoid then
			terror("Can't return value in void function")
		fi

		p:=readexpression()
		p:=coercemode(p,currproc.mode)
		checksymbol(semisym)
	elsif currproc.mode<>tvoid then
		terror("Return value needed")
	fi
	lex()

	return createunit1(jreturn,p)
end

function readgotostmt:unit=
	ref strec d
	unit p

	lex()
	checksymbol(namesym)
	d:=resolvename(currproc,lx.symptr,ns_labels,0)
	if d=nil then					!assume fwd ref
		d:=createdupldef(currproc,lx.symptr,labelid)
		d.mode:=tvoid
	fi
	p:=createunit1(jgoto,nil)

	p.def:=d
	lex()				!skip colon
	skipsymbol(semisym)
	return p
end

function readswitchstmt:unit=
	unit pindex,pstmt,p

	lex()
	pindex:=readcond()			!not a condition, but it doesn't matter
	coercemode(pindex,ti32)

	pushloop('S')
	pstmt:=readblock()			!not a condition, but it doesn't matter
	p:=createunit2(jswitch, pindex, pstmt)
	p.nextcase:=casevaluestack[loopindex]

	poploop()
	return p
end

function readcaselabel:unit=
	unit p,q
	int value

	lex()					!skip case/default
	value:=readconstintexpr()
	skipsymbol(colonsym)

	p:=createunit1(jcasestmt,readstatement())

	p.value:=value

	addcasevalue(value)
	return p
end

function readexprstmt:unit=
	return readexpression()
end

function readcond:unit=
!should be at '(', read conditional expr
	unit pcond
	skipsymbol(lbracksym)
	pcond:=readexpression()
	skipsymbol(rbracksym)
	return pcond
end

function isusertype(ref strec owner)int=
!current symbol is a namesymbol
!return typeno if it resolves to a user type, otherwise tnotset
!will peek at following symbol, and returns 0 if "," or ";" follows
	ref strec d

	d:=resolvename(owner,lx.symptr,ns_general,currblockno)
	if d then
		if d.nameid=typeid then
			return d.mode
		fi
		ist_symptr:=d
	fi
	return tnotset
end

function readlocaldecl:unit=
!at typebase starter inside function or block
	int m,mbase,linkage,nitems,wasenum,wasdef
	ref strec d
	unit ulist,ulistx,p
	ref paramrec pm

	ulist:=ulistx:=nil

	wasenum:=lx.symbol
	mbase:=readdeclspec(currproc,linkage)
	nitems:=0

	docase lx.symbol
	when namesym, mulsym, lbracksym then
		++nitems

		m:=readtype(currproc,d,mbase,pm)
		if d=nil then
			serror("Var name expected")
		fi

		if linkage=typedef_ss then
			d:=createtypedef(currproc,d,m)
		elsif pm then
			if lx.symbol=lcurlysym then
				serror("Nested function")
			fi
			d:=readfunction(d,m,linkage,pm,wasdef)
		else
			d:=readframevar(d,m,linkage)
			p:=createunit0(jtempdecl)
			p.def:=d
			addlistunit(&ulist,&ulistx,p)
		fi
		case lx.symbol
		when commasym then			!read next item
			lex()
		else
			skipsymbol(semisym)
			exit
		esac
	else
		case ttbasetype[mbase]
		when tenum, tstruct, tunion then		!assume defining a [part]type only
			skipsymbol(semisym)
			exit
	when ti32 then
		skipsymbol(semisym)
		exit

		else
			serror_s("Local decl error #",typename(m))
		esac
	end docase

	return ulist
end

function createtypedef(ref strec owner, symptr, int mode)ref strec=
!symptr is a generic symbol for the name
	ref strec d

	d:=checkdupl(owner,symptr,ns_general,currblockno)

	if d then			!existing name
		if d.nameid<>typeid then
			serror_s("Typedef name in use #",d.name)
		fi

		if d.mode<>mode then
			if not comparemode(d.mode, mode) then
				serror_s("Typedef redefined or can't match types #",d.name)
			fi
		fi
		return d
	fi

	d:=createdupldef(owner,symptr,typeid)

	d.mode:=mode
	tttypedef[mode]:=d

	d.blockno:=currblockno
	blockcounts[currblockno]:=1

	return d
end

function readparams(ref strec owner)ref paramrec=
	ref paramrec ulist,ulistx, pm, q
	int m,lastbasetype,nparams,variadic,flags,nnames
	ref strec d

	D:=NIL

	ulist:=ulistx:=nil
	variadic:=nparams:=nnames:=0

	lastbasetype:=tvoid

	int names:=0, nonames:=0,reported:=0

	while lx.symbol<>rbracksym do
		if lx.symbol=ellipsissym then
			variadic:=1
			lex()
			exit
		fi

		if istypestarter() then
			m:=readcasttype(d,1,pm,tvoid,&lastbasetype)
			if pm then			!was a fu nction; convert to fu nction pointer
				m:=createrefmode(createprocmode(m,pm))
			fi
		else
			if lastbasetype=tvoid then
				serror("Param type missing or misspelt")
			fi
			m:=readcasttype(d,1,pm, lastbasetype)

		fi

		case ttbasetype[m]
		when tarray then
			m:=createrefmode(tttarget[m])
		when tproc then
			m:=createrefmode(createprocmode(m,ttparams[m]))
		esac

		pm:=pcm_allocz(paramrec.bytes)
		pm.def:=d
		pm.mode:=m
		++nparams

		if d then names:=1 else nonames:=1 fi

	if names and nonames and not reported then
		reported:=1
	fi

		if d then
			++nnames
			q:=ulist
			while q do
				if q.def=d then
					serror_ss("Param name reused # #",d.name,namenames[d.nameid])
				fi
				q:=q.nextparam
			od

		fi

		addlistparam(&ulist,&ulistx,pm)
		case lx.symbol
		when commasym then
			lex()
		when ellipsissym, rbracksym then
		else
			serror("bad symbol in paramlist")
		esac
	od

	flags:=0
	skipsymbol(rbracksym)

	if variadic then
		flags:=pm_variadic
	elsif nparams=0 then
		flags:=pm_notset
	elsif nparams=1 and m=tvoid then
		flags:=pm_empty
		nparams:=0
		ulist.mode:=tvoid
	fi

	if ulist=nil then
		ulist:=pcm_allocz(paramrec.bytes)
	fi
	ulist.nparams:=nparams
	ulist.flags:=flags

	return ulist
end

function readcasttype(ref strec &d, int allowname=0,ref paramrec &pm,
	int m=tvoid, ref int mbase=nil)int=
!at first symbol of a type-spec
!ref paramrec pm
	ref strec owner
	int linkage

	owner:=(currproc|currproc|stmodule)

	linkage:=0
	d:=nil
	if m=tvoid then
		m:=readdeclspec(owner,linkage)
		if mbase then
			mbase^:=m
		fi

	fi
	pm:=nil

	case lx.symbol
	when namesym, mulsym, lbracksym, lsqsym then
		m:=readtype(owner,d, m, pm)
		if d and not allowname then
			serror_s("NAME not allowed in cast type #",d.name)
		fi
	esac

	return m
end

function readfunction(ref strec d, int m, linkage, ref paramrec pm, int &wasdef)ref strec=
!have read function declaration, with ";" or "{" nextloop
!d is generic st entry for name
!m is return type
!pm is linked list of parameter types
!set up the declaration properly in symbol table, checking for duplicates etc
!read function body if {...} follows
!return wasdef=1 if {...} encountered, as looping in the caller will be affected

	ref strec f,owner
	int scope

	owner:=stmodule
	wasdef:=0

	f:=checkdupl(owner, d, ns_general, 0)

	if f then					!already exists
		if f.nameid<>procid then
			serror_s("fn: name in use #",d.name)
		fi
!COMPARE PARAM LISTS...
!	if e.paramlist<>pm then
!		serror("fn: params don't match previous")
!	fi
		d:=f

!see how scope interacts with existing decl
		scope:=d.scope
		if scope=imported_scope and linkage=none_ss then
			scope:=exported_scope
		elsif linkage=static_ss then
			scope:=local_scope
		fi


	else
		d:=createdupldef(owner,d,procid)
		d.mode:=m
		case linkage
		when static_ss then
			scope:=local_scope
		when extern_ss then
			scope:=imported_scope
		else
!			scope:=exported_scope
			scope:=imported_scope
		esac
	fi

	d.paramlist:=pm
	d.scope:=scope

	if lx.symbol=lcurlysym then

		wasdef:=1
		if d.code then
			serror_s("Can't define function twice #",d.name)
		fi
		if scope=imported_scope then
			d.scope:=exported_scope
		fi

		readfunctionbody(d)
		if lx.symbol=semisym then
			serror("; after function def")
		fi
	fi

	return d
end

proc readfunctionbody(ref strec f)=
!positioned just after '{'; return at '}' (checked by caller)
	ref strec e
	unit p
	ref paramrec pm
	int pmcount

	currproc:=f
	nextblockno:=currblockno:=0
	pmcount:=0

!add named patams
	pm:=f.paramlist
	to pm.nparams do
		if pm.def=nil then
!			serror("Param name missing")
		else
			e:=createdupldef(f,pm.def,paramid)
			if e.name^='$' then			!assume block ret param
				e.used:=1
			fi

			e.blockno:=1
			e.mode:=pm.mode
		fi
		pm:=pm.nextparam
		pmcount:=1
	od

	p:=readcompoundstmt(pmcount)

	currproc.code:=p
	currproc:=nil
end

function createnegop(unit p)unit=
	unit q
	int t

	t:=p.mode

	if p.tag=jconst then
		case t
		when ti32,ti64,tu64 then
			p.value:=-p.value
			return p
		when tu32 then
			p.value:=(-p.value) iand 0xFFFF'FFFF
			return p
		when tr64 then
			p.xvalue:=-p.xvalue
			return p
		esac
	fi
	retry:
	if t>=tfirstnum and t<=tlastnum then
		coercebasetype(p)
		q:=createunit1(jneg,p)
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
	else
	CPL strmode(t)
		terror("neg bad type")
	fi

	q.mode:=p.mode
	return q
end

function createabsop(unit p)unit=
	unit q
	int t

	t:=p.mode

	if p.tag=jconst then
		case t
		when ti32,ti64 then
			p.value:=abs(p.value)
			return p
		esac
	fi

	if isintcc(t) then
		coercebasetype(p)
		q:=createunit1(jabs,p)
	else
		terror("abs bad type")
	fi

	q.mode:=p.mode
	return q
end

function createinotop(unit p)unit=
	unit q
	int t

	t:=ttbasetype[p.mode]

	if p.tag=jconst then
		case t
		when ti32,ti64,tu32,tu64 then
			p.value:=inot p.value
			return p
		esac
	fi
	if isintcc(t) then
		coercebasetype(p)
		q:=createunit1(jinot,p)
	else
	cpl strmode(t)
		terror("! bad type")
	fi

	q.mode:=p.mode
	return q
end

function createptrop(unit p)unit=
	unit q
	int t,m

	if not ttisref[t:=p.mode] then
		PRINTUNIT(NIL,P)
		terror("* not pointer")
	fi
	m:=tttarget[t]

!CPL "C/PTR",STRMODE(M)
!CPL "C/PTR",STRMODE(P.MODE)
!CPL "C/PTR",STRMODE(TTTARGET[P.MODE])
!
	case p.tag
	when jaddrof then
		q:=p.a
!		if p.alength then
			q.mode:=tttarget[p.mode]
!		ELSE
!			q.mode:=tttarget[p.mode]
!		FI
!CPL "DONE", STRMODE(Q.MODE)
		fixmemopnd(q)
!CPL "DONE", STRMODE(Q.MODE)
		return q
	esac

	q:=createunit1(jptr,p)
	q.mode:=m
	q:=arraytopointer(q)
	fixmemopnd(q)

	return q
end

function createincrop(int opc,unit p)unit=
!opc is jpreincr/decr or jpostincr/decr
	unit q
	int t

	t:=p.mode

	checklvalue(p,1)

	unless isintcc(t) and t<>tbool or ttisref[t] then
		terror("++ bad type")
	end unless
	q:=createunit1(opc,p)
	q.mode:=p.mode

	return q
end

function createaddrofop(unit p)unit=
	ref strec d
	unit q
	int t,u,alength

	alength:=0

	restartx:
	t:=p.mode
	if p.memmode then t:=p.memmode fi

	switch p.tag
	when jname then
P.DEF.ADDROF:=1
		if p.alength then
			t:=p.def.mode
			alength:=p.alength
		fi

	when jaddrof then
		if p.a.tag=jname and p.a.alength then		!sounds like ANAME => &ANAME
			p.mode:=createrefmode(p.a.def.mode)
	p.alength:=p.a.alength
			return p
		fi
	when jdot then
		q:=p.a
		if q.tag=jptr and q.a.tag=jconst then
			p:=createconstunit(p.offset+q.a.value, ti32)
			return p
		fi
		goto cad1
	when jaddptr then
		if p.alength then
			p.mode:=createrefmode(createarraymode(tttarget[p.mode],p.alength))
			return p
		fi
	when jwidenmem then
		p:=p.a
		goto restartx
	when jfuncname then
		return p
	else

	cad1:
		checklvalue(p)
	end switch

	p:=createunit1(jaddrof,p)
	p.mode:=createrefmode(t)
	p.alength:=alength

	return p
end

function createaddop(unit x,y)unit=
	unit z
	int s,t,u,opc,elemsize

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]
	opc:=jadd

	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)

	elsif s=tref then
	doaddref:
		u:=x.mode
		elemsize:=ttsize[tttarget[u]]
		if x.tag=jconst and y.tag=jconst then
			x.value +:=y.value*elemsize
			return x
		fi

		y:=coercemode(y,tptroffset)

		z:=createunit2(jaddptr,x,y)
		z.mode:=u
		z.ptrscale:=elemsize
		return z

	elsif t=tref then
		swap(x,y)
		goto doaddref
		terror("Sub bad types")
	fi

	if x.tag=jconst then
		if y.tag=jconst then
			return eval_add(opc,x,y,u)
		else
			swap(x,y)
		fi
		if y.value=0 then			!works for int/float
			return x				!x+0 => x
		fi
	fi
	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createsubop(unit x,y)unit=
	unit z
	int s,t,u,opc,elemsize

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]
	opc:=jsub

	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	elsif s=tref then
		if t<>tref then
			u:=x.mode
			elemsize:=ttsize[tttarget[u]]
			y:=coercemode(y,tptroffset)

			z:=createunit2(jsubptr,x,y)
			z.mode:=u
			z.ptrscale:=elemsize
			return z

		else							!ref-ref
			if x.tag=jconst and y.tag=jconst then
				x.value -:= y.value/ttsize[tttarget[x.mode]]
				x.mode:=ti32
				return x

			else
				z:=createunit2(opc,x,y)
				z.mode:=tptroffset
				z:=divunit(z,tttarget[x.mode])
				z.mode:=tptroffset
				return z
			fi
		fi
		y:=mulunit(y,tttarget[x.mode])
	else
		terror("Sub bad types")
	fi

	if x.tag=jconst and y.tag=jconst then
		return eval_sub(opc,x,y,u)
	fi
	if y.tag=jconst and y.value=0 then			!works for int/float
!		return x				!x-0 => x
	fi

	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createmulop(unit x,y)unit=
	unit z
	int s,t,u,opc

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]

	opc:=jmul
	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Mul bad types")
	fi

	if x.tag=jconst then
		if y.tag=jconst then
			return eval_mul(opc,x,y,u)
		else
			swap(x,y)
		fi
	fi

	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createdivop(unit x,y)unit=
	unit z
	int s,t,u,opc

	s:=ttbasetype[getmemmode(x)]
	t:=ttbasetype[getmemmode(y)]

	opc:=jdiv
	if u:=dominantmode[s,t] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Div bad types")
	fi

	if x.tag=jconst and y.tag=jconst then
		return eval_div(opc,x,y,u)
	elsif y.tag=jconst and u=tr64 then
		opc:=jmul
		y.xvalue:=1.0/y.xvalue

	fi
	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

function createremop(unit x,y)unit=
	unit z
	int s,t,u,opc

	s:=ttbasetype[x.mode]
	t:=ttbasetype[y.mode]

	opc:=jrem
	if u:=dominantmode[s,t] then			!were both numeric
		if u=tr64 or u=tr32 then
			u:=ti32
		fi
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("Rem bad types")
	fi

	if x.tag=jconst and y.tag=jconst then
		return eval_rem(opc,x,y,u)
	fi
	z:=createunit2(opc,x,y)
	z.mode:=u

	return z
end

proc insertunit(unit p, int tag)=
!wrap extra unit around p, using given tag
	unit q
	q:=createunit0(0)			!empty unit
	q^:=p^
	p.tag:=tag
	p.a:=q
	p.b:=p.c:=nil
	p.lineno:=q.lineno
	p.nextunit:=q.nextunit
	p.memmode:=0

	q.nextunit:=nil
end

function eval_add(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64,tu32,tu64 then
		x.value +:= y.value
		return x
	when tr64 then
		x.xvalue +:= y.xvalue
		return x
	elsif ttbasetype[t]=tref then	!assume y is const 0 int of any sub-type
		x.value +:= y.value*ttsize[tttarget[t]]
		return x			!will not change x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_sub(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64,tu32,tu64 then
		x.value -:= y.value
		return x
	when tr64 then
		x.xvalue -:= y.xvalue
		return x
	elsif ttbasetype[t]=tref then
		if ttbasetype[y.mode]=tref then
			terror("EVALSUB/REF")
		fi
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_mul(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64,ti16,ti8 then
		x.value *:= y.value
		return x
	when tu32,tu64,tu16,tu8 then
		x.uvalue := x.uvalue*y.uvalue
		return x
	when tr64 then
		x.xvalue *:= y.xvalue
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_div(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64 then
		if y.value=0 then serror("div 0") fi
		x.value := x.value/y.value
		return x
	when tu32,tu64 then
		if y.value=0 then serror("div 0") fi
		x.uvalue := x.uvalue/y.uvalue
		return x
	when tr64 then
		x.xvalue /:= y.xvalue
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_rem(int opc,unit x,y,int t)unit=
	unit z

	case t
	when ti32,ti64 then
		if y.value=0 then serror("rem 0") fi
		x.value := x.value rem y.value
		return x
	esac

	z:=createunit2(opc,x,y)
	z.mode:=t
	return z
end

function eval_convert(unit p, int t,opc)int=
!p contains a const unit, t is a target type, opc is conv op
!try and convert if possible
!return 1 if converted
	int s

	if opc=soft_c then
	dosoft:
		p.mode:=t
		return 1
	fi

!RETURN 0
	s:=p.mode
	if s=t then return 1 fi

	case s
	when ti32,ti16,ti8,ti64 then
		case t
		when tr64,tr32 then
			p.xvalue:=p.value
			p.mode:=t
			return 1
		when tu64,ti64,tu32,ti32,ti16,ti8,tu8,tu16 then
	dotrunc:
			case ttsize[t]
			when 1 then
				p.value iand:=255
				if stdsigned[t] then
					p.value:=i8(p.value)
				fi
			when 2 then
				p.value iand:=65535
				if stdsigned[t] then
					p.value:=i16(p.value)
				fi
			when 4 then
				p.value :=p.value iand 0xFFFF'FFFF
				if stdsigned[t] then
					p.value:=i32(p.value)
				fi
			esac
			goto dosoft
		esac
		if ttisref[t] then
			p.mode:=t
			return 1
		fi

	when tu32,tu8,tu16,tu64 then
		case t
		when tr64,tr32 then

			RETURN 0
			p.mode:=t
			return 1
		when tu64,ti64,ti32,tu32,tu64,tu16,ti8,tu8,ti16 then
			goto dotrunc
		esac
		if ttisref[t] then
			p.mode:=t
			return 1
		fi

	when tr64 then
		case t
		when ti32,ti64 then
			p.value:=p.xvalue
			p.mode:=t
			return 1
		when tu32,tu64 then
!		p.uvalue:=p.xvalue
			p.value:=p.xvalue
			p.mode:=t
			return 1
		when tr32 then
			p.mode:=tr32
			return 1
		esac
	elsif ttisref[p.mode] then
		if not p.isstrconst then
			case t
			when ti32,ti64,tu32,tu64 then
				p.mode:=t
				return 1
			esac
		fi
	esac

	return 0
end

proc coercecond(unit p)=
!p is an expression used as a condition
!Ensure result is i32; it doesn't need to be 0 or 1
!Anything else has istrue added

	int t
	if (t:=p.mode)=ti32 then return fi

	retry:
	case ttbasetype[t]
	when tr32,tr64,tref then
		goto doint

	elsif isintcc(t) then
	doint:
		if p.tag=jconst and p.value then			!check all types as one 64-bit field
			p.value:=1
		elsif p.tag=jconst and not p.value then			!check all types as one 64-bit field
			p.value:=0
		else
			insertunit(p,jistruel)
		fi
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
	else

		serror_s("Invalid condition #",strmode(t))
	esac
	p.mode:=ti32
end

proc coercebasetype(unit p)=
	int t

	if (t:=p.mode)>=ti8 and t<=ti16 then
		p:=coercemode(p,ti32)
	elsif t>=tbool and t<=tu16 then
		p:=coercemode(p,tu32)
	fi
end

proc checklvalue(unit p, int assign=0)=

	case p.tag
	when jname then
	when jptr then

	when jfuncname then
		if assign then notlv fi

	when jwidenmem then
		case p.a.tag
		when jname,jptr,jdot then
			p^:=p.a^
		else
			terror("CHECKLV/WIDEN")
		esac

	when jdot then

	when jconst then
		if not ttisref[p.mode] then
			goto notlv
		fi
	when jconvert then
		if assign then notlv fi

	else
	notlv:
		printunit(nil,p)
		terror_s("value: #",jtagnames[p.tag])
	esac
end

function createcall(unit p,q)unit=
!p is unit on left of param list, while q is the param list as the head of a unitlist
!do type-checking on params, and construct and return callfn unit
!p can be a simple name, or an expression that should be a function po inter
	unit r,s,u
	ref strec d
	ref paramrec pm
	int i,nparams,aparams,retmode,mproc,m,c
	[1024]char str
	ichar ss,tt,uu
	ref strbuffer exprstr

	d:=nil

	case p.tag
	when jptr then
	doptr:
		mproc:=p.mode

		while ttbasetype[mproc]=tref do
			r:=createunit1(jptr,p)
			mproc:=tttarget[mproc]
			r.mode:=mproc
			p:=r
		od

		if ttbasetype[mproc]<>tproc then
			serror_s("Not function pointer: #",typename(mproc))
		fi

		pm:=ttparams[mproc]
		retmode:=tttarget[mproc]

	when jname,jfuncname then
		d:=p.def
		if d.nameid=procid then
			pm:=d.paramlist
			retmode:=d.mode
		else							!assume fnptr, but needs ptr unit
			goto doptr
		fi
	when jdot,jcallfn,jifx,jconvert,jexprlist then
		r:=createunit1(jptr,p)
		r.mode:=tttarget[p.mode]
		p:=r
		goto doptr

	else
		CPL =JTAGNAMES[P.TAG]
		PRINTUNIT(NIL,P)
		serror("ccall?")
	esac

	nparams:=pm.nparams
	aparams:=0

	s:=q
	while s do
		++aparams				!number of actual params supplied
		s:=s.nextunit
	od

	if aparams<nparams then
		terror("1:Too few args")
	elsif aparams>nparams and pm.flags<>pm_variadic and pm.flags<>pm_notset then
!elsif aparams>nparams and pm.flags<>pm_variadic then
		if pm.flags<>pm_notset then
			cpl aparams,nparams


			terror("Too many args")
		fi
	fi

	s:=q

	for i:=1 to aparams do
		if i<=nparams then
			coercemode_inplace(s,pm.mode)
			pm:=pm.nextparam
		else					!assume variadic param
			if s.mode=tvoid then
				terror("Variadic param is void")
			fi
			coercebasetype(s)
		fi
		s:=s.nextunit
	od

	r:=createunit2(jcallfn,p,q)
	r.mode:=retmode
	fixmemopnd(r)
	r.aparams:=aparams

	return r
end

function arraytopointer(unit p)unit=
	unit q
	int offset
	int t,elemmode,refmode

	t:=p.mode
	elemmode:=tttarget[t]

	if ttbasetype[t]=tarray then
		refmode:=createrefmode(elemmode)
		case p.tag
		when jptr then
			p:=p.a

		when jdot then						!about to access array field
			offset:=p.offset
			p.tag:=jaddptr
			p.ptrscale:=1	!ttsize[elemmode]
			q:=createunit1(jaddrof,p.a)
			q.mode:=refmode
			p.a:=q
			p.b:=createconstunit(offset,ti32)

		else
			CPL "ATP:"
			printunit(nil,p)
			terror("ATP?")
		esac

		p.mode:=refmode
		p.alength:=ttlength[t]

	fi
	return p
end

function createindexop(unit p,q)unit=
!do p[q]
!convert to *(p+q)
	unit a

	a:=createaddop(p,q)
	return createptrop(a)
end

function readstructdecl(ref strec owner)int=
	ref strec d,e,currrecord
	ref strec ulist,ulistx,tagowner
	int funion,linkage,mbase,m
	int offset,recsize,maxsize,maxalignment,alignment,size
	ref paramrec pm
	ref fieldrec fieldlist,fl

	funion:=(lx.symbol=kunionsym)

	lex()				!skip 'struct' etc

	tagowner:=(currproc|currproc|stmodule)

	if lx.symbol=lcurlysym then				!anonymous struct tag
		d:=addnamestr(nextautotype())
	else
		checksymbol(namesym)
		d:=lx.symptr		!should be struct tag
		lex()

		if lx.symbol<>lcurlysym then			!reading incomplete enum
			e:=resolvename(tagowner,d,ns_tags,currblockno)
			if e then
				if e.nameid<>structtagid then
					serror_s("Struct tag in use #",e.name)
				fi

				return e.mode
			fi
!create new incomplete tag
			e:=createdupldef(tagowner,d,structtagid)
			e.mode:=createstructmode(e,(funion|tunion|tstruct))
			e.blockno:=currblockno
			blockcounts[currblockno]:=1
			return e.mode
		fi
	fi

!{ seen, so defining a new struct

	e:=checkdupl(tagowner,d,ns_tags,currblockno)

	if e then			!found in this linkage
		if e.nameid<>structtagid then
			serror_s("Struct tag in use #",e.name)
		fi
		if e.deflist then					!else filling in incomplete enum
			cpl "Prev",e.lineno iand 1677215, sourcefilenames[e.lineno>>24],sourcefilepaths[e.lineno>>24]
			serror_s("Redefining struct #",e.name)
		fi
	else						
		e:=createdupldef(tagowner,d,structtagid)
		e.mode:=createstructmode(e,(funion|tunion|tstruct))
		e.blockno:=currblockno
		blockcounts[currblockno]:=1
	fi

!e points to an def which has an empty {...} list
	lex()							!skip {

	currrecord:=e
	ulist:=ulistx:=nil
	offset:=maxsize:=recsize:=0
	maxalignment:=1
	fieldlist:=nil
	m:=-1

	while lx.symbol<>rcurlysym do
		mbase:=readdeclspec(currrecord,linkage)

		docase lx.symbol
		when namesym, mulsym, lbracksym then

			m:=readtype(currrecord,d,mbase,pm)
			if d=nil then
				serror("Field name expected")
			fi

			if linkage=typedef_ss or pm then
				serror("typedef or function inside struct")
			fi

			e:=checkdupl(currrecord, d, ns_fields, 0)

			if e then					!already exists
				serror_s("member name in use #",e.name)
			fi

			if linkage<>none_ss then
				serror("Can't use ss in struct")
			fi

	addanonfield:
			d:=createdupldef(nil,d,fieldid)
			d.mode:=m
!name is not linked in to record as they must be in sequence
			addlistdef(&ulist,&ulistx,d)
			currrecord.deflist:=ulist				!needed for dupl checking
			currrecord.deflistx:=ulistx
			d.owner:=currrecord
			alignment:=getalignment(m)
			if alignment>maxalignment then maxalignment:=alignment fi

			d.offset:=roundoffset(offset,alignment)
			size:=ttsize[m]
			recsize+:=d.offset-offset
			offset:=d.offset

			addnewfield(fieldlist,d,offset)

			if funion then
				maxsize:=max(maxsize,size)
			else
				offset+:=size
				recsize+:=size
			fi

			if lx.symbol=colonsym then
				lex()
				readassignexpr()
			fi

			case lx.symbol
			when commasym then			!read next item
				lex()
			else
				skipsymbol(semisym)
				exit
			esac
		when colonsym then				!apparently int:24 is allowed, with no member name
			lex()
			readassignexpr()
			skipsymbol(semisym)
			exit
		else
			case ttbasetype[mbase]
			when tstruct, tunion then		!assume defining a [part]type only
				d:=getautofieldname()
				m:=mbase
				goto addanonfield
			else
				if m=-1 then
					serror("Struct decl error")
				else
					serror_s("Struct decl error #",typename(m))
				fi
			esac
		end docase
	od

	skipsymbol(rcurlysym)

	currrecord.nextfield:=fieldlist
	ttsize[currrecord.mode]:=roundoffset((funion|maxsize|recsize),maxalignment)
	currrecord.align:=maxalignment

	if ttsize[currrecord.mode] in [1,2,4,8] then ttisblock[currrecord.mode]:=0 fi

	return currrecord.mode
end

function checkpointertypes(int s,t,hard)int=
!return 1 if pointer types s and t are compatible
!it is assumed that s is to be converted to t, or passed as a parameter expecting t
	int starget:=tttarget[s], ttarget:=tttarget[t]
	int sbase, tbase
	int sconst:=0,tconst:=0

	if ttconst[starget] then
		starget:=ttconsttype[starget]
		sconst:=1
	fi
	if ttconst[ttarget] then
		ttarget:=ttconsttype[ttarget]
		tconst:=1
	fi

	if not hard and sconst and not tconst then
		cpl strmode(s)
		cpl strmode(t)
		terror("const to non-const pointer")
	fi

	if starget=ttarget then return 1 fi

	s:=starget
	t:=ttarget
	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]

	if sbase in tfirstint..tlastint and tbase in tfirstint..tlastint then
		if ttsize[sbase]=ttsize[tbase] then		!ignore sign differences
			return 1
		fi
	fi

	if sbase=tvoid or tbase=tvoid then
		return 1
	fi

	if ttisref[s] and ttisref[t] then
		return checkpointertypes(s,t,hard)

	elsif ttbasetype[s]=tarray and ttbasetype[t]=tarray then
		if ttlength[s]<>ttlength[t] then
			if ttlength[s] and ttlength[t] then		!allow one dim to be 0
CPL "BAD REF[]"
RETURN 1
				return 0
			fi
		fi
		starget:=tttarget[s]
		ttarget:=tttarget[t]
		if starget=ttarget then return 1 fi
 
		if ttisref[starget] and ttisref[ttarget] then
			return checkpointertypes(starget,ttarget,hard)
		fi
		if ttbasetype[starget]=tarray and ttbasetype[ttarget]=tarray then
			return checkpointertypes(starget,ttarget,hard)
		fi
	elsif ttbasetype[s]=tproc and ttbasetype[t]=tproc then
		return 1				!NEED PROPER MATCH HERE
	fi

	return 0
end

function comparemode(int s,t)int=
!types s and t don't immediately match
!check further to see if they are compatible
!For example, if they are both arrays, then usually they will have different
!typenumbers. Arrays should match if they have the same element type, and
!same length, or one length is 0
!return 1 for compatible types

	if s=t then return 1 fi			!for when used recursively
	if ttbasetype[s]=tarray and ttbasetype[s]=tarray then
		if comparemode(tttarget[s],tttarget[t])=0 then
			return 0
		fi
		if ttlength[s]=0 or ttlength[t]=0 or ttlength[s]=ttlength[t] then
			return 1
		fi
	fi
	return 0
end

function readenumdecl(ref strec owner)int=
	ref strec d,e

	lex()				!skip 'enum'

	if lx.symbol=lcurlysym then				!anonymous enum tag
		readenumnames(owner)
		return tenum			!return generic enum
	fi

	checksymbol(namesym)
	d:=lx.symptr		!should be enum tag
	lex()

	if lx.symbol<>lcurlysym then			!reading incomplete enum
		e:=checkdupl(owner, d, ns_tags, currblockno)

		if e then
			if e.nameid<>enumtagid then
				serror_s("Enum tag in use #",e.name)
			fi
		fi

!create new incomplete enum tag
		e:=createdupldef(owner,d,enumtagid)
		e.mode:=createenummode(e)
		e.blockno:=currblockno
		blockcounts[currblockno]:=1
		return e.mode
	fi

!{ seen, so defining a new enum
	e:=checkdupl(owner,d,ns_tags,currblockno)

	if e then			!found in this linkage
		if e.nameid<>enumtagid then
			serror_s("Enum tag in use #",e.name)
		fi
		if e.deflist then					!else filling in incomplete enum
			serror_s("Redefining enum #",e.name)
		fi
	else						
		e:=createdupldef(owner,d,enumtagid)
		e.mode:=createenummode(e)
		e.blockno:=currblockno
		blockcounts[currblockno]:=1
	fi

!e points to an enum def which has an empty {...} list
!Now loop reading enum values

	readenumnames(owner)

	ttnamedef[e.mode]:=e
	return e.mode
end

proc readenumnames(ref strec owner)=
!at '{'; read set of enum names
	ref strec d,e
	ref strec ulist,ulistx
	int enumseq

	ulist:=ulistx:=nil
	enumseq:=0
	lex()

	case owner.nameid
	when procid,moduleid then		!fine
	else							!probably inside a struct
		owner:=(currproc|currproc|stmodule)
	esac

	while lx.symbol=namesym do
		d:=checkdupl(owner,lx.symptr,ns_general,currblockno)
		if d then
			serror_s("enum name reused #",d.name)
		fi
		d:=createdupldef(owner,lx.symptr,enumid)
		lex()
		if lx.symbol=assignsym then
			lex()
			enumseq:=readconstintexpr()
		fi
		d.index:=enumseq
		d.blockno:=currblockno
		blockcounts[currblockno]:=1
		++enumseq	
		if lx.symbol=commasym then
			lex()
		fi
	od
	skipsymbol(rcurlysym)
end

function createdotop(int opc, unit p,ref strec d)unit=
!opc is jdot or jidot
!Deal with field selection for p.d or p->d
	unit q,r,poffset,pb,pc
	ref strec e,f,prec,panon,pfield,gend
	int m,offset,scale
	ref fieldrec fl

!check that m is a proper pointer if needed, and a struct or union
	m:=p.mode
	if opc=jidot then			!
!	if ttbasetype[m]<>tref then
		if not ttisref[m] then
			serror("-> needs pointer")
		fi
		m:=tttarget[m]
	fi
	case ttbasetype[m]
	when tstruct,tunion then
	else
		serror(". -> not a struct")
	esac

!now need to resolve the field name d
	prec:=ttnamedef[m]				!r is record def

	f:=d
	while f:=f.nextdupl do
		if f.owner=prec then
			offset:=f.offset
			exit
		fi
	od

!not found; look for any anon fields
	if not f then
		gend:=d						!find generic field name version
		while gend.prevdupl do
			gend:=gend.prevdupl
		od

		fl:=prec.nextfield
		while fl do					!now search linear field list matching generic entries
			if fl.gendef=gend then
				f:=fl.def
				offset:=fl.offset
				exit
			fi
			fl:=fl.nextfield
		od
	fi

	if not f then
		terror_ss("Not a field of struct # #",d.name,strmode(m))
	fi


	poffset:=createconstunit(offset,ti32)

!will be p->field, or p.field
!p.field: *(p+offset)

	if opc=jidot then				!apply offset to lhs
		p:=createptrop(p)
	fi

	p:=createunit1(jdot,p)
	p.offset:=offset

	p.mode:=f.mode
	p:=arraytopointer(p)
	fixmemopnd(p)

	return p
end

function mulunit(unit p, int elemtype)unit=
!p is an int unit representing some offset i for *(A+i) or A[i]
!apply a scale so that is a byte offset
!t is the element type
	int elemsize

	if (elemsize:=ttsize[elemtype])<>1 then
		if p.tag=jconst then
			p.value:=p.value*elemsize
		else
			p:=createunit1(jscale,p)
			p.scale:=elemsize
			p.mode:=tptroffset
		fi
	fi
	return p
end

function divunit(unit p, int elemtype)unit=
	int elemsize

	if (elemsize:=ttsize[elemtype])<>1 then
		if p.tag=jconst then
			p.value:=p.value/elemsize
		else
			p:=createunit1(jscale,p)
			p.scale:=-elemsize
			p.mode:=tptroffset
		fi
	fi
	return p
end

function createassignopref(int opc, unit p,q)unit=
!opc is assignsym, addtosym etc
!do assign/addto when is a ref type
!return resulting unit
	int pmode,qmode,rmode,elemmode
	unit r

	pmode:=rmode:=p.mode
	elemmode:=tttarget[pmode]
	qmode:=q.mode

	case opc
	when assignsym then
		q:=coercemode(q,pmode)
		r:=createunit2(jassign,p,q)

	when addtosym then
		if ttisref[qmode] then		!ref+=ref
			serror("ptr+=ptr")
		fi

		q:=coercemode(q,tptroffset)					!ref+=int
		r:=createunit2(jaddto,p,mulunit(q,elemmode))

	when subtosym then
		if ttisref[qmode] then		!ref-=ref
			if not comparemode(pmode,qmode) then
				serror("-= refs don't match")
			fi
			r:=divunit(createunit2(jsub,p,q),elemmode)
			rmode:=ti32
		else								!ref-=int
			r:=createunit2(jsubto,p,mulunit(q,elemmode))
		fi
	else
		serror("Not allowed on ptrs")
	esac

	r.mode:=rmode
	return r
end

proc addnewfield(ref fieldrec &flist, ref strec d, int offset)=
!new field d has just been created for a record
!add it to the linear list of fields for the record
	ref strec e
	ref fieldrec f

	if d.name^<>'$' then			!normal field
		f:=pcm_allocz(f^.bytes)
		f.def:=d
		while d.prevdupl do			!look for generic entry
			d:=d.prevdupl
		od
		f.gendef:=d
		f.offset:=offset

		f.nextfield:=flist
		flist:=f

	else
		e:=ttnamedef[d.mode].deflist
		while e do
			addnewfield(flist,e,offset+e.offset)
			e:=e.nextdef
		od
	fi
end

proc pushloop(int looptype)=
!looptype is 'L' or 'S', ie a switch, so not really a loop
	if loopindex>=maxnestedloops then
		serror("Too many nested loop or switch")
	fi
	++loopindex
	looptypestack[loopindex]:=looptype
	casevaluestack[loopindex]:=nil

end

proc poploop=
	if loopindex then
		--loopindex
	else
		serror("poploop?")
	fi
end

proc addcasevalue(int value)=
	ref caserec p

	int index:=loopindex
	while index and looptypestack[index]<>'S' do
		--index
	od
	if index=0 then serror("case not inside switch stmt") fi

	p:=pcm_alloc(caserec.bytes)
	p.value:=value
	p.nextcase:=casevaluestack[index]
	casevaluestack[index]:=p
end

function roundoffset(int offset, alignment)int=
	int mask

	if structpadding then
		if alignment=1 then return offset fi
		mask:=alignment-1
		while offset iand mask do ++offset od
	fi
	return offset
end

proc fixmemopnd(unit p)=
	int t

!when p refers to a 1- 2- byte value, adjust the type
	if ingeneric then return fi

	case t:= ttbasetype[p.mode]
	when ti8,ti16,tu8,tu16,tbool then
		p.memmode:=t
		p.mode:=ti32
	esac
end

function docast(unit p,int t,hard=1,inplace=0)unit=
!apply cast to unit p
!if no cast needed, then just return p
	unit q
	int s,opc

	s:=p.mode

!CPL "DOCAST", STRMODE(S), STRMODE(T), S, T

	retry:

	if s=t then return p fi
	opc:=0

	if s<16 and t<16 then
		opc:=conversionops[s,t]

	elsif ttisref[s] and ttisref[t] then
		if checkpointertypes(s,t,hard) then
			p.mode:=t
			return p
		fi

	elsif ttconst[s] then
		s:=ttconsttype[s]
		goto retry
	elsif ttconst[t] then
		t:=ttconsttype[t]
		goto retry
!elsif ttisref[t] and (s>=tfirstint and s<=tlastint) and p.tag=jconst and p.value=0 then
	elsif ttisref[t] and isintcc(s) and p.tag=jconst and p.value=0 then
		opc:=soft_c
	fi

	if opc=0 then
		if not hard then
			cpl strmode(s)
			cpl strmode(t)

	PRINTUNIT(NIL,P)

			terror_ss("Can't do conversion # => #",typename(s),typename(t))
		fi
		opc:=hard_c
	fi

	case p.tag
	when jconst then		!try and convert
		if eval_convert(p,t,opc) then
			return p
		fi
	when jfuncname then
		p.mode:=t
		return p
	when jadd then
		if p.a.tag=jconst and p.b.tag=jconst then
			p.value:=p.a.value+p.b.value
			p.mode:=t
			p.tag:=jconst
			return p
		fi
	esac

	if inplace then
		insertunit(p,jconvert)
		p.convmode:=t
		p.mode:=getpromotedtype(t)
		p.opcode:=opc
		return nil
	else
		q:=createunit1(jconvert,p)
		q.opcode:=opc
		q.convmode:=t
		q.mode:=getpromotedtype(t)
	fi
	return q
end

function coercemode(unit p, int t)unit=
	int s,opc
	unit q

	if p.mode=t then return p fi
	docast(p,t,0,1)
	return p
end

proc coercemode_inplace(unit p, int t)=
	int s,opc
	unit q

	if p.mode=t then return fi
	docast(p,t,0,inplace:1)
end

function createsizeofop(unit p, int islength=0)unit=
	unit q
	int t,size

if islength and p.tag not in [jaddptr, jaddrof] then
printunit(nil,p)
 serror("Not array") fi

	t:=getmemmode(p)

	switch p.tag
	when jname then
		if p.alength then
			size:=ttsize[p.def.mode]/p.alength			!take account of array

		else
			size:=ttsize[p.def.mode]			!take account of array
		fi
	when jconst then
		case t
		when trefchar then					!const string
			size:=p.slength+1
		when trefwchar then
			size:=(p.wslength+1)*2
		else
			size:=ttsize[t]
		esac

	when jptr then
		if ttisref[t] and p.alength then		!result of array=>ptr conversion
			size:=ttsize[tttarget[t]]*p.alength
		else
			size:=ttsize[t]
		fi

	when jaddptr then
		if p.alength then	!derived from array expr that converted to pointer
			if islength then
				size:=p.alength
			else
				size:=ttsize[tttarget[t]]*p.alength
			fi

		else
			goto cad1
		fi

	when jaddrof then
		if p.a.tag=jname and p.a.alength then
			if islength then
				size:=p.a.alength
			else
				size:=ttsize[p.a.def.mode]
			fi
		else
			size:=8
		fi

	when jwidenmem then
		return createsizeofop(p.a)

	else
	cad1:
		size:=ttsize[t]
	end switch

	q:=createconstunit(size,tu64)
	return q
end

function readgeneric:unit=
!read generic construct; return chosen expr according to type of control expr
!at '_Generic'
	unit pexpr,pmatch,p
	ref paramrec pm
	int m,t,def,oldingeneric,count
	ref strec d

	lex()
	checksymbol(lbracksym)
	lex()
	oldingeneric:=ingeneric
	ingeneric:=1
	pexpr:=readassignexpr()
	ingeneric:=oldingeneric

	m:=pexpr.mode
	pmatch:=nil
	def:=0
	count:=0

	checksymbol(commasym)

	repeat						!at comma
		lex()					!skip comma
		if lx.symbol=kdefaultsym then
			if def then serror("generic/default twice") fi
			def:=1
			if count=0 then t:=-1 else t:=-2 fi
			lex()
		else
			t:=readcasttype(d,0,pm)
		fi
		checksymbol(colonsym)
		lex()
		p:=readassignexpr()

		if (t=-1 or t=m) then

			pmatch:=p
			++count
		fi
	until lx.symbol<>commasym

	checksymbol(rbracksym)
	lex()
	if not pmatch then serror("Generic: no type match") fi
	if count>1 then serror("Generic: multiple types match") fi

	return pmatch
end

global function getmemmode(unit p)int=
!return mode of p, but if p is a widening unit, see past that to
!the original memory mode
	if p.memmode then
		return p.memmode
	else
		return p.mode
	fi
end

func getpromotedtype(int t)int=
!if t is small, get promoted type
	if t=tvoid then return tvoid fi
	if ttsize[t]<4 then				!all 
		return ti32
	fi
	t
end
=== cc_gentcl.m 0 0 8/21 ===
global int retindex
global int initstaticsindex

const maxnestedloops	= 50

global [maxnestedloops, 4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit
!symbol dcmdskip

int nvarlocals, nvarparams

macro divider = tc_comment("------------------------")

[maxfixedtemp]tempmoderec tempmodes
int nredtemps


global proc codegen_tcl=

	scanstatics()

	scanimports()

	scanprocs()

end

proc scanstatics=
	symbol d, e

	d:=stmodule.deflist

	while d, d:=d.nextdef do
		if d.nameid=staticid and d.scope<>imported_scope then
!			PRINTLN "STATIC:", D.NAME, SCOPENAMES[D.SCOPE]
			dostaticvar(d)
		fi
	od
end

proc scanimports=
	symbol d, e

	d:=stmodule.deflist

	while d, d:=d.nextdef do
		if d.nameid=procid and d.scope=imported_scope then

			tc_setimport(getpsymbol(d))

!			PRINTLN "IMPORT:", D.NAME, SCOPENAMES[D.SCOPE], D.DEFLIST
!			doimportstaticvar(d)

!			e:=d.deflist
!			while e, e:=e.nextdef do
!				if e.nameid=paramid then
!					CPL "	PARAM", E.NAME
!				fi
!			od
		fi
	od


end

proc scanprocs=
	symbol d, e

	d:=stmodule.deflist

	while d, d:=d.nextdef do
		if d.nameid=procid and d.scope<>imported_scope then
!			PRINTLN "PROC:", D.NAME, SCOPENAMES[D.SCOPE]

			doprocdef(d)


!			doimportstaticvar(d)

!			e:=d.deflist
!			while e, e:=e.nextdef do
!				if e.nameid=paramid then
!					CPL "	PARAM", E.NAME
!				fi
!			od
		fi
	od


end


proc dostaticvar(symbol d)=
	psymbol p

	p:=getpsymbol(d)
	tc_addstatic(p)

	do_idata(d)
end

proc doprocdef(symbol d)=
	psymbol p, q
	symbol e

	p:=getpsymbol(d)

	tc_addproc(p)
	tc_currfunc(p)

	e:=d.deflist
	while e, e:=e.nextdef do
		q:=getpsymbol(e)

		case e.nameid
		when paramid then
			tc_addparam(q)
		when frameid then
			tc_addlocal(q)
		when staticid then
			tc_addstatic(q)
			do_idata(e)
		esac
	od

	tcl_start()

!	mmpos:=d.pos

	p.retindex:=++mlabelno
	divider()

	do_stmt(d.code)

	divider()

!	definefwdlabel(retindex)

	if p.ismain then
		tc_gen1(kstop, tc_genint(0))
		setmode(ti32)
	fi

	p.code:=tcl_end()

	tc_currfunc(nil)
	p.maxtemp:=ntemps

!	scanproctemps(p)
!!!	reducetemps(p) when freducetemps and ctarget
!	reducetemps(p)
end

proc do_idata(symbol d)=
	tcl pc

	return unless d.code

	tcl_start()
!	mmpos:=d.pos

	genidata(d.code)

	pc:=tcl_end()

	d.pdef.code:=pc
end

proc genidata(unit p, int doterm=1, am=1, offset=0)=
	int t, length, n, i, j, nwords, offset1, offset2, size, padding, isunion
	unit q, a, b
	ref strec d
	r32 sx
	[256]char str
	[16]char str2

	t:=p.mode
	a:=p.a
	b:=p.b

	case p.tag
	when jmakelist then
		n:=p.count					!number of supplied params
		if ttbasetype[t]=tarray then
			length:=ttlength[t]			!actual length of array
			q:=a
			for i:=1 to n do
				genidata(q)
				q:=q.nextunit
			od
			if n<length then			!rest will be zeros
				doresb((length-n)*ttsize[tttarget[t]])
			fi
		else
			isunion:=ttbasetype[t]=tunion

			d:=ttnamedef[t].deflist
			size:=ttsize[t]				!total bytes in struct
			offset1:=offset2:=0			!offset so far; 1/2 are in idata/in struct
			q:=a
			for i:=1 to n do
				genidata(q, 0)
				offset1+:=ttsize[q.mode]
				d:=d.nextdef
				if d and not isunion then
					offset2:=d.offset
				else
					offset2:=size
				fi

				padding:=offset2-offset1
				if padding>0 then
					doresb(offset2-offset1)
					offset1:=offset2
				fi
				q:=q.nextunit
			od
			if offset2<size then
				doresb(size-offset2)
			fi
		fi
		return
	when jconst then
		if isintcc(t) or isrealcc(t) then
			if t=tr32 then
				sx:=p.xvalue
				tc_gen1(kdata, tc_genint(ref u32(&sx)^))
			else

				tc_gen1(kdata, tc_genint(p.value))
			fi
			setmode(t)
		elsif ttbasetype[t]=tref then
			padding:=0
	doref:
			if p.value=0 then
				tc_gen1(kdata, tc_genint(0))

			elsif p.isstrconst then
				tc_gen1(kdata, tc_genstring(p.svalue))

			elsif p.iswstrconst then
GERROR("GENIDATA/WSTRING2")
				doresb(padding)
			else
				tc_gen1(kdata, tc_genint(p.value))
			fi
			setmode(t)

		elsif ttbasetype[t]=tarray then
			padding:=(ttlength[t]-p.slength)*ttsize[tttarget[t]]
			for i to p.slength do
				tc_gen1(kdata, tc_genint((p.svalue+i-1)^))
				setmode(tu8)
			od
			doresb(padding)

		else
			CPL strmode(t)
			GERROR("IDATA/SCALAR")
		fi
		return
	when jname, jfuncname then
		d:=p.def
		case d.nameid
		when staticid, procid then
			tc_gen1(kdata, genmemaddr_d(d))
			setmode(tu64)

		else
			gerror("Idata &frame", p)
		esac	
		return

	when jadd then
		if a.tag=jname and b.tag=jconst then
			d:=a.def
			case d.nameid
			when staticid then
				strcpy(&.str, "`")
				if d.scope=function_scope then
					strcat(&.str, currproc.name)
					strcat(&.str, ", ")
				fi
				strcat(&.str, d.name)
				strcat(&.str, "+")

				getstrint(b.value, &.str2)

				strcat(&.str, &.str2)
				tc_gen1(kdata, tc_genname(&.str))
			else
				gerror("Add/Idata &frame")
			esac	
		elsif a.tag=jconst and b.tag=jconst and ttbasetype[a.mode]=tref then		!ASSUME REF+REF
			print @&.str, a.value, ,"+", ,b.value
			tc_gen1(kdata, tc_genname(&.str))

		else
			gerror("1:Runtime or unsupported expr in static data")
		fi
		return

	when jaddrof then
		if a.tag=jptr then
			genidata(a.a, offset:offset)
		else
			genidata(a, am:0, offset:offset)
		fi

	when jaddptr, jsubptr then
		if b.tag<>jconst then gerror("Complex ptr expr in static data") fi
		genidata(a, offset:b.value*p.ptrscale+offset)

	when jconvert then
		genidata(a, offset:offset)

	else
		PRINTUNIT(NIL, P)
		gerror("2:Runtime expr in static data", p)
	esac
end

proc doresb(int n)=
	while n>=8 do
		tc_gen1(kdata, tc_genint(0))
		n-:=8
		setmode(tu64)
	od
	to n do
		tc_gen1(kdata, tc_genint(0))
		setmode(tu8)
	od

end

global proc scanproctemps(psymbol d)=
!build templist table for proc d, and fixup ltmode/islast fields in tcl code
	tcl p:=d.code, plast
	int ndest, temp
	ref temprec pt
	tclopnd a
	int ntempts

	ntemps:=d.maxtemp

	if ntemps<=maxfixedtemp then
		templist:=&fixedtemplist
		for i to ntemps do
			memset(templist, 0, ntemps*temprec.bytes)
		od

	else
!		gerror("Can't do that many temps right now")
		templist:=pcm_allocz(ntemps*temprec.bytes)
	fi

	while p, p:=p.next do
		ndest:=p.ndest
!		nopnds:=p.nopnds

		for i to p.nopnds do
			a:=p.abc[i]
			nextloop when a.optype<>temp_opnd
			temp:=a.tempno

			pt:=&templist[temp]
!CPL "LOOP", tclnames[p.opcode], =i, =temp,"//LAST:",,PT.LASTTCL,"LTC:",,PT.LTCOUNT,
!"RTC:",,PT.RTCOUNT,"IX:",,PT.INDEX

			plast:=pt.lasttcl

			if i<=ndest then				!ltemp
				if pt.rtcount then MERROR("SCANT1?") fi

				if plast=nil then			!first write
					if i=1 then
						p.ltmode:=1			!Set all 1st ltemps to 'Tm'
						p.firstlt:=1
!CPL "  FIRST WRITE"
					fi
				else						!subsequent write
					if i>1 then MERROR("SCANT3?") fi
					p.ltmode:=1
				fi

				++pt.ltcount
!CPL " ",=PT.LTCOUNT

			else							!rtemp
				if pt.ltcount=0 or plast=nil then MERROR("SCANT2?") fi

!CPL " ",=PT.RTCOUNT
				if pt.rtcount then			!previous rtemps exist
!CPL "  SUBSEQ RTEMP"
					plast.islast.[pt.index]:=0		!make the last not the last!
				elsif pt.index=1 then		!1st rtemp after leftmost ltemp
!CPL "  FIRST RTEMP",=PT.INDEX
					case pt.ltcount
					when 1 then
						plast.ltmode:=0		!change from tentative Tm/WN to W1 temp
					else					!assume 2 or more
						plast.ltmode:=2		!change last Tm temp to Tx
					esac
				fi

!CPL "  SET ISLAST"
				p.islast.[i]:=1				!assume this will be last rtemp

				++pt.rtcount
			fi

			pt.lasttcl:=p					!always points to last tcl that uses this temp
			pt.index:=i
		od
	od

	if ntemps>maxfixedtemp then
		pcm_free(templist, ntemps*temprec.bytes)
	fi
end

global proc reducetemps(psymbol d)=
!build templist table for proc d, and fixup ltmode/islast fields in tcl code
	tcl p:=d.code, plast
	int ndest, temp
	ref temprec pt
	tclopnd a
	int ntempts

!CPL "REDUCE TEMPS"
	ntemps:=d.maxtemp
	nredtemps:=0

	if ntemps<=maxfixedtemp then
		tempmap:=&fixedtempmap
		memset(tempmap, 0, ntemps)

	else
		tempmap:=pcm_allocz(ntemps)
	fi

	memset(&tempmodes, 0, min(ntemps, maxfixedtemp)*tempmoderec.bytes)

!set up translation map
	while p, p:=p.next do
		for i to p.nopnds do
			maptemp(D,p, i)
		od
	od

!now convert all the temp nos. Note that operands can be shared so can be
!visited more than once; a flag .reduced ensures only converted once
	p:=d.code
	while p, p:=p.next do
		for i to p.nopnds do
			a:=p.abc[i]
			if a.optype=temp_opnd and not a.reduced then
				a.tempno:=tempmap[a.tempno]
				a.reduced:=1
			fi
		od
	od

	if ntemps>maxfixedtemp then
		pcm_free(tempmap, ntemps)
	fi

	d.tempmodes:=pcm_alloc(nredtemps*tempmoderec.bytes)
	for i to nredtemps do
		d.tempmodes[i]:=tempmodes[i]
	od

	d.maxtemp:=nredtemps


!CPL "REDUCED TEMPS FROM",NTEMPS,"TO",NREDTEMPS, D.NAME,"///////",=d.maxtemp

!FI
end

proc maptemp(PSYMBOL D, tcl p, int n)=
	int oldtemp, newtemp
	int mode, size
	tclopnd a:=p.abc[n]
	ref tempmoderec pm

	return when a.optype<>temp_opnd

	oldtemp:=a.tempno

	if tempmap[oldtemp] then				!already mapped
		newtemp:=tempmap[oldtemp]
		finish	
	fi

!assume this is a new ltemp modified for first time.
!first get mode info

	mode:=p.mode
	size:=p.size

	if p.opcode=kcall then
		mode:=a.opmode
		size:=a.opsize
	fi

	for i to nredtemps do
		pm:=&tempmodes[i]
		if pm.used=0 then
			if pm.mode=mode and pm.size=size then
				pm.used:=1
				tempmap[oldtemp]:=newtemp:=i
				finish
			fi
		fi
	od

	if nredtemps>=maxfixedtemp then
		merror("Too many new temps")
	fi

	pm:=&tempmodes[++nredtemps]
	pm.mode:=mode
!CPL "SET NEW TEMPMODE",=NREDTEMPS, MODE

	pm.size:=size
	pm.used:=oldtemp
	tempmap[oldtemp]:=newtemp:=nredtemps

finish:
	if p.islast.[n] then			!last used of it; free this combo
		tempmodes[newtemp].used:=0
	fi
end

=== cc_blocktcl.m 0 0 9/21 ===
[maxnestedloops]int continuestack		!labels for continue/break
[maxnestedloops]int breakstack
int loopindex							!current level of nested loop/switch blocks

global proc do_stmt(unit p) =
	int oldclineno
	unit a, b
	symbol d
	tclopnd tx

	return unless p
!CPL "DO_STMT", JTAGNAMES[P.TAG]

	oldclineno:=clineno
	clineno:=p.lineno
	cfileno:=p.fileno
	mmpos:=cfileno<<24+clineno

	a:=p.a
	b:=p.b

	switch p.tag
	when jblock then
		while a do
			do_stmt(a)
			a:=a.nextunit
		od

	when jdecl then
		do_decl(p.def)

	when jcallfn then
		dx_call(p, a, b, nil, 0)

	when jreturn then
		do_return(p, a)
!
	when jassign then
		do_assign(a, b, 0)

	when jif, jifx then
		do_if(a, b, p.c)

	when jfor then
		do_for(a, b)

	when jwhile then
		do_while(a, b)

	when jdowhile then
		do_dowhile(a, b)

	when jgoto then
		do_goto(p.def)

	when jlabelstmt then
		do_labeldef(p.def)
		do_stmt(a)

!	when jcasestmt then
!
!		do_casestmt(p, a)
!
!	when jdefaultstmt then
!		sw_defaultseen:=1
!		tc_gen(klabel, genlabel(sw_defaultlabel))
!		do_stmt(a)
!
!	when jbreaksw then
!		genjumpl(sw_breaklabel)
!
!	when jbreak then
!		genjumpl(breakstack[loopindex])
!
!	when jcontinue then
!		genjumpl(continuestack[loopindex])
!
!	when jswitch then
!		do_switch(p, a, b)
!
	when jaddto then
		do_binto(a, b, kaddto)

	when jsubto then
		do_binto(a, b, ksubto)

	when jmulto then
		do_binto(a, b, kmulto)

	when jdivto then
		do_binto(a, b, (isrealcc(a.mode)|kdivto|kidivto))

	when jremto then
		do_binto(a, b, kiremto)

	when jiandto then
		do_binto(a, b, kbitandto)

	when jiorto then
		do_binto(a, b, kbitorto)

	when jixorto then
		do_binto(a, b, kbitxorto)

	when jshlto then
		do_binto(a, b, kshlto)

	when jshrto then
		do_binto(a, b, kshrto)

	when jpreincr, jpostincr then
		do_preincr(a, kincrto)

	when jpredecr, jpostdecr then
		do_preincr(a, kdecrto)

	when jexprlist then
		while a do
			do_stmt(a)
			a:=a.nextunit
		od

	else
!assume standalone expression (assign/call/addto/incr done above)
		tx:=dx_expr(p)
		tc_gen1(keval, tx)
		setmode_u((a|a|p))

	end switch

end

func dx_expr(unit p, tclopnd dx=nil, int am=0)tclopnd tx =
	int oldclineno, value, m
	unit a, b
	[256]char str
	symbol d


	return nil unless p

!CPL "DXEXPR", JTAGNAMES[P.TAG], =am
!PRINTUNIT(NIL, P)

	oldclineno:=clineno
	clineno:=p.lineno
	cfileno:=p.fileno

	a:=p.a
	b:=p.b
	m:=p.mode

	tx:=nil

	switch p.tag
	when jconst then
		tx:=dx_const(p)
!
	when jname then
		tx:=dx_name(p, am)

!	when jwidenmem then
!		dx_expr(a, am)
!
!	when jfuncname then
!		tc_gen(kload, genmemaddr_d(p.def))
!		setmode(tu64)
!
	when jassign then
		tx:=do_assign(a, b, 1)
!!!
!	when jandl, jorl then
!		dx_andorl(p)		!use non-short circuit versions for now
!
	when jnotl then
		if a.tag=jnotl then
			tc_gen2(ktoboolt, tx:=gendest(dx), dx_expr(a.a))
			setmode(tu32)
			setmode2(a.a.mode)
		else
			tc_gen2(knot, tx:=gendest(dx), dx_expr(a))
			setmode_u(a)
		fi

	when jistruel then
!		dx_expr(a)
!		tc_gen(ktoboolt)
!		setmode(tu32)
!		setmode2(a.mode)
!
	when jexprlist then
		while a, a:=b do
			b:=a.nextunit

			if b and a.tag in [jassign, jconvert, jifx] then
				do_stmt(a)
			else
				tx:=dx_expr(a)
				if b and (a.mode<>tvoid or a.tag=jconvert) then
					tc_gen1(keval, tx)
				fi
			fi
		od

	when jcallfn then
		tx:=dx_call(p, a, b, dx, 1)

	when jifx then
		tx:= dx_ifx(p, a, b, p.c, dx)

!	when jeq, jne, jlt, jle, jge, jgt then
!		dx_eq(p, a, b)
!
	when jadd then
		if ttisref[a.mode] and ttsize[b.mode]<=4 then
			b.mode:=tu64
		fi
		tx:=dx_bin(a, b, kadd, dx)

	when jsub then
		tx:=dx_bin(a, b, ksub, dx)
!
	when jmul then
		tx:=dx_bin(a, b, kmul, dx)

	when jdiv then
		tx:=dx_bin(a, b, (isrealcc(a.mode)|kdiv|kidiv), dx)
!
	when jrem then
		tx:=dx_bin(a, b, kirem, dx)
!
	when jiand then
		tx:=dx_bin(a, b, kbitand, dx)
!
	when jior then
		tx:=dx_bin(a, b, kbitor, dx)
!
	when jixor then
		tx:=dx_bin(a, b, kbitxor, dx)
!
	when jshl then
		tx:=dx_bin(a, b, kshl, dx)
!
	when jshr then
		tx:=dx_bin(a, b, kshr, dx)
!
	when jptr then
		tx:=dx_ptr(p, a, am)

	when  jaddptr then
		tx:=dx_addptr(p, a, b, dx)
!
!	when  jsubptr then
!		dx_addptr(p, a, b, ksubpx, am)
!
	when jconvert then
		if p.convmode=tvoid then
			tx:=dx_expr(a)
		else
			tx:=dx_convert(p, a, p.convmode, p.opcode)
		fi

!	when jscale then
!		dx_scale(p, a, b)
!
	when jneg then
		tc_gen2(kneg, tx:=gendest(dx), dx_expr(a))
		setmode_u(a)

	when jinot then
		tc_gen2(kbitnot, tx:=gendest(dx), dx_expr(a))
		setmode_u(a)

	when jpreincr, jpredecr, jpostincr, jpostdecr then
		tx:=dx_incrload(p, a, dx)

!	when jpostincr, jpostdecr then
!		tx:=dx_postincrx(p, a, dx)

	when jaddto then
		dx_bintox(a, b, kaddto)

!	when jsubto then
!		dx_bintox(a, b, ksubto)
!
!	when jmulto then
!		dx_bintox(a, b, kmulto)
!
!	when jdivto then
!		dx_bintox(a, b, kdivto)
!
!	when jremto then
!		dx_bintox(a, b, kiremto)
!
!	when jiandto then
!		dx_bintox(a, b, kbitandto)
!
!	when jiorto then
!		dx_bintox(a, b, kbitorto)
!
!	when jixorto then
!		dx_bintox(a, b, kbitxorto)
!
!	when jshlto then
!		dx_bintox(a, b, kshlto)
!
!	when jshrto then
!		dx_bintox(a, b, kshrto)
!
	when jaddrof then
		tx:=dx_expr(a, dx, 1)

	when jdot then
		tx:=dx_dot(p, a, b, dx, am)

!	when jsetjmp then
!		dx_expr(a)
!		tc_gen(ksetjmp)
!
!	when jlongjmp then
!		dx_expr(a)
!		dx_expr(b)
!		tc_gen(klongjmp)

	else
		gerror_s("DX-EXPR: can't do tag: #", jtagnames[p.tag])
	end switch

	clineno:=oldclineno

IF TX=NIL THEN GERROR("DX-EXPR: TX=NIL") FI

	tx
end

func dx_name(unit p, int am)tclopnd tx=
	symbol d:=p.def

	case d.nameid
	when staticid, frameid, paramid then
		if am then
			tx:=genmemaddr_d(d)
		else
			tx:=genmem_d(d)
			tx:=widenname(p, tx)
		fi
	else
		gerror("dxname")
	esac
	tx
end

func widenname(unit p, tclopnd dx)tclopnd tx=

	if p.memmode=tvoid then
		return dx
	fi

	int mode:=getmemmode(p)

CPL "WIDEN", STRMODE(MODE)

	if ttsize[mode]<4 then
		tc_gen2(kwiden, tx:=tc_gentemp(), dx)
		setmode((mode in [ti8, ti16]|ti32|tu32))
		setmode2(mode)
		tx
	else
		dx
	fi
end

func widen(unit p, tclopnd dx)tclopnd tx=

	if p.memmode=tvoid then
		return dx
	fi

	int mode:=getmemmode(p)

CPL "WIDEN", STRMODE(MODE)

	if ttsize[mode]<4 then
		if tccurr.opcode in [kmove, kiloadx, kincrload, kdecrload, kloadincr, kloaddecr] then
dowiden:
			tc_gen2(kwiden, tx:=tc_gentemp(), dx)
			setmode((mode in [ti8, ti16]|ti32|tu32))
			setmode2(mode)
		elsif tccurr.opcode=kcall and tccurr.nret then
			dowiden
		fi
	else
		tx:=dx
	fi

	tx

end

func dx_const(unit p)tclopnd tx=
	int t:=ttbasetype[p.mode]

	if t in tfirstint..tlastint then
		tc_genint(p.value)

	elsecase t
	when tr32 then
		tc_genr32(p.xvalue)

	when tr64 then
		tc_genreal(p.xvalue)

	elsif t=tref then
		if p.isstrconst then
			tc_genstring(p.svalue)
		elsif p.iswstrconst then
			GERROR("CONST/WSTRING")
			nil
		else
			tc_genint(p.value)
		fi
	else
		gerror("const?")
		nil
	fi
end

func do_assign(unit a, b, int res)tclopnd tx=
	tclopnd lhs, rhs

	rhs:=dx_expr(b)

	case a.tag
	when jname then
		tc_gen2(kmove, genmem_u(a), rhs)
		setmode(getmemmode(a))

	when jptr then
		tc_gen_ix(kistorex, dx_expr(a, am:1), nil, rhs)
		setmode(getmemmode(a))

	when jdot then
!		dx_expr(a.a, 1)
!		tc_gen(kload, genint(a.offset))
!		setmode(tu64)
!		tc_genix(kaddpx)
!		tc_setscaleoff(1)
!		setmode(getmemmode(a))
!
!		tc_genix(kistore)
		tc_gen_ix(kistorex, dx_expr(a, am:1), nil, rhs, offset:a.offset)


!		tc_setscaleoff(1)
		setmode(getmemmode(a))

	else
		GERROR_S("DOASSIGN not ready: #", jtagnames[a.tag])
	esac

	rhs
end

func dx_bin(unit a, b, int opc, tclopnd dx)tclopnd tx=
	tclopnd ax, bx

	ax:=dx_expr(a)
	bx:=dx_expr(b)
	tx:=gendest(dx)

	tc_gen3(opc, tx, ax, bx)
	setmode(a.mode)
	tx
end

proc do_while (unit pcond, pbody) =
	int lab_b, lab_c, lab_d

	if pcond.tag=jconst and pcond.value then
		do_while1(pbody)
		return
	fi

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	genjumpl(lab_c)		!direct to condition code which is at the end

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	genjumpcond(kjumpt, pcond, lab_b)
!	setmode_u(pcond)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_while1 (unit pbody) =
	int lab_b, lab_c, lab_d

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	genjumpl(lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc stacklooplabels(int a, b)=
	!don't check for loop depth as that has been done during parsing
	continuestack[++loopindex]:=a
	breakstack[loopindex]:=b
end

proc genjumpcond(int opc, unit p, int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q, r
	int lab2

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

	when jeq, jne, jlt, jle, jge, jgt then
		gcomparejump(opc, p, q, r, lab)

	when jexprlist then
		while q and (r:=q.nextunit) do
			do_stmt(q)
			q:=r
		od

		genjumpcond(opc, q, lab)
	else			!other expression
		tc_gen2(opc, tc_genlabel(lab), dx_expr(p))
		setmode_u(p)
	end switch
end

proc gcomparejump(int jumpopc, unit p, lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int cond
	tclopnd a, b

	cond:=getpclcond(p.tag)			!jeq => keq etc
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	a:=dx_expr(lhs)
	b:=dx_expr(rhs)

	tc_gen3(kjumpcc, tc_genlabel(lab), a, b)
	tccurr.cond:=cond
	setmode_u(lhs)

end

function getpclcond(int op)int=
	case op
	when jeq then return eq_cc
	when jne then return ne_cc
	when jlt then return lt_cc
	when jle then return le_cc
	when jge then return ge_cc
	when jgt then return gt_cc
	esac
	return 0
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

proc genjumpl(int lab)=
!generate unconditional jump to label
	tc_gen1(kjump, tc_genlabel(lab))
end

proc do_if(unit a, b, c)=
	int lab1, lab2

	lab1:=createfwdlabel()

	genjumpcond(kjumpf, a, lab1)

	do_stmt(b)

	if c then
		lab2:=createfwdlabel()			!label past else part
		genjumpl(lab2)
		definefwdlabel(lab1)
		do_stmt(c)
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_return(unit p, a)=
	if a then
		if currfunc.ismain then
			tc_gen1(kstop, dx_expr(a))
		else
			tc_gen1(kretfn, dx_expr(a))
			setmode_u(a)
		fi
	else
		tc_gen0(kretproc)
	fi
end

func dx_call(unit p, a, b, tclopnd dx, int res)tclopnd=
	ref paramrec pm
	int isfnptr, variadic, nparams, retmode, nbytes, m, nvariadics
	int nfixedparams, isfn, nret
	[maxparam]unit paramlist
	[maxparam]tclopnd paramopnds
	tclopnd ax, tx
	symbol dblock, dtemp
	unit q

	nret:=0
	if res and p.mode<>tvoid then nret:=1 fi

	tx:=nil
	isfn:=0

	case a.tag
	when jptr then
		m:=a.mode
		while ttbasetype[m]=tref do
			m:=tttarget[m]
		od

		isfn:=tttarget[m]<>tvoid
		pm:=ttparams[m]
		isfnptr:=1

	else
		pm:=a.def.paramlist
		isfnptr:=0
		isfn:=a.def.mode<>tvoid
	esac

	variadic:=pm.flags=pm_variadic
	nfixedparams:=pm.nparams
	nparams:=nvariadics:=0

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparam then gerror("maxparams") fi
		paramlist[++nparams]:=q

		if variadic and nparams>nfixedparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		fi
		if nparams<=nfixedparams then
			pm:=pm.nextparam
		fi
	od

	for i:=nparams downto 1 do			!downto 
		paramopnds[i]:=dx_expr(paramlist[i])
		setopndmode(paramopnds[i], paramlist[i].mode)
	od

	for i to nparams do
		extparamopnds[i]:=paramopnds[i]

		if i<=4 and variadic and i>nfixedparams and i<=4 then
			extparamopnds[i].isvariadic:=1			!whether params pushed as variadic
		fi
	od

	IF NOT ISFNPTR THEN
		AX:=GENMEMADDR_D(A.DEF)
	ELSE
		ax:=dx_expr(a, am:1)
	FI

	if nret=0 then				!proc
		tc_gen_call(ax, 0, nparams)

	else						!func
		tx:=extretopnds[1]:=gendest(dx)
		setopndmode(tx, p.mode)
		tc_gen_call(ax, 1, nparams)
	fi

	if variadic then
		tccurr.isvariadic:=1
	fi

	tccurr.nargs:=nparams
	tccurr.isvariadic:=nvariadics

	return tx
end

proc do_for (unit pinit, pbody) =
	unit pcond, pincr
	int lab_b, lab_c, lab_d, lab_cond

	pcond:=pinit.nextunit
	pincr:=pcond.nextunit

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_cond:=createfwdlabel()

	if pinit.tag<>jnull then
		do_stmt(pinit)
	fi

	genjumpl(lab_cond)		!direct to condition code which is at the end

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	do_stmt(pincr)
	definefwdlabel(lab_cond)

	if pcond.tag<>jnull then
		genjumpcond(kjumpt, pcond, lab_b)
	else
		genjumpl(lab_b)
	fi
	definefwdlabel(lab_d)
	--loopindex
end

proc do_decl(symbol d)=
	unit a

	a:=d.code
	d.used:=1
	if d.pdef then d.pdef.used:=1 fi

	if a.tag<>jmakelist then
		if ttbasetype[d.mode]=tarray and a.tag=jconst then	!probably string lit
			goto copyl
		fi
		tc_gen2(kmove, genmem_d(d), dx_expr(a))
		setmode(a.mode)
		return
	fi

copyl:
	tc_gen2(kmove, genmem_d(d), tc_genmem(d.pdata))
	setmode(d.mode)
end

proc do_preincr(unit a, int incrop)=
	tclopnd px

	if a.tag=jname then
		tc_gen1(incrop, genmem_u(a))
	else
		px:=evallv(a)
		tc_gen1(incrop, PX)
	fi
	setmode(getmemmode(a))
	setincrstep(a.mode)
end

func evallv(unit p, tclopnd dx=nil)tclopnd tx=
	tx:=makeindlv(dx_expr(p, am:1), p, p.mode)
	return tx
end

proc setincrstep(int m)=
	tccurr.step:=1

	if ttisref[m] then
		tccurr.step:=ttsize[tttarget[m]]
	fi
end

proc do_binto(unit a, b, int opc)=
!res=1 means value must be retained
	tclopnd px, bx

	bx:=dx_expr(b)

	if a.tag=jname then
		px:=genmem_u(a)
	else
		px:=evallv(a)
	fi

	tc_gen2(opc, px, bx)
	setmode(getmemmode(a))
end

func dx_bintox(unit a, b, int opc)tclopnd dx=
!res=1 means value must be retained
	tclopnd px, bx

GERROR("BINTOX NOT READY")

	bx:=dx_expr(b)

	if a.tag=jname then
		px:=genmem_u(a)
	else
		px:=evallv(a)
	fi

	tc_gen2(opc, px, bx)
	setmode(getmemmode(a))

	nil
end

func dx_ptr(unit p, a, int am)tclopnd tx=
	if am=0 then				!normal ptr deref
		tc_gen_ix(kiloadx, tx:=tc_gentemp(), dx_expr(a))
		setmode(getmemmode(p))
		widen(p, tx)
	else						!else just load the pointer
		dx_expr(a)
	fi
end

func dx_addptr(unit p, a, b, tclopnd dx)tclopnd tx=
	tc_gen_ix(kaddpx, tx:=gendest(dx), dx_expr(a), dx_expr(b), p.ptrscale)
	setmode(tu64)
	tx
end

func dx_convert(unit p, a, int t, opc)tclopnd tx=
!convert unit a to type t, using conversion opc (uwiden_c etc)
	int s, ssize, tsize
	tclopnd ax

	s:=a.mode

	ssize:=ttsize[s]
	tsize:=ttsize[t]

	ax:=dx_expr(a)

	if opc=soft_c then
		return ax
	fi

	tx:=tc_gentemp()

	case opc
	when hard_c then
!hard is an explicit cast for which no built-in code such as swiden_c has
!been detected. So just do a kind of type-punning, but ensure the sizes
!are correct

!		if stdcat[ttbasetype[s]]=realcat then gerror("Bad cast") fi
		if ttbasetype[s] in [tr32, tr64] then gerror("Bad cast") fi

		if tsize>ssize then			!widen
			tc_gen2(kwiden, tx, ax)
		elsif tsize<ssize then
			goto dotruncate
			tc_deltemp()
			return ax
		fi

	when swiden_c, uwiden_c then
		if ssize=tsize then
			tc_deltemp()
			return ax
		fi
		tc_gen2(kwiden, tx, ax)

	when sfloat_c, ufloat_c then
		tc_gen2(kfloat, tx, ax)

	when sfix_c, ufix_c then
		tc_gen2(kfix, tx, ax)

	when fwiden_c then
		tc_gen2(kfwiden,tx, ax)

	when fnarrow_c then
		tc_gen2(kfnarrow, tx, ax)

	when narrow_c, truncate_c then
dotruncate:
		tc_gen2(ktruncate, tx, ax)

		setmode(ti32)
		setmode2(t)
		return tx

	else
		gerror_s("Convert op not implem: #", convnames[opc])
	esac

	setmode(t)
	setmode2(s)
	tx
end

func dx_incrload(unit p, a, tclopnd dx)tclopnd tx=
!also loadincr
	tclopnd px	
	int opc

	case p.tag
	when jpreincr then opc:=kincrload
	when jpredecr then opc:=kdecrload
	when jpostincr then opc:=kloadincr
	else				opc:=kloaddecr
	esac

	tx:=gendest(dx)

	if a.tag=jname then
		tc_gen2(opc, tx, genmem_u(a))
	else
		tc_gen2(opc, tx, evallv(a))
	fi
	
	setincrstep(a.mode)
	setmode(getmemmode(a))

	tx:=widen(a, tx)
end

proc do_dowhile (unit pbody, pcond) =
	int lab_b, lab_c, lab_d

	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_c, lab_d)

	lab_b:=definelabel()

	do_stmt(pbody)

	definefwdlabel(lab_c)

	unless iscondfalse(pcond) then
		genjumpcond(kjumpt, pcond, lab_b)
	end

	definefwdlabel(lab_d)
	--loopindex
end

func dx_ifx(unit p, a, b, c, tclopnd dx)tclopnd tx=
	int lab1, lab2

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
	lab2:=createfwdlabel()

	tx:=gendest(dx)

	genjumpcond(kjumpf, a, lab1)

	tc_gen2(kmove, tx, dx_expr(b))

	genjumpl(lab2)
	definefwdlabel(lab1)

	tc_gen2(kmove, tx, dx_expr(c))

	definefwdlabel(lab2)

	tx
end

func dx_dot(unit p, a, b, tclopnd dx, int am)tclopnd tx=
	tclopnd px

	tx:=gendest(dx)
!	px:=evallv(a)
	px:=dx_expr(a, am:1)

	if am=1 then
		tc_gen_ix(kaddpx, tx, px, dx_expr(b), offset:p.offset)
		setmode(tu64)
	else
		tc_gen_ix(kiloadx, tx, px, dx_expr(b), offset:p.offset)
		setmode(getmemmode(p))
		tx:=widen(p, tx)
	fi

	tx
end

proc do_labeldef(symbol d)=
	if d.index<=0 then			!not already dealt with via goto
		d.index:=++mlabelno
	fi

	tc_comment(d.name)
CPL =D.INDEX

	tc_gen1(klabel, tc_genlabel(d.index))
end

proc do_goto(symbol d)=
	if d.index=0 then
		gerror_s("Label not defined: #", d.name)
	elsif d.index<0 then
		d.index:=++mlabelno	
	fi
	tc_gen1(kjump, tc_genlabel(d.index))
end

=== cc_libtcl.m 0 0 10/21 ===
!const freduce=1
const freduce=0

global [maxtuplesize]tclopnd extretopnds		!temps to hold func results
global [maxparam]tclopnd extparamopnds

global func getpsymbol(symbol d)psymbol p=
	symbol e
	ichar name
	[256]char str

	return nil when d=nil

	if d.pdef then return d.pdef fi

!CPL "GETPS", D.NAME
	name:=d.name

	if d.nameid in [frameid, paramid] then
		strcpy(str, d.name)
		if d.blockno>1 then
			strcat(str, ".")
			strcat(str, strint(d.blockno))
		fi
	elsif d.nameid=staticid and d.owner and d.owner.nameid=procid then
		strcpy(str, d.owner.name)
		strcat(str, ".")
		strcat(str, d.name)
		if d.blockno>1 then
			strcat(str, ".")
        	strcat(str, strint(d.blockno))
		fi
	else
		strcpy(str, d.name)
	fi

	d.pdef:=p:=tc_makesymbol(str, name2pid[d.nameid])

	p.mode:=gettclmode(d.mode)

	p.size:=ttsize[d.mode]
!	p.pos:=d.pos

	if d.scope=exported_scope then p.exported:=1 fi
	if d.scope=imported_scope then p.imported:=1; p.id:=import_id fi
	p.used:=d.used

	p.labelno:=d.index

	if d.nameid=procid and eqstring(d.name, "main") then
		d.ismain:=p.ismain:=1
	fi

	if d.nameid=procid then
		ref paramrec pm:=d.paramlist
		if pm.flags=pm_variadic then
			p.variadic:=pm.nparams			!imported, local or exported
		fi
	fi


!	p.varparams:=d.varparams
!	p.align:=getalignment(d.mode)		!mainly for vars, but no harm for procs etc

!	e:=d.owner
!	if ctarget and d.nameid=staticid and e and e.nameid=procid and d.code then
!		p.cprocowner:=e.pdef
!		e.pdef.chasstatics:=1
!	fi

	return p
end

global proc setmode(int mode)=
	tc_setmode(gettclmode(mode), ttsize[mode])
end

global proc setmode2(int mode)=
	tc_setmode2(gettclmode(mode))
end

global proc setmode_u(unit p)=
	int mode:=p.mode
	tc_setmode(gettclmode(mode), ttsize[mode])
end

global func genmem_u(unit p)tclopnd=
	tc_genmem(getpsymbol(p.def))
end

global func genmem_d(symbol d)tclopnd=
	tc_genmem(getpsymbol(d))
end

global func genmemaddr_d(symbol d)tclopnd=
	tc_genmemaddr(getpsymbol(d))
end

global func genmemaddr_u(unit p)tclopnd=
	return tc_genmemaddr(getpsymbol(p.def))
end

global proc checkaddpx(tcl p, int id=0)=
!addpx/loadpx has just been generated; see if it can be combined with previous addpx
!assume it looks like this:
! p  T1 := bp  + cp*sp + extrap        (p)
! q  T2 := T1 +  cq*sq + extraq        (q:=tccurr)
!For this to work, p must be addpx, q is addpx/loadpx, and cq must be int-opnd or
! be missing; T1 must only be used in these 2 ops

	tcl q:=tccurr

!CPL "CHECK1",TCLNAMES[P.OPCODE], =ID
	return unless freduce
CPL "CHECK2"

	return unless p.opcode=kaddpx
CPL "CHECK3"
	return unless q.c=nil or q.c.optype=int_opnd
CPL "CHECK4"
	return unless p.a=q.b
CPL "CHECK5"
	return unless q.islast.[2] = 1
CPL "CHECK6"

	p.a:=q.a							!move T2 over to P

	if q.c then							!I think that .c is optional
		p.extra +:= q.c.value*q.scale
	fi
	p.extra +:= q.extra
	p.opcode := q.opcode				!move opcode in case loadpx

	tccurr:=p							!discard new tcl op
end

global func gendest(tclopnd dx)tclopnd=
	if dx then return dx fi
	return tc_gentemp()
end

global func definelabel:int =
!	tc_gen(klabel, tc_genlabel(++mlabelno))
	tc_gen1(klabel, tc_genlabel(++mlabelno))
	return mlabelno
end

global func createfwdlabel:int =
	return ++mlabelno
end

global proc definefwdlabel(int lab) =
	tc_gen1(klabel, tc_genlabel(lab))
end

global proc setopndmode(tclopnd p, int m)=
	p.opmode:=gettclmode(m)
	p.opsize:=ttsize[m]
end

global func makeind(tclopnd a, unit q, int m)tclopnd p=
	tc_makeind(a, q, gettclmode(m), ttsize[m])
end

global func makeindlv(tclopnd a, unit q, int m, size=0)tclopnd p=
	tc_makeindlv(a, q, gettclmode(m), ttsize[m])
end


=== cc_lib.m 0 0 11/21 ===
global int autotypeno=0
global int nextafindex=0

const int unitheapsize=50000
ref unitrec unitheapptr=nil
int remainingunits=0

function newstrec:ref strec=
	ref strec p
	p:=pcm_alloc(strec.bytes)
!	memset(p,0,strec.bytes)
	clear p^

	p.lineno:=lx.lineno
	p.fileno:=lx.fileno

	return p
end

global proc initcclib=

end

global function createname(ref strec p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=jname
	u.def:=p

	return u
end

global function createunit0(int tag)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
	ref unitrec u

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

global function createconstunit(u64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.value:=a
	u.mode:=t
	return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.svalue:=s
	u.mode:=trefchar
	if length=-1 then
		u.slength:=strlen(s)
	else
		u.slength:=length
	fi
	u.isstrconst:=1
	return u
end

global function createwstringconstunit(ref u16 s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=jconst
	u.wsvalue:=s
	u.mode:=trefwchar
!if length=-1 then
!	u.slength:=strlen(s)
!else
		u.wslength:=length
!fi
	u.iswstrconst:=1
	return u
end

global function getoptocode(int opc)int=		!GETOPTOCODE
!opc is kadd etc
!return matching kaddto, etc
	static [0:jtagnames.len]i16 opctotable
	int n,opcto,i
	[20]char str

	opcto:=opctotable[opc]
	if opcto then return opcto fi				!find

!assume memoising table not filled in for this opc

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

global function getconstvalue(ref unitrec p,int ID=0)i64=	!GETCONSTVALUE
!extract value from kconst
	if p and p.tag=jconst then
		return p.value
	fi
	serror("GCV Not constant")
	return 0
end

global function nextautotype:ichar=
	static [32]char str

!sprintf(&.str,"$T%d",i32(++autotypeno))
	print @&.str,"$T",,++autotypeno
	return &.str
end

global function createconstmode(int m)int=
!create const version of mode m
	int newm
	if ttconst[m] then return m fi
	if ttconsttype[m] then return ttconsttype[m] fi
	newm:=copymode(m)
	ttconsttype[m]:=newm
	ttconst[newm]:=1

	ttconsttype[newm]:=m			!use consttype to point back as well as forwards
!	ttcat[newm]:=ttcat[m]

	return newm
end

global function createrefmode(int m)int=
!create ref version of mode m (including when m is already a ref)
	int newm

	if ttreftype[m] then
		++ttshared[ttreftype[m]]
		return ttreftype[m]
	fi
	newm:=createnewmode(tref)
	ttreftype[m]:=newm
	tttarget[newm]:=m
	ttisref[newm]:=1
!	ttcat[newm]:=d64cat

	return newm
end

global function createprocmode(int m, ref paramrec pm)int=
!create proc mode with return type
	int newm

	newm:=createnewmode(tproc)
	ttparams[newm]:=pm
	tttarget[newm]:=m
!	ttcat[newm]:=d64cat
	return newm
end

global function createarraymode(int m, length)int=
!create array of mode m (including when m is already a ref)
	int newm

	newm:=createnewmode(tarray)
	tttarget[newm]:=m
	ttlength[newm]:=length
	ttsize[newm]:=length*ttsize[m]
!	ttcat[newm]:=blockcat
	ttisblock[newm]:=1

	return newm
end

global function createenummode(ref strec e)int=
	int newm
	newm:=createnewmode(tenum)
	ttnamedef[newm]:=e

	return newm
end

global function createstructmode(ref strec s,int smode)int=
	int newm
	newm:=createnewmode(smode)
	ttnamedef[newm]:=s
	ttisblock[newm]:=1

	return newm
end

global proc setnameptr(ref unitrec p)=		!SETNAMEPTR
!p is a just created j...def unit which has a nameptr in the .a parameter
!set up an xref from the strec back to the -def unit
!Set up a pointer in the associated strec that points back to q

	p.def.code:=p
end

global function getautofieldname:ref strec=
!create auto-field name and return pointer to st entry
	[32]char str
	ichar name

!sprintf(&.str,"$F%d",i32(++nextafindex))
	print @&.str,"$F",,++nextafindex

	name:=pcm_copyheapstring(&.str)
	return addnamestr(name)
end

global func convertstringc(ichar s, t,int length=-1)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
	int c
	[20]char str
	ichar t0

	if length=-1 then
		length:=strlen(s)
	fi

	t0:=t

	to length do
		c:=s++^
		switch c
		when '"' then
			t++^:='\\'
			t++^:='"'
		when '\'' then
			t++^:='\\'
			t++^:='\''
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
		when 7 then
			t++^:='\\'
			t++^:='a'
		when 8 then
			t++^:='\\'
			t++^:='b'
		when 12 then
			t++^:='\\'
			t++^:='f'
		when 11 then
			t++^:='\\'
			t++^:='v'
		else
			if c<32 or c>=127 then
!			sprintf(&.str,"\\%03o",i32(c))
				fprint @&.str,"\\#o",c:"z3"
				t++^:=str[1]
				t++^:=str[2]
				t++^:=str[3]
				t++^:=str[4]
			else
				t++^:=c
			fi
		end switch
	od
	t^:=0
	return t-t0
end

global function getopcjname(int opc)ichar=		!GETOPCJNAME
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
	static [20]char str
	ichar name,s

	name:=jtagnames[opc]
	s:=strchr(name,' ')
	if s then
		memcpy(&.str,name,s-name)
		str[s-name+1]:=0
		return &.str
	else
		return name
	fi
end

global function strmode(int m,expand=1)ichar=		!STRMODE
	static [16384]char str

	istrmode(m,expand,&.str)

	return &.str
end

global function strmode2(int m,expand=1)ichar=		!STRMODE
	static [16384]char str

	istrmode(m,expand,&.str)

	return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=		!ISTRMODE
	ref strec d,q
	int value,needcomma,x,i,target,t,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim,slength
	[100]char strdim,strlength
	ref paramrec pm

	if m<tlast then
		strcpy(dest,typename(m))
		return
	fi

	t:=ttbasetype[m]
	case t
	when tref then
		if ttconst[m] then
			strcpy(dest,"const ref ")
		else
			strcpy(dest,"ref ")
		fi
		target:=tttarget[m]
		if target>=0 and ttbasetype[tttarget[m]]=tstruct then
			strcat(dest,typename(tttarget[m]))
		else
			istrmode(tttarget[m],0,dest+strlen(dest))
		fi
	when tarray then
		if ttlength[m] then
			fprint @dest,"[#]",ttlength[m]
		else
			strcpy(dest,"[]")
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tenum then
		strcpy(dest,"enum ")
		strcat(dest,typename(m))

	when tstruct,tunion then
		if not expand then
			strcpy(dest,typename(m))
			return
		fi

		strcpy(dest,typename(ttbasetype[m]))
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist
		while q do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,typename(m))

	when tproc then
		strcpy(dest,"proc(")
		pm:=ttparams[m]
		n:=pm.nparams
		for i to n do
			istrmode(pm.mode,0,dest+strlen(dest))
			if i<>n then
				strcat(dest,",")
			fi
			pm:=pm.nextparam
		od
		strcat(dest,")")
		istrmode(tttarget[m],0,dest+strlen(dest))

	elsif t<tlast then
		strcpy(dest,typename(m))
		return
	else
	CPL typename(m)
		mcerror("NEWSTRMODE")
	esac
end

global function typename(int m)ichar=
	int basem
	static [300]char str

	basem:=ttbasetype[m]
	case basem
	when tstruct,tunion then
		strcpy(&.str,(basem=tstruct|"struct "|"union "))
		if ttnamedef[m] then
			strcat(&.str,ttnamedef[m].name)
			strcat(&.str,".")
			strcat(&.str,strint(ttnamedef[m].blockno))
		fi
		return &.str
	when tarray then
		return "<array>"
	when tenum then
		if ttnamedef[m] then
			return ttnamedef[m].name
		fi
		return "<enum>"
	else
		if ttconst[m] then
			strcpy(&.str,"const ")
			strcat(&.str,stdtypenames[basem])
			return &.str
		fi
		return stdtypenames[basem]
	esac
	return ""
end

global function allocunitrec:ref unitrec=
	ref unitrec p
	ref i64 q
	int nwords

	++nunits

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.lineno:=lx.lineno

		if lx.fileno<=255 then
			p.fileno:=lx.fileno
		fi
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
	p.lineno:=lx.lineno
	if lx.fileno<=255 then
		p.fileno:=lx.fileno
	fi
	return p
end

function copymode(int m)int=
	if ntypes>=maxtype then
		serror("Too many types")
	fi
	++ntypes

!copy fields that won't already be zero
	ttnamedef[ntypes]:=ttnamedef[m]
	ttbasetype[ntypes]:=ttbasetype[m]
	ttlength[ntypes]:=ttlength[m]
	ttconst[ntypes]:=ttconst[m]
	ttsize[ntypes]:=ttsize[m]
	tttarget[ntypes]:=tttarget[m]
	ttparams[ntypes]:=ttparams[m]
	ttisref[ntypes]:=ttisref[m]
	ttisblock[ntypes]:=ttisblock[m]

	return ntypes
end

function createnewmode(int m)int=
!create new type unitialised except for given basetype m

	if ntypes>=maxtype then
		CPL =STRMODE(M)
		serror("Too many types/cnm")
	fi
	++ntypes

!leave length, const etc all zero
!copy basic size info from basetype

	ttbasetype[ntypes]:=m
	ttsize[ntypes]:=ttsize[m]

	return ntypes
end

global proc addlistunit(ref ref unitrec ulist,ulistx,ref unitrec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextunit:=p
	fi
	p.nextunit:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc addlistdef(ref ref strec ulist,ulistx,ref strec p)=
!add strec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextdef:=p
	fi
	p.nextdef:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc addlistparam(ref ref paramrec ulist,ulistx,ref paramrec p)=
!add paramrec p to end of linked list headed by ulist. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextparam:=p
	fi
	p.nextparam:=nil

	ulistx^:=p			!update end-of-list pointer
end

global proc checksymbol(int symbol)=
	[256]char str

	if lx.symbol<>symbol then
		fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]

	if lx.symbol=namesym then
		strcat(&.str," \"")
		strcat(&.str,getstname(lx.symptr))
		strcat(&.str,"\"")
	fi
		serror(&.str)
	fi
end

global proc skipsymbol(int symbol)=
	if lx.symbol<>symbol then checksymbol(symbol) fi
	lex()
end

global proc inittypetables=
	int i,j,size,bitsize,s,t,u

!Initialise type tt-tables from std types first all fields initially zero

	for i:=0 to tlast-1 do
		ttbasetype[i]:=i

		bitsize:=stdtypewidths[i]
		size:=bitsize/8

		ttsize[i]:=size
!		ttcat[i]:=stdcat[i]

		if i in [tarray, tstruct] then
			ttisblock[i]:=1
		fi

	od
	ntypes:=tlast-1

	trefchar:=createrefmode(ti8)

	trefwchar:=createrefmode(tu16)

!do dominant table
	for i:=1 to dominantsetuptable.len do
		s:=dominantsetuptable[i,1]
		t:=dominantsetuptable[i,2]
		u:=dominantsetuptable[i,3]
		dominantmode[s,t]:=u
	od

!do conversion table
	for i:=1 to convsetuptable.len do
		s:=convsetuptable[i,1]
		t:=convsetuptable[i,2]
		u:=convsetuptable[i,3]
		conversionops[s,t]:=u
	od
end

global function createdupldef(ref strec owner,symptr, int id)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
	ref strec p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id
	p.namespace:=namespaces[id]
	if q:=symptr.nextdupl then			!1st in dupl list
		q.prevdupl:=p
	fi
	p.nextdupl:=q
	p.prevdupl:=symptr
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

global function createnewmoduledef(ref strec owner,symptr)ref strec=
	ref strec p,q

	p:=createdupldef(owner,symptr,moduleid)
	return p
end

global function createnewproc(ref strec owner,symptr)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
	ref strec p,q

	p:=createdupldef(owner,symptr,procid)

	q:=p
	while q:=q.nextdupl do
		if q.owner=owner then
			cpl q.name,"in",owner.name
			serror("Dupl proc name")
		fi
!	q:=q.nextdupl
	od

	return p
end

global function resolvename(ref strec owner, symptr, int ns, blockno)ref strec=
!symptr is a generic st entry for a name
!owner is the st entry where the name has been encountered (the current
! module, or a function)
!ns is code of the namespace that is being searched
!blockno is zero, if searched at file scope, or non-zero if searching
!from inside a function. Then, it will be the current block number
!where the name has been encountered
!Search the symbol table (usually the dupl list for symptr) for
!any instance of the name which matches in owner, matches the
!namespace, and is within the blockno hierarchy
!return symptr of the st entry when resolved, or nil if not resolved
	int nsblock
	ref strec d

	if symptr.nameid>macroid then
		return symptr
	fi

	if ns=ns_labels then
		return resolvelabel(owner,symptr)
	fi

	if blockno and blockcounts[blockno]=0 then blockno:=blockowner[blockno] fi

	do							!loop for each block level
		nsblock:=ns<<16 ior blockno
		d:=symptr				!reset dupl list
		while d:=d.nextdupl do
			if owner.nameid=procid  and d.owner<>owner and d.owner.nameid=procid then
				exit
			fi
			if d.owner=owner and d.nsblock=nsblock then
!				d.used:=1
				if d.used<255 then ++d.used fi

				return d
			fi
		od

		if blockno=0 then
			case owner.nameid
			when procid then			!was in function, now search filescope
					!(THIS MIGHT BE NEEDED FOR PARAM-SCOPES where block number is zero)
				owner:=stmodule
				redoloop
			when structtagid then		!was in struct; now try owner (proc/module/other struct)
				owner:=owner.owner
				if owner=nil then		!not sure if possible, but just in case...
					return nil
				fi
			else
				return nil
			esac
		elsif (blockno:=blockowner[blockno])=0 then		!try next block level
			owner:=stmodule				!block 0 means outside outer block, so switch to module scope
		fi

	od

	return nil
end

global function resolvelabel(ref strec owner, symptr)ref strec=
		ref strec d
		d:=symptr				!reset dupl list
		while d:=d.nextdupl do
			if owner.nameid=procid  and d.owner<>owner and d.owner.nameid=procid then
				exit
			fi

			if d.owner=owner and d.namespace=ns_labels then
				return d
			fi
		od

		return nil
end

global function checkdupl(ref strec owner, symptr, int ns, blockno)ref strec=
!Same params as resolvename.
!But here, search only current scope level to see if something of the
!same name, and in the same namespace, already exists
!Returns nil, if such a name was not found, or a symptr to it
!A returned symbol might be of a different nameid, but that would
!be an error usually as you can't have two identical names in the same namespace.
!Some kinds of names can have repeated declarations in the same scope
	int nsblock
	ref strec d

	d:=symptr

	nsblock:=ns<<16 ior blockno

	while d:=d.nextdupl do
		if d.owner=owner and d.nsblock=nsblock then
			return d
		fi
	od

	return nil
end

global function checkdupl_inproc(ref strec owner, symptr, int ns, blockno)ref strec=
!special version of checkdupl
!assumes that dupl list starts at last proc

	int nsblock
	ref strec d

	d:=symptr

	nsblock:=ns<<16 ior blockno

	while (d:=d.nextdupl) and d.owner=owner do
		if d.nsblock=nsblock then
			return d
		fi
	od

	return nil
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tarray then
		return getalignment(tttarget[m])
	when tstruct,tunion then

		a:=ttnamedef[m].align
		if a=0 then
!		CPL("GETALIGN 0")
			RETURN 16
!		SERROR("GETALIGN 0")
		fi
		return a
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	esac
	cpl strmode(m),A
	serror("GETALIGN SIZE NOT 1248")

	return 0
end

global function isexported(ref strec d)int=
	if d.nameid=procid then
		if d.code and (d.scope=imported_scope or d.scope=exported_scope) then
			return 1
		fi
	else
		if d.scope=exported_scope then
			return 1
		fi
	fi
	return 0
end

global function isimported(ref strec d)int=
	if d.nameid=procid then
		if d.code=nil and (d.scope=imported_scope or d.scope=exported_scope) then
			return 1
		fi
	else
		if d.scope=imported_scope then
			return 1
		fi
	fi
	return 0
end

global function getstname(ref strec d)ichar=
	static [256]char name
	memcpy(&.name,d.name,d.namelen)
	name[d.namelen+1]:=0
	return &.name
end

global function isrealcc(int m)int=
	m:=ttbasetype[m]
	return tfirstreal<=m<=tlastreal
!return tfirstreal<=m and m<=tlastreal
end

global function isintcc(int m)int=
	m:=ttbasetype[m]
	return tfirstint<=m<=tlastint
end

global function ispoweroftwo(i64 x)int=
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

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

GLOBAL PROC PAUSE(ichar mess="PAUSE")=
	CP MESS
	OS_GETCH()
	CPL
END

global func gettclmode(int t)int u=
	u:=stdtcl[ttbasetype[t]]

	if u=tpblock then
		case ttsize[t]
		when 8 then u:=tpu64
		when 4 then u:=tpu32
		when 2 then u:=tpu16
		when 1 then u:=tpu8
		esac
	fi
	return u
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

=== cc_support.m 0 0 12/21 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global proc stopcompiler(ichar filename,int lineno)=
	if fwriteerrors then
		filehandle f
		f:=fopen("$error.tmp","w")
		println @f,filename,lineno
		fclose(f)
	fi
	println
	println
	stop 1
end

global proc mcerror(ichar mess)=
	println "\nMC Error:",mess
!os_getch()
	stop 40
end

global proc serror(ichar mess)=
	serror_gen(mess)
end

global proc serror_gen(ichar mess)=
	if currproc then
		print "\nIn function",currproc.name,," "
	ELSE
		CPL "OUTSIDE PROC"
	fi

	println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]
	showmacrolineno()

	println
	println "**** Syntax Error:",mess,"****"
	println

	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc serror_ss(ichar mess,a,b)=
	[256]char str
!	sprintf(&.str,mess,a,b)
	fprint @str, mess,a,b
	serror_gen(&.str)
end

global proc serror_s(ichar mess,a)=
	[256]char str
!	sprintf(&.str,mess,a)
	fprint @str, mess, a
	serror_gen(&.str)
end

global proc terror_gen(ichar mess)=

	if currproc then
		println "\nIn function",currproc.name
	fi

	println "Type error:",mess,"on line",lx.lineno,sourcefilepaths[lx.fileno]

	showmacrolineno()

	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc terror(ichar mess)=
	terror_gen(mess)
end

global proc terror_s(ichar mess,a)=
	[256]char str

	fprint @str, mess, a
	terror_gen(&.str)
end

global proc terror_ss(ichar mess,a,b)=
	[256]char str

	fprint @str, mess, a, b
	terror_gen(&.str)
end

global proc gerror_gen(ichar mess,ref unitrec p=nil)=
	int lineno,fileno

	if p then
		lineno:=p.lineno
		fileno:=p.fileno
	else
		lineno:=clineno
		fileno:=cfileno
	fi

	if currproc then
		print "In function",currproc.name,," "
	fi

	println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
	println
	println "**** Code Gen Error:",mess,"****"
	stopcompiler(sourcefilepaths[fileno],lineno)
end

global proc gerror(ichar mess,ref unitrec p=nil)=
	gerror_gen(mess,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
	[256]char str

	fprint @str, mess, s
	gerror_gen(&.str,p)
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

global proc loaderror(ichar mess,mess2="")=
	[512]char str

	fprint @str, mess, mess2
	println "Load Error:",&.str
	println "Stopping"
	stop 45
end

global function loadsourcefile(ichar file,shortfile)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
	ichar s

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles
	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(file)
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

	s:=cast(readfile(file))			!will overallocate by a few bytes
	if not s then				!unexpected error
		loaderror("LSF can't load ",file)
	fi

	sourcefiletext[nsourcefiles]:=s
	sourcefilesizes[nsourcefiles]:=rfsize
	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)

	return nsourcefiles
end

function splicelines(ichar s)ichar=
	ichar t,u

	t:=u:=pcm_alloc(strlen(s)+1)

	while s^ do
		if s^='\\' and (s+1)^=10 then s+:=2
		elsif s^='\\' and (s+1)^=13 and (s+2)^=10 then s+:=3
		else t++^ := s++^
		fi
		t^:=0
	od
	return u
end

global function loadbuiltin(ichar shortfile,hdrtext)int=
!loading build-in header with text at hdrtext
!Name of header is in 'file'.
	ichar s

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles
	sourcefilepaths[nsourcefiles]:="<builtin>"
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
	sourcefiletext[nsourcefiles]:=pcm_copyheapstring(hdrtext)

	sourcefilesizes[nsourcefiles]:=strlen(hdrtext)
	return nsourcefiles
end

proc gs_copytostr(ref strbuffer source,ref char s)=
	if source.length then
		memcpy(s,source.strptr,source.length)
		(s+source.length)^:=0
	else
		s^:=0
	fi
end

global proc gs_additem(ref strbuffer dest,ichar s)=		!GENITEM
!like genstr, but ensure there is white space separation as needed from the last output
	ichar d
	int lastchar,nextchar

	d:=dest.strptr

	if dest.length then
		lastchar:=(d+dest.length-1)^
		nextchar:=s^
		if isalphanum(lastchar) and isalphanum(nextchar) then
			strbuffer_add(dest," ")
		fi
	fi
	strbuffer_add(dest,s)
end

function isalphanum(int c)int=
	if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
		return 1
	fi
	return 0
end

proc showmacrolineno=
	if slineno then
!	println "	(Last macro invoked near line",
!		slineno,"in file",sourcefilenames[sfileno],,")"
	fi
end
=== cc_headersx.m 0 0 13/21 ===
!Built-in standard headers

global int builtinheaders=0

global function findheader(ichar name)ichar=
return nil
end

global proc writeheaders=
filehandle f
ichar ifile
int i
end

global function isheaderfile(ichar file)int=
return 0
end
=== cc_show.m 0 0 14/21 ===
int currfileno
int currlineno

strbuffer sbuffer
global ref strbuffer dest=&sbuffer
int destlinestart

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar


global proc printcode(filehandle f,ichar caption)=
	int i
	ref strec p

	println @f, caption

	p:=stmodule.deflist

	while p do
		case p.nameid
		when procid then
!		if p.scope<>imported_scope and p.code then
			if p.code then
				println @f,p.name,,"=",scopenames[p.scope]
				printunit(f,p.code,,"1")
				println @f
			fi
		esac
		p:=p.nextdef
	od
end

global proc printunit(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
!p is a tagrec
	ref unitrec q
	ref strec d
	int t,n,lincr
	ichar idname
	ref caserec pc

	if p=nil then
		return
	fi

	if p.tag>=jdummy then
		println "print unit: bad tag",p.tag
		stop 30
	fi

	if p.lineno then
		currlineno:=p.lineno
		currfileno:=p.fileno
	fi

	lincr:=1
	if level<0 then
		lincr:=-1
		print @dev,"             "
	fi

	print @dev,getprefix(abs(level),prefix,p)
	idname:=jtagnames[p.tag]
	if idname^='j' then ++idname fi

	print @dev,idname,,": "

	case p.tag
	when jname, jfuncname then
		d:=p.def

		print @dev,d.name,namenames[d.nameid]

		if d.code then
			print @dev," {",,jtagnames[d.code.tag],,"}"
		fi

		print @dev," ",,getdottedname(d)!,q

		if p.c then
			print @dev," Lastcall:",p.c
		fi

	when jtempdecl, jdecl, jgoto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

		println @dev
		printunit(dev,d.code,level+lincr,"1")
		return

	when jgoto then

		d:=p.def
		print @dev,d.name,namenames[d.nameid]

	when jlabelstmt then
		print @dev,p.def.name!,"+ LABELED STATEMENT"

	when jcasestmt then
		print @dev,"Value:", p.value

	when jconst then
		t:=p.mode
		if t=trefchar then
			if not p.isstrconst then
				goto doref
			fi
	dostring:
			if p.slength>256 then
				print @dev,"""",,"(LONGSTR)",""" *",,p.slength
			else
				print @dev,"""",,p.svalue,,""" *",,p.slength
			fi
		elsif t=trefwchar then
			if not p.iswstrconst then
				goto doref
			fi
			print @dev,"""",,"(WSTRING)",""" *",,p.wslength
		elsif t>=ti8 and t<=ti64 then
			print @dev,p.value
		elsif t>=tu8 and t<=tu64 then
			print @dev,p.uvalue
		elsif isrealcc(t) then
			print @dev,p.xvalue
		elsif ttbasetype[t]=tref then
			if p.isstrconst then
				goto dostring
			fi
	doref:
			print @dev,ref void(p.value)
		elsif ttbasetype[t]=tarray then
			if p.isstrconst then
				goto dostring
			fi
			serror("PRINTUNIT/CONST/aRRAY")
		else
			cpl typename(t)
			serror("PRINTUNIT BAD CONST")
		fi
		print @dev," ",,strmode(t)
		if p.isstrconst then print @dev,"<STRCONST>" fi
		if p.iswstrconst then print @dev,"<WSTRCONST>" fi

	when jconvert then
		print @dev,convnames[p.opcode]
		print @dev," "
		print @dev,typename(p.a.mode)
		print @dev," => "
		print @dev,typename(p.convmode)

	when jscale then
		print @dev,"Scale:",p.scale

	when jaddptr,jsubptr then
		print @dev,"Ptrscale:",p.ptrscale

	when jswitch then
		pc:=p.nextcase
		n:=0
		while pc do ++n; pc:=pc.nextcase od

		print @dev,p.nextcase,n

	when jcallfn then
		print @dev," Aparams:",p.aparams

	when jptr then

	when jdot then
		print @dev," Offset:",p.offset

	esac

	if p.memmode then
		print @dev, " Widen from:",strmode(p.memmode)
	fi

	if p.alength then print @dev," ALENGTH=",p.alength fi

	println @dev

	printunitlist(dev,p.a,level+lincr,"1")
	printunitlist(dev,p.b,level+lincr,"2")
	if p.tag<>jblock then					!.c is used to point to last element
		printunitlist(dev,p.c,level+lincr,"3")
	fi
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
	if p=nil then return fi

	while p do
		printunit(dev,p,level,prefix)
		p:=p.nextunit
	od
end

function getprefix(int level,ichar prefix,ref unitrec p)ichar=		!GETPREFIX
!combine any lineno info with indent string, return string to be output at start of a line
	static [512]char str
	[512]char indentstr
	ichar modestr
	int length

	indentstr[1]:=0
	if level>10 then level:=10 fi

	strcpy(&.indentstr,"-----------------------")

	modestr:=strmode(p.mode,0)
	length:=strlen(modestr)
	if length<strlen(&.indentstr) then
		memcpy(&.indentstr,modestr,length)
	else
		strcpy(&.indentstr,modestr)
	fi

	to level do
		strcat(&.indentstr,"|---")
	od

	strcpy(&.str,getlineinfok())
	strcat(&.str,&.indentstr)
	strcat(&.str,prefix)
	if prefix^ then
		strcat(&.str," ")
	fi

	return &.str
end

global function getdottedname(ref strec p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	ref strec owner

	strcpy(&.str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(&.str2,&.str)
		strcpy(&.str,owner.name)
		strcat(&.str,".")
		strcat(&.str,&.str2)
		owner:=owner.owner
	od
	if p.blockno then
	!	sprintf(&.str2,".%d",i32(p.blockno))
		print @&.str2,".",,p.blockno
		strcat(&.str,&.str2)
	fi
	return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
	static [40]char str

	fprint @&.str,"# ",currfileno,currlineno:"z5",$
	return &.str
end

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
	ref byte q
	strbuffer v
	ref strbuffer d:=&v
	int col,offset
	const tabstr="    "
	[256]char str
	int scope
	ref paramrec pm

	gs_init(d)

	offset:=0
	to level do
		gs_str(d,tabstr)
		offset+:=4
	od
	gs_str(d,":")

	if p.blockno then
!	sprintf(&.str,"#.%d",p.name,i32(p.blockno))
		print @&.str,p.name,,".",,p.blockno

		gs_leftstr(d,&.str,28-offset,'-')
	else
		gs_leftstr(d,p.name,28-offset,'-')
	fi
	gs_leftstr(d,namenames[p.nameid],12,'.')
	col:=gs_getcol(d)

	gs_str(d,"[")

	gs_str(d,scopenames[p.scope])
	gs_str(d," ")

	if p.align then
		gs_str(d,"@@")
		gs_strint(d,p.align)
		gs_str(d," ")
	fi
	if p.varparams then
		gs_str(d,"Var ")
GS_STRINT(D, P.VARPARAMS)

	fi
	if p.used then
		gs_str(d,"Used ")
	fi
	if p.nparams then
		fprint @&.str,"Pm:# ",p.nparams
		gs_str(d,&.str)
	fi

	gs_str(d,"]")
	gs_padto(d,col+10,'=')

	if p.owner then
		fprint @&.str,"(#)",p.owner.name
		gs_leftstr(d,&.str,18,' ')
	else
		gs_leftstr(d,"()",18,' ')
	fi

	case p.mode
	when tvoid then
		gs_str(d,"Void ")
	else
		gs_strsp(d,strmode(p.mode))
	esac

	case p.nameid
	when fieldid then
		gs_str(d,"Offset:")
		gs_strint(d,p.offset)

	when frameid,paramid then
		if p.code then
			gs_str(d,"=")

			gs_strvar(d,strexpr(p.code))
		fi
		gs_str(d," Offset: ")
		gs_strint(d,p.offset)

	when procid then

		gs_str(d,"Index:")
		gs_strint(d,p.index)

	when enumid then
		gs_str(d,"Enum:")
		gs_strint(d,p.index)

	when staticid then
		if p.code then
			gs_str(d,"=")
			gs_strvar(d,strexpr(p.code))
		fi
		gs_str(d,"STATIC********")
	esac

	gs_str(d," ")

	gs_str(d,"Lineno:")
	gs_strint(d,p.lineno iand 16777215)
	gs_str(d," ")
	gs_str(d,sourcefilenames[p.lineno>>24])

	if p.nameid=procid then
		gs_line(d)
		pm:=p.paramlist
		while pm do
			gs_str(d,"		Param: ")
			gs_leftstr(d,(pm.def|pm.def.name|"Anon"),10,'-')
			gs_str(d,pmflagnames[pm.flags])
			gs_str(d," Mode:")
			gs_str(d,strmode(pm.mode))
			gs_str(d," Code:")
			gs_strint(d,cast(p.code))

			gs_line(d)
			pm:=pm.nextparam
		od
	fi

	gs_println(d,f)

	if p.code then
		case p.nameid
		when frameid,staticid then
			printunit(f,p.code,-3)
		esac
	fi
end

global proc printstflat(filehandle f)=
	int i
	ref strec p
	ref tokenrec lx
	println @f,"GLOBAL SYMBOL TABLE:"

	for i:=0 to hstsize-1 do
		p:=hashtable^[i]
		if p.name then
			case p.symbol
			when namesym,ktypespecsym, ksourcedirsym then
				println @f,i,p,":",getstname(p),symbolnames[p.symbol],namenames[p.nameid]
				p:=p.nextdupl
				while p do
					print   @f,"	",p,getstname(p),symbolnames[p.symbol],namenames[p.nameid],
						p.prevdupl
					println @f,"(From",(p.owner|getstname(p.owner)|"-"),,")"
					p:=p.nextdupl
				od
			esac
		fi
	od
end

global function strexpr(ref unitrec p)ref strbuffer=
!vx_makestring("",exprstr)
	gs_init(exprstr)

	jeval(exprstr,p)
	return exprstr
end

proc jeval(ref strbuffer dest, ref unitrec p)=
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	ref unitrec q
	[16000]char str
	int lb,t

!CPL "JEVAL",P,JTAGNAMES[P.TAG]

	case p.tag
	when jconst then
		if (t:=p.mode)=trefchar then
			if p.slength=0 then goto doref fi		!might be initialised to something else
			if not p.isstrconst then goto doref fi		!might be initialised to something else
			if p.slength>str.len/2 then
				strcpy(&.str,"LONGSTR)")
			else
!*!				convertstring(p.svalue,&.str)
			fi
			gs_additem(dest,"""")
			gs_additem(dest,&.str)
			gs_additem(dest,"""")
			return
		elsif t>=ti8 and t<=ti64 then
			getstrint(p.value, &.str)

		elsif t>=tu8 and t<=tu64 then
			strcpy(&.str,strword(p.uvalue))

		elsif t=tr64 or t=tr32 then
			strcpy(&.str,strreal(p.xvalue))
		else
			case ttbasetype[p.mode]
			when tref then
	doref:
				print @&.str,ref void(p.svalue)
			when tarray then
				strcpy(&.str,"ARRAY")
			else
				CPL typename(p.mode)
				ABORTPROGRAM("EVAL/C")

			esac
		fi
		gs_additem(dest,&.str)

	when jname then
		gs_additem(dest,p.def.name)

	when jfuncname then
		gs_str(dest,"&")
		gs_additem(dest,p.def.name)

	when jandl,jorl,jandand,jeq,jne,jlt,jle,jgt,jge,jadd,jsub,jmul,jdiv,
			jrem,jiand,jior,jixor,jshl,jshr,
			jaddto,jsubto,jmulto,jdivto,
			jremto,jiandto,jiorto,jixorto,jshlto,jshrto 	then

		strcpy(&.str,getopcjname(p.tag))
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,&.str)
		jeval(dest,p.b)
		gs_additem(dest,")")

	when jneg,jabs,jinot,jnotl,jistruel then

		strcpy(&.str,getopcjname(p.tag))
		gs_additem(dest,&.str)
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,")")

	when jcallfn then
		jeval(dest,p.a)
		gs_additem(dest,"(")

		q:=p.b
		while q do
			jeval(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,")")

	when jdot then
		jeval(dest,p.a)
		gs_additem(dest,".")
		GS_STR(DEST,"???")

	when jidot then
		jeval(dest,p.a)
		gs_additem(dest,"->")
		jeval(dest,p.b)

	when jmakelist,jexprlist then
		lb:=p.tag=jexprlist
		gs_additem(dest,(lb|"("|"{"))

		q:=p.a
		while q do
			jeval(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,(lb|")"|"}"))

	when jassign then
		jeval(dest,p.a)
		gs_additem(dest,"=")
		jeval(dest,p.b)

	when jifx then
		jeval(dest,p.a)
		gs_additem(dest,"?")
		jeval(dest,p.b)
		gs_additem(dest,":")
		jeval(dest,p.c)

	when jconvert then

		gs_additem(dest,strmode(p.mode))
		gs_additem(dest,"(")
		jeval(dest,p.a)
		gs_additem(dest,")")

	when jptr then
		gs_additem(dest,"*(")
		jeval(dest,p.a)
		if p.b then
			gs_additem(dest,"+")
			jeval(dest,p.b)
		fi
		gs_additem(dest,")")

	when jblock then
		gs_additem(dest,"<JBLOCK>")

	when jpreincr then
		gs_additem(dest,"++")
		jeval(dest,p.a)

	when jpredecr then
		gs_additem(dest,"--")
		jeval(dest,p.a)

	when jpostincr then
		jeval(dest,p.a)
		gs_additem(dest,"++")

	when jpostdecr then
		jeval(dest,p.a)
		gs_additem(dest,"--")


	when jnull then
		gs_str(dest,"<nullunit>")

	when jscale then
		gs_str(dest,"scale((")
		jeval(dest,p.a)
		if p.scale>0 then
			gs_str(dest,")*")
			gs_strint(dest,p.scale)
		else
			gs_str(dest,")/")
			gs_strint(dest,-p.scale)
		fi
		gs_str(dest,")")
	when jaddptr then
		gs_str(dest,"(")
		jeval(dest,p.a)
		gs_str(dest,"+")
		jeval(dest,p.b)
		gs_str(dest,")")

	when jwidenmem then
		jeval(dest,p.a)


	else
!CPL JTAGNAMES[P.TAG]
	gs_str(dest,"<CAN'T DO JEVAL>")
	end
end

global proc printfilelist(filehandle f)=
	println @f,"Source files",nsourcefiles
	for i to nsourcefiles do
		fprintln @f,"# # (#)", i, sourcefilenames[i]:"12jl", sourcefilepaths[i]
	od
	println @f,"\nInput file:",inputfile
	println @f,"\nLibfiles",nlibfiles
	for i to nlibfiles do
		println @f,i, libfiles[i]
	od

end

global proc printmodelist(filehandle f)=
	int m, mbase
	ref strec d
	const tab="\t"

	println @f,"PROC MODELIST",ntypes

	for m:=0 to ntypes do
		println @f,m:"4", strmode(m)
		mbase:=ttbasetype[m]
		if tttypedef[m] then println @f,tab,"Typedef:",tttypedef[m].name fi

		println @f,tab,"Basetype:",mbase,strmode(mbase)
		println @f,tab,"Name:",typename(m)
		d:=ttnamedef[m]
		print @f,tab,"ttnamedef:",d,$
			if d then
				print @f, d.name,,".",,d.blockno
			else
				print @f,"-"
			fi
!(ttnamedef[m]|ttnamedef[m].name|"-")

		println @f
		
		println @f,tab,"Target:",strmode(tttarget[m])
		println @f,tab,"Size:",ttsize[m]
		println @f,tab,"Length:",ttlength[m]
		println @f,tab,"Isblock:",ttisblock[m]
		println @f,tab,"Const:",ttconst[m]
		println @f,tab,"Signed:",ttsigned[m]
		println @f,tab,"Ref:",ttreftype[m]
		println @f,tab,"Constver:",strmode(ttconsttype[m])
		println @f,tab,"Shared:",ttshared[m]
		println @f
	od

	println @f
end

=== tc_api.m 0 0 15/21 ===
int STSEQNO

export tcl tcstart			!start of tcl block
export tcl tccurr			!point to current tcl op
export tcl pcend			!point to last allocated tclrec
global int pcalloc			!number of tclrecs allocated
byte pcfixed				!whether code is fixed up
global int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

const pcelemsize = tclrec.bytes
global const tclbasesize = tclrec.bytes - 3*tclopnd.bytes	!base size excludes a,b,c fields

export int mlabelno
export byte phighmem
export byte pfullsys
global byte fpshortnames

!THESE DON'T WORK, AS THEY CAN'T BE SHARED. Eg. may need different types
! or need .isvariadic
!const maxsmallint=64
![0..maxsmallint]tclopnd smallintoperands

global ichar longstring					!used in stropnd
global int longstringlen

global int pstartclock
global int mcltime
global int sstime
global int exetime

global ref func (int pos, ichar &filename, &sourceline)int igetmsourceinfo

global byte fregoptim = 1
global byte fpeephole
global byte tc_useruntcl=0
!global byte pfullsys
export byte pverbose

export int mmpos


!---------------------------------------------------

proc start=
!CPL =PSTREC.BYTES
!CPL =TCLREC.BYTES
!CPL =opndREC.BYTES
!CPL =mclREC.BYTES
!CPL =tclbasesize
!CPL =TCLNAMES.LEN
!CPL =regset.len
!CPL =workregs.len
!CPL =REGNAMES.LEN
!CPL =v31
!CPL =rfirst, =rlast

!	for i:=0 to maxsmallint do
!		smallintoperands[i]:=tc_genint0(i)
!	od
end

export proc tcl_start =
!reset tcstart/tccurr for new TCL sequence (new proc or new init data)
	tcstart:=pcm_allocnfz(tclbasesize)
	tcstart.opcode:=knop
	tccurr:=tcstart
	ntemps:=0				!keep track of max used in proc

end

export func tcl_end:tcl pc=
!Terminate sequence; sets tcstart to nil so that tcl cannot be generated
!outside proc bodies etc
!But caller should copy tcstart value, or use its value returned here

	pc:=tcstart
	if pc.opcode=knop then
		pc.next
	else
		pc
	fi
end

export func tcl_writetcl(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writealltcl("caption")

	if filename then
		if pverbose then println "Writing TCL",filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

export func tcl_writepst(ichar filename=nil)ichar=
	ref strbuffer d

	d:=writepst()

	if filename then
		if pverbose then println "Writing PST",filename fi
		writefile(filename, d.strptr, d.length)
		""
	else
		d.strptr
	fi
end

global proc perror(ichar mess)=
	perror_s(mess, nil)
end

global proc perror_s(ichar mess, param=nil)=
	print "TCL error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

global proc tclerror(ichar mess)=
	println "TCL Error:", mess
	println
	stop 1
end

export func tc_makesymbol(ichar s, int id)psymbol d=
!Create a new st entry
!local/param/null-id names are not linked to psymbol table
!all others become part of main ST
!Only local/param have .owner set to currfunc

	d:=pcm_allocnfz(pstrec.bytes)
	d.name:=pcm_copyheapstring(s)
	d.seqno:=++stseqno

	case id
	when import_id then
		d.imported:=1
	when export_id then
		d.exported:=1
		id:=proc_id
	esac

	d.id:=id
!
!	if id in [local_id, param_id] then
!!*!		d.owner:=currfunc
!	elsif id then
!		tc_addsymbol(d)
!	fi
!
	d
end

export proc tc_addproc(psymbol d)=
	if pproctable=nil then
		pproctable:=pproctablex:=d
	else
		pproctablex.next:=d
		pproctablex:=d
	fi
end

export proc tc_addparam(psymbol d)=
	psymbol p

	tclerror("No proc") unless currfunc

	p:=currfunc.nextparam

	if p=nil then
		currfunc.nextparam:=d
	else
		while p.nextparam do p:=p.nextparam od		!look for last
		p.nextparam:=d
	fi
	++currfunc.nparams
end

export proc tc_addlocal(psymbol d)=
	psymbol p

	tclerror("No proc") unless currfunc

	p:=currfunc.nextlocal

	if p=nil then
		currfunc.nextlocal:=d
	else
		while p.nextlocal do p:=p.nextlocal od		!look for last
		p.nextlocal:=d
	fi
	++currfunc.nlocals
end

export proc tc_addstatic(psymbol d)=
!add to global static if outside a function, or to current function

	psymbol p

!CPL "ADD STATIC", D.NAME

	if currfunc=nil then
		if pstatictable=nil then
			pstatictable:=pstatictablex:=d
		else
			pstatictablex.next:=d
			pstatictablex:=d
		fi
	else

		p:=currfunc.nextstatic

		if p=nil then
			currfunc.nextstatic:=d
		else
			while p.nextstatic do p:=p.nextstatic od		!look for last
			p.nextstatic:=d
		fi
!		++currfunc.nstatics
	fi
end

global func newtcl(int opcode, nopnds)tcl p=

	p:=pcm_allocnfz(nopnds*tclopnd.bytes+tclbasesize)

!NALLTCLSIZE+:=nopnds*tclopnd.bytes+tclbasesize
!++NALLTCL
!TCLOPNDSIZE[NOPNDS]++
!

!CPL "NEWTCL", TCLNAMES[OPCODE]!=NOPNDS, =TCLOPND.BYTES, =TCLBASESIZE

	tccurr.next:=p
	tccurr:=p

	tccurr.opcode:=opcode
	tccurr.nopnds:=nopnds
	tccurr.pos:=mmpos
	tccurr.seqno:=++pcseqno
	tccurr.ndest:=tclwrite[opcode]			!move/call are set manually

!CPL "NEWTCL", TCLNAMES[OPCODE],=PCCURR, PCSEQNO
	return tccurr
end

global func newopnd:tclopnd=
	pcm_allocnfz(opndrec.bytes)
end

global proc tc_gen0(int opcode)=
	newtcl(opcode, 0)
end

global proc tc_gen1(int opcode, tclopnd a)=
	tcl p

	p:=newtcl(opcode, 1)

	p.a:=a
end

global proc tc_gen2(int opcode, tclopnd a, b)=
	tcl p

	p:=newtcl(opcode, 2)

	if opcode=kmove and a.optype=temp_opnd then
		p.ndest:=1
	fi

	p.a:=a
	p.b:=b

end

global proc tc_gen3(int opcode, tclopnd a, b, c)=
	tcl p

	p:=newtcl(opcode, 3)

	p.a:=a
	p.b:=b
	p.c:=c

end

global proc tc_gen4(int opcode, tclopnd a,b,c,d)=
!global proc tc_gen(int opcode, tclopnd a=nil, b=nil, c=nil, d=nil, E=NIL, F=NIL)=
	tcl p

	p:=newtcl(opcode,4)

	p.a:=a
	p.b:=b
	p.c:=c
	p.abc[4]:=d
end

global proc tc_gen_ix(int opcode, tclopnd a, b, c=nil, int scale=1, offset=0) =

!	IF A=NIL THEN A:=GENINT(0) FI
	IF B=NIL THEN B:=TC_GENINT(0) FI
	IF C=NIL THEN C:=TC_GENINT(0) FI
!INT OLDOP:=PCCURR.OPCODE
	tc_gen3(opcode, a,b,c)

	tccurr.scale:=scale
	tccurr.extra:=offset
end

global proc tc_gen_call(tclopnd fn, int nret, nargs)=
!nret is number of ret opnds in extretopnds
!nargs is the number of args opnds in extparamopnds
	tcl p
	tclopnd x
	int argoffset

	p:=newtcl(kcall, nret+nargs+1)

	p.abc[nret+1]:=fn
	p.abc[nret+1].opmode:=tpu64
	p.nret:=nret
	p.nargs:=nargs
	p.argoffset:=argoffset:=nret+1

	for i to nret do
		p.abc[i]:=x:=extretopnds[i]			!offset past .a
	od

	for i to nargs do
		p.abc[i+argoffset]:=x:=extparamopnds[i]
	od
end

global proc tc_gen_cond(int opcode, cond, tclopnd a, b, c)=
	tc_gen3(opcode,a,b,c)
	tccurr.cond:=cond
end

global func tc_genint(int a)tclopnd p=
!	if a in 0..maxsmallint then
!		return smallintoperands[a]
!	fi
	p:=newopnd()
	p.value:=a
	p.optype:=int_opnd
	return p
end

global func tc_genint0(int a)tclopnd p=
	p:=newopnd()
	p.value:=a
	p.optype:=int_opnd
	return p
end

global func tc_genreal(real x)tclopnd p=
	p:=newopnd()
	p.xvalue:=x
	p.optype:=real_opnd
	return p
end

global func tc_genr32(real x)tclopnd p=
	p:=newopnd()
	p.xvalue32:=x
	p.optype:=r32_opnd
	return p
end

global func tc_genstring(ichar s)tclopnd p=
	p:=newopnd()
	p.svalue:=pcm_copyheapstring(s)
!	p.svalue:=s
	p.optype:=string_opnd
	return p
end

global function tc_gendata(ref byte s, int length)tclopnd p=
	static [1..8]byte types=(tpu8, tpu16, tpblock, tpu32, tpblock, tpblock, tpblock, tpu64)
	p:=newopnd()
	p.svalue:=s			! assume already saved on heap
	p.optype:=data_opnd
!	p.opmode:=tpblock
!
!IF LENGTH IN 1..8 THEN
!	P.OPMODE:=TYPES[LENGTH]
!FI
!
!	p.opsize:=length
	return p
end

global func tc_genlabel(int labelno)tclopnd p=
	p:=newopnd()
	p.labelno:=labelno
	p.optype:=label_opnd
	return p
end

global func tc_genmem(psymbol d)tclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=mem_opnd
	return p
end

global func tc_genmemaddr(psymbol d)tclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=memaddr_opnd
	return p
end

global func tc_genname(ichar s)tclopnd p=
	return tc_genmem(tc_makesymbol(s, misc_id))
end

global func tc_gennameaddr(ichar s)tclopnd p=
	return tc_genmemaddr(tc_makesymbol(s, misc_id))
end

global func getfullname(psymbol d, int backtick=0)ichar=
!create fully qualified name into caller's dest buffer
	static [256]char str
	int n:=0
	psymbol e:=d

!	if fpshortnames then return d.name fi

	str[1]:=0
	if backtick then
		strcpy(str, "`")
	fi

	if d.imported then
		if backtick then
			strcat(str, d.name)
			strcat(str, "*")
		else
			strcat(str, d.name)
		fi
		return str
	fi


	if d.id in [local_id, param_id] then
IF CURRFUNC=NIL THEN
ABORTPROGRAM("CURRFUNC=0\N\N")
FI
		strcat(str, currfunc.name)
		strcat(str, ".")
		strcat(str, d.name)
		return str
	fi

	if backtick then
		strcat(str, d.name)
	else
		return d.name
	fi
end

export func strpmode(int mode, size=0)ichar=
	static [32]char str

	strcpy(str, "")

	case mode
	when tpblock then
		strcpy(str, "mem:")
		strcat(str, strint(size))
		str
	when tpvoid then
		"---"
	else
		pstdnames[mode]
	esac
end

export proc tc_setmode(int m, size=0)=
	tccurr.mode:=m

	if size then
		tccurr.size:=size
	else
		tccurr.size:=psize[tccurr.mode]
	fi

	if tclhastype[tccurr.opcode]=2 then
		tccurr.mode2:=tccurr.mode
	fi
end

export proc tc_setmode2(int m)=
	tccurr.mode2:=m
end

export proc tc_setimport(psymbol d)=
!allow the use of tc_addlocal
!use d=nil when done

	currfunc:=d
	return unless d

	if pimporttable=nil then
		pimporttable:=pimporttablex:=d
	else
		pimporttablex.next:=d
		pimporttablex:=d
	fi
end

export proc tc_comment(ichar s)=
!	return when fregoptim or fpeephole		!will get skipped anyway

	tc_gen1(kcomment, tc_genstring(s))
end

global proc tc_currfunc(psymbol d)=
	currfunc:=d
end

export proc tc_addplib(ichar name)=
	if nplibfiles>=maxplibfile then perror("Too many libs") fi

!CPL "ADDPLIB",NAME

!	plibfiles[++nplibfiles]:=pcm_copyheapstring(name)
	plibfiles[++nplibfiles]:=pcm_copyheapstring(changeext(name,""))
end

global func tc_makeind(tclopnd a, unit q, int m, size=8)tclopnd p=
	tcl pold

	p:=newopnd()
	if a=nil then gerror("MAKEIND A=0") fi
	p^:=a^
!TC_COMMENT(ADDSTR("MAKEIND1:", OPNDNAMES[P.OPTYPE]))
	case p.optype

	when memaddr_opnd then
		p.optype:=mem_opnd

	when temp_opnd, mem_opnd then

!	else							!everything else needs explicit loadptr to a new temp
!									!note will not detect invalid ops like floats or strings

!IF P.OPTYPE=TEMPPTR_OPND THEN
!	CPL "MAKEIND/TEMPPTR"
!FI
		pold:=tccurr
		tc_gen_ix(kiloadx, p:=tc_gentemp(),a, tc_genint(0))
		checkaddpx(pold)

!		tc_gen2(kiload, p:=tc_gentemp(tpu64),a)

		tc_setmode(m, size)

	else
		gerror("makeind?")
	esac

	return p
end

global func tc_makeindlv(tclopnd a, unit q, int m, size=8)tclopnd p=
	p:=newopnd()
	if a=nil then gerror("MAKEIND A=0") fi

	p^:=a^

	case p.optype
	when memaddr_opnd then
		p.optype:=mem_opnd
!	when mem_opnd then
!		p.optype:=memptr_opnd

	when temp_opnd then
!		p.optype:=tempptr_opnd

!	when tempptr_opnd, memptr_opnd then
!	when memptr_opnd then
	when mem_opnd then

		tc_gen2(kmove, p:=tc_gentemp(),a)
		tc_setmode(tpu64)
		p:=tc_makeindlv(p,q, m)

		return p

	else
		gerror("makeindlv?")

	esac

	return p
end

global func tc_gentemp:tclopnd p=
	int n
	p:=newopnd()

!CPL "GENTEMP", STRMODE(M)

	n:=++ntemps

	p.tempno:=n
	p.optype:=temp_opnd

	return p
end

global proc tc_deltemp=
!remove last-generated temp; not needed after all
!allocated operand may still exist
	--ntemps
end

export func convertstring(ichar s, t, int length)int=
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

EXPORT proc merror(ichar mess,ichar param="")=
	int lineno
	ichar filename, sourceline

	if igetmsourceinfo then
		lineno:=igetmsourceinfo(mmpos, filename, sourceline)
		CPL =LINENO
		CPL =FILENAME
	else
CPL "NO LINE INFO"
		lineno:=0
		filename:="?"
	fi

	if currfunc then
		println "Proc:", currfunc.name
	fi

	fprintln "MCL Error: # (#) on Line: # in #, TCL:#",mess,param, lineno, filename, pcseqno
OS_GETCH()
	pcerrorstop(filename, lineno)
end

global proc pcerrorstop(ichar filename,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
	println @f,filename,lineno
	println
	fclose(f)
	stop 1
end

export func getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

export proc tcl_genmcl=
	genmcl()
end

export proc tcl_setflags(int highmem=-1, verbose=-1, shortnames=-1) =

	if highmem>=0 then phighmem:=highmem fi

	if verbose>=0 then pverbose:=verbose fi
	if shortnames>=0 then fpshortnames:=shortnames fi
end

export func addstr(ichar s, t)ichar=
static [256]char str
	strcpy(str, s)
	strcat(str, t)
	str
end

export func tcl_writeasm(ichar filename=nil, int atype='AA')ichar=
	ref strbuffer asmstr
	filehandle f

CPL "WRITEASM"

!	if assemtype<>atype then
!		tclerror("Wrong ASM Module")
!	fi

!	if assemtype='NASM' then
!		phighmem:=2
!	fi

!CPL "WASM:",$LINENO
	tcl_genmcl()

!CPL "WASM:",$LINENO

	asmstr:=getassemstr()
!CPL "WASM:",$LINENO

	if filename then
!		if pverbose then println "Writing", filename fi
		println "Writing", filename

!CPL "WASM:",$LINENO
		f:=fopen(filename,"w")
!CPL "WASM:",$LINENO
		gs_println(asmstr, f)
!CPL "WASM:",$LINENO
		fclose(f)

		gs_free(asmstr)
		nil
	else
		asmstr.strptr
	fi
end

export proc tcl_writedll(ichar filename)=
	phighmem:=2
	genmcl()
	genss()
	int tt:=os_clock()
	writeexe(filename, 1)
	exetime:=os_clock()-tt
end

export proc tcl_writeexe(ichar filename)=

!CPL "WX",$LINENO
	genmcl()
!CPL "WX",$LINENO

	genss()
!CPL "WX",$LINENO
	int tt:=os_clock()
!CPL "WX",$LINENO
	writeexe(filename, 0)
!CPL "WX",$LINENO
	exetime:=os_clock()-tt
end

export proc tcl_genss(int obj=0)=
	genmcl()
	genss(obj)
end

export func tcl_writess(ichar filename=nil, int obj=0)ichar =
	ref strbuffer ssstr
	filehandle f

	genmcl()
	genss(obj)

	ssstr:=writessdata(not obj)

	if filename then
		f:=fopen(filename,"w")
		gs_println(ssstr, f)
		fclose(f)

		gs_free(ssstr)
		nil
	else
		ssstr.strptr
	fi
end

global proc tcl_runtcl=
end

global proc tcl_writeobj(ichar filename)= end

!global proc tcl_genss= end
!global proc tcl_runtcl= end
!global proc tcl_writeobj(ichar filename)= end
!global proc tcl_writeexe(ichar filename)= end
!global proc tcl_writedll(ichar filename)= end
global proc tcl_writemx(ichar filename)= end
global proc tcl_writemcl= end
global proc tcl_exec= end
export proc tcl_cmdskip(int a)=end



=== tc_decls.m 0 0 16/21 ===

global type psymbol = ref pstrec

!global record pstrec = $caligned
global record pstrec =
	ichar name
	psymbol next				!proc or global static
	psymbol nextparam
	psymbol nextlocal
	psymbol nextstatic
	tcl code					!proc body; istatic init data
	ref[]tempmoderec tempmodes	!procs only

!	psymbol owner
!	psymbol generic				!locals/params: version in global ST

!	ref procinforec info		!procs: info to help codegen	

	union
!		tcl pcaddr				!for procs: entry point to func
		ref proc dlladdr		!for imported funcs
		ivoid staddr			!var statics: address
		psymbol cprocowner		!C target:var statics: owner proc
		int retindex			!local return label index (used by mcl etc)

	end
	ref fwdrec fwdrefs			!fwd ref chain

	byte id
	byte opcode					!for opcode_rw
	byte subcode				!for jumpcc_rw/setcc_rw/type_rw
	byte nrefs
	i32 offset

	byte mode
	byte isentry
	byte nretvalues				!func: number of return values (0 for proc)
	byte variadic				!0 or N: fixed params of variadic function

	byte dllindex				!for dllproc: which dll in dlltable
	byte reg
	byte reftype
	byte segment

	i32 labelno
	u32 seqno

	u32 size
	i16 stindex
	i16 importindex

	u32 maxtemp				!for procs set after tcl codegen
	u32 pos: (sourceoffset:24, fileno:8)

	i16 nlocals
	i16 impindex
	i16 expindex
	u16 flags:(chasstatics:1, addrof:1, atvar:1, used:1,
				imported:1, exported:1, isthreaded:1, ishandler:1,
				ismain:1)

!	byte scope
	byte nparams
	byte align					!for variables
	struct						!for params only
		byte paramcat				!tc_reg/move/stack/spill
		byte fromreg				!tc_reg: move from .fromreg to .reg
	end
!	byte maxtemp				!for procs set after tcl codegen

end

global type tcl = ref tclrec

global record tclrec =
	tcl next
!--
	union						!two 32-bit params used according to opcode
		struct					!pointer ops
			i32 scale			!scale factor for offset
			i32 extra			!extra constant byte offset, already scaled
		end
		struct					!call/etc
			i32 nargs			!number of args set via setparam
!			i32 nret			!0/1/2 for call proc/func/mult
		end
		struct					!switch
			i32 minlab
			i32 maxlab
		end

!following single values set by tc_gen_n or tc_gen_cond or tc_gen_op
		i32 index				!general sequential index for setparam/temp etc
		i32 fnindex			!sysfn index number
		i32 cond				!tcl condition code for jumpcc etc
		i32 step				!always +ve fixed step size for forup/fordown; also INCR
		i32 truncmode			!convert/truncate: truncated mode

		struct
			i32 x				!common access to these two params
			i32 y
		end
	end
!--
	u32 pos:(psourceoffset:24, pfileno:8)
	u32 size
!--
	byte mode
	byte mode2
	byte opcode
	byte flags:(isglobal:1,
				isvariadic:1,	!for calls: calling a variadic function
				firstlt:1)

	byte nopnds
	byte argoffset				!So that p.abc[i+offset] accesses i'th argument
	union
		byte ndest				!no. of dest temps, usually 0 or 1, can N for calls, 2 for divrem
		byte nret				!alias for same value
	end
	byte ltmode					!is 0/1/2 for first ltemp, which can be Tm/Tx
!--
	u32 islast

	u16 seqno
	[2]byte spare2
!--	
!only allocated up to here; the rest depends on number of tclopnds

	union
		struct
			tclopnd a,b,c					!only present for correct .nopnds
		end
		[]tclopnd abc
!		[-1:]tclopnd args				!args [1] corresponds with abc[3]
	end
end

global type tclopnd = ref opndrec

global record opndrec =
	union
		i64 value
		r64 xvalue
		r32 xvalue32
		ichar svalue
		psymbol def
		struct
			u32 tempno
			byte reg
		end

		int labelno
		unit asmcode
	end
	u32  opsize
	byte optype
	byte opmode
	byte flags:(isvariadic:1,			!variadic argument (outside fixed params)
				isbinary:1,
				isstring:1,
				reduced:1)
	byte spare1
end

global record tempmoderec=
	u32 size
	i32 offset
	byte mode
	byte used
	[6]byte dummy
end

global record temprec =
!first group is the prepass to set up .ltmode/.islast within tcl ops

	tcl lasttcl				!nil to start, then reference to tcl that has last ref
	u16 ltcount				!how many writes so far to a temp
	u16 rtcount				!how many reads so for of a temp
	byte index				!index of operand for the last temp seen
	[3]byte spare
!second group is used during mcl pass for reg allocation and temp management

!	byte loc				!reg, spilled or mult
!	byte reg				!allocated reg (mult temps must use same reg)
!	byte SPARE
end

export record fwdrec =
	ref fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

global byte tcldone, mcldone, ssdone, objdone, exedone

global [maxfixedtemp]temprec fixedtemplist
global [maxfixedtemp]byte fixedtempmap
global ref[]temprec templist			!points to above for small numbers of temps
global ref[]byte tempmap

!global const maxfixedtemp=256
global const maxfixedtemp=512
global const maxltemp=4					!as used by multiple func return

global int ntemps

global psymbol pstatictable, pstatictablex
global psymbol pproctable, pproctablex
global psymbol pimporttable, pimporttablex

global psymbol currprog
export psymbol currfunc
global psymbol blockretname
global psymbol entryproc		!entry point func

global const maxparam=100

global const maxplibfile=50
global [maxplibfile]ichar plibfiles
global [maxplibfile]u64 plibinst
global int nplibfiles

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer

EXPORT ICHAR $PMODULENAME
=== tc_diags.m 0 0 17/21 ===
!byte fshowallmodes=1
byte fshowallmodes=0

int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

tcl currtcl

!const tab1="  "
!const tab2="    "
const tab1="  "
!const tab2=tab1+tab1

const tclindent = 1

!const fshowsymbols=1
const fshowsymbols=0

ref[]byte labeltab

global proc strtcl(tcl p, int inline=0)=
!inline=1 when generating inline comments for strmcl
	int opcode, nopnds
	tclopnd a,b,c
	int ntypes, defused

	const showformatted=1
	opcode:=p.opcode
	currtcl:=p			!currtcl used in psopnd etc

!CPL "----STRTCL",TCLNAMES[OPCODE], CURRFUNC.NAME

	nopnds:=p.nopnds
	ntypes:=tclhastype[opcode]

	a:=b:=c:=nil

	if nopnds then
		a:=p.a
		if nopnds>1 then
			b:=p.b
			if nopnds>2 then
				c:=p.c
			fi
		fi
	fi

	case opcode
	when klabel then
		strlabel(a.labelno,1)
		return
	when kcomment then
		psstr("!")
		psstr(a.svalue)
		return
!
	esac

	psstr(tab1)
!
!PSSTR(STRINT(INT(P),"H"))
!PSSTR(" ")
!PSSTR(STRINT(P.SEQNO,"4Z"))
!PSSTR(" ")

	defused:=0
	if not showformatted then
		goto default
	fi

	switch opcode
	when kmove then

		psopnd(1)
		psassign()
		psopnd(2)

	when klabel then

	when kjump then
		psstr("goto ")
		psopnd(1)

	when kijump then
		psstr("goto ")
		psopnd(1)
		psstr("^")

	when kadd..kfmod then
!IF A.OPTYPE<>TEMP_OPND THEN PSSTR("TTTADD/NONTEMP ") FI
		psopnd(1)
		psassign()
		psbinop(p.opcode,2,3)

	when kneg..ktypepun then
		psopnd(1)
		psassign()
		psmonop(p.opcode,2)

	when kaddto..ksubpxto then
		psbinop(p.opcode,1,2)

	when knegto..ktoboolto then
		psmonop(p.opcode,1)

	when kjumpcc then
		psstr("if ")
		psopnd(2)
		psstr(ccshortnames[p.cond])
		psopnd(3)
		psstr(" then goto ")
		psopnd(1)

	when ksetcc then
		psopnd(1)
		psassign()
		psopnd(2)
		psstr(ccshortnames[p.cond])
		psopnd(3)

	when kjumpf then
		psstr("if not ")
		psopnd(2)
		psstr(" then goto ")
		psopnd(1)

	when kjumpt then
		psstr("if ")
		psopnd(2)
		psstr(" then goto ")
		psopnd(1)

	when kjumpin, kjumpout then
		psstr("if ")
		psopnd(2)
		psstr((opcode=kjumpin|" in "|" not in "))
		psopnd(3)
		psstr(" .. ")
		psopnd(4)
		psstr(" then goto ")
		psopnd(1)

	when kforup, kfordown then
		psopnd(2)
		psstr((opcode=kforup|" +:= "|" -:= "))
		psint(p.step)

		psstr("; if ")
		psopnd(2)
		psstr((opcode=kforup|" <= "|" >= "))
		psopnd(3)
		psstr(" then goto ")
		psopnd(1)

	when kto then
		psopnd(2)
		psstr("--; if ")
		psopnd(2)
		psstr(" then goto ")
		psopnd(1)

	when kaddpx, kiloadx then
!IF OPCODE=KLOADPX AND A.OPTYPE<>TEMP_OPND THEN PSSTR("TTTLOADPX/NONTEMP ") FI
		psopnd(1)
		psstr(" := ")
		psptr(2, 3, p.scale, p.extra)
		if opcode=kiloadx then
			psstr("^")
		fi

	when kistorex then
		psptr(1, 2, p.scale, p.extra)
		psstr("^ := ")
		psopnd(3)

!	when ksysproc, ksysprocx then
!		psstr(sysfnnames[p.fnindex]+3)
!		psstr("(")
!		for i to p.nopnds do
!			psopnd(p.abc[i])
!			if i<p.nopnds then psstr(",") fi
!		od
!		psstr(")")
!
!	when ksysfn,ksysfnx then
!		psopnd(1)
!		psstr(" := ")
!		psstr(sysfnnames[p.fnindex]+3)
!		psstr("(")
!		if p.nopnds>1 then
!			psopnd(2)
!			if p.nopnds=3 then
!				psstr(",")
!				psopnd(3)
!			fi
!		fi
!		psstr(")")
!
	when kiswap then
		psstr("swap(")
		psopnd(1)
		psstr(",")
		psopnd(2)
		psstr(")")

!	when kblocktemp then
!		psstr("B")
!		psint(p.index)

	when kswitch, kswitchu then
		psstr((opcode=kswitch|"switch "|"switchu "))
		psopnd(3)
		psstr(" (")
		psopnd(1)
		psopnd(2)
		psint(p.minlab)
		psstr(":")
		psint(p.maxlab)
		psstr(")")

	when kcall then
		for i to p.nret do
!			psopnd(p.abc[i+1])
			psopnd(i)
			if i<p.nret then psstr(", ") fi
		od
		if p.nret then psstr(" := ") fi

		psopnd(p.nret+1)
		psstr("(")
		for i to p.nargs do
			psopnd(i+p.argoffset)
!			a:=p.abc[i+p.argoffset]
!			if a.isvariadic then psstr("?") fi
			if i<p.nargs then psstr(", ") fi
		od
		psstr(")")
!		if p.isvariadic then psstr("?") fi

	when kincrto, kdecrto then
		psopnd(1)
		if p.step=1 then
			psstr((opcode=kincrto|"++"|"--"))
		else
			psstr((opcode=kincrto|"+:="|"-:="))
			psint(p.step)
		fi

	when kstop then
		psstr("stop ")
		psopnd(1)

!	when kclear, kretfn, kretproc, kswlabel, kdata,
!			kretmult, keval then
!		goto default
!
	when kloadbit, kloadbf then
		psopnd(1)
		psassign()
		psopnd(2)
		psstr(".[")
		psopnd(3)
		if opcode=kloadbf then
			psstr("..")
			psopnd(4)
		fi
		psstr("]")

	when kstorebit then
		psopnd(1)
		psstr(".[")
		psopnd(2)
		psstr("]")
		psassign()
		psopnd(3)

	when kstorebf then
		psopnd(1)
		psstr(".[")
		psopnd(2)
		psstr("..")
		psopnd(3)
		psstr("]")
		psassign()
		psopnd(4)

	when kincrload, kdecrload then
		psopnd(1)
		psassign()
		psstr((opcode=kincrload|"++"|"--"))
		psopnd(2)

	when kloadincr, kloaddecr then
		psopnd(1)
		psassign()
		psopnd(2)
		psstr((opcode=kloadincr|"++"|"--"))

	when kretfn then
		psstr("return ")
		psopnd(1)

	when kretproc then
		psstr("return")

	else
		PSSTR("* ")				!may need attention
default:
!CPL "DEFAULT"
		psstr(tclnames[opcode])
		psstr(" ")
		defused:=1
		for i to nopnds do
			psopnd(i)
			psstr(" ")
		od

IF OPCODE IN [KSUBPX, KSUBP] AND P.EXTRA THEN
	CPL "SUBP/X HAS EXTRA OFFSET"
os_getch()
FI

	end switch

	if inline then
		PSTABTO(30)
!		psstr("   ")
	else
		PSTABTO(40)
	fi

	case ntypes
	when 1, 2 then
		psmode(p.mode, p.size)
!		if ntypes=2 and p.mode<>p.mode2 then
		if ntypes=2 then
			psstr("/")
			psmode(p.mode2)
		fi
		IF FSHOWALLMODES THEN RECASE 3 FI
	when 3 then
		if a then
			psstr("(")
			for i to nopnds do
				a:=p.abc[i]
				if i>1 then psstr(",") fi
				if i=p.nret+1 and i>1 then psstr(" ") fi
				if i=p.nret+2 then psstr(" ") fi
				if a.opmode then
					psmode(a.opmode, a.opsize)
					if a.isvariadic then psstr("?") fi
				else
					psstr("---")
				fi
			od
			psstr(")")
		fi
		psstr(" ")
	else
		psstr("---")
	esac

	if inline then
		psstr(" ")
	else
		PSTABTO(56)
	fi

	GS_LEFTSTR(DEST,TCLNAMES[OPCODE],9)
!IF OPCODE=KMOVE AND A.OPTYPE=TEMP_OPND THEN PSSTR("MT") FI

!return when inline

	PSSTR("|")
!CPL $LINENO,A,=P.NOPNDS
!RETURN

!if not showformatted then
	if defused and (p.x or p.y) then
		psstr(" X:")
		psint(p.x)
		psstr(" Y:")
		psstr(" Y:")
		psint(p.Y)
	fi

	if p.isglobal then psstr(" Isglobal") fi
	if p.isvariadic then psstr(" Isvariadic") fi
!
!IF OPCODE=KCALL THEN
!	PSSTR(" NRET:"); PSINT(P.NRET)
!	PSSTR(" NARGS:"); PSINT(P.NARGS)
!	PSSTR(" ARGOFF:"); PSINT(P.ARGOFFSET)
!FI

INT FIRSTLAST:=1
	for i to nopnds do
		a:=p.abc[i]
!		if a and a.optype in [temp_opnd, tempptr_opnd] then
		if a and a.optype=temp_opnd then
!			if i<=p.ndest and p.ltmode[i]<2 then
!			if i<=p.ndest then
			if i<=p.ndest AND (I>1 OR P.LTMODE<2) then
!				psstr(" (")
FIRSTLAST:=0
				psstr("<")
				psstr(strtemp(a.tempno))
				psstr(": ")
			fi
			if p.islast.[i] then
				IF FIRSTLAST THEN PSSTR("   "); FIRSTLAST:=0 FI
				psstr(":")
				psstr(strtemp(a.tempno))
				psstr(">")
!				psstr(")")
			fi
		fi
	od
!CPL $LINENO

!	PSSTR(" ")
!	PSINT(INT(P.A)); PSSTR(" ")
!	PSINT(INT(P.B)); PSSTR(" ")
!	PSINT(INT(P.C))

!IF P.NDEST THEN
!PSSTR(" LT:")
!PSINT(P.LTMODE)
!FI

end

proc psopnd(int n)=
	tclopnd a
	byte ptrflag

	a:=currtcl.abc[n]
	psstr(stropnd(a))

	ptrflag:=tcltempptr[currtcl.opcode]

	if a.optype=temp_opnd then
!		if n=1 and currtcl.ltmode then
!			psstr((currtcl.ltmode=2|"x"|"m"))
!		fi
!		if n=1 and currtcl.firstlt then psstr(".") fi
		if ptrflag=n or ptrflag=3 and n<3 then
			psstr("^")
		fi
	fi
end

global function stropnd(tclopnd p)ichar=
	[maxparam]tclopnd paramopnds
	static[512]char str
	[4]char str2
	psymbol d
	ref byte q
	int length

!RETURN "<OPND>"

	if p=nil then
		return "-"
	fi

	case p.optype
	when int_opnd then
		return strint(p.value)

	when real_opnd then
		return strreal(p.xvalue)
	when r32_opnd then
		return strreal(p.xvalue32)

	when string_opnd then
		length:=strlen(p.svalue)
		if length<str.len/4 then
			strcpy(str,"""")
			convertstring(p.svalue,&.str+1, length)
			strcat(str,"""")
		else
			return "<Long str>"
		fi

	when mem_opnd then
		print @str,p.def.name

	when memaddr_opnd then
		fprint @str,"&#",p.def.name

	when temp_opnd then
		return strtemp(p.tempno)

	when label_opnd then
		fprint @str,"L# ",p.labelno

	when data_opnd then
		q:=p.svalue

		if p.isstring then
			print @str, "S<"

			to min(currtcl.size,40) do
				str2[1]:=q^; str2[2]:=0
				strcat(str, str2)
				++q
			od
		else						!binary, or normal non-data-string data
			print @str, "B<"
			to min(currtcl.size,10) do
				strcat(str, strint(q^,"Hz2"))
				strcat(str, " ")
				++q
			od
!		else
!			STRCPY(STR, "<NOT S OR B>")
		fi
		strcat(str, ">")

	else
		return "<TCLOPND?>"
	esac

	return str
end

global function strtclstr(tcl p)ichar =
	gs_init(dest)

	destlinestart:=1
	strtcl(p,1)

	return dest.strptr
end

global proc strlabel(int labelno,colon=0)=
	psstr("L")
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

global func writealltcl(ichar caption)ref strbuffer=
!write all tcl code in system by scanning all procs
!tcl code is only stored per-proc
	tcl p
	psymbol d,e
	tclopnd a

CPL "WRITEALL PCL"

	gs_str(dest,"PROC ")
	gs_strln(dest,caption)
	gs_strln(dest,"!DATA ------------------------------------------------------")

!GS_STRLN(DEST,"<WRITEPSTALLTCL TO BE REVISED>")

!CHECKCOMM("ALL1")

!GS_STRLN(DEST, "<TCL WRITE NOT READY>")
!
!RETURN DEST


	labeltab:=pcm_allocz(mlabelno)			!indexed 1..labelno

	d:=pstatictable

	while d, d:=d.next do
		if d.id=static_id then
			psstr("var ")
			psstr(strpmode(d.mode, d.size))
			psstr(" ")
			psstr(d.name)

			if d.code then
				psstr(" = ")
				currtcl:=d.code
				psdata(d.code)
			else
				psline()
			fi
		fi
	od
	psline()

	gs_strln(dest,"!CODE ------------------------------------------------------")
	d:=pproctable

	while d, d:=d.next do
		currfunc:=d
		if d.id=proc_id then
			psprocsig(d)

			psprocbody(d)

			psstrline("End")
			psline()

		fi
	od
	psline()


!	p:=tcstart.next
!	while p, p:=p.next do
!		writetcl(p)
!		destlinestart:=dest.length
!	od

!	gs_strln(dest,"------------------------------------------------------")
!
	pcm_free(labeltab, mlabelno)
!
	return dest
end

proc writetcl(tcl p)=
	tclopnd a

	a:=p.a

!	unless p.opcode=klabel and not labeltab[a.labelno] then
		strtcl(p)
		gs_line(dest)
		psstrline("") when p.opcode=keval
!	end
end

proc psbinop(int opc, a, b)=
	tabledata []byte opcodes, []ichar opnames =
		(kadd,		"+"),
		(ksub,		"-"),
		(kmul,		"*"),
		(kdiv,		"/"),
		(kidiv,		"%"),
		(kirem,		"rem"),
		(kbitand,	"iand"),
		(kbitor,	"ior"),
		(kbitxor,	"ixor"),
		(kshl,		"<<"),
		(kshr,		">>"),
!		(kand,		"and"),
!		(kor,		"or"),
!		(kaddpx,	"+(pi)"),
		(ksubpx,	"-(pi)"),
		(ksubp,		"-(pp)"),
		(kpower,	"**"),
		(kaddto,	"+:="),
		(ksubto,	"-:="),
		(kmulto,	"*:="),
		(kdivto,	"/:="),
		(kidivto,	"%:="),
		(kiremto,	"rem:="),
		(kbitandto,	"iand:="),
		(kbitorto,	"ior:="),
		(kbitxorto,	"ixor:="),
		(kshlto,	"<<:="),
		(kshrto,	">>:="),
		(kmaxto,	"max:="),
		(kminto,	"min:="),
!		(kandto,	"and:="),
!		(korto,		"or:="),
	end

	for i to opcodes.len do
		if opc=opcodes[i] then
			psopnd(a)
			psstr(" ")
			psstr(opnames[i])
			psstr(" ")
			psopnd(b)
			return
		fi
	od

	psstr(tclnames[opc])	
	psstr("(")
	psopnd(a)
	psstr(",")
	psopnd(b)
	psstr(")")
end

proc psmonop(int opc, int a)=
	tabledata []byte opcodes, []ichar opnames =
		(kneg,		"-"),
		(kbitnot,	"inot "),
		(knot,		"not "),
		(knegto,	"-:="),
		(kabsto,	"abs:="),
		(kbitnotto,	"inot:="),
		(knotto,	"not:="),
		(ktoboolto,	"istrue:="),
	end

	for i to opcodes.len do
		if opc=opcodes[i] then
			psstr(opnames[i])
			psopnd(a)
			return
		fi
	od

	psstr(tclnames[opc])	
	psstr("(")
	psopnd(a)
	psstr(")")
end

proc psassign=
	gs_str(dest," := ")
end

global proc psmode(int mode, size=0) =
	psstr(strpmode(mode, size))
end

proc psprocsig(psymbol d)=
	psymbol e
	byte comma:=0
	int lastmode:=tvoid, m, lastsize, size

	psstr("Proc ")
	psstr(d.name)
	psstr("(")

	e:=d.nextparam

	while e, e:=e.nextparam do
		if comma then psstr(", ") fi
		if e.mode<>lastmode and e.size<>lastsize then
			lastmode:=e.mode
			lastsize:=e.size
			psstr(strpmode(lastmode, lastsize))
			psstr(" ")
		fi
		psstr(e.name)

		comma:=1
	od
	psstr(")")
	if d.mode then
		psstr(strpmode(d.mode, d.size))
	fi
!	psstr(":")
	psstrline(" =")

	comma:=0
	e:=d.nextlocal
	while e, e:=e.nextlocal do
		if comma then psline() fi
		psstr(tab1)
		psstr(strpmode(e.mode, e.size))
		psstr(" ")
		psstr(e.name)
		comma:=1
	od
	if comma then psline() fi
	if d.nextlocal then psline() fi

	e:=d.nextstatic
	while e, e:=e.nextstatic do
		if comma then psline() fi
		psstr(tab1)
		psstr(strpmode(e.mode, e.size))
		psstr(" ")
		psstr(e.name)

		if e.code then
			psstr(" = ")
			currtcl:=e.code
			psdata(e.code)
		fi
	od
	if d.nextstatic then psline() fi
end

proc psdata(tcl p)=
	tclopnd a
	byte mult:=istrue p.next

	if mult then psstrline("(") fi

	while p, p:=p.next do
		a:=p.a
		if mult then psstr("    ") fi
!		psstr(tclnames[p.opcode])
!		psstr(" ")
		psstr(stropnd(a))
		psstr(" ")

!!		psmode(a.opmode, a.opsize)
!		psmode(p.mode, p.size)
		if p.next then psstr(",") fi
		psline()
	od

	if mult then psstrline(")") fi
end

proc psprocbody(psymbol d)=
	tcl p
	tclopnd a

	p:=d.code

	return unless p

!do first pass populating label table

	while p, p:=p.next do
		if p.opcode<>klabel then
			for i to p.nopnds do
				a:=p.abc[i]
				if a.optype=label_opnd then
					labeltab[a.labelno]:=1
				fi
			od
		fi
	od

	p:=d.code				!skip nop
	destlinestart:=dest.length

	while p, p:=p.next do
		writetcl(p)
		destlinestart:=dest.length
	od

end

proc psptr(int a, b, scale, offset)=
!CPL "PSPTR",A,B
	tclopnd ax:=currtcl.abc[a]
	tclopnd bx:=currtcl.abc[b]

	psstr("(")
	psopnd(a)

	if bx then
		if bx.optype=int_opnd then
			offset+:=bx.value*scale
		else
			psstr(" + ")
			psopnd(b)
			if scale>1 then
				psstr("*")
				psint(scale)
			fi
		fi
	fi

	if offset>0 then
		psstr(" + ")
		psint(offset)

	elsif offset<0 then
		psstr(" - ")
		psint(-offset)
	fi
	psstr(")")
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

global proc psname(psymbol d)=
	gs_str(dest,getfullname(d))
end

global proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col od
end

global func writepst:ref strbuffer=
	gs_init(dest)

	gs_strln(dest,"------------------------------------------------------")
	writepst2("PROC PST Global Static Table", pstatictable)
	psline()
	writepst2("PROC PST Global Proc Table", pproctable)
	psline()

	writepst2("PROC PST Global Import Table", pimporttable)
	psline()

	return dest
end

global proc writepst2(ichar caption, psymbol d)=
	int i:=0, j
	psymbol e

	psstrline(caption)
	psline()

	while d, d:=d.next do
!PSSTR(STRINT(INT(D),"H"))
!PSSTR(" ")
		writepsymbol(d, "25jl")

		if d.id in [proc_id, import_id] then
			e:=d.nextparam
			j:=0
			while e, e:=e.nextparam do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
			e:=d.nextlocal
			j:=0
			while e, e:=e.nextlocal do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
			e:=d.nextstatic
			j:=0
			while e, e:=e.nextstatic do
				psstr("    ")
				writepsymbol(e, "21jl")
			od
		fi
!PSLINE()
	od
	psline()
end

proc writepsymbol(psymbol d, ichar fmt)=
	byte localfile:=0
	[256]char str

	print @str, d.seqno:"4", idnames[d.id]
	psstr(str)
	to 8-strlen(idnames[d.id]) do psstr(" ") od

	str[1]:=0

!	print @str, d.name:fmt
	print @str, d.name:fmt,D,$
	psstr(str)


	psstr(strpmode(d.mode, d.size))

	if d.id=proc_id then
		psstr(" Pm:")
		psint(d.nparams)
		psstr(" Loc:")
		psint(d.nlocals)
	fi

	if d.exported then psstr(" Exp") fi
	if d.imported then psstr(" Imp") fi
	if d.variadic then psstr(" Varpms:"); psint(d.variadic) fi
	if d.isthreaded then psstr(" TC") fi
!*!	if d.reg then psstr(" "); psstr(regnames[d.reg]) fi
!	if d.hasdot then psstr(" Dot") fi
	if d.isentry then psstr(" ENTRY PT") fi

	if d.id in [local_id, param_id, static_id] then
		PSSTR(" Align:")
		psint(d.align)
	fi

!	if d.id=proc_id then psstr(" .PCADDR ="); PSSTR(STRINT(CAST(D.PCADDR),"H")) fi

!	if d.owner then
!		psstr(" (")
!		psint(d.owner.seqno)
!		psstr(" ")
!		psstr(d.owner.name)
!		psstr(")")
!	fi	

	if ctarget and d.id=static_id and d.cprocowner then
		psstr(" (Proc:")
		psstr(d.cprocowner.name)
		psstr(") (D:")
!		psint(cast(d.pcdata))
!*!		psstr(strint(cast(d.pcdata),"H"))
		psstr(")")
	fi
	if ctarget and d.id=proc_id and d.chasstatics then
		psstr(" Has statics")
!		psint(d.chasstatics)
	fi

	psline()
end

global func strtemp(int temp)ichar=
	static [16]char str
	[2]char cc
	str[1]:='T'
	str[2]:=0

	strcat(str, strint(temp))

!	cc[1]:=temp+'a'-1
!	cc[2]:=0
!	strcat(str, cc)

	str
end

=== tc_tables.m 0 0 18/21 ===
!type system

export enumdata \
		[0:]ichar pstdnames,
		[0:]byte psize,

		[0:]byte psigned,
		[0:]byte pint,

		[0:]byte pfloat,
		[0:]byte pwide =

!                 names       size  s  i  f  w
	(tpvoid=0,    "void",    	0,	0, 0, 0, 0),

	(tpr32,       "r32",    	4,	0, 0, 1, 0),
	(tpr64,       "r64",    	8,	0, 0, 1, 1),

	(tpu8,        "u8",      	1,	0, 1, 0, 0),
	(tpu16,       "u16",    	2,	0, 1, 0, 0),
	(tpu32,       "u32",    	4,	0, 1, 0, 0),
	(tpu64,       "u64",    	8,	0, 1, 0, 0),

	(tpi8,        "i8",      	1,	1, 1, 0, 0),
	(tpi16,       "i16",    	2,	1, 1, 0, 0),
	(tpi32,       "i32",    	4,	1, 1, 0, 0),
	(tpi64,       "i64",    	8,	1, 1, 0, 0),

	(tpblock,     "mem",   		0,	0, 0, 0, 0),
	(tpvector,    "vec",   		0,	0, 0, 0, 0),

	(tplast,      "$last",   	0,	0, 0, 0, 0),


end

global const tpref = tpu64

global enumdata [0:]ichar opndnames =
	(no_opnd=0,			$),

	(mem_opnd,			$),
	(temp_opnd,			$),
	(memaddr_opnd,		$),

	(int_opnd,			$),
	(real_opnd,			$),
	(r32_opnd,			$),

	(string_opnd,		$),		!reference to a string elsewhere (so like a label)
	(stringimm_opnd,	$),		!immediate string using .asciz/.ascii
	(label_opnd,		$),
	(data_opnd,			$),

!	(metastring_opnd,	$),
end

!tclhastype:
! 0 		no type info
! 1			tcl.mode (t)
! 2			tcl.mode & tcl.mode2 (t & u)
! 3			Uses arg types only (eg. CALL)

global enumdata [0:]ichar tclnames,
				[0:]byte tclwrite,			!1/2: known fixed no. of temp dests; 0=not used, or depends on context
				[0:]byte tclhastype,
				[0:]byte tcltempptr =		!1/2/3 means temp in opnd 1/2/both must be T^

!TCL opcodes
! T	lvalue			T3
! M	lvalue			x, T (x is static/local/proc)
! P lvalue			x, T3^

! a b c d			Rvalues: T3, x, &x, 123 4.56 "ABC" L123
! D					Data Rvalue: x, &x, 123 4.56 "ABC" L123 <datastr>

! L Label index		Labels
! d symbol			M (ST entry)

!** means opcode needs a 4th tclopnd; this needs specialing handling for temps

!                    Wr Types T^         (a b c)
	(knop=0,	$+1,  0,  0,  0),  !     (- - -)
	(kcomment,	$+1,  0,  0,  0),  !     (a - -)
  
	(kmove,		$+1,  0,  1,  0),  !     (M b -)	M := b
	(keval,		$+1,  0,  1,  0),  !     (a - -)
  
	(kaddpx,	$+1,  1,  1,  0),  ! s,n (T b c)	T := b + c*s + n
	(kiloadx,	$+1,  1,  1,  0),  ! s,n (T b c)	T :=(b + c*s + n)^
	(kistorex,	$+1,  0,  1,  0),  ! s,n (b c r)	(a + b*s + n)^ := c
  
	(kcall,		$+1,  0,  3,  0),  ! r,n (a- - -)	([T ...] F [r ...]) r=nret, n=nargs
	(kretproc,	$+1,  0,  0,  0),  !     (- - -)	return
	(kretfn,	$+1,  0,  1,  0),  !     (a - -)	return a
	(kretmult,	$+1,  0,  3,  0),  ! n   (a ...)	return n values

	(kjump,		$+1,  0,  0,  0),  !     (L - -)	goto L
	(kjumpcc,	$+1,  0,  1,  0),  ! cc  (L b c)	goto L when b cc c
	(kjumpt,	$+1,  0,  1,  0),  !     (L b -)	goto L when istrue(b)
	(kjumpf,	$+1,  0,  1,  0),  !     (L b -)	goto L when not istrue(b)
	(kijump,	$+1,  0,  1,  0),  !     (a - -)	goto a
	(ksetcc,	$+1,  1,  1,  0),  ! cc  (T b c)	T := b cc c
  
	(kto,		$+1,  0,  1,  0),  !     (L b -)	--b; goto L when b<>0
	(kforup,	$+1,  0,  1,  0),  ! n   (L b c)	b+:=n; goto L when b <= c
	(kfordown,	$+1,  0,  1,  0),  ! n   (L b c)	b-:=n; goto L when b >= c

	(kiswap,	$+1,  0,  1,  3),  !     (P P -)	swap(P, P)
  
	(kadd,		$+1,  1,  1,  0),  !     (T b c)	T := b + c
	(ksub,		$+1,  1,  1,  0),  !     (T b c)	T := b - c
	(kmul,		$+1,  1,  1,  0),  !     (T b c)	T := b * c
	(kdiv,		$+1,  1,  1,  0),  !     (T b c)	T := b / c (float only)
	(kidiv,		$+1,  1,  1,  0),  !     (T b c)	T := b / c (int only; b % c)
	(kirem,		$+1,  1,  1,  0),  !     (T b c)	T := b irem c
	(kbitand,	$+1,  1,  1,  0),  !     (T b c)	T := b iand c
	(kbitor,	$+1,  1,  1,  0),  !     (T b c)	T := b ior c
	(kbitxor,	$+1,  1,  1,  0),  !     (T b c)	T := b ixor c
	(kshl,		$+1,  1,  1,  0),  !     (T b c)	T := b << c
	(kshr,		$+1,  1,  1,  0),  !     (T b c)	T := b >> c
	(kmin,		$+1,  1,  1,  0),  !     (T b c)	T := min(b, c)
	(kmax,		$+1,  1,  1,  0),  !     (T b c)	T := max(b, c)
  
	(katan2,	$+1,  1,  1,  0),  !     (T b c)	T := atan2(b, c)
	(kpower,	$+1,  1,  1,  0),  !     (T b c)    T := b ** c
	(kfmod,		$+1,  1,  1,  0),  !     (T b c)
  
	(ksubpx,	$+1,  1,  1,  0),  ! s   (T b c)	T := b - c*s
	(ksubp,		$+1,  1,  1,  0),  ! s   (T b c)	T := (b - c)/s

	(kneg,		$+1,  1,  1,  0),  !     (T b -)	T := -b
	(kabs,		$+1,  1,  1,  0),  !     (T b -)    T := abs b
	(kbitnot,	$+1,  1,  1,  0),  !     (T b -)    T := inot b
	(knot,		$+1,  1,  1,  0),  !     (T b -)    T := not b
	(ktoboolt,	$+1,  1,  2,  0),  !     (T b -)    T := istrue b
	(ktoboolf,	$+1,  1,  2,  0),  !     (T b -)    T := not istrue b
  
	(ksqr,		$+1,  1,  1,  0),  !     (T b -)    T := sqr(b)
  
	(ksqrt,		$+1,  1,  1,  0),  !     (T b -)	T := sqrt(b)
	(ksin,		$+1,  1,  1,  0),  !     (T b -)	T := sin(b)
	(kcos,		$+1,  1,  1,  0),  !     (T b -)	T := cos(b)
	(ktan,		$+1,  1,  1,  0),  !     (T b -)	T := tan(b)
	(kasin,		$+1,  1,  1,  0),  !     (T b -)	T := asin(b)
	(kacos,		$+1,  1,  1,  0),  !     (T b -)	T := asin(b)
	(katan,		$+1,  1,  1,  0),  !     (T b -)	T := atan(b)
 
	(klog,		$+1,  1,  1,  0),  !     (T b -)	T := log(b)
	(klog10,	$+1,  1,  1,  0),  !     (T b -)	T := log10(b)
	(kexp,		$+1,  1,  1,  0),  !     (T b -)	T := exp(b)
	(kround,	$+1,  1,  1,  0),  !     (T b -)	T := round(b)
	(kceil,		$+1,  1,  1,  0),  !     (T b -)	T := ceil(b)
	(kfloor,	$+1,  1,  1,  0),  !     (T b -)	T := floor(b)
	(kfract,	$+1,  1,  1,  0),  !     (T b -)	T := fract(b)
	(ksign,		$+1,  1,  1,  0),  !     (T b -)	T := sign(b)

	(kfloat,    $+1,  1,  2,  0),  !     (T b -)	T := float(b)
	(kfix,		$+1,  1,  2,  0),  !     (T b -)	T := fix(b)
	(ktruncate,	$+1,  1,  2,  0),  !     (T b -)	T := u(b)
	(kfwiden,	$+1,  1,  2,  0),  !     (T b -)	T := r64(b)
	(kfnarrow,	$+1,  1,  2,  0),  !     (T b -)	T := r32(b)
	(kwiden,	$+1,  1,  2,  0),  !     (T b -)	T := t(b)
  
	(ktypepun,	$+1,  1,  2,  0),  !     (T b -)	T := t(u@(b))
  
	(kaddto,	$+1,  0,  1,  1),  !     (P b -)    P +:= b
	(ksubto,	$+1,  0,  1,  1),  !     (P b -)	P -:= b
	(kmulto,	$+1,  0,  1,  1),  !     (P b -)	P *:= b
	(kdivto,	$+1,  0,  1,  1),  !     (P b -)	P /:= b (float)
	(kidivto,	$+1,  0,  1,  1),  !     (P b -)	P /:= b (int: %:= b)
	(kiremto,	$+1,  0,  1,  1),  !     (P b -)	P irem:= b
	(kbitandto,	$+1,  0,  1,  1),  !     (P b -)	P iand:= b
	(kbitorto,	$+1,  0,  1,  1),  !     (P b -)	P ior:= b
	(kbitxorto,	$+1,  0,  1,  1),  !     (P b -)	P ixor:= b
	(kshlto,	$+1,  0,  2,  1),  !     (P b -)	P <<:= b
	(kshrto,	$+1,  0,  2,  1),  !     (P b -)	P >>:= b
	(kminto,	$+1,  0,  1,  1),  !     (P b -)	P min:= b
	(kmaxto,	$+1,  0,  1,  1),  !     (P b -)	P max:= b
	(kaddpxto,	$+1,  0,  1,  1),  ! s   (P b -)    P +:= b*s
	(ksubpxto,	$+1,  0,  1,  1),  !     (P b -)    P -:= b*s
 
	(knegto,	$+1,  0,  1,  1),  !     (P - -)    -:=P
	(kabsto,	$+1,  0,  1,  1),  !     (P - -)    abs:=P
	(kbitnotto,	$+1,  0,  1,  1),  !     (P - -)	inot:=P
	(knotto,	$+1,  0,  1,  1),  !     (P - -)    not:=P
	(ktoboolto,	$+1,  0,  1,  1),  !     (P - -)    istrue+:=P
  
	(kincrto,	$+1,  0,  1,  1),  !     (P - -)	++P
	(kdecrto,	$+1,  0,  1,  1),  !     (P - -)	--P
	(kincrload,	$+1,  1,  1,  2),  !     (T P -)	T := ++P
	(kdecrload,	$+1,  1,  1,  2),  !     (T P -)	T := --P
	(kloadincr,	$+1,  1,  1,  2),  !     (T P -)	T := P++
	(kloaddecr,	$+1,  1,  1,  2),  !     (T P -)	T := P--
  
	(kswitch,	$+1,  0,  1,  0),  ! x,y (L L2 c)	switch on c; L=jumptable, L2=else label
	(kswitchu,	$+1,  0,  1,  0),  ! x,y (L L2 c)	switch on c; L=jumptable, L2=else label; unchecked
	(kswlabel,	$+1,  0,  0,  0),  !     (L - -)	label for switch jump table

	(kstop,		$+1,  0,  0,  0),  !
	(klabel,	$+1,  0,  0,  0),  !     (L - -)
  
	(kdata,		$+1,  0,  1,  0),  !
  
	(kloadbit,	$+1,  1,  1,  0),  !     (T b c)	T := b.[c]
	(kloadbf,	$+1,  1,  1,  0),  !     (T b c d)	T := b.[c..d]
	(kstorebit,	$+1,  0,  1,  1),  !	 (P b c)	P.[b] := c
	(kstorebf,	$+1,  0,  1,  1),  !     (P b c d)	P.[b..c] := d
	(kidivrem,	$+1,  2,  1,  0),  !     (T T c d)  (T1, T2) := C divrem d

	(kjumpin,	$+1,  0,  1,  0),  !     (L b c d)  goto L when b in c..d
	(kjumpout,	$+1,  0,  1,  0),  !     (L b c d)  goto L when b not in c..d
  
	(kclear,	$+1,  0,  1,  1),  !     (P - -)    clear P
  
	(klast,		$+1,  0,  0,  0),  !

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

export enumdata [0:]ichar idnames
	(null_id=0,		"--"),			!Not set (used for overall program name)
	(import_id,		"Import"),		!Imported symbol (proc or static)
	(proc_id,		"Proc"),		!Local proc
	(static_id,		"Static"),		!Local static
	(local_id,		"Local"),		!Function local var
	(param_id,		"Param"),		!Function param
	(label_id,		"Label"),		!Used in assembly
	(export_id,		"Export"),		!Used by makesymbol, is converted to proc_id/.exported
	(misc_id,		"Misc"),		!?
	(program_id,	"Program"),		!?
end

[]byte callops_d = (kcall, katan2, kpower, kfmod, ksin, kcos, ktan,
	kasin, kacos, katan, klog, klog10, kexp, kpower, kceil, kfloor)

global [tclnames.bounds]byte tclhascall			!has 1 when op may involve a call

proc start=
	for x in callops_d do
		tclhascall[x]:=1
	od
end

!Docs

!TYPEPUN combinations recognised for 64-bit targets:
!  T1 := typepun(sx)                     i64/r32         |<T1: 
!  T3 := typepun(sx)                     u64/r32         |<T3: 
!
!  T1 := typepun(x)                      i64/r64         |<T1: 
!  T3 := typepun(x)                      u64/r64         |<T3: 
!
!  T5 := typepun(a)                      r32/i64         |<T5: 
!  T5 := typepun(u)                      r32/u64         |<T5: 
!
!  T7 := typepun(a)                      r64/i64         |<T7: 
!  T7 := typepun(u)                      r64/u64         |<T7: 
!
!  T1 := typepun(x)                      i64/r64         |<T1: 
!  T3 := typepun(x)                      u64/r64         |<T3: 

!r32 source is typepunnded to 32 bits but is then widened to 64. The alternative
!would have been an untidy extra conversion. Otherwise widths must match.
=== info.txt 0 1 19/21 ===
    The 'MCC' C Compiler comprises:

    mcc.exe            Compiles to .asm files
    aa.exe             Assembles .asm files to .obj files
                       Assemblers and links .asm/.dll files to .exe
    Standard headers   A minimal set inside mcc.exe
    windows.h          As a standalone file

    Input files:

      prog             This is prog.c as the extension is optional
      prog.c
      lib.dll          Include .dll library when generating .exe
      @file            Read parameters and optons from given file

    Options:

      -exe             (DEFAULT) Compile all modules to one .exe file via .asm files
      -e               Preprocess each module to .i file
      -s               Compile each module to .asm file
      -c               Compile each module .obj via .asm

      -out:file        Specify output file for -exe only

    For .exe output, it will be named based on the first input file. Otherwise
    use -out option

    .obj files can be linked using gcc on Windows. This option is
    needed to be able to generate .dll files. However, this will not
    work on newer gcc versions because mcc's generated code is not position
    independent, and will only work loaded in the low 2GB of address space.

    Libraries msvcrt.dll, gdi32.dll, user32.dll and kernel32.dll are
    automatically included as search libraries for imported functions.

    Other kinds of binary libraries or files (.a, .lib, .obj etc) are not supported.

    Omissions, Restrictions and Bugs (highlights only as there are dozens):

      * No VLAs, compound literals, designated initialisers
      * Restrictions on complexity of data initialisers
=== cc_help.txt 0 1 20/21 ===
C Subset Compiler for 64-bit Windows

Normal use:           Compiles prog.c to:

    cc      prog      prog.exe (default)
    cc -r   prog      in-memory native code then execute
    cc -i   prog      in-memory IL then interpret

    cc -exe prog      prog.exe
    cc -dll prog      prog.dll
    cc -obj prog      prog.obj
    cc -a   prog      prog.asm
    cc -n   prog      prog.nasm
    cc -mx  prog      prog.mx
    cc -p   prog      prog.pcl (textual IL)
    cc -e   prog      prog.i   (preprocess only)

Other options:

    -incl:path        Add path to search for includes
    -ext              Used std headers external to compiler
    -opt              Optimise native code
    -out:file         Name output file (extension can be added)
    -rip              Use RIP address modes
    -himem            Generate PIC code (automatic with -obj/-dll)
    @file             Read files and options from a file
    file.dll          Include one or more extra dynamic libs
    -c                Same as -obj
    -s                Same as -asm
    -ei               Same as -e, but when result is interpreted

Notes:

    * Compiles single module only (use bcc driver program for multiple-modules)
    * For -i and -r, options and any .dlls must appear before the C source file
    * .c extension is optional on input file
=== mcc.h 0 1 21/21 ===
#define __attribute__(x)
#define _WIN32
#define WIN32
#define __WIN32__
#define __inline
#define __dllimport(x)
#define __declspec(x)
#define __stdcall
#define CALLBACK $callback
#define __cdecl
#define EXTERN_C extern
#define DECLSPEC_IMPORT
#define __32BIT__
#define register
#define __MCCC__

typedef signed char		i8;
typedef short			i16;
typedef int				i32;
typedef long long int	i64;
typedef unsigned char			u8;
typedef unsigned short			u16;
typedef unsigned int			u32;
typedef unsigned long long int	u64;

typedef unsigned char byte;

typedef float r32;
typedef double r64;

=== END ===
1 cc.m 0 0
2 cc_cli.m 0 0
3 dummy.m 0 0
4 cc_decls.m 0 0
5 cc_tables.m 0 0
6 cc_lex.m 0 0
7 cc_parse.m 0 0
8 cc_gentcl.m 0 0
9 cc_blocktcl.m 0 0
10 cc_libtcl.m 0 0
11 cc_lib.m 0 0
12 cc_support.m 0 0
13 cc_headersx.m 0 0
14 cc_show.m 0 0
15 tc_api.m 0 0
16 tc_decls.m 0 0
17 tc_diags.m 0 0
18 tc_tables.m 0 0
19 info.txt 0 1
20 cc_help.txt 0 1
21 mcc.h 0 1
