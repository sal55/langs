=== MA 32 ===
=== cc.m 0 0 1/32 ===
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
    $sourcepath "c:/bx/"

!	import tcl

	module tc_api
	module tc_decls
	module tc_diags
	module tc_tables

	module mc_decls_x		!x64-generating API
	module mc_lib_x
	module mc_asm_x

!	module mc_gen_x			!tcl -> mcl/x64
!	module mc_aux_x
!	module mc_conv_x
!	module mc_temp_x

	module mc_gen_xb			!tcl -> mcl/x64
	module mc_aux_xb
	module mc_conv_xb
	module mc_temp_xb

	module mc_objdecls
	module mc_genss
	module mc_writeexe

!	module mc_writess
!	module mc_disasm

	module mx_run_dummy
!	module mx_decls
!	module mx_run
!	module mx_lib
!	module mx_write
!	module mx_show

end
=== cc_cli.m 0 0 2/32 ===
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
	(tcl_sw,		"t",			&cc_pass,		tcl_pass),
	(tcli_sw,		"ti",			&cc_pass,		tcl_pass),
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

!CPL "SS", $LINENO
	do_gentcl()
!CPL "SS", $LINENO

	case cc_pass
	when runtcl_pass then
		tcl_runtcl()

	when mcl_pass then
		do_genmcl()

!	when nasm_pass then
	when asm_pass then
		do_asm()

	when obj_pass then
		do_obj()
!*!		

	when dll_pass then
		do_dll()

	when exe_pass then
		do_exe()

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

!CPL "TCL1"
!
	if fshowtcl or cc_pass=tcl_pass then			!need discrete file
!	if cc_pass=tcl_pass then			!need discrete file
		tcl_writetcl(changeext(outfile,"tcl"))
	fi
!CPL "TCL2"

!	if fshowpst then
!		tcl_writepst("PSYMTAB")
!	fi
!CPL "TCL3"

end

proc do_genmcl=
!CPL "DOMCL"
	return unless cc_pass >= mcl_pass

!CPL "DOMCL2"
	if cc_pass=mcl_pass then			!need discrete file
!CPL "DOMCL3"
!CPL "GENMCL_WRITEASM"
		tcl_writeasm(changeext(outfile, (ctarget|"cc"|"asm")))
	fi
end

proc do_asm=
!CPL "DOASM"
	return unless cc_pass >= asm_pass
!CPL "DOASM2"
	tcl_writeasm(outfile)
end

proc do_obj=
	return unless cc_pass = obj_pass
!*!	tcl_writeobj(outfile)
end

proc do_dll=
	return unless cc_pass = dll_pass
	tcl_writedll(outfile)
end

proc do_exe=
	return unless cc_pass = exe_pass
	tcl_writeexe(outfile)
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
!		println @logdev, tcl_writeasm(nil)
		addtolog(changeext(outfile, (ctarget|"cc"|"asm")), logdev)
	fi

	if fshowtcl and cc_pass>=tcl_pass then
		addtolog(changeext(outfile, "tcl"), logdev)
	fi
	if fshowpst and cc_pass>=tcl_pass then
		println @logdev, tcl_writepst()
!		addtolog("PSYMTAB", logdev)
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
	if ctarget and cc_pass in [mcl_pass, asm_pass] then
		outext:="cc"
	fi

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
=== dummy.m 0 0 3/32 ===
!global proc genss(int obj=0)=end

!global proc writeexe(ichar outfile, int dodll, ichar entrypoint=nil)=end
!
export function writessdata(int fexe)ref strbuffer= nil end

!export const ctarget=0

global const maxtuplesize = 4
=== cc_decls.m 0 0 4/32 ===
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

!global record fwdrec =
!	ref fwdrec nextfwd
!	i32 offset
!	i16 reltype
!	i16 seg
!end
!
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

global const maxparams = 100
=== cc_tables.m 0 0 5/32 ===
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

=== cc_lex.m 0 0 6/32 ===
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
=== cc_parse.m 0 0 7/32 ===
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

		if x.tag=jconst and y.tag=jconst then
			case u
			when ti32,ti64 then
				if opc=shlsym then
					x.value := x.value << y.value
				else
					x.value := x.value >> y.value
				fi
!				fixvalue(x.value, u)
				nextloop
			when tu32,tu64 then
				if opc=shlsym then
					x.uvalue := x.uvalue << y.value
				else
					x.uvalue := x.uvalue >> y.value
				fi
!				fixvalue(x.value, u)
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
!			scope:=exported_scope
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
!			fixvalue(p.value, t)
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

!proc fixvalue(i64 &a, t)=
!!reduce any value a to 32 bits according to t
!!	case t
!!	when ti32 then
!!		a:=i32(a)
!!	when tu32 then
!!		a:=u32(a)
!!	esac
!
!end
=== cc_gentcl.m 0 0 8/32 ===

const fscantemps=1
!!const fscantemps=0

const freducetemps=1
!const freducetemps=0



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

global proc codegen_tcl=

	dolibs()

	scanstatics()

	scanimports()

	scanprocs()
!CPL "DONE CODEGEN"

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
	psymbol p
	ref paramrec pm

	d:=stmodule.deflist

	while d, d:=d.nextdef do
		if d.nameid=procid and d.scope=imported_scope and d.used then

			tc_setimport(getpsymbol(d))

!			PRINTLN "IMPORT:", D.NAME, SCOPENAMES[D.SCOPE], =D.USED

			while pm, pm:=pm.nextparam do
				p:=getpsymbol(pm.def)
				p.mode:=gettclmode(pm.mode)
				p.size:=ttsize[pm.mode]
				tc_addparam(p)
			od
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
			if e.code then checkframeinit(e) fi

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

	if p.isentry then
		tc_gen(kstop, tc_genint(0))
		setmode(ti32)
	fi

	p.code:=tcl_end()

	tc_currfunc(nil)
	p.maxtemp:=ntemps

	scanproctemps(p) when fscantemps
!	reducetemps(p) when freducetemps and ctarget
	reducetemps(p) when freducetemps
end

proc checkframeinit(symbol d)=
!d has .code set; see if it needs a static autobvar
	[256]char str
	int align
	symbol e
	psymbol p

	if d.code.tag=jmakelist or ttbasetype[d.mode]=tarray and d.code.tag=jconst then
		fprint @str,"$#.#.#", d.owner.name, d.name, d.blockno
		e:=createdupldef(nil, addnamestr(str), staticid)
		p:=getpsymbol(e)
		p.mode:=gettclmode(d.mode)
		p.size:=ttsize[d.mode]
		d.pdata:=p

		tc_addstatic(p)
		do_idata(d)
		e.pdef.code:=d.pdef.code
		d.pdef.code:=nil
!		swap(e.pdef.code, d.pdef.code)

	fi
	
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
				tc_gen(kdata, tc_genint(ref u32(&sx)^))
			else

				tc_gen(kdata, tc_genint(p.value))
			fi
			setmode(t)
		elsif ttbasetype[t]=tref then
			padding:=0
	doref:
			if p.value=0 then
				tc_gen(kdata, tc_genint(0))

			elsif p.isstrconst then
				tc_gen(kdata, tc_genstring(p.svalue))

			elsif p.iswstrconst then
GERROR("GENIDATA/WSTRING2")
				doresb(padding)
			else
				tc_gen(kdata, tc_genint(p.value))
			fi
			setmode(t)

		elsif ttbasetype[t]=tarray then
			padding:=(ttlength[t]-p.slength)*ttsize[tttarget[t]]
			for i to p.slength do
				tc_gen(kdata, tc_genint((p.svalue+i-1)^))
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
			tc_gen(kdata, genmemaddr_d(d))
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
				tc_gen(kdata, tc_genname(&.str))
			else
				gerror("Add/Idata &frame")
			esac	
		elsif a.tag=jconst and b.tag=jconst and ttbasetype[a.mode]=tref then		!ASSUME REF+REF
			print @&.str, a.value, ,"+", ,b.value
			tc_gen(kdata, tc_genname(&.str))

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
		tc_gen(kdata, tc_genint(0))
		n-:=8
		setmode(tu64)
	od
	to n do
		tc_gen(kdata, tc_genint(0))
		setmode(tu8)
	od

end

proc dolibs=
	for i to nlibfiles do
		tc_addplib(libfiles[i])
	od
end

=== cc_blocktcl.m 0 0 9/32 ===
[maxnestedloops]int continuestack		!labels for continue/break
[maxnestedloops]int breakstack
int loopindex							!current level of nested loop/switch blocks

const maxswitchrange=500
const maxcases=maxswitchrange
const maxswitchdepth=20

ref[]i32 sw_labeltable			!set from do-switch
ref[]i32 sw_valuetable
int sw_lower
int sw_ncases					!1..n for serial switch; 0 for simple
byte sw_defaultseen				!starts at 0, set to 1 when default: seen
int sw_defaultlabel
int sw_breaklabel

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
		dx_call(p, a, b, 0)

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

	when jcasestmt then
		do_casestmt(p, a)

	when jdefaultstmt then
		sw_defaultseen:=1
		tc_gen(klabel, tc_genlabel(sw_defaultlabel))
		do_stmt(a)

	when jbreaksw then
		genjumpl(sw_breaklabel)

	when jbreak then
		genjumpl(breakstack[loopindex])

	when jcontinue then
		genjumpl(continuestack[loopindex])

	when jswitch then
		do_switch(p, a, b)

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
		tc_gen(keval, tx)
		setmode_u((a|a|p))

	end switch

end

func dx_expr(unit p, int am=0)tclopnd tx =
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
	when jfuncname then
		tx:=tc_gent(kmove, genmemaddr_d(p.def))
		setmode(tu64)

	when jassign then
		tx:=do_assign(a, b, 1)

	when jandl, jorl then
		tx:=dx_andorl(p)

	when jnotl then
		if a.tag=jnotl then
			p:=a
			a:=p.a
			doistruel
		elsif a.tag in jeq..jge then
			a.tag:=reversetclcond(a.tag)
			tx:=dx_expr(a)

		elsif isbool(a) then
			tx:=tc_gent(knot, dx_expr(a))
			setmode(ti32)
		else
			tx:=tc_gent(ktoboolf, dx_expr(a))
			setmode(ti32)
			setmode2(a.mode)
		fi

	when jistruel then
doistruel:
		if isbool(a) then
			tx:=dx_expr(a)
		else
			tx:=tc_gent(ktoboolt, dx_expr(a))
			setmode(ti32)
			setmode2(a.mode)
		fi

	when jexprlist then
		while a, a:=b do
			b:=a.nextunit

			if b and a.tag in [jassign, jconvert, jifx] then
				do_stmt(a)
			else
				tx:=dx_expr(a)
				if b and (a.mode<>tvoid or a.tag=jconvert) then
					tc_gen(keval, tx)
				fi
			fi
		od

	when jcallfn then
		tx:=dx_call(p, a, b, 1)
		if tx=nil then				!Lua can cast void function result
			tx:=tc_genint(0)
			setmode(ti32)
		fi

	when jifx then
		tx:= dx_ifx(p, a, b, p.c)

	when jeq, jne, jlt, jle, jge, jgt then
		tx:=dx_eq(p, a, b)

	when jadd then
		if ttisref[a.mode] and ttsize[b.mode]<=4 then
			b.mode:=tu64
		fi
		tx:=dx_bin(a, b, kadd)

	when jsub then
		tx:=dx_bin(a, b, ksub)
!
	when jmul then
		tx:=dx_bin(a, b, kmul)

	when jdiv then
		tx:=dx_bin(a, b, (isrealcc(a.mode)|kdiv|kidiv))
!
	when jrem then
		tx:=dx_bin(a, b, kirem)
!
	when jiand then
		tx:=dx_bin(a, b, kbitand)
!
	when jior then
		tx:=dx_bin(a, b, kbitor)
!
	when jixor then
		tx:=dx_bin(a, b, kbitxor)
!
	when jshl then
		tx:=dx_bin(a, b, kshl)
!
	when jshr then
		tx:=dx_bin(a, b, kshr)
!
	when jptr then
		tx:=dx_ptr(p, a, am)

	when  jaddptr then
		tx:=dx_addptr(p, a, b)
!
	when  jsubptr then
		tx:=dx_subptr(p, a, b)

	when jconvert then
		if p.convmode=tvoid then
			tx:=dx_expr(a)
		else
			tx:=dx_convert(p, a, p.convmode, p.opcode)
		fi

	when jscale then
		tx:=dx_scale(p, a, b)

	when jneg then
		tx:=tc_gent(kneg, dx_expr(a))
		setmode_u(a)

	when jinot then
		tx:=tc_gent(kbitnot, dx_expr(a))
		setmode_u(a)

	when jpreincr, jpredecr, jpostincr, jpostdecr then
		tx:=dx_incrload(p, a)

	when jaddto then
		tx:=dx_bintox(a, b, kaddto)

	when jsubto then
		tx:=dx_bintox(a, b, ksubto)

	when jmulto then
		tx:=dx_bintox(a, b, kmulto)

	when jdivto then
		tx:=dx_bintox(a, b, (isrealcc(a.mode)|kdivto|kidivto))

	when jremto then
		tx:=dx_bintox(a, b, kiremto)

	when jiandto then
		tx:=dx_bintox(a, b, kbitandto)

	when jiorto then
		tx:=dx_bintox(a, b, kbitorto)

	when jixorto then
		tx:=dx_bintox(a, b, kbitxorto)

	when jshlto then
		tx:=dx_bintox(a, b, kshlto)

	when jshrto then
		tx:=dx_bintox(a, b, kshrto)
!
	when jaddrof then
		tx:=dx_expr(a, 1)

	when jdot then
		tx:=dx_dot(p, a, b, am)

	when jsetjmp then
		tc_gen(ksetjmp, dx_expr(a))
		setmode(tu64)
		tc_gen(kgetr0, tx:=tc_gentemp())
		setmode(ti64)
!
!RETURN TC_GENINT(0)

	when jlongjmp then
		tc_gen(klongjmp, dx_expr(a), dx_expr(b))
		setmode(tu64)
RETURN TC_GENINT(0)
!RETURN NIL

	else
		gerror_s("DX-EXPR: can't do tag: #", jtagnames[p.tag])
	end switch

	clineno:=oldclineno

IF TX=NIL THEN CPL JTAGNAMES[P.TAG]
GERROR("DX-EXPR: TX=NIL") FI

	tx
end

func isbool(unit p)int =
	if p.tag in [jnotl, jistruel] or p.tag in jeq..jgt then
		1
	else
		0
	fi
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

	if ttsize[mode]<4 then
		tx:=tc_gent(kwiden, dx)
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

	if ttsize[mode]<4 then
		if tccurr.opcode in [kmove, kiloadx, kincrload, kdecrload, kloadincr, kloaddecr] then
dowiden:
			tx:=tc_gent(kwiden, dx)
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

func do_assign(unit a, b, int res)tclopnd =
	tclopnd lhs, rhs

	rhs:=dx_expr(b)

	case a.tag
	when jname then
		tc_gen(kmove, genmem_u(a), rhs)
		setmode(getmemmode(a))

	when jptr then
		tc_gen_ixs(kistorex, dx_expr(a, am:1), nil, rhs)
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
!TC_COMMENT("DOT1")
!TCLOPND AX:=DX_EXPR(A, AM:1)
!TC_COMMENT("DOT2")
!PRINTUNIT(NIL, A)

!		tc_gen_ixs(kistorex, ax, nil, rhs, offset:a.offset)
!		tc_gen_ixs(kistorex, dx_expr(a, am:1), nil, rhs, offset:a.offset)
		tc_gen_ixs(kistorex, dx_expr(a, am:1), nil, rhs)
!TC_COMMENT("DOT3")


!		tc_setscaleoff(1)
		setmode(getmemmode(a))

	else
		GERROR_S("DOASSIGN not ready: #", jtagnames[a.tag])
	esac

	rhs
end

func dx_bin(unit a, b, int opc)tclopnd tx=
	tclopnd ax, bx

	ax:=dx_expr(a)
	bx:=dx_expr(b)

	tx:=tc_gent(opc, ax, bx)
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
		tc_gen(opc, tc_genlabel(lab), dx_expr(p))
		setmode_u(p)
	end switch
end

proc gcomparejump(int jumpopc, unit p, lhs, rhs, int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int cond
	tclopnd a, b

	cond:=gettclcond(p.tag)			!jeq => keq etc
	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	a:=dx_expr(lhs)
	b:=dx_expr(rhs)

	tc_gen(kjumpcc, tc_genlabel(lab), a, b)
	tccurr.cond:=cond
	setmode_u(lhs)

end

function gettclcond(int op)int=
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
!reverse conditional operator, in that it is the opposite, so > becomes <=
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

global func reversetclcond(int cc)int=
!reverse jeq codes rather than eq_cc
!reverse conditional operator, in that it is the opposite, so > becomes <=
	case cc
	when jeq then cc:=jne
	when jne then cc:=jeq
	when jlt then cc:=jge
	when jle then cc:=jgt
	when jge then cc:=jlt
	when jgt then cc:=jle
	esac

	return cc
end

!global func reversecond_order(int cc)int=
!!change eg. > to <, so that a > b can become b < a
!!this allows swapping operands, but keeping same comparison
!
!	case cc
!	when eq_cc then cc:=eq_cc
!	when ne_cc then cc:=ne_cc
!	when lt_cc then cc:=gt_cc
!	when le_cc then cc:=ge_cc
!	when ge_cc then cc:=le_cc
!	when gt_cc then cc:=lt_cc
!	esac
!
!	return cc
!end


proc genjumpl(int lab)=
!generate unconditional jump to label
	tc_gen(kjump, tc_genlabel(lab))
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
		if currfunc.isentry then
			tc_gen(kstop, dx_expr(a))
		else
			tc_gen(kretfn, dx_expr(a))
		fi
		setmode_u(a)
	else
		tc_gen(kretproc)
	fi
end

func dx_call(unit p, a, b, int res)tclopnd=
	ref paramrec pm
	int isfnptr, variadic, nparams, retmode, nbytes, m, nvariadics
	int nfixedparams, isfn, nret
	[maxparams]unit paramlist
	[maxparams]tclopnd paramopnds
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
!IF TTISBLOCK[P.MODE] THEN
!	CPL "CALL BLOCK FUNC PTR", STRMODE(P.MODE)
!FI

	else
		pm:=a.def.paramlist

!IF TTISBLOCK[P.MODE] THEN
!	DTEMP:=A.DEF
!	CPL "CALL BLOCK FUNC", DTEMP.NAME, STRMODE(P.MODE)
!FI

		isfnptr:=0
		isfn:=a.def.mode<>tvoid
	esac

	variadic:=pm.flags=pm_variadic
	nfixedparams:=pm.nparams
	nparams:=nvariadics:=0

	q:=b
	while q, q:=q.nextunit do
		if nparams>=maxparams then gerror("maxparams") fi
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
		tx:=extretopnds[1]:=tc_gentemp()
		setopndmode(tx, p.mode)
		tc_gen_call(ax, 1, nparams)
	fi

	if variadic then
		tccurr.isvariadic:=1
	fi

	tccurr.nargs:=nparams
	tccurr.isvariadic:=nvariadics
	tccurr.mode:=gettclmode(p.mode)
	tccurr.size:=ttsize[p.mode]

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
		tc_gen(kmove, genmem_d(d), dx_expr(a))
		setmode(a.mode)
		return
	fi

copyl:
	tc_gen(kmove, genmem_d(d), tc_genmem(d.pdata))
	setmode(d.mode)
end

proc do_preincr(unit a, int incrop)=
	tclopnd px

	if a.tag=jname then
		tc_gen(incrop, genmem_u(a))
	else
		px:=evallv(a)
		tc_gen(incrop, px)
	fi
	setmode(getmemmode(a))
	setincrstep(a.mode)
end

func evallv(unit p)tclopnd tx=
	tx:=makeindlv(dx_expr(p, am:1), p.mode)
	return tx
end

proc setincrstep(int m)=
	tccurr.step:=1

	if ttisref[m] then
		tccurr.step:=ttsize[tttarget[m]]
	fi
end

func do_binto(unit a, b, int opc)tclopnd =
!returns pointer ref for ise from bintox
	tclopnd px, bx

	bx:=dx_expr(b)

	if a.tag=jname then
		px:=genmem_u(a)
	else
		px:=evallv(a)
	fi

	tc_gen(opc, px, bx)
	setmode(getmemmode(a))

	px
end

func dx_bintox(unit a, b, int opc)tclopnd tx=
!res=1 means value must be retained
	tclopnd px, bx

	px:=do_binto(a, b, opc)

	if px.optype=temp_opnd then
		px:=makeind(px, getmemmode(a))
		if px.optype=temp_opnd then
			return px
		fi

	else
		return px
	fi

	tx:=tc_gent(kmove, px)
	setmode(getmemmode(a))
	tx
end

func dx_ptr(unit p, a, int am)tclopnd tx=
	if am=0 then				!normal ptr deref
		tx:=tc_gen_ix(kiloadx, dx_expr(a))
		setmode(getmemmode(p))
		widen(p, tx)
	else						!else just load the pointer
		dx_expr(a)
	fi
end

func dx_addptr(unit p, a, b)tclopnd tx=
	tx:=tc_gen_ix(kaddpx, dx_expr(a), dx_expr(b), p.ptrscale)
	setmode(tu64)
	tx
end

func dx_subptr(unit p, a, b)tclopnd tx=
	tx:=tc_gent(ksubpx, dx_expr(a), dx_expr(b))
	tccurr.scale:=ttsize[tttarget[a.mode]]
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

	case opc
	when hard_c then
!hard is an explicit cast for which no built-in code such as swiden_c has
!been detected. So just do a kind of type-punning, but ensure the sizes
!are correct

!		if stdcat[ttbasetype[s]]=realcat then gerror("Bad cast") fi
		if ttbasetype[s] in [tr32, tr64] then gerror("Bad cast") fi

		if tsize>ssize then			!widen
			if ssize=0 then return ax fi
			tx:=tc_gent(kwiden, ax)
		elsif tsize<ssize then
			goto dotruncate
			return ax
		ELSE
			RETURN AX
		fi

	when swiden_c, uwiden_c then
		if ssize=tsize then
			return ax
		fi
		tx:=tc_gent(kwiden, ax)

	when sfloat_c, ufloat_c then
		tx:=tc_gent(kfloat, ax)

	when sfix_c, ufix_c then
		tx:=tc_gent(kfix, ax)

	when fwiden_c then
		tx:=tc_gent(kfwiden, ax)

	when fnarrow_c then
		tx:=tc_gent(kfnarrow, ax)

	when narrow_c, truncate_c then
dotruncate:
		tx:=tc_gent(ktruncate, ax)

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

func dx_incrload(unit p, a)tclopnd tx=
!also loadincr
	tclopnd px	
	int opc

	case p.tag
	when jpreincr then opc:=kincrload
	when jpredecr then opc:=kdecrload
	when jpostincr then opc:=kloadincr
	else				opc:=kloaddecr
	esac

	if a.tag=jname then
		tx:=tc_gent(opc, genmem_u(a))
	else
		tx:=tc_gent(opc, evallv(a))
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

func dx_ifx(unit p, a, b, c)tclopnd tx=
	int lab1, lab2

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
	lab2:=createfwdlabel()

	tx:=tc_gentemp()

	genjumpcond(kjumpf, a, lab1)

	fixtemp(p, tx, dx_expr(b))

	genjumpl(lab2)
	definefwdlabel(lab1)

	fixtemp(p, tx, dx_expr(c))

	definefwdlabel(lab2)

	tx
end

func dx_dot(unit p, a, b, int am)tclopnd tx=
	tclopnd px

	px:=dx_expr(a, am:1)

	if am=1 then
		tx:=tc_gen_ix(kaddpx, px, dx_expr(b), offset:p.offset)
		setmode(tu64)
	else
		tx:=tc_gen_ix(kiloadx, px, dx_expr(b), offset:p.offset)
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
	tc_gen(klabel, tc_genlabel(d.index))
end

proc do_goto(symbol d)=
	if d.index=0 then
		gerror_s("Label not defined: #", d.name)
	elsif d.index<0 then
		d.index:=++mlabelno	
	fi
	tc_gen(kjump, tc_genlabel(d.index))
end

proc do_switch(unit p, a, b)=
!need to create switch levels, as they can be nested; nested case labels
!belong to the top switch level
	[maxswitchrange]i32 labeltable				!sw_length+1 labels
	[maxcases]i32 valuetable					!sw_length+1 labels
	[maxswitchrange]byte flags					!flags to check dupl values
	int defaultlabel							!index of fwd default label
	int breakswlabel							!index of fwd break label
	int switchlabel								!index of fwd break label
	int lower, upper							!ower/upper ranges of switch case values
	int length, value, ncases
	byte serialsw
	int i, index
!int sw_index
	ref caserec pcase
	tclopnd ix

!store current set of global values for outer switch
	ref[]i32 old_labeltable
	ref[]i32 old_valuetable
	int old_ncases, old_lower
	byte old_defaultseen
	int old_defaultlabel
	int old_breaklabel

	pcase:=p.nextcase
	ncases:=length:=0

	while pcase do
		++ncases
		if ncases>maxcases then
			gerror("Too many cases on one switch")
		fi
		valuetable[ncases]:=value:=pcase.value

		if ncases=1 then
			lower:=upper:=value
		else
			lower:=min(lower, value)
			upper:=max(upper, value)
		fi
		pcase:=pcase.nextcase
	od

	if p.nextcase then
		length:=upper-lower+1
	else
		length:=0
	fi 

!allocate fwd labels
	defaultlabel:=createfwdlabel()		!(when no default:, same as breakswlabel)
	breakswlabel:=createfwdlabel()

!	if length>maxswitchrange then
	if length>maxswitchrange OR NCASES<=8 then

!NOTES: SERIAL switch needs a way of checking duplicate case values.
!Better if not an n-squared search
!Short length switches should also be done serially (length<=8)
!Then a dupl check is simpler

		serialsw:=1

		ix:=dx_expr(a)

		for i:=1 to ncases do
			labeltable[i]:=createfwdlabel()

			tc_gen_cond(kjumpcc, eq_cc,
				tc_genlabel(labeltable[i]), ix, tc_genint(valuetable[i]))
			setmode(ti32)

		od

		genjumpl(defaultlabel)

	elsif length=0 then
		genjumpl(defaultlabel)

	else
		serialsw:=0
		memset(&flags, 0, length)				!clear value flags

!fill table with defaults first
		for i:=1 to length do
			labeltable[i]:=defaultlabel
		od

!now, do labels for each case value
		for i:=1 to ncases do
			value:=valuetable[i]
			index:=value-lower+1			!index of value within label table
			labeltable[index]:=createfwdlabel()

			if flags[index] then
				gerror_s("Dupl case value: #", strint(value))
			fi
			flags[index]:=1
		od

!need a label for the switchtable itself
		switchlabel:=createfwdlabel()

		tc_gen(kswitch, tc_genlabel(switchlabel), tc_genlabel(defaultlabel),
			dx_expr(a))
		setmode(ti32)
		tccurr.minlab:=lower
		tccurr.maxlab:=lower+length-1
		definefwdlabel(switchlabel)

		for i:=1 to length do
			tc_gen(kswlabel, tc_genlabel(labeltable[i]))
		od
!		tc_gen(kendsw)
	fi

!generate code for the switch body
!I need to make available essential tables, offsets etc necessary for j-case
!to be mappable to a label
!note: if already in an outer switch, then must save those earlier vars
!save outer switch vars
	old_labeltable:=sw_labeltable
	old_valuetable:=sw_valuetable
	old_lower:=sw_lower
	old_ncases:=sw_ncases
	old_defaultseen:=sw_defaultseen
	old_defaultlabel:=sw_defaultlabel
	old_breaklabel:=sw_breaklabel

!set globals
	sw_labeltable:=&labeltable
	sw_valuetable:=&valuetable		!NEEDED ONLY FOR COMPLEX SWITCH
	sw_lower:=lower

	sw_ncases:=(serialsw|ncases|0)
	sw_defaultseen:=0
	sw_defaultlabel:=defaultlabel
	sw_breaklabel:=breakswlabel

	do_stmt(b)						!switch body

!need to note whether a default label has been generated; if not, define
!default label here
	if not sw_defaultseen then
		definefwdlabel(defaultlabel)
	fi
!define breakswlabel here
	definefwdlabel(breakswlabel)

!restore any values of outer switch statement
	sw_labeltable:=old_labeltable
	sw_valuetable:=old_valuetable
	sw_lower:=old_lower
	sw_ncases:=old_ncases
	sw_defaultseen:=old_defaultseen
	sw_defaultlabel:=old_defaultlabel
	sw_breaklabel:=old_breaklabel
end

proc do_casestmt(unit p, a)=
	int value

	if sw_ncases=0 then
		tc_gen(klabel, tc_genlabel(sw_labeltable[p.value-sw_lower+1]))
	else
		value:=p.value
		for i:=1 to sw_ncases do
			if sw_valuetable[i]=value then
				tc_gen(klabel, tc_genlabel(sw_labeltable[i]))
				exit
			fi
		else
			gerror("case: serial switch not found")
		od
	fi
	do_stmt(a)
end

func dx_eq(unit p, a, b)tclopnd tx=
!apply =, <= etc between a and b, and get a logical result 1 or 0
	tc_gen_cond(ksetcc, gettclcond(p.tag), tx:=tc_gentemp(), dx_expr(a), dx_expr(b))
	setmode_u(a)
	tx
end

func dx_scale(unit p, a, b)tclopnd tx=
	if p.scale>=0 then
		tx:=tc_gent(kmul, dx_expr(a), tc_genint(p.scale))
	else
		tx:=tc_gent(kidiv, dx_expr(a), tc_genint(-p.scale))
	fi
	setmode(ti64)
	tx
end

func dx_andorl(unit p)tclopnd tx=
!do short-circuit evaluation of a&&b or a||b
!return operand containing 1 or 0
	int lab1, lab2

	lab1:=createfwdlabel()			!dest label of main condition (to end of if, or start if else)

	tx:=tc_gentemp()

	genjumpcond(kjumpf, p, lab1)

	lab2:=createfwdlabel()			!label past else part
	tc_gen(kmove, tx, tc_genint(1))
	setmode_u(p.a)
	genjumpl(lab2)

	definefwdlabel(lab1)
	tc_gen(kmove, tx, tc_genint(0))
	setmode(ti32)
	setmode_u(p.a)

	definefwdlabel(lab2)
	tx
end

proc fixtemp(unit p, tclopnd tx, ax)=
	if tx then
		tc_gen(kmove, tx, ax)
		setmode_u(p)
	fi
end

=== cc_libtcl.m 0 0 10/32 ===
!const freduce=1
const freduce=0

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

	p.used:=istrue d.used

	p.labelno:=d.index

	if d.nameid=procid and eqstring(d.name, "main") then
		d.ismain:=p.isentry:=1
	fi

	if d.nameid=procid then
		ref paramrec pm:=d.paramlist
		if pm.flags=pm_variadic then
			p.variadic:=pm.nparams			!imported, local or exported
		fi
	fi

!CPL "GETPS", D.NAME, D, STRMODE(D.MODE), =D.ALIGN

	if d.mode then
		p.align:=getalignment(d.mode)
	fi

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
	tc_gen(klabel, tc_genlabel(++mlabelno))
	return mlabelno
end

global func createfwdlabel:int =
	return ++mlabelno
end

global proc definefwdlabel(int lab) =
	tc_gen(klabel, tc_genlabel(lab))
end

global proc setopndmode(tclopnd p, int m)=
	p.opmode:=gettclmode(m)
	p.opsize:=ttsize[m]
end

global func makeind(tclopnd a, int m)tclopnd p=
	tc_makeind(a, gettclmode(m), ttsize[m])
end

global func makeindlv(tclopnd a, int m, size=0)tclopnd p=
	tc_makeindlv(a, gettclmode(m), ttsize[m])
end


=== cc_lib.m 0 0 11/32 ===
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
!REF STREC D:=TTNAMEDEF[M]
!
!CPL "GETALIGN/STRUCT", D.NAME, D, D.ALIGN
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

!global function ispoweroftwo(i64 x)int=
!!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
!	i64 a
!	int n
!
!	a:=1
!	n:=0
!	to 60 do
!		++n
!		a:=a<<1
!		if a=x then
!			return n
!		fi
!	od
!	return 0
!end

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

=== cc_support.m 0 0 12/32 ===
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
=== cc_headersx.m 0 0 13/32 ===
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
=== cc_show.m 0 0 14/32 ===
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

=== tc_api.m 0 0 15/32 ===
int STSEQNO

const freduce=0				!addpx etc
!const freduce=1

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

export int pstartclock
export int mcltime
export int sstime
export int exetime

global ref func (int pos, ichar &filename, &sourceline)int igetmsourceinfo

export byte fregoptim = 1
export byte fpeephole
export byte tc_useruntcl=0
!global byte pfullsys
export byte pverbose

export int mmpos

[maxfixedtemp]tempmoderec tempmodes
int nredtemps


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

!CPL "ADDPARAM", D.NAME

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

	tccurr.next:=p
	tccurr:=p

	tccurr.opcode:=opcode
	tccurr.nopnds:=nopnds
	tccurr.pos:=mmpos
	tccurr.seqno:=++pcseqno
	tccurr.ndest:=tclwrite[opcode]			!move/call are set manually

	return tccurr
end

global func newopnd:tclopnd=
	pcm_allocnfz(opndrec.bytes)
end

export proc tc_gen(int opcode, tclopnd a=nil, b=nil, c=nil)=
!general purpose, up to 3 operands.
!Any temp dest must be allocated by caller

	tcl p
	int n:=tclnopnds[opcode]

	p:=newtcl(opcode, n)

	if n then
		p.a:=a
		if n>=2 then
			p.b:=b
			if n=3 then
				p.c:=c
			fi
		fi
	fi

	if opcode=kmove and a.optype=temp_opnd then
		p.ndest:=1
	fi
end

export proc tc_gen4(int opcode, tclopnd a,b,c,d) =
	tcl p

	p:=newtcl(opcode, 4)

	p.a:=a
	p.b:=b
	p.c:=c
	p.abc[4]:=d
end

export func tc_gent(int opcode, tclopnd b, c=nil)tclopnd=
!any op that returns a single temp. One or two rvalue operands can be provided

	tcl p
	int n:=tclnopnds[opcode]

	if b then
		n:=2
		if c then
			n:=3
		fi
	fi

	p:=newtcl(opcode, n)
	p.a:=tc_gentemp()
	if n>=2 then
		p.b:=b
		if n=3 then
			p.c:=c
		fi
	fi

	if opcode=kmove then
		p.ndest:=1
	fi
	return p.a
end

export func tc_gent4(int opcode, tclopnd b,c,d)tclopnd =
!4-opnd instrs only occur with single temp result
	tcl p

	p:=newtcl(opcode, 4)

	p.a:=tc_gentemp()
	p.b:=b
	p.c:=c
	p.abc[4]:=d

	p.a
end

export func tc_gen_ix(int opcode, tclopnd b, c=nil, int scale=1, offset=0)tclopnd tx =
!for iloadx/addpx only
	
	IF B=NIL THEN B:=TC_GENINT(0) FI
	IF C=NIL THEN C:=TC_GENINT(0) FI
	tx:=tc_gent(opcode, b,c)

	tccurr.scale:=scale
	tccurr.extra:=offset
	tx
end

export proc tc_gen_ixs(int opcode, tclopnd a, b, c=nil, int scale=1, offset=0) =
!for istorex only

!	IF A=NIL THEN A:=GENINT(0) FI
	IF B=NIL THEN B:=TC_GENINT(0) FI
	IF C=NIL THEN C:=TC_GENINT(0) FI
!INT OLDOP:=PCCURR.OPCODE
	tc_gen(opcode, a,b,c)

	tccurr.scale:=scale
	tccurr.extra:=offset
end

export proc tc_gen_call(tclopnd fn, int nret, nargs)=
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

export proc tc_gen_cond(int opcode, cond, tclopnd a, b, c)=
	tc_gen(opcode, a, b, c)
	tccurr.cond:=cond
end

export func tc_genint(int a)tclopnd p=
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

export func tc_genreal(real x)tclopnd p=
	p:=newopnd()
	p.xvalue:=x
	p.optype:=real_opnd
	return p
end

export func tc_genr32(real x)tclopnd p=
	p:=newopnd()
	p.xvalue32:=x
	p.optype:=r32_opnd
	return p
end

export func tc_genstring(ichar s)tclopnd p=
	p:=newopnd()
	p.svalue:=pcm_copyheapstring(s)
!	p.svalue:=s
	p.optype:=string_opnd
	return p
end

export function tc_gendata(ref byte s, int length)tclopnd p=
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

export func tc_genlabel(int labelno)tclopnd p=
	p:=newopnd()
	p.labelno:=labelno
	p.optype:=label_opnd
	return p
end

export func tc_genmem(psymbol d)tclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=mem_opnd
	return p
end

export func tc_genmemaddr(psymbol d)tclopnd p=
	p:=newopnd()
	p.def:=d
	p.optype:=memaddr_opnd
	return p
end

export func tc_genname(ichar s)tclopnd p=
	return tc_genmem(tc_makesymbol(s, misc_id))
end

export func tc_gennameaddr(ichar s)tclopnd p=
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

!CPL "SETIMPORT", D.NAME

	if pimporttable=nil then
		pimporttable:=pimporttablex:=d
	else
		pimporttablex.next:=d
		pimporttablex:=d
	fi
end

export proc tc_comment(ichar s)=
!	return when fregoptim or fpeephole		!will get skipped anyway

	tc_gen(kcomment, tc_genstring(s))
end

export proc tc_currfunc(psymbol d)=
	currfunc:=d
end

export proc tc_addplib(ichar name)=
	if nplibfiles>=maxplibfile then perror("Too many libs") fi

!CPL "ADDPLIB",NAME

!	plibfiles[++nplibfiles]:=pcm_copyheapstring(name)
	plibfiles[++nplibfiles]:=pcm_copyheapstring(changeext(name,""))
end

export func tc_makeind(tclopnd a, int m, size=8)tclopnd p=
	tcl pold

	p:=newopnd()
	if a=nil then perror("MAKEIND A=0") fi
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
		p:=tc_gen_ix(kiloadx, a, tc_genint(0))
		checkaddpx(pold)

!		tc_gen2(kiload, p:=tc_gentemp(tpu64),a)

		tc_setmode(m, size)

	else
		perror("makeind?")
	esac

	return p
end

export func tc_makeindlv(tclopnd a, int m, size=8)tclopnd p=
	p:=newopnd()
	if a=nil then perror("MAKEIND A=0") fi

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

		tc_gen(kmove, p:=tc_gentemp(),a)
		tc_setmode(tpu64)
		p:=tc_makeindlv(p, m)

		return p

	else
		perror("makeindlv?")

	esac

	return p
end

export func tc_gentemp:tclopnd p=
	int n
	p:=newopnd()

!CPL "GENTEMP", STRMODE(M)

	n:=++ntemps

	p.tempno:=n
	p.optype:=temp_opnd

	return p
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

!CPL "WRITEASM"

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
!CPL "WRITEASM/file"
		if pverbose then println "Writing ASM", filename fi
!		println "Writing ASM", filename

!CPL "WASM:",$LINENO
		f:=fopen(filename,"w")
!CPL "WASM:",$LINENO
		gs_println(asmstr, f)
!CPL "WASM:",$LINENO
		fclose(f)

		gs_free(asmstr)
		nil
	else
!CPL "WRITEASM/STR"
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

export proc tcl_runtcl=
end

export proc tcl_writeobj(ichar filename)= end

!global proc tcl_genss= end
!global proc tcl_runtcl= end
!export proc tcl_writeobj(ichar filename)= end
!export proc tcl_writeexe(ichar filename)= end
!export proc tcl_writedll(ichar filename)= end
export proc tcl_writemx(ichar filename)= end
export proc tcl_writemcl= end
!global proc tcl_exec= end
export proc tcl_cmdskip(int a)=end

export proc tcl_exec(int run=1)=
!	pcmdskip:=cmdskip
	genmcl()
	genss()
	runlibfile("dummy", pcmdskip, run)
end

export proc checkaddpx(tcl p, int id=0)=
!addpx/loadpx has just been generated; see if it can be combined with previous addpx
!assume it looks like this:
! p  T1 := bp  + cp*sp + extrap        (p)
! q  T2 := T1 +  cq*sq + extraq        (q:=tccurr)
!For this to work, p must be addpx, q is addpx/loadpx, and cq must be int-opnd or
! be missing; T1 must only be used in these 2 ops

	tcl q:=tccurr

!CPL "CHECK1",TCLNAMES[P.OPCODE], =ID
	return unless freduce

	return unless p.opcode=kaddpx
	return unless q.c=nil or q.c.optype=int_opnd
	return unless p.a=q.b
	return unless q.islast.[2] = 1

	p.a:=q.a							!move T2 over to P

	if q.c then							!I think that .c is optional
		p.extra +:= q.c.value*q.scale
	fi
	p.extra +:= q.extra
	p.opcode := q.opcode				!move opcode in case loadpx

	tccurr:=p							!discard new tcl op
end

export proc checkaddpx_store(tcl p, int id=0)=
!storepx has just been generated; see if it can be combined with previous addpx
!assume it looks like this:
! p  T1 := bp  + cp*sp + extrap        (tccurr-1)
! q  (T1 +  bq*sq + extraq)^ := c       (tcurr)

!For this to work, p must be addpx, q is storepx, and bq must be int-opnd or
! be missing; T1 must only be used in these 2 ops

	return unless freduce

	tcl q:=tccurr

	return unless p.opcode=kaddpx
	return unless q.b=nil or q.b.optype=int_opnd
	return unless p.a=q.a
	return unless q.islast.[1]=1

	if q.b then							!I think that .c is optional
		p.extra +:= q.b.value*q.scale
	fi
	p.extra +:= q.extra
	p.opcode := kistorex				!addpx becomes storepx

	moveopnd(p, p, 1, 2)
	moveopnd(p, p, 2, 3)
	moveopnd(p, q, 3, 3)

	--tccurr							!discard new tcl op
end

proc moveopnd(tcl p, q, int a, b)=
	if q.abc[b]=nil then
		p.abc[a]:=tc_genint(0)
		p.islast.[a]:=0
		return
	fi

	p.abc[a]:=q.abc[b]

	if q.islast.[b] then
		p.islast.[a]:=1
	else
		p.islast.[a]:=0
	fi
	q.islast.[b]:=0
	dummy()
end

global proc dummy()=
end

export proc scanproctemps(psymbol d)=
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

		for i to p.nopnds do
			a:=p.abc[i]
			nextloop when a.optype<>temp_opnd
			temp:=a.tempno

!CPL =TEMP, TCLNAMES[P.OPCODE], =NDEST

			pt:=&templist[temp]
!CPL "LOOP", tclnames[p.opcode], =i, =temp,"//LAST:",,PT.LASTTCL,"LTC:",,PT.LTCOUNT,
!"RTC:",,PT.RTCOUNT,"IX:",,PT.INDEX

			plast:=pt.lasttcl

			if i<=ndest then				!ltemp
				if pt.rtcount then PERROR("SCANT1?") fi

				if plast=nil then			!first write
					if i=1 then
						p.ltmode:=1			!Set all 1st ltemps to 'Tm'
						p.firstlt:=1
!CPL "  FIRST WRITE"
					fi
				else						!subsequent write
					if i>1 then PERROR("SCANT3?") fi
					p.ltmode:=1
				fi

				++pt.ltcount
!CPL " ",=PT.LTCOUNT

			else							!rtemp
				if pt.ltcount=0 or plast=nil then PERROR("SCANT2?") fi

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

export proc reducetemps(psymbol d)=
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

	pm.size:=size
	pm.used:=oldtemp
	tempmap[oldtemp]:=newtemp:=nredtemps

finish:
	if p.islast.[n] then			!last used of it; free this combo
		tempmodes[newtemp].used:=0
	fi
end
=== tc_decls.m 0 0 16/32 ===

export type psymbol = ref pstrec

!export record pstrec = $caligned
export record pstrec =
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
!	byte isentry
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
				isentry:1)

!	byte scope
	byte nparams
	byte align					!for variables
	struct						!for params only
		byte paramcat				!tc_reg/move/stack/spill
		byte fromreg				!tc_reg: move from .fromreg to .reg
	end
!	byte maxtemp				!for procs set after tcl codegen

end

export type tcl = ref tclrec

export record tclrec =
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

export type tclopnd = ref opndrec

export record opndrec =
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
!		unit asmcode
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

global record fwdrec =
	ref fwdrec nextfwd
	i32 offset
	i16 reltype
	i16 seg
end

export byte tcldone, mcldone, ssdone, objdone, exedone

global [maxfixedtemp]temprec fixedtemplist
global [maxfixedtemp]byte fixedtempmap
global ref[]temprec templist			!points to above for small numbers of temps
global ref[]byte tempmap

!global const maxfixedtemp=256
global const maxfixedtemp=512
global const maxltemp=4					!as used by multiple func return

export int ntemps

global psymbol pstatictable, pstatictablex
global psymbol pproctable, pproctablex
global psymbol pimporttable, pimporttablex

global psymbol currprog
export psymbol currfunc
global psymbol blockretname
export psymbol entryproc		!entry point func

global const maxparam=100

global const maxplibfile=50
global [maxplibfile]ichar plibfiles
global [maxplibfile]u64 plibinst
global int nplibfiles

strbuffer sbuffer
global ref strbuffer pdest=&sbuffer
global int pcmdskip

EXPORT ICHAR $PMODULENAME

export [pmaxtuplesize]tclopnd extretopnds		!temps to hold func results
export [maxparam]tclopnd extparamopnds

global byte pcheckunusedlocals

global const pmaxtuplesize = 4
=== tc_diags.m 0 0 17/32 ===
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
const tab1="    "
!const tab2=tab1+tab1

const labprefix = "L"
!const labprefix = "_"

const tclindent = 1

!const fshowsymbols=1
const fshowsymbols=0

ref[]byte labeltab

[tclnames.bounds]int countS
[tclnames.bounds, PSTDNAMES.bounds]int types

global proc strtcl(tcl p, int inlinex=0)=
!inlinex=1 when generating inlinex comments for strmcl
	int opcode, nopnds
	tclopnd a,b,c
	int ntypes, defused

	const showformatted=1
	opcode:=p.opcode

!++COUNTS[OPCODE]
!++TYPES[OPCODE, P.MODE]

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

	IF NOT A OR NOT B THEN
		CPL "MOVE BAD OPNDS"
	FI
	IF P.MODE=TPVOID THEN
		CPL "MOVE NO MODE"
	FI



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
		if p.step>1 then
			psstr(" *")
			psint(p.step)
		fi

	when kloadincr, kloaddecr then
		psopnd(1)
		psassign()
		psopnd(2)
		psstr((opcode=kloadincr|"++"|"--"))
		if p.step>1 then
			psstr(" *")
			psint(p.step)
		fi

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

	if inlinex then
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
			PSMODE(P.MODE, P.SIZE)

		fi
		psstr(" ")
	else
		psstr("---")
	esac

	if inlinex then
		psstr(" ")
	else
		PSTABTO(56)
	fi

	GS_LEFTSTR(DEST,TCLNAMES[OPCODE],9)
!IF OPCODE=KMOVE AND A.OPTYPE=TEMP_OPND THEN PSSTR("MT") FI

!return when inlinex

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
!		return strint(p.value,"H")
!		return strint(p.value,"HU")

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
!		fprint @str,"L# ",p.labelno
		fprint @str,"## ", labprefix, p.labelno
!		fprint @str,"## ","#",p.labelno

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
!	psstr("L")
	psstr(labprefix)
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

!CPL "WRITEALL PCL"

	gs_str(dest,"PROC ")
	gs_strln(dest,caption)
	gs_strln(dest,"!DATA ------------------------------------------------------")

!GS_STRLN(DEST,"<WRITEPSTALLTCL TO BE REVISED>")

!CHECKCOMM("ALL1")

!GS_STRLN(DEST, "<TCL WRITE NOT READY>")
!
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

	gs_strln(dest,"!IMPORTS ------------------------------------------------------")
	d:=pimporttable

	while d, d:=d.next do
		currfunc:=d
!cpl "import",d.name, idnames[d.id]
!		if d.id=proc_id then
			psstr("Import ")
			psprocsig(d, "")

!			psstrline("End")
			psline()

!		fi
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
!
	pcm_free(labeltab, mlabelno)
!
!INT NN:=0, TT:=0
!FOR I IN COUNTS.BOUNDS WHEN COUNTS[I] DO
!	PRINT ++NN:"2",,":", TCLNAMES[I]:"10JL", COUNTS[I]:"5JR", $
!
!
!	FOR J IN PSTDNAMES.BOUNDS WHEN TYPES[I,J] DO
!		PRINT STRPMODE(J),,":",,TYPES[I,J],$
!	OD
!	CPL
!
!	TT+:=COUNTS[I]
!OD
!CPL
!CPL =TT
!CPL

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

proc psprocsig(psymbol d, ichar term=" =")=
	psymbol e
	byte comma:=0
	int lastmode:=tpvoid, m, lastsize, size

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
	psstrline(term)

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

!!		psmode(a.opmode, a.opsize)
!		psmode(p.mode, p.size)
		if p.next then psstr(", ") fi

		psstr(" ! ")
		psmode(p.mode, p.size)

!		psstr(" ! ")
!		psmode(a.opmode, a.opsize)

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

	if d.exported then psstr(" Exported") fi
	if d.imported then psstr(" Imported") fi
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

=== tc_tables.m 0 0 18/32 ===
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

export const tpref = tpu64

export enumdata [0:]ichar opndnames =
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

export enumdata [0:]ichar tclnames,
				[0:]byte tclwrite,			!1/2: known fixed no. of temp dests; 0=not used, or depends on context
				[0:]byte tclhastype,
				[0:]byte tcltempptr,		!1/2/3 means temp in opnd 1/2/both must be T^
				[0:]byte tclnopnds =		!number of operands (some like call/retmult are variable)

!TCL opcodes
! T	lvalue			T3
! M	lvalue			x, T (x is static/local/proc)
! P lvalue			x, T3^

! a b c d			Rvalues: T3, x, &x, 123 4.56 "ABC" L123
! D					Data Rvalue: x, &x, 123 4.56 "ABC" L123 <datastr>

! L Label index		Labels
! d symbol			M (ST entry)

!** means opcode needs a 4th tclopnd; this needs specialing handling for temps

!                    Wr Types T^  N          (a b c)
	(knop=0,	$+1,  0,  0,  0,  0),  !     (- - -)
	(kcomment,	$+1,  0,  0,  0,  1),  !     (a - -)
  
	(kmove,		$+1,  0,  1,  0,  2),  !     (M b -)	M := b
	(keval,		$+1,  0,  1,  0,  1),  !     (a - -)
  
	(kaddpx,	$+1,  1,  1,  0,  3),  ! s,n (T b c)	T := b + c*s + n
	(kiloadx,	$+1,  1,  1,  0,  3),  ! s,n (T b c)	T :=(b + c*s + n)^
	(kistorex,	$+1,  0,  1,  0,  3),  ! s,n (b c r)	(a + b*s + n)^ := c
  
	(kcall,		$+1,  0,  3,  0,  0),  ! r,n (a - -)	([T ...] F [r ...]) r=nret, n=nargs
	(kretproc,	$+1,  0,  0,  0,  0),  !     (- - -)	return
	(kretfn,	$+1,  0,  1,  0,  1),  !     (a - -)	return a
	(kretmult,	$+1,  0,  3,  0,  0),  ! n   (a ...)	return n values

	(kjump,		$+1,  0,  0,  0,  1),  !     (L - -)	goto L
	(kjumpcc,	$+1,  0,  1,  0,  3),  ! cc  (L b c)	goto L when b cc c
	(kjumpt,	$+1,  0,  1,  0,  2),  !     (L b -)	goto L when istrue(b)
	(kjumpf,	$+1,  0,  1,  0,  2),  !     (L b -)	goto L when not istrue(b)
	(kijump,	$+1,  0,  1,  0,  1),  !     (a - -)	goto a
	(ksetcc,	$+1,  1,  1,  0,  3),  ! cc  (T b c)	T := b cc c
  
	(kto,		$+1,  0,  1,  0,  2),  !     (L b -)	--b; goto L when b<>0
	(kforup,	$+1,  0,  1,  0,  3),  ! n   (L b c)	b+:=n; goto L when b <= c
	(kfordown,	$+1,  0,  1,  0,  3),  ! n   (L b c)	b-:=n; goto L when b >= c

	(kiswap,	$+1,  0,  1,  3,  2),  !     (P P -)	swap(P, P)
  
	(kadd,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b + c
	(ksub,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b - c
	(kmul,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b * c
	(kdiv,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b / c (float only)
	(kidiv,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b / c (int only; b % c)
	(kirem,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b irem c
	(kbitand,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b iand c
	(kbitor,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b ior c
	(kbitxor,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b ixor c
	(kshl,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b << c
	(kshr,		$+1,  1,  1,  0,  3),  !     (T b c)	T := b >> c
	(kmin,		$+1,  1,  1,  0,  3),  !     (T b c)	T := min(b, c)
	(kmax,		$+1,  1,  1,  0,  3),  !     (T b c)	T := max(b, c)
  
	(katan2,	$+1,  1,  1,  0,  3),  !     (T b c)	T := atan2(b, c)
	(kpower,	$+1,  1,  1,  0,  3),  !     (T b c)    T := b ** c
	(kfmod,		$+1,  1,  1,  0,  3),  !     (T b c)
  
	(ksubpx,	$+1,  1,  1,  0,  3),  ! s   (T b c)	T := b - c*s
	(ksubp,		$+1,  1,  1,  0,  3),  ! s   (T b c)	T := (b - c)/s

	(kneg,		$+1,  1,  1,  0,  2),  !     (T b -)	T := -b
	(kabs,		$+1,  1,  1,  0,  2),  !     (T b -)    T := abs b
	(kbitnot,	$+1,  1,  1,  0,  2),  !     (T b -)    T := inot b
	(knot,		$+1,  1,  1,  0,  2),  !     (T b -)    T := not b
	(ktoboolt,	$+1,  1,  2,  0,  2),  !     (T b -)    T := istrue b
	(ktoboolf,	$+1,  1,  2,  0,  2),  !     (T b -)    T := not istrue b
  
	(ksqr,		$+1,  1,  1,  0,  2),  !     (T b -)    T := sqr(b)
  
	(ksqrt,		$+1,  1,  1,  0,  2),  !     (T b -)	T := sqrt(b)
	(ksin,		$+1,  1,  1,  0,  2),  !     (T b -)	T := sin(b)
	(kcos,		$+1,  1,  1,  0,  2),  !     (T b -)	T := cos(b)
	(ktan,		$+1,  1,  1,  0,  2),  !     (T b -)	T := tan(b)
	(kasin,		$+1,  1,  1,  0,  2),  !     (T b -)	T := asin(b)
	(kacos,		$+1,  1,  1,  0,  2),  !     (T b -)	T := asin(b)
	(katan,		$+1,  1,  1,  0,  2),  !     (T b -)	T := atan(b)
 
	(klog,		$+1,  1,  1,  0,  2),  !     (T b -)	T := log(b)
	(klog10,	$+1,  1,  1,  0,  2),  !     (T b -)	T := log10(b)
	(kexp,		$+1,  1,  1,  0,  2),  !     (T b -)	T := exp(b)
	(kround,	$+1,  1,  1,  0,  2),  !     (T b -)	T := round(b)
	(kceil,		$+1,  1,  1,  0,  2),  !     (T b -)	T := ceil(b)
	(kfloor,	$+1,  1,  1,  0,  2),  !     (T b -)	T := floor(b)
	(kfract,	$+1,  1,  1,  0,  2),  !     (T b -)	T := fract(b)
	(ksign,		$+1,  1,  1,  0,  2),  !     (T b -)	T := sign(b)

	(kfloat,    $+1,  1,  2,  0,  2),  !     (T b -)	T := float(b)
	(kfix,		$+1,  1,  2,  0,  2),  !     (T b -)	T := fix(b)
	(ktruncate,	$+1,  1,  2,  0,  2),  !     (T b -)	T := u(b)
	(kfwiden,	$+1,  1,  2,  0,  2),  !     (T b -)	T := r64(b)
	(kfnarrow,	$+1,  1,  2,  0,  2),  !     (T b -)	T := r32(b)
	(kwiden,	$+1,  1,  2,  0,  2),  !     (T b -)	T := t(b)
  
	(ktypepun,	$+1,  1,  2,  0,  2),  !     (T b -)	T := t(u@(b))
  
	(kaddto,	$+1,  0,  1,  1,  2),  !     (P b -)    P +:= b
	(ksubto,	$+1,  0,  1,  1,  2),  !     (P b -)	P -:= b
	(kmulto,	$+1,  0,  1,  1,  2),  !     (P b -)	P *:= b
	(kdivto,	$+1,  0,  1,  1,  2),  !     (P b -)	P /:= b (float)
	(kidivto,	$+1,  0,  1,  1,  2),  !     (P b -)	P /:= b (int: %:= b)
	(kiremto,	$+1,  0,  1,  1,  2),  !     (P b -)	P irem:= b
	(kbitandto,	$+1,  0,  1,  1,  2),  !     (P b -)	P iand:= b
	(kbitorto,	$+1,  0,  1,  1,  2),  !     (P b -)	P ior:= b
	(kbitxorto,	$+1,  0,  1,  1,  2),  !     (P b -)	P ixor:= b
	(kshlto,	$+1,  0,  2,  1,  2),  !     (P b -)	P <<:= b
	(kshrto,	$+1,  0,  2,  1,  2),  !     (P b -)	P >>:= b
	(kminto,	$+1,  0,  1,  1,  2),  !     (P b -)	P min:= b
	(kmaxto,	$+1,  0,  1,  1,  2),  !     (P b -)	P max:= b
	(kaddpxto,	$+1,  0,  1,  1,  2),  ! s   (P b -)    P +:= b*s
	(ksubpxto,	$+1,  0,  1,  1,  2),  !     (P b -)    P -:= b*s
 
	(knegto,	$+1,  0,  1,  1,  1),  !     (P - -)    -:=P
	(kabsto,	$+1,  0,  1,  1,  1),  !     (P - -)    abs:=P
	(kbitnotto,	$+1,  0,  1,  1,  1),  !     (P - -)	inot:=P
	(knotto,	$+1,  0,  1,  1,  1),  !     (P - -)    not:=P
	(ktoboolto,	$+1,  0,  1,  1,  1),  !     (P - -)    istrue+:=P
  
	(kincrto,	$+1,  0,  1,  1,  1),  !     (P - -)	++P
	(kdecrto,	$+1,  0,  1,  1,  1),  !     (P - -)	--P
	(kincrload,	$+1,  1,  1,  2,  2),  !     (T P -)	T := ++P
	(kdecrload,	$+1,  1,  1,  2,  2),  !     (T P -)	T := --P
	(kloadincr,	$+1,  1,  1,  2,  2),  !     (T P -)	T := P++
	(kloaddecr,	$+1,  1,  1,  2,  2),  !     (T P -)	T := P--
  
	(kswitch,	$+1,  0,  1,  0,  3),  ! x,y (L L2 c)	switch on c; L=jumptable, L2=else label
	(kswitchu,	$+1,  0,  1,  0,  3),  ! x,y (L L2 c)	switch on c; L=jumptable, L2=else label; unchecked
	(kswlabel,	$+1,  0,  0,  0,  1),  !     (L - -)	label for switch jump table

	(kstop,		$+1,  0,  0,  0,  1),  !     (a - -)
	(klabel,	$+1,  0,  0,  0,  1),  !     (L - -)
  
	(kdata,		$+1,  0,  1,  0,  1),  !
  
	(kloadbit,	$+1,  1,  1,  0,  3),  !     (T b c)	T := b.[c]
	(kloadbf,	$+1,  1,  1,  0,  4),  !     (T b c d)	T := b.[c..d]
	(kstorebit,	$+1,  0,  1,  1,  3),  !	 (P b c)	P.[b] := c
	(kstorebf,	$+1,  0,  1,  1,  4),  !     (P b c d)	P.[b..c] := d
	(kidivrem,	$+1,  2,  1,  0,  4),  !     (T T c d)  (T1, T2) := C divrem d

	(kjumpin,	$+1,  0,  1,  0,  4),  !     (L b c d)  goto L when b in c..d
	(kjumpout,	$+1,  0,  1,  0,  4),  !     (L b c d)  goto L when b not in c..d

	(ksetjmp,	$+1,  0,  1,  0,  1),  !     (a - -)
	(klongjmp,	$+1,  0,  1,  0,  2),  !     (a b -)
	(kgetr0,	$+1,  1,  1,  0,  1),  !     (T - -)    get value of r0 put there by set/longjmp
	(kinitdswx,	$+1,  0,  0,  0,  0),  !     (T - -)    Mark next instr as setting up local jumptable
  
	(kclear,	$+1,  0,  1,  1,  1),  !     (P - -)    clear P
  
	(klast,		$+1,  0,  0,  0,  0),  !

end

export enumdata [0:]ichar ccnames, [0:]ichar ccshortnames =
	(no_cc=0,	"xx",	"?"),
	(eq_cc,		"eq",	" == "),
	(ne_cc,		"ne",	" != "),
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

export [tclnames.bounds]byte tclhascall			!has 1 when op may involve a call

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
=== mc_decls_x.m 0 0 19/32 ===
export type mclopnd = ref mclopndrec

export record mclopndrec =
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		psymbol def
		i64 value		!immediate value
		r64 xvalue	!immediate real value, mainly for dq
		ichar svalue	!immediate string
		int labelno
		int sysfn
		struct
			i32 tempno
			byte lasttemp		!set to 1 if .islast applied to the temp
		end
	end

	u16 misc: (			! bitfields
		size:5,			! one of 1 2 4 8
		scale:4,		! one of 1 2 4 8
		mode:3,			! R, X, imm, [mem]
		valtype:4)

	byte reg			!0, or main register
	byte regix			!0, or index register
	i32 offset			!additional offset to memory operands
end

export type mcl = ref mclrec

export record mclrec = !$caligned
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

export enumdata [0:]ichar valtypenames =
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
	(data_val,		$),		!data string
!	(syscall_val,	$),		!
end

export enumdata []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_procstart,		$,		0,		0),		!
	(m_procend,			$,		0,		0),		!
	(m_comment,			$,		0,		0),		!
!	(m_blank,			$,		0,		0),		!
!	(m_deleted,			$,		0,		0),		!
	(m_labelname,		$,		0,		0),		!
	(m_define,			$,		0,		0),		!
	(m_definereg,		$,		0,		0),		!
	(m_definetemp,		$,		0,		0),		!
	(m_trace,			$,		0,		0),		!
	(m_endx,			$,		0,		0),		!

	(m_label,			$,		1,		0),		!
	(m_nop,				$,		0,		0x90),		!
!	(m_param,			$,		1,		0),		!
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
	(m_jmpcc,			$,		1,		0),		!
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

	(m_and,				$,		2,		0x04),	!
	(m_or,				$,		2,		0x01),	!
	(m_xor,				$,		2,		0x06),	!
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
	(m_not,				$,		1,		2),		!

	(m_inc,				$,		1,		0),		!
	(m_dec,				$,		1,		1),		!

	(m_cbw,				$,		0,		0),	!
	(m_cwd,				$,		0,		0),	!
	(m_cdq,				$,		0,		0),		!
	(m_cqo,				$,		0,		0),		!
	(m_setcc,			$,		1,		0),		!

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
	(m_comisd,			$,		2,		0x2F),	!
	(m_ucomisd,			$,		2,		0x2E),	!

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
	(m_minsd,			$,		2,		0x5D),	!
	(m_maxss,			$,		2,		0x5F),	!
	(m_maxsd,			$,		2,		0x5F),	!

	(m_db,				$,		1,		0),		!
	(m_dw,				$,		1,		0),		!
	(m_dd,				$,		1,		0),		!
	(m_dq,				$,		1,		0),		!
	(m_ascii,			$,		1,		0),		!
	(m_asciiz,			$,		1,		0),		!
!	(m_ddoffset,		$,		1,		0),		!

!	(m_segment,			$,		1,		0),		!
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

	(m_xxxx,			$,		0,		0xF4),	!
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

export const rframe = r14
export const rstack = r15

export enumdata [0:]ichar condnames, [0:]ichar asmcondnames,
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
end

global enumdata [0:]ichar pmcnames =
	(pmc_ignore=0,	"Ignore"),
	(pmc_stack,		"Stack"),
	(pmc_reg,		"Reg"),
	(pmc_spill,		"Spill"),
	(pmc_move,		"Move"),
end	

global enumdata [0:]ichar locnames =
	(no_loc=0,	"unused"),				!temp not in use yet, or retired
	(reg_loc,	"reg"),					!temp in use in given register
	(mem_loc,	"mem"),					!temp in use but spilled to memory
	(mult_loc,	"mult"),				!multi-temp in used but not using reg or mem
end


!global [r0..r15]byte workregs, workxregs		!1 indicates available work regs
!global int nworkregs, nworkxregs				!no. workregs assigned
!global int nregvars, nxregvars					!no. reg vars allocated (consec regs)
!global int maxregvars, maxxregvars				!no. reg vars available
!
!global int xregmax
!

!global [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
!global [r0..r15]byte xregset		!same for xregs
!
!global [r0..r15]byte isregvar
!global [r0..r15]byte isxregvar
!
!global record pair =
!	u64 low, high
!end

!global pair regsetpr @ regset
!global pair isregvarpr @ isregvar
!global const u64 invertbytes = 0x0101'0101'0101'0101
!
global [r0..r15]byte usedregs		!1 means used during proc
global [r0..r15]byte usedxregs		!1 means used during proc

global byte noxorclear		!1 to suppress xor optimisation

!global macro zz = noperands
!global macro yy = noperands-1
!global macro xx = noperands-2
!global macro ww = noperands-3

!global const maxcalldepth=32
!global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
!global [maxcalldepth]byte callblockret	!1 if fnc returns a block
!global [maxcalldepth]u32 callblocksize	!size of any returned block
!global [maxcalldepth,4]u32 callargsize	!size of any block pushed in low args
!global int ncalldepth
!
global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

export ref mclrec mccode, mccodex		!genmc adds to this linked list

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

!global psymbol currasmproc

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

export record relocrec =			!informal version
	ref relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
export record dbuffer =
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

global const init_ss_symbols=32768				!exported to coff
global ref []psymbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref[]psymbol labeldeftable

global int aaseqno
global int aapos

!The following are highly dependent on the ordering of the base types being:
! r32 r64 ints... block ..., with r32 having value 1
!They assume mode is not void, and for ispfloat, is not a block

global macro ispwide(m)  = m - 1
global macro ispfloat(m) = m <= tpr64
global macro ispint(m)   = m > tpr64	!when block type is not expected

EXPORT [1..8]byte regmodes=(tpu8, tpu16, 0, tpu32, 0,0,0, tpu64)

global int pmode
global tcl currtcl

global ref mclrec mclprocentry
global ref mclrec mce_oldmccodex, mce_lastmcl, mce_nextmcl		!used by reset/setmclentry
global ref mclrec mcf_oldmccodex, mcf_lastmcl, mcf_nextmcl		!used by reset/setmclentry for frame setup

!global byte fpshortnames
global byte fpcheckunusedlocals
!export byte phighmem

global record riprec =
	ref riprec next
	u32 offset			!within code segment, offset of d32 field
	i32 immsize			!0,1,4 bytes of trailing imm field
end

!global record fwdrec =
!	ref fwdrec nextfwd
!	i32 offset
!	i16 reltype
!	i16 seg
!end

global ref riprec riplist

!export ref proc (ref void) idomcl_assem
!export ref func (ref void)int icheckasmlabel
!export ref func (int)psymbol igethostfn

global const maxblocktemps=50
global [maxblocktemps]psymbol blockdefs
global int nblocktemps

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)

global u64 workset						!has .[reg]=1 when in use as work reg or temp
global u64 tempset						!has .[reg]=1 when in use as temp

global int nextworkreg, nextworkxreg		!per instruction
global int highworkreg, highworkxreg		!per function

global [maxfixedtemp]byte temploc			!loc-code of temp T
global [maxfixedtemp]byte tempreg			!reg-code of temp T when in loc is reg_loc or mult
global [maxfixedtemp]byte tempspilled		!1 when temp is spilled
!global [maxfixedtemp]i32 tempoffset			!for basic codegen
!global [maxfixedtemp]byte tempmode			!
!global [maxfixedtemp]u32 tempsize			!

global ref[]byte ptemploc, ptempreg			!will point to above, or heap allocated versions

global [pstdnames.bounds]byte ploadopx

global [pstdnames.bounds]byte ploadop

proc start=
	for i in ploadop.bounds do ploadop[i]:=m_nop od

	ploadop[tpu8]:=ploadop[tpu16]:=ploadop[tpu32]:=m_movzx
	ploadop[tpi8]:=ploadop[tpi16]:=ploadop[tpi32]:=m_movsx
	ploadop[tpr32]:=m_movd
	ploadop[tpr64]:=m_movq
	ploadop[tpu64]:=ploadop[tpi64]:=m_mov
end

GLOBAL INT REGVARA
GLOBAL INT XREGVARA

GLOBAL INT WORKREGA
GLOBAL INT WORKREGB
GLOBAL INT WORKXREGA
GLOBAL INT WORKXREGB
=== mc_lib_x.m 0 0 20/32 ===
!const fuseregtable=1
!const fuseregtable=0

global const targetsize=8

export const ctarget=0

!global int mclseqno
EXPORT int mclseqno
EXPORT int NMCLOPND

global int mstackdepth

[-1..10]mclopnd smallinttable
[20]psymbol nametable
int nnametable

global macro isframex(d) = (d.id in [local_id, param_id])

export proc mclinit(int bypass=0)=
	mclopnd a
	int r, s

	if mclrec.bytes>64 then ABORTPROGRAM("MCLREC>64B") fi

	for r:=r0 to r15 do
		regtable[r, 1]:=mgenreg0(r, 1)
		regtable[r, 2]:=mgenreg0(r, 2)
		regtable[r, 4]:=mgenreg0(r, 4)
		regtable[r, 8]:=mgenreg0(r, 8)
	od

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=mgenreg(rframe, tpu64)
	dstackopnd:=mgenreg(rstack, tpu64)

	initmcdest()

	setsegment('C')

	lab_funcnametable:=0
	lab_funcaddrtable:=0

	for i in smallinttable.bounds do
		smallinttable[i]:=mgenint0(i, 8)
	od

!bypass is used when directly using mcl api (eg. from an external assembler)
!then genmcl(), called from pcl functions, is a no-op
	if bypass then
		mcldone:=1
	fi
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
	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_label then
		labno:=a.labelno

	when m_mov then				!change to movd/q if needed
		if a.mode=a_xreg or (b and b.mode=a_xreg) then
			m.opcode:=(a.size=8|m_movq|m_movd)
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

export proc genmc_cond(int opcode, cond, mclopnd a=nil, b=nil)=
	genmc(opcode, a, b)
	mccodex.cond:=cond
end

export proc genmc_label(int opcode, labelno)=
	genmc(opcode, mgenlabel(labelno))
end

global proc genmc_string(int opcode, ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenstring(s))
end

global proc genmc_def(int opcode,  psymbol d)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenmem(d))
end

global proc genmc_defaddr(int opcode,  psymbol d)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenmemaddr(d))
end

global proc genmc_int(int opcode,  int a)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenint(a))
end

global proc genmc_name(int opcode,  ichar name)=
!as genmc but uses a single immediate string operand
	genmc(opcode, mgenname(name))
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

EXPORT func mgenindex(int areg=0, ireg=0, scale=1, offset=0, size=0, labno=0, psymbol def=nil)mclopnd=
!construct a mem address mode
	mclopnd a
	a:=newmclopnd()

	a.mode:=a_mem
	a.reg:=areg

	if areg=rframe or ireg=rframe then usedregs[rframe]:=1 fi

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
			usedregs[rframe]:=1
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

export func mgenstring(ichar s, int length=-1)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm

	if length<0 then
		length:=strlen(s)
	fi

	a.svalue:=pcm_alloc(length+1)
	memcpy(a.svalue, s, length)
	(a.svalue+length)^:=0

	a.valtype:=stringimm_val
	a.size:=8
	return a
end

export func mgendata(ref byte p, int length)mclopnd=
	mclopnd a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstringn(p, length)

	a.valtype:=data_val
	a.size:=length
	return a
end

global func mgenname(ichar s)mclopnd=
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
		genmc(m_align, mgenint(align))
	fi
end

global func changeopndsize(mclopnd a, int size)mclopnd=
	mclopnd b

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

export func mgenint(i64 x, int mode=tpi64)mclopnd a=
	int size:=psize[mode]

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
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global func mgenint0(i64 x, int size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global func mgenrealmem(r64 x, int mode=tpr64)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_mem

	if ispwide(mode) then
		a.value:=getrealindex(x)
	else
		a.value:=getr32index(x)

	fi
	a.valtype:=label_val
	a.size:=psize[mode]
	return a
end

export func mgenrealimm(r64 x, int mode=tpr64)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=realimm_val
	a.size:=psize[mode]
	return a
end

EXPORT func mgenlabel(int x=0)mclopnd a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm

	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val
	a.size:=8

	return a
end

global func mgenlabelmem(int x)mclopnd a=
!x is a label index
!generate immediate operand containing label

	a:=mgenlabel(x)
	a.mode:=a_mem
	return a
end

export func mgenmem(psymbol d, int mode=tpu64)mclopnd a=
	int reg

	if d.reg then
		if pfloat[d.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d, mode)
		fi
	fi

	reg:=rnone
	if isframex(d) then
!		if not foptim and (int(d.offset) in -128..64) and ttsize[d.mode]=8 then
!			return frameregtable[d.offset]
!		fi

		reg:=rframe
		usedregs[rframe]:=1

	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	++d.nrefs
	a.valtype:=def_val

	if mode then
		a.size:=psize[mode]
	else
		a.size:=min(d.size, 8)
	fi
	if a.size=0 then a.size:=8 fi

	return a
end

EXPORT func mgenmemaddr(psymbol d)mclopnd=
	mclopnd a

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

global func mgenreg0(int reg, size=8)mclopnd a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size

IF SIZE=0 THEN MERROR("1:SIZE=0") FI
	return a
end

EXPORT func mgenxreg(int xreg, size=8)mclopnd=
	mclopnd a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
IF SIZE=0 THEN MERROR("2:SIZE=0") FI
	return a
end

EXPORT func mgenreg(int reg, mode=tpi64)mclopnd a =
!EXPORT func mgenreg(int reg, mode)mclopnd a =
	int size:=psize[mode]

!	if pfloat[mode] and reg>=xr0 then
	if reg>=xr0 then
		a:=newmclopnd()
		a.mode:=a_xreg
		a.reg:=reg
		a.size:=psize[mode]
		a
	else
		if size=0 then size:=8 fi
		usedregs[reg]:=1

!IF REG IN R10..R13 THEN REGSET[REG]:=1 FI

!		if fuseregtable then
!			return regtable[reg, size]
!		fi
		return mgenreg0(reg, size)
	fi
end

!global func mgenregi(int reg, mode=tpi64)mclopnd a =
!!	if fuseregtable then
!!		return regtable[reg, psize[mode]]
!!	fi
!	return mgenreg0(reg, psize[mode])
!end

global func mgenireg(int reg, mode=tpi64, offset=0)mclopnd=
	mclopnd a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=psize[mode]
	a.offset:=offset

	return a
end

global func mgentemp(int n, mode=tpu64)mclopnd a=
!pcl temps are used to spill pcl operands from a register
!they will always be 64 bits

!	int reg, size
!
!	if pcltempflags[n] then			!already in use
!		return changeopndsize(pcltempopnds[n], psize[mode])
!	fi
!
	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=rframe
!	usedregs[rframe]:=1
	a.valtype:=temp_val
	a.size:=psize[mode]
	a.tempno:=n
	a.offset:=currfunc.tempmodes[n].offset
!
!	pcltempopnds[n]:=a
!	pcltempflags[n]:=1

	return a
end

global func roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	if size iand 7=0 then return size fi
	return size+(8-(size iand 7))
end

global proc merroropnd(ichar mess, int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]", mess, opndnames[opndtype]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mmpos>>24], mmpos iand 16777215)
end

global func mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_label, mgenlabel(lab))
end

global func mgenextname(ichar s)mclopnd=
	[64]char str
	psymbol d
	static [20]psymbol table
	static int ntable

	strcpy(&.str, s)
	str[strlen(s)]:=0			!lose final *

	d:=findnamesym(str)

	if not d then
		d:=pcm_allocnfz(pstrec.bytes)

		d.name:=pcm_copyheapstring(&.str)
		d.id:=import_id
		d.imported:=1
		addnamesym(d)
	fi

	return mgenmemaddr(d)
end

global func mgenregvar(psymbol d, int mode)mclopnd a=
	a:=mgenreg(d.reg, mode)
!	isregvar[d.reg]:=1

	return a
end

global func mgenxregvar(psymbol d)mclopnd a=
	a:=mgenxreg(d.reg)
!	isxregvar[d.reg]:=1

	return a
end

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
		genmc(m_sub, dstackopnd, mgenint(n))
	fi
end

global proc popstack(int n)=
	if n then
		genmc(m_add, dstackopnd, mgenint(n))
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

global func newblocktemp(int size)psymbol=
	[16]char str
	psymbol d

	if nblocktemps>maxblocktemps then
		merror("Too many block temps")
	fi
	++nblocktemps

	fprint @str, "$B#", nblocktemps

MERROR("NEWBLOCKTEMP")

!	d:=pc_makesymbol(str, misc_id)
	d.mode:=tpblock
	d.size:=size
	d.used:=1
	d.id:=local_id
	d.nextlocal:=currfunc.nextlocal
!*! 	d.owner:=currfunc
	currfunc.nextlocal:=d

	blockdefs[nblocktemps]:=d
	d
end

global func findnamesym(ichar s)psymbol d=
!search for s in cache of named symbols

	for i to nnametable do
		if eqstring(s, nametable[i].name) then
			return nametable[i]
		fi
	od
	nil
end

global proc addnamesym(psymbol d)=
!add new name symbol, which should be unique

	if nnametable<nametable.len then
		nametable[++nnametable]:=d
	else
		merror("Ext nametab overflow")
	fi
end

export proc callproc(ichar cpname, name, int lineno)=
RETURN
end

func mgenstringx(ichar s)mclopnd=
	mgenlabelmem(getstringindex(s))
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
!		genmc(m_label, mgenlabel(kk0used))
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
	genmc(m_db,mgenint(a))
end

proc gendbstring(ichar s, int length)=
!string is printable, and doesn't include double quotes
	genmc(m_db,mgenstring(s,length))
end

global proc genrealtable=
	ref constrec p

	return unless creallist or cr32list

	mcomment("Real Table")
	setsegment('I',8)
	p:=creallist
	while p, p:=p.nextconst do
		genmc(m_label,mgenlabel(p.labelno))

		if p.xvalue=infinity then
			genmc(m_dq, mgenint(u64@(p.xvalue)))
		else
			genmc(m_dq, mgenrealimm(p.xvalue,tpr64))
		fi
	od

	mcomment("Real32 Table")
	p:=cr32list
	while p, p:=p.nextconst do
		genmc(m_label,mgenlabel(p.labelno))
		if p.xvalue=infinity then
			genmc(m_dd, mgenint(int@(r32(p.xvalue))))
		else
			genmc(m_dd, mgenrealimm(p.xvalue,tpr32))
		fi

	od
end

=== mc_asm_x.m 0 0 21/32 ===
!const fshowseq=1
const fshowseq=0

!const showsizes=1
const showsizes=0

!const showfreed=1
const showfreed=0

[r0..xr15]psymbol regvars		!nil, or strec when it uses that reg

export int assemtype='AA'

proc writemcl(int index, ref mclrec mcl)=
	if mcl.opcode=m_comment and mcl.a.svalue^='?' then
	else
		strmcl(mcl)
		gs_line(pdest)
	fi
end

global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mclopnd a, b
	int opcode, cond, sizepref
	psymbol d

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
		when stringimm_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")

		if d.exported then
			if eqstring(getbasename(d.name), d.name) then
				asmstr(":")
			else
				asmstr("\n`")
				asmstr(getbasename(d.name))
				asmstr("::")
			fi
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
		asmstr(getregname(d.reg, d.size))
		return

	when m_definetemp then
		asmstr("    ")
		asmstr(gettempname(currfunc, a.tempno))
		asmstr(" = ")
		asmint(a.offset)
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

	ipadstr(&.opcname, (opcode=m_dq|4|10), " ")
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

	str[1]:=0

	case a.mode
	when a_reg, a_xreg then
		return strreg(a.reg, a.size)

	when a_imm then
		if opcode=m_dq and a.valtype=intimm_val then
			if a.value in 0..9 then
				strcat(&.str, strint(a.value))
			else
				strcat(&.str, "0x")
				strcat(&.str, strword(a.value, "H"))
			fi
		else
			strcpy(&.str, strvalue(a))
		fi

	when a_mem then
		case a.valtype
		when intimm_val then
			strcpy(&.str, strint(a.value))
		when realimm_val then
			strcpy(&.str, strreal(a.xvalue))
		when realmem_val then
			fprint @&.str, "M#", a.xvalue
		esac

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
		println "BAD OPND", A.MODE
		return "<BAD OPND>"
	esac

	return &.str
end

global func strvalue(mclopnd a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
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

!STRCAT(STR, " ")
!STRCAT(STR, STRINT(INT(DEF), "H"))


	when intimm_val then
		strcat(&.str, strint(value))

	when realimm_val then
		print @&.str, a.xvalue:"20.20"

	when realmem_val then
		strcat(&.str, "M")
		strcat(&.str, strreal(a.xvalue))

	when stringimm_val then
		strcat(&.str, """")
		strcat(&.str, a.svalue)
		strcat(&.str, """")

	when name_val then
		strcat(&.str, a.svalue)

	when label_val then
		strcat(&.str, "L")
		strcat(&.str, strint(a.labelno))
		goto addoffset

	when temp_val then
		return gettempname(currfunc, a.tempno)

	else
		merror("Stropnd?")
	esac

	return &.str
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
		getstrint(reg-r0, &.str2)
		rs:=&.str2
	esac

	print @&.str, prefix[size2], ,rs
	return &.str
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

global func getdispname(psymbol d)ichar=
	static [256]char str

	if d.reg then
		fprint @str, "##R.#", (fpshortnames|""|"`"), (pfloat[d.mode]|"X"|""),
			 (fpshortnames|d.name|getfullname(d))
		return str
	fi

	if fpshortnames then
		return getbasename(d.name)
	else
		return getfullname(d, backtick:1)
	fi
end 

global func gettempname(psymbol d, int n)ichar=
	static [128]char str

	if fpshortnames or d=nil then
		print @str, "T", ,n
	else
		fprint @str, "#.$T#", getdispname(d), n
	fi
	str
end

global func strreg(int reg, size=8)ichar=
	psymbol d

	d:=regvars[reg]

	if d and psize[d.mode]=size then
		return getdispname(d)
	fi
	getregname(reg, size)
end

export func getassemstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d, e
	ref mclrec m
	[32]char str2, str3
	int i

	gs_init(pdest)
!
	case phighmem
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

	if a.mode=a_reg or a.mode=a_xreg or b.mode=a_reg or b.mode=a_xreg then
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

=== mc_gen_xb.m 0 0 22/32 ===
const fshowtcl=2
!const fshowtcl=1
!const fshowtcl=0

!!const fshowworkregs=1
!const fshowworkregs=0

!global const docalltrace=1
!global const docalltrace=0

GLOBAL INT DEBUG

!global int framebytes, frameoffset, paramoffset

[tclnames.bounds]ref proc(tcl) px_handlertable

[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
![6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

global int mretindex

global proc genmcl(ichar dummy=nil)=

	return when mcldone

!CPL "DOING GENMCL2"

	IF FSHOWTCL THEN CPL "********* ASM HAS TCL INFO *********" FI

!CPL =CURRFUNC

	int tt:=os_clock()

	inithandlers()
	mclinit()
!CPL $LINENO

	mcomment("X64 CODE...")


!CPL $LINENO
	do_statics()
!CPL $LINENO

	mcomment("")
!CPL $LINENO

	do_procs()
!CPL $LINENO

!CPL "Done ConvertTCL"

	genrealtable()
!CPL $LINENO
	genabsneg()
	genstringtable()
!CPL $LINENO

	genmc(m_endx)					!need as buffer in optimiser
	genmc(m_endx)
!CPL $LINENO

	mcldone:=1

	mcltime:=os_clock()-tt
!CPL $LINENO

end

!FUNC CHECKFPUSED(MCLOPND A)int=
!	RETURN 0 WHEN A=NIL
!	if a.reg=rframe or a.regix=rframe then return 1 fi
!	0
!END
!
proc converttcl(tcl p)=

!RETURN WHEN P.OPCODE IN [KCOMMENT]
!IF P.OPCODE NOT IN [KCOMMENT, KLABEL] THEN CPL "    CONV", TCLNAMES[P.OPCODE], STRWORKREGS()FI

	doshowtcl(p) when fshowtcl

	pmode:=p.mode
	currtcl:=p
	mmpos:=p.pos

	pcseqno:=p.seqno

!	IF P.OPCODE NOT IN [KCOMMENT, KLABEL] THEN
!		MCOMMENT("CLEAR REGS")
!		CLEARREGX(R1)
!		FOR R IN R0..R13 WHEN R NOT IN R3..R9 DO
!!			GENMC(M_XOR, MGENREG(R, TPU32), MGENREG(R, TPU32))
!		OD
!	FI


	px_handlertable[p.opcode]^(p)

!	MCOMMENT("CLEAR REG")
!	CLEARREGX(R0, P.OPCODE)
!	CLEARREGX(R1, P.OPCODE)
!	CLEARREGX(R2, P.OPCODE)
!!	CLEARREGX(R9, P.OPCODE)
!	CLEARREGX(R10, P.OPCODE)
!	CLEARREGX(R11, P.OPCODE)
!	CLEARREGX(R12, P.OPCODE)
!	CLEARREGX(R13, P.OPCODE)
!
	nextworkreg:=r0				!reset
	nextworkxreg:=xr4
end

PROC CLEARREGX(INT R, OPCODE)=
	RETURN WHEN OPCODE IN [KCOMMENT, KLABEL, KDATA]
	RETURN WHEN OPCODE IN [KSWLABEL]

	GENMC(M_XOR, MGENREG(R, TPU32), MGENREG(R, TPU32))
END

proc inithandlers=
	static byte initdone=0
	ichar name, s
	int n

	if initdone then return fi

	n:=$getnprocs()

	for i to n do
		name:=$getprocname(i)
		if eqbytes(name, "tx_", 3) then
			for k in tclnames.bounds do
				s:=tclnames[k]
				if s^='k' then ++s fi				!some are kload, others just store
				if eqstring(s, name+3) then
					px_handlertable[k]:=$getprocaddr(i)
					exit
				fi
			else
				merror("Invalid handler name:", name)
			od
		fi
	od

	static [, 2]byte dupltable = (
!!mapping this op   =>  uses same handler as:
		(ktoboolf, 		ktoboolt)
		(kfordown, 		kforup)
		(kdecrto, 		kincrto)
		(kdecrload,		kincrload)
		(kloaddecr,		kloadincr)
		(kswitchu,		kswitch)
!		(kicallp, 		kcallp)
!		(kicallf, 		kcallp)
!
!		(kendmx, 		kresetmx)
!		(ktcproc, 		kproc)
!
!		(kidivto, 		kidiv)
!		(kiremto, 		kirem)
		)


	for i to dupltable.len do
		px_handlertable[dupltable[i, 1]]:=px_handlertable[dupltable[i, 2]]
	end

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		fi
	od

	initdone:=1
end

proc do_statics=
	psymbol p, d

	mcomment("STATICS")

	d:=pstatictable

	while d, d:=d.next do
		do_staticvar(d)
	od

	p:=pproctable

	while p, p:=p.next do

		d:=p.nextstatic

		while d, d:=d.nextstatic do
!CPL "LOCALS", P.NAME, D.NAME
			do_staticvar(d)
		od
	od

end

proc do_staticvar(psymbol d)=
	int size:=d.size
	tcl p

!	setsegment((d.code|'I'|'Z'), getalignment(d.mode))
	setsegment((d.code|'I'|'Z'), d.align)
!	genmc_name(m_labelname, d.name)
	genmc_def(m_labelname, d)

!	if d.atvar then
!		return
!	elsif d.code then
	if d.code then
		p:=d.code
		while p, p:=p.next do
!			do_staticdata(p)
!			do_staticdata(p.a)
!			do_staticdata(p.a, P)
			do_staticdata(p.a, P, D)
		od
	else
		genmc_int(m_resb, d.size)
	fi


	mcomment("")
end

proc do_staticdata(tclopnd a, TCL P, PSYMBOL D)=
	static [1..8]byte ops= (m_db, m_dw, 0, m_dd, 0, 0, 0, m_dq)
	[256]CHAR STR

!	FPRINT @str, "DATA: Opnd:(#) D:# P:# A:#  Value:#=", OPNDNAMES[a.optype], 
!		STRPMODE(D.MODE, D.SIZE), 
!		STRPMODE(P.MODE, P.SIZE), 
!		 strpmode(a.mode, a.size), 
!			A.VALUE
!
!!CPL "--DATA:", D.NAME, ":", "D:", STRPMODE(D.MODE, D.SIZE), "P:", STRPMODE(P.MODE, P.SIZE), 
!!	"A:",   STRPMODE(A.MODE, A.SIZE)
!
!	mcomment(str)

!	if p.mode=tpblock then
!		do_blockdata(p)
!		return
!	fi

	case a.optype
	when int_opnd then
		genmc_int(ops[p.size], a.value)
	when real_opnd then
		genmc_int(ops[p.size], u64@(a.xvalue))
	when r32_opnd then
		genmc_int(ops[p.size], u32@(a.xvalue32))

	when string_opnd then
		genmc_label(m_dq, getstringindex(a.svalue))

	when data_opnd then
		do_blockdata(p)
		return
!		genmc(m_ascii)
!		mgendata(a.svalue, p.size)
!
	when memaddr_opnd then
		genmc_defaddr(m_dq, a.def)

	when label_opnd then
		genmc_label(m_dq, a.labelno)

	else
		mcomm("Opnd not ready:", opndnames[a.optype])
	esac
	psline()

end

proc do_blockdata(tcl p) =
	ref byte s
	ref u64 d
	ref byte db@d
	int n,nqwords,nwords,r
	tclopnd a:=p.a

MCOMMENT("BLOCK DATA")

	n:=p.size
	return when n=0

	nwords:=n/8

!	IF P.MODE=TPBLOCK THEN
		d:=cast(a.svalue)
!	ELSE					!1/2/4/8-byte blocks may have u8-64 types
!CPL "POINT TO .VALUE"
!MCOMMENT("BLOCKDATA WITH INT TYPE")
!		d:=cast(&a.value)
!	fi

	to nwords do
		genmc(m_dq, mgenint(d++^))
	od

	r:=n-nwords*8
	to r do
		genmc(m_db, mgenint(db++^))
	od
!	if r then
!
!		genstring_db(cast(d), r, 'B')
!	fi
	MCOMMENT("ENDDATA")
end

proc do_procs=
	psymbol p

	mcomment("FUNCTIONS")

	p:=pproctable

	while p, p:=p.next do

		do_procdef(p)

	od

end

proc do_procdef(psymbol p)=
	tcl pc


	currfunc:=p


	mretindex:=p.retindex
	if mretindex=0 then mretindex:=++mlabelno fi

	setsegment('C', 1)
!	mcomment(" ------------- Proc:", p.name)
	genmc_def(m_procstart, p)
	genmc_def(m_labelname, p)
!	genmc_name(m_labelname, p.name)
!	if p.ismain then
!		genmc_name(m_labelname, "main")
!	fi

	ntemps:=p.maxtemp

	if ntemps<=maxfixedtemp then
		ptemploc:=&temploc
		ptempreg:=&tempreg
		memset(ptemploc, 0, ntemps)
		memset(ptempreg, 0, ntemps)
		memset(&tempspilled, 0, ntemps)

	else
		ptemploc:=pcm_allocz(ntemps)
		ptempreg:=pcm_allocz(ntemps)
	fi

	do_proccode_a()

	mcomment("?>>")
	mclprocentry:=mccodex

	if p.isentry and p.nparams=2 then
		fixmain()
	fi

	pc:=p.code

	while pc, pc:=pc.next do
!		mcomment(" do next tcl code:", tclnames[pc.opcode])
		converttcl(pc)
	od

	if mclprocentry=mccodex then		!empty body: add dummy mcl op
		mcomment("---")					!injection of entry code goes wrong otherwise
	fi

!CPL $LINENO
	do_proccode_b()
!CPL $LINENO
	do_proccode_c()
!CPL $LINENO

	if workset or tempset then
		CPL("WORK/TEMPSET NOT EMPTY")
!		MERROR("WORK/TEMPSET NOT EMPTY")
		MCOMMENT("WORK/TEMPSET NOT EMPTY")
	fi

!	mcomment("End", p.name)
	genmc(m_procend)
!CPL $LINENO
	mcomment("")

	if ntemps>maxfixedtemp then
		pcm_free(ptemploc, ntemps)
		pcm_free(ptempreg, ntemps)
		ptemploc:=&temploc
		ptempreg:=&tempreg
	fi
	currfunc:=nil
!CPL $LINENO
end

global proc start=
	ptemploc:=&temploc
	ptempreg:=&tempreg
end

global proc unimpl(tcl p)=
	[100]char str

	fprint @str, "Unimpl: # (#)", tclnames[p.opcode], strpmode(pmode, p.size)
!	fprint @str, "Unimpl: # (#)", tclnames[p.opcode]

	CPL STR

	mcomment(pcm_copyheapstring(str))
end

proc doshowtcl(tcl p)=
	[1256]char str

	return unless fshowtcl


	case p.opcode
!	when kretproc, kretfn then
	when kcomment, klabel, kswlabel then
!	when klabel then
	else
		IF FSHOWTCL=2 THEN MCOMMENT("") FI
!		strcpy(&.str, "                       ")
		strcpy(&.str, "-"*24)
		strcat(&.str, strtclstr(p))
		mcomment(pcm_copyheapstring(&.str))
		IF FSHOWTCL=2 THEN MCOMMENT("") FI
	esac
end

global proc fixmain=
!d is a main func with 2 params
!convert params to locald, add more locals needed for calling __getmainargs
	psymbol d:=currfunc, e
	psymbol dn, dargs, denv, dinfo
	mclopnd ax

	dn:=d.nextparam
	dargs:=dn.nextparam

!add 2 new locals
!	denv:=tc_makesymbol("$env", local_id)
	denv:=tc_makesymbol("$env", static_id)
	denv.mode:=tpref
	denv.size:=8

!	dinfo:=tc_makesymbol("$info", local_id)
	dinfo:=tc_makesymbol("$info", static_id)
	dinfo.mode:=tpblock
	dinfo.size:=128


	setsegment('Z',8)
	genmc(m_label, mgenmemaddr(dinfo))
	genmc(m_resb, mgenint(128))
	genmc(m_label, mgenmemaddr(denv))
	genmc(m_resb, mgenint(8))
	setsegment('C',1)
	tc_addlocal(denv)
	tc_addlocal(dinfo)

!remove dn/dargs as params

	dn.nextparam:=dargs.nextparam:=d.nextparam:=nil
	d.nparams:=0
	dn.id:=local_id
	dn.used:=1
	dargs.id:=local_id
	dargs.used:=local_id
!
!add them as locals

	tc_addlocal(dargs)
	tc_addlocal(dn)

	genmc(m_push, ax:=mgenreg(r0))
	genmc(m_lea , ax, mgenmem(dinfo))
DINFO.ADDROF:=1
	genmc(m_push, ax)
	genmc(m_sub, dstackopnd, mgenint(32))
	genmc(m_lea,  mgenreg(r10), mgenmem(dn))
DN.ADDROF:=1
	genmc(m_lea,  mgenreg(r11), mgenmem(dargs))
DARGS.ADDROF:=1
	genmc(m_lea,  mgenreg(r12), mgenmem(denv))
DENV.ADDROF:=1
	clearreg(mgenreg(r13))
	genmc(m_call, mgenextname("__getmainargs*"))
!
	genmc(m_sub, dstackopnd, mgenint(48))

!do pcmdskip fixes
	if pcmdskip then
		genmc(m_sub, mgenmem(dn), mgenint(pcmdskip, tpi32))
		genmc(m_add, mgenmem(dargs), mgenint(pcmdskip*8))
	fi

end
=== mc_aux_xb.m 0 0 23/32 ===
!ref mclrec mclframesetup

global int nsaveregs, nsavefregs		!number of integer/float non-vols to be saved
global int nspilled						!spilled int/float registers

global int framesize					!counts bytes in stack frame
!global int framebytes, frameoffset, paramoffset
global int paramstackoffset
global int paramspilloffset

psymbol dblockarg

int nsavedregs, nsavedxregs

global proc do_proccode_a=
	nextworkreg:=r0						!first 3 are volatile
	nextworkxreg:=xr4					!first 2 are volatile

	highworkreg:=nextworkreg			!assume workreg will be used
	highworkxreg:=nextworkxreg

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
!*!	mclopnd ax
	psymbol d
	[100]char str, newname
	int r, n

!CPL $LINENO

	if currfunc.tempmodes=nil then merror("Needs tempmodes") fi

	setmclentry(mclprocentry)

	framesize:=0
	dblockarg:=nil

!NEXTWORKREG:=R4
!NEXTWORKXREG:=XR7

	nsavedregs:=max(highworkreg-r2, 0)
	nsavedxregs:=max(highworkxreg-xr5, 0)
	nsavedbytes:=(nsavedregs+nsavedxregs)*8

!CPL $LINENO
!CPL CURRFUNC.NAME,"HIGHWORKREG=", STRREG(HIGHWORKREG)
!CPL CURRFUNC.NAME,"HIGHWORKXREG=", STRREG(HIGHWORKXREG)


!allocate offsets to args, and set defines

!CPL "PROC B", =CURRFUNC.NAME, =CURRFUNC.NPARAMS, STRPMODE(CURRFUNC.MODE, CURRFUNC.SIZE)

	if currfunc.mode=tpblock then	!need to inject extra parameter

		dblockarg:=tc_makesymbol("$block", param_id)
		dblockarg.nextparam:=currfunc.nextparam
		dblockarg.mode:=tpblock
		dblockarg.size:=currfunc.size

		currfunc.nextparam:=dblockarg
		++currfunc.nparams
	fi
!CPL "PROC B2", =CURRFUNC.NAME, =CURRFUNC.NPARAMS, STRPMODE(CURRFUNC.MODE, CURRFUNC.SIZE)

!IF NSAVEDBYTES THEN
!	CPL "Saving:", nsavedregs, nsavedxregs,"in", currfunc.name
!FI

	d:=currfunc.nextparam
	paramoffset:=16+nsavedbytes		!between top of stackframe and 1st param is fp/retaddr/saved

	while d, d:=d.nextparam do
		d.offset:=paramoffset
!CPL "SET DOFFSET", D.NAME, PARAMOFFSET
		paramoffset+:=8
!		if d.used then
			genmc_def(m_define, d)
!		elsif pcheckunusedlocals then
!			println "Unused param:", d.name, "in", currfunc.name
!		fi

	od
!CPL $LINENO


!allocate offsets to locals, and set defines
	d:=currfunc.nextlocal
	while d, d:=d.nextlocal do
		if not d.used then
			if pcheckunusedlocals then
				println "Unused local:", d.name, "in", currfunc.name
			fi
			nextloop
		fi

		size:=psize[d.mode]
		if d.mode=tpblock then
			size:=d.size
		fi

		if d.atvar then
			MERROR("PCODEB/@")
        else
			framesize+:=roundsizetg(size)
			d.offset:=-framesize
			genmc_def(m_define, d)
		fi
	od

!CPL "FS AFTER LOCALS", FRAMESIZE

!allocate offsets to temps, and set defines
	ntemps:=currfunc.maxtemp
	if ntemps>maxfixedtemp then merror("Too many temp") fi

	for i to ntemps do
!CPL I,STRPMODE(CURRFUNC.TEMPMODES[I].MODE),"//"
!CPL I,CURRFUNC.TEMPMODES[i].MODE,"//", =CURRFUNC.TEMPMODES[I].SIZE
		framesize+:=roundsizetg(currfunc.tempmodes[i].size)

		genmc(m_definetemp, mgentemp(i))
!CPL =FRAMESIZE
		mccodex.a.offset:=-framesize
		currfunc.tempmodes[i].offset:=-framesize
!		if -framesize < i16.min then merror("Temp offset outside > 16 bits") fi
		if -framesize < -8000000 then merror("Temp offset outside > 23 bits") fi
	od

!CPL "FS AFTER TEMPS", FRAMESIZE


	framesize+:=32									!shadow space
!CPL "FS AFTER SHADOW SPACE", FRAMESIZE

	if (framesize+nsavedbytes) iand 8 then			!keep stack frame 16-byte aligned
		framesize+:=8
	end

	savevolregs(nsavedregs, nsavedxregs)

!generate stack entry code proper:

	genmc(m_push, dframeopnd)
	genmc(m_mov, dframeopnd, dstackopnd)
	pushstack(framesize)

!spill any args to shadow space
	spillparams()

!	MCOMM("="*40)
	resetmclentry()
end

global proc do_proccode_c=
	int offset
	mclopnd ax, bx

!	MCOMM("="*40)

	genmc(m_label, mgenlabel(mretindex))

	if dblockarg then
!		MCOMMENT("BLOCK RETURN COPY NEEDED")
!D0 has address of block to be copied
!It needs to be returned on D0 after copying
MCOMMENT("BLOCKRET1")

		ax:=mgenireg(r0)
		bx:=mgenreg(r1)
		genmc(m_mov, bx, mgenmem(dblockarg))
		nextworkreg:=r2
		copyblock(mgenireg(r1), ax, dblockarg.size)		!does not affect r0
		genmc(m_xchg,  mgenreg(r0), bx)

MCOMMENT("BLOCKRET2")

	fi

	popstack(framesize)
	genmc(m_pop, dframeopnd)
	restorevolregs(nsavedregs, nsavedxregs)

	genmc(m_ret)
end

proc spillparams=
	psymbol d
	mclopnd ax
	int offset:=16, regoffset:=0, xregoffset, firstoffset

	regoffset:=0

	d:=currfunc.nextparam

	if currfunc.variadic then				!C proc def using ...
		firstoffset:=d.offset				!param offsets may be pushed up

		for i:=currfunc.nparams to 3 do				!0-based; if nparams=2, loops over 2..3 as 0..1 are normal
			ax:=mgenindex(areg:rframe, size:8, offset:i*8+firstoffset)
			genmc(m_mov, ax, mgenreg(i+r10))
		od
	fi

	while d, d:=d.nextparam do
		if regoffset>3 then exit fi

		if d.used or regoffset=0 then
			ax:=mgenindex(areg:rframe, size:8, offset:d.offset)
			case d.mode
			when tpr64 then
				genmc(m_movq, ax, mgenxreg(regoffset+xr0))
			when tpr32 then
				genmc(m_movd, changeopndsize(ax,4), mgenxreg(regoffset+xr0))
			else
				genmc(m_mov, ax, mgenreg(regoffset+r10))
			esac
		fi

		offset+:=8
		++regoffset
	od

end

proc savevolregs(int nregs, nxregs)=
	int reg
	mclopnd ax

	reg:=r3
	to nregs do
		genmc(m_push, mgenreg(reg++))
	od

	reg:=xr6
	ax:=mgenreg(r0)
	to nxregs do
		genmc(m_movq, ax, mgenreg(reg++))
		genmc(m_push, ax)
	od
end

proc restorevolregs(int nregs, nxregs)=
	int reg
	mclopnd ax

	reg:=xr6+nxregs
	ax:=mgenreg(r13)

	to nxregs do
		genmc(m_pop, ax)
		genmc(m_movq, mgenreg(--reg), ax)
	od
	reg:=r3+nregs
	to nregs do
		genmc(m_pop, mgenreg(--reg))
	od

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

!global proc do_blockdata(tcl p) =
!	ref byte s
!	ref u64 d
!	int n, nqwords, nwords, r
!
!	n:=p.size
!	return when n=0
!
!	nwords:=n/8
!
!	d:=cast(p.svalue)
!	to nwords do
!		genmc_int(m_dq, d++^)
!	od
!
!	r:=n-nwords*8
!!CPL "DOBLOCKDATA", =N, =NWORDS, =R
!	s:=cast(d)
!	for i to r do
!		genmc_int(m_db, s++^)
!	od
!!
!!	if r then
!!!CPL "BLOCK/END"
!!		genmc_string(m_ascii), s)
!!!		genstring_db(cast(d), r, 0)
!!	fi
!!	MGENCOMMENT("ENDDATA")
!
!end

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

func checkisleaf(tcl p, int &maxargs)int isleaf=
	isleaf:=1
	maxargs:=0

	while p, p:=p.next do
		if tclhascall[p.opcode] then
			isleaf:=0

			if p.opcode = kcall then
				maxargs max:=p.nargs
			else						!other op that may use call
				maxargs max:=2			!just assume 2
			fi

		fi
	od

	return isleaf
end

global proc clearblock(mclopnd ax, int n)=
!ax will always be D0 containing the address of the block that is to be cleared
	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg

!	ax.mode:=a_mem			!turn into [D0]
	ax:=mgenireg(ax.reg)

	oddbytes:=n rem 8		!will be zero, or 1..7

	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=mgenreg(getworkireg())
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

			rcount:=mgenreg(countreg:=getworkireg())
			lab:=++mlabelno

!			ax:=makesimpleaddr(ax)

			genmc(m_mov, rcount, mgenint(nwords))
			genmc(m_label, mgenlabel(lab))
			genmc(m_mov, ax, rx)

			genmc(m_add, mgenreg(ax.reg), mgenint(8))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, mgenlabel(lab))

			offset:=0
		else
			rcount:=mgenreg(countreg:=getworkireg())
			lab:=++mlabelno

!			ax:=makesimpleaddr(ax)
			genmc(m_mov, rcount, mgenint(nwords/4))
			genmc(m_label, mgenlabel(lab))

			for i to 4 do
				genmc(m_mov, applyoffset(ax, offset), rx)
				offset+:=8
			od

			genmc(m_add, mgenreg(ax.reg), mgenint(targetsize*4))

			genmc(m_dec, rcount)
			genmc_cond(m_jmpcc, ne_cond, mgenlabel(lab))

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

global proc copyblock(mclopnd ax,bx, int n)=
!ax, bx refer to memory; do ax:=bx for n bytes

	mclopnd rx, rcount
	int nwords, lab, oddbytes, offset, workreg, countreg, axreg

	if n=16 then
		rx:=mgenreg(getworkxreg())
		genmc(m_movdqu, rx, bx)
		genmc(m_movdqu, ax, rx)
		return
	fi

	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of u64s (ie. octobytes)

	rx:=mgenreg(getworkireg())		!work reg

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
		rcount:=mgenreg(getworkireg())
		lab:=++mlabelno

!		ax:=makesimpleaddr(ax)
!		bx:=makesimpleaddr(bx)
		ax.size:=8

		genmc(m_mov, rcount, mgenint(nwords))
		genmc(m_label, mgenlabel(lab))
		genmc(m_mov, rx, bx)
		genmc(m_mov, ax, rx)

		genmc(m_add, mgenreg(ax.reg), mgenint(targetsize))
		genmc(m_add, mgenreg(bx.reg), mgenint(targetsize))

		genmc(m_dec, rcount)
		genmc_cond(m_jmpcc, ne_cond, mgenlabel(lab))

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

global func gethostfn(int opc)psymbol d =
	ichar name, namec

!try manual seach through pcl code
	case opc
	when kpower then
		name:="msys.m$power_i64"		!msys or msysc
		namec:="msysc.m$power_i64"
	else
		name:=nil
	esac

	if name then
		psymbol ps:=pproctable
		while ps, ps:=ps.next do
			if eqstring(name, ps.name) or eqstring(namec, ps.name) then
				return ps
			fi
		od
	fi

	merror("gethostfn?", tclnames[opc])
	nil
end

=== mc_conv_xb.m 0 0 24/32 ===
!convert tcl to mcl cond codes
!order is in eq ne lt le ge gt
[6]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[6]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

proc tx_nop*(tcl p) =
	unimpl(p)
end

proc tx_comment*(tcl p) =
	mcomment(p.a.svalue)
!	mcomment("<COMMENT>")
end

proc tx_move*(tcl p) =	! M := b
	mclopnd ax

	ax:=loadopnd(p.b)
	storeopnd(p.a, ax)
end

proc tx_eval*(tcl p) =
	loadopnd(p.a)
end

proc tx_iloadx*(tcl p) =	! T :=(b + c*s + n)^
	mclopnd ax, bx, cx, px
	tclopnd c:=p.c
	int scale:=p.scale, offset:=p.extra

	bx:=loadopnd(p.b, tpu64)

	if scale=0 then merror("ILOADPX SCALE=0") fi

	unless c.optype=int_opnd and c.value=0 then
		cx:=loadopnd(c, tpi64)
		if scale not in [1,2,4,8] then
			genmc(m_imul2, cx, mgenint(scale))
			scale:=1
		fi
!		px:=mgenindex(areg:bx.reg, ireg:cx.reg, scale:scale, offset:offset, size:p.size)
		px:=mgenindex(areg:bx.reg, ireg:cx.reg, scale:scale, offset:offset)
	else
		px:=mgenireg(bx.reg, pmode, offset)
	end	

	if pmode=tpblock then
		if px.reg and px.regix=rnone then
			ax:=mgenreg(px.reg)
		else
			ax:=mgenreg(getworkireg())
			genmc(m_lea, ax, px)
		fi
	else
		ax:=mgenreg(getworkreg(pmode), pmode)
		genmc(m_mov, ax, px)
	fi

	storeopnd(p.a, ax)
end

proc tx_istorex*(tcl p) =	! (a + b*s + n)^ := c
	mclopnd ax, bx, cx, px
	tclopnd b:=p.b
	int scale:=p.scale, offset:=p.extra

	ax:=loadopnd(p.a, tpu64)

	if scale=0 then merror("ILOADPX SCALE=0") fi

	unless b.optype=int_opnd and b.value=0 then
		bx:=loadopnd(b, tpi64)
		if scale not in [1,2,4,8] then
			genmc(m_imul2, bx, mgenint(scale))
			scale:=1
		fi
		px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:offset, size:p.size)
	else
		px:=mgenireg(ax.reg, pmode, offset)
	end	

	cx:=loadopnd(p.c)
	if pmode=tpblock then
!CPL "ISTORE BLOCK", MSTROPND(PX), MSTROPND(CX)
		cx:=mgenireg(cx.reg)
		copyblock(px, cx, currtcl.size)
	else

		genmc(m_mov, px, changeopndsize(cx, p.size))
	fi

end

proc tx_call*(tcl p) =	! ([T ...] F [r ...]) r=nret, n=nargs
	do_call(p)
end

proc tx_retproc*(tcl p) =	! return
	genmc_label(m_jmp, mretindex)
end

proc tx_retfn*(tcl p) =	! return a

	if pfloat[p.mode] then
		loadopnd(p.a, reg:xr0)
	else
		loadopnd(p.a, reg:r0)
	fi
	genmc_label(m_jmp, mretindex)
end

proc tx_retmult*(tcl p) =	! return n values
	unimpl(p)
end

proc tx_jump*(tcl p) =	! goto L
	genmc_label(m_jmp, p.a.labelno)
end

proc tx_jumpcc*(tcl p) =	! goto L when b cc c
	do_jumpcc(p)
end

proc tx_jumpt*(tcl p) =	! goto L when istrue(b)
	do_jumptf(p, nz_cond)
end

proc tx_jumpf*(tcl p) =	! goto L when not istrue(b)
	do_jumptf(p, z_cond)
end

proc tx_ijump*(tcl p) =	! goto a
!	mclopnd ax
!
!	ax:=loadopnd(p.a)

!	genmc(m_jmp, mgenireg(ax.reg))
	genmc(m_jmp, loadopnd(p.a))
end

proc tx_setcc*(tcl p) =	! T := b cc c
	do_setcc(p)
end

proc tx_to*(tcl p) =	! --b; goto L when b<>0
	mclopnd ax
	tclopnd b:=p.b

	ax:=loadopnd(b)
	genmc(m_dec, ax)
	storeopnd(b, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.a.labelno))
end

proc tx_forup*(tcl p) =	! b+:=n; goto L when b <= c
	do_for(p)
end

proc tx_iswap*(tcl p) =	! swap(P, P)
	mclopnd px, qx, ax, bx

	px:=loadptropnd(p.a)
	qx:=loadptropnd(p.b)

	ax:=mgenreg(getworkreg(pmode), pmode)
	bx:=mgenreg(getworkreg(pmode), pmode)

	genmc(m_mov, ax, px)
	genmc(m_mov, bx, qx)

	genmc(m_mov, qx, ax)
	genmc(m_mov, px, bx)

end

proc tx_add*(tcl p) =	! T := b + c
	do_binarith(p, m_add, m_addss)
end

proc tx_sub*(tcl p) =	! T := b - c
	do_binarith(p, m_sub, m_subss)
end

proc tx_mul*(tcl p) =	! T := b * c
	do_binarith(p, m_imul2, m_mulss)
end

proc tx_div*(tcl p) =	! T := b / c (float only)
	do_binarith(p, m_nop, m_divss)
end

proc tx_idiv*(tcl p) =	! T := b / c (int only; b % c)
	do_divrem(p, issigned:psigned[pmode], isdiv:1)
end

proc tx_irem*(tcl p) =	! T := b irem c
	do_divrem(p, issigned:psigned[pmode], isdiv:0)
end

proc tx_bitand*(tcl p) =	! T := b iand c
	do_bitbin(p, m_and)
end

proc tx_bitor*(tcl p) =	! T := b ior c
	do_bitbin(p, m_or)
end

proc tx_bitxor*(tcl p) =	! T := b ixor c
	do_bitbin(p, m_xor)
end

proc tx_shl*(tcl p) =	! T := b << c
	do_shift(p, m_shl)
end

proc tx_shr*(tcl p) =	! T := b >> c
	do_shift(p, (psigned[pmode]|m_sar|m_shr))
end

proc tx_min*(tcl p) =	! T := min(b, c)
	if pfloat[pmode] then
		do_max_float(p, m_minss+pwide[pmode])
	else
		do_max_int(p, (psigned[pmode]|gt_cond|gtu_cond))
	fi
end

proc tx_max*(tcl p) =	! T := max(b, c)
	if pfloat[pmode] then
		do_max_float(p, m_maxss+pwide[pmode])
	else
		do_max_int(p, (psigned[pmode]|lt_cond|ltu_cond))
	fi
end

proc tx_subpx*(tcl p) =	! T := b - c*s
	int scale
	mclopnd ax, bx
	tclopnd b

	scale:=p.scale

	ax:=loadopnd(p.b)
	b:=p.c

	if b.optype=int_opnd then
		genmc(m_sub, ax, mgenint(b.value*scale))
	else
		bx:=loadopnd(b)
		scale:=scaleindex(bx, scale)
		if scale>1 then
			mulimm(bx, scale)
		fi
		genmc(m_sub, ax, bx)
	fi

	storeopnd(p.a, ax)

end

proc tx_subp*(tcl p) =	! T := (b - c)/s
	mclopnd ax, bx
	int n, scale:=p.scale

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	genmc(m_sub, ax, bx)

	if scale>1 then
		n:=ispoweroftwo(scale)
		if n then
			genmc(m_shr, ax, mgenint(n))
		else
			if ax.reg=r0 then
				genmc(m_cqo)
				genmc(m_mov, bx, mgenint(scale))
				genmc(m_idiv, bx)
			else
				merror("subp?")
			fi
		fi
	fi

	storeopnd(p.a, ax)
end

proc tx_atan2*(tcl p) =	! T := atan2(b, c)
	unimpl(p)
end

proc tx_power*(tcl p) =	! T := b ** c
	psymbol d
	
	if pint[pmode] then
		d:=gethostfn(kpower)
		do_host(p, d, 2)
	else
		do_maths(p, "pow*", 2)
	fi
end

proc tx_fmod*(tcl p) =
	unimpl(p)
end

proc tx_neg*(tcl p) =	! T := -b
	mclopnd ax

	ax:=loadopnd(p.b)

	if pfloat[pmode] then
		if ispwide(pmode) then
			if not labneg64 then labneg64:=mcreatefwdlabel() fi
			genmc(m_xorpd, ax, mgenlabelmem(labneg64))
		else
			if not labneg32 then labneg32:=mcreatefwdlabel() fi
			genmc(m_xorps, ax, mgenlabelmem(labneg32))
		fi
	else
		genmc(m_neg, ax)
	fi

	storeopnd(p.a, ax)
end

proc tx_abs*(tcl p) =	! T := abs b
! T := abs b
	mclopnd ax,lx

	ax:=loadopnd(p.b)

	if pint[pmode] then
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_label, lx)

	else
		if pwide[pmode] then
			if not lababs64 then lababs64:=mcreatefwdlabel() fi
			genmc(m_andpd, ax, mgenlabelmem(lababs64))
		else
			if not lababs32 then lababs32:=mcreatefwdlabel() fi
			genmc(m_andps, ax, mgenlabelmem(lababs32))
		fi
	fi

	storeopnd(p.a, ax)
end

proc tx_bitnot*(tcl p) =	! T := inot b
	mclopnd ax

	ax:=loadopnd(p.b)
	genmc(m_not, ax)
	storeopnd(p.a, ax)
end

proc tx_not*(tcl p) =	! T := not b
	mclopnd ax

	if not pint[pmode] then merror("not") fi
	ax:=loadopnd(p.b)
	genmc(m_xor, ax, mgenint(1))
	storeopnd(p.a, ax)
end

proc tx_toboolt*(tcl p) =	! T := istrue b
!toboolf implements 'not b' in HLL when b is not known to a boolean

	mclopnd ax, bx, cx
	byte pmode2:=p.mode2

	ax:=loadopnd(p.b, pmode2)

	if pfloat[pmode2] then
		bx:=mgenreg(getworkxreg(), pmode2)
		cx:=mgenreg(getworkireg(), tpu8)
		genmc(m_xorps+pwide[pmode2], bx, bx)
		genmc(m_comiss+pwide[pmode2], ax, bx)

		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), cx)
		genmc(m_movzx, changeopndsize(cx,4),cx)		!4 works for u32/u64
		storeopnd(p.a, cx)

	else
		genmc(m_test, ax, ax)
		genmc_cond(m_setcc, (p.opcode=ktoboolt|ne_cond|eq_cond), bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)
		storeopnd(p.a, ax)
	fi
end

proc tx_sqr*(tcl p) =	! T := sqr(b)
	mclopnd ax
	int opc

	ax:=loadopnd(p.b)
	if pfloat[pmode] then
		opc:=m_mulss+pwide[pmode]
	else
		opc:=m_imul2
	fi

	genmc(opc, ax, ax)
	storeopnd(p.a, ax)
end

proc tx_sqrt*(tcl p) =	! T := sqrt(b)
	mclopnd fx

	fx:=loadopnd(p.b)
	genmc(m_sqrtss+pwide[pmode], fx, fx)
	storeopnd(p.a, fx)
end

proc tx_sin*(tcl p) =	! T := sin(b)
	do_maths(p, "sin*")
end

proc tx_cos*(tcl p) =	! T := cos(b)
	do_maths(p, "cos*")
end

proc tx_tan*(tcl p) =	! T := tan(b)
	do_maths(p, "tan*")
end

proc tx_asin*(tcl p) =	! T := asin(b)
	do_maths(p, "asin*")
end

proc tx_acos*(tcl p) =	! T := asin(b)
	do_maths(p, "acos*")
end

proc tx_atan*(tcl p) =	! T := atan(b)
	do_maths(p, "atan*")
end

proc tx_log*(tcl p) =	! T := log(b)
	do_maths(p, "log*")
end

proc tx_log10*(tcl p) =	! T := log10(b)
	do_maths(p, "log10*")
end

proc tx_exp*(tcl p) =	! T := exp(b)
	do_maths(p, "exp*")
end

proc tx_round*(tcl p) =	! T := round(b)
	do_maths(p, "round*")
end

proc tx_ceil*(tcl p) =	! T := ceil(b)
	do_maths(p, "ceil*")
end

proc tx_floor*(tcl p) =	! T := floor(b)
	do_maths(p, "floor*")
end

proc tx_fract*(tcl p) =	! T := fract(b)
	unimpl(p)
end

proc tx_sign*(tcl p) =	! T := sign(b)
	unimpl(p)
end

proc tx_float*(tcl p) =	! T := float(b)
	mclopnd fx, ax

	ax:=loadopnd(p.b, p.mode2)
	fx:=mgenreg(getworkxreg(), pmode)

	genmc(m_cvtsi2ss+pwide[pmode], fx, ax)

	storeopnd(p.a, fx)
end

proc tx_fix*(tcl p) =	! T := fix(b)
	mclopnd fx, ax, bx

	fx:=loadopnd(p.b, p.mode2)
	bx:=ax:=mgenreg(getworkireg(), pmode)
	if bx.size<4 then bx:=changeopndsize(bx, 4) fi

	genmc(m_cvttss2si+pwide[p.mode2], bx, fx)

	storeopnd(p.a, ax)
end

proc tx_truncate*(tcl p) =	! T := u(b)
	mclopnd ax, bx
	byte pmode2:=p.mode2

	bx:=loadopnd(p.b, pmode2)
	ax:=changeopndsize(bx, p.size)

	if p.size<>psize[pmode2] then
		genmc(ploadop[pmode2], ax, bx)
	fi

	storeopnd(p.a, ax)
end

proc tx_fwiden*(tcl p) =	! T := r64(b)
	mclopnd fx, gx

	fx:=loadopnd(p.b, tpr32)
	gx:=changeopndsize(fx, 8)

	genmc(m_cvtss2sd, gx, fx)
	storeopnd(p.a, gx)
end

proc tx_fnarrow*(tcl p) =	! T := r32(b)
	mclopnd fx, gx

	fx:=loadopnd(p.b, tpr64)
	gx:=changeopndsize(fx, 4)

	genmc(m_cvtsd2ss, gx, fx)
	storeopnd(p.a, gx)
end

proc tx_widen*(tcl p) =	! T := t(b)
	mclopnd ax, bx

	ax:=loadopnd(p.b, p.mode2)

	if pmode=tpu64 and p.mode2=tpu32 then		!32-bit load should have zeroed top half
		bx:=changeopndsize(ax, p.size)
	else
		genmc((psigned[p.mode2]|m_movsx|m_movzx), bx:=changeopndsize(ax, p.size), ax)
	fi

	storeopnd(p.a, bx)

end

proc tx_typepun*(tcl p) =	! T := t(u@(b))
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
!note r32 with sign-bit set if NOT sign-extended, as r32 bit pattern is considered as u32

	byte smode:=p.mode2
	mclopnd ax, bx:=loadopnd(p.b, smode)

	if pfloat[smode] then
		ax:=mgenreg(getworkireg())
!CPL =MSTROPND(BX)

	else
		ax:=mgenreg(getworkxreg(), pmode)
!CPL =MSTROPND(BX)
	fi
	genmc(m_movq, ax, changeopndsize(bx,8))				!basically switch between float/non-float reg

	storeopnd(p.a, bx)
end

proc tx_addto*(tcl p) =	! P +:= b
	do_bintoarith(p, m_add, m_addss)
end

proc tx_subto*(tcl p) =	! P -:= b
	do_bintoarith(p, m_sub, m_subss)
end

proc tx_multo*(tcl p) =	! P *:= b
	do_bintoarith(p, m_imul2, m_mulss)
end

proc tx_divto*(tcl p) =	! P /:= b (float)
	do_bintoarith(p, m_nop, m_divss)
end

proc tx_idivto*(tcl p) =	! P /:= b (int: %:= b)
	do_divremto(p, issigned:psigned[pmode], isdiv:1)
end

proc tx_iremto*(tcl p) =	! P irem:= b
	do_divremto(p, issigned:psigned[pmode], isdiv:0)
end

proc tx_bitandto*(tcl p) =	! P iand:= b
	do_bitbinto(p, m_and)
end

proc tx_bitorto*(tcl p) =	! P ior:= b
	do_bitbinto(p, m_or)
end

proc tx_bitxorto*(tcl p) =	! P ixor:= b
	do_bitbinto(p, m_xor)
end

proc tx_shlto*(tcl p) =	! P <<:= b
	do_shiftto(p, m_shl)
end

proc tx_shrto*(tcl p) =	! P >>:= b
	do_shiftto(p, (psigned[pmode]|m_sar|m_shr))
end

proc tx_minto*(tcl p) =	! P min:= b
	if pfloat[pmode] then
		do_maxto_real(p, leu_cond)
	else
		do_maxto_int(p, (psigned[pmode]|le_cond|leu_cond))
	fi
end

proc tx_maxto*(tcl p) =	! P max:= b
	if pfloat[pmode] then
		do_maxto_real(p, geu_cond)
	else
		do_maxto_int(p, (psigned[pmode]|ge_cond|geu_cond))
	fi
end

proc tx_addpxto*(tcl p) =	! P +:= b*s
	mclopnd px, bx
	tclopnd b:=p.b

	px:=loadptropnd(p.a)

	if b.optype=int_opnd then
		genmc(m_add, px, mgenint(b.value*p.scale))
	else
		bx:=loadopnd(p.b)
		mulimm(bx, p.scale)
		genmc(m_add, px, bx)
	fi
end

proc tx_subpxto*(tcl p) =	! P -:= b*s
	mclopnd px, bx
	tclopnd b:=p.b

	px:=loadptropnd(p.a)

	if b.optype=int_opnd then
		genmc(m_sub, px, mgenint(b.value*p.scale))
	else
		bx:=loadopnd(b, pmode)
		mulimm(bx, p.scale)
		genmc(m_sub, px, bx)
	fi
end

proc tx_negto*(tcl p) =	! -:=P
	unimpl(p)
end

proc tx_absto*(tcl p) =	! abs:=P
	unimpl(p)
end

proc tx_bitnotto*(tcl p) =	! inot:=P
	unimpl(p)
end

proc tx_notto*(tcl p) =	! not:=P
	unimpl(p)
end

proc tx_toboolto*(tcl p) =	! istrue+:=P
	unimpl(p)
end

proc tx_incrto*(tcl p) =	! ++P
	mclopnd px

	px:=loadptropnd(p.a)
	genmc((p.opcode=kincrto|m_add|m_sub), px, mgenint(p.step))
end

proc tx_incrload*(tcl p) =	! T := ++P
	mclopnd ax, px

	px:=loadptropnd(p.b)
	genmc((p.opcode=kincrload|m_add|m_sub), px, mgenint(p.step))
	ax:=mgenreg(getworkireg(), pmode)
	genmc(m_mov, ax, px)
	storeopnd(p.a, ax)

end

proc tx_loadincr*(tcl p) =	! T := P++
	mclopnd ax, px

	px:=loadptropnd(p.b)
	ax:=mgenreg(getworkireg(), pmode)
	genmc(m_mov, ax, px)
	storeopnd(p.a, ax)

	genmc((p.opcode=kloadincr|m_add|m_sub), px, mgenint(p.step))
end

proc tx_switch*(tcl p) =	! switch on c; L=jumptable, L2=else label
	mclopnd ax
	int minlab:=p.minlab, maxlab:=p.maxlab

	ax:=loadopnd(p.c)								!load index

	if minlab then
		genmc(m_sub, ax, mgenint(minlab))			!base it from zero
	fi
	if p.opcode=kswitch then						!do range check (kswitchu is unchecked)
		genmc(m_cmp, ax, mgenint(maxlab-minlab+1))
		genmc_cond(m_jmpcc, geu_cond, mgenlabel(p.b.labelno))		!jump to else
	fi

	genmc(m_jmp, mgenindex(ireg:ax.reg, scale:8, labno:p.a.labelno))	!jump via table
end

proc tx_swlabel*(tcl p) =	! label for switch jump table
	genmc(m_dq, mgenlabel(p.a.labelno))
end

proc tx_addpx*(tcl p) =	! T := b + c*s + n
	mclopnd ax, bx, cx, px
	tclopnd c:=p.c
	int scale:=p.scale, offset:=p.extra

	bx:=loadopnd(p.b)

	if scale=0 then merror("ADDPX SCALE=0") fi

	ax:=mgenreg(getworkreg(pmode), pmode)

	unless c.optype=int_opnd and c.value=0 and offset=0 then
		cx:=loadopnd(c)
		if scale not in [1,2,4,8] then
			genmc(m_imul2, cx, mgenint(scale))
			scale:=1
		fi
		px:=mgenindex(areg:bx.reg, ireg:cx.reg, scale:scale, offset:offset, size:p.size)
		genmc(m_lea, ax, px)
	else
		ax:=bx
	end	

	storeopnd(p.a, ax)
end

proc tx_stop*(tcl p) =
	mclopnd ax

	ax:=loadopnd(p.a, reg:r10)

	genmc(m_call, mgenextname("exit*"))
end

proc tx_label*(tcl p) =
	genmc(m_label, mgenlabel(p.a.labelno))
end

proc tx_data*(tcl p) =
	unimpl(p)
end

proc tx_loadbit*(tcl p) =	! T := b.[c]
!t:=b.[c]
	tclopnd c:=p.c
	mclopnd ax, bx
	int i,m

	if c.optype=int_opnd then
		i:=c.value
		m:=(i in 0..31|tpu32|tpu64)

		ax:=loadopnd(p.b, m)
		if i then
			genmc(m_shr, ax, mgenint(i, m))
		fi
		goto skip when i=63
	else
		ax:=loadopnd(p.b)
		bx:=loadopnd(c, tpu8, reg:r10)
		genmc(m_shr, ax, bx)
	fi

	genmc(m_and, changeopndsize(ax, 4), mgenint(1, tpu32))
skip:

	storeopnd(p.a, ax)

end

proc tx_loadbf*(tcl p) =	! T := b.[c..d]
	tclopnd c:=p.c, d:=p.abc[4]
	mclopnd ax

	if c.optype=d.optype=int_opnd then
		ax:=do_loadbf_const(p, c.value, d.value)
	else
		ax:=do_loadbf_var(p)
	fi

	storeopnd(p.a, ax)
end

proc tx_storebit*(tcl p) =	! P.[b] := c
	do_storebit(p)
end

proc tx_storebf*(tcl p) =	! P.[b..c] := d
	do_storebf(p)
end

proc tx_idivrem*(tcl p) =	! (T1, T2) := C divrem d
	do_divrem(p, issigned:psigned[pmode], isdiv:2)
end

proc tx_jumpin*(tcl p) =	! goto L when b in c..d
	mclopnd ax, bx, cx
	int lab

	lab:=mcreatefwdlabel()

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

!note: jumpout is only defined for i64
	genmc(m_cmp, ax, bx)
	genmc_cond(m_jmpcc, lt_cond, mgenlabel(lab))

	cx:=loadopnd(p.abc[4])
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, le_cond, mgenlabel(p.a.labelno))
	mdefinefwdlabel(lab)

end

proc tx_jumpout*(tcl p) =	! goto L when b not in c..d
	mclopnd lx, ax, bx, cx

	lx:=mgenlabel(p.a.labelno)
	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

!note: jumpout is only defined for i64
	genmc(m_cmp, ax, bx)
	genmc_cond(m_jmpcc, lt_cond, lx)

	cx:=loadopnd(p.abc[4])
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, gt_cond, lx)
end

proc tx_clear*(tcl p) =	! clear P
	mclopnd ax

	ax:=loadopnd(p.a, tpu64)
	clearblock(ax, p.size)
end

proc do_binarith(tcl p, int iopc, fopc)=
!opc is integer op, fopc is r32 op; get r64 op by adding 1
	mclopnd ax, bx
	int opc

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	if pfloat[pmode] then
		opc:=fopc+pwide[pmode]
!		if p.size=8 then ++opc fi
	else
		opc:=iopc
	fi

	genmc(opc, ax, bx)
	storeopnd(p.a, ax)
end

proc do_call(tcl p)=
!call function:
! adjust stack if needed
! push stacked args
! load reg args
! create shadowspace if there were stacked args
! call the function
! pop all the extra stuff pushed to SP
! store results in dest temps
	int pushedbytes:=0, nargs:=p.nargs, nret:=p.nret, argoffset:=p.argoffset
	byte blockret
	u32 blocksize
	tclopnd a
	mclopnd ax, bx

!CPL "DOCALL", =NARGS, =NRET, =ARGOFFSET

	blockret:=0
	blocksize:=0
	if p.mode=tpblock then
		case nret
		when 0 then merror("block ret not used")
		when 1 then
		else merror("block/mult ret")
		esac

		if p.isvariadic then merror("block/variadic") fi

		blockret:=1
		blocksize:=p.size

		--argoffset
		++nargs

	fi

	if nargs>4 then			!need to push some args
		if nargs.odd then
			pushstack(8)
			pushedbytes:=8
		fi
		for i:=nargs downto 5 do
			a:= p.abc[i+argoffset]
			ax:= loadopnd(a, reg:r10)
			ax:=changeopndsize(ax, 8)

			if p.isvariadic and a.opmode=tpr32 then		!needs widening
				genmc(m_movq, bx:=mgenreg(xr0), ax)
				genmc(m_cvtss2sd, bx, changeopndsize(bx, 4))
				genmc(m_movq, ax, bx)
			fi

			genmc(m_push, ax)
			pushedbytes+:=8
		od
	fi

	for i to min(nargs,4) do
		if blockret and i=1 then
			a:=p.a					!will be temp
!			MCOMMENT("PUSH BLOCK ADDR")
			genmc(m_lea, mgenreg(r10), mgentemp(a.tempno))

		else

			a:=p.abc[i+argoffset]
			if pfloat[a.opmode] then
				if a.isvariadic and a.opmode=tpr32 then
					ax:=loadopnd(a, a.opmode, reg:xr0+i-1)
					bx:=changeopndsize(ax, 8)
					genmc(m_cvtss2sd, bx, ax)
					genmc(m_movq, mgenreg(r10+i-1), bx)

				else
					loadopnd(a, a.opmode, reg:xr0+i-1)
					if a.isvariadic then
						loadopnd(a, a.opmode, reg:r10+i-1)
					fi
				fi

			else
				loadopnd(a, a.opmode, reg:r10+i-1)
			fi
		fi
	od
!CPL $LINENO

	if nargs>4 then
		pushstack(32)
		pushedbytes+:=32
	fi

!CPL $LINENO
	a:=p.abc[nret+1]						!function operand
	if a.optype=memaddr_opnd then			!simple direct function
		genmc(m_call, mgenmemaddr(a.def))
	else
!CPL "HERE", STROPND(A), STRPMODE(A.OPMODE), STRPMODE(PMODE)
		ax:=loadopnd(a, tpu64)
		genmc(m_call, ax)
	fi
!CPL $LINENO

	if pushedbytes then
		popstack(pushedbytes)
	fi
!CPL $LINENO

	if not blockret then					!block already copied by callee into temp
		for i to nret do
			a:=p.abc[i]
			if pfloat[a.opmode] then
				ax:=mgenreg(xr0+i-1, a.opmode)
			else
				ax:=mgenreg(r0+i-1)
			fi
			storeopnd(a, ax, mode:a.opmode)
		od
	fi
!CPL $LINENO

end

proc do_jumpcc(tcl p) =
	int mcond
	byte mode:=p.mode
	mclopnd ax, bx, lx

	mcond:=ucondcodes[p.cond]
	lx:=mgenlabel(p.a.labelno)

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	if mode=tpblock then
		merror("jumpcc/block")
	elsif pint[mode] then
		if psigned[mode] then
			mcond:=scondcodes[p.cond]
		fi
		genmc(m_cmp, ax, bx)
	else
		genmc(m_comiss+pwide[mode], ax, bx)
	fi

	genmc_cond(m_jmpcc, mcond, lx)

end

proc do_setcc(tcl p) =
	int mcond
	byte mode:=p.mode
	mclopnd ax, bx, cx

!note: could share most of following with jumpcc:
	mcond:=ucondcodes[p.cond]

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	if mode=tpblock then
		merror("SETcc/block")
	elsif pint[mode] then
		if psigned[mode] then
			mcond:=scondcodes[p.cond]
		fi
		genmc(m_cmp, ax, bx)
	else
		genmc(m_comiss+pwide[mode], ax, bx)
	fi

	cx:=mgenreg(getworkireg(), tpu8)

	genmc_cond(m_setcc, mcond, cx)
	genmc(m_movzx, changeopndsize(cx, 4), cx)

	storeopnd(p.a, cx)
end

proc do_for(tcl p)=
!	(kforup,	$+1,  0,  1,  0),  ! n   (L b c)	b+:=n; goto L when b <= c
!	(kfordown,	$+1,  0,  1,  0),  ! n   (L b c)	b-:=n; goto L when b >= c

	mclopnd bx
	byte up:=p.opcode=kforup
	psymbol d

	bx:=loadopnd(p.b)								!index var
	genmc((up|m_add|m_sub), bx, mgenint(p.step))	!incr/decr it
	storeopnd(p.b, bx)

	genmc(m_cmp, bx, loadopnd(p.c))

	genmc_cond(m_jmpcc, (up|le_cond|ge_cond), mgenlabel(p.a.labelno))

end

proc do_jumptf(tcl p, int cond)=
	mclopnd ax, bx

	ax:=loadopnd(p.b)

	if pint[pmode] then
		genmc(m_test, ax, ax)
	else
		bx:=mgenreg(getworkxreg(), psize[pmode])
		genmc(m_xorps+pwide[pmode], bx, bx)
		genmc(m_comiss+pwide[pmode], ax, bx)
	fi

	genmc_cond(m_jmpcc, cond, mgenlabel(p.a.labelno))
end

proc do_bitbin(tcl p, int opc)=
	mclopnd ax, bx

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)
	genmc(opc, ax, bx)
	storeopnd(p.a, ax)
end

proc do_shift(tcl p, int opc)=
	mclopnd ax, bx
	tclopnd b:=p.c

	ax:=loadopnd(p.b)

	if b.optype=int_opnd then
		genmc(opc, ax, mgenint(b.value))
	else
		bx:=loadopnd(p.c, reg:r10)				!rcx = cl
		genmc(opc, ax, changeopndsize(bx,1))
	fi

	storeopnd(p.a, ax)
end

proc do_bintoarith(tcl p, int iopc, fopc)=
	mclopnd px, ax, bx
	byte wide

	px:=loadptropnd(p.a)

	bx:=loadopnd(p.b)

	if pfloat[pmode] then
		wide:=pwide[pmode]
		ax:=mgenreg(getworkxreg(), pmode)
		genmc(m_movd+wide, ax, px)
		genmc(fopc+wide, ax, bx)
		genmc(m_movd+wide, px, ax)
	elsif iopc=m_imul2 then
		ax:=mgenreg(getworkireg(), pmode)
		genmc(m_mov, ax, px)
		genmc(m_imul2, ax, bx)
		genmc(m_mov, px, ax)

	else
		genmc(iopc, px, bx)
	fi
end

proc do_bitbinto(tcl p, int opc)=
	mclopnd px, bx

	px:=loadptropnd(p.a)
	bx:=loadopnd(p.b)

	genmc(opc, px, bx)
end

proc do_shiftto(tcl p, int opc)=
	mclopnd px, bx
	tclopnd b:=p.b

	px:=loadptropnd(p.a)

	if b.optype=int_opnd then
		genmc(opc, px, mgenint(b.value))
	else
		bx:=loadopnd(b, reg:r10)
		genmc(opc, px, changeopndsize(bx,1))
	fi
end

global proc do_divrem(tcl p, int issigned, isdiv)=
!isdiv = 0/1/2 = rem/div/divrem
! Z' := Y % Z
	mclopnd ax, bx, rx
	tclopnd a, b
	int opc, n, shifts

	if isdiv=2 then
		a:=p.c
		b:=p.abc[4]
	else
		a:=p.b
		b:=p.c
	fi

	ax:=loadopnd(a, reg:r0)
	nextworkreg:=r1

	if b.optype=int_opnd and isdiv=1 then
		n:=b.value

		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts then
				genmc((issigned|m_sar|m_shr), ax, mgenint(shifts))
				storeopnd(p.a, ax)
				return
			fi
		esac
	fi 

	bx:=loadopnd(b)
	rx:=mgenreg(r11)

	if issigned then
		opc:=
			case psize[pmode]
			when 8 then	m_cqo
			when 4 then	m_cdq
			when 2 then	m_cwd
			else merror("div/u8"); 0
			esac
		genmc(opc)

		opc:=m_idiv
	else
		clearreg(rx)
		opc:=m_div
	fi

	genmc(opc, bx)

	case isdiv
	when 0 then				!rem; result in r11
		storeopnd(p.a, rx)
	when 1 then				!div; result in r0
		storeopnd(p.a, ax)
	else					!divrem; results in r0:r11
		storeopnd(p.a, ax)
		storeopnd(p.b, rx)
	esac

end

proc do_max_int(tcl p, int cond)=
	mclopnd ax,bx

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	storeopnd(p.a, ax)
end

global proc do_max_float(tcl p, int opc)=
	mclopnd ax,bx

	ax:=loadopnd(p.b)
	bx:=loadopnd(p.c)

	genmc(opc, ax, bx)

	storeopnd(p.a, ax)
end

global proc do_maxto_int(tcl p, int cond)=
	mclopnd px, ax, bx
	int lab

	px:=loadptropnd(p.a)
	ax:=mgenreg(getworkireg(), pmode)
	genmc(m_mov, ax, px)

	bx:=loadopnd(p.b)

	genmc(m_cmp, ax, bx)
	lab:=mcreatefwdlabel()

	genmc_cond(m_jmpcc, cond, mgenlabel(lab))
	genmc(m_mov, px, bx)
	mdefinefwdlabel(lab)
end

global proc do_maxto_real(tcl p, int cond)=
	mclopnd px, ax, bx
	int lab

	px:=loadptropnd(p.a)
	bx:=loadopnd(p.b)

	ax:=mgenreg(getworkxreg(), pmode)
	genmc(m_mov, ax, px)

	genmc(m_comiss+pwide[pmode], ax, bx)
	lab:=mcreatefwdlabel()

	genmc_cond(m_jmpcc, cond, mgenlabel(lab))
	genmc(m_mov, px, bx)
	mdefinefwdlabel(lab)
end

func do_loadbf_const(tcl p, int i, j)mclopnd =
	mclopnd ax, mx
	word mask

	ax:=loadopnd(p.b)

	if j=63 then			!signed field includes sign bit; assume i>0
		genmc(m_sar, ax, mgenint(i))
	else

		if i then
			genmc(m_shr, ax, mgenint(i))
		fi

		mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))

		if mask>=word(i32.max) then
			mx:=mgenreg(getworkireg())
			genmc(m_mov, mx, mgenint(mask))

		else
			mx:=mgenint(mask)
		fi

		genmc(m_and, ax, mx)
	fi

	ax
end

func do_loadbf_var(tcl p)mclopnd =
	merror("loadbf_var")
	nil
end

proc do_storebit(tcl p) =
!	P.[b] := c
	tclopnd b:=p.b, c:=p.c
	mclopnd px, ax, cx, ix
	int i, offset
	byte mask1s, mask0s

	if b.optype=int_opnd then		!a.[k] := 0/1/x
		px:=loadptropnd(p.a)
		px:=changeopndsize(px, 1)	!update only a specific byte
		i:=b.value	
		offset:=i/8					! byte offset 0..7
		i iand:=7					! i will be bit index 0..7
		px:=applyoffset(px, offset)	! point to that byte

		mask0s:=1<<i				!eg 00001000
		mask1s:=inot(1<<i)			!eg 11110111

		if c.optype=int_opnd then
			if c.value=0 then
				genmc(m_and, px, mgenint(mask1s, pmode))
			else
				genmc(m_or, px, mgenint(mask0s, pmode))
			fi
		else
			ax:=loadopnd(c, tpu8)
			genmc(m_and, px, mgenint(mask1s, pmode))		!clear dest bit first
			if i then
				genmc(m_shl, ax, mgenint(i, tpu8))
			fi
			genmc(m_or, px, ax)							!add in 0 or 1
		fi
	elsif c.optype=int_opnd then						!A.[i]:=0/1
		px:=loadptropnd(p.a)

		ax:=mgenreg(getworkireg())
		genmc(m_mov, ax, mgenint(1))
		
		cx:=mgenreg(r10,tpu64)
		ix:=loadopnd(b, tpi64, reg: r10)
		genmc(m_shl, ax, changeopndsize(cx, 1))

!Now have 00001000 for ezzmple in ax
		if c.value=0 then
			genmc(m_not, ax)				!change to 111101111
			genmc(m_and, px, ax)			!set to 0
		else								!set to 1 (assume c.value was 1)
			genmc(m_or, px, ax)
		fi

	else
			merror("Storebit: both vars")
	fi
end

global proc do_storebf(tcl p) =
!	P.[b..c] := d
	mclopnd px, rx, mx, dx
	tclopnd b:=p.b, c:=p.c, d:=p.abc[4]
	int i, j
	word mask

	unless b.optype=c.optype=int_opnd then
		merror("storebf not imm")
	end

	dx:=loadopnd(d)							!rhs: value to store

	px:=loadptropnd(p.a)

	i:=b.value
	j:=c.value

	mx:=mgenreg(getworkireg(),pmode)
	rx:=mgenreg(getworkireg(),pmode)

	genmc(m_mov, rx, px)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

	genmc(m_mov, mx, mgenint(mask, pmode))

	if i then
		genmc(m_shl, dx, mgenint(i))
	fi

	genmc(m_and, rx, changeopndsize(mx, p.size))
	genmc(m_or, rx, dx)

	genmc(m_mov, px, changeopndsize(rx, p.size))
end

proc do_maths(tcl p, ichar opname, int nargs=1)=
	do_callrts(p, opname, nil, nargs)
end

proc do_host(tcl p, psymbol d, int nargs=1)=
	do_callrts(p, nil, d, nargs)
end

global proc do_callrts(tcl p, ichar opname, psymbol d, int nargs)=
!simpler version of do_call where args are always <=4, and no variadics,
!there is always one return value, and func call is always direct
!mode of args and return value assumed to be all pmode
	tclopnd a
	mclopnd ax

	for i to nargs do
		a:=p.abc[i+1]
		if pfloat[pmode] then				!mode of tcl op not operands
			loadopnd(a, reg:xr0+i-1)
		else
			loadopnd(a, reg:r10+i-1)
		fi
	od

	if opname then
		genmc(m_call, mgenextname(opname))
	else
		genmc(m_call, mgenmemaddr(d))
	fi

	if pfloat[pmode] then
		ax:=mgenreg(xr0, pmode)
	else
		ax:=mgenreg(r0)
	fi

	storeopnd(p.a, ax)
end

global proc do_divremto(tcl p, int issigned, isdiv)=
! P /:= b or P rem:= b

!isdiv = 0/1 = rem/div
! Z' := Y % Z
	mclopnd px, ax, bx, rx
	tclopnd b:=p.b
	int opc, n, shifts

	ax:=mgenreg(r0, pmode)
	nextworkreg:=r1
	px:=loadptropnd(p.a)

	genmc(m_mov, ax, px)

	bx:=loadopnd(b)

	rx:=mgenreg(r11, pmode)

	if issigned then
		opc:=
			case psize[pmode]
			when 8 then	m_cqo
			when 4 then	m_cdq
			when 2 then	m_cwd
			else merror("div/u8"); 0
			esac
		genmc(opc)

		opc:=m_idiv
	else
		clearreg(rx)
		opc:=m_div
	fi

	genmc(opc, bx)

	genmc(m_mov, px, (isdiv|ax|rx))
end

proc tx_setjmp*(tcl p)=
	mclopnd px, ax, bx
	int lab:=mcreatefwdlabel()


	ax:=loadopnd(p.a)
	px:=mgenireg(ax.reg)

	bx:=mgenreg(getworkireg())
	genmc(m_mov, bx, mgenlabel(lab))
	genmc(m_mov, px, bx)

	genmc(m_mov, applyoffset(px,8), dstackopnd)
	genmc(m_mov, applyoffset(px,16), dframeopnd)

	clearreg(ax)
	mdefinefwdlabel(lab)

end

proc tx_longjmp*(tcl p)=
	mclopnd px, ax, bx, cx

	bx:=loadopnd(p.b, reg:r0)			!ret value
	nextworkreg++
	highworkreg max:=nextworkreg

	ax:=loadopnd(p.a)
	px:=mgenireg(ax.reg)

	genmc(m_mov, dstackopnd, applyoffset(px,8))
	genmc(m_mov, dframeopnd, applyoffset(px,16))

	cx:=mgenreg(getworkireg())
!
	genmc(m_mov, cx, px)		!load stored return address
	genmc(m_jmp, cx)			!
end

proc tx_getr0*(tcl p)=
	storeopnd(p.a, mgenreg(r0))
end

proc tx_initdswx*(tcl p)=
!only needed for C target
end

=== mc_temp_xb.m 0 0 25/32 ===

global func loadopnd(tclopnd a, int mode=pmode, reg=rnone, copy=0)mclopnd =
!Load operand into a register, and return register number

!* When a dest register is not provided:
!
!  * For something not in a register, allocate a workreg and load it
!  * If already in a register, return that register ...
!  * ... unless a flag says it needs a copy
!
!* When a dest register is provided:
!
!  * If already in a register, then move (unless already in dest)
!  * Otherwise, use that instead of a new work register
!
!* When this is a temp, it will either be in a reg, or be spilled.
!  Loading a spilled temp will alter the table entries. Moving a temp
!  to a new specified register (eg. an arg reg) will not alter table entries
!
!* For regvars, they can be used in situ, unless a dest reg is given, or the
!  flag says copy

!Note: for float operands, specifying a non-float register is valid: it will
!load to that integer register (this may be need to load a value which is then
!more simply pushed). But ultimately it may be better to use a dedicated pushopnd func.

	psymbol d
	mclopnd ax, bx

	if mode=tpvoid then mode:=a.opmode fi

	ax:=mgenreg(getworkregc(mode, reg), mode)

	case a.optype
	when mem_opnd then
		bx:=mgenmem(a.def, mode)
		if mode=tpblock and a.def.id<>param_id then
			dolea
		fi
domov:
		genmc(m_mov, ax, bx)

	when temp_opnd then
		bx:=mgentemp(a.tempno, mode)
		if mode=tpblock then
			dolea
		fi
		domov

	when int_opnd then
		CASE CURRTCL.SIZE
		WHEN 2 THEN
			A.VALUE IAND:=0xFFFF
		WHEN 4 THEN
			A.VALUE IAND:=0xFFFF'FFFF
		ESAC

		bx:=mgenint(a.value, pmode)
		domov

	when real_opnd then
		bx:=mgenrealmem(a.xvalue, tpr64)
		domov

	when r32_opnd then
		bx:=mgenrealmem(a.xvalue32, tpr32)
		domov

	when string_opnd then
		bx:=mgenlabelmem(getstringindex(a.svalue))
dolea:
		genmc(m_lea, ax, bx)


	when label_opnd then
		bx:=mgenlabelmem(a.labelno)
		dolea

	when memaddr_opnd then
		d:=a.def
		bx:=mgenmem(d, mode)
		if d.id=param_id and d.mode=tpblock then		!pcl mode will be u64
            domov
		else
			dolea
		fi

	else
		merror("Loadopnd:", opndnames[a.optype])
	esac

!IF AX.SIZE=0 THEN CPL "SIZE=0" FI
!IF BX AND BX.SIZE=0 THEN CPL "BSIZE=0", MCLNAMES[MCCODEX.OPCODE], OPNDNAMES[A.OPTYPE] FI

	ax
end

global func loadptropnd(tclopnd a, int reg=rnone)mclopnd=
	mclopnd ax

	reg:=getworkiregc(reg)
	ax:=mgenreg(reg)

	case a.optype
	when mem_opnd then
		genmc(m_lea, ax, mgenmem(a.def))

	when temp_opnd then
		genmc(m_mov, ax, mgentemp(a.tempno))
	else
		merror("Loadptrop", opndnames[a.optype])
	esac

	mgenireg(reg, pmode)
end

global func getworkireg:int=
	if nextworkreg>r9 then
		merror("No more work regs")
	fi

	nextworkreg++
	highworkreg max:=nextworkreg
	nextworkreg
end

global func getworkxreg:int=
	if nextworkxreg>xr15 then
		merror("No more work xregs")
	fi
	nextworkxreg++
	highworkxreg max:=nextworkxreg
	nextworkxreg
end

global func getworkregc(int mode, reg=rnone)int =
!get workreg conditionally: only reg is not specified
	if reg then
		reg
	else
		getworkreg(mode)
	fi
end

func getworkiregc(int reg=rnone)int=
	if reg then
		reg
	else
		getworkireg()
	fi
end

func getworkxregc(int reg=rnone)int=
	if reg then
		reg
	else
		getworkxreg()
	fi
end

global func getworkreg(int mode)int =
	if ispfloat(mode) then
		getworkxreg()
	else
		getworkireg()
	fi
end

global proc storeopnd(tclopnd a, mclopnd bx, int mode=pmode) =
!store operand currently in register bx, to tcl operand a
!a will a simple variable, or temp
!note: bx will not necessarily be the correct size for dest
	mclopnd ax

!cpl "STOREOPND", MSTROPND(BX)

	if mode<>tpblock then
		bx:=changeopndsize(bx, psize[mode])
	fi

	case a.optype
	when mem_opnd then
		ax:=mgenmem(a.def, mode)
	when temp_opnd then
		ax:=mgentemp(a.tempno, mode)
	else
		merror("Storeopnd:", opndnames[a.optype])
	esac

	if bx.reg>=xr0 then
		genmc((bx.size=8|m_movq|m_movd), ax, bx)
	elsif mode=tpblock then
		bx:=mgenireg(bx.reg)

		copyblock(ax, bx, currtcl.size)
	else
		genmc(m_mov, ax, bx)
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
		newreg:=getworkireg()
	elsif reg then				![reg] only; already simple
		return ax
	elsif regix then			![regix] only; may be scaled; use lea anyway
		newreg:=regix
	else						![reg+regix]
		newreg:=regix
	fi

	bx:=mgenireg(newreg)

	genmc(m_lea, mgenreg(newreg), ax)
	return bx
end
=== mc_objdecls.m 0 0 26/32 ===
global record imagefileheader =
	u16	machine
	u16	nsections
	u32	timedatestamp
	u32	symtaboffset
	u32	nsymbols
	u16	optheadersize
	u16	characteristics
end

global record imagedir =
	u32	virtualaddr
	u32	size
end

global record optionalheader =			!exe/dll only
	u16  magic
	byte     majorlv
	byte     minorlv
	u32 codesize
	u32 idatasize
	u32 zdatasize
	u32 entrypoint
	u32 codebase
!	u32 datebase		!32-bit exe files only
	u64	imagebase
	u32 sectionalignment
	u32 filealignment
	u16  majorosv
	u16  minorosv
	u16  majorimagev
	u16  minorimagev
	u16  majorssv
	u16  minorssv
	u32 win32version
	u32 imagesize
	u32 headerssize
	u32 checksum
	u16  subsystem
	u16  dllcharacteristics
	u64   stackreserve
	u64   stackcommit
	u64   heapreserve
	u64   heapcommit
	u32 loaderflags
	u32 rvadims
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
		u32	physical_address
		u32	virtual_size
	end
	u32	virtual_address
	u32	rawdata_size
	u32	rawdata_offset
	u32	relocations_ptr
	u32	linenos_offset
	u16	nrelocs
	u16	nlinenos
	u32	characteristics
end

global record imagesymbol =
	union
		[8]char shortname
		struct
			u32	shortx
			u32	longx
		end
		u64 longname
	end
	u32	value
	i16	sectionno
	u16	symtype
	byte	storageclass
	byte	nauxsymbols
end

global record importdirrec =
	u32	implookuprva
	u32	timedatestamp
	u32	fwdchain
	u32	namerva
	u32	impaddressrva
end

global record coffrelocrec =
	i32	virtualaddr
	i32	stindex
	i16	reloctype
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
	i32 length
	i16 nrelocs
	i16 nlines
	i32 checksum
	i16 sectionno
	i32 dummy
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
	psymbol def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	psymbol def				!full st entry
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
	u32 exportflags
	u32 timedatestamp
	u16 majorversion
	u16 minorversion
	u32 namerva
	u32 ordinalbase
	u32 naddrtable
	u32 nnamepointers
	u32 expaddressrva
	u32 namepointerrva
	u32 ordtablerva
end
=== mc_genss.m 0 0 27/32 ===
const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

BYTE DEBUG


const wbit = 3

byte rex
byte sizeoverride					!32=>16 switch
byte addroverride					!32=>16 switch
byte f2override						!xmm regs
byte f3override						!xmm regs
byte nowmask						!disable w-bit
byte usesizeb						!1 tests opnd b for wmask

GLOBAL record amoderec =					!return from genrm
!record amoderec =					!return from genrm
	byte modrm						!
	byte sib						!
	i8 usesib						!-1/0/1 = rip/not used/used
	byte dispsize					!0, 1 or 4
	i32 offset					!for dispsize = 1/4
end

mclopnd extraparam
ref[]tempmoderec tempmodes


int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
ref relocrec currrelocs
int nrelocs

[r0..r15]byte ishighreg				!high regs have 0x40 (see start)

REF MCLREC CURRMCL
ref riprec ripentry

macro genbyte(x) = currdata.pcurr++^:=x

!PROC GENBYTE(int x)=
!IF DEBUG THEN CPL "GENBYTE:",X:"2ZH" FI
!	currdata.pcurr++^:=x
!end
!

macro makemodrm(mode,opc,rm) = mode<<6+opc<<3+rm

global proc genss(int obj=0)=
	int index
	ref mclrec m

	return when ssdone

	sstime:=os_clock()

	initlib(mlabelno)
!CPL "INITLAB 50"
!	initlib(50)

	ss_zdatalen:=0
	ss_zdata:=buffercreate()
	ss_idata:=buffercreate()
	ss_code:=buffercreate()
	ss_idatarelocs:=nil
	ss_coderelocs:=nil
	ss_nsymbols:=0

	switchseg(code_seg)

	aaseqno:=9999
	extraparam:=nil

!	fixregvar()

	m:=mccode
	index:=0

	while m do
		doinstr(m,++index)
		m:=m.nextmcl
	od
!CPL "DONE GENSS LOOP"

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		axerror("Zdata contains code or data")
	fi

	if obj then					!do fixups needed for .obj files
		ref riprec pr			!(exe module does its own fixups)
		ref byte codeaddr
		ref u32 offsetptr

		codeaddr:=bufferelemptr(ss_code, 0)
			pr:=riplist
			while pr, pr:=pr.next do
				offsetptr:=ref u32(codeaddr+pr.offset)
				offsetptr^-:=pr.immsize
		od
	fi

	ssdone:=1
	sstime:=os_clock()-sstime

end

proc doinstr(ref mclrec m,int index)=
	mclopnd a,b
	psymbol d,e
	int x,offset,shortjmp,n

!CPL "DOINSTR",MCLNAMES[M.OPCODE], M.SEQNO

	if currdata.pend-currdata.pcurr<1024 then
		bufferexpand(currdata)
	fi

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=nowmask:=usesizeb:=0

	a:=m.a
	b:=m.b

	aaseqno:=m.seqno
	aapos:=m.mpos
	ripentry:=nil
	CURRMCL:=M

	switch m.opcode
	when m_procstart then
		currfunc:=m.a.def
		tempmodes:=currfunc.tempmodes

	when m_procend then
	when m_define then

	when m_definereg then
	when m_definetemp then

	when m_labelname then
		case a.valtype
		when stringimm_val then
		when def_val then
			d:=a.def
			d.reftype:=back_ref
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)
			if d.exported then
				getstindex(d)
			fi

			dofwdrefs(d)
		esac

	when m_label then

		if a.valtype=def_val then			!named label (probably from assembler)
			d:=a.def
		else
			d:=labeldeftable[a.labelno]
		fi
	
		d.reftype:=back_ref
		d.segment:=currseg
		d.offset:=getcurrdatalen(6)

		if d.exported then
			getstindex(d)
		fi

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

	when m_neg, m_not, m_mul, m_imul, m_div, m_idiv then
		do_neg(a,mclcodes[m.opcode])

	when m_add, m_sub, m_and, m_or, m_xor, m_adc, m_sbb, m_cmp then
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
			buffercheck(currdata, x)
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

	when m_comisd, m_ucomisd then
		do_arithxmm(a,b,0x66,mclcodes[m.opcode])

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

!	when m_param then
!		extraparam:=a

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

	when m_comment, m_endx then
	else
		println "*** SS:Can't do opcode",mclnames[m.opcode],"line",aaseqno,=M.OPCODE,=M_HALT
	CPL
	CPL
	AXERROR("STOPPING")
!	end switch
	end
!CPL "..........DONE"; STOP WHEN OS_GETCH()=27
end

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

proc genqword(i64 x)=
	addqword(currdata,x)
end

proc genopnd(mclopnd a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
	ref char s
	i64 x
	int length

	if size=0 then size:=a.size fi

!IF A.VALTYPE=DEF_VAL THEN
!	CPL "GENOPND",VALTYPENAMES[A.VALTYPE]
!FI

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
			r32 x32
			x32:=a.xvalue
			gendword(i32@(x32))
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
			genqword(i64@(a.xvalue))
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

proc addrelocitem(int reloctype, psymbol d)=
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

func getstindex(psymbol d)int=
!retrieve existing obj st index, or create new one
	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		fi
		d.stindex:=++ss_nsymbols

		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
			if d.imported then
				d.segment:=code_seg
			fi
		fi

	fi
	return d.stindex
end

proc genrel32(mclopnd a)=
!used by call/longjmp/ddoffset
	psymbol d

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
	else								!external psymbol
		gendword(a.offset)		!this is probably just zero
		addrelocitem(rel32_rel,d)
	esac
end

func getdef(mclopnd a,int dneeded=0)psymbol =
	psymbol d

	if a.mode in [a_mem,a_imm] then
		case a.valtype
		when label_val then
			return labeldeftable[a.labelno]
		when def_val then
			d:=a.def
			if d.reftype=0 then
				if not d.imported then
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

proc genabs32(mclopnd a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then

		gendword(d.offset+a.offset)
		addrelocitem(addr32_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
		if d.id in [local_id, param_id] then
			gendword(d.offset+a.offset)
		else
			gendword(a.offset)
			addrelocitem(addr32_rel,d)
		fi

	else								!external psymbol
		gendword(a.offset)					!this is probably just zero
		addrelocitem(addr32_rel,d)
	esac
end

proc genabs64(mclopnd a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then
		genqword(d.offset+a.offset)
		addrelocitem(addr64_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
		if d.id in [local_id, param_id] then
			genqword(d.offset+a.offset)
		else
			genqword(a.offset)
			addrelocitem(addr64_rel,d)
		fi

	else								!external psymbol
		genqword(a.offset)				!this is probably just zero
		addrelocitem(addr64_rel,d)
	esac
end

func getrel32(psymbol d,int offset)int=
!get rel difference between offset in this segment, and label d
	if d.reftype=back_ref then					!defined earlier in this segment
		if d.segment<>currseg then
			axerror("Rel label across segments2")
		fi
		return d.offset-(offset+1)
	else
		return i32.max
	fi
end

proc dofwdrefs(psymbol d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
	ref fwdrec f
	int offset, seg
	ref byte p8
	ref i32 p32
	ref i64 p64
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

func isbytesized(i64 x)int=
	return -128<=x<=127
end

func isdwordsized(i64 x)int=
	return i32.min<=x<=i32.max
end

proc genamode(mclopnd a, amoderec am)=
	psymbol d
	ref riprec pr

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

proc setopsize(mclopnd a)=
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

func getdispsize(mclopnd a, i32 &offset)int=
!look at imm/mem displacement, and return (0,1 or 4) and offset
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	psymbol d

	d:=getdef(a)
	offset:=a.offset

!CPL "GDS", D, VALTYPENAMES[A.VALTYPE], OFFSET

	if d then
		if d.id in [local_id, param_id] then
			offset+:=d.offset
		else
			return 4
		fi
	elsif a.valtype=temp_val and tempmodes then
!CPL "------TEMP", A.TEMPNO, =TEMPMODES, =TEMPMODES[A.TEMPNO].OFFSET
		offset+:=tempmodes[a.tempno].offset

	fi

	if offset then
		return (isbytesized(offset)|1|4)
	else
		return 0
	fi
end

proc checkhighreg(mclopnd a)=
	if a.mode=a_reg then
		rex ior:=ishighreg[a.reg]
	fi
end

proc do_loop(mclopnd a,int opc)=
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

proc do_jcxz(mclopnd a,int opsize)=
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

proc do_call(mclopnd a)=
	int am, regcode
	case a.mode
	when a_imm then
		genbyte(0xE8)
		genrel32(a)
	else				!indirect call
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("call[]size")
		esac

		genxrm(0xFF, 2, a)

	esac
end

proc do_jmp(mclopnd a,ref mclrec m)=
	int am, regcode, offset, shortjmp
	psymbol d:=getdef(a)

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
			axerror("jmp[]size")
		esac

		genxrm(0xFF, 4, a)
	esac

end

func getcurrdatalen(int id)int=
!I think that zdata-seg is only likely when id=6

	if currseg=zdata_seg then
		return ss_zdatalen
	fi
	return bufferlength(currdata)
end

proc do_cmovcc(int cond, mclopnd a,b)=
	if a.size<>b.size and b.size then
		axerror("1:Opnd size mismatch")
	fi
	if a.size=1 then axerror("cmov/byte") fi

	genrrm(0x0F'40+cond, a, b)
end

proc do_fmem(mclopnd a, int freal, code)=
!do fld/fild/fst/fstp/fist,fistp
!freal=1 for fld/etc, 0 for fild etc
!code is middle 3 bits of 2nd byte: 0=load, 2=store, 3=store+pop
	int am, regcode, mf

	if a.mode<>a_mem then
		axerror("fmem/not mem")
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
				axerror("r80 not allowed")
			esac
		else
			CPL "SIZE=",A.SIZE
			axerror("fmem size")
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
				axerror("fst i64?")
			esac
		else
			axerror("fmem int size")
		esac
	fi
	
	genxrm(0xD9+mf<<1, code, a)
end

proc genrel8(mclopnd a)=
!a is a known fwd reference, and expected to be <=127 bytes
	psymbol d

	d:=getdef(a,1)

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
		genbyte(0)
	else								!external psymbol
		axerror("genrel8")
	fi
end

func checkshortjump(ref mclrec m,psymbol d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
!return 0
! d can be a named label, or a labelno; either should have .labelno set
	int n
	mclopnd a

	n:=0
	m:=m.nextmcl
	while m and n<=8 do
		case m.opcode
		when m_label then
			a:=m.a

			case a.valtype
			when label_val then
				if a.labelno=d.labelno then return 1 fi
			when def_val then
				if a.def=d then return 1 fi
			esac

		when m_comment, m_endx then
		when m_resb then
			return 0
		else
			++n
		esac
		m:=m.nextmcl
	od

	return 0
end

func addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
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

proc do_popcnt(mclopnd a,b)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi

	f3override:=1
	genrrm(0x0F'B8, a, b)
end

proc do_bsf(mclopnd a,b, int opc)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi
	if a.size<>b.size then axerror("bsf size") fi

	genrrm(0x0F<<8+opc, a, b)
end

proc extendsymboltable=
	ref[]psymbol oldsymboltable
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
	psymbol d

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0
	labeldeftable:=pcm_alloc(nlabels*ref void.bytes)

!IF NLABELS<MLABELNO THEN
!CPL "INITLAB: BAD LABEL COUNT"
!CPL "INITLAB: BAD LABEL COUNT"
!!SROP
!STOP
!FI
!
!CPL "//INITLIB", NLABELS
!STOP

	for i to nlabels do
		d:=labeldeftable[i]:=pcm_allocnfz(pstrec.bytes)

!CPL "SETTING LAB",I,"TO",D
		d.labelno:=i
		fprint @&.str,"l#",i
		d.name:=pcm_copyheapstring(&.str)
		d.reftype:=fwd_ref
	od
end

global func buffercreate(int size=1024)ref dbuffer=
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

global func bufferlength(ref dbuffer a)int=
	return a.pcurr-a.pstart
end

global func bufferelemptr(ref dbuffer a, int offset)ref void=
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

global proc addqword(ref dbuffer a, i64 x)=
	a.pcurr64^:=x
	++(a.pcurr64)
end

proc genxrm(int opcode, code, mclopnd b)=
!deal with /d instructions, where code = 0..7
	amoderec am

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

proc genrrm(int opcode, mclopnd a, b)=
!deal with /r instructions; rrm = reg,reg/mem
!opcode represents 1, 2 and maybe 3-byte(?) opcodes, with last in lsb place
!a is a register mclopnd, b is a register or memory mclopnd, always
!when data direction is the other way, as in mov reg/mem, reg, then reverse mclopnds
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

	if opcode.[16..23] then genbyte(opcode.[16..24]) fi
	genrex()
	if opcode.[8..15] then genbyte(opcode.[8..15]) fi

	genbyte(opcode)

genamode(b,am)
end

func getregcode(int reg, int mask, isxreg=0)int regcode=
!convert m-register code (1 to 16/20) to hardware code (0..7 plus any rex bits)
!mask is the rex bit to set for high registers
!isxreg is 1 for float registers, where I don't need to do the usual mapping

!	if not isxreg then
		regcode:=regcodes[reg]
!	else
!		regcode:=reg-xr0
!	fi

	if regcode>=8 then
		regcode-:=8
		rex ior:=mask
	fi
	regcode
end

proc checkimmrange(int value, size)=
	case size
	when 1 then
		unless -128<=value<=255 then axerror("exceeding byte value") end

	when 2 then
		unless -32768<=value<=65535 then axerror("exceeding u16 value") end
	else
		unless -0x8000'0000<=value<=0xFFFF'FFFF then axerror("2:exceeding u32 value") end
	esac
end

func genrm(int reg, opc, mclopnd b, int isxreg=0)amoderec=
!reg =  0:	opc is a 3-bit code that goes in reg field of mod-reg-rm
!reg >= r0:	reg is an M-register code, which is converted to a machine reg encoding
!			of 3 bits (to go in middle of modrm byte), and may set rex.r bit for high
!			regs; opc is 0 in this case
!
!b is mclopnd containing rhs reg value, or is mem mclopnd using base/index regs and addr
!For now, return same encoded value as old genrm

	static []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
	int mode, rm, scale, index, base
	int regix, code, ismem
	amoderec am

	clear am

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
			if b.def.id=static_id then ismem:=2 fi
		when realmem_val then ismem:=2
		when label_val then ismem:=2
		esac

	else
		axerror("genrm not mem")
	esac

	mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
	scale:=0				!0=modrm only; 1/2/4/8 means sib used

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
SIMPLE:
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

IF B.SCALE<=1 THEN					!try and avoid sib
		SWAP(REG, REGIX)
		GOTO SIMPLE
FI

		am.dispsize:=4
		mode:=0
		rm:=4
		scale:=(b.scale|b.scale|1)
		base:=5
		index:=getregcode(regix, xmask)
		if regix=rstack then axerror("Scaled rstack?") fi

	else									!assume regix used; optional reg and disp
		am.dispsize:=getdispsize(b, am.offset)
		if am.dispsize then
			mode:=(am.dispsize=1|1|2)
		fi
		rm:=4

		scale:=(b.scale|b.scale|1)
!CP "SCAD"
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
			if regix=rstack and scale>1 then axerror("Can't scale rstack") fi
		fi
	fi

	if scale then
		am.sib:=scaletable[scale]<<6 + index<<3 + base
		am.usesib:=1
	fi

	if am.dispsize=4 and ismem then
		if reg or regix then
			if phighmem=2 AND ISMEM=2 then
				CPL "Addr32 can't use RIP, line",aaseqno,STRMCLSTR(CURRMCL)
			fi
		elsif phighmem then
			am.usesib:=-1
			mode:=0
			rm:=5
		fi
	fi

	am.modrm:=makemodrm(mode, opc, rm)

	return am
end

proc do_arith(mclopnd a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
	psymbol d
	int opc, dispsize
	i64 x

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
				if a.size<4 then axerror("add imm/size") fi
				genxrm(0x81, code, a)
				genopnd(b,4)
				return
			fi

			x:=b.value
			dispsize:=1
			if a.size=1 then
				opc:=0x80
!				if x not in -128..127 then axerror("Exceeding i8 range") fi
				checkimmrange(x,1)
				if x not in -128..255 then axerror("Exceeding i8/u8 range") fi
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
			axerror("ADD reg,???")
		esac

	when a_mem then
		case b.mode
		when a_reg then
			opc:=code<<3 ior (b.size=1|0x00|0x01)
			genrrm(opc, b, a)

		when a_imm then
			goto doregimm
		else
			axerror("ADD mem,???")
		esac

	else
	cpl opndnames_ma[code],=CODE
		axerror("1:Can't add to this opnd")
	esac
end

proc do_mov(mclopnd a,b)=
	int regcode, opc, dispsize
	i64 value
	psymbol d:=getdef(b)

	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			if a.size<>b.size and b.size then axerror("2:Opnd size mismatch") fi

			genrrm((a.size=1|0x8A|0x8B), a, b)

		when a_imm then
			value:=b.value

			regcode:=getregcode(a.reg, bmask)
			setopsize(a)
			if d and a.size<=2 then axerror("mov imm?") fi

			CHECKHIGHREG(A)

			case a.size
			when 1 then
				unless -128<=value<=255 then axerror("exceeding byte value") end
				genrex()
				genbyte(0xB0+regcode)
				genbyte(value)

			when 2 then
				unless -32768<=value<=65535 then axerror("exceeding u16 value") end
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
						axerror("1:exceeding u32 value")
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
			axerror("MOV REG/??")
		esac
	when a_mem then
		case b.mode
		when a_reg then

			if a.size=0 then a.size:=b.size fi
			if a.size<>b.size and a.size then axerror("3:Opnd size mismatch") fi
			genrrm((b.size=1|0x88|0x89), b, a)

		when a_imm then
			value:=b.value

			if a.size=0 then a.size:=1 fi
			if d and a.size<=2 then axerror("mov imm?") fi
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
			axerror("MOV MEM/?")
		esac
	else
		axerror("MOV ?/..")
	esac
end

proc do_push(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("pushreg not 64-bit") fi
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
			axerror("push imm value too large")
		fi

	when a_mem then
		if a.size<>8 then axerror("push not 64-bit") fi
		genxrm(0xFF, 6, a)

	else
		axerror("push opnd?")
	esac
end

proc do_pop(mclopnd a)=
	int code

	if a.size=0 then a.size:=8 fi

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("popreg not 64-bit") fi
		code:=getregcode(a.reg, bmask)
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a.size<>8 then axerror("pop not 64-bit") fi
		genxrm(0x8F, 0, a)
	else
		axerror("pop opnd?")
	esac
end

proc do_inc(mclopnd a,int code)=
!inc/dec

	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xFE|0xFF), code, a)
	else
		axerror("inc/opnd?")
	esac
end

proc do_neg(mclopnd a,int code)=
!neg/not/mul/imul/div/idiv
	case a.mode
	when a_reg, a_mem then
		genxrm((a.size=1|0xF6|0xF7), code, a)
	else
		axerror("neg/div/etc opnd?")
	esac
end

proc do_lea(mclopnd a,b)=
	int regcode, am

	unless a.mode=a_reg and b.mode=a_mem then
		axerror("LEA not reg/mem")
	end

	if a.size<4 then
CPL =A.SIZE
 axerror("LEA size error") fi

	genrrm(0x8D, a, b)
end

proc do_movsx(mclopnd a,b,int opc)=
!opc=B6 for movzx, and BE for movsx
	int am, regcode

	if a.mode<>a_reg then axerror("movsx not reg") fi

	if a.size=8 and b.size=4 then
		if opc=0xBE then
			do_movsxd(a,b)
		else						!movsx 4->8 bytes, do normal move 4->4
			a:=regtable[a.reg,4]
			do_mov(a,b)
		fi
		return
	fi

	if a.size=1 or a.size<=b.size then axerror("movsx size error") fi
	if opc=0xB6 and b.size=4 then axerror("movsx 4=>8 bytes?") fi

	case b.mode
	when a_reg then
	when a_mem then
		if b.size=0 then axerror("movsx need size prefix") fi
		if b.size=8 then axerror("movsx size 8") fi
	else
		axerror("movsx not reg/mem")
	esac

	genrrm(0x0F<<8+(b.size=1|opc|opc+1), a, b)
end

proc do_exch(mclopnd a,b)=
	int regcode, am

	if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then		!simple r0/reg
		if a.reg<>r0 then				!get a to be r0
			swap(a,b)
		fi
		if a.size<>b.size then axerror("exch size") fi

		setopsize(a)
		regcode:=getregcode(b.reg, bmask)
		genrex()
		genbyte(0x90+regcode)
		return
	fi

	if a.mode=a_mem then swap(a,b) fi

	unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then axerror("exch opnds") end
	if b.size=0 and b.mode=a_mem then b.size:=a.size fi
	if a.size<>b.size then axerror("exch size") fi

	genrrm((a.size=1|0x86|0x87), a, b)
end

proc do_movsxd(mclopnd a,b)=
	int regcode, am

	if b.mode=a_mem and b.size=0 then b.size:=4 fi

	if a.size<>8 or b.size>4 then axerror("movsxd size") fi

	if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("movsxd opnds")
	fi

	genrrm(0x63, a, b)
end

proc do_imul2(mclopnd a,b)=
	int regcode, am, opc, dispsize
	i64 value

	if a.mode<>a_reg then
		axerror("imul2 opnds")
	fi
	if b.size=0 then b.size:=a.size fi
	if a.size=1 then axerror("imul2 byte") fi

	case b.mode
	when a_reg,a_mem then
		if a.size<>b.size then axerror("imul2 size") fi

		genrrm(0x0F'AF, a, b)

	when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
		if getdef(b) then axerror("mul/label") fi
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
		axerror("imul2 opnds")
	esac
end

proc do_shift(mclopnd a,b,int code)=
	int w,opc,needdisp

	if a.mode<>a_reg and a.mode<>a_mem then axerror("shift opnds1?") fi
	if getdef(b) then axerror("shift/label") fi
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
		if b.reg<>r10 or b.size<>1 then axerror("cl or b10 needed") fi
		opc:=0xD2+w
	else
		axerror("shift opnds2?")
	esac

	genxrm(opc, code, a)

	if needdisp then genbyte(b.value); fixrip(1) fi
end

proc do_test(mclopnd a,b)=
	i64 value
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
		axerror("test opnds")
	fi

end

proc do_setcc(int cond, mclopnd b)=
!a is cond
!b is byte reg/mem

	if b.mode not in [a_reg, a_mem] or b.size>1 then axerror("setcc opnd/size") fi

	genxrm(0x0F'90+cond, 0, b)
end

proc checksize(mclopnd a, int size1=0, size2=0)=
	if a.size=0 then axerror("Need size") fi
	if size1 and a.size not in [size1,size2] then
		CPL =A.SIZE
		axerror("Wrong size")
	fi
end

proc do_arithxmm(mclopnd a,b,int prefix,opc)=
	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("arithxmm opnds")
	fi

	if prefix then genbyte(prefix) fi
	genrrm(0x0F<<8+opc, a, b)
end

proc do_logicxmm(mclopnd a,b,int opc,size)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("logicxmm opnds")
	fi

	if size=8 then genbyte(0x66) fi

	genrrm(0x0F<<8+opc, a, b)
end

proc do_convertfloat(mclopnd a,b,int prefix)=
!cvtss2sd and cvtsd2ss
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("convertfloat opnds")
	fi
	genbyte(prefix)
	nowmask:=1
	genrrm(0x0F'5A, a,b)
end

proc do_fix(mclopnd a,b,int prefix,opc)=
	int am, regcode

	if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("fix opnds")
	fi

	checksize(a, 4, 8)
	
	b.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	genrrm(0x0F<<8+opc, a, b)
end

proc do_float(mclopnd a,b,int prefix)=
!cvtss2si and cvtsd2si
	if a.mode<>a_xreg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("float opnds")
	fi

	checksize(b, 4, 8)
!
	a.size:=(prefix=0xF3|4|8)

	genbyte(prefix)
	usesizeb:=1
	genrrm(0x0F'2A, a, b)
end

proc do_movxmm(mclopnd a,b,int size)=
!do movd/movq depending on size being 4 or 8
	int am, regcode, regcode1, regcode2

	case a.mode
	when a_reg then
		case b.mode
		when a_xreg then
			if a.size<>size then axerror("1:movdq size") fi
			b.size:=a.size

			sizeoverride:=1
			genrrm(0x0F'7E, b, a)

		else
			axerror("movdq reg,?")
		esac
	when a_xreg then
		case b.mode
		when a_reg then
			a.size:=b.size
			if b.size<>size then axerror("3:movdq size") fi
			sizeoverride:=1
			genrrm(0x0F'6E, a, b)

		when a_xreg then
			a.size:=b.size
			f3override:=1
			genrrm(0x0F'7E, a, b)

		when a_mem then
			if b.size=0 then b.size:=a.size fi
!			if b.size<>size then axerror("31:movdq size") fi
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
			if a.size and a.size<>size then axerror("5:movdq size") fi

			sizeoverride:=1
			genrrm((size=4|0x0F'7E|0x0F'D6), b,a)

		else
			axerror("movdq mem,?")
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
		axerror("fixrip disp?")
	esac
	ripentry.immsize:=dispsize
end

proc do_bswap(mclopnd a)=
	int code
	if a.mode<>a_reg or a.size<4 then axerror("bswap reg>") fi

	setopsize(a)

	code:=getregcode(a.reg, bmask)

	genrex()
	genbyte(0x0F)
	genbyte(0xC8 + code)
end

proc do_movdqx(mclopnd a, b, int prefix)=
	prefix:=prefix<<16 + 0x0F<<8

	if a.size=0 then a.size:=16 fi
	if b.size=0 then b.size:=16 fi

	if a.mode=a_mem then
		genrrm(prefix+0x7F, b, a)
	else
		genrrm(prefix+0x6F, a, b)
	fi
end

proc do_dshift(mclopnd a, b, int c, opc)=

	if a.size=0 then a.size:=b.size fi
	if a.size<>b.size or a.size<=1 then axerror("dshift/size") fi

	sizeoverride:=0
	genrrm(0x0F<<8+opc, b, a)
	genbyte(c)
end
=== mc_writeexe.m 0 0 28/32 ===
!Create .exe file from SS-data (code, data, reloc and psymbol tables)

[maxplibfile]i64 libinsttable
[maxplibfile]ichar libinstnames
[maxplibfile]int libnotable			!index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
	ref basereloc nextitem
	u32 address				!virtual address
	i32 reloctype
end

ref basereloc basereloclist
int nbaserelocs
int maxrelocaddr
const maxbaseblock=500
[maxbaseblock]int blockbases
[maxbaseblock]i32 blockcounts
[maxbaseblock]i32 blockbytes
[maxbaseblock]byte blockpadding
int nbaseblocks
int basetablesize


const filealign = 512
!const filealign = 32
const sectionalign = 4096
const exe_imagebase = 0x40'0000
const dll_imagebase = 0x1000'0000

global int imagebase

int imagesize
int filesize
ref[]i64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
psymbol stentrypoint				!psymbol to be the entry point
psymbol stentrypoint2
psymbol stentrypoint3

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

global proc writeexe(ichar outfile, int dodll, ichar entrypoint=nil)=
	return when exedone

	genexe1(entrypoint, outfile, dodll)
	genexe2(outfile, dodll)

	exedone:=1
end

global proc genexe1(ichar entrypoint, outfile, int dodll)=
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

	if phighmem then
		ref riprec pr

		pr:=riplist
		while pr, pr:=pr.next do
			offsetptr:=ref u32(codeaddr+pr.offset)
			offset:=getripoffset(pr.offset, offsetptr^-imagebase, pr.immsize)
			offsetptr^:=offset	
		od
	fi
end

global proc genexe2(ichar outfile, int dodll)=
!construct the exe image in memory, then write out the file
	imagefileheader header
	optionalheader optheader
	int offset,i
	i64 aa

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

	if pverbose=2 then
		println "EXE size:  ", dataptr-datastart:"10s,jr"
		println
	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

proc loadlibs=
!load library instances
	int i
	i64 hinst
	ichar file
	[300]char filename

	for i to nplibfiles when plibfiles[i]^<>'$' do
		strcpy(&.filename, plibfiles[i])
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

	if pverbose then
		println "Code size: ", bufferlength(ss_code):"10s,jr","bytes"
		if pverbose=2 then
			println "Idata size:", bufferlength(ss_idata):"10s,jr"
			println "Code+Idata:", bufferlength(ss_code)+bufferlength(ss_idata):"10s,jr"
			println "Zdata size:", ss_zdatalen:"10s,jr"
		fi
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

func extractlibname(ichar name, int &libno,moduleno)ichar=
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

	for i:=1 to nplibfiles when libinsttable[i] do
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
	strcpy(&.str, plibfiles[n])
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
	psymbol d
	ichar name, libname, basename

	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.imported then
			if nimports>=maximports then axerror("genexe: Too many imports") fi
			++nimports
			name:=extractlibname(d.name,libno,1)
			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		elsif d.exported then
			basename:=getbasename(d.name)
			if userentrypoint then
				if eqstring(basename,userentrypoint) then
					stentrypoint:=d
				fi
			else
!				if eqstring(basename,"main") and not isdll then
				if d.isentry and not isdll then
					stentrypoint:=d
				fi
			fi

			if nexports>=maxexports then axerror("gendll: Too many exports") fi
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=getbasename(d.name)
		fi
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref u32 p32
	ref u64 p64
	psymbol d
	int offset,index,thunkoffset,iatoffset

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]
		index:=d.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset

		case r.reloctype
		when rel32_rel then
			if not d.imported then
				axerror("rel32/not imported")
			fi
			(ref u32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.imported then
				(ref u32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				u:=nil
!CPL =D.NAME, D.SEGMENT, =D
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				else
					PRINTLN D.NAME,D.SEGMENT
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
	psymbol d
	int index

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]

		case r.reloctype
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.imported then
			else

IF R.RELOCTYPE=ADDR32_REL THEN
!PRINTLN "??BASE RELOC",(D.SEGMENT=CODE_SEG|"CODE"|"DATA"),(R.RELOCTYPE=ADDR32_REL|"ADDR32"|"ADDR64")
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
!	header.subsystem:=2

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

	i64 aa
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
	ref u32 paddrtable
	ref u32 pnametable
	ref u16 pordtable
	ref char pdllname
	ref char pnames
	int addrtableoffset
	int nametableoffset
	int ordtableoffset
	int dllnameoffset
	int namesoffset
	int virtoffset
	int sectionno
	psymbol d
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

func getexporttablesize:int=
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
	
	ref u32 p32
	ref u16 p16
	int baseaddr,addr,nextblock
	ref basereloc q

	p32:=cast(pstart)

	for i to nbaseblocks when blockcounts[i] do
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
	psymbol d,e

!First, store 1..nexports into sortindex
	for i to nexports do
		sortindex[i]:=i
	od

!do bubble sort for now
	repeat
		int swapped:=0
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

func getsectionno(int segment)int=
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
	ref i64 paddr,pname
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
	ref u32 pextra

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
		if phighmem=0 then
			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x24
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(ref i32(thunkptr)^:=thunkaddr)
			thunkptr+:=4
		else					!use rip mode

			thunkptr++^:=0x48
			thunkptr++^:=0xFF
			thunkptr++^:=0x25
			thunkaddr:=imagebase+importtable[i].iatoffset
			(ref i32(thunkptr)^:=getripoffset(int(thunkptr-codebase),thunkaddr-imagebase))
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

	addr+:=sectiontable[csect].virtoffset		!now is offset rel to imagebase
	dest-(addr+4)-extra
end

=== mx_run_dummy.m 0 0 29/32 ===
global proc runlibfile(ichar filename, int cmdskip, int run)=
	abortprogram("No Run")
end

global proc writemcx(ichar filename)=
end
=== info.txt 0 1 30/32 ===
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
=== cc_help.txt 0 1 31/32 ===
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
=== mcc.h 0 1 32/32 ===
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
19 mc_decls_x.m 0 0
20 mc_lib_x.m 0 0
21 mc_asm_x.m 0 0
22 mc_gen_xb.m 0 0
23 mc_aux_xb.m 0 0
24 mc_conv_xb.m 0 0
25 mc_temp_xb.m 0 0
26 mc_objdecls.m 0 0
27 mc_genss.m 0 0
28 mc_writeexe.m 0 0
29 mx_run_dummy.m 0 0
30 info.txt 0 1
31 cc_help.txt 0 1
32 mcc.h 0 1
