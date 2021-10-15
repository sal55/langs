mapmodule cc_headers => cc_headersx

import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_lex
import cc_tables
import cc_lib
import cc_parse
import cc_headers
import cc_genpcl
import cc_export

import* cc_pcl

tabledata() []ichar modenames =
	(preprocess_mode,	$),
	(compile_mode,		$),
	(link_mode,			$),
	(run_mode,			$),
end

int cc_mode				!one of the above, default is link_mode
ichar linkoption
ichar destfilename		!base file
ichar destfileext		!extension

byte fdebugcompiler
byte fshowpaths
byte fshowheaders
byte fwriteheaders
byte fgetlib
byte fshowinfo
byte fstdin
byte fstdout
byte fmheaders
byte fshowlog
byte fatfile
byte fshowtiming
byte fbcclib
byte fshowast
byte fshowpcl
byte fshowmcl
byte fshowst
byte fshowstflat

ichar entrypointname

tabledata() []ichar optionnames=
	(e_sw,			"e"),
	(compile_sw,	"c"),
	(toassem_sw,	"s"),
	(obj_sw,		"obj"),
	(exe_sw,		"exe"),
	(run_sw,		"run"),

	(paths_sw,		"paths"),
	(headers_sw,	"headers"),
	(stdin_sw,		"stdin"),
	(stdout_sw,		"stdout"),

	(inclpath_sw,	"i"),
	(showincl_sw,	"includes"),

	(time_sw,		"time"),
	(v_sw,			"v"),
	(v2_sw,			"v2"),
	(quiet_sw,		"q"),
	(help_sw,		"h"),
	(help2_sw,		"help"),
	(info_sw,		"info"),
	(ext_sw,		"ext"),
	(writeheaders_sw,	"writeheaders"),
	(old_sw,		"old"),
	(getlib_sw,		"getlib"),
	(mheaders_sw,	"mheaders"),
	(automodules_sw,"auto"),
	(out_sw,		"out"),
	(at_sw,			"at"),

	(debug_sw,		"debug"),
	(showst_sw,		"showst"),
	(showstflat_sw,	"showstflat"),
	(showast_sw,	"showast"),
	(showpcl_sw,	"showpcl"),
	(showmcl_sw,	"showmcl"),

	(bcclib_sw,		"bcclib"),
	(callback_sw,	"callback"),
	(entry_sw,		"entry"),
	(splicing_sw,	"splicing"),
	(source_sw,		"source"),
end

const logfile="bcc.log"

int totallines=0
int nstringobjects=0

[sysparams.len]ichar extraparams	!after ":"
[sysparams.len]ichar extravalues
int nextraparams=0

global int progstart,progend

proc start=
	int pass
	int nextmodule
	ichar file
	int i
	ichar x

!CPL =STREC.BYTES

	starttiming()

	initdata()

	getinputoptions()
	initsearchdirs()
	SHOWSEARCHDIRS()

	if fdebugcompiler then
		debugcompile()
		stop
	fi

	nextmodule:=1
	pass:=1

	repeat
		compilemodules(nextmodule,ninputfiles,pass++)

		nextmodule:=addnewmodules()

	until nextmodule=0

!	if cc_mode>=link_mode then
!	fi

	if cc_mode=run_mode then
		do_runprog()
	fi

	if fshowtiming then showtiming() fi
	if fmheaders then writemheader(inputfiles[1]) fi

	if fatfile then
		writeatfile()
	fi

	stop 0
end

proc compilemodules(int a,b,pass)=
	[256]char str
	ichar ext

	if destfileext=nil then
		if cc_mode=preprocess_mode then
			ext:="i"
		elsif not fautomodules then
			ext:="pcl"
			if ninputfiles=1 and cc_mode>=link_mode then
				ext:=linkoption
			fi
		else
			ext:="pcl"
		fi
	else
		ext:=destfileext
	fi

	for m:=a to b do

		if fautomodules then
			fprint @&.str,"## Compiling # to # (Pass #)",
				(pass=1|"  "|"* "), m:"2",inputfiles[m]:"jl12",
				changeext(inputfiles[m],ext):"jl16", pass

			println &.str
		elsif not fquiet then
			if cc_mode=preprocess_mode then
				println "Preprocessing",inputfiles[m],"to",changeext(inputfiles[m],"i")
			elsif ninputfiles=1 then
				println "Compiling",inputfiles[m],"to",changeext(destfilename,ext)
			else
				println "Compiling",inputfiles[m],"to",changeext(inputfiles[m],"pcl")
			fi
		fi

		do_loadmodule(m)

		if cc_mode=preprocess_mode then
			do_preprocess(m)
			resetcompiler()
			next
		fi

		do_parsemodule(m)

		if not fatfile then
			do_genpcl(m)
		fi

		if not fmheaders then
			resetcompiler()
		fi
	od
end

proc debugcompile=
	int pass
	int nextmodule
	ichar file

!	PRINTLN "DEBUG COMPILE"

	do_loadmodule(1)

	do_parsemodule(1)
	showast(1)

	do_genpcl(1)
	showpcl()

!	showsttree(1)
!	showstflat()
!
!	printmodelist(logdev)
!
!	do_genlink()

	closelogfile()
end

proc do_loadmodule(int n)=
	if fverbose then
		CPL "Loading:",inputfiles[n]
	fi

	loadmainmodule(inputfiles[n])
end

proc do_preprocess(int n)=
	lex_preprocess_only(inputfiles[n],1, n, fstdout)
end

proc do_parsemodule(int n)=
	if fverbose then
		CPL "Parsing:"
	fi
	parsemodule(n)
end

proc do_genpcl(int n)=
	codegen_pcl(n)
end

proc do_runprog=
	[300]char str

	strcpy(&.str,destfilename)
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
	[100]char modulename
	[300]char path
	int status
	modulerec m
	int i,flag,fileno

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

	if not checkfile(filespec) and not fstdin then					!don't use searchdirs
		loaderror("Can't load main module: %s",filespec)
	fi

	if fstdin then
		fileno:=loadfromstdin(filespec)
	else
		fileno:=loadsourcefile(filespec,filespec)
	fi
	strcpy(&.modulename,extractbasefile(filespec))

	strcpy(&.path,extractpath(filespec))
	if path[1] then
		++nsearchdirs
		for i:=nsearchdirs downto 2 do
			searchdirs[i]:=searchdirs[i-1]
		od
		searchdirs[1]:=ref char(pcm_copyheapstring(&.path))
	fi

	addmodule(&.modulename,fileno,moduleid)

	return 1
end

function addmodule(ichar modulename,int fileno,id)int=
!Add new module with given name
!Source for module is already loaded in <fileno>
!return module no just added

!return new module number, or 0
!The module entry is NOT linked to the module list until imports have been loaded.

	modulerec m
	int i,status,k,flag,j
	ref modulerec pmodule

	pcm_clearmem(&m,m.bytes)

	m.name:=pcm_copyheapstring(modulename)
	m.fileno:=fileno

	stmodule:=createnewmoduledef(stprogram,addnamestr(m.name))
	m.stmodule:=stmodule

	if nmodules>=maxmodule then
		loaderror("Too many modules %s",modulename)
	fi

	pmodule:=&moduletable[++nmodules]
	pmodule^:=m
	m.stmodule.attribs.ax_moduleno:=nmodules

	if nmodules>=maxmodule then
		loaderror("Too many modules %s",modulename)
	fi

	return nmodules
end

proc closelogfile=			!CLOSELOGFILE
	[100]char str
	int pos

	if dologging then

		logdev:=fopen(logfile,"w")

		if fshowpcl then addtolog("OUT.PCL",logdev) fi
		if fshowast then addtolog("AST",logdev) fi
		if fshowst then addtolog("ST",logdev) fi
		if fshowstflat then addtolog("STFLAT",logdev) fi

		fclose(cast(logdev))

		print @&.str,"\\m\\med.bat",logfile

		if checkfile("cc.m") then
			os_execwait(&.str,1,nil)
		else
			println "Diagnostic outputs written to",logfile
		fi

	fi
end

proc initdata=
	pcm_init()
	lexsetup()
	inittypetables()
	initcclib()

	checkbcclib()
end

proc initsearchdirs=
	[300]char str1,str2
	int i

	searchdirs[++nsearchdirs]:=""
!	CPL =NSEARCHDIRS,=DOINTHEADERS
	if dointheaders=0 or not hasintlibs() then
		searchdirs[++nsearchdirs]:="/cxp/headers/"
	fi

	for i to nincludepaths when includepaths[i]^ do
		searchdirs[++nsearchdirs]:=includepaths[i]
	od
end

proc showsearchdirs=
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

proc showast(int n)=		!SHOWAST
	if fshowast then
		logdev:=fopen("AST","w")
		printcode(logdev,"PROC AST",n)
		println @logdev
		fclose(logdev)
	fi
end

proc showstflat=		!SHOWSTFLAT
	if fshowstflat then
		logdev:=fopen("STFLAT","w")
		println @logdev,"PROC STFLAT"
		printstflat(logdev)
		println @logdev
		fclose(logdev)
	fi
end

proc showsttree(int n)=		!SHOWSTTREE
	if fshowst then
		logdev:=fopen("ST","w")
		println @logdev,"PROC ST"
		printst(logdev,moduletable[n].stmodule)
		println @logdev
		fclose(logdev)
	fi
end

proc showpcl=			!SHOWPCL
	if fshowpcl then
		pcl_writepclfile("OUT.PCL")
	fi
end

proc showfiles=
	println "Sourcefiles:"

	for i:=1 to nsourcefiles do
		println i,":",sourcefilepaths[i],sourcefilenames[i],"Size:",sourcefilesizes[i]
	od
	println
end

proc starttiming=
	progstart:=os_clock()
end

proc showtiming=
	cpl os_clock()-progstart
end

proc getinputoptions=
	const slash='-'
	int i,j,k
	int paramno,pmtype,sw,ncolons
	ichar name,value,ext

	paramno:=2
	ncolons:=0

	do

	pmtype:=nextcmdparam(paramno,name,value,".c")
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
		if ninputfiles>=maxmodule then
			loaderror("Too many input files")
		fi
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
			recase pm_extra
!			goto doextra
		fi
	when pm_extra then
	doextra::
		extraparams[++nextraparams]:=pcm_copyheapstring(name)
		extravalues[nextraparams]:=pcm_copyheapstring(value)
	when 0 then
		exit
	esac
	od

	if cc_mode=0 then cc_mode:=link_mode fi
	if linkoption=nil then linkoption:="exe" fi

	if ninputfiles=0 and not fwriteheaders and not fgetlib then
		showcaption()
		println "Usage:"
		println "	",,sysparams[1],"prog[.c]                 # Compile prog.c to prog.exe"
		println "	",,sysparams[1],"-help                    # Show options"
		println "	",,sysparams[1],"-info                    # Further info"
		stop 22
	fi

	if fwriteheaders then
		writeheaders()
		stop 20
	fi

	ext:=linkoption
	case cc_mode
	when preprocess_mode then ext:="i"
	when compile_mode then ext:="pcl"
	else ext:=linkoption
	esac

	if destfilename=nil then
		destfilename:=pcm_copyheapstring(changeext(inputfiles[1],ext))
	elsif eqstring(destfileext,"") then
		destfileext:=ext
	fi

	if fmheaders and ninputfiles>1 then
		loaderror("-mheaders works on one file only")
	fi

	dologging:=fshowast or fshowpcl or fshowmcl or fshowst or fshowstflat

end

proc do_option(int sw, ichar value)=
	[300]char str
	int length

	case sw

	when obj_sw then
		linkoption:="obj"
		cc_mode:=link_mode

	when exe_sw then
		linkoption:="exe"
		cc_mode:=link_mode

	when e_sw then
		cc_mode:=preprocess_mode

	when compile_sw then
		linkoption:="obj"
		cc_mode:=link_mode

	when toassem_sw then
		cc_mode:=compile_mode

	when run_sw then
		cc_mode:=run_mode

	when paths_sw then
		fshowpaths:=1

	when headers_sw then
		fshowheaders:=1

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
!		loaderror("Path should end with / or \\: %s",value)
		esac

		includepaths[++nincludepaths]:=pcm_copyheapstring(value)

	when v_sw then
		fverbose:=1
	when v2_sw then
		fverbose:=1

	when quiet_sw then
		fquiet:=1

	when help_sw,help2_sw then
		showhelp()

	when info_sw then
		showextrainfo()

	when ext_sw then
		dointheaders:=0

	when writeheaders_sw then
		fwriteheaders:=1

	when getlib_sw then
		fgetlib:=1

	when old_sw then
		fmodern:=0

	when stdin_sw then
		fstdin:=1

	when stdout_sw then
		fstdout:=1

	when showincl_sw then
		fshowincludes:=1

	when mheaders_sw then
		fmheaders:=1
		cc_mode:=compile_mode

	when automodules_sw then
		fautomodules:=1

	when out_sw then
		destfilename:=pcm_copyheapstring(value)
		destfileext:=pcm_copyheapstring(extractext(value))

	when at_sw then
		fatfile:=1

	when time_sw then
		fshowtiming:=1

	when debug_sw then
		fdebugcompiler:=1

	when bcclib_sw then
		fbcclib:=1

	when entry_sw then
		entrypointname:=pcm_copyheapstring(value)

	when splicing_sw then
		flinesplicing:=1

	when showast_sw then
		fshowast:=1

	when showst_sw then
		fshowst:=1

	when showstflat_sw then
		fshowstflat:=1

	when showpcl_sw then
		fshowpcl:=1

	when showmcl_sw then
		fshowmcl:=1

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
	println strinclude "help.txt"

	stop 23
end

proc showextrainfo=
	static ichar infotext=strinclude "info.txt"

	println infotext

	stop 24
end

proc showcaption=
	println "BCC 'C' Compiler",$date,$time
end

proc resetcompiler=
!println "Reset compiler"
	freehashtable()

	ntypes:=ntypesreset
	stprogram:=stmodule:=nil
	currblockno:=nextblockno:=blocklevel:=0
	autotypeno:=0
	nextafindex:=0
	labelno:=0

!can't free fully at present as I don't know exact allocation size
!free only blocks allocated with malloc
	for i:=1 to nsourcefiles do
		if sourcefilesizes[i]>maxblocksize then
			free(sourcefiletext[i])
		fi
	od

	nsourcefiles:=0

	pcm_clearmem(&ttnamedef,ttnamedef.bytes)
	pcm_clearmem(&ttbasetype,ttbasetype.bytes)
	pcm_clearmem(&ttlength,ttlength.bytes)
	pcm_clearmem(&ttconst,ttconst.bytes)
	pcm_clearmem(&ttrestrict,ttrestrict.bytes)
	pcm_clearmem(&ttvolatile,ttvolatile.bytes)
	pcm_clearmem(&ttusertype,ttusertype.bytes)
	pcm_clearmem(&tttarget,tttarget.bytes)
	pcm_clearmem(&ttreftype,ttreftype.bytes)
	pcm_clearmem(&ttconsttype,ttconsttype.bytes)
	pcm_clearmem(&ttsize,ttsize.bytes)
	pcm_clearmem(&ttbitwidth,ttbitwidth.bytes)
	pcm_clearmem(&ttisref,ttisref.bytes)
	pcm_clearmem(&ttparams,ttparams.bytes)
	pcm_clearmem(&tttypedef,tttypedef.bytes)

	inittypetables()
end

function addnewmodules:int=
!see if any more modules have been discovered
!if so, add to inputfiles, increment ninputfile, and return
! old ninputfiles+1 (next file to compile)
!otherwise return 0 
	int nextinputfile,n,newfile

	nextinputfile:=ninputfiles+1
	n:=0

	for i:=1 to nautomodules do
		newfile:=1
		for j:=1 to ninputfiles do
			if eqstring(inputfiles[j],automodulenames[i]) then
				newfile:=0
				exit
			fi
		od
		if newfile then
			inputfiles[++ninputfiles]:=automodulenames[i]
			++n
		fi
	od

	nautomodules:=0

	if n=0 then return 0 fi
	return nextinputfile
end

proc writeatfile=
	filehandle f
	ichar file

	f:=fopen(file:=changeext(destfilename,""),"w")
	return unless f

	println "Writing @ file",file

	for i to ninputfiles do
		println @f,"	",,inputfiles[i]
	od
	for i to nlibfiles do
		println @f,"	",,libfiles[i]
	od

	fclose(f)
end

global proc addtolog(ichar filename, filehandle logdest)=
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

