import msys
import clib
import mlib
import oslib

import mm_lex
import mm_decls
import mm_support
import mm_tables
import mm_parse
import mm_lib
import mm_diags
import mm_name

import mm_type

import mm_genpcl

import mm_export

import* mm_pcl
import* pc_win64
import pci_mcl as md
import pc_libmcl

tabledata() []ichar optionnames=

	(load_sw,		"load"),
	(fixup_sw,		"fixup"),
	(parse_sw,		"parse"),
	(name_sw,		"name"),
	(type_sw,		"type"),

	(pcl_sw,		"pcl"),
	(asm_sw,		"asm"),
	(asm2_sw,		"c"),
	(obj_sw,		"obj"),
	(dll_sw,		"dll"),
	(exe_sw,		"exe"),
	(run_sw,		"run"),

	(pclexe_sw,		"pclexe"),
	(asmexe_sw,		"asmexe"),

	(sys_sw,		"sys"),
	(minsys_sw,		"minsys"),
	(nosys_sw,		"nosys"),
	(nofile_sw,		"nofile"),
	(rts_sw,		"rts"),
	(norts_sw,		"norts"),

	(debug_sw,		"debug"),

	(ma_sw,			"ma"),
	(docs_sw,		"docs"),
	(export_sw,		"exp"),

	(opt_sw,		"opt"),
	(opt1_sw,		"opt1"),
	(opt2_sw,		"opt2"),

	(ast1_sw,		"ast1"),
	(ast2_sw,		"ast2"),
	(ast3_sw,		"ast3"),
	(showpcl_sw,	"showpcl"),
	(showasm_sw,	"showasm"),
	(st_sw,			"st"),
	(pst_sw,		"pst"),
	(stflat_sw,		"stflat"),
	(types_sw,		"types"),
	(overloads_sw,	"overloads"),
	(ss_sw,			"ss"),

	(time_sw,		"time"),
	(v_sw,			"v"),
	(vv_sw,			"vv"),
	(quiet_sw,		"q"),
	(help_sw,		"h"),
	(help2_sw,		"help"),
	(ext_sw,		"ext"),
	(out_sw,		"out"),
	(outpath_sw,	"outpath"),
	(unused_sw,		"unused"),
	(set_sw,		"set"),
end

byte fpclexe
byte fasmexe

const logfile="mx.log"

[sysparams.len]ichar extraparams	!after ":"
[sysparams.len]ichar extravalues
int nextraparams=0

ichar outext=""				!for reporting of primary o/p file

const maxoptionvar=25
[maxoptionvar]ichar optionvars
[maxoptionvar]ichar optionvalues
int noptionvars

int startclock,endclock

global proc start_common(int os, target)=
	unit p,q,r
	int m,fileno,ntokens,t

	startclock:=os_clock()

	initdata(os,target)

	getinputoptions()

!	println "Passlevel:",passnames[passlevel],(prodmode|"P"|"D")
!	println =inputfiles[1]
!	println =outfile

!	if debugmode and passlevel<parse_pass then return fi
	if passlevel<parse_pass then return fi

	if fverbose>=1 then
		fprint "Compiling # to #",inputfiles[1]:"14jlp-",changeext(outfile,outext),$
!		print (msyslevel+1|" [No sys]"," [Min sys]" | " [Full sys]")

		println

	fi

	initsearchdirs()
	remove(logfile)

	do_loadmodules()

	do_parse()

	do_name()
	do_type()

	do_writema()

	do_writeexports(outfile)

	FIXSTLIST(STPROGRAM.DEFLIST)

	do_genpcl()

	if passlevel=run_pass then
		do_runprog()
	fi

	if fverbose>=2 then
		println "Finished."
	fi

	showlogfile()

	if fshowtiming then
		endclock:=os_clock()
		println "Time",endclock-startclock,"ms"
	fi
end

proc do_loadmodules=
	if fbundled then
		loadmafile()
	fi
	loadmainmodule(inputfiles[1])
end

proc do_parse=
!	if debugmode and passlevel<parse_pass then return fi
	if passlevel<parse_pass then return fi

	if fwritedocs then
		docfile:=fopen(changeext(outfile,"txt"),"w")
	fi

	for i:=2 to nmodules do
		parsemodule(i)
	od
	parsemodule(1)


	dostarimports()

	if docfile then
		fclose(docfile)
	fi

	if not debugmode or passlevel>=fixup_pass then
		fixusertypes()
	fi

	if debugmode and fshowast1 then showast("AST1") fi
end

proc do_name=
	if passlevel<name_pass then return fi

	rx_typetable()
	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)

	if debugmode and fshowast2 then showast("AST2") fi
end

proc do_type=
	if passlevel<type_pass then return fi

	tx_typetable()
		fixblockparams()

	for i:=1 to nmodules do
		tx_module(i)
	od
	tx_allprocs()

	if debugmode and fshowast3 then showast("AST3") fi
end

proc showlogfile=
	[256]char str
	filehandle logdev
	int size

	if not debugmode then return fi

	logdev:=fopen(logfile,"w")

	if fshowasm and passlevel>=mcl_pass then
		println @logdev,"PROC ASSEMBLY"
		addtolog(asmfilename,logdev)
	fi
	if fshowpcl and passlevel>=pcl_pass then
		println @logdev,"PROC PCL"
		addtolog(pclfilename,logdev)
	fi
	if fshowast3 and passlevel>=type_pass then	addtolog("AST3",logdev) fi
	if fshowast2 and passlevel>=name_pass then	addtolog("AST2",logdev) fi
	if fshowast1 and passlevel>=parse_pass then	addtolog("AST1",logdev) fi
	if fshowst then								showsttree("SYMBOL TABLE",logdev) fi
	if fshowstflat then							showstflat("FLAT SYMBOL TABLE",logdev) fi

	if fshowtypes then							printmodelist(logdev) fi

	if fshowoverloads then						printoverloads(logdev) fi

	size:=getfilesize(logdev)
	fclose(logdev)

	if size then
		print @&.str,"\\m\\olded.bat -w ",logfile

		if checkfile("mm.m") then
			os_execwait(&.str,1,nil)
		else
			println "Diagnostic outputs written to",logfile
		fi
	fi

	stop 0
end

proc initdata(int os, target)=
	pcm_init()
	lexsetup()
	initassemsymbols()
	init_tt_tables()
	initbblib()

	if os='W' then
		fwindows:=1
	else
		flinux:=1
	fi

	case target
	when 'X64' then
		fx64:=1
		if flinux then loaderror("Linux/x64") fi
!	when 'C64' then
!		fc64:=1
!		ctarget:=1
!	when 'C32' then
!		fc32:=1
!		ctarget:=1
	else
		loaderror("Bad os/target")
	esac

	addoptionvar("mm","1")
end

function loadmainmodule(ichar filespec)int=
!Used for main module. Will always be first module loaded, module list
!will be empty.
!Load file as string
!extract modulename
!call compilemodile(modulename, filename, source)
	[100]char modulename
	[300]char path
	ref byte source
	int status
	modulerec m
	int i,fileno

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

	strcpy(&.modulename,extractbasefile(filespec))
	strcpy(&.path,extractpath(filespec))
	if path[1] then
		addsearchdir(&.path)
	fi

	addmodule(&.modulename,fileno)
	addspecialtypes()

return 1
end

function addmodule(ichar modulename,int fileno)int=
!Add new module with given name
!Source for module is already loaded in <source>, of given length
!return module no just added

!return new module number, or 0
!The module entry is NOT linked to the module list until imports have been loaded.

	modulerec m
	const maximports=maxmodule
	[maximports]ichar importnames

	[0..maximports]byte importflags, importxd

	[maximports]int importmoduleno
	int nimports,i,status,k,flag,j,newmodno
	ref modulerec pmodule

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

	clear importflags

	nimports:=readimportlist(newmodno,&importnames,&importflags,&importxd,maximports)

	for i to nimports do
		flag:=0
		if fverbose=3 then
			println "Load import for",modulename,=importnames[i]
		fi
		k:=loadimport(importnames[i],importxd[i],modulename)
		pmodule.importmap[k]:=1
		pmodule.importstar[k]:=importflags[i]
		importmoduleno[i]:=k
	od
	return newmodno
end

function readimportlist(int m, ref[]ichar importnames,
	ref[0:]byte importflags, importxd, int maximports)int=
	int n,flag,xdflag
	ichar s
	[100]char name,libname
	ichar iname

	startlex("RIL",m)
	n:=0

	do
		lex()
		case lx.symbol
		when eofsym then
			exit
		when semisym,eolsym then

		when kimportsym then
			xdflag:=lx.subcode
			flag:=0
			lex()
			if lx.symbol=mulsym then
				flag:=1
				lex()
			fi

			if lx.symbol<>namesym then
				abortprogram("import: modulename expected")
			fi
			if ++n>=maximports then			!allow for extra msys module
				abortprogram("too many imports")
			fi

			iname:=mapimport(lx.symptr.name)
			importnames[n]:=pcm_copyheapstring(iname)
			importflags[n]:=flag
			importxd[n]:=xdflag

			repeat lex() until lx.symbol<>namesym

		when kimportpathsym then
			lex()
			if lx.symbol=stringconstsym then
				addsearchdir(lx.svalue)
				lex()
			else
				abortprogram("string path expected")
			fi

		when kmapmodulesym then
			domapmodule()

		else
			exit
		esac
	od

!make sure bsys is included on first module
	int needbsys,needclib
	ichar bsysname

	case msyslevel
	when 1 then
		bsysname:="msystemp"
	when 0,2 then
		bsysname:="msysp"
	esac

	if nmodules=1 then
		if msyslevel then
			needbsys:=1

			for i to n do
				if eqstring(importnames^[i],bsysname) then
						needbsys:=0
						exit
				fi
			od
		else
			needbsys:=0
		fi

		if needbsys then
			++n
			importnames^[n]:=pcm_copyheapstring(bsysname)
			importflags^[n]:=0
			importxd[n]:=0
		fi
	fi
	return n
end

function loadimport(ichar modulename,int xdflag,ichar ownername)int=
!Look at request for adding module, which might already be loaded
!Checks also that modules are added in the right order
!Calls loadmodule to process the module and deal with /its/ imports
!return modulen no of the existing or newly added module

	int i,fileno
	ichar ifilespec
	[300]char filespec
	ref char source
	ichar newname

	newname:=modulename

	for i:=1 to nmodules do
		if eqstring(moduletable[i].name,newname) then		!already loaded
			return i
		fi
	od

	fileno:=getmodulefile(modulename,ownername,xdflag)

	if xdflag then
		addcclib(modulename)
	fi

	return addmodule(newname,fileno)
end

proc initsearchdirs=
	[300]char str1,str2
	int i

	nsearchdirs:=0
	addsearchdir("c:/mx/")
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

proc getinputoptions=
	const slash='-'
	int i,j,k
	int paramno,pmtype,sw,ncolons,passfixed
	ichar name,value,filename,ext
	[300]char filespec

	prodmode:=1
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

	if prodmode=debugmode=0 then
		passlevel:=exe_pass
		outext:="exe"
		prodmode:=1
	elsif prodmode and passlevel=0 then
		passlevel:=exe_pass
		outext:="exe"
	elsif debugmode and passlevel=0 then
		passlevel:=mcl_pass
		outext:="asm"
	fi

	if msyslevel=-1 then
		msyslevel:=(prodmode|2|0)
		msyslevel:=(prodmode|2|0)
	fi

	if ninputfiles=0 then
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

!default output
		outfile:=pcm_copyheapstring(filename)

		if destfilename then
			outfile:=destfilename
		elsif destfilepath then
			strcpy(&.filespec,destfilepath)
			strcat(extractfile(&.filespec), outfile)
			outfile:=pcm_copyheapstring(&.filespec)	
		fi
	else
		loaderror("Specify one lead module only")
	fi

	case msyslevel
	when 1 then
		addmodulemapping("msys","msystemp")
	when 0,2 then	
		addmodulemapping("msys","msysp")
	esac

	asmfilename:=getoutfilename(outfile,"asm")
	pclfilename:=getoutfilename(outfile,"pcl")
	exefilename:=getoutfilename(outfile,"exe")
	dllfilename:=getoutfilename(outfile,"dll")
	objfilename:=getoutfilename(outfile,"obj")
	mafilename:=getoutfilename(outfile,"ma")
	expfilename:=getoutfilename(outfile,"exp")

end

proc do_option(int sw, ichar value)=
	static byte outused, outpathused

	switch sw
	when load_sw then passlevel:=load_pass
	when parse_sw then passlevel:=parse_pass
	when fixup_sw then passlevel:=fixup_pass
	when name_sw then passlevel:=name_pass
	when type_sw then passlevel:=type_pass
	when pcl_sw then passlevel:=pcl_pass; outext:="pcl"
	when asm_sw then passlevel:=asm_pass; outext:="asm"
	when obj_sw then passlevel:=obj_pass; outext:="obj"
	when exe_sw then passlevel:=exe_pass; outext:="exe"
	when dll_sw then passlevel:=dll_pass; outext:="dll"
	when run_sw then passlevel:=run_pass; outext:="exe"
	when pclexe_sw then passlevel:=pcl_pass; outext:="pcl"; fpclexe:=1
	when asmexe_sw then passlevel:=asm_pass; outext:="asm"; fasmexe:=1

	when ma_sw then fwritema:=1; outext:="ma"
	when export_sw then fwriteexports:=1
	when docs_sw then fwritedocs:=1

	when sys_sw then msyslevel:=2
	when minsys_sw then msyslevel:=1
	when nosys_sw then msyslevel:=0
	when nofile_sw then fnofile:=1
!	when rts_sw then fdorts:=1
	when norts_sw then fdorts:=0

	when opt_sw then foptim:=2
	when opt1_sw then foptim:=1
	when opt2_sw then foptim:=2

	when debug_sw then debugmode:=1; prodmode:=0

	when time_sw then fshowtiming:=1

	when v_sw then fverbose:=2

	when vv_sw then fverbose:=3

	when quiet_sw then fverbose:=0

	when help_sw,help2_sw then showhelp(); stop

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

	when ast1_sw then fshowast1:=1
	when ast2_sw then fshowast2:=1
	when ast3_sw then fshowast3:=1
	when showpcl_sw then fshowpcl:=1
	when showasm_sw then fshowasm:=1
	when st_sw then fshowst:=1
	when pst_sw then fshowpst:=1
	when stflat_sw then fshowstflat:=1
	when types_sw then fshowtypes:=1
	when overloads_sw then fshowoverloads:=1
	when ss_sw then fshowss:=1

!	when windows_sw then fwindows:=1
!	when linux_sw then flinux:=1
!	when x64_sw then fx64:=1
!	when c64_sw then fc64:=1
!	when c32_sw then fc32:=1

	endswitch

end

proc showcaption=
	println "M Compiler [PCL]", $date, $time
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

global proc showhelp=
	static ichar helptext=strinclude("mm_help.txt")
	println helptext
end

proc showast(ichar filename)=
	filehandle f

	f:=fopen(filename,"w")
	return unless f

	println @f,"PROC",filename
	printcode(f,"")
	println @f
	fclose(f)
end

proc domapmodule=
!working in prescan using lx; have just seen "mapmodule"
	[256]char genname, actualname,optionname,valuename
	int cond,option

	getpsname(&.genname)

	if lx.symbol<>sendtosym then abortprogram("=> expected") fi

	getpsname(&.actualname)

	cond:=0

	if lx.symbol=kwhensym then
		getpsname(&.optionname)
		if lx.symbol=eqsym then
			getpsname(&.valuename)
		else
			strcpy(&.valuename,"1")
		fi
		cond:=1
	fi

	while lx.symbol not in [semisym,eofsym] do lex() od

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

proc getpsname(ichar dest)=
!read raw name or string from prescan code from next symbol
!store string into dest, and move to next symbol
!	[64]char str

	lex()	
	case lx.symbol
	when namesym then
		strcpy(dest,lx.symptr.name)
	when stringconstsym then
		strcpy(dest,lx.svalue)
	when intconstsym then
		lx.svalue:=strint(int(lx.value))

	else
		abortprogram("map1")
	esac
	lex()
end

global proc addmodulemapping(ichar old, newx, optionname=nil, valuename=nil)=
	int option

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
	actualmodules[nmodulemap]:=pcm_copyheapstring(newx)
end

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

function findoptionvar(ichar name)int=
	for i to noptionvars do
		if eqstring(name, optionvars[i]) then
			return i
		fi
	od
	return 0
end

global proc initassemsymbols=
!initialise hash table from kwddata
	[32]char str
	int i

	for i to md.mclnames.len do
		addreservedword(md.mclnames[i]+2,asmopcodesym,i)
	od

	for i to md.dregnames.len do
		addreservedword(md.dregnames[i],regsym,md.regindices[i],md.regsizes[i])
	od


	for i to md.xmmregnames.len do
		addreservedword(md.xmmregnames[i],xregsym,i)
	od

	for i to md.fregnames.len do
		addreservedword(md.fregnames[i],fregsym,i)
	od

	for i to md.mregnames.len do
		addreservedword(md.mregnames[i],mregsym,i)
	od

	for i to md.jmpccnames.len do
		addreservedword(md.jmpccnames[i],jmpccsym,md.jmpcccodes[i])
	od

	for i to md.setccnames.len do
		addreservedword(md.setccnames[i],setccsym,md.setcccodes[i])
	od

	for i to md.cmovccnames.len do
		addreservedword(md.cmovccnames[i],movccsym,md.cmovcccodes[i])
	od

!for i to segmentnames.len do
	for i to md.segmentnames.upb do
		strcpy(&.str,md.segmentnames[i])
		str[strlen(&.str)-3]:=0
		addreservedword(pcm_copyheapstring(&.str),segnamesym,i)
	od

	addreservedword("aframe",regsym,md.r14,4)
	addreservedword("dframe",regsym,md.r14,8)
	addreservedword("astack",regsym,md.r15,4)
	addreservedword("dstack",regsym,md.r15,8)
	addreservedword("dprog",regsym,md.r8,8)
	addreservedword("dsptr",regsym,md.r9,8)
end

proc showmodules=

	println "Modules:",nmodules
	for i to nmodules do
!	print moduletable[i].name, sourcefilepaths[moduletable[i].fileno],$
		println moduletable[i].name, sourcefilepaths[moduletable[i].fileno],$

		print "                 "
		for k:=1 to nmodules do
			fprint "# ",moduletable[i].importmap[k]
		od
		println

		for k:=1 to nmodules when moduletable[i].importmap[k] do
			println "		",moduletable[k].name
		od

		println
	od
end

proc do_writema=
	if fwritema then
		if fbundled then
			loaderror("-ma used with .ma input")
		fi
		writemafile(inputfiles[1],mafilename)
		stop
	fi
end

proc do_writeexports(ichar expfile)=
	[300]char str

	if not fwriteexports and passlevel<>dll_pass then
		return
	fi

	writeexports(expfilename,moduletable[1].name)
	if fwriteexports then
		stop
	fi
end

proc do_runprog=
	[300]char str
	int i

	strcpy(&.str,outfile)

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

proc dostarimports=
!fixup up import*
	ref modulerec pm
	int m, star

	for i to nmodules do
		for k to nmodules when moduletable[i].importmap[k] and moduletable[i].importstar[k] do
			pm:=&moduletable[k]
			for j to nmodules when pm.importmap[j] do
				moduletable[i].importmap[j]:=1
			od
			m:=moduletable[i].importmap[k]
			star:=moduletable[i].importstar[k]
		od
	od

end

proc fixstlist(symbol d)=

	while d, d:=d.nextdef do
		fixst(d)
	od

end

proc fixst(symbol d)=
	unit p

	d.fwdrefs:=nil					!shares with deflistx, no longer needed
!
	d.iscallback:=d.fflang=callbackff
	if d.atvar then
		p:=d.equivvar
		if p.tag=j_addrof then p:=p.a fi
		if p.tag<>j_name then serror("FIXST@") fi
		p.def.isequivtarget:=1
	fi

	if ttbasetype[d.mode] in [tarray,trecord] or d.isequivtarget or d.atvar then
		d.noreg:=1
	fi

	fixstlist(d.deflist)

end

function getoutfilename(ichar file,ext)ichar=
	return pcm_copyheapstring(changeext(file,ext))
end

proc do_genpcl=
	[256]char str

	if passlevel<pcl_pass then return fi

	if passlevel=pcl_pass then
		codegen_pcl(rts:0)
	ELSIF FDORTS=0 THEN
		PRINTLN "----NO RTS"
		codegen_pcl(rts:0)
	else
		codegen_pcl(rts:1)
	fi

	if (passlevel=pcl_pass and not fnofile) or (debugmode and fshowpcl) then
		pcl_writepclfile(pclfilename)
	fi

	case passlevel
	when clang_pass then 
		loaderror("-Clang not ready")
	when pcl_pass then

		if fpclexe then			!invoke PC on resulting .pcl
			fprint @str,f"\mxp\pc -exe -rts #",pclfilename
			println "Running:",str
			stop system(str)
		fi
		return
	esac

!assume x64 target here
	pcl_setasmhandler(domcl_assem)

	pcl_genmcl(foptim)
	if (passlevel=asm_pass and not fnofile) or (debugmode and fshowasm) then
		pcl_writeasmfile(asmfilename)
	fi

	case passlevel
	when asm_pass then
		if fasmexe then			!invoke PC on resulting .pcl
			fprint @str,f"aa -exe #",asmfilename
			println "Running:",str
			stop system(str)
		fi
	when exe_pass then
		pcl_writeexefile(exefilename,foptim)
	when dll_pass then
		pcl_writedllfile(dllfilename,foptim)
	when run_pass then
		loaderror("CAN'T DO RUNEXE")
	esac
end

