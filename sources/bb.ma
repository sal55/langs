mafile 18
  1 bb.m               19684      804   0
  2 clibnew.m           3397    20512   0
  3 mlib.m             26695    23930   0
  4 msysnew.m          46919    50649   0
  5 oswindows.m        12536    97594   0
  6 bb_lex.m           41054   110153   0
  7 bb_decls.m         10885   151232   0
  8 bb_tables.m        43486   162143   0
  9 bb_support.m       13695   205656   0
 10 bb_lib.m           41476   219375   0
 11 bb_diags.m         14660   260877   0
 12 bb_mcldecls.m      13305   275566   0
 13 bb_parse.m         89673   288897   0
 14 bb_name.m          17855   378595   0
 15 bb_type.m          65736   396475   0
 16 bb_genpcl.m         9614   462238   0
 17 bb_libpcl.m        24449   471879   0
 18 bb_blockpcl.m      68880   496357   0
=== bb.m 1/18 ===
import clib
import mlib
import oslib
import bb_lex
import bb_decls
import bb_support
import bb_tables
import bb_parse
import bb_lib
import bb_diags
import bb_mcldecls
import bb_name
import bb_type
import bb_genpcl
import bb_libpcl


tabledata() []ichar optionnames=

	(prod_sw,		"prod"),
	(debug_sw,		"debug"),

	(compile_sw,	"c"),			!compile only to .asm
	(coff_sw,		"coff"),		!compile and generate .obj (default)
	(build_sw,		"build"),		!compile and generate .exe (default)
	(run_sw,		"run"),			!for .exe target, also run the result

	(load_sw,		"load"),
	(parse_sw,		"parse"),
	(fixup_sw,		"fixup"),
	(name_sw,		"name"),
	(type_sw,		"type"),
	(pcl_sw,		"pcl"),
	(mcl_sw,		"mcl"),
	(asm_sw,		"asm"),
	(obj_sw,		"obj"),
	(exe_sw,		"exe"),

	(ba_sw,			"ba"),
	(opt_sw,		"opt"),			!optimise

	(ast1_sw,		"ast1"),
	(ast2_sw,		"ast2"),
	(ast3_sw,		"ast3"),
	(showpcl_sw,	"showpcl"),
	(showmcl_sw,	"showmcl"),
	(st_sw,			"st"),
	(stflat_sw,		"stflat"),
	(types_sw,		"types"),
	(overloads_sw,	"overloads"),

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
	(set_sw,		"set"),
!	(writelibs_sw,	"writelibs")
end

const logfile="bx.log"

const maxoptionvar=25
[maxoptionvar]ichar optionvars
[maxoptionvar]ichar optionvalues
int noptionvars

proc start=
unit p,q,r
int m,fileno,ntokens,t

initdata()

getinputoptions()

!showoptions()

initsearchdirs()
remove(logfile)

t:=clock()
loadmainmodule(inputfiles[1])

!showmodules()

do_parse()

do_name()

do_type()

do_genpcl()

!t:=clock()
!
!CPL "PARSETIME=",T,=nalllines
showlogfile()

CPL
end

proc do_parse=

	if debugmode and passlevel<parse_pass then return fi

	for i:=2 to nmodules do
		parsemodule(i)
	od
	parsemodule(1)

	if not debugmode or passlevel>=fixup_pass then fixusertypes() fi

	if debugmode and fshowast1 then showast("AST1") fi
end

proc do_name=
	if debugmode and passlevel<name_pass then return fi

	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)

	if debugmode and fshowast2 then showast("AST2") fi
end

proc do_type=
	if debugmode and passlevel<type_pass then return fi

	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od

	if debugmode and fshowast3 then showast("AST3") fi
end

proc do_genpcl=
	if debugmode and passlevel<pcl_pass then return fi

	codegen_pcl()

	if debugmode and fshowpcl then showpcl("PCL") fi
end

proc showlogfile=
[256]char str
filehandle logdev
int show

if not debugmode then return fi

show:=0

!CPL "SHOWLOGFILE1",=FSHOWAST1
logdev:=fopen(logfile,"w")

if fshowasm and  passlevel=asm_pass then	show:=1; addtolog(outfilesource,logdev) fi
!if fshowss and   passlevel=exe_pass then	show:=1; addtolog("SS",logdev) fi
if fshowmcl and  passlevel>=mcl_pass then	show:=1; addtolog("MCL",logdev) fi
if fshowpcl and  passlevel>=pcl_pass then	show:=1; addtolog("PCL",logdev) fi

if fshowast3 and passlevel>=type_pass then	show:=1; addtolog("AST3",logdev) fi
if fshowast2 and passlevel>=name_pass then	show:=1; addtolog("AST2",logdev) fi
if fshowast1 and passlevel>=parse_pass then	show:=1; addtolog("AST1",logdev) fi
if fshowst then								show:=1; showsttree("SYMBOL TABLE",logdev) fi
if fshowstflat then							show:=1; showstflat("FLAT SYMBOL TABLE",logdev) fi
if fshowtypes then							show:=1; printmodelist(logdev) fi
if fshowoverloads then						show:=1; printoverloads(logdev) fi

fclose(logdev)

if show then

!	sprintf(&.str,"\\m\\med.bat %s",logfile)
!	print @&.str,"\\m\\med.bat",logfile
	print @&.str,"\\m\\ed.bat -w ",logfile

	if checkfile("bb.m") then
		os_execwait(&.str,1,nil)
!		os_execcmd(&.str,1)
	else
		println "Diagnostic outputs written to",logfile
	fi
fi

stop 0
end

global proc showpcl(ichar filename)=
	ref strbuffer pclstr

	gs_init(dest)
	pclstr:=writepclcode(filename)

	writegsfile(filename,pclstr)
end

proc initdata=
	pcm_init()
	lexsetup()
	initassemsymbols()
	inittypetables()
	initbblib()
!
	addmodulemapping("oslib","oswindows")
	addmodulemapping("osdll","oswindll")

	addmodulemapping("msys","bsys")
	addmodulemapping("mlib","blib")
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

	fileno:=getmainfile(filespec)

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

modulerec m
const maximports=maxmodule
[maximports]ichar importnames
[0..maximports]byte importflags
[maximports]int importmoduleno
int nimports,i,status,k,flag,j,newmodno
ref modulerec pmodule

convlcstring(modulename)

!CPL "ADDMODULE:",MODULENAME
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

tokenisemodule(newmodno)


!CPL "ADD1",=NEWMODNO
nimports:=readimportlist(newmodno,&importnames,&importflags,maximports)
!CPL "ADD2",NIMPORTS,,":"
!for i to nimports do
!	CPL I,":",IMPORTNAMES[I]
!OD
!STOP

for i to nimports do
	flag:=0
	if fverbose=2 then
		println "Load import for",modulename,=importnames[i]
	fi
!CPL "///CALLING LOADIMPORT",I,IMPORTNAMES[I]
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

function readimportlist(int m, ref[]ichar importnames,
							ref[0:]byte importflags, int maximports)int=
int n,flag,exportflag,i
ichar s
[100]char name,libname
ichar iname

starttkscan(m)

exportflag:=0

n:=0
do
	lex()
	case lx.symbol
	when eofsym then
		exit
	when semisym,eolsym then

	when kimportsym then
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
		importnames^[n]:=pcm_copyheapstring(iname)
		importflags^[n]:=flag

		repeat lex() until lx.symbol<>namesym

!perform mapping of name

	when kimportpathsym then
		lex()
		if lx.symbol=stringconstsym then
			addsearchdir(lx.svalue)
			lex()
		else
			abortprogram("string path expected")
		fi

	when kmapmodulesym then
PS("BEFORE MAP")
		domapmodule()
PS("AFTER MAP")

	else
		exit
	esac
od

!make sure bsys is included on first module
int needbsys
ichar bsysname

bsysname:="bsys"

if nmodules=1 then
	needbsys:=1

	for i to n do
		if eqstring(importnames^[i],bsysname) then
				needbsys:=0
				exit
		fi
	od

	if fnobsys then needbsys:=0 fi

	if needbsys then
		++n
		importnames^[n]:=pcm_copyheapstring(bsysname)
		importflags^[n]:=0
!		++n
!		importnames^[n]:=pcm_copyheapstring("bvar")
!		importflags^[n]:=0
	fi
fi
importflags^[0]:=exportflag

return n
end

function loadimport(ichar modulename,int &exportflag,ichar ownername)int=
!Look at request for adding module, which might already be loaded
!Checks also that modules are added in the right order
!Calls loadmodule to process the module and deal with /its/ imports
!return modulen no of the existing or newly added module

int i,fileno
ichar ifilespec
[300]char filespec
ref char source
ichar newname

!CPL "LOADIMPORT:",MODULENAME
newname:=modulename

for i:=1 to nmodules do
	if eqstring(moduletable[i].name,newname) then		!already loaded
		return i
	fi
od

fileno:=getmodulefile(modulename,ownername)

return addmodule(newname,fileno, exportflag)
end

proc lextest(ichar file)=

int fileno,t, ntokens
ref tokenrec tk

CPL =strec.bytes
CPL =unitrec.bytes

t:=clock()

lexsetup()

fileno:=getmainfile(file)

if not fileno then
	CPL "CAN'T LOAD", FILE
	stop
fi
CPL "LOADED",FILE

!tk:=readtokens_l(fileno, ntokens)
readtokens_a(fileno, ntokens)

!currtoken:=0

repeat
	lex()
	printsymbol(&lx)
until lx.symbol=eofsym

CPL CLOCK()-T,"msec"

CPL "READ",=NTOKENS

CPL =NALLLINES
!CPL =NTOKENS

!PRINTHASHTABLE()
end

proc initsearchdirs=
[300]char str1,str2
int i

!DIFFERENT SETUP NEEDED FOR LINUX

nsearchdirs:=0
addsearchdir("c:/bx/")
addsearchdir("c:/axb/")
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

proc tokenisemodule(int moduleno)=
	ref modulerec m:=&moduletable[moduleno]
	int ntokens

	m.tklist:=readtokens_a(m.fileno, ntokens)

end

proc getinputoptions=
const slash='-'
int i,j,k
int paramno,pmtype,sw,ncolons,passfixed
ichar name,value,filename,ext

paramno:=2
ncolons:=0

while pmtype:=nextcmdparam(paramno,name,value,"b") do
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
!	when pm_colon then
!		if ++ncolons>1 then
!			name:=":"
!			value:=nil
!			goto doextra
!		fi
!	when pm_extra then
!doextra::
!		extraparams[++nextraparams]:=pcm_copyheapstring(name)
!		extravalues[nextraparams]:=pcm_copyheapstring(value)
	esac

od

if debugmode then
	if passlevel=0 then passlevel:=pcl_pass fi
else
	passlevel:=prodpasslevel
	if passlevel=0 then passlevel:=exe_pass fi

	if fnobsys then
		if passlevel>=exe_pass then fnobsys:=0 fi
	fi

fi

if ninputfiles=0 then
	showcaption()
	println "Usage:"
	println "	",,sysparams[1],"filename[.b]     # Compile project to executable"
!	println "	",,sysparams[1],"-help            # Other options"
	stop

elsif ninputfiles=1 then
	filename:=inputfiles[1]				!primary file name
!
!	ext:=extractext(filename)
!!	if eqstring(ext,"ba") then
!!		fbundled:=1
!!		mafilename:=pcm_copyheapstring(filename)
!!		inputfiles[1]:=pcm_copyheapstring(changeext(filename,"b"))
!!	fi
!
	outfilesource:=pcm_copyheapstring(changeext(filename,"asm"))
	outfilebin:=pcm_copyheapstring(changeext(filename,(passlevel>=exe_pass|"exe"|"obj")))
else
	loaderror("Specify one lead module only")
fi
end

proc do_option(int sw, ichar value)=
int length

case sw
when compile_sw then
	prodpasslevel:=asm_pass

when coff_sw then
	prodpasslevel:=obj_pass

when build_sw then
	prodpasslevel:=exe_pass

when run_sw then
	prodpasslevel:=run_pass

when opt_sw then foptimise:=1

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

when out_sw then
	destfilename:=pcm_copyheapstring(value)

when unused_sw then
	fcheckunusedlocals:=1

when nosys_sw then
	fnobsys:=1

when debug_sw then
	debugmode:=1

when prod_sw then
	debugmode:=0

when load_sw then passlevel:=load_pass
when parse_sw then passlevel:=parse_pass
when fixup_sw then passlevel:=fixup_pass
when name_sw then passlevel:=name_pass
when type_sw then passlevel:=type_pass
when pcl_sw then passlevel:=pcl_pass
when mcl_sw then passlevel:=mcl_pass
when asm_sw then passlevel:=asm_pass
when obj_sw then passlevel:=obj_pass
when exe_sw then passlevel:=exe_pass

when ast1_sw then fshowast1:=1
when ast2_sw then fshowast2:=1
when ast3_sw then fshowast3:=1
when showpcl_sw then fshowpcl:=1
when showmcl_sw then fshowmcl:=1
when st_sw then fshowst:=1
when stflat_sw then fshowstflat:=1
when types_sw then fshowtypes:=1
when overloads_sw then fshowoverloads:=1

when ba_sw then
	fwriteba:=1

esac


end

proc showcaption=
	println "B Compiler", $date, $time
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
!	static ichar helptext=strinclude "mm_help.txt"
!	println helptext
	println "Help Text"
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

	if lx.symbol=namesym and eqstring(lx.symptr.name,"when") then
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

global proc addmodulemapping(ichar old, new, optionname=nil, valuename=nil)=
	int option
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

proc dosetoptionvar(ichar s)=
	ref char t
	[256]char name
	[256]char value

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

global proc initassemsymbols=
!initialise hash table from kwddata
[32]char str
int i

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

proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

proc showmodules=

println "Searchdirs:",nsearchdirs
FOR I TO NSEARCHDIRS DO
	CPL I, SEARCHDIRS[I]
OD

println "Modules:",nmodules
for i to nmodules do
	cpl moduletable[i].name, sourcefilepaths[moduletable[i].fileno]
od

end

proc showoptions=
CPL =DEBUGMODE
CPL =PASSNAMES[PASSLEVEL]
CPL =FSHOWAST1
CPL =FSHOWAST2
CPL =FSHOWAST3
CPL =FSHOWST
CPL =FSHOWSTFLAT
CPL =FSHOWTYPES
CPL =FSHOWOVERLOADS
CPL =FNOBSYS

end
=== clibnew.m 2/18 ===
global type filehandle=ref void

importlib $cstd=
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

importlib $cstdextra=
	clang function __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

global const c_eof		=-1
global const seek_set	= 0
global const seek_curr	= 1
global const seek_end	= 2
=== mlib.m 3/18 ===
import msys
import clib
import oslib

!const mem_check=1
const mem_check=0

GLOBAL INT MDEBUG


global [0..300]u64 allocupper
global int alloccode				!set by heapalloc
global int allocbytes				!set by heapalloc
global int fdebug=0
global int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

byte pcm_setup=0

int show=0

global int memtotal=0
global int64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
const int maxmemalloc=500000
[maxmemalloc+1]ref int32 memalloctable
[maxmemalloc+1]int32 memallocsize

const pcheapsize=1048576*2
ref byte pcheapstart
ref byte pcheapend			!points to first address past heap
ref byte pcheapptr

const int maxblockindex = 8 		!2048
global const int maxblocksize = 2048

[0:maxblocksize+1]byte sizeindextable	!convert byte size to block index 1..maxblockindex

const int size16   = 1			!the various index codes
const int size32   = 2
const int size64   = 3
const int size128  = 4
const int size256  = 5
const int size512  = 6
const int size1024 = 7
const int size2048 = 8

GLOBAL [0:9]ref wordp freelist

global record strbuffer =
	ichar strptr
	int32 length
	int32 allocated
end

global tabledata() [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

[2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

global function pcm_alloc(int n)ref void =		!PCM_ALLOC
ref byte p
!int i

!IF MDEBUG THEN
!CPL "PCMALLOC",N
!FI
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
!IF MDEBUG THEN
!CPL "PCMALLOC/ALLOCBYTES",ALLOCBYTES
!FI

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

global proc pcm_freestr(ichar s) =
pcm_free(s,strlen(s)+1)
end

global proc pcm_free(ref void p,int n) =		!PCM_FREE
!n can be the actual size requested it does not need to be the allocated size
int acode

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

!	(ref wordp(p))^:=wordp(int(freelist[acode]))
	cast(p,ref wordp)^:=wordp(int(freelist[acode]))
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
int j,k,k1,k2
int64 size
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
static int totalheapsize
ref byte p

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
static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

if n>maxblocksize then
	return n
else
	return allocbytes[sizeindextable[n]]
fi
end

global function pcm_array(int n)int =		!PCM_ARRAY
!n bytes are needed for an array return the number of bytes to be actually allocated
int m

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
int m

println "HEAP FREELISTS:",caption

m:=16
for i:=1 to 8 do
	pcm_printfreelist(m,freelist[i])
	m<<:=1
od
end

global function pcm_allocz(int n)ref void =		!PCM_ALLOCZ
ref void p
p:=pcm_alloc(n)

memset(p,0,n)
return p
end

global function pcm_copyheapstring(ref char s)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
ref char q
int n
if s=nil then return nil fi

n:=strlen(s)+1
q:=pcm_alloc(n)
memcpy(q,s,n)
return q
end

global function pcm_copyheapstringn(ref char s,int n)ref char =
ref char q
if s=nil then return nil fi

q:=pcm_alloc(n+1)
memcpy(q,s,n)
(q+n)^:=0
return q
end

global function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	ref char q
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
ref void p

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
	word32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

global proc readrandom(filehandle handlex, ref byte mem, int offset, size) =		!READRANDOM
	int a
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
filehandle f
int size
ref byte m,p

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
filehandle f
int n

f:=fopen(filename,"wb")
if f=nil then
	return 0
fi

n:=writerandom(f,data,0,size)
fclose(f)
return n
end

global function checkfile(ref char file)int=		!CHECKFILE
filehandle f
if f:=fopen(file,"rb") then
	fclose(f)
	return 1
fi
return 0
end

global proc readlinen(filehandle handlex,ref char buffer,int size) =		!READLINEN
!size>2
int ch
ref char p
int n
array[0:100]char buff
byte crseen

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
static [260]char newfile
array[32]char newext2
ref char sext
int n

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
ref char t,u

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
static [0:260]char str
ref char t
int n

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
ref char t

t:=extractpath(s)

if t^=0 then			!s contains no path
	return s
fi

return s+strlen(t)		!point to last part of s that contains the file
end

global function extractbasefile(ref char s)ichar=		!EXTRACTBASEFILE
static [0:100]char str
ref char f,e
int n,flen

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
ref char sext

sext:=extractext(s,1)

if sext^=0 then						!no extension not even "."
	return changeext(s,newext)
fi

return s							!has own extension; use that
end

global function alloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
ref void p

p:=malloc((n+1)*size)

if not p then
	abortprogram("Alloctable failure")
fi
return p
end

global function zalloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
ref int p

p:=alloctable(n,size)

pcm_clearmem(p,(n+1)*size)
return p
end

global proc checkfreelists(ichar s)=
ref wordp p,q
int64 aa

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
ref byte p

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
!(ref wordp(p))^:=wordp(int(freelist[2]))
cast(p,ref wordp)^:=wordp(int(freelist[2]))
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
int c

c:=fgetc(f)
if c=c_eof then return 1 fi
ungetc(c,f)
return 0;
end

global function pcm_smallallocz(int n)ref void =
ref byte p

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
ref byte p

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
int newlen,oldlen
ichar newptr

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
array[16]char s

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
int col,i,n,slen
array[2560]char str
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
int n
array[2560]char str

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
static int infile=0
static ichar filestart=nil
static ichar fileptr=nil
static byte colonseen=0
ref char q
ichar item,fileext
ichar rest
int length
static [300]char str

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
ref char p,pstart,pend
int n
static [256]char str

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
int n

n:=strlen(s)
to width-n do
	strcat(s,padchar)
od
end

global function padstr(ref char s,int width,ref char padchar=" ")ichar=
static [256]char str

strcpy(&.str,s)
ipadstr(&.str,width,padchar)
return &.str
end

global function chr(int c)ichar=
static [8]char str

str[1]:=c
str[2]:=0
return &.str
end

global function cmpstring(ichar s,t)int=
	int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

global function cmpstringn(ichar s,t,int n)int=
	int res
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
	int res
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

global function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
	word64 x,y
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
	int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

!global function mrandomreal:real =
!!positive random real value from 0 to 0.999999999999999999891579782751449556599254719913005828857421875
!!upper limit is (2**63-1)/(2**63)
!	return real(mrandomp())/9223372036854775808.0
!end

global function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

global function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
	return mrandomp()/9223372036854775807
end

global function checkpackfile:ref byte=
!find out if this executable contains extra packed files
!return 1 or 0

int a,offset,i,size
array[100]char name
array[300]char exefile
ref byte packexeptr			!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
int packexesize				!byte size
ref char packfilename
int packfilesize
ref byte packfileptr

!macro getfileint(data,offset)=(ref int32(data+offset))^
macro getfileint(data,offset)=cast(data+offset,ref int32)^

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
=== msysnew.m 4/18 ===
import clib
import mlib

global record procinforec=
	word16		fnindex
	byte		rettype
	byte		nparams
	[12]byte	paramlist
end

!for print/read routines
!------------------------------------------
record fmtrec=	! (default)
	byte	minwidth	! n (0)   min field width (0 if not used or don't care)
	i8		precision	! .n (0)   number of decimals/significant figures/max width
	byte	base		! B,H or Xn (10)  2 to 16

	char	quotechar	! Qc (0)   0 or '"' or c
	char	padchar		! Pc, Z (' ')
	char	realfmt		! E,F,G ('f') 'e' or 'f' or 'g'

	char	plus		! (0)   0 or '+'
	char	sepchar		! Sc (0)   0 or ',' or c placed every 3 (base=10) or 4 digits
	char	lettercase	! A,a ('A') 'A' or 'a'
	char	justify		! JL, JR, JC ('R') 'L' or 'R' or 'C'?
	char	suffix		! Tc (0)   0 or 'B' or 'H' or c
	char	usigned		! W (0)   0 or 'W' force unsigned o/p for ints (eg. for hex display)
	char	charmode	! C,D (0)  0 or 'C' or 'D'	o/p int as int or single char or double/multi-char
	char	heapmode	! M (0)  'M' for str-functions, return ptr tp heap string
	char	param		! Use int value for <fmtparam>
	byte	spare
end

int fmtparam			!as set with :'V'

enum (std_io,file_io,str_io)

const comma = ','

global int needgap			= 0
int outdev			= std_io
filehandle outchan	= nil
ref char fmtstr 	= nil

const maxiostack=10
[maxiostack]filehandle	outchan_stack
[maxiostack]int			outdev_stack
[maxiostack]ref char	fmtstr_stack
[maxiostack]byte		needgap_stack

[maxiostack]ref char	ptr_stack		!this one doesn't need pushing, as each is pointed to from outchan
int niostack=0

[0:]char digits=A"0123456789ABCDEF"
const onesixty=360
fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,0,0)

!Read buffer vars
const rd_buffersize = 16384	!total capacity of line buffer

ref char rd_buffer		! point to start of read buffer
int rd_length			! length of this line (as read by readln)
ref char rd_pos			! current position it's up to (next read starts here)
ref char rd_lastpos		! set by sread() just before reading used for reread()
int termchar			! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals

!------------------------------------------

const maxparam=128
global int nsysparams
global [maxparam]ichar sysparams

const maxcallback=8
[0..maxcallback,8]word64 callbackstack
int ncallbacks=0

word64 mask63	= 0x7FFF'FFFF'FFFF'FFFF
real offset64	= 9223372036854775808.0		! 2**63 as r64
real offset32	= 9223372036854775808.0		! 2**63 as r32

global proc m$init=
int32 nargs
int nargs64
ref[]ichar args
ref[]ichar env
static [128]byte startupinfo			! 68 or 104 bytes
int res
ichar s

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
static word64 p13,p14
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

global function m$lenstr_stringz(ref char s)int=
	strlen(s)
end

global function m$getdotindex(word64 a,int i)int=
!return (a iand (1dw<<i))>>i
return (a iand (1<<i))>>i
end

global proc m$setdotindex(ref word64 a, int i,x)=
ref word32 a32

!see comments on setdotslice
if i>=32 then
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
int w
word64 mask64
word mask
ref word32 a32

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
	ref ref char p
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
	array[20]char s

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
	array[40]char s
	fmtrec fmt
	int n

!CPL "PRINTI64",=FMTSTYLE
!

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
	array[40]char s
	fmtrec fmt

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
	array[40]char s
	fmtrec fmt

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
	array[40]char s
	fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	tostr_u128(a,&fmt,0)
	needgap:=1
end

global proc m$print_r64(real x,ichar fmtstyle=nil)=
	array[360]char s
	fmtrec fmt

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
	array[40]char s
	fmtrec fmt
	int n

	nextfmtchars()

	s[1]:=a
	s[2]:=0
	printstr(&.s)
	needgap:=1
end

global proc m$print_str(ichar s, fmtstyle=nil)=
	nextfmtchars()
	fmtrec fmt
	if fmtstyle=nil then
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,&fmt)
	fi
	needgap:=1
end

!global proc m$print_strsl(slice[]char s, ichar fmtstyle=nil)=
!	nextfmtchars()
!	fmtrec fmt
!	if fmtstyle=nil then
!		printstr_n(cast(s.sliceptr),s.len)
!	else
!		abortprogram("FORMATED PRINT SLICE NOT READY")
!!		strtofmt(fmtstyle,-1,&fmt)
!!		tostr_str(s,&fmt)
!	fi
!	needgap:=1
!end

!global proc m$print_flexstr(object s, ichar fmtstyle=nil)=
!	nextfmtchars()
!	fmtrec fmt
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

global proc printstr(ichar s)=
	int n
	ref ref char p

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

global proc printstr_n(ichar s,int n=-1)=
	ref ref char p

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
	ref ref char p
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

global proc nextfmtchars(int lastx=0)=
	char c
	ref char pstart
	int n

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

	char c
	byte wset
	int n
	array[0:100]char str

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
	array[0:20]char str
	ref char q
	int i,nchars

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

	int i,w,m

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
	word64 q,r
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
	array[0:onesixty]char t
	u64 dd
	int i,j,k,g
	int cc
	int dummy
	ref char s0

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
	array[0:160]char t
	u64 dd
	int i,j,k,g
	int dummy
	ref char s0

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
	word128 d,e,r
	word rlow

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
	array[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w,usigned
	static u64 mindint=0x8000'0000'0000'0000

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
	array[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

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
	array[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

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
	array[0:onesixty]char t
	int i,j,k,g,neg

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
	ref char u,v
	array[256]char str
	int w,nheap		! whether any heap storage is used  bytes allocated

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
	array[360]char str
	int n

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
	array[360]char str
	int n

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
	array[360]char str
	int n

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
	array[360]char str,str2
	array[0:10]char cfmt
	int n

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
	int oldlen,newlen,n
	ref char t

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

global function getfmt(ichar fmtstyle)ref fmtrec=
	static fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	fi
end

global function strint(int64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

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
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

global function strreal(real a, ichar fmtstyle=nil)ichar=
	static [320]char str
	ref fmtrec fmt

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
	int n

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
	ref char p,s,itemstr
	char quotechar, c

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
	byte signd
	word64 aa
	char c,d

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
	ref char s
	int length,c
	int64 aa

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
	array[512]char str
	ref char s
	int length
	int32 numlength
	real x

	s:=readitem(length)

	if length=0 or length>=str.len then		!assume not a real
		return 0.0
	fi
	memcpy(&.str,s,length)
	str[length+1]:=0

	itemerror:=0

	if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
		x:=0.0
		itemerror:=1
	fi

	return x
end

global proc m$read_strold(ref char dest, int destlen=0,fmt=0)=
	ref char s
	int length,numlength
	real x

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

global function m$read_str(int fmt=0)ichar t=
	ref char s
	int length,numlength
	real x

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

	t:=pcm_alloc(length+1)
	memcpy(t,s,length)
	(t+length)^:=0
	return t
end

global proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_strold(dest,destlen,fmt)
end

global proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

global proc reread=
	rd_pos:=rd_lastpos
end

global function valint(ichar s, int fmt=0)int64=
ref char old_pos, old_lastpos
int64 aa

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
ref char old_pos, old_lastpos
real x

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
=== oswindows.m 5/18 ===
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

importdll $windowsdlls=
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
	wt_word	eventtype
	word16	padding
		wt_bool	keydown			!key event record (was inside 'Event' union in win32)
		wt_word	repeatcount
		wt_word	virtualkeycode
		wt_word	virtualscancode
		union
			wt_word unicodechar
			wt_char asciichar
		end
		wt_dword controlkeystate
end

record rspoint=(int16 x,y)

record rsrect=
	int16 leftx,top,rightx,bottom
end

global record rpoint =
	wt_long x,y
end

record rconsole=
	rspoint size,pos
	word16 attributes
	rsrect window
	rspoint maxwindowsize
end

record rstartupinfo =
	wt_dword	size
!.if $64bit
	word32 dummy1
!.endif
	wt_ichar	reserved
	wt_ichar	desktop
	wt_ichar	title
	wt_dword	x
	wt_dword	y
	wt_dword	xsize
	wt_dword	ysize
	wt_dword	xcountchars
	wt_dword	ycountchars
	wt_dword	fillattribute
	wt_dword	flags
	wt_word		showwindow
	wt_word		reserved2
!.if $64bit
	word32 dummy2
!.endif
	wt_ptr		reserved4
	wt_handle	stdinput
	wt_handle	stdoutput
	wt_handle	stderror
end

record rprocess_information =
	wt_handle process
	wt_handle thread
	wt_dword processid
	wt_dword threadid
end

record rwndclassex =
	wt_uint		size
	wt_uint		style
	wt_wndproc	wndproc
	wt_int		clsextra
	wt_int		wndextra
	wt_handle	instance
	wt_handle	icon
	wt_handle	cursor
!	wt_handle	background
	wt_handle	background
	wt_ichar	menuname
	wt_ichar	classname
	wt_handle	iconsm
end

global record rmsg =
	wt_handle	hwnd
	wt_uint		message
!.if $64bit
	word32		dummy1
!.endif
	wt_wparam	wParam
	wt_lparam	lParam
	wt_dword	time
!.if $64bit
	word32		dummy2
!.endif
	wt_point	pt
end

!wt_word x
const NORMAL_PRIORITY_CLASS=32
const CREATE_NEW_CONSOLE=16
const DETACHED_PROCESS=16

wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

ref function(ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

global proc os_init=
int i,count
rconsole info

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
wt_dword exitcode
int status
int cflags:=0

rstartupinfo si
rprocess_information xpi

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
wt_dword exitcode
int i,j,k

rstartupinfo si
rprocess_information xpi

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
int k

k:=os_getchx() iand 255

return k
end

global function os_kbhit:int=
wt_dword count
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
wt_handle hinst

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
rwndclassex r
static byte registered

if registered then
	return
fi

!CPL "REG CLASS"

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
rmsg m
int i,result
intm l
static int count=0

!CPL "MAINWND/MV"

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
int count
int charcode,keyshift,keycode
int altdown,ctrldown,shiftdown,capslock

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
static [300]char name
static int n

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
wt_handle f;
int64 ctime,atime,wtime;

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
int64 a

queryperformancecounter(&a)
return a

end

global function os_hpfrequency:int64=
int64 a

queryperformancefrequency(&a)
return a

end

global proc os_peek=
int ticks
static int lastticks
array [100]byte m
	ticks:=GetTickCount()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end
=== bb_lex.m 6/18 ===
import msys
import mlib
import clib
!import oslib

import bb_decls
import bb_tables
import bb_support
import bb_lib

!GLOBAL INT NCLASSES
!GLOBAL INT NLOOKUPS


const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]int lxlineno_stack
[maxstackdepth]byte isfile_stack
int sourcelevel=0

global int NALLLINES
global int NLOOKUPS
global int NCLASHES
global int NTOKENS

const cr	= 13
const lf	= 10
const tab	= 9

!int assemmode

ref char lxstart
ref char lxsptr
int lxifcond
int longsuffix			!for real nos

int lxfileno
!int lx.pos
!int lxlastsymbol

!ref char lxsvalue
!int lxlength

!const inittokensize = 32768
!const inittokensize = 131072
!const inittokensize = 1048576
const inittokensize = 1048576*4
!const inittokensize = 1048576*16

ref[]tokenrec tokenlist
int tokenlistsize
!int currtoken
global ref tokenrec nexttoken

byte prescanmode=0

!const hstsize	= 2048
!const hstsize	= 8192
const hstsize	= 32768
!const hstsize	= 65536
!const hstsize	= 131072
!const hstsize	= 524288
const hstmask	= hstsize-1

global [0:hstsize]strec hashtable

[]ichar maxnumlist=(
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
[maxnumlist.len]int maxnumlen

global proc lex=
	lx:=nexttoken^
	++nexttoken
end

!global function nextsymbol:int=
!	return nexttoken^.symbol
!end

global function nextlx:ref tokenrec=
	return nexttoken
end

global proc lexreadtoken=
!read next token into nextlx
int c,hsum,commentseen,hashindex,length
ref char pstart,pnext,p,ss,lxsvalue

lx.subcode:=0

doswitch lxsptr++^
when 'a'..'z','$','_' then
	lxsvalue:=lxsptr-1
doname::
	hsum:=lxsvalue^

	doswitch c:=lxsptr++^
	when 'a'..'z','0'..'9','_','$' then
		hsum:=hsum<<4-hsum+c
	when 'A'..'Z' then
		(lxsptr-1)^:=c+' '
		hsum:=hsum<<4-hsum+c+' '
	when '"' then
		--lxsptr
		if lxsvalue+1=ref char(lxsptr) then
			case lxsvalue^
			when  'F','f','R','r' then 
				readrawstring()
				return
			when  'A','a','Z','z' then 
				readarraystring(lxsvalue^)
				return
			esac
		fi
		exit
	else
		--lxsptr
		exit
	end doswitch

	do_name(lxsvalue, lxsptr-lxsvalue, (hsum<<5-hsum) iand hstmask)
	return

when 'A'..'Z' then
	lxsvalue:=lxsptr-1
	lxsvalue^+:=32
	goto doname

when '0'..'9' then
	c:=(lxsptr-1)^
	case lxsptr^
	when ' ',')',cr,',','|' then		!assume single digit decimal
!	when ' ',')',cr,',' then		!assume single digit decimal
		lx.symbol:=intconstsym
		lx.subcode:=tint
		lx.value:=c-'0'
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
	when 0 then
		--lxsptr
		exit
	end
	++lx.pos
++NALLLINES
	lx.symbol:=eolsym
	return

when '#' then			!docstring to eol
	lxsvalue:=cast(lxsptr)

	doswitch c:=lxsptr++^
	when 13,10,0 then			!leave eol for next symbol
		--lxsptr
		exit
	end

	length:=lxsptr-cast(lxsvalue,ref char)
!	(lxsvalue+lxlength)^:=0
	lx.symbol:=docstringsym
	lx.svalue:=pcm_copyheapstringn(lxsvalue,length)
	return

when '\\' then			!line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
	commentseen:=0
	doswitch lxsptr++^			!read until end of this line
	when cr then
++NALLLINES
		++lx.pos
		++lxsptr				!skip lf
		exit
	when lf then
++NALLLINES
		++lx.pos
		exit
	when 0 then
		lx.symbol:=eofsym
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
		++lx.pos
++NALLLINES
		++lxsptr				!skip lf
	when lf then
++NALLLINES
		++lx.pos
	when ' ',tab then
	else
		--lxsptr
		exit
	enddoswitch
!	next

when '{' then
	lx.symbol:=lcurlysym
	return

when '}' then
	lx.symbol:=rcurlysym
	return

when '.' then
	switch lxsptr^
	when '.' then				!.. or ...
		++lxsptr
		if lxsptr^='.' then
			++lxsptr
			lx.symbol:=ellipsissym
		else
			lx.symbol:=rangesym
			lx.subcode:=j_makerange		!helps treat as opsym which all have k-code as subcode
		fi
		return
	when '0'..'9' then			!real const: deal with this after the switch
		--lxsptr
		readrealnumber(nil,0,10)
		return
	else
!		p:=lxsptr-2
!		if p<lxstart or p^=cr or p^=lf then
!			lx.symbol:=lexdotsym
!		else
			lx.symbol:=dotsym
!		fi
		return
	endswitch

when ',' then
	lx.symbol:=commasym
	return

when ';' then
	lx.symbol:=semisym
	return

when ':' then
	switch lxsptr^
	when '=' then
		++lxsptr
		lx.symbol:=assignsym
		lx.subcode:=j_assignx		!helps treat as opsym which all have k-code as subcode
	when ':' then
		++lxsptr
		case lxsptr^
		when '=' then
			++lxsptr
			lx.symbol:=deepcopysym
			lx.subcode:=j_deepcopyx
		else
			lx.symbol:=dcolonsym
		esac
	else
		lx.symbol:=colonsym
	endswitch
	return

when '(' then
	lx.symbol:=lbracksym
	return

when ')' then
	lx.symbol:=rbracksym
	return

when '[' then
	lx.symbol:=lsqsym
	return

when ']' then
	lx.symbol:=rsqsym
	return

when '|' then
!	NEXTLX.SYMBOL:=CURLSYM
!	RETURN

	if lxsptr^='|' then
		++lxsptr
		lx.symbol:=dbarsym
	else
		lx.symbol:=barsym
	fi
	return

when '^' then
	lx.symbol:=ptrsym
	return

when '@' then
	if lxsptr^='@' then
		++lxsptr
		lx.symbol:=datsym
	else
		lx.symbol:=atsym
	fi
	return

when '?' then
	lx.symbol:=questionsym
	return

!when 156 then		!'œ' in ansi font or whatever
!when 'œ' then		!'œ' in ansi font or whatever
!	lx.symbol:=poundsym
!	return
!
when '~' then
	lx.symbol:=curlsym
	return

!when 'ª' then
!	lx.symbol:=gatesym
!	return
!
when '+' then
	lx.symbol:=addsym
	if lxsptr^='+' then
		++lxsptr
		lx.symbol:=incrsym
		lx.subcode:=j_preincrx
		return
!	else
!		lx.subcode:=j_add
	fi
	return

when '-' then
	lx.symbol:=subsym
	if lxsptr^='-' then
		++lxsptr
		lx.symbol:=incrsym
		lx.subcode:=j_predecrx
		return
!	else
!		lx.subcode:=j_sub
	fi
	return

when '*' then
	if lxsptr^='*' then
		++lxsptr
		lx.symbol:=powersym
	else
		lx.symbol:=mulsym
	fi
	return

when '/' then
	lx.symbol:=divsym
	return

when '%' then
	lx.symbol:=idivsym
	return

when '=' then
	case lxsptr^
	when '>' then
		lx.symbol:=sendtosym
		++lxsptr
	when '=' then
		lx.symbol:=samesym
		++lxsptr
!	when ':' then
!		++lxsptr
!		if lxsptr^<>'=' then lxerror("=:?") fi
!		++lxsptr
!		lx.symbol:=dispassignsym
!		lx.subcode:=j_dispassign
!CPL "DISPASSIGN"
	else
		lx.symbol:=eqsym
	esac
	return

when '<' then
	switch lxsptr^
	when '=' then
		++lxsptr
		lx.symbol:=lesym
	when '>' then
		++lxsptr
		lx.symbol:=nesym
	when '<' then
		++lxsptr
		lx.symbol:=shlsym
	else
		lx.symbol:=ltsym
	endswitch
	return

when '>' then
	switch lxsptr^
	when '=' then
		++lxsptr
		lx.symbol:=gesym
	when '>' then
		++lxsptr
		lx.symbol:=shrsym
	else
		lx.symbol:=gtsym
	endswitch
	return

when '&' then
	case lxsptr^
!	when '&' then
!		++lxsptr
!		lx.symbol:=opsym
!		lx.subcode:=j_andand
	when '.' then
		++lxsptr
		lx.symbol:=anddotsym
		lx.subcode:=0
	else
		lx.symbol:=addrsym
		lx.subcode:=j_addrof
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
	++lx.pos
++NALLLINES
	lx.symbol:=eolsym
	return
when lf then			!only lfs not preceded by cr
	++lx.pos
++NALLLINES
	lx.symbol:=eolsym
	return

when 0 then
	if sourcelevel then
		unstacksource()
	else
		lx.symbol:=eofsym
		--lxsptr
		return
	fi

else
	lx.symbol:=errorsym
!	lx.value:=c
	return

end doswitch

end

proc readnumber(int base)=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
ref char pstart,dest
int c
ref char p

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
ref char pstart,dest
int c,n,base,suffix
ref char p

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
	base:=lx.value
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
!read entire numbers, convert to real value in lx.xvalue
ref char fractstart,ss
int fractlen,expon,i,c,n
real basex,x
const maxrealdigits=500
[maxrealdigits]char realstr
[32]char str

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

	lx.symbol:=decimalconstsym
!	lx.subcode:=tflexdecimal
	lx.svalue:=ss
!	lxlength:=strlen(ss)
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

lx.symbol:=realconstsym
lx.subcode:=treal
lx.xvalue:=x
end

function readrealbest(int intlen,fractlen,expon, ichar realstr)real=
	[32]char expstr

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
ref char numstart,numend
int expon,length,neg

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
return (neg|-lx.value|lx.value)
end

global proc printsymbol(ref tokenrec lp)=
tokenrec l
l:=lp^

printf("%-18s",symbolnames[l.symbol])

case l.symbol
!when rawnamesym then
!!	printstrn(l.svalue,l.length)
!	printstr(l.svalue)
!!	print " (",,l.hashvalue,,")"
when namesym then
	printstrn(l.symptr^.name,l.symptr^.namelen)

	if l.subcode then
		fprint " [#]",symbolnames[l.subcode]
	fi

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
	printstr(l.svalue)
	print """",strlen(l.svalue)
when charconstsym then
	print "'"
	printstr(l.svalue)
	print "'"
when decimalconstsym then
	printstr(l.svalue)
	print "L"
when assignsym,addrsym,ptrsym,deepcopysym,rangesym,
	andlsym,orlsym,xorlsym,eqsym,nesym,ltsym,lesym,gtsym,gesym,addsym,subsym,
	mulsym,divsym,idivsym,iremsym,iandsym,iorsym,ixorsym,shlsym,shrsym,
	minsym,maxsym,andbsym, orbsym,concatsym,powersym, xorlsym,
	idivremsym, samesym then
	print symbolnames[l.symbol]
elsif l.subcode then
	fprint "SUBCODE:",l.subcode
!	fprint "#",symbolnames[l.subcode]
end

println

end

proc stringtonumber(ichar s, int length, base)=
!convert decimal number s to an i64 value
!s contains only digits
!for hex, then a..f and A..F have been converted to '9'+1 to '9'+6
int64 a
word64 b
int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do		!trim leading zeros
	++s
	--length
od

lx.symbol:=intconstsym

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
				lx.subcode:=tu128
			else
				lx.subcode:=ti128
			fi

			lx.pvalue128:=stringtonumber128(s,length,16)
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

lx.value:=a

lx.subcode:=setinttype(a)
end

proc stringtodecimalnumber(ichar s, int length,suffix=0)=
int64 a
word64 b
int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do		!trim leading zeros
	++s
	--length
od

lx.symbol:=intconstsym

if length>20 or \
		(length=20 and strncmp(s,"18446744073709551615",20)>0) or suffix then

	if length>39 or \
		(length=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
		if suffix='W' then
			lxerror("-W overflows 128 bits")
		fi
dolongint::
		lx.symbol:=decimalconstsym
!		lx.subcode:=tflexdecimal
		lx.svalue:=pcm_copyheapstring(s)
!		lxlength:=length
	else						!greater than 64 bits, up to 128 bits

		if suffix='L' then goto dolongint fi

		if (length=39 and strncmp(s,"170141183460469231731687303715884105727",39)>0) then
			lx.subcode:=tu128
		else
			lx.subcode:=ti128
		fi

		lx.pvalue128:=stringtonumber128(s,length,10)
	fi
	return
fi

a:=0

to length do
	a:=a*10+s++^-'0'
od

lx.value:=a

lx.subcode:=setinttype(a)
end

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
! do maxnum support and such
int i!,n
static int n

for i to maxnumlist.len do
	maxnumlen[i]:=strlen(maxnumlist[i])
od

!tokenlist:=pcm_alloc(tokenrec.bytes*inittokensize)
!tokenlistsize:=inittokensize

inithashtable()
end

proc newtokenlist=
	tokenlist:=pcm_alloc(tokenrec.bytes*inittokensize)
	tokenlistsize:=inittokensize
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
ichar dest
int c

lx.symbol:=stringconstsym
lx.svalue:=++lxsptr

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
when cr,lf,0 then
	lxerror("Raw string not terminated")
	--lxsptr
	exit
else
	dest++^:=c
enddoswitch
!lxlength:=dest-lxsvalue
end

proc lookup(ref char name, int length, hashindex)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped

	lx.symptr:=&hashtable[hashindex]
	wrapped:=0

	do
		case lx.symptr^.namelen
		when 0 then
			exit
		when length then
			if memcmp(lx.symptr.name,name,length)=0 then	!match
				lx.symbol:=lx.symptr.symbol
				lx.subcode:=lx.symptr.subcode
				return
			fi
		esac

		++lx.symptr
		if ++hashindex>=hstsize then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			lx.symptr:=&hashtable[0]
			hashindex:=0
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr
	lx.symptr.name:=pcm_copyheapstringn(name,length)
	lx.symptr.namelen:=length
	lx.symptr.symbol:=namesym
	lx.symbol:=namesym
end

function lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
int j, wrapped, hashvalue

hashvalue:=gethashvaluez(name)

j:=hashvalue iand hstmask
lx.symptr:=&hashtable[j]
wrapped:=0

do
	if lx.symptr^.namelen=0 then
		exit
	elsif eqstring(lx.symptr^.name,name) then	!match
		cpl name
		lxerror("sys dupl name?")
	fi

	++lx.symptr
	if ++j>=hstsize then
		if wrapped then
			abortprogram("SYS:HASHTABLE FULL")
		fi
		wrapped:=1
		lx.symptr:=&hashtable[0]
		j:=0
	fi
od

!exit when not found; new name will go in entry pointed to by lxsymptr

lx.symptr^.name:=name				!assume can be shared (stored in a table)
lx.symptr^.namelen:=strlen(name)
lx.symptr^.symbol:=namesym			!usually replaced with actual symbol details

!CPL "ADDED SYS",NAME,STRLEN(NAME),SYMBOLNAMES[NAMESYM],=LX.SYMPTR,=HASHVALUE

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
	hsum:=hsum<<4-hsum+c
od
return hsum<<5-hsum
end

proc inithashtable=
!populate hashtable with standard symbols
int i
memset(&hashtable,0,hashtable.bytes)

for i:=1 to stnames.len do
	lookupsys(stnames[i])

	lx.symptr.symbol:=stsymbols[i]

	case stsymbols[i]
	when unitnamesym then
		lx.symptr.index:=stsubcodes[i]
		lx.symptr.subcode:=unitnamesym
		lx.symptr.symbol:=namesym		!masquerades as normal identifier
	else
		lx.symptr.subcode:=stsubcodes[i]
	esac
od
end

proc printhashtable=

println "Hashtable:"

for i:=0 to hstsize-1 do
	if hashtable[i].namelen then
		println i,hashtable[i].name,symbolnames[hashtable[i].symbol]
	fi
od


end


global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=

!CPL "ADDRESERVEDWORD",NAME; OS_GETCH()
	lookupsys(name)

	lx.symptr.symbol:=namesym
	lx.symptr.subcode:=symbol
	lx.symptr.index:=subcode
	lx.symptr.regsize:=regsize
end

function dolexdirective(int index)int=
!return 1: returns a new symbol
!return 0: symbol has been absorbed; caller needs to read a new symbol
ref strec symptr
ref char p
ichar file
int i,lastsymbol,cond,fileno,length
[256]char str

case index
when strincludedir,binincludedir then
	lexreadtoken()
	if lx.symbol<>stringconstsym then
!		if lx.symbol=rawnamesym and eqbytes(lxsvalue,"$filename",9) then
!			file:=sourcefilepaths[lxfileno]
!		else
			lxerror("strincl: string expected")
!		fi
	else
		file:=lx.svalue
	fi

	fileno:=getsupportfile(file)
	lx.svalue:=sourcefiletext[fileno]
	length:=sourcefilesizes[fileno]

	lx.symbol:=(index=strincludedir|stringconstsym|astringconstsym)
	lx.subcode:='A'			!for use when an astring
	(lx.svalue+length)^:=0			!sometimes .length is not used (eg. in newstringobj())
	return 1						!so get it right. Don't need the etx

when includedir then
	lexreadtoken()
	if lx.symbol<>stringconstsym then lxerror("include: string expected") fi
	file:=lx.svalue
	convlcstring(file)
	file:=addext(file,".b")		!add in extension if not present; assume same as source

!	if fverbose then
!		println "  Include:",file
!	fi
	stacksourcefile(file)
	return 0

when defineunitdir then
	LXERROR("DEFINE UNIT NOT DONE")

else
	cpl sourcedirnames[index]
	lxerror("Directive not implemented")
esac
return 0
END

proc lexreadline=
!read lex chars until eol
!returns with lxsptr pointing to what follows (crlf, etc)
!caller should remember lxsptr as start of text
!processing of next symbol deals with line counting

doswitch lxsptr^
when cr,lf then
	return
when 0 then
	--lxsptr
	return
else
	++lxsptr
enddoswitch
END

global proc startlex(ichar caption,int fileno)=
!s is a 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it
!(This lex system doesn't deal with include files so there no nested sourcefiles.
!There are only macro expansions which are dealt with locally.)

lxsptr:=sourcefiletext[fileno]
!CPL =FILENO

!CPL "STARTLEX-----------"

lxfileno:=fileno
lx.pos:=1

lx.symbol:=semisym
lx.subcode:=0
end

global function convertzstring(ichar s, int length)ichar=
static [300]char str

if length>str.len then
	abortprogram("convertzstr")
fi
memcpy(&.str,s,length)
str[length+1]:=0
return &.str
end

global function addnamestr(ichar name)ref strec=
	tokenrec oldlx
	ref strec symptr

	oldlx:=lx
	lookup(name,strlen(name), gethashvaluez(name) iand hstmask)
	symptr:=lx.symptr
	lx:=oldlx

	return symptr
end

!global function findname(ichar name)ref strec=
!!find arbitrary name in st
!!return strec of generic entry, or nil if not found
!
!lookup(name,strlen(name),gethashvaluez(name)) then
!	return lx.symptr
!else
!	return nil
!fi
!end

global proc ps(ichar caption)=
!print "PS:",,caption,,":"
print caption,,": "
printsymbol(&lx)
end

!global proc lex=
!!return next token in lx, using lexreadtoken but working a token ahead.
!!static int lastline=0
!int lineno,n,dir,namelen
!ref char p
!ref strec symptr
!
!
!lx:=nextlx				!grab that already read basic token
!
!lx.pos:=int(lx.fileno)<<24+lx.pos
!
!reenter::
!
!lexreadtoken()			!read new token for next time around
!
!if lx.symbol=namesym then			!zero-terminate identifiers
!	(lx.symptr^.name+lx.length)^:=0		!can only do so after next symbol is read
!fi
!
!switch lx.symbol
!
!when rawnamesym then
!	if not lookup() then					!name not found
!		lx.symbol:=namesym				!convert to actual identifier
!		return
!	fi
!
!found::
!	lx.symbol:=lx.symptr^.symbol			!convert to reserved word, type, op etc
!	lx.subcode:=lx.symptr^.subcode
!
!!deal with new set of symbols...
!	switch lx.symbol
!
!	when ksourcedirsym then
!		if not dolexdirective(lx.subcode) then
!			goto reenter
!		fi
!	when rawnamesym then					!might be user identifier (points to generic entry)
!
!		if lx.subcode=unitnamesym and \
!				(lx.symbol=intconstsym or lx.symbol=realconstsym) then
!			case lx.symbol
!			when intconstsym then
!				if lx.subcode=ti128 or lx.subcode=tu128 then
!					lxerror("No suffix on i128/u128")
!				fi
!				case lx.symptr^.index
!				when million_unit then lx.value *:= 1 million
!				when billion_unit then lx.value *:= 1 billion
!				when thousand_unit then lx.value *:= 1 thousand
!				when kilo_unit then lx.value *:= 1024
!				when mega_unit then lx.value *:= 1048576
!				when giga_unit then lx.value *:= (1048576*1024)
!				else
!					lxerror("Can't do this unit index")
!				esac
!				lx.subcode:=setinttype(lx.value)
!			else
!				lxerror("Unit suffix after float not implem")
!			esac
!			goto reenter
!		else
!			lx.symbol:=namesym
!			lxsvalue:=lx.symptr^.name
!		fi
!	when namesym then						!matches existing name
!		lxerror("NEXT NAME!!!")
!
!	when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,kforallsym,
!			kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
!			krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
!			ktrysym,ktabledatasym,kassemsym,kifsym then
!
!		if lx.symbol=kendsym then
!			lx.subcode:=lx.symbol			!turn end if to single end/if token
!			goto reenter
!		fi
!	when opsym then
!		goto doopsym
!	when sysconstsym then					!ST ENTRY LIMITED TO 16 bits signed
!		case lx.subcode
!		when con_const then
!			lx.symbol:=intconstsym
!			lx.value:=0
!			lx.subcode:=tint
!		when nil_const then
!			lx.symbol:=intconstsym
!			lx.value:=0
!			lx.subcode:=tref
!		when pi_const then
!			lx.symbol:=realconstsym
!			lx.xvalue:=3.1415926535897932384626
!			lx.subcode:=treal
!		when tab_const then
!			lx.symbol:=stringconstsym
!			lxsvalue:="\t"
!			lxlength:=1
!		else
!			lxerror("sysconst?")
!		esac
!	when machinetypesym then
!		case lx.subcode
!		when 'I','i' then lx.subcode:=(targetbits=32|ti32|ti64)
!		when 'W','w' then lx.subcode:=(targetbits=32|tu32|tu64)
!		esac
!		lx.symbol:=stdtypesym
!	end switch
!
!when rawxnamesym then
!	lookup()
!	lx.symbol:=namesym				!convert to actual identifier
!	return
!
!when eolsym then
!
!	switch lx.symbol
!	when commasym, lsqsym, lbracksym, !ignore eol
!		 assignsym,semisym then
!		goto reenter
!	when opsym then
!		if not assemmode then
!			goto reenter
!		fi
!		lx.symbol:=semisym
!	else										!convert to semicolon
!		lx.symbol:=semisym
!	end switch
!
!when stringconstsym then
!	if lx.symbol=stringconstsym then
!		n:=lxlength+lx.length
!		p:=pcm_alloc(n+1)
!		memcpy(p,lx.svalue,lx.length)
!		memcpy(p+lx.length,lxsvalue,lxlength)
!		(p+n)^:=0
!		lx.svalue:=p
!		lx.length:=n
!		goto reenter
!	fi
!when opsym then
!doopsym::
!	if lx.subcode=j_in and lx.symbol=opsym and lx.subcode=j_notl then
!		lx.subcode:=j_notin
!		goto reenter
!	fi
!when eofsym then
!endswitch
!
!end

global proc showhashtablesize=
int i,n

n:=0
for i:=0 to hstmask do
	if hashtable[i].name then
		++n
	fi
od
end

!global function checkname(ichar name,int length=0)int=
!!nextlx contains a rawnamesym
!!return if if it is the same as name
!!length is the name length, or 0 (-1 better for empty strings) to work it out
!!note that lxsvalue is not zero-terminated
!
!if length=0 then
!	length:=strlen(name)
!fi
!if lxlength=length and memcmp(lxsvalue,name,length)=0 then
!	return 1
!fi
!return 0
!end
!
function getstrfile(ichar filename,int32 &length)ichar=
!locate module within search paths for strinclude
!return pointer to loaded/in-memory file (or nil on error)
	

ichar file
static [300]char filespec
int i

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
int fileno
ichar basefile,sptr,path

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
lxfileno_stack[sourcelevel]:=lxfileno
lxlineno_stack[sourcelevel]:=lx.pos
isfile_stack[sourcelevel]:=isfile

lxstart:=lxsptr:=sptr
lx.pos:=1
lxfileno:=fileno
end

proc unstacksource=
if sourcelevel>0 then			!check that some source is stacked
	lxstart:=lxstart_stack[sourcelevel]
	lxsptr:=lxsptr_stack[sourcelevel]
	lx.pos:=lxlineno_stack[sourcelevel]
	lxfileno:=lxfileno_stack[sourcelevel]
	--sourcelevel
fi
end

proc readarraystring(int prefix)=
++lxsptr
lxreadstring('"')
lx.symbol:=astringconstsym
lx.subcode:=toupper(prefix)
end

!proc qadd(ref qint aa,bb)=
!!add bb to aa
!word64 low,high
!
!low:=aa^.lower+bb^.lower
!if aa^.lower>low then		!overflow
!	++aa^.upper
!fi
!
!aa^.lower:=low
!aa^.upper+:=bb^.upper
!end
!
!proc qadddigit(ref qint aa,int x)=
!!add small positive int x to a; x can be zero
!word64 low,high
!
!low:=aa^.lower+x
!if aa^.lower>low then		!overflow
!	++aa^.upper
!fi
!
!aa^.lower:=low
!end
!
!proc qshift(ref qint aa,int n)=
!!show aa left by n bits
!word64 low,high,overflow
!
!to n do
!	overflow:=0
!	if aa^.lower iand 0x8000'0000'0000'0000 then	!overflow
!		overflow:=1
!	fi
!	aa^.lower<<:=1
!	aa^.upper:=aa^.upper<<1+overflow
!
!od
!end
!
!proc qmul10(ref qint aa)=
!!multiply aa by 10
!word64 low,high
!qint bb
!
!bb:=aa^
!qshift(aa,2)		!*4
!qadd(aa,&bb)		!*5
!qshift(aa,1)		!*10
!end
!
!proc qmulbase(ref qint aa,int base)=
!!multiply aa by 10
!word64 low,high
!qint bb
!case base
!when 16 then
!	qshift(aa,4)
!when 10 then
!	qmul10(aa)
!else
!	bb:=aa^
!	to base-1 do
!		qadd(aa,&bb)
!	od
!esac
!end

function stringtonumber128(ichar s, int length,base)ref int128=
ref int128 aa
int c,d

aa:=pcm_allocz(int128.bytes)

to length do
	aa^:=aa^*base
!	qmulbase(aa,base)

		c:=s++^

		if c>='a' then
			d:=c-'a'+10
		elsif c>='A' then
			d:=c-'A'+10
		else
			d:=c-'0'
		fi

	aa^:=aa^+d
!	qadddigit(aa,d)
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
	int c,hsum,length

	lx.svalue:=lxsptr
	hsum:=0

	doswitch c:=lxsptr++^
	when 'A'..'Z','a'..'z','0'..'9','_','$' then
		hsum:=hsum<<4-hsum+c
	else
		--lxsptr
		exit
	end doswitch

	length:=lxsptr-lx.svalue

	if length=0 then
		lxerror("Bad ` name")
	fi
	lookup(lx.svalue,length, (hsum<<5-hsum) iand hstmask)
	lx.symbol:=rawxnamesym

	return
end

proc lxerror(ichar mess)=
	cpl "LEX ERROR",nalllines

	abortprogram(mess)
end

proc lxerror_s(ichar mess,s)=
	lxerror(mess)
end

proc lxreadstring(int termchar)=
!start from char just after " or ' (termchar will be " or ')

	ichar s,t
	int c, d, length, hasescape
	[8]char str

!CPL "READSTRING",CHR(TERMCHAR)

	if termchar='"' then
		lx.symbol:=stringconstsym
	else
		lx.symbol:=charconstsym
		lx.subcode:=tint
	fi

	s:=lxsptr

!do a first pass that terminates length of final string
	length:=0
	hasescape:=0

	doswitch c:=lxsptr++^
	when '\\' then			!escape char
		c:=lxsptr^
		if c in 'A'..'Z' then c+:=' ' fi
		++lxsptr
		hasescape:=1

		switch c
		when 'a','b','c','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
			++length
		when 'w' then
			++length
		when 'x' then	!2-digit hex code follows
			lxsptr+:=2
			++length
		else
			lxerror("Bad str escape")
		endswitch
	when '"','\'' then		!possible terminators
		if c=termchar then		!terminator char
!CPL "TERM SEEN",CHR(C)
			if lxsptr^=c then		!repeated, assume embedded term char
				++lxsptr
				++length
			else			!was end of string
				exit
			fi
		else
			++length
		fi
	when cr,lf,0 then
		lxerror("String not terminated")
	else
		++length
	end doswitch

!CPL "READSTRING",=LENGTH
	if length=0 then
		lx.svalue:=""
		return
	elsif not hasescape then
		lx.svalue:=pcm_copyheapstringn(s,length)
!CPL =LX.SVALUE
		return
	fi

!need to copy string to dest and expand the escape codes

	lx.svalue:=t:=pcm_alloc(length+1)

	do
		switch c:=s++^
		when '\\' then			!escape char
			c:=s^
			if c>='A'  and c<='Z' then c+:=' ' fi
			++s
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
				t++^:=cr
				c:=lf
			when 'x' then	!2-digit hex code follows
				c:=0
				to 2 do
					case d:=s++^
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
!			println c,char(c),=lx.pos
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
		when cr,lf,0 then
			lxerror("String not terminated")
		endswitch

		t++^:=c
	od

	t^:=0
end

proc do_name(ichar s, int length, hashindex)=

!do quick lookup for when name already exists in ST, and matches at first try

	lx.symptr:=&hashtable[hashindex]

	case lx.symptr.namelen
	when 1 then
!CP "1"
		if lx.symptr.name^=s^ then
			lx.symbol:=lx.symptr.symbol
			lx.subcode:=lx.symptr.subcode
			return
		fi
	when length then
!CP "*"
		if memcmp(lx.symptr.name,s,length)=0 then
			lx.symbol:=lx.symptr.symbol
			lx.subcode:=lx.symptr.subcode
			return
		fi
	esac
	lookup(s,length, hashindex)
end

proc extendtokenlist(int ntokens)=
	ref[]tokenrec oldtokenlist
	int oldtokenlistsize

	oldtokenlistsize:=tokenlistsize
	oldtokenlist:=tokenlist

CPL "EXTENDING TOKEN LIST TO", TOKENLISTSIZE,"TO",TOKENLISTSIZE*2

	tokenlistsize*:=2

	tokenlist:=pcm_alloc(tokenrec.bytes*tokenlistsize)

	memcpy(tokenlist,oldtokenlist,ntokens*tokenrec.bytes)

	pcm_free(oldtokenlist,tokenrec.bytes*oldtokenlistsize)
end

global proc starttkscan(int moduleno)=
	nexttoken:=moduletable[moduleno].tklist
end

global function readtokens_a(int fileno, &ntokens)ref tokenrec=
	ref tokenrec lastlx
	ref char p
	int lena,lenb,lastsymbol

	newtokenlist()

	ntokens:=0
	lastsymbol:=0

	startlex("",fileno)

	repeat
!		lex()
		lexreadtoken()

!CPL "TOKEN:",SYMBOLNAMES[LX.SYMBOL]

!GOTO SKIP

		switch lx.symbol
		when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,
				kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
				krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
				ktrysym,ktabledatasym,kassemsym,kifsym then

			if lastsymbol=kendsym then
				if lastlx.subcode then lxerror("end if if?") fi
				lastlx.subcode:=lx.symbol
				next
			fi

!		when sysconstsym then
!			case lx.subcode
!			when con_const then
!				lx.symbol:=intconstsym
!				lx.value:=0
!				lx.subcode:=tint
!			when nil_const then
!				lx.symbol:=intconstsym
!				lx.value:=0
!				lx.subcode:=tref
!			when pi_const then
!				lx.symbol:=realconstsym
!				lx.xvalue:=3.1415926535897932384626
!				lx.subcode:=treal
!			when tab_const then
!				lx.symbol:=stringconstsym
!				lx.svalue:="\t"
!			else
!				lxerror("sysconst?")
!			esac

		when eolsym then
			if lastsymbol in [commasym, lsqsym, lbracksym] then !ignore eol
				next
			elsif symboloptypes[lastsymbol]=bin_op and not assemmode then
				next
			else
				lx.symbol:=semisym
			fi

		when stringconstsym then
			if lastsymbol=stringconstsym then
				lena:=strlen(lastlx.svalue)
				lenb:=strlen(lx.svalue)
				p:=pcm_alloc(lena+lenb+1)
				memcpy(p,lastlx.svalue,lena)
				memcpy(p+lena,lx.svalue,lenb)
				(p+lena+lenb)^:=0
				lastlx.svalue:=p
				next
			fi
		when ksourcedirsym then
			if not dolexdirective(lx.subcode) then		!skip symbol
				next
			fi

		when namesym then
			if lx.subcode=unitnamesym then
				case lastsymbol
				when intconstsym then
					if lastlx.subcode in [ti128,tu128] then
						lxerror("No suffix on i128/u128")
					fi
					case lx.symptr^.index
					when million_unit then lastlx.value *:= 1 million
					when billion_unit then lastlx.value *:= 1 billion
					when thousand_unit then lastlx.value *:= 1 thousand
					when kilo_unit then lastlx.value *:= 1024
					when mega_unit then lastlx.value *:= 1048576
					when giga_unit then lastlx.value *:= (1048576*1024)
					else
						lxerror("Can't do this unit index")
					esac
					lastlx.subcode:=setinttype(lastlx.value)
					next
				when realconstsym then
					lxerror("Unit suffix after float not implem")
				esac
			fi
		when machinetypesym then
			case lx.subcode
			when 'I','i' then lx.subcode:=ti64
			when 'W','w' then lx.subcode:=tu64
			esac
			lx.symbol:=stdtypesym

		when rawxnamesym then
!CPL "RAW NAME",lx.symptr.name
			lx.symbol:=namesym

		when insym then
			if lastsymbol=notlsym then
				lastlx.symbol:=notinsym
				next
			fi
		end switch

SKIP::
		if (ntokens+4) >= tokenlistsize then			!some margin
			extendtokenlist(ntokens)
		fi
		++ntokens

!		lx.fileno:=lxfileno					!pop dotslice not working?
		lx.pos :=lx.pos ior lxfileno<<24

		tokenlist[ntokens]:=lx
		lastlx:=&tokenlist[ntokens]
		lastsymbol:=lx.symbol
!CPL "STORING",LX.FILENO, LXFILENO

	until lx.symbol=eofsym

	tokenlist[ntokens+1].symbol:=eofsym					!end with 2 eofs

	return &tokenlist[1]
end

=== bb_decls.m 7/18 ===
import bb_tables

global const maxmodule=50
global const maxlibfile=50
global const maxsourcefile=1000

global type unit = ref unitrec


global record tokenrec =		!should be 32-byte record
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
	ref strec def
	ref procrec nextproc
end

global record typenamerec=
	ref strec owner			!owner of scope where typename was encountered
							!moduleno required by resolvetypename can be derived from owner
!A/B used as follows
!  nil B			Simple typename B
!  A   B			Dotted pair A.B
!  A   nil          I think represents a typeof(x) where x is a name
	ref strec defa
	union
		ref strec defb
		ref strec def
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
	ref strec deflistx
	ref strec nextdef
	ref strec nextdupl
	ref strec firstdupl			!point to generic version

	ref unitrec code			!var idata/proc body/taggedunion tag value/etc
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

!	int32 lineno
	word32 pos: (lineno:24, fileno:8)
	byte flags: (isglobal:1, isstatic:1, used:1, txdone:1, circflag:1, islet:1)
	byte moduleno
	byte imported			!1=imported name; 0=local or global name
	byte namecat

	union
		struct				!when a proc
			ichar truename			!for imported name only
			ref strec paramlist

			byte asmused			!1 when proc contains asmcode
			byte dllindex			!for dllproc: which dll in dlltable
			byte extmodno			!for proc call chains: module no of proc owner
			byte simplefunc
			byte fflang				!0 windowsff. etc.
			byte nretvalues			!function: number of return values (0 for proc)
			byte varparams			!0 or 1; variadic params in B and FF
		end

		struct				!when a record or record field
			ref strec equivfield
			uflagsrec uflags
			int32 base_class
			byte bitfieldwidth		!width of bitfield in record
			byte align				!0, 2, 4, 8, 16 or 255 (auto-align)
			byte at					!0 or 1 if @ used (fields only)
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
			byte equals				!0 or 1 if @ used (static/frame vars only)
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

	int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
	int16 importindex
	byte reftype			!AX fields
	byte segment

	byte regsize
	[7]byte spare

end

global record unitrec =
	byte tag				!kcode tag number
	byte hasa, hasb, hasc	!whether .a, .b or .c points to a unit/unitlist
!	int32 lineno			!source lineno associated with item; fileno is in top byte
	word32 pos: (lineno:24, fileno:8)

	ref unitrec nextunit

	union
		struct
			union
				ref unitrec		a
				ref strec		def
				ref strec		labeldef
				int64			value
				word64			uvalue
				real64			xvalue
				ichar			svalue
				int64			range_lower
			end

			union
				ref unitrec		b
				int64			range_upper
			end
		end
		int128					value128
		word128					uvalue128
	end

	union
		ref unitrec		c
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

		int32 opcode			!various?
		int32 index
	end

	int32 mode
	int32 newmode
	byte moduleno
	byte initlet		!1 for an assignment that initialises a let
	byte isconst		!1 for j_const, and j_makerange with const range
	byte popflag		!1 when this unit needs to be popped after evaluation.

	[4]byte spare
end


global record modulerec =
	ichar name
	ref strec stmodule
	int fileno
	union
		ichar asmstr
		ichar clangstr
	end
	int strlength
	[maxmodule]byte importmap
	ref strec stinitproc
	ref tokenrec tklist
!	int16 moduleno
end

global const maxsearchdirs=10
global [maxsearchdirs]ichar searchdirs
global int nsearchdirs=0

global ref strec stprogram		!root into the symbol table
global ref strec stmodule		!main module
global ref strec stsysmodule	!optional sys module (needed for name resolving)
global ref strec alldeflist		!link together all (user) symbols

global tokenrec lx				!provides access to current token data
!global lexrec nextlx

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]ichar inputfiles
global [0..maxlibfile]ichar libfiles
global [0..maxsourcefile]ichar sourcefilenames
global [0..maxsourcefile]ichar sourcefilepaths
global [0..maxsourcefile]ichar sourcefiletext
global [0..maxsourcefile]int sourcefilesizes
global int nmodules
global int nsourcefiles
global int ninputfiles
global int nlibfiles

global ref strec currmodule
global int currmoduleno				!used when compiling modules

global const int maxtype=5'000

global int ntypes

global [0:maxtype]ref strec ttnamedef
global [0:maxtype]ref strec	ttowner		!for	ttlowerexpr/rtlengthexpr

global [0:maxtype]int32	ttbasetype		!basetype
global [0:maxtype]ichar	ttname
global [0:maxtype]byte	tttypecat

global [0:maxtype]int32	ttsize
global [0:maxtype]byte	ttsizeset
global [0:maxtype]int32	ttlower 		!.lbound (default 1)
global [0:maxtype]int32	ttlength 		!elements in array/record/tuple
global [0:maxtype]ref[]int32	ttmult 	!ttlength elements in tuple

global [0:maxtype]unit	ttdimexpr		!length, lower:length, or lower..upper

global [0:maxtype]int32	tttarget 		!for array/ref types
global [0:maxtype]int32	ttkeytype 		!for dict
global [0:maxtype]byte	ttusercat
global [0:maxtype]int32	ttlineno

global [0:maxtype]byte	ttisint			!is i8 i16 i32 i64 i128
global [0:maxtype]byte	ttisword			!is u8 u16 u32 u64 u128
global [0:maxtype]byte	ttisreal			!is r32 r64
global [0:maxtype]byte	ttisinteger		!is i8..i64/u8..u64/c8..c64
global [0:maxtype]byte	ttisnumeric		!is int/word/char/real
global [0:maxtype]byte	ttisshortint		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0:maxtype]byte	ttisref			!is a pointer

global const int maxtypename=5'000
global [0:maxtypename]typenamerec typenames
global [0:maxtypename]posrec typenamepos
global int ntypenames

global [0:maxtype]byte typestarterset

global ref strec currproc

global int mlineno=0		!set in pclgen dispatcher
global int alineno=0

global int debug=0
global int assemmode=0

global ref procrec proclist			!linked list of all procs
global int nproclist
global ref procrec staticlist		!linked list of all static
global int nstaticlist

GLOBAL INT NUNITS

!global int fvarnames=0		!display of names in asm/mcl

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

global byte fverbose=0		!whether to display message for each pass
global byte fquiet=0

!global int foptimise=0		!whether to generate optimised j-codes

global byte fnobsys
global byte fvarnames=0		!display of names in asm/mcl

global byte fbundled=0		!1 when .ma file is being compiler
global ichar bafilename
global byte fwriteba

global byte fexe
global byte fobj
global byte fwritelibs
global byte fshowtiming
global byte fshowss
global byte fshowpcl
global byte fshowmcl
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte fshowoverloads
global byte foptimise
global byte fshowasm
global byte fcheckunusedlocals=0

global byte dointlibs=1

!global tabledata() []ichar ccmodenames =
!	(compile_mode,	$),
!	(link_mode,		$),
!	(run_mode,		$),
!end
!
!global int cc_mode			!compile_mode/link_mode/run_mode

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
	(run_pass,		$),		!
end

!passlevel used for compiler debug only
global int passlevel=0
global int prodpasslevel=0
global int debugmode=0

global ichar outfile					!one of the following two
global ichar outfilesource				!.asm or .c filename
global ichar outfilebin				!.exe or .obj filename
global ichar destfilename				!nil, or sets outfilebin

!global ichar linkoption				!exe or obj

global ref strec extendtypelist

global [0:jtagnames.len]ref overloadrec overloadtable

global int labelno=0
global [sysfnnames.len]int sysfnlabels
global [sysfnnames.len]int sysfnproclabels

=== bb_tables.m 8/18 ===
global tabledata() [0:]ichar stdtypenames, [0:]byte stdtypebits,
		 [0:]byte stdtypecats =
	(tvoid=0,		$,		0,		0),

	(ti8,			$,		8,		tc_d124),
	(ti16,			$,		16,		tc_d124),
	(ti32,			$,		32,		tc_d124),
	(ti64,			$,		64,		tc_d8),
	(ti128,			$,		128,	tc_d16),

	(tu1,			$,		1,		tc_bit),
	(tu2,			$,		2,		tc_bit),
	(tu4,			$,		4,		tc_bit),

	(tu8,			$,		8,		tc_d124),
	(tu16,			$,		16,		tc_d124),
	(tu32,			$,		32,		tc_d124),
	(tu64,			$,		64,		tc_d8),
	(tu128,			$,		128,	tc_d16),

	(tc8,			$,		8,		tc_d124),
	(tc16,			$,		16,		tc_d124),
	(tc64,			$,		64,		tc_d8),

	(tr32,			$,		32,		tc_d124),
	(tr64,			$,		64,		tc_d8),

	(tref,			$,		64,		tc_d8),

	(tenum,			$,		0,		tc_d8),

	(tauto,			$,		0,		0),
	(tany,			$,		0,		0),
	(tproc,			$,		0,		0),
	(tlabel,		$,		0,		0),
	(tgenerator,	$,		128,	0),
	(ttype,			$,		64,		tc_d8),
	(tbitfield,		$,		8,		tc_bit),

	(trange,		$,		128,	tc_d16),
	(tarray,		$,		0,		tc_blk),
	(tsmallarray,	$,		0,		tc_d8),
	(tbits,			$,		0,		tc_blk),
	(tsmallbits,	$,		0,		tc_d8),
	(trecord,		$,		0,		tc_blk),
	(tsmallrecord,	$,		0,		tc_d8),
	(ttaggedunion,	$,		0,		tc_blk),
	(ttuple,		$,		0,		0),
!	(tmult,			$,		0,		0),

	(trefbit,		$,		128,	tc_d16),
	(tslice,		$,		128,	tc_d16),
	(tslice2d,		$,		128,	tc_d16),
	(tflex,			$,		128,	tc_d8),

	(tstring,		$,		64,		tc_man8),
	(tmanarray,	"tarray",	64,		tc_man8),
	(tmanbits,		$,		64,		tc_man8),
	(tset,			$,		64,		tc_man8),
	(tdict,			$,		64,		tc_man8),
	(tdecimal,		$,		64,		tc_man8),
	(tmanrecord,	$,		64,		tc_man8),

	(tparam1,		$,		0,		0),
	(tparam2,		$,		0,		0),
	(tparam3,		$,		0,		0),
	(tparam4,		$,		0,		0),

	(tpending,		$,		0,		0),

	(tlast,			$,		0,		0)
end

global tabledata() [0:]ichar typecatnames =
	(tc_none=0,		$),
	(tc_d8,			$),		!generic 64-bit value, int/ptr/float/small block
	(tc_x8,			$),		!64-bit float when it needs to be an actual float
	(tc_d16,		$),		!128-bit value (int/slice/small block etc)
	(tc_blk,		$),		!N-byte block
	(tc_man8,		$),		!Handle of managed, ref-counted type

	(tc_d124,		$),		!8/16/32-bit value
	(tc_x4,			$),		!32-bit float
	(tc_bit,		$),		!1..7 bits (can be more for some bitfields)
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64

global const tfirstnumtype = ti8
global const tlastnumtype  = tr64
global const maxtuplesize = 4

global int trefproc
global int treflabel
global int trefchar

global tabledata() [0:]ichar jtagnames, [0:]byte jisexpr=
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unaru or binary op 

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!	L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!	u means .a/b/c pointing to a single unit
!	u/nil means can be nil


![a=u] means a is a unit/list, or is nil

	(j_none=0,		$,		0), ! For tagname lookups when tag is zero
	(j_const,		$,		3), ! value/etc=value, typeno=type code
	(j_null,		$,		3), ! Place holder unit: means 'param no present' when used where a param is expected
	(j_name,		$,		3), ! def=nameptr
	(j_block,		$,		0), ! a=L
	(j_stmtblock,	$,		0), ! a=L
	(j_decimal,		$,		3), ! svalue=str, slength
	(j_assem,		$,		0), ! svalue=str, slength
	(j_assemmacro,	$,		0), !
	(j_assemreg,	$,		0), !
	(j_assemxreg,	$,		0), !
	(j_assemmem,	$,		0), !

!Logical Operators

	(j_andl,		$,		2), ! a b	This group are for conditional expressions (no result)
	(j_andb,		$,		2), ! a b
	(j_orl,			$,		2), ! a b
	(j_orb,			$,		2), ! a b
	(j_xorl,		$,		2), ! a b
	(j_xorb,		$,		2), ! a b
	(j_notl,		$,		1), ! a
	(j_istruel,		$,		1), ! a

!Expressions and Operators

	(j_makelist,	$,		3), ! a=L, b=[u], length=N; element list/lower bound expr
	(j_makerange,	$,		3), ! a b
	(j_makeset,		$,		3), ! a=L, length=N
	(j_makedict,	$,		3), !
	(j_makeslice,	$,		3), !
	(j_exprlist,	$,		3), ! a=u...	List of expressions, as (a;b;c), rather than (a,b,c)
	(j_multexpr,	$,		3), !
	(j_returnmult,	$,		3), !

	(j_keyword,		$,		3), ! def=st entry
	(j_keyvalue,	$,		3), ! a b
	(j_assignx,		$,		3), ! a b
	(j_deepcopyx,	$,		3), ! a b
	(j_callfn,		$,		3), ! a b
!	(j_applyop,		$,		0), ! opcode b c
!	(j_applyopx,	$,		1), ! opcode b c
	(j_new,			$,		3), ! newmode=T, a=L, length=N
	(j_destroy,		$,		0), ! a=L, length=N
	(j_clear,		$,		0), !

!Binary Ops

	(j_eq,			$,		2), ! a b
	(j_ne,			$,		2), ! a b
	(j_lt,			$,		2), ! a b
	(j_le,			$,		2), ! a b
	(j_gt,			$,		2), ! a b
	(j_ge,			$,		2), ! a b

	(j_same,		$,		2), ! a b

	(j_add,			$,		2), ! a b
	(j_sub,			$,		2), ! a b
	(j_mul,			$,		2), ! a b
	(j_div,			$,		2), ! a b
	(j_idiv,		$,		2), ! a b
	(j_irem,		$,		2), ! a b
	(j_idivrem,		$,		2), ! a b
	(j_iand,		$,		2), ! a b
	(j_ior,			$,		2), ! a b
	(j_ixor,		$,		2), ! a b
	(j_shl,			$,		2), ! a b
	(j_shr,			$,		2), ! a b
	(j_in,			$,		2), ! a b
	(j_notin,		$,		2), ! a b
	(j_inrev,		$,		2), ! a b
	(j_inrange,		$,		2), ! a b
!	(j_notinrange,	$,		1), ! a b
	(j_inset,		$,		2), ! a b
!	(j_notinset,	$,		1), ! a b
	(j_min,			$,		2), ! a b
	(j_max,			$,		2), ! a b
	(j_subref,		$,		2), ! a b
	(j_addoffset,	$,		2), ! a b
	(j_suboffset,	$,		2), ! a b
	(j_concat,		$,		2), ! a b
	(j_append,		$,		2), ! a b
	(j_clamp,		$,		2), ! a b

!	(j_insert,		$,		1), ! a b
!	(j_delete,		$,		1), ! a b

	(j_prepend,		$,		2), ! a b
	(j_flexptr,		$,		3), ! a b
	(j_stringz,		$,		3), ! a b
	(j_sliceptr,	$,		3), ! a b

	(j_index,		$,		3), ! a b		a[b]
	(j_slice,		$,		3), ! a b		a[b]
	(j_dot,			$,		3), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotindex,	$,		3), ! a b		a[b]
	(j_dotslice,	$,		3), ! a b		a[b]
	(j_anddotslice,	$,		3), ! a b		a[b]
	(j_anddotindex,	$,		3), ! a b		a[b]

	(j_power,		$,		2), ! a b	a**b				int/real

	(j_ptr,			$,		3), ! a		a^
	(j_addrof,		$,		3), ! a		&a
	(j_addroffirst,	$,		3), ! a		&a
	(j_convert,		$,		3), ! typeno=T a		T(a)			T
!	(j_convertref,	$,		1), ! typeno=T a		T(a)			T
	(j_autocast,	$,		3), ! typeno=T a		T(a)			T
	(j_typepun,		$,		3), ! typeno=T a		T@(a)			T
	(j_typeconst,	$,		3), ! typeno=T			typeconst(T)
	(j_operator,	$,		3), ! opcode=opc
	(j_upper,		$,		3), ! a		$					T

!Monadic Ops

	(j_neg,			$,		1), ! a		-a
	(j_abs,			$,		1), ! a		abs a
	(j_inot,		$,		1), ! a
	(j_chr,			$,		1), ! a
	(j_asc,			$,		1), ! a
	(j_sqrt,		$,		1), ! a
	(j_sqr,			$,		1), ! a

	(j_sign,		$,		1), ! a

	(j_maths,		$,		1), ! a
!	(j_sin,			$,		1), ! a
!	(j_cos,			$,		1), ! a
!	(j_tan,			$,		1), ! a
!	(j_asin,		$,		1), ! a
!	(j_acos,		$,		1), ! a
!	(j_atan,		$,		1), ! a
!	(j_ln,			$,		1), ! a
!	(j_lg,			$,		1), ! a
!	(j_log,			$,		1), ! a
!	(j_exp,			$,		1), ! a
!	(j_round,		$,		1), ! a
!	(j_floor,		$,		1), ! a
!	(j_ceil,		$,		1), ! a
!	(j_fract,		$,		1), ! a
	(j_fmod,		$,		1), ! a

	(j_lwb,			$,		1), ! a		a.lwb				int
	(j_upb,			$,		1), ! a							int
	(j_len,			$,		1), ! a							int
	(j_bounds,		$,		1), ! a							int
	(j_lenstr,		$,		1), ! a							int
	(j_bitwidth,	$,		1), ! a
	(j_bytesize,	$,		1), ! a
	(j_typeof,		$,		3), ! a
	(j_typestr,		$,		1), ! a
!	(j_sliceptr,	$,		1), ! a
	(j_bitfield,	$,		3), ! a

	(j_minvalue,	$,		3), ! a
	(j_maxvalue,	$,		3), ! a

!Increment

	(j_preincrx,	$,		3), ! a	++a
	(j_predecrx,	$,		3), ! a	--a
	(j_postincrx,	$,		3), ! a	a++
	(j_postdecrx,	$,		3), ! a	a--
	(j_incr,		$,		3), ! a	++a
	(j_decr,		$,		3), ! a	--a

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
	(j_andbto,		$,		0), ! a b
	(j_orbto,		$,		0), ! a b
	(j_xorbto,		$,		0), ! a b
	(j_shlto,		$,		0), ! a b
	(j_shrto,		$,		0), ! a b
	(j_andlto,		$,		0), ! a b
	(j_orlto,		$,		0), ! a b
	(j_appendto,	$,		0), ! a b
	(j_concatto,	$,		0), ! a b
	(j_minto,		$,		0), ! a b
	(j_maxto,		$,		0), ! a b
	(j_addoffsetto,	$,		0), ! a b
	(j_suboffsetto,	$,		0), ! a b

	(j_negto,		$,		0), ! a		-:=a
	(j_absto,		$,		0), ! a
	(j_inotto,		$,		0), ! a
	(j_notlto,		$,		0), ! a

!Translator Variables

	(j_cvlineno,	$,		3), ! 
	(j_cvstrlineno,	$,		3), ! 
	(j_cvmodulename,$,		3), ! 
	(j_cvfilename,	$,		3), ! 
	(j_cvfunction,	$,		3), ! 
	(j_cvdate,		$,		3), ! 
	(j_cvtime,		$,		3), ! 
	(j_cvversion,	$,		3), ! 
	(j_cvtypename,	$,		3), ! 
	(j_cvtargetbits,$,		3), ! 
	(j_cvtargetsize,$,		3), ! 
	(j_cvtargetcode,$,		3), ! 
	(j_cvnil,		$,		3), ! 
	(j_cvpi,		$,		3), ! 

	(j_whenthen,	$,		0), ! a=L b=u
	(j_elsif,		$,		0), ! opcode=condcode, a
	(j_fmtitem,		$,		3), ! a b  x/fmtstr
	(j_nogap,		$,		3), ! 

!Statements

	(j_callproc,	$,		0), ! a=fn b=L, length
	(j_return,		$,		0), ! a=x/nil
	(j_syscall,		$,		0), ! a=x or nil

	(j_assign,		$,		0), ! a b
	(j_deepcopy,	$,		0), ! a b
	(j_to,			$,		0), ! a=N, b=body, c=tempvar/nil, def=name
	(j_if,			$,		3), ! condcode a=then b=else
	(j_longif,		$,		3), ! a=(elsif ...) b=else		L is series of kelsif pairs
	(j_forup,		$,		0), ! a=x b=x c=x d=Body [e=Else]		Body is single item or list
	(j_fordown,		$,		0), ! a=x b=x c=x d=Body [e=Else]
	(j_while,		$,		0), ! a=x b=u
	(j_repeat,		$,		0), ! a=u b=x
	(j_goto,		$,		0), ! a=x
	(j_labeldef,	$,		0), ! def=nameptr
	(j_restart,		$,		0), ! [a=x]
	(j_redo,		$,		0), ! [a=x]
	(j_next,		$,		0), ! [a=x]
	(j_exit,		$,		0), ! [a=x]
	(j_do,			$,		0), ! [a=u
	(j_case,		$,		3), ! a=x b=L [c=else]		L is series of whenthen pairs
	(j_docase,		$,		0), ! a=x b=L [c=else]
	(j_switch,		$,		3), ! a=x b=L [c=else]
	(j_doswitch,	$,		0), ! a=x b=L [c=else]
	(j_swap,		$,		0), ! a b
	(j_select,		$,		3), ! Not implemented
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
	(j_eval,		$,		3), ! "


	(j_dummy,		$,		3)
end

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

global tabledata() [0:]ichar optypenames =
	(no_op=0,		$),
	(bin_op,		$),
	(mon_op,		$),
	(prop_op,		$),
end

global tabledata() []ichar symbolnames,
					[]byte symboloptypes,
					[]byte symboljtags,
					[]byte symboljtotags,
					[]byte symbolopprios,
					[]byte exprstarter =
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$,			0,	0,	0,	0,	0),		! Lex error
	(dotsym,			".",		0,	0,	0,	0,	0),		! "."
	(lexdotsym,			$,			0,	0,	0,	0,	0),		! ".", used at bol to prefix lexical 
	(anddotsym,			"&.",		0,	0,	0,	0,	1),		! "&."
	(commasym,			",",		0,	0,	0,	0,	0),		! ","
	(semisym,			";",		0,	0,	0,	0,	0),		! ";"
	(colonsym,			":",		0,	0,	0,	0,	0),		! ":"
	(dcolonsym,			"::",		0,	0,	0,	0,	0),		! "::"
	(assignsym,			":=",		bin_op,	j_assign,	0,	1,	0),		! :=
	(deepcopysym,		"::=",		0,	0,	0,	1,	0),		! ::=
	(sendtosym,			"=>",		0,	0,	0,	0,	0),		! =>
	(lbracksym,			"(",		0,	0,	0,	0,	1),		! (
	(rbracksym,			")",		0,	0,	0,	0,	0),		! )
	(lsqsym,			"[",		0,	0,	0,	0,	1),		! [
	(rsqsym,			"]",		0,	0,	0,	0,	0),		! ]
	(lcurlysym,			"{",		0,	0,	0,	0,	0),		! {
	(rcurlysym,			"}",		0,	0,	0,	0,	0),		! }
	(ptrsym,			"^",		0,	0,	0,	0,	0),		! ^
	(barsym,			"|",		0,	0,	0,	0,	0),		! |
	(dbarsym,			"||",		0,	0,	0,	0,	0),		! ||
	(atsym,				"@",		0,	0,	0,	0,	0),		! @
	(datsym,			"@@",		0,	0,	0,	0,	0),		! @@
	(questionsym,		"?",		0,	0,	0,	0,	0),		! ?
	(addrsym,			"&",		0,	0,	0,	0,	1),		! &
	(daddrsym,			"&&",		0,	0,	0,	0,	0),		! &&
	(curlsym,			"~",		0,	0,	0,	0,	0),		! ~
	(rangesym,			"..",		bin_op,	j_makerange,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,	0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,	0,	0,	0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		j_add,		j_addto,	4,	1),
	(subsym,			"-",		bin_op,		j_sub,		j_subto,	4,	1),
	(mulsym,			"*",		bin_op,		j_mul,		j_multo,	3,	0),
	(divsym,			"/",		bin_op,		j_div,		j_div,		3,	0),
	(idivsym,			"%",		bin_op,		j_idiv,		j_idiv,		3,	0),
	(iremsym,			"rem",		bin_op,		j_irem,		j_iremto,	3,	0),
	(iandsym,			"iand",		bin_op,		j_iand,		j_iandto,	4,	0),
	(iorsym,			"ior",		bin_op,		j_ior,		j_iorto,	4,	0),
	(ixorsym,			"ixor",		bin_op,		j_ixor,		j_ixorto,	4,	0),
	(shlsym,			"<<",		bin_op,		j_shl,		j_shlto,	3,	0),
	(shrsym,			">>",		bin_op,		j_shr,		j_shrto,	3,	0),
	(minsym,			"min",		bin_op,		j_min,		j_minto,	4,	1),
	(maxsym,			"max",		bin_op,		j_max,		j_maxto,	4,	1),
	(andlsym,			"and",		bin_op,		j_andl,		0,			7,	0),
	(orlsym,			"or",		bin_op,		j_orl,		0,			8,	0),
	(xorlsym,			"xor",		bin_op,		j_xorl,		0,			4,	0),
	(andbsym,			"andb",		bin_op,		j_andb,		j_andbto,	4,	0),
	(orbsym,			"orb",		bin_op,		j_orb,		j_orbto,	4,	0),
	(xorbsym,			"xorb",		bin_op,		j_xorb,		j_xorbto,	4,	0),
	(eqsym,				"=",		bin_op,		j_eq,		0,			6,	1),
	(nesym,				"<>",		bin_op,		j_ne,		0,			6,	0),
	(ltsym,				"<",		bin_op,		j_lt,		0,			6,	0),
	(lesym,				"<=",		bin_op,		j_le,		0,			6,	0),
	(gesym,				">=",		bin_op,		j_ge,		0,			6,	0),
	(gtsym,				">",		bin_op,		j_gt,		0,			6,	0),
	(prependsym,		"prepend",	bin_op,		j_prepend,	0,			4,	0),
	(appendsym,			"append",	bin_op,		j_append,	j_appendto,	4,	0),
	(concatsym,			"concat",	bin_op,		j_concat,	j_concatto,	4,	0),
	(idivremsym,		"idivdem",	bin_op,		j_idivrem,	0,			3,	0),
	(powersym,			"**",		bin_op,		j_power,	0,			2,	0),
	(samesym,			"==",		bin_op,		j_same,		0,			6,	0),
	(insym,				"in",		bin_op,		j_in,		0,			6,	0),
	(notinsym,			"notin",	bin_op,		j_notin,	0,			6,	0),
	(inrevsym,			"inrev",	0,	0,	0,	0,	0),

!	(opsym2,			$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(negsym,			"$neg",		mon_op,		j_neg,		0,			0,	1),
	(notlsym,			"not",		mon_op,		j_notl,		0,			0,	1),
	(istruesym,			"istrue",	mon_op,		j_istruel,	0,			0,	1),
	(inotsym,			"inot",		mon_op,		j_inot,		j_inotto,	0,	1),
	(abssym,			"abs",		mon_op,		j_abs,		j_absto,	0,	1),
	(sqrtsym,			"sqrt",		mon_op,		j_sqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		j_sqr,		0,			0,	1),

	(lensym,			"len",		prop_op,	j_len,		0,			0,	0),
	(lwbsym,			"lwb",		prop_op,	j_lwb,		0,			0,	0),
	(upbsym,			"upb",		prop_op,	j_upb,		0,			0,	0),
	(boundssym,			"bounds",	prop_op,	j_bounds,	0,			0,	0),
	(lenstrsym,			"lenstr",	prop_op,	j_lenstr,	0,			0,	0),
	(bitwidthsym,		"bitwidth",	prop_op,	j_bitwidth,	0,			0,	0),
	(bytessym,			"bytes",	prop_op,	j_bytesize,	0,			0,	0),
	(typestrsym,		"typestr",	prop_op,	j_typestr,	0,			0,	0),
	(minvaluesym,		"minvalue",	prop_op,	j_minvalue,	0,			0,	0),
	(maxvaluesym,		"maxvalue",	prop_op,	j_maxvalue,	0,			0,	0),

	(mathsopsym,		$,		0,	0,	0,	0,	1),		! sin etc
	(bitfieldsym,		$,		0,	0,	0,	0,	0),		! Special bit selections
	(eolsym,			$,		0,	0,	0,	0,	0),		! End of line
	(eofsym,			$,		0,	0,	0,	0,	0),		! Eof seen
	(rawxnamesym,		$,		0,	0,	0,	0,	0),		! unassigned name, case-sensitive, that is never a reserved word
	(docstringsym,		$,		0,	0,	0,	0,	0),		! ! #comment used as documentation string
	(incrsym,			$,		0,	0,	0,	0,	1),		! 1/2 = ++/--; later may add +2 for x++/x--
	(intconstsym,		$,		0,	0,	0,	0,	1),		! 123 32 bits signed
	(decimalconstsym,	$,		0,	0,	0,	0,	1),		! 123 or 123.4 decimal
	(realconstsym,		$,		0,	0,	0,	0,	1),		! 123.4 64 bits
	(charconstsym,		$,		0,	0,	0,	0,	1),		! 'A' or 'ABCD'
	(wcharconstsym,		$,		0,	0,	0,	0,	1),		! 'A'W or 'ABCD'W (but don't have a syntax yet)
	(stringconstsym,	$,		0,	0,	0,	0,	1),		! "ABC"
	(astringconstsym,	$,		0,	0,	0,	0,	1),		! A"ABC"
	(wstringconstsym,	$,		0,	0,	0,	0,	1),		! "ABC"W

!Second half are tokens that can be yielded after a name lookup::
	(unitnamesym,		$,		0,	0,	0,	0,	0),		! 
	(namesym,			$,		0,	0,	0,	0,	1),		! identifier symbol
	(ksourcedirsym,		$,		0,	0,	0,	0,	0),		! 
!	(lexmacronamesym,	$,		0,	0,	0,	0,	0),		! 
	(regsym,			$,		0,	0,	0,	0,	0),		! x64 registers
	(xregsym,			$,		0,	0,	0,	0,	0),		! XMM registers
	(fregsym,			$,		0,	0,	0,	0,	0),		! ST registers
	(mregsym,			$,		0,	0,	0,	0,	0),		! MMX registers
	(jmpccsym,			$,		0,	0,	0,	0,	0),		! 
	(setccsym,			$,		0,	0,	0,	0,	0),		! 
	(movccsym,			$,		0,	0,	0,	0,	0),		! 
	(segnamesym,		$,		0,	0,	0,	0,	0),		! 
	(asmopcodesym,		$,		0,	0,	0,	0,	0),		! MOV etc

	(stdtypesym,		$,		0,	0,	0,	0,	1),		! INT, CHAR etc
	(machinetypesym,	$,		0,	0,	0,	0,	1),		! INTM etc
!	(packtypesym,		$,		0,	0,	0,	0,	0),		! Byte etc
	(ktypeofsym,		$,		0,	0,	0,	0,	0),		! TYPEOF
	(ksubrangesym,		$,		0,	0,	0,	0,	0),		! SUBRANGE
	(koutsym,			$,		0,	0,	0,	0,	0),		! OUT
	(kicharsym,			$,		0,	0,	0,	0,	1),		! ICHAR
	(kifsym,			$,		0,	0,	0,	0,	1),		! 
	(kthensym,			$,		0,	0,	0,	0,	0),		! 
	(kelsifsym,			$,		0,	0,	0,	0,	0),		! 
	(kelsesym,			$,		0,	0,	0,	0,	0),		! 
	(kelsecasesym,		$,		0,	0,	0,	0,	0),		! 
	(kelseswitchsym,	$,		0,	0,	0,	0,	0),		! 
	(kelseselectsym,	$,		0,	0,	0,	0,	0),		! 
	(kendsym,			$,		0,	0,	0,	0,	0),		! 
	(kunlesssym,		$,		0,	0,	0,	0,	0),		! 
	(kcasesym,			$,		0,	0,	0,	0,	1),		! CASE
	(kdocasesym,		$,		0,	0,	0,	0,	0),		! DOCASE
	(krecasesym,		$,		0,	0,	0,	0,	0),		! RECASE
	(kwhensym,			$,		0,	0,	0,	0,	0),		! 
	(kforsym,			$,		0,	0,	0,	0,	0),		! FOR
	(ktosym,			$,		0,	0,	0,	0,	0),		! TO/DOWNTO
	(kbysym,			$,		0,	0,	0,	0,	0),		! 
	(kdosym,			$,		0,	0,	0,	0,	0),		! 
	(kwhilesym,			$,		0,	0,	0,	0,	0),		! 
	(krepeatsym,		$,		0,	0,	0,	0,	0),		! 
	(kuntilsym,			$,		0,	0,	0,	0,	0),		! 
	(kreturnsym,		$,		0,	0,	0,	0,	0),		! 
	(kstopsym,			$,		0,	0,	0,	0,	0),		! 
	(kloopsym,			$,		0,	0,	0,	0,	0),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$,		0,	0,	0,	0,	0),		! GO/GOTO
	(kswitchsym,		$,		0,	0,	0,	0,	0),		! SWITCH
	(kdoswitchsym,		$,		0,	0,	0,	0,	0),		! DOSWITCH
	(kprintsym,			$,		0,	0,	0,	0,	0),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		$,		0,	0,	0,	0,	0),		! SPRINT/SFPRINT
	(kreadsym,			$,		0,	0,	0,	0,	0),		! READ/READLN
	(ksreadsym,			$,		0,	0,	0,	0,	0),		! SREAD
	(ksreadlnsym,		$,		0,	0,	0,	0,	0),		! SREADLN
	(kprocsym,			$,		0,	0,	0,	0,	0),		! PROC
	(kfunctionsym,		$,		0,	0,	0,	0,	0),		! FUNCTION
!	(kmethodsym,		$,		0,	0,	0,	0,	0),		! METHOD
	(klabelsym,			$,		0,	0,	0,	0,	0),		! LABEL
	(krecordsym,		$,		0,	0,	0,	0,	0),		! RECORD
	(kstructsym,		$,		0,	0,	0,	0,	0),		! STRUCT
	(kunionsym,			$,		0,	0,	0,	0,	0),		! UNION
	(ktaggedunionsym,	$,		0,	0,	0,	0,	0),		! TAGGEDUNION
	(kimportsym,		$,		0,	0,	0,	0,	0),		! IMPORT
	(kimportmodulesym,	$,		0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
	(kimportpathsym,	$,		0,	0,	0,	0,	0),		! IMPORTPATH
	(kmapmodulesym,		$,		0,	0,	0,	0,	0),		! MAPMODULE
	(ktypesym,			$,		0,	0,	0,	0,	0),		! TYPE
	(ktypealiassym,		$,		0,	0,	0,	0,	0),		! TYPEALIAS
	(kextendtypesym,	$,		0,	0,	0,	0,	0),		! EXTENDTYPE
	(krefsym,			$,		0,	0,	0,	0,	1),		! REF
	(kmutsym,			$,		0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,		0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,		0,	0,	0,	0,	0),		! SLICE/SLICE2D
!	(karraysym,			$,		0,	0,	0,	0,	0),		! ARRAY
	(kdictsym,			$,		0,	0,	0,	0,	0),		! DICT
!	(kflexsym,			$,		0,	0,	0,	0,	0),		! FLEX
	(kmacrosym,			$,		0,	0,	0,	0,	0),		! MACRO
	(kexpandsym,		$,		0,	0,	0,	0,	0),		! EXPAND
	(koperatorsym,		$,		0,	0,	0,	0,	0),		! OPERATOR
	(kconstsym,			$,		0,	0,	0,	0,	0),		! 
	(kenumsym,			$,		0,	0,	0,	0,	0),		! 
	(knewsym,			$,		0,	0,	0,	0,	0),		! NEW
	(kdestroysym,		$,		0,	0,	0,	0,	0),		! DESTROY
	(kclearsym,			$,		0,	0,	0,	0,	0),		! CLEAR
	(kclasssym,			$,		0,	0,	0,	0,	0),		! CLASS
!	(kdirectivesym,		$,		0,	0,	0,	0,	0),		! TARGET/MODULE
	(kfflangsym,		$,		0,	0,	0,	0,	0),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		$,		0,	0,	0,	0,	0),		! global
	(kstaticsym,		$,		0,	0,	0,	0,	0),		! STATIC

	(ktrysym,			$,		0,	0,	0,	0,	0),		! 
	(kexceptsym,		$,		0,	0,	0,	0,	0),		! 
	(kfinallysym,		$,		0,	0,	0,	0,	0),		! 
	(kraisesym,			$,		0,	0,	0,	0,	0),		! 
	(kyieldsym,			$,		0,	0,	0,	0,	0),		! 
	(kcastsym,			$,		0,	0,	0,	0,	1),		! CAST
	(ktypeconstsym,		$,		0,	0,	0,	0,	0),		! TYPECONST
	(compilervarsym,	$,		0,	0,	0,	0,	1),		! $lineno etc
	(dollarsym,			$,		0,	0,	0,	0,	1),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$,		0,	0,	0,	0,	0),		! EVAL
	(ktabledatasym,		$,		0,	0,	0,	0,	0),		! tabledata
	(kstacksym,			$,		0,	0,	0,	0,	0),		! STACK/UNSTACK
	(kclampsym,			$,		0,	0,	0,	0,	1),			! CLAMP
	(kswapsym,			$,		0,	0,	0,	0,	0),		! SWAP
	(kerrorsym,			$,		0,	0,	0,	0,	0),		! PC_ERROR etc
!	(sysconstsym,		$,		0,	0,	0,	0,	0),		! nil, etc
	(kassemsym,			$,		0,	0,	0,	0,	0),		! ASM/ASSEM
	(ksyscallsym,		$,		0,	0,	0,	0,	1),		! $get_procname etc

	(kdummysym,			$,		0,	0,	0,	0,	0),		!
end

global tabledata() []ichar sourcedirnames =
	(includedir,	$),
	(strincludedir,	$),
	(binincludedir,	$),
	(textincludedir,$),
	(defineunitdir,	$),
end

!global tabledata() =
!	(nil_const),
!	(pi_const),
!	(tab_const),
!	(con_const)
!end

global tabledata() [0:]ichar fflangnames=
	(noff=0,		$), ! 
	(windowsff,		$), ! 
	(clangff,		$), ! 
	(thislangff,	$), ! 
	(blangff,		$), ! 
	(callbackff,	$), ! 
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
	(procid,		$,	proc_cat),		!Proc/method/function/op name
	(dllprocid,		$,	dllproc_cat),	!Dll Proc/function name
	(dllvarid,		$,	dllvar_cat),	!Dll variable name
	(genprocid,		$,	proc_cat),		!generic proc name
	(generatorid,	$,	proc_cat),		!generator proc name
	(constid,		$,	0),				!Named constant in type, proc or module
	(staticid,		$,	static_cat),	!Static in type or proc or module
	(frameid,		$,	frame_cat),		!Local var
	(paramid,		$,	frame_cat),		!Local param
	(fieldid,		$,	0),				!Field of Record or Class
	(genfieldid,	$,	0),				!Generic Field of Record or Class
	(enumid,		$,	0),				!Enum name, part of enum type only
	(labelid,		$,	0),				!Label name in proc only
	(blockid,		$,	0),				!Codeblock label name in proc only
	(aliasid,		$,	0),				!Alias to another name
	(macroid,		$,	0),				!Name of macro
	(macroparamid,	$,	0),				!Macro formal parameter name
	(linkid,		$,	0),				!Name in class defined in a base class
	(functionopid,	$,	0),				!Function-operator
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
	("extendtype",	kextendtypesym,	0),
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

	("type",		ktypesym,		0),
	("class",		kclasssym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("taggedunion",	ktaggedunionsym,0),
	("ref",			krefsym,		0),
	("var",			kmutsym,		0),
	("mut",			kmutsym,		0),
	("let",			kletsym,		0),

	("include",		ksourcedirsym,	includedir),
	("strinclude",	ksourcedirsym,	strincludedir),
	("bininclude",	ksourcedirsym,	binincludedir),
	("textinclude",	ksourcedirsym,	textincludedir),
	("defineunit",	ksourcedirsym,	defineunitdir),
	("macro",		kmacrosym,		0),
!	("expand",		kexpandsym,		0),
	("operator",	koperatorsym,	0),

	("assem",		kassemsym,		1),
	("asm",			kassemsym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),
!	("table",		kconstsym,		1),
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

	("new",			knewsym,		j_new),
	("destroy",		kdestroysym,	j_destroy),
	("clear",		kclearsym,		j_clear),

	("global",		kglobalsym,		1),
	("export",		kglobalsym,		2),

	("clang",		kfflangsym,		clangff),
	("blang",		kfflangsym,		thislangff),
	("windows",		kfflangsym,		windowsff),
	("callback",	kfflangsym,		callbackff),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),

	("word",		stdtypesym,		tword),

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

	("array",		kslicesym,		tmanarray),

	("string",		stdtypesym,		tstring),
	("set",			stdtypesym,		tset),
	("dict",		kdictsym,		0),
	("decimal",		stdtypesym,		tdecimal),
	("generator",	stdtypesym,		tgenerator),

	("$t",			stdtypesym,		tparam1),
	("$u",			stdtypesym,		tparam2),
	("$v",			stdtypesym,		tparam3),
	("$w",			stdtypesym,		tparam4),


!	("bitarray",	stdtypesym,		tbits),
!	("complex",		stdtypesym,		tcomplex64),
!	("string",		stdtypesym,		tflexstring),
!	("wstring",		stdtypesym,		tflexwstring),
!	("set",			stdtypesym,		tfixedset),
!	("decimal",		stdtypesym,		tflexdecimal),
!	("dict",		stdtypesym,		tflexdict),

	("range",		stdtypesym,		trange),
	("auto",		stdtypesym,		tauto),
!	("label",		stdtypesym,		tlabel),

!	("flex",		stdtypesym,		tvar),

	("intm",		machinetypesym,	'I'),
	("intp",		machinetypesym,	'i'),
	("wordm",		machinetypesym,	'W'),
	("wordp",		machinetypesym,	'w'),
	("slice",		kslicesym,		tslice),
	("slice2d",		kslicesym,		tslice2d),
	("flex",		kslicesym,		tflex),
!	("flex",		kflexsym,		0),
	("typeof",		ktypeofsym,			0),

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
	("nil",			compilervarsym,	j_cvnil),
	("pi",			compilervarsym,	j_cvpi),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("andb",		andbsym,		0),
	("or",			orlsym,			0),
	("orb",			orbsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
	("in",			insym,			0),
!	("notin",		notinsym,		0),
	("inrev",		inrevsym,		0),
	("rem",			iremsym,		0),
	("divrem",		idivremsym,		0),
	("min",			minsym,			0),
	("max",			maxsym,			0),

	("not",			notlsym,		0),
	("inot",		inotsym,		0),
	("istrue",		istruesym,		0),
	("abs",			abssym,			0),
	("$neg",		negsym,			0),

!	("asc",			opsym,			j_asc),
!	("tochr",		opsym,			j_chr),
	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),

	("sin",			mathsopsym,		mt_sin),
	("cos",			mathsopsym,		mt_cos),
	("tan",			mathsopsym,		mt_tan),
	("asin",		mathsopsym,		mt_asin),
	("acos",		mathsopsym,		mt_acos),
	("atan",		mathsopsym,		mt_atan),
	("atan2",		mathsopsym,		mt_atan2),
	("sign",		mathsopsym,		mt_sign),
	("ln",			mathsopsym,		mt_ln),
	("log",			mathsopsym,		mt_log),
	("lg",			mathsopsym,		mt_lg),
	("exp",			mathsopsym,		mt_exp),
	("round",		mathsopsym,		mt_round),
	("floor",		mathsopsym,		mt_floor),
	("ceil",		mathsopsym,		mt_ceil),
	("fract",		mathsopsym,		mt_fract),
	("fmod",		mathsopsym,		mt_fmod),

	("prepend",		prependsym,		0),
	("append",		appendsym,		0),
	("concat",		concatsym,		0),
!	("flexptr",		flexptrsym,		0),
!	("sliceptr",	sliceptrsym,		0),
!	("stringz",		stringzsym,		0),

	("len",			lensym,			0),
	("lwb",			lwbsym,			0),
	("upb",			upbsym,			0),
	("bounds",		boundssym,		0),
	("lenstr",		lenstrsym,		0),
	("bitwidth",	bitwidthsym,	0),
	("bytes",		bytessym,		0),
	("minvalue",	minvaluesym,	0),
	("maxvalue",	maxvaluesym,	0),
	("typestr",		typestrsym,		0),

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
	("od",			kendsym,	kdosym),
	("endproc",		kendsym,	kprocsym),
	("endfunction",	kendsym,	kfunctionsym),
	("endwhile",	kendsym,	kwhilesym),
	("endto",		kendsym,	ktosym),
	("enddo",		kendsym,	kdosym),
	("endunless",	kendsym,	kunlesssym),
	("endimportmodule",	kendsym,kimportmodulesym),
	("endtry",		kendsym,	ktrysym),
	("endrecord",	kendsym,	krecordsym),
	("endassem",	kendsym,	kassemsym),

!	("nil",			knilsym,		0),
!	("con",			sysconstsym,	con_const),
!	("pi",			sysconstsym,	pi_const),

	("$$dummy",		0,				0)
end

global tabledata() []ichar mathsnames =
	(mt_sign,		$),
	(mt_sin,		$),
	(mt_cos,		$),
	(mt_tan,		$),
	(mt_asin,		$),
	(mt_acos,		$),
	(mt_atan,		$),
	(mt_atan2,		$),
	(mt_ln,			$),
	(mt_lg,			$),
	(mt_log,		$),
	(mt_exp,		$),
	(mt_round,		$),
	(mt_floor,		$),
	(mt_ceil,		$),
	(mt_fract,		$),
	(mt_fmod,		$),
end

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
	(sysfn_print_strsl,			$),
	(sysfn_print_ptr,			$),
	(sysfn_print_c8,			$),
	(sysfn_print_newline,		$),
	(sysfn_print_end,			$),
	(sysfn_read_i64,			$),
	(sysfn_read_r64,			$),
	(sysfn_read_str,			$),
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
	(sysfn_lenstr_stringz,		$),

!var support

	(sysfn_initmemz_var,		$),
	(sysfn_freemem_var,			$),
	(sysfn_free_var,			$),
	(sysfn_share_var,			$),
	(sysfn_unshare_var,			$),
	(sysfn_dupl_var,			$),
	(sysfn_popmem_var,			$),
	(sysfn_storemem_var,		$),

	(sysfn_add_var,				$),
	(sysfn_sub_var,				$),
	(sysfn_mul_var,				$),
	(sysfn_div_var,				$),
	(sysfn_idiv_var,			$),
	(sysfn_irem_var,			$),
	(sysfn_power_var,			$),
	(sysfn_eq_var,				$),
	(sysfn_ne_var,				$),
	(sysfn_lt_var,				$),
	(sysfn_le_var,				$),
	(sysfn_ge_var,				$),
	(sysfn_gt_var,				$),
	(sysfn_isequal_var,			$),
	(sysfn_iand_var,			$),
	(sysfn_ior_var,				$),
	(sysfn_ixor_var,			$),
	(sysfn_shl_var,				$),
	(sysfn_shr_var,				$),
	(sysfn_andl_var,			$),
	(sysfn_orl_var,				$),
	(sysfn_append_var,			$),
	(sysfn_concat_var,			$),
	(sysfn_min_var,				$),
	(sysfn_max_var,				$),
	(sysfn_in_var,				$),

	(sysfn_neg_var,				$),
	(sysfn_abs_var,				$),
	(sysfn_inot_var,			$),
	(sysfn_notl_var,			$),
	(sysfn_istruel_var,		$),
	(sysfn_sqrt_var,			$),
	(sysfn_sin_var,				$),
	(sysfn_cos_var,				$),
	(sysfn_tan_var,				$),
	(sysfn_asin_var,			$),
	(sysfn_acos_var,			$),
	(sysfn_atan_var,			$),
	(sysfn_exp_var,				$),
	(sysfn_ln_var,				$),
	(sysfn_log_var,				$),
	(sysfn_round_var,			$),
	(sysfn_floor_var,			$),
	(sysfn_ceil_var,			$),
	(sysfn_fract_var,			$),
	(sysfn_asc_var,				$),
	(sysfn_chr_var,				$),
	(sysfn_lwb_var,				$),
	(sysfn_upb_var,				$),
	(sysfn_len_var,				$),
	(sysfn_bounds_var,			$),
!	(sysfn_share_var,			$),
!	(sysfn_unshare_var,			$),
!	(sysfn_free_var,			$),
!	(sysfn_dupl_var,			$),

	(sysfn_addto_var,			$),
	(sysfn_subto_var,			$),
	(sysfn_multo_var,			$),
	(sysfn_divto_var,			$),
	(sysfn_idivto_var,			$),
	(sysfn_iremto_var,			$),
	(sysfn_iandto_var,			$),
	(sysfn_iorto_var,			$),
	(sysfn_ixorto_var,			$),
	(sysfn_shlto_var,			$),
	(sysfn_shrto_var,			$),
	(sysfn_andto_var,			$),
	(sysfn_orto_var,			$),
	(sysfn_appendto_var,		$),
	(sysfn_concatto_var,		$),
	(sysfn_minto_var,			$),
	(sysfn_maxto_var,			$),

	(sysfn_negto_var,			$),
	(sysfn_absto_var,			$),
	(sysfn_inotto_var,			$),
	(sysfn_notlto_var,			$),
!	(sysfn_istruelto_var,		$),
	(sysfn_incrto_var,			$),
	(sysfn_decrto_var,			$),

	(sysfn_new_var,				$),
	(sysfn_print_var,			$),
	(sysfn_tostr_var,			$),
	(sysfn_getdot_var,			$),
	(sysfn_putdot_var,			$),
	(sysfn_getindex_var,		$),
	(sysfn_putindex_var,		$),
	(sysfn_getdotindex_var,		$),
	(sysfn_putdotindex_var,		$),
	(sysfn_getslice_var,		$),
	(sysfn_putslice_var,		$),
	(sysfn_getdotslice_var,		$),
	(sysfn_putdotslice_var,		$),
	(sysfn_getkeyindex_var,		$),
	(sysfn_putkeyindex_var,		$),
	(sysfn_insert_var,			$),
	(sysfn_delete_var,			$),
	(sysfn_resize_var,			$),

	(sysfn_make_int,			$),
	(sysfn_make_real,			$),
	(sysfn_make_string,			$),
	(sysfn_make_dec,			$),
	(sysfn_make_list,			$),
	(sysfn_make_listz,			$),
	(sysfn_make_array,			$),
	(sysfn_make_range,			$),

	(sysfn_var_to_int,			$),
	(sysfn_var_to_real,			$),
	(sysfn_var_to_string,		$),

end

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

	(c_anytovar,		$),
	(c_anytodecimal,	$),
	(c_ichartostring,	$),

	(c_vartoany,		$),
	(c_decimaltoany,	$),
	(c_stringtoichar,	$),

	(c_error,			$)
end

!dominantmode[s,t] returns the dominant type of s and t, widened to int/uint as needed
!only use on base types, to keep indices under 32
global [0:32,0:32]byte dominantmode

!conversionops[s,t] gives conversion op to convert numeric types s to t
!only use when both types are <16
global [0:32,0:32]byte conversionops

global [][4]byte typesetuptable=(
!---------------------
	(ti64,	ti64,	ti64,	c_none),
	(ti64,	ti128,	ti128,	c_iwiden),

	(ti64,	tu64,	ti64,	c_soft),
	(ti64,	tu128,	ti128,	c_uwiden),

	(ti64,	tc64,	ti64,	c_softtruncate),

	(ti64,	tr32,	tr64,	c_ifloat),
	(ti64,	tr64,	tr64,	c_ifloat),
!	(ti64,	tdec,	tdec,	c_diwiden),

	(ti64,	tref,	tvoid,	c_inttoref),
!---------------------
	(ti128,	ti64,	ti128,	c_softtruncate),
	(ti128,	ti128,	ti128,	c_none),

	(ti128,	tu64,	ti128,	c_soft),
	(ti128,	tu128,	ti128,	c_soft),

	(ti128,	tc64,	ti128,	c_softtruncate),

!	(ti128,	tr32,	tdec,	c_ifloat),
!	(ti128,	tr64,	tdec,	c_ifloat),
!	(ti128,	tdec,	tdec,	c_difloat),

	(ti128,	tref,	tvoid,	c_inttoref),
!---------------------
	(tu64,	ti64,	ti64,	c_soft),
	(tu64,	ti128,	ti128,	c_uwiden),

	(tu64,	tu64,	tu64,	c_none),
	(tu64,	tu128,	tu128,	c_uwiden),

	(tu64,	tc64,	tu64,	c_softtruncate),

	(tu64,	tr32,	tr64,	c_ufloat),
	(tu64,	tr64,	tr64,	c_ufloat),
!	(tu64,	tdec,	tdec,	c_dufloat),

	(tu64,	tref,	tvoid,	c_inttoref),
!---------------------
	(tu128,	ti64,	ti128,	c_soft),
	(tu128,	ti128,	ti128,	c_soft),

	(tu128,	tu64,	tu128,	c_none),
	(tu128,	tu128,	tu128,	c_none),

	(tu128,	tc64,	tu128,	c_softtruncate),

!	(tu128,	tr32,	tdec,	c_ufloat),
!	(tu128,	tr64,	tdec,	c_ufloat),
!	(tu128,	tdec,	tdec,	c_dufloat),

	(tu128,	tref,	tvoid,	c_inttoref),

!---------------------
	(tc64,	ti64,	ti64,	c_soft),
	(tc64,	ti128,	ti128,	c_uwiden),

	(tc64,	tu64,	tu64,	c_soft),
	(tc64,	tu128,	tu128,	c_uwiden),

	(tc64,	tc64,	tc64,	c_none),

	(tc64,	tr32,	tr64,	c_ufloat),
	(tc64,	tr64,	tr64,	c_ufloat),
!	(tc64,	tdec,	tdec,	c_dufloat),

	(tc64,	tref,	tvoid,	c_inttoref),

!---------------------
	(tr32,	ti64,	tr32,	c_ifix),
	(tr32,	ti128,	tr32,	c_ifix),

	(tr32,	tu64,	tr32,	c_ufix),
!	(tr32,	tu128,	tdec,	c_ufix),

	(tr32,	tc64,	tr32,	c_ufix),

	(tr32,	tr32,	tr32,	c_none),
	(tr32,	tr64,	tr64,	c_fwiden),
!	(tr32,	tdec,	tdec,	c_dfwiden),

	(tr32,	tref,	tvoid,	c_error),
!---------------------
	(tr64,	ti64,	tr64,	c_ifix),
!	(tr64,	ti128,	tdec,	c_ifix),

	(tr64,	tu64,	tr64,	c_ufix),
!	(tr64,	tu128,	tdec,	c_ufix),

	(tr64,	tc64,	tr64,	c_ufix),

	(tr64,	tr32,	tr64,	c_fnarrow),
	(tr64,	tr64,	tr64,	c_none),
!	(tr64,	tdec,	tdec,	c_dfwiden),

	(tr64,	tref,	tvoid,	c_error),
!---------------------
!	(tdec,	ti64,	tdec,	c_difix),
!	(tdec,	ti128,	tdec,	c_difix),

!	(tdec,	tu64,	tr64,	c_dufix),
!	(tdec,	tu128,	tdec,	c_dufix),

!	(tdec,	tc64,	tr64,	c_dufix),

!	(tdec,	tr32,	tr64,	c_dfnarrow),
!	(tdec,	tr64,	tr64,	c_dfnarrow),
!	(tdec,	tdec,	tdec,	c_none),

!	(tdec,	tref,	tvoid,	c_error),
!---------------------
	(tref,	ti64,	tvoid,	c_reftoint),
	(tref,	ti128,	tvoid,	c_error),

	(tref,	tu64,	tvoid,	c_reftoint),
	(tref,	tu128,	tvoid,	c_error),

	(tref,	tr32,	tvoid,	c_error),
	(tref,	tr64,	tvoid,	c_error),
!	(tref,	tdec,	tvoid,	c_error),

	(tref,	tref,	tref,	c_reftoref))

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,kenumsym,krecordsym,
		kicharsym, ktypeofsym, kslicesym, kdictsym)

=== bb_support.m 9/18 ===
import clib
import msys
import mlib
import oslib

import bb_decls
import bb_lib
import bb_tables
!import mm_gen

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global function loadsourcefile(ichar filespec)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
	ichar s,shortfile

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi

	shortfile:=extractfile(filespec)

!CPL "LOADSOURCEFILE",FILESPEC

	++nsourcefiles
	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(filespec)
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		loaderror("LSF can't load ",filespec)
	fi
	sourcefiletext[nsourcefiles]:=s

!	if fwritema then
!		mafiletext[nsourcefiles]:=pcm_copyheapstring(s)
!	fi
!
	sourcefilesizes[nsourcefiles]:=rfsize
	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return nsourcefiles
end

global function loadbuiltin(ichar shortfile, text)int=
!loading build-in header with given text
!Name of header is in 'file'.
	ichar s
	[128]char str

!CPL "LOADBUILTIN",SHORTFILE
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
!	if fwritema then
!		mafiletext[nsourcefiles]:=pcm_copyheapstring(text)
!	fi
!
	sourcefilesizes[nsourcefiles]:=strlen(text)
	return nsourcefiles
end

global function loadbundledfile(ichar filespec)int fileno=
!loading bundled file
!Name of header is in 'file'.
	ichar file
	int n,lastmatch

	file:=extractfile(filespec)

!	for i to nmafiles do
!		if eqstring(file,mafilenames[i]) then		!found
!			fileno:=mafilefileno[i]
!			if not fileno then					!cannot overflow sourcefiles; same limits?
!				fileno:=++nsourcefiles
!				mafilefileno[i]:=fileno
!
!				sourcefilepaths[nsourcefiles]:=mafilenames[i]
!				sourcefilenames[nsourcefiles]:=mafilenames[i]
!				sourcefiletext[nsourcefiles]:=mafiletext[i]
!				sourcefilesizes[nsourcefiles]:=mafilesizes[i]
!
!!				if mafilemult[i] then				!might be parses multiple times
!					sourcefiletext[nsourcefiles]:=pcm_copyheapstring(mafiletext[i])
!!				fi
!!			ELSE
!!				CPL "FOUND BUNDLED FILE SUBSEQ TIME",FILE
!
!			fi
!			return fileno
!		fi
!	od
!!
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

println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]

println
println "**** Syntax Error:",mess,"****"
stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

proc stopcompiler(ichar filename,int lineno)=
	filehandle f
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
[256]char str
fprint @&.str,mess,a
serror_gen(&.str)
end

global proc error_gen(int pass,ichar mess,unit p=nil)=
!general error handling for passes name, type and code gen
!pass='N' 'T' or 'G'
int lineno,fileno

!CPL "ERRORGEN",=P,=MLINENO,=P.LINENO

if p then
	fileno:=p.fileno
	lineno:=p.lineno
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
when 'A' then print "**** 'AX Code Gen Error: "
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

!global proc axerror(ichar mess)=
!CPL =ALINENO
!error_gen('A',mess)
!end
!
global proc txerror(ichar mess,unit p=nil)=
error_gen('T',mess,p)
end

global proc txerror_s(ichar mess,a,unit p=nil)=
[256]char str
fprint @&.str,mess,a
error_gen('T',&.str,p)
end

global proc txerror_ss(ichar mess,a,b)=
[256]char str
fprint @&.str,mess,a,b
error_gen('T',&.str)
end

global proc rxerror_s(ichar mess,a,unit p=nil)=
[256]char str
fprint @&.str,mess,a
error_gen('N',&.str,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
[256]char str

fprint @&.str,mess,s
error_gen('G',&.str,p)
end

global proc lxerror_gen(ichar mess)=

println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno]

println
println "**** Lex Error:",mess,"****"
println

stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
end

global proc lxerror_s(ichar mess,a)=
[256]char str
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

int a:=1
while a<x do
	a<<:=1
od
return a
end

global proc loaderror(ichar mess,mess2="",mess3="")=
	[512]char str
	fprint @&.str,mess,mess2,mess3

	println "Load Error:",&.str
	println "Stopping"
	stop 1
end

global proc gs_additem(ref strbuffer dest,ichar s)=		!GENITEM
!like genstr, but ensure there is white space separation as needed from the last output
ichar d
int lastchar,nextchar

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
int i,size,bitsize
int s,t,u,v

!Initialise type tt-tables from std types first all fields initially zero

for i:=0 to tlast-1 do

	ttname[i]:=stdtypenames[i]+1
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
!	ttbitwidth[i]:=bitsize

	ttisint[i]		:= (ti8<=i<=ti128|1|0)
	ttisword[i]		:= (tu1<=i<=tu128|1|0)
!	ttischar[i]		:= (tc8<=i<=tc64|1|0)

!	ttiswordchar[i] := ttisword[i] ior ttischar[i]

	ttisreal[i]		:= (tr32<=i<=tr64|1|0)

	ttisinteger[i]	:= ttisint[i] ior ttisword[i] ior (tc8<=i<=tc64)

	ttisnumeric[i]	:= ttisinteger[i] ior ttisreal[i]
	ttisshortint[i]	:= ttisinteger[i] and ttsize[i]<8

!	ttisbit[i]		:= (tu1<=i<=tu4|1|0)

	tttypecat[i]	:= stdtypecats[i]

	if i=tref then
		ttisref[i]		:= 1
	fi

	ttlower[i]:=1
od
ttsize[tref]:=8

!ttbitwidth[tref]:=64

ntypes:=tlast-1

!set up dominant/conversion lookup tables from linear table
for i:=1 to typesetuptable.len do
	s:=typesetuptable[i,1]
	t:=typesetuptable[i,2]
	u:=typesetuptable[i,3]
	v:=typesetuptable[i,4]

	dominantmode[s,t]:=u
	conversionops[s,t]:=v

od
!CPL "COPS::::",CONVERSIONOPS[TI64,TC8]

end

global proc addspecialtypes=
!abortprogram("createrefmode")
	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
	trefchar:=createrefmode(nil,tc8,0)
end

function findfile(ichar filename)ichar=
!look for file by searching through search dirs
!return nil when not found, or the name of the sucessful filespec
!locate module within search paths
!return full filespec
	static [300]char filespec

!CPL "FIND FILE:",FILENAME,=NSEARCHDIRS

	for i:=nsearchdirs downto 1 do
!CPL "FIND",I,SEARCHDIRS[I]
		strcpy(&.filespec,searchdirs[i])
		strcat(&.filespec,filename)

		if checkfile(&.filespec) then
			return &.filespec
		fi
	od

	return nil
end

global function findstdlib(ichar name)ichar=
!	for i:=1 to stdlibnames.len do
!		if eqstring(name,stdlibnames[i]) then
!			return stdlibtext[i]
!		fi
!	od
	return nil
end

global function getmainfile(ichar filename)int =
!locate and load lead module filename
!	if fbundled then
!		return loadbundledfile(filename)
!	fi
	if not checkfile(filename) then
		loaderror("Can't find main module: ##",filename)
	fi
	return loadsourcefile(filename)
end

global function getmodulefile(ichar modulename, ownername)int =
	[300]char filename
	ichar file,libtext

	strcpy(&.filename,addext(modulename,"b"))

!	if fbundled then
!		return loadbundledfile(&.filename)
!	fi

!	if dointlibs then
!		libtext:=findstdlib(&.filename)
!		if libtext then
!			return loadbuiltin(&.filename,libtext)
!		fi
!	fi

	file:=findfile(&.filename)

	if file=nil then
		loaderror("Can't find import module: # imported in: #",modulename,ownername)
	fi
!CP "GETMODULEFILE:"
	return loadsourcefile(file)
end

global function getsupportfile(ichar filename)int =
	ichar path,file

!CPL "GET SUPPORTFILE<",,FILENAME,,">"

!	if fbundled then
!		return loadbundledfile(filename)
!	fi

	path:=extractpath(filename)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		file:=filename
	else
!CPL "DOING FIND"
		file:=findfile(filename)
	fi

!CPL =FILE

	if file=nil or not checkfile(file) then
		loaderror("Can't find include file: # #",filename)
	fi

!CP "GETSUPPRTFILE:"
!	for i to nsourcefiles do
!		if eqstring(file,sourcefilepaths[i]) then
!!CPL "SUPPORTFILE",FILE,"ALREADY PRESENT"
!			return i
!		fi
!	od

!CPL "LOADING SOURCE:",FILE

	return loadsourcefile(file)
end

!global proc writemafile(ichar leadmodule,destfile)=
!	[256]char filename
!	filehandle f
!	[maxsourcefile]int fileoffsets, headeroffsets
!	int offset,nn,NEWOFFSET
!
!	strcpy(&.filename, changeext(leadmodule,"ma"))
!
!	if destfile then
!		strcpy(&.filename,destfile)
!	fi
!
!	println "Writing MA File",&.filename
!
!	f:=fopen(&.filename,"wb")
!	if not f then loaderror("Can't create ma file #",&.filename) fi
!
!!CPL =NSOURCEFILES
!!
!
!	println @f,"mafile",nsourcefiles
!
!	for i to nsourcefiles do
!		print @f,i:"3",sourcefilenames[i]:"16jl",sourcefilesizes[i]:"7"
!		headeroffsets[i]:=getfilepos(f)+1
!		println @f,"         "
!	od
!
!	for i to nsourcefiles do
!		fprintln @f,"=== # #/# ===",sourcefilenames[i],i,nsourcefiles
!
!		offset:=getfilepos(f)
!		fileoffsets[i]:=offset
!		nn:=writerandom(f,cast(mafiletext[i]),offset,sourcefilesizes[i])
!	od
!
!!Note: the first "=" of the "===" that follows each file may be replaced
!!by a zero-terminator after the .ma is loaded
!	println @f,"=== end ==="

!	for i to nsourcefiles do
!		setfilepos(f,headeroffsets[i])
!		print @f,fileoffsets[i]:"8"
!	od
!!
!	fclose(f)
!end
!
!global proc loadmafile=
!	filehandle f
!	[16]char kwd
!	[256]char filename
!	int index, size, offset
!
!	f:=fopen(mafilename,"rb")
!	if not f then
!		loaderror("Can't open ##",mafilename)
!	fi
!
!	readln @f
!
!	readstr(&.kwd,'n',kwd.len)
!	if not eqstring(&.kwd,"mafile") then
!		loaderror("Bad sig in ma file: # '#'",mafilename,&.kwd)
!	fi
!	read nmafiles
!
!	for i to nmafiles do
!		readln @f,index
!		readstr(&.filename,'n',filename.len)
!		read size, offset
!		mafilenames[i]:=pcm_copyheapstring(&.filename)
!		mafilesizes[i]:=size
!		mafileoffsets[i]:=offset
!		mafilefileno[i]:=0
!		mafilemult[i]:=0
!	od
!	fclose(f)
!
!!Directory has been read. Now read whole file into memory, use directory
!!to set up mafiletext values to each file, and add in terminator
!	mafilesource:=cast(readfile(mafilename))
!	if not mafilesource then loaderror("MA load?") fi
!
!	for i to nmafiles do
!		size:=mafilesizes[i]
!		offset:=mafileoffsets[i]
!
!		mafiletext[i]:=mafilesource+offset
!		(mafilesource+offset+size)^:=0
!	od
!end
!
!

global function mapimport(ichar name)ichar=
	for i to nmodulemap do
		if eqstring(name,genericmodules[i]) then
			return actualmodules[i]
		fi
	od
	return name
end

global proc initbblib=
int i

!translate into an instant lookup format
!for i:=1 to oplist.len do
!	jtagpriotable[oplist[i]]:=oppriolist[i]
!od
!
!for i:=1 to D_exprstarterset.len do exprstarterset[D_exprstarterset[i]]:=1 od
for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od

!for i:=1 to D_boolunitset.len do boolunitset[D_boolunitset[i]]:=1 od
!for i:=1 to D_refunitset.len do refunitset[D_refunitset[i]]:=1 od

!for i:=1 to D_monopset.len do monopset[D_monopset[i]]:=1 od

!condopset[j_eq]:=1
!condopset[j_ne]:=1
!condopset[j_lt]:=1
!condopset[j_le]:=1
!condopset[j_ge]:=1
!condopset[j_gt]:=1

end

=== bb_lib.m 10/18 ===
import msys
import mlib
import clib
import oslib

import bb_decls
import bb_tables
import bb_support
import bb_lex
import bb_diags

![0..4000]int callcounts

int autotypeno=0
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

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
	(j_idivto,		"%:="),
	(j_iremto,		"rem:="),
	(j_iandto,		"iand:="),
	(j_iorto,		"ior:="),
	(j_ixorto,		"ixor:="),
	(j_shlto,		"<<:="),
	(j_shrto,		">>:="),
	(j_andlto,		"and:="),
	(j_orlto,		"or:="),
	(j_appendto,	"append:="),
	(j_concatto,	"concat:="),
	(j_minto,		"min:="),
	(j_maxto,		"max:="),

	(j_negto,		"-:="),
	(j_absto,		"abs:="),
	(j_inotto,		"inot:="),
	(j_notlto,		"not:="),

	(j_preincrx,	"++"),
	(j_postincrx,	"++"),
	(j_predecrx,	"--"),
	(j_postdecrx,	"--"),

	(0,				"")
end

const int unitheapsize=32768
ref unitrec unitheapptr=nil
int remainingunits=0

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

global ichar framevarname			!normally nil, set to frame var def to display in comment

global function newstrec:ref strec=
ref strec p
p:=pcm_alloc(strec.bytes)
memset(p,0,strec.bytes)

p^.pos:=lx.pos
p^.moduleno:=currmoduleno
return p
end

global function getduplnameptr(ref strec owner,symptr,int id)ref strec=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
ref strec p,q

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
ref strec q

if q:=p^.nextdupl then
	if q^.owner=owner then
!RETURN
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
ref unitrec u

u:=allocunitrec()
u^.tag:=j_name
u^.def:=p

return u
end

global function createunit0(int tag)ref unitrec=
ref unitrec u

u:=allocunitrec()
u^.tag:=tag
return u
end

global function createunit1(int tag, ref unitrec p)ref unitrec=
ref unitrec u
u:=allocunitrec()
u.tag:=tag
u.a:=p
u.hasa:=1
return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
ref unitrec u

u:=allocunitrec()

u^.tag:=tag
u^.a:=p
u^.b:=q
u.hasa:=1
u.hasb:=1
return u
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
ref unitrec u

u:=allocunitrec()
u^.tag:=tag
u^.a:=p
u^.b:=q
u^.c:=r
u.hasa:=1
u.hasb:=1
u.hasc:=1
return u
end

global proc insertunit(unit p,int tag)=		!INSERTUNIT
!wrap extra unit around p, with given tag
!p itself is effectively changed
unit q,nextunit
int mode

q:=allocunitrec()
q^:=p^
mode:=q^.mode
nextunit:=q^.nextunit
q^.nextunit:=nil

memset(p,0,unitrec.bytes)
p^.tag:=tag
p^.lineno:=q^.lineno
p^.a:=q
p.hasa:=1
!CPL "INSERTUNIT",STRMODE(MODE)
p^.mode:=mode
p^.nextunit:=nextunit
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
unit r:=p^.nextunit
p^:=q^
p^.nextunit:=r
end

global function createconstunit(word64 a, int t)ref unitrec=
ref unitrec u
u:=allocunitrec()
u^.tag:=j_const
u^.value:=a
u^.mode:=t

!CPL "CREATECONST",STRMODE(T)
if t in [ti128,tu128] then
	u.value128:=ref int128(a)^
fi

u^.isconst:=1
return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
ref unitrec u
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
static [0:jtagnames.len]int16 opctotable
int n,opcto,i
[20]char str

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

!global function createtentativetype(ref strec d)int=			!CREATETYPE
!	int m
!
!!CPL "NEW TENT TYPE",D.NAME,NAMENAMES[D.NAMEID],=D.MODE
!	if d.nameid<>nullid then
!		serror("createtent/not nullid")
!	fi
!	if d.mode then
!		return d.mode
!	fi
!
!	m:=createusertype(d)
!	ttbasetype[m]:=ttentative
!
!	if ntentativetypes>=maxtentativetype then
!		serror("Too many tentative types")
!	fi
!	tentativetypes[++ntentativetypes]:=d
!
!	return m
!end

global function newtypename(ref strec a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	fi
	++ntypenames
	typenames[ntypenames].defa:=a		!leave .owner/.pmode to be filled in
	typenames[ntypenames].defb:=b		!used type's mode is used

	typenamepos[ntypenames].pos:=lx.pos

	return -ntypenames
end

global function createusertype(ref strec stname)int=
!create new, named user type
if ntypes>=maxtype then
cpl ntypes,stname^.name
	serror("Too many types")
fi

++ntypes
ttname[ntypes]:=stname^.name

ttnamedef[ntypes]:=stname
ttbasetype[ntypes]:=tvoid
ttlineno[ntypes]:=lx.pos

stname^.mode:=ntypes

return ntypes
end

global function createusertypefromstr(ichar name)int=
!create new, named user type
ref strec stname
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
int k,m

!CPL "CAM1",OWNER.NAME

if typedefx=0 then		!anon type
	for k:=tlast to ntypes do
		if ttusercat[k]=0 and ttbasetype[k]=tarray and tttarget[k]=target and
				sameunit(dimexpr, ttdimexpr[k],owner, ttowner[k]) then
!			ttlower[k]=lower and ttlength[k]=length then
			return k
		fi
	od
!CPL "ARRAY"
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi

!ttbasetype[m]:=tarray
ttbasetype[m]:=(target in [tu1,tu2,tu4]|tbits|tarray)
ttlower[m]:=1
ttdimexpr[m]:=dimexpr
!tttarget[m]:=target
storemode(owner,target,tttarget[m])
ttowner[m]:=owner
tttypecat[m]:=tc_blk

return m
end

function sameunit(unit p,q, ref strec powner=nil, qowner=nil)int=
!p are q are units just parses; no name resolving or type checking
!do a simple check to see if they are the same unit
	ref strec d,e

	if p=q then return 1 fi
	if p=nil or q=nil then return 0 fi

	if p.tag<>q.tag then return 0 fi

	case p.tag
	when j_const then
		return p.value=q.value
	when j_makerange,j_keyvalue then
		return sameunit(p.a, q.a) and sameunit(p.b, q.b)
	when j_name then
		if p.def=q.def and powner=qowner then
			return 1
		fi
	esac

	return 0

end

global function createarraymodek(ref strec owner,int target,int lower,length, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
int atype,k,m

atype:=tarray

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

storemode(owner,target,tttarget[m])
!tttarget[m]:=target
ttowner[m]:=owner

return m
end

!global function createsetmode(ref strec owner,unit dimexpr, int typedefx)int=		!CREATEARRAYMODE
!int k,m
!
!if typedefx=0 then		!anon type
!	m:=createusertypefromstr(nextautotype())
!else
!	m:=typedefx
!fi
!
!ttbasetype[m]:=tset
!ttlower[m]:=0
!ttdimexpr[m]:=dimexpr
!ttowner[m]:=owner
!
!return m
!end
!
!global function createsetmodek(ref strec owner,int length, int typedefx)int=		!CREATEARRAYMODE
!int k,m
!
!if typedefx=0 then		!anon type
!	m:=createusertypefromstr(nextautotype())
!else
!	m:=typedefx
!fi
!
!ttbasetype[m]:=tset
!ttlower[m]:=0
!ttlength[m]:=length
!ttowner[m]:=owner
!
!return m
!end

global function nextautotype:ichar=
static [32]char str

!sprintf(&.str,(ctarget|"_T$%lld"|"$T%lld"),++autotypeno)
print @&.str,"$T",,++autotypeno
return &.str
end

global proc converttoslice(int t,sltype)=
ttbasetype[t]:=sltype
ttsize[t]:=ttsize[tslice]
end

global function createslicemode(ref strec owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
int k,m

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

ttbasetype[m]:=slicetype
if dimexpr then
	ttdimexpr[m]:=dimexpr
else
	ttlower[m]:=1
fi
storemode(owner,target,tttarget[m])
!tttarget[m]:=target
ttowner[m]:=owner
tttypecat[m]:=tc_d16

return m
end

global function createslicemodek(ref strec owner,int target,lower, int typedefx=0)int=
!lower is lower bound of array
!length is length, unless lx<>nil!
int k,m

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
storemode(owner,target,tttarget[m])
!tttarget[m]:=target
ttowner[m]:=owner

return m
end

global function createrefmode(ref strec owner,int target,typedefx=0)int=		!CREATEREFPACKMODE
int k,m

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

storemode(owner,target,tttarget[m])
!tttarget[m]:=target
ttbasetype[m]:=(target in [tu1,tu2,tu4]|trefbit|tref)
ttsize[m]:=ttsize[tref]
ttisref[m]:=1
tttypecat[m]:=tc_d8
!tttypecode[m]:='P'

return m
end

!global function createrefbitmode(ref strec owner,int target,typedefx=0)int=		!CREATEREFPACKMODE
!int k,m
!
!if typedefx=0 then		!anon type
!	for k:=tlast to ntypes do
!		if ttusercat[k]=0 and ttbasetype[k]=trefbit and tttarget[k]=target then
!!		if  ttbasetype[k]=trefpacked and tttarget[k]=target then
!			return k
!		fi
!	od
!	m:=createusertypefromstr(nextautotype())
!else
!	m:=typedefx
!fi
!
!storemode(102,owner,target,&tttarget[m])
!ttbasetype[m]:=trefbit
!ttsize[m]:=ttsize[trefbit]
!
!return m
!end

!global function createsubrangemode(ref strec owner,unit prange,int typedefx=0)int=		!CREATEREFPACKMODE
!int k,m
!
!if typedefx=0 then		!anon type
!	m:=createusertypefromstr(nextautotype())
!else
!	m:=typedefx
!fi
!
!ttbasetype[m]:=tsubrange
!ttsize[m]:=ttsize[tsubrange]
!ttdimexpr[m]:=prange
!
!return m
!end

global function createrefprocmode(ref strec owner,stproc, paramlist,int kwd, prettype,typedefx)int=		!CREATEREFPROCMODE
!create a ref proc mode; (can't create a proc mode by itself, as it's meaningless)
int m, mproc

mproc:=createusertype(stproc)

!CPL "=======",=PRETTYPE,=typedefx

stproc.paramlist:=paramlist
!IF PRETTYPE<>TVOID THEN
!	CPL "REFPROCMODE",STRMODE(PRETTYPE)
!FI
stproc.mode:=prettype
ttbasetype[mproc]:=tproc

!don't bother looking for similar proc sig; each one is unique
if typedefx=0 then		!anon type
	m:=createusertypefromstr(nextautotype())
else
	m:=typedefx
fi


tttarget[m]:=mproc
ttbasetype[m]:=tref

ttsize[m]:=ttsize[tref]
ttisref[m]:=1
tttypecat[m]:=tc_d8

return m
end

global function createdictmode(ref strec owner,int keytype, valuetype,int typedefx=0)int=
int m

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

ttbasetype[m]:=tdict
storemode(owner,valuetype,tttarget[m])
storemode(owner,keytype,ttkeytype[m])
ttowner[m]:=owner
tttypecat[m]:=tc_man8
ttsize[m]:=ttsize[tdict]

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
static [256]char str
[256]char str2
ref strec owner

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
ref strec p
[32]char str
ichar name

if id=frameid and owner^.nameid<>procid then
	serror("Auto frame not in proc")
fi

if id=frameid then
	print @&.str,"av$",,++nextavindex
else
	print @&.str,"sv$",++nextsvindex
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
int ulen,vlen,i

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

global function createrecordmode(ref strec owner,int typedefx)int=	!CREATERECORDMODE
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def::
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=trecord
ttusercat[m]:=1
tttypecat[m]:=tc_blk

return m
end

global function createtaggedunionmode(ref strec owner,int typedefx)int=	!CREATERECORDMODE
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def::
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=ttaggedunion
ttusercat[m]:=1

return m
end

global function createtuplemode(ref strec owner,slice[]int elements,int typedefx)int=
int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=ttuple
ttusercat[m]:=1
ttlength[m]:=elements.len
ttmult[m]:=pcm_alloc(elements.len*int32.bytes)
for i to elements.len do
	storemode(owner,elements[i],ttmult[m,i])
od

return m
end

global function createenummode(ref strec owner,int typedefx)int=
!typedef is nil, or an empty moderec belonging to a user type
!owner is an strec for the name def::
! * user-supplied name belonging to the typedef (same as typedef.namedef)
! * user-supplied optional name from a stand-alone enum typespec
! * auto-generated name
int m

if typedefx=0 then
	m:=createusertype(owner)
else
	m:=typedefx
fi
ttbasetype[m]:=tenum
ttusercat[m]:=1
tttypecat[m]:=tc_d8

return m
end

global proc convertstring(ichar s, t)=		!CONVERTSTRING
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
unit q,a,b
[500]char str

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
	when ti128 then
		print @&.str,p.value128
	when tu128 then
		print @&.str,p.uvalue128

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
!		case ttbasetype[p^.mode]
!		when trange64 then
!			fprint @&.str,"#..#",p.qvalue.lower,p.qvalue.upper
!		else
SPRINTF(&.STR,"<EVAL/CONST PROBABLY VOID>")
!			CPL typename(p^.mode),STRMODE(TTBASETYPE[P^.MODE])
!			rxerror("EVAL/CONST",p)
!		esac
	esac
	gs_additem(dest,&.str)

when j_name then
	gs_additem(dest,p^.def^.name)

when j_andl,j_orl,j_eq,j_ne,j_lt,j_le,j_gt,j_ge,j_add,j_sub,j_mul,j_div,j_idiv,
	j_irem,j_iand,j_ior,j_ixor,j_shl,j_shr,j_in,j_notin,j_inrev,j_min,j_max,
	j_subref,j_addoffset,j_suboffset,
	j_concat,j_power, j_xorl, j_same,j_idivrem, j_append then

	strcpy(&.str,getopcjname(p^.tag))
	gs_additem(dest,"(")
	jeval(dest,a)
	gs_additem(dest,&.str)
	jeval(dest,b)
	gs_additem(dest,")")

when j_neg,j_abs,j_inot,j_sqrt,j_sqr,j_sign,
	j_lwb,j_upb,j_len,	j_bitwidth,j_bytesize,
	j_minvalue,j_maxvalue,j_asc, j_chr, j_bounds,
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

!when j_convertref then
!	gs_str(dest,"CONVERTREF<>")

when j_typestr then
	gs_additem(dest,"TYPESTR(")
	jeval(dest,a)
	gs_additem(dest,")")

!when j_head, j_tail, j_init, j_last, j_take, j_drop, j_reverse, j_left, j_right,
!	 j_convlc, j_convuc, j_flexptr, j_stringz then
!
!	gs_str(dest,jtagnames[p^.tag]+2)
!	gs_str(dest,"(")
!	jeval(dest,a)
!	case p^.tag
!	when j_take,j_drop, j_convuc,j_convlc, j_left,j_right then
!		gs_str(dest,",")
!		jeval(dest,b)
!	esac
!	gs_str(dest,")")
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
int i
[20]char str

for i:=1 to opc_codes.len do		!look for dedicated op name
	if opc=opc_codes[i] then
		return opc_names[i]
	fi
od

return jtagnames[opc]+2				!return standard jtag name
end

global function strmode(int m,expand=1)ichar=		!STRMODE
static [4096]char str
istrmode(m,expand,&.str)
return &.str
end

global function strmode2(int m,expand=1)ichar=		!STRMODE
static [4096]char str
istrmode(m,expand,&.str)
return &.str
end

global proc istrmode(int m,expand=1,ichar dest)=		!ISTRMODE
ref strec d,q,e
int value,needcomma,x,i,target,mbase,n
strbuffer sxx
ref strbuffer xx:=&sxx
ref strbuffer sdim,slength
[100]char strdim
ichar prefix
typenamerec tn

!IF M>NTYPES THEN
!	STRCPY(DEST,"<BAD TYPE NUMBER>")
!STOP
!	RETURN
!FI

if m<0 then
	strcpy(dest,"XX*")
	tn:=typenames[-m]

	if tn.defb=nil then			!assume typeof
		strcat(dest,"typeof(")
		strcat(dest,tn.defa.name)
		strcat(dest,")")
    else
		if tn.defa then
			strcat(dest,tn.defa.name)
			strcat(dest,".")
		fi
		strcat(dest,tn.def.name)
	fi
	return
fi

if m<tlast and m<>tref then
	strcpy(dest,typename(m))
	return
fi

case mbase:=ttbasetype[m]
when tref then
	strcpy(dest,"ref ")
	target:=tttarget[m]
	if target>=0 and ttbasetype[target]=trecord then
		strcat(dest,typename(target))
	else
		istrmode(tttarget[m],0,dest+strlen(dest))
	fi

when trefbit then
	strcpy(dest,"refbit ")
	istrmode(tttarget[m],0,dest+strlen(dest))

!when tset then
!	if ttdimexpr[m] then
!		gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
!!		sprintf(dest,"set[%s]",&.strdim)
!		fprint @dest,"set[#]",&.strdim
!	else
!!		sprintf(dest,"set[%lld]",ttlength[m])
!		fprint @dest,"set[#]",ttlength[m]
!	fi

!when tarray,tbits then
when tarray,tbits,tsmallarray,tsmallbits then
	if ttdimexpr[m] then
		gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
		fprint @dest,"@[#]",&.strdim
	else
		if ttlower[m]=1 then
			fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
		else
			fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
		fi
	fi
	istrmode(tttarget[m],0,dest+strlen(dest))

when tslice,tslice2d,tflex,tmanarray,tmanbits then
	prefix:=stdtypenames[mbase]+1

	if ttdimexpr[m] then
		gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
!		sprintf(dest,"@slice[%s:]",&.strdim)
		fprint @dest,"@#[#:]",prefix,&.strdim
	else
		if ttlower[m]=1 then
!			strcpy(dest,"slice[]")
			strcpy(dest,prefix)
			strcat(dest,"[]")
		else
!			fprint @dest,"slice[#:]",ttlower[m]
			fprint @dest,"#[#:]",prefix,ttlower[m]
		fi
	fi
	istrmode(tttarget[m],0,dest+strlen(dest))

when tdict then
	strcpy(dest,"dict[")
	istrmode(ttkeytype[m],0,dest+strlen(dest))
	strcat(dest,"]")
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
!		strcat(dest," ")
!		x:=q^.index
!		if x<>value then
!			value:=x
!!			sprintf(dest+strlen(dest),"%lld",value)
!			getstrint(value,dest+strlen(dest))
!		fi
!		++value
		q:=q^.nextdef
	od

	strcat(dest,")")

when trecord,ttaggedunion,tsmallrecord then
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

when ttuple then
	strcpy(dest,"Tuple(")
	n:=ttlength[m]
	for i to n do
		istrmode(ttmult[m,i],0,dest+strlen(dest))
		if i<n then strcat(dest,",") fi
	od

	strcat(dest,")")

!when trange64 then
!	strcpy(dest,"range")

!when tsubrange then
!	strcpy(dest,"subrange(")
!	strcat(dest,strexpr(ttdimexpr[m])^.strptr)
!	strcat(dest,")")

when tbitfield then
	strcpy(dest,"bitfield")

elsif ttbasetype[m]<tlast then
	strcpy(dest,"Alias for:")
	istrmode(tttarget[m],0,dest+strlen(dest))

else
CPL typename(m),STRMODE(TTBASETYPE[M])
	mcerror("NEWSTRMODE")
!	return "NEWSTRMODE:"+TOSTR(M)+":"+TOSTR(M.BASETYPENO)
esac
end

!global function countunits(ref unitrec p)int=
!int n
!n:=0
!while p do
!	++n
!	p:=p^.nextunit
!od
!return n
!end

global function finddefstr(ref strec owner,ichar name)ref strec=	!FINDDEFSTRING
!scan owner looking for a name
!return symptr if found, or nil
ref strec d

CPL "FINDDEF"
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
	ref procrec pp
	++nproclist
	pp:=pcm_alloc(procrec.bytes)
	pp^.nextproc:=proclist
	proclist:=pp
	pp^.def:=d
end

global proc addstatic(ref strec d)=
	ref procrec pp
	++nstaticlist
	pp:=pcm_alloc(procrec.bytes)
	pp^.nextproc:=staticlist
	staticlist:=pp
	pp^.def:=d
end

global function typename(int m)ichar=
	if m>=0 then
		return ttname[m]
	fi
	return typenames[-m].def.name

end

global function allocunitrec:ref unitrec=
ref unitrec p
ref int64 q
int nwords

!p:=pcm_alloc(unitrec.bytes)
!memset(p,0,unitrec.bytes)
!return p

if remainingunits-- then
	p:=unitheapptr
	++unitheapptr
	p^.pos:=lx.pos
	p^.moduleno:=currmoduleno
	return p
fi

!need first or new heap
p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

memset(p,0,unitheapsize*unitrec.bytes)
remainingunits:=unitheapsize-1
++unitheapptr
!p^.lineno:=lx.lineno
p.pos:=lx.pos

p^.moduleno:=currmoduleno
return p
end

global function createdupldef(ref strec owner,symptr, int id)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
ref strec p,q

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
ref strec p,q

p:=createdupldef(owner,symptr,moduleid)
return p
end

global function duplunit(unit p,int lineno=0)unit=
unit q
if p=nil then return nil fi

!q:=createunit0(p^.tag)
!
!q^.a:=duplunit(p^.a,lineno)
!q^.b:=duplunit(p^.b,lineno)
!q^.c:=duplunit(p^.c,lineno)
!q^.lineno:=(lineno|lineno|p^.lineno)
!q^.value:=p^.value			!copy main field of each union
!q^.opcode:=p^.opcode
!!CPL "DUPLUNIT",STRMODE(P.MODE)
!q^.mode:=p^.mode
!q^.moduleno:=p^.moduleno
!q^.isastring:=p^.isastring

q:=createunit0(p^.tag)

q^:=p^
q.nextunit:=nil
if q.hasa then q.a:=duplunit(q.a); q.hasa:=1 fi
if q.hasb then q.b:=duplunit(q.b); q.hasb:=1 fi
if q.hasc then q.c:=duplunit(q.c); q.hasc:=1 fi

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
int m,target
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
unit e,wt
int m,res

!RETURN 1
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
	return 1							!any non-void expr allowed as return value
else
	return 0
fi
end

!global function strqvalue(ref qint aa)ichar=
!static [64]char str
!
!fprint @&.str,"#:#\n",aa.upper:"H",aa^.lower:"Z16H"
!return &.str
!end
!
!global function makeqvalue(int64 a,signd)ref qint=
!!t=ti128/tu128
!!scat='U' or 'I' that represents type of a
!
!ref qint aa
!
!aa:=pcm_alloc(qint.bytes)
!aa^.lower:=a
!aa^.upper:=0
!
!if signd and a<0 then
!	aa^.upper:=0xFFFF'FFFF
!fi
!return aa
!end
!
!global function makeqvalue_ab(int64 a,b)ref qint=
!ref qint aa
!
!aa:=pcm_alloc(qint.bytes)
!aa^.lower:=a
!aa^.upper:=b
!
!return aa
!end
!
global function isconstint(unit a)int=
if a^.isconst and ttisinteger[a.mode] then return 1 fi
return 0
end

global function isconstunit(unit a)int=
return a^.isconst
end

global function faststrint(int a)ichar=
static [-999..999,8]char smallints

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
static [256]char str

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
ref strec owner

owner:=d^.owner

if owner=nil or owner^.nameid=programid then return fi
getownername(owner,dest)
strcat(dest,owner^.name)
strcat(dest,".")
end

!global function isintmode(int m)int=
!	return ttisinteger[m]
!end

global function isnumericmode(int m)int=
	return ttisnumeric[m]
end

global function isrefmode(int m)int=
	return ttisref[m]
end

global function strconstopnd(unit p)ichar=
!p is a const unit containing int/word/real
	static [256]char str
	int i,a,t
	real32 x32

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
	when ti128 then
		print @&.str,p.value128
	when tu128 then
		print @&.str,p.uvalue128
!	when trange64 then
!		print @&.str,p^.qvalue^.lower,,"..",,p^.qvalue^.upper
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

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
int a

case ttbasetype[m]
when tarray then
	return getalignment(tttarget[m])
when trecord then
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
int64 a
int n

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
!	if ttisflexvar[m] then return 0 fi
!	if ttisvar[m] then return 0 fi

!	if ttcat[m]<>tvoid and ttcat[m]=tblock then return 0 fi
!
!	if stdtypecat[ttbasetype[m]]=tblock then
!		if ttsize[m] not in [16,8,4,2,1] then
!			return 0
!		fi
!	fi
	return 1
end

global function getlow128(ref int128 a)word=
	return cast(a,ref word)^
end

global function gethigh128(ref int128 a)word=
	return (cast(a,ref word)+1)^
end

global proc putlow128(ref int128 a,word x)=
	cast(a,ref word)^:=x
end

global proc puthigh128(ref int128 a,word x)=
	(cast(a,ref word)+1)^:=x
end

!global proc showcallcounts=
!	for i to callcounts.upb when callcounts[i] do
!		println i:"z4",callcounts[i]:"8S,",,":",$get_procname(i)
!	od
!end
!
!global proc countfn(ichar name)=
!	int n
!!RETURN
!	for i to $get_nprocs() do
!		if eqstring(name,$get_procname(i)) then
!			n:=i
!			exit
!		fi
!	else
!		abortprogram("Can't find proc")
!	od
!	++callcounts[n]
!end

global function storemode(ref strec owner, int m, int32 &pmode)int =
	ref typenamerec r

	if m>=0 then
		pmode:=m
		return m
	fi

	r:=&typenames[-m]

!CPL =M, =R.PMODE

	if r.pmode=nil then
		r.owner:=owner
		pmode:=m
		r.pmode:=&pmode

IF R.PMODE=NIL THEN SERROR("PMODE=NIL") FI

		return m
	fi

!Already one instance of this mode; need a new slot
!CPL "STOREMODE/2",OWNER.NAME
	m:=newtypename(r.defa, r.defb)
	r:=&typenames[-m]

	r.owner:=owner
	pmode:=m
	r.pmode:=&pmode
	return m
end

global proc addoverload(int moduleno, opc, amode, bmode, rmode, unit pfunc)=
	ref overloadrec p
	ref strec owner

!really need to search for duplicate operator defs

	p:=pcm_allocz(overloadrec.bytes)

	p.moduleno:=moduleno
	owner:=moduletable[moduleno].stmodule
	storemode(stmodule, amode, p.amode)
	storemode(stmodule, bmode, p.bmode)
	storemode(stmodule, rmode, p.rmode)
	p.fncode:=pfunc

	p.nextoverload:=overloadtable[opc]

	overloadtable[opc]:=p
end

global function gettypebase(int m)int=
	switch ttbasetype[m]
!	when ti64,tu64,tr64,ti128,tu128,tc64 then m
	when ti8,ti16,ti32 then ti64
	when tu8,tu16,tu32 then tu64
	when tr32 then tr64
	when tc8,tc16 then tc64
	else
		m
	end switch
end
=== bb_diags.m 11/18 ===
import mlib
import clib
import oslib

import bb_decls
import bb_tables
import bb_support
import bb_lex
import bb_lib
import bb_mcldecls

int currlineno
int currfileno

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
const wtypecat	= 6
const wused		= 4
const wmode		= 24
!const wcategory	= 32
[256]char str
ichar mstr
strbuffer destv
ref strbuffer dest := &destv
int m

println @f,"MODELIST",ntypes

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
gs_leftstr(dest,"TCat",wtypecat)
gs_leftstr(dest,"Usd ",wused)
gs_leftstr(dest,"Mode",wmode)
!gs_leftstr(dest,"Category",wcategory)
gs_println(dest,f)

for m:=0 to ntypes do
!for m:=tlast to ntypes do
	gs_init(dest)
	gs_leftint(dest,m,wtypeno)
	gs_leftstr(dest,typename(m),wname)
	gs_leftstr(dest,typename(ttbasetype[m]),wbasetype)
	case m
	when tu1,tu2,tu4 then
		gs_str(dest,"-")
	else
		gs_leftint(dest,ttsize[m]*8,wbitsize)
	esac

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
	when tarray,trecord then
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

	gs_leftstr(dest,typecatnames[tttypecat[m]]+3,wtypecat)
!CPL =M,=TYPECATNAMES[M]
!	gs_leftstr(dest,"FRED",wtypecat)

	mstr:=strmode(m)
	if strlen(mstr)<wmode then
		strcpy(&.str,mstr)
	else
		memcpy(&.str,mstr,wmode)
		str[wmode]:=0
	fi
	gs_leftstr(dest,&.str,wmode)

	gs_println(dest,f)

	if m=trefchar then
		gs_init(dest)
		gs_str(dest," ")
		gs_println(dest,f)
	fi
od

println @f

println @f,"TYPE NAMES:",NTYPENAMES
typenamerec r

INT NN:=0

for i:=1 to ntypenames do
	r:=typenames[i]

	print @f,-i,," "
	if r.owner then
		print @f,r.owner.name,,"::"
	fi
	if r.defb=nil then
		fprint @f,"typeof(#)",r.defa.name
	else
		if r.defa then
			print @f,r.defa.name,,"."
		fi
		print @f,r.def.name
	fi
	if r.pmode=nil then
		print @f," No link"
++NN
	fi
	println @f
od

println @f
println @f,"EXTENDTYPES"
ref strec ee,q
	ee:=extendtypelist

	while ee do
		println @f,"Extend:",strmode(ee.mode)
		q:=ee.deflist

		while q<>nil do
			printst(f,q,1)
			q:=q.nextdef
		od

		ee:=ee.nextdef
	od
!
!CPL =NN,=NTYPENAMES

end

global proc printoverloads(filehandle f)=
	ref overloadrec p

	println @f,"OVERLOADS"
	for i to overloadtable.upb do
		p:=overloadtable[i]
		if p then
!			println @f,"Overloads for",jtagnames[i],P
			while p do
				if p.bmode then
					fprint @f,"operator (#)(#,#)#",
						jtagnames[i]+2,strmode(p.amode), strmode(p.bmode), strmode(p.rmode)
				else
					fprint @f,"operator (#)(#)#",
						jtagnames[i]+2,strmode(p.amode), strmode(p.rmode)
				fi
				if p.fncode then
					print @f,"=",p.fncode,strexpr(p.fncode).strptr
				fi
				println @f
				p:=p.nextoverload
			od
			println @f
		fi
	od

end

global proc printst(filehandle f,ref strec p,int level=0)=	!PRINTST
ref strec q

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
strec dd
ref byte q
strbuffer v
ref strbuffer d:=&v
int col,offset,n
const tabstr="    "
[256]char str

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

if dd.nameid=paramid and dd.parammode then
	gs_str(d,parammodenames[dd.parammode])
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
!CPL =P.OWNER
!RETURN

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
!CPL =P.NAME,=P.MODE!,=STRMODE(P.MODE)
	GS_STRINT(D,P^.MODE)
	GS_STR(D,":")

	gs_str(d,strmode(p.mode))
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
		gs_str(d,"/:=")
		gs_strvar(d,strexpr(p^.code))
	fi

	if p.nameid=paramid and p.variadic then
		gs_str(d,"...")
	fi
when genfieldid then
	gs_str(d,"Index:")
	gs_strint(d,p^.offset)

when procid,genprocid then

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
	if ttbasetype[p.mode]=ttaggedunion then
		gs_str(d,"Tagmode:")
		gs_str(d,strmode(p.enumtagmode))
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
	if p.nameid=fieldid then
		gs_str(d," @")
		gs_str(d,p^.equivfield^.name)
	fi
when 1 then
	if p.nameid in [frameid, staticid] then
		gs_strvar(d,strexpr(p^.equivvar))
	fi
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
int i
ref strec p
println @f,"GLOBAL SYMBOL TABLE:"

for i:=0 to hashtable.upb-1 do
!cpl i
!	if hashtable[i].name and hashtable[i].symbol=namesym then
	p:=&hashtable[i]
	if p^.name then
		case p^.symbol
		when namesym then
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
int i
ref strec p

!p:=moduletable[n].stmodule^.deflist
p:=stprogram^.deflist

println @f, caption, "PROGRAM"

while p do
	printmodulecode(f,p)
	p:=p^.nextdef
od
end

global proc printmodulecode(filehandle f,ref strec m)=
int i
ref strec p

p:=m^.deflist
currmodule:=m

println @f,"MODULE:",m^.name,namenames[m^.nameid]

while p do
	case p^.nameid
	when procid,genprocid then
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
ref unitrec q
ref strec d
int t
ichar idname
int64 a
real32 x32

if p=nil then
	return
fi

!CPL "PRINTUNIT",jtagnames[p.tag],=p,=p.a,=p.b, =p.c, =p.hasa, =p.hasb, =p.hasc,
!"//",P.LINENO, SOURCEFILENAMES[P.FILENO]

!IF P.TAG=J_MAKESET THEN
!CPL P.A,P.B,P.C
!
! RETURN FI

if p^.lineno then
	currlineno:=p^.lineno
	currfileno:=p^.fileno
fi

!IF P.TAG=J_PTR THEN
!CPL "PTR",=P,P.A,P.B,P.C
!!	PRINTUNIT(P.A)
!FI
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

!	if p.avcode then print @dev," AV:",p.avcode:"c" fi
	if p.avcode then print @dev," AV:",char(p.avcode) fi

when j_labeldef then
	println @dev,p^.def^.name

when j_const then
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
	when ti128 then
		print @dev,p.value128
	when tu128 then
		print @dev,p.uvalue128
	else
!		case ttbasetype[t]
!		when trange64 then
!			print @dev,p^.qvalue^.lower,,"..",,p^.qvalue^.upper
!		else
			cpl =typename(t),typename(ttbasetype[t])
PRINT @DEV,"<PRINTUNIT BAD CONST PROBABLY VOID"
!!			CPL("PRINTUNIT BAD CONST")
!			serror("PRINTUNIT BAD CONST")
!		esac
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
	print @dev,"Len:",p^.length," Makeax:",p.makearray

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
	ichar plus
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

when j_maths then
	print @dev,mathsnames[p.opcode]+3

when j_makeset then
!	if p.isconst then
!		print @dev,p.range_lower,,"..",,p.range_upper
!	fi
!
esac

if p^.isconst then
	print @dev," Is const"
fi

println @dev

if p.hasa then printunitlist(dev,p^.a,level+1,"1") fi
if p.hasb then printunitlist(dev,p^.b,level+1,"2") fi
if p.hasc then printunitlist(dev,p^.c,level+1,"3") fi

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
static [1024]char str
[1024]char indentstr
[16384]char modestr
ichar isexpr

indentstr[1]:=0
if level>10 then level:=10 fi

to level do
	strcat(&.indentstr,"- ")
od

isexpr:="S"
if jisexpr[p.tag] then isexpr:="E" fi

case p.tag
when j_if, j_switch, j_case, j_select then
	if p.mode=tvoid then
		isexpr:="S"
	fi
esac

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
static [40]char str

!sprintf(&.str,"%04d ",currlineno)
print @&.str,currlineno:"z4",," "
print @&.str,CURRFILENO:"Z2",currlineno:"z4",," "
return &.str
end

=== bb_mcldecls.m 12/18 ===
import bb_decls
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
	ref strec labeldef	!nil, or handle of strec for label
	union
		ref strec def
		int64 value		!immediate value
		real64 xvalue	!immediate real value, mainly for dq
		ichar svalue	!immediate string
		int labelno
		int sysfn
	end

	byte size			!byte size of operand: usually 1,2,4,8,16
	byte mode			!a_reg etc, low level operand details
	byte reg			!0, or main register
	byte regix			!0, or index register

	byte valtype		!interpretation of def/code/value/svalue
	byte scale			!1, or scale factor for regix
	int16 offset		!extra offset to label for mem/imm modes

	byte addrsize	!4 or 8 for a_mem when regs are involved
!	byte valtypex		!valtypex is 0 (no value or int) or 'R'/'S' in ma

end

global record mclrec =		!32 bytes
	ref mclrec nextmcl
	ref opndrec a,b
	byte opcode
	union
		byte cond
		byte isglobal
		byte sysindex
	end
	byte fileno
	byte spare
	int lineno
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

	(m_bsf,				$,		2,		0xBC),	!
	(m_bsr,				$,		2,		0xBD),	!

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
	("d2",		8,	r2),		!rsi     instructions, eg, b10 (cl) for shift count)
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

global []ichar xmmregnames = (
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

global []ichar fregnames = (
	"st0",
	"st1",
	"st2",
	"st3",
	"st4",
	"st5",
	"st6",
	"st7")

global []ichar mregnames = (
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

=== bb_parse.m 13/18 ===
import msys
import mlib
import clib
import oslib

import bb_decls
import bb_tables
import bb_support
import bb_lex
import bb_lib
!import mm_start
import bb_diags
import bb_mcldecls

macro readunit=readassignment()

mut int intabledata=0		!1 means reading table data line; $ gives tabledataname
mut int inreadprint=0
mut int inparamlist=0
mut int inrecordbody=0
mut int inimportmodule=0
mut int labelseen=0
mut ichar tabledataname=nil

const maxprocstack=10
[maxprocstack]ref strec procstack
int nprocstack=0

uflagsrec unionstring, unionpend
ref strec unionlastvar=nil
ref strec dretvar			!part of read-proc: nil, or symptr of retval variable

int try_level=0
int varattribs=0

const maxdollarstack=10
[maxdollarstack]unit dollarstack		!used for a[$]
int ndollar=0
int inmultexpr=0
int insiderecord=0
int insidedllimport=0
int yieldseen=0

global function parsemodule(int n)int=
modulerec m
ref strec p, owner
int globalflag,status

initparser()

m:=moduletable[n]
currmoduleno:=n

stmodule:=moduletable[n].stmodule
currproc:=stmodule

!startlex("PARSEMODULE",m.fileno)
starttkscan(n)

owner:=stmodule
lex()

!INT NTOKENS:=0
!REPEAT
!	LEX()
!CPL SYMBOLNAMES[LX.SYMBOL]
!++NTOKENS
!UNTIL LX.SYMBOL=EOFSYM
!CPL =NTOKENS
!CPL =NLOOKUPS
!CPL =NCLASHES
!CPL =NCLASHES/REAL (NLOOKUPS)
!STOP
!RETURN 1
!CPL "PARSING MODULE:",m.name
!PS("START")

status:=readmoduledefs(owner)

!CPL =STATUS, OWNER.DEFLIST
!

if not status then
	return 0
fi

return status
end

global function readmoduledefs(ref strec owner)int=
!first symbol has been read
ref strec p,dimport,stimport
int globalflag,i,callbackflag
ichar name


globalflag:=0
callbackflag:=0

do
!PS("READMODULE DEF")
	switch lx.symbol
	when kglobalsym then
		if globalflag then serror("global global?") fi
		globalflag:=lx.subcode			!1 or 2
		lex()

	when kprocsym,kfunctionsym then	!todo
		readprocdef(owner,globalflag,callbackflag)
		callbackflag:=0
		globalflag:=0

	when stdtypesym,namesym,lsqsym,krefsym,kicharsym,ktypeofsym,
		kdictsym,kslicesym then
		readvardef(owner,globalflag,0,staticid, 0)
		globalflag:=0

	when kmutsym then
		lex()
		readvardef(owner,globalflag,0,staticid,kmutsym)
		globalflag:=0

	when kletsym then
		lex()
		readvardef(owner,globalflag,0,staticid,kletsym)
		globalflag:=0

	when kimportmodulesym then
		readimportmodule(owner)

	when kimportpathsym then
		lexchecksymbol(stringconstsym)
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

	when ktaggedunionsym then
		readtaggeduniondef(owner,globalflag)
		globalflag:=0

	when kenumsym then
		lex()
		readenumtype(owner,0)

	when ktabledatasym then
		readtabledef(globalflag)
		globalflag:=0

	when docstringsym then
		adddocstring(lx.svalue)
		lex()

	when kimportsym then
		if globalflag then serror("glob/import?") fi
		lex()
		if lx.symbol=mulsym then
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
		if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
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

!	when kexpandsym then
!		serror("MODULE/EXPAND")

	when koperatorsym then
		readoperatordef(owner)
!		serror("MODULE/OPERATOR")

	when kextendtypesym then
		readextendtype(owner)

	when dotsym then
		SERROR("MODULE/DOT")
	else
error::
		PS("symbol")
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
ichar tabledataname:=""
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
if lx.symbol<>eqsym then
	serror("""="" expected")
fi
end

function getcurrline:int=
return lx.pos
end

function checkbegin(int fbrack)int=				!CHECKBEGIN
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
int closesym

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
[100]char str

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

function readvardef(ref strec owner,int isglobal=0,isstatic=0,varid=staticid, k)unit=
!positioned at symbol following 'mut' or 'let', which will at the first symbol of
!the type, or at the first name being defined if there is no type
!k is the keyword symbol used (let/mut), or set to 0 if no keyword has been used,
!then mut is assumed

!read vars inside module or proc
!isglobal must be 0 for procs
!isstatic must be 1 for modules
!varid must be frameid[let]/staticid[let] for procs, otherwise staticid[let]

unit ulist,ulistx, p
int nvars,m
ref strec stname

!lex()
ulist:=ulistx:=nil

if istypestarter() then
	m:=readtypespec(owner)
elsif k then
	m:=tauto
else
	serror("Readvar?")
!CPL "ERROR???"
fi

nvars:=0
while lx.symbol=namesym do

	++nvars
	stname:=getduplnameptr(owner,lx.symptr,varid)

!	stname.mode:=m
	storemode(owner,m,stname.mode)	
	stname^.isglobal:=isglobal
	stname^.isstatic:=isstatic
	stname^.islet:=(k=kletsym)

	adddef(owner,stname)
	if varid=staticid then
		addstatic(stname)
	fi

	lex()

	if lx.symbol in [assignsym,eqsym] then
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
			p.initlet:=1
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

!PS("ENDVAR")

if nvars=0 then
	serror("No vars declared")
fi
return ulist
end

proc readconstdef(ref strec owner,int isglobal=0)=
!at 'const' symbol
int nconsts,deft,m
ref strec stname

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

	m:=deft

	storemode(owner,m,stname.mode)	
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

!return positioned at symbol following closing ")"
!listtag is j_makelist or j_makearray if 'array' was used

unit ulist,ulistx, p,q,r, plower
int oldirp,length

lex()					!first symbol of first expression
ulist:=ulistx:=nil
plower:=nil
length:=0

if lx.symbol=atsym then			!lwb override
	lex()
	oldirp:=inreadprint
	inreadprint:=1
	plower:=readunit()

	inreadprint:=oldirp
	checksymbol(colonsym)
	lex()

elsif lx.symbol=intconstsym and nexttoken.symbol=colonsym then
	plower:=createconstunit(lx.value,lx.subcode)
	lex()
	lex()

elsif symboloptypes[lx.symbol]=bin_op and nexttoken.symbol=rbracksym then	!operator constant
	p:=createunit0(j_operator)
	p^.opcode:=lx.subcode
	lex()
	lex()
	return p
elsif symboloptypes[lx.symbol]=bin_op and nexttoken.symbol=assignsym then	!operator:= constant
	p:=createunit0(j_operator)
	p^.opcode:=symboljtotags[lx.symbol]
	lex()			!read :=
	lexchecksymbol(rbracksym)
	lex()
	return p
elsif istypestarter() then
	p:=readunit()
	checksymbol(rbracksym)
	lex()
	return p
fi

!check symbol after "("
case lx.symbol
when rbracksym then			!empty list
	lex()
	p:=createunit0(j_makelist)
	p.b:=plower
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
	if nexttoken.symbol=rbracksym then		!means one-element list
		lex()
		lex()
		p:=createunit1(j_makelist,p)
		p^.length:=length
		p^.b:=plower
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
	p^.b:=plower
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
	if nexttoken.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
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

function readcast:unit=
!also reads standalone type value
!t<>tvoid means already has ty[e
unit p
int opc,t

t:=readtypespec(currproc)

case lx.symbol
when rbracksym then
	p:=createunit0(j_typeconst)
!	p.value:=t
	p.mode:=ttype
	return p

when atsym then
	opc:=j_typepun
	lex()
when dotsym then			!allow T.type, but also just T (followed by . which
							!might be T.minvalue etc)
	if nexttoken.symbol=ktypesym then
		lex()
		p:=createunit0(j_typeconst)
		p.value:=t
		p.mode:=ttype
		lex()
	else					!leave dot to be processed by caller
		p:=createunit0(j_typeconst)
		p.value:=t
	fi
	return p
else
	opc:=j_convert
esac

checksymbol(lbracksym)
lex()
p:=readunit()
checksymbol(rbracksym)
lex()

p:=createunit1(opc,p)
!p.newmode:=t
storemode(currproc,t,p.newmode)
return p
end

function readopc:unit=			!READOPC
!op sym seen just before a term
unit p,q,r
int opc,opc2, mathsopc,firstsym

firstsym:=lx.symbol

if lx.symbol=mathsopsym then
	opc:=j_maths
	mathsopc:=lx.subcode
else
	opc:=symboljtags[lx.symbol]
	mathsopc:=0
fi

lex()
case firstsym
when addsym then			!ignore +
	return readterm2()
when subsym then			!convert minus to negate
	opc:=j_neg
when minsym,maxsym,concatsym,appendsym then

	p:=readterm2()

	if p.tag=j_makelist then
		if p.length<>2 then serror("Needs (x,y)") fi
		q:=p.a
		r:=q.nextunit
		q.nextunit:=nil
		return createunit2(opc,q,r)
	else		!assume single operand
		return createunit1(opc,p)

	fi
else
	if symboloptypes[firstsym]=bin_op then
		serror("Can't be used as unary op")
	fi

esac

if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
	lex()
	opc:=symboljtotags[firstsym]
fi

p:=createunit1(opc,q:=readterm2())

p.opcode:=mathsopc				!0, or mt_ code for j_maths

if q.tag=j_makelist then
	serror("Too many opnds")
fi

!*!evalmonop(p)
return p
end

function readsprint:unit=			!READSPRINT
int oldinreadprint,opc,isfprint
unit pformat, pdev, printlist, printlistx, p

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lexchecksymbol(lbracksym)
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
int oldinreadprint,opc
unit pformat,pdev,p, readlist,readlistx

oldinreadprint:=inreadprint
inreadprint:=1
opc:=lx.subcode
lexchecksymbol(lbracksym)
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
[100]char str
rsystemtime tm
static []ichar monthnames=("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
unit p

switch lx.subcode
when j_cvnil then
	p:=createconstunit(0,tref)
	lex()
	return p

when j_cvlineno then

	p:=createunit0(j_cvlineno)
	lex()
	return p

when j_cvstrlineno then
	getstrint(lx.lineno,&.str)

when j_cvmodulename then
	p:=createunit0(j_cvmodulename)
	lex()
	return p

when j_cvfilename then
	p:=createunit0(j_cvfilename)
	lex()
	return p

when j_cvfunction then
	strcpy(&.str,currproc^.name)

when j_cvdate then
	os_getsystime(&tm)
	fprint @&.str,"#-#-#",tm.day,monthnames[tm.month],tm.year:"4"

when j_cvtime then
	os_getsystime(&tm)
	fprint @&.str,"#:#:#",tm.hour:"z2",tm.minute:"z2",tm.second:"z2"

when j_cvtargetbits then
	lex()
	return createconstunit(targetbits,tint)
when j_cvtargetsize then
	lex()
	return createconstunit(targetsize,tint)
when j_cvtargetcode then
	strcpy(&.str,"wx64")

else
	serror_s("compiler var not impl: #",jtagnames[lx.subcode])
end switch
lex()

return createstringconstunit(pcm_copyheapstring(&.str),-1)
end

function readcastx:unit=
!explicit cast using syntax::
! cast(expr)
! cast(expr,type)
! cast@(expr,type)
!at 'cast'
	int opc,m
	unit p

	lex()
	opc:=j_convert
	if lx.symbol=atsym then
		opc:=j_typepun
		lex()
	fi
	checksymbol(lbracksym)
	lex()
	m:=tvoid
	p:=readunit()
	if lx.symbol<>commasym then
		if opc=j_typepun then serror("@ type missing") fi
		opc:=j_autocast
	else
		lex()
		m:=readtypespec(currproc)
	fi
	checksymbol(rbracksym)
	lex()

	p:=createunit1(opc,p)
!	p.newmode:=m
	storemode(currproc,m,p.newmode)

	return p
end

global proc checksymbol(int symbol)=
[100]char str

if lx.symbol<>symbol then
	fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]
	serror(&.str)
fi
end

proc lexchecksymbol(int symbol)=
	lex()
	checksymbol(symbol)
end

global function readtypespec(ref strec owner,int typedefx=0)int=			!READTYPESPEC
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
ref strec d,e
int t,kwd,fflang,sltype,w
unit x,pupper,plx
unit dim,length
const maxdim=30
[maxdim]unit dims
int ndims,i,n,k

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

when namesym then
	d:=lx.symptr
	lex()

	if lx.symbol=dotsym then
		lexchecksymbol(namesym)
		t:=newtypename(d,lx.symptr)
		lex()
	else
		t:=newtypename(nil,d)
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
	elsif lx.symbol=namesym and lx.subcode=asmopcodesym and lx.symptr.index=m_label then
		t:=createrefmode(owner,tlabel,typedefx)
	elsif lx.symbol=stdtypesym then
		case lx.subcode
		when tc8 then
			t:=trefchar
			if typedefx then tttarget[typedefx]:=tc8 fi
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
	if typedefx then tttarget[typedefx]:=tc8 fi

when ktypeofsym then
	lexchecksymbol(lbracksym)
	lexchecksymbol(namesym)

	t:=newtypename(cast(lx.symptr),nil)
	lexchecksymbol(rbracksym)
	lex()

when kslicesym then
	t:=readslicetype(owner,lx.subcode,typedefx)

when kdictsym then
	lexchecksymbol(lsqsym)
	lex()
	k:=readtypespec(owner)
	checksymbol(rsqsym)
	lex()
	t:=readtypespec(owner)
	t:=createdictmode(owner,k,t,typedefx)

else
	serror("Bad type starter")
esac

if typedefx then			!assume a simple alias
	ttbasetype[typedefx]:=ttbasetype[t]
fi

return t
end

function readslicetype(ref strec owner, int slicetype, typedefx)int=
!positioned at 'slice'
!dim is nil, or lower-bound expression
	unit plower
	int t

	lexchecksymbol(lsqsym)
	lex()
	if lx.symbol<>rsqsym then
		inreadprint:=1
		plower:=readunit()
		inreadprint:=0
		checksymbol(colonsym)
		lexchecksymbol(rsqsym)
	else
		plower:=nil
	fi
	lex()
	t:=readtypespec(owner,typedefx)

	if slicetype=tmanarray and t in [tu1,tu2,tu4] then
		slicetype:=tmanbits
	fi

	return createslicemode(owner,slicetype,t,plower,typedefx)
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
unit ulist,ulistx
int oldinparamlist

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
unit q,plower,pupper

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
		lexchecksymbol(rsqsym)
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
unit q
int t

while lx.symbol=dotsym do
	lex()
	switch lx.symbol
	when lsqsym then
		p:=readindex(p,1)
	when namesym then
		p:=createunit2(j_dot,p,createname(lx.symptr))
		lex()
	when lensym, lwbsym, upbsym, boundssym, lenstrsym, bitwidthsym,
		 bytessym, typestrsym, minvaluesym, maxvaluesym then
		p:=createunit1(symboljtags[lx.symbol],p)
		lex()
	when bitfieldsym then
		p:=createunit1(j_bitfield,p)
		p^.opcode:=lx.subcode
		lex()
!	when lbracksym then			!use for variable attributes
!		lex()
!		p:=createunit2(j_dotattr,p,readunit())
!		checksymbol(rbracksym)
!		lex()
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

function readconstexpr(int needconst=1)unit=
return readunit()
end

function readconstint:int=		!READCONSTINT
!read expression that must yield a constant int value *now*; return value
int64 x

!keep it simple for now
if lx.symbol=intconstsym then
	x:=lx.value
	lex()
	return x
elsif lx.symbol=subsym then
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
int kwd,startline,closesym
ref strec stproc,q,stname

kwd:=lx.symbol
yieldseen:=0

stproc:=readprocdecl(procowner,isglobal,fflang)

checkequals()
lex()

startline:=getcurrline()

closesym:=checkbegin(0)

pushproc(stproc)
nextavindex:=0

IF DRETVAR THEN
	stname:=getduplnameptr(stproc,dretvar,frameid)
	storemode(procowner,stproc.mode,stname.mode)
	adddef(stproc,stname)
fi

addtoproclist(stproc)

stproc^.code:=readsunit()

checkbeginend(closesym,kwd,startline)

stproc^.equals:=1
if yieldseen then
	stproc.nameid:=generatorid
fi

if ndocstrings then
	PRINTLN CURRPROC.NAME,,":"
	for i to ndocstrings do
		CPL DOCSTRINGS[I]
		pcm_free(docstrings[i],strlen(docstrings[i]+1))
	od
	CPL
	ndocstrings:=0
fi

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

int kwd,varparams,try_level, nparams, nretvalues
[maxtuplesize]int retmodes
int prettype@&retmodes

ichar metadata, truename
ref strec pequiv, stproc, owner, paramlist,nameptr

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

!CPL =NRETVALUES

stproc^.paramlist:=paramlist
stproc^.nretvalues:=nretvalues

case nretvalues
when 0 then
	procowner.mode:=tvoid
when 1 then
	storemode(procowner,retmodes[1],stproc.mode)
else
	stproc.mode:=createtuplemode(procowner,(&.retmodes,nretvalues),0)
esac

if lx.symbol=atsym then			!equivalence
	lexchecksymbol(namesym)
	lex()
	stproc^.at:=1
fi

stproc^.code:=nil

case fflang
when clangff,windowsff then
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
	ref strec stlist, stlistx, stname, d
	int parammode, pmode, m, parametric

	[30]char str
	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	parametric:=0

	if fflang=0 then fflang:=thislangff fi

	if lx.symbol in [koutsym, addrsym, insym] then
		pmode:=tvoid
	elsif lx.symbol=namesym and nexttoken.symbol in [commasym,rbracksym] then	!name only
		if fflang<>thislangff then				!assume type
			pmode:=readtypespec(procowner)
			return readparams_types(procowner,owner,fflang,varparams,nparams,pmode)
		else
			pmode:=tvoid
		fi
	else
		pmode:=readtypespec(procowner)
		if lx.symbol in [commasym,rbracksym] then
			return readparams_types(procowner,owner,fflang,varparams,nparams,pmode)
		fi
	fi

	do										!expect type of name at start of loop
		if pmode in tparam1..tparam4 then
			parametric:=1
		fi

!name expected here, with optional in/out/& just before
		parammode:=var_param
		case lx.symbol
		when insym then
			parammode:=in_param
			lex()
			if lx.symbol=colonsym then lex() fi
		when koutsym,addrsym then
			parammode:=out_param
			lex()
			if lx.symbol=colonsym then lex() fi
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

		storemode(owner,m,stname.mode)
		stname^.parammode:=parammode
		addlistparam(&stlist,&stlistx,stname)

		case lx.symbol
		when assignsym,eqsym then
			lex()
			stname^.code:=readunit()
			stname^.equals:=1
			stname^.optional:=1
		when ellipsissym then
			stname.variadic:=1
			varparams:=1
			lexchecksymbol(rbracksym)
			exit
		esac

		case lx.symbol
		when commasym then
			lex()
		when rbracksym then
			exit
		else
			serror("nameparams1")
		esac

		if istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
		fi
	od

	if parametric and owner.nameid=procid then
		owner.nameid:=genprocid
	fi

	return stlist
end

function readparams_types(ref strec procowner,owner,int fflang,&varparams,&nparams,int pmode)ref strec=			!READPARAMS
!read types-only non-empty parameter list, only for ffi
!positioned at first symbol after '('
	ref strec stlist, stlistx, stname
	int firstparam,m

	[30]char str
	stlist:=stlistx:=nil
	stname:=nil
	nparams:=0

	do
		++nparams
		print @&.str,"$",,nparams
		stname:=getduplnameptr(owner,addnamestr(&.str),paramid)
		adddef(owner,stname)
		m:=pmode
		storemode(owner,pmode,stname.mode)
		addlistparam(&stlist,&stlistx,stname)

		case lx.symbol
		when assignsym,eqsym then
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

		if lx.symbol=ellipsissym then
			varparams:=1
			lexchecksymbol(rbracksym)
			exit
		fi

		pmode:=readtypespec(procowner)
	od
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
int pos1, kwd, pos2
unit pthen,pcond, plist,plistx, pelse, p, pelsif

pos1:=lx.pos

kwd:=lx.symbol			!in case coming from elsecase etc

lex()
pcond:=readsunit()
skipsemi()

checksymbol(kthensym)
lex()

pthen:=readsunit()

if lx.symbol=kelsifsym then
	pos2:=lx.pos
	plist:=plistx:=createunit2(j_elsif,pcond,pthen)

	while lx.symbol=kelsifsym do
		pos2:=lx.pos
		lex()
		pcond:=readunit()
		checksymbol(kthensym)
		lex()
		pthen:=readsunit()
		pelsif:=createunit2(j_elsif,pcond,pthen)
		pelsif^.pos:=pos2
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
	p^.pos:=pos1
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
p^.pos:=pos1
return p
end

function readgoto(int gototag=j_goto)unit=	!READGOTO
ref strec d
unit p

if lx.subcode=1 then		!go used
	lexchecksymbol(ktosym)
fi
lex()

if lx.symbol=namesym and nexttoken.symbol<>ptrsym and nexttoken.symbol<>lsqsym and \
	nexttoken.symbol<>dotsym then			!assume simple label
	p:=createname(lx.symptr)

	lex()
else
	serror("GOTO LABEL EXPR")
fi

return readcondsuffix(createunit1(gototag,p))
end

function readunless:unit=
int pos
unit pcond, pthen, pelse, p
pos:=lx.pos
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
p^.pos:=pos
return p
end

function readswitchcase:unit=
int pos1, kwd, opc, pos2,rangeused, nwhen
unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

pos1:=lx.pos
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
	pos2:=lx.pos
	lex()
	pwhen:=pwhenx:=nil
	do
		p:=readunit()
		++nwhen
		p^.pos:=pos2
		if p^.tag=j_makerange then rangeused:=1 fi
		addlistunit(&pwhen,&pwhenx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od
	checksymbol(kthensym)
	lex()
	pthen:=readsunit()
	pwhenthen:=createunit2(j_whenthen,pwhen,pthen)
	pwhenthen^.pos:=pos2
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
p^.pos:=pos1
return p
end

function readstop:unit=
unit p
int i
lex()
if exprstarter[lx.symbol] then
	p:=createunit1(j_stop,readunit())
else
	p:=createunit0(j_stop)
fi
return readcondsuffix(p)
end

function readreturn:unit=
unit p,q,r

lex()
if exprstarter[lx.symbol] then
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
	unit p
	int pos

	pos:=lx.pos
	lex()
	p:=readsunit()
	checkend(kendsym,kdosym)
	lex()
	p:=createunit1(j_do,p)
	p^.pos:=pos
	return p
end

function readto:unit=
int pos,id
unit p, pcount, pbody

pos:=lx.pos
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
p^.pos:=pos
return p
end

function readwhile:unit=
int pos,id
unit pcond, pa, pb, pc, pbody, p

pos:=lx.pos
lex()

pcond:=readsunit(1)

checksymbol(kdosym)
lex()
pbody:=readsunit()

checkend(kendsym,kwhilesym,kdosym)
lex()

p:=createunit2(j_while,pcond,pbody)
p^.pos:=pos
return p
end

function readrepeat:unit=
int pos
unit pbody, pcond, p

pos:=lx.pos
lex()
pbody:=readsunit()
checksymbol(kuntilsym)
lex()
pcond:=readunit()
p:=createunit2(j_repeat,pbody,pcond)
p^.pos:=pos
return p
end

function readloopcontrol:unit=
int opc
unit p

opc:=lx.subcode
lex()
if lx.symbol=namesym and eqstring(lx.symptr^.name,"all") then
	lex()
	p:=createunit1(opc,createconstunit(0,tint))

elsif exprstarter[lx.symbol] then
	p:=createunit1(opc,readconstexpr(1))
else
	p:=createunit1(opc,createconstunit(1,tint))
!	p:=createunit0(opc)
fi
return readcondsuffix(p)
end

function readprint:unit=
int oldinreadprint, opc, isfprint, fshowname, length
unit pformat, pdev, printlist,printlistx, p,q
ref strbuffer expr

ichar s

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
	if not exprstarter[lx.symbol] and opc=j_cprintln then
		goto finish
	fi
	pformat:=readunit()
	if lx.symbol=commasym then lex() else goto finish fi
fi

if not exprstarter[lx.symbol] then
	goto finish
fi

do
	if lx.symbol=commasym then		!assume extra comma, meaning nogap
		addlistunit(&printlist,&printlistx, createunit0(j_nogap))
	else

		fshowname:=0
		if lx.symbol=eqsym then
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
int oldinreadprint,opc
unit pformat, pdev, readlist, readlistx, p, pread

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

if not exprstarter[lx.symbol] then
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
unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
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
unit p

lex()
p:=readunit()
return createunit1(j_raise,p)
end

function readfor:unit=
!on 'for'; syntax is::
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

!AV codes:
!	I	loop index, always i64; will be 'i' (declared or not declared) or autovar
!	L	forall local variable; will be 'x' (declared or not declared); type is variable
!	T	'to' limit; used for autovar storing value of complex to-limit; always int64
!		note for 'in' loops, this limit is upb(to-expr)
!	S	'step'; used for autovar storing value of step expr expr; always int64
!	A	used with forall autovar storing value of 'to' limit, which is the list
!		to iterate over

	int pos, opc, kwd
	unit pindex, plocal, pfrom, pto, pstep, prange, plist, passign
	unit pcond, pbody, pelse
	unit pto_temp, pstep_temp, ptemp, ptempx, plist_temp, prange_temp
	unit p

	ref strec d

	kwd:=lx.symbol				!for/forall/foreach

	pos:=lx.pos
	lex()						!skip 'for' kwd

!do pindex/plocal
	if kwd=kforsym then
		pindex:=readterm2()
		if pindex.tag<>j_name then serror("For1") fi
		plocal:=nil
	else
		plocal:=readterm2()
		if lx.symbol=commasym then
			lex()
			pindex:=plocal
			plocal:=readterm2()
		else
			pindex:=createname(getavname(currproc))
		fi
		plocal.avcode:='L'
	fi

	pindex.avcode:='I'
	if pindex.tag<>j_name or plocal and plocal.tag<>j_name then
		serror("For: name expected")
	fi

	opc:=j_forup
	pstep:=nil
	pcond:=nil

	if lx.symbol in [insym, inrevsym] then					!assume in/inrev
		if lx.symbol=j_inrev then
			opc:=j_fordown
		fi
		lex()
		prange:=readunit()

!now split prange into from/to parts
		pfrom:=getrangelwbunit(prange)
		pto:=getrangeupbunit(prange)

	else
		if kwd<>kforsym then serror("forall needs ""in""") fi
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
			lex()
			pstep:=readconstexpr(0)
			if pstep^.tag=j_const and pstep^.value=1 then		!by 1
				pstep:=nil
			fi

		else
			pstep:=nil
		fi
	fi

	if pstep=nil then
		pstep:=createconstunit(1,ti64)
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

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

	ptemp:=ptempx:=nil
	pto_temp:=pstep_temp:=plist_temp:=nil

	unless pto^.tag in [j_const,j_name] then
		pto_temp:=createname(getavname(currproc))
		pto_temp.avcode:='T'
		addlistunit(&ptemp,&ptempx,pto_temp)
	end

	unless pstep^.tag in [j_const,j_name] then
		pstep_temp:=createname(getavname(currproc))
		pstep_temp.avcode:='S'
		addlistunit(&ptemp,&ptempx,pstep_temp)
	end

	if kwd<>kforsym then
		plist:=prange
		unless prange^.tag in [j_const,j_name] then
			prange_temp:=createname(getavname(currproc))
			prange_temp.avcode:='A'
			addlistunit(&ptemp,&ptempx,prange_temp)
			plist:=prange_temp
		end
!for forall/foreach, need to include an assignment: local:=pto
!		passign:=createunit2(j_assign,duplunit(plocal),createunit2(j_index,duplunit(plist),duplunit(pindex)))
		passign:=createunit2(j_assignx,duplunit(plocal),createunit2(j_index,duplunit(plist),duplunit(pindex)))
		pbody:=createblock(passign,pbody)

		plocal.nextunit:=plist
	fi

	if pcond<>nil then
		pbody:=makeblock(createunit2(j_if,pcond,pbody))
	fi


!layout:
!pindex
!	pfrom
!	pto
!	pstep (set to const 1 if 'by' not used)
!	[plocal]
!pbody	(can be empty block, or wrapped in if() block when 'when' used)
!	[pelse]
![ptemp] List of 0 to 3 units

	pindex.nextunit:=pfrom
	pfrom.nextunit:=pto
	pto.nextunit:=pstep
	pstep.nextunit:=plocal

	pbody^.nextunit:=pelse

	p:=createunit3(opc,pindex,pbody,ptemp)

	p^.pos:=pos
	return p
end

global proc readtypedef(ref strec owner,int isglobal=0)=
!at 'type' symbol
ref strec sttype,stname
int t,m

lexchecksymbol(namesym)
stname:=lx.symptr

lex()
checkequals()
lex()

sttype:=getduplnameptr(owner,stname,typeid)
adddef(owner,sttype)
m:=createusertype(sttype)
ttusercat[m]:=1

t:=readtypespec(sttype,m)		!should return filled-in version of m

sttype.isglobal:=isglobal

!sttype.mode:=t
storemode(owner,t,sttype.mode)

if t>=0 then
	if ttisnumeric[t] then
		tttarget[m]:=t
	elsif ttisref[t] then
	elsecase ttbasetype[t]
	when tarray,tbits then
	when tslice then
	when trecord then
	when tenum then
	when tdict then
	else
		tttarget[m]:=t
	fi
else
	storemode(owner,t,tttarget[m])
fi

if t>=0 then
	ttisint[m]		:= ttisint[t]
	ttisword[m]		:= ttisword[t]
	ttisreal[m]		:= ttisreal[t]
	ttisinteger[m]	:= ttisinteger[t]
	ttisnumeric[m]	:= ttisinteger[t] ior ttisreal[t]
	ttisshortint[m]	:= ttisshortint[t]
!	ttisbit[m]		:= ttisbit[t]
	ttisref[m]		:= ttisref[t]
	tttypecat[m]	:= tttypecat[t]
else
	ttbasetype[m]:=tpending
fi
end

global proc readrecordfields(ref strec owner,int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
int nvars
ref strec stname,stbitfield

nvars:=0
while lx.symbol=namesym do

	stname:=getduplnameptr(owner,lx.symptr,fieldid)
	storemode(owner,m,stname.mode)
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
		lexchecksymbol(intconstsym)
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
!format is int : (a:1, b:3, c:2)
		lexchecksymbol(lbracksym)

		repeat
			lexchecksymbol(namesym)
			stbitfield:=getduplnameptr(owner,lx.symptr,fieldid)
			stbitfield^.mode:=tbitfield
			adddef(owner,stbitfield)

			stbitfield^.at:=2
			stbitfield^.equivfield:=stname

			lexchecksymbol(colonsym)
			lexchecksymbol(intconstsym)
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
int i,ncols,nrows,enums,nextenumvalue,firstval,lastval,startline,closesym
int ltype
unit plower
ichar enumtypename
ref strec stvar,stenum,stgen
const maxcols=20
[maxcols]ref strec varnameptrs
[maxcols]int varlisttypes
[maxcols]unit plist,plistx
const maxrows=500
[maxrows]int enumvalues

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
while lx.symbol<>eqsym do
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

!checkequals()
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
		if lx.symbol=eqsym then
			lex()
			nextenumvalue:=readconstint()
		fi
		enumvalues[nrows]:=nextenumvalue

		stenum:=getduplnameptr(currproc,stgen,constid)
		stenum.mode:=tint
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

!	stvar.mode:=varlisttypes[i]
	storemode(currproc,varlisttypes[i],stvar.mode)
	stvar^.isglobal:=isglobal

	adddef(currproc,stvar)
	addstatic(stvar)
od
end

global proc readclassdef(ref strec owner,int isglobal)=
!at 'class' symbol
!read enough of the class to be able to generate export data
int kwd, baseclass, m, startline, closesym, mrec, normalexit,isrecord
ref strec nameptr, sttype, newd, d,e

kwd:=lx.symbol
isrecord:=kwd=krecordsym

lexchecksymbol(namesym)
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

mrec:=createrecordmode(owner, m)
sttype.mode:=mrec

sttype^.base_class:=baseclass

closesym:=checkbegin(1)

startline:=getcurrline()

readclassbody(sttype,kwd)

checkbeginend(closesym,kwd,startline)

if baseclass then
	d:=ttnamedef[baseclass]^.deflist
	while d do
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
int kwd,t
ref strec d

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
when kclasssym,krecordsym then
	readclassdef(owner,0)

when ktypesym then
	readtypedef(owner)
when eofsym then
	serror("Class eof?")
	exit
when semisym then
	lex()

!when namesym then			!assume user type
!	++insiderecord
!	t:=newusertypex(lx.symptr)
!	--insiderecord
!	lex()
!	if lx.symbol=dotsym then
!		serror("Can't do a.b type inside class")
!	fi
!	readrecordfields(owner,t)

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

when kmutsym then

	lex()
	if istypestarter() then
readmut::
		++insiderecord
		t:=readtypespec(owner)
		--insiderecord
	else
		serror("need type")
	fi
	readrecordfields(owner,t)

when kletsym then
	serror("Let not allowed")

!when stdtypesym,namesym,lsqsym,krefsym,kicharsym,ktypeofsym then
!when stdtypesym,namesym then!,lsqsym,krefsym,kicharsym,ktypeofsym then
!	goto readmut

else
	if istypestarter() then
		goto readmut
!		serror("record:need var")
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
ref strec enumowner, stname, nameptr
int isanon, index, startline, closesym
unit pone,pindex

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

while lx.symbol=namesym do
	nameptr:=lx.symptr
	lex()
	if lx.symbol=eqsym then	!= follows
		lex()
		pindex:=readunit()
	fi

	if not isanon then
		stname:=getduplnameptr(owner,nameptr,enumid)
		stname^.code:=pindex
!		stname.mode:=typedefx
		stname.mode:=tint
		adddef(owner,stname)
	else
		stname:=getduplnameptr(enumowner,nameptr,constid)
		stname^.code:=pindex
		stname.mode:=tint
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
	typedefx:=createenummode(owner,typedefx)
	return typedefx
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

q^.at:=p^.at

q^.uflags:=p^.uflags		!for ^.uflags
storemode(owner,p.mode,q.mode)
end

proc readimportmodule(ref strec owner)=
!at 'importmodule' symbol
int isnew,startline,closesym
ref strec d,stname,stname0


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
	if ndllnametable>=maxdlllib then
		serror("Too many DLL libs")
	fi
	dllnametable[++ndllnametable]:=stname^.name
	stname^.dllindex:=ndllnametable
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
int pos,fflang

pos:=lx.pos

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

	when kmutsym then
		lex()
		readvardef(owner,1,0,dllvarid, kmutsym)

	when eofsym then
		exit

	when kendsym then
		exit
	else
		PS("symbol")
		serror("Not allowed in importmodule")
	endswitch
od
end

function readequivfield(ref strec owner)ref strec=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
ref strec p,d

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

function readrefproc(ref strec owner,int typedefx,int fflang)int=			!READREFPROC
!'ref' was seen, now positioned at 'proc' 'function' or 'method'
!read proc params and any result, return a complete ref proc spec
int kwd,prettype,m,varparams,nparams
[4]int retmodes
ref strec paramlist,stproc
int rettype2, rettype3, nretvalues
ichar name

kwd:=lx.symbol				!remember whether proc or function
lex()

paramlist:=nil
prettype:=tvoid
nretvalues:=0

!need to create suitable holding typename in advance
name:=nextautotype()
stproc:=getduplnameptr(stmodule,addnamestr(name),typeid)
adddef(stmodule,stproc)
retmodes[1]:=tvoid

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

!for i to nretvalues do
!	storemode(17,owner,retmodes[i],&stproc^.modelist[i])
!	stproc.mode:=retmodes[1]
	storemode(owner,retmodes[1],stproc.mode)
!od
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
unit ulist,ulistx,u

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
unit dlist,dlistx,p,pname,q
ichar name
int opc,noperands
ref strec stname

dlist:=dlistx:=nil

!CPL "ASSEM",lx.lineno iand 16777215

!look at the start of a line first

if lx.symbol=namesym and nexttoken.symbol in [colonsym,dcolonsym] then	!normal label
	p:=createunit0(j_labeldef)
	stname:=getduplnameptr(currproc,lx.symptr,labelid)
	p^.def:=stname
	adddef(currproc,stname)
	lex()			!skip colon
	if oneline then
		lex()
	fi
	return p

elsif lx.symbol=mulsym then		!*name	macro invocation
	lexchecksymbol(namesym)
	pname:=createname(lx.symptr)
	pname^.pos:=lx.pos

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

case lx.symbol
when andlsym then
doop::
	opc:=m_andx
	p:=createunit0(j_assem)
	p.opcode:=opc
	lex()
when orlsym then
	opc:=m_orx
	goto doop
when xorlsym then
	opc:=m_xorx
	goto doop

when notlsym then
	opc:=m_notx
	goto doop

elsif lx.symbol=namesym then				!assume opcode

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
esac

!any labels and opcodes have been read; now look at any operands
if lx.symbol not in [semisym,eofsym] then

noperands:=0

	do
		q:=readassemopnd()

		++noperands
		case noperands
		when 1 then p.a:=q; p.hasa:=1
		when 2 then p.b:=q; p.hasb:=1
		when 3 then p.c:=q; p.hasc:=1
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
	int reg,regix,scale,prefixmode
	unit pcode

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
	when addsym, subsym then
		return readunit()

	when stdtypesym then
		case lx.subcode
		when tu8,tu16,tu32,tu64 then
		else
			serror("Bad prefix")
		esac
		prefixmode:=lx.subcode
		lexchecksymbol(lsqsym)
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

		if lx.symbol=addsym and nexttoken.symbol=namesym and nextlx().symptr.subcode=regsym then
			lex()
		fi
		if lx.symbol=namesym and lx.symptr.subcode=regsym then
			regix:=lx.symptr.index
			lex()
		fi

		if lx.symbol=mulsym then
			lexchecksymbol(intconstsym)
			case scale:=lx.value
			when 1,2,4,8 then
			else
				serror("Bad scale")
			esac
			lex()
		fi

		case lx.symbol
		when addsym, subsym, intconstsym, namesym, lbracksym,ksyscallsym then
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
PS("BAD OPND")
		serror("ASM: Bad operand?")
	esac
	return nil
end

function makeastring:unit =
!current symbol is an 'astring', like a regular string constant, but intended
!to be a byte-array
!Simplest treatment, if not the most efficient, is to turn that into normal 
!makelist unit
unit ulist,ulistx, p, pconst
ref char s
int length

ulist:=ulistx:=nil

s:=lx.svalue
length:=strlen(s)
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
!read 1..maxtuplesize return types as part of function decl
int nretvalues

retmodes[1]:=readtypespec(owner)
nretvalues:=1
while lx.symbol=commasym do
	if nretvalues>=maxtuplesize then
		serror("Too many return values")
	fi
	lex()
	retmodes[++nretvalues]:=readtypespec(owner)
od

return nretvalues
end

function readset:unit=
!positioned at "["
	int length,nkeyvalues,oldirp
	unit p,ulist,ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(j_makeset,nil)
	when colonsym then
		lexchecksymbol(rsqsym)
		lex()
		return createunit1(j_makedict,nil)
	esac

	length:=0
	nkeyvalues:=0

	ulist:=ulistx:=nil

	do
		oldirp:=inreadprint
		inreadprint:=0
		p:=readunit()
		inreadprint:=oldirp
		if p^.tag=j_keyvalue then ++nkeyvalues fi
		++length

		addlistunit(&ulist,&ulistx,p)

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=rsqsym then exit fi
		when semisym then
			lexchecksymbol(rsqsym)
			exit
		when rsqsym then
			exit
		else
			serror("readset?")
		esac	
		skipsemi()						!allow a,b,c;]
	od
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
	if typestarterset[lx.symbol] then return 1 fi
	if lx.symbol=namesym then				!name ...
		case nexttoken.symbol
		when namesym then					!name name
			return 1
		when dotsym then					!name. ...
			if (nexttoken+1).symbol=namesym and (nexttoken+2).symbol=namesym then
				return 1					!name.name name
			fi
		esac
	fi
	return 0
end

function readassignment:unit p=
	int pos,opc

	case lx.symbol
	when namesym then
		case nexttoken.symbol
		when semisym, commasym, rbracksym then
			p:=createname(lx.symptr)
			p.pos:=lx.pos
			lex()
			return p
		esac
	esac

	p:=readorterms()

	if (opc:=lx.symbol) in [assignsym,deepcopysym] then
		pos:=lx.pos
		lex()
		p:=createunit2((opc=assignsym|j_assign|j_deepcopy),p,readassignment())
		p.pos:=pos
	fi
	return p
end

function readorterms:unit p=
	int pos

	p:=readandterms()

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()
		p:=createunit2(j_orl,p,readandterms())
		p.pos:=pos
	od

	return p
end

function readandterms:unit p=
	int pos

	p:=readcmpterms()

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()
		p:=createunit2(j_andl,p,readcmpterms())
		p.pos:=pos
	od

	return p
end

function readcmpterms:unit p=
	int pos,opc

	p:=readrangeterm()

	doswitch opc:=lx.symbol
	when eqsym, nesym, ltsym, lesym, gesym, gtsym, insym, notinsym then
		pos:=lx.pos
		lex()
		p:=createunit2(symboljtags[opc],p,readrangeterm())
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readrangeterm:unit p=
	int pos,opc

	p:=readaddterms()

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(j_makerange,p,readaddterms())
		p.pos:=pos
	fi

	return p
end

function readaddterms:unit p=
	int pos,opc

	p:=readmulterms()

	doswitch opc:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, andbsym, orbsym, xorbsym,
		concatsym, appendsym, prependsym, minsym, maxsym then
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(symboljtotags[opc],p,readassignment())
			p.pos:=pos
			exit
		fi

		p:=createunit2(symboljtags[opc],p,readmulterms())
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readmulterms:unit p=
	int pos,opc

	p:=readpowerterms()

	doswitch opc:=lx.symbol
	when mulsym, divsym, idivsym, iremsym, shlsym, shrsym then
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(symboljtotags[opc],p,readassignment())
			p.pos:=pos
			exit
		fi

		p:=createunit2(symboljtags[opc],p,readpowerterms())
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readpowerterms:unit p=
	int pos,opc

	p:=readterm2()

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(j_power,p,readpowerterms())
		p.pos:=pos
	od

	return p
end

function readterm2:unit=
!	int oldinrp,lineno,opc
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t

	pos:=lx.pos

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
			p.a:=q; p.hasa:=1
		else
			p:=createunit2(j_callfn,p,q)
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
		lexchecksymbol(lsqsym)
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

	p^.pos:=pos

	return p
end

function readterm:unit=
unit p,q,r
ref char pbyte
word64 a
int oldipl,opc,oldinrp,pos,shift,t

	pos:=lx.pos

	switch lx.symbol
	when namesym then
		if nexttoken.symbol=atsym then		!type-punning with user type
			p:=readcast()
		else
			p:=createname(lx.symptr)
			p^.pos:=lx.pos
			lex()
		fi

	when intconstsym,realconstsym then
		p:=createconstunit(lx.value,lx.subcode)
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue,-1)
		lex()

	when astringconstsym then
		p:=makeastring()
		lex()

	when decimalconstsym then
!		(lx.svalue+lx.length)^:=0
		p:=createunit0(j_decimal)
		p^.svalue:=lx.svalue
		p^.slength:=strlen(p.svalue)
		p^.mode:=tdecimal
		lex()

	when charconstsym then
		a:=0
		shift:=0
		pbyte:=lx.svalue
		to strlen(lx.svalue) do
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

	when addsym, subsym, minsym, maxsym, abssym, notlsym, istruesym, inotsym,
		mathsopsym, sqrtsym, sqrsym then
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

	when kcastsym then
		p:=readcastx()

	when ktypeconstsym then
		lexchecksymbol(lbracksym)
		lex()
		p:=createunit0(j_typeconst)

		p.value:=readtypespec(currproc)
		checksymbol(rbracksym)
		lex()

	when kclampsym then
		lexchecksymbol(lbracksym)
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

	when kyieldsym then
		lex()
		p:=createunit1(j_yield,readunit())
		yieldseen:=1

	when kswapsym then			!swap using function syntax
		lexchecksymbol(lbracksym)
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

	when ksyscallsym then
		p:=createunit0(j_syscall)
		p.opcode:=lx.subcode
		lex()

	when knewsym, kdestroysym, kclearsym then
		p:=readnew()

	else
		cpl symbolnames[lx.symbol],=LX.SYMBOL
		serror("readterm?")
	endswitch

!	p^.lineno:=lineno
	p^.pos:=pos
	return p
end

function readxunit:unit=
	return readsunit()
end

function readsunit(int inwhile=0)unit=
int pos,m,sym,opc
unit ulist,ulistx,p,q,r
ref strec stname

pos:=lx.pos
ulist:=ulistx:=nil

repeat
!PS("READSUNIT LOOP")
	while lx.symbol=semisym do
		lex()
	od
	switch lx.symbol
	when kstaticsym then
		lex()
		if lx.symbol in [kletsym,kmutsym] then
			opc:=lx.symbol
			lex()
		else
!			opc:=kmutsym
			opc:=0
		fi
		readvardef(currproc,0,1,staticid,opc)

	when kprocsym,kfunctionsym then
		readprocdef(currproc,0)

	when stdtypesym,lsqsym,krefsym,kicharsym,ktypeofsym,kdictsym,kslicesym then
		if nexttoken.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
			goto doexec
		else
			sym:=0
			goto dovar
		fi

	when kmutsym,kletsym then
		sym:=lx.symbol
		lex()
dovar::
		q:=readvardef(currproc,0,0,frameid,sym)
		while q do								!initialised decls involve code
			r:=q^.nextunit						!unlink from this block first
			q^.nextunit:=nil
			addlistunit(&ulist,&ulistx,q)		!add one by-one
			q:=r
		od

	when ktypesym then
		readtypedef(currproc,0)

	when kconstsym then
		readconstdef(currproc,0)

	when kclasssym,krecordsym then
		readclassdef(currproc,0)

	when docstringsym then
		adddocstring(lx.svalue)
		lex()

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
		case nexttoken.symbol
		when dcolonsym then
			p:=createunit0(j_labeldef)
			stname:=getduplnameptr(currproc,lx.symptr,labelid)
			adddef(currproc,stname)
			p^.def:=stname
			p^.trylevel:=try_level
			lex()
			lx.symbol:=semisym
			addlistunit(&ulist,&ulistx,p)
		when namesym then
			sym:=kmutsym
			goto dovar
		goto doexec

		else
			goto doexec
		esac
	when kdosym then				!u;u;u;do rather than u;u;u do
		if inwhile then
			exit
		fi
		goto doexec

	when kextendtypesym then
		readextendtype(currproc)

	when semisym then
	else							!assume a statement
doexec::
		p:=readunit()
doexec2::
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

ref strec nameptr,stmacro, paramlist,paramlistx, stname

lexchecksymbol(namesym)

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
	ref strec stmacro

	lexchecksymbol(namesym)			!alias name to use
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
	ref strec stmacro

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

function createblock(unit p, q)unit=
!add unit p to front of q, which may be nil, a single unit, or a block
	if q=nil then
		return p
	elsif q.tag=j_block then			!add as first block element
		p.nextunit:=q.a
		q.a:=p
		return q
	else
		p.nextunit:=q
		return makeblock(p)
	fi
end

proc adddocstring(ichar s)=
	if ndocstrings>docstrings.len then
		serror("Too many docstrings")
	fi
	docstrings[++ndocstrings]:=pcm_copyheapstringn(s,strlen(s))
end

global proc readtaggeduniondef(ref strec owner,int isglobal)=
int  m, startline, closesym, mtagged,enummode
ref strec nameptr, sttype

lexchecksymbol(namesym)
nameptr:=lx.symptr
lexchecksymbol(lbracksym)
lex()
enummode:=readtypespec(owner,0)
checksymbol(rbracksym)

lex()
checkequals()

lex()

sttype:=getduplnameptr(owner,nameptr,typeid)
adddef(owner,sttype)
m:=createusertype(sttype)

mtagged:=createtaggedunionmode(owner, m)
sttype.mode:=mtagged
sttype.enumtagmode:=enummode

closesym:=checkbegin(1)

startline:=getcurrline()

readtaggedunionbody(sttype)

checkbeginend(closesym,ktaggedunionsym,startline)

sttype^.isglobal:=isglobal
end

proc readtaggedunionbody(ref strec owner)=
!at first symbol of a taggedunion body
!read fields, constants, types, methods.
!classkwd=kclasssym or krecordsym
int kwd,t
ref strec d,stname
unit tagvalue

doswitch lx.symbol
when semisym then
	lex()

when kendsym,rbracksym,rcurlysym then
	exit

else
	inreadprint:=1
	tagvalue:=readunit()
	inreadprint:=0
	checksymbol(colonsym)
	lex()

	case lx.symbol
	when kmutsym then

		lex()
		if istypestarter() then
	readvar::
			++insiderecord
			t:=readtypespec(owner)
			--insiderecord
		else
			serror("need type")
		fi
!		readtaggedfields(owner,t)
		checksymbol(namesym)
		stname:=getduplnameptr(owner,lx.symptr,fieldid)
		storemode(owner,t,stname.mode)
		adddef(owner,stname)

		stname.code:=tagvalue

		lex()

	else
		if istypestarter() then
			goto readvar
		else
			serror("tagged union?")
		fi
	esac
enddoswitch
end

proc readtaggedfields(ref strec owner,int m)=
!positioned at just after type m has been read
!read vars inside struct for one line of struct body
int nvars
ref strec stname,stbitfield

nvars:=0
while lx.symbol=namesym do

	stname:=getduplnameptr(owner,lx.symptr,fieldid)
	storemode(owner,m,stname.mode)
	++nvars
	adddef(owner,stname)

	lex()

	if lx.symbol<>commasym then
		exit
	fi
	lex()
od

if nvars=0 then
	serror("No tagged fields")
fi
end

proc readextendtype(ref strec owner)=
	ref strec e
	int t, closesym, startline

	lex()
	t:=readtypespec(owner,0)
	checkequals()
	lex()


	e:=pcm_allocz(strec.bytes)
	e.name:="<extendtype>"
	e.namelen:=1

	e.nextdef:=extendtypelist
	extendtypelist:=e

	closesym:=checkbegin(1)

	startline:=getcurrline()

	readclassbody(e,0)
	e.mode:=t

	checkbeginend(closesym,kextendtypesym,startline)
end

function readnew:unit p=
	unit q
	int n

	p:=createunit0(lx.subcode)
	lexchecksymbol(lbracksym)
	lex()

	if lx.symbol<>rbracksym then

		n:=0

		do
			q:=readunit()
			case ++n
			when 1 then p.a:=q; p.hasa:=1
			when 2 then p.b:=q; p.hasb:=1
			when 3 then p.c:=q; p.hasc:=1
			else
				serror("too many args")
			esac
			if lx.symbol=commasym then
				lex()
			else
				exit
			fi
		od

		checksymbol(rbracksym)
	fi
	lex()

	return p
end

proc readoperatordef(ref strec owner)=
	int opc,opsymbol,amode, bmode,rmode
	unit p

	if owner.nameid<>moduleid then
		serror("Opdef not at module level")
	fi

	lexchecksymbol(lbracksym)
	lex()
	opsymbol:=lx.symbol
	if symboloptypes[opsymbol] not in [bin_op,mon_op, prop_op] then
		case opsymbol
		when lsqsym then
			lexchecksymbol(rsqsym)
			opc:=j_index
		when knewsym then
			opc:=j_new
		when kclearsym then
			opc:=j_clear
		else
			serror("Operator name expected")
		esac
	else
		opc:=symboljtags[opsymbol]
	fi
	lex()
	if lx.symbol=assignsym then
		opc:=symboljtotags[opsymbol]
		if opc=0 then
			serror("op:= not supported")
		fi
		lex()
	fi
	checksymbol(rbracksym)
	lexchecksymbol(lbracksym)

!SERROR("READ TYPES....")
	lex()
	amode:=readtypespec(owner)
	if lx.symbol=commasym then
		lex()
		bmode:=readtypespec(owner)
	else
		bmode:=tvoid
	fi
	checksymbol(rbracksym)
	lex()
	rmode:=readtypespec(owner)
	checkequals()

	lex()
	p:=readunit()

CPL =P

	addoverload(owner.moduleno,opc, amode,bmode,rmode,p)
end
=== bb_name.m 14/18 ===
import mlib
import clib

import bb_decls
import bb_tables
import bb_support
import bb_lib
import bb_diags
!import bb_type

ref strec currstproc
int allowmodname=0
int noexpand, noassem
int macrolevels

const maxmacroparams=50
[maxmacroparams]ref strec macroparams
[maxmacroparams]ref strec macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

global proc rx_unit(ref strec owner, unit p)=
ref strec d
unit a,b
int n,oldnoexpand,oldnoassem

a:=p^.a
b:=p^.b
mlineno:=p.pos

!CPL "RXUNIT",JTAGNAMES[P.TAG]

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

when j_callproc, j_callfn then
!CPL "RX/CALLFN",JTAGNAMES[A.TAG],=b
	if a^.tag=j_name then			!can expand possible macro if params not ready
		oldnoexpand:=noexpand; noexpand:=1
		rx_unit(owner,a)
		noexpand:=oldnoexpand
	else
!CPL "CALL2"
		rx_unit(owner,a)
!CPL "CALL3"
	fi

	rx_unitlist(owner,b)
!CPL "CALL4",A,A.NEXTUNIT,A.A,A.B,A.C,"//",A.HASA
!PRINTUNIT(A)


	if a^.tag=j_name then
		d:=a^.def
		case d^.nameid
		when typeid then		!change to type conversion
			p^.tag:=j_convert
			storemode(owner,d^.mode,p.newmode)
			p^.a:=b
			p^.b:=nil; p.hasb:=0
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

!when j_forup,j_fordown then			!a will be j_name unit
!	unit pindex,pfrom,pto,pstep,plocal
!	pindex:=a
!	pfrom:=pindex.nextunit
!	pto:=pfrom.nextunit
!	pstep:=pto.nextunit
!	plocal:=pstep.nextunit
!
!	resolvename(owner,pindex,ti64)
!	rx_unit(owner,pfrom)
!	rx_unit(owner,pto)
!	rx_unit(owner,pstep)
!	if plocal then
!		resolvename(owner,pindex,tany)
!	fi
!
!	goto dobc

!when j_forall, j_forallrev then			!a will be j_name unit
!!CPL "RX/FORALL"
!	resolvename(owner,a,ti64)
!!CPL "RX/FORALL2"
!	a:=a^.nextunit
!!CPL "NAME1:",NAMENAMES[A.DEF.NAMEID]
!!CPL "RX/FORALL3"
!	resolvename(owner,a,tany)			!won't know type until later
!!CPL "NAME2:",NAMENAMES[A.DEF.NAMEID]
!	a:=a^.nextunit
!	goto doabc

else
!CPL "ABC"
doabc::
	if p.hasa then rx_unitlist(owner,a) fi
	if p.hasb then rx_unitlist(owner,b) fi
	if p.hasc then rx_unitlist(owner,p.c) fi
!	rx_unitlist(owner,a)
!dobc::
!	if b then
!		rx_unitlist(owner,b)
!		if p^.c then rx_unitlist(owner,p^.c) fi
!	fi
endswitch
end

global function rx_module(int n)int=
modulerec m
ref strec stmodule, d
int globalflag,status

currmoduleno:=n

rx_passdef(stprogram,moduletable[n].stmodule)

return 1
end

global proc rx_deflist(ref strec owner,p)=
ref strec pstart:=p
	while p do
		rx_passdef(owner,p)
		p:=p^.nextdef
	od
end

global proc rx_passdef(ref strec owner,p)=
ref strec d

case p^.nameid
when moduleid,dllmoduleid then
	rx_deflist(p,p^.deflist)

when procid then
	rx_deflist(p,p^.deflist)
	currstproc:=p
	rx_unit(p,p^.code)
	currstproc:=nil

when dllprocid then
	rx_deflist(p,p^.deflist)

when constid,staticid,frameid,paramid then
	if p^.at=1 then
		rx_unit(owner,p^.equivvar)
	fi
	if p^.code then
		rx_unit(owner,p^.code)
	fi
when typeid then

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

int i,m,extcount,modno
ref strec p,powner,d,e,dlldef,extdef,moddef,extmod,q
[10]ref strec ambiglist

!CPL "RST1"
if owner^.nameid=procid then
	q:=owner^.deflist
	while q do
		if q^.firstdupl=stnewname then		!use that match
			return q
		fi
		q:=q^.nextdef
	od
fi

!CPL "RST2"
p:=stnewname^.nextdupl

extcount:=0
extmod:=dlldef:=extdef:=moddef:=nil

while p do						!for each possibe st entry of the same name
!CPL "RST3"
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

!CPL "RST4"
!if here, then no immediate match
!either of moddef/dlldef will be set
if moddef then				!go with that first
!CPL "RST41"
	return moddef
fi
if extdef then
!CPL "RST42"
	if extcount>1 then
!CPL "RST43"
		for i:=1 to extcount do
			extdef:=ambiglist[i]
			println i,extdef^.owner^.name,namenames[extdef^.owner^.nameid]
		od
		rxerror_s("Ambiguous ext name: #",extdef^.name)
	fi
!CPL "RST44",EXTDEF
	return extdef
fi
!CPL "RST5"
if extmod then return extmod fi
return dlldef				!will be nil when no match
end

global proc resolvename(ref strec owner, unit p)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	ref strec d,e
	unit q
	int moduleno, mode

	d:=p^.def
	moduleno:=p^.moduleno

	if d^.nameid<>nullid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)
	if not e then
		mode:=tvoid
		case p.avcode
		when 'I', 'T', 'S' then mode:=ti64
		when 'L','A' then mode:=tany
		esac
			

!CPL "UNDEF",P.LINENO

		if mode=tvoid then
			rxerror_s("Undefined: #",d^.name,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e^.lineno:=p^.lineno
			if mode<>tany then e^.islet:=1 fi
		fi
	fi

	e^.used:=1

	if e^.nameid=paramid and e^.parammode=out_param then
		p^.tag:=j_ptr
		p^.a:=createname(e)
		p.hasa:=1; p.hasb:=p.hasc:=0
	else
		p^.def:=e			!update link in kcode

		case e^.nameid
		when procid then
			if e^.isglobal then e^.namecat:=globalproc_cat fi
		esac
	fi

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
unit lhs,rhs
ref strec d,e,t
int m

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
			p.hasa:=p.hasb:=0
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
		when trecord then
		when tref then
			do
				m:=tttarget[m]
				case ttbasetype[m]
				when trecord then
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

!proc fixmode(ref typenamerec p, ref posrec pos)=
proc fixmode(ref typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
	ref int32 pmode
	ref strec a,d,e,f,owner
	int m,moduleno

	pmode:=p.pmode

	m:=-pmode^					!typename index

!CPL "FIXMODE1"

	d:=owner:=p.owner
	while d.nameid<>moduleid do d:=d.owner od
	moduleno:=d.moduleno

	a:=p.defa
	d:=p.defb

!cpl "FIX",OWNER,=A,=D
	if a=nil and d then			!simple type name V

!CPL "RESOLVE:",OWNER.NAME, D.NAME, MODULENO
		e:=resolvetopname(owner,d,moduleno,0)
!CPL =E

	elsif d=nil and a then		!typeno
		rxerror("Fixmode can't do typeof yet")
	else						!assume a.d type reference
!CPL "DOING A.B"
		e:=resolvetopname(owner,a,moduleno,0)
!CPL =E
		if e then
			f:=e.deflist
			e:=nil
			while f do
				if f.nameid=typeid and f.firstdupl=d then
!CPL "FOUND F",F.NAME,STRMODE(F.MODE)

					e:=f
					exit
				fi
				f:=f.nextdef
!			else
!				e:=nil
			od

		fi

	fi

	if e then
!CPL "FM2"
		pmode^:=e.mode
!CPL "FM3"

	else
		rxerror_s("2:Can't resolve tentative type: #",d.name)
	fi
end

global proc fixusertypes=
	ref typenamerec p
	int npasses,notresolved,m,zerosizes


!CPL("FIXUSERTYPES")
!RXERROR("FIXUSERTYPES")

	npasses:=0

!FOR I TO NTYPENAMES DO
!	IF TYPENAMES[I].PMODE=NIL THEN
!
!		CPL I,"NIL PMODE....",TYPENAMEPOS[I].LINENO,SOURCEFILENAMES[TYPENAMEPOS[I].FILENO]
!		STOP
!	FI
!OD


	repeat
		++npasses
		notresolved:=0

		for i to ntypenames do
			p:=&typenames[i]

			if p.pmode^<0 then
				mlineno:=typenamepos[i].pos
				fixmode(p)
				if p.pmode^<0 then
					++notresolved
				fi
			fi
		od

		if npasses>5 then
			rxerror("Fixtypes: too many passes (cyclic ref?)")
		fi

	until notresolved=0


!!	repeat
!		zerosizes:=0
!		for i:=tlast to ntypes when ttbasetype[i]=tpending do
!	CPL "FIXUPUT/PENDING:",I,TTNAMEDEF[I].NAME
!			m:=tttarget[i]
!			if ttsize[m] then
!	CPL "TARGET=",STRMODE(M)
!				ttbasetype[i]:=ttbasetype[m]
!				ttsize[i]:=ttsize[m]
!				ttlower[i]:=ttlower[m]
!				ttlength[i]:=ttlength[m]
!			else
!				++zerosizes
!			fi
!		od
!	until zerosizes=0
!	CPL =NPASSES

end

proc rx_assem(ref strec owner, unit p,a,b)=
!.a is just a const string containing the asm line, but with "#" inserted where
! resolved name is to go
!.b is a list of j_name units, each one pointing to a generic strec entry
! corresponding to the next # in the assem string
!First, resolve names
!Second, replace each # by resolved name

unit q
ref strec d
ref char s,pdest
[512]char str
ref strbuffer expr
int c

CPL "RX/ASSEM"

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
RXERROR("ASM/TXNC")
!				tx_namedconst(d)
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
	ref strec e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,e^.mode)
	adddef(owner,e)
	return e
end

proc converteqeq(ref strec owner,ref unitrec p)=
!detect exprs such as a=b=c and convert to a=b and b=c
int leftop,rightop
ref unitrec w,y1,y2,z

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
unit q

unit plist,plistx
plist:=plistx:=nil
while p do
	q:=copyunit(p)
	addlistunit(&plist,&plistx,q)
	p:=p^.nextunit
od
return plist
end

function copyunit(unit p)unit=
unit q
ref strec d

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

!!q^.b:=copylistunit(p^.b)
!!q^.c:=copylistunit(p^.c)
!!q^.lineno:=p^.lineno
!q^.pos:=p^.pos
!q^.value:=p^.value			!copy main field of each union
!q^.opcode:=p^.opcode
!q^.mode:=p^.mode
!q^.newmode:=p^.newmode
!q^.moduleno:=p^.moduleno
!q^.isastring:=p^.isastring
!q^.nextunit:=nil
!
!q.reginfo:=p.reginfo

q^:=p^
q.nextunit:=nil
if q.hasa then q.a:=copylistunit(q.a); q.hasa:=1 fi
if q.hasb then q.b:=copylistunit(q.b); q.hasb:=1 fi
if q.hasc then q.c:=copylistunit(q.c); q.hasc:=1 fi

return q
end

proc replaceunit(unit p,q)=
!replace p with q, keeping same address of p, and same next pointer
!original contents discarded
unit pnext
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
ref strec d,pm
unit pnew
int ignoreargs

!CPL "EXPANDMACRO"
if macrolevels>10 then
	rxerror("Too many macro levels (recursive macro?)")
fi

d:=a^.def

!First step: get list of macro formal parameters
!CPL =D.NAME,NAMENAMES[D.NAMEID]

pm:=d^.paramlist
nmacroparams:=0
while pm do
!CPL =PM.NAME,=PM.NEXTPARAM,NAMENAMES[PM.NAMEID]
	if nmacroparams>=maxmacroparams then
		rxerror("macro param overflow")
	fi
	macroparams[++nmacroparams]:=pm
	macroparamsgen[nmacroparams]:=pm^.nulldef
	pm:=pm^.nextparam
od

!CPL =NMACROPARAMS

!now get macro args into a list
nmacroargs:=0

!RETURN

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

pnew:=copyunit(d.code)

if not ignoreargs then				!normal expansion
	replaceunit(p,pnew)
else								!keep call and paramlist; just replace fn name
	p^.a:=pnew						!with expansion
fi
end

=== bb_type.m 15/18 ===
import msys
import mlib
import clib
import oslib

import bb_decls
import bb_tables
import bb_support
import bb_lib
import bb_name
import bb_diags

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
int countedfields
int inassem

proc tpass(unit p, int t=tany, lv=no_lv)=
ref strec d
unit a,b
int oldmlineno,m,nparams,paramtype,restype

if p=nil then return fi

!if lv in [need_lv, addrof_lv, indexlv_lv] and (not refunitset[p^.tag] or 
!	p.tag=j_slice) then
!
!	case p.tag
!	when j_const, j_callfn, j_callproc then
!		txerror("not allowed as lvalue")
!	esac
!fi

oldmlineno:=mlineno

mlineno:=p^.pos

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

when j_maths then
	tx_maths(p,a,b)

!when j_atan2, j_fmod then
!	tx_atan2(p,a,b)

when j_shl, j_shr then
	tx_shl(p,a,b)

when j_iand, j_ior, j_ixor then
	tx_iand(p,a,b)

when j_eq, j_ne then
	tx_eq(p,a,b)

when j_lt, j_le, j_ge, j_gt then
	tx_lt(p,a,b)

when j_same then
	tx_same(p,a,b)

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

when j_iandto, j_iorto, j_ixorto, j_andlto, j_orlto, j_appendto, j_concatto then
	tx_iandto(p,a,b)

when j_if then
	tx_if(p,a,b,p^.c,t,lv)

when j_longif then
	tx_longif(p,a,b,t,lv)

when j_index then
	tx_index(p,a,b,t,lv)

when j_ptr then
	tx_ptr(p,a,t,lv)

when j_callproc, j_callfn then
	tx_callproc(p,a,b,t)

when j_dot then
	tx_dot(p,a,b,lv)

when j_sqrt then
	tx_sqrt(p,a)

!when j_sqrt, j_sin, j_cos, j_tan, j_asin, j_acos, j_atan,
!	 j_ln, j_lg, j_log, j_exp, j_round, j_floor, j_ceil, j_fract  then

when j_sign then
	tx_sign(p,a)

when j_power then
	tx_power(p,a,b)

when j_andl, j_orl, j_xorl, j_andb, j_orb then
	tx_andl(p,a,b)

when j_neg, j_abs, j_sqr then
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

when j_lenstr then
	tx_lenstr(p,a)

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

!when j_keyindex then
!	tx_keyindex(p,a,b,p.c,lv)

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
when j_negto, j_absto, j_inotto, j_notlto then
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
	while b do
!		tx_unitlist(b)
		if b.tag=j_fmtitem then
			tpass(b.a)
			tpass(b.b,trefchar)
		else
			tpass(b)
		fi
		b:=b.nextunit
	od
	tx_unitlist(p^.c)

when j_forup, j_fordown then
	tx_for(a,b,p^.c)

!when j_forall, j_forallrev then
!	tx_forall(a,b,p^.c)

when j_to then
	tpass(a,ti64)
	tpass(b,tvoid)

when j_autocast then
	tpass(a)
	if t=tany then txerror("cast() needs type") fi
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

!when j_convertref then
!	tpass(a)
!
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
		t:=gettypebase(t)
	fi
	p.mode:=t

when j_in, j_notin then
	tx_in(p,a,b)

when j_recase then
	tpass(a,ti64)
	if a^.tag<>j_const then
		txerror("recase must be const")
	fi

!when j_head, j_tail, j_init, j_last, j_take, j_drop, j_reverse, j_left, j_right,
!	 j_convlc, j_convuc, j_flexptr, j_stringz, j_dupl then
!	tx_head(p,a,b)

when j_prepend, j_append,j_concat then
	tx_concat(p,a,b)

when j_cvlineno then
	p^.mode:=ti64
when j_cvfilename,j_cvmodulename then
	p^.mode:=trefchar

when j_bitfield then
	tx_bitfield(p,a,lv)

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

!when j_asc then
!	tpass(a,tvar)
!	p.mode:=ti64
!
!when j_chr then
!	tpass(a,tvar)
!	p.mode:=tvar

else
CPL "TXUNIT: CAN'T DO:",jtagnames[p^.tag]
doelse::
	tx_unitlist(a,t)
	tx_unitlist(b,t)
	tx_unitlist(p^.c,t)
endswitch

!CPL "TPASS5"
tevaluate(p)
!CPL "TPASS6"

case p^.tag
when j_makelist, j_return then
else
!if p^.tag<>j_makelist and p^.tag<>jthen
	if t<>tany and t<>tvoid and p^.mode<>t then		!does not already match
!CPL "TPASS65",P,T
		coerceunit(p,t)			!apply soft conversion
!CPL "TPASS66"
	fi
esac
!CPL "TPASS7"
if t=tvoid then
	fixvoidunit(p)
fi
!CPL "TPASS8"

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
	int i,u

	for i:=tuser to ntypes do

		setmodesize(i)
	od
end

proc setmodesize(int m)=
	int size,target

	if ttsize[m] then return fi

	mlineno:=ttlineno[m]
	case ttbasetype[m]
	when tarray then
		setarraysize(m)
	when trecord then
		setrecordsize(m)
	when tvoid,tproc then
	when tslice then
		setslicesize(m)
	when tauto then
TXERROR("SETMODESIZE/AUTO?")
	when tany then

	when tpending then
		target:=tttarget[m]
		setmodesize(target)

		ttbasetype[m]:=ttbasetype[target]
		ttsize[m]:=ttsize[target]
		ttlower[m]:=ttlower[target]
		ttlength[m]:=ttlength[target]
		ttnamedef[m]:=ttnamedef[target]
		tttypecat[m]:=tttypecat[target]


	when tenum then
!CPL "SMS/ENUM",TTSIZE[M]
	ttsize[m]:=8

	else
		if size:=ttsize[ttbasetype[m]] then
			ttsize[m]:=size
			return
		fi





		cpl "SIZE 0:",strmode(m),=m,=stdtypenames[ttbasetype[m]],ttnamedef[m].name
!		txerror("can't set mode size")
CPL"********",strmode(m), ttlineno[m]
		CPL("Can't set mode size")
!txerror("SMS")
	esac
end

proc setarraysize(int m)=
int lower,length,elemsize,target
unit pdim,a,b

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

!CPL "SAS",=M,TTSIZE[M]
	case ttsize[m]
	when 1,2,4 then tttypecat[m]:=tc_d124
	when 8 then tttypecat[m]:=tc_d8
	when 16 then tttypecat[m]:=tc_d16
	esac
	if tttypecat[m]<>tc_blk then
		ttbasetype[m]:=tsmallarray
	fi

end

proc setslicesize(int m)=
unit pdim

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
unit a,b

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
		if isboolunit(p.a) then
			deleteunit(p,p^.a)
		fi
	fi

when j_istruel then
	tpass(a)
	if isboolunit(a) then
		deleteunit(p,a)
	fi
else
	tpass(p)
	twidenopnd(p)
	if not isboolunit(p) then
		insertunit(p,j_istruel)
	fi
esac


p^.mode:=ti64
end

global function tx_module(int n)int=
modulerec m
ref strec stmodule, d
int globalflag,status

currmoduleno:=n

!CPL "TXMODULE",MODULETABLE[N].NAME

tx_passdef(moduletable[n].stmodule)

return 1
end

global proc tx_passdef(ref strec p)=
ref strec d
int oldmlineno,simplefunc
unit q

if p^.txdone then
	return
fi

oldmlineno:=mlineno
mlineno:=p^.pos
simplefunc:=1

d:=p^.deflist
while d do
	tx_passdef(d)

!	if p^.nameid in [procid,frameid] and d^.nameid=paramid then
	if p^.nameid=procid and d^.nameid in [frameid,paramid] then
!	if p^.nameid=procid and d^.nameid =paramid then
		unless issimpletype(d^.mode) then
!IF eqstring(p.name,"readparamsff_types") then
! CPL "1",P.NAME,"SETTING SIMPLE 0",D.NAME,STRMODE(D.MODE) FI
			simplefunc:=0
		end
	fi

	d:=d^.nextdef
od

q:=p^.code

case p^.nameid
when procid then
	currproc:=p
!CPL "PROC",P.NAME
    tpass(q,(currproc^.nretvalues>1|ttuple|p^.mode))
!IF eqstring(p.name,"readparamsff_types") then CPL "2",P.NAME,=SIMPLEFUNC FI

!see if simple function
	if p^.nretvalues>1 or not issimpletype(p^.mode) then
		simplefunc:=0
	fi

!IF eqstring(p.name,"readparamsff_types") then CPL "3",P.NAME,=SIMPLEFUNC FI

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
int m
unit dcode

m:=d^.mode
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

!CPL "----------HERE",STRMODE(M)

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

	if ttbasetype[m] in [tarray,tsmallarray] and ttlength[m]=0 then
		d^.mode:=dcode^.mode
	fi
else
	d^.circflag:=0
	d^.txdone:=1
fi
end

global proc tx_namedconst(ref strec d)=
int m

if d^.circflag then
	txerror("Circular const reference detected")
fi

unit q
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
unit q
int pmode

case p^.tag
when j_const then
	return
when j_makelist then
	q:=p^.a
	while q do
		checkconstexpr(q)
		q:=q^.nextunit
	od

!when j_convertref then
!	if tttarget[p^.a^.mode]=tvoid then
!		p^.a^.mode:=p^.mode
!		deleteunit(p,p^.a)
!	else
!		goto error
!	fi

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

if ttisinteger[q.mode] then
!case tttypecode[q^.mode]
!when 'I','U' then
	if ttsize[q^.mode]=16 then
		GERROR("GETCONSTINT/128")
	fi
	return q^.value
elsif ttisreal[q.mode] then
!when 'R' then
	return q^.xvalue
else
	cpl strmode(q^.mode)
	txerror("Getconstint: not int32/64")
fi
return 0
end

proc tevaluate(unit p)=
unit a,b
int ischar
int tag:=p^.tag

!CPL "EVALX",P
if jisexpr[tag]=2 then
	tevalbinop(p)

elsif jisexpr[tag]=1 then
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
int64 a,b,c
real x,y,z

!CPL "EVALB"

if p^.a^.tag<>j_const or p^.b^.tag<>j_const then
	return
fi

if ttsize[p^.mode]>8 then return fi

if ttisint[p.mode] then
!case tttypecode[p^.mode]
!when 'I' then
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
elsif ttisreal[p.mode] then
!when 'R' then
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
fi

end

proc tevalmonop(unit p)=
unit a
int64 ix,iy,iz
real x,z

!CPL "EVALM"

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
TXERROR("BITWIDTH")
!		if a^.tag=j_typeconst then
!			makenewconst(p,ttbitwidth[a^.value])
!		else
!			makenewconst(p,ttbitwidth[a^.mode])
!		fi
	esac

	return
fi

if ttisinteger[p.mode] then
!case tttypecode[p^.mode]
!when 'I','U' then

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

elsif ttisreal[p.mode] then
!when 'R' then
	x:=a^.xvalue
	switch p^.tag
	when j_neg then
		z:=-x
	else
		return
	end
	makenewconst(p,int64@(z))

fi
end

proc tevalconvert(unit p)=
unit q
int64 a,b
int32 a32
word64 u
real64 x
real32 x32
int s,t
ref int128 p128

!CPL "EVALC",JTAGNAMES[P.TAG]

q:=p^.a
!CPL "EVALC1",Q

tevaluate(q)
!CPL "EVALC2"

s:=q^.mode
case p^.opcode
when c_soft then
DOSOFT::
delmode::
	q^.mode:=p^.mode
	deleteunit(p,q)
	return

when c_reftoref then
	goto dosoft
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
!	if s=ti128 or s=tu128 then
!		a:=getlow128(q^.pvalue128)
!	fi
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
	when ti128,tu128 then
		p128:=pcm_allocz(int128.bytes)
		putlow128(p128,a)
		if ttisint[s] and a<0 then
			puthigh128(p128,0xFFFF'FFFF'FFFF'FFFF)
		fi
		makenewconst(p,int64@(p128))
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
p.a:=p.b:=nil
p.hasa:=p.hasb:=0
p^.value:=x
p^.isconst:=1
if t<>tvoid then
	p^.mode:=t
fi
end

proc tx_name(unit p,int t,lv)=
ref strec d
int oldmlineno
unit pcode
oldmlineno:=mlineno

d:=p^.def
mlineno:=d^.pos

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
CPL D.NAME,=LV,D.ISLET
	txerror("Can't use 'let' as lvalue")
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
CPL =MLINENO,=d.pos,=p.pos
MLINENO:=P.POS
CPL NAMENAMES[D.NAMEID]
	txerror_ss("TNAME? # #",namenames[d^.nameid],d^.name)
endswitch
mlineno:=oldmlineno

end

proc getdominantmode(int tag,s,t, &u,&v)=
int sbase:=ttbasetype[s],tbase:=ttbasetype[t]

if sbase<=tany and tbase<=tany then
	u:=dominantmode[sbase,tbase]
	if u=tref then			!can't return ref void; choose one
		u:=s
	fi
	v:=u
	return
fi

u:=v:=s

!if s=tvar or t=tvar then
!	u:=v:=tvar
if comparemodes(s,t) then
else
	u:=v:=0
fi

end

proc getdominantmodepp(unit p,a,b, int &u,&v)=
int abase,bbase,amode,bmode,tag

abase:=ttbasetype[amode:=a^.mode]
bbase:=ttbasetype[bmode:=b^.mode]

!CPL "GETDOMPP",STRMODE(ABASE),STRMODE(BBASE),AMODE,BMODE,=COMPAREMODES(AMODE,BMODE)

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

!	elsif bbase=tflexstring and amode=trefchar then
!		u:=v:=bmode
	else								!error
		u:=v:=0
	fi

	return

elsif abase<=tany and abase<=tany then
!CPL "GETDOMPP/ABASE<TANY"
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
elsif comparemodes(amode,bmode) then
	return

!elsif abase=tvar or bbase:=tvar then
!	u:=v:=tvar
!	return
fi

u:=v:=0
end

proc coerceunit(unit p,int t,hard=0)=
!p's mode is not t; apply coercion
!hard=0: is an implicit conversion
!hard=1: is an explicit conversion
int s,sbase,tbase,cc,starget,ttarget,result

if t=tvoid then return fi

s:=p^.mode

retry::


!CPL "COERCE",STRMODE(S), "=>",STRMODE(T)

if s=t then return fi

if comparemodes(s,t) then return fi

if s=tvoid and t<>tvoid then
CPL "COERCE"; PRINTUNIT(P)
	txerror("Void type not allowed in expr")
fi

sbase:=ttbasetype[s]
tbase:=ttbasetype[t]
result:=t

if sbase>=tany or tbase>=tany then			!at least one not simple scalar
	cc:=0
	case sbase
	when tarray,tsmallarray then
		case tbase
		when tslice then
			insertunit(p,j_slice)
			p.mode:=t
			return
		esac
	when tref then
		if s=trefchar and tbase=tslice then
			tstringslice(p,t)
			return
		fi
	when ttuple then
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
	p^.mode:=p^.newmode:=t
	tevalconvert(p)
	return
elsif ttisshortint[sbase] and ttisreal[tbase] then			!short to float
	cc:=(ttisint[sbase]|c_ifloat|c_ufloat)
	goto gotcc
elsif ttisreal[sbase] and ttisshortint[tbase] then			!float to short
	cc:=(ttisint[tbase]|c_ifix|c_ufix)

elsif ttisinteger[sbase] and tbase=tenum then
	p.mode:=t
	return
!	cc:=cc_none

else									!both under tany so scalars
	cc:=conversionops[sbase,tbase]

gotcc::
	case cc
	when c_error then
		txerror_ss("Conversion not allowed: # => #",strmode(s),strmode2(t))
	when c_none then
	when c_reftoref then
		starget:=tttarget[s]
		ttarget:=tttarget[t]
		if starget=ttarget then
			return
		elsif starget=tvoid or ttarget=tvoid then
		elsif not hard then
			if not comparemodes(s,t) then
				cpl Strmode(s),"||",Strmode2(t)
				txerror("ref->ref needs explicit cast")
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
int u,v

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
CPL =STRMODE(A.MODE), =STRMODE(B.MODE)
	txerror("add/no dom",p)
fi

coerceunit(a,u)
coerceunit(b,v)
p^.mode:=u

if p^.tag=j_subref and ttisref[v] then
	p^.mode:=ti64
fi

if ttbasetype[u]=tref then
	if b^.tag=j_const and ttbasetype[b^.mode]<>tref then			!scale offset and keep as normal add (won't be sub)
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
int u,v
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

!if stdtypecode[u]<>'R' and p^.tag=j_div and u<>tvar then
if not ttisreal[u]<>'R' and p^.tag=j_div then
	p^.tag:=j_idiv
fi

end

proc tx_shl(unit p,a,b)=
!shl/shr
tpass(a)
tpass(b)

twidenopnd(a)

unless ttisinteger[a.mode] then
	txerror("SHL/not int")
endunless

!if b.mode<>tvar then
	coerceunit(b,ti64)
!fi
p^.mode:=a^.mode
end

proc tx_iand(unit p,a,b)=
!iand/ior/ixor
int u,v

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
int abase,bbase,atype,btype,u,v

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

proc tx_same(unit p,a,b)=
int abase,bbase,atype,btype,u,v

tpass(a)
tpass(b)

TXERROR("SAME")
!unless a.mode=b.mode=tvar then
!	txerror("isequal: must be vars")
!end

p^.mode:=ti64
end

proc tx_lt(unit p,a,b)=
int amode,abase,bmode,bbase,u,v

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
unit q
ref strec d,e,pm
[maxparams]ref strec paramlist
[maxparams]unit arglist,newarglist
int nparams,i,j,k,nargs,m,kwdused,qm
ichar name

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
	p^.mode:=ttuple
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
			txerror_s("Param not optional: #",strint(i))
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
unit ulist:=nil, ulistx

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
int u

tpass(a)
!if not (ttflags[ttbasetype[a^.mode]] iand m_numeric) then
if not isnumericmode(a^.mode) then
CPL =STRMODE(A.MODE)
	txerror("Neg: not numeric",a)
fi
twidenopnd(a)

u:=a^.mode
coerceunit(a,u)
p^.mode:=u
end

proc tx_if (unit p,a,b,c,int t,lv) =
	int u,v

	tcond(a)

	tpass(b,t,lv)
	if t<>tvoid and not c then
		txerror("if needs else")
	fi
	tpass(c,t,lv)

	if t=tany then			!unknown types (eg. print)
!CPL "IF/ANY"
		getdominantmodepp(p, b,c,u,v)
!CPL =STRMODE(U),STRMODE(V)
		coerceunit(b,u)
		coerceunit(c,u)
		p^.mode:=u
	else				!know exactly what type needed
		p^.mode:=t
	fi
end

proc tx_longif (unit p,a,b,int t,lv) =
	unit q,r
	int u,v

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
	txerror("incr: not int/ref/var")
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

proc tx_for(unit pindex,pbody,ptemps)=
unit pfrom, pto, pstep, plocal, plist
int u,mlist,elemtype

pfrom:=pindex.nextunit
pto:=pfrom^.nextunit
pstep:=pto^.nextunit
plocal:=pstep.nextunit

tpass(pindex)
u:=pindex.mode

tpass(pfrom,u)
tpass(pto,u)
tpass(pstep,u)

if plocal then
	plist:=plocal.nextunit
	tpass(plist)
	mlist:=plist.mode

	case ttbasetype[mlist]
	when tarray,tsmallarray then
		elemtype:=tttarget[mlist]
!	when tvar then
!		elemtype:=tvar
	when tslice then
		elemtype:=tttarget[mlist]
	else
		txerror("forall/can't iterate")
	esac

	tpass(plocal)
	if plocal.mode=tany then
		plocal.mode:=elemtype
		plocal.def.mode:=elemtype
	fi
fi

tpass(pbody,tvoid)
tpass(pbody^.nextunit,tvoid)	!optional else
end

proc tx_index(unit p,a,b,int t,lv) =
!p is an index unit
!a is an array, b is an index
!t is the needed type for the element being indexed
int amode,emode,pmode,tmode,tbasemode

tpass(a,,lv)
deref(a)
amode:=a.mode

!tpass(b)			!index
tpass(b,ti64)			!index

!if b.mode<>tvar then
!	coerceunit(b,ti64)
!fi

if ttbasetype[amode] not in [tarray, tsmallarray, tslice, tmanarray] then
	txerror_s("Can't index: #",strmode(amode))
fi
!case ttbasetype[amode]
!when tvar then
!	p.mode:=tvar
!else
	p.mode:=tttarget[amode]
	twiden(p,lv)
!esac
end

proc tx_makerange(unit p,a,b)=
int u,v, amode,bmode

tpass(a)
tpass(b)

amode:=a^.mode
bmode:=b^.mode

!if ttisvar[amode] or ttisvar[bmode] then
!	coerceunit(a,tvar)
!	coerceunit(b,tvar)
!	p.mode:=tvar
!	return
!fi

if not ttisinteger[amode] or not ttisinteger[bmode] then
	txerror("range not int")
fi
!if tttypecode[amode]<>tttypecode[bmode] then
!	txerror("range: mixed i64/u64")
!fi
getdominantmodepp(p,a,b,u,v)

if ttisint[amode] then
	coerceunit(a,ti64)
	coerceunit(b,ti64)
else
	coerceunit(a,tu64)
	coerceunit(b,tu64)
fi
p^.mode:=trange
end

proc tx_makeset(unit p,a, int t)=
int x,y,isconst
int64 lower,upper
ref void pvoid

if t=tvoid then
	txerror("open(var) set type")
fi

lower:=2 billion
upper:=-2 billion

isconst:=1

while a do
	tpass(a)

	if not a^.isconst then
!TXERROR("MAKESET NOT CONST")
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
!p.hasa:=p.hasb:=0
!p^.range_lower:=lower
!p^.range_upper:=upper
!if isconst then
!	p^.mode:=createsetmodek(nil,(p^.length|upper+1|0),0)
!else

p^.mode:=tset
!fi
end

proc tx_makedict(unit p,a, int t)=
int x,y,isconst,km,vm
ref void pvoid

if t=tvoid then
	txerror("open(var) dict type")
fi

!if ttbasetype[t]<>tflexdict then
!	txerror("not dict type")
!fi

!km:=ttkeymode[t]
!vm:=tttarget[t]
!
!isconst:=1
!while a do					!a is a keyvalue type
!	tpass(a^.a,km)
!	tpass(a^.b,vm)
!
!	a:=a^.nextunit
!od

p^.isconst:=isconst
p^.mode:=tdict
end

proc tx_ptr(unit p,a,int t,lv)=
ref strec d

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
!when tvar then
!	p^.mode:=tvar
else
	txerror("PTR: need ref T")
esac
twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]ref strec fieldlist
	int i,nfields,indent,nrfields,size,index
	ref strec d,e
	ref char flags
	const ss='S', ee='E'
	int flag

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

	case ttsize[m]
	when 1,2,4 then tttypecat[m]:=tc_d124
	when 8 then tttypecat[m]:=tc_d8
	when 16 then tttypecat[m]:=tc_d16
	esac
	if tttypecat[m]<>tc_blk then
		ttbasetype[m]:=tsmallrecord
	fi
end

proc scanrecord(int state,ref[]ref strec fields, int &index, &isize, offset)=
 	ref strec e,f,ea
	int size:=0,fieldsize,bitoffset

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
!				if bitoffset>ttbitwidth[f^.equivfield^.mode] then
				if bitoffset>ttsize[f.equivfield.mode]*8 then
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
	int alength,tlength,elemtype,newt, i, nfields,isconst, m
	unit q,b
	ref strec e

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
	when tarray,tsmallarray then
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

	when trecord,tsmallrecord then
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
!CPL "MAKELIST/SL"
		if a=nil or (b:=a^.nextunit; b=nil) or b^.nextunit then
			txerror("bad slice init")
		fi
		p.b:=b
		a.nextunit:=nil
		tpass(a,,lv)
		if ttbasetype[a^.mode]<>tref then txerror("slice init not ref") fi
		if not comparemodes(tttarget[a.mode],tttarget[t]) then
!CPL =STRMODE(TTTARGET[A.MODE])
!CPL =STRMODE(TTTARGET[T])
			txerror("slice/ptr mismatch")
		fi
!m:=tttarget[a.mode]
!CPL =STRMODE(M),STRMODE(A.MODE)

		tpass(b,ti64)
		p^.mode:=t
		p.tag:=j_makeslice

!	when tflexarray then
!		elemtype:=tttarget[t]
!		q:=a
!		while q do
!			tpass(q,elemtype,lv)
!			unless q^.tag=j_const then isconst:=0 end
!			q:=q^.nextunit
!		od
!
!		p^.mode:=t

!	when tvoid,tvar then
	when tvoid then
		q:=a
		if p.makearray then
			if q=nil then txerror("array()?") fi
			tpass(q,,lv)
			m:=q.mode
			q:=q.nextunit
		else
TXERROR("MAKELIST1")
!			m:=tvar
		fi

		while q do
			tpass(q,m,lv)
			unless q^.tag=j_const then isconst:=0 end
			q:=q^.nextunit
		od

!*!		p^.mode:=tvar
	else
		txerror_s("Unknown makelist type: #",strmode(t))
	esac

	p^.isconst:=isconst

	tpass(p.b,ti64)

end

proc tx_dot(unit p,a,b,int lv)=
int recmode,recbasemode,i,j,newtag,tmode
unit q,pindex
ref strec d,dequiv

tpass(a,,lv)			!lhs, yeields ref array type

recmode:=a^.mode

recbasemode:=ttbasetype[recmode]

while recbasemode=tref do
	tmode:=tttarget[recmode]
	insertunit(a,j_ptr)
	recmode:=a^.mode:=tmode
	recbasemode:=ttbasetype[recmode]
od

if ttbasetype[recmode] not in [trecord,tsmallrecord] then
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
		pindex^.mode:=trange
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

!if ttisvar[d^.mode] then
!	CPL "DOT:NEED TO REMOVE ADDR OF"
!	removeaddrof(a)
!FI

p^.offset:=d^.offset
twiden(p,lv)
end

function resolvefield(ref strec d, int m)ref strec=
	ref strec e,t

	case ttbasetype[m]
	when trecord,tsmallrecord then
	when tref then
		m:=tttarget[m]
		if ttbasetype[m] not in [trecord,tsmallrecord] then
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
int sbase, tbase
ref strec d,e

!CPL "COMPAREMODES", =strmode(S), STRMODE(T)

if s=t then return 1 fi
sbase:=ttbasetype[s]
tbase:=ttbasetype[t]

if sbase<>tbase then return 0 fi
case sbase
when tref then
	return comparemodes(tttarget[s],tttarget[t])
when tarray, tsmallarray then
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

	switch p.tag
	when j_eq, j_ne, j_lt, j_le, j_ge, j_gt, j_andb, j_orb,
		j_andl, j_orl, j_notl, j_istruel, j_xorl, j_inrange, j_inset then
		1
	else
		0
	end switch
end

proc checkbool(int m)=
!check that mode can be converted to bool, ie. can have istrue applied

!unless tttypecode[m] then
!!unless stdtypeflags[m] iand (m_numeric + m_ref) then
!	txerror_s("Can't convert to bool: #",strmode(m))
!end unless
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
int m

tpass(a)
deref(a)
m:=a^.mode

case ttbasetype[m]
when tarray,tsmallarray then
	convintconst(p,ttlower[m]+ttlength[m]-1)
when tslice,tmanarray then
else
	txerror_s("UPB #",strmode(m))
esac
p^.mode:=ti64
end

proc tx_len(unit p,a)=
int m

tpass(a)
deref(a)

m:=a^.mode

case ttbasetype[m]
when tarray,tsmallarray then
	convintconst(p,ttlength[m])
when tslice,tmanarray then
else
	txerror_s("LEN #",strmode(m))
esac
p^.mode:=ti64
end

proc tx_lenstr(unit p,a)=
	int m

	tpass(a)
	m:=a^.mode

	if m<>trefchar then
		txerror("ichar expected")
	fi

	if a.tag=j_const then
!CPL "ISCONST"
		deleteunit(p,a)
		p.tag:=j_const
		p.value:=p.slength
	fi

	p.mode:=ti64
	p.isastring:=0

!	TXERROR("LENSTR")
!case ttbasetype[m]
!when tarray then
!	convintconst(p,ttlength[m])
!when tslice,tvar then
!else
!	txerror_s("LEN #",strmode(m))
!esac
!p^.mode:=ti64
end

proc tx_lwb(unit p,a)=
int m

tpass(a)
deref(a)
m:=a^.mode

case ttbasetype[m]
when tarray, tsmallarray then
	convintconst(p,ttlower[m])
when tslice then
	convintconst(p,ttlower[m])
!when tvar then

!when tflexstring then
!	convintconst(p,1)
else
	txerror_s("LWB #",strmode(m))
esac
p^.mode:=ti64
end

proc tx_bounds(unit p,a)=
int m,lower,upper
ref int128 p128

tpass(a)
deref(a)
m:=a^.mode

case ttbasetype[m]
when tarray,tsmallarray then
	lower:=ttlower[m]
	upper:=lower+ttlength[m]-1
when tslice,tmanarray then
	p.mode:=trange
	return
else
	txerror_s("BOUNDS #",strmode(m))
esac

p^.tag:=j_const
p^.mode:=trange	!createrangemode(currproc,ti64,0)

TXERROR("BOUNDS")

!p128:=pcm_alloc(int128.bytes)
!putlow128(p128,lower)
!putlow128(p128,upper)
!p^.pvalue128:=p128
p^.a:=p^.b:=p^.c:=nil
p^.isconst:=1
end

proc tx_sliceptr(unit p,a)=
int m,tmode

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
int u,atype

	tpass(a)

!	if a.mode=tvar then
!		p.mode:=tvar
!		return
!	fi

	unless ttisinteger[a.mode] then
		txerror("INOT/not int")
	end unless
	twidenopnd(a)

	u:=ttbasetype[a^.mode]
	coerceunit(a,u)
	p^.mode:=u
end

proc tx_atan2(unit p,a,b)=
int u

tpass(a)
tpass(b)

!if a.mode=tvar or b.mode=tvar then
!	u:=tvar
!else
	u:=tr64
!fi

coerceunit(a,u)
coerceunit(b,u)
p.mode:=u

if not ttisreal[u] and p^.tag=j_div then
	p^.tag:=j_idiv
fi
end

proc tx_swap(unit p,a,b)=
tpass(a,,need_lv)
tpass(b,,need_lv)

if not comparemodes(a^.mode,b^.mode) then
	txerror("SWAP: type mismatch")
fi

p^.mode:=tvoid
end

proc tx_select(unit p,a,b,c, int t,lv)=
int i,u,v
unit q

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
int amode,u,v
unit wt,w

if p^.tag=j_docase and lv then gerror("&docase") fi

tpass(a)

if a=nil then
	amode:=tany
else
	amode:=a^.mode
fi

!if ttflags[amode] iand m_int and ttsize[amode]<8 then
if ttisinteger[amode] and ttsize[amode]<8 then
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
			unless ttisinteger[amode] then txerror("case: need int index") end
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
int issub, atype, btype, u

tpass(a,,need_lv)
tpass(b)

issub:=p^.tag=j_sub
atype:=ttbasetype[a^.mode]
btype:=ttbasetype[b^.mode]

if p^.tag=j_divto and not ttisreal[atype] then
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
!when tflexstring then
!	if ttisinteger[btype] then
!		u:=ti64
!	fi

esac

coerceunit(b,u)

if isrefmode(atype) then
	if b^.tag=j_const and btype<>tref then			!scale offset and keep as normal add (won't be sub)
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

unless ttisinteger[a.mode] then
	txerror("iandto: not int")
end
p^.mode:=tvoid
end

proc tx_negto(unit p,a)=
int issub, atype, btype, u

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
int m

tpass(a)
m:=a^.mode
if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
	txerror("goto: not label")
fi
end

proc tx_switch(unit p,a,b,c,int t,lv)=
[0:2001]byte valueset
unit wt, w
int ax,bx,i,u,v

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
		when trange then			!assume makerange
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
int u,v

tpass(a)
tpass(b)

getdominantmodepp(p,a,b,u,v)
!u:=v:=dominantmode[a^.mode, b^.mode]
coerceunit(a,u)
coerceunit(b,v)
p^.mode:=u
end

proc tx_addroffirst(unit p,a,int t)=
!&.x maps to &x[x.lwb]
	int m

	tpass(a)
	m:=a^.mode
	if ttbasetype[m] not in [tarray,tsmallarray] then
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
int u,tmax
int64 x
ref int128 aa

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
			aa:=pcm_allocz(int128.bytes)
			puthigh128(aa,0x8000'0000'0000'0000)
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
			aa:=pcm_allocz(int128.bytes)
			putlow128(aa,0xFFFF'FFFF'FFFF'FFFF)
			puthigh128(aa,0x7FFF'FFFF'FFFF'FFFF)
			x:=cast(aa)
			tmax:=ti128
        when tu8,tc8 then x:=255
        when tu16,tc16 then x:=65535
        when tu32 then x:=4294967295
        when tu64 then x:=0; --x; tmax:=tu64
        when tu128 then
			aa:=pcm_allocz(int128.bytes)
			putlow128(aa,0xFFFF'FFFF'FFFF'FFFF)
			puthigh128(aa,0xFFFF'FFFF'FFFF'FFFF)
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
 	int m,nvalues,nret,i
	unit q

	m:=currproc^.mode
	nret:=currproc^.nretvalues
!CPL =CURRPROC.NRETVALUES

	if a=nil then
		if nret then
			txerror("return value(s) missing")
		fi
		return
	elsif nret=0 then
		txerror("Superfluous return value")
	fi

	if a^.tag=j_makelist then
CPL "RETURN/MAKELIST"
!		a^.tag:=j_returnmult
!		if a^.length<>nret then
!			txerror("Wrong number of return values")
!		fi
!		q:=a^.a				!point to list of return values
!		for i to nret do
!			tpass(q,currproc^.modelist[i])
!			q:=q^.nextunit
!		od
!
!		deleteunit(p,a)			!don't need return
!		if t=tvoid then
!			p^.mode:=tvoid
!		else
!			p^.mode:=tmult
!		fi

	else
		if nret>1 then txerror("RETERROR?") fi
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			p^.mode:=tvoid
		else
			deleteunit(p,a)
		fi
	fi
end

proc tx_dotindex(unit p,a,b,int lv) =
!a.[b], a is an int
int pmode
unit i,j

tpass(a,,lv)			!lhs

pmode:=tu64

if not ttisinteger[a.mode] then
!	case a^.mode
!	when tvar then
!		pmode:=tvar
!	else
		txerror("a.[i]: not int/str value")
!	esac
fi

tpass(b)			!index

case ttbasetype[b^.mode]
when trange then
	i:=b.a
	j:=b.b
	if i.tag=j.tag=j_const then
		if i.value>j.value then
			swap(b^.a,b^.b)
		fi
	fi
!when tvar then
else					!assume simple index
	coerceunit(b,ti64)
esac

p^.mode:=pmode
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

!CPL "SLICE"

	tpass(a)			!lhs
	tpass(b)			!will be a range

!CPL =STRMODE(A.MODE)

	if a.mode=trefchar then
		p.mode:=createslicemodek(currproc,tc8,1,0)
	else
		deref(a)
		case ttbasetype[a.mode]
		when tarray,tsmallarray then
			p.mode:=createslicemodek(currproc,tttarget[a.mode],1, 0)
	
		when tslice then
			p.mode:=a.mode
	
!		when tvar then
!			p.mode:=tvar
		else
	CPL =STRMODE(A.MODE)
			txerror("a[i..j]: not array")
		esac
	fi
end

proc tx_assign(unit p,a,b,int t)=
int m

case a^.tag
when j_makelist then
	tx_multassign(a,b)
when j_dotindex, j_dotslice then
	tx_dotindex(a,a^.a,a^.b,need_lv)
	tpass(b,a.mode)
else
!CPL "ASSIGN",JTAGNAMES[A.TAG]
	if a.tag=j_name and a.def.islet and p.initlet then
!CPL "HERE"
		tpass(a)
	else
		tpass(a,,need_lv)
	fi
	m:=a^.mode

	if ttisshortint[m] then
		tpass(b,gettypebase(m))
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
unit p,q,lhs,rhs
int nretmodes,i
ref strec d				!point to def containing return mode info

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
TXERROR("MULTASS/MODELIST")
!		if p^.mode<>d^.modelist[i++] then
!			txerror("mult ass/mult fn needs exact type match")
!		fi
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
	tpass(b)
	p.mode:=ti64

!	if a.mode=tvar or a.mode=tvar then
!		coerceunit(a,tvar)
!		coerceunit(b,tvar)
!		return
!	fi

	unless ttisinteger[a.mode] then
		txerror("'in' opnd must be int")
	end

	case b.tag
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

	elsif ttisinteger[b.mode] then
!when 'I','U' then
	else
		txerror("in rhs must be range/set")
	esac

end

function getprocretmodes(unit p)ref strec=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
ref strec d
unit a

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
unit q

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
!intended for widening narrow types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
	int m,u,mbase

!CPL "WIDEN",LVNAMES[LV]
	mbase:=ttbasetype[m:=p^.mode]

	case lv
	when need_lv then               !assume is OK (since this is only called for mem opnds)
	when index_lv,indexlv_lv then
!		unless ttisvar[m] then
!			if lv=indexlv_lv or (stdtypecode[mbase]='A' and stdtypecat[mbase]=tblock) or\
TXERROR("TWIDEN")

!			if lv=indexlv_lv or (mbase in [tarray,tslice, tmanarray] and stdtypecat[mbase]=tblock) or\
!				 mbase=trecord then		!insert ref
!				if p^.tag=j_ptr then
!					deleteunit(p,p^.a)
!				else
!					insertunit(p,j_addrof)
!					p^.mode:=createrefmode(nil,m)
!				fi
!			fi
!		end unless
	when addrof_lv then
		txerror("widen/addrof")
	esac
end

function twidenshort(unit p)int=
	if p^.tag=j_const then
		p^.mode:=gettypebase(p.mode)
		return p^.mode
	fi

	insertunit(p,j_convert)
	case p^.newmode:=gettypebase(p.mode)
	when ti64 then p^.opcode:=c_iwiden
	when tu64,tc64 then p^.opcode:=c_uwiden
	esac

	p^.mode:=p^.newmode

	return p^.mode
end

proc tx_concat(unit p,a,b)=
!does all of head tail init drop reverse prepend append concat left right
	int u,v

	tpass(a)
	tpass(b)
	p^.mode:=a^.mode
!	if a.mode<>tvar then
		txerror("head/etc can't be used with this type")
!	fi

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
	int newlen,alen:=a^.slength, blen:=b^.slength
	ref char newstr
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

proc tstringslice(unit p, int slicemode)=
!p is a string; insert conversions to turn it into a slice:
	unit b

	if tttarget[slicemode]<>tc8 then
		txerror("Not char slice")
	fi

	insertunit(p,j_slice)

	b:=duplunit(p.a)
	insertunit(b,j_lenstr)
	tx_lenstr(b,b.a)
	p.b:=createunit2(j_makerange,createconstunit(1,ti64),b)
	p.b.mode:=trange

	p.mode:=slicemode
end

proc tx_bitfield(unit p,a,int lv)=
int i,j,bitsize,topbit
unit r

tpass(a,,lv)

if not ttisinteger[a^.mode] then
	txerror("Int needed")
fi

bitsize:=ttsize[ttbasetype[a^.mode]]*8
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
	r^.mode:=trange
	p^.tag:=j_dotslice
	p^.b:=r
fi

p^.mode:=tu64
end

proc deref(unit a)=
!a is a unit that needs to be dereferenced because it's about to used as:
! a[i]
! a[i..j]
! a.lwb, a.upb, a.len
!Ie in an array context
int abasemode, tmode

abasemode:=ttbasetype[a.mode]

while abasemode=tref do
	tmode:=tttarget[a.mode]
	insertunit(a,j_ptr)
	a.mode:=tmode
	abasemode:=ttbasetype[a.mode]
od

end

proc tx_sqrt(unit p,a)=
	tpass(a)

	unless isnumericmode(a^.mode) then
		txerror("maths: not numeric")
	end unless
	coerceunit(a,tr64)
	p.mode:=tr64
end

proc tx_maths(unit p,a,b)=
!CPL "TXMATHS"
	tpass(a)

!	unless isnumericmode(a^.mode) then
!		txerror("maths: not numeric")
!	end unless
	coerceunit(a,tr64)
	if b then
		tpass(b,tr64)
	fi

	p.mode:=tr64
end
=== bb_genpcl.m 16/18 ===
import mlib
import clib
import oslib

import bb_decls
import bb_support
import bb_tables
import bb_lib
import bb_libpcl
import bb_blockpcl
import bb_diags

!const entrypointname = "main"
const entrypointname = "start"
const entrypointname2 = "main"

global function codegen_pcl:int=
!generate code for module n
ref strec d,e
ref procrec pp

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
[256]char str
[256]char name
int paramoffset,nparams,retaddrbytes
ref strec d
int n,lab,np,offset,reg,i,xreg,isstart,structret,isfloat,hasparams,hasequiv
unit q

!*!setalign(16)

!CPL "PROCDEF",P.NAME,=P.SIMPLEFUNC

!sprintf(&.str,"proc %s ----------------",p^.name)
strcpy(&.name,p.name)
convucstring(&.name)
fprint @&.str,"PROC # #",&.name,"=":"60P="
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
!	if d^.nameid=frameid and ttisvar[d.mode] then
!		initframedef(d)
!	fi
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
!	if d^.nameid in [frameid,paramid] and gettypecat_t(d^.mode)=tvar then
!		freeframevar(d)
!	fi
	d:=d^.nextdef
od

if isstart then
!	pushstack(40)					!stack misaligned here
	pushstack(32)
	genpc(k_pushint, zero_opnd);
!	setpclmode_t(ti64)
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
[256]char str
int offset

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
	unit p

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
		if tttypecat[d.mode]<>tc_man8 then
			genpc(k_istatic,genmem_d(d))
			pccodex.align:=getalignment(d.mode)
			genidata(d.code)
		else
			goto dozstatic
		fi
	else
dozstatic::
		genpc(k_zstatic,genmem_d(d),genint(ttsize[d^.mode]))
		pccodex^.align:=getalignment(d^.mode)
	fi
end

proc dostaticvariant(ref strec d)=
	unit p

!GERROR("STATIC VAR")

!	if d^.imported or d^.at=1 or not d^.code then return fi
	if d^.imported or d^.at=1 then return fi

	if tttypecat[d.mode]<>tc_man8 then return fi
GERROR("INIT STATIC MANVAR")

!no init needed when not initialised, as it will start off as all zeros
!which is 'void'

!	if d^.code then				!have an expression to assign
!		evalunit(d^.code)
!		genpc(k_popmem,genmem_d(d))
!		pccodex^.catmode:=tscalar
!	else						!
!		genpc(k_initmemz,genmemaddr_d(d))
!		pccodex^.catmode:=tvar
!	fi

end

proc genidata(unit p,int doterm=1, am='A')=
int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion,tbase
unit q,a,b
ref strec d
real32 sx
ref pclopndrec ax

t:=p^.mode
mlineno:=p^.lineno
tbase:=ttbasetype[t]

case p^.tag
when j_const then
!	case tttypecode[p^.mode]
	if ttisref[p.mode] then
!	when 'P' then
		if p^.mode=trefchar then
			if p^.svalue then
				genpc(k_dq,genstrimm(p^.svalue,p^.slength))
			else
				genpc(k_dq, genint(0))
			fi
		else
			genpc(k_dq,genint(p^.value))
		fi
	elsif ttisreal[p.mode] then
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
			genpc(k_dq,genint(p.range_lower))
			genpc(k_dq,genint(p.range_upper))
		else
			gerror_s("IDATA/INT:%s",strmode(p^.mode),p)
		esac

	fi

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
ref procrec pp
ref strec d

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
	dostaticvariant(d)
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
!d is a type that must be initialised
!can't use any actual initial value, as that might not be done until later
!in the code
!	genpc(k_initmemz,genmem_d(d))
!	setpclcat_t(tvar)
end

proc freeframevar(ref strec d)=
!d is a type that must be freed
!	genpc(k_freemem,genmem_d(d))
!	setpclcat_t(tvar)
end

function getconstframeoffset(unit p)int=
!p is the @ equiv expression
!extract offset if possible
unit a
ref strec d
int offset

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
=== bb_libpcl.m 17/18 ===
import msys
import mlib
import clib
import oslib

import bb_decls
import bb_support
import bb_tables
import bb_lib
!import mc_common

global int retindex
global int initstaticsindex

global const ptrsize=8

global record pclopndrec =
	union
		ref strec def
		unit code
		int value
		word uvalue
		real xvalue
		ichar svalue
		ref int128 pvalue128
		ref word128 puvalue128
		int labelno
		int pbytes
		int floatmap
	end
	int16 optype
	int16 size			!for real: 4 or 8 bytes
	union
!		int32 size
		int32 fbytes
		int32 nargs
	end
end

global record pclrec =
	int16 opcode
	union
		int16 cond
		int16 align
		int16 index
	end
	int32 size
	int16 mode				!actual mode for catmode
	int16 mode2				!actual mode for catmode2

	byte catmode			!basetype or category mode
	byte catmode2
	byte isglobal
	byte isfunction
	byte isvariadic
	byte SPARE3
	int16 fileno

	int32 lineno
	ref pclrec nextpcl
	pclopndrec a,b
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

	(k_stackall,	$),	!
	(k_startmult,	$),	!
	(k_resetmult,	$),	!
	(k_endmult,		$),	!

	(k_istatic,		$),	!
	(k_zstatic,		$),	!
!	(k_staticfn,	$),	!
	(k_equiv,		$),	!

	(k_pushmem,		$),	! Xa := A (mem strec)
!	(k_pushconst,	$),	! Xa := A (const unit)
!	(k_dpushconst,	$),	! Xa := A (const unit) then stack
	(k_dpushmem,	$),	! Xa := A (mem strec) then stack
!	(k_pushimm,		$),	! Xa := A (imm int/word/real/string etc)
!	(k_pushdec,		$),	! Xa := A (imm int/word/real/string etc)
	(k_pushaddr,	$),	! Xa := &A (mem)
	(k_pushptr,		$),	! Xa := Xa^
	(k_pushretslot,	$),	! Xa := Empty(T)
	(k_pushffretval,$),	! Xa := <value in D0 or XMM0>
	(k_pushretval,	$),	! Xa := <value in D1/D0 or XMM0>
	(k_stackargs,	$),	! Force last operand to be on real stack

	(k_pushint,		$),	! Xa := A (int64/word64)
	(k_pushreal,	$),	! Xa := A (real64)
	(k_pushint128,	$),	! Xa := A (int128/word128)
	(k_pushstr,		$),	! Xa := A (ref char)

	(k_dpushint,	$),	! Xa := A (int64/word64) then stack
	(k_dpushreal,	$),	! Xa := A (real64) then stack

	(k_makeint,		$),	! Xa := var(A), A is int
	(k_makereal,	$),	! Xa := var(A), A is real
	(k_makestr,		$),	! Xa := var(A), A is ref stringz
	(k_makedec,		$),	! Xa := var(A), A is ref stringz to become a Decimal

	(k_popmem,		$),	! A := Xa
	(k_popmemz,		$),	! A := Xa; dest not freed
	(k_popptr,		$),	! Ya^ := Xb
	(k_storemem,	$),	! (Xb, Ya) := Xa, A := Xa (duplicate stack then pop one)
	(k_storeptr,	$),	! ??(Xb, Ya) := Xa, A := Xa (duplicate stack then pop one)
	(k_unstack,		$),	! SP -:= A
	(k_popretval,	$),	! Store return value into return slot (fns that use vars)
	(k_moveretval,	$),	! Ensure return value is in D0/XMM0/D1:D0 (simple fns)
	(k_copyblock,	$),	! Xb^ := Ya^ for blocks
	(k_initmemz,	$),	! A := Empty
	(k_freemem,		$),	! free(A)
	(k_dupl,		$),	! dupl(A)

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
	(k_muli,		$),	! Xa := Xb*Ya (flex*int)

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
	(k_keyindex,	$),
	(k_storekeyindex,$),
	(k_popkeyindex,	$),

	(k_storeslice,	$),
	(k_popslice,	$),

	(k_indexref,	$),
	(k_indexmemref,	$),
	(k_indexmemcref,$),
	(k_keyindexref,	$),
!	(k_dotindexref,	$),
	(k_popdotindex,	$),
	(k_popdotslice,	$),
	(k_dotref,		$),

	(k_slice,		$),
!	(k_popslice,	$),

	(k_lwb,			$),
	(k_upb,			$),
	(k_len,			$),
	(k_bounds,		$),
	(k_lenstr,		$),

	(k_sqrt,		$),
	(k_sqr,			$),
	(k_sign,		$),
	(k_sin,			$),
	(k_cos,			$),
	(k_tan,			$),
	(k_asin,		$),
	(k_acos,		$),
	(k_atan,		$),
	(k_atan2,		$),
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
	(k_asc,			$),
	(k_chr,			$),

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
	(k_appendto,	$),
	(k_concatto,	$),
	(k_andlto,		$),
	(k_orlto,		$),

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
	(k_notlto,		$),

	(k_incrtomem,	$),	! ++A
	(k_decrtomem,	$),	! --A
	(k_incrto,		$),	! ++Xa^
	(k_decrto,		$),	! --Xa^

	(k_preincrtox,	$),
	(k_predecrtox,	$),
	(k_postincrtox,$),
	(k_postdecrtox,$),

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
	(k_unbox,		$),
	(k_box,			$),
	(k_typepun,		$),
	(k_ichartostring,	$),

	(k_swap,		$),
	(k_makerange,	$),
	(k_makelist,	$),
	(k_makeslice,	$),
	(k_makeset,		$),
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

	(intimm_opnd,		$),
	(int128imm_opnd,	$),
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

global ref pclrec pccode, pccodex		!genpc adds to this linked list

global ref pclrec allpclcode

[1..4]pclopndrec pclbuffers				!operand data written into this circular buffer
int nextpclindex=1						!next available slot

global int framebytes			!local stackframe size
global int parambytes
global int frameoffset
global int isthreadedproc
global int iscallbackproc

pclopndrec $zero_opnd
global ref pclopndrec zero_opnd

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
ref pclrec p, oldp

p:=pcm_allocz(pclrec.bytes)

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
ref pclopndrec a

a:=&pclbuffers[nextpclindex++]

if nextpclindex>pclbuffers.len then
	nextpclindex:=1
fi
return a
end

global function duplopnd(ref pclopndrec a)ref pclopndrec=
ref pclopndrec b
b:=newpclopnd()
if a=b then gerror("DUPLOPND/CLASH") fi
b^:=a^
return b
end

proc writepclblock(ref pclrec m)=
!block single block of mc code, usually belonging to one proc
!initstr=1 to initialise string o/p for single block
!initgenstr() when initstr
int i

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
ref strec d
ref procrec pp

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
	ref pclopndrec a
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
	ref pclopndrec a
	a:=newpclopnd()
	a^.optype:=strimm_opnd
	a^.svalue:=pcm_copyheapstring(s)
!	a^.size:=ptrsize
	return a
end

global function gensys(int fnindex)ref pclopndrec=
	ref pclopndrec a
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
!static [512]char str
static [1512]char str
[1512]char opnds
[1256]char opnd2
[1128]char opcname
pclopndrec a,b
int opcode,cond,sizepref

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
!	strcat(&.opcname,stdtypenames[pcl^.catmode])
	strcat(&.opcname,typecatnames[pcl.catmode]+3)
fi

ipadstr(&.opcname,16," ")

strcpy(&.str,"  ")


ipadstr(&.str,8)

strcat(&.str,&.opcname)

if a.optype and b.optype then		!2 operands
	strcpy(&.opnd2,stropnd(&b))
!	sprintf(&.opnds,"%s,\t%s", stropnd(&a),&.opnd2)
!	fprint @&.opnds,"#,\t#", stropnd(&a),&.opnd2
	fprint @&.opnds,"#,   #", stropnd(&a),&.opnd2

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

!if pcl^.catmode2 or pcl^.mode2 then
!!	sprintf(&.opnds," Target: %s (%s)",stdtypenames[pcl^.catmode2],
!!			(pcl^.mode2|strmode(pcl^.mode2)|"-"))
!	fprint @&.opnds," Target: # (#)",stdtypenames[pcl^.catmode2],
!			(pcl^.mode2|strmode(pcl^.mode2)|"-")
!	strcat(&.str,&.opnds)
!fi

!if pcl^.align and opcode not in [k_syscall, k_jumpcc] then
if pcl^.align then
	case opcode
	when k_syscall, k_jumpcc, k_jumpccimm then
	else
		print @&.opnds," Align:",,pcl^.align
		strcat(&.str,&.opnds)
	esac
fi

if opcode in [k_callff, k_callptrff] then
!	sprintf(&.opnds," Nargs:%d",pcl^.a.nargs)
	print @&.opnds," Nargs:",,pcl^.a.nargs
	strcat(&.str,&.opnds)
fi


ipadstr(&.str,54,"-")
strcat(&.str," C:"); strcat(&.str,typecatnames[pcl.catmode]+3)
ipadstr(&.str,63)
strcat(&.str," M:"); strcat(&.str,strmodev(pcl.mode))
ipadstr(&.str,80,"-")
strcat(&.str," ||C2:"); strcat(&.str,typecatnames[pcl.catmode2]+3)
ipadstr(&.str,88)
strcat(&.str," M2:"); strcat(&.str,strmodev(pcl.mode2))
ipadstr(&.str,96)
strcat(&.str,"cai:")
strcat(&.str,strint(pcl.cond))


return &.str
end

function strmodev(int m)ichar=
	if m=tvoid then return "-" fi
	return strmode(m)
end

global function stropnd(ref pclopndrec a)ichar=
static [512]char str

case a^.optype
when no_opnd then
	return ""

when mem_opnd then
	return strmemopnd(a^.def)

when memaddr_opnd then
	return strmemaddropnd(a^.def)

!when const_opnd then
!	return strconstopnd(a^.code)

when intimm_opnd then
	print @&.str,a^.value

!when wordimm_opnd then
!	print @&.str,a^.uvalue

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

when int128imm_opnd then
	print @&.str,a^.pvalue128^

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
static [16]char str
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
[256]char str
int iscallback

	iscallback:=iscallbackfn(currproc)

	genpc(k_procexit)
end

global function genint(int64 x,int size=8)ref pclopndrec=
ref pclopndrec a
a:=newpclopnd()
a^.optype:=intimm_opnd

a^.value:=x
return a
end

global function genint128(ref int128 pa)ref pclopndrec=
ref pclopndrec a

a:=newpclopnd()
a.optype:=int128imm_opnd

a.pvalue128:=pa
return a
return a
end

global function genreal(real64 x,int size=8)ref pclopndrec=
ref pclopndrec a

a:=newpclopnd()
a^.optype:=realimm_opnd
a^.xvalue:=x
a^.size:=size
return a
end

!global function genconst_u(unit p,int size=0)ref pclopndrec=
!!assume p is a const unit, or possible a name (gives a name
!	ref pclopndrec a
!
!	a:=newpclopnd()
!	a^.optype:=const_opnd
!	a^.code:=p
!!	a^.mode:=p^.mode
!!	a^.size:=(size|size|ttsize[p^.mode])
!
!return a
!end

global function genassem_u(unit p)ref pclopndrec=
!assume p is a const unit, or possible a name (gives a name
	ref pclopndrec a

	a:=newpclopnd()
	a^.optype:=assem_opnd
	a^.code:=p
	return a
end

global function genlabel(int x,isglobal=0)ref pclopndrec=
!x is a label index
!generate immediate operand containing label
	ref pclopndrec a

	a:=newpclopnd()
	a^.optype:=label_opnd
	a^.labelno:=x
	return a
end

global function genmem_u(unit p,int size=0)ref pclopndrec=
return genmem_d(p^.def,ttsize[p^.mode])
end

global function genmem_d(ref strec d,int size=0)ref pclopndrec=
	ref pclopndrec a

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
	ref pclopndrec a

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

global function isint32const(unit p)int=
int64 a
	if isconstint(p) and ttsize[p^.mode]<=8 then
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
	[256]char str

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
		genpc(k_stackargs)
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
	static [256]char str
	fprint @&.str,"[#]",d^.name
	return &.str
end

function strmemaddropnd(ref strec d)ichar=
	static [256]char str
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

static [,2]int16 maptable = (
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
!	(j_maths,	k_maths),
!	(j_sin,		k_sin),
!	(j_cos,		k_cos),
!	(j_tan,		k_tan),
!	(j_asin,	k_asin),
!	(j_acos,	k_acos),
!	(j_atan,	k_atan),
!	(j_ln,		k_ln),
!	(j_lg,		k_lg),
!	(j_log,		k_log),
!	(j_exp,		k_exp),
!	(j_round,	k_round),
!	(j_floor,	k_floor),
!	(j_ceil,	k_ceil),
!	(j_fract,	k_fract),
!	(j_fmod,	k_fmod),

	(j_addto,	k_addto),
	(j_subto,	k_subto),		!to be completed...

	(0,	0))

static [0..jtagnames.upb]int16 convtable
int opc

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
		when tc_d124 then pccodex^.catmode2:=tc_x4
		when tc_d8 then pccodex^.catmode2:=tc_x4
		when tc_d16 then gerror("widefloat?")
		esac
	else

		case pccodex^.catmode
		when tc_d124 then pccodex^.catmode:=tc_x4
		when tc_d8 then pccodex^.catmode:=tc_x8
		when tc_d16 then gerror("widefloat2?")
!		when tr32 then
!			if pccodex^.opcode=k_pushreal then
!				pccodex^.catmode:=tc_x4
!			fi
		esac
	esac
end

global proc setpclcat_u(unit p)=
	setpclcat_t(p^.mode)
end

global proc setpclcat_t(int m)=
!set catmode to broad category mode
	pccodex^.catmode:=tttypecat[m]
	pccodex^.mode:=m

!	if ttcat[m] then				!set up for records
!		pccodex^.catmode:=ttcat[m]		!to block/wide/scalar etc
!	fi
end

global proc setpclmode_u(unit p)=
	setpclmode_t(p^.mode)
end

global proc setpclmode_t(int m)=
!set catmode to basetype
!for records, a basic trecord will do (no matter if block or wide)
	pccodex^.catmode:=tttypecat[ttbasetype[m]]
	pccodex^.mode:=m
end
=== bb_blockpcl.m 18/18 ===
import mlib
import clib
import oslib

import bb_decls
import bb_support
import bb_tables
import bb_lib
import bb_libpcl
import bb_diags

const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

!const dodotchains=0
const dodotchains=1

const maxnestedloops	= 50

[maxnestedloops,4]int loopstack
int loopindex							!current level of nested loop/switch blocks

const maxparams=100

const maxswitchrange=500
const maxcases=maxswitchrange

const maxcasedepth=20
[maxcasedepth]unit casestmt
[maxcasedepth]int caseelse
int casedepth

ref[]int sw_labeltable			!set from do-switch
ref[]int sw_valuetable
int sw_lower
int sw_ncases					!1..n for serial switch; 0 for simple
byte sw_defaultseen				!starts at 0, set to 1 when default: seen
int sw_defaultlabel
int sw_breaklabel

int maxreg=0

global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
unit a,b
ref strec d
STATIC INT LEVEL

!	mlineno:=p^.lineno
	mlineno:=p^.pos

	a:=p^.a
	b:=p^.b

!++LEVEL
!TO LEVEL DO PRINT "   " OD
!CPL "EVALUNIT",JTAGNAMES[P.TAG],MLINENO IAND 16777215, SOURCEFILENAMES[MLINENO>>24]
!CPL "EVALUNIT",JTAGNAMES[P.TAG],MLINENO:"H"

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
!	when j_forall        then do_forall(p,a,b)
!	when j_forallrev     then do_forallrev(p,a,b)
!	when j_foreach       then do_foreach(p,a,b)
!	when j_foreachrev    then do_foreachrev(p,a,b)
	when j_while         then do_while(p,a,b)
	when j_repeat        then do_repeat(p,a,b)
	when j_goto          then do_goto(p,a,b)
!	when j_gotoblock     then do_gotoblock(p,a,b)
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
!	when j_lambda        then do_lambda(p,a,b)

!	when j_andl          then do_andl(p,a,b)		!short-circuit code
!	when j_orl           then do_orl(p,a,b)			!(has problems with eg. byte operands)

	when j_andl          then do_bin(p,a,b,k_iand)	!non-short-circuit code
	when j_orl           then do_bin(p,a,b,k_ior)

	when j_xorl          then do_bin(p,a,b,k_ixor)


	when j_andb          then do_bin(p,a,b,k_iand)
	when j_orb           then do_bin(p,a,b,k_ior)

!	when j_xorl          then do_xorl(p,a,b)

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
!	when j_applyop       then do_applyop(p,a,b)
!	when j_applyopx      then do_applyopx(p,a,b)
!	when j_andand        then do_andand(p,a,b)
	when j_eq            then do_setcc(p,a,b)
	when j_ne            then do_setcc(p,a,b)
	when j_lt            then do_setccx(p,a,b)
	when j_le            then do_setccx(p,a,b)
	when j_ge            then do_setccx(p,a,b)
	when j_gt            then do_setccx(p,a,b)
	when j_same          then do_same(p,a,b)
	when j_add           then do_bin(p,a,b,k_add)
	when j_sub           then do_bin(p,a,b,k_sub)
	when j_mul           then do_muldiv(p,a,b,k_mul)
	when j_div           then do_bin(p,a,b,k_div)
	when j_idiv          then do_muldiv(p,a,b,k_idiv)
	when j_irem          then do_muldiv(p,a,b,k_irem)
!	when j_divrem        then do_bin(p,a,b,k_divrem)
	when j_iand          then do_bin(p,a,b,k_iand)
	when j_ior           then do_bin(p,a,b,k_ior)
	when j_ixor          then do_bin(p,a,b,k_ixor)
	when j_shl           then do_shl(p,a,b,k_shl,k_shlc)
	when j_shr           then do_shl(p,a,b,k_shr,k_shrc)
	when j_in            then do_bin(p,a,b,k_in)
!	when j_notin         then do_bin(p,a,b,k_)
!	when j_inrev         then do_bin(p,a,b)
	when j_min           then do_bin(p,a,b,k_min)
	when j_max           then do_bin(p,a,b,k_max)
	when j_addoffset     then do_bin(p,a,b,k_addoffset)
	when j_suboffset     then do_bin(p,a,b,k_suboffset)
	when j_subref        then do_bin(p,a,b,k_subref)
	when j_concat        then do_bin(p,a,b,k_concat)
	when j_append        then do_bin(p,a,b,k_append)
	when j_clamp         then do_clamp(p,a,b)
!	when j_index         then do_index(p,a,b)
	when j_index         then do_index(p,0)
	when j_slice         then do_slice(p,a,b)
	when j_makeslice     then do_makeslice(p,a,b)
	when j_dotindex      then do_dotindex(p,a,b)
	when j_dotslice      then do_dotslice(p,a,b)
	when j_anddotindex   then do_anddotindex(p,a,b)
	when j_anddotslice   then do_anddotslice(p,a,b)
!	when j_keyindex      then do_keyindex(p,a,b,0)
!	when j_dot           then do_dot(p,a,p^.offset,0)
	when j_dot           then do_dot(p,0)
!	when j_dotattr       then do_dotattr(p,a,b)
!	when j_atan2         then do_atan2(p,a,b)
!	when j_power         then do_power(p,a,b)
	when j_ptr           then do_ptr(p,a,b)
	when j_addrof        then do_addrof(p,a)
	when j_addroffirst   then do_addrof(p,a)
	when j_convert       then do_convert(p,a,b)
!	when j_convertref    then do_convertref(p,a,b)
	when j_autocast      then do_autocast(p,a,b)
	when j_typepun       then do_typepun(p,a,b)
	when j_typeconst     then do_typeconst(p)
	when j_operator      then do_operator(p,a,b)
	when j_upper         then do_upper(p,a,b)
	when j_neg           then do_unary(p,a,k_neg)
	when j_abs           then do_unary(p,a,k_abs)
	when j_inot          then do_unary(p,a,k_inot)
	when j_sqrt          then
		evalunit(a)
		genpc(k_sqrt)
		setpclmode_u(a)

	when j_sqr           then do_sqr(p,a,b)
	when j_maths         then do_maths(p,a,b)
!	when j_sign          then do_sign(p,a,b)
!	when j_sin           then do_maths(p,a,sysfn_sin,k_sin)
!	when j_cos           then do_maths(p,a,sysfn_cos,k_cos)
!	when j_tan           then do_maths(p,a,sysfn_tan,k_tan)
!	when j_asin          then do_maths(p,a,sysfn_asin,k_asin)
!	when j_acos          then do_maths(p,a,sysfn_acos,k_acos)
!	when j_atan          then do_maths(p,a,sysfn_atan,k_atan)
!	when j_ln            then do_maths(p,a,sysfn_ln,k_ln)
!!	when j_lg            then do_maths(p,a,sysfn_lg,k_log)
!	when j_log           then do_maths(p,a,sysfn_log,k_log)
!	when j_exp           then do_maths(p,a,sysfn_exp,k_exp)
!	when j_round         then do_maths(p,a,sysfn_round,k_round)
!	when j_floor         then do_maths(p,a,sysfn_floor,k_floor)
!	when j_ceil          then do_maths(p,a,sysfn_ceil,k_ceil)
!	when j_fract         then do_maths(p,a,sysfn_fract,k_fract)
!	when j_fmod          then do_fmod(p,a,b)
	when j_lwb           then do_unary(p,a,k_lwb)
	when j_upb           then do_unary(p,a,k_upb)
	when j_len           then do_unary(p,a,k_len)
	when j_lenstr        then do_unary(p,a,k_lenstr)
	when j_bounds        then do_unary(p,a,k_bounds)
	when j_asc           then do_unary(p,a,k_asc)
	when j_chr           then do_unary(p,a,k_chr)

	when j_bitwidth      then do_bitwidth(p,a)
	when j_bytesize      then do_bytesize(p,a)
	when j_typeof        then do_typeof(p,a,b)
	when j_typestr       then do_typestr(p,a,b)
	when j_sliceptr      then do_sliceptr(p,a)
	when j_minvalue      then do_minvalue(p,a,b)
	when j_maxvalue      then do_maxvalue(p,a,b)

	when j_incr,j_decr	 then do_incr(p,a)

	when j_preincrx      then do_incrx(p,a,k_preincrtox)
	when j_predecrx      then do_incrx(p,a,k_predecrtox)
	when j_postincrx     then do_incrx(p,a,k_postincrtox)
	when j_postdecrx     then do_incrx(p,a,k_postdecrtox)

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

	when j_andlto        then do_binto(p,a,b,k_andlto)
	when j_orlto         then do_binto(p,a,b,k_orlto)
	when j_appendto      then do_binto(p,a,b,k_appendto)
	when j_concatto      then do_binto(p,a,b,k_concatto)

	when j_negto         then do_unaryto(p,a,k_negto)
	when j_absto         then do_unaryto(p,a,k_absto)
	when j_inotto        then do_unaryto(p,a,k_inotto)
	when j_notlto        then do_unaryto(p,a,k_notlto)

!	when j_head, j_tail, j_init, j_last, j_flexptr, j_stringz,j_reverse,
!		 j_dupl then
!!		doflexop(p,a,jtagnames[p^.tag]+2)
!		do_flexunartop(p,a,jtagnames[p^.tag]+2)
!
!	when j_take, j_drop, j_left, j_right,j_convlc, j_convuc then
!		do_flexbinop(p,a,b,0,jtagnames[p^.tag]+2)

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
		gerror_s("UNSUPPORTED TAG: #",JTAGNAMES[P^.TAG])
	endswitch

!CPL "DONE"

	if ttisreal[p^.mode] then
		makefloatopnds()
	fi

	if p^.popflag then
!		if ttisflexvar[p^.mode] then
!			callflexhandler(p^.mode,"free",1)
!		elsif p^.mode=tmult then

		if ttbasetype[p.mode]=ttuple then
GERROR("MULTRET2")
!			if p^.tag=j_callfn and p^.a^.tag=j_name then
!				d:=p^.a^.def
!				for i to d^.nretvalues do
!					genpc(k_free)
!					setpclcat_t(d^.modelist[i])
!					if ttisreal[d^.modelist[i]] then
!						makefloatopnds()
!					fi
!				od
!			else
!				gerror("Can't free mult/ret values")
!			fi
!
		else
			genpc(k_free)
			setpclcat_t(p^.mode)
			if ttisreal[p^.mode] then
				makefloatopnds()
			fi
		fi
		p^.popflag:=0
	fi
!TO LEVEL DO PRINT "   " OD
!CPL "DONE2",JTAGNAMES[P.TAG]
!--LEVEL
end

proc evalref(unit p)=
	unit a,b,c
	a:=p^.a
	b:=p^.b
	c:=p^.c

	switch p^.tag
	when j_name then
		genpc(k_pushaddr,genmem_d(p^.def))

	when j_index then
		do_index(p,1)

!	when j_keyindex then
!		do_keyindex(p,a,b,1)

	when j_slice then
		do_slice(p,a,b,1)

	when j_dot then
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
pclopndrec lx,mx
unit q,r,s
int lab2

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
!	if ttisvar[p.mode] then
!!		genpc(k_pushretslot)
!!		setpclcat_t(ti64)
!		evalunit(p)
!		genpc(k_istruel)
!		setpclcat_u(p)
!!		callflexhandler(p^.mode,"istrue",1)
!		genpc((opc=kjumpt|k_jumptrue|k_jumpfalse),genlabel(lab))
!		setpclmode_t(ti64)
!	else
		evalunit(p)

		genpc((opc=kjumpt|k_jumptrue|k_jumpfalse),genlabel(lab))
		setpclmode_u(p)
!	fi
endswitch
end

proc gcomparejump(int jumpopc,unit p,lhs,rhs,int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int cond

	cond:=p^.tag				!eqop,neop, etc

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	evalunit(lhs)

	evalunit(rhs)
	genpc_condlab(k_jumpcc,cond,lab)
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
int i

i:=loopindex-(n-1)		!point to entry
if i<1 or i>loopindex then gerror("Bad loop index") fi
return loopstack[i,k]
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #",mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] then
		if ttsize[mode]<16 then
			genpc(k_pushint,genint(p.value))
!			setpclmode_t((ttisint[mode]|ti64|tu64))
		else
			genpc(k_pushint128,genint128(&p.value128))
!			setpclmode_t((ttisint[mode]|ti128|tu128))
!			gerror("PUSHINT128")
		fi
	elsif ttisreal[mode] then
		genpc(k_pushreal,genreal(p.xvalue))
		setpclmode_t(tr64)

	elsif ttisref[mode] then
		if p.isastring then
			genpc(k_pushstr,genstrimm(p.svalue,p.length))
		else
			genpc(k_pushint, genint(p.value))
		fi
	else
		gerror("do_const")
	fi

!	case p^.mode
!	when ti8,ti16,ti32 then
!		setpclmode_t(ti64)
!	when tu8,tu16,tu32 then
!		setpclmode_t(tu64)
!	else
!		if p^.isastring then
!			setpclmode_t(trefchar)
!		else
!			setpclmode_u(p)
!		fi
!	esac
end

proc do_null(unit p,a,b) =
	unimpl("do_null")
end

proc do_name(unit p) =
	ref strec d

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
!CPL "SETPCLCAT FOR NAME",TTSIZE[P.MODE],STRMODE(TTCAT[P.MODE]),
!	STRMODE(P.MODE),
!STRMODE(TTBASETYPE[P.MODE]),
!STRMODE(STDTYPECAT[TTBASETYPE[P.MODE]])

		setpclcat_u(p)
!CPL "DONE",STRMODE(PCCODEX.CATMODE)

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
CPL "DECIMAL",P.SVALUE,P.LENGTH
!	unimpl("do_decimal")
	genpc(k_makedec,genstrimm(p.svalue,p.length))

end

proc do_callproc(unit p,a,b,int fncall) =
[maxparams]unit params
int nparams,m,simplefunc
ref strec d,e
unit q

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
		if ttbasetype[p.mode]=ttuple then
GERROR("MULTRET1")

!			for i:=d^.nretvalues downto 1 do
!				genpc(k_pushretslot)
!				m:=d^.modelist[i]
!				setpclcat_t(m)
!				if ttisreal[m] then
!					makefloatopnds()
!				fi
!				pccodex^.mode:=m
!			od
		elsif simplefunc then
!		if simplefunc then
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

!		if isshortconst(q) then
!			genpc(k_dpushconst,genconst_u(q))
!			setpclcat_u(q)
!		elsif isshortmem(q) then
		if isshortmem(q) then
			genpc(k_dpushmem,genmem_u(q))
			setpclcat_u(q)
		else
			evalunit(q)
		fi
	od
	genpc(k_stackargs)

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
	int offset

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
	unit c
	ref strec d
	int offset

!CPL "ASSIGN1"
	if tttypecat[a.mode]=tc_blk and fstore=0 then
!CPL "POSSIBLE BLOCK",TTSIZE[A.MODE]
!		if ttcat[a.mode]<>twide then
!		if ttbasetype[a.mode]<>trecord or ttsize[a.mode] not in [16,8,4,2,1] then
!CPL "DOBLOCK"
			do_assignblock(p,a,b)
			return
!		fi
	fi

!	if ttisvar[a^.mode] then
!		do_assignvariant(p,a,b,fstore)
!		return
!	fi

!CPL "ASSIGN4"

	case a^.tag
	when j_makelist then
		if fstore then gerror("multassign/store?") fi
		do_multassign(a,b)
		return
	when j_index then
		do_popindex(p,a.a, a.b, b, (fstore|k_storeindex|k_popindex))
		return
	when j_slice then
		do_popslice(p,a.a, a.b, b, (fstore|k_storeslice|k_popslice))
		return
!	when j_keyindex then
!		do_popkeyindex(p,a.a, a.b, b, (fstore|k_storekeyindex|k_popkeyindex))
!		return
	when j_dot then
		do_popdot(p,a.a,b, a.offset,(fstore|k_storedot|k_popdot))
		return
	esac

	evalunit(b)

	switch a^.tag
	when j_name then
		genpc((fstore|k_storemem|k_popmem),genmem_u(a))
!	when j_slice then
!CPL "ASSIGN/SLICE"
!		evalref(a)
!		genpc((fstore|k_storeptr|k_popptr),genint(0))
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
		setpclcat_u(a)
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
		setpclcat_u(a)
		return
	else
		cpl jtagnames[a^.tag]
		gerror("Can't assign")
	end switch

	setpclcat_u(a)

	if ttbasetype[b.mode]=ttuple then
GERROR("MULTRET3")
!		if fstore then gerror("chained assign not allowed") fi
!!CPL "NORMAL ASSIGN: RHS IS MULT"
!		if b^.tag<>j_callfn or b^.a^.tag<>j_name then
!			gerror("assign/mult/call error")
!		fi
!		d:=b^.a^.def
!!CPL "RET VALUES",D^.NRETVALUES
!		for i:=2 to d^.nretvalues do
!			genpc(k_free)
!			setpclcat_t(d^.modelist[i])
!			if ttisreal[d^.modelist[i]] then
!				makefloatopnds()
!			fi
!		od
	fi

end

proc do_shallowcopy(unit p,a,b) =
	unimpl("do_shallowcopy")
end

!proc do_deepcopy(unit p,a,b) =
!	unimpl("do_deepcopy")
!end

proc do_to(unit p,a,b) =
	unit avar
	pclopndrec avaropnd
	int lab_a,lab_b,lab_c,lab_d,count

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
		genpc(k_pushint,zero_opnd)
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

	genpc(k_decrtomem,&avaropnd)
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
	int lab1,lab2,ismult

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
	int labend,i,lab2,ismult
	unit pcond

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
	unit pfrom, pto, pstep, pelse, px, plimit
	pclopndrec indexopnd,autoopnd
	int lab_a,lab_b,lab_c,lab_d,lab_e
	int a,b

	if ivar^.tag<>j_name then
		gerror("complex (non-i64) for-loop var?")
	fi

	pfrom:=ivar^.nextunit
	pto:=pfrom^.nextunit

	if pto^.tag=j_ptr then
		px:=pto^.a
		ref strec d
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
!			pccodex^.b:=genconst_u(pfrom)^
			pccodex^.b:=genint(pfrom^.value)^

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
!		if pstep^.tag=j_const then
			genpc(k_pushmem,&indexopnd)
			setpclcat_u(ivar)
			evalunit(pstep)
!			genpc((down|k_sub|k_add),gentos(ti64))
			genpc((down|k_sub|k_add))
			setpclmode_t(ti64)
			genpc(k_popmem,&indexopnd)
			setpclcat_u(ivar)
!		else
!			gerror("CAN'T DO FOR/BY/VAR")
!!			rhs:=loadexpr(pstep,r0)
!!			genmc((down|m_sub|m_add),genmem_u(ivar),rhs)
!		fi
	else
		genpc((down|k_decrtomem|k_incrtomem),&indexopnd)
		setpclcat_t(ti64)
	fi

	genpc(k_pushmem,&indexopnd)
	setpclcat_u(ivar)

	if isshortconst(plimit) then
		genpc_condlab(k_jumpccimm,(down|j_ge|j_le),lab_b)
		pccodex^.b:=genint(plimit.value)^
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

proc do_while(unit p,pcond,pbody) =
	int lab_b,lab_c,lab_d

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
	int lab_ab, lab_c, lab_d

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
	ref strec d
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

!proc do_gotoblock(unit p,a,b) =
!	unimpl("do_gotoblock")
!end

proc do_labeldef(unit p) =
	ref strec d
	d:=p^.def
	if d^.index=0 then
		d^.index:=++labelno
	fi
!	genpcstr(k_userlabel,getfullname(d))
	gencomment(d.name)
	genpc(k_label,genlabel(d^.index))
end

proc do_exit(unit p,int k) =
	int n,index

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
	int lab_abc,lab_d

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
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases

	int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, ismult,labelse
	unit w,wt

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
	int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, ismult
	unit w,wt

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
	int minlab,maxlab,n,iscomplex,i
	int lab_a,lab_b,lab_d, labjump, elselab, labstmt,ax,bx,ismult
	ref pclopndrec ixopnd
	[0..maxlabels]ref pclrec labels
	unit w,wt

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
				gerror_s("Switch when2: not const: #",strexpr(w)^.strptr)
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
	[maxlabels]ref pclrec labels
	int labend,labjump,n,i,elselab,labstmt,ismult
	unit q

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
	unit q,r
	int m,widenop, fn

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
!		genpc(k_stackargs)
		do_syscallproc(sysfn_print_setfmt,1)
		q:=p^.c
	esac

	while q do
		case q^.tag
		when j_fmtitem then
			evalunit(q^.b)
			genpc(k_stackargs)
			r:=q^.a
			m:=r^.mode
		when j_nogap then
			do_syscallproc(sysfn_print_nogap,0)
			q:=q^.nextunit
			next
		else
			genpc(k_pushint,zero_opnd)
!		setpclcat_t(ti64)
			r:=q
			m:=q^.mode
		esac

		widenop:=0
		switch ttbasetype[m]
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
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sysfn_print_str
			else
				fn:=sysfn_print_ptr
			fi
		when tarray then
			GERROR("PRINTARRAY")
!		do_printarray(x,m)
			q:=q^.nextunit
		when trecord then
			GERROR("PRINTRECORD")
		when tslice then
			if tttarget[m]=tc8 then
				fn:=sysfn_print_strsl
			else
				gerror("PRINTSLICE")
			fi

		when tc8,tc16,tc64 then
			fn:=sysfn_print_c8

!		when tvar then
!			fn:=sysfn_print_var

		else
			gerror_s("PRINT/T=#",strmode(m))
		end switch

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
!		genpc(k_stackargs)
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
	int m

	if a then			!format
		evalunit(a)
	else
		genpc(k_pushint, zero_opnd)
	fi

	m:=p.mode

	if ttisinteger[m] then
		do_syscallproc(sysfn_read_i64,1)
	elsif ttisreal[m] then
		do_syscallproc(sysfn_read_r64,1)
	elsif m=trefchar then
		do_syscallproc(sysfn_read_str,1)
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
!	ref opndrec lhs,ax

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
		genpc(k_pushint,zero_opnd)
!SETPCLCAT_T(TI64)
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

!proc do_lambda(unit p,a,b) =
!	unimpl("do_lambda")
!end

proc do_andl(unit p,a,b) =
	int labfalse, labend

	genpc(k_startmult)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf,a,labfalse)
	genjumpcond(kjumpf,b,labfalse)

	genpc(k_pushint,genint(1))
	genpc(k_resetmult)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpc(k_pushint,genint(0))
	genpc(k_endmult)

	definefwdlabel(labend)
end

proc do_orl(unit p,a,b) =
	int labtrue, labfalse, labend

	genpc(k_startmult)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt,a,labtrue)
	genjumpcond(kjumpf,b,labfalse)

	definefwdlabel(labtrue)
	genpc(k_pushint,genint(1))
	genpc(k_resetmult)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpc(k_pushint,genint(0))
	genpc(k_endmult)

	definefwdlabel(labend)
end

proc do_xorl(unit p,a,b) =
	unimpl("do_xorl")
end

proc do_notl(unit p,a) =
	evalunit(a)
!	if not ttisvar[a.mode] 	and not islogical(a) then
	if not islogical(a) then
		genpc(k_istruel)
		setpclcat_u(a)
	fi
	genpc(k_notl)
	setpclcat_u(a)
end

proc do_istruel(unit p,a) =
	evalunit(a)
	if not islogical(a) then
		genpc(k_istruel)
	fi
	setpclcat_u(a)
end

proc do_makelist(unit p,a,b) =
	int lower
	unit a0
	a0:=a

!	while a do			!will be in reverse order
!		evalunit(a)
!		a:=a^.nextunit
!	od

	if a then
		evalrest(a)
	fi

	if ttbasetype[p^.mode]=tslice then
		genpc(k_makeslice)
	else
GERROR("MAKELIST")
!		if p.makearray then
!CPL "ARRAY",=A0,STRMODE(A0.MODE)
!FI
!		lower:=1
!		if p.b then
!			if p.b.tag=j_const then
!				lower:=p.b.value
!			else
!				gerror("lwb not const")
!			fi
!		fi
!		genpc(k_makelist,genint(p.length),genint(lower))
!		setpclcat_t(tvar)
!		if p.makearray and a0 then
!			pccodex.mode:=a0.mode
!		fi

	fi
end

proc evalrest(unit a)=
!a is a list; evaluate trailing units first then this one
	if a.nextunit then
		evalrest(a.nextunit)
	fi
	evalunit(a)
end

proc do_makerange(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	genpc(k_makerange)
end

proc do_makeset(unit p,a,b) =
GERROR("MAKESET")
!	while a do			!will be in reverse order
!		evalunit(a)
!		a:=a^.nextunit
!	od
!	genpc(k_makeset,genint(p.length))
!	setpclcat_t(tvar)
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

proc do_same(unit p,a,b) =
	unimpl("do_same")
end

proc do_muldiv(unit p,a,b,int opc) =
!used for mul, idiv, irem; only mul can be float
	int n

!	if ttisvar[a^.mode] then
!		if opc=k_mul and ttisinteger[b^.mode] then
!			opc:=k_muli
!		fi
!	fi

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
	when k_addoffset, k_suboffset, k_in then
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
	ref pclopndrec ix
	ref pclrec px
	unit q,a,b
	int abase

	a:=p^.a
	b:=p^.b

	abase:=ttbasetype[a.mode]

	if abase in [tmanarray,tslice] and not doref then
		evalunit(a)
		evalunit(b)
		genpc(k_index)

	elsif a.tag=j_name and not doref then
		evalunit(b)
		genpc(k_indexmem, genmem_u(a))

	else
		evalref(a)
		evalunit(b)
		genpc((doref|k_indexref|k_index))
	fi

	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode		!is void anyway?

	pccodex.catmode:=abase
	pccodex^.mode:=a.mode
end

proc do_slice(unit p,a,b, int doref=0) =
	int amode

!	if doref then
!		gerror("slice/lvalue?")
!	fi
	amode:=a.mode

	if b=nil then
		genpc(k_pushint,genint(ttlower[a.mode]))
!		setpclcat_t(ti64)
		genpc(k_pushint,genint(ttlength[a.mode]+ttlower[a.mode]-1))
!		setpclcat_t(ti64)
	else
		evalunit(b.a)
		evalunit(b.b)
	fi

	if ttbasetype[a.mode] in [tmanarray,tslice,tref] and not doref then
		evalunit(a)

	else
		evalref(a)
	fi

	genpc(k_slice)
	setpclmode_u(a)

	pccodex.catmode:=ttbasetype[a.mode]
	pccodex^.mode:=a.mode
end

!proc do_keyindex(unit p,a,b, int doref=0) =
!
!	if not doref then
!		evalunit(a)
!		evalunit(b)
!		genpc(k_keyindex)
!
!	else
!		evalref(a)
!		evalunit(b)
!		genpc((doref|k_keyindexref|k_keyindex))
!	fi
!
!	setpclcat_t(tvar)
!end
!
proc do_makeslice(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	genpc(k_makeslice)
!	setpclmode_u(a)
end

proc do_dotindex(unit p,a,b) =
	int bitno

	evalunit(a)
	evalunit(b)
	genpc(k_dotindex)
!	if ttisvar[a.mode] then
!		setpclmode_t(tvar)
!	else
		setpclmode_t(ti64)
!	fi

!	fi
end

proc do_dotslice(unit p,a,b) =
	unit x,y
	int bitno

!	if ttisflexvar[a^.mode] then
!		do_slice(p,a,b)
!		return
!	fi
!CPL "DOTSLICE"

	evalunit(a)

	if b^.tag<>j_makerange then
PRINTUNIT(B)
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
	int bitno
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

proc do_dot(unit p, int doref) =
	unit a,pname
	int offset
	a:=p^.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		if offset<>-1 then
			offset+:=p.offset
			a:=pname
		fi
	else
		offset:=p^.offset
	fi

	evalref(a)

dorest::

	genpc((doref|k_dotref|k_dot),genint(offset))

	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode
	pccodex^.mode2:=p^.mode

	pccodex^.catmode:=trecord
	pccodex^.mode:=a^.mode
end

function checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions,
!or -1 when offsets cannot be combined
	int offset

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

!proc do_dotattr(unit p,a,b) =
!	unimpl("do_dotattr")
!end

proc do_atan2(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	genpc(k_atan2)
	setpclcat_u(p)
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
	int opc

if p^.popflag then
	evalunit(a)
	return
fi

	case p^.tag
	when j_makelist, j_makeset then
	else
		case p^.opcode
		when c_ichartostring then
			gerror("ichar to string")
!			genpc(k_pushretslot)
!			setpclcat_t(tscalar)
!
!			genpc(k_pushint,genint((a^.tag=j_const|a^.slength|-1)))
!			evalunit(a)
!
!			callflexhandler(tflexstring,"make",2)
			return
		esac

		if p^.opcode in [c_uwiden, c_iwiden] and \
			a^.tag in [j_index,j_dot,j_ptr] and ttsize[p^.mode]=8 then
			evalunit(a)
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
!		when c_vartoany	then opc:=k_unbox
!		when c_anytovar	then
!!CPL "ANY TO VARIANT"
!			if a.tag=j_const and a.mode in [ti64,tc64] then
!				genpc(k_makeint,genint(a.value))
!				return
!			elsif a.tag=j_const and a.mode=tr64 then
!				genpc(k_makereal,genreal(a.xvalue))
!				return
!			elsif a.tag=j_const and a.mode=trefchar and a.isastring then
!				genpc(k_makestr,genstrimm(a.svalue,a.length))
!				return
!			elsif a.tag=j_makerange and a.mode=trange64 then
!!TXERROR("RANGE TO VAR")
!				evalunit(a.a)
!				evalunit(a.b)
!!				deleteunit(p,a)
!				genpc(k_makerange)
!!				genpc(k_makestr,genstrimm(a.svalue,a.length))
!				return
!			else
!				evalunit(a)
!				genpc(k_box)
!				pccodex^.mode:=a.mode
!				pccodex^.mode2:=tvar
!				return
!			fi
		when c_none then
			GERROR("BLOCKPCL/C_NONE")
		else
			cpl convnames[p^.opcode]
			gerror("No PCL convert op")
		endswitch

!CPL "CONV",=STRMODE(A.MODE),=STRMODE(P.MODE)

		evalunit(a)
!		genpc_opt(k_convert,p^.mode)
		genpc(opc)
!		setpclmode_u(p)
		pccodex^.mode:=a.mode
		pccodex^.mode2:=p.mode
!		pccodex^.mode2:=ttbasetype[a^.mode]
		return
	esac
	unimpl("CONVERT/MAKELIST/CONSTR")
end

!proc do_convertref(unit p,a,b) =
!	unimpl("do_convertref")
!end

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
	genpc(k_pushint,genint(p^.value))
!	setpclcat_t(ti64)
end

proc do_operator(unit p,a,b) =
	unimpl("do_operator")
end

proc do_upper(unit p,a,b) =
	unimpl("do_upper")
end

proc do_unary(unit p,a,int opc) =
	evalunit(a)
	genpc(opc)

	case opc
	when k_len, k_upb, k_lwb, k_lenstr,k_bounds,k_asc then
		setpclmode_u(a)
		return
!	when j_lwb, j_bounds then
!		gerror("lwb/bounds")
	esac

	setpclmode_u(p)
end

proc do_maths(unit p,a,b) =
GERROR("NEED NEW DO_MATHS")
	evalunit(a)

!	if ttisvar[a.mode] then
!		genpc(pclopc)
!		setpclcat_t(tvar)
!	else

!		do_syscallproc(sysfn,1)
!
!		genpc(k_pushretval)			!dummy op: put d0/x0 return value onto opnd stack
!		setpclcat_u(p)
!		if ttisreal[p^.mode] then
!			makefloatopnds()
!		fi
!		pccodex^.mode:=p^.mode
!!	fi
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
		genpc((p^.tag=j_incr|k_incrtomem|k_decrtomem), genmem_u(a))
	else
		evalref(a)
		genpc((p^.tag=j_incr|k_incrto|k_decrto))
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

!	if ttisflexvar[a^.mode] then
!		do_bintoflex(p,a,b,opc)
!		return
!	fi

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
	genpc(k_pushint,genint(p^.lineno iand 16777215))
!	setpclmode_t(ti64)
end

!proc do_cvstrlineno(unit p,a,b) =
!	unimpl("do_cvstrlineno")
!end
!
proc do_cvmodulename(unit p,a,b) =
	genpc(k_pushstr,genstrimm(moduletable[p^.moduleno].name))
!	setpclmode_t(trefchar)
end

proc do_cvfilename(unit p,a,b) =
!	genpc(k_pushconst,genstrimm(sourcefilenames[p^.lineno>>24]))
	genpc(k_pushstr,genstrimm(sourcefilenames[p^.lineno>>24]))
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
unit q
int nargs,nparams,retmode,nbytes,retsize,i,n,allsimple
int lab1,lab2,floatmap,mask
[maxparams]ref strec paramlist
[maxparams]unit params
[maxparams]byte widenfloat
![maxparams]byte isfloat
!int isvariadic
[256]char str

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
	genpc(k_stackargs)

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
	unit q,wt,w
	int destlab

	if casedepth=0 then
		gerror("recase outside case stmt")
	fi
	q:=casestmt[casedepth]

	destlab:=0

	wt:=q^.b
	while wt do
		w:=wt^.a
		while w do
			if w^.tag=j_const and ttisinteger[w.mode] and w^.value=a^.value then
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

!proc do_assignvariant(unit p,a,b, int fstore) =
!!fstore=1 when result is needed
!
!	if p^.tag in [j_deepcopy, j_deepcopyx] then
!		if ttisvar[a^.mode] then
!			genpc(k_pushretslot)
!			setpclcat_t(tscalar)
!			evalunit(b)
!			genpc(k_dupl)
!		else
!			gerror("::= not appropriate")
!		fi
!	else
!		evalunit(b)
!	fi
!
!	switch a^.tag
!	when j_name then
!		genpc((fstore|k_storemem|k_popmem),genmem_u(a))
!		setpclcat_u(a)
!		return
!	when j_index,j_dotindex then
!		if fstore then
!			gerror("str[]:=x.store?")
!		fi
!		evalunit(a^.b)
!		evalunit(a^.a)
!!		genpc((a.tag=j_index|k_index)
!
!		callflexhandler(a^.a^.mode,(a^.tag=j_index|"indexto"|"indextoc"),3)	
!!	when j_ptr then
!!		evalunit(a^.a)
!!		genpc((fstore|k_storeptr|k_popptr))
!!	when j_if, j_longif, j_case, j_switch, j_select then
!!		evalref(a)
!!		genpc((fstore|k_storeptr|k_popptr))
!!!		gerror("assign to if/select/etc")
!!	when j_dotindex then
!!		evalref(a^.a)
!!		evalunit(a^.b)
!!		if fstore then
!!			gerror("storedotix?")
!!		else
!!			genpc(k_popdotindex)
!!		fi
!	else
!		cpl jtagnames[a^.tag]
!		gerror("Can't assign to flex expr")
!	end switch
!
!!	setpclcat_u(a)
!end

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
	unit p
	int nlhs,nrhs
	ref strec d

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

GERROR("MULTASSIGN")
!	for i:=nlhs+1 to nrhs do
!
!		genpc(k_free)
!		setpclcat_t(d^.modelist[i])
!		if ttisreal[d^.modelist[i]] then
!			makefloatopnds()
!		fi
!	od

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
	int abase
	ref pclrec px
	unit q

	evalunit(c)

	abase:=ttbasetype[a.mode]
!	if abase in [tvar,tslice] then
	if abase in [tmanarray,tslice] then
		evalunit(a)
	else
		evalref(a)
	fi
	evalunit(b)
	genpc(opc)


	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode		!is void anyway?

	pccodex.catmode:=abase
	pccodex^.mode:=a.mode
end

proc do_popslice(unit p,a,b,c, int opc)=
!a[b.a..b.b]:=c
	int abase
	ref pclrec px
	unit q

	evalunit(c)
	if b=nil then
		genpc(k_pushint,genint(ttlower[a.mode]))
!		setpclcat_t(ti64)
		genpc(k_pushint,genint(ttlength[a.mode]+ttlower[a.mode]-1))
!		setpclcat_t(ti64)
	else
		evalunit(b.a)
		evalunit(b.b)
	fi

	abase:=ttbasetype[a.mode]
	if abase in [tmanarray,tslice] then
		evalunit(a)
	else
		evalref(a)
	fi
!	evalunit(b)
	genpc(opc)


	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode		!is void anyway?

	pccodex.catmode:=abase
	pccodex^.mode:=a.mode
end

proc do_popkeyindex(unit p,a,b,c, int opc)=
!a[b]:=c
GERROR("POPKEYINDEX")
!	int abase
!	ref pclrec px
!	unit q
!
!	evalunit(c)
!
!	evalunit(a)
!	evalunit(b)
!	genpc(opc)
!
!
!	setpclcat_u(p)
!!	pccodex^.catmode2:=pccodex^.catmode
!!	pccodex^.mode2:=pccodex^.mode		!is void anyway?
!
!	setpclcat_t(tvar)
end

proc do_popdot(unit p,a,c, int offset,opc)=
!a.offset:=c
	ref pclrec px
	unit pname
	int newoffset

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

	evalref(a)

!	case a^.tag
!	when j_name then
!		genpc(k_pushaddr,genmem_u(a))
!
!	when j_ptr then
!		evalunit(a.a)
!
!	when j_dot then
!!		do_dot(a, a.a,a^.offset,1)
!		do_dot(a, 1)
!
!	when j_index then
!		do_index(a,1)
!
!	else
!		printunit(a)
!		GERROR("Can't popdot this record")
!	esac

dorest::
	genpc(opc,genint(offset))

!set element mode
	setpclcat_u(p)
	pccodex^.catmode2:=pccodex^.catmode
	pccodex^.mode2:=pccodex^.mode		!is void anyway?

	pccodex^.catmode:=trecord
	pccodex^.mode:=a^.mode
end
=== end ===
