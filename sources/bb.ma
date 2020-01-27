mafile 25
  1 bb.m               18235     1005
  2 clibnew.m           3397    19264
  3 mlib.m             26695    22682
  4 msysnew.m          46919    49401
  5 oswindows.m        12536    96346
  6 bb_lex.m           40973   108905
  7 bb_decls.m          9896   149903
  8 bb_tables.m        42458   159825
  9 bb_support.m       13629   202310
 10 bb_lib.m           41400   215963
 11 bb_diags.m         13937   257389
 12 bb_mcldecls.m      13305   271355
 13 bb_parse.m         89362   284686
 14 mvar.m             11777   374070
 15 var_decls.m         6644   385874
 16 var_tables.m        3540   392546
 17 var_numbers.m       6385   396115
 18 var_objects.m      15562   402529
 19 var_support.m       3821   418120
 20 var_strings.m       9023   421970
 21 var_lists.m         8799   431020
 22 var_decimals.m      4121   439849
 23 mbignum.m          30191   443995
 24 var_arrays.m        6547   474214
 25 var_print.m         3114   480788
=== bb.m 1/25 ===
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

tabledata() []ichar optionnames=
	(exe_sw,		"exe"),			!generate .exe (default)
	(obj_sw,		"obj"),			!generate .obj

	(opt_sw,		"opt"),			!optimise

	(compile_sw,	"c"),			!compile only to .asm
	(link_sw,		"link"),		!compile and generate .exe or .obj (default)
	(run_sw,		"run"),			!for .exe target, also run the result

	(load_sw,		"load"),
	(parse_sw,		"parse"),
	(ba_sw,			"ba"),
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

CPL =STREC.BYTES

!fshowast1:=1
!fshowst:=1
!fshowtypes:=1
!fshowoverloads:=1

initdata()

getinputoptions()

initsearchdirs()
remove(logfile)

t:=clock()
loadmainmodule(inputfiles[1])

!CPL "LOADED",INPUTFILES[1]
!
!CPL =NMODULES
!FOR I TO NMODULES DO
!	CPL I,,":",MODULETABLE[I].NAME,"FILENO:",moduletable[i].fileno
!OD
!CPL
!global proc addoverload(moduleno, opc, amode, bmode, rmode, pfunc)=

!addoverload(1,j_add,ti64,ti64,ti64,nil)

do_parse()
t:=clock()

CPL "PARSETIME=",T,=nalllines

!CPL "CALLCOUNTS:"
!SHOWCALLCOUNTS()

!CPL =NLOOKUPS
!CPL =NCLASHES:"s,"

if fshowast1 then showast("AST1") fi

showlogfile()

CPL
end

proc do_parse=
	for i:=2 to nmodules do
		parsemodule(i)
	od
	parsemodule(1)

!	fixusertypes()
end

proc showlogfile=
[256]char str
filehandle logdev

!CPL "SHOWLOGFILE1",=FSHOWAST1
if fshowpcl1 or fshowpcl2 or fshowast1 or fshowast2 or fshowast3 or\
	fshowst or fshowstflat or fshowtypes or fshowmcl1 or fshowss or
	fshowoverloads then
	logdev:=fopen(logfile,"w")

!LOGDEV:=NIL

!CPL "SHOWLOGFILE2"
!	if fshowss then addtolog("SS",logdev) fi
!	if fshowmcl1 then addtolog("MCL",logdev) fi
!	if fshowmcl1 then addtolog("MCL.ASM",logdev) fi
	if fshowmcl1 then addtolog(outfilesource,logdev) fi
!	if fshowpcl2 then addtolog("PCL2",logdev) fi
	if fshowpcl1 then addtolog("PCL",logdev) fi
	if fshowast3 then addtolog("AST3",logdev) fi
	if fshowast2 then addtolog("AST2",logdev) fi
	if fshowast1 then addtolog("AST1",logdev) fi

	if fshowst then	showsttree("SYMBOL TABLE",logdev) fi
!	if fshowst then	showsttree("SYMBOL TABLE",nil) fi

!	if fshowstflat then	showstflat("FLAT SYMBOL TABLE",logdev) fi
	if fshowtypes then printmodelist(logdev) fi
	if fshowoverloads then printoverloads(logdev) fi
!	if fshowtypes then printmodelist(nil) fi

	fclose(logdev)

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
!CPL =NUNITS:"s,",=UNITREC.BYTES

stop 0
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

!if cc_mode=0 then
!	cc_mode:=link_mode
!fi
!
if linkoption=nil then linkoption:="exe" fi

!if ninputfiles=0 and not fwritelibs then
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
!
!	if islinux and eqstring(linkoption,"exe") then
!		linkoption:=""
!	fi
!
	outfilebin:=pcm_copyheapstring(changeext(filename,linkoption))
!
!
!	if cc_mode=compile_mode then
!		if destfilename then outfilesource:=destfilename fi
!		outfile:=outfilesource
!	else
!		if destfilename then outfilebin:=destfilename fi
!		outfile:=outfilebin
!	fi
!!CPL =OUTFILE
!!CPL =OUTFILESOURCE
!CPL =OUTFILEBIN
else
	loaderror("Specify one lead module only")
fi
end

proc do_option(int sw, ichar value)=
int length

case sw
when compile_sw then
	cc_mode:=compile_mode

when link_sw then
	cc_mode:=link_mode

when run_sw then
	cc_mode:=run_mode

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

when out_sw then
	destfilename:=pcm_copyheapstring(value)

when unused_sw then
	fcheckunusedlocals:=1

when nosys_sw then
	fnobsys:=1

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

!when set_sw then
!	dosetoptionvar(value)

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
=== clibnew.m 2/25 ===
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
=== mlib.m 3/25 ===
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
=== msysnew.m 4/25 ===
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
=== oswindows.m 5/25 ===
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
=== bb_lex.m 6/25 ===
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
			if lxsptr^=c then		!repeated, assume embedded term char
				++lxsptr
				++length
			else			!was end of string
				exit
			fi
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

=== bb_decls.m 7/25 ===
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
	ref strec owner
	ref strec defa
	union
		ref strec defb
		ref strec def
	end
	ref int32 pmode
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

	ref unitrec code
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

	int32 lineno
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
			ref strec nulldef		!generic st entry
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
	int32 lineno			!source lineno associated with item; fileno is in top byte

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
global byte fdebugcompiler		!1 for debug compile, 0 (default) for production compile

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
global byte fshowpcl1
global byte fshowpcl2
global byte fshowmcl1
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte fshowoverloads
global byte foptimise
global byte fcheckunusedlocals=0

global byte dointlibs=1

global tabledata() []ichar ccmodenames =
	(compile_mode,	$),
	(link_mode,		$),
	(run_mode,		$),
end

global int cc_mode			!compile_mode/link_mode/run_mode

!passlevel used for compiler debug only
global int passlevel=6		!1=parse, 2=name, 3=type, 4=gen1, 5=gen2, 6=asm, 7=link

global ichar outfile					!one of the following two
global ichar outfilesource				!.asm or .c filename
global ichar outfilebin				!.exe or .obj filename
global ichar destfilename				!nil, or sets outfilebin

global ichar linkoption				!exe or obj

global ref strec extendtypelist

global [0:jtagnames.len]ref overloadrec overloadtable
=== bb_tables.m 8/25 ===
global tabledata() [0:]ichar stdtypenames, [0:]byte stdtypebits =
	(tvoid=0,		$,		0),

	(ti8,			$,		8),
	(ti16,			$,		16),
	(ti32,			$,		32),
	(ti64,			$,		64),
	(ti128,			$,		128),

	(tu1,			$,		1),
	(tu2,			$,		2),
	(tu4,			$,		4),

	(tu8,			$,		8),
	(tu16,			$,		16),
	(tu32,			$,		32),
	(tu64,			$,		64),
	(tu128,			$,		128),

	(tc8,			$,		8),
	(tc16,			$,		16),
	(tc64,			$,		64),

	(tr32,			$,		32),
	(tr64,			$,		64),

	(tref,			$,		64),

	(tenum,			$,		0),

	(tauto,			$,		0),
	(tany,			$,		0),
	(tproc,			$,		0),
	(tlabel,		$,		0),
	(ttype,			$,		64),
	(tbitfield,		$,		8),

	(trange,		$,		128),
	(tarray,		$,		0),
	(tsmallarray,	$,		0),
	(tbits,			$,		0),
	(tsmallbits,	$,		0),
	(trecord,		$,		0),
	(tsmallrecord,	$,		0),
	(ttaggedunion,	$,		0),
	(ttuple,		$,		0),
	(tmult,			$,		0),

	(trefbit,		$,		128),
	(tslice,		$,		128),
	(tslice2d,		$,		128),
	(tflex,			$,		128),

	(tstring,		$,		0),
	(tmanarray,		$,		0),
	(tmanbits,		$,		0),
	(tset,			$,		0),
	(tdict,			$,		0),
	(tdecimal,		$,		0),
	(tmanrecord,	$,		0),

	(tparam1,		$,		0),
	(tparam2,		$,		0),
	(tparam3,		$,		0),
	(tparam4,		$,		0),

	(tpending,		$,		0),

	(tlast,			$,		0)
end

global tabledata() [0:]ichar stdtypecats =
	(tscalar,		$),
	(tfloat,		$),
	(twide,			$),
	(tblock,		$),
	(tmanaged,		$),

	(tshortscalar,	$),
	(tshortfloat,	$),
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

!a,b,c are unitrec refs, which can be a single unit, or a linked-list chain
!(usually in forward order)
!	L means .a/b/c pointing to a unitlist; L can be nil for an empty list
!	u means .a/b/c pointing to a single unit
!	u/nil means can be nil

![a=u] means a is a unit/list, or is nil

	(j_none=0,		$,		0), ! For tagname lookups when tag is zero
	(j_const,		$,		1), ! value/etc=value, typeno=type code
	(j_null,		$,		1), ! Place holder unit: means 'param no present' when used where a param is expected
	(j_name,		$,		1), ! def=nameptr
	(j_block,		$,		0), ! a=L
	(j_stmtblock,	$,		0), ! a=L
	(j_decimal,		$,		1), ! svalue=str, slength
	(j_assem,		$,		0), ! svalue=str, slength
	(j_assemmacro,	$,		0), !
	(j_assemreg,	$,		0), !
	(j_assemxreg,	$,		0), !
	(j_assemmem,	$,		0), !

!Logical Operators

	(j_andl,		$,		1), ! a b	This group are for conditional expressions (no result)
	(j_andb,		$,		1), ! a b
	(j_orl,			$,		1), ! a b
	(j_orb,			$,		1), ! a b
	(j_xorl,		$,		1), ! a b
	(j_xorb,		$,		1), ! a b
	(j_notl,		$,		1), ! a
	(j_istruel,		$,		1), ! a

!Expressions and Operators

	(j_makelist,	$,		1), ! a=L, b=[u], length=N; element list/lower bound expr
	(j_makerange,	$,		1), ! a b
	(j_makeset,		$,		1), ! a=L, length=N
	(j_makedict,	$,		1), !
	(j_makeslice,	$,		1), !
	(j_exprlist,	$,		1), ! a=u...	List of expressions, as (a;b;c), rather than (a,b,c)
	(j_multexpr,	$,		1), !
	(j_returnmult,	$,		1), !

	(j_keyword,		$,		1), ! def=st entry
	(j_keyvalue,	$,		1), ! a b
	(j_assignx,		$,		1), ! a b
	(j_deepcopyx,	$,		1), ! a b
	(j_callfn,		$,		1), ! a b
!	(j_applyop,		$,		0), ! opcode b c
!	(j_applyopx,	$,		1), ! opcode b c
	(j_new,			$,		1), ! newmode=T, a=L, length=N
	(j_destroy,		$,		0), ! a=L, length=N
	(j_clear,		$,		0), !

!Binary Ops

	(j_eq,			$,		1), ! a b
	(j_ne,			$,		1), ! a b
	(j_lt,			$,		1), ! a b
	(j_le,			$,		1), ! a b
	(j_gt,			$,		1), ! a b
	(j_ge,			$,		1), ! a b

	(j_same,		$,		1), ! a b

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
	(j_shl,			$,		1), ! a b
	(j_shr,			$,		1), ! a b
	(j_in,			$,		1), ! a b
	(j_notin,		$,		1), ! a b
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

!	(j_insert,		$,		1), ! a b
!	(j_delete,		$,		1), ! a b

	(j_prepend,		$,		1), ! a b
	(j_flexptr,		$,		1), ! a b
	(j_stringz,		$,		1), ! a b
	(j_sliceptr,	$,		1), ! a b

	(j_index,		$,		1), ! a b		a[b]
	(j_slice,		$,		1), ! a b		a[b]
	(j_dot,			$,		1), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotindex,	$,		1), ! a b		a[b]
	(j_dotslice,	$,		1), ! a b		a[b]
	(j_anddotslice,	$,		1), ! a b		a[b]
	(j_anddotindex,	$,		1), ! a b		a[b]

	(j_power,		$,		1), ! a b	a**b				int/real

	(j_ptr,			$,		1), ! a		a^
	(j_addrof,		$,		1), ! a		&a
	(j_addroffirst,	$,		1), ! a		&a
	(j_convert,		$,		1), ! typeno=T a		T(a)			T
!	(j_convertref,	$,		1), ! typeno=T a		T(a)			T
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
	(j_cvnil,		$,		1), ! 
	(j_cvpi,		$,		1), ! 

	(j_whenthen,	$,		0), ! a=L b=u
	(j_elsif,		$,		0), ! opcode=condcode, a
	(j_fmtitem,		$,		1), ! a b  x/fmtstr
	(j_nogap,		$,		1), ! 

!Statements

	(j_callproc,	$,		0), ! a=fn b=L, length
	(j_return,		$,		0), ! a=x/nil
	(j_syscall,		$,		0), ! a=x or nil

	(j_assign,		$,		0), ! a b
	(j_deepcopy,	$,		0), ! a b
	(j_to,			$,		0), ! a=N, b=body, c=tempvar/nil, def=name
	(j_if,			$,		1), ! condcode a=then b=else
	(j_longif,		$,		1), ! a=(elsif ...) b=else		L is series of kelsif pairs
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


	(j_dummy,		$,		1)
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
	(ksyscallsym,		$,		0,	0,	0,	0,	0),		! $get_procname etc

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

!	("$get_nprocs",		ksyscallsym,		sysfn_get_nprocs),
!	("$get_procname",	ksyscallsym,		sysfn_get_procname),
!	("$get_procaddr",	ksyscallsym,		sysfn_get_procaddr),
!
!	("$get_nexports",	ksyscallsym,		sysfn_get_nexports),
!	("$get_procexport",	ksyscallsym,		sysfn_get_procexport),
!
!	("$nprocs",			ksyscallsym,		sysfn_nprocs),
!	("$nexports",		ksyscallsym,		sysfn_nexports),
!	("$procnames",		ksyscallsym,		sysfn_procnames),
!	("$procaddrs",		ksyscallsym,		sysfn_procaddrs),
!	("$procexports",	ksyscallsym,		sysfn_procexports),
!
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

=== bb_support.m 9/25 ===
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

println "On line",lx.lineno iand 16777215,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]

println
println "**** Syntax Error:",mess,"****"
stopcompiler(sourcefilepaths[lx.fileno],lx.lineno iand 16777215)
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
when 'A' then print "**** 'AX Code Gen Error: "
esac
println mess

os_getch()

stopcompiler(sourcefilepaths[fileno],lineno iand 16777215)
end

!global proc rxerror(ichar mess,unit p=nil)=
!error_gen('N',mess,p)
!end
!
global proc gerror(ichar mess,unit p=nil)=
error_gen('G',mess,p)
end

!global proc axerror(ichar mess)=
!CPL =ALINENO
!error_gen('A',mess)
!end
!
!global proc txerror(ichar mess,unit p=nil)=
!error_gen('T',mess,p)
!end
!
!global proc txerror_s(ichar mess,a,unit p=nil)=
![256]char str
!fprint @&.str,mess,a
!error_gen('T',&.str,p)
!end
!
!global proc txerror_ss(ichar mess,a,b)=
![256]char str
!fprint @&.str,mess,a,b
!error_gen('T',&.str)
!end
!
!global proc rxerror_s(ichar mess,a,unit p=nil)=
![256]char str
!fprint @&.str,mess,a
!error_gen('N',&.str,p)
!end
!
!global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
![256]char str
!
!fprint @&.str,mess,s
!error_gen('G',&.str,p)
!end

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

=== bb_lib.m 10/25 ===
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

p^.lineno:=lx.lineno
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
CPL "INSERTUNIT",STRMODE(MODE)
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
ttlineno[ntypes]:=lx.lineno

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

global function createstringmode(int t,length,typedefx)int=		!CREATESTRINGMODE
!create fixed-bound string mode
!length is max length of string (including any count or terminator)
!ts is tstring or tcstring
int k,m

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
	strcpy(dest,"*")
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
when tarray,tbits then
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

when trecord,ttaggedunion then
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

q:=createunit0(p^.tag)

q^.a:=duplunit(p^.a,lineno)
q^.b:=duplunit(p^.b,lineno)
q^.c:=duplunit(p^.c,lineno)
q^.lineno:=(lineno|lineno|p^.lineno)
q^.value:=p^.value			!copy main field of each union
q^.opcode:=p^.opcode
CPL "DUPLUNIT",STRMODE(P.MODE)
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

global function gettypecat_t(int m)int=
	GERROR("GETTYPECAT")
RETURN 0
!	return stdtypecat[ttbasetype[m]]
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
=== bb_diags.m 11/25 ===
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
		gs_str(d,":=")
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
return &.str
end

=== bb_mcldecls.m 12/25 ===
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

=== bb_parse.m 13/25 ===
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

!		for i:=1 to nmodules do
!			if eqstring(name, moduletable[i].name) then
!				stimport:=moduletable[i].stmodule
!				exit
!			fi
!		else
!			CPL lx.symptr^.name
!			serror("Import stmt out of position?")
!		od

		lex()
!		domappedalias(dimport,stimport)
!		if lx.symbol=namesym and eqstring(lx.symptr^.name,"as") then
!			readimportalias(dimport)
!		fi

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
return lx.lineno
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
p.newmode:=t
!storemode(5,currproc,t,&p^.newmode)
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
	getstrint(lx.lineno iand 16777215,&.str)

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
	p.newmode:=m
!	if m then storemode(5,currproc,m,&p^.newmode) fi
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
!function readtypespec(ref strec owner,int typedefx=0)int=			!READTYPESPEC
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

stproc^.paramlist:=paramlist

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
int line, kwd, lineno
unit pthen,pcond, plist,plistx, pelse, p, pelsif

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
int line
unit pcond, pthen, pelse, p
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
int line, kwd, opc, lineno,rangeused, nwhen
unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

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
	int line

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
int line,id
unit p, pcount, pbody

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
int line,id
unit pcond, pa, pb, pc, pbody, p

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
int line
unit pbody, pcond, p

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

	int line, opc, kwd
	unit pindex, plocal, pfrom, pto, pstep, prange, plist, passign
	unit pcond, pbody, pelse
	unit pto_temp, pstep_temp, ptemp, ptempx, plist_temp, prange_temp
	unit p

	ref strec d

	kwd:=lx.symbol				!for/forall/foreach

	line:=lx.lineno
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

	p^.lineno:=line
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
	else
		tttarget[m]:=t
	fi
else
	tttarget[m]:=t
fi

if t>=0 then
	ttisint[m]		:= ttisint[t]
	ttisword[m]		:= ttisword[t]
!	ttiswordchar[m]	:= ttiswordchar[t]
	ttisreal[m]		:= ttisreal[t]
	ttisinteger[m]	:= ttisinteger[t]
	ttisnumeric[m]	:= ttisinteger[t] ior ttisreal[t]
	ttisshortint[m]	:= ttisshortint[t]
!	ttisbit[m]		:= ttisbit[t]
	ttisref[m]		:= ttisref[t]
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
		stname.mode:=typedefx
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
int lineno,fflang

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
	int lineno,opc

	case lx.symbol
	when namesym then
		case nexttoken.symbol
		when semisym, commasym, rbracksym then
			p:=createname(lx.symptr)
			p.lineno:=lx.lineno
			lex()
			return p
		esac
	esac

	p:=readorterms()

	if (opc:=lx.symbol) in [assignsym,deepcopysym] then
		lineno:=lx.lineno
		lex()
		p:=createunit2((opc=assignsym|j_assign|j_deepcopy),p,readassignment())
		p.lineno:=lineno
	fi
	return p
end

function readorterms:unit p=
	int lineno

	p:=readandterms()

	while lx.symbol=orlsym do
		lineno:=lx.lineno
		lex()
		p:=createunit2(j_orl,p,readandterms())
		p.lineno:=lineno
	od

	return p
end

function readandterms:unit p=
	int lineno

	p:=readcmpterms()

	while lx.symbol=andlsym do
		lineno:=lx.lineno
		lex()
		p:=createunit2(j_andl,p,readcmpterms())
		p.lineno:=lineno
	od

	return p
end

function readcmpterms:unit p=
	int lineno,opc

	p:=readrangeterm()

	doswitch opc:=lx.symbol
	when eqsym, nesym, ltsym, lesym, gesym, gtsym, insym, notinsym then
		lineno:=lx.lineno
		lex()
		p:=createunit2(symboljtags[opc],p,readrangeterm())
		p.lineno:=lineno
	else
		exit
	end doswitch

	return p
end

function readrangeterm:unit p=
	int lineno,opc

	p:=readaddterms()

	if lx.symbol=rangesym then
		lineno:=lx.lineno
		lex()
		p:=createunit2(j_makerange,p,readaddterms())
		p.lineno:=lineno
	fi

	return p
end

function readaddterms:unit p=
	int lineno,opc

	p:=readmulterms()

	doswitch opc:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, andbsym, orbsym, xorbsym,
		concatsym, appendsym, prependsym, minsym, maxsym then
		lineno:=lx.lineno
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(symboljtotags[opc],p,readassignment())
			p.lineno:=lineno
			exit
		fi

		p:=createunit2(symboljtags[opc],p,readmulterms())
		p.lineno:=lineno
	else
		exit
	end doswitch

	return p
end

function readmulterms:unit p=
	int lineno,opc

	p:=readpowerterms()

	doswitch opc:=lx.symbol
	when mulsym, divsym, idivsym, iremsym, shlsym, shrsym then
		lineno:=lx.lineno
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(symboljtotags[opc],p,readassignment())
			p.lineno:=lineno
			exit
		fi

		p:=createunit2(symboljtags[opc],p,readpowerterms())
		p.lineno:=lineno
	else
		exit
	end doswitch

	return p
end

function readpowerterms:unit p=
	int lineno,opc

	p:=readterm2()

	while lx.symbol=powersym do
		lineno:=lx.lineno
		lex()
		p:=createunit2(j_power,p,readpowerterms())
		p.lineno:=lineno
	od

	return p
end

function readterm2:unit=
!	int oldinrp,lineno,opc
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,lineno,shift,t

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

	p^.lineno:=lineno

	return p
end

function readterm:unit=
unit p,q,r
ref char pbyte
word64 a
int oldipl,opc,oldinrp,lineno,shift,t

	lineno:=lx.lineno

	switch lx.symbol
	when namesym then
		if nexttoken.symbol=atsym then		!type-punning with user type
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

	p^.lineno:=lineno
	return p
end

function readxunit:unit=
	return readsunit()
end

function readsunit(int inwhile=0)unit=
int lineno,m,sym,opc
unit ulist,ulistx,p,q,r
ref strec stname

lineno:=lx.lineno
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
int  m, startline, closesym, mtagged
ref strec nameptr, sttype

lexchecksymbol(namesym)
nameptr:=lx.symptr

lex()
checkequals()

lex()

sttype:=getduplnameptr(owner,nameptr,typeid)
adddef(owner,sttype)
m:=createusertype(sttype)

mtagged:=createtaggedunionmode(owner, m)
sttype.mode:=mtagged

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
ref strec d

doswitch lx.symbol
when semisym then
	lex()

when kendsym,rbracksym,rcurlysym then
	exit

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
	readtaggedfields(owner,t)

else
	if istypestarter() then
		goto readvar
	else
		serror("tagged union?")
	fi
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
=== mvar.m 14/25 ===
!VAR Support Library
!Support functions for Vars can be of 3 kinds:

! 1 High level; params and return types can be actual objects.
!   Body uses normal operations on vars, but implicit free etc
!   is called on params. Return value is in return slot on stac
! 2 Mid level; params and return type are explicit varrec records,
!   which expose variant structures. Since these are just normal
!   records, but small enough to fit into registers, no special
!   freeing is done. I think that return value is in D1:D0 (including
!   floats as those are inside a variant)

!	Could be defined as procs, with explicit loading to d1:d0, but
!	can also be functions provided they will be simple functions.

! 3 Low level; params are passed via registers, and return value will
!   be D1:D0. Not a normal call; likely to be via MCL code

! In cases (1) and (2), parameters for those implementing binary ops
! can be in inverse order, ie. (y,x) rather than (x,y), since the PCL
! stack will push x first, in the place where the second argument normally
! goes (normal function calls push right to left)

import clib
import var_decls
import var_tables
import var_numbers
import var_print
import var_objects
import var_strings
import var_lists
import var_support
import var_decimals
import var_arrays

global function m$make_int(int a)object p=
	return int_make(a)
end

global function m$make_real(real x)object p=
	return real_make(x)
end

global function m$make_string(ichar s)object p=
	return string_make(s)
end

global function m$make_dec(ichar s)object p=
	return decimal_make(s)
end

global function m$make_list(int n, lower, object a)object p=

	p:=list_make(&a, n, lower)

!no freeing needed as list_make will take over ownership
	return p
end

global function m$make_listz(int lower)object p=

	p:=list_make(nil, 0, lower)
	return p
end

global function m$make_range(int upper,lower)object p=
	return range_make(lower,upper)
end

global function m$var_to_int(object p)int=
	case p.tag
	when vt_int then
		return p.unum.value
	when vt_real then
		return p.unum.xvalue
	when vt_decimal then
		return decimal_toint(p)

	else
		vxerror("var->int?")
		return 0
	esac
end

global function m$var_to_real(object p)real=
	case p.tag
	when vt_int then
		return p.unum.value
	when vt_real then
		return p.unum.xvalue
	when vt_decimal then
		return decimal_toreal(p)

	else
		vxerror("var->real?")
		return 0
	esac
end

global function m$var_to_string(object p)ichar=
	case p.tag
	when vt_string then
		if p.ustr.length=0 then
			return ""
		else
			return convtostringz(p.ustr.strptr,p.ustr.length)
		fi
	else
		vxerror("var->string?")
		return nil
	esac
end

global proc m$initmemz_var(iobject p)=
	p^:=void_new()
end

global proc m$popmem_var(iobject pdest, object a)=
!free dest at given dest, transfer ownership of a to pdest
!CPL "POPMEM",PDEST, PDEST^
!	m$obj_mon_m(v_unshare,pdest^)
!	obj_unshare(pdest^)
	m$unshare_var(pdest^)
!global function m$unshare_var	(object a)object	= {m$obj_mon_m(v_unshare,a)}
	pdest^:=a
end

global proc m$freemem_var(object a)=
!CPL "FREEMEM"
!	obj_unshare(a)
	m$unshare_var(a)
end

global proc m$print_var(object a,ichar fmtstyle=nil)=
	obj_print(a,fmtstyle)	
end

function m$obj_bin_m(int opc, object a,b)object c=
	c:=obj_bin(opc,a,b)
	if --a.refcount=0 then obj_free(a) fi
	if --b.refcount=0 then obj_free(b) fi
	return c
end

proc m$obj_binto_m(int opc, iobject a, object b)=
	obj_binto(opc,a,b)
!	if --a.refcount=0 then obj_free(a) fi
	if --b.refcount=0 then obj_free(b) fi
end

proc m$obj_monto_m(int opc, iobject a)=
	obj_monto(opc,a)
!CPL "HERE",A.REFCOUNT
!	if --a.refcount=0 then obj_free(a^) fi
end

function m$obj_mon_m(int opc, object a)object c=
	c:=obj_mon(opc,a)
	if --a.refcount=0 then obj_free(a) fi
	return c
end

function m$obj_intbin_m(int opc, object a,b)int res=
	res:=obj_intbin(opc,a,b)
	if --a.refcount=0 then obj_free(a) fi
	if --b.refcount=0 then obj_free(b) fi
	return res
end

function m$obj_intmon_m(int opc, object a)int res=
	res:=obj_intmon(opc,a)
!	if --a.refcount=0 then obj_free(a) fi
	return res
end


global function m$add_var	(object b,a)object	= {m$obj_bin_m(v_add,a,b)}
global function m$sub_var	(object b,a)object	= {m$obj_bin_m(v_sub,a,b)}
global function m$mul_var	(object b,a)object	= {m$obj_bin_m(v_mul,a,b)}
global function m$div_var	(object b,a)object	= {m$obj_bin_m(v_div,a,b)}
global function m$idiv_var	(object b,a)object	= {m$obj_bin_m(v_idiv,a,b)}
global function m$irem_var	(object b,a)object	= {m$obj_bin_m(v_irem,a,b)}
global function m$power_var	(object b,a)object	= {m$obj_bin_m(v_power,a,b)}
global function m$iand_var	(object b,a)object	= {m$obj_bin_m(v_iand,a,b)}
global function m$ior_var	(object b,a)object	= {m$obj_bin_m(v_ior,a,b)}
global function m$ixor_var	(object b,a)object	= {m$obj_bin_m(v_ixor,a,b)}
global function m$shl_var	(object b,a)object	= {m$obj_bin_m(v_shl,a,b)}
global function m$shr_var	(object b,a)object	= {m$obj_bin_m(v_shr,a,b)}
global function m$andl_var	(object b,a)object	= {m$obj_bin_m(v_andl,a,b)}
global function m$orl_var	(object b,a)object	= {m$obj_bin_m(v_orl,a,b)}
global function m$append_var(object b,a)object	= {m$obj_bin_m(v_append,a,b)}
global function m$concat_var(object b,a)object	= {m$obj_bin_m(v_concat,a,b)}
global function m$min_var	(object b,a)object	= {m$obj_bin_m(v_min,a,b)}
global function m$max_var	(object b,a)object	= {m$obj_bin_m(v_max,a,b)}
global function m$atan2_var	(object b,a)object	= {m$obj_bin_m(v_atan2,a,b)}

global function m$eq_var	(object b,a)int	= {m$obj_intbin_m(v_equal,a,b)}
global function m$ne_var	(object b,a)int	= {not m$obj_intbin_m(v_equal,a,b)}
global function m$lt_var	(object b,a)int	= {m$obj_intbin_m(v_compare,a,b)<0}
global function m$le_var	(object b,a)int	= {m$obj_intbin_m(v_compare,a,b)<=0}
global function m$ge_var	(object b,a)int	= {m$obj_intbin_m(v_compare,a,b)>=0}
global function m$gt_var	(object b,a)int	= {m$obj_intbin_m(v_compare,a,b)>0}
global function m$in_var	(object b,a)int = {obj_in(a,b)}

global function m$isequal_var	(object b,a)int	= {m$obj_intbin_m(v_same,a,b)}

global function m$neg_var		(object a)object	= {m$obj_mon_m(v_neg,a)}
global function m$abs_var		(object a)object	= {m$obj_mon_m(v_abs,a)}
global function m$inot_var		(object a)object	= {m$obj_mon_m(v_inot,a)}
global function m$sqrt_var		(object a)object	= {m$obj_mon_m(v_sqrt,a)}
global function m$sin_var		(object a)object	= {m$obj_mon_m(v_sin,a)}
global function m$cos_var		(object a)object	= {m$obj_mon_m(v_cos,a)}
global function m$tan_var		(object a)object	= {m$obj_mon_m(v_tan,a)}
global function m$asin_var		(object a)object	= {m$obj_mon_m(v_asin,a)}
global function m$acos_var		(object a)object	= {m$obj_mon_m(v_acos,a)}
global function m$atan_var		(object a)object	= {m$obj_mon_m(v_atan,a)}
global function m$exp_var		(object a)object	= {m$obj_mon_m(v_exp,a)}
global function m$ln_var		(object a)object	= {m$obj_mon_m(v_ln,a)}
global function m$log_var		(object a)object	= {m$obj_mon_m(v_log,a)}
global function m$round_var		(object a)object	= {m$obj_mon_m(v_round,a)}
global function m$floor_var		(object a)object	= {m$obj_mon_m(v_floor,a)}
global function m$ceil_var		(object a)object	= {m$obj_mon_m(v_ceil,a)}
global function m$fract_var		(object a)object	= {m$obj_mon_m(v_fract,a)}
global function m$chr_var		(object a)object	= {m$obj_mon_m(v_chr,a)}
global function m$bounds_var	(object a)object	= {m$obj_mon_m(v_bounds,a)}
global function m$share_var		(object a)object	= {m$obj_mon_m(v_share,a)}

!global function m$unshare_var	(object a)object	= {m$obj_mon_m(v_unshare,a)}
global proc m$unshare_var	(object a) =
!CPL "M$UNSHARE",A,VARTYPENAMES[A.TAG]
	if --a.refcount<=0 then
		obj_free(a)
	fi
end

! {m$obj_mon_m(v_unshare,a)}

global function m$free_var		(object a)object	= {m$obj_mon_m(v_free,a)}
global function m$dupl_var		(object a)object	= {m$obj_mon_m(v_dupl,a)}

global function m$notl_var		(object a)int		= {m$obj_intmon_m(v_notl,a)}
global function m$istruel_var	(object a)int		= {m$obj_intmon_m(v_istruel,a)}
global function m$lwb_var		(object a)int		= {m$obj_intmon_m(v_lwb,a)}
global function m$upb_var		(object a)int		= {m$obj_intmon_m(v_upb,a)}
global function m$len_var		(object a)int		= {m$obj_intmon_m(v_len,a)}
global function m$asc_var		(object a)int		= {m$obj_intmon_m(v_asc,a)}

global proc		m$addto_var		(object b, iobject a)	= {m$obj_binto_m(v_addto,a,b)}
global proc		m$subto_var		(object b, iobject a)	= {m$obj_binto_m(v_subto,a,b)}
global proc		m$multo_var		(object b, iobject a)	= {m$obj_binto_m(v_multo,a,b)}
global proc		m$divto_var		(object b, iobject a)	= {m$obj_binto_m(v_divto,a,b)}
global proc		m$idivto_var	(object b, iobject a)	= {m$obj_binto_m(v_idivto,a,b)}
global proc		m$iremto_var	(object b, iobject a)	= {m$obj_binto_m(v_iremto,a,b)}
global proc		m$iandto_var	(object b, iobject a)	= {m$obj_binto_m(v_iandto,a,b)}
global proc		m$iorto_var		(object b, iobject a)	= {m$obj_binto_m(v_iorto,a,b)}
global proc		m$ixorto_var	(object b, iobject a)	= {m$obj_binto_m(v_ixorto,a,b)}
global proc		m$shlto_var		(object b, iobject a)	= {m$obj_binto_m(v_shlto,a,b)}
global proc		m$shrto_var		(object b, iobject a)	= {m$obj_binto_m(v_shrto,a,b)}
global proc		m$andto_var		(object b, iobject a)	= {m$obj_binto_m(v_andto,a,b)}
global proc		m$orto_var		(object b, iobject a)	= {m$obj_binto_m(v_orto,a,b)}
global proc		m$appendto_var	(object b, iobject a)	= {m$obj_binto_m(v_appendto,a,b)}
global proc		m$concatto_var	(object b, iobject a)	= {m$obj_binto_m(v_concatto,a,b)}
global proc		m$minto_var		(object b, iobject a)	= {m$obj_binto_m(v_minto,a,b)}
global proc		m$maxto_var		(object b, iobject a)	= {m$obj_binto_m(v_maxto,a,b)}

global proc		m$negto_var		(iobject a)	= {m$obj_monto_m(v_negto,a)}
global proc		m$absto_var		(iobject a)	= {m$obj_monto_m(v_absto,a)}
global proc		m$inotto_var	(iobject a)	= {m$obj_monto_m(v_inotto,a)}
global proc		m$notlto_var	(iobject a)	= {m$obj_monto_m(v_notlto,a)}
global proc		m$incrto_var	(iobject a)	= {m$obj_monto_m(v_incrto,a)}
global proc		m$decrto_var	(iobject a)	= {m$obj_monto_m(v_decrto,a)}

global function m$getindex_var		(int index, object a)object		= {obj_getindex(a,index)}
global proc		m$putindex_var		(int index, iobject a, object x)	= {obj_putindex(a,index,x)}

global function m$getdotindex_var	(int index, object a)object		= {obj_getdotindex(a,index)}
global proc		m$putdotindex_var	(int index, iobject a, object x)	= {obj_putdotindex(a,index, x)}
!
global function m$getslice_var		(object a, int j,i)object	= {obj_getslice(a,i,j)}
!global proc		 m$putslice_var		(object d,c,b,a)		= {obj_putslice(a,b,c,d)}
!global function m$getdotslice_var	(object c,b,a)object	= {obj_getdotslice(a,b,c)}
!global proc		m$putdotslice_var	(object d,c,b,a)		= {obj_putdotslice(a,b,c,d)}
!
!global function m$getkeyindex_var	(object c,b,a)object	= {obj_getkeyindex(a,b,c)}
!global proc		m$putkeyindex_var	(object c,b,a)			= {obj_putkeyindex(a,b,c)}

!global function m$insert_var	(object c,b,a)object	= {obj_insert(a,b,c)}
!global function m$delete_var	(object b,a)object	= {obj_delete(a,b)}
!global function m$resize_var	(object b,a)object	= {obj_resize(a,b)}
!

!global function newarray(int elemt, object dims)var=
!	case dims.tag
!	when vt_int then
!		return new_array(vt,
!
!	return obj_new(vt)
!end
!


global function newarray(int tag, length, lower=1)var v=
	return cast@(array_new(tag, length, lower),var)
end

global function newarrayr(int tag, lower, upper)var v=
	return var@(array_new(tag, upper-lower+1, lower))
end

=== var_decls.m 15/25 ===
import msys
import clib

import var_tables

!global macro tu1 = tbit
!global macro tu2 = tbit2
!global macro tu4 = tbit4

global type object  = ref objrec
global type iobject  = ref object

global const objectsize  = object.bytes
global const iobjectsize  = iobject.bytes

record listrec =
	word32		refcount
	byte		tag
	struct
		byte	objtype
		byte	mutable
		byte	spare
	end

	union
		iobject		iobjptr
		word64		padding1
	end

	word32		length
	int32		lower

	union
		object	objptr2
		word32	allocated
		word64	padding2
	end
end

record stringrec =
	word32		refcount
	byte		tag
	struct
		byte	objtype
		byte	mutable
		byte	rectype
	end

	union
		ichar		strptr
		word64		padding1
	end

	int32		length
	int32		lower

	union
		object	objptr2
		word32	allocated
	end
end

record recordrec =
	word32		refcount
	byte		tag
	struct
		byte	spare
		byte	mutable
		byte	rectype
	end

	union
		iobject		iobjptr
		ref byte	ptr
		word64		padding1
	end

	word32		length		!make is easier to index like a list
	int32		lower

	union
		object	recobj
		word32	spare3
	end
end

record decimalrec =
	word32		refcount
	byte		tag
	byte		spare1
	byte		spare2
	byte		spare3

	union
		ref void	bnptr
		word64		padding1
	end

	int			spare4
	int			spare5
end

record numrec =
	word32		refcount
	byte		tag
	byte		spare1
	byte		spare2
	byte		spare3

	union
		int64		value
		word64		uvalue
		real64		xvalue
	end

	int			spare4
	int			spare5
end

record rangerec =
	word32		refcount
	byte		tag
	byte		spare1
	byte		spare2
	byte		spare3

	int64		lower
	int64		upper

	int64		spare4
end

record setrec =
	word32		refcount
	byte		tag
	struct
		byte	spare
		byte	mutable
		byte	spare2
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	int16		elemtag

	word64		allocated64
end

record dictrec =
	word32		refcount
	byte		tag
	struct
		byte	spare
		byte	mutable
		byte	spare2
	end

	union
		iobject		iobjptr
		word64		padding1
	end

	word32		length
	int32		lower

	union
		struct
			word32		allocated
			word32		dictitems
		end
		object			objptr2
	end
end

record arrayrec =
	word32		refcount
	byte		tag
	struct
		byte	objtype
		byte	mutable
		byte	spare
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	int16		elemtype

	union
		object	objptr2
		word32	allocated
	end
end

record bitsrec =
	word32		refcount
	byte		tag
	struct
		byte	objtype
		byte	mutable
		byte	spare
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	byte		elemtype
	byte		bitoffset

	union
		object	objptr2
		word64	allocated64
	end
end

record structrec =
	word32		refcount
	byte		tag
	struct
		byte	spare
		byte	mutable
		byte	spare2
	end

	union
		ref byte	ptr
		word64		padding1
	end

	word32		length
	int16		lower
	int16		elemtag

	union
		object	objptr2
		word32	allocated
	end
end

global record objrec =				!32 bytes
	union
		struct
			word32		refcount
			byte		tag
			byte		objtype		!normal/slice/ext for string/list/array/struct
			byte		spare1
			byte		spare2

			word64		spare3
			word64		spare4
			union
				object	objptr2		!share with string/list/array/struct?
				word64	dummy4
			end
		end

		listrec				ulist
		stringrec			ustr
		recordrec			urec
		decimalrec			udec
		numrec				unum
		rangerec			urange
		setrec				uset
		dictrec				udict
		arrayrec			uarray
		bitsrec				ubits
		structrec			ustruct
	end
end

!global record genfieldnamerec =
!	ichar name					!after bc load
!	int32 dataindex
!	union
!		int32 datalength
!		int32 datalast
!	end
!end
!
!global record genfielddatarec =
!	int32 fieldindex
!	int32 recordtype
!	int32 fieldtype			!-procid, -constid, -staticid, -typeid are special codes
!	union
!		int32 offset			!or const value
!		word32 index			!into proctable, statictable, or type code
!		word32 procoffset
!	end
!end

global record fmtrec=
	byte	minwidth	! (0)   min field width (0 if not used or don't care)
	i8	precision	! (0)   number of decimals/significant figures/max width
	byte	base		! (10)  2,8,10,16

	char	quotechar	! (0)   0/'"'/c
	char	padchar		! (' ') ' '/'0'/c
	char	realfmt		! ('f') 'e'/'f'/'g'

	char	plus		! (0)   0/'+'
	char	sepchar		! (0)   0/','/c placed every 3 (base=10) or 4 digits
	char	lettercase	! ('A') 'A'/'a'
	char	justify		! ('R') 'L'/'R'/'C'?
	char	suffix		! (0)   0/'B'/'H'/c
	char	usigned		! (0)   0/'U' force unsigned o/p for ints (eg. for hex display)
	char	charmode	! (0)  0/'U'/'M'	o/p int as int/single char/multi-char
	char	showtype	! (0) 0/'Y'	Show type
	[2]byte	spare
end

global const int maxtype=300

global int ntypes
global [0:maxtype]int32 ttbasetype		!basetype
global [0:maxtype]ichar ttname 			!name of type
global [0:maxtype]int32 ttbitwidth

global [0:maxtype]int64 ttsize 			!.size in bytes
global [0:maxtype]int32 ttlower 		!.lbound (default 1 or case unused)
global [0:maxtype]word32 ttlength 		!elements in array/record (actual fields) (/string
global [0:maxtype]int32 ttstartfield 	!start index in pcfieldtable^[]
global [0:maxtype]int32 ttstructfields	!entries in pcfieldtable^[]

global [0:maxtype]int32 tttarget 		!for array/ref types
global [0:maxtype]byte ttusercat

global const hasrefmask = 0x10000		!1st bit of 3rd byte, when writing to .tagx

global tabledata() [0:]ichar objtypenames =
	(normal_obj=0,	$),
	(slice_obj,		$),
	(extslice_obj,	$)
end

global const int objsize=objrec.bytes

global [0..255]object chrtable		!remember single-character objects

!global const maxgenfields=1000
!global [maxgenfields]genfieldnamerec genfieldnames
!global [maxgenfields]genfielddatarec genfielddata
!global [maxgenfields]ref intpc genfieldpcaddress
!global int ngenfieldnames
!global int ngenfielddata

!global const int maxcmdparam=32
!global int ncmdparams
!global [0..maxcmdparam]ichar cmdparamtable
!
!global ref procrec proclist			!linked list of all procs
!global int nproclist

!!for bc file generation
!global int nstrings=0
!global int nsymbols=0
!global int nstructfields=0
!
!global ref[]ichar stringtable
!global ref[]int stringlentable


global [binopnames.len,32,32]ref void binoptable
!global [intbinopnames.len,32,32]ref void intbinoptable
global [monopnames.len,32]ref void monoptable
global [ibinopnames.len,32,32]ref void ibinoptable
global [imonopnames.len,32]ref void imonoptable

global [-256..+256]object inttable
=== var_tables.m 16/25 ===
global tabledata() [0:]ichar vartypenames =
	(vt_void=0,		$),	! means variant is unassigned

	(vt_int,		$),		! 64-bit signed int
	(vt_word,		$),		! 64-bit unsigned int
	(vt_real,		$),		! 64-bit float
	(vt_range,		$),		! 64+64-bit int:int
	(vt_decimal,	$),		! Arbitrary precision integer/float

	(vt_string,		$),		! 8-bit string, flex and mutable
	(vt_wstring,	$),		! 16/32-bit string
	(vt_list,		$),		! Sequence of variants
	(vt_record,		$),		! Record of shorts and longs
	(vt_dict,		$),		! Dictionary of V:V keys and values
	(vt_recordlink,	$),		! Link to record object

	(vt_set,		$),		! Pascal-like bit-set
	(vt_array,		$),		! Sequence of packed
	(vt_bits,		$),		! Sequence of bits
	(vt_struct,		$),		! Record of packed and flat arrays/structs

	(vt_type,		$),		! Represents a type-code
	(vt_refproc,	$),		! Pointer to Q proc
	(vt_refdllproc,	$),		! Pointer to foreign function
end

global tabledata() [0:]ichar packtypenames, [0:]byte packtypewidths,
					[0:]byte packtypesizes =

	(pt_void=0,		$,	0,		0),

	(pt_u1,			$,	1,		1),
	(pt_u2,			$,	2,		1),
	(pt_u4,			$,	4,		1),
	(pt_u8,			$,	8,		1),
	(pt_u16,		$,	16,		2),
	(pt_u32,		$,	32,		4),
	(pt_u64,		$,	64,		8),
	(pt_u128,		$,	128,	16),

	(pt_i8,			$,	8,		1),
	(pt_i16,		$,	16,		2),
	(pt_i32,		$,	32,		4),
	(pt_i64,		$,	64,		8),
	(pt_i128,		$,	128,	16),

	(pt_r32,		$,	32,		4),
	(pt_r64,		$,	64,		8),

	(pt_ref,		$,	64,		8),
end

global tabledata() []ichar binopnames, []byte intbinop =
	(v_add,			$,		0),
	(v_sub,			$,		0),
	(v_mul,			$,		0),
	(v_div,			$,		0),
	(v_idiv,		$,		0),
	(v_irem,		$,		0),
	(v_power,		$,		0),
	(v_equal,		$,		1),
	(v_compare,		$,		1),
	(v_same,		$,		1),
	(v_iand,		$,		0),
	(v_ior,			$,		0),
	(v_ixor,		$,		0),
	(v_shl,			$,		0),
	(v_shr,			$,		0),
	(v_andl,		$,		1),
	(v_orl,			$,		1),
	(v_append,		$,		0),
	(v_concat,		$,		0),
	(v_min,			$,		0),
	(v_max,			$,		0),
	(v_atan2,		$,		0),
	(v_in,			$,		1),
end

global tabledata() []ichar monopnames, []byte intmonop =
	(v_neg,			$,		0),
	(v_abs,			$,		0),
	(v_inot,		$,		0),
	(v_notl,		$,		1),
	(v_istruel,		$,		1),
	(v_sqrt,		$,		0),
	(v_sin,			$,		0),
	(v_cos,			$,		0),
	(v_tan,			$,		0),
	(v_asin,		$,		0),
	(v_acos,		$,		0),
	(v_atan,		$,		0),
	(v_exp,			$,		0),
	(v_ln,			$,		0),
	(v_log,			$,		0),
	(v_round,		$,		0),
	(v_floor,		$,		0),
	(v_ceil,		$,		0),
	(v_fract,		$,		0),
	(v_asc,			$,		0),
	(v_chr,			$,		0),
	(v_lwb,			$,		0),
	(v_upb,			$,		0),
	(v_len,			$,		0),
	(v_bounds,		$,		0),
	(v_share,		$,		0),
	(v_unshare,		$,		0),
	(v_free,		$,		0),
	(v_dupl,		$,		0),
end

global tabledata() []ichar ibinopnames =
	(v_addto,		$),
	(v_subto,		$),
	(v_multo,		$),
	(v_divto,		$),
	(v_idivto,		$),
	(v_iremto,		$),
	(v_iandto,		$),
	(v_iorto,		$),
	(v_ixorto,		$),
	(v_shlto,		$),
	(v_shrto,		$),
	(v_andto,		$),
	(v_orto,		$),
	(v_appendto,	$),
	(v_concatto,	$),
	(v_minto,		$),
	(v_maxto,		$),
end

global tabledata() []ichar imonopnames =
	(v_negto,		$),
	(v_absto,		$),
	(v_inotto,		$),
	(v_notlto,		$),
	(v_incrto,		$),
	(v_decrto,		$),
end

global tabledata() []ichar miscopnames =
	(v_new,			$),
	(v_make,		$),
	(v_print,		$),
	(v_tostr,		$),
	(v_getdot,		$),
	(v_putdot,		$),
	(v_getindex,	$),
	(v_putindex,	$),
	(v_getdotindex,	$),
	(v_putdotindex,	$),
	(v_getslice,	$),
	(v_putslice,	$),
	(v_getdotslice,	$),
	(v_putdotslice,	$),
	(v_getkeyindex,	$),
	(v_putkeyindex,	$),
	(v_insert,		$),
	(v_delete,		$),
	(v_resize,		$),
end
=== var_numbers.m 17/25 ===
import mlib
import var_decls
import var_tables
import var_objects
import var_strings

global function int_make(int a)object p=
!share this for common values

!CPL "INTMAKE1",A
	if a>=inttable.lwb and a<=inttable.upb then
		obj_share(p:=inttable[a])
		return p
	fi

!CPL "INTMAKE2",A
	p:=obj_new(vt_int)
	p.unum.value:=a

	return p
end

global function real_make(real x)object p=
!share this for common values

!	if a>=inttable.lwb and a<=inttable.upb then
!		obj_share(p:=inttable[a])
!		return p
!	fi
!
	p:=obj_new(vt_real)
	p.unum.xvalue:=x

	return p
end

global function range_make(int lower, upper)object p=
	p:=obj_new(vt_range)
	p.urange.lower:=lower
	p.urange.upper:=upper
	return p
end

proc int_unshare(object a)=
!CPL "UNSHARE INT",A.REFCOUNT, A.UNUM.VALUE
	if --a.refcount<=0 then
!CPL "FREE INT"
		pcm_free32(a)
	fi
end

proc real_unshare(object a)=
	int_unshare(a)
end

global function int_add(object a,b)object c=
	return int_make(a.unum.value + b.unum.value)
end

global function int_sub(object a,b)object c=
	return int_make(a.unum.value - b.unum.value)
end

global function real_sub(object a,b)object c=
	return real_make(a.unum.xvalue - b.unum.xvalue)
end

global function int_mul(object a,b)object c=
	return int_make(a.unum.value * b.unum.value)
end

global function real_mul(object a,b)object c=
	return real_make(a.unum.xvalue * b.unum.xvalue)
end

global function int_div(object a,b)object c=
	return real_make(real(a.unum.value)/b.unum.value)
end

global function int_idiv(object a,b)object c=
	return int_make(a.unum.value % b.unum.value)
end

global function int_irem(object a,b)object c=
	return int_make(a.unum.value rem b.unum.value)
end

global function int_iand(object a,b)object c=
	return int_make(a.unum.value iand b.unum.value)
end

global function int_ior(object a,b)object c=
	return int_make(a.unum.value ior b.unum.value)
end

global function int_ixor(object a,b)object c=
	return int_make(a.unum.value ixor b.unum.value)
end

global function int_shl(object a,b)object c=
	return int_make(a.unum.value << b.unum.value)
end

global function int_shr(object a,b)object c=
	return int_make(a.unum.value >> b.unum.value)
end

global function int_min(object a,b)object c=
	return int_make(a.unum.value min b.unum.value)
end

global function int_max(object a,b)object c=
	return int_make(a.unum.value max b.unum.value)
end

global function int_power(object a,b)object c=
	return int_make(a.unum.value ** b.unum.value)
end

global function int_equal(object a,b)int c=
	return a.unum.value=b.unum.value
end

global function int_compare(object a,b)int c=
	if a.unum.value<b.unum.value then
		return -1
	elsif a.unum.value>b.unum.value then
		return 1
	else
		return 0
	fi
end

global function real_compare(object a,b)int c=
	if a.unum.xvalue<b.unum.xvalue then
		return -1
	elsif a.unum.xvalue>b.unum.xvalue then
		return 1
	else
		return 0
	fi
end

global function int_neg(object a)object=
	return int_make(-a.unum.value)
end

global function int_abs(object a)object=
	return int_make(abs a.unum.value)
end

global function int_inot(object a)object=
	return int_make(inot a.unum.value)
end

global function int_notl(object a)int=
	return not a.unum.value
end

global function int_istruel(object a)int=
	return istrue a.unum.value
end

global function int_chr(object a)object=
	[8]char str
	str[1]:=a.unum.value
	str[2]:=0
	return string_make(&.str)
end

global function int_sqrt(object a)object=
	return real_make(sqrt a.unum.value)
end

!global function int_sin(object a)object={real_make(a.unum.value)}
!global function real_sin(object a)object={real_make(a.unum.value)}

global proc int_addto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value + b.unum.value)
	obj_unshare(a)
end

global proc int_subto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value - b.unum.value)
	obj_unshare(a)
end

global proc int_multo(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value * b.unum.value)
	obj_unshare(a)
end

global proc int_divto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(real(a.unum.value) * b.unum.value)
	obj_unshare(a)
end

global proc int_idivto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value / b.unum.value)
	obj_unshare(a)
end

global proc int_iremto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value rem b.unum.value)
	obj_unshare(a)
end

global proc int_iandto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value iand b.unum.value)
	obj_unshare(a)
end

global proc int_iorto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value ior b.unum.value)
	obj_unshare(a)
end

global proc int_ixorto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value ixor b.unum.value)
	obj_unshare(a)
end

global proc int_shlto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value << b.unum.value)
	obj_unshare(a)
end

global proc int_shrto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value >> b.unum.value)
	obj_unshare(a)
end

global proc int_minto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value min b.unum.value)
	obj_unshare(a)
end

global proc int_maxto(iobject pa, object b)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value max b.unum.value)
	obj_unshare(a)
end

global proc int_negto(iobject pa)=
	object a

	a:=pa^
	pa^:=int_make(-a.unum.value)
	obj_unshare(a)
end

global proc int_absto(iobject pa)=
	object a

	a:=pa^
	pa^:=int_make(abs a.unum.value)
	obj_unshare(a)
end

global proc int_inotto(iobject pa)=
	object a

	a:=pa^
	pa^:=int_make(inot a.unum.value)
	obj_unshare(a)
end

global proc int_notlto(iobject pa)=
	object a

	a:=pa^
	pa^:=int_make(not a.unum.value)
	obj_unshare(a)
end

global proc int_incrto(iobject pa)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value+1)
	obj_unshare(a)
end

global proc int_decrto(iobject pa)=
	object a

	a:=pa^
	pa^:=int_make(a.unum.value-1)
	obj_unshare(a)
end

global function real_add(object a,b)object c=
	return real_make(a.unum.xvalue + b.unum.xvalue)
end
=== var_objects.m 18/25 ===
import clib
!import msys
import mlib
!import oslib

import var_decls
import var_tables
import var_support
import var_strings
import var_lists
import var_numbers
import var_decimals
import var_arrays

global object zeroobj
global object emptylist
global object emptystring
global object emptyset
global objrec voidobj

proc $init=
	object p

	zeroobj:=pcm_allocz(objrec.bytes)
	zeroobj.refcount:=1

	emptylist:=obj_new(vt_list)
	emptylist.ulist.lower:=1
	emptylist.objtype:=normal_obj

	emptystring:=obj_new(vt_string)
	emptystring.objtype:=normal_obj

	emptyset:=obj_new(vt_set)
	emptyset.objtype:=normal_obj

!	voidobj:=zeroobj
!	voidobj.refcount:=0x8000'0000		!void calls to free

	for i in inttable do
		p:=obj_new(vt_int)
		p.unum.value:=i
		inttable[i]:=p
	od

end

global function obj_bin(int opc, object a,b)object=
	ref function(object a,b)object fnptr

	fnptr:=binoptable[opc,a.tag,b.tag]
	if fnptr=nil then
		fnptr:=binoptable[opc,a.tag,b.tag]:=getbinophandler(opc,a.tag,b.tag)
		if fnptr=nil then
			novxhandler(binopnames[opc],a.tag,b.tag)
		fi
	fi

	return fnptr^(a,b)
end

global function obj_intbin(int opc, object a,b)int=
	ref function(object a,b)int fnptr

	fnptr:=binoptable[opc,a.tag,b.tag]
	if fnptr=nil then
		fnptr:=binoptable[opc,a.tag,b.tag]:=getbinophandler(opc,a.tag,b.tag)
		if fnptr=nil then
			novxhandler(binopnames[opc],a.tag,b.tag)
		fi
	fi

	return fnptr^(a,b)
end

global proc obj_binto(int opc, iobject pa, object b)=
	ref function(iobject pa,object b)object fnptr

	fnptr:=ibinoptable[opc,pa.tag,b.tag]
	if fnptr=nil then
		fnptr:=ibinoptable[opc,pa.tag,b.tag]:=getibinophandler(opc,pa.tag,b.tag)
		if fnptr=nil then
			novxhandler(ibinopnames[opc],pa.tag,b.tag)
		fi
	fi

	fnptr^(pa,b)
end

global function obj_mon(int opc, object a)object=
	ref function(object a)object fnptr

	fnptr:=monoptable[opc,a.tag]
	if fnptr=nil then
		fnptr:=monoptable[opc,a.tag]:=getmonophandler(opc,a.tag)
		if fnptr=nil then
			novxhandler(monopnames[opc],a.tag,vt_void)
		fi
	fi

	return fnptr^(a)
end

global proc obj_monto(int opc, iobject pa)=
	ref function(iobject pa)object fnptr

	fnptr:=imonoptable[opc,pa.tag]
	if fnptr=nil then
		fnptr:=imonoptable[opc,pa.tag]:=getimonophandler(opc,pa.tag)
		if fnptr=nil then
			novxhandler(imonopnames[opc],pa.tag,vt_void)
		fi
	fi

	fnptr^(pa)
end

global function obj_intmon(int opc, object a)int=
	ref function(object a)int fnptr

	fnptr:=monoptable[opc,a.tag]
	if fnptr=nil then
		fnptr:=monoptable[opc,a.tag]:=getmonophandler(opc,a.tag)
		if fnptr=nil then
			novxhandler(monopnames[opc],a.tag,vt_void)
		fi
	fi

	return fnptr^(a)
end
proc novxhandler(ichar opname, int taga, tagb)=
	println opname,vartypenames[taga],vartypenames[tagb]
	vxerror("Can't find handler")
end

global function obj_new(int tag)object p=
!create new object descriptor, which all fields set to zero
!except refcount=1

!CPL "OBJNEW1"
	p:=pcm_alloc32()
	p^:=zeroobj^			!includes refcount=1
	p.tag:=tag
!
	return p
end

global function obj_newobj(object a, b=nil, c=nil, d=nil)object p=
!create new object descriptor, which all fields set to zero
!except refcount=1
	int tag

	checkobj(a,vt_int)
	tag:=a.unum.value	

	case tag
	when vt_array then
!!		return array_new(getint(b),getc,d)
!
!	when vt_list then
!		return list_new(b,c)
	else
		vxerror("obj/newobj")
	esac

!CPL "OBJNEW1"
	p:=pcm_alloc32()
	p^:=zeroobj^			!includes refcount=1
	p.tag:=tag
!
	return p
end

proc checkobj(object p, int tag)=
	if p.tag<>tag then
		cpl "Was",vartypenames[p.tag],"not",vartypenames[tag]
		vxerror("Unexpected type")
	fi
end

global function obj_dupl(object p)object q=
	int tp

	switch p.tag
	when vt_int,vt_real,vt_void then
		++p.refcount
		return p

	when vt_list,vt_record then
		q:=list_dupl(p)
	when vt_string then
		q:=string_dupl(p)

	when vt_decimal then
		q:=decimal_dupl(p)

!	when vt_struct then
!		tp:=p.ustruct.packtype
!		q:=struct_new(tp)
!		memcpy(q.ustruct.ptr,p.ustruct.ptr,ppsize[tp])

	else
	CPL VARTYPENAMES[P.TAG]
		VXERROR("CAN'T DUPL: ")
	end switch

	return q
end

global proc freeobject(object p)=
	pcm_free32(p)
end

global function void_new:object p=
	return &voidobj
end

!global function array_new(int ta, elemtype, length,lower)object p=
!!create a packed array type ta, with element-type t, given length and lower bound.
!!it will be initialised to zeros
!
!	ref byte q
!	int elemsize
!
!	elemsize:=ttsize[elemtype]
!
!	p:=obj_new(ta)
!	p.uarray.mutable:=1
!	p.uarray.lower:=lower
!	p.uarray.length:=length
!	p.uarray.objtype:=normal_obj
!	p.uarray.elemtype:=elemtype
!
!	if length then
!		q:=p.uarray.ptr:=pcm_allocz(length*elemsize)
!		p.uarray.allocated:=allocbytes/elemsize
!	fi
!
!	return p
!end
!
!global function list_new(int length,lower=1, object defval=nil)object p=
!	iobject q
!
!	p:=obj_new(vt_list)
!	p.ulist.mutable:=1
!	p.ulist.lower:=lower
!	p.ulist.length:=length
!	p.ulist.objtype:=normal_obj
!
!	if length then
!		q:=p.ulist.iobjptr:=pcm_alloc(length*objectsize)
!		p.ulist.allocated:=allocbytes/objectsize
!		to length do
!			if defval then
!				q^:=obj_share(defval)
!			else
!				q^:=&voidobj
!			fi
!			++q
!		od
!	fi
!
!	return p
!end
!
!global function set_new(int length,lower)object p=
!!create a packed array type ta, with element-type t, given length and lower bound.
!!it will be initialised to zeros
!
!	p:=bits_new(pt_u1,length,lower)
!	p.tag:=vt_set
!
!	return p
!end
!
!global function bits_new(int elemtype,length,lower)object p=
!!create a packed array type ta, with element-type t, given length and lower bound.
!!it will be initialised to zeros
!
!	ref byte q
!	int bitwidthx,nbits,nbytes
!
!	p:=obj_new(vt_bits)
!	p.ubits.mutable:=1
!	p.ubits.lower:=lower
!	p.ubits.length:=length
!	p.ubits.objtype:=normal_obj
!	p.ubits.elemtype:=elemtype
!
!	bitwidthx:=ttbitwidth[elemtype]		!should be 1, 2 or 4 bits
!	nbits:=length*bitwidthx				!total bits needed
!
!	nbytes := ((nbits-1)/64+1)*8		!bytes required in 64-bit blocks
!
!!CPL "BITSNEW",=LENGTH,P.UBITS.LENGTH
!
!
!	if length then
!!		p.ubits.ptr := pcm_allocz(nbytes)              !(turns total allocated in 'allocbytes')
!		p.ubits.ptr := pcm_alloc(nbytes)              !(turns total allocated in 'allocbytes')
!
!!CPL "BITS/NEW",=NBYTES
!
!		p.ubits.allocated64 := word64(allocbytes)*(8/bitwidthx)
!
!		pcm_clearmem(p.ubits.ptr,allocbytes)
!	else
!		p.ubits.ptr:=nil
!	fi
!
!	return p
!end
!
!global function struct_new(int ut)object p=
!	p:=obj_new(vt_struct)
!	p.ustruct.mutable:=1
!
!	p.ustruct.ptr:=pcm_allocz(ttsize[ut])
!	p.ustruct.allocated:=allocbytes
!
!	return p
!end
!
!global function dict_new(int n)object p=
!	int m
!
!	m:=max(16,nextpoweroftwo(n*2))		!list has 2n entries, min 16, rounded up to 2**x
!
!
!	p:=list_new(m,1,nil)
!	p.tag:=vt_dict
!	p.udict.dictitems:=0
!	return p
!
!end
!
!global function record_new(int ut)object p=
!	iobject q
!
!	p:=obj_new(vt_record)
!	p.urec.rectype:=ut
!	p.urec.mutable:=1
!!	p.urec.lower:=1
!	p.urec.length:=ttlength[ut]
!!	p.urec.objtype:=normal_obj
!
!!	p.urec.vptr:=pcm_alloc(p.urec.length*varrec.bytes)
!	p.urec.iobjptr:=q:=pcm_allocz(ttlength[ut]*objectsize)
!!	p.urec.allocated:=allocbytes/objectsize
!	to p.urec.length do
!		q^:=void_new()
!		++q
!	od
!!
!	return p
!end
!
!global proc list_free(object p)=
!!free contents of list, but not p itself
!	if p.ulist.length then
!!CPL "FREEING",P.ULIST.ALLOCATED*VARREC.BYTES
!		pcm_free(p.ulist.iobjptr,p.ulist.allocated*objectsize)
!	fi
!end
!
!global proc record_free(object p)=
!!free contents of list, but not p itself
!	pcm_free(p.urec.iobjptr,ttlength[p.urec.rectype]*objectsize)
!end
!
!global proc array_free(object p)=
!!free contents of list, but not p itself
!	if p.ulist.length then
!		pcm_free(p.uarray.ptr,p.uarray.allocated*ttsize[p.uarray.elemtype])
!	fi
!end
!
!global proc bits_free(object p)=
!!free contents of list, but not p itself
!	if p.ulist.length then
!		pcm_free(p.ubits.ptr,bits_bytesize(p))
!	fi
!end
!
!global proc dict_free(object p)=
!!free contents of list, but not p itself
!
!	if p.udict.length then
!		pcm_free(p.udict.iobjptr,p.udict.allocated*objectsize)
!	fi
!
!end
!
!global function bits_bytesize(object p)int=
!!return how many bytes are used by the object
!!should be bits, but set should work; also array?
!	int elemtype,nbits
!
!	elemtype:=p.ubits.elemtype
!
!	case elemtype
!	when pt_u1,pt_u2,pt_u4 then
!		nbits:=ttbitwidth[elemtype]*p.ubits.length
!		if nbits iand 7 then			!fractional number of bytes
!			return nbits/8+1
!		else
!			return nbits/8
!		fi
!	esac
!	return ttsize[elemtype]*p.uarray.length		!array?
!end
!
!global proc list_resize(object p,int n)=
!	iobject q
!
!	if n<=p.ulist.allocated then
!		p.ulist.length:=n
!	else
!!CPL "LIST RESIZE"
!		q:=pcm_alloc(n*objectsize)
!		if p.ulist.length then
!			memcpy(q,p.ulist.iobjptr,p.ulist.length*objectsize)
!			pcm_free(p.ulist.iobjptr,p.ulist.allocated*objectsize)
!		fi
!		p.ulist.iobjptr:=q
!		p.ulist.length:=n
!		p.ulist.allocated:=allocbytes/objectsize
!	fi
!end
!
!global proc array_resize(object p,int n)=
!	ref byte q
!	int elemsize
!
!	elemsize:=ttsize[p.uarray.elemtype]
!
!	if n<=p.uarray.allocated then
!		p.uarray.length:=n
!	else
!		q:=pcm_alloc(n*elemsize)
!		if p.uarray.length then
!			memcpy(q,p.uarray.ptr,p.uarray.length*elemsize)
!			pcm_free(p.uarray.ptr,p.uarray.allocated*elemsize)
!		fi
!		p.uarray.ptr:=q
!		p.uarray.length:=n
!		p.uarray.allocated:=allocbytes/elemsize
!	fi
!end
!
!global proc bits_resize(object p,int n)=
!	object pnew
!	ref byte q
!	int elemsize,oldrefcount
!
!	if n<=p.ubits.allocated64 then
!		p.ubits.length:=n
!		return
!	fi
!
!	pnew:=bits_new(p.ubits.elemtype,p.ubits.length,p.ubits.lower)
!
!	memcpy(pnew.ubits.ptr, p.ubits.ptr, bits_bytesize(p))
!
!	oldrefcount:=p.ubits.refcount
!	bits_free(p)
!	p^:=pnew^
!	p.refcount:=oldrefcount
!end
!
!global proc string_resize(object p,int n)=
!	ref char q
!	int elemsize
!
!	if n<=p.ustr.allocated then
!		p.ustr.length:=n
!	else
!		q:=pcm_alloc(n)
!		if p.ustr.length then
!			memcpy(q,p.ustr.strptr,p.ustr.length)
!			pcm_free(p.ustr.strptr,p.ustr.allocated)
!		fi
!
!		p.ustr.strptr:=q
!		p.ustr.length:=n
!		p.ustr.allocated:=allocbytes
!	fi
!end
!
!!global function copyonwrite(object p,int tag)object=
!!!if p is not writable, then make a writable copy
!!!return new object
!!var object q
!!var varrec v
!!
!!if p.ulist.mutable then return p fi
!!
!!v.tagx:=tag+hasrefmask
!!v.objptr:=p
!!
!!pc_dupl(&v)
!!
!!q:=v.objptr
!!q.ulist.mutable:=1
!!return q
!!end
!
!global function make_strslicexobj(ichar s, int length)object=
!!s is an existing non-allocated or external string
!!create a special string slice object, which for now has the format::
! .objtype=extslice, but .objptr2=0
!length can be 0, then s can be nil or ""
!
!	object p
!
!	if length=0 then s:=nil fi
!
!	p:=obj_new(vt_string)
!	p.ustr.strptr:=s
!	p.ustr.mutable:=1
!	p.ustr.length:=length
!	p.objtype:=extslice_obj		!.objptr2 will be zero
!	return p
!end
!
!global function bignum_make(ref void bn)object p=
!
!	p:=obj_new(vt_decimal)
!	p.udec.bnptr:=bn
!
!	return p
!end
!
global function obj_share(object p)object=
	++p^.refcount
	return p
end

global proc obj_free(object p)=
!remove p's resources when no more references to it
!CPL "///FREE OBJ REF COUNT=0"
!cPL "///FREE OBJ",P.TAG,VARTYPENAMES[P.TAG], =P.REFCOUNT
	object s

	switch p.tag
	when vt_int,vt_real, vt_range then
!CPL "FREE INT",p.refcount,P.UNUM.VALUE
		pcm_free32(p)

	when vt_list,vt_record then
		list_free(p)

	when vt_array then
		array_free(p)

	when vt_string then
!CPL "FREE STR"
		string_free(p)

	when vt_decimal then
		decimal_free(p)

	when vt_struct then
!	
!		s:=p.ustruct.structdef
!		pcm_free(p.ustruct.ptr,s.ustructdef.size)
!		pcm_free32(p)

	when vt_void then				!voids aren't freed

	else
		CPL "FREE",VARTYPENAMES[P.TAG]

	end switch
end

global proc obj_unshare(object p)=
CPL "OBJ_UNSHARE",=P,VARTYPENAMES[P.TAG],P.REFCOUNT

	if --p^.refcount>0 then
		return
	fi

	obj_free(p)
end

global function obj_getindex(object a,int index)object c=
	case a.tag
	when vt_list then
		return list_getindex(a,index)
	when vt_string then
		return string_getindex(a,index)
	else
		vxerror("getindex")
	esac
	return c
end

global proc obj_putindex(iobject a, int index,object c)=
	case a.tag
	when vt_list then
		list_putindex(a,index,c)
	when vt_string then
		string_putindex(a,index,c)
	else
		vxerror("putindex")
	esac
end

global function obj_getdotindex(object a,int index)object c=
	case a.tag
!	when vt_list then
!		return list_getindex(a,index)
	when vt_string then
		return string_getdotindex(a,index)
	else
		vxerror("getdorindex")
	esac
	return c
end

global proc obj_putdotindex(iobject a,int index, object c)=
	case a.tag
!	when vt_list then
!		list_putindex(a,index,c)
	when vt_string then
		string_putdotindex(a,index,c)
	else
		vxerror("putdotindex")
	esac
end

global function obj_getslice(object a, int i,j)object=
	case a.tag
	when vt_list then
		return list_getslice(a,i,j)
	when vt_string then
		return string_getslice(a,i,j)
	else
		vxerror("getslice")
	esac
	return nil
end

!global proc obj_putslice(object a,b,c,d)=
!	vxerror("putslice")
!end
!
!global function obj_getdotslice(object a,b,c)object d=
!	vxerror("getdotslice")
!	return d
!end
!
!global proc obj_putdotslice(object a,b,c,d)=
!	vxerror("putdotslice")
!end
!
!global function obj_getkeyindex(object a,b,c)object d=
!	vxerror("getkeyindex")
!	return d
!end
!
!global proc obj_putkeyindex(object a,b,c)=
!	vxerror("putkeyindex")
!end
!
!

proc void_unshare(object p)=
end

global function obj_equal(object a,b)int=
!compare objects that must have compatible types
!return 1/0 for equal/not equal

	if a.tag<>b.tag then
		VXERROR("EQUAL/MIXED")
!		return domixed_arith_int(a,b,cast(obj_equal))
	fi

	switch a.tag
	when vt_int then return a.unum.value=b.unum.value
	when vt_real then return (a.unum.xvalue=b.unum.xvalue|1|0)
	when vt_string then
		return string_equal(a,b)
	when vt_list then
		return list_equal(a,b)
	when vt_void then
		return 1
	else
		cpl vartypenames[a.tag]
		vxerror("obj/equal not ready")
	end switch
	return 0
end

global function obj_in(object a,b)int=
	case b.tag
	when vt_list then
		return list_in(a,b)
	when vt_string then
		return string_in(a,b)
	else
		cpl vartypenames[b.tag]
		vxerror("obj/in not ready")
	esac
	return 0
end

global function obj_compare(object a,b)int=
!compare objects that must have compatible types
!return -1/0/1 for less than/equal/greater than

	if a.tag<>b.tag then
		VXERROR("CMP/MIXED")
!		return domixed_arith_int(a,b,cast(obj_equal))
	fi

	switch a.tag
	when vt_int then return int_compare(a,b)
	when vt_real then return real_compare(a,b)
	when vt_string then
		return string_compare(a,b)
!	when vt_list then
!		return list_equal(a,b)
	when vt_void then
		return 0
	else
		cpl vartypenames[a.tag]
		vxerror("obj/compare not ready")
	end switch
	return 0
end

=== var_support.m 19/25 ===
import clib
import mlib

import var_tables

global function nextpoweroftwo(int x)int=
!return next power of 2 >= x

	if x=0 then return 0 fi

	int a:=1
	while a<x do
		a<<:=1
	od
	return a
end

global proc vxerror(ichar mess)=
	println "*** Lib error:",mess
	abortprogram("Stopping")
end

global function getzstring(ichar s, int length)ichar t=
	if length=0 then
		return ""
	fi
	t:=pcm_alloc(length+1)
	memcpy(t,s,length)
	(t+length)^:=0
	return t
end

global function getbinophandler(int opc,s,t)ref void =
!find handler for binary op
!look for function names in this form:
!    tag_opc           eg. int_add				when s=t
!	 taga_tagb_opc     eg. int_real_add			when s,t are different
!	 mixed_opc         eg. mixed_add			try this when no taga/b found

	[32]char name
	ref void fnptr

	strcpy(&.name,vartypenames[s]+3)
	strcat(&.name,"_")
	if t<>s then						!mixed types
		strcat(&.name,vartypenames[t]+3)
		strcat(&.name,"_")
	fi
	strcat(&.name,binopnames[opc]+2)
!CPL "GETBINOP:",&.NAME

	fnptr:=findproc(&.name)
	if fnptr=nil then
		strcpy(&.name,"mixed_")
		strcat(&.name,binopnames[opc]+2)
		fnptr:=findproc(&.name)
	fi

	return fnptr
end

global function getibinophandler(int opc,s,t)ref void =
!find handler for bintp op

	[32]char name
	ref void fnptr

	strcpy(&.name,vartypenames[s]+3)
	strcat(&.name,"_")
	if t<>s then						!mixed types
		strcat(&.name,vartypenames[t]+3)
		strcat(&.name,"_")
	fi
	strcat(&.name,ibinopnames[opc]+2)

!CPL =&.NAME
	fnptr:=findproc(&.name)
!	if fnptr=nil then
!		strcpy(&.name,"mixed_")
!		strcat(&.name,ibinopnames[opc]+2)
!CPL "SECOND TRY",=&.NAME
!		fnptr:=findproc(&.name)
!	fi

	return fnptr
end

global function getmonophandler(int opc,s)ref void =
!find handler for binary op
!look for function names in this form:
!    tag_opc           eg. int_neg

	[32]char name
	ref void fnptr

	strcpy(&.name,vartypenames[s]+3)
	strcat(&.name,"_")
	strcat(&.name,monopnames[opc]+2)

	fnptr:=findproc(&.name)

	return fnptr
end

global function getimonophandler(int opc,s)ref void =
!find handler for binary op
!look for function names in this form:
!    tag_opc           eg. int_neg

	[32]char name
	ref void fnptr

	strcpy(&.name,vartypenames[s]+3)
	strcat(&.name,"_")
	strcat(&.name,imonopnames[opc]+2)

	fnptr:=findproc(&.name)

	return fnptr
end

global function findproc(ichar name)ref void=
	int n
	n:=$get_nprocs()

	for i to n do
		if eqstring($get_procname(i),name) then
			return $get_procaddr(i)
		fi
	od
	return nil
end

global function convtostringz(ref char svalue,int length)ref char =
! a contains a string object which is a NON-zero-terminated string.
! Set up a pointer to a proper zero-terminated one and return that.
! This uses a ring of 3 static string objects it will only work for strings up to
! a certain length, and only if not more than 3 are needed for any single call.

	enum (strbufflen=2000)
	static [0:strbufflen]char strbuffer1
	static [0:strbufflen]char strbuffer2
	static [0:strbufflen]char strbuffer3
!	static [0:strbufflen]char strbuffer4
!	static [0:strbufflen]char strbuffer5
!	static [0:strbufflen]char strbuffer6
	static int strindex=0		!index of current buffer: cycles between 0,1,2
	static [0:]ref [0:]char table=(
		&strbuffer1,&strbuffer2,&strbuffer3)
!		&strbuffer4,&strbuffer5,&strbuffer6)
!	cast(strbuffer1),cast(strbuffer2),cast(strbuffer3),
!	cast(strbuffer4),cast(strbuffer5),cast(strbuffer6))
	ref[0:]char p

	if length>=strbufflen then
		vxerror("Convtostringz>=2000")
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

=== var_strings.m 20/25 ===
import clib
import msys
import mlib

import var_decls
import var_tables
import var_support
import var_objects
import var_numbers

global function string_make(ichar s, int length=-1)object=
!create a ivariant string from given string
!string will be copied to heap
	ref char t

	if s=nil then
		return string_makex(nil,0,0)
	fi

	if length=-1 then
		length:=strlen(s)
	fi

	if length=0 then
		return string_makex(t,0,0)
	else
		t:=pcm_alloc(length)
		memcpy(t,s,length)
		return string_makex(t,length,allocbytes)
	fi
end

global function string_makex(ichar s, int length,allocated)object=
!create a ivariant string from given string
!string is already on the heap
	ref char t
	object p

	if length=-1 then
		length:=strlen(s)
	fi

	p:=obj_new(vt_string)

	if length=0 then
		p^.ustr.strptr:=nil
	else
		p^.ustr.strptr:=s
		p^.ustr.length:=length
		p^.ustr.allocated:=allocated
	fi
	p^.ustr.lower:=1
	p^.ustr.mutable:=1
	p^.ustr.refcount:=1

	return p
end

global proc string_concatto(iobject pa, object b)=
!.refcount<=refc, assume sole owner of string, so do actual in-place append
!Otherwise, create a duplicate first. Or, better, just do 'ADD'.
	int alen,blen,newlen

	object a:=pa^

!CPL "CONCAT TO",PA,A,VARTYPENAMES[A.TAG]
	if not a.ustr.mutable then vxerror("string not mutable") fi

	alen:=a.ustr.length
	blen:=b.ustr.length

	if alen=0 then
		if blen then
			pa^:=obj_share(b)
		else
			pa^:=string_make("",0)
		fi

	elsif blen=0 then		!x+"" use x unchanged
!		return obj_share(a)
	fi

!do actual in-place append

	if blen=1 then					!add single character
		if ++alen>a.ustr.allocated then
			string_resize(a,alen)
		fi
		a.ustr.length:=alen
!		if bint then
!			(a.ustr.strptr+alen-1)^:=b.unumber.value
!		else
			(a.ustr.strptr+alen-1)^:=b.ustr.strptr^
!		fi
	else
		newlen:=alen+blen
		if newlen>a.ustr.allocated then
			string_resize(a,newlen)
		fi
		a.ustr.length:=newlen
		memcpy(a.ustr.strptr+alen,b.ustr.strptr,blen)
	fi
end

global proc string_appendto(iobject pa, object b)=
	string_concatto(pa,b)
end

global proc string_addto(iobject pa, object b)=
	string_concatto(pa,b)
end

global function string_add(object a,b)object c=
	return string_concat(a,b)
end

global function string_append(object a,b)object c=
	return string_concat(a,b)
end

global function string_concat(object a,b)object c=
	int alen,blen,bint
!	object d
	[16]char str

!	d:=nil
!	if b.tag=tint then
!		str[1]:=b.unumber.value
!		str[2]:=0
!		b:=d:=string_make(&.str,1)
!	fi

	alen:=a.ustr.length

	blen:=b.ustr.length

!CPL =A.USTRING.MUTABLE,=A.REFCOUNT

	if alen=0 then
		if blen then
			c:=obj_share(b)
		else
			c:=string_make("",0)
		fi

	elsif blen=0 then		!x+"" use x unchanged
		c:=obj_share(a)
	else 				!x+y: need to do some actual work!
		c:=string_maken(alen+blen)
		memcpy(c.ustr.strptr,a.ustr.strptr,alen)
		memcpy(c.ustr.strptr+alen,b.ustr.strptr,blen)
	fi

!	if d then obj_unshare(d) fi

	return c
end

function string_maken(int length)object=
!create an empty, uninitialised string of given length
	object p

	p:=obj_new(vt_string)

	if length then
		p^.ustr.strptr:=pcm_alloc(length)
	fi
	p^.ustr.mutable:=1
	p^.ustr.length:=length
	p^.ustr.lower:=1
	p^.ustr.allocated:=allocbytes
!	p^.refcount:=1
	return p
end

global function string_getslice(object a, int i,j)object=
	object b

	b:=obj_new(vt_string)
	b^.ustr.objtype:=slice_obj
	b^.ustr.mutable:=a^.ustr.mutable
	b^.ustr.lower:=1

	case a^.ustr.objtype
	when slice_obj then				!slice of a slice!
		b^.ustr.objptr2:=a^.ustr.objptr2		!link to original
		++b^.ustr.objptr2^.refcount
	when extslice_obj then
		b^.ustr.objptr2:=nil
		b^.ustr.objtype:=extslice_obj
	else
		++a^.refcount
		b^.ustr.objptr2:=a				!link to original
	esac
	b^.ustr.strptr:=a^.ustr.strptr+i-1

	b^.ustr.length:=j-i+1

	return b
end

global function string_getindex(object a, int index)object=

	if index<1 or index>a^.ustr.length then
		abortprogram("str/index/bounds")
	fi

	return string_getslice(a,index,index)
end

global function string_getdotindex(object a, int index)object=

	if index<1 or index>a^.ustr.length then
		abortprogram("str/dotindex/bounds")
	fi

	return int_make((a.ustr.strptr+index-1)^)
end

global proc string_putindex(iobject pa, int index,object c)=
!a is a string, b is an int; store c in a
	int ch	
	object a:=pa^

	--index
	if index<0 then
		vxerror("pop/str[int] lwb")
	elsif index>=a.ustr.length then		!don't allow appending
		vxerror("pop/str bounds")
	fi

	case c.tag
	when vt_string then
		if c.ustr.length<1 then vxerror("null str") fi
		ch:=c.ustr.strptr^
	when vt_int then
		ch:=c.unum.value
	else
		vxerror("str[i]:=?")
	esac

	(a.ustr.strptr+index)^:=ch
end

global proc string_putdotindex(iobject pa, int index,object c)=
!a is a string, b is an int; store c in a
	object a:=pa^

	--index
	if index<0 then
		vxerror("popdot/str[int] lwb")
	elsif index>=a.ustr.length then
		vxerror("popdot/str bounds")
	fi

	case c.tag
	when vt_int then
		(a.ustr.strptr+index)^:=c.unum.value
	else
		vxerror("str.[i]:=?")
	esac

end

global proc string_free(object p)=

	case p.ustr.objtype
	when normal_obj then
		if p.ustr.length then
			pcm_free(p.ustr.strptr,p.ustr.allocated)
		fi

	when slice_obj then
		obj_unshare(p.ustr.objptr2)
	when extslice_obj then
	esac

	pcm_free32(p)
end

proc string_unshare(object p)=
CPL "UNSHARE STRING"
	if --p.refcount=0 then
		string_free(p)
	fi
end

global function string_dupl(object p)object q=

	q:=obj_new(vt_void)
!CPL "STRDUPL",=P.REFCOUNT
!CPL "STRDUPL",=Q.REFCOUNT
	q^:=p^
	q.refcount:=1
	q.ulist.mutable:=1

	if p.ustr.length=0 then
		return q
	fi

	q.ustr.strptr:=pcm_alloc(p.ustr.length)
	q.ustr.allocated:=allocbytes

	memcpy(q.ustr.strptr,p.ustr.strptr,p.ustr.length)
	return q
end

global proc string_resize(object p,int n)=
	ref byte q
	int32 allocated

	if n<=p.ustr.allocated then
		p.ustr.length:=n
		return
	fi
!CPL "RESIZE"

	q:=pcm_alloc(n)

	if p.ustr.length then					!copy existing data then delete it
		memcpy(q,p.ustr.strptr,p.ustr.length)
!CPL "ALLOCATE:",P.USTRING.LENGTH
!CPL "FREE",P.USTRING.ALLOCATED
		pcm_free(p.ustr.strptr,p.ustr.allocated)
	fi

	p.ustr.strptr:=cast(q)
	p.ustr.allocated:=allocbytes
	p.ustr.length:=n
end

global function string_equal(object a,b)int=
	if a.ustr.length<>b.ustr.length then return 0 fi
	if a.ustr.length then
		return eqbytes(a.ustr.strptr,b.ustr.strptr,a.ustr.length)
	fi
	return 1				!both empty
end

global function string_int_mul(object a,b)object c=
	int m, oldlen, newlen
	ichar s

	m:=b.unum.value

	if m<0 then
		vxerror("string*neg")
	elsif m=0 then
		return string_make("")
	elsif m=1 then
		return obj_share(a)
	else
		oldlen:=a.ustr.length
		if oldlen then
			newlen:=oldlen*m
			c:=string_maken(newlen)
			s:=c.ustr.strptr
			to m do
				memcpy(s,a.ustr.strptr,oldlen)
				s+:=oldlen
			od

		else
			return string_make("")
		fi
	fi
	return c
end

global function string_compare(object a,b)int=
	int slen:=a.ustr.length
	int tlen:=b.ustr.length
	ref char s:=a.ustr.strptr
	ref char t:=b.ustr.strptr

	if slen=0 then
		if tlen=0 then
			return 0
		else
			return -1
		fi
	elsif tlen=0 then
		return 1
	elsif slen=tlen then
		if slen=1 then
			if s^<t^ then return -1
			elsif s^>t^ then return 1
			else
				return 0
			fi
		fi
		return cmpstringn(s,t,slen)
	else
		return cmpstring(convtostringz(s,slen), convtostringz(t,tlen))
	fi
!RETURN 0
end

global function string_len(object a)int = {a.ustr.length}
global function string_lwb(object a)int = {1}
global function string_upb(object a)int= {a.ustr.length+a.ustr.lower-1}
global function string_bounds(object a)object=
	return range_make(a.ustr.lower, a.ustr.length+a.ustr.lower-1)
end

global function string_notl(object a)int = {a.ustr.length=0}
global function string_istruel(object a)int = {a.ustr.length<>0}

global function string_asc(object a)int =
	if a.ustr.length then
		return a.ustr.strptr^
	fi
	return 0
end

global function string_min(object a,b)object c=
	if string_compare(a,b)<=0 then
		return obj_share(a)
	else
		return obj_share(b)
	fi
end

global function string_max(object a,b)object c=
	if string_compare(a,b)>=0 then
		return obj_share(a)
	else
		return obj_share(b)
	fi
end

global function string_in(object a,b)int =
	int alen,blen,result,i,j,k
	ref char sx, sy

	alen:=a.ustr.length
	blen:=b.ustr.length

	if alen=0 or blen=0 then		!at least one is empty
		return 0
	fi

	k:=blen-alen
	for i:=0 to k do			!all start positions
		sx:=a.ustr.strptr
		sy:=b.ustr.strptr+i
		for j:=1 to alen do			!all chars in y
			if sx^<>sy^  then
				goto nextpos
			fi
			++sx; ++sy
		od
		return i+1
nextpos::
	od
	return 0
end
=== var_lists.m 21/25 ===
import clib
import msys
import mlib
import var_decls
import var_tables
import var_support
import var_numbers
import var_objects

global proc list_free(object p)=
	iobject q

	case p.uarray.objtype
	when normal_obj then
		q:=p.ulist.iobjptr
		to p.ulist.length do
			obj_unshare(q^)
			++q
		od
		if p.tag=vt_list and p.ulist.length then
			pcm_free(p.ulist.iobjptr,p.ulist.allocated*object.bytes)
		elsif p.ulist.length then				!assume record
			pcm_free(p.urec.iobjptr,p.urec.length*object.bytes)
		fi

	when slice_obj then
		obj_unshare(p.ulist.objptr2)
	when extslice_obj then
	esac

	pcm_free32(p)
end

global function list_new(int length,lower=1, object defval=nil)object p=
	iobject q

	p:=obj_new(vt_list)
	p.ulist.mutable:=1
	p.ulist.lower:=lower
	p.ulist.length:=length
	p.ulist.objtype:=normal_obj

	if length then
		q:=p.ulist.iobjptr:=pcm_alloc(length*object.bytes)
		p.ulist.allocated:=allocbytes/object.bytes
		to length do
			if defval then
				q^:=obj_share(defval)
			else
				q^:=&voidobj
			fi
			++q
		od
	fi

	return p
end

global function list_make(iobject a, int length,lower=1)object p=
!create a list of length objects starting from a
!will transfer ownership of objects so caller doesn't need to free them.

!VAR INT SS:=SMALLMEMTOTAL

	p:=obj_new(vt_list)
	p.ulist.mutable:=1
	p.ulist.lower:=lower
	p.ulist.length:=length
	p.ulist.objtype:=normal_obj
	if length then
		p.ulist.iobjptr:=pcm_alloc(length*object.bytes)
		p.ulist.allocated:=allocbytes/object.bytes
		memcpy(p.ulist.iobjptr,a,length*object.bytes)
	fi

	return p
end

global function list_getindex(object a, int index)object=
!a is a list, b is an int; return list[int]
	word offset

	offset:=index-a.ulist.lower
	if offset>=word(a.ulist.length) then
		vxerror("list[int] bounds")
	fi

	return obj_share((a.ulist.iobjptr+offset)^)
end

global proc list_putindex(iobject pa, int index,object c)=
!a is a list, b is an int; store c in a
	word offset
	object a:=pa^,b

	offset:=index-a.ulist.lower
	if offset>=word(a.ulist.length) then
		if offset<0 then
			vxerror("pop/list[int] lwb")
		else
			if offset=a.ulist.length then
				list_appendto(pa,c)
				return
			else
				vxerror("pop/list bounds")
			fi
		fi
	fi

	(a.ulist.iobjptr+offset)^:=c
end

global function list_append(object a,object b)object c=
!do in-place append of b to list a
	int n

	c:=list_dupl(a)
	n:=c.ulist.length+1			!new length

	if n>c.ulist.allocated then		!need more space
		list_resize(c,n)
	else
		c.ulist.length:=n
	fi

	(c.ulist.iobjptr+n-1)^:=obj_share(b)			!set new element

	return c
end

global proc list_appendto(iobject pa, object b)=
!do in-place append of b to list a
	int n
	object a:=pa^

	if a.ulist.objtype<>normal_obj then
		vxerror("Can't extend slice")
	fi

	if not a.ulist.mutable then
		vxerror("list/append not mutable")
	fi

!	if a.refcount>refc then
!		return list_append(pa,b)
!	fi

	n:=a.ulist.length+1			!new length

	if n>a.ulist.allocated then		!need more space
		list_resize(a,n)
	else
		a.ulist.length:=n
	fi

	(a.ulist.iobjptr+n-1)^:=obj_share(b)			!set new element
end

global function list_concat(object a,object b)object c=
!do in-place append of b to list a
	iobject p

	if a.ulist.length=0 then
		return list_dupl(b)

	elsif b.ulist.length=0 then
		return list_dupl(a)

	else								!both non-empty
		c:=list_dupl(a)
		p:=b.ulist.iobjptr
		to b.ulist.length do
			list_appendto(&c,p^)
			++p
		od

		return c
	fi
end

global proc list_concatto(iobject pa, object b)=
!do in-place append of b to list a
	int n
	object a:=pa^
	iobject q

	if a.ulist.objtype<>normal_obj then
		vxerror("Can't extend slice")
	fi

	if not a.ulist.mutable then
		vxerror("list/concat not mutable")
	fi

	q:=b.ulist.iobjptr
	to b.ulist.length do
		list_appendto(pa,q^)
		++q
	od
end

global proc list_resize(object p,int n)=
	iobject q

	if n<=p.ulist.allocated then
		p.ulist.length:=n
	else
		q:=pcm_alloc(n*objectsize)
		if p.ulist.length then
			memcpy(q,p.ulist.iobjptr,p.ulist.length*objectsize)
			pcm_free(p.ulist.iobjptr,p.ulist.allocated*objectsize)
		fi
		p.ulist.iobjptr:=q
		p.ulist.length:=n
		p.ulist.allocated:=allocbytes/objectsize
	fi
end


global function list_dupl(object p)object q=
	ref object plist,qlist

	q:=obj_new(vt_void)
	q^:=p^
	q.refcount:=1
	q.ulist.mutable:=1

	if p.ulist.length=0 then
		return q
	fi

	qlist:=q.ulist.iobjptr:=pcm_alloc(p.ulist.length*object.bytes)
	q.ulist.allocated:=allocbytes/object.bytes	!probably same as p.allocated	
	plist:=p.ulist.iobjptr

	to q.ulist.length do
		qlist^:=obj_dupl(plist^)
		++qlist
		++plist
	od

	return q
end

global function list_mul(object p, int n)object q=
!global proc pc_mul_listi(ivariant a,b,c) =		!PC_MUL_LISTI
!a is a list, b is an int; c::=a*b
!Usually c coincides with a
	iobject r
	int newlength,oldlength

	oldlength:=p.ulist.length
	newlength:=oldlength*n

	if not oldlength then		!duplicating b times has no effect (leave c=a)
		return p
	fi

	if newlength<0 then
		vxerror("mullist 0")
	elsif newlength=0 then
		return list_new(0,1)
	fi

	if oldlength=1 then
		q:=list_new(newlength,p.ulist.lower)
		r:=q.ulist.iobjptr

		to newlength do
			r^:=obj_share(p.ulist.iobjptr^)
			++r
		od

	else
		vxerror("MULLISTINT/COMPLEX")
	fi

	return q
end

global function record_new(object p,pinit)object q=
	iobject r

	q:=obj_new(vt_record)
	q.urec.recobj:=p
	q.urec.length:=p.urec.length
	q.urec.lower:=1

	q.urec.iobjptr:=r:=pcm_alloc(q.urec.length*object.bytes)
	to q.urec.length do
		obj_share(pinit)
		r^:=pinit
		++r
	od
	return q
end

global function record_make(object prec, iobject p, int n)object q=
!prec is a recorddef; p points to n objects on the stack
!construct a record object matching p
	int nfields
	iobject r

VXERROR("RECORD/MAKE")

!	nfields:=prec.urecdef.nfields
!
!	if n>nfields then
!		vxerror("Too many record fields")
!	elsif n<nfields then
!		vxerror("Too few record fields")
!	fi
!
!	q:=obj_new(trecord)
!	q.urec.recobj:=prec
!	q.urec.length:=nfields
!	q.urec.lower:=1
!
!	q.urec.iobjptr:=r:=pcm_alloc(nfields*object.bytes)
!	to nfields do
!!		obj_share(p^)
!		r^:=p^
!		++r
!		++p
!	od

	return q
end

global function list_getslice(object p,int i,j)object q=
	int alower

	alower:=p.ulist.lower

	if i<alower or j>p.ulist.length+alower-1 or i>j then
		vxerror("list/slice bounds")
	fi

	q:=obj_new(vt_list)				!create slice object
	q.ulist.objtype:=slice_obj
	q.ulist.mutable:=p.ulist.mutable
	q.ulist.lower:=1

	case p.ulist.objtype
	when slice_obj then				!slice of a slice!
		q.ulist.objptr2:=p.ulist.objptr2		!link to original
		++q.refcount
	when extslice_obj then
		q.ulist.objptr2:=nil
		q.ulist.objtype:=extslice_obj
	else
		q.ulist.objptr2:=p				!link to original
		++q.ulist.objptr2.refcount
	esac
	q.ulist.iobjptr:=p.ulist.iobjptr+i-alower
	q.ulist.length:=j-i+1

	return q
end

global function list_len(object a)int= {a.ulist.length}
global function list_lwb(object a)int= {a.ulist.lower}
global function list_upb(object a)int= {a.ulist.length+a.ulist.lower-1}
global function list_bounds(object a)object=
	return range_make(a.ulist.lower, a.ulist.length+a.ulist.lower-1)
end

global function list_notl(object a)int={a.ulist.length=0}
global function list_istruel(object a)int={a.ulist.length<>0}

global function list_equal(object a,b)int=
	iobject p,q

	if a.ulist.length<>b.ulist.length then return 0 fi
	if a.ulist.length then
		p:=a.ulist.iobjptr
		q:=b.ulist.iobjptr
		to a.ulist.length do
			if not obj_equal(p^,q^) then return 0 fi
			++p
			++q
		od
		return 1				!all elements match
	else
		return 1				!both empty
	fi
end

global function list_in(object a,b)int=
	iobject p
	int offset,j

	offset:=b.ulist.lower-1			!offset turns 1..n index into lwb..upb
	if offset<0 then vxerror("in/list/lwb?") fi		!lwb not 1 or more

	p:=b.ulist.iobjptr
	for i to b.ulist.length do
		j:=i+offset
		if obj_equal(a,p^) then
			return j
		fi
		++p
	od

	return 0
end

global function list_int_mul(object a,b)object c=
	int m, oldlen, newlen, k, alen
	iobject p

	m:=b.unum.value
	oldlen:=a.ulist.length
	newlen:=oldlen*m

	if oldlen=0 then
		return list_new(0)
	fi

	if newlen<0 then
		vxerror("list*neg?")
	elsif newlen=0 then
		return list_new(0)
	fi

	c:=list_new(newlen)

	p:=a.ulist.iobjptr
	k:=1
	alen:=a.ulist.length

	for i to newlen do
		list_putindex(&c,i,p^)

		++p
		++k
		if k>alen then
			p:=a.ulist.iobjptr
			k:=1
		fi
	od

	return c
end
=== var_decimals.m 22/25 ===
import clib
import msys
import mlib
import mbignum

import var_decls
import var_tables
import var_support
import var_objects
import var_numbers


global function decimal_make(ichar s, int length=-1)object=
!turn string into a decimal bignum, then wrap that up as an object

	if length=-1 then
		length:=strlen(s)
	fi

	return makebnobj(bn_makestr(s,length))
end

global function makebnobj(bignum bn=nil)object c=
	if bn=nil then
		bn:=bn_init()
	fi

	c:=obj_new(vt_decimal)
	c.udec.bnptr:=bn
	return c
end

global function decimal_add(object a,b)object c=
	c:=makebnobj()
	bn_add(c.udec.bnptr,a.udec.bnptr, b.udec.bnptr)
	return c
end

global function decimal_sub(object a,b)object c=
	c:=makebnobj()
	bn_sub(c.udec.bnptr,a.udec.bnptr, b.udec.bnptr)
	return c
end

global function decimal_mul(object a,b)object c=
	c:=makebnobj()
	bn_mul(c.udec.bnptr,a.udec.bnptr, b.udec.bnptr)
	return c
end

global function decimal_div(object a,b)object c=
	c:=makebnobj()
	bn_div(c.udec.bnptr,a.udec.bnptr, b.udec.bnptr)
	return c
end

global function decimal_idiv(object a,b)object c=
	c:=makebnobj()
	bn_idiv(c.udec.bnptr,a.udec.bnptr, b.udec.bnptr)
	return c
end

global function decimal_irem(object a,b)object c=
	c:=makebnobj()
	bn_irem(c.udec.bnptr,a.udec.bnptr, b.udec.bnptr)
	return c
end

global function decimal_shl(object a,b)object c=
	bignum d
	if b.tag<>vt_decimal then vxerror("dec/shl not int") fi
	c:=makebnobj()
	d:=bn_makeint(2**bn_toint(b.udec.bnptr))
	bn_mul(c.udec.bnptr,a.udec.bnptr, d)
	bn_free(d)
	return c
end

global function decimal_shr(object a,b)object c=
	bignum d
	if b.tag<>vt_decimal then vxerror("dec/shr not int") fi
	c:=makebnobj()
	d:=bn_makeint(2**bn_toint(b.udec.bnptr))
	bn_idiv(c.udec.bnptr,a.udec.bnptr, d)
	bn_free(d)
	return c
end

global proc decimal_free(object p)=
!CPL "DECIMAL_FREE"
	bn_free(p.udec.bnptr)
	pcm_free32(p)
end

global function decimal_dupl(object a)object c =
	c:=makebnobj()
	bn_dupl(c.udec.bnptr,a.udec.bnptr)
	return c
end

global function decimal_equal(object a,b)int=
	return bn_equal(a.udec.bnptr,b.udec.bnptr)
end

global function decimal_compare(object a,b)int=
	return bn_cmp(a.udec.bnptr,b.udec.bnptr)
end

global function decimal_notl(object a)int = {bn_iszero(a.udec.bnptr)}
global function decimal_istruel(object a)int = {not bn_iszero(a.udec.bnptr)}

global function decimal_min(object a,b)object c=
	if decimal_compare(a,b)<=0 then
		return obj_share(a)
	else
		return obj_share(b)
	fi
end

global function decimal_max(object a,b)object c=
	if decimal_compare(a,b)>=0 then
		return obj_share(a)
	else
		return obj_share(b)
	fi
end

global function decimal_toint(object a)int=
	return bn_toint(a.udec.bnptr)
end

global function decimal_toreal(object a)real=
	return bn_tofloat(a.udec.bnptr)
end

global function decimal_neg(object a)object c=
	c:=obj_dupl(a)
	bn_negto(c.udec.bnptr)
	return c
end

global function decimal_abs(object a)object c=
	c:=obj_dupl(a)
	bn_absto(c.udec.bnptr)
	return c
end

global proc decimal_addto(iobject pa,object b)=
	bn_add(pa.udec.bnptr,pa.udec.bnptr, b.udec.bnptr)
end

global proc decimal_subto(iobject pa,object b)=
	bn_sub(pa.udec.bnptr,pa.udec.bnptr, b.udec.bnptr)
end

global proc decimal_multo(iobject pa,object b)=
	bn_mul(pa.udec.bnptr,pa.udec.bnptr, b.udec.bnptr)
end

global proc decimal_divto(iobject pa,object b)=
CPL "DIVTO"
	bn_div(pa.udec.bnptr,pa.udec.bnptr, b.udec.bnptr)
end

global proc decimal_idivto(iobject pa,object b)=
	bn_idiv(pa.udec.bnptr,pa.udec.bnptr, b.udec.bnptr)
end

global proc decimal_iremto(iobject pa,object b)=
	bn_irem(pa.udec.bnptr,pa.udec.bnptr, b.udec.bnptr)
end

global proc decimal_negto(iobject pa)=
	bn_negto(pa.udec.bnptr)
end

global proc decimal_absto(iobject pa)=
	bn_absto(pa.udec.bnptr)
end

global proc decimal_incrto(iobject pa)=
	bn_add(pa.udec.bnptr,pa.udec.bnptr,bn_const(1))
end

global proc decimal_decrto(iobject pa)=
	bn_sub(pa.udec.bnptr,pa.udec.bnptr,bn_const(1))
end

=== mbignum.m 23/25 ===
!(Decimal 'bignumber' library for integers and floats)

import clib
import mlib
import oslib

const digitwidth   = 9
const digitbase	= 1000000000
const digitfmt	 = "%09d"
const mdigitfmt	 = "z9"

INT NMAKE
INT NFREE


const digitmax	 = digitbase-1

global type bignum  = ref bignumrec
type elemtype = int32
const elemsize = elemtype.bytes

record bignumrec =
	ref[0:]elemtype num
	int length
	int expon
	int32 neg
	int32 numtype
end

record constrec =
	int64 value
	bignum bnvalue
	ref constrec nextconst
end

!special values for bignum types
tabledata() [0:]ichar fpnames =
	(zero_type = 0,	 $),
	(normal_type,	   $),
	(inf_type,	 	 $),
	(nan_type,	 	 $),
end

!recognised combinations of bignum types (bintypes)
enum (
	nn_types,	 	  ! both numbers (non-zero)
	zz_types,	 	  ! both zero
	ii_types,	 	  ! both infinity
	xx_types,	 	  ! one or both is nan

	nz_types,	 	  ! number/zero
	ni_types,	 	  ! number/infinity

	zn_types,	 	  ! zero/number
	in_types,	 	  ! infinity/number

	zi_types,	 	  ! zero/infinity
	iz_types)	 	  ! infinity/zero

const maxprec	  = 10 million
!int currprec	   = 100/digitwidth
int currprec	   = 300/digitwidth

int stblz	 	 	 !global set by smalltobig

ref constrec constlist=nil	  !use linked list of constant values

global function bn_init()bignum=
	bignum a

	a:=makebignum(0)
	return a
end

function readexpon(ichar s)int=
!s points just after 'e' or 'E'
	int neg, expon
	neg:=expon:=0

	case s^
	when '+' then ++s
	when '-' then neg:=1; ++s
	esac

	doswitch s^
	when '0'..'9' then
		expon:=expon*10+(s^-'0')
		++s
	when '_', '\'', '`', ' ' then
		++s
	when 0 then
		exit
	else
		bn_error("make expon?")
	end doswitch

	return (neg|-expon|expon)
end

global proc bn_print(bignum a,int format=0)=
	ichar s

	s:=bn_tostring(a,format)
	print s
!   free(s)
end

global proc bn_println(bignum a, int format=0)=
	bn_print(a,format)
	println
end

function getbintype(bignum a,b)int=
!return bintype code for combination of a and b
	int atype:=a^.numtype, btype:=b^.numtype

	if atype=nan_type or btype=nan_type then
		return xx_types
	fi

	case atype
	when normal_type then
		case btype
		when normal_type then
	 	   return nn_types
		when zero_type then
	 	   return nz_types
		else
	 	   return ni_types
		esac
	when zero_type then
		case btype
		when normal_type then
	 	   return zn_types
		when zero_type then
	 	   return zz_types
		else
	 	   return zi_types
		esac
	else
		case btype
		when normal_type then
	 	   return in_types
		when zero_type then
	 	   return iz_types
		else
	 	   return ii_types
		esac
	esac

end

function makebignum(int length)bignum=
!ndigits=0 to create a zero value
!these are wide digits
	bignum a

!CPL "MAKEBIGNUM",++NMAKE
	a:=bn_alloc(bignumrec.bytes)
	if length then
		a^.num:=bn_alloc(length*elemsize)
		a^.numtype:=normal_type
	else
		a^.num:=nil
		a^.numtype:=zero_type
	fi
	a^.length:=length
	a^.expon:=0
	a^.neg:=0

	return a
end

function makesmallnum(int length)ref elemtype=
	return bn_alloc(length*elemsize)
end

function smalltobig(bignum c, ref elemtype a, int length,alloc,offset=0)bignum =
!copy numeric data from smallnum into new bignum
!also normalises by removing trailing zeros and leading zeros
!sets up expon with assumption that sequence represents an int
!will also free alloc elemente of a, provided memory is not reused
!offset is to be added to a, when a doesn't point to original allocation

	ref elemtype p
	int leadingzeros, trailingzeros, nonzeros, newlength

	bn_setzero(c)

	p:=a
	leadingzeros:=trailingzeros:=nonzeros:=0
	to length do
		if p++^ then
	 	   nonzeros:=1
	 	   trailingzeros:=0
		else
	 	   if nonzeros then
	 	 	  ++trailingzeros
	 	   else
	 	 	  ++leadingzeros
	 	   fi
		fi
	od

	stblz:=leadingzeros

	if nonzeros then

		newlength:=length-trailingzeros-leadingzeros

		if newlength=length=alloc then	 	 !can use data in a directly
	 	   c^.num:=cast(a)
		else
	 	   c^.num:=cast(makesmallnum(newlength))
	 	   memcpy(c^.num,a+leadingzeros,newlength*elemsize)
	 	   freesmall(a+offset,alloc)
		fi
		c^.length:=newlength
		c^.numtype:=normal_type
		c^.expon:=length-1-leadingzeros	 		!include trailing zeros, but not leading ones?
	elsif alloc then	 	 	 	 	 	 	  !result stays at zero
		freesmall(a+offset,alloc)
	fi

	return c
end

proc freesmall(ref elemtype p, int length)=
	freemem(p,length*elemsize)
end

global function bn_alloc(int size)ref void=
	ref void p

	p:=pcm_alloc(size)
	if p=nil then
		abortprogram("bignum:out of memory")
	fi

	return p
end

global function checkedmalloc(int size)ref void=
	ref void p

	p:=malloc(size)
	if p=nil then
		abortprogram("CM:Out of memory")
	fi

	return p
end

global proc bn_free(bignum a)=
!free digit memory and descriptor
	if a then
!CPL "	FREE BIG NUM",++NFREE
		bn_setzero(a)
		freemem(a,bignumrec.bytes)
	fi
end

proc freemem(ref void p, int size)=
!(my own deallocator needs the size; C's free() doesn't)
!   free(p)
	pcm_free(p,size)
end

global proc bn_setzero(bignum a)=
!clear digit memory only; clear descriptor to a zero number
	if a then
		if a^.num then
	 	   freesmall(cast(a^.num),a^.length)
		fi
		a^.num:=nil
		a^.length:=0
		a^.neg:=0
		a^.expon:=0
		a^.numtype:=zero_type
	fi
end

global proc bn_move(bignum a,b)=
!move contents of b to a. Original value of a is cleared; b becomes zero

bn_setzero(a)
a^:=b^
memset(b,0,bignumrec.bytes)
end

global proc bn_dupl(bignum a,b)=
!copy contents of b to a. Each copy is independent
	bignum c
	int size

!   if a=b then
		c:=bn_init()
		c^:=b^
		if c^.length then
			c^.num:=cast(makesmallnum(size:=c^.length))
			memcpy(c^.num,b^.num, size*elemsize)
		fi
		bn_move(a,c)
		bn_free(c)
!   fi

!   bn_setzero(a)
!   a^:=b^
!   if a^.length then
!	   a^.num:=bn_alloc(a^.length*elemtype.bytes)
!   fi
end

global proc bn_setinf(bignum dest) =
	bn_setzero(dest)
	dest^.numtype:=inf_type
end

global proc bn_setnan(bignum dest) =
	bn_setzero(dest)
	dest^.numtype:=nan_type
end

proc bn_error(ichar mess) =
	print "BN:"
	abortprogram(mess)
end

global function bn_iszero(bignum a)int=
	return a^.numtype=zero_type
end

global proc bn_negto(bignum a)=
	if not bn_iszero(a) then
		a^.neg:=not a^.neg
	fi
end

global proc bn_absto(bignum a)=
	a^.neg:=0
end

global function bn_isint(bignum a)int =
	return a^.length<=a^.expon+1
end

global function bn_getprec(bignum a)int=
	return a^.length*digitwidth
end

global proc bn_setprec(bignum a,int prec)=
	int oldlength,newlength
	bignum c

	if a^.numtype<>normal_type then
		return
	fi

	if prec<1 or prec>maxprec then
		return
	fi

!prec is digit count, not words
	prec:=((prec-1)/digitwidth+1)*digitwidth		!must be multiple of digitwidth

!prec should be rounded up as needed to next multiple of digitwith
	newlength:=prec/digitwidth	 	 	 	   !no. words

	oldlength:=a^.length

!CPL =OLDLENGTH,=NEWLENGTH
!   if oldlength=newlength then
	if oldlength<=newlength then
!   if oldlength>=newlength then
		return
	fi

	c:=makebignum(newlength)
	c^.neg:=a^.neg
	c^.expon:=a^.expon

	for i:=0 to newlength-1 do
		if i<oldlength then
	 	   c^.num^[i]:=a^.num^[i]
		else
	 	   c^.num^[i]:=0
		fi
	od

	bn_move(a,c)
	bn_free(c)
end

global function bn_getglobalprec:int=
	return currprec*digitwidth
end

global proc bn_setglobalprec(int prec)=
	currprec:=((prec-1)/digitwidth+1)
end

global function bn_makeint(int x)bignum =
	bignum a
	[256]char str

	if x=0 then
		a:=makebignum(0)
	elsif x in 0..digitmax then
		a:=makebignum(1)
		a^.num^[0]:=x
	elsif -x in 0..digitmax then
		a:=makebignum(1)
		a^.num^[0]:=-x
		a^.neg:=1
	else
		sprintf(&.str,"%lld",x)
		a:=bn_makestr(&.str)
	fi

	return a
end

global function bn_makefloat(real64 x)bignum =
	bignum a
	[2048]char str

	sprintf(&.str,"%.30g",x)
!   sprintf(&.str,"%.17e",x)

CPL =&.STR

	return bn_makestr(&.str)
end

global proc bn_ipower(bignum d, a,int64 n)=
!return a**b for bigints
	bignum e,f

	if n<0 then
		bn_setzero(d)

	elsif n=0 then
		bn_move(d,bn_makeint(1))

	elsif n=1 then
		bn_dupl(d,a)
!
	elsif (n iand 1)=0 then
		e:=bn_init()
		bn_mulu(e,a,a)
		bn_ipower(d,e,n/2)
		bn_free(e)	  

	else	 	   !assume odd
		e:=bn_init()
		f:=bn_init()
		bn_mulu(e,a,a)
		bn_ipower(f,e,(n-1)/2)
		bn_mulu(d,a,f)
		bn_free(e)
		bn_free(f)

	fi
end

function smallsubto(ref elemtype p,q, int plen, qlen)int=
!subtract q from p, return new length. New p will be moved up if smaller
!p>=q, and plen>=qlen
	ref elemtype pp,qq
	int carry,diff,z

	pp:=p+plen-1
	qq:=q+qlen-1
	carry:=0
	z:=0	 	 	 	 !leading zeros

	to plen do
		if qq>=q then
	 	   diff:=pp^-qq^-carry
	 	   --qq
		else
	 	   diff:=pp^-carry
		fi

		if diff<0 then
	 	   carry:=1
	 	   pp^:=diff+digitbase
		else
	 	   pp^:=diff
	 	   carry:=0
		fi
		if pp^ then
	 	   z:=0
		else
	 	   ++z
		fi
		--pp
	od
	if carry then bn_error("SSUBTO/CARRY?") fi

	if z=plen then --z fi	 	  !result is zero, needs at least one digit

	if z then
		plen-:=z
		pp:=p
		qq:=p+z
		to plen do
	 	   pp++^:=qq++^
		od
	fi

	return plen
end

function smallmulto(ref elemtype p,q, int plen, m)int=
!multiply bignum sequence p inplace, by single digit m
!return new length (will be plen or plen+1, unless result is zero)
!p must be long enough to store the extra digit

	ref elemtype pp,qq
	int carry,d

	case m
	when 0 then
		p^:=0
		return 1
	when 1 then
		memcpy(p,q,plen*elemsize)
		return plen
	esac

	pp:=p+plen-1
	qq:=q+plen-1
	carry:=0

	to plen do
		d:=int64(qq^)*m+carry
		pp^:=d rem digitbase
		carry:=d/digitbase
		--qq
		--pp
	od

	if carry then	 	 	 !need extra digit
		pp:=p+plen
		to plen do
	 	   pp^:=(pp-1)^
	 	   --pp
		od
		pp^:=carry
		++plen
	fi

	return plen
end

global function bn_equal(bignum a,b)int=
	if a^.length<>b^.length or \
	   a^.numtype<>b^.numtype or \
	   a^.neg<>b^.neg or \
	   a^.expon<>b^.expon then
		return 0
	fi

	if a^.length=0 then return 1 fi

	return eqbytes(a^.num,b^.num,a^.length*elemsize)
end

global proc bn_addu(bignum dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry,expona,exponb
	int dc
	word j
	ref[0:]elemtype pa,pb
	ref elemtype pax,pbx
	ref elemtype c,c2

	if a^.expon<b^.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
	fi

	expona:=a^.expon
	exponb:=b^.expon
	preca:=a^.length
	precb:=b^.length

	offset:=expona-exponb	 	  !for indexing B elements shift to match A
	uppera:=preca-1
	upperb:=precb-1

	if uppera>(upperb+offset) then  !A defines overall precision; B contained within A
		upperc:=uppera
	else	 	 	 	 		!B extends overall precision
		upperc:=upperb+offset
	fi
	precc:=upperc+1

	c:=makesmallnum(precc)	 	 !no space for carry
	carry:=0
	pa:=a^.num
	pb:=b^.num

	for i:=upperc downto 0 do	 	  !do the add, starting from ls digit

		j:=i-offset	 	 	 	  !index of A/C in terms of B
		if i<=uppera and j<=word(upperb) then
	 	   dc:=pa^[i]+pb^[j]+carry
		elsif i<=uppera then
	 	   dc:=pa^[i]+carry
		elsif j<=word(upperb) then
	 	   dc:=pb^[j]+carry
		else
	 	   dc:=carry
		fi

		if dc>=digitbase then
	 	   carry:=1
	 	   (c+i)^:=dc-digitbase
		else
	 	   (c+i)^:=dc
	 	   carry:=0
		fi
	od

	if carry then
		c2:=makesmallnum(precc+1)
		c2^:=carry
		memcpy(c2+1,c,precc*elemsize)
		freesmall(c,precc)
		c:=c2
		++precc
	fi

	smalltobig(dest,c,precc,precc)

	dest^.expon:=expona+carry
end

proc bn_subu(bignum dest,a,b)=
	int preca, precb, precc
	int uppera,upperb,upperc, offset, carry, expona
	int da,db,dc, isneg, z, newprec,diff
	word j
	ref[0:]elemtype pa,pb
	ref elemtype c

!can only do subtract when a>=b; do some basic checks
	isneg:=0
	if a^.expon<b^.expon then	   !A has definite smaller magnitude
		swap(a,b)	 	 	 	!make sure A is always bigger or (approx) equal
		isneg:=1
	fi

!know that a>=b, and that isneg might be true
retry::
	expona:=a^.expon
	preca:=a^.length
	precb:=b^.length

	offset:=expona-b^.expon	 	!for indexing B elements shift to match A
	uppera:=preca-1
	upperb:=precb-1

	if uppera>(upperb+offset) then  !A defines overall precision; B contained within A
		upperc:=uppera
	else	 	 	 	 		!B extends overall precision
		upperc:=upperb+offset
	fi
	precc:=upperc+1

	c:=makesmallnum(precc)
	carry:=0
	pa:=a^.num
	pb:=b^.num

	for i:=upperc downto 0 do	 	  !do the add, starting from ls digit
		j:=i-offset	 	 	 	  !index of A/C in terms of B
		if i<=uppera and j<=word(upperb) then

	 	   diff:=pa^[i]-pb^[j]-carry
		elsif i<=uppera then
	 	   diff:=pa^[i]-carry
		elsif j<=word(upperb) then
	 	   diff:=-pb^[j]-carry
		else
	 	   diff:=-carry
		fi

		if diff<0 then
	 	   carry:=1
	 	   (c+i)^:=diff+digitbase
		else
	 	   (c+i)^:=diff
	 	   carry:=0
		fi
		
	od

	if carry then
		if isneg then	 	  !already swapped
	 	   bn_error("SUBU/CARRY")
		fi
		swap(a,b)
		isneg:=1
		freesmall(c,precc)
		goto retry
	fi

	smalltobig(dest,c,precc,precc)
	dest^.neg:=isneg
	dest^.expon:=expona-stblz

end

proc bn_mulu(bignum dest, a,b) =
!unsigned multiply, c:=a*b
!general scheme A1/B1 are least significant words
!x is total overflows (product overflow and carry) from previous column

!(A4 A3 A2 A1) * (B3 B2 B1)
!
!0	 0	 x	 A4.B1 A3.B1 A2.B1 A1.B1
!0	 x	 A4.B2 A3.B2 A2.B2 A1.B2 0
!x	 A4.B3 A3.B3 A2.B3 A1.B3 0	 0

	int uppera, upperb, upperc
	int precc,expona,exponb
	int ax,bx,cx		!indices within a,b,c
	int i,cx1, nc2
	i64 p,carry,x
	bignum d
	ref elemtype c
	i64 pdquot,pdrem

	expona:=a^.expon
	exponb:=b^.expon
	uppera:=a^.length-1
	upperb:=b^.length-1

	precc:=uppera+upperb+2
	nc2:=precc

	c:=makesmallnum(nc2)
	memset(c,0,precc*elemsize)
!   c^.expon:=a^.expon+b^.expon+1
	cx:=precc-1

	for bx:=upperb downto 0 do
		carry:=0

		cx1:=cx
		for ax:=uppera downto 0 do
	 	   p:=i64((a^.num^[ax]))*i64((b^.num^[bx]))+carry
	 	   pdquot:=p/digitbase
!	 	  x:=int(c^.num^[cx1])+p rem digitbase
	 	   x:=int64((c+cx1)^)+p rem digitbase
	 	   if x>digitmax then
	 	 	  carry:=pdquot+x/digitbase
!	 	 	 c^.num^[cx1--]:=x rem digitbase
	 	 	  (c+cx1--)^:=x rem digitbase
	 	   else
	 	 	  carry:=pdquot
!	 	 	 c^.num^[cx1--]:=x
	 	 	  (c+cx1--)^:=x
	 	   fi

		od
		(c+cx1)^:=carry
		--cx	 	 	  !for next row, start at next column in dest
	od

	smalltobig(dest,c,precc,nc2)
	dest^.expon:=expona+exponb+1-stblz

end

function smalldiv(ref elemtype x, b, int &xlen, nb)elemtype =
!x,b are smallnums: arrays of elements, of the exact lengths given
!x is same length as b, or at most one element longer
!(x can also be smaller, but then result is just 0)
!return integer x/b as machine word type 0..digitmax
!when digits are 0..9, then result of x/b is always going to be 0 to 9.

	int k,count
	int64 xx,y
	elemtype xi,bi
	ref elemtype e
	int esize,ne,nx

	nx:=xlen
	k:=0
	count:=0
	e:=makesmallnum(esize:=(nb+1))

	do
		if nx<nb then	 	 	 !completed this k
	 	   exit
		elsif nx>nb then	 	   !x will be at most 1 digit wider than b
	 	   xx:=int64(x^)*digitbase+int64((x+1)^)
	 	   y:=xx/(b^+1)
		else	 	 	 	 	 	   !x,b are same length
	 	   if x^>=(b^+1) then
	 	 	  y:=x^/(b^+1)
	 	   else
	 	 	  y:=1
	 	 	  for i:=0 to nb-1 do
	 	 	 	 xi:=(x+i)^
	 	 	 	 bi:=(b+i)^
	 	 	 	 if xi<bi then
	 	 	 	 	y:=0
	 	 	 	 	exit all
	 	 	 	 elsif xi>bi then
	 	 	 	 	exit
	 	 	 	 fi
	 	 	  od

	 	   fi
		fi
		k+:=y
		if y>1 then
	 	   ne:=smallmulto(e,b,nb,y)
	 	   nx:=smallsubto(x,e,nx,ne)
		elsif y then
	 	   nx:=smallsubto(x,b,nx,nb)
		else
	 	   BN_ERROR("smalldiv:Y=0")
		fi
	od

	freesmall(e,esize)
	xlen:=nx	 	 	 	 !return modified x, and new length of x
	return k
end

global proc bn_idivu(bignum dest,a,b,rm=nil)=
!neither a nor b are zero; both are positive
!integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper
	int uppera, upperb, upperc
	int n, k, nexta
	int64 xx,y
	ref elemtype pa,pb

	na:=a^.length
	nb:=b^.length
	expona:=a^.expon
	exponb:=b^.expon
	badjust:=exponb+1-nb

	if na>expona+1 or nb>exponb+1 then
		bn_error("idivu:a or b not int")
	fi
	nc:=expona+1

	if expona<exponb then
		bn_setzero(dest)
		if  rm then
	 	   bn_dupl(rm,a)
		fi
		return
	fi

	uppera:=na-1
	upperb:=nb-1
	upperc:=nc-1
	pa:=cast(a^.num)
	pb:=cast(b^.num)	 	   !p is not zero, and all digits of interest are present

!x is the moving and changing window into a that b is divided into get next digit of result
!use a permanently allocated smallnum, 1 digit wider than b
	n:=nb	 	 	 	!n is also how many digits of a we're into so far
	x:=makesmallnum(nx2:=n+1)	   !allow one extra digit
	nx:=n	 	 	 	 	   !current x size
	nupper:=nc-badjust

	for i:=0 to upperb do
		if i<=uppera then
	 	   (x+i)^:=(pa+i)^
		else
	 	   (x+i)^:=0
		fi
	od

	c:=makesmallnum(nc)
	cx:=0

	do
		k:=smalldiv(x,pb,nx,nb)

		(c+cx++)^:=k
		if n>=nupper then	 	 	 	!finished with A 
	 	   exit
		fi

		nexta:=(n>uppera|0|(pa+n)^)
		++n
		if nx=1 and x^=0 then
	 	   x^:=nexta	 	 	 !x is 1 digit long
		else
	 	   (x+nx)^:=nexta	 	 !next digit from a
	 	   ++nx
		fi
	od

	if rm and exponb<nb then		!no trailing zeros in b
		smalltobig(rm,x,nx,nx2)
	else
		freesmall(x,nx2)
	fi

	if cx=1 and c^=0 then
		freesmall(c,nc)
		bn_setzero(dest)
		if rm then
	 	   bn_dupl(rm,a)
		fi
		return
	fi

	if c^=0 and cx>=2 then	 	 	!leading zero (may not need cx check)
		smalltobig(dest,c+1,cx-1,nc,-1)
	else
		smalltobig(dest,c,cx,nc)
	fi
!   freesmall(c,nc)

	if rm and exponb>=nb then	 	  !has trailing zeros so natural rem doesn't work
		bignum d
		d:=bn_init()
		bn_mulu(d,b,dest)
		bn_subu(rm,a,d)
		bn_free(d)
	fi

end

function strvaln(ref char s,int n)int=	  !STRVALN
!convert first n chars of s to int value and return result will fit into 32 bits
	int a

	a:=0
	to n do
		if s^<>'_' then
	 	   a:=a*10+s^-'0'
		fi
		++s
	od
	return a
end

global function bn_makestr(ichar s, int length=0)bignum=
	ichar t,u
	int neg,dpindex,expon,nonzeros,talloc,dpseen
	int leadingzeros, trailingzeros,zerosafterdp
	int d,n,wd,dp,wdp,w,d2,na,nb
	bignum a

	if length=0 then
		length:=strlen(s)
	fi
	if length<=0 then
		return badnumber()
	fi
	talloc:=length+1+10	 	!allow for extending last wdigit group

	neg:=0
	case s^
	when '+' then ++s
	when '-' then neg:=1; ++s
	esac

	t:=u:=bn_alloc(talloc)	  !accummulate sig digits into t
	dpindex:=-1
	dpseen:=zerosafterdp:=0
	nonzeros:=0
	leadingzeros:=trailingzeros:=0
	expon:=0

	doswitch s^
	when '1'..'9' then
		u++^:=s++^
		trailingzeros:=0
		nonzeros:=1
	when '0' then
		if nonzeros then
	 	   ++trailingzeros
	 	   u++^:=s++^
		else
	 	   ++leadingzeros
	 	   if dpseen then
	 	 	  ++zerosafterdp
	 	   fi
	 	   ++s
		fi
	when '_', '\'', '`', ' ',13,10 then
		++s
	when '.' then
		if dpseen or dpindex>=0 then return badnumber() fi
		if nonzeros then
	 	   dpindex:=u-t
		else
	 	   dpseen:=1
		fi
!	   trailingzeros:=0
		++s
	when 0 then
		exit
	when 'e','E' then
		expon:=readexpon(s+1)
		exit
	else
		return badnumber()
	end doswitch

	u^:=0
	length:=u-t	 	 	   !new length of extracted digits
	if dpindex<0 then
		if dpseen then
	 	   dpindex:=-zerosafterdp
		else
	 	   dpindex:=length
		fi
	fi
	length-:=trailingzeros	  !adjust precision to ignore trailing zeros
	(t+length)^:=0

	if length=0 then
		return bn_makeint(0)
	fi

	d:=dpindex-1+expon
	n:=length
	dp:=0
	na:=1
	nb:=n-na

	w:=digitwidth

	if d>=0 then
		wd:=d/w
		wdp:=d rem w
	else
		d2:=abs(d+1)
		wd:=-(d2/w+1)
		wdp:=w-1-(d2 rem w)
	fi

	na:=wdp+1
	nb:=max(n-na,0)
	while nb rem w do ++nb od
	length:=nb/w+1
	u:=t+n
	to na+nb-n do
		u++^:='0'
	od
	n:=na+nb
	(t+n)^:=0

	a:=makebignum(length)
	a^.neg:=neg
	a^.expon:=wd
	u:=t
	a^.num^[0]:=strvaln(u,na)
	u+:=na
	
	for i:=1 to length-1 do
		a^.num^[i]:=strvaln(u,w)
		u+:=w
	od

	freemem(t,talloc)

	return a
end

proc bn_fdivu(bignum dest,a,b,int precision)=
!neither a nor b are zero; both are positive
!integer divide

	ref elemtype c,x,e
	int expona, exponb, badjust, exponc
	int na,nb,nc,nx,ne,nx2,ne2, cx,nupper,nc2
	int uppera, upperb, upperc
	int n, k, nexta
	int64 xx,y
	ref elemtype pa,pb

	na:=a^.length
	nb:=b^.length
	expona:=a^.expon
	exponb:=b^.expon

	if precision then
		precision:=((precision-1)/digitwidth+1)	 	!must be multiple of digitwidth
	else
		precision:=currprec
	fi
	nc:=precision

	uppera:=na-1
	upperb:=nb-1
	upperc:=nc-1
	pa:=cast(a^.num)
	pb:=cast(b^.num)	 	   !p is not zero, and all digits of interest are present

!x is the moving and changing window into a that b is divided into get next digit of result
!use a permanently allocated smallnum, 1 digit wider than b
	n:=nb	 	 	 	!n is also how many digits of a we're into so far
	x:=makesmallnum(nx2:=n+1)	   !allow one extra digit
	nx:=n	 	 	 	 	   !current x size

	for i:=0 to upperb do
		if i<=uppera then
	 	   (x+i)^:=(pa+i)^
		else
	 	   (x+i)^:=0
		fi
	od

	c:=makesmallnum(nc2:=nc+1)
	cx:=0

	do
		k:=smalldiv(x,pb,nx,nb)

		(c+cx++)^:=k

		if cx>nc then	 	 	 !reached given precision
	 	   exit
		fi

		nexta:=(n>uppera|0|(pa+n)^)
		++n
		if nx=1 and x^=0 then
	 	   x^:=nexta	 	 	 !x is 1 digit long
		else
	 	   (x+nx)^:=nexta	 	 !next digit from a
	 	   ++nx
		fi
	od

	freesmall(x,nx2)

	if cx=1 and c^=0 then
		freesmall(c,nc2)
		bn_setzero(dest)
		return
	fi

	if c^=0 and cx>=2 then	 	 	!leading zero (may not need cx check)
		smalltobig(dest,c+1,cx-1,nc2,-1)
		dest^.expon:=expona-exponb-1
	else
		smalltobig(dest,c,cx,nc2)
		dest^.expon:=expona-exponb
	fi
!   freesmall(c,nc2)
end

function tostring_float(bignum a,int fmt)ichar=
!a is an actual number (not zero, infinity etc)
	int expon,upper,nchars,w,prel,n,showdot
	ichar s,t

	expon:=a^.expon
	upper:=a^.length-1

	if fmt='I' and bn_isint(a) then
		showdot:=0
	else
		showdot:=1
	fi

	w:=digitwidth
	nchars:=3	 	 	 !sign and trailing .0
	if expon<0 then
		nchars+:=abs(expon-1)*w
	fi
	nchars+:=a^.length*w
	if expon-upper>0 then
		nchars+:=(expon-upper)*w
	fi
	nchars+:=8	 	 		!margin

!   s:=t:=bn_alloc(nchars)
	s:=t:=checkedmalloc(nchars)
	
	if a^.neg then
		t++^:='-'
	fi

	prel:=0
	if expon<0 then
		prel:=1
		t++^:='0'
		t++^:='.'
		to abs(expon)-1 do
	 	   to digitwidth do
	 	 	  t++^:='0'
	 	   od
		od
	fi

	for i:=0 to upper do
!	   t++^:='*'
		n:=sprintf(t,(i>0 or prel|digitfmt|"%d"),a^.num^[i])
		t+:=n
!	   print a^.num^[i]
		if expon=i and i<upper and showdot then
	 	   t++^:='.'
		fi
	od

	to expon-upper do
!print "+"
		to digitwidth do
	 	   t++^:='0'
		od
	od
	if expon>=upper and showdot then
		t++^:='.'
		t++^:='0'
	fi

	t^:=0
	return s
end

global function bn_tostring(bignum a,int fmt=0)ichar=
	int expon,upper
	ichar s,t

	t:=nil
	if a=nil then
		t:="<void>"
	else
		case a^.numtype
		when zero_type then t:=(fmt='E' or fmt='F'|"0.0"|"0")
		when inf_type then t:="<inf>"
		when nan_type then t:="<nan>"
		esac
	fi

	if t then
		s:=checkedmalloc(strlen(t)+1)
		strcpy(s,t)
		return s
	fi

	if fmt=0 or fmt='A' then
		if bn_isint(a) and (a^.expon-a^.length)*digitwidth<60 then
	 	   fmt:='I'
		elsif abs(a^.expon*digitwidth)<60 then
	 	   fmt:='F'
		else
	 	   fmt:='E'
		fi
	fi

	if fmt='E' then
		s:=tostring_scient(a)
	else
		s:=tostring_float(a,fmt)
	fi
	return s
end

function tostring_scient(bignum a)ichar=
!a is an actual number
	ichar s,t
	int expon,nchars,n,shift
	int64 x,scale

	nchars:=3

	expon:=a^.expon*digitwidth

	x:=a^.num^[0]
	scale:=1
	shift:=0
	while x>=10 do
		x:=x/10
		scale*:=10
		++expon
		++shift
	od

	nchars:=a^.length*digitwidth+16	 !allow for 1., and exponent

	s:=t:=checkedmalloc(nchars)

	if a^.neg then
		t++^:='-'
	fi

!	n:=sprintf(t,"%d.",x)
	print @t,x,,"."
	t+:=strlen(t)

	if shift then
!		n:=sprintf(t,"%0*d", shift, a^.num^[0]-x*scale)
		print @t, shift:"v",,a^.num^[0]-x*scale:"z*"
		t+:=strlen(t)
	fi

	for i to a^.length-1 do
!		n:=sprintf(t,digitfmt, a^.num^[i])
!		fprint @t,digitfmt, a^.num^[i]
		print @t,a^.num^[i]:mdigitfmt
		t+:=strlen(t)
	od

	while (t-1)^='0' and (t-2)^<>'.' do
		--t
	od

!	n:=sprintf(t,"e%d", expon)
	print @t,"e",,expon
	t+:=strlen(t)
	t^:=0

	return s
end

global function bn_add(bignum dest,a,b)int=
	int nega,negb

	switch getbintype(a,b)
	when nn_types then
	when zz_types then
		bn_setzero(dest)
		return 1
	when nz_types then
		bn_dupl(dest,a)
		return 1
	when zn_types then
		bn_dupl(dest,b)
		return 1
	else
		bn_setnan(dest)
		return 0
	end switch

	nega:=a^.neg
	negb:=b^.neg

	if not nega and not negb then	   !both positive
		bn_addu(dest,a,b)
	elsif nega and negb then	 	   !both negative
		bn_addu(dest,a,b)
		bn_negto(dest)
	elsif not nega and negb then		!a positive, b negative
		bn_subu(dest,a,b)
	else
		bn_subu(dest,b,a)	 	 	 !a negative, b positive
	fi

	return 1
end

global function bn_sub(bignum dest,a,b)int=
	int nega,negb

	switch getbintype(a,b)
	when nn_types then
	when zz_types then
		bn_setzero(dest)
		return 1
	when nz_types then
		bn_dupl(dest,a)
		return 1
	when zn_types then
		bn_dupl(dest,b)
		bn_negto(dest)
		return 1
	else
		bn_setnan(dest)
		return 0
	end switch

	nega:=a^.neg
	negb:=b^.neg

	if not nega and not negb then	   !both positive
		bn_subu(dest,a,b)
	elsif nega and negb then	 	   !both negative
		bn_subu(dest,b,a)
	elsif not nega and negb then		!a positive, b negative
		bn_addu(dest,a,b)
	else	 	 	 	 	 	   !a negative, b positive
		bn_subu(dest,b,a)
	fi

	return 1
end

global function bn_mul(bignum dest,a,b)int=
	int neg

	switch getbintype(a,b)
	when nn_types then
	when zz_types,nz_types,zn_types then
		bn_setzero(dest)
		return 1
	else
		bn_setnan(dest)
		return 0
	end switch

	neg:=a^.neg<>b^.neg
	bn_mulu(dest,a,b)
	if neg then	 !different signs
		bn_negto(dest)
	fi
	return 1
end

global function bn_mulp(bignum dest,a,b, int prec)int=
	int res:=bn_mul(dest,a,b)
	if res then
		bn_setprec(dest,(prec=0|currprec|prec))
	fi
	return res
end

global function bn_div(bignum dest,a,b,int prec=0)int=
	int neg

	switch getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_setzero(dest)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		return 0
	else
		bn_setnan(dest)
		return 0
	end switch

	neg:=a^.neg<>b^.neg

	bn_fdivu(dest,a,b,prec)
!   bn_idivu(dest,a,b)

	if neg then
		bn_negto(dest)
	fi
	return 1
end

global function bn_idiv(bignum dest,a,b)int=
	int neg
	switch getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_setzero(dest)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		return 0
	else
		bn_setnan(dest)
		return 0
	end switch

	neg:=a^.neg<>b^.neg
	bn_idivu(dest,a,b)
	if neg then
		bn_negto(dest)
	fi
	return 1
end

global function bn_idivrem(bignum dest,rm,a,b)int=
	int nega,negb

	switch getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_setzero(dest)
		bn_setzero(rm)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		bn_setzero(rm)
		return 0
	else
		bn_setnan(dest)
		return 0
	end switch

	nega:=a^.neg
	negb:=b^.neg
	bn_idivu(dest,a,b,rm)
	if nega<>negb then	  !different signs
		bn_negto(dest)
	fi
	if nega then bn_negto(rm) fi
	return 1
end

global function bn_irem(bignum dest,a,b)int=
	bignum rm,d
	int nega

	switch getbintype(a,b)
	when nn_types then
	when zn_types then
		bn_dupl(dest,b)
		return 1
	when zz_types,nz_types then
		bn_setinf(dest)
		bn_setzero(dest)
		return 0
	else
		bn_setnan(dest)
		return 0
	end switch

	nega:=a^.neg
	d:=bn_init()
	bn_idivu(d,a,b,dest)
	if nega then bn_negto(dest) fi
	bn_free(d)
	return 1
end

global function bn_cmp(bignum a,b)int=
	bignum d
	int neg

	if bn_equal(a,b) then
		return 0
	fi

	d:=bn_init()
	bn_sub(d,a,b)
	neg:=d^.neg
	bn_free(d)
	return (neg|-1|1)
end

global function bn_const(int value)bignum =
	ref constrec p
	bignum c

	p:=constlist

	while p do
		if p^.value=value then
	 	   return p^.bnvalue
		fi
		p:=p^.nextconst
	od

!not encountered before
	p:=bn_alloc(constrec.bytes)
	p^.bnvalue:=bn_makeint(value)
	p^.value:=value
	p^.nextconst:=constlist
	constlist:=p
	return p^.bnvalue
end

global function bn_sign(bignum a)int=
	if bn_iszero(a) then
		return 0
	elsif a^.neg then
		return -1
	else
		return 0
	fi
end

function badnumber:bignum=
	bignum c
	c:=makebignum(0)
	c^.numtype:=nan_type
	return c
end

global function bn_digits(bignum a)int=
!return number of digits in integer a
	int n
	[32]char str

	if not bn_isint(a) then
		return 0
	fi
	if bn_iszero(a) then
		return 1
	fi

	n:=sprintf(&.str,"%d",a^.num^[0])
	return n+a^.expon*digitwidth
end

global function bn_toint(bignum a)int64=
	int64 x
	if not bn_isint(a) then
		return 0
	fi
	if bn_iszero(a) then
		return 0
	fi

	x:=0
	for i:=0 to a^.length-1 do
		x:=x*digitbase+a^.num^[i]
	od

	if a^.neg then
		return -x
	else
		return x
	fi
end

global function bn_tofloat(bignum a)real64=
	real64 x
	ichar s

	if bn_iszero(a) then
		return 0.0
	fi

	s:=bn_tostring(a,'E')

	sscanf(s,"%lf", &x)
	return x
end

global proc bn_fix(bignum c, a) =
	if bn_iszero(a) or a^.expon<0 then
		bn_setzero(c)
		return
	fi

	bn_dupl(c,a)
	if not bn_isint(c) then
		bn_setprec(c,(c^.expon+1))
	fi
end
=== var_arrays.m 24/25 ===
import clib
import msys
import mlib

import var_decls
import var_tables
import var_support
import var_objects
import var_numbers

global proc array_free(object p)=
	ref byte q
	int elemsize

	case p.uarray.objtype
	when normal_obj then
		q:=p.uarray.ptr
		elemsize:=ttsize[p.uarray.elemtype]

		if p.uarray.length then
			pcm_free(q,p.uarray.allocated*elemsize)
		fi
	when slice_obj then
		obj_unshare(p.uarray.objptr2)
	when extslice_obj then
	esac

	pcm_free32(p)
end

global function array_new(int tp, length,lower)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int elemsize

CPL "array new",tp
	elemsize:=ttsize[tp]

	p:=obj_new(vt_array)
	p.uarray.mutable:=1
	p.uarray.lower:=lower
	p.uarray.length:=length
	p.uarray.objtype:=normal_obj
	p.uarray.elemtype:=tp

	if length then
		q:=p.uarray.ptr:=pcm_allocz(length*elemsize)
		p.uarray.allocated:=allocbytes/elemsize

!FOR I TO LENGTH DO
!	Q^:=I
!	++Q
!OD

	fi

	return p
end

global function array_getindex(object a, int index)object=
!a is a list, b is an int; return list[int]
	word offset
	int elemtype
	ref byte q

!CPL "ARRAY/GETINDEX",=INDEX, =A.UARRAY.LENGTH,=A.UARRAY.LOWER
	elemtype:=a.uarray.elemtype
	q:=a.uarray.ptr

	offset:=index-a.uarray.lower
!CPL =OFFSET,INT@(OFFSET),=INDEX-
	if offset>=word(a.uarray.length) then
		vxerror("array[int] bounds")
	fi

	return int_make(getpackptr(q,offset,elemtype))
end

!global function array_getindexref_int(object a, int index)object=
!!a is a array, b is an int; return array[int]
!	word offset,elemsize
!	ref byte q
!
!	offset:=index-a.uarray.lower
!	if offset>=word(a.uarray.length) then
!		vxerror("array[int] bounds")
!	fi
!
!	q:=a.uarray.ptr
!	elemsize:=packtypesizes[a.uarray.elemtype]
!
!	return refpack_make(q+offset*elemsize,a.uarray.elemtype,1,a)
!end

global proc array_putindex(object a, int index,object c)=
!a is the array, b is an int; store c in a
	word offset
	int value
	ref byte q

!CPL "LPI1"

	offset:=index-a.uarray.lower
	if offset>=word(a.uarray.length) then
		if offset<0 then
			vxerror("pop/array[int] lwb")
		else
			if offset=a.uarray.length then
VXERROR("ARRAY APPEND")
!				array_iappend(a,c)
				return
			else
				vxerror("pop/array bounds")
			fi
		fi
	fi
!CPL "LPIX",=A.IOBJPTR,OFFSET
	if c.tag<>vt_int then
		vxerror("putarray/not int")
	fi
	value:=c.unum.value
	q:=a.uarray.ptr

	putpackptr(q,offset,a.uarray.elemtype,value)
end

function getpackptr(ref void p, int offset,tp)int=
!extract the packed value pointer to by p, according to tp

CPL "GETPACKPTR",OFFSET,PACKTYPENAMES[TP]
	switch tp
	when pt_u8 then
		return (ref byte(p)+offset)^
	when pt_i32 then
		return (ref int32(p)+offset)^
	when pt_i64 then
		return (ref int64(p)+offset)^
	else
		vxerror("getpackptr/tp?")
		return 0
	endswitch
end

proc putpackptr(ref void p, int offset,tp, value)=
!store value as a packed value at location pointer to by p, according to tp


	switch tp
	when pt_u8 then
		(ref byte(p)+offset)^:=value
	when pt_i32 then
		(ref int32(p)+offset)^:=value
	when pt_i64 then
		(ref int64(p)+offset)^:=value
	else
CPL =TP
		vxerror("putpackptr/tp?")
	endswitch
end

global function array_append(object a,b)object c=
!do in-place append of b to array a
	int n

	c:=array_dupl(a)

	n:=c.uarray.length+1			!new length

	if n>c.uarray.allocated then		!need more space
		array_resize(a,n)
	else
		c.uarray.length:=n
	fi

	if b.tag<>vt_int then
		vxerror("appendarray/not int")
	fi

	putpackptr(c.uarray.ptr,n-1,c.uarray.elemtype,b.unum.value)

	return c
end

global function array_iappend(object a,b, int refc)object c=
!do in-place append of b to array a
	int n

	if a.refcount>refc then
		return array_append(a,b)
	fi

	if a.uarray.objtype<>normal_obj then
		vxerror("Can't extend array slice")
	fi

	if not a.uarray.mutable then
		vxerror("array/append not mutable")
	fi

	n:=a.uarray.length+1			!new length

	if n>a.uarray.allocated then		!need more space
		array_resize(a,n)
	else
		a.uarray.length:=n
	fi

	if b.tag<>vt_int then
		vxerror("appendarray/not int")
	fi

	putpackptr(a.uarray.ptr,n-1,a.uarray.elemtype,b.unum.value)
	return obj_share(a)
end

global proc array_resize(object p,int n)=
	ref byte q
	word32 allocated
	int elemsize

	elemsize:=packtypesizes[p.uarray.elemtype]

	if n<=p.uarray.allocated then
		p.uarray.length:=n
	else
		q:=pcm_alloc(n*elemsize)
		p.uarray.allocated:=allocbytes/elemsize
		if p.uarray.length then
			memcpy(q,p.uarray.ptr,p.uarray.length*elemsize)
		fi
		p.uarray.ptr:=q
		p.uarray.length:=n
	fi
end

!global function refpack_getptr(object p)object q=
!	word value
!
!	if p.tag<>trefpack then vxerror("getptr") fi
!
!	value:=getpackptr(p.urefpack.ptr,0,p.urefpack.target)
!
!!convert return value into an object
!	switch p.urefpack.target
!	when pt_i8..pt_i64, pt_u8..pt_u32 then
!		return int_make(value)
!	when pt_r64 then
!		return real_make(real@(value))
!	else
!		vxerror("refpack/getptr?")
!	endswitch
!	return nil
!end
!
!global proc refpack_putptr(object p,x)=
!	word value
!
!	if p.tag<>trefpack then vxerror("putptr") fi
!
!!CPL "REFPACK PUT",P.UREFPACK.MUTABLE
!
!	case x.tag
!	when vt_int then
!		value:=x.unum.value
!	when vt_real then
!		value:=int@(x.unum.xvalue)
!	else
!		vxerror("refpack/putptr?")
!	esac
!
!	if not p.urefpack.mutable then
!		vxerror("putptr - not mutable")
!	fi
!
!	putpackptr(p.urefpack.ptr,0,p.urefpack.target,value)
!end

global function array_dupl(object p)object q=
	int elemsize

	q:=obj_new(vt_void)
	q^:=p^
	q.refcount:=1
	q.uarray.mutable:=1

	elemsize:=packtypesizes[p.uarray.elemtype]

	if p.uarray.length=0 then
		return q
	fi

	q.uarray.ptr:=pcm_alloc(p.uarray.length*elemsize)
	q.uarray.allocated:=allocbytes/elemsize

	memcpy(q.uarray.ptr, p.uarray.ptr, p.uarray.length*elemsize)
	return q
end

global function array_len(object a)int= {a.uarray.length}
global function array_lwb(object a)int= {a.uarray.lower}
global function array_upb(object a)int= {a.uarray.length+a.uarray.lower-1}
global function array_bounds(object a)object=
	return range_make(a.uarray.lower, a.uarray.length+a.uarray.lower-1)
end

global function array_notl(object a)int={a.uarray.length=0}
global function array_istruel(object a)int={a.uarray.length<>0}

=== var_print.m 25/25 ===
import clib
import msys
import mlib
import mbignum

import var_decls
import var_tables
import var_support
import var_objects
import var_strings
import var_arrays

const maxlistdepth=4
int listdepth=0		!recursive nesting levels for lists/records

global proc obj_print(object p,ichar sfmt=nil)=
	[0:360]char str
	object q
!	ichar sfmt
	fmtrec format
	ref fmtrec fmt

!CPL "PRINT OBJ"

!	sfmt:=objgetfmt(pfmt)

	if sfmt=nil then
		if needgap then
			printstr_n(" ",1)
		else
			needgap:=1
		fi
	else
		nextfmtchars(0)
	fi

	q:=string_make("")
	listdepth:=0

!	switch p.tag
!	when tstring then
!		if pfmt.tag=tvoid then
!			printstr_n(cast(p.strptr),p.length)
!			return
!		fi
!	when tint, treal, trange, tword then	! small numeric types: use local string
!		q:=str_make("")
!		pch_tostr(q,p,fmtstr)
!		printstr_n(q.strptr,q.length)
!		str_free(q)
!		return
!	endswitch

	q:=string_make("")
!	strdest:=q

	pch_tostr(p,sfmt)

!	printstr_n(q.ustring.strptr,q.ustring.length)

	string_free(q)
end

proc list_print(object p, ichar fmt)=
	iobject q
	printstr("(")

	if p.ulist.lower<>1 then
		printstr(strint(p.ulist.lower))
		printstr(":")
	fi

	q:=p.ulist.iobjptr

	for i to p.ulist.length do
		pch_tostr(q^,fmt)
		++q
		if i<p.ulist.length then printstr(",") fi
	od

	printstr(")")

end

proc pch_tostr(object p,ichar fmt)=
	[256]char str

!	q:=str_make("")
!	listdepth:=0

!CPL "PCHTOSTR",TTNAME[P.TAG]

	switch p.tag
	when vt_int then
		print @&.str,p.unum.value:fmt

	when vt_real then
		print @&.str,p.unum.xvalue:fmt

	when vt_void then
		printstr_n("Void",-1)
		return

	when vt_string then
		printstr_n(p.ustr.strptr,p.ustr.length)
		return

!	when vt_proc then
!		proc_print(p)
!
	when vt_list then
		list_print(p,fmt)
		return

	when vt_array then
		array_print(p,fmt)
		return

	when vt_range then
		print @&.str,p.urange.lower,,"..",,p.urange.upper

	when vt_record then
		printstr("Record")
!		list_print(p,fmt)
		return

!	when vt_type then
!		printstr("<")
!		printstr(ttname[p.unumber.value])
!		printstr(">")

	when vt_struct then
!!		sprintf(&.str,"(Struct:%s*%d)",p.ustruct.structdef.ustructdef.name,
!!			p.ustruct.structdef.ustructdef.nfields)
!		fprint &.str,"(Struct:#*#)",p.ustruct.structdef.ustructdef.name,
!			p.ustruct.structdef.ustructdef.nfields
!
		printstr("<STRUCT>")
		return

	when vt_decimal then
		decimal_print(p,fmt)
		return

	else
		PRINTSTR("(TAG ")
		PRINTSTR(VARTYPENAMES[P.TAG])
		PRINTSTR(")")
		RETURN
	endswitch

	printstr_n(&.str)
!	addstring(&.str,-1)
end

proc array_print(object p, ichar fmt)=
	object q
	[32]char str
	int lower, upper

	printstr("A(")

	lower:=p.uarray.lower
	upper:=p.uarray.length+lower-1

!	q:=p.uarray.ptr

	for i:=lower to upper do
		q:=array_getindex(p,i)
		pch_tostr(q,fmt)
		obj_free(q)
		if i<upper then printstr(",") fi
	od

	printstr(")")

end

proc decimal_print(object p, ichar fmt)=
	ichar s
	s:=bn_tostring(p.udec.bnptr,0)
	printstr(s)
	free(s)
end

=== end ===
