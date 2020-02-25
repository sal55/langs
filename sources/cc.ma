mafile 73
  1 cc.m               17893     3169   0
  2 msysnew.m          46919    21086   0
  3 clibnew.m           3397    68029   0
  4 mlib.m             26695    71447   0
  5 oswindows.m        12536    98168   0
  6 cc_decls.m         10364   110729   0
  7 cc_tables.m        25838   121119   0
  8 cc_support.m        6293   146984   0
  9 cc_lex.m           72446   153300   0
 10 cc_headers.m        4675   225774   0
 11 cc_lib.m           35435   230473   0
 12 cc_parse.m         81164   265934   0
 13 cc_genmcl.m        11307   347125   0
 14 cc_libmcl.m        34475   358459   0
 15 cc_blockmcl.m      61618   392963   0
 16 cc_genasm.m        28426   454608   0
 17 cc_export.m         5903   483061   0
 18 cc_assembler.m      6162   488994   0
 19 ax_tables.m        12658   495183   0
 20 ax_decls.m          5554   507867   0
 21 ax_lex.m           13275   513445   0
 22 ax_parse.m          8887   526746   0
 23 ax_lib.m           16275   535657   0
 24 ax_genss.m         41023   551958   0
 25 ax_objdecls.m       2566   593010   0
 26 ax_writeexe.m      24274   595605   0
 27 ax_disasm.m        26006   619906   0
 28 ax_writeobj.m       7418   645941   0
 29 bcclib.asm          2765   653385   1
 30 assert.h              67   656174   1
 31 ctype.h              372   656264   1
 32 errno.h             1736   656659   1
 33 fenv.h               338   658417   1
 34 float.h             1511   658778   1
 35 inttypes.h          2016   660315   1
 36 stdint.h            1227   662355   1
 37 limits.h             660   663606   1
 38 locale.h             707   664290   1
 39 _ansi.h               22   665020   1
 40 math.h              1227   665064   1
 41 setjmp.h             364   666315   1
 42 signal.h             455   666703   1
 43 stdarg.h             340   667182   1
 44 stdbool.h             91   667547   1
 45 stddef.h             368   667662   1
 46 stdio.h             3032   668053   1
 47 stdlib.h            1381   671109   1
 48 _syslist.h            25   672516   1
 49 string.h            1513   672565   1
 50 time.h               903   674100   1
 51 utime.h               48   675026   1
 52 unistd.h              23   675098   1
 53 safelib.h             24   675146   1
 54 wchar.h              518   675193   1
 55 wctype.h              23   675735   1
 56 types.h              175   675781   1
 57 stat.h              1235   675978   1
 58 timeb.h              284   677236   1
 59 utime.h              128   677543   1
 60 memory.h              21   677695   1
 61 windows.h          92016   677741   1
 62 fcntl.h             1106   769780   1
 63 io.h                1216   770906   1
 64 direct.h             735   772146   1
 65 process.h            621   772906   1
 66 malloc.h              99   773551   1
 67 bcc.h                236   773671   1
 68 conio.h              353   773930   1
 69 winsock2.h          4255   774309   1
 70 _mingw.h            4027   778588   1
 71 windowsx.h           241   782641   1
 72 help.txt            1308   782906   1
 73 info.txt            2162   784238   1
=== cc.m 1/73 ===
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
!import cc_headersx
import cc_genmcl
import cc_libmcl
import cc_genasm
import cc_export

import cc_assembler

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
!var byte fcallback

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
	(bcclib_sw,		"bcclib"),
	(callback_sw,	"callback"),
	(entry_sw,		"entry"),
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

starttiming()

initdata()

getinputoptions()

if fdebugcompiler then
	debugcompile()
	stop
fi

if cc_mode>=link_mode then
	fastasm:=1
fi

initsearchdirs()
initlogfile()

nextmodule:=1
pass:=1

repeat
	compilemodules(nextmodule,ninputfiles,pass++)

	nextmodule:=addnewmodules()

until nextmodule=0

if cc_mode>=link_mode then
	do_genlink()
fi

if cc_mode=run_mode then
	do_runprog()
fi

if fshowtiming then showtiming() fi
if fmheaders then writemheader(inputfiles[1]) fi

if fatfile then
	writeatfile()
fi
!CPL =NGOTO
!CPL =NBREAK
!CPL =NBLOCKS
!CPL =NCOMPOUNDBLOCKS
!CPL =NCOMPOUNDBLOCKS/REAL(NBLOCKS)

stop 0
end

proc compilemodules(int a,b,pass)=
[256]char str
ichar ext

if destfileext=nil then
	if cc_mode=preprocess_mode then
		ext:="i"
	elsif not fautomodules then
		ext:="asm"
		if ninputfiles=1 and cc_mode>=link_mode then
			ext:=linkoption
		fi
	else
		ext:="asm"
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
			println "Compiling",inputfiles[m],"to",changeext(inputfiles[m],"asm")
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
		do_genmcl(m)
		do_genasm(m)
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

CPL "DEBUG COMPILE 123"

initsearchdirs()

logdest:=2
initlogfile()

do_loadmodule(1)

do_parsemodule(1)
	showast(1)

do_genmcl(1)
	showmcl("MCL",1)

do_genasm(1)
	showasm(1)

showsttree("ST",1)

!do_genlink()

!CPL =NGOTO
!CPL =NBREAK
!CPL =NBREAKSW
!CPL =NGOTO
!CPL =NBREAK
!CPL =NBLOCKS
!CPL =NCOMPOUNDBLOCKS
!CPL =NCOMPOUNDBLOCKS/REAL(NBLOCKS)


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

proc do_genmcl(int n)=
	codegen_mcl(n)
end

proc do_genasm(int n)=
	ichar outfileasm

	outfileasm:=pcm_copyheapstring(changeext(inputfiles[n],"asm"))

	codegen_writeasm(n,outfileasm)
end

proc do_runprog=
!CPL "RUN",DESTFILENAME

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
m.stmodule^.attribs.ax_moduleno:=nmodules

if nmodules>=maxmodule then
	loaderror("Too many modules %s",modulename)
fi

return nmodules
end

proc initlogfile=			!INITLOGFILE

case logdest
when 2 then
	remove(logfile)
	logdev:=cast(fopen(logfile,"w"))
when 0,1 then
	logdev:=ref void(os_getstdout())
esac
end

proc closelogfile=			!CLOSELOGFILE
[100]char str
int pos

if logdest=2 then
	fclose(cast(logdev))

!	sprintf(&.str,"\\m\\med.bat %s",logfile)
!CPL "CLOSE1"
	print @&.str,"\\m\\med.bat",logfile

!CPL "CLOSE2",&.STR
	if checkfile("cc.m") then
!CPL "CLOSE3",&.STR
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
if dointheaders=0 then
	searchdirs[++nsearchdirs]:="/cx/headers/"
!	searchdirs[++nsearchdirs]:="/cx2/headers/"
fi

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

proc showast(int n)=		!SHOWAST
if logdest then
	if logdest=2 then
		println @logdev			!make sure pos is not 0 (will stop displaying if first in file)
	fi

	printcode(logdev,"PROC AST",n)
	println @logdev
fi
end

proc showstflat(ichar caption)=		!SHOWSTFLAT
if logdest then
	if logdest=2 then
!!		stflat_tx:=last_tx:=ftell(logdev)
	fi
	println @logdev,"PROC",caption
	printstflat(logdev)
	println @logdev
fi
end

proc showsttree(ichar caption,int n)=		!SHOWSTTREE
if logdest then
	if logdest=2 then
!		sttree_tx:=last_tx:=ftell(logdev)
	fi
	println @logdev,"PROC",caption
	printst(logdev,moduletable[n].stmodule)
	println @logdev
fi
end

proc showmcl(ichar caption,int n)=			!SHOWPCL
ref strbuffer mclstr

if logdest then
	mclstr:=writemclcode(caption,n)
	gs_println(mclstr,logdev)
fi
end

proc showasm(int n)=
ichar asmstr,caption

caption:="PROC ASSEMBLY LISTING"

asmstr:=moduletable[n].asmstr
if asmstr=nil then
	return
fi

if logdest then
	println @logdev,caption,n
	println @logdev,asmstr
fi
end

proc showfiles=
int i
println "Sourcefiles:"

for i:=1 to nsourcefiles do
	cpl i,":",sourcefilepaths[i],sourcefilenames[i],"Size:",sourcefilesizes[i]
od
println
end

proc starttiming=
int t
progstart:=os_clock()
end

proc showtiming=
showlps("Program",os_clock()-progstart)
end

global proc showlps(ichar caption,int t)= !non-global gives odd error with lccwin)
[256]char str
int lps,txi
real tx,lpsx

txi:=t
if t>1 then
	lpsx:=(NALLLINES/real(t))
else
	lps:=lpsx:=0
fi

println nalllines,"Lines"

!sprintf(&.str,"%18s: %4d ms  %8dK Lines per second\n",caption,int32(txi),int32(lpsx))

fprintln "#: # ms  #dK Lines per second",caption:"18",txi:"4", int(lpsx):"8"

print &.str
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
!		recase pm_extra
		goto doextra
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
when compile_mode then ext:="asm"
else ext:=linkoption
esac

if destfilename=nil then
	destfilename:=pcm_copyheapstring(changeext(inputfiles[1],ext))
!	destfileext:=ext
elsif eqstring(destfileext,"") then
	destfileext:=ext
fi

!CPL =DESTFILENAME
!CPL =DESTFILEEXT

if fmheaders and ninputfiles>1 then
	loaderror("-mheaders works on one file only")
fi

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

!when compile_sw then
!	cc_mode:=compile_mode

when compile_sw then
	linkoption:="obj"
	cc_mode:=link_mode

!	cc_mode:=compile_mode

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

when automodules_sw then
	fautomodules:=1

when out_sw then
!	destfilename:=pcm_copyheapstring(extractbasefile(value))
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

when callback_sw then
	fcallback:=1

when entry_sw then
	entrypointname:=pcm_copyheapstring(value)

esac
end

proc showincludepaths=
int i
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
static ichar infotext=\
strinclude "info.txt"

println infotext

stop 24
end

proc showcaption=
println "BCC 'C' Compiler",$date,$time
end

proc do_genlink=
[maxmodule]ichar asmfiles
[maxmodule]ichar dllfiles
[maxmodule]ichar assemsources
ichar exefile,ext
int status, nfiles

nfiles:=ninputfiles
for i:=1 to ninputfiles do
	asmfiles[i]:=pcm_copyheapstring(changeext(inputfiles[i],"asm"))
	if fastasm then
		assemsources[i]:=moduletable[i].asmstr
	fi
od

if not eqstring(linkoption,"obj") or fbcclib then
	++nfiles
	asmfiles[nfiles]:="bcclib.asm"
	if fastasm then
		assemsources[nfiles]:=getbcclib()
	fi
fi
for i:=1 to nlibfiles do
	dllfiles[i]:=pcm_copyheapstring(libfiles[i])
od

if destfileext=nil then
	ext:=linkoption
else
	ext:=destfileext
fi
exefile:=pcm_copyheapstring(changeext(destfilename, ext))

if not assembler(exefile,&asmfiles,&dllfiles,
			nfiles,nlibfiles,eqstring(linkoption,"obj"),
			fautomodules or ninputfiles>1, (fastasm|&assemsources|nil),
			entrypointname) then
	println "Couldn't assemble or link"
	stop 1
fi
end

proc resetcompiler=
!println "Reset compiler"
freehashtable()

ntypes:=ntypesreset
nstrings:=nreals:=ndints:=0
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

!pcm_clearmem(&sourcefiletext,sourcefiletext.bytes)
!pcm_clearmem(&sourcefilesizes,sourcefilesizes.bytes)
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

proc showcompilerstate=
ref strec d

println "========= COMPILER DATA AFTER ONE MODULE ========="

cpl =ntypes
cpl =nmodules
cpl =nautomodules
cpl =nsourcefiles
cpl =nsearchdirs
!cpl =ndlltable
!cpl =ndllproctable
cpl =gethashtablesize()
cpl =nstrings
cpl =nreals
cpl =ndints
cpl =stprogram
cpl =stmodule
cpl =currblockno
cpl =blocklevel
cpl =nextblockno
cpl =autotypeno
cpl =nextafindex

!for i:=0 to hstmask do
!	d:=&hashtable[i]
!	if d^.name then
!if d^.symbol=namesym then
!		cpl i,d^.name,symbolnames[d^.symbol],namenames[d^.nameid]
!fi
!	fi
!od

!os_getch()

CPL
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
=== msysnew.m 2/73 ===
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
=== clibnew.m 3/73 ===
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
=== mlib.m 4/73 ===
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
=== oswindows.m 5/73 ===
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
=== cc_decls.m 6/73 ===
import clib
import cc_tables


!export
global type unit = ref unitrec
!endexport

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
		ref void mclcode
		ref void opnd
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
		int32 index			!case label index
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
!global ref strec stsysmodule	!optional sys module (needed for name resolving)
!global ref strec alldeflist		!link together all (user) symbols

global filehandle logdev		!dest for diagnostics and output of tables
global int logdest=0
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
global int fcallback=0		!1 to make all functions callbacks
!global ichar assemsource=nil
!global [maxmodule]ichar assemsources

global tokenrec lx				!provides access to current token data
global tokenrec nextlx

!global int tlex=0		!timing

global int debug=0

!global const maxdlllib=50
!global const maxdllproc=500

!global int ndlltable
!global int ndllproctable
!global [maxdlllib]ichar dlltable
!global [maxdlllib]word64 dllinsttable
!global [maxdllproc]dllprocrec dllproctable

!global const int maxcmdparam=32
!global int ncmdparams
!global [0..maxcmdparam]ichar cmdparamtable

!global ref procrec proclist			!linked list of all procs
!global int nproclist
global int NALLLINES
global int NLOOKUPS
global int NKEYWORDS

!global const hstsize	= 262144
!global const hstsize	= 131072
!global const hstsize	= 32768
global int hstsize	= 16384
!global int hstsize	= 256
!global int hstsize	= 8
!global const hstsize	= 262144
!global const hstmask	= hstsize-1

global int hstmask				!filled in with hstsize-1

global ref[0:]ref strec hashtable

global const maxblock=2100,maxblockstack=100
global [0..maxblock]int32 blockowner
global [0..maxblock]int32 blockcounts
global [0..maxblockstack]int32 blockstack
global int currblockno,nextblockno,blocklevel
global ref strec currproc

!global int progstart,progend

global int labelno=0
global const maxnestedloops=100

global int dointheaders=1				!allow internal std headers
!global int dointheaders=0
global ichar dheaderfile=nil			!result of -d:file.h switch

global int structpadding=1
global int callbackflag=0

global int slineno,sfileno

global ichar oemname="BCC"

GLOBAL INT NFORMATS
GLOBAL INT NGOTO
GLOBAL INT NBREAK
GLOBAL INT NBREAKSW
GLOBAL INT NBLOCKS
GLOBAL INT NCOMPOUNDBLOCKS
=== cc_tables.m 7/73 ===
global tabledata() [0:]ichar stdtypenames, [0:]byte stdtypewidths,
		 [0:]byte stdtypesigned, [0:]byte stdexpandtypes,
		 [0:]byte stdtypecat, [0:]ichar stdtypemnames =
	(tnone=0,		"none",		0,	0,	0,		0,		""),		!error or not set or not used
	(tvoid,			"void",		0,	0,	0,		0,		"void"),

	(tschar,		"schar",	8,	1,	tsint,	'I',	"i8"),		! This ordering is important
	(tsshort,		"short",	16,	1,	tsint,	'I',	"i16"),	!
	(tsint,			"int",		32,	1,	0,		'I',	"i32"),	!
	(tsllong,		"llong",	64,	1,	0,		'I',	"i64"),	!

	(tbool,			"bool",		8,	0,	tuint,	'U',	"byte"),		! As is this
	(tuchar,		"uchar",	8,	0,	tuint,	'U',	"byte"),		!
	(tushort,		"ushort",	16,	0,	tuint,	'U',	"u16"),	!
	(tuint,			"uint",		32,	0,	0,		'U',	"u32"),	!
	(tullong,		"ullong",	64,	0,	0,		'U',	"u64"),	!

	(tfloat,		"float",	32,	0,	0,		'R',	"r32"),	! And tfloat must be >= integer types
	(tdouble,		"double",	64,	0,	0,		'R',	"r64"),	!
	(tldouble,		"ldouble",	128,0,	0,		'R',	"r64"),	!

	(tcomplex,		"complex",	128,0,	0,		0,		""),	!

	(tenum,			"enum",		0,	0,	0,		0,		""),		!
	(tref,			"ref",		64,	0,	0,		0,		""),	! 
	(tproc,			"proc",		64,	0,	0,		0,		""),	!
	(tlabel,		"label",	64,	0,	0,		0,		""),	!

	(tarray,		"array",	0,	0,	0,		0,		""),		!
	(tstruct,		"struct",	0,	0,	0,		0,		""),		!
	(tunion,		"union",	0,	0,	0,		0,		""),		!

!User-defined types go here
	(tlast,			$,			0,	0,	0,		0,		"")		! 	!

end

global const tfirstnum=tschar, tlastnum=tldouble
global const tfirstint=tschar, tlastint=tullong
global const tfirstreal=tfloat, tlastreal=tldouble

global const tptroffset = tsllong		!for 64-bit target

global tabledata() []ichar typespecnames, []int32 typespectypes, []byte typespecsizes =
	(ts_void,		$,	tvoid,		0),
!	(ts_char,		$,	tuchar,		1),
	(ts_char,		$,	tschar,		1),
	(ts_short,		$,	0,			2),
	(ts_long,		$,	0,			4),
	(ts_int,		$,	tsint,		4),
	(ts_float,		$,	tfloat,		4),
	(ts_double,		$,	tdouble,	8),
	(ts_signed,		$,	0,			0),
	(ts_unsigned,	$,	0,			0),
	(ts_bool,		$,	tbool,		1),
	(ts_complex,	$,	tcomplex,	0),
	(ts_user,		$,	0,			0),
	(ts_struct,		$,	0,			0),
	(ts_union,		$,	0,			0),
	(ts_enum,		$,	0,			4),
	(ts_atomic,		$,	0,			0)
end

global tabledata() [0:]ichar pmflagnames=
	(pm_normal=0,		$),		! Normal param
	(pm_notset,			$),		! ()     (applied to one dummy tnone param)
	(pm_empty,			$),		! (void) (applied to one dummy tnone param)
	(pm_variadic,		$)		! (...) or (t,u,v,...) (applied to dummy or first param)
end

!scope here refers to linkage across modules
global tabledata() [0:]ichar scopenames=
	(no_scope=0,		"-"),		! 
	(function_scope,	"Fn"),		!within a function (note import/exported names can be declared in a block scope)
	(local_scope,		"Loc"),		!file-scope/not exported 
	(imported_scope,	"Imp"),		!imported from another module
	(exported_scope,	"Exp")		!file-scope/exported
end

!Call conventions
global tabledata() []ichar ccnames=

	(open_cc=0,		$), ! Not set: either own or clang, depending on whether fn was defined
	(own_cc,		$), ! Internal (x86/x64)
	(clang_cc,		$), ! External (all x64; clang only x86)
	(stdcall_cc,	$), ! (x86 non-clang)
	(callback_cc,	$), ! Internal when called from External

	(dummy_cc,		$)	! 
end

global tabledata() [0:]ichar linkagenames=
	(none_ss=0,		$),
	(static_ss,		$),
	(auto_ss,		$),
	(register_ss,	$),
	(extern_ss,		$),
	(typedef_ss,	$)
end

global tabledata() []ichar typequalnames=
	(const_qual,	$),
	(volatile_qual,	$),
	(restrict_qual,	$),
	(atomic_qual,	$)
end

global tabledata() []ichar fnspecnames=
	(inline_fnspec,		$),
	(noreturn_fnspec,	$),
	(callback_fnspec,	$),
end

global tabledata() =
	(pdm_date),
	(pdm_time),
	(pdm_file),
	(pdm_line),
	(pdm_func),
	(pdm_cdecl),
	(pdm_bcc),
	(pdm_stdc)
end

global tabledata() [0:]ichar jtagnames=

	(j_none=0,		$), !
	(j_const,		$), !
	(j_null,		$), !
	(j_name,		$), !
!	(j_nameaddr,	$), !
	(j_widenmem,	$), !
	(j_funcname,	$), !
	(j_block,		$), !
	(j_tempdecl,	$), !
	(j_decl,		$), !
!	(j_typeof,		$), !
!	(j_makeref,		$), !

!Statements

	(j_callproc,	$), ! 
	(j_return,		$), ! 
	(j_returnx,		$), ! 

	(j_assign,		$), ! 
	(j_if,			$), ! 
	(j_for,			$), ! 
	(j_while,		$), ! 
	(j_dowhile,		$), ! 
	(j_goto,		$), ! 
	(j_labelstmt,	$), ! 
	(j_casestmt,	$), ! 
	(j_defaultstmt,	$), ! 
	(j_break,		$), ! [
	(j_continue,	$), ! [
	(j_switch,		$), ! 
	(j_breaksw,		$), ! [
!	(j_eval,		$), ! 

!Expressions and Operators

!Logical Operators

	(j_andl,		"&& andl"), ! 
	(j_orl,			"|| orl"), ! 
	(j_notl,		"! notl"), ! 
	(j_istruel,		$), ! 

!Expressions and Operators

	(j_makelist,	$), ! 
	(j_exprlist,	$), ! 

!	(j_assignx,		$), ! 
	(j_callfn,		$), ! 
	(j_ifx,			$), ! 

!Binary Ops

	(j_andand,		"&&"), ! a 

	(j_eq,			"=="), ! a 
	(j_ne,			"!="), ! a 
	(j_lt,			"<"), ! a 
	(j_le,			"<="), ! a 
	(j_gt,			">"), ! a 
	(j_ge,			">="), ! a 

	(j_add,			"+ add"), ! 
	(j_sub,			"- sub"), ! 
	(j_mul,			"* mul"), ! 
	(j_div,			"/ div"), ! 
	(j_rem,			"% mod"), ! 
	(j_iand,		"& iand"), ! 
	(j_ior,			"| ior"), ! 
	(j_ixor,		"^ ixor"), ! 
	(j_shl,			"<<"), ! a 
	(j_shr,			">>"), ! a 

	(j_dot,			$), ! 
	(j_idot,		$), ! 
!	(j_dotref,		$), ! 
	(j_index,		$), ! 

	(j_ptr,			"ptr"), ! 
!	(j_ptroffset,	"ptroffset *"), ! 
	(j_addptr,		"addptr"), ! 
	(j_subptr,		"subptr"), ! 
	(j_addrof,		"addrof &"), ! 
	(j_convert,		$), ! 
	(j_scale,		$), ! 

!Monadic Ops

	(j_neg,			"- neg"), ! 
	(j_abs,			"abs"), ! 
	(j_inot,		"~ inot"), ! a

!In-place operators

	(j_addto,		"+="), ! a b	a+:=b
	(j_subto,		"-="), ! a b
	(j_multo,		"*="), ! a b
	(j_divto,		"/="), ! a b
	(j_remto,		"%="), ! a b
	(j_iandto,		"&="), ! a b
	(j_iorto,		"|="), ! a b
	(j_ixorto,		"^="), ! a b
	(j_shlto,		"<<="), ! a b
	(j_shrto,		">>="), ! a b

	(j_sqrt,		"sqrt"), ! a

	(j_preincr,		"++ preincr"), ! a	++a
	(j_predecr,		"-- preincr"), ! a	--a
	(j_postincr,	"++ postincr"), ! a	a++
	(j_postdecr,	"-- postdecr"), ! a	a--

	(j_cputime,		"cputime"), ! a	a--

	(j_dummy,		$)
end

global tabledata() []ichar symbolnames, []ichar shortsymbolnames, []byte symboltojtag=

!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$,	"",		0),			! Lex error
	(dotsym,			$,	".",	j_dot),		! "."
	(idotsym,			$,	"->",	j_idot),	! "->"
	(lexhashsym,		$,	"#",	0),			! "#" as first symbol on line
	(hashsym,			$,	"#",	0),			! "#" within macro def
	(lithashsym,		$,	"#",	0),			! "#" literal hash (not stringify op)
	(hashhashsym,		$,	"##",	0),			! "##" within macro def
	(commasym,			$,	",",	0),			! ","
	(semisym,			$,	";",	0),			! ";"
	(colonsym,			$,	":",	0),			! ":"
	(assignsym,			$,	"=",	j_assign),	! =
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
	(addsym,			$,	"+",	j_add),		! +
	(subsym,			$,	"-",	j_sub),		!
	(mulsym,			$,	"*",	j_mul),		!
	(divsym,			$,	"/",	j_div),		!
	(remsym,			$,	"%",	j_rem),		!
	(iorsym,			$,	"|",	j_ior),		!
	(iandsym,			$,	"&",	j_iand),	!
	(ixorsym,			$,	"^",	j_ixor),	!
	(orlsym,			$,	"||",	j_orl),		!
	(andlsym,			$,	"&&",	j_andl),	!
	(shlsym,			$,	"<<",	j_shl),		!
	(shrsym,			$,	">>",	j_shr),		!
	(inotsym,			$,	"~",	j_inot),	!
	(notlsym,			$,	"!",	j_notl),	!
	(incrsym,			$,	"++",	j_preincr),	!
	(decrsym,			$,	"--",	j_predecr),	!
	(abssym,			$,	"abs",	j_abs),		!

	(eqsym,				$,	"==",	j_eq),		!
	(nesym,				$,	"!=",	j_ne),		!
	(ltsym,				$,	"<",	j_lt),		!
	(lesym,				$,	"<=",	j_le),		!
	(gesym,				$,	">=",	j_ge),		!
	(gtsym,				$,	">",	j_gt),		!

	(addtosym,			$,	"+=",	j_addto),	!
	(subtosym,			$,	"-=",	j_subto),	!
	(multosym,			$,	"*=",	j_multo),	!
	(divtosym,			$,	"/=",	j_divto),	!
	(remtosym,			$,	"%=",	j_remto),	!
	(iortosym,			$,	"|=",	j_iorto),	!
	(iandtosym,			$,	"&=",	j_iandto),	!
	(ixortosym,			$,	"^=",	j_ixorto),	!
	(shltosym,			$,	"<<=",	j_shlto),	!
	(shrtosym,			$,	">>=",	j_shrto),	!
	(sqrtsym,			$,	"sqrt",	j_sqrt),	!

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
	(kstrincludesym,	$,	"k",	0),			!

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
	(kfnspecsym,		$,	"k",	0),			! INLINE etc
	(kalignassym,		$,	"k",	0),			! _ALIGNAS
	(kenumsym,			$,	"k",	0),			! ENUM
!	(kcallconvsym,		$,	"k",	0),			! CLANG etc
	(ksizeofsym,		$,	"k",	0),			! SIZEOF
	(klengthofsym,		$,	"k",	0),			! LENGTHOF
	(kdefinedsym,		$,	"k",	0),			! DEFINED
	(kgenericsym,		$,	"k",	0),			! _GENERIC
	(kalignofsym,		$,	"k",	0),			! _ALIGNOF
	(kshowmodesym,		$,	"k",	0),			! SHOWMODE
	(kshowtypesym,		$,	"k",	0),			! SHOWTYPE
	(ktypeofsym,		$,	"k",	0),			! TYPEOF
	(kstrtypesym,		$,	"k",	0),			! STRTYPE
	(kmccassertsym,		$,	"k",	0),			!
	(kcputimesym,		$,	"k",	0),			!
	(kconstantsym,		$,	"k",	0),			!CONSTANT
	(kstructinfosym,	$,	"k",	0),			!STRUCTINFO

	(kdummysym,			$,	"",		0)			!
end

global tabledata() []ichar sourcedirnames =
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
	(warningdir,	$),
	(messagedir,	$),
	(pausedir,		$),
	(debugondir,	$),
	(debugoffdir,	$),
	(showmacrodir,	$),
	(blankdir,		$),
	(linedir,		$),
!	(strincludedir,	$),
	(pragmadir,		$)
end

global tabledata() [0:]ichar namespacenames=
	(ns_none,		$),			!not set
	(ns_general,	$),			!variables, functions, typedefs, enum names
	(ns_tags,		$),			!struct, union, enum tags
	(ns_labels,		$),			!label names
	(ns_fields,		$)			!field names
end

global tabledata() [0:]ichar namenames, [0:]int32 namespaces=
	(nullid=0,		$,		ns_none),		!Not assigned, or keyword/macro defined by .symbol
	(macroid,		$,		ns_none),		!
	(programid,		$,		ns_none),		!Main root
	(moduleid,		$,		ns_none),		!
	(extmoduleid,	$,		ns_none),		!
	(typeid,		$,		ns_general),	!Type name in type, proc or module
	(procid,		$,		ns_general),	!Proc/method/function/op name
	(staticid,		$,		ns_general),	!Static in type or proc or module
	(frameid,		$,		ns_general),	!Local var
	(paramid,		$,		ns_general),	!Local param
	(fieldid,		$,		ns_fields),		!Field of Record or Class
	(enumid,		$,		ns_general),	!Enum name, part of enum type only
!	(macroparamid,	$,		0),				!
	(enumtagid,		$,		ns_tags),		!
	(structtagid,	$,		ns_tags),		!
	(constantid,	$,		ns_general),	!
	(labelid,		$,		ns_labels)		!Label name in proc only
end

global tabledata []ichar stnames, []int32 stsymbols, []int32 stsubcodes=

	("if",			kifsym,			j_if),
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
	("warning",		ksourcedirsym,	warningdir),
	("message",		ksourcedirsym,	messagedir),
	("MESSAGE",		ksourcedirsym,	messagedir),
	("pragma",		ksourcedirsym,	pragmadir),
	("line",		ksourcedirsym,	linedir),
	("pause",		ksourcedirsym,	pausedir),
	("debugon",		ksourcedirsym,	debugondir),
	("debugoff",	ksourcedirsym,	debugoffdir),
	("showmacro",	ksourcedirsym,	showmacrodir),
!	("strinclude",	ksourcedirsym,	strincludedir),
	("strinclude",	kstrincludesym,	0),

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
	("$callback",	kfnspecsym,		callback_fnspec),

	("_Alignas",	kalignassym,	0),

	("enum",		kenumsym,		0),

!	("$stdcall",	kcallconvsym,	stdcall_cc),
!	("$callback",	kcallconvsym,	callback_cc),
!	("$windows",	kcallconvsym,	stdcall_cc),
!!	("$clang",		kcallconvsym,	clang_cc),

	("void",		ktypespecsym,	ts_void),
	("char",		ktypespecsym,	ts_char),
	("short",		ktypespecsym,	ts_short),
	("long",		ktypespecsym,	ts_long),
	("int",			ktypespecsym,	ts_int),
	("float",		ktypespecsym,	ts_float),
	("double",		ktypespecsym,	ts_double),
	("signed",		ktypespecsym,	ts_signed),
	("unsigned",	ktypespecsym,	ts_unsigned),

	("_Bool",		ktypespecsym,	ts_char),

	("_Complex",	ktypespecsym,	ts_complex),

	("__DATE__",	predefmacrosym,	pdm_date),
	("__FILE__",	predefmacrosym,	pdm_file),
	("__LINE__",	predefmacrosym,	pdm_line),
!	("__STDC__",	predefmacrosym,	pdm_stdc),
	("__TIME__",	predefmacrosym,	pdm_time),
!	("__cdecl",		predefmacrosym,	pdm_cdecl),
	("__BCC__",		predefmacrosym,	pdm_bcc),
	("__func__",	predefmacrosym,	pdm_func),
	("__FUNCTION__",	predefmacrosym,	pdm_func),

!	("not",			notlsym,		0),
	("sizeof",		ksizeofsym,		0),
	("$sqrt",		sqrtsym,		0),
	("defined",		kdefinedsym,	0),
	("_Generic",	kgenericsym,	0),
	("_Alignof",	kalignofsym,	0),
	("$showmode",	kshowmodesym,	0),
	("$showtype",	kshowtypesym,	0),
	("typeof",		ktypeofsym,		0),
	("strtype",		kstrtypesym,	0),
	("_Static_assert",	kmccassertsym,	0),
	("cputime",		kcputimesym,	0),
!	("constant",	kconstantsym,	0),
	("structinfo",	kstructinfosym,	0),

	("$$dummy",		0,				0)
end

global tabledata() [0:]ichar convnames =

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
	(tschar,	tschar,		tsint),
	(tschar,	tsshort,	tsint),
	(tschar,	tsint,		tsint),
	(tschar,	tsllong,	tsllong),
	(tschar,	tbool,		tsint),
	(tschar,	tuchar,		tsint),
	(tschar,	tushort,	tsint),
	(tschar,	tuint,		tsint),
	(tschar,	tullong,	tsllong),
	(tschar,	tfloat,		tfloat),
	(tschar,	tdouble,	tdouble),
	(tschar,	tldouble,	tldouble),
	(tsshort,	tschar,		tsint),
	(tsshort,	tsshort,	tsint),
	(tsshort,	tsint,		tsint),
	(tsshort,	tsllong,	tsllong),
	(tsshort,	tbool,		tsint),
	(tsshort,	tuchar,		tsint),
	(tsshort,	tushort,	tsint),
	(tsshort,	tuint,		tsint),
	(tsshort,	tullong,	tsllong),
	(tsshort,	tfloat,		tfloat),
	(tsshort,	tdouble,	tdouble),
	(tsshort,	tldouble,	tldouble),
	(tsint,		tschar,		tsint),
	(tsint,		tsshort,	tsint),
	(tsint,		tsint,		tsint),
	(tsint,		tsllong,	tsllong),
	(tsint,		tbool,		tsint),
	(tsint,		tuchar,		tsint),
	(tsint,		tushort,	tsint),
	(tsint,		tuint,		tuint),
	(tsint,		tullong,	tsllong),
	(tsint,		tfloat,		tfloat),
	(tsint,		tdouble,	tdouble),
	(tsint,		tldouble,	tldouble),
	(tsllong,	tschar,		tsllong),
	(tsllong,	tsshort,	tsllong),
	(tsllong,	tsint,		tsllong),
	(tsllong,	tsllong,	tsllong),
	(tsllong,	tbool,		tsllong),
	(tsllong,	tuchar,		tsllong),
	(tsllong,	tushort,	tsllong),
	(tsllong,	tuint,		tsllong),
	(tsllong,	tullong,	tullong),
	(tsllong,	tfloat,		tfloat),
	(tsllong,	tdouble,	tdouble),
	(tsllong,	tldouble,	tldouble),
	(tbool,		tschar,		tsint),
	(tbool,		tsshort,	tsint),
	(tbool,		tsint,		tsint),
	(tbool,		tsllong,	tsllong),
	(tbool,		tbool,		tuint),
	(tbool,		tuchar,		tuint),
	(tbool,		tushort,	tuint),
	(tbool,		tuint,		tuint),
	(tbool,		tullong,	tullong),
	(tbool,		tfloat,		tfloat),
	(tbool,		tdouble,	tdouble),
	(tbool,		tldouble,	tldouble),
	(tuchar,	tschar,		tsint),
	(tuchar,	tsshort,	tsint),
	(tuchar,	tsint,		tsint),
	(tuchar,	tsllong,	tsllong),
	(tuchar,	tbool,		tnone),
	(tuchar,	tuchar,		tuint),
	(tuchar,	tushort,	tuint),
	(tuchar,	tuint,		tuint),
	(tuchar,	tullong,	tullong),
	(tuchar,	tfloat,		tfloat),
	(tuchar,	tdouble,	tdouble),
	(tuchar,	tldouble,	tldouble),
	(tushort,	tschar,		tsint),
	(tushort,	tsshort,	tsint),
	(tushort,	tsint,		tsint),
	(tushort,	tsllong,	tsllong),
	(tushort,	tbool,		tuint),
	(tushort,	tuchar,		tuint),
	(tushort,	tushort,	tuint),
	(tushort,	tuint,		tuint),
	(tushort,	tullong,	tullong),
	(tushort,	tfloat,		tfloat),
	(tushort,	tdouble,	tdouble),
	(tushort,	tldouble,	tldouble),
	(tuint,		tschar,		tsint),
	(tuint,		tsshort,	tsint),
	(tuint,		tsint,		tuint),
	(tuint,		tsllong,	tsllong),
	(tuint,		tbool,		tuint),
	(tuint,		tuchar,		tuint),
	(tuint,		tushort,	tuint),
	(tuint,		tuint,		tuint),
	(tuint,		tullong,	tullong),
	(tuint,		tfloat,		tfloat),
	(tuint,		tdouble,	tdouble),
	(tuint,		tldouble,	tldouble),
	(tullong,	tschar,		tullong),
	(tullong,	tsshort,	tullong),
	(tullong,	tsint,		tullong),
	(tullong,	tsllong,	tullong),
	(tullong,	tbool,		tullong),
	(tullong,	tuchar,		tullong),
	(tullong,	tushort,	tullong),
	(tullong,	tuint,		tullong),
	(tullong,	tullong,	tullong),
	(tullong,	tfloat,		tfloat),
	(tullong,	tdouble,	tdouble),
	(tullong,	tldouble,	tldouble),
	(tfloat,	tschar,		tdouble),
	(tfloat,	tsshort,	tdouble),
	(tfloat,	tsint,		tdouble),
	(tfloat,	tsllong,	tdouble),
	(tfloat,	tbool,		tdouble),
	(tfloat,	tuchar,		tdouble),
	(tfloat,	tushort,	tdouble),
	(tfloat,	tuint,		tdouble),
	(tfloat,	tullong,	tdouble),
	(tfloat,	tfloat,		tfloat),
	(tfloat,	tdouble,	tdouble),
	(tfloat,	tldouble,	tldouble),
	(tdouble,	tschar,		tdouble),
	(tdouble,	tsshort,	tdouble),
	(tdouble,	tsint,		tdouble),
	(tdouble,	tsllong,	tdouble),
	(tdouble,	tbool,		tdouble),
	(tdouble,	tuchar,		tdouble),
	(tdouble,	tushort,	tdouble),
	(tdouble,	tuint,		tdouble),
	(tdouble,	tullong,	tdouble),
	(tdouble,	tfloat,		tdouble),
	(tdouble,	tdouble,	tdouble),
	(tdouble,	tldouble,	tldouble),
	(tldouble,	tschar,		tdouble),
	(tldouble,	tsshort,	tdouble),
	(tldouble,	tsint,		tdouble),
	(tldouble,	tsllong,	tdouble),
	(tldouble,	tbool,		tdouble),
	(tldouble,	tuchar,		tdouble),
	(tldouble,	tushort,	tdouble),
	(tldouble,	tuint,		tdouble),
	(tldouble,	tullong,	tdouble),
	(tldouble,	tfloat,		tdouble),
	(tldouble,	tdouble,	tdouble),
	(tldouble,	tldouble,	tldouble),
)

!table used to set up conversionops
global [][3]byte convsetuptable=(
	(tschar,	tschar,		swiden_c),
	(tschar,	tsshort,	swiden_c),
	(tschar,	tsint,		swiden_c),
	(tschar,	tsllong,	swiden_c),
	(tschar,	tbool,		bool_c),
	(tschar,	tuchar,		soft_c),
	(tschar,	tushort,	swiden_c),
	(tschar,	tuint,		swiden_c),
	(tschar,	tullong,	swiden_c),
	(tschar,	tfloat,		sfloat_c),
	(tschar,	tdouble,	sfloat_c),
	(tschar,	tldouble,	sfloat_c),

	(tsshort,	tschar,		truncate_c),
	(tsshort,	tsshort,	no_conv),
	(tsshort,	tsint,		swiden_c),
	(tsshort,	tsllong,	swiden_c),
	(tsshort,	tbool,		bool_c),
	(tsshort,	tuchar,		truncate_c),
	(tsshort,	tushort,	soft_c),
	(tsshort,	tuint,		swiden_c),
	(tsshort,	tullong,	swiden_c),
	(tsshort,	tfloat,		sfloat_c),
	(tsshort,	tdouble,	sfloat_c),
	(tsshort,	tldouble,	sfloat_c),
	(tsint,		tschar,		truncate_c),

	(tsint,		tsshort,	truncate_c),

	(tsint,		tsint,		no_conv),
	(tsint,		tsllong,	swiden_c),
	(tsint,		tbool,		bool_c),
	(tsint,		tuchar,		truncate_c),
	(tsint,		tushort,	truncate_c),
	(tsint,		tuint,		soft_c),
	(tsint,		tullong,	swiden_c),
	(tsint,		tfloat,		sfloat_c),
	(tsint,		tdouble,	sfloat_c),
	(tsint,		tldouble,	sfloat_c),

	(tsllong,	tschar,		truncate_c),
!	(tsllong,	tschar,		narrow_c),

	(tsllong,	tsshort,	truncate_c),
	(tsllong,	tsint,		truncate_c),
	(tsllong,	tsllong,	no_conv),
	(tsllong,	tbool,		bool_c),

	(tsllong,	tuchar,		truncate_c),
!	(tsllong,	tuchar,		narrow_c),

	(tsllong,	tushort,	truncate_c),
	(tsllong,	tuint,		truncate_c),
	(tsllong,	tullong,	soft_c),
	(tsllong,	tfloat,		sfloat_c),
	(tsllong,	tdouble,	sfloat_c),
	(tsllong,	tldouble,	sfloat_c),
	(tbool,		tschar,		soft_c),
	(tbool,		tsshort,	uwiden_c),
	(tbool,		tsint,		uwiden_c),
	(tbool,		tsllong,	uwiden_c),
	(tbool,		tbool,		no_conv),
	(tbool,		tuchar,		soft_c),
	(tbool,		tushort,	uwiden_c),
	(tbool,		tuint,		uwiden_c),
	(tbool,		tullong,	uwiden_c),
	(tbool,		tfloat,		ufloat_c),
	(tbool,		tdouble,	ufloat_c),
	(tbool,		tldouble,	ufloat_c),
	(tuchar,	tschar,		soft_c),
	(tuchar,	tsshort,	uwiden_c),
	(tuchar,	tsint,		uwiden_c),
	(tuchar,	tsllong,	uwiden_c),
	(tuchar,	tbool,		bool_c),
	(tuchar,	tuchar,		soft_c),
	(tuchar,	tushort,	uwiden_c),
	(tuchar,	tuint,		uwiden_c),
	(tuchar,	tullong,	uwiden_c),
	(tuchar,	tfloat,		ufloat_c),
	(tuchar,	tdouble,	ufloat_c),
	(tuchar,	tldouble,	ufloat_c),

	(tushort,	tschar,		truncate_c),
	(tushort,	tsshort,	soft_c),
	(tushort,	tsint,		uwiden_c),
	(tushort,	tsllong,	uwiden_c),
	(tushort,	tbool,		bool_c),
	(tushort,	tuchar,		truncate_c),
	(tushort,	tushort,	no_conv),
	(tushort,	tuint,		uwiden_c),
	(tushort,	tullong,	uwiden_c),
	(tushort,	tfloat,		ufloat_c),
	(tushort,	tdouble,	ufloat_c),
	(tushort,	tldouble,	ufloat_c),

	(tuint,		tschar,		truncate_c),
	(tuint,		tsshort,	truncate_c),
	(tuint,		tsint,		soft_c),
	(tuint,		tsllong,	uwiden_c),
	(tuint,		tbool,		bool_c),
	(tuint,		tuchar,		truncate_c),
	(tuint,		tushort,	truncate_c),
	(tuint,		tuint,		no_conv),
	(tuint,		tullong,	uwiden_c),
	(tuint,		tfloat,		ufloat_c),
	(tuint,		tdouble,	ufloat_c),
	(tuint,		tldouble,	ufloat_c),

	(tullong,	tschar,		truncate_c),
	(tullong,	tsshort,	truncate_c),
	(tullong,	tsint,		truncate_c),
	(tullong,	tsllong,	soft_c),
	(tullong,	tbool,		bool_c),
	(tullong,	tuchar,		truncate_c),
	(tullong,	tushort,	truncate_c),
	(tullong,	tuint,		truncate_c),
	(tullong,	tullong,	no_conv),
	(tullong,	tfloat,		ufloat_c),
	(tullong,	tdouble,	ufloat_c),
	(tullong,	tldouble,	ufloat_c),

	(tfloat,	tschar,		sfix_c),
	(tfloat,	tsshort,	sfix_c),
	(tfloat,	tsint,		sfix_c),
	(tfloat,	tsllong,	sfix_c),
	(tfloat,	tbool,		ufix_c),
	(tfloat,	tuchar,		ufix_c),
	(tfloat,	tushort,	ufix_c),
	(tfloat,	tuint,		ufix_c),
	(tfloat,	tullong,	ufix_c),
	(tfloat,	tfloat,		no_conv),
	(tfloat,	tdouble,	fwiden_c),
	(tfloat,	tldouble,	fwiden_c),

	(tdouble,	tschar,		sfix_c),
	(tdouble,	tsshort,	sfix_c),
	(tdouble,	tsint,		sfix_c),
	(tdouble,	tsllong,	sfix_c),
	(tdouble,	tbool,		ufix_c),
	(tdouble,	tuchar,		ufix_c),
	(tdouble,	tushort,	ufix_c),
	(tdouble,	tuint,		ufix_c),
	(tdouble,	tullong,	ufix_c),
	(tdouble,	tfloat,		fnarrow_c),
	(tdouble,	tdouble,	no_conv),
	(tdouble,	tldouble,	no_conv),

	(tldouble,	tschar,		sfix_c),
	(tldouble,	tsshort,	sfix_c),
	(tldouble,	tsint,		sfix_c),
	(tldouble,	tsllong,	sfix_c),
	(tldouble,	tbool,		ufix_c),
	(tldouble,	tuchar,		ufix_c),
	(tldouble,	tushort,	ufix_c),
	(tldouble,	tuint,		ufix_c),
	(tldouble,	tullong,	ufix_c),
	(tldouble,	tfloat,		fnarrow_c),
	(tldouble,	tdouble,	no_conv),
	(tldouble,	tldouble,	no_conv),
)

global []int badexprs=(
j_const,
j_name,
j_ifx,
j_andl,
j_orl,
j_notl,
j_istruel,
j_exprlist,
j_andand,
j_eq,
j_ne,
j_lt,
j_le,
j_ge,
j_gt,
j_add,
j_sub,
j_mul,
j_div,
j_rem,
j_iand,
j_ior,
j_ixor,
j_shl,
j_shr,
j_dot,
j_idot,
j_index,
j_ptr,
j_addptr,
j_subptr,
j_neg,
j_abs,
j_inot)

=== cc_support.m 8/73 ===
import clib
import mlib
import oslib

import cc_decls
import cc_tables

const dolinesplicing=1
!const dolinesplicing=0


global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

proc stopcompiler(ichar filename,int lineno)=
	filehandle f

!	f:=fopen("$error.tmp","w")
!	println @f,filename,lineno
!	fclose(f)

	println
	println
	stop 1
end

global proc mcerror(ichar mess)=
println "MC Error:",mess
os_getch()
stop 40
end

global proc serror(ichar mess)=
serror_gen(mess)
end

global proc serror_gen(ichar mess)=
if currproc then
	print "In function",currproc^.name,," "
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
sprintf(&.str,mess,a,b)
serror_gen(&.str)
end

global proc serror_s(ichar mess,a)=
[256]char str
sprintf(&.str,mess,a)
serror_gen(&.str)
end

global proc terror_gen(ichar mess)=

if currproc then
	println "In function",currproc^.name
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

sprintf(&.str,mess,a)
terror_gen(&.str)
end

global proc terror_ss(ichar mess,a,b)=
[256]char str

sprintf(&.str,mess,a,b)
terror_gen(&.str)
end

global proc gerror_gen(ichar mess,ref unitrec p=nil)=
int lineno

if p then
	lineno:=p^.lineno
else
	lineno:=clineno
fi

if currproc then
	print "In function",currproc^.name,," "
fi

println "On line",lineno iand 16777215,"in file",sourcefilepaths[lineno>>24]
println
println "**** Code Gen Error:",mess,"****"
stopcompiler(sourcefilepaths[lineno>>24],lineno)
end


global proc gerror(ichar mess,ref unitrec p=nil)=
gerror_gen(mess,p)
end

global proc gerror_s(ichar mess,s,ref unitrec p=nil)=
[256]char str

sprintf(&.str,mess,s)
gerror_gen(&.str,p)
end

global proc nxerror(ichar mess,ref unitrec p=nil)=
int lineno

if p then
	lineno:=p^.lineno
else
	lineno:=0
fi
println "NX error:",mess,"on line",lineno,stmodule^.name
os_getch()
stopcompiler(stmodule^.name,lineno)
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

global proc loaderror(ichar mess,mess2="")=
[512]char str

sprintf(&.str,mess,mess2)
println "Load Error:",&.str
println "Stopping"
stop 45
end

global function loadfromstdin(ichar file)int=
ichar s
[30000]CHAR SRC
ref char p
int n,c

if nsourcefiles>maxsourcefile then
	loaderror("Too many source files")
fi
++nsourcefiles
sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(file)
sourcefilenames[nsourcefiles]:=pcm_copyheapstring(file)

println "Reading from stdin. Finish with Ctrl-Z:"
p:=&.src
n:=0
while (c:=getchar())<>c_eof do
	p++^:=c
	if ++n>=src.len then
		loaderror("stdin overflow")
	fi
od
!p++^:=26
p^:=0	

sourcefiletext[nsourcefiles]:=pcm_copyheapstring(&.src)
sourcefilesizes[nsourcefiles]:=strlen(&.src)
return nsourcefiles
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

if dolinesplicing then
	s:=splicelines(s)
fi

!CPL "SETFILETEXT2",=NSOURCEFILES,REF VOID S,=RFSIZE
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
!CPL "SETFILETEXT3",=NSOURCEFILES,REF VOID SOURCEFILETEXT[NSOURCEFILES]

sourcefilesizes[nsourcefiles]:=strlen(hdrtext)
return nsourcefiles
end

proc gs_copytostr(ref strbuffer source,ref char s)=
if source^.length then
	memcpy(s,source^.strptr,source^.length)
	(s+source^.length)^:=0
else
	s^:=0
fi
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

function isalphanum(int c)int=
if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
	return 1
fi
return 0
end

proc showmacrolineno=
if slineno then
	println "	(Last macro invoked near line",
		slineno,"in file",sourcefilenames[sfileno],,")"
fi
end
=== cc_lex.m 9/73 ===
! (C tokeniser module)

import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_headers
import cc_lib

ref tokenrec tkptr=nil

int dowhitespace=0

int NINCLUDES

record stackinforec =
	ref char startptr
	ref char sptr
	int32 lineno
	int32 fileno
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
!const int maxpastedtokens=17000
const int maxpastedtokens=87000
[maxpastedtokens]ichar pastedtokenlist
int npastedtokens=0
int isincludefile=0				!whether readng include file name

int firstsymbol=1
ref byte reallxsptr

GLOBAL int nhstsymbols
int hstthreshold				!limit above which new hst should be generated

global proc lex_preprocess_only(ichar infile,int showtokens, NN, toconsole=0)=
	ref char psource
	int ntokens,nlines,fileno,size
	int LENGTH
	int64 nchars,t,hashtot,symtot
	real tsecs
	static strbuffer sbuffer
	static ref strbuffer dest=&sbuffer
	filehandle f
	[300]char outfile
	ICHAR SS

	dowhitespace:=1
	fileno:=loadsourcefile(infile,infile)

	strcpy(&.outfile,changeext(infile,"i"))

	psource:=cast(sourcefiletext[fileno])
	size:=sourcefilesizes[fileno]

	nlines:=ntokens:=0
	hashtot:=symtot:=0
	t:=os_clock()

	destcopy:=dest
	gs_init(dest)

	NALLLINES:=0

!println "Preprocessing",infile

	lxsptr:=psource
	lxstart:=lxsptr
	nextlx.lineno:=1
	setfileno(1)
	ifcondlevel:=0

	stacksourcefile("bcc.h",1)

	nextlx.symbol:=eolsym

	repeat
		lexm()
		++ntokens

		if showtokens then
			emittoken(&nextlx,dest)
		fi

	until nextlx.symbol=eofsym

	if ifcondlevel then
		lxerror("#endif missing")
	fi

	nlines+:=NALLLINES

	if showtokens then
		if toconsole then
			gs_println(dest,nil)
		else
			f:=fopen(&.outfile,"wb")
			gs_println(dest,f)
			fclose(f)
		fi
	fi
end

global proc lexreadtoken=
!read next token into nextlx
	int c,csum,hsum,dodir
	ref char p,ss
	ichar searchstr

	nextlx.subcodex:=0

	doswitch lxsptr++^
	when 'A'..'Z','a'..'z','$','_' then
doname::
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

		ss:=pcm_alloc(nextlx.length+1)		!zero-term in lex(), as headers may need to be
		memcpy(ss,lxsvalue,nextlx.length)	!re-tokenised
		(ss+nextlx.length)^:=0
		lxsvalue:=ss

		lookup()						!clash, so do normal lookup to set lxsymptr
		return

	when '1'..'9' then					!can only be decimal
		case lxsptr^
		when ' ',')',cr,',',';' then		!assume single digit decimal
			nextlx.symbol:=intconstsym
			nextlx.subcode:=tsint
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
			nextlx.subcode:=tsint
			nextlx.value:=0
			nextlx.length:=1
			setnumberoffset(lxsptr-1-lxstart)
			return
		else

			readoctal(lxsptr-1)
			return
		endswitch					!else assume just zero	

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
	++NALLLINES
		case lxsptr^
		when cr then
			++lxsptr			!point to lf
			lxsptr++^:=' '		!set lf to space (so that '#' processing works
		when lf then
			lxsptr++^:=' '
		else
!		lxerror("\\ not followed by newline")	
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
		endswitch

	when ',' then
		nextlx.symbol:=commasym
		return

	when ';' then
		nextlx.symbol:=semisym
		return

	when ':' then
		nextlx.symbol:=colonsym
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
		endswitch
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
		endswitch
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
	++NALLLINES
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
!		while spacemap[(++lxsptr)^] do od
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
	doeof::
		--lxsptr
		if lx_stackindex then
			unstacksourcefile()
			nextlx.symbol:=eolsym
		else
			nextlx.symbol:=eofsym
		fi
		return

	when 12 then

	when 0xEF then
!LXERROR("BOM")
		lxsptr+:=2

	else
		!codes above 127 can be names; later, decode the actual unicode from
		!the utf8 sequence, and check for correct ranges of chars that are allowed
!		if (lxsptr-1)^ in 128..255 then goto doname fi
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
	int64 aa,cc,pref
	const maxrealdigits=500
	[maxrealdigits+12]char realstr
	ref char rs
	[32]char expstr
	word64 xx1,xx2

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
	nextlx.subcode:=tdouble
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
	int64 a

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
!int i
	PRINTLN "Lex error",mess,"in:",,sourcefilepaths[getfileno()],
	 "Line:",nextlx.lineno
	println
	println
	println
	os_getch()
	stop 11
end

global proc printsymbol(ref tokenrec lp)=
	tokenrec l
	l:=lp^

	printf("%-18s",symbolnames[l.symbol])

	case l.symbol
	when namesym then
		printstrn(l.symptr^.name,l.symptr^.namelen)

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

	retry::
	j:=lxhashvalue iand hstmask
	wrapped:=0


	do
		nextlx.symptr:=hashtable^[j]
		length:=nextlx.symptr^.namelen

		if not length then
			exit
		fi

		if length=nextlx.length then	!match on length
			if memcmp(nextlx.symptr^.name,lxsvalue,length)=0 then	!match
				return 1
			fi
		fi

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

	nextlx.symptr^.name:=lxsvalue
	nextlx.symptr^.namelen:=nextlx.length
	nextlx.symptr^.symbol:=namesym
	++nhstsymbols

	return 0
end

GLOBAL function gethashvalue(ichar s,int length=-1)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!assumes s is lower-case, as conversion not done
	int c,hsum

	if length=-1 then
		length:=strlen(s)
	fi
	hsum:=0

	to length do
		hsum:=hsum<<4-hsum+s++^
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

		nextlx.symptr^.symbol:=stsymbols[i]
		nextlx.symptr^.subcode:=stsubcodes[i]
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
		printstrn(pstart,lxsptr-pstart); PRINTLN
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
		if not syshdr then
			addautomodule(&.filename,getfileno())
		fi

	when definedir then
		dodefine()

	when undefdir then
		lexreadtoken()
		if nextlx.symbol<>namesym then
			lxerror("undef: name expected")
		fi
		d:=nextlx.symptr
		if d^.nameid<>macroid then
!		println getstname(nextlx.symptr)
!		lxerror("#undef: can't find macro")
		else
			d^.nameid:=nullid
			d^.symbol:=nextlx.symptr^.oldsymbol
			d^.mparamlist:=nil
			d^.attribs.ax_flmacro:=0
		fi

	when ifdefdir then
		cond:=getifdef()
		goto doif

	when ifndefdir then
		cond:=not getifdef()
		goto doif

	when ifdir then
		cond:=getifexpr()
	doif::

		++ifcondlevel
		if cond then			!carry on reading code as normal
			return 0
		else
	doskipcode::
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
		goto dowarning2

	when messagedir then
		lexm()
		print "#MESSAGE"
		if nextlx.symbol=eolsym then
			println " Line",nextlx.lineno+1,sourcefilenames[getfileno()]
		else
			showtoken(&nextlx); println
		fi
		goto dowarning2
	when warningdir,pausedir then
		lexm()
		print "#WARNING:"; showtoken(&nextlx); println
	dowarning2::
		while nextlx.symbol<>eolsym and nextlx.symbol<>eofsym do lexm() od
		if dir=pausedir then
			print "Press key..."
			os_getch()
			println
		fi

	when pragmadir then
		dopragmadir()

	when debugondir then
		debug:=1

	when debugoffdir then
		debug:=0

	when showmacrodir then
		lexreadtoken()
		if nextlx.symbol=namesym then
			d:=nextlx.symptr
			print nextlx.lineno, sourcefilenames[getfileno()],":"
			PRINT "SHOW MACRO",getstname(d),":"
			if d^.nameid=macroid then
				showtokens("tokens:",d^.tokenlist)
				println
			else
				PRINTLN "not a macro"
			fi
		else
			println "Not a name"
		fi

	else
	skip::
		println "DIRECTIVE NOT IMPL:",sourcedirnames[dir]
		lxsptr:=pstart
		nextlx.symbol:=lexhashsym
		return 1
		lxerror("Directive not implemented")
	esac
	return 0
END

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

	case nextlx.symptr^.symbol
	when ksourcedirsym then
		return nextlx.symptr^.subcode
	when kifsym then
		return ifdir
	when kelsesym then
		return elsedir
	when eolsym then
		return blankdir
	esac

	d:=nextlx.symptr
	if d^.nameid=macroid then			!could have redefined 'define' etc
		if d^.oldsymbol=ksourcedirsym then
			return d^.subcode
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
		if hashtable^[i]^.name then
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
	++NALLLINES
				lxsptr+:=2
				++nextlx.lineno
			when lf then			!loop
				++lxsptr
	++NALLLINES
				++nextlx.lineno
			esac					!else ignore and loop
!		lxerror("line comment LINE CONT")
		esac
	od
	++NALLLINES
	++nextlx.lineno
end

proc readblockcomment=
!positioned at '*' of '/*'

	do
		while commentmap[(++lxsptr)^] do od		!skip bulk of characters

		case lxsptr^
		when lf then
	++NALLLINES
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
	word64 aa
	int c,length,leading,ll,usigned
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
	enddoswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>16 then
		lxerror("Overflow in hex number")
	fi

	nextlx.symbol:=intconstsym
	if aa>0x7FFF'FFFF'FFFF'FFFF then
		nextlx.subcode:=tullong
	elsif aa>0xFFFF'FFFF then
		nextlx.subcode:=tsllong
	elsif aa>0x7FFF'FFFF then
		nextlx.subcode:=tuint
	else
		nextlx.subcode:=tsint
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readbinary(ref char pstart)=
!positioned at first char of binary number, after 0b/0B
	word64 aa
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
	enddoswitch

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
	nextlx.subcode:=tsint
	if aa>=0x7FFF'FFFF then
		nextlx.subcode:=tsllong
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readoctal(ref char pstart)=
!positioned at first char of octal number, after 0 (or at 8 or 9)
	word64 aa
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
!	lxerror("Can't do octal/floats")
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
	doalpha::
			readalphanumeric(pstart)
			return
		fi
		--lxsptr
		exit
	enddoswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>22 or length=22 and (res:=cmpstringn(p,"1777777777777777777777",22))>0 then
		lxerror("Overflow in octal number")
	fi

	to length do
		aa:=aa*8+p++^-'0'
	od

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tsint
	if aa>=0x7FFF'FFFF then
		nextlx.subcode:=tsllong
	fi
	nextlx.value:=aa

	checknumbersuffix()
end

proc readdecimal(ref char pstart)=
!positioned at first char of decimal number
!will read integer, unless ends with any of ".eE" than assumed to be real
	word64 aa
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
	enddoswitch

	setnumberoffset(pstart-lxstart)
	nextlx.length:=lxsptr-pstart

	if length>20 or length=20 and (res:=cmpstringn(p,"18446744073709551615",20))>0 then
		lxerror("Overflow in decimal number")
	fi

	to length do				!A..Z have been preprocessed so that they carry on from '9'
		aa:=aa*word64(10)+word(p++^-'0')
	od

	nextlx.symbol:=intconstsym
	nextlx.subcode:=tsint

	case ll
	when 0,1 then
		if usigned then
			if aa>=0xFFFF'FFFF then
				nextlx.subcode:=tullong
			else
				nextlx.subcode:=tuint
			fi
		else
			if aa>=0x7FFF'FFFF then
				nextlx.subcode:=tsllong
			fi
		fi
	else
		if usigned then
			nextlx.subcode:=tullong
		else
			nextlx.subcode:=tsllong
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
	enddoswitch

	return tsint			!don't bother for now
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

	strcpy(&.headerpath,extractpath(file))

	if headerpath[1]='/' or headerpath[2]=':' and headerpath[3]='/' then
		if checkfile(file) then
			return loadsourcefile(file,file)
		fi
		return 0			!don't both looking anywhere else
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
	reenter::

	lx:=nextlx				!grab that already read basic token

	lexm()			!read new token for next time around

	if lx.symbol=namesym and lx_stackindex=0 then
		(lx.symptr^.name+lx.length)^:=0
	fi

	docase nextlx.symbol
	when namesym then
		nextlx.symbol:=nextlx.symptr^.symbol			!convert to reserved word, type, op etc
		if nextlx.symbol=ksourcedirsym then
			nextlx.symbol:=namesym
		fi
		nextlx.subcode:=nextlx.symptr^.subcode

		return

	when eolsym then								!lose eols
		lexm()
	else
		return	
	enddocase

end

proc shownumberstr(ref tokenrec l,filehandle f=nil)=
	ref char s

	if getfilenox(l) then
		s:=sourcefiletext[getfilenox(l)]+getnumberoffsetx(l)
	else
		s:=pastedtokenlist[l^.pasteno]
	fi
	printstrn(s,l^.length,f)

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
	ref word16 wd,wd0
	int c,d,length

	if termchar='"' then
		nextlx.symbol:=(fwide|wstringconstsym|stringconstsym)
	else
		nextlx.symbol:=charconstsym
	fi

	nextlx.svalue:=lxsptr

	if lx_stackindex=0 or fwide then
		dest:=lxsptr				!form string into same buffer
		ws:=dest					!for wide only
	else							!for headers that can be re-read, form string externally
		dest:=&.str
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
	reenter::
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
	++NALLLINES
				++nextlx.lineno
				if lxsptr^=lf then ++lxsptr fi
				next
			when lf then
				next
			end						!else use the escaped character itself
		when '"','\'' then		!possible terminators
			if c=termchar then		!terminator char
				exit
			fi
		when lf,0 then
			println =nextlx.lineno
			lxerror("String not terminated")
		endswitch
	normalchar::

		if lx_stackindex=0 then
			dest++^:=c
		elsif ++length<maxlocalstr then
			dest++^:=c
		else
			lxerror("Local str too long")
		fi
	od
	dest^:=0


	if fwide then			!need to put string on heap was will use 16-bit chars
		length:=nextlx.length:=dest-nextlx.svalue

		wd0:=wd:=pcm_alloc(length*2+2)
		to length do
			wd++^:=ws++^
		od
		wd^:=0
		nextlx.svalue:=cast(wd0)

	else
		if lx_stackindex=0 then
			nextlx.length:=dest-nextlx.svalue
		else
			nextlx.length:=length

			nextlx.svalue:=pcm_alloc(length+1)
			memcpy(nextlx.svalue,&.str,length+1)
		fi
	fi
end

proc addlisttoken(ref ref tokenrec ulist,ulistx,ref tokenrec p)=
!add strec p to end of linked list headed by ulist^. ulistx^ is current end of list
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^^.nexttoken:=p
	fi
	p^.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlisttoken_copy(ref ref tokenrec ulist,ulistx,ref tokenrec q)=
!like addlisttoken but add copy of nextlx
!(as will likely be in nextlx)
!add strec p to end of linked list headed by ulist^. ulistx^ is current end of list
	ref tokenrec p

	p:=alloctoken()

	p^:=q^
	p^.nexttoken:=nil

	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^^.nexttoken:=p
	fi
	p^.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlist_nextlx(ref ref tokenrec ulist,ulistx)=
!like addlisttoken but add copy of nextlx

	ref tokenrec p
	p:=alloctoken()
	p^:=nextlx
	p^.nexttoken:=nil

	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^^.nexttoken:=p
	fi
	p^.nexttoken:=nil

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
			ulistx^^.nexttoken:=tk
		fi
		tk^.nexttoken:=nil
		ulistx^:=tk

		seq:=seq^.nexttoken
	od
end

proc addlistmparam(ref ref mparamrec ulist,ulistx,ref mparamrec p)=
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^^.nextmparam:=p
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
	stname^.lineno:=nextlx.lineno+int(getfileno())<<24

!if stname^.symbol<>namesym then
		stname^.oldsymbol:=stname^.symbol
!fi
	stname^.symbol:=namesym
	stname^.nameid:=macroid
	nparams:=0

	if lxsptr^='(' then
		++lxsptr
		stlist:=stlistx:=nil
		stname^.attribs.ax_flmacro:=1

		lexreadtoken()
		do
			case nextlx.symbol
			when namesym then			!next param
				d:=nextlx.symptr
				p:=stlist
				while p do
					if p^.def=d then
						lxerror("Dupl macro param")
					fi
					p:=p^.nextmparam
				od
				q:=pcm_alloc(mparamrec.bytes)
				q^.def:=d
				q^.nextmparam:=nil
				addlistmparam(&stlist,&stlistx,q)
				++nparams
				lexreadtoken()
!			(d^.name+d^.namelen)^:=0			!zero-term param name; it might be an identifier
				if nextlx.symbol=commasym then
					lexreadtoken()
				fi
			when rbracksym then
				exit
			when ellipsissym then					!I need to create a special symbol name
				d:=addnamestr("__VA_ARGS__")
				stname^.attribs.ax_varparams:=1		!flag macro as having a va/args as last param
				lexreadtoken()
				if nextlx.symbol<>rbracksym then
					lxerror("')' expected")
				fi

				q:=pcm_alloc(mparamrec.bytes)
				q^.def:=d
				q^.nextmparam:=nil
				addlistmparam(&stlist,&stlistx,q)
				++nparams
				exit
			else
				lxerror("macro params?")
			esac
		od
		stname^.mparamlist:=stlist
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
			p:=stname^.mparamlist
			paramno:=1
			while p do
				if p^.def=nextlx.symptr then
					nextlx.flags ior:=tk_parammask
					nextlx.paramno:=paramno
					exit
				fi
				p:=p^.nextmparam
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

	stname^.tokenlist:=tklist
	stname^.attribs.ax_nparams:=nparams
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
		if macrostack^.symptr=d then return 1 fi
		macrostack:=macrostack^.nexttoken
	od
	return 0
end

proc showtokens(ichar caption,ref tokenrec tk)=
	print caption,,"<"
	while tk do
		showtoken(tk)
		tk:=tk^.nexttoken
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
	tk:=tk^.nexttoken
end

proc lexm=
!wrapper around lexreadtoken that applies macro expansion to names
	ref strec d
	static int doreset=0
	int newlineno

	do
		if tkptr then
			nextlx:=tkptr^
			tkptr:=tkptr^.nexttoken
			if tkptr=nil then

				if nextlx.symbol=namesym and nextlx.symptr^.nameid=macroid and peeklb() then
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
	TEST1::

		case nextlx.symbol
		when lexhashsym then

			if dolexdirective() then
				return
			fi
			next
		when namesym then
			d:=nextlx.symptr
			case d^.symbol
			when predefmacrosym then
				sfileno:=getfileno()
				slineno:=nextlx.lineno
				expandpredefmacro(d^.subcode,&nextlx,nextlx.lineno)
				doreset:=1					!can screw up line/file numbers
				return
			else
				if d^.nameid<>macroid or noexpand then
					return
				fi
			esac
		else
			return
		esac
!have a macro. Now see whether this should be expanded
		sfileno:=getfileno()
		slineno:=nextlx.lineno
		if d^.attribs.ax_flmacro then		!function-like macro; need to peek for "("
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
!only a simplistic approach is used, eg. 0 or 1 space then a "(" must be next
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
	tk:=tk^.nexttoken
	if tk=nil then			!nothing follows
		return 0
	fi
	if tk^.symbol=lbracksym then
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

	p:=tk:=m^.tokenlist

	iscomplex:=useshh:=0
	while p do
		if p^.symbol=namesym then
			d:=p^.symptr
			if d^.nameid=macroid or d^.symbol=predefmacrosym then
				iscomplex:=1
				exit
			fi
		elsif p^.symbol=hashhashsym then
			iscomplex:=useshh:=1
			exit
		fi

		p:=p^.nexttoken
	od

	if not iscomplex then
		return tk
	fi

	newmacro.symptr:=m				!add m to macrostack
	newmacro.nexttoken:=macrostack

	if useshh then
		repl:=substituteargs(m,nil,nil,0,nil)
	else
		repl:=m^.tokenlist
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
!	expargs[i]:=scantokenseq(args[i], macrostack)
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

	reenter::
	expanded:=0

	newtk:=newtkx:=nil
	noexpandflag:=0

	simple:=1
	oldtk:=tk

	while tk do
		case tk^.symbol
		when namesym then
			if tk^.symptr^.nameid=macroid or tk^.symptr^.symbol=predefmacrosym then
				simple:=0
				exit
			fi
		esac

		if tk=nil then exit fi
		tk:=tk^.nexttoken
	od

	if simple then
		return oldtk
	fi

	tk:=oldtk
	while tk do
		case tk^.symbol
		when namesym then
			m:=tk^.symptr
			if m^.nameid=macroid and not noexpandflag then
!macro detected; check if candidate for expansion
				if tk^.flags iand tk_macrolit or noexpand then
					goto simpletoken
				fi

				if inmacrostack(m,macrostack) then		!is an active macro name
					addlisttoken_copy(&newtk,&newtkx,tk)
					newtkx^.flags ior:= tk_macrolit
					goto skip

				fi
	simple:=0
				if m^.attribs.ax_flmacro then
					if not peektk(tk) then goto simpletoken fi
					lexa(tk)
					expandtk:=expandfnmacro(m,macrostack,tk,1,dummy)
					addlisttoken_seq(&newtk,&newtkx,expandtk)
					expanded:=1
					next
				else
					expandtk:=expandobjmacro(m,macrostack,tk,0)
					expanded:=1
					addlisttoken_seq(&newtk,&newtkx,expandtk)
				fi
			elsif m^.symbol=kdefinedsym then
				noexpandflag:=1
				goto simpletoken
			elsif m^.symbol=predefmacrosym then
				expandtk:=alloctokenz()
				expandpredefmacro(m^.subcode,expandtk,nextlx.lineno)
				addlisttoken_copy(&newtk,&newtkx,expandtk)
				goto skip2
			else
				noexpandflag:=0
				goto simpletoken
			fi
		else
	simpletoken::
			addlisttoken_copy(&newtk,&newtkx,tk)
		esac

	skip::
		if tk=nil then exit fi
	skip2::
		tk:=tk^.nexttoken
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

	nparams:=d^.attribs.ax_nparams
	nargs:=0
	if nparams=0 then				!) must follow
		lexa(tksource)
		if nextlx.symbol<>rbracksym then lxerror("rmc: ')' expected") fi
		return 0					!no args
	fi

	paramno:=1
	lbcount:=1
	tklist:=tklistx:=nil
	usesvargs:=d^.attribs.ax_varparams			!whether macro contains ... va/args
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
					tklist^.symbol:=placeholdersym
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
					tklist^.symbol:=placeholdersym
				fi
				args^[paramno]:=tklist				!store this list
				exit
			fi
		else
	addtoken::
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

	const maxhashhash=100
	[maxhashhash]ref tokenrec hhpoints
	int nhashhash

	params:=m^.mparamlist
	seq:=seqstart:=m^.tokenlist		!input token sequence

	newtk:=newtkx:=nil				!output token sequence
	nhashhash:=0
	lasttoken:=nil

	while seq do
		case seq^.symbol
		when hashsym then
			if nargs then
				seq:=seq^.nexttoken
				if seq=nil then lxerror("# at end") fi
				unless seq^.flags iand tk_parammask then
					lxerror("# not followed by param")
				end unless
				n:=seq^.paramno

				stringify(args^[n],&tk)

				addlisttoken_copy(&newtk,&newtkx,&tk)
			else
				addlisttoken(&newtk,&newtkx,seq)
				newtkx^.symbol:=lithashsym				!change to #'
			fi
		when hashhashsym then
			if seq=seqstart then lxerror("## at start") fi
			if nhashhash>=maxhashhash then lxerror("Too many ##") fi
			hhpoints[++nhashhash]:=newtkx

		elsif seq^.symbol=namesym and seq^.flags iand tk_parammask and nargs then		!args can be () if no "(...)" followed
			n:=seq^.paramno
			if seq^.nexttoken and seq^.nexttoken^.symbol=hashhashsym or \
			   lasttoken and lasttoken^.symbol=hashhashsym then
				addlisttoken_seq(&newtk,&newtkx,args^[n])
			else
				tkexp:=expargs^[n]
				if tkexp=nil then
					tkexp:=expargs^[n]:=scantokenseq(args^[n],macrostack,expanded)
				fi
				addlisttoken_seq(&newtk,&newtkx,tkexp)
			fi

		else
	doother::
			addlisttoken_copy(&newtk,&newtkx,seq)
		esac

		lasttoken:=seq
		seq:=seq^.nexttoken
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
	doname::
		length:=l.symptr^.namelen
		return l.symptr^.name

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
!length:=1
!return " "
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
	convertstring(s,u,length)
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

	if lp^.symbol=eolsym and lasttoken=eolsym then
		return
	fi

	s:=strtoken(lp,length)

	if forcespace or needspace(lasttoken,lp^.symbol) then
		gs_char(dest,' ')
	fi

	gs_strn(dest,s,length)


	lasttoken:=lp^.symbol
end

global proc showtoken(ref tokenrec lp)=
	static strbuffer buffer
	static ref strbuffer dest=&buffer

	gs_init(dest)
	
	emittoken(lp,dest)
	
print dest^.length:"v",,dest^.strptr:".*"
end

proc stringify(ref tokenrec seq,dest)=
!stringify single or multiple token sequence, and store result as a single
!string token in dest
	ref char s
	int length,addspace
	static strbuffer buffer
	static ref strbuffer deststr=&buffer

	dest^.symbol:=stringconstsym
	dest^.nexttoken:=nil

	if seq^.nexttoken=nil then		!single
		s:=strtoken(seq,length)
		dest^.length:=length
		dest^.svalue:=s
		return 
	fi

!now do multiple tokens into one string
	gs_init(deststr)
	lasttoken:=0
	addspace:=0
	while seq do
		emittoken(seq,deststr,forcespace:addspace)
		addspace:=1
		seq:=seq^.nexttoken
	od

	dest^.length:=length
	dest^.svalue:=deststr^.strptr
	dest^.length:=deststr^.length
end

proc pastetokens(ref tokenrec tk, &tknext)=
!tk points into a token sequence
!paste the token at tk with the one at tk^.nexttoken, and replace
!tk with the new composite token; tk^.nexttoken is removed
!tknext is either nil, or refers to the next pair of tokens to be pasted together;
!there is a problem when tk^.nexttoken and tknext coincide, so something needs to
!be done in that case (set tknext to point to tk)

	ref tokenrec tk2
	int length1,length2
	ref char s,t,u
	tokenrec oldtoken,token
	ref char oldlxsptr
	int oldlx_stackindex

	tk2:=tk^.nexttoken
	if tk2=tknext then tknext:=tk fi
	tk^.nexttoken:=tk2^.nexttoken				!lose second token

	if tk^.symbol=placeholdersym then
		if tk2^.symbol=placeholdersym then			!two placeholders; leave only first
		else										!ph/token; use second
			tk^:=tk2^								!also unlinks the tk2 token
		fi
	elsif tk2^.symbol=placeholdersym then			!token/ph; leave only first
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
!		lxerror("token-paste error")
		fi

		nextlx:=oldtoken
		lxsptr:=oldlxsptr
		lx_stackindex:=oldlx_stackindex

		token.nexttoken:=tk^.nexttoken
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
		case nextlx.symptr^.symbol
		when kdefinedsym then
			noexpand:=1
			lb:=0
			lexm()
			if nextlx.symbol=lbracksym then
				lb:=1;
				lexm()
			fi
			if nextlx.symbol<>namesym then lxerror("defined?") fi
			res:=nextlx.symptr^.nameid=macroid
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
			case nextlx.symptr^.symbol
			when ktypespecsym then
				res:=typespecsizes[nextlx.symptr^.subcode]
			else
				lxerror("sizeof2")
			esac
			lexm()
			if nextlx.symbol<>rbracksym then lxerror("')' expected") fi
			lexm()
	
		else
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
	printstrn(nextlx.svalue,nextlx.length); PRINTLN
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
	if d^.nameid=macroid then
		res:=1
	elsif d^.symbol=predefmacrosym then
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
		nexttk:=tk^.nexttoken
		tk:=nexttk
	od
end

global proc fastreadtoken=
!read next token into nextlx
	int c,csum,hsum,commentseen,dodir,j
	ref char pstart,p
	ichar ss

	nextlx.subcodex:=0

	doswitch lxsptr++^
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
	++NALLLINES
		++nextlx.lineno
		nextlx.symbol:=eolsym
		nextlx.length:=0
		++lxsptr				!skip lf
	when lf then			!only lfs not preceded by cr
		++nextlx.lineno
++NALLLINES
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
	tk^.nexttoken:=nil
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

		tk^.symbol:=stringconstsym
		tk^.svalue:=pcm_copyheapstring(&.str)

	when pdm_time then
		os_getsystime(&tm)

		fprint @&.str,"#:#:#",tm.hour:"2",tm.minute:"z2",tm.second:"z2"

		tk^.symbol:=stringconstsym
		tk^.svalue:=pcm_copyheapstring(&.str)
	when pdm_file then
		tk^.symbol:=stringconstsym
		fileno:=getfilenox(tk)
		if fileno=0 then fileno:=sfileno fi
		if sfileno then
			tk^.svalue:=sourcefilenames[sfileno]
		else
			tk^.svalue:="(File not available)"
		fi
	when pdm_func then
		tk^.symbol:=stringconstsym
		if currproc then
			tk^.svalue:=currproc^.name
		else
			tk^.svalue:="???"
		fi
	when pdm_line then
		tk^.symbol:=intconstsym
		tk^.value:=lineno
	when pdm_stdc then
		tk^.symbol:=intconstsym
		tk^.value:=1
	when pdm_bcc then
		tk^.symbol:=intconstsym
		tk^.value:=1
	else
		println pdmcode
		lxerror("PDM")
	esac

	if tk^.symbol=stringconstsym then
		tk^.length:=strlen(tk^.svalue)
		tk^.subcode:=trefchar
	else
		tk^.subcode:=tsint
		s:=pcm_alloc(16)
!	sprintf(s,"%lld",tk^.value)
		getstrint(tk.value,s)
		tk^.length:=strlen(s)
		if npastedtokens>=maxpastedtokens then
			lxerror("2:Too many pasted tokens")
		fi
		pastedtokenlist[++npastedtokens]:=s
		setfilenox(tk,0)
		tk^.pasteno:=npastedtokens
	fi
end

proc dopragmadir=
	lexm()
	if nextlx.symbol=namesym then
		if memcmp(nextlx.symptr^.name,"pack",4)=0 then
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
		elsif memcmp(nextlx.symptr^.name,"$callback",9)=0 then
			callbackflag:=1
		fi
	fi
finish::
	while nextlx.symbol<>eolsym and nextlx.symbol<>eofsym do lexm() od
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
	++NINCLUDES
	stacksourcefile("bcc.h",1)
	if dheaderfile then
		stacksourcefile(dheaderfile,1)
	fi
end

proc addautomodule(ichar headername,int fileno)=
	ichar cfilename
	ichar headerfile
	int present

	headerfile:=sourcefilepaths[fileno]

	if not fautomodules then
		return
	fi

	if eqstring(extractext(headerfile),"c") then		!ignore .c files
		return
	fi

	cfilename:=changeext(headerfile,"c")
	if checkfile(cfilename) then
		present:=1
		for i:=1 to nautomodules do
			if eqstring(automodulenames[i],cfilename) then
				present:=0
				exit
			fi
		od
		if present then
			automodulenames[++nautomodules]:=pcm_copyheapstring(cfilename)
		fi
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

	tk^.fileno:=fileno iand 255
	tk^.numberoffset := (tk^.numberoffset iand 0xFFFFFF) ior (fileno iand 0xFF00)<<16
end

function getfileno:int=
	return (nextlx.numberoffset>>24)<<8 ior nextlx.fileno
end

function getfilenox(ref tokenrec tk)int=
	return (tk^.numberoffset>>24)<<8 ior tk^.fileno
end

function getnumberoffsetx(ref tokenrec tk)int=
	return tk^.numberoffset iand 0xFFFFFF
end

global proc freehashtable=
!free the user name entries in the hash table
!leave reserved words etc alone
	ref strec d,e,f

	for i:=0 to hstmask do
		d:=hashtable^[i]
		if d^.name and d^.symbol=namesym then
			if d^.nameid=macroid then
				freetokens(d^.tokenlist)
			fi
			f:=d^.nextdupl
			while f do
				freestentry(f)
				e:=f^.nextdupl
				pcm_free(f,strec.bytes)
				f:=e
			od
			pcm_clearmem(hashtable^[i],strec.bytes)
		elsif d^.name then
			d^.nextdupl:=nil
		fi
	od
end

proc freestentry(ref strec d)=
end

proc regenlookup(ref strec d)=
	int j, wrapped,length
	ref strec e

	j:=gethashvalue(d^.name,d^.namelen) iand hstmask
	wrapped:=0

	do
		e:=hashtable^[j]
		length:=e^.namelen

		if not length then
PCM_FREE(HASHTABLE^[J],STREC.BYTes)
			hashtable^[j]:=d
			++nhstsymbols
			return
		fi

		if length=d^.namelen then	!match on length
			if memcmp(e^.name,d^.name,length)=0 then	!match
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

proc printhashtable(ichar caption)=
	ref strec d

	println caption,,":"
	for i:=0 to  hstsize-1 do
		d:=hashtable^[i]
		if d^.name then
	IF EQSTRING(D^.NAME,"char") then PRINTLN "********************** CHAR" FI
			println i,":",d^.name
		else
			println i,": ----"
		fi
	IF I REM 30=0 THEN OS_GETCH() FI
	od
	println
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
		if d^.name then
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
	when 'f','F' then
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
	nextlx.subcode:=tdouble
	nextlx.xvalue:=x

	setnumberoffset(intstart-lxstart)
	nextlx.length:=lxsptr-intstart
end
=== cc_headers.m 10/73 ===
!Built-in standard headers

import clib
import mlib

ichar bcclibstr = strinclude "bcclib.asm"

ichar h_assert		=	strinclude "headers/assert.h"
ichar h_ctype		=	strinclude "headers/ctype.h"
ichar h_errno		=	strinclude "headers/errno.h"
ichar h_fenv		=	strinclude "headers/fenv.h"
ichar h_float		=	strinclude "headers/float.h"
ichar h_inttypes	=	strinclude "headers/inttypes.h"
ichar h_stdint		=	strinclude "headers/stdint.h"
ichar h_limits		=	strinclude "headers/limits.h"
ichar h_locale		=	strinclude "headers/locale.h"
ichar h__ansi		=	strinclude "headers/_ansi.h"
ichar h_math		=	strinclude "headers/math.h"
ichar h_setjmp		=	strinclude "headers/setjmp.h"
ichar h_signal		=	strinclude "headers/signal.h"
ichar h_stdarg		=	strinclude "headers/stdarg.h"
ichar h_stdbool		=	strinclude "headers/stdbool.h"
ichar h_stddef		=	strinclude "headers/stddef.h"
ichar h_stdio		=	strinclude "headers/stdio.h"
ichar h_stdlib		=	strinclude "headers/stdlib.h"
ichar h__syslist	=	strinclude "headers/_syslist.h"
ichar h_string		=	strinclude "headers/string.h"
ichar h_time		=	strinclude "headers/time.h"
ichar h_utime		=	strinclude "headers/utime.h"
ichar h_unistd		=	strinclude "headers/unistd.h"
ichar h_safelib		=	strinclude "headers/safelib.h"
ichar h_wchar		=	strinclude "headers/wchar.h"
ichar h_wctype		=	strinclude "headers/wctype.h"
ichar h_systypes	=	strinclude "headers/sys/types.h"
ichar h_sysstat		=	strinclude "headers/sys/stat.h"
ichar h_systimeb	=	strinclude "headers/sys/timeb.h"
ichar h_sysutime	=	strinclude "headers/sys/utime.h"
ichar h_memory		=	strinclude "headers/memory.h"
ichar h_windows		=	strinclude "headers/windows.h"
ichar h_fcntl		=	strinclude "headers/fcntl.h"
ichar h_io			=	strinclude "headers/io.h"
ichar h_direct		=	strinclude "headers/direct.h"
ichar h_process		=	strinclude "headers/process.h"
ichar h_malloc		=	strinclude "headers/malloc.h"
ichar h_bcc			=	strinclude "headers/bcc.h"
ichar h_conio		=	strinclude "headers/conio.h"
ichar h_winsock2	=	strinclude "headers/winsock2.h"
ichar h__mingw		=	strinclude "headers/_mingw.h"
!ivar char h_shellapi	=	strinclude "headers/shellapi.h"
ichar h_windowsx	=	strinclude "headers/windowsx.h"

global tabledata []ichar stdhdrnames, []ref ichar stdhdrtext =
	("bcc.h",		&h_bcc),
	("assert.h",	&h_assert),
	("ctype.h",		&h_ctype),
	("errno.h",		&h_errno),
	("fenv.h",		&h_fenv),
	("float.h",		&h_float),
	("inttypes.h",	&h_inttypes),
	("stdint.h",	&h_stdint),
	("limits.h",	&h_limits),
	("locale.h",	&h_locale),
	("_ansi.h",		&h__ansi),
	("math.h",		&h_math),
	("setjmp.h",	&h_setjmp),
	("signal.h",	&h_signal),
	("stdarg.h",	&h_stdarg),
	("stdbool.h",	&h_stdbool),
	("stddef.h",	&h_stddef),
	("stdio.h",		&h_stdio),
	("stdlib.h",	&h_stdlib),
	("_syslist.h",	&h__syslist),
	("string.h",	&h_string),
	("time.h",		&h_time),
	("utime.h",		&h_utime),
	("unistd.h",	&h_unistd),
	("safelib.h",	&h_safelib),
	("wchar.h",		&h_wchar),
	("wctype.h",	&h_wctype),
	("sys/types.h",	&h_systypes),
	("sys/stat.h",	&h_sysstat),
	("sys/timeb.h",	&h_systimeb),
	("sys/utime.h",	&h_sysutime),
	("malloc.h",	&h_malloc),
	("windows.h",	&h_windows),
	("fcntl.h",		&h_fcntl),
	("io.h",		&h_io),
	("direct.h",	&h_direct),
	("process.h",	&h_process),
	("memory.h",	&h_memory),
	("conio.h",		&h_conio),
	("winsock2.h",	&h_winsock2),
	("_mingw.h",	&h__mingw),
!	("shellapi.h",	&h_shellapi),
	("windowsx.h",	&h_windowsx)
end

global function findheader(ichar name)ichar=
	int i
	[256]char newname
	ichar s,t

	if strchr(name,'\\') then
		s:=name; t:=&.newname
		while s^ do
			if s^='\\' then
				t++^:='/'
			else
				t++^:=s^
			fi
			++s
		od
		t^:=0
		name:=&.newname
	fi

	for i:=1 to stdhdrnames.len do
		if eqstring(name,stdhdrnames[i]) then
			return stdhdrtext[i]^
		fi
	od
	return nil
end

global proc writeheaders=
filehandle f
ichar ifile
int i
for i:=1 to stdhdrnames.len do
	ifile:=changeext(stdhdrnames[i],"hdr")
	println "Writing internal",stdhdrnames[i],"as",ifile
	f:=fopen(ifile,"wb")
	fwrite(stdhdrtext[i]^,1,strlen(stdhdrtext[i]^),f)
	fclose(f)
od
end

global proc checkbcclib=
const libfile="bcclib.asm"
filehandle f

if not checkfile(libfile) then
	println "Writing",libfile
	f:=fopen(libfile,"wb")
	fwrite(bcclibstr,1,strlen(bcclibstr),f)
	fclose(f)
fi
end

global function getbcclib:ichar=
	ichar s
	int slen

	slen:=strlen(bcclibstr)
	s:=malloc(slen+1)
	memcpy(s,bcclibstr,slen+1)

	return s
end

global function isheaderfile(ichar file)int=
for i:=1 to stdhdrnames.len do
	if eqstring(stdhdrnames[i],file) then
		return 1
	fi
od
return 0
end
=== cc_lib.m 11/73 ===
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lex

global int autotypeno=0
int currlineno
global int nextafindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=50000
ref unitrec unitheapptr=nil
int remainingunits=0

function newstrec:ref strec=
ref strec p
p:=pcm_alloc(strec.bytes)
memset(p,0,strec.bytes)

p^.lineno:=lx.lineno+int(lx.fileno)<<24

p^.attribs.ax_moduleno:=currmoduleno
return p
end

global proc initcclib=

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
attribrec attrs
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

if p^.blockno then
!	sprintf(&.str,"%s.%d",p^.name,int32(p^.blockno))
	print @&.str,p^.name,,".",,p^.blockno

	gs_leftstr(d,&.str,28-offset,'-')
else
	gs_leftstr(d,p^.name,28-offset,'-')
fi
gs_leftstr(d,namenames[p^.nameid],12,'.')
col:=gs_getcol(d)
attrs:=p^.attribs

gs_str(d,"[")

gs_str(d,scopenames[p^.scope])
gs_str(d," ")

if attrs.ax_static then
	gs_str(d,"Stat")
fi
if attrs.ax_align then
	gs_str(d,"@@")
	gs_strint(d,attrs.ax_align)
	gs_str(d," ")
fi
if attrs.ax_varparams then
	gs_str(d,"Var ")
fi
if attrs.ax_used then
	gs_str(d,"Used ")
fi
if attrs.ax_forward then
	gs_str(d,"Fwd ")
fi
if attrs.ax_frame then
	gs_str(d,"Frm ")
fi
if attrs.ax_autovar then
	gs_str(d,"AV ")
fi
if attrs.ax_nparams then
!	sprintf(&.str,"Pm:%d ",int32(attrs.ax_nparams))
	fprint @&.str,"Pm:# ",attrs.ax_nparams

	gs_str(d,&.str)
fi
if attrs.ax_moduleno then
!	sprintf(&.str,"M#%d ",int32(attrs.ax_moduleno))
	fprint @&.str,"M# ",attrs.ax_moduleno
	gs_str(d,&.str)
fi
if attrs.ax_equals then
	gs_str(d,"= ")
fi
gs_str(d,"]")
gs_padto(d,col+10,'=')

if p^.owner then
!	sprintf(&.str,"(%s)",p^.owner^.name)
	fprint @&.str,"(#)",p.owner.name
	gs_leftstr(d,&.str,18,' ')
else
	gs_leftstr(d,"()",18,' ')
fi

case p^.mode
when tvoid then
	gs_str(d,"Void ")
else
	gs_strsp(d,Strmode(p^.mode))
esac

case p^.nameid
when fieldid then
	gs_str(d,"Offset:")
	gs_strint(d,p^.offset)

when frameid,paramid then
	if p^.code then
		gs_str(d,"=")
!CPL =P^.CODE,JTAGNAMES[P^.CODE^.TAG]
!STREXPR(P^.CODE)

		gs_strvar(d,strexpr(p^.code))
	fi
	gs_str(d," Offset: ")
	gs_strint(d,p^.offset)

when procid then

	gs_str(d,"Index:")
	gs_strint(d,p^.index)
	gs_str(d," Address:")
!	sprintf(&.str,"%p",ref void(p^.address))
	print @&.str,ref void(p^.address)
	gs_str(d,&.str)
	if p^.attribs.ax_callback then
		gs_str(d,"<callback fn>")
	fi

when enumid then
	gs_str(d,"Enum:")
	gs_strint(d,p^.index)

when staticid then
	if p^.code then
		gs_str(d,"=")
		gs_strvar(d,strexpr(p^.code))
	fi
	gs_str(d,"STATIC********")
esac

gs_str(d," ")

gs_str(d,"Lineno:")
gs_strint(d,p^.lineno iand 16777215)
gs_str(d," ")
gs_str(d,sourcefilenames[p^.lineno>>24])

if p^.nameid=procid then
	gs_line(d)
	pm:=p^.paramlist
	while pm do
		gs_str(d,"		Param: ")
		gs_leftstr(d,(pm^.def|pm^.def^.name|"Anon"),10,'-')
!		gs_leftstr(d,Strmode(pm^.mode),16, ' ')
		gs_str(d,pmflagnames[pm^.flags])

		gs_line(d)
		pm:=pm^.nextparam
	od
fi

gs_println(d,f)

if p^.code then
	case p^.nameid
	when frameid,staticid then
		printunit(f,p^.code,-3)
	esac
fi

end

global proc printstflat(filehandle f)=
int i
ref strec p
ref tokenrec lx
println @f,"GLOBAL SYMBOL TABLE:"

for i:=0 to hstsize-1 do
!cpl i
!	if hashtable[i].name and hashtable[i].symbol=namesym then
	p:=hashtable^[i]
	if p^.name then
		case p^.symbol
		when namesym then
!			println @f,i,p,":",p^.name,symbolnames[p^.symbol],namenames[p^.nameid]
			println @f,i,p,":",getstname(p),symbolnames[p^.symbol],namenames[p^.nameid]
			p:=p^.nextdupl
			while p do
				print   @f,"	",p,getstname(p),symbolnames[p^.symbol],namenames[p^.nameid],
					p^.prevdupl
				println @f,"(From",(p^.owner|getstname(p^.owner)|"-"),,")"
				p:=p^.nextdupl
			od
!		else
!			println @f,"not showing",p^.name
		esac
	fi
od
end

global function createname(ref strec p)ref unitrec=
ref unitrec u

u:=allocunitrec()

u^.tag:=j_name
u^.def:=p
u^.simple:=1

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
u^.tag:=tag
u^.a:=p
return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
ref unitrec u

u:=allocunitrec()

u^.tag:=tag
u^.a:=p
u^.b:=q
return u
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
ref unitrec u

u:=allocunitrec()
u^.tag:=tag
u^.a:=p
u^.b:=q
u^.c:=r
return u
end

global function createconstunit(word64 a, int t)ref unitrec=
ref unitrec u
u:=allocunitrec()
u^.tag:=j_const
u^.value:=a
u^.mode:=t
u^.simple:=1
return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
ref unitrec u
u:=allocunitrec()
u^.tag:=j_const
u^.svalue:=s
u^.mode:=trefchar
if length=-1 then
	u^.slength:=strlen(s)
else
	u^.slength:=length
fi
u^.isstrconst:=1
u^.simple:=1
return u
end

global function createwstringconstunit(ref word16 s, int length)ref unitrec=
ref unitrec u
u:=allocunitrec()
u^.tag:=j_const
u^.wsvalue:=s
u^.mode:=trefwchar
!if length=-1 then
!	u^.slength:=strlen(s)
!else
	u^.wslength:=length
!fi
u^.iswstrconst:=1
u^.simple:=1
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

global function getconstvalue(ref unitrec p,int ID=0)int64=	!GETCONSTVALUE
!extract value from kconst
if p and p^.tag=j_const then
	return p^.value
fi
serror("GCV Not constant")
return 0
end

global function nextautotype:ichar=
static [32]char str

!sprintf(&.str,"$T%d",int32(++autotypeno))
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

return newm
end

global function createrefmode(int m)int=
!create ref version of mode m (including when m is already a ref)
int newm

if ttreftype[m] then return ttreftype[m] fi
newm:=createnewmode(tref)
ttreftype[m]:=newm
tttarget[newm]:=m
ttisref[newm]:=1
return newm
end

global function createprocmode(int m, ref paramrec pm)int=
!create proc mode with return type
int newm

newm:=createnewmode(tproc)
ttparams[newm]:=pm
tttarget[newm]:=m
return newm
end

global function createarraymode(int m, length)int=
!create array of mode m (including when m is already a ref)
int newm
newm:=createnewmode(tarray)
tttarget[newm]:=m
ttlength[newm]:=length
ttsize[newm]:=length*ttsize[m]

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

return newm
end

global proc setnameptr(ref unitrec p)=		!SETNAMEPTR
!p is a just created j_...def unit which has a nameptr in the .a parameter
!set up an xref from the strec back to the -def unit
!Set up a pointer in the associated strec that points back to q

p^.def^.code:=p
end

global proc printcode_all(filehandle f,ichar caption)=
int i
ref strec p

for i:=1 to nmodules do
	printcode(f,caption,i)
od
end

global proc printcode(filehandle f,ichar caption,int n)=
int i
ref strec p

!CPL "PRINTCODE",F

p:=moduletable[n].stmodule^.deflist

println @f, caption, "MODULE:",moduletable[n].name

while p do
	case p^.nameid
	when procid then
!		if p^.scope<>imported_scope and p^.code then
		if p^.code then
			println @f,p^.name,,"=",scopenames[p^.scope]
			printunit(f,p^.code,,"1")
			println @f
		fi
	esac
	p:=p^.nextdef
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

if p^.tag>=j_dummy then
	println "print unit: bad tag",p^.tag
	os_getch()
	stop 30
fi

if p^.lineno then
	currlineno:=p^.lineno
fi

lincr:=1
if level<0 then
	lincr:=-1
!	level:=-level
	print @dev,"             "
fi

!PRINT @DEV,P:"10",," "

print @dev,getprefix(abs(level),prefix,p)
idname:=jtagnames[p^.tag]
if idname^='j' then idname+:=2 fi

print @dev,idname,,": "

case p^.tag
when j_name, j_funcname then
	d:=p^.def

	print @dev,d^.name,namenames[d^.nameid]

	if d^.code then
		print @dev," {",,jtagnames[d^.code^.tag],,"}"
	fi

	print @dev," ",,getdottedname(d)!,q

	if p^.c then
		print @dev," Lastcall:",p^.c
	fi

when j_tempdecl, j_decl, j_goto then

	d:=p^.def
	print @dev,d^.name,namenames[d^.nameid]

	println @dev
	printunit(dev,d^.code,level+lincr,"1")
	return

when j_goto then

	d:=p^.def
	print @dev,d^.name,namenames[d^.nameid]

when j_labelstmt then
	print @dev,p^.def^.name!,"+ LABELED STATEMENT"

when j_casestmt then
	print @dev,"Index:",p^.index

when j_const then
	t:=p^.mode
	if t=trefchar then
		if not p^.isstrconst then
			goto doref
		fi
dostring::
		if p^.slength>256 then
			print @dev,"""",,"(LONGSTR)",""" *",,p^.slength
		else
			print @dev,"""",,p^.svalue,,""" *",,p^.slength
		fi
	elsif t=trefwchar then
		if not p^.iswstrconst then
			goto doref
		fi
		print @dev,"""",,"(WSTRING)",""" *",,p^.wslength
	elsif t>=tschar and t<=tsllong then
		print @dev,p^.value
	elsif t>=tuchar and t<=tullong then
		print @dev,p^.uvalue
	elsif isrealcc(t) then
		print @dev,p^.xvalue
	elsif ttbasetype[t]=tref then
		if p^.isstrconst then
			goto dostring
		fi
doref::
		print @dev,ref void(p^.value)
	elsif ttbasetype[t]=tarray then
		if p^.isstrconst then
			goto dostring
		fi
		serror("PRINTUNIT/CONST/aRRAY")
	else
		cpl typename(t)
		serror("PRINTUNIT BAD CONST")
	fi
	print @dev," ",,Strmode(t)
	if p^.isstrconst then print @dev,"<STRCONST>" fi
	if p^.iswstrconst then print @dev,"<WSTRCONST>" fi

when j_convert then
	print @dev,convnames[p^.opcode]
	print @dev," "
	if p^.convmem then print @dev,"Mem:" fi
	print @dev,typename(p^.a^.mode)
	print @dev," => "
	if p^.convtomem then print @dev,"Mem:" fi
	print @dev,typename(p^.mode)

when j_scale then
	print @dev,"Scale:",p^.scale

when j_addptr,j_subptr then
	print @dev,"Ptrscale:",p^.ptrscale

when j_switch then
	pc:=p^.nextcase
	n:=0
	while pc do ++n; pc:=pc^.nextcase od

	print @dev,p^.nextcase,n

when j_callfn then
	print @dev," Aparams:",p^.aparams

when j_ptr then
!	if p^.memtype then
!		print @dev," Memtype:",Strmode(p^.memtype)
!	fi

when j_dot then
	print @dev," Offset:",p^.offset

esac

!if p^.simple then print @dev," <simple>" fi

if p^.alength then print @dev," ALENGTH=",p^.alength fi

println @dev

printunitlist(dev,p^.a,level+lincr,"1")
printunitlist(dev,p^.b,level+lincr,"2")
if p^.tag<>j_block then					!.c is used to point to last element
	printunitlist(dev,p^.c,level+lincr,"3")
fi
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
if p=nil then return fi

while p do
	printunit(dev,p,level,prefix)
	p:=p^.nextunit
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

!strcpy(&.indentstr,"-----------------")
strcpy(&.indentstr,"-----------------------")

modestr:=Strmode(p^.mode,0)
length:=strlen(modestr)
if length<strlen(&.indentstr) then
	memcpy(&.indentstr,modestr,length)
else
	strcpy(&.indentstr,modestr)
fi

to level do
	strcat(&.indentstr,"|---")
!	strcat(&.indentstr,"|------")
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

strcpy(&.str,p^.name)
owner:=p^.owner
while owner and owner^.nameid<>programid do
	strcpy(&.str2,&.str)
	strcpy(&.str,owner^.name)
	strcat(&.str,".")
	strcat(&.str,&.str2)
	owner:=owner^.owner
od
if p^.blockno then
!	sprintf(&.str2,".%d",int32(p^.blockno))
	print @&.str2,".",,p.blockno
	strcat(&.str,&.str2)
fi
return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
static [40]char str

!sprintf(&.str,"%05d ",int32(currlineno))
fprint @&.str,"# ",currlineno:"z5"
return &.str
end

global function getautofieldname:ref strec=
!create auto-field name and return pointer to st entry
[32]char str
ichar name

!sprintf(&.str,"$F%d",int32(++nextafindex))
print @&.str,"$F",,++nextafindex

name:=pcm_copyheapstring(&.str)
return addnamestr(name)
end

global proc convertstring(ichar s, t,int length=-1)=		!CONVERTSTRING
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
!			sprintf(&.str,"\\%03o",int32(c))
			fprint @&.str,"\\#o",c:"z3"
			t++^:=str[1]
			t++^:=str[2]
			t++^:=str[3]
			t++^:=str[4]
		else
			t++^:=c
		fi
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
ref unitrec q
[16000]char str
int lb,t

!CPL "JEVAL",P,JTAGNAMES[P^.TAG]

case p^.tag
when j_const then
	if (t:=p^.mode)=trefchar then
		if p^.slength=0 then goto doref fi		!might be initialised to something else
		if not p^.isstrconst then goto doref fi		!might be initialised to something else
		if p^.slength>str.len/2 then
			strcpy(&.str,"LONGSTR)")
		else
			convertstring(p^.svalue,&.str)
		fi
		gs_additem(dest,"""")
		gs_additem(dest,&.str)
		gs_additem(dest,"""")
		return
	elsif t>=tschar and t<=tsllong then
!		sprintf(&.str,"%lld",p^.value)
		getstrint(p.value, &.str)

	elsif t>=tuchar and t<=tullong then
!		sprintf(&.str,"%llu",p^.uvalue)
		strcpy(&.str,strword(p.uvalue))

	elsif t=tdouble or t=tfloat then
!		sprintf(&.str,"%f",p^.xvalue)
		strcpy(&.str,strreal(p.xvalue))
	else
		case ttbasetype[p^.mode]
		when tref then
doref::
!			sprintf(&.str,"%p",p^.svalue)
			print @&.str,ref void(p^.svalue)
		when tarray then
			strcpy(&.str,"ARRAY")
		else
			CPL typename(p^.mode)
			nxerror("EVAL/CONST",p)
		esac
	fi
	gs_additem(dest,&.str)

when j_name then
	gs_additem(dest,p^.def^.name)

when j_funcname then
	gs_str(dest,"&")
	gs_additem(dest,p^.def^.name)

when j_andl,j_orl,j_andand,j_eq,j_ne,j_lt,j_le,j_gt,j_ge,j_add,j_sub,j_mul,j_div,
		j_rem,j_iand,j_ior,j_ixor,j_shl,j_shr,
		j_addto,j_subto,j_multo,j_divto,
		j_remto,j_iandto,j_iorto,j_ixorto,j_shlto,j_shrto 	then

	strcpy(&.str,getopcjname(p^.tag))
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,&.str)
	jeval(dest,p^.b)
	gs_additem(dest,")")

when j_neg,j_abs,j_inot,j_notl,j_istruel then

	strcpy(&.str,getopcjname(p^.tag))
!	strcpy(&.str,"getopcjname(p^.tag)")
	gs_additem(dest,&.str)
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,")")

when j_callfn then
	jeval(dest,p^.a)
	gs_additem(dest,"(")

	q:=p^.b
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	gs_additem(dest,")")

when j_dot then
	jeval(dest,p^.a)
	gs_additem(dest,".")
GS_STR(DEST,"???")
!	jeval(dest,p^.b)

when j_idot then
	jeval(dest,p^.a)
	gs_additem(dest,"->")
	jeval(dest,p^.b)

when j_makelist,j_exprlist then
	lb:=p^.tag=j_exprlist
	gs_additem(dest,(lb|"("|"{"))

	q:=p^.a
	while q do
		jeval(dest,q)
		q:=q^.nextunit
		if q then gs_additem(dest,",") fi
	od
	gs_additem(dest,(lb|")"|"}"))

when j_assign then
	jeval(dest,p^.a)
	gs_additem(dest,"=")
	jeval(dest,p^.b)

when j_ifx then
	jeval(dest,p^.a)
	gs_additem(dest,"?")
	jeval(dest,p^.b)
	gs_additem(dest,":")
	jeval(dest,p^.c)

when j_convert then

	gs_additem(dest,Strmode(p^.mode))
	gs_additem(dest,"(")
	jeval(dest,p^.a)
	gs_additem(dest,")")

when j_ptr then
	gs_additem(dest,"*(")
	jeval(dest,p^.a)
	if p^.b then
		gs_additem(dest,"+")
		jeval(dest,p^.b)
	fi
	gs_additem(dest,")")

when j_block then
	gs_additem(dest,"<JBLOCK>")

when j_preincr then
	gs_additem(dest,"++")
	jeval(dest,p^.a)

when j_predecr then
	gs_additem(dest,"--")
	jeval(dest,p^.a)

when j_postincr then
	jeval(dest,p^.a)
	gs_additem(dest,"++")

when j_postdecr then
	jeval(dest,p^.a)
	gs_additem(dest,"--")


when j_null then
	gs_str(dest,"<nullunit>")

when j_scale then
	gs_str(dest,"scale((")
	jeval(dest,p^.a)
	if p^.scale>0 then
		gs_str(dest,")*")
		gs_strint(dest,p^.scale)
	else
		gs_str(dest,")/")
		gs_strint(dest,-p^.scale)
	fi
	gs_str(dest,")")

else
gs_str(dest,"<CAN'T DO JEVAL>")
end
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

global function Strmode(int m,expand=1)ichar=		!STRMODE
static [16384]char str

istrmode(m,expand,&.str)

return &.str
end

global function Strmode2(int m,expand=1)ichar=		!STRMODE
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
!		sprintf(dest,"[%d]",int32(ttlength[m]))
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
	strcpy(dest,typename(m))

when tproc then
	strcpy(dest,"proc[PM](")
	pm:=ttparams[m]
	n:=pm^.nparams
	for i to n do
		istrmode(pm^.mode,0,dest+strlen(dest))
		if i<>n then
			strcat(dest,",")
		fi
		pm:=pm^.nextparam
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

global function countunits(ref unitrec p)int=
int n
n:=0
while p do
	++n
	p:=p^.nextunit
od
return n
end

proc purgesymbol(ref strec p,prev,int del)=
!unlink and (when del=1) recover memory from st entry p
!p is removed from deflist chain of its owner, and from the dupllist chain
!prev is nil when p is the first entry in its owner's deflist, or points
!to the previous still existing entry in the chain. It is necessary so that
!the .nextdef link can be maintained, of either prev or owner
ref strec q

case p^.nameid
when fieldid then			!needed for genfieldtables
	return
esac

!Unlink child symbols
purgesymbollist(p^.deflist,0,del)

!unlink from deflist and continue deflist around it
if prev then
	prev^.nextdef:=p^.nextdef
else
	p^.owner^.deflist:=p^.nextdef
fi

!now need to unlink from dupllist. Here, the .prevdupl field will always
!be valid, pointing to the generic entry if necessary (that can't be unlinked
!here)

q:=p^.prevdupl
q^.nextdupl:=p^.nextdupl

!Now delete the entry
if del then
	pcm_free(p,strec.bytes)
fi
end

global proc purgesymbollist(ref strec p,int ismodule, del)=
!scan the list in p (the deflist of an owner symbol)
!and unlink a symbol when ismodule is 0 or it's not global
!when del=1, then also recover the memory used
!ismodule should be 1 for a module, then the global flag is checked

ref strec q,prev

serror("PURGESYMBOL")

end

global proc purgeprocs(ref strec p, int del)=
!scan procs in the list p, and remove frame vars

while p do
	if p^.nameid=procid then
		purgeproc(p,del)
	fi
	p:=p^.nextdef
od
end

global proc purgeproc(ref strec p, int del)=
!scan procs in the list p, and remove frame vars
ref strec q,prev,r

!NOTE: THIS CAN'T BE USED AT THE MINUTE, AS THE STRECS COMPRISING THE
!FRAME VARS CONTAIN FRAME OFFSETS NEEDED BY THE CODE GENERATOR.
!POSSIBLY, ENTRIES CAN BE UNLINKED INSTEAD, BUT CAN STILL BE POINTED
!TO BY REFERENCES WITHIN THE BYTE-CODE 

q:=p^.deflist
prev:=nil
while q do
	r:=q^.nextdef
	if q^.nameid=frameid then
		purgesymbol(q,prev,del)
	else
		prev:=q
	fi
	q:=r
od
end

global proc printmodelist(filehandle f)=		!PRINTMODELIST
const wtypeno	= 4
const wname		= 13
const wbasetype	= 13
const wbitsize	= 3
const wtarget	= 14
const wlength	= 4
const wsize		= 5
const wconst	= 3
const wrest		= 3
const wvolatile	= 3
const wused		= 3
const wconsttype= 5
const wreftype	= 5
const wnamedef	= 8
const wmode		= 32
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
gs_leftstr(dest,"Len",wlength)
gs_leftstr(dest,"Size",wsize)
gs_leftstr(dest,"C",wconst)
gs_leftstr(dest,"R",wrest)
gs_leftstr(dest,"V",wvolatile)
gs_leftstr(dest,"@Cnst",wconsttype)
gs_leftstr(dest,"@Ref",wreftype)
gs_leftstr(dest,"Tag",wnamedef)
gs_leftstr(dest,"Mode",wmode)
gs_println(dest,f)


for m:=0 to ntypes do
	gs_init(dest)
	gs_leftint(dest,m,wtypeno)
	gs_leftstr(dest,typename(m),wname)
	gs_leftstr(dest,typename(ttbasetype[m]),wbasetype)
	gs_leftint(dest,ttbitwidth[m],wbitsize)

	if tttarget[m] then
		gs_leftint(dest,tttarget[m],3)
		gs_leftstr(dest,typename(tttarget[m]),wtarget-3)
	else
		gs_leftstr(dest,"-",wtarget)
	fi

	case ttbasetype[m]
	when tarray,tstruct,tunion then
		gs_leftint(dest,ttlength[m],wlength)
	else
		gs_leftstr(dest,"",wlength)
	esac

	gs_leftint(dest,ttsize[m],wsize)
	gs_leftint(dest,ttconst[m],wconst)
	gs_leftint(dest,ttrestrict[m],wrest)
	gs_leftint(dest,ttvolatile[m],wvolatile)

	gs_leftint(dest,ttconsttype[m],wconsttype)
	gs_leftint(dest,ttreftype[m],wreftype)

	if ttnamedef[m] then
		gs_leftstr(dest,ttnamedef[m]^.name,wnamedef)
	else
		gs_leftstr(dest,"-",wnamedef)
	fi

	mstr:=Strmode(m)
	if strlen(mstr)<16 then
		gs_str(dest,mstr)
	else
		gs_println(dest,f)
		gs_init(dest)
		gs_str(dest,"		")
		gs_str(dest,mstr)
	fi
	gs_println(dest,f)
od

println @f
end

global function typename(int m)ichar=
int basem
static [300]char str

basem:=ttbasetype[m]
case basem
when tstruct,tunion then
	strcpy(&.str,(basem=tstruct|"struct "|"union "))
	if ttnamedef[m] then
		strcat(&.str,ttnamedef[m]^.name)
	fi
	return &.str
when tarray then
	return "<array>"
when tenum then
	if ttnamedef[m] then
		return ttnamedef[m]^.name
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
ref int64 q
int nwords

if remainingunits-- then
	p:=unitheapptr
	++unitheapptr
	p^.lineno:=lx.lineno
	if lx.fileno<=255 then
		p^.fileno:=lx.fileno
	fi
	return p
fi

!need first or new heap
p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

memset(p,0,unitheapsize*unitrec.bytes)
remainingunits:=unitheapsize-1
++unitheapptr
p^.lineno:=lx.lineno
if lx.fileno<=255 then
	p^.fileno:=lx.fileno
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
ttrestrict[ntypes]:=ttrestrict[m]
ttvolatile[ntypes]:=ttvolatile[m]
ttusertype[ntypes]:=ttusertype[m]
ttsize[ntypes]:=ttsize[m]
ttbitwidth[ntypes]:=ttbitwidth[m]
tttarget[ntypes]:=tttarget[m]
ttparams[ntypes]:=ttparams[m]
ttisref[ntypes]:=ttisref[m]

return ntypes
end

function createnewmode(int m)int=
!create new type unitialised except for given basetype m

if ntypes>=maxtype then
CPL =maxtype
	serror("Too many types/cnm")
fi
++ntypes

!leave length, const etc all zero
!copy basic size info from basetype

ttbasetype[ntypes]:=m
ttsize[ntypes]:=ttsize[m]
ttbitwidth[ntypes]:=ttbitwidth[m]

return ntypes
end

global proc addlistunit(ref ref unitrec ulist,ulistx,ref unitrec p)=
!add strec p to end of linked list headed by ulist^. ulistx^ is current end of list
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextunit:=p
fi
p^.nextunit:=nil

ulistx^:=p			!update end-of-list pointer
end

global proc addlistdef(ref ref strec ulist,ulistx,ref strec p)=
!add strec p to end of linked list headed by ulist^. ulistx^ is current end of list
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextdef:=p
fi
p^.nextdef:=nil

ulistx^:=p			!update end-of-list pointer
end

global proc addlistparam(ref ref paramrec ulist,ulistx,ref paramrec p)=
!add paramrec p to end of linked list headed by ulist^. ulistx^ is current end of list
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextparam:=p
fi
p^.nextparam:=nil

ulistx^:=p			!update end-of-list pointer
end

global proc checksymbol(int symbol)=
[256]char str

if lx.symbol<>symbol then
!	sprintf(&.str,"%s expected, not %s",symbolnames[symbol],symbolnames[lx.symbol])
	fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]

if lx.symbol=namesym then
	strcat(&.str," \"")
	strcat(&.str,getstname(lx.symptr))
	strcat(&.str,"\"")
fi
!	serror(symbolnames[symbol]+" expected, not "+symbolnames[lx.symbol])
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
	ttbitwidth[i]:=bitsize

od
ntypes:=tlast-1

!trefchar:=createrefmode(tuchar)
!trefwchar:=createrefmode(tushort)

trefchar:=createrefmode(tschar)
trefwchar:=createrefmode(tsshort)

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
ntypesreset:=ntypes
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
p^.namespace:=namespaces[id]
if q:=symptr^.nextdupl then			!1st in dupl list
	q^.prevdupl:=p
fi
p^.nextdupl:=q
p^.prevdupl:=symptr
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

global function createnewproc(ref strec owner,symptr)ref strec=
!create new proc entry
!symptr is the generic st entry for proc's name
ref strec p,q

p:=createdupldef(owner,symptr,procid)

q:=p
while q:=q^.nextdupl do
	if q^.owner=owner then
		cpl q^.name,"in",owner^.name
		serror("Dupl proc name")
	fi
!	q:=q^.nextdupl
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
static int NNN=0

int nsblock

ref strec d

if symptr^.nameid>macroid then
	return symptr
fi

if blockno and blockcounts[blockno]=0 then blockno:=blockowner[blockno] fi

do							!loop for each block level
	nsblock:=ns<<16 ior blockno
	d:=symptr				!reset dupl list
	while d:=d^.nextdupl do
		if d^.owner=owner and d^.nsblock=nsblock then
			return d
		fi
	od

	if blockno=0 then
		case owner^.nameid
		when procid then			!was in function, now search filescope
				!(THIS MIGHT BE NEEDED FOR PARAM-SCOPES where block number is zero)
			owner:=stmodule
			redo
		when structtagid then		!was in struct; now try owner (proc/module/other struct)
			owner:=owner^.owner
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

while d:=d^.nextdupl do
	if d^.owner=owner and d^.nsblock=nsblock then
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

while (d:=d^.nextdupl) and d^.owner=owner do
	if d^.nsblock=nsblock then
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
	a:=ttnamedef[m]^.attribs.ax_align
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
cpl Strmode(m),A
serror("GETALIGN SIZE NOT 1248")

return 0
end

global function isexported(ref strec d)int=
if d^.nameid=procid then
	if d^.code and (d^.scope=imported_scope or d^.scope=exported_scope) then
		return 1
	fi
else
	if d^.scope=exported_scope then
		return 1
	fi
fi
return 0
end

global function isimported(ref strec d)int=
if d^.nameid=procid then
	if d^.code=nil and (d^.scope=imported_scope or d^.scope=exported_scope) then
		return 1
	fi
else
	if d^.scope=imported_scope then
		return 1
	fi
fi
return 0
end

global function isstructunion(int m)int=
case ttbasetype[m]
when tstruct,tunion then
	case ttsize[m]
	when 1,2,4,8 then
	else
	 return 1
	esac
esac
return 0
end

global function getstname(ref strec d)ichar=
static [256]char name
memcpy(&.name,d^.name,d^.namelen)
name[d^.namelen+1]:=0
return &.name
end

global function isrealcc(int m)int=
m:=ttbasetype[m]
return tfirstreal<=m<=tlastreal
end

global function isintcc(int m)int=
m:=ttbasetype[m]
return tfirstint<=m<=tlastint
end
=== cc_parse.m 12/73 ===
!Parse C Code

import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables

import cc_lex
import cc_lib

!const needcompoundblock=1
const needcompoundblock=0

ref strec ist_symptr

INT INSIDEFOR

const maxtypemods=20
[maxnestedloops]byte looptypestack		!contains either 'L' or 'S' (loop or switch)
int loopindex							!current level of nested loop/switch blocks
[maxnestedloops]ref caserec casevaluestack		!linked list of case values for current switch

byte iscallbackfnx
byte constantseen=0
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
	when kshowtypesym then
		lex()
		t:=readcasttype(d,0,pm)
		skipsymbol(semisym)
		println "Type is:",Strmode(t)
		next
	when kmccassertsym then
		nitems:=1
	when semisym then
		serror("Extra semicolon 2")
	esac
	wasenum:=lx.symbol

	if lx.symbol=kmccassertsym then nitems:=1 fi

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
			constantseen:=0
		elsif pm then
readfn::
			if lx.symbol=lcurlysym and commaseen then serror("fn def after comma") fi

			d:=readfunction(d,m,linkage,pm,wasdef)
			if wasdef then exit fi			!can't have comma-separate fn defs

		elsif ttbasetype[m]=tproc then
			pm:=ttparams[m]
			m:=tttarget[m]
			constantseen:=0
			goto readfn

		else
			d:=readmodulevar(d,m,linkage)
			constantseen:=0
		fi

		case lx.symbol
		when commasym then			!read next item
			commaseen:=1
			lex()
		else
			skipsymbol(semisym)
			exit
		esac
	when kconstantsym then
		constantseen:=1
		lex()
		next 2
	when kstructinfosym then
		readstructinfosym()
	else
		case ttbasetype[mbase]
		when tenum, tstruct, tunion then		!assume defining a [part]type only
			skipsymbol(semisym)
			exit
		when tsint then				!allow for now, as it migt be an enum decl with no name
			skipsymbol(semisym)
			exit
		else
			serror_s("Decl error %s",typename(mbase))
		esac
	enddocase

	if nitems=0 and fmodern then
		case ttbasetype[mbase]
		when tstruct,tunion,tenum then
		else
			if wasenum<>kenumsym then
				CPL =STRMODE(MBASE)
				serror("Empty declaration")
			fi
		esac
	fi

od
end

global function parsemodule(int n)int=
int size,t
ref strec owner
real tsecs

loopindex:=iscallbackfnx:=constantseen:=ingeneric:=0
ist_symptr:=nil
memset(&casevaluestack,0,casevaluestack.bytes)

startlex("PARSETEST",moduletable[n].fileno)
owner:=stmodule
currproc:=nil
loopindex:=0

lex()

!while lx.symbol<>eofsym do
!	lex()
!od
!RETURN 1

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
	int32 typeno				!not set, int, float, char, struct, union, enum etc
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
!	byte iscallback				!1 if $callback fnspec used
end
declrec d
unit p
int t,mod,m,fstruct
ref paramrec pm
ref strec e

memset(&d,0,d.bytes)
fstruct:=mod:=0

doswitch lx.symbol
when ktypespecsym then
	switch lx.subcode
	when ts_int, ts_char, ts_float, ts_double, ts_bool, ts_void then
		if d.typeno then
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
	when ts_complex then
		if d.typeno and d.typeno<>tfloat and d.typeno<>tdouble then
			goto tserror
		fi
		d.typeno:=tcomplex
	else

tserror::
		serror_s("declspec/ts %s",typespecnames[lx.subcode])
	endswitch
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
	when callback_fnspec then
		callbackflag:=1
!		d.iscallback:=1
	esac
	lex()
when kstructsym,kunionsym then
	if d.typeno then serror("struct?") fi
	d.typeno:=readstructdecl(owner)
	d.isusertype:=1
	fstruct:=1

when kenumsym then
	if d.typeno then serror("enum?") fi
!	d.typeno:=readenumdecl(owner)
	readenumdecl(owner)
	d.typeno:=tsint			!disregard enum 'type'; just use int
	d.isusertype:=1

when namesym then			!should resolve to see if a user-type ...
							! ... unless a basetype already seen
	if not d.typeno and (m:=isusertype(owner)) then
		if mod then			!unsigned etc without proper base type; assume name is not part o it
			d.typeno:=tsint
			exit
		fi
!		if mod then serror("Can't mod usertype") fi
		d.typeno:=m
		d.isusertype:=1
		lex()
	else
		if d.typeno=0 and not mod then
			serror_s("Implicit decls not allowed: %s",lx.symptr^.name)
		fi

		if d.typeno=0 then d.typeno:=tsint fi
		exit
	fi

when ktypeofsym then
	lex()
	skipsymbol(lbracksym)
	p:=readexpression()
	skipsymbol(rbracksym)
	if d.typeno or mod then serror("typeof") fi
	d.typeno:=p^.mode


when kmccassertsym then
	dostaticassert()
else
	exit
end doswitch

t:=(d.typeno|d.typeno|tsint)

if not d.isusertype then				!otherwise everything should be set up
	case t
	when tsint then
		if d.isshort then
			t:=(d.isunsigned|tushort|tsshort)
		elsif d.islong then
			if wintarget then
				t:=(d.isunsigned|tuint|tsint)
			else
				t:=(d.isunsigned|tullong|tsllong)
			fi
		elsif d.isllong then
			t:=(d.isunsigned|tullong|tsllong)
		elsif d.isunsigned then
			t:=tuint
		fi
!	when tuchar then
	when tschar then
		if d.isshort or d.islong or d.isllong then serror("char decl?") fi
!		t:=(d.issigned|tschar|tuchar)
		t:=(d.isunsigned|tuchar|tschar)
	when tdouble then
		if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("dbl decl?") fi
!long double not supported; just use double
!		t:=tldouble
	when tcomplex then
		if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("Complex?") fi

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
when ktypespecsym then
	return 1
when ktypequalsym then
!	return lx.subcode=const_qual
	return 1
when namesym then
	d:=resolvename((currproc|currproc|stmodule),lx.symptr,ns_general,currblockno)
	if d then
		lx.symptr:=d
		return d^.nameid=typeid
	fi
when kstructsym,kunionsym,kenumsym then
	return 1
endswitch
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
	p:=createunit1(j_exprlist,ulist)
	if ulistx then
		p^.mode:=ulistx^.mode
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
gotp::
	lex()
	oldpmode:=p^.mode
	checklvalue(p)
	q:=readassignexpr()
	if ttisref[p^.mode] then
		return createassignopref(opc,p,q)
	fi

	q:=coercemode(q,oldpmode)
	if ttconst[oldpmode] then
		terror("Modifying read-only var")
	fi

	if q^.tag=j_convert and opc=assignsym then
		q^.convtomem:=1
	fi

	if p^.tag=j_ptr and p^.a^.tag=j_const then
		terror("Modifying constant?")
	fi


	r:=createunit2(symboltojtag[opc],p,q)

	r^.mode:=oldpmode
	return r
endswitch

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

	if u:=dominantmode[s:=ttbasetype[x^.mode],t:=ttbasetype[y^.mode]] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
		if pcond^.tag=j_const and x^.tag=j_const and y^.tag=j_const then
			return (pcond^.value|x|y)
		fi

	elsif s=tref and t=tref then
		u:=x^.mode
	elsif s=tref and t=tsint and y^.tag=j_const and y^.value=0 then
		u:=x^.mode
		coercemode(y,u)
	elsif s=tsint and t=tref and x^.tag=j_const and x^.value=0 then
		u:=y^.mode
		coercemode(x,u)
	elsif s=tstruct and t=tstruct then
		u:=x^.mode
	elsif s=t=tvoid then
		u:=tvoid
	else
CPL Strmode(x^.mode),Strmode(y^.mode)
		terror("?: incompatible types")
	fi

	pcond:=createunit3(j_ifx,pcond,x,y)
	pcond^.mode:=u
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

	if x^.tag=j_const and y^.tag=j_const then
		x^.value := (x^.value or y^.value|1|0)
		next
	fi
	x:=createunit2(j_orl,x,y)
	x^.mode:=tsint
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

	if x^.tag=j_const and y^.tag=j_const then
		x^.value := (x^.value and y^.value|1|0)
		next
	fi
	x:=createunit2(j_andl,x,y)
	x^.mode:=tsint
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

	if u:=dominantmode[ttbasetype[x^.mode],ttbasetype[y^.mode]] then			!were both numeric
		if u>=tfloat then terror("float|float") fi
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("invalid | operands")
	fi

	if x^.tag=j_const and y^.tag=j_const then
		case u
		when tsint,tsllong,tuint,tullong then
			x^.value ior:= y^.value
			next
		esac
	fi
	x:=createunit2(j_ior,x,y)
	x^.mode:=u
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

	if u:=dominantmode[ttbasetype[x^.mode],ttbasetype[y^.mode]] then			!were both numeric
		if u>=tfloat then terror("float^float") fi
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
		terror("invalid ^ operands")
	fi

	if x^.tag=j_const and y^.tag=j_const then
		case u
		when tsint,tsllong then
			x^.value ixor:= y^.value
			next
		esac
	fi
	x:=createunit2(j_ixor,x,y)
	x^.mode:=u
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

	if u:=dominantmode[ttbasetype[x^.mode],ttbasetype[y^.mode]] then			!were both numeric
		if u>=tfloat then terror("float&float") fi
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	else
cpl Strmode(x^.mode)
cpl Strmode(y^.mode)
		terror("invalid & operands")
	fi

	if x^.tag=j_const and y^.tag=j_const then
		case u
		when tsint,tsllong then
			x^.value iand:= y^.value
			next
		esac
	fi
	x:=createunit2(j_iand,x,y)
	x^.mode:=u
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

	if u:=dominantmode[s:=ttbasetype[x^.mode],t:=ttbasetype[y^.mode]] then			!were both numeric
		x:=coercemode(x,u)
		y:=coercemode(y,u)
	elsif s=tref and t=tref then
		if (ss:=tttarget[x^.mode])<>(tt:=tttarget[y^.mode]) then
			if ss<>tvoid and tt<>tvoid then
				if not checkpointertypes(x^.mode,y^.mode,1) then	!'hard'
!				if ttbasetype[ss]=tproc and ttbasetype[tt]=tproc then
!				elsif ttbasetype[ss]=tstruct and ttbasetype[tt]=tstruct then
!				else
					terror("Comparing distinct pointers/eq")
				fi
			fi
		fi
	elsif s=tref and t=tsint then
		if y^.tag<>j_const or y^.value<>0 then
			terror("Can't compare pointer to int")
		fi
	elsif s=tsint and t=tref then
		if x^.tag<>j_const or x^.value<>0 then
			terror("Can't compare pointer to int2")
		fi
	else
		terror("invalid == operands")
	fi

	if x^.tag=j_const and y^.tag=j_const then
		case u
		when tsint,tsllong,tuint,tullong,0 then			!0 when ref/ref ref/int int/ref
			if opc=eqsym then
				x^.value := x^.value = y^.value
			else
				x^.value := x^.value <> y^.value
			fi
			next
		esac
	fi
	x:=createunit2(symboltojtag[opc],x,y)
	x^.mode:=tsint
od


return x
end

function readrelexpr:unit=
unit x,y
int opc,s,t,u
int64 a,b,c
word64 aa,bb,cc

x:=readshiftexpr()

while (opc:=lx.symbol)=ltsym or opc=lesym or opc=gesym or opc=gtsym do
	lex()
	y:=readshiftexpr()

	if u:=dominantmode[s:=ttbasetype[x^.mode],t:=ttbasetype[y^.mode]] then			!were both numeric

		x:=coercemode(x,u)
		y:=coercemode(y,u)
	elsif s=tref and t=tref then
!		if tttarget[x^.mode]<>tttarget[y^.mode] then
		if not checkpointertypes(x^.mode,y^.mode,1) then		!use 'hard' mode
			terror("Comparing distinct pointers/rel")
		fi
	else
		terror("invalid rel operands")
	fi

	if x^.tag=j_const and y^.tag=j_const then
		a:=x^.value; b:=y^.value
		case u
		when tsint,tsllong then
			case opc
			when ltsym then c:=a<b
			when lesym then c:=a<=b
			when gesym then c:=a>=b
			else            c:=a>b
			esac
			x^.value:=c
			next
		when tuint,tullong then
			aa:=x^.value; bb:=y^.value
			case opc
			when ltsym then cc:=aa<bb
			when lesym then cc:=aa<=bb
			when gesym then cc:=aa>=bb
			else            cc:=aa>bb
			esac
			x^.value:=cc
			next
		esac
	fi

	x:=createunit2(symboltojtag[opc],x,y)
	x^.mode:=tsint
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
	unless (u:=ttbasetype[x^.mode])>=tfirstint and u<=tlastint then
		terror("shift:Not an int")
	end unless
	y:=coercemode(y,tsint)
!
	if x^.tag=j_const and y^.tag=j_const then
		case u
		when tsint,tsllong then
			if opc=shlsym then
				x^.value := x^.value << y^.value
			else
				x^.value := x^.value >> y^.value
			fi
			next
		when tuint,tullong then
			if opc=shlsym then
				x^.uvalue := x^.uvalue << y^.value
			else
				x^.uvalue := x^.uvalue >> y^.value
			fi
			next
		esac
	fi
	x:=createunit2((opc=shlsym|j_shl|j_shr),x,y)
	x^.mode:=u
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
int t,u,opc,shift,newlen,slength,tbase,fwide,newmode
ref char pbyte
int64 a
ref strec d
ichar ss,s
ref paramrec pm

switch lx.symbol
when intconstsym, realconstsym then
	p:=createconstunit(lx.value,lx.subcode)
	lex()
when namesym then
	if lx.symptr^.nameid<=macroid then
		d:=resolvename((currproc|currproc|stmodule),lx.symptr,ns_general,currblockno)
		if d=nil then
			serror_s("Undefined name \"%s\"", getstname(lx.symptr))
		fi
	else
		d:=lx.symptr
	fi

	d^.attribs.ax_used:=1
	case d^.nameid
	when enumid then
		p:=createconstunit(d^.index,tsint)
	when constantid then
		p:=createconstunit(d^.code^.value,d^.mode)
	when procid then
		if nextlx.symbol<>lbracksym then
			p:=createunit0(j_funcname)
			p^.def:=d
			p^.mode:=createrefmode(createprocmode(d^.mode,d^.paramlist))
!			p^.mode:=createprocmode(d^.mode,d^.paramlist)
		else
			goto doname
		fi

	else
doname::
		p:=createname(d)
		p^.mode:=t:=d^.mode
		if ttbasetype[t]=tarray then
!			p^.tag:=j_nameaddr
			p^.alength:=ttlength[t]
			p:=createaddrofop(p)
			p^.mode:=createrefmode(tttarget[t])
		elsif d^.nameid<>procid and d^.nameid<>constantid and ttsize[t]<4  then
			fixmemopnd(p)
		elsif d^.nameid=paramid then
			if isstructunion(p^.mode) then
				p^.lineno:=lx.lineno
				p^.mode:=createrefmode(p^.mode)
				p:=createptrop(p)
				p^.mode:=d^.mode
			fi
		fi
	esac
	p^.lineno:=lx.lineno
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
	    p^.wslength:=slength
		p^.mode:=trefwchar
	else
		p:=createstringconstunit(s,slength)
	    p^.slength:=slength
		p^.mode:=trefchar

	fi

	lex()

when kstrincludesym then
!when strincludedir then
	p:=readstrinclude()

when charconstsym then
	a:=0
	shift:=0
	pbyte:=lx.svalue
	to lx.length do
		a:=a ior word64(pbyte^)<<shift
		shift+:=8
		++pbyte
	od
	p:=createconstunit(a,tsint)
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
	p:=createunit1(j_notl,p)
	p^.mode:=tsint

	if p^.a^.tag=j_notl and p^.a^.a^.tag=j_notl then
		p^.a:=p^.a^.a^.a
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

when sqrtsym then
	lex()
	skipsymbol(lbracksym)
	p:=createsqrtop(readexpression())
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
	lex()
	if lx.symbol=lbracksym then		!possible type
		lex()
		if istypestarter() then
			t:=readcasttype(d,0,pm)
			skipsymbol(rbracksym)
			p:=createconstunit(ttsize[t],tullong)
		else
			p:=readexpression()
			skipsymbol(rbracksym)
			p:=createsizeofop(p)
		fi
	else
		p:=createsizeofop(readterm())
	fi
when klengthofsym then
	lex()
	if lx.symbol=lbracksym then		!possible type
		lex()
		if istypestarter() then
			t:=readcasttype(d,0,pm)
			skipsymbol(rbracksym)
			p:=createconstunit(ttlength[t],tsint)
		else
			p:=readexpression()
			skipsymbol(rbracksym)
			p:=createlengthofop(p)
		fi
	else
		p:=createlengthofop(readterm())
	fi
when kgenericsym then
	p:=readgeneric()
when kalignofsym then
	serror("rt/alignof")
when kstrtypesym then
	lex()
	skipsymbol(lbracksym)
	t:=readcasttype(d,0,pm)
	skipsymbol(rbracksym)
	p:=createstringconstunit(pcm_copyheapstring(Strmode(t)),-1)
when kcputimesym then
	p:=createunit0(j_cputime)
	p^.mode:=tsllong
	lex()

else
PS("RT")
	serror("Readterm?")
endswitch

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
	p:=createincrop(j_postincr,p)

when decrsym then
	lex()
	p:=createincrop(j_postdecr,p)
else
	exit
enddoswitch

return p
end

function readexprlist(unit p)unit=
! read comma-separated list, and return head of list (not as j_makelist etc)
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
	if e^.nameid<>staticid then
		serror_ss("var: name in use %s %s",e^.name,namenames[e^.nameid])
	fi
	emode:=e^.mode
	if emode<>m then
		if not comparemode(emode,m) then
redef::
			serror_s("var: redefining %s",e^.name)
		fi
		case ttbasetype[emode]
		when tarray then
			if ttlength[emode]=0 then			!replace empty array
				e^.mode:=m
			elsif ttlength[m] and ttlength[emode]<>ttlength[m] then
				goto redef
			fi
		esac

	fi
	d:=e

!see how scope interacts with existing decl
	scope:=d^.scope
	if scope=local_scope and linkage=none_ss or\
	   scope=exported_scope and linkage=static_ss or\
	   scope=imported_scope and linkage=static_ss then

!*!		serror("Linkage mismatch")

	elsif scope=imported_scope and linkage=none_ss then
		scope:=exported_scope
	fi

else
	d:=createdupldef(stmodule,d,(constantseen|constantid|staticid))
	d^.mode:=m
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
	if d^.code then
		serror_s("Can't init twice %s",d^.name)
	fi
	if scope=imported_scope then
		serror_s("Can't init extern %s",d^.name)
	fi
	lex()
	d^.code:=readinitexpr(stmodule,d^.mode)

	if d^.nameid=constantid then
		unless tfirstint<=ttbasetype[d^.mode]<=tlastreal then
			serror("constant only for int/float")
		end unless
		if d^.code^.tag<>j_const then
			serror("constant expr must be constant")
		fi
	fi

elsif constantseen then
	serror("constant must be initialised")
fi

d^.scope:=scope

return d
end

function readframevar(ref strec d,int m, linkage)ref strec=
ref paramrec pm
ref strec e
int scope,id

e:=checkdupl_inproc(currproc, d, ns_general, currblockno)

if e then					!already exists
		serror_s("var: name in use %s",e^.name)
	d:=e

!see how scope interacts with existing decl
	scope:=d^.scope
	if scope=local_scope and linkage=none_ss or\
	   scope=exported_scope and linkage=static_ss or\
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
	d^.mode:=m
	d^.blockno:=currblockno
	blockcounts[currblockno]:=1
fi

if lx.symbol=assignsym then
	if d^.code then
		serror_s("Can't init twice %s",d^.name)
	fi
	if scope=imported_scope then
		serror_s("Can't init extern %s",d^.name)
	fi
	lex()
	d^.code:=readinitexpr(currproc,d^.mode)
fi

d^.scope:=scope

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
		if i=1 then				!indicate to caller that this is a normal function
			pm:=modvalue[1]
		else					!assume fu nction pointer of some sort
			m:=createprocmode(m,modvalue[i])
		fi
	esac
od

return m
end

proc readnamedtype(ref strec owner, &d,
			[]int &modtype, []ref void &modvalue, int &nmodifiers)=
int length
[maxtypemods]int fconst
int nrefs
unit pdim

d:=nil
nrefs:=0

if lx.symbol=kfnspecsym then
	lex()				!ignore $callback etc (not needed in a type decl, only a function def)
fi

!accumulate pointers
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
when lbracksym then			!don't know how this would work...
	lex()
	readnamedtype(owner,d,modtype,modvalue,nmodifiers)
	skipsymbol(rbracksym)
esac

docase lx.symbol
when lsqsym then			!array
	lex()
	if lx.symbol=rsqsym then
		length:=0
	else
		pdim:=readassignexpr()

		if pdim^.tag=j_const then
			length:=pdim^.value
		else
			serror("Can't do VLAs")
		fi
		checksymbol(rsqsym)
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
enddocase

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
case p^.tag
when j_const then
	return p^.value

else
	serror_s("readconstint %s",jtagnames[p^.tag])
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
		if ttbasetype[melem]=tuchar and lx.symbol=stringconstsym then
			braces:=1
			goto doarraystring
		fi

	when tstruct,tunion then
		d:=ttnamedef[m]
		e:=d^.deflist
		if e=nil then
			terror("init/Empty struct")
		fi
		melem:=e^.mode
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

			if ttbasetype[melem]=tarray and ttbasetype[tttarget[melem]]=tuchar and p^.mode=trefchar then
			else
				p:=coercemode(p,melem)
			fi
		when tstruct then

			mm:=e^.mode

			if ttbasetype[mm]=tarray and ttbasetype[tttarget[mm]]=tuchar and p^.mode=trefchar then
			else
				p:=coercemode(p,mm)
			fi

			e:=e^.nextdef
			if e=nil then
				if lx.symbol=commasym and nextlx.symbol<>rcurlysym then
					terror("Too many struct elems")
				fi
			else
				melem:=e^.mode
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

donestruct::
	skipsymbol(rcurlysym)
	p:=createunit1(j_makelist,ulist)
	p^.count:=count

	p^.mode:=m

else
	braces:=0
	case mbase
	when tarray then
doarraystring::
		if lx.symbol<>stringconstsym and lx.symbol<>wstringconstsym and tttarget[m]<>tuchar then
			terror("{} initialiser expected")
		fi

		p:=readassignexpr()
		if p^.tag=j_const then p^.strarray:=1 fi
		case p^.mode
		when trefchar then
		when trefwchar then
		else
			terror("Array init")
		esac
		P^.MODE:=M

		if (dim:=ttlength[m])=0 then
			ttlength[m]:=ttsize[m]:=p^.slength+1
		else
			if p^.slength>dim then
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

	if p=nil then next fi				!might have been typedef etc
	if p^.tag=j_tempdecl then
		repeat
			q:=p^.nextunit
			if p^.def^.code and p^.def^.nameid<>staticid then
				p^.tag:=j_decl
				p^.nextunit:=nil
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
return createunit3(j_block,ulist,nil,ulistx)
end

function readblock(int ifelse=0)unit=
++NBLOCKS
!PS("READBLOCK")
if lx.symbol=lcurlysym then
++NCOMPOUNDBLOCKS
elsif lx.symbol=kifsym and ifelse then
++NCOMPOUNDBLOCKS
FI

	if not needcompoundblock then
		return readstatement()
	fi
!CPL =IFELSE
	if lx.symbol=kifsym and ifelse then
!CPL "HERE"
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
			p:=createunit0(j_break)
			lex()
!			if lx.symbol=namesym then
!				d:=resolvename(currproc,lx.symptr,ns_labels,0)
!				if d=nil then serror("block label not found") fi
!				d.attribs.ax_loop:=1
!				p.tag:=j_goto
!				p^.def:=d
!			fi

		else
			p:=createunit0(j_breaksw)
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

	p:=createunit0(j_continue)
	lex()

when kcasesym then
	return readcaselabel()

when kdefaultsym then
	lex()
	skipsymbol(colonsym)
	return createunit1(j_defaultstmt,readstatement())

when kshowmodesym then
	lex()
	p:=readexpression()
	ss:=strexpr(p)
	print "Mode is:",ss^.strptr,":",Strmode(p^.mode),"	on line",lx.lineno,
		"Size is",ttsize[p^.mode]
	if ttisref[p^.mode] then
		print " target size",ttsize[tttarget[p^.mode]]
	fi
	println

when kmccassertsym then
	dostaticassert()

when semisym then
	lex()	
	return nil

when namesym then
	if nextlx.symbol=colonsym then
		p:=createunit1(j_labelstmt,nil)
		d:=resolvename(currproc,lx.symptr,ns_labels,0)
		if d then
			if d^.index then
				cpl lx.symptr^.name
				terror("Duplicate label")
			else
				d^.index:=++labelno
			fi
		else
			d:=createdupldef(currproc,lx.symptr,labelid)
			d^.mode:=tvoid
			d^.index:=++labelno
		fi

		p^.def:=d
		lex()				!skip colon
		lex()
!		if lx.symbol=rcurlysym then serror("label before }") fi
		if lx.symbol=rcurlysym then
		elsif istypestarter() or lx.symbol=klinkagesym then
		else
			p^.a:=readstatement()
		fi
		return p
	else
		ist_symptr:=nil
		if isusertype(currproc) then
			goto doreaddecl
		fi
		if ist_symptr then lx.symptr:=ist_symptr fi		!make use of name resolve done by isusertype
		p:=readexpression()
	fi
when ktypespecsym, ktypequalsym, klinkagesym, kfnspecsym,
	kstructsym,kunionsym,kenumsym,ktypeofsym then
doreaddecl::
	return readlocaldecl()

else						!assume expression
	p:=readexpression()
!	TESTEXPR(p)
endswitch

skipsymbol(semisym)

return p
end

function readifstmt:unit=
unit pcond,pbody,pelse

lex()
pcond:=readcond()
coercecond(pcond)

pbody:=readblock()

!if pbody^.nextunit=nil and pbody^.tag=j_labelstmt then
!	serror("conditional label1")
!fi

pelse:=nil

if lx.symbol=kelsesym then
	lex()
	pelse:=readblock(1)
!	if pelse^.nextunit=nil and pelse^.tag=j_labelstmt then
!		serror("conditional label2")
!	fi
fi

return createunit3(j_if,pcond,pbody,pelse)
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
INSIDEFOR:=1
			d:=readframevar(d,m,linkage)
INSIDEFOR:=0

			if d^.code then
				p:=createunit0(j_decl)
				p^.def:=d
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
		enddocase
		pinit:=createunit3(j_block,ulist,nil,ulistx)

	else
		pinit:=readexpression()
	fi
else
	pinit:=createunit0(j_null)
fi
skipsymbol(semisym)

if lx.symbol<>semisym then
	pcond:=readexpression()
	coercecond(pcond)
else
	pcond:=createunit0(j_null)
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

pinit^.nextunit:=pcond			!the 3 for elements are linked together
pcond^.nextunit:=pincr

return createunit2(j_for, pinit, pbody)
end

function readwhilestmt:unit=
unit pcond,pbody

lex()
pcond:=readcond()
coercecond(pcond)
pushloop('L')
pbody:=readblock()
poploop()

return createunit2(j_while,pcond,pbody)
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
	return createunit2(j_dowhile,pbody,pcond)
end

function readreturnstmt:unit=
	unit p
	lex()
	p:=nil

	if lx.symbol<>semisym then
		if currproc^.mode=tvoid then
			terror("Can't return value in void function")
		fi

		p:=readexpression()
		p:=coercemode(p,currproc^.mode)
		checksymbol(semisym)
	elsif currproc^.mode<>tvoid then
		terror("Return value needed")
	fi
	lex()

	return createunit1(j_return,p)
end

function readgotostmt:unit=
	ref strec d
	unit p

	lex()
	checksymbol(namesym)
	d:=resolvename(currproc,lx.symptr,ns_labels,0)
	if d=nil then					!assume fwd ref
		d:=createdupldef(currproc,lx.symptr,labelid)
		d^.mode:=tvoid
!		d^.index:=++labelno
	fi
	p:=createunit1(j_goto,nil)
	p^.def:=d
	lex()				!skip colon
	skipsymbol(semisym)
	return p
end

function readswitchstmt:unit=
	unit pindex,pstmt,p

	lex()
	pindex:=readcond()			!not a condition, but it doesn't matter
	pushloop('S')
	pstmt:=readblock()			!not a condition, but it doesn't matter
	p:=createunit2(j_switch, pindex, pstmt)
	p^.nextcase:=casevaluestack[loopindex]

	poploop()
	return p
end

function readcaselabel:unit=
unit p,q
int value

lex()					!skip case/default
value:=readconstintexpr()
skipsymbol(colonsym)

p:=createunit1(j_casestmt,readstatement())
p^.index:=value
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
!return typeno if it resolves to a user type, otherwise 0
!will peek at following symbol, and returns 0 if "," or ";" follows
ref strec d

d:=resolvename(owner,lx.symptr,ns_general,currblockno)
if d then
	if d^.nameid=typeid then
		return d^.mode
	fi
	ist_symptr:=d
fi
return 0
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
!PS("RLD")
		if lx.symbol=lcurlysym then
			serror("Nested function")
		fi
		d:=readfunction(d,m,linkage,pm,wasdef)
	else
		d:=readframevar(d,m,linkage)
		p:=createunit0(j_tempdecl)
		p^.def:=d
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
when tsint then
	skipsymbol(semisym)
	exit

	else
		serror_s("Local decl error %s",typename(m))
	esac
enddocase

if nitems=0 and fmodern then
	case ttbasetype[mbase]
	when tstruct,tunion,tenum then
	else
		if wasenum<>kenumsym then
			serror("Empty local declaration")
		fi
	esac
fi

return ulist
end

function createtypedef(ref strec owner, symptr, int mode)ref strec=
!symptr is a generic symbol for the name
ref strec d

d:=checkdupl(owner,symptr,ns_general,currblockno)

if d then			!existing name
	if d^.nameid<>typeid then
		serror_s("Typedef name in use %s",d^.name)
	fi

	if d^.mode<>mode then
		if not comparemode(d^.mode, mode) then
			serror_s("Typedef redefined or can't match types %s",d^.name)
		fi
	fi
	return d
fi

d:=createdupldef(owner,symptr,typeid)

d^.mode:=mode
tttypedef[mode]:=d
d^.blockno:=currblockno
blockcounts[currblockno]:=1

return d
end

function readparams(ref strec owner)ref paramrec=
ref paramrec ulist,ulistx, pm, q
int m,nparams,variadic,flags,nnames
ref strec d

ulist:=ulistx:=nil
variadic:=nparams:=nnames:=0

if callbackflag then			!lex flag is out of step with parser
	iscallbackfnx:=1
	callbackflag:=0
fi

while lx.symbol<>rbracksym do
	if lx.symbol=ellipsissym then
		variadic:=1
		lex()
		exit
	fi

	m:=readcasttype(d,1,pm)
	if pm then			!was a fu nction; convert to fu nction pointer
		m:=createrefmode(createprocmode(m,pm))
	fi
	if ttbasetype[m]=tarray then
		m:=createrefmode(tttarget[m])
	fi

	pm:=pcm_allocz(paramrec.bytes)
	pm^.def:=d
	pm^.mode:=m
	++nparams
	if d then
		++nnames
		q:=ulist
		while q do
			if q^.def=d then
				serror_ss("Param name reused %s %s",d^.name,namenames[d^.nameid])
			fi
			q:=q^.nextparam
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
	if fmodern then
		terror("() Params not allowed")
	else
		flags:=pm_notset
	fi
elsif nparams=1 and m=tvoid then
	flags:=pm_empty
	nparams:=0
	ulist^.mode:=tnone
fi

if ulist=nil then
	ulist:=pcm_allocz(paramrec.bytes)
fi
ulist^.nparams:=nparams
ulist^.flags:=flags

return ulist
end

function readcasttype(ref strec &d, int allowname=0,ref paramrec &pm)int=
!at first symbol of a type-spec
!ref paramrec pm
ref strec owner
int m,linkage

owner:=(currproc|currproc|stmodule)

linkage:=0
d:=nil
m:=readdeclspec(owner,linkage)
pm:=nil

case lx.symbol
when namesym, mulsym, lbracksym, lsqsym then
	m:=readtype(owner,d, m, pm)
	if d and not allowname then
		serror_s("NAME not allowed in cast type %s",d^.name)
	fi
esac

return m
end

function readfunction(ref strec d, int m, linkage, ref paramrec pm, int &wasdef)ref strec=
!have read function declaration, with ";" or "{" next
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
	if f^.nameid<>procid then
		serror_s("fn: name in use %s",d^.name)
	fi
!COMPARE PARAM LISTS...
!	if e^.paramlist<>pm then
!		serror("fn: params don't match previous")
!	fi
	d:=f

!see how scope interacts with existing decl
	scope:=d^.scope
	if scope=local_scope and linkage=none_ss or\
	   scope=exported_scope and linkage=static_ss or\
	   scope=imported_scope and linkage=static_ss then
!*!		serror("Linkage3 mismatch")
	elsif scope=imported_scope and linkage=none_ss then
		scope:=exported_scope
	fi


else
	d:=createdupldef(owner,d,procid)
	d^.mode:=m
	case linkage
	when static_ss then
		scope:=local_scope
	when extern_ss then
		scope:=imported_scope
	else
		scope:=exported_scope
	esac
fi

if iscallbackfnx or fcallback then
!CPL "CALLBACK SEEN",D^.NAME
	d^.attribs.ax_callback:=1
	iscallbackfnx:=0
fi

d^.paramlist:=pm
d^.scope:=scope

if lx.symbol=lcurlysym then
	wasdef:=1
	if d^.code then
		serror_s("Can't define function twice %s",d^.name)
	fi
	if scope=imported_scope then
!		serror("Can't define imported function")
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
pm:=f^.paramlist
if pm^.def then			!params are named
	to pm^.nparams do
		e:=createdupldef(f,pm^.def,paramid)
		e^.blockno:=1
		e^.mode:=pm^.mode
		pm:=pm^.nextparam
		pmcount:=1
	od
elsif pm^.nparams then
	serror("Param names missing")
fi

p:=readcompoundstmt(pmcount)
currproc^.code:=p
currproc:=nil
end

function createnegop(unit p)unit=
unit q
int t

t:=p^.mode

if p^.tag=j_const then
	case t
	when tsint,tsllong,tullong then
		p^.value:=-p^.value
		return p
	when tuint then
		p^.value:=(-p^.value) iand 0xFFFF'FFFF
		return p
	when tdouble then
		p^.xvalue:=-p^.xvalue
		return p
	esac
fi
retry::
if t>=tfirstnum and t<=tlastnum then
	coercebasetype(p)
	q:=createunit1(j_neg,p)
elsif ttconst[t] then
	t:=ttconsttype[t]
	goto retry
else
CPL Strmode(t)
	terror("neg bad type")
fi

q^.mode:=p^.mode
return q
end

function createabsop(unit p)unit=
unit q
int t

t:=p^.mode

if p^.tag=j_const then
	case t
	when tsint,tsllong then
		p^.value:=abs(p^.value)
		return p
	esac
fi

!if t>=tfirstint and t<=tlastint then
if isintcc(t) then
	coercebasetype(p)
	q:=createunit1(j_abs,p)
else
	terror("abs bad type")
fi

q^.mode:=p^.mode
return q
end

function createsqrtop(unit p)unit=
unit q
int t

t:=p^.mode

if p^.tag=j_const then
	case t
	when tfloat,tdouble then
		p^.value:=sqrt(p^.xvalue)
		return p
	esac
fi

coercemode(p,tdouble)

q:=createunit1(j_sqrt,p)
q^.mode:=tdouble

return q
end

function createinotop(unit p)unit=
unit q
int t

t:=ttbasetype[p^.mode]

if p^.tag=j_const then
	case t
	when tsint,tsllong,tuint,tullong then
		p^.value:=inot p^.value
		return p
	esac
fi
if isintcc(t) then
	coercebasetype(p)
	q:=createunit1(j_inot,p)
else
cpl Strmode(t)
	terror("! bad type")
fi

q^.mode:=p^.mode
return q
end

function createptrop(unit p)unit=
unit q
int t,m

if not ttisref[t:=p^.mode] then
	terror("* not pointer")
fi
m:=tttarget[t]

case p^.tag
when j_addrof then
	q:=p^.a
	if p^.alength then
		q^.mode:=tttarget[p^.mode]
	fi
	return q
esac

q:=createunit1(j_ptr,p)
q^.mode:=m
q:=arraytopointer(q)
fixmemopnd(q)

return q
end

function createincrop(int opc,unit p)unit=
!opc is j_preincr/decr or j_postincr/decr
unit q
int t

t:=p^.mode

checklvalue(p)
!unless t>=tfirstint and t<=tlastint and t<>tbool or ttisref[t] then
!if isreal(t) and opc in [j_preincr, j_predecr] then
!	q:=createunit2((opc=j_preincr|j_addto|j_subto),p,createconstunit(word64@(1.0),tdouble))
!	q^.mode:=p^.mode
!	return q
!fi

unless isintcc(t) and t<>tbool or ttisref[t] then
	terror("++ bad type")
endunless
q:=createunit1(opc,p)
q^.mode:=p^.mode

return q
end

function createlengthofop(unit p)unit=
unit q
int t,size

t:=p^.mode
switch p^.tag
when j_name then
	size:=ttlength[p^.def^.mode]			!take account of array

when j_const then
	if t=trefchar then					!const string
		size:=p^.slength+1
	else
		size:=ttlength[t]
	fi

when j_ptr then
	if ttisref[t] and p^.alength then		!result of array=>ptr conversion
		size:=ttlength[tttarget[t]]*p^.alength
	else
		size:=ttlength[t]
	fi
when j_widenmem then
	return createsizeofop(p^.a)

else
	size:=ttlength[t]
endswitch

q:=createconstunit(size,tsint)
return q
end

function createaddrofop(unit p)unit=
ref strec d
unit q
int t,u,alength

alength:=0

restartx::
t:=p^.mode
switch p^.tag
when j_name then
	if p^.alength then
		t:=p^.def^.mode
		alength:=p^.alength
	fi

when j_addrof then
	if p^.a^.tag=j_name and p^.a^.alength then		!sounds like ANAME => &ANAME
		p^.mode:=createrefmode(p^.a^.def^.mode)
p^.alength:=p^.a^.alength
		return p
	fi
!doname:
!	d:=p^.def
!	if d^.nameid=constantid then serror("&constant not allowed") fi
!	m:=createrefmode(d^.mode)
!
!when j_funcname then
!	m:=createprocmode(p^.def^.mode,p^.def^.paramlist)
!
!when j_ptr then				!should cancel out
!	return p^.a
!
!when j_ptroffset then
!	p^.tag:=j_addptr
!	p^.mode:=createrefmode(p^.mode)
!	return p
!
!when j_addptr then
!	if p^.alength then	!derived from array expr that converted to pointer
!		p^.mode:=createrefmode(createarraymode(tttarget[t],p^.alength))
!		return p
!	else
!		goto cad1
!	fi
!when j_convert then
!	if p^.a^.tag=j_name then			!assume added by readterm
!		p:=p^.a
!		goto doname
!	fi
!	goto cad1
when j_dot then
	q:=p^.a
	if q^.tag=j_ptr and q^.a^.tag=j_const then
		p:=createconstunit(p^.offset+q^.a^.value, tsint)
		return p
	fi
	goto cad1
when j_addptr then
	if p^.alength then
		p^.mode:=createrefmode(createarraymode(tttarget[p^.mode],p^.alength))
		return p
	fi
when j_widenmem then
	p:=p^.a
	goto restartx
when j_funcname then
	return p
!when j_const then
!	if t=trefchar then					!const string
!		t:=createarraymode(tuchar,p^.slength+1)
!	else
!		goto cad1
!	fi
else

cad1::
	checklvalue(p)
endswitch

p:=createunit1(j_addrof,p)
p^.mode:=createrefmode(t)
p^.alength:=alength

!CPL "ADDROF",STRMODE(P^.MODE),STRMODE2(T)

return p
end

function createaddop(unit x,y)unit=
unit z
int s,t,u,opc,elemsize

!s:=ttbasetype[x^.mode]
!t:=ttbasetype[y^.mode]
s:=ttbasetype[getmemmode(x)]
t:=ttbasetype[getmemmode(y)]
opc:=j_add

if u:=dominantmode[s,t] then			!were both numeric
	x:=coercemode(x,u)
	y:=coercemode(y,u)

elsif s=tref then
doaddref::
	u:=x^.mode
	elemsize:=ttsize[tttarget[u]]
	if x^.tag=j_const and y^.tag=j_const then
		x^.value +:=y^.value*elemsize
		return x
	fi

	y:=coercemode(y,tptroffset)

	z:=createunit2(j_addptr,x,y)
	z^.mode:=u
	z^.ptrscale:=elemsize
	return z

elsif t=tref then
	swap(x,y)
	goto doaddref
else
	terror("Sub bad types")
fi

if x^.tag=j_const and y^.tag=j_const then
	return eval_add(opc,x,y,u)
fi
z:=createunit2(opc,x,y)
z^.mode:=u

return z
end

function createsubop(unit x,y)unit=
unit z
int s,t,u,opc,elemsize

!s:=ttbasetype[x^.mode]
!t:=ttbasetype[y^.mode]
s:=ttbasetype[getmemmode(x)]
t:=ttbasetype[getmemmode(y)]
opc:=j_sub

if u:=dominantmode[s,t] then			!were both numeric
	x:=coercemode(x,u)
	y:=coercemode(y,u)
elsif s=tref then
	if t<>tref then
		u:=x^.mode
		elemsize:=ttsize[tttarget[u]]
		y:=coercemode(y,tptroffset)

		z:=createunit2(j_subptr,x,y)
		z^.mode:=u
		z^.ptrscale:=elemsize
		return z

	else							!ref-ref
		z:=createunit2(opc,x,y)
		z^.mode:=tptroffset
		z:=divunit(z,tttarget[x^.mode])
		z^.mode:=tptroffset
		return z
	fi
	y:=mulunit(y,tttarget[x^.mode])
else
	terror("Sub bad types")
fi

if x^.tag=j_const and y^.tag=j_const then
	return eval_sub(opc,x,y,u)
fi
z:=createunit2(opc,x,y)
z^.mode:=u

return z
end

function createmulop(unit x,y)unit=
unit z
int s,t,u,opc

!s:=ttbasetype[x^.mode]
!t:=ttbasetype[y^.mode]
s:=ttbasetype[getmemmode(x)]
t:=ttbasetype[getmemmode(y)]

opc:=j_mul
if u:=dominantmode[s,t] then			!were both numeric
	x:=coercemode(x,u)
	y:=coercemode(y,u)
else
	terror("Mul bad types")
fi

if x^.tag=j_const and y^.tag=j_const then
	return eval_mul(opc,x,y,u)
fi
z:=createunit2(opc,x,y)
z^.mode:=u

return z
end

function createdivop(unit x,y)unit=
unit z
int s,t,u,opc

s:=ttbasetype[getmemmode(x)]
t:=ttbasetype[getmemmode(y)]

opc:=j_div
if u:=dominantmode[s,t] then			!were both numeric
	x:=coercemode(x,u)
	y:=coercemode(y,u)
else
	terror("Div bad types")
fi

if x^.tag=j_const and y^.tag=j_const then
	return eval_div(opc,x,y,u)
fi
z:=createunit2(opc,x,y)
z^.mode:=u

return z
end

function createremop(unit x,y)unit=
unit z
int s,t,u,opc

s:=ttbasetype[x^.mode]
t:=ttbasetype[y^.mode]

opc:=j_rem
if u:=dominantmode[s,t] then			!were both numeric
	if u=tdouble or u=tfloat then
!		u:=tsllong
		u:=tsint
	fi
	x:=coercemode(x,u)
	y:=coercemode(y,u)
else
	terror("Rem bad types")
fi

if x^.tag=j_const and y^.tag=j_const then
	return eval_rem(opc,x,y,u)
fi
z:=createunit2(opc,x,y)
z^.mode:=u

return z
end

proc insertunit(unit p, int tag)=
!wrap extra unit around p, using given tag
unit q
q:=createunit0(0)			!empty unit
q^:=p^
p^.tag:=tag
p^.a:=q
p^.b:=p^.c:=nil
p^.lineno:=q^.lineno
p^.simple:=0
p^.nextunit:=q^.nextunit

q^.nextunit:=nil
end

function eval_add(int opc,unit x,y,int t)unit=
unit z

case t
when tsint,tsllong,tuint,tullong then
	x^.value +:= y^.value
	return x
when tdouble then
	x^.xvalue +:= y^.xvalue
	return x
elsif ttbasetype[t]=tref then	!assume y is const 0 int of any sub-type
	x^.value +:= y^.value*ttsize[tttarget[t]]
	return x			!will not change x
esac

z:=createunit2(opc,x,y)
z^.mode:=t
return z
end

function eval_sub(int opc,unit x,y,int t)unit=
unit z

case t
when tsint,tsllong,tuint,tullong then
	x^.value -:= y^.value
	return x
when tdouble then
	x^.xvalue -:= y^.xvalue
	return x
elsif ttbasetype[t]=tref then
	if ttbasetype[y^.mode]=tref then
		terror("EVALSUB/REF")
	fi
	return x
esac

z:=createunit2(opc,x,y)
z^.mode:=t
return z
end

function eval_mul(int opc,unit x,y,int t)unit=
unit z

case t
when tsint,tsllong,tsshort,tschar then
	x^.value *:= y^.value
	return x
when tuint,tullong,tushort,tuchar then
!	x^.uvalue *:= y^.uvalue
	x^.uvalue := x^.uvalue*y^.uvalue
	return x
when tdouble then
	x^.xvalue *:= y^.xvalue
	return x
esac

z:=createunit2(opc,x,y)
z^.mode:=t
return z
end

function eval_div(int opc,unit x,y,int t)unit=
unit z

case t
when tsint,tsllong then
	if y^.value=0 then serror("div 0") fi
	x^.value := x^.value/y^.value
	return x
when tuint,tullong then
	if y^.value=0 then serror("div 0") fi
	x^.value := x^.value/y^.value
	return x
when tdouble then
!	if y^.xvalue=0 then serror("div 0.0") fi
	x^.xvalue /:= y^.xvalue
	return x
esac

z:=createunit2(opc,x,y)
z^.mode:=t
return z
end

function eval_rem(int opc,unit x,y,int t)unit=
unit z

case t
when tsint,tsllong then
	if y^.value=0 then serror("rem 0") fi
	x^.value := x^.value rem y^.value
	return x
esac

z:=createunit2(opc,x,y)
z^.mode:=t
return z
end

function eval_convert(unit p, int t,opc)int=
!p contains a const unit, t is a target type, opc is conv op
!try and convert if possible
!return 1 if converted
int s

!CPL "EVALCONVERT",CONVNAMES[OPC],JTAGNAMES[P.TAG]

if opc=soft_c then
dosoft::
	p^.mode:=t
	return 1
fi

s:=p^.mode
if s=t then return 1 fi

case s
when tsint,tsshort,tschar,tsllong then
	case t
	when tdouble,tfloat then
		p^.xvalue:=p^.value
		p^.mode:=t
		return 1
	when tullong,tsllong,tuint,tsint,tsshort,tschar,tuchar,tushort then
dotrunc::
		case ttsize[t]
		when 1 then p^.value iand:=255
		when 2 then p^.value iand:=65535
!		when 4 then p^.value iand:=0xFFFF'FFFF
		when 4 then p^.value :=p^.value iand 0xFFFF'FFFF
		esac

		goto dosoft
	esac
	if ttisref[t] then
		p^.mode:=t
		return 1
	fi

when tuint,tuchar,tushort,tullong then
	case t
	when tdouble,tfloat then
RETURN 1
!CPL "ULLONG TO FLOAT"
!		if p^.value>=0 then
SERROR("ULLONG TO FLOAT")

!			p^.xvalue:=p^.uvalue
!		else
!			p^.xvalue:=-p^.value
!		fi
		p^.mode:=t
		return 1
	when tullong,tsllong,tsint,tuint,tullong,tushort,tschar,tuchar,tsshort then
		goto dotrunc
	esac
	if ttisref[t] then
		p^.mode:=t
		return 1
	fi

when tdouble then
	case t
	when tsint then
		p^.value:=p^.xvalue
		p^.mode:=tsint
		return 1
	when tfloat then
		p^.mode:=tfloat
		return 1
	esac
elsif ttisref[p^.mode] then
	case t
	when tsint,tsllong,tuint,tullong then
		p^.mode:=t
		return 1
	esac
esac

return 0
end

proc coercecond(unit p)=
!p is an expression used as a condition
!make sure it is an int, or convert it to one
int t
if (t:=p^.mode)=tsint then return fi

retry::
case ttbasetype[t]
when tfloat,tdouble,tref then
	goto doint

!elsif t>=tfirstint and t<=tlastint then
elsif isintcc(t) then
doint::
	if p^.tag=j_const and p^.value then			!check all types as one 64-bit field
		p^.value:=1
	else
		insertunit(p,j_istruel)
	fi
elsif ttconst[t] then
	t:=ttconsttype[t]
	goto retry
else

	serror_s("Invalid condition %s",Strmode(t))
esac
p^.mode:=tsint
end

proc coercebasetype(unit p)=
int t

if (t:=p^.mode)>=tschar and t<=tsshort then
	p:=coercemode(p,tsint)
elsif t>=tbool and t<=tushort then
	p:=coercemode(p,tuint)
fi
end

proc checklvalue(unit p)=
case p^.tag
when j_name then
	if p^.def^.nameid=constantid then
		serror("'constant' name can't be lvalue")
	fi
when j_ptr then

when j_funcname then

when j_widenmem then
	case p^.a^.tag
	when j_name,j_ptr,j_dot then
		p^:=p^.a^
	else
		terror("CHECKLV/WIDEN")
	esac

when j_dot then

!when j_ptroffset then

when j_const then
	if not ttisref[p^.mode] then
		goto notlv
	fi
when j_convert then
	if p^.a^.tag=j_name then
!		p^:=p^.a^
		return
	fi

else
notlv::
	printunit(nil,p)
	terror_s("Not lvalue: %s",jtagnames[p^.tag])
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
case p^.tag
when j_ptr then
doptr::
	mproc:=p^.mode

	while ttbasetype[mproc]=tref do
		r:=createunit1(j_ptr,p)
		mproc:=tttarget[mproc]
		r^.mode:=mproc
		p:=r
	od

	if ttbasetype[mproc]<>tproc then
CPL =STRMODE(MPROC), =STRMODE(TTBASETYPE[MPROC]),=STRMODE(TPROC)
		serror_s("Not function pointer: %s",typename(mproc))
	fi

	pm:=ttparams[mproc]
	retmode:=tttarget[mproc]

when j_name,j_funcname then
	d:=p^.def
	if d^.nameid=procid then
		pm:=d^.paramlist
		retmode:=d^.mode
	else							!assume fnptr, but needs ptr unit
		r:=createunit1(j_ptr,p)
		r^.mode:=tttarget[d^.mode]
		p:=r
		goto doptr
	fi
when j_dot,j_callfn,j_ifx then
	r:=createunit1(j_ptr,p)
	r^.mode:=tttarget[p^.mode]
	p:=r
	goto doptr

!when j_callfn then
!	r:=createunit1(j_ptr,p)
!	r^.mode:=tttarget[p^.mode]
!	p:=r
!	goto doptr

else
CPL =JTAGNAMES[P^.TAG]
PRINTUNIT(NIL,P)
	serror("ccall?")
esac

!CPL "CALL2"

nparams:=pm^.nparams
aparams:=0

s:=q
while s do
	++aparams				!number of actual params supplied
	s:=s^.nextunit
od

!checking params is a little tricky because of variadic params
!but there must be at least <nparams> actual params.

if aparams<nparams then
	terror("Too few args")
elsif aparams>nparams and pm^.flags<>pm_variadic and pm^.flags<>pm_notset then
!elsif aparams>nparams and pm^.flags<>pm_variadic then
	if pm^.flags<>pm_notset then
		cpl aparams,nparams
		terror("Too many args")
	elsif fmodern then
		terror("Can't call () param function")
	fi
fi

s:=q
for i:=1 to aparams do
	if i<=nparams then
		coercemode_inplace(s,pm^.mode)
		pm:=pm^.nextparam
	else					!assume variadic param
		if s^.mode=tvoid then
			terror("Variadic param is void")
		fi
		coercebasetype(s)
	fi
	s:=s^.nextunit
od

r:=createunit2(j_callfn,p,q)
r^.mode:=retmode
r^.aparams:=aparams

!CPL "DONE CALL",D,D.NAME
!IF d and q and q.tag=j_const then

!VAR UNIT XXX
!
!IF d and q then
!!CPL "DQ CALL",d.name
!!IF EQSTRING(D.NAME,"printf") THEN CPL "///PRINTF" FI
!!IF EQSTRING(D.NAME,"sprintf") THEN CPL "///SPRINTF",Q.NEXTUNIT,JTAGNAMES[Q.NEXTUNIT.TAG] FI
!
!	XXX:=Q
!	if eqstring(d.name,"printf") and q.tag=j_const or
!	   eqstring(d.name,"sprintf") and (XXX:=q.nextunit) or
!	   eqstring(d.name,"fprintf") and (XXX:=q.nextunit) then
!
!		while xxx.tag=j_convert do xxx:=xxx.a od
!IF XXX.TAG=J_CONST THEN
!!CPL =JTAGNAMES[XXX.TAG]
!		ss:=XXX.svalue
!!CPL =ss
!		while c:=ss++^ do
!			if c='%' then
!			++NFORMATS
!			fi
!		od
!FI
!	fi
!fi
!

if d and eqstring(d.name,"printf") and q and q.tag=j_const and
		q.slength<str.len/2 then
	ss:=q.svalue
	tt:=&.str

	u:=q.nextunit
	while c:=ss++^ do
		if c='%' and ss^ in ['?','='] and u then
			if ss^='=' then
				++ss				!should be '?'
				exprstr:=strexpr(u)
				uu:=exprstr.strptr
				convucstring(uu)
				to exprstr.length do
					tt++^:=uu++^
				od
				tt++^:='='
			fi
			++ss

			tt++^:='%'
!CPL =STRMODE(U.MODE)
			case ttbasetype[u.mode]
			when tsint then
				tt++^:='d'
			when tsllong then
				tt++^:='l'
				tt++^:='l'
				tt++^:='d'
			when tuint then
				tt++^:='u'
			when tullong then
				tt++^:='l'
				tt++^:='l'
				tt++^:='u'
			when tfloat, tdouble,tldouble then
				tt++^:='f'
			when tref then
				if tttarget[u.mode]=tschar then
					tt++^:='s'
				else
					tt++^:='p'
				fi
			else
				tt++^:='?'
			esac
			u:=u.nextunit
		else
			tt++^:=c
		fi
	od
	tt^:=0
	q.svalue:=pcm_copyheapstring(&.str)
	q.slength:=strlen(&.str)

!CPL =&.STR
fi

return r
end

function arraytopointer(unit p)unit=
unit q
int offset
int t,elemmode,refmode

t:=p^.mode
elemmode:=tttarget[t]

if ttbasetype[t]=tarray then
	refmode:=createrefmode(elemmode)
	case p^.tag
	when j_ptr then
		p:=p^.a

	when j_dot then						!about to access array field
		offset:=p^.offset
		p^.tag:=j_addptr
		p^.ptrscale:=0	!ttsize[elemmode]
!		p^.a^.mode:=refmode
		q:=createunit1(j_addrof,p^.a)
		q^.mode:=refmode
		p^.a:=q
		p^.b:=createconstunit(offset,tsint)

	else
		CPL "ATP:"
		printunit(nil,p)
		terror("ATP?")
	esac

	p^.mode:=refmode
	p^.alength:=ttlength[t]

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
			if e^.nameid<>structtagid then
				serror_s("Struct tag in use %s",e^.name)
			fi

			return e^.mode
		fi
!create new incomplete tag
		e:=createdupldef(tagowner,d,structtagid)
		e^.mode:=createstructmode(e,(funion|tunion|tstruct))
		e^.blockno:=currblockno
		blockcounts[currblockno]:=1
		return e^.mode
	fi
fi

!{ seen, so defining a new struct

e:=checkdupl(tagowner,d,ns_tags,currblockno)

if e then			!found in this linkage
	if e^.nameid<>structtagid then
		serror_s("Struct tag in use %s",e^.name)
	fi
	if e^.deflist then					!else filling in incomplete enum
		cpl "Prev",e^.lineno iand 1677215, sourcefilenames[e^.lineno>>24],sourcefilepaths[e^.lineno>>24]
		serror_s("Redefining struct %s",e^.name)
	fi
else						
	e:=createdupldef(tagowner,d,structtagid)
	e^.mode:=createstructmode(e,(funion|tunion|tstruct))
	e^.blockno:=currblockno
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
			serror_s("member name in use %s",e^.name)
		fi

		if linkage<>none_ss then
			serror("Can't use ss in struct")
		fi

addanonfield::
		d:=createdupldef(nil,d,fieldid)
		d^.mode:=m
!name is not linked in to record as they must be in sequence
		addlistdef(&ulist,&ulistx,d)
		currrecord^.deflist:=ulist				!needed for dupl checking
		currrecord^.deflistx:=ulistx
		d^.owner:=currrecord
		alignment:=getalignment(m)
		if alignment>maxalignment then maxalignment:=alignment fi

		d^.offset:=roundoffset(offset,alignment)
		size:=ttsize[m]
		recsize+:=d^.offset-offset
		offset:=d^.offset

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
				serror_s("Struct decl error %s",typename(m))
			fi
		esac
	enddocase
od

skipsymbol(rcurlysym)

currrecord^.nextfield:=fieldlist
ttsize[currrecord^.mode]:=roundoffset((funion|maxsize|recsize),maxalignment)
currrecord^.attribs.ax_align:=maxalignment

return currrecord^.mode
end

function checkpointertypes(int s,t,hard)int=
!return 1 if pointer types s and t are compatible
!it is assumed that s is to be converted to t, or passed as a parameter expecting t
int starget:=tttarget[s], ttarget:=tttarget[t]
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
	cpl Strmode(s)
	cpl Strmode(t)
	terror("const to non-const pointer")
fi

if starget=ttarget then return 1 fi
s:=starget
t:=ttarget
if ttbasetype[s]=tvoid or ttbasetype[t]=tvoid then
	return 1
fi

if ttisref[s] and ttisref[t] then

	return checkpointertypes(s,t,hard)
elsif ttbasetype[s]=tarray and ttbasetype[t]=tarray then
	if ttlength[s]<>ttlength[t] then
		if ttlength[s] and ttlength[t] then		!allow one dim to be 0
			return 0
		fi
	fi
	starget:=tttarget[s]
	ttarget:=tttarget[t]
	if starget=ttarget then return 1 fi

	if ttisref[starget] and ttisref[ttarget] then
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
		if e^.nameid<>enumtagid then
			serror_s("Enum tag in use %s",e^.name)
		fi
	fi

!create new incomplete enum tag
	e:=createdupldef(owner,d,enumtagid)
	e^.mode:=createenummode(e)
	e^.blockno:=currblockno
	blockcounts[currblockno]:=1
	return e^.mode
fi

!{ seen, so defining a new enum
e:=checkdupl(owner,d,ns_tags,currblockno)

if e then			!found in this linkage
	if e^.nameid<>enumtagid then
		serror_s("Enum tag in use %s",e^.name)
	fi
	if e^.deflist then					!else filling in incomplete enum
		serror_s("Redefining enum %s",e^.name)
	fi
else						
	e:=createdupldef(owner,d,enumtagid)
	e^.mode:=createenummode(e)
	e^.blockno:=currblockno
	blockcounts[currblockno]:=1
fi

!e points to an enum def which has an empty {...} list
!Now loop reading enum values

readenumnames(owner)

ttnamedef[e^.mode]:=e
return e^.mode
end

proc readenumnames(ref strec owner)=
!at '{'; read set of enum names
ref strec d,e
ref strec ulist,ulistx
int enumseq

ulist:=ulistx:=nil
enumseq:=0
lex()

case owner^.nameid
when procid,moduleid then		!fine
else							!probably inside a struct
	owner:=(currproc|currproc|stmodule)
esac

while lx.symbol=namesym do
	d:=checkdupl(owner,lx.symptr,ns_general,currblockno)
	if d then
		serror_s("enum name reused %s",d^.name)
	fi
	d:=createdupldef(owner,lx.symptr,enumid)
!CPL "CREATED ENUM NAME",D^.NAME,"IN",OWNER^.NAME,NAMENAMES[OWNER^.NAMEID],CURRPROC,STMODULE
	lex()
	if lx.symbol=assignsym then
		lex()
		enumseq:=readconstintexpr()
	fi
	d^.index:=enumseq
	d^.blockno:=currblockno
	blockcounts[currblockno]:=1
	++enumseq	
	if lx.symbol=commasym then
		lex()
!		if lx.symbol=rcurlysym then			!this is allowed
!			serror("enum?")
!		fi
	fi
od
skipsymbol(rcurlysym)
end

function createdotop(int opc, unit p,ref strec d)unit=
!opc is j_dot or j_idot
!Deal with field selection for p.d or p->d
unit q,r,poffset,pb,pc
ref strec e,f,prec,panon,pfield,gend
int m,offset,scale
ref fieldrec fl

!CPL "CREATEDOTOP",jtagnames[opc]
!PRINTUNIT(NIL,P)

!check that m is a proper pointer if needed, and a struct or union
m:=p^.mode
if opc=j_idot then			!
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
while f:=f^.nextdupl do
	if f^.owner=prec then
		offset:=f^.offset
		exit
	fi
od

!not found; look for any anon fields
if not f then
	gend:=d						!find generic field name version
	while gend^.prevdupl do
		gend:=gend^.prevdupl
	od

	fl:=prec^.nextfield
	while fl do					!now search linear field list matching generic entries
		if fl^.gendef=gend then
			f:=fl^.def
			offset:=fl^.offset
			exit
		fi
		fl:=fl^.nextfield
	od
fi

if not f then
	terror_ss("Not a field of struct %s %s",d^.name,Strmode(m))
fi


poffset:=createconstunit(offset,tsint)

!will be p->field, or p.field
!p.field: *(p+offset)

if opc=j_idot then				!apply offset to lhs
	p:=createptrop(p)
fi

p:=createunit1(j_dot,p)
p^.offset:=offset

p^.mode:=f^.mode
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
	if p^.tag=j_const then
		p^.value:=p^.value*elemsize
	else
		p:=createunit1(j_scale,p)
		p^.scale:=elemsize
		p^.mode:=tptroffset
	fi
fi
return p
end

function divunit(unit p, int elemtype)unit=
int elemsize

if (elemsize:=ttsize[elemtype])<>1 then
	if p^.tag=j_const then
		p^.value:=p^.value/elemsize
	else
		p:=createunit1(j_scale,p)
		p^.scale:=-elemsize
		p^.mode:=tptroffset
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

pmode:=rmode:=p^.mode
elemmode:=tttarget[pmode]
qmode:=q^.mode

case opc
when assignsym then
	q:=coercemode(q,pmode)
	r:=createunit2(j_assign,p,q)

when addtosym then
	if ttisref[qmode] then		!ref+=ref
		serror("ptr+=ptr")
	fi

	q:=coercemode(q,tptroffset)					!ref+=int
	r:=createunit2(j_addto,p,mulunit(q,elemmode))

when subtosym then
	if ttisref[qmode] then		!ref-=ref
		if not comparemode(pmode,qmode) then
			serror("-= refs don't match")
		fi
		r:=divunit(createunit2(j_sub,p,q),elemmode)
		rmode:=tsint
	else								!ref-=int
		r:=createunit2(j_subto,p,mulunit(q,elemmode))
	fi
else
	serror("Not allowed on ptrs")
esac

r^.mode:=rmode
return r
end

proc addnewfield(ref fieldrec &flist, ref strec d, int offset)=
!new field d has just been created for a record
!add it to the linear list of fields for the record
ref strec e
ref fieldrec f

if d^.name^<>'$' then			!normal field
	f:=pcm_allocz(f^.bytes)
	f^.def:=d
	while d^.prevdupl do			!look for generic entry
		d:=d^.prevdupl
	od
	f^.gendef:=d
	f^.offset:=offset

	f^.nextfield:=flist
	flist:=f

else
	e:=ttnamedef[d^.mode]^.deflist
	while e do
		addnewfield(flist,e,offset+e^.offset)
		e:=e^.nextdef
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
p^.value:=value
p^.nextcase:=casevaluestack[index]
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

case t:= ttbasetype[p^.mode]
when tschar,tsshort then
	insertunit(p,j_widenmem)
	p^.mode:=tsint
when tuchar,tushort,tbool then
	insertunit(p,j_widenmem)
	p^.mode:=tuint
esac
end

function docast(unit p,int t,hard=1,inplace=0)unit=
!apply cast to unit p
!if no cast needed, then just return p
unit q
int s,opc

s:=p^.mode

retry::

!if t=tvoid then return p fi

if s=t then return p fi
opc:=0

if s<16 and t<16 then
	opc:=conversionops[s,t]

elsif ttisref[s] and ttisref[t] then
	if checkpointertypes(s,t,hard) then
		p^.mode:=t
		return p
	fi

elsif ttconst[s] then
	s:=ttconsttype[s]
	goto retry
elsif ttconst[t] then
	t:=ttconsttype[t]
	goto retry
!elsif ttisref[t] and (s>=tfirstint and s<=tlastint) and p^.tag=j_const and p^.value=0 then
elsif ttisref[t] and isintcc(s) and p^.tag=j_const and p^.value=0 then
	opc:=soft_c
fi

if opc=0 then
	if not hard then
		cpl Strmode(s)
		cpl Strmode(t)
		terror_ss("Can't do conversion %s => %s",typename(s),typename(t))
	fi
	opc:=hard_c
fi

case p^.tag
when j_const then		!try and convert
	if eval_convert(p,t,opc) then
		return p
	fi
when j_funcname then
	p^.mode:=t
	return p
when j_add then
	if p^.a^.tag=j_const and p^.b^.tag=j_const then
		p^.value:=p^.a^.value+p^.b^.value
		p^.mode:=t
		p^.tag:=j_const
		return p
	fi
esac

if inplace then
	insertunit(p,j_convert)
	p^.mode:=t
	p^.opcode:=opc
	return nil
else
	q:=createunit1(j_convert,p)
	q^.opcode:=opc
	q^.mode:=t
fi
return q
end

function coercemode(unit p, int t)unit=
int s,opc
unit q

if p^.mode=t then return p fi
docast(p,t,0,1)
return p
end

proc coercemode_inplace(unit p, int t)=
int s,opc
unit q

if p^.mode=t then return fi
docast(p,t,0,inplace:1)
end

proc dostaticassert=
int x
[256]char str
	lex()
	skipsymbol(lbracksym);
	x:=readconstintexpr()
	skipsymbol(commasym)
	checksymbol(stringconstsym)
	if not x then
		memcpy(&.str,lx.svalue,lx.length)
		str[lx.length+1]:=0
		serror(&.str)
	fi
	lex()
	skipsymbol(rbracksym)
end

function createsizeofop(unit p)unit=
unit q
int t,size

!CPL "SIZEOF"
!PRINTUNIT(NIL,P)

t:=p^.mode
switch p^.tag
!when j_nameaddr then
!	size:=ttsize[p^.def^.mode]			!take account of array
when j_name then
	if p^.alength then
		size:=ttsize[p^.def^.mode]/p^.alength			!take account of array
	else
		size:=ttsize[p^.def^.mode]			!take account of array
	fi
when j_const then
	case t
	when trefchar then					!const string
		size:=p^.slength+1
	when trefwchar then
		size:=(p^.wslength+1)*2
	else
		size:=ttsize[t]
	esac

when j_ptr then
	if ttisref[t] and p^.alength then		!result of array=>ptr conversion
		size:=ttsize[tttarget[t]]*p^.alength
	else
		size:=ttsize[t]
	fi

when j_addptr then
	if p^.alength then	!derived from array expr that converted to pointer
		size:=ttsize[tttarget[t]]*p^.alength
	else
		goto cad1
	fi

when j_addrof then
	if p^.a^.tag=j_name and p^.a^.alength then
		size:=ttsize[p^.a^.def^.mode]
	fi

when j_widenmem then
	return createsizeofop(p^.a)

else
cad1::
	size:=ttsize[t]
endswitch

!q:=createconstunit(size,tsint)
q:=createconstunit(size,tullong)
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

	m:=pexpr^.mode
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

proc readstructinfosym=
ref strec d,e
ref paramrec pm
int m, nfields
filehandle f
ichar name
[256]char str

lex()
m:=readcasttype(d,0,pm)

if ttbasetype[m]<>tstruct then
	serror("Struct type expected")
fi
d:=tttypedef[m]

e:=d^.deflist
nfields:=0
while e do
	++nfields
	e:=e^.nextdef
od

name:=d^.name

!sprintf(&.str,"$%s_info.h",name)
print @&.str,"$",,name,,"_info.h"

f:=fopen(&.str,"w");

!fprintf(f,"memberinfo_t $%s[] = {\n",name)
println @f,"memberinfo_t $",,name,,"[] = {"

e:=ttnamedef[m]^.deflist
nfields:=0

while e do
!	fprintf(f,"    {\"%s\", %d,%d,%d,%d,%d,%d}%s\n", e^.name,
!		int32(e^.mode), int32(ttbasetype[e^.mode]),int32(tttarget[e^.mode]),
!		int32(ttsize[e^.mode]),int32(e^.offset),int32(0),(e^.nextdef|","|""))

	println @f,"    {""#"", #,#,#,#,#,#}#", e.name,
		e.mode, ttbasetype[e.mode], tttarget[e^.mode],
		ttsize[e.mode], e.offset, 0, (e.nextdef|","|"")
	++nfields
	e:=e^.nextdef
od

!fprintf(f,"};\n")
println @f,"};"

!fprintf(f,"enum {$%s_length = %d};\n",name,int32(nfields));
println @f,"enum {$#_length = #};",name,nfields

fclose(f)
end

function getmemmode(unit p)int=
!return mode of p, but if p is a widening unit, see past that to
!the original memory mode
if p^.tag=j_widenmem then
	return p^.a^.mode
else
	return p^.mode
fi
end

function readstrinclude:unit=
	unit p
	ichar text

	lex()
	checksymbol(lbracksym)
	lex()
	p:=readexpression()
	checksymbol(rbracksym)
	lex()
	if p^.tag<>j_const or p^.mode<>trefchar then
		serror("String const expected")
	fi

	text:=cast(readfile(p^.svalue))
	if not text then
		serror_s("Can't read strinclude file: %s",p^.svalue)
	fi

	return createstringconstunit(text,strlen(text))
end

=== cc_genmcl.m 13/73 ===
!M Compiler - x64 Target Code Generator 1
!import main
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
import cc_libmcl
import cc_blockmcl


global function codegen_mcl(int n)int=
!generate code for module n
ref strec d,e

!if fverbose then
!	println "Generating MCL code:"
!fi
!
!CPL "GG1"

mclinit()
!CPL "GG2"

stmodule:=moduletable[n].stmodule

!do two passes: module decls first, then procs

d:=stmodule^.deflist
while d do
	case d^.nameid
	when staticid then
!CPL "GG3"
		dostaticvar(d)
!CPL "GG4"
	when procid then
		e:=d^.deflist
		while e do
			case e^.nameid
			when staticid then
!CPL "GG5"
				dostaticvar_fn(e)
!CPL "GG6"
			when frameid then
				if e^.code then
					if e^.code^.tag=j_makelist or \
					    ttbasetype[e^.mode]=tarray and e^.code^.tag=j_const then
!CPL "GG7"
						dostaticvar_fn(e)
!CPL "GG8"
					fi
				fi
			esac
			e:=e^.nextdef
		od

	esac
	d:=d^.nextdef
od
modulecode:=mccode

d:=stmodule^.deflist
while d do
	case d^.nameid
	when procid then
		if d^.code then
!CPL "GG9"
			genprocdef(d)
!CPL "GG10"
		fi
	esac
	d:=d^.nextdef
od

return 1
end

proc genprocdef (ref strec p) =	!GENPROCDEF
[256]char str
int paramoffset,nparams,m
ref strec d
int n,lab,np,offset,reg,i,xreg,ismain,structret
ref opndrec ax,bx
[4]int paramtypes

!CPL "*******************************PROCDEF",P^.NAME

setsegment('C')

initmcdest()
setalign(16)
genassem("!------------------------------------")

currproc:=p
dolabel(p)

!do local decls
frameoffset:=0
paramoffset:=retaddrbytes

nparams:=0
d:=p^.deflist
while d do
	switch d^.nameid
	when frameid then
		frameoffset-:=roundsizetg(ttsize[d^.mode])
		d^.offset:=frameoffset
	when paramid then
		d^.offset:=paramoffset
		paramoffset+:=8
		++nparams
		if nparams<=4 then
 paramtypes[nparams]:=d^.mode fi
	endswitch
	d:=d^.nextdef
od

structretoffset:=0
stacksetinstr:=nil
retbeforeblock:=0
if ttsize[p^.mode]>8 then			!when this proc returns a block
	frameoffset-:=8
	structretoffset:=frameoffset
fi
currblocksize:=0					!for when this proc calls a proc that returns a block

framebytes:=-frameoffset
parambytes:=paramoffset-retaddrbytes
iscallbackproc:=iscallbackfn(p)

n:=targetsize-1					!round to 4 or 8
while framebytes iand n do ++framebytes; --frameoffset od
while parambytes iand n do ++parambytes od

setsegment('C')
ismain:=0
if eqstring(p^.name,"main") then
	ismain:=1
	if parambytes then
		genmainprelude()
	fi
if p^.mode<>tsint then
	gerror("main needs int return type")
fi
fi

!CPL "GENPROCENTRY",P^.NAME,=FRAMEBYTES,=PARAMBYTES
genprocentry(framebytes,parambytes)

if nparams then						!1 or more params
	np:=min(4,nparams)
	if p^.paramlist^.flags=pm_variadic then
!CPL "IS VARIADIC FN"
		for i:=np+1 to 4 do
			paramtypes[i]:=tvoid
		od

		np:=4
	fi
	offset:=16

	for i:=1 to np do
		ax:=genireg(rframe,ptrsize)
		ax^.value:=offset
		ax^.valtype:=int_val
		if isrealcc(paramtypes[i]) then
			genmc(m_fmov, ax, genxreg(xr0+i-1,ttsize[paramtypes[i]]))
		else
			genmc(m_mov, ax, genreg(r10+i-1,8))
		fi
		offset+:=8
	od
fi

if structretoffset then
!genindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, ref strec def=nil)ref opndrec=
	bx:=genindex(areg:rframe,offset:structretoffset)
	genmc(m_mov,bx,genreg(r9,8))
fi

!CPL "PROC ENTRY",=ISCALLBACKPROC
if iscallbackproc then
	strcpy(&.str,"m$pushcallback*")
	genmc(m_call, genname(&.str))
fi

stackaligned:=initial_stackalignment

retindex:=lab:=createfwdlabel()

gencomment("-------------------------------------------------")

enterproc(p^.name)
!genmc(m_sub,genreg(rstack,8),genint(32))
!genmc(m_mov,genreg(r10,8),genstrimm(p^.name,p^.namelen))
!genmc(m_call, genname("$showfunc*"))
!genmc(m_add,genreg(rstack,8),genint(32))


do_stmt(p^.code)

definefwdlabel(retindex)
gencomment("-------------------------------------------------")

if ismain then
!	pushstack(40)					!stack misaligned here
	pushstack(32)
	genmc(m_mov, genreg(r10,8), genint(0));
	genmc(m_call, genname("exit*"))
else
	leaveproc(p^.name)
	genreturn(framebytes,parambytes)
fi

!GENCOMMENT("PROC BODY GOES HERE")
!if p^.mode<tvoid then
!	if not checkblockreturn(p^.code) then
!		gerror("Function needs explicit return statement:"+p.name,p.code)
!	fi
!fi

gencomment("")

p^.mclcode:=mccode
end

proc dolabel(ref strec d)=
[256]char str

strcpy(&.str,"`")
strcat(&.str,getfullname(d))
strcat(&.str,(isexported(d)|"::"|":"))

genmc(m_labelname,genname(&.str))
end

proc dolabel_fn(ref strec d,int dollar=0)=
[256]char str

!sprintf(&.str,"`%s%s.%s.%d:",(dollar|"$"|""),d^.owner^.name,d^.name,int32(d^.blockno))
fprint @&.str,"`##.#.#:",(dollar|"$"|""),d.owner.name,d.name,d.blockno

genmc(m_labelname,genname(&.str))
end

proc dostaticvar(ref strec d)=
int align

case d^.scope
when imported_scope then
	return
esac

align:=getalignment(d^.mode)

if d^.code then
	setsegment('I',align)

	dolabel(d)
	genidata(d^.code)
else
	setsegment('Z',align)
	dolabel(d)
	genmc(m_resb,genint(ttsize[d^.mode]))
fi
end

proc dostaticvar_fn(ref strec d)=
!statics inside procs
int align

case d^.scope
when imported_scope then
	dostaticvar(d)
	return
esac

align:=getalignment(d^.mode)

if d^.code then
	setsegment('I',align)
	dolabel_fn(d,d^.nameid=frameid)
	genidata(d^.code)
else
	setsegment('Z',align)
	dolabel_fn(d,0)

	GENMC(M_RESB,GENINT(TTSIZE[D^.MODE]))
fi
end

proc genprocentry(int fbytes,pbytes)=
!proc entry code

if fbytes or pbytes then			!need frame pointer
	genmc(m_push,dframeopnd)
	genmc(m_mov,dframeopnd,dstackopnd)

!the call and the push means stack is aligned at this point, so keep
!fbytes to multiple of 16
	if fbytes then
		pushstack(roundto(fbytes,16))
!		pushstackfp(roundto(fbytes,16))
	fi
else
	pushstack(8)					!keep aligned
fi

stacksetinstr:=mccodex
end

proc genidata(unit p,int doterm=1,am=1,offset=0)=
int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion
unit q,a,b
ref strec d
real32 sx
[256]char str
[16]char str2
ref opndrec ax

t:=p^.mode
a:=p^.a
b:=p^.b

!PRINTUNIT(NIL,P)

!CPL "GENIDATA",JTAGNAMES[P^.TAG],=offset

case p^.tag
when j_makelist then
	n:=p^.count					!number of supplied params
	if ttbasetype[t]=tarray then
		length:=ttlength[t]			!actual length of array
		q:=a
		for i:=1 to n do
			genidata(q)
			q:=q^.nextunit
		od
		if n<length then			!rest will be zeros
			n:=(length-n)*ttsize[tttarget[t]]		!bytes left
			while n>=8 do
				genmc(m_dq,genint(0,8));
				n-:=8
			od
			to n do
				genmc(m_db,genint(0));
			od
		fi
	else
		isunion:=ttbasetype[t]=tunion

		d:=ttnamedef[t]^.deflist
		size:=ttsize[t]				!total bytes in struct
		offset1:=offset2:=0			!offset so far; 1/2 are in idata/in struct
		q:=a
		for i:=1 to n do
			genidata(q,0)
			if ttbasetype[q^.mode]=tref and q^.strarray then	!IMMEDIATE STRING
				offset1+:=q^.slength
			else
				offset1+:=ttsize[q^.mode]
			fi
			d:=d^.nextdef
			if d and not isunion then
				offset2:=d^.offset
			else
				offset2:=size
			fi

			padding:=offset2-offset1
			if padding>0 then
				padding:=offset2-offset1
				if padding>0 then
					genmc(m_resb,genint(padding))
				fi
				offset1:=offset2
			fi
			q:=q^.nextunit
		od
		if offset2<size then
			n:=size-offset2
			while n>=8 do
				genmc(m_dq,genint(0,8));
				n-:=8
			od
			to n do
				genmc(m_db,genint(0));
			od
		fi
	fi
	return
when j_const then
!	if t>=tfirstint and t<=tlastreal then

	if isintcc(t) or isrealcc(t) then
		if t=tfloat then
			sx:=p^.xvalue
			genmc(m_dd,genint(int@(sx),4))
		else
			genmc((ttsize[t]|m_db, m_dw, 0, m_dd, 0,0,0, m_dq|0),genint(p^.value,ttsize[t]))
		fi
	elsif ttbasetype[t]=tref then
!CPL "GENIDATA/REF",STRMODE(T)
		padding:=0
doref::
		if p^.value=0 then
			genmc(m_dq,genint(0,8))
		elsif p^.strarray then					!immediate string (char[])
!CPL "GENIDATA/STRARRAY",strmode(tttarget[t])
			if ttsize[tttarget[t]]=1 then
				genmc(m_defstr,genstrimm(p^.svalue,p^.slength))
			else
				genmc(m_defwstr,genwstrimm(p^.wsvalue,p^.wslength))
			fi
			if padding>0 then
				genmc(m_resb,genint(padding))
			fi

		elsif p^.isstrconst then
!CPL "STRING"
			genmc(m_dq, genstrimm(p^.svalue,p^.slength))
			if padding>0 then
				genmc(m_resb,genint(padding))
			fi
		elsif p^.iswstrconst then
!CPL "WSTRING"
			genmc(m_dq, genwstrimm(p^.wsvalue,p^.wslength))
			if padding>0 then
				genmc(m_resb,genint(padding))
			fi
		else
			genmc(m_dq, genint(p^.value))
		fi
	elsif ttbasetype[t]=tarray then
		padding:=(ttlength[t]-p^.slength)*ttsize[tttarget[t]]
		goto doref
	else
		CPL Strmode(t)
		GERROR("IDATA/SCALAR")
	fi
	return
when j_name, j_funcname then
	d:=p^.def
	case d^.nameid
	when staticid,procid then
		ax:=genmemaddr_d(d)
		if ax then
			ax:=applyoffset(ax,offset)
		fi
		genmc((am=0 or ttsize[p^.mode]=8|m_dq|m_dd), ax)

	else
		gerror("Idata &frame",p)
	esac	
	return
when j_add then
	if a^.tag=j_name and b^.tag=j_const then
		d:=a^.def
		case d^.nameid
		when staticid then
			strcpy(&.str,"`")
			if d^.scope=function_scope then
				strcat(&.str,currproc^.name)
				strcat(&.str,",")
			fi
			strcat(&.str,d^.name)
			strcat(&.str,"+")

!			sprintf(&.str2,"%lld",b^.value)
			getstrint(b.value, &.str2)

			strcat(&.str,&.str2)
			genmc(m_dq, genname(&.str))
		else
			gerror("Add/Idata &frame")
		esac	
	elsif a^.tag=j_const and b^.tag=j_const and ttbasetype[a^.mode]=tref then		!ASSUME REF+REF
!		sprintf(&.str,"%lld+%lld",a^.value,b^.value)
		print @&.str,a^.value,,"+",,b^.value

		genmc(m_dq,genname(&.str))

	else
		gerror("1:Runtime or unsupported expr in static data")
	fi
	return
when j_addrof then
	if a^.tag=j_ptr then
		genidata(a^.a,offset:offset)
	else
		genidata(a, am:0,offset:offset)
	fi

when j_addptr,j_subptr then
	if b^.tag<>j_const then gerror("Complex ptr expr in static data") fi
	genidata(a,offset:b^.value*p^.ptrscale+offset)

when j_convert then
	genidata(a,offset:offset)

else
PRINTUNIT(NIL,P)
	gerror("2:Runtime expr in static data",p)
esac
end

proc genmainprelude=

genassem("	sub	Dstack,152")
genassem("	sub	Dstack,8")
genassem("	lea	D0,[Dstack+8]")
genassem("	push	D0")
genassem("	sub	Dstack,32")
genassem("	lea	D0,[Dstack+196]")
genassem("	mov	[Dstack],D0")
genassem("	lea	D0,[Dstack+184]")
genassem("	mov	[Dstack+8],D0")
genassem("	lea	D0,[Dstack+176]")
genassem("	mov	[Dstack+16],D0")
genassem("	mov	A0,0")
genassem("	mov	[Dstack+24],A0")
genassem("	mov	D10,[Dstack]")
genassem("	mov	D11,[Dstack+8]")
genassem("	mov	D12,[Dstack+16]")
genassem("	mov	D13,[Dstack+24]")
genassem("	call	__getmainargs*")
genassem("	add	Dstack,48")
genassem("	sub	Dstack,32")
genassem("	mov	A0,[Dstack+180]")
genassem("	mov	[Dstack],A0")
genassem("	mov	D0,[Dstack+168]")
genassem("	mov	[Dstack+8],D0")
genassem("	mov	D10,[Dstack]")
genassem("	mov	D11,[Dstack+8]")
genassem("	call	.main")
genassem("	mov A10,A0")
genassem("	call exit*")
gencomment("")
genassem(".main::")

end

=== cc_libmcl.m 14/73 ===
!M Compiler - x64 Target Code Generator 3
!import main
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib

GLOBAL INT NMEMADDR
GLOBAL INT NMEM

!const fdosimple=0
const fdosimple=1

const cc1 = 1
const cc2 = 2
const cc3 = 3
global const ccmode=cc3

global const ptrsize=8
global const targetsize=8
global const retaddrbytes=targetsize*2

global int fshowfullnames=1		! show fully qualified names in output
global int fshowmsource=0

global int fabsused=0
global int fchsused=0
global int kk0used=0

global int retindex
global int stackaligned
global const initial_stackalignment = 1

!These opcodes represent a more general, orthogonal version of x64:
!Int registers R0 to Rmax (Rmax is 15 for actual x64) (8, 16, 32, 64 bits)
!Float registers X0 to Xmax (32, 64 bits)
!Fewer restrictions on:
! imm fields, and width of imm field (can be up to 64 bits if needed)
! operands that can be pushed or popped
! register needed for variable shift (need not be cl)
! Extra reg for double-with mul or div is not specified; that will need to
!  be known to code generator
! Uses irem/urem op that needs expanding to multiple ops
! Other opcodes may need expanding to multiple too

global tabledata() []ichar mclnames =

	(m_comment,			$),	! str
	(m_blank,			$),	!

	(m_label,			$),	! lab
	(m_labelname,		$),	!

	(m_mov,				$),	! R,imm; R,R; R,mem; mem,imm; mem,R
	(m_push,			$),	! imm; R; mem; X
	(m_pop,				$),	! R; mem; X
	(m_lea,				$),	! mem
	(m_cmovcc,			$),	! cc,R,R; cc,R,mem
	(m_fmov,			$),	! X,imm, X,X; X,mem, mem,X; R,X; X,R (no conversions)

	(m_iwiden,			$),	! R,R; R,mem; R (single operand: doubles existing width)
	(m_uwiden,			$),	! R,R; R,mem; R
	(m_inarrow,			$),	! R,R (truncates to dest width then widens to original width)
	(m_unarrow,			$),	! R,R

	(m_call,			$),	! imm; R; mem
	(m_ret,				$),	!
	(m_retn,			$),	! imm

	(m_jmp,				$),	! lab
	(m_jmpcc,			$),	! cond, lab
	(m_exch,			$),	! R,R; R,mem

	(m_add,				$),	! R,imm; R,R; R,mem; mem,R
	(m_sub,				$),	!
	(m_imul,			$),	!
!	(m_umul,			$),	!

	(m_idiv,			$),	! R,imm; R,R; R,mem; mem,R
	(m_udiv,			$),	!

	(m_irem,			$),	! R,imm; R,R; R,mem; mem,R
	(m_urem,			$),	!

	(m_and,				$),	! R,imm; R,R; R,mem; mem,R
	(m_or,				$),	!
	(m_xor,				$),	!
	(m_test,			$),	!

	(m_cmp,				$),	!

	(m_shl,				$),	! R,imm; R,R; R,mem; mem,R (varible count may not be CL)
	(m_ishr,			$),	!
	(m_ushr,			$),	!

	(m_neg,				$),	! R; mem
	(m_not,				$),	! R; mem

	(m_inc,				$),	! R; mem
	(m_dec,				$),	! R; mem
	(m_setcc,			$),	!

	(m_fneg,			$),	! X
	(m_fabs,			$),	! X
	(m_fsqrt,			$),	! X,X; X,mem

	(m_fadd,			$),	! X,imm; X,X; X,mem
	(m_fsub,			$),	!
	(m_fmul,			$),	!
	(m_fdiv,			$),	!

	(m_fcmp,			$),	!

	(m_ufix,			$),	! R,X; R,mem
	(m_ifix,			$),	! R,X; R,mem
	(m_ufloat,			$),	! X,R; X,mem
	(m_ifloat,			$),	! X,R; X,mem
	(m_fwiden,			$),	! X,X; X,mem
	(m_fnarrow,			$),	! X,X; X,mem

	(m_fmin,			$),	! X,X; X,mem
	(m_fmax,			$),	! X,X; X,mem

	(m_resb,			$),	! imm,...
	(m_db,				$),	! imm,...

	(m_dw,				$),	!
	(m_dd,				$),	!
	(m_dq,				$),	!
	(m_defstr,			$),	! string constant in situ, maps to eg "db 'ABC',13,10,0"
	(m_defwstr,			$),	!
	(m_align,			$),	! imm
	(m_segment,			$),	! seg
	(m_cdq,				$),	!

	(m_assem,			$),	! str
	(m_end,				$),	!			!marks end of non-specific opcodes

!The following are extra-specific opcodes for x64 target
	(mx_imul2,			$),
	(mx_sar,			$),
	(mx_shr,			$),
	(mx_div,			$),
	(mx_movzx,			$),
	(mx_movsx,			$),
	(mx_inot,			$),
	(mx_mul,			$),

	(mx_movd,			$),
	(mx_movq,			$),

	(mx_addss,			$),
	(mx_addsd,			$),
	(mx_subss,			$),
	(mx_subsd,			$),
	(mx_mulss,			$),
	(mx_mulsd,			$),
	(mx_divss,			$),
	(mx_divsd,			$),
	(mx_comiss,			$),
	(mx_comisd,			$),
	(mx_cvtsi2ss,		$),
	(mx_cvtsi2sd,		$),
	(mx_cvtss2sd,		$),
	(mx_cvtsd2ss,		$),
	(mx_cvttss2si,		$),
	(mx_cvttsd2si,		$),

	(mx_sqrtss,			$),
	(mx_sqrtsd,			$),
	(mx_minss,			$),
	(mx_maxss,			$),
	(mx_minsd,			$),
	(mx_maxsd,			$),
	(mx_xorps,			$),
	(mx_xorpd,			$),
	(mx_andps,			$),
	(mx_andpd,			$),

end

global tabledata() [0:]ichar opndnames =
	(a_none=0,	$),
	(a_reg,		$),
	(a_imm,		$),
	(a_strimm,	$),		!immediate string (for comments, m_assem etc)
	(a_mem,		$),		!all memory modes: [d], [R], [R*4+R2+d+imm] etc
	(a_code,	$),		!mcl code seq
	(a_xreg,	$),		!xmm register
end

global tabledata() [0:]ichar regnames =
	(rnone=0,	$),
	(r0,		$),
	(r1,		$),
	(r2,		$),
	(r3,		$),
	(r4,		$),
	(r5,		$),
	(r6,		$),
	(r7,		$),
	(r8,		$),
	(r9,		$),
	(r10,		$),
	(r11,		$),
	(r12,		$),
	(r13,		$),
	(rframe,	$),
	(rstack,	$),
	(rfpu,		$)
end

global const rcx=r10
global const rdx=r11
global const r14=rframe
global const r15=rstack

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
	(fgt_cond=19,	"fgt",	"a"),
	(feq_cond=20,	"feq",	"z"),
	(fne_cond=21,	"fne",	"nz")
end

global tabledata() [0:]ichar valtypenames =
	(no_val=0,		$),
	(int_val,		$),
	(real_val,		$),
	(string_val,	$),
	(wstring_val,	$),
	(label_val,		$),
	(name_val,		$),
	(stringix_val,	$),
	(wstringix_val,	$),
	(intix_val,		$),
	(realix_val,	$),
end

global record opndrec = 	!32 bytes
	ref strec def		!nil, or handle of strec for var name
	union
		int64 value		!int/real/string value
		real64 xvalue
		ichar svalue
		ref word16 wsvalue
	end
	int32 size		!byte size of operand: usually 4,8, also 1,2 and N for block
	union
		int32 index		!int/real/string index
		int32 slength
		int32 wslength
	end

	byte mode		!a_reg etc, low level operand details
	byte reg			!0, or main register
	byte regix		!0, or index register
	byte valtype
	byte scale		!1, or scale factor for regix
	byte isglobal	!0/1 for labels
	byte isfloat	!used with realix_val

	byte s3		!spare

end

global record mclrec = 	!32 bytes
	ref mclrec nextmcl
	ref opndrec a,b
	byte opcode
	byte cond
	byte fileno
	byte spare
	int32 lineno
end

global ref mclrec mccode, mccodex		!genmc adds to this linked list

global ref mclrec modulecode			!mclrec linked list for module-level data

global int currsegment=0		!

global int currzdataalign=0
global int curridataalign=0

global int framebytes			!local stackframe size
global int parambytes
global int frameoffset
global int isthreadedproc
global int iscallbackproc

global int structretoffset			!0, or offset of R9 copy within struct
global ref mclrec stacksetinstr		!caller of any fn: instr that sets sp
!global int structrettemp			!caller of any fn: 0, or offset of struct area
global int currblocksize			!0, or set to largest block ret value
global int retbeforeblock			!1 when blockcall follows explicit return
!global int currblockoffset			!0, or current offset of currblocksize area

global ref opndrec dstackopnd
global ref opndrec dframeopnd


!global int labelno=0
global ref opndrec zero_opnd=nil
global unit zero_unit

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

global proc mclinit=

!CPL "MCLINIT"
zero_opnd:=genint(0)
zero_unit:=createconstunit(0,tsint)
zero_unit^.mode:=tsint
dframeopnd:=genreg(rframe,8)
dstackopnd:=genreg(rstack,8)

initmcdest()

end

global function gettargetdata(int f64)int=
CPL "GETTARGETDATA"
RETURN 1
end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 

mccode:=mccodex:=nil
end

global proc genmc(int opcode, ref opndrec a=nil,b=nil)=
ref mclrec m, oldm

m:=pcm_alloc(mclrec.bytes)

m^.lineno:=clineno
m^.opcode:=opcode

m^.a:=a
m^.b:=b

case opcode
when m_mov then
	if a and a^.mode=a_reg and b and b^.mode=a_mem then
		oldm:=mccodex
		if oldm and oldm^.opcode=m_mov and oldm^.a^.mode=a_mem and oldm^.b^.mode=a_reg then
			if  sameoperand(a,oldm^.b) and sameoperand(oldm^.a,b) then
!cpl "SAVE LOAD"
				return 			!don't generate the load
			fi
		fi
	fi
when m_jmp then
	case mccodex^.opcode
	when m_ret, m_retn, m_jmp then
!CPL "SUPPRESS JMP BECAUSE PREVIOUS OP IS",MCLNAMES[MCCODEX^.OPCODE]
		return
	esac
when m_push,m_pop then
	stackaligned ixor:=1
!when m_uwiden, m_iwiden then
!	 if b^.mode=a_imm then
!	fi
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

genmc(opcode,genstrimm(s))
end

function newopnd:ref opndrec=
ref opndrec a
a:=pcm_allocz(opndrec.bytes)
return a
end

global function duplopnd(ref opndrec a)ref opndrec=
ref opndrec b
b:=pcm_alloc(opndrec.bytes)
b^:=a^
return b
end

global function genxreg(int xreg,size=8)ref opndrec=
ref opndrec a

a:=newopnd()

a^.mode:=a_xreg
a^.reg:=xreg
a^.size:=size
return a
end

global function genindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, ref strec def=nil)ref opndrec=
!construct a mem address mode
ref opndrec a
a:=newopnd()

a^.mode:=a_mem
a^.reg:=areg

a^.regix:=ireg
a^.scale:=scale
a^.size:=(size|size|scale)

a^.value:=offset
if offset then a^.valtype:=int_val fi

a^.def:=def

if labno then		!assume label no
	a^.valtype:=label_val
	a^.value:=labno

elsif def and isframe(def) then
	if areg then gerror("gen/index/areg") fi
	a^.reg:=rframe
fi
!CPL =OFFSET,=DEF.NAME
return a
end

proc writemclblock(ref mclrec m)=
!block single block of mc code, usually belonging to one proc
!initstr=1 to initialise string o/p for single block
!initgenstr() when initstr
int i

i:=1
while m do
	writemcl(i,m)
	++i
	m:=m^.nextmcl
od
end

global function writemclcode(ichar caption,int nmodule)ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
ref strec d

gs_init(dest)

gs_strln(dest,caption)
gs_strln(dest,"---------------------------------------------")

gs_strln(dest,"MODULE CODE")
writemclblock(modulecode)
gs_strln(dest,"---------------------------------------------")

d:=moduletable[nmodule].stmodule^.deflist

while d do
	if d^.nameid=procid and d^.mclcode then
		gs_str(dest,"PROC:")
		gs_strln(dest,d^.name)
		writemclblock(d^.mclcode)
	fi
	d:=d^.nextdef
od

gs_strln(dest,"---------------------------------------------")

return dest
end

global proc gencomment(ichar s)=
if s=nil or s^=0 then
	genmc(m_blank)
else
	genmcstr(m_comment,s)
fi
end

global function genstrimm(ichar s,int length=-1)ref opndrec=
ref opndrec a
a:=newopnd()
a^.mode:=a_imm
if length<0 then
	length:=strlen(s)
fi
a^.svalue:=pcm_alloc(length+1)
memcpy(a^.svalue,s,length+1)

a^.valtype:=string_val
a^.size:=ptrsize
a^.slength:=length
return a
end

global function genwstrimm(ref word16 s,int length=-1)ref opndrec=
ref opndrec a
a:=newopnd()
a^.mode:=a_imm
!if length<0 then
!	length:=strlen(s)
!fi
a^.wsvalue:=pcm_alloc(length*2+2)
memcpy(a^.wsvalue,s,(length+1)*2)

a^.valtype:=wstring_val
a^.size:=ptrsize
a^.slength:=length
return a
end

global function genname(ichar s)ref opndrec=
ref opndrec a
a:=newopnd()
a^.mode:=a_imm
a^.svalue:=pcm_copyheapstring(s)
a^.valtype:=name_val
a^.size:=ptrsize
return a
end

proc writemcl(int index,ref mclrec mcl)=

GS_STRLN(DEST,STRMCL(MCL))

end

global function strmcl(ref mclrec mcl)ichar=
static [512]char str
[512]char opnds
[256]char opnd2
[128]char opcname
ref opndrec a,b
int opcode,cond,sizepref

opcode:=mcl^.opcode
cond:=mcl^.cond
a:=mcl^.a
b:=mcl^.b

case opcode
when m_assem then
	return a^.svalue
when m_blank then
	return ""
when m_comment then
!	sprintf(&.str,";%s",a^.svalue)
	print @&.str,";",,a.svalue
	return &.str

when m_labelname then
!	sprintf(&.str,"%s",a^.svalue)
	strcpy(&.str,a^.svalue)
	return &.str

when m_label then
	if b then
!		sprintf(&.str,"L%d:%s	<%s>",int32(a^.value),(a^.isglobal|":"|""),b^.def^.name)
		fprint @&.str,"L#:#	<#>",a.value,(a.isglobal|":"|""),b.def.name

	else
!		sprintf(&.str,"L%d:%s",int32(a^.value),(a^.isglobal|":"|""))
		fprint @&.str,"L#:#",a.value,(a.isglobal|":"|"")
	fi
	return &.str

esac

case opcode
when m_jmpcc then
!	sprintf(&.opcname,"j%s",asmcondnames[cond])
	print @&.opcname,"j",,asmcondnames[cond]

when m_setcc then
!	sprintf(&.opcname,"set%s",asmcondnames[cond])
	print @&.opcname,"set",,asmcondnames[cond]

when m_cmovcc then
!	sprintf(&.opcname,"cmov%s",asmcondnames[cond])
	print @&.opcname,"cmov",,asmcondnames[cond]

else
	strcpy(&.opcname,mclnames[opcode]+2)
esac

ipadstr(&.opcname,11," ")
!sprintf(&.str,"\t%s",&.opcname)
print @&.str,"\t",,&.opcname

if a and b then		!2 operands
	sizepref:=needsizeprefix(opcode,a,b)

	strcpy(&.opnd2,stropnd(b,sizepref))
!	sprintf(&.opnds,"%s,\t%s", stropnd(a,sizepref),&.opnd2)
	fprint @&.opnds,"#,\t#", stropnd(a,sizepref),&.opnd2

elsif a and a^.mode then								!1 operand
	if opcode=m_call then
		strcpy(&.opnds,stropnd(a,0))
	else
		strcpy(&.opnds,stropnd(a,1))
	fi
else
	opnds[1]:=0
fi

if opnds[1] then
	strcat(&.str,&.opnds)
fi

return &.str
end

global function stropnd(ref opndrec a,int sizeprefix=0,debug=0)ichar=
static [512]char str
[128]char str2
ichar plus,t

case a^.mode
when a_reg then
	return getregname(a^.reg,a^.size)

when a_imm then
	return strvalue(a)

when a_mem then
!	sprintf(&.str,"%s[",getsizeprefix(a^.size,sizeprefix))
	print @&.str,getsizeprefix(a^.size,sizeprefix),,"["

	plus:=""
	if a^.reg then
		strcat(&.str,getregname(a^.reg,ptrsize))
		plus:="+"
	fi
	if a^.regix then
		strcat(&.str,plus)
		strcat(&.str,getregname(a^.regix,ptrsize))
		plus:="+"
		if a^.scale>1 then
!			sprintf(&.str2,"*%d",int32(a^.scale))
			print @&.str2,"*",,a.scale
			strcat(&.str,&.str2)
		fi
	fi
	if a^.def or a^.valtype then			!.valtype should be 'I' if used
		t:=strvalue(a)
		if t^<>'-' then
			strcat(&.str,plus)
		fi
		strcat(&.str,t)
	fi
	strcat(&.str,"]")
when a_strimm then
!	sprintf(&.str,"/%s/*%d",a^.svalue,int32(a^.slength))
	fprint @&.str,"/#/*#",a.svalue,a.slength

when a_xreg then
	return fgetregname(a^.reg,a^.size)

else
	return "<BAD OPND>"
esac

return &.str
end

global function strvalue(ref opndrec a)ichar=
static [512]char str
[128]char str2
ref strec def
int64 value

def:=a^.def
value:=a^.value

if def then
	case def^.nameid
	when staticid then
		if def^.owner^.nameid=procid then
!			sprintf(&.str,"`%s.%s.%d",def^.owner^.name,def^.name,int32(def^.blockno))
			fprint @&.str,"`#.#.#",def.owner.name,def.name,def.blockno

		else
			strcpy(&.str,getfullname(def))
			if isimported(def) then
				strcat(&.str,"*")
			fi
		fi
	when frameid, paramid then
		strcpy(&.str,getfullname(def))
	else
		strcpy(&.str,getfullname(def))
		if isimported(def) then
			strcat(&.str,"*")
		fi
	esac
	if a^.valtype=int_val and value<>0 then
!		sprintf(&.str2,"%s%lld",(value>0|"+"|""),value)
		print @&.str2,(value>0|"+"|""),,value
		strcat(&.str,&.str2)
	fi
	return &.str
fi

case a^.valtype
when int_val,intix_val then
!	sprintf(&.str,"%lld",value)
	getstrint(value,&.str)

when real_val,realix_val then
!	sprintf(&.str,"%f (%s)",a^.xvalue, (a^.size=8|"double"|"float"))
!	fprint @&.str,"# (#)",a.xvalue, (a^.size=8|"xxxdouble"|"float")
	print @&.str,a.xvalue

when string_val,stringix_val then
	if strlen(a^.svalue)+4<str.len then
!		sprintf(&.str,"\"%s\"*%d",a^.svalue,int32(a^.slength))
		fprint @&.str,"\"#\"*#",a.svalue,a.slength
	else
		return "<LONGSTR>"
	fi
when wstring_val,wstringix_val then
	return "<WSTRING>"
when name_val then
	return a^.svalue
when label_val then
!	sprintf(&.str,"L%d",int32(value))
	print @&.str,"L",,value
else
	str[1]:=0
esac

return &.str
end

global proc setsegment(int seg,align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
if seg=currsegment then
	return
fi
case seg
when 'I' then genmc(m_segment,genname("idata"))
when 'Z' then genmc(m_segment,genname("zdata"))
when 'C' then genmc(m_segment,genname("code"))
when 'R' then genmc(m_segment,genname("rodata"))
esac
currsegment:=seg
currzdataalign:=curridataalign:=0
setalign(align)
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

global proc genassem(ichar s)=
genmcstr(m_assem,s)
end

global function strlabel(int n)ichar=
static [16]char str
!sprintf(&.str,"L%d",int32(n))
print @&.str,"L",,n
return &.str
end

global function makeindirect(ref opndrec a,int size=0)ref opndrec =
!turn m reg operand into indirect mem version
ref opndrec b

b:=duplopnd(a)

case b^.mode
when a_reg then
	b^.mode:=a_mem
else
CPL =STROPND(B)
	gerror("makeind")
esac
if size then b^.size:=size fi
return b
end

global function applyoffset(ref opndrec a,int offset,int size=0)ref opndrec=
!astr is an asm operand
!add possible byte offset
ref opndrec b

if offset=0 and size=0 then
	return a
fi
b:=duplopnd(a)
b^.value+:=offset
b^.valtype:=int_val
if size then
	b^.size:=size
fi

return b
end

global function applysize(ref opndrec a,int size)ref opndrec=
!astr is an asm operand
!add possible byte offset

if a^.size<>size then
	a:=duplopnd(a)
	a^.size:=size
fi
return a
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

!CPL "GENRET",CURRPROC^.NAME,FBYTES,PBYTES

iscallback:=iscallbackfn(currproc)
retbeforeblock:=1			!actually, this may be after a block, but then it doesn't matter

if fbytes or pbytes then
	if fbytes then
		popstack(roundto(fbytes,16))
	fi
	genmc(m_pop,dframeopnd)
	if iscallback then
		strcpy(&.str,"	call m$popcallback*")
		genassem(&.str)
	fi

	genmc(m_ret)
else

	if iscallback then
		strcpy(&.str,"	call m$popcallback*")
		genassem(&.str)
	fi

	popstack(8)
	genmc(m_ret)
fi
stackaligned:=initial_stackalignment		!for any following code
end

global function getsizeprefix(int size,enable=0)ichar=
if not enable then return "" fi
case size
when 1 then return "byte "
when 2 then return "word16 "
when 4 then return "word32 "
when 8 then return "word64 "
esac
return "N:"
end

global function needsizeprefix(int opcode,ref opndrec a,b)int=

case opcode
when m_uwiden,m_iwiden, mx_movsx, mx_movzx, mx_cvtsi2ss, mx_cvtsi2sd then
	return 1
when m_ifix then
	return 1
when m_shl, mx_shr, mx_sar then
	if a^.mode=a_mem then return 1 fi
	return 0
esac

if a^.mode=a_reg or a^.mode=a_xreg or b^.mode=a_reg or b^.mode=a_xreg then
	return 0
fi
return 1
end

global function changeopndsize(ref opndrec a,int size)ref opndrec=
ref opndrec b

if a^.size<>size then
	b:=duplopnd(a)
	b^.size:=size
	return b
fi
return a
end

global function genint(int64 x,int size=4)ref opndrec=
ref opndrec a

a:=newopnd()
a^.mode:=a_imm
a^.value:=x
a^.valtype:=int_val
a^.size:=size
return a
end

global function genreal(real64 x,int size=8)ref opndrec=
ref opndrec a

a:=newopnd()
a^.mode:=a_imm
a^.xvalue:=x
a^.valtype:=real_val
a^.size:=size
return a
end

global function genimm(unit p,int size=0)ref opndrec=
!assume p is a const unit, or possible a name (gives a name
ref opndrec a
int t

a:=newopnd()
a^.mode:=a_imm

!a^.value:=p^.
case p^.tag
when j_const then
	t:=p^.mode
	if isintcc(t) then
		a^.value:=p^.value
		a^.valtype:=int_val
		a^.size:=(size|size|ttsize[t])
	elsif isrealcc(t) then
		a^.xvalue:=p^.xvalue
		a^.valtype:=real_val
		a^.size:=(size|size|ttsize[t])
	else
		gerror("GENIMM/MODE?")
	fi

when j_name then
	a^.def:=p^.def
	a^.size:=ttsize[p^.def^.mode]
else
	gerror("genimm/unit")
esac

return a
end

global function genlabel(int x,isglobal=0)ref opndrec=
!x is a label index
!generate immediate operand containing label
ref opndrec a

a:=newopnd()
a^.size:=targetsize
a^.mode:=a_imm
a^.value:=x
a^.valtype:=label_val
a^.isglobal:=isglobal
return a
end

global function genmem_u(unit p,int size=0)ref opndrec=
return genmem_d(p^.def,ttsize[p^.mode])
end

global function genmem_d(ref strec d,int size=0)ref opndrec=
ref opndrec a

++NMEM

a:=newopnd()
a^.mode:=a_mem

if isframe(d) and fshowfullnames then
	a^.reg:=rframe
fi
a^.def:=d
a^.size:=(size|size|ttsize[d^.mode])

return a
end

global function genmemaddr_u(unit p)ref opndrec=
return genmemaddr_d(p^.def)
end

global function genmemaddr_d(ref strec d)ref opndrec=
ref opndrec a
++NMEMADDR

a:=newopnd()
a^.mode:=a_imm

if isframe(d) and fshowfullnames then
	a^.reg:=rframe
fi
a^.def:=d
a^.size:=ptrsize

return a
end

global function genreg(int reg,size=4)ref opndrec=
static [0:9]int isnormal=(0, 1,1,0,1,0,0,0,1)
int offset
ref opndrec a

if size<=8 and isnormal[size] then
	a:=newopnd()
	a^.mode:=a_reg
	a^.reg:=reg
	a^.size:=size
else
!	a:=newopnd()
!	a^.mode:=a_reg
!	a^.reg:=reg
!	a^.size:=8
!	offset:=getstructtemp(size)
!	genmc(m_lea,a,genindex(areg:rframe,offset:offset))
!
GERROR("GENREG/BLOCK SIZE")
!	return genblockreg(reg,size)
fi

return a
end

global function genireg(int reg,size=4)ref opndrec=
ref opndrec a

a:=newopnd()
a^.mode:=a_mem
a^.reg:=reg
a^.size:=size

return a
end

global function getopndsize_u(unit p)int=
return ttsize[p^.mode]
end

global function getopndsize_d(ref strec d)int=
return ttsize[d^.mode]
end

global function getmclcond(int opc,m)int=
int signedx

signedx:=stdtypesigned[ttbasetype[m]]		!note real types will use unsigned codes

if isrealcc(m) then
	case opc
	when j_lt then return flt_cond
	when j_le then return fle_cond
	when j_ge then return fge_cond
	when j_gt then return fgt_cond
	when j_eq then return feq_cond
	when j_ne then return fne_cond
	esac
else
	case opc
	when j_lt then return (signedx|lt_cond|ltu_cond)
	when j_le then return (signedx|le_cond|leu_cond)
	when j_ge then return (signedx|ge_cond|geu_cond)
	when j_gt then return (signedx|gt_cond|gtu_cond)
	when j_eq then return eq_cond
	when j_ne then return ne_cond
	esac
fi

return 0
end

global function getfullname(ref strec d)ichar=

return d^.name

end

global function roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
while size iand (targetsize-1) do ++size od
return size
end

global function iscallbackfn(ref strec p)int=
!return 1 if p is a function with clang atribute (needs caller stack adjust)

!RETURN 1

return p^.attribs.ax_callback
end

global function getregname(int reg,size=4)ichar=
static [1..8]ichar prefix=("B","W","","A","","","","D")
static [32]char str
[16]char str2
ichar rs

if size>8 then
	RETURN "DBIG"
FI

case reg
when rnone then return "-"
when rframe then rs:="frame"
when rstack then rs:="stack"
else
!	sprintf(&.str2,"%d",int32(reg-r0))
	getstrint(reg-r0,&.str2)
	rs:=&.str2
esac

!sprintf(&.str,"%s%s",prefix[size],rs)
print @&.str,prefix[size],,rs

return &.str
end

global function getblockname(int reg,size)ichar=
static [32]char str
!sprintf(&.str,"N%d(%d)",int32(reg-1),int32(size))
fprint @&.str,"N#(#)",reg-1,size
return &.str
end

global function fgetregname(int reg,size=8)ichar=
static [32]char str

!sprintf(&.str,(size=8|"DX%d"|"SX%d"),int32(reg-xr0))
print @&.str,(size=8|"DX"|"SX"),,reg-xr0

return &.str
end

global function issimple(unit p)int=
!return 1 if p is simple: can be evaluated using no registers

if not fdosimple then return 0 fi
return issimple0(p,0)

end

function issimple0(unit p,int level)int=
!return 1 if p is simple: can be evaluated using no registers
unit a

!return 0

++level

IF LEVEL>5 THEN
!IF LEVEL>3 THEN
!	CPL =LEVEL
	return 0
FI
a:=p^.a

switch p^.tag
when j_const, j_name, j_funcname then
	return 1

when j_ptr then
	if a^.tag=j_addptr or a^.tag=j_subptr then
		if issimple0(a^.a,level) and issimple0(a^.b,level) then
			return 1
		fi
	else
		return issimple0(a,level)
	fi

!when j_add, j_sub, j_iand, j_ior, j_ixor, j_assign then
when j_add, j_sub, j_iand, j_ior, j_ixor then
dobin::
	if issimple0(a,level) and issimple0(p^.b,level) then
		return 1
	fi

when j_mul then
	if gettypecat(p)='I' then
		goto dobin
	fi

!when j_widenmem then
!CPL "ISSIMPLE/WIDEN",JTAGNAMES[A^.TAG]
!	return issimple0(a)

when j_convert then
	case p^.opcode
!	when soft_c,hard_c,swiden_c,uwiden_c,truncate_c then
	when soft_c,hard_c,swiden_c,uwiden_c then
		return issimple0(a,level)
	esac

when j_addrof then
	return issimple0(a,level)

when j_dot then
	return issimple0(a,level)

!when j_preincr, j_predecr, j_postincr, j_postdecr then
!	return issimple(a)

when j_shl, j_shr then
	if issimple0(a,level) and p^.b^.tag=j_const then
		return 1
	fi

!when j_addptr, j_subptr then
!	if issimple0(a) and issimple0(p^.b) then
!		return 1
!	fi


!ELSE
!CPL "ISSIMPLE",JTAGNAMES[P^.TAG]
end switch

return 0
end

global function issimplepm(unit p)int=
!return 1 if p is simple: can be evaluated using no registers

if not fdosimple then return 0 fi
!return 0

case p^.tag
when j_const,j_name then
	return 1
!when j_nameaddr then
!	if p^.def^.nameid=staticid then
!		return 1
!	fi

!THE FOLLOWING are dangerous, as simple params are calculated into d10..d13.
!At d13, regs may overflow into rframe.

!when j_add, j_sub, j_iand, j_ior, j_ixor then
!	if issimplepm(p^.a) and issimplepm(p^.b) then
!		return 1
!	fi

esac
return 0
end

global function getaregs(ref opndrec ax)int=
int n

case ax^.mode
when a_reg then
	return 1

when a_mem then
	n:=0
	if ax^.reg and ax^.reg<>rframe then ++n fi
	if ax^.regix then ++n fi
	return n

when a_xreg then
	return 0

else
	gerror("getaregs")
esac
return 0
end

global function getlregs(ref opndrec ax)int=
case ax^.mode
when a_reg, a_xreg then
	return 1
when a_mem then
	if (ax^.reg and ax^.reg<>rframe) or ax^.regix then
		return 1
	fi
when a_imm then
	return 0
else
CPL OPNDNAMES[AX^.MODE]
	gerror("getlregs")
esac
return 0
end

global function isintconst(unit p)int=
!if p^.tag=j_const and tfirstint<=p^.mode<=tlastint then
if p^.tag=j_const and isintcc(p^.mode) then
	return 1
fi
return 0
end

global function _getnextreg(ref opndrec ax,int reg=0)int=
!ax is an operand; if it uses any registers, then return next available register
int maxreg,r,rix

r:=ax^.reg
rix:=ax^.regix

case ax^.mode
when a_reg,a_xreg then
	if r<rframe then
		return r+1	+1
	fi
when a_mem then
	maxreg:=0

	if r>=rframe and rix>rframe then		!rframe, rstack, or pseudo reg
	elsif r>rframe then
		maxreg:=rix
	elsif rix>rframe then
		maxreg:=r
	else
		if r<>rframe then
			maxreg:=max(r,rix)
		else
			maxreg:=rix
		fi
	fi
	if maxreg then
		return maxreg+1
	fi
esac

!no registers used; nevertheless, there may be a particular register to start using,
!this must be provided by reg
if reg=0 then
REG:=R0
!	gerror("GETNEXTREG REG=0")
fi

return reg			!no registers used
end

global function getnextreg(ref opndrec ax,int r=0)int=
!ax is an operand; if it uses any registers, then return next available register
static int maxreg=0
int reg

reg:=_getnextreg(ax,r)

if reg>maxreg then
	maxreg:=reg
fi

return reg
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

function sameoperand(ref opndrec a,b)int=
!check if same memory operand
if a^.mode<>b^.mode then return 0 fi
if a^.size<>b^.size then return 0 fi
if a^.value<>b^.value then return 0 fi
if a^.reg<>b^.reg then return 0 fi
if a^.regix<>b^.regix then return 0 fi
if a^.valtype<>b^.valtype then return 0 fi
if a^.scale<>b^.scale then return 0 fi

!CPL "SAME",STROPND(A),STROPND(B),A.DEF,B.DEF
if a^.def and b^.def and a^.def=b^.def and a^.value=b^.value then
	return 1
elsif a^.def=nil and b^.def=nil and a^.value=b^.value then
	return 1
fi
return 0
end

function findlastmcl:ref mclrec=
!starting from mcindex, find last executable mcl code
!will skip labels, comments, etc
GERROR("FINDLASTMCL")
!n:=imccode^.upb
!for i:=n downto 1 do
!	m:=imccode^[i]
!	if m<>m_comment then
!		return m
!	fi
!od
return nil
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

global proc pushstack(int n)=
int nwords
	if n then
		genmc(m_sub,dstackopnd,genint(n))
		if n iand 8 then
			stackaligned ixor:=1
		fi
	fi
end

global proc pushstackfp(int n)=
int nwords
	if n then
!		genmc(m_sub,dstackopnd,genint(n))
		genmc(m_mov, genreg(r13,8),genint(0))
		to n/8 do
			genmc(m_push,genreg(r13,8))
		od

		if n iand 8 then
			stackaligned ixor:=1
		fi
	fi
end

global proc popstack(int n)=
	if n then
		genmc(m_add,dstackopnd,genint(n))
		if n iand 8 then
			stackaligned ixor:=1
		fi
	fi
end

!global proc ipadstr(ref char s,int width,ref char padchar=" ")=
!int n
!n:=strlen(s)
!to width-n do
!	strcat(s,padchar)
!od
!end

global function definelabel:int =
genmc(m_label,genlabel(++labelno))
return labelno
end

global function createfwdlabel:int =
return ++labelno
end

global proc definefwdlabel(int lab) =
genmc(m_label,genlabel(lab))
end

global proc genjumpl(int lab) =
genmc(m_jmp,genlabel(lab))
end

global proc setalign(int align)=
if align>1 then
	genmc(m_align,genint(align))
fi
end

global function gettypecat(unit a)int=
!return 'I' 'U' or 'F' (or 'P' for pointers). Other types return 0
!rough type info for the code generator

return stdtypecat[ttbasetype[a^.mode]]
end

!function getstructtemp(int size)int=
!!return frame offset of temp area for structs returned from function
!if not stacksetinstr then
!	gerror("gst: frame not used")
!fi
!while size iand 15 do ++size od		!make 16n
!
!framebytes+:=size
!frameoffset+:=size
!
!stacksetinstr^.b^.value:=framebytes
!
!return frameoffset
!end

global proc doblockcall(int size)=
ref opndrec ax

!GERROR("DOBLOCKCALL")
if retbeforeblock then
	gerror("Block call after return")
fi
while size iand 15 do ++size od

if currblocksize=0 then
	currblocksize:=size
	frameoffset-:=size
	framebytes+:=size
!	stacksetinstr^.b^.value:=framebytes
	stacksetinstr^.b^.value:=roundto(framebytes,16)
elsif currblocksize<size then
	frameoffset-:=(size-currblocksize)
	framebytes+:=(size-currblocksize)
	currblocksize:=size
!	stacksetinstr^.b^.value:=framebytes
	stacksetinstr^.b^.value:=roundto(framebytes,16)
fi

ax:=genreg(r9,8)
genmc(m_lea,ax,genindex(areg:rframe,offset:frameoffset))

end

global function getblockreg(int size)ref opndrec=
!assume same size set by doblockcall
ref opndrec ax

while size iand 15 do ++size od
if currblocksize<size then gerror("getblockreg?") fi
genmc(m_lea,ax:=genreg(r0,8),genindex(areg:rframe,offset:frameoffset))
return ax
end

global proc copyretvalue(int size)=
!block return value is used; address is in D0
!copy to caller's pointer at [dframe+structretoffset]

!push d0
!push d0
!sub dstack,32
!mov d10,[dframe-nnn]
!mov d11,d0
!mov d12,size
!call memset*
!add dstack,32
!pop d0
!pop d0
[256]char str


genassem(";-----------")
genassem("	push d0")
genassem("	push d0")
genassem("	sub dstack,32")
!sprintf(&.str,"	mov d10,[dframe%d]",int32(structretoffset))
fprint @&.str,"	mov d10,[dframe#]",structretoffset

genassem(&.str)

genassem("	mov d11,d0")

!sprintf(&.str,"	mov d12,%d",int32(size))
print @&.str,"	mov d12,",size
genassem(&.str)
genassem("	call memcpy*")
genassem("	add dstack,32")

genassem("	pop d0")
genassem("	pop d0")
genassem(";-----------")

end

global proc enterproc(ichar name)=
RETURN
	if eqstring(name,"$showentry") or eqstring(name,"$showreturn") then
		return
	fi

	genmc(m_sub,genreg(rstack,8),genint(32))
	genmc(m_mov,genreg(r10,8),genstrimm(name))
	genmc(m_call, genname("$showentry*"))
	genmc(m_add,genreg(rstack,8),genint(32))
end

global proc leaveproc(ichar name)=
RETURN
	if eqstring(name,"$showentry") or eqstring(name,"$showreturn") then
		return
	fi
	genmc(m_sub,genreg(rstack,8),genint(32))
	genmc(m_mov,genreg(r10,8),genstrimm(name))
	genmc(m_call, genname("$showreturn*"))
	genmc(m_add,genreg(rstack,8),genint(32))
end
=== cc_blockmcl.m 15/73 ===
!M Compiler - x64 Target Code Generator 2
!import main
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
import cc_libmcl

const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

const tintptr=tullong

[maxnestedloops]int continuestack		!labels for continue/break
[maxnestedloops]int breakstack
int loopindex							!current level of nested loop/switch blocks

const maxparams=200

const maxswitchrange=500
const maxcases=maxswitchrange
const maxswitchdepth=20

ref[]int sw_labeltable			!set from do-switch
ref[]int sw_valuetable
int sw_lower
int sw_ncases					!1..n for serial switch; 0 for simple
byte sw_defaultseen				!starts at 0, set to 1 when default: seen
int sw_defaultlabel
int sw_breaklabel

int maxreg=0

global proc do_stmt(unit p) =
int oldclineno,value,i
unit a,b
ref opndrec rx,ax,bx,lhs,rhs
ref strec d
[256]char str

if p=nil then
	return
fi

!CPL "STMT",jtagnames[p^.tag],p^.lineno

oldclineno:=clineno
clineno:=p^.fileno<<24+p^.lineno

!SPRINTF(&STR,"%d",p^.lineno)
!GENCOMMENT(&STR)

a:=p^.a
b:=p^.b

!gentc(t_stmt)

switch p^.tag
when j_block then
	while a do
		do_stmt(a)
		a:=a^.nextunit
	od

when j_decl then
	do_decl(p^.def)

when j_callfn then
	dx_call(p,a,b,r0)

when j_return then
	do_return(a)

when j_assign then
	do_assign(a,b)

when j_if then
	do_if(a,b,p^.c)

when j_for then
	do_for(a,b)

when j_while then
	do_while(a,b)
!
when j_dowhile then
	do_dowhile(a,b)
!
when j_goto then
	do_goto(p^.def)

when j_labelstmt then
	do_labeldef(p^.def)
	do_stmt(a)
!
when j_casestmt then

!	sprintf(&.str,"case %d:",int32(p^.index))
	fprint @&.str,"case",p^.index,,":"

	gencomment(pcm_copyheapstring(&.str))
	if sw_ncases=0 then
		genmc(m_label,genlabel(sw_labeltable^[p^.index-sw_lower+1]))
	else
		value:=p^.index
		for i:=1 to sw_ncases do
			if sw_valuetable^[i]=value then
				genmc(m_label,genlabel(sw_labeltable^[i]))
				exit
			fi
		else
			gerror("case: serial switch not found")
		od
	fi
	do_stmt(a)

when j_defaultstmt then
	sw_defaultseen:=1
	gencomment("default:")
	genmc(m_label,genlabel(sw_defaultlabel))
	do_stmt(a)

when j_breaksw then
	genjumpl(sw_breaklabel)

when j_break then
	genjumpl(breakstack[loopindex])

when j_continue then
	genjumpl(continuestack[loopindex])

when j_switch then
	do_switch(p,a,b)
!
when j_addto then
	dx_addto(m_add,a,b,0)

when j_subto then
	dx_addto(m_sub,a,b,0)

when j_multo then
	dx_multo(a,b,0)
!
when j_divto,j_remto then
	dx_divto(p,a,b,0)
!
when j_iandto then
	dx_addto(m_and,a,b,0)

when j_iorto then
	dx_addto(m_or,a,b,0)

when j_ixorto then
	dx_addto(m_xor,a,b,0)

when j_shlto, j_shrto then
	dx_shlto(p,a,b,0)

when j_preincr, j_postincr then
	do_preincr(a,m_add,m_inc)

when j_predecr, j_postdecr then
	do_preincr(a,m_sub,m_dec)


!
!when j_null then
!	gerror("stmt/null")
!
when j_exprlist then
	do_exprlist(a)

else
!assume standalone expression (assign/call/addto/incr done above)

!CPL "LONE",=FSHOWNAMES
	if p^.tag<>j_const or not fshownames then
		loneexpr(p)
	fi

endswitch

!clineno:=oldclineno
end

function dx_expr(unit p, int reg=r0,am=1)ref opndrec =
int oldclineno,value,i,m
unit a,b
ref opndrec rx,ax,bx,lhs,rhs,tx
[256]char str
!ref strbuffer ex
ref strec d

if p=nil then
	return nil
fi

!CPL "EXPR",jtagnames[p^.tag],p^.lineno

if reg>r8 and reg<r10 then
	CPL "DOEXPR TOO MANY REGS?"
fi

oldclineno:=clineno
clineno:=p^.fileno<<24+p^.lineno
tx:=nil
!SPRINTF(&STR,"%d",p^.lineno)
!GENCOMMENT(&STR)

a:=p^.a
b:=p^.b
m:=p^.mode

switch p^.tag
when j_const then
	return dx_const(p,reg)

when j_name then
	return dx_name(p,reg,am)

!when j_nameaddr then
!	d:=p^.def
!	if isstructunion(tttarget[m]) then				!block
!		rx:=genreg(reg,ptrsize)
!		if d^.nameid=paramid then					!pointer to block
!			genmc(m_mov,rx,genmem_u(p))
!		else										!just a block
!			genmc(m_lea,rx,genmem_u(p))
!		fi
!		return rx
!
!	else
!		return dx_nameaddr(p,d,reg)
!	fi
!
when j_widenmem then
	return dx_widen(a,m,reg)

when j_funcname then
	return genmemaddr_u(p)

when j_assign then
	return dx_assign(a,b,reg)
!
when j_andl,j_orl then
	return dx_andorl(p,reg)		!use non-short circuit versions for now

when j_notl then
	return dx_notl(a,reg)

when j_istruel then
	return dx_istruel(a,reg)

when j_exprlist then
	return dx_exprlist(a,reg)

when j_callfn then
	return dx_call(p,a,b,reg)

when j_ifx then
	return dx_ifx(a,b,p^.c,reg)

when j_eq,j_ne,j_lt,j_le,j_ge,j_gt then
	return dx_eq(p,a,b,reg)
!
when j_add then
	if ttisref[a^.mode] and ttsize[b^.mode]<=4 then
		b^.mode:=tintptr
	fi
	return dx_add(a,b,reg)

when j_sub then
	return dx_sub(a,b,reg)

when j_mul then
	return dx_mul(p,a,b,reg)
!
when j_div then
	return dx_div(p,a,b,reg)

when j_rem then
	return dx_rem(p,a,b,reg)
!
when j_iand then
	return dx_iand(m_and,a,b,reg)

when j_ior then
	return dx_iand(m_or,a,b,reg)

when j_ixor then
	return dx_iand(m_xor,a,b,reg)

when j_shl,j_shr then
	return dx_shl(p,a,b,reg)

when j_ptr then
	return dx_ptr(p,a,reg,am)

!when j_ptroffset then
!	tx:=dx_ptroffset(p,a,b,p^.c, 0, reg)

when  j_addptr then
	return dx_addptr(p,a,b, reg, am)

when  j_subptr then
	return dx_subptr(a,b, reg, am)

when j_convert then
	if m=tvoid then
		return evalexpr(a,reg)
	else
		return dx_convert(a,m, p^.opcode, reg)
	fi

when j_scale then
	return dx_scale(p,a,b,reg)

when j_neg then
    return dx_neg(a,reg)

when j_inot then
	return dx_inot(a,reg)
!
when j_preincr, j_predecr then
	return dx_preincrx(p,a,reg)

when j_postincr, j_postdecr then
	return dx_postincrx(p,a,reg)

when j_addto then
	return dx_addto(m_add,a,b,reg)

when j_subto then
	return dx_addto(m_sub,a,b,reg)

when j_multo then
	return dx_multo(a,b,reg)

when j_divto, j_remto then
	return dx_divto(p,a,b,reg)

when j_iandto then
	return dx_addto(m_and,a,b,reg)

when j_iorto then
	return dx_addto(m_or,a,b,reg)

when j_ixorto then
	return dx_addto(m_xor,a,b,reg)

when j_shlto, j_shrto then
	return dx_shlto(p,a,b,reg)
!
when j_sqrt then
	return dx_sqrt(a,reg)

when j_addrof then
	return dx_addrof(p,a,reg,am)

when j_dot then
	return dx_dot(p,a,b,reg,am)

else
	gerror_s("DX-EXPR: can't do tag: %s",jtagnames[p^.tag])
endswitch

if tx=nil then
	GERROR_S("DX-EXPR: NO RESULT: %s",JTAGNAMES[P^.TAG])
fi

clineno:=oldclineno
return tx
end

proc loneexpr(unit p)=
if p and p^.tag<>j_null then
	loadexpr(p)
fi
end

proc do_assign(unit a,b)=
ref opndrec lhs,rhs
int reg,tx

case ttsize[a^.mode]
when 1,2,4,8 then
else
!CPL "BLOCK ASS"
	do_assignblock(a,b)
	return
esac
reg:=r0

!CPL =ISSIMPLE(A)
!CPL =ISSIMPLE(B)

if issimple(a) then
	if issimple(b) then			!simple:=simple
		lhs:=getlvalueopnd(a,reg)
		if isintconst(b) then
			rhs:=evalexpr(b,getnextreg(lhs,reg))
		else
			rhs:=loadexpr(b,getnextreg(lhs,reg),isassign:1)
		fi
	else					!simple:=complex
		rhs:=loadexpr(b,reg,isassign:1)
		lhs:=getlvalueopnd(a,getnextreg(rhs,reg))
	fi
else
	if issimple(b) then			!complex:=simple
		lhs:=getlvalueopnd(a,reg)
		rhs:=loadexpr(b,getnextreg(lhs,reg	),isassign:1)
	else					!complex:=complex
!CPL "ASSIGN CX/CX"
		tx:=saveexpr(b,r0)
		lhs:=getlvalueopnd(a,r0)
!		rhs:=restoreexpr(tx,r0+getaregs(lhs))
		rhs:=restoreexpr(tx,getnextreg(lhs,reg))
	fi
fi
storeopnd(lhs,rhs)
end

function dx_assign(unit a,b,int reg)ref opndrec =
ref opndrec lhs,rhs,rx,ax,bx
int tx

!CPL "DXASSIGN"

case ttsize[a^.mode]
when 1,2,4,8 then
else
	return do_assignblock(a,b,reg)
esac

if issimple(a) then
	if issimple(b) then			!simple:=simple
		lhs:=getlvalueopnd(a,reg)
		rhs:=loadexpr(b,getnextreg(lhs,reg),isassign:1)
		storeopnd(lhs,rhs)		!might be upgraded to m_fstore
	else					!simple:=complex
		rhs:=loadexpr(b,reg,isassign:1)
		lhs:=getlvalueopnd(a,getnextreg(rhs,reg))
		storeopnd(lhs,rhs)
	fi
else
	if issimple(b) then			!complex:=simple
		lhs:=getlvalueopnd(a,reg)
		rhs:=loadexpr(b,getnextreg(lhs,reg),isassign:1)
		storeopnd(lhs,rhs)
	else					!complex:=complex
		tx:=saveexpr(b,r0)
		ax:=getlvalueopnd(a,r0)
!		bx:=restoreexpr(tx,r0+getaregs(ax))
		bx:=restoreexpr(tx,getnextreg(ax,reg))
		storeopnd(ax,bx)
		rhs:=bx
	fi
fi

case rhs^.mode
when a_reg then
	if rhs^.reg<>reg then
		rx:=genreg(reg,rhs^.size)
		genmc(m_mov,rx,rhs)
		return rx
	fi
when a_xreg then
	if rhs^.reg<>reg then
		rx:=genxreg(reg,rhs^.size)
		genmc(m_fmov,rx,rhs)
		return rx
	fi
esac
return rhs
end

function saveexpr(unit a,int reg=r0)int=
!for now, just push to stack, and return size
pushexpr(a,reg)
return ttsize[a^.mode]
end

function fsaveexpr(unit a,int reg=xr0)int=
!for now, just push to stack, and return size
fpushexpr(a,reg)
return ttsize[a^.mode]
end

function restoreexpr(int tx, reg)ref opndrec=
!for now, tx is just the size
ref opndrec rx

genmc(m_pop,rx:=genreg(reg,targetsize))		!register must be full size
rx:=duplopnd(rx)
rx^.size:=tx
return rx
end

function frestoreexpr(int tx, reg)ref opndrec=
!for now, tx is just the size
ref opndrec rx

genmc(m_pop,rx:=genxreg(reg,tx))		!register must be full size
rx:=duplopnd(rx)
rx^.size:=tx
return rx
end

function getlvalueopnd (unit a,int reg=r0)ref opndrec=
return evalexpr(a,reg)
end

proc storeopnd(ref opndrec ax,bx)=
!ax is a memory operand, bx is a register operand
!store bx to ax
!note that bx miht refer to an xmm register

if bx^.mode=a_xreg then
	genmc(m_fmov,ax,bx)
else
	case ax^.size
	when 1,2,4 then
		bx:=changeopndsize(bx,ax^.size)
	esac
	genmc(m_mov,ax,bx)
fi
end

proc pushexpr(unit a,int reg=r0)=
ref opndrec ax

if a then
	ax:=dx_expr(a,reg)
	genmc(m_push,ax)
else
	genmc(m_push,zero_opnd)
fi
end

proc fpushexpr(unit a,int reg=xr0)=
ref opndrec ax

ax:=dx_expr(a,reg)
genmc(m_push,ax)
end

function dx_const(unit p, int reg)ref opndrec=
ref opndrec ax
int t

if (t:=ttbasetype[p^.mode])>=tfirstint and t<=tlastint then
	return genint(p^.value,ttsize[p^.mode])

elsif t>=tfirstreal and t<=tlastreal then
	ax:=genreal(p^.xvalue,ttsize[p^.mode])
	return ax

elsif t=tref then
	if p^.isstrconst then
		return genstrimm(p^.svalue,p^.slength)
	elsif p^.iswstrconst then
		return genwstrimm(p^.wsvalue,p^.wslength)

	fi
	return genint(p^.value,ptrsize)
fi
gerror_s("dxconst %s",Strmode(p^.mode))

return ax
end

function dx_constant(ref strec d, int reg)ref opndrec=
ref opndrec ax
int t

if (t:=ttbasetype[d^.mode])>=tfirstint and t<=tlastint then
	return genint(d^.code^.value,ttsize[d^.mode])

elsif t>=tfirstreal and t<=tlastreal then
	ax:=genreal(d^.code^.xvalue,ttsize[d^.mode])
	return ax
fi
gerror_s("dxconstant %s",Strmode(d^.mode))

return ax
end

proc do_labeldef(ref strec d)=
genmc(m_label,genlabel(d^.index),genmemaddr_d(d))
end

proc do_goto(ref strec d)=
if d^.index=0 then
	gerror_s("Label not defined: %s",d^.name)
fi
genmc(m_jmp,genlabel(d^.index))
end

function dx_add(unit a,b,int reg)ref opndrec=
ref opndrec ax,bx
int tx

if gettypecat(a)='R' then
	return dx_fadd(m_add,a,b,reg)
fi

!GOTO SIMPLEADD

if issimple(b) then
SIMPLEADD::
!CPL "ADD SX/SX"
	ax:=loadexpr(a,reg)
!	bx:=evalexpr(b,reg+getlregs(ax))
	bx:=evalexpr(b,getnextreg(ax,reg))

elsif issimple(a) then	!reverse order

	ax:=loadexpr(b,reg)
!	bx:=evalexpr(a,reg+getlregs(ax))
	bx:=evalexpr(a,getnextreg(ax,reg))
else
!	tx:=saveexpr(b,reg)
!	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,getnextreg(ax))
	tx:=saveexpr(a,reg)
	ax:=loadexpr(b,reg)
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi

genmc(m_add,ax,bx)

return ax
end

function dx_fadd(int opc,unit a,b,int reg)ref opndrec=
ref opndrec ax,bx
int tx

if issimple(b) then
	ax:=floadexpr(a,reg)
	if opc=m_imul and a^.tag=j_name and b^.tag=j_name and a^.def=b^.def then
!CPL "FLOAT/SQR DETECTED"
		bx:=ax
	else
!		bx:=fevalexpr(b,reg+getlregs(ax))
		bx:=fevalexpr(b,getnextreg(ax,reg))
	fi
elsif issimple(a) and (opc=m_add or opc=m_imul) then	!reverse order
	ax:=floadexpr(b,reg)
!	bx:=fevalexpr(a,reg+getlregs(ax))
	bx:=fevalexpr(a,getnextreg(ax,reg))
else
	tx:=fsaveexpr(b,reg)
	ax:=floadexpr(a,reg)
!	bx:=frestoreexpr(tx,reg+getlregs(ax))
	bx:=frestoreexpr(tx,getnextreg(ax,reg))
fi

case opc
when m_add then
	genmc(m_fadd,ax,bx)
when m_sub then
	genmc(m_fsub,ax,bx)
when m_imul then
	genmc(m_fmul,ax,bx)
when m_idiv then
	genmc(m_fdiv,ax,bx)
esac


return ax
end

function loadexpr(unit a,int reg=r0,isassign=0)ref opndrec=
ref opndrec ax,rx

ax:=dx_expr(a,reg,1)

case ax^.size
when 1,2,4,8 then
else				!block
	case ax^.mode
	when a_mem then
		if ax^.def=nil and ax^.regix=0 and ax^.value=0 then		!assume [reg]
			return genreg(ax^.reg,ptrsize)						!return block reg
		fi
		rx:=genreg(reg+getaregs(ax),ptrsize)
!		rx:=genreg(getnextreg(ax),ptrsize)
		genmc(m_lea, rx, ax)
		return rx

	when a_reg then
		return ax
	when a_imm then
	else
		gerror("loadexpr block not mem/reg")
	esac
esac

case ax^.mode
when a_reg then			!already in register
	if ax^.reg=reg then
		return ax
	fi
	rx:=genreg(reg,ax^.size)
	genmc(m_mov,rx,ax)
	return rx
when a_xreg then
	if isassign and ax^.reg=reg then
		return ax
	fi
	rx:=genreg(reg,ax^.size)
	genmc(m_fmov,rx,ax)
	return rx
esac

rx:=genreg(reg,ax^.size)

genmc(m_mov,rx,ax)
return rx
end

function evalexpr(unit p,int reg=r0)ref opndrec=
!evaluate expression in junit  p
!return filled-in operand which can be used to access the result.
!Result is not loaded into a register unless needed.
!Any registers used will always start from R0 and XR0
ref opndrec ax,rx

ax:=dx_expr(p,reg,1)

if ax^.mode=a_xreg then
	rx:=genreg(ax^.reg,ax^.size)
	genmc(m_fmov,rx,ax)
	return rx
fi
return ax
end

function evaladdr(unit p,int reg=r0)ref opndrec=
return dx_expr(p,reg,0)
end

function evalptr(unit p,int reg=r0)ref opndrec=
return dx_expr(p,reg,2)
end

function floadexpr(unit a,int xreg=r0)ref opndrec=
ref opndrec ax,rx

ax:=dx_expr(a,xreg)
rx:=genxreg(xreg,getopndsize_u(a))

case ax^.mode
when a_xreg then				!already in register
	if ax^.reg=xreg then
		return ax
	fi
	genmc(m_fmov,rx,ax)
	return rx
when a_reg then
	rx:=genxreg(ax^.reg,ax^.size)
	genmc(m_fmov,rx,ax)
	return rx
!	GERROR("FLOADEXPR REG")
esac

genmc(m_fmov,rx,ax)
return rx
end

function fevalexpr(unit p,int xreg=xr0)ref opndrec=
ref opndrec ax,rx

ax:=dx_expr(p,xreg)
if ax^.mode=a_reg then
	rx:=genxreg(xreg,getopndsize_u(p))
	genmc(m_fmov,rx,ax)
	return rx
fi
return ax
end

proc do_if(unit a,b,c)=
int lab1,lab2

lab1:=createfwdlabel()

genjumpcond(kjumpf,a,lab1,r0)

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

proc genjumpcond(int opc,unit p,int lab,reg=r0)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
unit q,r
int lab2
ref opndrec ax

q:=p^.a
r:=p^.b

switch p^.tag
when j_andl then
	case opc
	when kjumpf then
		genjumpcond(kjumpf,q,lab,reg)
		genjumpcond(kjumpf,r,lab,reg)
	when kjumpt then
		lab2:=createfwdlabel()
		genjumpcond(kjumpf,q,lab2,reg)
		genjumpcond(kjumpt,r,lab,reg)
		definefwdlabel(lab2)
	esac

when j_orl then
	case opc
	when kjumpf then
		lab2:=createfwdlabel()
		genjumpcond(kjumpt,q,lab2,reg)
		genjumpcond(kjumpf,r,lab,reg)
		definefwdlabel(lab2)
	when kjumpt then
		genjumpcond(kjumpt,q,lab,reg)
		genjumpcond(kjumpt,r,lab,reg)
	esac

when j_notl then
	case opc
	when kjumpf then
		genjumpcond(kjumpt,q,lab,reg)
	when kjumpt then
		genjumpcond(kjumpf,q,lab,reg)
	esac

when j_istruel then
	genjumpcond(opc,q,lab,reg)

when j_eq,j_ne,j_lt,j_le,j_ge,j_gt then

	gcomparejump(opc,p,q,r,lab,reg)

when j_exprlist then
	while q and (r:=q^.nextunit) do
		dx_expr(q,reg)
		q:=r
	od

	genjumpcond(opc,q,lab,reg)
else			!other expression
	case p^.tag
	when j_preincr then			!this already sets the flag
		do_preincr(p^.a,m_add,m_inc)
	when j_predecr then			!this already sets the flag
		do_preincr(p^.a,m_sub,m_dec)
	else
		ax:=loadexpr(p,reg)
		genmc(m_cmp,ax,genint(0))
	esac

	genmc_cond(m_jmpcc, (opc|ne_cond|eq_cond),genlabel(lab))
endswitch
end

proc gcomparejump(int jumpopc,unit p,lhs,rhs,int lab,reg)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
ref opndrec ax,bx
int mclcond,cond,tx

cond:=p^.tag				!eqop,neop, etc

if jumpopc=kjumpf then			!need to reverse condition
	cond:=reversecond(cond)		!eqop => neop, etc
fi

mclcond:=getmclcond(cond,lhs^.mode)

case mclcond
when flt_cond,fge_cond,fle_cond,fgt_cond,feq_cond, fne_cond then

	if issimple(rhs) then
		ax:=floadexpr(lhs,reg)
!		bx:=fevalexpr(rhs,reg+getlregs(ax))
		bx:=fevalexpr(rhs,getnextreg(ax,reg))
	else
		tx:=saveexpr(rhs,reg)
		ax:=floadexpr(lhs,reg)
!		bx:=frestoreexpr(tx,reg+getlregs(ax))
		bx:=frestoreexpr(tx,getnextreg(ax,reg))
	fi
	genmc(m_fcmp,ax,bx)

else
	if issimple(rhs) then
		ax:=loadexpr(lhs,reg)
!		bx:=evalexpr(rhs,reg+getlregs(ax))
		bx:=evalexpr(rhs,getnextreg(ax,reg))
		genmc(m_cmp,ax,bx)

	elsif issimple(lhs) then			!reverse test, but also modify condition
		mclcond:=reversemclcond(mclcond)
		ax:=loadexpr(rhs,reg)
!		bx:=evalexpr(lhs,reg+getlregs(ax))
		bx:=evalexpr(lhs,getnextreg(ax,reg))
		genmc(m_cmp,ax,bx)

	else
		pushexpr(rhs,reg)
		ax:=loadexpr(lhs,reg)
!		bx:=genreg(reg+getlregs(ax),ax^.size)
		bx:=genreg(getnextreg(ax,reg),ax^.size)
		genmc(m_pop,changeopndsize(bx,targetsize))
		genmc(m_cmp,ax,bx)
	fi
esac

genmc_cond(m_jmpcc, mclcond,genlabel(lab))
END

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

proc do_preincr(unit a,int addop,incrop)=
ref opndrec ptropnd
int size

ptropnd:=getlvalueopnd(a)

if ttbasetype[a^.mode]=tref and ttsize[tttarget[a^.mode]]<>1 then
	size:=ttsize[tttarget[a^.mode]]
	genmc(addop,ptropnd,genint(size))
else
	genmc(incrop,ptropnd)
fi
end

function reversemclcond(int cond)int=
!reverse order due to operands being switched, so that <= becomes >=
!it is NOT the opposite condition (eg. <= becomes > or <> becomes =)

switch cond
when lt_cond then cond:=gt_cond		!needs symmetric reversal: < >
when le_cond then cond:=ge_cond		!<= >=
when ge_cond then cond:=le_cond		!>= <=
when gt_cond then cond:=lt_cond		!> <
when ltu_cond then cond:=gtu_cond		!needs symmetric reversal: < >
when leu_cond then cond:=geu_cond		!<= >=
when geu_cond then cond:=leu_cond		!>= <=
when gtu_cond then cond:=ltu_cond		!> <
when flt_cond then cond:=fgt_cond		!needs symmetric reversal: < >
when fle_cond then cond:=fge_cond		!<= >=
when fge_cond then cond:=fle_cond		!>= <=
when fgt_cond then cond:=flt_cond		!> <
endswitch
return cond
end

proc do_while (unit pcond, pbody) =
int lab_b,lab_c,lab_d

if pcond^.tag=j_const and pcond^.value then
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

genjumpcond(kjumpt,pcond,lab_b)
definefwdlabel(lab_d)
--loopindex
end

proc do_while1 (unit pbody) =
int lab_b,lab_c,lab_d

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

proc stacklooplabels(int a,b)=
!don't check for loop depth as that has been done during parsing
continuestack[++loopindex]:=a
breakstack[loopindex]:=b
end

proc do_dowhile (unit pbody, pcond) =
int lab_b,lab_c,lab_d

lab_c:=createfwdlabel()
lab_d:=createfwdlabel()

stacklooplabels(lab_c, lab_d)

lab_b:=definelabel()

do_stmt(pbody)

definefwdlabel(lab_c)

genjumpcond(kjumpt,pcond,lab_b)
definefwdlabel(lab_d)
--loopindex
end

proc do_for (unit pinit, pbody) =
unit pcond,pincr
int lab_b,lab_c,lab_d,lab_cond

pcond:=pinit^.nextunit
pincr:=pcond^.nextunit

lab_c:=createfwdlabel()
lab_d:=createfwdlabel()
lab_cond:=createfwdlabel()

if pinit^.tag<>j_null then
	do_stmt(pinit)
fi

genjumpl(lab_cond)		!direct to condition code which is at the end

stacklooplabels(lab_c, lab_d)

lab_b:=definelabel()

do_stmt(pbody)

definefwdlabel(lab_c)

do_stmt(pincr)
definefwdlabel(lab_cond)

if pcond^.tag<>j_null then
	genjumpcond(kjumpt,pcond,lab_b)
else
	genjumpl(lab_b)
fi
definefwdlabel(lab_d)
--loopindex
end

function pushffparams(unit p,int variadic=0)int=
!p is a list of units representing params for a foreign function
!return number params allocated on stack
!return 1 if extra 8 bytes pushed for alignment, otherwise 0
ref opndrec rx,fx
[4]byte iscomplex
int i,m,n,dummypush,size,popbytes,ncomplex
unit q
[maxparams]unit paramlist


n:=0
while p do
	if n>=maxparams then gerror("TOO MANY PARAMS") fi
	++n
	paramlist[n]:=p
	p:=p^.nextunit
od

m:=max(n,4)			!number of params on stack (always at least 4 including shadow space)
dummypush:=0

if (m iand 1) and stackaligned or (m iand 1)=0 and not stackaligned then
	dummypush:=1
	if n>4 then
		pushstack(8)
	fi
	popbytes:=(m+1)*8
else
	popbytes:=m*8
fi

for i:=n downto 5 do
	q:=paramlist[i]
	if variadic and ttbasetype[q^.mode]=tfloat then		!need to convert to double
		pushfloatparam(q)
	else
		pushexpr(q)
	fi
od

if dummypush and n<=4 then
	pushstack(40)
else
	pushstack(32)
fi

n:=min(n,4)

if n=1 then			!simple param, just load direct
	pushoneparam(paramlist[i],variadic)
	return popbytes
fi

ncomplex:=0

!first pass, push complex params

!HAVE REVERSED ORDER OF PARAM EVALUATION: RIGHT TO LEFT FOR BOTTOM 4,
!FOR FIRST PASS, THEN LEFT TO RIGHT. Otherwise if doing param 4 first
!into r13, then can be overridden. (Perhaps need another scratch register)

!for i:=1 to n do
for i:=n downto 1 do
	q:=paramlist[i]
	if issimplepm(q) then
		iscomplex[i]:=0
	else
		pushexpr(q)
		iscomplex[i]:=1
		++ncomplex
	fi
od

!second pass, pop any complex params

!(NOTE: can be optimised when all only one complex param)
!for i:=n downto 1 do
for i:=1 to n do
	q:=paramlist[i]
	if gettypecat(q)<>'R' then
		if iscomplex[i] then
			genmc(m_pop,rx:=genreg(r10+i-1,targetsize))		!register must be full size
		else
			rx:=loadexpr(q,r10+i-1)
		fi
	else
		if variadic and ttbasetype[q^.mode]=tfloat then		!need to convert to double
			loadfloatparam(q,i-1,iscomplex[i])
		else
			size:=ttsize[q^.mode]
			if iscomplex[i] then
				genmc(m_pop,rx:=genreg(r13,targetsize))		!register must be full size
				genmc(m_fmov,fx:=genxreg(xr0+i-1,size),changeopndsize(rx,size))
			else
				fx:=floadexpr(q,xr0+i-1)
			fi

			if variadic then
				genmc(m_fmov,genreg(r10+i-1,size),fx)
			fi
		fi
	fi
od

return popbytes
end

proc pushoneparam(unit q, int variadic)=
ref opndrec fx,ax,bx

	if gettypecat(q)<>'R' then
		if issimple(q) then
			loadexpr(q,r10)
		else
			ax:=loadexpr(q,r0)
			bx:=genreg(r10,ax^.size)
			genmc(m_mov, bx,ax)
		fi
	else
		fx:=floadexpr(q,xr0)
		if variadic then
			genmc(m_fmov,genreg(r10,ttsize[q^.mode]),fx)
		fi
	fi
end

proc pushfloatparam(unit q)=
!for loading real32 to stack for variadic functions
ref opndrec ax,bx,fx

!GENCOMMENT("PUSHFLOATPARAM")

!fx:=floadexpr(q,xr15)
fx:=floadexpr(q,xr13)
genmc(m_fwiden,ax:=changeopndsize(fx,8),fx)
genmc(m_fmov,bx:=genreg(r13,8),ax)
genmc(m_push,bx)
end

proc loadfloatparam(unit q,int regoffset,iscomplex)=
!for loading real32 to regs for variadic functions
!q points to the real32 expr
!regoffset is 0..3; value needs loading in xmm0..3 and r10..r13
!iscomplex means the value exists, as real32, on the stack

ref opndrec ax,ax32,fx,fx32

fx:=genxreg(xr0+regoffset,8)
fx32:=genxreg(xr0+regoffset,4)
ax:=genreg(r10+regoffset,8)
ax32:=genreg(r10+regoffset,4)

if iscomplex then
	genmc(m_pop,ax)
	genmc(m_fmov,fx32,ax32)
	genmc(m_fwiden,fx,fx32)
	genmc(m_fmov,ax,fx)
else
	floadexpr(q,xr0+regoffset)
	genmc(m_fwiden,fx,fx32)
	genmc(m_fmov,ax,fx)
fi
end

function dx_call(unit p,a,b,int reg)ref opndrec=
ref opndrec result,cx,sx
ref paramrec pm
int isfnptr,variadic,nparams,retmode,nbytes,retsize,m,nregparams

retmode:=p^.mode
if retmode=tvoid then retmode:=tsint fi

case a^.tag
when j_ptr then
!CPL "DXCALL",STRMODE(A^.MODE)
	m:=a^.mode
	while ttbasetype[m]=tref do
		m:=tttarget[m]
	od

	pm:=ttparams[m]
	isfnptr:=1

else
	pm:=a^.def^.paramlist
	isfnptr:=0

esac

variadic:=pm^.flags=pm_variadic
nparams:=pm^.nparams

nbytes:=pushffparams(b,variadic)

retsize:=ttsize[retmode]
if retsize>8 then
	doblockcall(retsize)
fi

if not isfnptr then
	genmc(m_call,genmemaddr_u(a))
else
	if issimple(a) then
		genmc(m_call,changeopndsize(loadexpr(a^.a),ptrsize))
	else
		nregparams:=min(nparams,4)
		sx:=genireg(rstack,8)
		for i:=1 to nregparams do
			genmc(m_mov, applyoffset(sx,(i-1)*8),genreg(r10+i-1,8))
		od
		cx:=changeopndsize(loadexpr(a^.a),ptrsize)
		sx:=genireg(rstack,8)
		for i:=1 to nregparams do
			genmc(m_mov, genreg(r10+i-1,8),applyoffset(sx,(i-1)*8))
		od
		genmc(m_call,cx)
	fi

fi

popstack(nbytes)

if gettypecat(p)='R' then
	result:=genxreg(reg,retsize)
elsif retsize<=8 then
	result:=genreg(reg,retsize)
else
	result:=getblockreg(retsize)
fi
!IF TTSIZE[RETMODE]>8 THEN
!	CPL "CALL RETURNS BLOCK"
!FI

return result
end

proc do_return(unit a)=

leaveproc(currproc^.name)

if a then
	if isrealcc(a^.mode) then
		floadexpr(a)
	else
		loadexpr(a)

		if structretoffset then
			copyretvalue(ttsize[a^.mode])
		fi
	fi
fi
genreturn(framebytes,parambytes)
end

function dx_sub(unit a,b,int reg)ref opndrec=
ref opndrec ax,bx
int tx,doneg

if gettypecat(a)='R' then
	return dx_fadd(m_sub,a,b,reg)
fi
doneg:=0

!GOTO SIMPLESUB

if issimple(b) then
SIMPLESUB::
	ax:=loadexpr(a,reg)
!	bx:=evalexpr(b,reg+getlregs(ax))
	bx:=evalexpr(b,getnextreg(ax,reg))

elsif issimple(a) then		!reverse order, add neg at the end
	ax:=loadexpr(b,reg)
!	bx:=evalexpr(a,reg+getlregs(ax))
	bx:=evalexpr(a,getnextreg(ax,reg))
	doneg:=1
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,reg+getlregs(ax))
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi
genmc(m_sub,ax,bx)
if doneg then
	genmc(m_neg,ax)
fi

return ax
end

function dx_mul(unit p,a,b,int reg)ref opndrec=
ref opndrec ax,bx
int64 x
int n,opc,tx

case gettypecat(a)
when 'R' then
	return dx_fadd(m_imul,a,b,reg)
esac

!if b^.tag=j_const and tfirstint<=ttbasetype[b^.mode]<=tlastint then
if b^.tag=j_const and isintcc(b^.mode) then
	x:=b^.value
	if n:=ispoweroftwo(x) then
		p^.tag:=j_shl
		b^.value:=n
		return dx_shl(p,a,b,reg)
	fi
fi

if issimple(b) then

!should do special processing when a==b, but here only check for simple case of x*x
	ax:=loadexpr(a,reg)
	if a^.tag=j_name and b^.tag=j_name and a^.def=b^.def then
		bx:=ax
	else
!		bx:=evalexpr(b,reg+getlregs(ax))
		bx:=evalexpr(b,getnextreg(ax,reg))
	fi
elsif issimple(a) then	!reverse order
	ax:=loadexpr(b,reg)
!	bx:=evalexpr(a,reg+getlregs(ax))
	bx:=evalexpr(a,getnextreg(ax,reg))
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,reg+getlregs(ax))
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi

genmc(m_imul,ax,bx)

return ax
end

function dx_div(unit p,a,b,int reg)ref opndrec=
ref opndrec ax,bx
int opc,n,tx

case gettypecat(a)
when 'R' then
	return dx_fadd(m_idiv,a,b,reg)

when 'I' then
	opc:=m_idiv
else
	opc:=m_udiv
esac

!if b^.tag=j_const and tfirstint<=ttbasetype[b^.mode]<=tlastint then
if b^.tag=j_const and isintcc(b^.mode) then
	if n:=ispoweroftwo(b^.value) then
		p^.tag:=j_shr
		b^.value:=n
		return dx_shl(p,a,b,reg)
	fi
fi

if reg<>r0 then
	gerror("DIV REG NOT ZERO")
fi

if issimple(b) then
	ax:=loadexpr(a,reg)
	if b^.tag=j_const then
!		bx:=loadexpr(b,reg+getlregs(ax))
		bx:=loadexpr(b,getnextreg(ax,reg))
	else
!		bx:=evalexpr(b,reg+getlregs(ax))
		bx:=evalexpr(b,getnextreg(ax,reg))
	fi
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,reg+getlregs(ax))
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi

genmc(opc,bx)

return ax
end


function dx_shl(unit p,a,b,int reg)ref opndrec=
!shl/shr
ref opndrec ax,bx
int opc,tx

if p^.tag=j_shl then
	opc:=m_shl
else
	case gettypecat(p)
	when 'I' then
		opc:=m_ishr
	else
		opc:=m_ushr
	esac
fi

if issimple(b) and reg<>r10 then
	ax:=loadexpr(a,reg)
	if b^.tag=j_const then
		bx:=evalexpr(b,r10)
	else
		bx:=loadexpr(b,r10)
	fi
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,reg+getlregs(ax))
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi

genmc(opc,ax,bx)

return ax
end

function dx_iand(int opc,unit a,b,int reg)ref opndrec=
!iand/ior/ixor
!I think also and/or
ref opndrec ax,bx
int tx

if issimple(b) then
	ax:=loadexpr(a,reg)
!	bx:=evalexpr(b,reg+getlregs(ax))
	bx:=evalexpr(b,getnextreg(ax,reg))

elsif issimple(a) then	!reverse order
	ax:=loadexpr(b,reg)
!	bx:=evalexpr(a,reg+getlregs(ax))
	bx:=evalexpr(a,getnextreg(ax,reg))

else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,reg+getlregs(ax))
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi
genmc(opc,ax,bx)

return ax
end

function dx_preincrx(unit p,a,int reg)ref opndrec=
int opc,size
ref opndrec ptropnd, result

ptropnd:=getlvalueopnd(a,reg)

opc:=m_mov

result:=genreg(getnextreg(ptropnd,reg),getopndsize_u(p))
if result^.size>ptropnd^.size then
GERROR("PREINCRX WIDENING NEEDED")
!	opc:=getwidencode(a^.mode,p^.mode)
fi

genmc(opc,result,ptropnd)

!if p^.a^.mode.basetypeno=t_ref and p^.a^.mode.target.size<>1 then
if ttbasetype[a^.mode]=tref and ttsize[tttarget[a^.mode]]<>1 then
	size:=ttsize[tttarget[a^.mode]]
	if p^.tag= j_preincr then
		opc:=m_add
	else
		opc:=m_sub
	fi
	genmc(opc,result,genint(size))
else
	genmc((p^.tag=j_preincr|m_inc|m_dec),result)
fi
storeopnd(ptropnd,result)

return result
end

function dx_postincrx(unit p,a,int reg)ref opndrec=
int opc,size,convop
ref opndrec ptropnd, result, rr1

result:=genreg(reg,getopndsize_u(p))
ptropnd:=getlvalueopnd(a,reg+1)

opc:=m_mov

if result^.size>ptropnd^.size then
GERROR("POSTINCRX WIDENING NEEDED")
!	convop:=conversionops(ttbasetype[a^.mode],ttbasetype[p^.mode])
!	if 
!	opc:=getwidencode(a^.mode,p^.mode)
fi

!genmc(opc,rr1:=genreg(reg+1+getaregs(ptropnd),getopndsize_u(p)),ptropnd)
genmc(opc,rr1:=genreg(getnextreg(ptropnd,reg)+1,getopndsize_u(p)),ptropnd)
genmc(m_mov,result,rr1)

!if p^.a^.mode.basetypeno=t_ref and p^.a^.mode.target.size<>1 then
!	size:=p^.a^.mode.target.size
if ttbasetype[a^.mode]=tref and ttsize[tttarget[a^.mode]]<>1 then
	size:=ttsize[tttarget[a^.mode]]
	if p^.tag= j_postincr then
		opc:=m_add
	else
		opc:=m_sub
	fi
	genmc(opc,rr1,genint(size))
else
	genmc((p^.tag=j_postincr|m_inc|m_dec),rr1)
fi
storeopnd(ptropnd,rr1)
return result
end

function makeindexopnd(unit a,index=nil, int scale=1,size,offset=0,reg=r0)ref opndrec=
! General routine to deal with pointers, pointers+offsets, records+offsets,
! array access with indices+offsets
! a, index are units
! index is optional, so can be nil
! index can also be a const unit, in which case it's added onto the fixed offset
! scale is the size of the object being fetched, used as scale factor for pointer+offset
!  or for index ops
! offset is a fixed byte-offset (0 if not needed)
! For extra offsets associated with array lower-bound adjustments, the caller adds
! the necessary byte-offset into offset
! Returns a memory access operand, but does not load the value
! If the address is of interest, the caller uses LEA on the result
! to load the results, a load operation (eg. m_mov) can be used
int mulfactor, tx, reg2
ref opndrec ax,ix,m
ref strec d

!GENCOMMENT("MAKINDEX")

if index and index^.tag=j_const then
	offset+:=index^.value*scale
	index:=nil
fi
!size:=scale

if index then
	case scale
	when 1,2,4,8 then
		mulfactor:=1
	else								!do the multiply here
		mulfactor:=scale
		scale:=1
	esac
fi

if a^.tag=j_name then
	d:=a^.def
	if d^.nameid=paramid and isstructunion(d^.mode) then
GOTO MX2
	fi

!CPL "HERE6"
	if index then							!name/(simple-complex)
		if mulfactor=1 then					!using scale factor
			ix:=loadexpr(index,reg)
			m:=genindex(ireg:ix^.reg, scale:scale,def:d,offset:offset,size:size)
		else
			loadexpr(index,reg)
			mulreg(reg,mulfactor)
			m:=genindex(ireg:reg, scale:scale,def:d,offset:offset,size:size)
		fi
	else								!name/const
		m:=genindex(def:d,offset:offset,size:size)
	fi
else
mx2::
	if index then
		if issimple(a) then				!simple/(simple-complex)
!CPL "HERE3",MULFACTOR
			if mulfactor=1 then
!GENCOMMENT("HERE3")
!CPL =JTAGNAMES[INDEX^.TAG]
				ix:=loadexpr(index,reg)
				ax:=loadexpr(a,reg+1)
!CPL "HERE4"
!GENCOMMENT("HERE4")
!				ax:=loadexpr(a,getnextreg(ix))
				m:=genindex(areg:ax^.reg, ireg:ix^.reg, scale:scale,offset:offset,size:size)
			else
				loadexpr(index,reg)
				mulreg(reg,mulfactor)
				loadexpr(a,reg+1)
				m:=genindex(areg:reg+1, ireg:reg, scale:scale,offset:offset,size:size)
			fi
		elsif issimple(index) then			!complex/simple
!CPL "HERE4"
			loadexpr(a,reg)
			loadexpr(index,reg+1)
			mulreg(reg+1,mulfactor)
			m:=genindex(areg:reg, ireg:reg+1, scale:scale,offset:offset,size:size)
		else							!complex/complex
!CPL "HERE5"
			tx:=saveexpr(a,reg)
			ix:=loadexpr(index,reg)
			mulreg(reg,mulfactor)
!			ax:=restoreexpr(tx,reg2:=reg+getlregs(ix))
			ax:=restoreexpr(tx,reg2:=getnextreg(ix,reg))
			m:=genindex(areg:reg2, ireg:reg, scale:scale,offset:offset,size:size)
		fi
	else								!(simple-complex)/const
		ax:=loadexpr(a,reg)
		m:=genindex(areg:ax^.reg, scale:scale,offset:offset,size:size)
	fi
fi

return m
end

proc mulreg(int reg,int64 x)=
!multiply value in register by factor x
int n

if x>1 then
	if n:=ispoweroftwo(x) then
		genmc(m_shl,genreg(reg,8),genint(n))
	else
		genmc(m_imul,genreg(reg,8),genint(x))
	fi
fi
end

function dx_ptr(unit p,a,int reg,am=1)ref opndrec=
ref opndrec ax,bx

if am=0 then
	return loadexpr(a,reg)
fi

if ttbasetype[p^.mode]=tproc then gerror("deref/proc") fi

ax:=evalptr(a,reg)
case a^.tag
when j_addptr, j_subptr then
	if ax^.mode<>a_reg then			!ax represents access to actual pointer target
		ax:=applysize(ax,ttsize[p^.mode])
!CPL "DXPTR1",STROPND(AX)
		return ax
	fi

when j_addrof then
	return dx_expr(a^.a,reg,am)
esac

if ax^.mode<>a_reg then
	genmc(m_mov,bx:=genreg(reg,ptrsize),ax)
	ax:=bx
fi
ax:=makeindirect(ax,ttsize[p^.mode])

!CPL "DXPTR3",STROPND(AX)
return ax
end

function dx_addptr(unit p,a,b,int reg,am=1)ref opndrec=
!works out

!addptr differs from add in that b needs to scaled, usually by 2,4,8 but could
!be anything
int size,scale,mulfactor,reg1,tx,offset
ref opndrec m,ax,bx,rx
unit pname

!RETURN GENINT(999)

!CPL "ADDPTR"
!PRINTUNIT(NIL,A); CPL
!PRINTUNIT(NIL,B)

!AX:=EVALEXPR(A,REG)
!AX:=EVALADDR(A,REG)
!CPL =STROPND(AX)

!CPL "ADDPTR",=STRMODE(A^.MODE)
size:=scale:=ttsize[tttarget[a^.mode]]
if p^.ptrscale=0 then scale:=1 fi			!addptr from dot op: offset in bytes

!return genint(12345)

offset:=0
if b^.tag=j_const then
!CPL =SCALE
	b^.value*:=scale
	offset:=b^.value
	scale:=1
fi

case scale
when 2,4,8 then
	mulfactor:=1
else
	mulfactor:=scale
	scale:=1
esac

reg1:=reg+1

!GOTO CXCX

!if b^.tag=j_const then
!	loadexpr(a,reg)
!	m:=genindex(areg:reg, scale:scale,size:size, offset:b^.value)

if b^.tag=j_const then					!any+const
	case a^.tag
	when j_addrof then
		pname:=a^.a
!CPL "ADDROF/NAME"
		if pname^.tag<>j_name then goto other fi
		m:=genindex(def:pname^.def,offset:offset,size:size)
!	when j_addptr, j_subptr then
!		ax:=evalptr(a,reg)
!CPL "ADDPTR/ADDPTR+CONST",=STROPND(AX)
!		if ax^.mode=a_reg then
!			ax:=makeindirect(ax)
!		fi
!		m:=applyoffset(ax,offset,size)
!	when j_const then
	else
other::
		loadexpr(a,reg)
		m:=genindex(areg:reg, scale:scale,size:size, offset:b^.value)
!		goto cxcx
	esac

elsif a^.tag=j_addrof and a^.a^.tag=j_name then		!&a+const
!CPL "&NAME+ANY",MULFACTOR
!IF MULFACTOR>=8 THEN
!GOTO CXCX
!	PRINTUNIT(NIL,A)
!FI
	pname:=a^.a
	bx:=loadexpr(b,reg)				!R
	mulreg(bx^.reg,mulfactor)
	bx:=makeindirect(bx,size)		![R]
	bx^.regix:=bx^.reg
	bx^.reg:=0

	bx^.scale:=scale
	bx^.def:=pname^.def				![R+d]
	if isframe(pname^.def) then
		bx^.reg:=rframe
	fi
	m:=bx

elsif issimple(b) then
!CPL "CX/SX"
	loadexpr(a,reg)
	loadexpr(b,reg1)
	mulreg(reg1,mulfactor)
	m:=genindex(areg:reg, ireg:reg1, scale:scale,size:size)
elsif issimple(a) then
!CPL "SX/CX"
	loadexpr(b,reg)
	mulreg(reg,mulfactor)
	loadexpr(a,reg1)
	m:=genindex(areg:reg1, ireg:reg, scale:scale,size:size)
else
cxcx::
!CPL "CX/CX"

	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
	bx:=restoreexpr(tx,getnextreg(ax,reg))
	mulreg(bx^.reg,mulfactor)
	m:=genindex(areg:reg, ireg:bx^.reg, scale:scale,size:size)
fi

case am
when 1 then
	genmc(m_lea,rx:=genreg(reg,ptrsize),m)
	return rx
esac
return m
end

function dx_subptr(unit a,b,int reg,am)ref opndrec=
int size,scale,mulfactor,reg1,tx,offset
ref opndrec m,ax,bx,rx,ix
ref strec d

!CPL "SUBPTR",=AM

size:=scale:=ttsize[tttarget[a^.mode]]

if ttbasetype[b^.mode]=tref then		!ptr-ptr
	ax:=dx_sub(a,b,reg)
	divreg(ax^.reg,scale)
	return ax
fi

!if am='L' then
!	if scale=1 then
!		return dx_sub(a,b,reg)
!	fi
!	if b^.tag=j_const then
!		b^.value*:=scale
!		return dx_sub(a,b,reg)
!	fi
!fi

offset:=0
if b^.tag=j_const then
	b^.value:=b^.value*scale
	offset:=-b^.value
	scale:=1
fi

mulfactor:=scale
scale:=1

reg1:=reg+1

!CPL "SUBPTR2",=SIZE,=SCALE,=MULFACTOR,=OFFSET

if a^.tag=j_addrof and a^.a^.tag=j_name then
!CPL "SUBPTR3"
	a:=a^.a
fi

!GOTO CXCX

if b^.tag=j_const then
!CPL "SUB/CONST"
!GOTO CXCX
!if b^.tag=j_const then
	loadexpr(a,reg)
	m:=genindex(areg:reg, scale:scale,size:size, offset:offset)
!subptr1::
	if am=2 then
		return m
	fi
	genmc(m_lea,rx:=genreg(reg,ptrsize),m)

	return rx

elsif issimple(b) then
!CPL "SUB/CX/SX"
!GOTO CXCX
	ax:=loadexpr(a,reg)
	bx:=loadexpr(b,reg1)
	mulreg(reg1,mulfactor)
!	m:=genindex(areg:reg, ireg:reg1, scale:scale,size:size)
elsif issimple(a) then
!CPL "SUB/SX/CX"
!GOTO CXCX
	bx:=loadexpr(b,reg)
	mulreg(reg,mulfactor)
	ax:=loadexpr(a,reg1)
!	m:=genindex(areg:reg1, ireg:reg, scale:scale,size:size)
else
CXCX::
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,reg+getlregs(ax))
	bx:=restoreexpr(tx,getnextreg(ax,reg))
	mulreg(bx^.reg,mulfactor)
fi
m:=genindex(areg:reg, ireg:bx^.reg, scale:scale,size:size)

genmc(m_sub,ax,bx)

return ax
end

function dx_convert(unit a, int t,opc, reg)ref opndrec=
!convert unit a to type t, using conversion opc (uwiden_c etc)
ref opndrec ax,rx
int ssize,tsize

ssize:=ttsize[a^.mode]
tsize:=ttsize[t]

case opc
when sfix_c,ufix_c, fnarrow_c,fwiden_c then
	ax:=fevalexpr(a,reg)
else
	ax:=evalexpr(a,reg)			!may need feval for some ops
esac

case opc
when soft_c then
dosoft::
	return ax

when hard_c then

	rx:=genreg(reg,tsize)
	if tsize<ssize then				!narrow
		genmc(m_unarrow,rx,ax)

	elsif tsize>ssize then			!widen
		genmc(m_uwiden,rx,ax)

	else							!same size just different type
		return ax
	fi

when swiden_c, uwiden_c then
if ax^.mode=a_imm then
	ax^.size:=tsize
	return ax
fi
	if ssize=tsize then return ax fi
	rx:=genreg(reg,tsize)
	genmc((opc=swiden_c|m_iwiden|m_uwiden),rx,ax)

when sfloat_c,ufloat_c then
	rx:=genxreg(reg,tsize)	
	genmc((opc=sfloat_c|m_ifloat|m_ufloat),rx,ax)

when sfix_c,ufix_c then
	rx:=genreg(reg,tsize)	
	genmc((opc=sfix_c|m_ifix|m_ufix),rx,ax)

when fnarrow_c,fwiden_c then
	rx:=genxreg(reg,tsize)	
	genmc((opc=fnarrow_c|m_fnarrow|m_fwiden),rx,ax)

when narrow_c,truncate_c then
	rx:=genreg(reg,tsize)	
	genmc(m_unarrow,rx,ax)

else
	gerror_s("Convert op not implem: %s",convnames[opc])
esac
return rx
end

proc do_decl(ref strec d)=
ref opndrec ax
unit a,dest
[256]char str
int nbytes

a:=d^.code

!case ttbasetype[d^.mode]
!when tunion,tstruct then
!	if a^.tag<>j_makelist then
!CPL "HERE1"
!!		gerror("Dynamic struct init")
!	fi
!esac

if a^.tag<>j_makelist then
	if ttbasetype[d^.mode]=tarray and a^.tag=j_const then	!probably string lit
		goto copyl
	fi
	if gettypecat(a)='R' then
		ax:=floadexpr(a,xr0)
		genmc(m_fmov,genmem_d(d),ax)
	elsif a^.tag<>j_const then
		case ttsize[a^.mode]
		when 1,2,4,8 then
			ax:=loadexpr(a,r0)
			genmc(m_mov,genmem_d(d),ax)
		else
			dest:=createname(d)
			dest^.mode:=d^.mode
			do_assignblock(dest,a,r0)
		esac
	else
		genmc(m_mov,genmem_d(d),evalexpr(a))
	fi
	return
fi

copyl::

nbytes:=ttsize[d^.mode]
pushstack(32)

genmc(m_lea, genreg(r10,ptrsize),genmem_d(d))

!sprintf(&.str,"`$%s.%s.%d",currproc^.name,d^.name,int32(d^.blockno))
fprint @&.str,"`$#.#.#",currproc.name,d.name,d.blockno

genmc(m_mov, genreg(r11,ptrsize), genname(&.str))

genmc(m_mov,genreg(r12,ptrsize),genint(nbytes))

genmc(m_call,genname("memcpy*"))

popstack(32)

end

function do_assignblock (unit a,b, int regx=0)ref opndrec =
!special optimised code for blocks; specifically 16-byte ones
!return operand for one the block just copied, or 0 if it could not be done here
ref opndrec ax,bx,rx,rs,rd,rcount,bx2,rsa,rda
int rev,workreg,nwords,lab, regcount,regsource, regdest, offset, n, oddbytes, reg

reg:=(regx|regx|xr0)

rev:=0
if issimple(b) then
!CPL "SSB"
	ax:=getlvalueopnd(a,reg)
!	bx:=getlvalueopnd(b,reg+getaregs(ax))
	bx:=getlvalueopnd(b,getnextreg(ax,reg))

elsif issimple(a) then
!CPL "SSA"
	rev:=1
	ax:=getlvalueopnd(b,reg)
!	bx:=getlvalueopnd(a,reg+getaregs(ax))
	bx:=getlvalueopnd(a,getnextreg(ax,reg))

else
!CPL "CXAB"
	bx:=getlvalueopnd(b,reg)
	if bx^.mode<>a_reg then
		genmc(m_lea,bx2:=genreg(reg,ptrsize),bx)
	else
		bx2:=bx
	fi
	genmc(m_push,bx2)
	ax:=getlvalueopnd(a,reg)

!	genmc(m_pop,genreg(reg:=reg+getaregs(ax),ptrsize))
	genmc(m_pop,bx:=genreg(getnextreg(ax,reg),ptrsize))
	bx:=genireg(bx^.reg)

fi

if ax^.mode=a_reg then ax:=genireg(ax^.reg) fi
if bx^.mode=a_reg then bx:=genireg(bx^.reg) fi

if rev then
	swap(ax,bx)
fi

!ax is the access operand for the dest, and bx for the source

!CPL "BLOCK ASSIGN:",=GETNEXTREG(AX),GETNEXTREG(BX)
!CPL "BLOCK ASSIGN:",=STROPND(AX),=STROPND(BX)

IF GETNEXTREG(AX)>R4 OR GETNEXTREG(BX)>R4 THEN GERROR("ASSIGNBLOCK/REG") fi

workreg:=r4

!n:=ax^.size
n:=ttsize[a^.mode]

oddbytes:=n rem 8		!will be zero, or 1..7
n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
nwords:=n/8			!number of dwords (ie. octobytes)

if 1<=nwords<=4 then		!use unrolled code (no loop)
	offset:=0
	ax:=changeopndsize(ax,targetsize)
	bx:=changeopndsize(bx,targetsize)
	rx:=genreg(workreg,targetsize)
	to nwords do
		genmc(m_mov,rx,applyoffset(bx,offset))

		genmc(m_mov,applyoffset(ax,offset),rx)
		offset+:=8
	od
	rs:=bx
	rd:=ax

elsif nwords<>0 then		!use a loop
	lab:=++labelno
	regcount:=workreg+1
	regsource:=regcount+1
	regdest:=regsource+1
	genmc(m_lea,rsa:=genreg(regsource,ptrsize),bx)
	genmc(m_lea,rda:=genreg(regdest,ptrsize),ax)
	rs:=genireg(regsource,ptrsize)
	rd:=genireg(regdest,ptrsize)
	rx:=genreg(workreg,targetsize)
	rcount:=genreg(regcount,4)

	genmc(m_mov,rcount,genint(nwords))
	genmc(m_label,genlabel(lab))
	genmc(m_mov,rx,rs)
	genmc(m_mov,rd,rx)
	genmc(m_add,rsa,genint(targetsize))
	genmc(m_add,rda,genint(targetsize))
	genmc(m_dec,rcount)
	genmc_cond(m_jmpcc,ne_cond,genlabel(lab))
	offset:=0
else
	rd:=changeopndsize(ax,targetsize)
	rs:=changeopndsize(bx,targetsize)
	offset:=0
fi

if oddbytes then
	n:=oddbytes						!1..7

	if n>=4 then
		rx:=genreg(workreg,4)
		genmc(m_mov,rx,applyoffset(rs,offset))
		genmc(m_mov,applyoffset(rd,offset),rx)
		n-:=4
		offset+:=4
	fi
	if n>=2 then
		rx:=genreg(workreg,2)
		genmc(m_mov,rx,applyoffset(rs,offset))
		genmc(m_mov,applyoffset(rd,offset),rx)
		n-:=2
		offset+:=2
	fi
	if n=1 then
		rx:=genreg(workreg,1)
		genmc(m_mov,rx,applyoffset(rs,offset))
		genmc(m_mov,applyoffset(rd,offset),rx)
	fi
fi
!GENCOMMENT("------DONE BLOCK COPY")

if regx then
	return getlvalueopnd(a,reg)
fi
return genint(0)
end

function dx_widen(unit a, int m, reg)ref opndrec=
ref opndrec ax,bx
int opc

opc:=(gettypecat(a)='I'|m_iwiden|m_uwiden)

ax:=evalexpr(a,reg)

if ttsize[m]=ax^.size then
	return ax
fi
bx:=genreg(reg,ttsize[m])
genmc(opc,bx,ax)

return bx
end

function dx_neg(unit a,int reg)ref opndrec=
ref opndrec ax

if gettypecat(a)='R' then
	return dx_fneg(a,reg)
fi
ax:=loadexpr(a,reg)
genmc(m_neg,ax)
return ax
end

function dx_fneg(unit a,int reg)ref opndrec=
ref opndrec fx

fx:=floadexpr(a,reg)
genmc(m_fneg,fx)
return fx
end

function dx_inot(unit a,int reg)ref opndrec=
ref opndrec ax

ax:=loadexpr(a,reg)
genmc(m_not,ax)
return ax
end

proc do_switch(unit p,a,b)=
!need to create switch levels, as they can be nested; nested case labels
!belong to the top switch level
[maxswitchrange]int labeltable				!sw_length+1 labels
[maxcases]int valuetable					!sw_length+1 labels
[maxswitchrange]byte flags					!flags to check dupl values
int defaultlabel							!index of fwd default label
int breakswlabel							!index of fwd break label
int switchlabel								!index of fwd break label
int lower, upper							!ower/upper ranges of switch case values
int length,value,ncases
byte serialsw
int i,index
!int sw_index
ref caserec pcase
ref opndrec ax,bx

!store current set of global values for outer switch
ref[]int old_labeltable
ref[]int old_valuetable
int old_ncases,old_lower
byte old_defaultseen
int old_defaultlabel
int old_breaklabel

pcase:=p^.nextcase
ncases:=length:=0

while pcase do
	++ncases
	if ncases>maxcases then
		gerror("Too many cases on one switch")
	fi
	valuetable[ncases]:=value:=pcase^.value

	if ncases=1 then
		lower:=upper:=value
	else
		lower:=min(lower,value)
		upper:=max(upper,value)
	fi
	pcase:=pcase^.nextcase
od

if p^.nextcase then
	length:=upper-lower+1
else
	length:=0
fi 

!allocate fwd labels
defaultlabel:=createfwdlabel()		!(when no default:, same as breakswlabel)
breakswlabel:=createfwdlabel()

if length>maxswitchrange then

!NOTES: SERIAL switch needs a way of checking duplicate case values.
!Better if not an n-squared search
!Short length switches should also be done serially (length<=8)
!Then a dupl check is simpler

	serialsw:=1

	ax:=loadexpr(a)
	for i:=1 to ncases do
		labeltable[i]:=createfwdlabel()
		genmc(m_cmp,ax,genint(valuetable[i]))
		genmc_cond(m_jmpcc,eq_cond,genlabel(labeltable[i]))
	od
	genmc(m_jmp,genlabel(defaultlabel))

elsif length=0 then
!GERROR("L=0")
	genmc(m_jmp,genlabel(defaultlabel))

else
	serialsw:=0
	memset(&flags,0,length)				!clear value flags

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
			gerror_s("Dupl case value: %d",cast(value))
		fi
		flags[index]:=1
	od

!need a label for the switchtable itself
	switchlabel:=createfwdlabel()

	ax:=loadexpr(a)
	genmc(m_sub,ax,genint(lower))
	genmc(m_cmp,ax,genint(length))
	genmc_cond(m_jmpcc, geu_cond, genlabel(defaultlabel))

	genmc(m_jmp, genindex(ireg:r0,scale:8,labno:switchlabel))

	setsegment('I',8)
	definefwdlabel(switchlabel)

	for i:=1 to length do
		genmc(m_dq,genlabel(labeltable[i]))
	od
	setsegment('C')
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

function dx_rem(unit p,a,b,int reg)ref opndrec=
ref opndrec ax,bx
int tx,n
int opc

case gettypecat(a)
when 'I' then
	opc:=m_irem
else
	opc:=m_urem
!	if b^.tag=j_const and tfirstint<=ttbasetype[b^.mode]<=tlastint then
	if b^.tag=j_const and isintcc(b^.mode) then
		if n:=ispoweroftwo(b^.value) then
			p^.tag:=j_shr
			b^.value:=b^.value-1
			return dx_iand(m_and,a,b,reg)
		fi
	fi
esac

if reg<>r0 then
	gerror("REM REG NOT ZERO")
fi
if issimple(b) then
	ax:=loadexpr(a,reg)
	if b^.tag=j_const then
!		bx:=loadexpr(b,reg+getlregs(ax))
		bx:=loadexpr(b,getnextreg(ax,reg))
	else
!		bx:=evalexpr(b,reg+getlregs(ax))
		bx:=evalexpr(b,getnextreg(ax,reg))
	fi
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
!	bx:=restoreexpr(tx,reg+getlregs(ax))
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi
genmc(opc,bx)
return ax
end

function dx_ifx(unit a,b,c,int reg)ref opndrec=
ref opndrec bx,result
int lab1, lab2,isreal

lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
lab2:=createfwdlabel()

isreal:=gettypecat(b)='R'

genjumpcond(kjumpf,a,lab1,reg)
if isreal then
	result:=floadexpr(b,reg)
else
	result:=loadexpr(b,reg)
fi

genjumpl(lab2)
definefwdlabel(lab1)

if isreal then
	bx:=floadexpr(c,reg)
else
	bx:=loadexpr(c,reg)
fi

definefwdlabel(lab2)
return result
end

function dx_addto(int opc,unit a,b,int regx=0)ref opndrec=
ref opndrec lhs, rhs, ptropnd,rr2
int reg,tx

if gettypecat(b)='R' then
	return dx_faddto(opc,a,b,regx)
fi

reg:=(regx|regx|xr0)

if issimple(b) then
	ptropnd:=getlvalueopnd(a,reg)
	if b^.tag=j_const and b^.mode=tsint then
!		rhs:=evalexpr(b,reg+getaregs(ptropnd))
		rhs:=evalexpr(b,getnextreg(ptropnd,reg))
	else
!		rhs:=loadexpr(b,reg+getaregs(ptropnd))
		rhs:=loadexpr(b,getnextreg(ptropnd,reg))
	fi
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
elsif issimple(a) then
	rhs:=loadexpr(b,reg)
!	ptropnd:=getlvalueopnd(a,reg+getlregs(rhs))
	ptropnd:=getlvalueopnd(a,getnextreg(rhs,reg))
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
!	rr2:=restoreexpr(tx,reg+getaregs(ptropnd))
	rr2:=restoreexpr(tx,getnextreg(ptropnd,reg))
	rr2:=changeopndsize(rr2,ptropnd^.size)
	genmc(opc,ptropnd,rr2)
fi

return ptropnd

end

function dx_faddto(int opc,unit a,b,int regx=0)ref opndrec=
ref opndrec work, rhs, ptropnd
int reg,tx,tempreg

reg:=(regx|regx|xr0)

if issimple(b) then
	ptropnd:=getlvalueopnd(a,reg)
!	reg+:=getaregs(ptropnd)
	reg:=getnextreg(ptropnd,reg)
	rhs:=fevalexpr(b,reg)
!	reg+:=getlregs(rhs)
	reg:=getnextreg(rhs,reg)

elsif issimple(a) then
	rhs:=fevalexpr(b,reg)
!	reg+:=getlregs(rhs)
	reg:=getnextreg(rhs,reg)
	ptropnd:=getlvalueopnd(a,reg)
!	reg+:=getaregs(ptropnd)
	reg:=getnextreg(ptropnd,reg)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
	reg:=getnextreg(ptropnd,reg)
	rhs:=frestoreexpr(tx,reg)
	++reg
fi

work:=genxreg(reg,getopndsize_u(b))

genmc(m_fmov,work,ptropnd)

case opc
when m_add then opc:=m_fadd
when m_sub then opc:=m_fsub
when m_imul then opc:=m_fmul
when m_idiv then opc:=m_fdiv
esac

genmc(opc,work,rhs)
genmc(m_fmov,ptropnd,work)

return ptropnd
end

function dx_eq(unit p,a,b,int reg)ref opndrec=
!apply =, <= etc between a and b, and get a logical result 1 or 0
ref opndrec ax,bx,rx,rxb
int mclcond,tx

mclcond:=getmclcond(p^.tag,a^.mode)

case mclcond
when feq_cond, fne_cond,flt_cond,fge_cond,fle_cond,fgt_cond then
	if issimple(b) then
		ax:=floadexpr(a,reg)
!		bx:=fevalexpr(b,reg+getlregs(ax))
		bx:=fevalexpr(b,getnextreg(ax,reg))

	elsif issimple(a) then	!reverse order
		mclcond:=reversemclcond(mclcond)
		ax:=floadexpr(b,reg)
!		bx:=fevalexpr(a,reg+getlregs(ax))
		bx:=fevalexpr(a,getnextreg(ax,reg))
	else
		tx:=saveexpr(b,reg)
		ax:=floadexpr(a,reg)
!		bx:=frestoreexpr(tx,reg+getlregs(ax))
		bx:=frestoreexpr(tx,getnextreg(ax,reg))
	fi
	genmc(m_fcmp,ax,bx)
else
	if issimple(b) then
		ax:=loadexpr(a,reg)
!		bx:=evalexpr(b,reg+getlregs(ax))
		bx:=evalexpr(b,getnextreg(ax,reg))
	elsif issimple(a) then			!reverse order
		mclcond:=reversemclcond(mclcond)
		ax:=loadexpr(b,reg)
		bx:=evalexpr(a,reg)
	else
		tx:=saveexpr(b,reg)
		ax:=loadexpr(a,reg)
!		bx:=restoreexpr(tx,reg+getlregs(ax))
		bx:=restoreexpr(tx,getnextreg(ax,reg))
	fi
	genmc(m_cmp,ax,bx)
esac
rx:=genreg(reg,4)
rxb:=genreg(reg,1)

genmc_cond(m_setcc,mclcond,rxb)
genmc(m_uwiden,rx,rxb)
return rx
end

proc do_exprlist(unit a)=

while a do
	do_stmt(a)
	a:=a^.nextunit
od
end

function dx_exprlist(unit a,int reg)ref opndrec=
ref opndrec ax

while a do
	ax:=dx_expr(a,reg)
	a:=a^.nextunit
od
return ax
end

function dx_shlto(unit p,a,b,int regx=0)ref opndrec=
ref opndrec lhs, rhs, ptropnd,rr2
int reg,tx,opc

if p^.tag=j_shlto then
	opc:=m_shl
else
	case gettypecat(p)
	when 'I' then
		opc:=m_ishr
	else
		opc:=m_ushr
	esac
fi

reg:=(regx|regx|xr0)

if issimple(b) then
	ptropnd:=getlvalueopnd(a,reg)
	if b^.tag=j_const and b^.mode=tsint then
!		rhs:=evalexpr(b,reg+getaregs(ptropnd))
		rhs:=evalexpr(b,getnextreg(ptropnd,reg))
	else
!		rhs:=loadexpr(b,reg+getaregs(ptropnd))
		rhs:=loadexpr(b,getnextreg(ptropnd,reg))
	fi
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
elsif issimple(a) then
	rhs:=loadexpr(b,reg)
!	ptropnd:=getlvalueopnd(a,reg+getlregs(rhs))
	ptropnd:=getlvalueopnd(a,getnextreg(rhs,reg))
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
!	rr2:=restoreexpr(tx,reg+getaregs(ptropnd))
	rr2:=restoreexpr(tx,getnextreg(ptropnd,reg))
	rr2:=changeopndsize(rr2,ptropnd^.size)
	genmc(opc,ptropnd,rr2)
fi

return ptropnd

end

function dx_multo(unit a,b,int regx)ref opndrec=
ref opndrec work, rhs, ptropnd
int reg,tx,sgned

case gettypecat(b)
when 'R' then
	return dx_faddto(m_imul,a,b,regx)
when 'I' then
	sgned:=1
else
	sgned:=0
esac

reg:=(regx|regx|r0)

work:=genreg(reg,getopndsize_u(b))
++reg

if issimple(b) then
	ptropnd:=getlvalueopnd(a,reg)
!	reg+:=getaregs(ptropnd)
	reg:=getnextreg(ptropnd,reg)
	rhs:=evalexpr(b,reg)
!	reg+:=getlregs(rhs)
	reg:=getnextreg(rhs,reg)

elsif issimple(a) then
	rhs:=evalexpr(b,reg)
!	reg+:=getlregs(rhs)
	reg:=getnextreg(rhs,reg)
	ptropnd:=getlvalueopnd(a,reg)
!	reg+:=getaregs(ptropnd)
	reg:=getnextreg(ptropnd,reg)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
	reg:=getnextreg(ptropnd,reg)
	rhs:=restoreexpr(tx,reg)
	++reg
fi

loadviaptr(work,ptropnd,sgned)
!genmc(m_mov,work,ptropnd)

genmc(m_imul,work,rhs)

!genmc(m_mov,ptropnd,work)
genmc(m_mov,ptropnd,changeopndsize(work,ptropnd^.size))

return ptropnd
end

function dx_notl(unit a,int reg)ref opndrec=
ref opndrec ax,rx,rxb

if a^.tag=j_notl then
	return dx_istruel(a^.a,reg)
fi

ax:=loadexpr(a,reg)
genmc(m_and,ax,ax)
rx:=genreg(reg,4)
rxb:=genreg(reg,1)
genmc_cond(m_setcc,eq_cond,rxb)
genmc(m_uwiden,rx,rxb)
return rx
end

function dx_istruel(unit a, int reg)ref opndrec=
ref opndrec ax,rx,rxb

ax:=loadexpr(a,reg)
genmc(m_and,ax,ax)
rx:=genreg(reg,4)
rxb:=genreg(reg,1)
genmc_cond(m_setcc,ne_cond,rxb)
genmc(m_uwiden,rx,rxb)
return rx
end

function dx_andorl(unit p, int reg)ref opndrec =
!do short-circuit evaluation of a&&b or a||b
!return operand containing 1 or 0
int lab1,lab2
ref opndrec rx

lab1:=createfwdlabel()			!dest label of main condition (to end of if, or start if else)

genjumpcond(kjumpf,p,lab1)

rx:=genreg(reg,4)

lab2:=createfwdlabel()			!label past else part
genmc(m_mov,rx,genint(1))

genjumpl(lab2)
definefwdlabel(lab1)
genmc(m_mov,rx,genint(0))

definefwdlabel(lab2)
return rx
end

function dx_sqrt(unit a,int reg)ref opndrec=
ref opndrec fx
fx:=floadexpr(a,reg)
genmc(m_fsqrt,fx,fx)
return fx
end

function dx_scale(unit p,a,b, int reg)ref opndrec=
ref opndrec ax,bx,cx
int opc,scale,n

ax:=loadexpr(a,reg)

scale:=p^.scale
opc:=m_imul
if scale<0 then
	scale:=-scale
	opc:=m_idiv
fi

!CPL "SCALE",SCALE

n:=ispoweroftwo(scale)

if n=0 then
	bx:=genint(scale)
	if opc=m_imul then
		genmc(m_imul,ax,bx)
	else
		if ax^.reg<>r0 then
!CPL =SCALE
			gerror("scale/div by non-power-of-two/not r0")
		fi
		genmc(m_mov,cx:=genreg(reg+1,ax^.size),bx)
		genmc(m_idiv,cx)
	fi
else

	bx:=genint(n)

	if opc=m_imul then
		genmc(m_shl,ax,bx)
	else
		genmc(m_ishr,ax,bx)
	fi
fi
return ax
end

function dx_divto(unit p,a,b, int regx)ref opndrec=
ref opndrec work, rhs, ptropnd, ax
int reg,tx,opc,sgned

case gettypecat(b)
when 'R' then
	return dx_faddto(m_idiv,a,b,regx)
when 'I' then
	opc:=(p^.tag=j_divto|m_idiv|m_irem)
	sgned:=1
else
	opc:=(p^.tag=j_divto|m_udiv|m_urem)
	sgned:=0
esac

reg:=(regx|regx|xr0)

if reg<>r0 then
	GERROR("DIVTO: not R0")
fi
work:=genreg(reg,getopndsize_u(b))
++reg

if issimple(b) then
!CPL "A"
	ptropnd:=getlvalueopnd(a,reg)
!	reg+:=getaregs(ptropnd)
	reg:=getnextreg(ptropnd,reg)
	rhs:=evalexpr(b,reg)
!	reg+:=getlregs(rhs)
	reg:=getnextreg(rhs,reg)

elsif issimple(a) then
	rhs:=evalexpr(b,reg)
!	reg+:=getlregs(rhs)
	reg:=getnextreg(rhs,reg)
	ptropnd:=getlvalueopnd(a,reg)
!	reg+:=getaregs(ptropnd)
	reg:=getnextreg(ptropnd,reg)
else
!CPL "C"
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
!	reg+:=getaregs(ptropnd)
	reg:=getnextreg(ptropnd,reg)
	rhs:=restoreexpr(tx,reg)
	++reg
fi

loadviaptr(work,ptropnd,sgned)
genmc(opc,rhs)
genmc(m_mov,ptropnd,changeopndsize(work,ptropnd^.size))

return ptropnd
end

function dx_name(unit p,int reg,am)ref opndrec=
ref opndrec ax,bx

!CPL "DXNAME:",=AM,P^.DEF^.NAME,NAMENAMES[P^.DEF^.NAMEID],P^.DEF^.NAME
case p^.def^.nameid
when procid then
	return genmemaddr_u(p)
!when paramid then
!!CPL "HERE"
!	if isstructunion(p^.mode) then
!		ax:=genreg(reg,ptrsize)
!		genmc(m_mov,ax,genmem_u(p))
!		return genireg(ax^.reg,ttsize[p^.mode])
!	fi
esac

ax:=genmem_u(p)

return ax
end

proc divreg(int reg,int64 x)=
!divide value in register by factor x
ref opndrec rr2
int n

if x>1 then
	if n:=ispoweroftwo(x) then
!		genmc(m_shr,genreg(reg,4),n)
		genmc(m_ishr,genreg(reg,8),genint(n))
	else
		if reg<>r0 then
			GERROR("DIVREG NOT R0")
		fi
		genmc(m_cdq)
		genmc(m_mov,rr2:=genreg(r2,8),genint(x))
		genmc(m_idiv,rr2)
	fi
fi
end

function dx_addrof(unit p,a,int reg,am=1)ref opndrec=
ref opndrec ax,bx
!CPL "DXADDROF"

if a^.tag=j_name then
	ax:=genmem_u(a)
else
	ax:=evaladdr(a,reg)
fi

if am<>0 and ax^.mode<>a_reg then
	genmc(m_lea, bx:=genreg(reg,ptrsize),ax)
	return bx
fi

return ax
end

function dx_dot(unit p,a,b, int reg,am)ref opndrec=
!return from here is always a memory address mode, whatever am is
!(when am=0, then caller might do lea rather than mov)
ref opndrec ax,rx
!
!CPL "DXDOT",AM,B

!CPL "DOT0"
ax:=evalexpr(a,reg)
!CPL "DOT1",=STROPND(AX),AX^.SIZE

!ax:=applyoffset(ax,offset,ttsize[b^.def^.mode])
ax:=applyoffset(ax,p^.offset,ttsize[p^.mode])
!CPL "DOT2",=STROPND(AX),AX^.SIZE

return ax
end

proc loadviaptr(ref opndrec w, ptropnd, int sgned)=
!load @ptropnd to reg opnd w
!if size of w and ptropnd is same, or w is smaller, then
!load into return w via move
!when widening is needed, then create a narrower w2, and use movsx/movzx
ref opndrec w2

if w^.size<=ptropnd^.size then
	genmc(m_mov,w, ptropnd)
	return
fi

!w2:=changeopndsize(w,ptropnd^.size)

genmc((sgned|m_iwiden|m_uwiden),w,ptropnd)

!return w2
end
=== cc_genasm.m 16/73 ===
!M Compiler - x64 Target Code Generator 2
!import main
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
import cc_libmcl
!import mx_asm

ref mclrec currmcl

const initstringsize	= 1024
const initrealsize		= 16
const initdintsize		= 16

const wstringtablesize=1024
[wstringtablesize]ref word16 wstringtable
[wstringtablesize]int wstringlentable

ref []ichar stringtable
ref []int   stringlentable
ref []real	realtable
ref []int64 dinttable

int stringtablesize
int realtablesize
int dinttablesize

global int nwstrings=0
global int nstrings=0
global int nreals=0
global int ndints=0

const asmlinelength=20000
ref char asmstart
ref char asmptr
ref char asmend
[asmlinelength]char asmbuffer

global function codegen_writeasm(int moduleno,ichar outfile)int=
!take mc code, and write it as asm source into ttdeststr
ref strec d

gs_init(dest)

inita64()

stmodule:=moduletable[moduleno].stmodule

gs_str(dest,"!x64 output for ")
gs_str(dest,stmodule^.name)
gs_strln(dest,".c")

!CPL "MODULE",STMODULE^.NAME
writetoasm(modulecode)

d:=stmodule^.deflist
while d do
	case d^.nameid
	when procid then
		currproc:=d
		writetoasm(d^.mclcode)
	esac
	d:=d^.nextdef
od

writefabs()

genstringtable()
genwstringtable()
genrealtable()
gendinttable()

terma64()

if fverbose then
	println "Writing",outfile,,":"
fi

if fastasm then
!	CPL "USING MEM ASM"
!	assemsource:=dest^.strptr
!	assemsources[:=dest^.strptr
else
	writefile(outfile,cast(dest^.strptr),dest^.length)
!	moduletable[1].asmstr:=dest^.strptr
fi
moduletable[moduleno].asmstr:=dest^.strptr

return 1
end

global proc inita64=
initasmline()

stringtable:=pcm_alloc(ref void.bytes*initstringsize)
stringlentable:=pcm_alloc(int.bytes*initstringsize)
realtable:=pcm_alloc(real.bytes*initrealsize)
dinttable:=pcm_alloc(int64.bytes*initdintsize)

stringtablesize:=initstringsize
realtablesize:=initrealsize
dinttablesize:=initdintsize

end

global proc terma64=
!gs_strln(dest,"!TERMA64")
end

proc writetoasm(ref mclrec m)=
while m do
	mcltoa64(m)
	m:=m^.nextmcl
od
end

global proc mcltoa64(ref mclrec m)=
int opcode,cond
ref opndrec a,b

!CPL MCLNAMES[M^.OPCODE]

currmcl:=m

opcode:=m^.opcode
cond:=m^.cond

a:=m^.a
b:=m^.b

switch opcode
when m_comment then
	do_comment(a,b)

when m_blank then
	do_blank(a,b)

when m_end then
	do_end(a,b)

when m_label then
	do_label(a,b)

when m_labelname then
	do_labelname(a,b)

when m_mov then
	do_mov(a,b)

when m_push then
	do_push(a)

when m_pop then
	do_pop(a)

when m_lea then
	do_lea(a,b)

when m_cmovcc then
	do_cmovcc(a,b,cond)

when m_fmov then
	do_fmov(a,b)

when m_iwiden then
	do_changeop(mx_movsx)
!	do_iwiden(a,b)

when m_uwiden then
	do_changeop(mx_movzx)
!	do_uwiden(a,b)

when m_inarrow then
	do_inarrow(a,b)

when m_unarrow then
	do_unarrow(a,b)

when m_call then
	do_call(a,b)

when m_ret then
	do_ret(a,b)

when m_retn then
	do_retn(a,b)

when m_jmp then
	do_jmp(a,b)

when m_jmpcc then
	do_jmpcc(a,b,cond)

when m_exch then
	do_exch(a,b)

when m_add then
	do_add(a,b)

when m_sub then
	do_sub(a,b)

when m_imul then
	do_imul(a,b)

!when m_umul then
!	do_umul(a,b)

when m_idiv,m_udiv then
	do_idiv(a)

!when m_udiv then
!	do_udiv(a)

when m_irem then
	do_irem(a)

when m_urem then
	do_urem(a)

when m_and then
	do_and(a,b)

when m_or then
	do_or(a,b)

when m_xor then
	do_xor(a,b)

when m_test then
	do_test(a,b)

when m_cmp then
	do_cmp(a,b)

when m_shl then
	do_shl(m_shl,a,b)

when m_ishr then
	do_shl(mx_sar,a,b)

when m_ushr then
	do_shl(mx_shr,a,b)

when m_neg then
	do_neg(a,b)

when m_not then
	do_not(a,b)

when m_inc then
	do_inc(a,b)

when m_dec then
	do_dec(a,b)

when m_setcc then
	do_setcc(a,b,cond)

when m_fneg then
	do_fneg(a)

when m_fabs then
	do_fabs(a)

when m_fsqrt then
	do_changeop((a^.size=4|mx_sqrtss|mx_sqrtsd))

when m_fadd then
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	do_changeop((a^.size=4|mx_addss|mx_addsd))

when m_fsub then
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	do_changeop((a^.size=4|mx_subss|mx_subsd))

when m_fmul then
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	do_changeop((a^.size=4|mx_mulss|mx_mulsd))

when m_fdiv then
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	do_changeop((a^.size=4|mx_divss|mx_divsd))

when m_fcmp then
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	do_changeop((a^.size=4|mx_comiss|mx_comisd))

when m_ufix then
	do_ufix(a,b)

when m_ifix then
	do_ifix(a,b)

when m_ufloat then
	do_ufloat(a,b)

when m_ifloat then
	do_ifloat(a,b)

when m_fwiden then
	convertimm(b)
	do_changeop(mx_cvtss2sd)

when m_fnarrow then
	do_changeop(mx_cvtsd2ss)

when m_fmin then
	do_changeop((a^.size=4|mx_minss|mx_minsd))

when m_fmax then
	do_changeop((a^.size=4|mx_maxss|mx_maxsd))

when m_db then
	do_db(a,b)

when m_dw then
	do_dw(a,b)

when m_dd then
	do_dd(a,b)

when m_dq then
	do_dq(a,b)

when m_defstr then
	do_defstr(a^.svalue,a^.slength)

when m_defwstr then
	do_defwstr(a^.wsvalue,a^.wslength)

when m_align then
	do_align(a,b)

when m_segment then
	do_segment(a,b)

when m_assem then
	do_assem(a,b)

when m_resb then
	convmcl()

else
	gerror_s("a64:UNKNOWN MCL OP: %s",MCLNAMES[OPCODE])
endswitch

end

proc passthru(int opc)=
ichar s

strmclx(currmcl)
gs_strn(dest,asmstart,asmptr-asmstart)
end

proc convmcl=
ichar s

strmclx(currmcl)
gs_strn(dest,asmstart,asmptr-asmstart)
end

proc do_changeop(int opc)=
currmcl^.opcode:=opc
convmcl()
end

proc do_comment(ref opndrec a,b) =
	gs_str(dest,"! ")
	gs_strln(dest,a^.svalue)
end

proc do_blank(ref opndrec a,b) =
	gs_line(dest)
end

proc do_end(ref opndrec a,b) =
	passthru(m_end)
end

proc do_label(ref opndrec a,b) =
	passthru(m_label)
end

proc do_labelname(ref opndrec a,b) =
	passthru(m_labelname)
end

proc do_mov(ref opndrec a,b) =
ref mclrec m

	if a^.mode=a_reg and b^.mode=a_imm then
		convertimm(b,0)		!note might also be real, string etc
	elsif a^.mode=a_mem and b^.mode=a_imm then
		if b^.size=8 then		!assume mem dest
			convertimm(b,0)
			m:=currmcl
			initmcdest()
			genmc(m_mov,genreg(r13,8),b)
			domclseq(mccode)
			currmcl:=m
			m^.b:=genreg(r13,8)
		else
			if a^.size=8 then
				convertimm(b,-1)		!must fit in signed imm32
			else
				convertimm(b,0)		!unsigned
			fi
		fi
	fi
	convmcl()
end

proc do_push(ref opndrec a) =
ref opndrec rx

case a^.mode
when a_reg then
	if a^.size<>8 then
		a^.size:=8
	fi
!CPL "DOPUSH",=A,=A^.SIZE

when a_imm then
	convertimm(a,sx:1)
!CPL "PUSHIMM",VALTYPENAMES[A^.VALTYPE]

	if a^.size<>8 then
		a^.size:=8
	fi

when a_mem then
	if a^.size<>8 then
		initmcdest()
CASE A^.SIZE
WHEN 1,2,4,8 THEN
		genmc(m_mov,genreg(r13,a^.size),a)
ELSE
		genmc(m_lea,genreg(r13,8),a)
ESAC

		genmc(m_push,genreg(r13,8))
		domclseq(mccode)
		return
	fi
when a_xreg then
	if a^.size=4 then
		gs_str(dest,"	movd A13, ")
		gs_strln(dest,fgetregnamex(a^.reg))
	else
		gs_str(dest,"	movq D13, ")
		gs_strln(dest,fgetregnamex(a^.reg))
	fi
	gs_strln(dest,"	push D13")
	return

esac

convmcl()
end

proc do_pop(ref opndrec a) =
case a^.mode
when a_xreg then
	gs_strln(dest,"	pop D13")
	if a^.size=4 then
		gs_str(dest,"	movd ")
		gs_str(dest,fgetregnamex(a^.reg))
		gs_strln(dest,", A13")
	else
		gs_str(dest,"	movq ")
		gs_str(dest,fgetregnamex(a^.reg))
		gs_strln(dest,", D13")
	fi
	return

esac
convmcl()
end

proc do_lea(ref opndrec a,b) =
	passthru(m_lea)
end

proc do_cmovcc(ref opndrec a,b, int cond) =
	passthru(m_cmovcc)
end

proc do_fmov(ref opndrec a,b) =
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	do_changeop((a^.size=4|mx_movd|mx_movq))
end

proc do_iwiden(ref opndrec a,b) =
	passthru(m_iwiden)
end

proc do_uwiden(ref opndrec a,b) =
	passthru(m_uwiden)
end

proc do_inarrow(ref opndrec a,b) =
	passthru(m_inarrow)
end

proc do_unarrow(ref opndrec a,b) =
!ref opndrec a2,b2

int asize
	asize:=a^.size
	if asize=4 then
		currmcl^.b:=changeopndsize(b,4)
		do_changeop(m_mov)
	else
		if b^.mode=a_reg and a^.reg=b^.reg then		!don't need to move
		else
			initmcdest()
			genmc(m_mov,changeopndsize(a,b^.size),b)
			domclseq(mccode)
		fi
		initmcdest()
		if asize>2 then gerror("unarrow 4->8?") fi
		genmc(m_and,a, genint((asize|0xFF,0xFFFF|0)))
		domclseq(mccode)
	fi
end

proc do_call(ref opndrec a,b) =
	passthru(m_call)
end

proc do_ret(ref opndrec a,b) =
	passthru(m_ret)
end

proc do_retn(ref opndrec a,b) =
	passthru(m_retn)
end

proc do_jmp(ref opndrec a,b) =
	passthru(m_jmp)
end

proc do_jmpcc(ref opndrec a,b,int cond) =
	passthru(m_jmpcc)
end

proc do_exch(ref opndrec a,b) =
	passthru(m_exch)
end

proc do_add(ref opndrec a,b) =
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	passthru(m_add)
end

proc do_sub(ref opndrec a,b) =
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	passthru(m_sub)
end

proc do_imul(ref opndrec a,b) =
	if b and b^.mode then
		currmcl^.opcode:=mx_imul2
	fi
	if b^.mode=a_imm then
		convertimm(b,1)
	fi

	convmcl()
end

proc do_idiv(ref opndrec a) =
ref opndrec dx
ref mclrec m
	if a^.mode=a_imm then
		m:=currmcl
		initmcdest()
		genmc(m_mov,dx:=genreg(r13,a^.size), a)
		domclseq(mccode)
		currmcl:=m
		currmcl^.a:=dx
	fi
	if a^.reg=r11 then
		gerror("asm/div/dividing by edx")
	fi
	if currmcl^.opcode=m_idiv then
		gs_strln(dest,(a^.size<=4|"	cdq"|"	cqo"))
		convmcl()
	else
		gs_strln(dest,"	xor rdx,rdx")
		do_changeop(mx_div)
	fi
end

proc do_irem(ref opndrec a) =
	currmcl^.opcode:=m_idiv
	do_idiv(a)
	gs_strln(dest,"	xchg rax,rdx")
end

proc do_urem(ref opndrec a) =
	currmcl^.opcode:=m_udiv
	do_idiv(a)
	gs_strln(dest,"	xchg rax,rdx")
end

proc do_and(ref opndrec a,b) =
	if b^.mode=a_imm then
		if a^.size<=4 then
!			b^.value iand:=0xFFFF'FFFF
			b^.value :=b^.value iand 0xFFFF'FFFF
			convertimm(b,0)
		else
			convertimm(b,1)
		fi
	fi
	passthru(m_and)
end

proc do_or(ref opndrec a,b) =
	if b^.mode=a_imm then
		if a^.size<=4 then
!			b^.value iand:=0xFFFF'FFFF
			b^.value :=b^.value iand 0xFFFF'FFFF
			convertimm(b,0)
		else
			convertimm(b,1)
		fi
	fi
	passthru(m_or)
end

proc do_xor(ref opndrec a,b) =
	if b^.mode=a_imm then
		if a^.size<=4 then
!			b^.value iand:=0xFFFF'FFFF
			b^.value :=b^.value iand 0xFFFF'FFFF
			convertimm(b,0)
		else
			convertimm(b,1)
		fi
	fi
	passthru(m_xor)
end

proc do_test(ref opndrec a,b) =
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	passthru(m_test)
end

proc do_cmp(ref opndrec a,b) =
	if b^.mode=a_imm then
		convertimm(b,1)
	fi
	passthru(m_cmp)
end

proc do_shl(int opc, ref opndrec a,b) =
int exchreg,breg

	currmcl^.opcode:=opc
	case b^.mode
	when a_reg then
		breg:=b^.reg

		if a^.mode=a_reg and a^.reg=r10 then		!shifting r10; assume breg not r10
			gs_str(dest,"	xchg D10,")
			gs_strln(dest,getregnamex(breg,8))

			currmcl^.a:=genreg(breg,a^.size)
			currmcl^.b:=genreg(r10,1)
			convmcl()

			gs_str(dest,"	xchg D10,")
			gs_strln(dest,getregnamex(breg,8))

		else
			b^.size:=1
			exchreg:=0
			if b^.reg<>r10 then
				gs_str(dest,"	xchg D10,")
				gs_strln(dest,getregnamex(b^.reg,8))
				exchreg:=b^.reg
				b^.reg:=r10
			fi
			convmcl()
			if exchreg then
				gs_str(dest,"	xchg D10,")
				gs_strln(dest,getregnamex(exchreg,8))
			fi
		fi
		return
	when a_imm then
	when a_mem then
		gerror("SHL/MEM")
	esac

	convmcl()
end

proc do_neg(ref opndrec a,b) =
	passthru(m_neg)
end

proc do_not(ref opndrec a,b) =
	passthru(m_not)
end

proc do_inc(ref opndrec a,b) =
	passthru(m_inc)
end

proc do_dec(ref opndrec a,b) =
	passthru(m_dec)
end

proc do_setcc(ref opndrec a,b,int cond) =
	passthru(m_setcc)
end

proc do_fneg(ref opndrec a) =
	if a^.size=4 then
		currmcl^.opcode:=mx_xorps
		currmcl^.b:=genname("[fchsmask_ps]")
	else
		currmcl^.opcode:=mx_xorpd
		currmcl^.b:=genname("[fchsmask_pd]")
	fi
	fchsused:=1
	convmcl()
end

proc do_fabs(ref opndrec a) =
	if a^.size=4 then
		currmcl^.opcode:=mx_andps
		currmcl^.b:=genname("[fabsmask_ps]")
	else
		currmcl^.opcode:=mx_andpd
		currmcl^.b:=genname("[fabsmask_pd]")
	fi
	fabsused:=1
	convmcl()
end

proc do_fsqrt(ref opndrec a,b) =
	passthru(m_fsqrt)
end

proc do_ufix(ref opndrec a,b) =
!for now, same code as unsigned
!note that word(-1.0) cannot be converted anyway
	do_ifix(a,b)
end

proc do_ifix(ref opndrec a,b) =
	do_changeop((b^.size=4|mx_cvttss2si|mx_cvttsd2si))
end

proc do_ufloat(ref opndrec a,b) =
ref opndrec rx,fx
ichar name

	rx:=genreg(r10,b^.size)
	fx:=genxreg(xr15,a^.size)

	if a^.size=4 then
		if b^.size=4 then
			name:="m$ufloat_r32u32*"
		else
			name:="m$ufloat_r32u64*"
		fi
	else
		if b^.size=4 then
			name:="m$ufloat_r64u32*"
		else
			name:="m$ufloat_r64u64*"
		fi
	fi

	initmcdest()
	genmc(m_mov,rx,b)
	genmc(m_call,genname(name))
	genmc(m_fmov,a,fx)

	domclseq(mccode)
end

proc do_ifloat(ref opndrec a,b) =
	do_changeop((a^.size=4|mx_cvtsi2ss|mx_cvtsi2sd))
end

proc do_db(ref opndrec a,b) =
	passthru(m_db)
end

proc do_dw(ref opndrec a,b) =
	passthru(m_dw)
end

proc do_dd(ref opndrec a,b) =
	convertimm(a)
	passthru(m_dd)
end

proc do_dq(ref opndrec a,b) =
	convertimm(a)
	passthru(m_dq)
end

proc do_align(ref opndrec a,b) =
	passthru(m_align)
end

proc do_segment(ref opndrec a,b) =
	passthru(m_segment)
end

proc do_assem(ref opndrec a,b) =
	gs_strln(dest,a^.svalue)
end

proc strmclasm(ref mclrec mcl)=
static [512]char str
[512]char opnds
[256]char opnd2
[128]char opcname
ref opndrec a,b
int opcode,cond,sizepref,n

initasmline()

opcode:=mcl^.opcode
cond:=mcl^.cond
a:=mcl^.a
b:=mcl^.b

case opcode
when m_assem then
	asmstr(a^.svalue)
	return

when m_blank then
	return

when m_comment then
	asmchar('!')
	asmstr(a^.svalue)
	return

when m_labelname then
	asmstr(a^.svalue)
	return

when m_label then
	asmchar('L')
!	asmchar('M')
	asmint(a^.value)
	asmchar(':')
	if a^.isglobal then
		asmchar(':')
	fi
	if b then
		asmstr("	!<")
		asmstr(b^.def^.name)
		asmchar('>')
	fi
	return

esac

asmchar('\t')

case opcode
when m_jmpcc then
	asmchar('j')
	asmstr(asmcondnames[cond])

when m_setcc then
	asmstr("set")
	asmstr(asmcondnames[cond])

when m_cmovcc then
	asmstr("cmov")
	asmstr(asmcondnames[cond])

else
	asmstr(mclnames[opcode]+(opcode<=m_end|2|3))
esac

n:=asmptr-asmstart
!while n<10 do
while n<11 do
	asmchar(' ')
	++n
od

if a and b then		!2 operands
	sizepref:=needsizeprefix(opcode,a,b)

	stropndx(a,sizepref)
	asmstr(",\t")
	stropndx(b,sizepref)

elsif a and a^.mode then								!1 operand
	if opcode=m_call then
		stropndx(a,0)
	else
		stropndx(a,1)
	fi
fi
end

global proc stropndx(ref opndrec a,int sizeprefix=0,debug=0)=
static [512]char str
[128]char str2
ichar t, plus
ref char p,q

case a^.mode
when a_reg then
	asmstr(getregnamex(a^.reg,a^.size))

when a_imm then
	strvaluex(a)

when a_mem then
	if sizeprefix then
		asmstr(getsizeprefix(a^.size,1))
	fi
	asmchar('[')

	plus:=""
	if a^.reg then
!IF A^.REG=RFRAME THEN CPL "FRAMEREG" FI

IF FSHOWNAMES AND A^.DEF AND A^.REG=RFRAME THEN
ELSE

		asmstr(getregnamex(a^.reg,ptrsize))
		plus:="+"
FI
	fi
	if a^.regix then
		asmstr(plus)
		asmstr(getregnamex(a^.regix,ptrsize))
		plus:="+"
		if a^.scale>1 then
			asmchar('*')
			asmint(a^.scale)
		fi
	fi
	if a^.def or a^.valtype then			!.valtype should be 'I' if used
		p:=asmptr
		asmchar(' ')
		q:=asmptr
		strvaluex(a)
		if q^<>'-' and plus^='+' then
			p^:='+'
		fi
		if p^=' ' then
			to asmptr-p-1 do
				p^:=(p+1)^
				++p
			od
			--asmptr
		fi

	fi
	asmchar(']')

when a_strimm then
	asmchar('/')
	asmstr(a^.svalue)
	asmchar('/')

when a_xreg then
	asmstr(fgetregnamex(a^.reg))

else
	asmstr("<BAD OPND>")
esac
end

proc strmclx(ref mclrec mcl)=
strmclasm(mcl)
asmptr++^:=10
asmptr^:=0
end

function fgetregnamex(int reg)ichar =
static [16]char str

!sprintf(&.str,"XMM%d",int32(reg-r0))
print @&.str,"XMM",,reg-r0
return &.str
end

global function getstringname(int n)ichar=
static [16]char str
if n=0 then kk0used:=1 fi

!sprintf(&.str,"KK%d",int32(n))
print @&.str,"KK",,n
return &.str
end

global function getwstringname(int n)ichar=
static [16]char str
!if n=0 then kk0used:=1 fi

!sprintf(&.str,"WW%d",int32(n))
print @&.str,"WW",,n
return &.str
end

global function getrealname(int n)ichar=
static [16]char str

!sprintf(&.str,"R.%d",int32(n))
print @&.str,"R.",,n
return &.str
end

global function getsrealname(int n)ichar=
static [16]char str

!sprintf(&.str,"SR.%d",int32(n))
print @&.str,"SR.",,n
return &.str
end

global function getdintname(int n)ichar=
static [16]char str

!sprintf(&.str,"DD.%d",int32(n))
print @&.str,"DD.",,n
return &.str
end

global function getstringindex(ichar s,int length)int=

if s=nil or length=0 then			!assume nil
	kk0used:=1
	return 0
fi

if nstrings>=stringtablesize then
	extendstringtable()
fi

stringtable^[++nstrings]:=s
stringlentable^[nstrings]:=length

return nstrings
end

global function getwstringindex(ref word16 s,int length)int=

if nwstrings>=wstringtablesize then
	gerror("Too many wide strings")
fi

wstringtable[++nwstrings]:=s
wstringlentable[nwstrings]:=length

return nwstrings
end

function getrealindex(real x)int=

!if nreals>0 and x=realtable^[nreals] then return nreals fi

if nreals>=realtablesize then
	extendrealtable()
fi

realtable^[++nreals]:=x
return nreals
end

function getdintindex(int64 x)int=
if ndints>=dinttablesize then
	extenddinttable()
fi

dinttable^[++ndints]:=x
return ndints
end

proc strvaluex(ref opndrec a)=
ref strec d
static [256]char str
[256]char str2
int64 value

d:=a^.def
value:=a^.value

if d then
	case d^.nameid
	when staticid then
		if d^.owner^.nameid=procid then
			asmchar('`')
			asmstr(d^.owner^.name)
			asmchar('.')
			asmstr(d^.name)
			asmchar('.')
			asmint(d^.blockno)
		else
			asmchar('`')
			asmstr(getfullname(d))
			if isimported(d) then
				asmchar('*')
			fi
		fi
	when frameid, paramid then
		if fshownames then
			asmstr(d^.name)
		else
			asmint(d^.offset)
		fi
	else
		asmchar('`')
		asmstr(getfullname(d))
		if isimported(d) then
			asmchar('*')
		fi
	esac
	if a^.valtype=int_val and value<>0 then
		if value>0 then
			asmchar('+')
		fi
		asmint(value)
	fi
	return
fi

case a^.valtype
when intix_val then
	asmstr(getdintname(a^.index))
	return
when realix_val then
	if a^.isfloat then
		asmstr(getsrealname(a^.index))
	else
		asmstr(getrealname(a^.index))
	fi
	return
when stringix_val then
	asmstr(getstringname(a^.index))
	return
when wstringix_val then
	asmstr(getwstringname(a^.index))
	return
esac

!CPL "HERE",STRVALUE(A),VALTYPENAMES[A^.VALTYPE]
asmstr(strvalue(a))
end

proc convertimm(ref opndrec a,int sx=0)=
!a is an immediate operand
!convert if needed as follows:
!'I'	To int64 memory index
!'R'	To real64 memory index (or real32)
!'S'	To string memory index
!sx says how immediate ints are to be processed:
! 0		No restrictions; imm64 is OK (caller might need to generate extra steps)
! -1/1		limited to signed imm32
! -2/2		limited to unsigned imm32
! For 1/2, will convert to mem64 if out of range
! For -1/-2, will raise error
int64 value

if a^.def then return fi			!named label; will only have offset, assumed to be OK
value:=a^.value

case a^.valtype
when int_val then
	case abs(sx)
	when 1 then
		if int32.minvalue<=value<=int32.maxvalue then
		else
			if sx<0 then gerror("conv/imm1") fi
			a^.index:=getdintindex(value)
			a^.valtype:=intix_val
			a^.mode:=a_mem
		fi
	when 2 then
		if word32.minvalue<=value<=word32.maxvalue then
		else
			if sx<0 then gerror("conv/imm2") fi
			a^.index:=getdintindex(value)
			a^.valtype:=intix_val
			a^.mode:=a_mem
		fi
	esac
when real_val then
	a^.index:=getrealindex(a^.xvalue)
	a^.valtype:=realix_val
	a^.mode:=a_mem
	a^.isfloat:=a^.size=4
when string_val then
	a^.index:=getstringindex(a^.svalue,a^.slength)
	a^.valtype:=stringix_val
when Wstring_val then
	a^.index:=getwstringindex(a^.wsvalue,a^.wslength)
	a^.valtype:=wstringix_val
esac

end

proc genstringtable=				!GENSTRINGTABLE
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
int i, col

return unless nstrings or kk0used

gs_strln(dest,"!String Table")
gs_strln(dest,"	segment idata")
gs_str(dest,"	align ")
gs_strint(dest,targetsize)
gs_line(dest)

if kk0used then
	gs_strln(dest,"kk0:    db 0")
fi

for i to nstrings do
	col:=dest^.length
	gs_str(dest,getstringname(i))
	gs_str(dest,":")

	gs_padto(dest,8)

	genstring(stringtable^[i],stringlentable^[i])
od
gs_line(dest)
end

proc genwstringtable=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
int i, col

return unless nwstrings

gs_strln(dest,"!Wide String Table")
gs_strln(dest,"	segment idata")
gs_str(dest,"	align ")
gs_strint(dest,targetsize)
gs_line(dest)

!if kk0used then
!	gs_strln(dest,"kk0:    db 0")
!fi

for i to nwstrings do
	col:=dest^.length
	gs_str(dest,getwstringname(i))
	gs_str(dest,":")

	gs_padto(dest,8)

	genwstring(wstringtable[i],wstringlentable[i])
od
gs_line(dest)
end

proc do_defstr(ichar s,int length)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
int i, state, c, a,col

gs_str(dest,"	")
genstring(s,length)

end

proc do_defwstr(ref word16 s,int length)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
int i, state, c, a,col

gs_str(dest,"	")
genwstring(s,length)

end

proc genrealtable=				!GENREALTABLE
	record fprec=
		union
			real64 x64
			int64 ix64
		end
		union
			real32 x32
			int32 ix32
		end
	end
	fprec fp
	real x
	int i
	[1282]char str

	return unless nreals

	gs_strln(dest,"!Real64 Table")
	gs_strln(dest,"	segment idata")
	gs_str(dest,"	align ")
	gs_strint(dest,targetsize)
	gs_line(dest)

	for i to nreals do
		x:=realtable^[i]
		fp.x64:=x

		gs_str(dest,getrealname(i))
		gs_str(dest,":")
		gs_padto(dest,10,' ')
		gs_str(dest,"dq ")
		gs_strint(dest,fp.ix64)
		gs_str(dest,"	; ")
!		sprintf(&.str," %016llX ",fp.ix64)
		strcpy(&.str, strint(fp.ix64,"z16H"))
		gs_str(dest,&.str)

!		sprintf(&.str,"%.30g",x)
		strcpy(&.str,strreal(x,".30g"))
		gs_strln(dest,&.str)

	od
	gs_line(dest)
	gs_strln(dest,"!Real32 Table")

	for i to nreals do
		x:=realtable^[i]
		fp.x32:=x

		gs_str(dest,getsrealname(i))
		gs_str(dest,":")
		gs_padto(dest,10,' ')
		gs_str(dest,"dd ")
		gs_strint(dest,fp.ix32)
		gs_str(dest,"	; ")
!		sprintf(&.str,"%.30g",real(fp.x32))
		print @&.str,real(fp.x32):".30g"
		gs_strln(dest,&.str)

!	gs_strln(dest,leftstr(getrealname(i)+":",10)+"dq "+tostr(fp.ix64)+"	; "+tostr(x,".30g"))
	od
	gs_line(dest)
end

proc gendinttable=				!GENDINTTABLE
int i
int64 x

return unless ndints
gs_strln(dest,"!Int64 Table")
gs_strln(dest,"	segment idata")
gs_str(dest,"	align ")
gs_strint(dest,targetsize)
gs_line(dest)

for i to ndints do
	x:=dinttable^[i]
	gs_str(dest,getdintname(i))
	gs_str(dest,":")
	gs_str(dest,"dq ")
	gs_strint(dest,x)
	gs_line(dest)
od
gs_line(dest)
end

proc writefabs=
if fabsused or fchsused then
	gs_strln(dest,"	segment idata")
	gs_strln(dest,"	align 16")

	if fchsused then
		gs_strln(dest,"fchsmask_ps:	dq 0x80000000'80000000, 0x80000000'80000000")
		gs_strln(dest,"fchsmask_pd:	dq 0x80000000'00000000, 0x80000000'00000000")
	fi

	if fabsused then
		gs_strln(dest,"fabsmask_ps:	dq 0x7fffffff'7fffffff, 0x7fffffff'7fffffff")
		gs_strln(dest,"fabsmask_pd:	dq 0x7fffffff'ffffffff, 0x7fffffff'ffffffff")
	fi
fi

end

proc domclseq(ref mclrec m)=
!convert short mcl code sequence that might have been generated here
while m do
	mcltoa64(m)
	m:=m^.nextmcl
od
end

proc asmstr(ichar s)=
while (s^) do asmptr++^:=s++^ od
end

proc asmstrln(ichar s)=
char c
while c:=s^ do
	asmptr^:=c
	++asmptr
	++s
od
asmptr^:=10
++asmptr
end

proc asmline=
asmptr^:=10
++asmptr
end

proc asmln=
asmline()
end

proc asmint(int64 a)=
ichar s

s:=asmptr
getstrint(a,s)

!if a<10 then
!	++asmptr
!elsif a<100 then
!	asmptr+:=2
!else
	asmptr+:=strlen(s)
!fi

!n:=sprintf(asmptr,"%lld",a)
!asmptr+:=n
end

proc asmchar(int c)=
asmptr^:=c
++asmptr
end

proc asmterm=
asmptr^:=0
end

proc initasmline=

asmptr:=asmstart:=&.asmbuffer
asmend:=&.asmbuffer+asmlinelength
end

function getregnamex(int reg,size=4)ichar=
static [1..8,0..r15]ichar regnames = (
	("-","B0","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14","B15"),
	("-","W0","W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12","W13","W14","W15"),
	(nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil),
	("-","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","Aframe","Astack"),
	(nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil),
	(nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil),
	(nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil, nil,nil,nil,nil),
	("-","D0","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","Dframe","Dstack"))

return regnames[size,reg]
end

proc genstring(ichar s, int length)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
int i, state, c, a,col

gs_str(dest,"db ")
if length=0 then
	gs_strln(dest,"0")
	return
fi

state:=0
to length do
	a:=s++^
	if a<32 or a>=127 or a='\"' then
		if state=1 then
			gs_str(dest,""",")
			state:=0
		fi
		gs_strint(dest,a)
		gs_str(dest,",")
	else
		if state=0 then
			gs_str(dest,"""")
			state:=1
		fi
		gs_char(dest,a)
	fi
od
if state=1 then
	gs_str(dest,""",")
fi
gs_str(dest,"0")

gs_line(dest)
end

proc genwstring(ref word16 s, int length)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
int i, state, c, a,col

gs_str(dest,"dw ")
if length=0 then
	gs_strln(dest,"0")
	return
fi

state:=0
for i to length do
	gs_strint(dest,s++^)
!	if i<length then
		gs_str(dest,",")
!	fi
od
gs_str(dest,"0")

gs_line(dest)
end

proc extendrealtable=
	ref[]real oldrealtable
	int oldrealtablesize

	oldrealtablesize:=realtablesize
	oldrealtable:=realtable

	realtablesize*:=2
!CPL "EXTENDING REAL TABLE TO",REALTABLESIZE

	realtable:=pcm_alloc(real.bytes*realtablesize)

	for i:=1 to nreals do
		realtable^[i]:=oldrealtable^[i]
	od

	pcm_free(oldrealtable,real.bytes*oldrealtablesize)
end

proc extenddinttable=
	ref[]int64 olddinttable
	int olddinttablesize

	olddinttablesize:=dinttablesize
	olddinttable:=dinttable

	dinttablesize*:=2
!CPL "EXTENDING DINT TABLE TO",DINTTABLESIZE

	dinttable:=pcm_alloc(int64.bytes*dinttablesize)

	for i:=1 to ndints do
		dinttable^[i]:=olddinttable^[i]
	od

	pcm_free(olddinttable,int64.bytes*olddinttablesize)
end

proc extendstringtable=
	ref[]ichar oldstringtable
	ref[]int oldstringlentable
	int oldstringtablesize

	oldstringtablesize:=stringtablesize
	oldstringtable:=stringtable
	oldstringlentable:=stringlentable

	stringtablesize*:=2
!CPL "EXTENDING STRING TABLE TO",STRINGTABLESIZE

	stringtable:=pcm_alloc(ichar.bytes*stringtablesize)
	stringlentable:=pcm_alloc(int.bytes*stringtablesize)

	for i:=1 to nstrings do
		stringtable^[i]:=oldstringtable^[i]
		stringlentable^[i]:=oldstringlentable^[i]
	od

	pcm_free(oldstringtable,ichar.bytes*oldstringtablesize)
	pcm_free(oldstringlentable,int.bytes*oldstringtablesize)
end
=== cc_export.m 17/73 ===
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
import cc_headers
!import cc_headersx
import cc_lex

strbuffer mmbuffer
ref strbuffer mm=&mmbuffer

global proc writemheader(ichar infile)=
[300]char mfile
ref strec d,e
int m

strcpy(&.mfile,pcm_copyheapstring(changeext(infile,".m")))

gs_init(mm)

mmstr("importdll ")
!mmstr(changeext(infile,""))
mmstr(extractbasefile(infile))
mmstrln(" =")

stmodule:=moduletable[1].stmodule


d:=stmodule^.deflist

while d do
!CPL D^.NAME,D^.LINENO
	if isheaderfile(sourcefilenames[d^.lineno>>24]) then
		d:=d^.nextdef
		next
	fi
	case d^.nameid
	when staticid then
!GS_STR(MM,"STATIC "); GS_STRLN(MM,D^.NAME)
!		mmstr("    ")
		mmstr("    ")
		mmmode(d^.mode)
!		mmstr(" """)
		mmstr(" ")
		mmstr(fixname(d^.name))
!		mmstr("""")
		if d^.code then
!CPL "STATIC INIT VAR"
			mmstr(" =")
			mmstr(strexpr(d^.code)^.strptr)
		fi
		mmline()

	when procid then
		writefunction(d)
	when typeid then
!		m:=d^.mode
!		case ttbasetype[m]
!		when tstruct then
!			writerecord(m)
!		esac
	when enumid then
!		mmstr("ENUM ")
!		mmstrln(d^.name)
!		mmstr("    const """)
		mmstr("    const ")
		mmleftstr(fixname(d^.name),34)
!		mmstr(""" = ")
		mmstr(" = ")
		mmint(d^.index)
		mmline()

	when macroid then
		mmstr("MACRO ")
		mmstrln(fixname(d^.name))
	when structtagid then
		writerecord(d^.mode)
!		mmstr("STRUCT TAG")
!		mmstrln(d^.name)

	esac
	d:=d^.nextdef
od


for i:=0 to hstmask do
	e:=hashtable^[i]
	if e^.name and e^.symbol=namesym and e^.nameid=macroid then
		if not isheaderfile(sourcefilenames[e^.lineno>>24]) then
			if e^.tokenlist then
				mmstr("    const ")
				mmleftstr(fixname(e^.name),34)
				mmstr(" = ")
				showmacroseq(e^.tokenlist)
				mmstrln("    ! macro")
			fi
		fi

	fi
od
mmstrln("end")

!CPL "DONE"

!CPL MM^.STRPTR
moduletable[1].mhdrstr:=mm^.strptr

if logdest then
	println @logdev,"M HEADERS\N========="
	println @logdev,mm^.strptr
fi

CPL "Writing M Header:",&.mfile
writefile(&.mfile,cast(mm^.strptr),mm^.length)

end

proc showmacroseq(ref tokenrec tk)=
while tk do
	emittoken(tk,mm)
	tk:=tk^.nexttoken
od
end

proc mmstr(ichar s)=
gs_str(mm,s)
end

proc mmleftstr(ichar s,int n)=
gs_leftstr(mm,s,n)
end

proc mmstrln(ichar s)=
gs_strln(mm,s)
end

proc mmint(int a)=
[32]char str
!sprintf(&.str,"%lld",a)
getstrint(a,&.str)
gs_str(mm,&.str)
end

proc mmline()=
gs_line(mm)
end

proc writefunction(ref strec d)=
ichar file
ref paramrec pm
int n,isvar

!file:=sourcefilenames[d^.lineno>>24]
!if isheaderfile(file) then
!!	CPL "IS IN HEADER:",D^.NAME
!	return
!fi

if d^.mode=tvoid then
	mmstr("    clang proc     ")
else
	mmstr("    clang function ")
fi
mmstr("""")
!mmstr(fixname(d^.name))

mmstr(d^.name)

!mmstr(""" (")
mmstr("""")
!mmleftstr(" ",34-strlen(fixname(d^.name)))
mmleftstr(" ",34-strlen(d^.name))
mmstr("(")

!CPL D^.NAME,=D^.PARAMLIST

pm:=d^.paramlist
n:=pm^.nparams
isvar:=pm^.flags=pm_variadic
for i to n do
	mmmode(pm^.mode)
	if i<>n or isvar then
!		if i<>n then
		mmstr(",")
	fi
	pm:=pm^.nextparam
od
if isvar then
	mmstr("...")
fi

mmstr(")")

if d^.mode<>tvoid then
	mmmode(d^.mode)
fi

mmline()
!CPL "END FN"
end

proc mmmode(int m,expand=1) =
int t,u

t:=ttbasetype[m]
case t
when tref then
	mmstr("ref ")
	u:=tttarget[m]
	if ttbasetype[u]=tproc then
		writefnptr(u)
	else
		mmmode(tttarget[m])
	fi

when tarray then
	mmstr("[")
	if ttlength[m] then
		mmint(ttlength[m])
	fi
	mmstr("]")
	mmmode(tttarget[m])

when tenum then
	mmstr("int")

when tstruct,tunion then
	mmstr(fixname(ttnamedef[m]^.name))
!	MMSTR("<STRUCT/UNION>")

when tproc then
	MMSTR("<PROC>")
!	strcpy(dest,"proc[PM](")
!	pm:=ttparams[m]
!	n:=pm^.nparams
!	for i to n do
!		istrmode(pm^.mode,0,dest+strlen(dest))
!		if i<>n then
!			strcat(dest,",")
!		fi
!		pm:=pm^.nextparam
!	od
!	strcat(dest,")")
!	istrmode(tttarget[m],0,dest+strlen(dest))

else
	mmstr(stdtypemnames[t])
esac
end

proc writerecord(int m, rectype='R', level=1)=
ref strec d,e
int emode

!mmline()

to level do
	mmstr("    ")
od
++level

d:=ttnamedef[m]
if rectype='R' then
	mmstr("record ")
	mmstr(fixname(d^.name))
	mmstrln(" =")
else
	mmstrln((rectype='S'|"struct"|"union"))
fi

e:=d^.deflist
if e=nil then
	to level do
		mmstr("    ")
	od
	mmstrln("var int dummy    !empty record")
fi

while e do
	emode:=e^.mode
	to level do
		mmstr("    ")
	od

	if strchr(e^.name,'$') then
		case ttbasetype[emode]
		when tunion then
			writerecord(emode,'U',level)
		when tstruct then
			writerecord(emode,'S',level)
		esac
	else
		mmstr("var ")
		mmmode(e^.mode)
		mmstr(" ")
		mmstrln(fixname(e^.name))
	fi
	e:=e^.nextdef
od
to level-1 do
	mmstr("    ")
od
mmstrln("end")
mmline()
end

proc writefnptr(int m)=
ref paramrec pm
int isvar,n,target


target:=tttarget[m]


if target=tvoid then
	mmstr("clang proc(")
else
	mmstr("clang function(")
fi

pm:=ttparams[m]
n:=pm^.nparams
isvar:=pm^.flags=pm_variadic
for i to n do
	mmmode(pm^.mode)
	if i<>n or isvar then
!		if i<>n then
		mmstr(",")
	fi
	pm:=pm^.nextparam
od
if isvar then
	mmstr("...")
fi

mmstr(")")

if target<>tvoid then
	mmmode(target)
fi

end

function fixname(ichar name)ichar=
static []ichar reservedwords = (
	"function",
	"read",
	"type",
	"next",
	"stop",
	"callback",
	"len",
	"$dummy"
)
[128]char str

for i to reservedwords.len do
	if eqstring(reservedwords[i],name) then
		strcpy(&.str,name)
		strcat(&.str,"$")
!		return &.str
		return pcm_copyheapstring(&.str)
	fi
od

!strcpy(&.str,name)
!convlcstring(&.str)
!return pcm_copyheapstring(&.str)

return name
end
=== cc_assembler.m 18/73 ===
import clib
import mlib
import oslib

import ax_tables
import ax_decls
import ax_lex
import ax_parse
import ax_lib
import ax_genss
import ax_writeexe
import ax_writeobj

global function assembler(ichar outputfile, ref[]ichar asmfiles,dllfiles,
!		int nasmfiles,ndllfiles,fobj,fcaption,ichar assemsource=nil)int=
		int nasmfiles,ndllfiles,fobj,fcaption,ref[]ichar assemsources=nil,
		ichar entrypointname)int=
ref strbuffer ss
int ntokens,t,i

!CPL "INTERNAL ASSEMBLER", =ENTRYPOINTNAME

initall()

for i to nasmfiles do
	addmodule(asmfiles^[i])

!CPL I,ASMFILES^[I]

od
!CPL =NASMFILES,=NMODULES

!if nsearchlibs=0 then
	searchlibs[1]:="ucrtbase"
	searchlibs[1]:="msvcrt"
	searchlibs[2]:="gdi32"
	searchlibs[3]:="user32"
	searchlibs[4]:="kernel32"
	nsearchlibs:=4	
!fi

for i to ndllfiles do
	addsearchlib(dllfiles^[i])
od

!if nsearchlibs=0 then
!	searchlibs[1]:="msvcrt"
!	searchlibs[2]:="gdi32"
!	searchlibs[3]:="user32"
!	searchlibs[4]:="kernel32"
!	nsearchlibs:=4	
!fi
if nmodules=0 then
	loaderror("No input files specified")
fi

if fcaption then
	println "Assembling to",outputfile
fi

loadsourcefiles(assemsources)

parsemodules()

if fobj then
	genss()
	writess(outputfile)
else
	genss()
	initsectiontable()
	genexe(entrypointname)
	writeexe(outputfile)
fi
return 1
end

proc loadsourcefiles(ref[]ichar assemsources)=
	int i
	ichar source

!CPL "LOADSOURCEFILES",=REF VOID(ASSEMSOURCE)

	for i to nmodules do
!		if assemsources and i<nmodules then	!NOTE LAST MODULE IS ALWAYS BCCLIB
		if assemsources then
			source:=assemsources^[i]
		else
			source:=cast(readfile(moduletable[i].filename))
			if source=nil then
				loaderror_s("Can't load file: %s",moduletable[i].filename)
			fi
		fi
		moduletable[i].source:=source
	od
end

proc parsemodules=
	int i
	ichar source

	for i to nmodules do
		currmoduleno:=i
		modulenamelist:=nil
		readmodule(i)

		checkundefined()
		if nundefined then
			println "Couldn't assemble - press key"
			os_getch()
			stop 1
		fi

		scanglobals()			!fixup any globals and imports
		resethashtable()

	od

!Try scanning all mclcode to fix imports/exports. That is, all operands
!point to the same st entry
ref mclrec m

m:=mccode

while m do
	fixopnd(m^.a)
	fixopnd(m^.b)
	m:=m^.nextmcl
od

end

proc fixopnd(ref opndrec a)=
ref strec d
if a=nil then return fi
if a^.labeldef then
	d:=a^.labeldef
	if d^.basedef then
		a^.labeldef:=d^.basedef
	fi
fi
end

proc initall=
!pcm_init()
initlex()
initlib()
end

proc loaderror(ichar mess)=
println "Error:",mess
stop 1
end

proc loaderror_s(ichar mess,s)=
[256]char str
strcpy(&.str,mess)
strcat(&.str,mess)

!sprintf(&.str,mess,s)
loaderror(&.str)
end

proc addmodule(ichar name)=
if nmodules>=maxmodules then
	loaderror("Too many modules")
fi
++nmodules
moduletable[nmodules].filename:=pcm_copyheapstring(name)
moduletable[nmodules].name:=pcm_copyheapstring(extractfile(name))
moduletable[nmodules].source:="<empty>"

end

proc addsearchlib(ichar name)=
[300]char str
if nsearchlibs>=maxsearchlibs then
	loaderror("Too many libraries")
fi
++nsearchlibs
strcpy(&.str,name)
str[strlen(name)-3]:=0			!get rid of .dll extension
searchlibs[nsearchlibs]:=pcm_copyheapstring(&.str)
end

function getemptyst(ref strec d)ref strec=
!d is an existing strec
!create an new empty strec if needed (when d is also keyword name,
! and/or e is not nil), and return a pointer to that
!otherwise just return nil
ref strec dnew

if d^.ksymbol then					!need a replacement strec
	dnew:=pcm_allocz(strec.bytes)
	dnew^.name:=d^.name
	dnew^.namelen:=d^.namelen
	dnew^.ksymbol:=d^.ksymbol
	dnew^.subcode:=d^.subcode
	dnew^.regsize:=d^.regsize
	return dnew
fi
return nil
end

function findduplname(ref strec d)ref strec=
!look for any dupl global/export name to d

ref strec e
if d^.basedef then
	return d^.basedef
fi

e:=dupltable[d^.htfirstindex]

while e do
	if d^.namelen=e^.namelen and memcmp(d^.name,e^.name,d^.namelen)=0 then
		d^.basedef:=e
		return e
	fi
	e:=e^.nextdupl
od
return nil
end

proc adddupl(ref strec d)=
!add the first dupl entry for d in dupltable
!the linked list is in reverse order, and generally ends up containing
!one element unless there are two or more names that share the same default
!hash table entry

d^.nextdupl:=dupltable[d^.htfirstindex]
dupltable[d^.htfirstindex]:=d
end

proc scanglobals=
!have just finished parsing a module
!scan the symbols defined there to:
! * find any new imports/globals
! * find new imports/globals to merge with existing ones
!Then the entries in the hashtable must be purged, by substituting with
!either nil, or an empty value if keyword data or .basedef must be remembered

ref strec d,e

d:=modulenamelist

while d do
	case d^.symbol
	when importedsym then
		e:=findduplname(d)
		if e then
			case e^.symbol
			when importedsym then			!no change
			when exportedsym then
				d^.symbol:=exportedsym		!set both global
				d^.reftype:=e^.reftype:=fwd_ref
			esac
		else
			addimport(d)
			adddupl(d)
		fi
	when exportedsym then
		e:=findduplname(d)
		if e then
			case e^.symbol
			when importedsym then
				e^.symbol:=exportedsym		!set both global
				d^.reftype:=e^.reftype:=fwd_ref
			when exportedsym then			!error?
CPL MODULETABLE[D^.MODULENO].NAME,D^.NAME,D^.HTINDEX
CPL MODULETABLE[E^.MODULENO].NAME,E^.NAME,E^.HTINDEX
				serror_s("Multiply-defined global: %s",d^.name)
			esac
		else
			e:=d
			addimport(d)
			adddupl(d)
		fi
	esac

	d:=d^.nextdef
od
end

proc resethashtable=
!have just finished parsing a module
!scan the symbols defined there to:
! * find any new imports/globals
! * find new imports/globals to merge with existing ones
!Then the entries in the hashtable must be purged, by substituting with
!either nil, or an empty value if keyword data or .basedef must be remembered

ref strec d,e

d:=modulenamelist

while d do
	lexhashtable[d^.htindex]:=getemptyst(d)
	d:=d^.nextdef
od

modulenamelist:=nil

end
=== ax_tables.m 19/73 ===
!MXA Assembler Tables
!Some of these are used in an initialisation routines to set up the
!global hash table where names from the .asm file are looked up

global tabledata() []ichar symbolnames=
	(errorsym,			$),		! Lex error
	(commasym,			$),		! ","
	(colonsym,			$),		! ":"
	(dcolonsym,			$),		! "::"
	(lsqsym,			$),		! [
	(rsqsym,			$),		! ]

	(addsym,			$),		! +
	(subsym,			$),		! -
	(mulsym,			$),		! *

	(eqsym,				$),		! =

	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen

	(hashsym,			$),		! #

	(intconstsym,		$),		! 123 64 bits signed
	(realconstsym,		$),		! 123.4 64 bits
	(stringconstsym,	$),		! "ABC"

	(namesym,			$),		! raw name
	(namedconstsym,		$),		! name = expr
	(fwdlocalsym,		$),		! name
	(localsym,			$),		! name::
	(importedsym,		$),		! name*
	(exportedsym,		$),		! name:::

	(kopcodesym,		$),		! mov etc
	(kregsym,			$),		! d0, r5, eax etc
	(kxregsym,			$),		! xmm0 etc
	(kfregsym,			$),		! st0 etc
	(kmregsym,			$),		! mmx1 etc
	(kjmpccsym,			$),		! jz etc
	(ksetccsym,			$),		! setz etc
	(kmovccsym,			$),		! cmovz etc
	(kprefixsym,		$),		! dword etc
	(ksegnamesym,		$),		! idata etc

	(kdummysym,			$)		!
end

global tabledata() []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_comment,			$,		0,		0),		!
	(m_blank,			$,		0,		0),		!
	(m_end,				$,		0,		0),		!

	(m_label,			$,		1,		0),		!
	(m_nop,				$,		0,		0x90),		!
	(m_param,			$,		1,		0),		!
	(m_assem,			$,		1,		0),		!
	(m_proc,			$,		1,		0),		!

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

!regcodes is the is the set of internal codes used by the processor to
!identify a register. A 4-bit field, usually the bottom 3 bits form of the
!instruction bytes, while the top bit, if not 0, is part of the REX prefix

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

global []ichar xregnames = (
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

global tabledata() [0:]ichar condnames =

	(ov_cond	= 0,	"o"),
	(nov_cond	= 1,	"no"),

	(ltu_cond	= 2,	"b"),
	(geu_cond	= 3,	"ae"),

	(eq_cond	= 4,	"z"),
	(ne_cond	= 5,	"nz"),

	(leu_cond	= 6,	"be"),
	(gtu_cond	= 7,	"a"),

	(s_cond		= 8,	"s"),
	(ns_cond	= 9,	"ns"),

	(p_cond		= 10,	"p"),
	(np_cond	= 11,	"np"),

	(lt_cond	= 12,	"l"),
	(ge_cond	= 13,	"ge"),

	(le_cond	= 14,	"le"),
	(gt_cond	= 15,	"g"),
end

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

global tabledata []ichar prefixnames, []byte prefixsizes =
	("byte",	1),		
	("word",	2),
	("word16",	2),
	("word32",	4),
	("dword",	4),
	("word64",	8),
	("qword",	8),
	("word128",	16)
end

!global tabledata() []ichar scopenames =	! set in pass1
!	(fwd_scope,			$),		! has appeared in an expr, without *
!	(extern_scope,		$),		! has appeared in an expr, with *
!	(local_scope,		$),		! fwd label defined using ::
!	(global_scope,		$),		! fwd label defined using :::
!end
!
global tabledata() [0:]ichar reftypenames =	!use during pass2
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

global tabledata() []ichar segmentnames =
	(code_seg,		$),
	(idata_seg,		$),
	(zdata_seg,		$),
	(rodata_seg,	$),
	(impdata_seg,	$),
end
=== ax_decls.m 20/73 ===
!MXA Assembler Global Decls

global const compilerversion="2018.1.22"

!STREC usage::

!symbol=
! namesym			name, truename=""
! namedconstsym		name, value=(labeldef,value)
! labelsym			name, labdefined, value, segment, scope, lineno, stindex, offset

! kdirectivesym		// not done yet
! kopcodesym		name="mov" etc, subcode=m_mov etc
! kregsym			name="r0" etc, subcode=r0/etc, regsize=1/2/4/8
! kxregsym			name="xmm0" etc, subcode=r0/etc
! kfregsym			name="st0" etc, subcode=t0/etc
! kmregsym			name="mmx0" etc, subcode=r0/etc
! kjmpccsym			name="jz"/etc, subcode=z_cond/etc
! ksetccsym			name="setz"/etc, subcode=z_cond/etc
! kmovccsym			name="cmovz"/etc, subcode=z_cond/etc
! ksegnamesym,		name="code" etc, subcode=code_seg/etc

!global type strec     = forward
!global type fwdrec    = forward
!!global type valuerec  = forward
!global type opndrec  = forward

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record opndrec = !24 bytes
	ref strec labeldef	!nil, or handle of strec for label
	union
		int64 value		!const value/extra offset/cond code/string for comments
		real64 xvalue	!floating point value
		ref char svalue
	end
	byte mode		!a_reg etc, low level operand details
	byte size		!byte size of operand: 1,2,4,8
	byte reg		!0, or main register
	byte regix		!0, or index register

	byte scale		!0, or scale factor for regix
	byte addrsize	!4 or 8 for a_mem when regs are involved
	byte valtype	!0 (no value or int) or 'R'/'S'
	byte spare2
end

global record strec =
	ichar name			!name of symbol (named token/keyword or identifier)
	ref fwdrec fwdrefs	!fwd ref chain
!	union
!		ref valuerec value	!named constants: valuerec([label],[value])
		ref opndrec expr	!named constants: valuerec([label],[value])
!		struct
			int32 offset		!label (pass 2): offset of label when encountered
			int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
			int32 importindex	!genexe: index into import table
!		end
!	end

	byte symbol			!type of token, eg. namesym
	byte ksymbol		!type of keyword, eg. opcodesym
	byte subcode		!when used as keyword
	byte regsize		!for reg keywords

	byte scope			!label pass 1: fwd/extern/local/global
	byte reftype		!label pass 2: extern/back/fwd
	byte segment		!label pass 2: code_seg etc or 0
	byte namelen

	ref strec basedef		!nil, or global/import def using this name
	ref strec nextdef		!in module name list
	ref strec nextdupl		!when part of in global import list

	int32 moduleno
!	word16 htindex				!index into hashtable
!	word16 htfirstindex			!initial index before stepping to avoid clashes
	word32 htindex				!index into hashtable
	word32 htfirstindex			!initial index before stepping to avoid clashes
	[48]BYTE SPARE
end

global record relocrec =			!informal version
	ref relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
global record dbuffer =
	ref byte pstart
	union
		ref byte pcurr
		ref word16 pcurr16
		ref word32 pcurr32
		ref word64 pcurr64
	end
	ref byte pend
	int alloc
end

global record modulerec =
	ichar filename
	ichar name
	ichar source
end

global record stlistrec =
	ref strec def
	ref stlistrec nextitem
end

!global var symboltable=()		!list of strecs representing user identifiers

global int lxfileno=0	!*@ current source file number
global int lxlineno=0	!*@ current source line number

global int nsourcefiles=0	!no. of linear file names
!global sourcefiles=()	![1..nsourcefiles]str source filenames, linear list of all

global const maxmodules=200
global const maxsearchlibs=30
global [maxmodules]modulerec moduletable
global [maxsearchlibs]ichar searchlibs
global int nmodules
global int nsearchlibs

!global const hstsize=32768
!global const hstsize=65536
!global const hstsize=131072
!global const hstsize=262144
!global const hstsize=1048576
global const hstsize=2097152
!global const hstsize=2048

global const hstmask=hstsize-1
global [0:hstsize]ref strec lexhashtable
!global [hstsize]strec lexhashtable
global [0:hstsize]ref strec dupltable		!link dupl names

global ref void logdev		!dest for diagnostics and output of tables
!global var logpos		!position in logdev file relating to particular block

global int fverbose=0		!whether to display message for each pass
global int fquiet=0
!global ichar entrypointname = "start"

global int LINECOUNT=0

global int nundefined=0
global int alineno=0

!GLOBAL INT NALLOPNDS
!GLOBAL INT NREGS
!GLOBAL INT NIREGS
!GLOBAL INT NZEROS
!GLOBAL INT NBYTES
!GLOBAL INT NDWORDS

global int ss_zdatalen
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

!const max_ss_symbols=32768				!exported to coff
!global const init_ss_symbols=32768				!exported to coff
global const init_ss_symbols=16384
global ref []ref strec ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref stlistrec globalimportlist		!all global vars and imports across all moduls

global ref strec modulenamelist			!all defs defined in last module
global int currmoduleno

GLOBAL INT NMCLASM
GLOBAL INT NMCLOPNDSASM
=== ax_lex.m 21/73 ===
!MXA Tokeniser Module
!import main
import clib
import mlib
import oslib
import ax_tables
import ax_decls

macro testmode=0

!INT NHASH=0
!INT NCLASH=0
!INT NLOOKUPS=0
!INT NMEMSTR=0
!INT NNUMBERS=0
!INT NNAMES=0
!INT NTRUENAMES=0

fwdrec dummy1
opndrec dummy2
!valuerec dummy2

!this record stores details of each token, part from value of tokens such as intconst, string or
!identifiers (names which are keywords are not stored). 

!global type tokenrec = struct
!	byte symbol			!lex symbol
!	byte index
!	word16 subcode			!symbol subcode; in the case of macros, is index into macrotable
!	int32 lineno			!contains fileno in top byte
!end

const etx = 26
const cr  = 13
const lf  = 10

!the following returned by updated by lexreadtoken()

global int lxsymbol		!* main symbol kind
global int lxsubcode	!* for some symbols, which specific keyword

global int64 lxvalue
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
!global var lxlineno		!* contains names, strings, etc, or numeric values
int lxhashvalue
!ref strec laststentry

global ref byte lxsptr		!@ points to next char in source
ref byte lxstart		!@ start of source code for this file
global ref strec lxsymptr		!set by lookuplex()

[0..255]char alphamap
[0..255]char digitmap
[0..255]char commentmap

global proc lex=
!lowest level lex() function, reads names, numbers etc but does no lookups or translations
!returns results in lx-vars. Current source pointer should be in lxsptr
int i, c, d, hsum, csum, length
ref byte pstart

lxsubcode:=0

doswitch c:=lxsptr++^
when 'a'..'z','$','_','.' then
	pstart:=lxsptr-1		!point to start of name in source buffer
doname::
	hsum:=csum:=c

	doswitch c:=lxsptr++^
	when 'a'..'z','0'..'9','_','$','.' then
		csum+:=c
		hsum:=hsum<<3+csum
	when 'A'..'Z' then
		(lxsptr-1)^:=c+32
		csum+:=c+32
		hsum:=hsum<<3+csum
	else
		--lxsptr
		exit
	end

	lxlength:=lxsptr-pstart
	lxhashvalue:=hsum<<5 ixor csum
!
	if lookuplex(cast(pstart),lxlength) then
		if lxsymptr^.ksymbol then			!keywords take priority here
			lxsymbol:=lxsymptr^.ksymbol
			lxsubcode:=lxsymptr^.subcode
		else
			lxsymbol:=lxsymptr^.symbol
		fi
	else
		lxsymbol:=namesym
	fi

	return

when 'A'..'Z' then
	pstart:=lxsptr-1
	c:=pstart^:=pstart^+32
	goto doname

when '0'..'9' then
	readnumber(c)
	return

when '`' then
	pstart:=lxsptr		!point to start of name in source buffer
	hsum:=csum:=0

	doswitch c:=lxsptr^
	when 'A'..'Z','a'..'z','0'..'9','_','$','.' then
		++lxsptr
		csum+:=c
		hsum:=hsum<<3+csum
	else
		exit
	end

	lxsymbol:=namesym
	if pstart=lxsptr then
		lxerror("NULL ` name")
	fi
	lxlength:=lxsptr-pstart
	lxhashvalue:=hsum<<5 ixor csum

	if lookuplex(cast(pstart),lxlength) then
		lxsymbol:=lxsymptr^.symbol			!can't be a keyword
		if lxsymbol=0 then					!assume was a keyword; use as name
			lxsymbol:=lxsymptr^.symbol:=namesym
		fi
	fi
	return

when '!',';','#' then			!comment to eol

	while commentmap[lxsptr++^] do od

	if (lxsptr-1)^=0 then --lxsptr fi
!
	++lxlineno

	lxsymbol:=eolsym
	return

when ',' then
	lxsymbol:=commasym
	return

when ':' then
	if lxsptr^=':' then
		lxsymbol:=dcolonsym
		++lxsptr
	else
		lxsymbol:=colonsym
	fi
	return

when '[' then
	lxsymbol:=lsqsym
	return

when ']' then
	lxsymbol:=rsqsym
	return

when '+' then
	lxsymbol:=addsym
	return

when '-' then
	lxsymbol:=subsym
	return

when '*' then
	lxsymbol:=mulsym
	return

when '=' then
	lxsymbol:=eqsym
	return

when '\'' then
	pstart:=lxsptr

	do
		switch lxsptr++^
		when '\'' then
			exit
		when cr,lf then
			lxerror("String not terminated")
		endswitch
	od
	length:=lxsptr-pstart-1
	lxvalue:=0
	for i:=length downto 1 do
		lxvalue:=lxvalue<<8+(pstart+i-1)^
	od
	lxsymbol:=intconstsym
	return

when '"' then
	pstart:=lxsptr

	do
		switch lxsptr++^
		when '"' then
			lxsvalue:=cast(pstart)
			lxlength:=lxsptr-pstart-1
			(lxsvalue+lxlength)^:=0
			lxsymbol:=stringconstsym
			return
		when cr,lf,etx,0 then
			lxerror("String not terminated")
		endswitch
	od

when ' ',9 then

when cr then			!lf expected to follow

when lf then
	++lxlineno
	lxsymbol:=eolsym
	return

when 0,etx then
	lxsymbol:=eofsym
	--lxsptr
	return
else
	lxsymbol:=errorsym
	lxvalue:=c
	return

end doswitch
end

global proc initlex=
lxsubcode:=0
lxsymbol:=errorsym

lxlineno:=0

int i
for i:=0 to 255 do
	switch i
	when 'A'..'Z','a'..'z','$','_','0'..'9' then
		alphamap[i]:=1
	end
	switch i
	when '0'..'9' then
		digitmap[i]:=1
	end
	commentmap[i]:=1
!	linecommentmap[i]:=1
!	spacemap[i]:=0
od

commentmap[0]:=0
commentmap[lf]:=0

!lexhashtable:=new(list,1..hstsize)

inithashtable()
!laststentry:=&lexhashtable[$+1]
end

proc readreal(ref[]char s,int slen, intlen,exponseen)=
!intstr is a string containing all digits, before and after decimal point
!intlen=0:  no decimal point, so fractional part is empty
!intlen<>0: length of integer part
!expon=1:   e/E was last char, so need to read exponent first
!expon=0:   No e/E seen, so no exponent
int i,fractlen,expon,exponsign,c,digs
int64 x

if intlen=0 or intlen=slen then
	fractlen:=0
else
	fractlen:=slen-intlen
fi

expon:=0
exponsign:=0

if exponseen then
	case c:=lxsptr++^
	when '+' then
	when '-' then
		exponsign:=1
	else
		--lxsptr
	esac

	digs:=0
	doswitch c:=lxsptr++^
	when '0'..'9' then
		expon:=expon*10+c-'0'
		++digs
	else
		--lxsptr
		exit
	end
	if digs=0 then
		lxerror("Exponent error")
	fi
	if exponsign then expon:=-expon fi
fi

expon:=expon-fractlen

lxxvalue:=0.0

for i:=1 to slen do
	c:=s^[i]
	lxxvalue:=lxxvalue*10.0+(c-'0')
od

if expon>0 then
	to expon do
		lxxvalue:=lxxvalue*10.0
	od
elsif expon<0 then
	to -expon do
		lxxvalue:=lxxvalue/10.0
	od
fi

lxsymbol:=realconstsym
end

proc readnumber(int c)=
!A digit c 0..9 has just been read. Numeric formats are::
!1234
!0x1234
!2x1101
!Nx....		possible
[256]char str
int i,d,intlen,slen

d:=lxsptr^
case d
when 'x','X' then			!other base
	case c
	when '0' then			!hex
		++lxsptr
		readhex()
		return
	when '2' then			!binary
		++lxsptr
		readbinary()
		return
	else
		cpl c
		lxerror("Base not supported")
	esac
esac

!assume decimal
str[1]:=c
slen:=1
intlen:=0

doswitch c:=lxsptr++^
when '0'..'9' then
	str[++slen]:=c
when '_','\'','`' then
when '.' then
	intlen:=slen
when 'e','E' then
	readreal(&str,slen,intlen,1)
	return
else
	--lxsptr
	exit
end

if intlen then
	readreal(&str,slen,intlen,0)
	return
fi

if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
	lxerror("Overflow in 64-bit value")
fi

lxsymbol:=intconstsym

lxvalue:=0
for i:=1 to slen do
	lxvalue:=lxvalue*10+str[i]-'0'
od
end

proc readbinary=
!positioned at start of binary seq; 0 chars read yet
int ndigs

ndigs:=0
lxvalue:=0
doswitch lxsptr++^
when '0' then
	lxvalue:=lxvalue*2
	++ndigs
when '1' then
	lxvalue:=lxvalue*2+1
	++ndigs
when '2'..'9' then
	lxerror("Bad binary digit")
when '_','\'','`' then
else
	--lxsptr
	exit
end

if ndigs=0 then
	lxerror("No bin digits")
elsif ndigs>64 then
	lxerror("Overflow in binary number")
fi
lxsymbol:=intconstsym
end

proc readhex=
!positioned at start of hex seq; 0 chars read yet
int ndigs,c

ndigs:=0
lxvalue:=0
doswitch c:=lxsptr++^
when '0'..'9' then
	lxvalue:=lxvalue*16+c-'0'
	++ndigs
when 'A'..'F' then
	lxvalue:=lxvalue*16+(c-'A'+10)
	++ndigs
when 'a'..'f' then
	lxvalue:=lxvalue*16+(c-'a'+10)
	++ndigs
when '_','\'','`' then
else
	--lxsptr
	exit
end

if ndigs=0 then
	lxerror("No hex digits")
elsif ndigs>16 then
	lxerror("Overflow in hex number")
fi
lxsymbol:=intconstsym
end

global proc ps(ichar caption)=
PRINT CAPTION,":"
PRINTSYMBOL()
end

global proc printsymbol(filehandle dev=nil)=
[256]char str

strcpy(&.str,symbolnames[lxsymbol])
str[strlen(&.str)-2]:=0
!convucstring(&str)

print @dev,&.str
to 14-strlen(&.str) do print @dev," " od

case lxsymbol
when namesym then

	print @dev,lxsymptr^.name

when intconstsym then
	print @dev, lxvalue
when realconstsym then
	print @dev, lxxvalue
when stringconstsym then
	print @dev,"""",,lxsvalue,,""""!,,"end"
when errorsym then
	print @dev,lxvalue
else
	print @dev,symbolnames[lxsymbol]
	if lxsubcode then
		print " ",,lxsubcode
	fi

end

println @dev
end

proc clearhashtable=
!if defined in zdata, then will already be all zeros
!for i:=1 to hashtable.upb do
!	lexhashtable[i]:=void
!od
end

proc inithashtable=
!initialise hash table from kwddata
[32]char str
int i

if hstsize>65536 then
!limit in place because of 16-bit-wide strec fields like .htindex
!CPL "HASH SIZE OVER 64K"
!	lxerror("hash table limited to 64K entries")
fi

clearhashtable()

for i to mclnames.len do
	addreservedword(mclnames[i]+2,kopcodesym,i)
od

for i to dregnames.len do
	addreservedword(dregnames[i],kregsym,regindices[i])
	lxsymptr^.regsize:=regsizes[i]
od


for i to xregnames.len do
	addreservedword(xregnames[i],kxregsym,i)
od

for i to fregnames.len do
	addreservedword(fregnames[i],kfregsym,i)
od

for i to mregnames.len do
	addreservedword(mregnames[i],kmregsym,i)
od

for i to jmpccnames.len do
	addreservedword(jmpccnames[i],kjmpccsym,jmpcccodes[i])
od

for i to setccnames.len do
	addreservedword(setccnames[i],ksetccsym,setcccodes[i])
od

for i to cmovccnames.len do
	addreservedword(cmovccnames[i],kmovccsym,cmovcccodes[i])
od

for i to prefixnames.len do
	addreservedword(prefixnames[i],kprefixsym,prefixsizes[i])
od

for i to segmentnames.len do
	strcpy(&.str,segmentnames[i])
	str[strlen(&.str)-3]:=0
	addreservedword(pcm_copyheapstring(&.str),ksegnamesym,i)
od

addreservedword("aframe",kregsym,r14); lxsymptr^.regsize:=4
addreservedword("dframe",kregsym,r14); lxsymptr^.regsize:=8
addreservedword("astack",kregsym,r15); lxsymptr^.regsize:=4
addreservedword("dstack",kregsym,r15); lxsymptr^.regsize:=8
addreservedword("dprog",kregsym,r8); lxsymptr^.regsize:=8
addreservedword("dsptr",kregsym,r9); lxsymptr^.regsize:=8
end

proc addreservedword(ichar name,int symbol,subcode)=
lxhashvalue:=gethashvalue(name)
if lookuplex(name,0) then
	cpl =name
	lxerror("DUPL NAME")
fi

lxsymptr^.symbol:=0
lxsymptr^.ksymbol:=symbol
lxsymptr^.subcode:=subcode
end

global proc printhashtable(filehandle devx,ichar caption)=
ref strec r
int count,i

println @devx,caption,":"
count:=0
for i:=0 to lexhashtable.upb do
	r:=lexhashtable[i]
!	r:=lexhashtable[i]
	if R AND r^.name then
		count+:=1

	fi
od
println @devx,count," items in table",hstsize
end

function lookuplex(ichar name,int length=0)int=
!name is either in an existing table (for reserved words; length=0)
!or is in the source code (so is not zero-terminated; length is actual length)
!look for name in lexhashtable
!sets lxsymptr to entry in table, either of existing entry, or a new one
!returns 1/0 if found/not found (ie. old or new name)
ref strec e

int j,wrapped,insource,firstj

insource:=length
if length=0 then
	length:=strlen(name)
fi

firstj:=j:=(lxhashvalue iand hstmask)		!j=initial hash index

wrapped:=0

do
	lxsymptr:=lexhashtable[j]
	if lxsymptr=nil then				!unused entry, not found
		exit
	fi

	if lxsymptr^.namelen=length and memcmp(lxsymptr^.name,name,length)=0 then			!match
		return 1
	fi
	if ++j>hstsize then		!wraparound
		if wrapped then
			println "???????HASHTABLE FULL",hstsize,lxlineno
			stop 1
		fi
		wrapped:=1
		j:=1
	fi
od

!name not found
if insource then
	name:=makestring(name,length)
fi

if lxsymptr=nil then
	lxsymptr:=pcm_allocz(strec.bytes)
	lexhashtable[j]:=lxsymptr
fi

lxsymptr^.name:=name
lxsymptr^.namelen:=length
lxsymptr^.symbol:=namesym
lxsymptr^.ksymbol:=0
lxsymptr^.htindex:=j
lxsymptr^.htfirstindex:=firstj
lxsymptr^.moduleno:=currmoduleno
return 0
end

global proc initsourcefile(ichar source)=
lxstart:=lxsptr:=cast(source)
lxlineno:=1
end

global function addnamestr(ichar name)ref strec=
!add a new name to the symbol table
!return symptr to new (or existing) generic name
lxhashvalue:=gethashvalue(name)
lookuplex(pcm_copyheapstring(name),0)
return lxsymptr
end

global proc lxerror(ichar m)=			!LXERROR
![256]char str

!println "\w\w*** Lexical Error ***"

!sprintf(&.str,"*** %s *** on line %d",m,lxlineno)
!println &.str

fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno

stop 1
end

function gethashvalue(ref char s)int=
int hsum, csum, c

hsum:=csum:=0

while c:=s++^ do
	csum+:=c
	hsum:=hsum<<3 + csum
od
return hsum<<5 ixor csum
end

global proc skiptoeol=
!read lex tokens until eol and consume it
!return entire line as string
!note, exit with lxsptr pointing at the cr (or lf) char
repeat
	lex()
until lxsymbol=eolsym or lxsymbol=eofsym
END

function makestring(ichar p,int length)ref char=
!turn counted/non-terminated string from any source, into independent heap string
ref char s

s:=pcm_alloc(length+1)
memcpy(s,p,length)
(s+length)^:=0
return s
end
=== ax_parse.m 22/73 ===
import clib
import mlib
import ax_tables
import ax_decls
import ax_lex
import ax_lib

fwdrec dummy1

ref strec exprlabeldef
int64 exprvalue
int exprtype

global proc readmodule(int moduleno)=
ref strec symptr
int sym

initsourcefile(moduletable[moduleno].source)

lxsymbol:=eolsym

genmc(m_segment,genint(code_seg))

while lxsymbol=eolsym do

	lex()

	switch lxsymbol
	when kopcodesym then
		readinstr()

	when namesym then
		symptr:=lxsymptr
		lex()
		sym:=lxsymbol
!		lex()
		case sym
		when eqsym then
			lex()
			createnamedconst(symptr,readexpression())
		when colonsym,dcolonsym then
			createlabel(symptr,(sym=colonsym|localsym|exportedsym))
			genmc(m_label, genlab(symptr))
			symptr^.reftype:=fwd_ref
			lxsymbol:=eolsym
			redo
		else
			println symptr^.name
			serror("colon expected after label")
		esac

	when fwdlocalsym then
		symptr:=lxsymptr
		lex()
		case lxsymbol
		when eqsym then
			serror_s("Redefining label as const: %s",symptr^.name)
		when colonsym,dcolonsym then
			symptr^.fwdrefs:=nil
			genmc(m_label, genlab(symptr))
			symptr^.symbol:=(lxsymbol=colonsym|localsym|exportedsym)
			symptr^.reftype:=fwd_ref
			lxsymbol:=eolsym
			redo
		else
			serror("Instruction expected")
		esac

	when importedsym then
		serror_s("Defining imported name: %s",symptr^.name)
	when localsym, exportedsym then
		serror_s("Redefining symbol: %s",symptr^.name)
	when namedconstsym then
		serror_s("2:Const redefined: %s",symptr^.name)

	when kjmpccsym then
		readcondinstr(m_jmpcc)

	when ksetccsym then
		readcondinstr(m_setcc)

	when kmovccsym then
		readcondinstr(m_cmovcc)

	when eolsym then			!blank or comment line
	when eofsym then
!		println "EOF"
		return
	else
		println "Unknown symbol:",symbolnames[lxsymbol]
	end switch
od
serror("EOL expected")
end

global proc checkundefined=
int i
ref strec d

!for i:=1 to lexhashtable.len do
!	d:=lexhashtable[i]
!	if d and d^.namelen and d^.symbol=fwdlocalsym then
!		println "Undefined:",padstr(d^.name,20)!,"Line:",d^.lineno
!		++nundefined
!	fi
!od
d:=modulenamelist
while d do
	if d^.symbol=fwdlocalsym then
		println "Undefined:",padstr(d^.name,20)
		++nundefined
	fi
	d:=d^.nextdef
od
end

proc checksymbol(int symbol)=
[265]char str

if lxsymbol<>symbol then
!	sprintf(&.str,"%s expected not %s",symbolnames[symbol],symbolnames[lxsymbol])
	fprint @&.str,"# expected not #",symbolnames[symbol],symbolnames[lxsymbol]

	serror(&.str)
fi
end

proc readinstr=
!deal with opcode symbol
int opcode
ref opndrec a,b,c

	opcode:=lxsubcode

	lex()

	switch opcode
	when m_db, m_dw, m_dd, m_dq, m_ddoffset then
		do
			if lxsymbol=stringconstsym then
				a:=genstrimm(lxsvalue)
				lex()
				genmc(opcode,a)
			else
				a:=readoperand()
				genmc(opcode,a)
			fi
			if lxsymbol=commasym then
				lex()
			else
				exit
			fi
		od
	when m_segment then
		checksymbol(ksegnamesym)
		genmc(m_segment,genint(lxsubcode))
!		currseg:=lxsubcode					!used to assign segments to labels
		lex()

	when m_isegment then
		genmc(m_segment, genint(idata_seg))
	when m_zsegment then
		genmc(m_segment, genint(zdata_seg))
	when m_csegment then
		genmc(m_segment, genint(code_seg))

	when m_imul3 then
		a:=readoperand()
		checksymbol(commasym)
		lex()
		b:=readoperand()
		checksymbol(commasym)
		lex()
		c:=readoperand()
!		genmc(opcode,a,b,c)
		SERROR("IMUL3 CAN'T DO 3 OPNDS")
!		genmc(opcode,a,b)

	when m_proc then
		repeat
			lex()
		until lxsymbol=eolsym

!	when m_call then
!		a:=readoperand()
!		if a.mode=a_imm then
!			genmc(opcode,a)
!		else
!			genmc(m_call2,a)
!		fi
	else
		a:=b:=nil
		if lxsymbol<>eolsym then
			a:=readoperand()
			if lxsymbol=commasym then
				lex()
				b:=readoperand()
			fi
		fi 

		genmc(opcode,a,b)
	end

!checkeol()
!println "INSTRUCTION:",=lxsymptr
end

proc readcondinstr(int opc)=
ref opndrec a,b

a:=genint(lxsubcode)
lex()
b:=readoperand()

if lxsymbol=commasym and opc=m_cmovcc then		!ignore dest
!	unless b.mode=a_reg and b.reg=r0 then
!		gerror("Can't do two-operand cmov")
!	end
	genmc(m_param,b)							!store extra param as separate instr

	lex()
	b:=readoperand()
fi

genmc(opc,a,b)
end

function readoperand:ref opndrec=
!position at start of operand
!read reg, expression or complex operand, and return an opndrec
ref opndrec p
int size

	switch lxsymbol
	when kregsym then
		p:=regtable[lxsubcode, lxsymptr^.regsize]
		lex()
		return p
	when lsqsym then
		lex()
		return readaddrmode(0)
	when kxregsym then
		p:=genxreg(lxsubcode)
		lex()
		return p
	when kprefixsym then
		size:=lxsubcode
		lex()
		checksymbol(lsqsym)
!		if lxsymbol<>lsqsym then
!			serror("[ expected")
!		fi
		lex()
		return readaddrmode(size)

	else
!		if lxsymbol=intconstsym and lxsptr^=13 then
!			p:=genint(lxvalue)
!			lex()
!			return p
!		fi

		return readexpression()
	end
	return nil
end

function readexpression:ref opndrec=
ref strec labelx
int64 valuex
int typex

	readterm()

	docase lxsymbol
	when addsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("add real") fi
		exprvalue+:=valuex

	when subsym then
		labelx:=exprlabeldef
		valuex:=exprvalue
		typex:=exprtype
		lex()
		readterm()
		if exprlabeldef then serror("+label?") fi
		exprlabeldef:=labelx
		if typex or exprtype then serror("sub real") fi
		exprvalue:=valuex-exprvalue
	else
		exit
	end

	return genimm_expr(exprlabeldef,exprvalue,exprtype)
end

proc readterm=
!read term into exprlabeldef/exprvalue
ref strec symptr
real x
	exprlabeldef:=nil
	exprvalue:=0
	exprtype:=0

	switch lxsymbol
	when fwdlocalsym, localsym, exportedsym then
		exprlabeldef:=lxsymptr
		lex()
		if lxsymbol=mulsym then		!is extern name
			serror("* applied to non-extern label or applied inconsistently")
		fi

	when importedsym then
		exprlabeldef:=lxsymptr
		lex()
		if lxsymbol<>mulsym then		!is extern name
			serror("* missing or applied inconsistently")
		fi
		lex()

	when namedconstsym then
		exprlabeldef:=lxsymptr^.expr^.labeldef
		exprvalue:=lxsymptr^.expr^.value
		exprtype:=lxsymptr^.expr^.valtype

!		p:=lxsymptr^.value
		lex()
	when namesym then
		symptr:=lxsymptr
		exprlabeldef:=symptr
		lex()
		if lxsymbol=mulsym then		!is extern name
			createlabel(symptr,importedsym)
			lex()
		else
			createlabel(symptr,fwdlocalsym)
		fi

	when intconstsym then
		exprvalue:=lxvalue
		lex()
	when realconstsym then
		exprvalue:=int64@(lxxvalue)
		exprtype:='R'
		lex()

	when subsym then
		lex()
		readterm()
		if not exprlabeldef then
			if not exprtype then
				exprvalue:=-exprvalue
			else
				x:=-(real@(exprvalue))
				exprvalue:=int64@(x)
			fi
		else
			serror("neg/label")
		fi
	when addsym then
		lex()
		readterm()

	else
		serror("READTERM")
	end
end

proc readreg(int &reg,&regsize,&scale)=
!positioned at reg symbol
!read R or R*n for address mode
!return (reg, regsize, scale); scale is 0 when no *n

reg:=lxsubcode
regsize:=lxsymptr^.regsize
lex()
if lxsymbol=mulsym then
	lex()
	checksymbol(intconstsym)
	case lxvalue
	when 1,2,4,8 then
	else
		serror("*n must be 1,2,4,8")
	esac
	scale:=lxvalue
	lex()
else
	scale:=0
fi
end

function readaddrmode(int size)ref opndrec=
![" just seen, positioned at next symbol
!syntax: [Reg+Reg*scale+expr], all items optional, but at least one must be present
!read optional reg, index reg, scale factor, label, and offset
!size is 0, or is 1,2,4,8 when an override was used
int reg,regsize,scale,regix, addrsize, regixsize, scaleix
ref opndrec x
ref opndrec p

reg:=regix:=0
regsize:=regixsize:=0
scale:=scaleix:=0
x:=nil

if lxsymbol=kregsym then
	readreg(reg,regsize,scale)	
!	n:=1
	case lxsymbol
	when addsym then
		lex()
		if lxsymbol=kregsym then
			readreg(regix,regixsize,scaleix)	

			case lxsymbol
			when addsym,subsym then
				x:=readexpression()
			esac

		else
			x:=readexpression()
		fi
	when subsym then
		x:=readexpression()
	esac
else
	x:=readexpression()
fi

if scale and scaleix then serror("Two *N scales") fi
if reg=0 and regix=0 and 0 then	serror("Empty address mode") fi
checksymbol(rsqsym)
lex()

if scale and not scaleix then
	swap(reg,regix)
	swap(regsize,regixsize)
	swap(scale,scaleix)
fi
if scaleix=0 then scaleix:=1 fi

if regsize and regixsize and regsize<>regixsize then serror("Addr reg size mismatch") fi

!addrsize:=(regsize=4 or regixsize=4|4|8)

p:=genindex(areg:reg, ireg:regix, scale:scaleix, x:x, size:size,
	addrsize:(regsize=4 or regixsize=4|4|8))
return p
end
=== ax_lib.m 23/73 ===
import clib
import msys
import mlib
import ax_tables
import ax_decls
import ax_lex

const ptrsize=8

fwdrec dummy1
!valuerec dummy2

global tabledata() [0:]ichar opndnames =
	(a_none=0,	$),
	(a_reg,		$),
	(a_imm,		$),
 	(a_mem,		$),		!any memory modes: [d], [R], [R*4+R2+d+imm] etc
 	(a_cond,	$),		!a condition code for jcc/setcc
	(a_xreg,	$),		!xmm register
	(a_string,	$),		!immediate string (for comments)
end

!global type opndrec = record			!24 bytes
!	ref strec labeldef	!nil, or handle of strec for label
!	union
!		int64 value		!const value/extra offset/cond code/string for comments
!		real64 xvalue	!floating point value
!		ref char svalue
!	end
!	byte mode		!a_reg etc, low level operand details
!	byte size		!byte size of operand: 1,2,4,8
!	byte reg		!0, or main register
!	byte regix		!0, or index register
!
!	byte scale		!0, or scale factor for regix
!	byte addrsize	!4 or 8 for a_mem when regs are involved
!	byte valtype	!0 (no value or int) or 'R'/'S'
!	byte spare2
!end

global record mclrec =		!64 bytes
	ref mclrec nextmcl
	ref opndrec a,b
	int opcode
	int lineno
end

!!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!!is needed at strategic points to ensure that are at least n bytes left
!global type dbuffer = record
!	ref byte pstart
!	union
!		ref byte pcurr
!		ref word16 pcurr16
!		ref word32 pcurr32
!		ref word64 pcurr64
!	end
!	ref byte pend
!	int alloc
!end

global int currsegment=0		!

global opndrec dstackopnd
global opndrec dframeopnd

global int labelno=0
global ref opndrec zero_opnd=nil

global ref mclrec mccode, mccodex

strbuffer destv
global ref strbuffer dest=&destv

global [r0..r19, 1..8]ref opndrec regtable

global proc initlib=
zero_opnd:=genint(0)

int reg,size

for reg:=r0 to r15 do
	for size:=1 to 8 do
		case size
		when 1,2,4,8 then
			regtable[reg,size]:=genreg0(reg,size)
		esac
	od
od	
for reg:=r16 to r19 do
	regtable[reg,1]:=genreg0(reg,1)
od	

ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
ss_symboltablesize:=init_ss_symbols
ss_nsymbols:=0

end

global proc genmc(int opcode,ref opndrec a=nil,b=nil)=	!GENMC
ref mclrec m
int nopnds

!	m:=pcm_allocz(mclrec.bytes)
!	m:=pcm_allocz(mclrec.bytes)
	m:=pcm_alloc(mclrec.bytes)
++NMCLASM

	m^.nextmcl:=nil

!CPL "SET MLINENO",symbolnames[lxsymbol],mclnames[opcode]
	if lxsymbol=eolsym then
		m^.lineno:=lxlineno-1
	else
		m^.lineno:=lxlineno
	fi

	m^.opcode:=opcode

	nopnds:=(a=nil|0|(b=nil|1|2))

	if nopnds<mclnopnds[opcode] then
		serror("Too few operands")
	elsif nopnds>mclnopnds[opcode] then
		serror("Too many operands")
	fi

	m^.a:=a
	m^.b:=b

	if mccode then
		mccodex^.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

global proc genmcstr(int opcode,ichar s)=	!GENMCSTR
!as genmc but uses a single immediate string operand

genmc(opcode,genstrimm(s))
end

function newopnd(int mode)ref opndrec=
ref opndrec a

++NMCLOPNDSASM

!a:=pcm_allocz(opndrec.bytes)
a:=pcm_allocz(opndrec.bytes)
a^.mode:=mode
return a
end

global function genxreg(int xreg)ref opndrec=		!GENXREG
ref opndrec a

a:=newopnd(a_xreg)
a^.reg:=xreg
a^.size:=16
return a
end

global function genindex(int areg=0,ireg=0,scale=1,ref opndrec x=nil,int size=0,addrsize=8)ref opndrec=		!GENINDEX
!construct a mem address mode
ref opndrec a

if x then							!existing operand filled in with value
!	a:=genmem_expr(x)				!fill in label and/or offset
	a:=x
	x^.mode:=a_mem
else
	a:=newopnd(a_mem)
fi

a^.reg:=areg
a^.regix:=ireg
a^.scale:=scale
a^.size:=size
a^.addrsize:=addrsize
return a
end

global function writemclblock:ref strbuffer=		!WRITEMCLBLOCK
int i
ref mclrec m

gs_init(dest)

gs_strln(dest,"MC CODE")

m:=mccode
i:=1

while m do
	writemcl(i,m)
	m:=m^.nextmcl
	++i
od
return dest			!only used when initstr=1, otherwise caller ignores
end

global proc gencomment(ichar s=nil)=			!GENCOMMENT
if s=nil then
	genmc(m_blank)
else
	genmcstr(m_comment,s)
fi
end

global function genstrimm(ichar s)ref opndrec=			!GENSTRIMM
ref opndrec a
a:=newopnd(a_string)
a^.svalue:=s
return a
end

function getsizetag(int size)ichar=			!GETSIZETAG
case size
when 1 then return "b"
when 2 then return "h"
when 4 then return "w"
when 8 then return "d"
esac
GERROR("GETSIZETAG?")
!return tostr(size)
return nil
end

proc writemcl(int index,ref mclrec mcl)=			!WRITEMCL
[512]char mclstr
[512]char str
ichar semi

strcpy(&.mclstr,strmcl(mcl))
if mclstr[1]=0 then return fi

case mcl^.opcode
when m_comment then
	semi:=";"
else
	semi:=" "
esac

!sprintf(&.str,"%03d %04d ",semi,index, mcl^.lineno)
print @&.str,semi:"z3",index:"z4",," "!, mcl^.lineno

gs_str(dest,&.str)
gs_strln(dest,&.mclstr)
end

global function strmcl(ref mclrec mcl)ichar=			!STRMCL
static [512]char str
int opcode,sizepref

opcode:=mcl^.opcode

case opcode
when m_assem then
	return mcl^.a^.svalue
when m_blank then
	return ""
when m_comment then
!	if fshowcomments then
		strcpy(&.str,";")
		strcat(&.str,mcl^.a^.svalue)
		return &.str
!	fi
!when m_bsource then
!	strcpy(&str,";")
!	strcat(&str,mcl^.a.svalue)

!when m_labelname then
!	strcpy(&str,mcl^.a.svalue)
!	strcat(&str,":")
!	return &str

when m_label then
	strcpy(&.str,mcl^.a^.labeldef^.name)
	strcat(&.str,":")
	return &.str

esac

strcpy(&.str,"		")

case opcode
when m_jmpcc then
	strcat(&.str,"j")
	strcat(&.str,condnames[mcl^.a^.value])

when m_setcc then
	strcat(&.str,"set")
	strcat(&.str,condnames[mcl^.a^.value])
when m_cmovcc then
	strcat(&.str,"cmov")
	strcat(&.str,condnames[mcl^.a^.value])
else
	strcat(&.str,mclnames[opcode]+2)
esac

ipadstr(&.str,12)

!s+:=tab+tab+leftstr(opcname,10)

if mcl^.a and mcl^.b then		!2 operands
	sizepref:=needsizeprefix(mcl^.opcode,mcl^.a,mcl^.b)

	strcat(&.str,stropnd(mcl^.a,sizepref))
	strcat(&.str,",	")
	strcat(&.str,stropnd(mcl^.b,sizepref))

elsif mcl^.a then								!1 operand
	if mcl^.opcode=m_call then
		strcat(&.str,stropnd(mcl^.a,0))
	else
		strcat(&.str,stropnd(mcl^.a,1))
	fi
!else
!	opnds:=""
fi

!s+:=opnds

return &.str
end

global function stropnd(ref opndrec a,int sizeprefix=0)ichar=			!STROPND
static [256]char str
ichar plus,s
int64 value
ref strec d

case a^.mode
when a_reg then
	return getregname(a^.reg,a^.size)
when a_imm then
!	return STRVALUE(A^.LABELDEF,A^.VALUE)
	d:=a^.labeldef
	value:=a^.value
	if d then
		if d^.symbol=namedconstsym then
			return inttostr(d^.expr^.value)
		fi

!		s:=d^.name
		s:=GETFULLNAME(d)

		if value then
			if value>0 then
				strcpy(&.str,s)
				strcat(&.str,"+")
				strcat(&.str,inttostr(value))
			else
				strcpy(&.str,s)
				strcat(&.str,inttostr(value))
			fi
			return &.str
		else
			strcpy(&.str,s)
			return &.str
!			return s
		fi
	fi
	if a^.valtype=0 then
		return inttostr(value)
	else
		return realtostr(real@(value))
	fi

when a_mem then
	str[1]:=0
	strcat(&.str,getsizeprefix(a^.size,sizeprefix))
	strcat(&.str,"[")
	plus:=""

	if a^.reg then
		strcat(&.str,getregname(a^.reg,a^.addrsize))
		plus:="+"
	fi

	if a^.regix then
		strcat(&.str,plus)
		strcat(&.str,getregname(a^.regix,a^.addrsize))
		plus:="+"
		if a^.scale>1 then
			strcat(&.str,"*")
			strcat(&.str,inttostr(a^.scale))
		fi
	fi

	if a^.labeldef then
		strcat(&.str,plus)
		strcat(&.str,strdef(a^.labeldef))
		plus:="+"
	fi

	if a^.value>0 then
		strcat(&.str,plus)
		strcat(&.str,inttostr(a^.value))
	elsif a^.value<0 then
		strcat(&.str,inttostr(a^.value))
	fi

	strcat(&.str,"]")
when a_string then
	if strlen(a^.svalue)>=str.len then
!		sprintf(&.str,"\"%s\"","<Long string>")
		print @&.str,"""<Long string>"""
	else
!		sprintf(&.str,"\"%s\"",a^.svalue)
		print @&.str,"""",,a.svalue,,""""
	fi

when a_cond then
	return opndnames[a^.value]

when a_xreg then
	return xgetregname(a^.reg)

else
	return "<BAD OPND>"
esac

return &.str
end

function strdef(ref strec def)ichar=			!STRDEF
if def^.symbol=namedconstsym then
	return inttostr(def^.expr^.value)
fi
return getfullname(def)
end

global proc setsegment(int seg)=		!SETSEGMENT
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
if seg=currsegment then
	return
fi
case seg
when 'D' then genmcstr(m_segment,".data")
when 'Z' then genmcstr(m_segment,".bss")
when 'C' then genmcstr(m_segment,".text")
when 'R' then genmcstr(m_segment,".rodata")
esac
currsegment:=seg
!currzdataalign:=curridataalign:=0
end

function getsizeprefix(int size,enable=0)ichar=		!GETSIZEPREFIX
if not enable then return "" fi
case size
when 1 then return "byte "
when 2 then return "word "
when 4 then return "dword "
when 8 then return "qword "
!when 0 then return "<no size> "
when 0 then return ""
esac
return "N:"
end

function needsizeprefix(int opcode,ref opndrec a,b)int=		!NEEDSIZEPREFIX

case opcode
when m_movsx,m_movzx then
	return 1
when m_cvtsi2ss,m_cvtsi2sd then
	return 1
esac

if a^.mode=a_reg or a^.mode=a_xreg or b^.mode=a_reg or b^.mode=a_xreg then
	return 0
fi
return 1
end

global function genimm_expr(ref strec d, int64 value, int t, size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
ref opndrec a

a:=newopnd(a_imm)
a^.size:=size

a^.labeldef:=d
a^.value:=value
a^.valtype:=t

return a
end

global function genint(int64 x,int size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
ref opndrec a

!IF X=0 THEN ++NZEROS FI

a:=newopnd(a_imm)
a^.size:=size
a^.value:=x

return a
end

global function genlab(ref strec d,int size=4)ref opndrec=
!generate immediate operand
!x is valuerec from an expression, or direct strec (for label) or int
ref opndrec a

a:=newopnd(a_imm)
a^.size:=size
a^.labeldef:=d

return a
end

global function genmem(ref strec d,int size=4)ref opndrec=
!simple memory operand without registers
ref opndrec a

a:=genlab(d,size)
a^.mode:=a_mem
return a
end

global function genreg0(int reg,size=4)ref opndrec=	!GENREG

ref opndrec a
a:=newopnd(a_reg)
a^.reg:=reg
a^.size:=size
return a
end

global function getfullname(ref strec d)ichar=
static [256]char str
ichar ms

ms:=""
if d^.basedef then
	ms:=d^.basedef^.name
fi

!sprintf(&.str,"<%s : #%d &:%8p SYM:%.*s M:%s>",
!	d^.name,d^.moduleno,d,
!	strlen(symbolnames[d^.symbol])-3,symbolnames[d^.symbol],
!	ms)

fprint @&.str,"<# : ## &:# SYM:## M:#>",
	d^.name,"#",d^.moduleno,d:"8",
	strlen(symbolnames[d^.symbol])-3:"v",symbolnames[d^.symbol]:".*", ms

return &.str
return d^.name
end

global function getregname(int reg,size=4)ichar=
ichar prefix,rs
static [32]char str

case reg
when rnone then return "-"
when rframe then rs:="frame"
when rstack then rs:="stack"
!when r16..r19 then
!	rs:=(reg-r15|"0H","1H","10H","11H"|"?")

else
	rs:=inttostr(reg-r0)
esac

case size
when 1 then prefix:="B"
when 2 then prefix:="W"
when 4 then prefix:="A"
else prefix:="D"
esac

strcpy(&.str,prefix)
strcat(&.str,rs)
return &.str
end

global function xgetregname(int reg)ichar=
static [16]char str

!sprintf(&.str,"xmm%d",reg-r0)
print @&.str,"xmm",,reg-r0

return &.str
end

global proc printst(filehandle f)=
ref strec r
int count,i

r:=modulenamelist
while r do
	printstrec(f,r)
	r:=r^.nextdef
od

end

global proc printstrec(filehandle f,ref strec d)=
const w=16

case d^.symbol
when fwdlocalsym, localsym, exportedsym then
	println @f,"Label:       ",padstr(d^.name,w),(d^.scope=fwd_ref|"U"|"-"),
		symbolnames[d^.symbol],,"\T",,
	padstr((d^.segment|segmentnames[d^.segment]|"no seg"),12),
		d^.offset, d^.fwdrefs
when importedsym then
	println @f,"Label:       ",padstr(d^.name,w),"EXTERN"

when namedconstsym then
	println @f,"Named const: ",padstr(d^.name,w),"=",stropnd(d^.expr)
else
	println @f,"??"
esac
end

global proc adddef(ref strec d)=
d^.nextdef:=modulenamelist
modulenamelist:=d
end

global proc addimport(ref strec d)=
ref stlistrec p

p:=pcm_alloc(stlistrec.bytes)
p^.def:=d
p^.nextitem:=globalimportlist
globalimportlist:=p
end

global proc createlabel(ref strec symptr,int symbol)=
!symptr is a generic st entry
symptr^.symbol:=symbol
symptr^.stindex:=0
symptr^.moduleno:=currmoduleno
adddef(symptr)
end

global proc createnamedconst(ref strec symptr,ref opndrec expr)=
symptr^.symbol:=namedconstsym
symptr^.expr:=expr
!symptr^.stindex:=0
!symptr^.moduleno:=currmoduleno
adddef(symptr)
end

global proc gerror(ichar mess)=
println "SS code gen error:",mess
println "On line:", alineno
println
stop 1
end

global proc serror(ichar mess)=
println "Syntax error: '",,mess,,"' on line",lxlineno,moduletable[currmoduleno].name
stop 1
end

global proc serror_s(ichar mess, param)=
[256]char str
sprintf(&.str,mess, param)
serror(&.str)
end

function inttostr(int64 a)ichar=
static [64]char str

!sprintf(&.str,"%lld",a)
getstrint(a,&.str)
return &.str
end

function realtostr(real a)ichar=
static [64]char str
!sprintf(&.str,"%f",a)
strcpy(&.str,strreal(a))
return &.str
end

global function buffercreate(int size=1024)ref dbuffer=
ref dbuffer a

a:=pcm_alloc(dbuffer.bytes)

a^.alloc:=size
a^.pstart:=a^.pcurr:=pcm_alloc(a^.alloc)
a^.pend:=a^.pstart+a^.alloc
return a
end

proc bufferexpand(ref dbuffer a)=
int newalloc,usedbytes
ref byte p

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

!global proc buffercheck(ref dbuffer a,int n=1024)=
global proc buffercheck(ref dbuffer a,int n=1024)=
while a^.pend-a^.pcurr<n do
	bufferexpand(a)
od
end

global function bufferlength(ref dbuffer a)int=
return a^.pcurr-a^.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=
!IF OFFSET>=BUFFERLENGTH(A) THEN
!	GERROR("BUFFERELEMPTE/OVERFLOW")
!FI

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

global proc printmodulesymbols(filehandle f)=
[256]char str
ref strec d,e

	println @f,"MODULE SYMBOLS IN",moduletable[currmoduleno].name

	d:=modulenamelist

	while d do
		print @f,"   ",,padstr(d^.name,14),padstr(symbolnames[d^.symbol],12)

!		sprintf(&.str,"|| %6d %6d %8X",d^.htfirstindex,d^.htindex,d)

!		fprint @&.str,"|| # # #",d.htfirstindex:"6",d^.htindex:"6",d:"8H"
!		print @f,&.str

		fprint @f,"|| # # #",d.htfirstindex:"6",d^.htindex:"6",d:"8H"

		e:=dupltable[d^.htfirstindex]
		if e then
			print @f,"||"
			while e do
				print @f,"(",,e^.name,,")"
				e:=e^.nextdupl
			od
		fi
		println @f," BASE:",(d^.basedef|d^.basedef^.name|""),d^.basedef
		d:=d^.nextdef
	od
	println @f
end

global proc printimportsymbols(filehandle f)=
ref strec d,e
ref stlistrec p

	println @f,"GLOBAL IMPORT TABLE",globalimportlist

	p:=globalimportlist

	while p do
		d:=p^.def
		print @f,"   ",,padstr(d^.name,14),padstr(symbolnames[d^.symbol],12)
		println @f,=d^.offset,reftypenames[d^.reftype],ref void(d)
		p:=p^.nextitem
	od
	println @f
end

global proc printdupltable(filehandle f)=
[256]char str
ref strec d,e
ref stlistrec p
int i

println @f,"DUPL TABLE"

for i:=0 to dupltable.upb when dupltable[i] do
	d:=dupltable[i]

	print @f,"	",d^.htfirstindex,,":"
	while d do
!		sprintf(&.str,"(%6d %s (%s) %8X) ",d^.htindex,d^.name,
!				moduletable[d^.moduleno].name,d)
!		print @f,&.str

		fprint @&.str,"(# # (#) #) ",d.htindex:"6",d.name,
				moduletable[d.moduleno].name,d:"8H"

		d:=d^.nextdupl
	od
	println @f
od
println @f
end

=== ax_genss.m 24/73 ===
import clib
import mlib
import oslib
import ax_tables
import ax_decls
import ax_lex
import ax_lib
import ax_objdecls

const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

int rex
int sizeoverride					!32=>16 switch
int addroverride					!32=>16 switch
int f2override						!xmm regs
int f3override						!xmm regs

ref opndrec extraparam

int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
ref relocrec currrelocs
int nrelocs

ref mclrec currmcl


global proc genss=
int index
ref mclrec m

!CPL "GENSS"

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
	alineno:=m^.lineno

!CPL "INSTR",MCLNAMES[M.OPCODE],ALINENO

	doinstr(m,++index)
	m:=m^.nextmcl
od

switchseg(0)					!update ss_currrelocs etc

if bufferlength(ss_zdata) then
	gerror("Zdata contains code or data")
fi

end

proc doinstr(ref mclrec m,int index)=
ref opndrec a,b
ref strec d,e
int x,offset,shortjmp,n

CURRMCL:=M
buffercheck(currdata)

rex:=sizeoverride:=addroverride:=f2override:=f3override:=0

a:=m^.a
b:=m^.b

switch m^.opcode
when m_label then
	d:=a^.labeldef

	d^.reftype:=back_ref
	d^.segment:=currseg
	d^.offset:=getcurrdatalen(6)

	if d^.symbol=exportedsym then
		getstindex(d)
	fi

	dofwdrefs(d)

when m_call then
	do_call(a)

when m_jmp then
	do_jmp(a,m)

when m_jmpcc then
	offset:=getrel32(b^.labeldef,getcurrdatalen(7)+1)
	if offset<0 then			!backjump
		if offset<-126 then
			genbyte(0x0F)
			genbyte(0x80+a^.value)
			gendword(offset-4)
		else
			genbyte(0x70+m^.a^.value)
			genbyte(offset)
		fi
	else
		shortjmp:=checkshortjump(m,b^.labeldef)
		if not shortjmp then
			genbyte(0x0F)
			genbyte(0x80+a^.value)
			genrel32(b)
		else
			genbyte(0x70+a^.value)
			genrel8(b)
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
!	genopnd(a,4)
	genrel32(a)

when m_segment then
	switchseg(a^.value)
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
	if a^.mode<>a_imm then gerror("retn?") fi
	genbyte(0xC2)
	genword(a^.value)

when m_push then
	do_push(a)

when m_pop then
	do_pop(a)

when m_inc, m_dec then
!CPL "INC/DEC"
	do_inc(a,mclcodes[m^.opcode])

when m_neg, m_not, m_mul, m_imul, m_div, m_idiv then
	do_neg(a,mclcodes[m^.opcode])

when m_add, m_sub, m_and, m_or, m_xor, m_adc, m_sbb, m_cmp then
	do_arith(a,b, mclcodes[m^.opcode])

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
		gerror("resb?")
	fi

when m_align then
	if a^.mode=a_imm then
		x:=a^.value
!		if x not in 1..16384 then gerror("align2") fi
		if x<1 or x>16384 then gerror("align2") fi
		if currseg<>zdata_seg then
			while bufferlength(currdata) rem x do genbyte((currseg=code_seg|0x90|0)) od
		else
			while ss_zdatalen rem x do	++ss_zdatalen od
		fi
	else
		gerror("align?")
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
	do_setcc(a,b)

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
	do_cmovcc(a,extraparam,b)

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

when m_bsf, m_bsr then
	do_bsf(a,b,mclcodes[m.opcode])

else
	println "*** CAN'T DO OPCODE",mclnames[m^.opcode],"line",alineno
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
ref char s
int64 x
int length

if size=0 then size:=a^.size fi

switch a^.mode
when a_imm,a_mem then
when a_string then
	s:=a^.svalue
	length:=strlen(s)
	if length>100 then
		buffercheck(currdata,max(1024,length+1))
	fi
	while s^ do
		genbyte(s++^)
	od
	return
else
	gerror("GENOPND/bad opnd")
endswitch

if a^.labeldef and size<=2 then
	gerror("8/16-BIT RELOC")
fi

case size
when 1 then
	genbyte(a^.value)
when 2 then
	genword(a^.value)
when 4 then
	if a^.labeldef then
		genabs32(a)
	else
		x:=a^.value
		if a^.valtype then		!was real
			gendword(getr32bits(x))
		else
			gendword(x)
		fi
	fi
when 8 then
	if a^.labeldef then
		genabs64(a)
	else
		x:=a^.value
		if a^.valtype then
			genqword(int64@(x))
		else
			genqword(x)
		fi
	fi
esac
end

proc addrelocitem(int reloctype, ref strec d)=
ref relocrec r
int stindex, adjust

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
if d^.stindex=0 then
	if ss_nsymbols>=ss_symboltablesize then
		extendsymboltable()
!		cpl ss_nsymbols
!		gerror("genss: too many symbols")
	fi
	d^.stindex:=++ss_nsymbols
	ss_symboltable^[d^.stindex]:=d
fi
return d^.stindex
end

proc genrel32(ref opndrec a)=
!used by call/longjmp/ddoffset
ref strec d

d:=a^.labeldef

if d=nil then				!constant
	gendword(a^.value)
	return
fi

case d^.reftype
when back_ref then
	if d^.segment<>currseg then
		gerror("Rel label across segments")			!might be Ok if treated as external?
	fi
	gendword(d^.offset-(getcurrdatalen(2)+4))
when fwd_ref then
!CPL "GENREL32/FWD"
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(3),rel32_rel)
	gendword(0)
else								!external symbol
	gendword(a^.value)				!this is probably just zero
	addrelocitem(rel32_rel,d)
esac
end

proc genabs32(ref opndrec a)=
!absolute refs to labels
ref strec d

d:=a^.labeldef

!CPL "GENABS32",D.NAME,REFTYPENAMES[D.REFTYPE]

case d^.reftype
when back_ref then
	gendword(d^.offset+a^.value)
	addrelocitem(addr32_rel,d)

when fwd_ref then
!CPL "////FWD"
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
!CPL "HERE",A.VALUE
	gendword(a^.value)
	addrelocitem(addr32_rel,d)

else								!external symbol
	gendword(a^.value)				!this is probably just zero
	addrelocitem(addr32_rel,d)
esac
end

proc genabs64(ref opndrec a)=
!absolute refs to labels
ref strec d

!CPL "GENABS"

d:=a^.labeldef

case d^.reftype
when back_ref then
	genqword(d^.offset+a^.value)
	addrelocitem(addr64_rel,d)

when fwd_ref then
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(5),addr32_rel,currseg)
	genqword(a^.value)
	addrelocitem(addr64_rel,d)

else								!external symbol
	genqword(a^.value)				!this is probably just zero
	addrelocitem(addr64_rel,d)
esac
end

function getrel32(ref strec d,int offset)int=
!get rel difference between offset in this segment, and label d

if d^.reftype=back_ref then					!defined earlier in this segment
	if d^.segment<>currseg then
		gerror("Rel label across segments2")
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
ref fwdrec f
int offset, seg
ref byte p8
ref int32 p32
ref int64 p64
ref dbuffer data

if d^.fwdrefs=nil then return fi

f:=d^.fwdrefs

while f do
	offset:=f^.offset

	case f^.reltype
	when rel32_rel then
!CPL "REL32",OFFSET,D.OFFSET,BUFFERLENGTH(CURRDATA),F.SEG,SEGMENTNAMES[F.SEG],"//",D.NAME
		p32:=bufferelemptr(currdata,offset)
		p32^:=d^.offset-offset-4

	when addr32_rel,addr64_rel then
!CPL "ADDR32/64"
		case f^.seg
		when code_seg then data:=ss_code
		when zdata_seg then gerror("Fwd ref in zdata")
		when idata_seg then data:=ss_idata
		esac

		p32:=bufferelemptr(data,offset)
		if f^.reltype=addr32_rel then
			p32^:=p32^+d^.offset
		else
			p64:=cast(p32)
			p64^:=p64^+d^.offset
		fi
	when rel8_rel then
!CPL "REL8"
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
int code,am

	case a^.mode
	when a_reg then
		if a^.size<>8 then gerror("pushreg not 64-bit") fi
		code:=regcodes[a^.reg]
		if code>=8 then
			rex :=bmask
			code iand:=7
		fi
		genrex()
		genbyte(0x50+code)

	when a_imm then
		if a^.labeldef then
			genbyte(0x68)
			genopnd(a,4)
		elsif isbytesized(a^.value) then
			genbyte(0x6A)
			genbyte(a^.value)
		elsif isdwordsized(a^.value) then
			genbyte(0x68)
			gendword(a^.value)
		else
			gerror("push imm value too large")
		fi

	when a_mem then
		if a^.size<>8 then gerror("push not 64-bit") fi
		am:=genrm(a,6)
		genrex()
		genbyte(0xFF)
		genamode(a,am)
	else
		gerror("push opnd?")
	esac
end

proc do_pop(ref opndrec a)=
int code, am

	case a^.mode
	when a_reg then
		if a^.size<>8 then gerror("popreg not 64-bit") fi
		code:=regcodes[a^.reg]
		if code>=8 then
			rex :=bmask
			code iand:=7
		fi
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a^.size<>8 then gerror("pop not 64-bit") fi
		am:=genrm(a,0)
		genrex()
		genbyte(0x8F)
		genamode(a,am)
	else
		gerror("pop opnd?")
	esac
end

proc do_inc(ref opndrec a,int code)=
!inc/dec
int opc, am

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
		gerror("inc/opnd?")
	esac
end

proc do_neg(ref opndrec a,int code)=
!neg/not/mul/imul/div/idiv
int opc, am

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
		gerror("neg/div/etc opnd?")
	esac
end

proc genamode(ref opndrec a,int am)=
int sib,mode,dispsize

sib:=am>>16

mode:=(am>>8)iand 255
dispsize:=am iand 255

genbyte(mode)			!modrm byte

if sib>=0 then		!sib byte
	genbyte(sib)
fi
case dispsize			!disp bytes
when 0 then
when 1 then
	genbyte(a^.value)
when 4 then
!	gendisp32(a)


	if a^.labeldef then
		genabs32(a)
	else
		gendword(a^.value)
	fi
else
	gerror("genamode size 2/8")
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
	gerror("Operand size not set")
esac
end

proc setaddrsize(ref opndrec a)=
if a^.mode=a_mem and a^.addrsize=4 then
	addroverride:=1
fi
end

function getdispsize(ref opndrec a,int mand=1)int=
!look at imm/mem displacement, and return 0,1 or 4
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned

if a^.labeldef then return 4 fi
if a^.value or mand then
	return (isbytesized(a^.value)|1|4)
else
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
!static var scaletable=(0: 0, 0, 1, 0, 2, 0, 0, 3)
!                       1  2  3  4  5  6  7  8
static []int scaletable=( 0, 1, 0, 2, 0, 0, 0, 3)
int mode, rm, scale, dispsize, needsib, sib, index, base
int reg, regix, code

mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
scale:=0				!0=modrm only; 1/2/4/8 means sib used
dispsize:=0
needsib:=0
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

when a_xreg then
	code:=getregcodebx(a^.reg)

!	return makeam(makemodrm(3,code,opc), sib, dispsize)		!OLD
	return makeam(makemodrm(3,opc,code), sib, dispsize)		!NEW

else
	gerror("genrm not mem")
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
	dispsize:=getdispsize(a,0)
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
	if regix=rstack then gerror("Scaled rstack?") fi

else										!assume regix used; optional reg and disp
	dispsize:=getdispsize(a,0)
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

	if regix=rstack and scale>1 then gerror("Can't scale rstack") fi

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
int am, regcode, opc, dispsize
int64 x

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
		if b^.labeldef then
!			if code not in [0..7] then gerror("non-add arith/label") fi
			if code<0 or code>7 then gerror("non-add arith/label") fi
			if a^.size<4 then gerror("add imm/size") fi
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
			unless -0x8000'0000 <= x <= 0xFFFF'FFFF then gerror("3:exceeding word32 value") end
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
		gerror("ADD reg,???")
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
		gerror("ADD mem,???")
	esac

else
	gerror("Can't add to this opnd")
esac
end

proc do_mov(ref opndrec a,b)=
int regcode, am
int64 value

case a^.mode
when a_reg then
	case b^.mode
	when a_reg, a_mem then
		if a^.size<>b^.size and b^.size then
			gerror("Opnd size mismatch")
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
		if b^.labeldef and a^.size<=2 then gerror("mov imm?") fi
		case a^.size
		when 1 then
			checkhighreg(a)
			case a^.reg
			when r2,r3,r14,r15 then
				rex ior:=0x40
			esac
			unless -128<=value<=255 then gerror("exceeding byte value") end
			genrex()
			genbyte(0xB0+regcode)
			genbyte(value)

		when 2 then
!			if value not in -32768..65535 then gerror("exceeding word16 value") fi
			unless -32768<=value<=65535 then gerror("exceeding word16 value") end
			genbyte(0x66)
			genrex()
			genbyte(0xB8+regcode)
			genword(value)
		when 4 then
			if b^.labeldef then
				genrex()
				genbyte(0xB8+regcode)
				genopnd(b,4)
			else
!				unless -0x8000'0000<=value<=0xFFFF'FFFFu then
				unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
CPL value,ref void(value)
					gerror("1:exceeding word32 value")
				end
doreg32::
				genrex()
				genbyte(0xB8+regcode)
				gendword(value)
			fi

		else							!assum 8 bytes
			if b^.labeldef then
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
		gerror("MOV REG/??")
	esac
when a_mem then
	case b^.mode
	when a_reg then
		if a^.size<>b^.size and a^.size then
			gerror("Opnd size mismatch")
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
		if b^.labeldef and a^.size<=2 then gerror("mov imm?") fi

		if a^.size=0 then a^.size:=1 fi

		case a^.size
		when 0,1 then
			unless -128<=value<=255 then gerror("exceeding byte value") end

			setopsize(a)
			genrex()
			genbyte(0xC6)
			genamode(a,am)
			genbyte(value)

		when 2 then
			unless -32768<=value<=65535 then gerror("exceeding word16 value") end
			setopsize(a)
			genrex()
			genbyte(0xC7)
			genamode(a,am)
			genword(value)
		when 4,8 then
			if not b^.labeldef then
!				unless -0x8000'0000<=value<=0xFFFF'FFFF then gerror("2:exceeding word32 value") end
				unless -0x7FFF'FFFF<=value<=0xFFFF'FFFF then gerror("2:exceeding word32 value") end
			fi
			setopsize(a)
			genrex()
			genbyte(0xC7)
			genamode(a,am)
			genopnd(b,4)
!			gendword(value)
		esac

	else
		gerror("MOV MEM/?")
	esac
else
	gerror("MOV ?/..")
esac
end

function getregcoder(int reg)int=
int regcode

regcode:=regcodes[reg]
if regcode>=8 then
	regcode-:=8
	rex ior:=rmask
fi
return regcode
end

function getregcodeb(int reg)int=
int regcode

regcode:=regcodes[reg]
if regcode>=8 then
	regcode-:=8
	rex ior:=bmask
fi
return regcode
end

function getregcodebx(int reg)int=
!do not translate reg code (I think, when xmm reg code etc)

int regcode

regcode:=reg-1
if regcode>=8 then
	regcode-:=8
	rex ior:=bmask
fi
return regcode
end

function getregcoderx(int reg)int=
!do not translate reg code (I think, when xmm reg code etc)
int regcode

regcode:=reg-1
if regcode>=8 then
	regcode-:=8
	rex ior:=rmask
fi
return regcode
end


proc do_lea(ref opndrec a,b)=
int regcode, am

unless a^.mode=a_reg and b^.mode=a_mem then
	gerror("LEA not reg/mem")
end

if a^.size<4 then gerror("LEA size error") fi
regcode:=getregcoder(a^.reg)

am:=genrm(b,regcode)
setopsize(a)
genrex()
genbyte(0x8D)
genamode(b,am)

end

proc do_movsx(ref opndrec a,b,int opc)=
!opc=B6 for movzx, and BE for movsx
int am, regcode

if a^.mode<>a_reg then gerror("movsx not reg") fi
!if a^.size=1 or a^.size<=b^.size then gerror("movsx size error") fi

if a^.size=8 and b^.size=4 then
	if opc=0xBE then
		do_movsxd(a,b)
	else						!movsx 4->8 bytes, do normal move 4->4
		a:=regtable[a^.reg,4]
		do_mov(a,b)
	fi
	return
fi

!if (opc=0xBE and a^.size=8) or a^.size=1 or a^.size<=b^.size then gerror("movsx size error") fi
if a^.size=1 or a^.size<=b^.size then gerror("movsx size error") fi

if opc=0xB6 and b^.size=4 then gerror("movsx 4=>8 bytes?") fi

case b^.mode
when a_reg then
when a_mem then
	if b^.size=0 then gerror("movsx need size prefix") fi
	if b^.size=8 then gerror("movsx size 8") fi
else
	gerror("movsx not reg/mem")
esac

regcode:=getregcoder(a^.reg)

am:=genrm(b,regcode)
setopsize(a)
!cpl "CHECKHIGH/MOVSX"
checkhighreg(b)
genrex()
!CPL =REX:"H"
genbyte(0x0F)
genbyte((b^.size=1|opc|opc+1))
genamode(b,am)
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
int regcode, am

if a^.mode=a_reg and b^.mode=a_reg and (a^.reg=r0 or b^.reg=r0) and a^.size<>1 then		!simple r0/reg
	if a^.reg<>r0 then				!get a to be r0
		swap(a,b)
	fi
	if a^.size<>b^.size then gerror("exch size") fi

	setopsize(a)
	regcode:=getregcodeb(b^.reg)
	genrex()
	genbyte(0x90+regcode)
	return
fi

if a^.mode=a_mem then swap(a,b) fi

unless a^.mode=a_reg and (b^.mode=a_reg or b^.mode=a_mem) then gerror("exch opnds") end
if b^.size=0 and b^.mode=a_mem then b^.size:=a^.size fi
if a^.size<>b^.size then gerror("exch size") fi

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
int regcode, am

if b^.mode=a_mem and b^.size=0 then b^.size:=4 fi

if a^.size<>8 or b^.size>4 then gerror("movsxd size") fi

!if a^.mode<>a_reg or b^.mode not in [a_reg,a_mem] then
if a^.mode<>a_reg or (b^.mode<>a_reg and b^.mode<>a_mem) then
	gerror("movsxd opnds")
fi

regcode:=getregcoder(a^.reg)
am:=genrm(b,regcode)

setopsize(a)
genrex()
genbyte(0x63)
genamode(b,am)

end

proc do_imul2(ref opndrec a,b)=
int regcode, am, opc
int64 value

if a^.mode<>a_reg then
	gerror("imul2 opnds")
fi
if b^.size=0 then b^.size:=a^.size fi
if a^.size=1 then gerror("imul2 byte") fi

case b^.mode
when a_reg,a_mem then
	if a^.size<>b^.size then gerror("imul2 size") fi
	regcode:=getregcoder(a^.reg)
	am:=genrm(b,regcode)

	setopsize(a)
	genrex()
	genbyte(0x0F)
	genbyte(0xAF)
	genamode(b,am)

when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
	if b^.labeldef then gerror("mul/label") fi
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
	gerror("imul2 opnds")
esac
end

proc do_imul3(ref opndrec a,b,c)=
int64 value
int regcode1, regcode2, opc

if a^.mode<>a_reg or b^.mode<>a_reg then
	gerror("imul3 opnds")
fi
if a^.size=1 then gerror("imul3 byte") fi
if c^.mode<>a_imm then gerror("imul3 not imm") fi

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
int am, w

if a^.mode<>a_reg and a^.mode<>a_mem then gerror("shift opnds1?") fi

am:=genrm(a,opc)
checkhighreg(a)
setopsize(a)
genrex()
w:=(a^.size=1|0|1)

case b^.mode
when a_imm then
	if b^.labeldef then gerror("shift/label") fi
	if b^.value=1 then
		genbyte(0xD0+w)
		genamode(a,am)
	else
		genbyte(0xC0+w)
		genamode(a,am)
		genbyte(b^.value)
	fi
when a_reg then
	if b^.reg<>r10 or b^.size<>1 then gerror("cl or b10 needed") fi
	genbyte(0xD2+w)
	genamode(a,am)

else
	gerror("shift opnds2?")
esac
end

proc do_test(ref opndrec a,b)=
int64 value
int opc, am, regcode

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
	gerror("test opnds")
fi

end

proc do_loop(ref opndrec a,int opc)=
int offset

offset:=getrel32(a^.labeldef,getcurrdatalen(9)+1)
if offset<0 then			!backjump
	if offset<-126 then
		gerror("loop jmp out of range")
	fi
	genbyte(opc)
	genbyte(offset)
else
	gerror("Can't do loopxx fwd jump")
fi
end

proc do_jcxz(ref opndrec a,int opsize)=
int offset

offset:=getrel32(a^.labeldef,getcurrdatalen(10)+1)
if offset<0 then			!backjump
	if offset<-126 then
		gerror("jcxz jmp out of range")
	fi
	if opsize=4 then genbyte(0x67) fi
	genbyte(0xE3)
	genbyte(offset)
else
	gerror("Can't do jcxz fwd jump")
fi
end

proc do_setcc(ref opndrec a,b)=
!a is cond
!b is byte reg/mem
int am

if (b^.mode<>a_reg and b^.reg<>a_mem) or b^.size>1 then gerror("setcc opnd/size") fi

am:=genrm(b,0)
checkhighreg(b)
!CPL "CHECKHIGH/SETCC",REX:"H"
!genrex()
!setopsize(1)
genrex()
genbyte(0x0F)
genbyte(0x90+a^.value)
genamode(b,am)
end

proc do_movxmm(ref opndrec a,b,int size)=
!do movd/movq depending on size being 4 or 8
int am, regcode, regcode1, regcode2

case a^.mode
when a_reg then
	case b^.mode
	when a_xreg then
		if a^.size<>size then gerror("1:movdq size") fi

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
		gerror("movdq reg,?")
	esac
when a_xreg then
	case b^.mode
	when a_reg then
!CPL "MOVXMM XREG/REG"
!		if b^.size<>size then gerror("2:movdq size") fi
!		regcode:=getregcodeb(b^.reg)
!		am:=genrm(a,regcode)
!		setopsize(b)
!		genbyte(0x66)
!		genrex()
!		genbyte(0x0F)
!		genbyte(0x6E)
!		genamode(a,am)

		if b^.size<>size then gerror("3:movdq size") fi
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
		if b^.size and b^.size<>size then gerror("4:movdq size") fi
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
		gerror("movdq xreg,?")
	esac
when a_mem then
	case b^.mode
	when a_xreg then
		if a^.size and a^.size<>size then gerror("5:movdq size") fi
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
		gerror("movdq mem,?")
	esac
else
	gerror("movdq opnds")
esac

end

proc do_arithxmm(ref opndrec a,b,int prefix,opc)=
int am, regcode

!if a^.mode<>a_xreg or b^.mode not in [a_xreg, a_mem] then
if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	gerror("arithxmm opnds")
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
int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	gerror("logicxmm opnds")
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
int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	gerror("convertfloat opnds")
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
int am, regcode

if a^.mode<>a_reg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	gerror("fix opnds")
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
int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_reg and b^.mode<>a_mem) then
	gerror("float opnds")
fi

if b^.mode=a_mem then
	if b^.size=0 then b^.size:=4 fi
	if b^.size<>4 and b^.size<>8 then gerror("float size") fi
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
int am, regcode
	case a^.mode
	when a_imm then
		genbyte(0xE8)
		genrel32(a)
	else				!indirect call
		case a^.size
		when 0 then a^.size:=8
		when 1,2,4 then
			gerror("call[]size")
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
int am, regcode, offset, shortjmp

	case a^.mode
	when a_imm then
		offset:=getrel32(a^.labeldef,getcurrdatalen(11)+1)
		if offset<0 and offset>-126 then
			genbyte(0xEB)
			genbyte(offset)
		else
			shortjmp:=0
			if offset>0 then				!fwd jump
!check if destlabel occurs within next 8 instrs, then likely to need short disp
				shortjmp:=checkshortjump(m,a^.labeldef)
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
		case a^.size
		when 0 then a^.size:=8
		when 1,2,4 then
			gerror("jmp[]size")
		esac
		am:=genrm(a,4)
		setopsize(a)
		setaddrsize(a)
		genrex()
		genbyte(0xFF)
		genamode(a,am)
	esac

end

function getcurrdatalen(int id)int=
!I think that zdata-seg is only likely when id=6

if currseg=zdata_seg then
	return ss_zdatalen
fi
return bufferlength(currdata)
end

proc do_cmovcc(ref opndrec c,a,b)=
int am, regcode
	if a^.size<>b^.size and b^.size then
		gerror("Opnd size mismatch")
	fi
	if a^.size=1 then gerror("cmov/byte") fi
	regcode:=getregcoder(a^.reg)
	am:=genrm(b,regcode)

	setopsize(a)
	genrex()
	genbyte(0x0F)
	genbyte(0x40+c^.value)
	genamode(b,am)
end

proc do_fmem(ref opndrec a, int freal, code)=
!do fld/fild/fst/fstp/fist,fistp
!freal=1 for fld/etc, 0 for fild etc
!code is middle 3 bits of 2nd byte: 0=load, 2=store, 3=store+pop
int am, regcode, mf

if a^.mode<>a_mem then
	gerror("fmem/not mem")
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
			gerror("r80 not allowed")
		esac
	else
CPL "SIZE=",A^.SIZE
		gerror("fmem size")
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
			gerror("fst i64?")
		esac
	else
		gerror("fmem int size")
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
real32 sx:=x
return int32@(x)
end

proc genrel8(ref opndrec a)=
!a is a known fwd reference, and expected to be <=127 bytes
ref strec d

d:=a^.labeldef

if d^.reftype=fwd_ref then
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(3),rel8_rel)
	genbyte(0)
else								!external symbol
	gerror("genrel8")
fi
end

function checkshortjump(ref mclrec m,ref strec d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
!return 0
int n

RETURN 0

n:=0
m:=m^.nextmcl
while m and n<=8 do
	++n
	if m^.opcode=m_label and m^.a^.labeldef=d then
		return 1
	fi

	m:=m^.nextmcl
od

return 0
end

function addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
ref fwdrec q

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
int am,regcode

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
		gerror("movdqx?")
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
		gerror("movdqx")
	esac
else
	gerror("movdqx")
esac

end

proc do_popcnt(ref opndrec a,b)=
int am, regcode

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

proc do_bsf(ref opndrec a,b, int opc)=
int am, regcode

if b^.mode=a_mem then
	if b^.size=0 then b^.size:=8 fi
fi
if a.size<>b.size then gerror("bsf size") fi

!regcode:=getregcoderx(a^.reg)
regcode:=getregcodebx(a^.reg)
am:=genrm(b,regcode)
setopsize(a)
genrex()
genbyte(0x0F)
genbyte(opc)
genamode(b,am)
end

proc extendsymboltable=
	ref[]ref strec oldsymboltable
	int oldsymboltablesize

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
=== ax_objdecls.m 25/73 ===

global record imagefileheader =
	word16	machine
	word16	nsections
	word32	timedatestamp
	word32	symtaboffset
	word32	nsymbols
	word16	optheadersize
	word16	characteristics
end

global record imagedir =
	word32	virtualaddr
	word32	size
end

global record optionalheader =			!exe/dll only
	word16  magic
	byte     majorlv
	byte     minorlv
	word32 codesize
	word32 idatasize
	word32 zdatasize
	word32 entrypoint
	word32 codebase
!	word32 datebase		!32-bit exe files only
	word64	imagebase
	word32 sectionalignment
	word32 filealignment
	word16  majorosv
	word16  minorosv
	word16  majorimagev
	word16  minorimagev
	word16  majorssv
	word16  minorssv
	word32 win32version
	word32 imagesize
	word32 headerssize
	word32 checksum
	word16  subsystem
	word16  dllcharacteristics
	word64   stackreserve
	word64   stackcommit
	word64   heapreserve
	word64   heapcommit
	word32 loaderflags
	word32 rvadims
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
		word32	physical_address
		word32	virtual_size
	end
	word32	virtual_address
	word32	rawdata_size
	word32	rawdata_offset
	word32	relocations_ptr
	word32	linenos_offset
	word16	nrelocs
	word16	nlinenos
	word32	characteristics
end

global record imagesymbol =
	union
		[8]char shortname
		struct
			word32	shortx
			word32	longx
		end
		word64 longname
	end
	word32	value
	int16	sectionno
	word16	symtype
	byte	storageclass
	byte	nauxsymbols
end

global record importdirrec =
	word32	implookuprva
	word32	timedatestamp
	word32	fwdchain
	word32	namerva
	word32	impaddressrva
end

global record coffrelocrec =
	int32	virtualaddr
	int32	stindex
	int16	reloctype
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
	int32 length
	int16 nrelocs
	int16 nlines
	int32 checksum
	int16 sectionno
	int32 dummy
end
=== ax_writeexe.m 26/73 ===
!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order::
! initsectiontable()
! genexe()
! writeexe(filename)

import clib
import mlib
import oslib
import ax_objdecls
import ax_tables
import ax_decls
import ax_lib
import ax_disasm

!const maxsearchlibs=30
!!for now, use a fixed, built-in set of search libs
![]ichar searchlibs=("msvcrt","gdi32","user32","kernel32")
!int nsearchlibs=searchlibs.len
[maxsearchlibs]int64 libinsttable
[maxsearchlibs]ichar libinstnames
[maxsearchlibs]int libnotable			!index into dlltable

record sectionrec =
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

record importrec = 				!details about all imported symbols
	ref strec def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

record dllrec =					!all imported libraries
	ichar name					!name of library, including .dll
	int nprocs					!no. of imports which use this library
	int nametableoffset			!start of name table in impdir
	int addrtableoffset			!start of addr table (IAT)
	int dllnameoffset			!offset of name within impdir
	int dllextraoffset			!offset of mysterious region just before the name
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
int imagesize
int filesize
ref[]int64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
ref strec stentrypoint				!symbol to be the entry point
ref strec stentrypoint2
ref strec stentrypoint3

const maxsection = 10
[maxsection]sectionrec sectiontable
int nsections

ref byte importdir				!allowed section data for import directort in .idata

const maximports = 3000
[maximports]importrec importtable
int nimports

const maxlibs = 50
[maxlibs]dllrec dlltable
int ndlls

ref byte datastart
ref byte dataptr
ichar userentrypoint

global proc writeexe(ichar outfile)=
imagefileheader header
optionalheader optheader
int offset,i
int64 aa

!CPL "WRITEEXE"
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

if fverbose then
	CPL "Writing file:",outfile
fi

if writefile(outfile,datastart,dataptr-datastart)=0 then
	println "Error writing exe file (possibly still running)"
	stop 1
fi
end

global proc genexe(ichar entrypoint)=
!manipulate the ss data to fill in all the details needed for exe format

userentrypoint:=entrypoint
loadlibs()
scanst()				!build dll/import tables
getoffsets()
relocdata(&sectiontable[csect])
relocdata(&sectiontable[dsect])

end

proc loadlibs=
!load library instances
int i
int64 hinst
ichar file
[300]char filename

for i to nsearchlibs do
!	hinst:=os_getdllinst(searchlibs[i])
	strcpy(&.filename,searchlibs[i])
	strcat(&.filename,".dll")
	hinst:=os_getdllinst(&.filename)
	if hinst=0 then
		cpl searchlibs[i]
		gerror("Can't load search lib")
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
!CPL =BUFFERLENGTH(SS_CODE)

if bufferlength(ss_idata)=0 then
	addqword (ss_idata,0)
fi

sectiontable[dsect].name:=".data"
sectiontable[dsect].segtype:=idata_seg
sectiontable[dsect].data:=ss_idata

!CPL "IDATASIZE",=BUFFERLENGTH(SS_IDATA):"h"

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

!proc showsectiondata(ichar caption,ref dbuffer data)=
proc showsectiondata(ref sectionrec d)=
int i,k,length,bb
[128]char str,str2
ref byte p

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

ref byte baseaddr:=cast(imagebase+d^.virtoffset)

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
ref byte codeptr,codeend,codestart
int length,offset
ichar s
[16]char str

gs_strln(dest, "proc Section Code")

length:=p^.virtsize
codestart:=codeptr:=bufferelemptr(p^.data,0)
codeend:=codeptr+length

ref byte baseaddr:=cast(imagebase+p^.virtoffset)

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
ref relocrec r

gs_str(dest,"proc Section Relocs: ")
gs_str(dest,caption)
gs_str(dest," ")
gs_strint(dest,nrelocs)
gs_line(dest)

r:=relocs

while r do

!forall i,r in relocs do
!!CPL =R.RELOCTYPE,R.OFFSET,R.STINDEX
	gs_str(dest,"Reloc: ")
	gs_str(dest,relocnames[r^.reloctype])
	gs_str(dest," Offset: ")
	gs_strint(dest,r^.offset)
	gs_str(dest," ST Index: ")
	gs_strint(dest,r^.stindex)
	gs_str(dest," ")
	gs_str(dest,ss_symboltable^[r^.stindex]^.name)
	gs_line(dest)

	r:=r^.nextreloc
od
gs_line(dest)

end

proc gs_value(ichar caption, int64 value)=
[256]char str

strcpy(&.str,caption)
strcat(&.str,":")
ipadstr(&.str,20)
gs_str(dest,&.str)

!sprintf(&.str,"0x%llX %lld",value,value)
fprint @&.str,"0x# #",value:"H",value
gs_strln(dest,&.str)
end

proc showsymboltable2=

gs_strln(dest,"Proc Symbol Table")
int i
for i:=1 to ss_nsymbols do
	gs_strint(dest,i)
	gs_str(dest,": ")
	gs_strln(dest,ss_symboltable^[i]^.name)
od
gs_line(dest)
end

proc showimporttable=
[256]char str
dllrec d
importrec p


gs_strln(dest,"Proc Dll List")
int i
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
sectionrec s
int i

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
ref char s,name2
[256]char str
[256]char str2
int i

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
		if ndlls>=maxlibs then gerror("Too many libs") fi
		libno:=++ndlls

		dlltable[libno].name:=pcm_copyheapstring(&.str)
		dlltable[libno].nprocs:=1
		return (name2|name2|s+1)
!		return s+1
	fi

	++s
od

!do explicit search
int n

!CPL "EXPLICIT SEARCH",NAME

for i:=1 to nsearchlibs do
	if os_getdllprocaddr(libinsttable[i],name) then
		n:=i
		exit				!don't need the actual address; just whether it exists
	fi
else
	println name,moduletable[moduleno].name
	gerror("Can't find external function")
od

!found in search lib n
if libno:=libnotable[n] then			!already added this library
	++dlltable[libno].nprocs
	return name
fi

!first use of this lib
strcpy(&.str,searchlibs[n])
strcat(&.str,".dll")
if ndlls>=maxlibs then gerror("2:Too many libs") fi
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
ref strec d
ichar name, libname

for i:=1 to ss_nsymbols do

	d:=ss_symboltable^[i]
	case d^.symbol
	when importedsym then
!CPL "SCANST/IMPORTED",D.NAME
		if nimports>=maximports then gerror("genexe: Too many imports") fi
		++nimports

		name:=extractlibname(d^.name,libno,d^.moduleno)

		importtable[nimports].libno:=libno			!0 if no lib
		importtable[nimports].name:=name				!original, or 2nd part of lib.name
		importtable[nimports].def:=d

		d^.importindex:=nimports
	when exportedsym then
!CPL "SCANST/EXPORTED",D.NAME
!		if eqstring(d^.name,"start") then
!			stentrypoint:=d
!		elsif eqstring(d^.name,"main") then
!			stentrypoint2:=d
!		elsif eqstring(d^.name,"WinMain") then
!			stentrypoint3:=d
!		fi

		if userentrypoint then
			if eqstring(d^.name,userentrypoint) then
				stentrypoint:=d
			fi
		else
			if eqstring(d^.name,"main") then
				stentrypoint:=d
			elsif eqstring(d^.name,"start") then
				stentrypoint2:=d
			elsif eqstring(d^.name,"WinMain") then
				stentrypoint3:=d
			fi
		fi
	esac
od
end

proc relocdata(ref sectionrec s)=
ref sectionrec u
ref relocrec r
ref byte p
ref word32 p32
ref strec d
int offset,index,thunkoffset,iatoffset

p:=bufferelemptr(s^.data,0)
r:=s^.relocs

while r do
	d:=ss_symboltable^[r^.stindex]
	index:=d^.importindex				!into importtable
	thunkoffset:=importtable[index].thunkoffset

	case r^.reloctype
	when rel32_rel then
!		if d^.scope<>extern_scope then
		if d^.symbol<>importedsym then
			gerror("rel32/not imported")
		fi
		(ref word32(p+r^.offset)^:=thunkoffset-r^.offset-4)

	when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
		if d^.symbol=importedsym then
			(ref word32(p+r^.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
		else
			case d^.segment
			when zdata_seg then u:=&sectiontable[zsect]
			when idata_seg then u:=&sectiontable[dsect]
			when code_seg then u:=&sectiontable[csect]
			esac

!CPL =P,=R.OFFSET,=U,SEGMENTNAMES[D.SEGMENT]
			p32:=cast(p+r^.offset)
!CPL =P32,=U.VIRTOFFSET,=IMAGEBASE:"H"
			p32^:=p32^+u^.virtoffset+imagebase



		fi
	else
		cpl relocnames[r^.reloctype]
		gerror("Can't do this rel type")
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

memset(&header,0,header.bytes)

header.machine:=0x8664
header.nsections:=nsections
header.optheadersize:=optionalheader.bytes
header.characteristics:=0x22F

writerecordx(&header,header.bytes)
end

proc writeoptheader=
optionalheader header

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
		gerror("User entry point not found")
	else
!		cpl entrypointname
		gerror("Entry point not found: main or start")
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
imagesectionheader sheader

memset(&sheader,0,sheader.bytes)

strcpy(&sheader.name[1],s^.name)
sheader.virtual_size:=s^.virtsize
sheader.virtual_address:=s^.virtoffset
sheader.rawdata_offset:=s^.rawoffset
sheader.rawdata_size:=s^.rawsize

int64 aa
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

ref byte pimpdir

pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

!prepare the thunk area in the code segment
ref importdirrec pdir
ref int64 paddr,pname
int iatoffset
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
ref byte phint
ref word32 pextra

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

!write the thunk table
ref byte thunkptr,codebase
int thunkaddr
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

=== ax_disasm.m 27/73 ===
import clib
import msys
import oslib

!const showmregs=1
const showmregs=0

const halt=0xF4

int abc
real xyz

int res2
int lx

int nmodules
int xfchsmask_pd

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

[0:]ichar condnames = \
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

int rex

int addrmode						!amreg/ammem/amrel
int rmreg							!0, or 1..16; adjusted middle value of modrm byte
int rmopc							!0 to 7; middle value of modrm byte 
int basereg							!0, or 1..16
int indexreg						!0, or 1..16
int scale							!1,2,4
int opsize							!1,2,4,8
int offset
int offsetsize						!1 or 4
int sizeoverride					!32=>16 switch
int addroverride					!32=>16 switch
int f2override						!xmm regs
int f3override						!xmm regs

[256]char deststr
ichar destptr

!==============================================================

ref byte codeptr

global function decodeinstr(ref byte &cptr,baseaddr=nil)ichar=
!decode next instruction at codeptr
!return 1 if decoded, with codeptr stepped to start of next instruction
!return 0 when end-of-code seen (nop or 0x90)
int n,w
int opc,reg,op,xxx,oldopsize,dispsize
ref byte pstart
static [256]char str
[128]char str2
const maxinstrlen=14
ichar s

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
int opc,rhssize
ichar opcstr

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

when 0xBC, 0xBD then
	decodeaddr(1)
	genstr((opc=0xBC|"bsf "|"bsr "))
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
int modrm,xxx,mode,sib,rm

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
word a
a:=ref word16(codeptr)^
codeptr+:=2
return a
end

function readint16:int=
int a
a:=ref int16(codeptr)^
codeptr+:=2
return a
end

function readword32:word=
word a
a:=ref word32(codeptr)^
codeptr+:=4
return a
END

function readint32:int=
int a
a:=ref int32(codeptr)^
codeptr+:=4
return a
END

function readint64:int64=
int64 a
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
static []ichar regnames8=("al","cl","dl","bl","ah","ch","dh","bh",
						"r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b",
				"spl","bpl","sil","dil")

static []ichar regnames16=("ax","cx","dx","bx","sp","bp","si","di",
						"r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w")

static []ichar regnames32=("eax","ecx","edx","ebx","esp","ebp","esi","edi",
						"r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d")

static []ichar regnames64=("rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
						"r8","r9","r10","r11","r12","r13","r14","r15")

![]ichar mregnames8=("B0","B10","B11","B1","Bsp","Bbp","B2","B3",
static []ichar mregnames8=("B0","B10","B11","B1","B16","B18","B19","B17",
						"B12","B13","B4","B5","B6","B7","B8","B9",
					"B14","B15","B2","B3")

static []ichar mregnames16=("W0","W10","W11","W1","Wsp","Wbp","W2","W3",
						"W12","W13","W4","W5","W6","W7","W8","W9")

static []ichar mregnames32=("A0","A10","A11","A1","Astack","Aframe","A2","A3",
						"A12","A13","A4","A5","A6","A7","A8","A9")

static []ichar mregnames64=("D0","D10","D11","D1","Dstack","Dframe","D2","D3",
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
static []ichar fregnames=("st0","st1","st2","st3","st4","st5","st6","st7")
return fregnames[freg]
end

proc printaddrmode(int xmm=0)=
static [100]char str
ichar plus
int addrsize

genstr(" ")

case addrmode
when amreg then
	if xmm then
		genstr(strxmm(basereg))
	else
		getsilx(basereg)
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
![32]char str
!sprintf(&.str,"%lld",a)
genstr(strint(a))
end

proc genhex(int64 a)=
![32]char str
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
static [32]char str

!sprintf(&.str,"xmm%d",reg-1)
print @&.str,"xmm",,reg-1
return &.str
end

function strmmx(int reg)ichar=
static [32]char str

!sprintf(&.str,"mmx%d",reg-1)
print @&.str,"mmx",,reg-1
return &.str
end

proc decode8087(int ttt)=
byte bb
int longopc,freg,shortopc,code

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
int d, p

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
=== ax_writeobj.m 28/73 ===
!NEEDS REVISING TO MATCH UNLIMITED SS_SYMBOLTABLE size used for EXE
!and also unlimited strings

import clib
import mlib
import ax_objdecls
import ax_decls
import ax_tables
import ax_lib

int symtaboffset

ref byte datastart
ref byte dataptr

![0..ss_symboltable.len+10]imagesymbol symboltable		!needs a few more than ss set of symbols
[0..10'000]imagesymbol symboltable		!needs a few more than ss set of symbols

int nsymbols

int stoffset=0				!usually +7 to convert ss_symboltable indices to symboltable

const maxstring=5000
[maxstring]ichar stringtable
[maxstring]int stringlengths
int nextstringoffset=0
int nstrings=0

global proc writess(ichar outfile)=
	writecoff(outfile)
end

proc writerecord(ref void r, int length)=
	memcpy(dataptr,r,length)
	dataptr+:=length
end

proc writerelocs(ref relocrec r,int nrelocs)=
	static coffrelocrec s
	ref strec d

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
	int i
	for i:=1 to nsymbols do
		writerecord(&symboltable[i],imagesymbol.bytes)
	od
end

proc writestringtable=
!should immediately follow symboltable
	ref int32 p
	int i,n

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
	static imagesymbol r
	int length

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
	static imagesymbol r
	ref byte p:=cast(&r)
	int n

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
	static auxsectionrec r

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
	int offset

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
	ref strec s
	ichar name
	int i,sect, scope

	stoffset:=nsymbols-1

	nstrings:=0
	nextstringoffset:=4

	for i to ss_nsymbols do
		s:=ss_symboltable^[i]

		name:=s^.name

		case s^.segment
		when zdata_seg then sect:=1
		when idata_seg then sect:=2
		when code_seg then sect:=3
		else sect:=0
		esac

		case s^.symbol
		when fwdlocalsym,localsym then
			scope:=3
		when importedsym,exportedsym then
			scope:=2
		else
			scope:=0
		esac

		addsymbol(makesymbol(s^.name,s^.namelen,sectionno:sect, storage:scope, value:s^.offset))

	od
end

proc writecoff(ichar outfile)=
	imagefileheader header
	imagesectionheader zsection, isection, csection
	int offset
	int64 aa

	memset(&header,0,header.bytes)
	memset(&zsection,0,imagesectionheader.bytes)
	memset(&isection,0,imagesectionheader.bytes)
	memset(&csection,0,imagesectionheader.bytes)

	header.machine:=0x8664
	header.nsections:=3

!zsection:=new(imagesectionheader)
	strcpy(&zsection.name[1],".bss")
	zsection.rawdata_size:=ss_zdatalen


!	aa:=0xc040'0080
!	zsection.characteristics:=AA			!BUG in compiler or assemble; need to assign indirectly
	zsection.characteristics:=0xC040'0080

	if ss_nidatarelocs>=65536 or ss_ncoderelocs>=65536 then
		gerror("Too many relocs (exceeds 16-bit field)")
	fi

!isection:=new(imagesectionheader)
	strcpy(&isection.name[1],".data")
	isection.rawdata_size:=bufferlength(ss_idata)
	isection.nrelocs:=ss_nidatarelocs

!	AA:=0xC050'0040
!	isection.characteristics:=AA
	isection.characteristics:=0xC050'0040

	strcpy(&csection.name[1],".text")
	csection.rawdata_size:=bufferlength(ss_code)
	csection.nrelocs:=ss_ncoderelocs

!	AA:=0x6050'0020
!	csection.characteristics:=AA
	csection.characteristics:=0x6050'0020

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

	if fverbose then
		println "Writing file:",outfile
	fi
	writefile(outfile,datastart,dataptr-datastart)

end

=== bcclib.asm 29/73 ===
;	bcc support library

;Offsets in buffer:
kreturn	= 0
kstack	= 8
kframe	= 16

	segment code
$mccsetjmp::

;on entry to setjmp:
;Dstack		points to return address
;D10		points to address of buffer to store restore info
;Caller will have subtracted 32 from Dstack, and will add it again on return

; Store current state

	mov [D10+kstack],Dstack
	mov [D10+kframe],Dframe
	mov D0,[Dstack]			; return address
	mov [D10+kreturn],D0

	mov	A0,0
	ret

$mcclongjmp::

;on entry to longjmp:
;Dstack		points to return address
;D10		points to address of buffer containing store restore info
;D11		has return value to use
;Caller will have subtracted 32 from Dstack, and will add it again on return

; Restore state as it was on call to setjmp


	mov Dstack,[D10+kstack]		; restore stack value
	mov Dframe,[D10+kframe]		; restore frame ptr

	mov D0,[D10+kreturn]		; stored return address
	mov [Dstack+0],D0			; replace return address, as it will return elsewhere
	mov A0,A11					; return value (from 'setjmp', as it will be)

	ret

;Float routines for unsigned
;Input passed in D10
;Output in XMM15

m$ufloat_r64u32::
	mov D10,D10					; clear top half (already done if value just moved there)
	cvtsi2sd XMM15,D10
	ret

m$ufloat_r32u32::
	mov D10,D10
	cvtsi2ss XMM15,D10
	ret

m$ufloat_r64u64::
	cmp D10,0
	jl fl1
;number is positive, so can treat like i64
	cvtsi2sd XMM15,D10
	ret
fl1:						;negative value
	and D10,[mask63]		;clear top bit (subtract 2**63)
	cvtsi2sd XMM15,D10
	addsd XMM15,[offset64]	;(add 2**63 back to result)
	ret

m$ufloat_r32u64::
	cmp D10,0
	jl fl2
;number is positive, so can treat like i64
	cvtsi2ss XMM15,D10
	ret
fl2:						;negative value
	and D10,[mask63]		;clear top bit (subtract 2**63)
	cvtsi2ss XMM15,D10
	addss XMM15,[offset32]	;(add 2**63 back to result)
	ret

	segment idata
mask63:
	dq 0x7fffffffffffffff
offset64:
	dq 9223372036854775808.0		! 2**63 as r64
offset32:
	dd 9223372036854775808.0		; 2**63 as r32

	segment code
__rdtsc::
!	rdtsc
	mov eax,eax
	shl rdx,32
	or rax,rdx
	ret

	segment zdata
callbackstack:
	resb 576			!8-level stack
;	resb 5'120'000

ncallbacks:
	resb 4

segment code

m$pushcallback::
	inc dword [ncallbacks]
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
	ret

m$popcallback::
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
	dec dword [ncallbacks]
	ret


=== assert.h 30/73 ===
/* Header assert.h */

//#define assert(x) 0
#define assert(x)
=== ctype.h 31/73 ===
/* Header ctype.h */

int tolower(int);
int toupper(int);
int isalpha(int);
int isdigit(int);
int isspace(int);
int isalnum(int);
int isupper(int);
int islower(int);

int isxdigit(int);
int iscntrl(int);
int isgraph(int);
int ispunct(int);
int isprint(int);
int __isascii(int);
#define isascii __isascii

int __toascii(int);
#define toascii __toascii
=== errno.h 32/73 ===
/* Header errno.h */

#ifndef $errno
#define $errno

extern int* _errno(void);
#define errno (*_errno())

#define EPERM 1
#define ENOENT 2
#define ESRCH 3
#define EINTR 4
#define EIO 5
#define ENXIO 6
#define E2BIG 7
#define ENOEXEC 8
#define EBADF 9
#define ECHILD 10
#define EAGAIN 11
#define ENOMEM 12
#define EACCES 13
#define EFAULT 14
#define ENOTBLK 15
#define EBUSY 16
#define EEXIST 17
#define EXDEV 18
#define ENODEV 19
#define ENOTDIR 20
#define EISDIR 21
#define EINVAL 22
#define ENFILE 23
#define EMFILE 24
#define ENOTTY 25
#define ETXTBSY 26
#define EFBIG 27
#define ENOSPC 28
#define ESPIPE 29
#define EROFS 30
#define EMLINK 31
#define EPIPE 32
#define EDOM 33
#define ERANGE 34
#define ENOMSG 35
#define EIDRM 36
#define ECHRNG 37
#define EL2NSYNC 38
#define EL3HLT 39
#define EL3RST 40
#define ELNRNG 41
#define EUNATCH 42
#define ENOCSI 43
#define EL2HLT 44
#define EDEADLK 45
#define ENOLCK 46
#define EBADE 50
#define EBADR 51
#define EXFULL 52
#define ENOANO 53
#define EBADRQC 54
#define EBADSLT 55
#define EDEADLOCK 56
#define EBFONT 57
#define ENOSTR 60
#define ENODATA 61
#define ETIME 62
#define ENOSR 63
#define ENONET 64
#define ENOPKG 65
#define EREMOTE 66
#define ENOLINK 67
#define EADV 68
#define ESRMNT 69
#define ECOMM 70
#define EPROTO 71
#define EMULTIHOP 74
#define ELBIN 75
#define EOVERFLOW 76
#define EBADMSG 77
#define ENOTUNIQ 80
#define EBADFD 81
#define EREMCHG 82
#define ELIBACC 83
#define ELIBBAD 84
#define ELIBSCN 85
#define ELIBMAX 86
#define ELIBEXEC 87
#define ENOSYS 88
#define ENMFILE 89
#define ENOTEMPTY 90
#define ENAMETOOLONG 91
#define EILSEQ 92
#define __ELASTERROR 2000

#endif
=== fenv.h 33/73 ===
/* Header fenv.h */

int feclearexcept(int);
int fetestexcept(int);

#define FE_INVALID 1
#define FE_DENORMAL 2
#define FE_INEXACT 32
#define FE_DIVBYZERO 4 
#define FE_OVERFLOW 8
#define FE_UNDERFLOW 16
#define FE_STACKFAULT 64
#define FE_ALL_EXCEPT (FE_INVALID|FE_DENORMAL|FE_INEXACT|FE_DIVBYZERO|FE_OVERFLOW|FE_UNDERFLOW)
=== float.h 34/73 ===
/* Header float.h */

//#define DBL_MAX_10_EXP 308
//#define DBL_MANT_DIG 53

#define FLT_RADIX 2

#define FLT_DIG 6
#define FLT_MIN_EXP -125
#define FLT_MIN 1.17549435E-38F // decimal constant
#define FLT_MIN 0X1P-126F // hex constant
#define FLT_TRUE_MIN 1.40129846E-45F // decimal constant
#define FLT_TRUE_MIN 0X1P-149F // hex constant
#define FLT_HAS_SUBNORM 1
#define FLT_MIN_10_EXP -37
#define FLT_MAX_EXP +128
#define FLT_MAX 3.40282347E+38F // decimal constant
#define FLT_MAX 0X1.fffffeP127F // hex constant
#define FLT_MAX_10_EXP +38
#define FLT_EPSILON 1.19209290e-07F
#define FLT_MANT_DIG 23

#define DBL_MANT_DIG 53
#define DBL_EPSILON 2.2204460492503131E-16 // decimal constant
//#define DBL_EPSILON 0X1P-52 // hex constant
#define DBL_DECIMAL_DIG 17
#define DBL_DIG 15
#define DBL_MIN_EXP -1021
#define DBL_MIN 2.2250738585072014E-308 // decimal constant
//#define DBL_MIN 0X1P-1022 // hex constant
#define DBL_TRUE_MIN 4.9406564584124654E-324 // decimal constant
//#define DBL_TRUE_MIN 0X1P-1074 // hex constant
#define DBL_HAS_SUBNORM 1
#define DBL_MIN_10_EXP -307
#define DBL_MAX_EXP +1024
#define DBL_MAX 1.7976931348623157E+308 // decimal constant
//#define DBL_MAX 0X1.fffffffffffffP1023 // h
#define DBL_MAX_10_EXP +308

#define LDBL_MIN DBL_MIN
#define LDBL_MAX DBL_MAX
#define LDBL_EPSILON 2.2204460492503131E-16
#define LDBL_MANT_DIG 53
#define LDBL_MIN_EXP -1021
#define LDBL_MAX_EXP +1024

int     _isnan(double);
#define isnan _isnan
=== inttypes.h 35/73 ===
/* Header inttypes.h */

#include <stdint.h>

/* fprintf macros for signed types */
#define PRId8 "d"
#define PRId16 "d"
#define PRId32 "d"
#define PRId64 "I64d"

#define PRIdLEAST8 "d"
#define PRIdLEAST16 "d"
#define PRIdLEAST32 "d"
#define PRIdLEAST64 "I64d"

#define PRIdFAST8 "d"
#define PRIdFAST16 "d"
#define PRIdFAST32 "d"
#define PRIdFAST64 "I64d"

#define PRIdMAX "I64d"

#define PRIi8 "i"
#define PRIi16 "i"
#define PRIi32 "i"
#define PRIi64 "I64i"

#define PRIiLEAST8 "i"
#define PRIiLEAST16 "i"
#define PRIiLEAST32 "i"
#define PRIiLEAST64 "I64i"

#define PRIiFAST8 "i"
#define PRIiFAST16 "i"
#define PRIiFAST32 "i"
#define PRIiFAST64 "I64i"

#define PRIiMAX "I64i"

#define PRIo8 "o"
#define PRIo16 "o"
#define PRIo32 "o"
#define PRIo64 "I64o"

#define PRIoLEAST8 "o"
#define PRIoLEAST16 "o"
#define PRIoLEAST32 "o"
#define PRIoLEAST64 "I64o"

#define PRIoFAST8 "o"
#define PRIoFAST16 "o"
#define PRIoFAST32 "o"
#define PRIoFAST64 "I64o"

#define PRIoMAX "I64o"

/* fprintf macros for unsigned types */
#define PRIu8 "u"
#define PRIu16 "u"
#define PRIu32 "u"
#define PRIu64 "I64u"


#define PRIuLEAST8 "u"
#define PRIuLEAST16 "u"
#define PRIuLEAST32 "u"
#define PRIuLEAST64 "I64u"

#define PRIuFAST8 "u"
#define PRIuFAST16 "u"
#define PRIuFAST32 "u"
#define PRIuFAST64 "I64u"

#define PRIuMAX "I64u"

#define PRIx8 "x"
#define PRIx16 "x"
#define PRIx32 "x"
#define PRIx64 "I64x"

#define PRIxLEAST8 "x"
#define PRIxLEAST16 "x"
#define PRIxLEAST32 "x"
#define PRIxLEAST64 "I64x"

#define PRIxFAST8 "x"
#define PRIxFAST16 "x"
#define PRIxFAST32 "x"
#define PRIxFAST64 "I64x"

#define PRIxMAX "I64x"

#define PRIX8 "X"
#define PRIX16 "X"
#define PRIX32 "X"
#define PRIX64 "I64X"

#define PRIXLEAST8 "X"
#define PRIXLEAST16 "X"
#define PRIXLEAST32 "X"
#define PRIXLEAST64 "I64X"

#define PRIXFAST8 "X"
#define PRIXFAST16 "X"
#define PRIXFAST32 "X"
#define PRIXFAST64 "I64X"

#define PRIXMAX "I64X"

=== stdint.h 36/73 ===
/* Header stdint.h */

#ifndef $stdint
#define $stdint

typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long int uint64_t;

typedef long long int intptr_t;
typedef unsigned long long int uintptr_t;
typedef long long intmax_t;
typedef unsigned long long uintmax_t;

#define INT8_MIN -128
#define INT8_MAX 127
#define INT16_MIN -32768
#define INT16_MAX 32767
#define INT32_MIN 0x80000000
#define INT32_MAX 0x7FFFFFFF
#define INT64_MIN 0x8000000000000000
#define INT64_MAX 0x7FFFFFFFFFFFFFFF

#define UINT8_MAX 255
#define UINT16_MAX 65535
#define UINT32_MAX 0xFFFFFFFF
#define UINT64_MAX 0xFFFFFFFFFFFFFFFF

#define INTPTR_MIN 0x8000000000000000
#define INTPTR_MAX 0x7FFFFFFFFFFFFFFF
#define UINTPTR_MAX 0xFFFFFFFFFFFFFFFF

#define UINT64_MAX 0xFFFFFFFFFFFFFFFF
#define SIZE_MAX 0xFFFFFFFFFFFFFFFF

#define PTRDIFF_MIN 0x8000000000000000
#define PTRDIFF_MAX 0x7FFFFFFFFFFFFFFF

#define WCHAR_MIN 0
#define WCHAR_MAX 65535

#define WINT_MIN 0
#define WINT_MAX 65535

#define UINT64_C(x) (x##ull)

#endif
=== limits.h 37/73 ===
/* Header limits.h */

#define CHAR_BIT 8

#define CHAR_MIN 0
#define CHAR_MAX 255

#define UCHAR_MIN 0
#define UCHAR_MAX 255

#define SCHAR_MIN -128
#define SCHAR_MAX 127

#define SHRT_MIN -32768
#define SHRT_MAX 32767

#define USHRT_MIN 0
#define USHRT_MAX 65536

#define INT_MIN -2147483648
#define INT_MAX  2147483647

#define UINT_MIN 0
#define UINT_MAX 4294967295

#define LONG_MIN -2147483648
#define LONG_MAX  2147483647

#define ULONG_MIN 0
#define ULONG_MAX 4294967295

#define LLONG_MIN -9223372036854775808LL
#define LLONG_MAX  9223372036854775807LL

#define ULLONG_MIN 0
#define ULLONG_MAX 0xFFFFFFFFFFFFFFFFLL
=== locale.h 38/73 ===
/* Header locale.h */

#define LC_ALL 0
#define LC_COLLATE 1
#define LC_CTYPE 2
#define LC_MONETARY 3
#define LC_NUMERIC 4
#define LC_TIME 5

struct lconv {
	char *decimal_point;
	char *thousands_sep;
	char *grouping;
	char *int_curr_symbol;
	char *currency_symbol;
	char *mon_decimal_point;
	char *mon_thousands_sep;
	char *mon_grouping;
	char *positive_sign;
	char *negative_sign;
	char int_frac_digits;
	char frac_digits;
	char p_cs_precedes;
	char p_sep_by_space;
	char n_cs_precedes;
	char n_sep_by_space;
	char p_sign_posn;
	char n_sign_posn;
};
char *setlocale(int category, const char *locale);

struct lconv *localeconv(void);

char * setlocale(int,const char *);
=== _ansi.h 39/73 ===
/* Header _ansi.h */
=== math.h 40/73 ===
/* Header math.h */

#define HUGE_VAL 1.7976931348623156e+308

double floor(double);
double ceil(double);
double sqrt(double);
double sin(double);
double cos(double);
double tan(double);
double fmod(double,double);
double asin(double);
double acos(double);
double atan(double);
double log(double);
double log10(double);
double exp(double);
double modf(double,double*);
double atan2(double,double);
double pow(double,double);
double fabs(double);
double sinh(double);
double cosh(double);
double tanh(double);
double frexp(double,int*);
double ldexp(double,int);
int isnan(double);

long double fminl(long double,long double);
float fminf(float ,float);
double fmin(double,double);
float fabsf(float);

float floorf(float);

double _copysign(double,double);
#define copysign _copysign

long double fmaxl(long double,long double);
double fmax(double,double);
float fmaxf(float,float);
float fmodf(float,float);

//long double exp2l(long double);
double exp2(double);
float exp2f(float);

//double log2(double);
#define log2(x) (log(x)*1.442695041)

#define M_PI 3.1415926535897932384625433
#define M_PI_2 (M_PI/2.0)
#define M_2_PI 0.63661977236758134308

int isinf(double);
=== setjmp.h 41/73 ===
/* Header setjmp.h */


#ifndef $setjmp
#define $setjmp 1

typedef int jmp_buf[128];

//void longjmp(char*, int);

//void $mcclongjmp(char*, int);
void $mcclongjmp(jmp_buf, int);

//int $mccsetjmp(char*);
int $mccsetjmp(jmp_buf);


//int	_setjmp(char*);
//int	setjmp(char*);

#define setjmp $mccsetjmp
#define longjmp $mcclongjmp

#endif

=== signal.h 42/73 ===
/* Header signal.h */

#define SIGINT    2
#define SIGILL    4
#define SIGFPE    8
#define SIGSEGV  11
#define SIGTERM  15
#define SIGBREAK 21
#define SIGABRT  22

#define SIG_DFL (void (*)(int))0
#define SIG_IGN (void (*)(int))1
#define SIG_SGE (void (*)(int))3
#define SIG_ACK (void (*)(int))4

#define SIG_ERR (void (*)(int))-1

extern void (*signal(int, void (*)(int)))(int);

extern int raise(int);


typedef int sig_atomic_t;
=== stdarg.h 43/73 ===
/* Header stdarg.h */

#ifndef $STDARG
 #define $STDARG

//coded for x64 target as used by mcc (with first four params also on stack)

 typedef char *	va_list;
 #define va_start(ap,v) ap=((va_list)&v+8)
 #define va_arg(ap,t) *(t*)((ap+=8)-8)
 #define va_copy(dest,src) (dest=src)
 #define va_end(ap)	( ap = (va_list)0 )
#endif
=== stdbool.h 44/73 ===
/* Header stdbool.h */

#define bool unsigned char
#define true 1
#define false 0


=== stddef.h 45/73 ===
/* Header stddef.h */

#ifndef $stddef
#define $stddef

typedef signed long long int ssize_t;
typedef unsigned long long int size_t;

#define _WCHAR_T_DEFINED
//typedef unsigned short wchar_t;
typedef signed short wchar_t;

#define NULL ((void*)0)

#define offsetof(a,b) (size_t) &( ((a*)0) -> b)

typedef long long int ptrdiff_t;

#endif // stddef
=== stdio.h 46/73 ===
/* Header stdio.h */

#ifndef $stdio
#define $stdio 1

#define __attribute__(x)

//#message "STDIO included"

#ifndef $valist
	typedef char* va_list;
	#define $valist
#endif

#include <stddef.h>

typedef long long int fpos_t;

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define EOF (-1)
#define FILENAME_MAX 260

#define L_tmpnam 10

typedef struct {
	char *_ptr;
	int   _cnt;
	char *_base;
	int   _flag;
	int   _file;
	int   _charbuf;
	int   _bufsiz;
	char *_tmpfname;
	} FILE;

// _iob-based 
//extern FILE _iob[];
//#define stdin (&_iob[0])
//#define stdout (&_iob[1])
//#define stderr (&_iob[2])
//

// __iob_func-based
extern char* __iob_func(void);

#define stdin ((FILE*)(__iob_func()))
#define stdout ((FILE*)(__iob_func()+sizeof(FILE)))
#define stderr ((FILE*)(__iob_func()+sizeof(FILE)*2))

#define _IOREAD 0x0001
#define _IOWRT 0x0002

#define _IOFBF 0x0000
#define _IOLBF 0x0040
#define _IONBF 0x0004

#define _IOMYBUF 0x0008
#define _IOEOF 0x0010
#define _IOERR 0x0020
#define _IOSTRG 0x0040
#define _IORW 0x0080

#define BUFSIZ 512

FILE* fopen(const char*, const char*);
int fclose(FILE*);
long ftell(FILE*);
long long int _ftelli64(FILE*);
int fseek(FILE*,long,int);
int _fseeki64(FILE*,long long int,int);

size_t fread(void*, size_t, size_t, FILE*);
size_t fwrite(const void*, size_t, size_t, FILE*);
int remove(const char*);
int rename(const char *,const char *);
FILE* freopen(const char*, const char*, FILE*);
FILE* _wfopen(const wchar_t*,const wchar_t *);

int printf(const char*, ...);
int sprintf(char*,const char*, ...);
int fprintf(FILE*,const char*, ...);
int sscanf(const char*, const char*, ...);
int scanf(const char*, ...);
int fscanf(FILE *,const char *, ...);
int _snprintf(char *,size_t,const char*,...);
#define snprintf _snprintf
int _vsnprintf(char*, size_t, const char*, va_list);
int vsnprintf(char*,size_t,const char*,va_list);
int vsprintf(char*, const char*, va_list);
int _wremove(const wchar_t*);
int _wrename(const wchar_t*,const wchar_t*);

typedef char* va_list;

int vfprintf(FILE*, const char*, va_list);
int vprintf(const char*, va_list);

int puts(const char*);
char* fgets(char*, int, FILE*);
int fputs(const char*, FILE*);
int fgetc(FILE*);
int fputc(int, FILE*);
int ungetc(int, FILE*);
int getchar(void);
int putchar(int);
int fflush(FILE *);
int getc(FILE *);
int putc(int, FILE *);

int feof(FILE*);
int ferror(FILE*);
void clearerr(FILE*);

int fileno(FILE*);
int _fileno(FILE*);
int setvbuf(FILE*,char*,int,size_t);
FILE* _popen(const char*, const char*);
int _pclose(FILE*);
int _unlink(const char *);
#define unlink _unlink;
FILE* _fdopen(int, const char *);
#define fdopen _fdopen
int fgetpos(FILE*, fpos_t*);
int fsetpos(FILE*, const fpos_t*);
void perror(char*);
void setbuf(FILE*, char*);

void rewind(FILE*);

FILE* tmpfile(void);

char* tmpnam(char*);
wchar_t getwc(FILE *);

extern void* _wenviron;

#endif

=== stdlib.h 47/73 ===
/* Header stdlib.h */

#ifndef $stdlib
#define $stdlib 1

#include <stddef.h>

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#define RAND_MAX 32767

void* malloc(size_t);
void* realloc(void*, size_t);
void* calloc(size_t, size_t);

void free(void*);

void exit(int);

int system(const char*);
int _wsystem(const wchar_t*);

int abs(int);
long labs(long);
long long llabs(long);
int rand(void);
void srand(unsigned int);
int atoi(const char*);
long atol(const char*);
double atof(const char *);
int	atexit(void (*)(void));

void qsort(void*, size_t, size_t, int (*)(void*, void*));

typedef struct {
	long long int quot;
	long long int rem;
} lldiv_t;

lldiv_t lldiv(long long int, long long int);

char* getenv(const char*);
wchar_t* _wgetenv(const wchar_t*);
int _wputenv(const wchar_t*);


double strtod(const char*, char**);
float strtof(const char *, char**);
long double strtold(const char*, char**);
void abort(void);
long int strtol(const char*, char**, int);

long double strtold(const char *,char **);

long long int _strtoi64(const char *,char **,int);
#define strtoll _strtoi64

unsigned long long int _strtoui64(const char *,char **,int);

unsigned long long int strtoull(const char*, char**, int);
unsigned long strtoul(const char*, char**, int);

size_t _msize(void *);
#define msize _msize

#endif
=== _syslist.h 48/73 ===
/* Header _syslist.h */
=== string.h 49/73 ===
/* Header string.h */

#include <stddef.h>

void* memcpy(void*, const void*, size_t);
void* memmove(const void*, const void*, size_t);
void* memset(void*, int, size_t);
int memcmp(const void*, const void*, size_t);
void* memchr(const void *, int, size_t);

size_t strlen(const char*);
char* strcpy(char*, const char*);
char* strncpy(char*, const char*, size_t);
char* strcat(char*, const char*);
int strcmp(const char*, const char*);
int strncmp(const char*, const char*, size_t);
char* strchr(const char*, int);
char* strstr(const char*, const char*);
char* strrchr(const char*, int);
int _stricmp(const char*, const char*);
#define stricmp _stricmp

int _strnicmp(const char*, const char*, size_t);
#define strnicmp _strnicmp
char* strncat(char*, const char*, size_t);
char* strtok(char*, const char*);

size_t strcspn(const char*, const char*);
size_t strspn(const char*, const char*);
int strcoll(const char *, const char *);

unsigned long strtoul(const char*, char**, int);

char* strerror(int);
char* strpbrk(const char*, const char*);
size_t strxfrm(char*, const char *, size_t);

char* _strupr(char*);
char* _strlwr(char*);
#define strupr _strupr
#define strlwr _strlwr

char*strnupr(char*,size_t);
char*strnlwr(char*,size_t);
int strtrim(char*);
char*strrev(char*);

char* _strdup(const char*);
#define strdup _strdup

int _wcsicmp(const wchar_t *,const wchar_t *);

wchar_t *wcspbrk(const wchar_t*,const wchar_t*);

size_t wcslen(const wchar_t*);
=== time.h 50/73 ===
/* Header time.h */

#ifndef $time
#define $time

#include <stddef.h>

typedef long clock_t;
clock_t clock(void);

//typedef long time_t;
#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
typedef long long int time_t;
#endif

struct tm
{
  int	tm_sec;
  int	tm_min;
  int	tm_hour;
  int	tm_mday;
  int	tm_mon;
  int	tm_year;
  int	tm_wday;
  int	tm_yday;
  int	tm_isdst;
};

//struct tm *localtime(time_t*);
struct tm* _localtime32(time_t*);
#define localtime _localtime32

time_t _time64(time_t *_timer);
#define time _time64

#define CLOCKS_PER_SEC 1000

struct tm *gmtime(const time_t*);
size_t strftime(char *, size_t, const char *, const struct tm *);
time_t mktime(struct tm *);
double difftime(time_t, time_t);

char* asctime(const struct tm*);
//char* ctime(const time_t *_time);
char* _ctime64(const time_t *_time);
#define ctime _ctime64

#endif
=== utime.h 51/73 ===
/* utime.h header */

#include <sys/utime.h>
=== unistd.h 52/73 ===
/* unistd.h header */
=== safelib.h 53/73 ===
/* Header safelib.h */
=== wchar.h 54/73 ===
/* Header wchar.h */

#include <stddef.h>


#define _WCHAR_T_DEFINED
typedef unsigned short wint_t;
//typedef unsigned short wchar_t;
typedef signed short wchar_t;

size_t wcslen(const wchar_t*);
wchar_t* wcscpy(wchar_t*,const wchar_t*);
//wchar_t* _wgetenv(constwchar_t*);
wchar_t* wcstok(wchar_t*,wchar_t*,wchar_t**);
wchar_t* wcstok_s(wchar_t*,wchar_t*,wchar_t**);

wchar_t* wcschr(wchar_t*,wchar_t);
wchar_t* wcsstr(const wchar_t*,const wchar_t*);


int wcscmp(const wchar_t*,const wchar_t*);
=== wctype.h 55/73 ===
/* Header wctype.h */
=== types.h 56/73 ===
/* types.h */
#ifndef $systypes
#define $systypes 1

typedef long int off_t;
typedef long int ino_t;
typedef unsigned int dev_t;

typedef long long time_t;

#endif
=== stat.h 57/73 ===
/* stat.h */

#ifndef $sysstat
#define $sysstat

#include <stddef.h>

struct _stat {
	unsigned int	st_dev;
	unsigned short	st_ino;
	unsigned short	st_mode;
	short			st_nlink;
	short			st_uid;
	short			st_gid;
	unsigned long	st_rdev;
	unsigned int	st_size;
	unsigned long long int	st_atime;
	unsigned long long int	st_mtime;
	unsigned long long int	st_ctime;
};

#define stat _stat
#define _stati64 _stat

#define _S_IFMT 0xF000
#define _S_IFDIR 0x4000
#define _S_IFCHR 0x2000
#define _S_IFIFO 0x1000
#define _S_IFREG 0x8000
#define _S_IREAD 0x0100
#define _S_IWRITE 0x0080
#define _S_IEXEC 0x0040

#define S_IFMT 0xF000
#define S_IFDIR 0x4000
#define S_IFCHR 0x2000
#define S_IFIFO 0x1000
#define S_IFREG 0x8000
#define S_IREAD 0x0100
#define S_IWRITE 0x0080
#define S_IEXEC 0x0040

#define S_ISCHR(nd) (((nd) & S_IFMT) == S_IFCHR)
#define S_ISDIR(nd) (((nd) & S_IFMT) == S_IFDIR)
#define S_ISFIFO(nd) (((nd) & S_IFMT) == S_IFIFO)
#define S_ISREG(nd) (((nd) & S_IFMT) == S_IFREG)


int stat(const char *, struct stat*);

int _fstati64(int, struct stat*);

int fstat(int, struct stat *);
#define _fstat fstat

int _wstati64(const wchar_t,struct _stati64 *buffer);  



#endif
=== timeb.h 58/73 ===
/* timeb.h */

#ifndef $timeb
#define $timeb

#include <time.h>

struct _timeb {
	time_t time;
	unsigned short millitm;
	short timezone;
	short dstflag;
};
#define timeb _timeb

void _ftime64(struct _timeb*);
#define _ftime _ftime64
#define ftime _ftime64

#endif
=== utime.h 59/73 ===
/* sys/utime.h header */

#ifndef $utime
#define $utime

struct _utimbuf {
	long actime;
	long modtime;
};


#endif
=== memory.h 60/73 ===
#include <malloc.h>
=== windows.h 61/73 ===
#ifndef $windows
#define $windows 1

#include <stdarg.h>
#include <stddef.h>

#define IMAGE_NT_SIGNATURE
#define DUMMYUNIONNAME

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16
//#define IMAGE_SIZEOF_SHORT_NAME 8

//#define IMAGE_DIRECTORY_ENTRY_BASERELOC 5

#define IMAGE_DOS_SIGNATURE 0x5A4D
#define IMAGE_OS2_SIGNATURE 0x454E
#define IMAGE_OS2_SIGNATURE_LE 0x454C
#define IMAGE_VXD_SIGNATURE 0x454C
#define IMAGE_NT_SIGNATURE 0x4550
#define IMAGE_SIZEOF_FILE_HEADER 20
#define IMAGE_FILE_RELOCS_STRIPPED 1
#define IMAGE_FILE_EXECUTABLE_IMAGE 2
#define IMAGE_FILE_LINE_NUMS_STRIPPED 4
#define IMAGE_FILE_LOCAL_SYMS_STRIPPED 8
#define IMAGE_FILE_BYTES_REVERSED_LO 128
#define IMAGE_FILE_32BIT_MACHINE 256
#define IMAGE_FILE_DEBUG_STRIPPED 512
#define IMAGE_FILE_SYSTEM 0x1000
#define IMAGE_FILE_DLL 0x2000
#define IMAGE_FILE_BYTES_REVERSED_HI 0x8000
#define IMAGE_FILE_MACHINE_UNKNOWN 0
#define IMAGE_FILE_MACHINE_I386 0x14c
#define IMAGE_FILE_MACHINE_R3000 0x162
#define IMAGE_FILE_MACHINE_R4000 0x166
#define IMAGE_FILE_MACHINE_R10000 0x168
#define IMAGE_FILE_MACHINE_ALPHA 0x184
#define IMAGE_FILE_MACHINE_POWERPC 0x1F0
#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16
#define IMAGE_SIZEOF_ROM_OPTIONAL_HEADER 56
#define IMAGE_SIZEOF_STD_OPTIONAL_HEADER 28
#define IMAGE_SIZEOF_NT_OPTIONAL_HEADER 224
#define IMAGE_NT_OPTIONAL_HDR_MAGIC 0x10b
#define IMAGE_ROM_OPTIONAL_HDR_MAGIC 0x107
#define IMAGE_FIRST_SECTION(nth) ((PIMAGE_SECTION_HEADER) \
 ((DWORD)nth + FIELD_OFFSET( IMAGE_NT_HEADERS,OptionalHeader ) + \
 ((PIMAGE_NT_HEADERS)(nth))->FileHeader.SizeOfOptionalHeader))
#define IMAGE_SUBSYSTEM_UNKNOWN 0
#define IMAGE_SUBSYSTEM_NATIVE 1
#define IMAGE_SUBSYSTEM_WINDOWS_GUI 2
#define IMAGE_SUBSYSTEM_WINDOWS_CUI 3
#define IMAGE_SUBSYSTEM_OS2_CUI 5
#define IMAGE_SUBSYSTEM_POSIX_CUI 7
#define IMAGE_SUBSYSTEM_WINDOWS_CE_GUI 9
#define IMAGE_DIRECTORY_ENTRY_EXPORT 0
#define IMAGE_DIRECTORY_ENTRY_IMPORT 1
#define IMAGE_DIRECTORY_ENTRY_RESOURCE 2
#define IMAGE_DIRECTORY_ENTRY_EXCEPTION 3
#define IMAGE_DIRECTORY_ENTRY_SECURITY 4
#define IMAGE_DIRECTORY_ENTRY_BASERELOC 5
#define IMAGE_DIRECTORY_ENTRY_DEBUG 6
#define IMAGE_DIRECTORY_ENTRY_COPYRIGHT 7
#define IMAGE_DIRECTORY_ENTRY_GLOBALPTR 8
#define IMAGE_DIRECTORY_ENTRY_TLS 9
#define IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG 10
#define IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT 11
#define IMAGE_DIRECTORY_ENTRY_IAT 12
#define IMAGE_SIZEOF_SHORT_NAME 8
#define IMAGE_SIZEOF_SECTION_HEADER 40
#define IMAGE_SCN_TYPE_NO_PAD 8
#define IMAGE_SCN_CNT_CODE 32
#define IMAGE_SCN_CNT_INITIALIZED_DATA 64
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA 128
#define IMAGE_SCN_LNK_OTHER 256
#define IMAGE_SCN_LNK_INFO 512
#define IMAGE_SCN_LNK_REMOVE 0x800
#define IMAGE_SCN_LNK_COMDAT 0x1000
#define IMAGE_SCN_MEM_FARDATA 0x8000
#define IMAGE_SCN_MEM_PURGEABLE 0x20000
#define IMAGE_SCN_MEM_16BIT 0x20000
#define IMAGE_SCN_MEM_LOCKED 0x40000
#define IMAGE_SCN_MEM_PRELOAD 0x80000
#define IMAGE_SCN_ALIGN_1BYTES 0x100000
#define IMAGE_SCN_ALIGN_2BYTES 0x200000
#define IMAGE_SCN_ALIGN_4BYTES 0x300000
#define IMAGE_SCN_ALIGN_8BYTES 0x400000
#define IMAGE_SCN_ALIGN_16BYTES 0x500000
#define IMAGE_SCN_ALIGN_32BYTES 0x600000
#define IMAGE_SCN_ALIGN_64BYTES 0x700000
#define IMAGE_SCN_LNK_NRELOC_OVFL 0x1000000
#define IMAGE_SCN_MEM_DISCARDABLE 0x2000000
#define IMAGE_SCN_MEM_NOT_CACHED 0x4000000
#define IMAGE_SCN_MEM_NOT_PAGED 0x8000000
#define IMAGE_SCN_MEM_SHARED 0x10000000
#define IMAGE_SCN_MEM_EXECUTE 0x20000000
#define IMAGE_SCN_MEM_READ 0x40000000
#define IMAGE_SCN_MEM_WRITE 0x80000000
#define IMAGE_SIZEOF_SYMBOL 18
#define IMAGE_SYM_UNDEFINED (SHORT)0
#define IMAGE_SYM_ABSOLUTE (SHORT)-1
#define IMAGE_SYM_DEBUG (SHORT)-2
#define IMAGE_SYM_TYPE_NULL 0
#define IMAGE_SYM_TYPE_VOID 1
#define IMAGE_SYM_TYPE_CHAR 2
#define IMAGE_SYM_TYPE_SHORT 3
#define IMAGE_SYM_TYPE_INT 4
#define IMAGE_SYM_TYPE_LONG 5
#define IMAGE_SYM_TYPE_FLOAT 6
#define IMAGE_SYM_TYPE_DOUBLE 7
#define IMAGE_SYM_TYPE_STRUCT 8
#define IMAGE_SYM_TYPE_UNION 9
#define IMAGE_SYM_TYPE_ENUM 10
#define IMAGE_SYM_TYPE_MOE 11
#define IMAGE_SYM_TYPE_BYTE 12
#define IMAGE_SYM_TYPE_WORD 13
#define IMAGE_SYM_TYPE_UINT 14
#define IMAGE_SYM_TYPE_DWORD 15
#define IMAGE_SYM_TYPE_PCODE 0x8000
#define IMAGE_SYM_DTYPE_NULL 0
#define IMAGE_SYM_DTYPE_POINTER 1
#define IMAGE_SYM_DTYPE_FUNCTION 2
#define IMAGE_SYM_DTYPE_ARRAY 3
#define IMAGE_SYM_CLASS_END_OF_FUNCTION (BYTE )-1
#define IMAGE_SYM_CLASS_NULL 0
#define IMAGE_SYM_CLASS_AUTOMATIC 1
#define IMAGE_SYM_CLASS_EXTERNAL 2
#define IMAGE_SYM_CLASS_STATIC 3
#define IMAGE_SYM_CLASS_REGISTER 4
#define IMAGE_SYM_CLASS_EXTERNAL_DEF 5
#define IMAGE_SYM_CLASS_LABEL 6
#define IMAGE_SYM_CLASS_UNDEFINED_LABEL 7
#define IMAGE_SYM_CLASS_MEMBER_OF_STRUCT 8
#define IMAGE_SYM_CLASS_ARGUMENT 9
#define IMAGE_SYM_CLASS_STRUCT_TAG 10
#define IMAGE_SYM_CLASS_MEMBER_OF_UNION 11
#define IMAGE_SYM_CLASS_UNION_TAG 12
#define IMAGE_SYM_CLASS_TYPE_DEFINITION 13
#define IMAGE_SYM_CLASS_UNDEFINED_STATIC 14
#define IMAGE_SYM_CLASS_ENUM_TAG 15
#define IMAGE_SYM_CLASS_MEMBER_OF_ENUM 16
#define IMAGE_SYM_CLASS_REGISTER_PARAM 17
#define IMAGE_SYM_CLASS_BIT_FIELD 18
#define IMAGE_SYM_CLASS_FAR_EXTERNAL 0x44
#define IMAGE_SYM_CLASS_BLOCK 0x64
#define IMAGE_SYM_CLASS_FUNCTION 0x65
#define IMAGE_SYM_CLASS_END_OF_STRUCT 0x66
#define IMAGE_SYM_CLASS_FILE 0x67
#define IMAGE_SYM_CLASS_SECTION 0x68
#define IMAGE_SYM_CLASS_WEAK_EXTERNAL 0x69



#define MAKEWORD(a,b)	((WORD)(((BYTE)(((DWORD_PTR)(a))&0xFF))|(((WORD)((BYTE)(((DWORD_PTR)(b))&0xFF)))<<8)))
#define LOBYTE(w)	((BYTE)(((DWORD_PTR)(w))&0xFF))
#define HIBYTE(w)	((BYTE)((((DWORD_PTR)(w))>>8)&0xFF))

#define MAKELANGID(p,s)	((((WORD)(s))<<10)|(WORD)(p))
#define SUBLANG_DEFAULT	0x01
#define LANG_NEUTRAL 0x00


//#define __int64 long long int
#define TA_BASELINE	24
#define TA_BOTTOM	8
#define TA_TOP	0
#define TA_CENTER	6
#define TA_LEFT	0
#define TA_RIGHT	2
#define TA_RTLREADING	256
#define TA_MASK	(TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING)
#define TA_NOUPDATECP	0
#define TA_UPDATECP	1

#define GetRValue(rgb) ((BYTE) (rgb))
#define GetGValue(rgb) ((BYTE) (((WORD) (rgb)) >> 8))
#define GetBValue(rgb) ((BYTE) ((rgb) >> 16))

#define ANSI_CHARSET 0

#define OUT_DEFAULT_PRECIS	0
#define OUT_STRING_PRECIS	1
#define OUT_CHARACTER_PRECIS	2
#define OUT_STROKE_PRECIS	3
#define OUT_TT_PRECIS	4
#define OUT_DEVICE_PRECIS	5
#define OUT_RASTER_PRECIS	6
#define OUT_TT_ONLY_PRECIS	7
#define OUT_OUTLINE_PRECIS	8
#define CLIP_DEFAULT_PRECIS	0
#define CLIP_CHARACTER_PRECIS	1
#define CLIP_STROKE_PRECIS	2
#define CLIP_MASK	15
#define CLIP_LH_ANGLES	16
#define CLIP_TT_ALWAYS	32
#define CLIP_EMBEDDED	128
#define DEFAULT_QUALITY	0
#define DRAFT_QUALITY	1
#define PROOF_QUALITY	2
#define NONANTIALIASED_QUALITY	3
#define ANTIALIASED_QUALITY	4
#define DEFAULT_PITCH	0
#define FIXED_PITCH	1
#define VARIABLE_PITCH	2
#define FF_DECORATIVE	80
#define FF_DONTCARE	0
#define FF_MODERN	48
#define FF_ROMAN	16
#define FF_SCRIPT	64
#define FF_SWISS	32
#define HS_BDIAGONAL	3
#define HS_CROSS	4
#define HS_DIAGCROSS	5
#define HS_FDIAGONAL	2
#define HS_HORIZONTAL	0
#define HS_VERTICAL	1
#define LR_DEFAULTCOLOR	0
#define LR_LOADREALSIZE	128
#define LR_MONOCHROME	1
#define LR_COPYRETURNORG 4
#define LR_COPYDELETEORG 8
#define LR_LOADFROMFILE	16
#define LR_LOADTRANSPARENT 32
#define LR_DEFAULTSIZE	64
#define LR_VGACOLOR	0x80
#define LR_LOADMAP3DCOLORS 0x1000
#define LR_CREATEDIBSECTION 0x2000
#define LR_COPYFROMRESOURCE 0x4000
#define LR_SHARED	0x8000

#define FW_DONTCARE	0
#define FW_THIN	100
#define FW_EXTRALIGHT	200
#define FW_LIGHT	300
#define FW_NORMAL	400
#define FW_REGULAR	400
#define FW_MEDIUM	500
#define FW_SEMIBOLD	600
#define FW_BOLD	700
#define FW_EXTRABOLD	800
#define FW_HEAVY	900

#define	SM_CXSCREEN	0
#define	SM_CYSCREEN	1
#define	SM_CXVSCROLL	2
#define	SM_CYHSCROLL	3
#define	SM_CYCAPTION	4
#define	SM_CXBORDER	5
#define	SM_CYBORDER	6
#define	SM_CXDLGFRAME	7
#define	SM_CYDLGFRAME	8
#define	SM_CYVTHUMB	9
#define	SM_CXHTHUMB	10
#define	SM_CXICON	11
#define	SM_CYICON	12
#define	SM_CXCURSOR	13
#define	SM_CYCURSOR	14
#define	SM_CYMENU	15
#define	SM_CXFULLSCREEN	16
#define	SM_CYFULLSCREEN	17
#define	SM_CYKANJIWINDOW	18
#define	SM_MOUSEPRESENT	19
#define	SM_CYVSCROLL	20
#define	SM_CXHSCROLL	21
#define	SM_DEBUG	22
#define	SM_SWAPBUTTON	23
#define	SM_RESERVED1	24
#define	SM_RESERVED2	25
#define	SM_RESERVED3	26
#define	SM_RESERVED4	27
#define	SM_CXMIN	28
#define	SM_CYMIN	29
#define	SM_CXSIZE	30
#define	SM_CYSIZE	31
#define	SM_CXFRAME	32
#define	SM_CYFRAME	33
#define	SM_CXMINTRACK	34
#define	SM_CYMINTRACK	35
#define	SM_CXDOUBLECLK	36
#define	SM_CYDOUBLECLK	37
#define	SM_CXICONSPACING	38
#define	SM_CYICONSPACING	39
#define	SM_MENUDROPALIGNMENT	40
#define	SM_PENWINDOWS	41
#define	SM_DBCSENABLED	42
#define	SM_CMOUSEBUTTONS	43
#define	SM_CXFIXEDFRAME	SM_CXDLGFRAME
#define	SM_CYFIXEDFRAME	SM_CYDLGFRAME
#define	SM_CXSIZEFRAME	SM_CXFRAME
#define	SM_CYSIZEFRAME	SM_CYFRAME
#define	SM_SECURE	44
#define	SM_CXEDGE	45
#define	SM_CYEDGE	46
#define	SM_CXMINSPACING	47
#define	SM_CYMINSPACING	48
#define	SM_CXSMICON	49
#define	SM_CYSMICON	50
#define	SM_CYSMCAPTION	51
#define	SM_CXSMSIZE	52
#define	SM_CYSMSIZE	53
#define	SM_CXMENUSIZE	54
#define	SM_CYMENUSIZE	55
#define	SM_ARRANGE	56
#define	SM_CXMINIMIZED	57
#define	SM_CYMINIMIZED	58
#define	SM_CXMAXTRACK	59
#define	SM_CYMAXTRACK	60
#define	SM_CXMAXIMIZED	61
#define	SM_CYMAXIMIZED	62
#define	SM_NETWORK	63
#define	SM_CLEANBOOT	67
#define	SM_CXDRAG	68
#define	SM_CYDRAG	69
#define	SM_SHOWSOUNDS	70
#define	SM_CXMENUCHECK	71
#define	SM_CYMENUCHECK	72
#define	SM_SLOWMACHINE	73
#define	SM_MIDEASTENABLED	74
#define	SM_MOUSEWHEELPRESENT	75
#define	SM_XVIRTUALSCREEN	76
#define	SM_YVIRTUALSCREEN	77
#define	SM_CXVIRTUALSCREEN	78
#define	SM_CYVIRTUALSCREEN	79
#define	SM_CMONITORS	80
#define	SM_SAMEDISPLAYFORMAT	81
#define	SM_CMETRICS	76

#define SWP_DRAWFRAME	32
#define SWP_FRAMECHANGED	32
#define SWP_HIDEWINDOW	128
#define SWP_NOACTIVATE	16
#define SWP_NOCOPYBITS	256
#define SWP_NOMOVE	2
#define SWP_NOSIZE	1
#define SWP_NOREDRAW	8
#define SWP_NOZORDER	4
#define SWP_SHOWWINDOW	64
#define SWP_NOOWNERZORDER	512
#define SWP_NOREPOSITION	512
#define SWP_NOSENDCHANGING	1024

#define LF_FACESIZE	32
#define LF_FULLFACESIZE	64

#define FOREGROUND_BLUE	1
#define FOREGROUND_GREEN	2
#define FOREGROUND_RED	4
#define FOREGROUND_INTENSITY	8
#define BACKGROUND_BLUE	16
#define BACKGROUND_GREEN	32
#define BACKGROUND_RED	64

#define CTRL_C_EVENT	0
#define CTRL_BREAK_EVENT	1
#define CTRL_CLOSE_EVENT	2
#define CTRL_LOGOFF_EVENT	5
#define CTRL_SHUTDOWN_EVENT	6

#define INVALID_HANDLE_VALUE (HANDLE)(0xFFFFFFFFFFFFFFFF)
#define INVALID_FILE_SIZE ((DWORD)0xFFFFFFFFFFFFFFFF)
#define INVALID_SET_FILE_POINTER ((DWORD)-1)
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)

#define Int32x32To64(a,b)(((long long)((long)(a))) *((long long)((long)(b))))

#define PAGE_READONLY 2
#define PAGE_READWRITE 4
#define PAGE_WRITECOPY 8

#define FILE_BEGIN 0
#define FILE_CURRENT 1
#define FILE_END 2

#define FILE_READ_DATA	1
#define FILE_LIST_DIRECTORY	1
#define FILE_WRITE_DATA	2
#define FILE_ADD_FILE	2
#define FILE_APPEND_DATA	4
#define FILE_ADD_SUBDIRECTORY	4
#define FILE_CREATE_PIPE_INSTANCE 4
#define FILE_READ_EA	8
#define FILE_WRITE_EA	0x10
#define FILE_EXECUTE	0x20
#define FILE_TRAVERSE	0x0020
#define FILE_DELETE_CHILD	0x0040
#define FILE_READ_ATTRIBUTES	0x0080
#define FILE_WRITE_ATTRIBUTES	0x100
#define FILE_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE|0x1FF)
#define FILE_GENERIC_READ (STANDARD_RIGHTS_READ|FILE_READ_DATA| FILE_READ_ATTRIBUTES|FILE_READ_EA|SYNCHRONIZE)
#define FILE_GENERIC_WRITE (STANDARD_RIGHTS_WRITE|FILE_WRITE_DATA|FILE_WRITE_ATTRIBUTES|FILE_WRITE_EA|FILE_APPEND_DATA|SYNCHRONIZE)
#define FILE_GENERIC_EXECUTE (STANDARD_RIGHTS_EXECUTE|FILE_READ_ATTRIBUTES|FILE_EXECUTE|SYNCHRONIZE)

#define FILE_FLAG_WRITE_THROUGH 0x80000000
#define FILE_FLAG_OVERLAPPED 1073741824
#define FILE_FLAG_NO_BUFFERING 536870912
#define FILE_FLAG_RANDOM_ACCESS 268435456
#define FILE_FLAG_SEQUENTIAL_SCAN 134217728
#define FILE_FLAG_DELETE_ON_CLOSE 67108864
#define FILE_FLAG_BACKUP_SEMANTICS 33554432
#define FILE_FLAG_POSIX_SEMANTICS 16777216
#define FILE_FLAG_OPEN_REPARSE_POINT 0x200000
#define FILE_FLAG_OPEN_NO_RECALL 0x100000
#define FILE_FLAG_FIRST_PIPE_INSTANCE 0x00080000

#define SMTO_ABORTIFHUNG 2
#define SMTO_BLOCK 1
#define SMTO_NORMAL 0

#define FORMAT_MESSAGE_ALLOCATE_BUFFER 256
#define FORMAT_MESSAGE_IGNORE_INSERTS 512
#define FORMAT_MESSAGE_FROM_STRING 1024
#define FORMAT_MESSAGE_FROM_HMODULE 2048
#define FORMAT_MESSAGE_FROM_SYSTEM 4096
#define FORMAT_MESSAGE_ARGUMENT_ARRAY 8192
#define FORMAT_MESSAGE_MAX_WIDTH_MASK 255

#define HWND_BOTTOM (HWND)1
#define HWND_NOTOPMOST (HWND)(-2)
#define HWND_TOP (HWND)0
#define HWND_TOPMOST (HWND)(-1)
#define HWND_BROADCAST (HWND)65535
#define VER_PLATFORM_WIN32s 0
#define VER_PLATFORM_WIN32_WINDOWS 1
#define VER_PLATFORM_WIN32_NT 2

#define CP_ACP 0
#define CP_MACCP 2
#define CP_OEMCP 1
#define CP_THREAD_ACP 3
#define CP_SYMBOL 42
#define CP_UTF7 65000
#define CP_UTF8 65001

#define WAIT_OBJECT_0 0
#define WAIT_ABANDONED_0 0x80
#define WAIT_TIMEOUT 0x102
#define WAIT_IO_COMPLETION 0xC0
#define WAIT_ABANDONED 0x80
#define WAIT_FAILED 0xFFFFFFFF

#define GENERIC_READ  0x80000000
#define GENERIC_WRITE 0x40000000
#define FILE_SHARE_READ 1
#define FILE_SHARE_WRITE 2
#define FILE_SHARE_DELETE 4
#define CREATE_NEW 1
#define CREATE_ALWAYS 2
#define OPEN_EXISTING 3
#define OPEN_ALWAYS 4
#define TRUNCATE_EXISTING 5

//#define CALLBACK
#define WINAPI
#define UINT32 unsigned int
#define HANDLE_FLAG_INHERIT 1

#define STARTF_USESHOWWINDOW 1
#define STARTF_USESIZE 2
#define STARTF_USEPOSITION 4
#define STARTF_USECOUNTCHARS 8
#define STARTF_USEFILLATTRIBUTE 16
#define STARTF_RUNFULLSCREEN 32
#define STARTF_FORCEONFEEDBACK 64
#define STARTF_FORCEOFFFEEDBACK 128
#define STARTF_USESTDHANDLES 256
#define STARTF_USEHOTKEY 512

#define FILE_ATTRIBUTE_ARCHIVE 32
#define FILE_ATTRIBUTE_COMPRESSED 2048
#define FILE_ATTRIBUTE_NORMAL 128
#define FILE_ATTRIBUTE_DIRECTORY 16
#define FILE_ATTRIBUTE_HIDDEN 2
#define FILE_ATTRIBUTE_READONLY 1
#define FILE_ATTRIBUTE_SYSTEM 4
#define FILE_ATTRIBUTE_TEMPORARY 256
#define FILE_ATTRIBUTE_SPARSE_FILE 0x200
#define FILE_ATTRIBUTE_REPARSE_POINT 0x400
#define FILE_ATTRIBUTE_OFFLINE 0x1000
#define FILE_ATTRIBUTE_NOT_CONTENT_INDEXED 0x00002000
#define FILE_ATTRIBUTE_ENCRYPTED 0x4000

#define CREATE_UNICODE_ENVIRONMENT 1024

#define STILL_ACTIVE 0x103
#define FALSE 0
#define TRUE 1
#define INFINITE (-1)

#define CF_TEXT 1

#define GMEM_FIXED 0
#define GMEM_MOVEABLE 2
#define GMEM_MODIFY 128
#define GMEM_VALID_FLAGS 0x7F72

#define DIB_PAL_COLORS 1
#define DIB_RGB_COLORS 0

#define BI_RGB 0
#define BI_RLE8 1
#define BI_RLE4 2
#define BI_BITFIELDS 3
#define BI_JPEG 4
#define BI_PNG 5

#define MK_CONTROL 8
#define MK_LBUTTON 1
#define MK_MBUTTON 16
#define MK_RBUTTON 2
#define MK_SHIFT 4

#define MAKEINTRESOURCE(i) (LPSTR) ((DWORD) ((WORD) (i)))
#define MAKELONG(a,b) ((LONG) (((WORD) (a)) | ((DWORD) ((WORD) (b))) << 16))
#define MAKELPARAM(l,h) ((LPARAM) MAKELONG(l,h))
#define RGB(r,g,b) ((DWORD)(((BYTE)(r)|((WORD)(g) << 8))|(((DWORD)(BYTE)(b)) << 16)))

#define BLACKNESS 0x42
#define NOTSRCERASE 0x1100A6
#define NOTSRCCOPY 0x330008
#define SRCERASE 0x440328
#define DSTINVERT 0x550009
#define PATINVERT 0x5A0049
#define SRCINVERT 0x660046
#define SRCAND 0x8800C6
#define MERGEPAINT 0xBB0226
#define MERGECOPY 0xC000CA
#define SRCCOPY 0xCC0020
#define SRCPAINT 0xEE0086
#define PATCOPY 0xF00021
#define PATPAINT 0xFB0A09
#define WHITENESS 0xFF0062
#define NOMIRRORBITMAP 0x80000000
#define CAPTUREBLT 0x40000000

#define GWL_EXSTYLE (-20)
#define GWL_STYLE (-16)
#define GWL_WNDPROC (-4)
#define GWL_HINSTANCE (-6)
#define GWL_HWNDPARENT (-8)
#define GWL_ID (-12)
#define GWL_USERDATA (-21)
#define GWLP_WNDPROC (-4)
#define GWLP_HINSTANCE (-6)
#define GWLP_HWNDPARENT (-8)
#define GWLP_USERDATA (-21)
#define GWLP_ID (-12)

#define COLOR_HOTLIGHT 26
#define COLOR_GRADIENTACTIVECAPTION 27
#define COLOR_GRADIENTINACTIVECAPTION 28
#define COLOR_MENUHILIGHT 29
#define COLOR_MENUBAR 30
#define COLOR_3DDKSHADOW 21
#define COLOR_3DFACE 15
#define COLOR_3DHILIGHT 20
#define COLOR_3DHIGHLIGHT 20
#define COLOR_3DLIGHT 22
#define COLOR_BTNHILIGHT 20
#define COLOR_3DSHADOW 16
#define COLOR_ACTIVEBORDER 10
#define COLOR_ACTIVECAPTION 2
#define COLOR_APPWORKSPACE 12
#define COLOR_BACKGROUND 1
#define COLOR_DESKTOP 1
#define COLOR_BTNFACE 15
#define COLOR_BTNHIGHLIGHT 20
#define COLOR_BTNSHADOW 16
#define COLOR_BTNTEXT 18
#define COLOR_CAPTIONTEXT 9
#define COLOR_GRAYTEXT 17
#define COLOR_HIGHLIGHT 13
#define COLOR_HIGHLIGHTTEXT 14
#define COLOR_INACTIVEBORDER 11
#define COLOR_INACTIVECAPTION 3
#define COLOR_INACTIVECAPTIONTEXT 19
#define COLOR_INFOBK 24
#define COLOR_INFOTEXT 23
#define COLOR_MENU 4
#define COLOR_MENUTEXT 7
#define COLOR_SCROLLBAR 0
#define COLOR_WINDOW 5
#define COLOR_WINDOWFRAME 6
#define COLOR_WINDOWTEXT 8

#define SW_HIDE 0
#define SW_NORMAL 1
#define SW_MAXIMIZE 3
#define SW_MINIMIZE 6
#define SW_RESTORE 9
#define SW_SHOW 5
#define SW_SHOWDEFAULT 10
#define SW_SHOWMAXIMIZED 3
#define SW_SHOWMINIMIZED 2
#define SW_SHOWMINNOACTIVE 7
#define SW_SHOWNA 8
#define SW_SHOWNOACTIVATE 4
#define SW_SHOWNORMAL 1

#define PS_GEOMETRIC 65536
#define PS_COSMETIC 0
#define PS_ALTERNATE 8
#define PS_SOLID 0
#define PS_DASH 1
#define PS_DOT 2
#define PS_DASHDOT 3
#define PS_DASHDOTDOT 4
#define PS_NULL 5
#define PS_USERSTYLE 7
#define PS_INSIDEFRAME 6
#define PS_ENDCAP_ROUND 0
#define PS_ENDCAP_SQUARE 256
#define PS_ENDCAP_FLAT 512
#define PS_JOIN_BEVEL 4096
#define PS_JOIN_MITER 8192
#define PS_JOIN_ROUND 0
#define PS_STYLE_MASK 15
#define PS_ENDCAP_MASK 3840
#define PS_TYPE_MASK 983040

#define HTBOTTOM 15
#define HTBOTTOMLEFT 16
#define HTBOTTOMRIGHT 17
#define HTCAPTION 2
#define HTCLIENT 1
#define HTERROR (-2)
#define HTGROWBOX 4
#define HTHSCROLL 6
#define HTLEFT 10
#define HTMENU 5
#define HTNOWHERE 0
#define HTREDUCE 8
#define HTRIGHT 11
#define HTSIZE 4
#define HTSYSMENU 3
#define HTTOP 12
#define HTTOPLEFT 13
#define HTTOPRIGHT 14
#define HTTRANSPARENT (-1)
#define HTVSCROLL 7
#define HTZOOM 9
#define HTOBJECT 19
#define HTCLOSE 20
#define HTHELP 21
#define HTBORDER 18
#define HTMINBUTTON 8
#define HTMAXBUTTON 9

#define VK_LBUTTON 1
#define VK_RBUTTON 2
#define VK_CANCEL 3
#define VK_MBUTTON 4
#define VK_BACK 8
#define VK_TAB 9
#define VK_CLEAR 12
#define VK_RETURN 13
#define VK_SHIFT 16
#define VK_CONTROL 17
#define VK_MENU 18
#define VK_PAUSE 19
#define VK_PRINT 42
#define VK_CAPITAL 20
#define VK_KANA 0x15
#define VK_HANGEUL 0x15
#define VK_HANGUL 0x15
#define VK_JUNJA 0x17
#define VK_FINAL 0x18
#define VK_HANJA 0x19
#define VK_KANJI 0x19
#define VK_CONVERT 0x1C
#define VK_NONCONVERT 0x1D
#define VK_ACCEPT 0x1E
#define VK_MODECHANGE 0x1F
#define VK_ESCAPE 27
#define VK_SPACE 32
#define VK_PRIOR 33
#define VK_NEXT 34
#define VK_END 35
#define VK_HOME 36
#define VK_LEFT 37
#define VK_UP 38
#define VK_RIGHT 39
#define VK_DOWN 40
#define VK_SELECT 41
#define VK_EXECUTE 43
#define VK_SNAPSHOT 44
#define VK_INSERT 45
#define VK_DELETE 46
#define VK_HELP 47
#define VK_0 48
#define VK_1 49
#define VK_2 50
#define VK_3 51
#define VK_4 52
#define VK_5 53
#define VK_6 54
#define VK_7 55
#define VK_8 56
#define VK_9 57
#define VK_A 65
#define VK_B 66
#define VK_C 67
#define VK_D 68
#define VK_E 69
#define VK_F 70
#define VK_G 71
#define VK_H 72
#define VK_I 73
#define VK_J 74
#define VK_K 75
#define VK_L 76
#define VK_M 77
#define VK_N 78
#define VK_O 79
#define VK_P 80
#define VK_Q 81
#define VK_R 82
#define VK_S 83
#define VK_T 84
#define VK_U 85
#define VK_V 86
#define VK_W 87
#define VK_X 88
#define VK_Y 89
#define VK_Z 90
#define VK_LWIN 0x5B
#define VK_RWIN 0x5C
#define VK_APPS 0x5D
#define VK_NUMPAD0 96
#define VK_NUMPAD1 97
#define VK_NUMPAD2 98
#define VK_NUMPAD3 99
#define VK_NUMPAD4 100
#define VK_NUMPAD5 101
#define VK_NUMPAD6 102
#define VK_NUMPAD7 103
#define VK_NUMPAD8 104
#define VK_NUMPAD9 105
#define VK_MULTIPLY 106
#define VK_ADD 107
#define VK_SEPARATOR 108
#define VK_SUBTRACT 109
#define VK_DECIMAL 110
#define VK_DIVIDE 111
#define VK_F1 112
#define VK_F2 113
#define VK_F3 114
#define VK_F4 115
#define VK_F5 116
#define VK_F6 117
#define VK_F7 118
#define VK_F8 119
#define VK_F9 120
#define VK_F10 121
#define VK_F11 122
#define VK_F12 123
#define VK_F13 124
#define VK_F14 125
#define VK_F15 126
#define VK_F16 127
#define VK_F17 128
#define VK_F18 129
#define VK_F19 130
#define VK_F20 131
#define VK_F21 132
#define VK_F22 133
#define VK_F23 134
#define VK_F24 135
#define VK_NUMLOCK 144
#define VK_SCROLL 145
#define VK_LSHIFT 160
#define VK_LCONTROL 162
#define VK_LMENU 164
#define VK_RSHIFT 161
#define VK_RCONTROL 163
#define VK_RMENU 165

#define FILE_TYPE_UNKNOWN 0
#define FILE_TYPE_DISK 1
#define FILE_TYPE_CHAR 2
#define FILE_TYPE_PIPE 3
#define FILE_TYPE_REMOTE 0x8000

#define FILE_MAP_ALL_ACCESS 0xf001f
#define FILE_MAP_READ 4
#define FILE_MAP_WRITE 2
#define FILE_MAP_COPY 1

#define PM_NOREMOVE 0
#define PM_REMOVE 1
#define PM_NOYIELD 2

#define LOWORD(l) (((WORD) (l)) & 0xFFFF)
#define HIWORD(l) ((WORD) (((DWORD) (l) >> 16) & 0xFFFF))

//#define ERROR_BROKEN_PIPE 109

#define WM_NULL 0
#define WM_APP 0x8000
#define WM_ACTIVATE 6
#define WM_ACTIVATEAPP 28
#define WM_ASKCBFORMATNAME 780
#define WM_CANCELJOURNAL 75
#define WM_CANCELMODE 31
#define WM_CAPTURECHANGED 533
#define WM_CHANGECBCHAIN 781
#define WM_CHAR 258
#define WM_CHARTOITEM 47
#define WM_CHILDACTIVATE 34
#define WM_CHOOSEFONT_GETLOGFONT 1025
#define WM_CHOOSEFONT_SETLOGFONT 1125
#define WM_CHOOSEFONT_SETFLAGS 1126
#define WM_CLEAR 771
#define WM_CLOSE 16

//!=================

#define WM_COMMAND 273
#define WM_COMPACTING 65
#define WM_COMPAREITEM 57
#define WM_CONTEXTMENU 123
#define WM_COPY 769
#define WM_COPYDATA 74
#define WM_CREATE 1
#define WM_CTLCOLORBTN 309
#define WM_CTLCOLORDLG 310
#define WM_CTLCOLOREDIT 307
#define WM_CTLCOLORLISTBOX 308
#define WM_CTLCOLORMSGBOX 306
#define WM_CTLCOLORSCROLLBAR 311
#define WM_CTLCOLORSTATIC 312
#define WM_CUT 768
#define WM_DEADCHAR 259
#define WM_DELETEITEM 45
#define WM_DESTROY 2
#define WM_DESTROYCLIPBOARD 775
#define WM_DEVICECHANGE 537
#define WM_DEVMODECHANGE 27
#define WM_DISPLAYCHANGE 126
#define WM_DRAWCLIPBOARD 776
#define WM_DRAWITEM 43
#define WM_DROPFILES 563
#define WM_ENABLE 10
#define WM_ENDSESSION 22
#define WM_ENTERIDLE 289
#define WM_ENTERMENULOOP 529
#define WM_ENTERSIZEMOVE 561
#define WM_ERASEBKGND 20
#define WM_EXITMENULOOP 530
#define WM_EXITSIZEMOVE 562
#define WM_FONTCHANGE 29
#define WM_GETDLGCODE 135
#define WM_GETFONT 49
#define WM_GETHOTKEY 51
#define WM_GETICON 127
#define WM_GETMINMAXINFO 36
#define WM_GETTEXT 13
#define WM_GETTEXTLENGTH 14
#define WM_HELP 83
#define WM_HOTKEY 786
#define WM_HSCROLL 276
#define WM_HSCROLLCLIPBOARD 782
#define WM_ICONERASEBKGND 39
#define WM_IME_CHAR 646
#define WM_IME_COMPOSITION 271
#define WM_IME_COMPOSITIONFULL 644
#define WM_IME_CONTROL 643
#define WM_IME_ENDCOMPOSITION 270
#define WM_IME_KEYDOWN 656
#define WM_IME_KEYUP 657
#define WM_IME_NOTIFY 642
#define WM_IME_SELECT 645
#define WM_IME_SETCONTEXT 641
#define WM_IME_STARTCOMPOSITION 269
#define WM_INITDIALOG 272
#define WM_INITMENU 278
#define WM_INITMENUPOPUP 279
#define WM_INPUTLANGCHANGE 81
#define WM_INPUTLANGCHANGEREQUEST 80
#define WM_INPUT 0xff
#define WM_KEYDOWN 256
#define WM_KEYUP 257
#define WM_KILLFOCUS 8
#define WM_LBUTTONDBLCLK 515
#define WM_LBUTTONDOWN 513
#define WM_LBUTTONUP 514
#define WM_MBUTTONDBLCLK 521
#define WM_MBUTTONDOWN 519
#define WM_MBUTTONUP 520
#define WM_MDIACTIVATE 546
#define WM_MDICASCADE 551
#define WM_MDICREATE 544
#define WM_MDIDESTROY 545
#define WM_MDIGETACTIVE 553
#define WM_MDIICONARRANGE 552
#define WM_MDIMAXIMIZE 549
#define WM_MDINEXT 548
#define WM_MDIREFRESHMENU 564
#define WM_MDIRESTORE 547
#define WM_MDISETMENU 560
#define WM_MDITILE 550
#define WM_MEASUREITEM 44
#define WM_MENUCHAR 288
#define WM_MENUSELECT 287
#define WM_MENURBUTTONUP 0x0122
#define WM_MENUDRAG 0x0123
#define WM_MENUGETOBJECT 0x0124
#define WM_UNINITMENUPOPUP 0x0125
#define WM_MENUCOMMAND 0x0126
//#define WM_CHANGEUISTATE 0x0127
#define WM_UPDATEUISTATE 0x0128
#define WM_QUERYUISTATE 0x0129
#define UIS_SET 1
#define UIS_CLEAR 2
#define UIS_INITIALIZE  3
#define WM_MOUSEACTIVATE 33
#define WM_MOUSEMOVE 512
#define WM_MOUSEHOVER 0x2a1
#define WM_MOUSELEAVE 0x2a3
#define WM_MOUSEWHEEL 0x20A
#define WM_MOVE 3
#define WM_MOVING 534
#define WM_NCACTIVATE 134
#define WM_NCCALCSIZE 131
#define WM_NCCREATE 129
#define WM_NCDESTROY 130
#define WM_NCHITTEST 132
#define WM_NCLBUTTONDBLCLK 163
#define WM_NCLBUTTONDOWN 161
#define WM_NCLBUTTONUP 162
#define WM_NCMBUTTONDBLCLK 169
#define WM_NCMBUTTONDOWN 167
#define WM_NCMBUTTONUP 168
#define WM_NCMOUSEMOVE 160
#define WM_NCPAINT 133
#define WM_NCRBUTTONDBLCLK 166
#define WM_NCRBUTTONDOWN 164
#define WM_NCRBUTTONUP 165
#define WM_NEXTDLGCTL 40
#define WM_NOTIFY 78
#define WM_NOTIFYFORMAT 85
#define WM_PAINT 15
#define WM_PAINTCLIPBOARD 777
#define WM_PAINTICON 38
#define WM_PALETTECHANGED 785
#define WM_PALETTEISCHANGING 784
#define WM_PARENTNOTIFY 528
#define WM_PASTE 770
#define WM_PENWINFIRST 896
#define WM_SYNCPAINT 0x0088
#define WM_AFXFIRST 0x0360
#define WM_AFXLAST 0x037F
#define WM_HANDHELDFIRST 0x0358
#define WM_HANDHELDLAST 0x035F
#define WM_PENWINLAST 911
#define WM_POWER 72
#define WM_POWERBROADCAST 536
#define WM_PRINT 791
#define WM_PRINTCLIENT 792
#define WM_PSD_ENVSTAMPRECT 1029
#define WM_PSD_FULLPAGERECT 1025
#define WM_PSD_GREEKTEXTRECT 1028
#define WM_PSD_MARGINRECT 1027
#define WM_PSD_MINMARGINRECT 1026
#define WM_PSD_PAGESETUPDLG 1024
#define WM_PSD_YAFULLPAGERECT 1030
#define WM_QUERYDRAGICON 55
#define WM_QUERYENDSESSION 17
#define WM_QUERYNEWPALETTE 783
#define WM_QUERYOPEN 19
#define WM_QUEUESYNC 35
#define WM_QUIT 18
#define WM_RBUTTONDBLCLK 518
#define WM_RBUTTONDOWN 516
#define WM_RBUTTONUP 517
#define WM_RENDERALLFORMATS 774
#define WM_RENDERFORMAT 773
#define WM_SETCURSOR 32
#define WM_SETFOCUS 7
#define WM_SETFONT 48
#define WM_SETHOTKEY 50
#define WM_SETICON 128
#define WM_SETREDRAW 11
#define WM_SETTEXT 12
#define WM_SETTINGCHANGE 26
#define WM_SHOWWINDOW 24
#define WM_SIZE 5
#define WM_SIZECLIPBOARD 779
#define WM_SIZING 532
#define WM_SPOOLERSTATUS 42
#define WM_STYLECHANGED 125
#define WM_STYLECHANGING 124
#define WM_SYSCHAR 262
#define WM_SYSCOLORCHANGE 21
#define WM_SYSCOMMAND 274
#define WM_SYSDEADCHAR 263
#define WM_SYSKEYDOWN 260
#define WM_SYSKEYUP 261
#define WM_TCARD 82
#define WM_TIMECHANGE 30
#define WM_TIMER 275
#define WM_UNDO 772
#define WM_USER 1024
#define WM_USERCHANGED 84
#define WM_VKEYTOITEM 46
#define WM_VSCROLL 277
#define WM_VSCROLLCLIPBOARD 778
#define WM_WINDOWPOSCHANGED 71
#define WM_WINDOWPOSCHANGING 70
#define WM_XBUTTONDOWN 0x020B
#define WM_XBUTTONUP 0x020C
#define WM_XBUTTONDBLCLK 0x020D
#define WM_WININICHANGE 26
#define WM_KEYFIRST 256
#define WM_KEYLAST 264
#define WM_MOUSEFIRST 512
#define WM_MOUSELAST 0x020D
#define WM_NEXTMENU 0x0213
//#define WM_CHANGEUISTATE 0x0127
//#define WM_UPDATEUISTATE 0x0128
//#define WM_QUERYUISTATE 0x0129
#define WM_WTSSESSION_CHANGE 0x02B1
#define WM_LBTRACKPOINT 0x0131
#define WM_GETOBJECT 0x003D
#define WM_COMMNOTIFY 0x0044
#define WM_TABLET_FIRST 0x02c0
#define WM_TABLET_LAST 0x02df
#define WM_IME_REQUEST 0x0288
#define WM_IME_KEYLAST  0x010F
#define WM_NCMOUSEHOVER 0x02A0
#define WM_NCMOUSELEAVE 0x02A2

#define WS_BORDER 0x800000
#define WS_CAPTION 0xc00000
#define WS_CHILD 0x40000000
#define WS_CHILDWINDOW 0x40000000
#define WS_CLIPCHILDREN 0x2000000
#define WS_CLIPSIBLINGS 0x4000000
#define WS_DISABLED 0x8000000
#define WS_DLGFRAME 0x400000
#define WS_GROUP 0x20000
#define WS_HSCROLL 0x100000
#define WS_ICONIC 0x20000000
#define WS_MAXIMIZE 0x1000000
#define WS_MAXIMIZEBOX 0x10000
#define WS_MINIMIZE 0x20000000
#define WS_MINIMIZEBOX 0x20000
#define WS_OVERLAPPED 0
#define WS_OVERLAPPEDWINDOW 0xcf0000
#define WS_POPUP 0x80000000
#define WS_POPUPWINDOW 0x80880000
#define WS_SIZEBOX 0x40000
#define WS_SYSMENU 0x80000
#define WS_TABSTOP 0x10000
#define WS_THICKFRAME 0x40000
#define WS_TILED 0
#define WS_TILEDWINDOW 0xcf0000
#define WS_VISIBLE 0x10000000
#define WS_VSCROLL 0x200000

//!=================


//#define INVALID_HANDLE_VALUE (HANDLE)(0xffffffff)
#define MEM_RESERVE 8192
#define MEM_COMMIT 4096
#define PAGE_READWRITE 4
#define PAGE_WRITECOPY 8
#define PAGE_EXECUTE_READ 32
#define MEM_TOP_DOWN 1048576
#define MEM_RELEASE 32768
#define MAX_PATH 260
//#define FORMAT_MESSAGE_IGNORE_INSERTS 512
//#define FORMAT_MESSAGE_FROM_SYSTEM 4096
//#define WAIT_OBJECT_0 0
#define STD_INPUT_HANDLE (DWORD)(0xfffffff6)
#define STD_OUTPUT_HANDLE (DWORD)(0xfffffff5)
#define STD_ERROR_HANDLE (DWORD)(0xfffffff4)
#define ENABLE_LINE_INPUT 2
#define ENABLE_ECHO_INPUT 4
#define ENABLE_PROCESSED_INPUT 1
#define ENABLE_WINDOW_INPUT 8
#define ENABLE_MOUSE_INPUT 16
#define ENABLE_PROCESSED_OUTPUT 1
#define CONST const

#define CAPSLOCK_ON 128
#define ENHANCED_KEY 256
#define LEFT_ALT_PRESSED 2
#define LEFT_CTRL_PRESSED 8
#define NUMLOCK_ON 32
#define RIGHT_ALT_PRESSED 1
#define RIGHT_CTRL_PRESSED 4
#define SCROLLLOCK_ON 64
#define SHIFT_PRESSED 16
#define FROM_LEFT_1ST_BUTTON_PRESSED 1
#define RIGHTMOST_BUTTON_PRESSED 2
#define FROM_LEFT_2ND_BUTTON_PRESSED 4
#define FROM_LEFT_3RD_BUTTON_PRESSED 8
#define FROM_LEFT_4TH_BUTTON_PRESSED 16
#define DOUBLE_CLICK 2
#define MOUSE_MOVED 1
#define KEY_EVENT 1
#define MOUSE_EVENT 2
#define WINDOW_BUFFER_SIZE_EVENT 4
#define MENU_EVENT 8
#define FOCUS_EVENT 16

//for PlaySound
#define SND_SYNC 0 
#define SND_ASYNC 1 
#define SND_NODEFAULT 2 
#define SND_MEMORY 4 
#define SND_LOOP 8 
#define SND_NOSTOP 16 
#define SND_NOWAIT 8192 
#define SND_ALIAS 65536 
#define SND_ALIAS_ID 0x110000 
#define SND_FILENAME 0x20000 
#define SND_RESOURCE 0x40004 

#define CW_USEDEFAULT 0x80000000
#define CS_BYTEALIGNCLIENT 4096
#define CS_BYTEALIGNWINDOW 8192
#define CS_KEYCVTWINDOW 4
#define CS_NOKEYCVT 256
#define CS_CLASSDC 64
#define CS_DBLCLKS 8
#define CS_GLOBALCLASS 16384
#define CS_IME 0x10000
#define CS_DROPSHADOW 0x20000
#define CS_HREDRAW 2
#define CS_NOCLOSE 512
#define CS_OWNDC 32
#define CS_PARENTDC 128
#define CS_SAVEBITS 2048
#define CS_VREDRAW 1

#define IDI_APPLICATION MAKEINTRESOURCE(32512)
#define IDC_ARROW MAKEINTRESOURCE(32512)

#define BLACK_BRUSH 4
#define DKGRAY_BRUSH 3
#define GRAY_BRUSH 2
#define HOLLOW_BRUSH 5
#define LTGRAY_BRUSH 1
#define NULL_BRUSH 5
#define WHITE_BRUSH 0
#define BLACK_PEN 7
#define NULL_PEN 8
#define WHITE_PEN 6

#define MB_USERICON 128
#define MB_ICONASTERISK 64
#define MB_ICONEXCLAMATION 0x30
#define MB_ICONWARNING 0x30
#define MB_ICONERROR 16
#define MB_ICONHAND 16
#define MB_ICONQUESTION 32
#define MB_OK 0
#define MB_ABORTRETRYIGNORE 2
#define MB_APPLMODAL 0
#define MB_DEFAULT_DESKTOP_ONLY 0x20000
#define MB_HELP 0x4000
#define MB_RIGHT 0x80000
#define MB_RTLREADING 0x100000
#define MB_TOPMOST 0x40000
#define MB_DEFBUTTON1 0
#define MB_DEFBUTTON2 256
#define MB_DEFBUTTON3 512
#define MB_DEFBUTTON4 0x300
#define MB_ICONINFORMATION 64
#define MB_ICONSTOP 16
#define MB_OKCANCEL 1
#define MB_RETRYCANCEL 0x5
#define MB_SERVICE_NOTIFICATION 0x40000
#define MB_SETFOREGROUND 0x10000
#define MB_SYSTEMMODAL 4096
#define MB_TASKMODAL 0x2000
#define MB_YESNO 4
#define MB_YESNOCANCEL 3

#define DT_BOTTOM 8
#define DT_CALCRECT 1024
#define DT_CENTER 1
#define DT_EDITCONTROL 8192
#define DT_END_ELLIPSIS 32768
#define DT_PATH_ELLIPSIS 16384
#define DT_EXPANDTABS 64
#define DT_EXTERNALLEADING 512
#define DT_LEFT 0
#define DT_MODIFYSTRING 65536
#define DT_NOCLIP 256
#define DT_NOPREFIX 2048
#define DT_RIGHT 2
#define DT_RTLREADING 131072
#define DT_SINGLELINE 32
#define DT_TABSTOP 128
#define DT_TOP 0
#define DT_VCENTER 4
#define DT_WORDBREAK 16
#define DT_INTERNAL 4096
#define DT_WORD_ELLIPSIS 0x40000
#define DT_NOFULLWIDTHCHARBREAK 0x80000
#define DT_HIDEPREFIX 0x100000
#define DT_PREFIXONLY 0x200000

#define HKEY_CLASSES_ROOT (HKEY)0x80000000
#define HKEY_CURRENT_USER (HKEY)0x80000001
#define HKEY_LOCAL_MACHINE (HKEY)0x80000002
#define HKEY_USERS (HKEY)(-2147483645)
#define HKEY_PERFORMANCE_DATA (HKEY)(-2147483644)
#define HKEY_CURRENT_CONFIG (HKEY)(-2147483643)
#define HKEY_DYN_DATA ((HKEY)(0x80000006))

#define KEY_QUERY_VALUE 1
#define KEY_SET_VALUE 2
#define KEY_CREATE_SUB_KEY 4
#define KEY_ENUMERATE_SUB_KEYS 8
#define KEY_NOTIFY 16
#define KEY_CREATE_LINK 32
#define KEY_ALL_ACCESS 0xF003F
#define KEY_EXECUTE 0x20019
#define KEY_READ 0x20019
#define KEY_WRITE 0x20006

#define NO_ERROR 0
#define ERROR_SUCCESS 0
#define ERROR_INVALID_FUNCTION 1
#define ERROR_FILE_NOT_FOUND 2
#define ERROR_PATH_NOT_FOUND 3
#define ERROR_TOO_MANY_OPEN_FILES 4
#define ERROR_ACCESS_DENIED 5
#define ERROR_INVALID_HANDLE 6
#define ERROR_ARENA_TRASHED 7
#define ERROR_NOT_ENOUGH_MEMORY 8
#define ERROR_INVALID_BLOCK 9
#define ERROR_BAD_ENVIRONMENT 10
#define ERROR_BAD_FORMAT 11
#define ERROR_INVALID_ACCESS 12
#define ERROR_INVALID_DATA 13
#define ERROR_OUTOFMEMORY 14
#define ERROR_INVALID_DRIVE 15
#define ERROR_CURRENT_DIRECTORY 16
#define ERROR_NOT_SAME_DEVICE 17
#define ERROR_NO_MORE_FILES 18
#define ERROR_WRITE_PROTECT 19
#define ERROR_BAD_UNIT 20
#define ERROR_NOT_READY 21
#define ERROR_BAD_COMMAND 22
#define ERROR_CRC 23
#define ERROR_BAD_LENGTH 24
#define ERROR_SEEK 25
#define ERROR_NOT_DOS_DISK 26
#define ERROR_SECTOR_NOT_FOUND 27
#define ERROR_OUT_OF_PAPER 28
#define ERROR_WRITE_FAULT 29
#define ERROR_READ_FAULT 30
#define ERROR_GEN_FAILURE 31
#define ERROR_SHARING_VIOLATION 32
#define ERROR_LOCK_VIOLATION 33
#define ERROR_WRONG_DISK 34
#define ERROR_SHARING_BUFFER_EXCEEDED 36
#define ERROR_HANDLE_EOF 38
#define ERROR_HANDLE_DISK_FULL 39
#define ERROR_NOT_SUPPORTED 50
#define ERROR_REM_NOT_LIST 51
#define ERROR_DUP_NAME 52
#define ERROR_BAD_NETPATH 53
#define ERROR_NETWORK_BUSY 54
#define ERROR_DEV_NOT_EXIST 55
#define ERROR_TOO_MANY_CMDS 56
#define ERROR_ADAP_HDW_ERR 57
#define ERROR_BAD_NET_RESP 58
#define ERROR_UNEXP_NET_ERR 59
#define ERROR_BAD_REM_ADAP 60
#define ERROR_PRINTQ_FULL 61
#define ERROR_NO_SPOOL_SPACE 62
#define ERROR_PRINT_CANCELLED 63
#define ERROR_NETNAME_DELETED 64
#define ERROR_NETWORK_ACCESS_DENIED 65
#define ERROR_BAD_DEV_TYPE 66
#define ERROR_BAD_NET_NAME 67
#define ERROR_TOO_MANY_NAMES 68
#define ERROR_TOO_MANY_SESS 69
#define ERROR_SHARING_PAUSED 70
#define ERROR_REQ_NOT_ACCEP 71
#define ERROR_REDIR_PAUSED 72
#define ERROR_FILE_EXISTS 80
#define ERROR_CANNOT_MAKE 82
#define ERROR_FAIL_I24 83
#define ERROR_OUT_OF_STRUCTURES 84
#define ERROR_ALREADY_ASSIGNED 85
#define ERROR_INVALID_PASSWORD 86
#define ERROR_INVALID_PARAMETER 87
#define ERROR_NET_WRITE_FAULT 88
#define ERROR_NO_PROC_SLOTS 89
#define ERROR_TOO_MANY_SEMAPHORES 100
#define ERROR_EXCL_SEM_ALREADY_OWNED 101
#define ERROR_SEM_IS_SET 102
#define ERROR_TOO_MANY_SEM_REQUESTS 103
#define ERROR_INVALID_AT_INTERRUPT_TIME 104
#define ERROR_SEM_OWNER_DIED 105
#define ERROR_SEM_USER_LIMIT 106
#define ERROR_DISK_CHANGE 107
#define ERROR_DRIVE_LOCKED 108
#define ERROR_BROKEN_PIPE 109
#define ERROR_OPEN_FAILED 110
#define ERROR_BUFFER_OVERFLOW 111
#define ERROR_DISK_FULL 112
#define ERROR_NO_MORE_SEARCH_HANDLES 113
#define ERROR_INVALID_TARGET_HANDLE 114
#define ERROR_INVALID_CATEGORY 117
#define ERROR_INVALID_VERIFY_SWITCH 118
#define ERROR_BAD_DRIVER_LEVEL 119
#define ERROR_CALL_NOT_IMPLEMENTED 120
#define ERROR_SEM_TIMEOUT 121
#define ERROR_INSUFFICIENT_BUFFER 122
#define ERROR_INVALID_NAME 123
#define ERROR_INVALID_LEVEL 124
#define ERROR_NO_VOLUME_LABEL 125
#define ERROR_MOD_NOT_FOUND 126
#define ERROR_PROC_NOT_FOUND 127
#define ERROR_WAIT_NO_CHILDREN 128
#define ERROR_CHILD_NOT_COMPLETE 129
#define ERROR_DIRECT_ACCESS_HANDLE 130
#define ERROR_NEGATIVE_SEEK 131
#define ERROR_SEEK_ON_DEVICE 132
#define ERROR_IS_JOIN_TARGET 133
#define ERROR_IS_JOINED 134
#define ERROR_IS_SUBSTED 135
#define ERROR_NOT_JOINED 136
#define ERROR_NOT_SUBSTED 137
#define ERROR_JOIN_TO_JOIN 138
#define ERROR_SUBST_TO_SUBST 139
#define ERROR_JOIN_TO_SUBST 140
#define ERROR_SUBST_TO_JOIN 141
#define ERROR_BUSY_DRIVE 142
#define ERROR_SAME_DRIVE 143
#define ERROR_DIR_NOT_ROOT 144
#define ERROR_DIR_NOT_EMPTY 145
#define ERROR_IS_SUBST_PATH 146
#define ERROR_IS_JOIN_PATH 147
#define ERROR_PATH_BUSY 148
#define ERROR_IS_SUBST_TARGET 149
#define ERROR_SYSTEM_TRACE 150
#define ERROR_INVALID_EVENT_COUNT 151
#define ERROR_TOO_MANY_MUXWAITERS 152
#define ERROR_INVALID_LIST_FORMAT 153
#define ERROR_LABEL_TOO_LONG 154
#define ERROR_TOO_MANY_TCBS 155
#define ERROR_SIGNAL_REFUSED 156
#define ERROR_DISCARDED 157
#define ERROR_NOT_LOCKED 158
#define ERROR_BAD_THREADID_ADDR 159
#define ERROR_BAD_ARGUMENTS 160
#define ERROR_BAD_PATHNAME 161
#define ERROR_SIGNAL_PENDING 162
#define ERROR_MAX_THRDS_REACHED 164
#define ERROR_LOCK_FAILED 167
#define ERROR_BUSY 170
#define ERROR_CANCEL_VIOLATION 173
#define ERROR_ATOMIC_LOCKS_NOT_SUPPORTED 174
#define ERROR_INVALID_SEGMENT_NUMBER 180
#define ERROR_INVALID_ORDINAL 182
#define ERROR_ALREADY_EXISTS 183
#define ERROR_INVALID_FLAG_NUMBER 186
#define ERROR_SEM_NOT_FOUND 187
#define ERROR_INVALID_STARTING_CODESEG 188
#define ERROR_INVALID_STACKSEG 189
#define ERROR_INVALID_MODULETYPE 190
#define ERROR_INVALID_EXE_SIGNATURE 191
#define ERROR_EXE_MARKED_INVALID 192
#define ERROR_EXE_MACHINE_TYPE_MISMATCH 216
#define ERROR_BAD_EXE_FORMAT 193
#define ERROR_ITERATED_DATA_EXCEEDS_64k 194
#define ERROR_INVALID_MINALLOCSIZE 195
#define ERROR_DYNLINK_FROM_INVALID_RING 196
#define ERROR_IOPL_NOT_ENABLED 197
#define ERROR_INVALID_SEGDPL 198
#define ERROR_AUTODATASEG_EXCEEDS_64k 199
#define ERROR_RING2SEG_MUST_BE_MOVABLE 200
#define ERROR_RELOC_CHAIN_XEEDS_SEGLIM 201
#define ERROR_INFLOOP_IN_RELOC_CHAIN 202
#define ERROR_ENVVAR_NOT_FOUND 203
#define ERROR_NO_SIGNAL_SENT 205
#define ERROR_FILENAME_EXCED_RANGE 206
#define ERROR_RING2_STACK_IN_USE 207
#define ERROR_META_EXPANSION_TOO_LONG 208
#define ERROR_INVALID_SIGNAL_NUMBER 209
#define ERROR_THREAD_1_INACTIVE 210
#define ERROR_LOCKED 212
#define ERROR_TOO_MANY_MODULES 214
#define ERROR_NESTING_NOT_ALLOWED 215
#define ERROR_BAD_PIPE 230
#define ERROR_PIPE_BUSY 231
#define ERROR_NO_DATA 232
#define ERROR_PIPE_NOT_CONNECTED 233
#define ERROR_MORE_DATA 234
#define ERROR_VC_DISCONNECTED 240
#define ERROR_INVALID_EA_NAME 254
#define ERROR_EA_LIST_INCONSISTENT 255
#define ERROR_NO_MORE_ITEMS 259
#define ERROR_CANNOT_COPY 266
#define ERROR_DIRECTORY 267
#define ERROR_EAS_DIDNT_FIT 275
#define ERROR_EA_FILE_CORRUPT 276
#define ERROR_EA_TABLE_FULL 277
#define ERROR_INVALID_EA_HANDLE 278
#define ERROR_EAS_NOT_SUPPORTED 282
#define ERROR_NOT_OWNER 288
#define ERROR_TOO_MANY_POSTS 298
#define ERROR_PARTIAL_COPY 299
#define ERROR_MR_MID_NOT_FOUND 317
#define ERROR_INVALID_ADDRESS 487
#define ERROR_ARITHMETIC_OVERFLOW 534
#define ERROR_PIPE_CONNECTED 535
#define ERROR_PIPE_LISTENING 536
#define ERROR_EA_ACCESS_DENIED 994
#define ERROR_OPERATION_ABORTED 995
#define ERROR_IO_INCOMPLETE 996
#define ERROR_IO_PENDING 997
#define ERROR_NOACCESS 998
#define ERROR_SWAPERROR 999
#define ERROR_STACK_OVERFLOW 1001
#define ERROR_INVALID_MESSAGE 1002
#define ERROR_CAN_NOT_COMPLETE 1003
#define ERROR_INVALID_FLAGS 1004
#define ERROR_UNRECOGNIZED_VOLUME 1005
#define ERROR_FILE_INVALID 1006
#define ERROR_FULLSCREEN_MODE 1007
#define ERROR_NO_TOKEN 1008
#define ERROR_BADDB 1009
#define ERROR_BADKEY 1010
#define ERROR_CANTOPEN 1011
#define ERROR_CANTREAD 1012
#define ERROR_CANTWRITE 1013
#define ERROR_REGISTRY_RECOVERED 1014
#define ERROR_REGISTRY_CORRUPT 1015
#define ERROR_REGISTRY_IO_FAILED 1016
#define ERROR_NOT_REGISTRY_FILE 1017
#define ERROR_KEY_DELETED 1018
#define ERROR_NO_LOG_SPACE 1019
#define ERROR_KEY_HAS_CHILDREN 1020
#define ERROR_CHILD_MUST_BE_VOLATILE 1021
#define ERROR_NOTIFY_ENUM_DIR 1022
#define ERROR_DEPENDENT_SERVICES_RUNNING 1051
#define ERROR_INVALID_SERVICE_CONTROL 1052
#define ERROR_SERVICE_REQUEST_TIMEOUT 1053
#define ERROR_SERVICE_NO_THREAD 1054
#define ERROR_SERVICE_DATABASE_LOCKED 1055
#define ERROR_SERVICE_ALREADY_RUNNING 1056
#define ERROR_INVALID_SERVICE_ACCOUNT 1057
#define ERROR_SERVICE_DISABLED 1058
#define ERROR_CIRCULAR_DEPENDENCY 1059
#define ERROR_SERVICE_DOES_NOT_EXIST 1060
#define ERROR_SERVICE_CANNOT_ACCEPT_CTRL 1061
#define ERROR_SERVICE_NOT_ACTIVE 1062
#define ERROR_FAILED_SERVICE_CONTROLLER_CONNECT 1063
#define ERROR_EXCEPTION_IN_SERVICE 1064
#define ERROR_DATABASE_DOES_NOT_EXIST 1065
#define ERROR_SERVICE_SPECIFIC_ERROR 1066
#define ERROR_PROCESS_ABORTED 1067
#define ERROR_SERVICE_DEPENDENCY_FAIL 1068
#define ERROR_SERVICE_LOGON_FAILED 1069
#define ERROR_SERVICE_START_HANG 1070
#define ERROR_INVALID_SERVICE_LOCK 1071
#define ERROR_SERVICE_MARKED_FOR_DELETE 1072
#define ERROR_SERVICE_EXISTS 1073
#define ERROR_ALREADY_RUNNING_LKG 1074
#define ERROR_SERVICE_DEPENDENCY_DELETED 1075
#define ERROR_BOOT_ALREADY_ACCEPTED 1076
#define ERROR_SERVICE_NEVER_STARTED 1077
#define ERROR_DUPLICATE_SERVICE_NAME 1078
#define ERROR_DIFFERENT_SERVICE_ACCOUNT 1079
#define ERROR_END_OF_MEDIA 1100
#define ERROR_FILEMARK_DETECTED 1101
#define ERROR_BEGINNING_OF_MEDIA 1102
#define ERROR_SETMARK_DETECTED 1103
#define ERROR_NO_DATA_DETECTED 1104
#define ERROR_PARTITION_FAILURE 1105
#define ERROR_INVALID_BLOCK_LENGTH 1106

#define ERROR_DEVICE_NOT_PARTITIONED 1107
#define ERROR_UNABLE_TO_LOCK_MEDIA 1108
#define ERROR_UNABLE_TO_UNLOAD_MEDIA 1109
#define ERROR_MEDIA_CHANGED 1110
#define ERROR_BUS_RESET 1111
#define ERROR_NO_MEDIA_IN_DRIVE 1112
#define ERROR_NO_UNICODE_TRANSLATION 1113
#define ERROR_DLL_INIT_FAILED 1114
#define ERROR_SHUTDOWN_IN_PROGRESS 1115
#define ERROR_NO_SHUTDOWN_IN_PROGRESS 1116
#define ERROR_IO_DEVICE 1117
#define ERROR_SERIAL_NO_DEVICE 1118
#define ERROR_IRQ_BUSY 1119
#define ERROR_MORE_WRITES 1120
#define ERROR_COUNTER_TIMEOUT 1121
#define ERROR_FLOPPY_ID_MARK_NOT_FOUND 1122
#define ERROR_FLOPPY_WRONG_CYLINDER 1123
#define ERROR_FLOPPY_UNKNOWN_ERROR 1124
#define ERROR_FLOPPY_BAD_REGISTERS 1125
#define ERROR_DISK_RECALIBRATE_FAILED 1126
#define ERROR_DISK_OPERATION_FAILED 1127
#define ERROR_DISK_RESET_FAILED 1128
#define ERROR_EOM_OVERFLOW 1129
#define ERROR_NOT_ENOUGH_SERVER_MEMORY 1130
#define ERROR_POSSIBLE_DEADLOCK 1131
#define ERROR_MAPPED_ALIGNMENT 1132
#define ERROR_SET_POWER_STATE_VETOED 1140
#define ERROR_SET_POWER_STATE_FAILED 1141
#define ERROR_TOO_MANY_LINKS 1142
#define ERROR_OLD_WIN_VERSION 1150
#define ERROR_APP_WRONG_OS 1151
#define ERROR_SINGLE_INSTANCE_APP 1152
#define ERROR_RMODE_APP 1153
#define ERROR_INVALID_DLL 1154
#define ERROR_NO_ASSOCIATION 1155
#define ERROR_DDE_FAIL 1156
#define ERROR_DLL_NOT_FOUND 1157
#define ERROR_BAD_USERNAME 2202
#define ERROR_NOT_CONNECTED 2250
#define ERROR_OPEN_FILES 2401
#define ERROR_ACTIVE_CONNECTIONS 2402
#define ERROR_DEVICE_IN_USE 2404
#define ERROR_BAD_DEVICE 1200
#define ERROR_CONNECTION_UNAVAIL 1201
#define ERROR_DEVICE_ALREADY_REMEMBERED 1202
#define ERROR_NO_NET_OR_BAD_PATH 1203
#define ERROR_BAD_PROVIDER 1204
#define ERROR_CANNOT_OPEN_PROFILE 1205
#define ERROR_BAD_PROFILE 1206
#define ERROR_NOT_CONTAINER 1207
#define ERROR_EXTENDED_ERROR 1208
#define ERROR_INVALID_GROUPNAME 1209
#define ERROR_INVALID_COMPUTERNAME 1210
#define ERROR_INVALID_EVENTNAME 1211
#define ERROR_INVALID_DOMAINNAME 1212
#define ERROR_INVALID_SERVICENAME 1213
#define ERROR_INVALID_NETNAME 1214
#define ERROR_INVALID_SHARENAME 1215
#define ERROR_INVALID_PASSWORDNAME 1216
#define ERROR_INVALID_MESSAGENAME 1217
#define ERROR_INVALID_MESSAGEDEST 1218
#define ERROR_SESSION_CREDENTIAL_CONFLICT 1219
#define ERROR_REMOTE_SESSION_LIMIT_EXCEEDED 1220
#define ERROR_DUP_DOMAINNAME 1221
#define ERROR_NO_NETWORK 1222
#define ERROR_CANCELLED 1223
#define ERROR_USER_MAPPED_FILE 1224
#define ERROR_CONNECTION_REFUSED 1225
#define ERROR_GRACEFUL_DISCONNECT 1226
#define ERROR_ADDRESS_ALREADY_ASSOCIATED 1227
#define ERROR_ADDRESS_NOT_ASSOCIATED 1228
#define ERROR_CONNECTION_INVALID 1229
#define ERROR_CONNECTION_ACTIVE 1230
#define ERROR_NETWORK_UNREACHABLE 1231
#define ERROR_HOST_UNREACHABLE 1232
#define ERROR_PROTOCOL_UNREACHABLE 1233
#define ERROR_PORT_UNREACHABLE 1234
#define ERROR_REQUEST_ABORTED 1235
#define ERROR_CONNECTION_ABORTED 1236
#define ERROR_RETRY 1237
#define ERROR_CONNECTION_COUNT_LIMIT 1238
#define ERROR_LOGIN_TIME_RESTRICTION 1239
#define ERROR_LOGIN_WKSTA_RESTRICTION 1240
#define ERROR_INCORRECT_ADDRESS 1241
#define ERROR_ALREADY_REGISTERED 1242
#define ERROR_SERVICE_NOT_FOUND 1243
#define ERROR_NOT_AUTHENTICATED 1244
#define ERROR_NOT_LOGGED_ON 1245
#define ERROR_CONTINUE 1246
#define ERROR_ALREADY_INITIALIZED 1247
#define ERROR_NO_MORE_DEVICES 1248
#define ERROR_NOT_ALL_ASSIGNED 1300
#define ERROR_SOME_NOT_MAPPED 1301
#define ERROR_NO_QUOTAS_FOR_ACCOUNT 1302
#define ERROR_LOCAL_USER_SESSION_KEY 1303
#define ERROR_NULL_LM_PASSWORD 1304
#define ERROR_UNKNOWN_REVISION 1305
#define ERROR_REVISION_MISMATCH 1306
#define ERROR_INVALID_OWNER 1307
#define ERROR_INVALID_PRIMARY_GROUP 1308
#define ERROR_NO_IMPERSONATION_TOKEN 1309
#define ERROR_CANT_DISABLE_MANDATORY 1310
#define ERROR_NO_LOGON_SERVERS 1311
#define ERROR_NO_SUCH_LOGON_SESSION 1312
#define ERROR_NO_SUCH_PRIVILEGE 1313
#define ERROR_PRIVILEGE_NOT_HELD 1314
#define ERROR_INVALID_ACCOUNT_NAME 1315
#define ERROR_USER_EXISTS 1316
#define ERROR_NO_SUCH_USER 1317
#define ERROR_GROUP_EXISTS 1318
#define ERROR_NO_SUCH_GROUP 1319
#define ERROR_MEMBER_IN_GROUP 1320
#define ERROR_MEMBER_NOT_IN_GROUP 1321
#define ERROR_LAST_ADMIN 1322
#define ERROR_WRONG_PASSWORD 1323
#define ERROR_ILL_FORMED_PASSWORD 1324
#define ERROR_PASSWORD_RESTRICTION 1325
#define ERROR_LOGON_FAILURE 1326
#define ERROR_ACCOUNT_RESTRICTION 1327
#define ERROR_INVALID_LOGON_HOURS 1328
#define ERROR_INVALID_WORKSTATION 1329
#define ERROR_PASSWORD_EXPIRED 1330
#define ERROR_ACCOUNT_DISABLED 1331
#define ERROR_NONE_MAPPED 1332
#define ERROR_TOO_MANY_LUIDS_REQUESTED 1333
#define ERROR_LUIDS_EXHAUSTED 1334
#define ERROR_INVALID_SUB_AUTHORITY 1335
#define ERROR_INVALID_ACL 1336
#define ERROR_INVALID_SID 1337
#define ERROR_INVALID_SECURITY_DESCR 1338
#define ERROR_BAD_INHERITANCE_ACL 1340
#define ERROR_SERVER_DISABLED 1341
#define ERROR_SERVER_NOT_DISABLED 1342
#define ERROR_INVALID_ID_AUTHORITY 1343
#define ERROR_ALLOTTED_SPACE_EXCEEDED 1344
#define ERROR_INVALID_GROUP_ATTRIBUTES 1345
#define ERROR_BAD_IMPERSONATION_LEVEL 1346
#define ERROR_CANT_OPEN_ANONYMOUS 1347
#define ERROR_BAD_VALIDATION_CLASS 1348
#define ERROR_BAD_TOKEN_TYPE 1349
#define ERROR_NO_SECURITY_ON_OBJECT 1350
#define ERROR_CANT_ACCESS_DOMAIN_INFO 1351
#define ERROR_INVALID_SERVER_STATE 1352
#define ERROR_INVALID_DOMAIN_STATE 1353
#define ERROR_INVALID_DOMAIN_ROLE 1354
#define ERROR_NO_SUCH_DOMAIN 1355
#define ERROR_DOMAIN_EXISTS 1356
#define ERROR_DOMAIN_LIMIT_EXCEEDED 1357
#define ERROR_INTERNAL_DB_CORRUPTION 1358
#define ERROR_INTERNAL_ERROR 1359
#define ERROR_GENERIC_NOT_MAPPED 1360
#define ERROR_BAD_DESCRIPTOR_FORMAT 1361
#define ERROR_NOT_LOGON_PROCESS 1362
#define ERROR_LOGON_SESSION_EXISTS 1363
#define ERROR_NO_SUCH_PACKAGE 1364
#define ERROR_BAD_LOGON_SESSION_STATE 1365
#define ERROR_LOGON_SESSION_COLLISION 1366
#define ERROR_INVALID_LOGON_TYPE 1367
#define ERROR_CANNOT_IMPERSONATE 1368
#define ERROR_RXACT_INVALID_STATE 1369
#define ERROR_RXACT_COMMIT_FAILURE 1370
#define ERROR_SPECIAL_ACCOUNT 1371
#define ERROR_SPECIAL_GROUP 1372
#define ERROR_SPECIAL_USER 1373
#define ERROR_MEMBERS_PRIMARY_GROUP 1374
#define ERROR_TOKEN_ALREADY_IN_USE 1375
#define ERROR_NO_SUCH_ALIAS 1376
#define ERROR_MEMBER_NOT_IN_ALIAS 1377
#define ERROR_MEMBER_IN_ALIAS 1378
#define ERROR_ALIAS_EXISTS 1379
#define ERROR_LOGON_NOT_GRANTED 1380
#define ERROR_TOO_MANY_SECRETS 1381
#define ERROR_SECRET_TOO_LONG 1382
#define ERROR_INTERNAL_DB_ERROR 1383
#define ERROR_TOO_MANY_CONTEXT_IDS 1384
#define ERROR_LOGON_TYPE_NOT_GRANTED 1385
#define ERROR_NT_CROSS_ENCRYPTION_REQUIRED 1386
#define ERROR_NO_SUCH_MEMBER 1387
#define ERROR_INVALID_MEMBER 1388
#define ERROR_TOO_MANY_SIDS 1389
#define ERROR_LM_CROSS_ENCRYPTION_REQUIRED 1390
#define ERROR_NO_INHERITANCE 1391
#define ERROR_FILE_CORRUPT 1392
#define ERROR_DISK_CORRUPT 1393
#define ERROR_NO_USER_SESSION_KEY 1394
#define ERROR_LICENSE_QUOTA_EXCEEDED 1395
#define ERROR_INVALID_WINDOW_HANDLE 1400
#define ERROR_INVALID_MENU_HANDLE 1401
#define ERROR_INVALID_CURSOR_HANDLE 1402
#define ERROR_INVALID_ACCEL_HANDLE 1403
#define ERROR_INVALID_HOOK_HANDLE 1404
#define ERROR_INVALID_DWP_HANDLE 1405
#define ERROR_TLW_WITH_WSCHILD 1406
#define ERROR_CANNOT_FIND_WND_CLASS 1407
#define ERROR_WINDOW_OF_OTHER_THREAD 1408
#define ERROR_HOTKEY_ALREADY_REGISTERED 1409
#define ERROR_CLASS_ALREADY_EXISTS 1410
#define ERROR_CLASS_DOES_NOT_EXIST 1411
#define ERROR_CLASS_HAS_WINDOWS 1412
#define ERROR_INVALID_INDEX 1413
#define ERROR_INVALID_ICON_HANDLE 1414
#define ERROR_PRIVATE_DIALOG_INDEX 1415
#define ERROR_LISTBOX_ID_NOT_FOUND 1416
#define ERROR_NO_WILDCARD_CHARACTERS 1417
#define ERROR_CLIPBOARD_NOT_OPEN 1418
#define ERROR_HOTKEY_NOT_REGISTERED 1419
#define ERROR_WINDOW_NOT_DIALOG 1420
#define ERROR_CONTROL_ID_NOT_FOUND 1421
#define ERROR_INVALID_COMBOBOX_MESSAGE 1422
#define ERROR_WINDOW_NOT_COMBOBOX 1423
#define ERROR_INVALID_EDIT_HEIGHT 1424
#define ERROR_DC_NOT_FOUND 1425
#define ERROR_INVALID_HOOK_FILTER 1426
#define ERROR_INVALID_FILTER_PROC 1427
#define ERROR_HOOK_NEEDS_HMOD 1428
#define ERROR_GLOBAL_ONLY_HOOK 1429
#define ERROR_JOURNAL_HOOK_SET 1430
#define ERROR_HOOK_NOT_INSTALLED 1431
#define ERROR_INVALID_LB_MESSAGE 1432
#define ERROR_SETCOUNT_ON_BAD_LB 1433
#define ERROR_LB_WITHOUT_TABSTOPS 1434
#define ERROR_DESTROY_OBJECT_OF_OTHER_THREAD 1435
#define ERROR_CHILD_WINDOW_MENU 1436
#define ERROR_NO_SYSTEM_MENU 1437
#define ERROR_INVALID_MSGBOX_STYLE 1438
#define ERROR_INVALID_SPI_VALUE 1439
#define ERROR_SCREEN_ALREADY_LOCKED 1440
#define ERROR_HWNDS_HAVE_DIFF_PARENT 1441
#define ERROR_NOT_CHILD_WINDOW 1442
#define ERROR_INVALID_GW_COMMAND 1443
#define ERROR_INVALID_THREAD_ID 1444
#define ERROR_NON_MDICHILD_WINDOW 1445
#define ERROR_POPUP_ALREADY_ACTIVE 1446
#define ERROR_NO_SCROLLBARS 1447
#define ERROR_INVALID_SCROLLBAR_RANGE 1448
#define ERROR_INVALID_SHOWWIN_COMMAND 1449
#define ERROR_NO_SYSTEM_RESOURCES 1450
#define ERROR_NONPAGED_SYSTEM_RESOURCES 1451
#define ERROR_PAGED_SYSTEM_RESOURCES 1452
#define ERROR_WORKING_SET_QUOTA 1453
#define ERROR_PAGEFILE_QUOTA 1454
#define ERROR_COMMITMENT_LIMIT 1455
#define ERROR_MENU_ITEM_NOT_FOUND 1456
#define ERROR_INVALID_KEYBOARD_HANDLE 1457
#define ERROR_HOOK_TYPE_NOT_ALLOWED 1458
#define ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION 1459
#define ERROR_EVENTLOG_FILE_CORRUPT 1500
#define ERROR_EVENTLOG_CANT_START 1501
#define ERROR_LOG_FILE_FULL 1502
#define ERROR_EVENTLOG_FILE_CHANGED 1503

/*
proc
*/
#define DECLARE_HANDLE(n) typedef HANDLE n

typedef float FLOAT;
typedef int INT;
typedef unsigned char BYTE;
typedef BYTE* LPBYTE;
typedef struct {int dummy;} *HWND;
typedef const char *LPCSTR;
typedef char *LPSTR;
typedef unsigned int UINT;
typedef void* HANDLE;
typedef void* HINSTANCE;
typedef void* HMODULE;
typedef void* HLOCAL;
typedef void* LPVOID;
typedef void* HFONT;
//typedef unsigned short *LPWSTR;
typedef short *LPWSTR;
typedef const signed short *LPCWSTR;
typedef const void* LPCVOID;
typedef void* PVOID;
typedef int LONG;
typedef unsigned int ULONG;
typedef unsigned short WORD;
typedef unsigned long DWORD;
typedef DWORD* PDWORD;
typedef DWORD* LPDWORD;
typedef int BOOL;
typedef void VOID;
typedef long LONG_PTR;
typedef unsigned long ULONG_PTR;
typedef ULONG_PTR DWORD_PTR;
typedef unsigned long long ULONGLONG;
typedef signed long long LONGLONG;
typedef char CHAR;
typedef unsigned short WCHAR;
typedef short int SHORT;
typedef WORD* LPWORD;
typedef BOOL (CALLBACK *PHANDLER_ROUTINE) (DWORD);
typedef unsigned long long UINT_PTR;
typedef UINT_PTR WPARAM;
typedef LONG_PTR LPARAM;
typedef BYTE *PBYTE;
typedef unsigned long COLORREF;
typedef WORD ATOM;
typedef long LRESULT;
typedef HANDLE HICON;
typedef LRESULT (CALLBACK *WNDPROC) (HWND,UINT,WPARAM,LPARAM);
typedef HANDLE HCURSOR;
typedef char *PSTR;
typedef const unsigned short *LPCTSTR;
typedef DWORD (CALLBACK *PTHREAD_START_ROUTINE) (LPVOID);
typedef PTHREAD_START_ROUTINE LPTHREAD_START_ROUTINE;
typedef HANDLE HGLOBAL;
typedef HANDLE HDC;
typedef HANDLE HBITMAP;
typedef HANDLE HPEN;
typedef HANDLE HBRUSH;
typedef HANDLE *PHANDLE;
typedef HANDLE HMENU;
typedef HANDLE HGLRC;
typedef HANDLE HGDIOBJ;
typedef HANDLE HKEY,*PHKEY;
typedef DWORD ACCESS_MASK;
typedef ACCESS_MASK REGSAM;
typedef BOOL *LPBOOL;
typedef int *PINT;
typedef long *PLONG;
typedef short *PSHORT;
typedef ULONG_PTR SIZE_T;
typedef SIZE_T *PSIZE_T;
typedef HANDLE *LPHANDLE;
typedef long long int LONG64;
typedef unsigned short *PWORD;

/*
proc
*/

typedef struct tagPOINT {
	LONG x;
	LONG y;
} POINT,*PPOINT;
#define LPPOINT PPOINT

typedef struct tagMSG {
	HWND hwnd;
	UINT message;
	WPARAM wParam;
	LPARAM lParam;
	DWORD time;
	POINT pt;
} MSG,*LPMSG,*PMSG;

typedef struct _LIST_ENTRY {
	struct _LIST_ENTRY *Flink;
	struct _LIST_ENTRY *Blink;
} LIST_ENTRY,*PLIST_ENTRY;

typedef struct _CONSOLE_CURSOR_INFO {
	DWORD dwSize;
	BOOL bVisible;
} CONSOLE_CURSOR_INFO,*PCONSOLE_CURSOR_INFO;

typedef struct {
	PVOID BaseAddress;
	PVOID AllocationBase;
	DWORD AllocationProtect;
	DWORD RegionSize;
	DWORD State;
	DWORD Protect;
	DWORD Type;
} MEMORY_BASIC_INFORMATION;

typedef struct _RTL_CRITICAL_SECTION_DEBUG {
	WORD Type;
	WORD CreatorBackTraceIndex;
	struct _RTL_CRITICAL_SECTION *CriticalSection;
	LIST_ENTRY ProcessLocksList;
	DWORD EntryCount;
	DWORD ContentionCount;
	DWORD Spare[ 2];
} RTL_CRITICAL_SECTION_DEBUG,*PRTL_CRITICAL_SECTION_DEBUG;

typedef struct {
	PRTL_CRITICAL_SECTION_DEBUG DebugInfo;
	LONG LockCount;
	LONG RecursionCount;
	HANDLE OwningThread;
	HANDLE LockSemaphore;
	ULONG_PTR SpinCount;
} RTL_CRITICAL_SECTION, *PRTL_CRITICAL_SECTION;

typedef RTL_CRITICAL_SECTION CRITICAL_SECTION,*LPCRITICAL_SECTION;

typedef struct _SECURITY_ATTRIBUTES {
	DWORD nLength;
	LPVOID lpSecurityDescriptor;
	BOOL bInheritHandle;
} SECURITY_ATTRIBUTES,*LPSECURITY_ATTRIBUTES;

typedef enum _GET_FILEEX_INFO_LEVELS { GetFileExInfoStandard, GetFileExMaxInfoLevel } GET_FILEEX_INFO_LEVELS;

typedef struct tagFILETIME {
	DWORD dwLowDateTime;
	DWORD dwHighDateTime;
} FILETIME,*LPFILETIME,*PFILETIME;

typedef struct _WIN32_FIND_DATAA {
	DWORD dwFileAttributes;
	FILETIME ftCreationTime;
	FILETIME ftLastAccessTime;
	FILETIME ftLastWriteTime;
	DWORD nFileSizeHigh;
	DWORD nFileSizeLow;
	DWORD dwReserved0;
	DWORD dwReserved1;
	CHAR   cFileName[ MAX_PATH ];
	CHAR   cAlternateFileName[ 14 ];
} WIN32_FIND_DATAA, *PWIN32_FIND_DATAA, *LPWIN32_FIND_DATAA;

typedef struct _WIN32_FIND_DATAW {
	DWORD dwFileAttributes;
	FILETIME ftCreationTime;
	FILETIME ftLastAccessTime;
	FILETIME ftLastWriteTime;
	DWORD nFileSizeHigh;
	DWORD nFileSizeLow;
	DWORD dwReserved0;
	DWORD dwReserved1;
	WCHAR  cFileName[ MAX_PATH ];
	WCHAR  cAlternateFileName[ 14 ];
} WIN32_FIND_DATAW, *PWIN32_FIND_DATAW, *LPWIN32_FIND_DATAW;

typedef struct _COORD {
	SHORT X;
	SHORT Y;
} COORD;
//!=================

typedef struct _CHAR_INFO {
	union { WCHAR UnicodeChar; CHAR AsciiChar; } Char;
	WORD Attributes;
} CHAR_INFO,*PCHAR_INFO;

typedef struct _SMALL_RECT {
	SHORT Left;
	SHORT Top;
	SHORT Right;
	SHORT Bottom;
} SMALL_RECT,*PSMALL_RECT;

typedef struct _FOCUS_EVENT_RECORD {
	BOOL bSetFocus;
} FOCUS_EVENT_RECORD;

typedef struct _CONSOLE_SCREEN_BUFFER_INFO {
	COORD dwSize;
	COORD dwCursorPosition;
	WORD wAttributes;
	SMALL_RECT srWindow;
	COORD dwMaximumWindowSize;
} CONSOLE_SCREEN_BUFFER_INFO,*PCONSOLE_SCREEN_BUFFER_INFO;

typedef struct _KEY_EVENT_RECORD {
	BOOL bKeyDown;
	WORD wRepeatCount;
	WORD wVirtualKeyCode;
	WORD wVirtualScanCode;
	union { WCHAR UnicodeChar; CHAR AsciiChar; } uChar;
	DWORD dwControlKeyState;
} KEY_EVENT_RECORD;

typedef struct _MOUSE_EVENT_RECORD {
	COORD dwMousePosition;
	DWORD dwButtonState;
	DWORD dwControlKeyState;
	DWORD dwEventFlags;
} MOUSE_EVENT_RECORD;

typedef struct _WINDOW_BUFFER_SIZE_RECORD {
	COORD dwSize;
} WINDOW_BUFFER_SIZE_RECORD;

typedef struct _MENU_EVENT_RECORD {
	UINT dwCommandId;
} MENU_EVENT_RECORD,*PMENU_EVENT_RECORD;

typedef struct _INPUT_RECORD {
	WORD EventType;
	WORD __alignmentDummy;
	union {
		KEY_EVENT_RECORD KeyEvent;
		MOUSE_EVENT_RECORD MouseEvent;
		WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent;
		MENU_EVENT_RECORD MenuEvent;
		FOCUS_EVENT_RECORD FocusEvent;
	} Event;
} INPUT_RECORD,*PINPUT_RECORD;

typedef struct _STARTUPINFOW {
	DWORD cb;
	LPWSTR lpReserved;
	LPWSTR lpDesktop;
	LPWSTR lpTitle;
	DWORD dwX;
	DWORD dwY;
	DWORD dwXSize;
	DWORD dwYSize;
	DWORD dwXCountChars;
	DWORD dwYCountChars;
	DWORD dwFillAttribute;
	DWORD dwFlags;
	WORD wShowWindow;
	WORD cbReserved2;
	LPBYTE lpReserved2;
	HANDLE hStdInput;
	HANDLE hStdOutput;
	HANDLE hStdError;
} STARTUPINFOW, *LPSTARTUPINFOW;

typedef struct _PROCESS_INFORMATION {
	HANDLE hProcess;
	HANDLE hThread;
	DWORD dwProcessId;
	DWORD dwThreadId;
} PROCESS_INFORMATION,*LPPROCESS_INFORMATION;

typedef struct _WIN32_FILE_ATTRIBUTE_DATA {
	DWORD dwFileAttributes;
	FILETIME ftCreationTime;
	FILETIME ftLastAccessTime;
	FILETIME ftLastWriteTime;
	DWORD nFileSizeHigh;
	DWORD nFileSizeLow;
} WIN32_FILE_ATTRIBUTE_DATA,*LPWIN32_FILE_ATTRIBUTE_DATA;
typedef struct tagWNDCLASSA {
	UINT	style;
	WNDPROC	lpfnWndProc;
	int	cbClsExtra;
	int	cbWndExtra;
	HINSTANCE	hInstance;
	HICON	hIcon;
	HCURSOR	hCursor;
	HBRUSH	hbrBackground;
	LPCSTR	lpszMenuName;
	LPCSTR	lpszClassName;
} WNDCLASSA, *PWNDCLASSA,*NPWNDCLASSA,*LPWNDCLASSA;
typedef struct tagWNDCLASSW {
	UINT	style;
	WNDPROC	lpfnWndProc;
	int	cbClsExtra;
	int	cbWndExtra;
	HINSTANCE hInstance;
	HICON	hIcon;
	HCURSOR	hCursor;
	HBRUSH	hbrBackground;
	LPCWSTR	lpszMenuName;
	LPCWSTR	lpszClassName;
} WNDCLASSW, *PWNDCLASSW,*NPWNDCLASSW,*LPWNDCLASSW;
typedef struct tagWNDCLASSEXA {
	UINT	cbSize;
	UINT	style;
	WNDPROC	lpfnWndProc;
	int	cbClsExtra;
	int	cbWndExtra;
	HINSTANCE	hInstance;
	HICON	hIcon;
	HCURSOR	hCursor;
	HBRUSH	hbrBackground;
	LPCSTR	lpszMenuName;
	LPCSTR	lpszClassName;
	HICON	hIconSm;
} WNDCLASSEXA, *PWNDCLASSEXA,*NPWNDCLASSEXA,*LPWNDCLASSEXA;
typedef struct tagWNDCLASSEXW {
	UINT cbSize;
	UINT style;
	WNDPROC lpfnWndProc;
	int cbClsExtra;
	int cbWndExtra;
	HINSTANCE hInstance;
	HICON hIcon;
	HCURSOR hCursor;
	HBRUSH hbrBackground;
	LPCWSTR lpszMenuName;
	LPCWSTR lpszClassName;
	HICON hIconSm;
} WNDCLASSEXW,*PWNDCLASSEXW,*NPWNDCLASSEXW,*LPWNDCLASSEXW;
#ifdef UNICODE
	typedef WNDCLASSW WNDCLASS;
	typedef PWNDCLASSW PWNDCLASS;
	typedef NPWNDCLASSW NPWNDCLASS;
	typedef LPWNDCLASSW LPWNDCLASS;
	typedef WNDCLASSEXW WNDCLASSEX;
#else
	typedef WNDCLASSA WNDCLASS;
	typedef PWNDCLASSA PWNDCLASS;
	typedef NPWNDCLASSA NPWNDCLASS;
	typedef LPWNDCLASSA LPWNDCLASS;
	typedef WNDCLASSEXA WNDCLASSEX;
#endif
typedef struct _RECT {
	LONG left;
	LONG top;
	LONG right;
	LONG bottom;
} RECT,*LPRECT;
typedef struct tagPAINTSTRUCT {
	HDC hdc;
	BOOL fErase;
	RECT rcPaint;
	BOOL fRestore;
	BOOL fIncUpdate;
	BYTE rgbReserved[32];
} PAINTSTRUCT,*LPPAINTSTRUCT;
typedef struct tagCREATESTRUCT {
	LPVOID lpCreateParams;
	HINSTANCE hInstance;
	HMENU hMenu;
	HWND hwndParent;
	int cy;
	int cx;
	int y;
	int x;
	LONG style;
	LPCTSTR lpszName;
	LPCTSTR lpszClass;
	DWORD dwExStyle;
} CREATESTRUCT,*LPCREATESTRUCT;

typedef struct tagBITMAP {
	LONG bmType;
	LONG bmWidth;
	LONG bmHeight;
	LONG bmWidthBytes;
	WORD bmPlanes;
	WORD bmBitsPixel;
	LPVOID bmBits;
} BITMAP,*PBITMAP,*NPBITMAP,*LPBITMAP;
typedef struct tagBITMAPCOREHEADER {
	DWORD bcSize;
	WORD bcWidth;
	WORD bcHeight;
	WORD bcPlanes;
	WORD bcBitCount;
} BITMAPCOREHEADER;
 /* was missing jn */
typedef BITMAPCOREHEADER *LPBITMAPCOREHEADER;
typedef struct tagRGBTRIPLE {
	BYTE rgbtBlue;
	BYTE rgbtGreen;
	BYTE rgbtRed;
} RGBTRIPLE;
typedef struct _BITMAPCOREINFO {
	BITMAPCOREHEADER bmciHeader;
	RGBTRIPLE bmciColors[1];
} BITMAPCOREINFO;
/* was missing jn */
typedef BITMAPCOREINFO *LPBITMAPCOREINFO;
typedef struct tagBITMAPFILEHEADER {
	WORD bfType;
	DWORD bfSize;
	WORD bfReserved1;
	WORD bfReserved2;
	DWORD bfOffBits;
} BITMAPFILEHEADER,*LPBITMAPFILEHEADER,*PBITMAPFILEHEADER;
typedef struct tagBITMAPINFOHEADER {
	DWORD biSize;
	LONG biWidth;
	LONG biHeight;
	WORD biPlanes;
	WORD biBitCount;
	DWORD biCompression;
	DWORD biSizeImage;
	LONG biXPelsPerMeter;
	LONG biYPelsPerMeter;
	DWORD biClrUsed;
	DWORD biClrImportant;
} BITMAPINFOHEADER,*LPBITMAPINFOHEADER,*PBITMAPINFOHEADER;
typedef struct tagRGBQUAD {
	BYTE rgbBlue;
	BYTE rgbGreen;
	BYTE rgbRed;
	BYTE rgbReserved;
} RGBQUAD,*LPRGBQUAD;
typedef struct tagBITMAPINFO {
	BITMAPINFOHEADER bmiHeader;
	RGBQUAD bmiColors[1];
} BITMAPINFO,*LPBITMAPINFO,*PBITMAPINFO;
typedef union _LARGE_INTEGER {
	struct { DWORD LowPart; LONG HighPart;};
	struct { DWORD LowPart; LONG HighPart; } u;
	long long int QuadPart;
} LARGE_INTEGER,*PLARGE_INTEGER;
typedef VOID (CALLBACK *TIMERPROC) (HWND,UINT,UINT,DWORD);

typedef struct _STARTUPINFOA {
	DWORD cb;
	LPSTR lpReserved;
	LPSTR lpDesktop;
	LPSTR lpTitle;
	DWORD dwX;
	DWORD dwY;
	DWORD dwXSize;
	DWORD dwYSize;
	DWORD dwXCountChars;
	DWORD dwYCountChars;
	DWORD dwFillAttribute;
	DWORD dwFlags;
	WORD wShowWindow;
	WORD cbReserved2;
	LPBYTE lpReserved2;
	HANDLE hStdInput;
	HANDLE hStdOutput;
	HANDLE hStdError;
} STARTUPINFOA, *LPSTARTUPINFOA;
typedef struct _SYSTEMTIME {
	WORD wYear;
	WORD wMonth;
	WORD wDayOfWeek;
	WORD wDay;
	WORD wHour;
	WORD wMinute;
	WORD wSecond;
	WORD wMilliseconds;
} SYSTEMTIME,*LPSYSTEMTIME,*PSYSTEMTIME;
typedef struct _SYSTEM_INFO {
	WORD wProcessorArchitecture;
	WORD wReserved;
	DWORD dwPageSize;
	LPVOID lpMinimumApplicationAddress;
	LPVOID lpMaximumApplicationAddress;
	DWORD dwActiveProcessorMask;
	DWORD dwNumberOfProcessors;
	DWORD dwProcessorType;
	DWORD dwAllocationGranularity;
	WORD wProcessorLevel;
	WORD wProcessorRevision;
} SYSTEM_INFO,*LPSYSTEM_INFO;
typedef struct _OVERLAPPED {
	DWORD Internal;
	DWORD InternalHigh;
	DWORD Offset;
	DWORD OffsetHigh;
	HANDLE hEvent;
} OVERLAPPED,*LPOVERLAPPED;
typedef struct _IO_COUNTERS {
	ULONGLONG ReadOperationCount;
	ULONGLONG WriteOperationCount;
	ULONGLONG OtherOperationCount;
	ULONGLONG ReadTransferCount;
	ULONGLONG WriteTransferCount;
	ULONGLONG OtherTransferCount;
} IO_COUNTERS;
typedef IO_COUNTERS *PIO_COUNTERS;
typedef union _ULARGE_INTEGER {
	struct {DWORD LowPart; DWORD HighPart;};
	long long QuadPart;
} ULARGE_INTEGER,*PULARGE_INTEGER;
typedef struct tagLOGFONTW
{
	LONG	lfHeight;
	LONG	lfWidth;
	LONG	lfEscapement;
	LONG	lfOrientation;
	LONG	lfWeight;
	BYTE	lfItalic;
	BYTE	lfUnderline;
	BYTE	lfStrikeOut;
	BYTE	lfCharSet;
	BYTE	lfOutPrecision;
	BYTE	lfClipPrecision;
	BYTE	lfQuality;
	BYTE	lfPitchAndFamily;
	WCHAR	lfFaceName[LF_FACESIZE];
} LOGFONTW, *PLOGFONTW, *NPLOGFONTW, *LPLOGFONTW;
typedef struct _BY_HANDLE_FILE_INFORMATION {
	DWORD dwFileAttributes;
	FILETIME ftCreationTime;
	FILETIME ftLastAccessTime;
	FILETIME ftLastWriteTime;
	DWORD dwVolumeSerialNumber;
	DWORD nFileSizeHigh;
	DWORD nFileSizeLow;
	DWORD nNumberOfLinks;
	DWORD nFileIndexHigh;
	DWORD nFileIndexLow;
} BY_HANDLE_FILE_INFORMATION,*LPBY_HANDLE_FILE_INFORMATION;
typedef struct _IMAGE_DOS_HEADER {
	WORD e_magic;
	WORD e_cblp;
	WORD e_cp;
	WORD e_crlc;
	WORD e_cparhdr;
	WORD e_minalloc;
	WORD e_maxalloc;
	WORD e_ss;
	WORD e_sp;
	WORD e_csum;
	WORD e_ip;
	WORD e_cs;
	WORD e_lfarlc;
	WORD e_ovno;
	WORD e_res[4];
	WORD e_oemid;
	WORD e_oeminfo;
	WORD e_res2[10];
	LONG e_lfanew;
} IMAGE_DOS_HEADER,*PIMAGE_DOS_HEADER;
typedef struct _IMAGE_FILE_HEADER {
	WORD Machine;
	WORD NumberOfSections;
	DWORD TimeDateStamp;
	DWORD PointerToSymbolTable;
	DWORD NumberOfSymbols;
	WORD SizeOfOptionalHeader;
	WORD Characteristics;
} IMAGE_FILE_HEADER,*PIMAGE_FILE_HEADER;
typedef struct _IMAGE_DATA_DIRECTORY {
	DWORD VirtualAddress;
	DWORD Size;
} IMAGE_DATA_DIRECTORY,*PIMAGE_DATA_DIRECTORY;

typedef struct _IMAGE_OPTIONAL_HEADER {
    //
    // Standard fields.
    //

    WORD    Magic;
    BYTE    MajorLinkerVersion;
    BYTE    MinorLinkerVersion;
    DWORD   SizeOfCode;
    DWORD   SizeOfInitializedData;
    DWORD   SizeOfUninitializedData;
    DWORD   AddressOfEntryPoint;
    DWORD   BaseOfCode;
    DWORD   BaseOfData;

    //
    // NT additional fields.
    //

    DWORD   ImageBase;
    DWORD   SectionAlignment;
    DWORD   FileAlignment;
    WORD    MajorOperatingSystemVersion;
    WORD    MinorOperatingSystemVersion;
    WORD    MajorImageVersion;
    WORD    MinorImageVersion;
    WORD    MajorSubsystemVersion;
    WORD    MinorSubsystemVersion;
    DWORD   Win32VersionValue;
    DWORD   SizeOfImage;
    DWORD   SizeOfHeaders;
    DWORD   CheckSum;
    WORD    Subsystem;
    WORD    DllCharacteristics;
    DWORD   SizeOfStackReserve;
    DWORD   SizeOfStackCommit;
    DWORD   SizeOfHeapReserve;
    DWORD   SizeOfHeapCommit;
    DWORD   LoaderFlags;
    DWORD   NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER32, *PIMAGE_OPTIONAL_HEADER32;


typedef struct _IMAGE_OPTIONAL_HEADER64 {
    WORD        Magic;
    BYTE        MajorLinkerVersion;
    BYTE        MinorLinkerVersion;
    DWORD       SizeOfCode;
    DWORD       SizeOfInitializedData;
    DWORD       SizeOfUninitializedData;
    DWORD       AddressOfEntryPoint;
    DWORD       BaseOfCode;
    ULONGLONG   ImageBase;
    DWORD       SectionAlignment;
    DWORD       FileAlignment;
    WORD        MajorOperatingSystemVersion;
    WORD        MinorOperatingSystemVersion;
    WORD        MajorImageVersion;
    WORD        MinorImageVersion;
    WORD        MajorSubsystemVersion;
    WORD        MinorSubsystemVersion;
    DWORD       Win32VersionValue;
    DWORD       SizeOfImage;
    DWORD       SizeOfHeaders;
    DWORD       CheckSum;
    WORD        Subsystem;
    WORD        DllCharacteristics;
    ULONGLONG   SizeOfStackReserve;
    ULONGLONG   SizeOfStackCommit;
    ULONGLONG   SizeOfHeapReserve;
    ULONGLONG   SizeOfHeapCommit;
    DWORD       LoaderFlags;
    DWORD       NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;

#define IMAGE_SIZEOF_NT_OPTIONAL64_HEADER	240

typedef struct _IMAGE_SECTION_HEADER {
	BYTE Name[IMAGE_SIZEOF_SHORT_NAME];
	union {
		DWORD PhysicalAddress;
		DWORD VirtualSize;
	} Misc;
	DWORD VirtualAddress;
	DWORD SizeOfRawData;
	DWORD PointerToRawData;
	DWORD PointerToRelocations;
	DWORD PointerToLinenumbers;
	WORD NumberOfRelocations;
	WORD NumberOfLinenumbers;
	DWORD Characteristics;
} IMAGE_SECTION_HEADER,*PIMAGE_SECTION_HEADER;

typedef struct _IMAGE_IMPORT_BY_NAME {
	WORD Hint;
	BYTE Name[1];
} IMAGE_IMPORT_BY_NAME,*PIMAGE_IMPORT_BY_NAME;
typedef struct _IMAGE_THUNK_DATA {
	union {
		PBYTE ForwarderString;
		PDWORD Function;
		DWORD Ordinal;
		PIMAGE_IMPORT_BY_NAME AddressOfData;
	} ;
} IMAGE_THUNK_DATA,*PIMAGE_THUNK_DATA;

typedef struct _IMAGE_IMPORT_DESCRIPTOR {
    union {
        DWORD   Characteristics;            // 0 for terminating null import descriptor
        DWORD   OriginalFirstThunk;         // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
    } DUMMYUNIONNAME;
    DWORD   TimeDateStamp;                  // 0 if not bound,
                                            // -1 if bound, and real date\time stamp
                                            //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                            // O.W. date/time stamp of DLL bound to (Old BIND)

    DWORD   ForwarderChain;                 // -1 if no forwarders
    DWORD   Name;
    DWORD   FirstThunk;                     // RVA to IAT (if bound this IAT has actual addresses)
} IMAGE_IMPORT_DESCRIPTOR;

typedef struct _IMAGE_EXPORT_DIRECTORY {
    DWORD   Characteristics;
    DWORD   TimeDateStamp;
    WORD    MajorVersion;
    WORD    MinorVersion;
    DWORD   Name;
    DWORD   Base;
    DWORD   NumberOfFunctions;
    DWORD   NumberOfNames;
    DWORD   AddressOfFunctions;     // RVA from base of image
    DWORD   AddressOfNames;         // RVA from base of image
    DWORD   AddressOfNameOrdinals;  // RVA from base of image
} IMAGE_EXPORT_DIRECTORY, *PIMAGE_EXPORT_DIRECTORY;

//typedef struct _OSVERSIONINFOA {
//	DWORD dwOSVersionInfoSize;
//	DWORD dwMajorVersion;
//	DWORD dwMinorVersion;
//	DWORD dwBuildNumber;
//	DWORD dwPlatformId;
//	CHAR szCSDVersion[128];
//} OSVERSIONINFOA, *POSVERSIONINFOA, *LPOSVERSIONINFOA;
//
//typedef struct _OSVERSIONINFOW {
//	DWORD dwOSVersionInfoSize;
//	DWORD dwMajorVersion;
//	DWORD dwMinorVersion;
//	DWORD dwBuildNumber;
//	DWORD dwPlatformId;
//	WCHAR szCSDVersion[128];
//} OSVERSIONINFOW, *POSVERSIONINFOW, *LPOSVERSIONINFOW, RTL_OSVERSIONINFOW, *PRTL_OSVERSIONINFOW;

#define _FILETIME

/*
proc
*/

#ifdef UNICODE
	typedef WIN32_FIND_DATAW WIN32_FIND_DATA;
	typedef PWIN32_FIND_DATAW PWIN32_FIND_DATA;
	typedef LPWIN32_FIND_DATAW LPWIN32_FIND_DATA;
	typedef struct _OSVERSIONINFOW {
		DWORD dwOSVersionInfoSize;
		DWORD dwMajorVersion;
		DWORD dwMinorVersion;
		DWORD dwBuildNumber;
		DWORD dwPlatformId;
		WCHAR szCSDVersion[128];
	} OSVERSIONINFOW, *POSVERSIONINFOW, *LPOSVERSIONINFOW, RTL_OSVERSIONINFOW, *PRTL_OSVERSIONINFOW;
	typedef LPOSVERSIONINFOW LPOSVERSIONINFO;
#else
	typedef WIN32_FIND_DATAA WIN32_FIND_DATA;
	typedef PWIN32_FIND_DATAA PWIN32_FIND_DATA;
	typedef LPWIN32_FIND_DATAA LPWIN32_FIND_DATA;
	typedef struct _OSVERSIONINFOA {
		DWORD dwOSVersionInfoSize;
		DWORD dwMajorVersion;
		DWORD dwMinorVersion;
		DWORD dwBuildNumber;
		DWORD dwPlatformId;
		CHAR szCSDVersion[128];
	} OSVERSIONINFOA, *POSVERSIONINFOA, *LPOSVERSIONINFOA;
	typedef LPOSVERSIONINFOA LPOSVERSIONINFO;
#endif

int MessageBoxA(HWND,LPCSTR,LPCSTR,UINT);
#define MessageBox MessageBoxA
int MessageBoxW(HWND,LPCWSTR,LPCWSTR,UINT);

typedef struct {int dummy;} _Dummystruct;

//typedef int (*FARPROC)();
//typedef int (*FARPROC)(void);
//typedef int (*FARPROC)(_Dummystruct);
typedef int (*FARPROC)(struct {});

FARPROC GetProcAddress(HINSTANCE,const char*);
HINSTANCE LoadLibraryA(LPCSTR);
HMODULE GetModuleHandleA(char*);
HINSTANCE  LoadLibraryExW(LPCWSTR,HANDLE,DWORD);
HINSTANCE  LoadLibraryExA(LPCSTR,HANDLE,DWORD);

BOOL  SetEvent(HANDLE);
DWORD  WaitForMultipleObjects(DWORD,CONST HANDLE *,BOOL,DWORD);
LPVOID VirtualAlloc(LPVOID,DWORD,DWORD,DWORD);
BOOL VirtualFree(LPVOID,DWORD,DWORD);
BOOL VirtualProtect(LPVOID,DWORD,DWORD,PDWORD);
DWORD VirtualQuery(LPCVOID,MEMORY_BASIC_INFORMATION*,DWORD);
HLOCAL LocalFree(HLOCAL);
LPWSTR GetCommandLineW(VOID);
LPSTR GetCommandLineA(VOID);
LPWSTR* CommandLineToArgvW(LPCWSTR,int*);
DWORD GetModuleFileNameA(HINSTANCE,LPSTR,DWORD);
DWORD GetModuleFileNameW(HINSTANCE,LPWSTR,DWORD);
DWORD GetLastError(VOID);

DWORD FormatMessageA(DWORD,LPCVOID,DWORD,DWORD,LPSTR,DWORD,va_list *);

LONG InterlockedCompareExchange(LONG volatile*, LONG, LONG);
//LONG64 InterlockedCompareExchange64(LONG64 volatile*, LONG64, LONG64);

VOID InitializeCriticalSection(LPCRITICAL_SECTION);
VOID DeleteCriticalSection(LPCRITICAL_SECTION);
VOID EnterCriticalSection(LPCRITICAL_SECTION);
VOID LeaveCriticalSection(LPCRITICAL_SECTION);
DWORD GetCurrentThreadId(VOID);
BOOL CloseHandle(HANDLE);
BOOL FreeLibrary(HINSTANCE);
BOOL AreFileApisANSI(VOID);
VOID OutputDebugStringA(LPCSTR);
HANDLE CreateFileA(LPCSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES,DWORD,DWORD,HANDLE);
HANDLE CreateFileW(LPCWSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES,DWORD,DWORD,HANDLE);
HANDLE CreateFileMappingW(HANDLE,LPSECURITY_ATTRIBUTES,DWORD,DWORD,DWORD,LPCWSTR);
HANDLE CreateFileMappingA(HANDLE,LPSECURITY_ATTRIBUTES,DWORD,DWORD,DWORD,LPCSTR);
HANDLE CreateMutexW(LPSECURITY_ATTRIBUTES,BOOL,LPCWSTR);
HANDLE CreateMutexA(LPSECURITY_ATTRIBUTES,BOOL,LPCSTR);
BOOL DeleteFileA(LPCSTR);
BOOL DeleteFileW(LPCWSTR);
BOOL FlushFileBuffers(HANDLE);
DWORD FormatMessageW(DWORD,LPCVOID,DWORD,DWORD,LPWSTR,DWORD,va_list *);
//DWORD FormatMessageA(DWORD,LPCVOID,DWORD,DWORD,LPSTR,DWORD,va_list *);
DWORD GetCurrentProcessId(VOID);
BOOL GetDiskFreeSpaceW(LPCWSTR,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
BOOL GetDiskFreeSpaceA(LPCSTR,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
DWORD GetFileAttributesW(LPCWSTR);
DWORD GetFileAttributesA(LPCSTR);
BOOL GetFileAttributesExA(LPCSTR,GET_FILEEX_INFO_LEVELS,LPVOID);
BOOL GetFileAttributesExW(LPCWSTR,GET_FILEEX_INFO_LEVELS,LPVOID);
DWORD GetFileSize(HANDLE,LPDWORD);
BOOL SetConsoleMode(HANDLE,DWORD);
BOOL GetConsoleMode(HANDLE,LPDWORD);
HANDLE GetStdHandle(DWORD);
BOOL PeekConsoleInputW(HANDLE,PINPUT_RECORD,DWORD,LPDWORD);
BOOL ReadConsoleInputW(HANDLE,PINPUT_RECORD,DWORD,LPDWORD);
BOOL WriteConsoleInputW(HANDLE,CONST INPUT_RECORD *,DWORD,LPDWORD);
BOOL ReadConsoleOutputW(HANDLE,PCHAR_INFO,COORD,COORD,PSMALL_RECT);
BOOL WriteConsoleOutputW(HANDLE,CONST CHAR_INFO *,COORD,COORD,PSMALL_RECT);
BOOL ReadConsoleOutputCharacterW(HANDLE,LPWSTR,DWORD,COORD,LPDWORD);
BOOL WriteConsoleOutputCharacterW(HANDLE,LPCWSTR,DWORD,COORD,LPDWORD);
BOOL FillConsoleOutputCharacterW(HANDLE,WCHAR,DWORD,COORD,LPDWORD);
BOOL ScrollConsoleScreenBufferW(HANDLE,CONST SMALL_RECT *,CONST SMALL_RECT *,COORD,CONST CHAR_INFO *);
BOOL ScrollConsoleScreenBufferA(HANDLE,CONST SMALL_RECT *,CONST SMALL_RECT *,COORD,CONST CHAR_INFO *);
DWORD GetConsoleTitleW(LPWSTR,DWORD);
BOOL SetConsoleTitleW(LPCWSTR);
BOOL ReadConsoleW(HANDLE,LPVOID,DWORD,LPDWORD,LPVOID);
BOOL WriteConsoleW(HANDLE,CONST VOID *,DWORD,LPDWORD,LPVOID);
BOOL ReadConsoleOutputAttribute(HANDLE,LPWORD,DWORD,COORD,LPDWORD);
BOOL WriteConsoleOutputAttribute(HANDLE,CONST WORD *,DWORD,COORD,LPDWORD);
BOOL FillConsoleOutputAttribute(HANDLE,WORD,DWORD,COORD,LPDWORD);
BOOL GetConsoleMode(HANDLE,LPDWORD);
BOOL GetNumberOfConsoleInputEvents(HANDLE,LPDWORD);
BOOL GetConsoleScreenBufferInfo(HANDLE,PCONSOLE_SCREEN_BUFFER_INFO);
COORD GetLargestConsoleWindowSize(HANDLE);
BOOL GetConsoleCursorInfo(HANDLE,PCONSOLE_CURSOR_INFO);
BOOL GetNumberOfConsoleMouseButtons(LPDWORD);
BOOL SetConsoleMode(HANDLE,DWORD);
BOOL SetConsoleActiveScreenBuffer(HANDLE);
BOOL FlushConsoleInputBuffer(HANDLE);
BOOL SetConsoleScreenBufferSize(HANDLE,COORD);
BOOL SetConsoleCursorPosition(HANDLE,COORD);
BOOL SetConsoleCursorInfo(HANDLE,CONST CONSOLE_CURSOR_INFO *);
BOOL SetConsoleWindowInfo(HANDLE,BOOL,CONST SMALL_RECT *);
BOOL SetConsoleTextAttribute(HANDLE,WORD);
BOOL SetConsoleCtrlHandler(PHANDLER_ROUTINE,BOOL);
BOOL GenerateConsoleCtrlEvent(DWORD,DWORD);
BOOL AllocConsole(VOID);
BOOL FreeConsole(VOID);
HANDLE CreateConsoleScreenBuffer(DWORD,DWORD,CONST SECURITY_ATTRIBUTES *,DWORD,LPVOID);
DWORD GetFileType(HANDLE);
DWORD WaitForSingleObject(HANDLE,DWORD);
BOOL PeekNamedPipe(HANDLE,LPVOID,DWORD,LPDWORD,LPDWORD,LPDWORD);
BOOL CreatePipe(PHANDLE,PHANDLE,LPSECURITY_ATTRIBUTES,DWORD);
BOOL GetMessageA(LPMSG,HWND,UINT,UINT);
SHORT GetKeyState(int);
BOOL TranslateMessage(CONST MSG *);
BOOL PeekMessageA(LPMSG,HWND,UINT,UINT,UINT);
LONG DispatchMessageA(CONST MSG *);
BOOL IsWindow(HWND);
BOOL IsMenu(HMENU);
BOOL IsChild(HWND,HWND);
BOOL DestroyWindow(HWND);
BOOL ShowWindow(HWND,int);
SHORT GetKeyState(int);
SHORT GetAsyncKeyState(int);
BOOL GetKeyboardState(PBYTE);
BOOL SetKeyboardState(LPBYTE);
int GetKeyboardType(int);
BOOL GetExitCodeProcess(HANDLE,LPDWORD);
BOOL TerminateProcess(HANDLE,UINT);
BOOL SetHandleInformation(HANDLE,DWORD,DWORD);
HANDLE FindFirstFileA(LPCSTR,LPWIN32_FIND_DATA);
HPEN CreatePen(int,int,COLORREF);
HBRUSH CreateSolidBrush(COLORREF);
HGDIOBJ SelectObject(HDC,HGDIOBJ);
VOID PostQuitMessage(int);

BOOL PlaySoundA(LPCSTR,HMODULE,DWORD);
HICON LoadIconW(HINSTANCE,LPCWSTR);
HICON LoadIconA(HINSTANCE,LPCSTR);
HCURSOR LoadCursorW(HINSTANCE,LPCWSTR);
HCURSOR LoadCursorA(HINSTANCE,LPCSTR);
HGDIOBJ GetStockObject(int);
ATOM RegisterClassW(CONST WNDCLASS *);
ATOM RegisterClassA(CONST WNDCLASS *);
BOOL UpdateWindow(HWND);
HDC BeginPaint( HWND,LPPAINTSTRUCT);
BOOL EndPaint(HWND,CONST PAINTSTRUCT *);
BOOL GetClientRect(HWND,LPRECT);
int DrawTextW(HDC,LPCWSTR,int,LPRECT,UINT);
int DrawTextA(HDC,LPCSTR,int,LPRECT,UINT);
LRESULT DefWindowProcW(HWND,UINT,WPARAM,LPARAM);
LRESULT DefWindowProcA(HWND,UINT,WPARAM,LPARAM);
BOOL FillConsoleOutputCharacterA(HANDLE,WCHAR,DWORD,COORD,LPDWORD);
BOOL FillConsoleOutputCharacterW(HANDLE,WCHAR,DWORD,COORD,LPDWORD);
VOID Sleep(DWORD);
HANDLE CreateEventW(LPSECURITY_ATTRIBUTES,BOOL,BOOL,LPCWSTR);
HANDLE CreateEventA(LPSECURITY_ATTRIBUTES,BOOL,BOOL,LPCSTR);
HANDLE CreateThread(LPSECURITY_ATTRIBUTES,DWORD,LPTHREAD_START_ROUTINE,LPVOID,DWORD,LPDWORD);
LPVOID MapViewOfFile(HANDLE,DWORD,DWORD,DWORD,DWORD);
BOOL ReadConsoleOutputCharacterW(HANDLE,LPWSTR,DWORD,COORD,LPDWORD);
BOOL ReadConsoleOutputCharacterA(HANDLE,LPSTR,DWORD,COORD,LPDWORD);
BOOL WriteConsoleInputW(HANDLE,CONST INPUT_RECORD *,DWORD,LPDWORD);
BOOL WriteConsoleInputA(HANDLE,CONST INPUT_RECORD *,DWORD,LPDWORD);
UINT GetSystemDirectoryW(LPWSTR,UINT);
UINT GetSystemDirectoryA(LPSTR,UINT);
DWORD SearchPathW(LPCWSTR,LPCWSTR,LPCWSTR,DWORD,LPWSTR,LPWSTR *);
DWORD SearchPathA(LPCSTR,LPCSTR,LPCSTR,DWORD,LPSTR,LPSTR *);
DWORD GetTickCount(VOID);
ATOM RegisterClassExW(CONST WNDCLASSEX *);
ATOM RegisterClassExA(CONST WNDCLASSEX *);
HWND CreateWindowExW(DWORD,LPCWSTR,LPCWSTR,DWORD,int,int,int,int,HWND,HMENU,HINSTANCE,LPVOID);
HWND CreateWindowExA(DWORD,LPCSTR,LPCSTR,DWORD,int,int,int,int,HWND,HMENU,HINSTANCE,LPVOID);
COLORREF SetPixel(HDC,int,int,COLORREF);
COLORREF GetPixel(HDC,int,int);
BOOL LineTo(HDC,int,int);
VOID GetSystemTimeAsFileTime(LPFILETIME);
BOOL MoveWindow(HWND,int,int,int,int,BOOL);
BOOL GetWindowRect(HWND,LPRECT);
BOOL ScreenToClient(HWND,LPPOINT);
HWND GetParent(HWND);
HWND SetFocus(HWND);
BOOL InvalidateRect(HWND,CONST RECT *,BOOL);
BOOL DeleteObject(HGDIOBJ);
LONG GetWindowLongA(HWND,int);
LONG GetWindowLongW(HWND,int);
LONG SetWindowLongA(HWND,int,LONG);
LONG SetWindowLongW(HWND,int,LONG);
HDC CreateCompatibleDC(HDC);
BOOL BitBlt(HDC,int,int,int,int,HDC,int,int,DWORD);
BOOL DeleteDC(HDC);
LRESULT SendMessageW(HWND,UINT,WPARAM,LPARAM);
LRESULT SendMessageA(HWND,UINT,WPARAM,LPARAM);
LRESULT SendMessageTimeoutW(HWND,UINT,WPARAM,LPARAM,UINT,UINT,LPDWORD);
LRESULT SendMessageTimeoutA(HWND,UINT,WPARAM,LPARAM,UINT,UINT,LPDWORD);
BOOL ClientToScreen(HWND,LPPOINT);
HDC GetDC(HWND);
HBITMAP CreateDIBSection(HDC,CONST BITMAPINFO *,UINT,VOID **,HANDLE,DWORD);
BOOL OpenClipboard(HWND);
BOOL CloseClipboard(VOID);
BOOL EmptyClipboard(VOID);
BOOL IsClipboardFormatAvailable(UINT);
HANDLE SetClipboardData(UINT,HANDLE);
HANDLE GetClipboardData(UINT);
HGLOBAL GlobalAlloc(UINT,DWORD);
LPVOID GlobalLock(HGLOBAL);
LPVOID GlobalUnlock(HGLOBAL);
BOOL Rectangle(HDC,int,int,int,int);
HWND GetDesktopWindow(VOID);
BOOL AdjustWindowRect(LPRECT,DWORD,BOOL);
BOOL MoveToEx(HDC,int,int,LPPOINT);
HWND SetCapture(HWND hWnd);
BOOL ReleaseCapture(VOID);
int MapWindowPoints(HWND,HWND,LPPOINT,UINT);
BOOL GetCursorPos(LPPOINT);
DWORD GetLogicalDriveStringsW(DWORD,LPWSTR);
DWORD GetLogicalDriveStringsA(DWORD,LPSTR);
HANDLE FindFirstFileW(LPCWSTR,LPWIN32_FIND_DATAW);
HANDLE FindFirstFileA(LPCSTR,LPWIN32_FIND_DATA);
BOOL FindNextFileW(HANDLE,LPWIN32_FIND_DATAW);
BOOL FindNextFileA(HANDLE,LPWIN32_FIND_DATAA);
BOOL FindClose(HANDLE);
BOOL QueryPerformanceCounter(LARGE_INTEGER *);
BOOL QueryPerformanceFrequency(LARGE_INTEGER *);
UINT SetTimer(HWND,UINT,UINT,TIMERPROC);
BOOL KillTimer(HWND,UINT);
HBITMAP CreateCompatibleBitmap(HDC,int,int);
HBITMAP CreateDiscardableBitmap(HDC,int,int);
HDC CreateCompatibleDC(HDC);
int FillRect(HDC,CONST RECT *,HBRUSH);
LONG RegOpenKeyExW(HKEY,LPCWSTR,DWORD,REGSAM,PHKEY);
LONG RegOpenKeyExA(HKEY,LPCSTR,DWORD,REGSAM,PHKEY);
LONG RegQueryValueExW(HKEY,LPCWSTR,LPDWORD,LPDWORD,LPBYTE,LPDWORD);
LONG RegQueryValueExA (HKEY,LPCSTR,LPDWORD,LPDWORD,LPBYTE,LPDWORD);
LONG RegSetValueExW(HKEY,LPCWSTR,DWORD,DWORD,BYTE*,DWORD);
LONG RegSetValueExA(HKEY,LPCSTR,DWORD,DWORD,BYTE*,DWORD);
LONG RegCloseKey (HKEY);
BOOL SystemTimeToFileTime(CONST SYSTEMTIME *,LPFILETIME);
BOOL FileTimeToLocalFileTime(CONST FILETIME *,LPFILETIME);
BOOL LocalFileTimeToFileTime(CONST FILETIME *,LPFILETIME);
BOOL FileTimeToSystemTime(CONST FILETIME *,LPSYSTEMTIME);
LONG CompareFileTime(CONST FILETIME *,CONST FILETIME *);
DWORD GetFullPathNameW(LPCWSTR,DWORD,LPWSTR,LPWSTR *);
DWORD GetFullPathNameA(LPCSTR,DWORD,LPSTR,LPSTR *);
VOID GetSystemInfo(LPSYSTEM_INFO);
VOID GetSystemTime(LPSYSTEMTIME);
DWORD GetTempPathW(DWORD,LPWSTR);
DWORD GetTempPathA(DWORD,LPSTR);
BOOL GetVersionExW(LPOSVERSIONINFO);
BOOL GetVersionExA(LPOSVERSIONINFO);
HANDLE HeapCreate(DWORD,DWORD,DWORD);
BOOL HeapDestroy(HANDLE);
LPVOID HeapAlloc(HANDLE,DWORD,DWORD);
LPVOID HeapReAlloc(HANDLE,DWORD,LPVOID,DWORD);
BOOL HeapFree(HANDLE,DWORD,LPVOID);
DWORD HeapSize(HANDLE,DWORD,LPCVOID);
BOOL HeapValidate(HANDLE,DWORD,LPCVOID);
UINT HeapCompact(HANDLE,DWORD);
HINSTANCE LoadLibraryW(LPCWSTR);
HINSTANCE LoadLibraryExW(LPCWSTR,HANDLE,DWORD);
HINSTANCE LoadLibraryA(LPCSTR);
HINSTANCE LoadLibraryExA(LPCSTR,HANDLE,DWORD);
BOOL LockFile( HANDLE,DWORD,DWORD,DWORD,DWORD);
BOOL UnlockFile(HANDLE,DWORD,DWORD,DWORD,DWORD);
BOOL LockFileEx(HANDLE,DWORD,DWORD,DWORD,DWORD,LPOVERLAPPED);
BOOL UnlockFileEx(HANDLE,DWORD,DWORD,DWORD,LPOVERLAPPED);
int MultiByteToWideChar(UINT,DWORD,LPCSTR,int,LPWSTR,int);
int WideCharToMultiByte(UINT,DWORD,LPCWSTR,int,LPSTR,int,LPCSTR,LPBOOL);
BOOL WriteFile(HANDLE,LPCVOID,DWORD,LPDWORD,LPOVERLAPPED);
BOOL ReadFile(HANDLE,LPVOID,DWORD,LPDWORD,LPOVERLAPPED);
BOOL SetEndOfFile(HANDLE);
DWORD SetFilePointer(HANDLE,LONG,PLONG,DWORD);
DWORD WaitForSingleObject(HANDLE,DWORD);
DWORD WaitForMultipleObjects(DWORD,CONST HANDLE *,BOOL,DWORD);
DWORD WaitForSingleObjectEx(HANDLE,DWORD,BOOL);
DWORD WaitForMultipleObjectsEx(DWORD,CONST HANDLE *,BOOL,DWORD,BOOL);
VOID OutputDebugStringW(LPCWSTR);
VOID OutputDebugStringA(LPCSTR);
HANDLE GetProcessHeap(VOID);
DWORD WINAPI GetProcessIdOfThread(HANDLE);
DWORD WINAPI GetProcessId(HANDLE);
DWORD WINAPI GetCurrentProcessorNumber(void);
BOOL WINAPI GetProcessHandleCount(HANDLE,PDWORD);
BOOL WINAPI GetProcessIoCounters(HANDLE,PIO_COUNTERS);
BOOL WINAPI GetProcessWorkingSetSize(HANDLE,PSIZE_T,PSIZE_T);
BOOL WINAPI GetProcessWorkingSetSizeEx(HANDLE,PSIZE_T,PSIZE_T,PDWORD);
BOOL WINAPI SetProcessWorkingSetSize(HANDLE,SIZE_T,SIZE_T);
BOOL WINAPI SetProcessWorkingSetSizeEx(HANDLE,SIZE_T,SIZE_T,DWORD);
DWORD GetProcessHeaps(DWORD,PHANDLE);
DWORD GetProcessVersion(DWORD);
LPVOID MapViewOfFile(HANDLE,DWORD,DWORD,DWORD,DWORD);
BOOL FlushViewOfFile(LPCVOID,DWORD);
BOOL UnmapViewOfFile(LPCVOID);
HANDLE GetCurrentProcess(VOID);
BOOL SetFileTime(HANDLE,CONST FILETIME *,CONST FILETIME *,CONST FILETIME *);
DWORD GetEnvironmentVariableW(LPCWSTR,LPWSTR,DWORD);
BOOL SetEnvironmentVariableW(LPCWSTR,LPCWSTR);
DWORD GetEnvironmentVariableA(LPCSTR,LPSTR,DWORD);
BOOL SetEnvironmentVariableA(LPCSTR,LPCSTR);
BOOL SetCurrentDirectoryW(LPCWSTR);
DWORD GetCurrentDirectoryW(DWORD,LPWSTR);
BOOL SetCurrentDirectoryA(LPCSTR);
DWORD GetCurrentDirectoryA(DWORD,LPSTR);
VOID DebugBreak(VOID);
HMENU GetMenu(HWND);
BOOL AdjustWindowRectEx(LPRECT,DWORD,BOOL,DWORD);
BOOL SetWindowPos(HWND,HWND,int,int,int,int,UINT);
HBRUSH GetSysColorBrush(int);
BOOL GetUpdateRect(HWND,LPRECT,BOOL);
BOOL AngleArc(HDC,int,int,DWORD,FLOAT,FLOAT);
BOOL BeginPath(HDC);
BOOL CloseFigure(HDC);
BOOL EndPath(HDC);
BOOL FillPath(HDC);
BOOL FlattenPath(HDC);
BOOL StrokeAndFillPath(HDC);
BOOL StrokePath(HDC);
BOOL WidenPath(HDC);
BOOL Ellipse(HDC,int,int,int,int);
int ReleaseDC(HWND,HDC);
DWORD GetWindowThreadProcessId(HWND,LPDWORD);
int GetSystemMetrics(int);
HFONT CreateFontW(int,int,int,int,int,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,LPCWSTR);
HFONT CreateFontA(int,int,int,int,int,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,LPCSTR);
BOOL Polygon(HDC,CONST POINT *,int);
COLORREF SetTextColor(HDC,COLORREF);
COLORREF SetBkColor(HDC,COLORREF);
UINT SetTextAlign(HDC,UINT);
BOOL TextOutW(HDC,int,int,LPCWSTR,int);
BOOL TextOutA(HDC,int,int,LPCSTR,int);
DWORD GetLogicalDrives(VOID);
BOOL GetFileInformationByHandle(HANDLE,LPBY_HANDLE_FILE_INFORMATION);
HANDLE CreateFileMappingW(HANDLE,LPSECURITY_ATTRIBUTES,DWORD,DWORD,DWORD,LPCWSTR);
HANDLE CreateFileMappingA(HANDLE,LPSECURITY_ATTRIBUTES,DWORD,DWORD,DWORD,LPCSTR);
HWND GetTopWindow(HWND);
HWND GetActiveWindow(VOID);
BOOL SetWindowTextA(HWND,LPCSTR);

//!=================

/*
proc
*/
#define GetWindowLongPtrW GetWindowLongW
#define GetWindowLongPtrA GetWindowLongA
#define SetWindowLongPtrW SetWindowLongW
#define SetWindowLongPtrA SetWindowLongA

#ifdef UNICODE
	#define GetModuleHandle GetModuleHandleW
	#define LoadLibrary LoadLibraryW
	#define LoadLibraryEx LoadLibraryExW
	#define OutputDebugString OutputDebugStringW
	#define GetMessage GetMessageW
	#define PeekMessage PeekMessageW
	#define DispatchMessage DispatchMessageW
	#define PlaySound PlaySoundW
	#define TEXT(quote) L##quote
	#define LoadIcon LoadIconW
	typedef unsigned short *LPTSTR;
	typedef unsigned short TCHAR;
	#define LoadCursor LoadCursorW
	#define RegisterClass RegisterClassW
	#define CreateWindowEx CreateWindowExW
	#define CreateWindowW(Class,Name,Style,x,y,W,H,Parent,Menu,Inst,Param) CreateWindowExW(0,Class,Name,Style,x,y,W,H,Parent,Menu,Inst,Param)
	#define CreateWindow CreateWindowW
	#define DrawText DrawTextW
	#define DefWindowProc DefWindowProcW
	#define FillConsoleOutputCharacter FillConsoleOutputCharacterW
	#define CreateEvent CreateEventW
	#define ReadConsoleOutputCharacter ReadConsoleOutputCharacterW
	#define WriteConsoleInput WriteConsoleInputW
	#define GetSystemDirectory GetSystemDirectoryW
	#define SearchPath SearchPathW
	#define RegisterClassEx RegisterClassExA
	#define GetWindowLong GetWindowLongW
	#define GetWindowLongPtr GetWindowLongPtrW
	#define SetWindowLong SetWindowLongW
	#define SetWindowLongPtr SetWindowLongPtrW
	#define SendMessage SendMessageW
	#define SendMessageTimeout SendMessageTimeoutW
	#define GetLogicalDriveStrings GetLogicalDriveStringsW
	#define FindFirstFile FindFirstFileW
	#define FindNextFile FindNextFileW
	#define GetCommandLine GetCommandLineW
	#define ScrollConsoleScreenBuffer ScrollConsoleScreenBufferW
	#define CreateProcess CreateProcessW
	#define LPSTARTUPINFO LPSTARTUPINFOW
	#define CreateFile CreateFileW
	#define RegOpenKeyEx RegOpenKeyExW
	#define RegQueryValueEx RegQueryValueExW
	#define RegSetValueEx RegSetValueExW
	#define GetFullPathname GetFullPathNameW
	#define GetTempPath GetTempPathW
	#define GetEnvironmentVariable GetEnvironmentVariableW
	#define SetEnvironmentVariable SetEnvironmentVariableW
	#define GetCurrentDirectory GetCurrentDirectoryW
	#define SetCurrentDirectory SetCurrentDirectoryW
	#define CreateFont CreateFontW
	#define TextOut TextOutW
	#define CreateFileMapping CreateFileMappingW
	#define OSVERSIONINFO OSVERSIONINFOW
	#define STARTUPINFO STARTUPINFOW
#else
	#define GetModuleHandle GetModuleHandleA
	#define LoadLibrary LoadLibraryA

	#define LoadLibraryEx LoadLibraryExA
	#define OutputDebugString OutputDebugStringA
	#define GetMessage GetMessageA
	#define PeekMessage PeekMessageA
	#define DispatchMessage DispatchMessageA
	#define PlaySound PlaySoundA
	#define TEXT(quote) quote
	#define LoadIcon LoadIconA
	typedef unsigned char *LPTSTR;
	typedef unsigned char TCHAR;
	#define LoadCursor LoadCursorA
	#define RegisterClass RegisterClassA
	#define CreateWindowEx CreateWindowExA
	#define CreateWindowA(C,WN,S,x,y,W,H,hW,hM,hI,l) CreateWindowExA(0,C,WN,S,x,y,W,H,hW,hM,hI,l)
	#define CreateWindow CreateWindowA
	#define DrawText DrawTextA
	#define DefWindowProc DefWindowProcA
	#define FillConsoleOutputCharacter FillConsoleOutputCharacterA
	#define CreateEvent CreateEventA
	#define ReadConsoleOutputCharacter ReadConsoleOutputCharacterA
	#define WriteConsoleInput WriteConsoleInputA
	#define GetSystemDirectory GetSystemDirectoryA
	#define SearchPath SearchPathA
	#define RegisterClassEx RegisterClassExA
	#define GetWindowLong GetWindowLongA
	#define GetWindowLongPtr GetWindowLongPtrA
	#define SetWindowLong SetWindowLongA
	#define SetWindowLongPtr SetWindowLongPtrA
	#define SendMessage SendMessageA
	#define SendMessageTimeout SendMessageTimeoutA
	#define GetLogicalDriveStrings GetLogicalDriveStringsA
	#define FindFirstFile FindFirstFileA
	#define FindNextFile FindNextFileA
	#define GetCommandLine GetCommandLineA
	#define ScrollConsoleScreenBuffer ScrollConsoleScreenBufferA
	#define CreateProcess CreateProcessA
	#define LPSTARTUPINFO LPSTARTUPINFOA
	#define CreateFile CreateFileA
	#define RegOpenKeyEx RegOpenKeyExA
	#define RegQueryValueEx RegQueryValueExA
	#define RegSetValueEx RegSetValueExA
	#define GetFullPathname GetFullPathNameA
	#define GetVersionEx GetVersionExA
	#define GetEnvironmentVariable GetEnvironmentVariableA
	#define SetEnvironmentVariable SetEnvironmentVariableA
	#define GetCurrentDirectory GetCurrentDirectoryA
	#define SetCurrentDirectory SetCurrentDirectoryA
	#define CreateFont CreateFontA
	#define TextOut TextOutA
	#define CreateFileMapping CreateFileMappingA
	#define OSVERSIONINFO OSVERSIONINFOA
	#define STARTUPINFO STARTUPINFOA
	#define SetWindowText SetWindowTextA
#endif // windows.h

BOOL CreateProcessW(LPCWSTR,LPWSTR,LPSECURITY_ATTRIBUTES,LPSECURITY_ATTRIBUTES,BOOL,DWORD,LPVOID,LPCWSTR,LPSTARTUPINFOW,LPPROCESS_INFORMATION);
BOOL CreateProcessA(LPCSTR,LPSTR,LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES,BOOL,DWORD,LPVOID,LPCSTR,LPSTARTUPINFOA,LPPROCESS_INFORMATION);

#endif
=== fcntl.h 62/73 ===
/* FCNTL.H */

int _setmode(int,int);

#ifndef _INC_FCNTL
#define _INC_FCNTL

#define _O_RDONLY 0x0000
#define _O_WRONLY 0x0001
#define _O_RDWR 0x0002
#define _O_APPEND 0x0008
#define _O_CREAT 0x0100
#define _O_TRUNC 0x0200
#define _O_EXCL 0x0400
#define _O_TEXT 0x4000
#define _O_BINARY 0x8000
#define _O_WTEXT 0x10000
#define _O_U16TEXT 0x20000
#define _O_U8TEXT 0x40000
#define _O_ACCMODE (_O_RDONLY|_O_WRONLY|_O_RDWR)

#define _O_RAW _O_BINARY
#define _O_NOINHERIT 0x0080
#define _O_TEMPORARY 0x0040
#define _O_SHORT_LIVED 0x1000

#define _O_SEQUENTIAL 0x0020
#define _O_RANDOM 0x0010

#if !defined(NO_OLDNAMES) || defined(_POSIX)
#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY
#define O_RDWR _O_RDWR
#define O_APPEND _O_APPEND
#define O_CREAT _O_CREAT
#define O_TRUNC _O_TRUNC
#define O_EXCL _O_EXCL
#define O_TEXT _O_TEXT
#define O_BINARY _O_BINARY
#define O_RAW _O_BINARY
#define O_TEMPORARY _O_TEMPORARY
#define O_NOINHERIT _O_NOINHERIT
#define O_SEQUENTIAL _O_SEQUENTIAL
#define O_RANDOM _O_RANDOM
#define O_ACCMODE _O_ACCMODE
#endif

#endif
=== io.h 63/73 ===

#ifndef $io
#define $io

#include <stdint.h>
#include <wchar.h>
#include <time.h>

/* io.h */

#define _A_NORMAL 0x00
#define _A_RDONLY 0x01
#define _A_HIDDEN 0x02
#define _A_SYSTEM 0x04
#define _A_SUBDIR 0x10
#define _A_ARCH 0x20


//#message "IO included"
struct _finddata_t {
        unsigned    attrib;
        time_t      time_create;
        time_t      time_access;
        time_t      time_write;
        unsigned long    size;
        char        name[260];
};

int _isatty(int);

int _read(int, void*, unsigned int);
#define read _read

int _open(const char*, int, ...);
#define open _open

long _lseek(int, long, int);
#define lseek _lseek

int _close(int);
#define close _close

intptr_t _get_osfhandle(int);
int _open_osfhandle(intptr_t,int);

int _setmode(int,int);
#define setmode _setmode

int _wchmod(const wchar_t*, int);

int _chmod(const char *, int);
#define chmod _chmod

int _dup(int);
#define dup _dup

intptr_t _findfirst(const char*, struct _finddata_t*);
int _findnext(intptr_t, struct _finddata_t*);
int _findclose(intptr_t);

int _access(const char*,int);
#define access _access

int _wunlink(const wchar_t*);


#endif
=== direct.h 64/73 ===
#ifndef $direct
#define $direct

typedef struct {
 unsigned int total_clusters;
 unsigned int avail_clusters;
 unsigned int sectors_per_cluster;
 unsigned int bytes_per_sector;
} diskfree_t;

int chdir(const char *);
char* _getcwd(char *, int);
#define getcwd _getcwd
int _mkdir(const char *);
int _rmdir(const char *);
int _wrmdir(const wchar_t*);
int _wmkdir(const wchar_t*);

#define mkdir _mkdir
#define rmdir _rmdir

int _chdrive(int);
char* _getdcwd(int, char *, int);
wchar_t* _wgetcwd(wchar_t*, int);
int _wchdir(const wchar_t*);
int _getdrive(void);
int _chdir(const char*);
#define chdir _chdir

unsigned long _getdrives(void);
unsigned int _getdiskfree(unsigned int, diskfree_t*);

#endif

=== process.h 65/73 ===
/* Header process.h */
#ifndef $process
#define $process

#define P_WAIT		0
#define P_NOWAIT	1
#define P_OVERLAY	2
#define P_DETACH	4
#define WAIT_CHILD 0
#define _P_WAIT P_WAIT
#define _P_NOWAIT P_NOWAIT
#define _P_OVERLAY P_OVERLAY

int _spawnvp(int, const char*, const char*const*);
#define spawnvp _spawnvp

void endthread(void);
unsigned long _beginthreadex(void *,unsigned,unsigned (*)(void *),void *,unsigned,unsigned *);
void _endthreadex(unsigned);
int _cwait(int*, int, int);
#define cwait _cwait
//int _System(const char *cmd,int nCmdShow);

//#define _WAIT_CHILD	0
//#endif
#endif

=== malloc.h 66/73 ===
#include <stddef.h>

void* malloc(size_t);
void* realloc(void *, size_t);
void  free(void *);
=== bcc.h 67/73 ===
#define __attribute__(x)
#define _WIN32
#define WIN32
#define __inline
#define __dllimport(x)
#define __declspec(x)
#define __stdcall
#define CALLBACK $callback
#define __cdecl
#define EXTERN_C extern
#define DECLSPEC_IMPORT
=== conio.h 68/73 ===
/* conio.h */
#include <stddef.h>
#include <wchar.h>

int _getch(void);
int _kbhit(void);
wint_t _getwch(void);

#define getch _getch
#define kbhit _kbhit

int _putch(int);
int _cprintf(char *, ...);
int _getche(void);
int _ungetch(int);

#define putch _putch
#define cprintf _cprintf
#define getche _getche
#define ungetch _ungetch
=== winsock2.h 69/73 ===
#ifndef _WINSOCK2_H
#define _WINSOCK2_H

#include <_mingw.h>
#include <windows.h>

#ifndef FD_SETSIZE
#define FD_SETSIZE 64
#endif

typedef unsigned int SOCKET;

typedef struct fd_set {
  unsigned int   fd_count;
  SOCKET  fd_array[FD_SETSIZE];
} fd_set;

extern int __stdcall __WSAFDIsSet(SOCKET,fd_set *);

#ifndef FD_CLR
#define FD_CLR(fd,set) do { unsigned int __i;\
for (__i = 0; __i < ((fd_set *)(set))->fd_count ; __i++) {\
    if (((fd_set *)(set))->fd_array[__i] == (fd)) {\
    while (__i < ((fd_set *)(set))->fd_count-1) {\
        ((fd_set*)(set))->fd_array[__i] = ((fd_set*)(set))->fd_array[__i+1];\
        __i++;\
    }\
    ((fd_set*)(set))->fd_count--;\
    break;\
    }\
}\
} while (0)
#endif

#ifndef FD_SET
/* this differs from the define in winsock.h and in cygwin sys/types.h */
#define FD_SET(fd, set) do { unsigned int __i;\
for (__i = 0; __i < ((fd_set *)(set))->fd_count ; __i++) {\
    if (((fd_set *)(set))->fd_array[__i] == (fd)) {\
        break;\
    }\
}\
if (__i == ((fd_set *)(set))->fd_count) {\
    if (((fd_set *)(set))->fd_count < FD_SETSIZE) {\
        ((fd_set *)(set))->fd_array[__i] = (fd);\
        ((fd_set *)(set))->fd_count++;\
    }\
}\
} while(0)
#endif

#ifndef FD_ZERO
#define FD_ZERO(set) (((fd_set *)(set))->fd_count=0)
#endif

#ifndef FD_ISSET
#define FD_ISSET(fd, set) __WSAFDIsSet((SOCKET)(fd), (fd_set *)(set))
#endif

#ifndef _TIMEVAL_DEFINED /* also in sys/time.h */
#define _TIMEVAL_DEFINED
struct timeval {
    long tv_sec;
    long tv_usec;
};
#endif

struct in_addr {
    union {
        struct { unsigned char s_b1,s_b2,s_b3,s_b4; } S_un_b;
        struct { unsigned short s_w1,s_w2; } S_un_w;
        unsigned long S_addr;
    } S_un;
};
#define s_addr  S_un.S_addr

struct sockaddr_in {
    short sin_family;
    unsigned short sin_port;
    struct in_addr sin_addr;
    char sin_zero[8];
};

struct sockaddr {
    unsigned short sa_family;
    char sa_data[14];
};

struct hostent {
    char *h_name;
    char **h_aliases;
    short h_addrtype;
    short h_length;
    char **h_addr_list;
};
#define h_addr h_addr_list[0]

#define WSADESCRIPTION_LEN  256
#define WSASYS_STATUS_LEN   128
typedef struct WSAData {
    WORD wVersion;
    WORD wHighVersion;
    char szDescription[WSADESCRIPTION_LEN+1];
    char szSystemStatus[WSASYS_STATUS_LEN+1];
    unsigned short iMaxSockets;
    unsigned short iMaxUdpDg;
    char *lpVendorInfo;
} WSADATA;
typedef WSADATA *LPWSADATA;

#define INVALID_SOCKET (SOCKET)(~0)
#define SOCK_STREAM  1
#define SO_REUSEADDR 4
#define AF_INET 2
#define MSG_PEEK 0x2
#define INADDR_ANY (unsigned long)0x00000000
#define INADDR_LOOPBACK 0x7f000001
#define SOL_SOCKET 0xffff

#define SD_RECEIVE  0x00
#define SD_SEND     0x01
#define SD_BOTH     0x02

#define h_errno WSAGetLastError()

#define WSABASEERR 10000
#define TRY_AGAIN (WSABASEERR+1002)

SOCKET __stdcall accept(SOCKET, struct sockaddr *, int *);
int __stdcall bind(SOCKET, const struct sockaddr *, int);
int __stdcall closesocket(SOCKET);
int __stdcall connect(SOCKET, const struct sockaddr *, int);
struct hostent *__stdcall gethostbyname(const char *);
int __stdcall gethostname(char *, int);
int __stdcall getpeername(SOCKET, struct sockaddr *, int *);
int __stdcall getsockname(SOCKET, struct sockaddr *, int *);
unsigned long __stdcall htonl(unsigned long);
unsigned short __stdcall htons(unsigned short);
int __stdcall listen(SOCKET, int);
unsigned long __stdcall ntohl(unsigned long);
unsigned short __stdcall ntohs(unsigned short);
int __stdcall recv(SOCKET, char *, int, int);
int __stdcall recvfrom(SOCKET, char *, int, int, struct sockaddr *, int *);
int __stdcall select(int nfds, fd_set *, fd_set *, fd_set *, const struct timeval *);
int __stdcall send(SOCKET, const char *, int, int);
int __stdcall sendto(SOCKET, const char *, int, int, const struct sockaddr *, int);
int __stdcall setsockopt(SOCKET, int, int, const char *, int);
int __stdcall shutdown(SOCKET, int);
int __stdcall WSACleanup(void);
int __stdcall WSAGetLastError(void);
int __stdcall WSAStartup(WORD, LPWSADATA);

SOCKET __stdcall socket(int, int, int);

#endif

=== _mingw.h 70/73 ===
/*
 * _mingw.h
 *
 *  This file is for TinyCC and not part of the Mingw32 package.
 *
 *  THIS SOFTWARE IS NOT COPYRIGHTED
 *
 *  This source code is offered for use in the public domain. You may
 *  use, modify or distribute it freely.
 *
 *  This code is distributed in the hope that it will be useful but
 *  WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESS OR IMPLIED ARE HEREBY
 *  DISCLAIMED. This includes but is not limited to warranties of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 */

#ifndef __MINGW_H
#define __MINGW_H

/* some winapi files define these before including _mingw.h --> */
#undef __cdecl
#undef _X86_
#undef WIN32
/* <-- */

#include <stddef.h>
#include <stdarg.h>

#define __int8 char
#define __int16 short
#define __int32 int
#define __int64 long long
#define _HAVE_INT64

#define __cdecl
#define __declspec(x) __attribute__((x))
#define __unaligned __attribute__((packed))
#define __fastcall __attribute__((fastcall))

#define __MSVCRT__ 1
#undef _MSVCRT_
#define __MINGW_IMPORT extern __declspec(dllimport)
#define __MINGW_ATTRIB_NORETURN
#define __MINGW_ATTRIB_CONST
#define __MINGW_ATTRIB_DEPRECATED
#define __MINGW_ATTRIB_MALLOC
#define __MINGW_ATTRIB_PURE
#define __MINGW_ATTRIB_NONNULL(arg)
#define __MINGW_NOTHROW
#define __GNUC_VA_LIST

#define _CRTIMP extern
#define __CRT_INLINE extern __inline__

#define _CRT_ALIGN(x) __attribute__((aligned(x)))
#define DECLSPEC_ALIGN(x) __attribute__((aligned(x)))
#define _CRT_PACKING 8
#define __CRT_UNALIGNED
#define _CONST_RETURN

#ifndef _TRUNCATE
#define _TRUNCATE ((size_t)-1)
#endif

#define __CRT_STRINGIZE(_Value) #_Value
#define _CRT_STRINGIZE(_Value) __CRT_STRINGIZE(_Value)
#define __CRT_WIDE(_String) L ## _String
#define _CRT_WIDE(_String) __CRT_WIDE(_String)

#ifdef _WIN64
#define __stdcall
#define _AMD64_ 1
#define __x86_64 1
#define _M_X64 100 /* Visual Studio */
#define _M_AMD64 100 /* Visual Studio */
#define USE_MINGW_SETJMP_TWO_ARGS
#define mingw_getsp tinyc_getbp
#define __TRY__
#else
#define __stdcall __attribute__((__stdcall__))
#define _X86_ 1
#define _M_IX86 300 /* Visual Studio */
#define WIN32 1
#define _USE_32BIT_TIME_T
#ifdef __arm__
#define __TRY__
#else
#define __TRY__ void __try__(void**), *_sehrec[6]; __try__(_sehrec);
#endif
#endif

/* in stddef.h */
#define _SIZE_T_DEFINED
#define _SSIZE_T_DEFINED
#define _PTRDIFF_T_DEFINED
#define _WCHAR_T_DEFINED
#define _UINTPTR_T_DEFINED
#define _INTPTR_T_DEFINED
#define _INTEGRAL_MAX_BITS 64

#ifndef _TIME32_T_DEFINED
#define _TIME32_T_DEFINED
typedef long __time32_t;
#endif

#ifndef _TIME64_T_DEFINED
#define _TIME64_T_DEFINED
typedef long long __time64_t;
#endif

#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
#ifdef _USE_32BIT_TIME_T
typedef __time32_t time_t;
#else
typedef __time64_t time_t;
#endif
#endif

#ifndef _WCTYPE_T_DEFINED
#define _WCTYPE_T_DEFINED
typedef wchar_t wctype_t;
#endif

#ifndef _WINT_T
#define _WINT_T
typedef short wint_t;
#endif

typedef int errno_t;
#define _ERRCODE_DEFINED

typedef struct threadlocaleinfostruct *pthreadlocinfo;
typedef struct threadmbcinfostruct *pthreadmbcinfo;
typedef struct localeinfo_struct _locale_tstruct,*_locale_t;

/* for winapi */
#define _ANONYMOUS_UNION
#define _ANONYMOUS_STRUCT
#define DECLSPEC_NORETURN
#define DECLARE_STDCALL_P(type) __stdcall type
#define NOSERVICE 1
#define NOMCX 1
#define NOIME 1
#define __INTRIN_H_
#ifndef DUMMYUNIONNAME
#  define DUMMYUNIONNAME
#  define DUMMYUNIONNAME1
#  define DUMMYUNIONNAME2
#  define DUMMYUNIONNAME3
#  define DUMMYUNIONNAME4
#  define DUMMYUNIONNAME5
#endif
#ifndef DUMMYSTRUCTNAME
#  define DUMMYSTRUCTNAME
#endif
#ifndef WINVER
# define WINVER 0x0502
#endif
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x502
#endif

#define __C89_NAMELESS
#define __MINGW_EXTENSION
#define WINAPI_FAMILY_PARTITION(X) 1
#define MINGW_HAS_SECURE_API

#endif /* __MINGW_H */
=== windowsx.h 71/73 ===
/* Header windowsx.h */

#ifndef $windowsx
#define $windowsx 1

#define GET_X_LPARAM(lp)                        ((int)(short)LOWORD(lp))
#define GET_Y_LPARAM(lp)                        ((int)(short)HIWORD(lp))

#endif // windowsx

=== help.txt 72/73 ===
C Subset Compiler for 64-bit Windows

Normal use:

    bcc prog            Compile file prog.c to prog.exe
    bcc prog.c          Same (extension is optional)
    bcc a b c d.dll     Compile a.c, b.c, c.c and link with d.dll to a.exe

Options:

    -e              Write preprocessed output to prog.i
    -s              Compile only, to .asm
    -c              Same as -obj
    -exe            (DEFAULT) compile and link to .exe file

    -i:path         Add include path
    -ext            Don't use internal standard headers
    -old            Allow features such as () parameter lists, for old programs
    -out:file       Name exe file

    -obj            Link to single .obj file rather .exe
    -run            Link to .exe then run that program
    -auto           Locate .c files matched to headers and add to modules
    @file           Read further files and options from a file

Other Options:

    -info           Show further information
    -time           Show compiler timing stats
    -writeheaders   Write out internal headers as .hdr (not .h) files
    -at             Create an @ file of filenames suitable for most compilers
    -stdin          Read C file from console
    -stdout         Write preprocessor output to console, rather than .i file
=== info.txt 73/73 ===
    The 'BCC' C Compiler. Included in the one executable file:

       * A compiler that produces .asm (external or internal)
       * A minimal set of standard headers
       * A very minimal windows.h
       * An assembler/linker generating one .exe or .obj file
       * The bcclib.asm file (written out as needed)

    BCC only targets x64 with Win64 call convention. It will compile
    single or multiple .c files to one .asm, .exe or .obj file:

      -e    Preprocess each module to .i file
      -s    Compile all modules to one .asm file (NOT multiple)
      -exe  (DEFAULT) Compile all modules to one .exe file
      -obj  Compile all modules to one .obj file (NOT multiple)
      -c    Same as -obj
      -run  Compile to .exe then run the new program. Provide
            params after " : " (spaces needed)

    When there is one output file, it will be named based on the first
    input file. Otherwise use -out option (see bcc -help).

    .obj files can be linked using gcc on Windows. This option is
    needed to be able to generate .dll files.

    Libraries msvcrt.dll, gdi32.dll, user32.dll and kernel32.dll are
    automatically included as search libraries for imported functions.

    For other libraries, add .dll files to bcc command line or @ list.
    (Note that when any .dll is specified, the default set of DLLs
    is no longer included. You may need to explicitly specify msvcrt.dll etc.)
    Other kinds of binary libraries (.a, .lib etc) are not supported.

    (Programs using setjmp and certain internal ops will need bcclib.asm.
    This file is automatically written by bcc if not present, and
    automatically linked.)

    Omissions, Restrictsions and Bugs (highlights only as there are dozens):

       * No VLAs, compound literals, designated initialisers
       * Restrictions on complexity of data initialisers
       * Callback functions are buggy if called from external code (not
         compiled with bcc). Fix by adding $callback attribute to such
         functions, or the portable #pragma $callback just before.
         Typically, functions passed to qsort().
=== end ===
