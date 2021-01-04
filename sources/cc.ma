mafile 31
  1 cc.m               16894     1363   0
  2 help.txt            1308    18280   1
  3 info.txt            2162    19611   1
  4 msyslib.m          34947    21797   0
  5 mclib.m             3432    56766   0
  6 mlibnew.m          26089    60222   0
  7 mwindows.m         12837    86336   0
  8 cc_decls.m          9443    99198   0
  9 cc_tables.m        25878   108667   0
 10 cc_support.m        6191   134573   0
 11 cc_lex.m           72719   140788   0
 12 cc_headersx.m        669   213536   0
 13 bcclib.asm          2765   214231   1
 14 cc_lib.m           36810   217020   0
 15 cc_parse.m         83267   253856   0
 16 cc_genmcl.m        12759   337150   0
 17 cc_libmcl.m        34434   349936   0
 18 cc_blockmcl.m      61916   384399   0
 19 cc_genasm.m        28525   446342   0
 20 cc_export.m         5991   474894   0
 21 cc_assembler.m      6190   480915   0
 22 ax_tables.m        12416   487132   0
 23 ax_decls.m          4820   499574   0
 24 ax_lex.m           12713   504418   0
 25 ax_parse.m          8966   517157   0
 26 ax_lib.m           16788   526147   0
 27 ax_genss.m         40826   542961   0
 28 ax_objdecls.m       4301   583816   0
 29 ax_writeexe.m      25253   588146   0
 30 ax_disasm.m        26792   613426   0
 31 ax_writeobj.m       6997   640247   0
=== cc.m 1/31 ===
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
import cc_genmcl
import cc_libmcl
import cc_genasm
import cc_export

import cc_assembler
import CC_BLOCKMCL

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
	(splicing_sw,	"splicing"),
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

printmodelist(logdev)


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
	[100]char modulename
	[300]char path
	int status
	modulerec m
	int i,flag,fileno

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
if dointheaders=0 then
	searchdirs[++nsearchdirs]:="/cx/headers/"
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
	println @logdev,"PROC",caption
	printstflat(logdev)
	println @logdev
fi
end

proc showsttree(ichar caption,int n)=		!SHOWSTTREE
if logdest then
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
elsif eqstring(destfileext,"") then
	destfileext:=ext
fi

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

when splicing_sw then
	flinesplicing:=1

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
static ichar infotext=strinclude "info.txt"

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
freehashtable()

ntypes:=ntypesreset
nstrings:=nreals:=ndints:=0
stprogram:=stmodule:=nil
currblockno:=nextblockno:=blocklevel:=0
autotypeno:=0
nextafindex:=0
labelno:=0

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
=== help.txt 2/31 ===
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
=== info.txt 3/31 ===
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
=== msyslib.m 4/31 ===
import clib
import mlib

global record procinforec=
	word16		fnindex
	byte		rettype
	byte		nparams
	[12]byte	paramlist
end

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

const rd_buffersize = 16384	!total capacity of line buffer

ref char rd_buffer		! point to start of read buffer
int rd_length			! length of this line (as read by readln)
ref char rd_pos			! current position it's up to (next read starts here)
ref char rd_lastpos		! set by sread() just before reading used for reread()
int termchar			! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals


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

m$print_startcon()		!allow most print stmts without startcon/end

end

global proc m$stop(int n)=
	`exit(n)
end

global function m$lenstr_stringz(ref char s)int=
	strlen(s)
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
	[20]char s

	if fmtstyle=nil then
		fmtstyle:="z8H"
	fi
	m$print_u64(a,fmtstyle)
end

global proc m$print_ptr_nf(u64 a)=
	m$print_ptr(a)
end

global proc m$print_i64(int64 a,ichar fmtstyle=nil)=
	[40]char s
	fmtrec fmt
	int n

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
			needgap:=0
		else
			tostr_i64(a,&fmt)
		fi
	fi
	needgap:=1
end

global proc m$print_i64_nf(int64 a)=
	m$print_i64(a)
end

global proc m$print_u64(word64 a,ichar fmtstyle=nil)=
	[40]char s
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
	[40]char s
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
	[40]char s
	fmtrec fmt

	nextfmtchars()
	strtofmt(fmtstyle,-1,&fmt)
	tostr_u128(a,&fmt,0)
	needgap:=1
end

global proc m$print_r64(real x,ichar fmtstyle=nil)=
	[360]char s
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
	[40]char s
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

	if s=nil then
		printstr("<null>")
		return
	fi


	fmtrec fmt
	if fmtstyle=nil then
		printstr(s)
	else
		strtofmt(fmtstyle,-1,&fmt)
		tostr_str(s,&fmt)
	fi
	needgap:=1
end

global proc m$print_str_nf(ichar s)=
	m$print_str(s)
end

global proc m$print_strsl(slice[]char s, ichar fmtstyle=nil)=
	nextfmtchars()
	fmtrec fmt
	if fmtstyle=nil then
		printstr_n(cast(s.sliceptr),s.len)
	else
		abortprogram("FORMATED PRINT SLICE NOT READY")
	fi
	needgap:=1
end

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
		p^^:=0
	esac
end

global proc printstr_n(ichar s,int n=-1)=
	ref ref char p

	case n
	when -1 then n:=strlen(s)
	when 0 then return
	esac

	case outdev
	when std_io then
		printf("%.*s",n,s)
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	when str_io then
		p:=cast(outchan)
		memcpy(p^,s,n)
		p^+:=n
		p^^:=0
	esac
end

global proc printstrn_app(ichar s, int length, filehandle f=nil)=
if length then
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
		fi
		needgap:=0
		return
	fi

	pstart:=fmtstr
	n:=0

	do
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

	int c
	byte wset
	int n
	[0:100]char str

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
	[0:20]char str
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

	int i,w,m

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


function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
	[0:onesixty]char t
	u64 dd
	int i,j,k,g
	int cc
	int dummy
	ref char s0

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		dd:=aa rem base
		aa:=aa/base

		t[++i]:=digits[dd]

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
	[0:160]char t
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
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w,usigned
	const i64 mindint=0x8000'0000'0000'0000

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

	if (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

	return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
	fi

	return expandstr(&.str,s,n,fmt)
end

function u128tostrfmt(i128 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u128tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

	return expandstr(&.str,s,n,fmt)
end

function i64mintostr(ref char s,int base,int sep)int =		!I64MINTOSTR
	[0:onesixty]char t
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
	ref char u,v
	[256]char str
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
	[360]char str
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
	[360]char str
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
	[360]char str
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
	[360]char str,str2
	[0:10]char cfmt
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
	ref char p,s,itemstr
	char quotechar, c

	unless rd_buffer then 
		initreadbuffer()
	end unless


	s:=rd_pos

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

global function strtoint(ichar s,int length=-1, word base=10)int64=
	byte signd
	word64 aa
	word c,d

	itemerror:=0

	if length=-1 then
		length:=strlen(s)
	fi
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
	[512]char str
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

global proc m$read_str(ref char dest, int destlen=0,fmt=0)=
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

global proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_str(dest,destlen,fmt)
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

global proc m$float_u64_r64(word a)=
	assem
		cmp D10,0
		jl fl1
		cvtsi2sd XMM0,D10
		jmp flx
fl1:						!negative value
		and D10,[mask63]		!clear top bit (subtract 2**63)
		cvtsi2sd XMM0,D10
		addsd XMM0,[offset64]	!(add 2**63 back to result)
flx:
	end
end

global function m$power_i64(int64 a,n)int64=
	if n<0 then
		return 0
	elsif n=0 then
		return 1
	elsif n=1 then
		return a
	elsif n.even then
		return m$power_i64(sqr a, n/2)
	else			!assume odd
		return m$power_i64(sqr a, (n-1)/2)*a
	fi
end


global proc m$mul_i128(word128 aa,bb)=
	assem
		push d3
		push d4
		push d5
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
		pop d5
		pop d4
		pop d3
	end
end

global proc m$idiv_i128(word128 aa,bb)=
charlie::
	assem
		push d3
		push d4
		push d6


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
		pop d6
		pop d4
		pop d3

	end
	return

asm divbyzero:
CPL "DIV BY ZERO"
	stop 1
end

global proc m$dotindex(word i,a)=
end

global proc m$dotslice(word j,i,a)=
end

global proc m$popdotindex(word i,ref word p,word x)=
end

global proc m$popdotslice(word j,i, ref word p, word x)=
end



global proc mclunimpl(ichar mess)=
	printf("MCL-UNIMPL: %s\n",mess)
	stop 1
end
=== mclib.m 5/31 ===
global type filehandle=ref void

importlib $cstd=
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

	clang function strlen	(ichar)int
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
	clang function puts99	(ichar)int32
	clang function printf	(ichar, ...)int32

	clang function sprintf	(ichar, ichar, ...)int32

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
=== mlibnew.m 6/31 ===
import msys
import clib
import oslib

const mem_check=0

GLOBAL INT MDEBUG
GLOBAL INT NPCMALLOC


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

const int maxmemalloc=(mem_check|500000|2)
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

[2]word seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

global function pcm_alloc(int n)ref void =		!PCM_ALLOC
ref byte p

if not pcm_setup then
	pcm_init()
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

allocbytes:=allocupper[alloccode]

if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
	freelist[alloccode]:=ref wordp(int((freelist[alloccode])^))

	return p
fi

p:=pcheapptr				!Create item at start of remaining pool in heap block
pcheapptr+:=allocbytes			!Shrink remaining pool

if pcheapptr>=pcheapend then		!Overflows?
	p:=pcm_newblock(allocbytes)		!Create new heap block, and allocate from start of that
	return p
fi
if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

return p
end

global proc pcm_free(ref void p,int n) =		!PCM_FREE
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

	cast(p,ref wordp)^:=wordp(int(freelist[acode]))
	freelist[acode]:=p
fi
end

global proc pcm_freeac(ref void p,int alloc) =		!PCM_FREEAC
pcm_free(p,allocupper[alloc])
end

global proc pcm_copymem4(ref void p,q,int n) =	!PCM_COPYMEM4

memcpy(p,q,n)
end

global proc pcm_clearmem(ref void p,int n) =		!PCM_CLEARMEM
memset(p,0,n)
end

global proc pcm_init =		!PCM_INIT
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


if size<=maxblocksize then
	return sizeindextable[size]		!size 0 to 2KB
fi

size:=(size+255)>>8					!scale by 256


if size<=maxblocksize then
	return sizeindextable[size]+8
fi

size:=(size+63)>>6					!scale by 256

if size<=maxblocksize then
	return sizeindextable[size]+14
fi



size:=(size-2048+2047)/2048+22
return size
end

global function pcm_newblock(int itemsize)ref void=
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
static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

if n>maxblocksize then
	return n
else
	return allocbytes[sizeindextable[n]]
fi
end

global function pcm_array(int n)int =		!PCM_ARRAY
int m

if n<=maxblocksize then	!automatic rounding up used for small heap
	return pcm_round(n)
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
	ref char q
	if length=0 then return nil fi

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

proc addtomemalloc(ref int32 ptr,int size)=


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


for i to maxmemalloc do
	if memalloctable[i]=ptr then

if memallocsize[i]<>size then
	CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
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
int ch
ref char p
int n
[0:100]char buff
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
static [260]char newfile
[32]char newext2
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
ref char t,u

t:=extractfile(s)

if t^=0 then			!s contains no filename
	return ""
fi

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
ref char sext

sext:=extractext(s,1)

if sext^=0 then						!no extension not even "."
	return changeext(s,newext)
fi

return s							!has own extension; use that
end

global function alloctable(int n, size)ref void =		!ALLOCTABLE
ref void p

p:=malloc((n+1)*size)

if not p then
	abortprogram("Alloctable failure")
fi
return p
end

global function zalloctable(int n, size)ref void =		!ALLOCTABLE
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
		fi
		q:=p
		p:=ref wordp(int(p^))
	od

od
end


global function pcm_alloc32:ref void =		!PCM_ALLOC
ref byte p

allocbytes:=32


return pcm_alloc(32)
end

global proc pcm_free32(ref void p) =

smallmemtotal-:=32
if mem_check then removefrommemalloc(p,32) fi

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

allocbytes:=allocupper[alloccode:=sizeindextable[n]]

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

global function pcm_smallalloc(int n)ref void =
ref byte p

allocbytes:=allocupper[alloccode:=sizeindextable[n]]

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
[16]char s

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
[2560]char str
col:=dest^.length
strcpy(&.str,s)
slen:=strlen(s)
n:=w-slen
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
[2560]char str

n:=col-dest^.length
if n<=0 then return fi
for i:=1 to n do
	str[i]:=ch
od
str[n+1]:=0
gs_str(dest,&.str)
end

global proc gs_println(ref strbuffer dest,filehandle f=nil)=
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
	word64 x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

global function mrandomp:int =
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

global function mrandomint(int n)int=
	return mrandomp() rem n
end

global function mrandomrange(int a,b)int=
	int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

global function mrandomreal:real x=
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

global function mrandomreal1:real=
	return mrandomp()/9223372036854775807
end

global function checkpackfile:ref byte=

int a,offset,i,size
[100]char name
[300]char exefile
ref byte packexeptr			!for embedded pack files, contains pointer to in-memory version of this .exe file plus extras; else nil
int packexesize				!byte size
ref char packfilename
int packfilesize
ref byte packfileptr

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

global function pcm_allocx:ref void =
const n=32
ref word p

allocbytes:=32

if p:=ref word(freelist[2]) then		!Items of this block size available
	freelist[2]:=ref wordp(int((freelist[2])^))

else

	p:=cast(pcheapptr)				!Create item at start of remaining pool in heap block
	pcheapptr+:=32			!Shrink remaining pool

	if pcheapptr>=pcheapend then		!Overflows?
		p:=pcm_newblock(32)		!Create new heap block, and allocate from start of that
	fi

	p^:=0
	(p+1)^:=0
	(p+2)^:=0
	(p+3)^:=0

	return p
fi
end

=== mwindows.m 7/31 ===
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
	windows function "GetProcAddress"(wt_handle,wt_ichar)ref void
	windows function "LoadCursorA"(wt_handle,wt_ichar)wt_handle
	windows function "RegisterClassExA"(wt_ptr)wt_wordpm
	windows function "DefWindowProcA"(wt_handle,wt_uint,wt_wparam,wt_lparam)intm
	windows function "ReadConsoleInputA"(wt_handle,wt_ptr,wt_dword,wt_ptr)int
	windows proc     "Sleep"(wt_dword)
	windows function "GetModuleFileNameA"(wt_handle,wt_ichar,wt_dword)wt_dword

	windows proc     "ExitProcess"(wt_uint)
	windows proc	 "PostQuitMessage"(wt_int)


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
	word32 dummy1
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
	word32 dummy2
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
	wt_handle	background
	wt_ichar	menuname
	wt_ichar	classname
	wt_handle	iconsm
end

global record rmsg =
	wt_handle	hwnd
	wt_uint		message
	word32		dummy1
	wt_wparam	wParam
	wt_lparam	lParam
	wt_dword	time
	word32		dummy2
	wt_point	pt
end

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

hconsole:=GetStdHandle(u32(-11))
hconsolein:=GetStdHandle(u32(-10))

lastkey.repeatcount:=0
keypending:=0

SetConsoleCtrlHandler(nil,1)

SetConsoleMode(hconsole,1 ior 2)

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

status:=CreateProcessA(
	nil,
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

unless init_flag then os_init() end

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

global function os_getdllprocaddr(int hinst,ichar name)ref void=

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


memset(&r,0,r.bytes)
r.size:=r.bytes
r.style:=8 ior 32		!CS_DBLCLKS | CS_OWNDC
r.wndproc:=cast(&mainwndproc)
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

global callback function mainwndproc (
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)intm=
rmsg m
int i,result
intm l
static int count=0


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

proc timerproc(wt_handle hwnd, int msg, id, time)=
println "TIMERPROC"
end

global proc os_setmesshandler(ref void addr)=
wndproc_callbackfn:=addr
end

global function os_getchx:int=
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
[100]byte m
	ticks:=GetTickCount()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end
=== cc_decls.m 8/31 ===
import clib
import cc_tables

global type unit = ref unitrec

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


global record paramrec =
	ref strec def			!named param: st entry, otherwise nil
	ref paramrec nextparam
	int32 mode				!tnone when there are no normal params
	int16 nparams			!used on first param only
	int16 flags				!used on first param only
end

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
global const int maxtype=20'000

global int ntypes
global int ntypesreset

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
global int logdest=0
global int optflag=0			!1=stdoptimise; 0=disabled

global const sourceext="c"
global ref unitrec nullunit

global int fverbose=0		!whether to display message for each pass
global int fquiet=0
global int fshownames=0		!whether [dframe-8] or [dframe+a]
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
global int flinesplicing=0	!1 to deal with \ line continuations

global tokenrec lx				!provides access to current token data
global tokenrec nextlx


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

global int labelno=0
global const maxnestedloops=100

global int dointheaders=1				!allow internal std headers
global ichar dheaderfile=nil			!result of -d:file.h switch

global int structpadding=1
global int callbackflag=0

global int slineno,sfileno

global ichar oemname="BCC"

GLOBAL INT NLOOKUPS
GLOBAL INT NCLASHES
=== cc_tables.m 9/31 ===
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

	(tlast,			$,			0,	0,	0,		0,		"")		! 	!

end

global const tfirstnum=tschar, tlastnum=tldouble
global const tfirstint=tschar, tlastint=tullong
global const tfirstreal=tfloat, tlastreal=tldouble

global const tptroffset = tsllong		!for 64-bit target

global tabledata() []ichar typespecnames, []int32 typespectypes, []byte typespecsizes =
	(ts_void,		$,	tvoid,		0),
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

global tabledata() [0:]ichar scopenames=
	(no_scope=0,		"-"),		! 
	(function_scope,	"Fn"),		!within a function (note import/exported names can be declared in a block scope)
	(local_scope,		"Loc"),		!file-scope/not exported 
	(imported_scope,	"Imp"),		!imported from another module
	(exported_scope,	"Exp")		!file-scope/exported
end

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
	(j_widenmem,	$), !
	(j_funcname,	$), !
	(j_block,		$), !
	(j_tempdecl,	$), !
	(j_decl,		$), !


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



	(j_andl,		"&& andl"), ! 
	(j_orl,			"|| orl"), ! 
	(j_notl,		"! notl"), ! 
	(j_istruel,		$), ! 


	(j_makelist,	$), ! 
	(j_exprlist,	$), ! 

	(j_callfn,		$), ! 
	(j_ifx,			$), ! 


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
	(j_index,		$), ! 

	(j_ptr,			"ptr"), ! 
	(j_addptr,		"addptr"), ! 
	(j_subptr,		"subptr"), ! 
	(j_addrof,		"addrof &"), ! 
	(j_convert,		$), ! 
	(j_scale,		$), ! 


	(j_neg,			"- neg"), ! 
	(j_abs,			"abs"), ! 
	(j_inot,		"~ inot"), ! a


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
	(assignsym2,		$,	":=",	j_assign),	! =
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
	(placeholdersym,	$,	"",	0),			!
	(kstrincludesym,	$,	"k",	0),			!

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
	("__TIME__",	predefmacrosym,	pdm_time),
	("__BCC__",		predefmacrosym,	pdm_bcc),
	("__func__",	predefmacrosym,	pdm_func),
	("__FUNCTION__",	predefmacrosym,	pdm_func),

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


global [0:32,0:32]byte dominantmode

global [0:16,0:16]byte conversionops

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

	(tsllong,	tsshort,	truncate_c),
	(tsllong,	tsint,		truncate_c),
	(tsllong,	tsllong,	no_conv),
	(tsllong,	tbool,		bool_c),

	(tsllong,	tuchar,		truncate_c),

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

=== cc_support.m 10/31 ===
import clib
import mlib
import oslib

import cc_decls
import cc_tables


global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

proc stopcompiler(ichar filename,int lineno)=
	filehandle f

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
return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =		!SETELEM
p^[n>>3] ior:= bytemasks[n iand 7]
end

global function nextpoweroftwo(int x)int=

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
p^:=0	

sourcefiletext[nsourcefiles]:=pcm_copyheapstring(&.src)
sourcefilesizes[nsourcefiles]:=strlen(&.src)
return nsourcefiles
end

global function loadsourcefile(ichar file,shortfile)int=
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

if flinesplicing then
	s:=splicelines(s)
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
ichar s

if nsourcefiles>maxsourcefile then
	loaderror("Too many source files")
fi
++nsourcefiles
sourcefilepaths[nsourcefiles]:="<builtin>"
sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)

sourcefiletext[nsourcefiles]:=pcm_copyheapstring(hdrtext)

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
=== cc_lex.m 11/31 ===

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
	word c,csum,hsum,dodir
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
		endswitch

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

	when 0xEF then			!BOM
		lxsptr+:=2

	else
		!codes above 127 can be names; later, decode the actual unicode from
		!the utf8 sequence, and check for correct ranges of chars that are allowed
		if 128<=(lxsptr-1)^<= 255then goto doname fi

		PRINTLN "ERROR CHAR",(lxsptr-1)^,int((lxsptr-1)^),lx_stackindex
		lxerror("ERROR CHAR")
		nextlx.symbol:=errorsym
		return

	end doswitch

end

proc readrealnumber(ref char pstart,intstart, int intlen, base)=
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
	int i

	inithashtable()
	fillhashtable()

	for i:=0 to 255 do
		switch i
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

global function gethashvalue(ichar s,int length=-1)word=
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
	int i

	for i:=1 to stnames.len do
		lxsvalue:=stnames[i]


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
		print "#MESSAGE "
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
	word64 aa
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
	char c

	doswitch c:=lxsptr++^
	when 'L','l','u','U' then
	else
		if alphamap[c] then
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

	static [300]char filespec
	[300]char filespec2
	ichar hdrtext
	int i

	headerpath[1]:=0

	strcpy(&.filespec,file)
	convlcstring(&.filespec)

	for i:=1 to nsourcefiles do
		if eqstring(&.filespec,sourcefilenames[i]) then
			return i
		fi
	od

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
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx^^.nexttoken:=p
	fi
	p^.nexttoken:=nil

	ulistx^:=p			!update end-of-list pointer
end

proc addlisttoken_copy(ref ref tokenrec ulist,ulistx,ref tokenrec q)=
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

	stname^.oldsymbol:=stname^.symbol

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
	while alphamap[lxsptr++^] do od
	--lxsptr
	nextlx.svalue:=pstart
	nextlx.symbol:=rawnumbersym
	nextlx.length:=lxsptr-pstart
end

function inmacrostack(ref strec d, ref tokenrec macrostack)int=

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
	ref strec d
	static int doreset=0
	int newlineno

	do
		if tkptr then
			nextlx:=tkptr^
			tkptr:=tkptr^.nexttoken
			if tkptr=nil then

				if nextlx.symbol=namesym and nextlx.symptr^.nameid=macroid and peeklb() then
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
				expandpredefmacro(d^.subcode,&nextlx,slineno)
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
	if lxsptr^='(' or (lxsptr^=' ' and (lxsptr+1)^='(') then
		return 1
	fi
	return 0
end

function peektk(ref tokenrec tk)int=
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
				expandpredefmacro(m^.subcode,expandtk,slineno)
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
			if seq^.nexttoken and seq^.nexttoken^.symbol=hashhashsym or
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
			println i,":",d^.name
		else
			println i,": ----"
		fi
	od
	println
end

proc newhashtable=
	ref[0:]ref strec oldhashtable
	int oldhstsize
	ref strec d

	oldhashtable:=hashtable
	oldhstsize:=hstsize
	hstsize*:=2
	hstmask:=hstsize-1
	nhstsymbols:=0
	hstthreshold:=(6*hstsize)/10

	hashtable:=pcm_alloc(hstsize*(ref void.bytes))

	for i:=0 to hstmask do
		hashtable^[i]:=pcm_allocz(strec.bytes)
	od

	for i:=0 to oldhstsize-1 do
		d:=oldhashtable^[i]
		if d^.name then
			regenlookup(d)
		fi
	od

	pcm_free(oldhashtable,oldhstsize*(ref void.bytes))
end

proc old_readrealnumber(ref char pstart,intstart, int intlen, base)=
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
=== cc_headersx.m 12/31 ===

import clib
import mlib

ichar bcclibstr = strinclude "bcclib.asm"

global function findheader(ichar name)ichar=
return nil
end

global proc writeheaders=
filehandle f
ichar ifile
int i
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
return 0
end
=== bcclib.asm 13/31 ===
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


=== cc_lib.m 14/31 ===
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
	fprint @&.str,"Pm:# ",attrs.ax_nparams

	gs_str(d,&.str)
fi
if attrs.ax_moduleno then
	fprint @&.str,"M# ",attrs.ax_moduleno
	gs_str(d,&.str)
fi
if attrs.ax_equals then
	gs_str(d,"= ")
fi
gs_str(d,"]")
gs_padto(d,col+10,'=')

if p^.owner then
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

		gs_strvar(d,strexpr(p^.code))
	fi
	gs_str(d," Offset: ")
	gs_strint(d,p^.offset)

when procid then

	gs_str(d,"Index:")
	gs_strint(d,p^.index)
	gs_str(d," Address:")
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
		gs_str(d,pmflagnames[pm^.flags])

		gs_line(d)
		pm:=pm^.nextparam
	od
fi

gs_str(d," MODE:")
gs_strint(d,p.mode)

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
	p:=hashtable^[i]
	if p^.name then
		case p^.symbol
		when namesym then
			println @f,i,p,":",getstname(p),symbolnames[p^.symbol],namenames[p^.nameid]
			p:=p^.nextdupl
			while p do
				print   @f,"	",p,getstname(p),symbolnames[p^.symbol],namenames[p^.nameid],
					p^.prevdupl
				println @f,"(From",(p^.owner|getstname(p^.owner)|"-"),,")"
				p:=p^.nextdupl
			od
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
	u^.wslength:=length
u^.iswstrconst:=1
u^.simple:=1
return u
end

global function getoptocode(int opc)int=		!GETOPTOCODE
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

global function getconstvalue(ref unitrec p,int ID=0)int64=	!GETCONSTVALUE
if p and p^.tag=j_const then
	return p^.value
fi
serror("GCV Not constant")
return 0
end

global function nextautotype:ichar=
static [32]char str

print @&.str,"$T",,++autotypeno
return &.str
end

global function createconstmode(int m)int=
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
int newm


if ttreftype[m] then

 return ttreftype[m] fi
newm:=createnewmode(tref)
ttreftype[m]:=newm
tttarget[newm]:=m
ttisref[newm]:=1
return newm
end

global function createprocmode(int m, ref paramrec pm)int=
int newm

newm:=createnewmode(tproc)
ttparams[newm]:=pm
tttarget[newm]:=m
return newm
end

global function createarraymode(int m, length)int=
int newm


IF NTYPES>10000 THEN CPL =NTYPES FI

for i to ntypes do
	if ttbasetype[i]=tarray and tttarget[i]=m and ttlength[i]=length then
		return i
	fi
od


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


p:=moduletable[n].stmodule^.deflist

println @f, caption, "MODULE:",moduletable[n].name

while p do
	case p^.nameid
	when procid then
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
	print @dev,"             "
fi


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

when j_dot then
	print @dev," Offset:",p^.offset

esac


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
static [512]char str
[512]char indentstr
ichar modestr
int length

indentstr[1]:=0
if level>10 then level:=10 fi

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
	print @&.str2,".",,p.blockno
	strcat(&.str,&.str2)
fi
return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
static [40]char str

fprint @&.str,"# ",currlineno:"z5"
return &.str
end

global function getautofieldname:ref strec=
[32]char str
ichar name

print @&.str,"$F",,++nextafindex

name:=pcm_copyheapstring(&.str)
return addnamestr(name)
end

global proc convertstring(ichar s, t,int length=-1)=		!CONVERTSTRING
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
gs_init(exprstr)

jeval(exprstr,p)
return exprstr
end

proc jeval(ref strbuffer dest, ref unitrec p)=			!JEVAL
ref unitrec q
[16000]char str
int lb,t


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
		getstrint(p.value, &.str)

	elsif t>=tuchar and t<=tullong then
		strcpy(&.str,strword(p.uvalue))

	elsif t=tdouble or t=tfloat then
		strcpy(&.str,strreal(p.xvalue))
	else
		case ttbasetype[p^.mode]
		when tref then
doref::
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
ref strec q

case p^.nameid
when fieldid then			!needed for genfieldtables
	return
esac

purgesymbollist(p^.deflist,0,del)

if prev then
	prev^.nextdef:=p^.nextdef
else
	p^.owner^.deflist:=p^.nextdef
fi


q:=p^.prevdupl
q^.nextdupl:=p^.nextdupl

if del then
	pcm_free(p,strec.bytes)
fi
end

global proc purgesymbollist(ref strec p,int ismodule, del)=

ref strec q,prev

serror("PURGESYMBOL")

end

global proc purgeprocs(ref strec p, int del)=

while p do
	if p^.nameid=procid then
		purgeproc(p,del)
	fi
	p:=p^.nextdef
od
end

global proc purgeproc(ref strec p, int del)=
ref strec q,prev,r


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


if ntypes>=maxtype then
CPL =STRMODE(M)
	serror("Too many types/cnm")
fi
++ntypes


ttbasetype[ntypes]:=m
ttsize[ntypes]:=ttsize[m]
ttbitwidth[ntypes]:=ttbitwidth[m]

return ntypes
end

global proc addlistunit(ref ref unitrec ulist,ulistx,ref unitrec p)=
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextunit:=p
fi
p^.nextunit:=nil

ulistx^:=p			!update end-of-list pointer
end

global proc addlistdef(ref ref strec ulist,ulistx,ref strec p)=
if ulist^=nil then		!first
	ulist^:=ulistx^:=p
else
	ulistx^^.nextdef:=p
fi
p^.nextdef:=nil

ulistx^:=p			!update end-of-list pointer
end

global proc addlistparam(ref ref paramrec ulist,ulistx,ref paramrec p)=
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


for i:=0 to tlast-1 do
	ttbasetype[i]:=i

	bitsize:=stdtypewidths[i]
	size:=bitsize/8

	ttsize[i]:=size
	ttbitwidth[i]:=bitsize

od
ntypes:=tlast-1


trefchar:=createrefmode(tschar)
trefwchar:=createrefmode(tsshort)

for i:=1 to dominantsetuptable.len do
	s:=dominantsetuptable[i,1]
	t:=dominantsetuptable[i,2]
	u:=dominantsetuptable[i,3]
	dominantmode[s,t]:=u
od

for i:=1 to convsetuptable.len do
	s:=convsetuptable[i,1]
	t:=convsetuptable[i,2]
	u:=convsetuptable[i,3]
	conversionops[s,t]:=u
od
ntypesreset:=ntypes
end

global function createdupldef(ref strec owner,symptr, int id)ref strec=
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
ref strec p,q

p:=createdupldef(owner,symptr,procid)

q:=p
while q:=q^.nextdupl do
	if q^.owner=owner then
		cpl q^.name,"in",owner^.name
		serror("Dupl proc name")
	fi
od

return p
end

global function resolvename(ref strec owner, symptr, int ns, blockno)ref strec=
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
int a

case ttbasetype[m]
when tarray then
	return getalignment(tttarget[m])
when tstruct,tunion then
	a:=ttnamedef[m]^.attribs.ax_align
	if a=0 then
		RETURN 16
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
=== cc_parse.m 15/31 ===

import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables

import cc_lex
import cc_lib

const needcompoundblock=0

ref strec ist_symptr

INT INSIDEFOR
INT INTYPEOF

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


readmodule()

endlex()
return 1
end

function readdeclspec(ref strec owner,int &linkage)int=

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
	esac
	lex()
when kstructsym,kunionsym then
	if d.typeno then serror("struct?") fi
	d.typeno:=readstructdecl(owner)
	d.isusertype:=1
	fstruct:=1

when kenumsym then
	if d.typeno then serror("enum?") fi
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
	intypeof:=1
	p:=readterm()
	intypeof:=0
	skipsymbol(rbracksym)
	if d.typeno or mod then serror("typeof") fi
	d.typeno:=p.def.mode

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
	when tschar then
		if d.isshort or d.islong or d.isllong then serror("char decl?") fi
		t:=(d.isunsigned|tuchar|tschar)
	when tdouble then
		if d.isshort or d.isllong or d.issigned or d.isunsigned then serror("dbl decl?") fi
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
ref strec d

switch lx.symbol
when ktypespecsym then
	return 1
when ktypequalsym then
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
	elsif s=tunion and t=tunion then
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
		when tsint,tsllong,tuint,tullong then
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
		when tsint,tsllong,tuint,tullong then
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
		else
			goto doname
		fi

	else
doname::
		p:=createname(d)
		p^.mode:=t:=d^.mode
		if ttbasetype[t]=tarray then
			if not intypeof then
				p^.alength:=ttlength[t]
				p:=createaddrofop(p)
				p^.mode:=createrefmode(tttarget[t])
			fi
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
	p:=readstrinclude()

when charconstsym then
	a:=0
	shift:=0
	pbyte:=lx.svalue
	if lx.length>8 then serror("char const too long") fi

IF LX.LENGTH>1 THEN
CPL "MULTICHAR CONST:",LX.SVALUE
FI

	to lx.length do
		a:=a ior word64(pbyte^)<<shift
		shift+:=8
		++pbyte
	od
	p:=createconstunit(a,(lx.length<=4|tsint|tsllong))
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

	scope:=d^.scope
	if scope=local_scope and linkage=none_ss or
	   scope=exported_scope and linkage=static_ss or
	   scope=imported_scope and linkage=static_ss then


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

	scope:=d^.scope
	if scope=local_scope and linkage=none_ss or
	   scope=exported_scope and linkage=static_ss or
	   scope=imported_scope and linkage=static_ss then
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
			p:=createunit0(j_break)
			lex()

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


pelse:=nil

if lx.symbol=kelsesym then
	lex()
	pelse:=readblock(1)
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

p^.value:=value

addcasevalue(value)
return p
end

function readexprstmt:unit=
return readexpression()
end

function readcond:unit=
unit pcond
skipsymbol(lbracksym)
pcond:=readexpression()
skipsymbol(rbracksym)
return pcond
end

function isusertype(ref strec owner)int=
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

	case ttbasetype[m]
	when tarray then
		m:=createrefmode(tttarget[m])
	when tproc then
		m:=createrefmode(createprocmode(m,ttparams[m]))
	esac

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

ref strec f,owner
int scope

owner:=stmodule
wasdef:=0

f:=checkdupl(owner, d, ns_general, 0)


if f then					!already exists
	if f^.nameid<>procid then
		serror_s("fn: name in use %s",d^.name)
	fi
	d:=f

	scope:=d^.scope
	if scope=local_scope and linkage=none_ss or
	   scope=exported_scope and linkage=static_ss or
	   scope=imported_scope and linkage=static_ss then
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
	fi

	readfunctionbody(d)
	if lx.symbol=semisym then
		serror("; after function def")
	fi
fi

return d
end

proc readfunctionbody(ref strec f)=
ref strec e
unit p
ref paramrec pm
int pmcount

currproc:=f
nextblockno:=currblockno:=0
pmcount:=0

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
unit q
int t

t:=p^.mode

checklvalue(p)

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
else

cad1::
	checklvalue(p)
endswitch

p:=createunit1(j_addrof,p)
p^.mode:=createrefmode(t)
p^.alength:=alength


return p
end

function createaddop(unit x,y)unit=
unit z
int s,t,u,opc,elemsize

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
	x^.uvalue := x^.uvalue/y^.uvalue
	return x
when tdouble then
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
int s


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

RETURN 0
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
int t
if (t:=p^.mode)=tsint then return fi

retry::
case ttbasetype[t]
when tfloat,tdouble,tref then
	goto doint

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


when j_const then
	if not ttisref[p^.mode] then
		goto notlv
	fi
when j_convert then
	if p^.a^.tag=j_name then
		return
	fi

else
notlv::
	printunit(nil,p)
	terror_s("Not lvalue: %s",jtagnames[p^.tag])
esac
end

function createcall(unit p,q)unit=
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
R.MODE:=P.MODE
		p:=r

		goto doptr
	fi
when j_dot,j_callfn,j_ifx then
	r:=createunit1(j_ptr,p)
	r^.mode:=tttarget[p^.mode]
	p:=r
	goto doptr


else
CPL =JTAGNAMES[P^.TAG]
PRINTUNIT(NIL,P)
	serror("ccall?")
esac


nparams:=pm^.nparams
aparams:=0

s:=q
while s do
	++aparams				!number of actual params supplied
	s:=s^.nextunit
od


if aparams<nparams then
	terror("Too few args")
elsif aparams>nparams and pm^.flags<>pm_variadic and pm^.flags<>pm_notset then
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
		e:=createdupldef(tagowner,d,structtagid)
		e^.mode:=createstructmode(e,(funion|tunion|tstruct))
		e^.blockno:=currblockno
		blockcounts[currblockno]:=1
		return e^.mode
	fi
fi


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
	if ttbasetype[starget]=tarray and ttbasetype[ttarget]=tarray then
		return checkpointertypes(starget,ttarget,hard)
	fi
elsif ttbasetype[s]=tproc and ttbasetype[t]=tproc then
	return 1				!NEED PROPER MATCH HERE
fi

return 0
end

function comparemode(int s,t)int=

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

	e:=createdupldef(owner,d,enumtagid)
	e^.mode:=createenummode(e)
	e^.blockno:=currblockno
	blockcounts[currblockno]:=1
	return e^.mode
fi

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


readenumnames(owner)

ttnamedef[e^.mode]:=e
return e^.mode
end

proc readenumnames(ref strec owner)=
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
	fi
od
skipsymbol(rcurlysym)
end

function createdotop(int opc, unit p,ref strec d)unit=
unit q,r,poffset,pb,pc
ref strec e,f,prec,panon,pfield,gend
int m,offset,scale
ref fieldrec fl


m:=p^.mode
if opc=j_idot then			!
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

prec:=ttnamedef[m]				!r is record def

f:=d
while f:=f^.nextdupl do
	if f^.owner=prec then
		offset:=f^.offset
		exit
	fi
od

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
unit q
int s,opc

s:=p^.mode


retry::



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


t:=p^.mode
switch p^.tag
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

q:=createconstunit(size,tullong)
return q
end

function readgeneric:unit=
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

print @&.str,"$",,name,,"_info.h"

f:=fopen(&.str,"w");

println @f,"memberinfo_t $",,name,,"[] = {"

e:=ttnamedef[m]^.deflist
nfields:=0

while e do

	println @f,"    {""#"", #,#,#,#,#,#}#", e.name,
		e.mode, ttbasetype[e.mode], tttarget[e^.mode],
		ttsize[e.mode], e.offset, 0, (e.nextdef|","|"")
	++nfields
	e:=e^.nextdef
od

println @f,"};"

println @f,"enum {$#_length = #};",name,nfields

fclose(f)
end

function getmemmode(unit p)int=
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

=== cc_genmcl.m 16/31 ===
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
ref strec d,e


mclinit()

stmodule:=moduletable[n].stmodule


d:=stmodule^.deflist
while d do
	case d^.nameid
	when staticid then
		dostaticvar(d)
	when procid then
		e:=d^.deflist
		while e do
			case e^.nameid
			when staticid then
				dostaticvar_fn(e)
			when frameid then
				if e^.code then
					if e^.code^.tag=j_makelist or 
					    ttbasetype[e^.mode]=tarray and e^.code^.tag=j_const then
						dostaticvar_fn(e)
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
			genprocdef(d)
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


setsegment('C')

initmcdest()
setalign(16)
genassem("!------------------------------------")

currproc:=p
dolabel(p)

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

genprocentry(framebytes,parambytes)

if nparams then						!1 or more params
	np:=min(4,nparams)
	if p^.paramlist^.flags=pm_variadic then
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
	bx:=genindex(areg:rframe,offset:structretoffset)
	genmc(m_mov,bx,genreg(r9,8))
fi

if iscallbackproc then
	strcpy(&.str,"m$pushcallback*")
	genmc(m_call, genname(&.str))
fi

stackaligned:=initial_stackalignment

retindex:=lab:=createfwdlabel()

gencomment("-------------------------------------------------")

enterproc(p^.name)


do_stmt(p^.code)

definefwdlabel(retindex)
gencomment("-------------------------------------------------")

if ismain then
	pushstack(32)
	genmc(m_mov, genreg(r10,8), genint(0));
	genmc(m_call, genname("exit*"))
else
	leaveproc(p^.name)
	genreturn(framebytes,parambytes)
fi

if p^.mode<>tvoid then
	if not checkblockreturn(p^.code) then
		unless eqstring(p.name,"main") then
			gerror_s("Function needs explicit return statement: %s",p.name)
		end
	fi
fi

gencomment("")

p^.mclcode:=mccode
end

function checkblockreturn(unit p)int=
unit e,wt
int m,res


IF not FMODERN THEN RETURN 1 FI
if p=nil then return 0 fi

m:=p.mode

case p^.tag
when j_return then			!that's an easy one...
	return 1
when j_if then
	return checkblockreturn(p^.b) and checkblockreturn(p^.c)		!all branches must have a return

when j_switch then
	RETURN 1;
	return checkblockreturn(p.b)

when j_block then
	e:=p^.a
	if e then
		while e and e^.nextunit do
			e:=e^.nextunit
		od
		return checkblockreturn(e)
	fi

when j_labelstmt then
	return checkblockreturn(p.a)

when j_goto, j_while, j_dowhile then
	return 1;


esac

	return 0
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

if fbytes or pbytes then			!need frame pointer
	genmc(m_push,dframeopnd)
	genmc(m_mov,dframeopnd,dstackopnd)

	if fbytes then
		pushstack(roundto(fbytes,16))
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

	if isintcc(t) or isrealcc(t) then
		if t=tfloat then
			sx:=p^.xvalue
			genmc(m_dd,genint(int32@(sx),4))
		else
			genmc((ttsize[t]|m_db, m_dw, 0, m_dd, 0,0,0, m_dq|0),genint(p^.value,ttsize[t]))
		fi
	elsif ttbasetype[t]=tref then
		padding:=0
doref::
		if p^.value=0 then
			genmc(m_dq,genint(0,8))
		elsif p^.strarray then					!immediate string (char[])
			if ttsize[tttarget[t]]=1 then
				genmc(m_defstr,genstrimm(p^.svalue,p^.slength))
			else
				genmc(m_defwstr,genwstrimm(p^.wsvalue,p^.wslength))
			fi
			if padding>0 then
				genmc(m_resb,genint(padding))
			fi

		elsif p^.isstrconst then
			genmc(m_dq, genstrimm(p^.svalue,p^.slength))
			if padding>0 then
				genmc(m_resb,genint(padding))
			fi
		elsif p^.iswstrconst then
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

			getstrint(b.value, &.str2)

			strcat(&.str,&.str2)
			genmc(m_dq, genname(&.str))
		else
			gerror("Add/Idata &frame")
		esac	
	elsif a^.tag=j_const and b^.tag=j_const and ttbasetype[a^.mode]=tref then		!ASSUME REF+REF
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

=== cc_libmcl.m 17/31 ===
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
global int currblocksize			!0, or set to largest block ret value
global int retbeforeblock			!1 when blockcall follows explicit return

global ref opndrec dstackopnd
global ref opndrec dframeopnd


global ref opndrec zero_opnd=nil
global unit zero_unit

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

global proc mclinit=

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
				return 			!don't generate the load
			fi
		fi
	fi
when m_jmp then
	case mccodex^.opcode
	when m_ret, m_retn, m_jmp then
		return
	esac
when m_push,m_pop then
	stackaligned ixor:=1
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
return a
end

proc writemclblock(ref mclrec m)=
int i

i:=1
while m do
	writemcl(i,m)
	++i
	m:=m^.nextmcl
od
end

global function writemclcode(ichar caption,int nmodule)ref strbuffer=
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
	print @&.str,";",,a.svalue
	return &.str

when m_labelname then
	strcpy(&.str,a^.svalue)
	return &.str

when m_label then
	if b then
		fprint @&.str,"L#:#	<#>",a.value,(a.isglobal|":"|""),b.def.name

	else
		fprint @&.str,"L#:#",a.value,(a.isglobal|":"|"")
	fi
	return &.str

esac

case opcode
when m_jmpcc then
	print @&.opcname,"j",,asmcondnames[cond]

when m_setcc then
	print @&.opcname,"set",,asmcondnames[cond]

when m_cmovcc then
	print @&.opcname,"cmov",,asmcondnames[cond]

else
	strcpy(&.opcname,mclnames[opcode]+2)
esac

ipadstr(&.opcname,11," ")
print @&.str,"\t",,&.opcname

if a and b then		!2 operands
	sizepref:=needsizeprefix(opcode,a,b)

	strcpy(&.opnd2,stropnd(b,sizepref))
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
		print @&.str2,(value>0|"+"|""),,value
		strcat(&.str,&.str2)
	fi
	return &.str
fi

case a^.valtype
when int_val,intix_val then
	getstrint(value,&.str)

when real_val,realix_val then
	print @&.str,a.xvalue

when string_val,stringix_val then
	if strlen(a^.svalue)+4<str.len then
		fprint @&.str,"\"#\"*#",a.svalue,a.slength
	else
		return "<LONGSTR>"
	fi
when wstring_val,wstringix_val then
	return "<WSTRING>"
when name_val then
	return a^.svalue
when label_val then
	print @&.str,"L",,value
else
	str[1]:=0
esac

return &.str
end

global proc setsegment(int seg,align=1)=
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
	if eqstring(d^.name,"main") then
		return "main"
	elsif eqstring(d^.name,"start") then
		return "start"
	else
		return getdottedname(d)
	fi
	return ""
end

global function widenstr(ichar s,int w)int=

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
print @&.str,"L",,n
return &.str
end

global function makeindirect(ref opndrec a,int size=0)ref opndrec =
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

if a^.size<>size then
	a:=duplopnd(a)
	a^.size:=size
fi
return a
end

global function isframe(ref strec d)int=
case d^.nameid
when frameid, paramid then
	return 1
esac
return 0
end

global proc genreturn(int fbytes,pbytes)=
[256]char str
int iscallback


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
ref opndrec a
int t

a:=newopnd()
a^.mode:=a_imm

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
GERROR("GENREG/BLOCK SIZE")
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
while size iand (targetsize-1) do ++size od
return size
end

global function iscallbackfn(ref strec p)int=


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
	getstrint(reg-r0,&.str2)
	rs:=&.str2
esac

print @&.str,prefix[size],,rs

return &.str
end

global function getblockname(int reg,size)ichar=
static [32]char str
fprint @&.str,"N#(#)",reg-1,size
return &.str
end

global function fgetregname(int reg,size=8)ichar=
static [32]char str

print @&.str,(size=8|"DX"|"SX"),,reg-xr0

return &.str
end

global function issimple(unit p)int=

if not fdosimple then return 0 fi
return issimple0(p,0)

end

function issimple0(unit p,int level)int=
unit a


++level

IF LEVEL>5 THEN
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

when j_add, j_sub, j_iand, j_ior, j_ixor then
dobin::
	if issimple0(a,level) and issimple0(p^.b,level) then
		return 1
	fi

when j_mul then
	if gettypecat(p)='I' then
		goto dobin
	fi


when j_convert then
	case p^.opcode
	when soft_c,hard_c,swiden_c,uwiden_c then
		return issimple0(a,level)
	esac

when j_addrof then
	return issimple0(a,level)

when j_dot then
	return issimple0(a,level)


when j_shl, j_shr then
	if issimple0(a,level) and p^.b^.tag=j_const then
		return 1
	fi



end switch

return 0
end

global function issimplepm(unit p)int=

if not fdosimple then return 0 fi

case p^.tag
when j_const,j_name then
	return 1



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
if p^.tag=j_const and isintcc(p^.mode) then
	return 1
fi
return 0
end

global function _getnextreg(ref opndrec ax,int reg=0)int=
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

if reg=0 then
REG:=R0
fi

return reg			!no registers used
end

global function getnextreg(ref opndrec ax,int r=0)int=
static int maxreg=0
int reg

reg:=_getnextreg(ax,r)

if reg>maxreg then
	maxreg:=reg
fi

return reg
end

global function ispoweroftwo(int64 x)int=
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
if a^.mode<>b^.mode then return 0 fi
if a^.size<>b^.size then return 0 fi
if a^.value<>b^.value then return 0 fi
if a^.reg<>b^.reg then return 0 fi
if a^.regix<>b^.regix then return 0 fi
if a^.valtype<>b^.valtype then return 0 fi
if a^.scale<>b^.scale then return 0 fi

if a^.def and b^.def and a^.def=b^.def and a^.value=b^.value then
	return 1
elsif a^.def=nil and b^.def=nil and a^.value=b^.value then
	return 1
fi
return 0
end

function findlastmcl:ref mclrec=
GERROR("FINDLASTMCL")
return nil
end

global proc genmsource(int lineno)=			!GENBSOURCE
end

global function roundto(int64 a,n)int64=
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

return stdtypecat[ttbasetype[a^.mode]]
end


global proc doblockcall(int size)=
ref opndrec ax

if retbeforeblock then
cp "."
fi
while size iand 15 do ++size od

if currblocksize=0 then
	currblocksize:=size
	frameoffset-:=size
	framebytes+:=size
	stacksetinstr^.b^.value:=roundto(framebytes,16)
elsif currblocksize<size then
	frameoffset-:=(size-currblocksize)
	framebytes+:=(size-currblocksize)
	currblocksize:=size
	stacksetinstr^.b^.value:=roundto(framebytes,16)
fi

ax:=genreg(r9,8)
genmc(m_lea,ax,genindex(areg:rframe,offset:frameoffset))

end

global function getblockreg(int size)ref opndrec=
ref opndrec ax

while size iand 15 do ++size od
if currblocksize<size then gerror("getblockreg?") fi
genmc(m_lea,ax:=genreg(r0,8),genindex(areg:rframe,offset:frameoffset))
return ax
end

global proc copyretvalue(int size)=

[256]char str


genassem(";-----------")
genassem("	push d0")
genassem("	push d0")
genassem("	sub dstack,32")
fprint @&.str,"	mov d10,[dframe#]",structretoffset

genassem(&.str)

genassem("	mov d11,d0")

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
=== cc_blockmcl.m 18/31 ===
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

GLOBAL INT NADDTO
GLOBAL INT NADDTOX

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


oldclineno:=clineno
clineno:=p^.fileno<<24+p^.lineno


a:=p^.a
b:=p^.b


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
when j_dowhile then
	do_dowhile(a,b)
when j_goto then
	do_goto(p^.def)

when j_labelstmt then
	do_labeldef(p^.def)
	do_stmt(a)
when j_casestmt then

	fprint @&.str,"case",p^.index,,":"

	gencomment(pcm_copyheapstring(&.str))
	if sw_ncases=0 then
		genmc(m_label,genlabel(sw_labeltable^[p^.value-sw_lower+1]))
	else
		value:=p^.value
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
when j_addto then
++NADDTO
	dx_addto(m_add,a,b,0)

when j_subto then
++NADDTO
	dx_addto(m_sub,a,b,0)

when j_multo then
++NADDTO
	dx_multo(a,b,0)
when j_divto,j_remto then
++NADDTO
	dx_divto(p,a,b,0)
when j_iandto then
++NADDTO
	dx_addto(m_and,a,b,0)

when j_iorto then
++NADDTO
	dx_addto(m_or,a,b,0)

when j_ixorto then
++NADDTO
	dx_addto(m_xor,a,b,0)

when j_shlto, j_shrto then
++NADDTO
	dx_shlto(p,a,b,0)

when j_preincr, j_postincr then
	do_preincr(a,m_add,m_inc)

when j_predecr, j_postdecr then
	do_preincr(a,m_sub,m_dec)


when j_exprlist then
	do_exprlist(a)

else

	if p^.tag<>j_const or not fshownames then
		loneexpr(p)
	fi

endswitch

end

function dx_expr(unit p, int reg=r0,am=1)ref opndrec =
int oldclineno,value,i,m
unit a,b
ref opndrec rx,ax,bx,lhs,rhs,tx
[256]char str
ref strec d

if p=nil then
	return nil
fi


if reg>r8 and reg<r10 then
	CPL "DOEXPR TOO MANY REGS?"
fi

oldclineno:=clineno
clineno:=p^.fileno<<24+p^.lineno
tx:=nil

a:=p^.a
b:=p^.b
m:=p^.mode

switch p^.tag
when j_const then
	return dx_const(p,reg)

when j_name then
	return dx_name(p,reg,am)

when j_widenmem then
	return dx_widen(a,m,reg)

when j_funcname then
	return genmemaddr_u(p)

when j_assign then
	return dx_assign(a,b,reg)
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
when j_add then
	if ttisref[a^.mode] and ttsize[b^.mode]<=4 then
		b^.mode:=tintptr
	fi
	return dx_add(a,b,reg)

when j_sub then
	return dx_sub(a,b,reg)

when j_mul then
	return dx_mul(p,a,b,reg)
when j_div then
	return dx_div(p,a,b,reg)

when j_rem then
	return dx_rem(p,a,b,reg)
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
when j_preincr, j_predecr then
	return dx_preincrx(p,a,reg)

when j_postincr, j_postdecr then
	return dx_postincrx(p,a,reg)

when j_addto then
++NADDTOX
	return dx_addto(m_add,a,b,reg)

when j_subto then
++NADDTOX
	return dx_addto(m_sub,a,b,reg)

when j_multo then
++NADDTOX
	return dx_multo(a,b,reg)

when j_divto, j_remto then
++NADDTOX
	return dx_divto(p,a,b,reg)

when j_iandto then
++NADDTOX
	return dx_addto(m_and,a,b,reg)

when j_iorto then
++NADDTOX
	return dx_addto(m_or,a,b,reg)

when j_ixorto then
++NADDTOX
	return dx_addto(m_xor,a,b,reg)

when j_shlto, j_shrto then
++NADDTOX
	return dx_shlto(p,a,b,reg)
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
	do_assignblock(a,b)
	return
esac
reg:=r0


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
		tx:=saveexpr(b,r0)
		lhs:=getlvalueopnd(a,r0)
		rhs:=restoreexpr(tx,getnextreg(lhs,reg))
	fi
fi
storeopnd(lhs,rhs)
end

function dx_assign(unit a,b,int reg)ref opndrec =
ref opndrec lhs,rhs,rx,ax,bx
int tx


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
pushexpr(a,reg)
return ttsize[a^.mode]
end

function fsaveexpr(unit a,int reg=xr0)int=
fpushexpr(a,reg)
return ttsize[a^.mode]
end

function restoreexpr(int tx, reg)ref opndrec=
ref opndrec rx

genmc(m_pop,rx:=genreg(reg,targetsize))		!register must be full size
rx:=duplopnd(rx)
rx^.size:=tx
return rx
end

function frestoreexpr(int tx, reg)ref opndrec=
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


if issimple(b) then
SIMPLEADD::
	ax:=loadexpr(a,reg)
	bx:=evalexpr(b,getnextreg(ax,reg))

elsif issimple(a) then	!reverse order

	ax:=loadexpr(b,reg)
	bx:=evalexpr(a,getnextreg(ax,reg))
else
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
		bx:=ax
	else
		bx:=fevalexpr(b,getnextreg(ax,reg))
	fi
elsif issimple(a) and (opc=m_add or opc=m_imul) then	!reverse order
	ax:=floadexpr(b,reg)
	bx:=fevalexpr(a,getnextreg(ax,reg))
else
	tx:=fsaveexpr(b,reg)
	ax:=floadexpr(a,reg)
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
		bx:=fevalexpr(rhs,getnextreg(ax,reg))
	else
		tx:=saveexpr(rhs,reg)
		ax:=floadexpr(lhs,reg)
		bx:=frestoreexpr(tx,getnextreg(ax,reg))
	fi
	genmc(m_fcmp,ax,bx)

else
	if issimple(rhs) then
		ax:=loadexpr(lhs,reg)
		bx:=evalexpr(rhs,getnextreg(ax,reg))
		genmc(m_cmp,ax,bx)

	elsif issimple(lhs) then			!reverse test, but also modify condition
		mclcond:=reversemclcond(mclcond)
		ax:=loadexpr(rhs,reg)
		bx:=evalexpr(lhs,getnextreg(ax,reg))
		genmc(m_cmp,ax,bx)

	else
		pushexpr(rhs,reg)
		ax:=loadexpr(lhs,reg)
		bx:=genreg(getnextreg(ax,reg),ax^.size)
		genmc(m_pop,changeopndsize(bx,targetsize))
		genmc(m_cmp,ax,bx)
	fi
esac

genmc_cond(m_jmpcc, mclcond,genlabel(lab))
END

function reversecond(int op)int=

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
ref opndrec ax,bx,fx


fx:=floadexpr(q,xr13)
genmc(m_fwiden,ax:=changeopndsize(fx,8),fx)
genmc(m_fmov,bx:=genreg(r13,8),ax)
genmc(m_push,bx)
end

proc loadfloatparam(unit q,int regoffset,iscomplex)=

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


if issimple(b) then
SIMPLESUB::
	ax:=loadexpr(a,reg)
	bx:=evalexpr(b,getnextreg(ax,reg))

elsif issimple(a) then		!reverse order, add neg at the end
	ax:=loadexpr(b,reg)
	bx:=evalexpr(a,getnextreg(ax,reg))
	doneg:=1
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
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

if b^.tag=j_const and isintcc(b^.mode) then
	x:=b^.value
	if n:=ispoweroftwo(x) then
		p^.tag:=j_shl
		b^.value:=n
		return dx_shl(p,a,b,reg)
	fi
fi

if issimple(b) then

	ax:=loadexpr(a,reg)
	if a^.tag=j_name and b^.tag=j_name and a^.def=b^.def then
		bx:=ax
	else
		bx:=evalexpr(b,getnextreg(ax,reg))
	fi
elsif issimple(a) then	!reverse order
	ax:=loadexpr(b,reg)
	bx:=evalexpr(a,getnextreg(ax,reg))
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
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
		bx:=loadexpr(b,getnextreg(ax,reg))
	else
		bx:=evalexpr(b,getnextreg(ax,reg))
	fi
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi

genmc(opc,bx)

return ax
end


function dx_shl(unit p,a,b,int reg)ref opndrec=
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
	bx:=restoreexpr(tx,getnextreg(ax,reg))
fi

genmc(opc,ax,bx)

return ax
end

function dx_iand(int opc,unit a,b,int reg)ref opndrec=
ref opndrec ax,bx
int tx

if issimple(b) then
	ax:=loadexpr(a,reg)
	bx:=evalexpr(b,getnextreg(ax,reg))

elsif issimple(a) then	!reverse order
	ax:=loadexpr(b,reg)
	bx:=evalexpr(a,getnextreg(ax,reg))

else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
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
fi

genmc(opc,result,ptropnd)

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
fi

genmc(opc,rr1:=genreg(getnextreg(ptropnd,reg)+1,getopndsize_u(p)),ptropnd)
genmc(m_mov,result,rr1)

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
int mulfactor, tx, reg2
ref opndrec ax,ix,m
ref strec d


if index and index^.tag=j_const then
	offset+:=index^.value*scale
	index:=nil
fi

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
			if mulfactor=1 then
				ix:=loadexpr(index,reg)
				ax:=loadexpr(a,reg+1)
				m:=genindex(areg:ax^.reg, ireg:ix^.reg, scale:scale,offset:offset,size:size)
			else
				loadexpr(index,reg)
				mulreg(reg,mulfactor)
				loadexpr(a,reg+1)
				m:=genindex(areg:reg+1, ireg:reg, scale:scale,offset:offset,size:size)
			fi
		elsif issimple(index) then			!complex/simple
			loadexpr(a,reg)
			loadexpr(index,reg+1)
			mulreg(reg+1,mulfactor)
			m:=genindex(areg:reg, ireg:reg+1, scale:scale,offset:offset,size:size)
		else							!complex/complex
			tx:=saveexpr(a,reg)
			ix:=loadexpr(index,reg)
			mulreg(reg,mulfactor)
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

return ax
end

function dx_addptr(unit p,a,b,int reg,am=1)ref opndrec=

int size,scale,mulfactor,reg1,tx,offset
ref opndrec m,ax,bx,rx
unit pname




size:=scale:=ttsize[tttarget[a^.mode]]
if p^.ptrscale=0 then scale:=1 fi			!addptr from dot op: offset in bytes


offset:=0
if b^.tag=j_const then
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



if b^.tag=j_const then					!any+const
	case a^.tag
	when j_addrof then
		pname:=a^.a
		if pname^.tag<>j_name then goto other fi
		m:=genindex(def:pname^.def,offset:offset,size:size)
	else
other::
		loadexpr(a,reg)
		m:=genindex(areg:reg, scale:scale,size:size, offset:b^.value)
	esac

elsif a^.tag=j_addrof and a^.a^.tag=j_name then		!&a+const
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
	loadexpr(a,reg)
	loadexpr(b,reg1)
	mulreg(reg1,mulfactor)
	m:=genindex(areg:reg, ireg:reg1, scale:scale,size:size)
elsif issimple(a) then
	loadexpr(b,reg)
	mulreg(reg,mulfactor)
	loadexpr(a,reg1)
	m:=genindex(areg:reg1, ireg:reg, scale:scale,size:size)
else
cxcx::

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


size:=scale:=ttsize[tttarget[a^.mode]]

if ttbasetype[b^.mode]=tref then		!ptr-ptr
	ax:=dx_sub(a,b,reg)
	divreg(ax^.reg,scale)
	return ax
fi


offset:=0
if b^.tag=j_const then
	b^.value:=b^.value*scale
	offset:=-b^.value
	scale:=1
fi

mulfactor:=scale
scale:=1

reg1:=reg+1


if a^.tag=j_addrof and a^.a^.tag=j_name then
	a:=a^.a
fi


if b^.tag=j_const then
	loadexpr(a,reg)
	m:=genindex(areg:reg, scale:scale,size:size, offset:offset)
	if am=2 then
		return m
	fi
	genmc(m_lea,rx:=genreg(reg,ptrsize),m)

	return rx

elsif issimple(b) then
	ax:=loadexpr(a,reg)
	bx:=loadexpr(b,reg1)
	mulreg(reg1,mulfactor)
elsif issimple(a) then
	bx:=loadexpr(b,reg)
	mulreg(reg,mulfactor)
	ax:=loadexpr(a,reg1)
else
CXCX::
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
	bx:=restoreexpr(tx,getnextreg(ax,reg))
	mulreg(bx^.reg,mulfactor)
fi
m:=genindex(areg:reg, ireg:bx^.reg, scale:scale,size:size)

genmc(m_sub,ax,bx)

return ax
end

function dx_convert(unit a, int t,opc, reg)ref opndrec=
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

fprint @&.str,"`$#.#.#",currproc.name,d.name,d.blockno

genmc(m_mov, genreg(r11,ptrsize), genname(&.str))

genmc(m_mov,genreg(r12,ptrsize),genint(nbytes))

genmc(m_call,genname("memcpy*"))

popstack(32)

end

function do_assignblock (unit a,b, int regx=0)ref opndrec =
ref opndrec ax,bx,rx,rs,rd,rcount,bx2,rsa,rda
int rev,workreg,nwords,lab, regcount,regsource, regdest, offset, n, oddbytes, reg

reg:=(regx|regx|xr0)

rev:=0
if issimple(b) then
	ax:=getlvalueopnd(a,reg)
	bx:=getlvalueopnd(b,getnextreg(ax,reg))

elsif issimple(a) then
	rev:=1
	ax:=getlvalueopnd(b,reg)
	bx:=getlvalueopnd(a,getnextreg(ax,reg))

else
	bx:=getlvalueopnd(b,reg)
	if bx^.mode<>a_reg then
		genmc(m_lea,bx2:=genreg(reg,ptrsize),bx)
	else
		bx2:=bx
	fi
	genmc(m_push,bx2)
	ax:=getlvalueopnd(a,reg)

	genmc(m_pop,bx:=genreg(getnextreg(ax,reg),ptrsize))
	bx:=genireg(bx^.reg)

fi

if ax^.mode=a_reg then ax:=genireg(ax^.reg) fi
if bx^.mode=a_reg then bx:=genireg(bx^.reg) fi

if rev then
	swap(ax,bx)
fi



IF GETNEXTREG(AX)>R4 OR GETNEXTREG(BX)>R4 THEN GERROR("ASSIGNBLOCK/REG") fi

workreg:=r4

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
ref caserec pcase
ref opndrec ax,bx

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

defaultlabel:=createfwdlabel()		!(when no default:, same as breakswlabel)
breakswlabel:=createfwdlabel()

if length>maxswitchrange then


	serialsw:=1

	ax:=loadexpr(a)
	for i:=1 to ncases do
		labeltable[i]:=createfwdlabel()
		genmc(m_cmp,ax,genint(valuetable[i]))
		genmc_cond(m_jmpcc,eq_cond,genlabel(labeltable[i]))
	od
	genmc(m_jmp,genlabel(defaultlabel))

elsif length=0 then
	genmc(m_jmp,genlabel(defaultlabel))

else
	serialsw:=0
	memset(&flags,0,length)				!clear value flags

	for i:=1 to length do
		labeltable[i]:=defaultlabel
	od

	for i:=1 to ncases do
		value:=valuetable[i]
		index:=value-lower+1			!index of value within label table
		labeltable[index]:=createfwdlabel()

		if flags[index] then
			gerror_s("Dupl case value: %d",cast(value))
		fi
		flags[index]:=1
	od

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

old_labeltable:=sw_labeltable
old_valuetable:=sw_valuetable
old_lower:=sw_lower
old_ncases:=sw_ncases
old_defaultseen:=sw_defaultseen
old_defaultlabel:=sw_defaultlabel
old_breaklabel:=sw_breaklabel

sw_labeltable:=&labeltable
sw_valuetable:=&valuetable		!NEEDED ONLY FOR COMPLEX SWITCH
sw_lower:=lower

sw_ncases:=(serialsw|ncases|0)
sw_defaultseen:=0
sw_defaultlabel:=defaultlabel
sw_breaklabel:=breakswlabel

do_stmt(b)						!switch body

if not sw_defaultseen then
	definefwdlabel(defaultlabel)
fi
definefwdlabel(breakswlabel)

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
		bx:=loadexpr(b,getnextreg(ax,reg))
	else
		bx:=evalexpr(b,getnextreg(ax,reg))
	fi
else
	tx:=saveexpr(b,reg)
	ax:=loadexpr(a,reg)
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
		rhs:=evalexpr(b,getnextreg(ptropnd,reg))
	else
		rhs:=loadexpr(b,getnextreg(ptropnd,reg))
	fi
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
elsif issimple(a) then
	rhs:=loadexpr(b,reg)
	ptropnd:=getlvalueopnd(a,getnextreg(rhs,reg))
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
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
	reg:=getnextreg(ptropnd,reg)
	rhs:=fevalexpr(b,reg)
	reg:=getnextreg(rhs,reg)

elsif issimple(a) then
	rhs:=fevalexpr(b,reg)
	reg:=getnextreg(rhs,reg)
	ptropnd:=getlvalueopnd(a,reg)
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
ref opndrec ax,bx,rx,rxb
int mclcond,tx

mclcond:=getmclcond(p^.tag,a^.mode)

case mclcond
when feq_cond, fne_cond,flt_cond,fge_cond,fle_cond,fgt_cond then
	if issimple(b) then
		ax:=floadexpr(a,reg)
		bx:=fevalexpr(b,getnextreg(ax,reg))

	elsif issimple(a) then	!reverse order
		mclcond:=reversemclcond(mclcond)
		ax:=floadexpr(b,reg)
		bx:=fevalexpr(a,getnextreg(ax,reg))
	else
		tx:=saveexpr(b,reg)
		ax:=floadexpr(a,reg)
		bx:=frestoreexpr(tx,getnextreg(ax,reg))
	fi
	genmc(m_fcmp,ax,bx)
else
	if issimple(b) then
		ax:=loadexpr(a,reg)
		bx:=evalexpr(b,getnextreg(ax,reg))
	elsif issimple(a) then			!reverse order
		mclcond:=reversemclcond(mclcond)
		ax:=loadexpr(b,reg)
		bx:=evalexpr(a,reg)
	else
		tx:=saveexpr(b,reg)
		ax:=loadexpr(a,reg)
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
		rhs:=evalexpr(b,getnextreg(ptropnd,reg))
	else
		rhs:=loadexpr(b,getnextreg(ptropnd,reg))
	fi
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
elsif issimple(a) then
	rhs:=loadexpr(b,reg)
	ptropnd:=getlvalueopnd(a,getnextreg(rhs,reg))
	rhs:=changeopndsize(rhs,ptropnd^.size)
	genmc(opc,ptropnd,rhs)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
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
	reg:=getnextreg(ptropnd,reg)
	rhs:=evalexpr(b,reg)
	reg:=getnextreg(rhs,reg)

elsif issimple(a) then
	rhs:=evalexpr(b,reg)
	reg:=getnextreg(rhs,reg)
	ptropnd:=getlvalueopnd(a,reg)
	reg:=getnextreg(ptropnd,reg)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
	reg:=getnextreg(ptropnd,reg)
	rhs:=restoreexpr(tx,reg)
	++reg
fi

loadviaptr(work,ptropnd,sgned)

genmc(m_imul,work,rhs)

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


n:=ispoweroftwo(scale)

if n=0 then
	bx:=genint(scale)
	if opc=m_imul then
		genmc(m_imul,ax,bx)
	else
		if ax^.reg<>r0 then
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
	ptropnd:=getlvalueopnd(a,reg)
	reg:=getnextreg(ptropnd,reg)
	rhs:=evalexpr(b,reg)
	reg:=getnextreg(rhs,reg)

elsif issimple(a) then
	rhs:=evalexpr(b,reg)
	reg:=getnextreg(rhs,reg)
	ptropnd:=getlvalueopnd(a,reg)
	reg:=getnextreg(ptropnd,reg)
else
	tx:=saveexpr(b,reg)
	ptropnd:=getlvalueopnd(a,reg)
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

case p^.def^.nameid
when procid then
	return genmemaddr_u(p)
esac

ax:=genmem_u(p)

return ax
end

proc divreg(int reg,int64 x)=
ref opndrec rr2
int n

if x>1 then
	if n:=ispoweroftwo(x) then
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
ref opndrec ax,rx

ax:=evalexpr(a,reg)

ax:=applyoffset(ax,p^.offset,ttsize[p^.mode])

return ax
end

proc loadviaptr(ref opndrec w, ptropnd, int sgned)=
ref opndrec w2

if w^.size<=ptropnd^.size then
	genmc(m_mov,w, ptropnd)
	return
fi


genmc((sgned|m_iwiden|m_uwiden),w,ptropnd)

end
=== cc_genasm.m 19/31 ===
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
import cc_libmcl

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
ref strec d

gs_init(dest)

inita64()

stmodule:=moduletable[moduleno].stmodule

gs_str(dest,"!x64 output for ")
gs_str(dest,stmodule^.name)
gs_strln(dest,".c")

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


if fastasm then
else
	if fverbose then
		println "Writing",outfile,,":"
	fi
	writefile(outfile,cast(dest^.strptr),dest^.length)
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

when m_uwiden then
	do_changeop(mx_movzx)

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


when m_idiv,m_udiv then
	do_idiv(a)


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

when a_imm then
	convertimm(a,sx:1)

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
	asmstr(getregname(a^.reg,a^.size))

when a_imm then
	strvaluex(a)

when a_mem then
	if sizeprefix then
		asmstr(getsizeprefix(a^.size,1))
	fi
	asmchar('[')

	plus:=""
	if a^.reg then

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

print @&.str,"XMM",,reg-r0
return &.str
end

global function getstringname(int n)ichar=
static [16]char str
if n=0 then kk0used:=1 fi

print @&.str,"KK",,n

return &.str
end

global function getwstringname(int n)ichar=
static [16]char str

print @&.str,"WW",,n
return &.str
end

global function getrealname(int n)ichar=
static [16]char str

print @&.str,"R.",,n
return &.str
end

global function getsrealname(int n)ichar=
static [16]char str

print @&.str,"SR.",,n
return &.str
end

global function getdintname(int n)ichar=
static [16]char str

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

asmstr(strvalue(a))
end

proc convertimm(ref opndrec a,int sx=0)=
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
int i, col

return unless nwstrings

gs_strln(dest,"!Wide String Table")
gs_strln(dest,"	segment idata")
gs_str(dest,"	align ")
gs_strint(dest,targetsize)
gs_line(dest)


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
int i, state, c, a,col

gs_str(dest,"	")
genstring(s,length)

end

proc do_defwstr(ref word16 s,int length)=
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
		strcpy(&.str, strint(fp.ix64,"z16H"))
		gs_str(dest,&.str)

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
		print @&.str,real(fp.x32):".30g"
		gs_strln(dest,&.str)

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
int n



s:=asmptr
getstrint(a,s)

	asmptr+:=strlen(s)

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
int i, state, c, a,col

gs_str(dest,"dw ")
if length=0 then
	gs_strln(dest,"0")
	return
fi

state:=0
for i to length do
	gs_strint(dest,s++^)
		gs_str(dest,",")
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

	stringtable:=pcm_alloc(ichar.bytes*stringtablesize)
	stringlentable:=pcm_alloc(int.bytes*stringtablesize)

	for i:=1 to nstrings do
		stringtable^[i]:=oldstringtable^[i]
		stringlentable^[i]:=oldstringlentable^[i]
	od

	pcm_free(oldstringtable,ichar.bytes*oldstringtablesize)
	pcm_free(oldstringlentable,int.bytes*oldstringtablesize)
end
=== cc_export.m 20/31 ===
import msys
import mlib
import clib
import oslib

import cc_decls
import cc_support
import cc_tables
import cc_lib
import cc_headers
import cc_lex

INT NALLPROCS
INT NALLRECS

strbuffer mmbuffer
ref strbuffer mm=&mmbuffer

global proc writemheader(ichar infile)=
[300]char mfile
ref strec d,e
int m

strcpy(&.mfile,pcm_copyheapstring(changeext(infile,".m")))

gs_init(mm)

mmstr("importdll ")
mmstr(extractbasefile(infile))
mmstrln(" =")

stmodule:=moduletable[1].stmodule


d:=stmodule^.deflist

while d do
	if isheaderfile(sourcefilenames[d^.lineno>>24]) then
		d:=d^.nextdef
		next
	fi
	case d^.nameid
	when staticid then
		mmstr("    ")
		mmmode(d^.mode)
		mmstr(" ")
		mmstr(fixname(d^.name))
		if d^.code then
			mmstr(" =")
			mmstr(strexpr(d^.code)^.strptr)
		fi
		mmline()

	when procid then
++NALLPROCS
		writefunction(d)
	when typeid then
	when enumid then
		mmstr("    const ")
		mmleftstr(fixname(d^.name),34)
		mmstr(" = ")
		mmint(d^.index)
		mmline()

	when macroid then
		mmstr("MACRO ")
		mmstrln(fixname(d^.name))
	when structtagid then
++NALLRECS

		writerecord(d^.mode)

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

CPL "DONE"
CPL =NALLPROCS
CPL =NALLRECS

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


if d^.mode=tvoid then
	mmstr("    clang proc     ")
else
	mmstr("    clang function ")
fi
mmstr("""")

mmstr(d^.name)

mmstr("""")
mmleftstr(" ",34-strlen(d^.name))
mmstr("(")


pm:=d^.paramlist
n:=pm^.nparams
isvar:=pm^.flags=pm_variadic
for i to n do
	mmmode(pm^.mode)
	if i<>n or isvar then
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

when tproc then
	MMSTR("<PROC>")

else
	mmstr(stdtypemnames[t])
esac
end

proc writerecord(int m, rectype='R', level=1)=
ref strec d,e
int emode


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
		return pcm_copyheapstring(&.str)
	fi
od


return name
end
=== cc_assembler.m 21/31 ===
import clib
import mlib
import oslib

importpath "c:/ax/"
import ax_tables
import ax_decls
import ax_lex
import ax_parse
import ax_lib
import ax_genss
import ax_writeexe
import ax_writeobj

global function assembler(ichar outputfile, ref[]ichar asmfiles,dllfiles,
		int nasmfiles,ndllfiles,fobj,fcaption,ref[]ichar assemsources=nil,
		ichar entrypointname)int=
ref strbuffer ss
int ntokens,t,i


initall()

for i to nasmfiles do
	addmodule(asmfiles^[i])


od

	searchlibs[1]:="ucrtbase"
	searchlibs[1]:="msvcrt"
	searchlibs[2]:="gdi32"
	searchlibs[3]:="user32"
	searchlibs[4]:="kernel32"
	nsearchlibs:=4	

for i to ndllfiles do
	addsearchlib(dllfiles^[i])
od

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
	genexe(entrypointname,"",0)
	writeexe(outputfile,0)
fi
return 1
end

proc loadsourcefiles(ref[]ichar assemsources)=
	int i
	ichar source


	for i to nmodules do
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

d^.nextdupl:=dupltable[d^.htfirstindex]
dupltable[d^.htfirstindex]:=d
end

proc scanglobals=

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

ref strec d,e

d:=modulenamelist

while d do
	lexhashtable[d^.htindex]:=getemptyst(d)
	d:=d^.nextdef
od

modulenamelist:=nil

end
=== ax_tables.m 22/31 ===

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
	(m_leave,			$,		0,		0xC9),		!

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

	(m_pcmpistri,		$,		3,		0x63),		!
	(m_pcmpistrm,		$,		3,		0x62),		!

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
end

global const rframe = r14
global const rstack = r15




global tabledata []ichar dregnames, []byte regsizes, []byte regindices =
	("d0",		8,	r0),		!rax	d0..d9 are for general use
	("d1",		8,	r1),		!r10	d0..d2 are volatile in ABI
	("d2",		8,	r2),		!r11

	("d3",		8,	r3),		!rdi	d3..d9 are preserved across funcs in ABI
	("d4",		8,	r4),		!rbx
	("d5",		8,	r5),		!rsi
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
	("rbx",		8,	r4),
	("rcx",		8,	r10),
	("rdx",		8,	r11),
	("rsi",		8,	r5),
	("rdi",		8,	r3),
	("rbp",		8,	r14),
	("rsp",		8,	r15),
	("r8",		8,	r12),
	("r9",		8,	r13),
	("r10",		8,	r1),
	("r11",		8,	r2),
	("r12",		8,	r6),
	("r13",		8,	r7),
	("r14",		8,	r8),
	("r15",		8,	r9),

	("eax",		4,	r0),
	("ebx",		4,	r4),
	("ecx",		4,	r10),
	("edx",		4,	r11),
	("esi",		4,	r5),
	("edi",		4,	r3),
	("ebp",		4,	r14),
	("esp",		4,	r15),
	("r8d",		4,	r12),
	("r9d",		4,	r13),
	("r10d",	4,	r1),
	("r11d",	4,	r2),
	("r12d",	4,	r6),
	("r13d",	4,	r7),
	("r14d",	4,	r8),
	("r15d",	4,	r9),

	("ax",		2,	r0),
	("bx",		2,	r4),
	("cx",		2,	r10),
	("dx",		2,	r11),
	("si",		2,	r5),
	("di",		2,	r3),
	("bp",		2,	r14),
	("sp",		2,	r15),
	("r8w",		2,	r12),
	("r9w",		2,	r13),
	("r10w",	2,	r1),
	("r11w",	2,	r2),
	("r12w",	2,	r6),
	("r13w",	2,	r7),
	("r14w",	2,	r8),
	("r15w",	2,	r9),


	("al",		1,	r0),
	("bl",		1,	r4),
	("cl",		1,	r10),
	("dl",		1,	r11),

	("ah",		1,	r16),
	("bh",		1,	r17),
	("ch",		1,	r18),
	("dh",		1,	r19),

	("sil",		1,	r5),
	("dil",		1,	r3),
	("bpl",		1,	r14),
	("spl",		1,	r15),

	("r8b",		1,	r12),
	("r9b",		1,	r13),
	("r10b",	1,	r1),
	("r11b",	1,	r2),
	("r12b",	1,	r6),
	("r13b",	1,	r7),
	("r14b",	1,	r8),
	("r15b",	1,	r9),

end

global []ichar xregnames = (
	"xmm0",				! x0..x3 are used for parameter passing in ABI
	"xmm1",
	"xmm2",
	"xmm3",

	"xmm4",				! x4..x5 are volatile
	"xmm5",

	"xmm6",				! x6..x15 are preserved across functions in ABI
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
	("jc",		ltu_cond),
	("jnc",		geu_cond),
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
	("setc",	ltu_cond),
	("setnc",	geu_cond),
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
	("cmovc",	ltu_cond),
	("cmovnc",	geu_cond),
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
=== ax_decls.m 23/31 ===

global const compilerversion="2018.1.22"




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
		ref opndrec expr	!named constants: valuerec([label],[value])
			int32 offset		!label (pass 2): offset of label when encountered
			int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
			int32 importindex	!genexe: index into import table

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

global int lxfileno=0	!*@ current source file number
global int lxlineno=0	!*@ current source line number

global int nsourcefiles=0	!no. of linear file names

global const maxmodules=200
global const maxsearchlibs=30
global [maxmodules]modulerec moduletable
global [maxsearchlibs]ichar searchlibs
global int nmodules
global int nsearchlibs

global const hstsize=1048576*8

global const hstmask=hstsize-1
global [0:hstsize]ref strec lexhashtable
global [0:hstsize]ref strec dupltable		!link dupl names

global ref void logdev		!dest for diagnostics and output of tables

global int fverbose=0		!whether to display message for each pass
global int fquiet=0

global int LINECOUNT=0

global int nundefined=0
global int alineno=0

global int ss_zdatalen
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

global const init_ss_symbols=16384
global ref []ref strec ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref stlistrec globalimportlist		!all global vars and imports across all moduls

global ref strec modulenamelist			!all defs defined in last module
global int currmoduleno

GLOBAL INT NMCLASM
GLOBAL INT NMCLOPNDSASM
=== ax_lex.m 24/31 ===
import clib
import mlib
import oslib
import ax_tables
import ax_decls

macro testmode=0

const etx = 26
const cr  = 13
const lf  = 10


global int lxsymbol		!* main symbol kind
global int lxsubcode	!* for some symbols, which specific keyword

global int64 lxvalue
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
int lxhashvalue

global ref byte lxsptr		!@ points to next char in source
ref byte lxstart		!@ start of source code for this file
global ref strec lxsymptr		!set by lookuplex()

[0..255]char alphamap
[0..255]char digitmap
[0..255]char commentmap

global proc lex=
int i, c, d, hsum, length
ref byte pstart

lxsubcode:=0

doswitch c:=lxsptr++^
when 'a'..'z','$','_','.' then
	pstart:=lxsptr-1		!point to start of name in source buffer
doname::
	hsum:=pstart^

	doswitch c:=lxsptr++^
	when 'a'..'z','0'..'9','_','$','.' then
		hsum:=hsum<<4-hsum+c
	when 'A'..'Z' then
		(lxsptr-1)^:=c+32
		hsum:=hsum<<4-hsum+c+' '
	else
		--lxsptr
		exit
	end

	lxlength:=lxsptr-pstart
	lxhashvalue:=hsum<<5 -hsum

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
	hsum:=0

	doswitch c:=lxsptr^
	when 'A'..'Z','a'..'z','0'..'9','_','$','.' then
		++lxsptr
		hsum:=hsum<<4-hsum+c
	else
		exit
	end

	lxsymbol:=namesym
	if pstart=lxsptr then
		lxerror("NULL ` name")
	fi
	lxlength:=lxsptr-pstart
	lxhashvalue:=hsum<<5-hsum

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
od

commentmap[0]:=0
commentmap[lf]:=0

inithashtable()


end

proc readreal(ref[]char s,int slen, intlen,exponseen)=
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
end

proc inithashtable=
[32]char str
int i

if hstsize>65536 then
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
	if R AND r^.name then
		count+:=1

	fi
od
println @devx,count," items in table",hstsize
end

function lookuplex(ichar name,int length=0)int=
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
lxhashvalue:=gethashvalue(name)
lookuplex(pcm_copyheapstring(name),0)
return lxsymptr
end

global proc lxerror(ichar m)=			!LXERROR

fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno

stop 1
end


global function gethashvalue(ichar s)int=
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

global proc skiptoeol=
repeat
	lex()
until lxsymbol=eolsym or lxsymbol=eofsym
END

function makestring(ichar p,int length)ref char=
ref char s

s:=pcm_alloc(length+1)
memcpy(s,p,length)
(s+length)^:=0
return s
end
=== ax_parse.m 25/31 ===
import clib
import mlib
import ax_tables
import ax_decls
import ax_lex
import ax_lib

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
		case sym
		when eqsym then
			lex()
			case lxsymbol
			when kregsym then
				createregalias(symptr,lxsymptr.subcode, lxsymptr.regsize)
				lex()
			when kxregsym then
				createxregalias(symptr,lxsymptr.subcode)
				lex()
			else
				createnamedconst(symptr,readexpression())
			esac

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
	fprint @&.str,"# expected not #",symbolnames[symbol],symbolnames[lxsymbol]

	serror(&.str)
fi
end

proc readinstr=
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
		SERROR("IMUL3 CAN'T DO 3 OPNDS")

	when m_pcmpistri,m_pcmpistrm then
		a:=readoperand()
		checksymbol(commasym)
		lex()
		b:=readoperand()
		checksymbol(commasym)
		lex()
		c:=readoperand()
		if c.mode<>a_imm then serror("pcmpistr/not int") fi
		genmc(opcode,a,b)
		mccodex.c:=c.value

	when m_proc then
		repeat
			lex()
		until lxsymbol=eolsym

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

end

proc readcondinstr(int opc)=
ref opndrec a,b

a:=genint(lxsubcode)
lex()
b:=readoperand()

if lxsymbol=commasym and opc=m_cmovcc then		!ignore dest
	genmc(m_param,b)							!store extra param as separate instr

	lex()
	b:=readoperand()
fi

genmc(opc,a,b)
end

function readoperand:ref opndrec=
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
		lex()
		return readaddrmode(size)

	else
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
int reg,regsize,scale,regix, addrsize, regixsize, scaleix
ref opndrec x
ref opndrec p

reg:=regix:=0
regsize:=regixsize:=0
scale:=scaleix:=0
x:=nil

if lxsymbol=kregsym then
	readreg(reg,regsize,scale)	
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
checksymbol(rsqsym)
lex()

if scale and not scaleix then
	swap(reg,regix)
	swap(regsize,regixsize)
	swap(scale,scaleix)
fi
if scaleix=0 then scaleix:=1 fi

if regsize and regixsize and regsize<>regixsize then serror("Addr reg size mismatch") fi

p:=genindex(areg:reg, ireg:regix, scale:scaleix, x:x, size:size,
	addrsize:(regsize=4 or regixsize=4|4|8))
return p
end
=== ax_lib.m 26/31 ===
import clib
import msys
import mlib
import ax_tables
import ax_decls
import ax_lex

const ptrsize=8

fwdrec dummy1

global tabledata() [0:]ichar opndnames =
	(a_none=0,	$),
	(a_reg,		$),
	(a_imm,		$),
 	(a_mem,		$),		!any memory modes: [d], [R], [R*4+R2+d+imm] etc
 	(a_cond,	$),		!a condition code for jcc/setcc
	(a_xreg,	$),		!xmm register
	(a_string,	$),		!immediate string (for comments)
end


global record mclrec =		!64 bytes
	ref mclrec nextmcl
	ref opndrec a,b
	word16 opcode
	word16 c
	int lineno
end


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

	m:=pcm_alloc(mclrec.bytes)
++NMCLASM

	m^.nextmcl:=nil

	if lxsymbol=eolsym then
		m^.lineno:=lxlineno-1
	else
		m^.lineno:=lxlineno
	fi

	m^.opcode:=opcode

	nopnds:=(a=nil|0|(b=nil|1|2))
	if nopnds=2 and opcode in [m_pcmpistri,m_pcmpistrm] then nopnds:=3 fi

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

genmc(opcode,genstrimm(s))
end

function newopnd(int mode)ref opndrec=
ref opndrec a

++NMCLOPNDSASM

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
ref opndrec a

if x then							!existing operand filled in with value
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

print @&.str,semi:"z3",index:"z4",," "!, mcl^.lineno

gs_str(dest,&.str)
gs_strln(dest,&.mclstr)
end

global function strmcl(ref mclrec mcl)ichar=			!STRMCL
static [512]char str
[128]char str2
int opcode,sizepref

opcode:=mcl^.opcode

case opcode
when m_assem then
	return mcl^.a^.svalue
when m_blank then
	return ""
when m_comment then
		strcpy(&.str,";")
		strcat(&.str,mcl^.a^.svalue)
		return &.str


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
fi

case opcode
when m_pcmpistri,m_pcmpistrm then
	fprint @&.str2,", #",mcl.c
	strcat(&.str,&.str2)
esac


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
	d:=a^.labeldef
	value:=a^.value
	if d then
		if d^.symbol=namedconstsym then
			return inttostr(d^.expr^.value)
		fi

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
		print @&.str,"""<Long string>"""
	else
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
end

function getsizeprefix(int size,enable=0)ichar=		!GETSIZEPREFIX
if not enable then return "" fi
case size
when 1 then return "byte "
when 2 then return "word "
when 4 then return "dword "
when 8 then return "qword "
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
ref opndrec a

a:=newopnd(a_imm)
a^.size:=size

a^.labeldef:=d
a^.value:=value
a^.valtype:=t

return a
end

global function genint(int64 x,int size=4)ref opndrec=
ref opndrec a


a:=newopnd(a_imm)
a^.size:=size
a^.value:=x

return a
end

global function genlab(ref strec d,int size=4)ref opndrec=
ref opndrec a

a:=newopnd(a_imm)
a^.size:=size
a^.labeldef:=d

return a
end

global function genmem(ref strec d,int size=4)ref opndrec=
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
symptr^.symbol:=symbol
symptr^.stindex:=0
symptr^.moduleno:=currmoduleno
adddef(symptr)
end

global proc createnamedconst(ref strec symptr,ref opndrec expr)=
symptr^.symbol:=namedconstsym
symptr^.expr:=expr
adddef(symptr)
end

global proc createregalias(ref strec symptr,int regindex, regsize)=
symptr.symbol:=kregsym
symptr.ksymbol:=kregsym
symptr.subcode:=regindex
symptr.regsize:=regsize

adddef(symptr)
end

global proc createxregalias(ref strec symptr,int regindex)=
symptr.symbol:=kxregsym
symptr.ksymbol:=kxregsym
symptr.subcode:=regindex

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

getstrint(a,&.str)
return &.str
end

function realtostr(real a)ichar=
static [64]char str
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

global proc buffercheck(ref dbuffer a,int n=1024)=
while a^.pend-a^.pcurr<n do
	bufferexpand(a)
od
end

global function bufferlength(ref dbuffer a)int=
return a^.pcurr-a^.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=

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

		fprint @&.str,"(# # (#) #) ",d.htindex:"6",d.name,
				moduletable[d.moduleno].name,d:"8H"

		d:=d^.nextdupl
	od
	println @f
od
println @f
end

=== ax_genss.m 27/31 ===
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

when m_leave then
	genbyte(0xC9)

when m_retn then
	if a^.mode<>a_imm then gerror("retn?") fi
	genbyte(0xC2)
	genword(a^.value)

when m_push then
	do_push(a)

when m_pop then
	do_pop(a)

when m_inc, m_dec then
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

when m_andpd,m_xorpd, m_pand, m_pxor then
	do_logicxmm(a,b,mclcodes[m^.opcode],8)

when m_pcmpistri,m_pcmpistrm then
	do_pcmpistri(a,b,m.c,mclcodes[m.opcode])

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
		if a^.valtype then		!was real
			gendword(getr32bits(a.xvalue))
		else
			gendword(a.value)
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
if d^.stindex=0 then
	if ss_nsymbols>=ss_symboltablesize then
		extendsymboltable()
	fi
	d^.stindex:=++ss_nsymbols
	ss_symboltable^[d^.stindex]:=d
fi
return d^.stindex
end

proc genrel32(ref opndrec a)=
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
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(3),rel32_rel)
	gendword(0)
else								!external symbol
	gendword(a^.value)				!this is probably just zero
	addrelocitem(rel32_rel,d)
esac
end

proc genabs32(ref opndrec a)=
ref strec d

d:=a^.labeldef


case d^.reftype
when back_ref then
	gendword(d^.offset+a^.value)
	addrelocitem(addr32_rel,d)

when fwd_ref then
	d^.fwdrefs:=addfwdref(d^.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
	gendword(a^.value)
	addrelocitem(addr32_rel,d)

else								!external symbol
	gendword(a^.value)				!this is probably just zero
	addrelocitem(addr32_rel,d)
esac
end

proc genabs64(ref opndrec a)=
ref strec d

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
		p32:=bufferelemptr(currdata,offset)

		p32^:=d^.offset-offset-4

	when addr32_rel,addr64_rel then
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

if a^.labeldef then return 4 fi
if a^.value or mand then
	return (isbytesized(a^.value)|1|4)
else
	return 0
fi
end

function genrm(ref opndrec a,int opc)int=
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
		return makeam(makemodrm(3,opc,code), sib, dispsize)

	when a_mem then

	when a_xreg then
		code:=getregcodebx(a^.reg)
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

	else									!assume regix used; optional reg and disp
		dispsize:=getdispsize(a,0)
		if dispsize then
			mode:=(dispsize=1|1|2)
		fi
		rm:=4

		scale:=(a^.scale|a^.scale|1)
		if reg=0 then
			base:=5
		else
			if reg in [rframe,r7] and dispsize=0 then
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
return s<<16+m<<8+d
end

proc do_arith(ref opndrec a,b,int code)=
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
		setopsize(a)
		opc:=code<<3 ior (a^.size=1|0x02|0x03)
		genrex()
		genbyte(opc)
		genamode(b,am)

	when a_imm then
doregimm::
		if b^.labeldef then
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
			when r5,r3,r14,r15 then
				rex ior:=0x40
			esac
			unless -128<=value<=255 then gerror("exceeding byte value") end
			genrex()
			genbyte(0xB0+regcode)
			genbyte(value)

		when 2 then
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
				unless -0x8000'0000<=value<=0xFFFF'FFFF then gerror("2:exceeding word32 value") end
			fi
			setopsize(a)
			genrex()
			genbyte(0xC7)
			genamode(a,am)
			genopnd(b,4)
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

int regcode

regcode:=reg-1
if regcode>=8 then
	regcode-:=8
	rex ior:=bmask
fi
return regcode
end

function getregcoderx(int reg)int=
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
	int am, regcode

	if a^.mode<>a_reg then gerror("movsx not reg") fi

	if a^.size=8 and b^.size=4 then
		if opc=0xBE then
			do_movsxd(a,b)
		else						!movsx 4->8 bytes, do normal move 4->4
			a:=regtable[a^.reg,4]
			do_mov(a,b)
		fi
		return
	fi

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
	checkhighreg(b)
	genrex()
	genbyte(0x0F)
	genbyte((b^.size=1|opc|opc+1))
	genamode(b,am)
end

proc checkhighreg(ref opndrec a)=
if a^.mode=a_reg then
	case a^.reg
	when r5,r3,r14,r15 then
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
int am

if (b^.mode<>a_reg and b^.reg<>a_mem) or b^.size>1 then gerror("setcc opnd/size") fi

am:=genrm(b,0)
checkhighreg(b)
genrex()
genbyte(0x0F)
genbyte(0x90+a^.value)
genamode(b,am)
end

proc do_movxmm(ref opndrec a,b,int size)=
int am, regcode, regcode1, regcode2

case a^.mode
when a_reg then
	case b^.mode
	when a_xreg then
		if a^.size<>size then gerror("1:movdq size") fi
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

if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	gerror("arithxmm opnds")
fi

if b^.mode=a_xreg then

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
	regcode:=getregcoderx(a.reg)
	am:=genrm(b,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(opc)
	genamode(b,am)
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
int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	gerror("convertfloat opnds")
fi

genbyte(prefix)

if a^.mode=a_xreg then
	regcode:=getregcodeRx(a^.reg)
	am:=genrm(b,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(0x5A)
	genamode(b,am)
else
	regcode:=getregcoderx(b^.reg)
	am:=genrm(a,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(0x5A)
	genamode(b,am)
fi
end

proc do_fix(ref opndrec a,b,int prefix,opc)=
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
int am, regcode, mf

if a^.mode<>a_mem then
	gerror("fmem/not mem")
fi

if freal then
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
end

function getr32bits(real x)int=
real32 sx:=x
return int32@(sx)
end

proc genrel8(ref opndrec a)=
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
int n


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

proc do_pcmpistri(ref opndrec a,b,int c,opc)=
int am, regcode

if a^.mode<>a_xreg or (b^.mode<>a_xreg and b^.mode<>a_mem) then
	gerror("pcmpistrx opnds")
fi

genbyte(0x66)

if b^.mode=a_xreg then
	swap(a,b)
	regcode:=getregcoderx(b^.reg)
	am:=genrm(a,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(0x3A)
	genbyte(opc)
	genamode(a,am)
else
	regcode:=getregcoderx(a^.reg)
	am:=genrm(b,regcode)
	genrex()
	genbyte(0x0F)
	genbyte(0x3A)
	genbyte(opc)
	genamode(b,am)
fi

genbyte(c)

end

=== ax_objdecls.m 28/31 ===
import ax_decls

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

global record importrec = 		!details about all imported symbols
	ref strec def				!full st entry
	int libno					!which dll lib this belongs to
	ichar name					!name of symbol (extracted from lib.name if needed)
	int hintnameoffset			!voffset of hint/name entry in impdir section
	int iatoffset				!voffset of IAT entry
	int thunkoffset				!offset within code section of thunk entry
end

global record exportrec = 		!details about all exported symbols
	ref strec def				!full st entry
	ichar name					!name of symbol (extracted from lib.name if needed)
end

global record dllrec =			!all imported libraries
	ichar name					!name of library, including .dll
	int nprocs					!no. of imports which use this library
	int nametableoffset			!start of name table in impdir
	int addrtableoffset			!start of addr table (IAT)
	int dllnameoffset			!offset of name within impdir
	int dllextraoffset			!offset of mysterious region just before the name
end

global record exportdirrec =
	word32 exportflags
	word32 timedatestamp
	word16 majorversion
	word16 minorversion
	word32 namerva
	word32 ordinalbase
	word32 naddrtable
	word32 nnamepointers
	word32 expaddressrva
	word32 namepointerrva
	word32 ordtablerva
end

=== ax_writeexe.m 29/31 ===

import clib
import mlib
import oslib
import ax_objdecls
import ax_tables
import ax_decls
import ax_lib
import ax_disasm

[maxsearchlibs]int64 libinsttable
[maxsearchlibs]ichar libinstnames
[maxsearchlibs]int libnotable			!index into dlltable

global const zsect=3
global const dsect=2
global const csect=1
global const isect=4

record basereloc =
	ref basereloc nextitem
	word32 address				!virtual address
	int32 reloctype
end

ref basereloc basereloclist
int nbaserelocs
int maxrelocaddr
const maxbaseblock=500
[maxbaseblock]int blockbases
[maxbaseblock]int32 blockcounts
[maxbaseblock]int32 blockbytes
[maxbaseblock]byte blockpadding
int nbaseblocks
int basetablesize

const filealign = 512
const sectionalign = 4096
const exe_imagebase = 0x40'0000
const dll_imagebase = 0x6624'0000
global word imagebase

int imagesize
int filesize
ref[]int64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
ref strec stentrypoint				!symbol to be the entry point
ref strec stentrypoint2
ref strec stentrypoint3

const maxsection = 10
global [maxsection]sectionrec sectiontable
global int nsections

ref byte importdir				!allowed section data for import directort in .idata

global const maximports = 3000
global [maximports]importrec importtable
global int nimports

global const maxexports = 2000
global [maxexports]exportrec exporttable
global int nexports
ichar dllfilename
int isdll

global const maxlibs = 50
global [maxlibs]dllrec dlltable
global int ndlls

global ref byte datastart
global ref byte dataptr
global ichar userentrypoint

int exportdirvirtaddr
int exportdirvirtsize
int exportdiroffset				!from start of imp dir

int blockdirvirtaddr
int blockdirvirtsize
int blockdiroffset


global proc writeexe(ichar outfile, int dodll)=
	imagefileheader header
	optionalheader optheader
	int offset,i
	int64 aa

	dllfilename:=outfile
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


	if fverbose then
		CPL "Writing file:",outfile
	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

global proc genexe(ichar entrypoint, outfile, int dodll)=

	dllfilename:=outfile
	isdll:=dodll

	imagebase:=(isdll|dll_imagebase|exe_imagebase)

	userentrypoint:=entrypoint
	loadlibs()
	scanst()				!build dll/import tables

	getoffsets()
	relocdata(&sectiontable[csect])
	relocdata(&sectiontable[dsect])
end

proc loadlibs=
	int i
	int64 hinst
	ichar file
	[300]char filename

	for i to nsearchlibs do
		strcpy(&.filename,searchlibs[i])
		strcat(&.filename,".dll")
		hinst:=os_getdllinst(&.filename)
		if hinst=0 then
			cpl searchlibs[i]
			cpl &.FILENAME
			gerror("Can't load search lib")
		fi
		libinsttable[i]:=hinst
		libinstnames[i]:=pcm_copyheapstring(&.filename)
	od
end

global proc initsectiontable=

	sectiontable[csect].name:=".text"
	sectiontable[csect].segtype:=code_seg
	sectiontable[csect].data:=ss_code
	sectiontable[csect].virtsize:=bufferlength(ss_code)

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

	sectiontable[csect].rawsize:=roundtoblock(sectiontable[csect].virtsize,filealign)
	sectiontable[csect].nrelocs:=ss_ncoderelocs
	sectiontable[csect].relocs:=ss_coderelocs

	sectiontable[isect].name:=".idata"
	sectiontable[isect].segtype:=impdata_seg
	sectiontable[isect].virtsize:=0
	sectiontable[isect].rawsize:=0

	nsections:=4
end

function roundtoblock(int n,align)int=
	if n iand (align-1)=0 then return n fi

	return n+(align-(n iand (align-1)))
end

function extractlibname(ichar name, int &libno,moduleno)ichar=
	ref char s,name2
	[256]char str
	[256]char str2
	int i

	name2:=nil

	reenter::
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
			if ndlls>=maxlibs then gerror("Too many libs") fi
			libno:=++ndlls

			dlltable[libno].name:=pcm_copyheapstring(&.str)
			dlltable[libno].nprocs:=1
			return (name2|name2|s+1)
		fi

		++s
	od

	int n

	for i:=1 to nsearchlibs do
		if os_getdllprocaddr(libinsttable[i],name) then
			n:=i
			exit				!don't need the actual address; just whether it exists
		fi
	else
		println name,moduletable[moduleno].name
		gerror("Can't find external function")
	od

	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	fi

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

	int i,libno
	ref strec d
	ichar name, libname

	for i:=1 to ss_nsymbols do

		d:=ss_symboltable^[i]
		case d^.symbol
		when importedsym then
			if nimports>=maximports then gerror("genexe: Too many imports") fi
			++nimports

			name:=extractlibname(d^.name,libno,d^.moduleno)

			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d^.importindex:=nimports
		when exportedsym then
			if userentrypoint then
				if eqstring(d^.name,userentrypoint) then
					stentrypoint:=d
				fi
			else
				if eqstring(d^.name,"main") and not isdll then
					stentrypoint:=d
				elsif eqstring(d^.name,"start") and not isdll then
					stentrypoint2:=d
				elsif eqstring(d^.name,"dllmain") and isdll then
					stentrypoint:=d
				fi
			fi

			if nexports>=maxexports then gerror("gendll: Too many exports") fi
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=d.name

		esac
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref word32 p32
	ref word64 p64
	ref strec d
	word thunkoffset
	int offset,index,iatoffset

	p:=bufferelemptr(s^.data,0)
	r:=s^.relocs

	while r do
		d:=ss_symboltable^[r^.stindex]
		index:=d^.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset

		case r^.reloctype
		when rel32_rel then
			if d^.symbol<>importedsym then
				gerror("rel32/not imported")
			fi
			(ref word32(p+r^.offset)^:=int(thunkoffset)-r^.offset-4)

		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d^.symbol=importedsym then

				(ref word32(p+r^.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				case d^.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				esac

				p32:=cast(p+r^.offset)
IF R.RELOCTYPE=ADDR32_REL THEN

				p32^:=p32^+u^.virtoffset+imagebase
ELSE
				P64:=cast(P32)
				p64^:=p64^+u^.virtoffset+imagebase
fi
			fi
		else
			cpl relocnames[r^.reloctype]
			gerror("Can't do this rel type")
		esac

		r:=r^.nextreloc
	od

end

proc getbaserelocs(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref strec d
	int index

	p:=bufferelemptr(s^.data,0)
	r:=s^.relocs

	while r do
		d:=ss_symboltable^[r^.stindex]

		case r^.reloctype
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d^.symbol=importedsym then
			else
				case d^.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				esac

				newbasereloc(u.virtoffset+r.offset, r.reloctype)

			fi
		esac

		r:=r^.nextreloc
	od

end

proc writerecordx(ref void r, int length)=
	memcpy(dataptr,r,length)
	dataptr+:=length
end

proc writedosstub=
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
	dataptr:=datastart+offset			!data will have been cleared
end

proc writefileheader=
	imagefileheader header

	memset(&header,0,header.bytes)

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
			if not isdll then
				gerror("Entry point not found: main or start")
			fi
		fi
	else
		header.entrypoint:=sectiontable[csect].virtoffset+stentrypoint^.offset
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

	memset(&sheader,0,sheader.bytes)

	strcpy(&sheader.name[1],s^.name)
	sheader.virtual_size:=s^.virtsize
	sheader.virtual_address:=s^.virtoffset
	sheader.rawdata_offset:=s^.rawoffset
	sheader.rawdata_size:=s^.rawsize

	int64 aa
	case s^.segtype
	when zdata_seg then
		sheader.characteristics:=0xC050'0080
	when idata_seg then
		sheader.characteristics:=0xC050'0040
	when code_seg then
		sheader.characteristics:=0x6050'0020
	when impdata_seg then
		sheader.characteristics:=0x4030'0040
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
	else
		writerecordx(bufferelemptr(s^.data,0),s^.rawsize)
	esac
end

proc getoffsets=
	int fileoffset, imageoffset,i,diroffset,impdirno,hinttableoffset,j,n
	int codesize,length,thunkoffset,offset,dirstartoffset

	fileoffset:=128+4+imagefileheader.bytes+optionalheader.bytes	!dosstub+sig
	fileoffset+:=imagesectionheader.bytes*nsections

	fileoffset:=roundtoblock(fileoffset,filealign)
	imageoffset:=4096

	ref byte pcode
	codesize:=sectiontable[csect].virtsize
	pcode:=bufferelemptr(ss_code,codesize)
	while codesize iand 7 do pcode++^:=0x90; ++codesize od
	thunkoffset:=codesize
	codesize+:=nimports*8

	sectiontable[csect].virtsize:=codesize
	sectiontable[csect].rawsize:=roundtoblock(codesize,filealign)

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


	diroffset+:=(ndlls+1)*importdirrec.bytes			!need blank entry as terminator


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

	hinttableoffset:=diroffset
	for i to nimports do
		length:=strlen(importtable[i].name)+3
		if length iand 1 then ++length fi		!keep even
		importtable[i].hintnameoffset:=diroffset
		diroffset+:=length
	od


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

	sectiontable[impdirno].virtsize:=offset
	sectiontable[impdirno].rawsize:=roundtoblock(offset,filealign)
	filesize:=roundtoblock(fileoffset+offset,filealign)

	imagesize:=roundtoblock(imageoffset+(diroffset-dirstartoffset),sectionalign)

	ref byte pimpdir

	pimpdir:=sectiontable[impdirno].bytedata:=pcm_allocz(offset)

	ref importdirrec pdir
	ref int64 paddr,pname
	int iatoffset
	pdir:=cast(pimpdir)

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

	ref byte phint
	ref word32 pextra

	for i to nimports do
		phint:=pimpdir+importtable[i].hintnameoffset-dirstartoffset
		phint+:=2					!leave hint as 0
		strcpy(cast(phint),importtable[i].name)
	od
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

function getsectionno(int segment)int=
	case segment
	when zdata_seg then zsect
	when idata_seg then dsect
	when code_seg then csect
	else gerror("GSN"); 0
	esac
end

proc writeexporttable(ref byte pstart)=
	[maxexports]int sortindex
	ref exportdirrec phdr := cast(pstart)
	ref word32 paddrtable
	ref word32 pnametable
	ref word16 pordtable
	ref char pdllname
	ref char pnames
	int addrtableoffset
	int nametableoffset
	int ordtableoffset
	int dllnameoffset
	int namesoffset
	int virtoffset
	int sectionno
	ref strec d

	phdr.timedatestamp:=0x5f89f4f8

	phdr.ordinalbase:=1
	phdr.naddrtable:=nexports
	phdr.nnamepointers:=nexports

	addrtableoffset:=exportdirrec.bytes
	nametableoffset:=addrtableoffset+nexports*4
	ordtableoffset:=nametableoffset+nexports*4
	dllnameoffset:=ordtableoffset+nexports*2
	namesoffset:=dllnameoffset+strlen(dllfilename)+1

	virtoffset:=sectiontable[isect].virtoffset+exportdiroffset

	paddrtable:=cast(pstart+addrtableoffset)
	pnametable:=cast(pstart+nametableoffset)
	pordtable:=cast(pstart+ordtableoffset)
	pdllname:=cast(pstart+dllnameoffset)
	pnames:=cast(pstart+namesoffset)

	phdr.namerva:=dllnameoffset+virtoffset
	phdr.expaddressrva:=addrtableoffset+virtoffset
	phdr.namepointerrva:=nametableoffset+virtoffset
	phdr.ordtablerva:=ordtableoffset+virtoffset

	strcpy(pdllname,dllfilename)

	if nexports>maxexports then
		gerror("Too many exports - can't sort")
	fi

	sortexports(sortindex)

	for i to nexports do
		d:=exporttable[sortindex[i]].def
		sectionno:=getsectionno(d.segment)

		strcpy(pnames,d.name)
		pnametable^:=namesoffset+virtoffset
		++pnametable
		namesoffset+:=strlen(d.name)+1
		pnames+:=strlen(d.name)+1

		paddrtable^:=d.offset+sectiontable[sectionno].virtoffset
		++paddrtable
		pordtable^:=i-1
		++pordtable
	od


end

function getexporttablesize:int=
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

	p:=pcm_allocz(basereloc.bytes)
	p.address:=addr
	p.reloctype:=reltype

	p.nextitem:=basereloclist

	basereloclist:=p
	++nbaserelocs
	maxrelocaddr max:=addr

end

proc scanbaserelocs=
	int baseaddr,addr,nextblock
	ref basereloc p

	baseaddr:=0x1000
	nbaseblocks:=0

	repeat
		nextblock:=baseaddr+0x1000
		if nbaseblocks>=maxbaseblock then gerror("Too many blocks") fi
		++nbaseblocks
		blockbases[nbaseblocks]:=baseaddr
		blockcounts[nbaseblocks]:=0


		p:=basereloclist
		while p do
			addr:=p.address
			if addr>=baseaddr and addr<nextblock then
				++blockcounts[nbaseblocks]
			fi

			p:=p.nextitem
		od

		baseaddr:=nextblock
	until baseaddr>maxrelocaddr

	for i to nbaseblocks when blockcounts[i] do
		if blockcounts[i] iand 1 then
			++blockcounts[i]
			++blockpadding[i]
		fi
		blockbytes[i]:=blockcounts[i]*2+8
		basetablesize+:=blockbytes[i]
	od
end

proc writebasereloctable(ref byte pstart)=
	
	ref word32 p32
	ref word16 p16
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
			q:=q.nextitem
		od
		if blockpadding[i] then p16++^:=0 fi

		p32:=cast(p16)

	od
end

proc sortexports([]int &sortindex)=
	ref strec d,e
	for i to nexports do
		sortindex[i]:=i
	od

	int swapped

	repeat
		swapped:=0
		for i:=1 to nexports-1 do

			d:=exporttable[sortindex[i]].def
			e:=exporttable[sortindex[i+1]].def

			if strcmp(d.name, e.name)>0 then
				swapped:=1
				swap(sortindex[i], sortindex[i+1])
			fi
		od
	until not swapped

end
=== ax_disasm.m 30/31 ===
import clib
import msys
import oslib

const showmregs=0

const halt=0xF4

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

[0:]ichar condnames = 
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

ref byte codeptr

global function decodeinstr(ref byte &cptr,baseaddr=nil)ichar=
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


	if baseaddr then
		print @&.str,baseaddr:"z6h",,": "
	else
		print @&.str,pstart:"z6h",,": "
	fi

	n:=codeptr-pstart
	to n do
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
	int opc,rhssize,third,imm
	ichar opcstr

	switch opc:=codeptr++^
	when 0x2A then					!cvtsi2ss/sd XMM, REG/MEM
		decodeaddr(1)
		if f3override then
			genstr("cvtsi2ss ")
		else
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

	when 0x3A then					!possible pcmpistri
		third:=codeptr++^

		case third
		when 0x63 then
			genstr("pcmpistri ")
		when 0x62 then
			genstr("pcmpistrm ")
		else
			genstr("Unknown opcode 2-byte opcode: 0F ")
		    genhex(opc)
			return
		esac

		decodeaddr(1)
		genstr(strxmm(rmreg))
		genstr(", ")
		printaddrmode(1)
		genstr(", ")
		imm:=codeptr++^
		genintd(imm)

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
		printaddrmode()

	when 0xBC, 0xBD then
		decodeaddr(1)
		genstr((opc=0xBC|"bsf "|"bsr "))
		genstr(strreg(rmreg,opsize))
		genstr(", ")
		printaddrmode()

	when 0xD6 then
		decodeaddr(1)
		opsize:=8
		genstr("movq ")
		printaddrmode(1)
		genstr(",")
		genstr(strxmm(rmreg))	

	when 0xDB then					!PAND
		decodeaddr(1)
		genstr("pand ")
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=8	!(sizeoverride|8|4)
		printaddrmode(1)

	when 0xEF then					!PXOR
		decodeaddr(1)
		genstr("pxor ")
		genstr(strxmm(rmreg))
		genstr(", ")
		opsize:=8	!(sizeoverride|8|4)
		printaddrmode(1)


	else
	error::
		genstr("Unknown opcode 2-byte opcode: 0F ")
    genhex(opc)
	endswitch
end

proc decodeaddr(int w=0)=
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

	if mode=3 then		!plain register access
		basereg:=rm+1
		addrmode:=amreg
	elsif rm<>4 then				!not esp; no sib
		if mode=0 and rm=5 then		![ebp] is actually [rip+disp]
			offset:=readint32()		!
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

		if mode=0 and basereg=rframe and indexreg=rstack then	!no base/index regs, only d32 disp
			indexreg:=basereg:=0
			offset:=readint32()

		elsif mode=0 and basereg=rframe  then	!no base/index regs, only d32 disp
			basereg:=0
			offset:=readint32()

		elsif mode=0 and indexreg=rstack then	!no index register, only base; no disp
			indexreg:=0

		else
			case mode
			when 1 then
				offset:=readsbyte()
			when 2 then
				offset:=readint32()
			esac
			if indexreg=rstack then				!stack means no index reg
				indexreg:=0
			fi
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

global function strreg(int reg,opsize)ichar=
static []ichar regnames8=("al","cl","dl","bl","spl","bpl","sil","dil",
						"r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b",
				"ah","bh","ch","dh")

static []ichar regnames16=("ax","cx","dx","bx","sp","bp","si","di",
						"r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w")

static []ichar regnames32=("eax","ecx","edx","ebx","esp","ebp","esi","edi",
						"r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d")

static []ichar regnames64=("rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
						"r8","r9","r10","r11","r12","r13","r14","r15")

static []ichar mregnames8=("B0","B10","B11","B4","B15","B14","B5","B3",
						"B12","B13","B1","B2","B6","B7","B8","B9",
					"B16","B18","B19","B17")

static []ichar mregnames16=("W0","W10","W11","W4","Wsp","Wbp","W5","W3",
						"W12","W13","W1","W2","W6","W7","W8","W9")

static []ichar mregnames32=("A0","A10","A11","A4","Astack","Aframe","A5","A3",
						"A12","A13","A1","A2","A6","A7","A8","A9")

static []ichar mregnames64=("D0","D10","D11","D4","Dstack","Dframe","D5","D3",
						"D12","D13","D1","D2","D6","D7","D8","D9")

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
	genstr(strint(a))
end

proc genhex(int64 a)=
	genstr(strint(a,"h"))
end

function readimm:int=

	case opsize
	when 1 then return readsbyte()
	when 2 then return readint16()
	when 4,8 then return readint32()			!64-bit uses 32-bit immediate
	esac
	return 0
end

function readimm8:int64=
	if opsize<8 then return readimm() fi
	return readint64()
end

function strxmm(int reg)ichar=
	static [32]char str
	print @&.str,"xmm",,reg-1
	return &.str
end

function strmmx(int reg)ichar=
	static [32]char str

	print @&.str,"mmx",,reg-1
	return &.str
end

proc decode8087(int ttt)=
	byte bb
	int longopc,freg,shortopc,code

	bb:=codeptr++^			!following byte

	longopc:=ttt<<8+bb		!bottom 11 bits of 2-bytes opcode
	freg:=(bb iand 7)+1		!where bb specifies a register in bottom 3 bits


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
	if opsize=1 and not rex and reg>=5 and reg<=8 then
		reg+:=12				!5..8 => 17..20
	fi
end

proc getsilx(int &reg)=
	if addrmode=amreg and opsize=1 and rex=0 and reg>=5 and reg<=8 then
		reg+:=12				!5..8 => 17..20
	fi
end
=== ax_writeobj.m 31/31 ===

import clib
import mlib
import ax_objdecls
import ax_decls
import ax_tables
import ax_lib

int symtaboffset

ref byte datastart
ref byte dataptr

[0..10'000]imagesymbol symboltable

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

	strcpy(&zsection.name[1],".bss")
	zsection.rawdata_size:=ss_zdatalen

	zsection.characteristics:=0xC040'0080

	if ss_nidatarelocs>=65536 or ss_ncoderelocs>=65536 then
		gerror("Too many relocs (exceeds 16-bit field)")
	fi

	strcpy(&isection.name[1],".data")
	isection.rawdata_size:=bufferlength(ss_idata)
	isection.nrelocs:=ss_nidatarelocs

	isection.characteristics:=0xC050'0040

	strcpy(&csection.name[1],".text")
	csection.rawdata_size:=bufferlength(ss_code)
	csection.nrelocs:=ss_ncoderelocs

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


	header.symtaboffset:=offset
	offset+:=nsymbols*imagesymbol.bytes
	header.nsymbols:=nsymbols

	offset+:=nextstringoffset

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

=== end ===
