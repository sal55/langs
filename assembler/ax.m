import msys
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

global int logdest=0			!no diagnostic output

byte fshowmcl
byte fshowss
byte fshowsx
byte fshowtiming

tabledata() []ichar optionnames=
	(lex_sw,		"lex"),		!first four must be in this order
	(parse_sw,		"parse"),
	(gen_sw,		"gen"),
	(obj_sw,		"obj"),
	(exe_sw,		"exe"),

	(mcl_sw,		"mcl"),
	(ss_sw,			"ss"),
	(sx_sw,			"sx"),
	(time_sw,		"time"),
	(s_sw,			"s"),
	(d_sw,			"d"),
	(v_sw,			"v"),
	(q_sw,			"q"),
	(help_sw,		"help"),
	(out_sw,		"out"),
	(main_sw,		"main"),
	(start_sw,		"start"),
end

int axlevel = exe_sw

const logfile = "mx.log"

ichar inputfile
ichar outputfile

proc start=
ref strbuffer ss
int ntokens,t,i,U

T:=CLOCK()
initall()

getinputoptions()

inputfile:=moduletable[1].filename

initlogfile()

if axlevel=lex_sw then
	if nmodules>1 then loaderror("lex test/multi files") fi
	lextest(inputfile)
else
	if outputfile=nil then
		outputfile:=pcm_copyheapstring(changeext(inputfile,(axlevel=exe_sw|"exe"|"obj")))
	fi

	if not fquiet then
		if axlevel=obj_sw then
			println "Assembling",inputfile,"to",outputfile
		else
			println "Assembling/linking to",outputfile
		fi
	fi

	if fverbose then
		showcaption()
		println
	fi
	loadsourcefiles()
	parsemodules()

	case axlevel
	when obj_sw then
		genss()
		if fshowss or fshowsx then
			initsectiontable()					!need for display
			ss:=writessdata(0)
			gs_println(ss,logdev)
		fi

		writess(outputfile)
	when exe_sw then
		genss()
		initsectiontable()
		if fshowss then
			ss:=writessdata(0)
			gs_println(ss,logdev)
		fi

		genexe(nil)
		if fshowsx then
			ss:=writessdata(1)
			gs_println(ss,logdev)
		fi

		writeexe(outputfile)
	esac

	if fshowmcl then
		ss:=writemclblock()
		gs_println(ss,logdev)
	fi
fi

if fshowtiming then
	T:=CLOCK()-T
	CPL "Time",T
fi

closelogfile()
stop 0
end

proc loadsourcefiles=
	int i
	ichar source

	for i to nmodules do
		source:=cast(readfile(moduletable[i].filename))
		if source=nil then
			loaderror_s("Can't load file: %s",moduletable[i].filename)
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
		if fshowsx then
!			printmodulesymbols(logdev)
		fi
if i<>nmodules then
		resethashtable()
fi
	od

if fshowsx then
!	printimportsymbols(logdev)
!	printdupltable(logdev)
fi

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

proc initlogfile=
case logdest
when 2 then
	remove(logfile)
	logdev:=cast(fopen(logfile,"w"))
when 0,1 then
	logdev:=nil
esac

end

proc closelogfile=			!CLOSELOGFILE
[512]char str

if logdest=2 then
	fclose(logdev)

	print @&.str,f"\m\ed.bat",logfile

	os_execwait(&.str,1,nil)
fi
end

proc initall=
pcm_init()
initlex()
initlib()
end

proc lextest(ichar file)=
	loadsourcefiles()
	initsourcefile(moduletable[1].source)

	lxsymbol:=eolsym
	while lxsymbol<>eofsym do
		lex()
	od
end

proc getinputoptions=
const slash='-'
int i,j,k
int paramno,pmtype,sw
ichar name,value,ext

paramno:=2

while pmtype:=nextcmdparam(paramno,name,value,".asm") do
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
		addmodule(name)
	when pm_libfile then
		addsearchlib(name)
	esac
od

if nmodules=0 and nsearchlibs=0 then
	showcaption()
	println
	println "Usage:"
	println "	",,sysparams[1],"filename[.asm]           # Assemble filename.asm to filename.obj"
	println "	",,sysparams[1],"-help                    # Show options"
	stop 1
fi

if fshowss or fshowsx or fshowmcl then
	if logdest=0 then logdest:=2 fi
fi

if nsearchlibs=0 then
	searchlibs[1]:="msvcrt"
	searchlibs[2]:="gdi32"
	searchlibs[3]:="user32"
	searchlibs[4]:="kernel32"
	nsearchlibs:=4	
fi
if nmodules=0 then
	loaderror("No input files specified")
fi
end

proc do_option(int sw, ichar value)=

case sw
when lex_sw,parse_sw,gen_sw,obj_sw,exe_sw then
	axlevel:=sw
when mcl_sw then
	fshowmcl:=1
when ss_sw then
	fshowss:=1
when sx_sw then
	fshowsx:=1
when time_sw then
	fshowtiming:=1
when s_sw then
	logdest:=1
when d_sw then
	logdest:=2
when v_sw then
	fverbose:=1
when q_sw then
	fquiet:=1
when help_sw then
	showhelp()
when out_sw then
	outputfile:=pcm_copyheapstring(value)
when main_sw then
when start_sw then
!	entrypointname:="start"
esac

end

proc showhelp=
showcaption()
println
println strinclude "help.txt"
stop 1
end

proc showcaption=
print "AX Assembler/Linker",$date
end

proc loaderror(ichar mess)=
println "Error:",mess
stop 1
end

proc loaderror_s(ichar mess,s)=
[256]char str
sprintf(&.str,mess,s)
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

proc showmodules=
	int i
	println "Modules:",nmodules
	for i:=1 to nmodules do
		println "  ",i,,":",
			padstr(moduletable[i].name,13),
			padstr(moduletable[i].filename,25),
			strlen(moduletable[i].source)
	od
	println
	println "Search Libs:",nsearchlibs
	for i:=1 to nsearchlibs do
		println "  ",i,,":",searchlibs[i]
	od
	println
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
