mafile 15
  1 ax.m                9608      675   0
  2 msysnew.m          46919    10307   0
  3 clibnew.m           3397    57250   0
  4 mlib.m             26695    60668   0
  5 oswindows.m        12536    87389   0
  6 ax_tables.m        12658    99951   0
  7 ax_decls.m          5554   112634   0
  8 ax_lex.m           13275   118211   0
  9 ax_parse.m          8887   131511   0
 10 ax_lib.m           16275   140422   0
 11 ax_genss.m         41023   156723   0
 12 ax_objdecls.m       2566   197775   0
 13 ax_writeexe.m      24274   200370   0
 14 ax_disasm.m        26006   224671   0
 15 ax_writeobj.m       7418   250706   0
=== ax.m 1/15 ===
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
!U:=CLOCK();CPL "DONE PARSING",U-T;T:=U

!CPL "AX1"
	case axlevel
	when obj_sw then
!CPL "AX2"
		genss()
!CPL "AX3"
!U:=CLOCK();CPL "DONE GENSS",U-T;T:=U
		if fshowss or fshowsx then
			initsectiontable()					!need for display
			ss:=writessdata(0)
			gs_println(ss,logdev)
		fi

		writess(outputfile)
	when exe_sw then
		genss()
!U:=CLOCK();CPL "DONE GENSS",U-T;T:=U
		initsectiontable()
		if fshowss then
			ss:=writessdata(0)
			gs_println(ss,logdev)
		fi

		genexe(nil)
!U:=CLOCK();CPL "DONE GENEXE",U-T;T:=U
		if fshowsx then
			ss:=writessdata(1)
			gs_println(ss,logdev)
		fi

		writeexe(outputfile)
!U:=CLOCK();CPL "DONE WRITEEXE",U-T;T:=U
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

!	sprintf(&.str,f"\m\ed.bat %s",logfile)
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
println "HELP TEXT..."
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
=== msysnew.m 2/15 ===
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
=== clibnew.m 3/15 ===
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
=== mlib.m 4/15 ===
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
=== oswindows.m 5/15 ===
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
=== ax_tables.m 6/15 ===
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
=== ax_decls.m 7/15 ===
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
=== ax_lex.m 8/15 ===
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
=== ax_parse.m 9/15 ===
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
=== ax_lib.m 10/15 ===
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

=== ax_genss.m 11/15 ===
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
=== ax_objdecls.m 12/15 ===

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
=== ax_writeexe.m 13/15 ===
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

=== ax_disasm.m 14/15 ===
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
=== ax_writeobj.m 15/15 ===
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

=== end ===
