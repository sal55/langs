mafile 32
  1 qq.m               19548     1406   0
  2 mclib.m             3502    20976   0
  3 msyslib.m          35263    24502   0
  4 mlibnew.m          26375    59789   0
  5 mwindows.m         13091    86189   0
  6 qq_lex.m           33875    99303   0
  7 qq_decls.m         14905   133203   0
  8 qq_tables.m        49813   148134   0
  9 qq_pclgen.m        54791   197973   0
 10 qq_pcllib.m        10283   252791   0
 11 qq_parse.m         63138   263100   0
 12 qq_resolve.m       25260   326266   0
 13 qq_lib.m           25761   351550   0
 14 qq_handlers.m      43643   377340   0
 15 qq_names.m         16759   421009   0
 16 qq_modules.m        6464   437796   0
 17 qq_strings.m       14166   444288   0
 18 qq_print.m         41804   458480   0
 19 qq_show.m          18604   500309   0
 20 qq_host.m          22742   518938   0
 21 qq_vars.m          43477   541705   0
 22 qq_lists.m          8216   585208   0
 23 qq_dicts.m          4780   593450   0
 24 qq_sets.m           7395   598255   0
 25 qq_records.m        3417   605678   0
 26 qq_arrays.m         7721   609122   0
 27 qq_packed.m         8105   616870   0
 28 qq_bits.m           8877   625000   0
 29 qq_decimal.m         681   633905   0
 30 mbignum.m          29711   634611   0
 31 qq_calldll.m        3750   664350   0
 32 mwindll.m           2115   668125   0
=== qq.m 1/32 ===
!mapmodule qq_show => qq_dummyshow

import clib
import msys
import mlib
import oslib
import qq_lex
import qq_decls
import qq_tables

import qq_pclgen
import qq_pcllib

import qq_parse
import qq_resolve
import qq_lib
import qq_handlers
import qq_names
import qq_modules
import qq_strings
import qq_print
import qq_show
import qq_host
import qq_vars
import qq_lists
import qq_dicts
import qq_sets
import qq_records
import qq_arrays
import qq_packed
import qq_bits
import qq_decimal
import qq_calldll

!import qq_newvars

!import qq_hostlib

global tabledata() []ichar runnames =
	(load_cc,		$),
	(parse_cc,		$),
	(names_cc,		$),
	(gencode_cc,	$),
	(fixup_cc,		$),
	(run_cc,		$),
end

tabledata() []ichar optionnames=
	(load_sw,		"load"),
	(parse_sw,		"parse"),
	(names_sw,		"names"),
	(gen_sw,		"gen"),
	(fixup_sw,		"fixup"),
	(run_sw,		"run"),

	(ast1_sw,		"ast1"),
	(ast2_sw,		"ast2"),
	(pcl1_sw,		"pcl1"),
	(pcl2_sw,		"pcl2"),
	(st_sw,			"st"),
	(types_sw,		"types"),

	(fn_sw,			"fn"),
	(sw_sw,			"sw"),
	(asm_sw,		"asm"),
	(debug_sw,		"debug"),
	(fdebug_sw,		"fdebug"),
end

global byte runcode  = run_cc

var byte fshowpcl1
var byte fshowpcl2
var byte fshowast1
var byte fshowast2
var byte fshowst
var byte fshowtypes

const logfile="qq.log"

var ichar inputfile

var ref strbuffer pclstr

global const maxstatic=11000
global [maxstatic]variant statictable
global [maxstatic]symbol staticdefs
global int nstatics

global const maxproc=11000
global [maxproc]ref int proctable
global [maxproc]symbol procdefs
global int nprocs

proc start=
	var ichar source
	var int i,nnames,t
	var unit p

!CPL "HI THERE"
!CPL =OBJREC.BYTES
!CPL =STREC.BYTES
!CPL =UNITREC.BYTES
!CPL =LEXREC.BYTES
!CPL =PARAMREC.BYTES

	initprogram()

	getinputoptions()

INT U
	load_program(inputfile)

T:=CLOCK()
	parse_program()
	rx_program()
	gx_program()
!cpl "SS4"
	fixup_program()
!cpl "SS5"
!T:=CLOCK()-T
!CPL "Internal compile time=",T

	run_program()

	showlogfile()

!CPL =NALLUNITS

end

proc load_program(ichar file)=
	ichar source

	source:=loadmainmodule(inputfile)

	if not source then
		loaderror_s("Can't load",inputfile)
	fi
end

proc parse_program=
	int m

	return when runcode<parse_cc

	m:=1

!note that 'nmodules' can grow during the loop, as more new imports are encountered
	repeat
		parsemodule(m)
		++m
	until m>nmodules

!	fixusertypes()

	if fshowast1 then
		showast("AST1")
	fi

end

proc rx_program=
	return when runcode<names_cc

	fixusertypes()
	tx_typetable()

	for i to nmodules do
		rx_module(i)
	od


	if fshowast2 then
		showast("AST2")
	fi
end

proc gx_program=
	return when runcode<gencode_cc

!	if fshowast and runcode<run_cc then
!		pcldev:=fopen("PCL","w")
!	fi
!
	for i:=1 to nmodules do
		gencodemodule(i)
	od

	if fshowpcl1 then
		showpcl(1)
	fi
end

proc fixup_program=
	return when runcode<fixup_cc

	inithandlers()

	for i to nmodules do
		fixupmodule(i)
	od

	if fshowpcl2 then
		showpcl(2)
	fi
end

proc showlogfile=
	var [256]char str
	var filehandle logdev

	if fshowpcl1+fshowpcl2+fshowast1+fshowast2+fshowst+fshowtypes=0 then return fi
	if runcode=run_cc then
		return
	fi

	if fshowst then
		showsttree()
	fi

	if fshowtypes then
		showtypes()
	fi

	logdev:=fopen(logfile,"w")

	if runcode>=fixup_cc and fshowpcl2 then addtolog("PCL2",logdev) fi
	if runcode>=gencode_cc and fshowpcl1 then addtolog("PCL1",logdev) fi
	if runcode>=names_cc and fshowast2 then addtolog("AST2",logdev) fi
	if runcode>=parse_cc and fshowast1 then addtolog("AST1",logdev) fi
	if fshowst then addtolog("ST",logdev) fi
	if fshowtypes then addtolog("TYPES",logdev) fi
	fclose(logdev)

	fprint @&.str,"c:/m/med.bat #",logfile
!CPL =&.STR

	os_execwait(&.str,1,nil)

end

proc showsttree=
	var filehandle f
	var ref modulerec m
	symbol d
	ref genfieldrec g
	ref procrec p

	return unless fshowst

	f:=fopen("ST","w")
	printglobalsymbols(f)
	printglobalsymbols_full(f)

	println @f
	println @f,"Modules",nmodules
	for i to nmodules do
		m:=&moduletable[i]
!		println @f,"	",,i,,":",m.name,=m.startfn,=m.mainfn,=m.ast,=m.pcstart,=m.pcsize,
		println @f,"	",,i,,":",m.name,=m.parsed,=m.pcstart,=m.pcsize,
			=ref void(m.source)
	od

	println @f
	println @f,"PROC Global GenField Table",ngenfields
	for i to ngenfields do
		g:=genfieldtable[i]
		if g=nil then next fi
		fprintln @f,"   #) #:",i,g.def.name
		while g do
			d:=g.def
			println @f,"      ",d.name, namenames[d.nameid],d.owner.name
			g:=g.nextdef
		od
	od
	println @f


	println @f,"DLL Table", ndlllibs
	for i to ndlllibs do
		println @f, i,":",dlltable[i].name, dllinsttable[i]
	od
	println @f

	println @f,"DLL Proc Table", ndllprocs
	for i to ndllprocs do
		d:=dllproctable[i]
		println @f, i,":",d.name, dlllibindex[i], dllprocaddr[i],(d.mvarparams|"Variadic"|"")
	od
	println @f

	println @f,"All Proc Table",nproclist
	p:=proclist
	while p do
		println @f,"Proc:",p.def.name,p.def.owner.name
		p:=p.nextproc
	od
	println @f


	fclose(f)
end

proc showtypes=
	var filehandle f
	var ref modulerec m

	return unless fshowtypes
	return when runcode=run_cc

	f:=fopen("TYPES","w")
	printtypetables(f)

	fclose(f)
end

proc showpcl(int pass)=
	var filehandle f

	return when runcode=run_cc

	gs_init(pcldest)
	gs_str(pcldest,"PROC ALL PCL pass:")
	gs_strint(pcldest,pass)
	gs_line(pcldest)

	for i to nmodules do
		writeallpcl(i,pass)
	od

	f:=fopen((pass=1|"PCL1"|"PCL2"),"w")
	if not f then return fi
	gs_println(pcldest,f)

	fclose(f)
end

proc lextest=
var ichar source
var int ntokens,tm

!source:=cast(readfile(inputfile))
!if source=nil then
!	println "Can't load",inputfile
!	stop
!fi
!
!!lexsetup()

tm:=clock()

startlex("ONE",MODULETABLE[1].source,1)
lex()

repeat
!	lexreadtoken()
	lex()
	++ntokens
!	PS("LOOP")
	println symbolnames[lx.symbol],LX.POS:"H",REF VOID(MODULETABLE[1].SOURCE)
!IF LX.SYMBOL NOT IN [SEMISYM, EOFSYM] THEN
!	PRINTLN "---->"
!	GETERRORINFO(LX.POS,1)
!	SHOWERRORSOURCE()
!!	SHOWSOURCE(LX.POS)
!	PRINTLN
!FI

until lx.symbol in [eofsym,errorsym]
!until lx.symbol in [SEMISYM,eofsym,errorsym]
!until NEXTlx.symbol in [eofsym,errorsym]
tm:=clock()-tm

!CPL =NTOKENS
!CPL =LX.LINENO
!CPL =TM
!CPL =NSTDLOOKUPS
!CPL =NLOOKUPS
!CPL =NFOUNDLOOKUPS
!CPL =N2
!CPL =N4
!CPL =N8
!CPL =NALL

end

proc initprogram=
!	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
remove("AST1")
remove("AST2")
remove("PCL1")
remove("PCL2")
remove("ST")

initsearchdirs()

!CPL "SEARCH:",NSEARCHDIRS
!FOR I TO NSEARCHDIRS DO
!	CPL I,SEARCHDIRS[I]
!OD

lexinit()
inithostlib()

os_initwindows()
end

!function loadsource(ichar file)ichar=
!	var ichar s
!
!	s:=cast(readfile(file))
!	if s=nil then
!		println "Can't load",s
!		stop 1
!	fi
!	return s
!end

proc showast(ichar file)=
	var filehandle f
	symbol d
	var int k,i

	return when runcode=run_cc

	f:=fopen(file,"w")
	return unless f

	println @f,"PROC",file,,":"

	for i to nmodules do
!CPL "MODULE:",moduletable[i].name
		println @f,"MODULE:",moduletable[i].name
		printunit(moduletable[i].ast, dev:f)
		d:=moduletable[i].def.deflist
		while d do
			if d.nameid=procid then
!CPL "PROC:",D.NAME
				println @f,"\n---PROC",d.name
				printunit(d.code, dev:f)
			fi
			d:=d.nextdef
		od
		

	od

	printunit(lastast,dev:f)
	println @f

!	println @f,"Names from hashtable:"
!	k:=lastnameindex
!
!	i:=0
!	while k>=0 do
!		println @f,"	",,++i,,":",hashtable[k].name,hashtable[k].seqno
!		k:=hashtable[k].lastname
!	od
!	println @f

	fclose(f)
end

proc browsefile(ichar filename)=
	var [300]char str

!	sprintf(&.str,"\\m\\med.bat %s",filename)
	fprint @str,"\\m\\med.bat ",filename

	os_execwait(str,1,nil)
end

proc addtolog(ichar filename, filehandle logdest)=
var filehandle f
var int c

f:=fopen(filename,"rb")
if f=nil then return fi

do
	c:=fgetc(f)
	exit when c=c_eof
	fputc(c,logdest)
od
fclose(f)
end

proc showtiming(int ticks, lines)=
!CPL "RUNTIME:",TICKS
!CPL =LINES
!IF TICKS THEN
!	CPL LINES/(REAL(TICKS)*1000),"MLPS"
!fi
!cpl

end

proc getinputoptions=
	var int paramno,pmtype
	var ichar name,value

	paramno:=2

	while pmtype:=nextcmdparam(paramno,name,value,"q") do
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
!		if inputfile then
!			println "Too many input files"
!			stop 1
!		fi
			inputfile:=pcm_copyheapstring(name)
			exit				!leave any other files to app being run
		esac
	od

	if not inputfile then
		println "Q Interpreter"
		println "Usage:"
		println "	",,sysparams[1],"filename[.q]"
		stop
	fi

	if dispatchtype in [debug_dispatch,fdebug_dispatch] then
		hasbytecodes:=1
	else
		hasbytecodes:=0
	fi

	getsyscmdline(paramno-1)	!copy to cmdparams starting from this inputfile

!	for i:=1 to ncmdparams do
!		cpl i,":",cmdparamtable[i]
!	od

end

proc getsyscmdline(int n)=
!get system args starting from sysparams[n] and store into local cmdparams of a task

	setcmdparam(1,sysparams[1])

	for i:=n to nsysparams do
		setcmdparam(i-n+2,sysparams[i])
	od
end

global proc setcmdparam(int index, ichar s)=
!build cmd params for pc program, or set size (usually smaller) when s is nil
	if s=nil then
		ncmdparams:=index
	elsif index<=maxcmdparam then
		cmdparamtable[index]:=pcm_copyheapstring(s)
		ncmdparams max:=index
	fi

end

proc do_option(int sw, ichar value)=

case sw
when load_sw then runcode:=load_cc
when parse_sw then runcode:=parse_cc
when names_sw then runcode:=names_cc
when gen_sw then runcode:=gencode_cc
when fixup_sw then runcode:=fixup_cc
when run_sw then runcode:=run_cc

when ast1_sw then fshowast1:=1
when ast2_sw then fshowast2:=1
when pcl1_sw then fshowpcl1:=1
when pcl2_sw then fshowpcl2:=1
when st_sw then fshowst:=1
when types_sw then fshowtypes:=1

when fn_sw then dispatchtype:=fn_dispatch
when sw_sw then dispatchtype:=sw_dispatch
when asm_sw then dispatchtype:=asm_dispatch
when debug_sw then dispatchtype:=debug_dispatch
when fdebug_sw then dispatchtype:=fdebug_dispatch

esac

end

!FUNCTION GETPCLNAME:ICHAR=
!	RETURN PCLNAMES[GETPCLCODE(PCPTR)]
!END
!
!FUNCTION GETPCLCODE(REF INT PC)INT=
!	FOR I IN PCLNAMES.BOUNDS DO
!		IF CAST(PC^,REF VOID)=HANDLERTABLE[I] THEN
!			RETURN I
!		FI
!	OD
!	RETURN 0
!END

proc initsearchdirs=
[300]char str1,str2
int i

searchdirs[++nsearchdirs]:=""
strcpy(&.str1,os_gethostname())
if str1[1] then
	strcpy(&.str2,extractpath(&.str1))

	searchdirs[++nsearchdirs]:=ref char(pcm_copyheapstring(&.str2))
fi

strcpy(&.str1,os_getmpath())
if str1[1] then
	searchdirs[++nsearchdirs]:=ref char(pcm_copyheapstring(&.str1))
fi
!searchdirs[++nsearchdirs]:=pcm_copyheapstring("c:/qcc/")

!CPL "SEARCH DIRS:"
!for i to nsearchdirs do
!	cpl i,,":",searchdirs[i]
!od
!OS_GETCH()
end

global proc addsearchdir(ichar path)=
	for i to nsearchdirs do
		if eqstring(searchdirs[i],path) then return fi
	od
	searchdirs[++nsearchdirs]:=pcm_copyheapstring(path)
end

proc fixproc(symbol d)=
	ref int z
	variant p

	if not d.procfixed then
		if nprocs>=maxproc then gerror("Too many procs") fi
!CPL "FIXPROC",D.NAME, =D.MODULENO,namenames[d.nameid]
		z:=moduletable[d.moduleno].pcstart+d.labelno
		p:=pcm_alloc(varsize)
		proctable[++nprocs]:=z
		procdefs[nprocs]:=d
		d.pcaddress:=z
		d.procfixed:=1
	fi

end

proc fixupmodule(int m)=
	ref int pc,pcstart,z
	int cmd,y
	symbol d
	variant p

	setcmdmap()

	pc := pcstart := moduletable[m].pcstart


!CPL "FIXUP",STOPSEQ,RAISESEQ

	repeat
		cmd:=pc^
!
		pc^:=cast(cmdmap[cmd])			!convert cmd index to labeladdr/functionaddr/same index
		++pc

		case cmd
		when kprocdef then
			fixproc(cast(pc^))
		esac

		for i to pclnopnds[cmd] do
			switch pclfmt[cmd,i]
			when cproc then
				d:=cast(pc^)
				fixproc(d)
				pc^:=cast(d.pcaddress)
			when cmemory then
				d:=cast(pc^)
				if d.varptr=nil then
					if nstatics>=maxstatic then gerror("Too many statics") fi
					p:=pcm_alloc(varsize)
					statictable[++nstatics]:=p
					staticdefs[nstatics]:=d
					d.varptr:=p
				fi
				pc^:=cast(d.varptr)
			when cframe then
				d:=cast(pc^)
				pc^:=d.index*16
			when cgenfield then
				pc^:=symbol(pc^).genfieldindex
			when cstring then
!				CPL "STRING FIXUP"
				pc^:=cast(obj_make_string(cast(pc^),0))
			when clabel then
				y:=int(pcstart+pc^)
				pc^:=y

			end
			++pc
		od
	until cmd=kendmodule

!CPL "DONE FIXING"

end

proc run_program=
	var ref proc fnptr
	var int cmd,SS
	var ref int pcstart

	return when runcode<run_cc

!CPL "RUNPCLCODE"
	sptr:=&varstack[stacksize]
	stacklimit:=&varstack[1]
	stacktop:=sptr
	pcstart:=pcptr:=moduletable[1].pcstart
!CPL "RUNCODE",=PCPTR
	pcerrorpos:=0

!CPL =PCPTR, PCPTR^
!
!CPL "START PROGRAM",=PCPTR,=SPTR
!CPL "---------RUN PROGRAM"
	disploop()
!	disploop(1)


!	println "Starting interpreter:",&varstack[$]-sptr
!	println "Starting interpreter:"

end

proc disploop(int deb=0)=

	case dispatchtype
	when fn_dispatch then
!CPL "FN DISPATCH"
		repeat
			(cast(pcptr^, ref proc))^()
			(cast(pcptr^, ref proc))^()
			(cast(pcptr^, ref proc))^()

!			(cast(pcptr^, ref proc))^()
		until stopped

	when debug_dispatch, fdebug_dispatch then
!CPL "DEBUG DISPATCH"
		repeat
if deb then
println pclnames[pcptr^],=pcptr,=SPTR
fi
            handlertable[pcptr^]^()
!CPL "NEWPCPTR",PCPTR,=SPTR
		until stopped

!	when sw_dispatch then
!		disploop_sw()

	else
		loaderror_s("Dispatch not supported:",dispatchnames[dispatchtype])
	esac

end


!proc disploop_sw=
!	variant p
!!	int cmd
!
!	doswitch pcptr^
!!	do
!!	cmd:=pcptr^
!!!CPL "SWLOOP",PCLNAMES[CMD],HANDLERTABLE[CMD],K_STARTMODULE
!!	
!!	switch cmd
!	when knop then
!CPL "NOP"
!	when kstartmodule then
!		handlertable[kstartmodule]^()
!	when kcomment then
!		skip(1)
!	when kcallproc then	handlertable[kcallproc]^()
!	when kprocentry then	handlertable[kprocentry]^()
!	when kpushci then	handlertable[kpushci]^()
!	when kzpopf then	handlertable[kzpopf]^()
!	when ktof then
!		p:=cast(frameptr+getopndb)
!!		--p.value
!
!		if --p.value then
!			pcptr:=cast(getopnda)
!		else
!			skip(2)
!		fi
!
!
!!		k_tof()
!!	handlertable[cmd]^()
!	when kunshare then	handlertable[kunshare]^()
!	when kreturn then	handlertable[kreturn]^()
!	when kstop then
!		handlertable[kstop]^()
!		exit
!	else
!		pcerror_s("bytecode not supported:",pclnames[pcptr^])
!!	end
!!	end
!
!	enddoswitch
!
!end

proc setcmdmap=
	for i:=1 to klastpcl do
		case dispatchtype
		when fn_dispatch then
			cmdmap[i]:=handlertable[i]
		when debug_dispatch, fdebug_dispatch, sw_dispatch then
			cmdmap[i]:=cast(i)
		when asm_dispatch then
			cmdmap[i]:=cast(i)
		esac
	od
end

global function runproc_m(ref void amsg)int=
	varrec a,b,dest
	static int rmsg_typeno
	int i,result
	objrec obj

!PCERROR("RUNPROC/M NOT READY")
!CPL "RUNPROCM1"

	if rmsg_typeno=0 then
		for i to ntypes do
			if eqstring(ttname[i],"ws_msg64") then
				rmsg_typeno:=i
				exit
			fi
		od
	fi
	if rmsg_typeno=0 then
		abortprogram("mainwndproc: can't find rmsg")
	fi

!CPL "RUNPROCM2",=RMSG_TYPENO, TTNAMEDEF[RMSG_TYPENO]
	memset(&obj,0,objrec.bytes)
!clear obj
	obj.refcount:=99
	obj.ustruct.ptr:=ref byte(amsg)

	a.tagx:=rmsg_typeno ior hasrefmask
	a.objptr:=&obj

!A.TAGX:=TINT
!A.VALUE:=7777


++PCLLEVEL
	runproc(pcl_callbackfn,&a,nil,&dest)
--PCLLEVEL
	result:=dest.value

	result:=0			!WTF? BUT QX HAS THIS TOO, AND IT WORKS!

	return result
end

global proc runproc(ref void fnptr,variant a,b,dest) =
!Directly call a pcl function by supplying it's pc-address
!sptr/frameptr etc should already have been set up (any start proc should have been called)
!Allows 0, 1, or 2 params: (), (a), or (a,b)
!Note: param data is not freed here caller should take care of that
!Return values are stored in dest (any non-int or void result is returned as 0)
!Use of the stack::
! The stack as it was at the time of the callext call (or via a callback from Windows)
! is entirely unaffected. However some things will be pushed onto it here::
! * Push void which is used for any return value of the function that is called
! * Push 0, 1 or 2 parameters (as supplied in a and b; a is pushed first)
! * The interpreter is then started, at the function call pc address supplied
! * This involves pushes a retaddr value. Since this is not a conventional call,
!   The return address is contrived to point to a STOP0 pc opcode
! * After the return from the function, STOP0 is executed, which pushes a zero
!   value to the stack.
! * If the called function eventually returns, it will execute STOP0, but
!   there is no Retaddr value left on the stack, and it will know to use the
!	actual return value (0 is used of the called function did not return a value)
! * If STOP is explicitly used, then a Retaddr value stays on the stack (for this
!	function, or any nested one), and the Stop value is used instead

	variant oldsptr
	ref byte oldframeptr
	ref int oldpcptr
	byte oldstopped
	int nparams

!STATIC INT COUNT

!CPL "RUNPROC"
!IF ++COUNT>3 THEN STOP FI

	dest.tagx:=tint
	dest.value:=0

	oldstopped:=stopped		!not really need, as it can be assumed stopped=0
	oldpcptr:=pcptr
!CPL "OLD",=PCPTR
	oldsptr:=sptr
	oldframeptr:=frameptr

	(--sptr).tagx:=999				!put in marker (this location might be checked later)

	if b and b.tag then			!must stack in reverse order: (b,a) or (a)
		nparams:=2
		(--sptr)^:=b^
		(--sptr)^:=a^
	elsif a and a.tag then
		nparams:=1
		(--sptr)^:=a^
	else
		nparams:=0
	fi
	(--sptr).tagx:=treturn

	sptr.uret.retaddr:=stopseq


!	sptr.tagx:=treturn
!	sptr^.uret.retaddr := pcptr+3
!
!	sptr^.uret.frameptr_low := word32@(frameptr)
!!	sptr^.uret.stackadj :=getopndb
!	frameptr:=cast(sptr)
!
!




!CPL =STOPSEQ, =STOPSEQ^,PCLNAMES[STOPSEQ^]

	sptr.uret.frameptr_low:=int32@(frameptr)
!sptr.uret.frameptr_low:=int32@(int32(frameptr))
!	sptr.uret.stackadj:=nparams*varsize
	frameptr:=cast(sptr)
	pcptr:=fnptr

!CPL "*********RUNPROC2",=pcptr,PCLNAMES[PCPTR^],=SPTR
!	disploop(1)
	disploop(0)


!CPL "RUNPROC3"

!stack will either point to a stop-value, with a retaddr before it,
!or to the first param (or to the proc return value).
	if (sptr+1).tag=treturn then		!probably stop used
!		CPL "RUNPROC: STOP used"
		dest^:=sptr^
	else								!assume normal return used
	++SPTR
		dest^:=sptr^					!pick up return value

		if dest.tag=tvoid then		!no value; return 0
			dest.tagx:=tint
			dest.value:=0
		fi
	fi

	pcptr:=oldpcptr
	stopped:=oldstopped

!NOTE: could do with freeing items on the stack between oldsptr and current sptr
	sptr:=oldsptr			!need to reset these, as stop could have been executed anywhere
	frameptr:=oldframeptr	! and these could have arbitrary values
	stopped:=oldstopped
!CPL "RUNPROC4-----------------------------"
end

=== mclib.m 2/32 ===
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
	clang function _strdup  (ichar)ichar

	clang function puts		(ichar)int32
	clang function puts99	(ichar)int32
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

global macro strdup=_strdup

importlib $cstdextra=
	clang function __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

global const c_eof		=-1
global const seek_set	= 0
global const seek_curr	= 1
global const seek_end	= 2
=== msyslib.m 3/32 ===
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
!const rd_buffersize = 16384	!total capacity of line buffer
const rd_buffersize = 524288	!total capacity of line buffer

global ref char rd_buffer		! point to start of read buffer
int rd_length			! length of this line (as read by readln)
ref char rd_pos			! current position it's up to (next read starts here)
ref char rd_lastpos		! set by sread() just before reading used for reread()
int termchar			! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals

!------------------------------------------

const maxparam=128
global int nsysparams
global int nenvstrings
global [maxparam]ichar sysparams
global [maxparam]ichar envstrings

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

!PUTS("M$INIT")

res:=__getmainargs(&nargs,cast(&args),cast(&env),0,cast(&startupinfo))

nsysparams:=nargs

if nsysparams>maxparam then
	printf("Too many params\n")
	stop 50
fi

nargs64:=nargs			!bug when using 32-bit limit when compiled with mm
for i:=1 to nargs64 do
	sysparams[i]:=args[i]
od

int j:=1
nenvstrings:=0
while env[j] do
!	println "ENV:",J,ENV[J]
	envstrings[++nenvstrings]:=env[j]
	++j
OD



!PUTS("M$INIT")
m$print_startcon()		!allow most print stmts without startcon/end

end

global proc m$stop(int n)=
	`exit(n)
!	assem
!		mov d10,[n]
!		mov d0,`exit
!		call m$callff_4
!	end
end

global function m$lenstr_stringz(ref char s)int=
	strlen(s)
end

!global function m$getdotindex(word64 a,int i)int=
!!return (a iand (1dw<<i))>>i
!return (a iand (1<<i))>>i
!end
!
!global proc m$setdotindex(ref word64 a, int i,x)=
!ref word32 a32
!
!!see comments on setdotslice
!if i>=32 then
!	a^:=(a^ iand inot (1<<i)) ior (word64(x)<<i)
!
!else
!	a32:=cast(a)
!	a32^:=(a32^ iand inot (1<<i)) ior (word(x)<<i)
!fi
!end
!
!global function m$getdotslice(word64 a,int i,j)int=
!if i>=j then
!	return (a>>j)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(i-j+1))
!else
!	return (a>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
!fi
!end
!
!global proc m$setdotslice(ref word64 a, int i,j,word64 x)=
!!a^:=(a^ iand inot (1dw<<i)) ior (word64(x)<<i)
!int w
!word64 mask64
!word mask
!ref word32 a32
!
!if i>j then println "SETDOTSLICE?"; stop 52 fi
!
!!when j>=32, assume 64 bit dest, otherwise assume 32 bits to avoid writing
!!to bytes beyond the 32-bit value
!!THIS WILL BE A PROBLEM IF writing to 8/16 bit values too
!
!if j>=32 then
!	mask64:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
!	a^:=(a^ iand inot mask64) ior x<<i
!else
!	a32:=cast(a)
!	mask:=inot((0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i			!shifted field of w 1s
!	a32^:=(a32^ iand inot mask) ior x<<i
!fi
!
!end

!function m$get_nprocs:int=
!	5
!!	assem
!!		mov D0,[$nprocs]
!!	end
!end
!
!function m$get_procname(int n)ichar=
!nil
!!	assem
!!		lea D0,[$procnames]
!!		mov D1,[n]
!!		mov D0,[D0+D1*8-8]
!!!		mov D0,[sss]
!!	end
!end
!
!function m$get_procaddr(int n)ref proc=
!nil
!!	assem
!!		lea D0,[$procaddrs]
!!		mov D1,[n]
!!		mov D0,[D0+D1*8-8]
!!	end
!end
!
!global function m$get_nexports:int=
!786
!!	assem
!!		mov D0,[$nexports]
!!	end
!end
!
!global function m$get_procexport(int n)ref void=
!nil
!!	assem
!!		lea D0,[$procexports]
!!		mov D1,[n]
!!		shl D1,1
!!		lea D0,[D0+D1*8-16]
!!	end
!end

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
!		strtofmt(fmtstyle,-1,&fmt)
!		tostr_str(s,&fmt)
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

global proc m$print_space=
	needgap:=0
	printstr(" ")
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

!function mdivrem(word64 a,b)word64,word64=
!	word64 q,r
!	assem
!		xor rdx,rdx
!		mov rax,[a]
!		div u64 [b]
!		mov [q],rax	
!		mov [r],rdx	
!	end
!	return (q,r)
!end

function u64tostr(u64 aa,ref char s,word base,int sep)int =		!U64TOSTR
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
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
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w,usigned
!	static u64 mindint=0x8000'0000'0000'0000
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

!str uses upper cases for hex/etc see if lc needed
	if (fmt^.base>10 or fmt^.suffix) and fmt^.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt^.base,fmt^.sepchar)

	if fmt^.suffix then
		str[n]:=fmt^.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt^.base>10 or fmt^.suffix and fmt^.lettercase='a'	then	! need lower when
!		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u128tostrfmt(i128 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
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

global function strtoint(ichar s,int length=-1, word base=10)int64=
!return point to next char after terminator (which can be just off length of string)
	byte signd
	word64 aa
	word c,d

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
!number is positive, so can treat like i64
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

!global proc m$intoverflow=
!	abortprogram("Integer overflow detected")
!end

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
!does 128/64 bits only
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
!return a.[i] in d0
!	assem
!		mov d0,[a]
!		mov cl,[i]
!		shr d0,cl
!		and d0,1
!	end	
end

global proc m$dotslice(word j,i,a)=
!!return a.[i..j] in d0; assumes j>=i
!	assem
!		mov d0,[a]
!		mov rcx,[i]
!		shr d0,cl
!		sub rcx,[j]
!		neg rcx				!j-1
!		mov d2,0xFFFF'FFFF'FFFF'FFFE
!		shl d2,cl
!		not d2
!		and d0,d2
!	end	
end

global proc m$popdotindex(word i,ref word p,word x)=
!!p^.[i]:=x
!	assem
!		mov d3,[p]
!		mov cl,[i]
!		mov d0,[d3]
!		mov d1,1
!		shl d1,cl			!000001000
!		not d1				!111110111
!		and d0,d1			!clear that bit in dest
!		mov d1,[x]
!		and d1,1
!		shl d1,cl
!		or d0,d1
!		mov [d3],d0
!	end	
end

global proc m$popdotslice(word j,i, ref word p, word x)=
!p^.[i..j]:=x
!	assem
!!d3 = p
!!d4 = x, then shifted then masked x
!!d5 = i
!!d6 = clear mask
!
!		mov d3,[p]
!		mov d4,[x]
!		mov d5,[i]
!		mov rcx,d5			!i
!		shl d4,cl			!x<<i
!		mov rcx,[j]
!		sub rcx,d5			!j-i
!		inc rcx				!j-i+1
!		mov d2,0xFFFF'FFFF'FFFF'FFFF
!		shl d2,cl			!...111100000     (assume 5-bit slice)
!		not d2				!...000011111
!		mov rcx,d5			!i
!		shl d2,cl			!...000011111000  (assume i=3)
!		and d4,d2			!mask x (truncate extra bits)
!		mov d0,[d3]
!		not d2				!...111100000111
!		and d0,d2			!clear dest bits
!		or d0,d4			!add in new bits
!		mov [d3],d0
!	end	
end


!global function m$sin(real x)real = {`sin(x)}
!global function m$cos(real x)real = {`cos(x)}
!global function m$tan(real x)real = {`tan(x)}
!global function m$asin(real x)real = {`asin(x)}
!global function m$acos(real x)real = {`acos(x)}
!global function m$atan(real x)real = {`atan(x)}
!global function m$ln(real x)real = {`log(x)}
!!global function m$lg(real x)real = {`lg(x)}
!global function m$log(real x)real = {`log10(x)}
!global function m$exp(real x)real = {`exp(x)}
!global function m$floor(real x)real = {`floor(x)}
!global function m$ceil(real x)real = {`ceil(x)}
!global function m$fract(real x)real = {abortprogram("FRACT");0}
!global function m$round(real x)real = {abortprogram("ROUND");0}

global proc mclunimpl(ichar mess)=
	printf("MCL-UNIMPL: %s\n",mess)
	stop 1
end
=== mlibnew.m 4/32 ===
import msys
import clib
import oslib

!const mem_check=1
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

GLOBAL REF VOID ALLOCBASE

global int memtotal=0
global int64 smallmemtotal=0
global int smallmemobjs=0
global int maxmemtotal=0

!store all allocated pointers
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
smallmemtotal+:=allocbytes

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
int j,k,k1,k2
int64 size
const limit=1<<33

if pcm_setup then
	return
fi

pcm_newblock(0)

ALLOCBASE:=PCHEAPPTR
!CPL "*** SETALLOCBASE",STRALLOC(ALLOCBASE)

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
!os_getch()
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

!No items in freelists: allocate new space in this heap block

return pcm_alloc(32)
end

global proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

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

global function pcm_smallalloc(int n)ref void =
ref byte p

allocbytes:=allocupper[alloccode:=sizeindextable[n]]

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
[100]char name
[300]char exefile
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

!No items in freelists: allocate new space in this heap block
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

global function readline:ichar=
	readln
	return rd_buffer
end

global function stralloc(ref void p)ichar=
	return strint(int(ref byte(p)-allocbase))
end
=== mwindows.m 5/32 ===
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

record input_record = $caligned
	wt_word	eventtype
!	word16	padding
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

!memset(&si,0,si.bytes)
!memset(&xpi,0,xpi.bytes)
clear si
clear xpi

switch newconsole
when 0 then cflags := NORMAL_PRIORITY_CLASS
when 1 then cflags := NORMAL_PRIORITY_CLASS ior CREATE_NEW_CONSOLE
when 2 then cflags := NORMAL_PRIORITY_CLASS ior DETACHED_PROCESS
endswitch

si.size := rstartupinfo.bytes

!CPL "NEWEXECWAIT",CMDLINE
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

!memset(&si,0,si.bytes)
!memset(&xpi,0,xpi.bytes)
clear si
clear xpi

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
!CPL =HINST
return cast(hinst)
end

global function os_getdllprocaddr(int hinst,ichar name)ref void=
!CPL "GETPROCADDR:",HINST,NAME

return GetProcAddress(cast(int(hinst)),name)
!REF VOID P
!
!P:=GetProcAddress(cast(int(hinst)),name)
!CPL =P
!CPL =getlasterror()
!return P
end

global proc os_initwindows=
os_init()
!CPL "INITWIND"
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

!memset(&r,0,r.bytes)
clear r

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

global callback function mainwndproc (
		wt_handle hwnd, wt_uint message, wt_wparam wParam, wt_lparam lParam)intm=
rmsg m
int i,result
intm l
static int count=0

CPL "MAINWND/BB",MESSAGE

m.hwnd:=hwnd
m.message:=message
m.wParam:=wParam
m.lParam:=lParam
m.pt.x:=0
m.pt.y:=0

!CPL "BEFORE CALL",=HWND,=MESSAGE,=WPARAM,=REF VOID(LPARAM)
if (wndproc_callbackfn) then
	result:=(wndproc_callbackfn^)(&m)
else
	result:=0
fi

!CPL "AFTER CALL",=HWND,=MESSAGE,=WPARAM,=REF VOID(LPARAM)
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

GLOBAL PROC OS_TESTCALLBACK(ref void p)=



	IF WNDPROC_CALLBACKFN THEN
		(WNDPROC_CALLBACKFN)(P)
	ELSE
		ABORTPROGRAM("MESS HANDLER NOT DEFINED")
	FI

END

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
[100]byte m
	ticks:=GetTickCount()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end
=== qq_lex.m 6/32 ===
import* qq

!const diagmode=1
const diagmode=0

const etx	= 26
const cr	= 13
const lf	= 10
const tab	= 9

GLOBAL VAR INT NSTDLOOKUPS
GLOBAL VAR INT NLOOKUPS
GLOBAL VAR INT NFOUNDLOOKUPS
GLOBAL VAR INT N2,N4,N8,NALL

GLOBAL var int NALLLINES

var ref char lxsource		!start of module
var ref char lxstart		!start of this token
!var ref char lxsptr
var ref char lxsptr
var int lxifcond
var int longsuffix			!for real nos
var int lxmoduleno

!const hstsize	= 16384
!const hstsize	= 131072
const hstsize	= 32768
const hstmask	= hstsize-1

!const stdhstsize	= 512
!const stdhstmask	= stdhstsize-1

!int nextlxhashvalue
int nextlxlength
global int lxlength

!global var [0:hstsize]namerec hashtable
global var [0:hstsize]strec hashtable
!var [0:stdhstsize]namerec stdhashtable
var ref strec hashtablelast
!var ref strec stdhashtablelast
global var int lastnameindex=-1
global var int nameindex=0

!global const maxnames=12000			!used in nametable (see pcilib)
!global const maxnames=10000			!used in nametable (see pcilib)
!global const maxnames=2000			!used in nametable (see pcilib)
global const maxnames=20000			!used in nametable (see pcilib)

const maxsources=100
global var [maxsources]ichar sourcenames
var int nsources

var ichar sourcename

var []ichar maxnumlist=(
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
var [maxnumlist.len]int maxnumlen

var [0..255]char linecommentmap

global proc lexreadtoken=
!read next token into nextlx
var int c,csum,hsum,commentseen
var ref char pstart,pnext,p,ss

nextlx.subcode:=0

doswitch lxstart:=lxsptr; lxsptr++^
!when 'a'..'z','$','_' then
when 'a'..'e','g'..'z','$','_' then
dolower::
	nextlx.svalue:=lxsptr-1
doname::
	hsum:=nextlx.svalue^

	doswitch c:=lxsptr++^
	when 'A'..'Z' then
		(lxsptr-1)^:=c+' '
		hsum:=hsum<<4-hsum+c+' '
	when 'a'..'z','0'..'9','_','$' then
		hsum:=hsum<<4-hsum+c
	else
		--lxsptr
		exit
	end doswitch

!	do_name(nextlx.svalue, lxsptr-nextlx.svalue, nextlxhashvalue iand hstmask)
	do_name(nextlx.svalue, lxsptr-nextlx.svalue, (hsum<<5-hsum) iand hstmask)

	return

!when 'A'..'Z' then
when 'A'..'E','G'..'Z' then
doupper::
	nextlx.svalue:=lxsptr-1
	nextlx.svalue^+:=32
!	hsum:=csum:=nextlx.svalue^
	goto doname

when 'f' then
	if lxsptr^<>'"' then
		goto dolower
	fi
	readrawstring()
	return

when 'F' then
	if lxsptr^<>'"' then
		goto doupper
	fi
	readrawstring()
	return

when '0'..'9' then
	c:=(lxsptr-1)^
	case lxsptr^
	when ' ',')',cr,',','|' then		!assume single digit decimal
		nextlx.symbol:=intconstsym
		nextlx.subcode:=tint
		nextlx.value:=c-'0'
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
!	esac
	end
	return

when '!' then			!comment to eol
docomment::

	doswitch c:=lxsptr++^
	when 13 then
		++lxsptr
		exit
	when 10 then
		exit
	when etx,0 then
		--lxsptr
		exit
	end
!
!	while linecommentmap[c:=lxsptr++^] do od
!
!	case c
!	when 13 then
!		++lxsptr
!	when 10 then
!	when 0,etx then
!		--lxsptr
!	esac

!	nextlx.lineno:=nextlx.lineno+1
!++NALLLINES
	nextlx.symbol:=eolsym
	return

when '#' then			!docstring to eol
	nextlx.svalue:=cast(lxsptr)

	doswitch c:=lxsptr++^
	when 13,10,etx,0 then			!leave eol for next symbol
		--lxsptr
		exit
	end

	nextlxlength:=lxsptr-ref char(nextlx.svalue)
	nextlx.symbol:=docstringsym

	return

!	nextlx.symbol:=hashsym
!	return

when '\\' then			!line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
	commentseen:=0
	doswitch lxsptr++^			!read until end of this line
	when cr then
!++NALLLINES
!		++nextlx.pos
		++lxsptr				!skip lf
		exit
	when lf then
!++NALLLINES
!		++nextlx.pos
		exit
	when etx,0 then
		nextlx.symbol:=eofsym
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
!		++nextlx.pos
!++NALLLINES
		++lxsptr				!skip lf
	when lf then
!++NALLLINES
!		++nextlx.pos
	when ' ',tab then
	else
		--lxsptr
		exit
	enddoswitch
!	next

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
			nextlx.symbol:=rangesym
			nextlx.subcode:=jmakerange		!helps treat as opsym which all have k-code as subcode
		fi
		return
	when '0'..'9' then			!real const: deal with this after the switch
		--lxsptr
		readrealnumber(nil,0,10)
		return
	else
!		p:=lxsptr-2
!		if p<lxstart or p^=cr or p^=lf then
!			nextlx.symbol:=lexdotsym
!		else
			nextlx.symbol:=dotsym
!		fi
		return
	endswitch

when ',' then
	nextlx.symbol:=commasym
	return

when ';' then
	nextlx.symbol:=semisym
	return

when ':' then
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.symbol:=assignsym
		nextlx.subcode:=jassign			!helps treat as opsym which all have k-code as subcode
	when ':' then
		++lxsptr
		case lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=deepcopysym
			nextlx.subcode:=jdeepcopy
		else
			nextlx.symbol:=dcolonsym
		esac
	else
		nextlx.symbol:=colonsym
	endswitch
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
	if lxsptr^='|' then
		++lxsptr
		nextlx.symbol:=dbarsym
	else
		nextlx.symbol:=barsym
	fi
	return

when '^' then
	nextlx.symbol:=ptrsym
	nextlx.subcode:=jptrto
	return

when '@' then
	if lxsptr^='@' then
		++lxsptr
		nextlx.symbol:=datsym
	else
		nextlx.symbol:=atsym
	fi
	return

when '?' then
	nextlx.symbol:=questionsym
	return

!when 156 then		!'œ' in ansi font or whatever
!when 'œ' then		!'œ' in ansi font or whatever
!	nextlx.symbol:=poundsym
!	return
!
when '~' then
	nextlx.symbol:=curlsym
	return

!when 'ª' then
!	nextlx.symbol:=gatesym
!	return
!
when '+' then
	nextlx.symbol:=addsym
	if lxsptr^='+' then
		++lxsptr
		nextlx.symbol:=incrsym
		nextlx.subcode:=jincrload
		return
	else
		nextlx.subcode:=jadd
	fi
	return

when '-' then
	nextlx.symbol:=subsym
	if lxsptr^='-' then
		++lxsptr
		nextlx.symbol:=incrsym
		nextlx.subcode:=jdecrload
		return
	else
		nextlx.subcode:=jsub
	fi
	return

when '*' then
	nextlx.symbol:=mulsym
	if lxsptr^='*' then
		++lxsptr
		nextlx.symbol:=powersym
		nextlx.subcode:=jpower
	else
		nextlx.subcode:=jmul
	fi
	return

when '/' then
	nextlx.symbol:=divsym
!	case lxsptr^
!	when '/' then
!		++lxsptr
!		nextlx.subcode:=jddiv
!	else
		nextlx.subcode:=jdiv
!	esac
	return

when '%' then
	nextlx.symbol:=idivsym
	nextlx.subcode:=jidiv
	return

when '=' then
	case lxsptr^
	when '>' then
		nextlx.symbol:=sendtosym
		++lxsptr
	when '=' then
		nextlx.symbol:=isequalsym
		nextlx.subcode:=jisequal

		++lxsptr
!	when ':' then
!		++lxsptr
!		if lxsptr^<>'=' then lxerror("=:?") fi
!		++lxsptr
!		nextlx.symbol:=dispassignsym
!		nextlx.subcode:=jdispassign
!CPL "DISPASSIGN"
	else
		nextlx.symbol:=eqsym
		nextlx.subcode:=jeq
	esac
	return

when '<' then
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.symbol:=lesym
		nextlx.subcode:=jle
	when '>' then
		++lxsptr
		nextlx.symbol:=nesym
		nextlx.subcode:=jne
	when '<' then
		++lxsptr
		nextlx.symbol:=shlsym
		nextlx.subcode:=jshl
	else
		nextlx.symbol:=ltsym
		nextlx.subcode:=jlt
	endswitch
	return

when '>' then
	switch lxsptr^
	when '=' then
		++lxsptr
		nextlx.symbol:=gesym
		nextlx.subcode:=jge
	when '>' then
		++lxsptr
		nextlx.symbol:=shrsym
		nextlx.subcode:=jshr
	else
		nextlx.symbol:=gtsym
		nextlx.subcode:=jgt
	endswitch
	return

when '&' then
	case lxsptr^
	when '&' then
		++lxsptr
		nextlx.symbol:=daddrsym
!		nextlx.subcode:=0
	when '.' then
		++lxsptr
		nextlx.symbol:=anddotsym
!		nextlx.subcode:=0
	else
		nextlx.symbol:=addrsym
		nextlx.subcode:=jaddrof
	esac
	return

when '\'','`' then
	lxreadstring('\'')
	return

when '"' then
	lxreadstring('"')
	return

when ' ',tab then

when cr then
	++lxsptr				!skip lf
!	++nextlx.pos
!++NALLLINES
	nextlx.symbol:=eolsym
	return
when lf then			!only lfs not preceded by cr
!	++nextlx.pos
!++NALLLINES
	nextlx.symbol:=eolsym
	return

when etx,0 then
	nextlx.symbol:=eofsym
	--lxsptr
	return

else
	nextlx.symbol:=errorsym
	nextlx.value:=c
	return

end doswitch
!end switch
!od

end

proc lxreadstring(int termchar)=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter
!
!NOTE: "(For this to work, \w is changed to \wl, as two characters are generated)"
!I don't get that, as the CR,LF replace the \ and w respectively. So I've removed
!the need to have \wl

var ichar dest
var int c,d
var [8]char str

if termchar='"' then
	nextlx.symbol:=stringconstsym
!	nextlx.subcode:=tichar
else
	nextlx.symbol:=charconstsym
	nextlx.subcode:=tint
fi

!nextlx.symbol:=(termchar='"'|stringconstsym|charconstsym)
nextlx.svalue:=lxsptr

dest:=lxsptr				!form string into same buffer

do
	switch c:=lxsptr++^
	when '\\' then			!escape char
		c:=lxsptr^
		if c>='A'  and c<='Z' then c+:=' ' fi
		++lxsptr
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
			dest++^:=cr
			c:=lf
		when 'x' then	!2-digit hex code follows
			c:=0
			to 2 do
				case d:=lxsptr++^
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
!			println c,char(c),=nextlx.lineno
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
	when cr,lf,etx,0 then
!		cpl =nextlx.lineno
		lxerror("String not terminated")
!		--lxsptr
!		exit
	endswitch

	dest++^:=c
od
nextlxlength:=dest-nextlx.svalue
(nextlx.svalue+nextlxlength)^:=0
!(nextlx.svalue+(dest-nextlx.svalue))^:=0
end

proc readnumber(int base)=
!lxsptr positioned at first digit of number (could be separator)
!base is 2 to 10, or 16
var ref char pstart,dest
var int c
var ref char p

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
!	--lxsptr		!assume range
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
var ref char pstart,dest
var int c,n,base,suffix
var ref char p

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
	base:=nextlx.value
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
!read entire numbers, convert to real value in nextlx.xvalue
var ref char fractstart,ss
var int fractlen,expon,i,c,n
var real basex,x
const maxrealdigits=500
var [maxrealdigits]char realstr
var [32]char str

!CPL "READREAL",intlen

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

!	n:=sprintf(&.str,"%d",expon)
	strcpy(str,strint(expon))
	memcpy(ss+intlen+fractlen+2,&.str,strlen(str)+1)

	nextlx.symbol:=decimalconstsym
	nextlx.subcode:=tdecimal
	nextlx.svalue:=ss
	nextlxlength:=strlen(ss)
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

nextlx.symbol:=realconstsym
nextlx.subcode:=treal
nextlx.xvalue:=x
end

function readexponent(int base)int=
!positioned just after 'e' etc
!read exponent, which can have optional + or -, and return actual exponent value
var ref char numstart,numend
var int expon,length,neg

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
return (neg|-nextlx.value|nextlx.value)
end

global proc printsymbol(ref lexrec lp)=
var lexrec l
l:=lp^

printf("%-18s",symbolnames[l.symbol])

case l.symbol
when namesym then
!	print l.symptr^.name

	printstrn(l.symptr^.name,l.symptr^.namelen)
when intconstsym then
	case l.subcode
	when tint then print l.value,"int"
	when tword then print l.uvalue,"word"
	else print l.value
	esac

when realconstsym then
	print l.xvalue

when stringconstsym then
!	printf("\"%.*s\"",l.length,l.svalue)
	print """",$
!	printstrn(l.svalue,l.length)
	printstr(l.svalue)
	print $,""""
when charconstsym then
!	printf("'%.*s'",l.length,l.svalue)
	print "'",$
!	printstrn(l.svalue,l.length)
	printstr(l.svalue)
	print $,"'"
when decimalconstsym then
!	printstrn(l.svalue,l.length)
	printstr(l.svalue)
	print "L"
when assignsym,addrsym,ptrsym,deepcopysym,rangesym then
	print jtagnames[l.subcode]
elsif l.subcode then
	print "#",l.subcode
end

println

end

proc stringtonumber(ichar s, int length, base)=
!convert decimal number s to an i64 value
!s contains only digits
!for hex, then a..f and A..F have been converted to '9'+1 to '9'+6
var int64 a
var word64 b
var int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do		!trim leading zeros
	++s
	--length
od

nextlx.symbol:=intconstsym

if length>maxnumlen[base] or \
		(length=maxnumlen[base] and strncmp(s,maxnumlist[base],length)>0) then
	if base<>16 then
		lxerror("longint const")

	else
		if length>32 or \
			(length=32 and strncmp(s,"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0) then
			lxerror("longint non-base 10")
		else						!greater than 64 bits, up to 128 bits
LXERROR("128-BIT CONSTS")

!			if length=32 and strncmp(s,"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0 then
!				nextlx.subcode:=tword128
!			else
!				nextlx.subcode:=tint128
!			fi
!
!			nextlx.pvalue128:=stringtonumber128(s,length,16)
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

nextlx.value:=a

nextlx.subcode:=setinttype(a)
end

proc stringtodecimalnumber(ichar s, int length,suffix=0)=
var int64 a
var word64 b
var int c

!trim leading zeros, which make it difficult to do a string match with maxstr
while length>=2 and s^='0' do		!trim leading zeros
	++s
	--length
od

nextlx.symbol:=intconstsym

if length>20 or \
		(length=20 and strncmp(s,"18446744073709551615",20)>0) or suffix then

	if length>39 or \
		(length=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
		if suffix='W' then
			lxerror("-W overflows 128 bits")
		fi
dolongint::
		nextlx.symbol:=decimalconstsym
		nextlx.subcode:=tdecimal
		nextlx.svalue:=pcm_copyheapstringn(s,length)
		nextlxlength:=length
!CPL "DEC",=LENGTH
		
!		lxerror("longint const")
	else						!greater than 64 bits, up to 128 bits

		if suffix='L' then goto dolongint fi

LXERROR("128-BIT CONSTS2")

!		if (length=39 and strncmp(s,"170141183460469231731687303715884105727",39)>0) then
!			nextlx.subcode:=tword128
!		else
!			nextlx.subcode:=tint128
!		fi

!		nextlx.pvalue128:=stringtonumber128(s,length,10)
	fi
	return
fi

a:=0

to length do
	a:=a*10+s++^-'0'
od

nextlx.value:=a

nextlx.subcode:=setinttype(a)
end

global proc lexinit=
!do one-time setup::
! clear the hash table and populated it with reserved words
! do maxnum support and such
var int i!,n
static var int n

!CPL "LEX INIT"

for i to maxnumlist.len do
	maxnumlen[i]:=strlen(maxnumlist[i])
od


!CPL "CLEAR HASHTABLE"
memset(&hashtable,0,hashtable.bytes)
hashtablelast:=&hashtable[hstsize-1]
lastnameindex:=-1

inithashtable()


for i:=0 to 255 do
!	switch i
!	when 'A'..'Z','a'..'z','$','_','0'..'9' then
!		alphamap[i]:=1
!	end
!	switch i
!	when '0'..'9' then
!		digitmap[i]:=1
!	end
!	commentmap[i]:=1
	linecommentmap[i]:=1
!	spacemap[i]:=0
od
!CPL "LS4"

!commentmap['*']:=0
!commentmap[0]:=0
!commentmap[lf]:=0

linecommentmap[0]:=0
linecommentmap[cr]:=0
linecommentmap[lf]:=0
linecommentmap[etx]:=0

end

global proc printstrn(ichar s, int length)=
if length then
	printf("%.*s",length,s)
fi
end

proc printstr(ichar s)=
	print(s)
end

function scannumber(int base)ref char=
!lxsptr is at possible first digit of number sequence
!scan digits until non-digit
!return pointer to next char after compacted sequence
!sequence can be updated in-place (to close gaps caused by separators)
!start of sequence will be at lxsptr
var ref char dest
var int c

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
var ichar dest
var int c

nextlx.symbol:=stringconstsym
!nextlx.subcode:=tichar
nextlx.svalue:=++lxsptr

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
when cr,lf,etx,0 then
	lxerror("Raw string not terminated")
	--lxsptr
	exit
else
	dest++^:=c
enddoswitch
nextlxlength:=dest-nextlx.svalue
end

global function lookup(ichar name, int length, hashindex, sys)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in nextlxhashvalue
!return 1 (found) or 0 (not found)
!in either case, nextlx.symptr set to entry where name was found, or will be stored in
	var int j,wrapped,n
	var ref strec d
	var ref char s

!IF NOT SYS THEN CPL "LOOKUP",length:"v",NAME:".*",LENGTH, =HASHINDEX FI

	d:=&hashtable[hashindex]
	wrapped:=0

	do
		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
!IF NOT SYS THEN
!CPL "FOUND",NAME
!FI

			nextlx.symptr:=d
			nextlx.symbol:=d.symbolcode
			nextlx.subcode:=d.subcode
			return 1
		elsif n=0 then
			exit
		fi

		if ++d>hashtablelast then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			d:=&hashtable[0]
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr
!IF NOT SYS THEN CPL "LEX: NOT FOUND:",=D FI
	d^.name:=pcm_copyheapstringn(name,length)
	d^.namelen:=length
	d^.symbolcode:=namesym

	if not sys then
		if nameindex>=maxnames then
			lxerror("Too many names")
		fi

!		d^.lastname:=lastnameindex

		lastnameindex:=(ref byte(d)-ref byte(&hashtable[0]))/strec.bytes
	fi
	nextlx.symptr:=d
	nextlx.symbol:=d.symbolcode
	nextlx.subcode:=d.subcode

	return 0
end

!function stdlookup(int add)int=
!var int wrapped
!var ref strec d
!
!d:=&stdhashtable[nextlxhashvalue iand stdhstmask]
!
!wrapped:=0
!
!do
!	case d^.namelen
!	when 0 then
!		if not add then return 0 fi
!		exit
!	when nextlxlength then
!		if memcmp(d^.name,nextlx.svalue,nextlxlength)=0 then	!match
!			nextlx.symptr:=d
!			return 1
!		fi
!	esac
!
!	if ++d>stdhashtablelast then
!		if wrapped then
!			abortprogram("STDHASHTABLE FULL")
!		fi
!		wrapped:=1
!		d:=&stdhashtable[0]
!!		j:=0
!	fi
!od
!
!!exit when not found; new name will go in entry pointed to by lxsymptr
!
!d^.name:=nextlx.svalue
!d^.namelen:=nextlxlength
!d^.symbol:=rawnamesym
!nextlx.symptr:=d
!
!return 0
!end

global function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	var int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hsum<<4-hsum + c
	od
	return (hsum<<5-hsum) iand hstmask
end

proc inithashtable=
!populate hashtable with standard symbols
	var int i
	ichar name

	for i:=1 to stnames.len do
		addstname(stnames[i], stsymbols[i], stsubcodes[i])
	od

	for i to hostfnnames.upb when not hostinternal[i] do
		name:=hostfnnames[i]+5				!skip 'host_'
		addstname(name, khostfnsym, i)

	OD
!CPL "HASHTABLE AFTER INIT"
!FOR I IN HASHTABLE.BOUNDS DO
!	IF HASHTABLE[I].NAMELEN THEN
!		CPL I,":",HASHTABLE[I].NAME
!	FI
!OD
end

proc addstname(ichar name, int symbol, subcode)=
	if lookup(name,strlen(name),gethashvaluez(name),1) then
		println name
		abortprogram("Dupl ST entry")
	fi

	nextlx.symptr.symbolcode:=symbol
	nextlx.symptr.subcode:=subcode
end

proc lexreadline=
!read lex chars until eol
!returns with lxsptr pointing to what follows (crlf, etx etx)
!caller should remember lxsptr as start of text
!processing of next symbol deals with line counting

doswitch lxsptr^
when cr,lf then
	return
when etx,0 then
	--lxsptr
	return
else
	++lxsptr
enddoswitch
END

global proc startlex(ichar name,source,int moduleno)=
!Called at start of each new program. Note that a program can be:
! * The text of one module (not other modules or files will be involved)
! * The contents of any string in the host
! * The contents of any eval/exec call in a running qs program
!There will be multiple programs submitted throughout the host's run

!s is an etx and 0-terminated source string representing perhaps
!an entire file.
!Initial lex vars so that it is possible to start reading tokens from it
!(This lex system doesn't deal with include files so there no nested sourcefiles.
!There are only macro expansions which are dealt with locally.)

if nsources>=maxsources then
	lxerror("Too many sources")
fi

!CPL "***STARTLEX",SOURCE
!FOR I IN HASHTABLE.BOUNDS DO
!	IF HASHTABLE[I].NAMELEN THEN
!		CPL I,":",HASHTABLE[I].NAME
!	FI
!OD

!currmodulename:=pcm_copyheapstring(name)

!zero used entries in the user symbol table

!while lastnameindex>=0 do
!	hashtable[lastnameindex].namelen:=0
!
!	lastnameindex:=hashtable[lastnameindex].lastname
!od
!CPL "START LEX"
lastnameindex:=-1		!for linking within hashtable
nameindex:=0			!name ordinal value

!CPL "STARTLEX",REF VOID(SOURCE)

sourcename:=sourcenames[++nsources]:=pcm_copyheapstring(name)

lxsource:=lxsptr:=source
lxmoduleno:=moduleno
!nextlx.fileno:=nsources
!nextlx.lineno:=1

!CPL =LXSPTR

nextlx.symbol:=semisym
nextlx.subcode:=0
nextlx.moduleno:=moduleno
end

global function addnamestr(ichar name)ref strec=
	var lexrec oldlx
	var ref strec symptr

	oldlx:=nextlx
!	nextlxhashvalue:=gethashvaluez(name)

	nextlxlength:=strlen(name)
	nextlx.svalue:=pcm_alloc(nextlxlength+1)
	memcpy(nextlx.svalue,name,nextlxlength+1)
!CPL "ADDNAMESTR",NAME
	lookup(nextlx.svalue, nextlxlength, gethashvaluez(name),0)
	symptr:=nextlx.symptr

	nextlx:=oldlx

	return symptr
end

global proc PS1(ichar caption)=
!	print "PS:",,caption,,":"
	print caption,,":::"
	printsymbol(&lx)
end

global proc PS2(ichar caption)=
	print "	",,caption,,":##"
	printsymbol(&nextlx)
end

global proc PS(ichar caption)=
	PS1(caption)
end

global proc lex=
!return next token in lx, using lexreadtoken but working a token ahead.
!static int lastline=0
	var int lineno,n,dir,namelen
	var ref char p
	var ref strec symptr

	lx:=nextlx				!grab that already read basic token
	lxlength:=nextlxlength
!	lx.pos:=lxstart-lxsource
	lx.sourceoffset:=lxstart-lxsource

!CP "LEX:"; PRINTSYMBOL(&LX)

	reenter::

	lexreadtoken()			!read new token for next time around

!CP "LEX2:"; PRINTSYMBOL(&NEXTLX)


	reenter2::

	switch nextlx.symbol
!	when rawnamesym then					!identifier
!		LXERROR("LEX/RAWNAME")
	when unitnamesym then					!might be user identifier (points to generic entry)
		case lx.symbol
		when intconstsym then
!			if lx.subcode in [tint128, tword128] then
!				lxerror("No suffix on i128/u128")
!			fi
			case nextlx.symptr^.subcode
			when million_unit then lx.value *:= 1 million
			when billion_unit then lx.value *:= 1 billion
			when thousand_unit then lx.value *:= 1 thousand
			else
				lxerror("Can't do this unit index")
			esac
			lx.subcode:=setinttype(lx.value)
			goto reenter
		when realconstsym then
			lxerror("unit symbol after float?")
		else
			nextlx.symbol:=namesym				!convert to actual identifier
		esac

	when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,kforallsym,
			kdosym,ktosym,kprocsym,kfunctionsym,kimportdllsym,kunlesssym,
			krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
			ktrysym,ktabledatasym,kifsym then

		if lx.symbol=kendsym then
			lx.subcode:=nextlx.symbol			!turn end if to single end/if token
			goto reenter
		fi
!	when insym then
!		if lx.symbol=notlsym then
!			lx.subcode:=jnotin
!			goto reenter
!		fi
	when sysconstsym then					!ST ENTRY LIMITED TO 16 bits signed
		case nextlx.subcode
		when con_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint
		when true_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=1
			nextlx.subcode:=tint
		when false_const then
			nextlx.symbol:=intconstsym
			nextlx.value:=0
			nextlx.subcode:=tint
		when pi_const then
			nextlx.symbol:=realconstsym
!				nextlx.xvalue:=3.1415926535897932384626
			nextlx.xvalue:=pi
			nextlx.subcode:=treal
		when tab_const then
			nextlx.symbol:=stringconstsym
!				nextlx.subcode:=tichar
			nextlx.svalue:="\t"
			nextlxlength:=1
		else
			lxerror("sysconst?")
		esac

	when eolsym then

		switch lx.symbol
		when commasym, lsqsym, lbracksym, !ignore eol
			 assignsym,semisym then

			lexreadtoken()
			goto reenter2

			goto reenter
		elsif binopset[lx.symbol] then
			lexreadtoken()
			goto reenter2

		end switch
		nextlx.symbol:=semisym

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=jnotin
			goto reenter
		fi

!when stringconstsym then
!	if lx.symbol=stringconstsym then
!		n:=strlen(lx.svalue)+nextlxlength
!		p:=pcm_alloc(n+1)
!		memcpy(p,lx.svalue,lx.length)
!		memcpy(p+lx.length,nextlx.svalue,nextlx.length)
!		(p+n)^:=0
!		lx.svalue:=p
!		lx.length:=n
!		goto reenter
!!		lxerror("string/string")
!	fi
!

!	when insym then
!		if lx.symbol=notlsym then
!			lx.subcode:=jnotin
!			goto reenter
!		fi
	endswitch
end

global proc showhashtablesize=
var int i,n

n:=0
for i:=0 to hstmask do
	if hashtable[i].name then
		++n
	fi
od
end

global function checkname(ichar name,int length=0)int=
!nextlx contains a rawnamesym
!return if if it is the same as name
!length is the name length, or 0 (-1 better for empty strings) to work it out
!note that nextlx.svalue is not zero-terminated

if length=0 then
	length:=strlen(name)
fi
if nextlxlength=length and memcmp(nextlx.svalue,name,length)=0 then
	return 1
fi
return 0
end

!function getstrfile(ichar filename,int32 &length)ichar=
!!locate module within search paths for strinclude
!!return pointer to loaded/in-memory file (or nil on error)
!var ichar file
!static var [300]char filespec
!var int i
!
!for i to nsearchdirs do
!	strcpy(&.filespec,searchdirs[i])
!	strcat(&.filespec,filename)
!
!	if checkfile(&.filespec) then
!		file:=cast(readfile(&.filespec))
!		length:=rfsize
!		return file
!	fi
!od
!
!return nil
!end

function stringtonumber128(ichar s, int length,base)ref int128=
	int128 aa
	var int c,d

	aa:=0

!aa^.lower:=1

	to length do
		aa:=aa*base

		c:=s++^

		if c>='a' then
			d:=-'a'+10
		elsif c>='A' then
			d:=c-'A'+10
		else
			d:=c-'0'
		fi

		aa:=aa+d
	od

	return getheap128(aa)
end

function setinttype(word64 a)int=
	if a<u64(0x7FFF'FFFF'FFFF'FFFF) then
		return tint
	else
		return tword
	fi
end

global proc lxerror_s(ichar mess,a)=
	var [256]char str
!	sprintf(&.str,mess,a)
	fprint @str,mess,a
	lxerror(&.str)
end

proc do_name(ichar name, int length, hashindex)=
	ref strec d

!do quick lookup for when name already exists in ST, and matches at first try
!CPL "DO_NAME",LENGTH:"V",NAME:".*",=HASHINDEX


!	d:=&hashtable[hashindex]
!	if d.namelen=length then
!		if (length=1 and d.name^=name^) or memcmp(d.name,name,length)=0 then
!			nextlx.symptr:=d
!			nextlx.symbol:=d.symbolcode
!			nextlx.subcode:=d.subcode
!			return
!		fi
!	fi
	lookup(name,length, hashindex,0)
end

function getheap128(int128 a)ref int128 p=
	p:=pcm_alloc(16)
	p^:=a
	return p
end
=== qq_decls.m 7/32 ===
import clib
import qq_tables

!global const fixbytecodes=1		!convert bytecodes to handler addresses

global int dispatchtype=fn_dispatch
global int hasbytecodes=1			!depends on dispatchcode

global type unit      = ref unitrec
global type object    = ref objrec
global type symbol    = ref strec
global type strobject = ref stringobjrec
global type variant   = ref varrec
!global type intpc     = ref int

!global macro pr(a,b)=(a<<32 ior b)
global macro pr(a,b)=(a<<16 ior b)

!global const hasrefmask = 0x10000
global const hasrefmask = 0x100
global const varsize    = varrec.bytes

!global macro var_share(x) = obj_share(x) when x.hasref
!global macro var_unshare(x) = obj_unshare(x) when x.hasref

global record modulerec=
!	var int		modtype
	var ichar	name				!module name
	var ichar	sourcepath			!path+filename of module including extension
	var int		sourceloc			!DISK, INT or QA as a char const
	var ichar	source				!source text of module (may be overwritten)
	var ichar	originalsource		!original source
	var int		sourcelen			!number of chars in originak source
	var unit	ast					!ast for module-level code
	var int		parsed				!1 when already parsed
	var ref int	pcstart				!nil, or points to generated bytecode for whole module
	var ref int	pcend				!points to last allocated int
	var int		pcsize				!pcl size as number of allocated ints (some spare)
	var ref int32	pcsrcstart		!each entry is source-pos info (char offset into org source)

	var symbol	def					!associated stmodule entry
	var symbol	startfn				!nil, or st entry of start()
	var symbol	mainfn				!nil, or st entry of main()
	var [maxmodule] byte importmap	!visibility flags
	var int		moduleno			!need when only have a ref to modulerec
end

global record packfieldrec =
!	var ref packfieldrec nextfield
	var object structobj			!owner record
	var ichar name
	var int32 packmode				!index into tables
	var int32 offset				!byte offset
	var int32 size					!size
	var int32 length
end

!global record paramrec=
!	var symbol def
!	var ref paramrec nextparam
!	var ref int pcvalue				!not nil means default value label
!	var word16 mode					!for dllparams
!	var byte isoptional				!1 means optional param: use valuelab, or void
!	var byte index					!1,2,3...
!	var byte isvariadic
!end

global record procrec =
	symbol def
	ref procrec nextproc
end

!global record dllprocrec =
!	ichar name
!	int32 dllindex				!dll module index
!end

global record userxrec =
	symbol owner
	ref int16 pmode
	ref userxrec nextmode
end

record objhdrrec=
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3
	var word16	hdr4
end

global record objrec =
	union
		struct
			var word32	refcount
			var byte	flags : (mutable:1, fixedsize:1)
			var byte	objtype
			var int16	hdr4
			var [24]byte	uobject
		end

		listobjrec		ulist
		setobjrec		uset
		arrayobjrec		uarray
		bitsobjrec		ubits
		recordobjrec	urec
		stringobjrec	ustr
		structobjrec	ustruct
		dictobjrec		udict
		decimalobjrec	udec
	end
end

global record listobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3
	var word16	hdr4

	var variant	varptr

	var word32	length
	var int32	lower

	union
		var	word32	allocated
		var object objptr2
	end
end

global record decimalobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3
	var word16	hdr4

	ref void	bnptr

	int spare1,spare2
end

global record dictobjrec =
	var word32	refcount
	var byte	hdr2
	var byte	hdr3
	var int16	hdr4

	var variant	varptr

	var word32	length
	var int32	lower

	union
		struct
			var	word32	allocated
			var	word32	dictitems
		end
		var object objptr2
	end
end

global record recordobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3
	var int16	hdr4

	var variant	varptr

	var word32	length
	var int32	lower

	var object	recobj
end

global record structobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3
	var int16	packtype

	var ref byte	ptr

	var int		spare3

	var symbol	structdef
end

global record arrayobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3
	var int16	elemtype			!packtype

	var ref byte	ptr

	var word32	length
	var int32	lower

	union
		var	word32	allocated
		var object objptr2
	end
end

global record bitsobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3
	var int16	elemtype			!packtype

	var ref byte	ptr

	var word32	length
	var int16	lower
	var byte	bitoffset			!for refbit (+0/1/2/3/4/5/6/7, +0/2/4/6, +0/4)
	var byte	indexoffset			!for bits (add to index to index from bit 0)

	union
		var	word32	allocated
		var object objptr2
	end
end

global record setobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3				!slice/normal/etc, or int/bignum etc
	var int16	hdr4

	var ref byte	ptr

	var int		length

	union
		var	int64	allocated64
	end
end

global record stringobjrec =
	var word32	hdr1
	var byte	hdr2
	var byte	hdr3				!slice/normal/etc, or int/bignum etc
	var int16	hdr4

	var ichar	strptr

	var word32	length
	var int32	lower

	union
		var	word32	allocated
		var object objptr2
	end
end

record exceptionrec =
	union
		struct
			byte	tag
			byte	hasref
			byte	spare
			byte	exceptiontype
		end
	end
	struct
		int16 		frameoffset
		int16 		nexceptions
	end

	ref byte		ptr
end

record returnrec =
	union
		struct
			byte	tag
			byte	hasref
			byte	spare
			byte	stackadj
		end
	end
	int32			frameptr_low
	ref int			retaddr
end

record refrec =
	union
		struct
			byte	tag
			byte	hasref
			word16	spare
		end
	end
	struct
		word16		elemtag
		byte		bitoffset
		byte		bitlength		!for refbit/tbit: 0=1 bit, N=bitfield
	end

	union
		ref byte	ptr
		ref int64	ptr64
	end
end


global record varrec =
	union
		struct
			union
				struct
					byte	tag
					byte	hasref
					word16	usertag
				end
				word32		tagx
			end
			word32 			spare2
			union
				int64		value
				real64		xvalue
				word64		uvalue
				ichar		svalue
				struct
					int32	range_lower			!short range
					int32	range_upper
				end
				object		objptr				!objects where hasref=1
				variant		varptr				!for refvar
				ref byte	refptr				!for refproc etc
				symbol		def					!for tsymbol
			end
		end

		exceptionrec			uexcept
		returnrec				uret
		refrec					uref
!		operatorrec				uop
!		iterrec					uiter

	end
end


!==================================================

global record strec =

UNION
	STRUCT

	var ichar name				! name (likely shared pointer with generic entry)
	var symbol	owner
	var symbol	deflist			! first child name
	var symbol	deflistx		! points to last child

	var symbol	nextdef			! next name in this list
	var symbol	nextdupl		! next instance that shares the same name
	var symbol	firstdupl		! first or generic name entry
	var symbol	alias			! used for aliasid

!	var symbol	nextparam		! first or generic name entry

	union
		u64 a
		ref int pcaddress		!procs/labels
		variant varptr			!statics
		ichar truename			!dll procs
		symbol atfield			!fields
		int labelno				!proc/label label# before fixup
	end
	union
		u64 b
		unit code				!proc body/var initdata
		ref symbol topfieldlist		!structs; point to block of ttlength[mode] top fields
	end
	union
		u64 c
		int index				!frame/param/dllproc/enum/(const)
	end
	union
		u64 d
		struct
			int16 nparams		!procs/dllprocs
			int16 nlocals		!procs
		end
		struct
			int16 nfields		!records/structs
			int16 maxalign		!structs
			int32 fieldoffset
		end
		int genfieldindex		!generic
	end

	var word16	subcode
	var int16	moduleno
	var int16	mode
	var u16		flags: (isglobal:1, mstatic:1, misfunc:1, mbyref:1, menumx:1,
							moptional:1,  mvarparams:1, isframe:1,
							iscaligned:1)

	var byte	symbolcode
	var byte	nameid			! generic/static/proc etc

	var byte	mutable			! will be 1 for variables; 0 for modules procs, label etc
	var byte	namelen			! helps makes lookups faster
	var byte	procfixed		! 1 when procs have been fixedup
	END
!	[128]BYTE DUMMY128
END
end

global record lexrec =		!should be 32-byte record
	union
		var int64 value				!64-bit int
		var real xvalue				!64-bit float
		var word64 uvalue			!64-bit word
		var ichar svalue			!pointer to string or charconst (not terminated)
		var ref int128 pvalue128	!128-bit int/word
!		var symbol symptr			!pointer to symbol table entry for name
		var ref strec symptr			!pointer to symbol table entry for name
	end

!	var int32 hashvalue
!!	var int32 length					!length of name/string/char
!	var int32 pos: (lineno:24, fileno:8)
!	var word32 sourcepos
	var int32 pos: (sourceoffset:24, moduleno:8)

	var int16 symbol
	var int16 subcode
!	var byte fileno
end

global record uflagsrec =
	var [7]byte	codes
	var byte	ulength
end

global record fieldrec =
	var ichar name
	var int16 recordtype
	var int16 fieldtype
	var int32 fieldoffset
end

global record qint =	!store 128-bit signed or unsigned value
	var word64 lower
	var int64  upper
end

!global record strec =
!	var ichar name
!	var int32 lastname			!index of previous name
!
!	var byte namelen
!	var byte symbol
!
!	union
!		var int16 subcode		!for keywords
!		var word16 index		!sequential index for names
!		var word16 seqno		!sequential index for names
!	end
!end

!bit masks for .flags in unitrec, also used in pcl flag operands
!global const mglobal	= 0
!global const mstatic	= 1
!global const misfunc	= 2
!global const mbyref		= 3
!global const menumx		= 4
!global const moptional	= 5		!paramdef flag
!global const mcalign	= 6		!structdef
!global const mzerodefmask	= 256		!bit 8:paramdef flag

!global const mislet		= 4

global record unitrec =
	union
		struct
			var int16 tag
			union
!				var byte flags
!				var byte strtype			!for jmakestrtype
!				var byte arraycode			!for jmakeaxtype
				var byte elemtype			!for array constructors
			end
			union
				var byte nparams
				var byte enumindex
			end
			var int32 pos: (sourceoffset:24, moduleno:8)
		end
!		struct
!			byte moduleno
!			[3]byte spare
!		end
		ref void word1
	end

	var unit nextunit

	union
		struct
			union
				var unit a
				var symbol def
				var symbol labeldef
				var int64 value
				var word64 uvalue
				var real64 xvalue
				var ichar svalue
				var int64 range_lower
				var int64 qlower
!				var int opcode
			end
			union
				var unit b
				var int64 range_upper
				var int64 qupper
				var int64 slength
				var int16 mode
				var [4]byte cmpgenop
				struct
					var int32 length
					var int32 lower
				end
				var int64 index		!of enum name; or host index; or could be expr
			end
		end
		int128 value128
	end

!	[32]BYTE DUMMY

end

global var lexrec nextlx
global var lexrec lx
global const targetbits=64

global const maxsearchdirs=10
global var [maxsearchdirs]ichar searchdirs
global var int nsearchdirs=0

!global var int nstrings

!global var symbol stprogram
global var int qpos
global var int pcerrorpos
global ref modulerec pcerrormodule

!global const stacksize=1000
!global const stacksize=50
global const stacksize=30000
global var [stacksize]varrec varstack
global var variant sptr
global var variant stacklimit
global var variant stacktop
!global var variant frameptr
global var ref byte frameptr

!global var ref int pcstart
global var ref int pcptr
!global var int pcsize

global int stopped

global var symbol stprogram			!root of global symbol table
!global var symbol stmodule

global var symbol stcurrmodule		!current module during parse, name resolve, code gen
global var symbol stcurrproc		!current proc during parse, rx/cocde, or
									! set to stcurrmodule when outside a proc
global var ref modulerec currmodule	!set via stcurrmodule.moduleno

!global var symbol stcurrrunproc		!current proc being executed
!global var symbol stcurrprocdef		!current proc being defined
!global var symbol stcurrdllprocdef	!current dllproc being defined
!global var symbol stcurrrecorddef	!current record being defined
!global var int stcurrparsemodule	!current module being parsed
!global var symbol stcurrtypedef		!typedef..typedend
!global var symbol stcurrdllimport
!global var int pass2flag			!1 when generating pcl from ast

global var int debug
global var int COUNT

global const maxmodule=100
global var [maxmodule]modulerec moduletable
global var int nmodules

!global tabledata() [0:]ichar modtypenames=
!	(free_mod=0,	$),
!	(import_mod,	$),
!	(string_mod,	$),
!	(temp_mod,		$),
!end

global var int lastretindex



!global const maxpacktype=256
!
!global var [0:maxpacktype]byte ppcat		!tvoid/tint/tword/treal/tstring/trefpack/tpackrec/tarray
!
!global var [0:maxpacktype]int pptarget		!refpack:target, or array:element pack types; string=tp_u8
!
!global var [0:maxpacktype]int pplower		!lwb for array
!global var [0:maxpacktype]int pplength		!length for array
!global var [0:maxpacktype]int ppsize		!byte size of type
!global var [0:maxpacktype]object ppstructobj	!for a tstruct, tstructdef object
!global var int npacktypes

global unit lastast
global filehandle astdev

global var int inproc
GLOBAL INT NALLUNITS
GLOBAL INT NPROCUNITS
GLOBAL INT NALLPCL
GLOBAL INT NALLPROCS
GLOBAL INT NMAXPROCUNITS

!Errors
global [256]char errorline,errorpointer
global int errorlineno
global ref modulerec errormodule
global symbol sterrorproc

!Genfield Tables

global record genfieldrec=
	symbol def
	ref genfieldrec nextdef
end

global const maxgenfield=1000
global [maxgenfield]ref genfieldrec genfieldtable
global int ngenfields

global const maxdlllib=50
global const maxdllproc=2000

global int ndlllibs
global [maxdlllib]symbol dlltable
global [maxdlllib]u64 dllinsttable			!instance table

global int ndllprocs
global [maxdllproc]symbol dllproctable
global [maxdllproc]byte dlllibindex				!dll lib that proc belongs to
global [maxdllproc]ref void dllprocaddr			!pointer to external dll proc

global int fbundled
global int dointlibs

global tabledata() []ichar dispatchnames=
!	(lab_dispatch,		"-lab"),
	(fn_dispatch,		"-fn"),
	(sw_dispatch,		"-sw"),
	(debug_dispatch,	"-debug"),
	(fdebug_dispatch,	"-fdebug"),
	(asm_dispatch,		"-asm")
end

global const int maxcmdparam=32
global int ncmdparams
global [maxcmdparam]ichar cmdparamtable

global ichar err_message
global varrec err_var1, err_var2
global ref int err_pcptr

global ref int stopseq		!point to a 'stop 0' sequence
global ref int raiseseq		!point to a sequence of several 'raise' cmdcodes

global ref procrec proclist, proclistx
global int nproclist

global ref proc pcl_callbackfn=nil	!address of *PCL* function (pcdata address)

GLOBAL INT PCLLEVEL=0
=== qq_tables.m 8/32 ===
import qq_decls

!!---
global tabledata() [0:]ichar stdtypenames, [0:]byte stdtypewidths =
	(tvoid=0,		$,		128),	!- means variant is unassigned

!Numbers
	(tint,			"int",		64),	!- 64-bit signed int
	(tword,			"word",		64),	!- 64-bit unsigned int
	(treal,			"real",		64),	!- 64-bit float
	(tdecimal,		"dec",		0),
	(trange,		"range",	64),	!- 32+32-bit int:int

!-Composite objects
	(tstring,		"string",	0),		!O 8-bit string, flex and mutable
	(tset,			"set",		0),		!O Pascal-like bit-set
	(tdict,			"dict",		0),		!O Dictionary of X:Y keys and values
	(tlist,			"list",		0),		!O Array of variants
	(tarray,		"array",	0),		!O Array of packed
	(tbits,			"bits",		0),		!O Array if bits

	(trecord,		"record",	0),		!O * Record of variants
	(tstruct,		"struct",	0),		!O * Record of packed
	(tcarray,		"carray",	0),		!O * User-type array of packed

!-Special
	(ttype,			"type",		64),	!- Represents a type-code
	(toperator,		"operator",	64),	!- Represents an operator (as a bytecode op)
	(treturn,		"return",	0),		!- Return address descriptor, only on stack 
	(texception,	"except",	0),		!- Exception descriptor, only on stack
	(tsymbol,		"symbol",	64),	!- Named object

!-Pointers
	(trefvar,		"refvar",	64),	!- Reference to variant
	(trefpack,		"refpack",	64),	!- [*] Reference to packed type
	(trefbit,		"refbit",	64),	!- Reference to bit/bitfield

!-Packed types - numeric
	(tpi8,			"i8",		8),
	(tpi16,			"i16",		16),
	(tpi32,			"i32",		32),
	(tpi64,			"i64",		64),
	(tpi128,		"i128",		128),

	(tpu1,			"u1",		1),
	(tpu2,			"u2",		2),
	(tpu4,			"u4",		4),
	(tpu8,			"u8",		8),
	(tpu16,			"u16",		16),
	(tpu32,			"u32",		32),
	(tpu64,			"u64",		64),
	(tpu128,		"u128",		128),

	(tpr32,			"r32",		32),
	(tpr64,			"r64",		64),

!-Packed types - string fields
	(tpstringc,		"stringc",	0),		!- counted string field (uses get/setfs)
	(tpstringz,		"stringz",	0),		!- zero-terminated string field

!-Packed types - pointers (a second field may have has packed type of target/element)

	(tpcstring,		"cstring",	64),
	(tparray,		"parray",	64),
	(tpstruct,		"pstruct",	64),
	(tpref,			"pref",		64),
	(tpvar,			"ivar",		64),
end

global const tlast=stdtypenames.upb

global const tlastvartag=trefbit

global const tpvoid = tvoid

!!some common composite types
!global tabledata() []ichar twintypenames =
!	(tintint,			$),
!	(tintword,			$),
!	(tintreal,			$),
!	(tintdecimal,		$),
!
!	(twordint,			$),
!	(twordword,			$),
!	(twordreal,			$),
!	(tworddecimal,		$),
!
!	(trealint,			$),
!	(trealword,			$),
!	(trealreal,			$),
!	(trealdecimal,		$),
!
!	(tdecimalint,		$),
!	(tdecimalword,		$),
!	(tdecimalreal,		$),
!	(tdecimaldecimal,	$),
!
!	(trefpackint,		$),
!
!	(tstringstring,		$),
!	(tstringint,		$),
!	(tlistint,			$),
!end
!
!!conversion matrix from 2 base types (tvoid..tlastvartag) to twintypecode
!!this needs to be populated by an init routine that scans twintypesetup[]
!global [0..tlastvartag, 0..tlastvartag]byte twintypetable
!
![,3]byte twintypesetup=(
!	(tint,		tint,		tintint),
!	(tint,		tword,		tintword),
!	(tint,		treal,		tintreal),
!	(treal,		tint,		trealint),
!	(0,0,0))

global tabledata() [0:]ichar jtagnames,			! default name or "+" etc
					[0:]byte jflags,			! 0/1/2 = 0, 1 or 2 subtrees
					[0:]int16 jpclcodes,		! for arith, corresponding pcl opc
					[0:]int16 jtocodes, 		! for arith, corresponding jaddto op etc
					[0:]byte jhasvalue = 		! whether yields a value (0, 1 or 2=special)
	(jnone=0,			$,		0,		0,			0,	0),	
	(jlabeldef,			$,		1,		0,			0,	0),
	(jassign,			$,		2,		0,			0,	2),
	(jdeepcopy,			$,		2,		0,			0,	2),
	(jkeyword,			$,		2,		0,			0,	1),
	(jkeyvalue,			$,		2,		0,			0,	1),
	(jdocstring,		$,		1,		0,			0,	0),
	(jblock,			$,		1,		0,			0,	2),
	(jif,				$,		2,		0,			0,	2),
	(jselect,			$,		2,		0,			0,	2),
	(jwhenthen,			$,		2,		0,			0,	0),
	(jcase,				$,		2,		0,			0,	2),
	(jdocase,			$,		2,		0,			0,	0),
	(jswitch,			$,		2,		0,			0,	2),
	(jdoswitch,			$,		2,		0,			0,	0),
	(jrecase,			$,		1,		0,			0,	0),
	(jforup,			$,		2,		0,			0,	0),
	(jforupx,			$,		2,		0,			0,	0),
	(jfordown,			$,		2,		0,			0,	0),
	(jfordownx,			$,		2,		0,			0,	0),
	(jforall,			$,		2,		0,			0,	0),
	(jforallrev,		$,		2,		0,			0,	0),
	(jforeach,			$,		2,		0,			0,	0),
	(jdo,				$,		1,		0,			0,	0),
	(jdoonce,			$,		1,		0,			0,	0),
	(jto,				$,		2,		0,			0,	0),
	(jwhile,			$,		2,		0,			0,	0),
	(jrepeat,			$,		2,		0,			0,	0),
	(jtry,				$,		2,		0,			0,	0),
	(jexcept,			$,		2,		0,			0,	0),
	(jraise,			$,		1,		0,			0,	0),
	(jcall,				$,		2,		0,			0,	1),
	(jcallhost,			$,		1,		0,			0,	1),
	(jnil,				$,		0,		0,			0,	1),
	(jpnil,				$,		0,		0,			0,	0),
	(jswap,				$,		2,		0,			0,	0),
	(jgoto,				$,		1,		0,			0,	0),
	(jstop,				$,		1,		0,			0,	0),
	(jreturn,			$,		1,		0,			0,	2),
	(jtypeconst,		$,		0,		0,			0,	1),
	(jtypename,			$,		1,		0,			0,	1),
	(jconvert,			$,		1,		0,			0,	1),
	(jtypepun,			$,		1,		0,			0,	1),
	(jcmpchain,			$,		1,		0,			0,	1),
	(jname,				$,		0,		0,			0,	1),
	(jsymbol,			$,		1,		0,			0,	1),
	(jhostname,			$,		0,		0,			0,	1),
	(jintconst,			$,		0,		0,			0,	1),
	(jwordconst,		$,		0,		0,			0,	1),
	(jint128const,		$,		0,		0,			0,	1),
	(jword128const,		$,		0,		0,			0,	1),
	(jrealconst,		$,		0,		0,			0,	1),
	(jstringconst,		$,		0,		0,			0,	1),
	(jdot,				$,		2,		0,			0,	1),
	(jindex,			$,		2,		0,			0,	1),
	(jdotindex,			$,		2,		0,			0,	1),
	(jkeyindex,			$,		2,		0,			0,	1),
	(jredo,				$,		2,		0,			0,	0),
	(jnext,				$,		2,		0,			0,	0),
	(jexit,				$,		2,		0,			0,	0),
	(jptr,				$,		1,		0,			0,	1),
	(jaddrof,			$,		1,		0,			0,	1),
	(jptrto,			$,		1,		0,			0,	1),
	(jdaddrof,			$,		1,		0,			0,	1),
	(jnull,				$,		0,		0,			0,	1),
	(jprint,			$,		2,		0,			0,	0),
	(jprintln,			$,		2,		0,			0,	0),
	(jfprint,			$,		2,		0,			0,	0),
	(jfprintln,			$,		2,		0,			0,	0),
	(jcprint,			$,		2,		0,			0,	0),
	(jcprintln,			$,		2,		0,			0,	0),
	(jsprint,			$,		2,		0,			0,	1),
	(jsfprint,			$,		2,		0,			0,	1),
	(jnogap,			$,		0,		0,			0,	0),
	(jspace,			$,		0,		0,			0,	0),
	(jfmtitem,			$,		2,		0,			0,	0),
	(jread,				$,		2,		0,			0,	0),
	(jreadln,			$,		2,		0,			0,	0),
	(jnew,				$,		2,		0,			0,	1),
	(jdecimal,			$,		0,		0,			0,	1),
	(jincr,				$,		1,		0,			0,	1),
	(jdecr,				$,		1,		0,			0,	1),
	(jincrload,			$,		1,		kincrload,	0,	1),
	(jdecrload,			$,		1,		kdecrload,	0,	1),
	(jloadincr,			$,		1,		kloadincr,	0,	1),
	(jloaddecr,			$,		1,		kloaddecr,	0,	1),
	(jneg,				"-",	1,		kneg,		0,	1),
	(jabs,				$,		1,		kabs,		0,	1),
	(jnotl,				$,		1,		knotl,		0,	1),
	(jinot,				$,		1,		kinot,		0,	1),
	(jistruel,			$,		1,		kistruel,	0,	1),
	(jasc,				$,		1,		kasc,		0,	1),
	(jchr,				$,		1,		kchr,		0,	1),
	(jsqrt,				$,		1,		ksqrt,		0,	1),
	(jsqr,				$,		1,		ksqr,		0,	1),
	(jsin,				$,		1,		ksin,		0,	1),
	(jcos,				$,		1,		kcos,		0,	1),
	(jtan,				$,		1,		ktan,		0,	1),
	(jasin,				$,		1,		kasin,		0,	1),
	(jacos,				$,		1,		kacos,		0,	1),
	(jatan,				$,		1,		katan,		0,	1),
	(jln,				$,		1,		kln,		0,	1),
	(jlog,				$,		1,		klog,		0,	1),
	(jlg,				$,		1,		klg,		0,	1),
	(jexp,				$,		1,		kexp,		0,	1),
	(jround,			$,		1,		kround,		0,	1),
	(jfloor,			$,		1,		kfloor,		0,	1),
	(jceil,				$,		1,		kceil,		0,	1),
	(jfract,			$,		2,		kfract,		0,	1),
	(jfmod,				$,		2,		kfmod,		0,	1),
	(jsign,				$,		1,		ksign,		0,	1),
	(jnegto,			$,		1,		knegto,		0,	0),
	(jabsto,			$,		1,		kabsto,		0,	0),
	(jnotlto,			$,		1,		knotlto,	0,	0),
	(jinotto,			$,		1,		kinotto,	0,	0),
	(jlen,				$,		1,		klen,		0,	1),
	(jlwb,				$,		1,		klwb,		0,	1),
	(jupb,				$,		1,		kupb,		0,	1),
	(jbounds,			$,		1,		kbounds,	0,	1),
	(jboundsx,			$,		1,		kboundsx,	0,	1),
	(jbitwidth,			$,		1,		kbitwidth,	0,	1),
	(jbytesize,			$,		1,		kbytesize,	0,	1),
	(jtype,				$,		1,		ktype,		0,	1),
	(jelemtype,			$,		1,		kelemtype,	0,	1),
	(jbasetype,			$,		1,		kbasetype,	0,	1),
	(jdictitems,		$,		1,		kdictitems,	0,	1),
	(jminvalue,			$,		1,		kminvalue,	0,	1),
	(jmaxvalue,			$,		1,		kmaxvalue,	0,	1),
	(jisint,			$,		1,		kisint,		0,	1),
	(jisreal,			$,		1,		kisreal,	0,	1),
	(jisstring,			$,		1,		kisstring,	0,	1),
	(jisrange,			$,		1,		kisrange,	0,	1),
	(jisnumber,			$,		1,		kisnumber,	0,	1),
	(jislist,			$,		1,		kislist,	0,	1),
	(jisrecord,			$,		1,		kisrecord,	0,	1),
	(jispointer,		$,		1,		kispointer,	0,	1),
	(jisarray,			$,		1,		kisarray,	0,	1),
	(jismutable,		$,		1,		kismutable,	0,	1),
	(jisset,			$,		1,		kisset,		0,	1),
	(jisvoid,			$,		1,		kisvoid,	0,	1),
	(jisdef,			$,		1,		kisdef,		0,	1),
	(jtostr,			$,		1,		ktostr,		0,	1),
	(jisequal,			$,		2,		kisequal,	0,	1),
	(jadd,				"+",	2,		kadd,		jaddto,	1),
	(jsub,				"-",	2,		ksub,		jsubto,	1),
	(jmul,				"*",	2,		kmul,		jmulto,	1),
	(jdiv,				"/",	2,		kdiv,		jdivto,	1),
	(jidiv,				"%",	2,		kidiv,		jidivto,	1),
	(jirem,				$,		2,		kirem,		0,	1),
	(jidivrem,			$,		2,		kidivrem,	0,	1),
	(jiand,				$,		2,		kiand,		jiandto,	1),
	(jior,				$,		2,		kior,		jiorto,	1),
	(jixor,				$,		2,		kixor,		jixorto,	1),
	(jshl,				$,		2,		kshl,		jshlto,	1),
	(jshr,				$,		2,		kshr,		jshrto,	1),
	(jin,				$,		2,		kin,		0,	1),
	(jnotin,			$,		2,		knotin,		0,	1),
	(jinrev,			$,		2,		0,			0,	0),
	(jandl,				$,		2,		kandl,		jandlto,	1),
	(jorl,				$,		2,		korl,		jorlto,	1),
	(jxorl,				$,		2,		kxorl,		0,	1),
	(jeq,				$,		2,		keq,		0,	1),
	(jne,				$,		2,		kne,		0,	1),
	(jlt,				$,		2,		klt,		0,	1),
	(jle,				$,		2,		kle,		0,	1),
	(jge,				$,		2,		kge,		0,	1),
	(jgt,				$,		2,		kgt,		0,	1),
	(jmin,				$,		2,		kmin,		jminto,	1),
	(jmax,				$,		2,		kmax,		jmaxto,	1),
	(jconcat,			$,		2,		kconcat,	jconcatto,	1),
	(jappend,			$,		2,		kappend,	jappendto,	1),
	(jprepend,			$,		2,		kprepend,	0,	1),
	(jpower,			$,		2,		kpower,		0,	1),
	(jatan2,			$,		2,		katan2,		0,	1),
	(jaddto,			$,		2,		kaddto,		0,	0),
	(jsubto,			$,		2,		ksubto,		0,	0),
	(jmulto,			$,		2,		kmulto,		0,	0),
	(jdivto,			$,		2,		kdivto,		0,	0),
	(jidivto,			$,		2,		kidivto,	0,	0),
	(jandlto,			$,		2,		kandlto,	0,	0),
	(jorlto,			$,		2,		korlto,		0,	0),
	(jiandto,			$,		2,		kiandto,	0,	0),
	(jiorto,			$,		2,		kiorto,		0,	0),
	(jixorto,			$,		2,		kixorto,	0,	0),
	(jshlto,			$,		2,		kshlto,		0,	0),
	(jshrto,			$,		2,		kshrto,		0,	0),
	(jminto,			$,		2,		kminto,		0,	0),
	(jmaxto,			$,		2,		kmaxto,		0,	0),
	(jconcatto,			$,		2,		kconcatto,	0,	0),
	(jappendto,			$,		2,		kappendto,	0,	0),

	(jmakerange,		$,		2,		kmakerange,	0,	1),
	(jmakerangelen,		$,		2,		kmakerangelen,	0,	1),
	(jmakelist,			$,		1,		kmakelist,	0,	1),
	(jmakeset,			$,		1,		kmakeset,	0,	1),
	(jmakedict,			$,		1,		kmakedict,	0,	1),
end

!Foreign function Specifiers
global tabledata() [0:]ichar fflangnames=

	(noff=0,		$), ! 
	(windowsff,		$), ! 
	(clangff,		$), ! 
	(mlangff,		$), ! 
	(callbackff,	$), ! 

	(dummyff,		$) ! 
end

global type qd=[4]byte

global tabledata() [0:]ichar opndnames=
							!PCL1			PCL2
	(cnone=0,	$),
	(cmemory,	$),			!m Symbol		Address of static object
	(cframe,	$),			!f Symbol		Byte offset from Dframe
	(cproc,		$),			!p Symbol		Address of pccode entry point
	(cdllproc,	$),			!x Int			Int Index into dllproc table

	(cgenfield,	$),			!g Symbol		Index into genfieldtable

	(clabel,	$),			!l Label no		Address of pccode instruction
	(cint,		$),			!i
	(cword,		$),			!u
	(creal,		$),			!r
	(crange,	$),			!n
	(cstring,	$),			!s Stringz		Address of static Object with string
	(cstringz,	$),			!z Stringz
	(ctype,		$),			!t Typeno		Typeno
	(csymbol,	$),			!d Symbol		Symbol

	(clast,		"?")
end

!these aliases are used so that the cmdfmt table is tidier
const p = cproc
const m = cmemory
const f = cframe
const l = clabel
const x = cdllproc
const g = cgenfield
const i = cint
const u = cword
const r = creal
const n = crange
const s = cstring
const z = cstringz
const t = ctype
const d = csymbol

!Stack operands labeled X,Y,Z::
!X		X is top of the stack (1 operand)
!X,Y	Y is top of the stack (2 operands)
!X,Y,Z	Z is top of the stack (3 operands)
!suffixes a,b,c help indicate which operand goes where::
!a		always top of the second
!b		always second from the top
!c		always third from the top
!So Xb and Ya when there are two operands; Y is on top
!flags abcB uses a '1' bit to indicate that .a, .b or .c contains a unit list

!!----

global tabledata()  [0:]ichar pclnames, [0:]qd pclfmt =
	(kzero=0,		$,	qd(0,0,0,0)),
	(knop,			$,	qd(0,0,0,0)),		!simple nop

	(krunstart,		$,	qd(i,0,0,0)),		!Main: run main/start; other: run start
	(kstartmodule,	$,	qd(0,0,0,0)),
	(kprocdef,		$,	qd(d,0,0,0)),		!
	(kprocentry,	$,	qd(i,0,0,0)),		!A=number of locals; 
	(kprocend,		$,	qd(0,0,0,0)),
	(kprocparams,	$,	qd(i,0,0,0)),		!no. params of following procentry
	(kendmodule,	$,	qd(0,0,0,0)),		!Last 'executable' opcode
	(kcomment,		$,	qd(z,0,0,0)),

!	(kkeyword,		$,	qd(0,0,0,0)),		!

	(ktypename,		$,	qd(d,0,0,0)),		!turn name to user type

	(klabeldef,		$,	qd(d,0,0,0)),		!
	(klabel,		$,	qd(i,0,0,0)),		!

	(kpushm,		$,	qd(m,0,0,0)),		!Push [A]
	(kpushf,		$,	qd(f,0,0,0)),		!Push [A]
	(kpushmref,		$,	qd(m,0,0,0)),		!push &A
	(kpushfref,		$,	qd(f,0,0,0)),		!push &A
!	(kpushap,		$,	qd(d,0,0,0)),		!push &A (proc)
	(kpopm,			$,	qd(m,0,0,0)),		!A:=Xa
	(kpopf,			$,	qd(f,0,0,0)),		!A:=Xa

!	(kpushproc,		$,	qd(d,0,0,0)),		!push proc A
	(kpushlabel,	$,	qd(l,0,0,0)),		!push label A
	(khostname,		$,	qd(d,0,0,0)),		!find named host proc A, and push hostproc object

	(kpushci,		$,	qd(i,0,0,0)),		!Push constant signed int
	(kpushci128,	$,	qd(i,i,0,0)),		!Push constant signed int
	(kpushcu,		$,	qd(i,0,0,0)),		!Push constant unsigned int
	(kpushcu128,	$,	qd(i,i,0,0)),		!Push constant unsigned int
	(kpushvoid,		$,	qd(0,0,0,0)),		!
	(kpushnil,		$,	qd(0,0,0,0)),		!
	(kpushpnil,		$,	qd(0,0,0,0)),		!
	(kpushcw,		$,	qd(u,0,0,0)),		!Push constant unsigned int
	(kpushcr,		$,	qd(r,0,0,0)),		!Push constant real
	(kpushcn,		$,	qd(n,0,0,0)),		!Push range

	(kpushcs,		$,	qd(s,0,0,0)),		!Push constant string object

	(kpusht,		$,	qd(t,0,0,0)),		!Push type constant
	(kpushtp,		$,	qd(t,0,0,0)),		!Push packtype constant
	(kpushop,		$,	qd(i,i,0,0)),		!Push operator constant; i is 1 or 2 operands expected
	(kpushsymbol,	$,	qd(d,0,0,0)),		!Push symbol reference

	(kpushptr,		$,	qd(0,0,0,0)),		!Push Xa^
	(kpopptr,		$,	qd(0,0,0,0)),		!Ya^:=Xb; then pop both
	(kstoreptr,		$,	qd(0,0,0,0)),		!Ya^:=Xb; keep Xb on stack (as Xa)
	(kpoplist,		$,	qd(i,0,0,0)),		!Expand Xn+1 to N (A) lv items on stack

	(kzpopm,		$,	qd(m,0,0,0)),		!Pop A; do not free A first
	(kzpopf,		$,	qd(f,0,0,0)),		!Pop A; do not free A first
	(kzstorem,		$,	qd(m,0,0,0)),		!Store A; do not free A first
	(kzstoref,		$,	qd(f,0,0,0)),		!Store A; do not free A first

	(kdupl,			$,	qd(0,0,0,0)),		!Xa:=share(Xa), keep original on stack
	(kcopy,			$,	qd(0,0,0,0)),		!Xa:=deepcopy(Xa)
	(kswap,			$,	qd(0,0,0,0)),		!Yb^:=:Xa^; Xa^:=:A; A:=:B

	(kconvrefpack,	$,	qd(0,0,0,0)),		!Change ref in X to refpacked

	(kjump,			$,	qd(l,0,0,0)),		!Jump to L
	(kjumpname,		$,	qd(d,0,0,0)),		!Jump to A
	(kjumpptr,		$,	qd(0,0,0,0)),		!Jump to Xa^

	(kjumptrue,		$,	qd(l,0,0,0)),		!Jump to L when Xa is true
	(kjumpfalse,	$,	qd(l,0,0,0)),		!Jump to L when Xa is false

	(kjumpdef,		$,	qd(l,0,0,0)),		!Jump to L when Xa defined (X popped)
	(kjumpvoid,		$,	qd(l,0,0,0)),		!Jump to L when Xa is void

	(kjumpeq,		$,	qd(l,0,0,0)),		!Jump to L when Xb=Ya, Xa=A, A=B; (X,Y popped)
	(kjumpne,		$,	qd(l,0,0,0)),		!Jump to L when Xb<>Ya
	(kjumplt,		$,	qd(l,0,0,0)),		!Jump to L when Xb<Ya
	(kjumple,		$,	qd(l,0,0,0)),		!Jump to L when Xb<=Ya
	(kjumpge,		$,	qd(l,0,0,0)),		!Jump to L when Xb>=Ya
	(kjumpgt,		$,	qd(l,0,0,0)),		!Jump to L when Xb>Ya

	(kjumptesteq,	$,	qd(l,0,0,0)),		!Jump to L when Xb=Ya (Ya popped), or Xa=A; int/set and int/range use 'in' to compare
	(kjumptestne,	$,	qd(l,0,0,0)),		!Jump to L when Xb<>Ya

	(kjumplabel,	$,	qd(l,0,0,0)),		!Jumptable entry
	(kjumpclabel,	$,	qd(l,i,0,0)),		!Jumptable entry with value P (a cint)

	(kswitch,		$,	qd(i,i,0,0)),		!Jumptable has n entries, ci is lower bound. Jump indexed by Xa
	(kselect,		$,	qd(i,i,0,0)),		!Jumptable has n entries, ci is lower bound. Jump indexed by Xa

	(kcswitch,		$,	qd(i,i,i,0)),		!Jumptable has n (label,value) entries, plus 'else' entry. Search for Xa value and jump to label

	(ktom,			$,	qd(l,m,0,0)),		!
	(ktof,			$,	qd(l,f,0,0)),		!

	(kformci,		$,	qd(l,m,i,0)),		!
	(kforfci,		$,	qd(l,f,i,0)),		!
	(kformm,		$,	qd(l,m,m,0)),		!
	(kforff,		$,	qd(l,f,f,0)),		!

	(kfordmci,		$,	qd(l,m,i,0)),		!
	(kfordfci,		$,	qd(l,f,i,0)),		!
	(kfordmm,		$,	qd(l,m,m,0)),		!
	(kfordff,		$,	qd(l,f,f,0)),		!

!	(kcallfn,		$,	qd(p,i,0,0)),		!Call &A; A is cmemoryref; B is no. args
	(kcallproc,		$,	qd(p,i,0,0)),		!Call &A; A is cmemoryref; B is no. args
	(kcallptr,		$,	qd(i,i,0,0)),		!Call X^; A is no. of params supplied; B is stack adjust
	(kcalllab,		$,	qd(l,0,0,0)),		!Call label A
	(kreturn,		$,	qd(0,0,0,0)),		!Return from function, with optional value in caller's 	$retval
	(kpopretval,	$,	qd(i,0,0,0)),		!pop stack to caller's return slot; i=offset

!	(kfastcall,		$,	qd(l,0,0,0)),		!
!	(kfastreturn,	$,	qd(0,0,0,0)),		!
	(kmodulecall,	$,	qd(d,0,0,0)),		!
	(kmodulereturn,	$,	qd(0,0,0,0)),		!

	(kcalldll,		$,	qd(x,i,t,0)),		!Call dll function m; i=0/1=c/windows; t=result type (void for procs)

	(kcallhost,		$,	qd(i,0,0,0)),		!Call indexed, named host function &A/B

	(kunshare,		$,	qd(i,0,0,0)),		!Unshare and pop A var values on stack
	(kaddsp,		$,	qd(i,0,0,0)),		!SP+:=A; note: positive A will push, negative will pop (reverse of the hardware)

	(kstop,			$,	qd(0,0,0,0)),		!Stop program and return value X to any calling program
	(kstoprunproc,	$,	qd(0,0,0,0)),		!Used for reentrant callback calls
	(ktest,			$,	qd(0,0,0,0)),		!Various tests on X etc

	(kmakelist,		$,	qd(i,i,0,0)),		!A items on stack; make list with lwb B
	(kmakerecord,	$,	qd(i,t,0,0)),		!A items on stack; make record of type B
	(kmakearray,	$,	qd(i,i,t,t)),		!A items on stack; make array with lwb B, type C and elemtype D
	(kmakebits,		$,	qd(i,i,t,t)),		!A items on stack; make bits with lwb B, type C and elemtype D
	(kmakestruct,	$,	qd(i,t,0,0)),		!A items on stack; make struct with type B
	(kmakeset,		$,	qd(i,0,0,0)),		!A items on stack; make set
	(kmakerange,	$,	qd(0,0,0,0)),		!2 items on stack; make range
	(kmakerangelen,	$,	qd(0,0,0,0)),		!2 items on stack; make range; 2nd is length
	(kmakedict,		$,	qd(i,0,0,0)),		!A*2 items on stack (A key:val items); make dict
	(kmakedecimal,	$,	qd(0,0,0,0)),		!Turn string on stack to decimal number

	(ksoftconv,		$,	qd(t,0,0,0)),		!T:=A(Xa); T:=B(A); Type conversion; can only be a basic conversion (usually implicit)
	(khardconv,		$,	qd(t,0,0,0)),		!T:=A(Xa); T:=B(A); Type conversion; any conversion can be done provided it's possible (usually explicit)

	(kmixed,		$,	qd(0,0,0,0)),		!++Xa
	(kincrptr,		$,	qd(0,0,0,0)),		!++Xa^
	(kincrtom,		$,	qd(m,0,0,0)),		!++A
	(kincrtof,		$,	qd(f,0,0,0)),		!++A
	(kloadincr,		$,	qd(0,0,0,0)),		!T:=Xa^++
	(kincrload,		$,	qd(0,0,0,0)),		!T:=--Xa^

	(kdecrptr,		$,	qd(0,0,0,0)),		!--Xa^; pop X
	(kdecrtom,		$,	qd(m,0,0,0)),		!--A
	(kdecrtof,		$,	qd(f,0,0,0)),		!--A
	(kloaddecr,		$,	qd(0,0,0,0)),		!T:=Xa^--
	(kdecrload,		$,	qd(0,0,0,0)),		!T:=--Xa^

	(kincr,			$,	qd(0,0,0,0)),		!T:=++T
	(kdecr,			$,	qd(0,0,0,0)),		!T:=--T

	(kneg,			$,	qd(0,0,0,0)),		!T:=-Xa; T:=-A
	(kabs,			$,	qd(0,0,0,0)),		!abs Xa
	(knotl,			$,	qd(0,0,0,0)),		!not Xa
	(kinot,			$,	qd(0,0,0,0)),		!inot Xa
	(kistruel,		$,	qd(0,0,0,0)),		!istrue Xa
	(kasc,			$,	qd(0,0,0,0)),		!asc Xa
	(kchr,			$,	qd(0,0,0,0)),		!chr Xa

	(ksqrt,			$,	qd(0,0,0,0)),		!sqrt Xa
	(ksqr,			$,	qd(0,0,0,0)),		!sqr Xa
	(kcube,			$,	qd(0,0,0,0)),		!cube Xa
	(ksin,			$,	qd(0,0,0,0)),		!sin Xa
	(kcos,			$,	qd(0,0,0,0)),		!cos Xa
	(ktan,			$,	qd(0,0,0,0)),		!tan Xa
	(kasin,			$,	qd(0,0,0,0)),		!asin Xa
	(kacos,			$,	qd(0,0,0,0)),		!acos Xa
	(katan,			$,	qd(0,0,0,0)),		!atan Xa
	(ksign,			$,	qd(0,0,0,0)),		!sign Xa
	(kln,			$,	qd(0,0,0,0)),		!ln Xa
	(klog,			$,	qd(0,0,0,0)),		!log Xa
	(klg,			$,	qd(0,0,0,0)),		!lg Xa
	(kexp,			$,	qd(0,0,0,0)),		!exp Xa
	(kround,		$,	qd(0,0,0,0)),		!round Xa
	(kfloor,		$,	qd(0,0,0,0)),		!floor Xa
	(kceil,			$,	qd(0,0,0,0)),		!ceil Xa
	(kfract,		$,	qd(0,0,0,0)),		!fract Xa
	(kfmod,			$,	qd(0,0,0,0)),		!fmod(Xb, Ya)

	(knegto,		$,	qd(0,0,0,0)),		!-:=Xa^; -:=A
	(kabsto,		$,	qd(0,0,0,0)),		!abs:=^Xa; pop Xa
	(knotto,		$,	qd(0,0,0,0)),		!not:=Xa^; pop Xa
	(kinotto,		$,	qd(0,0,0,0)),		!inot:=Xa^; pop Xa
	(knotlto,		$,	qd(0,0,0,0)),		!not:=Xa^; pop Xa

	(klen,			$,	qd(0,0,0,0)),		!T:=Xa.len
	(klwb,			$,	qd(0,0,0,0)),		!Xa.lwb
	(kupb,			$,	qd(0,0,0,0)),		!Xa.upb
	(kbounds,		$,	qd(0,0,0,0)),		!Xa.bounds (as one range value)
	(kboundsx,		$,	qd(0,0,0,0)),		!Xa.bounds (as two ints)
	(kbitwidth,		$,	qd(0,0,0,0)),		!Xa.bitwidth
	(kbytesize,		$,	qd(0,0,0,0)),		!Xa.bytesize
	(ktype,			$,	qd(0,0,0,0)),		!Xa.type
	(kelemtype,		$,	qd(0,0,0,0)),		!Xa.elemtype
	(kbasetype,		$,	qd(0,0,0,0)),		!Xa.basetype
	(kdictitems,	$,	qd(0,0,0,0)),		!Xa.basetype
	(kminvalue,		$,	qd(0,0,0,0)),		!Xa.minvalue
	(kmaxvalue,		$,	qd(0,0,0,0)),		!Xa.maxvalue
	(kisint,		$,	qd(0,0,0,0)),		!Xa.isint
	(kisreal,		$,	qd(0,0,0,0)),		!Xa.isreal
	(kisstring,		$,	qd(0,0,0,0)),		!Xa.isstring
	(kisrange,		$,	qd(0,0,0,0)),		!Xa.isrange
	(kisnumber,		$,	qd(0,0,0,0)),		!Xa.isnumber
	(kislist,		$,	qd(0,0,0,0)),		!Xa.isarray
	(kisrecord,		$,	qd(0,0,0,0)),		!Xa.isrecord
	(kispointer,	$,	qd(0,0,0,0)),		!Xa.ispointer
	(kisarray,		$,	qd(0,0,0,0)),		!Xa.isarray
	(kismutable,	$,	qd(0,0,0,0)),		!Xa.ismutable
	(kisset,		$,	qd(0,0,0,0)),		!Xa.isset
	(kisvoid,		$,	qd(0,0,0,0)),		!Xa.isvoid
	(kisdef,		$,	qd(0,0,0,0)),		!Xa.isdef
	(ktostr,		$,	qd(0,0,0,0)),		!Xa.isnoneComment (may be suppressed from pcb file)
	(kisequal,		$,	qd(0,0,0,0)),		!Xb==Ya
	(kconvert,		$,	qd(t,0,0,0)),		!Xa==A(Xa)
	(ktypepun,		$,	qd(t,0,0,0)),		!Xa==A@(Xa)

	(kadd,			$,	qd(0,0,0,0)),		!T:=Xb+Ya
	(ksub,			$,	qd(0,0,0,0)),		!Xb-Ya
	(kmul,			$,	qd(0,0,0,0)),		!Xb*Ya
	(kdiv,			$,	qd(0,0,0,0)),		!Xb/Ya
	(kidiv,			$,	qd(0,0,0,0)),		!Xb%Ya
	(kirem,			$,	qd(0,0,0,0)),		!Xb rem Ya
	(kidivrem,		$,	qd(0,0,0,0)),		!Xb divrem Ya
	(kiand,			$,	qd(0,0,0,0)),		!Xb iand Ya
	(kior,			$,	qd(0,0,0,0)),		!Xb ior Ya
	(kixor,			$,	qd(0,0,0,0)),		!Xb ixor Ya
	(kshl,			$,	qd(0,0,0,0)),		!Xb shl Ya
	(kshr,			$,	qd(0,0,0,0)),		!Xb shr Ya
	(kin,			$,	qd(0,0,0,0)),		!Xb in Ya
	(knotin,		$,	qd(0,0,0,0)),		!Xb notin Ya
	(kinrev,		$,	qd(0,0,0,0)),		!Xb inrev Ya
	(kandl,			$,	qd(0,0,0,0)),		!Xb and Ya
	(korl,			$,	qd(0,0,0,0)),		!Xb or Ya
	(kxorl,			$,	qd(0,0,0,0)),		!Xb xor Ya
	(keq,			$,	qd(0,0,0,0)),		!Xb=Ya
	(kne,			$,	qd(0,0,0,0)),		!Xb<>Ya
	(klt,			$,	qd(0,0,0,0)),		!Xb<Ya
	(kle,			$,	qd(0,0,0,0)),		!Xb<=Ya
	(kge,			$,	qd(0,0,0,0)),		!Xb>=Ya
	(kgt,			$,	qd(0,0,0,0)),		!Xb>Ya
	(kmin,			$,	qd(0,0,0,0)),		!Xb min Ya
	(kmax,			$,	qd(0,0,0,0)),		!Xb max Ya
	(kconcat,		$,	qd(0,0,0,0)),		!Xb concat Ya
	(kappend,		$,	qd(0,0,0,0)),		!Xb append Ya
	(kprepend,		$,	qd(0,0,0,0)),		!Xb prepend Ya

	(kpower,		$,	qd(0,0,0,0)),		!Xb power Ya
	(katan2,		$,	qd(0,0,0,0)),		!Xb atan2 Ya

	(kaddto,		$,	qd(0,0,0,0)),		!Xb^+:=Y or Xa^+:=A or A+:=B
	(ksubto,		$,	qd(0,0,0,0)),		!Xb^-:=Ya
	(kmulto,		$,	qd(0,0,0,0)),		!Xb^*:=Ya
	(kdivto,		$,	qd(0,0,0,0)),		!Xb^/:=Ya
	(kidivto,		$,	qd(0,0,0,0)),		!Xb^%:=Ya

	(kandlto,		$,	qd(0,0,0,0)),		!Xb^ and:=Ya
	(korlto,		$,	qd(0,0,0,0)),		!Xb^ or:=Ya
	(kiandto,		$,	qd(0,0,0,0)),		!Xb^ iand:=Ya
	(kiorto,		$,	qd(0,0,0,0)),		!Xb^ ior:=Ya
	(kixorto,		$,	qd(0,0,0,0)),		!Xb^ ixor:=Ya
	(kshlto,		$,	qd(0,0,0,0)),		!Xb^ shl:=Ya
	(kshrto,		$,	qd(0,0,0,0)),		!Xb^ shr:=Ya
	(kminto,		$,	qd(0,0,0,0)),		!Xb^ min:=Ya
	(kmaxto,		$,	qd(0,0,0,0)),		!Xb^ max:=Ya
	(kconcatto,		$,	qd(0,0,0,0)),		!Xb^ concat:=Ya
	(kappendto,		$,	qd(0,0,0,0)),		!Xb^ concat:=Ya

!	(kdotname,		$,	qd(d,0,0,0)),		!T:=Xa.A
	(kdot,			$,	qd(g,0,0,0)),		!T:=Xa.A
	(kindex,		$,	qd(0,0,0,0)),		!T:=Xb[Ya]
!	(kslice,		$,	qd(0,0,0,0)),		!T:=Xb[Ya]
	(kdotindex,		$,	qd(0,0,0,0)),		!T:=Xb.[Ya]
!	(kdotslice,		$,	qd(0,0,0,0)),		!T:=Xc.[Yb..Za]
	(kkeyindex,		$,	qd(0,0,0,0)),		!T:=Xc{Yb,Za}

	(kdotref,		$,	qd(g,0,0,0)),		!T:=&Xa.A
	(kindexref,		$,	qd(0,0,0,0)),		!T:=&Xb[Ya]
!	(ksliceref,		$,	qd(0,0,0,0)),		!T:=&Xc[Yb..Za]
	(kdotindexref,	$,	qd(0,0,0,0)),		!T:=&Xb.[Ya]
!	(kdotsliceref,	$,	qd(0,0,0,0)),		!T:=&Xc.[Yb..Za]
	(kkeyindexref,	$,	qd(0,0,0,0)),		!T:=&Xc{Ya,Za}

!	(kpopdotname,	$,	qd(d,0,0,0)),		!Ya.A:=Xb
	(kpopdot,		$,	qd(g,0,0,0)),		!Ya.A:=Xb
	(kpopindex,		$,	qd(0,0,0,0)),		!Yb[Za]:=Xc
!	(kpopslice,		$,	qd(0,0,0,0)),		!Xc[Yb..Za]:=Wd
	(kpopdotindex,	$,	qd(0,0,0,0)),		!Yb.[Za]:=Xc
!	(kpopdotslice,	$,	qd(0,0,0,0)),		!Xc.[Yb..Za]:=Wd
	(kpopkeyindex,	$,	qd(0,0,0,0)),		!Yb{Za}:=Xc

!	(kappendset,	$,	qd(0,0,0,0)),		!Xb[Ya]:=1; pop Y, keep X on stack; Xa[A]:=1
!	(kexpandrange,	$,	qd(0,0,0,0)),		!(Xb,Ya):=(Xa.lwb,Xa.upb)
	(kexpand,		$,	qd(i,0,0,0)),		!Expand Xa when A objects are needed

!	(kpushad,		$,	qd(x,0,0,0)),		!push index of dll proc
	(kpushtry,		$,	qd(l,i,i,0)),		!Push try/except into; label/except code/no. exceptions
	(kraise,		$,	qd(0,0,0,0)),		!Raise exception Xa
	(kapplyop,		$,	qd(i,0,0,0)),		!applyop(Ya,Xb); i is 1 or 2, number of operands provided

	(klastpcl,		$,	qd(0,0,0,0))
end

global [0..klastpcl]ref void cmdmap			!map cmd index to possible fn/label address

global tabledata() []ichar symbolnames=
!First half are basic tokens returned by lexreadtoken()
	(errorsym,			$),		! Lex error
	(dotsym,			$),		! "."
	(lexdotsym,			$),		! ".", used at bol to prefix lexical 
	(anddotsym,			$),		! "&."
	(commasym,			$),		! ","
	(semisym,			$),		! ";"
	(colonsym,			$),		! ":"
	(dcolonsym,			$),		! "::"
	(assignsym,			$),		! :=
	(deepcopysym,		$),		! ::=
	(sendtosym,			$),		! =>
	(lbracksym,			$),		! (
	(rbracksym,			$),		! )
	(lsqsym,			$),		! [
	(rsqsym,			$),		! ]
	(lcurlysym,			$),		! {
	(rcurlysym,			$),		! }
	(ptrsym,			$),		! ^
	(barsym,			$),		! |
	(dbarsym,			$),		! ||
	(atsym,				$),		! @
	(datsym,			$),		! @@
	(questionsym,		$),		! ?
	(addrsym,			$),		! &
	(daddrsym,			$),		! &&
	(poundsym,			$),		! Œ Hmm, should be Pound A+156
	(curlsym,			$),		! ~
	(gatesym,			$),		! ª
	(rangesym,			$),		! ..
	(ellipsissym,		$),		! ...
	(hashsym,			$),		! #

	(addsym,			$),		! +
	(subsym,			$),		! -
	(mulsym,			$),		! *
	(divsym,			$),		! /
	(idivsym,			$),		! %
	(iremsym,			$),		! rem
	(idivremsym,		$),		! divrem
	(andlsym,			$),		! and
	(orlsym,			$),		! or
	(iandsym,			$),		! iand
	(iorsym,			$),		! ior
	(ixorsym,			$),		! xor
	(shlsym,			$),		! <<
	(shrsym,			$),		! >>

	(minsym,			$),		! min
	(maxsym,			$),		! max
	(prependsym,		$),		! prepend
	(appendsym,			$),		! append
	(concatsym,			$),		! concat
	(insym,				$),		! in
	(notinsym,			$),		! notin
	(inrevsym,			$),		! inrevsym
	(powersym,			$),		! **

	(eqsym,				$),		! =
	(nesym,				$),		! <>
	(ltsym,				$),		! <
	(lesym,				$),		! <=
	(gesym,				$),		! >=
	(gtsym,				$),		! >
	(isequalsym,		$),		! ==

	(notlsym,			$),		! not
	(inotsym,			$),		! inot
	(istruelsym,		$),		! istrue
	(abssym,			$),		! abs
	(sqrsym,			$),		! sqr
	(signsym,			$),		! sign
	(ascsym,			$),		! asc
	(chrsym,			$),		! chr

	(mathssym,			$),		! sin etc
	(maths2sym,			$),		! atan2 etc
	(propsym,			$),		! len etc

	(incrsym,			$),		! -
	(decrsym,			$),		! -

	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen
	(rawnamesym,		$),		! unassigned name before lookup
	(docstringsym,		$),		! ! #comment used as documentation string
	(intconstsym,		$),		! 123 32 bits signed
	(decimalconstsym,	$),		! 123 or 123.4 decimal
	(realconstsym,		$),		! 123.4 64 bits
	(charconstsym,		$),		! 'A' or 'ABCD'
	(wcharconstsym,		$),		! 'A'W or 'ABCD'W (but don't have a syntax yet)
	(stringconstsym,	$),		! "ABC"
	(astringconstsym,	$),		! A"ABC"
	(wstringconstsym,	$),		! "ABC"W

!Second half are tokens that can be yielded after a name lookup::
	(unitnamesym,		$),		! 
	(namesym,			$),		! identifier symbol
	(kstrincludesym,	$),		! 

	(stdtypesym,		$),		! INT, CHAR etc
	(packtypesym,		$),		! I64 etc
!	(packtypesym,		$),		! Byte etc
	(koutsym,			$),		! OUT
	(kicharsym,			$),		! ICHAR
	(kifsym,			$),		! 
	(kthensym,			$),		! 
	(kelsifsym,			$),		! 
	(kelsesym,			$),		! 
	(kelsecasesym,		$),		! 
	(kelseswitchsym,	$),		! 
	(kelseselectsym,	$),		! 
	(kendsym,			$),		! 
	(kunlesssym,		$),		! 
	(kcasesym,			$),		! CASE
	(kdocasesym,		$),		! DOCASE
	(krecasesym,		$),		! RECASE
	(kwhensym,			$),		! 
	(kforsym,			$),		! 
	(kforallsym,		$),		! FORALL
	(ktosym,			$),		! TO/DOWNTO
	(kbysym,			$),		! 
	(kdosym,			$),		! 
	(kdooncesym,		$),		! 
	(kwhilesym,			$),		! 
	(krepeatsym,		$),		! 
	(kuntilsym,			$),		! 
	(kreturnsym,		$),		! 
	(kstopsym,			$),		! 
	(kloopsym,			$),		! EXIT/NEXT/LOOP/REDO/RESTART
	(kgotosym,			$),		! GO/GOTO
	(kswitchsym,		$),		! SWITCH
	(kdoswitchsym,		$),		! DOSWITCH
	(kprintsym,			$),		! PRINT/PRINTLN/FPRINT/FPRINTLN
	(ksprintsym,		$),		! SPRINT/SFPRINT
	(kreadsym,			$),		! READ/READLN
	(ksreadsym,			$),		! SREAD
	(ksreadlnsym,		$),		! SREADLN
	(knewsym,			$),		! NEW
	(kprocsym,			$),		! PROC
	(kfunctionsym,		$),		! FUNCTION
!	(kmethodsym,		$),		! METHOD
	(klabelsym,			$),		! LABEL
	(krecordsym,		$),		! RECORD
	(kstructsym,		$),		! STRUCT
	(kunionsym,			$),		! UNION
	(kimportsym,		$),		! IMPORT
	(kimportdllsym,		$),		! IMPORTDLL
	(kmodulesym,		$),		! 
	(ktypesym,			$),		! TYPE
	(ktypeattrsym,		$),		! COMPACT/DERIVED
	(krefsym,			$),		! REF
	(kvarsym,			$),		! VAR
	(kletsym,			$),		! LET
	(kvariantsym,		$),		! VARIANT
	(kslicesym,			$),		! SLICE/DSLICE
!	(krangesym,			$),		! RANGE
!	(ksetsym,			$),		! SET
	(kmacrosym,			$),		! MACRO
	(kexpandsym,		$),		! EXPAND
	(koperatorsym,		$),		! OPERATOR
	(kconstsym,			$),		! 
	(kenumsym,			$),		! 
	(kclasssym,			$),		! CLASS
	(kdirectivesym,		$),		! TARGET/MODULE
	(kfflangsym,		$),		! JLANG CLANG WINDOWS HOST
	(kglobalsym,		$),		! global
	(kstaticsym,		$),		! STATIC
	(kcalignedsym,		$),		! $CALIGNED

	(ktrysym,			$),		! 
	(kexceptsym,		$),		! 
	(kfinallysym,		$),		! 
	(kraisesym,			$),		! 
	(kyieldsym,			$),		! 
	(kextendsym,		$),		!
	(kblocksym,			$),		!
	(kcastsym,			$),		! CAST
	(kptypesym,			$),		!
!	(ktypeconstsym,		$),		! TYPECONST
	(compilervarsym,	$),		! $lineno etc
	(dollarsym,			$),		! to be used for current array upperbound; also tabledata names
	(kevalsym,			$),		! EVAL
	(ktabledatasym,		$),		! tabledata
	(kmapsym,			$),		! MAP/MAPL/MAPR/MAPLR
	(kapplyopsym,		$),		! APPLYOP
	(kclampsym,			$),		! CLAMP
	(kswapsym,			$),		! SWAP
	(kcondcompsym,		$),		! $WHEN
	(kerrorsym,			$),		! PC_ERROR etc
	(sysconstsym,		$),		! nil, etc
	(khostfnsym,		$),		! LEFT, CONVLC etc
	(khostsym,			$),		! HOST
	(knilsym,			$),		! NIL/PNIL
	(kdummysym,			$)		!
end

global tabledata() =
!	(nil_const),
!	(pnil_const),
	(pi_const),
	(tab_const),
	(con_const),
	(true_const),
	(false_const),
end

global tabledata() =
	(thousand_unit),
	(million_unit),
	(billion_unit)
end

global tabledata() [0:]ichar parammodenames=
	(var_param=0,		"Var "),
	(in_param,			"In "),
	(out_param,			"Out "),
	(optional_param,	"Opt "),
end

global tabledata() [0:]ichar namenames =
	(genericid=0,	$),		! - 		Generic name, not yet resolved
	(programid,		$),		!
	(moduleid,		$),		!
	(dllmoduleid,	$),		!
	(procid,		$),		!Proc/method/function/op name
	(dllprocid,		$),		!
	(recordid,		$),		!
	(typeid,		$),		!
	(fieldid,		$),		!
	(structfieldid,	$),		!
	(staticid,		$),		!Static var in module/proc/record
	(frameid,		$),		!Local var in proc
	(paramid,		$),		!param in proc
	(dllparamid,	$),		!dll param in dllproc
	(labelid,		$),		!Label name in proc only
	(constid,		$),		!Label name in proc only
	(enumid,		$),		!Label name in proc only
	(aliasid,		$),		!
	(macroid,		$),		!
	(macroparamid,	$),		!
	(structblockid,	$),		! pseudo names used
	(unionblockid,	$),		!
	(endblockid,	$),		!
end

global tabledata() [0:]ichar objtypenames =
	(normal_obj=0,	$),
	(slice_obj,		$),
	(extslice_obj,	$)
end

global tabledata []ichar stnames, []int16 stsymbols, []int16 stsubcodes=

	("if",			kifsym,			0),
	("then",		kthensym,		0),
	("elsif",		kelsifsym,		0),
	("else",		kelsesym,		0),
	("elsecase",	kelsecasesym,	0),
	("elseswitch",	kelseswitchsym,	0),
	("case",		kcasesym,		jcase),
	("docase",		kdocasesym,		jdocase),
	("recase",		krecasesym,		jrecase),
	("when",		kwhensym,		0),
	("for",			kforsym,		0),
	("forall",		kforsym,		0),
!	("forall",		kforallsym,		jforall),
	("foreach",		kforsym,		1),
	("to",			ktosym,			0),
	("downto",		ktosym,			1),
	("by",			kbysym,			0),
	("do",			kdosym,			0),
	("doonce",		kdooncesym,		0),
	("end",			kendsym,		0),
	("while",		kwhilesym,		0),
	("repeat",		krepeatsym,		0),
	("until",		kuntilsym,		0),
	("always",		kuntilsym,		0),
	("return",		kreturnsym,		0),
	("yield",		kyieldsym,		0),
	("stop",		kstopsym,		0),
!	("restart",		kloopsym,		jrestart),
	("redo",		kloopsym,		jredo),
	("next",		kloopsym,		jnext),
	("exit",		kloopsym,		jexit),
	("goto",		kgotosym,		0),
	("go",			kgotosym,		1),
	("switch",		kswitchsym,		jswitch),
	("doswitch",	kdoswitchsym,	jdoswitch),
	("tabledata",	ktabledatasym,	0),
	("clamp",		kclampsym,		0),
	("applyop",		kapplyopsym,	0),
	("eval",		kevalsym,		0),
	("$windows",	kcondcompsym,	0),
	("$linux",		kcondcompsym,	0),

	("print",		kprintsym,		jprint),
	("println",		kprintsym,		jprintln),
	("fprint",		kprintsym,		jfprint),
	("fprintln",	kprintsym,		jfprintln),
	("sprint",		ksprintsym,		jsprint),
	("sfprint",		ksprintsym,		jsfprint),

	("cp",			kprintsym,		jprint),
	("cpl",			kprintsym,		jprintln),

	("read",		kreadsym,		jread),
	("readln",		kreadsym,		jreadln),

!	("new",			knewsym,		0),

	("cast",		kcastsym,		13),
	("$type",		kptypesym,		0),

	("proc",		kprocsym,		0),
	("function",	kfunctionsym,	0),
	("method",		kfunctionsym,	0),

! ("operator",	koperatorsym,		0),
	("type",		ktypesym,		0),
	("class",		kclasssym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("var",			kvarsym,		0),
	("let",			kletsym,		0),

	("strinclude",	kstrincludesym,	0),
	("macro",		kmacrosym,		0),
	("operator",	koperatorsym,	0),

	("static",		kstaticsym,		0),
	("$caligned",	kcalignedsym,	0),
	
	("const",		kconstsym,		0),
	("enum",		kenumsym,		0),

	("import",		kimportsym,		0),
	("importdll",	kimportdllsym,	0),
	("unless",		kunlesssym,		0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("finally",		kfinallysym,	0),
	("raise",		kraisesym,		0),
	("out",			koutsym,		0),

	("global",		kglobalsym,		0),
	("host",		khostsym,		0),

	("clang",		kfflangsym,		clangff),
!	("qlang",		kfflangsym,		qlangff),
	("windows",		kfflangsym,		windowsff),
	("callback",	kfflangsym,		callbackff),

	("swap",		kswapsym,		0),

	("void",		stdtypesym,		tvoid),

	("int",			stdtypesym,		tint),
!	("dint",		stdtypesym,		ti128),

	("word",		stdtypesym,		tword),

!	("dword",		stdtypesym,		tu128),
	("real",		stdtypesym,		treal),

	("string",		stdtypesym,		tstring),
	("list",		stdtypesym,		tlist),
	("array",		stdtypesym,		tarray),
	("bits",		stdtypesym,		tbits),
	("set",			stdtypesym,		tset),
	("dict",		stdtypesym,		tdict),
	("decimal",		stdtypesym,		tdecimal),
	("longint",		stdtypesym,		tdecimal),
	("typetype",	stdtypesym,		ttype),
	("range",		stdtypesym,		trange),
	("recordtype",	stdtypesym,		trecord),

	("cvoid",		packtypesym,	tpvoid),
	("i8",			packtypesym,	tpi8),
	("i16",			packtypesym,	tpi16),
	("i32",			packtypesym,	tpi32),
	("i64",			packtypesym,	tpi64),
	("i128",		packtypesym,	tpi128),

	("bit",			packtypesym,	tpu1),
	("u1",			packtypesym,	tpu1),
	("u2",			packtypesym,	tpu2),
	("u4",			packtypesym,	tpu4),
	("byte",		packtypesym,	tpu8),
	("u8",			packtypesym,	tpu8),
	("u16",			packtypesym,	tpu16),
	("u32",			packtypesym,	tpu32),
	("u64",			packtypesym,	tpu64),
	("u128",		packtypesym,	tpu128),

	("r32",			packtypesym,	tpr32),
	("r64",			packtypesym,	tpr64),

	("int8",		packtypesym,	tpi8),
	("int16",		packtypesym,	tpi16),
	("int32",		packtypesym,	tpi32),
	("int64",		packtypesym,	tpi64),
	("int128",		packtypesym,	tpi128),

	("word8",		packtypesym,	tpu8),
	("word16",		packtypesym,	tpu16),
	("word32",		packtypesym,	tpu32),
	("word64",		packtypesym,	tpu64),
	("word128",		packtypesym,	tpu128),

	("real32",		packtypesym,	tpr32),
	("real64",		packtypesym,	tpr64),

	("stringc",		packtypesym,	tpstringc),
	("stringz",		packtypesym,	tpstringz),
	("cstring",		packtypesym,	tpcstring),
!	("variadic",	packtypesym,	tp_variadic),
!	("refvoid",		packtypesym,	tp_ptr),

!	("char",		stdtypesym,		tc8),
!	("wchar",		stdtypesym,		tc16),
!	("char64",		stdtypesym,		tc64),

	("million",		unitnamesym,	million_unit),
	("billion",		unitnamesym,	billion_unit),
!	("thousand",	unitnamesym,	thousand_unit),
	("as",			unitnamesym,	0),

	("$lineno",		compilervarsym,	1),		!TEMP
!	("$strlineno",	compilervarsym,	j_cvstrlineno),
!	("$filename",	compilervarsym,	j_cvfilename),
!	("$modulename",	compilervarsym,	j_cvmodulename),
!	("$function",	compilervarsym,	j_cvfunction),
!	("$date",		compilervarsym,	j_cvdate),
!	("$time",		compilervarsym,	j_cvtime),
!	("$version",	compilervarsym,	J_cvversion),
!	("$typename",	compilervarsym,	j_cvtypename),
!	("$targetbits",	compilervarsym,	j_cvtargetbits),
!	("$targetsize",	compilervarsym,	j_cvtargetsize),
!!	("$targetname",	compilervarsym,	j_cvtargetname),
!	("$targetcode",	compilervarsym,	j_cvtargetcode),
	("$",			dollarsym,		0),

	("and",			andlsym,		jandl),
	("or",			orlsym,			jorl),
!	("xor",			opsym,			jxorl),
	("iand",		iandsym,		jiand),
	("ior",			iorsym,			jior),
	("ixor",		ixorsym,		jixor),
	("in",			insym,			jin),
	("notin",		notinsym,		jnotin),
	("inrev",		inrevsym,		jinrev),
	("rem",			iremsym,		jirem),
	("divrem",		idivremsym,		jidivrem),
	("min",			minsym,			jmin),
	("max",			maxsym,			jmax),

	("not",			notlsym,		jnotl),
	("inot",		inotsym,		jinot),
	("istrue",		istruelsym,		jistruel),
	("abs",			abssym,			jabs),
!	("$neg",		opsym,			jneg),
	("asc",			ascsym,			jasc),
	("chr",			chrsym,			jchr),
	("sqrt",		mathssym,		jsqrt),
	("sqr",			sqrsym,			jsqr),
	("cos",			mathssym,		jcos),
	("sin",			mathssym,		jsin),
	("tan",			mathssym,		jtan),
	("asin",		mathssym,		jasin),
	("acos",		mathssym,		jacos),
	("atan",		mathssym,		jatan),
	("atan2",		maths2sym,		jatan2),
	("sign",		signsym,		jsign),
	("ln",			mathssym,		jln),
	("log",			mathssym,		jlog),
	("exp",			mathssym,		jexp),
	("round",		mathssym,		jround),
	("floor",		mathssym,		jfloor),
	("ceil",		mathssym,		jceil),
	("fract",		mathssym,		jfract),
	("fmod",		maths2sym,		jfmod),

!	("head",		opsym,			khead),
!	("tail",		opsym,			ktail),
!	("init",		opsym,			kinit),
!	("last",		opsym,			klast),
!	("take",		opsym,			ktake),
!	("drop",		opsym,			kdrop),
!	("left",		opsym,			kleft),
!	("right",		opsym,			kright),
!	("insert",		opsym,			kinsert),
!	("delete",		opsym,			kdelete),
!	("ireverse",	opsym,			kireverse),
!	("reverse",		opsym,			kreverse),
!	("dupl",		opsym,			kdupl),
!	("zip",			opsym,			kzip),
	("prepend",		prependsym,		jprepend),
	("append",		appendsym,		jappend),
	("concat",		concatsym,		jconcat),
!	("convlc",		opsym,			kconvlc),
!	("convuc",		opsym,			kconvuc),

	("len",			propsym,		jlen),
	("lwb",			propsym,		jlwb),
	("upb",			propsym,		jupb),
	("bounds",		propsym,		jbounds),
	("bitwidth",	propsym,		jbitwidth),
	("bytes",		propsym,		jbytesize),
	("minvalue",	propsym,		jminvalue),
	("maxvalue",	propsym,		jmaxvalue),
	("basetype",	propsym,		jbasetype),
	("elemtype",	propsym,		jelemtype),
	("dictitems",	propsym,		jdictitems),
!	("type",		propsym,		ktype),

	("isvoid",		propsym,		jisvoid),
	("isdef",		propsym,		jisdef),
	("defined",		propsym,		jisdef),
	("isint",		propsym,		jisint),
	("isreal",		propsym,		jisreal),
	("islist",		propsym,		jislist),
	("isstring",	propsym,		jisstring),
	("isrange",		propsym,		jisrange),
	("ispointer",	propsym,		jispointer),
	("isarray",		propsym,		jisarray),
	("isrecord",	propsym,		jisrecord),
	("isset",		propsym,		jisset),

	("endif",		kendsym,		kifsym),
	("fi",			kendsym,		kifsym),
	("endcase",		kendsym,		kcasesym),
	("esac",		kendsym,		kcasesym),
	("enddocase",	kendsym,		kdocasesym),
	("endswitch",	kendsym,		kswitchsym),
	("enddoswitch",	kendsym,		kdoswitchsym),
	("endfor",		kendsym,		kforsym),
	("endforall",	kendsym,		kforallsym),
	("od",			kendsym,		kdosym),
	("enddoonce",	kendsym,		kdooncesym),
	("endproc",		kendsym,		kprocsym),
	("endfunction",	kendsym,		kfunctionsym),
	("endwhile",	kendsym,		kwhilesym),
	("endto",		kendsym,		ktosym),
	("enddo",		kendsym,		kdosym),
	("endunless",	kendsym,		kunlesssym),
!	("endmodule",	kendsym,		kmodulesym),
	("endmoduledll",	kendsym,	kimportdllsym),
	("endtry",		kendsym,		ktrysym),
	("endrecord",	kendsym,		krecordsym),
	("endclass",	kendsym,		kclasssym),
	("endblock",	kendsym,		kblocksym),

	("nil",			knilsym,		0),
	("pnil",		knilsym,		1),
	("con",			sysconstsym,	con_const),
!	("tab",			sysconstsym,	tab_const),
	("pi",			sysconstsym,	pi_const),
	("true",		sysconstsym,	true_const),
	("false",		sysconstsym,	false_const),

	("$$dummy",		0,				0)
end

global tabledata() [0:]ichar hostfnnames, [0:]byte hostnparams, [0:]byte hostisfn,
			[0:]byte hostinternal =
	(host_dummy=0,			$,	0,	0,	1),

	(host_startprint,		$,	1,	0,	1),	!startprint(x)	Set o/p dev for following print items
	(host_startprintcon,	$,	0,	0,	1),	!startprintcon()	Set console dev for following print items
	(host_strstartprint,	$,	0,	0,	1),	!strstartprint()	Set o/p dev for internal string
	(host_setformat,		$,	1,	0,	1),	!setformat(x)	Set up format string for following print items up to str/endprint
	(host_endprint,			$,	0,	0,	1),	!endprint()	Restore o/p dev
	(host_strendprint,		$,	0,	1,	1),	!strendprint()	Restore o/p dev, and return result as string
	(host_print,			$,	2,	0,	1),		!print(x,[y])	Print x, using default format code or y
	(host_print_nf,			$,	1,	0,	1),		!print(x)		Print x, using default format code

	(host_dprint,			$,	2,	0,	1),	!dprint(x,[y])	As print, but with extra debug stuff
	(host_println,			$,	0,	0,	1),	!println()	Print newline
	(host_printnogap,		$,	0,	0,	1),	!printnogap()	Suppress any gap before next print item
	(host_printspace,		$,	0,	0,	1),	!printspace		Extra space at beg or end

	(host_readln,			$,	1,	0,	1),	!sreadln(x)	Read line from console or device x, into read buffer
	(host_sreadln,			$,	1,	1,	1),	!sreadln(x)	Read line from console or device x, into read buffer
	(host_sread,			$,	1,	1,	1),	!sread([x])	Read item from read buffer, with/without format code
	(host_rereadln,			$,	0,	0,	1),	!sread([x])	Read item from read buffer, with/without format code
	(host_reread,			$,	0,	0,	1),	!sread([x])	Read item from read buffer, with/without format code

	(host_strtoval,			$,	2,	1,	0),	!
	(host_tostr,			$,	2,	1,	0),	!

	(host_leftstr,			$,	3,	1,	0),
	(host_rightstr,			$,	3,	1,	0),
	(host_convlc,			$,	2,	1,	0),
	(host_convuc,			$,	2,	1,	0),
	(host_iconvlc,			$,	2,	0,	0),		!&
	(host_iconvuc,			$,	2,	0,	0),		!&

	(host_stop,				$,	0,	0,	1),	!stop(x)	Stop execution
	(host_stopx,			$,	1,	0,	1),	!stopx(x)	Stop, with given return value
	(host_ismain,			$,	1,	1,	0),	!ismain(x)	Return 1 when module name x is main module
	(host_waitkey,			$,	0,	1,	0),
	(host_testkey,			$,	0,	1,	0),
	(host_execwait,			$,	3,	1,	0),
	(host_execcmd,			$,	3,	1,	0),
	(host_shellexec,		$,	2,	1,	0),
	(host_system,			$,	1,	1,	0),

	(host_makestr,			$,	2,	1,	0),
	(host_makestrslice,		$,	2,	1,	0),
	(host_makeref,			$,	2,	1,	0),

	(host_new,				$,	4,	1,	0),
	(host_newheap,			$,	4,	1,	0),
	(host_readlines,		$,	1,	1,	0),
	(host_heapvar,			$,	1,	1,	0),
!	(host_dictitems,		$,	1,	1,	0),
	(host_freeheap,			$,	1,	0,	0),
	(host_setoverload,		$,	3,	0,	0),

	(host_getcmdparam,		$,	1,	1,	0),
	(host_gethostname,		$,	0,	1,	0),

	(host_$setpcerror,		$,	1,	0,	0),
	(host_$setdebug,		$,	1,	0,	0),
	(host_$test,			$,	2,	1,	0),

	(host_ticks,			$,	0,	1,	0),
	(host_sleep,			$,	1,	0,	0),
	(host_random,			$,	1,	1,	0),
	(host_findmetafunction,	$,	1,	1,	0),
	(host_gethash,			$,	1,	1,	0),
	(host_getos,			$,	0,	1,	0),
	(host_gethostsize,		$,	0,	1,	0),
	(host_iswindows,		$,	0,	1,	0),
	(host_setmesshandler,	$,	1,	0,	0),
	(host_$setfprintf,		$,	2,	0,	0),

	(host_loadpcl,			$,	2,	1,	0),
	(host_runpcl,			$,	2,	1,	0),
	(host_runtask,			$,	2,	1,	0),
	(host_callext,			$,	3,	0,	0),
	(host_$pcldata,			$,	2,	1,	0),
	(host_getcstring,		$,	1,	1,	0),
	(host_$getparam,		$,	1,	1,	0),
	(host_clearlist,		$,	1,	0,	0),
	(host_makelink,			$,	1,	1,	0),
	(host_allparams,		$,	1,	1,	0),
	(host_stackvars,		$,	0,	1,	0),
	(host_makeempty,		$,	1,	1,	0),
	(host_$errorinfo,		$,	1,	1,	0),
	(host_strrepl,			$,	3,	1,	0),
	(host_$procsymbols,		$,	0,	1,	0),
	(host_$symbolowner,		$,	1,	1,	0),
	(host_$symbolname,		$,	1,	1,	0),
!	(host_symboladdr,		$,	1,	1,	0),
	(host_$symboldefs,		$,	1,	1,	0),
	(host_$testcallback,	$,	0,	0,	0),
	(host_$smallmemtotal,	$,	0,	1,	0),
	(host_$id,				$,	1,	1,	0),

	(host_last,				$,	0,	0,	1)
end

global tabledata() []ichar errornames =
	(pc_error,			$),
	(user_error,		$),
	(type_error,		$),
	(mixedtype_error,	$),
	(divide_error,		$),
	(stopmodule_error,	$),
	(bounds_error,		$)
end

global var []byte D_binopset = (
	andlsym, orlsym, eqsym, nesym, ltsym, lesym, gtsym, gesym, addsym,
	subsym, mulsym, divsym, idivsym, iremsym, iandsym, iorsym, ixorsym,
	shlsym, shrsym, minsym, maxsym,	concatsym, powersym, isequalsym,
	idivremsym,  maths2sym, appendsym, prependsym )

global var [0..symbolnames.upb]byte binopset

global var []byte D_unaryopset = (
	notlsym, inotsym, abssym, istruelsym, sqrsym, signsym, ascsym, chrsym,
	mathssym)

global var [0..symbolnames.upb]byte unaryopset

global var []byte D_addopset=(addsym, subsym, iandsym, iorsym, ixorsym,
		concatsym, appendsym, minsym, maxsym)

global var []byte D_cmpopset=(eqsym, nesym, ltsym, lesym, gesym, gtsym, isequalsym)

global var []byte D_mulopset=(mulsym, divsym, idivsym, iremsym, shlsym, shrsym)

global var [0..symbolnames.upb]byte addopset
global var [0..symbolnames.upb]byte cmpopset
global var [0..symbolnames.upb]byte mulopset
global var [0..symbolnames.upb]byte exprendset

global var []int D_exprstarterset= (lbracksym,lsqsym,ptrsym,addrsym,namesym,
	incrsym,decrsym,intconstsym,decimalconstsym,realconstsym,charconstsym,
	stringconstsym,stdtypesym,kptypesym,kapplyopsym,
	ksprintsym,ksreadsym,ksreadlnsym,knewsym,dollarsym,compilervarsym, kclampsym,
	kapplyopsym,kerrorsym,krefsym, kcastsym, anddotsym, packtypesym, ellipsissym,
	knilsym, khostfnsym,
	krecordsym, kstructsym)

global var [0:symbolnames.len]byte exprstarterset

!type tables
global const maxtype=250

global [0..maxtype]ichar ttname
global [0..maxtype]symbol ttnamedef
global [0..maxtype]int16 ttbasetype
global [0..maxtype]int16 tttarget

global [0..maxtype]int ttlower
global [0..maxtype]int ttlength

global [0..maxtype]unit ttlowerexpr
global [0..maxtype]unit ttlengthexpr

global [0..maxtype]int ttsize
global [0..maxtype]byte ttbitwidth
!global [0..maxtype]byte ttispacked
global [0..maxtype]symbol ttfields		!for initialially anonymous record field lists
global [0..maxtype]byte ttcaligned
global [0..maxtype]symbol ttowner
global int ntypes

global const int maxuserxtype=5000
global int nuserxtypes
global int userxtypebase			!first index (growing downwards) of userxtypes in current module
global ref userxrec userxmodelist	!list of all references to userx modes

global [0:maxuserxtype]symbol ttnamedefx
global [0:maxuserxtype]symbol ttnamedefx2
global [0:maxuserxtype]int ttposx
global [0:maxuserxtype]int ttxmap
global [0:maxuserxtype]byte ttxmoduleno

global [0..host_last]byte hostlvset

proc $init=
!	translate into an instant lookup format
	int i
!	for i:=1 to pclarith.len do
!		pcltoops[pclarith[i]]:=pclarithto[i]
!	od

	for i:=1 to D_binopset.len do
		binopset[D_binopset[i]]:=1
		exprstarterset[D_binopset[i]]:=1
	od

	for i:=1 to D_unaryopset.len do
		unaryopset[D_unaryopset[i]]:=1
		exprstarterset[D_unaryopset[i]]:=1
	od

	for i:=1 to D_exprstarterset.len do exprstarterset[D_exprstarterset[i]]:=1 od

	exprendset[semisym]:=1
	exprendset[commasym]:=1
	exprendset[rsqsym]:=1
	exprendset[rbracksym]:=1
	exprendset[kendsym]:=1
	exprendset[kdosym]:=1
	exprendset[ktosym]:=1

	for i:=1 to D_addopset.len do addopset[D_addopset[i]]:=1 od
	for i:=1 to D_mulopset.len do mulopset[D_mulopset[i]]:=1 od
	for i:=1 to D_cmpopset.len do cmpopset[D_cmpopset[i]]:=1 od

	for i in 0..tlast do
		ttname[i]:=stdtypenames[i]
		ttbasetype[i]:=i
		ttlower[i]:=1
		ttbitwidth[i]:=stdtypewidths[i]
		ttsize[i]:=stdtypewidths[i]/8
		if i in tpi8..tpref then
!			ttispacked[i]:=1
		fi

	od

	ntypes:=tlast

	hostlvset[host_iconvlc]:=1
	hostlvset[host_iconvuc]:=1

!set up twin types

!	for i to twintypesetup.upb do
!		twintypetable[twintypesetup[i,1],twintypesetup[i,2]]:=twintypesetup[i,3]
!	od
!
!	int tt
!	for i:=0 to tlastvartag do
!		for j:=0 to tlastvartag do
!			tt:=twintypetable[i,j]
!			if tt then
!				cpl "TT entry", stdtypenames[i], stdtypenames[j], twintypenames[tt]
!			fi
!		od
!	od

end
=== qq_pclgen.m 9/32 ===
import* qq

!not opcodes, just used internally here for conditional jump logic
const kjumpt = 1
const kjumpf = 0

const maxloopindex=20
var [maxloopindex,4]ref int loopstack
var [maxloopindex]int trylevelstack
var int loopindex=0
var int looptrylevel			!return by findlooplabel

const maxswitchrange=512
const maxlocals=300
const maxparams=100

const maxunits=400					!for constructors
var int trylevel=0
!var int currprocparams=0			!if inside proc, params used (needed by popretval)
var int currfunction=0				!0/1/2 = not a function/proc/function

!vars within curr procdef
int retindex						!common return point; label no
int retvaloffset					!offset of return value for procs (as stack slots)
int nprocparams						!no. of params
global int nproclocals				!no. of locals
global ref int pproclocals			!pointer to pcl operand of kprocentry; may need updating
const retaddrslots = 1				!+1 or +2, added to param indices (depends on return info size)

global proc evalunit(unit p,int res=1)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
!note: sometimes res can be 2, (passing on a res=2 from an outer stmt)
!that should be treated here same as 1 (res=2 has special meaning from pclhasvalue[] only)
	var unit a,b
	var symbol d
	var object ss
	int procflag,index

	qpos:=p.pos

	a:=p.a
	b:=p.b

!CPL "EVALUNIT",JTAGNAMES[P.TAG],=RES

	switch p.tag
	when jintconst      then
		genpc_int(kpushci,p.value)

	when jwordconst      then
		genpc_int(kpushcu,p.uvalue)

	when jrealconst     then
		genpc_real(kpushcr,p.xvalue)

	when jnone then

	when jstringconst   then
		genpc(kpushcs)
		genopnd_strz(p.svalue)

	when jname          then
		d:=p.def
		case d.nameid
		when frameid then
			genpc_name(kpushf,d)
		when paramid then
			genpc_name(kpushf,d)
			if d.mbyref then
				genpc(kpushptr)
			fi
		when staticid then
			genpc_name(kpushm,d)
		when procid,labelid then
			genpc_name(kpushsymbol,d)
		else
			cpl d.name
			gerror_s("Push name?",namenames[d.nameid])
		esac

	when jsymbol        then			!assume a is jname
		if a.tag=jname then
			genpc_name(kpushsymbol,a.def)
		else
			gerror(".$ name expected")
		fi

	when jhostname		then
		genpc_name(khostname,p.def)

	when jblock         then
		if a then
			while a and a.nextunit do
				evalunit(a,0)
				a:=a.nextunit
			od
			if a then
				evalunit(a,res)
			fi
		else
!			gerror("empty block")
		fi

	when jdecimal then
		genpc(kpushcs)
		genopnd_strz(p.svalue)
		genpc(kmakedecimal)

	when jcall          then do_call(p,a,b,res,procflag)
	when jreturn        then do_return(p,a)
	when jcallhost      then do_callhost(p,a,res)

	when jassign        then do_assign(a,b,res)
	when jdeepcopy      then do_assign(a,b,res,1)
	when jto            then do_to(p,a,b)
	when jif            then do_if(p,a,b,b.nextunit, res)
	when jforup,jfordown    then do_for(p,a,b)
	when jforupx,jfordownx  then do_forx(p,a,b)
	when jforall,jforallrev then do_forall(p,a,b)
	when jforeach       then do_forall(p,a,b)
!	when jforeach       then do_forall(p,a,b)
!!	when jforeachrev    then do_foreachrev(p,a,b)
!	when jcfor          then do_cfor(p,a,b)
	when jwhile         then do_while(p,a,b)
	when jrepeat        then do_repeat(p,a,b)
	when jgoto          then
		if a.tag=jname and a.def.nameid=labelid then
			d:=a.def
			if d.labelno=0 then
				d.labelno:=createfwdlabel()
			fi
!			genpc_name(kjumpname,a.def)
			genpc_lab(kjump,d.labelno)
		else
			evalunit(a)
			genpc(kjumpptr)
		fi

!	when jgotoblock     then do_gotoblock(p,a,b)
	when jlabeldef      then
		d:=a.def
		if d.labelno=0 then
			d.labelno:=definelabel()
		else
			index:=d.labelno
			definefwdlabel(index)
		fi

!	when jrestart       then do_exit(p,1)
	when jredo          then do_exit(p,1)
	when jnext          then do_exit(p,2)
	when jexit          then do_exit(p,3)
	when jdo,jdoonce    then do_do(p,a)
	when jcase,jdocase then do_case(p,a,b,res)
!	when jdocase        then do_case(p,a,b,p.c,1,0)
	when jswitch, jdoswitch then do_switch(p,a,b,res)
!	when jdoswitch      then do_switch(p,a,b,p.c,1,0)
	when jswap          then
		evalref(a)
		evalref(b)
		genpc(kswap)

	when jselect        then do_select(a,b,res)
	when jprint,jprintln,jsprint    then do_print(p,a,b)
	when jfprint,jfprintln,jsfprint then do_fprint(p,a,b,b.nextunit)
!	when jcprint        then do_cprint(p,a,b)
!	when jcprintln      then do_cprintln(p,a,b)
!	when jsprint        then do_sprint(p,a,b)
!	when jsfprint       then do_sfprint(p,a,b)
	when jread,jreadln  then do_read(p,a,b)
!	when jreadln        then do_readln(p,a,b)
!	when jsread         then do_sread(p,a,b)
	when jnew           then do_new(p)

!	when jsreadln       then do_sreadln(p,a,b)
	when jstop          then
		if a then
			evalunit(a)
		else
			genpc_int(kpushci,0)
		fi
		genpc(kstop)

	when jtry           then do_try(p,a,b)
!	when jexcept        then do_except(p,a,b)
!	when jyield         then do_yield(p,a,b)
!	when jraise         then do_raise(p,a,b)
!!	when jcallhostproc  then do_callhostproc(p,a,b)
!	when jeval          then do_eval(p,a,b)
!	when jlambda        then do_lambda(p,a,b)
	when jandl          then do_andl(a,b)
	when jorl          then do_orl(a,b)
!	when jxorl          then do_xorl(p,a,b)
!	when jnotl          then do_notl(p,a)
!	when jistruel       then do_istruel(p,a)
	when jmakelist then
		do_pushlist(a,p.length)
		genpc_int2(kmakelist,p.length,p.lower)

	when jmakeset then
		do_pushlist(a,p.length)
		genpc_int(kmakeset,p.length)

!	when jmakerecord then
!		do_pushlist(a,p.length)
!		genpc_int2(kmakerecord,p.length,0)
!
!	when jmakearray then
!		do_pushlist(a,p.length)
!		genpc_int4(kmakearray,p.length,1,0,0)
!
	when jmakedict then do_makedict(a,p.length)
!!		do_pushlist(a,p.length)
!!		genpc_int(kmakedict,p.length)

!	when jexprlist      then do_exprlist(p,a,b)
!	when jmultexpr      then do_multexpr(p,a,b)
!	when jkeyword       then do_bin(a,b,kkeyword)

	when jkeyvalue      then
		evalunit(a)
		evalunit(b)
!	when jassignx       then do_assign(p,a,b,1)
!	when jdeepcopyx     then do_assign(p,a,b,1)
!	when jcallmfn       then do_callproc(p,a,b,1)
!	when japplyop       then do_applyop(p,a,b)
!	when japplyopx      then do_applyopx(p,a,b)
!	when jandand        then do_andand(p,a,b)
!	when jeq            then do_setcc(p,a,b)
!	when jne            then do_setcc(p,a,b)
!	when jlt            then do_setccx(p,a,b)
!	when jle            then do_setccx(p,a,b)
!	when jge            then do_setccx(p,a,b)
!	when jgt            then do_setccx(p,a,b)
!	when jisequal       then do_isequal(p,a,b)
	when jadd, jsub, jmul, jdiv, jidiv, jidivrem, jiand, jior, jixor,
		 jshl, jshr, jin, jnotin, jmin, jmax, jmakerange, jmakerangelen,
		 jeq, jne, jlt, jle, jge, jgt, jirem, jpower,
		 jconcat, jappend,jisequal then
		evalunit(a)
		evalunit(b)
		genpc(jpclcodes[p.tag])

	when jaddto, jsubto, jmulto, jdivto, jidivto, jiandto, jiorto, jixorto,
		 jshlto, jshrto, jminto, jmaxto, jconcatto, jappendto then
		evalref(a)
		evalunit(b)
		genpc(jpclcodes[p.tag])

!	when jconcat        then do_bin(p,a,b,kconcat)
!	when jappend        then do_bin(p,a,b,kappend)
!	when jclamp         then do_clamp(p,a,b)

	when jneg, jabs, jlwb, jupb, jlen, jbounds, jnotl, jinot,jisarray,
		 jisint, jisreal, jbytesize, jisdef, jround, jisvoid, jtype,
		 jistruel, jsqr, jsqrt, jislist, jasc,jchr, jisstring, jisset,
		 jbasetype, jelemtype, jispointer, jisrange, jisrecord,
		 jfloor, jboundsx, jsin,jcos, jminvalue, jmaxvalue,
		 jdictitems then
		do_unary(a,jpclcodes[p.tag])

!	when jcmpchain then

	when jdot           then! do_bin(a,b,kdot)
		evalunit(a)
		genpc_name(kdot,b.def)

	when jindex         then do_bin(a,b,kindex)
!	when jslice         then do_bin(a,b,kslice)
	when jdotindex      then do_bin(a,b,kdotindex)
!	when jdotslice      then do_bin(a,b,kdotslice)
	when jkeyindex      then
		evalunit(a)
		evalunit(b)
		if b.nextunit then
			evalunit(b.nextunit)
		else
			genpc(kpushvoid)
		fi
		genpc(kkeyindex)

!	when jdotref        then do_binref(a,b,kdotref)
!	when jindexref      then do_binref(a,b,kindexref)
!	when jsliceref      then do_binref(a,b,ksliceref)
!	when jdotindexref   then do_binref(a,b,kdotindexref)
!	when jdotsliceref   then do_binref(a,b,kdotsliceref)


!	when jslice         then do_slice(p,a,b)
!	when jdotindex      then do_dotindex(p,a,b)
!	when jdotslice      then do_dotslice(p,a,b)
!	when janddotindex   then do_anddotindex(p,a,b)
!	when janddotslice   then do_anddotslice(p,a,b)
!	when jkeyindex      then do_keyindex(p,a,b)
!	when jdotattr       then do_dotattr(p,a,b)
!	when jatan2         then do_atan2(p,a,b)
!	when jpower         then do_power(p,a,b)
	when jptr           then do_unary(a,kpushptr)
	when jptrto then
		if a^.tag=jptr then			!^a^ cancel out (a might be byref param)
			evalunit(a.a)
		else
			evalref(a)
		fi

	when jaddrof        then
		evalref(a)
		genpc(kconvrefpack)
	when jdaddrof        then
		evalref(a)
		genpc(kconvrefpack)

!	when jaddroffirst   then do_addrof(p,a)
	when jconvert       then
		do_convert(p.mode, a)
!		evalunit(a)
!		genpc_int(kconvert,p.mode)
	when jtypepun       then
		evalunit(a)
		genpc_int(ktypepun,p.mode)

!	when jconvertref    then do_convertref(p,a,b)
!	when jautocast      then do_autocast(p,a,b)
	when jtypeconst     then
		genpc_int(kpusht,p.mode)
!	when jpacktypeconst     then
!		genpc_int(kpushtp,p.value)
!	when joperator      then do_operator(p,a,b)
	when jincrload, jdecrload, jloadincr, jloaddecr then
		do_incr(p,a,res)

!	when jcvtargetcode  then do_cvtargetcode(p,a,b)
!	when jassem         then do_assem(p,a)

!	when jmakereftype then
!		evalunit(a)
!		genpc(kmakereftype)

	when jtypename then
		genpc_name(ktypename,a.def)

	when jnil           then
		genpc(kpushnil)
	when jpnil           then
		genpc(kpushpnil)

	when jraise         then do_unary(a,kraise)

	when jdocstring then
	when jnull then
		genpc(kpushvoid)


	else
		gerror_s("UNSUPPORTED TAG:",JTAGNAMES[P.TAG],p)
	endswitch

!CPL "## END EVALUNIT",jtagnames[p.tag],=JHASVALUE[P.TAG],=RES,=JTAGNAMES[P.TAG]
!GENCOMMENT("ENDEVAL")
	case jhasvalue[p.tag]
	when 0 then
		if res then
			gerror_s("Value expected:",jtagnames[p.tag])
		fi
	when 1 then
		if not res then
			if p.tag=jcall and procflag=1 then		!procs have no ret value
			elsif p.tag in [jincrload,jdecrload,jloadincr,jloaddecr] then
			elsif p.tag=jcallhost and hostisfn[p.index]=0 then
			else
				genpc_int(kunshare,1)
			fi
		fi
	esac						!else ignore when 2, as already dealt with
!GENCOMMENT("ENDEVAL2")
end

global proc gencodemodule(int n)=
	symbol d,e
	int lab

	currmodule:=&moduletable[n]
	stcurrproc:=stcurrmodule:=currmodule.def

	resetpcl(moduletable[n].sourcelen)

!CPL "CODEGEN FOR",CURRMODULE.NAME

	gencomment("Module code:")

!jump around stop/raise block needed for reentry
	if n=1 then
		lab:=createfwdlabel()
		genpc_lab(kjump,lab)
!		genpc_int(kpushci,0)
!		genpc(kstop)
		genpc(kstoprunproc)
		stopseq:=pcllast
!CPL "SETSTOPSEQ",STOPSEQ^,=STOPSEQ,PCLNAMES[(STOPSEQ)^],=STOPSEQ,=PCLLAST

		raiseseq:=pcllast+1
		genpc_int(kpushci,0)
		genpc(kraise)
		definefwdlabel(lab)
	fi

	d:=stcurrmodule.deflist
	while d do
		if d.nameid=staticid and d.code then
			evalunit(d.code)
			genpc_name(kzpopm,d)
		elsif d.nameid=procid then
!CPL "PROC SEEN",D.NAME
			e:=d.deflist
			while e do
!CPL "DEFLIST",E.NAME
				if e.nameid=staticid and e.code then
!CPL "INIT STATIC IN PROC:",D.NAME
					evalunit(e.code)
					genpc_name(kzpopm,e)
				fi
				e:=e.nextdef
			od
		fi
		d:=d.nextdef
	od	
!	evalunit(stcurrmodule.code,0)

	if n=1 then

!		for i:=2 to nmodules do
		for i:=nmodules downto 2 do
!			[30]char str
			genpc_name(kmodulecall, moduletable[i].def)
!			print @str,"<Call module entry point for",currmodule.name,">"
!			gencomment(str)
		od

		if currmodule.mainfn then
			genpc_name(kcallproc, currmodule.mainfn)
			genopnd_int(0)
		elsif currmodule.startfn then
			genpc_name(kcallproc, currmodule.startfn)
			genopnd_int(0)
		fi

		evalunit(stcurrmodule.code,0)
		genpc_int(kpushci,0)
		genpc(kstop)
	else
		if currmodule.startfn then
			genpc_name(kcallproc, currmodule.startfn)
			genopnd_int(0)
		fi

		evalunit(stcurrmodule.code,0)
		genpc(kmodulereturn)
	fi

!	evalunit(stcurrmodule.code,0)

GENCOMMENT("END MODULE CODE")

	gencomment("Procs:")
	d:=stcurrmodule.deflist
	while d do
		switch d.nameid
		when procid then
			do_procdef(d)
		when staticid then
!		when typeid then
		when recordid then
		when constid then
		when enumid then
		when labelid then
		when typeid then
		when dllprocid then
		when aliasid then
		when macroid then
		else
			gerror_s("?Module def:",namenames[d.nameid])
		end switch

		d:=d.nextdef
	od	

	genpc(kendmodule)


	moduletable[n].pcstart:=pclstart
	moduletable[n].pcend:=pclend
	moduletable[n].pcsize:=pclend-pclstart
	moduletable[n].pcsrcstart:=pclsrcstart

end

proc do_procdef (symbol p) =
	int nfreevars,nnofreevars
	int isfunc
	symbol oldcurrproc

	oldcurrproc:=stcurrproc			!might be a method

	stcurrproc:=p

	retindex:=createfwdlabel()
	isfunc:=p.misfunc

	genprocentry(p,nfreevars,nnofreevars)

!CPL =TTNAME[P.MODE]

!ISFUNC:=1

	if p.code=nil then
		gerror_s("Empty proc body",p.name)
	else
!		do_block(p.code)
!CPL "----------PROC EVAL BODY",=ISFUNC
		evalunit(p.code, isfunc)

	fi

!	if isfunc then
!
!!		if p.owner.nameid<>typeid then		!not a method
!!			if not checkblockreturn(p.code) then
!!				cpl p.name
!!				gerror("Function needs explicit return statement",p.code)
!!			fi
!!		fi
!	fi

	definefwdlabel(retindex)			!common return point
	genprocexit(nfreevars,nnofreevars,isfunc)
	genpc(kprocend)

!	stcurrproc:=stcurrmodule
	stcurrproc:=oldcurrproc
end

proc genprocentry(symbol p, int &nfreevars,&nnofreevars) =		!GENPROCENTRY
	[200]char str
!	int nparamvars,nframevars,isfn,hasretval,fv,nallocvars,ninitvars
!	int i,j,nextoffset
	symbol d
!	[maxparams]symbol varlist
!	[maxparams]int fvlist
!	ref unitrec expr
!	[maxlocals]symbol locals
!	int nlocals
!
!	d:=p.deflist			!list of names in proc
!	isfn:=p.mode<>tvoid
!
!

!	print @str,"Proc",p.name
!	gencomment(str)
	genpc_name(kprocdef, p)

	nprocparams:=nproclocals:=0

	d:=p.deflist
!CPL "PROCDEF DEFLIST:",=P.MISFUNC
	while d do
!CPL D.NAME, NAMENAMES[D.NAMEID]
		case d.nameid
		when frameid then
			++nproclocals
			d.index:=-nproclocals
		when paramid then
			d.index:=nprocparams+retaddrslots
			++nprocparams
		esac

		d:=d.nextdef
	od

	retvaloffset:=nprocparams+retaddrslots
!
!	p.pcentry:=pclnext
	genpc_int(kprocparams, nprocparams)

	p.labelno:=definelabel()
	genpc_int(kprocentry, nproclocals)
	pproclocals:=pclnext-1
!CPL =PPROCLOCALS^,=NPROClocals

	d:=p.deflist
	while d do
		case d.nameid
		when frameid then
			if d.code then
!CPL =D.NAME,D.CODE
				evalunit(d.code)
				genpc_name(kzpopf, d)
			fi
		esac

		d:=d.nextdef
	od



!				nextoffset-:=1
!				varlist[j].index:=nextoffset
!			fi
!		od
!	od
!
!	genpc_s(kprocstart,p)
!	genopnd_int(nparamvars-hasretval)
!	p.index:=pcindex+1
!
!	if ninitvars+nallocvars then
!		genpc_int(kstackframe,ninitvars+nallocvars)
!	fi
!
!	for i:=1 to nframevars do
!		d:=varlist[i]
!		if d.code then
!			evalexpr(d.code)
!			d.attribs.ax_used:=1
!			genpc_s(kzpop_f,d)
!		fi
!	od
end

proc genprocexit(int nfree,nnofree,isfunc)=		!GENPROCEXIT
!CPL "GENPROCEXIT"

	if isfunc then
		genpc_int(kpopretval,(nprocparams+1)*varsize)
	fi
	if nproclocals then
		genpc_int(kunshare,nproclocals)
	fi

!	if nnofree then
!		genpc_int(kaddsp,nnofree)
!	fi
!	if nfree then
!		genfree(nfree)
!	fi

	genpc(kreturn)
!	gencomment("End")
end

proc evalref(unit p)=
	var unit a,b,c
	symbol d
	int lab1,lab2
	a:=p.a
	b:=p.b

!CPL "EHERE"
	switch p.tag
	when jname then
		d:=p.def
		if d.nameid=paramid and d.mbyref then
			genpc_name(kpushf,d)
		else
			genpc_name(kpushmref+d.isframe,d)
		fi

	when jdot then! do_binref(a,b,kdotref)
		evalunit(a)
		genpc_name(kdotref,b.def)
	when jindex then! do_binref(a,b,kindexref)
		evalunit(a)
		evalunit(b)
		genpc(kindexref)

!	when jslice then do_binref(a,b,ksliceref)

	when jdotindex then! do_binref(a,b,kdotindexref)
!		evalunit(a)
		evalref(a)
		evalunit(b)
		genpc(kdotindexref)

!	when jdotslice then do_binref(a,b,kdotsliceref)
	when jkeyindex then! do_binref(a,b,kkeyindexref)
		evalunit(a)
		evalunit(b)
		if b.nextunit then gerror("Def val not allowed") fi
!PRINTUNIT(B)
!cpl b.nextunit
		genpc(kkeyindexref)

	when jptr then
		evalunit(a)

	when jif then
		lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)
		lab2:=createfwdlabel()

		genjumpcond(kjumpf,p.a,lab1)
		evalref(p.b)
		genjumpl(lab2)
		definefwdlabel(lab1)
		evalref(p.b.nextunit)
		definefwdlabel(lab2)
	else
!		case p.tag
!		when jif then
!			do_if(p,a,b,c,1)
!		when jlongif then
!			do_longif(p,a,b,1)
!		when jselect then
!			do_select(p,a,b,c,1)
!		when jswitch then
!			do_switch(p,a,b,c,0,1)
!		when jcase then
!			do_case(p,a,b,c,0,1)
!		else
!			PRINTUNIT(P)
			gerror_s("evalref",jtagnames[p.tag])
!		esac
	end switch
end

proc DUMMY=
end

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	var unit q,r,s
	var int oldpos, lab2

!	oldlineno:=qlineno
!	qlineno:=p.lineno

	q:=p.a
	r:=p.b

	switch p.tag
	when jandl then
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

	when jorl then
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

	when jnotl then
		case opc
		when kjumpf then
			genjumpcond(kjumpt,q,lab)
		when kjumpt then
			genjumpcond(kjumpf,q,lab)
		esac

	when jistruel then
		genjumpcond(opc,q,lab)

	when jblock then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc,q,lab)

	when jeq,jne,jlt,jle,jge,jgt then
		gcomparejump(opc,p,q,r,lab)

	when jcmpchain then
		gencomment("CMPCHAIN NOT DONE")
!		r:=q.nextunit
!		i:=1
!		if opc=kjumpf then
!			while r do
!				evalunit(q)
!				evalunit(r)
!				genpc_cond(k_jumpcc,reversecond(p.cmpopindex[i]),genlabel(lab))
!				setmode_u(q)
!				++i
!				q:=r
!				r:=r.nextunit
!			od
!		
!		else
!			lab2:=createfwdlabel()
!			while r do
!				evalunit(q)
!				evalunit(r)
!				if r.nextunit then
!					genpc_cond(k_jumpcc,reversecond(p.cmpopindex[i]),genlabel(lab2))
!				else
!					genpc_cond(k_jumpcc,p.cmpopindex[i],genlabel(lab))
!				fi
!				setmode_u(q)
!				++i
!				q:=r
!				r:=r.nextunit
!			od
!			definefwdlabel(lab2)
!		fi
	else
		evalunit(p)
		genpc_lab((opc=kjumpt|kjumptrue|kjumpfalse),lab)
	endswitch
	qpos:=oldpos

end

proc gcomparejump(int jumpopc,unit p,lhs,rhs,int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	var int cond,opc

	cond:=p.tag				!eqop,neop, etc

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	case cond
	when jeq then opc:=kjumpeq
	when jne then opc:=kjumpne
	when jlt then opc:=kjumplt
	when jle then opc:=kjumple
	when jge then opc:=kjumpge
	when jgt then opc:=kjumpgt
	else
		gerror("GCOMP: no cond")
	esac

!CPL =OPC

	evalunit(lhs)
	evalunit(rhs)
	genpc_lab(opc,lab)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	genpc_lab(kjump,lab)
end

function reversecond(int op)int=
!reverse conditional operator

	case op
	when jeq then return jne
	when jne then return jeq
	when jlt then return jge
	when jle then return jgt
	when jge then return jlt
	when jgt then return jle
	esac
	return 0
end

proc stacklooplabels(ref int a,b,c)=	!STACKLOOPLABELS
!a is a list of labels associated with a loop, usually 4 in order A,B,C,D
	if loopindex>=maxloopindex then
		gerror("Too many nested loops")
	fi
	++loopindex
	loopstack[loopindex,1]:=a
	loopstack[loopindex,2]:=b
	loopstack[loopindex,3]:=c
!	loopstack[loopindex,4]:=d
!	trylevelstack[loopindex]:=trylevel
end

proc unstacklooplabels=	!UNSTACKLOOPLABELS
	--loopindex
end

function findlooplabel(int k,n)int=	!FINDLOOPLABEL
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
	var int i

	if n=0 then			!outermost loop
		i:=1
	else
		i:=loopindex-(n-1)		!point to entry
	fi

	if i<1 or i>loopindex then
		gerror("Bad loop index")
	fi

	looptrylevel:=trylevelstack[i]
	return loopstack[i,k]^
end

proc do_assign(unit a,b, int res,deepcopy=0)=
	var unit q
	var int n

!CPL "DO/ASS", =RES

	if a.tag=b.tag=jmakelist then
		if deepcopy or res then gerror("mult/ass::=") fi
		do_multassign(a,b)
		return
	fi

	evalunit(b)
	if deepcopy then
		genpc(kcopy)
	fi

	if res then
		genpc(kdupl)
	fi

	do_store(a)

!	case a.tag
!	when jname		then genpc_name(kpopm+a.def.isframe,a.def)
!	when jdot		then! do_bin(a.a, a.b, kpopdot)
!		evalunit(a.a)
!		genpc_name(kpopdot,a.b.def)
!
!	when jindex		then do_bin(a.a, a.b, kpopindex)
!	when jdotindex	then do_bin(a.a, a.b, kpopdotindex)
!	when jptr		then
!		evalunit(a.a)
!		genpc(kpopptr)
!
!	when jkeyindex	then do_bin(a.a, a.b, kpopkeyindex)
!
!	when jmakelist then			!assign to multiple destinations
!		q:=a.a
!		if q=nil then
!			gerror("assign to ()?")
!		else
!			n:=0
!			while q do
!				++n
!				evalref(q)
!				q:=q.nextunit
!			od
!		fi
!		genpc_int(kpoplist,n)
!
!	when jif then
!		evalref(a)
!		genpc(kpopptr)
!
!	else
!		gerror_s("Can't assign to this unit yet:",jtagnames[a.tag], a)
!	esac
end

proc do_bin(unit a,b, int opc)=
	evalunit(a)
	evalunit(b)
	genpc(opc)
end

proc do_binref(unit a,b, int opc)=
	evalref(a)
	evalunit(b)
	genpc(opc)
end

proc do_unary(unit a, int opc)=
	evalunit(a)
	genpc(opc)
end

proc do_unaryref(unit a, int opc)=
	evalref(a)
	genpc(opc)
end

proc do_pushlist(unit a, int n)=
	if a then
		pushlist(a)
	fi
end

proc pushlist(unit a)=
	if a.nextunit then
		pushlist(a.nextunit)
	fi
	evalunit(a)
end

proc do_makedict(unit a, int n)=
	to n do
		if a.tag=jkeyvalue then
			evalunit(a.a)
			evalunit(a.b)
		else
			gerror("dict not key:val")
		fi
		a:=a.nextunit
	od
	genpc_int(kmakedict,n)
end

proc do_call(unit p,a,b,int res, &procflag)=
	var int nargs, nsimple, isfunc, kwdindex,needpushptr
	symbol d
	unit c
	[maxparams]unit arglist

	isfunc:=1
	procflag:=0
	nargs:=nsimple:=0
	kwdindex:=0
	needpushptr:=0
	c:=b

	while c do
		arglist[++nargs]:=c
		if c.tag in [jintconst, jrealconst] then ++nsimple fi
		if c.tag=jkeyword then
			if kwdindex=0 then kwdindex:=nargs fi
		elsif kwdindex then
			gerror("Non-kwd follows kwd arg")
		fi
		c:=c.nextunit
	od

	if a.tag=jname then
		d:=a.def
retry::
		case d.nameid
		when procid then
			if d.misfunc then
				genpc(kpushvoid)
				nargs:=pushparams(d, arglist, nargs, kwdindex)
!				genpc_name(kcallfn,d)
				genpc_name(kcallproc,d)
			else					!proc call
				isfunc:=0
				procflag:=1
				nargs:=pushparams(d, arglist, nargs, kwdindex)
				genpc_name(kcallproc,d)
			fi
			genopnd_int(nargs)

		when dllprocid then
			genpc(kpushvoid)
			nargs:=pushparams(d, arglist, nargs, kwdindex)
			genpc_name(kcalldll,d)
			genopnd_int(nargs)
			genopnd_int(d.mode)
		when aliasid then
			d:=d.alias
			goto retry
		when staticid, frameid, paramid then
!			evalunit(a)
!			needpushptr:=1
			goto docallptr
		else
			gerror_s("CAN'T CALL:",namenames[d.nameid])
		esac
	else
docallptr::
!gerror("CALLPTR")
		if kwdindex then gerror("Kwd params not allowed for fnptr") fi
		genpc(kpushvoid)
		for i:=nargs downto 1 do
			evalunit(arglist[i])
		od
		evalunit(a)
		if needpushptr then
			genpc(kpushptr)
		fi
		genpc(kcallptr)
		genopnd_int(nargs)
		genopnd_int(0)
	fi

!CPL "DOCALL",=RES, =ISFUNC

	if res and not isfunc then
		gerror("Func ret value expected")
	fi

	if nargs then
		genpc_int(kunshare, nargs)
	fi
end

function pushparams(symbol d, []unit &arglist, int nargs, kwdindex)int=
!push args for a known, named function
!will deal with missing/optional args, default values, and keyword params
!should work also for dll procs
!In all cases, first nparams items in d.deflist will be parameter names,
!For dlls with no named params, the entries will be $1 etc.

	var int nparams, extra,n
	[maxparams]symbol paramlist
	[maxparams]byte byreflist
	symbol e, p

	nparams:=d.nparams
	e:=d.deflist
	n:=0
	while e do
		++n
		paramlist[n]:=e
		byreflist[n]:=e.mbyref
		e:=e.nextdef
	od

!CPL =N
!CPL =NPARAMS
!
!	IF N<>NPARAMS THEN GERROR("PUSHPARAMS: N<>NPARAMS") FI

!CPL "PUSHPARAMS FOR",D.NAME
!GERROR("PUSHPARAMS NEEDS REWRITING")
	if kwdindex then
		pushkwdparams(d, arglist, nargs, kwdindex)
		return d.nparams
	fi

	extra:=0

	if nargs=nparams then		!simplest case
!CPL "SIMPLE"
		for i:=nargs downto 1 do
!			evalunit(arglist[i])
			evalparam(arglist[i],byreflist[i])
		od
		return nargs
	fi

	if nargs>nparams then
		if not d.mvarparams then
			gerror("Too many args")
		fi
		for i:=nargs downto nparams+1 do
			evalunit(arglist[i])			!o/p variadic args
		od
		extra:=nargs-nparams
		nargs:=nparams
	fi

	if nargs<nparams then					!trailing args missing
!		e:=d.deflist
!		for i to nparams do
!			paramlist[i]:=e
!			e:=e.nextdef
!		od

		for i:=nparams downto nargs+1 do
			p:=paramlist[i]
			if not p.code and not p.moptional then
				gerror_s("Param not optional:",strint(i))
			fi
			if p.code then
				if byreflist[i] then gerror("byref with default val") fi
				evalunit(p.code)
			else
				genpc(kpushvoid)
			fi
		od

	fi

	for i:=nargs downto 1 do
!		evalunit(arglist[i])
		evalparam(arglist[i],byreflist[i])
	od

	return nparams+extra
end

proc evalparam(unit a, int byref)=
	if byref then
		evalref(a)
	else
		evalunit(a)
	fi
end


proc pushkwdparams(symbol d, []unit &arglist, int nargs, kwdindex)=
	var int nparams, i,j,k
	[maxparams]symbol paramlist
	[maxparams]unit keyunits
	unit p,q
	symbol e

!CPL =KWDINDEX

!CPL "PUSHPARAMS FOR",D.NAME
!GERROR("PUSHPARAMS NEEDS REWRITING")
	nparams:=d.nparams

	e:=d.deflist
	for i to nparams do
		paramlist[i]:=e
		if e.mbyref then
			gerror("byref with kwd param")
		fi
		e:=e.nextdef
	od

	for i:=kwdindex to nparams do
		keyunits[i]:=nil			!indicate param not set
	od

	for i:=kwdindex to nargs do
		p:=arglist[i]
		q:=p.a
		if q.tag<>jname then gerror("kwd not a name") fi
		e:=q.def
		k:=0
		for j:=1 to nparams do
			if eqstring(e.name, paramlist[j].name) then
				k:=j
				exit
			fi
		od

		if k=0 then gerror_s("Can't find kwd param:",e.name) fi
		if k<kwdindex then gerror_s("Kwd arg already positional:",e.name) fi
		if keyunits[k] then gerror_s("Repeating kwd arg:",e.name) fi

		keyunits[k]:=p.b
	od

	for i:=kwdindex to nparams do
		if keyunits[i]=nil then
			q:=paramlist[i].code
			if q=nil and not paramlist[i].moptional then
				gerror_s("Param not optional:",strint(i))
			fi
			keyunits[i]:=q			!q is nil when default value not set
		fi
	od

	for i:=nparams downto kwdindex do
		if keyunits[i] then
			evalunit(keyunits[i])
		else
			genpc(kpushvoid)
		fi
	od
	for i:=kwdindex-1 downto 1 do
		evalunit(arglist[i])
	od
end

proc do_if(unit p,a,b,pelse, int res)=
	var int lab1,lab2

	lab1:=createfwdlabel()				!dest label of main condition (to end of if, or start if else)

	if pelse or res then lab2:=createfwdlabel() fi	!label past else part

	genjumpcond(kjumpf,a,lab1)

	evalunit(b,res)

	if pelse or res then
		genjumpl(lab2)
		definefwdlabel(lab1)
		if pelse then
			evalunit(pelse,res)
		else
			genpc(kpushvoid)
		fi
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_do (unit p,a)=
	var int lab_abc,lab_d,lab_test
	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(&lab_abc, &lab_abc, &lab_d)

	evalunit(a,0)

	if p.tag=jdo then
		genjumpl(lab_abc)
	fi
	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_exit(unit p,int k) =
	var int n,index

	index:=p.a.value
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k,index)
	if n=0 then
CPL "BAD LOOP"
!		gerror("Bad exit/loop index",p)
	else
		genjumpl(n)
	fi
end

proc do_to (unit p,pcount,pbody)=
	var int lab_b,lab_c,lab_d
	var symbol temp
	var unit pav

	pav:=pcount.nextunit
	temp:=pav.def

!	lab_a:=definelabel()
	evalunit(pcount)
	genpc_name(kzpopm+temp.isframe,temp)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(&lab_b,&lab_c,&lab_d)

!check for count being nonzero
	if pcount.tag<>jintconst then			!assume const limit is non-zero
		genpc_name(kpushm+temp.isframe,temp)
		genpc_int(kpushci,0)
		genpc_lab(kjumple,lab_d)

	elsif pcount.value<=0 then		!const <=0, skip body
		genpc_lab(kjump,lab_d)
	fi

	definefwdlabel(lab_b)
	evalunit(pbody,0)
	definefwdlabel(lab_c)

	genpc_lab(ktom+temp.isframe,lab_b)
	genopnd_name(temp)


	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_while(unit p,pcond,pbody) =
	var int lab_b,lab_c,lab_d

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(&lab_b, &lab_c, &lab_d)

	genjumpl(lab_c)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalunit(pbody,0)

	definefwdlabel(lab_c)

	genjumpcond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p,a,b) =
	var int lab_b, lab_c, lab_d

	lab_b:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(&lab_b, &lab_c, &lab_d)

	evalunit(a,0)

	definefwdlabel(lab_c)

	unless b.tag=jintconst and b.value=0 then
		genjumpcond(kjumpf,b,lab_b)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_for(unit p,pvar,pbody)=
! a = pvar, pfrom, pto, [pstep]
! b = pbody [pelse]
	var unit pfrom, pto, pstep, pelse,plimit,pautovar
	var symbol dvar, limitvar
	var int lab_b,lab_c,lab_d,lab_e,opc,oldqpos
	var int step, fromval, limit, jumpinto

!CPL "FOR1"
	pfrom:=pvar.nextunit
	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pautovar:=nil
	if pstep then
		gerror("By N not implem")
	fi

	pelse:=pbody.nextunit
	dvar:=pvar.def

	if pto.tag not in [jintconst,jname] or
		 pto.tag=jname and pto.def.isframe<>dvar.isframe then
		pautovar:=createavnamex(stcurrproc)
	fi

	case p.tag
	when jforup then
		step:=1

	when jfordown then
		step:=-1
	esac

	jumpinto:=1			!assume jumping straight into increment

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(&lab_b,&lab_c,&lab_d)

	if pfrom.tag=jintconst then		!can incr/decr directly
		fromval:=pfrom.value
!see if limit is known
		if pto.tag=jintconst then
			limit:=pto.value
			if (step=-1 and fromval>=limit) or (step=1 and fromval<=limit) then 	!at least 1 iteration
				jumpinto:=0
			fi
		fi
		if jumpinto then
			if step<0 then
				++fromval
			else
				--fromval
			fi
			pfrom.value:=fromval
		fi
		genpc_int(kpushci,pfrom.value)

		genpc_name(kpopm+dvar.isframe,dvar)
	else
		evalunit(pfrom)
		genpc_name(kpopm+dvar.isframe,dvar)

		genpc_name((step<0|kincrtom|kdecrtom)+dvar.isframe,dvar)
	fi

	if pautovar then
		evalunit(pto)
		limitvar:=pautovar.def
		genpc_name(kzpopm+limitvar.isframe,limitvar)
		pto:=pautovar
	else
		limitvar:=pto.def
	fi

	if jumpinto then
		genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C::
	fi
	definefwdlabel(lab_b)

	evalunit(pbody,0)				!do loop body

	definefwdlabel(lab_c)

	if pto.tag=jintconst then
		opc:=(step<0|kfordmci|kformci)+dvar.isframe
	elsif dvar.isframe=limitvar.isframe then
		opc:=(step<0|kfordmm|kformm)+dvar.isframe
	else
!CPL =NAMENAMES[DVAR.NAMEID],DVAR.NAME
!CPL =NAMENAMES[LIMITVAR.NAMEID],LIMITVAR.NAME
		gerror("for:mixed m/f vars")
	fi

	oldqpos:=qpos
	qpos:=p.pos
	genpc_lab(opc,lab_b)
	qpos:=oldqpos
	genopnd_name(dvar)

	if pto.tag=jintconst then
		genopnd_int(pto.value)
	else
		genopnd_name(limitvar)
	fi

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse,0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_forx(unit p,pvar,pbody)=
! a = pvar, pbounds
! b = pbody [pelse]
	var unit pbounds, pelse,plimit,pautovar
	var symbol dvar, limitvar
	var int lab_b,lab_c,lab_d,lab_e,opc

	pbounds:=pvar.nextunit

!	pautovar:=createavnamex()			!upper bound
!	resolvename(stcurrproc,pautovar)
	pautovar:=createavnamex(stcurrproc)

	pelse:=pbody.nextunit
	dvar:=pvar.def

	if p.tag=jfordownx then
		gerror("Can't down inrev yet")
	fi

!now start generating code
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(&lab_b,&lab_c,&lab_d)

	evalunit(pbounds)				!stack has lwb, upb
!CPL "FORX",PCLNAMES[PBOUNDS.TAG]
	limitvar:=pautovar.def
	genpc_name(kzpopm+limitvar.isframe,limitvar)

!CPL =LIMITVAR.NAME
	genpc(kdecr)
	genpc_name(kpopm+dvar.isframe,dvar)		!from value
!CPL =DVAR.NAME

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C::
	definefwdlabel(lab_b)

	evalunit(pbody,0)				!do loop body

	definefwdlabel(lab_c)

	if dvar.isframe=limitvar.isframe then
		genpc_lab(kformm+dvar.isframe,lab_b)
	else
		gerror("forx:mixed m/f")
	fi
	genopnd_name(dvar)
	genopnd_name(limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse,0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_print(unit p,a,b)=
	var int issprint
	var unit x

	issprint:=p.tag=jsprint

	if issprint then
		callhostfn(host_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(host_startprint)
		else
			callhostfn(host_startprintcon)
		fi
	fi

	x:=b

	case p.tag
!when jfprint, jfprintln then
!	if not needprintend then
!		needprintend:=1
!		callhostfn(host_startprintcon)
!	fi
	when jcprint, jcprintln then
		callhostfn(host_startprintcon)
	esac

	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(host_print)
		when jnogap then
			callhostfn(host_printnogap)
		when jspace then
			callhostfn(host_printspace)
		else
!		genpc(kpushvoid)
			evalunit(x)
			callhostfn(host_print_nf)
		esac
		x:=x.nextunit
	od

	if p.tag=jprintln then
		callhostfn(host_println)
	fi
	if issprint then
		genpc(kpushvoid)
		callhostfn(host_strendprint)
	else
		callhostfn(host_endprint)
	fi
end

proc do_fprint (unit p,a,b,c)=
	var int issfprint
	var unit x

	issfprint:=p.tag=jsfprint

	if issfprint then
		callhostfn(host_strstartprint)
	else
		if a then
			evalunit(a)
			callhostfn(host_startprint)
		else
			callhostfn(host_startprintcon)
		fi
	fi

	evalunit(b)					!format string
	callhostfn(host_setformat)

	x:=c
	while x do
		case x.tag
		when jfmtitem then
			evalunit(x.b)
			evalunit(x.a)
			callhostfn(host_print)
		when jnogap then
			callhostfn(host_printnogap)
		else
			genpc(kpushvoid)
			evalunit(x)
			callhostfn(host_print)
		esac
		x:=x.nextunit
	od

	if p.tag=jfprintln then
		callhostfn(host_println)
	fi
	if issfprint then
		genpc(kpushvoid)
		callhostfn(host_strendprint)
	else
		callhostfn(host_endprint)
	fi

end

proc do_read (unit p,a,b)=
var unit x,xloop

if p.tag=jreadln then
	if a then
		evalunit(a)
		callhostfn(host_readln)
	else
		genpc(kpushvoid)
		callhostfn(host_readln)
	fi
fi

xloop:=b
while xloop do
	x:=xloop
	genpc(kpushvoid)
	if x.tag=jfmtitem then
		evalunit(x.b)
		callhostfn(host_sread)
		x:=x.a
	else
		genpc(kpushvoid)
		callhostfn(host_sread)
	fi
	if x.tag=jname then
		genpc_name(kpopm+x.def.isframe,x.def)
	else
		evalref(x)
		genpc(kpopptr)
	fi
	xloop:=xloop.nextunit
od
end

proc do_forall (unit p,pindex,pbody)=
!I think form pvar/prange into blocks, then those can be stored together
! a = pindex, plist, pvar
! b = pbody, [pelse]

	var int lab_b,lab_c,lab_d,lab_e
	var unit ploopvar, plist, pelse, plimitvar, plistvar
	var symbol indexvar,limitvar,loopvar, listvar

	plist:=pindex.nextunit
	ploopvar:=plist.nextunit

	if ploopvar=nil then			!no discrete index var
		ploopvar:=pindex

!		pindex:=createavnamex()
!		resolvename(stcurrproc,pindex)
		pindex:=createavnamex(stcurrproc)

	fi
	loopvar:=ploopvar.def

!	plimitvar:=createavnamex()			!upper bound
!	resolvename(stcurrproc,plimitvar)
	plimitvar:=createavnamex(stcurrproc)

	limitvar:=plimitvar.def
	indexvar:=pindex.def

	if plist.tag<>jname or plist.def.isframe<>loopvar.isframe then			!complex list

!		plistvar:=createavnamex()
!		resolvename(stcurrproc,plistvar)
		plistvar:=createavnamex(stcurrproc)

		listvar:=plistvar.def
		evalunit(plist)
		genpc_name(kzpopm+listvar.isframe,listvar)
	else
		plistvar:=plist
		listvar:=plistvar.def
	fi

	unless indexvar.isframe=loopvar.isframe=listvar.isframe then
		gerror("forall: mixed vars")
	end

	pelse:=pbody.nextunit

	if p.tag=jforallrev then
		gerror("Forall/rev not ready")
	fi

!set up initial loop var
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	lab_e:=(pelse|createfwdlabel()|lab_d)
	stacklooplabels(&lab_b,&lab_c,&lab_d)

!assume plist is a var where bounds are not known
!(can be optimised for a const range or a const list)
	genpc_name(kpushm+listvar.isframe, listvar)			!load the list
	genpc(kboundsx)			!extract bounds as (lower, upper); upper is tos

	genpc_name(kzpopm+listvar.isframe,limitvar)		!limit:=upb
	genpc(kdecr)
	genpc_name(kzpopm+indexvar.isframe,indexvar)		!index:=lwb-1 (will incr first thing)

	genjumpl(lab_c)			!jump straight into incr/jump Kfor cmdcode at C::

	definefwdlabel(lab_b)

!start of iteration, set up next loop variable
	genpc_name(kpushm+listvar.isframe,listvar)
	evalunit(pindex)

	if p.tag in [jforall,jforallrev] then
		genpc(kindex)
	else
		genpc(kdotindex)
	fi
	genpc_name(kpopm+loopvar.isframe,loopvar)

	evalunit(pbody,0)			!do loop body

	definefwdlabel(lab_c)

	if indexvar.isframe=limitvar.isframe then
		genpc_lab(kformm+indexvar.isframe,lab_b)
	else
		gerror("forall:mixed m/f")
	fi
	genopnd_name(indexvar)
	genopnd_name(limitvar)

	if pelse then
		definefwdlabel(lab_e)
		evalunit(pelse,0)			!any else part
	fi

	definefwdlabel(lab_d)
	unstacklooplabels()
end

proc do_case (unit p,pindex,pwhenthen,int res) =
!also temporarily deal wit switch/doswitch

	var int lab_a,lab_d
	var int loopsw,labnextwhen,labstmtstart,fmult
	var unit w,wt,pelse

	loopsw:=p.tag=jdocase or p.tag=jdoswitch
	pelse:=pindex.nextunit

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(&lab_a,&lab_a,&lab_d)
	else
		lab_d:=createfwdlabel()
	fi

	evalunit(pindex)			!load test expr p to t

	wt:=pwhenthen
	while wt do
		w:=wt.a
		fmult:=w.nextunit<>nil

		labnextwhen:=createfwdlabel()

		if fmult then
			labstmtstart:=createfwdlabel()
		fi

		while w do
			evalunit(w)
			w:=w.nextunit
			if w then					!not last
				genpc_lab(kjumptesteq,labstmtstart)
			else
				genpc_lab(kjumptestne,labnextwhen)
			fi
		od
		if fmult then
			definefwdlabel(labstmtstart)
		fi
		evalunit(wt.b,res)

		if not loopsw then
			genjumpl(lab_d)
		else
			genjumpl(lab_a)
		fi
		definefwdlabel(labnextwhen)
		wt:=wt.nextunit
	od

!at else part
	genpc_int(kunshare,1)

	if pelse then
		evalunit(pelse,res)
	elsif res then
		genpc(kpushvoid)
	fi
	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
		definefwdlabel(lab_d)
	fi
end

proc do_try(unit p,a,b) =
	var int labend,labx
	var unit ptry,x,pexcept,pexcode

	++trylevel
	labend:=createfwdlabel()
	ptry:=a
	labx:=createfwdlabel()

	pexcept:=b

	if pexcept=nil then
		gerror("try: no except")
	elsif pexcept.nextunit then
		gerror("Try:multiple except block not implemented")
	fi

	while pexcept do
		pexcode:=pexcept.a
		if pexcode=nil or pexcode.nextunit then
			gerror("Try:multiple except codes not implemented")
		fi
!GENCOMMENT("TRY/EXCEPT")
		genpc_lab(kpushtry,labx)
		genopnd_int(getconstvalue(pexcode))
		genopnd_int(1)
		evalunit(ptry,0)
		genjumpl(labend)
		definefwdlabel(labx)
		evalunit(pexcept.b,0)
		definefwdlabel(labend)
		pexcept:=pexcept.nextunit
	od

	genpc_int(kaddsp,1)
	--trylevel
end

function unitstoarray(unit p, ref[]unit plist, int maxunits)int=
!convert a linked list of units to a linear list
!return number of units
	var int n

	n:=0
	while p do
		if n>=maxunits then
			gerror("UTA Too many units")
		fi
		plist^[++n]:=p
		p:=p.nextunit
	od
	
	return n
end

proc do_select(unit pindex,pplist,int res)=
!generate selectx expression
	var int n,labend,i,lab,elselab
	var unit x,pelse

	var [maxswitchrange]unit plist
	var [maxswitchrange+1]int labels

!CPL "SEL1"
	pelse:=pindex.nextunit

	n:=unitstoarray(pplist,&plist,maxswitchrange)

!CPL "SEL2"
	if n>maxswitchrange then
		gerror("Selectx too complex")
	fi

	labend:=createfwdlabel()

!CPL "SEL3"
	evalunit(pindex)
	genpc_int2(kselect,n,1)
!CPL "SEL4"

	for i:=1 to n do
		labels[i]:=pclnext-pclstart		!store destination code index
		genpc_lab(kjumplabel,0)
	od
!CPL "SEL5"
	labels[n+1]:=pclnext-pclstart
	genpc_lab(kjumplabel,0)

!scan when statements again, o/p statements
	i:=1
	for i:=1 to n do
		x:=plist[i]
		lab:=definelabel()

		(pclstart+labels[i]+1)^:=lab
		evalunit(x,res)

		genjumpl(labend)	!break to end of statement
	od

	elselab:=definelabel()

	(pclstart+labels[n+1]+1)^:=elselab

	if pelse then
		evalunit(pelse,res)
	elsif res then
		genpc(kpushvoid)
	fi

	genpc(knop)

	definefwdlabel(labend)
end

proc do_andl(unit x,y)=
	var int a,b

	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpf,x,a)
	genjumpcond(kjumpf,y,a)

	genpc_int(kpushci,1)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci,0)
	genpc(knop)
	definefwdlabel(b)
end

proc do_orl(unit x,y)=
	var int a,b
	a:=createfwdlabel()
	b:=createfwdlabel()

	genjumpcond(kjumpt,x,a)
	genjumpcond(kjumpt,y,a)
	genpc_int(kpushci,0)
	genjumpl(b)
	definefwdlabel(a)
	genpc_int(kpushci,1)
	genpc(knop)
	definefwdlabel(b)
end

proc do_incr(unit p,a, int res)=
	symbol d
	if res then
		do_unaryref(a,jpclcodes[p.tag])
	elsif a.tag=jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			goto dounary
		else
			genpc_name((p.tag in [jincrload,jloadincr]|kincrtom|kdecrtom)+a.def.isframe,a.def)
		fi
	else
dounary::
		do_unaryref(a,(p.tag in [jincrload,jloadincr]|kincrptr|kdecrptr))
	fi
end

proc do_new(unit p)=
	var int n
	var unit q

	n:=p.nparams
	if n<1 or n>3 then gerror("new args") fi

	q:=p.a

	genpc(kpushvoid)
	to n do
		evalunit(q)
		q:=q.nextunit
	od
	to 3-n do
		genpc(kpushvoid)
	od

	callhostfn(host_new)

end

function checkblockreturn(unit p)int=
!p should be a block unit
!check that the last statement is a return; return 1/0 for return/not return
!just allow or check for return/if/longif for now
ref unitrec q,r

if p=nil then return 0 fi
if p.tag<>jblock then gerror("CBR?") fi

q:=p.a
if q=nil then return 0 fi		!empty block

while r:=q.nextunit do			!get q=last stmt in block
	q:=r
od

case q.tag
when jreturn then			!that's an easy one...
	return 1

when jif then
	return checkblockreturn(q.b) and checkblockreturn(q.b.nextunit)		!all branches must have a return
!when jlongif then
!	r:=q.a						!list of elsif units
!	while r do
!		if not checkblockreturn(r.b) then
!			return 0
!		fi
!		r:=r.nextunit
!	od
!	return checkblockreturn(q.b)		!else must have return too
esac
return 0
end

proc do_callhost(unit p, a, int res)=
	int index:=p.index
	int isfunc:=hostisfn[index]
	int nargs, nparams, fparams
	[10]unit plist
	unit q


	if res and not isfunc then
		gerror("Host proc not a function")
	fi

	if isfunc then
		genpc(kpushvoid)
	fi

	nargs:=0
	q:=a

	while q do
		if nargs>plist.upb then
			gerror("Too many host args")
		fi
		plist[++nargs]:=q

		q:=q.nextunit
	od

	if index=host_allparams and a=nil then
		nparams:=1
	else
		nparams:=nargs
	fi

	if nparams=0 and hostlvset[index] then
		gerror("LV hostfn: needs 1+ params")
	fi
	fparams:=hostnparams[index]
	if nparams>fparams then
		gerror("Hostfn too many params")
	fi

	to fparams-nparams do
		genpc(kpushvoid)
	od

!Finally, push all the params, which need to be done in reverse order
	for i:=nparams downto 1 do
		if i=1 and hostlvset[index] then
			evalref(plist[i])
		elsif i=1 and index=host_allparams and nargs=0 then
!			isfn:=stcurrproc^.mode<>tvoid
!			genpc_name(kpushap,stcurrproc)
			genpc_name(kpushmref,stcurrproc)
		else
			evalunit(plist[i])
		fi
	od  

	callhostfn(index,res)
end

proc callhostfn(int fnindex,calledasfn=0)=
!assume caller has verified that fn is a function when calledasfn is true
!called should have pushed retval as needed, and <aparams> params

	genpc_int(kcallhost,fnindex)

!	if hostisfn[fnindex] and not calledasfn then
!		genfree(1)
!	fi
end

proc genfree(int n)=
	genpc_int(kunshare,n)
end

proc do_return(unit p,a)=
	if a then

		evalunit(a)
!		genpc_int(kpopretval,(nprocparams+1)*varsize)
	elsif currfunction=2 then
		gerror("function needs return value")
	fi

!	if nproclocals=0 then
!		genpc(kreturn)
!	else
		genjumpl(retindex)
!	fi
end

proc do_multassign(unit a,b)=
	unit p,q
	[100]unit plist
	int n

	p:=a.a
	q:=b.a
	n:=0

	while p do
		if q=nil then gerror("Too few RHS elems") fi
		evalunit(q)
		if n>=plist.len then gerror("Too many elems") fi
		plist[++n]:=p

		p:=p.nextunit
		q:=q.nextunit
	od

	if q then gerror("Too few LHS elems") fi

	for i:=n downto 1 do
		do_store(plist[i])
	od
end

proc do_store(unit a)=
!store stack value to a
	symbol d
	unit p
	[100]unit plist
	int n

	case a.tag
	when jname then
		d:=a.def
		if d.nameid=paramid and d.mbyref then
			genpc_name(kpushf,d)
			genpc(kpopptr)
		else
			genpc_name(kpopm+d.isframe,d)
		fi

	when jdot then
		evalunit(a.a)
		genpc_name(kpopdot,a.b.def)

	when jindex then
		do_bin(a.a, a.b, kpopindex)
	when jdotindex then
!CPL "STORE DOTINDEX"
!PRINTUNIT(A)

		evalref(a.a)
		evalunit(a.b)
		genpc(kpopdotindex)
!!
!		do_bin(a.a, a.b, kpopdotindex)
	when jptr then
		evalunit(a.a)
		genpc(kpopptr)

	when jkeyindex then
		do_bin(a.a, a.b, kpopkeyindex)

	when jmakelist then			!assign to multiple destinations
		n:=0
		p:=a.a
		while p do
			if n>=plist.len then gerror("Too many elems") fi
			plist[++n]:=p
			p:=p.nextunit
		od
		if n=0 then gerror("Empty lhs list") fi

		genpc_int(kexpand,n)
!		for i:=n downto 1 do
		for i:=1 to n do
			do_store(plist[i])
		od

	when jif then
		evalref(a)
		genpc(kpopptr)

	else
		gerror_s("Can't store to this unit yet:",jtagnames[a.tag], a)
	esac
end

function getconstvalue(unit p)int =
	if p and p.tag=jintconst then
		return p.value
	fi
	gerror("gcv Not const")
	return 0
end

proc do_convert(int m,unit p)=
!apply type-conversion t on expression p
!also do constructors
	int n,elemmode,i,lowerx,lbound,mbase,nfields
	[maxunits]unit plist

	mbase:=ttbasetype[m]

	if p.tag<>jmakelist then		!assume regular type conversion

		if mbase=trecord then
			p:=createunit2(jmakelist,p,nil)
		else
			evalunit(p)
			genpc_int(kconvert,m)
			return
		fi
	fi

!a is a usertype
	n:=unitstoarray(p.a,&plist,maxunits)
	if n and plist[1].tag=jkeyvalue then
		case mbase
		when trecord then
			do_makerecordkv(m,n,plist)
		when tstruct then
			do_makestructkv(m,n,plist)
		else
			gerror("key:value not allowed")
		esac
		return
	fi

	for i:=1 to n do		!any elements need to be pushed
		evalunit(plist[i])
	od

	case mbase
	when trecord, tstruct then
		nfields:=ttlength[m]
		if n then
			checkelems(n,nfields,p)
		else				!allow 0 fields; use defaults of 0
			to nfields do
				genpc_int(kpushci,0)
			od
			n:=nfields
		fi
		genpc_int2((mbase=trecord|kmakerecord|kmakestruct),n,m)

	when tlist then		!probably just a list prefix used
!CPL "LIST",P.LOWER
		lowerx:=p.lower
		genpc_int2(kmakelist,n,lowerx)

	when tarray then
		genpc_int4(kmakearray,p.lower,n,tarray,p.elemtype)

	when tcarray then
		elemmode:=tttarget[m]
		lowerx:=ttlower[m]

		checkelems(n,ttlength[m],p)
		genpc_int4(kmakearray,lowerx,n,m,elemmode)

	when tbits then
		if m=tbits then			!not user-defined
			genpc_int4(kmakebits,p.lower,n,tbits,(p.elemtype=tvoid|tpu1|p.elemtype))
		else
			gerror("user-define bit array not ready")
		fi

	when tset then
		genpc_int(kmakeset,n)

	else
		gerror("Convert list")
!CPL "HERE"
!		evalunit(p)
!		genpc_int(kconvert,m)
	esac
end

!proc do_case (unit p,pindex,pwhenthen,int res) =
proc checkelems(int n,length, unit p)=
	if n<length then
		gerror("Too few elements")
	elsif n>n then
		gerror("Too many elements")
	fi
end

proc do_switch (unit p,pindex,pwhenthen, int res) =
	int minlab,maxlab,x,y,i,n
	unit w,wt, pelse

	pelse:=pindex.nextunit
!first a first scan over the when expressions; work out range and whether simple or complex
	minlab:=1000000
	maxlab:=-1000000			!highest index seen
!valueset:=[]

	n:=0				!no. different values
	wt:=pwhenthen

	while wt do
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				x:=getconstvalue(w.a)
				y:=getconstvalue(w.b)
dorange::
				for i:=x to y do
					minlab :=min(minlab,i)
					maxlab :=max(maxlab,i)
				od
			when jintconst then
				x:=y:=w.value
				goto dorange
			when jtypeconst then
				x:=y:=w.mode
				goto dorange
			else
				gerror_s("Switch when2: not const",strexpr(w)^.strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	if maxlab-minlab<=maxswitchrange then
		do_simpleswitch(p,pindex,pwhenthen,pelse, minlab,maxlab, res)
		return
	fi

	gerror("COMPLEX SWITCH/NOT COMPLETE")
end

proc do_simpleswitch(unit p,pindex,pwhenthen,pelse, int a,b, res) =
!a..b is the range of values of the switch which have been checked to
!be in range in terms of span. But the actual values can be anything.
!For example, 1000000 to 10000250 is valid. So, an offset needs to be
!used to bring the range down to 0 to 250

	unit w,wt,q
	int loopsw,n,offset,x,y,x0,i,labstmt,elselab
	[1..maxswitchrange+1]ref int labels
	int lab_a,lab_b,lab_c,lab_d

	loopsw:=p.tag=jdoswitch

	n:=b-a+1
	offset:=a-1		!a..b becomes 1..n

	if loopsw then
		lab_a:=definelabel()
		lab_d:=createfwdlabel()
		stacklooplabels(&lab_a,&lab_a,&lab_d)
	else
		lab_d:=createfwdlabel()
	fi
	elselab:=createfwdlabel()

	evalunit(pindex)

	genpc_int2(kswitch,n,a)

	for i:=1 to n do
		genpc_lab(kjumplabel,0)
		labels[i]:=pcllast+1		!for now, store destination code index
	od

	genpc_lab(kjumplabel,0)			!else label
	labels[n+1]:=pcllast+1

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when jmakerange then
				x0:=getconstvalue(w.a)
				y:=getconstvalue(w.b)

			when jintconst then
				x0:=y:=w.value
			when jtypeconst then
				x0:=y:=w.mode
			esac

			for x:=x0 to y do
				i:=x-offset
				if labels[i]^ then			!should have been zero
					cpl x,char(x)
					gerror("Dupl switch value")
				fi
				labels[i]^:=labstmt
			od
			w:=w.nextunit
		od

		evalunit(wt.b, res)

		if not loopsw then
			genjumpl(lab_d)
		else
			genjumpl(lab_a)
		fi
		wt:=wt.nextunit
	od

!	if pelse then
!		if pelse.nextunit=nil then
!			q:=pelse			!q is the only statement of the else
!		else
!			q:=nil
!		fi
!!		if loopsw and q and q.tag=jexit and (q.a=nil or getconstvalue(q.a)=1) then	!one stmt consisting of exit/1
!!			pelse:=nil
!!		else
!!		fi
!	fi

!fill in zero entries with else
	definefwdlabel(elselab)
	if pelse then		!do else part
!CPL "DEFELSE",=ELSELAB,=LOOPSW
		evalunit(pelse,res)
	fi	

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		unstacklooplabels()
	else
!CPL "DEFFWD",=LAB_D
		definefwdlabel(lab_d)
	fi

	for i:=1 to n do
		if labels[i]^=0 then
			labels[i]^:=elselab
		fi
	od
	labels[n+1]^:=elselab
!
!CPL =ELSELAB
!CPL =LAB_D
!CPL =LOOPSW
!
end

proc do_makestructkv(int m,n, []unit &plist)=
GERROR("STRUCTLV NOT READY")
end

proc do_makerecordkv(int m,nkeyvals, []unit &kvlist)=
	unit p
	[maxunits]unit plist
	int nfields, index
	symbol d:=ttnamedef[m], e, f, k

!CPL =D.NAME

	e:=d.deflist
	nfields:=0

	while e,e:=e.nextdef do
		if e.nameid=fieldid and e.atfield=nil then
			++nfields
			plist[nfields]:=nil
		fi

!		PRINTLN E.NAME,NAMENAMES[E.NAMEID],E.FIELDOFFSET,E.INDEX,E.ATFIELD
	od

	for i to nkeyvals do
		k:=kvlist[i].a.def
		p:=kvlist[i].b

		e:=d.deflist
		f:=nil
		while e,e:=e.nextdef do
			if e.nameid=fieldid and e.firstdupl=k then
!	CPL "FOUND:",E.NAME
				f:=e
				exit
			fi
		od

		if not f then
			gerror_s("Can't find field:",k.name)
		fi
		index:=f.index
		if plist[index] then
			gerror_s("Dupl key:",k.name)
		fi
		plist[index]:=p

!		CPL I,"KV:",K.NAME, JTAGNAMES[P.TAG]
	od

	for i to nfields do
		if plist[i] then
			evalunit(plist[i])
		else
			genpc_int(kpushci,0)
		fi
	od

!CPL =STRMODE(M), =NKEYVALS, =TTLENGTH[M]
	genpc_int2(kmakerecord,nfields,m)
end
=== qq_pcllib.m 10/32 ===
import* qq

const pclinitalloc=128
!const pclinitalloc=8192
!const pclinitalloc=65536
!const pclinitalloc=1048576
!const pclinitalloc=1048576*2
!const pclinitalloc=1048576*4

global var ref int pclstart				!point to start of current pcl block
global var ref int pclnext				!point to next available int in pcl block
global var ref int pclend				!point to last allocated int (with enough margin for on extra instr)
global var ref int pcllast				!points to start of last pcl instruction in pcl block
global var int pclalloc					!ints allocated

global var ref int32 pclsrcstart
global var ref int32 pclsrcnext

!global var int pclcurrblockno			!
global var int pclcurrlineno			!current line number
const pclelemsize=int.bytes
const pclsrcelemsize=int32.bytes

!global record blockrec =
!	var unit punit
!	var int labeloffset
!	var ichar name
!	var int16 res
!	var int16 nparams
!	var int16 isproc
!	var int16 spare
!end
!
!const blockinitalloc=8192
!!const blockinitalloc=131072
!global var ref []blockrec blocktable
!var int blockalloc=blockinitalloc
!global var int npendingblocks
!
!!labels are just numbers 1,2,3 which index both of these tables
!!labelblocktable is the pclblock no (as all labels are shared across the program)
!!labeloffsettable is the offset into the pclblock
!

global const labelinitalloc=8192
global var ref[]int labeloffsettable
global var int labelalloc
global var int nextlabelno



!global const labelinitalloc=8192
!!global const labelinitalloc=1048576
!global var ref[]int labeloffsettable
!global var int labelalloc
!global var int nextlabelno
!var ref[0:]int labelmap
!var int currlineno
!symbol currpclproc
!
global var [0..pclnames.upb]byte pclnopnds

!var strbuffer pclv
!global var ref strbuffer pcldest = &pclv

!GLOBAL PROC SHOWBLOCKS=
!CPL "BLOCKS",=NPENDINGBLOCKS
!FOR I TO NPENDINGBLOCKS DO
!	CPL I,BLOCKNAMES[I],BLOCKLABELS[I],BLOCKRES[I],BLOCKUNITS[I]
!OD
!
!END
!
proc $init=
	var int nn

!	println "PCL INIT"
	for i:=1 to klastpcl do
		nn:=0
		for j:=1 to 4 do
			if pclfmt[i,j]=0 then exit fi
			++nn
		od
		pclnopnds[i]:=nn
	od

	pcm_init()
!	resetpcl()

!label/block tables are not needed after the pcl sequence has been
!generated. But they are not freed; they can be reused, with their
!current sizes, for the next module. (Might be inefficient if there is one
!very large module, then mainly small ones.)

	labelalloc:=labelinitalloc
	labeloffsettable:=pcm_alloc(int.bytes*labelalloc)

!	blockalloc:=blockinitalloc
!	blocktable:=pcm_alloc(blockrec.bytes*blockalloc)

end

global proc resetpcl(int sourcesize)=
	int pclsize

	qpos:=0
	nextlabelno:=0
	pclcurrlineno:=0
!	npendingblocks:=0

!pcl dest is reallocated for each module
!Any current pcl data is presumably retained so that it can be run.

!	pclalloc:=pclinitalloc

!	pclsize:=sourcesize/2			!estimated num of pcl bytecode elements
	pclsize:=sourcesize			!estimated num of pcl bytecode elements

	pclalloc:=1024					!min
	while pclalloc<pclsize do
		pclalloc<<:=1
	od

!	pclalloc:=max(1024,sourcesize/2)

	pclstart:=pcm_alloc(pclalloc*pclelemsize)
	pclnext:=pclstart
	pclend:=pclstart+pclalloc-16			!allow margin for 1-2 pcl ops
	pcllast:=nil

	pclsrcstart:=pcm_alloc(pclalloc*pclsrcelemsize)
	pclsrcnext:=pclsrcstart

	genpc(kstartmodule)

end

global proc genpc(int opc)=
!STATIC VAR INT CC=0
!
!++NALLPCL
!

	if opc=0 then
		GERROR("ZERO PCL OP?")
	fi
IF OPC>PCLNAMES.LEN THEN
GERROR("GENPC:BAD OPC")
FI
!CPL "GENPC",OPC,PCLNAMES[OPC]

	if pclnext>=pclend then
		extendpcldata()
!		cpl =pclnext-pclstart, =pclalloc
!		gerror("pcl code overflow")
	fi
!	if qlineno<>pclcurrlineno and qlineno then
!		pclnext++^:=klineno
!		pclnext++^:=qlineno
!		pclcurrlineno:=qlineno
!	fi

!CPL "GENPC",PCLNAMES[OPC],PCLNEXT-PCLSTART

!only do overflow check at start of an instruction
	pclnext^:=opc
	pcllast:=pclnext
!CPL -OPC,=PCLNAMES[OPC],PCLLAST^,=PCLLAST

!	pclsrcnext^:=(pass2flag|qpos|lx.pos)
	pclsrcnext^:=qpos

!IF OPC=KCONSTDEF THEN
!CPL "GENPC/CONSTDEF",=PASS2FLAG,=QPOS,=LX.POS,"//",PCLSRCNEXT^,=PCLNEXT-PCLSTART
!FI
!cpl "---",PCLNEXT-PCLSTART, PCLSRCNEXT-PCLSRCSTART

	++pclnext
	++pclsrcnext

!	linetable^[pcindex]:=mlineno
end

global proc genopnd_int(int64 x)=
!no pcindex overflow check needed, as the genpc() check will be sufficient as
!it would allow for enough operands
	pclnext++^:=x
	++pclsrcnext
end

global proc genopnd_name(ref strec d)=
!	pclnext++^:=d.index
	pclnext++^:=int@(d)
	++pclsrcnext
end

global proc genpc_int(int opc, int64 a)=
	genpc(opc)
	pclnext++^:=a
	++pclsrcnext
end

global proc genpc_int2(int opc, int64 a,b)=
	genpc(opc)
	pclnext++^:=a
	pclnext++^:=b
	pclsrcnext+:=2

end

global proc genpc_int4(int opc, int64 a,b,c,d)=
	genpc(opc)
	pclnext++^:=a
	pclnext++^:=b
	pclnext++^:=c
	pclnext++^:=d
	pclsrcnext+:=4
end

global proc genpc_name(int opc, ref strec d)=
	genpc(opc)
	pclnext++^:=int64@(d)
	++pclsrcnext
!	pclnext++^:=d.index
end

!global proc genpc_str(int opc, ichar s, int length=-1)=
!	genpc(opc)
!	genopnd_str(s,length)
!end
!
!global proc genpc_strz(int opc, ichar s)=
!	genpc(opc)
!	genopnd_strz(s)
!end

!global proc genpc_obj(int opc, object p)=
!	genpc(opc)
!	genopnd_obj(p)
!end
!
global proc genopnd_strz(ichar s)=
!s must be a heap string, be a constant, or otherwise be persistent
	pclnext++^:=int64@(s)
	++pclsrcnext
end

global proc genopnd_str(object s)=
!s must be a heap string, be a constant, or otherwise be persistent
	pclnext++^:=int64@(s)
	++pclsrcnext
end

global proc genopnd_obj(object p)=
	pclnext++^:=int64@(p)
	++pclsrcnext
end

global proc genpc_real(int opc, real x)=
	genpc(opc)
	pclnext++^:=int64@(x)
	++pclsrcnext
end

global proc genpc_lab(int opc, int a)=
	var int lastpc
	genpc(opc)
	genopnd_lab(a)
end

global proc genopnd_lab(int a)=
	var int lastpc

	if a>=0 then				!normal, defined label
		pclnext++^:=a
		++pclsrcnext
		return
	fi

!a<0 means fwd label index
	a:=-a					!make positive
	lastpc:=labeloffsettable^[a]		!will be 0 (if first ref) or pc index of last ref
	labeloffsettable^[a]:=pclnext-pclstart
	pclnext++^:=lastpc
	++pclsrcnext
end

global proc gencomment(ichar s)=
	genpc(kcomment)
	genopnd_strz(pcm_copyheapstring(s))
end

proc convertstring(ichar s, t)=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
var int c

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

global function getdottedname(ref strec p)ichar=
!build full dotted name for st item p
static var [256]char str
var [256]char str2
var ref strec owner

strcpy(&.str,p^.name)
!strcat(&.str,"<DOTTED>")
!owner:=p^.owner
!while owner and owner^.nameid<>programid do
!	strcpy(&.str2,&.str)
!	strcpy(&.str,owner^.name)
!	strcat(&.str,".")
!	strcat(&.str,&.str2)
!	owner:=owner^.owner
!od
return &.str
end

proc extendpcldata=
	var int newpclalloc
	var ref int newpclstart
	var ref int32 newpclsrcstart

	newpclalloc:=pclalloc*2

CPL "EXTENDING PCL TABLE TO",=PCLSTART

	newpclstart:=pcm_alloc(pclelemsize*newpclalloc)
	newpclsrcstart:=pcm_alloc(pclsrcelemsize*newpclalloc)

!CPL =NEWPCLSTART

	memcpy(newpclstart,pclstart, (pclnext-pclstart)*pclelemsize)
	memcpy(newpclsrcstart,pclsrcstart, (pclnext-pclstart)*pclsrcelemsize)

	pclnext:=newpclstart+(pclnext-pclstart)
	pclend:=newpclstart+newpclalloc-10
!	pclend:=newpclstart+newpclalloc-1
	pcllast:=newpclstart+(pcllast-pclstart)
	pclsrcnext:=newpclsrcstart+(pclsrcnext-pclsrcstart)

	pcm_free(pclstart,pclalloc*pclelemsize)
	pcm_free(pclsrcstart,pclalloc*pclsrcelemsize)

	pclstart:=newpclstart
	pclalloc:=newpclalloc
	pclsrcstart:=newpclsrcstart
end

global proc extendlabeltable=
	var int newlabelalloc
	var ref[]int newlabeltable

	newlabelalloc:=labelalloc*2

!CPL "EXTENDING LABEL TABLE TO",NEWLABELALLOC

	newlabeltable:=pcm_alloc(int.bytes*newlabelalloc)

	memcpy(newlabeltable,labeloffsettable, labelalloc*int.bytes)

	pcm_free(labeloffsettable,labelalloc*int.bytes)

	labeloffsettable:=newlabeltable
	labelalloc:=newlabelalloc
end

!global proc extendblocktable=
!	var int newblockalloc
!	var ref[]blockrec newblocktable
!
!	newblockalloc:=blockalloc*2
!
!!CPL "EXTENDING BLOCK TABLE TO",NEWBLOCKALLOC
!
!	newblocktable:=pcm_alloc(blockrec.bytes*newblockalloc)
!
!	memcpy(newblocktable,blocktable, blockalloc*blockrec.bytes)
!
!	pcm_free(blocktable,blockalloc*blockrec.bytes)
!
!	blocktable:=newblocktable
!	blockalloc:=newblockalloc
!end

global proc pushint(int a)=
	genpc_int(kpushci,a)
end

global proc pushword(int a)=
	genpc_int(kpushcw,a)
end

global proc pushreal(real x)=
	genpc_int(kpushcr,int@(x))
end

global proc pushstring(ichar s)=
	genpc(kpushcs)
GERROR("PUSHSTRING")
!	genopnd_str(obj_makestring(s,0))
end

global function definelabel:int=
	if nextlabelno>=labelalloc then extendlabeltable() fi
	++nextlabelno
	labeloffsettable^[nextlabelno]:=pclnext-pclstart
	return pclnext-pclstart
end

global function createfwdlabel:int=
	if nextlabelno>=labelalloc then extendlabeltable() fi
	++nextlabelno
	labeloffsettable^[nextlabelno]:=0
	return -nextlabelno
end

global proc definefwdlabel(int &lab)=
!oldlab should be negative
	var int newlab,index,laboffset,pc,nextpc

	index:=lab
	if index>=0 then gerror("deffwdlabel?") fi
	index:=-index

	laboffset:=pclnext-pclstart

	pc:=labeloffsettable^[index]			!start of fwd ref chain
	while pc do						!pc is next pc-index of last label ref
		nextpc:=(pclstart+pc)^
		(pclstart+pc)^:=laboffset
		pc:=nextpc
	od
	labeloffsettable^[index]:=laboffset

	lab:=laboffset
end

global function isstatic(symbol d)int=
	return d.nameid=staticid
end
=== qq_parse.m 11/32 ===
import* qq

var symbol stmodule

var int inparamlist
var int intabledata
var ichar tabledataname=nil

const maxdollarstack=10
var [maxdollarstack]unit dollarstack		!used for a[$]
var int ndollar=0

macro readunit=readexpression()

const maxdocstring=50
[maxdocstring]ichar docstrings
int ndocstrings
int currdllindex
int inreadprint				!0 to allow k:v key-value pairs

global proc parsemodule(int n)=
	var unit p

	return when moduletable[n].parsed

!CPL "PARSE",N,"SOURCE=",STRLEN(MODULETABLE[N].SOURCE)


	currmodule:=&moduletable[n]
	startlex(currmodule.name, currmodule.source,n)
	stcurrmodule:=currmodule.def

!CPL "PARSE",STCURRMODULE.NAME,N
	lex()
	lex()

!	repeat
!		printsymbol(&lx)
!		lex()
!
!	until lx.symbol=eofsym
!	stop
!
!	lex()
!	lex()

!	repeat
!		lexreadtoken()
!		printsymbol(&nextlx)
!!		lex()
!
!	until nextlx.symbol=eofsym
!	stop

!CPL "P1"
!	resetpcl()
!CPL "P2"
!==================================================
! START HERE
!==================================================
	stcurrproc:=stcurrmodule

	p:=readsunit()

!CPL "PARSE",STCURRMODULE.NAME
!PRINTUNIT(P)
	stcurrmodule.code:=moduletable[n].ast:=p

!	if n=1 and astdev then
!		println @astdev,"MODULE AST:"
!		printunit(p,dev:astdev)
!	fi

	skipsemi()
	case lx.symbol
	when commasym then
		serror("Comma seq not allowed")
	when eofsym then
	else
		PS("EOF")
		serror("Bad symbol at eof")
	esac
end

function readexpression:unit p=
	p:=readterm2()

	if exprendset[lx.symbol] then return p fi

	if lx.symbol in [assignsym, deepcopysym] then
		return readassignment(p)
	else
		return readorterms(p)
	fi
end

function readassignment(unit p)unit=
	int pos,opc

	if exprendset[lx.symbol] then return p fi

	p:=readorterms(p)

	if lx.symbol in [assignsym,deepcopysym] then
		opc:=lx.subcode
		pos:=lx.pos
		lex()
		p:=createunit2(opc,p,readassignment(readterm2()))
		p.pos:=pos
	fi
	return p
end

function readorterms(unit p)unit =
	int pos

	if exprendset[lx.symbol] then return p fi

	p:=readandterms(p)

	while lx.symbol=orlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jorlto,p,readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jorl,p,readandterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readandterms(unit p)unit =
	int pos

!PS("CMP")
	p:=readcmpterms(p)
!	return p when lx.symbol<>andlsym

	while lx.symbol=andlsym do
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jandlto,p,readunit())
			p.pos:=pos
			exit
		fi

		p:=createunit2(jandl,p,readcmpterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readcmpterms(unit p)unit =
	int pos,n
	unit px,q
	[4]byte genops

	p:=readinterms(p)

	if not cmpopset[lx.symbol] then
		return p
	fi

	clear genops
	px:=p
	p:=createunit1(jcmpchain,p)
	n:=0				!n counts operand after the first

	while cmpopset[lx.symbol] do
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode
	
		pos:=lx.pos
		lex()

		q:=readinterms(readterm2())
		px.nextunit:=q
		px:=q

		q.pos:=pos
	od
!
	if n=1 then
		p.tag:=genops[1]
		q:=p.a
		p.b:=q.nextunit
		q.nextunit:=nil
!		p.hasb:=1
	else
!SERROR("CMP/CHAIN")
!	FOR I

		p.cmpgenop:=genops
	fi	

	return p
end

function readinterms(unit p)unit =
	int pos,opc

	p:=readrangeterm(p)

	doswitch lx.symbol
	when insym, notinsym then
		opc:=lx.subcode
!CPL "CMP",GENOPNAMES[OPC]

		pos:=lx.pos
		lex()

		p:=createunit2(opc,p,readrangeterm(readterm2()))
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readrangeterm(unit p)unit =
	int pos

	p:=readaddterms(p)

	if lx.symbol=rangesym then
		pos:=lx.pos
		lex()
		p:=createunit2(jmakerange,p,readaddterms(readterm2()))
		p.pos:=pos
	fi

	return p
end

function readaddterms(unit p)unit =
	int pos,opc,a,b
	unit q

	p:=readmulterms(p)
	while addopset[lx.symbol] do
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jtocodes[opc],p,readassignment(readterm2()))
			p.pos:=pos
			exit
		fi

		q:=readmulterms(readterm2())
		p:=createunit2(opc,p,q)
		p.pos:=pos
	od

	return p
end

function readmulterms(unit p)unit =
	int pos,opc,a,b
	unit q

	p:=readpowerterms(p)

	while mulopset[lx.symbol] do
		opc:=lx.subcode
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(jtocodes[opc],p,readassignment(readterm2()))
			p.pos:=pos
			exit
		fi

		p:=createunit2(opc,p,readpowerterms(readterm2()))
		p.pos:=pos
	od

	return p
end

function readpowerterms(unit p)unit =
	int pos

!	p:=readterm2()
	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(jpower,p,readpowerterms(readterm2()))
		p.pos:=pos
	od

	return p
end

!function readtermx(unit p, int pos)unit=
!	int pos
!
!	p:=readtermsuffix(p,pos)
!	return p
!end

function readterm2:unit p=
	int pos

	pos:=lx.pos
	p:=readterm()
	p:=readtermsuffix(p,pos)
	return p
end

function readtermsuffix(unit p, int pos)unit=
	var unit q,r
	var ref char pbyte
	var word64 a
	var int opc,oldipl,shift,t,nparams


	doswitch lx.symbol
	when lbracksym then
		lex()
		q:=readslist(1,1,nparams)
		checksymbol(rbracksym)
		lex()
		p:=createunit2(jcall,p,q)
		p:=readcondsuffix(p)

	when ptrsym then
		p:=createunit1(jptr,p)
		lex()

	when lsqsym then
		p:=readindex(p,0)

	when dotsym then
		p:=readdotsuffix(p)

	when lcurlysym then
		p:=readkeyindex(p)

	when colonsym then
		if not inreadprint then
			lex()
			q:=readunit()

			p:=createunit2((inparamlist|jkeyword|jkeyvalue),p,q)
		else
			exit
		fi

	when incrsym then
		case lx.subcode
		when jincrload then opc:=jloadincr
		when jdecrload then opc:=jloaddecr
		esac
		lex()
		p:=createunit1(opc,p)

!	when atsym then
!		SERROR("@??")
	else
		exit
	enddoswitch

	p.pos:=pos

	return p
end

function readterm:unit=
var unit p,q,r
var ref char pbyte
var word64 a
var int oldipl,opc,oldinrp,pos,shift,t,nparams,length

var [20]char str
int128 sab@&str
int64 sa@&str

!var word64 sa@str
!var word64 sb@str+8

!	lineno:=lx.pos
	pos:=lx.pos

!PS("RT1")
	switch lx.symbol
	when namesym then
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

	when intconstsym then
!CPL "INT",STDTYPENAMES[LX.SUBCODE]
		case lx.subcode
		when tint then p:=createintunit(lx.value)
		when tword then p:=createwordunit(lx.value)
!		when tint128 then p:=createint128unit(lx.pvalue128^)
!		when tword128 then p:=createword128unit(lx.pvalue128^)
!		when tnil then p:=createunit0(jnil)
		else
			serror("Unknown intconst type")
		esac
		lex()

	when realconstsym then
		p:=createrealunit(lx.xvalue)
		lex()

	when stringconstsym then
		p:=createstringunit(lx.svalue)
		lex()

	when decimalconstsym then
		p:=createstringunit(lx.svalue)
		p.tag:=jdecimal
		lex()
!		(lx.svalue+lx.length)^:=0
!		p:=createunit0(jdecimal)
!		p.svalue:=lx.svalue
!		p.slength:=lx.length
!		p.mode:=tflexdecimal
!		lex()

	when charconstsym then
		length:=strlen(lx.svalue)
		if length>16 then
			serror("char const too long")
		fi
		sab:=0
		memcpy(&.str,lx.svalue,length)
		if length<=8 then
			p:=createintunit(sa)
		else
			p:=createint128unit(sab)
		fi
		lex()

	when lbracksym then
		p:=readlbrack()

	when stdtypesym then
		if lx.subcode=tvoid then
			lex()
			if lx.symbol=dotsym and nextlx.symbol=ktypesym then
				lex()
				lex()
				p:=createunit0(jtypeconst)
				p.mode:=tvoid
			else
				p:=createunit0(jnull)
			fi
		else
			p:=readcast()
		fi

	when packtypesym then
		p:=createunit0(jtypeconst)
		p.mode:=lx.subcode
		lex()

!	when ellipsissym then
!		lex()
!		p:=createunit0(jpacktypeconst)
!		p.value:=tp_variadic

	when addsym then
		lex()
		p:=readterm2()

	when subsym  then
		lex()
		p:=readterm2()
		if p.tag=jintconst then
			p.value:=-p.value
		else
			p:=createunit1(jneg, p)
		fi

	when notlsym, istruelsym, inotsym, abssym, sqrsym, signsym,ascsym, chrsym,
			incrsym, decrsym, mathssym  then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc, readterm2())

	when lsqsym then
		p:=readset()

	when minsym, maxsym then
		opc:=lx.subcode
		lex()
		checksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		p:=createunit2(opc,p, readunit())
		checksymbol(rbracksym)
		lex()

	when ksprintsym then
		p:=readsprint()

	when ksreadsym,ksreadlnsym then
		p:=readsread()

	when addrsym,ptrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(opc,readterm2())
		if p.a.tag=jcall then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
		fi

	when daddrsym then
		lex()
		p:=createunit1(jdaddrof,readterm2())

	when knewsym then
		lex()
		checksymbol(lbracksym)
		lex()
		q:=readslist(0,0,nparams)
		checksymbol(rbracksym)
		lex()
		p:=createunit1(jnew,q)
		p.nparams:=nparams

!	when anddotsym then
!		lex()
!		p:=createunit1(jaddroffirst,readterm2())
!
!	when compilervarsym then
!		p:=readcompilervar()
!
!	when kerrorsym then
!		p:= createconstunit(lx.subcode,tint)
!		lex()

	when dollarsym then
		if intabledata then
			p:=createstringunit(tabledataname,-1)
		else
			if ndollar<=0 then
				serror("[$] No array")
			fi
			p:=createunit1(jupb,dollarstack[ndollar])
		fi
		lex()

	when dotsym,kglobalsym then
		lex()
		checksymbol(namesym)
		p:=createname(lx.symptr)
		p.pos:=lx.pos
		lex()

!	when kmapsym then
!		p:=readmap()

!	when kapplyopsym then
!		p:=readapplyop(1)
!
!	when kcastsym then
!		p:=readcastx()

!	when kptypesym then
!		lex()
!		checksymbol(lbracksym)
!		lex()
!!		p:=createunit1(jmakeptype,readtypespec())
!!		p:=readtypespec()
!		p:=readtypespec(1)
!!CPL "HERE",P
!		skipsemi()
!		checksymbol(rbracksym)
!		lex()
!
!	when ktypeconstsym then
!		lex()
!		checksymbol(lbracksym)
!		lex()
!		p:=createunit0(jtypeconst)
!
!		storemode(3,stcurrproc,readtypespec(stcurrproc),cast(&p.value))
!		checksymbol(rbracksym)
!		lex()
!
	when kclampsym then
		lex()
		checksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		if lx.symbol=rbracksym and q.tag=jmakerange then
			r:=q.b
			q:=q.a
		else
			checksymbol(commasym)
			lex()
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(jmax,p,q)
		p:=createunit2(jmin,q,r)

	when kgotosym then
		p:=readgoto()

	when kifsym then
		p:=readif()

	when kunlesssym then
		p:=readunless()

	when kcasesym,kdocasesym,kswitchsym,kdoswitchsym then
		p:=readswitchcase()

!	when krecasesym then
!		p:=readrecase()
!
	when kforsym then
		p:=readfor()

!	when kforallsym then
!		p:=readforall()

	when ktosym then
		p:=readto()

	when kdosym,kdooncesym then
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
!
	when kraisesym then	!todo
		lex()
		p:=createunit1(jraise,readunit())

	when kswapsym then			!swap using function syntax
		lex()
		checksymbol(lbracksym)
		lex()
		p:=readunit()
		checksymbol(commasym)
		lex()
		q:=readunit()
		checksymbol(rbracksym)
		lex()
		p:=createunit2(jswap,p,q)

	when khostfnsym then
		p:=readhostparams(nil,1)

!	when khostsym then
!!SERROR("HOST")
!		lex()
!		checksymbol(dotsym)
!		lex()
!		checksymbol(namesym)
!		p:=createunit0(jhost_name)
!		p.def:=lx.symptr
!		lex()

!	when kevalsym then
!		lex()
!		p:=createunit1(jeval,readunit())
!
!	when lcurlysym then
!		lex()
!		p:=readsunit()
!		checksymbol(rcurlysym)
!		lex()
!		p:=createunit1(jlambda,p)

	when knilsym then
		p:=createunit0((lx.subcode=1|jpnil|jnil))
		lex()

	else

error::
		cpl symbolnames[lx.symbol]
		serror("readterm?")
	endswitch

	p.pos:=pos
	return p
end

function readxunit:unit=
	return readsunit()
end

function readsunit(int inwhile=0)unit=
var int lineno,m,globalflag,staticflag
var unit ulist,ulistx,p,q,r
var symbol stname

lineno:=lx.pos
ulist:=ulistx:=nil
globalflag:=staticflag:=0

repeat
	while lx.symbol=semisym do
		lex()
	od
!PS("RS2")
!CHECKULIST("RS1",ULIST)

	switch lx.symbol
	when kstaticsym then
		lex()
		staticflag:=1
		checksymbol(kvarsym)
		redo

	when kglobalsym then
!CPL "RS/GLOBAL"
		if globalflag then serror("global global?") fi
		globalflag:=1
		lex()
		redo

	when kprocsym,kfunctionsym then
		readprocdef(globalflag)
		globalflag:=0

	when kvarsym,kletsym then
		readvardef(lx.symbol=kletsym, globalflag,staticflag)
		globalflag:=staticflag:=0

	when kconstsym then
		if staticflag then serror("static?") fi
		readconstdef(globalflag)
		globalflag:=0

	when ktypesym then
		readtypedef(globalflag)
		globalflag:=0

	when kclasssym,krecordsym then
!CHECKULUST("REC1",ULIST)
		readrecorddef(globalflag)
		globalflag:=0

	when ktabledatasym then
!SERROR("TABLEDATA")
		readtabledef(globalflag)
		globalflag:=0

	when docstringsym then
		if ndocstrings>=maxdocstring then
			serror("Too many docstrings for fn")
		fi
		docstrings[++ndocstrings]:=pcm_copyheapstringn(lx.svalue, lxlength)
		lex()

	when kenumsym then		!enum
		readenumtype(globalflag)

	when kimportsym then
		lex()
!SERROR("IMPORT")
		checksymbol(namesym)

		m:=checkmodule(lx.symptr.name)
		if not m then		!new module
!CPL "ADD IMPORT MODULE"
			if loadmodule(lx.symptr)=0 then
				serror_s("Can't find import module:",lx.symptr.name)
			fi
		else
			currmodule.importmap[m]:=1
		fi
		lex()

!		p:=createunit1(jimport,createname(lx.symptr))
!!		p.def:=lx.symptr
!		addlistunit(ulist,ulistx,p)
!!CHECKULIST("AFTER KIMPORT",ULIST)
!		lex()
!
	when kimportdllsym then
		readimportdll()

	when kmacrosym then
		readmacrodef(globalflag)
		globalflag:=0

	when eofsym then
		exit
!		cpl stcurrproc.name
!		serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
	when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,sendtosym,
			kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
		exit

	when namesym then
		if nextlx.symbol in [dcolonsym,colonsym] then
!		stname:=createlabel(lx.symptr,1)
!CPL "CREATE LABEL"
!			p:=createunit1(jlabeldef,createname(lx.symptr))
			p:=createunit1(jlabeldef,createname(addsymbol(stcurrproc, lx.symptr, labelid, 0)))
			lex()
			lx.symbol:=semisym
			addlistunit(ulist,ulistx,p)

		else
			goto doexec
		fi
	when kdosym then				!u;u;u;do rather than u;u;u do
		if inwhile then
			exit
		fi
		goto doexec

	when semisym then
	else							!assume a statement
doexec::
		p:=readunit()
!PRINTUNIT(P)
		if p.tag=jname and lx.symbol=namesym then
			serror("Possibly var/let needed")
		fi
		addlistunit(ulist,ulistx,p)
		if lx.symbol=kdosym then
			exit
		fi

	endswitch

!	if p then
!		genpcl(p,0)
!	fi


until lx.symbol<>semisym
!PS("RSX")

case lx.symbol
when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,kdosym,sendtosym,
	kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym,commasym,
	barsym,eofsym then
else
	serror("Readsunit: "";"" expected, or bad unit starter")
esac

!CPL "END SUNIT"

if ulist=nil or ulist.nextunit then			!empty or multiple => block
	return createunit1(jblock,ulist)
else
	return ulist							!single => one unit
fi
end

proc checksymbol(int symbol)=
	[100]char str

	if lx.symbol<>symbol then
		fprint @&.str,"# expected, not #",symbolnames[symbol],symbolnames[lx.symbol]
		serror(&.str)
	fi
end

proc checkequals=
!check that "=" is current symbol
	if lx.symbol<>eqsym then
		serror("""="" expected")
	fi
end

function checkbegin(int fbrack)int=
!look for ( or [ or begin, return close symbol expected
!positioned at this opening symbol
!fbrack=1 to allow left "("
	var int closesym

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

proc checkbeginend(int closesym,kwd,startline=0)=
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

proc skipsemi=
	while lx.symbol=semisym do lex() od
end

function readindex(unit p,int dot)unit=
!at '['; dot=0/1 for a[]/a.[]
!syntax is::
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	var unit q,plower,pupper

	lex()

	do
		if ndollar>=maxdollarstack then
			serror("Too many nested a[$]")
		fi
		dollarstack[++ndollar]:=p
		q:=readunit()
		--ndollar

!	if q.tag=jmakerange then		!convert into a discrete slice
!		p:=createunit2((dot|jdotslice|jslice),p,q)
!	else
			p:=createunit2((dot|jdotindex|jindex),p,q)
!	fi
		exit when lx.symbol<>commasym
		lex()
	od
	checksymbol(rsqsym)
	lex()
	return p
end

function readdotsuffix(unit p)unit=
!at '.' symbol
!read any modifiers for term currently in p
!multiple .terms can be present
	var unit q
	var int t

	while lx.symbol=dotsym do
		lex()
		switch lx.symbol
		when lsqsym then
			p:=readindex(p,1)
		when namesym then
			p:=createunit2(jdot,p,createname(lx.symptr))
			lex()
		when propsym then			!ought to check whether op is allowed in this form
			p:=createunit1(lx.subcode,p)
			lex()
		when ktypesym then
			p:=createunit1(jtype,p)
			lex()

		when dollarsym then
			if p.tag not in [jname,jdot] then
				serror("...name.$ needed")
			fi
			p:=createunit1(jsymbol,p)
			lex()

!	when ktypesym then			!.type, convert to .gettype
!		case p.tag
!		when ktypeconst then			!int.type=>int
!
!		else
!			p:=createunit1(jtypeof,p)
!		esac
!		lex()

		else
			serror("Unknown dot suffix")
		endswitch
	od
	return p
end

function readslist(int iscall=0,donulls, &nparams)unit=
!read comma-separated list of expressions
!positioned at first symbol of first expression
! it might be | or )
!
!donulls=1 means empty expressions are allowed (just comma or terminator, which
!result in null units
!return with symbol at terminating symbol: 1st non comma and is that a unit starter
!iscall=1 when called to read a function-call parameter list; then key:value pairs
!are treated as keyword arguments
!eg: (a,b,c	)
!eg: (a		!
	var unit ulist,ulistx
	var int oldinparamlist

	ulist:=ulistx:=nil
	nparams:=0

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
				addlistunit(ulist,ulistx,createunit0(jnull))
			else
				serror("null comma expr not allowed")
			fi
			lex()
		when rbracksym then
			if donulls then
				addlistunit(ulist,ulistx,createunit0(jnull))
			fi
			exit
		else
			addlistunit(ulist,ulistx,readunit())
			++nparams
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

function readcondsuffix(unit p)unit=
!p is a unit just read
!positioned at following symbol
!check whether a conditional suffix follows, and return p wrapped in a conditional if so
! ... if cond
! ... when cond
! ... unless cond

!case lx.symbol
	switch lx.symbol
	when kwhensym then
		lex()
		return createunit2(jif,readunit(),createunit1(jblock,p))
	when kunlesssym then
		lex()
		return createunit2(jif, createunit1(jnotl,readunit()),createunit1(jblock,p))
	else
		return p
	endswitch
end

function readkeyindex(unit p)unit=
!at '{'
!syntax is::
![x] or [x,...]			!single or multiple indexing (can also use [x][x].. for multiple)
!I don't need to allow indexing and section select within the same [...]
!exit with symbol just after closing ]
	var unit q

	lex()

	q:=readunit()

	if lx.symbol=commasym then
		lex()
		q.nextunit:=readunit()
	fi
	
	p:=createunit2(jkeyindex,p,q)

	checksymbol(rcurlysym)
	lex()
	return p
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

! (s||s|s)	!list comp [SYNTAX TO BE REVISED]
!return positioned at symbol following closing ")"
	var unit ulist,ulistx, p,q,r
	var int oldirp,length,lower,lowerseen,elemtype

	lex()					!first symbol of first expression
	ulist:=ulistx:=nil
	length:=0
	lower:=1
	lowerseen:=0

	elemtype:=tvoid

!PS("LB")

	if lx.symbol=packtypesym and nextlx.symbol=colonsym then
		elemtype:=lx.subcode
		lex()
		lex()
	fi

	if lx.symbol=intconstsym and nextlx.symbol=colonsym then
		lower:=lx.value
		lowerseen:=1
		lex()
		lex()
	fi

!check symbol after "("

	case lx.symbol
	when rbracksym then			!empty list
		lex()
		p:=createunit0(jmakelist)
		p.length:=0
		p.lower:=lower
		p.elemtype:=elemtype
		return p
	else					!assume normal expression follows
!PS("LB1")
!	p:=readunit()
		p:=readxunit()
!PS("LB2")
	esac

!check symbol after "(expr"
	case lx.symbol
	when rbracksym then			!simple (x) expression
		lex()
		if lowerseen then
!CPL "0: ON SIMPLE EXPR"
			p:=createunit2(jkeyvalue,createintunit(lower), p)
		fi

		return p

	when commasym then
		length:=1
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(jmakelist,p)
			p.length:=length
			p.lower:=lower
			p.elemtype:=elemtype
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
!		addlistunit(&ulist,&ulistx,readunit())
			addlistunit(ulist,ulistx,readxunit())
			++length
			skipsemi()						!allow a,b,c;) (works better with a,b,c\ followed by comment on next line followed by ")")
		until lx.symbol<>commasym
		checksymbol(rbracksym)
		lex()
		p:=createunit1(jmakelist,ulist)
		p.length:=length
		p.lower:=lower
		p.elemtype:=elemtype
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
			q.nextunit:=r
			return createunit2(jif,p,q)
		when rbracksym then
			lex()
			return createunit2(jif,p,q)

		esac

!assume selectx expression
		addlistunit(ulist,ulistx,q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
			repeat
				lex()				!skip comma
				addlistunit(ulist,ulistx,readxunit())
!			addlistunit(&ulist,&ulistx,readunit())
			until lx.symbol<>commasym
			checksymbol(barsym)
		else
			lex()					!skip |
		fi
		lex()
		r:=readxunit()
		checksymbol(rbracksym)
		lex()
		p.nextunit:=r
		return createunit2(jselect,p,ulist)

	else
		serror("(x ...")
	esac
	return nil
end

function readif:unit=
!at 'if'
	var int line, kwd, lineno
	var unit pthen,pcond, plist,plistx, pelse, p, pelsif

	line:=lx.pos

	kwd:=lx.symbol			!in case coming from elsecase etc

	lex()
	pcond:=readsunit()
	skipsemi()

	checksymbol(kthensym)
	lex()

	pthen:=readsunit()

	case lx.symbol
	when kelsifsym then
		lx.symbol:=kifsym		!for .kwd
		pelse:=readif()

	when kelsesym then		!get r=any else stmt or nil
		lex()
		pelse:=readsunit()
		checkend(kendsym,kwd)
		lex()
	when kelsecasesym,kelseswitchsym then
		lx.symbol:=kwd
	SERROR("ELSECASE NOT READY")
!	pelse:=makeblock(readswitchcase())
	else
		PELSE:=NIL
		checkend(kendsym,kwd)
		lex()
	esac

	pthen.nextunit:=pelse
	p:=createunit2(jif,pcond,pthen)
	p.pos:=line

	return p
end

!proc checknotempty(unit p)=
!	if p=nil or p.tag=jblock and p.length=0 then
!		serror("Empty block not allowed")
!	fi
!end

proc checkend(int endsym,endkwd1, endkwd2=0,startline=0)=
!at terminator symbol such as ), eof or 'end'
!check it matches what is expected
!endsym is symbol expected to match
!'end' can have optional keyword following; if present, it must match endkeyword
!Some loop ends (indicated by endkeyword=kforsym, etc) can be also be terminated with 'od'
!endsym should be lbracksym or kendsym
var [100]char str

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
!		serror("'end' by itself no longer valid")
		return
	fi

	unless (endkwd1 and endkwd1=lx.subcode) or (endkwd2 and endkwd2=lx.subcode) then
		strcpy(&.str,"Mismatched 'end'")
		goto error
	end unless
end

function readunless:unit=
	var int line
	var unit pcond, pthen, pelse, p
	line:=lx.pos
	lex()
	pcond:=readsunit()
	checksymbol(kthensym)
	lex()

	pthen:=readsunit()

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	else			!assume simple if-then
		pelse:=nil
	fi
	checkend(kendsym,kunlesssym)
	lex()
	pthen.nextunit:=pelse
	p:=createunit2(jif,createunit1(jnotl,pcond),pthen)
	p.pos:=line
	return p
end

function readwhile:unit=
	var int pos,id
	var unit pcond, pa, pb, pc, pbody, p

	pos:=lx.pos
	lex()

	pcond:=readsunit(1)

	checksymbol(kdosym)
	lex()
	pbody:=readsunit()

	checkend(kendsym,kwhilesym,kdosym)
	lex()

	p:=createunit2(jwhile,pcond,pbody)
	p.pos:=pos
	return p
end

function readrepeat:unit=
	var int pos
	var unit pbody, pcond, p

	pos:=lx.pos
	lex()
	pbody:=readsunit()
	checksymbol(kuntilsym)
	lex()
	pcond:=readunit()
	p:=createunit2(jrepeat,pbody,pcond)
	p.pos:=pos
	return p
end

function readfor:unit=
!on 'for'; syntax is::
! for term [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for term in/inrev expr [when expr] do stmts [else stmts] end/od

	var int line, opc, down, isforeach
	var unit pstep, pvar, pcond, pfrom, pto, pelse, pbody, p, plist,pvar2

	line:=lx.pos
	isforeach:=lx.subcode
	lex()			!skip 'for'
	pvar:=readterm2()

	if pvar.tag<>jname then
		serror("For: name expected")
	fi

	opc:=jforup
	pstep:=nil
	pcond:=nil
	pvar2:=nil

	if lx.symbol=commasym then			!double index
		lex()
		pvar2:=readterm2()
	fi

	if lx.symbol in [insym,inrevsym] then	!assume in/inrev
		down:=lx.symbol=inrevsym
		lex()
		plist:=readunit()

		case plist.tag
		when jmakerange then		!in a..b: simple iteration
			opc:=jforup
			pfrom:=plist.a
			pto:=plist.b
		when jbounds then			!
			plist.tag:=jboundsx
			opc:=jforupx
		else
			opc:=jforall
		esac
	else
		if lx.symbol=assignsym then
			lex()
			pfrom:=readunit()
		else
			pfrom:=createintunit(1)
		fi
		checksymbol(ktosym)
		down:=lx.subcode=1
		opc:=jforup
		lex()
		pto:=readunit()

		if lx.symbol=kbysym then
			lex()
			pstep:=readunit()
			if pstep.tag<>jintconst then serror("BY needs int constant") fi
			if pstep.value<0 then 
				serror("Step must be positive")
			elsif pstep.value=0 then
				serror("Zero step")
			fi
			pstep.value:=abs pstep.value
			if pstep.value=1 then		!by 1
				pstep:=nil
			fi
		fi
	fi

!	if pstep=nil then
!		pstep:=createintunit(1)
!	fi

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
		pbody.nextunit:=pelse
	else
		pelse:=nil
	fi
	checkend(kendsym,kforsym,kdosym)
	lex()

!CPL "FOR1",PCOND,=DOWN
	if pcond<>nil then
		pbody:=makeblock(createunit2(jif,pcond,pbody))
	fi

!CPL =PCLNAMES[OPC]
	case opc
	when jforall then
		pvar.nextunit:=plist
		plist.nextunit:=pvar2
		p:=createunit2((down|jforallrev|jforall),pvar,pbody)

	when jforupx then
!SERROR("FORUPX")
		pvar.nextunit:=plist
		p:=createunit2((down|jfordownx|jforupx),pvar,pbody)
	else
		pvar.nextunit:=pfrom
		pfrom.nextunit:=pto
		pto.nextunit:=pstep
		p:=createunit2((down|jfordown|jforup),pvar,pbody)
	esac

	if isforeach then
		if p.tag=jforall then
			p.tag:=jforeach
		else
			serror("Foreach?")
		fi
	fi

	p.pos:=line

	if pvar2 and opc not in [jforall, jforallrev] then
		serror("for i,j not allowed")
	fi

	return p
end

function readdo:unit=
	var unit p
	var int line,opc

	line:=lx.pos
	opc:=(lx.symbol=kdosym|jdo|jdoonce)
	lex()
	p:=readsunit()
	checkend(kendsym,kdosym)
	lex()
	p:=createunit1(opc,p)
	p.pos:=line
	return p
end

function readto:unit=
	var int line,id
	var unit p, pcount, pbody

	line:=lx.pos
	lex()

	pcount:=readunit()

	checksymbol(kdosym)
	lex()
	pbody:=readsunit()
	checkend(kendsym,ktosym,kdosym)
	lex()

	pcount.nextunit:=createavname()

	p:=createunit2(jto,pcount,pbody)
	p.pos:=line
	return p
end

function makeblock(unit p)unit=
	return createunit1(jblock,p)
end

proc readvardef(int islet, isglobal=0, isstatic=0)=
!positioned at 'var' 'let'
	var int nvars,varid
	symbol d

	lex()

	if stcurrproc.nameid=procid then
		varid:=(isstatic|staticid|frameid)
	else
		varid:=staticid
	fi

	nvars:=0
	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, varid, isglobal)
		lex()

		case lx.symbol
		when assignsym then
			if varid=staticid then serror("Need '=' for static") fi
			lex()
			d.code:=readunit()

		when eqsym then
			if varid<>staticid then serror("Need ':=' for non-static") fi
			lex()

			d.code:=readunit()
	    else
			if islet then
				serror("let needs :=")
			fi
		esac

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No vars declared")
	fi
end

proc readconstdef(int isglobal=0)=
!positioned at 'const'
	var int nvars
	symbol d

	lex()

	nvars:=0
	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, constid, isglobal)
		lex()

		checksymbol(eqsym)
		lex()

		d.code:=readunit()

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No consts declared")
	fi
end

function readreturn:unit=
	var unit p,q,r

	lex()
	if exprstarterset[lx.symbol] then
		q:=readunit()
		p:=createunit1(jreturn,q)
	else
		p:=createunit0(jreturn)
	fi

	return readcondsuffix(p)
end

function readprint:unit=
	var int opc, isfprint, fshowname, length
	var unit pformat, pdev, printlist,printlistx, p,q
	var ref strbuffer expr

	var ichar s

	opc:=lx.subcode

!CPL =SYMBOLNAMES[LX.SYMBOL]
!CPL =LX.SUBCODE,=JPRINT

	case opc
!when jfprint,jfprintln,jcprint,jcprintln then
	when jfprint,jfprintln then
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
		if not exprstarterset[lx.symbol] and opc=jcprintln then
			goto finish
		fi
		pformat:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

!if lx.symbol=semisym then
	if not exprstarterset[lx.symbol] then
		goto finish
	fi

	do
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(printlist,printlistx, createunit0(jnogap))
		when dollarsym then
			addlistunit(printlist,printlistx, createunit0(jspace))
			lex()
		else

			fshowname:=0
			if lx.symbol=eqsym then
				fshowname:=1
				lex()
			fi

			p:=readunit()

			if p.tag=jkeyvalue then
				p.tag:=jfmtitem
			fi


!			if lx.symbol=colonsym then
!				lex()
!				p:=createunit2(jfmtitem,p,readunit())
!			fi
			if fshowname then
!SERROR("CAN'T DO STREXPR/PRINT=")
				expr:=strexpr(p)
				strbuffer_add(expr,"=")
				s:=expr.strptr

				iconvucn(expr.strptr,expr.length)

				addlistunit(printlist,printlistx,q:=createstringunit(s,expr.length))
!			addlistunit(printlist,printlistx,createstringunit("<LABEL>",-1))
			fi
			addlistunit(printlist,printlistx,p)
		esac
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish::
	if opc=jprint and printlist=nil then
		serror("No print items")
	fi
	if opc=jfprint and printlist=nil and pformat=nil then
		serror("No print items")
	fi
	if opc=jcprint and printlist=nil and pformat=nil then
		serror("No cprint items")
	fi

	if isfprint then
		if pformat=nil and opc<>jcprintln then
			serror("No fmt str")
		fi
		if pformat=nil then
			pformat:=makeblock(pformat)
		fi
		pformat.nextunit:=printlist
		return createunit2(opc,pdev,pformat)
		return pformat
	else
!CPL =OPC,PCLNAMES[OPC]

		return createunit2(opc,pdev,printlist)
	fi
end

function readread:unit=
	var int opc
	var unit pformat, pdev, readlist, readlistx, p

	opc:=lx.subcode
	lex()

	readlist:=readlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		if opc=jread then
			serror("@ on read")
		fi
		lex()
		pdev:=readunit()
		if lx.symbol=commasym then lex() else goto finish fi
	fi

	if not exprstarterset[lx.symbol] then
		goto finish
	fi

	do
		p:=readunit()
		if p.tag=jkeyvalue then
			p.tag:=jfmtitem
		fi
		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	finish::
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit2(opc,pdev,readlist)
end

function readloopcontrol:unit=
	var int opc
	var unit p

	opc:=lx.subcode
	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name,"all") then
		lex()
		p:=createunit1(opc,createintunit(0))

	elsif exprstarterset[lx.symbol] then
		p:=createunit1(opc,readintunit())
	else
		p:=createunit1(opc,createintunit(1))
	fi
	return readcondsuffix(p)
end

function readintunit:unit p=
	p:=readunit()
	if p.tag<>jintconst then
		serror("int expr needed")
	fi
	return p
end

function readconstint:int=
!read expression that must yield a constant int value *now*; return value
	unit p

	p:=readunit()
	case p.tag
	when jintconst then
		return p.value
	when jrealconst then
		return p.xvalue
	else
		serror("Can't do complex expr")
	esac
	return 0
end

proc readenumtype(int isglobal=0)=
!positioned at 'enum'
!syntax is enum(a,b[=expr],c)
	var int index
	var symbol d

	lex()
	checksymbol(lbracksym)
	lex()
	if lx.symbol=rbracksym then serror("Empty enum list") fi

	index:=1

	while lx.symbol=namesym do
		d:=addsymbol(stcurrproc, lx.symptr, enumid, isglobal)
		lex()

		case lx.symbol
		when eqsym then
			lex()
			index:=readconstint()
		esac

		d.index:=index

		++index
!
		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od
	checksymbol(rbracksym)
	lex()
end

function readswitchcase:unit=
	var int pos, kwd, opc, lineno,rangeused, nwhen
	var unit pexpr,pwhenlist,pwhenlistx,pwhen,pwhenx,pelse,p,pthen,pwhenthen,q

	pos:=lx.pos
	kwd:=lx.symbol			!remember kcasesym etc
	opc:=lx.subcode			!pick up tag: kcase etc

!CPL =OPC

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
		pos:=lx.pos
		lex()
		pwhen:=pwhenx:=nil
		do
			p:=readunit()
			++nwhen
			p.pos:=pos
			if p.tag=jmakerange then rangeused:=1 fi
			addlistunit(pwhen,pwhenx,p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		if lx.symbol<>kthensym then checksymbol(sendtosym) fi
		lex()
		pthen:=readsunit()
!	pwhenthen:=createunit2(kwhenthen,q:=createunit1(jblock,pwhen),pthen)
		pwhenthen:=createunit2(jwhenthen,pwhen,pthen)
		pwhenthen.pos:=pos
!	q.pos:=lineno
		addlistunit(pwhenlist,pwhenlistx,pwhenthen)
	od


!if opc in [kswitch,kdoswitch] and not rangeused and nwhen<=6 then
!	opc:=(opc=kswitch|kcase|kdocase)
!fi

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
!!		pelse:=makeblock(readswitchcase())
		pelse:=readswitchcase()
!		pelse:=nil
	else
		PELSE:=NIL
!	pelse:=makeblock(nil)
		checkend(kendsym,kwd)
		lex()
	esac

	pexpr.nextunit:=pelse
	p:=createunit2(opc,pexpr,pwhenlist)
	p.pos:=pos
	return p
end

function readgoto:unit=

	if lx.subcode=1 then		!go used
		lex()
		checksymbol(ktosym)
	fi
	lex()

	return readcondsuffix(createunit1(jgoto,readunit()))
end

function readstop:unit=
	var unit p
	var int i
	lex()
	if exprstarterset[lx.symbol] then
		p:=createunit1(jstop,readunit())
	else
		p:=createunit1(jstop,createintunit(0))
	fi
	return readcondsuffix(p)
end

function readcast:unit p=
!just seem basic type name
	var int t,opc

	t:=lx.subcode
	lex()

	if t=trange and lx.symbol=lbracksym then
		lex()
		p:=readunit()
		if p.tag in [jkeyvalue,jkeyword] then
			p.tag:=jmakerangelen
		elsif p.tag=jmakerange then
		else
			serror("need a..b or a:n")
		fi
		checksymbol(rbracksym)
		lex()
		return p
	fi


!check for standalone value
	case lx.symbol
	when atsym,lbracksym then

	else						!convert to typeconst
		p:=createunit0(jtypeconst)
		p.mode:=t
		return p
	esac

	if lx.symbol=atsym then
		lex()
		opc:=jtypepun
	else
		opc:=jconvert
	fi
	checksymbol(lbracksym)
	p:=readunit()

	p:=createunit1(opc,p)
	storemode(stcurrproc,t,&p.mode)
!	p.mode:=t
	return p
end

function readset:unit=
!positioned at "["
	var int length,nkeyvalues,oldinparamlist
	var unit p,ulist,ulistx

	lex()					!first symbol of first expression

	case lx.symbol
	when rsqsym then		!empty set, same as 0
		lex()
		return createunit1(jmakeset,nil)
	when colonsym then
		lex()
		checksymbol(rsqsym)
		lex()
		return createunit1(jmakedict,nil)
	esac

	oldinparamlist:=inparamlist
	inparamlist:=0

	p:=readunit()
	length:=1
	nkeyvalues:=0
	if p.tag=jkeyvalue then ++nkeyvalues fi

	ulist:=ulistx:=p

	while lx.symbol=commasym do
		lex()
		if lx.symbol=rsqsym then exit fi		!allow trailing comma
		addlistunit(ulist,ulistx,p:=readunit())
		if p.tag=jkeyvalue then ++nkeyvalues fi

		++length
		skipsemi()						!allow a,b,c;]
	od

	checksymbol(rsqsym)
	inparamlist:=oldinparamlist

	lex()
	if nkeyvalues then
		if length>nkeyvalues then serror("dict: mixed elements") fi
		p:=createunit1(jmakedict,ulist)
	else
		p:=createunit1(jmakeset,ulist)
	fi
	p.length:=length
	return p
end

global proc readtabledef(int isglobal=0)=
!at 'tabledata' symbol
	var int i,ncols,nrows,enums,nextenumvalue,firstval,lastval,startline,closesym
	var int ltype,lower
	var unit ulist,ulistx, plower, p
	const maxcols=20
	var [maxcols]symbol varnames
	var [maxcols]unit plist,plistx
	var symbol d

	const maxrows=500
	var [maxrows]int enumvalues
!int nenums

	lex()
	enums:=0						!whether there is an enums column

	if lx.symbol=lbracksym then		!tabledata(...) read enum type
		enums:=1
		lex()						!don't support named enum type
		checksymbol(rbracksym)
		lex()
	fi

	nextenumvalue:=1
	nrows:=0			!number of data rows appearing
	ncols:=0			!number of data columns (varnames appearing)

!loop reading variable names
	while lx.symbol=namesym do
!		checksymbol(namesym)
		if ++ncols>maxcols then
			serror("tabledata/too many columns")
		fi
		varnames[ncols]:=lx.symptr

		lex()
		if lx.symbol=commasym then
			lex()
		else
			exit
		fi
	od

	checkequals()
	lex()					!skip =

	skipsemi()
	startline:=lx.pos
	closesym:=checkbegin(0)

	skipsemi()
	firstval:=lastval:=0

	for i:=1 to ncols do
		plist[i]:=plistx[i]:=nil
	od
	ulist:=ulistx:=nil

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

			d:=addsymbol(stcurrproc,lx.symptr,enumid, isglobal)
			lex()

			case lx.symbol
			when eqsym then
				lex()
				p:=readunit()
				if p.tag=jintconst then
					nextenumvalue:=p.value
				else
					SERROR("TABLEDATA: COMPLEX ENUM VAL")
				fi
			esac

			d.index:=nextenumvalue
			enumvalues[nrows]:=nextenumvalue
			++nextenumvalue

			tabledataname:=d.name

			if nrows=1 then firstval:=nextenumvalue fi
			lastval:=nextenumvalue

			if ncols then				!comma always expected
				checksymbol(commasym)		!check it
			fi
			lex()
		fi

		for i:=1 to ncols do
			addlistunit(plist[i],plistx[i],readunit())
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

!RETURN ULIST
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

!!for each variable, add a vardef initialised to the list
!!add the decls for the vars
!
	for i:=1 to ncols do
!
		d:=addsymbol(stcurrproc,varnames[i],staticid,isglobal)
!		if enums then
!			plower:=createintunit(enumvalues[1])
!		else
!			plower:=nil
!		fi

		p:=d.code:=createunit1(jmakelist,plist[i])
		p.length:=nrows
		p.lower:=(enums|enumvalues[1]|0)
	od
end

function readtry:unit=
	var unit ptry, pexceptlist, pexceptlistx, px, q, exlist,exlistx
!++try_level
	lex()

	ptry:=readsunit()
	pexceptlist:=pexceptlistx:=nil			!list of kexcept items

	while lx.symbol=kexceptsym do
		lex()
		exlist:=exlistx:=nil				!list of exception codes for this 'except'
		do
			addlistunit(exlist,exlistx,readunit())
			if lx.symbol<>commasym then exit fi
			lex()
		od
		checksymbol(kthensym)
		lex()
		px:=readsunit()
		addlistunit(pexceptlist,pexceptlistx,createunit2(jexcept,exlist,px))
	od
	checkend(kendsym,ktrysym)
	lex()

!--try_level

	return createunit2(jtry,ptry,pexceptlist)
end

function readsprint:unit=
	var int opc,isfprint
	var unit pformat, pdev, printlist, printlistx, p

	opc:=lx.subcode
	lex()
	checksymbol(lbracksym)
	lex()

	case opc
	when jsfprint,jcprint then
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
			addlistunit(printlist,printlistx,createunit0(jnogap))
		else
			p:=readunit()
			if p.tag=jkeyvalue then
				p.tag:=jfmtitem
			fi
			addlistunit(printlist,printlistx,p)
		fi
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

	finish::
	lex()
	if (opc=jprint or opc=jfprint) and printlist=nil then
		serror("No print items")
	fi

	if isfprint then
		if pformat=nil then
			serror("No fmt str")
		fi
		pformat.nextunit:=printlist
		return createunit2(opc,pdev,pformat)
	else
		return createunit2(opc,pdev,printlist)
	fi
end

function readsread:unit=
!NEED TO CHECK WHAT SREAD/SREADLN actually mean. I think they are actually supposed
!to work an item at a time::
! a:=sread([fmt])
! b:=sreadln([dev])	returns entire input line, but keeps line for subsequent sread/read
	var int opc
	var unit pformat,pdev,p, readlist,readlistx

	opc:=lx.subcode
	lex()
	checksymbol(lbracksym)
	lex()

	readlist:=readlistx:=nil
	pformat:=pdev:=nil

	if lx.symbol=atsym then
		if opc=jread then
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
		if p.tag=jkeyvalue then
			p.tag:=jfmtitem
		fi
		addlistunit(readlist,readlistx,p)
		if lx.symbol<>commasym then exit fi
		lex()
	od

	checksymbol(rbracksym)

	finish::
	lex()
	if opc=jread and readlist=nil then
		serror("No read items")
	fi

	return createunit2(opc,pdev,readlist)
end

function readtypespec(int full=0, symbol owner=nil)int=
!full=1 to allow structdefs

	int flags, arraycode, oldipl
	int a,b,t, startline, closesym, caligned
	symbol d
	const maxdim=10
	[maxdim]unit lowerdims,lengthdims
	int ndims
	unit x,lowerx, upperx, lengthx

	case lx.symbol
	when lsqsym then
		lex()
		ndims:=0
		inreadprint:=1
		do
			lowerx:=lengthx:=nil
			if lx.symbol=rsqsym or lx.symbol=commasym then		![]
			else
				x:=readunit()
				if x.tag=jmakerange then			![a..b] variable
					lowerx:=x.a
					upperx:=x.b
					if lowerx.tag=jintconst and upperx.tag=jintconst then
						lengthx:=createintunit(upperx.value-lowerx.value+1)
					else
						lengthx:=createunit2(jsub,upperx,lowerx)
						lengthx:=createunit2(jadd,lengthx,createintunit(1))
					fi
				else
					case lx.symbol
					when rsqsym,commasym then			![n]
						lengthx:=x
					when colonsym then				!a:n
						lowerx:=x
						lex()
						if not (lx.symbol=commasym or lx.symbol=rsqsym) then
							lengthx:=readunit()
						fi
					esac
				fi
			fi
			lowerdims[++ndims]:=lowerx
			lengthdims[ndims]:=lengthx
			exit when lx.symbol<>commasym
			lex()
		od
		checksymbol(rsqsym)
		lex()
		inreadprint:=0
		t:=readtypespec()

		for i:=ndims downto 1 do
			t:=makeaxtype(t,lowerdims[i],lengthdims[i])
		od
		return t

	when krefsym then
		lex()
		if lx.symbol=stdtypesym and lx.subcode=tvoid then
			lex()
			return makereftype(tvoid)
		else
			return makereftype(readtypespec())
		fi

	when namesym then
		d:=lx.symptr
		lex()
		if lx.symbol=dotsym then
			lex()
			checksymbol(namesym)
			t:=newusertypex(d,lx.symptr)
			lex()
			return t
		else
			return newusertypex(d)
!			T:=newusertypex(d)
!CPL "RTS",=T,STRMODE(T),=TLAST
!			return T
		fi

	when packtypesym then
		case lx.subcode
		when tpstringc, tpstringz then
			t:=lx.subcode
			lex()
			checksymbol(mulsym)
			lex()
!			return makestrtype(t,readconstint())
			return makestrtype(t,readunit())
		else
			t:=lx.subcode
			lex()
			return t
		esac

	when krecordsym then
		if owner=nil then serror("anon record") fi
		lex()
		closesym:=checkbegin(1)
		startline:=lx.pos
		t:=readrecordbody(owner)

		checkbeginend(closesym,krecordsym,startline)
		return t

	when kstructsym then
		if owner=nil then serror("anon struct") fi
		lex()
		caligned:=0
		if lx.symbol=kcalignedsym then
			caligned:=1
			lex()
		fi

		closesym:=checkbegin(1)
		startline:=lx.pos
		t:=readstructbody(owner,caligned)

		checkbeginend(closesym,kstructsym,startline)
		return t

!	else
!PS("PACK")
		if lx.symbol=stdtypesym and lx.subcode=tstring then
			lex()
			return tpcstring
		else
			serror("Pack type?")
		fi

	esac

	return tvoid
end

proc readtypedef(int isglobal)=
!at 'type'
	int ptype
	symbol d

	lex()
	checksymbol(namesym)
	d:=addsymbol(stcurrproc, lx.symptr, typeid, isglobal)

!D.MODE:=TPI32
	lex()
	checksymbol(eqsym)
	lex()	

!	lex()

	ptype:=readtypespec(1,d)

!CPL "TYPEDEF",D.MODE

	createusertype(d, ptype)

!CPL "TYPEDEF2",D.MODE

!CPL "READTYPEDEF PTYPE=",TTNAME[PTYPE],=STRMODE(PTYPE)

!
!	p:=createunit2(jtypedef,createname(d),ptype)
!	p.mglobal:=isglobal
!
!	return p
end

proc readimportdll=
!at 'importdll'
	[256]char str
	symbol stproc,d, stname
	int closesym, startpos, isfunc, isnew

	lex()
	checksymbol(namesym)
	stname:=lx.symptr

	lex()
	checksymbol(eqsym)
	lex()

!check for an existing dll with the same name, as this could be adding to it

	isnew:=1
	d:=stname.nextdupl
	while d do
		if d.nameid=dllmoduleid then
			stname:=d
			isnew:=0
			exit
		fi
		d:=d.nextdupl
	od

	if isnew then			!new
		stname:=addsymbol(stprogram,stname,dllmoduleid,0)
!		if eqstring(stname.name,"sys") then
!			stsysmodule:=stname
!		fi
		if ndlllibs>=maxdlllib then
			serror("Too many DLL libs")
		fi
!		dllnames[++ndlllibs]:=stname.name
		dlltable[++ndlllibs]:=stname
		stname.index:=ndlllibs
	fi

	currdllindex:=stname.index

	closesym:=checkbegin(1)
	startpos:=lx.pos
!------------------------------------
	do
!CPL "LOOP"
		skipsemi()

		case lx.symbol
		when kfflangsym then
			lex()
		when kprocsym,kfunctionsym then
			isfunc:=lx.symbol=kfunctionsym
			lex()
			case lx.symbol
			when namesym then
				stproc:=addsymbol(stcurrproc, lx.symptr, dllprocid, 1)
			when stringconstsym then
!CPL "READIMP/STR",LX.SVALUE
				strcpy(str,lx.svalue)
				convlcstring(str)
				stproc:=addsymbol(stcurrproc, addglobalname(str), dllprocid, 1)
				stproc.truename:=pcm_copyheapstring(lx.svalue)
			else
				serror("fn name expected")
			esac

			stproc.misfunc:=isfunc

			if ndllprocs>=maxdllproc then
				serror("Too many DLL procs")
			fi
			dllproctable[++ndllprocs]:=stproc
			dlllibindex[ndllprocs]:=currdllindex
			stproc.index:=ndllprocs

			lex()

!			pas:=nil
			if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
				lex()
				checksymbol(namesym)
!				addalias(stproc,lx.symptr)

				d:=addsymbol(stproc.owner, lx.symptr, aliasid, 1)
				d.alias:=stproc
				lex()
			fi
			readffiparams(stproc)
		else
			exit
		esac
	od	
!--------------------------------
!	checkend(closesym, kimportdllsym)
	checkbeginend(closesym, kimportdllsym, startpos)
!PS("DLL1")
!	lex()
!PS("DLL2")
end

proc readffiparams(symbol stproc)=
!at first symbol after func name
!return list of units with dllparam defs (can be empty)
!if there is a result type, then head of list will be a return def type
	int pret,ptype

	if lx.symbol=lbracksym then
		lex()
		if lx.symbol=rbracksym then
			lex()
		else
			ptype:=readtypespec()
			if lx.symbol in [commasym,rbracksym] then		!types only
				readtypeparams(stproc,ptype)
			else
				readtypenameparams(stproc,ptype)
			fi
		fi
	fi

	if lx.symbol in [colonsym,sendtosym] then
		if not stproc.misfunc then serror("Return type for proc?") fi
		lex()
	fi

	pret:=tvoid
	if stproc.misfunc then
		if lx.symbol=semisym then serror("Return type missing") fi
		pret:=readtypespec()
	fi

	storemode(stproc.owner,pret, &stproc.mode)
end

proc readtypeparams(symbol stproc, int ptype)=
!at symbol after ptype
	[32]char str
	int nparams
	symbol stname

	nparams:=0

	do
		++nparams
		print @str,"$",,nparams

		stname:=addsymbol(stproc, addglobalname(str), dllparamid, 0)
		storemode(stproc,ptype,&stname.mode)
		++stproc.nparams

		if lx.symbol=commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi
			ptype:=readtypespec()
		else
			exit
		fi
	od
	checksymbol(rbracksym)
	lex()
end

proc readtypenameparams(symbol stproc, int ptype)=
!at symbol after ptype
	symbol stname

	checksymbol(namesym)
	stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
	storemode(stproc,ptype,&stname.mode)
	++stproc.nparams
	lex()

	do

		if lx.symbol=eqsym then
			lex()
			stname.code:=readunit()
			stname.moptional:=1
		fi

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=ellipsissym then
				stproc.mvarparams:=1
				lex()
				exit
			fi

			if istypestarter() then			!new type
				ptype:=readtypespec()
			fi
			checksymbol(namesym)
			stname:=addsymbol(stproc, lx.symptr, dllparamid,0)
			storemode(stproc,ptype,&stname.mode)
			++stproc.nparams
			lex()
		else
			exit
		esac
	od
	checksymbol(rbracksym)
	lex()
end

global proc readrecorddef(int isglobal)=
!at 'record' symbol
	int kwd, baseclass, m, startline, closesym, normalexit
	symbol nameptr, d, newd, e

	kwd:=lx.symbol

!CPL "RECORD RECORD DEF-------------------------"

	lex()
	checksymbol(namesym)
	nameptr:=lx.symptr

	lex()
	baseclass:=0
	if lx.symbol=lbracksym then
!SERROR("BASE CLASS")
		lex()
		baseclass:=readtypespec()
		checksymbol(rbracksym)
		lex()
	fi

	checkequals()
	lex()

	d:=addsymbol(stcurrproc, nameptr, recordid, isglobal)

!	if baseclass then
!		if baseclass>0 then serror("baseclass?") fi
!		if nbaseclasses>=255 then
!				serror("Too many base classes")
!		fi
!		++nbaseclasses
!		storemode(owner,baseclass,&baseclasstable[nbaseclasses])
!		d.attribs.ax_baseclass:=nbaseclasses
!		baseclassdef[nbaseclasses]:=d
!	fi

	closesym:=checkbegin(1)

	startline:=lx.pos

	m:=readrecordbody(d)

	checkbeginend(closesym,kwd,startline)
end

function readrecordbody(symbol owner)int=
!at first symbol of a class or record body (after 'type T=record',
! or after 'record T ='
!read fields, constants, types, methods.
!create initially anonymous record type, and return type code
!caller will attached to named type as needed.

!int kwd
	symbol oldstcurrproc, e
	int m

	m:=addanontype()

	oldstcurrproc:=stcurrproc
	stcurrproc:=owner

	doswitch lx.symbol
	when kconstsym then
		readconstdef(0)
	when kvarsym then
		readrecordfields(owner)
	when kfunctionsym,kprocsym then
		readprocdef(0)

	when krecordsym,kclasssym then
		lex()
		serror("CLASS RECORD")
	when ktypesym then
		lex()
		serror("CLASS TYPE")
	when kendsym,rbracksym,rcurlysym then
		exit
	when eofsym then
		serror("Class eof?")
		exit
	when semisym then
		lex()
	else
		case lx.symbol
		when stdtypesym, packtypesym, lsqsym, krefsym then
			serror("Packed types not allowed in record")
		else
			serror("Unknown record field decl")
		esac
	enddoswitch

	ttfields[m]:=owner.deflist
	ttlength[m]:=owner.nfields
	ttlower[m]:=1
	ttbasetype[m]:=trecord

	createusertype(owner, m)

	e:=owner.deflist
	while e do
		addgenfield(e)
		e:=e.nextdef
	od

!	e:=owner.deflist
!	while e do
!CPL OWNER.NAME,"ADDGENFIELD:",E
!!		addgenfield(e)
!		e:=e.nextdef
!	od

	ttsize[m]:=varsize*owner.nfields

	stcurrproc:=oldstcurrproc

	return m
end

proc readrecordfields(symbol owner)=
!positioned at 'var'; read one line of var defs for a record
	var int nvars,offset,index
	symbol d

!CPL "RRF",OWNER.NAME, OWNER.INDEX,OWNER.NFIELDS

	lex()

	nvars:=0
	index:=owner.nfields
	offset:=index*varsize

	while lx.symbol=namesym do
		++nvars

		d:=addsymbol(stcurrproc, lx.symptr, fieldid, 0)
		d.atfield:=nil

		lex()

		if lx.symbol=atsym then
			lex()
			d.atfield:=readatfield()
!			readatfield()
			d.fieldoffset:=d.atfield.fieldoffset
			d.index:=d.atfield.index
		else
			d.fieldoffset:=offset
			offset+:=varsize
			d.index:=++index
		fi

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od


	if nvars=0 then
		serror("No fields")
	fi
	stcurrproc.nfields+:=nvars
end

function readstructbody(symbol owner, int caligned)int=
	int m, ngroups, nvars, t
	symbol d, e

	m:=addanontype()

	ngroups:=0

	do
		skipsemi()

!CPL =NGROUPS

		case lx.symbol
		when kstructsym then
			++ngroups
			lex()
			addstructflag(owner,structblockid)
!			addlistunit(ulist,ulistx,createunit0(jstructblock))

		when kunionsym then
			++ngroups
			lex()
			addstructflag(owner,unionblockid)
!			addlistunit(ulist,ulistx,createunit0(junionblock))

		when kendsym then
!CPL "END",=NGROUPS
			if ngroups then
				--ngroups
				lex()
				addstructflag(owner,endblockid)
			else
				exit
			fi

		when rbracksym then
			exit

		else
!PS("RSB1")
			t:=readtypespec(0,nil)
!CPL "RBS2",=T,=TTNAME[T],=STRMODE(T)

			nvars:=0
			while lx.symbol=namesym do
				++nvars
				d:=addsymbol(owner, lx.symptr,structfieldid, 0)
				storemode(owner,t,&d.mode)
!				storemode(
!				d.mode:=t
!CPL "STORED D",D,D.NAME, D.MODE
				lex()

				if lx.symbol<>commasym then
					exit
				fi
				lex()
				checksymbol(namesym)
			od
			if nvars=0 then serror("struct decl?") fi
!			owner.nfields:=nvars
		esac
	od

	ttfields[m]:=owner.deflist
	ttlength[m]:=owner.nfields
	ttlower[m]:=1
	ttcaligned[m]:=caligned
	ttbasetype[m]:=tstruct

	createusertype(owner, m)

	e:=owner.deflist
	while e do
		case e.nameid
		when structblockid, unionblockid, endblockid then
		else
			addgenfield(e)
		esac
		e:=e.nextdef
	od

!	ttsize[m]:=varsize*owner.nfields
	return m
end

proc addstructflag(symbol owner, int id)=
	static int structseqno
	[32]char str

	fprint @str,"$$#",++structseqno

	addsymbol(owner, addglobalname(str),id, 0)
end

proc readprocdef(int isglobal)=
!at 'proc' etc symbol; read proc def or declaration
	var int kwd,startline,closesym,nparams
	var unit pcode
	symbol d, oldstcurrproc
	[256]char str

	kwd:=lx.symbol
	lex()
	checksymbol(namesym)
!
	if stcurrproc.nameid=procid then
		serror("Nested proc")
	fi

	oldstcurrproc:=stcurrproc			!usually module, but could be a record
	stcurrproc:=d:=addsymbol(stcurrproc,lx.symptr,procid,isglobal)
	addproc(d)

	lex()

!skip <"abc"> meta data (probably won't be used here)
	if lx.symbol=ltsym then
		lex()
		checksymbol(stringconstsym)
		lex()
		case lx.symbol
		when gtsym then
			lex()
		when gesym then
			lx.symbol:=eqsym
		else
			serror(""">"" expected")
		esac
	fi

	readparams(d)

	checkequals()
	lex()

	startline:=lx.pos

	closesym:=checkbegin(0)

	d.code:=readsunit()

!CPL =D.NAME,D.CODE

	if eqstring(d.name,"start") then
		currmodule.startfn:=d
	elsif eqstring(d.name,"main") then
		currmodule.mainfn:=d
	fi

	checkbeginend(closesym,kwd,startline)

	stcurrproc.misfunc:=kwd=kfunctionsym

	if ndocstrings then
!		println "Docs for:",stcurrproc.name
!		for i to ndocstrings do
!			println "	",,docstrings[i]
!		od
		ndocstrings:=0
	fi

	stcurrproc:=oldstcurrproc
end

proc readparams(symbol procowner)=
	var int isbyref,isoptional
	var symbol d

	if lx.symbol<>lbracksym then
		return
	fi

	lex()
	case lx.symbol
	when rbracksym then
		lex()
		return
	when ellipsissym then
		lex()
		checksymbol(rbracksym)
		lex()
		procowner.mvarparams:=1
		return
	esac

!assume one or more params
	isbyref:=isoptional:=0

	do
		if lx.symbol=addrsym then
			++isbyref
			lex()
		fi
		if lx.symbol=questionsym then
			++isoptional
			lex()
		fi
		checksymbol(namesym)
		d:=addsymbol(procowner, lx.symptr, paramid,0)
		++procowner.nparams

		lex()

		if lx.symbol=eqsym then
			isoptional:=1
!			if isbyref+isoptional then serror("Mixed/dupl &/?/=") fi
			lex()
			d.code:=readunit()
		fi

		if isbyref and isoptional then serror("Mixed byref/optional") fi

		d.mbyref:=isbyref
		d.moptional:=isoptional
!CPL =D, =ISBYREF, =ISOPTIONAL

		isbyref:=isoptional:=0

		if lx.symbol=commasym then
			lex()
			if lx.symbol=ellipsissym then
				procowner.mvarparams:=1
				lex()
				exit
			fi
		else
			exit
		fi
	od

	checksymbol(rbracksym)
	lex()

!move params to paramlist (assume no other defs)
!	procowner.paramlist:=procowner.deflist
!	procowner.deflist:=procowner.deflistx:=nil

end

function readatfield:symbol=
!reading a class or struct body, where owner is that class/struct entry
!positioned at symbol following '@', should be name of an existing field
	symbol p,d

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=stcurrproc.deflist
	while p do
		if eqstring(p.name,d.name) then
			return p
		fi

		p:=p.nextdef
	od
	serror_s("Can't find @ field",d.name)
	return nil
end

function istypestarter:int=
	case lx.symbol
	when krefsym, packtypesym then
		return 1
	elsif lx.symbol=namesym then
		if nextlx.symbol=namesym then
			return 1
		fi
	esac
	return 0
end

proc readmacrodef(int isglobal)=
!positioned at 'macro'
!read expression macro-definition; global=1 if to be exported

	symbol stmacro, stname, owner

	lex()
	checksymbol(namesym)

	stmacro:=addsymbol(stcurrproc, lx.symptr, macroid, isglobal)
	owner:=stmacro

	lex()

	if lx.symbol=lbracksym then			!may have parameters
		lex()
		if lx.symbol<>rbracksym then
			do
				case lx.symbol
				when namesym then
					stname:=addsymbol(owner,lx.symptr,macroparamid,0)
					stname.firstdupl:=lx.symptr

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
		lex()
	fi

	checkequals()
	lex()
	stmacro.code:=readunit()
end

function readhostparams(unit lhs,int isfn)unit=
!hostfn name has been read
!lhs is not null when lhs.hostfn(...) has been used
!currently at hostfn symbol
	int fnindex,oldinrp, nargs
	unit p,q

	fnindex:=lx.subcode
	lex()
	checksymbol(lbracksym)
	lex()
	oldinrp:=inreadprint
	inreadprint:=0
	q:=readslist(1,1,nargs)

	checksymbol(rbracksym)
	lex()
	inreadprint:=oldinrp

	if lhs then
		lhs^.nextunit:=q
		q:=lhs
	fi

!	p:=createunit1((isfn|jcallhostfn|jcallhostproc),q)
	p:=createunit1(jcallhost,q)
	p^.index:=fnindex

	return p
end

=== qq_resolve.m 12/32 ===
import* qq

int allowmodname=0
int nprocs

int noexpand
int symbolmode
int macrolevels

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

const maxstructfields=100
[maxstructfields]symbol structfields
int ntopfields, nallfields

global proc rx_module(int n)=
!	CPL "RESOLVEMODULE",MODULETABLE[N].NAME

	currmodule:=&moduletable[n]
	stcurrproc:=stcurrmodule:=currmodule.def
	nprocs:=0

!move this to end of proc to allow module vars generated by assignment
!to be visible inside procs
	rx_passdef(stprogram, stcurrmodule)

!CPL "AFTER MODULE",STCURRMODULE.CODE, CURRMODULE.AST
	if nprocs=0 then
!		rx_unit(stprogram,currmodule.ast)
		rx_unit(stcurrmodule,currmodule.ast)
	elsif currmodule.ast.a then				!module block not empty
		RX_UNIT(STCURRMODULE,CURRMODULE.AST)
!		rxerror("Module code not allowed when procs are present",currmodule.ast)
	fi

!	rx_passdef(stprogram, stcurrmodule)
end

global proc rx_passdef(symbol owner,p)=
	symbol d

!CPL "PASSDEF",P.NAME
	case p.nameid
	when moduleid,dllmoduleid then
		rx_deflist(p,p.deflist)

	when procid then
		++nprocs
		fixmode(owner,p)
		rx_deflist(p,p.deflist)
		stcurrproc:=p
		rx_unit(p,p.code)
		stcurrproc:=stcurrmodule

	when dllprocid then
		fixmode(owner,p)
		rx_deflist(p,p.deflist)

	when constid,staticid,frameid,paramid then
		fixmode(owner,p)
!CPL "CONST1",P.NAME; PRINTUNIT(P.CODE)
!CPL "CONSTID1"; PRINTUNIT(P.CODE)
		if p.code then
			rx_unit(owner,p.code)

!CPL "CONSTID2"; PRINTUNIT(P.CODE)

		fi
	when typeid then
		fixmode(owner,p)
		rx_deflist(p,p.deflist)

!	else
	esac
end

global proc rx_deflist(symbol owner,p)=
	while p do
		rx_passdef(owner,p)
		p:=p.nextdef
	od
end

global proc rx_unit(symbol owner, unit p)=
	symbol d
	unit a,b
	int n, flags, oldnoexpand,oldsymbolmode, nk

!CPL "RXUNIT",JTAGNAMES[P.TAG]

	a:=p.a
	b:=p.b
	qpos:=p.pos

	switch p.tag
	when jname then
		resolvename(owner,p)
!CPL "NAME",P,JTAGNAMES[P.TAG],P.DEF
		if p.tag=jname and p.def.nameid=macroid and not noexpand then
!CPL "NAME EXPAND"
			++macrolevels
			expandmacro(p,p,nil)
			rx_unit(owner,p)
			--macrolevels
		fi

	when jkeyword then
		rx_unit(owner,b)		!do param value only

	when jdot then
		resolvedot(owner,p)

	when jcall then
!CPL "RX/JCALL",JTAGNAMES[A.TAG]
		if a.tag=jname then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=jtypeconst then
!			if b=nil then
!				rxerror("Empty constr; use new()")
!			fi
			p.tag:=jconvert
			p.a:=b
			p.b:=nil
			p.mode:=a.mode

!			if b=nil then
!				p
!
!			if b.nextunit or b.tag=jkeyword then
				nk:=0
				p.a:=createunit1(jmakelist,b)
				n:=0
				while b do
					if b.tag=jkeyword then
						++nk
						b.tag:=jkeyvalue
!						rx_unit(owner,b.a)
					fi
					++n
					b:=b.nextunit
				od
				if nk and nk<>n then
					rxerror("Mixed key:value")
				fi

				p.a.length:=n
!			fi
		elsif a.tag=jname and a.def.nameid=macroid then
			++macrolevels
			expandmacro(p,a,b)
			rx_unit(owner,p)
			--macrolevels
		fi

!	when jgoto then
!		if a.tag<>jname then rxerror("Not simple label") fi
!		resolvename(owner,a)
!!		if a.def.nameid<>labelid then
!CPL NAMENAMES[A.DEF.NAMEID]
!			rxerror_s("Not a label or not found:",a.def.name)
!		fi

	when jadd, jsub, jmul, jdiv, jidiv, jirem, jiand, jior, jixor,
		jshl, jshr, jmakerange then
!		rx_unitlist(owner,a)
		rx_unit(owner,a)
		if not b then rxerror("Binop missing opnd") fi
		rx_unit(owner,b)
		evalbinop(p,a,b)

	when jneg, jabs,jlen, jbytesize,jsqrt then
		rx_unit(owner,a)
		evalmonop(p)

!when jmakelist then
!when jindex,jdotindex then
!when jupper then
!when jhardconv then

	when jforup,jfordown then			!a will be jname unit
		resolvename(owner,a,tint)
		a:=a.nextunit
		goto doabc

	when jconvert then
!CPL "RX/CONVERT"
		rx_unit(owner,a)

!		if a.tag=jconst then
		evalmonop(p)
!		fi
!	GOTO DOABC

	when jsymbol then
		oldnoexpand:=noexpand
		oldsymbolmode:=symbolmode
		noexpand:=1
		symbolmode:=1
		rx_unit(owner,a)
		noexpand:=oldnoexpand
		symbolmode:=oldsymbolmode

		case a.tag
		when jname then
		when jtypeconst then
			d:=ttnamedef[a.mode]
!CPL "JSYM/TYPE",D,A.DEF

			if d then
				a.def:=d
				a.tag:=jname
			else
				rxerror("T.$?")
			fi

		else
printunit(a)
			rxerror(".$ not name")
		esac

	else
doabc::

		flags:=jflags[p.tag]
		if flags>=1 then rx_unitlist(owner,a) fi
		if flags=2 then rx_unitlist(owner,b) fi
	endswitch
end

proc rx_unitlist(symbol owner, unit p)=
	while p do
		rx_unit(owner,p)
		p:=p.nextunit
	od
end

proc evalmonop(unit p)=
	int a,c
	real x,z

	case p.a.tag
	when jintconst then
		a:=p.a.value

		switch p.tag
		when jneg then c:=-a
		when jabs then c:=abs(a)
		else
			return
		endswitch

		makeintconst(p,c)

	when jrealconst then
		x:=p.a.xvalue

		switch p.tag
		when jneg then z:=-x
		when jabs then z:=abs(x)
		else
			return
		endswitch

		makerealconst(p,z)
	else
		return 
	esac
end

proc evalbinop(unit p,lhs,rhs)=
	int a,b,c
	real x,y,z

	case pr(lhs.tag,rhs.tag)
	when pr(jintconst, jintconst) then
		a:=lhs.value
		b:=rhs.value

		switch p.tag
		when jadd then c:=a+b
		when jsub then c:=a-b
		when jmul then c:=a*b
		when jidiv then c:=a/b
		else
			return
		endswitch

		makeintconst(p,c)

	when pr(jrealconst, jrealconst) then
		x:=lhs.xvalue
		y:=rhs.xvalue

		switch p.tag
		when jadd then z:=x+y
		when jsub then z:=x-y
		when jmul then z:=x*y
		when jdiv then z:=x/y
		else
			return
		endswitch

		makerealconst(p,z)
	else
		return 
	esac
end

proc makeintconst(ref unitrec p,int64 value)=
!convert unit p, currently binop or monop, to a const
	p.tag:=jintconst
	p.a:=p.b:=nil
	p.value:=value
	p.mode:=tint
end

proc makerealconst(ref unitrec p,real64 xvalue)=
!convert unit p, currently binop or monop, to a const
	p.tag:=jrealconst
	p.a:=p.b:=nil
	p.xvalue:=xvalue
	p.mode:=treal
end

global proc resolvename(symbol owner, unit p, int mode=tvoid)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	symbol d,e
	unit q
	int moduleno

	d:=p.def
	moduleno:=p.moduleno

!CPL "RESOLVENAME",D.NAME,NAMENAMES[D.NAMEID],=MODULENO

!	if d.nameid not in [genericid,genfieldid] then			!assume already resolved
	if d.nameid<>genericid then			!assume already resolved
!	if d.nameid<>nullid and d.name<>genfieldid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)

!CPL =E,namenames[OWNER.NAMEID],=OWNER.NAME

	if not e then

		case owner.nameid
		when procid then	!add as framevar
!CPL "ASRX1"
			e:=p.def:=addsymbol(owner,p.def,frameid,0)
			e.mode:=tpvar
!CPL "NEW E:",E.NAME, NAMENAMES[E.NAMEID],=E,=D,=P.DEF
		when moduleid then
!RXERROR("ADD UNDEF NAME AS MODULE STATIC")
!CPL "NOT FOUND IN MODULE",D.NAME
!CPL "ASRX2"
			e:=p.def:=addsymbol(owner,p.def,staticid,0)
			e.mode:=tpvar

		else
!			cpl d.name,p.lineno,jtagnames[p.tag]
			rxerror_s("Undefined: #",d.name,p)
		esac
	else
retry::
!CPL "E FOUND",E.NAME,NAMENAMES[E.NAMEID]
		p.def:=e			!update link in kcode

		case e.nameid
		when constid then		!convert namedconst to const
			q:=e.code			!q is knamedconst unit; q.c is value
!CPL "CONSTQ"; PRINTUNIT(Q)
			rx_unit(owner,q)
			if q.tag not in [jintconst, jrealconst, jstringconst] then
!PRINTUNIT(Q)
				rxerror_s("Not const expr: #",jtagnames[q.tag])
			fi

			e.mode:=q.mode
			p.tag:=q.tag
			p.value:=q.value
			p.mode:=q.mode
			p.slength:=q.slength
		when enumid then
			p.tag:=jintconst
			p.value:=e.index
			p.mode:=tint
!			rxerror("FOUND ENUMID",p)
!			rxerror("FOUND ENUMID")
		when staticid then		!deal with python global accesses ?? WTF ???
		when typeid,recordid then
			p.tag:=jtypeconst
			p.mode:=p.def.mode

!		when aliasid then		!replace by what it is shadowing
!				!may never get here, if the substitution is done by resolvetopname()
!			e:=e.equiv
!			goto retry

!		when linkid then
!			rxerror("FOUND LINK",p)
		esac
	fi
end

global function resolvetopname(symbol owner,stnewname,int moduleno,fmodule)symbol=
!stnewname points to a symrec with nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)
!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match
!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int i,m,extcount,modno
	symbol p,q,powner,d,e,dlldef,extdef,moddef,extmod
	[10]symbol ambiglist
	INT DEB
	STATIC INT MAXDUPL
	INT NDUPL

!CPL "RESOLVETOPNAME",STNEWNAME.NAME,=MODULENO
	if owner.nameid=procid then
!CPL "OWNER IS PROC"
		q:=owner.deflist
		while q do
!CPL "	DEFLIST",Q.NAME,STNEWNAME.NAME,Q.FIRSTDUPL,STNEWNAME
			if q.firstdupl=stnewname then		!use that match
!CPL "******** FOUND IN PROC DEFLIST"
				return q
			fi
			q:=q.nextdef
		od
	fi

!CPL "RESOLVETOPNAME2",STNEWNAME.NAME,NAMENAMES[OWNER.NAMEID]
	p:=stnewname.nextdupl

!Q:=P
!CPL "DUPLLIST:"
!WHILE Q DO
!	CPL Q.NAME,NAMENAMES[Q.NAMEID],"IN",Q.OWNER.NAME
!	Q:=Q.NEXTDEF
!OD
!
!

	extcount:=0
	extmod:=dlldef:=extdef:=moddef:=nil
	NDUPL:=0


	while p do						!for each possibe st entry of the same name
	++NDUPL

		powner:=p.owner			!the owner of that entry

		switch powner.nameid
		when procid then
			if powner=owner then			!immediate match
				return p
			fi
		when moduleid then			!p is file-scope item
!CPL "DUPL/LOOP:MODULEID:",=POWNER.MODULENO, =MODULENO
!CPL "MODMAP:"
!	FOR I TO NMODULES DO
!		CPL I,":",MODULETABLE[MODULENO].IMPORTMAP[I]
!	OD

			if powner.moduleno=moduleno then		!same module
				if owner.nameid=moduleid then	!immediate match
					return p
				fi
				moddef:=p			!take note, but continue searching (in case proc etc)
			elsif moduletable[moduleno].importmap[powner.moduleno] then
				if p.isglobal then
									!matches an external module imported by this name's module
					++extcount			!if an ext match is closest, there can only be one
					extdef:=p
	storeextdef::
					if extcount<ambiglist.len then
						ambiglist[extcount]:=extdef
					fi

!				elsif p.nameid=aliasid and p.equiv.attribs.ax_global then
!					++extcount
!					extdef:=p.equiv
!					goto storeextdef
				fi
			fi
		when dllmoduleid then

			modno:=powner.moduleno
			if modno=moduleno or moduletable[moduleno].importmap[modno] then
				dlldef:=p
			fi

		when typeid then
			if powner=owner then			!immediate match
				return p
			fi
		when programid then					!p is a module
			if p.nameid=moduleid then		!match a module name
				if fmodule then
					return p			!immediate match (unless proc but that would have
				fi						!matched by now
			fi
		endswitch

		p:=p.nextdupl
	od
!
!CPL "****** DONE TOPNAME",=MODDEF, =EXTDEF, =DLLDEF

!IF NDUPL>MAXDUPL THEN
!	MAXDUPL:=NDUPL
!	CPL =MAXDUPL,STNEWNAME.NAME
!FI


!if here, then no immediate match
!either of moddef/dlldef will be set
	if moddef then				!go with that first
		return moddef
	fi
	if extdef then
		if extcount>1 then
			for i:=1 to extcount do
				extdef:=ambiglist[i]
				println i,extdef.owner.name,namenames[extdef.owner.nameid]
			od
			rxerror_s("Ambiguous ext name: #",extdef.name)
		fi
		return extdef
	fi
	if extmod then return extmod fi

!CPL "NO MATCH",STNEWNAME.NAME,DLLDEF

	return dlldef				!will be nil when no match
end

proc resolvedot(symbol owner,unit p)=
	symbol qdef,rdef,d,newd,e,fielddef
	unit q,r
	int nfields,oldallowmod

	if symbolmode then
		resolvedot_sym(owner, p)
		return
	fi

	q:=p.a			!lhs
	r:=p.b			!rhs
	rdef:=r.def							!st entry for the field

	oldallowmod:=allowmodname
	allowmodname:=q.tag=jname
	rx_unit(owner,q)
	allowmodname:=oldallowmod

	case q.tag
	when jname then		!continue below

		d:=q.def
	when jtypeconst then	!was type
		d:=q.def
		goto dotype
	else					!assume expression
		rdef:=r.def
		goto doexprdot
	esac

	switch d.nameid
	when dllmoduleid,moduleid,typeid,procid,dllprocid then	!M./T./P./C. non-var lhs
dotype::
		newd:=finddupl(d, rdef)

		if newd then					!found
			switch newd.nameid
			when enumid then			!convert whole thing to constant
				p.tag:=jintconst
				p.value:=newd.index
				p.mode:=tint
			when constid then
				q:=newd.code			!q is knamedconst unit; q.c is value
				case q.tag
				when jintconst then
					p.tag:=jintconst
					p.a:=p.b:=nil
					p.value:=q.value
					p.mode:=newd.mode

				else
					rxerror("Rxdot:const?",p)
				esac
			when typeid then
				p.tag:=jtypeconst
				p.mode:=newd.mode
				p.def:=newd
			when staticid then
				p.tag:=jname
				p.def:=newd

			when procid,dllprocid then
				p.tag:=jname
				p.a:=p.b:=nil
				p.def:=newd
			when macroid then
				if e.nameid=macroid and not noexpand then
					++macrolevels
					expandmacro(p,p,nil)
					rx_unit(owner,p)
					--macrolevels
				fi

			else
				cpl namenames[newd.nameid],,".",,newd.name
				rxerror("Rxdot:.name not allowed here",p)
			endswitch

		else
			cpl d.name,,".",,rdef.name
			rxerror("Can't resolve",p)
		fi

	when frameid, staticid, paramid, fieldid, structfieldid then	!X. normal lhs
doexprdot::
		nfields:=0
		fielddef:=nil
		e:=rdef.nextdupl
		while e do

			case e.nameid
			when fieldid,structfieldid, constid, procid, typeid, staticid, dllprocid then
				++nfields
				fielddef:=e				!use this when unique
			esac
			e:=e.nextdupl
		od

		case nfields
		when 0 then				!no field exists with this name
			cpl rdef.name
			rxerror("Can't find field")
		else					!dupl field
			if rdef.nameid<>genericid then
				rxerror("Field name not generic")
			fi
		esac

	else
		cpl namenames[d.nameid]
		rxerror("RXDOT:Unknown nameid",p)
	endswitch
end

proc resolvedot_sym(symbol owner,unit p)=
	symbol qdef,rdef,d,newd,e,fielddef
	unit q,r
	int nfields,oldallowmod

	q:=p.a			!lhs
	r:=p.b			!rhs
	rdef:=r.def							!st entry for the field

	oldallowmod:=allowmodname
	allowmodname:=q.tag=jname
	rx_unit(owner,q)
	allowmodname:=oldallowmod

	case q.tag
	when jname then		!continue below

		d:=q.def
	when jtypeconst then	!was type
		d:=q.def
		if symbolmode then
			newd:=finddupl(d, rdef)
			if newd=nil then
				rxerror_s("Can't resolve .",rdef.name)
			fi
			case newd.nameid
			when fieldid,structfieldid then
				CPL "*******FIELD.$"
			else
				rxerror_s(".$ ON type:",namenames[newd.nameid])
			esac

		fi
		goto dotype
	else					!assume expression
		rxerror("RXDOTSYM?")
	esac

	switch d.nameid
	when dllmoduleid,moduleid,typeid,procid,dllprocid then	!M./T./P./C. non-var lhs
	dotype::
		newd:=finddupl(d, rdef)

		if newd then					!found
			p.tag:=jname
			p.a:=p.b:=nil
			p.def:=newd
		else
			rxerror_s(".$ Can't resolve",d.name)
		fi
!
	else
		rxerror_s("RX.$: Unknown nameid:",namenames[d.nameid],p)
	endswitch
end

global function finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

	if pdupl.nameid<>genericid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl

	while pdupl do
		if pdupl.owner=d then
!			if pdupl.nameid in [aliasid,linkid] then
!				return d.equiv
!			fi

			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od

	return nil
end

proc expandmacro(unit p, a, b)=
!is is a macro name unit, b is a macro parameter list (rx-processed), which
!can be nil
!p is either the call-unit as this may originally have been, or the same as a::
!M => (NAME)
!M(A,B) => (CALL NAME (A,B))
!Need to replace M or M(A,B) with the duplicated AST from a.code.
!When a name appears in the AST which is a local macroparam name, then that is
!replaced by the corresponding argument from B;
!The new code replaces P (either CALL or NAME)
!Supplied args may be more than macro takes (error) or may be less (error,
!or allow default arg values to be specified)
	symbol d,pm
	unit pnew
	int ignoreargs

!CPL "EXPANDMACRO"
	if macrolevels>10 then
		rxerror("Too many macro levels (recursive macro?)")
	fi

	d:=a.def

!First step: get list of macro formal parameters
!CPL =D.NAME,NAMENAMES[D.NAMEID]

	pm:=d.deflist
	nmacroparams:=0
	while pm do
!CPL =PM.NAME,=PM.NEXTPARAM,NAMENAMES[PM.NAMEID]
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.firstdupl		!generic st entry
		pm:=pm.nextdef
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
		b:=b.nextunit
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
		p.a:=pnew						!with expansion
	fi
end

function copylistunit(unit p)unit=
	unit q
	unit plist,plistx

	plist:=plistx:=nil
	while p do
		q:=copyunit(p)
		addlistunit(plist,plistx,q)
		p:=p.nextunit
	od
	return plist
end

function copyunit(unit p)unit=
	unit q
	symbol d

	if p=nil then return nil fi

!need to quickly check if a name unit is a macroparam

	if p.tag=jname then
		d:=p.def
		for i to nmacroparams do
			if macroparamsgen[i]=d then
				return copyunit(macroargs[i])
				exit
			fi
		od
	fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	if jflags[q.tag] then
		q.a:=copylistunit(q.a)
		if jflags[q.tag]=2 then
			q.b:=copylistunit(q.b)
		fi
	fi
	return q
end

proc replaceunit(unit p,q)=
!replace p with q, keeping same address of p, and same next pointer
!original contents discarded
	unit pnext
	pnext:=p.nextunit
	p^:=q^
	p.nextunit:=pnext
end

proc fixmode(ref strec owner, p)=
ref strec d,e
int m

m:=p^.mode

if m>=0 then return fi
m:=-m

if ttxmap[m] then				!already fixed
	p^.mode:=ttxmap[m]
	return
fi

if ttnamedefx2[m] then
	rxerror("Can't resolve a:b tentative types yet")
fi

d:=ttnamedefx[m]

e:=resolvetopname(owner,d,ttxmoduleno[m],0)

if e then
	ttxmap[m]:=e^.mode
	p^.mode:=e^.mode

else
	rxerror_s("Can't resolve tentative type: #",d^.name)
fi

end

function fixmode2(ref strec owner, int m)int=
!if m is a userx type, fix it up and return fixed up mode
!otherwise just return m
ref strec d,e
[256]char str

!CPL "FM1",=M
if m>=0 then return m fi
m:=-m

!CPL "FM2",=M
if ttxmap[m] then				!already fixed
	return ttxmap[m]
fi

!CPL "FM3",=M
if ttnamedefx2[m] then
	rxerror("2:Can't resolve a:b tentative types yet")
fi

d:=ttnamedefx[m]

!CPL "FM3",=M
!CPL "FIXMODE2",D.NAME

IF OWNER=NIL THEN
CPL D^.NAME
RXERROR("FIXMODE2 OWNER=0")
FI

!TTXMODULENO[M]:=1
!CPL "FM4",=M,=OWNER.NAME,NAMENAMES[D.NAMEID],=TTXMODULENO[M]
e:=resolvetopname(owner,d,ttxmoduleno[m],0)
!CPL "FM5",=E

if e then
	ttxmap[m]:=e^.mode
	return e^.mode
else
!	fprint @&.str,"# in module #, line:#",d^.name,moduletable[ttxmoduleno[m]].name,ttlinenox[m]
	fprint @&.str,"# in module #, line:#",d^.name,moduletable[ttxmoduleno[m]].name

	rxerror_s("2:Can't resolve tentative type: #",&.str)
fi
return 0
end

global proc fixusertypes=
ref userxrec p
ref int pmode
int m, rescan,i

!CPL "FIXUSERTYPES",=USERXMODELIST
!RETURN

for i:=1 to 2 do
	p:=userxmodelist
!CPL =P
	rescan:=0

!CPL I,":",P	!P.PMODE

	while p do
		m:=p^.pmode^
		if m<0 then
			m:=fixmode2(p^.owner,m)
			if m<0 and i=2 and ttxmap[abs m] then
				m:=ttxmap[abs m]
			fi
			if m<0 then
				rescan:=1
			else
				p^.pmode^:=m


IF TTTARGET[M]=M THEN
	CPL =TTNAME[M]
	RXERROR("RECURSIVE TYPE?")
FI
			fi
		fi

		p:=p^.nextmode
	od
	if not rescan then exit fi

od
if rescan then
	RXERROR("FIXUSERTYPES PHASE ERROR")
fi
!CPL "FIXED USER TYPES"


!!scan baseclasses
!for i to nbaseclasses do
!	dobaseclass(i)
!od

end

global proc tx_typetable=
	for i:=tlast+1 to ntypes do
		converttype(i)
	od
end

function getconstint(symbol owner,unit a)int=
!process unit found in tt-tables, and convert to int

	if a=nil then
		rxerror("GETCONSTINT A=NIL")
	fi

	rx_unit(owner, a)

	case a.tag
	when jintconst then
		return a.value
	when jrealconst then
		return a.xvalue
	else
		rxerror_s("Getconstint: not int/real",jtagnames[a.tag])
	esac
	return 0
end

global proc converttype(int m)=
!This 'conversion' is mainly about working out lengths and sizes and offsets
	symbol d,f,owner
	int first,a,b,index,length,lower, elemtype, nbits
	const int maxfield=256
	[maxfield+1]symbol fieldlist
	int oldmodno,pos
	int maxalign, nfields, size
	unit plength, plower

!CPL "CONVERTTYPE",=M,=NTYPES,TTNAME[M],TTSIZE[M],=STRMODE(TTBASETYPE[M])

	if ttsize[m] then return fi			!assume already done

	owner:=ttowner[m]
	plower:=ttlowerexpr[m]
	plength:=ttlengthexpr[m]

	case ttbasetype[m]
	when tpstringc,tpstringz then
		ttsize[m]:=ttlength[m]:=getconstint(owner,plength)
!
!	when tparray then
	when tcarray then
		if m=tarray then CPL "CT:ARRAY/ARRAY" fi
		if plower then
			ttlower[m]:=getconstint(owner,plower)
		else
			ttlower[m]:=1
		fi

		ttlength[m]:=getconstint(owner,plength)
		elemtype:=tttarget[m]

		case elemtype
		when tpu1,tpu2,tpu4 then
			nbits:=ttlength[m]*ttbitwidth[tttarget[m]]
			ttsize[m]:=(nbits-1)/8+1
		else
			converttype(tttarget[m])
			ttsize[m]:=ttlength[m]*ttsize[tttarget[m]]
		esac

	when tstruct then
		d:=ttnamedef[m]
		f:=d.deflist

		nfields:=0
		while f do
			if nfields>=maxfield then rxerror("Too many fields") fi
			fieldlist[++nfields]:=f
			f:=f.nextdef
		od

		fieldlist[nfields+1]:=nil
		ntopfields:=nallfields:=0
		maxalign:=1
		index:=1

		scanstruct(1, fieldlist, index, size, 0, ttcaligned[m], maxalign, 2)

		if ttcaligned[m] then
			size:=roundoffset(size,maxalign)
			d.maxalign:=maxalign
		else
			d.maxalign:=1
		fi

		ttsize[m]:=size
		ttlower[m]:=1
		ttlength[m]:=ntopfields

!CPL =NALLFIELDS, =NTOPFIELDS
!FOR I TO NTOPFIELDS DO
!	CPL I,STRUCTFIELDS[I].NAME
!OD
		d.topfieldlist:=pcm_alloc(symbol.bytes*ntopfields)
		memcpy(d.topfieldlist,&structfields,symbol.bytes*ntopfields)
!CPL =D.TOPFIELDLIST
!CPL =STRUCTFIELDS[1]
!CPL =D.TOPFIELDLIST,structfields[1].name
!CPL =D.TOPFIELDLIST,d.topfieldlist.name

	when trecord then
!
	else
!		CPL "CAN'T DO:",STRMODE(M),strmode(ttbasetype[m]),TTSIZE[M],TTNAMEDEF[M].NFIELDS
		CPL "CAN'T DO:",STRMODE(M),strmode(ttbasetype[m])
	esac
!	currmoduleno:=oldmodno
end

proc scanstruct(int smode, []symbol &fields, int &index, &isize, offset,
	calign, &maxalign, countmode)=
!process a span of struct fields
!smode=1/0 for structmode/unionmode
!index is next field in fieldlist
!offset=current offset
!maxalign=current max alignment, which can be updated
!isize returns the size of this span
!countmode=2/1/0 to with counting top-level/nested/union fields
	symbol f
	int newoffset, fieldsize, alignment
	int nfields, structmode, ndepth, size

	size:=0

!CPL =COUNTMODE

	while f:=fields[index++] do
		case f.nameid
		when structfieldid then
!			if smode then ++countedfields fi
			converttype(f.mode)
			fieldsize:=ttsize[f.mode]

			if calign then
				alignment:=getalignment(f.mode)
				maxalign max:=alignment
				newoffset:=roundoffset(offset, alignment)
				size+:=newoffset-offset
			else
				newoffset:=offset
			fi
			f.fieldoffset:=newoffset
			offset:=newoffset
countfields::
			++nallfields
			if countmode then
!				++ntopfields
				structfields[++ntopfields]:=f

			fi
		when structblockid then
			scanstruct(1, fields, index, fieldsize, offset, calign, maxalign,countmode)

		when unionblockid then
			scanstruct(0, fields, index, fieldsize, offset, calign, maxalign, (countmode|1|0))

		when endblockid then
			isize:=size
!			++countedfields
			return
		esac

		if smode then
			offset+:=fieldsize
			size+:=fieldsize
		else
			size:=max(size,fieldsize)
			countmode:=0
		fi

	od

	isize:=size				!end of fields; tread as endblock
!	++countedfields
end
=== qq_lib.m 13/32 ===
import* qq

var int currlineno
global var int nextavindex=0

var strbuffer exprstrvar
var ref strbuffer exprstr=&exprstrvar

const maxlocalunits=500
[maxlocalunits]unitrec unitpool
int nlocalunits

!tabledata []int opccodes, []ichar opcnames =
!	(kadd,		"+"),
!	(ksub,		"-"),
!	(kmul,		"*"),
!	(kdiv,		"/"),
!	(kidiv,	"%"),
!	(kneg,		"-"),
!	(keq,		"="),
!	(kne,		"<>"),
!	(klt,		"<"),
!	(kle,		"<="),
!	(kgt,		">"),
!	(kge,		">="),
!	(kiand,	"iand"),
!	(kior,		"ior"),
!	(kixor,	"ixor"),
!	(kinot,	"inot"),
!	(kshl,		"<<"),
!	(kshr,		">>"),
!	(kandl,	"and"),
!	(korl,		"or"),
!
!	(knotl,	"not"),
!!	(kmin1,	"min"),
!!	(kmax1,	"max"),
!
!	(kaddto,	"+:="),
!	(ksubto,	"-:="),
!	(kmulto,	"*:="),
!	(kdivto,	"/:="),
!	(knegto,	"-:="),
!	(kshlto,	"<<:="),
!	(kshrto,	">>:="),
!
!!	(kpreincrx,	"++"),
!!	(kpostincrx,	"++"),
!!	(kpredecrx,	"--"),
!!	(kpostdecrx,	"--"),
!
!	(0,		"")
!end

global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

proc $init=
!	initpacktypes()
end

global proc reportcterror(ichar errortype,mess,int pos, symbol currproc=nil)=
	geterrorinfo(pos,currproc)
	println errortype,"Error:"
	println "    ",,mess
	println

	showerrorsource()
	stopcompiler(errormodule,errorlineno)
end

global proc geterrorinfo(word pos, symbol currproc=nil)=
!slow is the low word of a string pointer into source code
!moduleno is the module number if known, otherwise 0 to work it out here
!Set up global error vars: errorline, errorpointer, errormodule, errorlineno

	ichar stoken, sline, smodule, slineend, s
	int length, column, lineno, offset, soffset, moduleno

	soffset:=pos.[0..23]
	moduleno:=pos.[24..31]
!
!CPL "GEI",=MODULENO,CURRPROC.NAME

	if soffset=0 and moduleno=0 then
		strcpy(errorline,"<no data>")
		strcpy(errorpointer,"???")
		errormodule:=&moduletable[1]
		errorlineno:=1
		return
	fi

	if currproc=nil then
!CPL "CP0"
		println "Geterrorinfo: can't find module"
		stop 1
	elsif currproc.nameid=procid then
!CPL "CP1"
		errormodule:=&moduletable[currproc.owner.moduleno]
		sterrorproc:=currproc
	else
!CPL "CP2",CURRPROC.MODULENO,CURRPROC.NAME
		errormodule:=&moduletable[currproc.moduleno]
		sterrorproc:=nil
	fi

!CPL "CP3"

!	moduleno:=1

	smodule:=errormodule.originalsource
	stoken:=smodule+soffset

	sline:=slineend:=stoken

!CPL "CP31",REF VOID(SLINE)
	while sline>smodule and sline^<>10 do --sline od
!CPL "CP32"
	if sline^=10 then ++sline fi

!CPL "CP4"
	s:=sline
	errorlineno:=1
	while s>smodule do
		if s^=10 then ++errorlineno fi
		--s
	od
!CPL "CP5"

	while slineend^ not in [13,10,26,0] do ++slineend od
	length:=slineend-sline
	column:=stoken-sline+1

!CPL "CP6"
	length min:=errorline.len-1

	memcpy(&errorline, sline, length)
	errorline[length+1]:=0

!CPL "CP7"
	for i to column-1 do
		if errorline[i] not in [9,' '] then
			errorpointer[i]:=' '
		else
			errorpointer[i]:=errorline[i]
		fi
	od

!CPL "CP8"
	errorpointer[column]:='^'
	errorpointer[column+1]:=0
!	errormodule:=m
!	if currproc.nameid=procid then
!		sterrorproc:=currproc
!	else
!		sterrorproc:=nil
!	fi

!CPL =ERRORMODULE
end

proc showerrorsource=

	println "Line:",errorlineno,"in Module",errormodule.name,,".q:"
	if sterrorproc then
		println "In function:",sterrorproc.name
	fi
	println " |",errorline
	println " |",errorpointer
end

global proc stopcompiler(ref modulerec m,int lineno)=
	filehandle f
	f:=fopen("$error.tmp","w")
!	println @f,modulename,,".q",lineno
	println @f,m.sourcepath,lineno
	fclose(f)
	println
	println
	stop 1
end

!global proc prterror(ichar mess)=
!	reportcterror("Print",mess,qpos)
!end

global proc gerror(ichar mess,unit p=nil)=
!CPL "G1"
	reportcterror("Code Gen",mess,(p|p.pos|qpos),stcurrproc)
end

global proc gerror_s(ichar mess, param,unit p=nil)=
	var [300]char str
!CPL "G2",MESS
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
CPL "GERROR-------",=STCURRPROC,=STCURRPROC.NAME
	reportcterror("Code Gen",&.str,(p|p.pos|qpos),stcurrproc)
end

global proc serror(ichar mess)=
!CPL "SERROR",LX.POS
!CPL "S1"
	reportcterror("Syntax",mess,lx.pos,stcurrproc)
end

global proc serror_s(ichar mess,param)=
	[300]char str
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	reportcterror("Syntax",str,lx.pos,stcurrproc)
end

global proc rxerror(ichar mess,unit p=nil)=
!CPL "SERROR",LX.POS
!CPL "R1"
	reportcterror("Resolve",mess,(p|p.pos|qpos),stcurrproc)
end

global proc rxerror_s(ichar mess,param, unit p=nil)=
	[300]char str
!CPL "R2"
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	rxerror(str,p)
end

global proc lxerror(ichar mess)=
	reportcterror("Lex",mess,lx.pos,stcurrproc)
end

global proc pcerror(ichar mess)=
	getpcerrorpos(pcptr)
	reportpcerror(mess,pcerrorpos,pcerrormodule.def)
end

global proc pcerror_s(ichar mess, param)=
	var [300]char str
	getpcerrorpos(pcptr)
!CPL =PCERRORPOS
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	reportpcerror(str,pcerrorpos,pcerrormodule.def)
end

global proc reportpcerror(ichar mess,int pos, symbol currproc=nil)=
	variant s,send
	ref int pc
	int count,elineno
	ref modulerec emodule

	geterrorinfo(pos,currproc)
	emodule:=errormodule		!remember first error (written to $error.tmp)
	elineno:=errorlineno

	println
!	println "*********************************************************"
	println " ":"80p*"
	println "PC Error:"
	println "    ",,mess
	println

	showerrorsource()

!	CPL "STACK TRACE FOLLOWS"
	s:=sptr
	send:=&varstack[stacksize]
	count:=0
	while s<=send and count<5 do
		if s.tag=treturn then
			pc:=s.uret.retaddr-3		!go back three to get to start of kcall/kcallptr instr
			getpcerrorpos(pc)
			geterrorinfo(pcerrorpos, pcerrormodule.def)
			println "Called from line",errorlineno,"in",pcerrormodule.def.name
!=STERRORPROC
!
!			findlinenumber(ptr,lineno,moduleno)
!!		println "Called from:",lineno,moduleno
!			printlinenumber(lineno,moduleno,"Called from:")
			++count
		fi
		++s
	od

	stopcompiler(emodule,elineno)
end

proc getpcerrorpos(ref int pc)=
!given pcptr, set up pcerrorpos, the lsw of the source pointer
!and set up pcerrormodule
	int offset
	ref int pcstart
	ref int32 pcsrcstart

	pcerrormodule:=&moduletable[findmodulefrompc(pc)]
!CPL =PCERRORMODULE,PCPTR,PCLNAMES[PCPTR^]

	pcstart:=pcerrormodule.pcstart
	pcsrcstart:=pcerrormodule.pcsrcstart

	offset:=pc-pcstart
	pcerrorpos:=(pcsrcstart+offset)^
end

global proc loaderror_s(ichar mess, param)=
	var [300]char str
	strcpy(&.str,mess)
	strcat(&.str," ")
	strcat(&.str,param)
	fprintln "Load error: #: #",mess, param
	println
	println
	stop 1
end

global proc loaderror(ichar mess)=
	loaderror_s(mess,"")
end

function findmodulefrompc(ref int pc)int=
!given pcptr, find which module it's currently executing
	for i to nmodules do
		if pc>=moduletable[i].pcstart and pc<moduletable[i].pcend then
			return i
		fi
	od
	println "Can't find pcptr module",pc
	stop 1
	return 0
end

global proc prterror(ichar mess)=
	println "Print error:",mess
	os_getch()
	stop 1
end

global proc pcustype(ichar mess, variant x) =
!CPL "PCUS1",X.TAG
	pcustype_t(mess, x.tag)
end

global proc pcustype_t(ichar mess, int t) =
	[256]char str

!CPL "PCUS2",T,TTNAME[0]

	fprint @str,"Type not supported: # : #",mess, ttname[t]

!CPL =STR

	getpcerrorpos(pcptr)
!CPL "PCUS3"
	reportpcerror(str,pcerrorpos,pcerrormodule.def)
end

global proc pcmxtypes(ichar mess, variant x,y) =
[256]char str

pcmxtypestt(mess,x.tag,y.tag)
!
!fprint @str, "MXSTYPE:Type not supported: # : #/#",
!		mess,ttname[x.tag],ttname[y.tag]
!getpcerrorpos()
!generror("PC",&.str,pcerrorpos,pcerrormodule)
end

global proc pcmxtypestt(ichar mess, int t,u) =
	[256]char str

	fprint @str, "Types not supported: # : #/#",
			mess,ttname[t],ttname[u]
	getpcerrorpos(pcptr)
	reportpcerror(str,pcerrorpos,pcerrormodule.def)
end

!global function getoptocode(int opc)int=		!GETOPTOCODE
!!opc is jadd etc
!!return matching jaddto, etc
!	int opcto
!
!	opcto:=jpcltocodes[opc]
!
!	if not opcto then
!		cpl jtagnames[opc]
!		serror("Can't find -to version")
!	fi
!	return opcto
!end

global proc resetunits=
!reset reusable units pool for new proc body
	nlocalunits:=0
end

global function allocunitrec:unit p=
	if inproc and nlocalunits<maxlocalunits then
		p:=&unitpool[++nlocalunits]
	else
		p:=pcm_alloc(unitrec.bytes)
++NALLUNITS
	fi
	p.word1:=p.nextunit:=p.a:=p.b:=nil
	p.nextunit:=p.a:=p.b:=nil
	p.pos:=lx.pos
!cpl "ALLOCUNIT",P.MODULENO
	return p
end

global function createintunit(int64 a)unit=
	var unit u
	u:=allocunitrec()
	u^.tag:=jintconst
	u^.value:=a
	return u
end

global function createint128unit(int128 a)unit=
	var unit u
	u:=allocunitrec()
	u^.tag:=jint128const
	u^.value128:=a
	return u
end

global function createwordunit(int64 a)unit=
	var unit u
	u:=allocunitrec()
	u^.tag:=jwordconst
	u^.value:=a
	return u
end

global function createword128unit(int128 a)unit=
	var unit u
	u:=allocunitrec()
	u^.tag:=jword128const
	u^.value128:=a
	return u
end

global function createrealunit(real64 x)unit=
	var unit u
	u:=allocunitrec()
	u^.tag:=jrealconst
	u^.xvalue:=x
	return u
end

global function createstringunit(ichar s, int slength=-1)unit=
	var unit u
	if slength=-1 then
		slength:=strlen(s)
	fi

	u:=allocunitrec()
	u^.tag:=jstringconst
	u^.svalue:=pcm_alloc(slength+1)
	if slength then
		memcpy(u^.svalue,s,slength)
	fi
	(u^.svalue+slength)^:=0
	u^.slength:=slength
	return u
end

global function createunit0(int tag)unit=
	var unit u
	u:=allocunitrec()
	u.tag:=tag
	return u
end

global function createunit1(int tag, unit p)unit=
	var unit u
	u:=allocunitrec()
	u^.tag:=tag
	u^.a:=p
	return u
end

global function createunit2(int tag, unit p,q)unit=
	var unit u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q

	return u
end

global function createname(ref strec p)ref unitrec=
	var ref unitrec u

	u:=allocunitrec()
	u.tag:=jname
	u.def:=p

	return u
end

global proc addlistunit(unit &ulist,&ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
!p can be a list, then all are added

while p do

	if ulist=nil then		!first
		ulist:=ulistx:=p
	else
		ulistx^.nextunit:=p
	fi
	ulistx:=p			!update end-of-list pointer

	p:=p.nextunit
od
end

!global proc addlistparam(ref symbol ulist,ulistx,symbol p)=
!!add unit p to unit structure ulist,^ulistx  which can be null
!if ulist^=nil then		!first
!	ulist^:=ulistx^:=p
!else
!	ulistx^^.nextparam:=p
!fi
!ulistx^:=p			!update end-of-list pointer
!end

global function getrangelwbunit(unit p)unit=
if p.tag=jmakerange then
	return p.a
else
	return createunit1(jlwb,p)
fi
end

global function getrangeupbunit(unit p)unit=
if p.tag=jmakerange then
	return p.b
else
	return createunit1(jupb,p)
fi
end

global function createavname:unit=
!create auto-var name and return pointer to st entry
var symbol p
var [32]char str
var ichar name

sprintf(&.str,"av$%d",++nextavindex)

name:=pcm_copyheapstring(&.str)
p:=addnamestr(name)

return createname(p)
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

global function convCstring(ref char svalue,int length)ref char =
! a contains a string object which is a NON-zero-terminated string.
! Set up a pointer to a proper zero-terminated one and return that.
! This uses a ring of 3 static string objects it will only work for strings up to
! a certain length, and only if not more than 3 are needed for any single call.

	enum (strbufflen=2000)
	static var [0:strbufflen]char strbuffer1
	static var [0:strbufflen]char strbuffer2
	static var [0:strbufflen]char strbuffer3
	static var int strindex=0		!index of current buffer: cycles between 0,1,2
	static var [0:]ref [0:]char table=(
		&strbuffer1,&strbuffer2,&strbuffer3)
!	&strbuffer4,&strbuffer5,&strbuffer6)
	var ref[0:]char p

	if length>=strbufflen then
		pcerror("ConvCstring>=2000")
	fi

	if svalue=nil then
		return ""
	fi

	if ++strindex=table.len then
		strindex:=0
	fi
	p:=table[strindex]
	memcpy(p,svalue,length)
!	(p+length)^:=0
	p^[length]:=0
	return cast(p)
end

global function findprocname(ref proc fnptr)ichar=
	var ichar name
	var int n:=$get_nprocs()

	for i to n do
		if $get_procaddr(i)=fnptr then
			return $get_procname(i)
		fi
	od

	return "?"
end

!global function getpacktypename(int tp,long=0)ichar=
!	static var [64]char str
!	str[1]:=0
!	getpacktypename2(tp,&.str,long)
!	return &.str
!end

!global proc getpacktypename2(int tp,ichar dest,int long=0)=
!	var [64]char str,str2
!	str[1]:=0
!	str2[1]:=0
!
!	case ppcat[tp]
!	when tvoid then
!		strcat(dest,"void")
!	when trefpack then
!		strcat(dest,"ref ")
!		getpacktypename2(pptarget[tp],dest+4)
!	when tstring then
!		strcat(dest,"stringz")
!	when tint,tword,treal then
!		strcat(dest,packtypenames[tp])
!	when tarray then
!		getpacktypename2(pptarget[tp],&.str2)
!		sprintf(&.str,"[%d:%d]%s",pplower[tp],pplength[tp],&.str2)
!	when tstruct then
!		strcat(dest,"<Struct>")
!!	when tobject then
!!		strcat(dest,"<Object>")
!	when tstructdef then
!		if long then
!			strstructdef(tp,dest)
!		else
!			strcat(dest,ppstructobj[tp].ustructdef.name)
!		fi
!!		strcat(dest,"STRUCTDEF")
!
!	else
!CPL TTNAME[PPCAT[TP]]
!		STRCAT(DEST,"packtype?")
!	esac
!	strcat(dest,&.str)
!end
!
!proc strstructdef(int tp, ichar dest)=
!	var object p
!	var int n
!	var ref[]packfieldrec packfields
!	var [32]char str
!
!	p:=ppstructobj[tp]
!
!	sprintf(&.str,"Struct<%s>(",p.ustructdef.name)
!	strcat(dest,&.str)
!!	strcat(dest,"Struct(")
!
!	n:=p.ustructdef.nfields
!	packfields:=p.ustructdef.packfields
!
!	for i to n do
!		strcat(dest,"(")
!		strcat(dest,packfields^[i].name)
!		strcat(dest,":")
!		getpacktypename2(packfields^[i].packmode,dest)
!		sprintf(&.str,":%d +%d",packfields^[i].size,packfields^[i].offset)
!		strcat(dest,&.str)
!		strcat(dest,")")
!	od
!
!	sprintf(&.str,")<%d>",p.ustructdef.size)
!
!	strcat(dest,&.str)
!
!end

!function newpacktype:int=
!	if npacktypes>=maxpacktype then
!		pcerror("pack type overflow")
!	fi
!	return ++npacktypes
!end

!global function makepackarraytype(int tp, lower, length)int=
!	newpacktype()
!
!	ppcat[npacktypes]:=tarray
!	pptarget[npacktypes]:=tp
!	pplower[npacktypes]:=lower
!	pplength[npacktypes]:=length
!	ppsize[npacktypes]:=length*ppsize[tp]
!!	pprecobj[npacktypes]:=nil
!	return npacktypes
!end
!
!global function makepackreftype(int tp)int=
!	newpacktype()
!
!	ppcat[npacktypes]:=trefpack
!	pptarget[npacktypes]:=tp
!	ppsize[npacktypes]:=8
!	return npacktypes
!end

!global function makestructtype(object pstruct)int=
!	newpacktype()
!
!	ppcat[npacktypes]:=tstructdef
!!	pptarget[npacktypes]:=tp
!	ppsize[npacktypes]:=pstruct.ustructdef.size
!	ppstructobj[npacktypes]:=pstruct
!
!	return npacktypes
!end

global function convtostringz(ref char svalue,int length)ref char =
! a contains a string object which is a NON-zero-terminated string.
! Set up a pointer to a proper zero-terminated one and return that.
! This uses a ring of 3 static string objects it will only work for strings up to
! a certain length, and only if not more than 3 are needed for any single call.

	enum (strbufflen=2000)
	static var [0:strbufflen]char strbuffer1
	static var [0:strbufflen]char strbuffer2
	static var [0:strbufflen]char strbuffer3
	static var [0:strbufflen]char strbuffer4
	static var [0:strbufflen]char strbuffer5
	static var [0:strbufflen]char strbuffer6
	static var int strindex=0		!index of current buffer: cycles between 0,1,2
	static var [0:]ref [0:]char table=(
		&strbuffer1,&strbuffer2,&strbuffer3,
		&strbuffer4,&strbuffer5,&strbuffer6)
!	cast(strbuffer1),cast(strbuffer2),cast(strbuffer3),
!	cast(strbuffer4),cast(strbuffer5),cast(strbuffer6))
	var ref[0:]char p

	if length>=strbufflen then
		pcerror("Convtostringz>=2000")
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

global function strexpr(unit p)ref strbuffer=
	gs_init(exprstr)
	jeval(p)
	return exprstr
end

global function strexpr_s(unit p)ichar=
	if p=nil then return "" fi
	gs_init(exprstr)
	jeval(p)
	return exprstr.strptr
end

proc jeval(unit p)=
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
var unit q
var [500]char str

!CPL "JEVAL",JTAGNAMES[P.TAG]

case p.tag
when jintconst then
	sprintf(&.str,"%lld",p.value)
	additem(&.str)

when jrealconst then
	sprintf(&.str,"%f",p.value)
	additem(&.str)

when jstringconst then
	if p.slength>str.len/2 then
		strcpy(&.str,"LONGSTR)")
	else
		convertstring(p.svalue,&.str)
	fi
	additem("""")
	additem(&.str)
	additem("""")
!
when jname then
	additem(p.def.name)

when jadd, jsub, jmul, jdiv, jidiv, jidivrem, jiand, jior, jixor,
	 jshl, jshr, jin, jnotin, jmin, jmax, jmakerange,
	 jeq, jne, jlt, jle, jge, jgt, jirem, jpower,
	 jappend then

	strcpy(&.str,getopcname(p.tag))
	additem("(")
	jeval(p.a)
	additem(&.str)
	jeval(p.b)
	additem(")")

when jneg,jabs,jinot,jchr,jasc,jsqrt,jsqr,jsign,jsin,jcos,jtan,jasin,
	jacos,jatan,jln,jlg,jlog,jexp,jround,jfloor,jceil,jfract,jfmod, jlwb,jupb,jlen,jbounds,
	jbitwidth,jbytesize,jisvoid,jisdef,jisint,jisreal,jisstring,
	jisrange,jislist,jisrecord,jisset,jispointer,jminvalue,jmaxvalue,
	jnotl,jistruel, jismutable, jtype, jbasetype, jelemtype then

	strcpy(&.str,getopcname(p.tag))
	additem(&.str)
	additem("(")
	jeval(p.a)
	additem(")")

when jcall then
	jeval(p.a)
	additem("(")

	q:=p.b
	while q do
		jeval(q)
		q:=q.nextunit
		if q then additem(",") fi
	od
	additem(")")

when jcallhost then
	additem("Host<")
	additem(hostfnnames[p.index]+5)
	additem(">(")

	q:=p.a
	while q do
		jeval(q)
		q:=q.nextunit
		if q then additem(",") fi
	od
	additem(")")

when jindex,jdotindex then
	jeval(p.a)
!	if p.tag=jdotindex or p.tag=jdotslice then
	if p.tag=jdotindex then
		additem(".")
	fi
	additem("[")
	jeval(p.b)
	additem("]")

!when jkeyindex,jdotkeyindex then
!	jeval(p.a)
!	if p.tag=jdotkeyindex then
!		additem(".")
!	fi
!	additem("{")
!	jeval(p.b)
!	additem("}")
!
when jdot then
	jeval(p.a)
	additem(".")
	jeval(p.b)

!when jmakelist,jmakesetlist,jmakeconstr,jmakedict then
when jmakelist then
	additem("(")
	q:=p.a
	while q do
		jeval(q)
		q:=q.nextunit
		if q then additem(",") fi
	od
	additem(")")

when jmakeset,jmakedict then
	additem("[")
	q:=p.a
	while q do
		jeval(q)
		q:=q.nextunit
		if q then additem(",") fi
	od
	additem("]")

!when jmakerange then
!	additem("(")
!	jeval(p.a)
!	additem("..")
!	jeval(p.b)
!	additem(")")
!
when jassign then
	jeval(p.a)
	additem(":=")
	jeval(p.b)

!when jifx then
!	additem("(")
!	jeval(p.a)
!	additem("|")
!	jeval(p.b)
!	additem("|")
!	jeval(p.c)
!	additem(")")
!
when jtypeconst then
	additem(strmode(p.mode))

!when jclassconst then
!!	additem("Class<")
!	additem(p.def.name)
!	additem(">")
!
when jconvert then

	additem(strmode(p.mode))
	additem("(")
	jeval(p.a)
	additem(")")
when jkeyvalue then
	jeval(p.a)
	additem(":")
	jeval(p.b)

!when jlongint then
!	gs_str(dest,"Longint:")
!	goto dostring
!
!when jptr then
!	jeval(p.a)
!	additem("^")
!
when jaddrof then
	additem("&")
	jeval(p.a)

when jdaddrof then
	additem("&&")
	jeval(p.a)

when jptr then
	jeval(p.a)
	additem("^")

when jnil then
	additem("nil")

when jsymbol then
	jeval(p.a)
	additem(".$")

when jcmpchain then
	additem("CMPCHAIN:")
	q:=p.a
	jeval(q)

	for i to 4 do
		q:=q.nextunit
!CP P.CMPGENOP[I],$
		if p.cmpgenop[i]=0 then exit fi
		additem(jtagnames[p.cmpgenop[i]])
		jeval(q)

!		print @dev,jtagnames[p.cmpgenop[i]],$
	od
!CPL JTAGNAMES.LEN

!when jclamp then
!	additem("(")
!	jeval(p.a)
!	additem(",")
!	jeval(p.b)
!	additem(",")
!	jeval(p.c)
!	additem(")")
!
!when jblock then
!	additem("<JBLOCK>")
!
!when jmultexpr then
!	additem("MULTEXPR(")
!	q:=p.a
!	while q do
!		jeval(q)
!		q:=q.nextunit
!		if q then additem(",") fi
!	od
!	
!	additem(")")
!
!!when jupper then
!!	additem("<JUPPER>")
!
else
	CPL jtagnames[p.tag]
!CPL P.POS >>24
!CPL =MODULETABLE[1].NAME
!CPL =MODULETABLE[2].NAME
!PRINTUNIT(P)
	loaderror_s("CAN'T DO JEVAL:",jtagnames[p.tag])
!	PCERROR("CAN'T DO JEVAL:"+jtagnames[p.tag])
end
!CPL "JEVALX"
end

global proc additem(ichar s)=
!like genstr, but ensure there is white space separation as needed from the last output
var ichar d
var int lastchar,nextchar

d:=exprstr.strptr

if exprstr.length then
	lastchar:=(d+exprstr.length-1)^
	nextchar:=s^
	if isalphanum(lastchar) and isalphanum(nextchar) then
		strbuffer_add(exprstr," ")
	fi
fi
strbuffer_add(exprstr,s)
end

function isalphanum(int c)int=
if c>='A' and c<='Z' or c>='a' and c<='z' or c>='0' and c<='9' then
	return 1
fi
return 0
end

global function getopcname(int opc)ichar=
!op is a kcode representing an operator
!return the name as it might appear in J code
!caller must check for differences specific to the target
	ichar s

	s:=jtagnames[opc]
	if s^='j' then ++s fi
	return s
end

global proc convertstring(ichar s, t)=		!CONVERTSTRING
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
var int c

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

global function extractstringz(variant p)ichar=
!get string value from object, and return pointer to zero-terminated heap copy
	object q:=p.objptr
	if p.tag<>tstring then pcerror("estrz?") fi
	if q.ustr.length then
		return pcm_copyheapstring(convtostringz(q.ustr.strptr,q.ustr.length))
	else
		return pcm_copyheapstring("")
	fi
end

global function createavnamex(symbol owner)unit p=
!local autovar needed from genpcl
!needs to update local vars

	symbol d
	p:=createavname()
	resolvename(owner,p)
	d:=p.def

	if d.nameid=frameid then
		++nproclocals
		d.index:=-nproclocals
		pproclocals^:=nproclocals
	fi							!else created at module level

	return p
end

global proc storemode(symbol owner, int m, ref int16 p)=
	ref userxrec q
!CPL "STOREMODE",STRMODE(M)
	p^:=m
	if m>=0 then return fi

	q:=pcm_alloc(userxrec.bytes)
	q.owner:=owner

	IF OWNER=NIL THEN
!CPL =ID
		SERROR("STOREMODE/OWNER=0")
	FI

	q^.pmode:=p
	q^.nextmode:=userxmodelist
	userxmodelist:=q
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

global function raiseexception(int exceptno)ref int =
	variant stackend,oldsptr

	stackend:=&varstack[stacksize]
	oldsptr:=sptr
	do
		if sptr>=stackend then
			sptr:=oldsptr
			default_exception(exceptno)
		fi
		if sptr.tag=texception and (exceptno=0 or sptr.uexcept.exceptiontype=exceptno) then
			exit
		fi
		var_unshare(sptr)
		++sptr
	od

!found exception entry on stack; keep it there
	frameptr:=ref byte(sptr)+sptr.uexcept.frameoffset
	return cast(sptr.refptr)
end

global proc raise_error(int error_no)=
!exception raised internally (not in user code)
!caller may not be able to manipulate pcptr
!here, push the error number, and set pcptr to point to a
!block of several kraise opcodes, as it is not how the byte-code
!handler, when it proceeds, will step pcptr

	(--sptr).tagx:=tint
	sptr.value:=error_no

	err_pcptr:=pcptr

	pcptr:=raiseseq
end

proc default_exception(int exceptno)=
	CPL "DEFAULT EXCEPTION HANDLER"

	case exceptno
	when pc_error then
		pcerror("PC/ERROR")
	when user_error then
		pcerror("USER/ERROR")
	when type_error then
		pcptr:=err_pcptr
PCERROR("PCUSTYPEDEF")
!		pcustype_def(err_message,&err_var1)

	when mixedtype_error then
		pcptr:=err_pcptr
PCERROR("PCMXTYPESDEF")
!		pcmxtypes_def(err_message,&err_var1,&err_var2)

	when divide_error then
!	println "Divide by zero"
!CPL "DIVERROR",pcptr
		pcptr:=err_pcptr

		pcerror("EXCEPTION/DIVIDE BY ZERO")

	when stopmodule_error then
	CPL "STOPMODULEERROR"
	when bounds_error then
	CPL "BOUNDSERROR"
	else
		cpl "Exception:",errornames[exceptno]
	esac


	stop 1
end

global function testelem(ref[0:]byte p,int n)int =		!TESTELEM
!caller must check that n is in range
return ((p^[n>>3] iand bytemasks[n iand 7])|1|0)
end

global proc setelem(ref[0:]byte p,int n) =		!SETELEM
!CPL "SETELEM"

p^[n>>3] ior:= bytemasks[n iand 7]
end

global function strstack(variant p)ichar=
	return strint(int(p-&.varstack))
end

=== qq_handlers.m 14/32 ===
import* qq

global macro getopnda = (pcptr+1)^
global macro getopndb = (pcptr+2)^
global macro getopndc = (pcptr+3)^
global macro getopndd = (pcptr+4)^

!step pcptr to next bytecode, skipping n operands
!macro skip(n) = pcptr+:=(n+1)
global macro skip(n) = pcptr:=pcptr+(n+1)


var ref int paramdefretloc
var int insiderecorddef

global var [0..pclnames.upb]ref proc handlertable

global proc inithandlers=
	ichar name
	static int handlersdone=0

	if handlersdone then return fi

	for i to $get_nprocs() do
		name:=$get_procname(i)
		if eqbytes(name,"k_",2) then
			for k:=0 to pclnames.upb do
				if eqstring(name+2,pclnames[k]+1) then		!skip "k_" and "k"
					handlertable[k]:=$get_procaddr(i)
					exit
				fi
			else
				pcerror_s("Unknown handler",name)
			od
		fi
	od

	for i in handlertable.bounds when handlertable[i]=nil do
		handlertable[i]:=cast(kunimpl)
	od

	handlersdone:=1
end

proc kunimpl=
	if hasbytecodes then
		pcerror_s("Unimplemented:",pclnames[pcptr^])
	else
		pcerror("Unimplemented (use -fdebug to see opcode)")

!int n:=$get_nprocs
!for i to n do
!	if $get_procaddr(i)=cast(pcptr^,REF VOID) then
!CPL "PROC IS:",$GET_PROCNAME(I)
!		EXIT
!	FI
!OD
!
!
!
!		pcerror_s("Unimplemented:",getpclname(cast(pcptr^)))
	fi
end

GLOBAL proc k_startmodule=
!CPL "STARTMODULE"
	++pcptr
end

!proc k_lineno=
!	pclineno:=getopnda
!	skip(1)
!!	pcptr+:=2
!end

proc k_pushci=
	--sptr
	sptr.tagx:=tint
	sptr.value:=getopnda
	skip(1)
end

proc k_pushcu=
	--sptr
	sptr.tagx:=tword
	sptr.value:=getopnda
	skip(1)
end

proc k_pushnil=
	--sptr
	sptr.tagx:=trefpack
	sptr.uref.elemtag:=tpvoid
	sptr.uref.ptr:=nil
	++pcptr
end

!proc k_pushcr=
!	(--sptr)^:=real_make(real@(getopnda))
!	skip(1)
!end

proc k_pushcs=
	--sptr
	sptr.tagx:=tstring ior hasrefmask
	sptr.objptr:=cast(getopnda)
	++sptr.objptr.refcount
	skip(1)
end

proc k_pushcr=
	--sptr
	sptr.tagx:=treal
	sptr.xvalue:=real@(getopnda)
	skip(1)
end

proc k_stop=
	++sptr
	stopped:=1
!CPL "STOPPED", =PCPTR,=PCLLEVEL

end

proc k_stoprunproc=
	stopped:=1
CPL "******* STOPPING RUN PROC"

end

proc k_pushm=
	--sptr
	sptr^:=variant(getopnda)^
	var_share(sptr)

	skip(1)
end

proc k_pushf=
	--sptr
	sptr^:=variant(frameptr+getopnda)^
	var_share(sptr)

	skip(1)
end

proc k_pushmref=
	--sptr
	sptr.tagx:=trefvar
	sptr.varptr:=variant(getopnda)

	skip(1)
end

proc k_pushfref=
	--sptr
	sptr.tagx:=trefvar
	sptr.varptr:=variant(frameptr+getopnda)

	skip(1)
end

proc k_popm=
	variant p

	p:=variant(getopnda)
	var_unshare(p)
	p^:=sptr^				!transfer reference
	++sptr

	skip(1)
end

proc k_zpopm=
	(variant(getopnda))^:=sptr^				!transfer reference
	++sptr
	skip(1)
end

proc k_popf=
	variant p

	p:=variant(frameptr+getopnda)
	var_unshare(p)
	p^:=sptr^				!transfer reference
	++sptr

	skip(1)
end

proc k_zpopf=
	variant p

	p:=variant(frameptr+getopnda)
	p^:=sptr^				!transfer reference
	++sptr

	skip(1)
end

proc k_popretval=
!	k_popf()
	variant p

	p:=variant(frameptr+getopnda)
	p^:=sptr^				!transfer reference
	++sptr

	skip(1)
end

proc k_tom=
	variant p

	p:=cast(getopndb)

	--p.value

	if p.value then
		pcptr:=cast(getopnda)
	else
		skip(2)
	fi
end

global proc k_tof=
	variant p

	p:=cast(frameptr+getopndb)

	--p.value

	if p.value then
		pcptr:=cast(getopnda)
	else
		skip(2)
	fi
end

proc k_add=
	variant y:=sptr++
	varrec x:=sptr^

	var_add(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

proc k_sub=
	variant y:=sptr++
	varrec x:=sptr^

	var_sub(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

proc k_mul=
	variant y:=sptr++
	varrec x:=sptr^

	var_mul(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!
!
!
!	variant x,y
!	varrec z
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		z:=sptr^
!		case pr(x.tag,y.tag)
!		when pr(tlist,tint) then
!			var_mul_list(sptr,y.value)
!		when pr(tstring,tint) then
!			var_mul_string(sptr,y.value)
!		else
!			pcmxtypes("Mul",x,y)
!		esac
!		var_unshare(&z)
!	else
!		case x.tag
!		when tint then
!			sptr.value*:=y.value
!		when treal then
!			sptr.xvalue*:=y.xvalue
!		else
!			pcustype("Mul",x)
!		esac
!	fi
!
!	++pcptr
end

proc k_div=
	variant y:=sptr++
	varrec x:=sptr^

	var_div(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!	variant x,y
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("Div",x,y)
!	else
!
!		case x.tag
!		when tint then
!			sptr.tagx:=treal
!			sptr.xvalue:=real(sptr.value)/y.value
!		when treal then
!			sptr.xvalue/:=y.xvalue
!		else
!			pcustype("Div",x)
!		esac
!	fi
!
!	++pcptr
end

proc k_idiv=
	variant y:=sptr++
	varrec x:=sptr^

	var_idiv(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!	variant x,y
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("Idiv",x,y)
!	else
!
!		case x.tag
!		when tint then
!!			sptr.value/:=y.value
!			sptr.value:=sptr.value/y.value
!		else
!			pcustype("Idiv",x)
!		esac
!	fi
!
!	++pcptr
end

proc k_irem=
	variant y:=sptr++
	varrec x:=sptr^

	var_irem(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!	variant x,y
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("Irem",x,y)
!	else
!		case x.tag
!		when tint then
!!			sptr.value/:=y.value
!			sptr.value:=sptr.value rem y.value
!		else
!			pcustype("Irem",x)
!		esac
!	fi
!
!	++pcptr
end

proc k_iand=
	variant y:=sptr++
	varrec x:=sptr^

	var_iand(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!	variant x,y
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("Iand",x,y)
!
!	else
!		case x.tag
!		when tint then
!			sptr.value iand:=y.value
!		else
!			pcustype("Iand",x)
!		esac
!	fi
!	
!	++pcptr
end

proc k_ior=
	variant y:=sptr++
	varrec x:=sptr^

	var_ior(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
end

proc k_ixor=
	variant y:=sptr++
	varrec x:=sptr^

	var_ixor(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!	variant x,y
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("Ixor",x,y)
!
!	else
!		case x.tag
!		when tint then
!			sptr.value ixor:=y.value
!		else
!			pcustype("Ixor",x)
!		esac
!	fi
!	
!	++pcptr
end

proc k_shl=
	variant y:=sptr++
	varrec x:=sptr^

	var_shl(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!	variant x,y
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("Shl",x,y)
!
!	else
!!CPL "SHL"
!		case x.tag
!		when tint then
!			sptr.value <<:=y.value
!		else
!			pcustype("Shl",x)
!		esac
!	fi
!	
!	++pcptr
end

proc k_shr=
	variant y:=sptr++
	varrec x:=sptr^

	var_shr(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!	variant x,y
!	int f
!
!	y:=sptr++
!	x:=sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("Shr",x,y)
!
!	else
!		case x.tag
!		when tint then
!			sptr.value >>:=y.value
!		else
!			pcustype("Shr",x)
!		esac
!	fi
!	
!	++pcptr
end

proc k_sqr=
	case sptr.tag
	when tint then
		sptr.value:=sqr(sptr.value)
	when treal then
		sptr.xvalue:=sqr(sptr.xvalue)
	else
		pcustype("Sqr",sptr)
	esac

	++pcptr
end

proc k_sqrt=
	switch sptr.tag
	when tint then
		sptr.tagx:=treal
		sptr.xvalue:=sqrt sptr.value

	when treal then
		sptr.xvalue:=sqrt sptr.xvalue

	else
		pcustype("SQRT",sptr)
	end switch

	++pcptr
end

proc k_sin=
	switch sptr.tag
	when treal then
		sptr.xvalue:=sin(sptr.xvalue)

	else
		pcustype("SIN",sptr)
	end switch

	++pcptr
end

proc k_cos=
	switch sptr.tag
	when treal then
		sptr.xvalue:=cos(sptr.xvalue)

	else
		pcustype("COS",sptr)
	end switch

	++pcptr
end

proc k_neg=
	case sptr.tag
	when tint then
		sptr.value:=-sptr.value
!		-:=sptr.value
	when treal then
		sptr.xvalue:=-sptr.xvalue
!		-:=sptr.xvalue
	else
		pcustype("NEG",sptr)
	end case

	++pcptr
end

proc k_abs=
	case sptr.tag
	when tint then
		sptr.value:=abs sptr.value
!		-:=sptr.value
	when treal then
		sptr.xvalue:= abs sptr.xvalue
!		-:=sptr.xvalue
	else
		pcustype("ABS",sptr)
	end case

	++pcptr
end

proc k_inot=
	case sptr.tag
	when tint then
		sptr.value:=inot sptr.value
	else
		pcustype("INOT",sptr)
	end case

	++pcptr
end

proc k_istruel=
	int res

	res:=var_istruel(sptr)
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

proc k_notl=
	int res

	res:=not var_istruel(sptr)
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

proc k_jumpeq=
	variant y:=sptr++
	variant x:=sptr++

	if var_equal(x,y) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

proc k_jumpne=
	variant x,y

	y:=sptr++
	x:=sptr++

	if not var_equal(x,y) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

proc k_jumplt=
	variant x,y

!	y:=sptr++
!	x:=sptr++

	y:=sptr
	x:=sptr+1

	sptr+:=2

!	if x.tag=y.tag=tint then
!		if x.value<y.value then
!			pcptr:=cast(getopnda)
!		else
!			skip(1)
!		fi
!	elsif var_compare(x,y)<0 then
	if var_compare(x,y)<0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

proc k_jumple=
	variant x,y

	y:=sptr++
	x:=sptr++

!	if x.tag=y.tag=tint then
!		if x.value<=y.value then
!			pcptr:=cast(getopnda)
!		else
!			skip(1)
!		fi
	if var_compare(x,y)<=0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

proc k_jumpge=
	variant x,y

	y:=sptr
	x:=sptr+1

	sptr+:=2


!	if x.tag=y.tag=tint then
!		if x.value>=y.value then
!			pcptr:=cast(getopnda)
!		else
!			skip(1)
!		fi
	if var_compare(x,y)>=0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)

end

proc k_jumpgt=
	variant x,y

	y:=sptr++
	x:=sptr++

!	if x.tag=y.tag=tint then
!		if x.value>y.value then
!			pcptr:=cast(getopnda)
!		else
!			skip(1)
!		fi
	if var_compare(x,y)>0 then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)
	var_unshare(y)
end

proc k_jumpfalse=
	variant x:=sptr++

	if not var_istruel(x) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
	var_unshare(x)

end

proc k_jumptrue=
	variant x:=sptr++

	if var_istruel(x) then
		pcptr:=cast(getopnda)
	else
		skip(1)
	fi
end

proc k_incrtom=
	variant p
!
	p:=variant(getopnda)
	case p.tag
	when tint then
		++p.value
	when trefpack then
		p.uref.ptr+:=ttsize[p.uref.elemtag]
	when treal then
		p.xvalue+:=1

	else
		pcustype("incrtom",p)
	end
	skip(1)
end

proc k_incrtof=
	variant p

	p:=variant(frameptr+getopnda)
	case p.tag
	when tint,tword then
		++p.value
	when trefpack then
		p.uref.ptr+:=ttsize[p.uref.elemtag]
	when treal then
		p.xvalue+:=1
	else
		pcustype("incrtof",p)
	esac
	skip(1)
end

proc k_decrtom=
	variant p

	p:=variant(getopnda)
	case p.tag
	when tint,tword then
		--p.value
	when trefpack then
		p.value-:=ttsize[p.uref.elemtag]
	when treal then
		p.xvalue-:=1
	else
		pcustype("decrtom",p)
	esac
	skip(1)
end

proc k_decrtof=
	variant p

	p:=variant(frameptr+getopnda)
	case p.tag
	when tint,tword then
		--p.value
	when treal then
		p.xvalue-:=1
	else
		pcustype("decrtof",p)
	esac
	skip(1)
end

proc k_incrload=
	varrec v

	v:=sptr^
	k_incrptr()
	var_loadptr(&v,--sptr)
end

proc k_loadincr=
	varrec v

	v:=sptr^
	var_loadptr(sptr,sptr)
	--sptr
	sptr^:=v

	k_incrptr()
end

proc k_decrload=
	varrec v

	v:=sptr^
	k_decrptr()
	var_loadptr(&v,--sptr)
end

proc k_loaddecr=
	varrec v

	v:=sptr^
	var_loadptr(sptr,sptr)
	--sptr
	sptr^:=v

	k_decrptr()
end

proc k_incrptr=
	variant p

	p:=sptr++

	switch p.tag
	when trefvar then			!increment what ptr points to
		p:=p.varptr
		switch p.tag
		when tint,tword then
			++p.value
		when trefvar then			!incr the pointer
			++p.varptr
		when trefpack then			!incr the pointer
			p.uref.ptr+:=ttsize[p.uref.elemtag]
		when treal then
			p.xvalue+:=1
		else
			pcustype("incrptr/refvar",p)
		endswitch
	when trefpack then			!incr the packed type pointed to
		switch p.uref.elemtag
		when tpu8,tpi8 then
			++(p.uref.ptr)^
		else
			pcustype_t("incrptr/ref",p.uref.elemtag)
		endswitch

	else
		pcustype("incrptr",p)
	endswitch
	++pcptr
end

proc k_decrptr=
	variant p

	p:=sptr++

	switch p.tag
	when trefvar then			!increment what ptr points to
		p:=p.varptr
		switch p.tag
		when tint,tword then
			--p.value
		when trefvar then			!incr the pointer
			--p.varptr
		when trefpack then			!incr the pointer
			p.uref.ptr-:=ttsize[p.uref.elemtag]
		when treal then
			p.xvalue-:=1
		else
			pcustype("incrptr/refvar",p)
		endswitch
	when trefpack then			!incr the packed type pointed to
		switch p.uref.elemtag
		when tpu8,tpi8 then
			--(p.uref.ptr)^
		else
			pcustype_t("incrptr/ref",p.uref.elemtag)
		endswitch

	else
		pcustype("incrptr",p)
	endswitch
	++pcptr
end

!
!proc k_pushlabel=
!	(--sptr)^:=label_make(cast(getopnda))
!	skip(1)
!end

proc k_pushvoid=
	--sptr
	sptr.tagx:=tvoid
	++pcptr
end

proc k_callproc=
	const countinterval=10
	static int count=countinterval

!	if --count=0 then
!		count:=countinterval
!		os_peek()
!	fi

	if sptr<=stacklimit then
		pcerror("Stack Overflow")
	fi

	--sptr
	sptr.tagx:=treturn
	sptr^.uret.retaddr := pcptr+3

	sptr^.uret.frameptr_low := word32@(frameptr)
!	sptr^.uret.stackadj :=getopndb
	frameptr:=cast(sptr)

	pcptr:=cast(getopnda)

end

proc k_callptr=
	symbol d

	if sptr.tag<>tsymbol then
		pcustype("callptr not symbol",sptr)
	fi
	d:=sptr.def

!check. no. of params
!....

	sptr.tagx:=treturn
	sptr^.uret.retaddr := pcptr+3

	sptr^.uret.frameptr_low := word32@(frameptr)
!	sptr^.uret.stackadj :=getopndb
	frameptr:=cast(sptr)

	pcptr:=cast(d.pcaddress)
end

!proc k_callfn=
!!PCERROR("CALLFN")
!	k_callproc()
!end

global proc k_procentry =
	to getopnda do
		--sptr
		sptr.tagx:=tvoid
		sptr.value:=0
	od
	skip(1)
end

proc k_return=
	if sptr.tag<>treturn then
		pcerror_s("Not treturn:",ttname[sptr.tag])
	fi
	sptr.tagx:=0
	pcptr:=sptr.uret.retaddr

	(ref int32(&frameptr))^:=sptr.uret.frameptr_low

!	sptr:=variant((ref byte(sptr)+sptr^.uret.stackadj))
	++sptr

end

proc k_unshare=
!CPL "UNSHARE",GETOPNDA
	to getopnda do
!!CPL "//KUNSHARE"
		var_unshare(sptr)
		++sptr
	od
	skip(1)
end

proc k_formci=
	variant p

	p:=cast(getopndb)

	++p.value

	if p.value<=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

proc k_forfci=
	variant p

	p:=cast(frameptr+getopndb)

	++p.value

	if p.value<=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

proc k_fordmci=
	variant p

	p:=cast(getopndb)

	--p.value

	if p.value>=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

proc k_fordfci=
	variant p

	p:=cast(frameptr+getopndb)

	--p.value

	if p.value>=getopndc then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

proc k_formm=
	variant p,q

	p:=cast(getopndb)
	q:=cast(getopndc)

	++p.value

!CPL "FORMM",P.VALUE

	if p.value<=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

proc k_forff=
	variant p,q

	p:=cast(frameptr+getopndb)
	q:=cast(frameptr+getopndc)

	++p.value

	if p.value<=q.value then
		pcptr:=cast(getopnda)
	else
		skip(3)
	fi
end

proc k_comment=
!	println "Comment:", ichar(getopnda)
	skip(1)
end

proc k_makelist=
	variant x,y
	int n

	n:=getopnda

	x:=sptr				!start of data
	sptr+:=n-1

	var_make_list(x,sptr,n,getopndb)
	sptr.objptr.mutable:=0

	skip(2)
end

proc k_makedict=
	variant x
	int n

	n:=getopnda

	x:=sptr+n*2-1			!start of data

	var_make_dict(sptr,x,n)
	sptr:=x

	skip(1)
end

proc k_makeset=
	variant x
	int n

	n:=getopnda

	x:=sptr+n-1			!start of data

	var_make_set(sptr,x,n)
	sptr:=x
	sptr.objptr.mutable:=0

	skip(1)
end

proc k_makerecord=
	variant x,y
	int n

	n:=getopnda

	x:=sptr				!start of data
	sptr+:=n-1

	var_make_record(x,sptr,n,getopndb)
	sptr.objptr.mutable:=0

	skip(2)
end

proc k_makestruct=
	variant x,y
	int n

	n:=getopnda

	x:=sptr				!start of data
	sptr+:=n-1

	var_make_struct(x,sptr,n,getopndb)
	sptr.objptr.mutable:=0

	skip(2)
end

proc k_makearray=
	variant x
	int n

	n:=getopndb

	x:=sptr				!start of data
	sptr+:=n-1

	var_make_array(x,sptr,getopnda, n, getopndc, getopndd)
	sptr.objptr.mutable:=0

	skip(4)
end

proc k_makebits=
	variant x
	int n

	n:=getopndb

	x:=sptr				!start of data
	sptr+:=n-1

	var_make_bits(x,sptr,getopnda, n, getopndc, getopndd)
	sptr.objptr.mutable:=0

	skip(4)
end

proc k_index=
!x[y]
	variant y,z
	varrec x

	y:=sptr++
	x:=sptr^

	case y.tag
	when tint then
		var_getix(sptr,y.value)
	when trange then
		var_getslice(sptr,y.range_lower,y.range_upper)
	else
		pcmxtypes("Index",&x,y)
	esac

	var_unshare(&x)

	++pcptr
end

proc k_popindex=
!y[z]:=x
	variant x,y,z

	z:=sptr++		!index
	y:=sptr++		!list etc
	x:=sptr++		!value to store

	case z.tag
	when tint then
		var_putix(y, z.value, x)
		var_unshare(y)
	when trange then
		var_putslice(y, z.range_lower, z.range_upper, x)
		var_unshare(x)
		var_unshare(y)
	else
		pcmxtypes("Popindex",y,z)
	esac


	++pcptr
end

proc k_indexref=
!&x[y]
	variant y,p
	varrec x

	y:=sptr++
	x:=sptr^

	case y.tag
	when tint then
		var_getixref(sptr, y.value)
	else
		pcmxtypes("Indexref",sptr,y)
	esac

	var_unshare(&x)
	++pcptr
end

proc k_keyindex=
!x{y}
	variant d,k,p,def

	def:=sptr++			!def is any default value to be used
	k:=sptr++			!k is the key
	d:=sptr				!d is the dict

	if d^.tag<>tdict then
		pcustype("dict{}",d)
	fi

	p:=var_finddictitem(d,k,0)
	var_unshare(d)
	var_unshare(k)

	if p then			!found
		sptr^:=p^
		var_unshare(def)
	else
		sptr^:=def^			!use given default value when not found
	fi
	++pcptr
end

proc k_popkeyindex=
!y[z]:=x
	variant d,k,p,x

	k:=sptr++			!k is the key
	d:=sptr++			!d is the dict
	x:=sptr++			!value to pop

	if d.tag<>tdict then
		pcustype("dict{}:=",d)
	fi

	p:=var_finddictitem(d,k,1)

	if p.tag<>tvoid then
		var_unshare(p)
	fi
	p^:=x^

	var_unshare(d)
	var_unshare(k)

	++pcptr
end

proc k_keyindexref=
!y[z]:=x
	variant d,k,p,x

CPL "KEYIXREF"

	k:=sptr++			!k is the key
	d:=sptr				!d is the dict

	if d.tag<>tdict then
		pcustype("&dict{}",d)
	fi

	p:=var_finddictitem(d,k,0)
	if p=nil then
		pcerror("&dict{} not found")
	fi
	var_share(p)
	var_unshare(k)
	var_unshare(d)

	sptr.tagx:=trefvar
	sptr.varptr:=p

	++pcptr
end

proc k_dotindex=
!x.[y]
	variant y,z
	varrec x

	y:=sptr++
	x:=sptr^

	case y.tag
	when tint then
		var_getdotix(sptr,y.value)
	when trange then
		var_getdotslice(sptr,y.range_lower,y.range_upper)
	else
		pcmxtypes("Dotindex",&x,y)
	esac

	var_unshare(&x)

	++pcptr
end

proc k_dotindexref=
!x.[y]
	variant y,p
	varrec x

	y:=sptr++
	x:=sptr^

	case y.tag
	when tint then
		var_getdotixref(sptr, y.value)
	when trange then
		var_getdotsliceref(sptr, y.range_lower,y.range_upper)
	else
		pcmxtypes("Dotindexref",sptr,y)
	esac

	var_unshare(&x)
	++pcptr
end

proc k_popdotindex=
!y[z]:=x
	variant x,y,z,py

	z:=sptr++		!index
	y:=sptr++		!ref to int, string etc
	x:=sptr++		!value to store

	case z.tag
	when tint then
		var_putdotix(y, z.value, x)
		var_unshare(y)
	when trange then
		var_putdotslice(y, z.range_lower, z.range_upper, x)
		var_unshare(x)
		var_unshare(y)
	else
		pcmxtypes("Popdotindex",y,z)
	esac


	++pcptr
end

proc k_len=
	variant x:=sptr
	object p:=x.objptr
	int n

!CPL =STRMODE(TTBASETYPE[X.TAG])

	case x.tag
	when tlist,trecord,tarray,tparray,tdict,tbits then
		n:=p.ulist.length
	when tstring then
		n:=p.ustr.length
	when trecord, tcarray, tstruct then
		n:=ttlength[x.usertag]
	when tset then
		n:=p.uset.length
	else
		pcustype("Len",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

proc k_upb=
	variant x:=sptr
	object p:=x.objptr
	int n

	case x.tag
	when tlist,tdict then
		n:=p.ulist.length+p.ulist.lower-1
	when tstring then
		n:=p.ustr.length
	when tarray then
		n:=p.uarray.length+p.uarray.lower-1
	when tbits then
		n:=p.ubits.length+p.ubits.lower-1
	when trecord, tstruct then
		n:=ttlength[x.usertag]

	when tcarray then
		n:=ttlength[x.usertag]+ttlower[x.usertag]-1

	when tset then
		n:=p.uset.length-1

	else
		pcustype("Upb",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

proc k_lwb=
	variant x:=sptr
	object p:=x.objptr
	int n

	case x.tag
	when tlist then
		n:=p.ulist.lower
	when tstring,tdict then
		n:=1
	when tarray then
		n:=p.uarray.lower
	when tbits then
		n:=p.ubits.lower
	when trecord,tstruct then
		n:=1
	when tcarray then
		n:=ttlower[x.usertag]
	when tset then
		n:=0
	else
		pcustype("Lwb",x)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

proc k_bounds=
	int a,b,m
	object p

	m:=sptr.tag
	p:=sptr.objptr

	case m
	when tlist,tdict then
		a:=p.ulist.lower
		b:=p.ulist.length+a-1
	when tarray then
		a:=p.uarray.lower
		b:=p.uarray.length+a-1
	when tbits then
		a:=p.ubits.lower
		b:=p.ubits.length+a-1
	when tstring then
		a:=1
		b:=p.ustr.length
	when trange then
		a:=sptr.range_lower
		b:=sptr.range_upper
	when tstruct,trecord then
		a:=1
		b:=ttlength[sptr.usertag]
	when tcarray then
		a:=ttlower[sptr.usertag]
		b:=ttlength[sptr.usertag]

	when tset then
		a:=0
		b:=p.uset.length-1

	else
		pcustype("Bounds",sptr)
	esac

	var_unshare(sptr)
	sptr.tagx:=trange
	sptr.range_lower:=a
	sptr.range_upper:=b

	++pcptr
end

proc k_boundsx=
	int a,b,m
	object p

	m:=sptr.tag
	p:=sptr.objptr

	case m
	when tlist,tdict then
		a:=p.ulist.lower
		b:=p.ulist.length+a-1
	when tarray then
		a:=p.uarray.lower
		b:=p.uarray.length+a-1

	when tbits then
		a:=p.ubits.lower
		b:=p.ubits.length+a-1

	when tstring then
		a:=1
		b:=p.ustr.length
	when trange then
		a:=sptr.range_lower
		b:=sptr.range_upper
	when tstruct,trecord then
		a:=1
		b:=ttlength[sptr.usertag]
	when tcarray then
		a:=ttlower[sptr.usertag]
		b:=ttlength[sptr.usertag]
	when tset then
		a:=0
		b:=p.uset.length-1

	else
		pcustype("Boundsx",sptr)
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=a
	--sptr
	sptr.tagx:=tint
	sptr.value:=b

	++pcptr
end

proc k_dictitems=
	int n

	if sptr.tag<>tdict then
		pcerror("Not a dict")
	fi
	n:=sptr.objptr.udict.dictitems
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

proc k_append=
	variant y:=sptr++
	varrec x:=sptr^

	var_append(sptr,y)
	var_unshare(&x)
!	var_unshare(y)

	++pcptr
end

proc k_concat=
	variant y:=sptr++
	varrec x:=sptr^

	var_concat(sptr,y)

	var_unshare(&x)
	var_unshare(y)

	++pcptr
!!	variant x,y
!	varrec z
!	int f
!
!	y:=sptr
!	x:=sptr+1
!
!	++sptr
!
!	if x.tag<>y.tag and not var_mixed(x,y,f) then
!		pcmxtypes("concat",x,y)
!
!	else
!		z:=x^
!		case x.tag
!		when tstring then
!			var_add_string(x,y)
!			var_unshare(&z)
!			var_unshare(y)
!		when tlist then
!			var_dupl_list(x)
!			var_concatto_list(x,y)
!			var_unshare(&z)
!			var_unshare(y)
!		else
!			pcustype("Concat",x)
!		esac
!	fi
!
!	++pcptr
end

proc k_appendto=
!x append:= y
	variant px,x,y

	y:=sptr++
	px:=sptr++

	case px.tag
	when trefvar then
		var_appendto(px.varptr,y)
	else
		pcustype("Appendto",px)
	esac
	++pcptr
end

proc k_concatto=
!x append:= y
	variant px,x,y

	y:=sptr++
	px:=sptr++

	case px.tag
	when trefvar then
		var_concatto(px.varptr,y)
		var_unshare(y)
	else
		pcustype("Concatto",px)
	esac
	++pcptr
end

proc k_addto=
!x +:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_addto(px, y) then
		var_inplace(px,y, cast(var_add), cast(var_addmixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_subto=
!x -:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_subto(px, y) then
		var_inplace(px,y, cast(var_sub), cast(var_submixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_multo=
!x -:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_multo(px, y) then
		var_inplace(px,y, cast(var_mul), cast(var_mulmixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_iandto=
!px^ iand:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_iandto(px, y) then
		var_inplace(px,y, cast(var_iand), cast(var_iandmixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_iorto=
!px^ ior:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_iorto(px, y) then
		var_inplace(px,y, cast(var_ior), cast(var_iormixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_ixorto=
!px^ ixor:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_ixorto(px, y) then
		var_inplace(px,y, cast(var_ixor), cast(var_ixormixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_shlto=
!x <<:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_shlto(px, y) then
		var_inplace(px,y, cast(var_shl), cast(var_shlmixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_shrto=
!x >>:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_shrto(px, y) then
		var_inplace(px,y, cast(var_shr), cast(var_shrmixed))
	end

	var_unshare(y)
	++pcptr
end

proc k_copy=
	varrec x

	if sptr.hasref then
		x:=sptr^
		var_duplu(sptr)
		var_unshareu(&x)
	fi

	++pcptr
end

proc k_dupl=
	--sptr
	sptr^:=(sptr+1)^
	var_share(sptr)
	++pcptr
end

proc k_makerange=
	variant x,y

	y:=sptr++
	x:=sptr

	if x.tag<>tint or y.tag<>tint then
		pcerror("makerange/not int")
	fi

	sptr.tagx:=trange
	sptr.range_upper:=y.value
	sptr.range_lower:=x.value

	++pcptr
end

proc k_makerangelen=
	variant x,y

	y:=sptr++
	x:=sptr

	if x.tag<>tint or y.tag<>tint then
		pcerror("makerangelen/not int")
	fi

	sptr.tagx:=trange
	sptr.range_upper:=x.value+y.value-1
	sptr.range_lower:=x.value

	++pcptr
end

proc k_makedecimal=
	varrec x
	object p

	x:=sptr^

	if x.tag<>tstring then pcerror("Not str") fi
	p:=x.objptr
	if p.ustr.length=0 then pcerror("Null str") fi

	var_make_decimal(p.ustr.strptr, p.ustr.length, sptr)

	var_unshare(&x)

	++pcptr
end

proc k_dot=
	symbol d
	variant p
	ref byte q
	int rectype
	varrec v

	rectype:=sptr.usertag

	d:=resolvefield(getopnda, rectype)

	case d.nameid
	when fieldid then
		p:=sptr.objptr.urec.varptr+d.fieldoffset/varsize
		var_share(p)
		var_unshare(sptr)
		sptr^:=p^

	when structfieldid then
		var_loadpacked(sptr.objptr.ustruct.ptr+d.fieldoffset, d.mode, &v, nil)
		var_unshare(sptr)
		sptr^:=v

	else
		pcerror_s("DOT: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

proc k_dotref=
	symbol d
	variant p
	ref byte q
	int rectype

	rectype:=sptr.usertag

	d:=resolvefield(getopnda, rectype)

	case d.nameid
	when fieldid then
		p:=sptr.objptr.urec.varptr+d.fieldoffset/varsize
!Possible bug when sptr is a transient value which is now freed
!But you wouldn't normally use as an lvalue
		var_unshare(sptr)

		sptr.tagx:=trefvar
		sptr.varptr:=P

	when structfieldid then
!		var_loadpacked(sptr.objptr.ustruct.ptr+d.fieldoffset, d.mode, &v, nil)
		q:=sptr.objptr.ustruct.ptr+d.fieldoffset
		var_unshare(sptr)
		sptr.tagx:=trefpack
		sptr.uref.ptr:=q
		sptr.uref.elemtag:=d.mode

	else
		pcerror_s("DOTREF: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

proc k_popdot=
	symbol d
	variant p,x,y

	x:=sptr++
	y:=sptr++

	d:=resolvefield(getopnda, x.usertag)

	IF NOT X.HASREF THEN PCERROR("POPDOT") FI

	if not x.objptr.mutable then
		pcerror("Not mutable")
	fi

	case d.nameid
	when fieldid then
		p:=x.objptr.urec.varptr+d.fieldoffset/varsize
		var_unshare(p)
		p^:=y^				!transfer
		var_unshare(x)

	when structfieldid then
!		var_loadpacked(sptr.objptr.ustruct.ptr+d.fieldoffset, d.mode, &v, nil)
		var_storepacked(x.objptr.ustruct.ptr+d.fieldoffset, y, d.mode)
		var_unshare(x)

	else
		pcerror_s("POPDOT: can't do this fieldtype:",namenames[d.nameid])
	esac

	skip(1)
end

function resolvefield(int index, rectype)symbol d=
!index is a start point in the genfieldtable
!scan the linked list looking for a field/structfield/method etc whose
!owner type matches rectype
	ref genfieldrec g

!CPL "RF1",=INDEX,=STRMODE(RECTYPE)
	if index=0 then pcerror("Not a field") fi

	g:=genfieldtable[index]

	while g do
		d:=g.def
		if d.owner.mode=rectype then return d fi
!		CPL =G,=D.NAME,NAMENAMES[D.NAMEID],d.fieldoffset,=D.OWNER.MODE,=RECTYPE

		g:=g.nextdef
	od

	pcerror_s("Can't resolve field:",d.name)
!CPL "RF2"
	return nil
end

proc k_pushptr=
	variant p

	p:=sptr

	case p.tag
	when trefvar then
		sptr^:=p.varptr^

	when trefpack then
		var_loadpacked(p.uref.ptr,p.uref.elemtag, sptr, nil)

	when trefbit then
		var_loadbit(p.uref.ptr, p.uref.bitoffset, p.uref.elemtag, p.uref.bitlength, sptr)

!	when tsymbol then

	else
		pcustype("Pushptr",p)
	esac

	var_share(sptr)

	++pcptr	
end

proc k_popptr=
	variant p,x,y

	p:=sptr++
	x:=sptr++

	case p.tag
	when trefvar then
		var_unshare(p.varptr)
		p.varptr^:=x^
	when trefpack then
		var_storepacked(p.uref.ptr,x,p.uref.elemtag)
	when trefbit then
		var_storebit(p.uref.ptr, p.uref.bitoffset, x, p.uref.elemtag, p.uref.bitlength)

!global proc var_storebit(ref byte p,int shift,variant q,int t,bitlength) =


	else
		pcustype("Popptr",p)
	esac

	++pcptr	
end
!
proc k_islist=
	istype(tlist)
end

proc k_isstring=
	istype(tstring)
end

proc k_isrecord=
	istype(trecord)
end

proc k_swap=
	[1024]byte tempbuffer
	variant x,y
	varrec v
	int xt,yt,s,t,n
	ref byte p,q
	int a

	x:=sptr++
	y:=sptr++

	if x.tag<>y.tag then
		pcerror("Swap mismatch")
	fi

	case x.tag
	when trefvar then
		v:=x.varptr^
		x.varptr^:=y.varptr^
		y.varptr^:=v

!		swap(x.varptr^, y.varptr^)		!block swaps not implemented!
	when trefpack then
		s:=x.uref.elemtag
		t:=y.uref.elemtag
!	if x^.uref.elemtag=y^.uref.elemtag=tu8 then
		if s<>t then goto swaperror fi
		n:=ttsize[s]
		case n
		when 1 then
			p:=x.uref.ptr
			q:=y.uref.ptr
			a:=p^
			p^:=q^
			q^:=a
		elsif ttsize[s]<=tempbuffer.bytes then
			memcpy(&tempbuffer,x.uref.ptr,n)
			memcpy(x.uref.ptr,y.uref.ptr,n)
			memcpy(y.uref.ptr,&tempbuffer,n)
		else
			goto swaperror
		esac

	else
swaperror::
		pcmxtypes("Swap",x,y)
	esac

	++pcptr
end

proc k_jumptesteq=
!jump to L when x=y
! x<>y: keep x on the stack, skip
! x=y:  pop both jump
	variant x,y
	int xt,yt,res

!CPL "TESTEQ"

	y:=sptr++
	x:=sptr
	xt:=x.tag
	yt:=y.tag

	if xt<>yt then
		case pr(xt,yt)
		when pr(tint,trange) then
!CPL "EQ: INT/SET"
			if x.value not in y.range_lower..y.range_upper then
!CPL "NOT IN RANGE"
				skip(1)
				return
			fi
		when pr(tint,tset) then
			PCERROR("TESTEQ/INT/SET")

		esac
		var_unshare(x)
		var_unshare(y)
		++sptr
		pcptr:=cast(getopnda)
		return
	fi

	res:=var_equal(x,y)
!CPL "EQ: X/Y",=RES
	var_unshare(y)
	if res then
		var_unshare(x)
		++sptr
		pcptr:=cast(getopnda)
		return
	fi

	skip(1)
end

proc k_jumptestne=
!jump to L when x=y
! x<>y: keep x on the stack, skip
! x=y:  pop both jump
	variant x,y
	int xt,yt,res

	y:=sptr++
	x:=sptr
	xt:=x.tag
	yt:=y.tag

	if xt<>yt then
		case pr(xt,yt)
		when pr(tint,trange) then
			if x.value in y.range_lower..y.range_upper then
				++sptr
				skip(1)
				return
			fi
		when pr(tint,tset) then
			PCERROR("TESTNE/INT/SET")
		esac

		var_unshare(y)
!		++sptr
		pcptr:=cast(getopnda)
		return

	fi

	res:=var_equal(x,y)
	var_unshare(y)
	if not res then
		pcptr:=cast(getopnda)
		return
	fi
	var_unshare(x)
	++sptr

	skip(1)
end

proc k_jump=
	pcptr:=cast(getopnda)
end

proc k_jumpptr=
	symbol d

	if sptr.tag<>tsymbol then
		pcerror("symbol expected")
	fi
	d:=cast(sptr.def)
	--sptr
	if d.nameid<>labelid then
		pcerror("label expected")
	fi
!CPL =D.NAME,=D.PCADDRESS,=D.MODULENO, =D.PROCFIXED,=D.LABELNO
	if not d.procfixed then
		d.pcaddress:=moduletable[d.moduleno].pcstart+d.labelno
		d.procfixed:=1
	fi

	pcptr:=d.pcaddress
!	pcptr:=cast(getopnda)
end

proc k_decr=
	case sptr.tag
	when tint then
		--sptr.value
	else
		pcustype("decr",sptr)
	esac

	++pcptr
end

proc k_chr=
	var [8]char str

	if sptr.tag=tint then
		if sptr.uvalue>=128 then pcerror("chr:not ASCII") fi
		str[1]:=sptr.value
		str[2]:=0
		var_make_stringn(&.str,1,sptr,1)
	else
		pcustype("CHR",sptr)
	fi
	++pcptr
end

proc k_asc=
	int c

	if sptr.tag=tstring then
		if sptr.objptr.ustr.length then
			c:=sptr^.objptr.ustr.strptr^
		else
			c:=0
		fi
		var_unshareu(sptr)
		sptr.tagx:=tint
		sptr.value:=c
	else
		pcustype("ASC",sptr)
	fi

	++pcptr
end

proc k_pusht=
	--sptr
	sptr.tagx:=ttype
	sptr.value:=getopnda
	skip(1)
end

proc k_type=
	int t:=sptr.tag
	case t
	when trecord, tstruct, tcarray then
		t:=sptr.usertag
	esac

	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

proc k_basetype=
	int t:=sptr.tag
	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

proc k_elemtype=
	int t:=sptr.tag

	case t
	when tarray,tbits,tcarray then
		t:=sptr.objptr.uarray.elemtype
	when trefpack, trefvar, trefbit then
		t:=sptr.uref.elemtag
	when tset then
		t:=tpu1
	else
		pcerror("elemtype")
	esac

	var_unshare(sptr)
	sptr.tagx:=ttype
	sptr.value:=t
	++pcptr
end

proc k_nop={++pcptr}

!proc k_makereftype=
!	sptr.value:=makereftype(sptr.value)
!
!	++pcptr
!end

proc k_modulecall=
	symbol d:=cast(getopnda)
	int moduleno:=d.moduleno

!CPL "MDULECALL",D.NAME,D.PCADDRESS,D.MODULENO,MODULETABLE[MODULENO].PCSTART
	--sptr
	sptr.tagx:=treturn
	sptr^.uret.retaddr := pcptr+2

	pcptr:=moduletable[moduleno].pcstart

!
!	skip(1)
end

proc k_modulereturn=
	pcptr:=sptr.uret.retaddr
	++sptr
end

proc k_maxvalue=
	int64 a

	if sptr.tag=ttype then sptr.tag:=sptr.value fi

	case sptr.tag
	when tpu8 then a:=255
	when tpu16 then a:=65536
	when tpu32 then a:=0xFFFF'FFFF
	when tpu64,tword then a:=0xFFFF'FFFF'FFFF'FFFF
	when tpi8 then a:=127
	when tpi16 then a:=32767
	when tpi32 then a:=0x7FFF'FFFF
	when tpi64,tint then a:=0x7FFF'FFFF'FFFF'FFFF
	else
		pcustype("MAXVALUE",sptr)
	esac
	sptr.tagx:=tint
	sptr.value:=a

	++pcptr

end

proc k_minvalue=
	int64 a

	if sptr.tag=ttype then sptr.tag:=sptr.value fi

	case sptr.tag
	when tword,tpu8,tpu16,tpu32,tpu64 then a:=0
	when tpi8 then a:=-128
	when tpi16 then a:=-32768
	when tpi32 then a:=-0x8000'0000
	when tint,tpi64 then a:=-0x8000'0000'0000'0000
!	when tbignum then a:=-0x8000'0000'0000'0000
	else
		pcustype("MINVALUE",sptr)
	esac
	sptr^.tagx:=tint
	sptr^.value:=a

	++pcptr
end

proc k_callhost=
	callhostfunction(getopnda)
	skip(1)
end

proc k_expand=
	variant dest
	int n
	
	n:=getopnda
	dest:=sptr-n+1

	var_expand(sptr,dest,n)
	sptr:=dest

	skip(1)
end

!proc k_pushproc=
!	symbol d:=cast(getopnda)
!	--sptr
!	sptr.tagx:=tsymbol
!	sptr.def:=cast(getopnda)
!!
!	skip(1)
!end
!
proc k_pushsymbol=
	symbol d:=cast(getopnda)
	--sptr
	sptr.tagx:=tsymbol
	sptr.def:=cast(getopnda)
!
	skip(1)
end

proc k_eq=
	variant x,y
	int res

	y:=sptr
	x:=++sptr

	res:=var_equal(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

proc k_ne=
	variant x,y
	int res

	y:=sptr
	x:=++sptr

	res:=not var_equal(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

proc k_lt={do_cmp(klt)}
proc k_le={do_cmp(kle)}
proc k_ge={do_cmp(kge)}
proc k_gt={do_cmp(kgt)}

proc do_cmp(int opc)=
	variant x,y
	int res

	y:=sptr
	x:=++sptr

	res:=var_compare(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint

	case opc
	when klt then sptr.value:=res<0
	when kle then sptr.value:=res<=0
	when kge then sptr.value:=res>=0
	else sptr.value:=res>0
	esac

	++pcptr
end

proc k_calldll=
	symbol d:=cast(getopnda)
	int nargs:=getopndb, restype:=getopndc
	variant p

!CPL =NARGS,=RESTYPE
!CPL D.NAME,=D.INDEX
!
!CPL "K/CALLDLL",D.NAME,=NARGS,=TTNAME[RESTYPE]

!CPL "CALLDLL1",SPTR,nargs
	calldll(d, sptr, sptr+nargs, nargs)

	p:=sptr
	to nargs do
!		var_unshare(p)
		++p
	od


!CPL "CALLDLL2",SPTR
!	PCERROR("CALLDLL NOT READY")

	skip(3)

end

proc k_in=
	variant x,y
	int n

	y:=sptr
	x:=++sptr

	n:=var_in(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

proc k_notin=
	variant x,y
	int n

	y:=sptr
	x:=++sptr

	n:=not var_in(x,y)
	var_unshare(x)
	var_unshare(y)

	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

proc k_convrefpack =
	variant a
	int tag,elemtype
	ref void p
	object pa

	switch sptr.tag
	when trefvar then
		a:=sptr.varptr
		pa:=a.objptr
		switch a.tag
		when tint,tword then
			p:=&a.value
			elemtype:=tpi64
		when treal then
			p:=&a.value
			elemtype:=tpr64
		when tarray then
			p:=pa.uarray.ptr
			elemtype:=pa.uarray.elemtype
		when tbits then
			sptr.uref.ptr:=pa.ubits.ptr
			sptr.uref.bitoffset:=pa.ubits.indexoffset*ttbitwidth[pa.ubits.elemtype]
			sptr.uref.bitlength:=0
			sptr.tagx:=trefbit
			sptr.uref.elemtag:=pa.ubits.elemtype
			++pcptr
			return

		when tset then
			sptr.uref.ptr:=pa.uset.ptr
			sptr.uref.bitoffset:=0
			sptr.uref.bitlength:=0
			sptr.tagx:=trefbit
			sptr.uref.elemtag:=tpu1
			++pcptr
			return

		when tstring then
			p:=pa.ustr.strptr
			elemtype:=tpu8
			if p=nil then
				p:=""
!			pcerror("&null str")
			fi
		when tstruct then
			p:=pa.ustruct.ptr
			elemtype:=a.usertag
		else
!			if a.hasref then
!				p:=pa
!				elemtype:=tvoid
!				goto done
!			fi
			pcustype("Getrefpack1",a)
		end switch
	when trefpack,trefbit then
		++pcptr
		return

	else
		pcustype("Getrefpack2",sptr)
	endswitch
done::

	sptr.tagx:=trefpack
	sptr.uref.ptr:=p
	sptr.uref.elemtag:=elemtype

	++pcptr
end

proc k_isdef=
	int res:=sptr.tag<>tvoid
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

proc k_isvoid=
	int res:=sptr.tag=tvoid
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

proc k_isint=
	istype(tint, tword)
end

proc k_isset=
	istype(tset)
end

proc istype(int t1, t2=tvoid)=
!replace tos with 1 when tos has type t1 or, when t2 is not 0, t2; else tos:=0
	int res, t:=sptr.tag

	res:=t=t1
	if not res and t2 then
		res:=t=t2
	fi
	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=res
	++pcptr
end

proc k_convert=
	varrec x
	int t

	t:=getopnda

	if sptr.tag<>t then
		x:=sptr^
		var_convert(&x,t,sptr)
		var_unshare(&x)
	fi
!
!	CPL =TTNAME[T],TTNAME[SPTR.TAG]
!	PCERROR("CONV")

	skip(1)
end

proc k_switch=
	int index,n,lower

	n:=getopnda
	lower:=getopndb

	case sptr.tag
	when tint,ttype then
	else
	CPL ttname[sptr^.tag]
		pcerror("switch not int")
	esac
	index:=sptr.value-lower		!now 0-based index
	++sptr

	if u32(index)>=u32(n) then			!out of range
		pcptr:=ref int((pcptr+n*2+4)^)
	else					!in range
		pcptr:=ref int((pcptr+index*2+4)^) 	!+3 for sw cmd + 1 to label part of (kjumptable,label) pair
	fi
end

proc k_bytesize=
	int n
	object p:=sptr.objptr

	case sptr.tag
	when tarray then
		n:=p.uarray.length*ttsize[p.uarray.elemtype]
	when tset then
		n:=getbitssize(p.uset.length,tpu1)
	when tstring then
		n:=p.ustr.length
	when tbits then
		n:=bits_bytesize(p)
	when tlist then
		n:=p.ulist.length*varsize
	when trecord, tstruct, tcarray then
		n:=ttsize[sptr.usertag]	
	else
		n:=ttsize[sptr.tag]
	esac

	var_unshare(sptr)
	sptr.tagx:=tint
	sptr.value:=n

	++pcptr
end

proc k_min=
	variant x,y

	y:=sptr++
	x:=sptr

!	if x.tag=y.tag=tint then
!		if y.value<x.value then
!			sptr.value:=y.value
!		fi
!	elsif var_compare(x,y)<0 then		!x is smaller
	if var_compare(x,y)<0 then		!x is smaller
		var_unshare(y)
	else
		var_unshare(x)
		sptr^:=y^
	fi

	++pcptr
end


proc k_max=
	variant x,y

	y:=sptr++
	x:=sptr

!	if x.tag=y.tag=tint then
!		if y.value>x.value then
!			sptr.value:=y.value
!		fi
!	elsif var_compare(x,y)<0 then		!x is smaller
	if var_compare(x,y)>=0 then		!x is bigger
		var_unshare(y)
	else
		var_unshare(x)
		sptr^:=y^
	fi

	++pcptr
end

proc k_addsp=
!CPL "ADDSP"
	sptr+:=getopnda
	skip(1)
end

proc k_pushtry=
!CPL "KPUSHTRY"
	(--sptr)^.tagx:=texception
	sptr.refptr:=ref byte(getopnda)
	sptr.uexcept.frameoffset:=frameptr-ref byte(sptr)		!byte offset
	sptr.uexcept.exceptiontype:=getopndb
	sptr.uexcept.nexceptions:=getopndc
	skip(3)
end

proc k_raise=

!CPL "KRAISE"
!CPL =TTNAME[SPTR.TAG]

	if sptr.tag<>tint then
		pcerror("Raise: not Int on stack [not proceeding direct to RAISE]")
	fi
	pcptr:=raiseexception(sptr.value)				!will unwind stack and set pcptr to address of exception code
end

proc k_isequal=
	variant x,y
	int res

	y:=sptr++
	x:=sptr

	if x.hasref and y.hasref and x.objptr=y.objptr then
		res:=1
	else
		res:=0
	fi

	var_unshare(x)
	var_unshare(y)
	sptr.tagx:=tint
	sptr.value:=res

	++pcptr
end

!function var_equal(variant a,b)int=
!	int f
!
!	if a.tag=b.tag then
!		return var_equal(a,b)
!	else
!		return var_equalmixed(a,b)
!	fi
!end
!
!function var_compare(variant a,b)int=
!	if a.tag=b.tag then
!		return var_compare(a,b)
!	else
!		return var_comparemixed(a,b)
!	fi
!end

proc k_minto=
!x min:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_minto(px, y) then
		var_inplace(px,y, cast(var_min), cast(var_minmixed))
	end

	var_unshare(y)
	++pcptr
!	variant px,x,y
!	int f
!
!	y:=sptr++
!	px:=sptr++
!	x:=px.varptr
!
!	case px.tag
!	when trefvar then
!!		if x.tag<>y.tag and not var_mixed(x,y,f) then
!		if x.tag<>y.tag then
!			pcerror("refvar/minto/mixed")
!		fi
!		var_minto(x,y)
!		var_unshare(y)
!	else
!		pcustype("Minto",px)
!	esac
!
!	++pcptr
end

proc k_maxto=
!x max:= y
	variant y:=sptr++
	variant px:=sptr++

	if not var_maxto(px, y) then
		var_inplace(px,y, cast(var_max), cast(var_maxmixed))
	end

	var_unshare(y)
	++pcptr


!	variant px,x,y
!	int f
!
!	y:=sptr++
!	px:=sptr++
!	x:=px.varptr
!
!	case px.tag
!	when trefvar then
!!		if x.tag<>y.tag and not var_mixed(x,y,f) then
!		if x.tag<>y.tag then
!			pcerror("refvar/maxto/mixed")
!		fi
!		var_maxto(x,y)
!		var_unshare(y)
!	else
!		pcustype("Maxto",px)
!	esac
!
!	++pcptr
end

=== qq_names.m 15/32 ===
import* qq

global var [maxnames]symbol namemaptable			!convert name index to strec reference

!global const maxglobalprocs=12000
!global var [maxglobalprocs]symbol globalproctable
!global var int nglobalprocs

!vars for structdef

int sdsize, sdoffset
int sdaligned
int sdlevel
int sdmode
int sdnfields
int sdmaxalign
const int maxstructdepth=10
[maxstructdepth]byte sdunion		!1 if union model 0 for normal offset calc
[maxstructdepth]int sdmaxsize		!accumulate max size of union


proc $init=
!	globalhashtablelast:=&globalhashtable[hstsize-1]
end

!function lookup(ichar name)symbol=
!!lookup rawnamesym with details in nextlx
!!hash value already worked out in nextlx.hashvalue
!!return 1 (found) or 0 (not found)
!!in either case, nextlx.symptr set to entry where name was found, or will be stored in
!var int j,wrapped,length,hashvalue
!var symbol d
!var ref char s
!
!length:=strlen(name)
!hashvalue:=gethashvaluez(name)
!
!CPL "AGNLOOKUP",=HASHVALUE IAND HSTMASK
!
!d:=&globalhashtable[hashvalue iand hstmask]
!wrapped:=0
!
!do
!	case d.namelen
!	when 0 then
!		exit
!	when length then
!		if memcmp(d.name,name,length)=0 then	!match
!			return d
!		fi
!	esac
!
!	if ++d>globalhashtablelast then
!		if wrapped then
!			abortprogram("PC HASHTABLE FULL")
!		fi
!		wrapped:=1
!		d:=&globalhashtable[0]
!	fi
!od
!
!!exit when not found; new name will go in entry pointed to by lxsymptr
!
!CPL "NOT FOUND:",NAME,=D
!
!d.name:=name
!d.namelen:=length
!d.nameid:=genericid
!d.symbol:=rawnamesym
!if nameindex=nameindex.maxvalue then
!	lxerror("Too many names")
!fi
!d.index:=++nameindex
!
!return d
!end

global function addglobalname(ichar name)symbol=
!generic name encountered in namedef op. Convert to symbol reference
!will always return a generic strec, either existing, or just created
	var lexrec oldlx
	var symbol d

	oldlx:=nextlx

	lookup(name,strlen(name),gethashvaluez(name),0)

	d:=nextlx.symptr
	nextlx:=oldlx
	return d
end

function newstrec:symbol=
	var symbol p
	p:=pcm_alloc(strec.bytes)
	memset(p,0,strec.bytes)

!	p.moduleno:=currmoduleno
!	p.lineno:=lx.lineno
!	p.moduleno:=currmoduleno
	return p
end

global function addsymbol(symbol owner,d, int id, isglobal)symbol e=
!d should be a generic symbol (or if nil, then when name is provided
!to create a suitable generic symbol0
!create a dedicated strec for it, link it in to the dupl chain of the generic
!version, and insert it a child of owner
	symbol f

!CPL "ADDSYMBOL",D,D.NAME,NAMENAMES[ID]

!	if d=nil then
!!CPL "//ADDSYMBOL BY NAME",NAME
!		d:=addglobalname(name)
!	fi
!CPL "ADDSYMBOL",D.NAME,NAMENAMES[D.NAMEID], =D.NEXTDUPL,=d

!CPL "ADDSYMBOL",=ISGLOBAL

	e:=newstrec()
	e.name:=d.name
	e.namelen:=d.namelen
	e.owner:=owner
	e.nameid:=id
	e.isframe:=id=frameid or id=paramid

!CPL =STCURRPROC

	if currmodule then
		e.moduleno:=currmodule.moduleno
	fi

!CPL "SET MODULENO",D.NAME,=CURRMODULE,OWNER.NAME
!IF CURRMODULE THEN
!	CPL "	",=CURRMODULE.MODULENO,CURRMODULE.NAME
!FI
	e.firstdupl:=d
	e.isglobal:=isglobal

IF OWNER.NAMEID<>PROCID THEN
	e.nextdupl:=d.nextdupl

	d.nextdupl:=e
!CPL "ADDSYMBOL2",D.NAME,NAMENAMES[D.NAMEID], =D.NEXTDUPL,=D

	if e.nextdupl and e.nextdupl.owner=owner then
		cpl e.name,"in",owner.name
		serror("AS:Duplicate name")
	fi
else
	f:=owner.deflist
	while f do
		if f.firstdupl=e.firstdupl then
			cpl e.name,"in",owner.name
			serror("AS2:Duplicate name")
		fi
		f:=f.nextdef
	od
fi

	if owner.deflist=nil then			!first def
		owner.deflist:=e
	else
		owner.deflistx.nextdef:=e
	fi
	owner.deflistx:=e

	return e
end

global proc addproc(symbol d)=
	ref procrec p

	p:=pcm_allocz(procrec.bytes)
	p.def:=d

	if proclist=nil then
		proclist:=p
	else
		proclistx.nextproc:=p
	fi
	proclistx:=p
	++nproclist
end

global function createstroot(ichar name)symbol d=
	d:=newstrec()
	d.name:=pcm_copyheapstring(name)
	d.namelen:=strlen(name)
	d.nameid:=programid

	return d
end

global function newusertypex(ref strec d,e=nil)int=
	int i

!CPL "NEWUSERTYPE",D.NAME,=NUSERXTYPES,=NTYPES,NTYPES:"H"

	if nuserxtypes>=maxuserxtype then
		serror("Too many external user types")
	fi
	++nuserxtypes
	ttnamedefx[nuserxtypes]:=d
	ttnamedefx2[nuserxtypes]:=e

	ttxmoduleno[nuserxtypes]:=stcurrmodule.moduleno
	ttposx[nuserxtypes]:=lx.pos
!	ttowner[nuserxtypes]:=stcurrproc
	return -nuserxtypes
end

!global function getpushsymbol(ref symbol pd)symbol=
!!d is an strec encountered in the byte-code
!!if it is a generic name, then either resolve it, or generate a new symbol
!	var symbol d,e,owner
!	var object p
!
!!CPL "GETPUSH0",pd
!	d:=pd^
!
!!CPL "GETPUSH1",D.NAME
!	return d when d.nameid<>genericid
!!CPL "GETPUSH2",D.NAME,=STCURRRUNPROC,=D,=D.OWNER
!
!!	e:=resolvetopname(owner:=currrunproc,d,currmodule.moduleno,0)
!	e:=resolvetopname(owner:=stcurrrunproc,d,0)
!!CPL "GETPUSH3",E,NAMENAMES[E.NAMEID],E.OWNER
!
!	if e then
!		pd^:=e
!
!!		if (ref int(pd)-1)^=kpush_name and e.nameid in [frameid,paramid] then
!!CPL "GPS/PUSHNAME",E.NAME, E.OFFSET
!!		(ref int(pd)-1)^:=kpush_frame
!!		(ref int(pd))^:=e.offset
!!		
!!		fi
!
!		return e
!	fi
!
!	pcerror_s("push name: name not resolved:",d.name)
!	return d
!end

!global function getpopsymbol(ref symbol pd)symbol=
!!pd points to a csymbol reference in a pcl operand
!!if it is a generic name, then either resolve it, or generate a new symbol
!!will update the operand when it is generic
!	var symbol d,e,owner
!!	var object p
!
!	d:=pd^
!
!!CPL "GETPOPSYMBOL",D.NAME,=STCURRRUNPROC.NAME
!
!	return d when d.nameid<>genericid
!
!	e:=resolvetopname(owner:=stcurrrunproc,d,0)
!
!!CPL "GETPOP/RESOLVE",=E,=OWNER
!
!	if e then
!		pd^:=e
!		return e
!	fi
!!CPL "NOT RESOLVED"
!
!!not found: need to create a new name, which will be a variable
!
!!global function addsymbol(symbol owner,d, int id, ichar name=nil)symbol e=
!	case owner.nameid
!	when moduleid then
!!CPL "CREATE NEW STATIC"
!!		when 0, staticid then
!!CPL $LINENO
!			e:=addsymbol(owner,d,staticid)
!			e.mutable:=1
!!*!			e.varptr:=void_new()
!!		when constid then
!!			e:=addsymbol(owner,d,constid)
!!*!			e.varptr:=void_new()
!!		esac
!
!	when procid then				!create new frameid
!!CPL $LINENO
!		e:=addsymbol(owner,d,frameid)
!!CPL"HERE2"
!		e.mutable:=1
!		addlocalvar(e)
!
!	else
!
!CPL NAMENAMES[OWNER.NAMEID]
!CPL NAMENAMES[E.NAMEID]
!
!PCERROR("GETPOPSYMBOL: CAN'T CREATE FRAME VAR")
!	esac
!
!	pd^:=e
!	return e
!end

!global function getdefsymbol(symbol owner,ref symbol pd,int id,mutable=1,isglobal=0)symbol=
!!d is a symbol used in a -def opcode. Do special processing, which stops
!!a name being redefined
!	var symbol d,e
!	var object p
!
!
!	d:=pd^
!
!!CPL "GETDEFSYMBOL",D.NAME,=ISGLOBAL
!	if d.nameid<>genericid then
!		pcerror("REDEFINING DEF NAME")
!	fi
!
!!don't bother trying to resolve, as it should not be found. If it is, it will
!!either be in an outer scope, or if it is in this scope, the duplicate
!!will be detected below
!
!!CPL "GETDEFSYM",=OWNER.NAME, D.NAME,NAMENAMES[OWNER.NAMEID]
!
!!create new strec, but not attached object as that will be done by the caller
!	case owner.nameid
!	when moduleid then
!!CPL "DEF IN MODULE",NAMENAMES[ID]
!		if id=dllmoduleid then
!			e:=d.nextdupl
!			if e and e.owner=owner and e.nameid=dllmoduleid then
!				pd^:=e
!				return e
!			fi
!		fi
!!CPL $LINENO
!		e:=addsymbol(owner,d,id,isglobal:isglobal)
!!CPL "DONE ADDSYMBOL"
!		case id
!		when staticid then
!			e.mutable:=mutable
!!*!			e.varptr:=void_new()
!		when constid then
!!*!			e.varptr:=void_new()
!		when procid,labelid,recordid,enumid,typeid,dllmoduleid,dllprocid then
!!		when procid,labelid,recordid,enumid,typeid,dllmoduleid,dllprocid,
!!				moduleid then
!		else
!			pcerror("GDS1")
!		esac
!	when procid then				!create new frameid
!!CPL "DEF IN PROC",NAMENAMES[ID]
!!CPL $LINENO
!		e:=addsymbol(owner,d,id)
!		case id
!		when frameid,paramid then
!			e.mutable:=mutable
!			addlocalvar(e)
!		when staticid then
!			e.mutable:=mutable
!!*!			e.varptr:=void_new()
!		when constid then
!!*!			e.varptr:=void_new()
!		when enumid then
!		else
!			pcerror("GDS2")
!		esac
!	when recordid then
!!CPL "DEF IN RECORD",NAMENAMES[ID]
!!CPL $LINENO
!		e:=addsymbol(owner,d,id)
!		e.mutable:=1
!		++owner.nfields
!!*!		++owner.varptr.objptr.urecorddef.nfields
!		e.index:=owner.nfields
!
!	when typeid then
!!CPL "ADD IN TYPEID",NAMENAMES[ID]
!!CPL $LINENO
!		e:=addsymbol(owner,d,id)
!	when programid then
!!CPL "HERE"
!!CPL $LINENO
!		e:=addsymbol(owner,d,id)
!
!	else
!PCERROR("GETDEFSYMBOL: CAN'T CREATE DEF NAME")
!	esac
!!
!!	e:=addsymbol(owner,d,id)
!!	e.mutable:=1
!!CPL =E,NAMENAMES[E.NAMEID]
!
!	pd^:=e
!	return e
!end
!
!global function getprocsymbol(ref strec owner,d)symbol=
!	var symbol e

!	if d.nameid<>genericid then
!		pcerror("REDEF PROC NAME")
!	fi
!
!!CPL $LINENO
!	return addsymbol(owner,d,procid)
!end
!
!global function resolvefield(variant p, symbol d)symbol=
!!d should refer to a generic name entry
!!p is the thing being dot-accessed
!	var symbol e
!	var variant precord
!
!!CPL "RESOLVEFIELD:",D.NAME,"IN",TTNAME[P.TAG]
!	case p.tag
!	when trecord then
!!*!		precord:=p.objptr.urecord.recobj
!		e:=d.nextdupl
!		while e do
!			if precord=e.owner.varptr then
!				return e
!			fi
!			e:=e.nextdupl
!		od
!	else
!		println ttname[p.tag]
!		pcerror_s("Can't use DOT on this type:",ttname[p.tag])
!	esac
!
!	pcerror_s(".name not resolved:",d.name)
!	return d
!end

global function resolvedottedname(symbol owner, d)symbol e=
!d should be generic

	e:=d.nextdupl
	while e and e.owner<>owner do
		e:=e.nextdupl
	od

	return e
end

!global function findmoduleno:int=
!!using pcptr, try and find current module number from pcptr
!!VAR MODULEREC M
!
!	if pcptr=nil then return 1 fi		!have started to run main program yet
!
!	for i to nmodules do
!		if pcptr>=moduletable[i].pcstart and pcptr<=moduletable[i].pcend then
!			return i
!		fi
!	od
!	pcerror("Can't find module no")
!	return 0
!end

global function strmode(int t)ichar=
	static [2048]char str

	istrmode(t,&.str)
	return str
end

proc istrmode(int t, ichar dest)=
	static [2048]char str

	if t<0 then
		strcpy(dest,"*")
		strcat(dest,ttnamedefx[-t].name)
		if ttnamedefx2[-t] then
			strcat(dest,".")
			strcat(dest,ttnamedefx2[-t].name)
		fi
		return
	fi

	if t<tlast then
		strcpy(dest,ttname[t])
		return
	fi

	case ttbasetype[t]
	when tpref then
		strcpy(dest,"ref ")
		istrmode(tttarget[t],dest+4)
	when tparray then
!CPL "STRMODE/TPA",TTLOWEREXPR[T], TTLENGTHEXPR[T], STREXPR_S(TTLENGTHEXPR[T])
!PRINTUNIT(TTLOWEREXPR[T])
!PRINTUNIT(TTLENGTHEXPR[T])

	if ttlengthexpr[t] or ttlowerexpr[t] then
		fprint @dest,"[#:#]",strexpr_s(ttlowerexpr[t]), strexpr_s(ttlengthexpr[t])
	else
		fprint @dest,"[#..#]",ttlower[t],ttlength[t]+ttlower[t]-1
	fi
	istrmode(tttarget[t],dest+strlen(dest))

	else
!CPL =TTNAME[T],=TTNAME[TTBASETYPE[T]]
		strcpy(dest,ttname[t])
	esac

end

global proc createrecordtype(symbol d)=
	int m
!CPL "CREATE RECORD TYPE"
	d.nfields:=0
	m:=addusertype(d)
	ttbasetype[m]:=trecord
end

!global proc addrecordfield(symbol d)=
!	int m
!	symbol r:=stcurrrecorddef, dgen
!!	ref genfieldrec g
!
!!CPL "ADDRECFIELD"
!
!	m:=r.mode
!
!	ttlength[m]:=r.nfields
!	ttsize[m]:=varsize*r.nfields	
!
!!create genfield entry
!	addgenfield(d)
!!	dgen:=d.firstdupl
!!	index:=dgen.genfieldindex
!!
!!!CPL D.NAME,=D.INDEX
!!
!!	if index=0 then			!first field with this name
!!		if ngenfields>=maxgenfield then
!!			pcerror("Too many genfields")
!!		fi
!!		dgen.genfieldindex:=index:=++ngenfields
!!	fi
!!
!!	g:=pcm_alloc(genfieldrec.bytes)
!!	g.def:=d
!!	g.nextdef:=genfieldtable[index]
!!	genfieldtable[index]:=g
!end
!
global proc addgenfield(symbol d)=
	int index
	symbol dgen
	ref genfieldrec g

!CPL "ADDGENFIELD",D.NAME

	dgen:=d.firstdupl
	index:=dgen.genfieldindex

	if index=0 then			!first field with this name
		if ngenfields>=maxgenfield then
			pcerror("Too many genfields")
		fi
		dgen.genfieldindex:=index:=++ngenfields
	fi

	g:=pcm_alloc(genfieldrec.bytes)
	g.def:=d
	g.nextdef:=genfieldtable[index]
	genfieldtable[index]:=g
end

global function addusertype(symbol d)int=
!d is the name of a new user type; the details have been set up inside it
!but now create the actual type

	if ntypes>=maxtype then pcerror("Too many types") fi

	++ntypes
	d.mode:=ntypes
	ttnamedef[ntypes]:=d
	ttname[ntypes]:=d.name

	return ntypes

end

global function makereftype(int target)int=
	int newtype

!CPL "MAKENEWTYPE",TTNAME[TARGET]

	newtype:=addanontype()
	ttbasetype[newtype]:=tpref
!	ttispacked[newtype]:=1
!	tttarget[newtype]:=target

	storemode(stcurrproc,target,&tttarget[newtype])

	ttsize[newtype]:=8
	ttbitwidth[newtype]:=64
	return newtype
end

global function makeaxtype(int target, unit plower, plength)int=
	int newtype,length

!CPL "MAKENEWTYPE",TTNAME[TARGET]

	newtype:=addanontype()

	ttbasetype[newtype]:=tcarray
!	ttbasetype[newtype]:=tparray
!	ttispacked[newtype]:=1

!	tttarget[newtype]:=target
!CPL "ARRAY",STRMODE(TARGET)
	storemode(stcurrproc, target, &tttarget[newtype])

!	length:=b-a+1

	ttlower[newtype]:=1
	ttlengthexpr[newtype]:=plength
	ttlowerexpr[newtype]:=plower
!	ttsize[newtype]:=length*ttsize[target]
	return newtype
end

!global function makestrtype(int m, width)int=
global function makestrtype(int m, unit pwidth)int=
	int newtype

	newtype:=addanontype()
	ttbasetype[newtype]:=m
!	ttispacked[newtype]:=1
!	ttlength[newtype]:=width
	ttlengthexpr[newtype]:=pwidth
	ttlower[newtype]:=1
	ttowner[newtype]:=stcurrproc
!	ttsize[newtype]:=width
	return newtype
end

global function addanontype:int=
!d is the name of a new user type; the details have been set up inside it
!but now create the actual type
	[32]char str

	if ntypes>=maxtype then pcerror("Too many types") fi

	++ntypes
	print @str,"$T",,ntypes

	ttname[ntypes]:=pcm_copyheapstring(str)

	return ntypes

end

global proc createusertype(symbol d, int m)=
!	d.mode:=m
	storemode(stcurrproc,m,&d.mode)

!CPL "CUT",TTNAMEDEF[M],D.MODE,D.NAME
	if m>tlast and ttnamedef[m]=nil then
		ttnamedef[m]:=d
		ttname[m]:=d.name
	fi
end

!global proc resetstructdef(int aligned)=
!	sdaligned:=aligned
!	sdnfields:=0
!	sdsize:=0
!	sdoffset:=0
!	sdlevel:=1
!	sdmaxalign:=1
!	sdunion[sdlevel]:=0		!start off in struct mode
!	sdmaxsize[sdlevel]:=0
!
!	sdmode:=addanontype()
!	ttbasetype[sdmode]:=tpstruct
!!	ttispacked[sdmode]:=1
!	ttlower[sdmode]:=1
!end
!
!global function getstructdef:int=
!
!	if sdaligned then
!		sdsize:=roundoffset(sdsize,sdmaxalign)
!		stcurrtypedef.structmaxalign:=sdmaxalign
!	else
!		stcurrtypedef.structmaxalign:=1
!	fi
!
!	ttsize[sdmode]:=sdoffset
!
!	return sdmode
!end
!
!global proc addstructfield(symbol d, int m)=
!	int newoffset,fieldsize,alignment
!
!!	CPL "ADD STRUCT",D.NAME,TTNAME[M], =SDALIGNED
!	d.mode:=m
!
!	fieldsize:=ttsize[m]
!	if sdaligned then
!		alignment:=getalignment(m)
!		sdmaxalign max:=alignment
!		newoffset:=roundoffset(sdoffset,alignment)
!		sdsize+:=newoffset-sdoffset
!	else
!		newoffset:=sdoffset
!	fi
!
!	d.fieldoffset:=newoffset
!	sdoffset:=newoffset
!
!	if sdunion[sdlevel] then
!		sdmaxsize[sdlevel] max:=ttsize[m]
!	else
!		sdoffset+:=fieldsize
!		sdsize+:=fieldsize
!	fi
!
!	addgenfield(d)
!
!end
!
!global proc do_structblock(int code)=
!	if sdlevel>=maxstructdepth then
!		pcerror("struct/union nesting")
!	fi
!	++sdlevel
!	sdunion[sdlevel]:=code='U'
!	sdmaxsize[sdlevel]:=0
!end
!
!global proc do_endblock=
!	int blocksize
!
!	if sdlevel<=1 then
!		pcerror("struct/union ?")
!	fi
!
!	if sdunion[sdlevel] then
!		sdoffset+:=sdmaxsize[sdlevel]
!	fi
!	--sdlevel
!end
!
global function roundoffset(int offset, alignment)int=
	int mask

	if alignment=1 then return offset fi
	mask:=alignment-1
	while offset iand mask do ++offset od

	return offset
end

global function getalignment(int m)int=
!return alignment needed for type m, as 1,2,4,8
	int a

	case ttbasetype[m]
	when tparray then
		return getalignment(tttarget[m])
	when tpstruct then
!		return ttnamedef[m].structmaxalign
	esac

	a:=ttsize[m]
	case a
	when 1,2,4,8 then
		return a
	esac
	cpl ttname[m],a
	pcerror("Getalign not 1248")

	return 0
end

=== qq_modules.m 16/32 ===
import* qq

global function checkmodule(ichar modulename)int moduleno=
!return moduleno if already present, else 0
	for i to nmodules do
		if eqstring(moduletable[i].name,modulename) then	!already loaded
			return i
		fi
	od

	return 0
end

global function loadmainmodule(ichar filename)ichar=
	var ichar source,modulename
	var ref modulerec m
	var symbol stmodule
	int fileloc
	[300]char path

	stprogram:=createstroot("$program")

!	source:=loadsource(filename)
	source:=loadmainsource(filename, fileloc)

	if source=nil then
		return nil
	fi

	modulename:=pcm_copyheapstring(extractbasefile(filename))
	convlcstring(modulename)
!	currmodule:=addsymbol(stprogram,nil,moduleid,modulename)
!CPL "LOADPROG"
!	stmodule:=addsymbol(stprogram,nil,moduleid,modulename)
	stmodule:=addsymbol(stprogram,addglobalname(modulename),moduleid,0)

    stmodule.moduleno:=1

!	globalproctable[++nglobalprocs]:=stmodule
!	stmodule.fnindex:=1

	m:=&moduletable[addmodule(modulename)]
	m.def:=stmodule
	m.source:=source
	m.originalsource:=pcm_copyheapstring(source)
	m.sourcelen:=rfsize
	m.sourcepath:=pcm_copyheapstring(filename)
	m.sourceloc:=fileloc

!	strcpy(&.modulename,extractbasefile(filespec))
	strcpy(&.path,extractpath(filename))
!CPL =PATH
	if path[1] then
		addsearchdir(&.path)

!CPL "NEW SEARCHDIRS:",NSEARCHDIRS
!FOR I TO NSEARCHDIRS DO
!	CPL I,SEARCHDIRS[I]
!OD
!

	fi

	return source
end

global function loadmodule(symbol d)int moduleno=
!assume module not already loaded (has called checkmodule)
!load module source, and return new module number
!return 0 on error

!	[300]char path
	int fileloc
	var [300]char filename
	var symbol stmodule
	var ichar source,modulename
	var ref modulerec m
	var ichar ipath

!CPL "LOADMODULE",D.NAME

	modulename:=d.name

	strcpy(filename,modulename)
	strcat(filename,".q")

	source:=loadmodulesource(filename, ipath, fileloc)

	if source=nil then
		return 0
	fi

!CPL =STCURRMODULE,CURRMODULE,STCURRMODULE.MODULENO

	stmodule:=addsymbol(stprogram,d,moduleid,0)

	moduleno:=addmodule(modulename)
	currmodule.importmap[moduleno]:=1

	m:=&moduletable[moduleno]
	m.def:=stmodule
	m.source:=source
	m.originalsource:=pcm_copyheapstring(source)
	m.sourcelen:=rfsize
	m.sourcepath:=pcm_copyheapstring(ipath)
	m.sourceloc:=fileloc

	stmodule.moduleno:=moduleno

	return moduleno
end

function loadmainsource(ichar filename, int &fileloc)ichar=
!These loadxxxsource functions have the same pattern:
!They locate a main module, import module or support file (strinclude etc)
!by looking at internal libs, a loaded QA file, or by looking in search dirs

!When found, file is loaded into memory, and pointer to it is returned
!Also returned (not for main) is a pointer to the full path of the file
!but this needs to be consumed quickly by the caller
!Plus, fileloc is set to 'DISK', 'INT', or 'QA'
!When not found, returns nil

!In the case of an internal or QA file, the finalpath is the base filename, preceded
!by INT: or QA:

	if fbundled then
		fileloc:='QA'
		return loadbundledfile(filename)
	fi

	if not checkfile(filename) then
		loaderror_s("Can't find main module",filename)
	fi

	fileloc:='DISK'
	return loadsource(filename)
end

function loadmodulesource(ichar modulename, &finalpath, int &fileloc)ichar=
	[300]char filename
	ichar source

	strcpy(filename,addext(modulename,"q"))
	finalpath:=modulename

!CPL "GETMODULE",&.FILENAME
	if fbundled then
		fileloc:='QA'
		return loadbundledfile(filename)
	fi

	if dointlibs then
		source:=findstdlib(&.filename)
		if source then
			fileloc:='INT'
			return source
		fi
	fi

	finalpath:=findfile(filename)
	if finalpath=nil then
		return nil
	fi

	fileloc:='DISK' 
	return loadsource(finalpath)
end

!function loadsupportsource(ichar filename, &finalpath, int &fileloc)ichar=
!	ichar source
!
!	finalpath:=filename
!
!	if fbundled then
!		fileloc:='QA'
!		return loadbundledfile(filename)
!	fi
!
!	finalpath:=findfile(finalpath)
!	fileloc:='DISK' 
!
!	if finalpath then
!		return loadsource(finalpath)
!	fi
!	return nil
!end

function findfile(ichar filename)ichar=
!look for file by searching through search dirs
!return nil when not found, or the name of the sucessful filespec
!filename must be base filename, with extension
	static [300]char filespec

	for i:=nsearchdirs downto 1 do
		strcpy(filespec,searchdirs[i])
		strcat(filespec,filename)

		if checkfile(filespec) then
			return filespec
		fi
	od

	return nil
end

global function loadsource(ichar file)ichar=
	var ichar s

	s:=cast(readfile(file))
!	if s=nil then
!		println "Can't load",s
!		stop 1
!	fi
	return s
end

global function addmodule(ichar name)int=
	var ref modulerec m
    if nmodules>=maxmodule then
		abortprogram("Too many modules")
	fi
	m:=&moduletable[++nmodules]
!	m.modtype:=import_mod
	m.name:=pcm_copyheapstring(name)
	m.importmap[nmodules]:=1
	m.moduleno:=nmodules
	return nmodules
end

!global proc codegenmodule(int m)=
!	if moduletable[m].pcstart=nil then
!		pcl_codegen(m)
!	fi
!end

global proc labelfixups(ref int pcstart)=
	var ref int pc
	var int cmd,nopnds


ABORTPROGRAM("LABELFIXUPS")

!!CPL "LABELFIXUPS",PCSTART
!	pc:=pcstart
!
!	repeat
!!		cmd:=pc++^
!		cmd:=pc^
!
!	if fixbytecodes then
!		pc^:=cast(handlertable[cmd])
!	fi
!!CPL PCLNAMES[CMD],PC^
!		++pc
!
!		nopnds:=pclnopnds[cmd]
!		for i to nopnds do
!			if pclfmt[cmd,i]=clabel then
!				pc^:=cast(pcstart+pc^)
!			fi
!			++pc
!		od
!	until cmd in [kzero,kendmodule]
end

global proc labelunfixups(ref int pcstart)=
	var ref int pc
	var int cmd,nopnds

ABORTPROGRAM("LABELUNFIXUPS")
!	pc:=pcstart
!
!	repeat
!		cmd:=pc++^
!
!		nopnds:=pclnopnds[cmd]
!		for i to nopnds do
!			if pclfmt[cmd,i]=clabel then
!!				pc^:=cast(pcstart+pc^)
!				pc^:=cast(ref int(pc^)-pcstart)
!			fi
!			++pc
!		od
!	until cmd in [kzero,kendmodule]
end

global function loadbundledfile(ichar filespec,int support=0)ichar=
!loading bundled file
!Name of header is in 'file'.

ABORTPROGRAM("LOAD BUNDLED NOT READY")
	return nil
end

global function findstdlib(ichar name)ichar=
ABORTPROGRAM("FINDSTDLIB NOT READY")
!	for i:=1 to stdlibnames.len do
!		if eqstring(name,stdlibnames[i]) then
!			return stdlibtext[i]
!		fi
!	od
	return nil
end

=== qq_strings.m 17/32 ===
import* qq

global object emptystring

proc $init=
	emptystring:=obj_new()
	emptystring.refcount:=1
	emptystring.objtype:=normal_obj
end

global proc var_empty_string(variant dest)=
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=emptystring
	++emptystring^.refcount
end

global proc var_make_string(ichar s, variant dest, int mutable=0)=
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_string(s,mutable)
end

global proc var_make_stringn(ichar s, int length, variant dest, int mutable=0)=
	dest.tagx:=tstring ior hasrefmask
	dest.objptr:=obj_make_stringn(s,length,mutable)
end

global function obj_new_string(int n)object p=
	p:=obj_new()
	p.mutable:=1
	p.ustr.length:=n
	p.ustr.lower:=1
	p.objtype:=normal_obj

	if n then
		p.ustr.strptr:=pcm_alloc(n)
		p.ustr.allocated:=allocbytes
	fi

	return p
end

global function obj_make_string(ichar s,int mutable=0)object p=
	int n

	p:=obj_new_string(n:=strlen(s))
	p.mutable:=mutable

	if n then
		memcpy(p.ustr.strptr,s,n)
	fi
	return p
end

global function obj_make_stringn(ichar s,int length,mutable=0)object p=
!when s=nil, then the string data is not initialised

	p:=obj_new_string(length)
	p.mutable:=mutable

	if length then
		if s then
			memcpy(p.ustr.strptr,s,length)
		else
			memset(p.ustr.strptr,0,length)
		fi

	fi
	return p
end

global proc obj_free_string(object p)=
	variant q

	if p.ustr.length then
		pcm_free(p.ustr.strptr,p.ustr.allocated)
	fi

	pcm_free32(p)
end

global proc var_dupl_string(variant a)=
	object p,q

	p:=a.objptr
	q:=obj_new_string(p.ustr.length)
	a.objptr:=q

	if q.ustr.length then
		memcpy(q.ustr.strptr,p.ustr.strptr,q.ustr.length)
	fi
end

global proc var_getix_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.ustr.length) then
		pcerror("getstring[int] bounds")
	fi

!	var_unshareu(a)
	stringslice(a,index,index,a)
end

global proc var_getixref_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.ustr.length) then
		pcerror("getixref[int] bounds")
	fi

!	var_unshareu(a)

	a.tagx:=trefpack
	a.uref.elemtag:=tpu8
	a.uref.ptr:=cast(q.ustr.strptr+index-1)

!	stringslice(a,index,index,a)
end

global proc var_getdotix_string(variant a, int index)=
!put result into a (which will be on the stack)
	object q

	q:=a.objptr

	if word(index-1)>=word(q.ustr.length) then
		pcerror("x.[] bounds")
	fi

!	var_unshareu(a)

	a.tagx:=tint
	a.value:=(q.ustr.strptr+index-1)^
end

global proc var_getdotixref_string(variant a, int index,variant dest)=
	object q

CPL "GETDOTIXREF",TTNAME[A.TAG]

	q:=a.objptr

	--index

	if word(index)>=word(q.ustr.length) then
		pcerror("x.[] bounds")
	fi

	dest.tagx:=trefpack
	dest.uref.elemtag:=tpu8
	dest.uref.ptr:=q.ustr.strptr+index
end

global proc var_getslice_string(variant a, int i,j)=
	object p:=a.objptr

	if i<1 or j>p.ustr.length or i>j then
		pcerror("string/slice bounds")
	fi

!	var_unshareu(a)
	stringslice(a,i,j,a)
end

!global proc var_getixref_stringint(variant a, int index)=
!	variant p
!	object q
!	word offset
!
!	q:=a.objptr
!
!	offset:=index-q.ustr.lower
!	if offset>=word(q.ustr.length) then
!		pcerror("getstringref[int] bounds")
!	fi
!
!	p:=q.ustr.varptr+offset
!	var_unshare(a)			!a should not disappear; rvalues can't have & applied
!
!	a.tagx:=trefvar
!	a.varptr:=p
!end
!
proc stringslice(variant a, int i,j, variant dest)=
	object p,q

	p:=a.objptr

!CPL "SS1",=STRSTACK(A),=STRALLOC(P), =STRALLOC(P.USTR.STRPTR)
!
	q:=obj_new()
	q.mutable:=p.mutable
	q.ustr.length:=j-i+1
	q.ustr.lower:=1
	q.objtype:=slice_obj

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.ustr.objptr2:=p.ustr.objptr2		!link to original
		++q.ustr.objptr2.refcount
	when extslice_obj then
		q.ustr.objptr2:=nil
		q.objtype:=extslice_obj
	else
!CPL "MAKESLICE",STRSTACK(A),TTNAME[A.TAG],=P.REFCOUNT
		++p.refcount
		q.ustr.objptr2:=p				!link to original
!CPL "SS2",=STRSTACK(A),=STRALLOC(P), =STRSTACK(Q.USTR.VARPTR2),=STRALLOC(Q)
	esac
	q.ustr.strptr:=p.ustr.strptr+i-1

	dest.tagx:=a.tagx
	dest.objptr:=q
!CPL "MAKESLICEX",STRSTACK(A),TTNAME[A.TAG],=P.REFCOUNT
end

global proc var_putix_string(variant a, int index, variant x)=
	ichar s
	object p,q
	int length

	p:=a.objptr
	if not p.mutable then pcerror("Str not mutable") fi
	length:=p.ustr.length

	if index not in 1..length then
		if index=length+1 then
			var_addto_string(a,x)
			return
		else
			pcerror("putstring[int] bounds")
		fi
	fi

	s:=p.ustr.strptr+index-1
!	var_unshare(a)
!CPL "VAR/PUTIX2",X.OBJPTR.REFCOUNT,=X,=DEST
	if x.tag<>tstring then
		pcerror("s[i]:= not str")
	fi
	q:=x.objptr
	if q.ustr.length=0 then pcerror("s[i]:=""""") fi
	s^:=q.ustr.strptr^
end

global proc var_putslice_string(variant a, int i,j, variant x)=
!insert a substring into a
	ichar s
	object p,q
	int length,sublength

	p:=a.objptr
	if not p.mutable then pcerror("Str not mutable") fi
	length:=p.ustr.length

	if i<1 or j>p.ustr.length or i>j then
		pcerror("string/slice bounds")
	fi
	sublength:=j-i+1

	s:=p.ustr.strptr+i-1
!	var_unshare(a)
!CPL "VAR/PUTIX2",X.OBJPTR.REFCOUNT,=X,=DEST
	if x.tag<>tstring then
		pcerror("s[i..j]:= not str")
	fi
	q:=x.objptr
	if q.ustr.length<sublength then
		pcerror("substr too short")
	fi
	memcpy(s,q.ustr.strptr, sublength)
end

global proc var_putdotix_string(variant a, int index, variant x)=
	ichar s
	object p,q
	int length,ch

	if x.tag<>tint then
		pcerror("s.[i]:= not int")
	fi
	ch:=x.value

	p:=a.objptr
	if not p.mutable then pcerror("Str not mutable") fi
	length:=p.ustr.length

	if index not in 1..length then
		if index=length+1 then
			var_addto_string_ch(a,ch)
			return
		else
			pcerror("str.[int] bounds")
		fi
	fi

	(p.ustr.strptr+index-1)^:=ch
!	var_unshare(a)
end

!proc var_append_string(variant a,x)=
!!do in-place append of b to string a
!	objptr p,q
!	var int n
!
!	p:=a.objptr
!	q:=x.objptr
!
!	if x.tag<>tstring then pcerror("str expected") fi
!
!	if p.ustr.objtype<>normal_obj then
!		pcerror("Can't extend slice")
!	fi
!
!	if not p.mutable then
!		pcerror("string/append not mutable")
!	fi
!
!	n:=p.ustr.length+1			!new length
!
!	if n>p.ustr.allocated then		!need more space
!		obj_resize_string(p,n)
!	else
!		p.ustr.length:=n
!	fi
!
!	(p.ustr.varptr+n-1)^:=
!
!	(a.ustr.varptr+n-1)^:=x^		!transfers ownership
!
!end

global proc obj_resize_string(object p,int n)=
	ichar s
	int oldalloc

!CPL "RESIZE",N,P.USTR.LENGTH, P.USTR.ALLOCATED
	if n<=p.ustr.allocated then
		p.ustr.length:=n
	else
!CPL "RESIZING"
		oldalloc:=p.ustr.allocated
		s:=pcm_alloc(n)
		p.ustr.allocated:=allocbytes
		if p.ustr.length then
			memcpy(s,p.ustr.strptr,p.ustr.length)

			pcm_free(p.ustr.strptr, oldalloc)


		fi

		p.ustr.strptr:=s
		p.ustr.length:=n
	fi
end

!global proc var_appendto_string(variant a, x)=
!!a is a string (was a pointer)
!	obj_append_string(a.objptr,x)
!end
!
!

global proc var_add_string(variant a,b)=
!a':=a+b; original a is preserved, just that new result is into a
	object p:=a.objptr
	object q:=b.objptr
	object r

	int alen:=p.ustr.length
	int blen:=q.ustr.length
	int newlen

	if blen=0 then
!		var_unshare(b)
		var_shareu(a)
!CPL "HERE"
		return
	elsif alen=0 then
!		var_unshare(a)
		var_make_stringn(q.ustr.strptr,blen,a,1)
!		a^:=b^
		return
	fi

	newlen:=alen+blen
	r:=obj_new_string(newlen)
	memcpy(r.ustr.strptr, p.ustr.strptr, alen)
	memcpy(r.ustr.strptr+alen, q.ustr.strptr, blen)

!	var_unshare(a)
!	var_unshare(b)

	a.objptr:=r
end

global proc var_addto_string(variant a,b)=
!a+:=b; inplace add
!a is normally subject of a refvar, so not shared

!CPL "VAR/ATS"
	object p:=a.objptr
	object q:=b.objptr

!CP "<<"; VAR_PRINT(B); CP ">>"
	int alen:=p.ustr.length
	int blen:=q.ustr.length
	int newlen

!CPL =P.USTR.MUTABLE

	if not p.mutable then
		PCERROR("ADDTOSTR/NOT MUT")
	FI

	if blen=0 then
!		var_unshareu(b)
		return
	elsif alen=0 then			!copy b over and share
		var_unshareu(a)
		a^:=b^
		var_duplu(a)
CPL =B.OBJPTR.MUTABLE

!		var_shareu(b)
		return
	fi

	newlen:=alen+blen
	obj_resize_string(p,newlen)
	memcpy(p.ustr.strptr+alen, q.ustr.strptr, blen)
!	var_unshareu(b)

!CPL "ADDTO",A.OBJPTR.USTR.LENGTH
end

global proc var_addto_string_ch(variant a,int ch)=
!a+:=ch; inplace add
!a is normally subject of a refvar, so not shared
	object p:=a.objptr
	int alen:=p.ustr.length

	if not p.mutable then
		PCERROR("ADDTOSTR/ch/NOT MUT")
	FI
	obj_resize_string(p,alen+1)
	(p.ustr.strptr+alen)^:=ch
end

global function var_equal_string(variant x,y)int =
!return 1 if strings in x,y are equal, otherwise 0
	int n,res
	object px,py

	px:=x.objptr
	py:=y.objptr

	n:=px.ustr.length

	if n<>py.ustr.length then
		res:=0				!unequal lengths
	elsif n=0 then
		res:=1				!same zero length
	else
		res:=cmpstringn(px.ustr.strptr,py.ustr.strptr,n)=0
	fi

!	var_unshareu(x)
!	var_unshareu(y)
	return res
end

global function var_compare_string(variant x,y)int =
!return -1/0/+1
	int res
	object px,py

	px:=x.objptr
	py:=y.objptr

	res:=cmpstring_len(px.ustr.strptr, py.ustr.strptr, px.ustr.length, py.ustr.length)
!	var_unshareu(x)
!	var_unshareu(y)
	return res
end

function cmpstring_len(ref char s,t,int slen,tlen)int =
!compare the given strings with these lengths, and return -1,0,1

	if slen=0 then
		if tlen=0 then
			return 0		!empty:empty
		else
			return -1		!empty:str
		fi
	elsif tlen=0 then	!str:empty
		return 1
	else
		if slen=tlen then
			if slen=1 then
				if s^<t^ then return -1
				elsif s^>t^ then return 1
				else
					return 0
				fi
			fi
			return cmpstringn(s,t,slen)
		else
			return cmpstring(convCstring(s,slen),convCstring(t,tlen))
		fi
	fi
end

global function var_in_string(variant x,y)int =
!return start index of string x in y, or 0
	int xlen,ylen,result,i,j,k
	ref char sx, sy
	object px,py

	px:=x.objptr
	py:=y.objptr

	xlen:=px.ustr.length
	ylen:=py.ustr.length

	if xlen=0 or ylen=0 then		!at least one is empty
		return 0
	fi

	k:=ylen-xlen
	for i:=0 to k do			!all start positions
		sx:=px.ustr.strptr
		sy:=py.ustr.strptr+i
		for j:=1 to xlen do			!all chars in y
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

global proc var_iconvcase(variant a,b,int upper)=
!do in-place conversion of string in a^ to lower or upper (upper=0/1).
!a points directly to a varrec to be modified
!b is optional if supplied, gives number of chars to convert at left of string
!
	int i,n
	ref char s
	object pa

	pa:=a.objptr

	if b.tag>tvoid then		!allow void param to be regarded as missing one
		n:=var_getintvalue(b)
	else
		n:=pa.ustr.length			!default is the whole length of the string
	fi
!CPL =n

	if a.tag<>tstring then
		pcerror("convcase/notstr")
	fi

	if n<0 then
		pcerror("CONVCASE N<0")
	fi

	if n=0 then
		return
	fi

	if n>pa.ustr.length then
	cpl =n,pa.ustr.length
		pcerror("convcase/N?")
	fi
	s:=pa.ustr.strptr

	if upper then
		to n do
			s^:=toupper(s^)
			++s
		od
	else
		to n do
			s^:=tolower(s^)
			++s
		od
	fi
end

global function var_makestrslicexobj(ichar s, int length)object=
!s is an existing non-allocated or external string
!create a special string slice object, which for now has the format::
! .objtype=extslice, but .objptr2=0
!length can be 0, then s can be nil or ""
!
	object p

	if length=0 then s:=nil fi

	p:=obj_new()
	p.ustr.strptr:=s
	p.mutable:=1
	p.ustr.length:=length
	p.objtype:=extslice_obj		!.objptr2 will be zero
	return p
end

function var_asc(variant a)int=
	object p
	if a.tag<>tstring then pcerror("Asc:not str") fi
	p:=a.objptr
	if p.ustr.length<1 then pcerror("Asc:empty") fi

	return p.ustr.strptr^
end

global proc var_new_string(variant a, b, dest)=
	int length:=var_getintvalue(a)
	int ch

	if length<0 then pcerror("Length<0") fi

	var_make_stringn(nil,length,dest)

	case b.tag
	when tint then
		ch:=b.value
	when tstring then
		ch:=var_asc(b)
	when tvoid then
		ch:=' '
	else
		pcerror("Not int/str")
	esac

	if length then
		memset(dest.objptr.ustr.strptr,ch,length)
	fi
end

global proc var_new_stringn(int length, variant dest)=
	if length<0 then pcerror("Length<0") fi

	var_make_stringn(nil,length,dest)
end

global proc var_mul_string(variant a, int m)=
!a:=a*m
!a has been copied, so no need to unshare it

	int i,oldlen,newlen
	ref char newptr,p
	varrec v
	object pa,s

	if m<0 then
		pcerror("neg str mul")

	elsif m=0 then		!result is empty str
		var_empty_string(a)
		return

	elsif m=1 then		!leave a unchanged
		var_shareu(a)	!
		return

	else				!multiple non-null string by m
		pa:=a.objptr
		oldlen:=pa.ustr.length
		if oldlen then			!not empty string
			newlen:=oldlen*m

			v.objptr:=obj_new_string(newlen)
			v.tagx:=tstring ior hasrefmask

			p:=v.objptr.ustr.strptr
			to m do
				memcpy(p,pa.ustr.strptr,oldlen)
				p+:=oldlen
			od
			a^:=v
		else				!was empty string: copy to v
			var_empty_string(a)
			return
		fi
	fi
end

global proc var_convert_string_list(variant a, int t, variant dest)=
	object p:=a.objptr
	variant q
	int length:=p.ustr.length
	ichar s

	var_make_list(nil, dest, length, 1)
	q:=dest.objptr.ulist.varptr
	s:=p.ustr.strptr

	to length do
		var_make_stringn(s,1, q,1)
		++s
		++q
	od
end

global proc var_expand_string(variant a, dest, int m)=
	variant b,c
	object p
	ref char s
	int n

!CPL "EXPAND",=M


	p:=a.objptr
	b:=dest
	s:=p.ustr.strptr
	n:=1

	to m do
		if n>p.ustr.length then
			var_empty_string(dest)
		else
			var_make_stringn(s,1, dest,1)
			++s
		fi
		++n
		++dest
	od
end

=== qq_print.m 18/32 ===
import* qq

!Vars for i/o
!Makes use of stdio/fileio/strio/windio as used by Q system
global  int mindev		!one of stdio/fileio/strio/windio
global  int moutdev
global  ref int minchan		!actual file handles
global  filehandle moutchan
global  varrec minvar		!strio: vars to be used as source or dest
global  varrec moutvar		!str: used for sprint(=string) and @&.string (=refvar)
!global  ichar mfmtstr		!used for format string is nil (no fmt string) or points to fmt string
!global  ichar mfmtcurr	!point to next char to use in fmtstr
!global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,(0,0))
!global  filehandle testfilech	!non-zero means contains handle for test file o/p

!I/O Constants: print/read i/o channels
global const std_io	= 0		!console i/o
global const file_io	= 1		!uses file channel inchan or outchan
global const str_io	= 2		!uses string instr^ or outstr^
global const wind_io	= 3		!uses window inwind^ or outwind^
global const istr_io	= 4		!used by pcx interpreter

const maxoclevel=6
[0:maxoclevel]int32			moutdevstack
[0:maxoclevel]filehandle	moutchanstack
[0:maxoclevel]varrec		moutvarstack
[0:maxoclevel]byte			mgapstack
[0:maxoclevel]ref char		mfmtstrstack
[0:maxoclevel]ref char		mfmtcurrstack
int noclevels

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

const maxstrlen=256
const comma=','
const onesixty=1024

global  ichar mfmtstr		!used for format string is nil (no fmt string) or points to fmt string
global  ichar mfmtcurr	!point to next char to use in fmtstr
global  fmtrec defaultfmt = (0,0, 10, 0,' ','f', 0,0,0,'R',0,0, 0,0,(0,0))
byte mgapneeded

const minkb_size=16384		! start size of kb buffer
ref char kb_start			! point to start of read buffer
ref char kb_pos				! current position it's up to (next read starts here)
ref char kb_lastpos			! set by sread() just before reading used for reread()
int kb_size					! total available length of entire read buffer (which is not zero-terminated)
int kb_linelength			! length of this current line (set by readln)
int kb_length				! length of current contents of buffer (can be zero)
							! use kb_length-(kb_pos-kb_start) for length starting from kb_pos
int kb_lastlength			! used with kb_lastpos to remember start of last read item
char termchar				! terminator char set by readxxx()
int itemerror				!	set by some read functions, eg for reals

global filehandle testfilech	!non-zero means contains handle for test file o/p

const maxlistdepth=4
int listdepth=0				!recursive nesting levels for lists/records

![0:]char digits=('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F')
[0:]char digits=a"0123456789ABCDEF"

global proc pch_print(variant p, fmt=nil)=
	varrec v
	variant q
	object a
	ref char s
	varrec emptyfmt

!	CPL "VARPRINT",P.VALUE

	if fmt=nil then
		fmt:=&emptyfmt
		emptyfmt.tagx:=tvoid
	fi

	if mfmtstr=nil then
		if mgapneeded then
			printstr_n(" ",1)
		else
			mgapneeded:=1
		fi
	else
		printnextfmtchars(0)
	fi

	listdepth:=0
	a:=p.objptr

	pch_tostr(p,fmt,&v)
	printstr_n(v.objptr.ustr.strptr,v.objptr.ustr.length)
	var_unshare(&v)

!	when treal then print p.xvalue
!	when tpstringz then print p.svalue
!	when tvoid then print "<void>"
!	when tlist,trecord then
!!CPL "RECORD",A.ULIST.LENGTH,=A.UREC.VARPTR
!		q:=a.ulist.varptr
!		print "("
!		for i:=a.ulist.length downto 1 do
!!CPL "PRINT/RECELEM",I,Q
!			pch_print_nf(q)
!			mgapneeded:=0
!			if i>1 then print "," fi
!			++q
!		od
!		print ")"
!!	when trecord then
!!		a:=p.objptr
!!		q:=a.ulist.varptr
!!		print "(",$
!!		for i:=a.ulist.length downto 1 do
!!			var_print(q)
!!			mgapneeded:=0
!!			if i>1 then print $,",",$ fi
!!			++q
!!		od
!!		print $,")"
!	when trefvar then
!		fprint "<refvar:#:#>",p.varptr,ttname[p.varptr.tag]
!
!	when trefpack then
!		fprint "<refpack:#:#>",p.uref.ptr,ttname[p.uref.elemtag]
!
!	when ttype then
!		fprint "<#>",ttname[p.value]!+(p.value<=tlast|1|0)
!
!	when tsymbol then
!		fprint "<#:""#"">",namenames[p.def.nameid],p.def.name
!
!	when trange then
!		fprint "#..#",p.range_lower, p.range_upper
!
!	when tstring then
!!CPL "PRINT STR",A.USTR.LENGTH
!		s:=a.ustr.strptr
!		to a.ustr.length do
!			print s++^
!		od
!!		if a.ustr.length then
!!			print a.ustr.length:"v",,a.ustr.strptr:".*"
!!		fi
!
!	else
!!		println ttname[p.tag]
!		fprint "<?print:#>",ttname[p.tag]
!	end
!	mgapneeded:=1

end

global proc pch_print_nf(variant p)=
	pch_print(p, nil)
end

global proc pch_printnogap=
	mgapneeded:=0
!	print $
end

global proc pch_println=
	if mfmtstr then
		printnextfmtchars(1)
	fi
	mgapneeded:=0
	printstr_n("\n",-1)
end

proc pch_startprint(variant p)=
	object s

	switch ++noclevels
	when 0, 1 then		! no action needed

	when maxoclevel+1 then		! overflow
		prterror("print #x overflow")
	else
		moutdevstack[noclevels-1]:=moutdev
		moutchanstack[noclevels-1]:=cast(moutchan)
		moutvarstack[noclevels-1]:=moutvar
		mfmtstrstack[noclevels-1]:=mfmtstr
		mfmtcurrstack[noclevels-1]:=mfmtcurr
		mgapstack[noclevels-1]:=mgapneeded
	endswitch

	mfmtstr:=nil
	mfmtcurr:=nil

	if p=nil then
		goto doconsole
	fi
	switch p.tag
	when tint then
		switch p.value
		when 0 then
	doconsole::
			moutdev:=std_io
			moutchan:=nil
		
		when 1 then			! special sprint string
			moutdev:=str_io
			moutchan:=nil
			moutvar.tagx:=tstring ior hasrefmask

			s:=obj_new()
			s.mutable:=1
			moutvar.objptr:=s

		when 2 then
			if testfilech=nil then
				prterror("@2: file not open")
			fi
			moutdev:=file_io
			moutchan:=testfilech
		
		else
			moutdev:=file_io
			moutchan:=cast(filehandle(p.value))
		endswitch

	when trefvar then
		p:=p.varptr
		switch p.tag
		when tstring then
			moutdev:=istr_io
			moutchan:=nil
			moutvar.tagx:=trefvar
			moutvar.varptr:=p
		
		else
		PRINTLN ttname[p.tag]
			prterror("Print@^?")
		endswitch

	else
		switch p.tag
		when trecord, tstruct then		! check for specific records
			moutdev:=std_io
		else
		PRINTLN ttname[p.tag]
			prterror("Can't do startprint...")
		endswitch
	endswitch

	mgapneeded:=0
end

global proc pch_startprintcon=
	varrec v

	v.tagx:=tint
	v.value:=0
	pch_startprint(&v)
end

global proc pch_endprint=
	variant p

	if mfmtstr then
		printnextfmtchars(1)
	fi
	switch moutdev
	when istr_io then
		p:=moutvar.varptr
	endswitch

	if mfmtstr<>nil then
		pcm_free(mfmtstr,strlen(mfmtstr)+1)
	fi

	if --noclevels=-1 then
		prterror("resetoc??")
	fi

	if noclevels=0 then
		moutdev:=std_io

	else			! exit from higher nesting level
		moutdev:=moutdevstack[noclevels]
		moutchan:=cast(moutchanstack[noclevels])
		moutvar:=moutvarstack[noclevels]
		mgapneeded:=mgapstack[noclevels]
		mfmtstr:=mfmtstrstack[noclevels]
		mfmtcurr:=mfmtcurrstack[noclevels]
	fi
	mgapneeded:=0
end

global proc pch_printspace=
	mgapneeded:=0
	print " "
end

global proc pch_readln(variant dev) =
!note: generally, at least one spare given should be left at the of the buffer.
!(readline zero-terminates the input line anyway)
!Sometimes C-functions might be called directly, and a zero-terminator is added (eg. readreal/sscanf)
	filehandle ch
	int length
	object pdev

	if kb_start=nil then
		kb_start:=pcm_alloc(minkb_size)
		kb_size:=minkb_size
		kb_lastpos:=kb_start
		kb_pos:=kb_start
		kb_length:=0
		kb_lastlength:=0
		kb_linelength:=0
	fi

	switch dev.tag
	when tvoid then
doconsole::
		readlinen(nil,kb_start,kb_size)	! reads as zero-terminated
		kb_length:=strlen(kb_start)

	when tint then
		switch dev.value
		when 0 then
			goto doconsole
		when 1 then
			if testfilech=nil then
				prterror("R@2: file not open")
			fi
			ch:=cast(testfilech)

		else
			ch:=filehandle(dev.value)
		endswitch
		pc_readlinen(cast(ch),kb_start,kb_size)			! reads as zero-terminated
		kb_length:=strlen(kb_start)

	when tstring then
		pdev:=dev.objptr
		length:=pdev.ustr.length
		if length=0 then
			kb_length:=0
			kb_start^:=0
		elsif length>=kb_size then
			prterror("KB overflow")
		else
			kb_length:=length
			memcpy(kb_start,pdev.ustr.strptr,length)
		fi
	else
		pcustype("readln@",dev)
	endswitch

	kb_pos:=kb_start
	kb_lastpos:=kb_pos
	kb_linelength:=kb_length
end

proc pc_readlinen(filehandle handlex,ref char buffer,int size) =
	ref char p
	int n,x
	[0:100]char buff
	byte crseen
	type fntype=ref clang function (ichar,int32,filehandle)int
	int oldpos

	buffer^:=0

	fgets(buffer,size-2,handlex)

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

global proc pch_sread(variant fmt,variant dest) =
	int fmtcode
	char c

!pc_cfree(dest)
	fmtcode:=getreadfmtcode(fmt)
	kb_lastpos:=kb_pos
	kb_lastlength:=kb_length

	switch fmtcode
	when 'I' then
		stepkbpos(readint(kb_pos,kb_length,dest))

	when 'R' then
		stepkbpos(readreal(kb_pos,kb_length,dest))

	when 'N' then
!PUTS("SREAD/N")
		stepkbpos(readname(kb_pos,kb_length,dest))

	when 'S' then
		stepkbpos(readstring(kb_pos,kb_length,dest))

	when 'H' then
		stepkbpos(readhex(kb_pos,kb_length,dest))

	when 'B' then
		stepkbpos(readbin(kb_pos,kb_length,dest))

	when 'A' then
		stepkbpos(readany(kb_pos,kb_length,dest))

	when 'L' then
		if kb_length=0 then
!doemptystring::
			var_empty_string(dest)
		else
			var_make_stringn(kb_pos,kb_length,dest)
			kb_pos+:=kb_length
			kb_length:=0
		fi

	when 'C' then
		if kb_length=0 then
			var_empty_string(dest)
		else
			termchar:=kb_pos^
	dochar::
			dest.tagx:=tint
			dest.value:=termchar
			++kb_pos
			--kb_length
		fi

	when 'Z' then			! last terminator!
!	if termchar=0 then
!		goto doemptystring
!	fi
		goto dochar
	when 'E' then
		dest.tagx:=tint
		dest.value:=itemerror

	else
		prterror("SREAD/FMT?")
	endswitch
end

function readname(ref char s,int length,variant dest)ref char =
	ref char send
	ref char itemstr
	int itemlength
	send:=readitem(s,length,itemstr,itemlength)
	var_make_stringn(itemstr,itemlength,dest)

	iconvlcn(dest.objptr.ustr.strptr,dest.objptr.ustr.length)
	return send
end

function readstring(ref char s,int length,variant dest)ref char =
	ref char send
	ref char itemstr
	int itemlength
	send:=readitem(s,length,itemstr,itemlength)
	var_make_stringn(itemstr,itemlength,dest)
	return send
end

function readint(ref char sold,int length,variant dest)ref char =
!return point to next char after terminator (which can be just off length of string)
	ref char p,s				! s points to ^str
	ref char send
	ref char itemstr
	int itemlength,numlength

	send:=readitem(sold,length,s,itemlength)

	strtoint(s,itemlength,dest)

	return send
end

function readhex(ref char sold,int length,variant dest)ref char =
	[0:maxstrlen]char str		! local copy
	ref char p,s			! s points to ^str
	byte res
	i64 aa
	int a,t,nalloc
	char c

	if length=0 then
		dest.tagx:=tint
		dest.value:=0
		termchar:=0
		return sold
	fi

!copy to buffer first skip leading spaces, and any sign
	while (length and (sold^=' ' or sold^=9)) do
		++sold; --length
	od

	if length<=maxstrlen then	! use local buffer
		s:=&.str
		nalloc:=0
	else
		nalloc:=length+1
		s:=pcm_alloc(nalloc)
	fi

	p:=s				! p points to next char available
	while (length) do
		c:=toupper(sold^); ++sold; --length
		if c>='0' and c<='9' then
			p^:=c
			++p
		elsif c>='A' and c<='F' then
			p^:=c
			++p
		elsif c='_' then
		else
			termchar:=c
			exit
		fi
	od
	p^:=0				! use zero terminator for local string
	length:=p-s			! length of s

! try and work out type
	if length<=16 then
		t:=tint
	else
		t:=tdecimal
	fi
	p:=s
	switch t
	when tint then
		aa:=0
		do
			c:=p^; ++p
			if c=0 then
				exit
			fi
			if c<'A' then			! assume digit '0'..'9'
				aa:=aa*16+c-'0'
			else				! assume letter 'A'..'F'
				aa:=aa*16+(c-'A')+10
			fi
		od
		dest.tagx:=tint
		dest.value:=aa
	else
!	bx_makeu_base(s,strlen(s),dest,16)
		prterror("Readhex/long")
	endswitch

	if nalloc then
		pcm_free(s,nalloc)
	fi

	return sold
end

function readbin(ref char sold,int length,variant dest)ref char =
	[0:maxstrlen]char str		! local copy
	ref char p,s			! s points to ^str
	byte res
	i64 aa
	int a,t,nalloc
	char c

	if length=0 then
		dest.tagx:=tint
		dest.value:=0
		termchar:=0
		return sold
	fi

!copy to buffer first skip leading spaces, and any sign
	while (length and (sold^=' ' or sold^=9)) do
		++sold; --length
	od

	if length<=maxstrlen then	! use local buffer
		s:=&.str
		nalloc:=0
	else
		nalloc:=length+1
		s:=pcm_alloc(nalloc)
	fi

	p:=s				! p points to next char available
	while (length) do
		c:=toupper(sold^); ++sold; --length
		if c>='0' and c<='1' then
			p^:=c
			++p
		elsif c='_' then
		else
			termchar:=c
			exit
		fi
	od
	p^:=0				! use zero terminator for local string
	length:=p-s			! length of s

!try and work out type
	if length<=64 then
		t:=tint
	else
		t:=tdecimal
	fi

	p:=s
	switch t
	when tint then
		aa:=0
		do
			c:=p^; ++p
			if c=0 then
				exit
			fi
			aa:=aa*2+c-'0'
		od
		dest.tagx:=tint
		dest.value:=aa

	else
!	bx_makeu_base(s,strlen(s),dest,2)
		prterror("Readbin/long")
	endswitch

	if nalloc then
		pcm_free(s,nalloc)
	fi
	return sold
end

function readreal(ref char sold,int length,variant dest)ref char =
	[512]char str		! local copy
	real x
	ref char send
	ref char itemstr
	int itemlength,numlength

	send:=readitem(sold,length,itemstr,itemlength)
	strtoreal(itemstr,itemlength,dest)

	return send
end

function getreadfmtcode(variant p)int =
!p is a variant  which should point to a string containing a read format code.
!return that code as an upper when char code, eg. 'I'
	char c

!if p=nil or p.tag=tvoid then
	if p=nil or p.tag=tvoid then
!	return 'I'
		return 'A'
	fi
	if p.tag<>tstring then
	CPL "P=%s",ttname[p.tag]
		prterror("Readfmt?")
	fi
	if p.objptr.ustr.length=0 then
!	return 'I'
		return 'A'
	fi

	c:=toupper(p.objptr.ustr.strptr^)

	switch c
	when 'I', 'R', 'N', 'S', 'F', 'T', 'Z', 'C', 'L', 'H','B','A','E' then
		return c
	endswitch

	prterror("Readfmt2?")
	return 0
end

proc stepkbpos(ref char s) =
!a readxxx function has been called with kb_pos/kb_length, and has returned s to point to
!the character after the terminator
!adjust kb_pos/kb_length to point to that position
	int newlen

	newlen:=s-kb_pos

	if newlen=0 then		! nothing read probably was at end of buffer
		return
	fi
	if newlen>=kb_length then	! at end of buffer
		kb_pos:=kb_pos+kb_length	! point to just past buffer (but should never be accessed when kb_length=0)
		kb_length:=0
	else
		kb_pos:=kb_pos+newlen
		kb_length-:=newlen
	fi
end

function readany(ref char sold,int length,variant dest)ref char =
!read item as int, real or string depending on content
!return point to next char after terminator (which can be just off length of string)
	[0:maxstrlen]char str			! local copy
	ref char p,s				! s points to ^str
	byte signd,res
	i64 aa
	int digits,expon,other
	int t,nalloc
	char c

	ref char send
	ref char itemstr
	int itemlength,numlength

	itemerror:=0

	send:=readitem(sold,length,s,itemlength)

!now analyse item
!ints consist only of 0123456789+-_'
!reals consist only of 0123456789+-Ee.

	p:=s
	digits:=expon:=other:=0

	to itemlength do
		switch p++^
		when '0'..'9','+','-','_' then digits:=1
		when 'E','e','.' then expon:=1
		else other:=1
		end
	od

	dest.tagx:=tint

	if other or itemlength=0 then
		dest.value:='STR'
		

		var_make_stringn(s,itemlength,dest)
	elsif expon then
		strtoreal(s,itemlength,dest)
	else
		strtoint(s,itemlength,dest)
	fi

	return send
end

function readitem(ref char s,int length,ref char &itemstr,int &itemlength)ref char =		!READSTRING
!s points into the line buffer
!length is number of chars remaining in buffer
!identify a substring that can contain a name, int, real, string or filename
!return updated position of s that points past the item and past the immediate
!terminator 
!information about the read item is returned in itemstr, which points to
!the start of the item, and in itemlength. Item excludes any surrounding whitespace
!Item can be quoted, then the item points inside the quotes
!Any embedded quotes are removed, and the characters moved up. The item will
!be that reduced subsequence
!Note that this is destructive. On reread, the input will be different.
!I can mitigate this by adding spaces between the end of the item, and the next item,
!overwriting also the terminator. But this won't restore the line if one of the next
!reads is literal, using 'L' or 'C' codes.
	ref char p
	char quotechar, c

!scan string, eliminating leading white space
	while (length and (s^=' ' or s^=9)) do
		++s; --length
	od

	itemstr:=s				!assume starts here

	if length=0 then		! No more chars left to read return null string
		termchar:=0
		itemlength:=0
		return s
	fi

	quotechar:=0			! Allow possible enclosing single or double quotes
	if s^='"' then
		quotechar:='"'
		++s
		--length
	elsif s^='\'' then
		quotechar:='\''
		++s
		--length
	fi

!loop reading characters until separator or end reached
	p:=itemstr:=s

	while length do
		c:=s++^; --length
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
				if length and s^=quotechar then	! embedded quote
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

	if length=0 then
		termchar:=0
	fi
	itemlength:=p-itemstr				! actual length of token

	return s
end

proc strtoreal(ichar s,int length,variant dest)=
	[512]char str		! local copy
	real x
	int32 numlength

	dest.tagx:=treal

	if length>=str.bytes or length=0 then		!assume not a real
		dest.xvalue:=0.0
		return
	fi
	memcpy(&.str,s,length)
	str[length+1]:=0

	itemerror:=0

	if sscanf(&.str,"%lf%n", &x, &numlength)=0 or numlength<>length then
		if numlength=length then x:=0.0 fi
		itemerror:=1
	fi

	dest.xvalue:=x
end

proc strtoint(ichar s,int length, variant dest)=
!return point to next char after terminator (which can be just off length of string)
	[0:maxstrlen]char str			! local copy
	ref char p,q
	byte signd
	i64 aa
	int a,res,cat
	int t,nalloc
	char c

	itemerror:=0

	if length=0 then
		dest.tagx:=tint
		dest.value:=0
		return
	fi

!check for sign
	signd:=0
	if length and s^='-' then
		signd:=1; ++s; --length
	elsif length and s^='+' then
		++s; --length
	fi

	while s^='0' and length>1 do
		++s; --length
	od

	p:=q:=s				! p points to next char available

	while length do
		c:=q++^
		--length
		if c>='0' and c<='9' then
			p^:=c
			++p
		else
			if c='_' then
			else
				itemerror:=1
				exit
			fi
		fi
	od
	p^:=0				! use zero terminator for local string
	length:=p-s			! length of s

!classify magnitude of value as::
!'A' 0 to 2**63-1 or 0..9223372036854775807
!'B' 2**63 or 9223372036854775808
!'C' 2**63+1 to 2**64-1 or 9223372036854775809..18446744073709551615
!'D' 2**64 and over or 18446744073709551616 and over

	if length<=18 then
		cat:='A'
	elsif length=19 then
		case cmpstring(s,"9223372036854775808")
		when -1 then cat:='A'
		when 0 then cat:='B'
		else cat:='C'
		esac
	elsif length=20 then
		if cmpstring(s,"18446744073709551615")<=0 then
			cat:='C'
		else
			cat:='D'
		fi
	else
		cat:='D'
	fi

!now look at sign::
	if signd then
		case cat
		when 'B' then cat:='A'		!-922...808 can be int64
		when 'C' then cat:='D'		!needs longint
		esac
	fi

!convert cat to type

	case cat
	when 'A' then t:=tint
	when 'B','C' then t:=tword
	else t:=tdecimal
	esac

	p:=s
	if t<>tdecimal then
		aa:=0
		do
			c:=p^; ++p
			if c=0 then
				exit
			fi
			aa:=aa*10+(c-'0')
		od
		if signd then
			aa:=-aa
		fi
		dest.tagx:=t
		dest.value:=aa

	else
PCERROR("BX/MAKESTR")
!		bx_makestr(s,length,dest)
	fi
end

proc printnextfmtchars(int lastx) =
!o/p chars from fmtstr until # or eos is encountered
	char c
	ref char pstart
	int n

	pstart:=mfmtcurr
	n:=0

	do
		c:=mfmtcurr^
		switch c
		when '#' then
			if lastx then
				goto skip
			fi
			++mfmtcurr
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
			++mfmtcurr
			c:=mfmtcurr^
			if c then
				++mfmtcurr
				printstr_n(&c,1)
			fi
			pstart:=mfmtcurr
		else
skip::
			++n
			++mfmtcurr
		endswitch
	od
end

proc pch_setformat(variant p) =
	int n
	ref char s

	if p.tag<>tstring then
		prterror("(str)")
	fi
	if mfmtstr then
		prterror("Setfmt?")
	fi
	n:=p.objptr.ustr.length
	mfmtstr:=pcm_alloc(n+1)
	if n then
		memcpy(mfmtstr,p.objptr.ustr.strptr,n)
	fi
	s:=mfmtstr+n
	s^:=0

	mfmtcurr:=mfmtstr
end

global function pc_getfmt(variant p,ref fmtrec fmt)ref fmtrec=
!p is an optional fmt string to tostr and print
!turn into a proper format
!return pointer to a format, or to the default format is not supplied
!fmt points to a fmtrec in the caller to contain the processed format

if p=nil or p.tag=tvoid then
	return &defaultfmt
else
	if p.tag<>tstring then
		prterror("pc_getfmt/not str?")
	fi
	if p.objptr.ustr.strptr=nil then
		return &defaultfmt
	else
		pc_strtofmt(p.objptr.ustr.strptr,p.objptr.ustr.length,fmt)
		return fmt
	fi
fi
end

global proc pc_strtofmt(ref char s,int slen,ref fmtrec fmt) =
!convert format code string in s, to fmtrec at fmt^
!Format code is a string containing the following char codes (upper or lower when mostly)
!'	Add single quotes around string (and deal with embedded quotes)
!+	Always have + or - in front of integers
!n	Width
!.n	Max width/precision
!A	Convert to upper when
!a	Convert to lower when
!B	Binary
!C	Show int as single n-bit (unicode) character
!E,F,G	Specify format for double (corresponds to C format codes)
!H	Hex
!JL	Justify left
!JR	Justify right
!JC	Justify centre
!M	Show int as multiple 8-bit characters (lsb on left)
!O	Octal
!Pc	Use padding char c
!Q	Add double quotes around string (and deal with embedded quotes)
!Sc	Use separator char c between every 3 or 4 digits
!Tc	Use terminator char c (typically B or H)
!U	Show ints as unsigned
!Y	Add type suffix
!Z	Use 0 padding

	int c
	byte wset
	int n
	[0:100]char str

	initfmtcode(fmt)

	memcpy(&.str,s,slen)		!convert s/slen to zero-terminated string
	str[slen]:=0
	s:=&.str

	wset:=0
	while s^ do
		c:=s^
		++s
		switch c
		when 'B', 'b' then fmt.base:=2
		when 'H', 'h' then fmt.base:=16
		when 'O', 'o' then fmt.base:=8
		when 'X', 'x' then
			c:=s^
			if c then
				switch c
				when '2'..'9' then c:=c-'0'
				when '1' then
					++s
					c:=s^
					if c in '0'..'6' then
						c:=c-'0'+10
					fi
				else
					c:=10
				end
				fmt.base:=c
				++s
			fi

! fmt.base:=8
		when 'Q', 'q' then fmt.quotechar:='"'
		when '~' then fmt.quotechar:='~'
		when 'J', 'j' then
			fmt.justify:=toupper(s^)
			if s^ then
				++s
			fi
		when 'A' then fmt.lettercase:='A'
		when 'a' then fmt.lettercase:='a'
		when 'Z', 'z' then fmt.padchar:='0'
		when 'S', 's' then
			fmt.sepchar:=s^
			if s^ then
				++s
			fi
		when 'P', 'p' then
			fmt.padchar:=s^
			if s^ then
				++s
			fi
		when 'T', 't' then
			fmt.suffix:=s^
			if s^ then
				++s
			fi
		when 'U', 'u' then fmt.usigned:='U'
		when 'E', 'e' then fmt.realfmt:='e'
		when 'F', 'f' then fmt.realfmt:='f'
		when 'G', 'g' then fmt.realfmt:='g'
! when '0','1','2','3','4','5','6','7','8','9' then
		when '.' then
			wset:=1
		when comma,'_' then fmt.sepchar:=c
		when '+' then fmt.plus:='+'
		when 'M', 'm' then fmt.charmode:='M'
		when 'C', 'c' then fmt.charmode:='C'
		when 'Y', 'y' then fmt.showtype:='Y'
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
				if not wset then
					fmt.minwidth:=min(n,onesixty-1)
					wset:=1
				else
					fmt.precision:=min(n,100)
				fi
			fi
		endswitch
	od
end

proc initfmtcode(ref fmtrec f) =
	f^:=defaultfmt
end

function i64mintostr(ref char s,int base,int sep)int =
!convert minint to string in s do not include minus sign
!return number of chars in string
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

function u64tostr(u64 aa,ref char s,word base,int sep)int =
!convert 64-bit int a to string in s^
!base is number base, usually 10 but can be 2 or 16. Other bases allowed
!result when a=minint (will give "<minint>")
	[0:onesixty]char t
	int i,j,k,g
	int dummy
	ref char s0
!INT XX

	i:=0
	k:=0
	g:=(base=10|3|4)

	repeat
		t[++i]:=digits[word(aa rem base)]

!CPL =XX

		aa:=aa/base
		if sep and aa<>0 and ++k=g then
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

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt,int usigned)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt.unsigned forces ints to be treated as longs)
!returns length of s
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w
	static u64 mindint=0x8000'0000'0000'0000

	if fmt.usigned then
		usigned:=1
	fi

	if aa=mindint and not usigned then		! minint

		str[0]:='-'
		n:=i64mintostr(&str[1],fmt.base,fmt.sepchar)+1
	else
		if (not usigned and aa<-0) or fmt.plus then
			if aa<0 then
				aa:=-aa
				str[0]:='-'
			else
				str[0]:='+'
			fi
			n:=u64tostr(aa,&str[1],fmt.base,fmt.sepchar)+1
		else
			n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)
		fi
	fi

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt.base>10 or fmt.suffix and fmt.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w
!static u64 mindint=0x8000'0000'0000'0000

	n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt.base>10 or fmt.suffix and fmt.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function strtostrfmt(ref char s,ref char t,int n,ref fmtrec fmt)int =
!s is a string process according to fmtrec fmt^, and return result in t
!caller should check whether any changes are required to s (now it can just use s), but this
!check is done here anyway (with a simple copy to t)
!n is current length of s
!return length of t
!Three processing stages::
!1 Basic input string s
!2 Additions or mods: quotes, suffix, when conversion
!3 Width adjustment
!1 is detected here, 2 is done here, 3 is done by expandstr
	ref char u,v
	[256]char str
	int w,nheap		! whether any heap storage is used # bytes allocated

	nheap:=0

	if fmt.quotechar or fmt.lettercase then		! need local copy
		if n<256 then
			u:=&.str
		else
			nheap:=n+3					! allow for quotes+terminator
			u:=pcm_alloc(nheap)
		fi
		if fmt.quotechar then
			v:=u
			v^:=fmt.quotechar
			++v
			if n then
				strcpy(v,s)
				v+:=n
			fi
			v^:=fmt.quotechar
			++v
			v^:=0
			n+:=2
		else
			memcpy(u,s,n)
		fi
		switch fmt.lettercase
		when 'a' then	! need lower when
			convlcstring(u)
		when 'A' then
			convucstring(u)
		endswitch
		s:=u
	fi

	w:=fmt.minwidth
	if w>n then
! fmt.base:=0
		n:=expandstr(s,t,n,fmt)
	else
		memcpy(t,s,n)
	fi
	if nheap then
		pcm_free(u,nheap)
	fi
	return n
end

function expandstr(ref char s,ref char t,int n,ref fmtrec fmt)int =
!s contains a partly stringified value.
!widen s if necessary, according to fmt, and copy result to t
!n is current length of s
!note) = for non-numeric strings, fmt.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string caller must take care of this
!result will be zero-terminated, for use in this module

	int i,w,m

!check to see if result is acceptable as it is
	w:=fmt.minwidth
	if w=0 or w<=n then		! allow str to be longer than minwidth
		strncpy(t,s,n)
		(t+n)^:=0
		return n
	fi

	if fmt.justify='L' then	! left-justify
! strcpy(t,s)
		strncpy(t,s,n)
		t+:=n
		for i:=1 to w-n do
			t^:=fmt.padchar
			++t
		od
		t^:=0
	elsif fmt.justify='R' then
		if fmt.padchar='0' and fmt.base and (s^='-' or s^='+') then ! need to move sign outside 
			t^:=s^
			++t
			to w-n do
				t^:=fmt.padchar
				++t
			od
			strncpy(t,s+1,n-1)
			(t+n-1)^:=0
		else
			to w-n do
				t^:=fmt.padchar
				++t
			od
			strncpy(t,s,n)
			(t+n)^:=0
		fi

	else				! centre-justify?

		m:=(w-n+1)/2
		to m do
			t^:=fmt.padchar
			++t
		od
		strncpy(t,s,n)
		t+:=n
		to w-n-m do
			t^:=fmt.padchar
			++t
		od
		t^:=0

	fi
	return w
end

global proc addstring(object p,ref char t,int n=-1) =
!p is a pointer to an object string data, initially with an empty string
!store string t to to p, or append to an existing string
!n is the length of the string (-1 if not known) =
	int oldlen,newlen,oldbytes,newbytes
	ref char newptr

	if n=0 or t^=0 then
		return
	fi
	if n<0 then
		n:=strlen(t)
	fi

	oldlen:=p.ustr.length

	if p.refcount=0 then	! assume a fixed buffer
		if oldlen=0 then		! first string
			memcpy(p.ustr.strptr,t,n)
			p.ustr.length:=n
		else				! append to existing string
			memcpy(p.ustr.strptr+oldlen,t,n)
			p.ustr.length:=oldlen+n
		fi
		return
	fi

	if oldlen=0 then		! first or only string
		p.ustr.strptr:=pcm_alloc(n)
		p.ustr.length:=n
		p.ustr.allocated:=allocbytes
		memcpy(p.ustr.strptr,t,n)

	else				! append to existing string
		newlen:=oldlen+n
		oldbytes:=p.ustr.allocated
		newbytes:=oldlen+n
		if newbytes<=oldbytes then 		! fits in current allocation
			memcpy(p.ustr.strptr+oldlen,t,n)
		else					! need new allocation
			newptr:=pcm_alloc(newbytes)
			memcpy(newptr,p.ustr.strptr,oldlen)	! existing chars
			memcpy(newptr+oldlen,t,n)		! add new chars
			p.ustr.allocated:=allocbytes
			pcm_free(p.ustr.strptr,oldbytes)
			p.ustr.strptr:=newptr
		fi
		p.ustr.length:=newlen
	fi
end

proc domultichar (ref char p,int n,ref char dest,ref fmtrec fmt) =
!there are n (4 or 8) chars at p.!
!There could be 0 to 4 or 8 printable chars converted to string at dest
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

	expandstr(str,dest,nchars,fmt)
end

proc printstr_n(ref char s,int n) =
!send string s to current m output device
!n is::
! -1:	s is zero-terminated; calculate length
! 0:	s is empty string (no output)
! >0:	n is length of string
	variant  p
	int x
	type fntype= ref clang function (filehandle f, ichar s, int i, ichar t)int

	if n=-1 then		! was stringz
		n:=strlen(s)
	fi

	if n=0 then
		return
	fi

	switch moutdev
	when std_io then
		printstrn_app(s,n,nil)

	when file_io then
		printstrn_app(s,n,cast(moutchan))

	when str_io then
		addstring(moutvar.objptr,s,n)

	when istr_io then
		p:=moutvar.varptr
		if p.tag<>tstring then
			prterror("prtstrn1")
		fi
		addstring(moutvar.objptr,s,n)

	when wind_io then
		
	endswitch
end

proc tostr_int(variant p,ref fmtrec fmt,object dest) =
	[0:onesixty]char str

	switch fmt.charmode
	when 'M' then
		domultichar(ref char(&p.value),8,str,fmt)

	when 'C' then
		str[1]:=p.value
		str[2]:=0

	else
		i64tostrfmt(p.value,str,fmt,0)
	endswitch

	if fmt.showtype then
		addstring(dest,"I:",2)
	fi

	addstring(dest,str,strlen(str))
end

proc tostr_word(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str

	switch fmt^.charmode
	when 'M' then
		domultichar(ref char(&p.uvalue),8,str,fmt)

	when 'C' then
		str[1]:=p.uvalue
		str[2]:=0

	else
		u64tostrfmt(p.value,str,fmt)
	endswitch

	if fmt.showtype then
		addstring(dest,"W:",2)
	fi

	addstring(dest,str,strlen(str))
end

proc tostr_real(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str,str2
	[0:10]char cfmt
	int n

	cfmt[0]:='%'

	if fmt.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt.realfmt
		cfmt[4]:=0
		sprintf(str,cfmt,fmt.precision,p.xvalue)
	else
		cfmt[1]:=fmt.realfmt
		cfmt[2]:=0
		sprintf(str,cfmt,p.xvalue)
	fi

!at this point, n is the str length including signs and suffix
	n:=strlen(str)		! current length
	if n<fmt.minwidth then
		expandstr(str,str2,n,fmt)
		strcpy(str,str2)
	fi

	addstring(dest,str,strlen(str))
end

proc tostr_str(variant p, ref fmtrec fmt, object dest) =
	int oldlen,newlen
	ref char s
	[0:100]char str
	object q

!try and work out size of formatted string
	q:=p.objptr
	oldlen:=q.ustr.length
	newlen:=oldlen

	if fmt.quotechar or fmt.minwidth>newlen then
		if fmt.quotechar then
			newlen+:=2
		fi
		if fmt.minwidth>newlen then
			newlen:=fmt.minwidth
		fi
		s:=pcm_alloc(newlen+1)
		strtostrfmt(q.ustr.strptr,s,oldlen,fmt)
		addstring(dest,s,newlen)
		pcm_free(s,newlen+1)
	else
		addstring(dest,q.ustr.strptr,oldlen)
	fi
end

proc pch_tostr(variant a, b, result)=
	fmtrec fmt
	ref fmtrec ifmt
	object p

	ifmt:=pc_getfmt(b,&fmt)

	p:=obj_new_string(0)

	listdepth:=0

	tostr(a,ifmt,p)

	result.tagx:=tstring ior hasrefmask
	result.objptr:=p
end

proc tostr_list(variant p, ref fmtrec fmt, object dest) =
	variant q
	int i,n
	char c
	object r

	++listdepth

	r:=p.objptr
	if r.refcount<0 or listdepth>maxlistdepth then
		addstring(dest,"...",3)
		--listdepth
		return
	fi

	addstring(dest,"(",1)

	r.refcount:=-r.refcount
	q:=r.ulist.varptr

	if p.tag=tlist then
		n:=p.objptr.ulist.length
	else
		n:=ttlength[p.usertag]
	fi

!for i:=p.objptr.ulist.length downto 1 do
	for i:=n downto 1 do
		tostr(q,fmt,dest)
		++q
		if i<>1 then
			addstring(dest,",",1)
		fi
	od
	addstring(dest,")",1)
	r.refcount:=-r.refcount
	--listdepth
end

proc tostr_range(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str

	i64tostrfmt(p^.range_lower,str,fmt,0)
	strcat(str,"..")
	addstring(dest,str)
	i64tostrfmt(p^.range_upper,str,fmt,0)
	addstring(dest,str)
end

proc tostr_array(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str
	ref byte q
	int i,m,elemtype,a,b
	varrec v
	object pa
	ref byte ptr

	m:=p.tag
	pa:=p.objptr

	a:=pa.uarray.lower
	elemtype:=pa.uarray.elemtype
	b:=pa.uarray.length+a-1

	q:=pa.uarray.ptr

	fprint @str,"#[#:#]",ttname[m],pa.uarray.lower,ttname[elemtype]
	addstring(dest,str)
	addstring(dest,"A(")

	for i:=a to b do

		var_loadpacked(q,elemtype,&v,nil)
		q+:=ttsize[elemtype]
		tostr(&v,fmt,dest)
		if i<b then
			addstring(dest,",",1)
		fi
	od
	addstring(dest,")",1)
end

proc tostr_bits(variant p, ref fmtrec fmt, object dest) =
	[0:onesixty]char str
	ref byte q
	int i,m,elemtype,a,b,bitwidthx,offset
	varrec v
	object pa
	ref byte ptr

	m:=p.tag
	pa:=p.objptr

	a:=pa.ubits.lower
	elemtype:=pa.ubits.elemtype
	b:=pa.ubits.length+a-1
	bitwidthx:=ttbitwidth[elemtype]
	offset:=pa.ubits.indexoffset*bitwidthx
!CPL "PRINT",=OFFSET,=BITWIDTHX,=PA,=PA.UBITS.INDEXOFFSET

	q:=pa.ubits.ptr

	fprint @str,"#[#:#]",ttname[m],pa.ubits.lower,ttname[elemtype]
	addstring(dest,str)
	addstring(dest,"A(")

!CPL "PRINT101",=ELEMTYPE,PA.UBITS.ELEMTYPE

	for i:=a to b do

!CPL "PRINT102",=ELEMTYPE,PA.UBITS.ELEMTYPE
		var_loadbit(q,offset,elemtype,0,&v)
!CPL "PRINT103",=ELEMTYPE,PA.UBITS.ELEMTYPE
		offset+:=bitwidthx
		if offset>=8 then
			offset:=0
			++q
		fi
!CPL "PRINT104",=ELEMTYPE,PA.UBITS.ELEMTYPE,=TTNAME[V.TAG]
		tostr(&v,fmt,dest)
!CPL "PRINT105",=ELEMTYPE,PA.UBITS.ELEMTYPE
		if i<b then
			addstring(dest,",",1)
		fi
	od
	addstring(dest,")",1)
!CPL "PRINT201",=ELEMTYPE,PA.UBITS.ELEMTYPE
end

proc tostr_struct(variant p, ref fmtrec fmt, object dest) =
!	[0:onesixty]char str
	ref byte q
	int i,m,nfields,needcomma
	varrec v
	object pa
	ref byte ptr
	symbol d
	ref symbol r

!CPL "PRINTSTRUCT"

	m:=p.usertag
	pa:=p.objptr

!CPL =P, =PA
	d:=pa.ustruct.structdef
	if d=nil then d:=ttnamedef[m] fi
!CPL =D
	r:=d.topfieldlist
	nfields:=ttlength[m]

	needcomma:=0
	addstring(dest,"(")

	for i to nfields do
!CPL I,R.NAME,R.FIELDOFFSET,STRMODE(R.MODE)
		var_loadpacked(pa.ustruct.ptr+r.fieldoffset, r.mode, &v, nil)
		if needcomma then
			addstring(dest,",")
		fi
		needcomma:=1

		tostr(&v,fmt,dest)
		++r
	od
	addstring(dest,")")
end

proc tostr_set(variant p,ref fmtrec fmt,object dest) =
	[0:onesixty]char str
	variant q
	int i,j,first
	varrec v
	object s

	if fmt=nil then
		fmt:=&defaultfmt
	fi

	addstring(dest,"[",1)

	s:=p.objptr

	first:=1

	i:=0
	while (i<s.uset.length) do
		if testelem(cast(s.uset.ptr),i) then	! element i included
			j:=i+1				! now search for end of this '1' block
			while (j<s.uset.length and testelem(cast(s.uset.ptr),j)) do
				++j
			od
			--j				! last '1' in group
			if not first then
				addstring(dest,",",1)
			fi
			first:=0
			if i=j then
				v.tagx:=tint
				v.value:=i
			else
				v.tagx:=trange
				v.range_lower:=i
				v.range_upper:=j
			fi
			tostr(&v,fmt,dest)
			i:=j+1
		else
			++i
		fi
	od
	addstring(dest,"]",1)
end

proc tostr_decimal(variant p,ref fmtrec fmt,object dest) =
	ref char s

	s:=var_tostr_decimal(p,0)
	addstring(dest,s,-1)
	free(s)
end

proc tostr(variant p, ref fmtrec fmt, object dest) =
	[1024]char str

	switch p.tag
	when tint then
		tostr_int(p, fmt, dest)
	when tword then
		tostr_word(p, fmt, dest)
	when treal then
		tostr_real(p, fmt, dest)
	when tstring then
		tostr_str(p, fmt, dest)
	when trange then
		tostr_range(p, fmt, dest)
	when tlist,trecord then
		tostr_list(p, fmt, dest)
	when tarray,tcarray then
		tostr_array(p, fmt, dest)
	when tbits then
		tostr_bits(p, fmt, dest)
	when tset then
		tostr_set(p, fmt, dest)
	when tstruct then
		tostr_struct(p, fmt, dest)
	when tdecimal then
		tostr_decimal(p, fmt, dest)
!	when tdict then
	when tvoid then
		addstring(dest,"<Void>")
!	when trefvar then
	when trefvar then
		print @str,"Refvar:",,p.varptr
		addstring(dest,str)
		if p.varptr then
			fprint @str," <#>",ttname[p.varptr.tag]
			addstring(dest,str)
	fi

	when trefpack then
		fprint @str,"Ref #:#",ttname[p.uref.elemtag],p.uref.ptr
		addstring(dest,str)

	when trefbit then
		fprint @str,"Refbit #:# (#,#)",ttname[p.uref.elemtag],p.uref.ptr,p.uref.bitoffset,p.uref.bitlength
		addstring(dest,str)

	when tsymbol then
		if p.def then
			fprint @str,"<#:""#"">",namenames[p.def.nameid],p.def.name
			addstring(dest,str)
		else
			addstring(dest,"<nil>")
		fi
	when ttype then
!		fprint @str,"<#>",ttname[p.value]!+(p.value<=tlast|1|0)
		fprint @str,"#",ttname[p.value]!+(p.value<=tlast|1|0)
		addstring(dest,str)
	else
		pcustype("Tostr:",p)
	end
end

=== qq_show.m 19/32 ===
import* qq

!labels are just numbers 1,2,3 which index both of these tables
!labelblocktable is the pclblock no (as all labels are shared across the program)
!labeloffsettable is the offset into the pclblock

!global const labelinitalloc=8192
!global var ref[]int labeloffsettable
!global var int labelalloc
!global var int nextlabelno
var ref[0:]int labelmap
var int currlineno
symbol currpclproc

var strbuffer pclv
global var ref strbuffer pcldest = &pclv

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
!p is a tagrec
var ref unitrec q
var symbol d
var int t,flags
var ichar idname
var int64 a
var real32 x32

if p=nil then
	return
fi
	currlineno:=p^.pos iand 16777215
!CPL "PRINTUNIT:",P,P.TAG,JTAGNAMES[P.TAG],=LEVEL,CURRLINENO


!if p^.lineno then
!fi

print @dev,p,":"
print @dev,getprefix(level,prefix,p)

idname:=jtagnames[p.tag]
if idname^='j' then ++idname fi
print @dev,idname,,": "


case p^.tag
when jname,jhostname then
	d:=p^.def
	print @dev,d^.name,namenames[d.nameid],"Module:",p.moduleno
	if d.truename then
		print @dev," ",d.truename
	fi

when jintconst then
	print @dev,p^.value

when jwordconst then
	print @dev,p^.uvalue

when jint128const then
	print @dev,p^.qupper,p^.qlower

when jword128const then
	print @dev,p^.qupper,p^.qlower

when jrealconst then
	print @dev,p^.xvalue

when jstringconst then
	fprint @dev,"""#""",p.svalue

when jdecimal then
	print @dev,p^.svalue,,"L"

when jcmpchain then
	for i to 4 do
!CP P.CMPGENOP[I],$
		if p.cmpgenop[i]=0 then exit fi
		print @dev,jtagnames[p.cmpgenop[i]],$
	od
!CPL JTAGNAMES.LEN

!when jmakestrtype then
!	print @dev, ttname[p.strtype]

!when jimport then
!	print @dev, p^.def^.name

!when jprocdef then
!	print @dev, p^.def^.name
!	if p.mglobal then print @dev, " (global)" fi
!	if p.isfn then print @dev, " (func)" fi
!
!when jrecorddef then
!	print @dev, p^.def^.name
!	if p.mglobal then print @dev, " (global)" fi
!
!when jdocstring then
!	print @dev,"#",p^.svalue,P^.LENGTH
!
when jmakelist then
	print @dev,p.lower,,":",=p.length,ttname[p.elemtype]

!when jnew then
!	print @dev,p.nparams

!when jframesize then
!	print @dev,"Framesize",p.value
!
!when jconvert,jtypepun then
!	print @dev,"Mode",ttname[p.mode]
!
when jtypeconst,jconvert then
	print @dev,ttname[p.mode]

!when jpacktypeconst then
!	print @dev,getpacktypename(p.value)

!when kdecimal then
!	print @dev,p^.svalue,"Len:",p^.slength
!
!when ktypeconst then
!	print @dev,typename(p^.mode),typename(p^.value)
!
!when koperator then
!	print @dev,pclnames[p^.opcode]+1
!
!when kconvert,ktypepun then
!	print @dev,convnames[p^.opcode]," to:",strmode(p^.newmode)
!
!when kmakelist,kmultexpr then
!	print @dev,"Len:",p^.length
!
!when kdot then
!	print @dev,"Offset:",p^.offset
!
when jcallhost then
	print @dev,hostfnnames[p.index]+5

!when kindex, kptr then
!
!when kexit,kredo,krestart,knext then
!	print @dev,"#",,p^.index
!
esac

println @dev
flags:=jflags[p.tag]

if flags>=1 then printunitlist(dev,p.a,level+1,"1") fi
if flags=2 then printunitlist(dev,p.b,level+1,"2") fi

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
static var [1024]char str
var [1024]char indentstr
var [16384]char modestr

indentstr[1]:=0
!if level>10 then level:=10 fi
if level>20 then level:=10 fi

to level do
	strcat(&.indentstr,"- ")
od

!!sprintf(&.modestr,"%s",strmode(p^.mode))
!sprintf(&.modestr,"%s:%s",(p^.popflag|"POP"|"---"),strmode(p^.mode))
!modestr[256]:=0
!
!strcat(&.modestr,"-----------------------------")
!modestr[17]:=' '
!modestr[18]:=0

strcpy(&.str,getlineinfok())
!strcat(&.str,&.modestr)
strcat(&.str,&.indentstr)
strcat(&.str,prefix)
if prefix^ then
	strcat(&.str," ")
fi

return &.str
end

function getlineinfok:ichar=			!GETLINEINFO
	static var [40]char str

	sprintf(&.str,"%04d ",currlineno)
	return &.str
end

proc writepcl(ref int pcstart,pc, ref int32 pclsource, int pass)=
!write pc instruction to ttdeststr, as a single line of pcl
!index is index of ins in pccode/pcdata
var [512]char str
var qd fmt
var int cmdcode,a,needcomma,i,offset,labeldone,commentdone,soffset
var ref strec d
const tabx="!      ----------"

cmdcode:=pc^

memcpy(&fmt,&pclfmt[cmdcode],fmt.bytes)
labeldone:=commentdone:=0

case cmdcode
when kprocdef then
!	CPL "--PROCDEF",SYMBOL(PC^).NAME
	currpclproc:=cast((pc+1)^)
	return
when kprocparams then
	gstr(tabx)
	gstr("Procdef:")
	gstr(currpclproc.name)
	gstr(" (")
	gstrint((pc+1)^)
	gstr(" )")
	gline()
	return
when kprocend then
	gstr(tabx)
	gstrln("End")
	return
esac

offset:=PC-PCSTART

soffset:=(pclsource+offset)^
!CPL =SOFFSET,=OFFSET, (PCLSOURCE+OFFSET-1)^, (PCLSOURCE+OFFSET+1)^

!IF SOFFSET=0 THEN
!	GSTR("!---------------------------???")
!else
!	GETERRORINFO(SOFFSET,1)
!
!!	GSTR("ABC")
!	gstr("!---------------------------")
!	GSTR(errorline)
!FI
!gline()

++pc

if currlineno then
!	sprintf(&.str,"%8x %5d: %4d: ",pc-1,currlineno,pc-pcstart-1)
	fprint @str,"# #: #: ",pc-1:"8zh", currlineno:"5", pc-pcstart-1:"4"
else
!	sprintf(&.str,"%8x %4d: ",pc-1,pc-pcstart-1)
	fprint @str,"# #: ",pc-1:"8zh",pc-pcstart-1:"4"
fi

gstr(&.str)

offset:=pc-pcstart-1


if labelmap^[offset] then
!CPL "WRITEPCL LABEL",=OFFSET
	glabeldef(offset)
	gstr(&.str)
fi

case cmdcode
!when kprocstart then
!	gstr("PROC:")
!	d:=ref strec(pc^)
!	gstr(d^.name)
!	gstrln(":")
!	return
when kprocdef then
!	CPL "--PROCDEF",SYMBOL(PC^).NAME
	currpclproc:=cast(pc^)
	return
when kprocparams then

!when kprocend then
!	gline()
!	return
when kcomment then
	gstr("! ")
	gstrln(cast(pc^))
	return

when klabel then
	gstr("@@L")
	gstrint(pc++^)
	gstrln(":")
	return

when klabeldef then
!	gstr("L")
!	gstr(nametable[pc++^])
	gstr(ref strec(pc^).name)
GSTR(" /")
GSTR(NAMENAMES[REF STREC(PC^).NAMEID])
	++PC

	gstrln(":")
	return

!when kprocdef then
!	stcurrrunproc:=cast(pc^)

esac


!if cmdcode in [klabel,kcomment] then
!	return
!fi

strcpy(&.str,pclnames[cmdcode]+1)

a:=1
!case cmdcode
!when kcallhost then
!	a:=2			!skip 1st operand which is the hostfn code already shown
!	strcat(&.str,".<HOSTNAME>")
!!	strcat(&.str,".")
!!	strcat(&.str,hostfnnames[ptr++^]+5)
!esac

!gs_leftstr(pcldest," ",11,'-')
gs_leftstr(pcldest," ",7,'-')
!gs_leftstr(pcldest,&.str,23)
gs_leftstr(pcldest,&.str,10)
gstr("     ")

needcomma:=0

for i:=a to 4 do
	case fmt[i]
	when cnone then
		exit
	else
		if needcomma then gstr(", ") fi

		strcpy(&.str,writepclopnd(fmt[i],pc++^,i,cmdcode, pass, pcstart))
		gstr(&.str)
		needcomma:=1
	esac
od

gline()
end

function writepclopnd(int fmt,int64 x,int n,cmdcode, pass,ref int pcstart)ichar=		!WRITEPCLOPND
!f=o/p channel
!fmt=single operand code
!x is value of operand
!n is operand @ (1..4)
static var [512]char str,str2
var symbol d
var ichar suffix,s
var int slen
var object p

d:=ref strec(x)

case fmt
when cnone then
	return "None"

when cint,cword then

	case fmt
	when cint then suffix:=""
	when cword then suffix:="u"
	else
		suffix:=""
	esac
	print @str,x,,suffix,$
	if cmdcode=kcallhost then
		strcat(str,hostfnnames[x]+5)
	fi

when creal then
	strcpy(str,strreal(real@(x)))

when crange then
	fprint @str,"#..#",x iand 0xFFFF'FFFF,x>>32

when cstring then
	if pass=1 then
		s:=cast(x)
		slen:=strlen(s)
		goto dostring
	else
!RETURN "<STR>"
		p:=cast(x)
		if (slen:=p.ustr.length)=0 then return """" fi
		s:=p.ustr.strptr
		goto dostring
	fi

when cstringz then

	s:=cast(x)
	slen:=strlen(s)
dostring::
	if slen>=255 then slen:=255 fi
	memcpy(&.str,s,slen)			!truncate too-long strings
	str[slen+1]:=0
	convertstring(&.str,&.str2)
	fprint @str,"""#""",&.str2

when cmemory then
	if pass=1 then
		strcpy(str,d.name)
	else
		d:=nil
		for i to nstatics do
			if statictable[i]=variant(x) then
				d:=staticdefs[i]
				exit
			fi
		od

		fprint @str, "[#] (#:#)", x:"h",(d|d.owner.name|"?"),(d|d.name|"?")
	fi

when cframe then
	if pass=1 then
		strcpy(str,d.name)
	else
		d:=currpclproc.deflist

		while d do
			if d.nameid in [frameid, paramid] and d.index*16=x then
				fprint @str,"[#] (#)",x%16,d.name
				return str
			fi
			d:=d.nextdef
		od
	fi

when csymbol then
	fprint @str,"[#]",d.name

when cproc then
	if pass=1 then
		strcpy(str,d.name)
	else
		d:=nil
		for i to nprocs do
			if proctable[i]=ref int(x) then
				d:=procdefs[i]
				exit
			fi
		od

		fprint @str, "[#] (#)", x:"h",(d|d.name|"?")
	fi

when cdllproc then
	fprint @str,"[DLL:#]",getdottedname(d)

when cgenfield then
	if pass=1 then
		d:=symbol(x)
		fprint @str,".#",d.name
	else
		fprint @str,"## (#)","#",x, genfieldtable[x].def.name
	fi
!
when ctype then
	fprint @str,"T:# <#>",strmode(x),int(x)

when clabel then
	if pass=1 then
		fprint @str,"L#",x
	else
		fprint @str,"&# (L#)",x:"h",ref int(x)-pcstart
	fi
else
other::
	fprint @str,"<# #>",fmt,opndnames[fmt]
esac
return str
end

global proc writeallpcl(int n, pass)=
!display code currently in pccode/pcopnd
var int cmd,i,lastline,line,labno,offset,index,size,nopnds,x,y
var ref int pc,pclcode
var ref int32 pclsource
var ichar name

currlineno:=0

!if n=1 then
!	gs_init(pcldest)
!fi

!CPL "WRITEALLPCL",N,=PASS

if pass=2 and not hasbytecodes then
	gstrln("Can't show PCL; use -debug")
	return
fi

gstr("PCL FOR MODULE:")
gstrln(moduletable[n].name)

pc:=pclcode:=moduletable[n].pcstart
pclsource:=moduletable[n].pcsrcstart

size:=moduletable[n].pcsize
labelmap:=pcm_allocz((size+1)*int.bytes)

!CPL "ALLPCL",PC
repeat
!CPL "A1",PC
	cmd:=pc^

	nopnds:=pclnopnds[cmd]

!CPL PCLNAMES[CMD],=NOPNDS,PC
	for i to nopnds do
		case pclfmt[cmd,i]
		when cnone then
			exit
		when clabel then
			x:=(pc+i)^
			if pass=2 then
				x:=ref int(x)-pclcode
			fi

			labelmap^[x]:=1		!allow up to 4 labels at this loc
		esac

	od
	pc+:=pclnopnds[cmd]+1
until cmd in [kzero,kendmodule]

pc:=moduletable[n].pcstart

!CPL "ALLPCL2",PC
repeat
	cmd:=pc^

!CPL PC,=PCLNAMES[CMD]

	writepcl(pclcode,pc,pclsource, pass)
!CPL "DONEWRITEPCL"
	pc+:=pclnopnds[cmd]+1
until cmd in [kzero,kendmodule]
!CPL "DONE"

gline()
pcm_free(labelmap,size+1)

!gstrln("Names from hashtable:")
!for i in hashtable.bounds when hashtable[i].namelen and hashtable[i].symbolcode=namesym do
!	gstr("	")
!	gstr(hashtable[i].name)
!	if hashtable[i].truename then
!		gstr(" (")
!		gstr(hashtable[i].truename)
!		gstr(")")
!	fi
!	gstr(" Dupl:")
!	gstrint(cast(hashtable[i].nextdupl))
!	gline()
!od

end

proc gstr(ichar s)=
	gs_str(pcldest,s)
end

proc gstrln(ichar s)=
	gs_strln(pcldest,s)
end

proc gline=
	gs_line(pcldest)
end

proc gstrint(int a)=
	gs_strint(pcldest,a)
end

proc glabeldef(word64 lab)=
	while lab do
		gstr("L")
		gstrint(lab iand 0xFFFF)
		gstr(": ")
		lab>>:=16
	od
	gline()
end

global proc printglobalsymbols(filehandle f=nil)=
	println @f,"PROC Global Symbol Table"
	println @f

	printst(f,stprogram)

!	println @f,"Global Proc Table",nglobalprocs
!	for i to nglobalprocs do
!		println @f,i,,":",globalproctable[i].name,namenames[globalproctable[i].nameid]
!	od

end

global proc printst(filehandle f,symbol p,int level=0)=
var ref strec q

printstrec(f,p,level)

q:=p^.deflist

while q<>nil do
	printst(f,q,level+1)
	q:=q^.nextdef
od
end

global proc printglobalsymbols_full(filehandle f=nil)=
	println @f,"\nPROC Global All Symbols in Full"
	println @f

	printstfull(f,stprogram)
end

global proc printstfull(filehandle f,symbol p,int level=0)=
var ref strec q

printstrecfull(f,p)

q:=p^.deflist

while q<>nil do
	printstfull(f,q,level+1)
	q:=q^.nextdef
od
end

proc printstrec(filehandle f,symbol p,int level)=
var strec dd
var ref byte q
var strbuffer v
var ref strbuffer d:=&v
var int col,offset,n
const tabstr="    "
var [256]char str
!ref paramrec pm

!CPL "PRINTSTREC",P^.NAME,NAMENAMES[P.NAMEID]

!RETURN
!gs_init(d)

offset:=0
to level do
	print @f,tabstr
	offset+:=4
	col+:=4
od
!print @f,":"
print @f,p,":"

print @f,padstr(p.name,22-offset,"-")
print @f, padstr(namenames[p.nameid],12,".")

col:=40
dd:=p^


!gs_str(d,"[")

if p.isglobal then
	print @f,"Glob "
!	gs_str(d,"Glob ")
fi


!if p^.imported then
!!	gs_str(d,"Imp ")
!	gs_str(d,(p^.imported=2|"Imp/CLIB "|"Imp "))
!else
!	gs_str(d,(p^.isglobal|"Glob "|"Loc "))
!fi
!
!if dd.isstatic then
!	gs_str(d,"Stat")
!fi
!if dd.fflang then
!	gs_strsp(d,fflangnames[dd.fflang])
!fi
!if dd.parammode then
!	gs_str(d,parammodenames[dd.parammode])
!!	gs_str(d," ")
!fi
!if dd.align then
!	gs_str(d,"@@")
!	gs_strint(d,dd.align)
!	gs_str(d," ")
!fi
!if dd.optional then
!	gs_str(d,"Opt ")
!fi
!if dd.varparams then
!	gs_str(d,"Var ")
!fi
if dd.moduleno then
	fprint @f,"Modno:#",dd.moduleno
fi
!if dd.equals then
!	gs_str(d,":= ")
!fi
!
!if dd.used then
!	gs_str(d,"U ")
!fi
!
!gs_str(d,"]")

print @f,"=========="

if p^.owner then
!	sprintf(&.str,"(%s)",p^.owner^.name)
	fprint @str,"(#)",p.owner.name
	print @f, padstr(&.str,18,"-")
else
	print @f, padstr("()",18,"-")
fi


case p.nameid
when fieldid,frameid,paramid,enumid then
	print @f," Ix:",p.index,," "
	if p.nameid=fieldid and p.atfield then
		print @f,"@",p.atfield.name,$
	fi
	print @f," Offset:",p.fieldoffset,," "
when structfieldid then
	print @f," Offset:",p.fieldoffset,," "
when recordid then
	print @f," Nfields:",p.nfields,," "
when procid then
!	print @f," Proc entry:",p.pcentry

!	offset:=p.pcentry-moduletable[p.moduleno].pcstart
!	print @f," Offset:",offset
	print @f," Nparms:",p.nparams

when dllprocid then
	print @f," Nparms:",p.nparams

!when 

esac	

case p.nameid
when frameid, staticid,constid,macroid,paramid,dllparamid then
	if p.code then
!cpl "CALL STREXP"
!STREXPR(P.CODE).STRPTR
		print @f, (p.nameid<>frameid|" ="|" :="), strexpr(p.code).strptr
!		print(p.nameid<>frameid|" ="|" :="), strexpr(p.code).strptr
!		print @f, (p.nameid<>frameid|" ="|" :=")
	fi
esac


!if p.varptr then
!	print @f,"= ..."
!!	obj_print(p.objectptr,nil)
!fi

if p.mode then
!	fprint @f,"Mode:#",ttname[p.mode]
!CPL =P.MODE
	fprint @f,"Mode:#",strmode(p.mode)
!ELSE
!	print @f, "MODENO=",P.MODE
fi

!PRINT@F," DUPL:",P.NEXTDUPL


println @f
ichar tab:="          "

!if p.nameid in [procid, dllprocid] then
!	println @f,tab,"Paramlist"
!	pm:=p.paramlist
!	while pm do
!		if pm.def then
!			if pm.mode then
!				println @f,tab,tab,pm.def.name,ttname[pm.mode],pm.index
!			else
!				println @f,tab,tab,pm.def.name,pm.index,(pm.isvariadic|"..."|"")
!			fi
!		else
!				println @f,tab,tab,"--",ttname[pm.mode],pm.index,(pm.isvariadic|"..."|"")
!		fi
!		pm:=pm.nextparam
!	od
!fi
end

proc printstrecfull(filehandle f,symbol p)=
	symbol q
	ref symbol r
	const tab="\t"
	int n

!CPL "FULL",P.NAME,NAMENAMES[P.NAMEID],P.DEFLIST,=P.OWNER!,P.OWNER.NAME

!	println @f,"Name:",p.name,namenames[p.nameid],=P
!	println @f,"Name:",p.name,namenames[p.nameid]
	fprintln @f,"Name: ""#"" <#>",p.name,namenames[p.nameid]

	if p.owner then
		println @f,tab,=p.owner.name
	fi
	q:=p.deflist
	if q then
		print @f,tab,"P.DEFLIST="
		while q do
			print @f," ",,q.name
			q:=q.nextdef
		od
		println @f
	fi

	println @f,tab,=p.moduleno

	case p.nameid
	when programid then
	when procid then
		println @f,tab,=p.pcaddress
!		println @f,tab,=p.pcentryoffset
!		println @f,tab,=p.paramlist
!		println @f,tab,=p.fnindex
		println @f,tab,=p.nparams
		println @f,tab,=p.nlocals
		println @f,tab,=p.isglobal
		println @f,tab,=p.mvarparams
		println @f,tab,=p.labelno
		println @f,tab,=p.procfixed

	when dllprocid then
		println @f,tab,=p.truename
!		println @f,tab,=p.paramlist
!		println @f,tab,=p.fnaddr
		println @f,tab,=p.index
		println @f,tab,=p.nparams
		println @f,tab,=p.mode,strmode(p.mode)
		println @f,tab,=p.isglobal
		println @f,tab,=p.mvarparams

	when frameid then
		println @f,tab,=p.index
		println @f,tab,=p.mutable

	when paramid, dllparamid then
		println @f,tab,=p.index
		println @f,tab,=p.moptional
		println @f,tab,=p.mbyref
		println @f,tab,=p.mutable
!		println @f,tab,=p.parampcentry
		if p.code then
			println @f,tab,"P.CODE=",STREXPR_S(p.code)
		fi

		if p.nameid=dllparamid then
			println @f,tab,=p.mode,strmode(p.mode)
!			println @f,tab,=p.defparamtype,strmode(p.defparamtype)
		fi

	when fieldid,structfieldid then
		println @f,tab,=p.firstdupl.genfieldindex
		println @f,tab,=p.fieldoffset

	when labelid then
		println @f,tab,=p.labelno

	when aliasid then
		println @f,tab,"Aliases:",p.alias.name

	when typeid then
		n:=ttlength[p.mode]
		r:=p.topfieldlist
		if r then
			for i to n do
				println @f,tab,tab,i,":",R.name
				++r
			od
		fi

	esac

	println @f

end

global proc printtypetables(filehandle f)=
	symbol d

	println @f,"PROC TYPE TABLES"
!	for m:=0 to ntypes do
	for m:=tlast+1 to ntypes do
!		fprint @f, "#: #  (#)",i:"3",ttname[i]:"jl12",ttnamedef[i]
		fprintln @f, "#: # ",m:"3",ttname[m]:"jl12"
		d:=ttnamedef[m]

!		if d then
!			println @f,"	ST=",d
			println @f,"	ST=",d
			println @f,"	Len=",ttlength[m], "Lower",ttlower[m]
			println @f,"	Size=",ttsize[m]
			println @f,"	Basetype=",ttbasetype[m],ttname[ttbasetype[m]]
			println @f,"	Target=",tttarget[m],ttname[tttarget[m]]
!			println @f,"	Ispacked=",ttispacked[m]
			println @f,"	Caligned=",ttcaligned[m]

		d:=ttfields[m]
		if d then
			println @f,"	Fields:"
			while d do
				println @f,"		",d.name, (d.mode|strmode(d.mode)|"")
				d:=d.nextdef
			od
		fi

!		fi

!		println @f

	od

end

global function getpclname(ref void fnaddr)ichar=
	return pclnames[getpclcode(fnaddr)]
end

function getpclcode(ref void fnaddr)int=
	for i to pclnames.len do
		if fnaddr=handlertable[i] then
CPL "FOUND",I,FNADDR, HANDLERTABLE[I]
			return i
		fi
	od
	return 0
end

=== qq_host.m 20/32 ===
import* qq

record dimrec=(mut int lbound, upper, length)

type hostproc0=ref proc
type hostproc1=ref proc(variant a)
type hostproc2=ref proc(variant a,b)
type hostproc3=ref proc(variant a,b,c)
type hostproc4=ref proc(variant a,b,c,d)

type hostfn0=ref proc(variant a)
type hostfn1=ref proc(variant a,b)
type hostfn2=ref proc(variant a,b,c)
type hostfn3=ref proc(variant a,b,c,d)
type hostfn4=ref proc(variant a,b,c,d,e)

record overloadrec=
	int optype, optype2
	ref int pchandler
	ref overloadrec nextrec
end

!global tabledata() [0:]ichar packtypenames, [0:]int packtypewidths, [0:]int packconvtypes =
!	(tp_void=0,		$,	0,		tvoid),
!	(tp_i64,		$,	64,		tint),
!	(tp_u64,		$,	64,		tword),
!	(tp_r64,		$,	64,		treal),
!
!	(tp_pvoid,		$,	64,		trefpacked),	!here to trp64, must be same order as tvoid..tr64
!	(tp_pi8,		$,	64,		trefpacked), 
!	(tp_pi16,		$,	64,		trefpacked),
!	(tp_pi32,		$,	64,		trefpacked),
!	(tp_pi64,		$,	64,		trefpacked),
!	(tp_pi128,		$,	64,		trefpacked),
!	(tp_pu8,		$,	64,		trefpacked),
!	(tp_pu16,		$,	64,		trefpacked),
!	(tp_pu32,		$,	64,		trefpacked),
!	(tp_pu64,		$,	64,		trefpacked),
!	(tp_pu128,		$,	64,		trefpacked),
!	(tp_pr32,		$,	64,		trefpacked),
!	(tp_pr64,		$,	64,		trefpacked),
!
!	(tp_pstruct,	$,	64,		trefpacked),	
!	(tp_stringz,	$,	64,		tstringz),
!	(tp_variant,	$,	64,		tvariant),
!end

ref overloadrec tostr_list			!list of user overloads for tostr
ref overloadrec convert_list

const noparamtag=tvoid
const nodefault=-999999

global [0..hostfnnames.upb]ref proc hosttable

global proc callhostfunction(int hostfn) =
ref proc fnaddr
int nparams,isfn
object p

fnaddr:=hosttable[hostfn]
nparams:=hostnparams[hostfn]
isfn:=hostisfn[hostfn]

if fnaddr=nil then
	pcerror_s("Hostfn not implemented:",hostfnnames[hostfn])
fi
!CPL "CALLHOST",=hostfn,hostfnnames[hostfn],=nparams,=isfn

if isfn then		!functions

	switch nparams
	when 0 then
		hostfn0(fnaddr)^(sptr)
	when 1 then
		hostfn1(fnaddr)^(sptr,sptr+1)
	when 2 then
		hostfn2(fnaddr)^(sptr,sptr+1,sptr+2)
	when 3 then
		hostfn3(fnaddr)^(sptr,sptr+1,sptr+2,sptr+3)
	when 4 then
		hostfn4(fnaddr)^(sptr,sptr+1,sptr+2,sptr+3,sptr+4)
	else
		pcerror("callhost/fn")
	endswitch
else					!procs

	switch nparams
	when 0 then
		hostproc0(fnaddr)^()
	when 1 then
		hostproc1(fnaddr)^(sptr)
	when 2 then
		hostproc2(fnaddr)^(sptr,sptr+1)
	when 3 then
		hostproc3(fnaddr)^(sptr,sptr+1,sptr+2)
	when 4 then
		hostproc4(fnaddr)^(sptr,sptr+1,sptr+2,sptr+3)
	else
		pcerror("callhost/proc")
	endswitch
fi

to nparams do
	var_unshare(sptr) when sptr.hasref
	++sptr
od
end

!proc pch_strrepl(variant a, b, c, result)=
!	unless a.tag=b.tag=c.tag=tstring then
!		pcerror("not all strings")
!	end
!
!!    t:=""
!!    while n:=old in s do
!!        t +:= s[1..n-1]+newx
!!        s :=  s[n+old.len..$]
!!    od
!
!
!
!
!end
!
global proc inithostlib=

	var ichar name
	var int n:=$get_nprocs()

	for i to n do
		name:=$get_procname(i)
		if eqbytes(name,"pch_",4) then		!(should be OK with v short fn names)
			for k:=0 to hostfnnames.upb do
				if eqstring(name+4,hostfnnames[k]+5) then		!skip "pch_" and "host_"
					hosttable[k]:=$get_procaddr(i)
					exit
				fi
			else
				loaderror_s("Unknown hostfn",name)
			od
		fi
	od
end

proc pch_leftstr(variant a, b, c, result)=
	int n,length,padchar
	ref char s
	object pa

	padchar:=' '
	case c.tag
	when tvoid then
	when tstring then
		if c.objptr.ustr.length=1 then
			padchar:=c.objptr.ustr.strptr^
		else
			pcerror("left/padx")
		fi
	when tint then
		padchar:=c.value
	else
		pcerror("left/pad?")
	esac

	case b.tag
	when tvoid then
		n:=1
	when tint then
		n:=b.value
	else
		pcerror("left:bad n")
	esac
	if a.tag<>tstring then
		pcerror("left:not str")
	fi

	pa:=a.objptr
	length:=pa.ustr.length
	s:=pa.ustr.strptr

	if n=0 then
		var_empty_string(result)
		return
	fi

	result.tagx:=tstring ior hasrefmask
	if n>0 then			!leftmost n chars
		if n<=length then
			leftstring(a,n,result)
		else				!n>length
			padstring_right(a,n,padchar,result)
		fi
	else					!left chars chars excluding rightmost n
		n:=-n
		if n<length then
			leftstring(a,length-n,result)
		else
			var_empty_string(result)
		fi
	fi
end

proc pch_rightstr(variant a, b, c, result)=
	int n,length,padchar
	ref char s
	object pa

	padchar:=' '
	case c.tag
	when tvoid then
	when tstring then
		if c.objptr.ustr.length=1 then
			padchar:=c.objptr.ustr.strptr^
		else
			pcerror("right/padx")
		fi
	when tint then
		padchar:=c.value
	else
		pcerror("right/pad?")
	esac

	case b.tag
	when tvoid then
		n:=1
	when tint then
		n:=b.value
	else
		pcerror("right:bad n")
	esac

	pa:=a.objptr
	if a.tag<>tstring then
		pcerror("right:not str")
	fi

	length:=pa.ustr.length
	s:=pa.ustr.strptr

	result.tagx:=tstring ior hasrefmask

	if n=0 then
		var_empty_string(result)
		return
	fi

	if n>0 then			!rightmost n chars
		if n<=length then
			rightstring(a,n,result)
		else				!n>length
			padstring_left(a,n,padchar,result)
		fi
	else					!right chars chars excluding leftmost n
		n:=-n
		if n<length then
			rightstring(a,length-n,result)
		else
			var_empty_string(result)
		fi
	fi
end

proc pch_convlc(variant a, b, result)=
	checkparam(a,tstring)
	result^:=a^
	++result^.objptr^.refcount
	var_duplu(result)
	var_iconvcase(result,b,0)
end

proc pch_convuc(variant a, b, result)=
	checkparam(a,tstring)
	result^:=a^
	++result.objptr.refcount
	var_dupl(result) when result.hasref
	var_iconvcase(result,b,1)
end

!proc pch_iconvlc(variant a, b)=
!checkparam(a,trefvar)
!pc_iconvcase(a.varptr,b,0)
!end
!
!proc pch_iconvuc(variant a, b)=
!checkparam(a,trefvar)
!pc_iconvcase(a.varptr,b,1)
!end

proc pch_stop=
pcerror("host_stop not impl")
end

proc pch_stopx(variant a)=
pcerror("host_stopx not impl")
end

!proc pch_ismain(variant a, result)=
!int mainmod,ismain
!
!checkparam(a,tstring)
!result.tagx:=tint
!
!if eqstring(strpclversion,"404") then
!	mainmod:=1
!else
!	mainmod:=nmodules
!fi
!
!ismain:=cmpstring_len(a.objptr.ustr.strptr,moduletable[mainmod].name,
!							a.objptr.ustr.length,strlen(moduletable[mainmod].name))=0
!result.value:=ismain
!end

proc pch_waitkey(variant result)=
result.tagx:=tint
result.value:=os_getch()
end

proc pch_execwait(variant a, b, c, result)=
ref char workdir
int flag
object pa

checkparam(a,tstring)
pa:=a.objptr

flag:=checkparam(b,tint,0)

if c.tag=tvoid then
	workdir:=nil
else
	checkparam(c,tstring)
	workdir:=convCstring(c.objptr.ustr.strptr,c.objptr.ustr.length)
fi
result.tagx:=tint
result.value:=os_execwait(convCstring(pa.ustr.strptr,pa.ustr.length),flag,workdir)
end

proc pch_execcmd(variant a, b, c, result)=
ref char workdir
int flag
object pa

checkparam(a,tstring)
pa:=a.objptr

flag:=checkparam(b,tint,0)

if c.tag=tvoid then
	workdir:=nil
else
	checkparam(c,tstring)
	workdir:=convCstring(c.objptr.ustr.strptr,c.objptr.ustr.length)
fi
result.tagx:=tint
result.value:=os_execcmd(convCstring(pa.ustr.strptr,pa.ustr.length),flag)
end

proc pch_makestr(variant a, b, result)=
	int n
!	object s

	switch a.tag
	when trefpack then
	when tint then
	else
		pcerror("makestr")
	endswitch

	n:=var_getintvalue(b)
!	result.tagx:=tstring ior hasrefmask

!	s:=var_makestrslicexobj(cast(a.uref.ptr),n)
	var_make_stringn(cast(a.uref.ptr),n,result,mutable:1)

!	result.objptr:=s
end

!proc pch_makestrslice(variant a, b, result)=
!pcerror("MAKESTRSLICE")
!end
!
proc pch_makeref(variant a,b,result) =
	ref byte ptr

	switch (ttbasetype[a.tag])
	when trefvar,trefpack,tint then
		ptr:=a.uref.ptr
	when tstring,tarray,tlist,tset then
		ptr:=a.objptr.uarray.ptr
	else
		pcerror("makeref")
	endswitch

	result.tagx:=trefpack
	result.uref.ptr:=ptr
	result.uref.elemtag:=var_getintvalue(b)

	case result.uref.elemtag
	when tpu1,tpu2,tpu4 then
		result.tag:=trefbit
		result.uref.bitoffset:=0
		result.uref.bitlength:=0
	esac
end

proc pch_getcmdparam(variant a, result)=
!a=	void:	return number of cmd params following program name
!a= -2:		return name of invoked interpreter
!a= -1:		return name of .q program when run conventionally
!a= 1..N:   return name of n'th cmd param

	int n
	ref char s

	if a.tag=noparamtag then		!return number of cmds
		result.tagx:=tint
		result.value:=ncmdparams-2
		return
	fi

	n:=var_getintvalue(a)

	if n<0 then		!-2,-1, 1,2,3 => 1,2,3,4,5
		n+:=3
	else
		n+:=2
	fi

	var_make_string(cmdparamtable[n],result)
end

!proc pch_setpcerror(variant a)=
!object pa
!checkparam(a,tstring)
!pa:=a.objptr
!
!if pcerror_mess then
!	free(pcerror_mess)
!	pcerror_mess:=nil
!fi
!
!if pa.ustr.length then
!	pcerror_mess:=malloc(pa.ustr.length+1)
!	memcpy(pcerror_mess,pa.ustr.strptr,pa.ustr.length)
!	(pcerror_mess+pa.ustr.length)^:=0
!fi
!end

!proc pch_setdebug(variant a)=
!checkparam(a,tint)
!
!CPL "SETDEBUG................."
!!PCERROR("DEB")
!fdebug:=a.value
!end
!
!proc pch_setfprintf(variant a, b)=
!checkparam(a,trefdllproc)
!checkparam(b,trefdllproc)
!fprintf_ptr:=cast(a.refptr)
!fgets_ptr:=cast(b.refptr)
!end
!
proc pch_ticks(variant result)=
result.tagx:=tint
result.value:=os_clock()
end

proc pch_sleep(variant a)=
checkparam(a,tint)
os_sleep(a.value)
end

proc pch_random(variant a, result)=
! a=0		Result is pure int
! a=1		Result is 0.0 to 0.9999999...
! a=n		Result is 0 to n-1
! a=x..y	Result is x to y inclusive

int n,x

result.tagx:=tint			!assume int result (can be real too)

if a.tag=trange then
	x:=mrandomrange(a.range_lower, a.range_upper)
else
	checkparam(a,tint)
	n:=a.value
	if n>1 then					!0 to n-1
		x:=mrandomint(n)
	elsif n=0 then				!pure rand
		x:=mrandom()
	elsif n=1 then				!0.0 to 0.99999999
		result.tagx:=treal
		result.xvalue:=mrandomreal()
		return
	else
!		mseed(-n)
!		x:=0
	fi
fi
result.value:=x
end

proc pch_loadpcl(variant a, b, result)=
pcerror("host_loadpcl not impl")
end

proc pch_runpcl(variant a, b, result)=
pcerror("host_runpcl not impl")
end

proc pch_runtask(variant a, b, result)=
pcerror("host_runtask not impl")
end

proc pch_callext(variant a, b, c)=
pcerror("host_callext not impl")
end

proc pch_system(variant a,result) =		!PCH_SYSTEM
checkparam(a,tstring)
result.tagx:=tint
result.value:=system(convCstring(a.objptr.ustr.strptr,a.objptr.ustr.length))
!result.value:=os_execwait(convCstring(a.objptr.ustr.strptr,a.objptr.ustr.length))
!ICHAR S:=convCstring(a.objptr.ustr.strptr,a.objptr.ustr.length)
!CPL =S
!CPL =S
!CPL =S
!result.value:=os_execwait(S)
end

!proc pch_shellexec(variant a,b,result) =		!PCH_SHELLEXEC
!object pa,pb
!
!checkparam(a,tstring)
!checkparam(b,tstring)
!pa:=a.objptr
!pb:=b.objptr
!
!result.tagx:=tint
!result.value:=os_shellexec(convCstring(pa.ustr.strptr,pa.ustr.length),
!		convCstring(pb.ustr.strptr,pb.ustr.length))
!end
!
!proc pch_gethash(variant a,result) =		!PCH_GETHASH
!!convert a to hash value
!result.tagx:=tint
!result.value:=gethashvalue(a)
!end

proc pch_getcstring(variant a, result)=
!a is a string
!return an int which is a pointer to a zero-terminated temporary string
pcerror("PCH/GETCSTRING")
end

proc pch_$getparam(variant a, result)=
checkparam(a,tint)

!CPL a.value*varsize

result^:=variant(frameptr+a.value*varsize)^		!param 1/2/3... = offset 16/32/48... (varsize=16)
if result.hasref then
	++result.objptr.refcount
fi
end

proc pch_clearlist(variant a)=
int n
pcerror("PCH CLEARLIST")
end

!proc pch_makelink(variant a, result)=
!
!case ttbasetype[a.tag]
!when trecord then
!	result.tagx:=trecordlink
!	result.uref.elemtag:=a.tag
!	result.objptr:=a.objptr
!
!when tint then				!makelink(0) is allowed, it just returns nil
!	if a.value then
!		pcerror("makelink/int")
!	fi
!	result.tagx:=tint
!	result.value:=0
!when trecordlink then		!already link
!else
!CPL ttname[a.tag]
!CPL ttname[ttbasetype[a.tag]]
!	pcerror("makelink: not record/list")
!esac
!end
!
!proc pch_allparams(variant a,result)=
!!return all parameters as an external slice
!object p
!int nparams,isfn,i
!variant q
!ref int fnptr
!
!checkparam(a,trefproc)
!fnptr:=cast(a.refptr)
!nparams:=(fnptr-1)^
!
!p:=obj_new(tlist)
!p.objtype:=extslice_obj
!p.ulist.length:=nparams
!p.ulist.lower:=1
!p.ulist.vptr:=variant(frameptr)+1
!
!result.tagx:=tlist ior hasrefmask
!result.objptr:=p
!end

!proc pch_stackvars(variant result)=
!pcerror("STACKVARS")
!end

!proc pch_makeempty(variant a,result)=
!object p
!int t
!
!t:=ttbasetype[a.tag]
!if t=ttype then
!	t:=a.value
!fi
!
!case t
!!when tlist then
!!	p:=emptylist
!!	++p.refcount
!when tstring then
!	var_emptystring(result)
!	return
!!when tarray then
!!	p:=array_new(tarray,a.objptr.uarray.elemtag,0,1)
!else
!	pcustypet("makeempty?",t)
!esac
!
!result.tagx:=t ior hasrefmask
!result.objptr:=p
!end

!proc pch_readlines(variant a,result)=
!!a is a string containing a filename
!!read lines from file into a list of strings, and return as result
!!strings don't include the eol characters
!!returns 0 on error 
!ref byte p,q,pstart
!varrec v
!variant r
!object l
!int nlines,n
!
!checkparam(a,tstring)
!if a.objptr.ustr.length=0 then
!error::
!	result.tagx:=tint
!	result.value:=0
!	return
!fi
!
!p:=readfile(a.objptr.ustr.strptr)
!if p=nil then goto error fi
!
!!easiest to do two passes; first to count how many lines there are
!
!q:=p
!nlines:=0
!doswitch q++^
!when 26 then
!	exit
!when 13 then
!	++nlines
!	if q^=10 then
!		++q
!	fi
!
!when 10 then
!	++nlines
!
!end doswitch
!
!!for now, just return number of lines
!v.tagx:=tlist ior hasrefmask
!
!l:=list_new(nlines)
!v.objptr:=l
!r:=v.objptr.ulist.vptr
!
!!populate with strings
!q:=p
!pstart:=q
!doswitch q++^
!when 26 then
!	exit
!when 13 then
!	n:=q-pstart-1
!	if q^=10 then
!		++q
!	fi
!addline::
!	pc_makestring(cast(pstart),n,r)
!	++r
!	pstart:=q
!
!when 10 then
!	n:=q-pstart-1
!	goto addline
!
!end doswitch
!
!result^:=v
!
!free(p)
!end

!proc pch_dictitems(variant a,result)=
!if a.hasref then
!	result.tagx:=tint
!	result.value:=a.objptr.udict.dictitems
!else
!	pcerror(".alloclen/not heap")
!fi
!end

function checkparam(variant p,int tag,defaultx=nodefault)int64=
!check out a host param, usually for ints
!void:	return default value (assuming int needed), unless default=nodefault
!		then it's an error
!=tag:	return value
!other:	error

case p.tag
when tvoid then
	if defaultx=nodefault then
		pcerror("Missing host param")
	fi
!CPL "CHECKPARAM/VOID; return defaultx",defaultx
	return defaultx
when tag then
	return p.value
esac

if tag=tint then
	case p.tag
	when treal then
		return p.xvalue
	esac
fi
cpl ttname[p.tag]
pcerror("Host param wrong type")
return 0
end

proc leftstring(variant a, int n, variant result)=
!a is an existing string on varstack, which could have be cc_copy or cc_owner
!This can be ""
!return slice of left n chars (n<=length, but n is never zero) in result
!When a is a copy, then returns a view into a, otherwise it will create a new
!string
	object p

!NOTE can create slice here

	var_make_stringn(a.objptr.ustr.strptr,n,result,0)
!var_makestringn(ichar s, int length, variant dest, int mutable)=
end

proc rightstring(variant a, int n, variant result)=
!a is an existing string on varstack, which could have be cc_copy or cc_owner
!This can be ""
!return slice of right n chars (n<=length, but n is never zero) in result
!When a is a copy, then returns a view into a, otherwise it will create a new
!string
	object p

!NOTE can create slice here
!pc_makestring(a.objptr.strptr+(a.objptr.length-n),n,result)
	var_make_stringn(a.objptr.ustr.strptr+(a.objptr.ustr.length-n),n,result,0)
end

proc padstring_right(variant a,int n, fillchar, variant result)=
!a is a string (can be "")
!create a new string of n chars of which the first a.length are from a,
!and the rest are filled with <fillchar>
!n>length always
	ref char s
	int length

	length:=a.objptr.ustr.length

	var_new_stringn(n,result)
!global proc var_make_stringn(ichar s, int length, variant dest, int mutable=0)=
	s:=result.objptr.ustr.strptr

	if length then
		memcpy(s,a.objptr.ustr.strptr,length)
		s+:=length
	fi
	to n-length do
		s^:=fillchar
		++s
	od
end

proc padstring_left(variant a,int n, fillchar, variant result)=
!a is a string (can be "")
!create a new string of n chars of which the last a.length are from a,
!and the rest are filled on the left with <fillchar>
!n>length always
	ref char s
	int length,padlen

	length:=a.objptr.ustr.length
	padlen:=n-length

	var_make_stringn(nil,n,result,0)

	s:=result.objptr.ustr.strptr
	s+:=padlen

	if length then
		memcpy(s,a.objptr.ustr.strptr,length)
	fi
	to padlen do
		--s
		s^:=fillchar
	od
end

!proc pch_tostr(variant a, fmt, result)=
!	case a.tag
!	when tint then
!		var_makestring(strint(a.value),result)
!	else
!		pcustype("tostr",a)
!	esac
!end

proc getbounds(variant p,ref dimrec dims,int lower) =
! extract length or bounds from p, and return in dims
! p will be an int, range, or other value coerceable to int
! lower is default lower bound
	int n

	if not p then
		pcerror("New: no bounds")
	fi

	switch p.tag
	when noparamtag then
		dims.lbound:=lower
		dims.upper:=0
		dims.length:=0
	when trange then
		dims.lbound:=p.range_lower
		dims.upper:=p.range_upper
		dims.length:=p.range_upper-p.range_lower+1
		if dims.length<0 then
			dims.length:=0
			dims.upper:=dims.lbound-1
		fi
	else
		n:=var_getintvalue(p)
		dims.lbound:=lower
		dims.upper:=dims.length:=n
	endswitch
end

proc pch_new(variant a, b, c, d, result)=
	varrec v
	int i,t,nbytes,ival,nwords,nbits,offset,elemtype,n, usertag
	dimrec dims
	variant qvar
	ref int64 qint
	ref byte qbyte
	ref byte ptr
	object p

	t:=var_getintvalue(a)

	if t<0 or t>ntypes then
		pcustype_t("New:bad type",t)
	fi
	v.tagx:=t ior hasrefmask
	usertag:=0

	switch ttbasetype[t]
	when tstring then
		var_new_string(b,c, result)
		return

	when tlist then
		getbounds(b,&dims,1)
		p:=obj_newlist(dims.length,dims.lbound,c)

		v.objptr:=p

	when tarray then
		elemtype:=var_getintvalue(b)
		getbounds(c,&dims,1)
		if elemtype>=tpu1 and elemtype<=tpu4 then
			v.tag:=t:=tbits
			goto dobits2
		fi
!
doarray2::
!!PCERROR("DOARRAY2")
		p:=obj_newarray(elemtype, dims.lbound, dims.length)

		v.objptr:=p

		if dims.length then
			if d and d.tag<>tvoid then		!initial value supplied
				qbyte:=p.uarray.ptr
				to dims.length do
					var_storepacked(qbyte,d,elemtype)
					qbyte+:=ttsize[elemtype]
				od
			fi
		fi
!
	when tcarray then
		elemtype:=tttarget[t]
		dims.length:=ttlength[t]
		dims.lbound:=ttlower[t]
		dims.upper:=dims.length+dims.lbound-1

		d:=b					!any init value: move to d
		usertag:=t
		goto doarray2
!
	when tbits then
		elemtype:=var_getintvalue(b)
		if elemtype not in tpu1..tpu4 then
			pcerror("new: bad bits elem")
		fi
		getbounds(c,&dims,1)
dobits2::				!entry point from arrays, when element is bit type

		p:=obj_newbits(elemtype,dims.lbound,dims.length)
		v.objptr:=p

		if dims.length then
			if d and d.tag<>tvoid then		!initial value supplied
				qbyte:=p.ubits.ptr

				offset:=0
				to dims.length do
					var_storebit(qbyte,offset,d,elemtype,0)
					offset+:=ttbitwidth[elemtype]
					if offset>=8 then
						offset:=0
						++qbyte
					fi
				od
			fi
		fi
!
	when tset then
		getbounds(b,&dims,0)

		if dims.lbound<0 then
			pcerror("new:set:lwb")
		fi
		if dims.lbound<>0 then
			dims.lbound:=0
			dims.length:=dims.upper+1
		fi

		p:=obj_newset(dims.length)
		v.objptr:=p
!
	when trecord then
		p:=obj_new_record(t,b)
		var_fromobj(t,p,&v)
!		v.tag:=trecord
		usertag:=t

	when tstruct then

		p:=obj_new_struct(t)

		var_objtovar(t,p,&v)
!		v.tag:=tstruct
		usertag:=t

		if b and b.tag<>tvoid then
			pcerror("New: struct init")
		fi

	when tint,tword,treal,trefvar then
		v.value:=0
		v.hasref:=0
		if b and b.tag<>tvoid then
			pcerror("NEW(int/value)")
		fi


!	when tstring then
!		getbounds(b,&dims,0)
!		var_make_stringn(nil,dims.length,&v)
!
	when tdict then
!		getbounds(b,&dims,1)
!		n:=nextpoweroftwo(dims.length)
!
!		p:=dict_new(n)
!		p.udict.dictitems:=0
!		v.objptr:=p
!
	else
		pcustype_t("new",t)
	endswitch
finish::

	if usertag then
		v.usertag:=usertag
		v.tag:=ttbasetype[usertag]
	fi
	result^:=v

end

global proc pch_gethostname(variant result) =
	static [256]char name

	strcpy(name,os_gethostname())

	var_make_string(name,result)
end

proc pch_$test(variant a,b, result)=
OBJECT P
P:=A.OBJPTR

CPL "$TEST:",=A,=P.refcount-1
!CPL "====EMERGENCY STOP"
!STOP
end

proc pch_testkey(variant result)=
result.tagx:=tint
result.value:=os_kbhit()
end

proc pch_getos(variant result)=
	var_make_string(os_getos(),result)
end


proc pch_$procsymbols(variant result)=
	object p
	variant q
	ref procrec pp

	p:=obj_newlist(nproclist,1)
	q:=p.ulist.varptr
	pp:=proclist

	to nproclist do
		q.tagx:=tsymbol
		q.def:=pp.def
		++q
		pp:=pp.nextproc
	od

	result.tagx:=tlist ior hasrefmask
	result.objptr:=p
end

proc pch_$symbolowner(variant a, result)=
	checkparam(a,tsymbol)

	result.tagx:=tsymbol
	result.def:=a.def.owner
end

proc pch_$symbolname(variant a, result)=
	checkparam(a,tsymbol)

	var_make_string(a.def.name,result)
end

proc pch_$symboldefs(variant a, result)=
	object p
	variant q
	symbol d,e
	int n

	checkparam(a,tsymbol)

	d:=a.def
	e:=d.deflist
	n:=0
	while e do
		++n
		e:=e.nextdef
	od

	p:=obj_newlist(n,1)
	q:=p.ulist.varptr
	e:=d.deflist

	to n do
		q.tagx:=tsymbol
		q.def:=e
		++q
		e:=e.nextdef
	od

	result.tagx:=tlist ior hasrefmask
	result.objptr:=p
end

global proc pch_setmesshandler(variant fn)=
	if fn.tag<>tsymbol or fn.def.nameid<>procid then
		pcerror("Not proc ref")
	fi
	pcl_callbackfn:=cast(fn.def.pcaddress)
	os_setmesshandler(&runproc_m)
end

proc pch_$testcallback=
	varrec v

	v.tagx:=tint
	v.value:=123456
	CPL "TEST CALLBACK"
	OS_TESTCALLBACK(&v)
end

proc pch_$smallmemtotal(variant result)=
	result.tagx:=tint
	result.value:=smallmemtotal/varsize
end

proc pch_$id(variant a, result)=
	result.tagx:=tint
	result.value:=a.value
end
=== qq_vars.m 21/32 ===
!Var-routines are usually called from bytecode handlers, either directly on indirectly

!Rules for dealing with variant params are:

!* Input variants are usually never freed; leave that to the caller

!* A dest variant is assumed to be ready to write into

!* Where the same variant is both input and output, assume it can be overwritten
!  without needing to free prior contents (after extracting any necessary info!)

!* If a variant needs to be stored or copied, it should be explicitly shared

!* Variants in memory (not params, but could be part of a complex data structure
!  headed by a param), need to be unshared before overwriting, or shared if reused

!* For SOME var-functions (eg. the variant param for appending), it is assumed that the
!  function will consume the value, ie. transfer ownership, eg. to append to
!  a list. Where it doesn't (eg. append to a string), then the function should unshare.
!  (The alternate to share anyway, and get the caller to unshare, is little less efficient) 

!* Some Var functions are called recursively from others (eg. var_equal on lists),
!  then it is especially important the function does not share or unshare params
!  unless it knows what it's doing

import* qq

global macro var_share(x) = var_shareu(x) when x.hasref
global macro var_unshare(x) = var_unshareu(x) when x.hasref
global macro var_dupl(x) = var_duplu(x) when x.hasref

objrec zeroobj

global proc var_unshareu(variant p)=
!CPL "UNSHARE"
	if --p^.objptr^.refcount<=0 then
		var_free(p)
	fi
end

global proc var_shareu(variant p)=
	++p^.objptr^.refcount
end

global proc obj_shareu(object p)=
	++p^.refcount
end

global function void_new:variant p =
	p:=pcm_alloc(varrec.bytes)
	p.tagx:=tvoid
	return p
end

global function obj_new:object p=
	p:=pcm_alloc32()
	clear p^
!	p^:=zeroobj
	p.refcount:=1
	return p
end

global function var_getintvalue(variant p)int =
! return int value from variant, which should be a numeric type
	switch p^.tag
	when tint,ttype,tword then
		return p.value
	when treal then
		return p.xvalue
	else
		pcustype("getintvalue",p)
	endswitch
	return 0
end

global proc var_fromobj(int tag,object p, variant dest)=
	dest.tagx:=tag ior hasrefmask
	dest.objptr:=p
end

global proc var_empty(int tag, variant dest, int param=0)=
	switch tag
!	when tstring then
!		var_empty_string(tag, dest, param)
!	when tset then
!		var_empty_set(tag, dest, param)
!	when tlist then
!		var_empty_list(tag, dest, param)
!	when tdict then
!		var_empty_dict(tag, dest, param)
!	when tarray then
!		var_empty_array(tag, dest, param)
!	when tbits then
!		var_empty_bits(tag, dest, param)
!	when trecord then
!		var_empty_record(tag, dest, param)
!	when tstruct then
!		var_empty_struct(tag, dest, param)
!	when tdecimal then
!		var_new_decimal(tag, dest, param)
	else
		pcustype_t("empty", tag)
	endswitch
end

global proc var_new(int tag, variant dims, param1, param2, dest)=
	switch tag
!	when tstring then
!		var_new_string(tag, dims, param1, param2, dest)
!	when tset then
!		var_new_set(tag, dims, param1, param2, dest)
!	when tlist then
!		var_new_list(tag, dims, param1, param2, dest)
!	when tdict then
!		var_new_dict(tag, dims, param1, param2, dest)
!	when tarray then
!		var_new_array(tag, dims, param1, param2, dest)
!	when tbits then
!		var_new_bits(tag, dims, param1, param2, dest)
!	when trecord then
!		var_new_record(tag, dims, param1, param2, dest)
!	when tstruct then
!		var_new_struct(tag, dims, param1, param2, dest)
!	when tdecimal then
!		var_new_decimal(tag, dest, param)
	else
		pcustype_t("new", tag)
	endswitch
end

global proc var_make(int tag, variant data, dest, int n,  param=0)=
	switch tag
!	when tstring then
!		var_make_string(tag, data, dest, n,param)
!	when tset then
!		var_make_set(tag, data, dest, n,param)
!	when tlist then
!		var_make_list(tag, data, dest, n,param)
!	when tdict then
!		var_make_dict(tag, data, dest, n,param)
!	when tarray then
!		var_make_array(tag, data, dest, n,param)
!	when tbits then
!		var_make_bits(tag, data, dest, n,param)
!	when trecord then
!		var_make_record(tag, data, dest, n,param)
!	when tstruct then
!		var_make_struct(tag, data, dest, n,param)
!	when tdecimal then
!		var_make_decimal(data, dest, param)
	else
		pcustype_t("make", tag)
	endswitch
end

global proc var_free(variant a)=
	varrec v
	object q:=a.objptr

!RETURN
	case q.objtype
	when normal_obj then
		switch a.tag
		when tlist then
			obj_free_list(q)
		when trecord then
			obj_free_record(q)
		when tstring then
			obj_free_string(q)
		when tarray then
			obj_free_array(q,a.tag)
		when tcarray then
			obj_free_array(q,a.usertag)
		when tbits then
			obj_free_bits(q,a.tag)
		when tstruct then
			obj_free_struct(q,a.usertag)
		when tdict then
			obj_free_dict(q)
		when tset then
			obj_free_set(q)
!	when tdecimal then
!		obj_free_decimal(q)
!	else
		else
!			pcustype_t("free", a.tag)
			pcustype("free", a)
		endswitch
	when slice_obj then
		v.tagx:=a.tag
		v.objptr:=q.ulist.objptr2
		var_unshareu(&v)
		pcm_free32(q)
	else
		pcm_free32(q)
	esac
end
!	case ttbasetype[p.tag]
!	when tlist then
!		obj_freelist(q)
!	when trecord then
!		obj_freerecord(q)
!	when tstring then
!		obj_freestring(q)
!	when tarray then
!		obj_freearray(q,p.tag)
!	when tstruct then
!		obj_freestruct(q,p.tag)
!	when tdict then
!		obj_freedict(q)
!	when tset then
!		obj_freeset(q)
!	else
!		pcerror_s("Can't free",ttname[p.tag])
!	esac

global proc var_duplu(variant a)=
	switch a.tag
	when tstring then
		var_dupl_string(a)
	when tset then
		var_dupl_set(a)
	when tlist then
		var_dupl_list(a)
	when tdict then
		var_dupl_dict(a)
	when tarray	,tcarray then
		var_dupl_array(a)
	when tbits then
		var_dupl_bits(a)
	when trecord then
		var_dupl_record(a)
	when tstruct then
		var_dupl_struct(a)
!	when tdecimal then
!		var_dupl_decimal(q)
	else
		pcustype_t("dupl", a.tag)
	endswitch

!global proc var_duplu(variant a)=
!	case ttbasetype[a.tag]
!	when tlist then
!		var_dupllist(a)
!	when trecord then
!		var_duplrecord(a)
!	when tstring then
!		var_duplstring(a)
!	when tarray then
!		var_duplarray(a)
!	when tstruct then
!		var_duplstruct(a)
!	when tset then
!		var_duplset(a)
!	else
!		pcustype("Dupl",a)
!	esac
!end



end

global proc var_neg(variant a, dest)=
	switch a.tag
!	when tint, tword then
!		var_neg_int(a,dest)
!	when treal then
!		var_neg_real(a,dest)
!	when tdecimal then
!		var_neg_decimal(a,dest)
!	when tset then
!		var_neg_set(a,dest)
	else
		pcustype_t("neg", a.tag)
	endswitch
end

global proc var_abs(variant a, dest)=
	switch a.tag
!	when tint, tword then
!		var_abs_int(a,dest)
!	when treal then
!		var_abs_real(a,dest)
!	when tdecimal then
!		var_abs_decimal(a,dest)
	else
		pcustype_t("abs", a.tag)
	endswitch
end

global proc var_inot(variant a, dest)=
	switch a.tag
!	when tint, tword then
!		var_inot_int(a,dest)
!	when tset then
!		var_inot_set(a,dest)
	else
		pcustype_t("inot", a.tag)
	endswitch
end

global function var_istruel(variant a)int=
	switch a.tag
	when tint, tword, trefvar, trefpack, trefbit, ttype, tsymbol then
		return istrue a.value
	when treal then
		return (a.xvalue<>0|1|0)
!	when tdecimal then
!		return var_istruel_decimal(a)
	when tstring,tlist,tarray,tbits,tcarray then
		return a.objptr.ulist.length<>0
	when tset then
		return a.objptr.uset.length<>0
!		return var_istruel_string(a)
!	when tlist then
!		return var_istruel_list(a)
!	when tarray then
!		return var_istruel_array(a)
	when trecord, tstruct then
		return 1
!		return var_istruel_record(a)
!	when tstruct then
!		return var_istruel_struct(a)
!	when tbits then
!		return var_istruel_bits(a)
!	when tdict then
!		return var_istruel_dict(a)
	else
		pcustype_t("istruel", a.tag)
	endswitch
	return 0
!		return istrue x.value
!	when treal then
!		return (x.xvalue<>0|1|0)
!	when tlist, tstring then
!		return x.objptr.ulist.length<>0
!	elsecase ttbasetype[x.tag]
!	when trecord,tstruct then
!		return 1
!	else
!		pcustype("var/true",x)
!	esac
end

global proc var_negto(variant a)=
	switch a.tag
!	when tint, tword then
!		var_negto_int(a)
!	when treal then
!		var_negto_real(a)
!	when tdecimal then
!		var_negto_decimal(a)
!	when tset then
!		var_negto_set(a)
	else
		pcustype_t("negto", a.tag)
	endswitch
end

global proc var_absto(variant a)=
	switch a.tag
!	when tint, tword then
!		var_absto_int(a)
!	when treal then
!		var_absto_real(a)
!	when tdecimal then
!		var_absto_decimal(a)
	else
		pcustype_t("absto", a.tag)
	endswitch
end

global proc var_inotto(variant a)=
	switch a.tag
!	when tint, tword then
!		var_inotto_int(a)
!	when tset then
!		var_inotto_set(a)
	else
		pcustype_t("inotto", a.tag)
	endswitch
end

global proc var_add(variant a, b)=
	if a.tag<>b.tag then
		var_addmixed(a,b)
		return
	fi

	switch a.tag
	when tint, tword then
		a.value+:=b.value
	when treal then
		a.xvalue+:=b.xvalue
!	when tdecimal then
!		var_add_decimal(a,b)
	when tstring then
		var_add_string(a,b)
	when tset then
		var_dupl_set(a)
		var_iorto_set(a,b)
	else
		pcustype_t("add", a.tag)
	endswitch
end

global proc var_addmixed(variant a, b)=
	int newtag:=a.tag

	int tt

	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value+b.xvalue
	when pr(treal,		tint)     then
		a.xvalue+:=b.value
	when pr(tint,		tword)    then
		a.value+:=b.value
	when pr(tword,		tint)     then
		newtag:=tint
		a.value+:=b.value
	when pr(tint,		tdecimal) then
		pcerror("int/dec")
	when pr(tdecimal,	tint)     then
		pcerror("dec/int")
	when pr(treal,		tdecimal) then
		pcerror("real/dec")
	when pr(tdecimal,	treal)    then
		pcerror("dec/real")
	when pr(trefpack,	tint) then
		a.uref.ptr+:=ttsize[a.uref.elemtag]*b.value
	else
		pcmxtypes("Addmixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_sub(variant a, b)=
	ref byte p,q
	int elemsize,x

	if a.tag<>b.tag then
		var_submixed(a,b)
		return
	fi
	switch a.tag
	when tint, tword then
		a.value-:=b.value
	when treal then
		a.xvalue-:=b.xvalue
!	when tdecimal then
!		var_sub_decimal(a,b)
	when trefpack then
		p:=a.uref.ptr
		q:=b.uref.ptr
		case elemsize:=ttsize[a.uref.elemtag]
		when 1 then x:=p-q
		when 2 then x:=(p-q)>>1
		when 4 then x:=(p-q)>>2
		else x:=(p-q)/elemsize
		esac
		a.tagx:=tint
		a.value:=x
!		var_sub_refpack(a,b)
!	when tset then
!		var_sub_set(a,b)
	else
		pcustype_t("sub", a.tag)
	endswitch
end

global proc var_submixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value-b.xvalue
		
	when pr(treal,		tint)     then
		a.xvalue-:=b.value
	when pr(tint,		tword)    then
		a.value-:=b.value
	when pr(tword,		tint)     then
		newtag:=tint
		a.value-:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	when pr(trefpack,	tint) then
		a.uref.ptr-:=ttsize[a.uref.elemtag]*b.value
	else
		pcmxtypes("Submixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_mul(variant a, b)=
	if a.tag<>b.tag then
		var_mulmixed(a,b)
		return
	fi
	switch a.tag
	when tint then
		a.value*:=b.value
	when treal then
		a.xvalue*:=b.xvalue
!	when tdecimal then
!		var_mul_decimal(a,b)
	when tset then
		var_dupl_set(a)
		var_iandto_set(a,b)
	else
		pcustype_t("mul", a.tag)
	endswitch
end

global proc var_mulmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value*b.xvalue
		
	when pr(treal,		tint)     then
		a.xvalue*:=b.value
	when pr(tint,		tword)    then
		a.value*:=b.value
	when pr(tword,		tint)     then
		newtag:=tint
		a.value*:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	when pr(tstring, tint) then
		var_mul_string(a,b.value)
	when pr(tlist, tint) then
		var_mul_list(a,b.value)
	else
		pcmxtypes("Mulmixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_div(variant a, b)=
	if a.tag<>b.tag then
		var_divmixed(a,b)
		return
	fi
	switch a.tag
	when tint then
		a.tagx:=treal
		a.xvalue:=real(a.value)/b.value
!		var_div_int(a,b)
	when treal then
		a.xvalue/:=b.xvalue
!	when tdecimal then
!		var_div_decimal(a,b)
	else
		pcustype_t("div", a.tag)
	endswitch
end

global proc var_divmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		newtag:=treal
		a.xvalue:=a.value/b.xvalue
	when pr(treal,		tint)     then
		a.xvalue/:=b.value
	when pr(tint,		tword)    then
		newtag:=treal
		a.xvalue:=real(a.value)/real(b.value)
	when pr(tword,		tint)     then
		newtag:=treal
		a.xvalue:=real(a.value)/real(b.value)
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("Divmixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_idiv(variant a, b)=
	if a.tag<>b.tag then
		var_idivmixed(a,b)
		return
	fi
	switch a.tag
	when tint then
		a.value:=a.value/b.value
!		var_idiv_int(a,b)
!	when tdecimal then
!		var_idiv_decimal(a,b)
	else
		pcustype_t("idiv", a.tag)
	endswitch
end

global proc var_idivmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		tword)    then
		a.value:=a.value%b.uvalue
	when pr(tword,		tint)     then
		newtag:=tint
		a.value:=a.uvalue%b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("Idivmixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_irem(variant a, b)=
	if a.tag<>b.tag then
		var_iremmixed(a,b)
		return
	fi
	switch a.tag
	when tint then
		a.value:=a.value rem b.value

!		var_irem_int(a,b)
!	when tdecimal then
!		var_irem_decimal(a,b)
	else
		pcustype_t("irem", a.tag)
	endswitch
end

global proc var_iremmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		tword)    then
		a.value:=a.value rem b.uvalue
	when pr(tword,		tint)     then
		newtag:=tint
		a.value:=a.uvalue rem b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("Iremmixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_iand(variant a, b)=
	if a.tag<>b.tag then
		var_iandmixed(a,b)
		return
	fi
	switch a.tag
	when tint, tword then
		a.value iand:= b.value
	when tset then
		var_dupl_set(a)
		var_iandto_set(a,b)
	else
		pcustype_t("iand", a.tag)
	endswitch
end

global proc var_iandmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint, tword), pr(tword, tint) then
		newtag:=tint
		a.value iand :=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("Iandmixed",a,b)
	esac

	a.tag:=newtag
end


global proc var_ior(variant a, b)=
	if a.tag<>b.tag then
		var_iormixed(a,b)
		return
	fi
	switch a.tag
	when tint, tword then
		a.value ior:=b.value

	when tset then
		var_dupl_set(a)
		var_iorto_set(a,b)
	else
		pcustype_t("ior", a.tag)
	endswitch
end

global proc var_iormixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint, tword), pr(tword, tint) then
		newtag:=tint
		a.value ior :=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("Iormixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_ixor(variant a, b)=
	if a.tag<>b.tag then
		var_ixormixed(a,b)
		return
	fi
	switch a.tag
	when tint, tword then
		a.value ixor :=b.value
	when tset then
		var_dupl_set(a)
		var_ixorto_set(a,b)
	else
		pcustype_t("ixor", a.tag)
	endswitch
end

global proc var_ixormixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint, tword), pr(tword, tint) then
		newtag:=tint
		a.value iand :=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("Ixormixed",a,b)
	esac

	a.tag:=newtag
end

global proc var_shl(variant a, b)=
	if a.tag<>b.tag then
		var_shlmixed(a,b)
		return
	fi
	switch a.tag
	when tint then
		a.value<<:=b.value

!		var_shl_int(a,b)
!	when tdecimal then
!		var_shl_decimal(a,b)
	else
		pcustype_t("shl", a.tag)
	endswitch
end

global proc var_shlmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
	when pr(tint,		tword)    then
		a.value<<:=b.value
	when pr(tword,		tint)     then
		newtag:=tint
		a.uvalue<<:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("shlmixed",a,b)
	esac

	a.tag:=newtag
end


global proc var_shr(variant a, b)=
	if a.tag<>b.tag then
		var_shrmixed(a,b)
		return
	fi
	switch a.tag
	when tint then
		a.value>>:=b.value
!	when tint, tword then
!		var_shr_int(a,b)
!	when tdecimal then
!		var_shr_decimal(a,b)
	else
		pcustype_t("shr", a.tag)
	endswitch
end

global proc var_shrmixed(variant a, b)=
	int newtag:=a.tag
	case pr(a.tag,		b.tag)
!	when pr(tint,		treal)    then
!		newtag:=treal
!		a.xvalue:=a.value+b.xvalue
!		
!	when pr(treal,		tint)     then
!		a.xvalue+:=b.value
!	when pr(tint,		tword)    then
!		a.value+:=b.value
!	when pr(tword,		tint)     then
!		a.value+:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("shrmixed",a,b)
	esac

	a.tag:=newtag
end

global function var_in(variant a, b)int=
	case pr(a.tag,b.tag)
	when pr(tint, tset) then
		return var_in_set(a,b)
	when pr(tstring,tstring) then
		return var_in_string(a,b)
	elsecase b.tag
	when tlist then
		return var_in_list(a,b)
	else
		pcmxtypes("in", a,b)
	esac
	return 0

!	int i,xt,yt,n,a
!	int64 nn,aa
!	variant p,q
!	object px,py
!
!	xt:=x.tag
!	yt:=ttbasetype[y.tag]
!
!	px:=x.objptr
!	py:=y.objptr
!
!	switch xt
!	when tint then
!!doi64invar::
!		switch yt
!		when tset then
!			n:=x.value
!doi64inset::
!			if n>=py.uset.length then	!out of bounds so not in set
!				return 0
!			fi
!			n:=testelem(cast(py.uset.ptr),n)
!			return n
!!		when tlist then
!!			a:=x.value
!!			n:=py.ulist.length
!!			p:=py.ulist.vptr
!!			for i to n do
!!				if p.tag=tint and p.value=a then
!!					return i
!!				fi
!!				++p
!!			od
!!			return 0
!!		when tarray then
!!			case py.uarray.elemtag
!!			when ti8,tu8 then
!!				n:=u8inarray(x.value,py)
!!			when ti16,tu16 then
!!				n:=u16inarray(x.value,py)
!!			when ti32,tu32 then
!!				n:=u32inarray(x.value,py)
!!			when ti64,tu64 then
!!				n:=u64inarray(x.value,py)
!!			else
!!				pcustypet("x in array",py.uarray.elemtag)
!!			esac
!!			return n
!!		when tbits then
!!			case py.ubits.elemtag
!!			when tu1 then
!!				n:=bitinbits(x.value,py)
!!			else
!!				pcustypet("x in bits",py.ubits.elemtag)
!!			esac
!!			return n
!		when trange then
!			return if x.value in y.range_lower..y.range_upper then 1 else 0 fi
!		else
!			goto doinany
!		endswitch
!
!	when tstring then
!		switch yt
!		when tstring then
!			n:=var_strinstr(x,y)
!			return n
!		when tlist then
!			n:=py.ulist.length
!			p:=py.ulist.varptr
!			i:=py.ulist.lower
!			to n do
!				if p.tag=tstring then
!					if var_equal_string(x,p) then
!						return i!-py^.lower
!					fi 
!				fi
!				++p
!				++i
!			od
!			return 0
!		endswitch
!
!	else
!doinany::
!		switch yt
!		when tlist then		!x can be anything
!			n:=py.ulist.length
!			p:=py.ulist.varptr
!			for i to n do
!				if var_equal(x,p)=1 then
!					return i
!				fi
!				++p
!			od
!			return 0
!		endswitch
!	endswitch
!	pcmxtypes("varinvar:",x,y)
!	return 0
end

global function var_equal(variant a,b)int=
!can be called when a/b have same tags, or were mixed and have been
!converted, but also they haven't been checked.
	if a.tag<>b.tag then
		return var_equalmixed(a,b)
	fi

	switch a.tag
	when tint, tword, trefvar, trefpack, ttype, tsymbol then
		return a.value=b.value
!	when trefpack, trefbit then
!		return var_equal_refpack(a, b)
	when treal then
		return (a.xvalue=b.xvalue|1|0)
!	when tdecimal then
!		return var_equal_decimal(a, b)
	when tstring then
		return var_equal_string(a, b)
	when tset then
		return var_equal_set(a, b)
	when tlist then
		return var_equal_list(a, b)
	when tdict then
		return var_equal_dict(a, b)
	when tarray, tcarray then
		return var_equal_array(a, b)
	when tbits then
		return var_equal_bits(a, b)
	when trecord then
		return var_equal_record(a, b)
	when tstruct then
		return var_equal_struct(a, b)
	else
		pcustype_t("equal", a.tag)
	endswitch
	return 0

!		case ttbasetype[x.tag]
!		when tint,tword,trefpack,trefvar,ttype then
!			return x.value=y.value
!		when treal then
!			return (x.xvalue=y.xvalue|1|0)
!		when tstring then
!			return var_equalstring(x,y)
!		when tlist then
!			return var_equallist(x,y)
!		when tstruct then
!			return var_equalstruct(x,y)
!		when tset then
!			return var_equal_set(x,y)
!		else
!			pcustype("var/equal",x)
!		esac
!	fi
!	
end

global function var_equalmixed(variant a, b)int=
	case pr(a.tag,		b.tag)
	when pr(tint,		treal)    then
		return (a.value=b.xvalue|1|0)
		
	when pr(treal,		tint)     then
		return (a.xvalue=b.value|1|0)
!	when pr(tint,		tword)    then
!		a.value+:=b.value
!	when pr(tword,		tint)     then
!		a.value+:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		return 0
!		pcmxtypes("equalmixed",a,b)
	esac

	return 0
end

global function var_compare(variant a,b)int=
	if a.tag<>b.tag then
		return var_comparemixed(a,b)
	fi

	switch a.tag
	when tint, tword ,trefpack then
		return (a.value<b.value|-1|(a.value>b.value|1|0))
	when treal then
		return (a.xvalue<b.xvalue|-1|(a.xvalue>b.xvalue|1|0))
!	when tdecimal then
!		return var_compare_decimal(a,b)
	when tstring then
		return var_compare_string(a,b)
	else
		pcustype_t("compare", a.tag)
	endswitch
	return 0
!		case x.tag
!		when tint then
!			return (x.value<y.value|-1|(x.value>y.value|1|0))
!!			return sign(x.value-y.value)
!		when treal then
!			return (x.xvalue<y.xvalue|-1|(x.xvalue>y.xvalue|1|0))
!		when tstring then
!			return var_comparestring(x,y)
!		elsecase ttbasetype[x.tag]
!		when trefpack then
!			return (x.uref.ptr<y.uref.ptr|-1|(x.uref.ptr>y.uref.ptr|1|0))
!
!		else
end

global function var_comparemixed(variant a, b)int=
	case pr(a.tag,		b.tag)
!	when pr(tint,		treal)    then
!		newtag:=treal
!		a.xvalue:=a.value+b.xvalue
!		
!	when pr(treal,		tint)     then
!		a.xvalue+:=b.value
!	when pr(tint,		tword)    then
!		a.value+:=b.value
!	when pr(tword,		tint)     then
!		a.value+:=b.value
!	when pr(tint,		tdecimal) then
!		pcerror("int/dec")
!	when pr(tdecimal,	tint)     then
!		pcerror("dec/int")
!	when pr(treal,		tdecimal) then
!		pcerror("real/dec")
!	when pr(tdecimal,	treal)    then
!		pcerror("dec/real")
	else
		pcmxtypes("comparemixed",a,b)
	esac

	return 0
end


global proc var_concat(variant a,b)=
	if a.tag<>b.tag then
		pcmxtypes("Concat",a,b)
	fi

	switch a.tag
	when tstring then
		var_add_string(a,b)
	when tlist then
		var_dupl_list(a)
		var_concatto_list(a,b)
	when tarray then
		var_dupl_array(a)
		var_concatto_array(a,b)
!	when tarray then
!		var_concatto_array(a,b)
	else
		pcustype_t("concat", a.tag)
	endswitch
end

global proc var_append(variant a,b)=
!be is expected to be consumed here

	if a.tag<>b.tag then
		case a.tag
		when tlist then dolist
		when tarray then doarray
		when tbits then dobits
		esac
		error
	elseswitch a.tag
	when tstring then
		var_add_string(a,b)
!VAR_UNSHARE(A)
		var_unshareu(b)		!caller expects ownership to be xfered
	when tlist then
dolist::
		var_dupl_list(a)
		var_appendto_list(a,b)
	when tarray then
doarray::
		var_dupl_array(a)
		var_appendto_array(a,b)
	when tbits then
dobits::
		var_dupl_bits(a)
		var_appendto_bits(a,b)
	else
error::
		pcustype_t("append", a.tag)
	end
end

global proc var_min(variant a,b)=
	if a.tag<>b.tag then
		var_minmixed(a,b)
		return
	fi
	pcerror("VARMIN")
	

end

global proc var_minmixed(variant a,b)=
	pcerror("VARMINMIX")
end

global proc var_max(variant a,b)=
	if a.tag<>b.tag then
		var_maxmixed(a,b)
		return
	fi
	pcerror("VARMAX")
end

global proc var_maxmixed(variant a,b)=
	pcerror("VARMAXMIX")
end

global function var_addto(variant p,b)int=
!p^+:=b
!handles case where p is a refvar and types are mostly matched
	variant a:=p.varptr
	int newtag

	if p.tag<>trefvar then
		return 0
	fi
	newtag:=a.tag

	if a.tag<>b.tag then
		if newtag=tstring and b.tag=tint then
			var_addto_string_ch(a,b.value)
			return 1
		fi
		return 0

	fi

	switch a.tag
	when tint, tword then
		a.value+:=b.value
	when treal then
		a.xvalue+:=b.xvalue
!	when tdecimal then
!		var_addto_decimal(a,b)
	when tstring then
		var_addto_string(a,b)
	when tset then
		var_iorto_set(a,b)
	else
		return 0
	endswitch

	a.tag:=newtag

	return 1
end

global function var_subto(variant p,b)int=
	return 0
!	switch a.tag
!!	when tint, tword then
!!		var_subto_int(a,b)
!!	when treal then
!!		var_subto_real(a,b)
!!	when tdecimal then
!!		var_subto_decimal(a,b)
!!	when tset then
!!		var_subto_set(a,b)
!	else
!		pcustype_t("subto", a.tag)
!	endswitch
end

global function var_multo(variant p,b)int=
	variant a:=p.varptr
	int newtag

	if p.tag<>trefvar then
		return 0
	fi
	newtag:=a.tag

	if a.tag<>b.tag then
		return 0
	fi

	switch a.tag
	when tset then
		var_iandto_set(a,b)
	else
		return 0
	endswitch

	a.tag:=newtag

	return 1
end

global function var_divto(variant p,b)int=
	return 0
!	if a.tag<>b.tag then pcerror("divto/mixed") fi
!	switch a.tag
!!	when tint, tword then
!!		var_divto_int(a,b)
!!	when treal then
!!		var_divto_real(a,b)
!!	when tdecimal then
!!		var_divto_decimal(a,b)
!	else
!		pcustype_t("divto", a.tag)
!	endswitch
end

global function var_iandto(variant p,b)int=
	variant a:=p.varptr
	int newtag

	if p.tag<>trefvar then
		return 0
	fi
	newtag:=a.tag

	if a.tag<>b.tag then
		return 0
	fi

	switch a.tag
	when tset then
		var_iandto_set(a,b)
	else
		return 0
	endswitch

	a.tag:=newtag

	return 1
end

global function var_iorto(variant p,b)int=
	variant a:=p.varptr
	int newtag

	if p.tag<>trefvar then
		return 0
	fi
	newtag:=a.tag

	if a.tag<>b.tag then
		return 0
	fi

	switch a.tag
	when tset then
		var_iorto_set(a,b)
	else
		return 0
	endswitch

	a.tag:=newtag

	return 1
end

global function var_ixorto(variant p,b)int=
	variant a:=p.varptr
	int newtag

	if p.tag<>trefvar then
		return 0
	fi
	newtag:=a.tag

	if a.tag<>b.tag then
		return 0
	fi

	switch a.tag
	when tset then
		var_ixorto_set(a,b)
	else
		return 0
	endswitch

	a.tag:=newtag

	return 1
end

global function var_shlto(variant p,b)int=
	return 0
end

global function var_shrto(variant p,b)int=
	return 0
end

global function var_concatto(variant a,b)int=
!	return 0
	if a.tag<>b.tag then pcerror("concatto/mixed") fi
	switch a.tag
	when tstring then
		var_addto_string(a,b)
	when tlist then
		var_concatto_list(a,b)
	when tarray then
		var_concatto_array(a,b)
	else
		pcustype("concat",a)
	endswitch
	return 1
end

global function var_appendto(variant a,b)int=
!return 0
	if a.tag<>b.tag then
		case a.tag
		when tlist then dolist
		when tarray then doarray
		when tbits then dobits
		else
			pcerror("appendto/mixed")
		esac
	fi

	switch a.tag
	when tstring then
		var_addto_string(a,b)
		var_unshareu(b)		!caller expects ownership to be xfered
	when tlist then
dolist::
		var_appendto_list(a,b)
	when tarray then
doarray::
		var_appendto_array(a,b)
	when tbits then
dobits::
		var_appendto_bits(a,b)
	else
		pcustype("append",a)
		return 0
	endswitch
	return 1
end

global proc var_getix(variant a, int index)=
	switch a.tag
	when tstring then
		var_getix_string(a,index)
	when tlist then
		var_getix_list(a,index)
	when tarray,tcarray then
		var_getix_array(a,index)
	when tbits then
		var_getix_bits(a,index)
	when tset then
		var_getix_set(a,index)
	when trecord then
		var_getix_record(a,index)
!	when tstruct then
!		var_getix_struct(a,index)
	else
		pcustype_t("getix", a.tag)
	endswitch
end

global proc var_putix(variant a, int index, variant x)=
# comment

	switch a.tag
	when tstring then
		var_putix_string(a,index,x)
		var_unshareu(x)
	when tlist then
		var_putix_list(a,index,x)
	when tarray,tcarray then
		var_putix_array(a,index,x)
	when tbits then
		var_putix_bits(a,index,x)
	when tset then
		var_putix_set(a,index,x)
	when trecord then
		var_putix_record(a,index,x)
!	when tstruct then
!		var_putix_struct(a,index,x)
	else
		pcustype_t("putix", a.tag)
	endswitch
end

global proc var_getixref(variant a, int index)=
	switch a.tag
	when tstring then
		var_getixref_string(a,index)
	when tlist then
		var_getixref_list(a,index)
	when tarray,tcarray then
		var_getixref_array(a,index)
	when tbits then
		var_getixref_bits(a,index)
	when tset then
		var_getixref_set(a,index)
	when trecord then
		var_getixref_record(a,index,a)
!	when tstruct then
!		var_getixref_struct(a,index)
	else
		pcustype_t("getixref", a.tag)
	endswitch
end

global proc var_getslice(variant a, int i,j)=
	switch a.tag
	when tstring then
		var_getslice_string(a,i,j)
	when tlist then
		var_getslice_list(a,i,j)
	when tarray,tcarray then
		var_getslice_array(a,i,j)
	when tbits then
		var_getslice_bits(a,i,j)
	else
		pcustype_t("getslice", a.tag)
	endswitch
end

global proc var_putslice(variant a, int i,j, variant x)=
	if a.tag<>x.tag then
		pcerror("putslice: not compatible")
	fi

	switch a.tag
	when tstring then
		var_putslice_string(a,i,j,x)
	when tlist then
		var_putslice_list(a,i,j,x)
	when tarray,tcarray then
		var_putslice_array(a,i,j,x)
	when tbits then
		var_putslice_bits(a,i,j,x)
	else
		pcustype_t("putslice", a.tag)
	endswitch
end

global proc var_getdotix(variant a, int index)=
	switch a.tag
	when tint, tword then
		if index not in 0..63 then
			pcerror("int.[int] bounds")
		fi
		a.value:=(a.value>>index) iand 1
	when tstring then
		var_getdotix_string(a,index)
	when trecord then
		var_getix_record(a,index)
!	when tstruct then
!		var_getdotix_struct(a,index)
	else
		pcustype_t("getdotix", a.tag)
	endswitch
end

global proc var_putdotix(variant p, int index, variant x)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		switch a.tag
		when tint, tword then
			if index not in 0..63 then
				pcerror("int.[int]:= bounds")
			fi
			var_storebit(cast(&a.value),index,x,tpu1,1)

		when tstring then
			var_putdotix_string(a,index,x)
		when trecord then
			var_putix_record(a,index,x)
!	when tstruct then
!		var_putdotix_struct(a,index,x)
		else
			pcustype("putdotix", a)
		endswitch
	else
		pcustype("putdotix",p)
	fi
end

global proc var_getdotixref(variant p, int index)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		switch a.tag
		when tint, tword then
			if index not in 0..63 then
				pcerror("&int.[int] bounds")
			fi
			p.uref.ptr:=cast(&a.value)
			p.tagx:=trefbit
			p.uref.elemtag:=tpu1
			p.uref.bitoffset:=index
			p.uref.bitlength:=1
		when tstring then
			var_getdotixref_string(a,index,p)
		when trecord then
			var_getixref_record(a,index,p)
!		when tstruct then
!			var_getdotixref_struct(a,index)
		else
			pcustype_t("getdotixref", a.tag)
		endswitch
	else
		pcustype("not refvar",p)
	fi
end

global proc var_getdotslice(variant a, int i,j)=
	switch a.tag
	when tint, tword then

		if i>j then swap(i,j) fi
		if i<0 or j>63 then pcerror("int.[slice] bounds") fi
		a.value:=(a.value>>i)  iand  inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))

!		var_getdotslice_int(a,i,j)
	when tstring then
		var_getslice_string(a,i,j)
	else
		pcustype_t("getdotslice", a.tag)
	endswitch
end

global proc var_putdotslice(variant p, int i,j, variant x)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		switch a.tag
		when tint,tword then
			if i>j then swap(i,j) fi
			if i<0 or j>63 then pcerror("int.[slice]:= bounds") fi
			var_storebit(cast(&a.value), i,x,tpu1,j-i+1)
		when tstring then
			var_putslice_string(a,i,j,x)
		else
			pcustype("putdotslice", a)
		endswitch
	else
		pcustype("not ref",p)
	fi
end

global proc var_getdotsliceref(variant p, int i,j)=
	variant a

	if p.tag=trefvar then
		a:=p.varptr

		switch a.tag
		when tint, tword then
			if i>j then swap(i,j) fi
			if i<0 or j>63 then
				pcerror("&int.[slice] bounds")
			fi
			p.uref.ptr:=cast(&a.value)
			p.tagx:=trefbit
			p.uref.elemtag:=tpu1
			p.uref.bitoffset:=i
			p.uref.bitlength:=j-i+1
!			var_getdotsliceref_int(a,i,j)
		else
			pcustype("getdotsliceref", a)
		endswitch
	else
		pcustype("not ref",p)
	fi
end

global proc var_expand(variant a, dest, int m)=
!expand object at a to maximum m entries, starting at dest
!(dest may overlap a, since usually this is done on the stack)
!arrays, records are expanded the first m entries. If there are 
!fewer, then padded with void entries
!ranges expand to two integers
!minimum m will be 2.
	variant b,c
	object p
	ref char s
	int n

!CPL "EXPAND",=M

	if m<2 then pcerror("Expand: LHS too few") fi

	switch a.tag
	when tlist then
		p:=a.objptr
		b:=dest
		c:=p.ulist.varptr
		n:=1

		to m do
			if n>p.ulist.length then
				dest.tagx:=tvoid
			else
				dest^:=c^
				var_share(dest)
				++c
			fi
			++n
			++dest
		od

	when trange then			!expand to two ints
		dest.tagx:=tint
		dest.value:=a.range_lower
		++dest
		dest.tagx:=tint
		dest.value:=a.range_upper
		to m-2 do
			++dest
			dest.tagx:=tvoid
		od

	when tstring then
		var_expand_string(a, dest, m)
!		p:=a.objptr
!		b:=dest
!		s:=p.ustr.strptr
!		n:=1
!
!		to m do
!			if n>p.ustr.length then
!				var_empty_string(dest)
!			else
!				var_make_stringn(s,1, dest,1)
!				++s
!			fi
!			++n
!			++dest
!		od
!
!	when trecord then
!		qq:=q.objptr
!		nright:=ttlength[q.tag]
!		goto dolist
!
!	when tarray then
!		pcerror("POPPTRLIST ARRAY")

	else
		pcustype("expand",a)
	endswitch
end

global function var_minto(variant p,b)int=
	variant a:=p.varptr
	int newtag

	if p.tag<>trefvar then
		return 0
	fi
	if newtag:=a.tag<>b.tag then
		return 0
	fi

	switch a.tag
	when tint then
		a.value min:=b.value
!	when treal then
!		a.xvalue min:=b.xvalue
!!		var_addto_real(a,b)
!!	when tdecimal then
!!		var_addto_decimal(a,b)
!!	when tstring then
!!		var_addto_string(a,b)
	when tstring then
		if var_compare_string(a,b)>0 then		!b is smaller
			var_shareu(b)
			var_unshareu(a)
			a^:=b^
		fi

!!	when tset then
!!		var_addto_set(a,b)
	else
		return 0
	endswitch
	return 1
end

global function var_maxto(variant p,b)int=
	variant a:=p.varptr
	int newtag

	if p.tag<>trefvar then
		return 0
	fi
	if newtag:=a.tag<>b.tag then
		return 0
	fi

	switch a.tag
	when tint then
		a.value max:=b.value
!	when treal then
!		a.xvalue min:=b.xvalue
!!		var_addto_real(a,b)
!!	when tdecimal then
!!		var_addto_decimal(a,b)
!!	when tstring then
!!		var_addto_string(a,b)
	when tstring then
		if var_compare_string(a,b)<0 then		!b is bigger
			var_shareu(b)
			var_unshareu(a)
			a^:=b^
		fi

!!	when tset then
!!		var_addto_set(a,b)
	else
		return 0
	endswitch
	return 1
end

global proc var_inplace(variant px,y, ref proc(variant,variant) fnadd, fnaddmixed)=
	varrec x

	var_loadptr(px,&x)

CPL "INPLACE"

	if x.tag=y.tag then
		fnadd^(&x,y)
	else
		fnaddmixed^(&x,y)
	fi

	var_storeptr(px,&x)
	var_unshare(y)
end

global proc var_loadptr(variant x,y)=
!y:=x^

	switch x.tag
	when trefvar then
		y^:=(x.varptr)^
		if y.hasref then
			++y.objptr.refcount
		fi

	when trefpack then
		var_loadpacked(x.uref.ptr,x.uref.elemtag,y,nil)

	when trefbit then
		var_loadbit(x.uref.ptr, x.uref.bitoffset, x.uref.elemtag, x.uref.bitlength, y)

	else
		pcustype("var_loadptr",x)
	endswitch
end

global proc var_storeptr(variant p,q)=
!p^:=q
	variant dest
	variant pptr,qptr
	varrec v
	int i,n,etag
	int poffset,qoffset,bitwidthx
	ref byte pp,qq
	int aa,bb

	switch p.tag
	when trefvar then
		dest:=p.varptr
		var_unshare(dest)
		var_share(q)
		dest^:=q^

	when trefpack then
		var_storepacked(ref byte(p.uref.ptr),q,p.uref.elemtag)
!		var_unshare(q)

	when trefbit then
		var_storebit(p.uref.ptr,p.uref.bitoffset,q,p.uref.elemtag,p.uref.bitlength)

	else
		pcustype("var_popptr",p)
	endswitch
end

global proc var_loadbit(ref byte p,int shift,t,bitlength,variant dest) =
!t is tu1/tu2/tu4 load bitfield from p^ at given bit offset, to dest
	ref word pd
	word mask

	dest.tagx:=tint
	switch (t)
	when tpu1 then
		if bitlength=0 then
			dest.value:=not not (p^ iand (1<<shift))
		else
			pd:=cast(p)
			mask:=0xFFFF'FFFF'FFFF'FFFE
			case bitlength
			when 1 then
			when 64 then
				mask:=0
			else
				mask<<:=bitlength-1
			esac
			dest.value:=(pd^>>shift) iand (inot mask)
		fi

	when tpu2 then
		dest.value:=(p^ iand (3<<shift))>>shift
	when tpu4 then
		dest.value:=(p^ iand (15<<shift))>>shift
	else
		pcustype_t("loadbit",t)
	endswitch

end

global proc var_storebit(ref byte p,int shift,variant q,int t,bitlength) =
!t is tpu1/tu2/tu4 store bitfield to p^ at given bit offset, from dest
!shift will be 0,1,2,3,4,5,6,7
	ref word pd
	byte bb
	word mask1,mask2,newvalue

	if q.tag<>tint then
		pcerror("storebit not int")
	fi

	switch (t)
	when tpu1 then
		if bitlength=0 then
			p^:=(p^ iand inot(1<<shift)) ior ((q.value iand 1)<<shift)
		else
			pd:=cast(p)
			mask1:=0xFFFF'FFFF'FFFF'FFFE
			case bitlength
			when 1 then
			when 64 then
				mask1:=0
			else
				mask1<<:=bitlength-1
			esac

			mask1 :=inot mask1


			if shift then
				mask1<<:=shift
			fi

			mask2:=inot mask1
			newvalue:=q.value
			if shift then
				newvalue<<:=shift
			fi
			pd^:=(pd^ iand mask2) ior (newvalue iand mask1)
		fi

	when tpu2 then
		p^:=(p^ iand inot(3<<shift)) ior ((q.value iand 3)<<shift)
	when tpu4 then
		p^:=(p^ iand inot(15<<shift)) ior ((q.value iand 15)<<shift)
	else
		pcustype_t("storebit",t)
	endswitch
end

global proc var_convert(variant x, int t, variant dest)=
!convert x to type t and store new value in dest
!dest will be different location from x
	int s,tbase
	i64 aa
	varrec bn

!CPL "ICONV1",TTNAME[X.TAG],TTNAME[T]

	dest^:=x^

	s:=x.tag
!	if s=t and s<tlist then		!same type
	if s=t then		!same type
		return 							!Note: heap types such as arrays must match on elemtypes too
	fi
	tbase:=t

	dest.tag:=t			!assume works, so pre-set tag

	switch s
	when tint then
		switch tbase
		when tint then			!no changes needed
		when treal then
			dest.xvalue:=x.value
		when tword then
!CPL "INT TO WORD"
!		when tdecimal then
!			bx_makeint(sptr.value,sptr)
		else
			pcustype_t("conv dint=>",t)
		endswitch

	when tword then
		switch tbase
		when tint then
		when tword then
		when treal then
		else
			pcustype_t("conv dint=>",t);
		endswitch

	when treal then
		switch tbase
		when tint then
			dest.value:=x.xvalue
!	when tword then
!		x.uvalue:=x.xvalue
		else
			pcustype_t("conv real=>",t)
		endswitch

	when trefpack,trefvar,trefbit then
		switch tbase
		when tint,tword then
		else
			pcustype_t("conv ptr=>",t)
		endswitch
	when tstring then
		switch tbase
		when tlist then
			var_convert_string_list(x,t,dest)

!		when tdecimal then
!!BUG: NEED TO ZERO-TERMINATE STRING BEFORE CALLING BX_MAKESTR
!			(X.OBJPTR.USTR.STRPTR+X.OBJPTR.USTR.LENGTH)^:=0
!			bx_makestr(x.objptr.ustr.strptr,x.objptr.ustr.length,&bn)
!			x.tagx:=tstring ior hasrefmask		!set it back in order to free
!			pc_unshare(x)
!			x^:=bn
		when tstring then
		else
			pcustype_t("string=>",t)
		endswitch

	when ttype then
		if tbase<>tint then
			pcustype_t("type=>",t)
		fi

!	when tdecimal then
!		switch (tbase)
!		when tint then
!			aa:=bx_int(x)
!			x.tagx:=tdecimal ior hasrefmask		!set it back in order to free
!			pc_unshare(x)
!			x.tagx:=tint
!			x.value:=aa
!			x.tagx:=t
!		else
!			pcustype_t("decimal=>",t)
!		endswitch
!
	else
		pcmxtypestt("Convert s.t",s,t)
	endswitch

end

global function var_gethashvalue(variant p)int=
	int hsum,csum,c,n,i,result
	ref char s,s0

	switch p^.tag
	when tstring then
		n:=p.objptr.ustr.length
		if not n then return 0 fi
		hsum:=0
		s:=p.objptr.ustr.strptr
		to n do
			c:=s++^
			hsum:=(hsum<<4-hsum) +c
		od
		result:=hsum<<5-hsum

		return result iand 0x7FFF'FFFF'FFFF'FFFF		!keep positive

	when tint,tword,treal,trange then
		return p^.value
	else
		pcustype("Can't hash:",p)
	endswitch
	return 0
end

global proc var_objtovar(int tag, object p, variant q)=
	q.tagx:=tag ior hasrefmask
	q.objptr:=p
end

global proc var_putdotix_intint(variant a, int index, variant b)=
!a^.[index]:=b
!a, b are both ints
	word x:=a.value
	word y:=b.value

	if index not in 0..63 then
		pcerror("int.[int]:= bounds")
	fi

	a.value:=x iand inot (1<<index) ior y<<index
end
=== qq_lists.m 22/32 ===
import* qq

global proc var_make_list(variant a, dest, int n, lower) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last/first of the n vars)
	object p
	variant b

	p:=obj_newlist(n, lower)

	b:=p.ulist.varptr

	if n and a then
		to n do
			b^:=a^				!assume list initialised to void
			++a
			++b
		od
	fi							!else leave as voids if not empty

	dest.tagx:=tlist ior hasrefmask
	dest.objptr:=p
end

global function obj_newlist(int n, lower, variant defval=nil)object p=
	variant a

	p:=obj_new()
	p.mutable:=1
	p.ulist.lower:=lower
	p.ulist.length:=n
	p.objtype:=normal_obj

	if n then
		p.ulist.varptr:=a:=pcm_alloc(n*varrec.bytes)
		p.ulist.allocated:=allocbytes/varrec.bytes

		if defval and defval.tag<>tvoid then
			to n do
				var_share(defval)
				a^:=defval^
				++a
			od
		else
			to n do
				a.tagx:=tvoid
				++a
			od
		fi
	fi

	return p
end

global proc obj_free_list(object p)=
	variant q
	varrec v

!CPL "FREELIST",P.ULIST.LENGTH
!RETURN
	q:=p.ulist.varptr
	to p.ulist.length do
		var_unshare(q)
		++q
	od
	if p.ulist.length then
		pcm_free(p.ulist.varptr,p.ulist.allocated*varrec.bytes)
	fi

	pcm_free32(p)
end

global proc var_getix_list(variant a, int index)=
!put result into a (which will be on the stack)
	variant p
	object q
	word offset

	q:=a.objptr

	offset:=index-q.ulist.lower
	if offset>=word(q.ulist.length) then
		pcerror("getlist[int] bounds")
	fi

	a^:=(q.ulist.varptr+offset)^
	var_share(a)
end

global proc var_getslice_list(variant a, int i,j)=
	varrec v,v2
	int alower
	object p,q

	p:=a.objptr

!CPL "LIST SLICE",=I,=J,=P.ULIST.LENGTH,=P.ULIST.VARPTR

	alower:=p.ulist.lower

	if i<alower or j>p.ulist.length+alower-1 or i>j then
		pcerror("list/slice bounds")
	fi

	q:=obj_new()

	v.objptr:=q
	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.ulist.lower:=1
	q.ulist.varptr:=p.ulist.varptr+i-alower

	case p.objtype
	when slice_obj then				!slice of a slice!
!CPL "SLICE SLICE"
		q.ulist.objptr2:=p.ulist.objptr2		!link to original
		obj_shareu(q.ulist.objptr2)

	when extslice_obj then
		q.ulist.objptr2:=nil
		q.objtype:=extslice_obj
	else
		q.ulist.objptr2:=p				!link to original
		++p.refcount
!		var_shareu(a)
	esac

	q.ulist.length:=j-i+1
	a.objptr:=q
!	var_unshare(a)
!CPL "	LIST SLICE'",=Q.ULIST.LENGTH,=a.objptr.ULIST.VARPTR,=q.ulist.varptr,=Q
!CPL "	",=A.OBJPTR, =Q, =V.OBJPTR
end

global proc var_getixref_list(variant a, int index)=
	variant p
	object q
	word offset

	q:=a.objptr

	offset:=index-q.ulist.lower
	if offset>=word(q.ulist.length) then
		pcerror("getlistref[int] bounds")
	fi

	p:=q.ulist.varptr+offset
!	var_unshare(a)			!a should not disappear; rvalues can't have & applied

	a.tagx:=trefvar
	a.varptr:=p
end

global proc var_putix_list(variant a, int index, variant x)=
	variant dest
	object q
	word offset

	q:=a.objptr

	if not q.mutable then pcerror("List not mutable") fi

	offset:=index-q.ulist.lower
	if offset>=word(q.ulist.length) then
		if int(offset)<0 then
			pcerror("putlist[int] lwb")
		elsif offset=q.ulist.length then
			obj_append_list(q,x)
			return
		else
			pcerror("putlist[int] bounds")
		fi
	fi

	dest:=q.ulist.varptr+offset
	var_unshare(dest)
	dest^:=x^				!xfer ownership	
var_share(dest)
end

global proc var_putslice_list(variant a, int i,j, variant x)=
!insert a substring into a
	variant r,s
	object p,q
	int length,sublength

	p:=a.objptr
	if not p.mutable then pcerror("List not mutable") fi
	length:=p.ulist.length

	if i<1 or j>p.ulist.length or i>j then
		pcerror("list/slice bounds")
	fi
	sublength:=j-i+1

!	if x.tag<>tlist then
!		pcerror("a[i..j]:= not list")
!	fi
	q:=x.objptr
	if q.ulist.length<sublength then
		pcerror("substr too short")
	fi

	r:=p.ulist.varptr+i-1
	s:=q.ulist.varptr
!	var_unshare(a)
!CPL "VAR/PUTIX2",X.OBJPTR.REFCOUNT,=X,=DEST
	to sublength do
		r^:=s^
		var_share(r)
		++r
		++s
	od

end

proc obj_append_list(object a, variant x)=
!do in-place append of b to list a
	var int n

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcerror("list/append not mutable")
	fi

	n:=a.ulist.length+1			!new length

	if n>a.ulist.allocated then		!need more space
		obj_resize_list(a,n)
	else
		a.ulist.length:=n
	fi

!	var_share
	(a.ulist.varptr+n-1)^:=x^		!transfers ownership

end

global proc obj_resize_list(object p,int n)=
	var variant q
	var word32 allocated

	if n<=p.ulist.allocated then
		p.ulist.length:=n
	else
		q:=pcm_alloc(n*varrec.bytes)
!		p.ulist.allocated:=allocbytes/varrec.bytes
		allocated:=allocbytes/varrec.bytes
		if p.ulist.length then
			memcpy(q,p.ulist.varptr,p.ulist.length*varsize)
			pcm_free(p.ulist.varptr, p.ulist.allocated*varsize)
		fi
		p.ulist.varptr:=q
		p.ulist.length:=n
		p.ulist.allocated:=allocated
	fi
end

global proc var_appendto_list(variant a, x)=
!a is a list (was a pointer)
	obj_append_list(a.objptr,x)
end

!global proc var_append_list(variant x, y)=
!!a is a list (was a pointer)
!!	varrec z
!!	z:=x^
!	var_dupl_list(x)
!	var_appendto_list(x,y)
!!	var_unshare(&z)
!end
!
global proc var_dupl_list(variant a)=
	object p,q
	variant plist, qlist

	p:=a.objptr
	q:=obj_new()
	q^:=p^
	q.refcount:=1
	q.mutable:=1
	q.objtype:=normal_obj

	a.objptr:=q

	if q.ulist.length=0 then return fi

	qlist:=q.ulist.varptr:=pcm_alloc(p.ulist.length*varrec.bytes)
	q.ulist.allocated:=allocbytes/varrec.bytes	!probably same as p.allocated	
	plist:=p.ulist.varptr

	to q.ulist.length do
		qlist^:=plist^
		if qlist.tag=trecord then
			var_share(qlist)
		else
			var_dupl(qlist)
		fi
		++qlist
		++plist
	od
end

global proc var_mul_list(variant p, int m)=
	int oldlength, newlength, n
	object q:=p.objptr, r
	variant a,b

	oldlength:=q.ulist.length
	newlength:=oldlength*m

	if oldlength=0 then return fi

	if newlength<0 then
		pcerror("list*int <0")
	elsif newlength=0 then
		p.objptr:=obj_newlist(0,q.ulist.lower)
		return
	fi

	r:=obj_newlist(newlength, q.ulist.lower)
	a:=r.ulist.varptr
	b:=q.ulist.varptr
	n:=0

	to newlength do
		a^:=b^
		var_share(a)
		++a
		if oldlength>1 then
			++b
			if ++n=oldlength then
				b:=q.ulist.varptr
				n:=0
			fi
		fi
	od

	p.objptr:=r
end

global function var_equal_list(variant x,y)int =
!return 1 if lists in x,y are equal, otherwise 0
	int xlen,ylen,res
	object px,py
	variant a,b

	px:=x.objptr
	py:=y.objptr

	xlen:=px.ulist.length
	ylen:=py.ulist.length

	if xlen<>ylen then return 0 fi		!unequal lengths

	if xlen=0 then return 1 fi			!both empty

	a:=px.ulist.varptr
	b:=py.ulist.varptr

	to xlen do
		if var_equal(a,b)=0 then return 0 fi	!at least one mismatch
		++a
		++b

	od

	return 1
end

global proc var_concatto_list(variant a,b)=
!do in-place append of b to list a
!both a,b must be lists
!a must own its data
	variant newptr,c,d
	int n,alen,blen,newlen,oldbytes,newbytes
	variant v
	object pa,pb

	pa:=a.objptr

	if not pa.mutable then
		pcerror("concatlist/not mut")
	fi

	pb:=b.objptr

	alen:=pa.ulist.length
	blen:=pb.ulist.length

	if alen=0 then					!concat to empty list
		if blen then				!copy b to a (else leave a as empty)
!global proc obj_resize_list(object p,int n)=
			obj_resize_list(pa,blen)
			d:=pa.ulist.varptr
			memcpy(d,pb.ulist.varptr,blen*varsize)
			to blen do
				var_share(d)
				++d
			od
		fi
	elsif blen then					!neither list is empty (else leave a unchanged)
		newlen:=alen+blen
!		list_resize(pa,newlen)
		obj_resize_list(pa,newlen)
		d:=pa.ulist.varptr+alen
		memcpy(d,pb.ulist.varptr,blen*varsize)
		to blen do
			var_share(d)
			++d
		od
	fi
end

global function var_in_list(variant a,b)int =
	int n:=b.objptr.ulist.length
	variant x:=b.objptr.ulist.varptr

!CPL "HERE"
	for i to n do
!CPL I,"CALL VAREQ"
!		INT RES:=VAR_EQUAL(A,X)
!CPL =RES
!
		if var_equal(a,x)=1 then
			return i
		fi
		++x
	od
	return 0
end
=== qq_dicts.m 23/32 ===
import* qq

global proc var_make_dict(variant a, dest, int n) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last/first of the n vars)
	object p
	variant b
	varrec v

	p:=obj_new_dict(n)

	b:=p.udict.varptr
	v.tagx:=tdict+hasrefmask
	v.objptr:=p

	to n do
		adddictitem(&v,a,a-1)
		a-:=2
	od
	p.udict.dictitems:=n

	dest^:=v
end

global function obj_new_dict(int n)object p=
	int m

	m:=max(16,nextpoweroftwo(n*2))		!list has 2n entries, min 16, rounded up to 2**x

	p:=obj_newlist(m,1,nil)
	p.udict.dictitems:=0

	return p
end

global proc obj_free_dict(object p)=
	variant q
	varrec v

CPL("FREEDICT")
!PCERROR("FREEDICT")

!CPL "FREELIST",P.ULIST.LENGTH
	case p.objtype
	when normal_obj then
		q:=p.ulist.varptr
		to p.ulist.length do
			var_unshare(q)
			++q
		od
		if p.ulist.length then
			pcm_free(p.ulist.varptr,p.ulist.allocated*varrec.bytes)
		fi

	when slice_obj then
		v.tagx:=tlist+hasrefmask
		v.objptr:=p.ulist.objptr2
		var_unshare(&v)
	when extslice_obj then
	esac

	pcm_free32(p)
end

global proc var_dupl_dict(variant a)=
	object p,q
	variant plist, qlist

PCERROR("DUPLDICT")
!	p:=a.objptr
!	q:=obj_new()
!	q^:=p^
!	q.refcount:=1
!	q.ulist.mutable:=1
!
!	a.objptr:=q
!
!	if q.ulist.length=0 then return fi
!
!	qlist:=q.ulist.varptr:=pcm_alloc(p.ulist.length*varrec.bytes)
!	q.ulist.allocated:=allocbytes/varrec.bytes	!probably same as p.allocated	
!	plist:=p.ulist.varptr
!
!	to q.ulist.length do
!		qlist^:=plist^
!		var_dupl(qlist)
!		++qlist
!		++plist
!	od
end

global function var_equal_dict(variant x,y)int =
!return 1 if lists in x,y are equal, otherwise 0
	int xlen,ylen,res
	object px,py
	variant a,b

PCERROR("EQUALDICT")
!	px:=x.objptr
!	py:=y.objptr
!
!	xlen:=px.ulist.length
!	ylen:=py.ulist.length
!
!	if xlen<>ylen then return 0 fi		!unequal lengths
!
!	if xlen=0 then return 1 fi			!both empty
!
!	a:=px.ulist.varptr
!	b:=py.ulist.varptr
!
!	to xlen do
!		if var_equal(a,b)=0 then return 0 fi	!at least one mismatch
!		++a
!		++b
!
!	od
!
	return 1
end

global function var_finddictitem(variant vd, variant p,int doins)variant=
!look for key p in dict d
!when key is found:    will return a pointer to the value
!when key not found::
!   doins=1:     Will insert the key and a void value, and return a pointer to the value
!   doins=0:     Will return nil

	int hash,index,size,keytag,wrapped,limit
	int64 keyvalue
	variant q
	object pa,qa,d

	retry::
	d:=vd.objptr

	size:=d.udict.length/2

	index:=(var_gethashvalue(p) iand (size-1))		!0-based index

	q:=d.udict.varptr+index*2							!point to key of key/value pair
	wrapped:=0
	keytag:=p.tag
	keyvalue:=p.value							!when int
	pa:=p.objptr								!when string

	do
		if q.tag=tvoid then					!unused entry; not found
			exit

		elsif q.tag=keytag then
			case keytag
			when tint,treal,tword,trange then
				if q.value=keyvalue then
					++q
					var_share(q)
					return q
				fi
			when tstring then
				qa:=q.objptr
				if pa.ustr.length=qa.ustr.length then	!match on length at least
					if memcmp(pa.ustr.strptr,qa.ustr.strptr,pa.ustr.length)=0 then
						++q
						var_share(q)
						return q
					fi
				fi
			esac
		fi

!no match
		++index
		q+:=2
		if index>=size then
			if wrapped then					!shouldn't happen if dict was properly expanded
				pcerror("DICT FULL?")
			fi
			wrapped:=1
			index:=0
			q:=d.udict.varptr
		fi
	od

!exit when not found
	if doins then
!	limit:=size*15/16
		limit:=size*3/4
!	limit:=size*7/8
		if d.udict.dictitems>=limit then
			expanddict(vd)
			goto retry
		fi
		q^:=p^
		var_share(q)
		++(d.udict.dictitems)
		return q+1							!point to entry; leave value as void
	else
		return nil
	fi
end

proc expanddict(variant vd)=
!double the size of the dict
	int n,m,i,j,k
	object d,e
	variant p,q,r
	varrec ev

	d:=vd.objptr

	n:=d.udict.allocated			!nos of keys and values (all slots)
	m:=n/2					!number of dict slots

	p:=d.udict.varptr							!old data

	e:=obj_new_dict(m*2)
	var_objtovar(tdict,e,&ev)

	q:=p

	for i:=1 to m do
		if q.tag<>tvoid then
			r:=var_finddictitem(&ev,q,1)
			var_unshare(q)
			++q
			r^:=q++^					!transfer ownership of data
		else
			q+:=2
		fi
	od

	obj_free_dict(d)
	vd.objptr:=e
end

proc adddictitem(variant d, p, q)=
!d is a dict, p:q are akey:value pair to be added to it
	object da
	variant r

	da:=d.objptr

	if da.udict.length=0 then				!cannot be empty
		pcerror("NULL DICT")
	fi

	r:=var_finddictitem(d,p,1)

	var_unshare(r)			!overwrite any existing value
	r^:=q^
	var_share(r)
end

=== qq_sets.m 24/32 ===
import* qq

global proc obj_free_set(object p)=
	if p.uset.length then
		pcm_free(p.uset.ptr, getbitssize(p.uset.allocated64, tpu1))
	fi
	pcm_free32(p)
end

global proc var_dupl_set(variant a)=
	object p:=a.objptr
	object q
	int nbytes, nbits:=p.uset.length

	q:=obj_newset(nbits)	

	if nbits then
		memcpy(q.uset.ptr, p.uset.ptr, getbitssize(nbits, tpu1))
	fi

	a.objptr:=q
end

global function var_equal_set(variant x,y)int=
	int xbytes:=getsetbytes(x)
	int ybytes:=getsetbytes(y)
	if xbytes<>ybytes then return 0 fi

	return eqbytes(x.objptr.ustruct.ptr, y.objptr.ustruct.ptr, xbytes)
end

function getsetbytes(variant x)int=
	int nbits:=x.objptr.uset.length
	if nbits then
		if nbits iand 7 then
			return nbits/8+1
		else
			return nbits/8
		fi
	else
		return 0
	fi
end

global proc var_make_set(variant data, dest, int n) =
! data points to n vars in a block (on the stack, but the caller takes care of that)
! These will be in reverse order, but it doesn't matter for sets.
! dest points to the place to put the resulting set.
! Note: dest will likely correspond to the last data element, so do not override until done.

	variant q
	ref byte p
	int top,a,b,i,j,t,size
	byte alloc
	object s
	static int count=0

	if n=0 then
		var_emptyset(dest)
		return
	fi

!First scan to work out size of set
	top:=0
	q:=data

	to n do
		switch q.tag		!scan items, which should be ranges or integers
		when trange then

			a:=q.range_lower
			b:=q.range_upper

		when tint then
			a:=q.value
			b:=a
		
		else			!assume numeric value of some sort
			b:=a:=var_getintvalue(q)
		endswitch
		if a<0 or b<0 then
			pcerror("Neg set element")
		fi
		if a>top then
			top:=a
		fi
		if b>top then
			top:=b
		fi
		++q
	od

	s:=obj_newset(top+1)

!Second scan to store elements
	q:=data
	to n do
		switch q.tag
		when trange then
			a:=q.range_lower
			b:=q.range_upper
			if a>b then
				swap(a,b)
!				t:=a; a:=b; b:=t
			fi

		when tint then
			b:=a:=q.value

		else
			b:=a:=var_getintvalue(q)
		endswitch

		for j:=a to b do
			setelem(cast(s.uset.ptr),j)
		od
		++q
	od

	var_objtovar(tset,s,dest)
end

global function obj_newset(int length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	ref byte q
	int nbits,nbytes

	p:=obj_new()
	p.mutable:=1
	p.uset.length:=length

	nbytes := ((length-1)/64+1)*8		!bytes required in 64-bit blocks

	if length then
		p.uset.ptr := pcm_alloc(nbytes)              !(turns total allocated in 'allocbytes')
		p.uset.allocated64:=word64(allocbytes)*8
		pcm_clearmem(p.uset.ptr,allocbytes)
	else
		p.uset.ptr:=nil
	fi

	return p
end

global proc var_emptyset(variant dest)=
	var_objtovar(tset,obj_newset(0),dest)
end

global proc var_getix_set(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	object p

	p:=a.objptr

	if u64(index)>=u64(p.uset.length) then
		pcerror("set[int] bounds")
	fi

	a.tagx:=tint
	a.value:=not not ((p.uset.ptr+index>>3)^ iand (1<<(index iand 7)))
end

global proc var_putix_set(variant a, int index, variant x)=
!a[index]:=x
	object p
	ref byte q
	var int newoffset

	p:=a.objptr
	if not p.mutable then pcerror("Not Mutable") fi

	if u64(index)>=u64(p.uset.length) then
		if index<0 then
			pcerror("lwb")
		else
			pcerror("set[i]:=x bounds")
		fi
	fi

	q:=getoffset(p.uset.ptr, index, newoffset)

	var_storebit(q, newoffset,x,tpu1,0)
end

global proc var_getixref_set(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	ref byte q
	int offset, newoffset

	p:=a.objptr
	if not p.mutable then pcerror("Not Mutable") fi

	if u64(index)>=u64(p.uset.length) then
		pcerror("&set[i] bounds")
	fi

	q:=getoffset(p.uset.ptr, index,  newoffset)

	a.tagx:=trefbit
	a.uref.elemtag:=tpu1
	a.uref.ptr:=q
	a.uref.bitoffset:=newoffset
end

function getoffset(ref byte p, int index, &newoffset)ref byte=
!p, with intra-byte offset 'offset', forms a bit pointer to bit-type t
!step it by 'index' elements, and return a new byte-byte, and new offset

	p+:=index>>3				!add number of whole bytes
	newoffset:=index iand 7

	return p
end

global function var_in_set(variant a,b)int =
	int i:=a.value,m
	static [0:]byte masks=(1,2,4,8,16,32,64,128)
	object p

	p:=b.objptr

	if u64(i)>=u64(p.uset.length) then
		return 0
	fi

	if	(p.uset.ptr+i>>3)^ iand masks[i iand 7] then
		return 1
	else
		return 0
	fi
end

global proc iresizeset(variant p,int n)=
!make sure set x has at least n elements, extending as needed
!this is done in-place, so caller must ensure p can be modified
!x should also be a cc_owner type (as it makes use of .alloc)
	object pp

	pp:=p.objptr

	if pp.uset.length>=n then		!already large enough
		return
	fi

	obj_resize_set(pp,n)
end

global proc obj_resize_set(object p,int n)=
	ref byte q
	int newsize,elemtype

	elemtype:=p.ubits.elemtype

	if n<=p.uset.allocated64 then
		p.uset.length:=n
	else
!CPL "RESIZE"
		newsize:=getbitssize(n,tpu1)
		q:=pcm_allocz(newsize)
		if p.uset.length then
			memcpy(q,p.uset.ptr, getbitssize(p.uset.length,tpu1))
			pcm_free(p.uset.ptr, getbitssize(p.uset.allocated64, tpu1))
		fi
		p.uset.ptr:=q
		p.uset.length:=n
		p.uset.allocated64:=allocbytes*8
	fi
end

global proc iorsetbits(ref int p,q,int n)=
	to (n-1)/64+1 do
		p++^ ior:= q++^
	od
end

global proc ixorsetbits(ref int p,q,int n)=
	to (n-1)/64+1 do
		p++^ ixor:= q++^
	od
end

global proc iandsetbits(ref word p,q,int n)=
	to (n-1)/64+1 do
		p++^ iand:= q++^
	od
end

global proc var_iorto_set(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
	int xlen,ylen
	int n,i
	ref int p
	object px,py
	ref byte pp

	px:=x.objptr
	py:=y.objptr

	xlen:=px.uset.length
	ylen:=py.uset.length

	if ylen=0 then			!return x unchanged
	elsif xlen=0 then		!return y
		x^:=y^
		var_dupl_set(x)
	else
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		iorsetbits(cast(px.uset.ptr),cast(py.uset.ptr),ylen)

!		var_unshare(y)
	fi
end

global proc var_iandto_set(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
	int xlen,ylen
	int n,i
	ref int p
	object px,py
	ref byte pp

	px:=x.objptr
	py:=y.objptr

	xlen:=px.uset.length
	ylen:=py.uset.length

	if ylen=0 then				!return empty set
		var_emptyset(x)
	elsif xlen=0 then			!return x unchanged
	else						!x iand:= y
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		iandsetbits(cast(px.uset.ptr),cast(py.uset.ptr),ylen)
	fi
end

global proc var_ixorto_set(variant x,y) =
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
	int xlen,ylen
	int n,i
	ref int p
	object px,py
	ref byte pp

	px:=x.objptr
	py:=y.objptr

	xlen:=px.uset.length
	ylen:=py.uset.length

	if ylen=0 then				!return x unchanged
		var_emptyset(x)
	elsif xlen=0 then			!return y
		x^:=y^
		var_dupl_set(x)
	else						!x iand:= y
		px:=x.objptr

		iresizeset(x,ylen)		!make sure x is at least as big as y

		ixorsetbits(cast(px.uset.ptr),cast(py.uset.ptr),ylen)
	fi
end

=== qq_records.m 25/32 ===
import* qq

global proc var_make_record(variant a, dest, int n, rectype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	object p
	variant b
	int m

	p:=obj_new_record(rectype,nil)

	b:=p.urec.varptr
	a:=a+n-1

	m:=ttlength[rectype]

	if n<m then
		pcerror("Too few elements")
	elsif n>m then
		println =n,=m
		pcerror("Too many elements")
	fi

	to n do
		b^:=a^				!assume list initialised to void
		--a
		++b
	od

	dest.tagx:=trecord ior hasrefmask
	dest.usertag:=rectype
	dest.objptr:=p
end

global function obj_new_record(int m, variant defval)object p=
	variant a
	int n

	p:=obj_new()
	p.mutable:=1
	p.urec.lower:=1
	n:=ttlength[m]
!CPL "NEWREC",TTNAME[M],=TTLENGTH[M],TTNAMEDEF[M].NFIELDS
	p.urec.length:=n
	p.objtype:=normal_obj

	if n then
		p.urec.varptr:=a:=pcm_alloc(n*varrec.bytes)
!		p.urec.allocated:=allocbytes/varrec.bytes

		if defval and defval.tag<>tvoid then
			a:=p.urec.varptr
			to n do
				a^:=defval^
				var_share(a)
				++a
			od
		else
			to n do
				a.tagx:=tint
				a.value:=0
				++a
			od
		fi
	fi

	return p
end

global proc obj_free_record(object p)=
	variant q

!CPL "FREELIST",P.ULIST.LENGTH
	q:=p.urec.varptr
	to p.urec.length do
		var_unshare(q)
		++q
	od
	if p.urec.length then
		pcm_free(p.urec.varptr,p.urec.length*varrec.bytes)
	fi

	pcm_free32(p)
end

global proc var_dupl_record(variant a)=
	object p,q
	variant plist, qlist

	p:=a.objptr
	q:=obj_new()
	q^:=p^
	q.refcount:=1
	q.mutable:=1

	a.objptr:=q

	if q.urec.length=0 then return fi

	qlist:=q.urec.varptr:=pcm_alloc(p.urec.length*varrec.bytes)
!	q.urec.allocated:=allocbytes/varrec.bytes	!probably same as p.allocated	
	plist:=p.urec.varptr

	to q.ulist.length do
		qlist^:=plist^
		if qlist.tag=trecord then
			var_share(qlist)
		else
			var_dupl(qlist)
		fi
		++qlist
		++plist
	od
end

global function var_equal_record(variant x,y)int =
!return 1 if x and y are of same records with identical field values, else 0
	int xlen,ylen,res
	object px,py
	variant a,b

	if x.usertag<>y.usertag then return 0 fi

	px:=x.objptr
	py:=y.objptr

	a:=x.objptr.urec.varptr
	b:=y.objptr.urec.varptr

	to ttlength[x.usertag] do
		if var_equal(a,b)=0 then return 0 fi	!at least one mismatch
		++a
		++b

	od

	return 1
end

global proc var_getix_record(variant a, int index)=
!put result into a (which will be on the stack)
	object q
	word offset

	q:=a.objptr

	offset:=index-1
	if offset>=word(ttlength[a.usertag]) then
		pcerror("record[int] bounds")
	fi

	a^:=(q.urec.varptr+offset)^
	var_share(a)
end

global proc var_putix_record(variant a, int index, variant x)=
	variant dest
	object q
	word offset

	q:=a.objptr

	if not q.mutable then pcerror("Not mutable") fi

	offset:=index-1
	if offset>=word(ttlength[a.usertag]) then
		pcerror("rec[int] bounds")
	fi

	dest:=q.ulist.varptr+offset
	var_unshare(dest)
	dest^:=x^				!xfer ownership	
	var_share(dest)
end

global proc var_getixref_record(variant a, int index, variant dest)=
	variant p
	object q
	word offset

	q:=a.objptr

	offset:=index-1
	if offset>=word(q.ulist.length) then
		pcerror("^rec[int] bounds")
	fi

	p:=q.ulist.varptr+offset

	dest.tagx:=trefvar
	dest.varptr:=p
end

=== qq_arrays.m 26/32 ===
import* qq

global proc obj_free_array(object p, int tag)=
	var ref byte q
	var int elemsize

	q:=p.uarray.ptr
	elemsize:=ttsize[p.uarray.elemtype]

	if p.uarray.length then
		pcm_free(q,p.uarray.allocated*elemsize)
	fi

	pcm_free32(p)
end

global proc var_make_array(variant a, dest, int lower, n, axtype, elemtype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	object p
	ref byte q
	int m

	if n then
		if elemtype=tvoid then			!generic array
			case (a+n-1).tag
			when tint then elemtype:=tpi64
			when treal then elemtype:=tpr64
			when tword then elemtype:=tpu64
			else
				elemtype:=tpi64
			esac
			m:=0
		else
			m:=ttlength[axtype]
			if n<m then
				pcerror("Too few elements")
			elsif n>m then
				println =n,=m
				pcerror("Too many elements")
			fi
		fi
	elsif elemtype=tvoid then
		elemtype:=tpi64
	fi

	p:=obj_newarray(elemtype,lower,n)
	a:=a+n-1
	q:=p.uarray.ptr

	to n do
		var_storepacked(q,a,elemtype)
		q+:=ttsize[elemtype]
		--a
	od

	if axtype=tarray then
		dest.tagx:=tarray ior hasrefmask
	else
		dest.tag:=tcarray ior hasrefmask
		dest.usertag:=axtype
	fi
	dest.objptr:=p
end

global function obj_newarray(int elemtype, lower,length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	var ref byte q
	var int elemsize

	p:=obj_new()
	p.mutable:=1
	p.uarray.lower:=lower
	p.uarray.length:=length
	p.objtype:=normal_obj
	p.uarray.elemtype:=elemtype
	elemsize:=ttsize[elemtype]

	if length then
		p.uarray.ptr:=pcm_allocz(length*elemsize)
		p.uarray.allocated:=allocbytes/elemsize
	fi

	return p
end

global proc var_getix_array(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	varrec v
	object p
	int elemtype

	v:=a^
	p:=a.objptr
	elemtype:=p.uarray.elemtype
	index-:=p.uarray.lower

	if u32(index)>=u32(p.uarray.length) then
		pcerror("ax[int] bounds")
	fi

	if elemtype=tpu8 then
		a.tagx:=tint
		a.value:=(p.uarray.ptr+index)^
	else
		var_loadpacked(p.uarray.ptr+index*ttsize[elemtype],elemtype, a)
	fi

!	var_unshare(&v)
end

global proc var_putix_array(variant a, int index, variant x)=
!a[index]:=x
	varrec v
	object p
	var word offset
	var int elemtype

	v:=a^
	p:=a.objptr
	elemtype:=p.uarray.elemtype

	index-:=p.uarray.lower

	if u32(index)>=u32(p.uarray.length) then
		if index<0 then
			pcerror("lwb")
		elsif index=p.uarray.length then
			if a.tag=tcarray then
				pcerror("Can't append user type")
			fi
			obj_append_array(p,x)
		else
			pcerror("ax[i]:=x bounds")
		fi
	fi

	if elemtype=tpu8 then
		if x.tag<>tint then pcerror("rhs not int") fi
		a.tagx:=tint
		(p.uarray.ptr+index)^:=a.value
	else
		var_storepacked(p.uarray.ptr+index*ttsize[elemtype],x,elemtype)
	fi
end

global proc var_getixref_array(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	var word offset
	var int elemtype

	v:=a^
	p:=a.objptr
	elemtype:=p.uarray.elemtype

	index-:=p.uarray.lower

	if u32(index)>=u32(p.uarray.length) then
		if index<0 then
			pcerror("lwb")
		else
			if u32(index)=u32(p.uarray.length) then
PCERROR("PUTIXREF NEEDS IAPPEND")
!				var_iappendarray(a,nil)
				p:=a.objptr
			else
				pcerror("ax[i]:=x bounds")
			fi
		fi
	fi

	a.tagx:=trefpack
	a.uref.elemtag:=elemtype
	a.uref.ptr:=p.uarray.ptr+index*ttsize[elemtype]
end

proc obj_append_array(object a, variant x)=
!do in-place append of b to list a
	var int n
	ref byte q

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcerror("Not mutable")
	fi

	n:=a.uarray.length+1			!new length

	if n>a.uarray.allocated then		!need more space
		obj_resize_array(a,n)
	else
		a.uarray.length:=n
	fi

	q:=a.uarray.ptr+(n-1)*ttsize[a.uarray.elemtype]

	var_storepacked(cast(q), x, a.uarray.elemtype)
end

global proc var_appendto_array(variant a, x)=
	obj_append_array(a.objptr,x)
end

global proc obj_resize_array(object p,int n)=
	ref byte q
	int elemsize

	elemsize:=ttsize[p.uarray.elemtype]

	if n<=p.uarray.allocated then
		p.uarray.length:=n
	else
		q:=pcm_alloc(n*elemsize)
		if p.uarray.length then
			memcpy(q,p.uarray.ptr,p.uarray.length*elemsize)
			pcm_free(p.uarray.ptr,p.uarray.allocated*elemsize)
		fi
		p.uarray.ptr:=q
		p.uarray.length:=n
		p.uarray.allocated:=allocbytes/elemsize
	fi
end

global proc var_dupl_array(variant a)=
	object p,q
	var int elemsize

	p:=a.objptr
	q:=obj_newarray(p.uarray.elemtype, p.uarray.lower, p.uarray.length)
	a.objptr:=q

	if p.uarray.length then
		memcpy(q.uarray.ptr, p.uarray.ptr,
			p.uarray.length*ttsize[p.uarray.elemtype])
	fi
end

global function var_equal_array(variant a,b)int=
	object p:=a.objptr
	object q:=b.objptr
	int length,elemsize:=p.uarray.elemtype

	if p.uarray.elemtype<>q.uarray.elemtype then
		return 0
	fi
	length:=p.uarray.length

	if length<>q.uarray.length then
		return 0
	fi
	if length=0 then return 1 fi

	return eqbytes(p.uarray.ptr, q.uarray.ptr, ttsize[p.uarray.elemtype]*length)
end

global proc var_concatto_array(variant a,b)=
!do in-place append of b to array a
!both a,b must be arrays
!a must own its data
	ref byte d
	int n,alen,blen,newlen,oldbytes,newbytes,elemsize
	variant v
	object pa,pb

	pa:=a.objptr
	pb:=b.objptr

	if not pa.mutable then
		pcerror("concatarray/not mut")
	fi

	if pa.uarray.elemtype<>pb.uarray.elemtype then
		pcerror("concat/not compat")
	fi
	elemsize:=ttsize[pa.uarray.elemtype]

	alen:=pa.uarray.length
	blen:=pb.uarray.length

	if alen=0 then					!concat to empty array
		if blen then				!copy b to a (else leave a as empty)
			obj_resize_array(pa,blen)
			d:=pa.uarray.ptr
			memcpy(d,pb.uarray.ptr,blen*elemsize)
		fi
	elsif blen then					!neither array is empty (else leave a unchanged)
		newlen:=alen+blen
!		array_resize(pa,newlen)
		obj_resize_array(pa,newlen)
		d:=pa.uarray.ptr+alen*elemsize
		memcpy(d,pb.uarray.ptr,blen*elemsize)
	fi
end

global proc var_getslice_array(variant a, int i,j)=
	int alower,elemsize
	object p,q

	p:=a.objptr

	alower:=p.uarray.lower
	elemsize:=ttsize[p.uarray.elemtype]

	if i<alower or j>p.uarray.length+alower-1 or i>j then
		pcerror("array/slice bounds")
	fi

	q:=obj_new()

	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.uarray.lower:=1
	q.uarray.ptr:=p.uarray.ptr+(i-alower)*elemsize
	q.uarray.elemtype:=p.uarray.elemtype

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.uarray.objptr2:=p.uarray.objptr2		!link to original
		obj_shareu(q.uarray.objptr2)

	when extslice_obj then
		q.uarray.objptr2:=nil
		q.objtype:=extslice_obj
	else
		q.uarray.objptr2:=p				!link to original
		++p.refcount
	esac

	q.uarray.length:=j-i+1
	a.objptr:=q
end

global proc var_putslice_array(variant a, int i,j, variant x)=
!insert a substring into a
	ref byte r,s
	object p,q
	int length,sublength,elemsize

	if a.tag=tcarray then
		pcerror("userax/putslice")
	fi
	p:=a.objptr
	if not p.mutable then pcerror("Not mutable") fi
	length:=p.uarray.length

	if i<1 or j>p.uarray.length or i>j then
		pcerror("array/slice bounds")
	fi

	sublength:=j-i+1

	q:=x.objptr
	if q.uarray.length<sublength then
		pcerror("substr too short")
	fi
	if p.uarray.elemtype<>q.uarray.elemtype then
		pcerror("Not compat")
	fi
	elemsize:=ttsize[p.uarray.elemtype]

	r:=p.uarray.ptr+(i-1)*elemsize
	s:=q.uarray.ptr
	memcpy(r,s,sublength*elemsize)
end

=== qq_packed.m 27/32 ===
import* qq

global proc var_loadpacked(ref void p,int t,variant dest, object ownerobj=nil) =
! p is a direct pointer to a packed type of type t.
! Extract target and store in varrec dest, which should have been freed.
!ownerobj is nil, or points to an array obj of which an element is being accessed
!this is mainly for arrays of structs
	int length
	variant q,r
	ref int pp
	object s
	ref char ss

	switch t
	when tpi8 then
		dest.tagx:=tint
		dest.value:=ref i8(p)^

	when tpi16 then
		dest.tagx:=tint
		dest.value:=ref i16(p)^

	when tpi32 then
		dest.tagx:=tint
		dest.value:=ref int32(p)^

	when tpi64,tint then
		dest.tagx:=tint
		dest.value:=ref i64(p)^

	when tpu8 then
		dest.tagx:=tint
		dest.value:=ref byte(p)^

	when tpu16 then
		dest.tagx:=tint
		dest.value:=ref u16(p)^

	when tpu32 then
		dest.tagx:=tint		!BETTER I64
		dest.value:=ref u32(p)^

	when tpu64 then
		dest.tagx:=tword		!BETTER I64
		dest.value:=ref u32(p)^

	when tpr64 then
		dest.tagx:=treal
		dest.xvalue:=ref r64(p)^

	when tpr32 then
		dest.tagx:=treal
		dest.xvalue:=ref r32(p)^

!	when tpstringz then
!		dest.tagx:=tstring ior hasrefmask
!		length:=ttlength[t]
!		if length>=2 then		!normal fixed string
!			length:=getfslength(p,length)
!		else				!assume string basetype: char target (length can be 0)
!			length:=1
!		fi
!		s:=make_strslicexobj(p,length)
!		dest.objptr:=s
!
!	when tpstringz then		!zero-terminated string
!		dest.tagx:=tstring ior hasrefmask
!		ss:=p
!		to ttlength[t] do
!			exit when ss^=0
!			++ss
!		od
!
!		s:=make_strslicexobj(p,ss-ref char(p))
!		dest.objptr:=s
!
	when trefpack,tpref then
		dest.tagx:=trefpack
		dest.uref.ptr:=cast(ref i64(p)^)
		dest.uref.elemtag:=tttarget[t]

!	when tpstruct then
!		s:=obj_new(t)
!		s.ustruct.mutable:=1
!		s.ustruct.ptr:=p
!	dostruct::
!		dest.objptr:=s
!		dest.tagx:=t ior hasrefmask
!		if ownerobj then
!			s.objtype:=slice_obj
!			s.ustruct.objptr2:=ownerobj
!			++ownerobj.refcount
!		else
!			s.objtype:=extslice_obj
!		fi
!	when tparray then
!		s:=array_new(t,ttelemtype[t],ttlength[t],ttlower[t])
!		s.uarray.mutable:=1
!		s.uarray.lower:=ttlower[t]
!		s.uarray.ptr:=p
!		s.uarray.length:=ttlength[t]
!		s.uarray.elemtag:=ttelemtype[t]
!		goto dostruct
	else
		pcmxtypestt("loadpacked",t,t)
	endswitch
end

global proc var_storepacked(ref byte p,variant q,int t) =
!p points directly to a packed value of type t, which is to receive a value currently
!in variant q

	int plength,qlength
	int s,sbase,tbase
	object qa

	sbase:=s:=q.tag		!storing coercible sbase type to fixed type tbase
	tbase:=t

	switch (sbase)
	when tint,tword, trefpack then
		switch tbase
		when tpi8,tpu8 then
			(ref byte(p)^):=q.value
			return
		when tpi16,tpu16 then
			(ref u16(p)^):=q.value
			return
		when tpi32,tpu32 then
			(ref int32(p)^):=q.value
			return
		when tpi64,tpu64,tint,tword,trefpack,tpref then
			(ref i64(p)^):=q.value
			return
		when tpr32 then
			(ref r32(p)^):=q.value
			return
		when tpr64 then
			(ref r64(p)^):=q.value
			return
		endswitch

	when treal then
		switch tbase
		when tpi32,tpu32 then
			(ref int32(p)^):=q.xvalue
			return
		when tpi64,tpu64 then
			(ref int64(p)^):=q.xvalue
			return
		when tpr32 then
		(ref r32(p)^):=q.xvalue
			return
		when tpr64 then
			(ref r64(p)^):=q.xvalue
			return
		when tpi16,tpu16 then
			(ref int16(p)^):=q.xvalue
			return
		endswitch

!	when tstring then
!		qa:=q.objptr
!		plength:=ttlength[t]
!		qlength:=qa.ustr.length
!		switch tbase
!		when tstring then			!ref string assumed here to mean special 1-char string
!			if t=tbase then			!if basetype, then means special 1-char string
!				if qlength<>1 then
!					pcerror("Str not len 1")
!				fi
!				(ref char(p)^):=ref char(qa.ustr.strptr)^
!				return
!			fi
!			if qlength>plength then		!truncate
!				qlength:=plength
!			fi
!			memcpy(p,qa.ustr.strptr,qlength)		!copy the number of chars provided
!			setfslength(cast(p),plength,qlength)
!			return
!
!		when tstringz then
!			if qlength>=plength then			!truncate as needed; no teminator to allow space for terminator
!				memcpy(p,qa.ustr.strptr,plength)		!copy the number of chars provided
!				(ref byte(p)+plength-1)^:=0			!zero terminator
!
!			else
!				memcpy(p,qa.ustr.strptr,qlength)		!copy the number of chars provided
!				(ref byte(p)+qlength)^:=0			!zero terminator
!			fi
!
!			return
!
!		endswitch

!	when tstruct then
!		if s<>t then
!			pcmxtypestt("spack struct",s,t)
!		fi
!		memcpy(p,q.objptr.ustruct.ptr,ttsize[t])
!		return

!	when tarray then
!		if s<>t then				!not direct match: check whether compatible
!!		if tbase<>tarray or q.elemtype<>ttelemtype[t] or q.length<>ttlength[t] then	!not compatible
!				pcmxtypestt("spack array",s,t)
!!		fi
!		fi
!		memcpy(p,q.objptr.uarray.ptr,ttsize[t])
!		return
!
	endswitch

	pcmxtypestt("storepacked (source->dest)",s,t)
end

proc setfslength(ref char s,int m,n) =		!SETFSLENGTH
!set up lengthcode of fixed string starting at s, of maximum length m, with actual length n
!a,b are the last two chars of the fixed string::
!a b
!0,N	Length is N
!0,0	Length is 0 (special case of 0,N)
!X,0	Length is M-1
!X,Y	Length is M
!NOTE: this only works up for m in 2..256, and the string can't contain zero bytes

	if m=n then		!no length needed (x,y)
	elsif n=m-1 then	!n=m-1, use (x,0)
		(s+m-1)^:=0
	else			!n<=m-2, so encode length at end of string (0,0) or (0,n)
		(s+m-2)^:=0		!
		(s+m-1)^:=n		!store count n (n can be zero)
	fi
end

global function getfslength(ref char s,int m)int =		!GETFSLENGTH
!s points to a packed string encoded with length at it's end. m is the max length (m>=2)
!return the actual encoded length (see setfslength for encoding scheme)
	s+:=m-1			!point to last char

	if (s-1)^=0 then		!(0,n) length is n
		return s^
	elsif s^=0 then		!(x,0) length is m-1
		return m-1
	else				!(x,y) length is m
		return m
	fi
end

global proc var_make_struct(variant a, dest, int n, rectype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	symbol d
	ref symbol r
	object p
	variant b
	int m
	ref byte q

	p:=obj_new_struct(rectype)

	b:=p.urec.varptr
	a:=a+n-1

	m:=ttlength[rectype]
	d:=ttnamedef[rectype]
!CPL "MAKESTRUCT",D.NAME
!CPL "MAKESTRUCT",M,D
	r:=d.topfieldlist

!FOR I TO N DO
!	CPL I,R.NAME,STRMODE(R.MODE)
!	++R
!OD
!PCERROR("MAKESTRUCT")

	if n<m then
		pcerror("Too few elements")
	elsif n>m then
		println =n,=m
		pcerror("Too many elements")
	fi

	q:=p.ustruct.ptr

	to n do
		var_storepacked(q, a, r.mode)
		q+:=ttsize[r.mode]
		++r
		--a
	od

	dest.tagx:=tstruct ior hasrefmask
	dest.usertag:=rectype
	dest.objptr:=p
end

global function obj_new_struct(int m)object p=
	int size

	p:=obj_new()
	p.mutable:=1

	size:=ttsize[m]
	if size then
		p.ustruct.ptr:=pcm_allocz(size)
	fi
	p.ustruct.structdef:=ttnamedef[m]

	return p
end

global proc var_dupl_struct(variant a)=
	object p,q
	var int size

!CPL "DS1"
	p:=a.objptr
!CPL "DS2"
!
	size:=ttsize[a.usertag]
	q:=obj_new_struct(a.usertag)
	a.objptr:=q

	memcpy(q.ustruct.ptr, p.ustruct.ptr, size)
end

global proc obj_free_struct(object p, int tag)=
	pcm_free(p.ustruct.ptr, ttsize[tag])
	pcm_free32(p)
end

global function var_equal_struct(variant x,y)int=
!assume tags match

	return eqbytes(x.objptr.ustruct.ptr, y.objptr.ustruct.ptr, ttsize[x.tag])
end

global proc var_getix_struct(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	symbol d
	ref symbol r
	varrec v
	object p
	int elemtype

	v:=a^
	p:=a.objptr

	if index<1 or index>ttlength[a.tag] then
		pcerror("struct[int] bounds")
	fi

	d:=p.ustruct.structdef
	r:=(d.topfieldlist+index-1)

	var_loadpacked(p.ustruct.ptr+r.fieldoffset, r.mode, a)

!	var_unshare(&v)
end

=== qq_bits.m 28/32 ===
import* qq

global proc obj_free_bits(object p, int tag)=
	if p.ubits.length then
		pcm_free(p.ubits.ptr, getbitssize(p.ubits.allocated, p.ubits.elemtype))
	fi

	pcm_free32(p)
end

global proc var_make_bits(variant a, dest, int lower, n, bxtype, elemtype) =
!create a list of n vars starting from a in reverse order (a is the last)
!put the result in dest (note this will be the last the n vars)
	object p
	ref byte q
	int bitwidthx,offset

	p:=obj_newbits(elemtype,lower,n)
	a:=a+n-1
	q:=p.ubits.ptr

	bitwidthx:=ttbitwidth[elemtype]
	offset:=0

	to n do
		var_storebit(q,offset,a,elemtype,bitwidthx)
		offset+:=bitwidthx
		if offset>=8 then
			++q
			offset:=0
		fi
		--a
	od

	dest.tagx:=bxtype ior hasrefmask
	dest.objptr:=p
end

global function obj_newbits(int elemtype, lower,length)object p=
!create a packed array with element-type t, given length and lower bound.
!it will be initialised to zeros

	var ref byte q
	var int nbits,bitwidthx,nbytes

	p:=obj_new()
	p.mutable:=1
	p.ubits.lower:=lower
	p.ubits.length:=length
	p.objtype:=normal_obj
	p.ubits.elemtype:=elemtype

	if length then
		nbytes:=getbitssize(length, elemtype)
		p.ubits.ptr:=pcm_allocz(nbytes)
		p.ubits.allocated:=allocbytes*(8/ttbitwidth[elemtype])
	fi

	return p
end

global proc var_getix_bits(variant a, int index)=
!a is a list, b is an int; return a[b] into a, which will be on the stack usually
	object p
	ref byte q
	int elemtype,offset,shift

	p:=a.objptr
	elemtype:=p.ubits.elemtype
	index-:=p.ubits.lower

	if u32(index)>=u32(p.ubits.length) then
		pcerror("ax[int] bounds")
	fi
	q:=p.ubits.ptr
	a.tagx:=tint

	index+:=p.ubits.indexoffset

	switch p.ubits.elemtype
	when tpu1 then
		a.value:=not not ((q+index>>3)^ iand (1<<(index iand 7)))
	when tpu2 then
		shift:=(index iand 3)*2
		a.value:=((q+index>>2)^ iand (3<<shift))>>shift
	when tpu4 then
		shift:=(index iand 1)*4
		a.value:=((q+index>>1)^ iand (15<<shift))>>shift
	else
		pcustype_t("bitix",p.ubits.elemtype)
	end
end

global proc var_putix_bits(variant a, int index, variant x)=
!a[index]:=x
	object p
	ref byte q
	var int elemtype, newoffset

	p:=a.objptr
	elemtype:=p.ubits.elemtype

	index-:=p.ubits.lower

	if u32(index)>=u32(p.ubits.length) then
		if index<0 then
			pcerror("lwb")
		elsif index=p.ubits.length then
			obj_append_bits(p,x)
		else
			pcerror("bx[i]:=x bounds")
		fi
	fi

	q:=getindexoffset(p.ubits.ptr, p.ubits.indexoffset, index, elemtype, newoffset)
	var_storebit(q, newoffset*ttbitwidth[elemtype],x,elemtype,0)
end

global proc var_getixref_bits(variant a, int index)=
!a[index]:=x
	varrec v
	object p
	ref byte q
	int offset, newoffset
	var int elemtype

	p:=a.objptr
	elemtype:=p.ubits.elemtype
	index-:=p.ubits.lower

	if u32(index)>=u32(p.ubits.length) then
!
		pcerror("&bx[i] bounds")
	fi

	q:=getindexoffset(p.ubits.ptr, p.ubits.indexoffset, index, elemtype, newoffset)

	a.tagx:=trefbit
	a.uref.elemtag:=elemtype
	a.uref.ptr:=q
	a.uref.bitoffset:=newoffset*ttbitwidth[elemtype]
end

function getindexoffset(ref byte p, int offset, index, t, &newoffset)ref byte=
!p, with intra-byte offset 'offset', forms a bit pointer to bit-type t
!step it by 'index' elements, and return a new byte-byte, and new offset

	index+:=offset

	switch t
	when tpu1 then
		p+:=index>>3				!add number of whole bytes
		newoffset:=index iand 7
	when tpu2 then
		p+:=index>>2
		newoffset:=index iand 3
	when tpu4 then
		index+:=offset>>2
		p+:=index>>1
		newoffset:=index iand 1
	end

	return p
end

proc obj_append_bits(object a, variant x)=
!do in-place append of b to list a
	var int n, newoffset, elemtype
	ref byte q

	if a.objtype<>normal_obj then
		pcerror("Can't extend slice")
	fi

	if not a.mutable then
		pcerror("Not mutable")
	fi

	n:=a.ubits.length+1			!new length
	elemtype:=a.ubits.elemtype

	if n>a.ubits.allocated then		!need more space
		obj_resize_bits(a,n)
	else
		a.ubits.length:=n
	fi

	q:=getindexoffset(a.ubits.ptr, a.ubits.indexoffset, n-a.ubits.lower, elemtype, newoffset)
	var_storebit(q,newoffset*ttbitwidth[elemtype],x,elemtype, 0)
end

global proc var_appendto_bits(variant a, x)=
	obj_append_bits(a.objptr,x)
end

global proc obj_resize_bits(object p,int n)=
	ref byte q
	int newsize,elemtype

	elemtype:=p.ubits.elemtype

	if n<=p.ubits.allocated then
		p.ubits.length:=n
	else
		newsize:=getbitssize(n,elemtype)
		q:=pcm_alloc(newsize)
		if p.ubits.length then
			memcpy(q,p.ubits.ptr, bits_bytesize(p))
			pcm_free(p.ubits.ptr, getbitssize(p.ubits.allocated, elemtype))
		fi
		p.ubits.ptr:=q
		p.ubits.length:=n
		p.ubits.allocated:=allocbytes*(8/ttbitwidth[elemtype])
	fi
end

global proc var_dupl_bits(variant a)=
	object p,q
	var int elemsize

	p:=a.objptr

	q:=obj_newbits(p.ubits.elemtype, p.ubits.lower, p.ubits.length)
	q.ubits.indexoffset:=p.ubits.indexoffset

	a.objptr:=q

	if p.ubits.length then
		memcpy(q.ubits.ptr, p.ubits.ptr, bits_bytesize(p))
	fi
end

global function var_equal_bits(variant a,b)int=
	object p:=a.objptr
	object q:=b.objptr
	int length,elemsize:=p.ubits.elemtype

	if p.ubits.elemtype<>q.ubits.elemtype then
		return 0
	fi
	length:=p.ubits.length

	if length<>q.ubits.length then
		return 0
	fi
	if length=0 then return 1 fi

	return eqbytes(p.ubits.ptr, q.ubits.ptr, bits_bytesize(p))
end

global proc var_concatto_bits(variant a,b)=
!do in-place append of b to array a
!both a,b must be arrays
!a must own its data
	ref byte d
	int n,alen,blen,newlen,oldbytes,newbytes,elemsize
	variant v
	object pa,pb

PCERROR_S("VAR/BITS/NOT READY",$FUNCTION)
	pa:=a.objptr
	pb:=b.objptr

	if not pa.mutable then
		pcerror("concatarray/not mut")
	fi

	if pa.ubits.elemtype<>pb.ubits.elemtype then
		pcerror("concat/not compat")
	fi
	elemsize:=ttsize[pa.ubits.elemtype]

	alen:=pa.ubits.length
	blen:=pb.ubits.length

	if alen=0 then					!concat to empty array
		if blen then				!copy b to a (else leave a as empty)
!global proc obj_resize_bits(object p,int n)=
			obj_resize_bits(pa,blen)
			d:=pa.ubits.ptr
			memcpy(d,pb.ubits.ptr,blen*elemsize)
		fi
	elsif blen then					!neither array is empty (else leave a unchanged)
		newlen:=alen+blen
!		array_resize(pa,newlen)
		obj_resize_bits(pa,newlen)
		d:=pa.ubits.ptr+alen*elemsize
		memcpy(d,pb.ubits.ptr,blen*elemsize)
	fi
end

global proc var_getslice_bits(variant a, int i,j)=
	int alower,elemtype,newoffset
	object p,q

	p:=a.objptr

	alower:=p.ubits.lower
	elemtype:=p.ubits.elemtype

	if i<alower or j>p.ubits.length+alower-1 or i>j then
		pcerror("bits/slice bounds")
	fi

	q:=obj_new()

	q.objtype:=slice_obj
	q.mutable:=p.mutable
	q.ubits.lower:=1
	q.ubits.elemtype:=elemtype

	q.ubits.ptr:=getindexoffset(p.ubits.ptr, p.ubits.indexoffset, i-alower, elemtype, newoffset)
	q.ubits.indexoffset:=newoffset

	case p.objtype
	when slice_obj then				!slice of a slice!
		q.ubits.objptr2:=p.ubits.objptr2		!link to original
		obj_shareu(q.ubits.objptr2)

	when extslice_obj then
		q.ubits.objptr2:=nil
		q.objtype:=extslice_obj
	else
		q.ubits.objptr2:=p				!link to original
		++p.refcount
	esac

	q.ubits.length:=j-i+1
	a.objptr:=q
end

global proc var_putslice_bits(variant a, int i,j, variant x)=
!insert a substring into a
	ref byte pp,qq
	object p,q
	int length,sublength,elemtype,offsetp,offsetq,bitwidthx
	varrec v

	p:=a.objptr
	if not p.mutable then pcerror("Not mutable") fi
	length:=p.ubits.length
	elemtype:=p.ubits.elemtype

	if i<1 or j>p.ubits.length or i>j then
		pcerror("bits/slice bounds")
	fi

	sublength:=j-i+1

	q:=x.objptr
	if q.ubits.length<sublength then
		pcerror("substr too short")
	fi
	if p.ubits.elemtype<>q.ubits.elemtype then
		pcerror("Not compat")
	fi

	bitwidthx:=ttbitwidth[elemtype]

	pp:=getindexoffset(p.ubits.ptr, p.ubits.indexoffset, i-p.ubits.lower, elemtype, offsetp)
	qq:=getindexoffset(q.ubits.ptr, q.ubits.indexoffset, 0, elemtype, offsetq)
	offsetq*:=bitwidthx
	offsetq*:=bitwidthx

	to sublength do
		var_loadbit(qq, offsetq, elemtype,0, &v)
		var_storebit(pp, offsetp, &v, elemtype,0)
		offsetp+:=bitwidthx
		if offsetp>=8 then ++pp; offsetp:=0 fi
		offsetq+:=bitwidthx
		if offsetq>=8 then ++qq; offsetq:=0 fi
	od	
end

global function bits_bytesize(object p)int=
!return how many bytes are used by the object
!should be bits, but set should work; also array?

	return getbitssize(p.ubits.length, p.ubits.elemtype)
end

global function getbitssize(int n, t)int=
!return bytesize of n bit elements of type t
!will round to whole number of 64-bit values, but expressed as bytes
	int nbits:=n*ttbitwidth[t]
	return ((nbits-1)/64+1)*8			!bytes required in 64-bit blocks
end
=== qq_decimal.m 29/32 ===
import* qq
import mbignum

global proc var_make_decimal(ichar s, int length, variant dest)=
	ichar t

!must zero-terminate
	t:=pcm_alloc(length+1)
	memcpy(t,s,length)
	(t+length)^:=0

	makebnvar(dest,bn_makestr(s,length))
	pcm_free(t,length+1)
end

function makebnvar(variant dest,bignum bn=nil)bignum=
!dest is an uninitialised variant
!set it up to point to in initialised bignum handle
	object p

	if bn=nil then
		bn:=bn_init()
	fi

	p:=obj_new()
	p.udec.bnptr:=bn

	dest.tagx:=tdecimal ior hasrefmask
	dest.objptr:=p
	return bn
end

global function var_tostr_decimal(variant a,int fmt)ichar=
	return bn_tostring(a.objptr.udec.bnptr,fmt)
end
=== mbignum.m 30/32 ===
!(Decimal 'bignumber' library for integers and floats)

import clib
import mlib
import oslib

const digitwidth   = 9
const digitbase	= 1000000000
const digitfmt	 = "%09d"
const mdigitfmt	 = "z9"

const digitmax	 = digitbase-1

export type bignum  = ref bignumrec
type elemtype = int32
const elemsize = elemtype.bytes

export record bignumrec =
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

export proc bn_print(bignum a,int format=0)=
	ichar s

	s:=bn_tostring(a,format)
	print s
!   free(s)
end

export proc bn_println(bignum a, int format=0)=
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

export function bn_alloc(int size)ref void=
	ref void p

	p:=pcm_alloc(size)
	if p=nil then
		abortprogram("bignum:out of memory")
	fi

	return p
end

export function checkedmalloc(int size)ref void=
	ref void p

	p:=malloc(size)
	if p=nil then
		abortprogram("CM:Out of memory")
	fi

	return p
end

export proc bn_free(bignum a)=
!free digit memory and descriptor
	if a then
		bn_setzero(a)
		freemem(a,bignumrec.bytes)
	fi
end

proc freemem(ref void p, int size)=
#(my own deallocator needs the size; C's free() doesn't)
	pcm_free(p,size)
end

export proc bn_setzero(bignum a)=
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

export proc bn_move(bignum a,b)=
#move contents of b to a. Original value of a is cleared; b becomes zero

bn_setzero(a)
a^:=b^
memset(b,0,bignumrec.bytes)
end

export proc bn_dupl(bignum a,b)=
#copy contents of b to a. Each copy is independent
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

export proc bn_setinf(bignum dest) =
	bn_setzero(dest)
	dest^.numtype:=inf_type
end

export proc bn_setnan(bignum dest) =
	bn_setzero(dest)
	dest^.numtype:=nan_type
end

proc bn_error(ichar mess) =
	print "BN:"
	abortprogram(mess)
end

export function bn_iszero(bignum a)int=
	return a^.numtype=zero_type
end

export proc bn_negto(bignum a)=
	if not bn_iszero(a) then
		a^.neg:=not a^.neg
	fi
end

export proc bn_absto(bignum a)=
	a^.neg:=0
end

export function bn_isint(bignum a)int =
	return a^.length<=a^.expon+1
end

export function bn_getprec(bignum a)int=
	return a^.length*digitwidth
end

export proc bn_setprec(bignum a,int prec)=
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

	if oldlength<=newlength then
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

export function bn_getglobalprec:int=
	return currprec*digitwidth
end

export proc bn_setglobalprec(int prec)=
	currprec:=((prec-1)/digitwidth+1)
end

export function bn_makeint(int x)bignum =
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
		print @str,x
		a:=bn_makestr(&.str)
	fi

	return a
end

export function bn_makefloat(real64 x)bignum =
	bignum a
	[2048]char str

	sprintf(&.str,"%.30g",x)

	return bn_makestr(&.str)
end

export proc bn_ipower(bignum d, a,int64 n)=
#return a**b for bigints
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

export function bn_equal(bignum a,b)int=
	if a^.length<>b^.length or 
	   a^.numtype<>b^.numtype or 
	   a^.neg<>b^.neg or 
	   a^.expon<>b^.expon then
		return 0
	fi

	if a^.length=0 then return 1 fi

	return eqbytes(a^.num,b^.num,a^.length*elemsize)
end

export proc bn_addu(bignum dest,a,b)=
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

function smalldiv(ref elemtype x, b, int &xlen, nb)int =
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

export proc bn_idivu(bignum dest,a,b,rm=nil)=
#neither a nor b are zero; both are positive
#integer divide

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

export function bn_makestr(ichar s, int length=0)bignum=
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
		n:=sprintf(t,(i>0 or prel|digitfmt|"%d"),a^.num^[i])
		t+:=n
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

export function bn_tostring(bignum a,int fmt=0)ichar=
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

	print @t,x,,"."
	t+:=strlen(t)

	if shift then
		print @t, shift:"v",,a^.num^[0]-x*scale:"z*"
		t+:=strlen(t)
	fi

	for i to a^.length-1 do
		print @t,a^.num^[i]:mdigitfmt
		t+:=strlen(t)
	od

	while (t-1)^='0' and (t-2)^<>'.' do
		--t
	od

	print @t,"e",,expon
	t+:=strlen(t)
	t^:=0

	return s
end

export function bn_add(bignum dest,a,b)int=
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

export function bn_sub(bignum dest,a,b)int=
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

export function bn_mul(bignum dest,a,b)int=
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

export function bn_mulp(bignum dest,a,b, int prec)int=
	int res:=bn_mul(dest,a,b)
	if res then
		bn_setprec(dest,(prec=0|currprec|prec))
	fi
	return res
end

export function bn_div(bignum dest,a,b,int prec=0)int=
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

	if neg then
		bn_negto(dest)
	fi
	return 1
end

export function bn_idiv(bignum dest,a,b)int=
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

export function bn_idivrem(bignum dest,rm,a,b)int=
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

export function bn_irem(bignum dest,a,b)int=
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

export function bn_cmp(bignum a,b)int=
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

export function bn_const(int value)bignum =
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

export function bn_sign(bignum a)int=
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

export function bn_digits(bignum a)int=
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

export function bn_toint(bignum a)int64=
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

export function bn_tofloat(bignum a)real64=
	real64 x
	ichar s

	if bn_iszero(a) then
		return 0.0
	fi

	s:=bn_tostring(a,'E')

	sscanf(s,"%lf", &x)
	return x
end

export proc bn_fix(bignum c, a) =
	if bn_iszero(a) or a^.expon<0 then
		bn_setzero(c)
		return
	fi

	bn_dupl(c,a)
	if not bn_isint(c) then
		bn_setprec(c,(c^.expon+1)*digitwidth)
	fi
end
=== qq_calldll.m 31/32 ===
import* qq
import osdll

global proc calldll(symbol d, variant args, result, int nargs)=
	symbol e
	const maxparams=100
	[maxparams]int arglist
	int n, retcode, retval,fnindex,libindex
	word dllinst
	ref proc fnaddr
	ichar name

!CPL "CALLDLL",D.NAME,=NARGS,=TTNAME[D.MODE]

	if nargs>maxparams then pcerror("Too many dll args") fi

	e:=d.deflist
	n:=0

	for i to nargs do
		if e=nil then		!need to check for variadic
			if d.mvarparams then
				arglist[i]:=vartopacked(args,nil)
				++args
			else
				pcerror("Too many dll args")
			fi

		else
			arglist[i]:=vartopacked(args,e)
			++args
			e:=e.nextdef
		fi
	od

	if d.mode in [tpr64] then
		retcode:='R'
	else
		retcode:='I'
	fi
	fnaddr:=dllprocaddr[d.index]
	if fnaddr=nil then
		fnaddr:=loaddllfunction(d)
	fi

	retval:=os_calldllfunction(fnaddr, retcode, nargs,&arglist, nil)

	packedtovar(retval, d.mode, result)
end

function vartopacked(variant p, symbol d)word=
!convert variant to packed type matching d.mode that will fit into a word
!when d is nil, means variadic arg
	int s:=p.tag, t
	object a

	if d=nil then				!variadic
		case s
		when tstring then
			a:=p.objptr
			return word@(convCstring(a.ustr.strptr,a.ustr.length))
		when tint,treal,tword,trefpack then
			return p.value
		else
			pcerror("Bad variadic param")
		esac
	fi

	t:=d.mode

	switch ttbasetype[t]
	when tpi64, tpu64, tpi32, tpu32, tpref then
		case s
		when tint, tword, trefpack,trefvar then
			return p.value
		when treal then
			return word@(p.xvalue)
		else
error::
			println ttname[s],"should be", ttname[t]," param:",d.name, d.index
			pcerror("DLL: wrong param type")
		esac

	when tpr64 then
		case s
		when tint, tword then
			return word@(real(p.value))
		when treal then
			return word@(p.xvalue)
		else
			error
		esac
	when tpcstring then
		case s
		when tstring then
			a:=p.objptr
			return word@(convCstring(a.ustr.strptr,a.ustr.length))
		else
			error
		esac
	when tpref then
		case s
		when trefpack then
			return word(p.uref.ptr)
		else
			error
		esac
	else
		pcmxtypestt("DLL params:",s,t)
	end

	return 0

end

proc packedtovar(word retval, int t, variant dest)=
!convert packed value retval of type t to variant
!assume variant is ready to receive value (eg. is void)
	int tbase:=ttbasetype[t]
	object a

	switch tbase
	when tpvoid then
	when tpr64 then
		dest.tagx:=treal
		dest.xvalue:=real@(retval)
	when tpr32 then
		PCERROR("dll/r32ret")
	when tpi64,tpu64 then
		dest.tagx:=tint
		dest.value:=retval
	when tpi32 then
		dest.tagx:=tint
		dest.value:=int32(retval)
	when tpu32 then
		dest.tagx:=tint
		dest.value:=word32(retval)
	when tpi16 then
		dest.tagx:=tint
		dest.value:=int16(retval)
	when tpu16 then
		dest.tagx:=tint
		dest.value:=word16(retval)
	when tpref then
		dest.tagx:=trefpack
		dest.uref.ptr:=cast(retval)
		dest.uref.elemtag:=tttarget[t]

	else
		pcerror_s("Rettype not supported:",ttname[t])
	endswitch
end

function loaddllfunction(symbol d)ref proc=
	int fnindex,libindex
	word dllinst
	ref proc fnaddr
	ichar name

!CPL "LOADING",D.NAME

	fnindex:=d.index
	fnaddr:=dllprocaddr[fnindex]

	if fnaddr=nil then
		libindex:=dlllibindex[fnindex]
		dllinst:=dllinsttable[libindex]
		if dllinst=0 then
			dllinst:=os_getdllinst(dlltable[libindex].name)
			if dllinst=0 then
				pcerror_s("Can't load DLL:",dlltable[libindex].name)
			fi
			dllinsttable[libindex]:=dllinst
		fi

		name:=(d.truename|d.truename|d.name)
		fnaddr:=os_getdllprocaddr(dllinst,name)

		if fnaddr=nil then
			pcerror_s("Can't find DLL func:",name)
		fi
		dllprocaddr[fnindex]:=fnaddr
	fi

	return fnaddr
end
=== mwindll.m 32/32 ===
import clib
import mlib

!IMPORT OSWINDLLC

global function os_calldllfunction(ref proc fnaddr,
		int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
	word64 a
	real64 x
	int oddstack, nextra, pushedbytes

!	return os_calldllfunctionc(fnaddr,retcode,nargs,args,argcodes)

	oddstack:=nextra:=0

	assem
		test astack,8
		jz L100
		mov byte [oddstack],1
L100:
	end

	if oddstack then
		if nargs<5 then
			nextra:=5-nargs
		elsif nargs.even then
			nextra:=1
		fi

	else
		if nargs<4 then
			nextra:=4-nargs
		elsif nargs.odd then
			nextra:=1
		fi
	fi

	pushedbytes:=(nextra+nargs)*8

!RETURN 0

!CPL "D4"
	to nextra do
		assem
			push 0
		end
	od
!CPL "D5"

	for i:=nargs downto 1 do
		a:=args^[i]					!get generic 64-bit value to push
		assem
			push word64 [a]
		end
	od

!CPL =NEXTRA+NARGS,=pushedbytes,=oddstack

!load first 4 args to registers; this first version will blindly load 4 args
!(even if there are less) to both integer and xmm registers. Should be int/pointer
!types to integer regs; float types to xmm; and variadic to both
	assem
		mov D10,[Dstack]
		movq XMM0,[Dstack]
		mov D11,[Dstack+8]
		movq XMM1,[Dstack+8]
		mov D12,[Dstack+16]
		movq XMM2,[Dstack+16]
		mov D13,[Dstack+24]
		movq XMM3,[Dstack+24]
	end

	if retcode='I' then
		a:=((ref function:int64(fnaddr))^())
		asm add Dstack,[pushedbytes]
		return a
	else
		x:=((ref function:real64(fnaddr))^())
		asm add Dstack,[pushedbytes]
		return word64@(x)
	fi
end	

global function os_pushargs(ref[]word64 args, int nargs, nextra,
					ref proc fnaddr, int isfloat)word64=
!	a:=os_pushargs(&wordargs, na, nextra, fnaddr, retttype=tp_r64)
!implements central part of 'callapplproc' which needs to be in asm
	word64 a
	real64 x

CPL "PUSH ARGS",NARGS, NEXTRA

	to nextra do
		asm	push 0
	end

CPL "PUSH ARGS2"
	for i to nargs do
		a:=args[i]
		asm push word64 [a]
	od
CPL "PUSH ARGS3"

	if isfloat then
		x:=((ref function:real64(fnaddr))^())
		a:=int64@(x)
	else
		a:=((ref function:int64(fnaddr))^())
	fi

	return a
end
=== end ===
