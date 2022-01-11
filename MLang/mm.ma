=== MA 44 ===
=== mm.m 0 0 1/44 ===
	module mmcli
	module mm_blockpcl
	module mm_decls
	module mm_diags
!	module mm_diags_dummy
	module mm_export
	module mm_genpcl
	module mm_lex
	module mm_lib
	module mm_libsources
!	module mm_libsources_dummy
	module mm_modules
	module mm_name
	module mm_parse
!	module mm_start
	module mm_support
	module mm_tables
	module mm_type

	subprog pc
		module pc_decls
		module pc_disasm
		module pc_genmcl
		module pc_genss
		module pc_lex
		module pc_libmcl
		module pc_libpcl
		module pci_mcl as md
!		module pc_mcxdecls
		module pc_objdecls
		module pc_optim
		module pc_parse
		module pc_runmx
		module pc_stackmcl
		module pc_tables
		module pc_win64
		module pc_writeexe
		module pc_writemx
		module pc_writess
		module rundecls


		module runshow
		module runmx
=== mmcli.m 0 0 2/44 ===

!macro SHOW(m) = println m
macro SHOW(m) = eval 0

tabledata() []ichar optionnames=

	(header_sw,		"header"),
	(load_sw,		"load"),
	(fixup_sw,		"fixup"),
	(parse_sw,		"parse"),
	(name_sw,		"name"),
	(type_sw,		"type"),

	(pcl_sw,		"pcl"),
	(asm_sw,		"asm"),
	(asm2_sw,		"c"),
	(obj_sw,		"obj"),
	(mx_sw,			"mx"),
	(ml_sw,			"ml"),
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
	(mas_sw,		"mas"),
	(docs_sw,		"docs"),
	(export_sw,		"exp"),
	(lib_sw,		"lib"),

	(opt_sw,		"opt"),
	(opt1_sw,		"opt1"),
	(opt2_sw,		"opt2"),

	(ast1_sw,		"ast1"),
	(ast2_sw,		"ast2"),
	(ast3_sw,		"ast3"),
	(showmx_sw,		"showmx"),
	(showpcl_sw,	"showpcl"),
	(showasm_sw,	"showasm"),
	(st_sw,			"st"),
	(pst_sw,		"pst"),
	(stflat_sw,		"stflat"),
	(types_sw,		"types"),
	(overloads_sw,	"overloads"),
	(ss_sw,			"ss"),
	(showmodules_sw,"modules"),

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

ichar outext=""				!for reporting of primary o/p file

int startclock,endclock
int cmdskip

ichar inputfile

proc start=
	start_common('W','X64')
end

global proc start_common(int os, target)=
	unit p,q,r
	int m,fileno,ntokens,t

	startclock:=os_clock()

	initdata(os,target)

	getinputoptions()

	readprojectfile(inputfile)

	if fverbose>=1 then
		if passlevel=run_pass then
			println "Compiling",inputfile,"to memory"
		else
			fprint "Compiling # to #",inputfile:"14jlp-",changeext(outfile,outext),$
		fi
		print (msyslevel+1|" [No sys]"," [Min sys]" | " [Full sys]")
		println
	fi

	remove(logfile)

	do_loadmodules()

	do_parse()

	do_name()

	do_type()

	do_writema()

	do_writeexports()

	do_genpcl()

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
	if passlevel<load_pass then return fi

	loadmodules()

	addspecialtypes()
end

proc do_parse=
	if passlevel<parse_pass then return fi

	if fwritedocs then
		docfile:=fopen(changeext(outfile,"txt"),"w")
	fi

	for i:=2 to nmodules do
		parsemodule(i)
	od
	parsemodule(1)

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

	if fshowmodules then showprojectinfo(logdev) fi

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
	else
		loaderror("Bad os/target")
	esac
end

proc getinputoptions=
	const slash='-'
	int i,j,k
	int paramno,pmtype,sw,ncolons,passfixed
	ichar name,value,filename,ext
	[300]char filespec

	prodmode:=1
	paramno:=1
	ncolons:=0

	while pmtype:=nextcmdparamnew(paramno,name,value,"m") do
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
			if inputfile then
				loaderror("Specify one lead module only")
			fi
			convlcstring(name)
			inputfile:=pcm_copyheapstring(name)

			if passlevel=run_pass then
				cmdskip:=paramno-1+$CMDSKIP
				exit
			fi

		when pm_libfile then
			loaderror("Lib files go in module headers")
		else
			loaderror("Invalid params")
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

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "	",,cmdparams[0],"filename[.m]     # Compile project to executable"
		println "	",,cmdparams[0],"-help            # Other options"
		stop

	else
		filename:=inputfile					!primary file name
!default output
		outfile:=pcm_copyheapstring(filename)
		if fwritema then
			outext:="ma"
		fi

		if destfilename then
			outfile:=destfilename
		elsif destfilepath then
			strcpy(&.filespec,destfilepath)
			strcat(extractfile(&.filespec), outfile)
			outfile:=pcm_copyheapstring(&.filespec)	
		fi
	fi

	asmfilename:=getoutfilename(outfile,"asm")
	pclfilename:=getoutfilename(outfile,"pcl")
	exefilename:=getoutfilename(outfile,"exe")
	libfilename:=getoutfilename(outfile,(libmode|"ml"|"mx"))

	objfilename:=getoutfilename(outfile,"obj")
	mafilename:=getoutfilename(outfile,"ma")

	strcpy(filespec,changeext(outfile,""))
	strcat(filespec,"_exp")
	expfilename:=getoutfilename(filespec,"m")
end

proc do_option(int sw, ichar value)=
	static byte outused, outpathused

	switch sw
	when header_sw then passlevel:=header_pass
	when load_sw then passlevel:=load_pass
	when parse_sw then passlevel:=parse_pass
	when fixup_sw then passlevel:=fixup_pass
	when name_sw then passlevel:=name_pass
	when type_sw then passlevel:=type_pass
	when pcl_sw then passlevel:=pcl_pass; outext:="pcl"
	when asm_sw then passlevel:=asm_pass; outext:="asm"
	when obj_sw then passlevel:=obj_pass; outext:="obj"
	when exe_sw then passlevel:=exe_pass; outext:="exe"
	when mx_sw then passlevel:=lib_pass; outext:="mx"
	when ml_sw then passlevel:=lib_pass; outext:="ml"; libmode:=1
	when run_sw then passlevel:=run_pass; outext:="mem";
	when pclexe_sw then passlevel:=pcl_pass; outext:="pcl"; fpclexe:=1
	when asmexe_sw then passlevel:=asm_pass; outext:="asm"; fasmexe:=1

	when ma_sw then fwritema:=1; outext:="ma"
	when mas_sw then fwritema:=2; outext:="ma"
	when export_sw then fwriteexports:=1
	when docs_sw then fwritedocs:=1
	when lib_sw then libmode:=1

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
	when showmx_sw then fshowmx:=1
	when showpcl_sw then fshowpcl:=1
	when showasm_sw then fshowasm:=1
	when st_sw then fshowst:=1
	when pst_sw then fshowpst:=1
	when stflat_sw then fshowstflat:=1
	when types_sw then fshowtypes:=1
	when overloads_sw then fshowoverloads:=1
	when ss_sw then fshowss:=1
	when showmodules_sw then fshowmodules:=1

!	when windows_sw then fwindows:=1
!	when linux_sw then flinux:=1
!	when x64_sw then fx64:=1
!	when c64_sw then fc64:=1
!	when c32_sw then fc32:=1

	endswitch

end

proc showcaption=
	println "M Compiler", $date, $time
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
	for i to segmentnames.upb do
		strcpy(&.str,segmentnames[i])
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

!proc showmodules=
!
!	println "Modules:",nmodules
!	for i to nmodules do
!!	print moduletable[i].name, sourcefilepaths[moduletable[i].fileno],$
!		println moduletable[i].name, sourcefilepaths[moduletable[i].fileno],$
!
!		print "                 "
!		for k:=1 to nmodules do
!			fprint "# ",moduletable[i].importmap[k]
!		od
!		println
!
!		for k:=1 to nmodules when moduletable[i].importmap[k] do
!			println "		",moduletable[k].name
!		od
!
!		println
!	od
!end

proc do_writeexports=
	[300]char str

	if not fwriteexports and passlevel<>lib_pass then
		return
	fi

	if not libmode then return fi

	writeexports(expfilename,extractbasefile(libfilename))
	if fwriteexports then
		stop
	fi
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

	for i to nlibfiles when libfiles[i]^<>'$' do
		pcl_addlibfile(libfiles[i],libtypes[i])
	od

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
	when lib_pass then
		pcl_writelibfile(libfilename,foptim)
	when run_pass then
		pcl_runlibfile(libfilename,foptim,fshowmx, cmdskip)
	esac
end

=== mm_blockpcl.m 0 0 3/44 ===
const kjumpt = 1		!pseudo ops used for conditional jump logic
const kjumpf = 0

const dodotchains=1

const maxnestedloops	= 50

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

global macro getmemmode_m(p) = (p.memmode|p.memmode|p.mode)

global proc evalunit(unit p)=
!p is a single executable unitrec; not a list or const
!should be called via execblock() to executate a code item in a unitrec
	unit a,b
	symbol d
	ref[]int32 pmult

	if p=nil then return fi
	mlineno:=p.pos

	a:=p.a
	b:=p.b

	switch p.tag
	when j_const         then do_const(p)
	when j_null          then
	when j_name          then do_name(p)
	when j_block,j_stmtblock then
				         do_block(p)
	when j_callproc      then do_callproc(p,a,b,0)
	when j_return        then do_return(p,a)
	when j_returnmult    then do_returnmult(p,a)
	when j_assign        then do_assign(p,a,b)
	when j_to            then do_to(p,a,b)
	when j_if            then do_if(p,a,b,p.c,0)
	when j_longif        then do_longif(p,a,b,0)
	when j_forup         then do_for(p,a,b,p.c,0)
	when j_fordown       then do_for(p,a,b,p.c,1)
	when j_forall        then do_forall(p,a,b,p.c,0)
	when j_forallrev     then do_forall(p,a,b,p.c,1)
	when j_while         then do_while(p,a,b,p.c)
	when j_repeat        then do_repeat(p,a,b)
	when j_goto          then do_goto(a)
	when j_labeldef      then do_labeldef(p)
	when j_restart       then do_exit(p,1)
	when j_redo          then do_exit(p,2)
	when j_next          then do_exit(p,3)
	when j_exit          then do_exit(p,4)
	when j_do            then do_do(p,a,b)
	when j_case          then do_case(p,a,b,p.c,0,0)
	when j_docase        then do_case(p,a,b,p.c,1,0)
	when j_switch        then do_switch(p,a,b,p.c,0,0)
	when j_doswitch      then do_switch(p,a,b,p.c,1,0)
	when j_recase        then do_recase(p,a)
	when j_swap          then do_swap(p,a,b)
	when j_select        then do_select(p,a,b,p.c,0)
	when j_print,j_println then
		do_print(p,a,b)
	when j_fprint,j_fprintln, j_cprint, j_cprintln then
		do_print(p,a,b)
	when j_read	        then do_read(p,a)
	when j_readln        then do_readln(a)
	when j_stop          then do_stop(p,a)
	when j_eval          then
		evalunit(a)
		pcl_gen(keval)
	when j_andl          then do_andl(p,a,b)
	when j_orl           then do_orl(p,a,b)

	when j_makerange     then PCL_GENCOMMENT("MAKERANGE")
	when j_callfn        then do_callproc(p,a,b,1)

	when j_cmp           then do_setcc(p,a,b)
	when j_cmpchain      then do_setccchain(p,a)

	when j_bin           then do_bin(p,a,b)
	when j_index         then do_index(p,a,b)
	when j_slice         then do_slice(a,b)
	when j_makeslice     then
		evalunit(b)
		evalunit(a)
		pcl_gen(kmakeslice)
		setmode(tu128)

	when j_dotindex      then do_dotindex(p,a,b)
	when j_dotslice      then do_dotslice(p,a,b)
	when j_dot           then do_dot(p)
	when j_ptr           then do_ptr(p,a)
	when j_addrof        then evalref(a,b)
	when j_addroffirst   then evalref(a)
	when j_convert       then do_convert(p,a)
	when j_typepun       then do_typepun(p,a)
	when j_shorten       then do_shorten(p,a)
	when j_typeconst     then do_typeconst(p)

	when j_unary         then do_unary(p,a)

	when j_notl          then do_notl(p,a)
	when j_istruel       then do_istruel(p,a)

	when j_incr          then
		if p.pclop in [kincr, kdecr] then
			do_incr(p,a)
		else
			do_incrload(p,a)
		fi
!
	when j_binto         then do_binto(p,a,b)
!
	when j_unaryto       then do_unaryto(p,a)
!
	when j_syscall then
		do_syscall(p,a)

	when j_assem         then
		pcl_gen(kassem,pcl_genassem(p))
		setmode_u(p)

	when j_cvlineno      then
		pcl_gen(kpush,pcl_genint(p.lineno iand 16777215))

	when j_empty         then do_empty(p,a)

	else
		PRINTLN "UNSUPPORTED TAG: ",JTAGNAMES[P.TAG],
				MLINENO IAND 16777215, SOURCEFILENAMES[MLINENO>>24]
		pcl_gencomment("Unimplemented:")
		pcl_gencomment(jtagnames[p.tag])
		return
	endswitch

	if p.mode<>tvoid and not p.resultflag then
		case p.tag
		when j_assign, j_callproc, j_syscall then

		else
			pcl_gen(kpopstack)
			setmode_u(p)
		esac
	fi
end

proc evalref(unit p, q=nil)=
	unit a,b,c
	a:=p.a
	b:=p.b
	c:=p.c
	mlineno:=p.pos

	switch p.tag
	when j_name then
		genpushmemaddr_d(p.def)
		if q then					!addrof may have optional byte offset
			genpushint(q.value)
			pcl_gen(kaddrefoff)
			pcl_setscale(1)
!			pccurrxx.scale:=1
			setmode(tu8)
		fi
	when j_index then
		do_indexref(a,b)

	when j_dot then
		do_dotref(p)

	when j_ptr then
		evalunit(p.a)

	else
		case p.tag
		when j_if then
			do_if(p,a,b,c,1)
		when j_longif then
			do_longif(p,a,b,1)
!		when j_select then
!			do_select(p,a,b,c,1)
!		when j_switch then
!			do_switch(p,a,b,c,0,1)
!		when j_case then
!			do_case(p,a,b,c,0,1)
		else
			PRINTUNIT(P)
			gerror("evalref")
		esac
	end switch
end

proc evallv(unit p)=
	evalref(p)
end

global proc evalunitx(unit p, int isref) =
!call either evalunit (isref=0) or evalref(isref=1)
	if isref then
		evalref(p)
	else
		evalunit(p)
	fi
end

global proc evalblock(unit p) =
	evalunit(p)
end

proc evalarray(unit p)=
	if ttbasetype[p.mode]=tslice then
		evalunit(p)
		pcl_gen(ksliceptr)
		setmode(tu64)
	elsif p.mode=trefchar then
		evalunit(p)
	else
		evalref(p)
	fi

end

proc do_block(unit p)=
	unit a:=p.a

	while a do
		evalunit(a)
		a:=a.nextunit
	od
end

proc genjumpcond(int opc,unit p,int lab)=
!p is some conditional expression of arbitrary complexity
!opc is kjumpf or kjumpt
!evaluate and generate jumps as needed
	unit q,r,s
	int lab2,i

	q:=p.a
	r:=p.b

	switch p.tag
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
		evalunit(q)

		pcl_gen((opc=kjumpt|kjumptrue|kjumpfalse),pcl_genlabel(lab))
		setmode_u(q)

	when j_block then
		while q and q.nextunit do
			evalunit(q)
			q:=q.nextunit
		od
		genjumpcond(opc,q,lab)

	when j_cmp then

		gcomparejump(opc,p.pclop,q,r,lab)

	when j_inrange then
		evalunit(q)
		evalunit(r.a)
		evalunit(r.b)
		pcl_gen((opc=kjumpf|kjumpnotinrange|kjumpinrange),pcl_genlabel(lab))
		setmode_u(q)

	when j_inset then
		s:=r.a
		if s=nil then
			gerror("empty set")
		fi

		if opc=kjumpf then
			lab2:=createfwdlabel()
			evalunit(q)

			while s do
				evalunit(s)
				s:=s.nextunit
				if s then
					pcl_gen(ksetjumpeq,pcl_genlabel(lab2))
				else
					pcl_gen(ksetjumpne,pcl_genlabel(lab))
				fi
				setmode_u(q)
			od
			definefwdlabel(lab2)
		else
			evalunit(q)

			while s do
				evalunit(s)
				s:=s.nextunit
				pcl_gen((s|ksetjumpeq|ksetjumpeqx),pcl_genlabel(lab))
				setmode_u(q)
			od
		fi

	when j_cmpchain then
		r:=q.nextunit
		i:=1
		if opc=kjumpf then
			while r do
				evalunit(q)
				evalunit(r)
				pcl_gen(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq),pcl_genlabel(lab))
				setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
		
		else
			lab2:=createfwdlabel()
			while r do
				evalunit(q)
				evalunit(r)
				if r.nextunit then
					pcl_gen(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq),pcl_genlabel(lab2))
				else
					pcl_gen(condtopclop(p.cmpgenop[i],kjumpeq),pcl_genlabel(lab))
				fi
				setmode_u(q)
				++i
				q:=r
				r:=r.nextunit
			od
			definefwdlabel(lab2)
		fi
	else			!other, single expression
		evalunit(p)
		if p.mode not in [ti64,tu64] then gerror("jumptrue/not i64") fi

		pcl_gen((opc=kjumpt|kjumptrue|kjumpfalse),pcl_genlabel(lab))
		setmode(ti64)
	endswitch
end

proc gcomparejump(int jumpopc,int cond,unit lhs,rhs,int lab)=
!jumpopc is the base cmdcode needed: kjumpt or kjumpt
!p is the eq/compare unit
!convert into jumpcc cmdcode
	int opc

	if jumpopc=kjumpf then			!need to reverse condition
		cond:=reversecond(cond)		!eqop => neop, etc
	fi

	evalunit(lhs)
	evalunit(rhs)

	pcl_gen(condtopclop(cond,kjumpeq),pcl_genlabel(lab))
	setmode_u(lhs)
end

proc genjumpl(int lab)=
!generate unconditional jump to label
	pcl_gen(kjump,pcl_genlabel(lab))
end

proc unimpl(ichar mess)=
	gerror_s("Unimplemented: #",mess)
end

proc do_const(unit p) =
	int mode:=p.mode

	if ttisinteger[mode] then
		if ttsize[mode]<16 then
			genpushint(p.value,mode)
		else
			pcl_gen(kpush, pcl_genint128(p.value128,mode))
		fi
	elsif ttisreal[mode] then
		if ttsize[mode]=4 then
			genpushreal32(p.xvalue)
		else
			genpushreal(p.xvalue)
		fi

	elsif ttisref[mode] then
		if p.isastring then
			genpushstring(p.svalue)
		else
			genpushint(p.value)
		fi
	else
		gerror("do_const")
	fi
	setmode(mode)
end

proc do_name(unit p)=
	symbol d

	d:=p.def
	case d.nameid
	when procid,dllprocid then
		genpushmemaddr_d(d)
	when labelid then
		if d.index=0 then
			d.index:=++plabelno
		fi
		pcl_gen(kjump, pcl_genlabel(d.index))
		p.resultflag:=0
		p.mode:=tvoid
!
	when fieldid then
		genpushint(d.offset,ti64)


	else
		genpushmem_d(d)
!		setmode(getmemmode(p))
		pccurr.mode:=getpclmode(getmemmode_m(p))
		pccurr.size:=ttsize[p.mode]
	esac
end

proc do_stop(unit p,a) =
	if a then
		evalunit(a)
	else
		pcl_gen(kpush,pcl_genint(0))
	fi
	pcl_gen(kstop)
end

proc do_andl(unit p,a,b) =
	int labfalse, labend

	pcl_gen(kstartmult)

	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpf,a,labfalse)
	genjumpcond(kjumpf,b,labfalse)

	genpushint(1)
	pcl_gen(kresetmult)

	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pcl_gen(kendmult)

	definefwdlabel(labend)
end

proc do_orl(unit p,a,b) =
	int labtrue, labfalse, labend

	pcl_gen(kstartmult)
	labtrue:=createfwdlabel()
	labfalse:=createfwdlabel()
	labend:=createfwdlabel()

	genjumpcond(kjumpt,a,labtrue)
	genjumpcond(kjumpf,b,labfalse)

	definefwdlabel(labtrue)
	genpushint(1)
	pcl_gen(kresetmult)
	genjumpl(labend)

	definefwdlabel(labfalse)
	genpushint(0)
	pcl_gen(kendmult)

	definefwdlabel(labend)
end

proc do_notl(unit p,a) =
	evalunit(a)
	pcl_gen(p.pclop)
	setmode(ti64)
end

proc do_istruel(unit p,a) =
	evalunit(a)
	if islogical(a) then
		return
	fi
	pcl_gen(p.pclop)
	setmode(ti64)
end

proc do_typepun(unit p, a) =
	evalunit(a)
	setmode_u(a)
	if a.mode=p.mode then return fi
	pcl_gen(ktypepun)
	setmode(p.convmode)
end

proc do_shorten(unit p, a) =
	evalunit(a)
end

global function islogical(unit p)int=			!ISLOGICAL
!return 1 if p is known to have a logical value
	case p.tag
	when j_istruel,j_notl,j_andl,j_orl,j_xorl then
		return 1
	esac
	return 0
end

proc do_assign(unit p,a,b) =
!fstore=1 when result is needed
	unit c
	symbol d
	int offset

!deal with list constructs on either side
!	if a.tag=j_makelist and b.tag=j_makelist then
!		do_multassign_lr(a,b)
!		return
!	elsif a.tag=j_makelist then
!		do_multassign_l(a,b)
!		return
!	elsif b.tag=j_makelist then
!		do_multassign_r(a,b)
!		return
!	fi

!Simple assignment, but look at block sizes
	if a.tag<>j_makelist and b.tag=j_makelist then
		if not p.resultflag then
			do_assignblock(p,a,b)		!(avoids pushing/popping block data)
			return
		fi
	fi

	if a.tag=j_makelist then
		if p.resultflag then gerror("multass/store") fi
		do_multassign(a,b)
		return
	elsif b.tag=j_callfn and ttbasetype[b.mode]=ttuple then
		do_multassign(a,b)
		return
	fi

	case a.tag
	when j_index then

		do_storeindex(p,a.a,a.b,b)
		return
	when j_slice then
GERROR("ASS/SLICE")

	when j_dot then
		do_storedot(a,a.b,b)
		return
	esac

	switch a.tag
	when j_name then
		evalunit(b)
		pcl_gen((p.resultflag|kstore|kpop), genmem_u(a))
	when j_ptr then
		evalunit(b)
		evalref(a)

		if pcl_getopcode()=kaddrefoff then 
			pcl_setopcode((p.resultflag|kstoreptroff|kpopptroff))
		else
			pcl_gen((p.resultflag|kstoreptr|kpopptr))
		fi
		setmode(getmemmode_m(a))

	when j_dotindex then
		evalunit(b)
		evalref(a.a)
		evalunit(a.b)
		pcl_gen((p.resultflag|kstoredotindex|kpopdotindex))
		setmode_u(a.a)
		return
	when j_dotslice then
		evalunit(b)
		evalref(a.a)
		evalunit(a.b.a)
		evalunit(a.b.b)
		pcl_gen((p.resultflag|kstoredotslice|kpopdotslice))
		setmode_u(a.a)
		return
	else
		cpl jtagnames[a.tag]
		gerror("Can't assign")
	end switch

	setmode_u(a)

end

proc do_bin(unit p,a,b) =
	int offset

	evalunit(a)

	if pcl_getopcode()=kaddrefoff and
			p.pclop in [kaddrefoff, ksubrefoff] and
		ttisref[a.mode] and ttisinteger[b.mode] and b.tag=j_const then
		offset:=ttsize[tttarget[a.mode]]*b.value
		if p.pclop=kaddrefoff then
			pcl_addoffset(offset)
		else
			pcl_addoffset(-offset)
		fi
		return
	fi

	evalunit(b)

	pcl_gen(p.pclop)
	setmode_u(p)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi

	if p.pclop=ksubref and ttisref[a.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi
end

proc do_setcc(unit p,a,b) =
	evalunit(a)
	evalunit(b)
	pcl_gen(condtopclop(p.pclop,kseteq))
	setmode_u(a)
end

proc do_setccchain(unit p,q) =
	int lab1,lab2,i
	unit r

	lab1:=createfwdlabel()
	lab2:=createfwdlabel()

	r:=q.nextunit
	i:=1

	pcl_gen(kstartmult)

	while r do
		evalunit(q)
		evalunit(r)
		pcl_gen(condtopclop(reversecond(p.cmpgenop[i]),kjumpeq),pcl_genlabel(lab1))
		setmode_u(q)
		++i
		q:=r
		r:=r.nextunit
	od

	genpushint(1)
	pcl_gen(kresetmult)
	pcl_gen(kjump, pcl_genlabel(lab2))

	definefwdlabel(lab1)
	genpushint(0)
	pcl_gen(kendmult)
	definefwdlabel(lab2)
end

proc do_binto(unit p,a,b)=
	evallv(a)
	evalunit(b)

	pcl_gen(p.pclop)
	setmode_u(a)

	if ttisref[a.mode] and ttisinteger[b.mode] then
		pcl_setscale(ttsize[tttarget[a.mode]])
	fi

end

proc do_unary(unit p,a) =
	evalunit(a)

	pcl_gen(p.pclop)
	setmode_u(p)
	if p.pclop=kupb and ttbasetype[a.mode]=tslice then
		pcl_setxy(ttlower[a.mode],0)
	fi

end

proc do_unaryto(unit p,a)=
	evallv(a)

	pcl_gen(p.pclop)
	setmode_u(a)
end

proc do_ptr(unit p,a)=

	evalunit(a)

!	if ttbasetype[p.mode] in [trecord,tarray] then
!		return
!	fi

	if pcl_getopcode()=kaddrefoff then 
		pcl_setopcode(kpushptroff)
	else
		pcl_gen(kpushptr)
	fi
	setmode(getmemmode_m(p))
end

proc do_labeldef(unit p)=
	symbol d
	[256]char str

	d:=p.def
	if d.index=0 then
		d.index:=++plabelno
	fi
	print @&.str,d.name,,"::"
	pcl_gencomment(&.str)
	pcl_gen(klabel,pcl_genlabel(d.index))
end

proc do_goto(unit a)=
	symbol d

	case a.tag
	when j_name then
		d:=a.def
		if d.index=0 then
			d.index:=++plabelno
		fi
		pcl_gen(kjump, pcl_genlabel(d.index))

	else
		gerror("goto ptr?")
	esac
end

proc do_do(unit p,a,b) =
	int lab_abc,lab_d

	lab_abc:=definelabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_abc, lab_abc, lab_abc, lab_d)

	evalblock(a)

	genjumpl(lab_abc)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_to(unit p,a,b) =
	unit cvar
	int lab_a,lab_b,lab_c,lab_d,count

	cvar:=p.c

	lab_a:=definelabel()
	a.mode:=ti64

	evalunit(a)
	pcl_gen(kpop,genmem_u(cvar))
	setmode(ti64)

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()
	stacklooplabels(lab_a,lab_b,lab_c,lab_d)

!check for count being nonzero
	if a.tag<>j_const then			!assume const limit is non-zero
		evalunit(cvar)
		evalunit(pzero)

		pcl_gen(kjumple,pcl_genlabel(lab_d))
		setmode(ti64)

	else
		count:=a.value
		if count<=0 then
			genjumpl(lab_d)
		fi
	fi

	definefwdlabel(lab_b)
	evalblock(b)			!main body

	definefwdlabel(lab_c)

	pcl_gen(kto,pcl_genlabel(lab_b))
	pcl_gen(kopnd,genmem_u(cvar))

	definefwdlabel(lab_d)
	--loopindex
end

proc do_while(unit p,pcond,pbody,pincr) =
	int lab_b,lab_c,lab_d,lab_incr

	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pincr then
		lab_incr:=createfwdlabel()
	else
		lab_incr:=lab_c
	fi

	stacklooplabels(lab_c, lab_b, lab_c, lab_d)

	genjumpl(lab_incr)		!direct to condition code which is at the end

	definefwdlabel(lab_b)

	evalblock(pbody)

	definefwdlabel(lab_c)

	if pincr then
		evalblock(pincr)
		definefwdlabel(lab_incr)
	fi

	docond(kjumpt,pcond,lab_b)
	definefwdlabel(lab_d)
	--loopindex
end

proc do_repeat(unit p,a,b) =
	int lab_ab, lab_c, lab_d

	lab_ab:=definelabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	stacklooplabels(lab_ab, lab_ab, lab_c, lab_d)

	evalblock(a)

	definefwdlabel(lab_c)

	unless b.tag=j_const and b.value=0 then
		docond(kjumpf,b,lab_ab)
	end

	definefwdlabel(lab_d)
	--loopindex
end

proc do_exit(unit p,int k) =
	int n,index

	index:=p.index
	if index=0 then index:=loopindex fi

	n:=findlooplabel(k,index)
	if n=0 then
		gerror("Bad exit/loop index",p)
	else
		genjumpl(n)
	fi
end

proc do_if(unit p,a,b,c, int isref) =
	int lab1,lab2,ismult

	ismult:=p.mode<>tvoid

	if ismult then pcl_gen(kstartmult) fi

	lab1:=createfwdlabel()

	docond(kjumpf,a,lab1)

	evalunitx(b,isref)
	if ismult then pcl_gen(kresetmult) fi

	if c then
		lab2:=createfwdlabel()			!label past else part
		genjumpl(lab2)
		definefwdlabel(lab1)
		evalunitx(c,isref)
		if ismult then pcl_gen(kendmult) fi
		definefwdlabel(lab2)
	else
		definefwdlabel(lab1)
	fi
end

proc do_longif(unit p,a,b, int isref) =
	int labend,i,lab2,ismult
	unit pcond

	labend:=createfwdlabel()
	ismult:=p.mode<>tvoid

	pcond:=a
	i:=0
	if ismult then pcl_gen(kstartmult) fi

	while pcond do
		++i
		lab2:=createfwdlabel()

		docond(kjumpf,pcond.a,lab2)

		evalunitx(pcond.b,isref)
		if ismult then pcl_gen(kresetmult) fi

		if pcond.nextunit or b then
			genjumpl(labend)
		fi
		definefwdlabel(lab2)
		pcond:=pcond.nextunit
	od

	if b then
		evalunitx(b,isref)
		if ismult then pcl_gen(kendmult) fi
	fi
	definefwdlabel(labend)
end

proc do_return(unit p,a) =
	if a then
		evalunit(a)

		if ttbasetype[p.mode] in [tslice, ti128, tu128] then
			pcl_genx(ksetretmult,2)

		else
			pcl_gen(ksetret)
		fi
		setmode_u(a)
	fi
	genjumpl(retindex)
end

proc do_returnmult(unit p,a) =
	[maxparams]unit params
	unit q
	int nparams

	q:=a
	nparams:=0
	while q do
		if nparams>=maxparams then gerror("Mult?") fi
		params[++nparams]:=q
		q:=q.nextunit
	od

	for i:=nparams downto 1 do
		evalunit(params[i])
	od

!need individual setret codes (not sure about the order)
	pcl_genx(ksetretmult, nparams)

	genjumpl(retindex)
	p.resultflag:=1
end

proc do_callproc(unit p,a,b,int isfn) =
	[maxparams]unit paramlist
	int nparams,nmult,ffi,isptr,nslots,nvariadics, blockret, nret
	symbol d
	symbol dtemp
	ref[]int32 pmult
	unit q

	isptr:=0
	case a.tag
	when j_name then
		d:=a.def

	when j_ptr then
		d:=ttnamedef[a.mode]
		isptr:=1
	else
		gerror("call/not ptr")
	esac

	nparams:=0
	nslots:=0
	nvariadics:=0
	blockret:=0
	ffi:=0

!	if d.fflang in [clangff,windowsff] then
!		ffi:=1
!	fi

	q:=b
	while q do
		++nslots
		if nparams>=maxparams then gerror("maxparams") fi
		paramlist[++nparams]:=q
!		if ffi and d.varparams and nparams>=d.varparams and nparams<=4 and nvariadics=0 then
		if d.varparams and nparams>=d.varparams and nparams<=4 and nvariadics=0 then
			nvariadics:=nparams
		fi
		if ttbasetype[q.mode] in [tslice, ti128, tu128] then
			++nslots
		fi
		q:=q.nextunit
	od

	pcl_gen(ksetargs)
	pcl_setnargs(nslots)
	pcl_setnvariadics(nvariadics)

	for i:=nparams downto 1 do
		evalunit(paramlist[i])
	od

	if not isptr then
		pcl_gen((isfn|kcallfn|kcallproc), genmemaddr_d(d))
	else
		evalunit(a.a)
		pcl_gen((isfn|kcallfnptr|kcallprocptr))
	fi
	if isfn then
		setmode(getmemmode_m(p))
	fi

	if d.nretvalues>1 then
		nret:=d.nretvalues
		pmult:=ttmult[d.mode]

		for i to nret do
			pcl_gent(ktype, getpclmode(pmult[i]))
		od
	fi
end

proc do_print(unit p,a,b) =
	unit q,r,fmt
	int m, fn, needprintend

	if a then
		needprintend:=1
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi
		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			pcl_gen_sysproc(sysfn_print_startfile,a)
		when tc8 then
			pcl_gen_sysproc(sysfn_print_startstr,a)
		when tref then
			pcl_gen_sysproc(sysfn_print_startptr,a)
		else
			gerror("@dev?")
		esac
	else
		needprintend:=1
		pcl_gen_sysproc(sysfn_print_startcon)
	fi

	q:=b

	case p.tag
	when j_fprint,j_fprintln then
		if ttbasetype[q.mode]<>tref or ttbasetype[tttarget[q.mode]]<>tc8 then
			gerror("string expected")
		fi
		pcl_gen_sysproc(sysfn_print_setfmt,q)
		q:=p.c
	esac

	while q do
		case q.tag
		when j_fmtitem then
			fmt:=q.b
			r:=q.a
			m:=r.mode
		when j_nogap then
			pcl_gen_sysproc(sysfn_print_nogap)
			q:=q.nextunit
			next
		when j_space then
			pcl_gen_sysproc(sysfn_print_space)
			q:=q.nextunit
			next
		else
			fmt:=nil
			r:=q
			m:=q.mode
		esac

		switch ttbasetype[m]
		when ti64 then
			fn:=sysfn_print_i64
			if not fmt then fn:=sysfn_print_i64_nf fi
		when tu64 then
			fn:=sysfn_print_u64
		when tr32 then
			fn:=sysfn_print_r32
		when tr64 then
			fn:=sysfn_print_r64
		when ti128 then
			fn:=sysfn_print_i128
		when tu128 then
			fn:=sysfn_print_u128
		when tref then
			if tttarget[m]=tc8 or tttarget[m]=tarray and tttarget[tttarget[m]]=tc8 then
				fn:=sysfn_print_str
				if not fmt then fn:=sysfn_print_str_nf fi
			else
				fn:=sysfn_print_ptr
				if not fmt then fn:=sysfn_print_ptr_nf fi
			fi
		when tarray then
			GERROR("PRINTARRAY")
			q:=q.nextunit
		when trecord then
			GERROR("PRINTRECORD")
		when tslice then
			if tttarget[m]=tc8 then
				fn:=sysfn_print_strsl
			else
				gerror("PRINTSLICE")
			fi

		when tc64 then
			fn:=sysfn_print_c8

		else
			gerror_s("PRINT/T=#",strmode(m))
		end switch

		case fn
		when sysfn_print_i64_nf, sysfn_print_str_nf, sysfn_print_ptr_nf then
			pcl_gen_sysproc(fn, r)
		else
			pcl_gen_sysproc(fn, r, (fmt|fmt|pzero))
		esac

		q:=q.nextunit
	od

	case p.tag
	when j_println,j_fprintln then
		pcl_gen_sysproc(sysfn_print_newline)
	esac
	if needprintend then
		pcl_gen_sysproc(sysfn_print_end)
	fi
end

proc do_incr(unit p,a) =
	evallv(a)
	pcl_gen(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc setincrstep(int m)=
	pcl_setincr(1)

	if ttisref[m] then
		pcl_setincr(ttsize[tttarget[m]])
	fi
end

proc do_incrload(unit p,a) =
	evallv(a)
	pcl_gen(p.pclop)
	setmode_u(a)
	setincrstep(a.mode)
end

proc do_for(unit p,pindex,pfrom, pbody, int down) =
	unit pto, pstep, pelse, px, plimit, ptoinit
	int lab_a,lab_b,lab_c,lab_d,lab_e
	int a,b,stepx

	pto:=pfrom.nextunit
	pstep:=pto.nextunit
	pelse:=pbody.nextunit
	ptoinit:=pindex.nextunit

	if pto.tag=j_ptr then
		px:=pto.a
		symbol d
		if px.tag=j_name and (d:=px.def).nameid=paramid and
			 d.parammode=out_param then
			gerror("Possibly using &param as for-loop limit")
		fi
	fi

	lab_a:=definelabel()
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi

	stacklooplabels(lab_a, lab_b, lab_c, lab_d)

!now start generating code
	evalunit(pfrom)
	pcl_gen(kpop,genmem_u(pindex))
	setmode_u(pindex)

	if ptoinit then			!using temp for limit
		ptoinit.resultflag:=0
		evalunit(ptoinit)
	fi

	if pfrom.tag=j_const and pto.tag=j_const then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pcl_gen(kjump, pcl_genlabel(lab_e))
		fi
	else
		if pfrom.tag=j_const then				!reverse condition; compare mem:imm
			evalunit(pto)
			evalunit(pfrom)
			pcl_gen((down|kjumpgt|kjumplt),pcl_genlabel(lab_e))
		else
			evalunit(pindex)
			evalunit(pto)
			pcl_gen((down|kjumplt|kjumpgt),pcl_genlabel(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	if pstep then
		if pstep.tag<>j_const then
			gerror("for/step non-const not ready")
		fi
		stepx:=pstep.value
		if stepx<=0 then
			gerror("Bad for-step")
		fi
		pcl_genx((down|kfordown|kforup),stepx, pcl_genlabel(lab_b))
		setmode_u(pindex)
	else
		pcl_genx((down|kfordown|kforup),1, pcl_genlabel(lab_b))
		setmode_u(pindex)
	fi

	pcl_gen(kopnd, genmem_u(pindex))
	if pto.tag=j_const then
		pcl_gen(kopnd, pcl_genint(pto.value))
	else
		pcl_gen(kopnd, genmem_u(pto))
	fi

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_forall(unit p,pindex,plist, pbody, int down) =
	unit plocal, pfrom, pto, pelse, px, plimit, passign
	int lab_a,lab_b,lab_c,lab_d,lab_e
	int a,b,stepx

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit
	pelse:=pbody.nextunit

	lab_a:=definelabel()
	lab_b:=createfwdlabel()
	lab_c:=createfwdlabel()
	lab_d:=createfwdlabel()

	if pelse then
		lab_e:=createfwdlabel()
	else
		lab_e:=lab_d
	fi


	stacklooplabels(lab_a, lab_b, lab_c, lab_d)

!now start generating code

	evalunit(pfrom)
	pcl_gen(kpop, genmem_u(pindex))

	setmode_u(pindex)

	if pfrom.tag=j_const and pto.tag=j_const then
		a:=pfrom.value
		b:=pto.value
		if (down and a>=b) or (not down and a<=b) then	!in range
		else							!loop not executed
			pcl_gen(kjump, pcl_genlabel(lab_e))
		fi
	else
		if pfrom.tag=j_const then				!reverse condition; compare mem:imm
			evalunit(pfrom)
			evalunit(pto)
			pcl_gen((down|kjumpgt|kjumplt),pcl_genlabel(lab_e))
		else
			evalunit(pfrom)
			evalunit(pto)
			pcl_gen((down|kjumplt|kjumpgt),pcl_genlabel(lab_e))
		fi
		setmode_u(pindex)
	fi

	definefwdlabel(lab_b)

!need to generate assignment to local
	passign.resultflag:=0
	evalunit(passign)

	evalblock(pbody)				!do loop body

	definefwdlabel(lab_c)

	pcl_genx((down|kfordown|kforup),1, pcl_genlabel(lab_b))
	setmode_u(pindex)

	pcl_gen(kopnd, genmem_u(pindex))
	if pto.tag=j_const then
		pcl_gen(kopnd, pcl_genint(pto.value))
	else
		pcl_gen(kopnd, genmem_u(pto))
	fi

	if pelse then
		definefwdlabel(lab_e)
		evalblock(pelse)
	fi

	definefwdlabel(lab_d)
	--loopindex
end

proc do_swap(unit p,a,b) =
	evallv(a)
	evallv(b)
	pcl_gen(kswap)
	setmode_u(a)
end

proc do_convert(unit p,a) =
	int opc

	case p.tag
	when j_makelist, j_makeset then
	else
		case p.pclop
		when ksoftconv then
			gerror("CONV/SOFTCONV")
		when kerror then
			gerror("CONV/ERROR")
		else
			evalunit(a)
			pcl_gen(p.pclop)
		esac
		setmode_u(p)
		pcl_setoldtype(getpclmode(p.convmode))
	esac
end

proc do_dot(unit pdot) =
	int offset
	unit a,pname

	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)

	if offset then
		genpushint(offset)
		pcl_gen(kpushptroff)
	else
		pcl_gen(kpushptr)
	fi
	pcl_setscale(1)

	setmode(getmemmode_m(pdot))
end

global function checkdotchain(unit p, &pname)int=
!return accumulated offset of this and nested dot-expressions,
!or -1 when offsets cannot be combined
	int offset,axmode

	case p.tag
	when j_dot then
		offset:=checkdotchain(p.a,pname)
		return p.offset+offset

	else							!anything else, is the start expression
		pname:=p
		return 0
	esac
	return 0
end

proc do_dotref(unit pdot) =
	int imode:=createrefmode(nil,pdot.mode,0)
	int offset
	unit a,pname


	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)

	if offset then
		genpushint(offset)
		pcl_gen(kaddrefoff)
		pcl_setscale(1)
!		pccurrxx.scale:=1
	fi
	setmode(imode)
end

proc do_storedot(unit pdot,pfield, rhs) =
	int offset
	unit a,pname

	evalunit(rhs)
	a:=pdot.a

	if dodotchains then
		pname:=nil
		offset:=checkdotchain(a,pname)
		offset+:=pdot.offset
		a:=pname
	else
		offset:=pdot.offset
	fi

	evalref(a)
	genpushint(offset)

	pcl_gen((pdot.resultflag|kstoreptroff|kpopptroff))
	pcl_setscale(1)
	setmode_u(pdot)
end

proc do_index(unit p,parray,pindex) =
	int addoffset,scale
	if ttbasetype[p.mode] in [tarray,tblock] then
		do_indexref(parray,pindex)
		return
	fi
	addoffset:=getindexoffset(pindex)

	evalarray(parray)
	evalunit(pindex)
	pcl_gen(kpushptroff)
	setmode(getmemmode_m(p))

	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale + addoffset*scale)
end

proc do_storeindex(unit p,parray,pindex,rhs) =
	int addoffset, scale
	addoffset:=getindexoffset(pindex)

	evalunit(rhs)
	evalarray(parray)
	evalunit(pindex)

	pcl_gen((p.resultflag|kstoreptroff|kpopptroff))
	setmode_u(p.a)

	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

proc do_indexref(unit parray,pindex) =
	int addoffset,scale
	addoffset:=getindexoffset(pindex)

	evalarray(parray)
	evalunit(pindex)

	pcl_gen(kaddrefoff)
!
	setmode(tttarget[parray.mode])
	pcl_setscale(scale:=ttsize[tttarget[parray.mode]])
	pcl_setoffset(-ttlower[parray.mode]*scale+addoffset*scale)
end

function getindexoffset(unit &pindex)int offset=
!convert index like [i+3] to [i], returning the +3 etc as a separate offset
	int addoffset:=0

	if pindex.tag=j_bin and pindex.pclop in [kadd, ksub] then
		if pindex.b.tag=j_const then		!incorporate const offset into lwb adjustment
			addoffset:=(pindex.pclop=kadd|pindex.b.value|-pindex.b.value)
			pindex:=pindex.a
		fi
	fi
	return addoffset
end

proc do_switch(unit p,pindex,pwhenthen,pelse, int loopsw,isref) =
	const maxlabels = 1000
	int minlab,maxlab,n,iscomplex,i
	int lab_a,lab_b,lab_d, labjump, elselab, labstmt,ax,bx,ismult
	[0..maxlabels]ref pclrec labels
	unit w,wt

	ismult:=p.mode<>tvoid and not loopsw

	minlab:=1000000
	maxlab:=-1000000		!highest index seen

	n:=0				!no. different values
	iscomplex:=0			!whether complex switch

	wt:=pwhenthen
	while wt do
		w:=wt.a
		while w do		!for each when expression
			case w.tag
			when j_makerange then
				ax:=w.a.value
				bx:=w.b.value
	dorange::
				for i:=ax to bx do
					minlab := min(i,minlab)
					maxlab := max(i,maxlab)
				od
			when j_const then		!assume int
				ax:=bx:=w.value
				goto dorange
			else
				gerror_s("Switch when2: not const: #",strexpr(w).strptr)
			esac
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

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

	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then pcl_gen(kstartmult) fi

	evalunit(pindex)
	pcl_genxy(kswitch, minlab, maxlab,pcl_genlabel(labjump))
	pcl_gen(kopnd,pcl_genlabel(elselab))

	definefwdlabel(labjump)

	for i:=minlab to maxlab do			!fill with else labels first
		pcl_gen(kswitchlabel,pcl_genlabel(elselab))
		labels[i]:=pccurr
	od
	pcl_gen(kendswitch)

!scan when statements again, o/p statements

	wt:=pwhenthen
	while wt do
		labstmt:=definelabel()
		w:=wt.a
		while w do
			case w.tag
			when j_makerange then
				ax:=w.a.value
				bx:=w.b.value
			when j_const then
					ax:=bx:=int(w.value)
			esac
			for i:=ax to bx do
				labels[i].labelno:=labstmt
			od
			w:=w.nextunit
		od

		evalunitx(wt.b,isref)
		if ismult then pcl_gen(kresetmult) fi
		genjumpl((loopsw|lab_a|lab_d))
		wt:=wt.nextunit
	od

	definefwdlabel(elselab)
	if pelse then
		evalunitx(pelse,isref)
		if ismult then pcl_gen(kendmult) fi
	fi

	if loopsw then
		genjumpl(lab_a)
		definefwdlabel(lab_d)
		--loopindex
	else
		definefwdlabel(lab_d)
	fi
end

proc do_select(unit p,a,b,c, int isref) =
	const maxlabels=256
	[maxlabels]ref pclrec labels
	int labend,labjump,n,i,elselab,labstmt,ismult
	unit q

	ismult:=p.mode<>tvoid and p.resultflag

	q:=b
	n:=0
	while q do
		if n>=maxlabels then gerror("selectx: too many labels") fi
		++n
		q:=q.nextunit
	od

	labend:=createfwdlabel()
	labjump:=createfwdlabel()
	elselab:=createfwdlabel()

	if ismult then pcl_gen(kstartmult) fi
	evalunit(a)

	pcl_genxy(kswitch, 1, n, pcl_genlabel(labjump))
	pcl_gen(kopnd, pcl_genlabel(elselab))


	definefwdlabel(labjump)

	q:=b
	i:=0
	for i:=1 to n do
		pcl_gen(kswitchlabel,pcl_genlabel(elselab))
		labels[i]:=pccurr
	od
	pcl_gen(kendswitch)

	q:=b
	i:=0
	while q do
		labstmt:=definelabel()
		++i
		labels[i].labelno:=labstmt
		evalunitx(q,isref)
		if ismult then pcl_gen(kresetmult) fi
		genjumpl(labend)
		q:=q.nextunit
	od

	definefwdlabel(elselab)

	evalunitx(c,isref)
	if ismult then pcl_gen(kendmult) fi

	definefwdlabel(labend)
end

proc do_case(unit p,pindex,pwhenthen,pelse, int loopsw,isref) =
	const maxcase=256
	[maxcase]int labtable
	[maxcase]unit unittable
	int ncases, opc, ismult

	int lab_abc, lab_d, fmult, labnextwhen, labstmtstart, labelse
	unit w,wt

	if pindex=nil then
		GERROR("EMPTY CASE NOT DONE")
	fi

	ismult:=p.mode<>tvoid and not loopsw

	if loopsw then
		lab_abc:=definelabel()		!start of loop
		lab_d:=createfwdlabel()	!end of case/end of loop
		stacklooplabels(lab_abc,lab_abc,lab_abc,lab_d)
	else
		lab_d:=createfwdlabel()	!end of case/end of loop
	fi

	if ismult then pcl_gen(kstartmult) fi
	evalunit(pindex)

	if casedepth>=maxcasedepth then
		gerror("case nested too deeply")
	fi
	casestmt[++casedepth]:=p

	ncases:=0
	wt:=pwhenthen
	while wt do
		w:=wt.a
		if ncases>=maxcase then
			gerror("too many cases")
		fi
		labtable[++ncases]:=createfwdlabel()
		unittable[ncases]:=wt.b

		while w do
			evalunit(w)
			pcl_gen(kcasejumpeq, pcl_genlabel(w.whenlabel:=labtable[ncases]))
			setmode_u(w)
			w:=w.nextunit
		od

		wt:=wt.nextunit
	od

	pcl_gen(kpopstack)
	setmode_u(pindex)

	labelse:=createfwdlabel()
	caseelse[casedepth]:=labelse
	genjumpl(labelse)

	for i:=1 to ncases do
		definefwdlabel(labtable[i])
		evalunitx(unittable[i],isref)
		if ismult then pcl_gen(kresetmult) fi

		if loopsw then
			genjumpl(lab_abc)
		else
			genjumpl(lab_d)
		fi
	od

	definefwdlabel(labelse)

	if pelse then
		evalunitx(pelse,isref)
		if ismult then pcl_gen(kendmult) fi
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

proc do_dotindex(unit p,a,b) =
	evalunit(a)
	evalunit(b)

	pcl_gen(kdotindex)
	setmode(ti64)
end

proc do_dotslice(unit p,a,b) =
	evalunit(a)
	evalunit(b.a)
	evalunit(b.b)

	pcl_gen(kdotslice)
	setmode(ti64)
end

proc do_read(unit p,a) =
	int m

	m:=p.mode

	if a=nil then
		a:=pzero
	fi

	if ttisinteger[m] then
		pcl_gen_sysfn(sysfn_read_i64,a)
	elsif ttisreal[m] and ttsize[m]=8 then
		pcl_gen_sysfn(sysfn_read_r64,a)
	elsif m=trefchar then
		pcl_gen_sysfn(sysfn_read_str,a)
	else
CPL =STRMODE(M)
		GERROR("CAN'T READ THIS ITEM")
	fi
	setmode_u(p)
end

proc do_readln(unit a) =
	if a then
		if ttbasetype[a.mode]<>tref then gerror("@dev no ref") fi

		case ttbasetype[tttarget[a.mode]]
		when tvoid then
			pcl_gen_sysproc(sysfn_read_fileline, a)
		when tu8,tc8 then
			pcl_gen_sysproc(sysfn_read_strline, a)
		else
			gerror("rd@dev?")
		esac
	else
		pcl_gen_sysproc(sysfn_read_conline)
	fi
end

proc docond(int opc,unit p,int lab)=
	genjumpcond(opc,p,lab)
end

proc do_syscall(unit p,a)=
	case p.fnindex
	when sysfn_getnprocs then
		pcl_gen(kgetnprocs)
	when sysfn_getprocname then
		evalunit(a)
		pcl_gen(kgetprocname)
	when sysfn_getprocaddr then
		evalunit(a)
		pcl_gen(kgetprocaddr)
	esac
	setmode(ti64)
end

proc do_slice(unit a,b, int doref=0) =
!generate separate code for (ptr, length) parts

	IF DOREF THEN GERROR("DOSLICE/REF?") fi

	if b=nil then

		if a.tag=j_const then			!assume string
			genpushint(strlen(a.svalue))
		else
			genpushint(ttlength[a.mode])
		fi
		evalarray(a)

	else
!worth checking for const bounds? Both must be const to make it worthwhile
		if b.a.tag=b.b.tag=j_const then
			genpushint(b.b.value-b.a.value+1)
		else
			evalunit(b.b)
			evalunit(b.a)
			pcl_gen(ksub)
			setmode(ti64)
			genpushint(1)
			pcl_gen(kadd)
		fi
		setmode(ti64)

		do_indexref(a,b.a)
	fi

	pcl_gen(kmakeslice)
	setmode(tu128)
end

proc do_assignblock(unit p,a,b) =
!fstore=1 when result is needed
!method used is::
! load ref to lhs
! load ref to rhs
! do block xfer, not using the stack

	if b.tag=j_makelist then
		if ttbasetype[a.mode]=tarray then
			do_assignarray(a,b)
		else
			do_assignrecord(a,b)
		fi
	else
		GERROR("ASSIGN BLOCK")
	fi
end

proc do_assignarray(unit a,b)=
	unit passign, pindex, pconst,q
	int index

	if ttbasetype[tttarget[a.mode]]=tc8 then
		gerror("Assignment not suitable for []char type")
	fi

	pconst:=createconstunit(1,ti64)
	pindex:=createunit2(j_index,a,pconst)
	passign:=createunit2(j_assign,pindex, b.a)
	passign.mode:=pindex.mode:=tttarget[a.mode]

	index:=ttlower[a.mode]
	q:=b.a

	while q do
		pconst.value:=index
		pconst.resultflag:=1
		passign.b:=q
		evalunit(passign)

		++index
		q:=q.nextunit
	od

end

proc do_assignrecord(unit a,b)=
	unit passign, pdot, pfield,q
	int m,fieldtype
	symbol d,e

	pfield:=createunit0(j_name)
	pdot:=createunit2(j_dot,a,pfield)
	passign:=createunit2(j_assign,pdot, b.a)
	passign.mode:=pdot.mode:=tttarget[a.mode]

	m:=a.mode
	d:=ttnamedef[m]
	e:=d.deflist
	q:=b.a
	while e do
		if e.nameid=fieldid and e.mode<>tbitfield then
			fieldtype:=e.mode
			pfield.def:=e
			passign.mode:=pfield.mode:=pdot.mode:=fieldtype
			passign.b:=q
			pdot.offset:=e.offset
			evalunit(passign)
			q:=q.nextunit
		fi
		e:=e.nextdef
	od
end

proc pushrhs(unit a)=
	if a=nil then return fi
	pushrhs(a.nextunit)
	evalunit(a)
end

proc do_multassign(unit a,b)=
	unit p
	int nlhs,nrhs
	symbol d

	nlhs:=a.length

	if b.tag=j_callfn then
		evalunit(b)
		if b.a.tag<>j_name then
			gerror("multassign from fn: not simple fn")
		fi
		d:=b.a.def
		nrhs:=d.nretvalues

		if a.tag<>j_makelist then		!mult-ret fn assigned to scalar
			if a.nextunit then GERROR("MULTASS?") fi
			nlhs:=1
		else
			a:=a.a					!point to elements of makelist
		fi

	else
		nrhs:=b.length
		pushrhs(b.a)			!push rhs elements in right-to-left order
		a:=a.a					!point to elements of makelist

	fi

	repeat
		switch a.tag
		when j_name then
			pcl_gen(kpop,genmem_u(a))
		when j_index, j_slice,j_dot then
			evalref(a)
			pcl_gen(kpopptr,pcl_genint(0))
		when j_ptr then
			evalunit(a.a)
			pcl_gen(kpopptr,pcl_genint(0))
		when j_if, j_longif, j_case, j_switch, j_select then
			evalref(a)
			pcl_gen(kpopptr,pcl_genint(0))
		when j_dotindex then
			evalref(a.a)
			evalunit(a.b)
			pcl_gen(kpopdotindex)
		else
			cpl jtagnames[a.tag]
			gerror("Bad mult assign element")
		end switch

		setmode_u(a)

		a:=a.nextunit
	until a=nil

	d:=getprocretmodes(b)

	for i:=nlhs+1 to nrhs do
		pcl_gen(kpopstack)
		setmode(ttmult[d.mode,i])
	od
end

proc do_recase(unit p,a)=
	unit q,wt,w
	int destlab,casevalue

	if casedepth=0 then
		gerror("recase outside case stmt")
	fi

	if a then
		casevalue:=a.value
	else				!a=null means goto else
		genjumpl(caseelse[casedepth])
	fi

	q:=casestmt[casedepth]

	destlab:=0

	wt:=q.b
	while wt do
		w:=wt.a
		while w do
			if w.tag=j_const and ttisinteger[w.mode] and w.value=casevalue then
				destlab:=w.whenlabel
				exit all
			fi
			w:=w.nextunit
		od
		wt:=wt.nextunit
	od

	if destlab=0 then
		genjumpl(caseelse[casedepth])
	else
		genjumpl(destlab)
	fi
end

proc do_empty(unit p,a)=
	evallv(a)

	pcl_gen(kclear)

	setmode_u(a)
end

proc do_typeconst(unit p)=
	genpushint(p.value,ti64)
end

function condtopclop(int cond, baseop)int=
!turn keq etc into kjumpeq etc
!baseop is kjumpeq, kseteq, kselecteq
	return baseop+(cond-keq)
end
=== mm_decls.m 0 0 4/44 ===
global const maxmodule=200
global const maxsubprog=30
global const maxlibfile=50
global const maxsourcefile=1000

global type unit   = ref unitrec
global type symbol = ref strec

global macro pr(a,b)	= (a<<16 ior b)

global record tokenrec =		!should be 16-byte record
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
	symbol def
	ref procrec nextproc
end

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record typenamerec=
	symbol owner			!owner of scope where typename was encountered
							!moduleno required by resolvetypename can be derived from owner
!A/B used as follows
!  nil B			Simple typename B
!  A   B			Dotted pair A.B
!  A   nil          I think represents a typeof(x) where x is a name
	symbol defa
	union
		symbol defb
		symbol def
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
	union
		ref strec deflistx
!		ref pstrec pdef
		ref fwdrec fwdrefs	!fwd ref chain
	end
	ref strec nextdef
	ref strec nextdupl
	ref strec firstdupl			!point to generic version
	psymbol pstdef

	unit code			!var idata/proc body/taggedunion tag value/etc

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

	word32 pos: (lineno:24, fileno:8)
	word16 flags: (
		isglobal:2,
		isstatic:1,
		used:1,
		txdone:1,
		circflag:1,
		islet:1,

		iscallback:1,
		addrof:1,
		noreg:1,
		equals:1,
		isequivtarget:1,
		atfield:1,
		atvar:1,
		isimport:1)
	union
		byte moduleno
		byte subprogno
	end
	byte reg

	union
		struct				!when a proc
			ichar truename			!for imported name only
			ref strec paramlist

			byte asmused			!1 when proc contains asmcode
			byte dllindex			!for dllproc: which dll in dlltable
			byte extmodno			!for proc call chains: module no of proc owner
			byte fflang				!0 windowsff. etc.
			byte nretvalues			!function: number of return values (0 for proc)
			byte varparams			!0 or 1; variadic params in B and FF
			byte isthreaded			!0 or 1; variadic params in B and FF
		end

		struct				!when a record or record field
			ref strec equivfield
			uflagsrec uflags
			int32 baseclass
			byte bitfieldwidth		!width of bitfield in record
			byte align				!0, 2, 4, 8, 16 or 255 (auto-align)
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
!			symbol equivvar
!			byte equals				!0 or 1 if @ used (static/frame vars only)
		end

		struct						!macro param
			ref void macro_dummy	!needs nextparam
			ref strec nulldef		!generic st entry
		end

		struct						!when a tagged union
			int32 enumtagmode
		end


!		byte subprogno
		[24]byte dummy
	end

	int16 nrefs
!	int16 pmode

	union
		int16 stindex		!label pass 2: 0, or 1-based index within coff symboltable
		int16 maxalign		!for record types (doesn't fit above)
		int16 regsize
	end
	byte reftype			!AX fields
	byte segment
	int16 importindex
!	byte subprogno
!	[2]BYTE PADDING

end

global record unitrec =
	byte tag				!kcode tag number
	byte hasa, hasb, hasc	!whether .a, .b or .c points to a unit/unitlist
!	int32 lineno			!source lineno associated with item; fileno is in top byte
	word32 pos: (lineno:24, fileno:8)

	unit nextunit

	union
		struct
			union
				unit			a
				symbol		def
				symbol		labeldef
				int64			value
				word64			uvalue
				real64			xvalue
				ichar			svalue
				int64			range_lower
			end

			union
				unit			b
				int64			range_upper
			end
		end
		int128					value128
		word128					uvalue128
		struct
			word64				low128
			word64				high128
		end
	end

	union
		unit			c
		[4]int16		cmppclmode
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

		struct
			union
				int16 bitopindex	!
				int16 opcindex		!operator nodes
				int16 fnindex		!sysfn_add_var etc
				int16 condcode		!pcl_eq etc; for j_eq etc
				int16 asmopcode		!for j_assem
				int16 bfcode
			end
		end
		int32 index
		[4]byte cmpgenop			!cmpchain: up to 8 genops
	end

	int32 mode
	union
		int32 convmode	!convert/typepun: target mode (will be widened to give unit mode)
		int32 memmode	!name/ptr/index/dot: void=LVALUE; non-void=RVALUE
	end
	byte moduleno
	byte initlet		!1 for an assignment that initialises a let
	byte isconst		!1 for j_const, and j_makerange with const range
	byte resultflag		!1 when the result of this unit is needed; 0=void or discarded
	byte pclop			!generic operator for j_bin, incr etc
	byte pclmode		!eg kadd:i64

	byte istrueconst	!1 for actual "123" etc, not result of reduction
	byte spare
end

global record modulerec =
	ichar name
	symbol stmodule
	symbol stsubprog
	ichar path			!path where module source file resides
	symbol stinitproc
	symbol stmacro		!will be turned into a macro
	int16 fileno		!sourcefile table index once loaded
	int16 issyslib		!1 if system lib (different search rules)
	int16 subprogno
end

global record subprogrec =
	ichar name
	symbol stsubprog
	int issyslib		!1 if system lib (different search rules)
!	int loccode			!'INT' 'SS' 'DISK'; file system where located (for 'import')
	ichar path			!path where import module source file resides
	int16 startmodule
	int16 endmodule
	int fileno
end

!global const maxsearchdirs=10
!global [maxsearchdirs]ichar searchdirs
!global int nsearchdirs=0

global symbol stprogram		!root into the symbol table
global symbol stmodule		!main module
global symbol stsubprog
global symbol stsysmodule	!optional sys module (needed for name resolving)
global symbol alldeflist		!link together all (user) symbols

global tokenrec lx				!provides access to current token data
global tokenrec nextlx			!provides access to next token

global [0..maxmodule]modulerec moduletable
global [0..maxmodule]byte moduletosub				!convert module no to subprog no
global [0..maxsubprog]subprogrec subprogtable

global [0..maxlibfile]ichar libfiles
global [0..maxlibfile]byte libtypes

global [0..maxsourcefile]ichar sourcefilespecs		!full path
global [0..maxsourcefile]ichar sourcefilepaths		!path only
global [0..maxsourcefile]ichar sourcefilenames		!base filename only
global [0..maxsourcefile]ichar sourcefiletext		!text
global [0..maxsourcefile]ichar sourcefiledupl		!copy for ma file only
global [0..maxsourcefile]int sourcefilesizes
global [0..maxsourcefile]byte issupportfile
global [0..maxsourcefile]byte issyslibfile
global int nmodules
global int nsubprogs
global int nsourcefiles
global int nlibfiles

!!.ma file directory
!global [0..maxsourcefile]ichar mafilenames
!global [0..maxsourcefile]int mafilesizes
!global [0..maxsourcefile]int mafileoffsets
!global [0..maxsourcefile]ichar mafiletext
!global [0..maxsourcefile]byte mafilefileno			!0 or index into sourcefile tables
!global [0..maxsourcefile]byte mafilesupport			!1 means support file eg. for strinclude
!global int nmafiles
!global ichar mafilesource
!
global symbol currmodule
global int currmoduleno				!used when compiling modules

global const int maxtype=6'000

global int ntypes

global [0..maxtype]symbol		ttnamedef
global [0..maxtype]symbol		ttowner			!for ttlowerexpr/rtlengthexpr

global [0..maxtype]int32		ttbasetype		!basetype
global [0..maxtype]ichar		ttname

global [0..maxtype]int32		ttsize
global [0..maxtype]byte			ttsizeset
global [0..maxtype]int32		ttlower 		!.lbound (default 1)
global [0..maxtype]int32		ttlength 		!elements in array/record/tuple
global [0..maxtype]ref[]int32	ttmult 			!ttlength elements in tuple

global [0..maxtype]unit			ttdimexpr		!length, lower:length, or lower..upper

global [0..maxtype]int32		tttarget 		!for array/ref types
global [0..maxtype]int32		ttkeytype 		!for dict
global [0..maxtype]byte			ttusercat
global [0..maxtype]int32		ttlineno

global [0..maxtype]byte			ttisint			!is i8 i16 i32 i64 i128
global [0..maxtype]byte			ttisword		!is u8 u16 u32 u64 u128
global [0..maxtype]byte			ttisreal		!is r32 r64
global [0..maxtype]byte			ttisinteger		!is i8..i64/u8..u64/c8..c64
global [0..maxtype]byte			ttisallnum		!all numeric types including short/decimal
global [0..maxtype]byte			ttismainnum		!all numerics excl short
global [0..maxtype]byte			ttisshort		!is i8/i16/i32/u8/u16/u32/c8/c16
global [0..maxtype]byte			ttisref			!is a pointer

!global const int maxtypename=4'000
!global const int maxtypename=8'000
global const int maxtypename=12'000
global [0..maxtypename]typenamerec typenames
global [0..maxtypename]posrec typenamepos
global int ntypenames

global [0..symbolnames.upb]byte typestarterset

global symbol currproc
global symbol currsubprog

global int alineno=0

global int debug=0
global int assemmode=0
global int headermode=0

global ref procrec proclist,proclistx			!linked list of all procs
global ref procrec staticlist,staticlistx		!linked list of all static
global ref procrec constlist,constlistx		!linked list of all export consts

!global const maxmodulemap=25
!global [maxmodulemap]ichar genericmodules
!global [maxmodulemap]ichar actualmodules
!global int nmodulemap

global unit nullunit

global int targetbits=64
global int targetsize=8

global [20]ichar docstrings
global int ndocstrings

!global const maxdlllib=51
global const maxdllproc=1000

global int ndllproctable
global [maxdllproc]dllprocrec dllproctable

global int fverbose=1		!1=normal, 0=less verbose, 2/3 = more verbose

global int msyslevel=2		!0/1/2 = none/min/normal
global byte fvarnames=0		!display of names in asm/mcl

global byte fbundled=0		!1 when .ma file is being compiler
global byte fwritema
global byte fwriteexports
global byte fwritedocs

global byte fexe
global byte fobj
global byte fwritelibs
global byte fshowtiming
global byte fshowss
global byte fshowmx
global byte fshowpcl
global byte fshowasm
global byte fshowast1
global byte fshowast2
global byte fshowast3
global byte fshowst
global byte fshowstflat
global byte fshowtypes
global byte fshowoverloads
global byte fshowmodules
global byte foptim
global byte fcheckunusedlocals=0
global byte fwindows
global byte flinux
global byte fx64
global byte fssonly
global byte fnofile
global byte fdorts=1

global byte dointlibs=1

!pcl/mcl-pass are relevant only for x64 target, and not allowed for 
global tabledata() []ichar passnames =
	(header_pass,	$),
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
	(lib_pass,		$),		!
	(run_pass,		$),		!will do up to .exe then run the .exe
	(clang_pass,	$),
end

!passlevel used for compiler debug only
global int passlevel=0
global int prodmode=0
global int debugmode=0
!global int mainlib=0					!1 means main app lib (so export $cmdskip)
global int libmode=0					!1 means eventual ML/LIB target

global ichar outfile					!one of the following two
global ichar destfilename				!nil, or sets outfile
global ichar destfilepath				!nil, or sets path (can't be mixed with destfilename)
global ichar asmfilename				!set from outfile
global ichar pclfilename				!
global ichar exefilename				!
global ichar libfilename				!
global ichar objfilename				!
global ichar mafilename					!
global ichar expfilename				!

global symbol extendtypelist

global [0:jtagnames.len]ref overloadrec overloadtable

global int nunits

=== mm_diags.m 0 0 5/44 ===
int currlineno
int currfileno

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

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

gs_leftstr(d,p.name,28-offset,'-')
gs_leftstr(d,namenames[p.nameid],12,'.')

col:=gs_getcol(d)
dd:=p^


gs_str(d,"[")
if p.isimport then
	gs_str(d,"Imp ")
else
	gs_str(d,(p.isglobal|"Global ","Exp ", "ExpQ "|"Loc "))
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
	gs_str(d," maxalign:")
	gs_strint(d,dd.maxalign)
	gs_str(d," ")
fi
if dd.optional then
	gs_str(d,"Opt ")
fi
if dd.varparams then
	gs_str(d,"Var:")
	gs_strint(d,dd.varparams)
	gs_str(d," ")
fi

if dd.moduleno then
	if dd.nameid<>subprogid then
		print @&.str,"Modno#",,dd.moduleno
	else
		print @&.str,"Subno#",,dd.subprogno
	fi
	gs_str(d,&.str)
fi

if dd.used then
	gs_str(d,"U ")
fi

if dd.isthreaded then
	gs_str(d,"Threaded ")
fi


gs_str(d,"]")
gs_padto(d,col+10,'=')

if p.owner then
	fprint @&.str,"(#)",p.owner.name
	gs_leftstr(d,&.str,18,'-')
else
	gs_leftstr(d,"()",18,'-')
fi

case p.mode
when tvoid then
	gs_str(d,"Void ")
else
	GS_STRINT(D,P.MODE)
	GS_STR(D,":")

	gs_str(d,strmode(p.mode))
	gs_str(d," ")
esac

case p.nameid
when fieldid,paramid then
	gs_str(d," Offset:")
	gs_strint(d,p.offset)
	if p.mode=tbitfield then
		gs_str(d," Bitoffset:")
		gs_strint(d,p.bitoffset)
		gs_str(d,":")
		gs_strint(d,p.bitfieldwidth)
	fi

!	sprintf(&.str,"%.*s",int(p.uflags.ulength),&p.uflags.codes)
	print @&.str,p.uflags.ulength:"v",ichar(&p.uflags.codes):".*"
	gs_str(d," UFLAGS:")
	gs_str(d,&.str)
	gs_str(d,"-")
	gs_strint(d,p.uflags.ulength)

	if p.code then
		gs_str(d,"/:=")
		gs_strvar(d,strexpr(p.code))
	fi

	if p.nameid=paramid and p.variadic then
		gs_str(d,"...")
	fi
when genfieldid then
	gs_str(d,"Index:")
	gs_strint(d,p.offset)

when procid,genprocid then

	gs_str(d,"Index:")
	gs_strint(d,p.index)

	gs_str(d," Nret:")
	gs_strint(d,p.nretvalues)

when dllprocid then
	gs_str(d,"Index/PCaddr:")
	gs_strint(d,p.index)
	if p.truename then
		gs_str(d," Truename:")
		gs_str(d,p.truename)
	fi

when staticid then
	if p.code then
		gs_str(d,"=")
		gs_strvar(d,strexpr(p.code))
	fi

when frameid then
	if p.code then
		gs_str(d,":=")
		gs_strvar(d,strexpr(p.code))
	fi

when constid then
	gs_str(d,"Const:")
	gs_strvar(d,strexpr(p.code))

when typeid then
	if p.baseclass then
		gs_str(d,"Baseclass:")
		GS_STR(D,"<HAS BASECLASS>")
	fi
when enumid then
	gs_str(d,"Enum:")
	gs_strint(d,p.index)
when dllmoduleid then
	gs_str(d,"DLL#:")
	gs_strint(d,p.dllindex)
esac

if p.atfield then
	gs_str(d," @")
	gs_str(d,p.equivfield.name)
fi
if p.atvar then
	gs_strvar(d,strexpr(p.equivvar))
fi

!gs_str(d," Module# ")
!gs_strint(d,p.moduleno)
!
gs_str(d," Lineno:")
gs_strint(d,p.lineno iand 16777215)

gs_println(d,f)

case p.nameid
when constid,frameid,staticid,macroid then
	if p.code then
		printunit(p.code,dev:f)
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
	if p.name then
		case p.symbol
		when namesym then
			println @f,i,p,":",p.name,symbolnames[p.symbol],namenames[p.nameid]
!			if p.symbol=lexmacronamesym then
!!				lx:=p.macrotoken
!				println @f,"			",p.macrovalue
!			fi
			p:=p.nextdupl
			while p do
				println @f,"	",p,p.name,symbolnames[p.symbol],namenames[p.nameid],
					"(From",(p.owner|p.owner.name|"-"),,")"
				p:=p.nextdupl
			od
		esac
	fi
od
end

global proc printcode(filehandle f,ichar caption)=
ref strec p
ref procrec pp

pp:=proclist

while pp do
	p:=pp.def

	print @f,p.name,,"=",(p.isglobal|"Global","Export","ExportQ"|"Local")
	if p.owner.nameid=typeid then
		print @f," in record",p.owner.name
	fi
	println @f
	printunit(p.code,,"1",dev:f)
	println @f
	pp:=pp.nextproc
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
static int cmpchain=0

if p=nil then
	return
fi

if p.lineno then
	currlineno:=p.lineno
	currfileno:=p.fileno
fi

print @dev,p,":"
print @dev,getprefix(level,prefix,p)

idname:=jtagnames[p.tag]+2
print @dev,idname,,": "

case p.tag
when j_name then
	d:=p.def

	print @dev,d.name,namenames[d.nameid]

	if d.code then
		print @dev," {",,jtagnames[d.code.tag],,"}"
	fi

	print @dev," ",,getdottedname(d)!,q
	print @dev,(p.dottedname|" {Dotted}"|"")

	if p.c then
		print @dev," Lastcall:",p.c
	fi

	if p.addroffirst then
		print @dev," Addroffirst."
	fi

	print @dev," Moduleno:",p.moduleno

	if p.avcode then print @dev," AV:",char(p.avcode) fi

PRINT @DEV,=P.INDEX


when j_labeldef then
	println @dev,p.def.name

when j_const then
	t:=p.mode
	a:=p.value
	if t=trefchar then
		if p.slength>256 then
			print @dev,"""",,"(LONGSTR)",""" *",,p.slength
		elsif p.slength then
			print @dev,"""",,p.svalue,,""" *",,p.slength
		else
			print @dev,""""""
		fi

	elsecase ttbasetype[t]
	when ti64,ti32,ti16,ti8 then print @dev,int64(a)
	when tu64,tu32,tu16,tu8 then print @dev,word64(a)
	when tc64,tc8,tc16 then print @dev,chr(a)

!	when ti32,ti64,ti8,ti16 then
!		print @dev,p.value
!	when tu32,tu64,tu8,tu16 then
!		print @dev,p.uvalue
	when tr32 then
		x32:=p.xvalue
		print @dev,real64(x32)
	when tr64 then
		print @dev,p.xvalue
	when tref then
		if p.value then
			print @dev,"#",,p.value,P.SLENGTH
		else
			print @dev,"NIL"
		fi
	when ti128 then
		print @dev,p.value128
	when tu128 then
		print @dev,p.uvalue128
	else
		println =typename(t),typename(ttbasetype[t])
		PRINT @DEV,"<PRINTUNIT BAD CONST PROBABLY VOID"
	fi
	print @dev," ",,typename(t)
	if p.isastring then
		print @dev," <isstr>"
	fi

	if p.whenlabel then
		print @dev," *L",,p.whenlabel
	fi

when j_decimal then
	print @dev,p.svalue,"Len:",p.slength

when j_typeconst then
	print @dev,typename(p.mode),typename(p.value)

!when j_operator then
!	print @dev,jtagnames[p.opcode]+2

when j_bitfield then
	print @dev,bitfieldnames[p.bfcode]+3

when j_convert,j_typepun then
!	print @dev,pclnames[p.pclop]," Convmode:",strmode(p.convmode)
	print @dev," Convmode:",strmode(p.convmode)

when j_makelist,j_multexpr then
	print @dev,"Len:",p.length," Makeax:",p.makearray

when j_dot then
	print @dev,"Offset:",p.offset

when j_index, j_ptr then

when j_exit,j_redo,j_restart,j_next then
	print @dev,"#",,p.index

when j_syscall then
	print @dev,sysfnnames[p.fnindex]+6

when j_assem then
!	print @dev,mclnames[p.index]+2
!	if p.index in [m_jmpcc, m_setcc, m_cmovcc] then
!		print @dev," ",condnames[p.cond],=P.COND
!	fi

when j_assemreg then
!	print @dev,regnames[p.reg],"size:",p.regsize

when j_assemxreg then
!	print @dev,xmmregnames[p.reg]

when j_assemmem then
!	ichar plus
!	plus:=""
!	if p.prefixmode then print @dev,strmode(p.prefixmode) fi
!	print @dev,"["
!	if p.reg then 
!		print @dev,regnames[p.reg]
!		plus:="+"
!	fi
!	if p.regix then 
!		print @dev,plus,,regnames[p.regix]
!!		plus:="+"
!	fi
!	if p.scale>1 then
!		print @dev,"*",,p.scale
!	fi
!	print @dev,"]"
!
!when j_unary, j_bin, j_unaryto, j_binto, j_cmp then
!	if p.opindex then
!		print @dev,pclnames[p.opindex]
!	else
!		print @dev,pclnames[p.pclop]
!	fi

when j_makeset then
!	if p.isconst then
!		print @dev,p.range_lower,,"..",,p.range_upper
!	fi
when j_cmpchain then
	for i to p.cmpgenop.len do
		if p.cmpgenop[i]=0 then exit fi
		print @dev,pclnames[p.cmpgenop[i]],," "
	od
esac

case p.tag
when j_name, j_ptr, j_index, j_dot,j_callproc, j_callfn, j_assign then
	if p.memmode=tvoid then
!		print @dev," LVALUE"
	else
		print @dev," WIDEN FROM:",strmode(p.memmode)
	fi
esac

if p.isconst then
	print @dev," Is const"
fi

case p.tag
when j_bin, j_binto, j_unary, j_unaryto, j_cmp, j_incr, j_convert,
	j_andl, j_orl, j_notl, j_istruel then
	if p.pclop then
		fprint @dev," Pcl<#:#>",pclnames[p.pclop],pstdnames[p.pclmode]
	else
		fprint @dev," no-op"
	fi
esac


println @dev

if p.hasa then printunitlist(dev,p.a,level+1,"1") fi
if p.hasb then printunitlist(dev,p.b,level+1,"2") fi
if p.hasc then printunitlist(dev,p.c,level+1,"3") fi
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
if p=nil then return fi

while p do
	printunit(p,level,prefix,dev)
	p:=p.nextunit
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

fprint @&.modestr,"# #:#",isexpr,(p.resultflag|"RES"|"---"),strmode(p.mode)
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

fprint @&.str,"# # ",CURRFILENO:"Z2",currlineno:"z4"
return &.str
end

global proc printmodelist(filehandle f)=		!PRINTMODELIST
int mbase
static ichar tab="\t"

println @f,"MODELIST",ntypes

for m:=0 to ntypes do
!for m:=tlast to ntypes do
	println @f,m:"4",strmode(m)
	mbase:=ttbasetype[m]

	println @f,tab,"Basetype:",mbase,strmode(mbase)
	println @f,tab,"ttname:",ttname[m]
	println @f,tab,"ttnamedef:",ttnamedef[m],(ttnamedef[m]|ttnamedef[m].name|"-")
	println @f,tab,"Target:",strmode(tttarget[m])
	println @f,tab,"Code:",stdcodes[mbase]:"c"
	println @f,tab,"Size:",ttsize[m],"Sizeset",ttsizeset[m]
	fprintln @f,"# Bounds: #..#  Length:#",tab,ttlower[m],ttlower[m]+ttlength[m]-1,ttlength[m]
	if mbase=ttuple then
		print @f,tab,"Mult:"
		for i to ttlength[m] do print @f,strmode(ttmult[m,i]),," " od
		println @f
	fi
	println @f,tab,"Keytype:",strmode(ttkeytype[m])
	println @f,tab,"Isint:",ttisint[m]
	println @f,tab,"Isword:",ttisword[m]
	println @f,tab,"Isreal:",ttisreal[m]
	println @f,tab,"Isinteger:",ttisinteger[m]
	println @f,tab,"Isallnum:",ttisallnum[m]
	println @f,tab,"Ismainnum:",ttismainnum[m]
	println @f,tab,"Isshort:",ttisshort[m]
	println @f,tab,"Isref:",ttisref[m]
	println @f
od

end

=== mm_export.m 0 0 6/44 ===
strbuffer sbuffer
ref strbuffer dest=&sbuffer

const expscope=export_scope

global proc writeexports(ichar outfile, modulename)=
	ref strec d,e
	ref procrec pp
	[300]char filename
	filehandle f

	println "Writing exports file to",outfile

	gs_init(dest)
!	wxstr("importlib $")
	wxstr("importlib ")
	wxstr(modulename)
	wxstrln(" =")

	for i:=tuser to ntypes do
		d:=ttnamedef[i]
!		if d.isglobal=expscope and d.name^<>'$' then
		if isexported(d) and d.name<>'$' then

			case ttbasetype[i]
			when trecord then
				exportrecord(d)
			when tenum then
				exportenum(d)
			else
				wxstr("    type ")
				wxstr(d.name)
				wxstr(" = ")
				wxstr(strmode(d.mode,0))
				wxline()
			esac
		fi
	od

	pp:=staticlist
	while pp, pp:=pp.nextproc do
!	while pp do
		d:=pp.def
!		if d.isglobal=expscope then
		if isexported(d) then
			exportstatic(d)
		fi
!		pp:=pp.nextproc
	od
	if staticlist then wxline() fi

	pp:=constlist
	while pp, pp:=pp.nextproc do
		d:=pp.def
		if isexported(d) then
			exportconst(d)
		fi
	od
	if constlist then wxline() fi

	pp:=proclist
	while pp, pp:=pp.nextproc do
!	while pp do
		d:=pp.def
!		if d.isglobal=expscope then
		if isexported(d) then
			exportproc(d)
		fi
!		pp:=pp.nextproc
	od

	wxstrln("end importlib")

	f:=fopen(outfile,"wb")
	gs_println(dest,f)
	fclose(f)
end

function isexported(symbol d)int=
!return 1 when d is has exportscope and is in the main subprogram
	return d.isglobal=expscope and moduletosub[d.moduleno]=1
!	return d.isglobal=expscope and d=nil

end

proc exportstatic(ref strec d)=
	wxstr("    var ")
	wxmode(d.mode)
	wxstr(" ")
	wxstr(d.name)
	wxline()
end

proc exportconst(ref strec d)=
	wxstr("    const ")
	wxmode(d.mode)
	wxstr(" ")
	wxstr(d.name)
	wxstr(" = ")
	jeval(dest,d.code)
	wxline()
end

proc exportproc(ref strec d)=
	ref strec e
	int currmode,needcomma

	wxstr("    mlang ")
	wxstr((d.mode=tvoid|"proc     "|"function "))
	wxstr(d.name)
	wxstr("(")

	e:=d.deflist
	needcomma:=0
	currmode:=tvoid

	while e do
		if e.nameid=paramid then
			if needcomma then wxstr(",") fi
			if e.parammode<>out_param then
				if e.mode<>currmode then
					wxmode(e.mode)
					wxstr(" ")
					currmode:=e.mode
				fi
			else
				wxmode(tttarget[e.mode])
				wxstr(" &")
				currmode:=tvoid
			fi
			wxstr(e.name)
			if e.code then
				wxstr("=")
				if ttisref[e.mode] and e.code.tag=j_const and e.code.value=0 then
					wxstr("nil")
				else
					jeval(dest,e.code)
				fi
			fi
			needcomma:=1
		fi
		e:=e.nextdef
	od

	wxstr(")")
	if d.mode then
		wxstr(" => ")
		wxmode(d.mode)
	fi
	wxline()
end

proc exportenum(ref strec d)=
	ref strec e
	wxstr("    type ")
	wxstr(d.name)
	wxstr(" = enum(")

	e:=d.deflist
	while e do
		wxstr(e.name)
		wxstr("=")
		jeval(dest,e.code)
		e:=e.nextdef
		if e then
			wxstr(", ")
		fi
	od

	wxstrln(")")
end

proc wxstr(ichar s)=
	gs_str(dest,s)
end

proc wxstrln(ichar s)=
	gs_strln(dest,s)
end

proc wxline=
	gs_line(dest)
end

proc exportrecord(ref strec d)=
	ref strec e
	ref char flags
	int flag,indent
	const tab="    "

	e:=d.deflist

	wxstr("    record ")
	wxstr(d.name)
	wxstr(" = ")
	wxline()

	indent:=2

	while e do
		if e.nameid=fieldid then
			flags:=cast(&e.uflags)
			docase flags^
			when 'S' then
				to indent do wxstr(tab) od
				wxstrln("struct")
				++indent
				++flags
			when 'U' then
				to indent do wxstr(tab) od
				wxstrln("union")
				++indent
				++flags
			else
				exit
			end docase

			to indent do wxstr(tab) od
			wxmode(e.mode)
			wxstr(" ")
			wxstrln(e.name)

			do
				flag:=flags++^
				case flag
				when '*'  then
				when 'E' then
					--indent
					to indent do wxstr(tab) od
					wxstrln("end")
				else
					exit
				esac
			od
		fi

		e:=e.nextdef
	od

	wxstrln("    end")
	wxline()
end

proc wxmode(int mode)=
	ichar name
	if mode>=tuser then
		name:=ttnamedef[mode].name
		if name^<>'$' then
			wxstr(name)
			return
		fi
	fi
	wxstr(strmode(mode,0))
end
=== mm_genpcl.m 0 0 7/44 ===

const entrypointname = "start"
const entrypointname2 = "main"

global int retindex
global int initstaticsindex

global int framebytes, parambytes
global int retpending					!1 means not set
global pcl pclastframe
global int pcltempindex

global symbol pclcurrproc

const maxnestedloops	= 50

global [maxnestedloops,4]int loopstack
global int loopindex							!current level of nested loop/switch blocks

unitrec zero_unit
global unit pzero=&zero_unit

global function codegen_pcl(int rts):int=
!generate code for module n
	symbol d,e
	ref procrec pp

	pcl_start(nunits)

	for i to nlibfiles when libfiles[i]^<>'$' do
		pcl_gen((libtypes[i]='D'|kimportdll|kimportlib), pcl_genname(libfiles[i]))
	od
	pcl_gencomment("")
!	PCL_GEN(KIMPORTDLL, pcl_genname("abc"))

	SCANSYMBOL(STPROGRAM)

	pp:=staticlist
	while pp do
		d:=pp.def
		dostaticvar(d)
		pp:=pp.nextproc
	od

	pcl_gencomment("")

	pp:=proclist
	while pp do
		d:=pp.def
		genprocdef(currproc:=d)
		pp:=pp.nextproc
	od

	pcl_endprog(0,dorts:rts)

	return 1
end

proc genprocdef (symbol p) =	!GENPROCDEF
	[1256]char str
	[1256]char name
	int paramoffset,nparams,retaddrbytes
	symbol d
	int n,lab,np,offset,reg,i,xreg,isstart,structret,isfloat,hasparams,hasequiv
	unit q
	pcl pcproc
	pcl oldpccode,oldpccurr, pp,qq

	d:=p.deflist

	isstart:=0
	if p.isglobal and eqstring(p.name,entrypointname) then
		isstart:=libmode+1				!isstart = 1/2 means start-mx/start-ml
!CPL "ENTRY POINT FOUND",P.NAME
		pcl_setentrypoint(getpst(p),libmode)
		p.index:=plabelno
	fi

	retpending:=1

	MLINENO:=P.POS
	pcl_gen((p.isthreaded|kthreadedproc|kprocdef),genmem_d(p))
	setmode(p.mode)
	if p.isglobal=export_scope and moduletosub[p.moduleno]=1 then
!CPL "EXPORTING",P.NAME, P.MODULENO,MODULETOSUB[P.MODULENO]
!OS_GETCH()
		pcl_setexported(1)
	fi

	if p.name^='$' then
		pcl_setrtsproc()
	fi

	pcproc:=pccurr
	pclastframe:=pcproc
	pcltempindex:=0

	d:=p.deflist
	while d do
		MLINENO:=D.POS
		case d.nameid
		when frameid then
			if not d.atvar then
				pcl_gen(klocal,genmem_d(d))
				setmode(d.mode)
				pclastframe:=pccurr
			fi
		when paramid then
			pcl_gen(kparam,genmem_d(d))
			setmode(d.mode)
		esac
		d:=d.nextdef
	od

	genprocentry(isstart)


	if isstart=1 and msyslevel=2 then		!no m$init for libs

		pcl_gen_sysproc(sysfn_init)
	fi

	retindex:=lab:=createfwdlabel()

	if isstart then
		for i:=nmodules downto 1 do
			d:=moduletable[i].stinitproc
			if d then
				pcl_gen(ksetargs)
				pcl_gen(kcallproc, genmemaddr_d(d))
				setmode(ti64)
			fi
		od
	fi

!	pcl_gencomment("-------------------------------------------------")
	evalunit(p.code)
!	pcl_gencomment("-------------------------------------------------")

	definefwdlabel(retindex)

	if isstart=1 then
		pcl_gen(kpush,pcl_genint(0))
		pcl_gen(kstop)
	fi
	genreturn()

	if p.mode<>tvoid then
		if not checkblockreturn(p.code) then
			gerror_s("Function needs explicit return: ",p.name)
		fi
	fi

	pcl_gen(kendproc)

	pcl_gencomment("")
end

proc genprocentry(int isstart)=
!proc entry code

	pcl_gen(kprocentry)
end

proc dostaticvar(symbol d)=
	unit p
	int expflag

	if d.isimport then return fi

	expflag:=d.isglobal=export_scope and
		(moduletosub[d.moduleno]=1 or (d.name^='$' and not libmode))
!	expflag:=d.isglobal=export_scope
!
!IF EXPFLAG THEN
!	CPL "EXPORTING",D.NAME,=MAINLIB
!FI

	if d.atvar=1 then
		return
	elsif d.code then
		pcl_gen(kistatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
		pcl_setexported(expflag)
		genidata(d.code)
	else
dozstatic::
		pcl_gen(kzstatic,genmem_d(d))
		setmode(d.mode)
		pcl_setalign(getalignment(d.mode))
		pcl_setexported(expflag)
	fi

end

proc genidata(unit p,int doterm=1, am='A',offset=0)=
	int t,length,n,i,j,nwords,offset1,offset2,size,padding,isunion,tbase
	unit q,a,b
	symbol d
	real32 sx

	t:=p.mode
	mlineno:=p.lineno
	tbase:=ttbasetype[t]

	case p.tag
	when j_const then
		if ttisref[p.mode] then
			if p.mode=trefchar then
				if p.svalue then
					pcl_gen(kdq,pcl_genstring(p.svalue))
				else
					pcl_gen(kdq,pcl_genint(0))
				fi
			else
				pcl_gen(kdq,pcl_genint(p.value))
			fi
		elsif ttisreal[p.mode] then
			case ttsize[p.mode]
			when 4 then
				pcl_gen(kdd,pcl_genreal32(p.xvalue))
			when 8 then
				pcl_gen(kdq,pcl_genreal(p.xvalue))
			else
				gerror_s("IDATA/REAL:",strmode(p.mode),p)
			esac

		else						!assume int/word
			case ttsize[getmemmode_m(p)]
			when 1 then
				pcl_gen(kdb,pcl_genint(p.value))
			when 2 then
				pcl_gen(kdw,pcl_genint(p.value))
			when 4 then
				pcl_gen(kdd,pcl_genint(p.value))
			when 8 then
				pcl_gen(kdq,pcl_genint(p.value))
			when 16 then
				pcl_gen(kdq,pcl_genint(p.range_lower))
				pcl_gen(kdq,pcl_genint(p.range_upper))
			else
				gerror_s("IDATA/INT:",strmode(p.mode),p)
			esac

		fi

	when j_makelist then
		q:=p.a
		while q do
			genidata(q)
			q:=q.nextunit
		od

	when j_name then
		d:=p.def
		case d.nameid
		when staticid,procid,dllprocid then
			pcl_gen((am='P' or ttsize[p.mode]=8|kdq|kdd), genmemaddr_d(d))
			if offset then
				pcl_setscale(1)
				pcl_setoffset(offset)
			fi
		else
			gerror("Idata &frameXXX")
		esac
		return
	when j_convert then
		genidata(p.a)
	when j_shorten then
		a:=p.a
		case ttsize[p.mode]
		when 1 then
			pcl_gen(kdb,pcl_genint(a.value))
		when 2 then
			pcl_gen(kdw,pcl_genint(a.value))
		when 4 then
			pcl_gen(kdd,pcl_genint(a.value))
!	when 8 then
!		pcl_gen(kdq,pcl_genint(a.value))
!	when 16 then
!		pcl_gen(kdq,pcl_genint(a.range_lower))
!		pcl_gen(kdq,pcl_genint(a.range_upper))
		else
			gerror_s("IDATA/SHORTEN:",strmode(p.mode),p)
		esac

	when j_addrof,j_addroffirst then
		genidata(p.a,am:'P',offset:(p.b|p.b.value|0))
	else
		gerror_s("IDATA: ",jtagnames[p.tag],p)

	esac
end

global function genmem_u(unit p)pcl=
	return pcl_genmem(getpst(p.def))
end

global function genmem_d(symbol d)pcl=
	return pcl_genmem(getpst(d))
end

global proc genpushmem_d(symbol d)=
	pcl_gen(kpush,pcl_genmem(getpst(d)))
end

global function genmemaddr_d(symbol d)pcl=
	return pcl_genmemaddr(getpst(d))
end

global proc genpushmemaddr_d(symbol d)=
	pcl_gen(kpush,pcl_genmemaddr(getpst(d)))
end

global proc setmode(int m)=
	pcl_settype(getpclmode(m),ttsize[m])
end

global proc setmode_u(unit p)=
	pcl_settype(getpclmode(p.mode),ttsize[p.mode])
end

global function definelabel:int =
	pcl_gen(klabel,pcl_genlabel(++plabelno))
	return plabelno
end

global function createfwdlabel:int =
	return ++plabelno
end

global proc definefwdlabel(int lab) =
	pcl_gen(klabel,pcl_genlabel(lab))
end

global proc genreturn=
!assume returning from currproc
	case currproc.nretvalues
	when 0 then
		pcl_gen(kretproc)
	when 1 then
		pcl_gen(kretfn)
		setmode(currproc.mode)

	else
		pcl_genx(kretfn,currproc.nretvalues)
	esac
end

global function reversecond(int pclop)int=
!reverse conditional operator
	case pclop
	when keq then pclop:=kne
	when kne then pclop:=keq
	when klt then pclop:=kge
	when kle then pclop:=kgt
	when kge then pclop:=klt
	when kgt then pclop:=kle
	esac

	return pclop
end

global proc stacklooplabels(int a,b,c,d)=
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

global function findlooplabel(int k,n)int=
!k is 1,2,3,4 for label A,B,C,D
!n is a 1,2,3, etc, according to loop nesting index
	int i

	i:=loopindex-(n-1)		!point to entry
	if i<1 or i>loopindex then gerror("Bad loop index") fi
	return loopstack[i,k]
end

global proc pcl_gen_sysfn(int fnindex, unit a=nil,b=nil,c=nil)=
!CPL "GENSYSFN"
	pcl_gen_sysproc(fnindex, a,b,c, 1)
end

function getslots(unit p)int=
	if ttbasetype[p.mode] in [ti128, tu128, tslice] then return 2 fi
	return 1
end

global proc pcl_gen_sysproc(int fnindex, unit a=nil,b=nil,c=nil, int asfunc=0)=
	int nargs:=0, opc
	pcl p
	opc:=0

	pcl_gen(ksetargs)
	p:=pccurr

	if c then evalunit(c); nargs+:=getslots(c) fi
	if b then evalunit(b); nargs+:=getslots(b) fi
	if a then evalunit(a); nargs+:=getslots(a) fi

	p.nargs:=nargs

	pcl_gen((asfunc|kcallfn|kcallproc), pcl_genmemaddr(getsysfnhandler(fnindex)))
	pcl_setnargs(nargs)
end

proc $init=
	zero_unit.tag:=j_const
	zero_unit.mode:=ti64
	zero_unit.value:=0
	zero_unit.resultflag:=1
end

global function newframetemp(symbol owner,int size)symbol d=
!create a new local frame var used for implicit temps
	[32]char str

	fprint @&.str,"$T#",++pcltempindex
	d:=pcm_allocz(strec.bytes)

	d.name:=pcm_copyheapstring(&.str)
	d.owner:=owner

	d.nameid:=frameid

	return d
end

global proc addframetemp(symbol d)=
	pcl oldpccurr,pnew,pnext

	oldpccurr:=pccurr

	pcl_gen(klocal,pcl_genmem(getpst(d)))
	setmode(d.mode)

!	pnew:=pccurr
!	pccurr:=oldpccurr
!	pclcurr.nextpcl:=nil
!
!	pnext:=pclastframe.nextpcl
!	pclastframe.nextpcl:=pnew
!	pnew.nextpcl:=pnext
!	pclastframe:=pnew

end

proc getfullname(symbol d,ichar dest)=
	if d.owner and d.owner.nameid<>programid then
		getfullname(d.owner,dest)
		strcat(dest,".")
	fi
	strcat(dest,d.name)
end

global function getpst(symbol d)psymbol p=
	[300]char str
	symbol e

	return nil when d=nil

!CPL "GETPST",D.NAME

	if d.pstdef then return d.pstdef fi
	str[1]:=0

	if d.name^='m' and (d.name+1)^='$' then
		for i to sysfnnames.len do
			if eqstring(d.name+2, sysfnnames[i]+6) then

				if sysfnhandlers[i] then
					return d.pstdef:=sysfnhandlers[i]
				fi

				d.pstdef:=p:=pcm_allocz(p^.bytes)
				getfullname(d, str)
				p.name:=pcm_copyheapstring(str)
					p.symbol:=namesym
				sysfnhandlers[i]:=p
				return p
			fi
		od
	fi

	d.pstdef:=p:=pcm_allocz(p^.bytes)

!CPL "GETPST",

	if d.atvar and d.equivvar then
!CPL "EQUIV",D.NAME
		p:=getpst(e:=d.equivvar.def)
		d.pstdef:=e.pstdef
!	PCL_ADDSYM(P)
!		return p
		return e.pstdef
	fi
	if d.nameid in [dllprocid,dllvarid] then
		if d.truename then
			strcpy(str,d.truename)
			p.istruename:=1
		else
			strcpy(str,d.name)
		fi
	else
		getfullname(d, str)
	fi
	p.name:=pcm_copyheapstring(str)
	p.symbol:=namesym
	pcl_addsym(p)

	return p
end

function getsysfnhandler(int fn)psymbol p=
	[300]char str

	if sysfnhandlers[fn] then
		return sysfnhandlers[fn]
	fi


	sysfnhandlers[fn]:=p:=pcm_allocz(p^.bytes)

	IF MSYSLEVEL=1 THEN
		strcpy(str,"msystemp.m$")
	else
!		strcpy(str,"msyslib.m")
		strcpy(str,"msysp.m$")
!		strcpy(str,"t.m$")
	fi
	strcat(str,sysfnnames[fn]+6)
	p.name:=pcm_copyheapstring(str)
	p.symbol:=namesym

	pcl_addsym(p)

	return p
end

proc doimportedproc(symbol d)=
	symbol e

	pcl_gen(kextproc,genmem_d(d))
	setmode(d.mode)

	e:=d.deflist
	while e, e:=e.nextdef do
		if e.nameid=paramid then
			pcl_gent(kextparam, getpclmode(e.mode))
		fi
	od
	if d.varparams then
		pcl_genxy(kextvariadics,d.varparams,0)
	fi

	pcl_gen(kendextproc)
end

proc scansymbol(symbol d)=
	symbol e
	if d.nameid=dllprocid and d.used then
		doimportedproc(d)
	elsif d.nameid=dllvarid and d.used then
		pcl_gen(kextvar,genmem_d(d))
		setmode(d.mode)
!		doimportedproc(d)
	fi

	case d.nameid
	when programid,moduleid then
	else
		return
	esac

	e:=d.deflist

	while e, e:=e.nextdef do
		scansymbol(e)
	od
end

global proc genpushint(int a,mode=tpi64)=
	pcl_gen(kpush, pcl_genint(a,mode))
end

global proc genpushreal(real x)=
	pcl_gen(kpush,pcl_genreal(x))
end

global proc genpushreal32(real x)=
	pcl_gen(kpush,pcl_genreal32(x))
end

global proc genpushstring(ichar s)=
	pcl_gen(kpush,pcl_genstring(s))
end

=== mm_lex.m 0 0 8/44 ===
macro hashc(hsum,c)=hsum<<4-hsum+c
macro hashw(hsum)=(hsum<<5-hsum)

const maxstackdepth=20
[maxstackdepth]ref char lxstart_stack
[maxstackdepth]ref char lxsptr_stack
[maxstackdepth]int lxfileno_stack
[maxstackdepth]tokenrec lxnextlx_stack
global int sourcelevel=0

const cr	= 13
const lf	= 10
const tab	= 9

ref char lxstart
ref char lxsptr
int lxifcond
int longsuffix			!for real nos

int lxfileno
!global const hstsize	= 32768
global const hstsize	= 65536
global const hstmask	= hstsize-1

global [0:hstsize]strec hashtable
global [0:hstsize]word hashkeys
symbol hashtablelast

global int astringlength

byte prescanmode=0

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
	int lena,lenb
	ref char p

	lx:=nextlx				!grab that already read basic token
reenter::
	lexreadtoken()
reenter2::

	switch nextlx.symbol
	when kcasesym,kswitchsym,kdocasesym,kdoswitchsym,kforsym,
			kdosym,ktosym,kprocsym,kfunctionsym,kimportmodulesym,kunlesssym,
			krecordsym,kstructsym,kunionsym,ktypesym,kwhilesym,kclasssym,
			ktrysym,ktabledatasym,kassemsym,kifsym then

		if lx.symbol=kendsym then
			if lx.subcode then lxerror("end if if?") fi
			lx.subcode:=nextlx.symbol
			reenter
		fi

	when eolsym then
		if lx.symbol in [commasym, lsqsym, lbracksym] then !ignore eol
			reenter
		elsif symboloptypes[lx.symbol]=bin_op and not assemmode and 
			lx.symbol not in [maxsym, minsym] then
			reenter
		fi
		nextlx.symbol:=semisym

	when stringconstsym then
		if lx.symbol=stringconstsym then
			lena:=strlen(lx.svalue)
			lenb:=strlen(nextlx.svalue)
			p:=pcm_alloc(lena+lenb+1)
			memcpy(p,lx.svalue,lena)
			memcpy(p+lena,nextlx.svalue,lenb)
			(p+lena+lenb)^:=0
			lx.svalue:=p
		fi
	when ksourcedirsym then
		if not dolexdirective(nextlx.subcode) then		!skip symbol
			reenter
		fi

	when namesym then
		case nextlx.subcode
		when unitnamesym then
			case lx.symbol
			when intconstsym then
				if lx.subcode in [ti128,tu128] then
					lxerror("No suffix on i128/u128")
				fi
				case nextlx.symptr.index
				when million_unit then lx.value *:= 1 million
				when billion_unit then lx.value *:= 1 billion
				when thousand_unit then lx.value *:= 1 thousand
				when kilo_unit then lx.value *:= 1024
				when mega_unit then lx.value *:= 1048576
				when giga_unit then lx.value *:= (1048576*1024)
				else
					lxerror("Can't do this unit index")
				esac
				lx.subcode:=setinttype(lx.value)
				reenter
			when realconstsym then
				lxerror("Unit suffix after float not implem")
			else
				nextlx.symbol:=namesym
			esac
		when kheadersym then
			if not headermode then
				nextlx.symbol:=namesym
			else
				nextlx.symbol:=kheadersym
				nextlx.subcode:=nextlx.symptr.index

			fi
		else
			nextlx.symbol:=namesym
		esac
	when machinetypesym then
		case nextlx.subcode
		when 'I','i' then nextlx.subcode:=ti64
		when 'W','w' then nextlx.subcode:=tu64
		esac
		nextlx.symbol:=stdtypesym

	when rawxnamesym then
		nextlx.symbol:=namesym

	when insym then
		if lx.symbol=notlsym then
			lx.symbol:=notinsym
			lx.subcode:=knotin
			reenter
		fi
	when eqsym then
		if lx.symbol=notlsym then
			lx.symbol:=cmpsym
			lx.subcode:=kne
			reenter
		fi
	end switch

	nextlx.pos :=nextlx.pos ior lxfileno<<24

!CP "     ";PS("LEX")

end

global proc lexreadtoken=
!read next token into nextlx
	int c,hsum,commentseen,hashindex,length
	ref char pstart,pnext,p,ss,lxsvalue

	nextlx.subcode:=0

	doswitch lxsptr++^
	when 'a'..'z','_','$',0x80..0xEE, 0xF0..0xFF then
		lxsvalue:=lxsptr-1
	doname::
		hsum:=lxsvalue^

		doswitch c:=lxsptr++^
		when 'a'..'z','0'..'9','_','$',0x80..0xEE, 0xF0..0xFF then
			hsum:=hashc(hsum,c)
		when 'A'..'Z' then
			(lxsptr-1)^:=c+' '
			hsum:=hashc(hsum,c+' ')
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

		lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))
		return

	when 'A'..'Z' then
		lxsvalue:=lxsptr-1
		lxsvalue^+:=32
		goto doname

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
		elsif c='1' and lxsptr^ in '0'..'6' and (lxsptr+1)^ in ['x','X'] then
			int base:=lxsptr^+(10-'0')
			lxsptr+:=2
			readnumber(base)

		else
			--lxsptr
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
		++nextlx.pos
		nextlx.symbol:=eolsym
		return

	when '#' then			!docstring to eol
		lxsvalue:=cast(lxsptr)

		doswitch c:=lxsptr++^
		when 13,10,0 then			!leave eol for next symbol
			--lxsptr
			exit
		end

		length:=lxsptr-cast(lxsvalue,ref char)
		nextlx.symbol:=docstringsym
		nextlx.svalue:=pcm_copyheapstringn(lxsvalue,length)
		return

	when '\\' then			!line continuation

!two stages::
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
		commentseen:=0
		doswitch lxsptr++^			!read until end of this line
		when cr then
			++nextlx.pos
			++lxsptr				!skip lf
			exit
		when lf then
			++nextlx.pos
			exit
		when 0 then
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
			++nextlx.pos
			++lxsptr				!skip lf
		when lf then
			++nextlx.pos
		when ' ',tab then
		else
			--lxsptr
			exit
		enddoswitch

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
				nextlx.subcode:=j_makerange		!helps treat as opsym which all have k-code as subcode
			fi
			return
		when '0'..'9' then			!real const: deal with this after the switch
			--lxsptr
			readrealnumber(nil,0,10)
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
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=assignsym
			nextlx.subcode:=j_assign		!helps treat as opsym which all have k-code as subcode
		when ':' then
			++lxsptr
			case lxsptr^
			when '=' then
				++lxsptr
				nextlx.symbol:=deepcopysym
				nextlx.subcode:=j_deepcopy
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

	when '~' then
		nextlx.symbol:=curlsym
		return

	when '+' then
		nextlx.symbol:=addsym
		if lxsptr^='+' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kincr
			return
		fi
		return

	when '-' then
		nextlx.symbol:=subsym
		if lxsptr^='-' then
			++lxsptr
			nextlx.symbol:=incrsym
			nextlx.subcode:=kdecr
			return
		fi
		return

	when '*' then
		if lxsptr^='*' then
			++lxsptr
			nextlx.symbol:=powersym
		else
			nextlx.symbol:=mulsym
		fi
		return

	when '/' then
		nextlx.symbol:=divsym
		return

	when '%' then
		nextlx.symbol:=idivsym
		return

	when '=' then
		case lxsptr^
		when '>' then
			nextlx.symbol:=sendtosym
			++lxsptr
		when '=' then
			++lxsptr
!			if lxsptr^='=' then
!				++lxsptr
!				nextlx.symbol:=ssmarkersym
!			else
				nextlx.symbol:=samesym
!			fi
		else
			nextlx.symbol:=eqsym
			nextlx.subcode:=keq
		esac
		return

	when '<' then
		nextlx.symbol:=cmpsym
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.subcode:=kle
		when '>' then
			++lxsptr
			nextlx.subcode:=kne
		when '<' then
			++lxsptr
			nextlx.symbol:=shlsym
		else
			nextlx.subcode:=klt
		endswitch
		return

	when '>' then
		nextlx.symbol:=cmpsym
		switch lxsptr^
		when '=' then
			++lxsptr
			nextlx.symbol:=cmpsym
			nextlx.subcode:=kge
		when '>' then
			++lxsptr
			nextlx.symbol:=shrsym
		else
			nextlx.symbol:=cmpsym
			nextlx.subcode:=kgt
		endswitch
		return

	when '&' then
		case lxsptr^
!			when '&' then
!			++lxsptr
!			nextlx.symbol:=opsym
!			nextlx.subcode:=j_andand
		when '.' then
			++lxsptr
			nextlx.symbol:=anddotsym
			nextlx.subcode:=0
		else
			nextlx.symbol:=addrsym
			nextlx.subcode:=j_addrof
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
		++nextlx.pos
		nextlx.symbol:=eolsym
		return
	when lf then			!only lfs not preceded by cr
		++nextlx.pos
		nextlx.symbol:=eolsym
		return

	when 0 then
!CPL "EOF SEEN",=SOURCELEVEL
		if sourcelevel then
			unstacksource()
RETURN
		else
			nextlx.symbol:=eofsym
			--lxsptr
			return
		fi

	when 0xEF then			!BOM
		lxsptr+:=2

	else
		nextlx.symbol:=errorsym
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
		getstrint(expon,&.str)
		memcpy(ss+intlen+fractlen+2,&.str,strlen(&.str)+1)

		nextlx.symbol:=decimalconstsym
		nextlx.svalue:=ss
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

	nextlx.symbol:=realconstsym
	nextlx.subcode:=treal
	nextlx.xvalue:=x

end

function readrealbest(int intlen,fractlen,expon, ichar realstr)real=
	[32]char expstr

	(realstr+intlen+fractlen)^:=0
	expon-:=fractlen

	print @&.expstr,"e",,expon
!FPRINTLN "RRBEST: INTLEN=# fractlen=# expon=# realstr=<#> expstr=<#>",
!	INTLEN,FRACTLEN,EXPON, REALSTR, &.expstr
	strcat(realstr,&.expstr)
!FPRINTLN "realstr'=<#>", realstr
!CPL =STRTOD("10e-1",NIL)
!CPL =STRTOD(REALSTR,NIL)

!REAL X:=STRTOD(REALSTR,NIL)
!PRINTF("PRINTF X=%f\n",X)

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
	when namesym then
		printstrn(l.symptr.name,l.symptr.namelen)

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
		andlsym,orlsym,eqsym,cmpsym,addsym,subsym,
		mulsym,divsym,idivsym,iremsym,iandsym,iorsym,ixorsym,shlsym,shrsym,
		minsym,maxsym,powersym,samesym then
		print symbolnames[l.symbol]
	elsif l.subcode then
		fprint "SUBCODE:",l.subcode
!	fprint "#",symbolnames[l.subcode]
	end

	print $,=lx.fileno
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

	nextlx.symbol:=intconstsym

	if length>maxnumlen[base] or 
			(length=maxnumlen[base] and strncmp(s,maxnumlist[base],length)>0) then
		if base<>16 then
			lxerror("longint const")

		else
			if length>32 or 
				(length=32 and strncmp(s,"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0) then
				lxerror("longint const")

			else						!greater than 64 bits, up to 128 bits

				if length=32 and strncmp(s,"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",32)>0 then
					nextlx.subcode:=tu128
				else
					nextlx.subcode:=ti128
				fi

				nextlx.pvalue128:=stringtonumber128(s,length,16)
			fi
		fi
		return
	fi

	a:=0

	if base<=10 then
		to length do
			a:=a*base+s++^-'0'
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
	int64 a
	word64 b
	int c

!trim leading zeros, which make it difficult to do a string match with maxstr
	while length>=2 and s^='0' do		!trim leading zeros
		++s
		--length
	od

	nextlx.symbol:=intconstsym

	if length>20 or 
			(length=20 and strncmp(s,"18446744073709551615",20)>0) or suffix then

		if length>39 or 
			(length=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
			if suffix='W' then
				lxerror("-W overflows 128 bits")
			fi
	dolongint::
			nextlx.symbol:=decimalconstsym
			nextlx.svalue:=pcm_copyheapstring(s)
		else						!greater than 64 bits, up to 128 bits

			if suffix='L' then goto dolongint fi

			if (length=39 and strncmp(s,"170141183460469231731687303715884105727",39)>0) then
				nextlx.subcode:=tu128
			else
				nextlx.subcode:=ti128
			fi

			nextlx.pvalue128:=stringtonumber128(s,length,10)
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

global proc lexsetup=
!do one-time setup::
! clear the hash table and populated it with reserved words
! do maxnum support and such
	int i!,n
	static int n

	for i to maxnumlist.len do
		maxnumlen[i]:=strlen(maxnumlist[i])
	od

	inithashtable()
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
	when 'A'..'D','F','a'..'d','f' then
		if 11<=base<=16 then		!NEEDS TO CHECK LIMITS FOR BASES 10..15
			dest++^:=c
		else
			--lxsptr
			exit
		fi
	when 'E','e' then
		if base<15 then
			--lxsptr
			exit
		else
			dest++^:=c
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

	nextlx.symbol:=stringconstsym
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
	when cr,lf,0 then
		lxerror("Raw string not terminated")
		--lxsptr
		exit
	else
		dest++^:=c
	enddoswitch
end

proc lookup(ref char name, int length, hashindex0)=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int wrapped, hashindex,INDEX,n
	symbol d

	hashindex:=hashindex0 iand hstmask

	d:=&hashtable[hashindex]
	wrapped:=0

	do
		if (n:=d.namelen)=length and memcmp(d.name,name,n)=0 then	!match
			nextlx.symptr:=d
			nextlx.symbol:=d.symbol
			nextlx.subcode:=d.subcode
			return
		elsif n=0 then
			exit
		fi
		++d
		if d>=hashtablelast then
			if wrapped then
				abortprogram("HASHTABLE FULL")
			fi
			wrapped:=1
			d:=&hashtable[0]
		fi
	od

!exit when not found; new name will go in entry pointed to by lxsymptr

	d.name:=pcm_copyheapstringn(name,length)
	d.namelen:=length
	d.symbol:=namesym

	nextlx.symptr:=d
	nextlx.symbol:=d.symbol
!	nextlx.subcode:=d.subcode
end

function lookupsys(ref char name)int=
!lookup rawnamesym with details in nextlx
!hash value already worked out in lxhashvalue
!return 1 (found) or 0 (not found)
!in either case, lx.symptr set to entry where name was found, or will be stored in
	int j, wrapped, hashvalue

	j:=gethashvaluez(name) iand hstmask

	lx.symptr:=&hashtable[j]
	wrapped:=0

	do
		if lx.symptr.namelen=0 then
			exit
		elsif eqstring(lx.symptr.name,name) then	!match
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
	lx.symptr.name:=name				!assume can be shared (stored in a table)
	lx.symptr.namelen:=strlen(name)
	lx.symptr.symbol:=namesym			!usually replaced with actual symbol details

	return 0
end

function gethashvaluez(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!ASSUMES S is lower-case, as conversion not done
	int c,hsum

	if s^=0 then return 0 fi

	hsum:=s++^

	do
		c:=s++^
		exit when c=0
		hsum:=hashc(hsum,c)
	od
	return hashw(hsum)
end

proc inithashtable=
!populate hashtable with standard symbols
	int i
	memset(&hashtable,0,hashtable.bytes)
	hashtablelast:=&hashtable[hstsize-1]

	for i:=1 to stnames.len do
		lookupsys(stnames[i])

		lx.symptr.symbol:=stsymbols[i]

		case stsymbols[i]
		when unitnamesym, kheadersym then
			lx.symptr.index:=stsubcodes[i]
			lx.symptr.subcode:=stsymbols[i]
			lx.symptr.symbol:=namesym		!masquerades as normal identifier
		else
			lx.symptr.subcode:=stsubcodes[i]
		esac
	od
end

global proc printhashtable=
	println "Hashtable:"

	for i:=0 to hstsize-1 do
		if hashtable[i].namelen then
			println i,hashtable[i].name,symbolnames[hashtable[i].symbol]
		fi
	od
end

global proc addreservedword(ichar name,int symbol,subcode, regsize=0)=

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
!	when strincludedir,binincludedir then
!		lexreadtoken()
!		if nextlx.symbol<>stringconstsym then
!			lxerror("strincl: string expected")
!		else
!			file:=nextlx.svalue
!		fi
!
!		fileno:=getsupportfile(file, sourcefilepaths[lxfileno], issupport:1)
!		nextlx.svalue:=sourcefiletext[fileno]
!		astringlength:=length:=sourcefilesizes[fileno]
!
!		nextlx.symbol:=(index=strincludedir|stringconstsym|astringconstsym)
!		nextlx.subcode:='A'			!for use when an astring
!		(nextlx.svalue+length)^:=0			!sometimes .length is not used (eg. in newstringobj())
!		return 1						!so get it right. Don't need the etx

	when includedir then
		lexreadtoken()
		if nextlx.symbol<>stringconstsym then lxerror("include: string expected") fi
		file:=nextlx.svalue
		convlcstring(file)
		file:=addext(file,".m")		!add in extension if not present; assume same as source

		fileno:=getsupportfile(file, sourcefilepaths[lxfileno], issupport:1)
		stacksource(fileno)
		return 0

!	when defineunitdir then
!		LXERROR("DEFINE UNIT NOT DONE")
!
	else
		cpl sourcedirnames[index]
		lxerror("Directive not implemented")
	esac
	return 0
end

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

global proc startlex(int fileno)=
!start processing one of the file in sourcefile tables as source code
!assume it is a complete header or module

	lxsptr:=sourcefiletext[fileno]

	lxfileno:=fileno
	nextlx.pos:=1

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global function addnamestr(ichar name)ref strec=
	tokenrec oldlx
	ref strec symptr

	oldlx:=nextlx
	lookup(name,strlen(name), gethashvaluez(name))
	symptr:=nextlx.symptr
	nextlx:=oldlx

	return symptr
end

global proc ps(ichar caption)=
	print "PS:",caption,,": "
	printsymbol(&lx)
end

global proc psnext(ichar caption)=
	print caption,,": "
	printsymbol(&nextlx)
end

global proc psx(ichar caption)=
	print caption,,": "
	printsymbol(&lx)
	print "	"
	printsymbol(&nextlx)
end

global proc stacksource(int fileno)=
!introduce new source level for macros or include files
!not used for main source

	if sourcelevel>=maxstackdepth then
		lxerror("Include file/macro overflow")
	fi
	++sourcelevel
	lxstart_stack[sourcelevel]:=lxstart
	lxsptr_stack[sourcelevel]:=lxsptr
	lxfileno_stack[sourcelevel]:=lxfileno
	lxnextlx_stack[sourcelevel]:=nextlx

	lxstart:=lxsptr:=sourcefiletext[fileno]

	nextlx.pos:=1
	lxfileno:=fileno

	nextlx.symbol:=semisym
	nextlx.subcode:=0
end

global proc unstacksource=
	if sourcelevel>0 then			!check that some source is stacked
		lxstart:=lxstart_stack[sourcelevel]
		lxsptr:=lxsptr_stack[sourcelevel]
		nextlx:=lxnextlx_stack[sourcelevel]
		lxfileno:=lxfileno_stack[sourcelevel]
		--sourcelevel
	fi
end

proc readarraystring(int prefix)=
	++lxsptr
	lxreadstring('"')
	nextlx.symbol:=astringconstsym
	nextlx.subcode:=toupper(prefix)
	astringlength:=strlen(nextlx.svalue)
end

function stringtonumber128(ichar s, int length,base)ref int128=
	int128 aa
	int c,d
	aa:=0
	to length do
		aa:=aa*base
		c:=(s++)^

		if c>='a' then
			d:=c-'a'+10
		elsif c>='A' then
			d:=c-'A'+10
		else
			d:=c-'0'
		fi

		aa:=aa+d
	od

	ref int128 p
	p:=pcm_alloc(int128.bytes)
	p^:=aa

	return p
end

function setinttype(word64 a)int=
	if a<=u64(0x7FFF'FFFF'FFFF'FFFF) then
		return ti64
	else
		return tu64
	fi
end

proc readrawxname=
	int c,hsum,length

	nextlx.svalue:=lxsptr
	hsum:=0

	doswitch c:=lxsptr++^
	when 'A'..'Z','a'..'z','0'..'9','_','$',128..255then
		hsum:=hashc(hsum,c)
	else
		--lxsptr
		exit
	end doswitch

	length:=lxsptr-nextlx.svalue

	if length=0 then
		lxerror("Bad ` name")
	fi
	lookup(nextlx.svalue,length, hashw(hsum))
	nextlx.symbol:=rawxnamesym

	return
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
		nextlx.symbol:=stringconstsym
	else
		nextlx.symbol:=charconstsym
		nextlx.subcode:=tint
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
		when 'a','b','c','e','r','f','l','n','s','t','v','y','z','0','"','q','\\','\'' then
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
				hasescape:=1
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

	if length=0 then
		nextlx.svalue:=""
		return
	elsif not hasescape then
		nextlx.svalue:=pcm_copyheapstringn(s,length)
		return
	fi

!need to copy string to dest and expand the escape codes

	nextlx.svalue:=t:=pcm_alloc(length+1)

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
!			when 'u' then			!reserved for unicode, like \x but with 4 hex digits
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
				lxerror_s("Unknown string escape: \\%s",&.str)
			end
		when '"','\'' then		!possible terminators
			if c=termchar then		!terminator char
				if s^=c then		!repeated, assume embedded term char
					++s
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
=== mm_lib.m 0 0 9/44 ===
int autotypeno=0
global int nextavindex=0
int nextsvindex=0

strbuffer exprstrvar
ref strbuffer exprstr=&exprstrvar

const int unitheapsize=32768
ref unitrec unitheapptr=nil
int remainingunits=0

strbuffer sbuffer
global ref strbuffer dest=&sbuffer

global ichar framevarname			!normally nil, set to frame var def to display in comment

global function newstrec:symbol=
	symbol p
	p:=pcm_alloc(strec.bytes)
	clear p^

	p.pos:=lx.pos
	p.moduleno:=currmoduleno
	return p
end

global function getduplnameptr(symbol owner,symptr,int id)symbol=
!create duplicate stentry
!owner is the owner ST
!symptr points to the current generic entry for the name (nameid=0)
!id is the desired nameid
!new entry is created, and added to the dupl chain for this name
!return pointer to new strec; this can be stored directly in a -def unit
!but such nameptrs are not allowed elsewhere; it must be wrapped in a knameunit
	symbol p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

!	if id=frameid or id=paramid then
!		p.frame:=1
!	fi

	p.nextdupl:=symptr.nextdupl
	p.firstdupl:=symptr
	symptr.nextdupl:=p

	return p
end

global proc adddef(symbol owner,p)=
!add new st def p, to existing deflist of owner
!assumes p already has a .owner link to owner, but is not yet part of owner's deflist
!pgeneric points to the 'generic' entry for this name in the main hash table.
!this is needed as it contains the head of the dupl list for this name (linking
!all instances of this name).
!Usually the dupl list is checked to ensure that there are no existing names
!with this same owner. (pgeneric can be nil to avoid this check.)
!ASSUMES THAT P IS LAST THING ADDED TO HEAD OF DUPLLIST (just after PGENERIC)
	symbol q

	if q:=p.nextdupl then
		if q.owner=owner then
			cpl q.name,"in",owner.name
			serror("Duplicate name")
		fi
	fi

	if owner.deflist=nil then			!first def
		owner.deflist:=p
	else
		owner.deflistx.nextdef:=p
	fi

	owner.deflistx:=p
end

global function createname(symbol p)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=j_name
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
	u.hasa:=1
	return u
end

global function createunit2(int tag, ref unitrec p,q)ref unitrec=
	ref unitrec u

	u:=allocunitrec()

	u.tag:=tag
	u.a:=p
	u.b:=q
	u.hasa:=1
	u.hasb:=1
	return u
end

global function createunit3(int tag, ref unitrec p,q,r)ref unitrec=
	ref unitrec u

	u:=allocunitrec()
	u.tag:=tag
	u.a:=p
	u.b:=q
	u.c:=r
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
	mode:=q.mode
	nextunit:=q.nextunit
	q.nextunit:=nil

	clear p^

	p.tag:=tag
	p.pos:=q.pos
	p.a:=q
	p.hasa:=1
	p.mode:=mode
	p.nextunit:=nextunit
	p.resultflag:=q.resultflag
end

global proc deleteunit(unit p,q)=
!delete p, replace by q, so that what was addressed by p now contains q
	unit r:=p.nextunit
	p^:=q^
	p.nextunit:=r
end

global function createconstunit(word64 a, int t)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=j_const
	u.value:=a
	u.mode:=t

	if t in [ti128,tu128] then
		u.value128:=ref int128(a)^
	fi

	u.isconst:=1
	return u
end

global function createstringconstunit(ichar s, int length)ref unitrec=
	ref unitrec u
	u:=allocunitrec()
	u.tag:=j_const
	u.svalue:=s
	u.mode:=trefchar
	u.isastring:=1
	if length=-1 then
		u.slength:=strlen(s)
	else
		u.slength:=length
	fi
	return u
end

global function newtypename(symbol a,b)int=
	if ntypenames>=maxtypename then
		serror("Too many type names")
	fi
	++ntypenames
	typenames[ntypenames].defa:=a		!leave .owner/.pmode to be filled in
	typenames[ntypenames].defb:=b		!used type's mode is used

	typenamepos[ntypenames].pos:=lx.pos

	return -ntypenames
end

global function createusertype(symbol stname)int=
!create new, named user type
	if ntypes>=maxtype then
	cpl ntypes,stname.name
		serror("Too many types")
	fi

	++ntypes
	ttname[ntypes]:=stname.name

	ttnamedef[ntypes]:=stname
	ttbasetype[ntypes]:=tvoid
	ttlineno[ntypes]:=lx.pos

	stname.mode:=ntypes

	return ntypes
end

global function createusertypefromstr(ichar name)int=
!create new, named user type
	symbol stname

	stname:=getduplnameptr(stmodule,addnamestr(name),typeid)
	return createusertype(stname)
end

global function getrangelwbunit(ref unitrec p)ref unitrec=				!GETRANGELWB
	if p.tag=j_makerange then
		return p.a
	else
		p:=createunit1(j_unary,p)
		p.pclop:=klwb
		return p
	fi
end

global function getrangeupbunit(ref unitrec p)ref unitrec=				!GETRANGEUPB
	if p.tag=j_makerange then
		return p.b
	else
		p:=createunit1(j_unary,p)
		p.pclop:=kupb
		return p
	fi
end

global function createarraymode(symbol owner,int target,unit dimexpr, int typedefx)int=		!CREATEARRAYMODE
!lower is lower bound of array
!length is length, unless lx<>nil!
	int k,m

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes do
			if ttusercat[k]=0 and ttbasetype[k]=tarray and tttarget[k]=target and
					sameunit(dimexpr, ttdimexpr[k],owner, ttowner[k]) then
				return k
			fi
		od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	ttbasetype[m]:=tarray
	ttlower[m]:=1
	ttdimexpr[m]:=dimexpr
	storemode(owner,target,tttarget[m])
	ttowner[m]:=owner

	return m
end

function sameunit(unit p,q, symbol powner=nil, qowner=nil)int=
!p are q are units just parses; no name resolving or type checking
!do a simple check to see if they are the same unit
	symbol d,e

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

global function createarraymodek(symbol owner,int target,int lower,length, int typedefx)int=		!CREATEARRAYMODE
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
	ttowner[m]:=owner

	return m
end

global function nextautotype:ichar=
	static [32]char str

	print @&.str,"$T",,++autotypeno
	return &.str
end

global function createslicemode(symbol owner,int slicetype,target,unit dimexpr, int typedefx=0)int=
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
	ttowner[m]:=owner

	return m
end

global function createslicemodek(symbol owner,int target,lower, int typedefx=0)int=
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
	ttowner[m]:=owner

	return m
end

global function createrefmode(symbol owner,int target,typedefx=0)int=		!CREATEREFPACKMODE
	int k,m

	if typedefx=0 then		!anon type
		for k:=tlast to ntypes do
			if ttusercat[k]=0 and ttbasetype[k]=tref and tttarget[k]=target then
				return k
			fi
		od
		m:=createusertypefromstr(nextautotype())
	else
		m:=typedefx
	fi

	storemode(owner,target,tttarget[m])
	ttbasetype[m]:=tref
	ttsize[m]:=ttsize[tref]
	ttisref[m]:=1

	return m
end

global function createrefprocmode(symbol owner,stproc, paramlist,int kwd, prettype,typedefx)int=		!CREATEREFPROCMODE
!create a ref proc mode; (can't create a proc mode by itself, as it's meaningless)
	int m, mproc

	mproc:=createusertype(stproc)
	stproc.paramlist:=paramlist

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

global proc copyttvalues(int dest, source)=
	ttisint[dest]		:= ttisint[source]
	ttisword[dest]		:= ttisword[source]
	ttisreal[dest]		:= ttisreal[source]
	ttisinteger[dest]	:= ttisinteger[source]
	ttisallnum[dest]	:= ttisallnum[source]
	ttismainnum[dest]	:= ttismainnum[source]
	ttisshort[dest]		:= ttisshort[source]
	ttisref[dest]		:= ttisref[source]
end

global proc setnameptr(ref unitrec p)=		!SETNAMEPTR
!p is a just created j_...def unit which has a nameptr in the .a parameter
!set up an xref from the strec back to the -def unit
!Set up a pointer in the associated strec that points back to q

p.def.code:=p
end

global function getdottedname(symbol p)ichar=		!GETDOTTEDNAME
!build full dotted name for st item p
	static [256]char str
	[256]char str2
	symbol owner

	strcpy(&.str,p.name)
	owner:=p.owner
	while owner and owner.nameid<>programid do
		strcpy(&.str2,&.str)
		strcpy(&.str,owner.name)
		strcat(&.str,".")
		strcat(&.str,&.str2)
		owner:=owner.owner
	od
	return &.str
end

global function getavname(symbol owner,int id=frameid)symbol=
!create auto-var name and return pointer to st entry
	symbol p
	[32]char str
	ichar name

	if id=frameid and owner.nameid<>procid then
		serror("Auto frame not in proc")
	fi

	if id=frameid then
		print @&.str,"av_",,++nextavindex
	else
		print @&.str,"sv_",++nextsvindex
	fi

	name:=pcm_copyheapstring(&.str)
	addnamestr(name)

	p:=getduplnameptr(owner,addnamestr(name),id)
!p.namecat:=frame_cat
	p.used:=1

	p.mode:=tint

	adddef(owner,p)
	return p
end

global proc unionstr_clear(ref uflagsrec u)=
	((ref word64(u))^:=0)		!clear flags and length togetjer
end

global proc unionstr_append(ref uflagsrec u, int c)=
	if u.ulength=(u.codes.len-1) then
		serror("Uflags overflow/a")
	fi
	++u.ulength
	u.codes[u.ulength]:=c
end

global proc unionstr_concat(ref uflagsrec u, v)=
	int ulen,vlen,i

	ulen:=u.ulength
	vlen:=v.ulength
	if ulen+vlen>u.codes.len then
		serror("Uflags overflow/c")
	fi
	for i:=1 to vlen do
		u.codes[i+ulen]:=v.codes[i]
	od
	u.ulength:=ulen+vlen
end

global function unionstr_last(ref uflagsrec u)int=
	if u.ulength then
		return u.codes[u.ulength]
	fi
	return 0 
end

global proc unionstr_copy(ref uflagsrec u,v)=
	memcpy(u,v,uflagsrec.bytes)
end

global function createrecordmode(symbol owner,int typedefx)int=	!CREATERECORDMODE
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

global function createtuplemode(symbol owner,slice[]int elements,int typedefx)int=
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

global function createenummode(symbol owner,int typedefx)int=
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

global proc jeval(ref strbuffer dest, ref unitrec p)=			!JEVAL
!p represents an expression. It can be a unitrec only, not a list (lists only occur inside
!kmakelist and kmakeset units, which specially dealt with here)
!dest is a destination string. Special routines such as gs_additem() are used, which take care
!of separators so that successive alphanumeric items don't touch
	unit q,a,b
	[500]char str

	if p=nil then
		return
	fi

	a:=p.a
	b:=p.b

	switch p.tag
	when j_const then

		case ttbasetype[p.mode]
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
			print @&.str,p.xvalue
		when tref then
			if p.mode=trefchar and p.isastring then
				if p.slength>str.len/2 then
					strcpy(&.str,"LONGSTR)")
				else
					convertstring(p.svalue,&.str)
				fi
				gs_additem(dest,"""")
				gs_additem(dest,&.str)
				gs_additem(dest,"""")
				return
			else
				print @&.str,ref void(p.value)
			fi
		else
			strcpy(&.STR,"<EVAL/CONST PROBABLY VOID>")
		esac
		gs_additem(dest,&.str)

	when j_name then
		gs_additem(dest,p.def.name)

	when j_bin,j_cmp then

		strcpy(&.str,pclnames[p.pclop])
		gs_additem(dest,"(")
		jeval(dest,a)
		gs_additem(dest,&.str)
		jeval(dest,b)
		gs_additem(dest,")")

	when j_unary, j_istruel then

		strcpy(&.str,pclnames[p.pclop])
		gs_additem(dest,&.str)
		gs_additem(dest,"(")

		if a.tag=j_typeconst then
			gs_additem(dest,STRMODE(a.value))
		else
			jeval(dest,a)
		fi
		gs_additem(dest,")")

	when j_callfn,j_callproc then
		jeval(dest,a)
		gs_additem(dest,"(")

		q:=b
		while q do
			jeval(dest,q)
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,")")

	when j_index,j_dotindex,j_slice,j_dotslice then
		jeval(dest,a)
		if p.tag=j_dotindex or p.tag=j_dotslice then
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
			q:=q.nextunit
			if q then gs_additem(dest,",") fi
		od
		gs_additem(dest,")")

	when j_makerange then
		gs_additem(dest,"(")
		jeval(dest,a)
		gs_additem(dest,"..")
		jeval(dest,b)
		gs_additem(dest,")")

	when j_assign then
		jeval(dest,a)
		gs_additem(dest,":=")
		jeval(dest,b)

	when j_if then
		gs_additem(dest,"(")
		jeval(dest,a)
		gs_additem(dest,"|")
		jeval(dest,b)
		gs_additem(dest,"|")
		jeval(dest,p.c)
		gs_additem(dest,")")

	when j_typeconst then
		gs_additem(dest,strmode(p.mode))

	when j_convert then

		gs_additem(dest,strmode(p.convmode))
		gs_additem(dest,"(")
		jeval(dest,a)
		gs_additem(dest,")")

	when j_shorten then

		gs_additem(dest,"shorten(")
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
			jeval(dest,p.b)
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
		jeval(dest,p.c)
		gs_additem(dest,")")

	when j_block then
		gs_additem(dest,"<JBLOCK>")

	when j_null then
		gs_str(dest,"<nullunit>")

	when j_addrof then
		gs_additem(dest,"&")
		jeval(dest,a)
		if b then
			gs_str(dest,"+")
			gs_strint(dest,b.value)
		fi

	when j_addroffirst then
		gs_additem(dest,"&.")
		jeval(dest,a)

	when j_typestr then
		gs_additem(dest,"TYPESTR(")
		jeval(dest,a)
		gs_additem(dest,")")

	when j_cvlineno, j_cvfilename, j_cvmodulename then
		gs_str(dest,"$")
		gs_str(dest,jtagnames[p.tag]+2)

	when j_bitfield then
		jeval(dest,a)
		gs_str(dest,".")
		gs_str(dest,bitfieldnames[p.bitopindex])

	when j_fmtitem then
		jeval(dest,a)
		gs_str(dest,":")
		jeval(dest,b)

	when j_typeof then
		gs_str(dest,"typeof(")
		jeval(dest,a)
		gs_str(dest,")")

	when j_syscall then
		gs_str(dest,sysfnnames[p.fnindex]+6)
		gs_str(dest,"(")
		if a then jeval(dest,a) fi
		gs_str(dest,")")
	when j_incr then
		gs_str(dest,"incr ")
		jeval(dest,a)
	when j_strinclude then
		gs_str(dest,"newstrinclude ")
		jeval(dest,a)

	else
		CPL jtagnames[p.tag]
		gerror("CAN'T DO JEVAL",p)
	end
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
	symbol d,q,e
	int value,needcomma,x,i,target,mbase,n
	strbuffer sxx
	ref strbuffer xx:=&sxx
	ref strbuffer sdim,slength
	[100]char strdim
	ichar prefix
	typenamerec tn

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

	when tarray then
		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
			fprint @dest,"@[#]",&.strdim
		else
			if ttlength[m] then
				if ttlower[m]=1 then
					fprint @dest,"[#]",ttlength[m]+ttlower[m]-1
				else
					fprint @dest,"[#..#]",ttlower[m],ttlength[m]+ttlower[m]-1
				fi
			else
				if ttlower[m]=1 then
					fprint @dest,"[]"
				else
					fprint @dest,"[#:]",ttlower[m]
				fi
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tslice then
		prefix:=stdnames[mbase]

		if ttdimexpr[m] then
			gs_copytostr(strexpr(ttdimexpr[m]),&.strdim)
			fprint @dest,"@#[#:]",prefix,&.strdim
		else
			if ttlower[m]=1 then
				strcpy(dest,prefix)
				strcat(dest,"[]")
			else
				fprint @dest,"#[#:]",prefix,ttlower[m]
			fi
		fi
		istrmode(tttarget[m],0,dest+strlen(dest))

	when tenum then
		d:=ttnamedef[m]
		if not expand then
			strcpy(dest,d.name)
			return
		fi

		strcpy(dest,"enum(")

		value:=1
		needcomma:=0
		q:=d.deflist
		while q do
!	forall i,q in d.deflist do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			strcat(dest,q.name)
			q:=q.nextdef
		od

		strcat(dest,")")

	when trecord then
		if not expand then
			strcpy(dest,typename(m))
			return
		fi
		strcpy(dest,"")
		if expand<>2 then
			strcat(dest,typename(ttbasetype[m]))
		fi
		strcat(dest,"(")
		d:=ttnamedef[m]
		needcomma:=0

		q:=d.deflist

		while q, q:=q.nextdef do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
		od
		strcat(dest,")")

	when tvoid then			!must be a usertype that is not defined (as normal voids checked above)
		strcpy(dest,"void")

	when tuser then
		strcpy(dest,typename(m))
	when tproc then

		d:=ttnamedef[m]

		strcpy(dest,"proc(")
		q:=d.paramlist
		needcomma:=0
		while q<>nil do
			if needcomma then strcat(dest,",") fi
			needcomma:=1
			istrmode(q.mode,0,dest+strlen(dest))
			strcat(dest," ")
			strcat(dest,q.name)
			q:=q.nextdef
		od
		strcat(dest,")")
		if d.mode<>tvoid then
			istrmode(d.mode,0,dest+strlen(dest))
		fi

	when ttuple then
		strcpy(dest,"Tuple(")
		n:=ttlength[m]
		for i to n do
			istrmode(ttmult[m,i],0,dest+strlen(dest))
			if i<n then strcat(dest,",") fi
		od

		strcat(dest,")")

	when tbitfield then
		strcpy(dest,"bitfield")

	elsif ttbasetype[m]<tlast then
		strcpy(dest,"Alias for:")
		istrmode(tttarget[m],0,dest+strlen(dest))

	else
		println typename(m),STRMODE(TTBASETYPE[M])
		mcerror("NEWSTRMODE")
	esac
end

global proc addtoproclist(symbol d)=
	ref procrec pp

!CPL "ADDPROC",D.NAME

	pp:=pcm_alloc(procrec.bytes)

	if proclist=nil then
		proclist:=proclistx:=pp
	else
		proclistx.nextproc:=pp
		proclistx:=pp
	fi
!
	pp.def:=d
end

global proc addstatic(symbol d)=
	ref procrec pp
	pp:=pcm_alloc(procrec.bytes)

	if staticlist=nil then
		staticlist:=staticlistx:=pp
	else
		staticlistx.nextproc:=pp
		staticlistx:=pp
	fi

	pp.def:=d
end

global proc addconst(symbol d)=
	ref procrec pp
	pp:=pcm_alloc(procrec.bytes)

	if constlist=nil then
		constlist:=constlistx:=pp
	else
		constlistx.nextproc:=pp
		constlistx:=pp
	fi
	pp.def:=d
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

	++nunits

	if remainingunits-- then
		p:=unitheapptr
		++unitheapptr
		p.pos:=lx.pos
		p.moduleno:=currmoduleno
		return p
	fi

!need first or new heap
	p:=unitheapptr:=pcm_alloc(unitheapsize*unitrec.bytes)

	memset(p,0,unitheapsize*unitrec.bytes)
	remainingunits:=unitheapsize-1
	++unitheapptr
	p.pos:=lx.pos

	p.moduleno:=currmoduleno
	return p
end

global function createdupldef(symbol owner,symptr, int id)symbol=
!create new proc entry
!symptr is the generic st entry for proc's name
	symbol p,q

	p:=newstrec()

	p.name:=symptr.name
	p.namelen:=symptr.namelen
	p.symbol:=namesym
	p.owner:=owner
	p.nameid:=id

	p.nextdupl:=symptr.nextdupl
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

global function createnewmoduledef(symbol owner,symptr, int id=moduleid)symbol=
	return createdupldef(owner,symptr,id)
end

global function duplunit(unit p,int lineno=0)unit=
	unit q
	if p=nil then return nil fi

	q:=createunit0(p.tag)

	q^:=p^
	q.nextunit:=nil
	if q.hasa then q.a:=duplunit(q.a); q.hasa:=1 fi
	if q.hasb then q.b:=duplunit(q.b); q.hasb:=1 fi
	if q.hasc then q.c:=duplunit(q.c); q.hasc:=1 fi

	return q
end

global function checkblockreturn(unit p)int=
!p is any statement
!check p, or the last statement of a block, or the last statement of any
!nested block, a return, or is a unit yielding some value (as explicit return
!statement not needed)
! return 1/0 for return/not return
	unit e,wt
	int m,res

	if p=nil then return 0 fi

	m:=p.mode

	case p.tag
	when j_return then			!that's an easy one...
		return 1
	when j_stop then
		return 1
	when j_if then
		p.ifretflag:=1
		return checkblockreturn(p.b) and checkblockreturn(p.c)		!all branches must have a return

	when j_longif then
		e:=p.a
		p.ifretflag:=1
		while e, e:=e.nextunit do
			if not checkblockreturn(e.b) then
				return 0
			fi
		od
		return checkblockreturn(p.b)		!else must have return too
	when j_block then
		e:=p.a
		if e then
			while e and e.nextunit do
				e:=e.nextunit
			od
			return checkblockreturn(e)
		fi

	when j_case, j_switch, j_docase, j_doswitch then
		p.ifretflag:=1
		wt:=p.b
		while wt do
			if not checkblockreturn(wt.b) then
				return 0
			fi

			wt:=wt.nextunit
		od

		return checkblockreturn(p.c)		!else

	when j_assem then						!assume yes
		return 1
	esac

	if jisexpr[p.tag] and m<>tvoid then
		return 1							!any non-void expr allowed as return value
	else
		return 0
	fi
end

global function isconstunit(unit a)int=
	return a.isconst
end

global proc getownername(symbol d, ichar dest)=
	symbol owner

	owner:=d.owner

	if owner=nil or owner.nameid=programid then return fi
	getownername(owner,dest)
	strcat(dest,owner.name)
	strcat(dest,".")
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

	a:=ttsize[m]
	case a
	when 1,2,4,8,16 then
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
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	int64 a
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

global proc addlistunit(ref unit ulist,ulistx,unit p)=
!add unit p to unit structure ulist,^ulistx  which can be null
	if ulist^=nil then		!first
		ulist^:=ulistx^:=p
	else
		ulistx.nextunit:=p
	fi
	ulistx^:=p			!update end-of-list pointer
end

global function storemode(symbol owner, int m, int32 &pmode)int =
	ref typenamerec r

	if m>=0 then
		pmode:=m
		return m
	fi

	r:=&typenames[-m]

	if r.pmode=nil then
		r.owner:=owner
		pmode:=m
		r.pmode:=&pmode

	IF R.PMODE=NIL THEN SERROR("PMODE=NIL") FI

		return m
	fi

!Already one instance of this mode; need a new slot
	m:=newtypename(r.defa, r.defb)
	r:=&typenames[-m]

	r.owner:=owner
	pmode:=m
	r.pmode:=&pmode
	return m
end

global function gettypebase(int m)int=
	switch ttbasetype[m]
	when ti8,ti16,ti32 then ti64
!	when tu8,tu16,tu32 then tu64
	when tu8,tu16,tu32 then ti64

	when tr32 then tr64

	when tc8,tc16 then tc64
	else
		m
	end switch
end

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
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

global function getprocretmodes(unit p)symbol=
!p must be a call unit, for a proc with multiple values; at least one expected
!however, here it only populates retmodes with the available types
	symbol d
	unit a

	if p.tag<>j_callfn then txerror("multass/need multfn") fi
	a:=p.a

	case a.tag
	when j_name then
		return a.def
	else
		return ttnamedef[tttarget[a.mode]]
	esac
end

global function getmemmode(unit p)int =
	if p.memmode then
		return p.memmode
	fi
	return p.mode
end

global function isnum(int m)int=
!if if a numeric type, include decimal
!does not include short types
!should be base types, but numbers usually will be.

	if m>=tfirstnum and m<=tlastnum then
		return 1
	fi
	return 0
end

global function isboolunit(unit p)int=
!check that unit p has an inherent bool result, and return 1 if so, otherwise 0.
!This is done without checking types, so an EQ unit will always be bool
!Used by caller to determine whether an istrue op needs to be inserted

	case p.tag
	when j_cmp,j_andl, j_orl, j_notl, j_istruel, j_inrange, j_inset,
			j_cmpchain then
		return 1
	else
		0
	esac
end

global function getpclmode(int t)int u=
	u:=stdtopcl[ttbasetype[t]]
	return u
end

global proc domcl_assem(unit pcode)=
	return when not pcode or pcode.tag<>j_assem

	inf_assem:=1

	genmc(pcode.asmopcode, genasmopnd(pcode.a),genasmopnd(pcode.b))
	mccodex.cond:=pcode.cond

	case pcode.asmopcode
	when m_pcmpistri,m_pcmpistrm then
		if pcode.c=nil or pcode.c.tag<>j_const then gerror("pcmpistr/no imm") fi
		mccodex.c:=pcode.c.value

	esac

end

function genasmopnd(unit p)mcloperand ax=
	symbol d
	int offset,labno
	unit a				!expr: nil/name/const/(add name, const)
	unit x,y
	symbol e

	if p=nil then return nil fi

	case p.tag
	when j_assemreg then
		ax:=mgenreg(p.reg,p.regsize)

	when j_const then
		ax:=mgenint(p.value)

	when j_assemmem then
		a:=p.a
		d:=nil
		offset:=labno:=0

		if a then
			case a.tag
			when j_const then
				offset:=a.value
			when j_name then
				d:=a.def
				if d.nameid=labelid then
					labno:=fixasmlabel(d)
					d:=nil
				fi
			when j_bin then
				x:=a.a
				y:=a.b
				if x.tag=j_name and y.tag=j_const then
					d:=x.def
					if d.nameid=labelid then
						labno:=fixasmlabel(d)
						d:=nil
					fi
				else
					goto error
				fi
				offset:=(a.pclop in [kadd,kaddrefoff]|y.value|-y.value)
			when j_unary then
				if a.pclop<>kneg then merror("assume/unary") fi
				unless a.a.tag=j_const then gerror("-name") end
				offset:=-a.a.value
			when j_syscall then
MERROR("ASSEM/SYSFN?")
!				labno:=getsysfnlabel(a.opcode)

			else
error::
				cpl jtagnames[a.tag]
				gerror("Can't do memexpr")
			esac
		fi
		ax:=mgenindex(areg:p.reg, ireg:p.regix, scale:p.scale, size:ttsize[p.prefixmode],
			offset:offset, labno:labno, def:getpst(d))

	when j_name then
		d:=p.def
		if d.nameid=labelid then
			labno:=fixasmlabel(d)
			ax:=mgenlabel(labno)
		else
			ax:=mgenmemaddr(getpst(d))
		fi

	when j_assemxreg then
		ax:=mgenxreg(p.reg)
	when j_bin then				!assume add/sub
		x:=p.a
		y:=p.b
		if x.tag=j_name and y.tag=j_const then
			d:=x.def
			offset:=(p.pclop in [kadd,kaddrefoff]|y.value|-y.value)
			if d.nameid=labelid then
				labno:=fixasmlabel(d)
				ax:=mgenlabel(labno)
			else
				ax:=mgenmemaddr(getpst(d))
			fi
			ax.offset:=offset
		else
			gerror("ax:imm/add")
		fi
	else
		cpl jtagnames[p.tag]
		gerror("genasmopnd?")
	esac

	return ax

end

function fixasmlabel(symbol d)int=
!d.labelno contains the label number that is passed to PCL
!PCL maintains a labelmap[] array to convert such labels to renumbered labels
!Do that translation here, and return that new label
!Note: mapped label is stored as negative value to indicate it's been done
!Will return +ve mapped label

	if d.labelno=0 then
		CPL =D.NAME,D.OWNER.NAME
		gerror("FIXASMLABEL: zero")
	elsif d.labelno>0 then
		d.labelno:=-labelmap[d.labelno]
	fi
	return -d.labelno
end
=== mm_libsources.m 0 0 10/44 ===
global tabledata []ichar stdlibnames, []ichar stdlibtext =
	("msysp.m",			strinclude "msysp.m"),
	("mlibp.m",			strinclude "mlibp.m"),
	("mclib.m",			strinclude "mclib.m"),
	("mwindowsp.m",		strinclude "mwindowsp.m"),
	("mwindllp.m",		strinclude "mwindllp.m"),
end

global function findstdlib(ichar name)ichar=
	for i:=1 to stdlibnames.len do
		if eqstring(name,stdlibnames[i]) then
			return stdlibtext[i]
		fi
	od
	return nil
end

=== mm_modules.m 0 0 11/44 ===
!int ssfile=0

ichar headerpathx	= ""
ichar altpathx		= ""
ichar importpathx	= ""
ichar subprogpath	= ""
int dirline
int issyslib

[headervarnames.len]ichar headervars

macro mainmodule=headervars[hv_mainmodule]

byte freadma

global proc readprojectfile(ichar filename)=
	int fileno,headerdir, dir, oldsyslib
	ichar basefile

!	println "READPROJECT",filename


	if not checkfile(filename) then
		loaderror("Can't find main module or project: ##",filename)
	fi

	if eqstring(convlcstring(extractext(filename)),"ma") then
		filename:=loadmafile(filename)
	fi

	fileno:=getsupportfile(filename)

	basefile:=extractbasefile(sourcefilenames[fileno])

!CPL =BASEFILE
!STOP

	initheadervars()

	headermode:=1
	headerdir:=0

	moduletable[0].name:="PROGRAM"
	moduletable[0].fileno:=0

	stprogram:=createdupldef(nil,addnamestr("$prog"),programid)
	moduletable[0].stmodule:=stprogram
!
	addfirstsubprogram(basefile,fileno)

	startlex(fileno)

	do
		lex()
		skipsemi()

		case lx.symbol
		when kheadersym then
			headerdir:=1
			dir:=lx.subcode
			dirline:=lx.lineno
			lex()
			case dir
			when hdr_module then
				readmoduledir()
				mainmodule:=""
			when hdr_sysmodule then
				oldsyslib:=issyslib
				issyslib:=1
				readmoduledir()
				issyslib:=oldsyslib
				mainmodule:=""
			when hdr_subprog then
				altpathx:=""
				issyslib:=0
				readsubprogram()
			when hdr_syssubprog then
				if importpathx^=0 then
					importpathx:=headervars[hv_devpath]
				fi
				issyslib:=1
				readsubprogram()
			when hdr_import then
				if lx.symbol=namesym and eqstring(lx.symptr.name,"mlib") then
					recase hdr_sysimport
				fi
				issyslib:=0
				altpathx:=""
				readimport()
			when hdr_sysimport then
				if importpathx^=0 then
					importpathx:=headervars[hv_devpath]
				fi
				issyslib:=1
				altpathx:=""
				readimport()

			when hdr_altpath then
				altpathx:=fixpath(readvar())
			when hdr_importpath then
				importpathx:=fixpath(readvar())
				subprogpath:=(importpathx^|importpathx|headerpathx)

			when hdr_setvar then
				dosetvar()

			when hdr_showvar then
				doshowvar()

			when hdr_linkdll then
				addlib(readvar(),'D')
			when hdr_linklib then
				addlib(readvar(),'L')
			else
				loaderror("Hdr directive not ready:##",headerdirnames[dir])
			esac

			checksymbol(semisym)

		when semisym then
		when eofsym then
			exit

		else
			if sourcelevel then
				setmixedimport()
				unstacksource()
			else
				setmixedprogram(basefile)
				exit
			fi
		esac
	od


	if nmodules=0 then
		loaderror("No modules specified")
	fi

	addsyslib()


	addlib("msvcrt",'D')
	addlib("user32",'D')
	addlib("gdi32",'D')
	addlib("kernel32",'D')


!OS_GETCH()
!	LOADMODULES()
!	SHOWPROJECTINFO(); CPL
!
!
!	STOP
end

proc initheadervars=
	for i to headervars.len do
		headervars[i]:=""
	od

	headervars[hv_devpath]:="c:/mx/"
!	headervars[hv_devpath]:="c:/mx2/"
!	headervars[hv_devpath]:="c:/oldmx/"
	headervars[hv_mmpath]:=pcm_copyheapstring(extractpath(sysparams[1]))
	subprogpath:=headerpathx:=headervars[hv_hdrpath]:=pcm_copyheapstring(sourcefilepaths[1])
	headervars[hv_windows]:="1"
	mainmodule:="1"

end

global proc showprojectinfo(filehandle dev)=
	ref modulerec pm
	ref subprogrec ps
	static ichar tab="    "

	println @dev,"Project Structure:"
	println @dev,"---------------------------------------"
	println @dev,"Modules",nmodules
	for i to nmodules do
		pm:=&moduletable[i]

		if i>1 and pm.subprogno<>moduletable[i-1].subprogno then
			println @dev
		fi

		print @dev, tab,i:"2",pm.name:"16jl", "Sys:",pm.issyslib, "Path:",pm.path,
			"Sub:",subprogtable[pm.subprogno].name,"Fileno:",pm.fileno
		if pm.stmacro then
			print @dev," Alias:",pm.stmacro.name
		fi
		println @dev
	od
	println @dev

	println @dev,"Subprograms",nsubprogs
	for i to nsubprogs do
		ps:=&subprogtable[i]
		println @dev, tab,i,ps.name,"Sys:",ps.issyslib, "Path:",ps.path, "Fileno:",ps.fileno
		if ps.startmodule then
			print @dev, tab,tab
			for j:=ps.startmodule to ps.endmodule do
				print @dev, moduletable[j].name,$
			od
			println @dev
		fi
	od
	println @dev

!	println @dev,"Sourcefiles",nsourcefiles
!	for i to nsourcefiles do
!		println @dev, tab,i,sourcefilenames[i]
!		println @dev, tab,tab,sourcefilepaths[i]
!		println @dev, tab,tab,sourcefilespecs[i]
!		println @dev, tab,tab,=strlen(sourcefiletext[i])
!	od
!	println @dev

	println @dev,"Link files",nlibfiles
	for i to nlibfiles do
		println @dev, tab, libfiles[i]:"16Jjl",(libtypes[i]='D'|"DLL"|"LIB")
	od
	println @dev

!	println @dev,"Header Variables:"
!	for i to headervars.len do
!		fprintln @dev,"\t#: #",headervarnames[i],headervars[i]
!	od
!	println @dev
!	println @dev,"---------------------------------------"

!	println @dev,"Symboltable:"
!	symbol d:=stprogram.deflist
!	while d, d:=d.nextdef do
!		ichar id
!		case d.nameid
!		when moduleid then id:="Mod"
!		when subprogid then id:="Sub"
!		else id:="---"
!		esac
!		fprintln @dev,"    # # (m#, s#)",d.name,id,d.moduleno, d.subprogno
!	od


end

proc readmoduledir=
!at symbol following 'module'
	ichar modulename, modulefilespec
	symbol stalias

	checksymbol(namesym)
	modulename:=modulefilespec:=pcm_copyheapstring(lx.symptr.name)
	convlcstring(modulename)
	stalias:=nil

	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
		lex()
		if lx.symbol=namesym then
			stalias:=lx.symptr
			lex()
		else
			stalias:=addnamestr(readvar())
		fi
!CPL =STALIAS.NAME
	fi

	if checkwhen() then
		addmodule(modulename,stalias)
	fi
end

function checkwhen:int=
	int index

	return 1 when lx.symbol<>kwhensym

	lex()
	checksymbol(kheadervarsym)
	index:=lx.subcode
	lex()

	return eqstring(headervars[index],"1")
end

proc addmodule(ichar modulename, symbol stalias=nil)=
!	Add new module with given name (should be on the heap)
	ref modulerec pm
	ref subprogrec ps

	for i to nmodules do
		if eqstring(moduletable[i].name, modulename) then
			loaderror("Duplicate module name: # (Line:#)",modulename,strint(dirline))
		fi
	od

	for i to nsubprogs do
		if eqstring(subprogtable[i].name, modulename) then
			loaderror("Clashing subprog/module name: # (Line:#)",modulename,strint(dirline))
		fi
	od

	if nmodules>=maxmodule then
		loaderror("Too many modules",modulename)
	fi
	pm:=&moduletable[++nmodules]

	pm.name:=pcm_copyheapstring(modulename)
	pm.subprogno:=nsubprogs

	pm.stmodule:=stmodule:=createnewmoduledef(stprogram,addnamestr(modulename))

	pm.path:=(altpathx^|altpathx|subprogpath)
	pm.issyslib:=issyslib

	stmodule.moduleno:=nmodules
	moduletosub[nmodules]:=nsubprogs
!	stmodule.subprogno:=nsubprogs

	ps:=&subprogtable[nsubprogs]

	if ps.startmodule=0 then
		ps.startmodule:=ps.endmodule:=nmodules
	else
		ps.endmodule:=nmodules
	fi

!CPL =PM.STALIAS

	if stalias then

		pm.stmacro:=getduplnameptr(stprogram, stalias, macroid)
!CPL =STPROGRAM.DEFLIST,STPROGRAM.DEFLISTX
!		symbol stmacro:=createnewmoduledef(stprogram, pm.stalias, macroid)
!CPL =STPROGRAM.DEFLIST,STPROGRAM.DEFLISTX
		adddef(stprogram, pm.stmacro)
		pm.stmacro.paramlist:=nil
		pm.stmacro.code:=createname(stmodule)
	fi
end

proc addsubprogram(ichar subprogname,int fileno)=
!	Add new subprogram with given name (should be on the heap)
	ref subprogrec ps

	if nsubprogs>=maxsubprog then
		loaderror("Too many subprograms",subprogname)
	fi

	for i to nsubprogs do
		if eqstring(subprogtable[i].name, subprogname) then
			loaderror("Duplicate subprog name: # (Line:#)",subprogname,strint(dirline))
		fi
	od
	ps:=&subprogtable[++nsubprogs]

	ps.name:=pcm_copyheapstring(subprogname)

	subprogpath:=ps.path:=(importpathx^|importpathx|subprogpath)

	stsubprog:=createnewmoduledef(stprogram,addnamestr(subprogname),subprogid)
	stsubprog.subprogno:=nsubprogs
	ps.stsubprog:=stsubprog
!	stsubprog.subprogno:=nsubprogs
	ps.fileno:=fileno
	ps.issyslib:=issyslib
end

proc addfirstsubprogram(ichar progname, int fileno)=
	ref subprogrec ps

	nsubprogs:=1
	ps:=&subprogtable[1]
	ps.name:=pcm_copyheapstring(progname)
	ps.path:=headerpathx

	stsubprog:=createnewmoduledef(stprogram,addnamestr(progname),subprogid)
	stsubprog.subprogno:=1
	ps.stsubprog:=stsubprog
!	stsubprog.subprogno:=nsubprogs
	ps.fileno:=fileno
end

proc readsubprogram=
	ichar subprogname, subprogfilespec

	checksymbol(namesym)
	subprogname:=subprogfilespec:=pcm_copyheapstring(lx.symptr.name)
	convlcstring(subprogname)

	lex()

	if lx.symbol=kwhensym then
		lex()
		lex()
	fi

	addsubprogram(subprogname,0)

end

proc readimport=
	ichar subprogname, path
	int fileno

	checksymbol(namesym)

	subprogname:=pcm_copyheapstring(lx.symptr.name)
	convlcstring(subprogname)

	lex()

	path:=(importpathx^|importpathx|subprogpath)

	fileno:=getsupportfile(subprogname,"m", path)
	addsubprogram(subprogname,fileno)

	stacksource(fileno)
end

function readvar:ichar s=
	case lx.symbol
	when stringconstsym then
		s:=pcm_copyheapstring(lx.svalue)
	when kheadervarsym then
		s:=headervars[lx.subcode]
	when namesym then
		s:=lx.symptr.name
	else
		loaderror("readvar/bad expr")
		s:="?"
	esac
	lex()
	return s
end

function fixpath(ichar path)ichar=
	[300]char newpath
	int n:=strlen(path)
	if n=0 then return path fi
	if (path+n-1)^ in ['\\','/'] then
		return path
	fi
	strcpy(newpath,path)
	strcat(newpath,"\\")
	return pcm_copyheapstring(newpath)
end

proc dosetvar=
	int index

	checksymbol(kheadervarsym)
	index:=lx.subcode
	lex()
	checksymbol(eqsym)
	lex()
	headervars[index]:=readvar()
end

proc doshowvar=
	if lx.symbol=stringconstsym then
		println lx.svalue
	else
		checksymbol(kheadervarsym)
		println headervarnames[lx.subcode]+3,"=",headervars[lx.subcode]
	fi
	lex()
end

proc setmixedprogram(ichar basefile)=
	[100]char name
	int oldns
!	loaderror("SIMPLE PROG SETUP NOT DONE")

!CPL "MIXEDPROG",=NSUBPROGS
	print @name,"$",,basefile
	oldns:=nsubprogs
	nsubprogs:=1
	addmodule(name)
	nsubprogs:=oldns
	moduletable[nmodules].fileno:=1
end

proc setmixedimport=
	[100]char name
!	loaderror("SIMPLE PROG SETUP NOT DONE")

!CPL "MIXED IMPORT", SUBPROGTABLE[NSUBPROGS].NAME, =NSUBPROGS

	print @name,"$",,subprogtable[nsubprogs].name
	addmodule(name)
	moduletable[nmodules].fileno:=subprogtable[nsubprogs].fileno
end

global proc loadmodules =
	ref modulerec pm

	for i to nmodules do
		pm:=&moduletable[i]
!		PRINTLN "LOADING:",I,PM.NAME
		loadmodule(pm)
	od
end

proc loadmodule(ref modulerec pm)=
	[300]char filespec
	ichar path

	if pm.fileno then
!		println pm.name,"Already loaded"
		return
	fi

!CPL "LOADING............."
!	fprintln "   # (path:#) syslib:#",&.filespec, pm.path, pm.issyslib

	path:=pm.path
	if path^=0 and pm.issyslib then
		path:=f"c:\mx\"
	fi

	pm.fileno:=getsupportfile(pm.name, "m", path, issyslib:pm.issyslib)
end

proc addsyslib=
!add in syslib if mlib not already included

	if msyslevel=0 then return fi

	for i to nsubprogs do
!		if subprogtable[i].issyslib then return fi
		if eqstring(subprogtable[i].name,"mlib") then return fi
	od

	issyslib:=1
	importpathx:=headervars[hv_devpath]
	altpathx:=""
	if msyslevel=1 then
		addsubprogram("mlibtemp",0)
		addmodule("msystemp")
		return
	fi

	addsubprogram("mlib",0)
	addmodule("msysp")
!	addmodule("msysnew")
	addmodule("mlibp")
	addmodule("mclib")
	addmodule("mwindowsp")
	addmodule("mwindllp")
!	addmodule("mwindllx")

end

global proc addlib(ichar libname, int libtype='D')=
!CPL "ADDLIB",LIBNAME,LIBTYPE:"C"
	for i to nlibfiles do
		if eqstring(libfiles[i],libname) then return fi
	od
	if nlibfiles>=maxlibfile then
		loaderror("Too many libs")
	fi
	libfiles[++nlibfiles]:=libname
	libtypes[nlibfiles]:=libtype
end

function readfileline(ichar s)ichar =
!s points into a string representing an entire file, which should be 0-terminated
!read the line s until the next eol or eof, into a line buffer
!return a pointer to the next line
	[2048]char str
	ichar t:=str
	int n, c

	n:=0
	docase c:=s++^
	when 0 then
		--s
		exit
	when 10 then
		exit
	else
		if n<str.len then
			t++^:=c
		fi
	end docase

	t^:=0

	readln @&.str
	return s
end

function findnextlineheader(ichar s)ichar=
!starting from s, find next sequence of lf followed by ===
!return nil if eof found
!otherwise return pointer to just after the ===
	int c

	docase c:=s++^
	when 0 then
		return nil
	when 10 then
		if s^='=' and (s+1)^='=' and (s+2)^='=' then
			return s+3
		fi
	end docase

	return nil
end

function loadmafile(ichar filespec, ichar builtinstr=nil)ichar=
!load ma file from disk
!unless filespec is nil, then direct from builtinstr
!return name of lead module
	ichar s,t
	[100]char name
	[300]char newfilespec
	int sys,support

	freadma:=1

	if filespec then
		s:=cast(readfile(filespec))
		if s=nil then							!file not found on disk
			loaderror("Can't find MA file ##",filespec)
		fi
		strcpy(newfilespec,extractpath(filespec))
	else
		s:=builtinstr
		newfilespec[1]:=0
	fi

!need to scan file pickuping the file headers, and populating sourctables

	s:=readfileline(s+3)
	readstr(name,'n')
	if not eqstring(name,"ma") then
		loaderror("MA: bad header")
	fi

	--s					!point to previous lf

	if nsourcefiles then
		loaderror("QA/table not empty")
	fi

	s:=findnextlineheader(s)

	do
		if s=nil then
			loaderror("Unexpected EOF in MA file")
			exit
		fi
		s:=readfileline(s)

		readstr(name,'n')
		read sys,support

!		println "Found file",name
		if eqstring(name,"end") then
			exit
		fi
		if nsourcefiles>=maxsourcefile then
			loaderror("Too many files in MA")
		fi

		t:=findnextlineheader(s)
		if t=nil then
			loaderror("MA error")
		fi

		++nsourcefiles
		sourcefilenames[nsourcefiles]:=sourcefilespecs[nsourcefiles]:=pcm_copyheapstring(name)
		sourcefilesizes[nsourcefiles]:=t-s-3
		sourcefiletext[nsourcefiles]:=s
		sourcefilepaths[nsourcefiles]:=""
		sourcefilespecs[nsourcefiles]:=""
		issyslibfile[nsourcefiles]:=sys
		issupportfile[nsourcefiles]:=support
		s:=t
	od
!
	for i to nsourcefiles do
!		cpl i,sourcefilenames[i],sourcefilesizes[i]
		(sourcefiletext[i]+sourcefilesizes[i])^:=0	
	od

!finally, set inputfile to the first file found
	strcat(newfilespec, sourcefilenames[1])
	return pcm_copyheapstring(newfilespec)
end
=== mm_name.m 0 0 12/44 ===
symbol currstproc
int allowmodname=0
int noexpand, noassem
int macrolevels

const maxmacroparams=50
[maxmacroparams]symbol macroparams
[maxmacroparams]symbol macroparamsgen
[maxmacroparams]unit macroargs
int nmacroparams
int nmacroargs

global proc rx_typetable=
	symbol d

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			d:=ttnamedef[i]
			if d.baseclass then
				do_baseclass(d)
			fi
		fi
	od
end

global proc rx_unit(symbol owner, unit p)=
	symbol d
	unit a,b
	int n,oldnoexpand,oldnoassem,oldtag,useparams

	a:=p.a
	b:=p.b
	mlineno:=p.pos

!cpl "RXU",JTAGNAMES[P.TAG]

	switch p.tag
	when j_name then
!CPL "NAME"
		resolvename(owner,p)
!CPL "NAME1",NAMENAMES[P.DEF.NAMEID]
		if P.TAG=J_NAME AND p.def.nameid=macroid and not noexpand then
			++macrolevels
			expandmacro(p,p,nil)
			rx_unit(owner,p)
			--macrolevels
		fi

	when j_keyword then
		rx_unit(owner,b)		!do param value only

	when j_dot then
!		if b.tag=j_name then
!			d:=resolvetopname(owner,b.def,b.moduleno,fmodule:0,fdoambig:0)
!		fi
!CPL "JDOT"
		resolvedot(owner,p)

	when j_callproc, j_callfn then
		oldtag:=p.tag

		if a.tag=j_name then			!can expand possible macro if params not ready
			oldnoexpand:=noexpand; noexpand:=1
			rx_unit(owner,a)
			noexpand:=oldnoexpand
		else
			rx_unit(owner,a)
		fi

		rx_unitlist(owner,b)

		if a.tag=j_name then
			d:=a.def
			case d.nameid
			when typeid then		!change to type conversion
				p.tag:=j_convert
				storemode(owner,d.mode,p.convmode)
				p.a:=b
				p.b:=nil; p.hasb:=0
				if b.nextunit then
					p.a:=createunit1(j_makelist,b)
					n:=0
					while b do
						++n
						b:=b.nextunit
					od
					p.a.length:=n
				fi
			when macroid then
				++macrolevels
				if d.deflist then			!macro uses params
					expandmacro(p,a,b)
					b:=nil
					useparams:=0
				else						!macro has no params
					expandmacro(p,a,nil)
					useparams:=1
				fi

				rx_unit(owner,p)
				--macrolevels

				if useparams and p.tag not in [j_callproc, j_callfn] then
					insertunit(p,oldtag)
					p.b:=b					!note b may be nil
					p.hasb:=1
				FI

			else
				if d.mode=tvoid then
					p.tag:=j_callproc
				fi
			esac
		fi

	when j_andl, j_orl then
		rx_unit(owner,a)
		rx_unit(owner,b)
		if not isboolunit(a) then insertunit(a,j_istruel); a.pclop:=kistruel fi
		if not isboolunit(b) then insertunit(b,j_istruel); b.pclop:=kistruel fi

	when j_istruel then
	doistruel::
		rx_unit(owner,a)

		if isboolunit(a) then
			deleteunit(p,a)
		fi
		goto doabc

	when j_notl then
		rx_unit(owner,a)
		if a.tag=j_notl then
			deleteunit(p,a)
			p.tag:=j_istruel
			p.pclop:=kistruel
			a:=p.a
			goto doistruel
		fi
		if not isboolunit(a) then
			insertunit(a,j_istruel); a.pclop:=kistruel
			a:=p.a
		fi
		goto doabc

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

	else
doabc::
		if p.hasa then rx_unitlist(owner,a) fi
		if p.hasb then rx_unitlist(owner,b) fi
		if p.hasc then rx_unitlist(owner,p.c) fi
	endswitch
end

global function rx_module(int n)int=
	modulerec m
	symbol stmodule, d
	int globalflag,status

	currmoduleno:=n

	rx_passdef(stprogram,moduletable[n].stmodule)

	return 1
end

global proc rx_deflist(symbol owner,p)=
	symbol pstart:=p
	while p do
		rx_passdef(owner,p)
		p:=p.nextdef
	od
end

global proc rx_passdef(symbol owner,p)=
	symbol d

	case p.nameid
	when moduleid,dllmoduleid then
		rx_deflist(p,p.deflist)

	when procid then
		rx_deflist(p,p.deflist)
		currstproc:=p
		rx_unit(p,p.code)
		currstproc:=nil

	when dllprocid then
		rx_deflist(p,p.deflist)

	when constid,staticid,frameid,paramid then
		if p.atvar then
			rx_unit(owner,p.equivvar)
		fi
		if p.code then
			rx_unit(owner,p.code)
		fi
	when typeid then
		rx_deflist(p,p.deflist)

	else
	esac
end

proc rx_unitlist(symbol owner, unit p)=
	while p do
		rx_unit(owner,p)
		p:=p.nextunit
	od
end

global function resolvetopname(symbol owner,stnewname,int moduleno,allowmod)symbol =
!stnewname points to a symrec with generic nullid
!This is a top-level name (left-most name of any dotted sequence, or standalone name)

!Search through all the duplicate symrecs (all names with identical names have symrecs that
!are linked together, always starting with a nullid symrec) looking for the best match

!moduleno is the module where the currently generic name is encountered
!(derived from a unit if in an expression, or an STREC if a type in a declaration)

	int extcount, subprogno
	symbol p,q, powner,extdef,moddef
	[10]symbol ambiglist

!CPL "TOP1",STNEWNAME.NAME,=MODULENO

	if owner.nameid=procid then
		q:=owner.deflist
		while q, q:=q.nextdef do
			if q.firstdupl=stnewname then		!immediate match
				return q
			fi
		od
	fi

	p:=stnewname.nextdupl
	subprogno:=moduletosub[moduleno]

	extcount:=0
	extdef:=moddef:=nil

	while p, p:=p.nextdupl do						!p is next candidate
		powner:=p.owner								!the owner of that entry

!CPL =POWNER.NAME
		switch powner.nameid
		when moduleid then							!candidate is file-scope item
!CPL "OWNER IS MODULE"
			if powner.moduleno=moduleno then		!same module
!CPL "SAME"
				return p
			elsif p.isglobal then	!matches an external module
!CPL "IS GLOBAL"
				if moduletosub[powner.moduleno]=subprogno or		!within same subprog
					 p.isglobal=export_scope or
					 p.isimport then 				!visible outside subprog
					++extcount			!if an ext match is closest, there can only be one
					extdef:=p
!CPL "ADDED TO EXT LIST"
					if extcount<ambiglist.len then
						ambiglist[extcount]:=extdef
					fi
				fi
			fi

		when typeid then					!only for code inside a record def
			if powner=owner or powner=owner.owner then		!immediate match
				return p					!looks at 2 nested record levels only
			fi

		when programid then					!p is a module
			case p.nameid
			when moduleid, subprogid then	!match a module/subprog name
!CPL "MODDEF"
				moddef:=p
			when macroid then
				return p

			esac

		endswitch
	od

	if allowmod and moddef then
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
	return nil
!	return moddef				!return nil, or any matching module/subprog
end

global proc resolvename(symbol owner, unit p)=
!p is a name tag inside given owner
!resolve name
!report error if unresolved, unless mode is not void. Then an unresolved
!name is added as a frame (assumes this is a proc)

	symbol d,e
	unit q
	int moduleno, mode

	d:=p.def
	moduleno:=p.moduleno

	if d.nameid<>nullid then			!assume already resolved
		return
	fi

	e:=resolvetopname(owner,d,moduleno,allowmodname)

!CPL "RESOLVENAME",=E

	if not e then
		mode:=tvoid
		case p.avcode
		when 'I', 'T', 'S' then mode:=ti64
		when 'L','A' then mode:=tany
		esac

		if mode=tvoid then
			rxerror_s("Undefined: #",d.name,p)
		else
			e:=addframevar(owner,d,moduleno,mode)
			e.lineno:=p.lineno
			if mode<>tany then e.islet:=1 fi
		fi
	fi

	e.used:=1

	if e.nameid=paramid and e.parammode=out_param then
		p.tag:=j_ptr
		p.a:=createname(e)
		p.hasa:=1; p.hasb:=p.hasc:=0
	else
		p.def:=e			!update link in kcode
	fi

end

global function finddupl(symbol d, pdupl)symbol=
!trying to resolve a field name, by scanning a dupllist headed by pdupl
!which ought to point to nullid entry
!d will be the owner of the matching entry

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl

	while pdupl do
		if pdupl.owner=d then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

global function finddupl_sub(symbol d, pdupl)symbol=
!version of finddupl where d is a subprog
	int subprogno

	if pdupl.nameid<>nullid then		!assume already resolved
		return pdupl
	fi
	pdupl:=pdupl.nextdupl
	subprogno:=d.subprogno

	while pdupl do
		if moduletosub[pdupl.owner.moduleno]=subprogno then
			return pdupl
		fi
		pdupl:=pdupl.nextdupl
	od
	return nil
end

proc resolvedot(symbol owner,unit p)=
	unit lhs,rhs
	symbol d,e,t,f
	int m,moduleno,subprogno,oldallowmod

	moduleno:=p.moduleno
	subprogno:=moduletosub[moduleno]
	lhs:=p.a
	rhs:=p.b
	e:=rhs.def				!p.b will be a name type (could perhaps be stored as p.def)

!CPL "DOT1"
	oldallowmod:=allowmodname
	allowmodname:=lhs.tag=j_name
	rx_unit(owner,lhs)
	allowmodname:=oldallowmod

!	rx_unit(owner,lhs)
!CPL "DOT2"
!PRINTUNIT(P)

	case lhs.tag
	when j_name then
		d:=lhs.def
		case d.nameid
		when moduleid, typeid, procid, typeid then

			e:=finddupl(d,e)
			if e then
				if d.nameid=moduleid then
					if moduletosub[e.moduleno]<>subprogno then
						if e.isglobal<>export_scope then
							rxerror_s("Need export to import '#'",e.name)
						fi
					elsif e.moduleno<>moduleno then
						if not e.isglobal then
							rxerror_s("Need global to import '#'",e.name)
						fi
					fi
				fi
domodule::
				p.tag:=j_name			!convert to dot to name
				p.a:=p.b:=nil
				p.hasa:=p.hasb:=0
				p.def:=e
				case e.nameid
				when enumid then
				when constid then
				esac
			else
				rxerror_s("Can't resolve .#",p.b.def.name,p)
			fi

		when frameid, staticid, paramid then		!.x applied to normal var
			m:=d.mode
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
				p.b.def:=e
			else
				rxerror_s("Not a field: #",rhs.def.name)
			fi
		when subprogid then
			e:=finddupl_sub(d,e)
			if e then
				if moduletosub[e.moduleno]<>subprogno then
					if e.isglobal<>export_scope then
						rxerror_s("Need export to import '#'",e.name)
					fi
				fi
				goto domodule
			else
				rxerror_s("Can't resolve sub.#",p.b.def.name,p)
			fi

		esac

	else
!Can't fully resolve at this time; leave to next pass
		unless e.nextdupl then
			rxerror_s("Not a field: #",e.name)
		endunless
	esac
end

proc fixmode(ref typenamerec p)=
!p refers to a negative mode that is a typename index
!fix that up if possible
	ref int32 pmode
	symbol a,d,e,f,owner
	int m,moduleno

	pmode:=p.pmode

	m:=-pmode^					!typename index

	d:=owner:=p.owner
	while d.nameid<>moduleid do d:=d.owner od
	moduleno:=d.moduleno

	a:=p.defa
	d:=p.defb

	if a=nil and d then			!simple type name V
		e:=resolvetopname(owner,d,moduleno,0)

	elsif d=nil and a then		!typeno
		rxerror("Fixmode can't do typeof yet")
	else						!assume a.d type reference
		e:=resolvetopname(owner,a,moduleno,0)
		if e then
			f:=e.deflist
			e:=nil
			while f do
				if f.nameid=typeid and f.firstdupl=d then
					e:=f
					exit
				fi
				f:=f.nextdef
			od

		fi

	fi

	if e and e.nameid=typeid then
		pmode^:=e.mode

	else
		rxerror_s("2:Can't resolve tentative type: #",d.name)
	fi
end

global proc fixusertypes=
	ref typenamerec p
	int npasses,notresolved,m,zerosizes
	symbol d

	npasses:=0
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
			println "Type phase errors - check these user types:"

			for i to ntypenames do
				p:=&typenames[i]

				if p.pmode^<0 then
					d:=p.defb
					if d=nil then d:=p.defa fi
					println "	",d.name
				fi
			od

			rxerror("Fixtypes: too many passes (cyclic ref?)")
		fi

	until notresolved=0
end

global proc fixblockparams=
!make sure all block params are changed to references

	ref procrec pp
	symbol d,e

	RETURN

	pp:=proclist
	while pp do
		d:=pp.def
		e:=d.deflist
		while e do
			if e.nameid=paramid then
				if ttbasetype[e.mode] in [trecord, tarray] then
					e.parammode:=out_param
					e.mode:=createrefmode(d,e.mode,0)
				fi
			fi
			e:=e.nextdef
		od
	
		pp:=pp.nextproc
	od
end

global function resolve_equiv_name(symbol owner,p)symbol=
!@p or @p+offset used for a field offset
!owner is record type of which it should be a member
!currently, p is at strec that might be null
!return matching fieldid name
	if p.nameid=fieldid then
		return p
	fi

	RXERROR("RESOLVE EQUIV FIELD/COMPLEX")

	return nil
end

function addframevar(symbol owner, d, int moduleno, mode)symbol=
!owner should be a proc; d is a generic st entry
!add framewith the name of d and given mode to the proc
	symbol e
	e:=getduplnameptr(owner,d,frameid)
	storemode(owner,mode,e.mode)
	adddef(owner,e)
	return e
end

function copylistunit(unit p)unit=
	unit q

	unit plist,plistx
	plist:=plistx:=nil
	while p do
		q:=copyunit(p)
		addlistunit(&plist,&plistx,q)
		p:=p.nextunit
	od
	return plist
end

function copyunit(unit p)unit=
	unit q
	symbol d

	if p=nil then return nil fi

!need to quickly check if a name unit is a macroparam

	if p.tag=j_name then
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
	if q.hasa then q.a:=copylistunit(q.a); q.hasa:=1 fi
	if q.hasb then q.b:=copylistunit(q.b); q.hasb:=1 fi
	if q.hasc then q.c:=copylistunit(q.c); q.hasc:=1 fi

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

proc expandmacro(unit p, a, b)=
!a is a macro name unit, b is a macro parameter list (rx-processed), which
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

	if macrolevels>10 then
		rxerror("Too many macro levels (recursive macro?)")
	fi

	d:=a.def

!CPL "EXPANDMACRO",D.NAME

!First step: get list of macro formal parameters

	pm:=d.paramlist
	nmacroparams:=0
	while pm do
		if nmacroparams>=maxmacroparams then
			rxerror("macro param overflow")
		fi
		macroparams[++nmacroparams]:=pm
		macroparamsgen[nmacroparams]:=pm.nulldef
		pm:=pm.nextparam
	od

!now get macro args into a list
	nmacroargs:=0

	while b do
		if nmacroargs>=maxmacroparams then
			rxerror("macro arg overflow")
		fi
		macroargs[++nmacroargs]:=b
		b:=b.nextunit
	od

	if nmacroargs<nmacroparams then
		PRINTLN =NMACROARGS, NMACROPARAMS
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

proc duplfield(symbol owner,p,q)=
!p is strec of an existing field, const etc
!q is a newly created strec with the same id and name
!copy the relevant fields of p to q

	if p.code then
		serror("DUPLFIELD")
	fi

!Need to copy whatever are relevant attributes

	q.atfield:=p.atfield
	q.flags:=p.flags

	q.uflags:=p.uflags		!for .uflags
	storemode(owner,p.mode,q.mode)
end

proc do_baseclass(symbol p)=
!p is class name, which has a baseclass, do the copying necessary for
!inheriting fields
	symbol d,e,newd,dbase
	int normalexit

	dbase:=ttnamedef[p.baseclass]
	d:=dbase.deflist

	while d do				!for each element of base class
		e:=p.deflist

		normalexit:=1
		while e do			!for each element of new class
			if eqstring(d.name,e.name) then
				normalexit:=0
				exit
			fi
			e:=e.nextdef
		od
		if normalexit then
!duplicate d in this class; keep it simple for now
!(procs will need a more elaborate duplication, and really needs to share code)
			case d.nameid
			when procid,linkid then
				newd:=getduplnameptr(p,d,linkid)
				newd.equivfield:=d
			else
				newd:=getduplnameptr(p,d,d.nameid)
				duplfield(p.owner,d,newd)
			esac
			adddef(p,newd)
		fi
		d:=d.nextdef
	od
end
=== mm_parse.m 0 0 13/44 ===
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

const maxforloops=10
[maxforloops]ref strec forindexvars
int nforloops

global filehandle docfile

global function parsemodule(int n)int=
	ref modulerec pm
	ref strec p, owner
	int globalflag,status

	initparser()

	pm:=&moduletable[n]
	currmoduleno:=n

	stmodule:=pm.stmodule
!CPL "PARSING",=N, PM.NAME,=MODULETABLE[N].FILENO,=STMODULE.MODULENO

	currproc:=stmodule

	stsubprog:=subprogtable[stmodule.moduleno].stsubprog
	currsubprog:=stsubprog

	startlex(pm.fileno)

	owner:=stmodule

	lex()
	status:=readmoduledefs(owner)

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


	globalflag:=local_scope
	callbackflag:=0

	do
!PS("RM1")
		switch lx.symbol
		when kglobalsym then
			if globalflag then serror("global global?") fi
			globalflag:=lx.subcode
			lex()

		when kprocsym,kfunctionsym then	!todo
			readprocdef(owner,globalflag,callbackflag)
			callbackflag:=0
			globalflag:=local_scope

		when stdtypesym,namesym,lsqsym,krefsym,kicharsym,ktypeofsym,
			kdictsym,kslicesym then
			readvardef(owner,globalflag,0,staticid, 0)
			globalflag:=local_scope

		when kmutsym then
			lex()
			readvardef(owner,globalflag,0,staticid,kmutsym)
			globalflag:=local_scope

		when kletsym then
			lex()
			readvardef(owner,globalflag,0,staticid,kletsym)
			globalflag:=local_scope

		when kimportmodulesym then
			readimportmodule(owner)

!		when kimportpathsym then
!			lexchecksymbol(stringconstsym)
!			lex()
!
!		when kmapmodulesym then
!			repeat
!				lex()
!			until lx.symbol in [semisym,eofsym]
!
		when ktypesym then
			readtypedef(owner,globalflag)
			globalflag:=local_scope

		when kconstsym then
			readconstdef(owner,globalflag)
			globalflag:=local_scope

		when kclasssym,krecordsym then
			readclassdef(owner,globalflag)
			globalflag:=local_scope

		when kenumsym then
			lex()
			readenumtype(owner,0,globalflag)
			globalflag:=local_scope

		when ktabledatasym then
			readtabledef(owner,globalflag)
			globalflag:=local_scope

		when docstringsym then
			adddocstring(lx.svalue)
			lex()

!		when kimportsym then
!			if globalflag then serror("glob/import?") fi
!			lex()
!			if lx.symbol=mulsym then
!				lex()
!			fi
!			checksymbol(namesym)
!
!!need to check that the import has been done (in case import stmt is badly placed)
!!!(note: can't detect a badly placed import if that lib has been loaded elsewhere)
!!			dimport:=lx.symptr
!!!			name:=mapimport(dimport.name)
!!			name:=mapimport(dimport.name)
!!
!!			for i:=1 to nmodules do
!!				if eqstring(name, moduletable[i].name) then
!!					stimport:=moduletable[i].stmodule
!!					exit
!!				fi
!!			else
!!				CPL lx.symptr.name
!!				serror("Import stmt out of position?")
!!			od
!			lex()
!!
!!			domappedalias(dimport,stimport)
!			if lx.symbol=namesym and eqstring(lx.symptr.name,"as") then
!	lex()
!	lex()
!!				readimportalias(dimport)
!			fi

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
			globalflag:=local_scope

		when kheadersym then
			repeat
				lex()
!PS("SKIP HDR")
			until lx.symbol=semisym

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

global proc skipsemi=
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
			fprint @(&.str+strlen(&.str))," (from line #)",startline iand 16777215
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

	ulist:=ulistx:=nil

	if istypestarter() then
		m:=readtypespec(owner)
	elsif k then
		m:=tauto
	else
		serror("Readvar?")
	fi

	nvars:=0
	while lx.symbol=namesym do

		++nvars
		stname:=getduplnameptr(owner,lx.symptr,varid)

		stname.isglobal:=isglobal

		stname.isstatic:=isstatic
		stname.islet:=(k=kletsym)
		if varid=dllvarid then
			stname.isimport:=1
		fi

		adddef(owner,stname)
		if varid=staticid then
			addstatic(stname)
		fi

		lex()

		if lx.symbol=colonsym then
			if m<>tauto then serror("Mixed var T x:T") fi
			lex()
			m:=readtypespec(owner)
		fi

		storemode(owner,m,stname.mode)

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
			if lx.symbol=kemptysym then
				lex()
				if varid<>frameid then serror("empty: not frame") fi
				p:=createunit1(j_empty,createname(stname))
				addlistunit(&ulist,&ulistx,p)
			else
				stname.code:=readunit()
				stname.equals:=1
				if varid=frameid then
					p:=createunit2(j_assign,createname(stname),stname.code)
					p.initlet:=1
					addlistunit(&ulist,&ulistx,p)
				fi
			fi

		elsif lx.symbol=atsym then
			if k=kletsym then serror("let@") fi
			lex()
			stname.atvar:=1
			stname.equivvar:=readunit()
		elsif k=kletsym then
			serror("let needs :=/=")
		fi

		if lx.symbol<>commasym then
			exit
		fi
		lex()
	od

	if nvars=0 then
		serror("No vars declared")
	fi
	return ulist
end

proc readconstdef(ref strec owner,int isglobal=0)=
!at 'const' symbol
	int nconsts,deft,m
	ref strec stname

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
		stname.code:=readconstexpr(1)

		m:=deft

		storemode(owner,m,stname.mode)
		++nconsts

		stname.isglobal:=isglobal

		adddef(owner,stname)
		if isglobal=export_scope and stname.name^<>'$' then
			addconst(stname)
		fi

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

	elsif lx.symbol=intconstsym and nextlx.symbol=colonsym then
		plower:=createconstunit(lx.value,lx.subcode)
		plower.istrueconst:=1
		lex()
		lex()

	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=rbracksym then	!operator constant
		p:=createunit0(j_operator)
		p.opcindex:=lx.subcode
		lex()
		lex()
		return p
	elsif symboloptypes[lx.symbol]=bin_op and nextlx.symbol=assignsym then	!operator:= constant
		p:=createunit0(j_operator)
		p.pclop:=symbolgentoops[lx.symbol]
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
		p.length:=0
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
		if nextlx.symbol=rbracksym then		!means one-element list
			lex()
			lex()
			p:=createunit1(j_makelist,p)
			p.length:=length
			p.b:=plower
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
		p.length:=length
		p.b:=plower
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
			return createunit3(j_if,fixcond(p),q,r)
		when rbracksym then
			lex()
			return createunit3(j_if,fixcond(p),q,nil)

		esac

!assume selectx expression
		addlistunit(&ulist,&ulistx,q)	!start with one-element list
		checksymbol(commasym)
		if nextlx.symbol<>barsym then		!(n|a,| using one-element list; not useful but allow it...
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
		ulistx^.nextparam:=p
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
		if nextlx.symbol=ktypesym then
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
	storemode(currproc,t,p.convmode)
	return p
end

function readopc:unit=			!READOPC
!op sym seen just before a term
	unit p,q,r
	int tag,opc,firstsym

	firstsym:=lx.symbol

	case lx.symbol
	when mathsopsym then
		tag:=j_unary
		opc:=lx.subcode
	when maths2opsym then
		tag:=j_bin
		opc:=lx.subcode
	else
		tag:=j_unary
		opc:=symbolgenops[firstsym]
	esac

	lex()
	case firstsym
	when addsym then			!ignore +
		return readterm2()
	when subsym then			!convert minus to negate
		opc:=kneg
	when minsym,maxsym,maths2opsym then
		p:=readterm2()

		if p.tag=j_makelist then
			if p.length<>2 then serror("Needs (x,y)") fi
			q:=p.a
			r:=q.nextunit
			q.nextunit:=nil
			p:=createunit2(j_bin,q,r)
			p.pclop:=opc
			return p
		else		!assume single operand
			SERROR("READOPC/SINGLE OPND?")
			return createunit1(opc,p)

		fi
	else
		if symboloptypes[firstsym]=bin_op then
			serror("Can't be used as unary op")
		fi

	esac

	if lx.symbol=assignsym then	!op:=, not normally allowed inside expressions
		lex()
		tag:=j_unaryto
		case firstsym
		when subsym then
			opc:=knegto
		else
			opc:=symbolgentoops[firstsym]
			if opc=0 then
				serror("op:= not available")
			fi
		esac
	fi

	p:=createunit1(tag,q:=readterm2())

	p.pclop:=opc

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
		if pformat.tag=j_null then
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
	ref modulerec currmodule:=&moduletable[currmoduleno]

	switch lx.subcode
	when j_cvnil then
		p:=createconstunit(0,tref)
		lex()
		return p

	when j_cvpi then
		p:=createconstunit(int64@(3.14159'265358'979'3238'4626'433'832),treal)
		lex()
		return p

	when j_cvlineno then

		p:=createunit0(j_cvlineno)
		lex()
		return p

	when j_cvstrlineno then
		getstrint(lx.lineno,&.str)

	when j_cvmodulename then
		strcpy(str,stmodule.name)

	when j_cvfilename then

		strcpy(str,sourcefilepaths[currmodule.fileno])

	when j_cvfunction then
		strcpy(&.str,currproc.name)

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

	when j_cvversion then
		strcpy(&.str,"Compiler:BX Experimental")

	when j_cvtrue,j_cvfalse then
		p:=createconstunit(lx.subcode=j_cvtrue,tint)
		lex()
		return p
	
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
	storemode(currproc,m,p.convmode)

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
		if lx.symbol=ktosym then lex() fi

		case lx.symbol
		when kprocsym,kfunctionsym then	!function pointer being created
			t:=readrefproc(owner,typedefx,fflang)

		when kfflangsym then
			fflang:=lx.subcode
			goto retry
!*!	elsif lx.symbol=namesym and lx.subcode=asmopcodesym and lx.symptr.index=m_label then
!*!		t:=createrefmode(owner,tlabel,typedefx)
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
			if lx.symbol in [commasym,semisym] then
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
			plower:=createunit1(j_unary,duplunit(p))
			plower.pclop:=klwb
			pupper:=createunit1(j_unary,duplunit(p))
			pupper.pclop:=kupb
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

		if q.tag=j_makerange then		!convert into a discrete slice
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
		when propsym then
	doprop::
			p:=createunit1(j_unary,p)
			p.pclop:=lx.subcode
			lex()
		when bitfieldsym then
			p:=createunit1(j_bitfield,p)
			p.bfcode:=lx.subcode
			lex()
		when ktypesym then			!.type, convert to .gettype
			case p.tag
			when j_typeconst then			!int.type=>int

			else
				p:=createunit1(j_typeof,p)
			esac
			lex()

		when maxsym then
			lx.subcode:=kmaxvalue
			goto doprop

		when minsym then
			lx.subcode:=kminvalue
			goto doprop
		when stdtypesym then
			if p.tag=j_typeconst and lx.subcode=trange then
				q:=createunit2(j_makerange,
					createunit1(j_unary,p),
					createunit1(j_unary,p))
				q.a.pclop:=kminvalue
				q.b.pclop:=kmaxvalue
			else
				error
			fi
			lex()
			p:=q

		else
	error::
			serror("Unknown dot suffix")
		endswitch
	od
	return p
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
	nforloops:=0

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

	stproc.code:=readsunit()

	checkbeginend(closesym,kwd,startline)

	if yieldseen then
		stproc.nameid:=generatorid
	fi

	if ndocstrings and docfile and stproc.isglobal=export_scope then
		println @docfile,"proc",stproc.name
		for i to ndocstrings do
			println @docfile,docstrings[i]
			pcm_free(docstrings[i],strlen(docstrings[i]+1))
		od
		println @docfile

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

	int kwd,varparams,try_level, nparams, nretvalues, isthreaded
	[maxtuplesize]int retmodes
	int prettype@&retmodes

	ichar metadata, truename
	ref strec pequiv, stproc, owner, paramlist,nameptr

	kwd:=lx.symbol				!remember keyword
	isthreaded:=lx.subcode

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
	if insidedllimport then isglobal:=program_scope fi
	stproc.isthreaded:=isthreaded

	if truename then
		stproc.truename:=truename
	fi

	if stproc.name^='$' and eqstring(stproc.name,"$init") then
		moduletable[stmodule.moduleno].stinitproc:=stproc
	fi

	adddef(procowner,stproc)
	if stproc.nameid=dllprocid then
		stproc.isimport:=1
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

	stproc.paramlist:=paramlist
	stproc.nretvalues:=nretvalues

	case nretvalues
	when 0 then
		stproc.mode:=tvoid
	when 1 then
		storemode(procowner,retmodes[1],stproc.mode)
	else
		stproc.mode:=createtuplemode(procowner,(&.retmodes,nretvalues),0)
	esac

	if lx.symbol=atsym then			!equivalence
		lexchecksymbol(namesym)
	SERROR("READPROCDEF @")
		lex()
		stproc.atvar:=1
	fi

	stproc.code:=nil

	case fflang
	when clangff,windowsff then
!		if procowner.nameid<>dllmoduleid then
!			println stproc.name,fflangnames[fflang]
!			serror("FF should be in dll import")
!		fi
	else			!assume this language
		case procowner.nameid
		when moduleid then
		when dllmoduleid then
			serror("Need FF specifier")
		esac
	esac
	stproc.isglobal:=isglobal
	stproc.varparams:=varparams
	stproc.fflang:=fflang

	if procowner=stmodule and 
		(stproc.namelen=5 and eqstring(stproc.name,"start")) or 
		(stproc.namelen=4 and eqstring(stproc.name,"main")) then
		stproc.isglobal:=export_scope
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
	int parammode, pmode, m, pmprefix

	[30]char str
	stlist:=stlistx:=nil
	pmode:=tvoid
	nparams:=0
	pmprefix:=0
	parammode:=var_param

	if fflang=0 then fflang:=mlangff fi

	if lx.symbol in [koutsym,addrsym] then
		parammode:=out_param
		pmprefix:=1
		lex()
	elsif lx.symbol=insym then
		parammode:=in_param
		pmprefix:=1
		lex()
	fi

	if lx.symbol=namesym and nextlx.symbol in [commasym,rbracksym] then	!types only
		pmode:=readtypespec(procowner)
typesonly::
		return readparams_types(procowner,owner,fflang,varparams,nparams,pmode,parammode)
	else
		pmode:=readtypespec(procowner)
		if lx.symbol in [commasym,rbracksym] then			!types only
			goto typesonly
		fi
	fi

!types+names
	if pmprefix then
		serror("&/out must be applied to param name")
	fi

	goto gotmode

	do										!expect type of name at start of loop
		if istypestarter() then				!assume new mode
			pmode:=readtypespec(procowner)
		fi
gotmode::

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
		stname.parammode:=parammode
		addlistparam(&stlist,&stlistx,stname)
		parammode:=var_param

		case lx.symbol
		when assignsym then
			lex()
dodefvalue::
			stname.code:=readunit()
			stname.equals:=1
			stname.optional:=1
		when eqsym then
			lex()
			goto dodefvalue
		esac

		case lx.symbol
		when commasym then
			lex()
		when rbracksym then
			exit
		else
			serror("nameparams1")
		esac
	od

return stlist
end

function readparams_types(ref strec procowner,owner,int fflang,&varparams,&nparams,
			int pmode, parammode)ref strec=
!read types-only non-empty parameter list, only for ffi
!positioned at first symbol after '('
	ref strec stlist, stlistx, stname
	int firstparam,m

	[30]char str
	stlist:=stlistx:=nil
	stname:=nil
	nparams:=0
	goto gotmode

	do
		if lx.symbol=ellipsissym then
			varparams:=nparams+1		!pos of 1st varparam
			lex()
			checksymbol(rbracksym)
			exit
		fi

		pmode:=readtypespec(procowner)
gotmode::
		++nparams
		print @&.str,"$",,nparams
		stname:=getduplnameptr(owner,addnamestr(&.str),paramid)
		adddef(owner,stname)
		if parammode=out_param then
			m:=createrefmode(procowner,pmode)
		else
			m:=pmode
		fi

		storemode(owner,m,stname.mode)
		stname.parammode:=parammode
		addlistparam(&stlist,&stlistx,stname)
		parammode:=var_param

		case lx.symbol
		when assignsym,eqsym then
			lex()
			stname.code:=readunit()
			stname.equals:=1
		when namesym then
			serror("Can't mixed unnamed/named params")
		endcase

		case lx.symbol
		when commasym then
			lex()
			if lx.symbol=addrsym then
				parammode:=out_param
				lex()
			fi
		when rbracksym then
			exit
		else
			serror("typeparams3")
		endcase

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
	unit q

	switch lx.symbol
	when kwhensym then
		lex()
		return createunit2(j_if,fixcond(readunit()),createunit1(j_block,p))
	when kunlesssym then
		lex()
		q:=createunit1(j_notl,fixcond(readunit()))
		q.pclop:=knotl
		return createunit2(j_if, q,createunit1(j_block,p))
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
	pcond:=fixcond(readsunit())
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
			pcond:=fixcond(readunit())
			checksymbol(kthensym)
			lex()
			pthen:=readsunit()
			pelsif:=createunit2(j_elsif,pcond,pthen)
			pelsif.pos:=pos2
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
		p.pos:=pos1
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
	p.pos:=pos1
	return p
end

function readgoto(int gototag=j_goto)unit=	!READGOTO
	ref strec d
	unit p

	if lx.subcode=1 then		!go used
		lexchecksymbol(ktosym)
	fi
	lex()

	if lx.symbol=namesym and nextlx.symbol<>ptrsym and nextlx.symbol<>lsqsym and
		nextlx.symbol<>dotsym then			!assume simple label
		p:=createname(lx.symptr)

		lex()
	else
		serror("GOTO LABEL EXPR")
	fi

	return readcondsuffix(createunit1(gototag,p))
end

function readunless:unit=
	int pos
	unit pcond, pthen, pelse, p,q
	pos:=lx.pos
	lex()
	pcond:=fixcond(readsunit())
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
	p:=createunit3(j_if,q:=createunit1(j_notl,pcond),pthen,pelse)
	q.pclop:=knotl
	p.pos:=pos
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
			p.pos:=pos2
			if p.tag=j_makerange then rangeused:=1 fi
			addlistunit(&pwhen,&pwhenx,p)
			if lx.symbol<>commasym then exit fi
			lex()
		od
		checksymbol(kthensym)
		lex()
		pthen:=readsunit()
		pwhenthen:=createunit2(j_whenthen,pwhen,pthen)
		pwhenthen.pos:=pos2
		addlistunit(&pwhenlist,&pwhenlistx,pwhenthen)
	od

!	if opc=j_switch and not rangeused then
!		if nwhen<=8 then
!			opc:=j_case
!		fi
!	fi

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
	p.pos:=pos1
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
		p.length:=1
	else
		p:=createunit0(j_return)
		p.length:=0
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
	p.pos:=pos
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
	if currproc.nameid<>procid then id:=staticid fi

	p:=createunit3(j_to,pcount,pbody,createname(getavname(currproc,id)))
!p:=createunit2(j_to,pcount,pbody)
	p.pos:=pos
	return p
end

function readwhile:unit=
	int pos
	unit pcond, pbody, pincr, p

	pos:=lx.pos
	lex()

	pcond:=fixcond(readsunit(1))
	pincr:=nil

	if lx.symbol=commasym then
		lex()
		pincr:=readsunit(1)
	fi

	checksymbol(kdosym)
	lex()
	pbody:=readsunit()

	if lx.symbol=kstepsym then
		if pincr then serror("Double incr") fi
		lex()
		pincr:=readsunit()
	fi

	checkend(kendsym,kwhilesym,kdosym)
	lex()

	p:=createunit3(j_while,pcond,pbody,pincr)
	p.pos:=pos

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
	pcond:=fixcond(readunit())
	p:=createunit2(j_repeat,pbody,pcond)
	p.pos:=pos

	return p
end

function readloopcontrol:unit=
	int opc
	unit p

	opc:=lx.subcode

	lex()
	if lx.symbol=namesym and eqstring(lx.symptr.name,"all") then
		lex()
		p:=createunit1(opc,createconstunit(0,tint))

	elsif exprstarter[lx.symbol] then
		p:=createunit1(opc,readconstexpr(1))
	else
		p:=createunit1(opc,createconstunit(1,tint))
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
		case lx.symbol
		when commasym then		!assume extra comma, meaning nogap
			addlistunit(&printlist,&printlistx, createunit0(j_nogap))
		when dollarsym then		!assume extra comma, meaning nogap
			addlistunit(&printlist,&printlistx, createunit0(j_space))
			lex()

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
				s:=expr.strptr
				iconvucn(expr.strptr,expr.length)

				addlistunit(&printlist,&printlistx,q:=createstringconstunit(s,expr.length))
			fi
			addlistunit(&printlist,&printlistx,p)
		esac
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

!

		p:=createunit2(j_assign,p,pread)

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
! for var [:= expr] to/downto expr [by expr] [when expr] do stmts [else stmts] end/od
! for var[,var] in/inrev expr [when expr] do stmts [else stmts] end/od *FORALL*
! for var in/inrev expr.bounds [when expr] do stmts [else stmts] end/od
! for var in/inrev <rangeexpr> [when expr] do stmts [else stmts] end/od

!AV codes:
!	I	loop index, always i64; will be 'i' (declared or not declared) or autovar
!	L	forall local variable; will be 'x' (declared or not declared); type is variable

	int pos, opc, kwd
	unit pindex, plocal				!for index; for index,local
	unit pfrom, pto, pstep, ptoinit	!for INDEX:=FROM to/downto TO [by STEP]/ INDEX in FROM..TO
	unit plist, passign				!for INDEX in/inrev LIST (also LIST.BOUNDS)
	unit pcond, pbody, pelse
	unit p
!
	pos:=lx.pos
	lex()						!skip 'for' kwd

	plocal:=nil
	ptoinit:=nil
	pindex:=readname()

	if nforloops>=maxforloops then
		serror("Too many for-loops")
	fi
	for i to nforloops do
		if forindexvars[i]=pindex.def then
			serror("Re-using nested loop index")
		fi
	od
	forindexvars[++nforloops]:=pindex.def

	if lx.symbol=commasym then
		lex()
		plocal:=readname()
	fi

	opc:=j_forup
	pstep:=nil
	pcond:=nil

	if lx.symbol in [insym, inrevsym] then				!assume forall
		if lx.symbol=j_inrev then
			opc:=j_fordown				!tentative; may be changed to forall
		fi
		lex()

		plist:=readunit()

		if plist.tag=j_unary and plist.pclop=kbounds then
			pfrom:=getrangelwbunit(plist.a)
			pto:=getrangeupbunit(plist.a)
		elsif plist.tag=j_makerange then
			pfrom:=plist.a
			pto:=plist.b
		else
			opc:=(opc=j_forup|j_forall|j_forallrev)
			pfrom:=getrangelwbunit(duplunit(plist))
			pto:=getrangeupbunit(duplunit(plist))
		fi

	else
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
			if pstep.tag=j_const then
				if pstep.value=1 then		!by 1
					pstep:=nil
				fi
			fi
		fi
	fi

	if lx.symbol=kwhensym then
		lex()
		pcond:=fixcond(readunit())
	fi
	checksymbol(kdosym)
	lex()
	pbody:=readsunit()
	pelse:=nil

	if lx.symbol=kelsesym then
		lex()
		pelse:=readsunit()
	fi
	checkend(kendsym,kforsym,kdosym)
	lex()

!deal with complex limit
!problem: autovar for STEP only created when there is an autovar for TO

	if pcond<>nil then
		pbody:=makeblock(createunit2(j_if,pcond,pbody))
	fi
	pbody.nextunit:=pelse

!forup/down layout
!	a:	pindex
!	b:	pfrom/pto/pstep
!	c:	pbody

!forall/rev layout
!	a:	pindex/plocal/pfrom/pto
!	b:	plist/passign
!	c:	pbody

	case opc
	when j_forup, j_fordown then
		if plocal then serror("for i,x?") fi
		pindex.avcode:='I'
		if pto.tag not in [j_const, j_name] then
			plocal:=createname(getavname(currproc))
			plocal.avcode:='I'
			ptoinit:=createunit2(j_assign, plocal, pto)
			pindex.nextunit:=ptoinit
			pto:=plocal
		fi

		pfrom.nextunit:=pto
		pto.nextunit:=pstep

		p:=createunit3(opc, pindex, pfrom, pbody)

	else										!assume forall/rev

		if plocal=nil then						!only for x
			plocal:=pindex
			pindex:=createname(getavname(currproc))
		fi
		pindex.avcode:='I'
		plocal.avcode:='L'
		pindex.nextunit:=plocal
		plocal.nextunit:=pfrom
		pfrom.nextunit:=pto

		passign:=createunit2(j_assign,duplunit(plocal),
					createunit2(j_index,duplunit(plist),duplunit(pindex)))
		plist.nextunit:=passign

		p:=createunit3(opc, pindex, plist, pbody)

	esac

	p.pos:=pos
	--nforloops
	return p
end

function readname:unit p=
	p:=readterm2()
	if p.tag<>j_name then serror("Name expected") fi
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
	storemode(owner,t,sttype.mode)

	if t>=0 then
		if ttisallnum[t] then
			tttarget[m]:=t
		elsif ttisref[t] then
		elsecase ttbasetype[t]
		when tarray then
		when tslice then
		when trecord then
		when tenum then
		else
			tttarget[m]:=t
		fi
	else
		storemode(owner,t,tttarget[m])
	fi

	if t>=0 then
		copyttvalues(m,t)
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
			unionstr_copy(&stname.uflags,&unionpend)
			unionstr_concat(&unionstring,&unionpend)
			unionstr_clear(&unionpend)
		else
			unionstr_clear(&stname.uflags)
		fi
		unionlastvar:=stname			!filled in from outside with 'E' codes

		adddef(owner,stname)

		lex()

		case lx.symbol
		when atsym then
			lex()
			stname.atfield:=1
			stname.equivfield:=readequivfield(owner)

		when datsym then
			lexchecksymbol(intconstsym)
			case lx.value
			when 1,2,4,8,16 then
				stname.align:=lx.value
			when 0 then
				stname.align:=255
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
				stbitfield.mode:=tbitfield
				adddef(owner,stbitfield)

				stbitfield.atfield:=1
				stbitfield.equivfield:=stname

				lexchecksymbol(colonsym)
				lexchecksymbol(intconstsym)
				stbitfield.bitfieldwidth:=lx.value
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

global proc readtabledef(ref strec owner,int isglobal=0)=
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
			enumtypename:=lx.symptr.name
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
		ltype:=readtypespec(owner)
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
			tabledataname:=stgen.name		!allow to be picked up by $ lx.symbol
			lex()
			if lx.symbol=eqsym then
				lex()
				nextenumvalue:=readconstint()
			fi
			enumvalues[nrows]:=nextenumvalue

			stenum:=getduplnameptr(owner,stgen,constid)
			stenum.mode:=tint
			stenum.code:=createconstunit(nextenumvalue,tint)
			stenum.isglobal:=isglobal
			adddef(owner,stenum)
			if isglobal=export_scope then
				addconst(stenum)
			fi

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

		stvar:=getduplnameptr(owner,varnameptrs[i],staticid)
		stvar.code:=createunit1(j_makelist,plist[i])
		stvar.code.length:=nrows

		storemode(owner,varlisttypes[i],stvar.mode)
		stvar.isglobal:=isglobal

		adddef(owner,stvar)
		addstatic(stvar)
	od
end

global proc readclassdef(ref strec owner,int isglobal)=
!at 'class' symbol
!read enough of the class to be able to generate export data
	int kwd, baseclass, m, startline, closesym, mrec, normalexit,isrecord, align
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

	align:=0
	if lx.symbol=atsym then
		if lx.subcode=0 then
			lex()
			align:=readconstint()
		else
			lex()
		fi
		align:=1
	fi



	sttype:=getduplnameptr(owner,nameptr,typeid)
	adddef(owner,sttype)
	m:=createusertype(sttype)

	mrec:=createrecordmode(owner, m)
	storemode(owner,mrec,sttype.mode)

	storemode(owner,baseclass,sttype.baseclass)
	sttype.align:=align

	closesym:=checkbegin(1)

	startline:=getcurrline()

	readclassbody(sttype,kwd)

	checkbeginend(closesym,kwd,startline)

	sttype.isglobal:=isglobal
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

		if owner.isimport then
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

	when kenumsym then
		lex()
		readenumtype(owner,0,0)

	when ktabledatasym then
		readtabledef(owner,0)

	when kmacrosym then
		readmacrodef(owner,0)

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
			case unionstr_last(&unionlastvar.uflags)
			when 'E','*' then
			else
				unionstr_append(&unionlastvar.uflags,'*')
			esac
			unionstr_append(&unionlastvar.uflags,'E')
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
	int isanon, index, startline, closesym, knownindex
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
	knownindex:=1
	index:=1

	while lx.symbol=namesym do
		nameptr:=lx.symptr
		lex()
		if lx.symbol=eqsym then	!= follows
			lex()
			pindex:=readunit()
			knownindex:=0
			if pindex.tag=j_const then
				knownindex:=1
				index:=pindex.value
			fi
		fi

		if not isanon then
			stname:=getduplnameptr(owner,nameptr,enumid)
		else
			stname:=getduplnameptr(enumowner,nameptr,constid)
		fi

		if knownindex then
			pindex:=createconstunit(index,ti64)
			stname.code:=pindex
			++index
		else
			stname.code:=pindex
			pindex:=createunit2(j_bin,pindex,pone)
			pindex.pclop:=kadd
		fi
		stname.mode:=tint

		if not isanon then
			adddef(owner,stname)
		else
			adddef(enumowner,stname)
		fi

		stname.isglobal:=isglobal

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

proc readimportmodule(ref strec owner)=
!at 'importmodule' symbol
	int isnew,startline,closesym, libtype
	ref strec d,stname,stname0

	if insidedllimport then serror("nested importdll") fi
	libtype:=lx.subcode

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
		stname:=getduplnameptr(stmodule,stname,dllmoduleid)
!		if eqstring(stname.name,"sys") then
!			stsysmodule:=stname
!		fi
		adddef(stmodule,stname)
		
		addlib(stname.name, libtype)

	stname.dllindex:=nlibfiles
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
			readtypedef(owner,program_scope)

		when kconstsym then
			readconstdef(owner,program_scope)

		when kclasssym,krecordsym then
			readclassdef(owner,program_scope)

		when kmutsym then
			lex()
			readvardef(owner,program_scope,0,dllvarid, kmutsym)

		when stdtypesym,namesym,lsqsym,krefsym,kicharsym,ktypeofsym,
			kdictsym,kslicesym then
			readvardef(owner,program_scope,0,dllvarid, 0)

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

	checksymbol(namesym)
	d:=lx.symptr
	lex()

	p:=owner.deflist
	while p do
		if eqstring(p.name,d.name) then
			return p
		fi

		p:=p.nextdef
	od
	cpl d.name
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

	storemode(owner,retmodes[1],stproc.mode)
	stproc.nretvalues:=nretvalues

	ttnamedef[m]:=stproc
	stproc.fflang:=fflang

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

!look at the start of a line first

	if lx.symbol=namesym and nextlx.symbol in [colonsym,dcolonsym] then	!normal label
		p:=createunit0(j_labeldef)
		stname:=getduplnameptr(currproc,lx.symptr,labelid)
		p.def:=stname
		adddef(currproc,stname)
		lex()			!skip colon
		if oneline then
			lex()
		fi
		return p

	elsif lx.symbol=mulsym then		!*name	macro invocation
		lexchecksymbol(namesym)
		pname:=createname(lx.symptr)
		pname.pos:=lx.pos

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
		opc:=m_andx
	doop::
		p:=createunit0(j_assem)
		p.asmopcode:=opc
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
			p.asmopcode:=lx.symptr.index

		when jmpccsym then
			p.asmopcode:=m_jmpcc
			p.cond:=lx.symptr.index
		when setccsym then
			p.asmopcode:=m_setcc
			p.cond:=lx.symptr.index
		when movccsym then
			p.asmopcode:=m_cmovcc
			p.cond:=lx.symptr.index
		else
	PS("ASM")
			serror("x64 op expected")
		esac

		lex()
	else
	PS("ASM")
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
		case lx.symptr.subcode
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

!		if lx.symbol=addsym and nextlx.symbol=namesym and nextlx().symptr.subcode=regsym then
		if lx.symbol=addsym and nextlx.symbol=namesym and nextlx.symptr.subcode=regsym then
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
	length:=astringlength
	to astringlength do
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
	p.length:=length
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
		if p.tag=j_keyvalue then ++nkeyvalues fi
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
	p.length:=length
	return p
end

function istypestarter:int=
	if typestarterset[lx.symbol] then return 1 fi
	if lx.symbol=namesym then				!name ...
		case nextlx.symbol
		when namesym then					!name name
			return 1
		when addrsym then
			return 1
		esac
	fi
	return 0
end

function readassignment:unit p=
	int pos,opc

	if lx.symbol=namesym and nextlx.symbol=assignsym then
			pos:=lx.pos
			p:=createname(lx.symptr)
			lex()
			lex()
			p:=createunit2(j_assign,p, readassignment())
			p.pos:=lx.pos
			return p
	fi

	p:=readorterms()

	if (opc:=lx.symbol)=assignsym then
		pos:=lx.pos
		lex()
		if lx.symbol=kemptysym then
			p:=createunit1(j_empty, p)
			lex()
		else
			p:=createunit2(j_assign,p,readassignment())
		fi
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

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=korlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_orl,p,readandterms())
		p.pclop:=korl
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

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=kandlto
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_andl,p,readcmpterms())
		p.pclop:=kandl
		p.pos:=pos
	od

	return p
end

function readcmpterms:unit p=
	int pos,opc,n
	unit ulist,ulistx,q
	[4]byte genops

	p:=readinterms()

	if lx.symbol not in [eqsym,cmpsym] then
		return p
	fi

	ulist:=ulistx:=p
	p:=createunit1(j_cmpchain,p)
	n:=0				!n counts operand after the first
!	memset(&genops,0,genops.bytes)
	clear genops

	doswitch lx.symbol
	when eqsym, cmpsym then
		++n
		if n>genops.len then serror("cmpchain: Too many items") fi
		genops[n]:=lx.subcode

		pos:=lx.pos
		lex()

		q:=readinterms()
		addlistunit(&ulist,&ulistx,q)
		q.pos:=pos
	else
		exit
	end doswitch

	if n=1 then
		p.tag:=j_cmp
		q:=p.a
		p.pclop:=genops[1]
		p.b:=q.nextunit
		q.nextunit:=nil
		p.hasb:=1
	else
		p.cmpgenop:=genops
	fi

	return p
end

function readinterms:unit p=
	int pos,opc

	p:=readrangeterm()

	doswitch lx.symbol
	when insym, notinsym then
		opc:=lx.subcode

		pos:=lx.pos
		lex()

		p:=createunit2(j_bin,p,readrangeterm())
		p.pclop:=opc
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
	int pos,sym, tag, genop

	p:=readmulterms()

	doswitch sym:=lx.symbol
	when addsym, subsym, iandsym, iorsym, ixorsym, minsym, maxsym then
		pos:=lx.pos
		genop:=lx.subcode
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_bin,p,readmulterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readmulterms:unit p=
	int pos,sym

	p:=readpowerterms()

	doswitch sym:=lx.symbol
	when mulsym, divsym, idivsym, iremsym, shlsym, shrsym then
		pos:=lx.pos
		lex()

		if lx.symbol=assignsym then
			lex()
			p:=createunit2(j_binto,p,readassignment())
			p.pclop:=symbolgentoops[sym]
			p.pos:=pos
			exit
		fi

		p:=createunit2(j_bin,p,readpowerterms())
		p.pclop:=symbolgenops[sym]
		p.pos:=pos
	else
		exit
	end doswitch

	return p
end

function readpowerterms:unit p=
	int pos

	p:=readterm2()

	while lx.symbol=powersym do
		pos:=lx.pos
		lex()
		p:=createunit2(j_bin,p,readpowerterms())
		p.pclop:=kpower
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
		when kincr then opc:=kloadincr
		when kdecr then opc:=kloaddecr
		esac
		lex()
		p:=createunit1(j_incr,p)
		p.pclop:=opc

	when anddotsym then
		lexchecksymbol(lsqsym)
		lex()
		q:=readunit()
		if q.tag=j_makerange then
			p:=createunit2(j_anddotslice,p,q)
		else
			p:=createunit2(j_anddotindex,p,q)
		fi
		checksymbol(rsqsym)
		lex()

	else
		exit
	enddoswitch

	p.pos:=pos

	return p
end

function readterm:unit=
	unit p,q,r
	ref char pbyte
	word64 a
	int oldipl,opc,oldinrp,pos,shift,t,length
	u128 aa

	pos:=lx.pos

	switch lx.symbol
	when namesym then
		if nextlx.symbol=atsym then		!type-punning with user type
			p:=readcast()
		else
			p:=createname(lx.symptr)
			p.pos:=lx.pos
			lex()
		fi

	when intconstsym,realconstsym then
		p:=createconstunit(lx.value,lx.subcode)
		p.istrueconst:=1
		lex()

	when stringconstsym then
		p:=createstringconstunit(lx.svalue,-1)
		lex()

	when astringconstsym then
		p:=makeastring()
		lex()

	when decimalconstsym then
		SERROR("DEC CONST")

	when charconstsym then
		length:=strlen(lx.svalue)
		if length>16 then serror("Char const too long") fi
		if length>8 then
			aa:=0
			memcpy(&aa,lx.svalue,length)
			p:=createconstunit(cast(&aa),tu128)
		else
			a:=0
			if length then
				memcpy(&a,lx.svalue,length)
			fi
			p:=createconstunit(a,tc64)
		fi
		p.istrueconst:=1
		lex()

	when lbracksym then
!CPL "READLB"
		p:=readlbrack()

	when stdtypesym,krefsym,kicharsym,ktypeofsym then
		p:=readcast()

	when addsym, subsym, minsym, maxsym, abssym, inotsym,
		mathsopsym, sqrtsym, sqrsym, maths2opsym,signsym then
		p:=readopc()

	when notlsym then
!		if nextlx().symbol=assignsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(j_notl, readterm2())
			p.pclop:=knotl
		fi

	when istruelsym then
!		if nextlx().symbol=assignsym then
		if nextlx.symbol=assignsym then
			p:=readopc()
		else
			lex()
			p:=createunit1(j_istruel, readterm2())
			p.pclop:=kistruel
		fi

	when lsqsym then
		p:=readset()

	when incrsym then
		opc:=lx.subcode
		lex()
		p:=createunit1(j_incr,readterm2())
		p.pclop:=opc

	when ksprintsym then
		p:=readsprint()

	when ksreadsym,ksreadlnsym then
		p:=readsread()

	when addrsym then
		lex()
		p:=createunit1(j_addrof,readterm2())
		if p.a.tag=j_callfn then
			if p.a.b then
				serror("Params not allowed")
			fi
			p.a:=p.a.a			!lose the call
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
			p:=createunit1(j_unary,dollarstack[ndollar])
			p.pclop:=kupb
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
		if lx.symbol=rbracksym and q.tag=j_makerange then
			r:=q.b
			q:=q.a
		else
			checksymbol(commasym)
			lex()
			r:=readunit()
			checksymbol(rbracksym)
		fi
		lex()

		q:=createunit2(j_bin,p,q)
		q.pclop:=kmax
		p:=createunit2(j_bin,q,r)
		p.pclop:=kmin

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
		currproc.asmused:=1
		assemmode:=1
		if lx.subcode=0 then
			p:=readassemline()
		else
			p:=readassemblock()
		fi
		assemmode:=0

	when ksyscallsym then
		p:=createunit0(j_syscall)
		p.fnindex:=lx.subcode
		lex()

	when kstrincludesym then
		lex()
		p:=createunit1(j_strinclude,readterm2())

	when kemptysym then
		lex()
		p:=createunit1(j_empty, readterm2())

	else
		cpl symbolnames[lx.symbol],=LX.SYMBOL
		serror("readterm?")
	endswitch

	p.pos:=pos
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
			if nextlx.symbol in [lbracksym, atsym, dotsym] then		!is a cast etc
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
				r:=q.nextunit						!unlink from this block first
				q.nextunit:=nil
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

		when ktabledatasym then
			readtabledef(currproc,0)

		when eofsym then
			cpl currproc.name
			serror("Unexpected EOF in proc")

!these are needed to check for an empty sunit preceding
		when rbracksym,kthensym,kelsifsym,kelsesym,kuntilsym,kwhensym,
				kelsecasesym,kelseswitchsym,kexceptsym,kendsym,rcurlysym then
			exit
!
		when namesym then
			case nextlx.symbol
			when dcolonsym then
				p:=createunit0(j_labeldef)
				stname:=getduplnameptr(currproc,lx.symptr,labelid)
				adddef(currproc,stname)
				p.def:=stname
				p.trylevel:=try_level
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

		when semisym then

		when kstepsym then
			exit

		else							!assume a statement
	doexec::
			p:=readunit()
	doexec2::
			if p.tag=j_name and lx.symbol=namesym then
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
		barsym, kstepsym then
	else
		serror("Readsunit: "";"" expected, or bad unit starter")
	esac

	if ulist=nil or ulist.nextunit then
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
					stname.nulldef:=lx.symptr

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
	stmacro.paramlist:=paramlist
	stmacro.isglobal:=isglobal

	checkequals()
	lex()
	stmacro.code:=readunit()
end

proc readimportalias(ref strec dimport)=
!positioned at 'as'; read following name as alias for the import module name
!implement as a macro
	ref strec stmacro

	lexchecksymbol(namesym)			!alias name to use
	stmacro:=getduplnameptr(stmodule,lx.symptr,macroid)
	adddef(stmodule,stmacro)

	lex()

	stmacro.paramlist:=nil
	stmacro.code:=createname(dimport)
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
	stmacro.paramlist:=nil
	stmacro.code:=createname(stimport)
end

function readrecase:unit=
	lex()
	if lx.symbol=kelsesym then
		lex()
		return createunit0(j_recase)
	else
		return createunit1(j_recase,readunit())
	fi
end

proc adddocstring(ichar s)=
	if ndocstrings>docstrings.len then
		serror("Too many docstrings")
	fi
	docstrings[++ndocstrings]:=pcm_copyheapstringn(s,strlen(s))
end

function fixcond(unit p)unit=
	if not isboolunit(p) then
!PRINTUNIT(P)
		insertunit(p, j_istruel)
		p.pclop:=kistruel
	fi
	return p
end
=== mm_support.m 0 0 14/44 ===
global [0:]byte bytemasks=(1,2,4,8,16,32,64,128)

global function loadsourcefile(ichar filespec)int=
!file is a complete file spec of a file known to exist
!shortfile is the name as it might appear in an include statement; part- or fully-qualified
!return index into sourcefile tables
	ichar s,basefilename

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi

	basefilename:=extractfile(filespec)

	++nsourcefiles
	sourcefilespecs[nsourcefiles]:=pcm_copyheapstring(filespec)
	sourcefilepaths[nsourcefiles]:=pcm_copyheapstring(extractpath(filespec))
	sourcefilenames[nsourcefiles]:=pcm_copyheapstring(basefilename)

	s:=cast(readfile(filespec))			!will overallocate by a few bytes
	if not s then				!unexpected error
		loaderror("LSF can't load ",filespec)
	fi
	sourcefiletext[nsourcefiles]:=s

	if fwritema then
		sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(s)
	fi

	sourcefilesizes[nsourcefiles]:=rfsize
	(s+rfsize)^:=0				!replace etx,0 by 0,0 (effectively, just zero)
	return nsourcefiles
end

global function loadbuiltin(ichar shortfile, text)int=
!loading built-in file with given text, which has just been located,
!and add to the list of sourcefiles
	ichar s

	if nsourcefiles>maxsourcefile then
		loaderror("Too many source files")
	fi
	++nsourcefiles

	sourcefilepaths[nsourcefiles]:=""
	sourcefilespecs[nsourcefiles]:=sourcefilenames[nsourcefiles]:=pcm_copyheapstring(shortfile)
	issyslibfile[nsourcefiles]:=1

!source code may be written to (avoids doing that with names
!but may happen with real numbers); need to make writeable copy
!sourcefiletext[nsourcefiles]:=hdrtext
	sourcefiletext[nsourcefiles]:=pcm_copyheapstring(text)
	if fwritema then
		sourcefiledupl[nsourcefiles]:=pcm_copyheapstring(text)
	fi

	sourcefilesizes[nsourcefiles]:=strlen(text)
	return nsourcefiles
end

!global function loadbundledfile(ichar filespec,int support=0)int fileno=
!!loading bundled file
!!Name of header is in 'file'.
!	ichar file
!	int n,lastmatch
!
!	file:=extractfile(filespec)
!
!	for i to nmafiles do
!		if eqstring(file,mafilenames[i]) and support=mafilesupport[i] then		!found
!!		if eqstring(file,mafilenames[i]) then		!found
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
!				sourcefiletext[nsourcefiles]:=pcm_copyheapstring(mafiletext[i])
!
!			fi
!			return fileno
!		fi
!	od
!!
!	loaderror("Can't find bundled file: # #",filespec)
!	return 0
!end

global proc mcerror(ichar mess)=
	println "MC Error:",mess

	stop 1
end

global proc serror_gen(ichar mess)=

	if currproc and currproc^.nameid=procid then
		print "In function",currproc^.name,," "
	fi

!	println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno],sourcefilenames[lx.fileno]
	println "On line",lx.lineno,"in file",sourcefilespecs[lx.fileno]

	println
	println "**** Syntax Error:",mess,"****"
!	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
	stopcompiler(sourcefilespecs[lx.fileno],lx.lineno)
end

global proc stopcompiler(ichar filename,int lineno)=
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
		fileno:=p.fileno
		lineno:=p.lineno
	else
		fileno:=mlineno>>24
		lineno:=mlineno iand 16777215
	fi

	if currproc and currproc^.nameid=procid then
		print "In function",currproc^.name,," "
	fi

!	println "On line",lineno iand 16777215,"in file",sourcefilepaths[fileno]
	println "On line",lineno iand 16777215,"in file",sourcefilespecs[fileno]
	println
	case pass
	when 'N' then print "**** RX Name Error: "
	when 'T' then print "**** TX Type Error: "
	when 'G' then print "**** GX Code Gen Error: "
	when 'A' then print "**** 'AX Code Gen Error: "
	esac
	println mess

	os_getch()

	stopcompiler(sourcefilespecs[fileno],lineno iand 16777215)
end

global proc rxerror(ichar mess,unit p=nil)=
	error_gen('N',mess,p)
end

global proc gerror(ichar mess,unit p=nil)=
	error_gen('G',mess,p)
end

global proc axerror(ichar mess)=
	CPL =ALINENO
	error_gen('A',mess)
end

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

!	println "On line",lx.lineno,"in file",sourcefilepaths[lx.fileno]
	println "On line",lx.lineno,"in file",sourcefilespecs[lx.fileno]

	println
	println "**** Lex Error:",mess,"****"
	println

!	stopcompiler(sourcefilepaths[lx.fileno],lx.lineno)
	stopcompiler(sourcefilespecs[lx.fileno],lx.lineno)
end

global proc lxerror(ichar mess)=
	lxerror_gen(mess)
end

global proc loaderror(ichar mess,mess2="",mess3="")=
	[512]char str
	if strchr(mess,'#') then
		fprint @str,mess,mess2,mess3
	else
		print @str,mess
	fi

	println "Load Error:",str
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

global proc init_tt_tables=
	int i,size,bitsize
	int s,t,u,v

!Initialise type tt-tables from std types first all fields initially zero

	for i:=0 to tlast-1 do

		ttname[i]:=stdnames[i]
		ttbasetype[i]:=i
		bitsize:=stdbits[i]

		switch bitsize
		when 0 then
			size:=0
		when 1,2,4 then
			size:=1
		else
			size:=bitsize/8
		endswitch

		ttsize[i]:=size

		case stdcodes[i]
		when 'I' then
			ttisint[i]:=1
			ttisinteger[i]:=1
		when 'U','C' then
			ttisword[i]:=1
			ttisinteger[i]:=1
		when 'R' then
			ttisreal[i]:=1
		when 'P','Q' then
			ttisref[i]:=1
		esac

		if ttisinteger[i] and size<8 then
			ttisshort[i]:=1
		fi

		if ttisinteger[i] or ttisreal[i] then
			ttisallnum[i]:=1
			if not ttisshort[i] then
				ttismainnum[i]:=1
			fi
		fi

		ttlower[i]:=1
	od

	ttbasetype[trefchar]:=tref
	tttarget[trefchar]:=tc8

	ntypes:=tlast-1
end

global proc addspecialtypes=
	trefproc:=createrefmode(nil,tproc,0)
	treflabel:=createrefmode(nil,tlabel,0)
end

global function getintfile(ichar filespec)int =
	[300]char filename
	ichar file,libtext

	if not dointlibs then loaderror("getintfile:-ext?") fi

	strcpy(&.filename,extractfile(filespec))

	libtext:=findstdlib(&.filename)
	if libtext then
		return loadbuiltin(&.filename,libtext)
	fi
	return 0
end

global function getsupportfile(ichar filename, ext="", path="",
	int issyslib=0, issupport=0)int =
!filename is a rel/abs/base filespec (rel path, abs path or none)
!syslib=1: look first inside INT list
!syslib=0 or not found in int (or -EXT mode0):
!	fbundled; load from list of bundled files from .ma fle
!	filename has rel path/no path: appl to given path
!	filename has abs path: use as is
!look up filename at that final location only (not multiple places)
!issupport=0 for a support file; helps with bundled files where there may
!be duplicates

	[300]char filespec,filespec2
	ichar file
	int fileno

	file:=filename

	if ext^ then
		strcpy(filespec,addext(filename,ext))
		file:=&.filespec
	fi

	if issyslib and dointlibs then
		fileno:=getintfile(file)
		return fileno when fileno
	fi

	if fbundled then
LOADERROR("BUNDLED")
!		return loadbundledfile(file,issupport)
	fi

	if not isabspath(file) then
		strcpy(filespec2,path)
		strcat(filespec2,file)
		file:=&.filespec2
	fi

	if file=nil or not checkfile(file) then
		loaderror("Can't find file: # #",filename)
	fi

	fileno:=loadsourcefile(file)
	issupportfile[fileno]:=issupport
	issyslibfile[fileno]:=issyslib
	return fileno
end

function isabspath(ichar filespec)int=
	ichar path:=extractpath(filespec)
	if path^ in ['\\','/'] or path^<>0 and (path+1)^=':' then	!absolute path
		return 1
	fi
	return 0
end

global proc initbblib=
	for i:=1 to D_typestarterset.len do typestarterset[D_typestarterset[i]]:=1 od
end

global proc do_writema=
	[maxsourcefile]int sflist
	[300]char str
	filehandle f
	int offset, nfiles, fileno, index

	if not fwritema then
		return
	fi

!first build a table of source files to be o/p
	nfiles:=0
	for i to nsourcefiles when not (issyslibfile[i] and fwritema=1) do
		sflist[++nfiles]:=i
	od

	if nfiles=0 then loaderror("MA:no files") fi

	f:=fopen(mafilename,"wb")
	if not f then loaderror("Can't create MA file #",mafilename) fi

	println "Writing ",mafilename
	fprintln @f,"=== MA # ===",nfiles

	for i to nfiles do
		fileno:=sflist[i]
		fprintln @f,"=== # # # #/# ===",
			extractfile(sourcefilespecs[fileno]),
			issyslibfile[fileno],
			issupportfile[fileno],
			i,nfiles

		offset:=getfilepos(f)
		writerandom(f,cast(sourcefiledupl[fileno]),offset,sourcefilesizes[fileno])
	od

	println @f,"=== END ==="

	for i to nfiles do
		index:=sflist[i]
		println @f,i,extractfile(sourcefilespecs[index]),
			issyslibfile[index],
			issupportfile[index]
	od

	fclose(f)
	stop
end

=== mm_tables.m 0 0 15/44 ===

global tabledata() 	[0:]ichar stdnames,
		[0:]byte stdbits,
		[0:]byte stdcodes,
		[0:]byte stdtopcl =
!    type        name    bits   code   tabtype     tabtype2    pcltype     cat         cat2
    (tvoid=0,    "void",    0,    0,	tpvoid),

    (tc64,       "c64",    64,   'C',	tpu64),
    (tu64,       "u64",    64,   'U',	tpu64),
    (tu128,      "u128",  128,   'U',	tpu128),
    (ti64,       "i64",    64,   'I',	tpi64),
    (ti128,      "i128",  128,   'I',	tpi128),
    (tr32,       "r32",    32,   'R',	tpr32),
    (tr64,       "r64",    64,   'R',	tpr64),

    (trange,     "range", 128,   'G',	tpu128),  
    (tref,       "ref",    64,   'P',	tpu64),  
    (trefchar,   "ichar",  64,   'P',	tpu64),  
    (tarray,     "array",   0,   'A',	tpblock),  
    (tslice,     "slice", 128,     0,	tpu128),  
    (trecord,    "rec",     0,     0,	tpblock),  

    (tblock,     "block",   0,     0,	tpblock),  
    (tshort,     "short",   0,     0,	tpvoid),  

    (tc8,        "c8",      8,   'C',	tpu8),  
    (tc16,       "c16",    16,   'C',	tpu16),  
    (ti8,        "i8",      8,   'I',	tpi8),  
    (ti16,       "i16",    16,   'I',	tpi16),  
    (ti32,       "i32",    32,   'I',	tpi32),  
    (tu8,        "u8",      8,   'U',	tpu8),
    (tu16,       "u16",    16,   'U',	tpu16),
    (tu32,       "u32",    32,   'U',	tpu32),  

    (tenum,      "enum",   64,     0,	tpu64),  

    (tauto,      "auto",    0,     0,	tpvoid),  
    (tany,       "any",     0,     0,	tpvoid),  
    (tproc,      "proc",    0,     0,	tpvoid),  
    (tlabel,     "label",   0,     0,	tpvoid),  
    (ttype,      "type",   64,     0,	tpvoid),  
    (tbitfield,  "bitfl",   8,     0,	tpvoid),  
    (ttuple,     "tuple",   0,     0,	tpvoid),  
    (tpending,   "pend",    0,     0,	tpvoid),  

    (tlast,      "last ",   0,     0,	tpvoid),  
end

global const tuser	= tlast

global const tint	= ti64
global const tword	= tu64
global const treal	= tr64
global const tfirstnum	= tc64
global const tlastnum	= tr64

global const tfirsttabletype	= tc64			!bounds used in optables
global const tlasttabletype		= tenum

global const maxtuplesize = 4

global int trefproc
global int treflabel

global []int typerank=(
	1,	!tc64
	2,	!tu64
	4,	!tu128
	3,	!ti64
	5,	!ti128
	6,	!tr32
	7)	!tr64

global tabledata() []ichar sysfnnames, []byte sysfnparams, []byte sysfnres =
	(sysfn_init,				$,	0,	0),
	(sysfn_print_startfile,		$,	0,	0),
	(sysfn_print_startstr,		$,	0,	0),
	(sysfn_print_startptr,		$,	0,	0),
	(sysfn_print_startcon,		$,	0,	0),
	(sysfn_print_setfmt,		$,	0,	0),
	(sysfn_print_nogap,			$,	0,	0),
	(sysfn_print_space,			$,	0,	0),
	(sysfn_print_i64,			$,	0,	0),
	(sysfn_print_i64_nf,		$,	0,	0),
	(sysfn_print_u64,			$,	0,	0),
	(sysfn_print_r64,			$,	0,	0),
	(sysfn_print_r32,			$,	0,	0),
	(sysfn_print_i128,			$,	0,	0),
	(sysfn_print_u128,			$,	0,	0),
	(sysfn_print_str,			$,	0,	0),
	(sysfn_print_str_nf,		$,	0,	0),
	(sysfn_print_strsl,			$,	0,	0),
	(sysfn_print_ptr,			$,	0,	0),
	(sysfn_print_ptr_nf,		$,	0,	0),
	(sysfn_print_c8,			$,	0,	0),
	(sysfn_print_newline,		$,	0,	0),
	(sysfn_print_end,			$,	0,	0),
	(sysfn_read_i64,			$,	0,	0),
	(sysfn_read_r64,			$,	0,	0),
	(sysfn_read_str,			$,	0,	0),
	(sysfn_read_fileline,		$,	0,	0),
	(sysfn_read_strline,		$,	0,	0),
	(sysfn_read_conline,		$,	0,	0),

	(sysfn_getnprocs,			$,	0,	1),		!access functions
!	(sysfn_getnexports,			$,	0,	1),
	(sysfn_getprocname,			$,	0,	1),
	(sysfn_getprocaddr,			$,	0,	1),
!	(sysfn_getprocexport,		$,	0,	1),
end
!
global [sysfnnames.len]psymbol sysfnhandlers

!global [sysfnnames.len]int sysfnproclabels

global int mlineno
global byte fshowpst


!!---
global tabledata() [0:]ichar jtagnames, [0:]byte jisexpr =
!Basic units; these don't follow normal rules of params needing to be units or lists
!jisexpr=1/2 when unit returns a value; 1 means unary, 2 binary op,
! 3 means returns a value, but is not a unary or binary op 

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
	(j_namelv,		$,		3), ! def=nameptr
	(j_block,		$,		0), ! a=L
	(j_stmtblock,	$,		0), ! a=L
	(j_decimal,		$,		3), ! svalue=str, slength
	(j_assem,		$,		0), ! svalue=str, slength
	(j_assemmacro,	$,		0), !
	(j_assemreg,	$,		0), !
	(j_assemxreg,	$,		0), !
	(j_assemmem,	$,		0), !
	(j_strinclude,	$,		0), !

!Logical Operators

	(j_andl,		$,		2), ! a b	This group are for conditional expressions (no result)
	(j_orl,			$,		2), ! a b
	(j_xorl,		$,		2), ! a b
	(j_notl,		$,		1), ! a
	(j_istruel,		$,		1), ! a

!Expressions and Operators

	(j_makelist,	$,		3), ! a=L, b=[u], length=N; element list/lower bound expr
	(j_makerange,	$,		3), ! a b
	(j_makeset,		$,		3), ! a=L, length=N
	(j_makedict,	$,		3), !
	(j_makeslice,	$,		3), !
	(j_exprlist,	$,		3), ! a=u...	List of expressions, as (a;b;c,	0), rather than (a,b,c)
	(j_multexpr,	$,		3), !
	(j_returnmult,	$,		3), !

	(j_keyword,		$,		3), ! def=st entry
	(j_keyvalue,	$,		3), ! a b
	(j_assign,		$,		3), ! a b
	(j_deepcopy,	$,		3), ! a b
	(j_callfn,		$,		3), ! a b
!	(j_applyop,		$,		0), ! opcode b c
!	(j_applyopx,	$,		1), ! opcode b c
	(j_new,			$,		3), ! newmode=T, a=L, length=N
	(j_destroy,		$,		0), ! a=L, length=N
!	(j_clear,		$,		0), !

!	(j_setcc,		$,		2), ! a b
	(j_cmp,			$,		2), ! a b
	(j_cmpchain,	$,		1), ! a b
	(j_bin,			$,		2), ! a b
	(j_unary,		$,		1), ! a b
	(j_binto,		$,		2), ! a b
	(j_unaryto,		$,		1), ! a b
	(j_incr,		$,		3), ! a	++a

	(j_inrev,		$,		2), ! a b
	(j_inrange,		$,		2), ! a b
	(j_inset,		$,		2), ! a b
	(j_clamp,		$,		2), ! a b

	(j_flexptr,		$,		3), ! a b
	(j_stringz,		$,		3), ! a b
	(j_sliceptr,	$,		3), ! a b

	(j_index,		$,		3), ! a b		a[b]
	(j_indexlv,		$,		3), ! a b		a[b]
	(j_slice,		$,		3), ! a b		a[b]
	(j_dot,			$,		3), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotlv,		$,		3), ! a b opcode	a.b; opcode=0/1/2 used for signalling in rx pass
	(j_dotindex,	$,		3), ! a b		a[b]
	(j_dotslice,	$,		3), ! a b		a[b]
	(j_anddotslice,	$,		3), ! a b		a[b]
	(j_anddotindex,	$,		3), ! a b		a[b]

	(j_ptr,			$,		3), ! a		a^
	(j_ptrlv,		$,		3), ! a		a^
	(j_addrof,		$,		3), ! a		&a
	(j_addroffirst,	$,		3), ! a		&a
	(j_convert,		$,		3), ! typeno=T a		T(a)			T
	(j_shorten,		$,		3), !
	(j_autocast,	$,		3), ! typeno=T a		T(a)			T
	(j_typepun,		$,		3), ! typeno=T a		T@(a)			T
	(j_typeconst,	$,		3), ! typeno=T			typeconst(T)
	(j_operator,	$,		3), ! opcode=opc
	(j_upper,		$,		3), ! a		$					T

	(j_bitwidth,	$,		1), ! a
	(j_bytesize,	$,		1), ! a
	(j_typeof,		$,		3), ! a
	(j_typestr,		$,		1), ! a
!	(j_sliceptr,	$,		1), ! a
	(j_bitfield,	$,		3), ! a

	(j_minvalue,	$,		3), ! a
	(j_maxvalue,	$,		3), ! a

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
	(j_cvtrue,		$,		3), ! 
	(j_cvfalse,		$,		3), ! 

	(j_whenthen,	$,		0), ! a=L b=u
	(j_elsif,		$,		0), ! opcode=condcode, a
	(j_fmtitem,		$,		3), ! a b  x/fmtstr
	(j_nogap,		$,		3), ! 
	(j_space,		$,		3), ! 

!Statements

	(j_callproc,	$,		0), ! a=fn b=L, length
	(j_return,		$,		0), ! a=x/nil
	(j_syscall,		$,		0), ! a=x or nil

!	(j_assign,		$,		3), ! a b
	(j_to,			$,		0), ! a=N, b=body, c=tempvar/nil, def=name
	(j_if,			$,		3), ! condcode a=then b=else
	(j_longif,		$,		3), ! a=(elsif ...) b=else		L is series of kelsif pairs
	(j_forup,		$,		0), ! 
	(j_fordown,		$,		0), !
	(j_forall,		$,		0), !
	(j_forallrev,	$,		0), !
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
!	(j_recaseelse,	$,		0), ! Not implemented

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
	(j_stack,		$,		0), ! "
	(j_unstack,		$,		0), ! "
	(j_empty,		$,		1), ! "

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

!!---
global tabledata() []ichar symbolnames,
					[]byte symboloptypes,
					[]byte symbolgenops,
					[]byte symbolgentoops,
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
	(assignsym,			":=",		bin_op,	0,	0,	1,	0),		! :=
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
	(rangesym,			"..",		bin_op,	0,	0,	5,	0),		! ..
	(ellipsissym,		"...",		0,	0,	0,	0,	0),		! ...
	(hashsym,			"#",		0,	0,	0,	0,	0),		! #

!	(opsym,				$,		0,	0,	0,	0,	0),		! Any operator or property tag (use sets to distinguish)

	(addsym,			"+",		bin_op,		kadd,		kaddto,		4,	1),
	(subsym,			"-",		bin_op,		ksub,		ksubto,		4,	1),
	(mulsym,			"*",		bin_op,		kmul,		kmulto,		3,	0),
	(divsym,			"/",		bin_op,		kdiv,		kdivto,		3,	0),
	(idivsym,			"%",		bin_op,		kidiv,		kidivto,	3,	0),
	(iremsym,			"rem",		bin_op,		kirem,		kiremto,	3,	0),
	(iandsym,			"iand",		bin_op,		kiand,		kiandto,	4,	0),
	(iorsym,			"ior",		bin_op,		kior,		kiorto,		4,	0),
	(ixorsym,			"ixor",		bin_op,		kixor,		kixorto,	4,	0),
	(shlsym,			"<<",		bin_op,		kshl,		kshlto,		3,	0),
	(shrsym,			">>",		bin_op,		kshr,		kshrto,		3,	0),
	(minsym,			"min",		bin_op,		kmin,		kminto,		4,	1),
	(maxsym,			"max",		bin_op,		kmax,		kmaxto,		4,	1),
	(andlsym,			"and",		bin_op,		kandl,		kandlto,	7,	0),
	(orlsym,			"or",		bin_op,		korl,		korlto,		8,	0),
	(xorlsym,			"xor",		bin_op,		0,			0,			8,	0),

	(eqsym,				"=",		bin_op,		keq,		0,			6,	1),
	(cmpsym,			"cmp",		bin_op,		0,			0,			6,	1),
!	(appendsym,			"append",	bin_op,		kappend,	kappendto,	4,	0),
!	(concatsym,			"concat",	bin_op,		kconcat,	kconcatto,	4,	0),
	(powersym,			"**",		bin_op,		kpower,		0,			2,	0),
	(samesym,			"==",		bin_op,		ksame,		0,			6,	0),
!	(ssmarkersym,		"===",		0,			0,			0,			0,	0),
	(insym,				"in",		bin_op,		kin,		0,			6,	0),
	(notinsym,			"notin",	bin_op,		knotin,		0,			6,	0),
	(inrevsym,			"inrev",	0,			0,			0,			0,	0),

	(negsym,			"$neg",		mon_op,		kneg,		0,			0,	1),
	(notlsym,			"not",		mon_op,		knotl,		knotlto,	0,	1),
	(istruelsym,		"istrue",	mon_op,		kistruel,	kistruelto,	0,	1),
	(inotsym,			"inot",		mon_op,		kinot,		kinotto,	0,	1),
	(abssym,			"abs",		mon_op,		kabs,		kabsto,		0,	1),
	(signsym,			"sign",		mon_op,		ksign,		0,			0,	1),
	(sqrtsym,			"sqrt",		mon_op,		ksqrt,		0,			0,	1),
	(sqrsym,			"sqr",		mon_op,		ksqr,		0,			0,	1),

	(propsym,				$,		prop_op,	0,			0,			0,	0),
	(mathsopsym,		$,		0,	0,	0,	0,	1),		! sin etc
	(maths2opsym,		$,		0,	0,	0,	0,	1),		! atan2 etc

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
	(kstrincludesym,	$,		0,	0,	0,	0,	1),		! 
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
	(kstepsym,			$,		0,	0,	0,	0,	0),		! STEP
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
!	(kimportsym,		$,		0,	0,	0,	0,	0),		! IMPORT
	(kimportmodulesym,	$,		0,	0,	0,	0,	0),		! IMPORTDLL/IMPORTMODULE
!	(kimportpathsym,	$,		0,	0,	0,	0,	0),		! IMPORTPATH
!	(kmapmodulesym,		$,		0,	0,	0,	0,	0),		! MAPMODULE
	(ktypesym,			$,		0,	0,	0,	0,	0),		! TYPE
	(ktypealiassym,		$,		0,	0,	0,	0,	0),		! TYPEALIAS
	(kextendtypesym,	$,		0,	0,	0,	0,	0),		! EXTENDTYPE
	(krefsym,			$,		0,	0,	0,	0,	1),		! REF
	(kmutsym,			$,		0,	0,	0,	0,	0),		! MUT
	(kletsym,			$,		0,	0,	0,	0,	0),		! LET
	(kslicesym,			$,		0,	0,	0,	0,	0),		! SLICE/SLICE2D
	(karraysym,			$,		0,	0,	0,	0,	0),		! ARRAY
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
	(kheadersym,		$,		0,	0,	0,	0,	0),		! MODULE
	(kheadervarsym,		$,		0,	0,	0,	0,	0),		! MODULE
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
!	(kemitcsym,			$,		0,	0,	0,	0,	0),		! EMITC
	(kemptysym,			$,		0,	0,	0,	0,	0),		! EMPTY

	(kdummysym,			$,		0,	0,	0,	0,	0),		!
end

global tabledata() []ichar sourcedirnames =
	(includedir,	$),
!	(strincludedir,	$),
	(binincludedir,	$),
!	(textincludedir,$),
!	(defineunitdir,	$),
!	(emitcdir,		$),
end

global tabledata() []ichar headerdirnames =
!	(hdr_ssfile,		$),
	(hdr_module,		$),
	(hdr_import,		$),
	(hdr_subprog,		$),
	(hdr_sysmodule,		$),
	(hdr_sysimport,		$),
	(hdr_syssubprog,	$),
!	(hdr_importmodule,	$),
!	(hdr_alias,			$),
	(hdr_altpath,		$),
	(hdr_importpath,	$),
	(hdr_linkdll,		$),
	(hdr_linklib,		$),
	(hdr_exportmodule,	$),
	(hdr_file,			$),
	(hdr_runexe,		$),
	(hdr_setvar,		$),
	(hdr_showvar,		$),
end

global tabledata() []ichar headervarnames =
	(hv_devpath,		$),
	(hv_mmpath,			$),
	(hv_hdrpath,		$),
	(hv_ctarget,		$),
	(hv_windows,		$),
	(hv_linux,			$),
	(hv_optim,			$),
	(hv_mainmodule,		$),
	(hv_a,				$),
	(hv_b,				$),
	(hv_c,				$),
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
	(mlangff,		$), ! 
	(callbackff,	$), ! 
end

global tabledata() [0:]ichar scopenames=
	(local_scope=0,		$), ! 		!module
	(program_scope,		$), ! 		!global/inter-module
	(export_scope,		$), ! 		!export/inter-subprog
	(exportq_scope,		$), ! 
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

global tabledata() [0:]ichar namenames
	(nullid=0,		$),		!Not assigned (sometimes converted to genfieldid)
	(programid,		$),		!Main root
	(subprogid,		$),
	(moduleid,		$),		!Current or imported module
	(dllmoduleid,	$),		!
	(typeid,		$),		!Type name in type, proc or module
	(procid,		$),		!Proc/method/function/op name
	(dllprocid,		$),		!Dll Proc/function name
	(dllvarid,		$),		!Dll variable name
	(genprocid,		$),		!generic proc name
	(generatorid,	$),		!generator proc name
	(constid,		$),		!Named constant in type, proc or module
	(staticid,		$),		!Static in type or proc or module
	(frameid,		$),		!Local var
	(paramid,		$),		!Local param
	(fieldid,		$),		!Field of Record or Class
	(genfieldid,	$),		!Generic Field of Record or Class
	(enumid,		$),		!Enum name, part of enum type only
	(labelid,		$),		!Label name in proc only
	(blockid,		$),		!Codeblock label name in proc only
	(aliasid,		$),		!Alias to another name
	(macroid,		$),		!Name of macro
	(macroparamid,	$),		!Macro formal parameter name
	(linkid,		$),		!Name in class defined in a base class
	(functionopid,	$),		!Function-operator
end

!global tabledata() [0:]ichar pstnames =
!	(no_name = 0,		$),
!	(proc_name,			$),
!	(dllproc_name,		$),
!	(istatic_name,		$),
!	(zstatic_name,		$),
!	(param_name,		$),
!	(frame_name,		$),
!end

!!---
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
	("forall",		kforsym,		0),
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
	("$step",		kstepsym,		0),
	("goto",		kgotosym,		0),
	("go",			kgotosym,		1),
	("switch",		kswitchsym,		j_switch),
	("doswitch",	kdoswitchsym,	j_doswitch),
	("tabledata",	ktabledatasym,	0),
	("clamp",		kclampsym,		0),
	("eval",		kevalsym,		0),
!	("extendtype",	kextendtypesym,	0),
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

!	("stack",		kstacksym,		j_stack),
!	("unstack",		kstacksym,		j_unstack),

	("cp",			kprintsym,		j_print),
	("cpl",			kprintsym,		j_println),

	("read",		kreadsym,		j_read),
	("readln",		kreadsym,		j_readln),
	("cast",		kcastsym,		j_convert),

	("proc",		kprocsym,		0),
	("function",	kfunctionsym,	0),
	("threadedproc",		kprocsym,		1),
!	("threadedfunction",	kfunctionsym,	1),
!	("method",		kmethodsym,		0),

	("type",		ktypesym,		0),
	("class",		kclasssym,		0),
	("record",		krecordsym,		0),
	("struct",		kstructsym,		0),
	("union",		kunionsym,		0),
	("ref",			krefsym,		0),
	("pointer",		krefsym,		0),
	("returning",	sendtosym,		0),
	("var",			kmutsym,		0),
	("mut",			kmutsym,		0),
	("let",			kletsym,		0),

	("include",		ksourcedirsym,	includedir),
!	("strinclude",	ksourcedirsym,	strincludedir),
	("strinclude",	kstrincludesym,	0),
	("bininclude",	ksourcedirsym,	binincludedir),
!	("textinclude",	ksourcedirsym,	textincludedir),
!	("defineunit",	ksourcedirsym,	defineunitdir),
	("macro",		kmacrosym,		0),
!	("expand",		kexpandsym,		0),
!	("operator",	koperatorsym,	0),
!	("emitc",		ksourcedirsym,	emitcdir),
!	("cclib",		ksourcedirsym,	cclibdir),

	("assem",		kassemsym,		1),
	("asm",			kassemsym,		0),

	("static",		kstaticsym,		0),
	
	("const",		kconstsym,		0),
!	("table",		kconstsym,		1),
	("enum",		kenumsym,		0),

	("$get_nprocs",		ksyscallsym,		sysfn_getnprocs),
	("$getnprocs",		ksyscallsym,		sysfn_getnprocs),

	("$get_procname",	ksyscallsym,		sysfn_getprocname),
	("$getprocname",	ksyscallsym,		sysfn_getprocname),

	("$get_procaddr",	ksyscallsym,		sysfn_getprocaddr),
	("$getprocaddr",	ksyscallsym,		sysfn_getprocaddr),

	("importdll",	kimportmodulesym,	'D'),
	("importlib",	kimportmodulesym,	'L'),
	("unless",		kunlesssym,			0),

	("try",			ktrysym,		0),
	("except",		kexceptsym,		0),
	("finally",		kfinallysym,	0),
	("raise",		kraisesym,		0),
	("out",			koutsym,		0),

	("new",			knewsym,		j_new),
	("destroy",		kdestroysym,	j_destroy),
!	("clear",		kclearsym,		j_clear),

	("global",		kglobalsym,		program_scope),
	("exportq",		kglobalsym,		exportq_scope),
	("export",		kglobalsym,		export_scope),

	("clang",		kfflangsym,		clangff),
	("mlang",		kfflangsym,		mlangff),
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
!	("u1",			stdtypesym,		tu1),
!	("u2",			stdtypesym,		tu2),
!	("u4",			stdtypesym,		tu4),
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

!	("bit",			stdtypesym,		tu1),
!	("bit2",		stdtypesym,		tu2),
!	("bit4",		stdtypesym,		tu4),

	("char",		stdtypesym,		tc8),
	("wchar",		stdtypesym,		tc16),
	("char64",		stdtypesym,		tc64),

!	("range",		stdtypesym,		trange),
	("auto",		stdtypesym,		tauto),
!	("label",		stdtypesym,		tlabel),

!	("flex",		stdtypesym,		tvar),

	("intm",		machinetypesym,	'I'),
	("intp",		machinetypesym,	'i'),
	("wordm",		machinetypesym,	'W'),
	("wordp",		machinetypesym,	'w'),
	("slice",		kslicesym,		tslice),
!	("slice2d",		kslicesym,		tslice2d),
!!	("flex",		kslicesym,		tflex),
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
	("true",		compilervarsym,	j_cvtrue),
	("false",		compilervarsym,	j_cvfalse),
	("$",			dollarsym,		0),

	("and",			andlsym,		0),
	("or",			orlsym,			0),
	("xor",			xorlsym,		0),
	("iand",		iandsym,		0),
	("ior",			iorsym,			0),
	("ixor",		ixorsym,		0),
	("in",			insym,			kin),
	("notin",		notinsym,		knotin),
	("inrev",		inrevsym,		0),
	("rem",			iremsym,		0),
!	("divrem",		idivremsym,		0),
	("min",			minsym,			0),
	("max",			maxsym,			0),

	("not",			notlsym,		0),
	("inot",		inotsym,		0),
	("istrue",		istruelsym,		0),
	("abs",			abssym,			kabs),
	("$neg",		negsym,			0),

!	("asc",			opsym,			j_asc),
!	("tochr",		opsym,			j_chr),
	("sqr",			sqrsym,			0),
	("sqrt",		sqrtsym,		0),
	("sign",		signsym,		0),

	("sin",			mathsopsym,		ksin),
	("cos",			mathsopsym,		kcos),
	("tan",			mathsopsym,		ktan),
	("asin",		mathsopsym,		kasin),
	("acos",		mathsopsym,		kacos),
	("atan",		mathsopsym,		katan),
!	("sign",		mathsopsym,		ksign),
	("ln",			mathsopsym,		kln),
	("log",			mathsopsym,		klog),
!	("lg",			mathsopsym,		klg),
	("exp",			mathsopsym,		kexp),
	("round",		mathsopsym,		kround),
	("floor",		mathsopsym,		kfloor),
	("ceil",		mathsopsym,		kceil),
	("fract",		mathsopsym,		kfract),

	("atan2",		maths2opsym,	katan2),
	("fmod",		maths2opsym,	kfmod),

!	("append",		appendsym,		0),
!	("concat",		concatsym,		0),
!	("flexptr",		flexptrsym,		0),
!	("sliceptr",	propsym,		ksliceptr),
!	("stringz",		stringzsym,		0),

	("len",			propsym,	klen),
	("lwb",			propsym,	klwb),
	("upb",			propsym,	kupb),
	("bounds",		propsym,	kbounds),
	("lenstr",		propsym,	klenstr),
	("bitwidth",	propsym,	kbitwidth),
	("bytes",		propsym,	kbytesize),
	("minvalue",	propsym,	kminvalue),
	("maxvalue",	propsym,	kmaxvalue),
	("typestr",		propsym,	ktypestr),

!	("len",			propsym,	0),
!	("lwb",			propsym,	0),
!	("upb",			propsym,	0),
!	("bounds",		propsym,	0),
!	("lenstr",		propsym,	0),
!	("bitwidth",	propsym,	0),
!	("bytes",		propsym,	0),
!	("minvalue",	propsym,	0),
!	("maxvalue",	propsym,	0),
!	("typestr",		propsym,	ktypestr),

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

	("$caligned",	atsym,			1),
	("empty",		kemptysym,		0),
	("clear",		kemptysym,		0),

!	("ssfile",		kheadersym,		hdr_ssfile),
	("module",		kheadersym,		hdr_module),
	("sysmodule",	kheadersym,		hdr_sysmodule),
	("import",		kheadersym,		hdr_import),
	("sysimport",	kheadersym,		hdr_sysimport),
	("subprog",		kheadersym,		hdr_subprog),
	("syssubprog",	kheadersym,		hdr_syssubprog),
!	("importmodule",kheadersym,		hdr_importmodule),
!	("alias",		kheadersym,		hdr_alias),
	("altpath",		kheadersym,		hdr_altpath),
	("importpath",	kheadersym,		hdr_importpath),
	("linkdll",		kheadersym,		hdr_linkdll),
	("linklib",		kheadersym,		hdr_linklib),
	("exportmodule",kheadersym,		hdr_exportmodule),
	("runexe",		kheadersym,		hdr_runexe),
	("setvar",		kheadersym,		hdr_setvar),
	("showvar",		kheadersym,		hdr_showvar),

	("$devpath",	kheadervarsym,	hv_devpath),
	("$mmpath",		kheadervarsym,	hv_mmpath),
	("$hdrpath",	kheadervarsym,	hv_hdrpath),
	("$ctarget",	kheadervarsym,	hv_ctarget),
	("$windows",	kheadervarsym,	hv_windows),
	("$linux",		kheadervarsym,	hv_linux),
	("$optim",		kheadervarsym,	hv_optim),
	("$mainmodule",	kheadervarsym,	hv_mainmodule),
	("$a",			kheadervarsym,	hv_a),
	("$b",			kheadervarsym,	hv_b),
	("$c",			kheadervarsym,	hv_c),


!global tabledata() []ichar headervarnames =
!	(hv_devpath,		$),
!	(hv_mmpath,			$),
!	(hv_hdrpath,		$),
!	(hv_ctarget,		$),
!	(hv_windows,		$),
!	(hv_linux,			$),
!	(hv_optim,			$),
!	(hv_a,				$),
!	(hv_b,				$),
!	(hv_c,				$),
!end

!	("nil",			knilsym,		0),
!	("con",			sysconstsym,	con_const),
!	("pi",			sysconstsym,	pi_const),

	("$$dummy",		0,				0)
end

global []int D_typestarterset= (stdtypesym,lsqsym,krefsym,kenumsym,krecordsym,
		kicharsym, ktypeofsym, kslicesym, kdictsym)

!list of genops that have an int result, used to populate intresult[]
[]byte intresultlist = (
	kin, knotin, klwb, kupb, klen, klenstr, kbitwidth,
	kbytesize, keq, kne, klt, kle, kge, kgt,
	kandl, korl, knotl, kistruel)

!!---
!Table of soft conversions between numeric types. In this form, easy to
!maintain that using a 2d table directly
global [,3]int16 softconvmappings =(
	(tc64,	tc64,	ksoftconv),
	(tc64,	tu64,	ksoftconv),
	(tc64,	tu128,	kwidenw),
	(tc64,	ti64,	ksoftconv),
	(tc64,	ti128,	kwidenw),
	(tc64,	tr32,	kfloat),
	(tc64,	tr64,	kfloat),

	(tu64,	tc64,	ksoftconv),
	(tu64,	tu64,	ksoftconv),
	(tu64,	tu128,	kwidenw),
	(tu64,	ti64,	ksoftconv),
	(tu64,	ti128,	kwidenw),
	(tu64,	tr32,	kfloat),
	(tu64,	tr64,	kfloat),

	(tu128,	tc64,	ksofttruncw),
	(tu128,	tu64,	ksofttruncw),
	(tu128,	tu128,	ksoftconv),
	(tu128,	ti64,	ksofttruncw),
	(tu128,	ti128,	ksoftconv),
	(tu128,	tr32,	kerror),
	(tu128,	tr64,	kerror),

	(ti64,	tc64,	ksoftconv),
	(ti64,	tu64,	ksoftconv),
	(ti64,	tu128,	kwidenw),
	(ti64,	ti64,	ksoftconv),
	(ti64,	ti128,	kwidenw),
	(ti64,	tr32,	kfloat),
	(ti64,	tr64,	kfloat),

	(ti128,	tc64,	ksofttruncw),
	(ti128,	tu64,	ksofttruncw),
	(ti128,	tu128,	ksoftconv),
	(ti128,	ti64,	ksofttruncw),
	(ti128,	ti128,	ksoftconv),
	(ti128,	tr32,	kerror),
	(ti128,	tr64,	kerror),

	(tr32,	tc64,	kfix),
	(tr32,	tu64,	kfix),
	(tr32,	tu128,	kerror),
	(tr32,	ti64,	kfix),
	(tr32,	ti128,	kerror),
	(tr32,	tr32,	ksoftconv),
	(tr32,	tr64,	kfwiden),

	(tr64,	tc64,	kfix),
	(tr64,	tu64,	kfix),
	(tr64,	tu128,	kerror),
	(tr64,	ti64,	kfix),
	(tr64,	ti128,	kerror),
	(tr64,	tr32,	kfnarrow),
	(tr64,	tr64,	ksoftconv))


!this 2d array maps [genop, basetype] to a spec-op
!It is filled in at runtime from genspecmappings

!global [firstgenop..lastgenop, tfirsttabletype..tlasttabletype]int16 optypetable

global [pclnames.lwb..pclnames.upb]byte intresult

global [tfirstnum..tlastnum, tfirstnum..tlastnum]int64 softconvtable

!global [specopnames.lwb..specopnames.upb]byte specoptogen
!global [specopnames.lwb..specopnames.upb]byte specoptotype

global proc inittypetables=
	int genop, s,t, a, specop

!populate the table with given combinations
!	for i in genspecmappings.bounds do
!		genop:=genspecmappings[i,1]
!		t:=genspecmappings[i,2]
!		specop:=genspecmappings[i,3]
!
!		optypetable[genop,t] := specop
!
!!build cross-reference tables
!		specoptogen[specop]:=genop
!		specoptotype[specop]:=t
!	od
!
!!now scan trying to fix some empty slots
!	for genop in firstgenop..lastgenop do
!		for t in tfirsttabletype..tlasttabletype do
!			if optypetable[genop,t]=0 then
!!CPL "OPTYPE0",STDNAMES[T];!STRMODE(T)
!				case t
!				when tc64 then
!					if a:=optypetable[genop,tu64] then optypetable[genop,tc64]:=a
!					elsif a:=optypetable[genop,ti64] then optypetable[genop,tc64]:=a
!					fi
!				when tu64 then
!					if a:=optypetable[genop,ti64] then optypetable[genop,tu64]:=a fi
!				when tu128 then
!					if a:=optypetable[genop,ti128] then optypetable[genop,tu128]:=a fi
!				esac
!			fi
!		od
!	od
!
!populate intresultlist
	for i in intresultlist.bounds do
		intresult[intresultlist[i]]:=1
	od

!do softconversions
	for i in softconvmappings.bounds do
		s:=softconvmappings[i,1]
		t:=softconvmappings[i,2]
!		specop:=softconvmappings[i,3]

		softconvtable[s,t]:=softconvmappings[i,3]
	od
end

=== mm_type.m 0 0 16/44 ===
const nolv=0
const needlv=1

const maxparams=100
const maxfields=200
int countedfields
int inassem

global proc $init=
	inittypetables()
end

global proc tx_allprocs=
	ref procrec pp
	unit pcode

	pp:=proclist
	while pp do
		currproc:=pp.def
		pcode:=currproc.code

		if ttisshort[currproc.mode] then
			mlineno:=currproc.pos
			txerror("proc short ret type")
		 fi

	    tpass(pcode,(currproc.nretvalues>1|ttuple|currproc.mode))

		case ttbasetype[currproc.mode]
		when tvoid then		!PROC
		when ttuple then	!MULT FN
		else				!REGULAR FN
			if pcode.tag<>j_return then
				insertunit(pcode,j_return)
				pcode.mode:=currproc.mode
				pcode.resultflag:=1
			fi
		esac

		pp:=pp.nextproc
	od
end

proc tpass(unit p, int t=tany, lv=nolv)=
	ref strec d
	unit a,b,c
	int oldmlineno,m,nparams,paramtype,restype

	if p=nil then return fi

	oldmlineno:=mlineno

	mlineno:=p.pos

	a:=p.a
	b:=p.b

	p.resultflag:=t<>tvoid

	switch p.tag
	when j_name then
		tx_name(p,t,lv)
	when j_const, j_decimal then

	when j_typeconst then
		p.mode:=ti64

	when j_bytesize, j_bitwidth then
		tx_bytesize(p,a)

	when j_bin then
		tx_bin(p,a,b)

	when j_unary then
		tx_unary(p,a)

	when j_binto then
		tx_binto(p,a,b)

	when j_unaryto then
		tx_unaryto(p,a)

	when j_assign,j_deepcopy then
		tx_assign(p,a,b,t)

	when j_multexpr then
		while a do
			tpass(a)
			a:=a.nextunit
		od

	when j_cmp then
		tx_cmp(p,a,b)

	when j_addrof then
		if a.tag=j_ptr then
			deleteunit(p,a)
			deleteunit(p,p.a)
			tpass(p,t)
		else
			tpass(a,,needlv)
			p.mode:=createrefmode(nil,a.mode)
		fi

	when j_addroffirst then
		tx_addroffirst(p,a,t)

	when j_if then
		tx_if(p,a,b,p.c,t,lv)

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

	when j_andl, j_orl, j_xorl then
		tx_andl(p,a,b)

	when j_notl then
		tx_notl(p,a)

	when j_istruel then
		tx_istruel(p,a)

	when j_convert then
		tx_convert(p,a,1)

	when j_typepun then
		tx_typepun(p,a)

	when j_sliceptr then
		tx_sliceptr(p,a)

	when j_incr then
		tx_incrto(p,a,t)

	when j_makerange then
		tx_makerange(p,a,b)

	when j_makeset then
		tx_makeset(p,a,t)

	when j_swap then
		tx_swap(p,a,b)

	when j_select then
		tx_select(p,a,b,p.c,t,lv)

	when j_switch, j_doswitch then
		tx_switch(p,a,b,p.c,t,lv)

	when j_case, j_docase then
		tx_case(p,a,b,p.c,t,lv)

	when j_exprlist then
		tx_exprlist(p,a,t)

	when j_dotindex, j_dotslice, j_anddotindex then
		tx_dotindex(p,a,b,lv)

	when j_slice then
		tx_slice(p,a,b)

	when j_block,j_stmtblock then
		tx_block(p,a,t,lv)

	when j_eval then
		tpass(a,tany)

	when j_do then
		tpass(a,tvoid)

	when j_return then
		tx_return(p,a,t)

	when j_print,j_println,j_fprint,j_fprintln then

		tx_unitlist(a)
		fixchararray(a)

		while b do
			if b.tag=j_fmtitem then
				tpass(c:=b.a)
				tpass(b.b,trefchar)
			else
				tpass(c:=b)
			fi
			fixchararray(c)
			b:=b.nextunit
		od
		tx_unitlist(p.c)

	when j_forup, j_fordown then
		tx_for(a,b,p.c)

	when j_forall, j_forallrev then
		tx_forall(a,b,p.c)

	when j_to then
		tpass(a,ti64)
		tpass(b,tvoid)
		tpass(p.c,ti64)		!when autovar present

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
		if iscondtrue(a) then
			p.tag:=j_do
			p.a:=b
			p.hasb:=0
		elsif iscondfalse(a) then
			p.tag:=j_null
			p.hasa:=p.hasb:=0
		fi
		tpass(b,tvoid)
		tpass(p.c,tvoid)

	when j_repeat then
		tpass(a,tvoid)
		tcond(b)
		if iscondtrue(b) or iscondfalse(b) then txerror("repeat/const cond") fi

	when j_nogap, j_space then

	when j_assem then
		if t<>tvoid then
			p.mode:=t
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
		if a.tag=j_typeconst then
			p.value:=a.value
		else
			p.value:=a.mode
		fi
		p.tag:=j_typeconst
		p.mode:=ti64
		p.hasa:=0

	when j_typestr then
		tpass(a)
	CPL "TYPESTR",STRMODE(A.MODE)
		if a.tag=j_typeconst then
			m:=a.value
		else
			tpass(a)
			m:=a.mode
		fi
		p.tag:=j_const
		p.mode:=trefchar
		p.a:=nil; p.hasa:=0
		p.svalue:=pcm_copyheapstring(strmode(m,0))
		p.slength:=strlen(p.svalue)
		p.isastring:=1

	when j_fmtitem then
		tpass(a)
		tpass(b)

	when j_readln then
		tpass(a)

	when j_read then
		if a then
			tpass(a,tc64)
		fi
		if ttisallnum[t] then
			t:=gettypebase(t)
		fi
		p.mode:=t
	when j_recase then
		if a then
			tpass(a,ti64)
			if a.tag<>j_const then
				txerror("recase must be const")
			fi
		fi

	when j_cvlineno then
		p.mode:=ti64
	when j_cvfilename,j_cvmodulename then
		p.mode:=trefchar

	when j_bitfield then
		tx_bitfield(p,a,lv)

	when j_syscall then
		restype:=tvoid
		paramtype:=tvoid
		case p.fnindex
		when sysfn_getnprocs then restype:=ti64
		when sysfn_getprocname then paramtype:=ti64; restype:=trefchar; 
		when sysfn_getprocaddr then paramtype:=ti64; restype:=tref; 
		esac

		if paramtype<>tvoid then
			if a=nil then txerror("sys: arg missing") fi
			tpass(a,paramtype)
			if a.nextunit then txerror("sys: too many args") fi
		elsif a then txerror("sys: too many args")
		fi

		p.mode:=restype

	when j_cmpchain then
		tx_cmpchain(p,a)

	when j_empty then
		tpass(a,,needlv)

	when j_shorten then

	when j_strinclude then
		tx_strinclude(p,a)

	else
		CPL "TXUNIT: CAN'T DO:",jtagnames[p.tag]
	doelse::

		if p.hasa then tx_unitlist(a,t) fi
		if p.hasb then tx_unitlist(b,t) fi
		if p.hasc then tx_unitlist(p.c,t) fi
	endswitch

	tevaluate(p)

	case p.tag
	when j_makelist, j_return then
	else
		if t<>tany and t<>tvoid and p.mode<>t then		!does not already match
			coerceunit(p,t)			!apply soft conversion
		fi
	esac
!
	IF T=TVOID THEN
		CASE P.TAG
		WHEN J_CONST, J_BIN, j_UNARY, J_CMP THEN
			TXERROR("Eval needed")
		WHEN J_NAME THEN
			unless ttisref[p.mode] and tttarget[p.mode]=tlabel then
				TXERROR("Eval needed2")
			end

		esac
	fi

	mlineno:=oldmlineno
end

proc tx_block(unit p,a, int t,lv)=
	while a and a.nextunit do
		tpass(a,tvoid)
		a:=a.nextunit
	od
	if a then
		tx_unitlist(a,t,lv)
		p.mode:=(t<>tvoid|a.mode|tvoid)
	fi
end

global proc tx_typetable=
	ref strec d

	for i:=tuser to ntypes do
		if ttbasetype[i]=trecord then
			tx_passdef(d:=ttnamedef[i])
		fi
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

	when tenum then
		ttsize[m]:=8
	when ttuple then

	else
		if size:=ttsize[ttbasetype[m]] then
			ttsize[m]:=size
			return
		fi
		println "SIZE 0:",strmode(m),=m,=stdnames[ttbasetype[m]]
		println "Can't set mode size"
	esac
end

proc setarraysize(int m)=
	int lower,length,elemsize,target,size
	unit pdim,a,b

	if ttsizeset[m] then return fi

	pdim:=ttdimexpr[m]

	if pdim then
		a:=pdim.a
		b:=pdim.b
		rx_unit(ttowner[m],pdim)

		case pdim.tag
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
	ttsize[m]:=size:=length*elemsize
	ttsizeset[m]:=1
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

	a:=p.a
	b:=p.b

	tpass(p)
end

global function tx_module(int n)int=
	modulerec m
	ref strec stmodule, d
	int globalflag,status

	currmoduleno:=n

	tx_passdef(moduletable[n].stmodule)

	return 1
end

global proc tx_passdef(ref strec p)=
	ref strec d
	int oldmlineno
	unit q

	if p.txdone then
		return
	fi

	oldmlineno:=mlineno
	mlineno:=p.pos

	d:=p.deflist
	while d do
		tx_passdef(d)
		d:=d.nextdef
	od

	q:=p.code

	case p.nameid
	when procid then
		currproc:=nil
	when constid,enumid then
		tx_namedconst(p)
	when staticid, frameid, paramid then
		tx_namedef(p)
	esac

	p.txdone:=1
	mlineno:=oldmlineno
end

proc tx_unitlist(unit p, int t=tany, lv=nolv)=
	while p do
		tpass(p,t)
		p:=p.nextunit
	od
end

proc tx_namedef(ref strec d)=
	int m,mold
	unit dcode,pequiv

	m:=d.mode
	setmodesize(m)

	if d.circflag then
		txerror("Circular reference detected")
	fi
	if d.txdone then return fi
	dcode:=d.code

	d.circflag:=1

	if d.atvar then
		pequiv:=d.equivvar
		if pequiv.tag=j_addrof then deleteunit(pequiv,pequiv.a) fi
		if pequiv.tag<>j_name then
			txerror("@name needed")
		fi
		tpass(pequiv)
	fi

	if dcode and d.nameid<>frameid then
		mold:=m
		m:=gettypebase(m)

		if ttbasetype[m]=tslice and dcode.tag=j_const and dcode.mode=trefchar then
			tpass(dcode,trefchar)
		else
			tpass(dcode,m)
		fi
		d.circflag:=0
		d.txdone:=1
		if ttbasetype[m]=tarray and ttlength[m]=0 then
			d.mode:=dcode.mode
		fi

		if mold<>m then
			if ttisinteger[m] and ttisshort[mold] then
				insertunit(d.code,j_shorten)
				d.code.mode:=mold
			elsif mold=tr32 then
				d.code.mode:=mold
			fi
		fi

		if d.nameid=staticid then
			checkconstexpr(d.code)
		fi

	elsif dcode and d.nameid=frameid and ttbasetype[m]=tarray and ttlength[m]=0 then
		tpass(dcode,m)
		d.mode:=dcode.mode
		d.circflag:=0
		d.txdone:=1

	else
		d.circflag:=0
		d.txdone:=1
	fi
end

global proc tx_namedconst(ref strec d)=
	int m

	if d.circflag then
		txerror("Circular const reference detected")
	fi

	unit q
	if d.txdone then return fi
	q:=d.code

	m:=d.mode

	d.circflag:=1
	tx_expr(q,(m=tauto|tany|m))

	d.circflag:=0
	checkconstexpr(q)
	if m=tauto then
		d.mode:=q.mode
	fi

	d.txdone:=1
end

proc tx_expr(unit p, int t=tany)=
	tpass(p,t)
end

proc checkconstexpr(unit p)=
!check whether p is const expr
	unit q
	int pmode

	case p.tag
	when j_const then
		return
	when j_makelist then
		q:=p.a
		while q do
			checkconstexpr(q)
			q:=q.nextunit
		od

	when j_convert then

		if ttbasetype[p.a.mode]=tref then
			if tttarget[p.a.mode]=tvoid then
				p.a.mode:=p.mode
				deleteunit(p,p.a)
			else
				goto error
			fi
		fi
		goto error

	when j_shorten then
		checkconstexpr(p.a)

	when j_addrof, j_addroffirst then
		case p.a.tag
		when j_name then
		else
			goto error
		esac

	when j_name then
		if p.def.nameid=fieldid then return fi
		error
	else
	error::
		println jtagnames[p.tag],STRMODE(P.MODE)
	PRINTUNIT(P)
		txerror("Getconstexpr: not const")
	esac
end

function getconstint(unit q, int t=tany)int64=
	checkconstexpr(q)

	if ttisinteger[q.mode] then
		if ttsize[q.mode]=16 then
			GERROR("GETCONSTINT/128")
		fi
		return q.value
	elsif ttisreal[q.mode] then
		return q.xvalue
	else
		cpl strmode(q.mode)
		txerror("Getconstint: not int32/64")
	fi
	return 0
end

proc makenewconst(unit p,int64 x,int t=tvoid)=
!modify p (usually a binop, monop, convert op etc) to a new const unit
!p will usually already have the result mode
!the x value will do for int/word/real

	p.tag:=j_const
	p.a:=p.b:=nil
	p.hasa:=p.hasb:=0
	p.value:=x
	p.isconst:=1
	if t<>tvoid then
		p.mode:=t
	fi
end

proc tx_name(unit p,int t,lv)=
	ref strec d
	int oldmlineno
	unit pcode
	oldmlineno:=mlineno

	d:=p.def
	mlineno:=d.pos

	switch d.nameid
	when constid,enumid then			!note: currently, rxpass converts names to constants

		if lv then txerror("&const") fi

		tx_namedconst(d)
		pcode:=d.code

		p.tag:=j_const
		p.def:=nil
		p.a:=nil
	    p.c:=nil
		p.hasa:=p.hasc:=0

		if pcode.tag=j_convert then		!assume c_soft
			p.value:=pcode.a.value

		else
			p.value:=pcode.value
		fi

		p.slength:=pcode.slength
		p.mode:=d.mode
		p.isconst:=1
		p.isastring:=pcode.isastring

	when staticid,frameid,paramid then

	if d.islet and lv then
		println D.NAME,=LV,D.ISLET
		txerror("Can't use 'let' as lvalue")
	fi

		tx_namedef(d)

		if not inassem then
			p.mode:=d.mode
			twiden(p,lv)
		else
			p.mode:=trefchar
		fi

	when procid,dllprocid then

		p.mode:=trefproc	!use generic refproc mode (yields return type of actual proc mode
				!after a call op, or actual refproc in other context. Don't use actual
				!refproc here, to avoid generating thousands of ref proc modes, one
				!for each call, that will never be needed

	when labelid,blockid then
		p.mode:=treflabel

	when moduleid then
		txerror_s("Module name can't be used on it's own: #",d.name)

	when fieldid then
		p.tag:=j_const
		p.def:=nil
		p.a:=nil
	    p.c:=nil
		p.hasa:=p.hasc:=0

		p.value:=d.offset

		p.mode:=ti64
		p.isconst:=1


	when typeid then
		p.tag:=j_typeconst
		p.value:=d.mode
		p.mode:=ti64

	when dllvarid then
		if d.code then
			txerror("Can't init dllvar")
		fi
	p.mode:=d.mode

	else
		MLINENO:=P.POS
		CPL NAMENAMES[D.NAMEID]
		txerror_ss("TNAME? # #",namenames[d.nameid],d.name)
	endswitch
	mlineno:=oldmlineno

end

proc tx_bin(unit p,a,b)=
!deal with most binary ops
	int amode,bmode,abase,bbase,cmode, relop

	tpass(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]
	relop:=0

!deal with possible asymmetric ops

	case p.pclop
	when kadd then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			if a.isastring and b.isastring then
				combinestrings(p)
				return
			fi

			txerror("ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddrefoff
			p.mode:=amode
			return
		fi
	when ksub then				!ref-int or ref-ref
		if abase=tref and bbase=tref then
			if not comparemodes(amode, bmode) then
				txerror("ref-ref: not compat")
			fi
			p.pclop:=ksubref
			p.mode:=ti64
			return
		elsif abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubrefoff
			p.mode:=amode
			return
		fi
	when kshl, kshr then
		coerceunit(b,ti64)
		p.mode:=amode
		return
	when keq, kne, klt, kle, kge, kgt then
		if abase=bbase=tref then
			p.mode:=ti64	
			return
		fi
		if p.pclop not in [keq, kne] then
			relop:=1
		fi

	when kin, knotin then
		if not isnum(abase) then txerror("IN lhs not int") fi
		case b.tag
		when j_makerange,j_makeset then
			p.tag:=(b.tag=j_makerange|j_inrange|j_inset)
			p.mode:=ti64
			if p.pclop=knotin then
				addnotl(p)
			fi
			return
		else
			txerror("IN ?")
		esac

	esac

	if isnum(abase) and isnum(bbase) then	!num op num

		if relop and ttsize[abase]<16 then

			if abase=tu64 and bbase<>tu64 then
				if b.tag=j_const and b.istrueconst and b.value.[63]=0 then
					bbase:=b.mode:=tu64
				fi
			elsif abase<>tu64 and bbase=tu64 then
				if a.tag=j_const and a.istrueconst and a.value.[63]=0 then
					abase:=a.mode:=tu64
				fi
			fi

			if abase=tu64 and bbase<>tu64 or abase<>tu64 and bbase=tu64 then
				txerror("Mixed sign")
			fi
		fi

		if typerank[abase]>=typerank[bbase] then
			cmode:=abase
		else
			cmode:=bbase
		fi

		if p.pclop=kdiv and ttisinteger[cmode] then
			p.pclop:=kidiv
		fi

		coerceunit(a,cmode)
		coerceunit(b,cmode)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode2(bmode))
		fi
		cmode:=amode
		p.pclmode:=getpclmode(getnewbase(abase))
	fi

	if intresult[p.pclop] or p.pclop=ksubrefoff then
		cmode:=ti64
	fi

	p.mode:=cmode
end

proc tx_binto(unit p,a,b)=
	int abase, bbase, amode,bmode, opc

	tpass(a,,needlv)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if p.pclop=kdivto and ttisinteger[abase] then
		p.pclop:=kidivto
	fi

	p.mode:=tvoid

	case p.pclop
	when kaddto then				!ref+ref not allowed; or ref+int (later refchar+refchar)
		if abase=tref and bbase=tref then
			txerror("to:ref+ref")
		fi
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=kaddrefoffto
			return
		fi
	when ksubto then				!ref-int
		if abase=tref and bbase<=tlastnum then
			coerceunit(b,ti64)
			p.pclop:=ksubrefoffto
			return
		fi
	when kshlto, kshrto then
		coerceunit(b,ti64)
		p.pclmode:=getpclmode(abase)
		return
	esac

	if isnum(abase) and isnum(bbase) then	!num op num
		p.pclmode:=getpclmode(abase)
		coerceunit(b,abase)

	elsif ttisshort[abase] and isnum(bbase) then
		p.pclmode:=getpclmode(abase)
		coerceunit(b,abase)

	else
		if not comparemodes(amode,bmode) then
			txerror_ss("BIN: modes not compatible: # #",strmode(amode),strmode(bmode))
		fi
		p.pclmode:=getpclmode(getnewbase(amode))
	fi
end

function getdominantmode(int amode,bmode)int=
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if abase<=tlastnum and bbase<=tlastnum then	!num op num
		if typerank[abase]>=typerank[bbase] then
			return abase
		else
			return bbase
		fi

	else
		if not comparemodes(amode,bmode) then
			txerror("Getdom: no dominant mode")
		fi
		return amode
	fi
end

function getdominantmodepp(unit a,b)int=
	int amode:=a.mode, bmode:=b.mode
	int abase,bbase

	abase:=ttbasetype[amode]
	bbase:=ttbasetype[bmode]

	if abase<=tlastnum and bbase<=tlastnum then	!num op num
		if typerank[abase]>=typerank[bbase] then
			return abase
		else
			return bbase
		fi

	else
		if not comparemodes(amode,bmode) then
			txerror("Getdom: no dominant mode")
		fi
		return amode
	fi
end

proc tx_cmp(unit p,a,b)=
	int abase,bbase,atype,btype,u,v

	tx_bin(p,a,b)
end

proc tx_cmpchain(unit p,a)=
	int u,genop
	unit q,r

	q:=a
	while q do
		tpass(q,tany)

		if q=a then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	q:=a
	r:=a.nextunit
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	for i:=1 to p.cmpgenop.len do
		genop:=p.cmpgenop[i]
		if genop=0 then exit fi

		p.cmppclmode[i]:=getpclmode(u)
	od

	p.mode:=ti64
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

	retry::

	case a.tag
	when j_name then
		d:=a.def

		if d.nameid in [procid, dllprocid] then
	getparams::
			e:=d.deflist
			while e do
				if e.nameid=paramid then
					if nparams>=maxparams then txerror("Param overflow") fi
					paramlist[++nparams]:=e
				fi
				e:=e.nextdef
			od

		else					!assume fn ptr
			while ttbasetype[a.mode]=tref do
				insertunit(a,j_ptr)
				a.mode:=tttarget[a.mode]
			od
			goto dorefproc
		fi

	when j_if,j_select then
		TXERROR("Can't do ifx/function")

	else
	dorefproc::
		if a.tag=j_dot then
			tmethodcall(p,a,pargs)
			a:=p.a
			pargs:=p.b
			goto retry
		fi

		if ttbasetype[a.mode]<>tproc then
			txerror("Function pointer expected")
		fi

		d:=ttnamedef[a.mode]

		if d=nil then txerror("Function expected") fi
		goto getparams
	esac

	q:=pargs
	while q do
		if nargs>=maxparams then txerror("Param overflow") fi
		arglist[++nargs]:=q
		q:=q.nextunit
	od

	p.mode:=d.mode				!type returned by function (will be void for procs)

	if p.mode=tvoid and p.tag=j_callfn then
		p.tag:=j_callproc
	fi

	if p.mode and t<>tvoid then
		twiden(p,nolv)
	fi

	if d.varparams then
		for i to nargs do

			if i<=nparams then
				tpass(arglist[i],paramlist[i].mode)
			else
				tpass(arglist[i])
			fi
		od
		if t=tvoid then
			p.tag:=j_callproc
		fi
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
		switch q.tag
		when j_keyword then
			name:=q.a.def.name
			for j to nparams do
				if eqstring(paramlist[j].name,name) then
					exit
				fi
			else
				txerror_s("Can't find kwd param: #",name)
			od

			if newarglist[j] then
				txerror_s("Kwd: # already used or was implicit",name)
			fi
			newarglist[j]:=q.b
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

	for i to nparams do
		q:=newarglist[i]			!will be nil of not supplied
		pm:=paramlist[i]			!formal param (an st entry)
		if q=nil then
			unless pm.optional then
				txerror_s("Param not optional: #",strint(i))
			end
			if pm.code then		!provide default value
				newarglist[i]:=duplunit(pm.code,p.lineno)
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

		if pm.parammode=out_param then
			tpass(q,,needlv)
			m:=tttarget[pm.mode]
			qm:=q.mode

			if not comparemodes(qm,m) then
				cpl =strmode(qm)
				cpl =strmode(m)
				txerror("&param: type mismatch")
			fi

			insertunit(q,j_addrof)
			q.mode:=createrefmode(nil,qm)

		else
			tpass(q,pm.mode)
		fi

		if ulist=nil then
			ulist:=q
		else
			ulistx.nextunit:=q
		fi
		ulistx:=q
		q.nextunit:=nil
	od
	p.b:=ulist

	if t=tvoid then
		p.tag:=j_callproc
	fi

end

proc tx_unary(unit p,a)=
	int opc,size,amode,mbase,tmax,x,xhigh

	tpass(a)
	amode:=a.mode

	switch p.pclop
	when klwb, kupb, klen, kbounds then
		do_bounds(p,a)
		return
	when kbytesize,kbitwidth then
		size:=ttsize[(a.tag=j_typeconst|a.value|amode)]*(p.pclop=kbytesize|1|8)
		makenewconst(p,size)
		p.mode:=ti64
		return
	when kminvalue, kmaxvalue then
		tmax:=ti64
		if a.tag=j_typeconst then
			mbase:=ttbasetype[a.value]
		else
			mbase:=ttbasetype[getmemmode(a)]
		fi

		if p.pclop=kminvalue then
			case mbase
			when ti8 then x:=-128
			when ti16 then x:=-32768
			when ti32 then x:=-2_147_483_648
			when ti64 then x:=int64.minvalue
			when ti128 then
				xhigh:=0x8000'0000'0000'0000
				x:=0
				tmax:=ti128
			when tu128 then
				x:=xhigh:=0
			when tu8,tu16,tu32,tu64,tu128,tc8,tc16,tc64 then x:=0
			else
 	           txerror_s("Can't do minvalue on #",strmode(mbase))
			esac
		else
			case mbase
			when ti8 then x:=127
			when ti16 then x:=32767
			when ti32 then x:=2_147_483_647
			when ti64 then x:=0x7fff'ffff'ffff'ffff
			when ti128 then
				x:=0xFFFF'FFFF'FFFF'FFFF
				xhigh:=0x7FFF'FFFF'FFFF'FFFF
				tmax:=ti128
			when tu8,tc8 then x:=255
			when tu16,tc16 then x:=65535
			when tu32 then x:=4294967295
			when tu64 then x:=0; --x; tmax:=tu64
			when tu128 then
				x:=0xFFFF'FFFF'FFFF'FFFF
				xhigh:=0xFFFF'FFFF'FFFF'FFFF
				tmax:=tu128
			else
				txerror_s("Can't do maxvalue on #",strmode(mbase))
			esac
		fi
		p.tag:=j_const
		p.a:=nil; p.hasa:=0
		p.value:=x
	    p.high128:=xhigh
		p.mode:=tmax
		p.isconst:=1
		return
	when katan, kln, kexp, ksqrt then
		if ttisinteger[amode] then coerceunit(a,amode:=tr64) fi
!	when ksin,kcos,ktan, kasin, kacos, ksqrt then
	when ksin,kcos,ktan, kasin, kacos then
		coerceunit(a,amode:=tr64)
	when ktypestr then
		p.tag:=j_const
		if a.tag=j_typeconst then
			amode:=a.value
		else
			amode:=getmemmode(a)
		fi

		p.mode:=trefchar
		p.hasa:=0
		p.svalue:=pcm_copyheapstring(strmode(amode))
		p.isastring:=1
		p.length:=strlen(p.svalue)
		return
	endswitch

	p.pclmode:=getpclmode(getnewbase(amode))
	p.mode:=amode
end

proc tx_unaryto(unit p,a)=
	int abase, amode, opc

	tpass(a,,needlv)

	amode:=a.mode

	p.pclmode:=getpclmode(amode)
	p.mode:=tvoid
end

proc tx_if(unit p,a,b,c,int t,lv) =
	int u

	tcond(a)

!process both branches even if one will never be executed (for typechecking etc)
	tpass(b,t,lv)
	if t<>tvoid and not c then
		txerror("if needs else")
	fi
	tpass(c,t,lv)

	if t=tany then			!unknown types (eg. print)
		u:=getdominantmodepp(b,c)
		coerceunit(b,u)
		coerceunit(c,u)
		p.mode:=u
	else				!know exactly what type needed
		p.mode:=t
	fi

	if iscondtrue(a) then		!branch b only
		deleteunit(p,b)
	elsif iscondfalse(a) then	!branch c only
		if c=nil then
			c:=createunit0(j_block)
		fi
		deleteunit(p,c)
	fi

end

proc tx_longif(unit p,a,b,int t,lv) =
	unit q,r
	int u

	u:=tvoid

	q:=a
	while q do				!all elseif unots
		tcond(q.a)
		r:=q.b
		tpass(r,t,lv)

		if t=tany then
			if u=tvoid then
				u:=r.mode
			else
				u:=getdominantmode(u,r.mode)
			fi
		fi

		q:=q.nextunit
	od

	if t<>tvoid and b=nil then
		txerror("longif needs else")
	fi
	tpass(b,t,lv)

	if t=tany then
		u:=getdominantmode(u,b.mode)
	fi

	if t<>tvoid then
		q:=a
		while q do				!all elseif unots
			if t=tany then
				coerceunit(q.b,u)
			fi
			q.mode:=q.b.mode
			q:=q.nextunit
		od
		if t=tany then
			coerceunit(b,u)
		fi
		p.mode:=b.mode
	fi
end

proc tx_incrto(unit p,a,int t)=
	tpass(a,,needlv)

	if t<>tvoid then
		case p.pclop
		when kincr then p.pclop:=kincrload
		when kdecr then p.pclop:=kdecrload
		esac
		p.mode:=gettypebase(a.mode)
	else				!a++ a-- to ++a --a
		case p.pclop
		when kloadincr then p.pclop:=kincr
		when kloaddecr then p.pclop:=kdecr
		esac
		p.mode:=tvoid
	fi

	p.pclmode:=getpclmode(a.mode)

	twiden(p,0)
end

proc tx_for(unit pindex,pfrom,pbody)=
	unit pto, pstep, plocal, plist
	int u

	pto:=pfrom.nextunit
	pstep:=pto.nextunit

	tpass(pindex)
	if pindex.tag<>j_name then
		txerror("Loop index not a variable")
	fi
	u:=pindex.mode
	tpass(pindex.nextunit)

	tpass(pfrom,u)
	tpass(pto,u)
	tpass(pstep,u)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_forall(unit pindex,plist,pbody)=
	unit plocal,pfrom,pto,passign
	int u,mlist,elemtype

	plocal:=pindex.nextunit
	pfrom:=plocal.nextunit
	pto:=pfrom.nextunit
	passign:=plist.nextunit

	tpass(pindex,ti64)
	tpass(pfrom,ti64)
	tpass(pto,ti64)

	tpass(plist)
	mlist:=plist.mode

	case ttbasetype[mlist]
	when tarray then
		elemtype:=tttarget[mlist]
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

	tpass(passign)

	tpass(pbody,tvoid)
	tpass(pbody.nextunit,tvoid)	!optional else
end

proc tx_index(unit p,a,b,int t,lv) =
!p is an index unit
!a is an array, b is an index
!t is the needed type for the element being indexed
	int amode,emode,pmode,tmode,tbasemode

	tpass(a,,lv)
	deref(a,t<>tvoid)
	amode:=a.mode

	tpass(b,ti64)			!index

	if ttbasetype[amode] not in [tarray, tslice] then
		txerror_s("Can't index: #",strmode(amode))
	fi
	p.mode:=tttarget[amode]
	twiden(p,lv)
end

proc tx_makerange(unit p,a,b)=
	int amode,bmode

	tpass(a)
	tpass(b)

	amode:=a.mode
	bmode:=b.mode

	if not ttisinteger[amode] or not ttisinteger[bmode] then
		txerror("range not int")
	fi

	if ttisint[amode] then
		coerceunit(a,ti64)
		coerceunit(b,ti64)
	else
		coerceunit(a,tu64)
		coerceunit(b,tu64)
	fi
	p.mode:=trange
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

		if not a.isconst then
			isconst:=0
		else
			case a.tag
			when j_makerange then
				lower min:=a.a.value
				upper max:=a.b.value
			when j_const then
				coerceunit(a,ti64)
				lower min:=y:=a.value
				upper max:=y:=a.value
			esac
		fi
		a:=a.nextunit
	od

	p.isconst:=isconst
end

proc tx_ptr(unit p,a,int t,lv)=
	ref strec d

	tpass(a)

	case ttbasetype[a.mode]
	when tvoid then
		txerror("Deref Void")
	when tref then
		p.mode:=tttarget[a.mode]

	when tslice then
		txerror("Can't deref slice")
	else
		txerror("PTR: need ref T")
	esac

	twiden(p,lv)
end

proc setrecordsize(int m)=
	[maxfields+8]ref strec fieldlist
	int i,nfields,indent,nrfields,size,index, maxalign
	ref strec d,e
	ref char flags
	const ss='S', ee='E'
	int flag

	if ttsize[m] then return fi

	d:=ttnamedef[m]
	e:=d.deflist
	nfields:=0

	fieldlist[++nfields]:=ref strec@(ss)

	while e do
		if e.nameid=fieldid then
			if nfields>=maxfields then
				gerror("srs:too many fields")
			fi

			setmodesize(e.mode)
			flags:=cast(&e.uflags)
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
		fi

		e:=e.nextdef
	od

	fieldlist[++nfields]:=ref strec@(ee)
	fieldlist[nfields+1]:=nil			!terminator

	countedfields:=0
	index:=2
	maxalign:=1
	scanrecord('S',&fieldlist,index,size,0, d.align, maxalign)

	if d.align then
		size:=roundoffset(size,maxalign)
		d.maxalign:=maxalign
	else
		d.maxalign:=1
	fi

	ttsize[m]:=size
	ttlength[m]:=countedfields
	ttlower[m]:=1
end

proc scanrecord(int state,ref[]ref strec fields, int &index, &isize, offset, calign, &maxalign)=
 	ref strec e,f,ea
	int size:=0,fieldsize,bitoffset:=0, alignment, newoffset

	while f:=fields^[index++] do
		case int(f)
		when 'S','U' then
			scanrecord(int(f),fields, index,fieldsize, offset, calign, maxalign)
		when 'E' then			!end of this nested block
			if state='U' then ++countedfields fi
			isize:=size
			return
		else
			if f.mode=tbitfield then
				fieldsize:=0
				ea:=f.equivfield
				f.offset:=ea.offset
				f.bitoffset:=bitoffset
				bitoffset+:=f.bitfieldwidth
				if bitoffset>ttsize[f.equivfield.mode]*8 then
					txerror("Bit fields overflow type")
				fi

			elsif f.atvar then
				bitoffset:=0
				e:=f.equivfield
				fieldsize:=0
				ea:=resolve_equiv_name(f.owner,e)
				f.offset:=ea.offset
			else
				bitoffset:=0
				if state='S' then ++countedfields fi
				fieldsize:=ttsize[f.mode]
				if calign then
					alignment:=getalignment(f.mode)
					if alignment>maxalign then maxalign:=alignment fi
					newoffset:=roundoffset(offset,alignment)
					size+:=newoffset-offset
				else
					newoffset:=offset
				fi
				f.offset:=newoffset
				offset:=newoffset
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

function roundoffset(int offset, alignment)int=
	int mask

	if alignment=1 then return offset fi
	mask:=alignment-1
	while offset iand mask do ++offset od

	return offset
end

proc tx_convert(unit p,a,int hard=0)=
	if a.tag=j_makelist then
		tx_makelist(a,a.a,p.convmode,nolv)
	else
		tpass(a)
		coerceunit(a,p.convmode,hard)
	fi
	deleteunit(p,a)			!get rid of this convert (may be replaced by new convert unit)
end

proc tx_makelist(unit p,a, int t,lv)=
	int alength,tlength,elemtype,newt, i, nfields,isconst, m
	unit q,b
	ref strec e

	alength:=p.length
	newt:=0
	isconst:=1

	tlength:=ttlength[t]

	if tlength then
		if alength<tlength then
			CPL =alength, =tlength
			txerror("Too few elements")
		elsif alength>tlength then
			CPL =alength, =tlength
			txerror("Too many elements")
		fi
	fi

	case ttbasetype[t]
	when tarray then
		elemtype:=tttarget[t]
		if tlength=0 then
			newt:=createarraymodek(nil, elemtype, ttlower[t],alength,0)
		else
			newt:=t
		fi
		q:=a
		while q do
			tpass(q,elemtype,lv)

			unless q.tag=j_const then isconst:=0 end
			q:=q.nextunit
		od

		p.mode:=newt

	when trecord then
		e:=ttnamedef[t].deflist
		q:=a
		while q and e do
			if e.nameid=fieldid then 
				while e.mode=tbitfield do
					e:=e.nextdef
					if not e then exit fi
				od

				tpass(q,e.mode,lv)
				unless q.tag=j_const then isconst:=0 end
				q:=q.nextunit
			fi

			e:=e.nextdef
		od
		while e and (e.nameid<>fieldid or e.mode=tbitfield) do
			e:=e.nextdef
		od
		if q or e then
			txerror("Can't initialise unions")
		fi
		p.mode:=t
	when tslice then
		if a=nil or (b:=a.nextunit; b=nil) or b.nextunit then
			txerror("bad slice init")
		fi
		p.b:=b
		p.hasb:=1
		a.nextunit:=nil
		tpass(a,,lv)
		if ttbasetype[a.mode]<>tref then txerror("slice init not ref") fi
		if tttarget[a.mode]<>tvoid then
			if not comparemodes(tttarget[a.mode],tttarget[t]) then
				txerror("slice/ptr mismatch")
			fi
		fi

		tpass(b,ti64)
		p.mode:=t
		p.tag:=j_makeslice
		p.resultflag:=1

	when tvoid then
		q:=a
		if p.makearray then
			if q=nil then txerror("array()?") fi
			tpass(q,,lv)
			m:=q.mode
			q:=q.nextunit
		else
			TXERROR("MAKELIST1")
		fi

		while q do
			tpass(q,m,lv)
			unless q.tag=j_const then isconst:=0 end
			q:=q.nextunit
		od

	else
		txerror_s("Unknown makelist type: #",strmode(t))
	esac

	p.isconst:=isconst

	tpass(p.b,ti64)

end

proc tx_dot(unit p,a,b,int lv)=
	int recmode,recbasemode,i,j,newtag,tmode
	unit q,pindex
	ref strec d,dequiv

	tpass(a)			!lhs, yeields ref array type

	recmode:=a.mode

	recbasemode:=ttbasetype[recmode]

	while recbasemode=tref do
		tmode:=tttarget[recmode]
		insertunit(a,j_ptr)
		recmode:=a.mode:=tmode
		recbasemode:=ttbasetype[recmode]
	od

	if ttbasetype[recmode]<>trecord then
		txerror("Bad record type")
	fi

	d:=b.def

	if d.nameid=nullid then			!not resolved; lhs mode wasn't available
		d:=b.def:=resolvefield(d,recmode)
	fi

	if d.mode=tbitfield then
		i:=d.bitoffset
		j:=i+d.bitfieldwidth-1
		dequiv:=d.equivfield
		b.def:=dequiv				!change from bitfield field to containing int
		b.mode:=dequiv.mode
		p.offset:=d.offset

		if i=j then					!single bit
			pindex:=createconstunit(i,ti64)
			newtag:=j_dotindex
		else						!bit slice
			pindex:=createunit2(j_makerange,createconstunit(i,ti64),createconstunit(j,ti64))
			pindex.mode:=trange
			pindex.a.resultflag:=1
			pindex.b.resultflag:=1
			newtag:=j_dotslice
		fi

		p.mode:=b.mode
		twiden(p,lv)
		insertunit(p,newtag)
		p.mode:=tu64
		p.b:=pindex
		p.hasb:=1
		p.a.resultflag:=1
		p.b.resultflag:=1
		p.resultflag:=1

		return

	fi

	b.mode:=d.mode
	p.mode:=d.mode

	p.offset:=d.offset
	twiden(p,lv)
end

function resolvefield(ref strec d, int m)ref strec=
	ref strec e,t

	case ttbasetype[m]
	when trecord then
	when tref then
		m:=tttarget[m]
		if ttbasetype[m]<>trecord then
			txerror("3:record expected")
		fi
	else
		txerror("4:record expected")
	esac
	t:=ttnamedef[m]

	e:=finddupl(t,d)
	if not e then
		txerror_s("Not a field: #",d.name)
	fi
	return e
end

proc tx_andl(unit p,a,b)=
	tpass(a)
	tpass(b)

	p.mode:=ti64

!assume both aren't const, as that would be evaluated
	if iscondfalse(a) or iscondfalse(b) then
		makenewconst(p,0,ti64)
	elsif iscondtrue(a) then
		deleteunit(p,b)
	elsif iscondtrue(b) then
		deleteunit(p,a)
	fi

end

proc convintconst(unit p,int64 x)=				!CONVINTCONST
!convert unit p into int const x
	p.tag:=j_const
	p.mode:=ti64
	p.a:=p.b:=p.c:=nil
	p.hasa:=p.hasb:=p.hasc:=0
	p.value:=x
	p.isconst:=1
end

proc tx_sliceptr(unit p,a)=
	int m,tmode

	tpass(a)
	m:=a.mode

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

	p.mode:=createrefmode(nil,tmode)
end

proc tx_swap(unit p,a,b)=
	tpass(a,,needlv)
	tpass(b,,needlv)

	if not comparemodes(a.mode,b.mode) then
		txerror("SWAP: type mismatch")
	fi

	p.mode:=tvoid
end

proc tx_select(unit p,a,b,c, int t,lv)=
	int i,u
	unit q

	tpass(a,ti64)

	q:=b
	while q do
		tpass(q,t,lv)
		if q=b then
			u:=q.mode
		else
			u:=getdominantmode(u,q.mode)
		fi

		q:=q.nextunit
	od

	tpass(c,t,lv)
	u:=getdominantmode(u,c.mode)

	q:=b
	while q do
		coerceunit(q,u)
		q:=q.nextunit
	od

	p.mode:=u
end

proc tx_case(unit p,a,b,c, int t,lv)=
	int amode,u
	unit wt,w

	if p.tag=j_docase and lv then gerror("&docase") fi

	tpass(a)

	if a=nil then
		amode:=tany
	else
		amode:=a.mode
	fi

	if ttisinteger[amode] and ttsize[amode]<8 then
		coerceunit(a,tint)
		amode:=tint
	fi
	u:=tvoid

	wt:=b
	while wt do				!whenthen chain
		w:=wt.a
		while w do				!each expr between when...then
			tpass(w)
			if w.tag=j_makerange then
				unless ttisinteger[amode] then txerror("case: need int index") end
			else
				if amode=tany then
						if not isboolunit(w) then
							TXERROR("CASE/BOOL?")
							insertunit(w,j_istruel)
						fi
				else
					coerceunit(w,amode)
				fi
			fi
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)			!process block
		if t<>tvoid then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi
		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
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
	p.mode:=ti64
end

proc tx_istruel(unit p,a)=
	int abase

	tpass(a)

	if isboolunit(a) then
		deleteunit(p,a)
		return
	fi

	abase:=ttbasetype[a.mode]
	if abase=tref then abase:=ti64 fi

	p.mode:=ti64
	p.pclmode:=getpclmode(abase)
end

proc tx_typepun(unit p,a)=
	int smode
	case a.tag
	when j_makelist then
		TXERROR("TYPEPUN/LIST")
	else
		tpass(a)

		smode:=getmemmode(a)

		if ttsize[smode]<ttsize[p.convmode] then
			txerror("Typepun: sizes must match")
		fi

		p.mode:=gettypebase(p.convmode)
	esac
end

proc tx_bytesize(unit p,a)=
	tpass(a)
	p.mode:=ti64
end

proc tx_exit(unit p,a)=
	if a=nil then return fi
	tpass(a,ti64)
	if a.tag<>j_const then
		txerror("exit/etc not const")
	fi
	p.index:=a.value
	p.a:=nil
	p.hasa:=0
end

proc tx_goto(unit p,a)=
	int m

	tpass(a)
	m:=a.mode

	if ttbasetype[m]<>tref or ttbasetype[tttarget[m]]<>tlabel then
		txerror("goto: not label")
	fi
end

proc tx_switch(unit p,a,b,c,int t,lv)=
	[0:2048]byte valueset
	unit wt, w
	int ax,bx,i,u

	if p.tag=j_doswitch and lv then gerror("&doswitch") fi

	tpass(a,ti64)

	memset(&valueset,0,valueset.bytes)
	u:=tvoid

	wt:=b
	while wt do

		w:=wt.a
		while w do
			tpass(w)

			if not isconstunit(w) then txerror("Switch not constant") fi

			case ttbasetype[w.mode]
			when trange then			!assume makerange
				ax:=w.a.value
				bx:=w.b.value
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
				ax:=bx:=w.value
				goto dorange
			esac
			w:=w.nextunit
		od
		tpass(wt.b,t,lv)

		if t=tany then
			if u then
				u:=getdominantmode(u,wt.b.mode)
			else
				u:=wt.b.mode
			fi
		fi

		wt:=wt.nextunit
	od

	if c then
		tpass(c,t,lv)
		if t=tany then
			u:=getdominantmode(u,c.mode)
		fi
	elsif t<>tvoid then
		txerror("switch needs else")
	fi

	if t<>tvoid then
		w:=b.a
		while w do				!all elseif unots
			if t=tany then
				coerceunit(b.b,u)
			fi
			w.mode:=b.b.mode
			w:=w.nextunit
		od
		if t=tany then
			coerceunit(c,u)
			p.mode:=u
		else
			p.mode:=t
		fi
	else
		p.mode:=tvoid
	fi
end

proc tx_addroffirst(unit p,a,int t)=
!&.x maps to &x[x.lwb]
	int m

	tpass(a)
	m:=a.mode
	if ttbasetype[m]<>tarray then
		txerror("&. ref[] expected")
	fi

	m:=createrefmode(nil,tttarget[m])
	if a.tag=j_name then
		a.addroffirst:=1
	fi
	p.mode:=m
end

proc tx_return(unit p,a, int t)=
 	int m,nvalues,nret,i
	ref[]int32 pmult
	unit q

	m:=currproc.mode
	nret:=currproc.nretvalues
	pmult:=ttmult[currproc.mode]

	if a=nil then
		if nret then
			txerror("return value(s) missing")
		fi
		return
	elsif nret=0 then
		txerror("Superfluous return value")
	fi

	if a.tag=j_makelist then
		a.tag:=j_returnmult
		if a.length<>nret then
			case ttbasetype[m]
			when trecord, tarray then
				txerror("return constructor not supported")
			else
				txerror("Wrong number of return values")
			esac
		fi
		q:=a.a				!point to list of return values
		for i to nret do
			tpass(q,pmult[i])
			q:=q.nextunit
		od

		deleteunit(p,a)			!don't need return
		if t=tvoid then
			p.mode:=tvoid
		else
			p.mode:=ttuple
		fi

	else
		if nret>1 then txerror("RETERROR?") fi
		tpass(a,m)

		if t=tvoid then					!regular out-of-line return
			p.mode:=tvoid
		else
			deleteunit(p,a)
!			P.MODE:=A.MODE
		fi
	fi

	IF TTISSHORT[P.MODE] THEN TXERROR("SHORT RET TYPE") FI
end

proc tx_dotindex(unit p,a,b,int lv) =
!a.[b], a is an int
	int pmode
	unit i,j

	tpass(a,,lv)			!lhs

	pmode:=tu64

	if not ttisinteger[a.mode] then
		txerror("a.[i]: not int/str value")
	fi

	tpass(b)			!index

	case ttbasetype[b.mode]
	when trange then
		i:=b.a
		j:=b.b
		if i.tag=j.tag=j_const then
			if i.value>j.value then
				swap(b.a,b.b)
			fi
		fi
	else					!assume simple index
		coerceunit(b,ti64)
	esac

	p.mode:=pmode
end

proc tx_slice(unit p,a,b) =
!a[b], b is a rtange

	tpass(a)			!lhs
	tpass(b)			!will be a range

	if a.mode=trefchar then
		p.mode:=createslicemodek(currproc,tc8,1,0)
	else
		deref(a)
		case ttbasetype[a.mode]
		when tarray then
			p.mode:=createslicemodek(currproc,tttarget[a.mode],1, 0)

		when tslice then
			p.mode:=a.mode

		else
			CPL =STRMODE(A.MODE)
			txerror("a[i..j]: not array")
		esac
	fi
end

proc tx_assign(unit p,a,b,int t)=
	int m,mm
	ref strec d

	case a.tag
	when j_makelist then
		tx_multassign(a,b)
	when j_dotindex, j_dotslice then
		tx_dotindex(a,a.a,a.b,needlv)
		tpass(b,a.mode)
		p.mode:=ti64
	else
		if a.tag=j_name and a.def.islet and p.initlet then
			tpass(a)
		else
			tpass(a,,needlv)
		fi
		m:=a.mode

		a.resultflag:=t<>tvoid

		if ttbasetype[m]=tslice and b.tag=j_makelist then
			tx_makelist(b,b.a,m,0)

		elsif ttisshort[m] and t<>tvoid then
			p.memmode:=m
			p.mode:=gettypebase(m)
			tpass(b,p.mode)

		else
			if b.pclop in [kidiv, kirem] then		!CAN'T JUST OVERRIDE MODE
				tpass(b)
			elsif b.tag=j_read then
				tpass(b,m)
			else
				mm:=m
				if ttisshort[m] then
					mm:=gettypebase(m)
				fi
				case b.tag
				when j_autocast then
					tpass(b,mm)
				when j_makelist then
					tpass(b,m)
				else
					tpass(b)
				esac
				if ttbasetype[b.mode]=ttuple then
					d:=getprocretmodes(b)
					coerceunit(a,ttmult[d.mode,1])
					p.mode:=a.mode
				else
					coerceunit(b,mm)
					p.mode:=mm
				fi
			fi
		fi
	esac
end

proc tx_multassign(unit a,b)=
!a is a multexpr; b might be multexpr, or a function with multiple returns
	unit p,q,lhs,rhs
	int nretmodes,i
	ref[]int32 pmult
	ref strec d				!point to def containing return mode info

	nretmodes:=0

	if b.tag<>j_makelist then

		tpass(b)
		d:=getprocretmodes(b)
		nretmodes:=d.nretvalues

		if ttbasetype[d.mode]<>ttuple then txerror("Not a tuple") fi

		if a.length>nretmodes then
			txerror("mult ass/mult returns don't agree in number")
		fi
		if nretmodes<=1 then
			txerror("mult ass rhs needs fn yielding 2+ values")
		fi

		p:=a.a
		pmult:=ttmult[d.mode]
		i:=1

		while p do
			tpass(p,,needlv)
			if p.mode<>pmult[i++] then
				txerror("mult ass/mult fn needs exact type match")
			fi
			p:=p.nextunit
		od
		return
	fi

	if a.length<>b.length then
		txerror("Mult assign: count mismatch")
	fi
	if a.length=0 then
		txerror("Invalid assignment")
	fi
	rhs:=b.a
	lhs:=a.a

	p:=lhs
	while p do
		tpass(p,,needlv)
		p:=p.nextunit
	od

	p:=lhs

	q:=rhs
	while q do
		tpass(q,p.mode)
		p:=p.nextunit
		q:=q.nextunit
	od
end

proc tx_exprlist(unit p,a,int t)=
	unit q

	q:=a
	while q and q.nextunit do
		tpass(q)
		q:=q.nextunit
	od

!q points to last expr
	tpass(q,t)
	p.mode:=q.mode
end

proc twiden(unit p, int lv)=
!intended for widening narrow types for memory access nodes Name, Index, Dot, Ptr.
!But will also be used to generally apply
	int m,u,mbase

	mbase:=ttbasetype[m:=p.mode]

	if mbase=tvoid then return fi		!nothing to widen (error?)
	if lv then return fi				!lv, keep memory mode as dest

	if not ttisshort[mbase] then return fi	!no widening needed
	case p.tag
	when j_name, j_ptr, j_index, j_dot then
			p.memmode:=m				!non-void marks this as non-lv too
			p.mode:=gettypebase(m)
	when j_callproc,j_callfn then
		p.memmode:=m
		p.mode:=gettypebase(m)
	else
		PRINTUNIT(P)
		txerror_s("widen? #",jtagnames[p.tag])
	esac
end

proc removeaddrof(unit p)=
!p is a lhs of dot operator used for flex/var
!will need to remove any addrof that has been applied
	if p=nil then return fi
	case p.tag
	when j_addrof then
		deleteunit(p,p.a)
	when j_if then
		removeaddrof(p.b)
		removeaddrof(p.c)
	else
		txerror("dot/flex: complex record expr, can't remove &")
	esac

end

proc tstringslice(unit p, int slicemode)=
!p is a string; insert conversions to turn it into a slice:
	unit a,b,prange
	int length

	if tttarget[slicemode]<>tc8 then
		txerror("Not char slice")
	fi
!
	a:=p
	insertunit(p,j_slice)


	if p.a.tag=j_const then
	else
		b:=duplunit(p.a)
		insertunit(b,j_unary)
		prange:=createunit2(j_makerange,createconstunit(1,ti64),b)

		prange.mode:=trange
		p.b:=prange
		p.hasb:=1
	fi

	p.mode:=slicemode
end

proc tx_bitfield(unit p,a,int lv)=
	int i,j,bitsize,topbit
	unit r

	tpass(a,,lv)

	if not ttisinteger[a.mode] and not ttisref[a.mode] then
		txerror("Int/ref needed")
	fi

	bitsize:=ttsize[ttbasetype[a.mode]]*8
	topbit:=bitsize-1

	case p.bfcode
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
	else
		CPL P.BFCODE
		TXERROR("BITFIELD")
	esac

	if i=j then			!single bit
		p.tag:=j_dotindex
		p.b:=createconstunit(i,ti64)
		p.hasb:=1
		p.resultflag:=1
		p.b.resultflag:=1

		if p.bitopindex=bf_even then
			p.mode:=tu64
			addnotl(p)
		fi

	else
		r:=createunit2(j_makerange,createconstunit(i,ti64),createconstunit(j,ti64))
		r.a.resultflag:=1
		r.b.resultflag:=1
		r.mode:=trange
		p.tag:=j_dotslice
		p.hasb:=1
		p.b:=r
	fi

	p.mode:=tu64
end

proc deref(unit a, int needres=1)=
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

proc tmethodcall(unit p, pdot, pargs)=
	int mrec
	unit prec, pfield, pfunc
	ref strec d,e

	prec:=pdot.a
	pfield:=pdot.b
	mrec:=prec.mode
	d:=pfield.def

	e:=resolvefield(d,mrec)

	if e=nil then
		txerror_s("Can't resolve method:",d.name)
	fi

	pfunc:=createname(e)
	pfunc.mode:=e.mode
	prec.nextunit:=pargs

	p.a:=pfunc
	p.b:=prec
end

function softconvert(int s,t,hard=0)int=
!see what implicits are needed t convert s to t
!return value will be:
!	kerror		No implicit conversions exist
!	op_softconv		No conversion needed (eg. i64 to u64, or ref void to ref T)
!	op_widen_...	Etc, an actual op index

	int sbase, tbase

	if s=t then return ksoftconv fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]

	if sbase<=tlastnum and tbase<=tlastnum then		!both numeric
		return softconvtable[sbase,tbase]
	fi

	if sbase=tbase=tref then
		if s=tref or t=tref then						!at least one is ref void
			return ksoftconv
		fi												!ref T/ref U
		return (comparemodes(s,t)|ksoftconv|kerror)
	fi

	if sbase=tarray and t=trefchar and tttarget[s]=tc8 then
		return kcharaxtoichar
	fi

	if not hard and sbase<=tlastnum and ttisshort[tbase] then
		return ksofttruncshort		!soft truncate needed for idata
	fi

!a few other coercions to be added later, such as refchar to slice/string
	if sbase=tbase=tarray then
		if comparemodes(s,t) then
			return ksoftconv
		fi
	fi

	if sbase=tarray and tbase=tslice then
		if not comparemodes(tttarget[s],tttarget[t]) then
			txerror("Bad array to slice")
		fi
		return karraytoslice
	fi
	if s=trefchar and tbase=tslice then
		if tttarget[t] not in [tu8,tc8] then
			txerror("Bad string to slice")
		fi

		return kichartoslice
	fi
	if sbase=tslice and tbase=tslice then
		if comparemodes(s,t) then
			return ksoftconv
		fi
	fi

	return kerror
end

function comparemodes(int s,t)int=
!return 1 if modes s,t are compatible. That is, ref s/ref t would be interchangeable.
!a direct compare may be false because refs/arrays but be constructed at
!different times
	int sbase, tbase, starg, ttarg
	ref strec d,e

	if s=t then return 1 fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]

	case sbase
	when tref then
		starg:=tttarget[s]
		ttarg:=tttarget[t]
		if starg=tvoid or ttarg=tvoid then
			return 1
		fi

		return comparemodes(tttarget[s],tttarget[t])

	when tarray then
		if comparemodes(tttarget[s],tttarget[t]) and (ttlength[s]=ttlength[t] or
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
	elsif sbase=tc8 and tbase=tu8 or sbase=tu8 and tbase=tc8 then
		return 1
	else
!else needs complex param/result-matching
!...
	esac
	return 0
end

function hardconvert(int s,t)int=
!an explicit conversion from s to t has been requested
!see if it is possible, and return an approrpiate conversion
	int opc, sbase, tbase
	int sint, tint, sref, tref

	opc:=softconvert(s,t,1)				!implicit conversion anyway?
	if opc<>kerror then				!yes
		return opc
	fi

	if s=tvoid then						!assume t<>tvoid, as that is tested above
		txerror("Non-void type expected")
	fi

	sbase:=ttbasetype[s]
	tbase:=ttbasetype[t]
	sref:=ttisref[s]
	tref:=ttisref[t]
	sint:=ttisinteger[s]
	tint:=ttisinteger[t]

	if sref and tint or sint and tref or sref and tref then
		return ksoftconv
	elsif sbase=tenum and tint or sint and tbase=tenum then
		return ksoftconv
	elsif sint and ttisshort[tbase] then
		return (ttsize[sbase]=16|ksofttruncw|ktruncate)
	fi

	return kerror
end

proc applyconversion(unit p, int s,t, opc)=
	int cmpop
!
	case opc
	when kerror then
		txerror("No conversion possible")
	when ksoftconv then
		p.mode:=t
		return
	when ksofttruncshort then
		insertunit(p,j_shorten)
		p.mode:=t			!don't use the short target mode
		return

	when karraytoslice then
		insertunit(p,j_slice)
		p.mode:=t
		return
	when kichartoslice then
		tstringslice(p,t)
		return

	when kcharaxtoichar then
		insertunit(p,j_addroffirst)
		p.mode:=trefchar
		return

	esac

	if tevalconvert(p,s,t,opc) then		!try and apply it directly
		return
	fi

!have to add an explict conversion node
	insertunit(p, j_convert)
	p.pclop:=opc

	p.convmode:=s
	p.resultflag:=1

	if ttisshort[t] then
		p.convmode:=t
		t:=gettypebase(t)
	fi

	p.mode:=t
end

proc coerceunit(unit p, int t, hard=0)=
	int opc,s

	s:=p.mode

	if t=tvoid or s=t then return fi
	if s=tvoid and t<>tvoid then
		txerror("Void type in expression/return value missing")
	fi

	if hard then
		opc:=hardconvert(s,t)
	else
		opc:=softconvert(s,t)
	fi

	if opc=kerror then
		println strmode(s),"=>",strmode(t)
		if not hard and hardconvert(s,t)<>kerror then
			txerror("Need explicit conversion")
		else
			txerror("Can't do conversion")
		fi
	fi

	applyconversion(p,s,t,opc)
end

function tevalconvert(unit p,int s,t,opc)int=
!conversion op opc to convert from s to t is about to be applied to be
!try and do that at compile time to avoid adding a runtime conversion
!return 1 if it could apply it, 0 if it couldn't
!caller should have already evaluated p to reduce constants etc
	real x,z
	int a,c
	int128 aa
!
	if p.tag<>j_const then
		return 0
	fi

	case pr(s,    t)
	when pr(ti64, tr64) then
		z:=p.value

	when pr(tr64, ti64) then
		c:=p.xvalue

	when pr(tr64, tr32) then
		z:=real32(p.xvalue)
	else
		return 0
	esac

	if ttisreal[t] then
		makenewconst(p,int64@(z),t)

	else
		makenewconst(p,c,t)
	fi

	return 1
end

proc do_bounds(unit p,a) =
	int m,mbase,opc,lower,upper

	deref(a)

	m:=a.mode
	if a.tag=j_typeconst then m:=a.value fi

	mbase:=ttbasetype[m]
	p.mode:=ti64

	case p.pclop
	when klwb then
		case mbase
		when tarray,tslice then
			convintconst(p,ttlower[m])
			return
		else
error::
			txerror("lwb/upb/len?")
		esac

	when kupb then
		case mbase
		when tarray then
			convintconst(p,ttlower[m]+ttlength[m]-1)
		when tslice then
			p.pclop:=kupb
		else
			goto error
		esac

	when klen then
		case mbase
		when tarray then
			convintconst(p,ttlength[m])
		when tslice then
			p.pclop:=klen
		else
			goto error
		esac
	when kbounds then
		p.mode:=trange
		case mbase
		when tarray then
			p.range_lower:=ttlower[m]
			p.range_upper:=p.range_lower+ttlength[m]-1
			p.tag:=j_const
			p.a:=p.b:=p.c:=nil
			p.hasa:=p.hasb:=p.hasc:=0
			p.isconst:=1
			return

		when tslice then
			p.pclop:=kbounds
		when ti32 then
			convintconst(p,int32.max-int32.min+1)
			return
		else
			goto error
		esac
	esac
end

proc addnotl(unit p)=
	insertunit(p,j_notl)
	p.mode:=ti64
	p.pclop:=knotl
end

proc tevaluate(unit p)=
	unit a,b,pname
	int offset

	int tag:=p.tag

	if jisexpr[tag]=2 then
		tevalbinop(p)

	elsif jisexpr[tag]=1 then
		tevalmonop(p)

	elsecase tag
	when j_makerange then
		a:=p.a
		b:=p.b
		if ttsize[a.mode]<=8 then			!const range only for 32-bits
			tevaluate(a)
			tevaluate(b)
			if a.tag=j_const and b.tag=j_const then
				p.isconst:=a.isconst iand b.isconst
			fi
		fi

	when j_addrof then
		a:=p.a

		pname:=addrdotindex(a, offset)

		if pname then
			deleteunit(a,pname)
			if p.b=nil then
				p.hasb:=1
				p.b:=createconstunit(offset,ti64)
			else 
				p.b.value+:=offset
			fi
		fi
	fi

end

function addrdotindex(unit p, int &offset)unit q=
	int axmode

	case p.tag
	when j_dot then
		if p.a.tag=j_name then
			offset:=p.offset
			return p.a
		else
			q:=addrdotindex(p.a,offset)
			offset+:=p.offset
			return q
		fi
	when j_index then
		axmode:=p.a.mode
		if p.b.tag=j_const then
			if p.a.tag=j_name then
				offset:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				return p.a
			else
				q:=addrdotindex(p.a,offset)
				if q then
					offset+:=(p.b.value-ttlower[axmode])*ttsize[tttarget[axmode]]
				fi
				return q
			fi
		else
			return nil
		fi
	else
		return nil
	esac

end

proc tevalbinop(unit p)=
	int64 a,b,c,offset
	real x,y,z
	unit lhs, rhs

	lhs:=p.a
	rhs:=p.b

	unless lhs.tag=rhs.tag=j_const then
		if lhs.tag=j_addrof and rhs.tag=j_const then
			if lhs.a.tag=j_name then			!reduce addrof(a)+k => addrof(a,k)
				offset:=rhs.value*ttsize[tttarget[lhs.mode]]
				if lhs.b=nil then
					lhs.hasb:=1
					lhs.b:=createconstunit(offset,ti64)
				else
					lhs.b.value+:=offset
				fi
				deleteunit(p,lhs)
			fi
		fi
		return
	end

	if ttisreal[p.mode] then
		x:=p.a.xvalue
		y:=p.b.xvalue
	else
		a:=p.a.value
		b:=p.b.value
	fi

	case p.mode
	when ti64, tu64 then

		switch p.pclop
		when kadd then c:=a+b

		when ksub then c:=a-b

		when kmul then c:=a*b

		when kidiv then c:=a/b

		when kshl then c:=a<<b

		when keq then c:=a=b

		when kne then c:=a<>b

		when klt then c:=a<b

		when kle then c:=a<=b

		when kge then c:=a>=b

		when kgt then c:=a>b

		when kandl then c:=a and b

		when korl then c:=a or b

		when kiand then c:=a iand b
		when kior then c:=a ior b
		else
			return
		end

	when tr64,tr32 then

		switch p.pclop
		when kadd then z:=x+y

		when ksub then z:=x-y

		when kmul then z:=x*y
		when kdiv then z:=x/y

		else
			return
		end
	else
		return
	esac
!
	if ttisreal[p.mode] then
		makenewconst(p,int64@(z))
	else
		makenewconst(p,c)
	fi
end

proc tevalmonop(unit p)=
	int64 a,b,c
	real x,z
	ref int128 q

	unless p.a.tag=j_const then
		return
	end

	a:=p.a.value
	x:=p.a.xvalue

	case p.mode
	when ti64, tu64 then

		switch p.pclop
		when kneg then c:=-a

		when kistruel then c:=istrue a

		when knotl then c:=not a
		when kinot then c:=inot a
		when kabs then c:=abs a

		else
			return
		end switch
	when tr64, tr32 then
		switch p.pclop
		when kneg then z:=-x
		when katan then z:=atan(x)
		when ksqrt then z:=sqrt(x)

		else
			return
		end switch
	else
		return
	esac

	if ttisreal[p.mode] then
		makenewconst(p,int64@(z))
	else
		makenewconst(p,c)
	fi
end

function getnewbase(int m)int=
	m:=ttbasetype[m]
	case m
	when tarray, trecord then
		return tblock
	esac
	return m
end

function iscondtrue(unit p)int =
	if p.tag=j_const and p.value<>0 then 1 else 0 fi
end

function iscondfalse(unit p)int =
	if p.tag=j_const and p.value=0 then 1 else 0 fi
end

proc fixchararray(unit a)=
!turn []char into ichar at certain points
	if a and ttbasetype[a.mode]=tarray and tttarget[a.mode]=tc8 then
		coerceunit(a,trefchar,0)
	fi
end

proc combinestrings(unit p)=
!p is (add, a, b) where a and b are string constants.
	unit a:=p.a, b:=p.b
	int alen:=a.length
	int blen:=b.length
	int clen:=alen+blen
	ichar s

	if blen=0 then
		deleteunit(p,a)
		return
	elsif alen=0 then
		deleteunit(p,b)
		return
	fi

	s:=pcm_alloc(clen+1)
	memcpy(s,a.svalue,alen)
	memcpy(s+alen,b.svalue,blen)
	(s+clen)^:=0

	deleteunit(p,a)
	p.length:=clen
	p.svalue:=s

end

proc tx_strinclude(unit p,a)=
	int fileno

	tpass(a)
	if a.tag<>j_const or not a.isastring then
		txerror("strincl/not string")
	fi

!	moduleno:=p.moduleno
	fileno:=moduletable[p.moduleno].fileno

!CPL "STRINCL",=MODULETABLE[MODULENO].NAME
!CPL =fileno
!CPL =SOURCEFILEPATHS[fileno]
!
!OS_GETCH()
	fileno:=getsupportfile(a.svalue,path:sourcefilepaths[fileno],issupport:1)

	a.svalue:=sourcefiletext[fileno]
	a.slength:=sourcefilesizes[fileno]
!
	deleteunit(p,a)
end
=== pc_decls.m 0 0 17/44 ===
export type psymbol = ref pstrec
export type pcl = ref pclrec

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record pstrec =
	ichar name			!name of symbol (named token/keyword or identifier)
	ref fwdrec fwdrefs	!fwd ref chain
	pcl pcldef			!points to pcl instruction that defines this name

	byte symbol			!type of token, eg. namesym
	byte ksymbol		!type of keyword, eg. opcodesym
	union
		byte subcode		!when used as keyword
		byte rtsindex	!proc names: rts index if it's an rts function
	end
	byte pclop			!pcl op used to define this: klocal etc

	byte scope			!label pass 1: fwd/extern/local/global
	byte reftype		!label pass 2: extern/back/fwd
	byte segment		!label pass 2: code_seg etc or 0
	byte namelen

	int32 offset		!label (pass 2): offset of label when encountered
	int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
	int32 labelno		!assigned global label when a proc/static
	int16 importindex	!genexe: index into import table

	word16 flags:(isdefined:1, isimported:1, addrof:1,
				isexported:1, isfloat:1, noreg:1, atvar:1,
				isthreaded:1, istruename:1, isrts:1)

	int16 nrefs
	int16 impindex
	int16 expindex
	byte reg
	byte spare
	psymbol nextsym
!	[9]byte spare
end

export record pclrec =
	byte opndtype
	byte opcode
	byte flags:(isexported:1, isimported:1)
	byte mode				!t in tables

	int32 size

	union
		struct
			union
				int64 value
				real64 xvalue
				real32 xvalue32
				ichar svalue
				int tempno
				int labelno
				psymbol def
				ref void asmcode
			end
			union						!two 32-bit params used according to opcode
				struct
					int32 x				!common access to these 1/2 extra attribs
					int32 y
				end

				struct					! (x,y) pointer ops
					int32 scale			! scale factor for offset
					int32 extra			! extra constant byte offset, already scaled
				end
				struct					! (x,y) call/etc
					int32 nargs			! number of args
					int32 nvariadics	! 0, or arg # that is first variadic
				end
				struct					! (x,y) switch
					int32 minlab
					int32 maxlab
				end

				int32 oldmode			! (x) u in tables
				int32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
				int32 truncmode			! (x) convert/truncate: truncated mode
				int32 align
				int32 extvariadics		! (x) for an extproc; 0 or start param of variadics
				int32 nret				! (x) setretmult: no. of return values

			end
		end
		int128 value128
	end
	u32 seqno
	u32 spare
end

global int optimflag

global [rtsnames.len]psymbol rtsproctable		!nil, or an rts proc has been defined in pcl code

global const maxpdlllib=50
global int npdllnametable
global [maxpdlllib]ichar pdllnametable

global psymbol pallsymbols				!linked list of all symbols
global byte pstready=1					!1 addsym enabled (disabled during hst init)

!PROC $INIT=
!CPL "PCL DECLS", PSTREC.BYTES
!END
=== pc_disasm.m 0 0 18/44 ===
!const showmregs=1
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
!0F has been decoded
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

	when 0xA2 then
		genstr("cpuid")

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
	genstr(strint(a))
end

proc genhex(int64 a)=
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
	if opsize=1 and not rex and reg>=5 and reg<=8 then
		reg+:=12				!5..8 => 17..20
	fi
end

proc getsilx(int &reg)=
!as getsil but used for basereg, which must have addrmode=amreg
	if addrmode=amreg and opsize=1 and rex=0 and reg>=5 and reg<=8 then
		reg+:=12				!5..8 => 17..20
	fi
end
=== pc_genmcl.m 0 0 19/44 ===
!const fshowpcl=1
const fshowpcl=0
!const fshowopndstack=1
const fshowopndstack=0

!const fshowbothmcl=1
const fshowbothmcl=0

ref mclrec procdefmcl	!points to first mcl instr for proc


pcl currpcl
[0..klast]ref proc(pcl) px_handlertable

[0..5]byte scondcodes=(eq_cond, ne_cond, lt_cond, le_cond, ge_cond, gt_cond)
[0..5]byte ucondcodes=(eq_cond, ne_cond, ltu_cond, leu_cond, geu_cond, gtu_cond)

[]int multregs=(r0,r1,r2,r10,r11,r12)
[]int multxregs=(r0,r1,r2,r3,r4,r5)

global proc genmcl(int optim)=

	if mcldone then return fi

	inithandlers()
	optimflag:=optim

	mclinit()

!	for i to nlibfiles when libfiles[i]^<>'$' do
!		pcl_gen((libtypes[i]='D'|kimportdll|kimportlib), pcl_genname(libfiles[i]))
!	od

	currpcl:=pcstart
	mlabelno:=plabelno
	mseqno:=0
	passno:=1

	repeat
		convertpcl(currpcl)
		++currpcl
	until currpcl.opcode=kendprogram

	genabsneg()
	genstringtable()
	genrealtable()
	genrtsproctable()

	genfunctiontable()
	genmc(m_nop)
	genmc(m_nop)

	mcldone:=1
end

proc convertpcl(pcl p)=
	[1256]char str
	ichar ss
	int m

	if fshowpcl  then
		case p.opcode
		when klabel, kcomment, klocal, kprocdef, kprocentry, kthreadedproc,
			kretproc, kendproc, kparam, kextproc, kextparam, kendextproc then
		else
				strcpy(&.str,"                       ")
				strcat(&.str,pclnames[p.opcode])
				mgencomment(&.str)
		esac
	fi

!CPL PCLNAMES[P.OPCODE]

	mseqno:=p.seqno
	px_handlertable[p.opcode]^(p)

	if fshowopndstack then
		case p.opcode
		when klabel, kcomment, klocal, kprocdef, kprocentry, kthreadedproc,
			kretproc, kendproc, kparam, kextproc, kextparam, kendextproc then
		else
			showopndstack()
		esac
	fi
end

proc inithandlers=
	static byte initdone=0
	ichar name
	int n

	if initdone then return fi

	n:=$get_nprocs()

	for i to n do
		name:=$get_procname(i)
		if eqbytes(name,"px_",3) then
			for k in pclnames.bounds do
				if eqstring(pclnames[k]+1,name+3) then
					px_handlertable[k]:=$get_procaddr(i)
					exit
				fi
			else
				gerrorc("Invalid handler name:",name)
			od
		fi
	od

	for i in px_handlertable.bounds do
		if not px_handlertable[i] then
			px_handlertable[i]:=cast(&unimpl)
		fi
	od

	initdone:=1
end

proc unimpl(pcl p)=
!doesn't need a handler, but used as default handler for all opcodes
!for which its pc-handler doesn't exist
	[300]char str

	print @str,"Unimplemented Opcode:",pclnames[p.opcode]
	mgencomment(str)
!	gerrorc("Unimplemented Opcode:",pclnames[p.opcode])
	println "Unimplemented Opcode:",pclnames[p.opcode]
end

global proc gerrorc(ichar mess, param=nil)=
	print "MCL Gen error:",mess
	if param then
		print ":",param
	fi

!	println " on line:",lxlineno
	stop 1
end

proc px_zero(pcl p)=
	unimpl(p)
end

proc px_nop(pcl p)=
	unimpl(p)
end

proc px_stop(pcl p)=
	psymbol d

	loadparam(1,r10)

	d:=pcl_makesymbol("exit")
	d.isimported:=1
	genmc(m_call, mgenmemaddr(d))

	delopnd()
end

proc px_comment(pcl p)=
	mgencomment(p.svalue)
end

proc px_importdll(pcl p)=
!	unimpl(p)
end

proc px_importlib(pcl p)=
!	unimpl(p)
end

proc px_istatic(pcl p)=
	setsegment('I',p.align)
	genmc(m_labelname,mgenmemaddr(p.def))
end

proc px_zstatic(pcl p)=
	psymbol d

	d:=p.def
	setsegment('Z',p.align)
	genmc(m_labelname,mgenmemaddr(d))

	genmc(m_resb,mgenint(p.size))
end

proc px_equiv(pcl p)=
	unimpl(p)
end

proc px_procdef(pcl p)=
	ichar name

	procdefpcl:=currpcl
	procdef:=currpcl.def

	name:=procdef.name
!
!CPL "--------PROCDEF",NAME

	setsegment('C',16)
	if passno=1 then
		mgencomment("DUMMY")
		procdefmcl:=mccodex
	fi

	genmc(m_procstart,mgenmemaddr(procdef))
	genmc(m_labelname,mgenmemaddr(procdef))

	nlocals:=nparams:=0
end

proc px_threadedproc(pcl p)=
	px_procdef(p)
end

proc px_procentry(pcl p)=
	int np, regoffset, offset, dreg, xreg, nregparams, nspill,hasequiv
	mcloperand ax
	psymbol d

!CPL "PROCENTRY",PROCDEF.NAME,=INF_LEAFPROC

	framebytes:=0
	frameoffset:=0
	paramoffset:=0
	needstackframe:=0
	ndsaveregs:=nxsaveregs:=0			!not of if b=non-vol regs to be spilled
	ndsavepush:=0
	nregparams:=nspill:=0
	needshadow48:=0			!duplicate caller's shadow space
	needshadow32:=0			!local shadow space
	hasequiv:=0

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF1",=NDSAVEPUSH,=NDSAVEREGS,=INF_HIGHREG,=INF_ASSEM FI
	if inf_assem then
!CPL "SKIP",INF_ASSEM
 skip fi

	if passno=2 and inf_leafproc then	!no point in pass1 as no info avialable
		dreg:=r10			!next available dreg
		xreg:=r0			!next available xreg

		for i to nparams do
			if i>4 then exit fi
			d:=paramdefs[i]
			case pcat[d.pcldef.mode]
			when d64cat then
				if not d.addrof and not d.noreg and d.nrefs then
					d.reg:=dreg
					isregvar[dreg]:=1
					if dreg=r10 then inf_r10used:=1 fi
					if dreg=r11 then inf_r11used:=1 fi
					if dreg=r13 then inf_r13used:=1 fi
					++nregparams
				fi
			when x64cat then
				if not d.addrof and d.nrefs then
					d.reg:=xreg
					isxregvar[dreg]:=1
					++nregparams
				fi
			esac
			++dreg
			++xreg
		od
	fi
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF2",=NDSAVEPUSH,=NDSAVEREGS,=INF_HIGHREG FI

	if passno=2 then		!no point in pass1 as no info avialable
		dreg:=r9			!next available dreg
		xreg:=r15			!next available xreg
		for i to nlocals do
			d:=localdefs[i]
			case pcat[d.pcldef.mode]
			when d64cat then
				if not d.addrof and not d.noreg and d.nrefs then
					if dreg<=inf_highreg or dreg<r3 then next fi
					dsaveregs[++ndsaveregs]:=dreg
					d.reg:=dreg
					isregvar[dreg]:=1
					--dreg
				fi
			when x64cat then
				if not d.addrof and d.nrefs and not d.noreg then
!				if not d.addrof and d.nrefs then
					if xreg<=inf_highxreg or xreg<r6 then next fi
					xsaveregs[++nxsaveregs]:=xreg
					d.reg:=xreg
					isxregvar[dreg]:=1
					--xreg
				fi
			esac
		od

!see if any params not regvars can use spare nonvol regs
		if not inf_leafproc then
			for i to nparams do
				if i>4 then exit fi
				d:=paramdefs[i]
				case pcat[d.pcldef.mode]
				when d64cat then
					if not d.addrof and d.nrefs and not d.noreg then
						if dreg<=inf_highreg or dreg<r3 then next fi
						dsaveregs[++ndsaveregs]:=dreg
						d.reg:=dreg
						isregvar[dreg]:=1
						--dreg
						++nregparams
					fi
				when x64cat then
					if not d.addrof and d.nrefs and not d.noreg then
						if xreg<=inf_highxreg or xreg<r6 then next fi
						xsaveregs[++nxsaveregs]:=xreg
						d.reg:=xreg
						isxregvar[dreg]:=1
						--xreg
						++nregparams
					fi
				esac
			od
		fi

	fi
skip::
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF3",=NDSAVEPUSH,=NDSAVEREGS FI

	for i to nparams do
		d:=paramdefs[i]

		if not d.reg then			!not a regvar
			if i>1 and pcat[d.pcldef.mode]=widecat and paramdefs[i-1]=d then
			else
				d.offset:=paramoffset+16
				genmc(m_define, mgenname(getfullname(d)), mgenint(d.offset))
			fi

		elsif pcat[d.pcldef.mode]=d64cat then
			genmc(m_definereg, mgenname(getfullname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getfullname(d)), mgenxreg(d.reg))
		fi
		paramoffset+:=8
	od

	for i:=r3 to inf_highreg do		!add any non-vol regs
		dsaveregs[++ndsaveregs]:=i
	od

	for i:=r6 to inf_highxreg do		!add any non-vol xregs
		xsaveregs[++nxsaveregs]:=i
	od

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF4",=NDSAVEPUSH,=NDSAVEREGS FI

!Decided class of proc entry/exit code:
!(1) Full (always a stack frame, nonvols not pushed)
!(2) Pushed nonvols, uses stack frame
!(3) Pushed nonvols, no stack frame

	if nparams>4 then
		needstackframe:=1
		nspill:=4-nregparams
	else
		ndsavepush:=ndsaveregs
		ndsaveregs:=0
		nspill:=nparams-nregparams
		if nspill then needstackframe:=1 fi
!		if nspill and nthen needstackframe:=1 fi
!		if ndsavepush then
!			if nspill then
!				needstackframe:=1
!				needshadow48:=1
!			fi
!		fi
	fi
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF5",=NDSAVEPUSH,=NDSAVEREGS FI

	for i to nlocals do
		d:=localdefs[i]
		if d.atvar then
			hasequiv:=1
        elsif not d.reg then
			frameoffset-:=roundsizetg(d.pcldef.size)
			d.offset:=frameoffset
			genmc(m_define, mgenname(getfullname(d)), mgenint(d.offset))
		elsif pcat[d.pcldef.mode]=d64cat then
			genmc(m_definereg, mgenname(getfullname(d)), mgenreg(d.reg))
		else
			genmc(m_definereg, mgenname(getfullname(d)), mgenxreg(d.reg))
		fi
	od

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF6",=NDSAVEPUSH,=NDSAVEREGS FI
	if hasequiv then
		for i to nlocals do
			d:=localdefs[i]

			if d.atvar then
MERROR("D.ATVAR NOT READY")
!				d.offset:=d.equivvar.def.offset
!				if d.reg then merror("@ on reg var") fi
!				genmc(m_define, mgenname(getfullname(d)),
!					 mgenname(getfullname(d.equivvar.def)))
			fi
		od
	fi

	frameoffset-:=ndsaveregs*8			!non-vol reg spill area
	dsaveoffset:=frameoffset
	frameoffset-:=nxsaveregs*8
	xsaveoffset:=frameoffset

	framebytes:=-frameoffset

!CPL =PROCDEF.ISTHREADED
	if (nlocals or nparams) and procdef.isthreaded then
		merror("params/locals in threaded?")
	fi
	if framebytes then needstackframe:=1 fi	!may already be set

	while framebytes iand 15 do ++framebytes od	!multiple of 16

	if needstackframe and ndsavepush.odd then framebytes+:=8 fi	!alignment

	if needstackframe and not inf_leafproc then
		framebytes +:= 32
	fi

	if needstackframe and ndsavepush then needshadow48:=1 fi

!start to generate code
	if not needstackframe and not inf_leafproc and not procdef.isthreaded then
		needshadow32:=(ndsavepush.odd | 32 | 40)
	fi

!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN CPL "SF7",=NDSAVEPUSH,=NDSAVEREGS FI
!!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN
!!CPL "............FOUND PROC"
!mgeninfos("***NAME***",PROCDEF.NAME)
!mgeninfo("NPARAMS",nparams)
!mgeninfo("NDSAVE",ndsaveregs)
!mgeninfo("NXSAVE",nxsaveregs)
!mgeninfo("NDPUSH",ndsavepush)
!mgeninfo("NSPILL",nspill)
!mgeninfo("NREGPARAMS",nregparams)
!mgeninfo("SHADOW48",needshadow48)
!mgeninfo("FRAMEBYTES",framebytes)
!mgeninfo("SHADOW32",needshadow32)
!mgeninfo("NEEDFRAME",needstackframe)
!mgeninfo("LEAFPROC",inf_leafproc)
!mgeninfo("ISTHREADED",procdef.isthreaded)
!mgeninfo("ASSEM USED",inf_assem)
!fi

	for i to ndsavepush do
		genmc(m_push, mgenreg(dsaveregs[i]))
	od

	if needshadow48 then			!create new shadow space to spill params
!MGENCOMMENT("NEED SHADOW SPACE")
		pushstack(48)
	fi

	if needstackframe then
		genmc(m_push, dframeopnd)
		genmc(m_mov, dframeopnd, dstackopnd)
		if framebytes then
			pushstack(framebytes)
		fi
	elsif needshadow32 then
!MGENCOMMENT("PUSH SHADOW32")
		pushstack(needshadow32)
	fi

!SAVE D3..D9
	offset:=dsaveoffset
	for i to ndsaveregs do
		genmc(m_mov, mgenindex(areg:rframe, size:8, offset:offset),
			mgenreg(dsaveregs[i]))
		offset+:=8
	od

	offset:=xsaveoffset
	for i to nxsaveregs do
		genmc(m_movq, mgenindex(areg:rframe, size:8, offset:offset),
			mgenxreg(xsaveregs[i]))
		offset+:=8
	od

	offset:=16
	regoffset:=0
	for i to nparams do
		if regoffset>3 then exit fi
		d:=paramdefs[i]
		IF NOT D.REG THEN
			ax:=mgenindex(areg:rframe, size:8, offset:offset)
!			ax:=mgenindex(areg:rframe, size:d.size, offset:offset)
			case pcat[d.pcldef.mode]
			when x64cat then
				genmc(m_movq, ax, mgenxreg(r0+regoffset))
			when x32cat then
				genmc(m_movd, changeopndsize(ax,4), mgenxreg(r0+regoffset))
!			when widecat then
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
!				offset+:=8
!				++regoffset
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
			else
				genmc(m_mov, ax, mgenreg(regoffset+r10))
			esac
		elsif d.reg then			!may use non-vol regs
			case pcat[d.pcldef.mode]
			when x64cat then
				if d.reg<>r0+regoffset then
					genmc(m_movq, mgenxreg(d.reg), mgenxreg(r0+regoffset))
				fi
!			when x32cat then
!				genmc(m_movd, ax, mgenxreg(r0+regoffset))
!			when widecat then
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
!				offset+:=8
!				++regoffset
!				genmc(m_mov, ax, mgenreg(regoffset+r10))
			when d64cat then
				if d.reg<>r10+regoffset then
					genmc(m_mov, mgenreg(d.reg), mgenreg(regoffset+r10))
				fi
			esac

		fi
		offset+:=8
		++regoffset
	od
end

proc px_endproc(pcl p)=
	genmc(m_procend)

!CPL "PROCEND",PROCDEF.NAME,=PASSNO
	if passno=1 then
		resetopnds1()

!CPL "AFTER PASS1",=INF_ASSEM,=OPTIMFLAG
		if inf_assem then
			inf_assem:=0
			resetopnds2()

		elsif optimflag and not inf_assem then
			passno:=2
!CPL "HERE"
			currpcl:=procdefpcl-1

			if not fshowbothmcl then
				mccodex:=procdefmcl
				mccodex.nextmcl:=nil
			fi

		fi
	else
		peephole(procdefmcl)
		resetopnds2()
		passno:=1
	fi
end

proc px_endprogram(pcl p)=
	unimpl(p)
end

proc px_extproc(pcl p)=
!	unimpl(p)
end

proc px_extparam(pcl p)=
!	unimpl(p)
end

proc px_extvariadics(pcl p)=
!	unimpl(p)
end

proc px_endextproc(pcl p)=
!	unimpl(p)
end

proc px_extvar(pcl p)=
!	unimpl(p)
end

proc px_local(pcl p)=
	if nlocals>=maxlocals then merror("Too many locals") fi
	++nlocals
	case pcat[p.mode]
	when d64cat then
		++inf_proclocals
	when x64cat then
		++inf_procxlocals
	esac
	localdefs[nlocals]:=p.def
end

proc px_param(pcl p)=
	if nparams>=maxparams then merror("Too many params") fi
	++nparams
	paramdefs[nparams]:=p.def
	if pcat[p.mode]=widecat then
		++nparams
		paramdefs[nparams]:=p.def
	fi
end

proc px_label(pcl p)=
	genmc(m_label, mgenlabel(p.labelno))
end

proc px_labelname(pcl p)=
	unimpl(p)
end

proc px_push(pcl p)=
	case p.opndtype
	when mem_opnd then
		addmem(p)
	when memaddr_opnd then
		addmemaddr(p.def)
	when int_opnd then
		addint(p.value)
	when real_opnd then
		addreal(p.xvalue)
	when real32_opnd then
		addreal32(p.xvalue32)
	when string_opnd then
		addstring(p.svalue)
	when int128_opnd then
		addint128(cast(&p.value128))
	else
		merror("push",opndnames[p.opndtype])
	esac
end

proc px_pop(pcl p)=
	mcloperand ax,bx

	case p.opndtype
	when mem_opnd then
		case pcat[p.mode]
		when d64cat then
!load any defered memtypes
			for i to noperands do
				case pclstack[i].loc
				when stack_loc then		!should not be any more
					exit
				when mem_loc then
					if pclstack[i].def=p.def then
						genopnd_ld(i)
					fi
				esac
			od

			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa))
		when x64cat then
			genmc(m_movq, mgenmem(p.def), genopnd_ld(xa))
		when x32cat then
			genmc(m_movd, mgenmem(p.def), genopnd_ld(xa))
		when widecat then
			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa))
			if p.opcode<>kstore then delopnd() fi
			genmc(m_mov, mgenmemhigh(p.def), genopnd_ld(xa))
			if p.opcode<>kstore then delopnd() fi
			return
		when shortcat then
			genmc(m_mov, mgenmem(p.def), genopnd_ld(xa,p.size))

		when blockcat then
			bx:=genopnd_ind()
			addmemaddr(p.def)
			ax:=genopnd_ind()

			copyblock(ax,bx,p.size)

			delopnd()
		else
!			merrort("POPMEM",p.mode)
			CPL "POPMEM",p.mode
		esac
	else
		merroropnd("POP",p.opndtype)
	esac

	if p.opcode<>kstore then
		delopnd()
	fi
end

proc px_store(pcl p)=
	px_pop(p)
end

proc px_pushnc(pcl p)=
	unimpl(p)
end

proc px_opnd(pcl p)=
	unimpl(p)
end

proc px_type(pcl p)=
	unimpl(p)
end

proc px_pushptroff(pcl p)=
	mcloperand ax,bx,cx,fx
	int m

	m:=p.mode

	cx:=do_addrmode(p)

	if pclstack[2].loc<>reg_loc then
		pclstack[2].fmt:=reg_d64			!reg not needed to load addr, but
		pclstack[2].loc:=reg_loc			!need to prepare it for result
		pclstack[2].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=genopnd(xb)

!here, ax is a suitable dest reg (not used for float dest), cx is the access mode

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax, cx)

	when x64cat then
!need to turn ax into a float reg
		addreg_x64()
		swapopnds(1,3)
		fx:=genopnd(xc)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
!need to turn ax into a float reg
		addreg_x32()
		swapopnds(1,3)
		fx:=genopnd(xc)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when shortcat then
		cx.size:=p.size
		genmc((psigned[m]|m_movsx|m_movzx), ax, cx)

	when widecat then
		bx:=genopnd_d64()
		swapopnds(1,2)
		swapopnds(2,3)
		genmc(m_mov, bx, applyoffset(cx,8,8))
		genmc(m_mov, ax, changeopndsize(cx,8))
		delopnd()
		setwideopnd()
		return
	when blockcat then
		genmc(m_lea, ax, cx)

	else
		merrort("pushptroff",m)
	esac	

	delopnd()
end

proc px_popptroff(pcl p)=
	mcloperand ax,bx,cx,px
	int m

	m:=p.mode

	px:=do_addrmode(p)
	cx:=genopnd_ld(xc)

	case pcat[m]
	when d64cat then
		genmc(m_mov, px,cx)

	when x64cat then
		genmc(m_movq, px,cx)

	when x32cat then
		genmc(m_movd, changeopndsize(px,4),cx)

	when shortcat then
		px.size:=p.size
		genmc(m_mov, px,changeopndsize(cx,p.size))

	when widecat then
		genmc(m_mov, changeopndsize(px,8),cx)
		genmc(m_mov, applyoffset(px,8,8),genopnd_ld(xc+1))

	when blockcat then
		copyblock(px,makeopndind(cx),p.size)

	else
		merrort("popptroff ",m)
	esac	

	delopnd()
	delopnd()
	if p.opcode=kpopptroff then
		delopnd()
		if pcat[m]=widecat then
			delopnd()
		fi
	fi
end

proc px_storeptroff(pcl p)=
	px_popptroff(p)
end

proc px_pushptr(pcl p)=
	mcloperand ax,px,cx,fx,bx
	int m

	m:=p.mode
	if isregvaropnd(xa) and pcat[m]<>blockcat then
		cx:=mgenireg(pclstack[1].reg)
		ax:=makeregopnd(xa)
	elsif pclstack[xa].fmt=imm_memaddr then
		cx:=mgenmem(pclstack[1].def)
		ax:=makeregopnd(1)
	else
		ax:=genopnd_ld()
		cx:=makeopndind(ax)
	fi

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax, cx)

	when shortcat then
		genmc((psigned[m]|m_movsx|m_movzx), ax, changeopndsize(cx,p.size))

	when x64cat then
		addreg_x64()
		swapopnds(1,2)
		fx:=genopnd(xb)

		genmc(m_movq, fx, cx)
		delopnd()

	when x32cat then
		addreg_x32()
		swapopnds(1,2)
		fx:=genopnd(xb)

		genmc(m_movd, fx, changeopndsize(cx,4))
		delopnd()

	when widecat then
		bx:=genopnd_d64()
		genmc(m_mov, bx, cx)
		genmc(m_mov, ax, applyoffset(cx,8))
		setwideopnd()

	when blockcat then		!nothing further needed

	else

		MGENCOMMENT("****PUSHPTR")
!		merror("pushptr ",stdnames[m])
	esac	

end

proc px_popptr(pcl p)=
	mcloperand ax,bx,cx,px
	int m

	m:=p.mode
	bx:=genopnd_ld(xb)
	if isregvaropnd(xa) and pcat[m]<>blockcat then
		ax:=mgenireg(pclstack[1].reg)
	else
		ax:=genopnd_ind(ya)
	fi

	case pcat[m]
	when d64cat then
		genmc(m_mov, ax,bx)

	when shortcat then
		genmc(m_mov, changeopndsize(ax,psize[m]),changeopndsize(bx,psize[m]))

	when x64cat then
		genmc(m_movq, ax,bx)

	when x32cat then
		genmc(m_movd, changeopndsize(ax,4),bx)

	when widecat then
		genmc(m_mov, ax,bx)
		genmc(m_mov, applyoffset(ax,8),genopnd_ld(xb+1))

	when blockcat then
		copyblock(ax,makeopndind(bx),p.size)

	else
		merrort("popptr ",m)
	esac	

	delopnd()
	if p.opcode=kpopptr then
		delopnd()
		if pcat[m]=widecat then
			delopnd()
		fi
	fi

end

proc px_storeptr(pcl p)=
	px_popptr(p)
end

proc px_dotindex(pcl p)=
	mcloperand ax
	int i

	if pclstack[1].fmt<>imm_d64 then
		merror("dotix i not imm")
	fi

	ax:=genopnd_ld(xb)
	i:=pclstack[1].value

	if i then
		genmc(m_shr, ax, mgenint(i))
	fi
	genmc(m_andx, changeopndsize(ax,4), mgenint(1))

	delopnd()
end

proc px_popdotindex(pcl p)=
	mcloperand ax,bx,cx,rx,mx
	int i,size,cxfmt,rhs,axoffset

	if pclstack[3].fmt=imm_d64 then
		rhs:=pclstack[3].value
		cx:=nil
	else
		cx:=genopnd_ld(xc)
	fi

	if pclstack[1].fmt<>imm_d64 then
		merror("popdotix i not imm")
	fi
	i:=pclstack[1].value
	size:=p.size

	axoffset:=xb

	addreg_d64()
	rx:=genopnd()
	addreg_d64()
	mx:=genopnd()

!	if pclfmt[axindex]=imm_memaddr then
!		genmc(m_mov, mgenmem(pcldef[axindex]))
!	else
		ax:=genopnd_ind(axoffset+2,size:size)
		genmc((size=8|m_mov|m_movzx),rx,ax)
!	fi


!	genmc(m_mov,mx,mgenint(1<<i))
	genmc(m_mov,mx,mgenint(inot(1<<i)))
!	genmc(m_notx,mx)
	genmc(m_andx,rx,mx)

	if cx then
		if i then genmc(m_shl, cx, mgenint(i)) fi
		genmc(m_orx, rx, cx)
	elsif rhs<>0 then
		genmc(m_orx, rx, mgenint(1<<i))
	fi

!	if pclfmt[axindex]=imm_memaddr then
!		genmc(m_mov, mgenmem(pcldef[axindex]), rx)
!	else
		genmc(m_mov,ax,changeopndsize(rx,size))
!	fi

	delopnd()			!mx
	delopnd()			!rx
	delopnd()			!bx/index
	delopnd()			!addr
	if p.opcode=kpopdotindex then
		delopnd()		!value being stored
	fi
end

proc px_storedotindex(pcl p)=
	px_popdotindex(p)
end

proc px_dotslice(pcl p)=
	mcloperand ax,mx,mx4
	int i,j
	word mask

	if pclstack[yb].fmt<>imm_d64 or pclstack[za].fmt<>imm_d64 then
		merror("dotslice i/j not imm")
	fi

	ax:=genopnd_ld(xc)
	i:=pclstack[yb].value
	j:=pclstack[za].value

	if i then
		genmc(m_shr, ax, mgenint(i))
	fi

	mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
	if mask<=word(int32.maxvalue) then			!use immediate
		genmc(m_andx, ax, mgenint(mask))
	else
		mx:=makeregopnd(yb)
		genmc(m_mov, mx, mgenint(mask))
		genmc(m_andx, ax, mx)
	fi

	delopnd()
	delopnd()
end

proc px_popdotslice(pcl p)=
	mcloperand ax,rx,mx,mx4,dx
	int i,j,size
	word mask

	if pclstack[yb].fmt<>imm_d64 or pclstack[za].fmt<>imm_d64 then
		merror("popdotslice i/j not imm")
	fi

	dx:=genopnd_ld(wd)

	size:=p.size
	ax:=genopnd_ind(xc,size:size)

	i:=pclstack[yb].value
	j:=pclstack[za].value

	mx:=makeregopnd(yb)
	rx:=makeregopnd(za)

	loadtoreg(rx,ax,p.mode)

	mask:=inot((inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1)))<<i)

!	mask:=inot(0xFFFF'FFFF'FFFF'FFFF<<(j-i+1))
	genmc(m_mov, mx, mgenint(mask))

	if i then
		genmc(m_shl, dx, mgenint(i))
	fi

	genmc(m_andx, rx, mx)
	genmc(m_orx, rx, dx)

	storefromreg(ax,rx,size)

	delopnd()			!j
	delopnd()			!i
	delopnd()			!A
	if p.opcode=kpopdotslice then
		delopnd()		!x
	fi

end

proc px_storedotslice(pcl p)=
	px_popdotslice(p)
end

proc px_popstack(pcl p)=
	delopnd()
end

proc px_eval(pcl p)=
	if pclstack[1].wide='L' then
		genopnd_ld(ya)
		genopnd_ld(xb)
		delopnd()
		delopnd()
	else
		genopnd_ld(xa)
		delopnd()
	fi
end

proc px_callproc(pcl p)=
	int nslots, nargs:=callargs[ncalldepth]

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenmemaddr(p.def))

	to nargs do
		poparg()
	od
	popslots(nslots)
end

proc px_callprocptr(pcl p)=
	int nslots, nargs:=callargs[ncalldepth]
	nslots:=do_pushparams(p,1)

	genmc(m_call, genopnd_ld(xa))

	delopnd()			!the ptr

	to nargs do
		poparg()
	od
	popslots(nslots)
end

proc px_retproc(pcl p)=
	int offset

	offset:=dsaveoffset
	for i to ndsaveregs do
		genmc(m_mov, mgenreg(dsaveregs[i]),
			mgenindex(areg:rframe, size:8, offset:offset))
		offset+:=8
	od

	offset:=xsaveoffset
	for i to nxsaveregs do
		genmc(m_movq, mgenxreg(xsaveregs[i]),
			mgenindex(areg:rframe, size:8, offset:offset))
		offset+:=8
	od

!	if framebytes then
!		genmc(m_add, dstackopnd, mgenint(framebytes))
!	fi
!	if framebytes or parambytes then
!		genmc(m_pop, dframeopnd)
!	fi

	if needstackframe then
		if framebytes then
			genmc(m_add, dstackopnd, mgenint(framebytes))
		fi
		genmc(m_pop, dframeopnd)
	elsif needshadow32 then
		popstack(needshadow32)
	fi

	if needshadow48 then
		popstack(48)
	fi
	for i:=ndsavepush downto 1 do
		genmc(m_pop, mgenreg(dsaveregs[i]))
	od

!	if ndsavepush.odd then
!		genmc(m_pop, mgenreg(dsaveregs[1]))
!!		popstack(8)
!	fi
!
	genmc(m_ret)
end

proc px_callfn(pcl p)=
	px_callproc(p)

	dogetretvalue(p)
end

proc px_callfnptr(pcl p)=
	px_callprocptr(p)

	dogetretvalue(p)
end

proc px_retfn(pcl p)=
	px_retproc(p)
end

proc px_jump(pcl p)=
	genmc(m_jmp, mgenlabel(p.labelno))
end

proc px_jumpptr(pcl p)=
	unimpl(p)
end

proc px_jumpeq(pcl p)=
	dojumpcc(p)
end

proc px_jumpne(pcl p)=
	dojumpcc(p)
end

proc px_jumplt(pcl p)=
	dojumpcc(p)
end

proc px_jumple(pcl p)=
	dojumpcc(p)
end

proc px_jumpge(pcl p)=
	dojumpcc(p)
end

proc px_jumpgt(pcl p)=
	dojumpcc(p)
end

proc px_jumptrue(pcl p)=
	dojumptruefalse(p,nz_cond)
end

proc px_jumpfalse(pcl p)=
	dojumptruefalse(p,z_cond)
end

proc px_jumpinrange(pcl p)=
	mcloperand ax,bx,cx,lx,nolx
	int nolab

	ax:=genopnd_ld(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	lx:=mgenlabel(p.labelno)

	genmc(m_cmp, ax,bx)

	nolx:=mgenlabel(nolab:=mcreatefwdlabel())
	genmc_cond(m_jmpcc, (psigned[p.mode]|lt_cond|ltu_cond),nolx)
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, (psigned[p.mode]|le_cond|leu_cond),lx)
	mdefinefwdlabel(nolab)

	delopnd()
	delopnd()
	delopnd()
end

proc px_jumpnotinrange(pcl p)=
	mcloperand ax,bx,cx,lx

	ax:=genopnd_ld(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	lx:=mgenlabel(p.labelno)

	genmc(m_cmp, ax,bx)

	genmc_cond(m_jmpcc, (psigned[p.mode]|lt_cond|ltu_cond),lx)
	genmc(m_cmp, ax, cx)
	genmc_cond(m_jmpcc, (psigned[p.mode]|gt_cond|gtu_cond),lx)

	delopnd()
	delopnd()
	delopnd()
end

proc px_setjumpeq(pcl p)=
	genmc(m_cmp,genopnd_ld(xb),genopnd(ya))

	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
end

proc px_setjumpeqx(pcl p)=
	genmc(m_cmp,genopnd_ld(xb),genopnd(ya))

	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc px_setjumpne(pcl p)=
	genmc(m_cmp,genopnd(xb),genopnd(ya))

	genmc_cond(m_jmpcc, ne_cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc px_seteq(pcl p)=
	dosetcc(p)
end

proc px_setne(pcl p)=
	dosetcc(p)
end

proc px_setlt(pcl p)=
	dosetcc(p)
end

proc px_setle(pcl p)=
	dosetcc(p)
end

proc px_setge(pcl p)=
	dosetcc(p)
end

proc px_setgt(pcl p)=
	dosetcc(p)
end

proc px_casejumpeq(pcl p)=
	genmc(m_cmp, genopnd_ld(xb), genopnd(ya))
	genmc_cond(m_jmpcc, eq_cond, mgenlabel(p.labelno))
	delopnd()
end

proc px_selecteq(pcl p)=
	doselectcc(p)
end

proc px_selectne(pcl p)=
	doselectcc(p)
end

proc px_selectlt(pcl p)=
	doselectcc(p)
end

proc px_selectle(pcl p)=
	doselectcc(p)
end

proc px_selectge(pcl p)=
	doselectcc(p)
end

proc px_selectgt(pcl p)=
	doselectcc(p)
end

proc px_selecttrue(pcl p)=
	mcloperand ax,bx,cx,dx

	if pcat[p.mode]<>d64cat then merrort("selecttrue",p.mode) fi

	ax:=genopnd_ld(xa)
!	genmc(m_andx, ax,ax)
	genmc(m_test, ax,ax)

	noxorclear:=1
	dx:=genopnd_ld(yb)
	cx:=genopnd_ld(xc)
!	dx:=genopnd_ld(xc)
!	cx:=genopnd_ld(yb)
	noxorclear:=0
	genmc_cond(m_cmovcc, z_cond, cx,dx)

	delopnd()
	delopnd()
end

proc px_to(pcl p)=
	pcl q
	mcloperand ax

	q:=currpcl:=p+1

	ax:=mgenmem(q.def)
	genmc(m_dec, ax)
	genmc_cond(m_jmpcc, nz_cond, mgenlabel(p.labelno))
end

proc px_forup(pcl p)=
	do_for(p, m_inc, m_add, le_cond)
end

proc px_fordown(pcl p)=
	do_for(p, m_dec, m_sub, ge_cond)
end

proc px_swap(pcl p)=
	mcloperand ax,bx

	mcloperand px:=genopnd_ind(xb,p.size)
	mcloperand qx:=genopnd_ind(ya,p.size)

	ax:=mgenreg(getnextreg(),p.size)
	bx:=mgenreg(getnextreg(),p.size)

	case pcat[p.mode]
	when d64cat,shortcat then
		genmc(m_mov, ax, px)
		genmc(m_mov, bx, qx)
		genmc(m_mov, qx, ax)
		genmc(m_mov, px, bx)

	else
		merrort("swap",p.mode)
	esac

	freereg(ax.reg)
	freereg(bx.reg)

	delopnd()
	delopnd()
end

proc px_makeslice(pcl p)=
	setwideopnd()
end

proc px_switch(pcl p)=
	int minlab, maxlab, jumplab, elselab
	mcloperand ax

	minlab:=p.minlab
	maxlab:=p.maxlab
	jumplab:=p.labelno
	currpcl:=p+1
	elselab:=currpcl.labelno

	ax:=genopnd_ld(xa)
	if minlab<>0 then
		genmc(m_sub,ax,mgenint(minlab))
	fi
	genmc(m_cmp,ax,mgenint(maxlab-minlab+1))
	genmc_cond(m_jmpcc,geu_cond,mgenlabel(elselab))
	genmc(m_jmp, mgenindex(ireg:ax.reg,scale:8,labno:jumplab))

	delopnd()

	setsegment('I')
end

proc px_switchlabel(pcl p)=
	genmc(m_dq, mgenlabel(p.labelno))
end

proc px_endswitch(pcl p)=
	setsegment('C')
end

proc px_clear(pcl p)=
	mcloperand ax

	ax:=genopnd_ind()
	clearblock(ax,p.size)
	delopnd()
end

proc px_csegment(pcl p)=
	unimpl(p)
end

proc px_isegment(pcl p)=
	unimpl(p)
end

proc px_zsegment(pcl p)=
	unimpl(p)
end

proc px_rosegment(pcl p)=
	unimpl(p)
end

proc px_data(pcl p)=
	mcloperand ax
	int opc

	case p.opndtype
	when int_opnd then
		ax:=mgenint(p.value)
	when real_opnd,real32_opnd then
		ax:=mgenrealimm(p.xvalue,p.size)
	when string_opnd then
		 ax:=mgenlabel(getstringindex(p.svalue))

	when memaddr_opnd then
		ax:=mgenmemaddr(p.def)
		ax.offset:=p.extra
	else
		merror("db/dq optype? #", opndnames[p.opndtype])
	esac

	case p.size
	when 1 then opc:=m_db
	when 2 then opc:=m_dw
	when 4 then opc:=m_dd
	when 8 then opc:=m_dq
	else
		merror("DATA/not 1248")
	esac

	genmc(opc,ax)
end

proc px_db(pcl p)=
	unimpl(p)
end

proc px_dw(pcl p)=
	unimpl(p)
end

proc px_dd(pcl p)=
	unimpl(p)
end

proc px_dq(pcl p)=
	unimpl(p)
end

proc px_dstring(pcl p)=
	unimpl(p)
end

proc px_dstringz(pcl p)=
	unimpl(p)
end

proc px_reserve(pcl p)=
	unimpl(p)
end

proc px_assem(pcl p)=
!CPL "PCL/GENASM",=HOSTASMHANDLER
	if hostasmhandler then
		hostasmhandler(p.asmcode)
	else
		unimpl(p)
	fi
	if p.mode then
		dogetretvalue(p)
	fi
end

proc px_add(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if pclstack[1].fmt=imm_d64 and pclstack[1].value=1 then
			genmc(m_inc, ax)
		else
			bx:=genopnd(ya)
			genmc(m_add,ax,bx)
		fi
	when x64cat then
		dobin_float(m_addsd)
	when x32cat then
		dobin_float(m_addss)
	when widecat then
		dobin_i128(p,m_add, m_adc)
		return
	else
		merrort("add:",p.mode)
	esac
	delopnd()
end

proc px_sub(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if pclstack[1].fmt=imm_d64 and pclstack[1].value=1 then
			genmc(m_dec, ax)
		else
			bx:=genopnd(ya)
			genmc(m_sub,ax,bx)
		fi
	when x64cat then
		dobin_float(m_subsd)
	when x32cat then
		dobin_float(m_subss)
	when widecat then
		dobin_i128(p,m_sub, m_sbb)
		return
	else
		merrort("sub:",p.mode)
	esac
	delopnd()
end

proc px_mul(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		if  pclstack[1].fmt=imm_d64 then
			mulimm(ax,pclstack[1].value)
			delopnd()
			return
		fi

		bx:=genopnd(ya)
		genmc(m_imul2,ax,bx)

	when x64cat then
		dobin_float(m_mulsd)
	when x32cat then
		dobin_float(m_mulss)
	when widecat then
		do_syscall(rts_mul_i128,4,widecat)
		return
	else
		merrort("mul:",p.mode)
	esac
	delopnd()
end

proc px_div(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when x64cat then
		dobin_float(m_divsd)
	when x32cat then
		dobin_float(m_divss)
!	when widecat then
	else
		merrort("div:",p.mode)
	esac
	delopnd()
end

proc px_idiv(pcl p)=
	case p.mode
	when tpi64 then
		do_divrem(p, issigned:1, isdiv:1)
	when tpu64 then
		do_divrem(p, issigned:0, isdiv:1)
	when tpi128 then
		swapopnds(1,3)
		swapopnds(2,4)
		do_syscall(rts_div_i128,4,widecat)
	when tpu128 then
		swapopnds(1,3)
		swapopnds(2,4)
!		do_syscall(rts_div_u128,4,widecat)
		do_syscall(rts_div_i128,4,widecat)
	else
		merrort("idiv:",p.mode)
	esac
end

proc px_irem(pcl p)=
	case p.mode
	when tpi64 then
		do_divrem(p, issigned:1, isdiv:0)
	when tpu64 then
		do_divrem(p, issigned:0, isdiv:0)
	else
		merrort("irem:",p.mode)
	esac
end

proc px_iand(pcl p)=
	dobitwise(p, m_andx)
end

proc px_ior(pcl p)=
	dobitwise(p, m_orx)
end

proc px_ixor(pcl p)=
	dobitwise(p, m_xorx)
end

proc px_shl(pcl p)=
	case pcat[p.mode]
	when d64cat then
		do_shift(p,m_shl)
	else
		merrort("shl:",p.mode)
	esac
end

proc px_shr(pcl p)=
	case p.mode
	when tpi64 then
		do_shift(p,m_sar)
	when tpu64 then
		do_shift(p,m_shr)
	else
		merrort("shr:",p.mode)
	esac
end

proc px_in(pcl p)=
	unimpl(p)
end

proc px_notin(pcl p)=
	unimpl(p)
end

proc px_min(pcl p)=
	case p.mode
	when tpi64 then
		domax_int(gt_cond)
	when tpu64 then
		domax_int(gtu_cond)
	when tpr64 then
		domax_float(m_minsd)
	when tpr32 then
		domax_float(m_minss)
	else
		merrort("min:",p.mode)
	esac
end

proc px_max(pcl p)=
	case p.mode
	when tpi64 then
		domax_int(lt_cond)
	when tpu64 then
		domax_int(ltu_cond)
	when tpr64 then
		domax_float(m_maxsd)
	when tpr32 then
		domax_float(m_maxss)
	else
		merrort("max:",p.mode)
	esac
end

proc px_eq(pcl p)=
	unimpl(p)
end

proc px_ne(pcl p)=
	unimpl(p)
end

proc px_lt(pcl p)=
	unimpl(p)
end

proc px_le(pcl p)=
	unimpl(p)
end

proc px_ge(pcl p)=
	unimpl(p)
end

proc px_gt(pcl p)=
	unimpl(p)
end

proc px_same(pcl p)=
	unimpl(p)
end

proc px_andl(pcl p)=
	unimpl(p)
end

proc px_orl(pcl p)=
	unimpl(p)
end

proc px_addrefoff(pcl p)=
	mcloperand ax,cx

	cx:=do_addrmode(p)

	if pclstack[2].loc<>reg_loc then
		pclstack[2].fmt:=reg_d64			!reg not needed to load addr, but
		pclstack[2].loc:=reg_loc			!need to prepare it for result
		pclstack[2].reg:=getnextreg()		!(although wasted for floats)
	fi
	ax:=genopnd(xb)

	genmc(m_lea, ax, cx)
	delopnd()
end

proc px_subref(pcl p)=
	mcloperand ax,bx
	int n

	ax:=genopnd_ld(xb)
!	bx:=genopnd_ld(ya)
	bx:=genopnd(ya)
	genmc(m_sub,ax,bx)

	if p.scale>1 then
		n:=ispoweroftwo(p.scale)
		if n then
			genmc(m_shr, ax, mgenint(n))
		else
			MERROR("SUB/REF NOT POWER OF TWO")
		fi
	fi

	delopnd()
end

proc px_subrefoff(pcl p)=
	int scale, extra, offset
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=genopnd_ld(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(m_sub, ax, mgenint(pclstack[1].value*scale+extra))
	else
		bx:=genopnd_ld(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
MERROR("SUBREF/EXTRA")
!			genmc(m_add, ax, mgenint(extra))
		fi
	fi
	delopnd()
end

proc px_neg(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_neg,ax)

	when x64cat then
		if not labneg64 then labneg64:=mcreatefwdlabel() fi
		genmc(m_xorpd,genopnd_ld(xa),mgenlabelmem(labneg64))
	when x32cat then
		if not labneg32 then labneg32:=mcreatefwdlabel() fi
		genmc(m_xorps,genopnd_ld(xa),mgenlabelmem(labneg32))

	when widecat then
		mcloperand axlow, axhigh, bxlow, bxhigh
		axlow:=genopnd_ld()
		axhigh:=genopnd_ld(2)

		bxhigh:=genopnd_d64()
		bxlow:=genopnd_d64()

		genmc(m_xorx,bxlow,bxlow)
		genmc(m_xorx,bxhigh,bxhigh)
		genmc(m_sub,bxlow,axlow)
		genmc(m_sbb,bxhigh,axhigh)

		swapopnds(1,3)
		swapopnds(2,4)

		delopnd()
		delopnd()
	else
		merrort("neg",p.mode)
	esac
end

proc px_abs(pcl p)=
	mcloperand ax,lx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_cmp, ax, mgenint(0))

		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_neg,ax)
		genmc(m_label, lx)

	when x64cat then
		if not lababs64 then lababs64:=mcreatefwdlabel() fi
		genmc(m_andpd,genopnd_ld(xa),mgenlabelmem(lababs64))
	when x32cat then
		if not lababs32 then lababs32:=mcreatefwdlabel() fi
		genmc(m_andps,genopnd_ld(xa),mgenlabelmem(lababs32))
	else
		merrort("abs",p.mode)
	esac
end

proc px_inot(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_notx,ax)

	else
		merrort("inot",p.mode)
	esac
end

proc px_notl(pcl p)=
	mcloperand ax

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_xorx,ax,mgenint(1))

	else
		merrort("notl",p.mode)
	esac
end

proc px_istruel(pcl p)=
	mcloperand ax, bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_setcc, ne_cond, bx:=changeopndsize(ax,1))
		genmc(m_movzx, changeopndsize(ax,4),bx)

	else
		merrort("istruel",p.mode)
	esac
end

proc px_sqr(pcl p)=
	mcloperand ax

	ax:=genopnd_ld(xa)

	case pcat[p.mode]
	when d64cat then
		genmc(m_imul2,ax,ax)
!
	when x64cat then
		genmc(m_mulsd,ax,ax)
	when x32cat then
		genmc(m_mulss,ax,ax)
	else
		merrort("sqr",p.mode)
	esac
end

proc px_sqrt(pcl p)=
	mcloperand ax

	ax:=genopnd_ld(xa)

	case pcat[p.mode]
	when x64cat then
		genmc(m_sqrtsd,ax,ax)
	when x32cat then
		genmc(m_sqrtss,ax,ax)
	else
		merrort("sqrt",p.mode)
	esac
end

proc px_sin(pcl p)=
	domaths(p,"sin*")
end

proc px_cos(pcl p)=
	domaths(p,"cos*")
end

proc px_tan(pcl p)=
	domaths(p,"tan*")
end

proc px_asin(pcl p)=
	domaths(p,"asin*")
end

proc px_acos(pcl p)=
	domaths(p,"acos*")
end

proc px_atan(pcl p)=
	domaths(p,"atan*")
end

proc px_ln(pcl p)=
	domaths(p,"log*")
end

proc px_log(pcl p)=
	domaths(p,"log10*")
end

proc px_exp(pcl p)=
	domaths(p,"exp*")
end

proc px_round(pcl p)=
	domaths(p,"round*")
end

proc px_floor(pcl p)=
	domaths(p,"floor*")
end

proc px_ceil(pcl p)=
	domaths(p,"ceil*")
end

proc px_fract(pcl p)=
	unimpl(p)
end

proc px_sign(pcl p)=
	unimpl(p)
end

proc px_atan2(pcl p)=
	unimpl(p)
end

proc px_power(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then

		swapopnds(1,2)
		do_syscall(rts_power_i64,2,d64cat)

	when x64cat then
!CPL "HERE"
		swapopnds(1,2)
		domaths2(p,"pow*")
!		dobin_float(m_mulsd)
!	when x32cat then
!		dobin_float(m_mulss)
!	when widecat then
!!		do_syscall(rts_mul_i128,4,widecat)
!!		delopnd()
!!		delopnd()
		return
	else
		merrort("power:",p.mode)
	esac
!	delopnd()
end

proc px_fmod(pcl p)=
	unimpl(p)
end

proc px_incr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincr(p,m_inc, m_add)
	else
		merrort("incr", p.mode)
	esac
end

proc px_decr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincr(p,m_dec, m_sub)
	else
		merrort("decr", p.mode)
	esac
end

proc px_incrload(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincrload(p,m_inc, m_add)
	else
		merrort("incrload", p.mode)
	esac
end

proc px_decrload(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doincrload(p,m_dec, m_sub)
	else
		merrort("decrload", p.mode)
	esac
end

proc px_loadincr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doloadincr(p,m_inc, m_add)
	else
		merrort("loadincr", p.mode)
	esac
end

proc px_loaddecr(pcl p)=
	case pcat[p.mode]
	when d64cat,shortcat then
		doloadincr(p,m_dec, m_sub)
	else
		merrort("loaddecr", p.mode)
	esac
end

proc px_addto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_add)
	when x64cat then
		dobinto_float(p,m_addsd)
	when x32cat then
		dobinto_float32(p,m_addss)
!	when shortcat then

!	when widecat then
	else
		merrort("addto:",p.mode)
	esac
end

proc px_subto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_sub)
	when x64cat then
		dobinto_float(p,m_subsd)
	when x32cat then
		dobinto_float32(p,m_subss)
!	when widecat then
	else
		merrort("subto:",p.mode)
	esac
end

proc px_multo(pcl p)=
	mcloperand ax,bx,cx

	case pcat[p.mode]
	when d64cat then
		addreg_d64()
		ax:=genopnd_ind(xc)
		bx:=genopnd(yb)
		cx:=genopnd(za)

		genmc(m_mov, cx,ax)

		if  pclstack[2].fmt=imm_d64 then
			mulimm(cx, pclstack[2].value)
		else
			genmc(m_imul2, cx,bx)
		fi
		genmc(m_mov, ax,cx)

		delopnd()
		delopnd()
		delopnd()
	when x64cat then
		dobinto_float(p,m_mulsd)
	when x32cat then
		dobinto_float32(p,m_mulss)
!	when widecat then
	else
		merrort("multo:",p.mode)
	esac
end

proc px_divto(pcl p)=
	mcloperand ax,bx,cx

	case pcat[p.mode]
	when x64cat then
		dobinto_float(p,m_divsd)
	when x32cat then
		dobinto_float32(p,m_divss)
!	when widecat then
	else
		merrort("divto:",p.mode)
	esac
end

proc px_idivto(pcl p)=
	unimpl(p)
end

proc px_iremto(pcl p)=
	unimpl(p)
end

proc px_iandto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_andx)
!	when widecat then
	else
		merrort("iandto:",p.mode)
	esac
end

proc px_iorto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_orx)
!	when widecat then
	else
		merrort("iorto:",p.mode)
	esac
end

proc px_ixorto(pcl p)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat,shortcat then
		dobinto_int(p,m_xorx)
!	when widecat then
	else
		merrort("ixorto:",p.mode)
	esac
end

proc px_shlto(pcl p)=
	case p.mode
	when tpi64,tpu64 then
		do_shiftnto(p,m_shl)
	else
		merrort("shlto:",p.mode)
	esac
end

proc px_shrto(pcl p)=
	case p.mode
	when tpi64 then
		do_shiftnto(p,m_sar)
	when tpu64 then
		do_shiftnto(p,m_shr)
	else
		merrort("shrto:",p.mode)
	esac
end

proc px_minto(pcl p)=
	case p.mode
	when tpi64 then
		domaxto_int(le_cond)
	when tpu64 then
		domaxto_int(leu_cond)
	when tpr64 then
		domaxto_r64(leu_cond)
	when tpr32 then
		domaxto_r32(leu_cond)
!	when tpi128 then
	else
		merrort("minto:",p.mode)
	esac
end

proc px_maxto(pcl p)=
	case p.mode
	when tpi64 then
		domaxto_int(ge_cond)
	when tpu64 then
		domaxto_int(geu_cond)
	when tpr64 then
		domaxto_r64(geu_cond)
	when tpr32 then
		domaxto_r32(geu_cond)
!	when tpi128 then
	else
		merrort("maxto:",p.mode)
	esac
end

proc px_andlto(pcl p)=
	unimpl(p)
end

proc px_orlto(pcl p)=
	unimpl(p)
end

proc px_addrefoffto(pcl p)=
	int scale, extra,offset
!
	scale:=p.scale
	extra:=p.extra
	offset:=pclstack[1].value*scale+extra	!in case imm_d64

	mcloperand ax,bx,rx
	int reg,size

	if ismemaddr(xb) then
		ax:=mgenmem(pclstack[2].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)

		genmc(m_mov, rx, ax)

		if pclstack[1].fmt=imm_d64 then
			genmc(m_add,rx,mgenint(offset))
		else
			bx:=genopnd_ld(ya)
			mulimm(bx,scale)
			genmc(m_add,rx,bx)
		fi

		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=genopnd_ind(xb)
		if pclstack[1].fmt=imm_d64 then
			genmc(m_add,ax,mgenint(offset))
		else
			bx:=genopnd_ld(ya)
			mulimm(bx,scale)
			genmc(m_add,ax,bx)
		fi
	fi
	delopnd()
	delopnd()
end

proc px_subrefoffto(pcl p)=
	int scale, extra
	mcloperand ax,bx

	scale:=p.scale
	extra:=p.extra

	ax:=genopnd_ind(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(m_sub, ax, mgenint(pclstack[1].value*scale+extra))
	else
		bx:=genopnd_ld(xa)
		scale:=scaleindex(bx,scale)
		if scale>1 then
			mulimm(bx,scale)
		fi
		genmc(m_sub, ax, bx)
		if extra then
MERROR("SUBTOREF/EXTRA")
!			genmc(m_sub, ax, mgenint(extra))
		fi
	fi

	delopnd()
	delopnd()
end

proc px_negto(pcl p)=
	unimpl(p)
end

proc px_absto(pcl p)=
	unimpl(p)
end

proc px_inotto(pcl p)=
	unimpl(p)
end

proc px_notlto(pcl p)=
	unimpl(p)
end

proc px_istruelto(pcl p)=
	unimpl(p)
end

proc px_typepun(pcl p)=
	mcloperand ax,bx,cx

	bx:=genopnd_ld(xa)

	case pcat[p.mode]
	when d64cat then
		case pclstack[1].fmt
		when xreg_x64 then
			addreg_d64()
			ax:=genopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(1,2)
			delopnd()
		when reg_d64 then
		else
			goto error
		esac

	when x64cat then
		case pclstack[1].fmt
		when reg_d64 then
			addreg_x64()
			ax:=genopnd(xa)
            genmc(m_movq,ax,bx)
			swapopnds(1,2)
			delopnd()
		else
			goto error
		esac
	when shortcat then
		case pclstack[1].fmt
		when xreg_x32 then
			addreg_d64()
			ax:=genopnd(xa)
			cx:=changeopndsize(ax,4)
            genmc(m_movd, cx,bx)
			swapopnds(1,2)
			delopnd()

			genmc((psigned[p.mode]|m_movsx|m_movzx),ax,cx)
		when reg_d64 then

		else
			goto error
		esac

	else
error::
CPL =PSTDNAMES[P.MODE]
CPL =FMTNAMES[PCLSTACK[1].FMT]
CPL "TYPEPUN"
!MGENCOMMENT("TYPEPUN")
		MERROR("TYPEPUN")
	esac
end

proc px_softconv(pcl p)=
	unimpl(p)
end

proc px_widen(pcl p)=
	unimpl(p)
end

proc px_narrow(pcl p)=
	unimpl(p)
end

proc px_float(pcl p)=
	mcloperand ax,fx
	int lab,lab2

	ax:=genopnd_ld(xa)

	case p.oldmode
	when tpi64 then
		addreg_x64()
		fx:=genopnd(xa)
		genmc(m_cvtsi2sd, fx, ax)
		swapopnds(1,2)
	when tpu64 then
		addreg_x64()
		fx:=genopnd(xa)

		lab:=mcreatefwdlabel()
		lab2:=mcreatefwdlabel()
		genmc(m_cmp, ax, mgenint(0))
		genmc_cond(m_jmpcc, lt_cond, mgenlabel(lab))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_jmp, mgenlabel(lab2))

		mdefinefwdlabel(lab)
		if not labmask63 then
			labmask63:=++mlabelno
			laboffset64:=++mlabelno
		fi
		genmc(m_andx,ax, mgenlabelmem(labmask63))
		genmc(m_cvtsi2sd, fx, ax)
		genmc(m_addsd, fx, mgenlabelmem(laboffset64))
		mdefinefwdlabel(lab2)
		swapopnds(1,2)

	else
		merrort("float",p.mode)
	esac

	delopnd()
end

proc px_fix(pcl p)=
	mcloperand fx,ax
	int newmode:=p.mode, oldmode:=p.oldmode

	case pcat[newmode]
	when d64cat then
		fx:=genopnd_ld(xa)
		addreg_d64()
		ax:=genopnd(xa)
		genmc((oldmode=tpr64|m_cvttsd2si|m_cvttss2si), ax, fx)
		swapopnds(1,2)
		delopnd()

	else
		merrort("fix->",newmode)
	esac
end

proc px_truncate(pcl p)=
	mcloperand ax
	int mask

	case psize[p.truncmode]
	when 1 then mask:=255
	when 2 then mask:=65535
	when 4 then mask:=0xFFFF'FFFF
	esac

	ax:=genopnd_ld(xa)
	genmc(m_andx, ax, mgenint(mask))

	genmc((psigned[p.truncmode]|m_movsx|m_movzx), ax, changeopndsize(ax,psize[p.truncmode]))
end

proc px_fwiden(pcl p)=
	mcloperand fx
	fx:=genopnd_ld()
	genmc(m_cvtss2sd, fx,fx)
	pclstack[1].fmt:=xreg_x64
end

proc px_fnarrow(pcl p)=
	mcloperand ax:=genopnd_ld(xa)
	genmc(m_cvtsd2ss, ax,ax)
	pclstack[1].fmt:=xreg_x32
end

proc px_softtruncw(pcl p)=
	mcloperand ax,bx,bx2,lx

	ax:=genopnd_ld()
	swapopnds(1,2)
	delopnd()
	unsetwideopnd()
end

proc px_widenw(pcl p)=
	mcloperand ax,bx,bx2,lx

	ax:=genopnd_ld()
	bx:=genopnd_d64()
	bx2:=changeopndsize(bx,4)

	swapopnds(1,2)

	if psigned[p.mode] then
		genmc(m_xorx,bx2,bx2)
		genmc(m_cmp,ax,mgenint(0))
		genmc_cond(m_jmpcc, ge_cond, lx:=mgenlabel(++mlabelno))
		genmc(m_notx, bx)
		genmc(m_label,lx)
	else
		genmc(m_xorx,bx2,bx2)
	fi

	setwideopnd()
end

proc px_len(pcl p)=
	delopnd()
	unsetwideopnd()
end

proc px_lwb(pcl p)=
	unimpl(p)
end

proc px_upb(pcl p)=
	unimpl(p)
end

proc px_bounds(pcl p)=
	unimpl(p)
end

proc px_lenstr(pcl p)=
	unimpl(p)
end

proc px_bitwidth(pcl p)=
	unimpl(p)
end

proc px_bytesize(pcl p)=
	unimpl(p)
end

proc px_bytes(pcl p)=
	unimpl(p)
end

proc px_minvalue(pcl p)=
	unimpl(p)
end

proc px_maxvalue(pcl p)=
	unimpl(p)
end

proc px_typestr(pcl p)=
	unimpl(p)
end

proc px_error(pcl p)=
	unimpl(p)
end

proc px_arraytoslice(pcl p)=
	unimpl(p)
end

proc px_ichartoslice(pcl p)=
	unimpl(p)
end

proc px_softtruncshort(pcl p)=
	unimpl(p)
end

proc px_charaxtoichar(pcl p)=
	unimpl(p)
end

proc px_sliceptr(pcl p)=
	swapopnds(1,2)
	delopnd()
	unsetwideopnd()
end

proc px_startmult(pcl p)=
	pushallopnds()
end

proc px_resetmult(pcl p)=
	if pclstack[1].float then
		MERROR("RESETMULT/XREG")
	else
		movetoreg(r0)
	fi

	if p.opcode=kresetmult then
		delopnd()
	fi
end

proc px_endmult(pcl p)=
	px_resetmult(p)
end

proc px_setret(pcl p)=
	do_setret(r0,r0)

	regset[r0]:=0
	xregset[r0]:=0
end

proc px_setretmult(pcl p)=
	int k,wide

	k:=0

	for i:=1 to p.nret do
		++k
		wide:=pclstack[1].wide
		do_setret(multregs[k],multxregs[k])
!		if wide then
!			++k
!			do_setret(multregs[k],multxregs[k])
!		fi
	od

	for i:=1 to k do
		regset[multregs[i]]:=xregset[multxregs[i]]:=0
	od
end

proc px_setargs(pcl p)=
	int nslots,shadow,align,nargs,opcode,nvars

	if p then
		nargs:=p.nargs
		nvars:=p.nvariadics
		opcode:=p.opcode
	else
		nargs:=sa_nargs		!set via global
		nvars:=0
		opcode:=0
	fi

	nslots:=0			!total slots to be recovered after a call
	shadow:=0			!whether 4-slot shadow space to be created
	align:=0			!whether stack alignment fix needed

	case opcode
	when ksetargs then
		saveallopnds()		!get latest mstackdepth
	else
		saveallopnds(nargs+1)
	esac

	if nargs<=4 then					!no pushed args needed
		if mstackdepth=0 then
		else
			shadow:=1
			align:=mstackdepth.odd
			nslots:=4
		fi
	else								!some pushed params
		shadow:=1
		nslots:=nargs
		align:=(mstackdepth+nslots).odd
	fi

	nslots+:=align
	if align then
		if opcode=ksetargs then		!normal
			pushslots(1)
			align:=0
		fi								!else leave to be stored in callalign
	fi

	if ncalldepth>=maxcalldepth then
		merror("Too many nested calls")
	fi
	++ncalldepth
	IF NCALLDEPTH<1 THEN
		ABORTPROGRAM("CALLDEPTH?")
	FI

	callargs[ncalldepth]:=nargs
	callnvars[ncalldepth]:=nvars
	callslots[ncalldepth]:=nslots
	callshadow[ncalldepth]:=shadow
	callalign[ncalldepth]:=align
end

proc px_duplstack(pcl p)=
	mcloperand ax

	if pcat[p.mode]>=widecat then
		merrort("duplstack",p.mode)
	fi

	duploperand()
end

proc px_swapstack(pcl p)=
	swapopnds(1,p.x+1)		!the x-attr is 0-based; need 1-based
end

proc px_getnprocs(pcl p)=
	dosetfntable()
	addlabel(lab_funcnprocs)
end

proc px_getprocname(pcl p)=
	mcloperand ax

	dosetfntable()
	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcnametable))
	else
		merrort("gpn",p.mode)
	esac
end

proc px_getprocaddr(pcl p)=
	mcloperand ax

	dosetfntable()
	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_mov, ax, mgenindex(ireg:ax.reg,scale:8,offset:-8,labno:lab_funcaddrtable))
	else
		merrort("gpa",p.mode)
	esac
end

proc px_last(pcl p)=
	unimpl(p)
end

proc dobin_float(int opc)=
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	genmc(opc,ax,bx)
end

function do_pushparams(pcl p, int isptr)int=
!isptr=1 when top pcl operand is the function pointer for indirect calls

	int pushedslots, nparams, nvars, nargs

	if p then
		nargs:=callargs[ncalldepth]
		nvars:=callnvars[ncalldepth]
	else
		nargs:=sa_nargs; isptr:=0
		nvars:=0
	fi

!CPL "DOPUSHPARAMS", =NCALLDEPTH, =NARGS, =CALLSLOTS[NCALLDEPTH],=CALLARGS[NCALLDEPTH]


	if nargs>inf_maxargs and nargs<=4 then inf_maxargs:=nargs fi
	nparams:=nargs

	if nparams>4 then
		pushallopnds(isptr+4+1)
	fi

!low params are 'pushed' after high params
!this allows r13 to be used as a scratch register

	do_pushlowparams(nparams,nvars,isptr)

	if callshadow[ncalldepth] then
		pushslots(callalign[ncalldepth]+4)
	fi

	pushedslots:=callslots[ncalldepth]
	--ncalldepth
	return pushedslots
end

proc do_pushlowparams(int nparams, nvariadics=0, isptr=0)=
!nparams=0 to 4 (if more than 4 in total, then nparams must be 4 here)
!load params to D10-13/X0-3
!does not do anything with the stack at all
! Params are categorised as follows:
! Variadic:
!   float:  load to both D and X registers
!   other:  load to D register only
! Normal/non-variadic:
!   float:  load to X register
!   other:  load to D register

	int ireg, xreg, j

	if nparams=0 then return fi
	nparams min:=4

	for i to nparams do
		j:=i+isptr
		ireg:=r10+i-1
		xreg:=xr0+i-1

		if pclstack[j].float then
			unless nvariadics and i>=nvariadics then ireg:=0 end
		else
			xreg:=0
		fi

		if ireg then loadparam(j,ireg) fi
		if xreg then loadxparam(j,xreg) fi
	od
end

proc do_for(pcl p, int incop, addop, cond)=
	pcl q,r
	mcloperand ax,bx,cx,dx,mx
	int reg

	q:=p+1
	r:=currpcl:=q+1

	mx:=mgenmem(q.def)

	if q.def.reg then
		if p.stepx=1 then
			genmc(incop, mx)
		else
			genmc(addop, mx, mgenint(p.stepx))
		fi
		ax:=mx
	else
		ax:=mgenreg(getnextreg())
		genmc(m_mov, ax,mx)
		if p.stepx=1 then
			genmc(incop, ax)
		else
			genmc(addop, ax, mgenint(p.stepx))
		fi
		genmc(m_mov, mx, ax)
	fi

	if r.opndtype=int_opnd then
		bx:=mgenint(r.value)
	else
		bx:=mgenmem(r.def)
	fi

	genmc(m_cmp, ax, bx)
	freereg(ax.reg)
!

	genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
end

proc dojumpcc(pcl p)=
	int m:=p.mode, cond
	mcloperand ax,bx

	if p.mode in [tpi128,tpu128] then
		dojumpcc_i128(p)
		return
	fi

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	case pcat[m]
	when d64cat then
		if psigned[m] then
			cond:=scondcodes[p.opcode-kjumpeq]
		else
			cond:=ucondcodes[p.opcode-kjumpeq]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[p.opcode-kjumpeq]
		genmc(m_comiss,ax,bx)

	when x64cat then
		cond:=ucondcodes[p.opcode-kjumpeq]
		genmc(m_comisd,ax,bx)
	else
		merrort("jumpcc:",p.mode)
	esac

	genmc_cond(m_jmpcc,cond, mgenlabel(p.labelno))
	delopnd()
	delopnd()
end

proc dosetcc(pcl p)=
	int m:=p.mode, cond
	mcloperand ax,bx

!	if p.mode in [tpi128,tpu128] then
!		dojumpcc_i128(p)
!		return
!	fi

	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)

	case pcat[m]
	when d64cat then
		if psigned[m] then
			cond:=scondcodes[p.opcode-kseteq]
		else
			cond:=ucondcodes[p.opcode-kseteq]
		fi
		genmc(m_cmp,ax,bx)

	when x32cat then
		cond:=ucondcodes[p.opcode-kseteq]
		genmc(m_comiss,ax,bx)

	when x64cat then
		cond:=ucondcodes[p.opcode-kseteq]
		genmc(m_comisd,ax,bx)
	else
		merrort("setcc:",p.mode)
	esac

	genmc_cond(m_setcc,cond, bx:=changeopndsize(ax,1))
	genmc(m_movzx, changeopndsize(ax,4), bx)
	delopnd()
end

proc do_setretfloat(int destreg)=
	int currreg
	mcloperand ax,rx

	rx:=mgenxreg(destreg)

	ax:=genopnd_ld(1)
	currreg:=ax.reg

	case pclstack[1].loc
	when xreg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				merror("setretfloat/dest in use")
			else
				genmc(m_movq, rx, ax)
				xregset[destreg]:=1
			fi
		fi
	else
		merror("setretf?")
	esac
	delopnd()		!assume next is a jump to return point

end

proc do_setret(int destreg,destxreg)=
!make sure top-of-stack is in nth register for multi-value return
!for normal returns, n will be 1
!nth value must be in d0/d1/d2, or x0/x1/x2
!Value might not be on the stack
!prior registers not available. Current value will not be in previous
!regs as they will have been moved out

	int currreg
	mcloperand ax,rx

	if pclstack[1].float then
		do_setretfloat(destxreg)
		return
	fi

	rx:=mgenreg(destreg)

	ax:=genopnd_ld(1)
	currreg:=ax.reg

	case pclstack[1].loc
	when reg_loc then
		if currreg<>destreg then

			if regset[destreg] then
				swapopndregs(destreg)
				genmc(m_xchg, rx, ax)
			else
				genmc(m_mov, rx, ax)
			fi
		fi
	else
CPL =LOCNAMES[PCLSTACK[1].LOC]
!CPL =PROCDEF.NAME
		merror("setret?")
	esac
	delopnd()						!assume next is a jump to return point
	regset[destreg]:=1
	mccodex.regend[destreg]:=0			!d0 will not be freed
end

proc dogetretvalue(pcl p)=
	int reg,xreg,i,n
	[10]int cats

	if (p+1).opcode=ktype then
		n:=0
		while (++p).opcode=ktype do
			cats[++n]:=pcat[p.mode]
		od
		currpcl:=p-1

		for i:=n downto 1 do 
			case cats[i]
			when shortcat, widecat then
				merror("Short/wide mulret type")
			esac

			dogetretvalue_n(multregs[i],multxregs[i], cats[i])
		od

	else
		dogetretvalue_n(r0,r0,pcat[p.mode])
		if pcat[p.mode]=shortcat then
			genmc((psigned[p.mode]|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.size))
		fi
	fi
end

proc dogetretvalue_n(int reg,xreg,cat)=

	case cat
	when d64cat,shortcat then
		addreg0(reg)
	when x64cat then
		addxreg0(xreg,xreg_x64)
	when x32cat then
		addxreg0(xreg,xreg_x32)
	when widecat then
		addwidereg0(reg)
!	when shortcat then
!		genmc((ttisint[p.mode]|m_movsx|m_movzx), mgenreg(r0),mgenreg(r0,p.size))
	else
CPL PSTDNAMES[CAT]
		merror("getretval/n?")
	esac
end

proc do_shift(pcl p, int opc)=
	mcloperand ax
	ax:=genopnd_ld(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(opc, ax, mgenint(pclstack[1].value))
	else
		if inf_r10used then merror("shift:cl in use") fi
		loadparam(reg:r10)
		genmc(opc,ax, mgenreg(r10,1))
	fi
	delopnd()
end

proc mulimm(mcloperand ax, int n)=
!multiply operand in ax (a simple reg) by constant n
!will try efficient method if possible, otherwise use normal multiply 
	int shifts,m

	case n
	when 0 then
		genmc(m_xorx, ax,ax)
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

proc dojumptruefalse(pcl p, int cond)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xa)
		genmc(m_test, ax,ax)
		genmc_cond(m_jmpcc, cond, mgenlabel(p.labelno))
	else
		merrort("jumptrue/false",p.mode)
	esac
	delopnd()
end

proc dobitwise(pcl p, int opc)=
	mcloperand ax,bx

	case pcat[p.mode]
	when d64cat then
		ax:=genopnd_ld(xb)
		bx:=genopnd(ya)
		genmc(opc,ax,bx)
	when widecat then
		dobin_i128(p,opc,opc)
		return
	else
		merrort("bitwise:",p.mode)
	esac
	delopnd()
end

function do_addrmode(pcl p)mcloperand px =
!Top two stack elements are an array (xb) and index (ya)
!Return a operand which provdes the address mode to access the element,
!for either reading or writing
!The address mode will use 0, 1 or 2 registers. The registers may be 1 or 2
!associated with the pcl operands, or may be regvars.
!If for reading, caller will need to make their own arrangements for a dest reg.
!When Xb has to be loaded into a register anyway, then the caller can make use
!of that

	mcloperand ax,bx
	int m, scale, extra,offset, reg,regix
	psymbol d

	scale:=p.scale
	extra:=p.extra
	offset:=pclstack[1].value*scale+extra	!for imm offset

	m:=p.mode

	px:=nil

	if isregvaropnd(xb) then
		if isregvaropnd(ya) then			!regvar/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(areg:pclstack[2].reg,ireg:regix, offset:extra, scale:scale)

		elsif isimm64(ya) then			!regvar/imm
			px:=mgenindex(areg:pclstack[2].reg, offset:offset)
		else							!regvar/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(areg:pclstack[2].reg, ireg:bx.reg, scale:scale, offset:extra)
		fi
	elsif ismemaddr(xb) then
		d:=pclstack[2].def
		if isregvaropnd(ya) then			!memaddr/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(ireg:regix, def:d, offset:extra, scale:scale)

		elsif isimm64(ya) then			!memaddr/imm
			px:=mgenindex(def:d, offset:offset)
		else							!memaddr/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(ireg:bx.reg, def:d, offset:extra, scale:scale)
		fi
	else								!
		ax:=genopnd_ld(xb)
		if isregvaropnd(ya) then			!any/regvar
			reg:=pclstack[1].reg
			regix:=scaleregvar(reg,scale,1)
			px:=mgenindex(areg:ax.reg, ireg:regix, offset:extra, scale:scale)
		elsif isimm64(ya) then			!any/imm
			px:=mgenindex(areg:ax.reg, offset:offset)
		else							!any/any
			scale:=scaleindex(bx:=genopnd_ld(ya),scale)
			px:=mgenindex(areg:ax.reg, ireg:bx.reg, scale:scale, offset:extra)

		fi
	fi

!CPL "DAM",PX.SIZE,P.SIZE
	if px.size=0 then px.size:=p.size fi
	return px
end

function scaleregvar(int reg, &scale, n)int=
!When scale is 1/2/3/4, return reg (a regvar) and scale unchanged;
!otherwise set up a new register for operand n
!Copy reg to it, and scale. Return new reg, and set scale to 1
	int regix
	mcloperand ax

	if scale in [1,2,4,8] then return reg fi

	regix:=getnextreg()
	ax:=mgenreg(regix)
	genmc(m_mov,ax, mgenreg(reg))

	pclstack[n].fmt:=reg_d64
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=regix
	mulimm(ax,scale)
!	genmc(m_imul2, ax, mgenint(scale))
	scale:=1

	return regix
end

function scaleindex(mcloperand ax, int scale)int=
!when scale is 1/2/3/4, return scale unchanged
!anything else, scale value in ax, return 1
	int n
	if scale in [1,2,4,8] then return scale fi
	mulimm(ax,scale)
!
!	n:=ispoweroftwo(scale)
!	if n then
!		genmc(m_shl, ax, mgenint(n))
!	else
!		genmc(m_imul2, ax, mgenint(scale))
!	fi
	return 1
end

function makeregopnd(int n)mcloperand ax=
!turn given pcl operand, which does not occupy a register,
!make it into register operand. Note that other characteristics, such
!as value/def for imm/mem/memaddr, are not affected
!offset = xa, yb etc

	pclstack[n].fmt:=reg_d64
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=getnextreg()

	return genopnd(n)
end

proc dobinto_int(pcl p, int opc)=
	mcloperand ax,bx,rx
	int reg,size

	size:=p.size

	if size=8 and ismemaddr(xb) then
		ax:=mgenmem(pclstack[2].def)
		reg:=getnextreg()
		rx:=mgenreg(reg)
		genmc(m_mov, rx, ax)
		bx:=genopnd(ya)
		genmc(opc,rx,bx)
		genmc(m_mov, ax,rx)
		freereg(reg)
	else
		ax:=genopnd_ind(xb,size:size)
		bx:=genopnd_ld(ya,size)

		genmc(opc,ax,bx)
	fi
	delopnd()
	delopnd()
end

proc dobinto_float(pcl p, int opc)=
	mcloperand ax,bx,cx

	addreg_x64()
	ax:=genopnd_ind(xc)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	genmc(m_movq, cx,ax)
	genmc(opc, cx,bx)
	genmc(m_movq, ax,cx)

	delopnd()
	delopnd()
	delopnd()
end

proc dobinto_float32(pcl p, int opc)=
	mcloperand ax,bx,cx

	addreg_x32()
	ax:=genopnd_ind(xc,4)
	bx:=genopnd(yb)
	cx:=genopnd(za)

	genmc(m_movd, cx,ax)
	genmc(opc, cx,bx)
	genmc(m_movd, ax,cx)

	delopnd()
	delopnd()
	delopnd()
end

proc doselectcc(pcl p)=
	mcloperand ax,bx, cx,dx
	int cond

	if pcat[p.mode]<>d64cat then merrort("selectcc",p.mode) fi

	ax:=genopnd_ld(xb)
!	bx:=genopnd_ld(ya)
	bx:=genopnd(ya)
	genmc(m_cmp,ax,bx)

	noxorclear:=1
	dx:=genopnd_ld(xc)
	cx:=genopnd_ld(wd)
	noxorclear:=0
	if psigned[p.mode] then
		cond:=scondcodes[p.opcode-kselecteq]
	else
		cond:=ucondcodes[p.opcode-kselecteq]
	fi
	genmc_cond(m_cmovcc, reversemcond(cond), cx,dx)
	delopnd()
	delopnd()
	delopnd()
end

function reversemcond(int cond)int=

	case cond
	when z_cond then return nz_cond
	when nz_cond then return nz_cond

	when lt_cond then return ge_cond
	when le_cond then return gt_cond
	when ge_cond then return lt_cond
	when gt_cond then return le_cond

	when ltu_cond then return geu_cond
	when leu_cond then return gtu_cond
	when geu_cond then return ltu_cond
	when gtu_cond then return geu_cond
	esac
	return 0
end

proc do_divrem(pcl p, int issigned, isdiv)=
	int opc, n, shifts

	loadopnd(2)

	if isdiv and pclstack[1].fmt=imm_d64 then
		n:=pclstack[1].value
		case n
		when 0 then
			merror("Divide by zero")
		when 1 then
			delopnd()
			return
		else
			shifts:=ispoweroftwo(n)
			if shifts then
				genmc((issigned|m_sar|m_shr), genopnd(xb), mgenint(shifts))
				delopnd()
				return
			fi
		esac
	fi 

	loadopnd(1)
	saverdx()
	fixdivopnds()

	if issigned then
		genmc(m_cqo)
		opc:=m_idiv
	else
		genmc(m_xorx, mgenreg(r11),mgenreg(r11))
		opc:=m_div
	fi

	genmc(opc, genopnd(ya,p.size))

	if not isdiv then
		genmc(m_xchg,mgenreg(r0),mgenreg(r11))
	fi
	restorerdx()

	delopnd()

end

proc fixdivopnds=
!two div operands exist as the top two operands, which will be
!in registers
!the div op requires that x is in d0, and y in any other register
!d11 also needs to be free, which will be the case is reg allocs only
!go up to d9, and d10/d11/12/13 are in use for win64 parameter passing
	int regx,regy,zop

	regx:=pclstack[2].reg
	regy:=pclstack[1].reg

	if regx=r0 then			!regy will be OK
		return
	fi
	if regy=r0 then			!need to swap then
		genmc(m_xchg,genopnd(xb),genopnd(ya))
		swapopnds(1,2)		!switch operands
		return
	fi

!neither x nor y in r0
	if regset[r0]=0 then	!d0 not in use
		genmc(m_xchg,mgenreg(r0),genopnd(xb))
		regset[regx]:=0
		pclstack[2].reg:=r0
		regset[r0]:=1
		return
	fi

!need to move current occupier of r0
!	for zop:=1 to noperands do
	for zop:=noperands downto 1 do
		if pclstack[zop].loc=reg_loc and pclstack[zop].reg=r0 then exit fi
	od

!zop is the operand number that happens to be using r0
	genmc(m_xchg,mgenreg(r0),genopnd(xb))	
	swap(pclstack[2].reg,pclstack[zop].reg)		!switch registers

end

proc saverdx=
	if inf_r11used then
		genmc(m_push, mgenreg(r11))
	fi
end

proc restorerdx=
	if inf_r11used then
		genmc(m_pop, mgenreg(r11))
	fi
end

proc doincr(pcl p, int incrop, addop)=
	if p.stepx=1 then
		if ismemaddr(xa) then
			genmc(incrop, mgenmem(pclstack[1].def))
		else
			genmc(incrop, genopnd_ind(xa))
		fi
	else
		if ismemaddr(xa) then
			genmc(addop, mgenmem(pclstack[1].def), mgenint(p.stepx))
		else
			genmc(addop, genopnd_ind(xa), mgenint(p.stepx))
		fi
	fi
	delopnd()
end

proc doincrload(pcl p, int incrop, addop)=
	mcloperand ax, mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclstack[1].def)
		ax:=makeregopnd(xa)
	else
		mx:=genopnd_ind(xa,p.size)
		ax:=genopnd(xa)
	fi

	if p.stepx=1 then
		genmc(incrop, mx)
	else
		genmc(addop, mx, mgenint(p.stepx))
	fi

	if pcat[p.mode]=shortcat then
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi
end

proc doloadincr(pcl p, int incrop, addop)=
	mcloperand ax,mx

	if ismemaddr(xa) then
		mx:=mgenmem(pclstack[1].def)
	else
		mx:=genopnd_ind(xa,p.size)
	fi

	addreg_d64()
	ax:=genopnd()

	if pcat[p.mode]=shortcat then
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mx)
	else
		genmc(m_mov, ax,mx)
	fi

	if p.stepx=1 then
		genmc(incrop,mx)
	else
		genmc(addop,mx, mgenint(p.stepx))
	fi

	swapopnds(1,2)
	delopnd()
end

proc dobin_i128(pcl p, int opc1, opc2)=
	mcloperand axl,axh, bxl, bxh

	axl:=genopnd_ld(3)
	axh:=genopnd_ld(4)
	bxl:=genopnd(1)
	bxh:=genopnd(2)

	genmc(opc1, axl, bxl)
	genmc(opc2, axh, bxh)

	delopnd()
	delopnd()
end

proc dojumpcc_i128(pcl p)=
	mcloperand axlow,axhigh,bxlow,bxhigh, cx,dx, lxtrue, lxfalse

	if p.mode=tpu128 and p.opcode not in [kjumpeq, kjumpne] then
		dojumpcc_u128(p)
		return
	fi

	lxtrue:=mgenlabel(p.labelno)

	axhigh:=genopnd_ld(4)
	axlow:=genopnd_ld(3)
	bxhigh:=genopnd(2)
	bxlow:=genopnd(1)

	case p.opcode
	when kjumpeq then
		genmc(m_cmp,axlow,bxlow)
		genmc_cond(m_jmpcc,ne_cond,lxfalse:=mgenlabel())
		genmc(m_cmp,axhigh,bxhigh)
		genmc_cond(m_jmpcc,eq_cond,lxtrue)
		genmc(m_label,lxfalse)
	when kjumpne then
		genmc(m_cmp,axlow,bxlow)
		genmc_cond(m_jmpcc,ne_cond,lxtrue)
		genmc(m_cmp,axhigh,bxhigh)
		genmc_cond(m_jmpcc,ne_cond,lxtrue)
	else
		genmc(m_sub, axlow,bxlow)
		genmc(m_sbb, axhigh,bxhigh)

		genmc(m_cmp,axhigh, zero_opnd)
		if psigned[p.mode] then
			case p.opcode
			when kjumplt then
				genmc_cond(m_jmpcc, lt_cond, lxtrue)
			when kjumple then
				genmc_cond(m_jmpcc, lt_cond, lxtrue)
				genmc(m_orx,axlow,axhigh)
				genmc_cond(m_jmpcc, eq_cond, lxtrue)
			when kjumpgt then
				genmc_cond(m_jmpcc, lt_cond, lxfalse:=mgenlabel())
				genmc(m_orx,axlow,axhigh)
				genmc_cond(m_jmpcc, ne_cond, lxtrue)
				genmc(m_label,lxfalse)
			when kjumpge then
				genmc_cond(m_jmpcc, ge_cond, lxtrue)
			esac
		else
			merrort("jumpcc/rel",p.mode)
		fi
	esac

	delopnd()
	delopnd()
	delopnd()
	delopnd()

end

proc dojumpcc_u128(ref pclrec p) =
!handled relops (not eq/ne)
	mcloperand lxtrue,lxfalse, ax1,bx1,ax2,bx2
	int cond1,cond2,cond3

	case p.opcode
	when kjumpgt then
		cond1:=gtu_cond
		cond2:=ltu_cond
		cond3:=gtu_cond
	when kjumpge then
		cond1:=gtu_cond
		cond2:=ltu_cond
		cond3:=geu_cond
	when kjumplt then
		cond1:=ltu_cond
		cond2:=gtu_cond
		cond3:=ltu_cond
	when kjumple then
		cond1:=ltu_cond
		cond2:=gtu_cond
		cond3:=leu_cond
	else
		MERROR("JCC/U128")
	esac

	lxtrue:=mgenlabel(p.labelno)
	lxfalse:=mgenlabel(++mlabelno)

	ax2:=genopnd_ld(4)
	ax1:=genopnd_ld(3)
	bx2:=genopnd(2)
	bx1:=genopnd(1)

	genmc(m_cmp,ax2,bx2)
	genmc_cond(m_jmpcc, cond1, lxtrue)
	genmc_cond(m_jmpcc, cond2, lxfalse)
	genmc(m_cmp,ax1,bx1)
	genmc_cond(m_jmpcc, cond3, lxtrue)

	genmc(m_label,lxfalse)

	delopnd()
	delopnd()
	delopnd()
	delopnd()
end

proc do_syscall(int fnindex, nargs, retcat)=
!retcat = 0, d64cat, x64cat, x32cat, widecat

	int nslots

	sa_nargs:=nargs
	px_setargs(nil)

	nslots:=do_pushparams(nil,0)

	genmc(m_call, mgenlabel(getrtsproclabel(fnindex)))

!	mccodex.a.mode:=a_mem
	mccodex.a.size:=8

	to sa_nargs do
		poparg()
	od
	popslots(nslots)

	getretvalue_bycat(retcat)
end

proc getretvalue_bycat(int cat)=
	case cat
	when 0 then
		return
	when d64cat then
		addreg0(r0)
	when x64cat then
		addxreg0(r0,xreg_x64)
	when x32cat then
		addxreg0(r0,xreg_x32)
	when widecat then
		addwidereg0(r0)
	else
		merror("getval bycat")
	esac
end

proc px_pushlabel(pcl p)=
	if p.opndtype<>label_opnd then merror("pushlabel") fi
!CPL "ADD LABEL..."
	addlabeladdr(p.labelno)
end

proc do_shiftnto(pcl p,int opc)=
!shift opc=shl/shr/sar, when both operands are on the stack
!first operand is address of dest
	mcloperand px

	px:=genopnd_ind(xb)

	if pclstack[1].fmt=imm_d64 then
		genmc(opc, px, mgenint(pclstack[1].value))
	else
		if inf_r10used then merror("shiftto:cl in use") fi
		loadparam(1,r10)
		genmc(opc, px, mgenreg(r10,1))
	fi

	delopnd()
	delopnd()
end

proc domax_float(int opc)=
	mcloperand ax,bx
	ax:=genopnd_ld(xb)
	bx:=genopnd(ya)
	genmc(opc,ax,bx)
	delopnd()
end

proc domax_int(int cond)=
	mcloperand ax,bx

	ax:=genopnd_ld(xb)
	bx:=genopnd_ld(ya)

	genmc(m_cmp, ax, bx)
	genmc_cond(m_cmovcc, cond, ax, bx)

	delopnd()
end

proc domaxto_r64(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)
	addreg_x64()
	ax:=genopnd(xa)

	genmc(m_movq, ax, px)

	genmc(m_comisd, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movq, px,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_r32(int cond)=
	mcloperand px,ax,bx,lx
	int lab

	px:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)
	addreg_x32()
	ax:=genopnd(xa)

	genmc(m_movd, ax, px)

	genmc(m_comiss, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_movd, px,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
	delopnd()
end

proc domaxto_int(int cond)=
	mcloperand ax,bx,lx
	int lab

	ax:=genopnd_ind(xb)
	bx:=genopnd_ld(ya)

	genmc(m_cmp, ax, bx)
	lab:=++mlabelno

	genmc_cond(m_jmpcc, cond, lx:=mgenlabel(lab))
	genmc(m_mov, ax,bx)
	genmc(m_label, lx)
	delopnd()
	delopnd()
end

proc dosetfntable=
	if lab_funcnametable=0 then
		lab_funcnametable:=++mlabelno
		lab_funcaddrtable:=++mlabelno
		lab_funcnprocs:=++mlabelno
	fi
end

proc loadtoreg(mcloperand rx, ax, int m)=
	if pcat[m]=d64cat then
		genmc(m_mov, rx, ax)
	elsif psigned[m] then
		genmc(m_movsx, rx, ax)
	else
		genmc(m_movzx, rx, ax)
	fi
end

proc storefromreg(mcloperand ax, rx, int size)=
	genmc(m_mov, ax, changeopndsize(rx,size))
end

proc domaths(pcl p, ichar opname)=
	int nslots
	p.nargs:=1

	px_setargs(p)

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenextname(opname))

	poparg()

	popslots(nslots)

	dogetretvalue(p)
end

proc domaths2(pcl p, ichar opname)=
	int nslots
	p.nargs:=2

	px_setargs(p)

	nslots:=do_pushparams(p,0)

	genmc(m_call, mgenextname(opname))

	poparg()
	poparg()

	popslots(nslots)

	dogetretvalue(p)
end
=== pc_genss.m 0 0 20/44 ===
const wmask = 2x1000				!1 means 64-bit operand size
const rmask = 2x0100				!extends mod/rm reg field
const xmask = 2x0010				!extends sib index field
const bmask = 2x0001				!extends mod/rm r/m field, also sib base field

int rex
int sizeoverride					!32=>16 switch
int addroverride					!32=>16 switch
int f2override						!xmm regs
int f3override						!xmm regs

mcloperand extraparam

int currseg=0
ref dbuffer currdata				!copy of ss_idata or ss_code
ref relocrec currrelocs
int nrelocs

int instrno=2

REF MCLREC CURRMCL

global proc genss=
	int index
	ref mclrec m

	initlib(mlabelno)

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

	fixregvar()

	m:=mccode
	index:=0

	while m do
		doinstr(m,++index)
		m:=m.nextmcl
	od

	switchseg(0)					!update ss_currrelocs etc

	if bufferlength(ss_zdata) then
		axerror("Zdata contains code or data")
	fi
end

proc doinstr(ref mclrec m,int index)=
	mcloperand a,b
	psymbol d,e
	int x,offset,shortjmp,n

	buffercheck(currdata)

	rex:=sizeoverride:=addroverride:=f2override:=f3override:=0

	a:=m.a
	b:=m.b

	++instrno
	alineno:=instrno

	CURRMCL:=M

	switch m.opcode
	when m_procstart then
		CURRASMPROC:=M.A.DEF
	when m_procend then
	when m_define then

	when m_definereg then
	when m_deleted then

	when m_labelname then
		case a.valtype
		when stringimm_val then
		when def_val then
			d:=a.def
			d.reftype:=back_ref
			d.segment:=currseg
			d.offset:=getcurrdatalen(6)

			if d.isexported then
				getstindex(d)
			fi

			dofwdrefs(d)
		esac

	when m_label then
		d:=labeldeftable[a.labelno]

		d.reftype:=back_ref
		d.segment:=currseg
		d.offset:=getcurrdatalen(6)
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

	when m_ddoffset then
		genrel32(a)

	when m_segment then
		switchseg(a.value)

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

	when m_neg, m_notx, m_mul, m_imul, m_div, m_idiv then
		do_neg(a,mclcodes[m.opcode])

	when m_add, m_sub, m_andx, m_orx, m_xorx, m_adc, m_sbb, m_cmp then
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
!		if x not in 1..16384 then axerror("align2") fi
			if x<1 or x>16384 then axerror("align2") fi
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

	when m_comment then
	when m_blank then
	else
		println "*** Can't do opcode",mclnames[m.opcode],"line",alineno,=M.OPCODE,=M_HALT
	CPL
	CPL
	AXERROR("STOPPING")
	endswitch

end

proc genbyte(int x)=
	currdata.pcurr++^:=x
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

proc genopnd(mcloperand a,int size=0)=
!generate any label/offset/label+offset/immstring part
!ignore reg etc
!any labels, assume abs addresses of 32 or 64 bits
	ref char s
	int64 x
	int length

	if size=0 then size:=a.size fi

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
			real32 x32
			x32:=a.xvalue
			gendword(int32@(x32))
		when realmem_val then
			CPL "		OPND/REALMEM4"
		when stringimm_val then
			CPL "		OPND/STRINGIMM4"
		when def_val,label_val then
			genabs32(a)
		when name_val then
			CPL "		OPND/NAME4"
		else
			cpl valtypenames[a.valtype]
			axerror("OPND/4/VALTYPE?")
		esac

	when 8 then
		case a.valtype
		when intimm_val then
			genqword(a.value)
		when realimm_val then
			genqword(int64@(a.xvalue))
		when realmem_val then
			CPL "		OPND/REALMEM8",ALINENO
		when stringimm_val then
			CPL "		OPND/STRINGIMM8"
		when def_val,label_val then
			genabs64(a)
		when name_val then
			CPL "		OPND/NAME8"
		else
			CPL "HERE"
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

	r:=pcm_alloc(relocrec.bytes)
	r.nextreloc:=currrelocs
	r.reloctype:=reloctype
	r.offset:=getcurrdatalen(1)-adjust
	r.stindex:=stindex

	++nrelocs
	currrelocs:=r
end

function getstindex(psymbol d)int=
!retrieve existing obj st index, or create new one
	if d.stindex=0 then
		if ss_nsymbols>=ss_symboltablesize then
			extendsymboltable()
		fi
		d.stindex:=++ss_nsymbols
		ss_symboltable[d.stindex]:=d

		if d.segment=0 then
			if d.isimported then
				d.segment:=code_seg
			fi
		fi

	fi
	return d.stindex
end

proc genrel32(mcloperand a)=
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
	else								!external symbol
		gendword(a.offset)		!this is probably just zero
		addrelocitem(rel32_rel,d)
	esac
end

function getdef(mcloperand a,int dneeded=0)psymbol =
	psymbol d

	if a.mode in [a_mem,a_imm] then
		case a.valtype
		when label_val then
			return labeldeftable[a.labelno]
		when def_val then
			d:=a.def
			if d.reftype=0 then
				if not d.isimported then
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

proc genabs32(mcloperand a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then

		gendword(d.offset+a.offset)
		addrelocitem(addr32_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(4),addr32_rel,currseg)
!CPL "NEW1",D.FWDREFS, D.NAME,D
		if d.pcldef and d.pcldef.opcode in [klocal, kparam] then
			gendword(d.offset+a.offset)
		else
			gendword(a.offset)
			addrelocitem(addr32_rel,d)
		fi

	else								!external symbol
		gendword(a.offset)					!this is probably just zero
		addrelocitem(addr32_rel,d)
	esac
end

proc genabs64(mcloperand a)=
!absolute refs to labels
	psymbol d

	d:=getdef(a,1)

	case d.reftype
	when back_ref then
		genqword(d.offset+a.offset)
		addrelocitem(addr64_rel,d)

	when fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(5),addr64_rel,currseg)
		if d.pcldef and d.pcldef.opcode in [klocal, kparam] then
			genqword(d.offset+a.offset)
		else
			genqword(a.offset)
			addrelocitem(addr64_rel,d)
		fi

	else								!external symbol
		genqword(a.offset)				!this is probably just zero
		addrelocitem(addr64_rel,d)
	esac
end

function getrel32(psymbol d,int offset)int=
!get rel difference between offset in this segment, and label d
	if d.reftype=back_ref then					!defined earlier in this segment
		if d.segment<>currseg then
			axerror("Rel label across segments2")
		fi
		return d.offset-(offset+1)
	else
		return int32.maxvalue
	fi
end

proc dofwdrefs(psymbol d)=
!label d has been encountered
!update any fwd refs
!assume inside same offset, at least for rel-32 which only works in text segment
	ref fwdrec f
	int offset, seg
	ref byte p8
	ref int32 p32
	ref int64 p64
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

proc do_push(mcloperand a)=
	int code,am

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("pushreg not 64-bit") fi
		code:=regcodes[a.reg]
		if code>=8 then
			rex :=bmask
			code iand:=7
		fi
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
		am:=genrm(a,6)
		genrex()
		genbyte(0xFF)
		genamode(a,am)
	else
		axerror("push opnd?")
	esac
end

proc do_pop(mcloperand a)=
	int code, am

	case a.mode
	when a_reg then
		if a.size<>8 then axerror("popreg not 64-bit") fi
		code:=regcodes[a.reg]
		if code>=8 then
			rex :=bmask
			code iand:=7
		fi
		genrex()
		genbyte(0x58+code)

	when a_mem then
		if a.size<>8 then axerror("pop not 64-bit") fi
		am:=genrm(a,0)
		genrex()
		genbyte(0x8F)
		genamode(a,am)
	else
		axerror("pop opnd?")
	esac
end

proc do_inc(mcloperand a,int code)=
!inc/dec
	int opc, am

	opc:=(a.size=1|0xFE|0xFF)

	case a.mode
	when a_reg, a_mem then
		am:=genrm(a,code)
		checkhighreg(a)
		setopsize(a)
		genrex()
		genbyte(opc)
		genamode(a,am)

	else
		axerror("inc/opnd?")
	esac
end

proc do_neg(mcloperand a,int code)=
!neg/not/mul/imul/div/idiv
int opc, am

	opc:=(a.size=1|0xF6|0xF7)

	case a.mode
	when a_reg, a_mem then
		am:=genrm(a,code)
		checkhighreg(a)
		setopsize(a)
		genrex()
		genbyte(opc)
		genamode(a,am)

	else
		axerror("neg/div/etc opnd?")
	esac
end

proc genamode(mcloperand a,int am)=
	int sib,mode,dispsize,offset
	psymbol d

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
		getdispsize(a,offset)
		genbyte(offset)
	when 4 then

		case a.mode
		when a_mem then

			case a.valtype
			when def_val, label_val then
				genabs32(a)
			when no_val then
				getdispsize(a,offset)
				gendword(offset)
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

function makemodrm(int mode,opc,rm)int=
	return mode<<6+opc<<3+rm
end

proc setopsize(mcloperand a)=
	case a.size
	when 1 then			!assume set via specific opcodes
	when 2 then			!override default 4 bytes
		sizeoverride:=1
	when 8 then			!override default 4 bytes
	    rex ior:=wmask
	when 4 then			!assume 4 bytes is default
	else
		axerror("Operand size not set")
	esac
end

proc setaddrsize(mcloperand a)=
	if a.mode=a_mem and a.addrsize=4 then
		addroverride:=1
	fi
end

function getdispsize(mcloperand a, int &offset)int=
!look at imm/mem displacement, and return 0,1 or 4
!0 is returned when no disp is needed (no labeldef and offset is zero)
!unless mand=1 then 1 is returned
	psymbol d

	d:=getdef(a)
	offset:=a.offset

	if d then
		if d.pcldef and d.pcldef.opcode in [klocal, kparam] then
			offset+:=d.offset
		else
			return 4
		fi
	fi

	if offset then
		return (isbytesized(offset)|1|4)
	else
		return 0
	fi
end

function genrm(mcloperand a,int opc)int=
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
	int mode, rm, scale, dispsize, sib, index, base
	int reg, regix, code, offset

	mode:=rm:=0				!modrm is (mode, x, rm), of (2,3,3) bits
	scale:=0				!0=modrm only; 1/2/4/8 means sib used
	dispsize:=0
	sib:=-1

	if a.mode=a_mem and a.addrsize=4 then
		addroverride:=1
	fi

	case a.mode
	when a_reg then			!modrm can only ref to a single register
		code:=getregcodeb(a.reg)
		return makeam(makemodrm(3,opc,code), sib, dispsize)

	when a_mem then

	when a_xreg then
		code:=getregcodebx(a.reg)
		return makeam(makemodrm(3,opc,code), sib, dispsize)		!NEW

	else
		axerror("genrm not mem")
	esac

	reg:=a.reg
	regix:=a.regix

	if reg=regix=0 then						!address only
		mode:=0
		rm:=4
		scale:=1
		index:=4
		base:=5
		dispsize:=4

	elsif a.scale<=1 and regix=0 then			!simple address mode (no sib)
		dispsize:=getdispsize(a,offset)
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
		scale:=(a.scale|a.scale|1)
		base:=5
		index:=regcodes[regix]
		if regix=rstack then axerror("Scaled rstack?") fi

	else										!assume regix used; optional reg and disp
		dispsize:=getdispsize(a,offset)
		if dispsize then
			mode:=(dispsize=1|1|2)
		fi
		rm:=4

		scale:=(a.scale|a.scale|1)
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

		if regix=rstack and scale>1 then axerror("Can't scale rstack") fi

	fi

	if index>=8 then rex ior:= xmask; index iand:=7 fi
	if base>=8  then rex ior:= bmask; base  iand:=7 fi

	if scale then
		sib:=scaletable[scale]<<6 + index<<3 + base
	fi
	rm iand:=7

	return makeam(makemodrm(mode:mode,opc:opc,rm:rm), sib, dispsize)
end

function makeam(int m,s,d)int=
!convert mode, sib, dispsize into 32-bit value::
! ssssssss ssssssss mmmmmmmm dddddddd
!return m<<16+s<<8+d
!note: s can be -1, so allow to extend into sign bit::
	return s<<16+m<<8+d
end

proc do_arith(mcloperand a,b,int code)=
!code is 3-bit 0..7 value indicating which of add, sub, and, or, xor, adc, sbb, cmp
!ops is being done
	int am, regcode, opc, dispsize
	int64 x

	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			regcode:=getregcoder(a.reg)
			am:=genrm(b,regcode)
			checkhighreg(a)
			checkhighreg(b)
			setopsize(a)
			opc:=code<<3 ior (a.size=1|0x02|0x03)
			genrex()
			genbyte(opc)
			genamode(b,am)

		when a_imm then
	doregimm::
			if getdef(b) then
				if code<0 or code>7 then axerror("non-add arith/label") fi
				if a.size<4 then axerror("add imm/size") fi
				am:=genrm(a,code)
				setopsize(a)
				genrex()
				genbyte(0x81)
				genamode(a,am)
				genopnd(b,4)
				return

			fi

			x:=b.value
			dispsize:=1
			if a.size=1 then
				opc:=0x80
			elsif -128<=x<=127 then
				opc:=0x83
			else
				unless -0x8000'0000 <= x <= 0xFFFF'FFFF then axerror("3:exceeding word32 value") end
				opc:=0x81
				dispsize:=(a.size=2|2|4)
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
			axerror("ADD reg,???")
		esac

	when a_mem then
		case b.mode
		when a_reg then
			regcode:=getregcoder(b.reg)
			am:=genrm(a,regcode)
			checkhighreg(b)
			setopsize(b)
			opc:=code<<3 ior (b.size=1|0x00|0x01)
			genrex()
			genbyte(opc)
			genamode(a,am)

		when a_imm then
			go to doregimm
		else
			axerror("ADD mem,???")
		esac

	else
		CPL OPNDNAMES_MA[A.MODE]
		axerror("Can't add to this opnd")
	esac
end

proc do_mov(mcloperand a,b)=
	int regcode, am
	int64 value

	case a.mode
	when a_reg then
		case b.mode
		when a_reg, a_mem then
			if a.size<>b.size and b.size then
				axerror("1:Opnd size mismatch")
			fi
			checkhighreg(a)
			checkhighreg(b)
			regcode:=getregcoder(a.reg)
			am:=genrm(b,regcode)

			setopsize(a)
			genrex()
			genbyte((a.size=1|0x8A|0x8B))
			genamode(b,am)

		when a_imm then
			value:=b.value
			regcode:=getregcodeb(a.reg)
			if getdef(b) and a.size<=2 then axerror("mov imm?") fi
			case a.size
			when 1 then
				checkhighreg(a)
				case a.reg
				when r5,r3,r14,r15 then
					rex ior:=0x40
				esac
				unless -128<=value<=255 then axerror("exceeding byte value") end
				genrex()
				genbyte(0xB0+regcode)
				genbyte(value)

			when 2 then
!			if value not in -32768..65535 then axerror("exceeding word16 value") fi
				unless -32768<=value<=65535 then axerror("exceeding word16 value") end
				genbyte(0x66)
				genrex()
				genbyte(0xB8+regcode)
				genword(value)
			when 4 then
				if getdef(b) then
					genrex()
					genbyte(0xB8+regcode)
					genopnd(b,4)
				else
					unless -0x8000'0000<=value<=u32(0xFFFF'FFFF) then
						CPL value,ref void(value)
						axerror("1:exceeding word32 value")
					end
doreg32::
					genrex()
					genbyte(0xB8+regcode)
					gendword(value)
				fi

			else							!assum 8 bytes
				if getdef(b) then
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
			axerror("MOV REG/??")
		esac
	when a_mem then
		case b.mode
		when a_reg then
			if a.size<>b.size and a.size then
				axerror("2:Opnd size mismatch")
			fi
			regcode:=getregcoder(b.reg)
			checkhighreg(b)
			am:=genrm(a,regcode)
			setopsize(b)
			genrex()
			genbyte((b.size=1|0x88|0x89))
			genamode(a,am)

		when a_imm then
			value:=b.value
			am:=genrm(a,0)
			if getdef(b) and a.size<=2 then axerror("mov imm?") fi

			if a.size=0 then a.size:=1 fi

			case a.size
			when 0,1 then
				unless -128<=value<=255 then axerror("exceeding byte value") end

				setopsize(a)
				genrex()
				genbyte(0xC6)
				genamode(a,am)
				genbyte(value)

			when 2 then
				unless -32768<=value<=65535 then axerror("exceeding word16 value") end
				setopsize(a)
				genrex()
				genbyte(0xC7)
				genamode(a,am)
				genword(value)
			when 4,8 then
				if not getdef(b) then
					unless -0x8000'0000<=value<=0xFFFF'FFFF then axerror("2:exceeding word32 value") end
				fi
				setopsize(a)
				genrex()
				genbyte(0xC7)
				genamode(a,am)
				genopnd(b,4)
			esac

		else
			CPL OPNDNAMES_MA[A.MODE]
			CPL OPNDNAMES_MA[B.MODE]
			axerror("MOV MEM/?")
		esac
	else
		axerror("MOV ?/..")
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


proc do_lea(mcloperand a,b)=
	int regcode, am

	unless a.mode=a_reg and b.mode=a_mem then
		axerror("LEA not reg/mem")
	end

	if a.size<4 then axerror("LEA size error") fi
	regcode:=getregcoder(a.reg)

	am:=genrm(b,regcode)
	setopsize(a)
	genrex()
	genbyte(0x8D)
	genamode(b,am)
end

proc do_movsx(mcloperand a,b,int opc)=
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

	regcode:=getregcoder(a.reg)

	am:=genrm(b,regcode)
	setopsize(a)
	checkhighreg(b)
	genrex()
	genbyte(0x0F)
	genbyte((b.size=1|opc|opc+1))
	genamode(b,am)
end

proc checkhighreg(mcloperand a)=
	if a.mode=a_reg then
		case a.reg
		when r5,r3,r14,r15 then
			rex ior:=0x40
		esac
	fi
end

proc do_exch(mcloperand a,b)=
	int regcode, am

	if a.mode=a_reg and b.mode=a_reg and (a.reg=r0 or b.reg=r0) and a.size<>1 then		!simple r0/reg
		if a.reg<>r0 then				!get a to be r0
			swap(a,b)
		fi
		if a.size<>b.size then axerror("exch size") fi

		setopsize(a)
		regcode:=getregcodeb(b.reg)
		genrex()
		genbyte(0x90+regcode)
		return
	fi

	if a.mode=a_mem then swap(a,b) fi

	unless a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then axerror("exch opnds") end
	if b.size=0 and b.mode=a_mem then b.size:=a.size fi
	if a.size<>b.size then axerror("exch size") fi

	if a.size=1 then
		checkhighreg(a)
		checkhighreg(b)
	fi

	regcode:=getregcoder(a.reg)

	am:=genrm(b,regcode)
	setopsize(a)
	genrex()
	genbyte((a.size=1|0x86|0x87))
	genamode(b,am)
end

proc do_movsxd(mcloperand a,b)=
	int regcode, am

	if b.mode=a_mem and b.size=0 then b.size:=4 fi

	if a.size<>8 or b.size>4 then axerror("movsxd size") fi

	if a.mode<>a_reg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("movsxd opnds")
	fi

	regcode:=getregcoder(a.reg)
	am:=genrm(b,regcode)

	setopsize(a)
	genrex()
	genbyte(0x63)
	genamode(b,am)
end

proc do_imul2(mcloperand a,b)=
	int regcode, am, opc
	int64 value

	if a.mode<>a_reg then
		axerror("imul2 opnds")
	fi
	if b.size=0 then b.size:=a.size fi
	if a.size=1 then axerror("imul2 byte") fi

	case b.mode
	when a_reg,a_mem then
		if a.size<>b.size then axerror("imul2 size") fi
		regcode:=getregcoder(a.reg)
		am:=genrm(b,regcode)

		setopsize(a)
		genrex()
		genbyte(0x0F)
		genbyte(0xAF)
		genamode(b,am)

	when a_imm then						!imul reg1,reg2,imm but implemented as imul reg,imm
		if getdef(b) then axerror("mul/label") fi
		value:=b.value
		regcode:=getregcoder(a.reg)		!same reg used in two places
		regcode:=getregcodeb(a.reg)
		opc:=0xC0+regcode<<3+regcode
		setopsize(a)
		genrex()

		if -128<=value<=127 then
			genbyte(0x6B)
			genbyte(opc)
			genbyte(value)
		elsif a.size=2 then
			genbyte(0x69)
			genbyte(opc)
			genword(value)
		else
			genbyte(0x69)
			genbyte(opc)
			gendword(value)
		fi
	else
		axerror("imul2 opnds")
	esac
end

proc do_shift(mcloperand a,b,int opc)=
	int am, w

	if a.mode<>a_reg and a.mode<>a_mem then axerror("shift opnds1?") fi

	am:=genrm(a,opc)
	checkhighreg(a)
	setopsize(a)
	genrex()
	w:=(a.size=1|0|1)

	case b.mode
	when a_imm then
		if getdef(b) then axerror("shift/label") fi
		if b.value=1 then
			genbyte(0xD0+w)
			genamode(a,am)
		else
			genbyte(0xC0+w)
			genamode(a,am)
			genbyte(b.value)
		fi
	when a_reg then
		if b.reg<>r10 or b.size<>1 then axerror("cl or b10 needed") fi
		genbyte(0xD2+w)
		genamode(a,am)

	else
		axerror("shift opnds2?")
	esac
end

proc do_test(mcloperand a,b)=
	int64 value
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
		opc:=(a.size=1|0xF6|0xF7)
		value:=b.value

		am:=genrm(a,0)
		checkhighreg(a)
		setopsize(a)
		genrex()
		genbyte(opc)
		genamode(a,am)
		case a.size
		when 1 then
			genbyte(value)
		when 2 then
			genword(value)
		else
			gendword(value)
		esac

	elsif a.mode=a_reg and (b.mode=a_reg or b.mode=a_mem) then
	doregmem::
		regcode:=getregcoder(a.reg)
		am:=genrm(b,regcode)
		checkhighreg(a)
		checkhighreg(b)
		setopsize(a)
		genrex()
		genbyte((a.size=1|0x84|0x85))
		genamode(b,am)

	elsif a.mode=a_mem and b.mode=a_reg then
		swap(a,b)
		goto doregmem
	else
		axerror("test opnds")
	fi

end

proc do_loop(mcloperand a,int opc)=
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

proc do_jcxz(mcloperand a,int opsize)=
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

proc do_setcc(int cond, mcloperand a)=
!a is cond
!b is byte reg/mem
	int am

	if (a.mode<>a_reg and a.reg<>a_mem) or a.size>1 then axerror("setcc opnd/size") fi

	am:=genrm(a,0)
	checkhighreg(a)
	genrex()
	genrex()
	genbyte(0x0F)
	genbyte(0x90+cond)
	genamode(a,am)
end

proc do_movxmm(mcloperand a,b,int size)=
!do movd/movq depending on size being 4 or 8
	int am, regcode, regcode1, regcode2

	case a.mode
	when a_reg then
		case b.mode
		when a_xreg then
			if a.size<>size then axerror("1:movdq size") fi

			regcode:=getregcoderx(b.reg)
			am:=genrm(a,regcode)
			setopsize(a)
			genbyte(0x66)
			genrex()
			genbyte(0x0F)
			genbyte(0x7E)
			genamode(b,am)

		else
			axerror("movdq reg,?")
		esac
	when a_xreg then
		case b.mode
		when a_reg then
			if b.size<>size then axerror("3:movdq size") fi
			regcode:=getregcoderx(a.reg)
			am:=genrm(b,regcode)
			setopsize(b)
			genbyte(0x66)
			genrex()
			genbyte(0x0F)
			genbyte(0x6E)
			genamode(a,am)

		when a_xreg then
			regcode1:=getregcoderx(a.reg)
			regcode2:=getregcodebx(b.reg)
			genbyte(0xF3)
			genrex()
			genbyte(0x0F)
			genbyte(0x7E)
			genbyte(0xC0+regcode1<<3+regcode2)

		when a_mem then
			if b.size and b.size<>size then axerror("4:movdq size") fi
			regcode:=getregcoderx(a.reg)
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
			axerror("movdq xreg,?")
		esac
	when a_mem then
		case b.mode
		when a_xreg then
			if a.size and a.size<>size then axerror("5:movdq size") fi
			regcode:=getregcoderx(b.reg)
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
			axerror("movdq mem,?")
		esac
	else
		axerror("movdq opnds")
	esac

end

proc do_arithxmm(mcloperand a,b,int prefix,opc)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("arithxmm opnds")
	fi

	if b.mode=a_xreg then
		regcode:=getregcoderx(a.reg)
		am:=genrm(b,regcode)
		if prefix then genbyte(prefix) fi
		genrex()
		genbyte(0x0F)
		genbyte(opc)
		genamode(a,am)
	else
		regcode:=getregcoderx(a.reg)
		am:=genrm(b,regcode)
		if prefix then genbyte(prefix) fi
		genrex()
		genbyte(0x0F)
		genbyte(opc)
		genamode(b,am)
	fi
end

proc do_logicxmm(mcloperand a,b,int opc,size)=
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("logicxmm opnds")
	fi

	if size=8 then
		genbyte(0x66)
	fi

	if b.mode=a_xreg then
		regcode:=getregcoderx(a.reg)
		am:=genrm(b,regcode)
		genrex()
		genbyte(0x0F)
		genbyte(opc)
		genamode(b,am)
	else
		regcode:=getregcoderx(a.reg)
		am:=genrm(b,regcode)
		genrex()
		genbyte(0x0F)
		genbyte(opc)
		genamode(b,am)
	fi
end

proc do_convertfloat(mcloperand a,b,int prefix)=
!cvtss2sd and cvtsd2ss
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("convertfloat opnds")
	fi

	genbyte(prefix)

	if a.mode=a_xreg then
		regcode:=getregcodeRx(a.reg)
		am:=genrm(b,regcode)
		genrex()
		genbyte(0x0F)
		genbyte(0x5A)
		genamode(b,am)
	else
		regcode:=getregcoderx(b.reg)
		am:=genrm(a,regcode)
		genrex()
		genbyte(0x0F)
		genbyte(0x5A)
		genamode(b,am)
	fi
end

proc do_fix(mcloperand a,b,int prefix,opc)=
!cvtss2si and cvtsd2si opc=2d
!cvttss2si and cvttsd2si opc=2c
	int am, regcode

	if a.mode<>a_reg or (b.mode<>a_xreg and b.mode<>a_mem) then
		axerror("fix opnds")
	fi

	genbyte(prefix)

	if b.mode=a_xreg then
		regcode:=getregcoder(a.reg)
		am:=genrm(b,regcode)
		setopsize(a)
	else
		regcode:=getregcoder(a.reg)
		am:=genrm(b,regcode)
		setopsize(a)
	fi

	genrex()
	genbyte(0x0F)
	genbyte(opc)
	genamode(b,am)
end

proc do_float(mcloperand a,b,int prefix)=
!cvtss2si and cvtsd2si
	int am, regcode

	if a.mode<>a_xreg or (b.mode<>a_reg and b.mode<>a_mem) then
		axerror("float opnds")
	fi

	if b.mode=a_mem then
		if b.size=0 then b.size:=4 fi
		if b.size<>4 and b.size<>8 then axerror("float size") fi
	fi

	genbyte(prefix)

	regcode:=getregcoderx(a.reg)
	am:=genrm(b,regcode)
	setopsize(b)
	genrex()
	genbyte(0x0F)
	genbyte(0x2A)
	genamode(b,am)
end

proc do_call(mcloperand a)=
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
		am:=genrm(a,2)
		setopsize(a)
		setaddrsize(a)
		genrex()
		genbyte(0xFF)
		genamode(a,am)

	esac
end

proc do_jmp(mcloperand a,ref mclrec m)=
	int am, regcode, offset, shortjmp
	psymbol d

	case a.mode
	when a_imm then				!assume label_val
		case a.valtype
		when label_val,def_val then
			d:=getdef(a,1)
			offset:=getrel32(d,getcurrdatalen(11)+1)+a.offset
			if offset<0 and offset>-126 then
				genbyte(0xEB)
				genbyte(offset)
			else
				shortjmp:=0
				if offset>0 then				!fwd jump
!check if destlabel occurs within next 8 instrs, then likely to need short disp
					shortjmp:=checkshortjump(m,d)
				fi

				if not shortjmp then
					genbyte(0xE9)
					genrel32(a)
				else
					genbyte(0xEB)
					genrel8(a)
				fi
			fi
		else
			CPL VALTYPENAMES[A.VALTYPE]
			AXERROR("JMP/IMM NOT LABELNO")
		esac
	else				!indirect jump
		case a.size
		when 0 then a.size:=8
		when 1,2,4 then
			axerror("jmp[]size")
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

proc do_cmovcc(int cond, mcloperand a,b)=
	int am, regcode
	if a.size<>b.size and b.size then
		axerror("3:Opnd size mismatch")
	fi
	if a.size=1 then axerror("cmov/byte") fi
	regcode:=getregcoder(a.reg)
	am:=genrm(b,regcode)

	setopsize(a)
	genrex()
	genbyte(0x0F)
	genbyte(0x40+cond)
	genamode(b,am)
end

proc do_fmem(mcloperand a, int freal, code)=
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
		when 16 then
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

	am:=genrm(a,code)
	genrex()
	genbyte(0xD9+mf<<1)
	genamode(a,am)
end

proc genrel8(mcloperand a)=
!a is a known fwd reference, and expected to be <=127 bytes
	psymbol d

	d:=getdef(a,1)

	if d.reftype=fwd_ref then
		d.fwdrefs:=addfwdref(d.fwdrefs,getcurrdatalen(3),rel8_rel)
!CPL "NEW3",D.FWDREFS, D.NAME,D
		genbyte(0)
	else								!external symbol
		axerror("genrel8")
	fi
end

function checkshortjump(ref mclrec m,psymbol d)int=
!at mccode[index] which should contain a jmp/jmpcc instruction
!d is the labeldef being jumped to
!return 1 if this is certain to be a short jump (8-bit disp) otherwise 0 
!return 0
! d can be a named label, or a labelno; either should have .labelno set
	int n

	n:=0
	m:=m.nextmcl
	while m and n<=8 do
		case m.opcode
		when m_label then
!		++n
			if m.a.labelno=d.labelno then
				return 1
			fi
!		when m_comment, m_blank then
		when m_comment, m_blank, m_deleted then
		else
			++n
		esac
		m:=m.nextmcl
	od

	return 0
end

function addfwdref(ref fwdrec p, int offset, reltype, seg=0)ref fwdrec=
	ref fwdrec q

	q:=pcm_alloc(fwdrec.bytes)
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

proc do_movdqx(mcloperand a,b, int opc)=
int am,regcode

case a.mode
when a_xreg then
	case b.mode
	when a_xreg then
		regcode:=getregcodebx(b.reg)
		am:=genrm(a,regcode)
		genbyte(opc)
		genrex()
		genbyte(0x0F)
		genbyte(0x6F)
		genamode(a,am)

	when a_mem then
		regcode:=getregcoderx(a.reg)
		am:=genrm(b,regcode)
		genbyte(opc)
		genrex()
		genbyte(0x0F)
		genbyte(0x6F)
		genamode(b,am)

	else
		axerror("movdqx?")
	esac
when a_mem then
	case b.mode
	when a_xreg then
		regcode:=getregcoderx(b.reg)
		am:=genrm(a,regcode)
		genbyte(opc)
		genrex()
		genbyte(0x0F)
		genbyte(0x7F)
		genamode(a,am)

	else
		axerror("movdqx")
	esac
else
	axerror("movdqx")
esac

end

proc do_popcnt(mcloperand a,b)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi

	genbyte(0xF3)

	regcode:=getregcodebx(a.reg)
	am:=genrm(b,regcode)
	setopsize(a)
	genrex()
	genbyte(0x0F)
	genbyte(0xB8)
	genamode(b,am)
end

proc do_bsf(mcloperand a,b, int opc)=
	int am, regcode

	if b.mode=a_mem then
		if b.size=0 then b.size:=8 fi
	fi
	if a.size<>b.size then axerror("bsf size") fi

	regcode:=getregcodebx(a.reg)
	am:=genrm(b,regcode)
	setopsize(a)
	genrex()
	genbyte(0x0F)
	genbyte(opc)
	genamode(b,am)
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

proc fixregvar=
	ref mclrec m
	m:=mccode

!	while m do
!		if m.a then fixopnd(m.a) fi
!		if m.b then fixopnd(m.b) fi
!		m:=m.nextmcl
!	od
end

global proc initlib(int nlabels)=
	[256]char str

	ss_symboltable:=pcm_alloc(init_ss_symbols*ref void.bytes)
	ss_symboltablesize:=init_ss_symbols
	ss_nsymbols:=0

	labeldeftable:=pcm_alloc(nlabels*ref void.bytes)
	for i to nlabels do
		labeldeftable[i]:=pcm_allocz(pstrec.bytes)
		labeldeftable[i].labelno:=i
		fprint @&.str,"(L#)",i
		labeldeftable[i].name:=pcm_copyheapstring(&.str)
		labeldeftable[i].reftype:=fwd_ref
	od
end

global function buffercreate(int size=1024)ref dbuffer=
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

global function bufferlength(ref dbuffer a)int=
	return a.pcurr-a.pstart
end

global function bufferelemptr(ref dbuffer a, int offset)ref void=
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

global proc addqword(ref dbuffer a, int64 x)=
	a.pcurr64^:=x
	++(a.pcurr64)
end

=== pc_lex.m 0 0 21/44 ===
!Tokeniser Module

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
	(addrsym,			$),		! &

	(eqsym,				$),		! =

	(eolsym,			$),		! End of line
	(eofsym,			$),		! Eof seen

!	(hashsym,			$),		! #
	(labelsym,			$),		! #123

	(intconstsym,		$),		! 123 64 bits signed
	(realconstsym,		$),		! 123.4 64 bits
	(stringconstsym,	$),		! "ABC"
	(int128constsym,	$),		! 123 128 bits unsigned

	(namesym,			$),		! raw name

	(kopcodesym,		$),		! push etc
	(typesym,			$),		! i32 etc

	(kdummysym,			$)		!
end

macro testmode=0

const etx = 26
const cr  = 13
const lf  = 10

global const hstsize=65536*4
global const hstmask=hstsize-1

global [0:hstsize]psymbol lexhashtable
int nsymbols

!the following returned by updated by lexreadtoken()

global int lxfileno=0	!*@ current source file number
global int lxlineno=0	!*@ current source line number

global int nsourcefiles=0	!no. of linear file names

global int lxsymbol		!* main symbol kind
global int lxsubcode	!* for some symbols, which specific keyword

global int64 lxvalue
global word128 lxuvalue128
global real64 lxxvalue
global ichar lxsvalue
global int lxlength
global byte lxtruename
int lxhashvalue

global ref byte lxsptr		!@ points to next char in source
ref byte lxstart			!@ start of source code for this file
global psymbol lxsymptr	!set by lookuplex()

![0..255]char alphamap
![0..255]char digitmap
[0..255]char commentmap

global proc lex=
!lowest level lex() function, reads names, numbers etc but does no lookups or translations
!returns results in lx-vars. Current source pointer should be in lxsptr
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
		lxtruename:=0

		if lookuplex(cast(pstart),lxlength) then
			if lxsymptr.ksymbol then			!keywords take priority here
				lxsymbol:=lxsymptr.ksymbol
				lxsubcode:=lxsymptr.subcode
			else
				lxsymbol:=lxsymptr.symbol
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
		lxtruename:=1
		if pstart=lxsptr then
			lxerror("NULL ` name")
		fi
		lxlength:=lxsptr-pstart
		lxhashvalue:=hsum<<5-hsum

		if lookuplex(cast(pstart),lxlength) then
			lxsymbol:=lxsymptr.symbol			!can't be a keyword
			if lxsymbol=0 then					!assume was a keyword; use as name
				lxsymbol:=lxsymptr.symbol:=namesym
			fi
		fi
		return

	when '!',';' then			!comment to eol

		while commentmap[lxsptr++^] do od

		if (lxsptr-1)^=0 then --lxsptr fi
!
		++lxlineno

		lxsymbol:=eolsym
		return

	when '#' then				!label
		lxvalue:=0
		doswitch c:=lxsptr++^
		when '0'..'9' then
			lxvalue:=lxvalue*10+c-'0'
		else
			--lxsptr
			exit
		end
	
		if lxvalue=0 then lxerror("Bad label") fi
		if labelnooffset=0 then
			maxuserlabel max:=lxvalue
		else
			lxvalue+:=labelnooffset
		fi
		lxsymbol:=labelsym
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

	when '-' then
		c:=lxsptr++^
		if c not in '0'..'9' then lxerror("Bad no") fi
		readnumber(c,-1)
		return

	when '*' then
		lxsymbol:=mulsym
		return

	when '&' then
		lxsymbol:=addrsym
		return

	when '"' then
		readstring()
		return

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
	static byte done=0

	return when done

	lxsubcode:=0
	lxsymbol:=errorsym

	lxlineno:=0

	for i:=0 to 255 do
!		switch i
!		when 'A'..'Z','a'..'z','$','_','0'..'9' then
!			alphamap[i]:=1
!		end
!		switch i
!		when '0'..'9' then
!			digitmap[i]:=1
!		end
		commentmap[i]:=1
	od

	commentmap[0]:=0
	commentmap[lf]:=0

	inithashtable()

	done:=1
end

proc readreal(ichar s,int slen)=
	int c

	c:=lxsptr^
	lxsptr^:=0
	lxxvalue:=strtod(s,nil)
	lxsptr^:=c

	lxsymbol:=realconstsym
end

proc readnumber(int c, signx=1)=
!A digit c 0..9 has just been read. Numeric formats are::
!1234
!0x1234
!2x1101
!Nx....		possible
	[256]char str
	int i,d,intlen,slen,isfloat,sepseen
	ichar s

	d:=lxsptr^
	case d
	when 'x','X' then			!other base
		case c
		when '0' then			!hex
			++lxsptr
			readhex()
			lxvalue*:=signx
			return
		else
			cpl c
			lxerror("Base not supported")
		esac
	esac

!assume decimal
	str[1]:=c
	s:=lxsptr-1				!start of number in source
	slen:=1
	isfloat:=sepseen:=0

	doswitch c:=lxsptr++^
	when '0'..'9' then
		str[++slen]:=c
	when '_','\'' then
		sepseen:=1
	when '.' then
		if isfloat then lxerror("float?") fi
		isfloat:=1
		intlen:=slen
	when 'e','E' then
		if isfloat=2 then lxerror("float?") fi
		isfloat:=2
	when '-','+' then
		if isfloat<>2 then lxerror("float?") fi
		isfloat:=3
	else
		--lxsptr
		exit
	end

	if isfloat then
		if sepseen then lxerror("seps in float?") fi
		readreal(s,lxsptr-s)
		lxxvalue*:=signx
		return
	fi

	if slen>20 or slen=20 and cmpstring(&.str,"18446744073709551615")>0 then
		if slen>39 or 
				(slen=39 and strncmp(s,"340282366920938463463374607431768211455",39)>0) then
			lxerror("Overflows 128 bits")
		fi
		stringtonumber128(str,slen,10)
		return
	fi

	lxsymbol:=intconstsym

	lxvalue:=0
	for i:=1 to slen do
		lxvalue:=lxvalue*10+str[i]-'0'
	od
	lxvalue*:=signx
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
	elsif ndigs>32 then
		lxerror("Overflow in hex number")
	elsif ndigs>16 then
		lxerror("hex/128 bits not ready")
!		stringtonumber128(str,slen,16)
		return
	fi
	lxsymbol:=intconstsym
end

global proc ps(ichar caption)=
	PRINT CAPTION,LXSPTR,":"
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
		print @dev,lxsymptr.name

	when intconstsym then
		print @dev, lxvalue
	when realconstsym then
		print @dev, lxxvalue
	when stringconstsym then
		print @dev,"""",,lxsvalue,,""""!,,"end"
	when errorsym then
		print @dev,lxvalue
	when kopcodesym then
		print @dev,pclnames[lxsubcode]+1

	when typesym then
		print @dev,pstdnames[lxsubcode]

	else
		print @dev,symbolnames[lxsymbol]
		if lxsubcode then
			print " ",,lxsubcode
		fi

	end

	println @dev,$,lxlineno
end

proc inithashtable=
!initialise hash table from kwddata
	pstready:=0
	for i in pclnames.bounds do
		addreservedword(pclnames[i]+1,kopcodesym,i)
	od

	for i in pstdnames.bounds do
		addreservedword(pstdnames[i],typesym,i)
	od

	addreservedword("proc",kopcodesym,kprocdef)
	addreservedword("function",kopcodesym,kprocdef)
	addreservedword("end",kopcodesym,kendproc)
	addreservedword("endext",kopcodesym,kendextproc)

	pstready:=1

end

proc addreservedword(ichar name,int symbol,subcode)=
	lxhashvalue:=gethashvalue(name)
	if lookuplex(name,0) then
		cpl =name
		lxerror("DUPL NAME")
	fi

	lxsymptr.symbol:=0
	lxsymptr.ksymbol:=symbol
	lxsymptr.subcode:=subcode
end

global proc printhashtable(filehandle devx,ichar caption)=
	psymbol r
	int count,i

	println @devx,caption,":"
	count:=0
	for i:=0 to lexhashtable.upb do
		r:=lexhashtable[i]
		if R AND r.name then
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
	psymbol e
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

		if lxsymptr.namelen=length and memcmp(lxsymptr.name,name,length)=0 then			!match
			return 1
		fi

		if ++j>=hstsize then		!wraparound
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
		name:=pcm_copyheapstringn(name,length)
	fi

	if ++nsymbols>((hstsize*7)/8) then
		lxerror("Hashtable getting full")
	fi

	if lxsymptr=nil then
		lxsymptr:=pcm_allocz(pstrec.bytes)
		lexhashtable[j]:=lxsymptr
	fi

	lxsymptr.name:=name
	lxsymptr.namelen:=length
	lxsymptr.symbol:=namesym
	lxsymptr.ksymbol:=0

	PCL_ADDSYM(LXSYMPTR)

	return 0
end

global proc startlex(ichar source)=
	initlex()
	lxstart:=lxsptr:=cast(source)
	lxlineno:=1
	lxsymbol:=errorsym
end

global function addnamestr(ichar name)psymbol=
!add a new name to the symbol table
!return symptr to new (or existing) generic name
	lxhashvalue:=gethashvalue(name)
	lookuplex(pcm_copyheapstring(name),0)
	return lxsymptr
end

global proc lxerror(ichar m)=			!LXERROR

	fprintln "\w\w Lexical Error\n*** # *** on line #",m,lxlineno
	stop 1
end

global function gethashvalue(ichar s)int=
!get identical hash function to that calculated by lexreadtoken
!but for a zero-terminated string
!assumes S is lower-case, as conversion not done
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

proc readstring=
!read string inplace: new string, with expanded control characters,
!is stored on top of original string in the source
!new string is same length or shorter

	var ichar dest
	var int c,d
	var [8]char str

	lxsymbol:=stringconstsym

	lxsvalue:=lxsptr

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
!			when 'u' then			!reserved for unicode, like \x but with 4 hex digits
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
				println "<",,char(c),,">"
				lxerror("Unknown string escape")
			end
		when '"' then		!possible terminators
			if lxsptr^=c then		!repeated, assume embedded term char
				++lxsptr
			else			!was end of string
				exit
			fi
		when cr,lf,0 then
			lxerror("String not terminated")
		endswitch

		dest++^:=c
	od
	lxlength:=dest-lxsvalue
	(lxsvalue+lxlength)^:=0		!overwrites final " or earlier
end

proc stringtonumber128(ichar s, int length,base)=
	word128 aa
	int c,d
	aa:=0
	to length do
		aa:=aa*base
		c:=(s++)^

		if c>='a' then
			d:=c-'a'+10
		elsif c>='A' then
			d:=c-'A'+10
		else
			d:=c-'0'
		fi

		aa:=aa+d
	od

	lxuvalue128:=aa
	lxsymbol:=int128constsym
end

=== pc_libmcl.m 0 0 22/44 ===
const fasmformat=1
!const fasmformat=0

const fuseregtable=1
!const fuseregtable=0

const targetsize=8

global int fshowmsource=0

global macro isframex(d) = (d.pclop in [klocal, kparam])

[r1..rstack]mcloperand rd

global proc mclinit=
	mcloperand a
	int r,s

	for r:=r0 to r15 do
		regtable[r,1]:=mgenreg0(r,1)
		regtable[r,2]:=mgenreg0(r,2)
		regtable[r,4]:=mgenreg0(r,4)
		regtable[r,8]:=mgenreg0(r,8)
		regtable[r,16]:=mgenreg0(r,16)

		rd[r]:=regtable[r,8]
	od

	zero_opnd:=mgenint0(0)

	for i:=0 to maxsmallint do
		smallinttable[i]:=mgenint0(i)
	od

	for i in frameregtable.bounds do
		a:=newmclopnd()
		a.mode:=a_mem
		a.reg:=rframe
		a.size:=8
		a.offset:=i
		frameregtable[i]:=a
	end

	dframeopnd:=mgenreg(rframe,8)
	dstackopnd:=mgenreg(rstack,8)

	initmcdest()

	setsegment('C')

	stringtable:=pcm_alloc(ref void.bytes*initstringsize)
	stringlabtable:=pcm_alloc(int32.bytes*initstringsize)
	realtable:=pcm_alloc(real.bytes*initrealsize)
	reallabtable:=pcm_alloc(int32.bytes*initrealsize)

	nstrings:=0
	nreals:=0

	stringtablesize:=initstringsize
	realtablesize:=initrealsize

	pclstack:=cast(&pclopndstack[maxoperands])

	lab_funcnametable:=0
	lab_funcaddrtable:=0

end

global proc initmcdest=
!reset mccode/mccodex
!called should have saved any values from last linked list 
	mccode:=mccodex:=nil
	clear rtsproclabels
end

export proc genmc(int opcode, mcloperand a=nil,b=nil)=
	ref mclrec m, oldm
	int labno

	m:=pcm_allocz(mclrec.bytes)
	m.opcode:=opcode

	m.a:=a
	m.b:=b

	case opcode
	when m_call then
		++inf_proccalls

	when m_lea then
		if b and b.valtype=def_val then
			b.def.addrof:=1
		fi
	when m_label then
		labno:=a.labelno
		if labno>maxlabelno then
	CPL =LABNO, MAXLABELNO
			merror("Too many labels")
		fi
		labeltable[labno]:=m

	esac

	if mccode then
		mccodex.nextmcl:=m
		mccodex:=m
	else
		mccode:=mccodex:=m
	fi
end

global proc genmc_cond(int opcode, cond, mcloperand a=nil,b=nil)=
	genmc(opcode,a,b)
	mccodex.cond:=cond
end

global proc genmc_str(int opcode,ichar s)=
!as genmc but uses a single immediate string operand
	genmc(opcode,mgenstring(s))
end

function newmclopnd:mcloperand a=
	a:=pcm_allocz(mclopndrec.bytes)
	return a
end

global function duplopnd(mcloperand a)mcloperand=
	mcloperand b
	b:=pcm_alloc(mclopndrec.bytes)
	b^:=a^
	return b
end

export function mgenxreg(int xreg,size=8)mcloperand=
	mcloperand a

!	if xreg=rnone then xreg:=++currxregno fi
	a:=newmclopnd()

	a.mode:=a_xreg
	a.reg:=xreg
	a.size:=size
	return a
end

export function mgenindex(int areg=0,ireg=0,scale=1,offset=0,size=0, labno=0, psymbol def=nil)mcloperand=
!construct a mem address mode
	mcloperand a
	a:=newmclopnd()

	a.mode:=a_mem
	a.reg:=areg

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
		fi
	fi

	return a
end

global function getmclstr:ref strbuffer=
!write all mcl code in system by scanning all procs
!mcl code is only stored per-proc
	psymbol d,e
	ref mclrec m
	[32]char str2,str3
	int i

	gs_init(dest)

	for i to nplibfiles when plibfiles[i]^<>'$' do
		asmstr("          ")
		asmstr((plibtypes[i]='D'|"importdll "|"importlib "))
		asmstr(plibfiles[i])
		gs_line(dest)
	od

	m:=mccode
	i:=1
	while m do
		writemcl(i,m)
		++i
		m:=m.nextmcl
	od

	return dest
end

global proc mgencomment(ichar s)=
!if not debugmode then return fi
	if s=nil or s^=0 then
		genmc(m_blank)
	else
		genmc_str(m_comment,s)
	fi
end

global function mgenstring(ichar s,int length=-1)mcloperand=
	mcloperand a
	a:=newmclopnd()
	a.mode:=a_imm
	if length<0 then
		length:=strlen(s)
	fi
	a.svalue:=pcm_alloc(length+1)
	memcpy(a.svalue,s,length)
	(a.svalue+length)^:=0

	a.valtype:=stringimm_val
	a.size:=8
	return a
end

global function mgenname(ichar s)mcloperand=
	[64]char str
	mcloperand a
	a:=newmclopnd()
	a.mode:=a_imm
	a.svalue:=pcm_copyheapstring(s)
	a.valtype:=name_val
	a.size:=8

	return a
end

proc writemcl(int index,ref mclrec mcl)=

	case mcl.opcode
	when m_deleted then
	else
		strmcl(mcl)
		gs_line(dest)
	esac
end

global proc strmcl(ref mclrec mcl)=
	static [512]char str
	[128]char opcname
	mcloperand a,b
	int opcode,cond,sizepref
	ichar s,comment
	psymbol d

	opcode:=mcl.opcode

	cond:=mcl.cond
	a:=mcl.a
	b:=mcl.b
	comment:=nil

	case opcode
	when m_procstart then
!CPL "PROCSTART"
		asmstr(";Proc ")
		asmstr(a.def.name)
		currasmproc:=a.def

		return

	when m_procend then
		asmstr(";End ")
		currasmproc:=nil

		return

	when m_blank then
		return
	when m_comment then
		asmchar(';')
		asmstr(a.svalue)
		GOTO DOCOMMENTS
		return
	when m_deleted then
		asmstr("; <deleted>")
		GOTO DOCOMMENTS
		return

	when m_labelname then				!label name will be complete and will have colon(s)
		d:=a.def
!ASMSTR("LABNAME/")
		case a.valtype
		when def_val then
			asmstr(getfullname(d))
		when stringimm_val then
			asmstr(a.svalue)
			return
		else
			merror("strmcl/lab")
		esac

		asmstr(":")

		if d.isexported then
!IF D=PSTENTRYPOINT THEN
!CPL "ASM ENTRY POINT",D.NAME,=PMAINLIB
!FI
			unless d=pstentrypoint and plibmode then
				asmstr("\n")
				asmstr(getbasename(d.name))
				asmstr("::")
			end
		fi
		return

	when m_label then
		fprint @&.str,"L#:",a.value
		asmstr(&.str)
		return

	when m_define then
		asmstr("          ")
		asmstr(a.svalue)
		asmstr(" = ")
		asmopnd(b)
		return

	when m_definereg then
		asmstr("          ")
		asmstr(a.svalue)
		asmstr(" = ")

		case b.mode
		when a_reg then
			asmstr(getregname(b.reg, b.size))
		else
			asmstr(fgetregname(b.reg, b.size))
		esac
		return

	esac

	case opcode
	when m_jmpcc then
		print @&.opcname,"j",,asmcondnames[cond]

	when m_setcc then
		print @&.opcname,"set",,asmcondnames[cond]

	when m_cmovcc then
		print @&.opcname,"cmov",,asmcondnames[cond]

	when m_call then
		strcpy(&.opcname,"call")
	when m_andx then
		strcpy(&.opcname,"and")
	when m_orx then
		strcpy(&.opcname,"or")
	when m_xorx then
		strcpy(&.opcname,"xor")
	when m_notx then
		strcpy(&.opcname,"not")

	ELSIF OPCODE>M_HALT THEN
		STRCPY(&.OPCNAME,STRINT(OPCODE))

	else
		strcpy(&.opcname,mclnames[opcode]+2)
	esac
	ipadstr(&.opcname,10," ")

	if not fasmformat then
		if a and b then
			fprint @&.str,"  #/#",a.size,b.size
		elsif a then
			fprint @&.str,"  #",a.size
		else
			strcpy(&.str,"  ")
		fi
	else
		strcpy(&.str,"  ")
	fi

	ipadstr(&.str,10)

	strcat(&.str,&.opcname)

	asmstr(&.str)

	if a and b then		!2 operands
		sizepref:=needsizeprefix(opcode,a,b)
!
		asmopnd(a,sizepref)
		asmstr(",	")
		asmopnd(b,sizepref)

	elsif a and a.mode then								!1 operand
		if opcode=m_call then
			asmopnd(a,0)
		else
			asmopnd(a,1)
		fi
	fi

DOCOMMENTS::
!IF PRODMODE THEN
!	RETURN
!FI
!	asmstr(" !")
!
!	if comment then
!!		asmstr("	!")
!		asmstr(comment)
!	fi

!	IF MCL.COMMENT THEN
!		ASMSTR(" ")
!		ASMSTR(MCL.COMMENT)
!	FI
!
!	for i in mcl.regend.bounds do
!		if mcl.regend[i] then
!			asmstr(" Free:")
!			asmstr(getregname(i))
!		fi
!	od

end

global proc asmopnd(mcloperand a,int sizeprefix=0,debug=0)=
	asmstr(stropnd(a,sizeprefix,debug))
end

global proc setsegment(int seg,align=1)=
!seg is 'D', 'Z', 'C', 'R' for data, zdata, code, rdata
	int opc,oldalign

	if seg<>currsegment then
		case seg
		when 'I' then opc:=m_isegment
		when 'Z' then opc:=m_zsegment
		when 'C' then opc:=m_csegment
		when 'R' then MERROR("CAN'T DO RODATA SEG")
		else
			MERROR("BAD SEG CODE")
		esac
		if mccodex and mccodex.opcode in [m_isegment,m_zsegment,m_csegment] then
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
		genmc(m_align,mgenint(align))
	fi
end

global function getsizeprefix(int size,enable=0)ichar=
	if not enable then return "" fi
	case size
	when 1 then return "byte "
	when 2 then return "word16 "
	when 4 then return "word32 "
	when 8 then return "word64 "
	when 16 then return "word128 "
	esac
	return ""
end

global function needsizeprefix(int opcode,mcloperand a,b)int=
	case opcode
	when m_movsx, m_movzx, m_cvtsi2ss, m_cvtsi2sd then
		return 1

	when m_cvtss2si,m_cvtsd2si, m_cvttss2si,m_cvttsd2si then
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

global function changeopndsize(mcloperand a,int size)mcloperand=
	mcloperand b

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

global function makeopndind(mcloperand a,int size=0)mcloperand=
	mcloperand b

	if a.mode<>a_reg then
		merror("makeopndind")
	fi

	return mgenireg(a.reg,size)
end

global function applyoffset(mcloperand a,int offset,int size=0)mcloperand=
!astr is an asm operand
!add possible byte offset
	mcloperand b

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

export function mgenint(int64 x,int size=8)mcloperand=
	if x in 0..maxsmallint and size=8 then
		return smallinttable[x]
	fi

	return mgenint0(x,size)
end

global function mgenint0(int64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm

	a.value:=x
	a.valtype:=intimm_val
	a.size:=size

	return a
end

global function mgenrealmem(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_mem
	a.value:=getrealindex(x,size)
	a.valtype:=label_val
	a.size:=size
	return a
end

global function mgenrealimm(real64 x,int size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_imm
	a.xvalue:=x
	a.valtype:=realimm_val
	a.size:=size
	return a
end

export function mgenlabel(int x=0)mcloperand a=
!x is a label index
!generate immediate operand containing label
	a:=newmclopnd()
	a.mode:=a_imm
	if x=0 then x:=++mlabelno fi
	a.value:=x
	a.valtype:=label_val
	return a
end

global function mgenlabelmem(int x)mcloperand a=
!x is a label index
!generate immediate operand containing label

	a:=mgenlabel(x)
	a.mode:=a_mem
	return a
end

global function mgenregvar(psymbol d)mcloperand a=
	a:=mgenreg(d.reg,8)
	isregvar[d.reg]:=1

	return a
end

global function mgenxregvar(psymbol d)mcloperand a=
	a:=mgenxreg(d.reg)
	isxregvar[d.reg]:=1

	return a
end

global function mgenmem(psymbol d)mcloperand a=
	pcl p:=d.pcldef
	int reg

IF P=NIL THEN
CPL D.NAME,".PCLDEF NOT SET"
STOP
FI

	if d.reg then
		if pfloat[p.mode] then
			return mgenxregvar(d)
		else
			return mgenregvar(d)
		fi
	fi

	reg:=rnone
	if isframex(d) then
		if not optimflag and (int(d.offset) in -128..64) and p.size=8 then
			return frameregtable[d.offset]
		fi

		reg:=rframe
	fi

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.def:=d
	++d.nrefs
	a.valtype:=def_val

	a.size:=min(p.size,8)

	return a
end

global function mgenmemhigh(psymbol d)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_mem

	if isframex(d) then
		a.reg:=rframe
	fi
	++d.nrefs
	a.def:=d
	a.valtype:=def_val
	a.offset:=8
	a.size:=8

	return a
end

export function mgenmemaddr(psymbol d)mcloperand=
	mcloperand a

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

export function mgenreg(int reg,size=8)mcloperand=
	if fuseregtable then
		return regtable[reg,size]
	fi
	return mgenreg0(reg,size)
end

global function mgenreg0(int reg,size=8)mcloperand a=
	a:=newmclopnd()
	a.mode:=a_reg
	a.reg:=reg
	a.size:=size
	return a
end

global function mgenireg(int reg,size=8,offset=0)mcloperand=
	mcloperand a

	a:=newmclopnd()
	a.mode:=a_mem
	a.reg:=reg
	a.size:=size
	a.offset:=offset

	return a
end

global function roundsizetg(int size)int=
!make sure size is round up to next multiple of targetsize (4 or 8)
	while size iand (targetsize-1) do ++size od
	return size
end

global function getregname(int reg,size=8)ichar=
	static [1..17]ichar prefix=("B","W","","A","","","","D","","","","","","","","Q","N")
	static [32]char str
	[16]char str2
	ichar rs
	int size2

	size2:=size
	if size2>16 then
		size2:=17
	FI

	case reg
	when rnone then return "-"
	when rframe then rs:="frame"
	when rstack then rs:="stack"
	else
		getstrint(reg-r0,&.str2)
		rs:=&.str2
	esac

	print @&.str,prefix[size2],,rs
	return &.str
end

global function fgetregname(int reg,size=8)ichar=
	static [32]char str

	if reg=rnone then return "-" fi

	if fasmformat then
		print @&.str,"XMM",,reg-xr0
	else
		print @&.str,(size=8|"DX"|"SX"),,reg-xr0
	fi
	return &.str
end

global function sameoperand(mcloperand a,b)int=
	return memcmp(a,b,mclopndrec.bytes)=0
end

global function sameregopnd(mcloperand a,b)int=
!check if same register operand
	unless a.mode=b.mode=a_reg then return 0 end
	return a.reg=b.reg
end

global function getstringindex(ichar s)int=
	if s=nil then			!assume nil
		kk0used:=++mlabelno
		return kk0used
	fi

	if nstrings>=stringtablesize then
		extendstringtable()
	fi

	if nstrings and eqstring(stringtable[nstrings],s) then
		return stringlabtable[nstrings]
	fi

	stringtable[++nstrings]:=s
	stringlabtable[nstrings]:=++mlabelno

	return mlabelno
end

global function getrealindex(real x,int size)int=
	if nreals>=realtablesize then
		extendrealtable()
	fi

	realtable[++nreals]:=x
	++mlabelno
	reallabtable[nreals]:=(size=8|mlabelno|-mlabelno)
	return mlabelno
end

proc extendstringtable=
	ref[]ichar oldstringtable
	ref[]int32 oldstringlabtable
	int oldstringtablesize

	oldstringtablesize:=stringtablesize
	oldstringtable:=stringtable
	oldstringlabtable:=stringlabtable

	stringtablesize*:=2

	stringtable:=pcm_alloc(ichar.bytes*stringtablesize)
	stringlabtable:=pcm_alloc(int32.bytes*stringtablesize)

	for i:=1 to nstrings do
		stringtable[i]:=oldstringtable[i]
		stringlabtable[i]:=oldstringlabtable[i]
	od

	pcm_free(oldstringtable,ichar.bytes*oldstringtablesize)
	pcm_free(oldstringlabtable,int32.bytes*oldstringtablesize)
end

proc extendrealtable=
	ref[]real oldrealtable
	ref[]int32 oldreallabtable
	int oldrealtablesize

	oldrealtablesize:=realtablesize
	oldrealtable:=realtable
	oldreallabtable:=reallabtable

	realtablesize*:=2

	realtable:=pcm_alloc(real.bytes*realtablesize)
	reallabtable:=pcm_alloc(int32.bytes*realtablesize)

	for i:=1 to nreals do
		realtable[i]:=oldrealtable[i]
		reallabtable[i]:=oldreallabtable[i]
	od

	pcm_free(oldrealtable,real.bytes*oldrealtablesize)
	pcm_free(oldreallabtable,int32.bytes*oldrealtablesize)
end

proc asmstr(ichar s)=
	gs_str(dest,s)
end

proc asmchar(int c)=
	gs_char(dest,c)
end

global function getfullname(psymbol d)ichar=
	static [256]char str
	ichar name:=d.name

	if d.reg then
		fprint @str,"#.#",(d.isfloat|"X"|"R"), name
		return str
	fi

	if d.istruename and d.isimported then
		strcpy(str,"`")
		strcat(str,name)
		strcat(str,"*")
	elsif d.isimported then
		strcpy(str,name)
		strcat(str,"*")
	else		
		return name
	fi
!	esac
end 

global function getfulltempname(int tempno)ichar=
	RETURN "TEMP"
end 

export proc merror(ichar mess,ichar param="")=
!	fprintln "MCL Error: # (#) on Line: # in #",mess,param
	fprintln "MCL Error: # (#) [#]",mess,param,mseqno
!		mlineno iand 16777215, sourcefilenames[mlineno>>24]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

export proc merrort(ichar mess,int t)=
	fprintln "MCL Type not supported: # (#) [#]",mess,pstdnames[t], mseqno
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc merroropnd(ichar mess,int opndtype)=
	fprintln "MCL Opnd not supported: # (#) [#]",mess,opndnames[opndtype], mseqno
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global proc genstringtable=
	int col

	setsegment('I',8)

	if kk0used then
		genmc(m_label,mgenlabel(kk0used))
		gendb(0)
	fi
	return unless nstrings

	for i to nstrings do
		genmc(m_label,mgenlabel(stringlabtable[i]))
		genstring(stringtable[i],1)
	od
end

global proc genstring(ichar s, int doterm)=
!string table generated in ax pass, so is just text
!this is target-specific, so should really be moved
	int i, c, seqlen, length
	ref char seq

	length:=strlen(s)

	if length=0 then
		gendb(0)
		return
	fi

	seqlen:=0

	to length do
		c:=s++^
		if c<32 or c>=127 or c='\"' then
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
	if doterm then
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

proc gendq(int a)=
	genmc(m_dq,mgenint(a))
end

proc gendqname(psymbol d)=
	genmc(m_dq,mgenmemaddr(d))
end

proc gendqlabel(int lab)=
	genmc(m_dq,mgenlabel(lab))
end

global proc genrealtable=
	real x

	return unless nreals

	mgencomment("Real Table")
	setsegment('I',8)

	for i to nreals do
		genmc(m_label,mgenlabel(abs(reallabtable[i])))
		x:=realtable[i]

		if reallabtable[i]>0 then
			genmc(m_dq, mgenrealimm(x,8))
		else
			genmc(m_dd, mgenrealimm(x,4))
		fi
	od
end

global proc genabsneg=
	setsegment('I',16)

	if lababs32 then
		mgencomment("lababs32")
		genmc(m_label,mgenlabel(lababs32))
		gendq(0x7FFF'FFFF'7FFF'FFFF)
		gendq(0x7FFF'FFFF'7FFF'FFFF)
	fi
	if lababs64 then
		mgencomment("lababs64")
		genmc(m_label,mgenlabel(lababs64))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		gendq(0x7FFF'FFFF'FFFF'FFFF)
	fi

	if labneg32 then
		mgencomment("labneg32")
		genmc(m_label,mgenlabel(labneg32))
		gendq(0x8000'0000'8000'0000)
		gendq(0x8000'0000'8000'0000)
	fi
	if labneg64 then
		mgencomment("labneg64")
		genmc(m_label,mgenlabel(labneg64))
		gendq(0x8000'0000'0000'0000)
		gendq(0x8000'0000'0000'0000)
	fi

	if labzero then
		mgencomment("labzero")
		genmc(m_label,mgenlabel(labzero))
		gendq(0)
	fi

	if labmask63 then
		mgencomment("mask63/offset64")
		genmc(m_label,mgenlabel(labmask63))
		gendq(0x7FFF'FFFF'FFFF'FFFF)
		genmc(m_label,mgenlabel(laboffset64))
		gendq(0x43E0'0000'0000'0000)
	fi
end

global function mdefinelabel:int =
	genmc(m_label,mgenlabel(++mlabelno))
	return mlabelno
end

global function mcreatefwdlabel:int =
	return ++mlabelno
end

global proc mdefinefwdlabel(int lab) =
	genmc(m_label,mgenlabel(lab))
end

global function stropnd(mcloperand a,int sizeprefix=0,debug=0)ichar=
	static [512]char str
	[128]char str2
	ichar plus,t
	int offset,tc

	str[1]:=0

	case a.mode
	when a_reg then
		return strreg(a.reg, a.size)

	when a_imm then
		strcpy(&.str,strvalue(a))

	when a_mem then
		case a.valtype
		when intimm_val then
			strcpy(&.str,strint(a.value))
		when realimm_val then
			strcpy(&.str,strreal(a.xvalue))
		when realmem_val then
			fprint @&.str,"M#",a.xvalue
		esac

		strcat(&.str,getsizeprefix(a.size,sizeprefix))
		strcat(&.str,"[")

		plus:=""
		if a.reg then
			strcat(&.str,strreg(a.reg,8))
			plus:="+"
		fi
		if a.regix then
			strcat(&.str,plus)
			strcat(&.str,strreg(a.regix,8))
			plus:="+"
			if a.scale>1 then
				strcat(&.str,"*")
				strcat(&.str,strint(a.scale))
			fi
		fi

		if a.valtype in [def_val,label_val, temp_val] then
			if plus^='+' then
				strcat(&.str,plus)
			fi
			strcat(&.str,strvalue(a))
	    elsif offset:=a.offset then
			print @&.str2,offset:"+"
			strcat(&.str,&.str2)
		fi
		strcat(&.str,"]")

	when a_xreg then
		return strxreg(a.reg,a.size)

	else
		println "BAD OPND",A.MODE
		return "<BAD OPND>"
	esac

	return &.str
end

function strreg(int reg, size=8)ichar=
	psymbol d

	d:=checkregvar(reg,0)

	if size=8 and d then
		return getfullname(d)
	else
		getregname(reg,size)
	fi
end

function checkregvar(int reg, isfloat)psymbol d=
	pcl p
!CPL "CHECKREGVAR",CURRASMPROC!.NAME
	if currasmproc=nil then return nil fi
	p:=currasmproc.pcldef+1

	while p.opcode<>kendproc, ++p do
		if p.opcode in [klocal, kparam] then
			d:=p.def
			if d.reg=reg then
!CPL D.NAME,=PSTDNAMES[P.MODE],=D.REG, REG
				if isfloat and pfloat[p.mode] then return d fi
				if not isfloat and not pfloat[p.mode] then return d fi
			fi
		fi
	od
!	od
	return nil
end

function strxreg(int reg, size=8)ichar=
	psymbol d

	d:=checkregvar(reg,1)

	if size=8 and d then
		return getfullname(d)
	else
		return fgetregname(reg,size)
	fi
end

global function strvalue(mcloperand a)ichar=
	static [512]char str
	[128]char str2
	psymbol def
	int64 value,offset,length
	ichar ss

	def:=a.def
	value:=a.value

	strcpy(&.str,"")

	case a.valtype
	when def_val then
		strcat(&.str,getfullname(def))

	addoffset::
		if offset:=a.offset then
			print @&.str2,(offset>0|"+"|""),,offset
			strcat(&.str,&.str2)
		fi

	when temp_val then
		strcat(&.str,getfulltempname(a.tempno))

	when intimm_val then
		strcat(&.str,strint(value))

	when realimm_val then
		print @&.str,a.xvalue:"20.20"

	when realmem_val then
		strcat(&.str,"M")
		strcat(&.str,strreal(a.xvalue))

	when stringimm_val then
		strcat(&.str,"""")
		strcat(&.str,a.svalue)
		strcat(&.str,"""")

	when name_val then
		strcat(&.str,a.svalue)

	when syscall_val then
		strcat(&.str,"XXX")

	when label_val then
		strcat(&.str,"L")
		strcat(&.str,strint(a.labelno))
		goto addoffset

!	else
	esac

	return &.str

end

global proc writegsfile(ichar filename, ref strbuffer d)=
	filehandle f

	f:=fopen(filename,"w")
	gs_println(d,f)
	fclose(f)
end

global function ismemaddr(int n)int=
	if pclstack[n].fmt=imm_memaddr then return 1 fi
	return 0
end

global function isimm64(int n)int=
	if pclstack[n].fmt=imm_d64 then return 1 fi
	return 0
end

global function isregvaropnd(int n)int=
	if pclstack[n].loc=regvar_loc then return 1 fi
	return 0
end

global proc copyblock(mcloperand ax,bx, int n)=
	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg


	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg

	offset:=0

	if 1<=nwords<=4 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,targetsize)
		bx:=changeopndsize(bx,targetsize)

		to nwords do
			genmc(m_mov,rx,applyoffset(bx,offset))
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop
		rcount:=mgenreg(countreg:=getnextreg())	!count
		lab:=++mlabelno

		ax:=makesimpleaddr(ax)
		bx:=makesimpleaddr(bx)

		genmc(m_mov,rcount,mgenint(nwords))
		genmc(m_label,mgenlabel(lab))
		genmc(m_mov,rx,bx)
		genmc(m_mov,ax,rx)

		genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))
		genmc(m_add,mgenreg(bx.reg),mgenint(targetsize))

		genmc(m_dec,rcount)
		genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

		offset:=0
		freereg(countreg)
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx,4)
			genmc(m_mov,rx,applyoffset(bx,offset,4))
			genmc(m_mov,applyoffset(ax,offset,4),rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx,2)
			genmc(m_mov,rx,applyoffset(bx,offset,2))
			genmc(m_mov,applyoffset(ax,offset,2),rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx,1)
			genmc(m_mov,rx,applyoffset(bx,offset,1))
			genmc(m_mov,applyoffset(ax,offset,1),rx)
		fi
	fi

	freereg(workreg)
end

function makesimpleaddr(mcloperand ax)mcloperand bx=
!assume ax is an ireg, but need a simple one with areg set but not ireg
	int newreg

	if ax.reg and not ax.regix then return ax fi
	newreg:=(ax.reg | ax.reg | (ax.regix | ax.regix | getnextreg()))
	bx:=mgenireg(newreg)

	genmc(m_lea, mgenreg(newreg), ax)
	return bx
end

global function ispoweroftwo(int64 x)int=
!when x is a power of two, and is at least 2, then return the power (ie. equiv number of shifts)
!otherwise return zero when x is negative, 0, 1, not a power of two, or more than 2**31
	int64 a
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

global function getrtsproclabel(int fnindex)int=
	if rtsproclabels[fnindex]=0 then
		rtsproclabels[fnindex]:=++mlabelno
		return mlabelno
	fi
	return rtsproclabels[fnindex]
end

global proc genrtsproctable=
	[256]char str
	int proclab
	psymbol d

	setsegment('C',16)

	for i to rtsnames.len when rtsproclabels[i] do
		print @str,"Generating code for",rtsnames[i]

		mgencomment(str)
		genmc(m_label, mgenlabel(rtsproclabels[i]))

		if not rtsproctable[i] then

			case i
			when rts_mul_i128 then
				genrts_mul_i128()
			when rts_div_i128 then
				genrts_div_i128()
			else
				cpl rtsnames[i]
				merror("RTS fn not defined")
!				mgencomment("Code not ready")
			esac
			mgencomment("")
		else
			genmc(m_jmp, mgenmemaddr(rtsproctable[i]))
		fi
	od
end

proc genrts_mul_i128=
	mgencomment("<code for mul-i128>")

!a1=D10
!a2=D11 => D9
!b1=D12
!b2=D13
	genrtsentry()
	genmc(m_push, rd[r6])
	genmc(m_push, rd[r7])
	genmc(m_push, rd[r9])

	genmc(m_mov, rd[r9],rd[r11])

	genmc(m_mov, rd[r0],rd[r10])			!a1
	genmc(m_imul2, rd[r0],rd[r13])			!*b2
	genmc(m_mov, rd[r6],rd[r0])				!=>d6

	genmc(m_mov, rd[r0],rd[r9])				!a2
	genmc(m_imul2, rd[r0],rd[r12])			!*b1
	genmc(m_mov, rd[r7],rd[r0])				!=>d7

	genmc(m_mov, rd[r0],rd[r10])			!a1
	genmc(m_imul, rd[r12])					!*b1; d0=low word of a1*b1; d11=high word
	genmc(m_add, rd[r11],rd[r6])			!+ a1*b2<<64
	genmc(m_add, rd[r11],rd[r7])			!+ a2*b1<<64
	genmc(m_mov, rd[r1],rd[r11])			!

	genmc(m_pop, rd[r9])
	genmc(m_pop, rd[r7])
	genmc(m_pop, rd[r6])


	genrtsexit()
end

proc genrts_div_i128=
	mgencomment("<code for div-i128>")

!a1=D10
!a2=D11 => D3
!b1=D12
!b2=D13

	genrtsentry()
	genmc(m_push, rd[r3])

	genmc(m_mov, rd[r3],rd[r11])

	genmc(m_mov, rd[r0], rd[r3])		!a2
	genmc(m_xorx, rd[r11], rd[r11])
	genmc(m_div, rd[r12])				!a2/b1
	genmc(m_mov, rd[r1], rd[r0])		!=> c2
	genmc(m_mul, rd[r12])				!c2*b
	genmc(m_sub, rd[r3], rd[r0])		!a2-:=c2*b

	genmc(m_mov, rd[r0], rd[r10])
	genmc(m_mov, rd[r11], rd[r3])		!a2:a1
	genmc(m_div, rd[r12])				!/b1

	genmc(m_pop, rd[r3])

	genrtsexit()
end

!proc genrts_div_i128=
!	mgencomment("<code for div-i128>")
!
!!a1=D10
!!a2=D11 => D9
!!b1=D12
!!b2=D13
!
!	genrtsentry()
!	genmc(m_push, rd[r6])
!	genmc(m_push, rd[r9])
!
!	genmc(m_mov, rd[r9],rd[r11])
!
!	genmc(m_mov, rd[r0], rd[r9])		!a2
!	genmc(m_xorx, rd[r11], rd[r11])
!	genmc(m_div, rd[r12])				!a2/b1
!	genmc(m_mov, rd[r6], rd[r0])		!=> c2
!	genmc(m_mul, rd[r12])				!c2*b
!	genmc(m_sub, rd[r9], rd[r0])		!a2-:=c2*b
!
!	genmc(m_mov, rd[r0], rd[r10])
!	genmc(m_mov, rd[r11], rd[r9])		!a2:a1
!	genmc(m_div, rd[r12])				!/b1
!	genmc(m_mov, rd[r1], rd[r6])
!
!	genmc(m_pop, rd[r9])
!	genmc(m_pop, rd[r6])
!
!	genrtsexit()
!end

proc genrtsentry=
	genmc(m_push, dframeopnd)
	genmc(m_mov, dframeopnd, dstackopnd)
	genmc(m_sub, dstackopnd, mgenint(32))
end

proc genrtsexit=
	genmc(m_add, dstackopnd, mgenint(32))
	genmc(m_pop, dframeopnd)
	genmc(m_ret)
end

global proc clearblock(mcloperand ax, int n)=
!ax is the operand with the address of memory to be cleared
!generate code to clear n bytes

	mcloperand rx, rcount
	int nwords,lab,oddbytes,offset,workreg, countreg


	oddbytes:=n rem 8		!will be zero, or 1..7
	n-:=oddbytes			!n will always be a multiple of 8; n can be zero too
	nwords:=n/8				!number of word64s (ie. octobytes)

	rx:=mgenreg(workreg:=getnextreg())		!work reg
	genmc(m_xorx,rx,rx)

	offset:=0

	if 1<=nwords<=8 then		!use unrolled code (no loop)
		ax:=changeopndsize(ax,targetsize)

		to nwords do
			genmc(m_mov,applyoffset(ax,offset),rx)
			offset+:=8
		od

	elsif nwords<>0 then		!use a loop

!SPLIT INTO TWO VERSIONS:
! NWORDS IS A MULTIPLE OF 4, so can write 4 words at a time, in 1/4 of iterations
! Or do one word at a time like now.
! nword is a multiple of 4 happens when N is a multiple of 32 bytes, which will
! always be the case for power-of-two sizes of 32 bytes or more. 32/64 may already
! be done without a loop. So non-part-unrolled version only really for odd array or
! struct sizes, such as [100]char.

		if nwords iand 3 then		!not 4n

			rcount:=mgenreg(countreg:=getnextreg())	!count
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)

			genmc(m_mov,rcount,mgenint(nwords))
			genmc(m_label,mgenlabel(lab))
			genmc(m_mov,ax,rx)

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
			freereg(countreg)
		else
			rcount:=mgenreg(countreg:=getnextreg())	!count
			lab:=++mlabelno

			ax:=makesimpleaddr(ax)
			genmc(m_mov,rcount,mgenint(nwords/4))
			genmc(m_label,mgenlabel(lab))

			for i to 4 do
				genmc(m_mov,applyoffset(ax,offset),rx)
				offset+:=8
			od

			genmc(m_add,mgenreg(ax.reg),mgenint(targetsize*4))

			genmc(m_dec,rcount)
			genmc_cond(m_jmpcc,ne_cond,mgenlabel(lab))

			offset:=0
			freereg(countreg)
		fi
	fi

	if oddbytes then
		n:=oddbytes						!1..7

		if n>=4 then
			rx:=changeopndsize(rx,4)
			genmc(m_mov,applyoffset(ax,offset,4),rx)
			n-:=4
			offset+:=4
		fi
		if n>=2 then
			rx:=changeopndsize(rx,2)
			genmc(m_mov,applyoffset(ax,offset,2),rx)
			n-:=2
			offset+:=2
		fi
		if n=1 then
			rx:=changeopndsize(rx,1)
			genmc(m_mov,applyoffset(ax,offset,1),rx)
		fi
	fi

	freereg(workreg)
end

global proc genfunctiontable=
	[256]char str
	ichar s,t
	pcl currpcl
	int firststringlab,nextlab,nprocs

	if lab_funcaddrtable=0 then return fi
	mgencomment("Function Table")
	nprocs:=0

	setsegment('C',16)
	genmc(m_label, mgenlabel(lab_funcaddrtable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			genmc(m_dq,mgenmemaddr(currpcl.def))
			++nprocs
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	firststringlab:=0

	genmc(m_label, mgenlabel(lab_funcnametable))
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			if firststringlab=0 then
				firststringlab:=nextlab:=++mlabelno
			else
				nextlab:=++mlabelno
			fi

			genmc(m_dq,mgenlabel(nextlab))
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	nextlab:=firststringlab
	currpcl:=pcstart
	repeat
!		if currpcl.opcode=kprocdef then
		if currpcl.opcode in [kprocdef,kthreadedproc] then
			genmc(m_label,mgenlabel(nextlab))
			s:=currpcl.def.name
			t:=s

			while s^ do
				if s^='.' then
					t:=s+1
				fi
				++s
			od
			genstring(t,1)
			++nextlab
		fi
		++currpcl
	until currpcl.opcode=kendprogram

	genmc(m_label, mgenlabel(lab_funcnprocs))
	genmc(m_dq, mgenint(nprocs))
end

global function mgenextname(ichar s)mcloperand=
	[64]char str
	psymbol d

	strcpy(&.str,s)
	str[strlen(s)]:=0

	d:=pcm_allocz(pstrec.bytes)
!	
	d.name:=pcm_copyheapstring(&.str)
	d.isimported:=1

	return mgenmemaddr(d)
end

export proc pcl_setasmhandler(ref void fnaddr)=
	hostasmhandler:=fnaddr
end

global proc mgeninfo(ichar s, int value)=
	[256]char str
	fprint @&.str,"# #",s,value
	genmc_str(m_comment,&.str)
end

global proc mgeninfos(ichar s, svalue)=
	[256]char str
	fprint @&.str,"# #",s,svalue
	genmc_str(m_comment,&.str)
end

=== pc_libpcl.m 0 0 23/44 ===
global pcl pcstart			!start of pcl block
export pcl pccurr			!point to current pcl op
global pcl pcend			!point to last allocated pclrec
global int pcalloc			!number of pclrecs allocated
byte pcfixed				!whether code is fixed up
int pcseqno
int pcneedfntable			!whether kgetnprocs etc are used

!const fshowsymbols=1
const fshowsymbols=0

int initpcalloc=65536
!int initpcalloc=65536*4
!int initpcalloc=4*1048576
!int initpcalloc=16*1048576

const pcelemsize = pclrec.bytes

strbuffer sbuffer
ref strbuffer dest=&sbuffer
int destlinestart

!global const maxlabels=10000			!in one file
!global const maxlabels=100000			!in one file
global const maxlabels=800000			!in one file
export [maxlabels]int32 labelmap				!map user labels to global labels
export int plabelno					!current highest global labelno
global int maxuserlabel					!set by lex: highest #label
global int labelnooffset				!normally 0; set to offset (maxlabelno) when processing rts
GLOBAL INT NPCL

!const maxgloballabels=50000				!in all files
!const maxgloballabels=100000				!in all files
const maxgloballabels=800000				!in all files
[maxgloballabels]pcl labeloffset		!pcl addr of label

global ichar longstring					!used in stropnd
global int longstringlen
global ichar errormess

global int mcldone

export proc pcl_start(int nunits=0)=
!returns a descriptor to the global tables
!at the moment little is done with the descriptor, except to have something
!tangible to pass back to the caller of the API. There is no mechanism
!to allow multiple, active sets of pcltables

	pcalloc:=initpcalloc

	if nunits then				!use approx alloc of 10% more
		nunits:=nunits*9/8		!approx expected number of pcl ops
		while pcalloc<nunits do
			pcalloc*:=2
		od
	fi

!CPL =PCALLOC,=NUNITS

	pcstart:=pcm_allocz(pcalloc*pcelemsize)
	pcend:=pcstart+pcalloc-8

	pccurr:=pcstart-1
	pcfixed:=0
	pcseqno:=0
	pcneedfntable:=0

	plabelno:=maxuserlabel:=labelnooffset:=0
	mcldone:=0

	clear rtsproctable

	pstentrypoint:=nil

!CLEAR USED PORTIONS OF LABELMAP/LABELOFFSET FOR NEXT FILE
!...
end

export proc pcl_end(int fixup=0)=

	if pccurr>=pccurr and pccurr.opcode<>kendprogram then
		pcl_gen(kendprogram)
	fi	

	if fixup then
		fixuppcl()
	fi
	
!destroy assets associated with pcldescr, which should be the values
!currently in pcstart etc
!note that the values in pcldescr will likely be out of date
end

export proc pcl_free(int fixup)=
!destroy resources used for pcl data

	pcstart:=pccurr:=pcend:=nil
	pcfixed:=0
end

global proc fixuppcl=
	psymbol d
	pcl p, pextproc
	int labno, insideproc, extparams,isthreaded

	return when pcfixed

!CPL "FIXUPPCL"

	p:=pcstart
	insideproc:=0
	pextproc:=nil

	while p<=pcend, ++p do
		switch p.opcode
		when klocal, kparam then
			if insideproc<>1 then perror("Not allowed") fi
			d:=p.def
			d.pclop:=p.opcode
			d.pcldef:=p
			if d.isdefined then perror_s("Redefining local/param/ext:",d.name) fi
			d.isdefined:=1

		when kprocdef then
			isthreaded:=0
doprocdef::
			if insideproc then perror("Nested proc") fi
			insideproc:=1
			d:=p.def
			d.pclop:=p.opcode
			d.pcldef:=p
			if d.isdefined then perror_s("Redefining proc:",d.name) fi
			d.isdefined:=1
			d.isthreaded:=isthreaded
			assignlabeltoname(d)
		when kistatic, kzstatic then
			d:=p.def
			d.pclop:=p.opcode
			d.pcldef:=p
			if d.isdefined then perror_s("Redefining proc/static/label:",d.name) fi
			d.isdefined:=1
			assignlabeltoname(d)

		when kendproc then
			if insideproc<>1 then perror("End without proc") fi
			insideproc:=0

		when kendextproc then
			if insideproc<>2 then perror("End without extproc") fi
			insideproc:=0

		when kextproc then
			pextproc:=p
			extparams:=0
			if insideproc then perror("Nested proc") fi
			insideproc:=2
doextproc::
			d:=p.def
			if not d.isdefined then
				d.pclop:=p.opcode
				d.pcldef:=p
				if d.isdefined then perror_s("Redefining extproc:",d.name) fi
				d.isdefined:=1
				d.isimported:=1
			fi

		when kextparam then
			if insideproc<>2 then perror("Not allowed") fi
			++extparams

		when kextvariadics then
			if insideproc<>2 then perror("Not allowed") fi
			p.extvariadics:=extparams

		when kextvar then
			goto doextproc

		when kthreadedproc then
			isthreaded:=1
			doprocdef

		when klabelname then
			labno:=p.labelno
			if labno not in 1..maxlabels then
				perror_s("3:Label out of range",strint(labno))
			fi
			p.labelno:=fixlabel(labno)
			p.opndtype:=label_opnd

		when kdb then p.opcode:=kdata; p.mode:=tpu8;  p.size:=1
		when kdw then p.opcode:=kdata; p.mode:=tpu16; p.size:=2
		when kdd then p.opcode:=kdata; p.mode:=tpu32; p.size:=4
		when kdq then p.opcode:=kdata; p.mode:=tpu64; p.size:=8
		when kdata then if p.size=0 then p.size:=psize[p.mode] fi

		when kimportdll, kimportlib then
!CPL "IMPORTDLL",P.DEF
			goto doextproc

		elsecase p.opndtype
		when real_opnd then
			if p.mode=tpr32 or p.size=4 then
				p.opndtype:=real32_opnd
				p.xvalue32:=p.xvalue
			fi
		when label_opnd then
			p.labelno:=fixlabel(p.labelno)
		when kgetnprocs, kgetprocname, kgetprocaddr then
			pcneedfntable:=1

		end
	od

	if insideproc then perror("End missing") fi

	pcfixed:=1

	int undef:=0

!CPL "ALL SYMBOLS"
	d:=pallsymbols
	while d, d:=d.nextsym do

		if d.pcldef=nil then
			++undef
			println "Undefined:", d.name,REF VOID(D.NAME)
!
!		fprintln "# def:# imp:# exp:# sym:# pcldef:#",
!			D.NAME:"20JL",d.isdefined, d.isimported,d.isexported,
!			D.symbol, d.pcldef
		fi
	od


	if undef then
		perror_s("Undefined names:",strint(undef))
	fi
end

proc extendpclblock=
	var int newpcalloc, lengthused
	var pcl newpcstart

	newpcalloc:=pcalloc*2
	lengthused:=pccurr-pcstart+1

	newpcstart:=pcm_alloc(pcelemsize*newpcalloc)

CPL "EXTEND PCL",NEWPCALLOC

	memcpy(newpcstart,pcstart, lengthused*pcelemsize)
	pcm_clearmem(newpcstart+lengthused,(newpcalloc-lengthused)*pcelemsize)

	pccurr:=newpcstart+(pccurr-pcstart)
	pcend:=newpcstart+newpcalloc-8

	pcm_free(pcstart,pcalloc*pcelemsize)

	pcstart:=newpcstart
	pcalloc:=newpcalloc
end

global function newpcl:pcl =
	if pccurr>=pcend then
		extendpclblock()
	fi

++NPCL


	++pccurr
	pccurr.seqno:=++pcseqno
	return pccurr
end

export proc pcl_gen(int opcode, pcl p=nil) =
	psymbol d

	if p=nil then
		p:=newpcl()
	fi

!	case opcode
!	when KLABELNAME THEN PERROR("PCLGEN/LABELNAME")
!	esac

	p.opcode:=opcode
end

export proc pcl_gent(int opcode, t, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	if t<0 then
		p.mode:=tpblock
		p.size:=-t
	else
		p.mode:=t
	fi
end

export proc pcl_genx(int opcode, int x, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
end

export proc pcl_genxy(int opcode, int x,y, pcl p=nil) =

	if p=nil then
		p:=newpcl()
	fi

	p.opcode:=opcode
	p.x:=x
	p.y:=y
end

export function pcl_genint(int a,mode=tpi64)pcl p=
	p:=newpcl()
	p.value:=a
	p.opndtype:=int_opnd
!	p.mode:=mode
	return p
end

export function pcl_genint128(int128 a,int mode=tpi128)pcl p=
	p:=newpcl()
	p.value128:=a
	p.opndtype:=int128_opnd
	return p
end

export function pcl_genreal(real x)pcl p=
	p:=newpcl()
	p.xvalue:=x
	p.opndtype:=real_opnd
	return p
end

export function pcl_genreal32(real x)pcl p=
	p:=newpcl()
	p.xvalue32:=x
	p.opndtype:=real32_opnd
	return p
end

export function pcl_genstring(ichar s)pcl p=
	p:=newpcl()
	p.svalue:=pcm_copyheapstring(s)
	p.opndtype:=string_opnd
	return p
end

export function pcl_genlabel(int a)pcl p=
	p:=newpcl()
	p.labelno:=a
	p.opndtype:=label_opnd
	return p
end

export function pcl_genmem(psymbol d)pcl p=
	p:=newpcl()
	p.def:=d
	p.opndtype:=mem_opnd
	return p
end

export function pcl_genmemaddr(psymbol d)pcl p=
	p:=newpcl()
	p.def:=d
	p.opndtype:=memaddr_opnd
	return p
end

export proc pcl_gencomment(ichar s)=
	pcl_gen(kcomment,pcl_genstring(s))
end

export function pcl_genname(ichar s)pcl=
	return pcl_genmem(pcl_makesymbol(s))
end

export function pcl_gennameaddr(ichar s)pcl=
	return pcl_genmemaddr(pcl_makesymbol(s))
end

export function pcl_genassem(ref void code)pcl p=
	p:=newpcl()
	p.asmcode:=code
	p.opndtype:=assem_opnd
	return p
end

export function pcl_makesymbol(ichar s)psymbol d =
!CPL "MAKESYM",S
	d:=addnamestr(s)
	return d
end

global proc strpcl(pcl p)=
	[256]char pmodestr
	[256]char str
	int opcode,defused

	const showformatted=1

	opcode:=p.opcode

!CPL PCLNAMES[OPCODE]




	case opcode
	when klabel then
!CPL "PCL/LABEL", P.DEF.NAME,P.ISEXPORTED
		strlabel(p.labelno,1)
		return
	when kcomment then
		if p.svalue^ then
			psstr("!")
			psstr(p.svalue)
		fi
		return
	when kprocdef, kthreadedproc then
		if p.def.isrts then
			psstr("Procrts")
		elsif opcode=kthreadedproc then
			psstr("Threadedproc")
		else
			psstr("Proc")
		fi
		psstr(" ")
		psname(p.def)
		psstr((p.isexported|"::"|":"))
		if p.mode then
			psstr(" ")
			psstr(strpmode(p.mode,p.size))
		fi
		return

	when kendproc then
		psstr("End")
		psline()
		return

	when kendextproc then
		psstr("Endext")
		psline()
		return

	when kextproc then
		psstr("Extproc")
		psstr(" ")
		psname(p.def)
		if p.mode then
			psstr(" ")
			psstr(strpmode(p.mode,p.size))
		fi
		return


	when klabelname then
		psname(p.def)
		psstr((p.def.isexported|"::"|":"))
		return

	when kendprogram then
		psstr("Endprogram")
		return

	esac

	psstr("    ")
	strcpy(str,pclnames[opcode]+1)
	gs_leftstr(dest,str,15)

	if p.opndtype<>no_opnd then
		psstr(stropnd(p))
		psstr(" ")
!	else
!		pstabto(
	fi
	pstabto(30)

	if p.mode<>tpvoid then
		psstr(strpmode(p.mode, p.size))
		psstr(" ")
	fi

	if pclhastype[opcode]=2 then
		if p.mode=tpvoid then
			psstr("void")
		fi
		psstr(strpmode(p.oldmode,p.size))
		psstr(" ")
	fi

	if pclextra[opcode] then
		psint(p.x)
		if pclextra[opcode]=2 then
			psstr(" ")
			psint(p.y)
		fi
	fi

!	if p.isglobal then psstr(" Isglobal") fi
!	if p.isvariadic then psstr(" Isvariadic") fi
end

global function stropnd(pcl p)ichar=
	static[512]char str
	int length
	psymbol d

	if p=nil then
		return ""
	fi

	case p.opndtype
	when int_opnd then
		return strint(p.value)
	when real_opnd then
		print @str,p.xvalue:"e16.16"

	when real32_opnd then
		print @str,p.xvalue32:"e16.16"

	when string_opnd then
		if (length:=strlen(p.svalue))<str.len/2 then
			strcpy(str,"""")
			convertstring(p.svalue,&.str+1)
			strcat(str,"""")
		else
			if longstring then
				pcm_free(longstring,longstringlen)
			fi
			longstringlen:=length*2
			longstring:=pcm_alloc(longstringlen)
			longstring^:='"'
			length:=convertstring(p.svalue, longstring+1)
			(longstring+length+1)^:='"'
			(longstring+length+2)^:=0
			return longstring
		fi

	when mem_opnd then
		d:=p.def
		print @str,(d.istruename|"`"|""),,d.name
		if p.opcode in [kistatic, kzstatic] then
			strcat(str,":")
			if p.isexported then
				strcat(str,":")
			fi
		fi

	when memaddr_opnd then
		d:=p.def
		fprint @str,"&##",(d.istruename|"`"|""),d.name

	when label_opnd then
		fprint @str,"## ","#",p.labelno

	when no_opnd then
		return ""
	when int128_opnd then
		print @str,p.value128

	when assem_opnd then
		return strint(int(p.asmcode))

	else
		println OPNDNAMES[P.OPNDTYPE]
		return "<PCLOPND?>"
	esac

	return str
end

proc psstr(ichar s)=
	gs_str(dest,s)
end

proc psline=
	gs_line(dest)
end

proc psint(int a)=
	gs_str(dest,strint(a))
end

proc psname(psymbol d)=
	if d.istruename then
		gs_str(dest,"`")
	fi
	gs_str(dest,d.name)
end

proc pstabto(int n)=
	int col:=dest.length-destlinestart
	while n>col do psstr(" "); ++col od
end

global function convertstring(ichar s, t)int=
!convert string s, that can contain control characters, into escaped form
!return new string in t, so that ABC"DEF is returned as ABC\"DEF
!returns length of t
	int c
	ichar t0:=t

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

	return t-t0
end

proc strlabel(int labelno,colon=0)=
	psstr("#")
	psint(labelno)
	if colon then
		psstr(":")
	fi
	psstr(" ")
end

proc psopnd(pcl p)=
	psstr(stropnd(p))
end

global function strpmode(int m, size)ichar=
	static [64]char str
	if m<>tpblock then
		strcpy(str, pstdnames[m])
	else
		fprint @str,"#:#", pstdnames[m],size
	fi
	return str
end

global proc writepcl(pcl p)=
	strpcl(p)
	gs_line(dest)
end

global function writeallpcl:ichar,int=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	psymbol d,e

	gs_init(dest)
	destlinestart:=dest.length

	p:=pcstart

	while p<=pccurr do
		writepcl(p)

		destlinestart:=dest.length
		++p
	od

	if fshowsymbols then
		writesymbols()
	fi

	if longstring then
		pcm_free(longstring,longstringlen)
	fi
	return (dest.strptr,dest.length)
end

global function nextlabel:int=
	if plabelno>=maxgloballabels then
		pclerror("Too many global labels")
	fi
	++plabelno
end

global function fixlabel(int userlab)int=
	if userlab not in 1..maxlabels then
		perror_s("Label no out of range:",strint(userlab))
	fi
	if labelmap[userlab]=0 then
		labelmap[userlab]:=nextlabel()
		return plabelno
	else
		return labelmap[userlab]
	fi
end

global proc pdefinelabel(int label,seqno=0)=
!define new global label to point just after pccurr
!label must be in range
	if labeloffset[label] then
		pclerror("Redefining label:",strint(label))
	fi
	pcl_gen(klabel, pcl_genlabel(label))
	pccurr.seqno:=seqno

	labeloffset[label]:=pccurr
end

global proc assignlabeltoname(psymbol d)=
!isdef=1 means can't already be assigned
	if d.labelno then
!		if isdef then
!			pclerror("Duplicate name def:",d.name,lineno)
!		fi
		return
	fi
	d.labelno:=nextlabel()
end

global proc pclerror(ichar mess,param=nil,int lineno=0)=
	print "PCC error:",mess
	if param then
		print param
	fi
	if lineno then
		print " on line:",lineno
	fi
	println
	stop 1
end

global function getpclstr(pcl p)ichar=
	gs_init(dest)
	destlinestart:=dest.length

	strpcl(p)
	(dest.strptr+dest.length)^:=0
	return dest.strptr
end

export function pcl_getopcode:int=
	return pccurr.opcode
end

export proc pcl_setopcode(int opc)=
	pccurr.opcode:=opc
end

export proc pcl_settype(int t,size=0)=
	pccurr.mode:=t
	pccurr.size:=size
	if t<0 then
		pccurr.mode:=tpblock
		pccurr.size:=-t
	fi
end

export proc pcl_setxy(int x,y)=
	pccurr.x:=x
	pccurr.y:=y
end

export proc pcl_setscale(int scale)=
	pccurr.scale:=scale
end

export proc pcl_setoffset(int offset)=
	pccurr.extra:=offset
end

export proc pcl_addoffset(int offset)=
	pccurr.extra+:=offset
end

export proc pcl_setincr(int n)=
	pccurr.stepx:=n
end

export proc pcl_setnargs(int n)=
	pccurr.nargs:=n
end

export proc pcl_setnmult(int n)=
	abortprogram("SETNMULT")
end

export proc pcl_setrettypes(ref[]int types, int n)=
	abortprogram("SETRETTYPES")
end

export proc pcl_setexported(int x)=
	psymbol def:=pccurr.def

	pccurr.isexported:=x
	if def then
		def.isexported:=x

!CPL "SETEXPORTED:",DEF.NAME,X,=DEF=PSTENTRYPOINT
!		if def=pstentrypoint then
!			def.name:=pcm_copyheapstring(getbasename(def. 

	fi
end

export proc pcl_isthreaded(int x)=
!	pccurr.isthreasexported:=1
	if pccurr.def then
		pccurr.def.isthreaded:=x
	fi
end

export proc pcl_setnvariadics(int n)=
	pccurr.nvariadics:=n
end

export proc pcl_setalign(int n)=
	pccurr.align:=n
end

export proc pcl_setrtsproc=
	if pccurr.def and pccurr.opcode=kprocdef then
		definertsproc(pccurr.def)
	fi
end

export proc pcl_setoldtype(int t)=
	pccurr.oldmode:=t
end

export proc pcl_setpos(int pos)=
	abortprogram("SETPOS")
end

export function pcl_lasterror:ichar=
	return errormess
end

export function pcl_writepclfile(ichar filename)int=
	ichar source
	int length

	fixuppcl()

	(source,length):=writeallpcl()

	return writefile(filename,source,length)
end

export proc pcl_addlibfile(ichar filename, int libtype)=
	ichar source
	int length

!CPL "PCL ADDLIBFILE",FILENAME,LIBTYPE:"C"

	for i to nplibfiles do
		if eqstring(filename,plibfiles[i]) then		!already present
			return
		fi
	od

	if nplibfiles>maxplibfile then
		perror("PCL:Too many libs")
	fi
	plibfiles[++nplibfiles]:=filename
	plibtypes[nplibfiles]:=libtype
end

export proc pcl_setentrypoint(psymbol d, int libmode=0)=
	pstentrypoint:=d
	plibmode:=libmode
end

export proc pcl_addsym(psymbol d)=
	if pstready then

!IF D.NAME=NIL THEN
!CPL "ADDSYM NULL NAME:",D, D.SYMBOL
!!ELSE
!!CPL "ADDSYM",D.NAME
!FI

		d.nextsym:=pallsymbols
		pallsymbols:=d
	fi
end

global proc perror(ichar mess)=
	perror_s(mess, nil)
end

global proc perror_s(ichar mess, param=nil)=
	print "PCL error:",mess
	if param then
		print ":",param
	fi

	stop 1
end

global proc writesymbols=
!write all pcl code in system by scanning all procs
!pcl code is only stored per-proc
	pcl p
	psymbol d,e

	p:=pcstart

	psline()
	psstr("PROC PCL DEF OPS")
	psline()

	while p<=pccurr, ++p do
		case p.opcode
		when kprocdef, kistatic, kzstatic, kextproc, klabelname,
			klocal, kparam then

			d:=p.def
PSINT(INT(p))
PSSTR(" ")
PSINT(INT(d))
PSSTR(" ")

			psstr(d.name)
			psstr(": ")
			psint(p.seqno)
			psline()

			psstr("	Opcode:"); psstr(pclnames[p.opcode]); psline()
			psstr("	PCLdef:"); psstr((d.pcldef|pclnames[d.pcldef.opcode]|"---")); psline()
			psstr("	Isdefined:"); psint(d.isdefined); psline()
			psstr("	Isexported:"); psint(d.isexported); psline()
			psstr("	Isimported:"); psint(d.isimported); psline()
			psstr("	Extvariadics:"); psint(p.extvariadics); psline()
			psstr("	Isaddrof:"); psint(d.addrof); psline()
			psstr("	Label#:"); psint(d.labelno); psline()

		esac
	od

	p:=pcstart

	psline()
	psstr("PROC PCL UNDEFINED MEM REFS")
	psline()

!CPL =PCLCODE, =PCLCURR

	while p<=pccurr, ++p do
		if p.opndtype in [mem_opnd, memaddr_opnd] and not p.def.isdefined then
			d:=p.def
PSINT(INT(p))
PSSTR(" ")
PSINT(INT(d))
PSSTR(" ")
			psstr("Not defined: ")
			psstr(d.name)
			psstr(" ")
			psint(p.seqno)
			psline()
			d.isdefined:=1
		fi
	od
end

global function getbasename(ichar s)ichar t=
	t:=s+strlen(s)-1
	while t>s and (t-1)^<>'.' do
		--t
	od

	return t
end

global proc definertsproc(psymbol d)=
!request made that d is an RTS proc
	ichar s:=d.name
	int c

!look for rts name following $; just scan LTR for first $
	while (c:=s++^)<>'$' do od

	for i to rtsnames.len do
		if eqstring(rtsnames[i]+4, s) then
			d.isrts:=1
			d.rtsindex:=i
			rtsproctable[i]:=d
			return
		fi
	else
	od
end

global function roundtoblock(int n,align)int=
!round up n until it is a multiple of filealign (which is a power of two)
!return aligned value. Returns original if already aligned
	if n iand (align-1)=0 then return n fi
	return n+(align-(n iand (align-1)))
end

=== pci_mcl.m 0 0 24/44 ===
export type mcloperand = ref mclopndrec

export record mclopndrec =		!up to 32 bytes
!	ref pstrec labeldef	!nil, or handle of strec for label
	union
		psymbol def
		int64 value		!immediate value
		real64 xvalue	!immediate real value, mainly for dq
		ichar svalue	!immediate string
		int labelno
		int sysfn
		int tempno
	end

	byte size			!byte size of operand: usually 1,2,4,8,16
	byte mode			!a_reg etc, low level operand details
	byte reg			!0, or main register
	byte regix			!0, or index register

	byte valtype		!interpretation of def/code/value/svalue
	byte scale			!1, or scale factor for regix
!	int16 offset		!extra offset to label for mem/imm modes
	int32 offset		!extra offset to label for mem/imm modes

	byte addrsize		!4 or 8 for a_mem when regs are involved
!	byte valtypex		!valtypex is 0 (no value or int) or 'R'/'S' in ma

end

export record mclrec =		!32 bytes
	ref mclrec nextmcl
	mcloperand a,b
	byte opcode
	union
		byte cond
		byte isglobal
		byte sysindex
	end
!	byte fileno
	byte c
!	byte spare1, spare2
	int pos:(lineno:24, fileno:8)
	ichar comment
	[r0..r15]byte regend		!1 indicates register freed.
end

global tabledata() [0:]ichar valtypenames =
	(no_val=0,		$),		!no operand
	(intimm_val,	$),		!immediate int
	(realimm_val,	$),		!immediate real (mainly for dq etc)
	(realmem_val,	$),		!indirect real (for movq etc)
	(stringimm_val,	$),		!immediate string, for comments, or address of string etc
	(def_val,		$),		!var/proc name
	(temp_val,		$),		!temporary
	(label_val,		$),		!label index
!	(labelind_val,	$),		!label index
	(name_val,		$),		!immediate string must be output as ah unquoted name
	(syscall_val,	$),		!
end

export tabledata() []ichar mclnames, []byte mclnopnds, []byte mclcodes =

	(m_procstart,		$,		0,		0),		!
	(m_procend,			$,		0,		0),		!
	(m_comment,			$,		0,		0),		!
	(m_blank,			$,		0,		0),		!
	(m_deleted,			$,		0,		0),		!
	(m_labelname,		$,		0,		0),		!
	(m_define,			$,		0,		0),		!
	(m_definereg,		$,		0,		0),		!

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
	(m_leave,			$,		0,		0xC9),	!
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

	(m_cpuid,			$,		0,		0),		!

	(m_halt,			$,		0,		0xF4),	!
end

export tabledata() [0:]ichar regnames, [0:]byte regcodes =
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

export const rframe = r14
export const rstack = r15

global tabledata() [0:]ichar condnames, [0:]ichar asmcondnames,
		[0:]int asmrevcond =

	(ov_cond=0,		"ov",	"o",		nov_cond),
	(nov_cond=1,	"nov",	"no",		ov_cond),

	(ltu_cond=2,	"ltu",	"b",		geu_cond),
	(geu_cond=3,	"geu",	"ae",		ltu_cond),

	(eq_cond=4,		"eq",	"z",		ne_cond),
	(ne_cond=5,		"ne",	"nz",		eq_cond),

	(leu_cond=6,	"leu",	"be",		gtu_cond),
	(gtu_cond=7,	"gtu",	"a",		leu_cond),

	(s_cond=8,		"s",	"s",		ns_cond),
	(ns_cond=9,		"ns",	"ns",		s_cond),

	(p_cond=10,		"p",	"p",		np_cond),
	(np_cond=11,	"np",	"np",		p_cond),

	(lt_cond=12,	"lt",	"l",		ge_cond),
	(ge_cond=13,	"ge",	"ge",		lt_cond),

	(le_cond=14,	"le",	"le",		gt_cond),
	(gt_cond=15,	"gt",	"g",		le_cond),

	(flt_cond=16,	"flt",	"b",		fge_cond),		!special floating point codes
	(fge_cond=17,	"fge",	"ae",		flt_cond),
	(fle_cond=18,	"fle",	"be",		fgt_cond),
	(fgt_cond=19,	"fgt",	"a",		fle_cond)
end

global const z_cond = eq_cond
global const nz_cond = ne_cond

!I use my own register designations Dn, An, Wn, Bn (8,4,2,1 bytes),
!which have a more sensible order than the official ones.
!The mapping is shown against Dn. Some (not all) of the official register
!names are used too

!Regindex is the ordinal value used to represent the register: 1..16
!This table is intended for initialising the global symbol table

export tabledata []ichar dregnames, []byte regsizes, []byte regindices =
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

export []ichar xmmregnames = (
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

export []ichar fregnames = (
	"st0",
	"st1",
	"st2",
	"st3",
	"st4",
	"st5",
	"st6",
	"st7")

export []ichar mregnames = (
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

export tabledata []ichar jmpccnames, []byte jmpcccodes =
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


export tabledata []ichar setccnames, []byte setcccodes =
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

export tabledata []ichar cmovccnames, []byte cmovcccodes =
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

!export tabledata() [0:]ichar segmentnames =
!	(no_seg=0,		$),
!	(code_seg,		$),
!	(idata_seg,		$),
!	(zdata_seg,		$),
!	(rodata_seg,	$),
!	(impdata_seg,	$),
!end

export tabledata() [0:]ichar reftypenames =	!use during pass2
	(extern_ref=0,		$),		!is external
	(fwd_ref,			$),		!not yet reached
	(back_ref,			$),		!has been reached
end

export tabledata() [0:]ichar opndnames_ma =
	(a_none=0,	$),
	(a_reg,		$),		! Ri
	(a_imm,		$),		! d including def name, label etc
	(a_mem,		$),		! any memory modes: [d], [R], [R*4+R2+d+imm] etc
	(a_cond,	$),		! a condition code for jcc/setcc
	(a_xreg,	$),		! xmm register
	(a_wreg,	$),		! Wide integer register, means Ri and Ri+1
!	(a_regvar,	$),		! Reg holding a regvar
!	(a_xregvar,	$),		! Reg holding a regvar
end

global int mlabelno
!global byte foptimise

global const maxoperands=200

global [maxoperands+10]pclstackrec pclopndstack
global int noperands
global int mstackdepth

global record pclstackrec =
	byte fmt
	byte loc
	byte reg
	byte float
!	byte high			!1 when high word of wide
!	byte low			!1 when low word of wide
	byte wide			!0 or 'H' or 'L'
	int16 offset
	union
		int value
		real xvalue
		ichar svalue
		psymbol def
		int labno
	end
end

global ref[]pclstackrec pclstack
global pclstackrec pclstackzero

!Where any active operand is located:

global tabledata() [0:]ichar locnames =
	(no_loc=0,		$),			! not set
	(reg_loc,		$),			! in a d64 register
	(xreg_loc,		$),			! in an x64
	(stack_loc,		$),			! on the hardware stack (must be ordered properly)
	(imm_loc,		$),			! still as an immediate value
	(mem_loc,		$),			! still in variable
	(memhigh_loc,	$),			! still in variable
	(regvar_loc,	$),			! still in a reg variable
	(xregvar_loc,	$),			! still in an xreg variable
end

global tabledata() [0:]ichar fmtnames, [0:]byte loccodes, [0:]byte floatloc,
		[0:]byte loadfmt, [0:]byte pushfmt =
	(nofmt_void=0,	$,	0,				0,	0,			0),
	(reg_d64,		$,	reg_loc,		0,	0,			stack_d64),

	(xreg_x64,		$,	xreg_loc,		1,	0,			stack_x64),
	(xreg_x32,		$,	xreg_loc,		1,	0,			stack_x32),

	(stack_d64,		$,	stack_loc,		0,	reg_d64,	0),
	(stack_x64,		$,	stack_loc,		1,	xreg_x64,	0),
	(stack_x32,		$,	stack_loc,		1,	xreg_x32,	0),

	(imm_d64,		$,	imm_loc,		0,	reg_d64,	stack_d64),
	(imm_x64,		$,	imm_loc,		1,	xreg_x64,	stack_x64),
	(imm_x32,		$,	imm_loc,		1,	xreg_x32,	stack_x32),
	(imm_str,		$,	imm_loc,		0,	reg_d64,	stack_d64),
	(imm_memaddr,	$,	imm_loc,		0,	reg_d64,	stack_d64),

	(imm_label,		$,	imm_loc,		0,	reg_d64,	stack_d64),
	(imm_labaddr,	$,	imm_loc,		0,	reg_d64,	stack_d64),

	(mem_d64,		$,	mem_loc,		0,	reg_d64,	stack_d64),
	(mem_x64,		$,	mem_loc,		1,	xreg_x64,	stack_x64),
	(mem_x32,		$,	mem_loc,		1,	xreg_x32,	stack_x32),

	(memhigh_d64,	$,	mem_loc,		0,	reg_d64,	stack_d64),

	(regvar_d64,	$,	regvar_loc,		0,	reg_d64,	stack_d64),
	(xregvar_x64,	$,	xregvar_loc,	1,	xreg_x64,	stack_x64),
end

global const regmax=r9				!can use r0 to regmax inclusive; only those regs
global const xregmax=xr6

global [r0..r15]byte regset			!register in-use flags: 0/1: free/in-use
global [r0..r15]byte xregset		!same for xregs

global [r0..r15]byte isregvar
global [r0..r15]byte isxregvar

!These vars give info on the resources used by a proc

!global [r0..r15]byte allregmap		!all regs used
!global [r0..r15]byte allxregmap		!all xregs used

global int inf_proccalls
global int inf_proclocals
global int inf_procxlocals

global int inf_leafproc
global int inf_highreg
global int inf_highxreg
global int inf_maxargs
export int inf_assem

global int inf_r10used		!these may be set in pass2 when occupied by params
global int inf_r11used
global int inf_r13used

!global [16]int inf_dsaveregs
!global [16]int inf_xsaveregs
!global int inf_ndsaveregs	!set in procentry; at one or both will be zero
!global int inf_ndsavepush
!global int inf_nxsaveregs
!global int inf_dsaveoffset
!global int inf_xsaveoffset

global [16]int dsaveregs
global [16]int xsaveregs
global int ndsaveregs	!set in procentry; at one or both will be zero
global int ndsavepush
global int nxsaveregs
global int dsaveoffset
global int xsaveoffset
global int needstackframe
global int framebytes
global int needshadow48
global int needshadow32		!has value 0, 32 or 40, the actual spaced needed

global byte noxorclear		!1 to suppress xor optimisation

global const wd = 4
global const xc = 3
global const yb = 2
global const za = 1

global const xb = 2
global const ya = 1

global const xa = 1

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

global pcl procdefpcl
global psymbol procdef

global const maxcalldepth=16
global [maxcalldepth]int callshadow
global [maxcalldepth]int callslots
global [maxcalldepth]byte callalign		!pending 1-slot alignment for syscalls
global [maxcalldepth]byte callargs
global [maxcalldepth]byte callnvars
global int ncalldepth

global const maxparams=32
global const maxlocals=256

!these are reset at each procdef
global [maxparams]psymbol paramdefs
global [maxlocals]psymbol localdefs
global int nparams, nlocals
global int retmode
!global ref strec procdef
global int passno
global int sa_nargs

global []int multregs=(r0,r1,r2,r10,r11,r12)
global []int multxregs=(r0,r1,r2,r3,r4,r5)

!global int frameoffset,paramoffset
global int paramoffset

global int lababs32, lababs64
global int labneg32, labneg64
global int labmask63, laboffset64
global int labzero
global int kk0used=0

!global int retindex
global int stackaligned
global const initial_stackalignment = 1

global const rtos=rnone			!means stack operand

!global const rcx=r10
!global const rdx=r11
!global const r14=rframe
!global const r15=rstack

export ref mclrec mccode, mccodex		!genmc adds to this linked list

global int currsegment=0		!

global int currzdataalign=0
global int curridataalign=0

!global int framebytes			!local stackframe size
!global int parambytes
global int frameoffset
global int isthreadedproc
global int iscallbackproc

global int structretoffset			!0, or offset of R9 copy within struct
global ref mclrec stacksetinstr		!caller of any fn: instr that sets sp
global int currblocksize			!0, or set to largest block ret value
!global ref mclrec allmclcode
global ichar allasmstr
global int allasmstrlen

global mcloperand dstackopnd
global mcloperand dframeopnd

global mcloperand zero_opnd=nil
!global unit zero_unit

global [r0..r15,1..16]mcloperand regtable

global const maxsmallint=32
global [0..maxsmallint]mcloperand smallinttable

global [-128..64]mcloperand frameregtable

global const initstringsize	= 1024
global const initrealsize		= 16

global ref []ichar	stringtable
!global ref []int32    stringlentable
global ref []int32   stringlabtable
global ref []real	realtable
global ref []int32	reallabtable

global int stringtablesize
global int realtablesize

global int nstrings=0
global int nreals=0

!global const maxmlabelno=20'000
!global const maxlabelno=40'000
!global const maxlabelno=60'000
!global const maxlabelno=80'000
!global const maxlabelno=200'000
!global const maxlabelno=400'000
global const maxlabelno=800'000
!global const maxlabelno=1400'000
global [maxlabelno]ref mclrec labeltable

!int framebytes, parambytes

global strbuffer sbuffer
global ref strbuffer dest=&sbuffer
global int destlinestart
global psymbol currasmproc
global int noregvar				!1 to inhibit strreg showing regvar names

global int mseqno

global [rtsnames.len]int rtsproclabels		!non-zero means rtsfn has been used

global int lab_funcnametable
global int lab_funcaddrtable
global int lab_funcnprocs

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

global int ss_zdatalen
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

!const max_ss_symbols=32768				!exported to coff
global const init_ss_symbols=32768				!exported to coff
!global const init_ss_symbols=16384
global ref []psymbol ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

!global ref stlipstrec globalimportlist		!all global vars and imports across all moduls

global ref[]psymbol labeldeftable

global int alineno
!global int pmlineno

global proc axerror(ichar mess)=
!	fprintln "MCL Error: # (#) on Line: # in #",mess,param
!	fprintln "MCL Error: # (#) [#]",mess,param,mseqno
	fprintln "MCL Error: #",mess
!		mlineno iand 16777215, sourcefilenames[mlineno>>24]
	PRINTLN
	STOP 1
!	stopcompiler(sourcefilepaths[mlineno>>24],mlineno iand 16777215)
end

global const maxplibfile=50
global [0..maxplibfile]ichar plibfiles
global [0..maxplibfile]byte plibtypes
global int nplibfiles

global ref proc(ref void)hostasmhandler

global psymbol pstentrypoint
global int plibmode
=== pc_objdecls.m 0 0 25/44 ===
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
=== pc_optim.m 0 0 26/44 ===
import clib
import mlib
import* pci_core
import* pci_mcl


global proc peephole(ref mclrec m)=
	ref mclrec m2,m3,mtarget,lastmcl
	int lab1,lab2
STATIC INT COUNT=0

	lastmcl:=nil

!RETURN
	if optimflag<2 then return fi

	do
!ADDNOTE("XXXXXX")
		m2:=m.nextmcl
!CPL =M,=M2,MCLNAMES[M.OPCODE]
		while m2 and m2.opcode in [m_comment, m_deleted] do m2:=m2.nextmcl od

		switch m.opcode
		when m_procstart then
!CPL "PEEPHOLE",M.A.DEF.NAME

		when m_procend then
			exit

		when m_jmp then
dojmp::
			if m.a.valtype<>label_val then skip fi
!CPL VALTYPENAMES[M.A.VALTYPE]
			mtarget:=labeltable[m.a.labelno].nextmcl
			while mtarget.opcode=m_label do mtarget:=mtarget.nextmcl od
			if mtarget.opcode=m_jmp then
				m.a:=mgenlabel(mtarget.a.labelno)
			fi

			if m.opcode=m_jmp and m2.opcode=m_jmp then
				deletemcl(m2,101)
			fi


		when m_jmpcc then
			if m2.opcode<>m_jmp then goto dojmp fi
!jcc followed by jmp; detect jcc L1; jmp L2; L1: and replace with:
! jncc L2; <deleted>; L1
			lab1:=m.a.labelno
			m3:=m2.nextmcl
			if m3.opcode=m_label and m3.a.labelno=lab1 then
				m.a:=mgenlabel(m2.a.labelno)
				m.cond:=asmrevcond[m.cond]
				deletemcl(m2,102)
			fi

		when m_test then
			case lastmcl.opcode
			when m_andx, m_orx, m_xorx then
				if sameregopnd(m.a,m.b) and sameregopnd(m.a,lastmcl.a) then
					deletemcl(m,103)
!					m.opcode:=m_deleted
				fi
			esac

		when m_movzx then
!CPL "MOVZX",M.LINENO,SOURCEFILEPATHS[M.FILENO]
!ADDNOTE("MOVZX",M)
			if m.a.mode=a_reg and m.a.size=8 and m.b.size<4 then
				m.a:=changeopndsize(m.a,4)
			fi
!			if m2.opcode=m_test and isreg(m2.a,r0) and isreg(m2.b,r0) and
!					m2.nextmcl.opcode=m_jmpcc then
!				m.opcode:=m_cmp
!				m.a:=m.b
!				m.b:=mgenint(0)
!!				IF M2.NEXTMCL.REGEND[R0]<>1 THEN
!!					CPL "MOVZX/TEST; FREE NOT SEEN"
!!					ADDNOTE("FREENOT SEEN",M)
!!				FI
!				deletemcl(m2,104)
!ADDNOTE("MOVZX2",M2)
!			fi
!
		when m_mov then
!SKIP2
			if m.a.mode=a_reg and m.a.reg=r10 and m.b.mode=a_reg and m.b.reg<=r1 then
				if lastmcl.a.mode=a_reg and lastmcl.a.reg=m.b.reg and
						lastmcl.opcode in [m_mov, m_movsx, m_movzx, m_lea] then
					lastmcl.a:=mgenreg(r10)
					deletemcl(m,105)
				fi
			fi

			if isreg0(m.a) and isregopnd(m.b) then
				if isreg0(m2.b) and m2.regend[r0] AND M2.A.SIZE=8 then
					m2.b:=m.b
!					addnote("MOV D0,RV;OPC D0... => OPC RV...",m2)
					deletemcl(m,106)
					skip
				fi

				if not isreg0(m2.a) then skip fi
				m3:=m2.nextmcl

				if m2.opcode=m_cmp and m3.opcode=m_jmpcc and m3.regend[r0] then
					m2.a:=m.b
					deletemcl(m,107)
				elsif m2.opcode=m_test and isreg0(m2.b) and
						m3.opcode=m_jmpcc and m3.regend[r0] then
					m2.a:=m.b
					m2.b:=m.b
					deletemcl(m,108)
				elsif m2.opcode in [m_inc, m_dec] and isreg0(m2.a) then
					m.opcode:=m_lea
					m.b:=mgenindex(areg:m.b.reg,offset:(m2.opcode=m_inc|1|-1))
					deletemcl(m2,120)
					redo
				elsif m2.opcode in [m_add, m_sub] and isreg0(m2.a) then
					if isconst(m2.b) and (m2.b.value in int32.minvalue..int32.maxvalue) then
!ADDNOTE("IMM121",M)
						m.opcode:=m_lea
						m.b:=mgenindex(areg:m.b.reg,
							offset:(m2.opcode=m_add|m2.b.value|-m2.b.value))
						deletemcl(m2,121)
						redo
					elsif isregopnd(m2.b) and m2.opcode=m_add then
						m.opcode:=m_lea
						m.b:=mgenindex(areg:m.b.reg,ireg:m2.b.reg)
						deletemcl(m2,122)
						redo
					fi
				fi
			fi

			if isreg0(m.a) and isconst(m.b) and
					 (m.b.value in int32.minvalue..int32.maxvalue) then
				if isreg0(m2.b) and m2.regend[r0] then
					m2.b:=m.b
!ADDNOTE("IMM109",M2)
!					addnote("MOV D0,IMM; OPC XXX,DO => OPC XXX,IMM",M2)
					deletemcl(m,109)
				fi
			fi

!			if isreg0(m.b) and m2.opcode=m_mov and isreg0(m2.a) and sameoperand(m.a,m2.b) then
!!				CPL "RELOADING VALUE JUST STORED",++COUNT
!				deletemcl(m2,140)
!			elsif m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg and

			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg and
				m.b.reg=m2.a.reg and sameoperand(m.a,m2.b) then
!CPL "RELOADING VALUE/NOT REG0",++COUNT
!ADDNOTE("RELOAD VALUE/NOT R0",M2)
				deletemcl(m2,141)
			fi



			if m.b.mode=a_reg and m2.opcode=m_mov and m2.a.mode=a_reg then
				 if m.a.mode=a_mem and sameoperand(m.a, m2.b) then		!mov [MEM1],Da; mov Db,[MEM1] => mov Db,Da
!CPL "MOV [MEMX],DA; MOV DB,[MEMX] DETECTED",++COUNT
					m2.b:=mgenreg(m.b.reg)
				fi
			fi
		when m_xorx then
			if isreg0(m.a) and isreg0(m.b) then
!CPL =M2.REGEND[R0]
				if isreg0(m2.b) and m2.regend[r0] then
					m2.b:=mgenint(0)
!					ADDNOTE("USE IMM 0",M2)
					deletemcl(m,110)
				fi
			fi

		when m_lea then
			if isreg0(m.a) and m2.opcode=m_mov then
				if isregopnd(m2.a) and isreg0(m2.b) and m2.regend[r0] then
!cpl "THIS"
					m.a:=m2.a
					deletemcl(m2,131)
				fi
			fi

		end switch

skip::
		lastmcl:=m
		m:=m2
	od
end

function isreg(mcloperand a, int reg=rnone)int=
	if not a then return 0 fi
	if not a.mode=a_reg then return 0 fi
	if reg=rnone then return 0 fi
	return reg=a.reg
end

function isreg0(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_reg and a.reg=r0 then return 1 fi
	return 0
end

function isregopnd(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_reg and isregvar[a.reg] then return 1 fi
	return 0
end

function isconst(mcloperand a)int=
	if not a then return 0 fi
	if a.mode=a_imm and a.valtype=intimm_val then
		return 1
	fi
	return 0
end

proc deletemcl(ref mclrec m,int id=0)=
	[128]char str
!	fprint @&.str,"TO BE DELETED (#)",id

!	fprint @&.str,"DELETED (#)",id
!	addnote(&.str,m)

!if m.opcode=m_label then
!CPL "DELETING LABEL",ID
!FI

	m.opcode:=m_deleted
end
=== pc_parse.m 0 0 27/44 ===
int insideproc

global function parse(ichar source)int=
	psymbol d

	startlex(source)
	int labno

!	repeat
!		lex()
!		printsymbol()
!	until lxsymbol=eofsym
!	STOP

	lxsymbol:=errorsym

	do
		lex()

		case lxsymbol
		when kopcodesym, typesym then
			if lxsubcode=kendprogram then exit fi

			readinstr()

		when labelsym then
			dolabel()

		when namesym then
			SERROR("""NAME:"" NOT SUPPORTED")
!			d:=lxsymptr
!			lex()
!			case lxsymbol
!			when colonsym then
!			when dcolonsym then
!				d.isexported:=1
!			else
!				serror(": expected")
!			esac
!			assignlabeltoname(d)
!			pcl_gen(klabelname,pcl_genmemaddr(d))
!			pccurr.seqno:=lxlineno
!!			pcl_gen(klabel,genlabel(d.labelno))
!			lex()

		when intconstsym then
			next

		when eolsym then
			next
		when eofsym then
			exit
		else
			println =symbolnames[lxsymbol]
			serror("Bad input")
		esac
		checksymbol(eolsym)
	od

	if insideproc then serror("End missing") fi

	return 1
end

proc checksymbol(int symbol)=
	if lxsymbol<>symbol then
		println symbolnames[symbol],"expected, not",symbolnames[lxsymbol],,", on line:",lxlineno
		stop 1
	fi
end

proc dolabel=
	if lxvalue not in 1..maxlabels then
		serror_s("1:Label out of range:",strint(lxvalue))
	fi

	pdefinelabel(lxvalue,lxlineno)
	lex()
	checksymbol(colonsym)
	lex()
end

proc readmode(pcl p)=
	if p.mode<>tpvoid then
		p.oldmode:=lxsubcode
		lex()
		return
	fi

	p.mode:=lxsubcode
	p.size:=psize[lxsubcode]
	lex()
	if p.mode=tpblock then
		checksymbol(colonsym)
		lex()
		checksymbol(intconstsym)
		p.size:=lxvalue
		lex()
	fi
end

proc readinstr=
!I have a bare opcode; see if any other operands follow. Syntax is:
! push[:type[:size]] [operand [:type[:size]] [,A[,B]]]
	int opcode,mode,size
	pcl p

	p:=newpcl()				!all zeros
	p.seqno:=lxlineno
	mode:=tpvoid			!possible override
	size:=0

	if lxsymbol=typesym then
		readmode(p)
		checksymbol(kopcodesym)
	fi

	p.opcode:=opcode:=lxsubcode

	lex()

	if lxsymbol=typesym then
		readmode(p)
	fi

	case pclhasopnd[opcode]
	when 1 then
		case lxsymbol
		when intconstsym then
			p.value:=lxvalue
			p.opndtype:=int_opnd
			mode:=tpi64
			lex()
		when realconstsym then
			p.xvalue:=lxxvalue
			p.opndtype:=real_opnd
			mode:=tpu64
			lex()
		when stringconstsym then
			p.svalue:=pcm_copyheapstring(lxsvalue)
			p.opndtype:=string_opnd
			mode:=tpu64
			lex()
		when labelsym then
			if lxvalue not in 1..maxlabels then
				serror_s("2:Label out of range:",strint(lxvalue))
			fi
			p.labelno:=lxvalue
			p.opndtype:=label_opnd
			lex()
		when namesym then
			p.def:=lxsymptr
			p.opndtype:=mem_opnd
			p.def.istruename:=lxtruename

			lex()

		when int128constsym then
			p.value128:=lxuvalue128
			p.opndtype:=int128_opnd
			mode:=tpi128
			lex()

		when addrsym then
			lex()
			checksymbol(namesym)
			p.def:=lxsymptr
			p.def.istruename:=lxtruename
			p.opndtype:=memaddr_opnd
			lex()
			if lxsymbol=mulsym then
				p.def.isimported:=1
				lex()
			fi
		else
			serror("Missing or Bad Operand")
		esac

	when 2 then			!must be name that defines a proc or static or named label
		checksymbol(namesym)
		p.def:=lxsymptr
		if opcode=kprocrts then
			p.opcode:=kprocdef
			definertsproc(p.def)
		fi
		p.opndtype:=mem_opnd
		lex()
		case lxsymbol
		when colonsym then
		when dcolonsym then
			p.def.isexported:=1
			p.isexported:=1
		else
			serror("Colon expected")
		esac
		lex()

	when 3 then			!must be name that defines a local or param
		checksymbol(namesym)
		p.def:=lxsymptr
		p.opndtype:=mem_opnd
		lex()
		if lxsymbol=colonsym then serror(": not used for locals") fi
	when 4 then			!must be name that defines a proc or static or named label
		checksymbol(namesym)
		p.def:=lxsymptr
		p.opndtype:=mem_opnd
		lex()

	esac

	if lxsymbol=typesym then
		readmode(p)
	fi

!if no type used, but available from operand, then use that

	if p.mode=tpvoid then
		if mode<>tpvoid then
			p.mode:=mode
			p.size:=size
		fi
	fi

	if pclextra[opcode] then
		checksymbol(intconstsym)
		p.x:=lxvalue
		lex()
		if pclextra[opcode]=2 then
			checksymbol(intconstsym)
			p.y:=lxvalue
			lex()
		fi
	fi
end

global proc serror(ichar mess)=
	serror_s(mess, nil)
end

global proc serror_s(ichar mess, param=nil)=
	print "Syntax error:",mess
	if param then
		print ":",param
	fi

	println " on line:",lxlineno
	stop 1
end

export function pcl_readpclfile(ichar filename, rtsfile=nil)int=
!note: currently only works one-time
	ichar source

	pcl_start()

	if not parse_readsource(filename) then return 0 fi
	
	if rtsfile then
		labelnooffset:=maxuserlabel
		if not parse_readrts() then
			println "No RTS found"
			return 0
		fi
	fi

	pcl_gen(kendprogram)
	pcl_end()

	return 1
end

global function parse_readsource(ichar filename)int=
	ichar source

	source:=readfile(filename)
	if source=nil then
		errormess:="Can't load file"
		return 0
	fi	

	if not parse(source) then
		errormess:="Couldn't parse"
		return 0
	fi
	return 1
end

global function parse_readrts:int=
	static ichar source = strinclude("rts.pcl")

	if not parse(source) then
		errormess:="Couldn't parse RTS"
		return 0
	fi
	return 1
end
=== pc_runmx.m 0 0 28/44 ===
int nsymimports=0, nsymexports=0

librec lib

global function writememlib(ichar filename)ref librec plib=
	int n, k

	clear lib

!CPL "WRITEMEMLIB",FILENAME

	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	lib.version:="0.1234"

	lib.filespec:=filename
	lib.libname:=pcm_copyheapstring(extractbasefile(filename))
	lib.libno:=1

	scansymbols()
	writerelocs()

	lib.zdatasize:=ss_zdatalen
!	lib.zdataptr:=pcm_allocz(ss_zdatalen)
	lib.codesize:=bufferlength(ss_code)
	lib.idatasize:=bufferlength(ss_idata)

	lib.codeptr:=bufferelemptr(ss_code,0)
	lib.idataptr:=bufferelemptr(ss_idata,0)

	int ndlls:=0, nlibs:=0

	for i to nplibfiles do
		if plibtypes[i]='D' then ++ndlls else ++nlibs fi
	od

	lib.ndlllibs:=ndlls
	lib.nlibs:=nlibs

	lib.dllnames:=pcm_alloc(ichar.bytes*ndlls)
	lib.libnames:=pcm_alloc(ichar.bytes*nlibs)

	k:=0
	for i to nplibfiles when plibtypes[i]='D' do
		lib.dllnames[++k]:=plibfiles[i]
	od

	k:=0
	for i to nplibfiles when plibtypes[i]='L' do
		lib.libnames[++k]:=plibfiles[i]
	od

	writesymbols()
	plib:=pcm_alloc(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

proc writerelocs=
	ref relocrec oldr
	mcxreloc newr
	int n, k
	psymbol d
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	lib.nrelocs:=ss_nidatarelocs+ss_ncoderelocs
	lib.reloctable:=pcm_alloc(lib.nrelocs*mcxreloc.bytes)

	k:=0

	for i in code_seg..idata_seg do
		oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

		while oldr, oldr:=oldr.nextreloc do
			clear newr

			newr.offset:=oldr.offset
			newr.segment:=(i=code_seg|idata_seg|code_seg)

			d:=ss_symboltable[oldr.stindex]

			case oldr.reloctype
			when rel32_rel then
				if d.isimported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.isimported then
					newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
					newr.stindex:=d.impindex
				else
					if oldr.reloctype=addr32_rel then
						newr.reloctype:=locabs32_rel
					else
						newr.reloctype:=locabs64_rel
					fi
					newr.targetsegment:=d.segment
				fi
			else
				axerror("reloc?")
			esac

			lib.reloctable[++k]:=newr

		od
	od
end

proc scansymbols=
	psymbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.isexported then d.expindex:=++nsymexports fi
		if d.isimported then d.impindex:=++nsymimports fi
	od
end

proc writesymbols=
	psymbol d
	u64 epoffset:=-1
	int n, k
	ichar name

	lib.nimports:=nsymimports
	lib.nexports:=nsymexports
	lib.importnames:=pcm_alloc(nsymimports*ichar.bytes)
	lib.exports:=pcm_alloc(nsymexports*ichar.bytes)
	lib.exportsegs:=pcm_alloc(nsymexports)
	lib.exportoffsets:=pcm_alloc(nsymexports*u64.bytes)

	k:=0
	for i to ss_nsymbols when ss_symboltable[i].impindex do
		lib.importnames[++k]:=ss_symboltable[i].name
	od

	k:=0
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			name:=getbasename(d.name)
			if epoffset=-1 and (eqstring(name,"start") or eqstring(name,"main")) then
				epoffset:=d.offset
			fi
			lib.exports[++k]:=name
			lib.exportsegs[k]:=d.segment
			lib.exportoffsets[k]:=d.offset
		fi
	od

	lib.entryoffset:=u32(epoffset)
end

proc roundsegment(ref dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	od
end
=== pc_stackmcl.m 0 0 29/44 ===
global macro freereg(r) =
	(regset[r]:=0; mccodex.regend[r]:=1)

global proc resetopnds1=
!after pass1

!CPL =RESETOPNDS1
	if mstackdepth then
		println "1:HW stack not empty",procdef.name,=mstackdepth
		MSTACKDEPTH:=0
!		merror("reset:mstackdepth?")
	fi
	if noperands then
		println "1:Reset:pcl stack not empty:",procdef.name,=noperands
		NOPERANDS:=0
!		merror("reset:pcl stack not empty?")
	fi
!	if ncalldepth then merror("reset:call stack not empty?") fi

!should set these to zero but should already be zero

!check reg flags
	for i in regset.bounds do
		if regset[i] or xregset[i] then
			println "Reset: reg flag set",procdef.name
			exit
		fi
	od

!--------------------------------------------
!!Work out values for the optimiser, and display as comments
	if inf_proccalls=0 then inf_leafproc:=1 fi

!	if nproccalls=0 and (nprocparams+nprocxparams)=0 then ++nzeroparamleaf fi

!	mgeninfos("High reg:  ",getregname(inf_highreg))
!	mgeninfos("High xreg: ",fgetregname(inf_highxreg))
!	mgeninfo ("Calls:     ",inf_proccalls)
!	mgeninfos("Leaf func: ",(inf_leafproc|"Yes"|"No"))
!	mgeninfo ("Locals:    ",inf_proclocals)
!	mgeninfo ("Xlocals:   ",inf_procxlocals)
!	mgeninfo ("Max args:  ",inf_maxargs)

!--------------------------------------------
!reset the values for next proc
!	memset(&.regset,0,regset.bytes)
!	memset(&.xregset,0,xregset.bytes)

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

	if not optimflag then			!else needed for pass 2 procentry
		inf_proccalls:=0
		inf_maxargs:=0
		inf_proclocals:=0
		inf_procxlocals:=0

		inf_leafproc:=0
!cpl "CLEAR HIGHREG",RNONE
		inf_highreg:=inf_highxreg:=rnone
		inf_assem:=0
	else
!optimising: need some of these so don't clear

		inf_proccalls:=0
		inf_maxargs:=0
		inf_proclocals:=0
		inf_procxlocals:=0

!		inf_leafproc:=0
!		inf_highreg:=inf_highxreg:=rnone
!		inf_assem:=0


	fi
end

global proc resetopnds2=
!after pass2

	if mstackdepth then
		println "2:HW stack not empty",procdef.name,=mstackdepth
	fi
	if noperands then
		println "2:Reset:pcl stack not empty:",procdef.name,=noperands
		NOPERANDS:=0
	fi
!	if ncalldepth then merror("reset:call stack not empty?") fi

!should set these to zero but should already be zero

!check reg flags
	for i in regset.bounds do
		if regset[i] or xregset[i] then
			println "2:Reset: reg flag set",regnames[i],procdef.name
			exit
		fi
	od

!MGENINFOS("2:RESETOPNDS",procdef.name)
	inf_proccalls:=0
	inf_maxargs:=0
	inf_proclocals:=0
	inf_procxlocals:=0

	inf_leafproc:=0
	inf_highreg:=inf_highxreg:=rnone
	inf_assem:=0

	inf_r10used:=inf_r11used:=inf_r13used:=0

!	memset(&.regset,0,regset.bytes)
!	memset(&.xregset,0,xregset.bytes)

	clear regset
	clear xregset
	clear isregvar
	clear isxregvar

end

proc newopnd(int fmt)=
	if noperands>=maxoperands then
		merror("PCL stack overflow")
	fi
	++noperands
	pclstack:=cast(&pclstack[0])
	pclstack[1]:=pclstackzero
	pclstack[1].fmt:=fmt
	pclstack[1].loc:=loccodes[fmt]
	pclstack[1].reg:=rnone
!	pclstack[1].value:=0
!	pclstack[1].high:=0
!	pclstack[1].low:=0
	pclstack[1].float:=floatloc[fmt]
end

global proc duploperand=
!assume 64-bit operand
	int reg

	++noperands
	pclstack:=cast(&pclstack[0])
	pclstack[1]:=pclstack[2]

!There is now a simple duplicate; but it will need more work depending
!on the current format
	case loccodes[pclstack[1].fmt]
	when imm_loc then			!can be shared
	when mem_loc then			!
	when regvar_loc then		!
	when reg_loc then			!need to be physically duplicated
		reg:=getnextreg(0)
		pclstack[1].reg:=reg
		genmc(m_mov, mgenreg(pclstack[2].reg),mgenreg(reg))

	when xreg_loc then			!need to be physically duplicated
		reg:=getnextxreg(0)
		pclstack[1].reg:=reg
		genmc(m_movq, mgenxreg(pclstack[2].reg),mgenreg(reg))
	when stack_loc then			!need to be physically duplicated
		reg:=getnextreg(0)
		genmc(m_mov, mgenreg(reg),dstackopnd)
		genmc(m_push, mgenreg(reg))
	esac
end

global proc addint(int a)=
	newopnd(imm_d64)
	pclstack[1].value:=a
end

global proc addint128(ref int p)=
	newopnd(imm_d64)
	pclstack[1].value:=(p+1)^
	newopnd(imm_d64)
	pclstack[1].value:=p^
	setwideopnd()
end

global proc addreal(real x)=
	newopnd(imm_x64)
	pclstack[1].xvalue:=x
end

global proc addreal32(real x)=
	newopnd(imm_x32)
	pclstack[1].xvalue:=x
end

global proc addstring(ichar s)=
	newopnd(imm_str)
	pclstack[1].svalue:=s
end

global proc addmem(pcl p)=
	mcloperand ax
	psymbol d:=p.def

	case pcat[p.mode]
	when d64cat then
		if d.reg then
			newopnd(regvar_d64)
			pclstack[1].reg:=d.reg
		else
			newopnd(mem_d64)
		fi
	when x64cat then
		if d.reg then
			newopnd(xregvar_x64)
			pclstack[1].reg:=d.reg
		else
			newopnd(mem_x64)
		fi
	when x32cat then newopnd(mem_x32)
	when widecat then
		newopnd(memhigh_d64)
		pclstack[1].def:=d
		pclstack[1].wide:='H'
		newopnd(mem_d64)
		pclstack[1].wide:='L'
	when blockcat then
		newopnd(imm_memaddr)

	when shortcat then
		addreg_d64()
		ax:=genopnd(xa)
		genmc((psigned[p.mode]|m_movsx|m_movzx), ax, mgenmem(d))
		return

!	when tvoid then

	else
		addreg_d64()
!		MGENCOMMENT("****ADDMEM?")
		merror("ADDMEM?")
	esac

	pclstack[1].def:=d
end

global proc addmemaddr(psymbol d)=
	newopnd(imm_memaddr)
	pclstack[1].def:=d
end

global proc addlabeladdr(int lab)=
	newopnd(imm_labaddr)
	pclstack[1].value:=lab
end

global proc addlabel(int lab,offset=0)=
	newopnd(imm_label)
	pclstack[1].value:=lab
	pclstack[1].offset:=offset
end

global proc addreg0(int reg)=
!turn return value in r0 into a new pclstack operand
!(modified for mult regs)
	newopnd(reg_d64)
	pclstack[1].reg:=reg
	if regset[reg] then
		merror("addreg0/reg in use")
	fi
	regset[reg]:=1
end

global proc addwidereg0(int reg)=
!turn return value in r0 into a new pclstack operand
	int reg2
	reg2:=reg+1
	if reg2=r3 then reg2:=r10 fi
	newopnd(reg_d64)
	newopnd(reg_d64)
	pclstack[2].reg:=reg2
	pclstack[1].reg:=reg
	if regset[reg] then
		merror("addwidereg/reg(s) in use")
	fi
	regset[reg]:=1
!	regset[reg2]:=1
	setwideopnd(1)
end

global proc addxreg0(int reg,fmt)=
!turn return value in x0 into a new pclstack operand
	newopnd(fmt)
	pclstack[1].reg:=reg
	if xregset[reg] then merror("addxreg0/reg in use") fi
	xregset[reg]:=1
end

global proc addreg_d64=
!create new pcl opnd in any d64 reg
	newopnd(reg_d64)
	pclstack[1].reg:=getnextreg()
end

global proc addreg_x64=
	newopnd(xreg_x64)
	pclstack[1].reg:=getnextxreg()
end

global proc addreg_x32=
	newopnd(xreg_x32)
	pclstack[1].reg:=getnextxreg()
end

global function stropndstack(int indent=0)ichar=
	static [512]char str
	[512]char str2
	ichar s:=&.str, t
	pclstackrec pc

	if indent then
		fprint @s, "                                     ("
	else
		fprint @s, "("
	fi

	for i:=noperands downto 1 do
		pc:=pclstack[i]
		case pc.loc
		when reg_loc then
			strcat(s, regnames[pc.reg])

		when regvar_loc then
			strcat(s, regnames[pc.reg])
			strcat(s, "=")
			strcat(s, pc.def.name)

		when xreg_loc then
			strcat(s, xregnames[pc.reg])
			strcat(s, ":")
			strcat(s, fmtnames[pc.fmt])
		when xregvar_loc then
			strcat(s, xregnames[pc.reg])
			strcat(s, "=")
			strcat(s, pc.def.name)


		when stack_loc then
			strcat(s, "T")
		when mem_loc then
			strcat(s,"M:")
!			if pc.fmt=memhigh_d64 then strcat(s,"H:") fi
			strcat(s,pc.def.name)
		elsecase pc.fmt
		when imm_d64 then
			strcat(s, strint(pc.value))
		when imm_x64 then
				strcat(s, strreal(pc.xvalue))
		when imm_str then
			t:=pc.svalue
			if strlen(t)>20 then
				strcat(s,"LONG STR")
			else
				strcat(s,"""")
				convertstring(t,s+strlen(s))
				strcat(s,"""")
			fi
		when imm_memaddr then
			strcat(s,"&")
			strcat(s,pc.def.name)
		when imm_labaddr then
			strcat(s,"&#")
			strcat(s,strint(pc.labno))
		when imm_label then
			strcat(s,"#")
			strcat(s,strint(pc.labno))
		else
!			strcat(s,"??")
!			strcat(s,LOCNAMES[PCLLOC[I]])
			strcat(s,FMTNAMES[pc.fmt])
		esac
		if pc.wide then strcat(s,(pc.wide='H'|"(H)"|"(L)")) fi
!		if pc.high then strcat(s,"(H)") fi
!		if pc.low then strcat(s,"(L)") fi

		if i>1 then strcat(s,",") fi
	od
	strcat(s,") (")
	for r:=r0 to regmax do
		strcat(s,(regset[r]|"1 "|"0 "))
	od
	strcat(s,") (")
	for r:=r0 to xregmax do
		strcat(s,(xregset[r]|"1 "|"0 "))
	od

	strcat(s,") hwstack:")
	strcat(s,strint(mstackdepth))
	strcat(s," noperands:")
	strcat(s,strint(noperands))
	strcat(s," ncalldepth:")
	strcat(s,strint(ncalldepth))
	strcat(s," callslots[]:")
	strcat(s,strint(callslots[ncalldepth]))
	return s
end

global proc loadopnd(int n=1, int nvreg=0)=
	int reg,value
	mcloperand ax

	if n>noperands then
		MGENCOMMENT("UNDERFLOW")
		RETURN
		merror("loadopnd/underflow")
	fi
	if pclstack[n].loc=reg_loc then
		return
	fi

	if pclstack[n].loc=xreg_loc then
		return
	fi

	if pclstack[n].float then
		reg:=getnextxreg(nvreg)
	else
		reg:=getnextreg(nvreg)
	fi

	case pclstack[n].loc
	when xreg_loc then
		MERROR("LOADOPND/XREG")
!	when stack_loc then
!		MERROR("LOADOPND/STACK")
	elsecase pclstack[n].fmt
	when imm_d64 then
		value:=pclstack[n].value

		if value=0 and not noxorclear then
			ax:=mgenreg(reg,4)
			genmc(m_xorx,ax,ax)
		else
			genmc(m_mov,mgenreg(reg),mgenint(pclstack[n].value))
		fi

	when imm_x64 then
		genmc(m_movq,mgenxreg(reg),mgenrealmem(pclstack[n].xvalue))

	when imm_x32 then
		genmc(m_movd,mgenxreg(reg),mgenrealmem(pclstack[n].xvalue,4))

	when imm_str then
		genmc(m_mov,mgenreg(reg),mgenlabel(getstringindex(pclstack[n].svalue)))

	when imm_labaddr then
		genmc(m_mov, mgenreg(reg), mgenlabel(pclstack[n].labno))

	when imm_label then
		genmc(m_mov, mgenreg(reg), mgenlabelmem(pclstack[n].labno))
		mccodex.b.offset:=pclstack[n].offset

	when mem_d64 then
		genmc(m_mov,mgenreg(reg),mgenmem(pclstack[n].def))

	when mem_x64 then
		genmc(m_movq,mgenxreg(reg),mgenmem(pclstack[n].def))

	when mem_x32 then
		genmc(m_movd,mgenxreg(reg),mgenmem(pclstack[n].def))

	when memhigh_d64 then
		genmc(m_mov,mgenreg(reg),mgenmemhigh(pclstack[n].def))

	when regvar_d64 then
		genmc(m_mov,mgenreg(reg),mgenregvar(pclstack[n].def))

	when xregvar_x64 then
		genmc(m_movq,mgenxreg(reg),mgenxregvar(pclstack[n].def))

	when stack_d64 then
!		checkstackorder(n)
		genmc(m_pop, mgenreg(reg))
		--mstackdepth

	when stack_x64 then
!		checkstackorder(n)
		if inf_r13used then merror("R13 in use") fi
		genmc(m_pop, mgenreg(r13))
		genmc(m_movq, mgenxreg(reg), mgenreg(r13))
		--mstackdepth

	when stack_x32 then
!		checkstackorder(n)
		if inf_r13used then merror("R13 in use") fi
		genmc(m_pop, mgenreg(r13))
		genmc(m_movd, mgenxreg(reg), mgenreg(r13,4))
		--mstackdepth

	when imm_memaddr then
		genmc(m_lea,mgenreg(reg),mgenmem(pclstack[n].def))

	else
		CPL STROPNDSTACK()
		MERROR("LOADOPND??",fmtnames[pclstack[n].fmt])
	esac

	pclstack[n].reg:=reg
	pclstack[n].fmt:=loadfmt[pclstack[n].fmt]
	pclstack[n].loc:=loccodes[pclstack[n].fmt]
end

global proc loadparam(int n=1, reg)=
	int oldreg, value
	mcloperand ax

	ax:=mgenreg(reg)
	oldreg:=pclstack[n].reg

	case pclstack[n].fmt
	when reg_d64, regvar_d64 then
		genmc(m_mov, ax, mgenreg(oldreg))
		freereg(oldreg)

	when xreg_x64, xregvar_x64 then
		genmc(m_movq, ax, mgenxreg(oldreg))
		return							!leave loc unchanged
	when xreg_x32 then
		genmc(m_movd, changeopndsize(ax,4), mgenxreg(oldreg))
		return

	when imm_d64 then
		value:=pclstack[n].value
		if value=0 then
			ax:=mgenreg(reg,4)
			genmc(m_xorx, ax,ax)
		else
			genmc(m_mov, ax, mgenint(pclstack[n].value))
		fi
	when imm_x64 then
		genmc(m_mov, ax, mgenrealmem(pclstack[n].xvalue))
	when imm_str then
		genmc(m_mov,ax, mgenlabel(getstringindex(pclstack[n].svalue)))
!		genmc(m_mov,ax,mgenstring(pclvalues[n].svalue))

	when mem_d64 then
		genmc(m_mov,ax,mgenmem(pclstack[n].def))

	when memhigh_d64 then
		genmc(m_mov,ax,mgenmemhigh(pclstack[n].def))

	when imm_memaddr then
		genmc(m_lea,ax,mgenmem(pclstack[n].def))
	when mem_x64 then
		genmc(m_mov,ax,mgenmem(pclstack[n].def))

	when stack_d64 then

!		checkstackorder(n)
		genmc(m_pop, ax)
		--mstackdepth

	else
		CPL "LOADPARAM:",FMTNAMES[pclstack[n].FMT]
		MGENCOMMENT("****LOADPARAM??")
		MERROR("LOADPARAM??",fmtnames[pclstack[n].fmt])
	esac
	pclstack[n].loc:=reg_loc
	pclstack[n].reg:=reg
end

global proc loadxparam(int n=1, reg)=
	mcloperand ax

!	if reg=rnone then
!		reg:=getnextreg(nvreg)
!	else
!		if regset[reg] then
!			merror("loadopnd/reg in use")
!		fi
!	fi

	ax:=mgenxreg(reg)

	case pclstack[n].fmt
	when reg_d64, regvar_d64 then
		genmc(m_movq, ax, mgenreg(pclstack[n].reg))

	when xreg_x64, xregvar_x64 then
		genmc(m_movq, ax, mgenxreg(pclstack[n].reg))
	when xreg_x32 then
		genmc(m_movd, ax, mgenxreg(pclstack[n].reg))
	when imm_x64 then
		genmc(m_movq, ax, mgenrealmem(pclstack[n].xvalue))

	when imm_x32 then
		genmc(m_movd, ax, mgenrealmem(pclstack[n].xvalue))

	when mem_d64, mem_x64 then
		genmc(m_movq,ax,mgenmem(pclstack[n].def))

	when mem_x32 then
		genmc(m_movd,ax,mgenmem(pclstack[n].def))
	else
		CPL "??LOADXPARAM",N,NOPERANDS
		MGENCOMMENT("****LOADXPARAM??")
		MERROR("LOADXPARAM??",fmtnames[pclstack[n].fmt])
	esac
end

global function genopnd(int index=1,size=8)mcloperand ax=
!int, float, or low half of wide
	int reg, value

	case pclstack[index].loc
	when reg_loc then
		return mgenreg(pclstack[index].reg,size)

	when regvar_loc then
		return mgenregvar(pclstack[index].def)

	when xreg_loc then
		return mgenxreg(pclstack[index].reg,size)

	when xregvar_loc then
		return mgenxregvar(pclstack[index].def)

	elsecase pclstack[index].fmt
	when mem_d64, mem_x64, mem_x32 then
		return mgenmem(pclstack[index].def)

	when memhigh_d64 then
		return mgenmemhigh(pclstack[index].def)
	when imm_d64 then
		value:=pclstack[index].value
		if int32.minvalue<=value<=int32.maxvalue then
			return mgenint(value)
		fi
		ax:=mgenreg(getnextreg())
		genmc(m_mov, ax, mgenint(value))
		pclstack[index].reg:=ax.reg
		pclstack[index].fmt:=reg_d64
		pclstack[index].loc:=reg_loc
		return ax

	when imm_x64 then
		return mgenrealmem(pclstack[index].xvalue)

	when imm_x32 then
		return mgenrealmem(pclstack[index].xvalue,4)

	when imm_memaddr then
		reg:=getnextreg()
		ax:=mgenreg(reg)
		genmc(m_lea,ax,mgenmem(pclstack[index].def))
		pclstack[index].reg:=ax.reg
		pclstack[index].fmt:=reg_d64
		pclstack[index].loc:=reg_loc
		return ax
	when imm_str then
		genmc(m_lea,ax:=mgenreg(getnextreg()),mgenlabel(getstringindex(pclstack[index].svalue)))
		pclstack[index].reg:=ax.reg
		pclstack[index].fmt:=reg_d64
		pclstack[index].loc:=reg_loc
		return ax

	else
		CPL =LOCNAMES[PCLSTACK[INDEX].LOC]
		CPL =FMTNAMES[PCLSTACK[INDEX].FMT]
		CPL("GENOPND??")
		MGENCOMMENT("****GENOPND??")
		merror("GENOPND? ",fmtnames[pclstack[index].fmt])
	esac

	return nil
end

global function genopnd_ld(int index=1,size=8)mcloperand=
	loadopnd(index)
	return genopnd(index,size)
end

global function genopnd_ind(int index=1,size=8)mcloperand=
!int, float, or low half of wide
!
	case pclstack[index].loc
	when reg_loc then
		return mgenireg(pclstack[index].reg,size)
		return mgenireg(pclstack[index].reg,size)
		return mgenireg(pclstack[index].reg,size)
	esac

	loadopnd(index)

	return genopnd_ind(index,size)
end

global function genopnd_d64:mcloperand=
!create new d64 register operand
	addreg_d64()
	return genopnd()
end

global proc setwideopnd(int n=1)=
	pclstack[n].wide:='L'
	pclstack[n+1].wide:='H'
end

global proc unsetwideopnd(int n=1)=
	pclstack[n].wide:=0
end

global function getnextreg(int nvreg=0)int=
	int reg,firstreg

	firstreg:=(nvreg|r3|r0)

	for r:=firstreg to regmax do
		if regset[r]=0 then
			regset[r]:=1
			inf_highreg max:=r
!IF EQSTRING(PROCDEF.NAME,"qq_jhandlers.j_storef") THEN
!CPL "SET",=INF_HIGHREG,PROCDEF.NAME
!FI
			return r
		fi
	od

!all regs occupied; need to free one
	for i:=noperands downto 1 do
		if pclstack[i].loc=reg_loc then
			reg:=pclstack[i].reg
			if reg>=firstreg then
				pushopnd(i)
				return getnextreg(nvreg)
			fi
		fi
	od
	merror("NO FREE REGS")
	return 0
end

global function getnextxreg(int nvreg=0)int=
	int reg,firstreg

	firstreg:=(nvreg|r6|r4)

	for r:=firstreg to regmax do
		if xregset[r]=0 then
			xregset[r]:=1
			inf_highxreg max:=r
			return r
		fi
	od

!all regs occupied; need to free one
!	for i:=1 to noperands do
	for i:=noperands downto 1 do
		if pclstack[i].loc=reg_loc then
			reg:=pclstack[i].reg
			if reg>=firstreg then
				pushopnd(i)
				return getnextxreg(nvreg)
			fi
		fi
	od
	merror("NO FREE XREGS")
	return 0
end

global proc delopnd=
	if noperands<=0 then
		MGENCOMMENT("****DELND/UNDERFLOW")
		RETURN
!		merror("popopnd/underflow")
	fi

	case pclstack[1].loc
	when reg_loc,regvar_loc then
		freereg(pclstack[1].reg)
	when xreg_loc,xregvar_loc then
		freexreg(pclstack[1].reg)
	when mem_loc then
	when imm_loc then
!	when stack_opnd then
!	when str_loc then
	else
		merror("Can't pop opnd: #",locnames[pclstack[1].loc])
	esac

	--noperands
	pclstack:=cast(&pclstack[2])
end

global proc freexreg(int xr)=
	xregset[xr]:=0
end

global proc pushopnd(int n)=
!make sure operand n is on the hw stack; caller must now that all
!previous pclstack operands are already on the stack

	case pclstack[n].loc
	when reg_loc then
		genmc(m_push, mgenreg(pclstack[n].reg))
		freereg(pclstack[n].reg)

	when regvar_loc then
		genmc(m_push, mgenreg(pclstack[n].reg))

	when xreg_loc then
		if inf_r13used then merror("2:R13 in use") fi
		genmc(m_movq,mgenreg(r13), mgenxreg(pclstack[n].reg))
		genmc(m_push, mgenreg(r13))
		freexreg(pclstack[n].reg)

	when stack_loc then
		return
	elsecase pclstack[n].fmt
	when memhigh_d64 then
		genmc(m_push, mgenmemhigh(pclstack[n].def))

	when mem_d64, mem_x64 then
		genmc(m_push, mgenmem(pclstack[n].def))

	when mem_x32 then
		if inf_r13used then merror("4:R13 in use") fi
		genmc(m_mov,mgenreg(r13,4), mgenmem(pclstack[n].def))
		genmc(m_push, mgenreg(r13))

	when imm_d64 then
		genmc(m_push, mgenint(pclstack[n].value))

	when imm_x64 then
		genmc(m_push, mgenrealmem(pclstack[n].xvalue))

	when imm_str then
		genmc(m_push, mgenlabel(getstringindex(pclstack[n].svalue)))

	when imm_memaddr then
		if inf_r13used then merror("3:R13 in use") fi
		genmc(m_lea, mgenreg(r13), mgenmem(pclstack[n].def))
		genmc(m_push, mgenreg(r13))

	else
		merror("Can't push opnd: #",fmtnames[pclstack[n].fmt])
	esac

	pclstack[n].loc:=stack_loc
	pclstack[n].fmt:=pushfmt[pclstack[n].fmt]
	++mstackdepth
end

global proc pushallopnds(int n=1)=
	for i:=noperands downto n do
		pushopnd(i)
	od
end

global proc poparg=
	case pclstack[1].loc
	when reg_loc then freereg(pclstack[1].reg)
	when xreg_loc then freexreg(pclstack[1].reg)
	when stack_loc then
	when imm_loc then
	when mem_loc then
	when regvar_loc then
	when xregvar_loc then
	else
		CPL "POPARG:",LOCNAMES[PCLSTACK[1].LOC]
		MGENCOMMENT("****POPARG?")
		merror("poparg? #",locnames[pclstack[1].loc])
	esac
	--noperands
	pclstack:=cast(&pclstack[2])
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
		genmc(m_sub,dstackopnd,mgenint(n))
	fi
end

global proc popstack(int n)=
	if n then
		genmc(m_add,dstackopnd,mgenint(n))
	fi
end

global proc saveopnd(int n)=
!make sure operand n is on the hw stack; caller must now that all
!previous pclstack operands are already on the stack
	int reg

!TO BE REVISED
	case pclstack[n].loc
	when reg_loc then
		reg:=pclstack[n].reg
		if reg in r0..r2 then
			pushopnd(n)
		fi

	when xreg_loc then
		reg:=pclstack[n].reg
		if reg in r0..r5 then
			pushopnd(n)
		fi
	when stack_loc then
	when regvar_loc, xregvar_loc then
	when imm_loc then
		pushopnd(n)
	elsecase pclstack[n].fmt
	when memhigh_d64 then
		pushopnd(n)
	when mem_d64, mem_x64,mem_x32 then
		pushopnd(n)

	else
		merror("Can't save opnd: #",fmtnames[pclstack[n].fmt])
	esac
end

global proc saveallopnds(int n=1)=
	for i:=noperands downto n do
!	for i to n do
		saveopnd(i)
	od
end

global proc movetoreg(int newreg)=
	int oldreg

	loadopnd()

	oldreg:=pclstack[1].reg

	if oldreg=newreg then
		return
	fi

	if regset[newreg] then merror("movereg/reg in use") fi
	genmc(m_mov, mgenreg(newreg), mgenreg(oldreg))
	freereg(oldreg)
	pclstack[1].reg:=newreg
	regset[newreg]:=1
	if newreg>=r10 then inf_highreg max:=newreg fi
end

global proc swapopnds(int m,n)=
!exchange top opndstack entry (m assumed to be 1) with n'th entry down
!uses notional index of stack with:
!	[1] meaning opndstack[noperands]
!	[n] meaning opndstack[noperands-n+1]
!NOTE: all operands m to n inclusive
!caller is responsible for this (getopnds(n) might ensure this when m=1)
!usually m=1
	pclstackrec t

	t:=pclstack[m]
	pclstack[m]:=pclstack[n]
	pclstack[n]:=t

!swap(pclstack[m],pclstack[n])
end

global proc swapopndregs(int reg2)=
!top operand is in a register. Need to switch its reg with whatever is occupying
!reg2

	int reg1:=pclstack[1].reg

	for i:=2 to noperands do
		if pclstack[i].loc=reg_loc and pclstack[i].reg=reg2 then
			swap(pclstack[1].reg, pclstack[2].reg)
			return
		fi
	else
		CPL PROCDEF.NAME
		merror("swapopndregs/reg not found")
	od
end

global proc showopndstack=
	mgencomment(stropndstack(1))
end

=== pc_tables.m 0 0 30/44 ===
!type system

export tabledata() 	[0:]ichar pstdnames,
		[0:]byte psize, [0:]byte psigned, [0:]byte pfloat, [0:]byte pcat =
!    type         name    bits SZ   S F Cat
	(tpvoid=0,    "void",    	0,	0,0, voidcat),

	(tpu8,        "u8",      	1,	0,0, shortcat),
	(tpu16,       "u16",    	2,	0,0, shortcat),
	(tpu32,       "u32",    	4,	0,0, shortcat),
	(tpu64,       "u64",    	8,	0,0, d64cat),
	(tpu128,      "u128",  		16, 0,0, widecat),

	(tpi8,        "i8",      	1,	1,0, shortcat),
	(tpi16,       "i16",    	2,	1,0, shortcat),
	(tpi32,       "i32",    	4,	1,0, shortcat),
	(tpi64,       "i64",    	8,	1,0, d64cat),
	(tpi128,      "i128", 	 	16,	1,0, widecat),

	(tpr32,       "r32",    	4,	0,1, x32cat),
	(tpr64,       "r64",    	8,	0,1, x64cat),

	(tpblock,     "block",   	0,	0,0, blockcat),

	(tplast,      "$last",   	0,	0,0, voidcat),
end

global tabledata() [0:]ichar catnames =
	(voidcat=0,		$),
	(d64cat,		$),
	(x32cat,		$),
	(x64cat,		$),
	(shortcat,		$),
	(widecat,		$),
	(blockcat,		$),
end

global tabledata() [0:]ichar opndnames =
	(no_opnd=0,			$),
	(mem_opnd,			$),
	(memaddr_opnd,		$),
	(label_opnd,		$),
	(int_opnd,			$),
	(real_opnd,			$),
	(int128_opnd,		$),
	(real32_opnd,		$),
	(string_opnd,		$),
	(assem_opnd,		$),
end

!Stack operands are:
!	Xa				1st of 1
!   Xb, Ya			1st/2nd of 2
!   Xc, Yb, Za		1st/2nd/3rd of 3
!   Xd, Yc, Zb, Wa	1st/2nd/3rd/4th of 4
! X is always the 'left-most' operand, but will be at offset 0, 1 2 from top of stack
! a (as in Xa, Ya, Za, Wa) is always the top of stack

!Immediate operand:
!   A			(various)
!Extra info:
!   op			opindex
!   fn			fnindex
!   cc			cond code
!   t[:size]    type (:size for block types)
!   u           secondary type for some ops (convert etc)
!   n			nargs for calls
!   s x			scale and offset for ptr/offset ops
!   x y			min/max lab index for switch
!	B			Secondary operand in a following kopnd instruction
!	C			Tertiary operand in a following kopnd instruction

!Stack usage is represented by (a b):
! a is the number of stack elements that will be popped
! b is the number of new stack elements that will be pushed
! Something like (1 1) can mean the same element stays in place


export tabledata() [0:]ichar pclnames,
			[0:]byte pclhasopnd,			!1+ has operand; 2=name defines a name; 3=defines local/param; 4=imported func
			[0:]byte pclhastype,
			[0:]byte pclextra =

!                          Op T X
	(kzero=0,			$,	0,0,0),	! (0 0)
	(knop,				$,	0,0,0),	! (0 0)
	(kstop,				$,	0,0,0),	! (1 0)	Stop Xa
	(kcomment,			$,	1,0,0),	! (0 0)	Comment A (a string)

	(kimportdll,		$,	1,0,0),	! (0 0) A Use external dll A
	(kimportlib,		$,	1,0,0),	! (0 0) A Use external lib A
!	(kimport,			$,	1,0,0),	! (0 0) A Import symbol A
!	(kexport,			$,	1,0,0),	! (0 0) A Export symbol A (or declare using ::)
	(kistatic,			$,  2,1,0),	! (0 0) (A,t) Define idata label (must be followed by correct kdata ops)
	(kzstatic,			$,	2,1,0),	! (0 0) (A,t) Define zdata labe and reserve sufficient space
	(kequiv,			$,	2,0,0),	! (0 0) (A) Define equivalence to this var?

!	(kprocdef,			$,	2,1,1),	! (0 0) (A,t, isrts) Define proc A, of given return type
	(kprocdef,			$,	2,1,0),	! (0 0) (A,t) Define proc A, of given return type
	(kprocentry,		$,	0,0,0),	! (0 0)
	(kendproc,			$,	0,0,0),	! (0 0)
	(kendprogram,		$,	0,0,0),	! (0 0)
	(kextproc,			$,	4,1,0),	! (0 0) (t)
	(kextvar,			$,	4,1,0),	! (0 0) (t)
	(kextparam,			$,	0,1,0),	! (0 0) (t) Anonymous params
	(kextvariadics,		$,	0,1,0),	! (0 0) (t) ... parameter
	(kendextproc,		$,	0,0,0),	! (0 0)
	(kthreadedproc,		$,	2,1,0),	! (0 0) (A,t) Define proc A, of given return type
	(kprocrts,			$,	2,1,0),	! (0 0) (A,t) Alternate to procdef but for rts functions

	(klocal,			$,	3,1,0),	! (0 0) (A,t) Define local A of type t
	(kparam,			$,	3,1,0),	! (0 0) (A,t) Define param A of type t
	(klabel,			$,	1,0,0),	! (0 0) (L) Define numbered label L
	(klabelname,		$,	2,0,0),	! (0 0) (A) Define named label

	(kpush,				$,	1,1,0),	! (0 1) (X,t)	Push operand X of type t; X is anything pushable
	(kpop,				$,	1,1,0),	! (1 0) (L,t)	pop to label X
	(kstore,			$,	1,1,0),	! (1 1) (L,t)	store to label X but stays on the stack
	(kpushnc,			$,	1,1,0),	! (0 1) (X,t)	Push optimised for blocks (no copying)
	(kpushlabel,		$,	1,0,0),	! (0 1) (L)		Push address of label L

	(kopnd,				$,	1,0,0),	! (0 0) (X) Define auxiliary operand X (not sure about extra stuff yet)
	(ktype,				$,	0,1,0),	! (0 0) (t) Define auxiliary type t
	(kduplstack,		$,	0,1,0),	! (1 2) (t) Ya':=Xa; X stays on stack
	(kswapstack,		$,	0,1,1),	! (1 1) (t,N) Swap Xa with element +N away

	(kpushptroff,		$,	0,1,2),	! (2 1) (t,scale,offset) Xa:=(Xb+Ya*scale+offset)^ using given type
	(kpopptroff,		$,	0,1,2),	! (3 0) (t,scale,offset) (Yb+Za*scale+offset)^:=Xc
	(kstoreptroff,		$,	0,1,2),	! (3 1) (t,scale,offset) (Yb+Za*scale+offset)^:=Xc, Xc stays as Xa
!	(kindex,			$,	0,0,0),	! (0 0)
!	(kpopindex,			$,	0,0,0),	! (0 0)
!	(kstoreindex,		$,	0,0,0),	! (0 0)

	(kpushptr,			$,	0,1,0),	! (1 1) Xa:=Xa^
	(kpopptr,			$,	0,1,0),	! (2 0) Ya^:=Xb
	(kstoreptr,			$,	0,1,0),	! (2 1) Ya^:=Xb, keep Xb on stack as Xa

	(kdotindex,			$,	0,1,0),	! (2 1)	Xa:=Xb.[Ya]
	(kpopdotindex,		$,	0,1,0),	! (3 0) Yb^.[Za]:=Xc
	(kstoredotindex,	$,	0,1,0),	! (3 1) Yb^.[Za]:=Xc, keep Xc as Xa

	(kdotslice,			$,	0,1,0),	! (3 1) Xa:=Xc.[Yb..Za]
	(kpopdotslice,		$,	0,1,0),	! (4 0) Yc^.[Zb..Wa]:=Xd
	(kstoredotslice,	$,	0,1,0),	! (4 1) Yc^.[Zb..Wa]:=Xd, keep

!	(kfree,				$,	0,1,0),	! (1 0)	Pop Xa
	(kpopstack,			$,	0,1,0),	! (1 0)	Pop Xa
	(keval,				$,	0,1,0),	! (1 0) Evaluate Xa [load to an actual register], then pop

	(kcallproc,			$,	1,0,0),	! (n 0) (A) Call &A with nargs, then pop args
	(kcallprocptr,		$,	0,0,0),	! (n+1 0) Call Xa with nargs, then pop args
	(kretproc,			$,	0,0,0),	! (0 0) Return from proc

	(kcallfn,			$,	1,1,0),	! (n 1) (A, t), Call &A, then pop args, leave retval
	(kcallfnptr,		$,	0,1,0),	! (n+1 1) (t) Call Xa, then pops args, leave retval
	(kretfn,			$,	0,1,0),	! (0 0) (t) Return from function with Xa=retval

	(kjump,				$,	1,0,0),	! (0 0) (L) goto L
	(kjumpptr,			$,	0,0,0),	! (1 0) goto Xa

	(kjumpeq,			$,	1,1,0),	! (2 0) (L,t) goto L when Xb = Ya
	(kjumpne,			$,	1,1,0),	! (2 0) (L,t) goto L when <>
	(kjumplt,			$,	1,1,0),	! (2 0) (L,t) goto L when Xb < Ya
	(kjumple,			$,	1,1,0),	! (2 0) (L,t) goto L when <=
	(kjumpge,			$,	1,1,0),	! (2 0) (L,t) goto L when >=
	(kjumpgt,			$,	1,1,0),	! (2 0) (L) goto L when >

	(kjumptrue,			$,	1,1,0),	! (1 0) (L,t) goto L when Xa is true
	(kjumpfalse,		$,	1,1,0),	! (1 0) (L,t) goto L when Xa is false

	(kjumpinrange,		$,	1,1,0),	! (3 0) (L,t) goto L when Xc in Yb..Za
	(kjumpnotinrange,	$,	1,1,0),	! (3 0) (L,t) goto L when Xc not in Yb..Za

	(ksetjumpeq,		$,	1,1,0),	! (2 1) (L,t) goto L when Xb=Ya; pop Y, leave Xa
	(ksetjumpeqx,		$,	1,1,0),	! (0 0) (L,t) goto L when Xb=Ya; pop both
	(ksetjumpne,		$,	1,1,0),	! (0 0) (L,t) goto L when Xb<>Ya; pop both

!	(ksetcc,			$,	0,1,1),	! (2 1) (t,cc) Xa:=Xb cc Ya
	(kseteq,			$,	0,1,0),	! (2 1) (t) Xa:=Xb = Ya
	(ksetne,			$,	0,1,0),	! (2 1) (t) Xa:=Xb <> Ya
	(ksetlt,			$,	0,1,0),	! (2 1) (t) Xa:=Xb < Ya
	(ksetle,			$,	0,1,0),	! (2 1) (t) Xa:=Xb <= Ya
	(ksetge,			$,	0,1,0),	! (2 1) (t) Xa:=Xb >= Ya
	(ksetgt,			$,	0,1,0),	! (2 1) (t) Xa:=Xb > Ya

	(kcasejumpeq,		$,	1,1,1),	! (2 1) (L,t) goto L when Xb=Ya; pop Ya, leave Xa

!	(kselectcc,			$,	0,1,1),	! (4 1) (t,cc) Xa:=(Zb op Wa|Xd|Yc)
	(kselecteq,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb = Wa|Xd|Yc)
	(kselectne,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb <> Wa|Xd|Yc)
	(kselectlt,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb < Wa|Xd|Yc)
	(kselectle,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb <= Wa|Xd|Yc)
	(kselectge,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb >= Wa|Xd|Yc)
	(kselectgt,			$,	0,1,0),	! (4 1) (t) Xa:=(Zb > Wa|Xd|Yc)

	(kselecttrue,		$,	0,1,0),	! (3 1) (t) Xa:=(Za|Xc|Yb)

	(kto,				$,	1,0,0),	! (0 0) (L)(B,t) --B (aux); goto L when B<>0 

	(kforup,			$,	1,1,1),	! (0 0) (L,t,n)(B,t)(C,t) B+:=n; goto L when B<=C
	(kfordown,			$,	1,1,1),	! (0 0) (L,t,n)(B,t)(C,t) B-:=n; goto L when B>=C

	(kswap,				$,	0,1,0),	! (2 0) (t) swap(Xb^,Yb^)

	(kmakeslice,		$,	0,1,0),	! (2 1) (t) Xa:=slice(Xb, Ya)

	(kswitch,			$,	1,0,2),	! (1 0) (L,x,y)(B) L=jumptab; B=elselab; x/y=min/max values
	(kswitchlabel,		$,	1,0,0),	! (0 0) (L) jumptable entry
	(kendswitch,		$,	0,0,0),	! (0 0)	Mark end of switch jumptable

	(kclear,			$,	0,1,0),	! (1 0) (t) Clear Xa^

	(kcsegment,			$,	0,0,0),	! (0 0) Switch to that segment (usually automatic, so these override)
	(kisegment,			$,	0,0,0),	! (0 0) ..
	(kzsegment,			$,	0,0,0),	! (0 0) ..
	(krosegment,		$,	0,0,0),	! (0 0) ..

	(kdata,				$,	1,1,0),	! (0 0) (X,t) Define inline data of various kinds

	(kdb,				$,	1,0,0),	! (0 0) (X) Define a u8 data value
	(kdw,				$,	1,0,0),	! (0 0) (X) u16 value: ...
	(kdd,				$,	1,0,0),	! (0 0) (X) u32 value: u32/i32/r32, depends on operand
	(kdq,				$,	1,0,0),	! (0 0) (X) u64 value: u64/i64/r64/string/addr/label, depends on operan
	(kdstring,			$,	1,0,0),	! (0 0) (S) u8 sequence from string literal (no terminator)
	(kdstringz,			$,	1,0,0),	! (0 0) (S) u8 sequence from string literal, nul added

	(kreserve,			$,	0,1,0),	! (0 0) (t) Reserve space big enough for t
	(kassem,			$,	1,0,0),	! (0 0) to be worked out....

	(kadd,				$,	0,1,0),	! (2 1) (t) Xa := Xb + Ya
	(ksub,				$,	0,1,0),	! (2 1) (t)
	(kmul,				$,	0,1,0),	! (2 1) (t)
	(kdiv,				$,	0,1,0),	! (2 1) (t)
	(kidiv,				$,	0,1,0),	! (2 1) (t)
	(kirem,				$,	0,1,0),	! (2 1) (t)
	(kiand,				$,	0,1,0),	! (2 1) (t)
	(kior,				$,	0,1,0),	! (2 1) (t)
	(kixor,				$,	0,1,0),	! (2 1) (t)
	(kshl,				$,	0,1,0),	! (2 1) (t)
	(kshr,				$,	0,1,0),	! (2 1) (t)
	(kin,				$,	0,1,0),	! (2 1) (t)
	(knotin,			$,	0,1,0),	! (2 1) (t)
	(kmin,				$,	0,1,0),	! (2 1) (t)
	(kmax,				$,	0,1,0),	! (2 1) (t)
	(keq,				$,	0,1,0),	! (2 1) (t)
	(kne,				$,	0,1,0),	! (2 1) (t)
	(klt,				$,	0,1,0),	! (2 1) (t)
	(kle,				$,	0,1,0),	! (2 1) (t)
	(kge,				$,	0,1,0),	! (2 1) (t)
	(kgt,				$,	0,1,0),	! (2 1) (t)
	(ksame,				$,	0,1,0),	! (2 1) (t)
	(kandl,				$,	0,1,0),	! (2 1) (t)
	(korl,				$,	0,1,0),	! (2 1) (t)
	(kaddrefoff,		$,	0,1,2),	! (2 1) (t,scale,offset) Xa := Xb + Ya*scale + offset
	(ksubrefoff,		$,	0,1,2),	! (2 1) (t,scale,offset) Xa := Xb - Ya*scale + offset
	(ksubref,			$,	0,1,1),	! (2 1) (t,scale) Xa := (Xb - Ya)/scale

	(kneg,				$,	0,1,0),	! (1 1) (t) Xa:=-Xa
	(kabs,				$,	0,1,0),	! (1 1) (t)
	(kinot,				$,	0,1,0),	! (1 1) (t)
	(knotl,				$,	0,1,0),	! (1 1) (t)
	(kistruel,			$,	0,1,0),	! (1 1) (t)
	(ksqr,				$,	0,1,0),	! (1 1) (t)

	(ksqrt,				$,	0,1,0),	! (1 1) (t) Xa:=sqrt(Xa)
	(ksin,				$,	0,1,0),	! (1 1) (t)
	(kcos,				$,	0,1,0),	! (1 1) (t)
	(ktan,				$,	0,1,0),	! (1 1) (t)
	(kasin,				$,	0,1,0),	! (1 1) (t)
	(kacos,				$,	0,1,0),	! (1 1) (t)
	(katan,				$,	0,1,0),	! (1 1) (t)
	(kln,				$,	0,1,0),	! (1 1) (t)
	(klog,				$,	0,1,0),	! (1 1) (t)
	(kexp,				$,	0,1,0),	! (1 1) (t)
	(kround,			$,	0,1,0),	! (1 1) (t)
	(kfloor,			$,	0,1,0),	! (1 1) (t)
	(kceil,				$,	0,1,0),	! (1 1) (t)
	(kfract,			$,	0,1,0),	! (1 1) (t)
	(ksign,				$,	0,1,0),	! (1 1) (t)
	(katan2,			$,	0,1,0),	! (1 1) (t)
	(kpower,			$,	0,1,0),	! (1 1) (t)
	(kfmod,				$,	0,1,0),	! (1 1) (t)

	(kincr,				$,	0,1,1),	! (1 0) (t,step) Xa^+:=step
	(kdecr,				$,	0,1,1),	! (1 0) (t,step) Xa^-:=step
	(kincrload,			$,	0,1,1),	! (1 1) (t,step) Xa:=(Xa+:=step)^
	(kdecrload,			$,	0,1,1),	! (1 1) (t,step) Xa:=(Xa-:=step)^
	(kloadincr,			$,	0,1,1),	! (1 1) (t,step) Xa:=Xa++^ (difficult to express step)
	(kloaddecr,			$,	0,1,1),	! (1 1) (t,step) Xa:=Xa--^

	(kaddto,			$,	0,1,0),	! (2 0) (t) Xa^ +:= Ya
	(ksubto,			$,	0,1,0),	! (2 0) (t)
	(kmulto,			$,	0,1,0),	! (2 0) (t)
	(kdivto,			$,	0,1,0),	! (2 0) (t)
	(kidivto,			$,	0,1,0),	! (2 0) (t)
	(kiremto,			$,	0,1,0),	! (2 0) (t)
	(kiandto,			$,	0,1,0),	! (2 0) (t)
	(kiorto,			$,	0,1,0),	! (2 0) (t)
	(kixorto,			$,	0,1,0),	! (2 0) (t)
	(kshlto,			$,	0,1,0),	! (2 0) (t)
	(kshrto,			$,	0,1,0),	! (2 0) (t)
	(kminto,			$,	0,1,0),	! (2 0) (t)
	(kmaxto,			$,	0,1,0),	! (2 0) (t)
	(kandlto,			$,	0,1,0),	! (2 0) (t)
	(korlto,			$,	0,1,0),	! (2 0) (t)
	(kaddrefoffto,		$,	0,1,2),	! (2 0) (t,scale,offset) Xa^ +:= Ya
	(ksubrefoffto,		$,	0,1,2),	! (2 0) (t,scale,offset) Xa^ -:= Ya

	(knegto,			$,	0,1,0),	! (1 0) (t) -:=Xa^
	(kabsto,			$,	0,1,0),	! (1 0) (t)
	(kinotto,			$,	0,1,0),	! (1 0) (t)
	(knotlto,			$,	0,1,0),	! (1 0) (t)
	(kistruelto,		$,	0,1,0),	! (1 0) (t)

!for conversions, t is always the current operand type
!u is the new type. However, for conversions involving widening, the result
!must end up at at least 64 bit. So i64->u8 masks to 8 bits, the sign-extends to u64

!	(kconvert,			$,	0,2,1),	! (1 1) (t,u)
	(ktypepun,			$,	0,2,1),	! (1 1) (t,u)

	(ksoftconv,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) ??

	(kwiden,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) Widen int type, from t to wider int u
	(knarrow,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) I think reduces i128/u128 t to i64/u64 u
	(kfloat,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,t) Int u to real t
	(kfix,				$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,t) Real u to int t
	(ktruncate,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) Mask to width of u, but type is widend to i64/u64
	(kfwiden,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) r32 to r64
	(kfnarrow,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) r64 to r32
	(ksofttruncw,		$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) 128 bits to 64
	(kwidenw,			$,	0,2,0),	! (1 1) (t,u) Xa:=cast(Xa,u) 64 bits to 128

!These ones are currently still needed by or all PCL targets

	(kstartmult,		$,	0,0,0),
	(kresetmult,		$,	0,0,0),
	(kendmult,			$,	0,0,0),
	(ksetret,			$,	0,1,0),	! (0 0) (t) Set Xa as return value of type t
!	(ksetretmult,		$,	0,1,2),
	(ksetretmult,		$,	0,0,1), ! (0 0) (n) Set N return values
	(ksetargs,			$,	0,0,2), ! (nargs, nvars)

!these are special ones used reflection

	(kgetnprocs,		$,	0,0,0), ! (0 1) Get number of functions in function table
	(kgetprocname,		$,	0,0,0), ! (1 1) Xa:=Getprocname(Xa) Name of nth function (1-based)
	(kgetprocaddr,		$,	0,0,0), ! (1 1) Xa:=Getprocaddr(Xa) Addr of nth function (1-based)

!ops used internally by M compiler until they can be replaced
!(usually they will be turned into something else, constants etc)
	(klen,				$,	0,0,0),
	(klwb,				$,	0,0,0),
	(kupb,				$,	0,0,0),
	(kbounds,			$,	0,0,0),
	(klenstr,			$,	0,0,0),
	(kbitwidth,			$,	0,0,0),
	(kbytesize,			$,	0,0,0),
	(kbytes,			$,	0,0,0),
	(kminvalue,			$,	0,0,0),
	(kmaxvalue,			$,	0,0,0),
	(ktypestr,			$,	0,0,0),
	(kerror,			$,	0,0,0),
	(karraytoslice,		$,	0,0,0),
	(kichartoslice,		$,	0,0,0),
	(ksofttruncshort,	$,	0,0,0),
	(kcharaxtoichar,	$,	0,0,0),
	(ksliceptr,			$,	0,0,0),

	(klast,				$,	0,0,0),	! (0 0)
end

global tabledata() []ichar rtsnames =
	(rts_rts_unimpl,		$),
	(rts_power_i64,			$),
	(rts_float_u64r64,		$),
	(rts_mul_i128,			$),
	(rts_div_i128,			$),
	(rts_div_u128,			$),
end
=== pc_win64.m 0 0 31/44 ===
export function pcl_genmcl(int optim=0)int=
	fixuppcl()
	genmcl(optim)

	return 1
end

export function pcl_writeasmfile(ichar filename, int optim=0)int=
	pcl_genmcl(optim)

	writeasmfile(filename)
	return 1
end

export function pcl_getasmstring(int optim=0)ichar=
	ref strbuffer asmstr
	pcl_genmcl(optim)
	asmstr:=getmclstr()
	return asmstr.strptr
end

export function pcl_writeexefile(ichar filename, int optim=0)int=
	pcl_genmcl(optim)
	return writeexefile(filename,optim,0)
end

export function pcl_writelibfile(ichar filename, int optim=0)int=
	pcl_genmcl(optim)

	genss()
	writemcx(filename)

	return 1
end

!export function pcl_writeobjfile(ichar filename, int optim=0)int=
!	return writeexefile(filename,optim,1)
!end

function writeexefile(ichar filename, int optim=0, int gendll=0)int=
	[300]char asmfilename
	[300]char str

	genss()

	initsectiontable()

	genexe(nil, filename, gendll)
	writeexe(filename, gendll)
	return 1
end

!function oldwriteexefile(ichar filename, int optim=0, int gendll=0)int=
!	[300]char asmfilename
!	[300]char str
!
!	strcpy(asmfilename, changeext(filename,"asm"))
!	if not pcl_writeasmfile(asmfilename,optim) then return 0 fi
!
!	fprint @str,"/m/aa # #",&.asmfilename,(gendll|"-dll"|"-exe")
!
!	if system(str)=0 then
!		return 1
!	else
!		return 0
!	fi
!end

proc writeasmfile(ichar filename)=
!already generated as mcl
	ref strbuffer asmstr
	asmstr:=getmclstr()
	writegsfile(filename,asmstr)
	gs_free(asmstr)
end

export function pcl_readrts(ichar filename)int=
	if maxuserlabel then		!probably via parsing
		labelnooffset:=maxuserlabel
	else						!probably via API
		labelnooffset:=++plabelno
	fi
	if not parse_readrts() then
		println "No RTS file found"
		return 0
	fi
	return 1
end

export proc pcl_endprog(int fixup=1, dorts=1)=

	if dorts then
		pcl_readrts("rts.pcl")
	fi

	pcl_end(fixup)
end

export proc pcl_showss(ichar filename,int fexe)=
	ref strbuffer ssstr

	gs_init(dest)
	ssstr:=writessdata(fexe)
	writegsfile(filename,ssstr)
end

export proc pcl_writeclangfile(ichar filename)=
!	genclang(filename)
end

export function pcl_runlibfile(ichar filename, int optim=0, fshowmx, cmdskip)int=
	ref librec plib

!	REF BYTE PP
!	PP:=READFILE("PI.MX")
!	IF PP=NIL THEN STOP FI
!	PLIB:=LOADMEMMCB("DUMMYNAME",PP)

	pcl_genmcl(optim)

	genss()
	plib:=writememlib(filename)

	loadmemmcu(plib)

	fixuplib(plib)

	if fshowmx then
		initlogfile()
		showlibs()
		closelogfile()
	else
		runprogram(plib, cmdskip)
	fi
	return 1
end

=== pc_writeexe.m 0 0 32/44 ===
!Create .exe file from SS-data (code, data, reloc and symbol tables)
!Call order::
! initsectiontable()
! genexe()
! writeexe(filename)

[maxplibfile]int64 libinsttable
[maxplibfile]ichar libinstnames
[maxplibfile]int libnotable			!index into dlltable

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
!const filealign = 32
const sectionalign = 4096
const exe_imagebase = 0x40'0000

const dll_imagebase = 0x1000'0000
!const dll_imagebase = 0x1'0000'0000

!const dll_imagebase = 0x6624'0000
global int imagebase

int imagesize
int filesize
ref[]int64 thunktable				!point into code segment
int fileiatoffset
int fileiatsize
psymbol stentrypoint				!symbol to be the entry point
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

!global const maxlibs = 50
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

global proc writeexe(ichar outfile,int dodll)=
	imagefileheader header
	optionalheader optheader
	int offset,i
	int64 aa

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

!	println =filesize, =dataptr-datastart			!these should match

!	if fverbose>=2 then
!		println "Writing file:",outfile
!	fi

	if writefile(outfile,datastart,dataptr-datastart)=0 then
		println "Error writing exe file (possibly still running)"
		stop 1
	fi
end

global proc genexe(ichar entrypoint, outfile, int dodll)=
!manipulate the ss data to fill in all the details needed for exe format

!CPL =OUTFILE
	dllfilename:=extractfile(outfile)
!CPL =DLLFILENAME
	isdll:=dodll

	setuplibfiles()

	imagebase:=(isdll|dll_imagebase|exe_imagebase)

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

!	for i to nplibfiles when plibtypes[i]='D' do
	for i to nplibfiles do
		if plibtypes[i]='L' then
CPL =PLIBFILES[I],PLIBTYPES[I]:"c"
			axerror("Can't use LIB files with EXE")
		fi
		strcpy(&.filename,plibfiles[i])
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
!	sectiontable[zsect].rawsize:=roundtoblock(ss_zdatalen,filealign)
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
		axerror("Can't find external function")
	od

!found in search lib n
	if libno:=libnotable[n] then			!already added this library
		++dlltable[libno].nprocs
		return name
	fi

!first use of this lib
	strcpy(&.str,plibfiles[n])
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
	ichar name, libname, dname, basename

	for i:=1 to ss_nsymbols do
!DNAME:="XXX"
		d:=ss_symboltable[i]
		dname:=d.name
		if d.isimported then
			if nimports>=maximports then axerror("genexe: Too many imports") fi
			++nimports
!			name:=extractlibname(dname,libno,d.moduleno)

!CPL "IMPTABLE",DNAME

			name:=extractlibname(dname,libno,1)
			importtable[nimports].libno:=libno			!0 if no lib
			importtable[nimports].name:=name				!original, or 2nd part of lib.name
			importtable[nimports].def:=d

			d.importindex:=nimports
		elsif d.isexported then
!CPL "EXPORTED",D.NAME,=DNAME,MM.MODULETOSUB[D.MODULENO]
!CPL "EXPORTED",D.NAME,=DNAME
			basename:=getbasename(dname)
			if userentrypoint then
				if eqstring(basename,userentrypoint) then
					stentrypoint:=d
				fi
			else
				if eqstring(basename,"main") and not isdll then
					stentrypoint:=d
				elsif eqstring(basename,"start") and not isdll then
					stentrypoint2:=d
				elsif eqstring(basename,"dllmain") and isdll then
					stentrypoint:=d
				fi
			fi

			if nexports>=maxexports then axerror("gendll: Too many exports") fi
			++nexports

			exporttable[nexports].def:=d
			exporttable[nexports].name:=getbasename(dname)
!CPL "EXPORTED",EXPORTTABLE[NEXPORTS].NAME

!OS_GETCH()

		fi
	od
end

proc relocdata(ref sectionrec s)=
	ref sectionrec u
	ref relocrec r
	ref byte p
	ref word32 p32
	ref word64 p64
	psymbol d
	int offset,index,thunkoffset,iatoffset

	p:=bufferelemptr(s.data,0)
	r:=s.relocs

	while r do
		d:=ss_symboltable[r.stindex]
		index:=d.importindex				!into importtable
		thunkoffset:=importtable[index].thunkoffset
!CPL "RELOCDATA",D.NAME,INDEX,THUNKOFFSET,=D.ISIMPORTED
!CPL "RELOCDATA",D.NAME,=INDEX

		case r.reloctype
		when rel32_rel then
			if not d.isimported then
				axerror("rel32/not imported")
			fi
!CPL "RELOC/REL32REL",D.NAME
			(ref word32(p+r.offset)^:=thunkoffset-r.offset-4)
!
		when addr32_rel, addr64_rel then				!for addr64, just leave top half zero
			if d.isimported then
!CPL "IMPORTED",D.NAME
				(ref word32(p+r.offset)^:=imagebase+thunkoffset+sectiontable[csect].virtoffset)
			else
				u:=nil
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				else
					CPL D.NAME,D.SEGMENT
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
	ref sectionrec u
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
			if d.isimported then
			else
				case d.segment
				when zdata_seg then u:=&sectiontable[zsect]
				when idata_seg then u:=&sectiontable[dsect]
				when code_seg then u:=&sectiontable[csect]
				esac

				newbasereloc(u.virtoffset+r.offset, r.reloctype)

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
				axerror("Entry point not found: main or start")
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

	int64 aa
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

!proc addsearchlib(ichar name) =
!	name:=changeext(name,"")
!!CPL "ADD SEARRCH"
!
!	for i to npsearchlibs do
!		if eqstring(psearchlibs[i],name) then return fi
!	od
!
!	if npsearchlibs>=maxpsearchlibs then
!		axerror("Too many LIB files")
!	fi
!!CPL "ADDSEARCH",name
!!	searchlibs[++nsearchlibs]:=pcm_copyheapstring(changeext(name,""))
!	psearchlibs[++npsearchlibs]:=pcm_copyheapstring(name)
!end

proc setuplibfiles=
!collare external libs from multiple sources:
! 1  Fixed set of libraries
! 2  .dll provided on command line
! 3  CCLIB names
! 4 Set of IMPORTDLL names
	static []ichar stdlibs = ("msvcrt","gdi32","user32","kernel32")

	for i to stdlibs.len do
		pcl_addlibfile(stdlibs[i], 'D')
	od

!	npsearchlibs:=0
!
!	psearchlibs[1]:="msvcrt"
!	psearchlibs[2]:="gdi32"
!	psearchlibs[3]:="user32"
!	psearchlibs[4]:="kernel32"
!	npsearchlibs:=4	
!
!	for i to nplibfiles do addsearchlib(plibfiles[i]) od
!	for i to npdllnametable when pdllnametable[i]^<>'$' do
!		addsearchlib(pdllnametable[i])
!	od
!
!CPL "SEARCH LIBS:"
!FOR I TO NPSEARCHLIBS DO
!	CPL PSEARCHLIBS[I]
!OD

end

proc writeexporttable(ref byte pstart)=
	const maxexports=2000
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

function getexporttablesize:int=
	int size

	size:=exportdirrec.bytes
	size+:=nexports*4			!address table entries
	size+:=nexports*4			!name pointers
	size+:=nexports*2			!ordinal table

CPL =DLLFILENAME
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
		if blockcounts[i].odd then
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
!
			q:=q.nextitem
		od
		if blockpadding[i] then p16++^:=0 fi

		p32:=cast(p16)

	od
end

proc sortexports([]int &sortindex)=
!sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
	psymbol d,e
!First, store 1..nexports into sortindex
	for i to nexports do
		sortindex[i]:=i
	od

!do bubble sort for now
	int swapped

!CPL "SORTEXPORTS"
!
!CPL =NEXPORTS
!RETURN

!		for i:=1 to nexports do
!
!			fprintln """#"",",exporttable[i].def.name
!		od

	repeat
		swapped:=0
		for i:=1 to nexports-1 do

			d:=exporttable[sortindex[i]].def
			e:=exporttable[sortindex[i+1]].def

!			if strcmp(d.name, e.name)>0 then
			if strcmp(getbasename(d.name), getbasename(e.name))>0 then

				swapped:=1
				swap(sortindex[i], sortindex[i+1])
			fi
		od
	until not swapped

end

function getsectionno(int segment)int=
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
!CPL =DIROFFSET
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
	ref int64 paddr,pname
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

	if isdll then
		writeexporttable(ref byte(pimpdir)+exportdiroffset)
		writebasereloctable(ref byte(pimpdir)+blockdiroffset)
	fi

!write the thunk table
	ref byte thunkptr,codebase
	int thunkaddr
	thunkptr:=bufferelemptr(ss_code,thunkoffset)
	codebase:=bufferelemptr(ss_code,0)


!-----------------------------------------------
!	for i to nimports do
!		importtable[i].thunkoffset:=thunkptr-codebase
!		thunkptr++^:=0x48
!		thunkptr++^:=0xFF
!		thunkptr++^:=0x24
!		thunkptr++^:=0x25
!		thunkaddr:=imagebase+importtable[i].iatoffset
!		(ref int32(thunkptr)^:=thunkaddr)
!
!		thunkptr+:=4
!	od
!-----------------------------------------------
	for i to nimports do
		importtable[i].thunkoffset:=thunkptr-codebase
!INT IMOFFSET:=THUNK

		thunkptr++^:=0xFF
		thunkptr++^:=0x25
!CPL "THUNK",IMPORTTABLE[I].NAME,=IMPORTTABLE[I].IATOFFSET:"H"
!CPL "THUNK2",=CODEBASE
!CPL =sectiontable[csect].virtoffset,=THUNKOFFSET

!INT IATOFFSET:=IMPORTTABLE[I].IATOFFSET

!		thunkaddr:=imagebase+importtable[i].iatoffset
!		thunkaddr:=importtable[i].iatoffset-(sectiontable[csect].virtoffset+thunkoffset+6)
		thunkaddr:=importtable[i].iatoffset-(sectiontable[csect].virtoffset+
			importtable[i].thunkoffset+6)

!		thunkaddr:=importtable[i].iatoffset
		(ref int32(thunkptr)^:=thunkaddr)
		thunkptr+:=4
		thunkptr++^:=0x90
		thunkptr++^:=0x90
	od
!-----------------------------------------------
end
=== pc_writemx.m 0 0 33/44 ===
!Translate SS data directly into MCB block, then write as mx/ml file

int nsymimports=0, nsymexports=0

ref dbuffer dest

global proc writemcx(ichar filename)=
	int n

!IF PSTENTRYPOINT THEN
!CPL "WRITEMCX",=PSTENTRYPOINT.NAME,=PMAINLIB
!FI
!
	ss_zdatalen:=roundtoblock(ss_zdatalen, 8)

	roundsegment(ss_code,8,0x90)
	roundsegment(ss_idata,8,0)

	dest:=buffercreate()

	genword32(mcxsig)

	genbyte(version_dir)
	genstring("0.1234")

	scansymbols()
	writerelocs()

	genbyte(zdata_dir)
	genword32(ss_zdatalen)

	genbyte(code_dir)
	genword32(n:=bufferlength(ss_code))
	genblock(bufferelemptr(ss_code,0), n)

	genbyte(idata_dir)
	genword32(n:=bufferlength(ss_idata))

	genblock(bufferelemptr(ss_idata,0), n)

	int ndlls:=0, nlibs:=0
	for i to nplibfiles do
		if plibtypes[i]='D' then ++ndlls else ++nlibs fi
	od

	genbyte(dlls_dir)
	genword32(ndlls)
	for i to nplibfiles when plibtypes[i]='D' do
		genstring(plibfiles[i])
	od

	genbyte(libs_dir)
	genword32(nlibs)
	for i to nplibfiles when plibtypes[i]='L' do
		genstring(plibfiles[i])
	od

	writesymbols()

	genbyte(end_dir)

	writefile(filename, dest.pstart, dest.pcurr-dest.pstart)
end

proc writerelocs=
	ref relocrec oldr
	mcxreloc newr
	int n,count
	psymbol d
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	genbyte(reloc_dir)
	genword32(n:=ss_nidatarelocs+ss_ncoderelocs)

	count:=0

	for i in code_seg..idata_seg do
		oldr:=(i=code_seg|ss_idatarelocs|ss_coderelocs)

		while oldr, oldr:=oldr.nextreloc do
			++count
			clear newr

			newr.offset:=oldr.offset
			newr.segment:=(i=code_seg|idata_seg|code_seg)

			d:=ss_symboltable[oldr.stindex]

			case oldr.reloctype
			when rel32_rel then
				if d.isimported then
					newr.stindex:=d.impindex
					newr.reloctype:=imprel32_rel
				else
					axerror("rel32/rel not imported")
				fi
			when addr32_rel, addr64_rel then
				if d.isimported then
					newr.reloctype:=(oldr.reloctype=addr32_rel|impabs32_rel|impabs64_rel)
					newr.stindex:=d.impindex
				else
					if oldr.reloctype=addr32_rel then
						newr.reloctype:=locabs32_rel
					else
						newr.reloctype:=locabs64_rel
					fi
					newr.targetsegment:=d.segment
				fi
			else
				axerror("reloc?")
			esac

			genblock(&newr, newr.bytes)

		od
	od
end

proc scansymbols=
	psymbol d
	for i:=1 to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.isexported then d.expindex:=++nsymexports fi
		if d.isimported then d.impindex:=++nsymimports fi
	od
end

proc writesymbols=
	psymbol d
!	u64 epoffset:=-1
	int n
	ichar name

	genbyte(importsymbols_dir)
	genword32(nsymimports)

	for i to ss_nsymbols when ss_symboltable[i].impindex do
		genstring(ss_symboltable[i].name)
	od

	genbyte(exportsymbols_dir)
	genword32(nsymexports)

	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then

			if d=pstentrypoint and plibmode then
				name:=d.name
!CPL "ENTRY POINT:",NAME,=D.OFFSET
			else
				name:=getbasename(d.name)
			fi
!			if epoffset=-1 and (eqstring(name,"start") or eqstring(name,"main")) then
!				epoffset:=d.offset
!			fi
			genstring(name)

		fi
	od

	genbyte(exportsegs_dir)
	genword32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genbyte(d.segment)
		fi
	od

	genbyte(exportoffsets_dir)
	genword32(nsymexports)
	for i to ss_nsymbols do
		d:=ss_symboltable[i]
		if d.expindex then
			genword32(d.offset)
		fi
	od

	genbyte(entry_dir)		!must be present; writes 0xFFFFFFFF when no entry point
	if pstentrypoint then
		genword32(pstentrypoint.offset)
	else
		genword32(0xFFFF'FFFF)
	fi
end

proc roundsegment(ref dbuffer p, int align, value)=
	int length:=bufferlength(p)
	int newlength:=roundtoblock(length, align)

	buffercheck(p, align)

	to newlength-length do
		p.pcurr++^:=value
	od
end

proc genbyte(int x)=
	buffercheck(dest,1)
	dest.pcurr++^:=x
end

proc genword32(int x)=
	buffercheck(dest,4)
	dest.pcurr32++^:=x
end

proc genstring(ichar s)=
	genblock(s, strlen(s)+1)
end

proc genblock(ref void p, int length)=
	buffercheck(dest,length)
	memcpy(dest.pcurr, p, length)
	dest.pcurr+:=length
end
=== pc_writess.m 0 0 34/44 ===
global function writessdata(int fexe)ref strbuffer=
	gs_init(dest)
	showssdata(fexe)

	gs_line(dest)
	return dest
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

proc showsectiondata(ref sectionrec d)=
int i,k,length,bb
	[128]char str,str2
	ref byte p

	gs_str(dest,"proc Section ")
	gs_str(dest,d.name)
	gs_str(dest," Size:")
	gs_strint(dest,d.virtsize)
	gs_line(dest)
	gs_line(dest)

	k:=0
	if d.segtype<>impdata_seg then
		p:=bufferelemptr(d.data,0)
	else
		p:=d.bytedata
	fi
	length:=d.virtsize

	str[1]:=0

	ref byte baseaddr:=cast(imagebase+d.virtoffset)

	print @&.str2,baseaddr:"Z8H",,": "

	gs_str(dest,&.str2)

	for i:=1 to length do
		bb:=p++^
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

	length:=p.virtsize
	codestart:=codeptr:=bufferelemptr(p.data,0)
	codeend:=codeptr+length

	ref byte baseaddr:=cast(imagebase+p.virtoffset)

	while codeptr<codeend do
		offset:=codeptr-codestart
		s:=decodeinstr(codeptr,baseaddr+offset)
		exit when s=nil

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

		gs_str(dest,"Reloc: ")
		gs_str(dest,relocnames[r.reloctype])
		gs_str(dest," Offset: ")
		gs_strint(dest,r.offset)
		gs_str(dest," ST Index: ")
		gs_strint(dest,r.stindex)
		gs_str(dest," ")
		gs_str(dest,ss_symboltable[r.stindex].name)
		gs_line(dest)

		r:=r.nextreloc
	od
	gs_line(dest)

end

proc gs_value(ichar caption, int64 value)=
	[256]char str

	strcpy(&.str,caption)
	strcat(&.str,":")
	ipadstr(&.str,20)
	gs_str(dest,&.str)

	fprint @&.str,"0x# #",value:"H",value
	gs_strln(dest,&.str)
end

proc showsymboltable2=

	gs_strln(dest,"Proc Symbol Table")
	int i
	for i:=1 to ss_nsymbols do
		gs_strint(dest,i)
		gs_str(dest,": ")
		gs_strln(dest,ss_symboltable[i].name)
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

=== rundecls.m 0 0 35/44 ===
!Declarations for M-Code scheme
!Terms:
! MCU		MCode Unit, binary code/data/imports/relocs for whole program (LIBREC)
! MCB		MCU rendered to flat data block, written out as .mx/.ml file
! MCX		MCU with allocations, imports and fixups done to make it ready to run
! LIB		Informal reference to MCU or MCX in-memory data; or to MX/ML file
!
! MCU is created from SS data (which normally was used to generate EXE)
! An MCU block is either converted to MCB which is then send to a file;
! or or it directly fixed up into MCX to allow immediate execution

!single byte tags in mcx file

global const mcxsig = 'MCX\e'

global tabledata() [0:]ichar mcxdirnames =
	(pad_dir = 0,		$),		! nothing follows except next tag; for padding/alignment
	(version_dir,		$),		! STR string follows with version code (stringz)
	(code_dir,			$),		! N(u32) then N bytes of code data
	(idata_dir,			$),		! N(u32) then N bytes init data
	(zdata_dir,			$),		! N(u32) (no data follows)
	(reloc_dir,			$),		! N(u32) then N records follow
	(dlls_dir,			$),		! N(u32) then N STR items, the DLL base names
	(libs_dir,			$),		! N(u32) then N STR items, the MCX base names
	(importsymbols_dir,	$),		! N(u32) then N STR items, the imported names
	(exportsymbols_dir,	$),		! N(u32) then N STR items, the exported names
	(exportsegs_dir,	$),		! N(u32) then N u8 items, each is a segment code
	(exportoffsets_dir,	$),		! N(u32) then N u32 items, each an offset in the segment
	(entry_dir,			$),		! N(u32) N is a byte offset within code segment for entry point
	(end_dir,			$),		! nothing follows; end of file
end

!Relocation codes

global tabledata() [0:]ichar mcxrelocnames =
	(no_rel = 0,		$),

	(locabs32_rel,	"locabs32"),		! add target segment address to 32-bit offset
	(locabs64_rel,	"locabs64"),		! add target segment address to 64-bit offset

	(impabs32_rel,	"impabs32"),		! replace 32-bit 0-field with address of imported symbol
	(impabs64_rel,	"impabs64"),		! replace 64-bit 0-field with address of imported symbol

	(imprel32_rel,	"imprel32"),		! replace 32-bit 0-field with offset of thunk entry for symbol
end

! Explanation of reloc codes
! No reloc
!	For local call/jmp, which are /only/ within code segment, no fixups are needed
!
! Locabs32/Locabs64
!	Reloc field contains offset of location within target segment, plus any
!   constant offset (eg. A+3 has offset of A, plus 3)
!   Baseaddr of that segment is added to that offset
!
! Impabs32/64
!	Reloc field contains any local offset (eg. the 3 in A+3)
!	Symbol index is used (via xlate to global index) to get abs address of symbol
!   Reloc field is replaced with 32/64 bits of that address plus the original value
!
! Imprel32
!	Only used for imported names, and only for CALL. Reloc field may be zeros
!	Reloc field will be changed to relative offset thunk table at end of code segment
!	Thunk table (indexed by local import index), is populated with JMPs to values
!	stored in address table which follows immediately
!	That address table needs to be populated with abs addresses of those imports
!	(Calls to LIB rather than DLL can have CALL offset replaced with direct offset to the
!	imported function, provided top 31 bits of address are zero.)

export tabledata() []ichar segmentnames =
	(code_seg,		"code"),
	(idata_seg,		"idata"),
	(zdata_seg,		"zdata"),
	(rodata_seg,	"rodata"),
	(impdata_seg,	$),
end

!Describe an MCX program loaded into memory

global record librec=
!The first section describes data residing in a file and loaded into these vars
!(code block is loaded directly into an actual executable block, with thunk/
!address table space added)

	ichar version

	int codesize			! bytes in code block, excluding thunk/addr tables
	int idatasize			! bytes in idata block
	int zdatasize			! bytes in zdata block (no data; created on fixup)

	int nrelocs				! size of reloctable
	int	ndlllibs			! size of imported dll names
	int	nlibs				! size of imported libnames
	int nimports			! size of imports/importlib tables
	int nexports			! size of exports/exportsegs/exportoffsets tables

	ref byte codeptr		! executable code block (includes thunk/addr table)
	ref byte idataptr		! initialised data block

	ref[]mcxreloc	reloctable		! table of reloc entries
	ref[]ichar		dllnames		! base names of imported dll files (no extension)
	ref[]ichar		libnames		! base names of imported mcx files (no extension)
	ref[]ichar		importnames		! names of imported symbols
	ref[]ichar		exports			! names of exported symbols
	ref[]byte		exportsegs		! segment where each is located
	ref[]u64		exportoffsets	! offsets within each segment

	u64 entryoffset					! offset within code block where execution will start
									! value of 0xFFFFFFFF (is u32 in file) means not set

!The next section is filled in after loading

	ref byte zdataptr				! zeroed data block
	int codexsize					! bytes in thunk/addr tables that follow code
	ref[]u64		exportaddr		! fully fixed-up addresses of exported symbols (not in file)
	ref[]int16		importxreftable	! map symbol index to global one

	ichar			filespec		!full path
	ichar			libname			!base name of library
	ref byte		entryaddr		!start address (left at nil when entryoffset not set)
	int				libno			!index of this entry in progtable
end

!Reloc item record
! For Locabs-codes, the field contains the offset of the local symbol within target segment
! For Imp-codes, the field contains zero bytes

global record mcxreloc =
	u32		offset			! Offset with .segment of the reloc item
	union
		u16		stindex			! For Imp-codes, index into global import tables
		byte	targetsegment	! For Loc-codes, target segment refered to
	end
	byte	segment			! Segment containing the reloc item
	byte	reloctype		! Reloc code (see enums); also sets size of reloc item
end

global const maxdlls =		20
global const maxlibs =		20
global const maxsymbols =	3000

!Global DLL tables

global [maxdlls]ichar		dllnametable
global [maxdlls]u64			dllinsttable
global int ndlllibs

!Global Prog table

global [maxlibs]ichar		libnametable
global [maxlibs]ref librec	libtable
global [maxlibs]byte		librelocated		!1 when relocated
global [maxlibs]byte		libinitdone			!1 when entry point called
global int nlibs

!Global import tables

global [maxsymbols]ichar	symbolnametable	! Name of symbol
global [maxsymbols]byte		symboldefined	! 1 when fully resolved with address
global [maxsymbols]ref void	symboladdress	! Abs address
global [maxsymbols]int16	symbollibindex	! Lib index where defined
global [maxsymbols]byte		symboldllindex	! DLL index of library where found
global int nsymbols

=== runshow.m 0 0 36/44 ===
int logdest=2

const logfile="rx.log"

ref void logdev		!dest for diagnostics and output of tables

strbuffer destv
ref strbuffer dest=&destv

global proc initlogfile=
	case logdest
	when 2 then
		remove(logfile)
		logdev:=cast(fopen(logfile,"w"))
	when 0,1 then
		logdev:=nil
	esac

end

global proc closelogfile=
	[512]char str

	if logdest=2 then
		fclose(logdev)

		print @&.str,f"\m\ed.bat",logfile

		os_execwait(&.str,1,nil)
	fi
end

global proc showlibs=
!show details of all libs, plus the global data
	showglobals(logdev)

	for i to nlibs do
!	for i to nlibs when libdefined[i] do
		showlib(libtable[i], logdev)
	od
end

global proc showlib(ref librec lib, filehandle logdev)=
	[300]char str
	u64 sig
	int dir,n
	ref byte q
	ref[]ichar names

	gs_init(dest)

	showstrln("-------------------------")
	showstr("LIBFILE: ")
	showstr(lib.libname)
	showstr(" ")
	showstrln(lib.filespec)

	print @str,"Version:",lib.version
	showstrln(str)

	showstrln("")

	fprint @str,"Zdata size: # #", lib.zdatasize, lib.zdataptr
	showstrln(str)

	fprint @str,"Idata size: # #", lib.idatasize, lib.idataptr
	showstrln(str)

	showsectiondata(lib.idataptr, lib.idatasize)

	fprint @str,"Code size: # # Extra:#", lib.codesize, lib.codeptr, lib.codexsize
	showstrln(str)
	showsectioncode(lib.codeptr, lib.codesize,lib.codexsize)
	showrelocs(lib)

	fprint @str,"DLL Libs #", n:=lib.ndlllibs
	showstrln(str)
	shownames(lib.dllnames,n)
	showstrln("")

	fprint @str,"Libs #", n:=lib.nlibs
	showstrln(str)
	shownames(lib.libnames,n)
	showstrln("")

	fprint @str,"Imports #", n:=lib.nimports
	showstrln(str)
	names:=lib.importnames
	for i to n do
		fprint @str,"   #: #", i, names[i]:"20jl"
		showstrln(str)
	od
	showstrln("")
!	shownames(lib.imports,n)

	fprint @str,"Exports #", n:=lib.nexports
	showstrln(str)
	names:=lib.exports
	showstrln("     Name                 Seg      Offset")
	showstrln("--------------------------------------------")

	for i to n do
		fprint @str,"#: # # #",
			i:"3", names[i]:"20jl",
			segmentnames[lib.exportsegs[i]]:"8jl",
			lib.exportoffsets[i]:"8zh"
		showstrln(str)
	od
	showstrln("")


	fprint @str,"Entry point offset:  #",lib.entryoffset
	showstrln(str)
	fprint @str,"Entry point address: #",lib.entryaddr
	showstrln(str)
	showstrln("")

FINISH::

	gs_println(dest,logdev)
end

proc showstr(ichar str)=
	gs_str(dest, str)
end

proc showstrln(ichar str)=
	gs_strln(dest, str)
end

proc showstrint(int a)=
	gs_strint(dest, a)
end

proc shownames(ref[]ichar names, int n)=
	[300]char str
	for i to n do
		fprint @str,"   #: #", i, names[i]
		showstrln(str)
	od
end

proc showrelocs(ref librec lib)=
	[300]char str
	mcxreloc r
	int n:=lib.nrelocs, m
	u64 targetoffset
	ref u64 baseptr64
	ref u32 baseptr32@baseptr64

	fprint @str,"Relocs #", n:=lib.nrelocs
	showstrln(str)

	showstrln("     Type       Seg      Offset    Symbol/Target+Offset")
	showstrln("---------------------------------------------------------")

	for i to n do
		r:=lib.reloctable[i]
		fprint @str,"#: # # ##",
			i:"3", mcxrelocnames[r.reloctype]:"10jl",
			segmentnames[r.segment]:"8jl",
			r.offset:"8zh",,"  "

		m:=strlen(str)
		case r.reloctype
		when locabs32_rel, locabs64_rel then
			case r.segment
			when code_seg then baseptr64:=cast(lib.codeptr+r.offset)
			when idata_seg then baseptr64:=cast(lib.idataptr+r.offset)
			esac
			if r.reloctype=locabs32_rel then
				targetoffset:=baseptr32^
			else
				targetoffset:=baseptr64^
			fi

!CPL =BASEPTR32, =BASEPTR64,=BASEPTR32^, =BASEPTR64^


			print @&.str+m, segmentnames[r.targetsegment]:"6jlt:",,targetoffset:"8zh"
		else
!CPL "RELOC/OTHER",MCXRELOCNAMES[R.RELOCTYPE],R.STINDEX
			print @&.str+m, lib.importnames[r.stindex]
		esac
		showstrln(str)
	od
	showstrln("")
end

proc showsectiondata(ref byte p, int length)=
	int i,k,bb
	[128]char str,str2

	showstr("proc Section ")
	showstr("Idata:")
	showstr(" Size:")
	showstrint( length)
	gs_line(dest)
	gs_line(dest)

	k:=0

	str[1]:=0

	ref byte baseaddr:=nil

	print @&.str2,baseaddr:"Z8H",,": "

	showstr(&.str2)

	for i:=1 to length do
		bb:=p++^
		print @&.str2,bb:"z2H",," "
		showstr(&.str2)

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
					showstr("   ")
					strcat(&.str," ")
				od
			fi
			showstr("	[")
			showstr(&.str)
			showstrln("]")
			k:=0
			str[1]:=0
			baseaddr+:=16
			print @&.str2,baseaddr:"z8h",,": "
			showstr(&.str2)
		fi
	od
	if k=0 then
		gs_line(dest)
	fi

	gs_line(dest)
	if k then gs_line(dest) fi
end

proc showsectioncode(ref byte p, int length, extra)=
	ref byte codeptr,codeend, codeendx,codestart
	int offset
	ichar s
	[16]char str

	showstrln( "proc Section Code")

	codestart:=codeptr:=p
	codeend:=codeptr+length
	codeendx:=codeend+extra

!	ref byte baseaddr:=cast(imagebase+p.virtoffset)
	ref byte baseaddr:=nil

	while codeptr<codeendx do
		if codeptr=codeend then
			showstrln("")
		fi
		offset:=codeptr-codestart
!S:=NIL
		s:=decodeinstr(codeptr,baseaddr+offset)
		exit when s=nil

		print @&.str,offset:"4",," "
		showstr(&.str)

		showstrln(s)
	od

	gs_line(dest)
end

global proc showglobals(filehandle logdev)=
	[300]char str
	[300]char name

	gs_init(dest)
	showstrln("Global Tables\n")

	print @str, "DLLs:",ndlllibs
	showstrln(str)

	for i to ndlllibs do
		print @str,i,,":",dllnametable[i]:"16jl", dllinsttable[i]:"h"
		showstrln(str)
	od
	showstrln("")

	print @str, "LIBs:",nlibs
	showstrln(str)

	for i to nlibs do
		print @str,i,,":",libnametable[i]:"20jl", (librelocated[i]|"Relocated"|"-")
		showstrln(str)
	od
	showstrln("")

	print @str, "Global Symbols:",nsymbols
	showstrln(str)

	showstrln("     Name              Def Address       Lib        Dll")
	showstrln("-----------------------------------------------------------")

	for i to nsymbols do
		strcpy(name,symbolnametable[i])
		if strlen(name)>17 then
			strcat(name,"\n                      ")
		fi
		fprint @str,"#: # # #  # #",
			i:"3",
!			symbolnametable[i]:"17jl",
			&.name:"17jl",
			(symboldefined[i]|"Y"|"-"):"3JL",
			symboladdress[i]:"Z12H",
			(symbollibindex[i]|libnametable[symbollibindex[i]]|"-"):"10jl",
			(symboldllindex[i]|dllnametable[symboldllindex[i]]|"-"):"10jl"
!symboldllindex[i]
		showstrln(str)
	od
	showstrln("")

	gs_println(dest,logdev)
end
=== runmx.m 0 0 37/44 ===

global function loadmx(ichar filename)ref librec plib=
!load mx/ml into mcu then scan for other imported libraries
	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=addlib(name)

	plib:=loadlibfile(filename,newlib)
	loadimports(plib)

	return plib
end

global proc loadmemmcu(ref librec lib)=
	int newlib
	ichar name:=lib.libname

	checknew(name,lib.filespec)

	newlib:=addlib(name)
	libtable[newlib]:=lib

	loadimports(lib)
end

global function loadmemmcb(ichar filename, ref byte p)ref librec plib=
!read from mcb block in memory
!'filename' is just an identifying string

	int newlib
	ichar name

	name:=pcm_copyheapstring(convlcstring(extractbasefile(filename)))
	checknew(name,filename)

	newlib:=addlib(name)
	plib:=readlibfile(filename,p)
	plib.libno:=newlib
	libtable[newlib]:=plib	

	loadimports(plib)
	return plib
end

global proc fixuplib(ref librec lib)=
!do second fixup pass, which is done across global symbols, but then 
!all relocs are done for all libs which are not yet relocated

!	alloclibdata(lib)
!	donewlib(lib)					!update global tables

!global fixups
!	loadimports()

	loaddlls()				!global
	scansymbols()			!global
	dorelocations()			!all libs
end

global proc loadimports(ref librec plib)=
! load imported libs
! do first fixup pass which sets up tables adds imports/exports to global table
! This is done per libs and can be called on imported sub-libs

	ref librec qlib
	ichar name

	for i to plib.nlibs do
		name:=plib.libnames[i]
		int n:=findlib(name)
		if not n then									!not already loaded
			n:=addlib(name)
			println "Loading sublib", name
			qlib:=loadlibfile(addext(name,"ml"),n)		!get mcu
			loadimports(qlib)						!recursive call
		fi
	od

	alloclibdata(plib)
	dosymbols(plib)
end

proc loaddlls=
!load all dll instances
	u64 inst

	for i to ndlllibs when not dllinsttable[i] do
		inst:=os_getdllinst(dllnametable[i])
		if inst=0 then
			error("Can't find DLL: #", dllnametable[i])
		fi
		dllinsttable[i]:=inst
    od
end

function loadlibfile(ichar filename, int libno)ref librec plib=
!read mcb file into memory, process it into a new librec
	ref byte p

	p:=readmxfile(filename)
	if p=nil then
		error("Can't find #",filename)
	fi

	plib:=readlibfile(filename,p)
	plib.libno:=libno
	libtable[libno]:=plib	
end

function xxxloadlibmem(ichar filename, int libno)ref librec plib=
!read mcb file into memory, process it into a new librec
	ref byte p

	p:=readmxfile(filename)
	if p=nil then
		error("Can't find #",filename)
	fi

	plib:=readlibfile(filename,p)
	plib.libno:=libno
	libtable[libno]:=plib	
end

function readlibfile(ichar filespec, ref byte p)ref librec plib=
!p points to an MCB block; scan that into an MCU descriptor (librec)

	librec lib
	u64 sig
	int dir,n,tablesize
	ref byte q

	clear lib

	sig:=readu32(p)
	if sig<>mcxsig then
		println "Bad sig - not MCX file"
		stop 1
	fi

	lib.filespec:=pcm_copyheapstring(filespec)
	lib.libname:=pcm_copyheapstring(extractbasefile(filespec))

	doswitch dir:=readbyte(p)
	when version_dir then
		lib.version:=readstring(p)

	when zdata_dir then
		lib.zdatasize:=readu32(p)
!		lib.zdataptr:=pcm_allocz(lib.zdatasize)

	when idata_dir then
		lib.idatasize:=n:=readu32(p)
		lib.idataptr:=pcm_alloc(n)
		memcpy(lib.idataptr, p, n)	
		p+:=n

	when code_dir then
		lib.codesize:=n:=readu32(p)
		lib.codeptr:=p				!for now, point into file image
		p+:=n

	when dlls_dir then
		lib.ndlllibs:=n:=readu32(p)
		lib.dllnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.dllnames[i]:=readstring(p)
		od

	when libs_dir then
		lib.nlibs:=n:=readu32(p)
		lib.libnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.libnames[i]:=readstring(p)
		od
	when importsymbols_dir then
		lib.nimports:=n:=readu32(p)
		lib.importnames:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.importnames[i]:=readstring(p)
		od

	when exportsymbols_dir then
		lib.nexports:=n:=readu32(p)
		lib.exports:=pcm_alloc(ichar.bytes*n)
		for i to n do
			lib.exports[i]:=readstring(p)
		od

	when exportsegs_dir then
		n:=readu32(p)
		lib.exportsegs:=pcm_alloc(n)
		for i to n do
			lib.exportsegs[i]:=readbyte(p)
		od

	when exportoffsets_dir then
		n:=readu32(p)
		lib.exportoffsets:=pcm_alloc(u64.bytes*n)
		for i to n do
			lib.exportoffsets[i]:=readu32(p)
		od

	when reloc_dir then
		lib.nrelocs:=n:=readu32(p)
		n:=lib.nrelocs*mcxreloc.bytes
		lib.reloctable:=pcm_alloc(n)
		memcpy(lib.reloctable, p, n)
		p+:=n

	when entry_dir then
		lib.entryoffset:=readu32(p)

	when end_dir then
		exit

	when pad_dir then

	else
		println "Unknown directive:",mcxdirnames[dir]
		stop
	end doswitch

	plib:=pcm_alloc(librec.bytes)
	memcpy(plib, &lib, librec.bytes)	

	return plib
end

proc alloclibdata(ref librec lib)=
	int tablesize, n
	ref byte p

	lib.zdataptr:=pcm_allocz(lib.zdatasize)

	tablesize:=lib.nimports*16			!add in thunk table+address table
	n:=lib.codesize

	p:=os_allocexecmem(n+tablesize)		!MUST BE EXECUTABLE MEMORY
	if p=nil then
		error("Can't alloc code memory")
	fi
	memcpy(p, lib.codeptr, n)

	memset(p+n, 0, tablesize)
!	memset(p+n, 0xAA, tablesize)

	lib.codeptr:=p
	lib.codexsize:=tablesize

	lib.exportaddr:=pcm_alloc(u64.bytes*lib.nexports)
	lib.importxreftable:=pcm_alloc(i16.bytes*lib.nimports)

	if lib.entryoffset<>0xFFFF'FFFF then
		lib.entryaddr:=lib.codeptr+lib.entryoffset
	fi
end

function readmxfile(ichar filename)ref byte p=
!read in mx/ml file into an mcb block, add end_dir byte at the end just in case
!return pointer to mcb block

	p:=readfile(filename)
	return nil when p=nil
	(p+rfsize)^:=end_dir		!add eof-marker

	return p
end

function readbyte(ref byte &p)int=
	return p++^
end

function readu32(ref byte &p)u64 x=
	x:=ref u32(p)^
	p+:=4
	x
end

function readstring(ref byte &p)ichar s=
	s:=pcm_copyheapstring(p)

	while (++p)^ do od
	++p

	return s
end

proc error(ichar mess, param="")=
	if param^ then
		fprintln mess,param
	else
		println mess
	fi
	println "Aborting"
	stop 1
end

function findlib(ichar name)int n=
!find an existing library existing

	for i to nlibs do
		if eqstring(name,libnametable[i]) then return i fi
	od
	return 0
end

function addlib(ichar name)int n=
!add a new lib slot with given name
	if nlibs>=maxlibs then 
		error("Too many libs")
	fi

	libnametable[++nlibs]:=name
	return nlibs
end

function adddll(ichar name)int=
	for i to ndlllibs do
		if eqstring(name,dllnametable[i]) then return i fi
	od

	if ndlllibs>=maxdlls then 
		error("Too many DLLs")
	fi

	dllnametable[++ndlllibs]:=name
	return ndlllibs
end

!function addsymbol(ichar name, u64 address, int dllindex, libindex)int=
function addsymbol(ichar name)int=
	for i to nsymbols do
		if eqstring(name,symbolnametable[i]) then return i fi
	od

	if nsymbols>=maxsymbols then 
		error("Too many Imports")
	fi

	symbolnametable[++nsymbols]:=name
	return nsymbols
end

proc dosymbols(ref librec lib)=
!Add any dll libs to global table (libs already done)
!Then deal with imported and exported symbols

	int ix, libx, dllx
	ref byte baseaddr

	for i to lib.ndlllibs do
		adddll(lib.dllnames[i])
	od

	for i to lib.nimports do
		ix:=addsymbol(lib.importnames[i])
		lib.importxreftable[i]:=ix
	od

	for i to lib.nexports do
		ix:=addsymbol(lib.exports[i])
		if symboldefined[ix] then
!			error("Dupl symbol:",lib.exports[i])
			CPL "Dupl symbol:",lib.exports[i]
			NEXT
		fi
		symboldefined[ix]:=1

		case lib.exportsegs[i]
		when code_seg then baseaddr:=lib.codeptr
		when idata_seg then baseaddr:=lib.idataptr
		when zdata_seg then baseaddr:=lib.zdataptr
		else baseaddr:=nil
		esac

		symboladdress[ix]:=cast(baseaddr+lib.exportoffsets[i])
		symbollibindex[ix]:=lib.libno

	od
end

function finddllsymbol(ichar name, int &dllindex)ref void p=
!look up symbol in any of the DLLs
!return address, or void if not found
!dllindex is set to dll where it was found

	dllindex:=0
	for i to ndlllibs do
		p:=os_getdllprocaddr(dllinsttable[i], name)
		if p then
			dllindex:=i
			return p
		fi
	od

	return nil
end

proc scansymbols=
	int dllindex,undef:=0
	ref void p

	for i to nsymbols when not symboldefined[i] do
		p:=finddllsymbol(symbolnametable[i], dllindex)
		if p then
			symboladdress[i]:=p
			symboldllindex[i]:=dllindex
			symboldefined[i]:=1
		else
			println "Undef",symbolnametable[i]
			++undef
		fi
	od

	if undef then
!		error("Symbols Undefined")
	fi
end

proc dorelocations=
	for i to nlibs when not librelocated[i] do
		reloclib(libtable[i])
	od
end

proc reloclib(ref librec lib)=
	int index, targetoffset
	ichar name
	ref byte p
	ref byte q
	ref u64 qaddr		!to import address table
	mcxreloc r

!do thunk tables first
	p:=lib.codeptr+lib.codesize
	qaddr:=cast(p+lib.nimports*u64.bytes)

	for i to lib.nimports do
		name:=lib.importnames[i]
		p++^:=0x48
		p++^:=0xFF
		p++^:=0x24
		p++^:=0x25
		(ref u32(p)^:=cast(qaddr))
		p+:=4

		index:=lib.importxreftable[i]
!		CPL "------",i,index,symbolnametable[index],symboladdress[index]
		qaddr++^:=cast(symboladdress[index])

	od

!Now do the actual relocations
	for i to lib.nrelocs do
		r:=lib.reloctable[i]
		case r.segment
		when code_seg then p:=lib.codeptr+r.offset
		when idata_seg then p:=lib.idataptr+r.offset
		when zdata_seg then p:=lib.zdataptr+r.offset
		esac

		case r.reloctype
		when locabs32_rel then
			targetoffset:=ref u32(p)^
			case r.targetsegment
			when code_seg then
				(ref u32(p)^ := cast(lib.codeptr+targetoffset))
			when idata_seg then
				(ref u32(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_seg then
				(ref u32(p)^ := cast(lib.zdataptr+targetoffset))
			esac

		when locabs64_rel then
			targetoffset:=ref u32(p)^
			case r.targetsegment
			when code_seg then
				(ref u64(p)^ := cast(lib.codeptr+targetoffset))
			when idata_seg then
				(ref u64(p)^ := cast(lib.idataptr+targetoffset))
			when zdata_seg then
				(ref u64(p)^ := cast(lib.zdataptr+targetoffset))
			esac

		when impabs64_rel then

			index:=lib.importxreftable[r.stindex]			!global index
			(ref u64(p)^+:=cast(symboladdress[index],u64))

		when impabs32_rel then
			index:=lib.importxreftable[r.stindex]			!global index
			(ref u32(p)^+:=cast(symboladdress[index],u32))

		when imprel32_rel then
			if r.segment<>code_seg then error("imprel32?") fi
			index:=r.stindex								!local index
			q:=lib.codeptr+lib.codesize+(index-1)*8

			(ref u32(p)^ := q-(p+4))	!offset to thunk entry
		esac

	od

	librelocated[lib.libno]:=1

end

proc setcmdparams(int cmdskip)=
!adjust cmdparams visible to application by setting $cmdskip flag

	for i to nsymbols do
		if symbolnametable[i]^='$' and eqstring(symbolnametable[i],"$cmdskip") then
!CPL "FOUND $CMDSKIP, SET TO",CMDSKIP
			(ref byte(symboladdress[i])^:=cmdskip)
		fi
	od

end

global proc runprogram(ref librec lib, int cmdskip=0)=
	ref proc fnptr
!	ref function(int,int)int fnptr
	int libno:=lib.libno

!CPL "RUNPROG"

	for i to nlibs when i<>libno and not libinitdone[i] do
		calllibinit(libtable[i])
	od

	if lib.entryaddr=nil then
		error("No entry point found")
	fi

	setcmdparams(cmdskip)

	fnptr:=cast(lib.entryaddr)

	fnptr()

	libinitdone[i]:=1

end

global proc calllibinit(ref librec lib)=
	ref proc fnptr
!	ref function(int,int)int fnptr
	int libno:=lib.libno

!CPL "CALLLIBINIT",LIB.LIBNAME

!CPL "RUNPROG"

	if lib.entryaddr then
		fnptr:=cast(lib.entryaddr)
CPL "STARTING LIB",LIB.LIBNAME
		fnptr()
	fi
	libinitdone[lib.libno]:=1
end

proc checknew(ichar name, filename)=
	if findlib(name) then
		error("Lib already exists:",filename)
	fi
end
=== mm_help.txt 0 1 38/44 ===
M Compiler Generating x64 native code - Windows Version

Whole-program compiler builds entire program from the lead module
into a executable file.

    mm main              # Create main.exe from lead module main.m
    mm main.m            # Same (.m extension is default)
    mm -c main           # Create single-file main.asm intermediate ASM

Options:

    -exe                 # Generate .exe executable file (default)
    -dll                 # Generate .dll library and .exp file
    -pcl                 # Generate intermediate PCL file only
    -asm                 # Generate intermediate ASM file only

    -opt                 # Apply simple optimiser

    -out:file            # Name of output file 

    -ma                  # Create .ma file combining source/support files
    -docs                # Create .txt with docstrings of exported files (not finished)
    -run                 # For -exe mode only: run resulting executable

    @file                # Read options from file

Example:

     mm -run prog : abc def

Any parameters for the new program must follow " : " (spaces needed).
=== msysp.m 0 1 39/44 ===
import clib

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

export int $cmdskip			!0 unless set by READMCX/etc

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
!const rd_buffersize = 524288	!total capacity of line buffer

global ref char rd_buffer		! point to start of read buffer
int rd_length			! length of this line (as read by readln)
ref char rd_pos			! current position it's up to (next read starts here)
ref char rd_lastpos		! set by sread() just before reading used for reread()
int termchar			! terminator char set by readxxx()
int itemerror			!	set by some read functions, eg for reals

[4096]char printbuffer
ichar printptr
int printlen

!------------------------------------------

const maxparam=128
export int nsysparams
export int ncmdparams
export int nenvstrings
export [maxparam]ichar sysparams
!export ref[]ichar cmdparams
export ref[0:]ichar cmdparams
export ref[]ichar envstrings
!export [maxparam]ichar envstrings

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
!	ref[]ichar env
	static [128]byte startupinfo			! 68 or 104 bytes
	int res
	ichar s
	
!CPL "////////STD SYSLIB NEW*******"
	res:=__getmainargs(&nargs,cast(&args),cast(&envstrings),0,cast(&startupinfo))
!	res:=__getmainargs(&nargs,cast(&args),cast(&env),1,cast(&startupinfo))
	
	nsysparams:=nargs
	
	if nsysparams>maxparam then
		printf("Too many params\n")
		stop 50
	fi

	nargs64:=nargs			!bug when using 32-bit limit when compiled with mm
	for i:=1 to nargs64 do
		sysparams[i]:=args[i]
	od
	
!assume nsysparams is >=1, since first is always the program name
!CPL =$CMDSKIP

	ncmdparams:=nsysparams-($cmdskip+1)
!	cmdparams:=cast(&sysparams[2])
	cmdparams:=cast(&sysparams[$cmdskip+1])
!	cmdparams:=cast(&sysparams[])

	int j:=1
	nenvstrings:=0
	while envstrings[j] do
		++nenvstrings

!		if nenvstrings<maxparam then
!			envstrings[++nenvstrings]:=env[j]
			++j
!		fi
	od

!	_setmode(0,32768);
!	_setmode(1,32768);

!!	_setmode(2,32768);
!os_init()

!	m$print_startcon()		!allow most print stmts without startcon/end ?????
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
	resetprintbuffer()
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
!PRINTF("STARTCON %d\n",NIOSTACK);
	pushio()
	outdev:=std_io
	resetprintbuffer()
end

global proc m$freddy=
	pushio()
	outdev:=std_io
end

global proc m$print_setfmt(ref char format)=
	fmtstr:=format
end

global proc m$print_end=
	needgap:=0
	nextfmtchars(1)
	if niostack=1 and outdev in [std_io,file_io] then
		dumpprintbuffer()
	fi

	if niostack=0 then return fi
	outchan	:= outchan_stack[niostack]
	outdev	:= outdev_stack[niostack]
	fmtstr	:= fmtstr_stack[niostack]
	needgap	:= needgap_stack[niostack]


	--niostack
!PRINTF("ENDCON %d\n",NIOSTACK);
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
!	if fmtstyle=nil then
!		printstr_n(cast(s.sliceptr),s.len)
!	else
		abortprogram("FORMATED PRINT SLICE NOT READY")
!		strtofmt(fmtstyle,-1,&fmt)
!		tostr_str(s,&fmt)
!	fi
	needgap:=1
end

global proc m$print_newline=
	needgap:=0
	nextfmtchars(1)
!RETURN
	printstr("\w")
!	printstr("\n")
end

global proc m$print_nogap=
	needgap:=0
end

global proc m$print_space=
	needgap:=0
!RETURN
	printstr(" ")
end

export proc printstr(ichar s)=
	printstr_n(s,strlen(s))
end

export proc printstr_n(ichar s,int n)=
	ref ref char p

	return when n=0

	if niostack=1 and outdev in [std_io,file_io] then
		addtobuffer(s,n)
	else
		dumpstr(s,n)
	fi
end

export proc printstrn_app(ichar s, int length, filehandle f=nil)=
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
	[4]char str
!RETURN

	str[1]:=ch
	str[0]:=ch
	printstr_n(str,1)
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

proc strtofmt(ref char s,int slen,ref fmtrec fmt) =		!PC_STRTOFMT
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

	int c, base
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
		when 'B', 'b' then fmt.base:=2
		when 'H', 'h' then fmt.base:=16
		when 'O', 'o' then fmt.base:=8
		when 'X', 'x' then
			base:=0
			do
				c:=s^
				if c in '0'..'9' then
					base:=base*10+c-'0'
					++s
				else
					exit
				fi
			od
			if base in 2..16 then
				fmt.base:=base
			fi

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
		when 'W', 'w' then fmt.usigned:='W'
		when 'E', 'e' then fmt.realfmt:='e'
		when 'F', 'f' then fmt.realfmt:='f'
		when 'G', 'g' then fmt.realfmt:='g'
! when '0','1','2','3','4','5','6','7','8','9' then
		when '.' then
			wset:=1
		when comma,'_' then fmt.sepchar:=c
		when '+' then fmt.plus:='+'
		when 'D', 'd' then fmt.charmode:='D'
		when 'C', 'c' then fmt.charmode:='C'
		when 'M', 'm' then fmt.heapmode:='M'
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
					fmt.minwidth:=n
					wset:=1
				else
					fmt.precision:=n
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
!note) = for non-numeric strings, fmt.base should be set to 0, to avoid moving
!a leading +/- when right-justifying with '0' padding.
!t MUST be big enough for the expanded string; caller must take care of this
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
		if base=10 then
			assem
				mov		rcx, [aa]
				mov		rax, rcx
				mov		rdx, 7378697629483820647
				imul	rdx
				mov		rax, rdx
				mov		rdx, rcx
				sar		rdx, 63
				sar		rax, 2
				sub		rax, rdx
				lea		rdx, [rax+rax*4]
				add		rdx, rdx
				sub		rcx, rdx
				mov		[dd], rcx
				mov		[aa], rax
			end
		else
			dd:=aa rem base
			aa:=aa/base
		fi

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
	word dlow
	REF U64 P:=cast(&D)
	REF U64 Q:=cast(&D)

	d:=a/b
	r:=a-d*b
	rlow:=r
	dlow:=d

	remainder:=rlow
	return d
end

function i64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =
!a is signed 64-bit int/long, fmt is a ref to a filled-in fmtrec
!convert a to a string in s, according to fmt
!a basic conversion is done first,: the field manipulation is done
!signed=1 for int, 0 for u32 (fmt.unsigned forces ints to be treated as longs)
!returns length of s
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w,usigned
	const i64 mindint=0x8000'0000'0000'0000

	usigned:=0
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
	if (fmt.base>10 or fmt.suffix) and fmt.lettercase='a'	then	! need lower when
		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u64tostrfmt(i64 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u64tostr(aa,&.str,fmt.base,fmt.sepchar)

	if fmt.suffix then
		str[n]:=fmt.suffix
		str[++n]:=0
	fi

!str uses upper cases for hex/etc see if lc needed
	if fmt.base>10 or fmt.suffix and fmt.lettercase='a'	then	! need lower when
!		convlcstring(&.str)
	fi

!at this point, n is the str length including signs and suffix
	return expandstr(&.str,s,n,fmt)
end

function u128tostrfmt(i128 aa,ref char s,ref fmtrec fmt)int =		!U64TOSTRFMT
!see i64tostrfmt
	[0:onesixty]char str				! allow for binary with separators!
	int i,j,k,n,w

	n:=u128tostr(aa,&.str,fmt.base,fmt.sepchar)

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

	case fmt.charmode
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

	case fmt.charmode
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

	case fmt.charmode
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

	if fmt.precision then
		cfmt[1]:='.'
		cfmt[2]:='*'
		cfmt[3]:=fmt.realfmt
		cfmt[4]:=0
		sprintf(&.str,&.cfmt,fmt.precision,x)
	else
		cfmt[1]:=fmt.realfmt
		cfmt[2]:=0
		sprintf(&.str,&.cfmt,x)
	fi

!at this point, n is the str length including signs and suffix

	n:=strlen(&.str)		! current length

	if n<fmt.minwidth then
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

	if fmt.quotechar or fmt.minwidth>newlen or fmt.lettercase or fmt.precision then
		if fmt.quotechar then
			newlen+:=2
		fi
		if fmt.minwidth>newlen then
			newlen:=fmt.minwidth
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

function getfmt(ichar fmtstyle)ref fmtrec=
	static fmtrec fmt
	if fmtstyle then
		strtofmt(fmtstyle,-1,&fmt)
		return &fmt
	else
		return &defaultfmt
	fi
end

export function strint(int64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_i64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export proc getstrint(int64 a, ichar dest)=
	m$print_startstr(dest)
	tostr_i64(a,getfmt(nil))
	m$print_end()
end

export function strword(word64 a, ichar fmtstyle=nil)ichar=
	static [100]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_u64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export function strreal(real a, ichar fmtstyle=nil)ichar=
	static [320]char str
	ref fmtrec fmt

	m$print_startstr(&.str)
	tostr_r64(a,fmt:=getfmt(fmtstyle))
	m$print_end()
	return getstr(&.str,fmt)
end

export function getstr(ichar s, ref fmtrec fmt)ichar=
	if fmt.heapmode then
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
	ichar p
	initreadbuffer()

	if f=filehandle(1) then
		rd_buffer^:=0
		p:=getcommandlinea()
		repeat
			++p
		until p^ in [' ','\t',0]
		strcpy(rd_buffer, p)
!CPL GETCOMMANDLINEA()
!
!		for i:=2 to nsysparams do
!			strcat(rd_buffer,sysparams[i])
!			strcat(rd_buffer," ")
!		od
		rd_length:=strlen(rd_buffer)
		rd_pos:=rd_buffer
		rd_lastpos:=nil
		return
	fi

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

export function strtoint(ichar s,int length=-1, word base=10)int64=
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

export proc readstr(ref char dest, int fmt=0,destlen=0)=
	m$read_str(dest,destlen,fmt)
end

export proc rereadln=
	rd_pos:=rd_buffer
	rd_lastpos:=rd_pos
end

export proc reread=
	rd_pos:=rd_lastpos
end

export function valint(ichar s, int fmt=0)int64=
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

export function valreal(ichar s)real=
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

proc mclunimpl(ichar mess)=
	printf("MCL-UNIMPL: %s\n",mess)
	stop 1
end

proc dumpstr(ichar s, int n, fbuffer=0)=
!fbuffer=1 when outputting contents of buffer

	ref ref char p

	return when n=0

	if outdev=str_io then
		p:=cast(outchan)
		memcpy(p^,s,n)
		p^+:=n
		p^^:=0
		return
	fi

	if fbuffer and n>=2 and outdev=std_io then
		--printptr				!point to last char
		if printptr^=10 then
			if (printptr-1)^=13 then		!crlf
!PUTS("<CRLF>")
				(printptr-1)^:=0
			else							!lf only
!PUTS("<LF>")
				printptr^:=0
			fi
			puts(printbuffer)
			return
		fi
	fi

	case outdev
	when std_io then
		printf("%.*s",n,s)
	when file_io then
		fprintf(outchan,"%.*s",n,s)
	esac
end

proc dumpprintbuffer=
	if printlen then
		dumpstr(&.printbuffer,printlen,1)
	fi

	resetprintbuffer()
end

proc resetprintbuffer=
	printptr:=&.printbuffer
	printlen:=0
end

proc addtobuffer(ichar s, int n)=
!RETURN
	if printlen+n>=(printbuffer.len-8) then
		dumpprintbuffer()
	fi

	if n<printbuffer.len then
		memcpy(printptr,s,n)
		printptr+:=n
		printlen+:=n
		return
	fi

	dumpstr(s, n)			!don't bother with buffer
end
=== mlibp.m 0 1 40/44 ===
import clib

!const mem_check=1
const mem_check=0

global [0..300]u64 allocupper
global int alloccode				!set by heapalloc
export int allocbytes				!set by heapalloc
export int fdebug=0
export int rfsize

const threshold=1<<25
const alloc_step=1<<25
word maxmemory
int  maxalloccode

GLOBAL REF VOID ALLOCBASE

byte pcm_setup=0

int show=0

global int memtotal=0
export int64 smallmemtotal=0
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
export const int maxblocksize = 2048
export const int $maxblocksizexx = 2048

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

export record strbuffer =
	ichar strptr
	int32 length
	int32 allocated
end

export tabledata() [0:]ichar pmnames=
	(pm_end=0,		$),
	(pm_option,		$),
	(pm_sourcefile,	$),
	(pm_libfile,	$),
	(pm_colon,		$),
	(pm_extra,		$),
end

![2]word seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)
[2]int seed = (0x2989'8811'1111'1272',0x1673'2673'7335'8264)

export function pcm_alloc(int n)ref void =		!PCM_ALLOC
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

!		if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

		return p
	fi

	alloccode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc
	allocbytes:=allocupper[alloccode]
	smallmemtotal+:=allocbytes

	if p:=ref byte(freelist[alloccode]) then		!Items of this block size available
!		if mem_check then addtomemalloc(ref int32(p),allocbytes) fi
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
!	if mem_check then addtomemalloc(ref int32(p),allocbytes) fi

	return p
end

export proc pcm_free(ref void p,int n) =		!PCM_FREE
!n can be the actual size requested it does not need to be the allocated size
	int acode

	if n=0 then return fi

	if n>maxblocksize then		!large block
!		if mem_check then removefrommemalloc(p,n) fi

		free(p)
		return
	fi

	if p then
		acode:=sizeindextable[n]		!Size code := 0,1,2 etc for 0, 16, 32 etc

		smallmemtotal-:=allocupper[acode]

!		if mem_check then removefrommemalloc(p,allocupper[acode]) fi

		cast(p,ref wordp)^:=wordp(int(freelist[acode]))
		freelist[acode]:=p
	fi
end

!export proc pcm_free(ref void p,int n)=
!!n can be the actual size requested it does not need to be the allocated size
!	int acode
!
!	if p=nil or n=0 then return fi
!
!	if n>maxblocksize then
!		free(p)
!	else
!		acode:=sizeindextable[n]		! n=1..1024 => 1,2,3,4,5,6,7
!		ref word(p)^:=word(int(freelist[acode]))
!		freelist[acode]:=p
!	fi
!end
!

export proc pcm_freeac(ref void p,int alloc) =		!PCM_FREEAC
	pcm_free(p,allocupper[alloc])
end

export proc pcm_copymem4(ref void p,q,int n) =	!PCM_COPYMEM4
!copy n bytes of memory from q to p.
!the memory spaces used are multiples of 16 bytes, but n itself could be anything
!n can be zero, and need not be a multiple of 4 bytes

	memcpy(p,q,n)
end

export proc pcm_clearmem(ref void p,int n) =		!PCM_CLEARMEM
	memset(p,0,n)
end

export proc pcm_init =		!PCM_INIT
!set up sizeindextable too
	int j,k,k1,k2
	int64 size
	const limit=1<<33

	alloccode:=0
	if pcm_setup then
		return
	fi

	pcm_newblock(0)

!	ALLOCBASE:=PCHEAPPTR

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

export function pcm_getac(int size)int =		!PCM_GETAC
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

export function pcm_newblock(int itemsize)ref void=
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

export function pcm_round(int n)int =		!PCM_ROUND
!for any size n, return actual number of bytes that would be allocated
	static [0:maxblockindex+1]int32 allocbytes=(0,16,32,64,128,256,512,1024,2048)

	if n>maxblocksize then
		return n
	else
		return allocbytes[sizeindextable[n]]
	fi
end

export function pcm_array(int n)int =		!PCM_ARRAY
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

export function pcm_allocz(int n)ref void =		!PCM_ALLOCZ
	ref void p
	p:=pcm_alloc(n)

	memset(p,0,n)
	return p
end

export function pcm_copyheapstring(ref char s)ref char =
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

export function pcm_copyheapstringn(ref char s,int n)ref char =
	ref char q
	if s=nil then return nil fi

	q:=pcm_alloc(n+1)
	memcpy(q,s,n)
	(q+n)^:=0
	return q
end

export function pcm_copyheapblock(ref char s, int length)ref char =
!allocate enough bytes for string s: copy s to the heap
!return pointer to new string
	ref char q
	if length=0 then return nil fi

	q:=pcm_alloc(length)
	memcpy(q,s,length)
	return q
end

!proc addtomemalloc(ref int32 ptr,int size)=
!!add ptr to allocated table
!
!!CPL "***************ADD TO ALLOC:",ptr,size
!	for i to maxmemalloc do
!		if memalloctable[i]=ptr then
!			CPL "ALLOC ERROR:",ptr,"ALREADY ALLOCATED\n\n\n"
!			stop 2
!		fi
!
!		if memalloctable[i]=nil then		!unused entry
!			memalloctable[i]:=ptr
!			memallocsize[i]:=size
!			return
!		fi
!	od
!	CPL "MEMALLOCTABLE FULL\n\n\n\n"; os_getch()
!	stop 3
!end
!
!proc removefrommemalloc(ref int32 ptr,int size)=
!!remove ptr to allocated table
!
!!CPL "------------------************REMOVE FROM ALLOC:",ptr,size
!
!	for i to maxmemalloc do
!		if memalloctable[i]=ptr then
!			if memallocsize[i]<>size then
!				CPL "REMOVE:FOUND",ptr,"IN MEMALLOCTABLE, FREESIZE=",size,", BUT STORED AS BLOCK SIZE:",memallocsize[i]
!				abortprogram("MEMSIZE")
!			fi
!			memalloctable[i]:=nil
!			return
!		fi
!	od
!	CPL "CAN'T FIND",ptr,"IN MEMALLOCTABLE",size
!	abortprogram("MEM")
!	stop 4
!end

export function allocmem(int n)ref void =		!ALLOCMEM
	ref void p

	p:=malloc(n)
	if p then
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

export proc abortprogram(ref char s) =		!ABORTPROGRAM
	println s
	print   "ABORTING: Press key..."
!os_getch()
	stop 5
end

export function getfilesize(filehandle handlex)int=		!GETFILESIZE
	word32 p,size

	p:=ftell(handlex)		!current position
	fseek(handlex,0,2)		!get to eof
	size:=ftell(handlex)		!size in bytes
	fseek(handlex,p,seek_set)	!restore position
	return size
end

export proc readrandom(filehandle handlex, ref byte mem, int offset, size) =		!READRANDOM
	int a
	fseek(handlex,offset,seek_set)
	a:=fread(mem,1,size,handlex)			!assign so as to remove gcc warning
end

export function writerandom(filehandle handlex, ref byte mem, int offset,size)int =		!WRITERANDOM
	fseek(handlex,offset,seek_set)
	return fwrite(mem,1,size,handlex)
end

export function setfilepos(filehandle file,int offset)int=
	return fseek(file,offset,0)
end

export function getfilepos(filehandle file)int=
	return ftell(file)
end

export function readfile(ref char filename)ref byte =		!READFILE
	filehandle f
	int size
	ref byte m,p

	f:=fopen(filename,"rb")
	if f=nil then
		return nil
	fi
	rfsize:=size:=getfilesize(f)

	m:=malloc(size+2)		!allow space for etx/zeof etc

	if m=nil then
		return nil
	fi

	readrandom(f,m,0,size)
	p:=m+size			!point to following byte
	(ref u16(p)^:=0)	!add two zero bytes

	fclose(f)
	return m
end

export function writefile(ref char filename,ref byte data,int size)int =
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

export function checkfile(ref char file)int=		!CHECKFILE
	filehandle f
	if f:=fopen(file,"rb") then
		fclose(f)
		return 1
	fi
	return 0
end

export proc readlinen(filehandle handlex,ref char buffer,int size) =		!READLINEN
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

export proc iconvlcn(ref char s,int n) =		!ICONVLCN
	to n do
		s^:=tolower(s^)
		++s
	od
end

export proc iconvucn(ref char s,int n) =		!ICONVUCN
	to n do
		s^:=toupper(s^)
		++s
	od
end

export function convlcstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=tolower(s^)
		++s
	od
	s0
end

export function convucstring(ref char s)ichar s0=
	s0:=s
	while (s^) do
		s^:=toupper(s^)
		++s
	od
	s0
end

export function changeext(ref char s,newext)ichar=		!CHANGEEXT
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

export function extractext(ref char s,int period=0)ichar=		!EXTRACTEXT
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

export function extractpath(ref char s)ichar=		!EXTRACTPATH
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

export function extractfile(ref char s)ichar=		!EXTRACTFILE
	ref char t

	t:=extractpath(s)

	if t^=0 then			!s contains no path
		return s
	fi

	return s+strlen(t)		!point to last part of s that contains the file
	end

export function extractbasefile(ref char s)ichar=		!EXTRACTBASEFILE
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

export function addext(ref char s,ref char newext)ichar=		!ADDEXT
!when filespec has no extension of its own, add newext
	ref char sext

	sext:=extractext(s,1)

	if sext^=0 then						!no extension not even "."
		return changeext(s,newext)
	fi

	return s							!has own extension; use that
end

export function alloctable(int n, size)ref void =		!ALLOCTABLE
!Allocate table space for n elements, each of size <size>
!Allows for 1-based indexing, so allocates (n+1) elements
	ref void p

	p:=malloc((n+1)*size)

	if not p then
		abortprogram("Alloctable failure")
	fi
	return p
end

export function zalloctable(int n, size)ref void =		!ALLOCTABLE
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

export function pcm_alloc32:ref void =		!PCM_ALLOC
	ref byte p
	allocbytes:=32
	smallmemtotal+:=32

	if p:=ref byte(freelist[2]) then		!Items of this block size available
		freelist[2]:=ref wordp(int((freelist[2])^))
		return p
	fi

!No items in freelists: allocate new space in this heap block
	return pcm_alloc(32)
end

export proc pcm_free32(ref void p) =
!n can be the actual size requested it does not need to be the allocated size

	smallmemtotal-:=32
!	if mem_check then removefrommemalloc(p,32) fi

	cast(p,ref wordp)^:=wordp(int(freelist[2]))
	freelist[2]:=p
end

export proc outbyte(filehandle f,int x)=
	fwrite(&x,1,1,f)
end

export proc outword16(filehandle f,word x)=
	fwrite(&x,2,1,f)
end

export proc outword32(filehandle f,word x)=
	fwrite(&x,4,1,f)
end

export proc outword64(filehandle f,word64 x)=
	fwrite(&x,8,1,f)
end

export proc outstring(filehandle f, ichar s)=
	fwrite(s,strlen(s)+1,1,f)
end

export proc outblock(filehandle f, ref void p, int n)=
	fwrite(p,n,1,f)
end

export function myeof(filehandle f)int=
	int c

	c:=fgetc(f)
	if c=c_eof then return 1 fi
	ungetc(c,f)
	return 0;
end

export function pcm_smallallocz(int n)ref void =
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

export function pcm_smallalloc(int n)ref void =
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

export proc strbuffer_add(ref strbuffer dest, ichar s, int n=-1)=
	int newlen,oldlen
	ichar newptr

	IF N=0 THEN CPL "N=0" FI

	if n=-1 then
		n:=strlen(s)
	fi

	oldlen:=dest.length

	if oldlen=0 then				!first string
		dest.strptr:=pcm_alloc(n+1)
		dest.allocated:=allocbytes
		dest.length:=n				!length always excludes terminator
		memcpy(dest.strptr,s,n)
		(dest.strptr+n)^:=0
		return
	fi

	newlen:=oldlen+n
	if newlen+1>dest.allocated then
		newptr:=pcm_alloc(newlen+1)
		memcpy(newptr,dest.strptr,oldlen)
		dest.strptr:=newptr
		dest.allocated:=allocbytes
	fi

	memcpy(dest.strptr+oldlen,s,n)
	(dest.strptr+newlen)^:=0

	dest.length:=newlen
end

export proc gs_init(ref strbuffer dest)=			!INITGENSTR
	pcm_clearmem(dest,strbuffer.bytes)
end

export proc gs_free(ref strbuffer dest)=
	if dest.allocated then
		pcm_free(dest.strptr,dest.allocated)
	fi
end

export proc gs_str(ref strbuffer dest,ichar s)=			!GENSTR
	strbuffer_add(dest,s)
end

export proc gs_char(ref strbuffer dest,int c)=
	[16]char s

	s[1]:=c
	s[2]:=0

	strbuffer_add(dest,&.s,1)
end

export proc gs_strn(ref strbuffer dest,ichar s,int length)=
	strbuffer_add(dest,s,length)
end

export proc gs_strvar(ref strbuffer dest,s)=			!GENSTR
	strbuffer_add(dest,s.strptr)
end

export proc gs_strint(ref strbuffer dest,int64 a)=
	strbuffer_add(dest,strint(a))
end

export proc gs_strln(ref strbuffer dest,ichar s)=		!GENSTRLN
	gs_str(dest,s)
	gs_line(dest)
end

export proc gs_strsp(ref strbuffer dest,ichar s)=
	gs_str(dest,s)
	gs_str(dest," ")
end

export proc gs_line(ref strbuffer dest)=
	strbuffer_add(dest,"\w")
end

export function gs_getcol(ref strbuffer dest)int=
	return dest.length
end

export proc gs_leftstr(ref strbuffer dest, ichar s, int w, padch=' ')=
	int col,i,n,slen
	[2560]char str
	col:=dest.length
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

export proc gs_leftint(ref strbuffer dest, int a, int w, padch=' ')=
	gs_leftstr(dest,strint(a),w,padch)
end

export proc gs_padto(ref strbuffer dest,int col, ch=' ')=
	int n
	[2560]char str

	n:=col-dest.length
	if n<=0 then return fi
	for i:=1 to n do
		str[i]:=ch
	od
	str[n+1]:=0
	gs_str(dest,&.str)
end

export proc gs_println(ref strbuffer dest,filehandle f=nil)=
	if dest.length=0 then return fi
	(dest.strptr+dest.length)^:=0

	if f=nil then
		println dest.strptr,,"\c"
	else
		println @f,dest.strptr,,"\c"
	fi
end

!export function nextcmdparam(int &paramno, ichar &name, &value, ichar defext=nil)int=
!	static int infile=0
!	static ichar filestart=nil
!	static ichar fileptr=nil
!	static byte colonseen=0
!	ref char q
!	ichar item,fileext
!	ichar rest
!	int length
!	static [300]char str
!
!	reenter::
!	value:=nil
!	name:=nil
!
!	if infile then
!		if readnextfileitem(fileptr,item)=0 then		!eof
!			free(filestart)								!file allocated via malloc
!			infile:=0
!			goto reenter
!		fi
!	else
!		if paramno>nsysparams then
!			return pm_end
!		fi
!		item:=sysparams[paramno]
!		++paramno
!
!		length:=strlen(item)
!
!		if item^='@' then		!@ file
!			filestart:=fileptr:=cast(readfile(item+1))
!			if filestart=nil then
!				println "Can't open",item
!				stop 7
!			fi
!			infile:=1
!			goto reenter
!		fi
!
!		if item^=':' then
!			colonseen:=1
!			return pm_colon
!		fi
!	fi
!
!	value:=nil
!	if item^='-' then
!		name:=item+(colonseen|0|1)
!		q:=strchr(item,':')
!		if not q then
!			q:=strchr(item,'=')
!		fi
!		if q then
!			value:=q+1
!			q^:=0
!		fi
!		return (colonseen|pm_extra|pm_option)
!	fi
!
!	fileext:=extractext(item,0)
!	name:=item
!
!	if fileext^=0 then							!no extension
!		strcpy(&.str,name)
!		if defext and not colonseen then
!			name:=addext(&.str,defext)				!try .c
!		fi
!	elsif eqstring(fileext,"dll") or eqstring(fileext,"mcx") then
!		return (colonseen|pm_extra|pm_libfile)
!	fi
!	return (colonseen|pm_extra|pm_sourcefile)
!end

export function nextcmdparamnew(int &paramno, ichar &name, &value, ichar defext=nil)int=
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
		if paramno>ncmdparams then
			return pm_end
		fi
		item:=cmdparams[paramno]
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
!	elsif eqstring(fileext,"dll") then
	elsif eqstring(fileext,"dll") or eqstring(fileext,"mcx") then
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

export proc ipadstr(ref char s,int width,ref char padchar=" ")=
	int n

	n:=strlen(s)
	to width-n do
		strcat(s,padchar)
	od
end

export function padstr(ref char s,int width,ref char padchar=" ")ichar=
	static [256]char str

	strcpy(&.str,s)
	ipadstr(&.str,width,padchar)
	return &.str
end

export function chr(int c)ichar=
	static [8]char str

	str[1]:=c
	str[2]:=0
	return &.str
end

export function cmpstring(ichar s,t)int=
	int res
	if (res:=strcmp(s,t))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function cmpstringn(ichar s,t,int n)int=
	int res
	if (res:=strncmp(s,t,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function eqstring(ichar s,t)int=
	return strcmp(s,t)=0
end

export function cmpbytes(ref void p,q,int n)int=
	int res
	if (res:=memcmp(p,q,n))<0 then
		return -1
	elsif res>0 then
		return 1
	else
		return 0
	fi
end

export function eqbytes(ref void p,q,int n)int=
	return memcmp(p,q,n)=0
end

export proc mseed(word64 a,b=0)=
	seed[1]:=a
	if b then
		seed[2]:=b
	else
		seed[2] ixor:=a
	fi
end

export function mrandom:word =
!return pure 64-bit word value, 0 to 2**64-1
!(cast result for signed value)
!	word64 x,y
	int x,y
	x:=seed[1]
	y:=seed[2]
	seed[1]:=y
	x ixor:=(x<<23)
	seed[2]:= x ixor y ixor (x>>17) ixor (y>>26)
	return seed[2]+y
end

export function mrandomp:int =
!pure 64-bit int value, positive only, 0 to 2**63-1
	return mrandom() iand 0x7FFF'FFFF'FFFF'FFFF
end

export function mrandomint(int n)int=
!positive random int value from 0 to n-1
	return mrandomp() rem n
end

export function mrandomrange(int a,b)int=
!random int value from a to b inclusive
!span extent must be 1 to 2**63-1
	int span
	span:=b-a+1
	if span<=0 then
		return 0
	fi
	return (mrandomp() rem span)+a
end

export function mrandomreal:real x=
!positive random real value from 0 to just under (but not including) 1.0
	repeat x:=mrandomp()/9223372036854775808.0 until x<>1.0
	return x
end

export function mrandomreal1:real=
!positive random real value from 0 to 1.0 inclusive
	return mrandomp()/9223372036854775807
end

export function checkpackfile:ref byte=
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

export function pcm_allocx:ref void =
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

export function readline:ichar=
	readln
	return rd_buffer
end

export function stralloc(ref void p)ichar=
	return strint(int(ref byte(p)-allocbase))
end

export function findfunction(ichar name)ref void=
	for i to $getnprocs() do
		if eqstring($getprocname(i),name) then
			return $getprocaddr(i)
		fi
	od
	return nil
end

=== mclib.m 0 1 41/44 ===
export type filehandle=ref void

importdll $cstd=
	function malloc		(word64)ref void
	function realloc	(ref void, word)ref void
	proc     free		(ref void)
	proc     memset		(ref void, int32, word)
	proc     memcpy		(ref void, ref void, word)
	function clock		:int32
	function ftell		(filehandle)int32
	function fseek		(filehandle, int32, int32)int32
	function fread		(ref void, word, word, filehandle)word
	function fwrite		(ref void, word, word, filehandle)word
	function getc		(filehandle)int32
	function ungetc		(int32, filehandle)int32
	function fopen		(ichar,ichar="rb")filehandle
	function fclose		(filehandle)int32
	function fgets		(ichar, int, filehandle)ichar
	function remove		(ichar)int32
	function rename		(ichar, ichar)int32
	function getchar	:int32
	proc     putchar	(int32)
	proc     setbuf		(filehandle, ref byte)

	function strlen		(ichar)int
	function strcpy		(ichar, ichar)ichar
	function strcmp		(ichar, ichar)int32
	function strncmp	(ichar, ichar, word)int32
	function strncpy	(ichar, ichar, word)word
	function memcmp		(ref void, ref void, word)int32
	function strcat		(ichar, ichar)ichar
	function tolower	(int32)int32
	function toupper	(int32)int32
	function isalpha	(int32)int32
	function isupper	(int32)int32
	function islower	(int32)int32
	function isalnum	(int32)int32
	function isspace	(int32)int32
	function strstr		(ichar, ichar)ichar
	function atol		(ichar)intm
	function atoi		(ichar)int32
!	function strtod		(ichar,ref ref char)real64
	function strtod		(ichar,ref ref char)real64
	function _strdup	(ichar)ichar

	function puts		(ichar)int32
	function printf		(ichar, ...)int32

	function sprintf	(ichar, ichar, ...)int32

	function sscanf		(ichar, ichar, ...)int32
	function scanf		(ichar, ...)int32

	function rand		:int32
	proc     srand		(word32)
	function system		(ichar)int32

	function fgetc		(filehandle)int32
	function fputc		(int32,  filehandle)int32
	function fprintf	(filehandle, ichar, ...)int32
	function fputs		(ichar,  filehandle)int32
	function feof		(filehandle)int32
	function getch		:int32
	function _getch		:int32
	function kbhit		:int32
	function _mkdir		(ichar)int32
	function mkdir		(ichar)int32
	function strchr		(ichar,int32)ichar

	function _setmode	(int32,int32)int32

	proc     _exit		(int32)
	proc     "exit"		(int32)
!	proc     `exit		(int32)
	function pow		(real,real)real

	function `sin 		(real)real
	function `cos		(real)real
	function `tan		(real)real
	function `asin		(real)real
	function `acos		(real)real
	function `atan 		(real)real
	function `log		(real)real
	function `log10		(real)real
	function `exp		(real)real
	function `floor		(real)real
	function `ceil		(real)real

	proc      qsort   	(ref void, word64, word64, ref proc)

end

export macro strdup=_strdup

importdll $cstdextra=
	function __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

export const c_eof		=-1
global const seek_set	= 0
global const seek_curr	= 1
global const seek_end	= 2
=== mwindowsp.m 0 1 42/44 ===
import clib

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

export record rsystemtime =
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

	windows proc	 "MessageBoxA"(wt_int x=0,wt_ichar message, caption="Caption",wt_int y=0)

	windows function "QueryPerformanceCounter"(ref int64)wt_bool
	windows function "QueryPerformanceFrequency"(ref int64)wt_bool

	windows function "CreateFileA"(wt_ichar,wt_dword,wt_dword,wt_ptr,wt_dword,wt_dword,wt_handle)wt_handle
	windows function "GetFileTime"(wt_handle,wt_ptr,wt_ptr,wt_ptr)wt_bool

	windows proc     "GetSystemTime"(ref rsystemtime)
	windows proc     "GetLocalTime"(ref rsystemtime)

	windows function "GetTickCount64":u64
	windows function "PeekMessageA"		(ref void, ref wt_handle, wt_uint,wt_uint,wt_uint)wt_bool

	windows function "GetCommandLineA":ichar

	windows function "VirtualAlloc" (ref void, wt_dword, wt_dword, wt_dword)ref void
	windows function "VirtualProtect" (ref void, wt_dword, wt_dword, ref wt_dword)wt_bool

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

const MEM_COMMIT				= 4096
const MEM_RESERVE				= 8192
const PAGE_EXECUTE				= 16
const PAGE_EXECUTE_READ			= 32
const PAGE_EXECUTE_READWRITE	= 64
const PAGE_NOACCESS				= 1


wt_handle hconsole, hconsolein

input_record lastkey, pendkey
int keypending			!whether pendkey contains a new key event detected by flushkbd

ref function(ref void)int wndproc_callbackfn=nil	!windows call-back: address of handler

int init_flag=0

export proc os_init=
	int i,count
	rconsole info

!general initialisation
	hconsole:=GetStdHandle(u32(-11))
	hconsolein:=GetStdHandle(u32(-10))

	lastkey.repeatcount:=0
	keypending:=0

	SetConsoleCtrlHandler(nil,1)

	SetConsoleMode(hconsole,1 ior 2)

	init_flag:=1

end

export function os_execwait(ichar cmdline,int newconsole=0,ichar workdir=nil)int =
	wt_dword exitcode
	int status
	int cflags:=0

	rstartupinfo si
	rprocess_information xpi

	clear si
	clear xpi

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
	fi

	WaitForSingleObject(xpi.process, 0xFFFF'FFFF)
	GetExitCodeProcess(xpi.process,&exitcode)

	CloseHandle(xpi.process)
	CloseHandle(xpi.thread)

	return exitcode
end

export function os_execcmd(ichar cmdline, int newconsole=0)int =
	wt_dword exitcode
	int i,j,k

	rstartupinfo si
	rprocess_information xpi

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

export function os_getch:int=
	int k

	k:=os_getchx() iand 255

	return k
	end

export function os_kbhit:int=
	wt_dword count

	unless init_flag then os_init() end

	GetNumberOfConsoleInputEvents(hconsolein,&count)
	return count>1
end

export proc os_flushkeys=
	FlushConsoleInputBuffer(hconsolein)
end

export function os_getconsolein:ref void=
	return ref void(hconsolein)
end

export function os_getconsoleout:ref void=
	return ref void(hconsole)
end

export function os_proginstance:ref void=
	abortprogram("PROGINST")
	return nil
end

export function os_getdllinst(ichar name)u64=
	wt_handle hinst

	hinst:=LoadLibraryA(name)
	return cast(hinst)
end

export function os_getdllprocaddr(int hinst,ichar name)ref void=
	return GetProcAddress(cast(hinst),name)
end

export proc os_initwindows=
	os_init()
	os_gxregisterclass("pcc001")
end

export proc os_gxregisterclass(ichar classname)=
	const idcarrow=32512
	rwndclassex r
	static byte registered

	if registered then
		return
	fi

	clear r

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

export proc timerproc(wt_handle hwnd, int msg, id, time)=
	println "TIMERPROC"
end

!GLOBAL PROC OS_TESTCALLBACK(ref void p)=
!	IF WNDPROC_CALLBACKFN THEN
!		(WNDPROC_CALLBACKFN)(P)
!	ELSE
!		ABORTPROGRAM("MESS HANDLER NOT DEFINED")
!	FI
!END

export proc os_setmesshandler(ref void addr)=
	wndproc_callbackfn:=addr
end

export function os_getchx:int=
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

export function os_getos=>ichar=
	if $targetbits=32 then
		return "W32"
	else
		return "W64"
	fi
end

export function os_gethostsize=>int=
	return $targetbits
end

export function os_shellexec(ichar opc, file)int=
	return system(file)
end

export proc  os_sleep(int a)=
	Sleep(a)
end

export function os_getstdin:filehandle =
	return fopen("con","rb")
end

export function os_getstdout:filehandle =
	return fopen("con","wb")
end

export function os_gethostname:ichar=
	static [300]char name
	static int n

	GetModuleFileNameA(nil,&.name,name.bytes)
	return &.name
end

export function os_getmpath:ichar=
	return F"C:\m\"
end

export proc os_exitprocess(int x)=
	stop x
!	ExitProcess(x)
end

export function os_clock:int64=
	return clock()
end

export function os_ticks:int64=
	return GetTickCount64()
end

export function os_getclockspersec:int64=
	return 1000
end

export function os_iswindows:int=
	return 1
end

export function os_filelastwritetime(ichar filename)int64=
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

export proc os_getsystime(ref rsystemtime tm)=
	GetLocalTime(tm)
end

export proc os_messagebox(ichar s,t)=
	messageboxa(0,s,t,0)
end

export function os_hpcounter:int64=
	int64 a

	queryperformancecounter(&a)
	return a

end

export function os_hpfrequency:int64=
	int64 a

	queryperformancefrequency(&a)
	return a

end

export proc os_peek=
	int ticks
	static int lastticks
	[100]byte m
	ticks:=GetTickCount64()
	if ticks-lastticks>=1000 then
		lastticks:=ticks
		PeekMessageA(&m,nil,0,0,0)
	fi
end

export function os_allocexecmem(int n)ref byte=
	ref byte p
	u32 oldprot
	int status

	p := VirtualAlloc(nil, n, MEM_RESERVE ior MEM_COMMIT, PAGE_NOACCESS);
	if p = nil then return nil fi

	status := VirtualProtect(p, n, PAGE_EXECUTE_READWRITE, &oldprot);
	if status = 0 then return nil fi

	return p
end



=== mwindllp.m 0 1 43/44 ===
export function os_calldllfunction(ref proc fnaddr,
		int retcode, nargs, ref[]i64 args, ref[]byte argcodes)word64 =
	word64 a
	real64 x
	int oddstack, nextra, pushedbytes

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

	to nextra do
		assem
			push 0
		end
	od

	for i:=nargs downto 1 do
		a:=args^[i]					!get generic 64-bit value to push
		assem
			push word64 [a]
		end
	od

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
!implements central part of 'callapplproc' which needs to be in asm
	word64 a
	real64 x

	to nextra do
		asm	push 0
	end

	for i to nargs do
		a:=args[i]
		asm push word64 [a]
	od

	if isfloat then
		x:=((ref function:real64(fnaddr))^())
		a:=int64@(x)
	else
		a:=((ref function:int64(fnaddr))^())
	fi

	return a
end
=== rts.pcl 0 1 44/44 ===
!
Procrts rts.$power_i64: i64
    param          rts.$power_i64.a i64 
    param          rts.$power_i64.n i64 
    procentry                 
!-------------------------------------------------
    startmult                 
    push           rts.$power_i64.n i64 
    push           0          i64 
    jumpge         #8         i64 
    push           0          i64 
    resetmult                 
    jump           #9         
#8: 
    push           rts.$power_i64.n i64 
    push           0          i64 
    jumpne         #10        i64 
    push           1          i64 
    resetmult                 
    jump           #9         
#10: 
    push           rts.$power_i64.n i64 
    push           1          i64 
    jumpne         #11        i64 
    push           rts.$power_i64.a i64 
    resetmult                 
    jump           #9         
#11: 
    push           rts.$power_i64.n i64 
    push           0          i64 
    dotindex                  i64 
    jumptrue       #12        i64 
    setargs                   2 0
    push           rts.$power_i64.n i64 
    push           2          i64 
    idiv                      i64 
    push           rts.$power_i64.a i64 
    sqr                       i64 
    callfn         &rts.$power_i64 i64 
    resetmult                 
    jump           #9         
#12: 
    setargs                   2 0
    push           rts.$power_i64.n i64 
    push           1          i64 
    sub                       i64 
    push           2          i64 
    idiv                      i64 
    push           rts.$power_i64.a i64 
    sqr                       i64 
    callfn         &rts.$power_i64 i64 
    push           rts.$power_i64.a i64 
    mul                       i64 
    endmult                   
#9: 
    setret                    i64 
    jump           #13        
!-------------------------------------------------
#13: 
    retfn                     i64 
End

!
    endprogram                
=== END ===
1 mm.m 0 0
2 mmcli.m 0 0
3 mm_blockpcl.m 0 0
4 mm_decls.m 0 0
5 mm_diags.m 0 0
6 mm_export.m 0 0
7 mm_genpcl.m 0 0
8 mm_lex.m 0 0
9 mm_lib.m 0 0
10 mm_libsources.m 0 0
11 mm_modules.m 0 0
12 mm_name.m 0 0
13 mm_parse.m 0 0
14 mm_support.m 0 0
15 mm_tables.m 0 0
16 mm_type.m 0 0
17 pc_decls.m 0 0
18 pc_disasm.m 0 0
19 pc_genmcl.m 0 0
20 pc_genss.m 0 0
21 pc_lex.m 0 0
22 pc_libmcl.m 0 0
23 pc_libpcl.m 0 0
24 pci_mcl.m 0 0
25 pc_objdecls.m 0 0
26 pc_optim.m 0 0
27 pc_parse.m 0 0
28 pc_runmx.m 0 0
29 pc_stackmcl.m 0 0
30 pc_tables.m 0 0
31 pc_win64.m 0 0
32 pc_writeexe.m 0 0
33 pc_writemx.m 0 0
34 pc_writess.m 0 0
35 rundecls.m 0 0
36 runshow.m 0 0
37 runmx.m 0 0
38 mm_help.txt 0 1
39 msysp.m 0 1
40 mlibp.m 0 1
41 mclib.m 0 1
42 mwindowsp.m 0 1
43 mwindllp.m 0 1
44 rts.pcl 0 1
