
!macro SHOW(m) = println m
macro SHOW(m) = eval 0

enumdata []ichar optionnames, []byte debflag=

	(header_sw,		"header",		0),
	(load_sw,		"load",			0),
	(fixup_sw,		"fixup",		0),
	(parse_sw,		"parse",		0),
	(name_sw,		"name",			0),
	(type_sw,		"type",			0),

	(pcl_sw,		"pcl",			0),
	(asm_sw,		"asm",			0),
	(asm2_sw,		"c",			0),
	(mcl_sw,		"mcl",			0),
	(obj_sw,		"obj",			0),
	(mx_sw,			"mx",			0),
	(ml_sw,			"ml",			0),
	(exe_sw,		"exe",			0),
	(mexe_sw,		"mexe",			0),
	(run_sw,		"run",			0),

	(sys_sw,		"sys",			0),
	(minsys_sw,		"minsys",		0),
	(nosys_sw,		"nosys",		0),
	(minos_sw,		"minos",		0),
	(nofile_sw,		"nofile",		0),

	(ma_sw,			"ma",			0),
	(mas_sw,		"mas",			0),
	(docs_sw,		"docs",			0),
	(export_sw,		"exp",			0),
	(lib_sw,		"lib",			0),

	(opt_sw,		"opt",			0),
	(peephole_sw,	"peep",			0),
	(regoptim_sw,	"regs",			0),

	(ast1_sw,		"ast1",			1),
	(ast2_sw,		"ast2",			1),
	(ast3_sw,		"ast3",			1),
	(showmx_sw,		"showmx",		1),
	(showpcl_sw,	"showpcl",		1),
	(showasm_sw,	"showasm",		1),
	(st_sw,			"st",			1),
	(pst_sw,		"pst",			1),
	(stflat_sw,		"stflat",		1),
	(types_sw,		"types",		1),
	(overloads_sw,	"overloads",	1),
	(ss_sw,			"ss",			1),
	(showmodules_sw,"modules",		1),

	(time_sw,		"time",			0),
	(v_sw,			"v",			0),
	(vv_sw,			"vv",			0),
	(quiet_sw,		"q",			0),
	(help_sw,		"h",			0),
	(help2_sw,		"help",			0),
	(ext_sw,		"ext",			0),
	(out_sw,		"out",			0),
	(outpath_sw,	"outpath",		0),
	(unused_sw,		"unused",		0),
	(set_sw,		"set",			0),
end

byte fpclexe
byte fasmexe
byte msfile

global const logfile=langname+"x.log"

ichar outext=""				!for reporting of primary o/p file

int startclock,endclock
int cmdskip

ichar inputfile

int loadtime
int parsetime
int resolvetime
int typetime
int pcltime
int mcltime
int sstime
int exetime
int compiletime

proc main=
	unit p,q,r
	int m,fileno,ntokens,t

	stepruncount()
	startclock:=os_clock()

	initdata('W','X64')

	getinputoptions()

	readprojectfile(inputfile)

	if fverbose>=1 then
		if passlevel=run_pass then
			if not msfile or fverbose>1 then
				println "Compiling",inputfile,"to memory"
			fi
		else
			fprint "Compiling # to #",inputfile:"14jlp-",changeext(outfile,outext),$
			println
		fi
	fi

	remove(logfile)

	do_loadmodules()
	loadtime:=clock()-startclock

	do_parse()

	do_name()

	do_type()

	do_writema()

	do_writeexports()

	case passlevel
	when pcl_pass then
		codegen_pcl()
	when mcl_pass then
		codegen_pcl()
		genmcl()
	when asm_pass then
		writeasmfile(asmfilename)

	when exe_pass then
  		writeexefile(exefilename)

	when lib_pass then
		writelibfile(libfilename)

	when run_pass then
		runlibfile(libfilename)

	esac

	if debugmode then showlogfile() fi

	if fshowtiming then
		endclock:=os_clock()
		compiletime:=endclock-startclock
		showtimings()
	fi

	if fverbose>=2 then
		println "Finished."
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

	int tt:=clock()

	for i:=2 to nmodules do
		parsemodule(i)
	od
	parsemodule(1)
	parsetime:=clock()-tt

	if docfile then
		fclose(docfile)
	fi

	if not debugmode or passlevel>=fixup_pass then
		fixusertypes()
	fi

	fixstartprocs()

	if debugmode and fshowast1 then showast("AST1") fi
end

proc do_name=
	if passlevel<name_pass then return fi

	int tt:=clock()
	rx_typetable()
	for i:=2 to nmodules do
		rx_module(i)
	od
	rx_module(1)
	resolvetime:=clock()-tt

	if debugmode and fshowast2 then showast("AST2") fi
end

proc do_type=
	if passlevel<type_pass then return fi

	int tt:=clock()
	tx_typetable()

	for i:=1 to nmodules do
		tx_module(i)
	od
	tx_allprocs()
	typetime:=clock()-tt

	if debugmode and fshowast3 then showast("AST3") fi
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

	if eqstring(extractfile(os_gethostname()),"ms.exe") then
		msfile:=1
		do_option(run_sw, "")
	fi

	while pmtype:=nextcmdparamnew(paramno,name,value,langext) do
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

	if passlevel<exe_pass then fshowss:=0 fi

	if msyslevel=-1 then
		msyslevel:=(prodmode|2|0)
		msyslevel:=(prodmode|2|0)
	fi

	if inputfile=nil then
		showcaption()
		println "Usage:"
		println "	",,cmdparams[0],"filename[."+langext+"]     # Compile project to executable"
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
	mafilename:=getoutfilename(outfile,langextma)

	strcpy(filespec,changeext(outfile,""))
	strcat(filespec,"_exp")
	expfilename:=getoutfilename(filespec,langext)
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
	when mcl_sw then passlevel:=mcl_pass; outext:="asm"
	when obj_sw then passlevel:=obj_pass; outext:="obj"
	when exe_sw then passlevel:=exe_pass; outext:="exe"
	when mexe_sw then passlevel:=lib_pass; outext:="exe"; mxstub:=1
	when mx_sw then passlevel:=lib_pass; outext:="mx"
	when ml_sw then passlevel:=lib_pass; outext:="ml"; libmode:=1
	when run_sw then passlevel:=run_pass; outext:="mem";

	when ma_sw then fwritema:=1; outext:=langextma
	when mas_sw then fwritema:=2; outext:=langextma
	when export_sw then fwriteexports:=1
	when docs_sw then fwritedocs:=1
	when lib_sw then libmode:=1

	when sys_sw then msyslevel:=2
	when minsys_sw then msyslevel:=1
	when nosys_sw then msyslevel:=0
	when minos_sw then minos:=1
	when nofile_sw then fnofile:=1

	when opt_sw then fpeephole:=1; fregoptim:=1
	when peephole_sw then fpeephole:=1
	when regoptim_sw then fregoptim:=1

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
	when stflat_sw then fshowstflat:=1
	when types_sw then fshowtypes:=1
	when overloads_sw then fshowoverloads:=1
	when ss_sw then fshowss:=1
	when showmodules_sw then fshowmodules:=1

	end switch

	debugmode ior:=debflag[sw]

end

proc showcaption=
	println langnameuc,"Compiler [M5]", $date, $time
end

global proc showhelp=
	static ichar helptext=strinclude(langhelpfile)
	println helptext
end

global proc initassemsymbols=
!initialise hash table from kwddata
	[32]char str
	int i

	for i to md.mclnames.len when i<>m_sub do
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

	for i to segmentnames.upb do
		strcpy(&.str,segmentnames[i])
		str[strlen(&.str)-3]:=0
		addreservedword(pcm_copyheapstring(&.str),segnamesym,i)
	od

	static []ichar regnames=("aframe","dframe","astack","dstack","dprog","dsptr")
	static []byte regnos=(r14,r14, r15,r15, r8, r9)
	static []byte sizes=(4,8,4,8,8,8)
	for i to regnames.len do
		addreservedword(regnames[i], regsym, regnos[i], sizes[i])
	od

end

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

function getoutfilename(ichar file,ext)ichar=
	return pcm_copyheapstring(changeext(file,ext))
end

proc fixstartprocs=
!make sure each module has a start proc
!make sure the lead module has a main proc
	ref modulerec ms
	symbol d
	unit p, q
	int s

	for i to nmodules do
		ms:=&moduletable[i]
		if ms.ststart then
			subproghasstart[ms.subprogno]:=1
		fi
	od

	for i to nmodules do
		ms:=&moduletable[i]
		if ms.ststart=nil then
			s:=ms.subprogno
			if subproghasstart[s] and subprogtable[s].firstmodule=i then
				ms.ststart:=addstartproc(ms.stmodule,"start", program_scope,i)
			fi
		fi

		if ms.modulecode then
			p:=makeblock(ms.modulecode)
			q:=ms.ststart.code					!will be a block
			p.nextunit:=q.a
			ms.ststart.code.a:=p
		fi

!add automatic main proc, but only if modulecode exists
		if i=mainmoduleno and ms.stmain=nil and ms.modulecode then
			ms.stmain:=addstartproc(ms.stmodule,"main", export_scope,i)
		fi
	od
end

function addstartproc(symbol owner, ichar name, int scope,moduleno)symbol stproc=
	stproc:=getduplnameptr(owner,addnamestr(name),procid)
	stproc.scope:=scope
	stproc.moduleno:=moduleno
	stproc.subprogno:=moduletosub[moduleno]
	stproc.code:=makeblock(nil)
	adddef(owner,stproc)
	addtoproclist(stproc)

	return stproc
end

proc stepruncount=
	int count
	filehandle f:=fopen(langhomedir+"/bcrun.txt","r+")
	return when not f
	readln @f,count
	fseek(f,0,seek_set)	!restore position
	println @f,count+1
	fclose(f)	
end

global function runlibfile(ichar filename)int=
	ref librec plib

	codegen_pcl()
	genmcl()

	genss()
	plib:=writememlib(filename)

	loadmemmcu(plib)
	fixuplib(plib)

	if fshowmx then
		LOADERROR("SHOWMX missing")
!		initlogfile()
!		showlibs()
!		closelogfile()
	else
		runprogram(plib, cmdskip)
	fi
	return 1
end

global function writeexefile(ichar filename, int gendll=0)int=

	int tt:=clock()

	codegen_pcl()
	pcltime:=clock()-tt

	tt:=clock()
	genmcl()
	mcltime:=clock()-tt

	tt:=clock()
	genss()
	sstime:=clock()-tt

	tt:=clock()
	initsectiontable()

	genexe(nil, filename, gendll)
	writeexe(filename, gendll)
	exetime:=clock()-tt
	return 1
end

global function writelibfile(ichar filename)int=
	codegen_pcl()
	genmcl()
	genss()

	writemcx(filename)

	return 1
end

global function writeasmfile(ichar filename)int=
	codegen_pcl()
	genmcl()

	ref strbuffer asmstr
	asmstr:=getmclstr()
	writegsfile(filename,asmstr)
	gs_free(asmstr)

	return 1
end

proc showtime(ichar caption, int t)=
	fprintln "# # ms # %", caption:"12jl", t:"5", (t*100.0)/compiletime:"5.1jr"
end

proc showtimings=
	showtime("Load:",		loadtime)
	showtime("Parse:",		parsetime)
	showtime("Resolve:",	resolvetime)
	showtime("Type:",		typetime)
	showtime("PCL:",		pcltime)
	showtime("MCL:",		mcltime)
	showtime("SS:",			sstime)
	showtime("EXE:",		exetime)
	println "-----------------------------"
	showtime("Total:",		compiletime)
end
